#!/usr/bin/env python3
"""Pulse — bebop's slim external-status fetcher.

Tangled from bebop.org (HUD design, phase 4) — do not edit directly.
Walks the live venues' branches, asks GitLab for MR review state and
head-pipeline status, writes the sidecar through the citizen verbs.
Then the derived sets: your open MRs that no session holds, and the
MRs you review. No Jira, no chart writes, no opinions.
"""

import json
import os
import re
import subprocess
import sys
from urllib.parse import quote

# cron's PATH has neither glab nor emacsclient
os.environ["PATH"] = ("/usr/local/bin:/opt/homebrew/bin:/usr/bin:/bin:"
                      + os.environ.get("PATH", ""))
# glab prefers GITLAB_TOKEN over its own login; a stale one in the
# environment would turn every call into a 401. Use the login.
os.environ.pop("GITLAB_TOKEN", None)

BOTS = {"GitLabDuo"}
TICKET = re.compile(r"[A-Z][A-Z0-9]+-\d+")


def sh(args, cwd=None, timeout=60):
    r = subprocess.run(args, cwd=cwd, capture_output=True,
                       text=True, timeout=timeout)
    if r.returncode != 0:
        raise RuntimeError((r.stderr or r.stdout).strip()[:200])
    return r.stdout.strip()


def el(expr):
    return sh(["emacsclient", "--eval", expr], timeout=30)


def el_string(expr):
    # emacsclient prints the elisp representation; a returned string
    # arrives as a quoted literal whose escapes JSON also understands.
    return json.loads(el(expr))


def el_quote(s):
    # A JSON string literal is a valid elisp string literal once it is
    # pure ASCII: \" \\ \n and \uXXXX read the same in both.
    return json.dumps(s, ensure_ascii=True)


def api(path, cwd=None):
    return json.loads(sh(["glab", "api", path], cwd=cwd))


def mr_state(mr):
    if mr["state"] == "merged":
        return "merged"
    if mr["state"] == "opened":
        return "draft" if mr.get("draft") else "open"
    return mr["state"]


def is_bot(username):
    return username in BOTS or "bot" in username.lower()


def threads(discussions):
    """Yield the unresolved, resolvable threads as lists of human-ish notes."""
    for d in discussions:
        notes = [n for n in d.get("notes", []) if not n.get("system")]
        if (notes and notes[0].get("resolvable")
                and not notes[0].get("resolved")):
            yield notes


def on_me(discussions, me):
    """Count unresolved threads where a human other than ME spoke last.
    A thread whose last note is mine waits on them; a bot's (Duo's)
    thread asks less of me than a teammate's and does not count."""
    return sum(1 for notes in threads(discussions)
               if notes[-1]["author"]["username"] not in (me,)
               and not is_bot(notes[-1]["author"]["username"]))


def mr_status(project, iid, me, cwd=None):
    """Fetch one MR's detail and threads; return the facts pulse keeps."""
    base = f"projects/{project}/merge_requests/{iid}"
    detail = api(base, cwd)
    discussions = api(f"{base}/discussions?per_page=100", cwd)
    return {
        "detail": detail,
        "discussions": discussions,
        "unresolved": sum(1 for _ in threads(discussions)),
        "on_me": on_me(discussions, me),
        "mergeable": detail.get("detailed_merge_status") == "mergeable",
        "pipeline": (detail.get("head_pipeline") or {}).get("status"),
    }


def cache(name, st):
    el(f'(bebop-mr-cache-set {el_quote(name)} "{mr_state(st["detail"])}"'
       f' {st["unresolved"]} {st["detail"]["iid"]}'
       f' {st["on_me"]} {"t" if st["mergeable"] else "nil"})')
    if st["pipeline"]:
        el(f'(bebop-pipeline-cache-set {el_quote(name)}'
           f' "{st["pipeline"]}")')


def item(mr, **facts):
    """The derived-set row for MR: what the dashboard draws and acts on."""
    return dict({
        "ref": mr["references"]["full"].split("/")[-1],
        "project": mr["references"]["full"].split("!")[0],
        "iid": mr["iid"],
        "title": mr["title"],
        "author": mr["author"]["username"],
        "branch": mr["source_branch"],
        "url": mr["web_url"],
        "updated": mr["updated_at"],
    }, **facts)


def session_for(mr, branches, sessions):
    """Name the session that holds MR, or None.
    A venue on the MR's branch holds it; failing that, a session named
    for the MR's ticket in the same repo (charts without venues)."""
    if mr["source_branch"] in branches:
        return branches[mr["source_branch"]]
    m = TICKET.search(mr["source_branch"]) or TICKET.search(mr["title"])
    if not m:
        return None
    repo = mr["references"]["full"].split("!")[0].split("/")[-1]
    key = m.group(0).lower()
    for s in sessions:
        name = s["name"].lower()
        if key in name and ("--" not in name or name.startswith(repo + "--")):
            return s["name"]
    return None


def derived(me, sessions, branches, done):
    """Write the Authored and Reviewing derived sets."""
    authored = []
    for mr in api("merge_requests?scope=created_by_me&state=opened"
                  "&per_page=100"):
        name = session_for(mr, branches, sessions)
        if name in done and done[name]["detail"]["iid"] == mr["iid"]:
            st = done[name]  # the venue walk already fetched it
        else:
            st = mr_status(mr["project_id"], mr["iid"], me)
            if name:
                cache(name, st)  # an on-deck session's MR still wants you
        authored.append(item(
            mr, session=name, state=mr_state(mr),
            on_me=st["on_me"], unresolved=st["unresolved"],
            mergeable=st["mergeable"], pipeline=st["pipeline"],
            your_move=bool(st["on_me"] or st["mergeable"]
                           or st["pipeline"] == "failed")))
    el(f'(bebop-derived-set-put "Authored"'
       f' {el_quote(json.dumps(authored))})')

    reviewing = []
    for mr in api("merge_requests?scope=all&state=opened"
                  f"&reviewer_username={me}&per_page=100"):
        if mr.get("draft") or mr["author"]["username"] == me:
            continue
        base = f"projects/{mr['project_id']}/merge_requests/{mr['iid']}"
        mine = [r for r in api(f"{base}/reviewers")
                if r["user"]["username"] == me]
        state = mine[0]["state"] if mine else "unreviewed"
        replied = False
        spoke = False
        if state != "approved":
            for d in api(f"{base}/discussions?per_page=100"):
                notes = [n for n in d.get("notes", []) if not n.get("system")]
                authors = [n["author"]["username"] for n in notes]
                if me in authors:
                    spoke = True
                    if (authors[-1] != me and not is_bot(authors[-1])
                            and not notes[0].get("resolved")):
                        replied = True
        reviewing.append(item(
            mr, review=state, replied=replied, head=mr["sha"],
            your_move=state != "approved" and (replied or not spoke)))
    el(f'(bebop-derived-set-put "Reviewing"'
       f' {el_quote(json.dumps(reviewing))})')
    return len(authored), len(reviewing)


def main():
    try:
        sessions = json.loads(el_string("(bebop-list-sessions)"))
    except Exception:
        return  # Emacs isn't up; pulse skips a beat, silently
    me = api("user")["username"]
    beats = 0
    done = {}
    branches = {}
    for s in sessions:
        name, venue = s.get("name"), s.get("venue")
        if not (venue and os.path.isdir(venue)):
            continue
        try:
            branch = sh(["git", "-C", venue,
                         "rev-parse", "--abbrev-ref", "HEAD"])
        except Exception as e:
            print(f"pulse: {name}: {e}", file=sys.stderr)
            continue
        branches[branch] = name
        if not s.get("live"):
            continue
        try:
            mrs = api("projects/:id/merge_requests"
                      f"?source_branch={quote(branch, safe='')}"
                      "&state=all&per_page=5", venue)
            # Open beats merged; closed MRs are skipped outright —
            # bebop-mr-cache-set's contract is draft|open|merged, and
            # a closed MR's threads ask nothing of anyone.
            mr = (next((m for m in mrs if m["state"] == "opened"), None)
                  or next((m for m in mrs if m["state"] == "merged"), None))
            if mr is None:
                continue
            st = mr_status(":id", mr["iid"], me, venue)
            cache(name, st)
            done[name] = st
            beats += 1
        except Exception as e:
            print(f"pulse: {name}: {e}", file=sys.stderr)
    try:
        counts = derived(me, sessions, branches, done)
        print(f"pulse: derived sets: {counts[0]} authored,"
              f" {counts[1]} reviewing")
    except Exception as e:
        print(f"pulse: derived sets: {e}", file=sys.stderr)
    try:
        el("(bebop--render)")
    except Exception:
        pass
    print(f"pulse: {beats} session(s) refreshed")


if __name__ == "__main__":
    main()
