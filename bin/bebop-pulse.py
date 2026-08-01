#!/usr/bin/env python3
"""Pulse — bebop's slim external-status fetcher.

Tangled from bebop.org (HUD design, phase 4) — do not edit directly.
Walks the live venues' branches, asks GitLab for MR review state and
head-pipeline status, writes the sidecar through the citizen verbs.
No Jira, no chart writes, no opinions.
"""

import json
import os
import subprocess
import sys
from urllib.parse import quote

# cron's PATH has neither glab nor emacsclient
os.environ["PATH"] = ("/usr/local/bin:/opt/homebrew/bin:/usr/bin:/bin:"
                      + os.environ.get("PATH", ""))


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


def mr_state(mr):
    if mr["state"] == "merged":
        return "merged"
    if mr["state"] == "opened":
        return "draft" if mr.get("draft") else "open"
    return mr["state"]


def main():
    try:
        sessions = json.loads(el_string("(bebop-list-sessions)"))
    except Exception:
        return  # Emacs isn't up; pulse skips a beat, silently
    beats = 0
    for s in sessions:
        name, venue = s.get("name"), s.get("venue")
        if not (s.get("live") and venue and os.path.isdir(venue)):
            continue
        try:
            branch = sh(["git", "-C", venue,
                         "rev-parse", "--abbrev-ref", "HEAD"])
            mrs = json.loads(sh(
                ["glab", "api",
                 "projects/:id/merge_requests"
                 f"?source_branch={quote(branch, safe='')}"
                 "&state=all&per_page=5"],
                cwd=venue))
            # Open beats merged; closed MRs are skipped outright —
            # bebop-mr-cache-set's contract is draft|open|merged, and
            # a closed MR's threads ask nothing of anyone.
            mr = (next((m for m in mrs if m["state"] == "opened"), None)
                  or next((m for m in mrs if m["state"] == "merged"), None))
            if mr is None:
                continue
            iid = mr["iid"]
            detail = json.loads(sh(
                ["glab", "api", f"projects/:id/merge_requests/{iid}"],
                cwd=venue))
            discussions = json.loads(sh(
                ["glab", "api",
                 f"projects/:id/merge_requests/{iid}/discussions?per_page=100"],
                cwd=venue))
            unresolved = sum(
                1 for d in discussions
                if d.get("notes")
                and d["notes"][0].get("resolvable")
                and not d["notes"][0].get("resolved"))
            el(f'(bebop-mr-cache-set "{name}" "{mr_state(detail)}"'
               f' {unresolved} {iid})')
            pipeline = (detail.get("head_pipeline") or {}).get("status")
            if pipeline:
                el(f'(bebop-pipeline-cache-set "{name}" "{pipeline}")')
            beats += 1
        except Exception as e:
            print(f"pulse: {name}: {e}", file=sys.stderr)
    try:
        el("(bebop--render)")
    except Exception:
        pass
    print(f"pulse: {beats} session(s) refreshed")


if __name__ == "__main__":
    main()
