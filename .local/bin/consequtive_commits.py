# Filename: consequtive_commits.py
# Author: Luis Henriquez <luis@luishp.xyz>
# Created: 2024-12-21 17:48:41
# Description: Script to combine groups of similar commits together.

import subprocess
import re
import pprint as pp

def get_git_log():
    """Fetches the commit log."""
    log_format = "%H %s"
    result = subprocess.run(
        ["git", "--work-tree=/home/Luis", "--git-dir=/home/luis/.dotfiles" , "log", "--pretty=format:" + log_format],
        capture_output=True,
        text=True
    )
    if result.returncode != 0:
        print("Error fetching git log:", result.stderr)
        return []
    return result.stdout.splitlines()

def group_commits_by_path(commits, path_pattern):
    """Groups consecutive commits matching the path pattern."""
    pattern = re.compile(path_pattern)
    grouped = []
    current_group = []

    for commit in commits:
        hash_, message = commit.split(" ", 1)
        if pattern.search(message):
            current_group.append(hash_)
        else:
            if current_group:
                grouped.append(current_group)
                current_group = []

    if current_group:
        grouped.append(current_group)
    return grouped

def squash_commits(groups):
    """Squash groups of commits using rebase."""
    for group in groups:
        if len(group) > 1:
            base_commit = group[-1]  # Keep the last commit as base
            commits_to_squash = group[:-1]
            for commit in reversed(commits_to_squash):
                # Interactive rebase to squash commits
                cmd = ["git", "rebase", "-i", f"{base_commit}^"]
                print(f"Squashing commits: {commits_to_squash} into {base_commit}")
                subprocess.run(cmd)

def main():
    log = get_git_log()
    pp.pprint(log)
    # if not log:
    #     return

    # path_pattern = r"README\.org"  # Customize this regex for other paths
    # grouped_commits = group_commits_by_path(log, path_pattern)

    # print("Grouped commits to squash:", grouped_commits)
    # squash_commits(grouped_commits)

if __name__ == "__main__":
    main()
