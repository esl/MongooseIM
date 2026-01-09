---
name: gh-unresolved-comments
description: Fetch recent unresolved GitHub PR review comments with gh, triage them into an actionable list, apply fixes in the repo, and create a focused commit.
---

# Skill Instructions

Use this skill when you need to:
- find unresolved PR review threads/comments ("outdated" or "needs changes")
- produce a short actionable checklist of what to change
- implement the requested changes safely in the codebase
- run relevant checks/tests
- create a clean, review-friendly commit

This skill is optimized for GitHub CLI (`gh`) + the GitHub GraphQL API.

## Preconditions

- You are in a git repo cloned from GitHub.
- `gh auth status` succeeds for the current user.
- For JSON filtering, `jq` is recommended.

## Procedure

### 1) Identify the PR

Prefer using the current branch’s PR if one exists:

- Find the PR: `gh pr status --json currentBranch --jq '.currentBranch | .number, .url'`
- If that fails, list likely PRs: `gh pr list --limit 10`

Record:
- `OWNER/REPO`
- `PR_NUMBER`

### 2) Fetch unresolved review threads (primary)

GitHub “unresolved comments” generally means *unresolved review threads*.
Use GraphQL via `gh api graphql` to fetch review threads and filter by `isResolved == false`.

Run:

```bash
owner="esl"  # set
repo="MongooseIM"  # set
pr=0000  # set

gh api graphql \
  -f owner="$owner" \
  -f repo="$repo" \
  -F pr=$pr \
  -f query='query($owner:String!, $repo:String!, $pr:Int!) {
    repository(owner:$owner, name:$repo) {
      pullRequest(number:$pr) {
        url
        title
        reviewThreads(first: 100) {
          nodes {
            isResolved
            isOutdated
            path
            line
            startLine
            diffSide
            comments(last: 10) {
              nodes {
                url
                author { login }
                body
                createdAt
                lastEditedAt
              }
            }
          }
        }
      }
    }
  }' \
  > /tmp/pr_review_threads.json
```

Then filter unresolved threads:

```bash
jq -r '
  .data.repository.pullRequest.reviewThreads.nodes
  | map(select(.isResolved == false))
  | to_entries
  | map({
      idx: (.key + 1),
      path, line, startLine, diffSide,
      isOutdated,
      url: (.comments.nodes[-1].url),
      author: (.comments.nodes[-1].author.login),
      createdAt: (.comments.nodes[-1].createdAt),
      lastEditedAt: (.comments.nodes[-1].lastEditedAt),
      body: (.comments.nodes[-1].body)
    })
  | .[]
  | "[#\(.idx)] \(.path):\(.line // .startLine // "?") (outdated=\(.isOutdated))\n\(.url)\nby \(.author) at \(.lastEditedAt // .createdAt)\n\(.body)\n---"
' /tmp/pr_review_threads.json
```

#### “Recent” unresolved comments

If you need “recent”, define a cutoff and filter by `createdAt`/`lastEditedAt`.
Example: last 14 days:

```bash
cutoff="$(date -u -v-14d +%Y-%m-%dT%H:%M:%SZ)"

jq -r --arg cutoff "$cutoff" '
  .data.repository.pullRequest.reviewThreads.nodes
  | map(select(.isResolved == false))
  | map(select((.comments.nodes[-1].lastEditedAt // .comments.nodes[-1].createdAt) >= $cutoff))
  | .[]
  | "\(.comments.nodes[-1].url)\n\(.comments.nodes[-1].author.login): \(.comments.nodes[-1].body)\n---"
' /tmp/pr_review_threads.json
```

### 3) Triage: produce an actionable list

Create a short list of items where each item includes:
- link to the comment (`url`)
- what must change (1 sentence)
- where (module/file and relevant function) and any constraints
- acceptance check (what command/test validates it)

Prefer grouping by file/module to minimize back-and-forth edits.

### 4) Address each item

For each item:
1. Open the referenced file(s) and locate the exact context.
2. Apply the smallest change that satisfies the review.
3. Keep style consistent with the codebase (Erlang formatting, typespecs for exported functions, etc.).
4. Update/extend tests if the change affects behavior.

If a comment is about naming or code style, prefer refactors that reduce churn.

### 5) Validate locally

Run the narrowest relevant check first, then broaden if needed:
- `./rebar3 compile`
- `./rebar3 lint`
- targeted Common Test suite/case if mentioned in the PR

Avoid arbitrary sleeps in tests; use existing wait helpers.

### 6) Create a focused commit

Stage only related hunks:
- `git status`
- `git add -p`

Commit message guidance:
- Use a short, imperative summary.
- Optionally include `Refs: #<issue>` or the PR number.

Example:
- `git commit -m "graphql: address review feedback for listUsers paging"`

### 7) (Optional) Re-check unresolved threads

Re-run the unresolved threads query to ensure you addressed everything.

## Notes & Edge cases

- Some “comments” live as PR issue comments (not review threads). If needed, also fetch issue comments:
  - `gh pr view $pr --json comments --jq '.comments[] | {author:.author.login, createdAt, url, body}'`
- `isOutdated: true` threads may still be useful context; don’t “fix” them unless the reviewer asked.
- If there are more than 100 threads, page the GraphQL query (`after` cursor).
