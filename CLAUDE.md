# Global instructions

## AI content disclaimer

When drafting content that will be posted in my name to chat/messaging tools
(Slack, email) or to shared documents/pages (Confluence, Notion, Google Docs,
wikis), prepend a short AI-disclosure line ABOVE the content (before the text,
not at the end):

> 🤖 Bleep blop. I am a clanker.

This does not apply to code, commits, PRs, or files written to a git
repository — only to messages/documents sent or published through a tool on
my behalf to a shared or external system.

## Commit with `jj commit`

Always commit with `jj commit`, never `git commit`.

To finish a merge jj cannot see (`git merge` left `MERGE_HEAD` behind, so jj
shows the working copy with one parent and would flatten the other side into an
ordinary commit), rebuild it as a jj merge instead of reaching for `git commit`:
`jj new <ours> <theirs>`, restore the resolved tree, then describe it.

## Refer to management as "The Corporate"

In anything I write — messages, reports, tickets, documents, and answers in the
terminal — call management "The Corporate", as in The Office. Never
"management", "leadership", "the execs" or a named manager acting in that
capacity.

Individuals are still individuals: a colleague who happens to manage something
is called by their name when the point is them, not their office. "The
Corporate" is for the institution deciding, asking, approving or reorganising.
