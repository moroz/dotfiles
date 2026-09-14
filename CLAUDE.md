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

## Slack status updates

A status update is posted in two steps. The top-level message is an abstract of
two or three sentences ending in a thread pointer (🧵); the body goes in a
threaded reply. The AI disclosure line above goes on the **reply**, not on the
abstract. A note that asks for a decision is shorter still — say what is needed
and from whom.

Link every ticket and PR mentioned. Never link a handoff or working page.

Slack allows only one attached draft per channel, and a threaded reply needs a
real parent `ts`, so a two-step message cannot be drafted whole: draft the
abstract, and hand me the thread body as text to paste once the parent is
posted.

Default to drafting and waiting for my go-ahead, even when sending would be
convenient. Check any factual claim — what merged, what deployed, when
something happened — against `git log` or the ticket before it goes in the
message.

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
