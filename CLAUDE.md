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

## Review a PR adversarially, in a background agent

When I ask for a PR review — mine or your own — run it as a **background agent**
rather than inline, so the reading stays out of the main thread. Brief it with
the PR number, the repo path, what the change is meant to do, and the
`CLAUDE.md` and pattern docs it has to judge against. Tell it to verify every
claim against the code, to say plainly when a suspicion does not survive
checking, and to report each finding as
`path:line: <severity>: <problem>. <fix>.` It reviews only: no edits, no
commits, no pushes.

**Post the findings as a comment on the PR**, and say what you did about each
one — fixed, documented, or rejected with the reason. A finding you disagree
with is worth more in the comment than out of it: a rejected finding and its
argument is a decision the next reader can see. The comment carries the AI
disclosure line like anything else published in my name.

Then fix what survived, in commits of their own on the same branch, and say in
the commit message that review found it.

**Review your own work this way before asking me to merge it.** The automated
reviewer on the PR runs from the base commit on every push, so a finding left
for it costs a full round and leaves the code unchanged in the meantime.

## Refer to management as "The Corporate"

In anything I write — messages, reports, tickets, documents, and answers in the
terminal — call management "The Corporate", as in The Office. Never
"management", "leadership", "the execs" or a named manager acting in that
capacity.

Individuals are still individuals: a colleague who happens to manage something
is called by their name when the point is them, not their office. "The
Corporate" is for the institution deciding, asking, approving or reorganising.

## PDFs: Typst, IBM Plex Sans

Typeset every PDF with **Typst** in **IBM Plex Sans** (IBM Plex Mono for
code). Keep the `.typ` source, its images and the build script together where
the PDF is built, so it can be rebuilt; build there, and copy only the finished
PDF to wherever I read it. Where that is on a given machine is that machine's
setup (the VM's is in claude-vm's `CLAUDE.md`).

Every PDF carries this metadata **in its filename**, and again in its footer:

- a **timestamp** of when it was built;
- its **theme** (`light` or `dark`), when it comes in more than one;
- the **SHA of the working tree** it was built from, when it was built from a
  repository: `git rev-parse HEAD`, suffixed `-dirty` when there are
  uncommitted changes.

Filename: `<name>_<YYYYMMDD-HHMM>[_<theme>][_<sha12>].pdf`, e.g.
`harness-proposal_20261007-1235_dark_91fb05c3903d.pdf`; leave out a part
that does not apply. The footer gives the full form: date and time with UTC
offset, the theme, and the full SHA. When the content draws on several
repositories, the footer lists each one's commit; the filename carries a SHA
only for the tree the PDF was built in.

The build script stamps the metadata, so a rebuild never carries a stale
name.

### "iPad PDF"

When I ask for an iPad PDF, I mean:

- sized for the **iPad 11th generation** (11-inch, 2360 × 1640 at 264 ppi):
  a page in its 1.44 aspect ratio, margins and type sized to read without
  zooming, no multi-column layout that needs panning;
- **WCAG AAA** contrast: at least 7:1 for body text and 4.5:1 for large text,
  in every theme;
- **two builds, light and dark**, from the same source, each stamped with its
  theme.
