# forge-dashboard — implementation spec (phases 1–2)

Emacs package providing a triage-oriented dashboard on top of forge
(source reference: `../forge/lisp`, forge 0.5.x). Full design doc lives
elsewhere; this file is the authoritative spec for phases 1 and 2.

## Purpose

One buffer answering: what is ready to merge, what is blocked on me,
what has gone stale — triageable in one or two keystrokes per item.
All data comes from forge's local SQLite db (read-only); no network
calls except explicit refresh via existing forge pull commands.

## Package layout

- `forge-dashboard.el` — `forge-dashboard-mode` (derived from `magit-mode`),
  buffer `*forge-dashboard*`, entry point `forge-dashboard`, section
  rendering, repo classification, db queries, `forge-dashboard-menu` transient.
- `forge-dashboard-triage.el` — attention states, urgency scoring, snooze/done
  store, triage flow, nudge templates.
- `Makefile` — `check` target: byte-compile (warnings as errors) + ERT batch
  run. Must locate forge/magit and their deps (closql, emacsql, ghub,
  transient, magit) — use `../forge/lisp` for forge plus installed packages;
  on this NixOS host use `nix-shell` or the user's Emacs package dirs as
  needed, but `make check` must run non-interactively.
- `test/forge-dashboard-test.el` — focused ERT tests.

Dependencies: forge, magit, transient, compat/seq as needed. Emacs 29+.

## Phase 1 — core dashboard

Buffer structure (magit-section tree), top to bottom:

1. Header line: "Forge Dashboard" + "updated N ago" (db-side latest update).
2. `Owned repositories` — repos whose owner equals my githost login
   (derived from the git variable ghub uses, e.g. `github.user`), or is
   listed in `forge-owned-accounts` (existing forge defcustom, optional
   override). Per repo: open PR count, open issue count, unread count;
   children: up to `forge-dashboard-topics-per-repo` (defcustom, default
   3) open topics, newest first, then an ellipsis row "…N more (RET to
   list all)".
3. `Member repositories` — repos where my login is among the assignable
   users in forge's `assignee` table (assignability implies membership /
   collaborator access) and owner ≠ me, plus owners listed in defcustom
   `forge-dashboard-organizations` (optional override for repos whose
   assignee data is not synced). Grouped by owner, same repo rendering
   as Owned. Zero config in the common case.

Rows and behavior:

- Topic row: number, title, type, author, age. Unread topics use
  `forge-topic-slug-unread`-style face (red); pending orange. RET →
  `forge-visit-topic`. `b` browse, `y` copy URL.
- Repo row: RET → forge topic-list buffer for the repo
  (`forge-list-topics`). `b` → browse repo.
- Age rendered on every topic row with a color ramp: fresh (dim) →
  aging (orange, > 7d) → stale (red, > `forge-dashboard-stale-after`).
- `g` re-renders from db. `G` runs `forge-pull` for each dashboard repo
  sequentially. `?` opens `forge-dashboard-menu` (transient) with:
  section toggles, type filter (all/pr/issue), per-repo limit, refresh
  actions.
- Only repos already tracked in the forge db appear; classification is
  purely local.

Implementation notes:

- Query topics via forge's db layer (`forge-sql`/`forge--list-topics` with
  `forge--topics-spec`); do not reimplement schema access. Read the actual
  API from `../forge/lisp/forge-topic.el` and `forge-topics.el` first —
  verify function signatures against the checked-out source, not memory.
- Rendering with `magit-insert-section`; section values are forge topic /
  repository objects so existing forge commands work at point.
- Functional style: pure functions computing row data from topic objects;
  rendering separate from data.

## Phase 2 — triage core

Attention states, computed locally per open topic at redraw:

| state              | badge | rule                                                                 | ball |
|--------------------|-------|----------------------------------------------------------------------|------|
| ready-to-merge     | ✅    | my PR (or PR in owned repo), ≥1 approval, no CHANGES_REQUESTED, not draft, no merge conflict. CI is NOT a gate — show CI ✓/✗ as an informational badge only. | me → merge |
| changes-requested  | ⛔    | my PR, latest review = CHANGES_REQUESTED                             | me |
| they-replied       | ⛔    | my topic, last comment not mine, topic unread/pending                | me |
| review-requested   | ⛔    | I am requested reviewer, no review from me yet                       | me |
| awaiting-review    | ⏳    | my PR, no review activity for `forge-dashboard-stale-after` (7d)     | them → nudge |
| stale              | ⏳    | no activity for stale threshold (default 14d)                        | decide |
| snoozed            | zZ    | local snooze timestamp in future                                     | hidden |

Notes: "my" = `forge--forge-current-user` / githost user. Degrade
gracefully per host: if a datum (e.g. CI status, review states) is not
in the db for a host, the rule yields nil rather than erroring.

Urgency score = state weight × age; used to sort the attention queue.
Order: ready-to-merge first, then blocked (⛔), then stale (⏳).

New buffer sections, rendered ABOVE Owned/Organizations:

1. `Ready to merge` — ✅ rows: repo, number, title, approval count,
   CI badge, hint "M to merge". `M` at point → `forge-merge`.
2. `Needs attention` — ⛔ then ⏳ rows: repo, number, title, reason
   ("changes requested", "stale 21d"), ball ("on me", "→ nudge?"), age.

Snooze/done store: aux SQLite table (own table in forge's db via
`closql`/`forge-sql`, or a separate sqlite file if cleaner — prefer
simple) mapping topic id → snooze-until timestamp / done flag.
Snoozed topics are hidden from attention sections until expiry.
`d` (done) clears the attention flag until new activity arrives
(store last-updated stamp with the done mark).

Triage flow (`t`): linear one-item-at-a-time over the queue in order.
Transient or dedicated minor mode showing item 2/7 with context line
(approvals, CI, last activity) and keys:
`M` merge, `RET` visit, `b` browse, `c` checkout PR, `C` comment
pre-filled from `forge-dashboard-nudge-templates` (alist of name →
string, default gentle ping), `z` snooze (1d/3d/1w/custom),
`d` done, `x` close topic, `SPC` skip, `q` quit. Point/queue advances
after each action.

Keys in dashboard buffer: `t` triage, `z` snooze, `d` done, `C` nudge,
`M` merge (ready rows only), plus phase-1 keys.

## Tests (focused, no slop)

- Attention-state classification: pure function over synthetic topic
  data (plists or stub objects) covering each state rule, including the
  CI-not-a-gate case (approved + CI failed → ready-to-merge) and host
  degradation (missing CI data).
- Urgency ordering: ready > blocked > stale; age tiebreak.
- Snooze store: set/expire/done-until-new-activity round trip
  (in-memory or temp-file db).
- No rendering snapshot tests.

## Conventions

- lexical-binding, standard elisp header/footer, GPL boilerplate not
  required (no license file yet).
- Functional style; avoid mutable state where reasonable.
- Commit per meaningful step with jj (`jj commit -m "..."`); this repo
  is jj-colocated. Do not push.
- Keep it simple (YAGNI). Comments stay in sync with code.
