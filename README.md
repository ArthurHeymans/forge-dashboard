# forge-dashboard

A triage dashboard for [Forge](https://github.com/magit/forge): one buffer
showing what is ready to merge, what is blocked on you, and what has gone
stale.

![forge-dashboard](assets/dashboard.png)

All data is read from Forge's local SQLite database. The network is only
touched when you explicitly pull with Forge's commands.

## Sections

- **Ready to merge** — approved PRs, with CI status and an `M` merge hint.
- **Needs attention** — grouped as **On me**, **Nudge**, and **Decide**. Each
  row states the reason (`changes requested`, `they replied`,
  `review requested`, `awaiting review 9d`, `stale 21d`).
- **Owned repositories** — repos owned by your githost login (or listed in
  Forge's `forge-owned-accounts`), with open PR/issue/unread counts.
- **Member repositories** — repos where your login is assignable, plus
  `forge-dashboard-organizations` overrides, grouped by owner.

Unread topics get a `[NEW]` badge and ages use a color ramp (fresh, aging
after 7 days, stale). Draft PRs use Forge's draft face.

## Attention states

| Badge | State               | Meaning                                                                                                   |
|-------|---------------------|-----------------------------------------------------------------------------------------------------------|
| ✅    | `ready-to-merge`    | My PR (or a PR in an owned repo): approved, no `CHANGES_REQUESTED`, not a draft, no conflicts. CI never gates. |
| ⛔    | `changes-requested` | My PR whose latest review requests changes.                                                               |
| ⛔    | `they-replied`      | My topic with an unread reply from someone else.                                                          |
| ⛔    | `review-requested`  | I am a requested reviewer and haven't reviewed yet.                                                       |
| ⏳    | `awaiting-review`   | My PR with no review for `forge-dashboard-awaiting-review-after` days (7).                                |
| ⏳    | `stale`             | No activity for `forge-dashboard-stale-after` days (14).                                                  |

Rows are urgency-sorted: ready first, then blocked on you, then stale. A rule
only fires when the host provides the data, so e.g. repos without CI or
review state simply skip those rules.

## Requirements

- Emacs 29+
- Forge 0.5.x (which pulls in Magit, Transient, Closql, Emacsql, Ghub)

## Installation

Clone next to Forge and add both to your `load-path`:

```elisp
(use-package forge-dashboard
  :load-path ("~/src/forge-dashboard" "~/src/forge/lisp")
  :commands (forge-dashboard))
```

## Usage

`M-x forge-dashboard` opens the dashboard.

| Key       | Action                                                       |
|-----------|--------------------------------------------------------------|
| `RET`     | Visit topic (or list the repository's topics)                |
| `b`       | Browse topic or repository in a browser                      |
| `y`       | Copy topic or repository URL                                 |
| `g` / `G` | Refresh from the local db / pull each dashboard repo         |
| `t`       | Triage the attention queue one item at a time                |
| `z` / `d` | Snooze topic / mark done until new activity                  |
| `C`       | Nudge with a pre-filled comment template                     |
| `M`       | Merge (ready rows only)                                      |
| `?`       | Dashboard menu: section toggles, type filter, per-repo limit |

In triage (`t`): `M` merge, `RET` visit, `b` browse, `c` check out the PR,
`C` comment from `forge-dashboard-nudge-templates`, `z` snooze
(1d/3d/1w/custom), `d` done, `x` close, `SPC` skip, `n`/`p` switch pages
(`All`, `Ready to merge`, one per attention group), `q` quit.

Snooze/done marks are stored in a separate local SQLite file
(`forge-dashboard-triage-file`); the forge database is never written to.

## Customization

- `forge-dashboard-organizations` — extra owners counted as member repos.
- `forge-dashboard-topics-per-repo` — max topics per repo (`nil` = all).
- `forge-dashboard-stale-after` (14), `forge-dashboard-awaiting-review-after` (7) — day thresholds.
- `forge-dashboard-urgency-weights` — per-state urgency weights.
- `forge-dashboard-attention-groups` — named attention subgroups; also triage pages.
- `forge-dashboard-nudge-templates` — pre-filled comment texts.

## Development

`make check` byte-compiles (warnings as errors) and runs the ERT suite in
`test/`.

## License

GPL-3.0-or-later, see [LICENSE](LICENSE).
