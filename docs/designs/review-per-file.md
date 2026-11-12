# Review once per file per turn

Status: implemented in 0.19.0 (`efrit-review-covered-p`).

## The problem

The reviewer is a second model call before every write or exec batch.
One real turn on 2026-11-11 made 13 edits to two files under one
request. Each edit was reviewed: 13 calls, 2 to 10 seconds each, one
of 900 seconds when the endpoint stalled. Every verdict was "approved".
The reviewer judged the same question 13 times: is an edit to
`tzz.emacs.functions.el` what the user asked for? The answer did not
change between edits.

## The rule

An approved edit to file F, under request R, covers later edits to F
under the same R. A later edit to F is not reviewed when all of these
hold:

1. The same turn and the same user request. The cover is dropped when
   the turn ends (`efrit-review-forget-session`) or a new user message
   arrives.
2. The tool is one of the file editors: `edit_file`, `edit_buffer`,
   `create_file`, `write_file`. The target is the same canonical path
   or buffer name.
3. The reviewer approved an edit to that target earlier this turn. A
   rejection does not cover; a batch that was never reviewed (a
   read-only turn, a user allow after escalation) does not cover.
4. The new edit carries no FLAG line (`efrit-review-flags-for-use`
   returns nil). A key binding, an advice, a `use-package :vc` block, a
   shadowed name, a load, or a process start in the new text is a new
   question, and goes to the reviewer.
5. The sandbox does not need to ask for it. A write the sandbox would
   prompt about carries a `[SANDBOX would ask]` line, and the reviewer
   may vouch for it; that decision is per call.

Exec calls (`eval_sexp`, `shell_exec`, `run_program`) are never
covered. Two evals are never the same question.

## What the user sees

A covered call shows a row `⚖ not reviewed: covered by the review of
F earlier this turn` in place of the review fold, through the existing
`review-skipped` event, so a missing review fold is never mistaken for
a missed review.

## Why not a time window or a count

A window ("no review for 60 s after an approval") covers edits to
other files, and a count ("every third edit") is arbitrary. The file is
what the reviewer judged; the request is what it judged against. Those
two are the key.

## Why FLAG lines break the cover

The reviewer approved an edit whose flags it saw. The next edit to the
same file can add a `(global-set-key …)` or an `advice-add` the first
one did not have. That is the case the flags were built for (0.8.0) and
a cover that hid it would undo that work.
