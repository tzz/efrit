# Changelog

All notable changes to Efrit will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Changed
- **API Simplification**: `efrit-do` is now the primary async interface (non-blocking execution with progress buffer)
  - Old `efrit-do` (sync) renamed to `efrit-do-sync`
  - Old `efrit-do-async` renamed to `efrit-do`
  - Old `efrit-do-async-legacy` deprecated (use `efrit-do-sync` instead)
- **Documentation**: Added migration guide in README for users upgrading from previous versions
- **Key bindings**: Updated default keybinding documentation to reflect new async-first approach

### Added
- **Progress buffer integration**: Async commands now show real-time progress with thinking and tool execution
- **Command queuing**: Support for queuing commands while another is running
- **Background execution**: `efrit-do-silently` for non-blocking execution without visible progress buffer
- **Progress visualization**: Interactive progress buffer with session status updates

### Deprecated
- `efrit-do-sync`: Use `efrit-do` for async or if blocking is needed use `efrit-do-sync` explicitly
- `efrit-do-async-legacy`: Use `efrit-do-sync` if blocking behavior is required
- `efrit-do-async`: Use `efrit-do` (now the primary interface)
- `efrit-api-url`: Use `efrit-api-base-url` in efrit-common.el instead. The legacy variable still works as an override but is deprecated.


## [0.22.1] - 2026-11-12

### Changed

- The Emacs index lives in efrit's data directory only (`efrit-emacs-index.eld` in `efrit-data-directory`), never beside efrit's code or in an Emacs checkout (tzz: "conflicts with the way emacs is organized").  `make emacs-index` writes `$(EMACS_INDEX)`, default `~/.emacs.d/.efrit/efrit-emacs-index.eld`; pass `EMACS_INDEX=` when `efrit-data-directory` is set elsewhere.  CI writes `dist/`.  The `.gitignore` entry is gone with the in-tree file

### Added

- `checkout=DIR` for the builder: an Emacs source checkout's `lisp/`, `doc/`, `configure.ac` version and HEAD (through vc-git) in one argument; `make emacs-index EMACS_SRC=` uses it
- `efrit-emacs-index-build-check-batch`: exits 0 when the index needs no rebuild, else 1 — the probe a configuration manager runs first.  Current means built in this format from the checkout's HEAD, or, with `max-age=1d` (seconds or s/m/h/d), younger than that: a checkout following master is indexed at most once a day, and never while it stands still (tzz: "Once a day is fine")

## [0.22.0] - 2026-11-12

The Emacs index is a build artifact pinned to one Emacs, and it
indexes the manuals too (tzz: "It's a build artifact, belongs in CI.
But developers like me should be building it locally, pinned to the
Emacs version from Git or by release.  It's advisory anyway").

### Added

- The Info manuals' indices: every concept, variable, function, key, program and type entry of every manual, with the node it points to, from the Texinfo sources of an Emacs checkout (`doc/`: explicit `@cindex`/`@vindex`/… entries, the implicit ones of `@defun`/`@defopt`/`@deffn`/…, chapters pulled in by `@include`, `@value` in titles) or from installed `.info` files (their index nodes; system manuals like grep and find too).  Emacs 32: 41 000 entries in 82 manuals.  Each library lists the nodes that document what it defines (`documented_in` in `library_profile`)
- `info_lookup` tool (read, not reviewed): which node documents a term (exact entries first, then entries containing it; `kind` narrows to one index), and a node's text read from the installed `.info` without an Info buffer
- `make emacs-index`: the `$(EMACS)` binary's own lisp and Info directories; `EMACS_SRC=DIR` a source checkout as it is; `EMACS_SRC=DIR EMACS_REF=REF` one tag or commit through `git archive`, the checkout untouched.  The index records the Emacs version (from `configure.ac` for sources), the revision and the ref
- CI builds the index for each Emacs it tests and keeps it as a build artifact (`efrit-emacs-index-<version>`)
- The doctor has an "Emacs library index" layer: none built, built for another Emacs (with a Rebuild button), or what is loaded

### Changed

- `efrit-emacs-index.eld` is no longer in the repository (`.gitignore`); `make clean` removes it.  An index of another format is not read; one for another Emacs version is read and the doctor says so
- `M-x efrit-emacs-index-rebuild` indexes the installed manuals as well as the libraries
- An installed library shipped as `foo.el.gz` is named `foo`, not `foo.el`

## [0.21.1] - 2026-11-12

### Fixed

- A read bounded by a timeout is a pause, not a prompt: `(read-event nil t SECONDS)` and `(read-char … SECONDS)` run under eval_sexp's input guard, so `sit-for` works inside an eval.  A `(sit-for 2)` in a use-package `:config` block, reached by a reload from an eval, was refused with "`read-event' is blocked" and the rest of the block (timers, demon init) never ran; the error stayed in `*Warnings*`.  A `read-event` without a timeout is still refused (`efrit-tools--timed-read-functions`)

## [0.21.0] - 2026-11-12

The Emacs library index: what every core library touches on its
own, known before a call instead of learned from a denial (tzz: "a
valuable index for AI use to avoid investigating it last minute").

### Added

- `efrit-emacs-index.eld`: 1567 libraries of the Emacs lisp tree read without loading (`lisp/dev/efrit-emacs-index-build.el`, 2 s): per library the options naming files and programs with their defaults rendered (`locate-user-emacs-file`, `expand-file-name` under the Emacs directory, literals), the programs started by name, the configuration and state files mentioned, the hosts in URL literals, the primitive classes used (reads, writes, processes, network, prompts, auth-source), hooks, requires, commands.  A copy ships with efrit; `M-x efrit-emacs-index-rebuild` indexes the running Emacs into the data directory, which then wins (`lisp/core/efrit-emacs-index.el`)
- The sandbox waves a library through to its own files: when the library behind an access (the frame above it) owns the file through one of its options or literal paths, the read or write is expected and granted on that file alone (`efrit-sandbox-expected-library-files`).  savehist's history, recentf's list, transient's files, tramp's cache, the mailcap files, and 800 more, with no list written by hand.  `~/`, `/`, the Emacs directory and `.efrit/` are never covered this way
- The sandbox prompt's detail for a library's access names the library and its profile: `write-region, from savehist-save (reads/writes ~/.emacs.d/history)`
- `library_profile` tool (read, not reviewed): a library's profile by name, or a search of the index by regexp over names, summaries, keywords, programs, files and hosts (`library_profile query="pdf"` → doc-view).  `library_contract` carries the same profile under `library` for the function's library
- The reviewer gets `[FLAG library: doc-view runs gs, …; may prompt]` under a `require`/`load` of a library that starts programs, contacts hosts, prompts or reads credentials
- Consecutive successful calls of one read tool share a row in the agent buffer: `elisp_docs ×3 · a, b, c`, each call's input and result in the body, the first call's id kept so the fold toggles as one; the `not reviewed: read-only turn (…)` notes between them merge into one naming each tool once (`efrit-agent-merge-tool-runs`).  Exec and write tools, failures, and different tools stay one row per call (tzz: "merge sequential calls to the same tool with the same approval")

### Changed

- System prompt: `library_profile` before calling into a library; the tool selection guide lists it

## [0.20.1] - 2026-11-12

### Added

- Routines noticed across sessions: `tool-patterns.json` in the data directory counts, per project, the sessions that ran each 2–4 call sequence (once per session, shapes only, never arguments).  When `efrit-tool-defs-session-threshold` (3) separate sessions ran the same sequence, the `⚙ define_tool` note appears, saying so.  A once-a-morning routine never repeats inside one session, so the in-session detector alone never saw it (tzz: "something efrit learns on its own instead of making me catch the patterns").  A noted sequence is proposed again after `undefine_tool` removes a tool whose origin names it

### Fixed

- A stat of `~/.mailcap`, `/etc/mime.types` and the other RFC 1524 files from an eval is an expected read, granted on that file alone: `mailcap-mime-info`'s first call parses them and one `(mailcap-mime-info "application/pdf")` asked the user four times for files the form never named, offering "read files under ~/" (`efrit-sandbox-expected-read-files`, `efrit-sandbox--library-config-files`, `MAILCAPS`/`MIMETYPES` honoured)
- The sandbox prompt for a file an eval reaches through a library names the library: `file-attributes, from mailcap-parse-mailcaps`, not `file-attributes` alone (`efrit-sandbox-eval--asking-library`)

## [0.20.0] - 2026-11-12

Results as good as a careful reader's, as fast: ask Emacs about the
library, verify by running, keep the whole change in view, and let
efrit grow its own tools (tzz: "take advantage of its higher coupling
with emacs internals"; "build in a way for efrit to extend itself").

### Added

- `library_contract` tool: what to know before calling, wrapping or rebinding a function, from the loaded definition in one call: kind, signature, the `interactive-only` / obsolete / `pure` declarations with their replacements, the options and variables it reads (and sets), the hooks it runs, what the docstring says it depends on (point, region, current buffer), its advice, and its callers in the user's own code first (`load-history` walk, no project needed).  Read class, not reviewed (`efrit-tool-contract.el`)
- `probe_command` tool: run a command (or any function) in a throwaway buffer with named collaborators stubbed, under the sandbox with input blocked, and get back the value or error, every call each stub received with its arguments, the messages, the buffer's text/point/mark after and the other buffers changed.  Exec class.  Stubs on efrit's own functions are refused; a stub is a `cl-letf` through `efrit-eval-with-function`
- The reviewer sees the function an edit leaves, not the fragment: for `edit_file`/`edit_buffer` on Lisp, the batch carries `[AFTER THE EDIT, NAME: …]` with the whole top-level form computed from the current text (visiting buffer first), and `[CALLED BY: …]` with the loaded callers (`efrit-review-after-edit`, `efrit-review-after-edit-max-chars`)
- The session's change as one diff: every file a write tool touched is tracked per session; a unified diff of all of them against where each started (VC HEAD, else the text before the first write, else nothing for a new file) goes into the editor context of each user turn (`session-diff` source) and into the reviewer's message as CHANGES THIS SESSION.  Most recently touched first, budgeted, omitted files named.  `/diff` shows it; a new conversation forgets it (`efrit-session-diff.el`)
- Tools defined in JSON by the user and the model: `tools.json` under `efrit-data-directory` (global) and `.efrit/tools.json` (project; shadows a global name), each definition `{name, description, class, input_schema, body: {kind: elisp|template, source}}`, registered through `efrit-register-tool` and re-read when the files change.  An `elisp` body is one form evaluated with `input` bound exactly as `eval_sexp` evaluates; a `template` body is a shell line with `{{field}}` placeholders run as `shell_exec` runs.  `define_tool` (write class; the user approves the definition once at the sandbox prompt and can edit it there), `undefine_tool`, `list_tools`, `/tools`.  When the same 2–4 call sequence repeats three times in a session, a `⚙` note and a `[note: …]` in the result say `define_tool` can make it one call, once per sequence.  Design: `docs/designs/user-defined-tools.md` (`efrit-tool-defs.el`)

### Changed

- System prompt: when to use `library_contract` and `probe_command`, that the session diff is the state to continue from, and how to propose a tool
- `define_tool` / `undefine_tool` are approved per call, with no grant recorded (`efrit-sandbox-approve-once`): the definitions file is under `.efrit/`, which no grant may cover, and the prompt's `e` edits the definition's JSON before it is written

### Fixed

- `shell_exec` / `run_program` could return output with its tail missing on a loaded machine: the process exit was seen before the last pipe read.  `efrit-do--run-argv` drains the output after the exit

## [0.19.0] - 2026-11-11

Fewer prompts, fewer reviews, one bug class closed (tzz: "super super
slow and prompting a LOT ... prompt only for actually important or
dangerous things").

### Changed

- Writes where Emacs keeps its own state are expected, not asked about: backups (`backup-directory-alist`), auto-saves, `package-user-dir` and the archives under it, `efrit-data-directory`, undo-fu-session, recentf, savehist, save-place, bookmarks, the Tramp cache, `url-configuration-directory`, transient history, the eln cache (`efrit-sandbox--housekeeping-directories`, on `efrit-sandbox-expected-write-roots`).  The home directory itself never qualifies.  One reload from an eval had asked seven times
- The reviewer judges an edit to a file once per turn: after it approves an edit to F under a request, plain later edits to F under the same request run without a review call.  An edit that adds a FLAG (key, hook, advice, `:vc`, load, process), one the sandbox would ask about, an edit to another file, and every exec call are still reviewed.  The transcript says `not reviewed: covered by the review of F earlier this turn`.  Design: `docs/designs/review-per-file.md`
- `cl-letf` over the function cell of an ordinary symbol is allowed in `eval_sexp`: the form is rewritten to `efrit-eval-with-function`, which restores the definition however the body ends.  efrit's own symbols stay refused, and so does a bare `fset`.  The model's test fixtures were refused twice on 2026-11-11 and the diagnosis cost a question to the user

### Fixed

- A package install reached at run time inside an eval (`use-package … :ensure` under a reload: two macro layers the static inspector cannot see) is refused when `package-install` is called, before anything downloads or `package-review` can prompt.  Before, `package-review`'s `read-multiple-choice` hit the input stub and use-package posted a `*Warnings*` error the user had to dismiss by hand
- `load_project_file` compares truenames: a project reached through a symlink (`~/autodist` → a checkout elsewhere) refused its own files four times in one turn
- The non-blocking chooser (`request_user_input` with options) waits for an open modal prompt to close instead of opening over it; the two shared one menu's state and the second died with `Wrong type argument: stringp, nil` (`efrit-choice-open`)
- Tool rows chopped to a narrow side window are re-cut when the buffer is shown in a wider existing window (`window-buffer-change-functions`), not only on a frame resize
## [0.18.0] - 2026-11-10

The reviewer's advisor.

### Added

- `efrit-review-advisor`: when a decision backend is configured, each reviewable call also goes to the decision model with two fixed questions (does the request cover this action; how much harm if not), and the answers appear under the call in the reviewer's view as `[ADVISOR: asked P, risk LEVEL S]`, beside the FLAG lines.  The reviewer decides; the advisor informs: it never approves, rejects, or raises a confidence, and a slow or absent advisor (`efrit-review-advisor-timeout`, 3 s) leaves an `unavailable` line and the review goes on

### Fixed (0.17.1 – 0.17.3)

- `decide`, `generate_image`, `generate_video` stay registered without a backend; the description says so and a call names the fix
- Tools that wait on their own request wait with `sit-for`: Emacs stayed responsive-looking frozen for an image generation; `C-c C-k` cancels such requests too
- A generated image's line is a result, not work: it stays visible when the turn folds; the fold summary counts rows by id (two adjacent tool rows read as one)

### Fixed (0.18.1 – 0.18.10)

- The reviewer's call count and purpose count uses, not description lines
- Folding a turn no longer resets a tool row's own folded/unfolded state (arrows and text disagreed)
- A Gnus agent buffer roots at `efrit-gnus-project-root` (default `efrit-default-project-root`, else `~`), never at the current buffer's remote project; an existing buffer is re-rooted
- A session rooted on a remote host froze Emacs in every review: the standing instructions and the symbolic-path templates re-walked the remote ancestors and re-read the remote `CLAUDE.md` over TRAMP, inside the curl sentinel.  Remote roots now keep their located files and text for `efrit-instructions-remote-cache-seconds` (300; `efrit-instructions-forget` drops it), and templates come from local instruction files only
- `C-c C-k` (cancel) on a turn with nothing in flight (no stream, no tool: a callback died between the response and the next request) ends the turn at once instead of setting a flag nothing reads; the buffer had shown `reviewing...` for good
- `C-c C-k` cancels the turn with point in the input too: the input map bound it to a clear of the input, which shadowed cancel and threw the draft away; `efrit-agent-input-clear` now kills to the kill ring, `C-c C-u` is the key for that
- Resume: `M-x efrit-resume` lists sessions as `id | time | project | first words`, newest first, and accepts a full id or a unique part of one as typed (`/resume 232956`, `M-x efrit-resume-session`); a restored conversation whose assistant entries are content blocks no longer crashes (`stringp`) and renders its text plus one `⚙ tool, tool` line, oldest first.  `efrit-session-persist-load` returns the conversation in the live order (oldest first); `ask_session` sent a peer's transcript backwards
- Resume opens a new agent buffer; the one you resume from is left alone.  When the session's saved directory differs from the current one you choose which the restored session works in (`efrit-resume-directory`: `ask`, `saved`, `current`)
- The header names the agent-switching keys (`C-c ]/[`, `C-c }/{`, `C-c ? l`) and the count once a second agent buffer exists
- `web_search` asks the sandbox for the engine's host (`efrit-web-search-host`), which is a configured host: a search no longer prompts as "a host your request did not name"
- A local buffer with no visited file (`*scratch*`, a temp or help buffer) needs no `buffer` grant: nothing the file sandbox protects is behind it.  The kinds in `efrit-sandbox-guarded-buffer-regexps` (the message log, shells, mail readers, eww) still ask, and the prompt says which kind instead of "visits a file outside the project"
- A gateway that closes the stream before the final event (`provider_connection_failed`, "connection ended mid-stream") is a transport failure: efrit-reconnect retries the same request instead of ending the turn as a hard error
- The reviewer reads the conversation so far (earlier requests and the agent's answers, clipped by `efrit-review-conversation-max-chars`), not only the request of the moment: "try from the latest again" is judged against the task it continues.  The advisor gets the same context
- A remote project root is walked with one `directory-files` per ancestor instead of a stat per candidate instruction file: five names over nine ancestors was 45 TRAMP round trips before the first review could start

## [0.17.0] - 2026-11-10

Decision models.

### Added

- `efrit-decisions`: fixed questions about a text answered by a decision model as numbers.  Backends via `efrit-decisions-add-backend` (name, URL, model; efrit's key or an auth-source host); protocol `decisions-v1` (`{model, input, questions}` → `{answers}`), questions as plists of type `predicate` (a probability), `choice` (a value with probabilities) or `score` (a probability-weighted index on labelled levels).  `efrit-decide`, `efrit-decide-async`, `efrit-decide-many` (parallel, ordered, cancellable)
- Tool `decide` (exec; many inputs, one row of answers each), registered while a backend exists; command `efrit-decide-lines` over the region's lines with named question sets from `efrit-decisions-questions-file`, results in a sortable table (`w` copies TSV)
- `efrit-gnus-triage` (`d` under the Gnus prefix): the selection or all unread through the decision model (action, urgency, asks-me), a table sorted by urgency, `RET` opens the article.  The sandbox and the reviewer do not use decisions, by design
- `efrit-http`: the curl transport (JSON POST as a process, cancel, sync wait) shared by efrit-media and efrit-decisions

### Fixed (0.16.1 – 0.16.6)

- The generated image is a fold in the transcript (`RET`/`TAB` unfold, `o` opens, `c`/`m`/`w`); a video unfolds to a first-frame poster with duration, size and audio; videos open through `efrit-media-video-player` (system opener by default), not `find-file`
- Font-lock is a no-op in agent buffers whatever turns it on (a jit function erased image `display` properties)
- `url-retrieve` results are delivered from a timer: a prompt inside the url filter hung Emacs past SIGUSR2
- efrit's own gateway hosts (API base, media backends, `efrit-sandbox-expected-hosts`) are expected `net` targets

## [0.16.0] - 2026-11-10

Image and video generation.

### Added

- `efrit-media`: generate an image or a video from a prompt through a configured backend.  Backends are added with `efrit-media-add-backend` (name, kind, protocol, URL, model; efrit's API key and auth scheme by default, or an auth-source host); two protocols ship, `openai-images` (OpenAI's images shape, `b64_json`) and `interactions` (Google's Interactions shape, `steps[].content[]`), and a package adds one with two `cl-defmethod`s.  The request runs as a curl process; `efrit-media-cancel` stops it.  Results land under `efrit-data-directory/media/` with a JSON sidecar (prompt, backend, model, time)
- Tools `generate_image` and `generate_video` (exec class; the sandbox asks for the endpoint's host; the reviewer sees the prompt), registered only for kinds that have a backend; the result shows in the agent buffer with keys to open (`RET`), copy (`c`) or move (`m`) it into the project's `media/`, or copy its path (`w`)
- Commands `efrit-generate-image`, `efrit-generate-video` (the region as the default prompt; `C-u` picks the backend), `efrit-media-copy-to-project`, `efrit-media-move-to-project`, `efrit-media-open`

### Fixed

- `efrit-agent-fold` no longer uses `cl-member-if` (obsolete in Emacs 31)

## [0.15.0] - 2026-11-02

The prompt library in every interaction.

### Added

- A prompt carries uses (`mail`, `code`, `chat`, `documents`, or any symbol): where it is offered.  Each chooser passes the use it serves and shows the prompts tagged with it plus the untagged ones, which fit everywhere.  Pills in the chooser and the manager's new Uses column; a Uses line in the editor; `efrit-prompts-for`, `efrit-prompts-fits-p`, `efrit-prompts-read` takes a USE
- `efrit-scope-insert-prompt` (`C-c p`, `/prompt`, menu `L`): a library prompt into the agent buffer's input, placeholders filled from the buffer you came from, to add to and send
- The Gnus prompts are tagged `mail`, the scope and refactoring prompts `code`

### Fixed

- A gnus batch whose analysis preceded a tool call reached the closing summary as its sign-off line only; the whole turn's text is stitched now, and the closing message lists the selection's references instead of inviting a search (0.14.1)
- Long prose between two tool rounds stays visible when a turn folds; each work span folds on its own (0.14.2)
- The unread mark clears at the bottom of a large frame (`window-end` ran short); the dwell hook is restored when Emacs evicts it (0.14.3)
- A bold heading split over two streamed chunks keeps one colour: font-lock is off in the agent buffer and the renderer rebuilds `face` from its mirror (0.14.4)

## [0.14.0] - 2026-10-23

### Changed

- Emacs 29.1 is the minimum.  55 of the modules already used 29 APIs (tree-sitter, `string-pixel-width`, the 29 `subr-x`); 28.1 never built.  CI runs 29.1, 29.4, 30.1, 30.2, 31.1 and snapshot

### Fixed

- `efrit-choice` compiles under warnings-as-errors on a snapshot Emacs (the runtime-defined prefix and transient's slot are declared)

## [0.13.14] - 2026-10-21

Agent buffer interaction.

### Added

- RET steers a running turn, M-RET queues
- Introspection tools: symbol_info, live_state, mode_line_report, buffer_scan, buffer_list (local buffers first, remote by name only)
- efrit-agent-screenshot: /shot and show_ui_element attach a frame image with a layout block of windows and line constructs
- efrit-agent-fold: a finished turn's tool and review rows fold under one line above the answer; isearch opens folds
- efrit-choice: one transient chooser for questions, multi-select sets and confirmations; question options as wid-edit widgets with All / None / Submit; confirm_action never times out
- efrit-questions: a bare re-ask after the user asked something is refused once with the question quoted; prose choice lists become a real question
- reproduce_command: run the user's failing command and return the backtrace from the signal point
- efrit-default-project-root for turns outside a VC repo
- Reviewer sees the turn so far; a removal after an addition is a move
- Stale loop entries recovered; cancel follows the session status
- Package installs and synchronous downloads refused in eval_sexp
- File changed on disk reconciled without asking; chopped rows re-cut on resize

## [0.12.36] - 2026-10-20

Agents, pills, worktrees, symbolic paths.

### Added

- efrit-agent-ring: move between agents, attention flags (waiting, unread with dwell), mark unread; worktree instances; diagnose rows; dashboard folders; focus and restore
- efrit-pill: SVG pill primitive; agents in the tab line as pills; pills in dashboard, diagnose and mode line; one spinner per mode line
- efrit-agent-consult: consult another agent
- run_program tool (argv, no shell); shell grants tolerate read-only extras; no shelling out for what Emacs wraps
- efrit-sandbox-symbolic: instruction templates become expected paths and reviewer facts; placeholders rendered in prompts
- read_file over several paths with a budget; project_outline tool; load_project_file and run_ert run in this Emacs
- Pasted and dropped images kept outside the project, pruned by age
- Editing without junk backups; buffer grant covered by read and write grants on the file's directory
- Final message shown after narration; full-width header

## [0.11.1] - 2026-10-11

Local code review.

### Added

- efrit-code-review: scope by diff hash, findings model, gates from old_lines to a patch, applied bottom-up; submit_review, verify_block and read_diff tools
- efrit-code-review-ui: review buffer with queue, apply and dismiss; Magit dispatch entry and agent menu entry
- surrounding_context tool; scoped read_file

## [0.10.3] - 2026-10-08

Reconnect.

### Added

- efrit-reconnect: transport failures retried with backoff and a probe of the endpoint; the turn's work is kept
- After repeated failures the user is asked: keep waiting, retry or abort; nothing answers for them

## [0.9.2] - 2026-10-04

Reviewer confidence.

### Added

- efrit-grant-history records what the user allowed
- efrit-review-confidence: history and facts give a score; reviewer confidence at or above the threshold satisfies the sandbox without a prompt, once per session, with hard blockers

## [0.8.5] - 2026-10-02

Walk-away.

### Added

- Timeout via the limits menu (30 min default); a notification on every prompt; efrit-unattended mode; progress-based stall detection
- Prompts never time out and are answered only by the user; a menu closed by anything else is a denial
- efrit-review-flags: static analysis of proposed Lisp changes (keymaps, hooks, advice, load order, eval forms) feeds the reviewer as FLAG lines
- Circuit breaker per target (URL, path, command) and per tool; a trip pauses one tool, not the turn

## [0.6.2] - 2026-10-01

Sandbox and review fixes.

### Fixed

- Grants are scoped to the VC root; an expected/unusual classifier decides what is asked about; transport paths are exempt
- A review rejection advises the model and never ends the turn; standing instructions are shown to the reviewer
- Session history fixes

## [0.5.3] - 2026-09-30

Test drive fixes.

### Fixed

- Tour stops no longer depend on each other; the drive dumps state on failure
- Dedicated window layout for the drive
- The sandbox prompt works under inhibit-quit
- efrit-run-tests: one Emacs per ERT file, explicit file selection
- efrit-do legacy path renders into the session's buffer

## [0.5.0] - 2026-09-29

Editor-integrated agent.

### Added

- efrit-api-stream: streaming over curl; token-by-token rendering, real cancel, partial answer kept on timeout
- efrit-api-auth-scheme for bearer-token LLM proxies; efrit-api-request-transforms and efrit-api-extra-body
- efrit-permissions: consent before mutating tool calls, with a diff preview and edit-before-approve in the prompt
- efrit-events: an event bus the agent buffer subscribes to; idle timer; live token usage in the header line
- efrit-context-sources: editor context (buffer, point, region, project) prepended to REPL turns; editor_state tool
- efrit-file-io: buffer-aware, verified file edits
- efrit-models: model discovery and selection; gateway model errors recognised
- efrit-doctor: checks every configuration layer and offers fixes
- efrit-agent-svg-header: graphical header for the agent buffer
- efrit-system-prompt-functions; dead task classifier dropped
- Tramp: sandbox and tool processes are remote-aware
- Generated autoloads cover lisp/ subdirectories; make test runs ERT
- Agent buffer scrolls only windows that were following the output
- Smaller ports: transient menu, template expander, Lisp balance check, tail truncation, warn-once, hardened persistence
- efrit-sandbox: scope grants per project (read, write, shell, net, elisp, buffer), JSON store, every tool checked, eval_sexp wrapped with static inspection and dynamic enforcement; live buffers visiting out-of-project files gated
- efrit-sandbox-ui: transient prompt with the form shown, grant list, ? toggles inline details, y yanks, b opens a buffer; a denial continues the turn as a failed tool result
- efrit-review: a second model reviews mutating tool calls before they run; every turn shows its review status; control-only turns emit no review note; network calls reviewed
- efrit-instructions: AGENTS.md / CLAUDE.md loaded in layers
- efrit-limits: circuit breaker per turn with a raise prompt; limits menu
- efrit-reload: self-updating reload that refreshes keymaps, reverts unmodified buffers visiting reloaded files, adopts changed defcustom defaults the user never set
- Agent buffer: comint-style input, user-turn block, answers as prose, one-line tool rows chopped at the window width, menus for choices and limits, review rows, report buffers on demand, SVG activity spinner; the window is dedicated and closes on quit
- emacs_apropos tool and a prompt that teaches the model Emacs first
- Permissions: editor across projects, per-command shell grants, project policy file; read requests ask for the enclosing git tree once and absorb narrower grants
- Zero byte-compile warnings, gated in make
- efrit-package-review: wraps package-review (Emacs 31); the model reads sources, diff and changelog before "Install?", with a rendered report; batch review of installed packages
- efrit-testdrive: guided live test drive of the whole system
- efrit-tool-registry: packages register tools and submit prompts
- efrit-settings: per-project settings file
- efrit-log-mode with a debug toggle; API requests and responses, bus events, sandbox decisions and raw reviewer answers logged
- efrit-permissions-ui split out; sandbox prompt N/q
- API: astral characters escaped as UTF-16 surrogate pairs; JSON escapes never case-converted; synchronous requests keep Emacs alive; a turn whose request cannot be built fails instead of hanging
- Reload drops features whose file was removed
- Agent buffer subscribes to the bus; efrit-loop knows no interface
- efrit-gnus: analyse Gnus articles in isolated batch turns with a stitched closing summary; generic expander for articles with links
- efrit-documents: a source protocol with doc_* tools; Google Drive, Google Calendar, Confluence and Jira sources; efrit-auth over auth-source and OAuth2
- efrit-prompts and a prompt editor: scoped task prompts, review before send
- efrit-markdown: renderer for the agent buffer (images, tables, code blocks)
- efrit-agent-mentions: @file completion with #symbol and #L ranges, slash commands, drag and drop of files and images
- efrit-agent-menu: the transient menu for the agent buffer
- efrit-transcript: the turn is appended to a transcript file as it goes
- API watchdog for asynchronous requests; timeouts tuned
- Agent buffer: queue and steer while a turn runs, invisible folds with isearch support, undo hygiene, quote region into prompt, narrow to the last N turns, list editing in the input
- efrit-vcs: all version-control work through VC and Magit, never a shell git; efrit-magit hunk actions with provenance; efrit-commit builds commit messages from the staged diff
- Multi-session: rendering routed by session id, per-session project root, per-turn state keyed by session; efrit-agent-instances with *efrit[project]* buffers, side-window slots, rename, dashboard
- Navigation tools: xref_references, xref_apropos, imenu_symbols, treesit_info, show_location, get_last_error; get_diagnostics over the project
- efrit-brief: question-only briefs drive a read-only turn; efrit-ask side requests; efrit-rewrite with an editable-region protocol; efrit-scope task commands; regenerate the last turn
- efrit-commands: send-DWIM, investigate-exception, checkpoint steer, :shell commands, blame and log prompts, refactor catalog
- efrit-diagnostics-baseline: new diagnostics appended to edit results; fix-errors-in-scope
- efrit-edit-history context source; efrit-inline-diff hunk overlays; efrit-text-window; efrit-throttle; efrit-notify; efrit-presets; efrit-next-steps buttons; efrit-elisp-fix balancer
- ediff review from the diff preview; context indicator in the input header; pins

## [0.4.1] - 2025-11-30

### Added
- **efrit-chat improvements**: Batch mode support and error classification
- **Transparency features**: Tool visibility indicators and incremental response display for streaming content
- **Input handling**: Display of thinking/reasoning to users when available
- **Buffer tools**: New buffer creation and editing capabilities in efrit-chat
- **Session features**: History navigation (M-p/M-n) and context-aware input completion in efrit-agent
- **Error recovery**: Action buttons in efrit-agent for failed tool execution
- **Display enhancements**: Inline diff display for tool results and thinking indicator display

### Changed
- **Chat refactoring**: Reorganized efrit-chat.el into three focused modules
  - `efrit-chat-api.el`: API communication
  - `efrit-chat-buffer.el`: Buffer management
  - `efrit-chat-persistence.el`: Session persistence and history
- **UI refactoring**: Split efrit-agent into 5 logical modules for better maintainability
  - `efrit-agent-core.el`: Core session management
  - `efrit-agent-render.el`: Rendering logic
  - `efrit-agent-integration.el`: Executor integration
  - `efrit-agent-tools.el`: Tool-specific display
  - `efrit-agent-input.el`: Input handling
- **Tool handlers**: Extracted tool-specific logic from efrit-do.el into `efrit-do-handlers.el`
- **Model version**: Fixed model version mismatch issues in efrit-chat

### Fixed
- Fixed efrit-chat model version configuration
- Fixed efrit-agent mode initialization logic
- Corrected Makefile dependency paths for reorganized modules
- Corrected link to contributor guidelines in README
- Improved error handling and classification in chat mode
- Fixed batch mode support for concurrent requests

### Removed
- Removed debug test files and stale documentation references

## [0.4.0] - 2025-11-28

### Added
- **Public release of 0.4.0**: First stable version with modernized architecture
- **Comprehensive documentation**: Consolidated from 27 files to 10
- **Test coverage reporting**: Added coverage metrics to test runner
- **Development tooling**: Improved with CLAUDE.md agent instructions
- **Session persistence**: Full session transcript persistence and resume capability
- **Autonomous startup**: Development mode for daemon startup and testing

### Changed
- **Async architecture refactoring**: Simplified and reorganized core modules
  - Moved executor and tools to core/ directory
  - Consolidated session-related modules (6 modules → 1 focused efrit-session.el)
  - Reorganized interfaces into interfaces/ directory
  - Merged UI modules into support/efrit-ui.el
- **Major version bump**: Incremented to 0.4.0 to reflect significant modernization
- **Testing**: Expanded automated test specs for Tiers 1-6
- **MCP integration**: Enabled ESM tests and improved security

### Fixed
- Fixed unicode escape bug causing API failures
- Fixed session message syncing for transcript persistence
- Fixed circuit breaker being too restrictive for multi-step coding tasks
- Added error loop detection to circuit breaker
- Fixed multibyte text in HTTP request errors

## [0.3.1] - 2025-11-26

### Added
- **Multimodal support**: Added read_image tool for image analysis
- **Image handling**: Proper base64 encoding and Anthropic Claude API integration
- **Resource limits**: Budget management system (efrit-budget.el)

### Fixed
- Fixed void function error requiring efrit-tool-utils at load time
- Improved read_image tool prompting to prevent find-file misuse
- Fixed sandbox bypass for read-only operations

## [0.3.0] - 2025-11-24

### Added
- **First working e2e multi-turn chat**: Complete request/response cycle with proper tool handling
- **Tool protocol fixes**: Proper tool_use_id capture and tool result submission
- **MCP server foundation**: Started TypeScript/Node MCP server implementation
- **Todo write system**: Implemented todo_write tool for session note-taking

### Changed
- **Tool protocol**: Completely rewrote tool result submission format
- **Circuit breaker**: Added better error recovery and action buttons

### Fixed
- Fixed tool use protocol to properly submit tool results back to Claude
- Fixed multibyte text in HTTP request errors
- Fixed streamlined mode tool_result format

## [0.2.0] and Earlier

Earlier versions focused on architecture modernization, refactoring, and laying groundwork for the public release.

---

## Legend

- **Added**: New features
- **Changed**: Changes in existing functionality
- **Fixed**: Bug fixes
- **Removed**: Removed functionality
- **Deprecated**: Deprecated functionality (will be removed in a future version)
