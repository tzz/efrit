# Tools the user and the model define together

Status: implemented in 0.20.0 (`efrit-tool-defs.el`).

## The problem

efrit's tools are fixed at build time. Every user has routines efrit
does not know: "run the project's test target and show the failures",
"open the Jira ticket named in the branch", "reformat this buffer the
way our linter wants". Today the model rebuilds each of those from
`eval_sexp` and `shell_exec` calls every time, and the reviewer and
the sandbox look at each rebuild as if it were new.

We do not know what users will want. So efrit does not ship the
tools: it ships the way to define them, and the model proposes one
when it sees itself repeat.

## A definition

A tool definition is one JSON object:

```json
{
  "name": "run_tests",
  "description": "Run the project's test target; return the failures.",
  "input_schema": {"type": "object",
                   "properties": {"target": {"type": "string"}},
                   "required": []},
  "class": "exec",
  "body": {"kind": "elisp",
           "source": "(efrit-tool-defs-shell (format \"make %s\" (or (alist-get \"target\" input nil nil #'equal) \"test\")))"},
  "origin": {"session": "…", "when": "2026-11-12T10:00:00Z", "pattern": "shell_exec×3"}
}
```

`name` is a lowercase identifier that is not one of efrit's own tools.
`class` is the permission class (`read`, `write`, `exec`, `net`); it
decides what the reviewer and the sandbox do with calls to the tool.
`body.kind` is `elisp` or `template`:

- `elisp`: `source` is one Lisp form evaluated with `input` bound to
  the tool input (an alist with string keys). It runs exactly like an
  `eval_sexp` form: static inspection, the dynamic guards, and the
  input block. A definition cannot do what `eval_sexp` cannot.
- `template`: `source` is a text with `{{field}}` placeholders filled
  from the input, then run as a `shell_exec` line. The sandbox judges
  the filled line as it judges any shell line.

`origin` is informational: who defined it and from what repetition.

## Where definitions live

Two files, both plain JSON, both editable by hand:

- global: `tools.json` in `efrit-data-directory`;
- per project: `.efrit/tools.json` under the project root.

A project definition shadows a global one of the same name. Both files
are read at load and at the start of each turn when their modification
time changed, so a hand edit takes effect without a reload. Each
definition becomes an entry in `efrit-tool-registry` (the same path
packages use), so the schema, dispatch and permission class need no
new code paths.

## Defining one: `define_tool`

The model calls `define_tool` with the definition and a `scope`
(`project` or `global`). The tool is `write` class, so the reviewer
sees it, and the sandbox asks the user once with the definition's
JSON in view: the prompt is the only consent step, and `e` at the
prompt edits the JSON before it is defined. No grant is recorded
(`efrit-sandbox-approve-once`): the files live under `.efrit/`, which
`efrit-sandbox-always-deny` keeps out of every grant, and a standing
grant would let the next definition in unseen. Each definition is
approved on its own.

A definition is validated before anything is written: name shape, no
collision with a built-in tool, class in the allowed set, body kind
known, an `elisp` body that reads as one form and passes the static
inspection (`efrit-sandbox-eval-inspect`), a `template` body whose
placeholders all name fields of the schema.

`undefine_tool` removes one by name and scope. `list_tools` (read
class) shows the model what is defined, with origins.

## Noticing a repetition

`efrit-tool-defs-watch` keeps the last tool names of a session. When
the same sequence of two to four calls appears three times in one
session, with the same tool names and the same shape of input
(`eval_sexp` forms with the same head symbol; `shell_exec` lines with
the same first word), a note is published:

    ⚙ you ran eval_sexp(project-root…) → shell_exec(make…) three times;
      define_tool can make that one call

and the same text goes into the next tool result as a `[note]` so the
model sees it. The note does not call `define_tool`: the model
proposes a definition and the user approves it at the prompt. A
sequence that was noted is not noted again in the session.

## Running one

An `elisp` definition is evaluated through `efrit-tools--eval-with-context`
with `input` let-bound, so it is observed the same way (messages,
changed buffers) and refused the same way. A `template` body goes
through the shell tool. Either way the call shows as a tool row named
after the definition, and the permission class is the definition's.

## What this is not

Not a plugin system: a definition cannot load code, require a
library, or outlive the sandbox. Not a macro over the model: the model
still decides when to call a tool and the user still owns consent.
Not hidden: `/tools` lists what is defined, and the JSON files are the
source of truth.
