# Backlog: emacs-in-buffer-completion

## What

Add in-buffer completion to the Emacs completion stack, the one part
of the Vertico/Consult migration that was scoped as "Future" and never
built:

- **Corfu** — in-buffer completion popup (the modern replacement for
  Company).
- **Cape** — completion-at-point extensions that feed Corfu additional
  backends (dabbrev, file paths, keywords).

The minibuffer half of the stack is done and in use: Vertico,
Orderless, Marginalia, Consult, and Embark all live in
`emacs/core.el`. Corfu/Cape are the buffer-local complement — they
complete *at point* while typing, where the shipped stack completes in
the minibuffer.

## Why

The completion migration replaced Ivy/Helm and stopped at the
minibuffer boundary, leaving in-buffer completion on whatever Emacs
does by default. It was marked "Next Phase (Future)" in the feature
spec rather than dropped, so the want is real but was never urgent
enough to schedule.

Filed as backlog when the completion feature spec was closed out, so
the deferred want survives in the right tier rather than as an
unticked box on a landed spec.

## Open questions

- Does in-buffer completion actually earn its keep here? The shipped
  minibuffer stack covers most navigation; Corfu changes typing
  ergonomics, which is a stronger personal-taste call. Worth a short
  trial before committing it to `emacs/core.el`.
- Terminal compatibility is the known sharp edge — Corfu's popup is
  a child frame, which doesn't render in a TTY. `corfu-terminal`
  exists for that case. Since this env is used headless over SSH on
  the kelasa targets, the terminal path is the primary one, not the
  fallback.
- Which Cape backends are worth wiring, if any. The full set is more
  than this config needs.
- Interaction with the per-language modules in `emacs/*-mode.el` and
  any LSP completion they set up.

## Related

- `emacs/core.el` — where the shipped Vertico/Consult/Embark stack
  lives; Corfu/Cape would join it.
- `home/emacs.nix` — aggregated Emacs module; new packages land via
  the ELPA path it manages.
