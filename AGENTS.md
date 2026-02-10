# AGENTS.md

## Philosophy
This codebase intentionally combines:
- Emacs philosophy: extensible, hackable editor behavior.
- Suckless philosophy: configuration lives in source code.

Treat source code as the configuration interface.

## Architecture Contract
Use this structure as the default:
- `src/core/`: core editor mechanics and state machine.
- `src/keymaps.zig`: canonical source-of-truth for keybindings and which-key labels.
- `src/extensions/`: optional/extra features layered on top of core.

Current expected split:
- Core examples: editor loop/state, buffers, windows, terminal I/O, search, kill ring, keymap engine.
- Extension examples: fuzzy matching, syntax highlighting/tree-sitter.

## Source-As-Config Rules
- All default keychords must be defined in `src/keymaps.zig`.
- Which-key discoverability labels must be defined in `src/keymaps.zig`.
- Do not duplicate keybinding definitions inside `src/core/` files.
- Adding/changing behavior that is user-triggered should update keymaps in code, not external config files.

## Extension Workflow
For a new extension:
1. Add a Zig file in `src/extensions/`.
2. Wire extension behavior into core only where needed (minimal integration points).
3. Add command routing support in core/editor.
4. Bind keychords in `src/keymaps.zig`.
5. Ensure which-key label exists in `src/keymaps.zig`.
6. Add tests for happy path, cancel path (if modal), and one edge case.

## Non-Negotiables
- Keep default Emacs keybindings/keychords unless explicitly requested otherwise.
- Preserve Emacs behavior semantics over literal historical C internals.
- Keep changes incremental and test-backed.
- Avoid unrelated refactors in feature branches.

## Behavioral Invariants
- `C-x C-c` => `save-buffers-kill-terminal` behavior.
- `C-x s` => `save-some-buffers` behavior.
- `M-x` command naming stays Emacs-like.
- Dirty buffers without file path prompt for write path in save flow.
- Quit-after-save exits only when not canceled and with no save failures.

## Where To Edit
- Keybindings and command labels: `src/keymaps.zig`
- Core editor behavior/state: `src/core/editor.zig`
- Keymap engine primitives: `src/core/keymap.zig`
- Terminal input/output: `src/core/term.zig`
- Syntax/tree-sitter extension: `src/extensions/syntax.zig`
- Fuzzy extension: `src/extensions/fuzzy.zig`
- Build config: `build.zig`

## Required Workflow
1. Implement the smallest change that solves the requested behavior.
2. Update all impacted layers:
   - command routing
   - keybinding table/labels
   - minibuffer/status rendering (if relevant)
   - tests
3. Run validation before finishing.

## Validation Gate (Required)
- `zig fmt src/main.zig src/keymaps.zig src/core/*.zig src/extensions/*.zig`
- `zig build test`
- `zig build`

Do not claim completion if any required check fails.

## Dependencies
- System `tree-sitter` C headers/libs are required.
- Canonical checks for this repo are `zig build test` and `zig build`.

## Testing Policy
- Add/update targeted tests in:
  - `src/core/editor.zig`
  - `src/keymaps.zig`
  - `src/core/keymap.zig` (engine-level tests)
- Cover at least:
  - happy path
  - cancel/abort path (for modal flows)
  - one edge path relevant to the change

## Release Checklist
- Automated:
  - `zig build test`
  - `zig build`
- Manual smoke (`zig build run -- /tmp/mg-zig-smoke.txt`):
  - `M-x save-some-buffers`
  - `M-x save-buffers-kill-terminal`
  - `C-x s` with `y/n/!/q/C-g`
  - `C-x s` on no-file buffer prompts write path and saves on `Enter`
  - `C-x C-c` clean session exits immediately
  - `C-x C-c` dirty session enters save-before-quit flow
  - `C-x C-c` canceled by `q`/`C-g`
  - `C-x 2`, `C-x 3` split behavior and separators/modelines
  - `C-x C-f`, `C-x C-s`, `C-x C-w` basic file flow
