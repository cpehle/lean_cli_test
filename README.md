# LeanCliTest

Lean 4 helpers for testing CLI and TUI apps with `tmux`.

## Features

- Isolated tmux server/session lifecycle
- Send text and keys (`sendText`, `sendKeys`, `sendCtrlC`)
- Capture rendered pane snapshots (`captureSnapshot`)
- Expect-style waits (`waitForSubstring`)
- Golden snapshot comparison (`Golden.assertPaneSnapshot`)

## Requirements

- Lean toolchain: `leanprover/lean4:nightly-2025-12-01`
- `tmux` on `PATH`

## Install and Test

```bash
lake update
lake build
lake test
```

## Quick Example

```lean
import LeanCliTest

open LeanCliTest

def smoke : IO Unit := do
  Tmux.withSession (fun session => do
    Tmux.sendText session "printf 'hello\\n'"
    discard <| Tmux.waitForSubstring session "hello"
  )
```

## Golden Snapshot Workflow

```lean
let goldenPath : System.FilePath := "testdata/golden/cat_echo.snap"
Golden.assertPaneSnapshot session goldenPath {
  source := .visible
  start := .line 0
  stop := .visibleBottom
  wrap := .keepPhysicalLines
  trailingCells := .trimEmptyCells
}
```

- Verify mode (default): compare against committed golden file.
- On mismatch: test fails and writes `*.actual.snap`.
- Update mode: `UPDATE_GOLDENS=1 lake test`.
