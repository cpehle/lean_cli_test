import LeanCliTest
import LeanTest

namespace LeanCliTest.TestSuite

open LeanCliTest

def waitCfg : Tmux.WaitConfig := {
  timeoutMs := 5000
  pollMs := 40
}

@[test]
def testRunsSingleCommand : IO Unit := do
  Tmux.withSession (config := { cols := 100, rows := 30 }) (fun session => do
    Tmux.sendText session "printf 'hello-from-lean-cli-test\\n'"
    let pane <- Tmux.waitForSubstring session "hello-from-lean-cli-test" waitCfg
    LeanTest.assertTrue (pane.contains "hello-from-lean-cli-test")
  )

@[test]
def testInteractsWithPromptedProcess : IO Unit := do
  Tmux.withSession (fun session => do
    Tmux.sendText session "sh -c 'echo ready-for-input; read value; echo got:$value'"
    discard <| Tmux.waitForSubstring session "ready-for-input" waitCfg
    Tmux.sendText session "banana"
    discard <| Tmux.waitForSubstring session "got:banana" waitCfg
    Expect.assertPaneContains session "got:banana"
  )

@[test]
def testCtrlCStopsForegroundProgram : IO Unit := do
  Tmux.withSession (fun session => do
    Tmux.sendText session "cat"
    Tmux.sendText session "line-through-cat"
    discard <| Tmux.waitForSubstring session "line-through-cat" waitCfg
    Tmux.sendCtrlC session
    Tmux.sendText session "echo after-ctrl-c"
    discard <| Tmux.waitForSubstring session "after-ctrl-c" waitCfg
  )

@[test_should_error]
def testWaitForSubstringTimesOut : IO Unit := do
  Tmux.withSession (fun session => do
    discard <| Tmux.waitForSubstring session "this-needle-will-never-appear" {
      timeoutMs := 300
      pollMs := 50
    }
  )

@[test]
def testSnapshotFormat : IO Unit := do
  Tmux.withSession (fun session => do
    Tmux.sendText session "printf 'snapshot-line-1\\nsnapshot-line-2\\n'"
    discard <| Tmux.waitForSubstring session "snapshot-line-2" waitCfg
    let snap <- Tmux.captureSnapshot session
    LeanTest.assertTrue (snap.source == .visible)
    LeanTest.assertTrue (snap.wrap == .joinWrapped)
    LeanTest.assertTrue (snap.includeEscapes == false)
    LeanTest.assertTrue (snap.start == .line (-200))
    LeanTest.assertTrue (snap.stop == .visibleBottom)
    LeanTest.assertTrue (snap.text.contains "snapshot-line-1")
    LeanTest.assertTrue ((snap.lines.toList.any fun line => line.contains "snapshot-line-2"))
  )

@[test]
def testCanReuseExplicitServerSocket : IO Unit := do
  let socket <- Tmux.freshSocketName
  let server <- Tmux.startServer {
    socketName? := some socket
    keepAlive := true
  }
  try
    Tmux.withSessionOn server (config := { cols := 90, rows := 20 }) (fun session => do
      Tmux.sendText session "printf 'phase-1\\n'"
      discard <| Tmux.waitForSubstring session "phase-1" waitCfg
    )
    Tmux.withSessionOn server (fun session => do
      Tmux.sendText session "printf 'phase-2\\n'"
      discard <| Tmux.waitForSubstring session "phase-2" waitCfg
    )
  finally
    Tmux.stopServer server

@[test]
def testGoldenSnapshotComparison : IO Unit := do
  let goldenPath : System.FilePath := "testdata/golden/cat_echo.snap"
  Tmux.withSession (config := {
    shell? := some "cat"
    cols := 20
    rows := 4
  }) (fun session => do
    Tmux.sendText session "golden-check"
    discard <| Tmux.waitForSubstring session "golden-check" waitCfg
    Golden.assertPaneSnapshot session goldenPath {
      source := .visible
      start := .line 0
      stop := .visibleBottom
      wrap := .keepPhysicalLines
      trailingCells := .trimEmptyCells
    }
  )

@[test]
def testGoldenUpdateModeWritesFile : IO Unit := do
  let now <- IO.monoMsNow
  let path : System.FilePath := System.FilePath.mk s!"/tmp/lean-cli-test-update-{now}.snap"
  Golden.assertText path "updated-snapshot\n" { update := some true }
  let fileExists <- path.pathExists
  LeanTest.assertTrue fileExists
  let content <- IO.FS.readFile path
  LeanTest.assertEqual content "updated-snapshot\n"
  IO.FS.removeFile path

end LeanCliTest.TestSuite
