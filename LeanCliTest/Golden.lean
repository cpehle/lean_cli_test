import LeanCliTest.Tmux
import LeanTest

namespace LeanCliTest
namespace Golden

/--
Configuration for golden snapshot assertions.

If `update` is `none`, update mode is controlled by `updateEnvVar`.
-/
structure AssertConfig where
  /-- Explicit update mode override. If `none`, read from `updateEnvVar`. -/
  update : Option Bool := none
  /-- Environment variable checked when `update = none`. -/
  updateEnvVar : String := "UPDATE_GOLDENS"
  /-- Write an `.actual.snap` file when a mismatch is detected. -/
  writeActualOnMismatch : Bool := true
  /-- Normalize line endings to `\n` before comparing. -/
  normalizeLineEndings : Bool := true
  deriving Repr, Inhabited

private def normalizeText (text : String) (cfg : AssertConfig) : String :=
  if cfg.normalizeLineEndings then
    (text.replace "\r\n" "\n").replace "\r" "\n"
  else
    text

private def parseEnabledFlag (raw : String) : Bool :=
  let value := raw.trimAscii.toString.toLower
  !(value.isEmpty || value == "0" || value == "false" || value == "no" || value == "off")

private def resolveUpdateMode (cfg : AssertConfig) : IO Bool := do
  match cfg.update with
  | some enabled => pure enabled
  | none =>
    match <- IO.getEnv cfg.updateEnvVar with
    | some raw => pure (parseEnabledFlag raw)
    | none => pure false

private def ensureParentDir (path : System.FilePath) : IO Unit := do
  match path.parent with
  | some parent => IO.FS.createDirAll parent
  | none => pure ()

private def actualPathFor (goldenPath : System.FilePath) : System.FilePath :=
  match goldenPath.extension with
  | some ext => goldenPath.withExtension s!"actual.{ext}"
  | none => System.FilePath.mk (goldenPath.toString ++ ".actual")

private def firstDiffLine? (expected actual : String) : Option Nat := Id.run do
  let rec go (expectedLines actualLines : List String) (line : Nat) : Option Nat :=
    match expectedLines, actualLines with
    | [], [] => none
    | expectedLine :: expectedTail, actualLine :: actualTail =>
      if expectedLine == actualLine then
        go expectedTail actualTail (line + 1)
      else
        some line
    | _, _ => some line
  go (expected.splitOn "\n") (actual.splitOn "\n") 1

/--
Assert that `actualText` matches the golden snapshot at `goldenPath`.

Update mode writes/overwrites the golden file. Verify mode compares and fails on mismatch.
-/
def assertText
    (goldenPath : System.FilePath)
    (actualText : String)
    (cfg : AssertConfig := {}) : IO Unit := do
  let updateMode <- resolveUpdateMode cfg
  let actual := normalizeText actualText cfg

  if updateMode then
    ensureParentDir goldenPath
    IO.FS.writeFile goldenPath actual
    return ()

  if !(<- goldenPath.pathExists) then
    LeanTest.fail s!"missing golden snapshot: {goldenPath}\nSet {cfg.updateEnvVar}=1 to create it."

  let expected <- IO.FS.readFile goldenPath
  let expected := normalizeText expected cfg
  if expected == actual then
    return ()

  let diffLine := firstDiffLine? expected actual
  let actualPath := actualPathFor goldenPath
  if cfg.writeActualOnMismatch then
    ensureParentDir actualPath
    IO.FS.writeFile actualPath actual

  let lineHint := match diffLine with
    | some line => s!"first differing line: {line}"
    | none => "snapshot contents differ"
  let actualHint := if cfg.writeActualOnMismatch then
    s!"\nWrote actual snapshot: {actualPath}"
  else
    ""

  LeanTest.fail
    s!"snapshot mismatch: {goldenPath}\n{lineHint}\nExpected length: {expected.length}, actual length: {actual.length}{actualHint}"

/-- Assert a tmux snapshot against a golden file path. -/
def assertSnapshot
    (goldenPath : System.FilePath)
    (snapshot : Tmux.Snapshot)
    (cfg : AssertConfig := {}) : IO Unit :=
  assertText goldenPath snapshot.text cfg

/-- Capture a snapshot and assert it against a golden file path. -/
def assertPaneSnapshot
    (session : Tmux.Session)
    (goldenPath : System.FilePath)
    (capture : Tmux.CaptureConfig := {})
    (cfg : AssertConfig := {}) : IO Unit := do
  let snapshot <- Tmux.captureSnapshot session capture
  assertSnapshot goldenPath snapshot cfg

end Golden
end LeanCliTest
