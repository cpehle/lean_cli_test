import Lean

namespace LeanCliTest
namespace Tmux

/--
Explicit tmux server assumptions.

- `socketName?` maps to `tmux -L`.
- `socketPath?` maps to `tmux -S` and overrides `socketName?`.
- `configFile?` maps to `tmux -f` (defaults to `/dev/null` for deterministic tests).
- `keepAlive` controls `set-option -g exit-empty`.
-/
structure ServerConfig where
  socketName? : Option String := none
  socketPath? : Option String := none
  configFile? : Option String := some "/dev/null"
  keepAlive : Bool := false
  deriving Repr, Inhabited

/-- Handle for a started tmux server context. -/
structure Server where
  config : ServerConfig
  deriving Repr, Inhabited

/--
Configuration for creating a tmux session/pane.

These are explicit terminal assumptions for TUI determinism.
-/
structure SessionConfig where
  /-- Optional explicit session name. If omitted, a unique name is generated. -/
  name? : Option String := none
  /-- Optional working directory for the initial tmux window. -/
  cwd? : Option System.FilePath := none
  /-- Optional shell/command used as the initial program in the window. -/
  shell? : Option String := none
  /-- Pane width in columns. -/
  cols : Nat := 120
  /-- Pane height in rows. -/
  rows : Nat := 40
  /-- Retained history lines for `capture-pane`. -/
  historyLimit : Nat := 50000
  /-- Additional environment variables passed to `tmux new-session`. -/
  env : Array (Prod String String) := #[]
  deriving Repr, Inhabited

/-- Handle for an active tmux session and pane target. -/
structure Session where
  name : String
  target : String
  server : ServerConfig
  deriving Repr, Inhabited

/-- Configuration for `capture-pane`. -/
inductive ScreenSource where
  /-- Default pane view (visible area + history, depending on bounds). -/
  | visible
  /-- Alternate screen (`capture-pane -a`), typically where TUIs render. -/
  | alternate
  /-- Pane mode screen (`capture-pane -M`), e.g. copy-mode content. -/
  | mode
  deriving Repr, Inhabited, BEq, DecidableEq

inductive StartBound where
  /-- Absolute/relative line index for `-S`. -/
  | line (n : Int)
  /-- Start of history (`capture-pane -S -`). -/
  | historyStart
  deriving Repr, Inhabited, BEq, DecidableEq

inductive EndBound where
  /-- Absolute/relative line index for `-E`. -/
  | line (n : Int)
  /-- End of visible pane (`capture-pane -E -`). -/
  | visibleBottom
  deriving Repr, Inhabited, BEq, DecidableEq

inductive WrapMode where
  /-- Keep physical wrapped lines as separate lines. -/
  | keepPhysicalLines
  /-- Join wrapped lines (`capture-pane -J`). -/
  | joinWrapped
  deriving Repr, Inhabited, BEq, DecidableEq

inductive TrailingCellMode where
  /-- Let tmux decide default trailing-cell behavior. -/
  | tmuxDefault
  /-- Preserve trailing spaces at each line end (`capture-pane -N`). -/
  | preserveTrailingSpaces
  /-- Trim trailing empty-cell positions (`capture-pane -T`). -/
  | trimEmptyCells
  deriving Repr, Inhabited, BEq, DecidableEq

inductive NonPrintableMode where
  /-- Keep non-printable bytes as-is. -/
  | raw
  /-- Escape non-printables as octal `\\xxx` (`capture-pane -C`). -/
  | octalEscaped
  deriving Repr, Inhabited, BEq, DecidableEq

structure CaptureConfig where
  source : ScreenSource := .visible
  /-- Start line for capture; default `line (-200)` matches previous behavior. -/
  start : StartBound := .line (-200)
  /-- End line for capture; default `visibleBottom` matches previous `"-"` behavior. -/
  stop : EndBound := .visibleBottom
  /-- Keep ANSI escapes for styling/color assertions (`capture-pane -e`). -/
  includeEscapes : Bool := false
  /-- Join wrapped lines by default (matches previous behavior). -/
  wrap : WrapMode := .joinWrapped
  /-- Trailing-cell mode. Only used when `wrap = keepPhysicalLines`. -/
  trailingCells : TrailingCellMode := .tmuxDefault
  /-- Handling for non-printable bytes. -/
  nonPrintable : NonPrintableMode := .raw
  /-- Include incomplete escape-prefix fragments (`capture-pane -P`). -/
  includePartialEscapePrefix : Bool := false
  /-- Quiet errors (for example when alternate screen is unavailable). -/
  quiet : Bool := false
  deriving Repr, Inhabited

/--
Structured snapshot of a pane capture.

`text` is newline-delimited output exactly as returned by `tmux capture-pane`.
-/
structure Snapshot where
  target : String
  text : String
  lines : Array String
  source : ScreenSource
  start : StartBound
  stop : EndBound
  includeEscapes : Bool
  wrap : WrapMode
  trailingCells : TrailingCellMode
  nonPrintable : NonPrintableMode
  includePartialEscapePrefix : Bool
  quiet : Bool
  deriving Repr, Inhabited

/-- Configuration for polling/waiting on tmux output. -/
structure WaitConfig where
  timeoutMs : Nat := 3000
  pollMs : Nat := 50
  capture : CaptureConfig := {}
  deriving Repr, Inhabited

private def serverCliArgs (server : ServerConfig) : Array String := Id.run do
  let mut args := #[]
  if let some socketPath := server.socketPath? then
    args := args.push "-S"
    args := args.push socketPath
  else if let some socketName := server.socketName? then
    args := args.push "-L"
    args := args.push socketName
  if let some cfg := server.configFile? then
    args := args.push "-f"
    args := args.push cfg
  args

private def runTmux
    (server : ServerConfig)
    (args : Array String)
    (cwd? : Option System.FilePath := none) : IO IO.Process.Output := do
  IO.Process.output {
    cmd := "tmux"
    args := serverCliArgs server ++ args
    cwd := cwd?
  }

private def renderCommand (server : ServerConfig) (args : Array String) : String :=
  "tmux " ++ String.intercalate " " (serverCliArgs server ++ args).toList

private def runTmuxChecked
    (server : ServerConfig)
    (args : Array String)
    (cwd? : Option System.FilePath := none) : IO String := do
  let out <- runTmux server args cwd?
  if out.exitCode != 0 then
    let stderr := out.stderr.trimAscii.toString
    throw <| IO.userError s!"{renderCommand server args} failed with exit code {out.exitCode}\n{stderr}"
  pure out.stdout

private def isNoServerError (stderr : String) : Bool :=
  stderr.contains "no server running" || stderr.contains "can't find"

/-- Verify that tmux is installed and executable. -/
def ensureAvailable : IO Unit := do
  discard <| runTmuxChecked {} #["-V"]

private def mkFreshName (stem : String) : IO String := do
  let now <- IO.monoMsNow
  pure s!"{stem}-{now}"

/-- Create a fresh socket name for isolated tmux servers. -/
def freshSocketName : IO String :=
  mkFreshName "lean-cli-test-socket"

private def mkFreshSessionName : IO String :=
  mkFreshName "lean-cli-test"

private def StartBound.toTmuxArg : StartBound -> String
  | .line n => toString n
  | .historyStart => "-"

private def EndBound.toTmuxArg : EndBound -> String
  | .line n => toString n
  | .visibleBottom => "-"

/--
Start (or attach to) a tmux server context based on `ServerConfig`.

`keepAlive` is applied when sessions are created.
-/
def startServer (config : ServerConfig := {}) : IO Server := do
  ensureAvailable
  discard <| runTmuxChecked config #["start-server"]
  pure { config := config }

/--
Stop a tmux server context by killing its server.

If no server is running for the selected socket/config, this is treated as success.
-/
def stopServer (server : Server) : IO Unit := do
  let out <- runTmux server.config #["kill-server"]
  if out.exitCode != 0 then
    let stderr := out.stderr.trimAscii.toString
    if isNoServerError stderr then
      pure ()
    else
      throw <| IO.userError s!"failed to kill tmux server: {stderr}"

/--
Run an action with an explicit tmux server context.

Use `startServer`/`stopServer` directly if you want a longer-lived server/socket.
-/
def withServer (action : Server -> IO a) (config : ServerConfig := {}) : IO a := do
  let server <- startServer config
  try
    action server
  finally
    stopServer server

/-- Check whether a session exists on a specific server context. -/
def hasSession (server : Server) (sessionName : String) : IO Bool := do
  let out <- runTmux server.config #["has-session", "-t", sessionName]
  pure (out.exitCode == 0)

/-- Start a detached tmux session and return a session handle. -/
def startSession (server : Server) (config : SessionConfig := {}) : IO Session := do
  let generatedName <- mkFreshSessionName
  let name := config.name?.getD generatedName

  let mut args := #[
    "new-session", "-d", "-P", "-F", "#{session_name}:#{window_index}.#{pane_index}",
    "-s", name,
    "-x", toString config.cols,
    "-y", toString config.rows
  ]

  if let some cwd := config.cwd? then
    args := args.push "-c"
    args := args.push cwd.toString

  for (key, value) in config.env do
    args := args.push "-e"
    args := args.push s!"{key}={value}"

  if let some shell := config.shell? then
    args := args.push shell

  let targetOut <- runTmuxChecked server.config args
  let target := targetOut.trimAscii.toString
  let target := if target.isEmpty then s!"{name}:0.0" else target

  discard <| runTmuxChecked server.config #[
    "set-option",
    "-t", name,
    "history-limit", toString config.historyLimit
  ]

  if server.config.keepAlive then
    discard <| runTmuxChecked server.config #[
      "set-option",
      "-g",
      "exit-empty",
      "off"
    ]

  pure { name, target, server := server.config }

/--
Kill a tmux session.

If the session is already missing, this is treated as success.
-/
def killSession (session : Session) : IO Unit := do
  let out <- runTmux session.server #["kill-session", "-t", session.name]
  if out.exitCode != 0 then
    let stderr := out.stderr.trimAscii.toString
    if stderr.contains "can't find session" || stderr.contains "no server running" then
      pure ()
    else
      throw <| IO.userError s!"failed to kill tmux session '{session.name}': {stderr}"

/--
Run an action inside a fresh session on an already-started tmux server.
-/
def withSessionOn (server : Server) (action : Session -> IO a) (config : SessionConfig := {}) : IO a := do
  let session <- startSession server config
  try
    action session
  finally
    killSession session

/--
Run an action in a fresh session, using an ephemeral tmux server context.
-/
def withSession
    (action : Session -> IO a)
    (config : SessionConfig := {})
    (serverConfig : ServerConfig := {}) : IO a := do
  withServer (config := serverConfig) fun server =>
    withSessionOn server action config

/-- Send key names directly (`Enter`, `C-c`, `Up`, etc.) using `tmux send-keys`. -/
def sendKeys (session : Session) (keys : Array String) : IO Unit := do
  if keys.isEmpty then
    pure ()
  else
    let args := #["send-keys", "-t", session.target] ++ keys
    discard <| runTmuxChecked session.server args

/--
Send literal text to the active pane.

When `pressEnter` is true, an additional `C-m` keypress is sent afterwards.
-/
def sendText (session : Session) (text : String) (pressEnter : Bool := true) : IO Unit := do
  if !text.isEmpty then
    discard <| runTmuxChecked session.server #["send-keys", "-t", session.target, "-l", text]
  if pressEnter then
    sendKeys session #["C-m"]

/-- Interrupt the foreground process in the pane with Ctrl-C. -/
def sendCtrlC (session : Session) : IO Unit :=
  sendKeys session #["C-c"]

/-- Capture a structured snapshot from pane/history. -/
def captureSnapshot (session : Session) (config : CaptureConfig := {}) : IO Snapshot := do
  if config.wrap == .joinWrapped && config.trailingCells != .tmuxDefault then
    throw <| IO.userError
      "invalid capture config: trailingCells is only applied when wrap = keepPhysicalLines"

  let mut args := #[
    "capture-pane", "-p",
    "-t", session.target,
    "-S", config.start.toTmuxArg,
    "-E", config.stop.toTmuxArg
  ]

  match config.source with
  | .visible => pure ()
  | .alternate => args := args.push "-a"
  | .mode => args := args.push "-M"

  if config.includeEscapes then
    args := args.push "-e"

  match config.wrap with
  | .joinWrapped =>
    args := args.push "-J"
  | .keepPhysicalLines =>
    match config.trailingCells with
    | .tmuxDefault => pure ()
    | .preserveTrailingSpaces => args := args.push "-N"
    | .trimEmptyCells => args := args.push "-T"

  match config.nonPrintable with
  | .raw => pure ()
  | .octalEscaped => args := args.push "-C"

  if config.includePartialEscapePrefix then
    args := args.push "-P"
  if config.quiet then
    args := args.push "-q"

  let text <- runTmuxChecked session.server args
  pure {
    target := session.target
    text := text
    lines := (text.splitOn "\n").toArray
    source := config.source
    start := config.start
    stop := config.stop
    includeEscapes := config.includeEscapes
    wrap := config.wrap
    trailingCells := config.trailingCells
    nonPrintable := config.nonPrintable
    includePartialEscapePrefix := config.includePartialEscapePrefix
    quiet := config.quiet
  }

/-- Capture output text from pane/history. -/
def capturePane (session : Session) (config : CaptureConfig := {}) : IO String := do
  let snapshot <- captureSnapshot session config
  pure snapshot.text

/--
Poll pane content until `predicate` succeeds or timeout is reached.

Returns the first captured pane output that satisfies the predicate.
-/
def waitUntil (session : Session) (predicate : String -> Bool) (config : WaitConfig := {}) : IO String := do
  let start <- IO.monoMsNow
  let mut latest := ""
  while true do
    let snapshot <- captureSnapshot session config.capture
    latest := snapshot.text
    if predicate latest then
      return latest

    let now <- IO.monoMsNow
    if now - start >= config.timeoutMs then
      throw <| IO.userError (
        s!"timed out after {config.timeoutMs}ms while waiting on tmux target '{session.target}'\n" ++
        s!"Last capture:\n{latest}"
      )

    IO.sleep (UInt32.ofNat config.pollMs)

  pure latest

/-- Wait until the captured pane contains `needle` as a substring. -/
def waitForSubstring (session : Session) (needle : String) (config : WaitConfig := {}) : IO String :=
  waitUntil session (fun screen => screen.contains needle) config

/-- Convenience helper: send text, then wait for a specific substring in the pane output. -/
def sendAndWaitForSubstring
    (session : Session)
    (text : String)
    (needle : String)
    (wait : WaitConfig := {})
    (pressEnter : Bool := true) : IO String := do
  sendText session text pressEnter
  waitForSubstring session needle wait

end Tmux
end LeanCliTest
