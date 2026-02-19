import LeanCliTest.Tmux
import LeanTest

namespace LeanCliTest
namespace Expect

/-- Assert that the current pane capture contains `needle`. -/
def assertPaneContains
    (session : Tmux.Session)
    (needle : String)
    (capture : Tmux.CaptureConfig := {}) : IO Unit := do
  let pane <- Tmux.capturePane session capture
  if pane.contains needle then
    pure ()
  else
    LeanTest.fail s!"expected tmux pane to contain '{needle}'\nCaptured pane:\n{pane}"

/-- Assert that `needle` appears in the pane before timeout. -/
def assertEventuallyContains
    (session : Tmux.Session)
    (needle : String)
    (wait : Tmux.WaitConfig := {}) : IO Unit := do
  discard <| Tmux.waitForSubstring session needle wait

end Expect
end LeanCliTest
