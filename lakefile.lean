import Lake
open Lake DSL

package LeanCliTest where
  version := v!"0.1.0"

require LeanTest from git "https://github.com/cpehle/lean_test"

@[default_target]
lean_lib LeanCliTest where
  roots := #[`LeanCliTest]

@[test_driver]
lean_exe test where
  root := `TestDriver
