import Lake
open Lake DSL

package presto {
  -- add package configuration options here
}

@[default_target]
lean_exe presto {
  root := `Main
}
