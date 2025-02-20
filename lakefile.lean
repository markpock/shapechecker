import Lake
open Lake DSL


package "Backend" where
  -- Settings applied to both builds and interactive editing
  leanOptions := #[
    ⟨`pp.unicode.fun, true⟩ -- pretty-prints `fun a ↦ b`
  ]
  -- add any additional package configuration options here

require "leanprover-community" / "batteries" @ "git#v4.13.0"
require alloy from git "https://github.com/tydeu/lean4-alloy.git"


@[default_target]
lean_lib «Backend» where
  -- add any library configuration options here
