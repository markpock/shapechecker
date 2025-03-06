import Lake
open Lake DSL

package "Backend" where
  -- Settings applied to both builds and interactive editing
  leanOptions := #[
    ⟨`pp.unicode.fun, true⟩ -- pretty-prints `fun a ↦ b`
  ]

require "leanprover-community" / "batteries" @ "git#v4.13.0"
require alloy from git "https://github.com/tydeu/lean4-alloy.git"
require Parser from git "https://github.com/fgdorais/lean4-parser"

module_data alloy.c.o.export : BuildJob System.FilePath
module_data alloy.c.o.noexport : BuildJob System.FilePath

lean_lib «Backend» where
  precompileModules := true
  nativeFacets := fun shouldExport =>
    if shouldExport then #[Module.oExportFacet, `alloy.c.o.export]
    else #[Module.oNoExportFacet, `alloy.c.o.noexport]

-- extern_lib extlib pkg := do
--   -- Supposedly, tracks PythonAdapter.c as an input.
--   let _ <- inputTextFile <| pkg.dir / "Backend" / "PythonAdapter.c"

--   proc {
--     cmd := "make"
--     cwd := some $ pkg.dir / "Backend"
--   }

--   proc {
--     cmd := "ranlib"
--     args := #["libpython_adapter.a"]
--     cwd := some $ pkg.dir / "build"
--   }

--   return .pure $ pkg.dir / "build" / "libpython_adapter.a"

extern_lib pythonItself := do
  return BuildJob.pure $ "/opt/homebrew/opt/python@3.13/Frameworks/Python.framework/Versions/3.13/lib/libpython3.13.dylib"

@[default_target]
lean_exe «BackendExe» where
  root := `Main
  supportInterpreter := true
