import Lake
open Lake DSL

package "ShapeChecker" where
  leanOptions := #[
    ⟨`pp.unicode.fun, true⟩
  ]
  extraCFlags := #["-I", (← getLeanIncludeDir).toString]

require "leanprover-community" / "batteries" @ "git#v4.13.0"
require alloy from git "https://github.com/tydeu/lean4-alloy.git"
require Parser from git "https://github.com/fgdorais/lean4-parser"

module_data alloy.c.o.export : BuildJob System.FilePath
module_data alloy.c.o.noexport : BuildJob System.FilePath

lean_lib «ShapeChecker» where
  precompileModules := true
  nativeFacets := fun shouldExport =>
    if shouldExport then #[Module.oExportFacet, `alloy.c.o.export]
    else #[Module.oNoExportFacet, `alloy.c.o.noexport]

target ffi.o pkg : FilePath := do
  let oFile := pkg.buildDir / "native" / "ffi.o"
  let srcJob <- inputTextFile <| pkg.dir / "ShapeChecker" / "Frontend" / "PythonAdapter.c"
  let leanIncludeDir ← getLeanIncludeDir
  let flags := #[
    "-I", "/opt/homebrew/opt/python@3.13/Frameworks/Python.framework/Versions/3.13/include/python3.13",
    "-I", leanIncludeDir.toString,
    "-fPIC"
  ]
  buildO oFile srcJob flags

extern_lib pythonParser pkg := do
  let name := nameToStaticLib "pythonParser"
  let ffiO <- fetch <| pkg.target ``ffi.o
  buildStaticLib (pkg.buildDir / "lib" / name) #[ffiO]

extern_lib pythonDylib := do
  return BuildJob.pure $ "/opt/homebrew/opt/python@3.13/Frameworks/Python.framework/Versions/3.13/lib/libpython3.13.dylib"

@[default_target]
lean_exe «ShapeCheckerExe» where
  root := `Main
  supportInterpreter := true
