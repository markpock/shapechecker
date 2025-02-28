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
require Parser from git "https://github.com/fgdorais/lean4-parser"

target pythonAdapter pkg : FilePath := do
  let srcDir := pkg.dir / "Backend"
  let outputFile := srcDir / "libpython_adapter.a"

  -- Create a job to track the source file
  -- let srcJob ← inputTextFile <| srcDir / "PythonAdapter.c"

  -- Run the system make command instead of trying to compile directly with Lake
  proc {
    cmd := "make"
    cwd := some srcDir
  }

  return BuildJob.pure outputFile


-- extern_lib python_native pkg :=
--    inputTextFile <| pkg.dir / python_include_dir / nameToStaticLib "python_native"

module_data alloy.c.o.export : BuildJob System.FilePath
module_data alloy.c.o.noexport : BuildJob System.FilePath

lean_lib «Backend» where
  precompileModules := true
  -- supportInterpreter := true
  nativeFacets := fun shouldExport =>
    if shouldExport then
      #[Module.oExportFacet, `alloy.c.o.export]
    else
      #[Module.oNoExportFacet, `alloy.c.o.noexport]
  -- weakLeancArgs := #[
  --   "-I", python_include_dir
  -- ]
  -- and whatever other configuration options you wish to add

-- target ffi.o pkg : FilePath := do
--   let oFile := pkg.buildDir / "c" / "ffi.o"
--   let srcJob ← inputTextFile <| pkg.dir / "c" / "ffi.cpp"
--   let weakArgs := #["-I", (← getLeanIncludeDir).toString]
--   buildO oFile srcJob weakArgs #["-fPIC"] "c++" getLeanTrace

-- extern_lib libleanffi pkg := do
--   let ffiO ← ffi.o.fetch
--   let name := nameToStaticLib "leanffi"
--   buildStaticLib (pkg.nativeLibDir / name) #[ffiO]

extern_lib extlib pkg := do

  -- Create a job to track the source file
  -- let srcJob ← inputTextFile <| srcDir / "PythonAdapter.c"

  -- Run the system make command instead of trying to compile directly with Lake
  proc {
    cmd := "make"
    cwd := some $ pkg.dir / "Backend"
  }

  println!(pkg.dir)
  proc {
    cmd := "ranlib"
    args := #["libpython_adapter.a"]
    cwd := some $ pkg.dir / "Backend"
  }

  return BuildJob.pure $ pkg.dir / "Backend" / "libpython_adapter.a"
  -- println! "Hello"
  -- ffiO
  -- buildStaticLib (pkg.nativeLibDir / (nameToStaticLib "leanffi")) #[ffiO]

extern_lib pythonItself := do
  return BuildJob.pure $ "/opt/homebrew/opt/python@3.13/Frameworks/Python.framework/Versions/3.13/lib/libpython3.13.dylib"

@[default_target]
lean_exe «BackendExe» where
  root := `Main
  -- Enables the use of the Lean interpreter by the executable (e.g.,
  -- `runFrontend`) at the expense of increased binary size on Linux.
  -- Remove this line if you do not need such functionality.
  supportInterpreter := true
  -- weakLeancArgs := #[
  --   "-I", python_include_dir
  -- ]
