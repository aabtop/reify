# Defines a derivation for setting up a CMake project and its source code such
# that it's ready to be compiled.  The output of this derivation could be
# copied onto a Windows machine and compiled there.
{pkgs, runCommand, bash, coreutils, xxd, inputInterfacePath}:

let
  reifyInterface = pkgs.callPackage ../interface { inherit inputInterfacePath; };
  reifyTscWrapper = pkgs.callPackage ../tsc_wrapper {};
  reifyNamespace = reifyInterface.config.namespace;
  namespaceHeaderContents = ''
    #ifndef _REIFY_GENERATED_PROJECT_NAMESPACE_H_
    #define _REIFY_GENERATED_PROJECT_NAMESPACE_H_

    #define REIFY_GENERATED_PROJECT_NAMESPACE ${reifyNamespace}

    #endif  // _REIFY_GENERATED_PROJECT_NAMESPACE_H_
  '';
in
  runCommand
    "reify-${reifyInterface.config.namespace}-project"
    {
      buildInputs = [xxd];
      inherit reifyNamespace;
      cmake_project = ../cmake_project;
      ts_lib_dir =
          reifyInterface.inputInterfacePath + ("/" + reifyInterface.config.typescriptLibDir);
      inherit reifyTscWrapper;
      reifyInterfaceDefinitionFiles = reifyInterface.interfaceDefinitionFiles;
    }
    ''
      # Copy the static project files into the output project directory.
      mkdir -p $out
      cp -r $cmake_project/* $out/

      # Add the generated interface files to the generated source folder.
      mkdir -p $out/src_gen/public_include
      cp $reifyInterfaceDefinitionFiles/$reifyNamespace.h $out/src_gen/public_include
      cp $reifyInterfaceDefinitionFiles/reify_cpp_v8_interface.h $out/src_gen/public_include
      mkdir -p $out/src_gen/interface/cpp
      cp $reifyInterfaceDefinitionFiles/reify_cpp_v8_interface.cc \
         $out/src_gen/interface/cpp

      # Add the TypeScript interface files to the generated source folder.
      # This is just for reference, they aren't referenced by the build system,
      # instead only their C++ header file versions are referenced.
      mkdir -p $out/src_gen/interface/ts
      cp $reifyInterfaceDefinitionFiles/*.ts $out/src_gen/interface/ts
      cp $ts_lib_dir/lib.ts $out/src_gen/interface/ts

      # Setup a header file that defines the customized C++ namespace for this
      # project.
      echo "${namespaceHeaderContents}" > $out/src_gen/public_include/reify_generated_project_namespace.h

      # Convert the TypeScript interface files to header files for inclusion
      # in the C++ binary.
      ln -s $out/src_gen/interface/ts ts
      xxd -i ts/lib.ts > $out/src_gen/reify_interface_ts.h
      xxd -i ts/reify_ts_interface.ts > $out/src_gen/reify_generated_interface_ts.h

      # Add the TypeScript compiler JS binary to the generated source folder.
      ln -s $reifyTscWrapper tsc_wrapper.js
      xxd -i tsc_wrapper.js > $out/src_gen/tsc_wrapper.h

      # Finally generate a CMake include file providing a set of baked in
      # configuration options.
      echo "set(REIFY_GENERATED_PROJECT_NAMESPACE \"$reifyNamespace\")" > $out/src_gen/generated_reify_options.cmake
    ''
