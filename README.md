# Reify

Reify lets you embed a customized [TypeScript](https://www.typescriptlang.org/) runtime within a C++ application.

Reify is a C++ library generator. The input to Reify is a set of types which define the language in which the C++ host code will communicate with the embedded TypeScript code. The output from Reify is a statically linkable C++ library with the following components:

 1. A [TypeScript declaration file](https://www.typescriptlang.org/docs/handbook/declaration-files/introduction.html) which defines the input set of types in terms of pure TypeScript (e.g. no injected native types).
 1. A TypeScript compiler/executor.
 1. Bindings code which enable the host C++ application to execute TypeScript functions which accept and return types defined by the input set of types.
 1. A C++ header file that defines the input set of types in terms of pure C++ 17 concepts (e.g. structs and `std::variant` ).  Importantly, this header file knows nothing about TypeScript and depends only the C++ STL; the output objects are completely independent of the TypeScript runtime in which they were created.

A C++ application that links with the the Reify-generated library can easily execute TypeScript functions on the fly, receive the arbitrarily complex return
value output of the function calls as runtime-independent C++ objects, and then pass those C++ objects off to domain-specific processing components.

## Example Usage

A great example of Reify usage can be seen in the [Hypo project](https://github.com/aabtop/reify/tree/master/projects/hypo), which generates 2D and 3D geometry data files (e.g. it supports [SVG](https://en.wikipedia.org/wiki/Scalable_Vector_Graphics) for 2D output and [STL](https://en.wikipedia.org/wiki/STL_(file_format)) for 3D output) when given TypeScript that describes the geometry. As an example, the file [eyes.ts](./projects/hypo/src/example_scripts/eyes.ts) generates the following SVG output:

![Output of eyes.ts](./projects/hypo/readme_assets/eyes.svg)

The set of core Hypo types shared between TypeScript and C++ are defined in the file [ReifyInputInterface.hs](./projects/hypo/src/interface/ReifyInputInterface.hs), which also specifies other Reify configuration including the project's namespace ("hypo" in this case) and a directory where a domain-specific TypeScript library is defined which augments the generated TypeScript declarations.

Hypo's interaction with the Reify-generated static library is entirely confined to a couple pages of well-documented code in [main.cc](./projects/hypo/src/main.cc). After we obtain an output from executing a TypeScript function, we have a pure C++ object that we can pass off to a system that has no knowledge of TypeScript.  In Hypo's case, we pass the results off to code that uses the [CGAL library](https://www.cgal.org/) to generate and output geometry.

## Implementation Details

### TypeScript/JavaScript Environment
Reify internally uses the [V8 engine](https://v8.dev/) (the JavaScript engine which powers Chrome) to execute the compiled TypeScript.  The TypeScript compiler is written in TypeScript itself, and so the compiled-to-JavaScript TypeScript compiler is embedded within the C++ static library and executed through V8.

### Data Representation

The implementation enables efficient communication between V8 and C++ by making heavy use of the concept of immutable reference counted objects.  Whenever a V8 object is returned to C++, we create a reference counted pure C++ object with the same information contained in the V8 object, cache a reference to the C++ object inside of the V8 object, and then freeze the V8 object.  At this point both the C++ and V8 objects may no longer be mutated, and thus we achieve consistency without any subsequent interactions with the bindings layer on either side.  This system enables V8 object references to be paralleled by C++ object references, so that the object reference network structure is preserved.

### Type Definitions

The interface types are defined by Haskell, and the system for performing this
generation can be found at [src/third_party/idt](src/third_party/idt). The
subproject is called "IDT" which refers to "Interface Description Tree", expressing the preference to describe the interface's abstract syntax tree directly as opposed to using a specialized [IDL](https://en.wikipedia.org/wiki/Interface_description_language) language.  By describing the types directly in a first-class programming language, we are able to be more expressive, for example we can leverage Haskell's existing module system to create a modular definition of the interface types.

While only used by Reify at the time of this writing, IDT is meant to be
independent from Reify, though I don't think that's currently reflected in the codebase :(. The folder [src/interface](src/interface) contains the Reify-specific code that interacts with IDT.

The source IDT file (e.g. [from hypo](./projects/hypo/src/interface/ReifyInputInterface.hs)) is used to generate three target interfaces:

1. The TypeScript interface, defined by [TargetTypeScript.hs](src/third_party/idt/src/TargetTypeScript.hs).
1. The C++ V8 bindings layer, defined by [TargetCppV8.hs](src/third_party/idt/src/TargetCppV8.hs).
1. The pure C++ interface, defined by [TargetCppImmutableRefCounted.hs](src/third_party/idt/src/TargetCppImmutableRefCounted.hs).

Note that the interfaces described by the IDT are very abstract.  They can be used to generate C++ and TypeScript interfaces, but they may also be used to generate other backend targets, such as JSON and binary representations.

As a bonus, a description of the IDT types themselves can be defined by IDT, as is done in [IdtIdt.hs](src/third_party/idt/src/IdtIdt.hs).

## Development environment

The root [Dockerfile](Dockerfile) in this repository defines the basic Linux development environment setup, and so the project can always be built from within that container.  Outside of that though, all you need to have installed is [Nix](https://nixos.org/nix/).  From [Getting Nix](https://nixos.org/download.html):

``` 
curl -L https://nixos.org/nix/install | sh
```

### Building the project

Reify heavily depends on [Nix](https://nixos.org/nix/) for its high-level build
process.  When Reify generates a project, it is output as a [CMake](https://cmake.org/) project tree.

As a "library generator", Reify is ultimately defined by a Nix function whose
input is the path to the interface description (for example [ReifyInputInterface.hs](./projects/hypo/src/interface/ReifyInputInterface.hs) in Hypo).  Thus, there is nothing to actually build until an interface description is provided, at which point a library can be generated and built.

The primary Nix file which acts as the highest level Reify build function is defined in [src/nix/project.nix](project.nix), whose output is a directory tree that represents a CMake project which is the static library.  The generated CMake project can be built and referenced like any regular CMake project.

For an example of how to interact with the Reify project generator, see [default.nix from the Hypo project](projects/hypo/src/default.nix) which calls the
Reify function passing into it the Hypo interface.  It then sets up its own
CMake project to refer to the generated Reify CMake project, and then builds that.

Reify itself is a library generator so there is nothing to build until you give it an interface.  An example client however is the Hypo project which can be built with:

``` 
cd projects/hypo/src
nix-build
```

after which the output binary `projects/hypo/src/result/bin/hypo` will have been produced.

### Visual Studio Code

A [.devcontainer.json](.devcontainer.json) file exists to have Visual Studio Code automatically connect to a development container defined by the Dockerfile mentioned above.  This will not only enable you to instantly hit `CTRL+SHIFT+B` to build the project (it's currently configured to build Hypo), but GDB is configured within that environment so you can also set breakpoints and hit `F5` to start the debugger and step through the code.

Additionally, it is verified to work well with [Visual Studio Codespaces](https://visualstudio.microsoft.com/services/visual-studio-codespaces/) (and presumably [GitHub Codespaces](https://github.com/features/codespaces/)), so you can develop Reify in the cloud!

### Developing on/for Windows

Reify depends heavily on Nix, but Nix is Linux-only. As a result, you cannot build Reify natively on Windows.  Hope is not lost though!  Using [Docker Desktop for Windows](https://www.docker.com/products/docker-desktop), you can spin up a Linux development environment from the provided [Dockerfile](Dockerfile) and start running the usual Nix commands.  The resulting output of Reify is a CMake project source tree, and so once this is obtained it can be pulled out into a Windows environment and built there for Windows.
