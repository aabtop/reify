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

A great example of Reify usage can be seen in the [Hypo project](https://github.com/aabtop/reify/tree/master/projects/hypo), which generates 2D and 3D geometry data files (e.g. it supports [SVG](https://en.wikipedia.org/wiki/Scalable_Vector_Graphics) for 2D output and [STL](https://en.wikipedia.org/wiki/STL_(file_format)) for 3D output) when given TypeScript that describes the geometry. As an example, the file [eyes.ts](./projects/hypo/src/playground_workspace/example_scripts/eyes.ts) generates the following SVG output:

![Output of eyes.ts](./projects/hypo/readme_assets/eyes.svg)

The set of core Hypo types shared between TypeScript and C++ are defined in the file [ReifyInputInterface.hs](./projects/hypo/src/interface/ReifyInputInterface.hs), which also specifies other Reify configuration including the project's namespace ("hypo" in this case) and a directory where a domain-specific TypeScript library is defined which augments the generated TypeScript declarations.

Hypo's interaction with the Reify-generated static library is entirely confined to a couple pages of well-documented code in [main.cc](./projects/hypo/src/main.cc). After we obtain an output from executing a TypeScript function, we have a pure C++ object that we can pass off to a system that has no knowledge of TypeScript.  In Hypo's case, we pass the results off to code that uses the [CGAL library](https://www.cgal.org/) to generate and output geometry.

## Implementation Details

### TypeScript/JavaScript Environment

Reify internally uses the [V8 engine](https://v8.dev/) (the JavaScript engine which powers Chrome) to execute the compiled TypeScript.  The TypeScript compiler is written in TypeScript itself, and so the compiled-to-JavaScript TypeScript compiler is embedded within the C++ static library and executed through V8.

### Data Representation

The implementation enables efficient communication between V8 and C++ by making heavy use of the concept of immutable reference counted objects.  Whenever a V8 object is returned to C++, we create a reference counted pure C++ object with the same information contained in the V8 object, cache a reference to the C++ object inside of the V8 object, and then freeze the V8 object.  At this point both the C++ and V8 objects may no longer be mutated, and thus we achieve consistency without any subsequent interactions with the bindings layer on either side.  By requiring interface types to be immutable, we enable the V8 object reference graph to be paralleled by a C++ object reference graph, and the C++ object references may be effortlessly passed to other threads and systems.

### Type Definitions

The interface types are defined in Haskell, and the system for performing this
generation can be found at [src/idt](src/idt). The
subproject is called "IDT" which refers to "Interface Description Tree", expressing the preference to describe the interface's abstract syntax tree directly as opposed to using a specialized [IDL](https://en.wikipedia.org/wiki/Interface_description_language) language.  By describing the types directly in a first-class programming language, we are able to be more expressive, for example we can leverage Haskell's existing module system to create a modular definition of the interface types.

The source IDT file (e.g. [from hypo](./projects/hypo/src/interface/ReifyInputInterface.hs)) is used to generate three target interfaces:

1. The TypeScript interface, defined by [TargetTypeScript.hs](src/idt/src/targets/typescript/TargetTypeScript.hs).
1. The C++ V8 bindings layer, defined by [TargetCppV8.hs](src/idt/src/targets/typescript_in_cpp_via_v8/TargetCppV8.hs).
1. The pure C++ interface, defined by [TargetCppImmutableRefCounted.hs](src/idt/src/targets/pure_cpp/TargetCppImmutableRefCounted.hs).

Note that the interfaces described by the IDT are very abstract.  They can be used to generate C++ and TypeScript interfaces, but they may also be used to generate other backend targets, such as JSON and binary representations.

As a bonus, a description of the IDT types themselves can be defined by IDT, as is done in [IdtIdt.hs](src/idt/src/IdtIdt.hs).

## Development environment

The best source of truth for what that is are the Dockerfiles:

 * [Linux Dev Environment Dockerfile](/dockerdev/linux/Dockerfile)
 * [Windows Dev Environment Dockerfile](/dockerdev/linux/Dockerfile)

Projects depend on Reify via the [Bazel](https://bazel.build/) build system. Bazel offers the ability
to specify at build time which targets you would like generated, and how they
should be parameterized.
