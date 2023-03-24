# WebAssembly specification DSL

**Archived in favour of <https://github.com/Wasm-DSL/spectec>.**

This project provides a domain-specific language (DSL) for describing [WebAssembly](https://webassembly.org) (or Wasm) [specification](https://webassembly.github.io/spec/core/). This can then be used as the single canonical representation, from which one can automatically derive:

- human-readable specification,
- a reference interpreter,
- stubs for proof assistants, and more.

The DSL is written as a library in [OCaml](https://ocaml.org).

## Motivation

The Wasm specification is light years ahead of specifications for [other](https://www.ecma-international.org/publications-and-standards/standards/ecma-262/) [languages](https://isocpp.org/std/the-standard) when it comes to clarity, unambiguity and size. Even so, it can be improved:

- Currently, each validation and execution rule is written twice, once in prose and once as a mathematical judgement. This can lead to inconsistency and provides an additional burden on writers of the specification.
- The mathematical judgements are written in LaTeX, which is not everyone's cup of tea.
- The specification is not machine readable (unless one wants to parse prose or LaTeX) and cannot be used in further analysis.

The problem is becoming more and annoying with the growth of the specification.

## How to use

The easiest way to test the project is using [dune](https://dune.build). Once installed, you can rune `make`, which will result in a `main.exe` executable. Running it will produce a human-readable output of the currently implemented specification.

## Explanation of design choices

### Why not use an existing tool like [Ott](https://github.com/ott-lang/ott)?

The reason is twofold. First, none of the existing tools support the exact outputs we want to achieve without significant rewrite. Second, the structure of the Wasm specification (syntactic families, typing and reduction rules, …) are well established, and putting them in a general tool discards a significant amount of domain-specific knowledge.

### Why an embedded DSL instead of a new language?

The main reason is to not introduce [yet another standard](https://xkcd.com/927/). Furthermore, defining a new language requires a parser, a runtime, and so on. The additional flexibility that an independent DSL brings is relatively small if one uses a reasonably flexible host language.

### Why OCaml?

OCaml is a reasonably flexible host language. Furthermore, it is well established in the WebAssembly community, being the language
in which the reference interpreter is written.

### Have you considered writing your DSL in [tagless-final style](https://discuss.ocaml.org/t/explain-like-im-5-years-old-tagless-final-pattern/9394)?

Yes, the first prototype included a [tagless-final variant](https://github.com/matijapretnar/wasm-spec-dsl/blob/cafbbec16a4a85b28e065f05a0396aed90825e58/tagless.ml). It turned out to be longer than the usual tagged representation, was more confusing for newcomers, and made it harder to perform manipulations (which is the main goal of the project).
