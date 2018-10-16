This repo is here to explore designs and architectures for new stanc, `stanc3`.
We'll go through some goals of the compiler redesign and new AST,
a roadmap for how the work splits up over time, and goals for additions to the Stan language (these may be moved later).

# Pain points with the current `stanc` architecture
1. C++ is a pain to write optimization and type-checking passes in; adding a language feature touches 40+ files
2. No one has wanted to work on the compiler (probably because of C++ + Spirit Qi)
3. Distribution is a pain (targets C++ and requires C++ toolchain at runtime)
4. Difficult for possible contributors to jump in - people tend to compile TO Stan, [rewrite a Stan parser in another language](https://github.com/deepppl/yaps/blob/master/yaps/stan.g4), or trick the compiler into emitting the AST as text so they can read it in somewhere else.

# Timeline for a New Stanc
1. Create skeleton end-to-end functional interpreters in both Rust an OCaml displaying a minimum non-trivial operation in each module (Nov 2018)
1. Choose Rust or OCaml and
    1. announce project seeking help,
    1. Define AST [Middle Intermediate Representation](https://blog.rust-lang.org/2016/04/19/MIR.html) equivalent
    1. extend parsing to full Stan 2 language (2 months)
    1. type-checking (2 months)
    1. "backend" - Interpreter + FFI to Math lib or emit C++ (2 months)
1. In parallel,
    1. Change model, algorithms, and cmdstan to support ahead-of-time compilation (1.5 months?)
    1. Refactor CmdStan to CmdStan 3 to serve as new basis for interfaces (3 months)
        1. Logging-style I/O refactor? (2 months)
    1. Figure out builds (2 weeks)
    1. Create `install_stan()` CRAN script (2 weeks)

# Architectural goals for the new compiler and/or interpreter
* **Multiple phases**, each with human-readable intermediate representations for easy debugging and optimization design.
* First, an **interpreter** - improves compile times and distribution. Later, JIT with LLVM.
* **Optimizing** - takes advantage of info known at the Stan language level.
* **Retain info at runtime** - e.g. `isFinite(X)` can be checked once, matrix sizes (useful for JIT). And line numbers!
* [Mono I/O layer](https://discourse.mc-stan.org/t/universal-static-logger-style-output/4851) ([design doc](https://docs.google.com/document/d/1wgmYDI2RW91S4Amh0w0O2PL9BCsdCKFqBLev65Zr5Lg/edit))
* Eventually, have a server mode that serves `log_prob` and `grad_log_prob`

## Distinct Phases
1. Parse Stan language into AST that represents the syntax quite closely
1. Typecheck & add type information
1. De-sugar into [Middle Intermediate Representation](https://blog.rust-lang.org/2016/04/19/MIR.html)
1. Analyze & optimize MIR -> MIR (will be many passes)
1. Interpret MIR, emit C++, emit LLVM IR, or emit Tensorflow

## Potential Optimizations
* Data and parameters are never modified
* Conditionally independent code-motion
* `target+=` is commutative
* Pattern rewrites; `exp(x) - 1` -> `exp1m(x)`

# Roadmap
## Part 1 - all high level bullets can be in parallel
1. Get a simple language working end to end in OCaml. This means all 4 phases (though all can be simple). Each of these phases should be able to be worked on in parallel as well.
    1. Parse a simple arithmetic expression language
    1. TODO: Typechecking phase
    1. TODO: Optimization phase
    1. Interpret
    1. TODO: FFI with simple AOT compiled Math library
    1. TODO: Compile and link against HMC algorithm
    1. TODO(optional): Emit C++ code
1. Fix the model concept such that there are non-templated versions available.
1. Compile the math library ahead of time for just 1 container type per argument
1. Write code to generate Ocaml FFI wrappers
1. Write code to generate C++ `extern "C"` wrappers

## Part 2
1. Get Stan 2 parsing and input AST defined.
1. Expand FFI coverage to all of Math library for a single container type
1. Figure out IR - maps? new data types?
1. Pretty print AST sexp
1. More optimizations ;)

# Interpreter design
1. Lex and parse Stan 2+3 into a typed AST that retains type and line number information
1. Typecheck AST: Returns "OK" or some error with line numbers.
1. Optimize MIR: Returns a new MIR graph and populates a set of data structures from AST node to arbitrary metadata to capture information from flow analysis, etc.
1. Interpret! This stage must have FFI externs to C from both the OCaml side and the C++ side (to wrap C++ functions in C wrappers we can call from OCaml).

## AST and IRs
* The AST should have different variant types for each different type of syntax, and thus follow closely. Think about how a pretty-printer would want to deal with an AST (thanks @jimtla!)
* The AST should keep track of debug information (line number, etc) in each node itself, rather than in some external data structures keyed off nodes.
This is so that when we run an optimization pass, we will be forced to design how our AST operations affect line numbers as well as the semantics, and at the end of the day we can always point a user to a specific place in their Stan code.
* We should also keep track of the string representation of numeric literals so we can make sure not to convert accidentally and lose precision.
* It would be nice to have different types for side-effect free code. We might need to analyze for print statements, or possibly ignore them as they are moved around.
* We would prefer to keep track of flow dependencies via MIR CFG pointers to other MIR nodes or symbols rather than via SSA or other renaming schemes.

# Stan 3 language goals
* Make it easier for users to share code (modularity and encapsulation are important here)
* Make it easier for users to compose models together
* Force users to learn as little as possible to get numerical stability and performance (looking at you, `transformed data`)
* Capture arbitrary metadata about AST nodes or variables:
    - @silent do not save these values
    - @prior tag on AST for automatic SBC, PPC
    - @opencl on matrix types to send to GPU
    - @hierarchical_params for GMO et al
    - ??? @broadcast
    - ??? @genquant
    - ??? constraints (`lower=0`, `corr_matrix`) ???
* User-defined derivatives
* tuples or structs
* missing data
* automated vectorization
* `extern` for FFI w/ gradients
