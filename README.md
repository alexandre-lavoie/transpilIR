# transpilIR

A transpiler for compiler IRs. The goal of the tool is to let you use fast/simple IRs (ex: [QBE](https://c9x.me/compile/)) while easily being able to convert to slow/performant IRs (ex: [LLVM](https://llvm.org/)).

> [!IMPORTANT]
> This project is in **very early alpha**. This means that the project is useable but will likely break. **Do not** use this for anything serious yet.

## 🧩 Compability

### 🔍 Overview

| From  \ To | QBE    | C | LLVM |
|-----------:|:------:|:-:|:----:|
| QBE        | 🚧     |🚧 | ⏳   |

- 🚧 : In Progress
- ⏳ : Planned

### 📦 QBE

[QBE](https://c9x.me/compile/) is a lightweight compiler backend that "aims to provide 70% of the performance of industrial optimizing compilers in 10% of the code". It supports the following 64-bit targets: amd64 (linux and osx), arm64, and riscv64.

Currently, QBE to C is passing most unit tests for linux amd64. It should be stable enough to test it out.

- [QBE](https://c9x.me/compile/): 54 passed, 2 failed, 56 total (phi ordering and argument match count)
- [Hare](https://harelang.org/): 495 passed, 71 failed, 8 skipped, 574 total (mostly floating point math)

## 🚩 Getting Started

### 📘 Requirements

- [Zig](https://ziglang.org/)

### 🏗 Build

```
zig build
```

### 🖥 CLI

Copy the binary from `./zig-out/bin/transpilir` to your prefered location. See the CLI arguments with `transpilir -h`.

```
transpilir [OPTIONS] {file,-}
        -b        disables colors
        -h        prints this help message
        -o file   path to output file
        -s src    source IR
                  qbe (default)
        -r ir     target IR
                  c (default), qbe
        -c cmp    IR compiler
                  auto (default), gcc, llvm, qbe, irc
        -t trgt   output target
                  native (default), ir, wasm32, amd64, amd64_sysv, x86_64
        -z opt    optimization level of compiler
                  o0 (default), o1, o2, o3
        -d flgs   comma list of debug flags to activate
                  source, lexer, parser, memory, typing, optimization, ir, target
```

The CLI tries to match QBE's. The binary should almost be a drop in replacement.

Various paths can be specified through evironments variables:

```
GCC, LLVM, QBE
```

## 🗺 Usage

### 🐇 Hare

Transpilir can be used as a drop in replacement for QBE. Simply create a script that wraps the `transpilir` binary with desired configuration (here is a working bash script):

```bash
#!/bin/bash

/absolute/path/to/transpilir -b -c gcc -z o0 $@
```

Currently, there seems to be a bug somewhere in the C logic that causes the program to crash. We only managed to get the compiler to fully compile with `GCC` and `-O0` (and there still are some bugs). We're pretty sure after fixes optimizations would work with some minor tweaking.

## 👥 Contributing

Currently, the code is abit of a mess. There are many TODOs that should be addressed before making any type of serious release. If you feel compelled to help, just shoot us a PR and we'll have a look!

## 📜 License

All code written by us in this repo is [MIT licensed](https://opensource.org/license/mit). This should be everything under the `src`. We make use of QBE/Hare's unit/integration tests to validate the correctness of the tool. These follow their respective owner's licensing.
