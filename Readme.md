# C Compiler

## Academic project

The purpose of this academic project was to build a C subset compiler for a stack machine to better understand the compilation process. 

I'm building this compiler in Ada. The assembly code generated can be executed by the mini stack machine (msm).

The msm has a simple assembly instruction set because there are few operations and no registers.


## C restriction

The main restriction are
- Only one type is handled ```int```
- No ```void``` and no ```int *```
- No constants and no global variables
- no include
- No bitwize operations
- no case statements

## Features

The main features are
- variables
- conditional statements
- loop statements
- functions *(can be recursive)*
- Heap allocations
- Arrays *(on the heap)*

## Usage
```
usage: main [--help|-h] files [files ...] [--debug|-d] 

reduce C Compiler for msm

positional arguments:
   files                 Files to compile
   
optional arguments:
   --help, -h            Show this help message
   --debug, -d           Print debug infos
```

## Setup

* Clone this projet
```bash
git clone https://github.com/DamDC91/sudoku
```
* Compile it with gprbuild (you will need [GNAT](https://www.adacore.com/download))
```bash
gprbuild -p -P main.gpr
```
* Compile the stack machine
```
cd msm; make
```
* Run the tests
```
./launch_test
```
