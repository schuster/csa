CSA: Communicating State Agents, and APS: Actor Protocol Specifications

This is a package for Racket implementing the CSA state-machine-based actor
language as well as a simple syntax checker for APS, the corresponding
specification language.

Installation
============

Prerequisites: A recent version of Racket (http://racket-lang.org/)

To install:
1. Install the asyncunit package (used for testing) with "raco pkg install
   asyncunit" at the command line.
2. Set working directory to the directory containing this README.
3. Install the CSA package by running "raco pkg install" (without any other
   arguments; this installs the package in the current directory).

File walk-through
=================

The examples directory has the implementations for examples from the paper. Each
program has its implementation, written in "#lang csa", and a small test suite,
written in Racket in a corresponding "-tests.rkt" file. Run each test file with
"racket <program>-tests.rkt", or run all tests at once with "racket
x-all-tests.rkt".

A simple "hello world" program is also included, which can be run as "racket
hello-world.rkt".
