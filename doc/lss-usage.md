% LSS(1) LSS User Guide
% Galois, Inc
% March 18, 2013

NAME
====

lss - LLVM symbolic simulator

SYNOPSIS
========

lss [*OPTIONS*] [*BITCODE FILE*]

DESCRIPTION
===========

The LLVM Symbolic Simulator, lss, interprets a fully-linked LLVM
bitcode file, but treats certain program variables as arbitrary,
symbolic expressions rather than concrete values. Therefore, the
resulting values of certain program variables may be described as
symbolic expressions in terms of the initial values of symbolic input
values.

The symbolic expressions representing the values of particular program
variables can be stored in the form of And-Inverter Graphs (AIGs) for
further processing by external tools.

Example usage: 

    lss --memtype=dagbased --dbug=2 trivial.bc

will simulate the function `main()` from the LLVM bitcode file
`trivial.bc` in the current directory, using the DAG-based memory
model, and running at a debug level of 2.

OPTIONS
=======

-a *ARGS*, \--argv=*ARGS*
:   Specify the space-delimited list of arguments to pass as `argv`
    to the `main()` function.

-d *INT*, \--dbug[=*INT*]
:   Sets the debug level for the current run:

      1.  Little to no output; the lowest verbosity level.
      2.  Instruction trace only.
      3.  Warnings on symbolic validity results from mem model; Displays
          error paths as they are encountered.  Mem model info for error
          paths.
      4.  Path constraints on nontrivial path merges.
      5.  Simulator internal state (control stack dump per instruction).
      6.  Memory model dump on load/store operations only; complete path
          dumps on nontrivial path merges. Potentially produces a
          /significant/ amount of output.
      7.  Memory model dump pre/post every operation.  Potentially
          produces a /significant/ amount of output.

-e, \--errpaths
:   Indicates that, upon program completion, details should be
    displayed about any paths on which errors occurred.  Potentially
    verbose.

-b *BACKEND*, --backend=*BACKEND*
:   where *BACKEND* is one of `bitblast`, `dag` or `saw`.  Selects the
    memory model to use for the current run.  Each BACKEND has a
    distinct feature profile and represents various implementation
    tradeoffs (e.g., `bitblast` is in general more efficient, but
    symbolic lengths are only supported by `dag`.)

-x, \--xlate
:   Displays to stdout the LLVM-Sym translation of the input LLVM
    bitcode, and then terminates.  Viewing this representation is
    useful when interpreting path location information provided in
    various debug modes; also when reasoning about the behavior of
    programs operating on symbolic inputs.

--startdebugger
:   Enter the LLVM debugger immediately upon entering 'main()'

--satbranches
:   Check the satisfiability of symbolic path assertions at branches.
    This can sometimes prune infeasible execution paths in the
    symbolic simulator.  Only supported by some backends.

    Note: sometimes it's essential to prune these infeasible
    paths. E.g., when reading or writing an array in a symbolically
    bounded loop, symbolic simulation will fail with an out-of-bounds
    read error without this option.

-V, \--version
:   Print out the version of the simulator.

-?, \--help
:   Print a help message.

SEE ALSO
========

`cryptol` (1).

`jss` (1).

`sawscript` (1).
