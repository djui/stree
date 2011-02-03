# STree / PTree

STree and PTree are two modules that print the supervision tree given
a supervisor in ASCII. This is similar to the `appmon` application,
although this one needs an app as root (and is a GUI).

## STree

Prints the supervision tree of a given supervisor.

### Usage

    erl -pa ebin -s stree main SUP_PID -s init stop

### Example

    erl -pa ebin -s stree main kernel_sup -s init stop

## PTree

Prints the process link tree of a given supervisor (with link depth equal 3).

### Usage

    erl -pa ebin -s ptree main SUP_PID -s init stop

### Example

    erl -pa ebin -s ptree main kernel_sup -s init stop
    
