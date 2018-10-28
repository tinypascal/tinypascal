# tinypascal
All of this code (including bugs) is public domain.  
  
This is an advanced interpreter study based on the amazing series https://zserge.com/blog/cucu-part1.html  
  
I suggest looking at the tinymath project first to get a feel for the code. This interpreter is quite complex compared to the math one.  

The goal was to follow the tutorial but end up with an interpreter instead of a compiler. This repository is the result of my studies consisting of the following parts:
- lexer
- parser
- syntax tree builder
- interpreter (abstract syntax tree)

Its primary purpose is to provide a learning platform to build on, contains debugging information for each stage. The interpreter is fully functional but only supports a subset of the pascal language, please see file test.tp for details. 
Sample debugging output for the input 'a := 5; if a > 3 then writeln(2*a);':
  
```
--- Start        17:02:14
Lexer start
*** Lexer ***
1 Identifier: a
1 Operator: :=
1 Number: 5
1 Single: ;
1 Identifier: if
1 Identifier: a
1 Operator: >
1 Number: 3
1 Identifier: then
1 Identifier: writeln
1 Single: (
1 Number: 2
1 Operator: *
1 Identifier: a
1 Single: )
1 Single: ;
Lexer done       17:02:14 (0s)
Parser start

*** Parser ***

# Program accepted
#begin_marker var(a) number(5) := var(a) number(3) > string(writeln) number(2) v
ar(a) * func-arg func-call NOP if-then begin-end
Parser done      17:02:14 (0s)
Syntax Tree start

*** Syntax Tree ***
# Workstack:

Current Operation: #begin_marker
Current Workstack:

Current Operation: var a
Current Workstack: #begin_marker
Current Workstack:

Current Operation: var a
Current Workstack: #begin_marker

Current Operation: number 5
Current Workstack: #begin_marker var

Current Operation: :=
Current Workstack: #begin_marker var number

Current Operation: var a
Current Workstack: #begin_marker :={var; number}

Current Operation: number 3
Current Workstack: #begin_marker :={var; number} var

Current Operation: >
Current Workstack: #begin_marker :={var; number} var number

Current Operation: string writeln
Current Workstack: #begin_marker :={var; number} >{var; number}

Current Operation: number 2
Current Workstack: #begin_marker :={var; number} >{var; number} string

Current Operation: var a
Current Workstack: #begin_marker :={var; number} >{var; number} string number

Current Operation: *
Current Workstack: #begin_marker :={var; number} >{var; number} string number var

Current Operation: func-arg
Current Workstack: #begin_marker :={var; number} >{var; number} string *{number; var}

Current Operation: func-call
Current Workstack: #begin_marker :={var; number} >{var; number} string func-arg{*}

Current Operation: NOP
Current Workstack: #begin_marker :={var; number} >{var; number} func-call{string; func-arg}

Current Operation: if-then
Current Workstack: #begin_marker :={var; number} >{var; number} func-call{string; func-arg} NOP

Current Operation: begin-end
Current Workstack: #begin_marker :={var; number} if-then{>; func-call; NOP}

# Resulting Tree:
+ begin-end
  + :=
    | var a (index 0)
    | number 5
  + if-then
    + >
      | var a (index 0)
      | number 3
    + func-call(sys-call) writeln (id 4)
      | string
      + func-arg
        + *
          | number 2
          | var a (index 0)
    | NOP

# Resulting Symbol Table:
Var_0 = a

Syntax Tree done 17:02:14 (0s)
Interpreter start

*** Interpreter ***
10

# Variable State Dump
Var_0 value: 5

# Execution halted.
Interpreter done 17:02:14 (0s)
--- Ready        17:02:14 (0s)
```
