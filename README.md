[![Build Status](https://travis-ci.org/jfmc/ftclp.svg?branch=master)](https://travis-ci.org/jfmc/ftclp)

# FTCLP

Interpreter for Constraint Logic Programming (CLP), called FTCLP 
(Failure-based Tabled Constraint Logic Programs), that improves standard CLP evaluation 
mechanism by getting rid of infinite loops and redundant sub-computations.

FTCLP is a solver for recursive CLP programs (aka Constrained Horn Clauses).

It is implemented using the Ciao Prolog system and the SMT solver MathSAT 5 for 
checking satisfiability and generation of interpolants.

## Background

### Logic Programming based on Prolog's SLD evaluation mechanism

SLD resolution is the common evaluation strategy in Prolog programs. 
However, it may not be complete or efficient in presence of recursion. 
For instance, for programs that implement the transitive closure of a graph SLD 
may not terminate if the graph contain cycles. Another classical example is Fibonacci
where the evaluation of the sub-goals with n-1 and n-2 (where n is the input) generates
an exponential number of subgoals.

### Tabled (Constraint) Logic Programming
To tackle these issues, an alternative evaluation strategy calling Tabling was proposed
by Tamaki, Sato, and D.S. Warren between 1986-1992. The main idea is to memoize the 
answers of some subgoals and use these answers to reuse subsequent calls to these subgoals. 
Tabling is currently available in most of the Prolog systems and has been successfully used 
in many different applications.

Tabled CLP is a natural extension of Tabling to Constraint Logic Programming. It was 
proposed by P.Codognet in 1995. Although the main idea is the same, there are important differences 
due to the domain of use. Tabled CLP makes explicit the requirement of the tabling execution on the 
constraint domain. For instance, to detect when a more particular call can consume answers from a more 
general one, it uses constraint entailment. And for determining the calling constraint for a tabled call,
it needs to make use of constraint projection (i.e. existential quantifier elimination). The projection
operation is a particularly onerous requirement. Many constraint domains have no projection operation
(or weak projection only), and for those that do, the cost is often prohibitive.

## Our Contribution

FTCLP is a new concept of Tabled CLP for solving Constrained Horn clauses. Similarly to Tabled CLP, 
FTCLP records certain derivations in order to prune further derivations. However, FTCLP only 
learns from failed derivations. This allows it to compute Craig interpolants rather than constraint 
projection for generation of reusing conditions. As a result, the technique can be used where projection
is too expensive or does not exist. Moreover, it can get termination in cases where the use of projection
cannot. FTCLP is based on this [paper] (http://www.clip.dia.fi.upm.es/~jorge/docs/ftclp.pdf)

# Installation

This guide is only for Linux OS and it has only been tested with Ubuntu 12.04/13.10 on a 64-bit machine.

## Requirements

If your OS is linux 64 bits, then you need to install the 32-bit libraries:

For Ubuntu:
 ```sudo apt-get install ia32-libs libc6-i386 libc6-dev-i386 lib32gcc1 lib32stdc++6 g++-4.6-multilib lib32bz2-dev lib32z1-dev ```

Also, you might need to add manually the symbolic link:

 ```ln -s /usr/lib32/libstdc++.so.6 /usr/lib32/libstdc++.so ```

If you run the script ```install_gmp``` then you will need to install ```m4```. For Ubuntu:
```sudo apt-get install m4```

## Setup

 - ```export FTCLP_INSTALL=/home/jorge/ftclp ```
 - ```export PATH=${PATH}:${FTCLP_INSTALL}/bin ``` (optional)
 - ```make install```  to install any required third-party system (e.g., Ciao system).
 - ```make all``` to compile all prolog files and generate executable ```ftclp``` in the ```bin``` directory
 

## Troubleshooting

- If during ```make all```  you get an error like
```"/usr/include/gmp.h:47:22: fatal error: gmp-i386.h: No such file or directory"```
then, type the following commands:

  - ```sudo apt-get install apt-file``` 
  - ```apt-file update```
  - ```apt-file search stubs-32.h```
  - ```lib32gmp-dev: /usr/include/gmp-i386.h```
  - ```apt-get install lib32gmp-dev```

- If the execution of ```ftclp``` fails because it cannot find GMP you
might also try:

```'ls /lib/libgmp* /usr/lib/libgmp* /usr/share/lib/libgmp* /usr/local/lib/libgmp* \
 /usr/local/share/lib/libgmp* /opt/lib/libgmp* /opt/gmp/libgmp* /opt/local/lib/libgmp* \
 /usr/lib/x86_64-linux-gnu/libgmp* /usr/lib/i386-linux-gnu/libgmp* '```

If you get a match, you can use that directory name as a value for
```GMP_LIB``` in ```${FTCLP_INSTALL}/Makefile.conf```


# Usage

```bin/ftclp -help```

```
Failure Tabled Constraint Logic Programming by Interpolation (FTCLP)
  Authors: G. Gange, J.A. Navas, P. Schachte, H. Sondergaard, and P.J. Stuckey.
  (C)2013 The University of Melbourne.
  Description: A solver for recursive Constrained Horn Clauses.

  Usage: cmmd -goal <g> -f  <input_file>.pl [options] 

         g must be a single atom between quotes (e.g., 'foo(X)') 
         For a goal with multiple atoms g1,...,gn create a new clause of the form:
         newg :- g1,...,gn. and call with -goal 'newg'

  Options: 
   -help, --help      : display this list of options
   -debug             : debug mode
   -dot-output        : display derivation tree in dot format
   -show-answers      : show answers
   -dump-interpolants : write interpolants into <input_file>.intp
   -depth     <n>     : explore up to depth n  (default unlimited)
   -solutions <n>     : compute n solutions    (default all solutions)
   -integer-arithmetic: interpret all constraints over integer arithmetic (default reals)
   -clause-pruning    : pruning at the level of clause (default no pruning)
   -pred-pruning      : pruning at the level of predicate (default no pruning)
   -infinite-pruning  : pruning infinite derivations (default no pruning)
        -min-unroll <n>: minimum number of unrollings for recursive clauses 
                          before attempting child-parent subsumption (default 1)
        -max-unroll <n>: maximum number of unrollings for recursive clauses (default unlimited)
   -minimize-intp-calls: optimization to minimize the number of interpolation calls
                         (only if interpolants are inductive)
   -unscoped-intp      : generation of not necessarily well scoped interpolants
   -scoped-intp        : generation of well scoped interpolants
```   

## Common Usage Configurations
- Standard CLP without tabling: run without options
- Failure-Tabling CLP: run with options ```-clause-pruning -unscoped-intp```
- Failure-Tabling CLP with special treatment for infinite derivations: run with options 
```-clause-pruning -scoped-intp -infinite-pruning```

### Scoped vs Unscoped interpolants
An interpolant is well scoped with respect to a head clause H (or a predicate P) if only contains 
variables of H (P). Otherwise, we say it is an unscoped or out-of-scoped interpolant. Ideally, 
we would like to have scoped interpolants because they can prune more derivations but they are 
currently more expensive to compute.

The option ```-scoped-intp``` ensures that only scoped interpolants are computed. 
The other option ```-unscoped-intp``` may produce unscoped interpolants which may reduce 
the pruning capabilities although it's generally faster.

##View Answers (Solutions)
To see the answers (solutions) generated by the CLP program use the option ```-show-answers```. 
Currently, the format of the answers is very limited. Current CLP systems dump the state of the
solver and projected onto a set of variables of interest. Instead, we simply print the sequence 
of executed clauses that led to a solution. A clause is denoted by p/n/k where p is the name 
of the predicate functor, n is the arity of the predicate, and k is the number of the clause.
The number of the clauses are assigned based on the order the clauses appear in the program.

## Program annotations

The CLP programs can be annotated with the following directives:

```:- tabled(foo(_,num)).```

Tells the CLP interpreter that it should do tabling on ```foo/2```. Moreover, 
it provides the interpreter with type information. For instance, in this example 
the 2nd argument is a number and hence, it will model it using the theory of (integer or real)
linear arithmetic. The ```_``` symbol tells the interpreter that the argument should be modelled 
with Herbrand logic and use Prolog unification on it.

Warning: this directive is a must if we want to do tabling on a particular predicate.

```:- discriminants(bar(d,nd)).```

This is optional but it may help significantly tabling. This idea comes from 
Christiansen et.al. (1) that present a technique to identify non-discriminating arguments, 
i.e., arguments that do not have any impact on the control flow of the logic program. 
The symbol 'd' says to our interpreter that the argument is discriminant and 'nd' non-discriminating.
If an argument is marked as 'nd' then we can ignore it during the caching phase which increases the 
likelihood of pruning.

Christiansen et.al. (1) present an automatic program transformation to eliminate from the original
program all the non-discriminating arguments. In the future, we would like to use this transformation.
Meanwhile, user must say so.

```:- no_cache(foo(_,_)).```

It says to the interpreter not to generate interpolants for foo/2. Although it's a bit 
counter-intuitive sometimes we might want to have:

- ```:- tabled(foo(num,num)).```

- ```:- no_cache(foo(_,_)).```

These directives together will tell our solver that the two arguments of foo should be 
modelled using the integer/real arithmetic but we should not bother generating interpolants
and caching them. Instead, if we would use:

```:- no_cache(foo(_,_)).```

Our interpreter will not generate interpolants or caching for ```foo/2``` neither but the 
difference is that it will apply unification whenever foo's arguments are involved.

Bibliography:

1. Non-discriminating Arguments and Their Uses. H. Christiansen and J. P. Gallagher. ICLP'09.

# Current Limitations

- FTCLP only proves invariant candidates generated by interpolation. Thus, any other technique 
that aims at finding safe invariants (e.g., Abstract Interpretation) can help our tool. 
Specially, ideas from abstract compilation could be very useful.

- FTCLP only generates interpolants from real and linear integer arithmetic theories. 
Consideration of recursive datatypes such as lists is in our TODO list.

- FTCLP does not use any capability from the Tabled CLP system implemented in Ciao.

- FTCLP only solves linear recursive Horn clauses.


# Using FTCLP for proving verification conditions

CLP (aka Constrained Horn Clauses, CHC) provides a suitable formalism for expressing veriﬁcation 
conditions that guarantee the correctness of imperative, functional, or concurrent programs. 
Since FTCLP is a solver for CHCs we can use it in order to prove whether a set of verification 
conditions (VCs) holds.

These VCs can come from safety or termination as well as from different programming languages. 
Once we translated those to Constrained Horn clauses we can use directly FTCLP to prove them. 
This is one of the key advantages of using CLP as intermediate language since we do not need 
to implement different tools for different languages or properties. Moreover, since the use 
of CLP as intermediate representation in verification tools is becoming more popular, it is 
much easier to communicate FTCLP with other verifiers.

The VCs must be encoded in such way that they hold iff its corresponding CLP program has no solutions.

We provide a script called ```horn-prover``` in the ```bin``` directory that specializes FTCLP 
(by choosing some suitable options) for proving verification conditions.


## Usage

```
horn_prover filename.pl -entry E [ <options> ]
       E     is a single atom between single quotes (e.g., 'prove')
options:
       -integer-arithmetic interpret all constraints over integer arithmetic (default real arithmetic)
       -min-unroll <n>     minimum num of unrollings for recursive clauses before attempt child-parent subsumption
       -max-unroll <n>     maximum num of unrollings for recursive clauses (default: no limit)
       -dump-interpolants  write interpolants to filename.intp
       -show-proof         write the proof tree in a .dot file
```

## Examples

Suppose the following C program fragment:

```
 x=i; y=j;
 while (x!=0){
   x--;
   y--;
 }
 if (i==j)
    assert(y<=0);
```

Next, we translate the above program into a set of CHCs using an encoding based on a small-step semantics form:

```state(X1,...,Xn) :- C(X1,...,Xn,X1',...,Xn'), next_state(X1',...,Xn').```

```
prove :- 
        clp_meta([ X .=. I, Y .=.J ]), l(X,Y,I,J).

:- tabled(l(num,num,num,num)).
l(X,Y,I,J):- 
        clp_meta([ X .<>.0,  
                   X1 .=. X-1, Y1 .=. Y-1]), 
        l(X1,Y1,I,J).
l(X,Y,I,J):- 
        clp_meta([X .=. 0]), 
        err(X,Y,I,J).

:- tabled(err(num,num,num,num)).
err(_X,Y,I,J):- clp_meta([I .=. J, Y .>. 0]).
```

First note that we have encoded the loop using the relation ```l/4```. 
The first rule of ```l/4``` is recursive and corresponds to the body of the loop. 
The second rule of ```l/4``` corresponds to the exit of the loop. 
Note that the ```err/4``` relation encodes the safety property and it is the negation of 
the condition that appears in the C ```assert```. 
The initial state has been encoded within the relation ```prove/0``` before we call ```l/4```. 
It should not been hard to see that the original C fragment is safe iff the relation ```prove/0``` 
has no solutions.

Alternatively, we can encode the verification conditions originated form the above program 
using Hoare-logic based encoding:

```
prove :- 
        inv(X,Y,I,J),
        clp_meta([X .=. 0]), 
        clp_meta([I .=. J]),
        err(X,Y,I,J).

:- tabled(inv(num,num,num,num)).
% base case
inv(X,Y,I,J):- 
        clp_meta([ X .=. I, Y .=.J ]).
% inductive case
inv(X1,Y1,I,J):- 
        clp_meta([ X0 .<>.0,  
                   X1 .=. X0-1, Y1 .=. Y0-1]),
        inv(X0,Y0,I,J).
          
:- tabled(err(num,num,num,num)).
err(_X,Y,I,J):- clp_meta([Y .>. 0]).
```

FTCLP reports no solutions with either the above small-step or Hoare-logic encodings 
which means that the original C program is safe.

## Grammar for Constraints

The relation clp_meta/1 takes as its only argument a list (in Prolog format) of type Constraint. 
The grammar for Constraint is defined as follows:

```
     Constraint  := <Expr> <RelOp>   <Expr> 
     Expr        := <Expr> <ArithOp> <Expr> | <Var> | <Number> 
     RelOp       := .<. | .>. | .=. | .<>. | .=<.| .>=.   
     ArithOp     := + | - | * | /  
```

This is the standard format used in Constraint Logic Programming systems.

## Program annotations
All relations should be annotated via tabled/1 directives:

```:- tabled(foo(_,num)).```

Tells FTCLP that it should do tabling on ```foo/2```. Moreover, it tells FTCLP that the second argument 
is a number and hence, it will model it using the theory of integer or real linear arithmetic, depending 
on one of the horn-prover script options. The ```_``` symbol tells us that the argument should be 
modelled with Herbrand logic and use Prolog unification on it.

## Translation to SMTLIB2 format

Our CLP notation can be translated in a straightforward manner to SMTLIB2 format. 
E.g., the above program can be written in SMTLIB2 as follows:

```
(declare-rel prove ())
(declare-rel l (Int Int Int Int))
(declare-rel err (Int Int Int Int))
(declare-var x Int)
(declare-var y Int)
(declare-var i Int)
(declare-var j Int)
(declare-var x1 Int)
(declare-var y1 Int)
(rule (=> (and (not (= x 0)) (= x1 (- x 1)) (= y1 (- y 1)) (l x1 y1 i j)) (l x y i j)))
(rule (=> (and (= x 0) (err x y i j )) (l x y i j)))
(rule (=> (and (= i j) (> y 0)) (err x y i j)))
(rule (=> (and (= x i) (= y j) (l x y i j)) prove))
```

Just to be clear, a parser from SMTLIB2 is not currently implemented.

## View Counterexamples
Since VCs hold iff its corresponding CLP program has no solutions, if a solution is found then
it means that the program is unsafe and the solution corresponds to a counterexample. We print 
the sequence of executed clauses (separated by the dash - symbol) that led to a solution. 
A clause is denoted by p/n/k where p is the name of the relation functor, n is the arity of 
the relation, and k is the number of the clause. The number of the clauses are assigned based
on the order in which the clauses appear in the program. This is an example of a counterexample:

```Solution: prove/0/1-p/6/2-l/4/1-l/4/2-err/1/1```

