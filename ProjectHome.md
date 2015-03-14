# Description #

This project provides an interpreter for _Constraint Logic Programming_ (_CLP_), called _FTCLP_ (_Failure-based Tabled Constraint Logic Programs_) that can improve standard CLP evaluation mechanism by getting rid of infinite loops and redundant sub-computations.

FTCLP is a _solver_ for recursive CLP programs, also called _Constrained Horn Clauses_.

It is implemented using the [Ciao Prolog](http://ciaohome.org) system and the SMT solver [MathSAT 5](http://mathsat.fbk.eu) for checking satisfiability and generation of interpolants.

# Logic Programming based on Prolog's SLD evaluation mechanism #

[SLD](http://en.wikipedia.org/wiki/SLD_resolution) resolution is the common evaluation strategy in Prolog programs. However, it may not be complete or efficient in presence of recursion. For instance, for programs that implement the transitive closure of a graph SLD may not terminate if the graph contain cycles. Another classical example is Fibonacci where the evaluation of the sub-goals with n-1 and n-2 (where n is the input) generates an exponential number of subgoals.

# Tabled (Constraint) Logic Programming #

To tackle these issues, an alternative evaluation strategy calling _Tabling_ was proposed by Tamaki, Sato, and D.S. Warren between 1986-1992. The main idea is to _memoize_ the answers of some subgoals and use these answers to reuse subsequent calls to these subgoals. Tabling is currently available in most of the Prolog systems and has been successfully used in many different applications.

_Tabled CLP_ is a natural extension of Tabling to Constraint Logic Programming. It was proposed by P.Codognet in 1995. Although the main idea is the same, there are important differences due to the domain of use. Tabled CLP makes explicit the requirement of the tabling execution on the constraint domain.  For instance, to detect when a
more particular call can consume answers from a more general one, it
uses _constraint entailment_.  And for determining the calling
constraint for a tabled call, it needs to make use of _constraint projection_ (i.e. existential quantifier elimination). The projection operation is a particularly onerous
requirement. Many constraint domains have no projection operation (or
weak projection only), and for those that do, the cost is often prohibitive.

# Our Contribution: Failure-Based Tabled Constraint Logic Programming (FTCLP) #

FTCLP is a new concept of Tabled CLP for solving Constrained Horn clauses. Similarly to Tabled CLP, FTCLP  records certain derivations in order to prune further derivations. However, FTCLP only learns from failed derivations. This allows it to compute [Craig interpolants](http://en.wikipedia.org/wiki/Craig_interpolation) rather than constraint projection for generation of reusing conditions. As a result, the technique can be used where projection is too expensive or does not exist. Moreover, it can get termination in cases where the use of projection cannot.

# Get the code, build it, and play with it #

Click [here](INSTALL.md) to read the installation and usage guide. It is also very important to know how to annotate the CLP programs since the current implementation lacks of some program analyses like types, modes, etc. Please read [this document](Annotations.md).

# Proving Verification Conditions with FTCLP #

One of its main applications is solving the verification conditions originated from programs. Click [here](ProvingVerificationConditions.md) for more details.

# References #
For all details, please read this paper recently published in [ICLP'13](http://www.iclp2013.org/en/):

  * [Failure Tabled Constraint Logic Programming by Interpolation](http://www.clip.dia.fi.upm.es/~jorge/docs/ftclp.pdf).

  * [Online appendix](http://www.clip.dia.fi.upm.es/~jorge/docs/ftclp_appendix.pdf).