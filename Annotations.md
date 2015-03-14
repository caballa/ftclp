# Program annotations #

The CLP programs can be annotated with the following directives:

```
:- tabled(foo(_,num)).
```

Tells the CLP interpreter that it should do tabling on foo/2. Moreover, it provides the interpreter with _type_ information. For instance, in this example the 2nd argument is a number and hence, it will model it using the theory of (integer or real) linear arithmetic. The `_` symbol tells the interpreter that the argument should be modelled with Herbrand logic and use Prolog unification on it.

**Warning**: this directive is a must if we want to do tabling on a particular predicate.

```
:- discriminants(bar(d,nd)).
```

This is optional but it may help significantly tabling.  This idea
comes from Christiansen et.al. (1) that present a technique to
identify non-discriminating arguments, i.e., arguments that do not
have any impact on the control flow of the logic program. The symbol
'd' says to our interpreter that the argument is discriminant and 'nd'
non-discriminating. If an argument is marked as 'nd' then we can ignore it during the caching phase which increases the likelihood of pruning.

Christiansen et.al. (1) present an automatic program transformation to eliminate from the
original program all the non-discriminating arguments. In the future,
we would like to use this transformation. Meanwhile, user must say so.

```
:- no_cache(foo(_,_)).
```

It says to the interpreter not to generate interpolants for foo/2. Although it's a bit counter-intuitive sometimes we might want to have:

```
:- tabled(foo(num,num)).
:- no_cache(foo(_,_)).
```

These directives together will tell our solver that the two arguments of foo should be modelled using the integer/real arithmetic but we should not bother generating interpolants and caching them. Instead, if we would use:

```
:- no_cache(foo(_,_)).
```

Our interpreter will not generate interpolants or caching for foo/2 neither but the difference is that it will apply unification whenever foo's arguments are involved.


## Bibliography ##

(1) Non-discriminating Arguments and Their Uses. H. Christiansen and J. P. Gallagher. ICLP'09.