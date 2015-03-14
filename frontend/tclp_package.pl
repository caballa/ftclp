% Author: Jorge. A Navas, The University of Melbourne 2012

:- load_compilation_module(ftclp(frontend(tclp_tr))).
:- add_sentence_trans(tclp_expansion/2).

:- multifile rule/3.
:- multifile constraints/2.
:- multifile builtin/3.

:- multifile tabled/1.
:- multifile no_cache/1.
:- multifile discriminants/1.
%  Deprecated 
:- multifile mode/1.


