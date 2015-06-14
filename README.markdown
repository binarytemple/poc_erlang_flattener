
An Erlang module that can flatten nested [proplists](http://www.erlang.org/doc/man/proplists.html) into a list of dotted values e.g

    Input
    [{foo1,bar1},
     {foo2,10},
     {foo3,"blah"},
     {foo4,{baz,"bar"}},
     {foo4,[{foo5,"bar"}]},
     {foo5,[{foo6,"bar"},{foo7,"barbara"}]}]
    Transformed
    [["foo1=bar1"],
     [{[],"foo2=10"}],
     "foo3=blah",
     ["foo4.baz=bar"],
     ["foo4.foo5=bar"],
     ["foo5.foo6=bar","foo5.foo7=barbara"]]

TODO:

* Consistent handling
* Handle binaries
* Fix all the bugs

