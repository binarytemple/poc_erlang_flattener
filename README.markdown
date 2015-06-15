
An Erlang module that can flatten nested [proplists](http://www.erlang.org/doc/man/proplists.html) into a list of dotted values e.g

    Input
    [{foo1,<<"bar1">>},
     {foo1,bar1},
     {foo2,10},
     {foo3,"blah"},
     {foo4,{baz,"bar"}},
     {foo4,[{foo5,"bar"}]},
     {foo5,[{foo6,"bar"},{foo7,"barbara"}]},
     {foo6,[{foo7,"baz"},{foo8,{foo9,"barbaraz"}}]},
     {foo7,[{foo8,"baz"},{foo9,{foo10,<<"barbaraz">>}}]}]
    Transformed 1 
    ["foo1=bar1","foo1=bar1","foo2=10","foo3=blah","foo4.baz=bar",
     ["foo4.foo5=bar"],
     ["foo5.foo6=bar","foo5.foo7=barbara"],
     ["foo6.foo7=baz","foo6.foo8.foo9=barbaraz"],
     ["foo7.foo8=baz","foo7.foo9.foo10=barbaraz"]]
    Transformed 2 
    ["foo1=bar1","foo1=bar1","foo2=10","foo3=blah","foo4.baz=bar","foo4.foo5=bar",
     "foo5.foo6=bar","foo5.foo7=barbara","foo6.foo7=baz",
     "foo6.foo8.foo9=barbaraz","foo7.foo8=baz","foo7.foo9.foo10=barbaraz"]


TODO:

* Consistent handling
* Fix all the bugs
* De-uglify the code
* Write a unit test

