
An Erlang module that can flatten nested propslists into a list of dotted values e.g

```
[{foo,"bar"},{foo2,{baz,"bof"}}] -> ["foo=bar","foo2.baz=bof"]
```
