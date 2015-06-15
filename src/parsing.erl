-module(parsing).
-author("bryanhunt").

-export([init/0, is_string/1, flatton/1]).

is_string(XY) ->
  InRange = fun(XX) ->
    if XX < 0 -> false;
      XX > 255 -> false;
      true -> true
    end
  end,
  case is_list(XY) of
    false -> false;
    true -> lists:all(InRange, XY)
  end
.

all_strings(List) ->
  lists:all(
    fun(X) -> is_string(X) end,
    List)
.

stringify_array(L = [_ | _]) ->
  string:join(L, ",")
.



prepend_prefix(List = [_ | _]) ->
  [prepend_prefix(X) || X <- List]
;

prepend_prefix({[], Path}) ->
  Path
;

prepend_prefix({Depth, Path = [{[_], [_]} | _]}) ->
  "foo"
;

prepend_prefix({Depth, Path}) ->
  Prefix = case length(Depth) > 0 of
             true ->
               string:join(
                 [atom_to_list(X) || X <- Depth]
                 , ".") ++ ".";
             false -> []
           end,
  Prefix ++ Path
.



flatton({X, Y}) -> flatton({X, Y}, [], []);
flatton(X) -> flatton(X, [], []).

flatton([{Prop, Val} | Y], Depth, Acc) when is_binary(Prop) ->
  flatton(Y, Depth, Acc ++ [{Depth, atom_to_list(Prop) ++ "=" ++ binary_to_list(Val)}])
;

flatton([{Prop, Val} | Y], Depth, Acc) ->
  case {is_atom(Prop), is_string(Prop), is_atom(Val), is_integer(Val)} of
    {true, _, true, _} -> flatton(Y, Depth, Acc ++ [{Depth, atom_to_list(Prop) ++ "=" ++ atom_to_list(Val)}]);
    {true, _, _, true} -> flatton(Y, Depth, Acc ++ [{Depth, atom_to_list(Prop) ++ "=" ++ integer_to_list(Val)}]);
    {true, _, _, _} ->
      case is_string(Val) of
        true -> flatton(Y, Depth, Acc ++ [{Depth, atom_to_list(Prop) ++ "=" ++ Val}]);
      %% the case of other structure..
        _ -> X = flatton(Val),
          NewDepth = Depth ++ [Prop],
          flatton(Y, NewDepth, Acc ++ [{NewDepth,X}])
      end;
    {false, true, _, _} -> flatton(Y, Depth, Acc ++ [{Depth, [atom_to_list(Prop) ++ "=" ++ atom_to_list(Val)]}]);
    _ -> flatton(Y, [] ++ Prop, Acc ++ [io_lib:format("~p=~p", [Prop, Val])])
  end
;

flatton({Prop, Val}, Depth, []) when is_atom(Prop) and is_atom(Val) ->
  prepend_prefix({Depth,
      atom_to_list(Prop) ++ "=" ++ atom_to_list(Val)})
;

flatton({Prop, Val}, Depth, []) when is_binary(Val)->
      flatton([], Depth, atom_to_list(Prop) ++ "=" ++ binary_to_list(Val))
;

flatton({Prop, Val}, Depth, []) ->
  case is_string(Val) of
    true ->
%%       prepend_prefix([{Depth, atom_to_list(Prop) ++ "=" ++ Val}]);
      flatton([], Depth, atom_to_list(Prop) ++ "=" ++ Val);

    false -> case is_integer(Val) of
               true -> flatton([], Depth, atom_to_list(Prop) ++ "=" ++ integer_to_list(Val));
               _ ->
                 NewDepth = Depth ++ [Prop],
                 case is_tuple(Val) of
                   true -> L = tuple_to_list(Val),
                     AtomHead = is_atom(hd(L)),
                     Len = length(L),
                     [H | [T | _]] = L,
                     StringTail = is_string(T),
                     ListTail = is_list(T),
                     AtomTail = is_atom(T),
                     case AtomHead of
                       true when Len == 2 ->
                         case {StringTail, ListTail, AtomTail} of
                           {true, _, _} ->
                             prepend_prefix({NewDepth, atom_to_list(H) ++ "=" ++ T});
                           {_, _, true} ->
                             prepend_prefix({NewDepth, atom_to_list(H) ++ "=" ++ atom_to_list(Val)});
                           {_, true, _} ->
                             case all_strings(T) of
                               true ->
%%                                  flatton([], NewDepth, [{NewDepth, atom_to_list(H) ++ "=" ++ stringify_array(T)}]);
                                 prepend_prefix({NewDepth, atom_to_list(H) ++ "=" ++ stringify_array(T)});
                               _ -> "can't cope"
                             end;
                           _ -> "freakout!"
                         end;
                       _ -> "cant handle"
                     end;
                   _ -> case is_list(Val) of
                          _ -> flatton(Val, NewDepth, [])
                        end
                 end
             end
  end
;

flatton([], _, X = [{[_], _} | _]) ->
  prepend_prefix(X)
;



flatton([], Depth, X) ->
  prepend_prefix({Depth, X})
.

init() ->
  Input = [
    {foo1, <<"bar1">>}
    ,
    {foo1, bar1},
    {foo2, 10},
    {foo3, "blah"},
    {foo4, {baz, "bar"}},
    {foo4, [{foo5, "bar"}]},
    {foo5, [
      {foo6, "bar"},
      {foo7, "barbara"}
    ]},
    {foo6, [
      {foo7, "baz"},
      {foo8, {foo9, "barbaraz"}}
    ]},
    {foo7, [
      {foo8, "baz"},
      {foo9, {foo10, <<"barbaraz">>}}
    ]}
  ],

  io:format("Input~n", []),
  io:format("~p~n", [Input]),

  io:format("Transformed~n", []),
  Flattened =
    [flatton(X) || X <- Input]
  ,
  io:format("~p~n", [Flattened]),
  Flattened
.