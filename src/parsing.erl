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

stringify_array( L= [_|_]) ->

  string:join(L,  ",")

  .


flatton({X, Y}) -> flatton({X, Y}, [], []);
flatton(X) -> flatton(X, [], []).

flatton([{Prop, Val} | Y], Depth, Acc) ->
  case {is_atom(Prop), is_string(Prop), is_atom(Val), is_integer(Val)} of
    {true, _, true, _} -> flatton(Y, Depth, Acc ++ [atom_to_list(Prop) ++ "=" ++ atom_to_list(Val)]);
    {true, _, _, true} -> flatton(Y, Depth, Acc ++ [atom_to_list(Prop) ++ "=" ++ integer_to_list(Val)]);
    {true, _, _, _} ->
      case is_string(Val) of
        true -> flatton(Y, Depth, Acc ++ atom_to_list(Prop) ++ "=" ++ Val);
      %% the case of other structure..
        _ -> X = flatton(Val),
          flatton(Y, Depth, Acc ++ [X])
      end;
    {false, true, _, _} -> flatton(Y, Depth, Acc ++ [atom_to_list(Prop) ++ "=" ++ atom_to_list(Val)]);
    _ -> flatton(Y, [] ++ Prop, Acc ++ [io_lib:format("~p=~p", [Prop, Val])])
  end
;

flatton({Prop, Val}, Depth, []) when is_atom(Prop) and is_atom(Val) ->

  prepend_prefix(Depth,

    atom_to_list(Prop) ++ "=" ++ atom_to_list(Val))

;

flatton({Prop, Val}, Depth, []) ->
  case is_string(Val) of
    true -> prepend_prefix(Depth, atom_to_list(Prop) ++ "=" ++ Val);
    _ -> case is_integer(Val) of
           true -> atom_to_list(Prop) ++ "=" ++ integer_to_list(Val);
           _ ->
             case is_tuple(Val) of
               true -> L = tuple_to_list(Val),
                 AtomHead = is_atom(hd(L)),
                 Len = length(L),
                 [H | T] = L,
                 StringTail = is_string(T),
                 ListTail = is_list(T),
                 AtomTail = is_atom(T),
                 case AtomHead of
                   true when Len == 2 ->
                     case {StringTail, ListTail, AtomTail} of
                       {true, false, false} ->
                         flatton([], Depth ++ [Prop], atom_to_list(H) ++ "=" ++ Val);
                       {false, false, true} ->
                         flatton([], Depth ++ [Prop], atom_to_list(H) ++ "=" ++ atom_to_list(Val));
                       {false, true, false} ->
                         case all_strings(T) of
                           true -> flatton([], Depth ++ [Prop], atom_to_list(H) ++ "=" ++ stringify_array(T));
                           _ -> "can't cope"
                         end;
                       _ -> "freakout!"
                     end;
                   _ -> "cant handle"
                 end;
               _ -> case is_list(Val) of
                      _ -> flatton(Val, Depth ++ [Prop], [])
                    end
             end
         end
  end
;

flatton([], Depth, X) ->
  prepend_prefix(Depth, X)
.

prepend_prefix(Depth, Path) ->

  Prefix = case length(Depth) > 0 of
    true ->
  string:join(
    [atom_to_list(X) || X <- Depth]
    , ",") ++ ".";
      false -> []
end,

  Prefix ++ Path
;

prepend_prefix([], Path) ->
  Path
.


init() ->
  Test = [
%%     {foo1, bar1},
%%     {foo2, 10},
%%     {foo3, "blah"},
%%     {foo4, {baz, "bar"}},
%%      {foo4, [{foo5, "bar"}]},
    {foo4, [
      {foo5, "bar"},
      {foo6, "barbara"}
    ]}

  ],

%%     ,
%%     {foo6, [{foo7, [{foo8, "bar"}]}]}],
  io:format("~p~n", [Test]),
  io:format("transformed~n", []),
  Flattened = [flatton(X) || X <- Test],
  io:format("~p~n", [Flattened]),
  ok
.

%% extractPropLists([], ResultList) -> ResultList;
%% extractPropLists( [H|T], ResultList ) ->    extractPropLists(T, extractPropLists(H, ResultList));
%% extractPropLists( {K,V}, ResultList ) -> [ {K,V} | extractPropLists(K, extractPropLists(V, ResultList)) ];
%% extractPropLists( T, ResultList ) -> ResultList.