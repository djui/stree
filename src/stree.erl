%%!/usr/bin/env escript
%% -*- erlang -*-
%%! -noshell -noinput
%%% @doc Draws the supervision tree of a given supervisor.
%%% @usage "erl -pa ebin -s ptree main SUP_PID -s init stop"
%%% @author Uwe Dauernheim <uwe@dauernheim.net>
-module(stree).

-author("Uwe Dauernheim <uwe@dauernheim.net>").

-export([main/1]).

main([S])               -> main(S);
main(S) when is_atom(S) -> main(whereis(S));
main(S) when is_pid(S)  -> ok = precheck(S), build_tree(S).

precheck(undefined) -> throw(not_found);
precheck(P)         ->
  {dictionary, D} = process_info(P, dictionary),
  precheck2(proplists:get_value('$initial_call', D), P).

precheck2({supervisor, kernel, 1}, _) -> ok;
precheck2(_,                       _) -> throw(no_supervisor).

build_tree(R) ->
  T = traverse({R, regname(R), supervisor}),
  asciify_tree(T).

traverse({N = undefined, Id, T})  -> {{N, Id, T}, []};
traverse({N, Id, T = worker})     -> {{N, Id, T}, []};
traverse({N, Id, T = supervisor}) ->
  Cs = supervisor:which_children(N),
  {{N, Id, T}, [traverse({CN, CId, CT}) || {CId, CN, CT, _CMs} <- Cs]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ASCII Drawing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

asciify_tree(T) -> asciify_tree([T], 0).

asciify_tree([],   _) -> ok;
asciify_tree([{{P, N, T}, ST}], 0) ->
  io:format("~s:~s~n", [shorttype(T), nicename(P, N)]),
  asciify_tree(ST, 1);
asciify_tree([{{P, N, T}, ST}], L) ->
  io:format("~s`-~s:~s~n", [indent(L*2), shorttype(T), nicename(P, N)]),
  asciify_tree(ST, L+1);
asciify_tree([{{P, N, T}, ST}|Rest], L) ->
  io:format("~s+-~s:~s~n", [indent(L*2), shorttype(T), nicename(P, N)]),
  asciify_tree(ST, L+1),
  asciify_tree(Rest, L).

indent(N) -> string:right("", N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

regname(R) ->
  try {registered_name, N} = process_info(R, registered_name), N
  catch _:_ -> undefined
  end.

nicename(P, undefined) -> io_lib:format("~p",      [P]);
nicename(P, N)         -> io_lib:format("~p (~p)", [N, P]).

shorttype(supervisor) -> "s";
shorttype(worker)     -> "w".
