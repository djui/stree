%%!/usr/bin/env escript
%% -*- erlang -*-
%%! -noshell -noinput
%%% @doc Draws the process tree of a given supervisor.
%%% @usage "erl -pa ebin -s stree main SUP_PID -s init stop"
%%% @author Uwe Dauernheim <uwe@dauernheim.net>
-module(ptree).

-author("Uwe Dauernheim <uwe@dauernheim.net>").

-export([main/1, main/2]).

-define(DEPTH, 3).

main(S) -> main(S, ?DEPTH).

main(undefined, _)             -> throw(no_supervisor);
main([S], Depth)               -> main(S, Depth);
main(S, Depth) when is_atom(S) -> main(whereis(S), Depth);
main(S, Depth) when is_pid(S)  -> ok = precheck(S), build_tree(S, Depth).

precheck(undefined) -> throw(not_found);
precheck(P)         ->
  {dictionary, D} = process_info(P, dictionary),
  precheck2(proplists:get_value('$initial_call', D), P).

precheck2({supervisor, kernel, 1}, _) -> ok;
precheck2(_,                       _) -> throw(no_supervisor).

build_tree(R, Depth) ->
  T = traverse(R, R, Depth),
  asciify_tree(T).

traverse(_, _, 0) -> [];
traverse(N, P, Depth) ->
  {links, Ls} = process_info(N, links),
  {dictionary, D} = process_info(N, dictionary),
  As = proplists:get_value('$ancestors', D, []),
  Cs = (Ls -- [P]) -- As,
  ST = [traverse(C, N, Depth-1) || C <- Cs, is_port(C) =:= false],
  {N, ST}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ASCII Drawing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

asciify_tree(T) -> asciify_tree([T], 0).

asciify_tree([{R, ST}], 0) ->
  io:format("~s~n", [nicename(R, regname(R))]),
  asciify_tree(ST, 1);
asciify_tree([{R, ST}], L) ->
  io:format("~s`-~s~n", [indent(L*2), nicename(R, regname(R))]),
  asciify_tree(ST, L+1);
asciify_tree([{R, ST}|Rest], L) ->
  io:format("~s+-~s~n", [indent(L*2), nicename(R, regname(R))]),
  asciify_tree(ST, L+1),
  asciify_tree(Rest, L);
asciify_tree(_, _) -> ok.

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
