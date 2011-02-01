%%!/usr/bin/env escript
%% -*- erlang -*-
%%! -noshell -noinput
%%% @doc Draws the supervision tree of a given supervisor.
%%% @author Uwe Dauernheim <uwe@dauernheim.net>
-module(stree).

-author("Uwe Dauernheim <uwe@dauernheim.net>").

-export([main/1]).

-define(DEPTH, 3).

main([S]) when is_atom(S) -> main(S);
main(S) when is_atom(S)   -> main(whereis(S));
main(S) when is_pid(S)    -> precheck1(S).

precheck1(undefined) -> exit(not_found);
precheck1(P)         ->
  {dictionary, D} = process_info(P, dictionary),
  precheck2(proplists:get_value('$initial_call', D), P).

precheck2({supervisor, kernel, 1}, P) -> build_tree(P);
precheck2(_,                       _) -> exit(no_supervisor).

build_tree(R) ->
  T = traverse(R, R, ?DEPTH),
  dot(T).

traverse(_, _, 0) -> [];
traverse(N, P, Depth) ->
  {links, Ls} = process_info(N, links),
  {dictionary, D} = process_info(N, dictionary),
  As = proplists:get_value('$ancestors', D, []),
  Cs = (Ls -- [P]) -- As,
  ST = [traverse(C, N, Depth-1) || C <- Cs, is_port(C) =:= false],
  {N, ST}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Drawing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dot(T = {R, _}) ->
  io:format("graph \"~s\" {~n", [regname(R)]),
  dot_traverse(T),
  io:format("}~n").

dot_traverse([])      -> ok;
dot_traverse({R, ST}) ->
  [io:format("\"~s\" -- \"~s\";~n", [regname(R), regname(R2)]) || {R2, _} <- ST],
  [dot_traverse(P) || P <- ST].

regname(R) ->
  try {registered_name, N} = process_info(R, registered_name), nicename(R, N)
  catch _:_ -> nicename(R)
  end.

nicename(P)    -> io_lib:format("~p", [P]).
nicename(P, N) -> io_lib:format("~p (~p)", [N, P]).
