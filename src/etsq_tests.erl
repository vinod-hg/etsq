%% @author vinod
%% @doc @todo Add description to etsq_tests.


-module(etsq_tests).
-compile(export_all).

-export([init/0,
         time/3,
         stats/3]).

-type microseconds() :: pos_integer().
-type milliseconds() :: pos_integer().

%% ====================================================================
%% API functions
%% ====================================================================

init() ->
    etsq:new(queue),
    ets:new(tab, [named_table, public]).

-spec time(run_ets | run_queue, pos_integer()) -> microseconds().
time(Op, NumOp) ->
    {Time, _} = timer:tc(?MODULE, Op, [NumOp]),
    Time.

-spec time(pos_integer(), run_ets | run_queue, pos_integer()) -> microseconds().
time(NumProc, Op, NumOp) ->
    {Time, _} = timer:tc(?MODULE, spawn, [NumProc, Op, NumOp]),
    Time.

-spec stats(run_ets | run_queue, pos_integer()) -> milliseconds().
stats(Op, NumOp) ->
    erlang:statistics(runtime),
    ?MODULE:Op(NumOp),
    {_, Time} = erlang:statistics(runtime),
    Time.

-spec stats(pos_integer(), run_ets | run_queue, pos_integer()) -> milliseconds().
stats(NumProc, Op, NumOp) ->
    erlang:statistics(runtime),
    ?MODULE:spawn(NumProc, Op, NumOp),
    {_, Time} = erlang:statistics(runtime),
    Time.

run_ets(Num) ->
    Self = self(),
    Data = lists:seq(1, 100),
    L = lists:seq(1, Num),
    [ets:insert(tab, {{Self, K}, Data}) || K <- L],
    [ets:take(tab, {Self, K}) || K <- L].

run_queue(Num) ->
    Self = self(),
    Data = lists:seq(1, 100),
    L = lists:seq(1, Num),
    [etsq:push(queue, {{Self, K}, Data}) || K <- L],
    [etsq:pop(queue) || _ <- L].

spawn(NumProc, Op, NumOp) ->
    Pid = self(),
    L = lists:seq(1, NumProc),
    [spawn_link(fun() -> ?MODULE:Op(NumOp), Pid ! done end) || _ <- L],
    [receive done -> ok end || _ <- L].
