%% @author vinod
%% @doc @todo Add description to ets_queue.


-module(etsq).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-on_load(load_nif/0).

-export([load_nif/0,
         new/1,
         info/1,
         push/2,
         pop/1]).

-export([test/0,
         test1/0]).

%% ====================================================================
%% API functions
%% ====================================================================



%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
    {ok, #state{}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(Reason, State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(OldVsn, State, Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

-define(LIB_BASE_NAME, "etsq").
-define(LIB_NIF_VSN, 1).
-define(LIB_APP_NAME, etsq).


load_nif() ->
    LibBaseName = ?LIB_BASE_NAME,
    PrivDir = code:priv_dir(etsq),
    LibName = case erlang:system_info(build_type) of
          opt ->
              LibBaseName;
          Type ->
              LibTypeName = LibBaseName ++ "."  ++ atom_to_list(Type),
              case (filelib:wildcard(
                  filename:join(
                [PrivDir,
                 "lib",
                 LibTypeName ++ "*"])) /= []) orelse
              (filelib:wildcard(
                 filename:join(
                   [PrivDir,
                "lib",
                erlang:system_info(system_architecture),
                LibTypeName ++ "*"])) /= []) of
              true -> LibTypeName;
              false -> LibBaseName
              end
          end,
    Lib = filename:join([PrivDir, "lib", LibName]),
    Status = case erlang:load_nif(Lib, ?LIB_NIF_VSN) of
         ok -> ok;
         {error, {load_failed, _}}=Error1 ->
             ArchLibDir =
             filename:join([PrivDir, "lib",
                    erlang:system_info(system_architecture)]),
             Candidate =
             filelib:wildcard(filename:join([ArchLibDir,LibName ++ "*" ])),
             case Candidate of
             [] -> Error1;
             _ ->
                 ArchLib = filename:join([ArchLibDir, LibName]),
                 erlang:load_nif(ArchLib, ?LIB_NIF_VSN)
             end;
         Error1 -> Error1
         end,
    case Status of
    ok -> ok;
    {error, {E, Str}} ->
        error_logger:error_msg("Unable to load ~p nif library. "
                   "Failed with error:~n\"~p, ~s\"~n", [?LIB_APP_NAME, E, Str]),
        Status
    end.


new(_Name) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,?LINE}).

info(_Name) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,?LINE}).

push(Name, Term) ->
    push_back(Name, term_to_binary(Term)).

pop(Name) ->
    case pop_front(Name) of
        Value when is_integer(Value) ->
            Value;
        Value ->
            binary_to_term(Value)
    end.

push_back(_Name, _Bin) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,?LINE}).
pop_front(_Name) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,?LINE}).

test() ->
    io:format("~nets:new(a) -> ~p.~n", [etsq:new(a)]),
    io:format("~netsq:push(a, 1) -> ~p.~n", [etsq:push(a, 1)]),
%%     timer:sleep(100),
    io:format("~netsq:pop(a) -> ~p.~n", [etsq:pop(a)]).


test1() ->
    etsq:new(b),
    etsq:push(b, 1),
    etsq:pop(b).