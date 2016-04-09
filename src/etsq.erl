%% @author vinod
%% @doc @todo Add description to ets_queue.


-module(etsq).
-on_load(load_nif/0).

-export([load_nif/0,
         new/1,
         info/1,
         push/2,
         pop/1,
         front/1]).

%% ====================================================================
%% API functions
%% ====================================================================

-define(LIB_BASE_NAME, "etsq").
-define(LIB_NIF_VSN, 1).
-define(LIB_APP_NAME, etsq).

-spec new(atom()) -> ok | {error, already_exists}.
new(_Name) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,?LINE}).

-spec info(atom()) -> ok.
info(_Name) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,?LINE}).

-spec push(atom(), term()) -> ok.
push(Name, Term) ->
    push_back(Name, term_to_binary(Term)).

-spec pop(atom()) -> ok | {error, empty}.
pop(Name) ->
    get_val(pop_front(Name)).

-spec front(atom()) -> ok | {error, empty}.
front(Name) ->
    get_val(get_front(Name)).


get_val(Value) when is_binary(Value) ->
    binary_to_term(Value);
get_val(Value) ->
    Value.

push_back(_Name, _Bin) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,?LINE}).
pop_front(_Name) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,?LINE}).
get_front(_Name) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,?LINE}).

-spec load_nif() -> ok | {error, term()}.
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
