-module(configer_loader).

-include("configer_internal.hrl").
-export([load/1]).

-spec load(SpecYaml :: file:name()) -> {ok, #spec{}} | {error, {Reason, Path :: file:name()}}
    when Reason :: badformat | badconfig | baddefines | badinclude.
load(SpecYaml) ->
    case yaml:load_file(SpecYaml) of
        {ok, [Spec = #{<<"configs">> := _}]} ->
            load_(Spec);
        {ok, _} ->
            {error, badformat};
        {error, R} ->
            {error, {R, SpecYaml}}
    end.

load_(Spec = #{<<"configs">> := Configs}) ->
    Includes = maps:get(<<"includes">>, Spec, []),
    case load_includes(Includes, #{}) of
        {ok, Acc0} ->
            Defines = maps:get(<<"defines">>, Spec, #{}),
            case verify_definitions(Defines, Acc0) of
                {ok, Acc1} ->
                    case verify_definitions(Configs, Acc1) =/= error of
                        true ->
                            {ok, #spec{configs = Configs, defines = Acc1}};
                        false ->
                            {error, badconfig}
                    end;
                error ->
                    {error, baddefines}
            end;
        Err = {error, _} ->
            Err
    end.

load_includes([], Acc) ->
    {ok, Acc};
load_includes([Path | Tail], Acc0) ->
    case yaml:load_file(Path) of
        {ok, [Spec0]} ->
            Spec1 = default_spec(Spec0),
            case load_includes_(Spec1) of
                {ok, Acc} ->
                    load_includes(Tail, maps:merge(Acc, Acc0));
                {error, R} ->
                    {error, {R, Path}}
            end;
        Err = {error, _} ->
            Err
    end.

load_includes_(#{<<"can_be_included">> := false}) ->
    {error, badinclude};
load_includes_(#{<<"includes">> := Includes, <<"defines">> := Defines}) ->
    Acc0 = load_includes(Includes, #{}),
    case verify_definitions(Defines, Acc0) of
        Ok = {ok, _} ->
            Ok;
        error ->
            {error, baddefines}
    end.

verify_definitions(Defines, Acc) ->
    verify_definitions_(maps:keys(Defines), Defines, Acc).

verify_definitions_([], _Defines, Acc) ->
    {ok, Acc};
verify_definitions_([DefineKey|Tail], Defines, Acc0) ->
    Define0 = maps:get(DefineKey, Defines),
    Define1 = default_definition(Define0),
    BuildsOn = maps:get(<<"builds_on">>, Define1),
    HasAllReqs = 
        lists:all(fun(Requirement) -> 
            maps:is_key(Requirement, Acc0)
        end, BuildsOn),
    case HasAllReqs of
        true ->
            Acc1 = maps:merge(Define1, Acc0),
            verify_definitions_(Tail, Defines, Acc1);
        false ->
            error
    end.

default_spec() ->
    #{
        <<"can_be_included">> => false,
        <<"configs">> => #{},
        <<"defines">> => #{},
        <<"includes">> => []
    }.

default_spec(Map) ->
    Acc0 = default_spec(),
    Acc1 = merge_if_exists(<<"can_be_included">>, Map, Acc0),
    Acc2 = merge_if_exists(<<"includes">>, Map, Acc1),
    Acc3 = merge_if_exists(<<"defines">>, Map, Acc2),
    merge_if_exists(<<"configs">>, Map, Acc3).

default_definition() ->
    #{<<"builds_on">> => [], <<"properties">> => []}.

default_definition(Map) ->
    Acc0 = default_definition(),
    Acc1 = merge_if_exists(<<"builds_on">>, Map, Acc0),
    merge_if_exists(<<"properties">>, Map, Acc1).

merge_if_exists(Key, Map, Acc) ->
    case maps:find(Key, Map) of
        {ok, Value} ->
            Acc#{Key => Value};
        error ->
            Acc
    end.