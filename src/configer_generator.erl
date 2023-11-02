-module(configer_generator).

-include("configer_internal.hrl").
-export([generate/1]).

-spec generate(Spec :: #spec{}) -> ok.
generate(#spec{configs = Configs, defines = Defines}) ->
    maps:foreach(fun(ConfigName, ConfigDef) -> 
        case catch generate_(binary_to_list(ConfigName), ConfigDef, Defines) of
            ok ->
                ok;
            Err ->
                error_logger:error_msg("Failed to generate config for '~s' with error ~p~n", [ConfigName, Err])
        end
    end, Configs).

generate_(ConfigName, #{<<"builds_on">> := BuildsOn, <<"properties">> := Props}, Defines) ->
    case evaluate_props(BuildsOn, Defines, Props) of
        {ok, EvalProps} ->
            ConfigDir = configer_config:get_config_dir(),
            ConfigPath = filename:join(ConfigDir, ConfigName),
            ok = filelib:ensure_dir(ConfigPath),
            FileName = [ConfigPath, ".sys.config"],
            {ok, IODev} = file:open(FileName, [write]),
            file:write(IODev, "[\n"),
            maps:fold(fun(App, AppConfig, PropCount) -> write_app_config(IODev, App, AppConfig, PropCount) end, maps:size(EvalProps), EvalProps),
            file:write(IODev, "]."),
            ok = file:close(IODev);
        Err = {error, _} ->
            Err
    end.

evaluate_props(BuildsOn, Defines, Props) ->
    case evaluate_props_(BuildsOn, Defines, #{}) of
        {ok, Acc} ->
            {ok, maps:merge(Props, Acc)};
        Err = {error, _} ->
            Err
    end.

evaluate_props_([], _Defines, Acc) ->
    {ok, Acc};
evaluate_props_([BuildsOn|Tail], Defines, Acc0) ->
    case evaluate_props_(Tail, Defines, Acc0) of
        {ok, Acc1} ->
            case maps:find(BuildsOn, Defines) of
                {ok, Props} ->
                    {ok, maps:merge(Props, Acc1)};
                error ->
                    {error, {badbuildon, BuildsOn}}
            end;
        Err = {error, _} ->
            Err
    end.

write_app_config(IODev, App, AppConfigs, PropCount) ->
    ok = file:write(IODev, ["\t{", App, ", [\n"]),
    [maps:fold(fun(Key, Value, {PropCount0_, Depth}) -> 
        PropCount1_ = write_prop(IODev, Key, Value, Depth, PropCount0_),
        {PropCount1_, Depth}
    end, {maps:size(AppConfig), 2}, AppConfig) || AppConfig <- AppConfigs],
    ok = file:write(IODev, ["\t]}"]),
    ok = file:write(IODev, case PropCount =/= 1 of
            true -> 
                ",\n";
            false ->
                "\n"
        end),
    PropCount - 1.
write_prop(IODev, Key, Value, Depth, PropCount) ->
    ok = file:write(IODev, [
        lists:duplicate(Depth, "\t"),
        ["{", Key, ",", Value, "}"],
        case PropCount =/= 1 of
            true -> 
                ",\n";
            false ->
                "\n"
        end
    ]),
    PropCount - 1.