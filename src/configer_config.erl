-module(configer_config).
-export([get_config_dir/0]).

get_config_dir() ->
    {ok, ConfigDir} = get_value(config_dir),
    ConfigDir.

%% Internal

get_value(Key) ->
    application:load(configer),
    application:get_env(configer, Key).

get_value(Key, Default) ->
    case get_value(Key) of
        {ok, Value} ->
            Value;
        undefined ->
            Default
    end.