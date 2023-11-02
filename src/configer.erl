-module(configer).

-include("configer_internal.hrl").

-export([load/1]).

-spec load(SpecYaml :: file:name()) -> {ok, #spec{}} | {error, {Reason, Path :: file:name()}}
    when Reason :: badformat | badconfig | baddefines | badinclude.
load(SpecYaml) ->
    configer_loader:load(SpecYaml).

-spec generate(Spec :: #spec{}) -> no_return().
generate(Spec) ->
    configer_generator:generate(Spec).