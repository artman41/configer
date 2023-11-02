-module(specification_SUITE).

-behaviour(ct_suite).

%%% ct_suite callbacks
-export([all/0]).
-export([groups/0]).
-export([suite/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([group/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%%% ct_suite callbacks
-spec all() -> [TestDef :: ct_suite:ct_test_def()] | {skip, Reason :: term()}.
all() ->
	[].

-spec groups() -> [GroupDef :: ct_suite:ct_group_def()].
groups() ->
	[].

-spec suite() -> [Info :: ct_suite:ct_info()].
suite() ->
	[].

-spec init_per_suite(Config :: ct_suite:ct_config()) -> NewConfig :: ct_suite:ct_config() | {skip, Reason :: term()} | {skip_and_save, Reason :: term(), SaveConfig :: ct_suite:ct_config()}.
init_per_suite(Config) ->
	Config.

-spec end_per_suite(Config :: ct_suite:ct_config()) -> term() | {save_config, SaveConfig :: ct_suite:ct_config()}.
end_per_suite(Config) ->
	Config.

-spec group(GroupName :: ct_suite:ct_groupname()) -> [Info :: ct_suite:ct_info()].
group(_) ->
	[].

-spec init_per_group(GroupName :: ct_suite:ct_groupname(), Config :: ct_suite:ct_config()) -> NewConfig :: ct_suite:ct_config() | {skip, Reason :: term()}.
init_per_group(_GroupName, Config) ->
	Config.

-spec end_per_group(GroupName :: ct_suite:ct_groupname(), Config :: ct_suite:ct_config()) -> term() | {return_group_result, Status :: ct_suite:ct_status()}.
end_per_group(_GroupName, Config) ->
	Config.

-spec init_per_testcase(TestCase :: ct_suite:ct_testname(), Config :: ct_suite:ct_config()) -> NewConfig :: ct_suite:ct_config() | {fail, Reason :: term()} | {skip, Reason :: term()}.
init_per_testcase(_TestCase, Config) ->
	Config.

-spec end_per_testcase(TestCase :: ct_suite:ct_testname(), Config :: ct_suite:ct_config()) -> term() | {fail, Reason :: term()} | {save_config, SaveConfig :: ct_suite:ct_config()}.
end_per_testcase(_TestCase, Config) ->
	Config.

