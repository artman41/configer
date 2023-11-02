PROJECT = configer
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

define PROJECT_ENV
[
	{config_dir, "./config"}
]
endef

DEPS += yamler

SHELL_OPTS += $(foreach dep,$(DEPS),-eval 'application:load($(dep)), {ok, Mods} = application:get_key($(dep), modules), [Mod:module_info() || Mod <- Mods].')

ERLC_OPTS += +debug_info

include erlang.mk
