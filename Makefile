PROJECT = lasse

SHELL_DEPS = sync
TEST_DEPS = cowboy shotgun meck katana mixer
BUILD_DEPS = inaka_mk hexer_mk

dep_meck = hex 0.8.4
dep_cowboy = git https://github.com/extend/cowboy.git 1.0.4
dep_shotgun = hex 0.1.15
dep_katana = hex 0.2.18
dep_mixer = git https://github.com/inaka/mixer.git 0.1.4
dep_inaka_mk = git https://github.com/inaka/inaka.mk.git 1.0.0
dep_hexer_mk = git https://github.com/inaka/hexer.mk.git 1.0.1

DEP_PLUGINS = inaka_mk hexer_mk

include erlang.mk

ERLC_OPTS := +warn_unused_vars +warn_export_all +warn_shadow_vars +warn_unused_import +warn_unused_function
ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
ERLC_OPTS += +warn_export_vars +warn_exported_vars +warn_missing_spec +warn_untyped_record +debug_info

SHELL_OPTS += -s sync
