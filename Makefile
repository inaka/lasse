PROJECT = lasse

CONFIG ?= rel/sys.config

SELL_DEPS = sync
TEST_DEPS = cowboy lager shotgun meck katana xref_runner

dep_meck = git https://github.com/eproxus/meck.git 0.8.2
dep_cowboy = git https://github.com/extend/cowboy.git 1.0.1
dep_shotgun = git https://github.com/inaka/shotgun.git 0.1.11
dep_katana = git https://github.com/inaka/erlang-katana.git 0.2.5
dep_xref_runner = git https://github.com/inaka/xref_runner.git 0.2.2

DIALYZER_DIRS := ebin/
DIALYZER_OPTS := --verbose --statistics -Werror_handling \
                 -Wrace_conditions #-Wunmatched_returns

include erlang.mk

ERLC_OPTS := +warn_unused_vars +warn_export_all +warn_shadow_vars +warn_unused_import +warn_unused_function
ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
ERLC_OPTS += +warn_export_vars +warn_exported_vars +warn_missing_spec +warn_untyped_record +debug_info

TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'
CT_OPTS += -cover test/${PROJECT}.coverspec -vvv -erl_args -config ${CONFIG}

SHELL_OPTS += -name ${PROJECT}@`hostname` -config ${CONFIG} -s lager -s sync

quicktests: app
	@$(MAKE) --no-print-directory app-build test-dir ERLC_OPTS="$(TEST_ERLC_OPTS)"
	@mkdir -p logs/
	$(gen_verbose) $(CT_RUN) -suite $(addsuffix _SUITE,$(CT_SUITES)) $(CT_OPTS)

erldocs:
	erldocs . -o docs
