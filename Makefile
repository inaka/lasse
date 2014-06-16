PROJECT = lasse

TEST_DEPS = cowboy lager sync gun meck

dep_cowboy = https://github.com/extend/cowboy.git master
dep_lager = https://github.com/basho/lager.git master
dep_sync = https://github.com/rustyio/sync.git master
dep_gun = https://github.com/extend/gun.git master
dep_meck = https://github.com/eproxus/meck master

include erlang.mk

TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'

CT_SUITES = lasse_handler
CT_OPTS = -cover test/lasse_handler.coverspec -cover_stop false -erl_args -config rel/sys.config 

test-shell: build-tests
	erl -pa ebin -pa deps/*/ebin -pa test -s sync -config rel/sys.config
