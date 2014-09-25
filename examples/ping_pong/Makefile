PROJECT = ping_pong

DEPS = cowboy lager lasse sync

dep_cowboy = https://github.com/extend/cowboy.git 0.10.0
dep_lager = https://github.com/basho/lager.git master
dep_lasse = ../../ master
dep_sync = https://github.com/rustyio/sync master

include erlang.mk

ERLC_OPTS += +'{parse_transform, lager_transform}'

CONFIG ?= "rel/sys.config"
RUN = erl -pa ebin -pa deps/*/ebin -smp enable -s sync -config ${CONFIG}
NODE = ${PROJECT}

run: all
	if [ -n "${NODE}" ]; then ${RUN} -name ${NODE}@`hostname` -s ${PROJECT}; \
	else ${RUN} -s ${PROJECT}; \
	fi
