PROJECT = ping_pong

DEPS = cowboy lasse sync

dep_cowboy = git https://github.com/ninenines/cowboy.git  2.0.0-pre.3
dep_lasse = git https://github.com/inaka/lasse.git cowboy2
dep_sync = git https://github.com/rustyio/sync master

include ../../erlang.mk

CONFIG ?= "rel/sys.config"
RUN = erl -pa ebin -pa deps/*/ebin -smp enable -s sync -config ${CONFIG}
NODE = ${PROJECT}

run: all
	if [ -n "${NODE}" ]; then ${RUN} -name ${NODE}@`hostname` -s ${PROJECT}; \
	else ${RUN} -s ${PROJECT}; \
	fi
