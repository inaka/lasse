PROJECT = ping_pong

DEPS = cowboy lasse sync

dep_cowboy = git https://github.com/extend/cowboy.git 1.0.x
dep_lasse = git ../../ master
dep_sync = git https://github.com/rustyio/sync master

include erlang.mk

CONFIG ?= "rel/sys.config"
RUN = erl -pa ebin -pa deps/*/ebin -smp enable -s sync -config ${CONFIG}
NODE = ${PROJECT}

run: all
	if [ -n "${NODE}" ]; then ${RUN} -name ${NODE}@`hostname` -s ${PROJECT}; \
	else ${RUN} -s ${PROJECT}; \
	fi
