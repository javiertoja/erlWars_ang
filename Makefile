PROJECT = erlwars

DEPS = cowboy jiffy
dep_cowboy = pkg://cowboy master
dep_jiffy = https://github.com/davisp/jiffy.git master
include ./erlang.mk
