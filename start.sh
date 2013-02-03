#!/bin/sh

cp ybot.config `pwd`/ebin
exec erl -pa ebin/ deps/*/ebin -s ybot -config ybot -s reloader -boot start_sasl +P 2000000
