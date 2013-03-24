#!/bin/sh

kill $(ps x | grep 'skype.py' |  awk '/python/' | awk '{print $1}')
cp ybot.config `pwd`/ebin
exec erl -pa ebin/ deps/*/ebin plugins/*/ebin -s ybot -config ybot -s reloader -boot start_sasl +P 2000000
