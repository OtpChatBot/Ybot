#!/bin/bash

kill $(ps x | grep 'skype.py' |  awk '/python/' | awk '{print $1}')
cp ../ybot.config ../ebin

erl -pa ../deps/*/ebin ../ebin \
    -boot start_sasl +P 2000000 \
    -sname ybot@$(hostname) \
    -noshell \
    -detached \
    -s ybot \
    -config ybot
