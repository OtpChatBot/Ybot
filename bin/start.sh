#!/bin/bash

SKYPE_PID=$(ps x | grep 'skype.py' |  awk '/python/' | awk '{print $1}')
if [ -n "$SKYPE_PID" ]; then
   kill $SKYPE_PID
fi

cp ybot.config ebin/

erl -pa deps/*/ebin ebin \
    -boot start_sasl +P 2000000 \
    -sname ybot@$(hostname) \
    -s ybot \
    -config ybot \
    -noshell \
    -detached