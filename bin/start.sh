#!/bin/bash

SKYPE_PID=$(ps x | grep 'skype.py' |  awk '/python/' | awk '{print $1}')
if [ -n "$SKYPE_PID" ]; then
   kill $SKYPE_PID
fi

if [ -f "ybot.config" ]
then
    cp ybot.config ebin/
else
    echo "Unable to find ybot.config file!"
    exit 1
fi

erl -pa deps/*/ebin plugins/*/ebin ebin \
    -boot start_sasl +P 2000000 \
    -sname ybot@$(hostname) \
    -s ybot \
    -config ybot \
    -noshell \
    -detached