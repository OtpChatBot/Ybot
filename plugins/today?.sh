#!/usr/bin/sh

#
# Ybot today plugin. Show curent day of the week.
#

PLATFORM=$(uname -s)
if [ "$PLATFORM" == "Darwin" ]
then
    echo `date "+%A"`
else
    echo `date --date=${dateinfile#?_} "+%A"`
fi
