#!/usr/bin/sh

#
# Ybot today plugin. Show curent day of the week.
#

case $( uname -s ) in
Darwin) echo echo `date "+%A"`;;
*)      echo `date --date=${dateinfile#?_} "+%A"`;;
esac
