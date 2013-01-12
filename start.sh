#!/bin/sh

cp ybot.config `pwd`/ebin
cd `pwd`/ebin 
exec erl -s ybot -config ybot