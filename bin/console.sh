#!/bin/bash

erl -remsh ybot@$(hostname) -sname ybot_$RANDOM@$(hostname)