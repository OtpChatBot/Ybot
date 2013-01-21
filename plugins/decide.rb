#!/usr/bin/env ruby

#
# Ybot decision helper plugin
# Usage: Ybot decide foo bar
#

options = ARGV[0].split(' ')

puts 'Definitely: ' + options[rand(options.length)]
