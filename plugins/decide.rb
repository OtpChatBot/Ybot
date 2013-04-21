#!/usr/bin/env ruby

#
# Ybot decision helper plugin
# Usage: Ybot decide foo bar
#

if ARGV.length != 1
    puts 'Wrong argument\nUsage: Ybot decide foo bar'
    exit
end

options = ARGV[0].split(' ')

puts 'Definitely: ' + options[rand(options.length)]
