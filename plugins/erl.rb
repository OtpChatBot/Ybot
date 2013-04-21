#!/usr/bin/env ruby

#
## Eval simple erlang expressions with tryerlang.org
## Usage:
## Ybot erl 1 + 1.
#

require 'net/http'
require 'json'

# new erlang expression 
expr = ''

if ARGV.length != 1
    puts 'Wrong argument\nUsage: Ybot erl 1 + 1.'
    exit
end

# collect arguments in expression
ARGV.each do |arg|
    expr += arg
end

# send request
uri = URI('http://www.tryerlang.org/api/eval/default/intro')
# get response
response = Net::HTTP.post_form(uri, {'expression' => expr})

puts 'Result: ' + JSON.parse(response.body)['result']