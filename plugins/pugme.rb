#!/usr/bin/env ruby

#
# Ybot pugme plugin
# Usage: Ybot pugme
#

require 'net/http'
require 'json'

uri = URI('http://pugme.herokuapp.com/random')
res = Net::HTTP.get_response(uri)

# get url
result = JSON.parse(res.body)['pug']

# return url
puts result
