#!/usr/bin/env ruby

#
# Ybot Chuck Norris plugin
# Usage: Ybot chuck
#

require 'net/http'
require 'json'

uri = URI('http://api.icndb.com/jokes/random')
res = Net::HTTP.get_response(uri)

# get url
result = JSON.parse(res.body)['value']

# return url
puts result["joke"]
