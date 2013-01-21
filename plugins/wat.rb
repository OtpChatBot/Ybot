#!/usr/bin/env ruby

#
# Ybot WAT plugin
# Usage: Ybot wat
#

require 'net/http'
require 'json'

uri = URI('http://watme.herokuapp.com/random')
res = Net::HTTP.get_response(uri)

# get url
result = JSON.parse(res.body)

# return url
puts result["wat"]
