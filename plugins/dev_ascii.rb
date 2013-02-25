#!/usr/bin/env ruby

#
## Eval simple erlang expressions with tryerlang.org
## Usage:
## Ybot erl 1 + 1.
#

require 'net/http'
require 'cgi'

text = ARGV[0]

# send request
def http_get(domain, path, params)
  print "#{path}?".concat(params.collect { |k,v| "#{k}=#{CGI::escape(v.to_s)}" }.join('&'))
    return Net::HTTP.get(domain, "#{path}?".concat(params.collect { |k,v| "#{k}=#{CGI::escape(v.to_s)}" }.join('&'))) if not params.nil?
    return Net::HTTP.get(domain, path)
end

response = http_get("asciime.heroku.com", "/generate_ascii", {:s => text})
puts response
