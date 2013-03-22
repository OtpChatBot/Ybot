#!/usr/bin/env ruby

#
# Ybot math plugin
# Usage: Ybot math 1 +1
#

require 'net/http'
require 'cgi'

# New math expression
expr = ''

if ARGV.length != 1
	puts 'Wrong argument\nUsage: Ybot math 1 + 1'
	exit
end

# Collect math expr in one string
ARGV.each do |arg|
	expr += arg
end

uri = URI('http://www.google.com/ig/calculator?hl=en&q=' + CGI.escape(expr))
res = Net::HTTP.get_response(uri)

result = res.body.split('rhs:')[1].split(",")[0]

if result == " \"\""
	puts 'Wrong expression'
else
	# print result
	puts 'Answer:' + result.gsub(/\"+/, "")
end
