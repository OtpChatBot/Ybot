#!/usr/bin/env ruby

#
# Ybot GitHub status plugin
# Usage: Ybot github_status
#

require 'net/https'
require 'json'

uri = URI('https://status.github.com/api/status.json')
http = Net::HTTP.new(uri.host, 443)
http.use_ssl = true
http.verify_mode = OpenSSL::SSL::VERIFY_NONE
res = http.start do |http|
        request = Net::HTTP::Get.new(uri.request_uri)
        http.request(request)
      end
result = JSON.parse(res.body)
puts "GitHub status: " + result['status']
