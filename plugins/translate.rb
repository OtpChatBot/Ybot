#!/usr/bin/env ruby

#
# Ybot translate text with Google translate
# Usage => Ybot translate en/gb some text
#

require 'uri'
require 'net/http'

# check argument
if ARGV.length != 1
    puts 'Wrong usage.\nUsage =>Ybot translate en/gb some text'
    exit
end

languages = ["af","sq","ar", "az", "eu", "bn","be","bg","ca","zh-CN","zh-TW","hr","cs","da","nl","en","eo","et","tl","fi","fr","gl", 
             "ka","de","el","gu","ht","iw","hi","hu","is","id","ga","it","ja","kn","ko","la","lv","lt","mk","ms","mt","no","fa","pl", 
             "pt","ro","ru","sr","sk","sl","es","sw","sv","ta","te","th","tr","uk","ur","vi","cy","yi"]

# get arg
arg = ARGV[0]
# get from lang
from = arg.split(' ')[0].split('/')
# get to lang
to = arg.split(' ')[0].split('/')[1]

if (languages.include? from[0]) == false
	puts 'This language ' + from[0] + ' is not supported'
	exit
end

if (languages.include? to) == false
	puts 'This language ' + to + ' is not supported'
	exit
end

# get text to translate
text = arg.split(to)[1].lstrip
# Make request
uri = URI.parse('http://translate.google.com/translate_t?text=' + URI::escape(text.to_s) + '&hl=' + to.to_s + '&langpair=' + from.to_s[0])
http = Net::HTTP.new(uri.host, uri.port) 
request = Net::HTTP::Get.new(uri.request_uri)
# return
puts http.request(request).body.split('#fff\'">')[1].split('</span>')[0]
