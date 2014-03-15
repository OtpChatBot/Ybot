#!/usr/bin/env ruby

#
# Ybot translate text with Google translate
# Usage => Ybot translate en/gb some text
#

require 'uri'
require 'net/http'

# check argument
if ARGV.length != 1
    puts 'Wrong usage. Usage => ybot: translate en/de some text'
    exit
end

languages = ["af","sq","ar", "az", "eu", "bn","be","bg","ca","zh-CN","zh-TW","hr","cs","da","nl","en","eo","et","tl","fi","fr","gl", 
             "ka","de","el","gu","ht","iw","hi","hu","is","id","ga","it","ja","kn","ko","la","lv","lt","mk","ms","mt","no","fa","pl", 
             "pt","ro","ru","sr","sk","sl","es","sw","sv","ta","te","th","tr","uk","ur","vi","cy","yi"]

# get arg
arg = ARGV[0]

# get from lang
from = arg.split(' ')[0].split('/')[0]

# get to lang
to = arg.split(' ')[0].split('/')[1]

if (languages.include? from) == false
    puts 'This language ' + from + ' is not supported'
    exit
end

if (languages.include? to) == false
    puts 'This language ' + to + ' is not supported'
    exit
end

# get text to translate
text = arg.split(to)[1].lstrip

# Make request
uri = URI.parse('http://translate.google.com/?text=' + URI::escape(text.to_s) + '&hl=' + to.to_s + '&langpair=' + from.to_s)
http = Net::HTTP.new(uri.host, uri.port)
request = Net::HTTP::Get.new(uri.request_uri)

# parse response and return translated text
body = http.request(request).body
puts body.split('TRANSLATED_TEXT=\'')[1].split('\'')[0]
