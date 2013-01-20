#!/usr/bin/python

#
# Ybot url shortener plugin
# Usage:
# 
# Ybot short_url https://twitter.com/
#

import sys
import urllib
import urllib2
import simplejson as json

# Get url from command args
url = sys.argv[1]
# Google api url
api_url = "https://www.googleapis.com/urlshortener/v1/url"
# Request headers
header = { "Content-Type": "application/json" }
# Request body params
params = { "longUrl": url }

# Make request
response = urllib2.urlopen(urllib2.Request(api_url, json.dumps(params), header))
# Got json data
json_data = response.read()

# Return shorten url
print json.loads(json_data)['id']