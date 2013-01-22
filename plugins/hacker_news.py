"""
Ybot hacker news plugin

Usage:

  Ybot hacker_news 

"""

import sys
import urllib
import urllib2
import simplejson as json

mode = sys.argv[1]

# Header
front = 'Last hacker news:\n'
# News counter
i = 1

try:
	# Api url
	api_url = 'http://api.ihackernews.com/page'
	# header
	header = { "Content-Type": "application/json" }
	# Get front page
	response = urllib2.urlopen(urllib2.Request(api_url, '', header))
	# read response
	json_data = response.read()
	# Get news items
	items = json.loads(json_data)['items']
	# Make string titles
	for item in items:
		front += str(i) + '. ' + item['title'] + '--> ' + item['url'] + '\n'
		i += 1
	# response
	print front.encode('utf-8')
except urllib2.HTTPError:
	print "Sorry, but server not response now, please try later."