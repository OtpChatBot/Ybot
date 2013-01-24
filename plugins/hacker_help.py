#
# Ybot search N question in stackoverflow.com
#
# Usage:
#
# Ybot hacker_help Convert integer to string in c++
#

import sys
import urllib
import urllib2
import simplejson as json
import zlib

# http://api.stackoverflow.com/1.1/search?&intitle=%3Cdiv%3E

# Get question from command line
question = sys.argv[1]
# stackoverflow api url
api_url = 'http://api.stackoverflow.com/1.1/search?&intitle=' + urllib.quote_plus(question)

# Make request
response = urllib2.urlopen(api_url)
# read response
json_data = response.read()
# Get questions
questions = json.loads(zlib.decompress( json_data, 16+zlib.MAX_WBITS ))['questions']

# print questions with links
for q in questions:
	# return
	print (q['title'] + ' ' + '--> http://stackoverflow.com/questions/' + str(q['question_id'])).encode('utf-8')
