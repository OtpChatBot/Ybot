#
# Return Ybot external IP address
#
# Usage:
#
#   Ybot ip
#

import urllib
import urllib2
import simplejson as json

# api url
url = 'http://jsonip.com'
# send response
response = urllib2.urlopen('http://jsonip.com/').read()
# print response
print 'My ip is: ' + json.loads(response)['ip']