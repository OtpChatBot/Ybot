"""
Ybot check site up/down

Usage:

  Ybot check-site http://example.com 

"""

import sys
import urllib
import urllib2

# Get url for checking
url = sys.argv[1]
# Send request
response = urllib2.urlopen('http://www.isup.me/' + url).read()
# check site up or down
check_site = 'It\'s just you' in response

# Return
if check_site == True:
	print "Site is up"
else:
	print "Site is down"

