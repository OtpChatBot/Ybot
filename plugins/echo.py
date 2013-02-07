#
# Simple Ybot echo plugin
#
# Usage:
#
# Ybot echo some text
#

import sys

if len(sys.argv) != 2:
	print 'Wrong arguments\nUsage: Ybot echo some text'
	exit(0)

# return
print sys.argv[1]