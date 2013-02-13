#!/usr/bin/env python

import sys
import time
import urllib2
import urllib
import Skype4Py

# get host
host = sys.argv[1]
# get port
port = sys.argv[2]

"""
	Init skype bot
"""
class SkypeBot(object):
	# init skype connection
	def __init__(self):
		self.Skype = Skype4Py.Skype()
		self.Skype.Attach()
		self.Skype.OnMessageStatus = self.MessageStatus
		
	# get incoming message
	def MessageStatus(self, msg, status):
		if status == Skype4Py.cmsReceived:
			try:
				# Check message
				if msg.Body.split(' ')[0] == self.Skype.CurrentUser.Handle:
					# send request to Ybot
					response = urllib2.urlopen(host + ':' + str(port), data = urllib.urlencode({msg.Body : msg.Body}))
					# send response
					msg.Chat.SendMessage(response.read())
			except URLError:
				exit(0)

bot = SkypeBot()

while True:
	time.sleep(1)
