#!/usr/bin/env python

import sys, os
import time
import urllib2
import urllib
import thread

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
                if (msg == 'name?'):
                    msg.Chat.SendMessage(self.Skype.CurrentUser.Handle)
                # Check message
                elif msg.Body.split(' ')[0] == self.Skype.CurrentUser.Handle:
                    # send request to Ybot
                    response = urllib2.urlopen(host + ':' + str(port), data = urllib.urlencode({msg.Body : msg.Body}))
                    # send response
                    msg.Chat.SendMessage(response.read())
            except URLError:
                exit(0)

# create bot
bot = SkypeBot()

# Send ping to Ybot every minute
# if ping failed, exit from script.
def host_ping():
    try:
        urllib2.urlopen(host + ':' + str(port))
    except:
        os._exit(1)
    time.sleep(30)
    host_ping()

thread.start_new_thread(host_ping, ())

while True:
    time.sleep(1)
