"""
Ybot hacker news plugin.

This plugin has two modes:
  
  * Get all news from front page
  * Get N last news from front page

Usage:

  Ybot hacker_news
  Ybot hacker_news 5

"""

import sys
import urllib
import urllib2
import simplejson as json

# Get news list
def get_news_list():
    # list news
    list_news = []
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
            # add news to list
            list_news.extend([str(i) + '. ' + item['title'] + '--> ' + item['url'] + '\n'])
            i += 1
        # return list news
        return list_news
    # server error
    except urllib2.HTTPError:
        # return empty list
        return []

# Header
front = 'Last hacker news:\n'

if len(sys.argv) > 1:
    try:
        # get news count
        news_count = int(sys.argv[1])
        # get all news
        all_news = get_news_list()
        if all_news == []:
            print "Sorry, but server not response now, please try later."
        else:
            # get news
            news = all_news[:news_count]
            # print news
            for x in news:
                front += x
            print front.encode('utf-8')
    except ValueError:
        print "hacker_news argument must be a number"
else:
    # get all news
    all_news = get_news_list()  
    # print news
    for x in all_news:
        front += x
    print front.encode('utf-8')
