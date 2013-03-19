Ybot
===============

Ybot is a customizable bot which was inspired by GitHub's
[Hubot](http://hubot.github.com/). Hubot is really cool, but sorry, I
don't like JavaScript and I don't know coffescript. Ybot is completely
written in Erlang/OTP and you can write plugins in Python, Perl, Ruby,
or even in shell.

![Ybot](http://s7.postimage.org/5hiyut7ff/logo79187921.png "Ybot logo")

[![Build Status](https://travis-ci.org/0xAX/Ybot.png)](https://travis-ci.org/0xAX/Ybot)

[![Flattr this git repo](http://api.flattr.com/button/flattr-badge-large.png)](https://flattr.com/submit/auto?user_id=0xAX&url=https://github.com/0xAX/Ybot&title=Ybot&language=&tags=github&category=software)

Features
=========

  * Completely written in Erlang;
  * Simultaneously run any number of bots on different transports;
  * You don't need to know Erlang to write a plugin;
  * Supports plugins in different technology:
    * Python plugins;
    * Ruby plugins;
    * Shell plugins;
    * Perl plugins;
    * Elixir plugins;
    * Scala plugins;
  * Command history with tunable limit;
  * Dynamic search and loading of plugins;
  * Very easily extensible, just put plugin to plugin directory;
  * Supports IRC and IRC via SSL;
  * Supports XMPP and XMPP via ssl;
  * Supports Campfire
  * HTTP interface;
  * GTalk supporting;
  * HipChat supporting;
  * Flowdock supporting;
  * Skype supporting;
  * Talkerapp supporting;

Building and Running
=====================

First of all you must get your own Ybot:

```
git clone https://github.com/0xAX/Ybot.git
```

Or download the source file archive: [.tar.gz](https://github.com/0xAX/Ybot/tarball/master) or [.zip](https://github.com/0xAX/Ybot/zipball/master)

After getting source you need to download dependencies and build the source:

```
./rebar get-deps && ./rebar compile
```

Then edit the `ybot.config` configuration file and you can run your Ybot copy:

```
./start.sh
```

Run on heroku
===============

  * Get you ybot and configure it.
  * Download heroku

```
cd Ybot
heroku create your-application-name -s cedar
heroku config:add BUILDPACK_URL=http://github.com/heroku/heroku-buildpack-erlang.git
git push heroku master
```

Dependencies
=============

  * [lager](https://github.com/basho/lager) - A logging framework for Erlang/OTP.
  * [reloader](https://github.com/bjnortier/reloader) - Mochiweb's reloader.
  * [ibrowse](https://github.com/cmullaparthi/ibrowse) - Erlang http client.
  * [mochijson2](https://github.com/bjnortier/mochijson2) - Erlang json encoder/decoder.
  * [cowboy](https://github.com/extend/cowboy) - Small, fast, modular HTTP server written in Erlang.
  * [Skype4Py](https://github.com/awahlig/skype4py) - Platform indepeneant Python wrapper for the Skype API.

Transport
==========

Ybot's basic transport is a network interface. Ybot is a chat bot and he
spends all of his life chatting. Ybot receives chat messages and execute
commands depending on those received messages. At that moment Ybot
supports:

  * IRC (+ssl supporting, +private messages supporting).
  * XMPP MUC (+single user chat supporting, +private messages supporting, +ssl supporting).
  * Campfire.
  * HTTP.
  * Gtalk.
  * HipChat.
  * Flowdock.
  * Skype.
  * Talkerapp.

Plugins
==========

Ybot is a chat bot and it can execute different commands. Commands are
simple chat messages. For example, a chat session:

```
you: Ybot math 1 + 5
Ybot: Answer: 6
```

Here are a few simple rules for structuring Ybot plugins.

  * You must address messages to the `Ybot`.

  * After addressing the bot you specify the command to be executed, for example `math` or `ping`. Every command consists from one word.

  * After the command you can specify arguments. Ybot sends all arguments in '' and it turns them into one argument.

  * One command = One plugin. Plugins must live in the `plugins` directory.

  * Each plugin must have the correct extension, for example:

    * .py
    * .rb
    * .shell

Containing Python, Ruby or shell code.

  * The name of the plugin file must be the same name as the command.
    For example if we have a `Ybot ping` command, we must have a plugin
    named `ping.py` or `ping.rb` or `ping.shell`, etc.

  * Plugin can consist of any code but write the results to `STDOUT` in the end.

Example Ybot:

```
Ybot math 3 ^ 2
```

Here Ybot calls the `math` plugin with the argument: '3 ^ 2'

Current plugins
================

  * chuck - Chuck Norris quotes :)
  * decide - Ybot try to help make decisions for you.
  * github_status - Github status state.
  * echo - simple echo plugin.
  * help - Ybot help.
  * ping - Ybot simple ping/pong.
  * math - Ybot calculate math expressions.
  * date - Ybot show date/time.
  * pugme - Ybot pugme service plugin.
  * erl   - Ybot computation of erlang expression using tryerlang.org.
  * today? - Ybot return current day.
  * shorten_url - Ybot url shortener with goo.gl.
  * hacker_news - Ybot download news from https://news.ycombinator.com/
  * wat - Ybot random WAT image
  * check-site - Ybot check site up/down state
  * ruby - Ybot eval simple ruby expression
  * ip - Ybot external ip
  * hacker_help - Ybot search in stackoverflow.
  * translate - use google translate from Ybot.
  * url - url encode/decode plugin.

These are Ybot's core plugins. You can find other plugins at [ybot-contrib](https://github.com/0xAX/ybot-contrib) or see at [ybot plugins catalog](http://0xax.github.com/ybot-plugins.html).

Ybot brain REST API
============
Ybot brain allows to persist data using REST API. It is designed to support multiple storage engines, but the default is based on Mnesia.

REST API endpoint:
`http://localhost:8090/memories` (port and host is configurable)

An example JSON memory object:
```
{
 "id":"88DAF3FFB0419E0A8368036200000553620002B99862000429C1",
 "plugin":"foo",
 "key":"key1",
 "value":"foo1",
 "created":"2013-03-13 23:45:00"
 }
```


Contribute
============

Ybot is an open source project under the Erlang public license (see LICENSE file). Issues, questions and patches are welcome.

If you're hacking Ybot core, please, before sending your pull request,
pull and merge Ybot master to avoid conflicts.

  * Fork main ybot repository (https://github.com/0xAX/Ybot).
  * Make your changes in your clone of ybot.
  * Test it.
  * Send pull request.

Author
========

Creator of Ybot [@0xAX](https://twitter.com/0xAX).

Thank you all who participating in Ybot developing. Names and contact information of those who helped Yubotu, you can find the file [AUTHORS](https://github.com/0xAX/Ybot/blob/master/AUTHORS)
