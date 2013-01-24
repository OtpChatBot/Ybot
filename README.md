Ybot
===============

Ybot - is a customizable bot software which inspired with Github hubot. Github hubot is realy cool, but sorry, i don't like javascript and i don't know coffescript. Ybot written fully with erlang/otp and you can write plugins in other different scripting language like python, ruby or shell. 

[![Build Status](https://travis-ci.org/0xAX/Ybot.png)](https://travis-ci.org/0xAX/Ybot)

[![Flattr this git repo](http://api.flattr.com/button/flattr-badge-large.png)](https://flattr.com/submit/auto?user_id=0xAX&url=https://github.com/0xAX/Ybot&title=Ybot&language=&tags=github&category=software)

Features
=========

  * Fully written with erlang;
  * Simultaneously run any number of bots on different transports;
  * Supporting plugins in different technology;
  * Now supporting:
    * Python plugins;
    * Ruby plugins;
    * Shell plugins;
    * Perl plugins;
    * Elixir plugins;
  * To write a plugin does not need to know erlang;
  * Dynamic observing and loading plugins;
  * Very easily extensible;
  * Irc supporting, also irc ssl supporting;
  * XMPP supporting;
  * Campfire supporting;

Building and Running
=====================

First of all you must get your own Ybot:

```
git clone https://github.com/0xAX/Ybot.git
```

Or download archive with source: [.tar.gz](https://github.com/0xAX/Ybot/tarball/master) or [.zip](https://github.com/0xAX/Ybot/zipball/master)

After getting source you must download dependencies and build sources:

```
./rebar get-deps && ./rebar compile
```

After that configure with ybot.config and you can run your Ybot copy:

```
./start.sh
```

Dependencies
=============

  * [lager](https://github.com/basho/lager) - A logging framework for Erlang/OTP.
  * [reloader](https://github.com/bjnortier/reloader) - Mochiweb's reloader.
  * [ibrowse](https://github.com/cmullaparthi/ibrowse) - Erlang http client.
  * [mochijson2](https://github.com/bjnortier/mochijson2) - Erlang json encoder/decoder.

Transport
==========

Ybot transport - is network interface which Ybot supported. Ybot is chat bot and all time of his life he spends chatting. Ybot receives chat messages and execute some commands depending on received message. Add that moment hubot supports:

  * Irc supporting.
  * Xmpp muc supporting. 
  * Campfire supporting.

Plugins
==========

Ybot chat bot and it can execute different commands. Command - is simple chat message. For example chat session:

```
you: Ybot math 1 + 5
Ybot: Answer: 6
```

Here are a few simple rules, of structure of the all Ybot plugins.

  * Every message to Ybot must started from `Ybot` chat login.

  * After `Ybot` login is command for example `math` or `ping` or something else supporting command. Every command consists from one word.

  * After `Ybot` login and command are command arguments. Ybot send all arguments in '', it turns one argument. 

  * One command = One plugin. We must have plugins in our plugin directory.

  * Plugin is file necessarily with extension of:

    * .py
    * .rb
    * .shell

with python, ruby or shell code.

  * Name of this file must be same as command. For example if we got `Ybot ping` command, we must have plugin ping.py or ping.rb or ping.shell

  * Plugin can consist any code and write result to stdout in the end.

Example of treatment to Ybot:

```
Ybot math 3 ^ 2
```

Here ybot call plugin with math.some_ext with argument: '3 ^ 2'

Current plugins
================

  * chuck - Chuck Norris quotes :)
  * decide - Ybot try to help decide you.
  * github_status - Github status state.
  * help - Ybot help.
  * ping - Ybot simple ping/pong.
  * math - Ybot calculate math expressions.
  * date - Ybot show date/time.
  * pugme - Ybot pugme service plugin.
  * erl   - Ybot computation of erlang expression with tryerlang.org.
  * today? - Ybot return current day.
  * shorten_url - Ybot url shortener with goo.gl.
  * hacker_news - Ybot download news from https://news.ycombinator.com/
  * wat - Ybot random WAT image
  * check-site - Ybot check site up/down state
  * ruby - Ybot eval simple ruby expression
  * ip - Ybot external ip
  * hacker_help - Ybot search in stackoverflow.

Contribute
============

Ybot - is open source project under Erlang public license (see LICENSE file). Issues, questions and patches are welcome.

If you're hacking Ybot core, please, before pull request, pull and merge Ybot master, for avoiding further conflicts.

  * Fork main ybot repository (https://github.com/0xAX/Ybot).
  * Make your changes in your clone of ybot.
  * Test it.
  * Send pull request.

Author
========

[@0xAX](https://twitter.com/0xAX)
