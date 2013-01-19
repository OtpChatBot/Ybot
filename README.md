Ybot
===============

Ybot - is a customizable bot software which inspired with Github hubot. Github hubot is realy cool, but sorry, i don't like javascript and i don't know coffescript. Ybot written fully with erlang/otp and you can write plugins in other different scripting language like python, ruby or shell. 

[![Build Status](https://travis-ci.org/0xAX/Ybot.png)](https://travis-ci.org/0xAX/Ybot)

Features
=========

  * Fully written with erlang.
  * Supporting plugins in different technology. 
  * Now supporting:
    * Python plugins
    * Ruby plugins
    * Shell plugins
  * To write a plugin does not need to know erlang
  * Very easily extensible
  * Irc supporting
  * XMPP supporting
  * Campfire supporting

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

Ybot can execute different commands. Command - is simple chat message. For example chat session:

```
you: Ybot math 1 + 5
Ybot: Answer: 6
```

Here are a few simple rules, of structure of the all Ybot plugins.

1. Every message to Ybot must started from `Ybot` chat login.

2. After `Ybot` login is command for example `math` or `ping`. Every command consists from one word.

3. After `Ybot` login and command are command arguments.

4. One command = One plugin. We must have plugins in our plugin directory.

5. Plugin is file necessarily with extension of:

  * .py
  * .rb
  * .shell

with python, ruby or shell code.

6. Name of this file must be same as command. For example if we got `Ybot ping` command, we must have plugin ping.py or ping.rb or ping.shell

7. Plugin can consist any code and write result to stdout in the end.

Example of treatment to Ybot:

```
Ybot math 3^2
```

Current plugins
================

  * Ping - simple ping/pong
  * Math - calculate math expressions
  * Date - Date/time
  * Pugme - pugme service plugin
  * Erl   - computation of erlang expression with tryerlang.org.


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