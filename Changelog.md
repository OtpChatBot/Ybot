# Ybot news & changelog

## Ybot-0.3 --> In development

  * New wat plugin
  * Irc ssl supporting --> #16
  * New irc configuration options: {use_ssl, true | false}
  * New irc configuration options: {port, PortNumber :: integer()}
  * New check-site plugin added
  * hacker_news plugin improved. Added two modes.
  * Join to channels with key to irc transport added.
  * ruby.rb plugin added. Eval simple ruby expression.
  * ip.py plugin added. Return Ybot external ip address.
  * stackoverflow search plugin added.
  * Dynamic loading plugins.
  * Added command history.
  * commands_history configuration parameter added.
  * history_command_limit_count configuration parameter added.
  * Added http transport.
  * Irc private messages support added --> # 24

## Ybot-0.2 --> released

  * Xmpp transport supporting added
  * Campfire transport supporting added
  * New plugins added (chuck, github_status, erl, decide)
  * Ibrowse added to dependencies
  * Reloader added to dependencies
  * Lager added to dependencies
  * Mochijson2 added to dependencies
  * Code cleaned & documentation improved
  * Help plugin fixed --> #9
  * Command args must be string, not list of strings --> #12
  * Added supporting for perl plugins.
  * Added supporting for elixir plugins.
  * Added hacker_news plugin

## Ybot-0.1 --> released

  * Irc transport support
  * 5 core plugins: help, date, today?, math and ping
  * Xmpp experemental support in xmpp-transport-experemental branch
  * Simple command: Ybot hi, Ybot bue and etc...
  * Python plugins supporting
  * Ruby plugins supporting
  * Shell plugin supporting
  * Ybot stales after some time of inactivity -> #2 fixed
  * Documentation improved
  * Code layout fixed