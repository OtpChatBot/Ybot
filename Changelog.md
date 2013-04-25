# Ybot news & changelog

## Ybot-0.3.4 -- In development

  * today plugin compatible with Mac OS X date.
  * fixed http application/json data receiving.
  * #62 fixed. Security issue. Command args escaping.

## Ybot-0.3.3 --> released

  * #47. HipChat config moved to separte config from XMPP transport.
  * XMPP 'is_hipchat' option was removed.
  * XMPP 'hipchat_nick' option was removed.
  * #48. Mnesia backend storage added.
  * Erlang/otp application as plugin support added.
  * New core plugin - memory.
  * New config option - brain_storage.
  * New config option - brain_api_host.
  * New config option - brain_api_port.
  * checking_new_plugins, checking_new_plugins_timeout, commands_history and history_command_limit_count aren't required parametes now.
  * New Ybot erlang api, ybot:get_plugins_directory/0, get_runned_transports/0, is_new_plugins_observing/0 and etc... (see ybot.erl)
  * New dependence - jiffy.
  * New configuration option - webadmin_host :: integer()
  * New configuration option - webadmin_port :: binary()
  * Web interface added.
  * mochijson2 removed from dependecies.
  * New internal command 'name?'.
  * 'help' plugin updated.
  * all api from ybot.erl beside start/stop were removed.
  * ybot_shell added for testing Ybot from erlang shell.
  * reconnect_timeout option added to flowdock transport.
  * reconnect_timeout option added to talkerapp transport.
  * Improved skype support.
  * priv/skype.py now sends ping to Ybot every minute in separate thread, script work will end when ping fail..
  * Fixed long message sending for campfire transport.

## Ybot-0.3.2 --> released

  * Fixed #42 issue. Unable to connect to IRC bug.
  * Fixed #44 issue. Fixed internal Ybot commands.
  * #42. If bot nickname already in use, generate new name and try to reconnect.
  * #45 fixed. Timeout error from IRC transport.
  * #43 fixed. Unable to compile using rebar.
  * Help plugin improved.
  * #44 Internal commands tested and fixed.
  * To all plugins added checks arguments.
  * New internal command 'announce' added.
  * New api ybot:act/1 added
  * New api ybot:plugins/0 added
  * All plugins argumets checking added.
  * Fixed campfire image/video posting.
  * Scala plugins support added.
  * Url decode/encode new plugin added.
  * math.rb plugin result output fixed.
  * New core plugin translate.rb added. tranlate text with google translate plugin added.

## Ybot-0.3.1 --> released

  * Added reconnect timeout option and reconnect ability to irc client --> #33.
  * hacker_help plugin fixed --> #35
  * Added reconnect timeout option to xmpp and campfire.
  * Added supporting of HipChat.
  * HTTP transport imrpoved. JSON request support added.
  * HTTP transport bot-nick parameter added to config.
  * ybot_manager get_all_transports_pid api added.
  * Use cowboy web server instead inets httpd for http serving.
  * Added http `PUT` request support. Resend request body from http to all runned transports.
  * echo plugin added.
  * Flowdock support added.
  * Skype support added.
  * Removed http PUT request supporting. Now only POST.
  * New transport - Talkerapp (http://talkerapp.com/rooms).
  * Message parsers moved to ybot_parser module.

## Ybot-0.3 --> released

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
  * Added PASS paraemetr to IRC.
  * Irc private messages support added --> # 24
  * XMPP-muc private message supporting added.
  * XMPP single user chat supporting added.
  * XMPP ssl support added.
  * GTALK support added.
  * New option xmpp port added. 
  * New option xmpp use_ssl added.
  * Transport options validating added.
  * Procfile added for Heroku deploying support.
  * Added experemental message parser to irc handler --> #23

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
