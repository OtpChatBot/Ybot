XMPP based chats
=======================

This is `xmpp` transport for [Ybot](https://github.com/0xAX/Ybot) chat robot.

Jabber
================

For using Ybot with `jabber`, you must to configure it in configuration file ybot.config, something like that:

```erlang
[
    {ybot,
        [
            % list of transport
            {transports, [                    
                    % Xmpp Multi-user chat Login Password Room Host Resource
                    {xmpp, <<"ybot2@jabber.org">>,
                           <<"password">>,
                           <<"ybot_test@conference.jabber.org">>,
                           <<"jabber.org">>,
                           <<"home">>,
                           [{port, 5222}, {use_ssl, false}, {reconnect_timeout, 5000}]
                    }
                ]
            },

            % Loading new plugins during work or not
            {checking_new_plugins, false},
            % Checking new plugins timeout
            {checking_new_plugins_timeout, 20000},

            % Save commands history
            {commands_history, true},
            % Command history limit
            {history_command_limit_count, 100},
            
            % plugins directory path
            {plugins_path, "plugins/"}
        ]
    }
].
```

HipChat
================

For using Ybot with [HipChat](https://www.hipchat.com/), you must to configure it in configuration file ybot.config, something like that:

```erlang
[
    {ybot,
        [
            % list of transport
            {transports, [                    
                    {xmpp, <<"hipchat_nick@chat.hipchat.com">>,
                           <<"password">>,
                           <<"room@conf.hipchat.com">>,
                           <<"chat.hipchat.com">>,
                           <<"bot">>,
                           [{port, 5223}, {use_ssl, true}, {reconnect_timeout, 0}, 
                            % is this hipchat
                            {is_hipchat, true},
                            % Room nickname. Get it in account xmpp settings
                            {hipchat_nick, <<"ybot ybot">>}]
                    }
                ]
            },

            % Loading new plugins during work or not
            {checking_new_plugins, false},
            % Checking new plugins timeout
            {checking_new_plugins_timeout, 20000},

            % Save commands history
            {commands_history, true},
            % Command history limit
            {history_command_limit_count, 100},
            
            % plugins directory path
            {plugins_path, "plugins/"}
        ]
    }
].
```
Little note about `hipchat_nick`. You must get it in the `xmpp settings` in your HipChat account page.

Author
========================

If you have any questions/suggestions, write me at twitter to [0xAX](https://twitter.com/0xAX)