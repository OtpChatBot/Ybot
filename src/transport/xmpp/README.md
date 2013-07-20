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
                     % Hipchat config
                      {hipchat,
                             % Bot Hipchat Username
                             <<"00000_00000@chat.hipchat.com">>,
                             % Bot Hipchat password
                             <<"password">>,
                             % Hipchat room
                             <<"ybot_test@conf.hipchat.com">>,
                             % Server
                             <<"chat.hipchat.com">>,
                             % Resource
                             <<"bot">>,
                             % Hipchat nick
                             <<"ybot">>,
                             % Options
                             [
                                 {reconnect_timeout, 0}
                             ]
                    },
                ]
            },

            .....
            .....
            .....
        ]
    }
].
```
Little note about `hipchat_nick`. You must get it in the `xmpp settings` in your HipChat account page.

Author
========================

If you have any questions/suggestions, write me at twitter to [0xAX](https://twitter.com/0xAX)