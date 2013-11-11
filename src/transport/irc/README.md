Ybot irc transport
========================

This is `irc` transport for [Ybot](https://github.com/0xAX/Ybot) chat robot.

Usage
=========

For using Ybot with irc, you must to configure it in configuration file ybot.config, something like that:

```erlang
[
    {ybot,
        [
            % list of transport
            {transports, [
                    % Irc transport
                    {irc, 
                          % Irc nick
                          <<"ybot">>,
                          % Irc channels list with keys
                          [{<<"#linknet">>, <<>>}, {<<"#help">>, <<>>}],
                          % Irc server host / pass
                          {<<"irc.freenode.net">>, <<>>},
                          % Options
                          [
                            % Port number
                            {port, 6667},
                            % Use ssl connection or not 
                            {use_ssl, false},
                            % Reconnect timeout. Put 0 if you no need it.
                            {reconnect_timeout, 5000}
                          ]
                    }
                ]
            },
            
            % Loading new plugins during work or not
            {checking_new_plugins, false},
            % Checking new plugins timeout
            {checking_new_plugins_timeout, 60000},
            
            % Save commands history
            {commands_history, false},
            % Command history limit
            {history_command_limit_count, 1000},
 
            % plugins directory path
            {plugins_path, "plugins/"}
        ]
    }
].
```

Author
========================

If you have any questions/suggestions, write me at twitter to [0xAX](https://twitter.com/0xAX)