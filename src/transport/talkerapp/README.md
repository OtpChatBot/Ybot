Ybot Talkerapp transport
========================

This is [talkerapp](http://talkerapp.com/) transport for [Ybot](https://github.com/0xAX/Ybot) chat robot.

Usage
=========

For using Ybot with `talkerapp`, you must to configure it in configuration file ybot.config, something like that:

```erlang
[
    {ybot,
        [
            % list of transport
            {transports, [

                    % talkerapp
                    {talkerapp,
                        % talkerapp nick
                        <<"ybot">>,
                        % room
                        <<"ybot_test">>,
                        % token
                        <<"api_access_token">>
                    }
            
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