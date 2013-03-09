Ybot http transport
====================

This is `http` transport for [Ybot](https://github.com/0xAX/Ybot) chat robot.

Usage
=========

```erlang
[
    {ybot,
        [
            % list of transport
            {transports, [    
                    % Http Ybot interface
                    {http,
                        % Http server host 
                        <<"localhost">>, 
                        % Http server port
                        8080,
                        % Ybot nick
                        <<"Ybot">>
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

At that moment you can communicate with Ybot via http with two ways:

  1. Send simple http post request with command in body:

  > curl -X POST -d "Ybot date" http://localhost:8080

  1. Send http post request with application/json header and json body:

```javascript
{
    "type" : "command_type()", 
    "content": "Ybot haker_news 5"
}
```

Where `command_type()`:

  * `broadcast` - Message from `content` field will be sending in all Ybot connected chats.
  * `response` -  Ybot parse `contet` field, execute message, execute plugin, and send result back.

Author
========================

If you have any questions/suggestions, write me at twitter to [0xAX](https://twitter.com/0xAX)