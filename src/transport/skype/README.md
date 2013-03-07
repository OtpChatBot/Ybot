Ybot skype transport
========================

This is [skype](http://www.skype.com/) transport for [Ybot](https://github.com/0xAX/Ybot) chat robot.

Usage
========================

There is `python` script - skype.py which you can find [here](https://github.com/0xAX/Ybot/blob/master/priv/skype.py), 
which uses skype api for works with skype. Also if you want to use Ybot with skype, it depends on [Skype4Py](https://github.com/awahlig/skype4py) library, which you can install with:

```
pip install Skype4Py
```

or download source code and

```
python setup.py install
```

`skype.py` script receives skype messages and sends it to Ybot via http, got response from Ybot and sends it back to skype, all simple.
After Ybot configuring, you must configure your skype. Open `/etc/dbus-1/system.d/skype.conf` and add:

```xml
<!DOCTYPE busconfig PUBLIC "-//freedesktop//DTD D-BUS Bus Configuration 1.0//EN" "http://www.freedesktop.org/standards/dbus/1.0/busconfig.dtd">
<busconfig>
    <policy context="default">
        <allow own="com.Skype.API"/>
        <allow send_destination="com.Skype.API"/>
        <allow receive_sender="com.Skype.API"/>
        <allow send_path="/com/Skype"/>
    </policy>
</busconfig>
```

After skype configuring, you must have run skype in the same machine as Ybot. Now configure Ybot. 
We must run http interface and skype interface in Ybot:

```erlang
[
    {ybot,
        [
            % list of transport
            {transports, [
                    % Irc transport
                    {irc, 
                    
                    % Http Ybot interface
                    {http,
                        % Http server host 
                        <<"localhost">>, 
                        % Http server port
                        8080,
                        % Ybot nick
                        <<"Ybot">>
                    },
                    
                    % skype
                    {skype,
                        % use skype or not
                        true,
                        % Ybot http interface host
                        <<"http://localhost">>,
                        % Ybot http interface port
                        8080
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