%%%-----------------------------------------------------------------------------
%%% @author 0xAX <anotherworldofworld@gmail.com>
%%% @doc
%%% Xml util functions.
%%% @end
%%%-----------------------------------------------------------------------------
-module(xmpp_xml).

%% API
-export([auth_plain/1, 
         create_session/0, 
         bind/1, 
         muc/1, 
         message/2,
         private_message/2,
         presence/0,
         presence_subscribed/1]).

-include("xmpp.hrl").

%% @doc send online status to all
presence() ->
    % xml data structure
    XmlData = [{'presence', [{'priority', '50'}, {'show', 'chat'}, {'status', 'Hello, from Ybot'}], []}],
    % convert to xml
    lists:flatten(xmerl:export_simple(XmlData, xmerl_xml, [{prolog, ""}])).

%% @doc send 'subscribed' presence
-spec presence_subscribed(To :: string()) -> string().
presence_subscribed(To) ->
    % xml data structure
    XmlData = [{'presence', [{'to', To}, {'type', 'subscribed'}], []}],
    % convert to xml
    lists:flatten(xmerl:export_simple(XmlData, xmerl_xml, [{prolog, ""}])).

%% @doc xmpp plain authentication
-spec auth_plain(Login :: string()) -> string().
auth_plain(Login) ->
    % xml structure data
    XmlData = [{'auth', [{'xmlns', 'urn:ietf:params:xml:ns:xmpp-sasl'},
                         {'mechanism', 'PLAIN'}
                        ], [Login]
               }
              ],
    % convert to xml
    lists:flatten(xmerl:export_simple(XmlData, xmerl_xml, [{prolog, ""}])).

%% @doc bind xmpp resource
-spec bind(Resource :: string()) -> string().
bind(Resource) ->
    % xml structure data
    XmlData = [{'iq', [{'type', 'set'}, {'id', '9746'}],
                    [{'bind',
                        [{'xmlns', 'urn:ietf:params:xml:ns:xmpp-bind'}],
                            [{'resource', [], [Resource]}]
                    }]
              }],
    % convert to xml
    lists:flatten([xmerl:export_simple(XmlData, xmerl_xml, [{prolog, ""}])]).

%% @doc create new session
-spec create_session() -> string().
create_session() ->
    % xml structure data
    XmlData = [{'iq', [{'type', 'set'}, {'id', '9746'}], 
                [{'session', 
                    [{'xmlns', 'urn:ietf:params:xml:ns:xmpp-session'}], []
                }]
              }],
    % convert to xml
    lists:flatten([xmerl:export_simple(XmlData, xmerl_xml, [{prolog, ""}])]).

%% @doc send private message
-spec private_message(To :: string(), Message :: string()) -> string().
private_message(To, Message) ->
  % xml data structure
  XmlData = [{'message', [{'to', To},  {'type', "chat"}],
              [{'body', [Message]}]
            }],
  
  % convert to xml
  lists:flatten(xmerl:export_simple(XmlData, xmerl_xml, [{prolog, ""}])).

%% @doc Send message to muc
-spec message(Room :: string(), Message :: string()) -> string().
message(Room, Message) ->
    % xml structure data
    XmlData = [{'message', [{'to', Room}, {'type', "groupchat"}],
                [{'body', [Message]}]
              }],
    % convert to xml
    lists:flatten(xmerl:export_simple(XmlData, xmerl_xml, [{prolog, ""}])).

%% @doc Join to muc
-spec muc(Room :: string()) -> string().
muc(Room) ->
    % xml structure data
    XmlData = [{'presence', [{'to', Room}],
                [{'x', [{'xmlns', 'http://jabber.org/protocol/muc'}],
                    [{'history', [{'maxchars', "0"}], [""]}]
                }]
            }
           ],
    % convert to xml
    lists:flatten(xmerl:export_simple(XmlData, xmerl_xml, [{prolog, ""}])).