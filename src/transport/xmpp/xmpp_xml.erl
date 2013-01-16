%%%----------------------------------------------------------------------
%%% File    : transport/xmpp/xmpp_xml.erl
%%% Author  : 0xAX <anotherworldofworld@gmail.com>
%%% Purpose : Xml util functions
%%%----------------------------------------------------------------------
-module(xmpp_xml).

%% API
-export([auth_plain/1, 
         create_session/0, 
         bind/1, 
         muc/1, 
         message/3]).

-include("xmpp.hrl").

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
    XmlData = [{'iq', [{'type', 'set'}, {'id', '10000'}], 
                [{'session', 
                    [{'xmlns', 'urn:ietf:params:xml:ns:xmpp-session'}], []
                }]
              }],
    % convert to xml
    lists:flatten([xmerl:export_simple(XmlData, xmerl_xml, [{prolog, ""}])]).

%% @doc Send message to muc
-spec message(From :: string(), Room :: string(), Message :: string()) -> string().
message(From, Room, Message) ->
    % xml structure data
    XmlData = [{'message', [{'to', Room}, {'from', From}, {'type', "groupchat"}],
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