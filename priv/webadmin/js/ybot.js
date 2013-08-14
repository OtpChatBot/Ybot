var app = angular.module('ybot', ['ui.bootstrap']);

var req_url = window.location.pathname + 'admin';

/*
 * This is main angular controller for Ybot web interface
 */
function YbotController ($scope, $location, $http) {

    $scope.home = function(){
        $scope.selection = 'home';
        $scope.header    = 'Ybot configuration'

        var data = {'method' : 'get_start_page', 'params' : []};

        //
        // Send request for front page
        //
        $http({'method' : 'POST', 'url' : '/admin', 'data' : data}).success(function (data) {
            $scope.transports = data.transport.split('\n').splice(0, data.transport.split('\n').length - 1);
            $scope.plugins = data.plugins.split('\n').splice(0, data.plugins.split('\n').length - 1);
            $scope.is_history = data.is_history;
            $scope.history_limit = data.history_limit;
            $scope.is_observer = data.is_observer;
            $scope.observer_timeout = data.observer_timeout;
            $scope.storage_type = data.storage_type;
        });
    }

    $scope.plugnis_obs_history = function(){
        $scope.selection = 'observer_and_history';
        
        //
        // Update history settings
        //
        $scope.update_history_settings = function(historyTimeout){
            var is_history = document.getElementById("history_checked").checked;
            // request body
            var data = {'method' : 'update_history', 'params' : {'timeout' : historyTimeout, 'is_history' : is_history}};
            //
            // Send request for updating history settings
            //
            $http({'method' : 'POST', 'url' : '/admin', 'data' : data}).success(function (data) {});
        }

        //
        // Update observer settings
        //
        $scope.update_observer_settings = function(observerTimeout){
            var is_observer = document.getElementById("observer_checked").checked;
            // request body
            var data = {'method' : 'update_observer', 'params' : {'timeout' : observerTimeout, 'is_observer' : is_observer}};
            // 
            // Send request for updating observer settings
            //
            $http({'method' : 'POST', 'url' : '/admin', 'data' : data}).success(function (data) {});
        }
    }

    $scope.upload_plugin = function(){
        $scope.selection = 'upload_plugin';

        //
        // upload new plugin
        //
        $scope.upload_new_plugin = function(newPluginPath){
            var data = {'method' : 'upload_plugin', 'params' : {'upload_plugin_path' : newPluginPath}}
            $http({'method' : 'POST', 'url' : '/admin', 'data' : data}).success(function (data) {});        
        }
    }

    $scope.transport = function(){
        $scope.selection = 'transport';

        var data = {'method' : 'get_runned_transports', 'params' : []};
        $http({'method' : 'POST', 'url' : '/admin', 'data' : data}).success(function (data) {
            $scope.runned_transports = data.transport.split('\n').splice(0, data.transport.split('\n').length - 1);
        });
    }

    $scope.activate_new_transport = function(){
        $scope.selection = 'activate_transport';

        $scope.start_irc = function(nick, password, channel, channel_key, host, port, ssl, timeout){
            var data = {'method' : 'start_irc', 'params' : {'irc_login' : nick, 'irc_password': password,
                                                          'irc_channel': channel, 'irc_channel_key' : channel_key, 
                                                          'irc_server_host': host, 'irc_server_port' : port,
                                                          'irc_use_ssl' : ssl, 'irc_reconnect_timeout' : timeout}};
            $http({'method' : 'POST', 'url' : '/admin', 'data' : data}).success(function (data) {});
        }

        $scope.start_xmpp = function(jabber_nick, jabber_password, jabber_room, jabber_server, jabber_resource, jabber_port,
                                     jabber_ssl, jabber_reconnect_timeout){
            var data = {'method' : 'start_xmpp', params : {'xmpp_login' : jabber_nick, 'xmpp_password': jabber_password,
                                                           'xmpp_room':jabber_room, 'xmpp_server': jabber_server,
                                                           'xmpp_resource':jabber_resource, 'xmpp_port':jabber_port,
                                                           'xmpp_ssl':jabber_ssl, 'xmpp_reconnect_timeout':jabber_reconnect_timeout}};
            $http({'method' : 'POST', 'url' : '/admin', 'data' : data}).success(function (data) {});
        }
        
        $scope.start_campfire = function(campfire_nick, campfire_token, campfire_room_id, campfire_subdomain, campfire_reconnect_timeout){
            var data = {'method' : 'start_campfire', params : {'login':campfire_nick, 'token':campfire_token,
                                                               'room':campfire_room_id, 'subdomain':campfire_subdomain, 
                                                               'reconnect_timeout': campfire_reconnect_timeout}};
            $http({'method' : 'POST', 'url' : '/admin', 'data' : data}).success(function (data) {});
        }
        
        $scope.start_hipchat = function(hipchat_jid, hipchat_password, hipchat_room, hipchat_nick, hipchat_reconnect_timeout){
            var data = {'method' : 'start_hipchat', params : {'hipchat_jid' : hipchat_jid, 'hipchat_password' : hipchat_password,
                                                              'hipchat_room' : hipchat_room, 'hipchat_nick':hipchat_nick,
                                                              'hipchat_reconnect_timeout' : hipchat_reconnect_timeout}};
            $http({'method' : 'POST', 'url' : '/admin', 'data' : data}).success(function (data) {});
        }
        
        $scope.start_skype = function(skype_http_host, skype_http_port){
            var data = {'method' : 'start_skype', params : {'skype_http_host' : skype_http_host, 'skype_http_port' : skype_http_port}};
            $http({'method' : 'POST', 'url' : '/admin', 'data' : data}).success(function (data) {});
        }
        
        $scope.start_http = function(http_host, http_port, http_bot_nick){
            var data = {'method' : 'start_http', params : {'http_host':http_host,'http_port':http_port,'http_bot_nick':http_bot_nick}};
            $http({'method' : 'POST', 'url' : '/admin', 'data' : data}).success(function (data) {});
        }
        
        $scope.start_flowdock = function(flowdock_nick, flowdock_login, flowdock_password, flowdock_org, flowdock_flow){
            var data = {'method' : 'start_flowdock', params : {'flowdock_nick':flowdock_nick,'flowdock_login':flowdock_login,
                                                               'flowdock_password':flowdock_password, 'flowdock_org':flowdock_org,
                                                               'flowdock_flow':flowdock_flow}};
            $http({'method' : 'POST', 'url' : '/admin', 'data' : data}).success(function (data) {});
        }
        
        $scope.start_talkerapp = function(talkerapp_nick, talkerapp_room, talkerapp_token){
            var data = {'method' : 'start_talkerapp', params : {'talkerapp_nick' : talkerapp_nick,'talkerapp_room': talkerapp_room,
                                                                'talkerapp_token':talkerapp_token}};
            $http({'method' : 'POST', 'url' : '/admin', 'data' : data}).success(function (data) {});
        }
    }

    $scope.storage = function(){
        $scope.selection = 'storage';

        var data = {'method' : 'get_storage_info', params : []};
        $http({'method' : 'POST', 'url' : '/admin', 'data' : data}).success(function (data) {
            $scope.storage_http_host = data.storage_host;
            $scope.storage_http_port = data.storage_port;
        });

    }
};