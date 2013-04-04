var app = angular.module('ybot', []);
 
/*
 * This is main angular controller for Ybot web interface
 */
function YbotController ($scope, $http) {
    var req_url = window.location.pathname + 'admin';

    // click on ybot plugins side bar
    $scope.acive_ybot_settings = function(){
        // setup header
        $scope.header = $('a_ybot_plugins').innerHTML;
        // active current li
        activate_li('li_ybot_plugins', 'div_ybot_plugins');
        
        // Send request for getting current settings
        $http.get(req_url + "?req=ybot_plugins_settings").success(function (data) {
            // Observer part
            if (data.is_observer == true)
                $('checkbox_is_observer').checked = true;
            else
                $('checkbox_is_observer').checked = false;

            // History part
            if (data.is_history == true)
                $('checkbox_is_history').checked = true;
            else
                $('checkbox_is_history').checked = false;

            $('input_observer_timeout').value = data.observer_timeout;
            $('input_history_limit').value = data.history_limit;
        });
        // return
        return true;
    }

    // click on update observer button
    $scope.update_observer_settings = function(){
        // get observer timeout value
        var obs_timeout = $('input_observer_timeout').value;
        // get using observer or not value
        var is_observer = $('checkbox_is_observer').checked;
        // check observer timeout
        if (isInt(obs_timeout) == false || obs_timeout <= 0){
            $('input_observer_timeout').value = '';
            $('span_observer_error1').style.visibility = "visible";
            return false;
        }
        else{
            $('span_observer_error1').style.visibility = "hidden";
            // send request update observer settings
            var data = {'is_observer' : is_observer, 'timeout' : obs_timeout};
            $http.post(req_url + '?req=update_observer_settings', data);
            // return
            return true;
        }
    }

    // click on upload plugin button
    $scope.upload_plugin = function(){
        // get plugin path
        var plugin_path = $('plugin_web_path').value;
        // check plugin path
        if (plugin_path == ''){
            // show error
            $('span_upload_error').style.visibility = "visible";
            // return
            return false;
        }
        else{
            // hide error
            $('span_upload_error').style.visibility = "hidden";
            // send request to server on new plugin upload
            data = {'upload_plugin_path' : plugin_path};
            $http.post(req_url + '?req=upload_web_plugin', data);
            // clear plugin path
            $('plugin_web_path').value = ''
            // return
            return true;
        }
    }

    // click on update history button
    $scope.update_history_settings = function(){
        // get history limit value
        var history_limit = $('input_history_limit').value;
        // get using history or not value
        var is_history = $('checkbox_is_history').checked;
        // check history limit
        if (isInt(history_limit) == false || history_limit <= 0){
            $('input_history_limit').value = '';
            $('span_observer_error2').style.visibility = "visible";
            return false;
        }
        else{
            $('span_observer_error2').style.visibility = "hidden";
            // send request to update history settings
            var data = {'is_history' : is_history, 'limit' : history_limit};
            $http.post(req_url + '?req=update_history_settings', data);
            // return
            return true;
        }
    }

    // click on upload plugin side bar
    $scope.active_upload_plugin = function(){
        // setup header
        $scope.header = $('a_ybot_upload_plugin').innerHTML;
        // active current li
        activate_li('li_ybot_upload_plugin', 'div_ybot_upload');
        // return
        return true;
    }

    // click on runned transport side bar
    $scope.active_runned_transport = function(){
        // setup header
        $scope.header = $('a_ybot_runned_transports').innerHTML;
        // active current li
        activate_li('li_runned_transports', 'div_ybot_transports');
        
        $http.get(req_url + "?req=main_web_interface_req").success(function (data) {
            $scope.runned_transports = data.transport.split('\n').splice(0, data.transport.split('\n').length - 1);
        });

        // return
        return true;
    }

    // click on new transport side bar
    $scope.active_new_transport = function(){
        // setup header
        $scope.header = $('a_ybot_new_transport').innerHTML;
        // active current li
        activate_li('li_start_new_transport', 'div_ybot_runned_transports');

        // init tabs
        new Control.Tabs('tabs_example_one'); 
        $$('.tabs').each(function(tab_group){ new Control.Tabs(tab_group); });
    
        // return
        return true;
    }

    // click on storage options side bar
    $scope.active_ybot_storage = function(){
        // setup header
        $scope.header = $('a_ybot_storage_opts').innerHTML;
        // active current li
        activate_li('li_storage_option', 'div_ybot_storage');
        // send request
        $http.get(req_url + '?req=storage_info').success(function(data){
            $scope.storage_http_host = data.storage_host;
            $scope.storage_http_port = data.storage_port;
        });

        // return
        return true;
    }

    /*
     * Start transports
     */
    // start irc
    $scope.start_irc = function(){
        /*
         * Get transport data
         */
        var irc_login = ($('input_irc_nick').value == '');
        var irc_password = $('input_irc_password').value;
        var irc_channel = $('input_irc_channel').value;
        var irc_channel_key = $('input_irc_channel_key').value;
        var irc_server_host = $('input_irc_host').value;
        var irc_server_port = $('input_irc_port').value;
        var irc_use_ssl = $('input_irc_ssl').checked;
        var irc_reconnect_timeout = $('input_irc_reconnect_timeout').value;
        if (irc_login == '' || irc_password == '' || irc_channel_key == '' || irc_server_host == '' || irc_server_host == ""
            || irc_reconnect_timeout == '')
            return false;
        // send request to server
        data = {'transport':'irc','irc_login' : irc_login, 'irc_password':irc_password,'irc_channel': irc_channel, 
                'irc_channel_key' : irc_channel_key, 'irc_server_host':irc_server_host, 'irc_server_port' : irc_server_port,
                'irc_use_ssl' : irc_use_ssl, 'irc_reconnect_timeout' : irc_reconnect_timeout};
        $http.post(req_url + '?req=start_irc', data).success(function(data){
            $('input_irc_nick').value = '';
            $('input_irc_password').value = '';
            $('input_irc_channel').value = '';
            $('input_irc_channel_key').value = '';
            $('input_irc_host').value = '';
            $('input_irc_port').value = '';
            $('input_irc_ssl').checked = false;
            $('input_irc_reconnect_timeout').value = '';
        });
        // return
        return true;
    }

    // start jabber
    $scope.start_xmpp = function(){
        // get data
        var xmpp_login = $('input_jabber_nick').value;
        var xmpp_password = $('input_jabber_password').value;
        var xmpp_room = $('input_jabber_room').value;
        var xmpp_server = $('input_jabber_server').value;
        var xmpp_resource = $('input_jabber_resource').value;
        var xmpp_port = $('input_jabber_port').value;
        var xmpp_ssl = $('input_jabber_ssl').checked;
        var xmpp_reconnect_timeout = $('input_jabber_reconnect_timeout').value;
        if (xmpp_login == '' || xmpp_password == '' | xmpp_room == '' || xmpp_resource == '' || xmpp_port == '' || xmpp_reconnect_timeout == '')
            return false;
        // send request to server
        data = {'transport': 'xmpp', 'xmpp_login' : xmpp_login, 'xmpp_password':xmpp_password,'xmpp_room':xmpp_room,
                'xmpp_server':xmpp_server,'xmpp_resource':xmpp_resource,'xmpp_port':xmpp_port,'xmpp_ssl':xmpp_ssl,
                'xmpp_reconnect_timeout':xmpp_reconnect_timeout};
        $http.post(req_url + '?req=start_xmpp', data).success(function(data){
            $('input_jabber_nick').value = '';
            $('input_jabber_password').value = '';
            $('input_jabber_room').value = '';
            $('input_jabber_server').value = '';
            $('input_jabber_resource').value = '';
            $('input_jabber_port').value = '';
            $('input_jabber_ssl').checked = false;
            $('input_jabber_reconnect_timeout').value;
        });
        // return
        return true;
    }

    // start campfire
    $scope.start_campfire = function(){
        // get data
        var campfire_login = $('input_campfire_nick').value;
        var campfire_token = $('input_campfire_token').value;
        var campfire_room = $('input_campfire_room_id').value;
        var campfire_sub_domain = $('input_campfire_subdomain').value;
        var campfire_reconnect_timeout = $('input_campfire_reconnect_timeout').value;
        if (campfire_login == '' || campfire_token == '' || campfire_sub_domain == '' || campfire_reconnect_timeout == '')
            return false;
        // send request to server
        data = {'transport':'campfire', 'login':campfire_login,'token':campfire_token,'room':campfire_room,
                'subdomain':campfire_sub_domain, 'reconnect_timeout':campfire_reconnect_timeout};
        $http.post(req_url + '?req=start_campfire', data).success(function(data){
            $('input_campfire_nick').value = '';
            $('input_campfire_token').value = '';
            $('input_campfire_room_id').value = '';
            $('input_campfire_subdomain').value = '';
            $('input_campfire_reconnect_timeout').value = '';
        });
        // return
        return true;
    }

    // start hipchat
    $scope.start_hipchat = function () {
        // get data
        var hipchat_jid = $('input_hipchat_jid').value;
        var hipchat_password = $('input_hipchat_password').value;
        var hipchat_room = $('input_hipchat_room').value;
        var hipchat_nick = $('input_hipchat_nick').value;
        var hipchat_reconnect_timeout = $('input_hipchat_reconnect_timeout').value;
        if (hipchat_jid == '' || hipchat_password == '' || hipchat_room == '' || hipchat_nick == '' || hipchat_reconnect_timeout == '')
            return false;
        // send request to server
        data = {'transport':'hipchat','hipchat_jid':hipchat_jid,'hipchat_password':hipchat_password,'hipchat_room':hipchat_room,
                'hipchat_nick':hipchat_nick,'hipchat_reconnect_timeout':hipchat_reconnect_timeout};
        $http.post(req_url + '?req=start_hipchat', data).success(function(data){
            $('input_hipchat_jid').value = '';
            $('input_hipchat_password').value = '';
            $('input_hipchat_room').value = '';
            $('input_hipchat_nick').value = '';
            $('input_hipchat_reconnect_timeout').value = '';
        });
        // return
        return true;
    }

    // start skype
    $scope.start_skype = function(){
        // get data
        var skype_http_host = $('input_skype_http_host').value;
        var skype_http_port = $('input_skype_http_port').value;
        if (skype_http_host == '' || skype_http_port == '')
            return false;
        // send request
        data = {'transport':'skype','skype_http_host':skype_http_host,'skype_http_port':skype_http_port};
        $http.post(req_url + '?req=start_skype', data).success(function(data){
            $('input_skype_http_host').value = '';
            $('input_skype_http_port').value = '';
        });
        // return
        return true;
    }

    // start flowdock
    $scope.start_flowdock = function(){
        // get data
        var flowdock_nick = $('input_flowdock_nick').value;
        var flowdock_login = $('input_flowdock_login').value;
        var flowdock_password = $('input_flowdock_password').value;
        var flowdock_org = $('input_flowdock_org').value;
        var flowdock_flow = $('input_flowdock_flow').value;
        if (flowdock_nick == '' || flowdock_login == '' || flowdock_password == '' || flowdock_org == '' || flowdock_flow == '')
            return false;
        // send request
        data = {'transport':'flowdock','flowdock_nick':flowdock_nick,'flowdock_login':flowdock_login,'flowdock_password':flowdock_password,
                'flowdock_org':flowdock_org,'flowdock_flow':flowdock_flow};
        $http.post(req_url + '?req=start_flowdock', data).success(function(data){
            $('input_flowdock_nick').value = '';
            $('input_flowdock_login').value = '';
            $('input_flowdock_password').value = '';
            $('input_flowdock_org').value = '';
            $('input_flowdock_flow').value = '';
        });
        // return
        return true;
    }

    // start http
    $scope.start_http = function(){
        // get data
        var http_host = $('input_http_host').value;
        var http_port = $('input_http_port').value;
        var http_bot_nick = $('input_http_bot_nick').value;
        if (http_host == '' || http_port == '' || http_bot_nick == '')
            return false;
        // send request
        data = {'transport':'http','http_host':http_host,'http_port':http_port,'http_bot_nick':http_bot_nick};
        $http.post(req_url + '?req=start_http', data).success(function(data){
            $('input_http_host').value = '';
            $('input_http_port').value = '';
            $('input_http_bot_nick').value = ''; 
        });
        // return
        return true;
    }

    // start talkerapp
    $scope.start_talkerapp = function(){
        // get data
        var talkerapp_nick = $('input_talkerapp_nick').value;
        var talkerapp_room = $('input_talkerapp_room').value;
        var talkerapp_token = $('input_talkerapp_token').value;
        if (talkerapp_nick == '' || talkerapp_room == '' || talkerapp_token == '')
            return false;
        // send request
        data = {'transport':'talkerapp','talkerapp_nick':talkerapp_nick,'talkerapp_room':talkerapp_room,'talkerapp_token':talkerapp_token};
        $http.post(req_url + '?req=start_talkerapp', data).success(function(data){
            $('input_talkerapp_nick').value = '';
            $('input_talkerapp_room').value = '';
            $('input_talkerapp_token').value = '';
        });
        // return
        return true;
    }

    /*
     * Handle main page
     */

    // Send request for front page
    $http.get(req_url + "?req=main_web_interface_req").success(function (data) {
        $scope.transports = data.transport.split('\n').splice(0, data.transport.split('\n').length - 1);
        $scope.plugins = data.plugins.split('\n').splice(0, data.plugins.split('\n').length - 1);
        $scope.is_history = data.is_history;
        $scope.history_limit = data.history_limit;
        $scope.is_observer = data.is_observer;
        $scope.observer_timeout = data.observer_timeout;
        $scope.storage_type = data.storage_type;
    });

    // put main header in content div
    $scope.header = 'Ybot web interface';
    // activate li
    activate_li('li_web_interface', 'div_ybot_web_interface');
};

/*
 * Helper functions
 */

function activate_li(li_id, div_id){
    var divs = new Array('div_ybot_web_interface', 'div_ybot_plugins', 'div_ybot_upload', 
                         'div_ybot_transports', 'div_ybot_runned_transports', 'div_ybot_storage');
    // hide all divs
    divs.each(function(div){
        $(div).style.visibility = "hidden";
    });
    // activate li with id == li_id
    $$('li').each(function(li){     
        if (li.className == "active")
            li.className = "";

        if (li.id == li_id)
            $(li_id).className = "active";
    });

    // show main content
    $(div_id).style.visibility = "visible";
}

function isInt(n) {
    if (Math.floor(n) == n)
        return true;
    else
        return false;
}