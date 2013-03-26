var app = angular.module('ybot', []);

/*
 * This is main angular controller for Ybot web interface
 */
function YbotController ($scope, $http) {
    var req_url = window.location.pathname + 'admin';

    // click on ybot plugins side bar
    $scope.acive_ybot_plugins = function(){
        // setup header
        $scope.header = $('a_ybot_plugins').innerHTML;
        // active current li
        activate_li('li_ybot_plugins', 'div_ybot_plugins');
        // return
        return true;
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
        // return
        return true;
    }

    // click on new transport side bar
    $scope.active_new_transport = function(){
        // setup header
        $scope.header = $('a_ybot_new_transport').innerHTML;
        // active current li
        activate_li('li_start_new_transport', 'div_ybot_runned_transports');
        // return
        return true;
    }

    // click on storage options side bar
    $scope.active_ybot_storage = function(){
        // setup header
        $scope.header = $('a_ybot_storage_opts').innerHTML;
        // active current li
        activate_li('li_storage_option', 'div_ybot_storage');
        // return
        return true;
    }
    
    /*
     * Handle main page
     */
    // Send request for front page
    $http.get(req_url + "?req=main_web_interface_req").success(function (data) {
    });

    // put main header in content div
    $scope.header = 'Ybot web interface';
    // activate li
    activate_li('li_web_interface', 'div_ybot_web_interface');
};

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

