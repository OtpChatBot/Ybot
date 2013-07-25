var app = angular.module('ybot', []);

var req_url = window.location.pathname + 'admin';

/*
 * This is main angular controller for Ybot web interface
 */
function YbotController ($scope, $location, $http) {

    $scope.home = function(){
    
        $scope.selection = 'home';
        $scope.header    = 'Ybot configuration'

        //
        // Send request for front page
        //
        $http.get(req_url + "?req=main_web_interface_req").success(function (data) {
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
    }

    $scope.upload_plugin = function(){
        $scope.selection = 'upload_plugin';
    }

    $scope.transport = function(){
        $scope.selection = 'transport';
    }

    $scope.activate_new_transport = function(){
        $scope.activate_transport = 'activate_transport';
    }

    $scope.storage = function(){
        $scope.storage = 'storage';
    }
};