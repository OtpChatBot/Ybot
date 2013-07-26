var app = angular.module('ybot', []);

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