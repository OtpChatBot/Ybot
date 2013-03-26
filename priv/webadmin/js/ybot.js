var app = angular.module('ybot', []);

/*
 * This is main angular controller for Ybot web interface
 */
function YbotController ($scope) {
	$scope.start = function(){
		$scope.header = "Ybot web interface";
	}

	// click on ybot configuration side bar
	$scope.active_ybot_configuration = function(){
		// setup header
		$scope.header = $('a_ybot_configuration').innerHTML;
		// active current li
		activate_li('li_ybot_configuration');
		// return
		return true;
	}

	// click on ybot plugins side bar
	$scope.acive_ybot_plugins = function(){
		// setup header
		$scope.header = $('a_ybot_plugins').innerHTML;
		// active current li
		activate_li('li_ybot_plugins');
		// return
		return true;
	}

	// click on upload plugin side bar
	$scope.active_upload_plugin = function(){
		// setup header
		$scope.header = $('a_ybot_upload_plugin').innerHTML;
		// active current li
		activate_li('li_ybot_upload_plugin');
		// return
		return true;
	}

	// click on runned transport side bar
	$scope.active_runned_transport = function(){
		// setup header
		$scope.header = $('a_ybot_runned_transports').innerHTML;
		// active current li
		activate_li('li_runned_transports');
		// return
		return true;
	}

	// click on new transport side bar
	$scope.active_new_transport = function(){
		// setup header
		$scope.header = $('a_ybot_new_transport').innerHTML;
		// active current li
		activate_li('li_start_new_transport');
		// return
		return true;
	}

	// click on storage options side bar
	$scope.active_ybot_storage = function(){
		// setup header
		$scope.header = $('a_ybot_storage_opts').innerHTML;
		// active current li
		activate_li('li_storage_option');
		// return
		return true;
	}
	
	/*
	 * Handle main page
	 */
	$scope.header = "Ybot web interface";

};

function activate_li(li_id){
	$$('li').each(function(li){		
		if (li.className == "active")
			li.className = "";

		if (li.id == li_id)
			$(li_id).className = "active";
	});
}

