<?php
	class ado {
		var $conn;	
		var	$host;
		var	$bank;
		var	$user;
		var	$pswrd;
		function db_connect() {
		}
		
		function db_name($bank,$conn) {
		  echo "This method is not disponible in this database.";		
		}
		
		function query($sql) {
		  echo "This method is not disponible in this database.";				
		}

		function to_array($res) {
		  echo "This method is not disponible in this database.";				
		}
		
		function num_rows($res) {
		  echo "This method is not disponible in this database.";				
		}
		
		function num_fields($res) {
		  echo "This method is not disponible in this database.";				
		}

		function affected_rows($res) {
		  echo "This method is not disponible in this database.";				
		}		
		function db_close() {
		  echo "This method is not disponible in this database.";			
		}
	}
?>