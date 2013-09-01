<?
include_once("dbauth.php");

function db_connect() {
	global $HOST, $USER, $PASS, $DB;
	return new mysqli($HOST, $USER, $PASS, $DB);
}

function db_init($con) {
	$queryCreateTable = "CREATE TABLE IF NOT EXISTS Servers (
		IP VARCHAR(64) NOT NULL,
		Port INT(6) unsigned NOT NULL,
		Name VARCHAR(64) NOT NULL,
		Players INT(4) NOT NULL,
		Pingable INT(1) NOT NULL,
		Dedicated TINYINT(1) NOT NULL,
		OS VARCHAR(64) NOT NULL,
		Rev VARCHAR(8) NOT NULL,
		CodeRev VARCHAR(8) NOT NULL,
		Expiry DATETIME NOT NULL,
		CONSTRAINT PK_ID PRIMARY KEY (IP,Port)
	)";

	if(!$con->query($queryCreateTable))
		die("Table creation failed ".mysqli_error($con));
	
	$queryCreateTable = "CREATE TABLE IF NOT EXISTS Stats (
		Rev VARCHAR(8) NOT NULL,
		Timestamp DATETIME NOT NULL,
		Players INT(6) unsigned NOT NULL,
		Servers INT(6) unsigned NOT NULL,
		PRIMARY KEY (Timestamp)
	)";

	if(!$con->query($queryCreateTable))
		die("Table creation failed ".mysqli_error($con));
	
	$queryCreateTable = "CREATE TABLE IF NOT EXISTS Games (
		Rev VARCHAR(8) NOT NULL,
		Timestamp DATETIME NOT NULL,
		Map VARCHAR(64) NOT NULL,
		Players INT(6) unsigned NOT NULL,
		PRIMARY KEY (Timestamp)
	)";

	if(!$con->query($queryCreateTable))
		die("Table creation failed ".mysqli_error($con));
	
	$queryCreateTable = "CREATE TABLE IF NOT EXISTS PlayerTime (
		Rev VARCHAR(8) NOT NULL,
		PlayerMinutes INT(16) unsigned NOT NULL,
		PRIMARY KEY (Rev)
	)";

	if(!$con->query($queryCreateTable))
		die("Table creation failed ".mysqli_error($con));
}

?>