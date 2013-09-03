<?
include_once("serverlib.php");
include_once("consts.php");
include_once("db.php");
if(!CheckVersion($_REQUEST["rev"])) {
	die("Invalid revision");
}

$con = db_connect();

$Map = $con->real_escape_string($_REQUEST["map"]);
$PlayerCount = $con->real_escape_string($_REQUEST["playercount"]);
$Rev = $con->real_escape_string($_REQUEST["rev"]);
if(($Map == "") || ($Rev == "") || ($PlayerCount == "")) {
	$con->close();
	die("Incorrect parameters");
}

db_init($con);
$Now = date("Y-m-d H:i:s");
$query = "INSERT INTO Games (Rev, Timestamp, Map, Players) VALUES ('$Rev', '$Now', '$Map', $PlayerCount)";
if(!$con->query($query)) error_log("Error adding game: ".mysqli_error($con));
$con->close();
die('success');
?>