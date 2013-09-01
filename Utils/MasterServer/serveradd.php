<?php
include_once("serverlib.php");
include_once("consts.php");
include_once("db.php");
global $MAX_TTL;

$con = db_connect();

//Clean and verify parameters
$Name = $con->real_escape_string($_REQUEST["name"]);
if(!isset($_REQUEST["ip"])) {
	$IP = $con->real_escape_string($_SERVER['REMOTE_ADDR']); //Use the server's IP as it appears to us
} else {
	$IP = $con->real_escape_string($_REQUEST["ip"]);
}
$Port = intval($con->real_escape_string($_REQUEST["port"]));
if(($Port < 1) || ($Port > 65535)) {
	$con->close();
	die("invalid port");
}
$PlayerCount = intval($con->real_escape_string($_REQUEST["playercount"]));
$TTL = max(min(intval($con->real_escape_string($_REQUEST["ttl"])),$MAX_TTL),1);
$Rev = $con->real_escape_string($_REQUEST["rev"]);
if(!CheckVersion($Rev)) {
	$con->close();
	die("Invalid revision");
}
$Coderev = $con->real_escape_string($_REQUEST["coderev"]);
$IsDedicated = intval($con->real_escape_string($_REQUEST["dedicated"]));
$OS = $con->real_escape_string($_REQUEST["os"]);
if(($Name === "") || ($IP === "") || ($Port === "") || ($TTL === "") || ($PlayerCount === "")) {
	error_log("Incorrect parameters ".$Name." ".$IP." ".$Port." ".$TTL." ".$PlayerCount." ");
	$con->close();
	die("Incorrect parameters");
}

//Don't allow buggy Linux server versions, Windows server (ICS) is ok
if((($Coderev == "r4179") || ($Coderev == "r4185")) && ($_SERVER['HTTP_USER_AGENT'] != "Mozilla/4.0 (compatible; ICS)"))
{
	$con->close();
	die("Please download the server update");
}

$Pingable = intval(TestConnection($IP, $Port));

db_init($con);
Remove_Old_Servers($con);
$Expiry = date("Y-m-d H:i:s", time()+$TTL);
$query = "REPLACE INTO Servers (IP, Port, Name, Players, Pingable, Dedicated, OS, Rev, CodeRev, Expiry) VALUES ('$IP', $Port, '$Name', $PlayerCount, $Pingable, $IsDedicated, '$OS', '$Rev', '$Coderev', '$Expiry')";
if(!$con->query($query)) error_log("Error adding server: ".mysqli_error($con));
$con->close();
die('success');
?>