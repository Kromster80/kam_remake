<?php
include("serverlib.php");
if(!CheckVersion($_REQUEST["rev"]))
{
	die("Invalid revision");
}

$Name = $_REQUEST["name"];
$IP = $_SERVER['REMOTE_ADDR']; //Use the server's IP as it appears to us
$Port = $_REQUEST["port"];
$PlayerCount = $_REQUEST["playercount"];
$TTL = $_REQUEST["ttl"];
$Rev = $_REQUEST["rev"];
if(($Name == "") || ($IP == "") || ($Port == "") || ($TTL == "") || ($PlayerCount == ""))
{
	die("Incorrect parameters");
}
echo AddServer($Name,$IP,$Port,$PlayerCount,$TTL,$Rev);
?>
