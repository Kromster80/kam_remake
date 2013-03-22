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
$Coderev = $_REQUEST["coderev"];
$IsDedicated = $_REQUEST["dedicated"];
$OS = $_REQUEST["os"];
if(($Name == "") || ($IP == "") || ($Port == "") || ($TTL == "") || ($PlayerCount == ""))
{
	die("Incorrect parameters");
}

//Don't allow buggy Linux server versions, Windows server (ICS) is ok
if((($Coderev == "r4179") || ($Coderev == "r4185")) && ($_SERVER['HTTP_USER_AGENT'] != "Mozilla/4.0 (compatible; ICS)"))
{
	die("Please download the server update");
}

echo AddServer($Name,$IP,$Port,$PlayerCount,$IsDedicated,$OS,$TTL,$Rev);
?>