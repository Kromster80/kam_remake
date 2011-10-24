<?php
if ($_REQUEST["format"] == "ajaxupdate") {
/*
* the requester expects to receive a json object
* some browsers withdraw the standard text/html that php returns by default
*/
header('Cache-Control: no-cache, must-revalidate');
header('Expires: Mon, 26 Jul 1997 05:00:00 GMT'); //expired in the past to prevent caching
header('Content-type: application/json');
}
include("serverlib.php");
if((!CheckVersion($_REQUEST["rev"])) && (!isset($_REQUEST["format"])))
{
	die("Please update");
}
if(isset($_REQUEST["format"]))
{
	$Format = $_REQUEST["format"];
}
else
{
	$Format = "";
}
echo GetServers($Format);
?>
