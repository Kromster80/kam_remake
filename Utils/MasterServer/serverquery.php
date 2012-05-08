<?php
$format = "";
if(isset($_REQUEST["format"])) $format = $_REQUEST["format"];
if ($format == "ajaxupdate") {
/*
* the requester expects to receive a json object
* some browsers withdraw the standard text/html that php returns by default
*/
header('Cache-Control: no-cache, must-revalidate');
header('Expires: Mon, 26 Jul 1997 05:00:00 GMT'); //expired in the past to prevent caching
header('Content-type: application/json');
}
$rev = "";
if(isset($_REQUEST["rev"])) $rev = $_REQUEST["rev"];

include("serverlib.php");
if((!isset($_REQUEST["format"])) && (!CheckVersion($rev)))
{
	die("Invalid revision");
}
echo GetServers($format,$rev);
?>
