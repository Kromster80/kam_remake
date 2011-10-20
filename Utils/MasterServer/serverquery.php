<?php
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
