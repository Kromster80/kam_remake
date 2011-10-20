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

global $DO_STATS;

if ($DO_STATS && $_REQUEST["format"] == "fancy") echo GetFancyStats($_REQUEST["since"], $_REQUEST["to"]);
else echo GetStats($_REQUEST["format"]);
?>
