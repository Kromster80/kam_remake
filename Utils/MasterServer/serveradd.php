<?
include("serverlib.php");
if(($_REQUEST["ip"] == "") || ($_REQUEST["port"] == "") || ($_REQUEST["ttl"] == ""))
{
	die("Incorrect parameters");
}
echo AddServer($_REQUEST["ip"],$_REQUEST["port"],$_REQUEST["ttl"]);
?>
