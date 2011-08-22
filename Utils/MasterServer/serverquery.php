<?
include("serverlib.php");
if(!CheckVersion($_REQUEST["rev"]))
{
	die("Please update");
}
echo GetServers();
?>
