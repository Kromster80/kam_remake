<?
include("serverlib.php");
$IP = $_SERVER['REMOTE_ADDR']; //Use the server's IP as it appears to us
$Port = $_REQUEST["port"];
$TTL = $_REQUEST["ttl"];
if(($IP == "") || ($Port == "") || ($TTL == ""))
{
	die("Incorrect parameters");
}
echo AddServer($IP,$Port,$TTL);
?>
