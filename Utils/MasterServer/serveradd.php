<?
include("serverlib.php");
if(!CheckVersion($_REQUEST["rev"]))
{
	die("Please update");
}

$Name = $_REQUEST["name"];
$IP = $_SERVER['REMOTE_ADDR']; //Use the server's IP as it appears to us
$Port = $_REQUEST["port"];
$TTL = $_REQUEST["ttl"];
if(($Name == "") || ($IP == "") || ($Port == "") || ($TTL == ""))
{
	die("Incorrect parameters");
}
echo AddServer($Name,$IP,$Port,$TTL);
?>
