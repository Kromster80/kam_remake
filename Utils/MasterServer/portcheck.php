<?php
if((!isset($_REQUEST["ip"])) || (!isset($_REQUEST["port"])))
{
	die('Incorrect parameters');
}
$fp = fsockopen($_REQUEST["ip"], $_REQUEST["port"], $errnum, $errstr, 10);
if($fp)
{
	fclose($fp);
	echo 'TRUE';
}
else
{
	echo 'FALSE';
}
?>