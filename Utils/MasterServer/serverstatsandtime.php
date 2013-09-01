<?php
include_once("serverlib.php");
include_once("consts.php");
global $BASE_URL;
$format = "";
if(isset($_REQUEST["format"])) $format = $_REQUEST["format"];

echo file_get_contents($BASE_URL.'serverstats.php?format='.$format);
echo '<BR><BR>
Total player-time:
<BR>';
echo file_get_contents($BASE_URL.'servertime.php?format='.$format);
?>
