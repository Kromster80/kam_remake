<?php
$format = "";
if(isset($_REQUEST["format"])) $format = $_REQUEST["format"];
include("serverlib.php");

echo GetStats($format);
echo '<BR><BR>
Total player-time:
<BR>';
echo GetTime($format);
?>
