<?php
include_once("serverstats.php");
include_once("servertime.php");

function ServerStatsAndTime($format)
{
    ServerStats($format);
    echo '<BR><BR>
Total player-time:
<BR>';
    ServerTime($format);
}

//If we were called directly
if ( basename(__FILE__) == basename($_SERVER["SCRIPT_FILENAME"]) ) {
    $format = "";
    if(isset($_REQUEST["format"])) $format = $_REQUEST["format"];
    ServerStatsAndTime($format);
}
?>
