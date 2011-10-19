<?php
include("serverlib.php");

global $DO_STATS;

if ($DO_STATS && $_REQUEST["format"] == "fancy") echo GetFancyStats($_REQUEST["since"], $_REQUEST["to"]);
else echo GetStats($_REQUEST["format"]);
?>
