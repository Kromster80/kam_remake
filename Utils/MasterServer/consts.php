<?php
global $MAIN_VERSION, $MAX_TTL, $STATS_REFRESH, $TABLE_REFRESH, $TIME_REFRESH, $BASE_URL;

//Used as default when a specific revision is not requested
$MAIN_VERSION = 'r5503';

//Maximum TTL (time to live) a server can specify, otherwise an entry could be added that would expire in 100 years
$MAX_TTL = 600; //10 minutes

//Prime numbers are good for refresh times so they don't occur in sync at the same time
$STATS_REFRESH = 17; //seconds
$TABLE_REFRESH = 23; //seconds
$TIME_REFRESH = 307; //seconds

//URL where the master server is located. Used for links to images, etc.
$BASE_URL = "http://kam.hodgman.id.au/"; //Must end in a slash

?>