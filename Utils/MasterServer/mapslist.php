<?
include_once("serverlib.php");
include_once("consts.php");
include_once("db.php");

function MapsList($rev, $num, $format)
{
    if(!CheckVersion($rev))
    {
        die("Invalid revision");
    }

    $con = db_connect();

    $format = $con->real_escape_string($format);
    $num = $con->real_escape_string($num);
    $rev = $con->real_escape_string($rev);

    $result = "";
    switch($format) {
        default:
            $result .= '<table border="1" width="100%"><tr><td><strong>#</strong></td><td><strong>Map</strong></td><td style="text-align: center"><strong>Games played</strong></td><td style="text-align: center"><strong>Average players per game</strong></td></tr>'."\n";
    }
    if($num != -1) 
        $limit = "LIMIT $num";
    else
        $limit = "";
    $data = $con->query("SELECT Map, COUNT(Map) as MapCount, AVG(Players) as AvgPlayers FROM Games WHERE Rev='$rev' GROUP BY Map ORDER BY MapCount DESC ".$limit);
    if(!$data) error_log("Error getting maps: ".mysqli_error($con));

    $index = 0;
    while($row = $data->fetch_array()) {
        $Map = $row['Map'];
        $MapCount = $row['MapCount'];
        $AvgPlayers = $row['AvgPlayers'];
        $index++;
        switch($format) {
            default:
                $result .= "<tr class=\"no_translate\"><td>$index</td><td style=\"white-space: nowrap;\">$Map</td><td style=\"text-align: center\">$MapCount</td><td style=\"text-align: center\">".number_format($AvgPlayers, 2)."</td></tr>\n";
        }
    }
        
    switch($format) {
        default:
            $result .= "</table>\n";
    }

    echo $result;
}

//If we were called directly
if ( basename(__FILE__) == basename($_SERVER["SCRIPT_FILENAME"]) ) {
    $rev = "";
    if(isset($_REQUEST["rev"])) $rev = $_REQUEST["rev"];
    $num = -1;
    if(isset($_REQUEST["num"])) $num = $_REQUEST["num"];
    $format = "";
    if(isset($_REQUEST["format"])) $format = $_REQUEST["format"];
    MapsList($rev, $num, $format);
}
?>