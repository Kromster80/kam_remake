<?
include_once("serverlib.php");
include_once("consts.php");
include_once("db.php");
$format = "";
if(isset($_REQUEST["format"])) $format = $_REQUEST["format"];
if ($format == "ajaxupdate") {
	/*
	* the requester expects to receive a json object
	* some browsers withdraw the standard text/html that php returns by default
	*/
	header('Cache-Control: no-cache, must-revalidate');
	header('Expires: Mon, 26 Jul 1997 05:00:00 GMT'); //expired in the past to prevent caching
	header('Content-type: application/json');
}

$con = db_connect();
$data = $con->query("SELECT SUM(PlayerMinutes) as PlayerMinuteSum FROM PlayerTime");
if(!$data) error_log("Error getting time: ".mysqli_error($con));
$data = $data->fetch_array();
$PlayerMinutes = $data["PlayerMinuteSum"];
$con->close();

global $TIME_REFRESH, $BASE_URL;
$result = "";
$hours = floor($PlayerMinutes/60)%24;
$days = floor($PlayerMinutes/(60*24))%365;
$years = floor($PlayerMinutes/(60*24*365));
switch($Format) {
	case "minutes": $result = $PlayerMinutes; break;
	case "ajaxupdate":
		$data = json_encode(Array("yr"=>$years,"dy"=>$days,"hr"=>$hours));
		$result = $_GET['jsonp_callback']."(".$data.")";
	break;
	default:
		$result = "<span id=\"years\">$years</span> ".plural($years,"year").", ";
		$result .= "<span id=\"days\">$days</span> ".plural($days,"day").", ";
		$result .= "<span id=\"hours\">$hours</span> ".plural($hours,"hour")."";
		$startscript = '<script type="text/javascript">'."\n".
		'function uppt(){setTimeout(function (){jQuery.ajax({dataType: "jsonp",jsonp: "jsonp_callback",url: "'.$BASE_URL.'servertime.php?format=ajaxupdate",success: function (data){jQuery("#years").empty().append(data.yr);jQuery("#days").empty().append(data.dy);jQuery("#hours").empty().append(data.hr);uppt();}});}, '.(1000*$TIME_REFRESH).');}'."\n".
		'jQuery(document).ready(function($){uppt();});</script>'."\n";
		$result = $startscript.$result;
	break;
}
echo $result;

?>