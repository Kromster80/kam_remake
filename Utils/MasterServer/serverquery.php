<?php
include_once("serverlib.php");
$con = db_connect();
$format = "";
if(isset($_REQUEST["format"])) $format = $con->real_escape_string($_REQUEST["format"]);
if ($format == "ajaxupdate") {
	/*
	* the requester expects to receive a json object
	* some browsers withdraw the standard text/html that php returns by default
	*/
	header('Cache-Control: no-cache, must-revalidate');
	header('Expires: Mon, 26 Jul 1997 05:00:00 GMT'); //expired in the past to prevent caching
	header('Content-type: application/json');
}
include_once("flag.php");
include_once("consts.php");
include_once("db.php");

global $MAIN_VERSION, $BASE_URL;

$rev = "";
if(isset($_REQUEST["rev"]))	$rev = $con->real_escape_string($_REQUEST["rev"]);
if((!isset($_REQUEST["format"])) && (!CheckVersion($rev))) {
	$con->close();
	die("Invalid revision");
}
if($rev == "") $rev = $MAIN_VERSION;

$Result = "";
$server_count = 0;
/////////////////////////////////////////
//              HEADER                 //
/////////////////////////////////////////
switch($format) {
	case "ajaxupdate":
		$Result = array();
	break;
	case "refresh":
		$Result = '<script type="text/javascript">'."\n".
		'function srvlsttim(dat){var x="<tr><td><strong>Name</strong></td><td><strong>Address</strong></td><td style=\"text-align: center\"><strong>Players</strong></td></tr>";for(var n=0;n<dat.cnt;n++){x+="<tr><td><img src=\"'.$BASE_URL.'flags/"+dat.srvs[n].c+".gif\" alt=\""+dat.srvs[n].c+"\" />&nbsp;"+dat.srvs[n].n+"</td><td>"+(dat.srvs[n].a=="0"?" <img src=\"'.$BASE_URL.'error.png\" alt=\"Server unreachable\" style=\"vertical-align:middle\" />":"")+dat.srvs[n].i+"</td><td style=\"text-align: center\">"+dat.srvs[n].p+"</td></tr>";jQuery("#ajxtbl").empty().append(x);}}'."\n".
		'function updsr(){setTimeout(function (){jQuery.ajax({dataType: "jsonp",jsonp: "jsonp_callback",url: "'.$BASE_URL.'serverquery.php?format=ajaxupdate&rev='.$rev.'",success: function (data){srvlsttim(data);updsr();}});}, 35000);}'."\n".
		'jQuery(document).ready(function($){updsr();});</script>'."\n";
	case "table":
		$Result .= '<table border="1" width="100%" id="ajxtbl"><tr><td><strong>Name</strong></td><td><strong>Address</strong></td><td style="text-align: center"><strong>Players</strong></td></tr>';
	break;
	case "kamclub":
		$Result .= '<table border="1" width="100%" id="ajxtbl" style="font-size:11px; font-family:Arial,Tahoma"><tr><td><strong>Название сервера</strong></td><td><strong>Адрес</strong></td><td style="text-align: center"><strong>Кол-во игроков</strong></td></tr>';
	break;
	case "kamclubeng":
		$Result .= '<table border="1" width="100%" id="ajxtbl" style="font-size:11px; font-family:Arial,Tahoma"><tr><td><strong>Server name</strong></td><td><strong>Address</strong></td><td style="text-align: center"><strong>Players</strong></td></tr>';
	default:
}
/////////////////////////////////////////
//                BODY                 //
/////////////////////////////////////////
Remove_Old_Servers($con);
$data = $con->query("SELECT IP, Port, Name, Players, Dedicated, OS, Pingable FROM Servers WHERE Rev='$rev' ORDER BY Players DESC");
while($row = $data->fetch_array())
{
	$Name = $row['Name'];
	$IP = $row['IP'];
	$Port = $row['Port'];
	$PlayerCount = $row['Players'];
	$IsDedicated = $row['Dedicated'];
	$OS = $row['OS'];
	$Alive = $row['Pingable'];
	
	$server_count++;
	switch($format)
	{
		case "refresh":
		case "kamclub":
		case "kamclubeng":
		case "table":
			//Clean color codes matching [$xxxxxx] or []
			$Name = preg_replace('/\\[\\$[0-9a-fA-F]{6}\\]|\\[\\]|\[\]/',"",$Name); //WTF regex
			$Country = IPToCountry($IP);
			$Warning = '';
			if(!$Alive) $Warning = ' <IMG src="'.$BASE_URL.'error.png" alt="Server unreachable" style="vertical-align:middle">';
			$Result .= "<TR class=\"no_translate\"><TD><IMG src=\"".$BASE_URL."flags/".strtolower($Country).".gif\" alt=\"".GetCountryName($Country)."\">&nbsp;$Name</TD><TD>$Warning$IP</TD><TD style=\"text-align: center\">$PlayerCount</TD></TR>\n";
			break;
		case "ajaxupdate":
			//Clean color codes matching [$xxxxxx] or []
			$Name = preg_replace('/\\[\\$[0-9a-fA-F]{6}\\]|\\[\\]|\[\]/',"",$Name); //WTF regex
			$srvsgl = array();
			$srvsgl['c'] = strtolower(IPToCountry($IP));
			$srvsgl['n'] = $Name;
			if(!$Alive) { $srvsgl['a'] = "0"; } //$Alive could be '' to mean false
			else        { $srvsgl['a'] = "1"; }
			$srvsgl['i'] = $IP;
			//$Result['o'] = $Port; // not used yet
			$srvsgl['p'] = $PlayerCount;
			$Result[] = $srvsgl;
			break;
		default:
			if(substr($rev,1) >= 4878) //New server releases expect more parameters
				$Result .= "$Name,$IP,$Port,$IsDedicated,$OS\n";
			else
				$Result .= "$Name,$IP,$Port\n";
	}
}
/////////////////////////////////////////
//              FOOTER                 //
/////////////////////////////////////////
switch($format) {
	case "ajaxupdate":
		$Result = json_encode(Array("cnt"=>$server_count,"srvs"=>$Result));
		$Result = $_GET['jsonp_callback']."(".$Result.")";
		break;
	case "kamclub":
	case "kamclubeng":
	case "refresh":
	case "table":
		$Result .= '</table>';
	default:
}
echo $Result;
$con->close();
?>
