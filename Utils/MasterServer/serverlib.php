<?php

global $DISALLOWED_CHARS, $MAIN_VERSION, $MAX_TTL, $DO_STATS;
$DO_STATS = true;
$MAIN_VERSION = 'r2678';
$MAX_TTL = 600; //10 minutes
$DISALLOWED_CHARS  = array("|", ",","\n","\r");

if ($DO_STATS) include("statistics.php");

/*
* json_encode provider for the case that the webserver has PHP < 5.2.0
*/
if (!function_exists('json_encode'))
{
  function json_encode($a=false)
  {
    if (is_null($a)) return 'null';
    if ($a === false) return 'false';
    if ($a === true) return 'true';
    if (is_scalar($a))
    {
      if (is_float($a))
      {
        // Always use "." for floats.
        return floatval(str_replace(",", ".", strval($a)));
      }

      if (is_string($a))
      {
        static $jsonReplaces = array(array("\\", "/", "\n", "\t", "\r", "\b", "\f", '"'), array('\\\\', '\\/', '\\n', '\\t', '\\r', '\\b', '\\f', '\"'));
        return '"' . str_replace($jsonReplaces[0], $jsonReplaces[1], $a) . '"';
      }
      else
        return $a;
    }
    $isList = true;
    for ($i = 0, reset($a); $i < count($a); $i++, next($a))
    {
      if (key($a) !== $i)
      {
        $isList = false;
        break;
      }
    }
    $result = array();
    if ($isList)
    {
      foreach ($a as $v) $result[] = json_encode($v);
      return '[' . join(',', $result) . ']';
    }
    else
    {
      foreach ($a as $k => $v) $result[] = json_encode($k).':'.json_encode($v);
      return '{' . join(',', $result) . '}';
    }
  }
}

function GetDataFileName($Rev)
{
	global $MAIN_VERSION;
	if($Rev == "") $Rev = $MAIN_VERSION;
	Return "servers.$Rev.txt";
}

function CheckVersion($aRev)
{
	$Result = ctype_alnum($aRev); //Protect against injection attacks by only allowing alphanumeric characters
	$Result = $Result && (strlen($aRev) <= 7); //Don't allow really long names
	return $Result;
}

function plural($count, $singular, $plural = 's') {
    if ($plural == 's') {
        $plural = $singular . $plural;
    }
    return ($count == 1 ? $singular : $plural);
}


function GetStats($Format)
{
	global $MAIN_VERSION;
	$DATA_FILE = GetDataFileName($MAIN_VERSION); //Use the main revision for stats
	$ServerCount = 0;
	$TotalPlayerCount = 0;
	if(!file_exists($DATA_FILE))
		return "";
	$Lines = file($DATA_FILE);
	foreach($Lines as $Line)
	{
		//Data does not yet use quotes or backslashes, but it might in future
		$Line = trim(stripslashes_if_gpc_magic_quotes($Line));
		list($Name,$IP,$Port,$PlayerCount,$Alive,$Expiry) = explode("|",$Line);
		if(time() < $Expiry)
		{
			$ServerCount++;
			$TotalPlayerCount = $TotalPlayerCount + $PlayerCount;
		}
	}
	switch ($Format)
	{
		case "kamclub":
			return '<html><head><META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=UTF-8"></head><body><div style="font-size:11px; font-family:Arial,Tahoma"><b>Кол-во серверов:</b> '.$ServerCount.'<BR><b>Кол-во игроков:</b> '.$TotalPlayerCount.'</font></div></body></html>';
		case "ajaxupdate":
			$data = json_encode(Array("pct"=>$TotalPlayerCount,"sct"=>$ServerCount));
			return $_GET['jsonp_callback']."(".$data.")";
		case "csv":
			return $ServerCount.','.$TotalPlayerCount;
		case "refresh":
			/*
			* user-side request after 30s with parameter ?format=ajaxupdate which then updates the numbers
			*/
			$startscript = '<script type="text/javascript">'."\n".
			'function updnr(){setTimeout(function (){jQuery.ajax({dataType: "jsonp",jsonp: "jsonp_callback",url: "http://lewin.hodgman.id.au/kam_remake_master_server/serverstats.php?format=ajaxupdate",success: function (data){jQuery("#scount").empty().append(data.sct);jQuery("#pcount").empty().append(data.pct);updnr();}});}, 30000);}'."\n".
			'jQuery(document).ready(function($){updnr();});</script>'."\n";
			return $startscript.'There '.plural($ServerCount,'is','are',true).' <span id="scount">'.$ServerCount.'</span> '.plural($ServerCount,'server').' running and <span id="pcount">'.$TotalPlayerCount.'</span> '.plural($TotalPlayerCount,'player').' online';
		default:
			return 'There '.plural($ServerCount,'is','are',true).' '.$ServerCount.' '.plural($ServerCount,'server').' running and '.$TotalPlayerCount.' '.plural($TotalPlayerCount,'player').' online';
	}
}

function GetServers($aFormat,$aRev)
{
	$DATA_FILE = GetDataFileName($aRev);
	include("flag.php");
	$Result = "";
	$cnts = 0;
	if(!file_exists($DATA_FILE))
		return "";
	switch($aFormat)
	{
		case "ajaxupdate":
			$Result = array();
		break;
		case "refresh":
			$Result = '<script type="text/javascript">'."\n".
			'function srvlsttim(dat){var x="<tr><td><strong>Name</strong></td><td><strong>Address</strong></td><td style=\"text-align: center\"><strong>Players</strong></td></tr>";for(var n=0;n<dat.cnt;n++){x+="<tr><td><img src=\"http://lewin.hodgman.id.au/kam_remake_master_server/flags/"+dat.srvs[n].c+".gif\" alt=\""+dat.srvs[n].c+"\" />&nbsp;"+dat.srvs[n].n+"</td><td>"+(dat.srvs[n].a=="0"?" <img src=\"http://lewin.hodgman.id.au/kam_remake_master_server/error.png\" alt=\"Server unreachable\" style=\"vertical-align:middle\" />":"")+dat.srvs[n].i+"</td><td style=\"text-align: center\">"+dat.srvs[n].p+"</td></tr>";jQuery("#ajxtbl").empty().append(x);}}'."\n".
			'function updsr(){setTimeout(function (){jQuery.ajax({dataType: "jsonp",jsonp: "jsonp_callback",url: "http://lewin.hodgman.id.au/kam_remake_master_server/serverquery.php?format=ajaxupdate",success: function (data){srvlsttim(data);updsr();}});}, 30000);}'."\n".
			'jQuery(document).ready(function($){updsr();});</script>'."\n";
		case "table":
			$Result .= '<table border="1" width="100%" id="ajxtbl"><tr><td><strong>Name</strong></td><td><strong>Address</strong></td><td style="text-align: center"><strong>Players</strong></td></tr>';
		default:
	}
	$Lines = file($DATA_FILE);
	foreach($Lines as $Line)
	{
		//Data does not yet use quotes or backslashes, but it might in future
		$Line = trim(stripslashes_if_gpc_magic_quotes($Line));
		list($Name,$IP,$Port,$PlayerCount,$Alive,$Expiry) = explode("|",$Line);
		if(time() < $Expiry)
		{
			$cnts++;
			switch($aFormat)
			{
				case "refresh":
				case "table":
					$Country = IPToCountry($IP);
					$Warning = '';
					if(!$Alive) $Warning = ' <IMG src="http://lewin.hodgman.id.au/kam_remake_master_server/error.png" alt="Server unreachable" style="vertical-align:middle">';
					$Result .= "<TR><TD><IMG src=\"http://lewin.hodgman.id.au/kam_remake_master_server/flags/".strtolower($Country).".gif\" alt=\"".GetCountryName($Country)."\">&nbsp;$Name</TD><TD>$Warning$IP</TD><TD style=\"text-align: center\">$PlayerCount</TD></TR>\n";
					break;
				case "ajaxupdate":
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
					$Result .= "$Name,$IP,$Port\n";
			}
		}
	}
	switch($aFormat)
	{
		case "ajaxupdate":
			$Result = json_encode(Array("cnt"=>$cnts,"srvs"=>$Result));
			$Result = $_GET['jsonp_callback']."(".$Result.")";
			break;
		case "refresh":
		case "table":
			$Result .= '</table>';
		default:
	}
	return $Result;
}

function AddServer($aName,$aIP,$aPort,$aPlayerCount,$aTTL,$aRev)
{
	global $DISALLOWED_CHARS, $MAX_TTL, $DO_STATS, $MAIN_VERSION;
	$DATA_FILE = GetDataFileName($aRev);
	//Remove characters that are not allowed (used for internal formatting)
	$aName = str_replace($DISALLOWED_CHARS,"",$aName);
	$aIP = str_replace($DISALLOWED_CHARS,"",$aIP);
	$aPort = str_replace($DISALLOWED_CHARS,"",$aPort);
	$aTTL = str_replace($DISALLOWED_CHARS,"",$aTTL);
	$aPlayerCount = str_replace($DISALLOWED_CHARS,"",$aPlayerCount);
	//Enforce max TTL, so people can not add a server that lasts a thousand years!
	$aTTL = min($aTTL,$MAX_TTL);
	$Servers = "";
	$Exists = false;
	$aAlive = (file_get_contents('http://beta.nonsenseinc.de/portcheck.php?ip='.$aIP.'&port='.$aPort.'') == 'TRUE');
	
	//Only record statistics about the main revision (for now)
	if (($DO_STATS) && ($aRev == $MAIN_VERSION)) StatsUpdate($aName,$aPlayerCount);
	
	if(file_exists($DATA_FILE))
	{
		$Lines = file($DATA_FILE);
		foreach($Lines as $Line)
		{
			//Data does not yet use quotes or backslashes, but it might in future
			$Line = trim(stripslashes_if_gpc_magic_quotes($Line));
			list($Name,$IP,$Port,$PlayerCount,$Alive,$Expiry) = explode("|",$Line);
			if(time() < $Expiry)
			{
				if(($IP == $aIP) && ($Port == $aPort))
				{
					$Servers .= "$aName|$IP|$Port|$aPlayerCount|$aAlive|".(time()+$aTTL)."\n";
					$Exists = true;
				}
				else
				{
					$Servers .= "$Name|$IP|$Port|$PlayerCount|$Alive|$Expiry\n";
				}
			}
		}
	}
	if(!$Exists)
	{
		$Servers .= "$aName|$aIP|$aPort|$aPlayerCount|$aAlive|".(time()+$aTTL)."\n";
	}
	$fh = fopen($DATA_FILE, 'w') or die("can't open file");
	fwrite($fh, $Servers);
	fclose($fh);
	return 'Success';
}

function stripslashes_if_gpc_magic_quotes( $string ) {
    if(get_magic_quotes_gpc()) {
        return stripslashes($string);
    } else {
        return $string;
    }
}

?>
