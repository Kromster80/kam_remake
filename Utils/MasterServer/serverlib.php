<?

global $DATA_FILE, $DISALLOWED_CHARS, $GAME_VERSION, $MAX_TTL;
$GAME_VERSION = 'r2170';
$MAX_TTL = 600; //10 minutes
$DATA_FILE = "servers.txt";
$DISALLOWED_CHARS  = array("|", ",","\n","\r");

function CheckVersion($aRev)
{
	global $GAME_VERSION;
	return ($aRev == $GAME_VERSION);
}

function GetServers()
{
	global $DATA_FILE;
	$Result = "";
	if(!file_exists($DATA_FILE))
		return "";
	$Lines = file($DATA_FILE);
	foreach($Lines as $Line)
	{
		//Data does not yet use quotes or backslashes, but it might in future
		$Line = trim(stripslashes_if_gpc_magic_quotes($Line));
		list($Name,$IP,$Port,$Expiry) = explode("|",$Line);
		if(time() < $Expiry)
		{
			$Result .= "$Name,$IP,$Port\n";
		}
	}
	return $Result;
}

function AddServer($aName,$aIP,$aPort,$aTTL)
{
	global $DATA_FILE, $DISALLOWED_CHARS, $MAX_TTL;
	//Remove characters that are not allowed (used for internal formatting)
	$aName = str_replace($DISALLOWED_CHARS,"",$aName);
	$aIP = str_replace($DISALLOWED_CHARS,"",$aIP);
	$aPort = str_replace($DISALLOWED_CHARS,"",$aPort);
	$aTTL = str_replace($DISALLOWED_CHARS,"",$aTTL);
	//Enforce max TTL, so people can not add a server that lasts a thousand years!
	$aTTL = min($aTTL,$MAX_TTL);
	$Servers = "";
	$Exists = false;
	if(file_exists($DATA_FILE))
	{
		$Lines = file($DATA_FILE);
		foreach($Lines as $Line)
		{
			//Data does not yet use quotes or backslashes, but it might in future
			$Line = trim(stripslashes_if_gpc_magic_quotes($Line));
			list($Name,$IP,$Port,$Expiry) = explode("|",$Line);
			if(time() < $Expiry)
			{
				if(($IP == $aIP) && ($Port == $aPort))
				{
					$Servers .= "$aName|$IP|$Port|".(time()+$aTTL)."\n";
					$Exists = true;
				}
				else
				{
					$Servers .= "$Name|$IP|$Port|$Expiry\n";
				}
			}
		}
	}
	if(!$Exists)
	{
		$Servers .= "$aName|$aIP|$aPort|".(time()+$aTTL)."\n";
	}
	$fh = fopen($DATA_FILE, 'w') or die("can't open file");
	fwrite($fh, $Servers);
	fclose($fh);
	return "Success";
}

function stripslashes_if_gpc_magic_quotes( $string ) {
    if(get_magic_quotes_gpc()) {
        return stripslashes($string);
    } else {
        return $string;
    }
}

?>
