<?

global $DATA_FILE;
$DATA_FILE = "servers.txt";

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
		list($IP,$Port,$Expiry) = explode("|",$Line);
		if(time() < $Expiry)
		{
			$Result .= "$IP,$Port\n";
		}
	}
	return $Result;
}

function AddServer($aIP,$aPort,$aTTL)
{
	global $DATA_FILE;
	$Servers = "";
	$Exists = false;
	if(file_exists($DATA_FILE))
	{
		$Lines = file($DATA_FILE);
		foreach($Lines as $Line)
		{
			//Data does not yet use quotes or backslashes, but it might in future
			$Line = trim(stripslashes_if_gpc_magic_quotes($Line));
			list($IP,$Port,$Expiry) = explode("|",$Line);
			if(time() < $Expiry)
			{
				if(($IP == $aIP) && ($Port == $aPort))
				{
					$Servers .= "$IP|$Port|".(time()+$aTTL)."\n";
					$Exists = true;
				}
				else
				{
					$Servers .= "$IP|$Port|$Expiry\n";
				}
			}
		}
	}
	if(!$Exists)
	{
		$Servers .= "$aIP|$aPort|".(time()+$aTTL)."\n";
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
