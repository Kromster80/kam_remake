<?php
include_once("consts.php");
include_once("statistics.php");

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

function GetMapFileName($Rev)
{
	global $MAIN_VERSION;
	if($Rev == "") $Rev = $MAIN_VERSION;
	Return "maps.$Rev.txt";
}

function TestConnection($aIP, $aPort) {
	//My server (kam.hodgman.id.au) can do outgoing connections on port 56789 (99% of servers use this) because we asked for permission
	if($aPort == "56789") {
	  $fp = @fsockopen($aIP, $aPort, $errnum, $errstr, 6); //@ means suppress errors such as "failed to connect"
	  if($fp) {
		fclose($fp);
		return true;
	  } else
		return false;
	}
	else
	  return (file_get_contents('http://www.kamremake.com/portcheck.php?ip='.$aIP.'&port='.$aPort.'') == 'TRUE');
}

function Remove_Old_Servers($con) {
	$con->query("DELETE FROM Servers WHERE Expiry <= '".date("Y-m-d H:i:s")."'");
}

function CheckVersion($aRev)
{
	$Result = ctype_alnum($aRev); //Protect against injection attacks by only allowing alphanumeric characters
	$Result = $Result && (strlen($aRev) <= 8); //Don't allow really long names
	return $Result;
}

function plural($count, $singular, $plural = 's') {
    if ($plural == 's') {
        $plural = $singular . $plural;
    }
    return ($count == 1 ? $singular : $plural);
}

function stripslashes_if_gpc_magic_quotes( $string ) {
    if(get_magic_quotes_gpc()) {
        return stripslashes($string);
    } else {
        return $string;
    }
}

?>
