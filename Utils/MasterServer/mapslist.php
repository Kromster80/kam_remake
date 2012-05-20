<?

include("serverlib.php");
if(!CheckVersion($_REQUEST["rev"]))
{
	die("Invalid revision");
}

$format = "";
if(isset($_REQUEST["format"])) $format = $_REQUEST["format"];

$num = -1;
if(isset($_REQUEST["num"])) $num = $_REQUEST["num"];

echo GetMaps($format,$_REQUEST["rev"],$num);

?>