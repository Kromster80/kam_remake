<?

include("serverlib.php");
if(!CheckVersion($_REQUEST["rev"]))
{
	die("Invalid revision");
}

$Map = $_REQUEST["map"];
$PlayerCount = $_REQUEST["playercount"];
$Rev = $_REQUEST["rev"];
if(($Map == "") || ($Rev == "") || ($PlayerCount == ""))
{
	die("Incorrect parameters");
}
echo AddMap($Map,$PlayerCount,$Rev);


?>