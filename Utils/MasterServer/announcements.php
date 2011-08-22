<?
include("serverlib.php");
global $GAME_VERSION;

$Lang = $_REQUEST["lang"];
$Rev = $_REQUEST["rev"];

//First see if they are up to date
if($Rev != $GAME_VERSION)
{
	switch($Lang)
	{
		/*case 'ger':
			echo "";
			break;*/
		default:
			echo "Your KaM Remake version is out of date! You are running ".$Rev." but the most recent version is ".$GAME_VERSION.".|You cannot play online until you have updated.||Please download the update at: http://castlesand.googlecode.com/";
	}
}
else
{
	switch($Lang)
	{
		/*case 'ger':
			echo "";
			break;*/
		default:
			echo "Welcome to the Knights and Merchants Remake online!||Weekly matches are currently run every Sunday at 9pm Central European Time. Please join us if you can!|Thank you for your support!";
	}
}
?>
