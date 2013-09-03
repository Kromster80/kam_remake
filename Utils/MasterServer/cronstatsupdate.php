<?
//Only allowed to run from cron job (CLI or wget from same server)
(PHP_SAPI === 'cli') or ($_SERVER['SERVER_ADDR'] == $_SERVER['REMOTE_ADDR']) or die('not allowed');

include_once("statistics.php");
include_once("consts.php");
include_once("db.php");

global $MAIN_VERSION;

$con = db_connect();
AddStatsRecord($con, $MAIN_VERSION);
$con->close();
echo "done";

/*
function Update_Crontab() {
	exec('echo "10 * * * * php '.dirname(__FILE__).'/cron_stats.php" | crontab');
}
Update_Crontab();
*/
?>