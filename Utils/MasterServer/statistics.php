<?php

global $STATS_FILE, $STATS_TEMP_FILE, $STATS_PERIOD;
$STATS_FILE = "stats.txt"; //actual statistics record
$STATS_TEMP_FILE = "stats_tmp.txt"; //servers and playercount of the last x minutes
$STATS_PERIOD = 600; //seconds between stat records

function StatsUpdate($Server, $PlayerCount) {
	global $STATS_TEMP_FILE, $STATS_PERIOD;

	$TempEntries = array();

	if (file_exists($STATS_TEMP_FILE)) {
		$StatsTemp = file($STATS_TEMP_FILE, FILE_SKIP_EMPTY_LINES);

		$LastMajUpdate = rtrim(array_shift($StatsTemp), "\n");

		foreach ($StatsTemp as $Entry) {
			$Entry = explode(',', rtrim($Entry, "\n"));
			$TempEntries[$Entry[0]] = (int)$Entry[1];
		}

		if ($LastMajUpdate < time() - $STATS_PERIOD) {
			StatsMajUpdate($TempEntries);
			$LastMajUpdate = time();
			$TempEntries = array();
		}

		//if (($TempEntries[$Server]) < $PlayerCount)
			$TempEntries[$Server] = $PlayerCount;
	}
	else {
		$LastMajUpdate = time();
		$TempEntries[$Server] = $PlayerCount;
	}

	$fh = fopen($STATS_TEMP_FILE, 'w');
	fwrite($fh, $LastMajUpdate."\n");

	foreach ($TempEntries as $Key=>$Val) 
		fwrite($fh, $Key.','.$Val."\n");

	fclose($fh);
}

function StatsMajUpdate($TempEntries) {
	global $STATS_FILE;

	$Servers = 0; $Players = 0;

	foreach ($TempEntries as $Server=>$PlayerCount) {
		$Servers++; $Players += $PlayerCount;
	}

	$fh = fopen($STATS_FILE, 'a');
	fwrite($fh, time().','.$Servers.','.$Players."\n");
	fclose($fh);
}

function GetServerGraph($size = array(500, 200), $timespan = array(0, 0), $period = 1) {
	global $STATS_FILE, $STATS_PERIOD;
	
	if (!file_exists($STATS_FILE)) return "No statistics available!";
	else $Stats = file($STATS_FILE, FILE_SKIP_EMPTY_LINES);

	$tsince = ($timespan[0] > 0) ? $timespan[0] : strtok($Stats[0], ',');
	$tto    = ($timespan[1] > 0) ? $timespan[1] : time();
	$width  = $size[0];
	$height = $size[1];
	
	$ServerLine = "";
	$PlayerLine = "";
	$Timestamps = "";

	//date_default_timezone_set("UTC");
	
	$p = $period;
	foreach ($Stats as $Line) {
		$Line = explode(',', rtrim($Line, "\n"));
		if ($Line[0] > $tto) break;
		if ($Line[0] < $tsince) continue; 
		
		if (!($p++ % $period)) {
			$Timestamps .= 't('.$Line[0].'),';
			$p = 1;
		}

		$ServerLine .= "'".$Line[1]."',";
		$PlayerLine .= "'".$Line[2]."',";
	}
	
	$ServerLine = rtrim($ServerLine, ",");
	$PlayerLine = rtrim($PlayerLine, ",");
	$Timestamps.= 't('.strtok($Stats[count($Stats) - 1], ',').')';

	$xticks = ($tto - $tsince) / $STATS_PERIOD / $period;
	
	return 
	'<canvas id="Stats" width="'.$width.'" height="'.$height.'">[No canvas support]</canvas><script>
		function t(ts) {
			date = new Date(ts * 1000);
			hours = (date.getHours() < 10) ? String(0)+date.getHours() : date.getHours();
			minutes = (date.getMinutes() < 10) ? String(0)+date.getMinutes() : date.getMinutes();
			return hours+":"+minutes;
			return date.toLocaleString(); 
		}
        window.onload = function () {
            var line = new RGraph.Line("Stats", ['.$ServerLine.'], ['.$PlayerLine.']);
            line.Set("chart.linewidth", 2);
            line.Set("chart.colors", ["red", "black"]);
            //line.Set("chart.title", "Statistics");
            line.Set("chart.labels", ['.$Timestamps.']);
			line.Set("chart.key", ["Servers", "Players"]);
			line.Set("chart.text.angle", 45);
			line.Set("chart.gutter.bottom", 50);
			line.Set("chart.gutter.left", 40);
			line.Set("chart.gutter.top", 10);
			line.Set("chart.gutter.right", 10);
			line.Set("chart.xticks", '.$xticks.');
			line.Set("chart.text.font", "Calibri, MS Sans Serif, Arial, sans-serif");
            line.Draw();
            //document.write((new Date()).toLocaleString());
        }
    </script>';	
}

if (count(get_included_files()) == 1) { //if we have been called directly
	if (
		isset($_REQUEST["since"]) &&
		isset($_REQUEST["to"]) && 
		isset($_REQUEST["width"]) && 
		isset($_REQUEST["height"]) && 
		isset($_REQUEST["period"])
	) {
		if (isset($_REQUEST["html"]))
			echo '<!doctype html><html><head><META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=UTF-8">
			<script src="RGraph/libraries/RGraph.common.core.js" ></script><script src="RGraph/libraries/RGraph.line.js">
			</script><!--[if IE 8]><script src="RGraph/excanvas/excanvas.original.js"></script><![endif]--></head><body>';
		echo GetServerGraph(array($_REQUEST["width"], $_REQUEST["height"]), 
			array($_REQUEST["since"], $_REQUEST["to"]), $_REQUEST["period"]);
		if (isset($_REQUEST["html"])) echo '</body></html>';
	} 
	else if (isset($_REQUEST["default"])) {
		echo '<!doctype html><html><head><META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=UTF-8">
		<script src="RGraph/libraries/RGraph.common.core.js" ></script><script src="RGraph/libraries/RGraph.line.js">
		</script><!--[if IE 8]><script src="RGraph/excanvas/excanvas.original.js"></script><![endif]--></head><body>';
		echo GetServerGraph(array(512,256), array(time() - 24*60*60, time()), 18);
		echo '</body></html>';
	} 
	else echo '<!doctype html><html><head></head><body><form><p>All textfields are mandatory!</p>
		<label>since (timestamp): </label><input name="since" type="text" value="0"><br>
		<label>to (timestamp): </label><input name="to" type="text" value="0"><br>
		<label>width (pixel): </label><input name="width" type="text" value="512"><br>
		<label>height (pixel): </label><input name="height" type="text" value="256"><br>
		<label>period (of labels per datapoints): </label><input name="period" type="text" value="18"><br>
		<input type="checkbox" name="html" value="true" checked><label>include HTML-header and footer</label><br>
		<input type="submit" value="Submit"></form></body></html>';
}

?>
