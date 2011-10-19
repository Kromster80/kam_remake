<?php

global $STATS_FILE, $STATS_TEMP_FILE;
$STATS_FILE = "stats.txt"; //actual statistics record
$STATS_TEMP_FILE = "stats_tmp.txt"; //servers and playercount of the last 10 minutes

function StatsUpdate($Server, $PlayerCount) {
	global $STATS_TEMP_FILE;

	$TempEntries = array();

	if (file_exists($STATS_TEMP_FILE)) {
		$StatsTemp = file($STATS_TEMP_FILE, FILE_SKIP_EMPTY_LINES);

		$LastMajUpdate = rtrim(array_shift($StatsTemp), "\n");

		foreach ($StatsTemp as $Entry) {
			$Entry = explode(',', rtrim($Entry, "\n"));
			$TempEntries[$Entry[0]] = (int)$Entry[1];
		}

		if ($LastMajUpdate < time() - 600) {
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

function GetFancyStats($since = 0, $to = 0) {
	global $STATS_FILE;
	if ($to == 0) $to = time();
	
	if (!file_exists($STATS_FILE)) return "No statistics available!";
	$Stats = file($STATS_FILE, FILE_SKIP_EMPTY_LINES);
	
	$ServerLine = "";
	$PlayerLine = "";
	$Timestamps = "";
	
	$lastday = 0;
	date_default_timezone_set('Europe/Berlin');
	
	foreach ($Stats as $Line) {
		$Line = explode(',', rtrim($Line, "\n"));
		if ($Line[0] > $to) break;
		if ($Line[0] < $since) continue; 
		
		if (date("d", $Line[0]) != $lastday) {
			$date = date("d. G:i", $Line[0]);
			$lastday = date("d", $Line[0]);
		}
		else $date = date("G:i", $Line[0]);
		
		$Timestamps .= "'".$date."',";  //insert date() here!
		$ServerLine .= "'".$Line[1]."',";
		$PlayerLine .= "'".$Line[2]."',";
	}
	
	$ServerLine = rtrim($ServerLine, ",");
	$PlayerLine = rtrim($PlayerLine, ",");
	$Timestamps = rtrim($Timestamps, ",");
	
	return "<!doctype html><html><head><META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=UTF-8\">
	<script src=\"RGraph/libraries/RGraph.common.core.js\" ></script><script src=\"RGraph/libraries/RGraph.line.js\" ></script>
	<!--[if IE 8]><script src=\"RGraph/excanvas/excanvas.original.js\"></script><![endif]--></head><body>
	<canvas id=\"Stats\" width=\"1000\" height=\"250\">[No canvas support]</canvas><script>
        window.onload = function () {
            var line = new RGraph.Line('Stats', [".$ServerLine."], [".$PlayerLine."]);
            line.Set('chart.linewidth', 2);
            line.Set('chart.colors', ['red', 'black']);
            line.Set('chart.title', 'Statistics');
            line.Set('chart.labels', [".$Timestamps."]);
			line.Set('chart.key', ['Server', 'Player']);
            line.Draw();
        }
    </script></body></html>";
	
}

?>
