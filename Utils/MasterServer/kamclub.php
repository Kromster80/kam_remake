<?
include_once("consts.php");
global $BASE_URL, $TABLE_REFRESH;
$RefRate = $TABLE_REFRESH;
if($_REQUEST['type'] == 'graph') {
  $RefRate = $RefRate*10;
}
$format = $_REQUEST['format'];
if($format == "") {
  $format = "kamclub";
}

?><html>
<head>
<META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=UTF-8"><meta http-equiv="refresh" content="<? echo $RefRate; ?>">
<script src="RGraph/libraries/RGraph.common.core.js" ></script>
<script src="RGraph/libraries/RGraph.line.js"></script>
</head>
<body style="font-size:9px; font-family:Arial,Tahoma">
<?
if($_REQUEST['type'] == 'list')
	echo file_get_contents($BASE_URL.'serverquery.php?format='.$format);
if($_REQUEST['type'] == 'graph')
	echo file_get_contents($BASE_URL.'statistics.php?since='.(time()-(24*60*60)-(10*60)).'&to=0&width=500&height=250&period=18&format='.$format);
?>
</body>
</html>