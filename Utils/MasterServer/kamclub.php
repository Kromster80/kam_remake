<?
$RefRate = 60 + rand(1,9);
if($_REQUEST['type'] == 'graph')
{
  $RefRate = $RefRate*10;
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
	echo file_get_contents('http://kam.hodgman.id.au/serverquery.php?format=kamclub');
if($_REQUEST['type'] == 'graph')
	echo file_get_contents('http://kam.hodgman.id.au/statistics.php?since='.(time()-(24*60*60)-(10*60)).'&to=0&width=500&height=250&period=18&format=kamclub');
?>
</body>
</html>