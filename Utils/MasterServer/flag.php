<?php
function IPToCountry($ip) {
	$ip = floatval(sprintf("%u\n", ip2long($ip)));
	$piece = substr($ip, 0, 3);

	if (!file_exists('ip_files/' . $piece . '.php'))
		$country="unknown";
	
	include 'ip_files/' . $piece . '.php';
	foreach ($entries AS $e)
	{
		$e[0] = floatval($e[0]);
		if ($e[0] <= $ip and $e[1] >= $ip)
			$country = $e[2];
	}
	if ($country==""){
		$country="unknown";
	}
    return $country;
}

function GetCountryCode($ip)
{
	return strtolower(iptocountry($ip));
}

function GetCountryName($Country)
{
	include("ip_files/countries.php");
	return $countries[$Country][1];
}

?>