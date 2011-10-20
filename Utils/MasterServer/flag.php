<?php
function IPToCountry($ip) {   
    $numbers = preg_split( "/\./", $ip);   
    include("ip_files/".$numbers[0].".php");
    $code=($numbers[0] * 16777216) + ($numbers[1] * 65536) + ($numbers[2] * 256) + ($numbers[3]);   
    foreach($ranges as $key => $value){
        if($key<=$code){
            if($ranges[$key][0]>=$code){$country=$ranges[$key][1];break;}
            }
    }
    if ($country==""){$country="unkown";}
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