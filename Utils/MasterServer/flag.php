<?php
function iptocountry($ip) {   
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

$Image = "http://lewin.hodgman.id.au/kam_remake_master_server/flags/".strtolower(iptocountry($_REQUEST["ip"]).".gif");
header("Location: ".$Image);

?>