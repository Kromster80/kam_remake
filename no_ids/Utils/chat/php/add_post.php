<?php
   include "my_sql_utils.php";

   $q=$_POST['text'];
   $text = html_entity_decode($q, ENT_NOQUOTES, 'UTF-8');

    $params = array(str_post('user_name'),str_post('room_name'), "'".$text."'");
    exec_mysql_proc("add_post", $params);
?>