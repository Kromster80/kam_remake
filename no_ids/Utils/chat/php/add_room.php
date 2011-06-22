<?php
   include "my_sql_utils.php";

    $params = array(str_post('host_name'), str_post('room_name'));
    exec_mysql_proc("add_room", $params);
?>