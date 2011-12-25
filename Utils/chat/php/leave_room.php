<?php
   include "my_sql_utils.php";

    $params = array(str_post('user_name'), str_post('room_name'));
    exec_mysql_proc("leave_room", $params);
?>