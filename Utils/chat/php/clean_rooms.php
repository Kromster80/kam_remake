<?php
    include "clean_users_in_room.php";
    $params = array();
    exec_mysql_proc("clean_rooms", $params);
?>