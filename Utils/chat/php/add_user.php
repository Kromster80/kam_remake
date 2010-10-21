<?php
    include "clean_users.php";
    $params = array(str_post('name'), str_post('password'), str_post('ip'));
    exec_mysql_proc("add_user", $params);
?>