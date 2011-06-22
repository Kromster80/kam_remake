<?php
    include "my_sql_utils.php";
    $params = array(str_post('name'));
    exec_mysql_proc("update_last_visit", $params);
?>