<?php
    include "my_sql_utils.php";
    $params = array();
    exec_mysql_proc("clean_last_posts", $params);
?>