<?php

    function no_str_post($param_name)
   {
       return $_POST[$param_name];
   }

    function str_post($param_name)
   {
      return "'".no_str_post($param_name)."'";
   }

    function exec_mysql_proc($proc_name, $params)
    {
      echo $proc_name." ";
      $mysqli = new mysqli("localhost", "assoft_user", "assoft_user0", "assoft");
      $s = "CALL ".$proc_name."(";
      for ($i = 0; $i < count($params); $i++)
      {
        $s = $s.$params[$i];
        if ($i != count($params) - 1)
          $s = $s.", ";
      }
      $s = $s.")";

       if($stmt = $mysqli ->prepare($s))
       {
          if (!$stmt->execute())
          {
            echo "  fail:   ";
            echo $mysqli->error;
            echo '<br>ErrorCode = '.$mysqli->errno; 
          }
          $stmt->close();    
       }
       else
       {
          echo $mysqli ->error;
       }
    
      $mysqli->close(); 

    }
?>