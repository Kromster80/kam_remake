<?php
    echo "Clean users  ";
    $mysqli = new mysqli("localhost", "assoft_user", "assoft_user0", "assoft");
    
     if($stmt = $mysqli ->prepare("CALL clean_users()"))
     {
        if (!$stmt->execute())
        {
          echo "fail   ";
          echo $mysqli->errstr;
          echo '<br>ErrorCode = '.$mysqli->errno; 
        }                                         
        
        $stmt->close();    
     }
     else
     {
        echo $mysqli ->error;
     }
    
    $mysqli->close(); 
?> 