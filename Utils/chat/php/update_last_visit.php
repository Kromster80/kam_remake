<?php
    echo "Update last visit  ";
    $mysqli = new mysqli("localhost", "assoft_user", "assoft_user0", "assoft");
    
     if($stmt = $mysqli ->prepare("CALL update_last_visit(?)"))
     {
        $u1 = $_POST['name'];
        $stmt->bind_param('s', $u1);
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