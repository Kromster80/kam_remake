<?php
    echo "Add room ";
    $mysqli = new mysqli("localhost", "assoft_user", "assoft_user0", "assoft");
    
     if($stmt = $mysqli ->prepare("CALL add_room(?, ?)"))
     {
        $u1 = $_POST['host_name'];
        $u2 = $_POST['room_name'];
        $stmt->bind_param('ss', $u1, $u2);
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
?>