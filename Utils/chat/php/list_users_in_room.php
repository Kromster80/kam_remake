<?php
    echo "Get users in room list  ";
    $mysqli = new mysqli("localhost", "assoft_user", "assoft_user0", "assoft");
   
     if($stmt = $mysqli ->prepare("CALL get_room_users_list('".$_POST['room_name']."')"))
     {
        if (!$stmt->execute())
        {
          echo "fail   ";
          echo $mysqli->errstr;
         echo '<br>ErrorCode = '.$mysqli->errno;
        }
                   
        $stmt->bind_result($col1);

        echo '<p>';
        printf("%s\n", 'user name');                                    
        echo '</p>';        
        while ($stmt->fetch()) {            
          echo  $col1.'<br>';
       }                            
       
        $stmt->close();    
     }
     else
     {
        echo $mysqli ->error;
     }
   
    $mysqli->close();
?>
