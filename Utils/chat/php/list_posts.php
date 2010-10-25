<?php
    echo "Get posts list  ";
    $mysqli = new mysqli("localhost", "assoft_user", "assoft_user0", "assoft");
   
     if($stmt = $mysqli ->prepare("CALL get_posts_list('".$_POST['room_name']."')"))
     {
        if (!$stmt->execute())
        {
          echo "fail   ";
          echo $mysqli->errstr;
         echo '<br>ErrorCode = '.$mysqli->errno;
        }
                   
        $stmt->bind_result($col1, $col2, $col3);

        echo '<p>';
        printf("%s %s %s\n", 'user name', 'text', 'time');                                    
        echo '</p>';        
        while ($stmt->fetch()) {            
          echo  $col1.','. $col2.','. $col3.'<br>';
       }                            
       
        $stmt->close();    
     }
     else
     {
        echo $mysqli ->error;
     }
   
    $mysqli->close();
?>