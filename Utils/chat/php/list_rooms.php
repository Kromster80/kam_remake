<?php
    echo "Get rooms list  ";
    $mysqli = new mysqli("localhost", "assoft_user", "assoft_user0", "assoft");
    
     if($stmt = $mysqli ->prepare("CALL get_rooms_list()"))
     {
        if (!$stmt->execute())
        {
          echo "fail   ";
          echo $mysqli->errstr;
         echo '<br>ErrorCode = '.$mysqli->errno; 
        }
                    
        $stmt->bind_result($col1, $col2); 

        echo '<p>';
        printf("%s %s\n", 'host name', 'root_name');                                    
        echo '</p>';        
        while ($stmt->fetch()) {            
          echo  $col1.','. $col2.'<br>';
       }                            
        
        $stmt->close();    
     }
     else
     {
        echo $mysqli ->error;
     }
    
    $mysqli->close(); 
?> 