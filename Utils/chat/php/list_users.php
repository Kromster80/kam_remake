<?php
    include "clean_users.php";

    echo "Get users list  ";
    $mysqli = new mysqli("localhost", "assoft_user", "assoft_user0", "assoft");
    
     if($stmt = $mysqli ->prepare("CALL get_users_list()"))
     {
        if (!$stmt->execute())
        {
          echo "fail   ";
          echo $mysqli->errstr;
         echo '<br>ErrorCode = '.$mysqli->errno; 
        }
                    
        $stmt->bind_result($col1, $col2, $col3, $col4); 

        echo '<p>';
        printf("%s %s %s %s\n", 'name', 'ip', 'last visit', 'outTime');                                 
        echo '</p>';        
        while ($stmt->fetch()) {            
          echo  $col1.','. $col2.','. $col3.','.$col4.'<br>';
       }                            
        
        $stmt->close();    
     }
     else
     {
        echo $mysqli ->error;
     }
    
    $mysqli->close(); 
?> 