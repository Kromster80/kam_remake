<?php
    echo "Add user";


    $mysqli = new mysqli("localhost", "assoft_user", "assoft_user0", "assoft");

     if($stmt = $mysqli ->prepare("CALL add_user(?, ?, ?)"))
     {
        $u1 = $_GET['name'];
        $u2 = $_GET['password'];
        $u3 = $_GET['ip'];
        $stmt->bind_param('sss', $u1, $u2, $u3);
        if (!$stmt->execute())
        {
          echo "fail   ";
          echo $mysqli->error;
          die;
        }

        //$stmt->bind_result($col1);

        /* &#1042;&#1099;&#1073;&#1086;&#1088;&#1082;&#1072; &#1079;&#1085;&#1072;&#1095;&#1077;&#1085;&#1080;&#1081; */
        //while ($stmt->fetch()) {
        //   printf("%s \n", $col1); }

        $stmt->close();
     }
     else
     {
        echo "Errormessage: %s\n", $mysqli ->error;
     }

    $mysqli->close();
?>