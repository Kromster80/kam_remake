<HTML>
<HEAD>
<TITLE></TITLE>
</HEAD>
<BODY>  
    <?php
      function AddForm($FileName, $ParamsNames, $Params, $BtnName, $Index)
      {
        echo "<form action='index.php?f=".$Index."' method=POST>";
        if (( count( $_POST ) ) && ($_GET['f'] == $Index))        
        {
           include $FileName;
        }
        else if (!($_GET['f']))
        { 
          if ($Params)
          {                                                                    
            for ($i = 0; $i < count($Params); $i++)
            {
            	echo "<p>".$ParamsNames[$i]."</p><input name='".$Params[$i]."' value='' type='text' />";
            }
          }
         echo "<p><input name='op' id='add' value='".$BtnName."' class='form-submit' type='submit' /></p>";
        }  
        echo "</form>";
      }
    ?>
<Table Border=1>
<tr>
<td valign=Top>
    <?php
      AddForm('add_user.php', array('User Name', 'PassWord', 'IP'), 
              array('name', 'password', 'ip'), 'Add User', 1);
      AddForm('list_users.php', array(), array(), 'Get Users', 2);   
      AddForm('clean_users.php', array(), array(), 'Clean Users', 3);           
      AddForm('update_last_visit.php', array('User Name'), array('name'), 'Update last visit', 4); 
      AddForm('add_room.php', array('Host Name', 'Room Name'), 
              array('host_name', 'room_name'), 'Add Room', 5);        
      AddForm('list_rooms.php', array(), array(), 'Get Rooms', 6);     
    ?>
</td>
<td valign=Top>
    <?php
      AddForm('add_user_to_room.php', array('User Name', 'Room Name'),
              array('user_name', 'room_name'), 'Add User To Room', 7);
      AddForm('leave_room.php', array('User Name', 'Room Name'),
              array('user_name', 'room_name'), 'Leave Room', 8);
      AddForm('clean_users_in_room.php', array(), array(), 'Clean Users in Room', 9);
      AddForm('clean_rooms.php', array(), array(), 'Clean Rooms', 10);
      AddForm('list_users_in_room.php', array('Room Name'), array('room_name'), 'Get Users in Room', 11);
    ?>
</td>
<td valign=Top>
    <?php
        AddForm('add_post.php', array('User Name', 'Room Name', 'Text'), 
                array('user_name', 'room_name', 'text'), 'Add Post', 12);
        AddForm('list_posts.php', array('Room Name'), array('room_name'), 'List Posts', 13);
        AddForm('clean_posts.php', array(), array(), 'Clean posts', 14);
    ?>
</td>
</tr>
</Table>
</BODY>
</HTML>