<HTML>
<HEAD>
<TITLE></TITLE>
</HEAD>
<BODY>     
    <form action="index.php?f=1" method=POST>  
    <?php
         if (( count( $_POST ) ) && ($_GET['f'] == 1))        
         {
           include 'add_user.php';
    ?>         
    <?php
         } else if (!($_GET['f']))
         {
    ?> 
         <p>User Name</p><input name='name' value='' type='text' />
         <p>PassWord</p><input name='password' value='' type='text' />
         <p>IP</p><input name='ip' value='' type='text' />    
         <p><input name='op' id='add' value='Add User' class='form-submit' type='submit' /></p>
    <?php
         }
    ?> 
    </form>    
    <form action="index.php?f=2" method=POST>  
    <?php
         if (( count( $_POST ) ) && ($_GET['f'] == 2))        
         {
           include 'list_users.php';
    ?>         
    <?php
         } else if (!($_GET['f']))
         {
    ?> 
         <p><input name='op' id='add' value='Get Users' class='form-submit' type='submit' /></p>
    <?php
         }
    ?> 
    </form>  
    <form action="index.php?f=3" method=POST>  
    <?php
         if (( count( $_POST ) ) && ($_GET['f'] == 3))        
         {
           include 'clean_users.php';
    ?>         
    <?php
         } else if (!($_GET['f']))
         {
    ?> 
         <p><input name='op' id='add' value='Clean Users' class='form-submit' type='submit' /></p>
    <?php
         }
    ?> 
    </form>  
    <form action="index.php?f=4" method=POST>  
    <?php
         if (( count( $_POST ) ) && ($_GET['f'] == 4))        
         {
           include 'update_last_visit.php';
    ?>         
    <?php
         } else if (!($_GET['f']))
         {
    ?> 
         <p>User Name</p><input name='name' value='' type='text' />
         <p><input name='op' id='add' value='Update last visit' class='form-submit' type='submit' /></p>
    <?php
         }
    ?> 
    </form>  
    <form action="index.php?f=5" method=POST>  
    <?php
         if (( count( $_POST ) ) && ($_GET['f'] == 5))        
         {
           include 'add_room.php';
    ?>         
    <?php
         } else if (!($_GET['f']))
         {
    ?> 
         <p>Host Name</p><input name='host_name' value='' type='text' />
         <p>Room Name</p><input name='room_name' value='' type='text' />
         <p><input name='op' id='add' value='Add Room' class='form-submit' type='submit' /></p>
    <?php
         }
    ?> 
    </form>    
    <form action="index.php?f=6" method=POST>  
    <?php
         if (( count( $_POST ) ) && ($_GET['f'] == 6))        
         {
           include 'list_rooms.php';
    ?>         
    <?php
         } else if (!($_GET['f']))
         {
    ?> 
         <p><input name='op' id='add' value='Get Rooms' class='form-submit' type='submit' /></p>
    <?php
         }
    ?> 
    </form>  
</BODY>
</HTML>