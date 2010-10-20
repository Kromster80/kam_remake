<HTML>
<HEAD>
<TITLE></TITLE>
</HEAD>
<BODY>
    <form action="index.php?f=1" method=POST>
    <?php
         if (( count( $_POST ) ) && ($_GET['f'] == 1))
         {
    ?>
         <a href='add_user.php?name=<?php echo $_POST[ 'name' ]
                                                               ?>&password=<?php echo $_POST[ 'password' ]
                                                               ?>&ip=<?php echo $_POST[ 'ip' ] ?>'>&#1089;&#1089;&#1099;&#1083;&#1082;&#1072;</a>
    <?php
         } else if (!($_GET['f']))
         {
    ?>
         <p>add_user</p>
         <p>User Name</p><input name='name' value='' type='text' />
         <p>PassWord</p><input name='password' value='' type='text' />
         <p>IP</p><input name='ip' value='' type='text' />
         <p><input name='op' id='checkball' value='Enter' class='form-submit' type='submit' /></p>
    <?php
         }
    ?>
    </form>
</BODY>
</HTML>