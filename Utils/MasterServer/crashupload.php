<?
  if($_REQUEST["rev"] == "") die("Incorrect parameters");
  $ToSend = "New crash report for revision ".$_REQUEST["rev"]." uploaded:\n";
  $FoundFile = False;
  if (!is_dir('crashes')) {
    mkdir('crashes');
  }
  foreach($_FILES as $file)
  {
    $ToSend .= "http://lewin.hodgman.id.au/kam_remake_master_server/crashes/".$file['name']."\n";
    $uploadfile = "crashes/".$file['name'];
    move_uploaded_file($file['tmp_name'], $uploadfile);
    $FoundFile = True;
  }
  if($FoundFile)
  {
    $headers = 'Content-Type: text/plain; charset="iso-8859-2"'."\n".
             "Content-Transfer-Encoding: 8bit"."\n".
			 "From: KaM Remake Server <contact@kamremake.com>"."\n";
    mail ("contact@kamremake.com", "Crash Report ".$_REQUEST["rev"], $ToSend, $headers);
	echo "Success";
  }
?>