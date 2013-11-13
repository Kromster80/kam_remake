<?
  $upload_errors = array(
    UPLOAD_ERR_OK         => "No errors",
    UPLOAD_ERR_INI_SIZE   => "Larger than upload_max_filesize",
    UPLOAD_ERR_FORM_SIZE  => "Larger than form MAX_FILE_SIZE",
    UPLOAD_ERR_PARTIAL    => "Partial upload",
    UPLOAD_ERR_NO_FILE    => "No file",
    UPLOAD_ERR_NO_TMP_DIR => "No temporary directory",
    UPLOAD_ERR_CANT_WRITE => "Can't write to disk",
    UPLOAD_ERR_EXTENSION  => "File upload stopped by extension",
    UPLOAD_ERR_EMPTY      => "File is empty" // add this to avoid an offset
  );

  if($_REQUEST["rev"] == "") die("Incorrect parameters");
  $ToSend = "New crash report for revision ".$_REQUEST["rev"]." uploaded:\n";
  $FoundFile = False;
  $Failed = False;
  if (!is_dir('crashes')) {
    mkdir('crashes');
  }
  foreach($_FILES as $file)
  {
    $ToSend .= "http://kam.hodgman.id.au/crashes/".$file['name'];
	$ToSend .= " Size: ".$file['size']." Status: ".$file['error']." (".$upload_errors[$file['error']].") IP: ".$_SERVER['REMOTE_ADDR'];
    $uploadfile = "crashes/".$file['name'];
    if(!move_uploaded_file($file['tmp_name'], $uploadfile))
	{
	  $Failed = True;
	  $ToSend .= " MOVE FAILED!";
	}
	$ToSend .= "\n";
    $FoundFile = True;
  }
  if($Failed || !$FoundFile)
  {
    header("HTTP/1.1 500 Upload Failed");
	echo "ERROR: Upload failed";
  }
  
  //Now try to paste the bug report file into the email body
  if(!$Failed)
  {
	$zip = new ZipArchive;
    $res = $zip->open($uploadfile);
    if ($res === TRUE) {
      $zip->extractTo("crashes/", "bugreport.txt");
      $zip->close();
	  if(file_exists("crashes/bugreport.txt")) {
		$file_text = file_get_contents("crashes/bugreport.txt");
		$file_text = str_replace("\r\n", "\n", $file_text);
		$file_text = str_replace("\r", "\n", $file_text);
		$ToSend .= "\n\n\n".$file_text;
	  }
	  unlink("crashes/bugreport.txt");
	}
  }
  
  if($FoundFile)
  {
    $headers = 'Content-Type: text/plain; charset="iso-8859-2"'."\n".
             "Content-Transfer-Encoding: 8bit"."\n".
			 "From: KaM Remake Server <contact@kamremake.com>"."\n";
    mail ("contact@kamremake.com,lewin@hodgman.id.au", "Crash Report ".$_REQUEST["rev"], $ToSend, $headers);
	echo "Done";
  }
?>