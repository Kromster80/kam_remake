<?php
include("serverlib.php");
global $MAIN_VERSION;

$Lang = $_REQUEST["lang"];
$Rev = $_REQUEST["rev"];

if($Rev == "r2722")
{
  die("Thank you for helping me test :)");
}

//First see if they are up to date
if($Rev != $MAIN_VERSION)
{
	echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!|~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|";
	switch($Lang)
	{
		case 'spa':
			echo "¡La version del kam Remake está desactualizada! Estás ejecutando ".$Rev." pero la versión más reciente es ".$MAIN_VERSION.".||Por favor bajate la actualizacion en: www.kamremake.com";
			//No puedes jugar en multijugador hasta que no actualices.
			break;
		case 'ita':
			echo "La tua versione di \"KaM Remake\" non è aggiornata! Stai utilizzando la versione ".$Rev.", mentre la più recente è ".$MAIN_VERSION.".||Puoi scaricare l'aggiornamento dal sito: www.kamremake.com.";
			//Non potrai giocare online prima di aver aggiornato il programma.
			break;
		case 'ptb':
			echo "Sua versão do KaM Remake está desatualizada! Você está executando ".$Rev." mas a versão mais recente é ".$MAIN_VERSION.".|| Por favor, baixe a atualização em: www.kamremake.com";
			//Você não pode jogar online até que atualize seu jogo.
			break;
		case 'hun':
			echo "A KaM Remake verziód túl régi! Te a ".$Rev." verziót futtatod, miközben a ".$MAIN_VERSION." verzió a legújabb.||Kérlek töltsd le a játék frissítését a hivatalos oldalon: www.kamremake.com";
			//Nem játszhatsz interneten, amíg nem frissíted a játékodat.
			break;
		case 'rus':
			echo "Âàøà âåðñèÿ èãðû óñòàðåëà! Âû èñïîëüçóåòå âåðñèþ ".$Rev." òîãäà êàê ïîñëåäíÿÿ äîñòóïíàÿ âåðñèÿ - ".$MAIN_VERSION.".||Ïîæàëóéñòà ñêà÷àéòå åå ñ ñàéòà: www.kamremake.com";
			//Âû íå ìîæåòå èãðàòü îíëàéí ïîêà íå îáíîâèòå ñâîþ âåðñèþ.
			break;
		case 'cze':
			echo "Máte zastaralou verzi KaM Remake! Používáte verzi ".$Rev.", ale nejnovìjší verze je ".$MAIN_VERSION.".||Prosím, stáhnìte si aktualizaci na: www.kamremake.com";
			//Nemùžete hrát online dokud neaktualizujete.
			break;
		case 'fre':
			echo "Votre version de KaM Remake n'est pas mise à jour ! Vous avez la version ".$Rev." mais la version la plus récente est la ".$MAIN_VERSION.".||Veuillez télécharger la mise à jour sur: www.kamremake.com";
			//Vous ne pouvez pas jouer en ligne tant que vous n'avez pas mis à jour votre version.
			break;
		case 'pol':
			echo "Twoja wersja KaM Remake jest nieaktualna! U¿ywasz ".$Rev." ale najnowsz¹ jest ".$MAIN_VERSION.".||Proszê pobraæ aktualizacjê ze strony: www.kamremake.com";
			//Nie mo¿esz graæ online zanim nie zaktualizujesz swojej wersji gry.
			break;
		case 'dut':
			echo "Uw KaM Remake versie is niet de nieuwste. U draait ".$Rev." maar de meest recente versie is ".$MAIN_VERSION.".||U kunt de nieuwste versie downloaden van: www.kamremake.com";
			//U kunt niet online spelen totdat u de nieuwste versie heeft geïnstalleerd.
			break;
		case 'swe':
			echo "Du har inte den senaste versionen av KaM Remake! Du kör ".$Rev.", medan den senaste versionen är ".$MAIN_VERSION.".||Ladda ner uppdateringen här: www.kamremake.com";
			//Du kan inte spela online förrän du har uppdaterat.
			break;
		case 'ger':
			echo "Deine Version des Remakes ist veraltet! Du hast ".$Rev.", die neuste ist ".$MAIN_VERSION.".||Bitte lade das neuste Update von www.kamremake.com runter.";
			//Solange du nicht die aktuelle Version hast, kannst du nicht online spielen.
			break;
		default:
			echo "Your KaM Remake version is out of date! You are running ".$Rev." but the most recent version is ".$MAIN_VERSION.".||Please download the update at: www.kamremake.com";
			//You cannot play online until you have updated.
	}
	echo "||~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!";
}
else
{
	echo "Happy New Year! :-)";
	//echo "A KaM Remake Christmas tournament is being organised!||Visit the forum to register: tinyurl.com/KAMCOMP";
	//echo "Welcome to the new version!||Have fun, report problems and spread the word :-)";
	//echo "Use our webchat to organise your games and stay in contact: www.kamremake.com/chat||Server admins: Don't forget to update your servers to r2460 if you haven't already.||Have fun :)";
	//echo "TO THE OWNERS OF THE FOLLOWING SERVERS:|KaM srv from Natodia|[PL] Reborn Army KaM Server||You need to update your servers to r2460 ASAP. (download at www.kamremake.com) Due to bugs in the old server versions there are \"ghost\" players on your server which failed to disconnect properly.|If anyone knows the owners of these servers, please ask them to update. Playing on these servers is not recommended as they are more likely to crash your game.";
	//echo 'There is a new Servers page on the website! Check it out at www.kamremake.com/servers||We have also released an update to the dedicated server that fixes crashes on Linux. Please update your servers as soon as possible to the new version r2460. Thanks to everyone who helped test this server fix.';
	//echo 'WE NEED YOUR HELP!|We are having difficulties with the Linux build of the dedicated server. The servers occasionally crash which stops all games running on them. The following servers are running a new unreleased fix (r2446) which we are testing for release:| - [DE] KaM Remake Server| - Linux r2446 Server| - Jecy\'s r2446 Dedicated Server|Please help us by playing in these servers as much as possible until further notice. This will help us assess whether the crashes are fixed. Thanks! :)';
	/*switch($Lang)
	{
		case 'ger':
			echo "Willkommen bei Knights and Merchants Remake Online!||Jeden Samstag um 21Uhr CET finden Wettkämpfe statt. Seid dabei! Danke für Eure Unterstützung!";
			break;
		default:
			echo "Welcome to the Knights and Merchants Remake online!||Weekly matches are currently run every Saturday at 9pm Central European Time. Please join us if you can!|Thank you for your support!";
	}*/
}
?>
