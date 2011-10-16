<?
include("serverlib.php");
global $GAME_VERSION;

$Lang = $_REQUEST["lang"];
$Rev = $_REQUEST["rev"];

//First see if they are up to date
if($Rev != $GAME_VERSION)
{
	switch($Lang)
	{
		/*case 'ger':
			echo "";
			break;*/
		case 'cze':
			echo "Máte zastaralou verzi KaM Remake! Používáte verzi ".$Rev.", ale nejnovìjší verze je ".$GAME_VERSION.".|Nemùžete hrát online dokud neaktualizujete.||Prosím, stáhnìte si aktualizaci na: www.kamremake.com";
			break;
		case 'fre':
			echo "Votre version de KaM Remake n'est pas mise à jour ! Vous avez la version ".$Rev." mais la version la plus récente est la ".$GAME_VERSION.".|Vous ne pouvez pas jouer en ligne tant que vous n'avez pas mis à jour votre version.||Veuillez télécharger la mise à jour sur: www.kamremake.com";
			break;
		case 'pol':
			echo "Twoja wersja KaM Remake jest nieaktualna! U¿ywasz ".$Rev." ale najnowsz¹ jest ".$GAME_VERSION.".|Nie mo¿esz graæ online zanim nie zaktualizujesz swojej wersji gry.||Proszê pobraæ aktualizacjê ze strony: www.kamremake.com";
			break;
		case 'dut':
			echo "Uw KaM Remake versie is niet de nieuwste. U draait ".$Rev." maar de meest recente versie is ".$GAME_VERSION.".|U kunt niet online spelen totdat u de nieuwste versie heeft geïnstalleerd.||U kunt de nieuwste versie downloaden van: www.kamremake.com";
			break;
		case 'swe':
			echo "Du har inte den senaste versionen av KaM Remake! Du kör ".$Rev.", medan den senaste versionen är ".$GAME_VERSION.".|Du kan inte spela online förrän du har uppdaterat.||Ladda ner uppdateringen här: www.kamremake.com";
			break;
		case 'ger':
			echo "Deine Version des Remakes ist veraltet! Du hast ".$Rev.", die neuste ist ".$GAME_VERSION.".|Solange du nicht die aktuelle Version hast, kannst du nicht online spielen.||Bitte lade das neuste Update von www.kamremake.com runter.";
			break;
		default:
			echo "Your KaM Remake version is out of date! You are running ".$Rev." but the most recent version is ".$GAME_VERSION.".|You cannot play online until you have updated.||Please download the update at: www.kamremake.com";
	}
}
else
{
	switch($Lang)
	{
		/*case 'ger':
			echo "";
			break;*/
		default:
			echo "Welcome to the Knights and Merchants Remake online!||Weekly matches are currently run every Saturday at 9pm Central European Time. Please join us if you can!|Thank you for your support!";
	}
}
?>
