<?php
function EchoUpdateMessage($Lang, $Rev, $MAIN_VERSION)
{
	echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!|~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|";
	switch($Lang)
	{
		case 'tur':
			echo "Eski bir KaM Remake versiyonu kullanэyorsunuz! ".$Rev." versiyonunu kullanэyorsunuz fakat ".$MAIN_VERSION." versiyonu kullanэlabilir durumda.||Lьtfen www.kamremake.com adresinden gьncellemeyi indiriniz.";
			break;
		case 'jpn':
			echo "Anata no KAM Remake no ba-jon wa jidaiokure desu yo!Ѓ@".$Rev." ga jikkou saremashite iru, shikashi ".$MAIN_VERSION." ba-jon wa yori saikin desu. Kono rinku (www.kamremake.com) kara appude-to ga daunro-do saremashite kudasai.";
			break;
		case 'bel':
			echo "Ваша версія KaM Remake ўстарэла! Вы выкарыстоўваеце ".$Rev.", а самая апошняя версія ".$MAIN_VERSION.". ||Калі ласка загрузіце абнаўленне па адрасе: www.kamremake.com";
			break;
		case 'nor':
			echo "Din versjon av KaM Remake er utdatert! Du kjшrer ".$Rev." men den mest nylige versjonen er ".$MAIN_VERSION.".||Vennligst last ned oppdateringen pе: www.kamremake.com";
			break;
		case 'ukr':
			echo "Ваша ".$Rev." версія KaM Remake застаріла! Завантажте нову ".$MAIN_VERSION." модифікацію на сайті: www.kamremake.com";
			break;
		case 'rom':
			echo "Versiunea ta de KaM Remake este expiratг! Acum rulezi ".$Rev." dar cea mai recentг versiune este ".$MAIN_VERSION.".||Te rugгm sг descarci update-ul de la www.kamremake.com";
			break;
		case 'svk':
			echo "Vaљa verzia KaM Remake je zastaralб! Mбte spustenъ verziu ".$Rev.", ale poslednб verzia je ".$MAIN_VERSION.".||Prosнm, stiahnite si aktualizбciu na strбnke: www.kamremake.com";
			break;
		case 'est':
			echo "Teie KaM Remake versioon on vana! Teie versioon on ".$Rev." , aga viimane versioon on ".$MAIN_VERSION.".||Palun laadige alla uuendus: www.kamremake.com";
			break;
		case 'bul':
			echo "Версията на играта е с по-стара версия! Вие използвате ".$Rev." ,но се препоръчва последната версия, която е ".$MAIN_VERSION.".||Моля изтеглете ъпдейта от: www.kamremake.com";
			break;
		case 'spa':
			echo "ЎLa version del kam Remake estб desactualizada! Estбs ejecutando ".$Rev." pero la versiуn mбs reciente es ".$MAIN_VERSION.".||Por favor bajate la actualizacion en: www.kamremake.com";
			//No puedes jugar en multijugador hasta que no actualices.
			break;
		case 'ita':
			echo "La tua versione di \"KaM Remake\" non и aggiornata! Stai utilizzando la versione ".$Rev.", mentre la piщ recente и ".$MAIN_VERSION.".||Puoi scaricare l'aggiornamento dal sito: www.kamremake.com.";
			//Non potrai giocare online prima di aver aggiornato il programma.
			break;
		case 'ptb':
			echo "Sua versгo do KaM Remake estб desatualizada! Vocк estб executando ".$Rev." mas a versгo mais recente й ".$MAIN_VERSION.".|| Por favor, baixe a atualizaзгo em: www.kamremake.com";
			//Vocк nгo pode jogar online atй que atualize seu jogo.
			break;
		case 'hun':
			echo "A KaM Remake verziуd tъl rйgi! Te a ".$Rev." verziуt futtatod, mikцzben a ".$MAIN_VERSION." verziу a legъjabb.||Kйrlek tцltsd le a jбtйk frissнtйsйt a hivatalos oldalon: www.kamremake.com";
			//Nem jбtszhatsz interneten, amнg nem frissнted a jбtйkodat.
			break;
		case 'rus':
			echo "Ваша версия игры устарела! Вы используете версию ".$Rev." тогда как последняя доступная версия - ".$MAIN_VERSION.".||Пожалуйста скачайте ее с сайта: www.kamremake.com";
			//Вы не можете играть онлайн пока не обновите свою версию.
			break;
		case 'cze':
			echo "Mбte zastaralou verzi KaM Remake! Pouћнvбte verzi ".$Rev.", ale nejnovмjљн verze je ".$MAIN_VERSION.".||Prosнm, stбhnмte si aktualizaci na: www.kamremake.com";
			//Nemщћete hrбt online dokud neaktualizujete.
			break;
		case 'fre':
			echo "La version de KaM Remake que vous utilisez n'est pas mise а jour !|Vous avez la version ".$Rev." alors que la version la plus rйcente est la ".$MAIN_VERSION.".||Veuillez tйlйcharger la mise а jour sur www.kamremake.com.";
			//Vous ne pouvez pas jouer en ligne tant que vous n'avez pas mis а jour votre version.
			break;
		case 'pol':
			echo "Twoja wersja KaM Remake jest nieaktualna! Uїywasz ".$Rev." ale najnowsz№ jest ".$MAIN_VERSION.".||Proszк pobraж aktualizacjк ze strony: www.kamremake.com";
			//Nie moїesz graж online zanim nie zaktualizujesz swojej wersji gry.
			break;
		case 'dut':
			echo "Uw KaM Remake versie is niet de nieuwste. U draait ".$Rev." maar de meest recente versie is ".$MAIN_VERSION.".||U kunt de nieuwste versie downloaden van: www.kamremake.com";
			//U kunt niet online spelen totdat u de nieuwste versie heeft geпnstalleerd.
			break;
		case 'swe':
			echo "Du har inte den senaste versionen av KaM Remake! Du kцr ".$Rev.", medan den senaste versionen дr ".$MAIN_VERSION.".||Ladda ner uppdateringen hдr: www.kamremake.com";
			//Du kan inte spela online fцrrдn du har uppdaterat.
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
?>