<?php
function EchoUpdateMessage($Lang, $Rev, $MAIN_VERSION)
{
	echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!|~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|";
	switch($Lang)
	{
		case 'tur':
			echo "Eski bir KaM Remake versiyonu kullanıyorsunuz! ".$Rev." versiyonunu kullanıyorsunuz fakat ".$MAIN_VERSION." versiyonu kullanılabilir durumda.||Lütfen www.kamremake.com adresinden güncellemeyi indiriniz.";
			break;
		case 'jpn':
			echo "アナタのKAMリメイクのバージョンは時代遅れですよ。".$Rev."が実行されましている，しかし".$MAIN_VERSION."バージョンはより最近です。このリンク「www.kamremake.com」からアップデートがダウンロードされましてください。";
			break;
		case 'bel':
			echo "Ваша версія KaM Remake ўстарэла! Вы выкарыстоўваеце ".$Rev.", а самая апошняя версія ".$MAIN_VERSION.". ||Калі ласка загрузіце абнаўленне па адрасе: www.kamremake.com";
			break;
		case 'nor':
			echo "Din versjon av KaM Remake er utdatert! Du kjører ".$Rev." men den mest nylige versjonen er ".$MAIN_VERSION.".||Vennligst last ned oppdateringen på: www.kamremake.com";
			break;
		case 'ukr':
			echo "Ваша ".$Rev." версія KaM Remake застаріла! Завантажте нову ".$MAIN_VERSION." модифікацію на сайті: www.kamremake.com";
			break;
		case 'rom':
			echo "Versiunea ta de KaM Remake este expirată! Acum rulezi ".$Rev." dar cea mai recentă versiune este ".$MAIN_VERSION.".||Te rugăm să descarci update-ul de la www.kamremake.com";
			break;
		case 'svk':
			echo "Vaša verzia KaM Remake je zastaralá! Máte spustenú verziu ".$Rev.", ale posledná verzia je ".$MAIN_VERSION.".||Prosím, stiahnite si aktualizáciu na stránke: www.kamremake.com";
			break;
		case 'est':
			echo "Teie KaM Remake versioon on vana! Teie versioon on ".$Rev." , aga viimane versioon on ".$MAIN_VERSION.".||Palun laadige alla uuendus: www.kamremake.com";
			break;
		case 'bul':
			echo "Версията на играта е с по-стара версия! Вие използвате ".$Rev." ,но се препоръчва последната версия, която е ".$MAIN_VERSION.".||Моля изтеглете ъпдейта от: www.kamremake.com";
			break;
		case 'spa':
			echo "¡La version del kam Remake está desactualizada! Estás ejecutando ".$Rev." pero la versión más reciente es ".$MAIN_VERSION.".||Por favor bajate la actualizacion en: www.kamremake.com";
			break;
		case 'ita':
			echo "La tua versione di \"KaM Remake\" non è aggiornata! Stai utilizzando la versione ".$Rev.", mentre la più recente è ".$MAIN_VERSION.".||Puoi scaricare l'aggiornamento dal sito: www.kamremake.com.";
			break;
		case 'ptb':
			echo "Sua versão do KaM Remake está desatualizada! Você está executando ".$Rev." mas a versão mais recente é ".$MAIN_VERSION.".|| Por favor, baixe a atualização em: www.kamremake.com";
			break;
		case 'hun':
			echo "A KaM Remake verziód túl régi! Te a ".$Rev." verziót futtatod, miközben a ".$MAIN_VERSION." verzió a legújabb.||Kérlek töltsd le a játék frissítését a hivatalos oldalon: www.kamremake.com";
			break;
		case 'rus':
			echo "Ваша версия игры устарела! Вы используете версию ".$Rev." тогда как последняя доступная версия - ".$MAIN_VERSION.".||Пожалуйста скачайте ее с сайта: www.kamremake.com";
			break;
		case 'cze':
			echo "Máte zastaralou verzi KaM Remake! Používáte verzi ".$Rev.", ale nejnovější verze je ".$MAIN_VERSION.".||Prosím, stáhněte si aktualizaci na: www.kamremake.com";
			break;
		case 'fre':
			echo "La version de KaM Remake que vous utilisez n'est pas mise à jour !|Vous avez la version ".$Rev." alors que la version la plus récente est la ".$MAIN_VERSION.".||Veuillez télécharger la mise à jour sur www.kamremake.com.";
			break;
		case 'pol':
			echo "Twoja wersja KaM Remake jest nieaktualna! Używasz ".$Rev." ale najnowszą jest ".$MAIN_VERSION.".||Proszę pobrać aktualizację ze strony: www.kamremake.com";
			break;
		case 'dut':
			echo "Uw KaM Remake versie is niet de nieuwste. U draait ".$Rev." maar de meest recente versie is ".$MAIN_VERSION.".||U kunt de nieuwste versie downloaden van: www.kamremake.com";
			break;
		case 'swe':
			echo "Du har inte den senaste versionen av KaM Remake! Du kör ".$Rev.", medan den senaste versionen är ".$MAIN_VERSION.".||Ladda ner uppdateringen här: www.kamremake.com";
			break;
		case 'ger':
			echo "Deine Version des Remakes ist veraltet! Du hast ".$Rev.", die neuste ist ".$MAIN_VERSION.".||Bitte lade das neuste Update von www.kamremake.com runter.";
			break;
		default:
			echo "Your KaM Remake version is out of date! You are running ".$Rev." but the most recent version is ".$MAIN_VERSION.".||Please download the update at: www.kamremake.com";
	}
	echo "||~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!";
}
?>