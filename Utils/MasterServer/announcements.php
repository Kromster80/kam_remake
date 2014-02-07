<?php
include("consts.php");
global $MAIN_VERSION;

$Lang = $_REQUEST["lang"];
$Rev = $_REQUEST["rev"];
$RevNum = intval(substr($Rev, 1));

if($RevNum > 6097) {
    include('announcements.utf8.php');
} else {
    include('announcements.ansi.php');
}

if(($Rev == "r3374") || ($Rev == "r3252") || ($Rev == "r3311") || ($Rev == "r3812") || ($Rev == "r3870") || ($Rev == "r3967") || ($Rev == "r3985") || ($Rev == "r4125") || ($Rev == "r4297") || ($Rev == "r5057") || ($Rev == "r5116") || ($Rev == "r5349") || ($Rev == "r5459"))
{
  die('[$0000FF]THE SCRIPTING DEMO IS OUT![]||This release candidate is now redundant. Please download the release from www.kamremake.com||Thanks again for your help testing!');
}

if($Rev == "r6157")
{
  die('Welcome to the Winter Tournament Beta!');
}

//First see if they are up to date
if($Rev != $MAIN_VERSION)
{
    EchoUpdateMessage($Lang, $Rev, $MAIN_VERSION); //Will use either ANSI or Unicode based on which file gets included above
}
else
{
    echo 'The community is organising a [$00CC74]2vs2 tournament[]. Show your KaM skill in front of [$FF78A5]live stream audiences[]! Registrations close 5th February so find a partner and enter soon :) More information at [$F8A070]www.kamremake.com[]||Want to socialise with a community of KaM players, or talk with your friends while you play? Join the KaM community teamspeak server! More information at:|[$F8A070]http://kamts.eu[]||Enjoy playing KaM Remake? Please consider [$00EEFF]donating[] to help cover our running costs and support future development. Visit [$F8A070]www.kamremake.com/donations[]';
    //echo 'Want to socialise with a community of KaM players, or talk with your friends while you play? Join the KaM community teamspeak server! More information at:|[$F8A070]http://kamts.eu[]||Don\'t forget to try out some of the special multiplayer missions which use dynamic scripts to create new game modes and objectives.||Enjoy playing KaM Remake? Please consider [$00EEFF]donating[] to help cover our running costs and support future development. Visit [$F8A070]www.kamremake.com/donations[]';
    //echo 'Did you know we have an official forum? Read about the latest developments to the KaM Remake, discuss your ideas and get responses from developers, see the latest fan-made maps, and take part in community events like tournaments and beta testing. Visit our forum at:|[$F8A070]knightsandmerchants.net/forum[]||Don\'t forget to try out some of the special multiplayer missions which use dynamic scripts to create new game modes and objectives.||Enjoy playing KaM Remake? Please consider [$00EEFF]donating[] to help cover our running costs and support future development. Visit [$F8A070]www.kamremake.com/donations[]';
	//echo 'We have upgraded to a [$009BEE]new server[] so you should notice the server list is refreshing faster! Read more at [$F8A070]www.kamremake.com[]||Don\'t forget to try out some of the special multiplayer missions which use dynamic scripts to create new game modes and objectives.||Enjoy playing KaM Remake? Please consider [$00EEFF]donating[] to help cover our running costs and support future development. Visit [$F8A070]www.kamremake.com/donations[]';
	//echo 'The 2 vs 2 Powah Tournament has begun! Go to [$F8A070]www.kamremake.com[] to find out more!||Can you help translate our website [$F8A070]www.kamremake.com[]? We need volunteers to help translate it into any language. Please contact us through our website.';
	//echo 'Powah Tour (Florek, Mulberry, To) are organising a tournament! Go to [$F8A070]www.kamremake.com[] to find out more!||Can you help translate our website [$F8A070]www.kamremake.com[]? We need volunteers to help translate it into any language. Please contact us through our website.';
	//echo '[$FF6EAF]Welcome to the 4th multiplayer demo[] :)|Please report any issues.';
	//echo "We will hopefully be releasing an update to the KaM Remake soon! (around 29th of April if testing goes well)||Check the news story on our website for more information: www.kamremake.com";
	//echo "The recent '500 Internal Server Errors' seem to be resolved, it was a problem at the hosting provider.|Please let us know if you have problems refreshing the server list or these announcements.||Thanks for your patience :)";
	//echo "We're getting some '500 Internal Server Errors' on our master server which we will try to fix as soon as possible.||If you can't see any servers in the list, please press Refresh Server List to try again.||Updated 19/02/2012.";
	//echo "Happy New Year! :-)";
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