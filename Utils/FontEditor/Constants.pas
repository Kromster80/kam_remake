unit Constants;
{$I ..\..\KaM_Remake.inc}
interface
uses Classes, Graphics, Controls, Forms,  KromUtils;


{Palettes}
const
 //Palette filename corresponds with pal_**** constant, except pal_lin which is generated proceduraly (filename doesn't matter for it)
 PalFiles: array[1..12]of string = (
   'map.bbm', 'pal0.bbm', 'pal1.bbm', 'pal2.bbm', 'pal3.bbm', 'pal4.bbm',
   'pal5.bbm', 'setup.bbm', 'setup2.bbm', 'map.bbm', 'mapgold.lbm', 'setup.lbm');
   pal_map=1; pal_0=2; pal_1=3; pal_2=4; pal_3=5; pal_4=6;
   pal_5=7; pal_set=8; pal_set2=9; pal_lin=10; pal2_mapgold=11; pal2_setup=12;


{Fonts}
type //Indexing should start from 1.
  TKMFont = ( fnt_Nil=0,
   fnt_Adam,     fnt_Antiqua,  fnt_Briefing, fnt_Font01,      fnt_Game,
   fnt_Grey,     fnt_KMLobby0, fnt_KMLobby1, fnt_KMLobby2,    fnt_KMLobby3,
   fnt_KMLobby4, fnt_MainA,    fnt_MainB,    fnt_MainMapGold, fnt_Metal,
   fnt_Mini,     fnt_Minimum,  fnt_Outline,  fnt_System,      fnt_Won,
   fnt_Unicode,  fnt_UnicodeGame);

const
  FontFileNames: array[TKMFont] of string = ( 'nil',
    'adam', 'antiqua', 'briefing', 'font01', 'game', 'grey', 'kmlobby0', 'kmlobby1', 'kmlobby2', 'kmlobby3',
    'kmlobby4', 'maina', 'mainb', 'mainmapgold', 'metal', 'mini', 'mininum','outline', 'system', 'won',
    'unicode', 'unicode_game');

var
  FontPal: array [TKMFont] of byte = ( 10, //Those 10 are unknown Pal, no existing Pal matches them well
   10, 2, 1,10, 2, 2, 12,12,12,12,
   12, 8,10,11, 2, 8,  8, 2,10, 9,
   12, 2); //@Krom: Can this be loaded from the file? It would make it easier and more versatile.


implementation

end.
