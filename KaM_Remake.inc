//WDC/FPC defines used for RTL-specific cases
//If you use any Delphi compiler that is not listed here - you need to add it
{$IFDEF VER140} {$DEFINE WDC} {$ENDIF} //Define Windows Delphi 6 compiler
{$IFDEF VER150} {$DEFINE WDC} {$ENDIF} //Define Windows Delphi 7 compiler
{$IFDEF VER220} {$DEFINE WDC} {$ENDIF} //Define Windows Delphi XE compiler
{$IFDEF VER230} {$DEFINE WDC} {$ENDIF} //Define Windows Delphi XE2 compiler
{$IFDEF VER240} {$DEFINE WDC} {$ENDIF} //Define Windows Delphi XE3 compiler
{.$DEFINE FPC} //Define Free Pascal compiler (defined automatically by Lazarus)

{$IFDEF FPC}
  //Set FPC to work in Delphi mode
  {$MODE Delphi}
{$ENDIF}

{$IFDEF WDC}
  //Project can be compiled without madExcept features (if you dont have madExcept installed)
  {$DEFINE USE_MAD_EXCEPT}
{$ENDIF}