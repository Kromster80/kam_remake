//WDC/FPC defines used for RTL-specific cases
//If you use any Delphi compiler that is not listed here - you need to add it
{$IFDEF VER140} {$DEFINE WDC} {$ENDIF} //Define Windows Delphi 6 compiler
{$IFDEF VER150} {$DEFINE WDC} {$ENDIF} //Define Windows Delphi 7 compiler
{$IFDEF VER220} {$DEFINE WDC} {$ENDIF} //Define Windows Delphi XE compiler
{$IFDEF VER230} {$DEFINE WDC} {$ENDIF} //Define Windows Delphi XE2 compiler
{$IFDEF VER240} {$DEFINE WDC} {$ENDIF} //Define Windows Delphi XE3 compiler
{$IFDEF VER250} {$DEFINE WDC} {$ENDIF} //Define Windows Delphi XE4 compiler
{$IFDEF VER260} {$DEFINE WDC} {$ENDIF} //Define Windows Delphi XE5 compiler
{$IFDEF VER270} {$DEFINE WDC} {$ENDIF} //Define Windows Delphi XE6 compiler
{$IFDEF VER280} {$DEFINE WDC} {$ENDIF} //Define Windows Delphi XE7 compiler
{$IFDEF VER290} {$DEFINE WDC} {$ENDIF} //Define Windows Delphi XE8 compiler
{$IFDEF VER300} {$DEFINE WDC} {$ENDIF} //Define Windows Delphi 10 Seattle compiler
{$IFDEF VER310} {$DEFINE WDC} {$ENDIF} //Define Windows Delphi 10.1 Berlin compiler
{$IFDEF VER320} {$DEFINE WDC} {$ENDIF} //Define Windows Delphi 10.2 Tokyo compiler
{.$DEFINE FPC} //Define Free Pascal compiler (defined automatically by Lazarus)

//Lazarus compiler specific settings
{$IFDEF FPC}
  //Set FPC to work in Delphi mode
  {$MODE Delphi}

  //Currently FPC does not define UNICODE define because in Delphi it means more than just a
  //default string type but a full RTL implementation : using Windows 'W' functions instead of 'A',
  //another class declarations(TStringStream for example).
  {$IFDEF FPC_UNICODESTRINGS}
    {$DEFINE UNICODE}
  {$ENDIF}
{$ENDIF}

//Delphi compiler specific settings
{$IFDEF WDC}
  //Project can be compiled without madExcept features (if you dont have madExcept installed)
  {$DEFINE USE_MAD_EXCEPT}
{$ENDIF}

//Use secure authentication unit (KM_NetAuthSecure)
{$DEFINE USESECUREAUTH}
