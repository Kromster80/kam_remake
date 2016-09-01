{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Version defines for ICS (Internet Component Suite)
Creation:     2003
Version:      8.09
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1996-2016 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
              <francois.piette@overbyte.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

History:
May 31, 2004 V1.01 Correctly defined COMPILERX_UP symbols
Sep 07, 2004 V1.02 Updated for Delphi 9/W32
Mar 11, 2005 V1.03 Marco (marcov@stack.nl) added some defines for better FPC
                   support
Mar 13, 2005 V1.04 Changed conditional symbol for streams supporting 64 bits
                   seek.
Jul 30, 2005 V1.05 Updated for Delphi 10. Not tested !
Sept 2, 2005 V1.06 64-bit streams are Delphi 6 and later
Oct 05, 2008 V1.09 USE_LINE for COMPILER10_UP, removed UNICODE check for .NET.
Sep 21, 2010 V1.10 Arno corrected VER180/VER185 and added VER190 and VER160.
                   If the compiler version is not detected define latest
                   supported compiler version.
Sep 24, 2010 V1.11 Arno removed BCB7/C++BuilderX.
Oct 23, 2010 V1.12 PasDoc compatible.
Feb 4,  2011 V1.13 BUILTIN_TIMEOUT and BUILTIN_THROTTLE now enabled by default
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory
Apr 2013 - V8.01 - Arno XE4
Aug 2013 - V8.02 - FPiette XE5
Apr 2014 - V8.03 - Arno Latest supported compiler version fall back updated
Aug 2014 - V8.04 - Angus XE7
Nov 8,  2014 V8.05 Eugene Kotlyarov added namespace for all RTL units for XE2 and
                    later, using new define RTL_NAMESPACES
Feb 2015 - V8.06 - Angus XE8
Jun 2015 - V8.07 - Angus XE9
Aug 2015 - V8.08 - Angus XE9 is now Delphi 10 Seattle
Apr 2016 - V8.09 - Angus Delphi 10.1 and 10.2, USE_SSL now enabled


  SYMBOL           Compiler version
  ===============  ======================================
  DELPHI1          Delphi 1
  DELPHI2          Delphi 2
  DELPHI3          Delphi 3
  DELPHI4          Delphi 4
  DELPHI5          Delphi 5
  DELPHI6          Delphi 6
  DELPHI7          Delphi 7
  DELPHI8          Delphi 8
  DELPHI9          Delphi 2005
  DELPHI10         Delphi 2006
  DELPHI11         Delphi 2007
  DELPHI12         Delphi 2009
  DELPHI14         Delphi 2010
  DELPHI15         Delphi XE
  DELPHI16         Delphi XE2
  DELPHI1_UP       Delphi 1  or higher
  DELPHI2_UP       Delphi 2  or higher
  DELPHI3_UP       Delphi 3  or higher
  DELPHI4_UP       Delphi 4  or higher
  DELPHI5_UP       Delphi 5  or higher
  DELPHI6_UP       Delphi 6  or higher
  DELPHI7_UP       Delphi 7  or higher
  DELPHI8_UP       Delphi 8  or higher  (Delphi 8 is .NET only)
  DELPHI9_UP       Delphi 9  or higher  (Delphi 2005 - Diamondback, has W32 compiler)
  DELPHI10_UP      Delphi 10 or higher  (Delphi 2006 - Dexter)
  DELPHI11_UP      Delphi 11 or higher  (Delphi 2007 - Spacely)
  DELPHI12_UP      Delphi 12 or higher  (Delphi 2009 - Tiburon)
  DELPHI14_UP      Delphi 14 or higher  (Delphi 2010 - Weaver)
  DELPHI15_UP      Delphi 15 or higher  (Delphi XE   - Fulcrum)
  DELPHI16_UP      Delphi 16 or higher  (Delphi XE2  - Pulsar)
  DELPHI17_UP      Delphi 17 or higher  (Delphi XE3  - WaterDragon)
  DELPHI18_UP      Delphi 18 or higher  (Delphi XE4  - Quitessence)
  DELPHI19_UP      Delphi 19 or higher  (Delphi XE5  - Zephyr)
  DELPHI20_UP      Delphi 20 or higher  (Delphi XE6  - Proteus)
  DELPHI21_UP      Delphi 21 or higher  (Delphi XE7  - Carpathia)
  DELPHI22_UP      Delphi 22 or higher  (Delphi XE8  - Elbrus)
  DELPHI23_UP      Delphi 23 or higher  (Delphi 10 Seattle - Aitana)
  DELPHI24_UP      Delphi 24 or higher  (Delphi 10.1 Berlin - BigBen)
  DELPHI25_UP      Delphi 25 or higher  (Delphi 10.2 - Godzilla)

  BCB1             C++ Builder 1
  BCB3             C++ Builder 3
  BCB4             C++ Builder 4
  BCB5             C++ Builder 5
  BCB6             C++ Builder 6
  BCB1_UP          C++ Builder 1  or higher
  BCB3_UP          C++ Builder 3  or higher
  BCB4_UP          C++ Builder 4  or higher
  BCB5_UP          C++ Builder 5  or higher
  BCB6_UP          C++ Builder 6  or higher
  BCB10_UP         C++ Builder 2006 or higher
  BCB11_UP         C++ Builder 2007 or higher
  BCB12_UP         C++ Builder 2009 or higher
  BCB14_UP         C++ Builder 2010 or higher
  BCB15_UP         C++ Builder XE   or higher
  BCB16_UP         C++ Builder XE2  or higher
  BCB17_UP         C++ Builder XE3  or higher
  BCB18_UP         C++ Builder XE4  or higher
  BCB19_UP         C++ Builder XE5  or higher
  BCB20_UP         C++ Builder XE6  or higher
  BCB21_UP         C++ Builder XE7  or higher
  BCB22_UP         C++ Builder XE8  or higher
  BCB23_UP         C++ Builder 10 Seattle  or higher

  COMPILER1        Delphi 1
  COMPILER2        Delphi 2  or C++ Builder 1
  COMPILER3        Delphi 3
  COMPILER35       C++ Builder 3
  COMPILER4        Delphi 4  or C++ Builder 4
  COMPILER5        Delphi 5  or C++ Builder 5
  COMPILER6        Delphi 6  or C++ Builder 6
  COMPILER7        Delphi 7  (or C++ Builder X ?)
  COMPILER8        Delphi 8
  COMPILER9        Delphi 9
  COMPILER10       Delphi or C++ Builder 2006
  COMPILER11       Delphi or C++ Builder 2007
  COMPILER12       Delphi or C++ Builder 2009
  COMPILER14       Delphi or C++ Builder 2010
  COMPILER15       Delphi or C++ Builder XE
  COMPILER16       Delphi or C++ Builder XE2
  COMPILER17       Delphi or C++ Builder XE3
  COMPILER18       Delphi or C++ Builder XE4
  COMPILER19       Delphi or C++ Builder XE5
  COMPILER20       Delphi or C++ Builder XE6
  COMPILER21       Delphi or C++ Builder XE7
  COMPILER22       Delphi or C++ Builder XE8
  COMPILER23       Delphi or C++ Builder 10 Seattle
  COMPILER24       Delphi or C++ Builder 10.1
  COMPILER25       Delphi or C++ Builder 10.2

  COMPILER1_UP     Delphi 1  or higher
  COMPILER2_UP     Delphi 2  or C++ Builder 1 or higher
  COMPILER3_UP     Delphi 3  or higher
  COMPILER35_UP    C++ Builder 3 or higher
  COMPILER4_UP     Delphi 4  or C++ Builder 4 or higher
  COMPILER5_UP     Delphi 5  or C++ Builder 5 or higher
  COMPILER6_UP     Delphi 6  or C++ Builder 6 or higher
  COMPILER7_UP     Delphi 7  or higher
  COMPILER8_UP     Delphi 8  or higher
  COMPILER9_UP     Delphi 9  or higher
  COMPILER10_UP    Delphi or C++ Builder 2006 or higher
  COMPILER11_UP    Delphi or C++ Builder 2007 or higher (Highlander)
  COMPILER12_UP    Delphi or C++ Builder 2009 or higher (Tiburon)
  COMPILER14_UP    Delphi or C++ Builder 2010 or higher (Weaver)
  COMPILER15_UP    Delphi or C++ Builder XE   or higher (Fulcrum)
  COMPILER16_UP    Delphi or C++ Builder XE2  or higher (Pulsar)
  COMPILER17_UP    Delphi or C++ Builder XE3  or higher (WaterDragon)
  COMPILER18_UP    Delphi or C++ Builder XE4  or higher (Quitessence)
  COMPILER19_UP    Delphi or C++ Builder XE5  or higher (Zephyr)
  COMPILER20_UP    Delphi or C++ Builder XE6  or higher (Proteus)
  COMPILER21_UP    Delphi or C++ Builder XE7  or higher (Carpathia)
  COMPILER22_UP    Delphi or C++ Builder XE8  or higher (Elbrus)
  COMPILER23_UP    Delphi or C++ Builder 10 Seattle or higher (Aitana)
  COMPILER24_UP    Delphi or C++ Builder 10.1 Berlin or higher (BigBen)
  COMPILER25_UP    Delphi or C++ Builder 10.2 or higher (Godzilla)

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

{ User defines: Enable or disable these defines depending on how you want    }
{ your ICS Packages to be compiled.                                          }

{ If NO_ADV_MT is defined, then there is less multithread code compiled.     }
{.$DEFINE NO_ADV_MT}

{ Uncomment next line to always include SSL code into the binary.            }
{ In most cases leave it commented and define USE_SSL in the project options }
{ if required.                                                               }
{$DEFINE USE_SSL}

{ Next line specifies to link the ZLIB .obj files.                           }
{$DEFINE ZLIB_OBJ}

{ Next symbol affects D2010 and later only. If defined adds RTTI directive   }
{ "$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])" to all units which  }
{ disables all the inherited RTTI options and sets the new ones to not emit  }
{ any extended RTTI information.                                             }
{.$DEFINE NO_EXTENDED_RTTI}

{ Next symbols include extra features.                                       }
{$DEFINE BUILTIN_TIMEOUT}
{$DEFINE BUILTIN_THROTTLE}


{ Do not change anything below this line !                                   }
{----------------------------------------------------------------------------}
{$DEFINE VER_UNKNOWN}

{$IFDEF FPC}
  {$MODE Delphi}                           { delphi mode            }
  {$H+}                                    { Use long strings       }
  {$ASMMODE Intel}                         { Intel syntax assembler }
  {$DEFINE USEWINDOWS}                     { Use "Windows" unit.    }
  {$DEFINE NOFORMS}                        { No unit forms.         }
  {$DEFINE SOCKETULONGMUSTMATCH}
  // With FPC, relying directly on Delphi version is evil.
  // (http://www.stack.nl/~marcov/porting.pdf)
  // However FPC 1.9.x is more D6/D7 than previous, so
  // we have to define something, sigh!
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}

  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
  {$DEFINE COMPILER5_UP}
  {$DEFINE COMPILER4_UP}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$DEFINE VER_UNKNOWN}

{$IFDEF VER320}
  {$UNDEF VER_UNKNOWN}
  {$DEFINE COMPILER25}
  {$IFDEF BCB}
    {$DEFINE BCB25}
  {$ELSE}
    {$DEFINE DELPHI25}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER310}
  {$UNDEF VER_UNKNOWN}
  {$DEFINE COMPILER24}
  {$IFDEF BCB}
    {$DEFINE BCB24}
  {$ELSE}
    {$DEFINE DELPHI24}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER300}
  {$UNDEF VER_UNKNOWN}
  {$DEFINE COMPILER23}
  {$IFDEF BCB}
    {$DEFINE BCB23}
  {$ELSE}
    {$DEFINE DELPHI23}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER290}
  {$UNDEF VER_UNKNOWN}
  {$DEFINE COMPILER22}
  {$IFDEF BCB}
    {$DEFINE BCB22}
  {$ELSE}
    {$DEFINE DELPHI22}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER280}
  {$UNDEF VER_UNKNOWN}
  {$DEFINE COMPILER21}
  {$IFDEF BCB}
    {$DEFINE BCB21}
  {$ELSE}
    {$DEFINE DELPHI21}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER270}
  {$UNDEF VER_UNKNOWN}
  {$DEFINE COMPILER20}
  {$IFDEF BCB}
    {$DEFINE BCB20}
  {$ELSE}
    {$DEFINE DELPHI20}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER260}
  {$UNDEF VER_UNKNOWN}
  {$DEFINE COMPILER19}
  {$IFDEF BCB}
    {$DEFINE BCB19}
  {$ELSE}
    {$DEFINE DELPHI19}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER250}
  {$UNDEF VER_UNKNOWN}
  {$DEFINE COMPILER18}
  {$IFDEF BCB}
    {$DEFINE BCB18}
  {$ELSE}
    {$DEFINE DELPHI18}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER240}
  {$UNDEF VER_UNKNOWN}
  {$DEFINE COMPILER17}
  {$IFDEF BCB}
    {$DEFINE BCB17}
  {$ELSE}
    {$DEFINE DELPHI17}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER230}
  {$UNDEF VER_UNKNOWN}
  {$DEFINE COMPILER16}
  {$IFDEF BCB}
    {$DEFINE BCB16}
  {$ELSE}
    {$DEFINE DELPHI16}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER220}
  {$UNDEF VER_UNKNOWN}
  {$DEFINE COMPILER15}
  {$IFDEF BCB}
    {$DEFINE BCB15}
  {$ELSE}
    {$DEFINE DELPHI15}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER210}
  {$UNDEF VER_UNKNOWN}
  {$DEFINE COMPILER14}
  {$IFDEF BCB}
    {$DEFINE BCB14}
  {$ELSE}
    {$DEFINE DELPHI14}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER200}
  {$UNDEF VER_UNKNOWN}
  {$DEFINE COMPILER12}
  {$IFDEF BCB}
    {$DEFINE BCB12}
  {$ELSE}
    {$DEFINE DELPHI12}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER190} {Delphi 2007 for .NET}
  {$UNDEF VER_UNKNOWN}
  {$DEFINE COMPILER11}
  {$DEFINE DELPHI11}
  {$DEFINE DELPHI}
{$ENDIF}

{$IFDEF VER180} { Delphi 2006 or 2007 }
  {$UNDEF VER_UNKNOWN}
  {$IFDEF VER185} { Delphi 2007 }
    {$DEFINE COMPILER11}
    {$IFDEF BCB}
      {$DEFINE BCB11}
    {$ELSE}
      {$DEFINE DELPHI11}
      {$DEFINE DELPHI}
    {$ENDIF}
  {$ELSE}         { Delphi 2006 }
    {$DEFINE COMPILER10}
    {$IFDEF BCB}
      {$DEFINE BCB10}
    {$ELSE}
      {$DEFINE DELPHI10}
      {$DEFINE DELPHI}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER170}
  {$UNDEF VER_UNKNOWN}
  {$DEFINE COMPILER9}
  {$DEFINE DELPHI9}
  {$DEFINE DELPHI}
{$ENDIF}

{$IFDEF VER160}  { Delphi 8 for .NET }
  {$UNDEF VER_UNKNOWN}
  {$DEFINE COMPILER8}
  {$DEFINE DELPHI8}
  {$DEFINE DELPHI}
{$ENDIF}

{$IFDEF VER150}
  {$UNDEF VER_UNKNOWN}
  {$DEFINE COMPILER7}
  {$DEFINE DELPHI7}
  {$DEFINE DELPHI}
{$ENDIF}

{$IFDEF VER140}
  {$UNDEF VER_UNKNOWN}
  {$DEFINE COMPILER6}
  {$IFDEF BCB}
    {$DEFINE BCB6}
  {$ELSE}
    {$DEFINE DELPHI6}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER130}
  {$UNDEF VER_UNKNOWN}
  {$DEFINE COMPILER5}
  {$IFDEF BCB}
    {$DEFINE BCB5}
  {$ELSE}
    {$DEFINE DELPHI5}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER125}
  {$UNDEF VER_UNKNOWN}
  {$DEFINE COMPILER4}
  {$DEFINE BCB4}
  {$DEFINE BCB}
{$ENDIF}

{$IFDEF VER120}
  {$UNDEF VER_UNKNOWN}
  {$DEFINE COMPILER4}
  {$DEFINE DELPHI4}
  {$DEFINE DELPHI}
{$ENDIF}

{$IFDEF VER110}
  {$UNDEF VER_UNKNOWN}
  {$DEFINE COMPILER35}
  {$DEFINE BCB3}
{$ENDIF}

{$IFDEF VER100}
  {$UNDEF VER_UNKNOWN}
  {$DEFINE COMPILER3}
  {$DEFINE DELPHI3}
  {$DEFINE DELPHI}
{$ENDIF}

{$IFDEF VER93}
  {$UNDEF VER_UNKNOWN}
  {$DEFINE COMPILER2}
  {$DEFINE BCB1}
  {$DEFINE BCB}
{$ENDIF}

{$IFDEF VER90}
  {$UNDEF VER_UNKNOWN}
  {$DEFINE COMPILER2}
  {$DEFINE DELPHI2}
  {$DEFINE DELPHI}
{$ENDIF}

{$IFDEF VER80}
  {$UNDEF VER_UNKNOWN}
  {$DEFINE COMPILER1}
  {$DEFINE DELPHI1}
  {$DEFINE DELPHI}
{$ENDIF}

{$IFDEF VER_UNKNOWN}   { If the compiler version is not detected define latest }
  {$DEFINE COMPILER23} { supported compiler version.                           }
  {$IFDEF BCB}
    {$DEFINE BCB23}
  {$ELSE}
    {$DEFINE DELPHI23}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF DELPHI25}
  {$DEFINE DELPHI25_UP}
  {$DEFINE DELPHI24_UP}
  {$DEFINE DELPHI23_UP}
  {$DEFINE DELPHI22_UP}
  {$DEFINE DELPHI21_UP}
  {$DEFINE DELPHI20_UP}
  {$DEFINE DELPHI19_UP}
  {$DEFINE DELPHI18_UP}
  {$DEFINE DELPHI17_UP}
  {$DEFINE DELPHI16_UP}
  {$DEFINE DELPHI15_UP}
  {$DEFINE DELPHI14_UP}
  {$DEFINE DELPHI12_UP}
  {$DEFINE DELPHI11_UP}
  {$DEFINE DELPHI10_UP}
  {$DEFINE DELPHI9_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI24}
  {$DEFINE DELPHI24_UP}
  {$DEFINE DELPHI23_UP}
  {$DEFINE DELPHI22_UP}
  {$DEFINE DELPHI21_UP}
  {$DEFINE DELPHI20_UP}
  {$DEFINE DELPHI19_UP}
  {$DEFINE DELPHI18_UP}
  {$DEFINE DELPHI17_UP}
  {$DEFINE DELPHI16_UP}
  {$DEFINE DELPHI15_UP}
  {$DEFINE DELPHI14_UP}
  {$DEFINE DELPHI12_UP}
  {$DEFINE DELPHI11_UP}
  {$DEFINE DELPHI10_UP}
  {$DEFINE DELPHI9_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI23}
  {$DEFINE DELPHI23_UP}
  {$DEFINE DELPHI22_UP}
  {$DEFINE DELPHI21_UP}
  {$DEFINE DELPHI20_UP}
  {$DEFINE DELPHI19_UP}
  {$DEFINE DELPHI18_UP}
  {$DEFINE DELPHI17_UP}
  {$DEFINE DELPHI16_UP}
  {$DEFINE DELPHI15_UP}
  {$DEFINE DELPHI14_UP}
  {$DEFINE DELPHI12_UP}
  {$DEFINE DELPHI11_UP}
  {$DEFINE DELPHI10_UP}
  {$DEFINE DELPHI9_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI22}
  {$DEFINE DELPHI22_UP}
  {$DEFINE DELPHI21_UP}
  {$DEFINE DELPHI20_UP}
  {$DEFINE DELPHI19_UP}
  {$DEFINE DELPHI18_UP}
  {$DEFINE DELPHI17_UP}
  {$DEFINE DELPHI16_UP}
  {$DEFINE DELPHI15_UP}
  {$DEFINE DELPHI14_UP}
  {$DEFINE DELPHI12_UP}
  {$DEFINE DELPHI11_UP}
  {$DEFINE DELPHI10_UP}
  {$DEFINE DELPHI9_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI21}
  {$DEFINE DELPHI21_UP}
  {$DEFINE DELPHI20_UP}
  {$DEFINE DELPHI19_UP}
  {$DEFINE DELPHI18_UP}
  {$DEFINE DELPHI17_UP}
  {$DEFINE DELPHI16_UP}
  {$DEFINE DELPHI15_UP}
  {$DEFINE DELPHI14_UP}
  {$DEFINE DELPHI12_UP}
  {$DEFINE DELPHI11_UP}
  {$DEFINE DELPHI10_UP}
  {$DEFINE DELPHI9_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI20}
  {$DEFINE DELPHI20_UP}
  {$DEFINE DELPHI19_UP}
  {$DEFINE DELPHI18_UP}
  {$DEFINE DELPHI17_UP}
  {$DEFINE DELPHI16_UP}
  {$DEFINE DELPHI15_UP}
  {$DEFINE DELPHI14_UP}
  {$DEFINE DELPHI12_UP}
  {$DEFINE DELPHI11_UP}
  {$DEFINE DELPHI10_UP}
  {$DEFINE DELPHI9_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI19}
  {$DEFINE DELPHI19_UP}
  {$DEFINE DELPHI18_UP}
  {$DEFINE DELPHI17_UP}
  {$DEFINE DELPHI16_UP}
  {$DEFINE DELPHI15_UP}
  {$DEFINE DELPHI14_UP}
  {$DEFINE DELPHI12_UP}
  {$DEFINE DELPHI11_UP}
  {$DEFINE DELPHI10_UP}
  {$DEFINE DELPHI9_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI18}
  {$DEFINE DELPHI18_UP}
  {$DEFINE DELPHI17_UP}
  {$DEFINE DELPHI16_UP}
  {$DEFINE DELPHI15_UP}
  {$DEFINE DELPHI14_UP}
  {$DEFINE DELPHI12_UP}
  {$DEFINE DELPHI11_UP}
  {$DEFINE DELPHI10_UP}
  {$DEFINE DELPHI9_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI17}
  {$DEFINE DELPHI17_UP}
  {$DEFINE DELPHI16_UP}
  {$DEFINE DELPHI15_UP}
  {$DEFINE DELPHI14_UP}
  {$DEFINE DELPHI12_UP}
  {$DEFINE DELPHI11_UP}
  {$DEFINE DELPHI10_UP}
  {$DEFINE DELPHI9_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI16}
  {$DEFINE DELPHI16_UP}
  {$DEFINE DELPHI15_UP}
  {$DEFINE DELPHI14_UP}
  {$DEFINE DELPHI12_UP}
  {$DEFINE DELPHI11_UP}
  {$DEFINE DELPHI10_UP}
  {$DEFINE DELPHI9_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI15}
  {$DEFINE DELPHI15_UP}
  {$DEFINE DELPHI14_UP}
  {$DEFINE DELPHI12_UP}
  {$DEFINE DELPHI11_UP}
  {$DEFINE DELPHI10_UP}
  {$DEFINE DELPHI9_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI14}
  {$DEFINE DELPHI14_UP}
  {$DEFINE DELPHI12_UP}
  {$DEFINE DELPHI11_UP}
  {$DEFINE DELPHI10_UP}
  {$DEFINE DELPHI9_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI12}
  {$DEFINE DELPHI12_UP}
  {$DEFINE DELPHI11_UP}
  {$DEFINE DELPHI10_UP}
  {$DEFINE DELPHI9_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI11}
  {$DEFINE DELPHI11_UP}
  {$DEFINE DELPHI10_UP}
  {$DEFINE DELPHI9_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI10}
  {$DEFINE DELPHI10_UP}
  {$DEFINE DELPHI9_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI9}
  {$DEFINE DELPHI9_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI8}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI7}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI6}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI5}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI4}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI3}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI2}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI1}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF COMPILER25}
  {$DEFINE COMPILER25_UP}
  {$DEFINE COMPILER24_UP}
  {$DEFINE COMPILER23_UP}
  {$DEFINE COMPILER22_UP}
  {$DEFINE COMPILER21_UP}
  {$DEFINE COMPILER20_UP}
  {$DEFINE COMPILER19_UP}
  {$DEFINE COMPILER18_UP}
  {$DEFINE COMPILER17_UP}
  {$DEFINE COMPILER16_UP}
  {$DEFINE COMPILER15_UP}
  {$DEFINE COMPILER14_UP}
  {$DEFINE COMPILER12_UP}
  {$DEFINE COMPILER11_UP}
  {$DEFINE COMPILER10_UP}
  {$DEFINE COMPILER9_UP}
  {$DEFINE COMPILER8_UP}
  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
  {$DEFINE COMPILER5_UP}
  {$DEFINE COMPILER4_UP}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER24}
  {$DEFINE COMPILER24_UP}
  {$DEFINE COMPILER23_UP}
  {$DEFINE COMPILER22_UP}
  {$DEFINE COMPILER21_UP}
  {$DEFINE COMPILER20_UP}
  {$DEFINE COMPILER19_UP}
  {$DEFINE COMPILER18_UP}
  {$DEFINE COMPILER17_UP}
  {$DEFINE COMPILER16_UP}
  {$DEFINE COMPILER15_UP}
  {$DEFINE COMPILER14_UP}
  {$DEFINE COMPILER12_UP}
  {$DEFINE COMPILER11_UP}
  {$DEFINE COMPILER10_UP}
  {$DEFINE COMPILER9_UP}
  {$DEFINE COMPILER8_UP}
  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
  {$DEFINE COMPILER5_UP}
  {$DEFINE COMPILER4_UP}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER23}
  {$DEFINE COMPILER23_UP}
  {$DEFINE COMPILER22_UP}
  {$DEFINE COMPILER21_UP}
  {$DEFINE COMPILER20_UP}
  {$DEFINE COMPILER19_UP}
  {$DEFINE COMPILER18_UP}
  {$DEFINE COMPILER17_UP}
  {$DEFINE COMPILER16_UP}
  {$DEFINE COMPILER15_UP}
  {$DEFINE COMPILER14_UP}
  {$DEFINE COMPILER12_UP}
  {$DEFINE COMPILER11_UP}
  {$DEFINE COMPILER10_UP}
  {$DEFINE COMPILER9_UP}
  {$DEFINE COMPILER8_UP}
  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
  {$DEFINE COMPILER5_UP}
  {$DEFINE COMPILER4_UP}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER22}
  {$DEFINE COMPILER22_UP}
  {$DEFINE COMPILER21_UP}
  {$DEFINE COMPILER20_UP}
  {$DEFINE COMPILER19_UP}
  {$DEFINE COMPILER18_UP}
  {$DEFINE COMPILER17_UP}
  {$DEFINE COMPILER16_UP}
  {$DEFINE COMPILER15_UP}
  {$DEFINE COMPILER14_UP}
  {$DEFINE COMPILER12_UP}
  {$DEFINE COMPILER11_UP}
  {$DEFINE COMPILER10_UP}
  {$DEFINE COMPILER9_UP}
  {$DEFINE COMPILER8_UP}
  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
  {$DEFINE COMPILER5_UP}
  {$DEFINE COMPILER4_UP}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER21}
  {$DEFINE COMPILER21_UP}
  {$DEFINE COMPILER20_UP}
  {$DEFINE COMPILER19_UP}
  {$DEFINE COMPILER18_UP}
  {$DEFINE COMPILER17_UP}
  {$DEFINE COMPILER16_UP}
  {$DEFINE COMPILER15_UP}
  {$DEFINE COMPILER14_UP}
  {$DEFINE COMPILER12_UP}
  {$DEFINE COMPILER11_UP}
  {$DEFINE COMPILER10_UP}
  {$DEFINE COMPILER9_UP}
  {$DEFINE COMPILER8_UP}
  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
  {$DEFINE COMPILER5_UP}
  {$DEFINE COMPILER4_UP}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER20}
  {$DEFINE COMPILER20_UP}
  {$DEFINE COMPILER19_UP}
  {$DEFINE COMPILER18_UP}
  {$DEFINE COMPILER17_UP}
  {$DEFINE COMPILER16_UP}
  {$DEFINE COMPILER15_UP}
  {$DEFINE COMPILER14_UP}
  {$DEFINE COMPILER12_UP}
  {$DEFINE COMPILER11_UP}
  {$DEFINE COMPILER10_UP}
  {$DEFINE COMPILER9_UP}
  {$DEFINE COMPILER8_UP}
  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
  {$DEFINE COMPILER5_UP}
  {$DEFINE COMPILER4_UP}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER19}
  {$DEFINE COMPILER19_UP}
  {$DEFINE COMPILER18_UP}
  {$DEFINE COMPILER17_UP}
  {$DEFINE COMPILER16_UP}
  {$DEFINE COMPILER15_UP}
  {$DEFINE COMPILER14_UP}
  {$DEFINE COMPILER12_UP}
  {$DEFINE COMPILER11_UP}
  {$DEFINE COMPILER10_UP}
  {$DEFINE COMPILER9_UP}
  {$DEFINE COMPILER8_UP}
  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
  {$DEFINE COMPILER5_UP}
  {$DEFINE COMPILER4_UP}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER18}
  {$DEFINE COMPILER18_UP}
  {$DEFINE COMPILER17_UP}
  {$DEFINE COMPILER16_UP}
  {$DEFINE COMPILER15_UP}
  {$DEFINE COMPILER14_UP}
  {$DEFINE COMPILER12_UP}
  {$DEFINE COMPILER11_UP}
  {$DEFINE COMPILER10_UP}
  {$DEFINE COMPILER9_UP}
  {$DEFINE COMPILER8_UP}
  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
  {$DEFINE COMPILER5_UP}
  {$DEFINE COMPILER4_UP}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER17}
  {$DEFINE COMPILER17_UP}
  {$DEFINE COMPILER16_UP}
  {$DEFINE COMPILER15_UP}
  {$DEFINE COMPILER14_UP}
  {$DEFINE COMPILER12_UP}
  {$DEFINE COMPILER11_UP}
  {$DEFINE COMPILER10_UP}
  {$DEFINE COMPILER9_UP}
  {$DEFINE COMPILER8_UP}
  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
  {$DEFINE COMPILER5_UP}
  {$DEFINE COMPILER4_UP}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER16}
  {$DEFINE COMPILER16_UP}
  {$DEFINE COMPILER15_UP}
  {$DEFINE COMPILER14_UP}
  {$DEFINE COMPILER12_UP}
  {$DEFINE COMPILER11_UP}
  {$DEFINE COMPILER10_UP}
  {$DEFINE COMPILER9_UP}
  {$DEFINE COMPILER8_UP}
  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
  {$DEFINE COMPILER5_UP}
  {$DEFINE COMPILER4_UP}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER15}
  {$DEFINE COMPILER15_UP}
  {$DEFINE COMPILER14_UP}
  {$DEFINE COMPILER12_UP}
  {$DEFINE COMPILER11_UP}
  {$DEFINE COMPILER10_UP}
  {$DEFINE COMPILER9_UP}
  {$DEFINE COMPILER8_UP}
  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
  {$DEFINE COMPILER5_UP}
  {$DEFINE COMPILER4_UP}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER14}
  {$DEFINE COMPILER14_UP}
  {$DEFINE COMPILER12_UP}
  {$DEFINE COMPILER11_UP}
  {$DEFINE COMPILER10_UP}
  {$DEFINE COMPILER9_UP}
  {$DEFINE COMPILER8_UP}
  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
  {$DEFINE COMPILER5_UP}
  {$DEFINE COMPILER4_UP}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER12}
  {$DEFINE COMPILER12_UP}
  {$DEFINE COMPILER11_UP}
  {$DEFINE COMPILER10_UP}
  {$DEFINE COMPILER9_UP}
  {$DEFINE COMPILER8_UP}
  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
  {$DEFINE COMPILER5_UP}
  {$DEFINE COMPILER4_UP}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER11}
  {$DEFINE COMPILER11_UP}
  {$DEFINE COMPILER10_UP}
  {$DEFINE COMPILER9_UP}
  {$DEFINE COMPILER8_UP}
  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
  {$DEFINE COMPILER5_UP}
  {$DEFINE COMPILER4_UP}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER10}
  {$DEFINE COMPILER10_UP}
  {$DEFINE COMPILER9_UP}
  {$DEFINE COMPILER8_UP}
  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
  {$DEFINE COMPILER5_UP}
  {$DEFINE COMPILER4_UP}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER9}
  {$DEFINE COMPILER9_UP}
  {$DEFINE COMPILER8_UP}
  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
  {$DEFINE COMPILER5_UP}
  {$DEFINE COMPILER4_UP}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER8}
  {$DEFINE COMPILER8_UP}
  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
  {$DEFINE COMPILER5_UP}
  {$DEFINE COMPILER4_UP}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER7}
  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
  {$DEFINE COMPILER5_UP}
  {$DEFINE COMPILER4_UP}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER6}
  {$DEFINE COMPILER6_UP}
  {$DEFINE COMPILER5_UP}
  {$DEFINE COMPILER4_UP}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER5}
  {$DEFINE COMPILER5_UP}
  {$DEFINE COMPILER4_UP}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER4}
  {$DEFINE COMPILER4_UP}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER35}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER3}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER2}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER1}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF BCB23}
  {$DEFINE BCB23_UP}
  {$DEFINE BCB22_UP}
  {$DEFINE BCB21_UP}
  {$DEFINE BCB20_UP}
  {$DEFINE BCB19_UP}
  {$DEFINE BCB18_UP}
  {$DEFINE BCB17_UP}
  {$DEFINE BCB16_UP}
  {$DEFINE BCB15_UP}
  {$DEFINE BCB14_UP}
  {$DEFINE BCB12_UP}
  {$DEFINE BCB11_UP}
  {$DEFINE BCB10_UP}
  {$DEFINE BCB6_UP}
  {$DEFINE BCB5_UP}
  {$DEFINE BCB4_UP}
  {$DEFINE BCB3_UP}
  {$DEFINE BCB1_UP}
{$ENDIF}

{$IFDEF BCB22}
  {$DEFINE BCB22_UP}
  {$DEFINE BCB21_UP}
  {$DEFINE BCB20_UP}
  {$DEFINE BCB19_UP}
  {$DEFINE BCB18_UP}
  {$DEFINE BCB17_UP}
  {$DEFINE BCB16_UP}
  {$DEFINE BCB15_UP}
  {$DEFINE BCB14_UP}
  {$DEFINE BCB12_UP}
  {$DEFINE BCB11_UP}
  {$DEFINE BCB10_UP}
  {$DEFINE BCB6_UP}
  {$DEFINE BCB5_UP}
  {$DEFINE BCB4_UP}
  {$DEFINE BCB3_UP}
  {$DEFINE BCB1_UP}
{$ENDIF}

{$IFDEF BCB21}
  {$DEFINE BCB21_UP}
  {$DEFINE BCB20_UP}
  {$DEFINE BCB19_UP}
  {$DEFINE BCB18_UP}
  {$DEFINE BCB17_UP}
  {$DEFINE BCB16_UP}
  {$DEFINE BCB15_UP}
  {$DEFINE BCB14_UP}
  {$DEFINE BCB12_UP}
  {$DEFINE BCB11_UP}
  {$DEFINE BCB10_UP}
  {$DEFINE BCB6_UP}
  {$DEFINE BCB5_UP}
  {$DEFINE BCB4_UP}
  {$DEFINE BCB3_UP}
  {$DEFINE BCB1_UP}
{$ENDIF}

{$IFDEF BCB20}
  {$DEFINE BCB20_UP}
  {$DEFINE BCB19_UP}
  {$DEFINE BCB18_UP}
  {$DEFINE BCB17_UP}
  {$DEFINE BCB16_UP}
  {$DEFINE BCB15_UP}
  {$DEFINE BCB14_UP}
  {$DEFINE BCB12_UP}
  {$DEFINE BCB11_UP}
  {$DEFINE BCB10_UP}
  {$DEFINE BCB6_UP}
  {$DEFINE BCB5_UP}
  {$DEFINE BCB4_UP}
  {$DEFINE BCB3_UP}
  {$DEFINE BCB1_UP}
{$ENDIF}

{$IFDEF BCB19}
  {$DEFINE BCB19_UP}
  {$DEFINE BCB18_UP}
  {$DEFINE BCB17_UP}
  {$DEFINE BCB16_UP}
  {$DEFINE BCB15_UP}
  {$DEFINE BCB14_UP}
  {$DEFINE BCB12_UP}
  {$DEFINE BCB11_UP}
  {$DEFINE BCB10_UP}
  {$DEFINE BCB6_UP}
  {$DEFINE BCB5_UP}
  {$DEFINE BCB4_UP}
  {$DEFINE BCB3_UP}
  {$DEFINE BCB1_UP}
{$ENDIF}

{$IFDEF BCB18}
  {$DEFINE BCB18_UP}
  {$DEFINE BCB17_UP}
  {$DEFINE BCB16_UP}
  {$DEFINE BCB15_UP}
  {$DEFINE BCB14_UP}
  {$DEFINE BCB12_UP}
  {$DEFINE BCB11_UP}
  {$DEFINE BCB10_UP}
  {$DEFINE BCB6_UP}
  {$DEFINE BCB5_UP}
  {$DEFINE BCB4_UP}
  {$DEFINE BCB3_UP}
  {$DEFINE BCB1_UP}
{$ENDIF}

{$IFDEF BCB17}
  {$DEFINE BCB17_UP}
  {$DEFINE BCB16_UP}
  {$DEFINE BCB15_UP}
  {$DEFINE BCB14_UP}
  {$DEFINE BCB12_UP}
  {$DEFINE BCB11_UP}
  {$DEFINE BCB10_UP}
  {$DEFINE BCB6_UP}
  {$DEFINE BCB5_UP}
  {$DEFINE BCB4_UP}
  {$DEFINE BCB3_UP}
  {$DEFINE BCB1_UP}
{$ENDIF}

{$IFDEF BCB16}
  {$DEFINE BCB16_UP}
  {$DEFINE BCB15_UP}
  {$DEFINE BCB14_UP}
  {$DEFINE BCB12_UP}
  {$DEFINE BCB11_UP}
  {$DEFINE BCB10_UP}
  {$DEFINE BCB6_UP}
  {$DEFINE BCB5_UP}
  {$DEFINE BCB4_UP}
  {$DEFINE BCB3_UP}
  {$DEFINE BCB1_UP}
{$ENDIF}

{$IFDEF BCB15}
  {$DEFINE BCB15_UP}
  {$DEFINE BCB14_UP}
  {$DEFINE BCB12_UP}
  {$DEFINE BCB11_UP}
  {$DEFINE BCB10_UP}
  {$DEFINE BCB6_UP}
  {$DEFINE BCB5_UP}
  {$DEFINE BCB4_UP}
  {$DEFINE BCB3_UP}
  {$DEFINE BCB1_UP}
{$ENDIF}


{$IFDEF BCB14}
  {$DEFINE BCB14_UP}
  {$DEFINE BCB12_UP}
  {$DEFINE BCB11_UP}
  {$DEFINE BCB10_UP}
  {$DEFINE BCB6_UP}
  {$DEFINE BCB5_UP}
  {$DEFINE BCB4_UP}
  {$DEFINE BCB3_UP}
  {$DEFINE BCB1_UP}
{$ENDIF}

{$IFDEF BCB12}
  {$DEFINE BCB12_UP}
  {$DEFINE BCB11_UP}
  {$DEFINE BCB10_UP}
  {$DEFINE BCB6_UP}
  {$DEFINE BCB5_UP}
  {$DEFINE BCB4_UP}
  {$DEFINE BCB3_UP}
  {$DEFINE BCB1_UP}
{$ENDIF}

{$IFDEF BCB11}
  {$DEFINE BCB11_UP}
  {$DEFINE BCB10_UP}
  {$DEFINE BCB6_UP}
  {$DEFINE BCB5_UP}
  {$DEFINE BCB4_UP}
  {$DEFINE BCB3_UP}
  {$DEFINE BCB1_UP}
{$ENDIF}

{$IFDEF BCB10}
  {$DEFINE BCB10_UP}
  {$DEFINE BCB6_UP}
  {$DEFINE BCB5_UP}
  {$DEFINE BCB4_UP}
  {$DEFINE BCB3_UP}
  {$DEFINE BCB1_UP}
{$ENDIF}

{$IFDEF BCB6}
  {$DEFINE BCB6_UP}
  {$DEFINE BCB5_UP}
  {$DEFINE BCB4_UP}
  {$DEFINE BCB3_UP}
  {$DEFINE BCB1_UP}
{$ENDIF}

{$IFDEF BCB5}
  {$DEFINE BCB5_UP}
  {$DEFINE BCB4_UP}
  {$DEFINE BCB3_UP}
  {$DEFINE BCB1_UP}
{$ENDIF}

{$IFDEF BCB4}
  {$DEFINE BCB4_UP}
  {$DEFINE BCB3_UP}
  {$DEFINE BCB1_UP}
{$ENDIF}

{$IFDEF BCB3}
  {$DEFINE BCB3_UP}
  {$DEFINE BCB1_UP}
{$ENDIF}

{$IFDEF BCB1}
  {$DEFINE BCB1_UP}
{$ENDIF}

{$IFDEF CLR}
  {$DEFINE DOTNET}
  {$DEFINE BOMB}
{$ELSE}
  {$IFNDEF COMPILER7_UP}
    {$DEFINE BOMB}
  {$ENDIF}
{$ENDIF}

{$IFDEF BOMB}
  {$MESSAGE FATAL 'At least Delphi 7 or C++ Builder 2006 for Win32 is required to compile ICSv7.'}
{$ENDIF}

{ TStream class support 64 bit seek }
{$IFNDEF FPC}             { Not for FPC !              }
  {$IFDEF COMPILER6_UP}   { OK for Delphi/BCB 6 and up }
    {$DEFINE STREAM64}
  {$ENDIF}
{$ENDIF}

{$IFNDEF DELPHI1}                          { Only Delphi1 lack windows unit }
  {$DEFINE USEWINDOWS}                     { Use "Windows" unit.            }
{$ENDIF}

{$IFDEF DELPHI6_UP}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
  {$WARN UNIT_DEPRECATED OFF}
{$ENDIF}

{$IFDEF DELPHI7_UP}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

{$IFDEF COMPILER10_UP}
  {.$IFNDEF DEBUG}
    {$DEFINE USE_INLINE}
  {.$ENDIF}
  {$DEFINE HAS_SEARCHREC_SIZE64}  // Not for sure might be already D2005
{$ENDIF}

{$IFDEF DELPHI12}
  { Since Fulcrum always OFF and ignored }
  {$STRINGCHECKS OFF}
  { No string checks in pure Delphi code }
{$ENDIF}
{$IFDEF DELPHI14}
  { Since Fulcrum always OFF and ignored }
  {$STRINGCHECKS OFF}
  { No string checks in pure Delphi code }
{$ENDIF}

{ ICS SSL code doesn't work with FPC }
{$IFDEF FPC}
  {$UNDEF USE_SSL}
{$ENDIF}

{ ICS SSL code doesn't work with .NET }
{$IFDEF DOTNET}
  {$UNDEF USE_SSL}
{$ENDIF}

{$IFNDEF MSWINDOWS}
  {$IFNDEF USE_ICONV}
    {$DEFINE USE_ICONV}
  {$ENDIF}
{$ENDIF}

{$IFDEF DELPHI16_UP}
  {$DEFINE RTL_NAMESPACES}
{$ENDIF}

