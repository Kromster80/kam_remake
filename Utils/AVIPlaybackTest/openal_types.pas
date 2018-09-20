unit openal_types;
{
Stucuk:

Seperated the Types into this seperate file for use with the Meaaov OpenALHandler.
}
(* Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is OpenAL1.0 - Headertranslation to Object Pascal.
 *
 * The Initial Developer of the Original Code is
 * Delphi OpenAL Translation Team.
 * Portions created by the Initial Developer are Copyright (C) 2001-2004
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Tom Nuydens             (delphi3d@gamedeveloper.org)
 *   Dean Ellis              (dean.ellis@sxmedia.co.uk)
 *   Amresh Ramachandran     (amreshr@hotmail.com)
 *   Pranav Joshi            (pranavjosh@yahoo.com)
 *   Marten van der Honing   (mvdhoning@noeska.com)
 *   Benjamin Rosseaux (BeRo)
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *)

interface

type
  // OpenAL boolean type.
  TALboolean = Boolean;
  PALboolean = ^TALboolean;
  // OpenAL 8bit signed byte.
  TALbyte = ShortInt;
  PALbyte = ^TALbyte;
  // OpenAL 8bit unsigned byte.
  TALuByte = Char;
  PALuByte = PChar;
  // OpenAL 16bit signed short integer type.
  TALshort = SmallInt;
  PALshort = ^TALshort;
  // OpenAL 16bit unsigned short integer type.
  TALushort = Word;
  PALushort = ^TALushort;
  // OpenAL 32bit unsigned integer type.
  TALuint = Cardinal;
  PALuint = ^TALuint;
  // OpenAL 32bit signed integer type.
  TALint = Integer;
  PALint = ^TALint;
  // OpenAL 32bit floating point type.
  TALfloat = Single;
  PALfloat = ^TALfloat;
  // OpenAL 64bit double point type.
  TALdouble = Double;
  PALdouble = ^TALdouble;
  // OpenAL 32bit type.
  TALsizei = Cardinal;
  PALsizei = ^TALsizei;
  // OpenAL void type
  TALvoid = Pointer;
  PALvoid = ^TALvoid;
  PPALvoid = ^PALvoid;
  // OpenAL enumerations.
  TALenum = Integer;
  PALenum = ^TALenum;
  // OpenAL bitfields.
  TALbitfield = Cardinal;
  PALbitfield = ^TALbitfield;
  // OpenAL clamped float.
  TALclampf = TALfloat;
  PALclampf = ^TALclampf;
  // Openal clamped double.
  TALclampd = TALdouble;
  PALclampd = ^TALclampd;

  // ALC enumerations.
  TALCenum = integer;
  PALCenum = ^TALCenum;
  // ALC boolean type.
  TALCboolean = boolean;
  PALCboolean = ^TALCboolean;
  // ALC 8bit signed byte.
  TALCbyte = ShortInt;
  PALCbyte = ^TALCbyte;
  // ALC 8bit unsigned byte.
  TALCubyte = Char;
  PALCubyte = PChar;
  // ALC 16bit signed short integer type.
  TALCshort = smallint;
  PALCshort = ^TALCshort;
  // ALC 16bit unsigned short integer type.
  TALCushort = Word;
  PALCushort = ^TALCushort;
  // ALC 32bit unsigned integer type.
  TALCuint = Cardinal;
  PALCuint = ^TALCuint;
  // ALC 32bit signed integer type.
  TALCint = integer;
  PALCint = ^TALCint;
  // ALC 32bit floating point type.
  TALCfloat = single;
  PALCfloat = ^TALCfloat;
  // ALC 64bit double point type.
  TALCdouble = double;
  PALCdouble = ^TALCdouble;
  // ALC 32bit type.
  TALCsizei = integer;
  PALCsizei = ^TALCsizei;
  // ALC void type
  TALCvoid = Pointer;
  PALCvoid = ^TALCvoid;
  // ALC device
  TALCdevice = TALCvoid;
  PALCdevice = ^TALCdevice;
  // ALC context
  TALCcontext = TALCvoid;
  PALCcontext = ^TALCcontext;

  //EAX extension
  DSPROPERTY_EAX_LISTENERPROPERTY = LongWORD;
  DSPROPERTY_EAX_BUFFERPROPERTY = LongWORD;

  // Use this structure for DSPROPERTY_EAXLISTENER_ALLPARAMETERS
  // - all levels are hundredths of decibels
  // - all times are in seconds
  // - the reference for high frequency controls is 5 kHz
  //
  // NOTE: This structure may change in future EAX versions.
  //       It is recommended to initialize fields by name:
  //              myListener.lRoom = -1000;
  //              myListener.lRoomHF = -100;
  //              ...
  //              myListener.dwFlags = myFlags /* see EAXLISTENERFLAGS below */ ;
  //       instead of:
  //              myListener = { -1000, -100, ... , 0x00000009 };
  //       If you want to save and load presets in binary form, you
  //       should define your own structure to insure future compatibility.
  //
  TEaxListenerProperties = packed record
    lRoom: integer; // room effect level at low frequencies
    lRoomHF: integer;
    // room effect high-frequency level re. low frequency levelimplementation
    flRoomRolloffFactor: double; // like DS3D flRolloffFactor but for room effect
    flDecayTime: double; // reverberation decay time at low frequenciesend.
    flDecayHFRatio: double; // high-frequency to low-frequency decay time ratio
    lReflections: integer; // early reflections level relative to room effect
    flReflectionsDelay: double; // initial reflection delay time
    lReverb: integer; // late reverberation level relative to room effect
    flReverbDelay: double;
    // late reverberation delay time relative to initial reflection
    dwEnvironment: cardinal; // sets all listener properties
    flEnvironmentSize: double; // environment size in meters
    flEnvironmentDiffusion: double; // environment diffusion
    flAirAbsorptionHF: double; // change in level per meter at 5 kHz
    dwFlags: cardinal; // modifies the behavior of properties
  end;
  PEaxListenerProperties = ^TEaxListenerProperties;

  // Use this structure for DSPROPERTY_EAXBUFFER_ALLPARAMETERS
  // - all levels are hundredths of decibels
  //
  // NOTE: This structure may change in future EAX versions.
  //       It is recommended to initialize fields by name:
  //              myBuffer.lDirect = 0;
  //              myBuffer.lDirectHF = -200;
  //              ...
  //              myBuffer.dwFlags = myFlags /* see EAXBUFFERFLAGS below */ ;
  //       instead of:
  //              myBuffer = { 0, -200, ... , 0x00000003 };
  //
  TEaxBufferProperties = packed record
    lDirect: integer; // direct path level
    lDirectHF: integer; // direct path level at high frequencies
    lRoom: integer; // room effect level
    lRoomHF: integer; // room effect level at high frequencies
    flRoomRolloffFactor: double; // like DS3D flRolloffFactor but for room effect
    lObstruction: integer;
    // main obstruction control (attenuation at high frequencies)
    flObstructionLFRatio: double;
    // obstruction low-frequency level re. main control
    lOcclusion: integer;
    // main occlusion control (attenuation at high frequencies)
    flOcclusionLFRatio: double; // occlusion low-frequency level re. main control
    flOcclusionRoomRatio: double; // occlusion room effect level re. main control
    lOutsideVolumeHF: integer; // outside sound cone level at high frequencies
    flAirAbsorptionFactor: double;
    // multiplies DSPROPERTY_EAXLISTENER_AIRABSORPTIONHF
    dwFlags: Cardinal; // modifies the behavior of properties
  end;
  PEaxBufferProperties = ^TEaxBufferProperties;

const
  //bad value
  AL_INVALID                                = -1;

  AL_NONE                                   = 0;

  //Boolean False.
  AL_FALSE                                  = 0;

  //Boolean True.
  AL_TRUE                                   = 1;

  //Indicate the type of AL_SOURCE.
  //Sources can be spatialized
  AL_SOURCE_TYPE                            = $200;

  //Indicate source has absolute coordinates.
  AL_SOURCE_ABSOLUTE                       = $201;

  //Indicate Source has relative coordinates.
  AL_SOURCE_RELATIVE                       = $202;

  //Directional source, inner cone angle, in degrees.
  //Range:    [0-360]
  //Default:  360
  AL_CONE_INNER_ANGLE                      = $1001;

  //Directional source, outer cone angle, in degrees.
  //Range:    [0-360]
  //Default:  360
  AL_CONE_OUTER_ANGLE                       = $1002;

  //Specify the pitch to be applied, either at source,
  //or on mixer results, at listener.
  //Range:   [0.5-2.0]
  //Default: 1.0
  AL_PITCH                                  =$1003;

  //Specify the current location in three dimensional space.
  //OpenAL, like OpenGL, uses a right handed coordinate system,
  //where in a frontal default view X (thumb) points right,
  //Y points up (index finger), and Z points towards the
  //viewer/camera (middle finger).
  //To switch from a left handed coordinate system, flip the
  //sign on the Z coordinate.
  //Listener position is always in the world coordinate system.
  AL_POSITION                               =$1004;

  //Specify the current direction.
  AL_DIRECTION                              =$1005;

  // Specify the current velocity in three dimensional space.
  AL_VELOCITY                               =$1006;

  //Indicate whether source is looping.
  //Type: ALboolean?
  //Range:   [AL_TRUE, AL_FALSE]
  //Default: FALSE.
  AL_LOOPING                                =$1007;

  //Indicate the buffer to provide sound samples.
  //Type: ALuint.
  //Range: any valid Buffer id.
  AL_BUFFER                                 =$1009;

  //Indicate the gain (volume amplification) applied.
  //Type:   ALfloat.
  //Range:  ]0.0-  ]
  //A value of 1.0 means un-attenuated/unchanged.
  //Each division by 2 equals an attenuation of -6dB.
  //Each multiplicaton with 2 equals an amplification of +6dB.
  //A value of 0.0 is meaningless with respect to a logarithmic
  //scale; it is interpreted as zero volume - the channel
  //is effectively disabled.
  AL_GAIN                                   =$100A;

  //Indicate minimum source attenuation
  //Type: ALfloat
  //Range:  [0.0 - 1.0]
  //Logarthmic
  AL_MIN_GAIN                               =$100D;

  //Indicate maximum source attenuation
  //Type: ALfloat
  //Range:  [0.0 - 1.0]
  //Logarthmic
  AL_MAX_GAIN                               =$100E;

  //Indicate listener orientation.
  //at/up
  AL_ORIENTATION                            =$100F;

  //Specify the channel mask. (Creative)
  //Type:	 ALuint
  //Range:	 [0 - 255]
  AL_CHANNEL_MASK                           =$3000;

  //Source state information.
  AL_SOURCE_STATE                           =$1010;
  AL_INITIAL                                =$1011;
  AL_PLAYING                                =$1012;
  AL_PAUSED                                 =$1013;
  AL_STOPPED                                =$1014;

  //Buffer Queue params
  AL_BUFFERS_QUEUED                         =$1015;
  AL_BUFFERS_PROCESSED                      =$1016;

  //Sound samples: format specifier.
  AL_FORMAT_MONO8                           =$1100;
  AL_FORMAT_MONO16                          =$1101;
  AL_FORMAT_STEREO8                         =$1102;
  AL_FORMAT_STEREO16                        =$1103;

  //source specific reference distance
  //Type: ALfloat
  //Range:  0.0 - +inf
  //At 0.0, no distance attenuation occurs.  Default is
  //1.0.
  AL_REFERENCE_DISTANCE                     =$1020;

  //source specific rolloff factor
  //Type: ALfloat
  //Range:  0.0 - +inf
  AL_ROLLOFF_FACTOR                         =$1021;

  //Directional source, outer cone gain.
  //Default:  0.0
  //Range:    [0.0 - 1.0]
  //Logarithmic
  AL_CONE_OUTER_GAIN                        =$1022;

  //Indicate distance above which sources are not
  //attenuated using the inverse clamped distance model.
  //Default: +inf
  //Type: ALfloat
  //Range:  0.0 - +inf
  AL_MAX_DISTANCE                           =$1023;

  //Sound samples: frequency, in units of Hertz [Hz].
  //This is the number of samples per second. Half of the
  //sample frequency marks the maximum significant
  //frequency component.
  AL_FREQUENCY                              =$2001;
  AL_BITS                                   =$2002;
  AL_CHANNELS                               =$2003;
  AL_SIZE                                   =$2004;
  AL_DATA                                   =$2005;

  //Buffer state.
  //Not supported for public use (yet).
  AL_UNUSED                                 =$2010;
  AL_PENDING                                =$2011;
  AL_PROCESSED                              =$2012;

  //Errors: No Error.
  AL_NO_ERROR                               =AL_FALSE;

  //Invalid Name paramater passed to AL call.
  AL_INVALID_NAME                           =$A001;

  //Invalid parameter passed to AL call.
  AL_ILLEGAL_ENUM                           =$A002;
  AL_INVALID_ENUM                           =$A002;

  //Invalid enum parameter value.
  AL_INVALID_VALUE                          =$A003;

  //Illegal call.
  AL_ILLEGAL_COMMAND                        =$A004;
  AL_INVALID_OPERATION                      =$A004;

  //No mojo.
  AL_OUT_OF_MEMORY                          =$A005;

  // Context strings: Vendor Name.
  AL_VENDOR                                 =$B001;
  AL_VERSION                                =$B002;
  AL_RENDERER                               =$B003;
  AL_EXTENSIONS                             =$B004;

  // Global tweakage.

  // Doppler scale.  Default 1.0
  AL_DOPPLER_FACTOR                         =$C000;

  // Tweaks speed of propagation.
  AL_DOPPLER_VELOCITY                       =$C001;

  // Distance models
  //
  // used in conjunction with DistanceModel
  //
  // implicit: NONE, which disances distance attenuation.
  AL_DISTANCE_MODEL                         =$D000;
  AL_INVERSE_DISTANCE                       =$D001;
  AL_INVERSE_DISTANCE_CLAMPED               =$D002;

  //bad value
  ALC_INVALID                              =0;

  //Boolean False.
  ALC_FALSE                                =0;

  //Boolean True.
  ALC_TRUE                                 =1;

  //followed by <int> Hz
  ALC_FREQUENCY                            =$1007;

  //followed by <int> Hz
  ALC_REFRESH                              =$1008;

  //followed by AL_TRUE, AL_FALSE
  ALC_SYNC                                 =$1009;

  //errors

  //No error
  ALC_NO_ERROR                             =ALC_FALSE;

  //No device
  ALC_INVALID_DEVICE                       =$A001;

  //invalid context ID
  ALC_INVALID_CONTEXT                      =$A002;

  //bad enum
  ALC_INVALID_ENUM                         =$A003;

  //bad value
  ALC_INVALID_VALUE                        =$A004;

  //Out of memory.
  ALC_OUT_OF_MEMORY                        =$A005;

  //The Specifier string for default device
  ALC_DEFAULT_DEVICE_SPECIFIER             =$1004;
  ALC_DEVICE_SPECIFIER                     =$1005;
  ALC_EXTENSIONS                           =$1006;

  ALC_MAJOR_VERSION                        =$1000;
  ALC_MINOR_VERSION                        =$1001;

  ALC_ATTRIBUTES_SIZE                      =$1002;
  ALC_ALL_ATTRIBUTES                       =$1003;

  //EAX extension
  DSPROPSETID_EAX20_ListenerProperties     : TGuid = '{0306A6A8-B224-11d2-99E5-0000E8D8C722}';
  DSPROPSETID_EAX20_BufferProperties       : TGuid = '{0306A6A7-B224-11d2-99E5-0000E8D8C722}';

  // For compatibility with future EAX versions:
  //DSPROPSETID_EAX_ListenerProperties = DSPROPSETID_EAX20_ListenerProperties;
  //DSPROPSETID_EAX_SourceProperties = DSPROPSETID_EAX20_BufferProperties;

  //Enumerations DSPROPERTY_EAX_LISTENERPROPERTY
  DSPROPERTY_EAXLISTENER_NONE              = 0;
  DSPROPERTY_EAXLISTENER_ALLPARAMETERS     = 1;
  DSPROPERTY_EAXLISTENER_ROOM              = 2;
  DSPROPERTY_EAXLISTENER_ROOMHF            = 3;
  DSPROPERTY_EAXLISTENER_ROOMROLLOFFFACTOR = 4;
  DSPROPERTY_EAXLISTENER_DECAYTIME         = 5;
  DSPROPERTY_EAXLISTENER_DECAYHFRATIO      = 6;
  DSPROPERTY_EAXLISTENER_REFLECTIONS       = 7;
  DSPROPERTY_EAXLISTENER_REFLECTIONSDELAY  = 8;
  DSPROPERTY_EAXLISTENER_REVERB            = 9;
  DSPROPERTY_EAXLISTENER_REVERBDELAY       = 10;
  DSPROPERTY_EAXLISTENER_ENVIRONMENT       = 11;
  DSPROPERTY_EAXLISTENER_ENVIRONMENTSIZE   = 12;
  DSPROPERTY_EAXLISTENER_ENVIRONMENTDIFFUSION = 13;
  DSPROPERTY_EAXLISTENER_AIRABSORPTIONHF   = 14;
  DSPROPERTY_EAXLISTENER_FLAGS             = 15;

  // OR these flags with property id
  DSPROPERTY_EAXLISTENER_IMMEDIATE         = $00000000;
  // changes take effect immediately
  DSPROPERTY_EAXLISTENER_DEFERRED          = LongWORD($80000000);
  DSPROPERTY_EAXLISTENER_COMMITDEFERREDSETTINGS = (DSPROPERTY_EAXLISTENER_NONE or
                                                   DSPROPERTY_EAXLISTENER_IMMEDIATE);

  // used by DSPROPERTY_EAXLISTENER_ENVIRONMENT
  //Enummetation
  EAX_ENVIRONMENT_GENERIC                  = 0;
  EAX_ENVIRONMENT_PADDEDCELL               = 1;
  EAX_ENVIRONMENT_ROOM                     = 2;
  EAX_ENVIRONMENT_BATHROOM                 = 3;
  EAX_ENVIRONMENT_LIVINGROOM               = 4;
  EAX_ENVIRONMENT_STONEROOM                = 5;
  EAX_ENVIRONMENT_AUDITORIUM               = 6;
  EAX_ENVIRONMENT_CONCERTHALL              = 7;
  EAX_ENVIRONMENT_CAVE                     = 8;
  EAX_ENVIRONMENT_ARENA                    = 9;
  EAX_ENVIRONMENT_HANGAR                   = 10;
  EAX_ENVIRONMENT_CARPETEDHALLWAY          = 11;
  EAX_ENVIRONMENT_HALLWAY                  = 12;
  EAX_ENVIRONMENT_STONECORRIDOR            = 13;
  EAX_ENVIRONMENT_ALLEY                    = 14;
  EAX_ENVIRONMENT_FOREST                   = 15;
  EAX_ENVIRONMENT_CITY                     = 16;
  EAX_ENVIRONMENT_MOUNTAINS                = 17;
  EAX_ENVIRONMENT_QUARRY                   = 18;
  EAX_ENVIRONMENT_PLAIN                    = 19;
  EAX_ENVIRONMENT_PARKINGLOT               = 20;
  EAX_ENVIRONMENT_SEWERPIPE                = 21;
  EAX_ENVIRONMENT_UNDERWATER               = 22;
  EAX_ENVIRONMENT_DRUGGED                  = 23;
  EAX_ENVIRONMENT_DIZZY                    = 24;
  EAX_ENVIRONMENT_PSYCHOTIC                = 25;
  EAX_ENVIRONMENT_COUNT                    = 26;

  // Used by DSPROPERTY_EAXLISTENER_FLAGS
  //
  // Note: The number and order of flags may change in future EAX versions.
  //       It is recommended to use the flag defines as follows:
  //              myFlags = EAXLISTENERFLAGS_DECAYTIMESCALE | EAXLISTENERFLAGS_REVERBSCALE;
  //       instead of:
  //              myFlags = 0x00000009;
  //
  // These flags determine what properties are affected by environment size.
  EAXLISTENERFLAGS_DECAYTIMESCALE          = $00000001;
  // reverberation decay time
  EAXLISTENERFLAGS_REFLECTIONSSCALE        = $00000002; // reflection level
  EAXLISTENERFLAGS_REFLECTIONSDELAYSCALE   = $00000004;
  // initial reflection delay time
  EAXLISTENERFLAGS_REVERBSCALE             = $00000008; // reflections level
  EAXLISTENERFLAGS_REVERBDELAYSCALE        = $00000010;
  // late reverberation delay time

  // This flag limits high-frequency decay time according to air absorption.
  EAXLISTENERFLAGS_DECAYHFLIMIT            = $00000020;
  EAXLISTENERFLAGS_RESERVED                = $FFFFFFC0; // reserved future use

  // property ranges and defaults:
  EAXLISTENER_MINROOM                      = -10000;
  EAXLISTENER_MAXROOM                      = 0;
  EAXLISTENER_DEFAULTROOM                  = -1000;

  EAXLISTENER_MINROOMHF                    = -10000;
  EAXLISTENER_MAXROOMHF                    = 0;
  EAXLISTENER_DEFAULTROOMHF                = -100;

  EAXLISTENER_MINROOMROLLOFFFACTOR         = 0.0;
  EAXLISTENER_MAXROOMROLLOFFFACTOR         = 10.0;
  EAXLISTENER_DEFAULTROOMROLLOFFFACTOR     = 0.0;

  EAXLISTENER_MINDECAYTIME                 = 0.1;
  EAXLISTENER_MAXDECAYTIME                 = 20.0;
  EAXLISTENER_DEFAULTDECAYTIME             = 1.49;

  EAXLISTENER_MINDECAYHFRATIO              = 0.1;
  EAXLISTENER_MAXDECAYHFRATIO              = 2.0;
  EAXLISTENER_DEFAULTDECAYHFRATIO          = 0.83;

  EAXLISTENER_MINREFLECTIONS               = -10000;
  EAXLISTENER_MAXREFLECTIONS               = 1000;
  EAXLISTENER_DEFAULTREFLECTIONS           = -2602;

  EAXLISTENER_MINREFLECTIONSDELAY          = 0.0;
  EAXLISTENER_MAXREFLECTIONSDELAY          = 0.3;
  EAXLISTENER_DEFAULTREFLECTIONSDELAY      = 0.007;

  EAXLISTENER_MINREVERB                    = -10000;
  EAXLISTENER_MAXREVERB                    = 2000;
  EAXLISTENER_DEFAULTREVERB                = 200;

  EAXLISTENER_MINREVERBDELAY               = 0.0;
  EAXLISTENER_MAXREVERBDELAY               = 0.1;
  EAXLISTENER_DEFAULTREVERBDELAY           = 0.011;

  EAXLISTENER_MINENVIRONMENT               = 0;
  EAXLISTENER_MAXENVIRONMENT               = EAX_ENVIRONMENT_COUNT - 1;
  EAXLISTENER_DEFAULTENVIRONMENT           = EAX_ENVIRONMENT_GENERIC;

  EAXLISTENER_MINENVIRONMENTSIZE           = 1.0;
  EAXLISTENER_MAXENVIRONMENTSIZE           = 100.0;
  EAXLISTENER_DEFAULTENVIRONMENTSIZE       = 7.5;

  EAXLISTENER_MINENVIRONMENTDIFFUSION      = 0.0;
  EAXLISTENER_MAXENVIRONMENTDIFFUSION      = 1.0;
  EAXLISTENER_DEFAULTENVIRONMENTDIFFUSION  = 1.0;

  EAXLISTENER_MINAIRABSORPTIONHF           = -100.0;
  EAXLISTENER_MAXAIRABSORPTIONHF           = 0.0;
  EAXLISTENER_DEFAULTAIRABSORPTIONHF       = -5.0;

  EAXLISTENER_DEFAULTFLAGS                 = EAXLISTENERFLAGS_DECAYTIMESCALE or
                                             EAXLISTENERFLAGS_REFLECTIONSSCALE or
  	                                     EAXLISTENERFLAGS_REFLECTIONSDELAYSCALE or
		                             EAXLISTENERFLAGS_REVERBSCALE or
		                             EAXLISTENERFLAGS_REVERBDELAYSCALE or
		                             EAXLISTENERFLAGS_DECAYHFLIMIT;

  // For compatibility with future EAX versions:
  //DSPROPSETID_EAX_BufferProperties = DSPROPSETID_EAX20_BufferProperties;

  //Enumeration DSPROPERTY_EAX_BUFFERPROPERTY
  DSPROPERTY_EAXBUFFER_NONE                = 0;
  DSPROPERTY_EAXBUFFER_ALLPARAMETERS       = 1;
  DSPROPERTY_EAXBUFFER_DIRECT              = 2;
  DSPROPERTY_EAXBUFFER_DIRECTHF            = 3;
  DSPROPERTY_EAXBUFFER_ROOM                = 4;
  DSPROPERTY_EAXBUFFER_ROOMHF              = 5;
  DSPROPERTY_EAXBUFFER_ROOMROLLOFFFACTOR   = 6;
  DSPROPERTY_EAXBUFFER_OBSTRUCTION         = 7;
  DSPROPERTY_EAXBUFFER_OBSTRUCTIONLFRATIO  = 8;
  DSPROPERTY_EAXBUFFER_OCCLUSION           = 9;
  DSPROPERTY_EAXBUFFER_OCCLUSIONLFRATIO    = 10;
  DSPROPERTY_EAXBUFFER_OCCLUSIONROOMRATIO  = 11;
  DSPROPERTY_EAXBUFFER_OUTSIDEVOLUMEHF     = 12;
  DSPROPERTY_EAXBUFFER_AIRABSORPTIONFACTOR = 13;
  DSPROPERTY_EAXBUFFER_FLAG                = 14;

  // OR these flags with property id
  DSPROPERTY_EAXBUFFER_IMMEDIATE           = $00000000;
  // changes take effect immediately
  DSPROPERTY_EAXBUFFER_DEFERRED            = LongWORD($80000000);
  DSPROPERTY_EAXBUFFER_COMMITDEFERREDSETTINGS = DSPROPERTY_EAXBUFFER_NONE or
  DSPROPERTY_EAXBUFFER_IMMEDIATE;

  // Used by DSPROPERTY_EAXBUFFER_FLAGS
  //    TRUE:    value is computed automatically - property is an offset
  //    FALSE:   value is used directly
  //
  // Note: The number and order of flags may change in future EAX versions.
  //       To insure future compatibility, use flag defines as follows:
  //              myFlags = EAXBUFFERFLAGS_DIRECTHFAUTO | EAXBUFFERFLAGS_ROOMAUTO;
  //       instead of:
  //              myFlags = 0x00000003;
  //
  EAXBUFFERFLAGS_DIRECTHFAUTO              = $00000001;
  // affects DSPROPERTY_EAXBUFFER_DIRECTHF
  EAXBUFFERFLAGS_ROOMAUTO                  = $00000002;
  // affects DSPROPERTY_EAXBUFFER_ROOM
  EAXBUFFERFLAGS_ROOMHFAUTO                = $00000004;
  // affects DSPROPERTY_EAXBUFFER_ROOMHF

  EAXBUFFERFLAGS_RESERVED                  = $FFFFFFF8; // reserved future use

  // property ranges and defaults:
  EAXBUFFER_MINDIRECT                      = -10000;
  EAXBUFFER_MAXDIRECT                      = 1000;
  EAXBUFFER_DEFAULTDIRECT                  = 0;

  EAXBUFFER_MINDIRECTHF                    = -10000;
  EAXBUFFER_MAXDIRECTHF                    = 0;
  EAXBUFFER_DEFAULTDIRECTHF                = 0;

  EAXBUFFER_MINROOM                        = -10000;
  EAXBUFFER_MAXROOM                        = 1000;
  EAXBUFFER_DEFAULTROOM                    = 0;

  EAXBUFFER_MINROOMHF                      = -10000;
  EAXBUFFER_MAXROOMHF                      = 0;
  EAXBUFFER_DEFAULTROOMHF                  = 0;

  EAXBUFFER_MINROOMROLLOFFFACTOR           = 0.0;
  EAXBUFFER_MAXROOMROLLOFFFACTOR           = 10.;
  EAXBUFFER_DEFAULTROOMROLLOFFFACTOR       = 0.0;

  EAXBUFFER_MINOBSTRUCTION                 = -10000;
  EAXBUFFER_MAXOBSTRUCTION                 = 0;
  EAXBUFFER_DEFAULTOBSTRUCTION             = 0;

  EAXBUFFER_MINOBSTRUCTIONLFRATIO          = 0.0;
  EAXBUFFER_MAXOBSTRUCTIONLFRATIO          = 1.0;
  EAXBUFFER_DEFAULTOBSTRUCTIONLFRATIO      = 0.0;

  EAXBUFFER_MINOCCLUSION                   = -10000;
  EAXBUFFER_MAXOCCLUSION                   = 0;
  EAXBUFFER_DEFAULTOCCLUSION               = 0;

  EAXBUFFER_MINOCCLUSIONLFRATIO            = 0.0;
  EAXBUFFER_MAXOCCLUSIONLFRATIO            = 1.0;
  EAXBUFFER_DEFAULTOCCLUSIONLFRATIO        = 0.25;

  EAXBUFFER_MINOCCLUSIONROOMRATIO          = 0.0;
  EAXBUFFER_MAXOCCLUSIONROOMRATIO          = 10.0;
  EAXBUFFER_DEFAULTOCCLUSIONROOMRATIO      = 0.5;

  EAXBUFFER_MINOUTSIDEVOLUMEHF             = -10000;
  EAXBUFFER_MAXOUTSIDEVOLUMEHF             = 0;
  EAXBUFFER_DEFAULTOUTSIDEVOLUMEHF         = 0;

  EAXBUFFER_MINAIRABSORPTIONFACTOR         = 0.0;
  EAXBUFFER_MAXAIRABSORPTIONFACTOR         = 10.0;
  EAXBUFFER_DEFAULTAIRABSORPTIONFACTOR     = 1.0;

  EAXBUFFER_DEFAULTFLAGS                   = EAXBUFFERFLAGS_DIRECTHFAUTO or
  EAXBUFFERFLAGS_ROOMAUTO or
  EAXBUFFERFLAGS_ROOMHFAUTO;

  // Material transmission presets
  // 3 values in this order:
  //     1: occlusion (or obstruction)
  //     2: occlusion LF Ratio (or obstruction LF Ratio)
  //     3: occlusion Room Ratio

  // Single window material preset
  EAX_MATERIAL_SINGLEWINDOW                = -2800;
  EAX_MATERIAL_SINGLEWINDOWLF              = 0.71;
  EAX_MATERIAL_SINGLEWINDOWROOMRATIO       = 0.43;

  // Double window material preset
  EAX_MATERIAL_DOUBLEWINDOW                = -5000;
  EAX_MATERIAL_DOUBLEWINDOWHF              = 0.40;
  EAX_MATERIAL_DOUBLEWINDOWROOMRATIO       = 0.24;

  // Thin door material preset
  EAX_MATERIAL_THINDOOR                    = -1800;
  EAX_MATERIAL_THINDOORLF                  = 0.66;
  EAX_MATERIAL_THINDOORROOMRATIO           = 0.66;

  // Thick door material preset
  EAX_MATERIAL_THICKDOOR                   = -4400;
  EAX_MATERIAL_THICKDOORLF                 = 0.64;
  EAX_MATERIAL_THICKDOORROOMRTATION        = 0.27;

  // Wood wall material preset
  EAX_MATERIAL_WOODWALL                    = -4000;
  EAX_MATERIAL_WOODWALLLF                  = 0.50;
  EAX_MATERIAL_WOODWALLROOMRATIO           = 0.30;

  // Brick wall material preset
  EAX_MATERIAL_BRICKWALL                   = -5000;
  EAX_MATERIAL_BRICKWALLLF                 = 0.60;
  EAX_MATERIAL_BRICKWALLROOMRATIO          = 0.24;

  // Stone wall material preset
  EAX_MATERIAL_STONEWALL                   = -6000;
  EAX_MATERIAL_STONEWALLLF                 = 0.68;
  EAX_MATERIAL_STONEWALLROOMRATIO          = 0.20;

  // Curtain material preset
  EAX_MATERIAL_CURTAIN                     = -1200;
  EAX_MATERIAL_CURTAINLF                   = 0.15;
  EAX_MATERIAL_CURTAINROOMRATIO            = 1.00;

implementation

end.
