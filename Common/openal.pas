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

unit openal;

{$IFDEF FPC}
 // Added by bero
 {$MODE Delphi}
 {$IFDEF CPUI386}
  {$DEFINE CPU386}
  {$ASMMODE INTEL}
 {$ENDIF}
 {$IFNDEF WIN32}
  {$LINKLIB c}
 {$ENDIF}
{$ENDIF}

interface

uses
  SysUtils{$IFDEF Win32},Windows{$ENDIF};

{ $ DEFINE ALUT} //define ALUT to use alut.dll

const
{$IFDEF Win32}
  callibname='OpenAL32.dll';
  calutlibname='Alut.dll';
{$ENDIF}
{$IFDEF Linux}
  callibname='libopenal.so';
  calutlibname='libalut.so';
{$ENDIF}


type
  // OpenAL boolean type.
  TALboolean = Boolean;
  PALboolean = ^TALboolean;
  //character
  TALchar = char;
  PALchar = pchar;
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
  // ALC character type
  TALCchar = char;
  PALCchar = pchar;
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

  //END EAX Extension

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
  //AL_SOURCE_TYPE                            = $200;

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

  //Source buffer position information
  AL_SEC_OFFSET                             = $1024;
  AL_SAMPLE_OFFSET                          = $1025;
  AL_BYTE_OFFSET                            = $1026;

  //Source type (Static, Streaming or undetermined)
  //Source is Static if a Buffer has been attached using AL_BUFFER
  //Source is Streaming if one or more Buffers have been attached using alSourceQueueBuffers
  //Source is undetermined when it has the NULL buffer attached
  AL_SOURCE_TYPE                            = $1027;
  AL_STATIC                                 = $1028;
  AL_STREAMING                              = $1029;
  AL_UNDETERMINED                           = $1030;

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
  //AL_DATA                                   =$2005;

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

  //Speed of Sound in units per second
  AL_SPEED_OF_SOUND                         = $C003;

  // Distance models
  //
  // used in conjunction with DistanceModel
  //
  // implicit: NONE, which disances distance attenuation.
  AL_DISTANCE_MODEL                         =$D000;
  AL_INVERSE_DISTANCE                       =$D001;
  AL_INVERSE_DISTANCE_CLAMPED               =$D002;
  AL_LINEAR_DISTANCE                        =$D003;
  AL_LINEAR_DISTANCE_CLAMPED                =$D004;
  AL_EXPONENT_DISTANCE                      =$D005;
  AL_EXPONENT_DISTANCE_CLAMPED              =$D006;

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

  //followed by <int> Num of requested Mono (3D) Sources
  ALC_MONO_SOURCES                         =$1010;

  //followed by <int> Num of requested Stereo Sources
  ALC_STEREO_SOURCES                       =$1011;

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

  //ALC_ENUMERATE_ALL_EXT enums
  ALC_DEFAULT_ALL_DEVICES_SPECIFIER        =$1012;
  ALC_ALL_DEVICES_SPECIFIER                =$1013;

  //Capture extension
  ALC_CAPTURE_DEVICE_SPECIFIER             =$310;
  ALC_CAPTURE_DEFAULT_DEVICE_SPECIFIER     =$311;
  ALC_CAPTURE_SAMPLES                      =$312;

  //EFX Extension
  ALC_EXT_EFX_NAME                         ='ALC_EXT_EFX';

  //Context definitions to be used with alcCreateContext.
  //These values must be unique and not conflict with other
  //al context values.
  ALC_EFX_MAJOR_VERSION                    =$20001;
  ALC_EFX_MINOR_VERSION                    =$20002;
  ALC_MAX_AUXILIARY_SENDS                  =$20003;

  //Listener definitions to be used with alListener functions.
  //These values must be unique and not conflict with other
  //al listener values.
  AL_METERS_PER_UNIT                       =$20004;

  //Source definitions to be used with alSource functions.
  //These values must be unique and not conflict with other
  //al source values.
  AL_DIRECT_FILTER                         =$20005;
  AL_AUXILIARY_SEND_FILTER                 =$20006;
  AL_AIR_ABSORPTION_FACTOR                 =$20007;
  AL_ROOM_ROLLOFF_FACTOR                   =$20008;
  AL_CONE_OUTER_GAINHF                     =$20009;
  AL_DIRECT_FILTER_GAINHF_AUTO             =$2000A;
  AL_AUXILIARY_SEND_FILTER_GAIN_AUTO       =$2000B;
  AL_AUXILIARY_SEND_FILTER_GAINHF_AUTO     =$2000C;

  //Effect object definitions to be used with alEffect functions.

  //Effect parameter value definitions, ranges, and defaults
  //appear farther down in this file.

  //Reverb Parameters
  AL_REVERB_DENSITY                        =$0001;
  AL_REVERB_DIFFUSION                      =$0002;
  AL_REVERB_GAIN                           =$0003;
  AL_REVERB_GAINHF                         =$0004;
  AL_REVERB_DECAY_TIME                     =$0005;
  AL_REVERB_DECAY_HFRATIO                  =$0006;
  AL_REVERB_REFLECTIONS_GAIN               =$0007;
  AL_REVERB_REFLECTIONS_DELAY              =$0008;
  AL_REVERB_LATE_REVERB_GAIN               =$0009;
  AL_REVERB_LATE_REVERB_DELAY              =$000A;
  AL_REVERB_AIR_ABSORPTION_GAINHF          =$000B;
  AL_REVERB_ROOM_ROLLOFF_FACTOR            =$000C;
  AL_REVERB_DECAY_HFLIMIT                  =$000D;

  //Chorus Parameters
  AL_CHORUS_WAVEFORM                       =$0001;
  AL_CHORUS_PHASE                          =$0002;
  AL_CHORUS_RATE                           =$0003;
  AL_CHORUS_DEPTH                          =$0004;
  AL_CHORUS_FEEDBACK                       =$0005;
  AL_CHORUS_DELAY                          =$0006;

  //Distortion Parameters
  AL_DISTORTION_EDGE                       =$0001;
  AL_DISTORTION_GAIN                       =$0002;
  AL_DISTORTION_LOWPASS_CUTOFF             =$0003;
  AL_DISTORTION_EQCENTER                   =$0004;
  AL_DISTORTION_EQBANDWIDTH                =$0005;

  //Echo Parameters
  AL_ECHO_DELAY                            =$0001;
  AL_ECHO_LRDELAY                          =$0002;
  AL_ECHO_DAMPING                          =$0003;
  AL_ECHO_FEEDBACK                         =$0004;
  AL_ECHO_SPREAD                           =$0005;

  //Flanger Parameters
  AL_FLANGER_WAVEFORM                      =$0001;
  AL_FLANGER_PHASE                         =$0002;
  AL_FLANGER_RATE                          =$0003;
  AL_FLANGER_DEPTH                         =$0004;
  AL_FLANGER_FEEDBACK                      =$0005;
  AL_FLANGER_DELAY                         =$0006;

  //Frequencyshifter Parameters
  AL_FREQUENCY_SHIFTER_FREQUENCY           =$0001;
  AL_FREQUENCY_SHIFTER_LEFT_DIRECTION      =$0002;
  AL_FREQUENCY_SHIFTER_RIGHT_DIRECTION     =$0003;

  //Vocalmorpher Parameters
  AL_VOCAL_MORPHER_PHONEMEA                =$0001;
  AL_VOCAL_MORPHER_PHONEMEA_COARSE_TUNING  =$0002;
  AL_VOCAL_MORPHER_PHONEMEB                =$0003;
  AL_VOCAL_MORPHER_PHONEMEB_COARSE_TUNING  =$0004;
  AL_VOCAL_MORPHER_WAVEFORM                =$0005;
  AL_VOCAL_MORPHER_RATE                    =$0006;

  //Pitchshifter Parameters
  AL_PITCH_SHIFTER_COARSE_TUNE             =$0001;
  AL_PITCH_SHIFTER_FINE_TUNE               =$0002;

  //Ringmodulator Parameters
  AL_RING_MODULATOR_FREQUENCY              =$0001;
  AL_RING_MODULATOR_HIGHPASS_CUTOFF        =$0002;
  AL_RING_MODULATOR_WAVEFORM               =$0003;

  //Autowah Parameters
  AL_AUTOWAH_ATTACK_TIME                   =$0001;
  AL_AUTOWAH_RELEASE_TIME                  =$0002;
  AL_AUTOWAH_RESONANCE                     =$0003;
  AL_AUTOWAH_PEAK_GAIN                     =$0004;

  //Compressor Parameters
  AL_COMPRESSOR_ONOFF                      =$0001;

  //Equalizer Parameters
  AL_EQUALIZER_LOW_GAIN                    =$0001;
  AL_EQUALIZER_LOW_CUTOFF                  =$0002;
  AL_EQUALIZER_MID1_GAIN                   =$0003;
  AL_EQUALIZER_MID1_CENTER                 =$0004;
  AL_EQUALIZER_MID1_WIDTH                  =$0005;
  AL_EQUALIZER_MID2_GAIN                   =$0006;
  AL_EQUALIZER_MID2_CENTER                 =$0007;
  AL_EQUALIZER_MID2_WIDTH                  =$0008;
  AL_EQUALIZER_HIGH_GAIN                   =$0009;
  AL_EQUALIZER_HIGH_CUTOFF                 =$000A;

  //Effect type
  AL_EFFECT_FIRST_PARAMETER                =$0000;
  AL_EFFECT_LAST_PARAMETER                 =$8000;
  AL_EFFECT_TYPE                           =$8001;

  //Effect type definitions to be used with AL_EFFECT_TYPE.
  AL_EFFECT_NULL                           =$0000;  // Can also be used as an Effect Object ID
  AL_EFFECT_REVERB                         =$0001;
  AL_EFFECT_CHORUS                         =$0002;
  AL_EFFECT_DISTORTION                     =$0003;
  AL_EFFECT_ECHO                           =$0004;
  AL_EFFECT_FLANGER                        =$0005;
  AL_EFFECT_FREQUENCY_SHIFTER              =$0006;
  AL_EFFECT_VOCAL_MORPHER                  =$0007;
  AL_EFFECT_PITCH_SHIFTER                  =$0008;
  AL_EFFECT_RING_MODULATOR                 =$0009;
  AL_EFFECT_AUTOWAH                        =$000A;
  AL_EFFECT_COMPRESSOR                     =$000B;
  AL_EFFECT_EQUALIZER                      =$000C;

  //Auxiliary Slot object definitions to be used with alAuxiliaryEffectSlot functions.
  AL_EFFECTSLOT_EFFECT                     =$0001;
  AL_EFFECTSLOT_GAIN                       =$0002;
  AL_EFFECTSLOT_AUXILIARY_SEND_AUTO        =$0003;

  //Value to be used as an Auxiliary Slot ID to disable a source send..
  AL_EFFECTSLOT_NULL                       =$0000;

  //Filter object definitions to be used with alFilter functions.

  //Lowpass parameters.
  AL_LOWPASS_GAIN                          =$0001;
  AL_LOWPASS_GAINHF                        =$0002;

  // Highpass Parameters
  AL_HIGHPASS_GAIN                         =$0001;
  AL_HIGHPASS_GAINLF                       =$0002;

  // Bandpass Parameters
  AL_BANDPASS_GAIN                         =$0001;
  AL_BANDPASS_GAINLF                       =$0002;
  AL_BANDPASS_GAINHF                       =$0003;

  // Filter type
  AL_FILTER_FIRST_PARAMETER                =$0000;
  AL_FILTER_LAST_PARAMETER                 =$8000;
  AL_FILTER_TYPE                           =$8001;

  // Filter type definitions to be used with AL_FILTER_TYPE.
  AL_FILTER_NULL                           =$0000;  // Can also be used as a Filter Object ID
  AL_FILTER_LOWPASS                        =$0001;
  AL_FILTER_HIGHPASS                       =$0002;
  AL_FILTER_BANDPASS                       =$0003;


  //Filter ranges and defaults.

  //Lowpass filter
  LOWPASS_MIN_GAIN                                   =0.0;
  LOWPASS_MAX_GAIN                                   =1.0;
  LOWPASS_DEFAULT_GAIN                               =1.0;

  LOWPASS_MIN_GAINHF                                 =0.0;
  LOWPASS_MAX_GAINHF                                 =1.0;
  LOWPASS_DEFAULT_GAINHF                             =1.0;

  //Highpass filter
  HIGHPASS_MIN_GAIN                                  =0.0;
  HIGHPASS_MAX_GAIN                                  =1.0;
  HIGHPASS_DEFAULT_GAIN                              =1.0;

  HIGHPASS_MIN_GAINLF                                =0.0;
  HIGHPASS_MAX_GAINLF                                =1.0;
  HIGHPASS_DEFAULT_GAINLF                            =1.0;

  //Bandpass filter
  BANDPASS_MIN_GAIN                                  =0.0;
  BANDPASS_MAX_GAIN                                  =1.0;
  BANDPASS_DEFAULT_GAIN                              =1.0;

  BANDPASS_MIN_GAINHF                                =0.0;
  BANDPASS_MAX_GAINHF                                =1.0;
  BANDPASS_DEFAULT_GAINHF                            =1.0;

  BANDPASS_MIN_GAINLF                                =0.0;
  BANDPASS_MAX_GAINLF                                =1.0;
  BANDPASS_DEFAULT_GAINLF                            =1.0;

  //Effect parameter structures, value definitions, ranges and defaults.

  //AL reverb effect parameter ranges and defaults
  AL_REVERB_MIN_DENSITY                              =0.0;
  AL_REVERB_MAX_DENSITY                              =1.0;
  AL_REVERB_DEFAULT_DENSITY                          =1.0;

  AL_REVERB_MIN_DIFFUSION                            =0.0;
  AL_REVERB_MAX_DIFFUSION                            =1.0;
  AL_REVERB_DEFAULT_DIFFUSION                        =1.0;

  AL_REVERB_MIN_GAIN                                 =0.0;
  AL_REVERB_MAX_GAIN                                 =1.0;
  AL_REVERB_DEFAULT_GAIN                             =0.32;

  AL_REVERB_MIN_GAINHF                               =0.0;
  AL_REVERB_MAX_GAINHF                               =1.0;
  AL_REVERB_DEFAULT_GAINHF                           =0.89;

  AL_REVERB_MIN_DECAY_TIME                           =0.1;
  AL_REVERB_MAX_DECAY_TIME                           =20.0;
  AL_REVERB_DEFAULT_DECAY_TIME                       =1.49;

  AL_REVERB_MIN_DECAY_HFRATIO                        =0.1;
  AL_REVERB_MAX_DECAY_HFRATIO                        =2.0;
  AL_REVERB_DEFAULT_DECAY_HFRATIO                    =0.83;

  AL_REVERB_MIN_REFLECTIONS_GAIN                     =0.0;
  AL_REVERB_MAX_REFLECTIONS_GAIN                     =3.16;
  AL_REVERB_DEFAULT_REFLECTIONS_GAIN                 =0.05;

  AL_REVERB_MIN_REFLECTIONS_DELAY                    =0.0;
  AL_REVERB_MAX_REFLECTIONS_DELAY                    =0.3;
  AL_REVERB_DEFAULT_REFLECTIONS_DELAY                =0.007;

  AL_REVERB_MIN_LATE_REVERB_GAIN                     =0.0;
  AL_REVERB_MAX_LATE_REVERB_GAIN                     =10.0;
  AL_REVERB_DEFAULT_LATE_REVERB_GAIN                 =1.26;

  AL_REVERB_MIN_LATE_REVERB_DELAY                    =0.0;
  AL_REVERB_MAX_LATE_REVERB_DELAY                    =0.1;
  AL_REVERB_DEFAULT_LATE_REVERB_DELAY                =0.011;

  AL_REVERB_MIN_AIR_ABSORPTION_GAINHF                =0.892;
  AL_REVERB_MAX_AIR_ABSORPTION_GAINHF                =1.0;
  AL_REVERB_DEFAULT_AIR_ABSORPTION_GAINHF            =0.994;

  AL_REVERB_MIN_ROOM_ROLLOFF_FACTOR                  =0.0;
  AL_REVERB_MAX_ROOM_ROLLOFF_FACTOR                  =10.0;
  AL_REVERB_DEFAULT_ROOM_ROLLOFF_FACTOR              =0.0;

  AL_REVERB_MIN_DECAY_HFLIMIT                        =AL_FALSE;
  AL_REVERB_MAX_DECAY_HFLIMIT                        =AL_TRUE;
  AL_REVERB_DEFAULT_DECAY_HFLIMIT                    =AL_TRUE;

  //AL chorus effect parameter ranges and defaults
  AL_CHORUS_MIN_WAVEFORM                             =0;
  AL_CHORUS_MAX_WAVEFORM                             =1;
  AL_CHORUS_DEFAULT_WAVEFORM                         =1;

  AL_CHORUS_WAVEFORM_SINUSOID                        =0;
  AL_CHORUS_WAVEFORM_TRIANGLE                        =1;

  AL_CHORUS_MIN_PHASE                                =-180;
  AL_CHORUS_MAX_PHASE                                =180;
  AL_CHORUS_DEFAULT_PHASE                            =90;

  AL_CHORUS_MIN_RATE                                 =0.0;
  AL_CHORUS_MAX_RATE                                 =10.0;
  AL_CHORUS_DEFAULT_RATE                             =1.1;

  AL_CHORUS_MIN_DEPTH                                =0.0;
  AL_CHORUS_MAX_DEPTH                                =1.0;
  AL_CHORUS_DEFAULT_DEPTH                            =0.1;

  AL_CHORUS_MIN_FEEDBACK                             =-1.0;
  AL_CHORUS_MAX_FEEDBACK                             =1.0;
  AL_CHORUS_DEFAULT_FEEDBACK                         =0.25;

  AL_CHORUS_MIN_DELAY                                =0.0;
  AL_CHORUS_MAX_DELAY                                =0.016;
  AL_CHORUS_DEFAULT_DELAY                            =0.016;

  //AL distortion effect parameter ranges and defaults
  AL_DISTORTION_MIN_EDGE                             =0.0;
  AL_DISTORTION_MAX_EDGE                             =1.0;
  AL_DISTORTION_DEFAULT_EDGE                         =0.2;

  AL_DISTORTION_MIN_GAIN                             =0.01;
  AL_DISTORTION_MAX_GAIN                             =1.0;
  AL_DISTORTION_DEFAULT_GAIN                         =0.05;

  AL_DISTORTION_MIN_LOWPASS_CUTOFF                   =80.0;
  AL_DISTORTION_MAX_LOWPASS_CUTOFF                   =24000.0;
  AL_DISTORTION_DEFAULT_LOWPASS_CUTOFF               =8000.0;

  AL_DISTORTION_MIN_EQCENTER                         =80.0;
  AL_DISTORTION_MAX_EQCENTER                         =24000.0;
  AL_DISTORTION_DEFAULT_EQCENTER                     =3600.0;

  AL_DISTORTION_MIN_EQBANDWIDTH                      =80.0;
  AL_DISTORTION_MAX_EQBANDWIDTH                      =24000.0;
  AL_DISTORTION_DEFAULT_EQBANDWIDTH                  =3600.0;

  //AL echo effect parameter ranges and defaults
  AL_ECHO_MIN_DELAY                                  =0.0;
  AL_ECHO_MAX_DELAY                                  =0.207;
  AL_ECHO_DEFAULT_DELAY                              =0.1;

  AL_ECHO_MIN_LRDELAY                                =0.0;
  AL_ECHO_MAX_LRDELAY                                =0.404;
  AL_ECHO_DEFAULT_LRDELAY                            =0.1;

  AL_ECHO_MIN_DAMPING                                =0.0;
  AL_ECHO_MAX_DAMPING                                =0.99;
  AL_ECHO_DEFAULT_DAMPING                            =0.5;

  AL_ECHO_MIN_FEEDBACK                               =0.0;
  AL_ECHO_MAX_FEEDBACK                               =1.0;
  AL_ECHO_DEFAULT_FEEDBACK                           =0.5;

  AL_ECHO_MIN_SPREAD                                 =-1.0;
  AL_ECHO_MAX_SPREAD                                 =1.0;
  AL_ECHO_DEFAULT_SPREAD                             =-1.0;

  //AL flanger effect parameter ranges and defaults
  AL_FLANGER_MIN_WAVEFORM                            =0;
  AL_FLANGER_MAX_WAVEFORM                            =1;
  AL_FLANGER_DEFAULT_WAVEFORM                        =1;

  AL_FLANGER_WAVEFORM_SINUSOID                       =0;
  AL_FLANGER_WAVEFORM_TRIANGLE                       =1;

  AL_FLANGER_MIN_PHASE                               =-180;
  AL_FLANGER_MAX_PHASE                               =180;
  AL_FLANGER_DEFAULT_PHASE                           =0;

  AL_FLANGER_MIN_RATE                                =0.0;
  AL_FLANGER_MAX_RATE                                =10.0;
  AL_FLANGER_DEFAULT_RATE                            =0.27;

  AL_FLANGER_MIN_DEPTH                               =0.0;
  AL_FLANGER_MAX_DEPTH                               =1.0;
  AL_FLANGER_DEFAULT_DEPTH                           =1.0;

  AL_FLANGER_MIN_FEEDBACK                            =-1.0;
  AL_FLANGER_MAX_FEEDBACK                            =1.0;
  AL_FLANGER_DEFAULT_FEEDBACK                        =-0.5;

  AL_FLANGER_MIN_DELAY                               =0.0;
  AL_FLANGER_MAX_DELAY                               =0.004;
  AL_FLANGER_DEFAULT_DELAY                           =0.002;

  //AL frequency shifter effect parameter ranges and defaults
  AL_FREQUENCY_SHIFTER_MIN_FREQUENCY                 =0.0;
  AL_FREQUENCY_SHIFTER_MAX_FREQUENCY                 =24000.0;
  AL_FREQUENCY_SHIFTER_DEFAULT_FREQUENCY             =0.0;

  AL_FREQUENCY_SHIFTER_MIN_LEFT_DIRECTION            =0;
  AL_FREQUENCY_SHIFTER_MAX_LEFT_DIRECTION            =2;
  AL_FREQUENCY_SHIFTER_DEFAULT_LEFT_DIRECTION        =0;

  AL_FREQUENCY_SHIFTER_MIN_RIGHT_DIRECTION           =0;
  AL_FREQUENCY_SHIFTER_MAX_RIGHT_DIRECTION           =2;
  AL_FREQUENCY_SHIFTER_DEFAULT_RIGHT_DIRECTION       =0;

  AL_FREQUENCY_SHIFTER_DIRECTION_DOWN                =0;
  AL_FREQUENCY_SHIFTER_DIRECTION_UP                  =1;
  AL_FREQUENCY_SHIFTER_DIRECTION_OFF                 =2;

  //AL vocal morpher effect parameter ranges and defaults
  AL_VOCAL_MORPHER_MIN_PHONEMEA                      =0;
  AL_VOCAL_MORPHER_MAX_PHONEMEA                      =29;
  AL_VOCAL_MORPHER_DEFAULT_PHONEMEA                  =0;

  AL_VOCAL_MORPHER_MIN_PHONEMEA_COARSE_TUNING	       =-24;
  AL_VOCAL_MORPHER_MAX_PHONEMEA_COARSE_TUNING	       =24;
  AL_VOCAL_MORPHER_DEFAULT_PHONEMEA_COARSE_TUNING    =0;

  AL_VOCAL_MORPHER_MIN_PHONEMEB                      =0;
  AL_VOCAL_MORPHER_MAX_PHONEMEB                      =29;
  AL_VOCAL_MORPHER_DEFAULT_PHONEMEB                  =10;

  AL_VOCAL_MORPHER_PHONEME_A                         =0;
  AL_VOCAL_MORPHER_PHONEME_E                         =1;
  AL_VOCAL_MORPHER_PHONEME_I                         =2;
  AL_VOCAL_MORPHER_PHONEME_O                         =3;
  AL_VOCAL_MORPHER_PHONEME_U                         =4;
  AL_VOCAL_MORPHER_PHONEME_AA                        =5;
  AL_VOCAL_MORPHER_PHONEME_AE                        =6;
  AL_VOCAL_MORPHER_PHONEME_AH                        =7;
  AL_VOCAL_MORPHER_PHONEME_AO                        =8;
  AL_VOCAL_MORPHER_PHONEME_EH                        =9;
  AL_VOCAL_MORPHER_PHONEME_ER                        =10;
  AL_VOCAL_MORPHER_PHONEME_IH                        =11;
  AL_VOCAL_MORPHER_PHONEME_IY                        =12;
  AL_VOCAL_MORPHER_PHONEME_UH                        =13;
  AL_VOCAL_MORPHER_PHONEME_UW                        =14;
  AL_VOCAL_MORPHER_PHONEME_B                         =15;
  AL_VOCAL_MORPHER_PHONEME_D                         =16;
  AL_VOCAL_MORPHER_PHONEME_F                         =17;
  AL_VOCAL_MORPHER_PHONEME_G                         =18;
  AL_VOCAL_MORPHER_PHONEME_J                         =19;
  AL_VOCAL_MORPHER_PHONEME_K                         =20;
  AL_VOCAL_MORPHER_PHONEME_L                         =21;
  AL_VOCAL_MORPHER_PHONEME_M                         =22;
  AL_VOCAL_MORPHER_PHONEME_N                         =23;
  AL_VOCAL_MORPHER_PHONEME_P                         =24;
  AL_VOCAL_MORPHER_PHONEME_R                         =25;
  AL_VOCAL_MORPHER_PHONEME_S                         =26;
  AL_VOCAL_MORPHER_PHONEME_T                         =27;
  AL_VOCAL_MORPHER_PHONEME_V                         =28;
  AL_VOCAL_MORPHER_PHONEME_Z                         =29;

  AL_VOCAL_MORPHER_MIN_PHONEMEB_COARSE_TUNING        =-24;
  AL_VOCAL_MORPHER_MAX_PHONEMEB_COARSE_TUNING        =24;
  AL_VOCAL_MORPHER_DEFAULT_PHONEMEB_COARSE_TUNING    =0;

  AL_VOCAL_MORPHER_MIN_WAVEFORM                      =0;
  AL_VOCAL_MORPHER_MAX_WAVEFORM                      =2;
  AL_VOCAL_MORPHER_DEFAULT_WAVEFORM                  =0;

  AL_VOCAL_MORPHER_WAVEFORM_SINUSOID                 =0;
  AL_VOCAL_MORPHER_WAVEFORM_TRIANGLE                 =1;
  AL_VOCAL_MORPHER_WAVEFORM_SAWTOOTH                 =2;

  AL_VOCAL_MORPHER_MIN_RATE                          =0.0;
  AL_VOCAL_MORPHER_MAX_RATE                          =10.0;
  AL_VOCAL_MORPHER_DEFAULT_RATE                      =1.41;

  //AL pitch shifter effect parameter ranges and defaults
  AL_PITCH_SHIFTER_MIN_COARSE_TUNE                   =-12;
  AL_PITCH_SHIFTER_MAX_COARSE_TUNE                   =12;
  AL_PITCH_SHIFTER_DEFAULT_COARSE_TUNE               =12;

  AL_PITCH_SHIFTER_MIN_FINE_TUNE                     =-50;
  AL_PITCH_SHIFTER_MAX_FINE_TUNE                     =50;
  AL_PITCH_SHIFTER_DEFAULT_FINE_TUNE                 =0;

  //AL ring modulator effect parameter ranges and defaults
  AL_RING_MODULATOR_MIN_FREQUENCY                    =0.0;
  AL_RING_MODULATOR_MAX_FREQUENCY                    =8000.0;
  AL_RING_MODULATOR_DEFAULT_FREQUENCY                =440.0;

  AL_RING_MODULATOR_MIN_HIGHPASS_CUTOFF              =0.0;
  AL_RING_MODULATOR_MAX_HIGHPASS_CUTOFF              =24000.0;
  AL_RING_MODULATOR_DEFAULT_HIGHPASS_CUTOFF          =800.0;

  AL_RING_MODULATOR_MIN_WAVEFORM                     =0;
  AL_RING_MODULATOR_MAX_WAVEFORM                     =2;
  AL_RING_MODULATOR_DEFAULT_WAVEFORM                 =0;

  AL_RING_MODULATOR_SINUSOID                         =0;
  AL_RING_MODULATOR_SAWTOOTH                         =1;
  AL_RING_MODULATOR_SQUARE                           =2;

  //AL autowah effect parameter ranges and defaults
  AL_AUTOWAH_MIN_ATTACK_TIME                         =0.0001;
  AL_AUTOWAH_MAX_ATTACK_TIME                         =1.0;
  AL_AUTOWAH_DEFAULT_ATTACK_TIME                     =0.06;

  AL_AUTOWAH_MIN_RELEASE_TIME                        =0.0001;
  AL_AUTOWAH_MAX_RELEASE_TIME                        =1.0;
  AL_AUTOWAH_DEFAULT_RELEASE_TIME                    =0.06;

  AL_AUTOWAH_MIN_RESONANCE                           =2.0;
  AL_AUTOWAH_MAX_RESONANCE                           =1000.0;
  AL_AUTOWAH_DEFAULT_RESONANCE                       =1000.0;

  AL_AUTOWAH_MIN_PEAK_GAIN                           =0.00003;
  AL_AUTOWAH_MAX_PEAK_GAIN                           =31621.0;
  AL_AUTOWAH_DEFAULT_PEAK_GAIN                       =11.22;

  //AL compressor effect parameter ranges and defaults
  AL_COMPRESSOR_MIN_ONOFF                            =0;
  AL_COMPRESSOR_MAX_ONOFF                            =1;
  AL_COMPRESSOR_DEFAULT_ONOFF                        =1;

  //AL equalizer effect parameter ranges and defaults
  AL_EQUALIZER_MIN_LOW_GAIN                          =0.126;
  AL_EQUALIZER_MAX_LOW_GAIN                          =7.943;
  AL_EQUALIZER_DEFAULT_LOW_GAIN                      =1.0;

  AL_EQUALIZER_MIN_LOW_CUTOFF                        =50.0;
  AL_EQUALIZER_MAX_LOW_CUTOFF                        =800.0;
  AL_EQUALIZER_DEFAULT_LOW_CUTOFF                    =200.0;

  AL_EQUALIZER_MIN_MID1_GAIN                         =0.126;
  AL_EQUALIZER_MAX_MID1_GAIN                         =7.943;
  AL_EQUALIZER_DEFAULT_MID1_GAIN                     =1.0;

  AL_EQUALIZER_MIN_MID1_CENTER                       =200.0;
  AL_EQUALIZER_MAX_MID1_CENTER                       =3000.0;
  AL_EQUALIZER_DEFAULT_MID1_CENTER                   =500.0;

  AL_EQUALIZER_MIN_MID1_WIDTH                        =0.01;
  AL_EQUALIZER_MAX_MID1_WIDTH                        =1.0;
  AL_EQUALIZER_DEFAULT_MID1_WIDTH                    =1.0;

  AL_EQUALIZER_MIN_MID2_GAIN                         =0.126;
  AL_EQUALIZER_MAX_MID2_GAIN                         =7.943;
  AL_EQUALIZER_DEFAULT_MID2_GAIN                     =1.0;

  AL_EQUALIZER_MIN_MID2_CENTER                       =1000.0;
  AL_EQUALIZER_MAX_MID2_CENTER                       =8000.0;
  AL_EQUALIZER_DEFAULT_MID2_CENTER                   =3000.0;

  AL_EQUALIZER_MIN_MID2_WIDTH                        =0.01;
  AL_EQUALIZER_MAX_MID2_WIDTH                        =1.0;
  AL_EQUALIZER_DEFAULT_MID2_WIDTH                    =1.0;

  AL_EQUALIZER_MIN_HIGH_GAIN                         =0.126;
  AL_EQUALIZER_MAX_HIGH_GAIN                         =7.943;
  AL_EQUALIZER_DEFAULT_HIGH_GAIN                     =1.0;

  AL_EQUALIZER_MIN_HIGH_CUTOFF                       =4000.0;
  AL_EQUALIZER_MAX_HIGH_CUTOFF                       =16000.0;
  AL_EQUALIZER_DEFAULT_HIGH_CUTOFF                   =6000.0;

  //Source parameter value definitions, ranges and defaults.

  AL_MIN_AIR_ABSORPTION_FACTOR                       =0.0;
  AL_MAX_AIR_ABSORPTION_FACTOR                       =10.0;
  AL_DEFAULT_AIR_ABSORPTION_FACTOR                   =0.0;

  AL_MIN_ROOM_ROLLOFF_FACTOR                         =0.0;
  AL_MAX_ROOM_ROLLOFF_FACTOR                         =10.0;
  AL_DEFAULT_ROOM_ROLLOFF_FACTOR                     =0.0;

  AL_MIN_CONE_OUTER_GAINHF                           =0.0;
  AL_MAX_CONE_OUTER_GAINHF                           =1.0;
  AL_DEFAULT_CONE_OUTER_GAINHF                       =1.0;

  AL_MIN_DIRECT_FILTER_GAINHF_AUTO                   =AL_FALSE;
  AL_MAX_DIRECT_FILTER_GAINHF_AUTO                   =AL_TRUE;
  AL_DEFAULT_DIRECT_FILTER_GAINHF_AUTO               =AL_TRUE;

  AL_MIN_AUXILIARY_SEND_FILTER_GAIN_AUTO             =AL_FALSE;
  AL_MAX_AUXILIARY_SEND_FILTER_GAIN_AUTO             =AL_TRUE;
  AL_DEFAULT_AUXILIARY_SEND_FILTER_GAIN_AUTO         =AL_TRUE;

  AL_MIN_AUXILIARY_SEND_FILTER_GAINHF_AUTO           =AL_FALSE;
  AL_MAX_AUXILIARY_SEND_FILTER_GAINHF_AUTO           =AL_TRUE;
  AL_DEFAULT_AUXILIARY_SEND_FILTER_GAINHF_AUTO       =AL_TRUE;

  //Listener parameter value definitions, ranges and defaults.
  //AL_MIN_METERS_PER_UNIT                             =FLT_MIN;
  //AL_MAX_METERS_PER_UNIT                             =FLT_MAX;
  AL_DEFAULT_METERS_PER_UNIT                         =1.0;


  //EFX AEX extenstion

  //Effect object definitions to be used with alEffect functions.

  //Effect parameter value definitions, ranges, and defaults
  //appear farther down in this file.

  //AL EAXReverb effect parameters.
  AL_EAXREVERB_DENSITY                     =$0001;
  AL_EAXREVERB_DIFFUSION                   =$0002;
  AL_EAXREVERB_GAIN                        =$0003;
  AL_EAXREVERB_GAINHF                      =$0004;
  AL_EAXREVERB_GAINLF                      =$0005;
  AL_EAXREVERB_DECAY_TIME                  =$0006;
  AL_EAXREVERB_DECAY_HFRATIO               =$0007;
  AL_EAXREVERB_DECAY_LFRATIO               =$0008;
  AL_EAXREVERB_REFLECTIONS_GAIN            =$0009;
  AL_EAXREVERB_REFLECTIONS_DELAY           =$000A;
  AL_EAXREVERB_REFLECTIONS_PAN             =$000B;
  AL_EAXREVERB_LATE_REVERB_GAIN            =$000C;
  AL_EAXREVERB_LATE_REVERB_DELAY           =$000D;
  AL_EAXREVERB_LATE_REVERB_PAN             =$000E;
  AL_EAXREVERB_ECHO_TIME                   =$000F;
  AL_EAXREVERB_ECHO_DEPTH                  =$0010;
  AL_EAXREVERB_MODULATION_TIME             =$0011;
  AL_EAXREVERB_MODULATION_DEPTH            =$0012;
  AL_EAXREVERB_AIR_ABSORPTION_GAINHF       =$0013;
  AL_EAXREVERB_HFREFERENCE                 =$0014;
  AL_EAXREVERB_LFREFERENCE                 =$0015;
  AL_EAXREVERB_ROOM_ROLLOFF_FACTOR         =$0016;
  AL_EAXREVERB_DECAY_HFLIMIT               =$0017;

  //Effect type definitions to be used with AL_EFFECT_TYPE.
  AL_EFFECT_EAXREVERB                      =$8000;

  //Effect parameter structures, value definitions, ranges and defaults.

  //AL reverb effect parameter ranges and defaults

  AL_EAXREVERB_MIN_DENSITY                 =0.0;
  AL_EAXREVERB_MAX_DENSITY                 =1.0;
  AL_EAXREVERB_DEFAULT_DENSITY             =1.0;

  AL_EAXREVERB_MIN_DIFFUSION               =0.0;
  AL_EAXREVERB_MAX_DIFFUSION               =1.0;
  AL_EAXREVERB_DEFAULT_DIFFUSION           =1.0;

  AL_EAXREVERB_MIN_GAIN                    =0.0;
  AL_EAXREVERB_MAX_GAIN                    =1.0;
  AL_EAXREVERB_DEFAULT_GAIN                =0.32;

  AL_EAXREVERB_MIN_GAINHF                  =0.0;
  AL_EAXREVERB_MAX_GAINHF                  =1.0;
  AL_EAXREVERB_DEFAULT_GAINHF              =0.89;

  AL_EAXREVERB_MIN_GAINLF                  =0.0;
  AL_EAXREVERB_MAX_GAINLF                  =1.0;
  AL_EAXREVERB_DEFAULT_GAINLF              =1.0;

  AL_EAXREVERB_MIN_DECAY_TIME              =0.1;
  AL_EAXREVERB_MAX_DECAY_TIME              =20.0;
  AL_EAXREVERB_DEFAULT_DECAY_TIME          =1.49;

  AL_EAXREVERB_MIN_DECAY_HFRATIO           =0.1;
  AL_EAXREVERB_MAX_DECAY_HFRATIO           =2.0;
  AL_EAXREVERB_DEFAULT_DECAY_HFRATIO       =0.83;

  AL_EAXREVERB_MIN_DECAY_LFRATIO           =0.1;
  AL_EAXREVERB_MAX_DECAY_LFRATIO           =2.0;
  AL_EAXREVERB_DEFAULT_DECAY_LFRATIO       =1.0;

  AL_EAXREVERB_MIN_REFLECTIONS_GAIN        =0.0;
  AL_EAXREVERB_MAX_REFLECTIONS_GAIN        =3.16;
  AL_EAXREVERB_DEFAULT_REFLECTIONS_GAIN    =0.05;

  AL_EAXREVERB_MIN_REFLECTIONS_DELAY       =0.0;
  AL_EAXREVERB_MAX_REFLECTIONS_DELAY       =0.3;
  AL_EAXREVERB_DEFAULT_REFLECTIONS_DELAY   =0.007;

  AL_EAXREVERB_DEFAULT_REFLECTIONS_PAN: array[0..2] of TALFloat = (0.0, 0.0, 0.0);

  AL_EAXREVERB_MIN_LATE_REVERB_GAIN        =0.0;
  AL_EAXREVERB_MAX_LATE_REVERB_GAIN        =10.0;
  AL_EAXREVERB_DEFAULT_LATE_REVERB_GAIN    =1.26;

  AL_EAXREVERB_MIN_LATE_REVERB_DELAY       =0.0;
  AL_EAXREVERB_MAX_LATE_REVERB_DELAY       =0.1;
  AL_EAXREVERB_DEFAULT_LATE_REVERB_DELAY   =0.011;

  AL_EAXREVERB_DEFAULT_LATE_REVERB_PAN: array[0..2] of TALFloat = (0.0, 0.0, 0.0);

  AL_EAXREVERB_MIN_ECHO_TIME               =0.075;
  AL_EAXREVERB_MAX_ECHO_TIME               =0.25;
  AL_EAXREVERB_DEFAULT_ECHO_TIME           =0.25;

  AL_EAXREVERB_MIN_ECHO_DEPTH              =0.0;
  AL_EAXREVERB_MAX_ECHO_DEPTH              =1.0;
  AL_EAXREVERB_DEFAULT_ECHO_DEPTH          =0.0;

  AL_EAXREVERB_MIN_MODULATION_TIME         =0.04;
  AL_EAXREVERB_MAX_MODULATION_TIME         =4.0;
  AL_EAXREVERB_DEFAULT_MODULATION_TIME     =0.25;

  AL_EAXREVERB_MIN_MODULATION_DEPTH        =0.0;
  AL_EAXREVERB_MAX_MODULATION_DEPTH        =1.0;
  AL_EAXREVERB_DEFAULT_MODULATION_DEPTH    =0.0;

  AL_EAXREVERB_MIN_AIR_ABSORPTION_GAINHF   =0.892;
  AL_EAXREVERB_MAX_AIR_ABSORPTION_GAINHF   =1.0;
  AL_EAXREVERB_DEFAULT_AIR_ABSORPTION_GAINHF =0.994;

  AL_EAXREVERB_MIN_HFREFERENCE             =1000.0;
  AL_EAXREVERB_MAX_HFREFERENCE             =20000.0;
  AL_EAXREVERB_DEFAULT_HFREFERENCE         =5000.0;

  AL_EAXREVERB_MIN_LFREFERENCE             =20.0;
  AL_EAXREVERB_MAX_LFREFERENCE             =1000.0;
  AL_EAXREVERB_DEFAULT_LFREFERENCE         =250.0;

  AL_EAXREVERB_MIN_ROOM_ROLLOFF_FACTOR     =0.0;
  AL_EAXREVERB_MAX_ROOM_ROLLOFF_FACTOR     =10.0;
  AL_EAXREVERB_DEFAULT_ROOM_ROLLOFF_FACTOR =0.0;

  AL_EAXREVERB_MIN_DECAY_HFLIMIT           = AL_FALSE;
  AL_EAXREVERB_MAX_DECAY_HFLIMIT           = AL_TRUE;
  AL_EAXREVERB_DEFAULT_DECAY_HFLIMIT       = AL_TRUE;

  //END EFX AEX extension

  // EFX utils version 1.0
type
  _EFXEAXREVERBPROPERTIES = packed record
	  flDensity: single;
	  flDiffusion: single;
	  flGain: single;
	  flGainHF: single;
	  flGainLF: single;
	  flDecayTime: single;
	  flDecayHFRatio: single;
	  flDecayLFRatio: single;
	  flReflectionsGain: single;
	  flReflectionsDelay: single;
	  flReflectionsPan: packed array[0..2] of single;
	  flLateReverbGain: single;
	  flLateReverbDelay: single;
	  flLateReverbPan: packed array[0..2] of single;
	  flEchoTime: single;
	  flEchoDepth: single;
	  flModulationTime: single;
	  flModulationDepth: single;
	  flAirAbsorptionGainHF: single;
	  flHFReference: single;
	  flLFReference: single;
	  flRoomRolloffFactor: single;
	  iDecayHFLimit: integer;
  end;
  EFXEAXREVERBPROPERTIES = _EFXEAXREVERBPROPERTIES;
  PEFXEAXREVERBPROPERTIES = ^_EFXEAXREVERBPROPERTIES;

  _EFXLOWPASSFILTER = packed record
	  flGain: single;
	  flGainHF: single;
  end;
  EFXLOWPASSFILTER = _EFXLOWPASSFILTER;
  PEFXLOWPASSFILTER = ^_EFXLOWPASSFILTER;

  //END EFX extension

const
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

  //END EAX extension

var
  //OpenAL Maintenance Functions
  //State Management and Query.
  //Error Handling.
  //Extension Support.

  //Renderer State management.
  alEnable: procedure(capability: TALenum); cdecl;
  alDisable: procedure(capability: TALenum); cdecl;
  alIsEnabled: function(capability: TALenum): TALboolean; cdecl;

  //Application preferences for driver performance choices.
  alHint: procedure(target, mode: TALenum); cdecl;

  //State retrieval.
  alGetBooleanv: procedure(param: TALenum; data: PALboolean); cdecl;
  alGetIntegerv: procedure(param: TALenum; data: PALint); cdecl;
  alGetFloatv: procedure(param: TALenum; data: PALfloat); cdecl;
  alGetDoublev: procedure(param: TALenum; data: PALdouble); cdecl;
  alGetString: function(param: TALenum): PALubyte; cdecl;

  //State retrieval.through return value ( for compatibility )
  alGetBoolean: function(param: TALenum): TALboolean; cdecl;
  alGetInteger: function(param: TALenum): TALint; cdecl;
  alGetFloat: function(param: TALenum): TALfloat; cdecl;
  alGetDouble: function(param: TALenum): TALdouble; cdecl;

  //ERROR support.

  //Obtain the most recent error generated in the AL state machine.
  alGetError: function: TALenum; cdecl;

  //EXTENSION support.

  //Obtain the address of a function (usually an extension)
  // with the name fname. All addresses are context-independent.
  alIsExtensionPresent: function(fname: Pchar): TALboolean; cdecl;

  //Obtain the address of a function (usually an extension)
  //with the name fname. All addresses are context-independent.
  alGetProcAddress: function(fname: PALuByte): Pointer; cdecl;

  //Obtain the integer value of an enumeration (usually an extension) with the name ename.
  alGetEnumValue: function(ename: PALuByte): TALenum; cdecl;

  //LISTENER
  //Listener is the sample position for a given context.
  //The multi-channel (usually stereo) output stream generated
  //by the mixer is parametrized by this Listener object:
  //its position and velocity relative to Sources, within
  //occluder and reflector geometry.

  //Listener Gain:  default 1.0f.
  alListenerf: procedure(param: TALenum; value: TALfloat); cdecl;

  //Listener Position.
  //Listener Velocity.
  alListener3f: procedure(param: TALenum; f1: TALfloat; f2: TALfloat; f3: TALfloat); cdecl;

  //Listener Position:        array [0..2] of TALfloat
  //Listener Velocity:        array [0..2] of TALfloat
  //Listener Orientation:     array [0..5] of TALfloat  forward and up vector.
  alListenerfv: procedure(param: TALenum; values: PALfloat); cdecl;

  //Listener Environment:  default 0.
  alListeneri: procedure(param: TAlenum; value: TALint); cdecl;

  alListener3i: procedure(param: TALenum; value1: TALint; value2: TALint; value3: TALint );

  alListeneriv: procedure(param: TALenum; const values: PALint );

  //TODO: verder ontbrekende functies toevoegen.

  //Retrieve listener information
  alGetListeneriv: procedure(param: TALenum; values: PALint); cdecl;
  alGetListenerfv: procedure(param: TALenum; values: PALfloat); cdecl;

  //SOURCE
  //Source objects are by default localized. Sources
  //take the PCM data provided in the specified Buffer,
  //apply Source-specific modifications, and then
  //submit them to be mixed according to spatial
  //arrangement etc.

  //Create Source objects.
  alGenSources: procedure(n: TALsizei; sources: PALuint); cdecl;

  //Delete Source objects.
  alDeleteSources: procedure(n: TALsizei; sources: PALuint); cdecl;

  //Verify a handle is a valid Source.
  alIsSource: function(id: TALuint): TALboolean; cdecl;

  //Set an integer parameter for a Source object.
  alSourcei: procedure(source: TALuint; param: TALenum; value: TALint); cdecl;
  //Set an 3 integer parameter for a Source object.
  alSource3i: procedure(source: TALuint; param: TALenum; v1: TALint; v2: TALint; v3: TALint); cdecl;
  //Set an integer vector parameter for a Source object.
  alSourceiv: procedure(source: TALuint; param: TALenum; values: PALint); cdecl;

  //Set a float parameter for a Source object.
  alSourcef: procedure(source: TALuint; param: TALenum; value: TALfloat); cdecl;
  //Set a 3 float parameter for a Source object.
  alSource3f: procedure(source: TALuint; param: TALenum; v1: TALfloat; v2: TALfloat; v3: TALfloat); cdecl;
  //Set a float vector parameter for a Source object.
  alSourcefv: procedure(source: TALuint; param: TALenum; values: PALfloat); cdecl;

  //Get an integer scalar parameter for a Source object.
  alGetSourcei: procedure(source: TALuint; param: TALenum; value: PALint); cdecl;
  //Get three integer scalar parameter for a Source object.
  alGetSource3i: procedure(source: TALuint; param: TALenum; v1: PALint; v2: PALint; v3: PALint); cdecl;
  //Get a integer vector parameter for a Source object.
  alGetSourceiv: procedure(source: TALuint; param: TALenum; values: PALint); cdecl;

  //Get a float scalar parameter for a Source object.
  alGetSourcef: procedure(source: TALuint; param: TALenum; value: PALfloat); cdecl;
  //Get three float scalar parameter for a Source object.
  alGetSource3f: procedure(source: TALuint; param: TALenum; v1: PALfloat; v2: PALfloat; v3: PALfloat); cdecl;
  //Get a float vector parameter for a Source object.
  alGetSourcefv: procedure(source: TALuint; param: TALenum; values: PALfloat); cdecl;

  //Activate a source, start replay.
  alSourcePlay: procedure(source: TALuint); cdecl;

  //Pause a source,
  //temporarily remove it from the mixer list.
  alSourcePause: procedure(source: TALuint); cdecl;

  //Stop a source,
  //temporarily remove it from the mixer list,
  //and reset its internal state to pre-Play.
  //To remove a Source completely, it has to be
  //deleted following Stop, or before Play.
  alSourceStop: procedure(source: TALuint); cdecl;


  //Rewind a souce.  Stopped paused and playing sources,
  //resets the offset into the PCM data and sets state to
  //AL_INITIAL.
  alSourceRewind: procedure(source: TALuint); cdecl;

  //vector forms of those functions we all love
  alSourcePlayv: procedure(n: TALsizei; sources: PALuint); cdecl;
  alSourceStopv: procedure(n: TALsizei; sources: PALuint); cdecl;
  alSourceRewindv: procedure(n: TALsizei; sources: PALuint); cdecl;
  alSourcePausev: procedure(n: TALsizei; sources: PALuint); cdecl;

    //Queue stuff
  alSourceQueueBuffers: procedure(source: TALuint; n: TALsizei; buffers: PALuint); cdecl;
  alSourceUnqueueBuffers: procedure(source: TALuint; n: TALsizei; buffers: PALuint); cdecl;

  //BUFFER
  //Buffer objects are storage space for sample data.
  //Buffers are referred to by Sources. There can be more than
  //one Source using the same Buffer data. If Buffers have
  //to be duplicated on a per-Source basis, the driver has to
  //take care of allocation, copying, and deallocation as well
  //as propagating buffer data changes.

  //Buffer object generation.
  alGenBuffers: procedure(n: TALsizei; buffers: PALuint); cdecl;
  alDeleteBuffers: procedure(n: TALsizei; buffers: PALuint); cdecl;
  alIsBuffer: function(buffer: TALuint): TALboolean; cdecl;
  //Specify the data to be filled into a buffer.
  alBufferData: procedure(buffer: TALuint; format: TALenum; data: Pointer; size, freq: TALsizei); cdecl;
  //set parameter for an buffer object
  alBufferi: procedure(buffer: TALuint; param: TALenum; value: TALint); cdecl;
  alBuffer3i: procedure(buffer: TALuint; param: TALenum; v1: TALint; v2: TALint; v3: TALint); cdecl;
  alBufferiv: procedure(buffer: TALuint; param: TALenum; values: PALint); cdecl;

  alBufferf: procedure(buffer: TALuint; param: TALenum; value: TALfloat); cdecl;
  alBuffer3f: procedure(buffer: TALuint; param: TALenum; v1: TALfloat; v2: TALfloat; v3: TALfloat); cdecl;
  alBufferfv: procedure(buffer: TALuint; param: TALenum; values: PALfloat); cdecl;

  //read parameter for an buffer object
  alGetBufferi: procedure(buffer: TALuint; param: TALenum; value: PALint); cdecl;
  alGetBuffer3i: procedure(buffer: TALuint; param: TALenum; v1: PALint; v2: PALint; v3: PALint); cdecl;
  alGetBufferiv: procedure(buffer: TALuint; param: TALenum; values: PALint); cdecl;

  alGetBufferf: procedure(buffer: TALuint; param: TALenum; value: PALfloat); cdecl;
  alGetBuffer3f: procedure(buffer: TALuint; param: TALenum; v1: PALfloat; v2: PALfloat; v3: PALfloat); cdecl;
  alGetBufferfv: procedure(buffer: TALuint; param: TALenum; values: PALfloat); cdecl;

  //Global Parameters
  alDistanceModel: procedure(value: TALenum); cdecl;
  alDopplerFactor: procedure(value: TALfloat); cdecl;
  alDopplerVelocity: procedure(value: TALfloat); cdecl;
  alSpeedOfSound: procedure( value: TALfloat ); cdecl;

  //alc functions

  //Context Support
  alcCreateContext: function(device: TALCdevice; attrlist: PALCint): TALCcontext; cdecl;
  alcMakeContextCurrent: function(context: TALCcontext): TALCenum; cdecl;
  alcProcessContext: procedure(context: TALCcontext); cdecl;
  alcSuspendContext: procedure(context: TALCcontext); cdecl;
  alcDestroyContext: procedure(context: TALCcontext); cdecl;
  alcGetCurrentContext: function: TALCcontext; cdecl;
  alcGetContextsDevice: function(context: TALCcontext): TALCdevice; cdecl;

  //Device Management
  alcOpenDevice: function(deviceName: PALCuByte): TALCdevice; cdecl;
  alcCloseDevice: procedure(device: TALCdevice); cdecl;

  //Error Support
  //Obtain the most recent Context error
  alcGetError: function(device: TALCdevice): TALCenum; cdecl;

  //Extension support.
  //Query for the presence of an extension, and obtain any appropriate
  //function pointers and enum values.
  alcIsExtensionPresent: function(device: TALCdevice; extName: PALuByte): TALCboolean; cdecl;
  alcGetProcAddress: function(device: TALCdevice; funcName: PALuByte): TALCvoid; cdecl;
  alcGetEnumValue: function(device: TALCdevice; enumName: PALuByte): TALCenum; cdecl;

  //Query functions
  alcGetString: function(device: TALCdevice; param: TALCenum): PALCubyte; cdecl;
  alcGetIntegerv: procedure(device: TALCdevice; param: TALCenum; size: TALCsizei; data: PALCint); cdecl;

  //Capture functions
  alcCaptureOpenDevice: function( const devicename: PALCchar; frequency: TALCuint; format: TALCenum; buffersize: TALCsizei): PALCdevice; cdecl;
  alcCaptureCloseDevice: function( device: PALCdevice ): TALCboolean; cdecl;
  alcCaptureStart: procedure( device: PALCdevice ); cdecl;
  alcCaptureStop: procedure( device: PALCdevice ); cdecl;
  alcCaptureSamples: procedure( device: PALCdevice; buffer: TALCvoid; samples: TALCsizei ); cdecl;

  //EAX functions
  EAXSet: Function(const Guid: TGUID; ALuint1: TALuint; ALuint2: TALuint; point: Pointer; ALuint3: TALuint): TALenum; cdecl;
  EAXGet: Function(const Guid: TGUID; ALuint1: TALuint; ALuint2: TALuint; point: Pointer; ALuint3: TALuint): TALenum; cdecl;

  //X-RAM enums
  AL_EAX_RAM_SIZE: TALEnum;
  AL_EAX_RAM_FREE: TALEnum;
  AL_STORAGE_AUTOMATIC: TALEnum;
  AL_STORAGE_HARDWARE: TALEnum;
  AL_STORAGE_ACCESSIBLE: TALEnum;
  //X-RAM functions
  EAXSetBufferMode: function(n: TALsizei;buffers: PALuint; value: TALint ): TALboolean; cdecl;
  EAXGetBufferMode: function(buffer: TALuint; value: PALint ): TALenum; cdecl;

  //EFX functions

  //Effect object functions.

  //Create Effect objects.
  ALGENEFFECTS: procedure( n: TALsizei; effects: PALuint ); cdecl;

  //Delete Effect objects.
  ALDELETEEFFECTS: procedure( n: TALsizei; effects: PALuint ); cdecl;

  //Verify a handle is a valid Effect.
  ALISEFFECT: function( eid: TALuint ): TALboolean; cdecl;

  // Set an integer parameter for an Effect object.
  ALEFFECTI: procedure( eid: TALuint; param: TALCenum; value: TALint); cdecl;
  ALEFFECTIV: procedure( eid: TALuint; param: TALenum; values: PALint ); cdecl;

  // Set a floating point parameter for an Effect object.
  ALEFFECTF: procedure( eid: TALuint; param: TALenum; value: TALfloat); cdecl;
  ALEFFECTFV: procedure( eid: TALuint; param: TALenum; values: PALfloat ); cdecl;

  // Get an integer parameter for an Effect object.
  ALGETEFFECTI: procedure( eid: TALuint; param: TALenum; value: PALint ); cdecl;
  ALGETEFFECTIV: procedure( eid: TALuint; param: TALenum; values: PALint ); cdecl;

  // Get a floating point parameter for an Effect object.
  ALGETEFFECTF: procedure( eid: TALuint; param: TALenum; value: PALfloat ); cdecl;
  ALGETEFFECTFV: procedure( eid: TALuint; param: TALenum; values: PALfloat ); cdecl;

  //Filter object functions

  // Create Filter objects.
  ALGENFILTERS: procedure( n: TALsizei; filters: PALuint ); cdecl;

  // Delete Filter objects.
  ALDELETEFILTERS: procedure( n: TALsizei; filters: PALuint ); cdecl;

  // Verify a handle is a valid Filter.
  ALISFILTER: function( fid: TALuint ): TALboolean; cdecl;

  // Set an integer parameter for a Filter object.
  ALFILTERI: procedure( fid: TALuint; param: TALenum; value: TALint ); cdecl;
  ALFILTERIV: procedure( fid: TALuint; param: TALenum; values: PALint ); cdecl;

  // Set a floating point parameter for an Filter object.
  ALFILTERF: procedure( fid: TALuint; param: TALenum; value: TALfloat); cdecl;
  ALFILTERFV: procedure( fid: TALuint; param: TALenum; values: PALfloat ); cdecl;

  // Get an integer parameter for a Filter object.
  ALGETFILTERI: procedure( fid: TALuint; param: TALenum; value: PALint ); cdecl;
  ALGETFILTERIV: procedure( fid: TALuint; param: TALenum; values: PALint ); cdecl;

  // Get a floating point parameter for a Filter object.
  ALGETFILTERF: procedure( fid: TALuint; param: TALenum; value: PALfloat ); cdecl;
  ALGETFILTERFV: procedure( fid: TALuint; param: TALenum; values: PALfloat ); cdecl;

  // Auxiliary Slot object functions

  // Create Auxiliary Slot objects.
  ALGENAUXILIARYEFFECTSLOTS: procedure( n: TALsizei; slots: PALuint ); cdecl;

  // Delete Auxiliary Slot objects.
  ALDELETEAUXILIARYEFFECTSLOTS: procedure( n: TALsizei; slots: PALuint ); cdecl;

  // Verify a handle is a valid Auxiliary Slot.
  ALISAUXILIARYEFFECTSLOT: function( slot: TALuint ): TALboolean; cdecl;

  // Set an integer parameter for a Auxiliary Slot object.
  ALAUXILIARYEFFECTSLOTI: procedure( asid: TALuint; param: TALenum; value: TALint ); cdecl;
  ALAUXILIARYEFFECTSLOTIV: procedure( asid: TALuint; param: TALenum; values: PALint ); cdecl;

  // Set a floating point parameter for an Auxiliary Slot object.
  ALAUXILIARYEFFECTSLOTF: procedure( asid: TALuint; param: TALenum; value: TALfloat ); cdecl;
  ALAUXILIARYEFFECTSLOTFV: procedure( asid: TALuint; param: TALenum; values: PALfloat ); cdecl;

  // Get an integer parameter for a Auxiliary Slot object.
  ALGETAUXILIARYEFFECTSLOTI: procedure( asid: TALuint; param: TALenum; value: PALint ); cdecl;
  ALGETAUXILIARYEFFECTSLOTIV: procedure( asid: TALuint; param: TALenum; values: PALint ); cdecl;

  // Get a floating point parameter for a Auxiliary Slot object.
  ALGETAUXILIARYEFFECTSLOTF: procedure( asid: TALuint; param: TALenum; value: PALfloat ); cdecl;
  ALGETAUXILIARYEFFECTSLOTFV: procedure( asid: TALuint; param: TALenum; values: PALfloat ); cdecl;


{$IFDEF ALUT}
  //External Alut functions (from dll or so)
  alutInit: procedure(argc: PALint; argv: array of PALbyte); cdecl;
  alutExit: procedure; cdecl;

  alutLoadWAVFile: procedure(fname: string; var format: TALenum; var data: TALvoid; var size: TALsizei; var freq: TALsizei; var loop: TALint); cdecl;
  alutLoadWAVMemory: procedure(memory: PALbyte; var format: TALenum; var data: TALvoid; var size: TALsizei; var freq: TALsizei; var loop: TALint); cdecl;
  alutUnloadWAV: procedure(format: TALenum; data: TALvoid; size: TALsizei; freq: TALsizei); cdecl;
{$ELSE}
  //Internal Alut functions
  procedure alutInit(argc: PALint; argv: array of PALbyte);
  procedure alutExit;

  procedure alutLoadWAVFile(fname: string; var format: TALenum; var data: TALvoid; var size: TALsizei; var freq: TALsizei; var loop: TALint);
  procedure alutLoadWAVMemory(memory: PALbyte; var format: TALenum; var data: TALvoid; var size: TALsizei; var freq: TALsizei; var loop: TALint);
  procedure alutUnloadWAV(format: TALenum; data: TALvoid; size: TALsizei; freq: TALsizei);
{$ENDIF}

var
  LibHandle          : THandle = 0;
{$IFDEF ALUT}
  AlutLibHandle      : THandle = 0;
{$ENDIF}
  EFXUtilLibHandle       : THandle = 0;
type
  HMODULE = THandle;

{$IFDEF ALUT}
function InitOpenAL(LibName: String = callibname;AlutLibName: String = calutlibname): Boolean;
{$ELSE}
function InitOpenAL(LibName: String = callibname): Boolean;
{$ENDIF}
procedure ReadOpenALExtensions;


implementation

uses classes;

type
  //WAV file header
  TWAVHeader = record
    RIFFHeader: array [1..4] of Char;
    FileSize: Integer;
    WAVEHeader: array [1..4] of Char;
    FormatHeader: array [1..4] of Char;
    FormatHeaderSize: Integer;
    FormatCode: Word;
    ChannelNumber: Word;
    SampleRate: Integer;
    BytesPerSecond: Integer;
    BytesPerSample: Word;
    BitsPerSample: Word;
  end;

const
  WAV_STANDARD  = $0001;
  WAV_IMA_ADPCM = $0011;
  WAV_MP3       = $0055;

{$IFDEF FPC}
{$IFNDEF Win32}
// Added by bero
const
  RTLD_LAZY         = $001;
  RTLD_NOW          = $002;
  RTLD_BINDING_MASK = $003;
  LibraryLib        = {$IFDEF Linux}'dl'{$ELSE}'c'{$ENDIF};

function LoadLibraryEx(Name : PChar; Flags : LongInt) : Pointer; cdecl; external LibraryLib name 'dlopen';
function GetProcAddressEx(Lib : Pointer; Name : PChar) : Pointer; cdecl; external LibraryLib name 'dlsym';
function FreeLibraryEx(Lib : Pointer) : LongInt; cdecl; external LibraryLib name 'dlclose';

function LoadLibrary(Name : PChar) : THandle;
begin
 Result := THandle(LoadLibraryEx(Name, RTLD_LAZY));
end;

function GetProcAddress(LibHandle : THandle; ProcName : PChar) : Pointer;
begin
 Result := GetProcAddressEx(Pointer(LibHandle), ProcName);
end;

function FreeLibrary(LibHandle : THandle) : Boolean;
begin
 if LibHandle = 0 then
   Result := False
  else
   Result := FreeLibraryEx(Pointer(LibHandle)) = 0;
end;
{$ENDIF}
{$ENDIF}

//ProcName can be case sensitive !!!
function alProcedure(ProcName : PChar) : Pointer;
begin
Result := NIL;
if Addr(alGetProcAddress) <> NIL then
 Result := alGetProcAddress(ProcName);
if result <> NIL then
 exit;
Result := GetProcAddress(LibHandle, ProcName);
end;

{$IFDEF ALUT}
function InitOpenAL(LibName, AlutLibName: String): Boolean;
{$ELSE}
function InitOpenAL(LibName: String): Boolean;
{$ENDIF}
begin
  Result       := False;
  if LibHandle<>0 then FreeLibrary(LibHandle);
  LibHandle    := LoadLibrary(PChar(LibName));
{$IFDEF ALUT}
  if AlutLibHandle<>0 then FreeLibrary(AlutLibHandle);
  AlutLibHandle    := LoadLibrary(PChar(AlutLibName));

  if (AlutLibHandle <> 0) then
  begin
    alutInit:= GetProcAddress(AlutLibHandle, 'alutInit');
    alutExit:= GetProcAddress(AlutLibHandle, 'alutExit');
    alutLoadWAVFile:= GetProcAddress(AlutLibHandle, 'alutLoadWAVFile');
    alutLoadWAVMemory:= GetProcAddress(AlutLibHandle, 'alutLoadWAVMemory');
    alutUnloadWAV:= GetProcAddress(AlutLibHandle, 'alutUnloadWAV');
  end;
{$ENDIF}

  alGetProcAddress := GetProcAddress(LibHandle, 'alGetProcAddress');

  if (LibHandle <> 0) then
  begin

    alEnable:= alProcedure('alEnable');
    alDisable:= alProcedure('alDisable');
    alIsEnabled:= alProcedure('alIsEnabled');
    alHint:= alProcedure('alHint');
    alGetBooleanv:= alProcedure('alGetBooleanv');
    alGetIntegerv:= alProcedure('alGetIntegerv');
    alGetFloatv:= alProcedure('alGetFloatv');
    alGetDoublev:= alProcedure('alGetDoublev');
    alGetString:= alProcedure('alGetString');
    alGetBoolean:= alProcedure('alGetBoolean');
    alGetInteger:= alProcedure('alGetInteger');
    alGetFloat:= alProcedure('alGetFloat');
    alGetDouble:= alProcedure('alGetDouble');
    alGetError:= alProcedure('alGetError');
    alIsExtensionPresent:= alProcedure('alIsExtensionPresent');
    alGetEnumValue:= alProcedure('alGetEnumValue');

    alListenerf:= alProcedure('alListenerf');
    alListener3f:= alProcedure('alListener3f');
    alListenerfv:= alProcedure('alListenerfv');

    alListeneri:= alProcedure('alListeneri');
    alListener3i:= alProcedure('alListener3i');
    alListeneriv:= alProcedure('alListeneriv');

    alGetListeneriv:= alProcedure('alGetListeneriv');
    alGetListenerfv:= alProcedure('alGetListenerfv');

    alGenSources:= alProcedure('alGenSources');
    alDeleteSources:= alProcedure('alDeleteSources');
    alIsSource:= alProcedure('alIsSource');

    alSourcei:= alProcedure('alSourcei');
    alSource3i:= alProcedure('alSource3i');
    alSourceiv:= alProcedure('alSourceiv');

    alSourcef:= alProcedure('alSourcef');
    alSource3f:= alProcedure('alSource3f');
    alSourcefv:= alProcedure('alSourcefv');

    alGetSourcei:= alProcedure('alGetSourcei');
    alGetSource3i:= alProcedure('alGetSource3i');
    alGetSourceiv:= alProcedure('alGetSourceiv');

    alGetSourcef:= alProcedure('alGetSourcef');
    alGetSource3f:= alProcedure('alGetSource3f');
    alGetSourcefv:= alProcedure('alGetSourcefv');

    alSourcePlay:= alProcedure('alSourcePlay');
    alSourcePause:=alProcedure('alSourcePause');
    alSourceStop:= alProcedure('alSourceStop');
    alSourceRewind:= alProcedure('alSourceRewind');
    alSourcePlayv:= alProcedure('alSourcePlayv');
    alSourceStopv:= alProcedure('alSourceStopv');
    alSourceRewindv:= alProcedure('alSourceRewindv');
    alSourcePausev:= alProcedure('alSourcePausev');
    alGenBuffers:= alProcedure('alGenBuffers');
    alDeleteBuffers:= alProcedure('alDeleteBuffers');
    alIsBuffer:= alProcedure('alIsBuffer');
    alBufferData:= alProcedure('alBufferData');

    alBufferi:= alProcedure('alBufferi');
    alBuffer3i:= alProcedure('alBuffer3i');
    alBufferiv:= alProcedure('alBufferiv');

    alBufferf:= alProcedure('alBufferf');
    alBuffer3f:= alProcedure('alBuffer3f');
    alBufferfv:= alProcedure('alBufferfv');

    alGetBufferi:= alProcedure('alGetBufferi');
    alGetBuffer3i:= alProcedure('alGetBuffer3i');
    alGetBufferiv:= alProcedure('alGetBufferiv');

    alGetBufferf:= alProcedure('alGetBufferf');
    alGetBuffer3f:= alProcedure('alGetBuffer3f');
    alGetBufferfv:= alProcedure('alGetBufferfv');

    alSourceQueueBuffers:= alProcedure('alSourceQueueBuffers');
    alSourceUnqueueBuffers:= alProcedure('alSourceUnqueueBuffers');
    alDistanceModel:= alProcedure('alDistanceModel');
    alDopplerFactor:= alProcedure('alDopplerFactor');
    alDopplerVelocity:= alProcedure('alDopplerVelocity');
    alSpeedOfSound := alProcedure('alSpeedOfSound');

    alcCreateContext:= alProcedure('alcCreateContext');
    alcMakeContextCurrent:= alProcedure('alcMakeContextCurrent');
    alcProcessContext:= alProcedure('alcProcessContext');
    alcSuspendContext:= alProcedure('alcSuspendContext');
    alcDestroyContext:= alProcedure('alcDestroyContext');
    alcGetError:= alProcedure('alcGetError');
    alcGetCurrentContext:= alProcedure('alcGetCurrentContext');
    alcOpenDevice:= alProcedure('alcOpenDevice');
    alcCloseDevice:= alProcedure('alcCloseDevice');
    alcIsExtensionPresent:= alProcedure('alcIsExtensionPresent');
    alcGetProcAddress:= alProcedure('alcGetProcAddress');
    alcGetEnumValue:= alProcedure('alcGetEnumValue');
    alcGetContextsDevice:= alProcedure('alcGetContextsDevice');
    alcGetString:= alProcedure('alcGetString');
    alcGetIntegerv:= alProcedure('alcGetIntegerv');

    alcCaptureOpenDevice:= alProcedure('alcCaptureOpenDevice');
    alcCaptureCloseDevice:= alProcedure('alcCaptureCloseDevice');
    alcCaptureStart:= alProcedure('alcCaptureStart');
    alcCaptureStop:= alProcedure('alcCaptureStop');
    alcCaptureSamples:= alProcedure('alcCaptureSamples');

    Result:=True;
  end;
end;

procedure ReadOpenALExtensions;
begin
  if (LibHandle <> 0) then
    begin
      //EAX Extensions
      if alIsExtensionPresent('EAX2.0') then
      begin
        EAXSet := alProcedure('EAXSet');
        EAXGet := alProcedure('EAXGet');
      end;
      //EAX-RAM Extension
      if alIsExtensionPresent('EAX-RAM') then
      begin
        EAXSetBufferMode := alGetProcAddress('EAXSetBufferMode');
    		EAXGetBufferMode := alGetProcAddress('EAXGetBufferMode');
        AL_EAX_RAM_SIZE := alGetEnumValue('AL_EAX_RAM_SIZE');
        AL_EAX_RAM_FREE := alGetEnumValue('AL_EAX_RAM_FREE');
        AL_STORAGE_AUTOMATIC := alGetEnumValue('AL_STORAGE_AUTOMATIC');
        AL_STORAGE_HARDWARE := alGetEnumValue('AL_STORAGE_HARDWARE');
        AL_STORAGE_ACCESSIBLE := alGetEnumValue('AL_STORAGE_ACCESSIBLE');
      end;
      if alcIsExtensionPresent(alcGetContextsDevice(alcGetCurrentContext()), ALC_EXT_EFX_NAME) then
      begin
        alGenEffects := alGetProcAddress('alGenEffects');
		    alDeleteEffects := alGetProcAddress('alDeleteEffects');
		    alIsEffect := alGetProcAddress('alIsEffect');
		    alEffecti := alGetProcAddress('alEffecti');
		    alEffectiv := alGetProcAddress('alEffectiv');
		    alEffectf := alGetProcAddress('alEffectf');
		    alEffectfv := alGetProcAddress('alEffectfv');
		    alGetEffecti := alGetProcAddress('alGetEffecti');
		    alGetEffectiv := alGetProcAddress('alGetEffectiv');
		    alGetEffectf := alGetProcAddress('alGetEffectf');
        alGetEffectfv := alGetProcAddress('alGetEffectfv');
        alGenFilters := alGetProcAddress('alGenFilters');
		    alDeleteFilters := alGetProcAddress('alDeleteFilters');
		    alIsFilter := alGetProcAddress('alIsFilter');
		    alFilteri := alGetProcAddress('alFilteri');
		    alFilteriv := alGetProcAddress('alFilteriv');
		    alFilterf := alGetProcAddress('alFilterf');
		    alFilterfv := alGetProcAddress('alFilterfv');
		    alGetFilteri := alGetProcAddress('alGetFilteri');
		    alGetFilteriv := alGetProcAddress('alGetFilteriv');
		    alGetFilterf := alGetProcAddress('alGetFilterf');
		    alGetFilterfv := alGetProcAddress('alGetFilterfv');
		    alGenAuxiliaryEffectSlots := alGetProcAddress('alGenAuxiliaryEffectSlots');
		    alDeleteAuxiliaryEffectSlots := alGetProcAddress('alDeleteAuxiliaryEffectSlots');
		    alIsAuxiliaryEffectSlot := alGetProcAddress('alIsAuxiliaryEffectSlot');
		    alAuxiliaryEffectSloti := alGetProcAddress('alAuxiliaryEffectSloti');
		    alAuxiliaryEffectSlotiv := alGetProcAddress('alAuxiliaryEffectSlotiv');
        alAuxiliaryEffectSlotf := alGetProcAddress('alAuxiliaryEffectSlotf');
		    alAuxiliaryEffectSlotfv := alGetProcAddress('alAuxiliaryEffectSlotfv');
		    alGetAuxiliaryEffectSloti := alGetProcAddress('alGetAuxiliaryEffectSloti');
		    alGetAuxiliaryEffectSlotiv := alGetProcAddress('alGetAuxiliaryEffectSlotiv');
		    alGetAuxiliaryEffectSlotf := alGetProcAddress('alGetAuxiliaryEffectSlotf');
		    alGetAuxiliaryEffectSlotfv := alGetProcAddress('alGetAuxiliaryEffectSlotfv');
      end;
    end;
end;

//Internal Alut replacement procedures

procedure alutInit(argc: PALint; argv: array of PALbyte);
var
  Context: PALCcontext;
  Device: PALCdevice;
begin
  //Open device
  Device := alcOpenDevice(nil); // this is supposed to select the "preferred device"
  //Create context(s)
  Context := alcCreateContext(Device, nil);
  //Set active context
  alcMakeContextCurrent(Context);
end;

procedure alutExit;
var
  Context: PALCcontext;
  Device: PALCdevice;
begin
  //Get active context
  Context := alcGetCurrentContext;
  //Get device for active context
  Device := alcGetContextsDevice(Context);
  //Release context(s)
  alcDestroyContext(Context);
  //Close device
  alcCloseDevice(Device);
end;

function LoadWavStream(Stream: Tstream; var format: TALenum; var data: TALvoid; var size: TALsizei; var freq: TALsizei; var loop: TALint): Boolean;
var
  WavHeader: TWavHeader;
  readname: pchar;
  name: string;
  readint: integer;
begin
    Result:=False;

    //Read wav header
    stream.Read(WavHeader, sizeof(TWavHeader));

    //Determine SampleRate
    freq:=WavHeader.SampleRate;

    //Detemine waveformat
    if WavHeader.ChannelNumber = 1 then
    case WavHeader.BitsPerSample of
    8: format := AL_FORMAT_MONO8;
    16: format := AL_FORMAT_MONO16;
    end;

    if WavHeader.ChannelNumber = 2 then
    case WavHeader.BitsPerSample of
    8: format := AL_FORMAT_STEREO8;
    16: format := AL_FORMAT_STEREO16;
    end;

    //go to end of wavheader
    stream.seek((8-44)+12+4+WavHeader.FormatHeaderSize+4,soFromCurrent); //hmm crappy...
    //loop to rest of wave file data chunks
    repeat
      //read chunk name
      getmem(readname,4);
      stream.Read(readname^, 4);
      name:=readname[0]+readname[1]+readname[2]+readname[3];
      if name='data' then
      begin
        //Get the size of the wave data
        stream.Read(readint,4);
        size:=readint;
        if WavHeader.BitsPerSample = 8 then size:=size+1; //fix for 8bit???
        //Read the actual wave data
        getmem(data,size);
        stream.Read(Data^, size);

        //Decode wave data if needed
        if WavHeader.FormatCode=WAV_IMA_ADPCM then
        begin
          //TODO: add code to decompress IMA ADPCM data
        end;
        if WavHeader.FormatCode=WAV_MP3 then
        begin
          //TODO: add code to decompress MP3 data
        end;
        Result:=True;
      end
      else
      begin
        //Skip unknown chunk(s)
        stream.Read(readint,4);
        stream.Position:=stream.Position+readint;
      end;
    until stream.Position>=stream.size;

end;

procedure alutLoadWAVFile(fname: string; var format: TALenum; var data: TALvoid; var size: TALsizei; var freq: TALsizei; var loop: TALint);
var
  Stream : TFileStream;
begin
  Stream:=TFileStream.Create(fname,$0000);
  LoadWavStream(Stream, format, data, size, freq, loop);
  Stream.Free;
end;

procedure alutLoadWAVMemory(memory: PALbyte; var format: TALenum; var data: TALvoid; var size: TALsizei; var freq: TALsizei; var loop: TALint);
var Stream: TMemoryStream;
begin
  Stream:=TMemoryStream.Create;
  Stream.Write(memory,sizeof(memory^));
  LoadWavStream(Stream, format, data, size, freq, loop);
  Stream.Free;
end;

procedure alutUnloadWAV(format: TALenum; data: TALvoid; size: TALsizei; freq: TALsizei);
begin
  //Clean up
  if data<>nil then freemem(data);
end;

end.
