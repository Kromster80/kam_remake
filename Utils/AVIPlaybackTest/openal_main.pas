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

unit openal_main;

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
  SysUtils{$IFDEF Win32},Windows{$ENDIF},openal_types;

{$DEFINE ALUT} //define ALUT to use alut.dll

const
{$IFDEF Win32}
  callibname='OpenAL32.dll';
  calutlibname='Alut.dll';
{$ENDIF}
{$IFDEF Linux}
  callibname='libopenal.so';
  calutlibname='libalut.so';
{$ENDIF}

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

  //Listener Environment:  default 0.
  alListeneri: procedure(param: TAlenum; value: TALint); cdecl;

  //Listener Gain:  default 1.0f.
  alListenerf: procedure(param: TALenum; value: TALfloat); cdecl;

  //Listener Position.
  //Listener Velocity.
  alListener3f: procedure(param: TALenum; f1: TALfloat; f2: TALfloat; f3: TALfloat); cdecl;

  //Listener Position:        array [0..2] of TALfloat
  //Listener Velocity:        array [0..2] of TALfloat
  //Listener Orientation:     array [0..5] of TALfloat  forward and up vector.
  alListenerfv: procedure(param: TALenum; values: PALfloat); cdecl;

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
  //Set a float parameter for a Source object.
  alSourcef: procedure(source: TALuint; param: TALenum; value: TALfloat); cdecl;
  //Set a 3 float parameter for a Source object.
  alSource3f: procedure(source: TALuint; param: TALenum; v1: TALfloat; v2: TALfloat; v3: TALfloat); cdecl;
  //Set a float vector parameter for a Source object.
  alSourcefv: procedure(source: TALuint; param: TALenum; values: PALfloat); cdecl;

  //Get an integer scalar parameter for a Source object.
  alGetSourcei: procedure(source: TALuint; param: TALenum; value: PALint); cdecl;
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
  //read parameter for an buffer object
  alGetBufferi: procedure(buffer: TALuint; param: TALenum; value: PALint); cdecl;
  alGetBufferf: procedure(buffer: TALuint; param: TALenum; value: PALfloat); cdecl;

  //Queue stuff
  alSourceQueueBuffers: procedure(source: TALuint; n: TALsizei; buffers: PALuint); cdecl;
  alSourceUnqueueBuffers: procedure(source: TALuint; n: TALsizei; buffers: PALuint); cdecl;

  //Knobs and dials
  alDistanceModel: procedure(value: TALenum); cdecl;
  alDopplerFactor: procedure(value: TALfloat); cdecl;
  alDopplerVelocity: procedure(value: TALfloat); cdecl;

  //alc
  alcCreateContext: function(device: TALCdevice; attrlist: PALCint): TALCcontext; cdecl;

  //There is no current context, as we can mix
  //several active contexts. But al* calls
  //only affect the current context.
  alcMakeContextCurrent: function(context: TALCcontext): TALCenum; cdecl;

  //Perform processing on a synced context, non-op on a asynchronous
  //context.
  alcProcessContext: procedure(context: TALCcontext); cdecl;

  //Suspend processing on an asynchronous context, non-op on a
  //synced context.
  alcSuspendContext: procedure(context: TALCcontext); cdecl;

  alcDestroyContext: procedure(context: TALCcontext); cdecl;

  alcGetError: function(device: TALCdevice): TALCenum; cdecl;

  alcGetCurrentContext: function: TALCcontext; cdecl;

  alcOpenDevice: function(deviceName: PALCuByte): TALCdevice; cdecl;
  alcCloseDevice: procedure(device: TALCdevice); cdecl;

  alcIsExtensionPresent: function(device: TALCdevice; extName: PALuByte): TALCboolean; cdecl;
  alcGetProcAddress: function(device: TALCdevice; funcName: PALuByte): TALCvoid; cdecl;
  alcGetEnumValue: function(device: TALCdevice; enumName: PALuByte): TALCenum; cdecl;

  alcGetContextsDevice: function(context: TALCcontext): TALCdevice; cdecl;

  //Query functions
  alcGetString: function(device: TALCdevice; param: TALCenum): PALCubyte; cdecl;
  alcGetIntegerv: procedure(device: TALCdevice; param: TALCenum; size: TALCsizei; data: PALCint); cdecl;

  //EAX functions
  EAXSet: Function(const Guid: TGUID; ALuint1: TALuint; ALuint2: TALuint; point: Pointer; ALuint3: TALuint): TALenum; cdecl;
  EAXGet: Function(const Guid: TGUID; ALuint1: TALuint; ALuint2: TALuint; point: Pointer; ALuint3: TALuint): TALenum; cdecl;

  //External Alut functions (from dll or so)
  alutInit: procedure(argc: PALint; argv: array of PALbyte); cdecl;
  alutExit: procedure; cdecl;

//  alutLoadWAVFile: procedure(fname: String; var format: TALenum; var data: TALvoid; var size: TALsizei; var freq: TALsizei; var loop: TALint); cdecl;
  alutLoadWAVMemory: procedure(memory: PALbyte; var format: TALenum; var data: TALvoid; var size: TALsizei; var freq: TALsizei; var loop: TALint); cdecl;
  alutUnloadWAV: procedure(format: TALenum; data: TALvoid; size: TALsizei; freq: TALsizei); cdecl;

  procedure alutLoadWAVFile(fname: string; var format: TALenum; var data: TALvoid; var size: TALsizei; var freq: TALsizei; var loop: TALint); cdecl;

var
  LibHandle          : THandle = 0;
{$IFDEF ALUT}
  AlutLibHandle      : THandle = 0;
{$ENDIF}
type
  HMODULE = THandle;

function InitOpenAL: Boolean; cdecl;
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

function InitOpenAL: Boolean; cdecl;
begin
  Result       := False;
  if LibHandle<>0 then FreeLibrary(LibHandle);
  LibHandle    := LoadLibrary(PChar(callibname));
{$IFDEF ALUT}
  if AlutLibHandle<>0 then FreeLibrary(AlutLibHandle);
  AlutLibHandle    := LoadLibrary(PChar(calutlibname));

  if (AlutLibHandle <> 0) then
  begin
    alutInit:= GetProcAddress(AlutLibHandle, 'alutInit');
    alutExit:= GetProcAddress(AlutLibHandle, 'alutExit');
   // alutLoadWAVFile:= GetProcAddress(AlutLibHandle, 'alutLoadWAVFile');
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
    alListeneri:= alProcedure('alListeneri');
    alListenerf:= alProcedure('alListenerf');
    alListener3f:= alProcedure('alListener3f');
    alListenerfv:= alProcedure('alListenerfv');
    alGetListeneriv:= alProcedure('alGetListeneriv');
    alGetListenerfv:= alProcedure('alGetListenerfv');
    alGenSources:= alProcedure('alGenSources');
    alDeleteSources:= alProcedure('alDeleteSources');
    alIsSource:= alProcedure('alIsSource');
    alSourcei:= alProcedure('alSourcei');
    alSourcef:= alProcedure('alSourcef');
    alSource3f:= alProcedure('alSource3f');
    alSourcefv:= alProcedure('alSourcefv');
    alGetSourcei:= alProcedure('alGetSourcei');
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
    alGetBufferi:= alProcedure('alGetBufferi');
    alGetBufferf:= alProcedure('alGetBufferf');
    alSourceQueueBuffers:= alProcedure('alSourceQueueBuffers');
    alSourceUnqueueBuffers:= alProcedure('alSourceUnQueueBuffers');
    alDistanceModel:= alProcedure('alDopplerModel');
    alDopplerFactor:= alProcedure('alDopplerFactor');
    alDopplerVelocity:= alProcedure('alDopplerVelocity');

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

    Result:=True;
  end;
end;

procedure ReadOpenALExtensions;
begin
  if (LibHandle <> 0) then
    begin
      EAXSet := alProcedure('EAXSet');
      EAXGet := alProcedure('EAXGet');
    end;
end;

//Internal Alut replacement procedures

{procedure alutInit(argc: PALint; argv: array of PALbyte);
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
end;        }

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
       {
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
end;   }

end.
