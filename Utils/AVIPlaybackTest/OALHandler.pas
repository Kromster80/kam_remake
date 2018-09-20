unit OALHandler;
{
 By Stuart "Stucuk" Carey
 Started 2018/09/18
 Copyright (C) Stuart Carey 2018

 License:
  Feel Free to use this in whatever you want!
}

interface

uses
  openal_types;

type
TOALSound = Class(TObject)
private
 FBuffer,
 FSource  : Cardinal;
public
 constructor Create(Format : Integer; Frequency : Integer; Size : Cardinal; Data : Pointer; Loop : Boolean);
 destructor Destroy; override;

 procedure Stop;
 procedure Play;
 procedure Pause;
end;

implementation

uses openal_main;

constructor TOALSound.Create(Format : Integer; Frequency : Integer; Size : Cardinal; Data : Pointer; Loop : Boolean);
begin
 Inherited Create();

 AlGenBuffers(1, @FBuffer);
 AlBufferData(FBuffer, Format, Data, Size, Frequency);

 AlGenSources(1, @FSource);
 AlSourcei (FSource, AL_BUFFER, FBuffer);

 AlSourcef ( FSource, AL_PITCH, 1 );
 AlSourcef ( FSource, AL_GAIN, 10 );
 if Loop then
  AlSourcei ( FSource, AL_LOOPING, 1)
 else
  AlSourcei ( FSource, AL_LOOPING, 0);
end;

destructor TOALSound.Destroy;
begin
 Inherited Destroy;

 AlDeleteSources(1, @FSource);
 AlDeleteBuffers(1, @FBuffer);
end;

procedure TOALSound.Stop;
begin
 alSourceStop(FSource);
end;

procedure TOALSound.Play;
begin
 alSourcePlay(FSource);
end;

procedure TOALSound.Pause;
begin
 alSourcePause(FSource);
end;

const
 Pos : Array [0..2] of Single = (0,0,0);
var
 Device,
 Context : Pointer;

initialization
 InitOpenAL;

 Device  := alcOpenDevice(Nil);
 Context := alcCreateContext(Device,Nil);
 alcMakeContextCurrent(Context);
 alListenerfv(AL_POSITION,@Pos[0]);
 alListenerfv(AL_VELOCITY,@Pos[0]);
 alListenerfv(AL_ORIENTATION,@Pos[0]);

finalization

 alcGetCurrentContext();
 alcMakeContextCurrent(Nil);
 alcDestroyContext(Context);
 alcCloseDevice(Device);
end.
