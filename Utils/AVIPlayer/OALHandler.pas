unit OALHandler;
{
  Copyright (c) <2018> <Stuart "Stucuk" Carey>

  This software is provided 'as-is', without any express or implied
  warranty. In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
  claim that you wrote the original software. If you use this software
  in a product, an acknowledgment in the product documentation would be
  appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
  misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

  // Altered by Krom for KaM Remake
}

interface

type
  TOALSound = class
  private
    FBuffer, FSource: Cardinal;
  public
    constructor Create(aFormat: Integer; aFrequency, aSize: Cardinal; aData: Pointer; aLoop: Boolean);
    destructor Destroy; override;

    procedure Stop;
    procedure Play;
    procedure Pause;
  end;


implementation
uses
  openal;


{ TOALSound }
constructor TOALSound.Create(aFormat: Integer; aFrequency, aSize: Cardinal; aData: Pointer; aLoop: Boolean);
begin
  inherited Create;

  AlGenBuffers(1, @FBuffer);
  AlBufferData(FBuffer, aFormat, aData, aSize, aFrequency);

  AlGenSources(1, @FSource);
  AlSourcei(FSource, AL_BUFFER, FBuffer);

  AlSourcef(FSource, AL_PITCH, 1);
  AlSourcef(FSource, AL_GAIN, 10);
  if aLoop then
    AlSourcei(FSource, AL_LOOPING, 1)
  else
    AlSourcei(FSource, AL_LOOPING, 0);
end;

destructor TOALSound.Destroy;
begin
  AlDeleteSources(1, @FSource);
  AlDeleteBuffers(1, @FBuffer);

  inherited;
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
  Pos: Array [0..2] of Single = (0, 0, 0);

var
  Device, Context: Pointer;

initialization
  InitOpenAL;
  Device := alcOpenDevice(nil);
  Context := alcCreateContext(Device, nil);
  alcMakeContextCurrent(Context);
  alListenerfv(AL_POSITION, @Pos[0]);
  alListenerfv(AL_VELOCITY, @Pos[0]);
  alListenerfv(AL_ORIENTATION, @Pos[0]);

finalization
  alcGetCurrentContext;
  alcMakeContextCurrent(nil);
  alcDestroyContext(Context);
  alcCloseDevice(Device);

end.
