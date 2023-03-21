{ WinSock2 patch for larger TFDSet (1024 instead of 64).

  These types and functions are identical with the ones from WinSock2, the
  only difference is the value of FD_SETSIZE and therefore a larger TFDSet
  array. Putting this unit *after* the winsock2 unit into your uses clause
  will make these new definitions take precedence.
}
unit lws2override;

{$mode objfpc}{$H+}

interface
uses
  windows,
  WinSock2;

const
  // "64 sockets ought to be enough for anybody"
  FD_SETSIZE     =   1024; //      ...except me!

type
  PFDSet = ^TFDSet;
  TFDSet = record
    fd_count: u_int;
    fd_array: array[0..FD_SETSIZE-1] of TSocket;
  end;
  fdset = TFDSet;

  function select(nfds: Longint; readfds, writefds, exceptfds: PFDSet; timeout: PTimeVal): Longint; stdcall;external WINSOCK2_DLL name 'select';
  function __WSAFDIsSet(s: TSOcket; var FDSet: TFDSet): Bool; stdcall; external WINSOCK2_DLL name '__WSAFDIsSet';

procedure FD_CLR(Socket: TSocket; var FDSet: TFDSet);
function FD_ISSET(Socket: TSocket; var FDSet: TFDSet): Boolean;
procedure FD_SET(Socket: TSocket; var FDSet: TFDSet);
procedure FD_ZERO(var FDSet: TFDSet);

implementation

procedure FD_CLR(Socket: TSocket; var FDSet: TFDSet);
var
  I: cardinal;
begin
  I := 0;
  while I < FDSet.fd_count do
  begin
    if FDSet.fd_array[I] = Socket then
    begin
      while I < FDSet.fd_count - 1 do
      begin
        FDSet.fd_array[I] := FDSet.fd_array[I + 1];
        Inc(I);
      end;
      Dec(FDSet.fd_count);
     Break;
    end;
    Inc(I);
  end;
end;

function FD_ISSET(Socket: TSocket; var FDSet: TFDSet): Boolean;
begin
  FD_ISSET := __WSAFDIsSet(Socket, FDSet);
end;

procedure FD_SET(Socket: TSocket; var FDSet: TFDSet);
begin
  if FDSet.fd_count < FD_SETSIZE then
  begin
    FDSet.fd_array[FDSet.fd_count] := Socket;
    Inc(FDSet.fd_count);
  end;
end;

procedure FD_ZERO(var FDSet: TFDSet);
begin
  FDSet.fd_count := 0;
end;

end.

