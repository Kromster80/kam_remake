const
  winsocket = 'wsock32.dll';

[DllImport(winsocket, CharSet = CharSet.Ansi, SetLastError = False, EntryPoint = 'WSAStartup')]
function WSAStartup; external;
[DllImport(winsocket, SetLastError = False, EntryPoint = 'WSACleanup')]
function WSACleanup; external;
[DllImport(winsocket, SetLastError = False, EntryPoint = 'WSAGetLastError')]
function WSAGetLastError; external;
[DllImport(winsocket, SetLastError = False, EntryPoint = 'WSASetLastError')]
procedure WSASetLastError; external;
[DllImport(winsocket, SetLastError = False, EntryPoint = 'WSAAsyncSelect')]
function WSAAsyncSelect; external;
[DllImport(winsocket, SetLastError = False, EntryPoint = 'WSAAsyncGetHostByName')]
function WSAAsyncGetHostByName; external;
[DllImport(winsocket, SetLastError = False, EntryPoint = 'WSAAsyncGetHostByAddr')]
function WSAAsyncGetHostByAddr; external;
[DllImport(winsocket, CharSet = CharSet.Ansi, SetLastError = False, EntryPoint = 'WSACancelAsyncRequest')]
function WSACancelAsyncRequest; external;
[DllImport(winsocket, CharSet = CharSet.Ansi, SetLastError = False, EntryPoint = 'gethostname')]
function gethostname; external;
[DllImport(winsocket, CharSet = CharSet.Ansi, SetLastError = False, EntryPoint = 'getprotobyname')]
function getprotobyname; external;
[DllImport(winsocket, CharSet = CharSet.Ansi, SetLastError = False, EntryPoint = 'getservbyname')]
function getservbyname; external;
[DllImport(winsocket, CharSet = CharSet.Ansi, SetLastError = False, EntryPoint = 'gethostbyname')]
function gethostbyname; external;
[DllImport(winsocket, CharSet = CharSet.Ansi, SetLastError = False, EntryPoint = 'gethostbyaddr')]
function gethostbyaddr; external;
[DllImport(winsocket, SetLastError = False, EntryPoint = 'getpeername')]
function getpeername; external;
[DllImport(winsocket, SetLastError = False, EntryPoint = 'ntohl')]
function ntohl; external;
[DllImport(winsocket, SetLastError = False, EntryPoint = 'htonl')]
function htonl; external;
[DllImport(winsocket, SetLastError = False, EntryPoint = 'htons')]
function htons; external;
[DllImport(winsocket, SetLastError = False, EntryPoint = 'ntohs')]
function ntohs; external;
[DllImport(winsocket, CharSet = CharSet.Ansi, SetLastError = False, EntryPoint = 'inet_addr')]
function inet_addr; external;
[DllImport(winsocket, CharSet = CharSet.Ansi, SetLastError = False, EntryPoint = 'inet_ntoa')]
function inet_ntoa; external;
[DllImport(winsocket, SetLastError = False, EntryPoint = 'socket')]
function socket; external;
[DllImport(winsocket, SetLastError = False, EntryPoint = 'shutdown')]
function shutdown; external;
[DllImport(winsocket, SetLastError = False, EntryPoint = 'closesocket')]
function closesocket; external;
[DllImport(winsocket, SetLastError = False, EntryPoint = 'bind')]
function bind; external;
[DllImport(winsocket, SetLastError = False, EntryPoint = 'accept')]
function accept; external;
[DllImport(winsocket, SetLastError = False, EntryPoint = 'connect')]
function connect; external;
[DllImport(winsocket, SetLastError = False, EntryPoint = 'listen')]
function listen; external;
[DllImport(winsocket, SetLastError = False, EntryPoint = 'send')]
function send; external;
[DllImport(winsocket, SetLastError = False, EntryPoint = 'sendto')]
function sendto; external;
[DllImport(winsocket, SetLastError = False, EntryPoint = 'recv')]
function recv; external;
[DllImport(winsocket, SetLastError = False, EntryPoint = 'recvfrom')]
function recvfrom; external;
[DllImport(winsocket, SetLastError = False, EntryPoint = 'ioctlsocket')]
function ioctlsocket; external;
[DllImport(winsocket, SetLastError = False, EntryPoint = 'getsockname')]
function getsockname; external;
[DllImport(winsocket, SetLastError = False, EntryPoint = 'setsockopt')]
function setsockopt_integer; external;
[DllImport(winsocket, SetLastError = False, EntryPoint = 'setsockopt')]
function setsockopt_tinaddr; external;
[DllImport(winsocket, SetLastError = False, EntryPoint = 'setsockopt')]
function setsockopt_tlinger; external;
[DllImport(winsocket, SetLastError = False, EntryPoint = 'setsockopt')]
function setsockopt_ip_mreq; external;
[DllImport(winsocket, SetLastError = False, EntryPoint = 'getsockopt')]
function getsockopt_integer; external;
[DllImport(winsocket, SetLastError = False, EntryPoint = 'getsockopt')]
function getsockopt_ip_mreq; external;
[DllImport(winsocket, SetLastError = False, EntryPoint = 'getsockopt')]
function getsockopt_tinaddr; external;
[DllImport(winsocket, SetLastError = False, EntryPoint = 'getsockopt')]
function getsockopt_tlinger; external;





