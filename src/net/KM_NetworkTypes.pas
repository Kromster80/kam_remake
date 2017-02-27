unit KM_NetworkTypes;
{$I KaM_Remake.inc}
interface


const
  NET_ADDRESS_EMPTY = 0;    //Yet undefined
  NET_ADDRESS_OTHERS = -1;  //Recipient
  NET_ADDRESS_ALL = -2;     //Recipient
  NET_ADDRESS_HOST = -3;    //Sender/Recipient
  NET_ADDRESS_SERVER = -4;  //Sender/Recipient

  //Size of chunks that a file is sent in (must be smaller than MAX_PACKET_SIZE)
  //Making it less than Ethernet MTU (~1500) helps to avoids inefficient IP fragmentation
  FILE_CHUNK_SIZE = 1024; //1kb
  MAX_PACKET_SIZE = 20480; //20kb. Maximum length of a KM packet
  MAX_CHUNKS_BEFORE_ACK = 80; //Number of chunks of a file that can be in flight

  //Client-Server-Client exchange packets. Each packet is a certain type
type
  TKMessageKind = (
    mk_AskToJoin,       //Client asks Host if he can join
    mk_AllowToJoin,     //Host allows Client to join
    mk_RefuseToJoin,    //Host can refuse when e.g. Nikname is already taken

    mk_AskForAuth,      //Joiner sends challenge to host, and askes host to send its challenge
    mk_IndexOnServer,   //Server tells Client his index
    mk_ClientLost,      //Server tells clients that someone has disconnected
    mk_ReassignHost,    //Server tells clients who is the new host after the host disconnects

    mk_GameVersion,     //Server tells a new client which game version we are using
    mk_WelcomeMessage,  //Server sends a welcome message to the client
    mk_ServerName,      //Server sends the server name to the client
    mk_JoinRoom,        //Client requests to be placed in a room
    mk_ConnectedToRoom, //Server tells a client they have been successfully added to a room
    mk_SetGameInfo,     //Host tells the server the player list, map, etc to be reported to queries
    mk_KickPlayer,      //Host askes the server to kick someone
    mk_BanPlayer,       //Host askes the server to ban someone from this room
    mk_GiveHost,        //Host askes the server to reassign host
    mk_ResetBans,       //Host askes the server to reset the ban list for this room
    mk_Kicked,          //Server tells a client they were kicked just before disconnecting then
    mk_LangCode,        //Client tells host his language code
    mk_AuthChallenge,   //Host sends solution and own challenge back to joiner

    mk_GetServerInfo,   //Client askes for server for the server details (for querying)
    mk_ServerInfo,      //Server sends client the server info on request

    mk_Disconnect,      //Joiner tells Host that he is leaving the lobby/game deliberately
                        //Host tells Joiners that he is quitting
                        //A. Server runs on the same machine and stops right after
                        //B. Server runs on different machine and assigns Host role to some Client

    mk_Ping,            //Server pings Clients
    mk_Pong,            //Clients reply to Server with pong
    mk_PingInfo,        //Server sends list of ping times to Clients
    mk_FPS,             //Client tells other clients his FPS

    mk_PlayersList,     //Host keeps the players list and sends it to everyone on change
    mk_GameOptions,     //Host keeps the game options and sends it to everyone on change

    mk_StartingLocQuery,//Joiner asks Host if he can take that starting location
    mk_SetTeam,         //Joiner tells Host which team he is on
    mk_FlagColorQuery,  //Joiner asks Host if he can take specific color

    mk_ResetMap,        //Reset the map selection to blank
    mk_MapSelect,       //Host selects the map to play
    mk_SaveSelect,      //Host selects the save to play
    mk_ReadyToStart,    //Joiner tells he's ready to play the game
    mk_HasMapOrSave,    //Joiner tells host he has the map/save file
    mk_Start,           //Host says to start the game
    mk_ReadyToReturnToLobby, //Joiner/host tells others they are ready to return to lobby

    mk_ReadyToPlay,     //Joiner tells Host he has loaded the map and clock can start
    mk_Play,            //Host tells everyone that the game may begin
    mk_AskToReconnect,  //Dropped player askes permission from the host to reconnect
    mk_RefuseReconnect, //Host tells the dropped player he is not allowed to reconnect
    mk_ResyncFromTick,  //Dropped player requests other players to send missed commands from specified tick
    mk_ReconnectionAccepted, //Host tells dropped player they are accepted back into the game
    mk_ClientReconnected, //Host tells other players the index of a reconnected client

    mk_Commands,        //Clients exchange commands for next ticks
    mk_TextTranslated,  //Clients exchange translated text (system messages)
    mk_TextChat,        //Clients exchange chat messages

    mk_ReqPassword,     //Server requests joiner to send password
    mk_Password,        //Joiner sends password to server
    mk_SetPassword,     //Host sets password on server

    mk_FileRequest,     //Joiner requests host to send file
    mk_FileChunk,       //Host sends chunk of file to joiner
    mk_FileAck,         //Joiner tells host he received a chunk
    mk_FileEnd,         //Host informs joiner that the whole file has been sent

    mk_Vote             //Joiner tells host his vote
  );


  TKMPacketFormat = (
    pfNoData,   // Packet contains no data
    pfBinary,   // Packet contains binary data (Stream)
    pfNumber,   // Packet contains an integer
    pfStringA,  // Packet contains ANSI string
    pfStringW   // Packet contains Unicode string
  );

const
  NetPacketType: array [TKMessageKind] of TKMPacketFormat = (
    pfBinary,   //mk_AskToJoin
    pfNoData,   //mk_AllowToJoin
    pfNumber,   //mk_RefuseToJoin
    pfBinary,   //mk_AskForAuth
    pfNumber,   //mk_IndexOnServer
    pfNumber,   //mk_ClientLost
    pfBinary,   //mk_ReassignHost
    pfStringA,  //mk_GameVersion
    pfStringW,  //mk_WelcomeMessage
    pfStringA,  //mk_ServerName
    pfNumber,   //mk_JoinRoom
    pfNumber,   //mk_ConnectedToRoom
    pfBinary,   //mk_SetGameInfo
    pfNumber,   //mk_KickPlayer
    pfNumber,   //mk_BanPlayer
    pfNumber,   //mk_GiveHost
    pfNoData,   //mk_ResetBans
    pfNumber,   //mk_Kicked
    pfStringA,  //mk_LangCode
    pfBinary,   //mk_AuthChallenge
    pfNoData,   //mk_GetServerInfo
    pfBinary,   //mk_ServerInfo
    pfNoData,   //mk_Disconnect
    pfNoData,   //mk_Ping
    pfNoData,   //mk_Pong
    pfBinary,   //mk_PingInfo
    pfNumber,   //mk_FPS
    pfBinary,   //mk_PlayersList
    pfBinary,   //mk_GameOptions
    pfNumber,   //mk_StartingLocQuery
    pfNumber,   //mk_SetTeam
    pfNumber,   //mk_FlagColorQuery
    pfNoData,   //mk_ResetMap
    pfBinary,   //mk_MapSelect
    pfBinary,   //mk_SaveSelect
    pfNoData,   //mk_ReadyToStart
    pfNoData,   //mk_HasMapOrSave
    pfBinary,   //mk_Start
    pfNoData,   //mk_ReadyToReturnToLobby
    pfNoData,   //mk_ReadyToPlay
    pfNoData,   //mk_Play
    pfStringA,  //mk_AskToReconnect
    pfNumber,   //mk_RefuseReconnect
    pfNumber,   //mk_ResyncFromTick
    pfNoData,   //mk_ReconnectionAccepted
    pfNumber,   //mk_ClientReconnected
    pfBinary,   //mk_Commands
    pfBinary,   //mk_TextTranslated
    pfBinary,   //mk_TextChat
    pfNoData,   //mk_ReqPassword
    pfBinary,   //mk_Password
    pfStringA,  //mk_SetPassword
    pfStringW,  //mk_FileRequest
    pfBinary,   //mk_FileChunk
    pfNoData,   //mk_FileAck
    pfNoData,   //mk_FileEnd
    pfNoData    //mk_Vote
  );


type
  TMPGameState = (mgsNone, mgsLobby, mgsLoading, mgsGame);
  TKMServerType = (mstClient, mstDedicated, mstLocal);
  TNetPlayerType = (nptHuman, nptComputer, nptClosed);

const
  //Used in the dedicated server display as it does not care about translations (translated ones are in KM_TextLibrary)
  GameStateText: array [TMPGameState] of UnicodeString = ('None', 'Lobby', 'Loading', 'Game');
  NetPlayerTypeName: array [TNetPlayerType] of UnicodeString = ('Human', 'AI Player', 'Closed');
  ServerTypePic: array [TKMServerType] of Word = (74, 75, 79);


implementation


end.
