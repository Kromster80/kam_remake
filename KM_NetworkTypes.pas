unit KM_NetworkTypes;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils;


const
  NET_ADDRESS_EMPTY = 0;    //Yet undefined
  NET_ADDRESS_OTHERS = -1;  //Recipient
  NET_ADDRESS_ALL = -2;     //Recipient
  NET_ADDRESS_HOST = -3;    //Sender/Recipient
  NET_ADDRESS_SERVER = -4;  //Sender/Recipient

  MAX_PACKET_SIZE = 10240; //10kb. Maximum length of a KM packet

  //Client-Server-Client exchange packets. Each packet is a certain type
type
  TKMessageKind = (
    mk_AskToJoin,       //Client asks Host if he can join
    mk_AllowToJoin,     //Host allows Client to join
    mk_RefuseToJoin,    //Host can refuse when e.g. Nikname is already taken

    mk_HostingRights,   //Server tells client they have hosting rights (during inital connection only)
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
    mk_Kicked,          //Server tells a client they were kicked just before disconnecting then
    mk_LangID,          //Client tells host his language ID

    mk_GetServerInfo,   //Client askes for server for the server details (for querying)
    mk_ServerInfo,      //Server sends client the server info on request

    mk_Disconnect,      //Joiner tells Host that he is leaving the lobby/game deliberately
                        //Host tells Joiners that he is quitting
                        //A. Server runs on the same machine and stops right after
                        //B. Server runs on different machine and assigns Host role to some Client

    mk_Ping,            //Server pings Clients
    mk_Pong,            //Clients reply to Server with pong
    mk_PingInfo,        //Server sends list of ping times to Clients

    mk_PlayersList,     //Host keeps the players list and sends it to everyone on change
    mk_GameOptions,     //Host keeps the game options and sends it to everyone on change

    mk_StartingLocQuery,//Joiner asks Host if he can take that starting location
    mk_SetTeam,         //Joiner tells Host which team he is on
    mk_FlagColorQuery,  //Joiner asks Host if he can take specific color

    mk_ResetMap,        //Reset the map selection to blank
    mk_MapSelect,       //Host selects the map to play
    mk_MapCRC,          //Host tells clients what the CRC of the map should be
    mk_SaveSelect,      //Host selects the save to play
    mk_SaveCRC,         //Host tells clients what the CRC of the save should be
    mk_ReadyToStart,    //Joiner tells he's ready to play the game
    mk_Start,           //Host says to start the game

    mk_ReadyToPlay,     //Joiner tells Host he has loaded the map and clock can start
    mk_Play,            //Host tells everyone that the game may begin
    mk_AskToReconnect,  //Dropped player askes permission from the host to reconnect
    mk_RefuseReconnect, //Host tells the dropped player he is not allowed to reconnect
    mk_ResyncFromTick,  //Dropped player requests other players to send missed commands from specified tick
    mk_ReconnectionAccepted, //Host tells dropped player they are accepted back into the game
    mk_ClientReconnected, //Host tells other players the index of a reconnected client

    mk_Commands,        //Clients exchange commands for next ticks
    mk_Text             //Clients exchange text messages
    );


  TKMPacketFormat = (pfNoData, pfNumber, pfText);

const
  NetPacketType:array[TKMessageKind] of TKMPacketFormat =
  ( pfText,     //mk_AskToJoin
    pfNoData,   //mk_AllowToJoin
    pfText,     //mk_RefuseToJoin
    pfNoData,   //mk_HostingRights
    pfNumber,   //mk_IndexOnServer
    pfNumber,   //mk_ClientLost
    pfNumber,   //mk_ReassignHost
    pfText,     //mk_GameVersion
    pfText,     //mk_WelcomeMessage
    pfText,     //mk_ServerName
    pfNumber,   //mk_JoinRoom
    pfNumber,   //mk_ConnectedToRoom
    pfText,     //mk_SetGameInfo
    pfNumber,   //mk_KickPlayer
    pfText,     //mk_Kicked
    pfNumber,   //mk_LangID
    pfNoData,   //mk_GetServerInfo
    pfText,     //mk_ServerInfo
    pfNoData,   //mk_Disconnect
    pfNoData,   //mk_Ping
    pfNoData,   //mk_Pong
    pfText,     //mk_PingInfo
    pfText,     //mk_PlayersList
    pfText,     //mk_GameOptions
    pfNumber,   //mk_StartingLocQuery
    pfNumber,   //mk_SetTeam
    pfNumber,   //mk_FlagColorQuery
    pfNoData,   //mk_ResetMap
    pfText,     //mk_MapSelect
    pfNumber,   //mk_MapCRC
    pfText,     //mk_SaveSelect
    pfNumber,   //mk_SaveCRC
    pfNoData,   //mk_ReadyToStart
    pfText,     //mk_Start
    pfNoData,   //mk_ReadyToPlay
    pfNoData,   //mk_Play
    pfText,     //mk_AskToReconnect
    pfText,     //mk_RefuseReconnect
    pfNumber,   //mk_ResyncFromTick
    pfNoData,   //mk_ReconnectionAccepted
    pfNumber,   //mk_ClientReconnected
    pfText,     //mk_Commands
    pfText      //mk_Text
  );


type
  TMPGameState = (mgs_None, mgs_Lobby, mgs_Loading, mgs_Game);

const
  //Used in the dedicated server display as it does not care about translations (translated ones are in KM_TextLibrary)
  GameStateText:array[TMPGameState] of string = ('None','Lobby','Loading','Game');


implementation


end.
