program KaM_Remake;
{$I KaM_Remake.inc}

uses
  //{$IFDEF WDC} FastMM4, {$ENDIF} //Can be used only in Delphi, not Lazarus
  {$IFDEF USE_MAD_EXCEPT}
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListModules,
  {$ENDIF}
  {$IFDEF UNIX} cthreads, {$ENDIF} //Required for thread support on Unix/Linux
  Forms,
  {$IFDEF FPC} Interfaces, {$ENDIF}
  KM_FormMain in 'src\KM_FormMain.pas' {FormMain},
  KM_FormLoading in 'src\KM_FormLoading.pas' {FormLoading},
  //KM_Achievements in 'KM_Achievements.pas',

  KM_AI in 'src\ai\KM_AI.pas',
  KM_AIArmyEvaluation in 'src\ai\KM_AIArmyEvaluation.pas',
  KM_AIAttacks in 'src\ai\KM_AIAttacks.pas',
  KM_AICityPlanner in 'src\ai\KM_AICityPlanner.pas',
  KM_AIDefensePos in 'src\ai\KM_AIDefensePos.pas',
  KM_AIInfluences in 'src\ai\KM_AIInfluences.pas',
  KM_AIFields in 'src\ai\KM_AIFields.pas',
  KM_AIGeneral in 'src\ai\KM_AIGeneral.pas',
  KM_AIGoals in 'src\ai\KM_AIGoals.pas',
  KM_AIMayor in 'src\ai\KM_AIMayor.pas',
  KM_AIMayorBalance in 'src\ai\KM_AIMayorBalance.pas',
  KM_AISetup in 'src\ai\KM_AISetup.pas',

  KM_Alerts in 'src\KM_Alerts.pas',
  KM_BinPacking in 'src\KM_BinPacking.pas',
  KM_BuildList in 'src\KM_BuildList.pas',
  KM_Campaigns in 'src\KM_Campaigns.pas',
  KM_CommonClasses in 'src\KM_CommonClasses.pas',
  KM_CommonTypes in 'src\KM_CommonTypes.pas',
  KM_Controls in 'src\KM_Controls.pas',
  KM_DedicatedServer in 'src\KM_DedicatedServer.pas',
  KM_Defaults in 'src\KM_Defaults.pas',
  KM_Deliveries in 'src\KM_Deliveries.pas',
  {$IFDEF USE_MAD_EXCEPT}KM_Exceptions in 'src\KM_Exceptions.pas',{$ENDIF}
  KM_FileIO in 'src\KM_FileIO.pas',
  KM_FogOfWar in 'src\KM_FogOfWar.pas',
  KM_Game in 'src\KM_Game.pas',
  KM_GameApp in 'src\KM_GameApp.pas',
  KM_GameCursor in 'src\KM_GameCursor.pas',
  KM_GameInfo in 'src\KM_GameInfo.pas',
  KM_GameOptions in 'src\KM_GameOptions.pas',
  KM_GameInputProcess in 'src\KM_GameInputProcess.pas',
  KM_GameInputProcess_Multi in 'src\KM_GameInputProcess_Multi.pas',
  KM_GameInputProcess_Single in 'src\KM_GameInputProcess_Single.pas',

  KM_GUIGameBuild in 'src\gui\KM_GUIGameBuild.pas',
  KM_GUIGameChat in 'src\gui\KM_GUIGameChat.pas',
  KM_GUIGameHouse in 'src\gui\KM_GUIGameHouse.pas',
  KM_GUIGameRatios in 'src\gui\KM_GUIGameRatios.pas',
  KM_GUIGameStats in 'src\gui\KM_GUIGameStats.pas',
  KM_GUIGameMenuSettings in 'src\gui\KM_GUIGameMenuSettings.pas',
  KM_GUIMapEdAttack in 'src\gui\KM_GUIMapEdAttack.pas',
  KM_GUIMapEdExtras in 'src\gui\KM_GUIMapEdExtras.pas',
  KM_GUIMapEdFormations in 'src\gui\KM_GUIMapEdFormations.pas',
  KM_GUIMapEdGoal in 'src\gui\KM_GUIMapEdGoal.pas',
  KM_GUIMapEdHouse in 'src\gui\KM_GUIMapEdHouse.pas',
  KM_GUIMapEdMarkerDefence in 'src\gui\KM_GUIMapEdMarkerDefence.pas',
  KM_GUIMapEdMarkerReveal in 'src\gui\KM_GUIMapEdMarkerReveal.pas',
  KM_GUIMapEdMenu in 'src\gui\KM_GUIMapEdMenu.pas',
  KM_GUIMapEdMenuLoad in 'src\gui\KM_GUIMapEdMenuLoad.pas',
  KM_GUIMapEdMenuSave in 'src\gui\KM_GUIMapEdMenuSave.pas',
  KM_GUIMapEdMenuQuit in 'src\gui\KM_GUIMapEdMenuQuit.pas',
  KM_GUIMapEdMenuSettings in 'src\gui\KM_GUIMapEdMenuSettings.pas',
  KM_GUIMapEdMessage in 'src\gui\KM_GUIMapEdMessage.pas',
  KM_GUIMapEdPlayer in 'src\gui\KM_GUIMapEdPlayer.pas',
  KM_GUIMapEdPlayerBlockHouse in 'src\gui\KM_GUIMapEdPlayerBlockHouse.pas',
  KM_GUIMapEdPlayerBlockTrade in 'src\gui\KM_GUIMapEdPlayerBlockTrade.pas',
  KM_GUIMapEdPlayerBlockUnit in 'src\gui\KM_GUIMapEdPlayerBlockUnit.pas',
  KM_GUIMapEdPlayerColors in 'src\gui\KM_GUIMapEdPlayerColors.pas',
  KM_GUIMapEdPlayerGoals in 'src\gui\KM_GUIMapEdPlayerGoals.pas',
  KM_GUIMapEdPlayerView in 'src\gui\KM_GUIMapEdPlayerView.pas',
  KM_GUIMapEdMission in 'src\gui\KM_GUIMapEdMission.pas',
  KM_GUIMapEdMissionAlliances in 'src\gui\KM_GUIMapEdMissionAlliances.pas',
  KM_GUIMapEdMissionMode in 'src\gui\KM_GUIMapEdMissionMode.pas',
  KM_GUIMapEdMissionPlayers in 'src\gui\KM_GUIMapEdMissionPlayers.pas',
  KM_GUIMapEdTerrain in 'src\gui\KM_GUIMapEdTerrain.pas',
  KM_GUIMapEdTerrainBrushes in 'src\gui\KM_GUIMapEdTerrainBrushes.pas',
  KM_GUIMapEdTerrainHeights in 'src\gui\KM_GUIMapEdTerrainHeights.pas',
  KM_GUIMapEdTerrainTiles in 'src\gui\KM_GUIMapEdTerrainTiles.pas',
  KM_GUIMapEdTerrainObjects in 'src\gui\KM_GUIMapEdTerrainObjects.pas',
  KM_GUIMapEdTerrainSelection in 'src\gui\KM_GUIMapEdTerrainSelection.pas',
  KM_GUIMapEdTown in 'src\gui\KM_GUIMapEdTown.pas',
  KM_GUIMapEdTownUnits in 'src\gui\KM_GUIMapEdTownUnits.pas',
  KM_GUIMapEdTownHouses in 'src\gui\KM_GUIMapEdTownHouses.pas',
  KM_GUIMapEdTownScript in 'src\gui\KM_GUIMapEdTownScript.pas',
  KM_GUIMapEdTownDefence in 'src\gui\KM_GUIMapEdTownDefence.pas',
  KM_GUIMapEdTownOffence in 'src\gui\KM_GUIMapEdTownOffence.pas',
  KM_GUIMapEdUnit in 'src\gui\KM_GUIMapEdUnit.pas',
  KM_GUIMenuCampaign in 'src\gui\KM_GUIMenuCampaign.pas',
  KM_GUIMenuCampaigns in 'src\gui\KM_GUIMenuCampaigns.pas',
  KM_GUIMenuCredits in 'src\gui\KM_GUIMenuCredits.pas',
  KM_GUIMenuError in 'src\gui\KM_GUIMenuError.pas',
  KM_GUIMenuLoad in 'src\gui\KM_GUIMenuLoad.pas',
  KM_GUIMenuLoading in 'src\gui\KM_GUIMenuLoading.pas',
  KM_GUIMenuLobby in 'src\gui\KM_GUIMenuLobby.pas',
  KM_GUIMenuMain in 'src\gui\KM_GUIMenuMain.pas',
  KM_GUIMenuMapEditor in 'src\gui\KM_GUIMenuMapEditor.pas',
  KM_GUIMenuMultiplayer in 'src\gui\KM_GUIMenuMultiplayer.pas',
  KM_GUIMenuOptions in 'src\gui\KM_GUIMenuOptions.pas',
  KM_GUIMenuReplays in 'src\gui\KM_GUIMenuReplays.pas',
  KM_GUIMenuResultsMP in 'src\gui\KM_GUIMenuResultsMP.pas',
  KM_GUIMenuResultsSP in 'src\gui\KM_GUIMenuResultsSP.pas',
  KM_GUIMenuSingleMap in 'src\gui\KM_GUIMenuSingleMap.pas',
  KM_GUIMenuSinglePlayer in 'src\gui\KM_GUIMenuSinglePlayer.pas',

  KM_Hand in 'src\KM_Hand.pas',
  KM_HandsCollection in 'src\KM_HandsCollection.pas',
  KM_HandSpectator in 'src\KM_HandSpectator.pas',
  KM_HandStats in 'src\KM_HandStats.pas',
  KM_HouseBarracks in 'src\houses\KM_HouseBarracks.pas',
  KM_HouseInn in 'src\houses\KM_HouseInn.pas',
  KM_HouseCollection in 'src\houses\KM_HouseCollection.pas',
  KM_HouseMarket in 'src\houses\KM_HouseMarket.pas',
  KM_Houses in 'src\houses\KM_Houses.pas',
  KM_HouseSchool in 'src\houses\KM_HouseSchool.pas',
  KM_HTTPClient in 'src\KM_HTTPClient.pas',
  {$IFDEF WDC} KM_HTTPClientOverbyte in 'src\KM_HTTPClientOverbyte.pas', {$ENDIF}
  {$IFDEF FPC} KM_HTTPClientLNet in 'src\KM_HTTPClientLNet.pas', {$ENDIF}
  KM_Hungarian in 'src\KM_Hungarian.pas',
  KM_InterfaceDefaults in 'src\gui\KM_InterfaceDefaults.pas',
  KM_InterfaceGame in 'src\gui\KM_InterfaceGame.pas',
  KM_InterfaceGamePlay in 'src\gui\KM_InterfaceGamePlay.pas',
  KM_InterfaceMainMenu in 'src\gui\KM_InterfaceMainMenu.pas',
  KM_InterfaceMapEditor in 'src\gui\KM_InterfaceMapEditor.pas',
  KM_Log in 'src\KM_Log.pas',
  KM_Main in 'src\KM_Main.pas',
  KM_Maps in 'src\KM_Maps.pas',
  KM_MapEditor in 'src\KM_MapEditor.pas',
  KM_MasterServer in 'src\KM_MasterServer.pas',
  KM_MessageLog in 'src\KM_MessageLog.pas',
  KM_MessageStack in 'src\KM_MessageStack.pas',
  KM_Minimap in 'src\KM_Minimap.pas',
  KM_MissionScript in 'src\KM_MissionScript.pas',
  KM_MissionScript_Info in 'src\KM_MissionScript_Info.pas',
  KM_MissionScript_Preview in 'src\KM_MissionScript_Preview.pas',
  KM_MissionScript_Standard in 'src\KM_MissionScript_Standard.pas',
  KM_Music in 'src\KM_Music.pas',
  KM_NavMesh in 'src\KM_NavMesh.pas',

  {$IFDEF USESECUREAUTH}
    KM_NetAuthSecure in 'src\net\KM_NetAuthSecure.pas',
  {$ELSE}
    KM_NetAuthUnsecure in 'src\net\KM_NetAuthUnsecure.pas',
  {$ENDIF}
  KM_NetClient in 'src\net\KM_NetClient.pas',
  {$IFDEF WDC} KM_NetClientOverbyte in 'src\net\KM_NetClientOverbyte.pas', {$ENDIF}
  {$IFDEF FPC} KM_NetClientLNet in 'src\net\KM_NetClientLNet.pas', {$ENDIF}
  KM_NetFileTransfer in 'src\net\KM_NetFileTransfer.pas',
  KM_NetPlayersList in 'src\net\KM_NetPlayersList.pas',
  KM_NetServer in 'src\net\KM_NetServer.pas',
  {$IFDEF WDC} KM_NetServerOverbyte in 'src\net\KM_NetServerOverbyte.pas', {$ENDIF}
  {$IFDEF FPC} KM_NetServerLNet in 'src\net\KM_NetServerLNet.pas', {$ENDIF}
  KM_NetUDP in 'src\net\KM_NetUDP.pas',
  {$IFDEF WDC} KM_NetUDPOverbyte in 'src\net\KM_NetUDPOverbyte.pas', {$ENDIF}
  {$IFDEF FPC} KM_NetUDPLNet in 'src\net\KM_NetUDPLNet.pas', {$ENDIF}
  KM_NetworkClasses in 'src\net\KM_NetworkClasses.pas',
  KM_Networking in 'src\net\KM_Networking.pas',
  KM_NetworkTypes in 'src\net\KM_NetworkTypes.pas',

  KM_Outline in 'src\KM_Outline.pas',
  KM_PathFinding in 'src\KM_PathFinding.pas',
  KM_PathFindingAStarOld in 'src\KM_PathFindingAStarOld.pas',
  KM_PathFindingAStarNew in 'src\KM_PathFindingAStarNew.pas',
  KM_PathFindingJPS in 'src\KM_PathFindingJPS.pas',
  KM_PathFindingRoad in 'src\KM_PathFindingRoad.pas',
  KM_PerfLog in 'src\KM_PerfLog.pas',
  KM_Pics in 'src\KM_Pics.pas',
  KM_PNG in 'src\KM_PNG.pas',
  KM_Points in 'src\KM_Points.pas',
  KM_PolySimplify in 'src\KM_PolySimplify.pas',
  KM_Projectiles in 'src\KM_Projectiles.pas',
  KM_Render in 'src\KM_Render.pas',
  KM_RenderAux in 'src\KM_RenderAux.pas',
  KM_RenderControl in 'src\KM_RenderControl.pas',
  KM_RenderPool in 'src\KM_RenderPool.pas',
  KM_RenderTerrain in 'src\KM_RenderTerrain.pas',
  KM_RenderUI in 'src\KM_RenderUI.pas',
  KM_Resolutions in 'src\KM_Resolutions.pas',

  KM_Resource in 'src\res\KM_Resource.pas',
  KM_ResCursors in 'src\res\KM_ResCursors.pas',
  KM_ResFonts in 'src\res\KM_ResFonts.pas',
  KM_ResHouses in 'src\res\KM_ResHouses.pas',
  KM_ResLocales in 'src\res\KM_ResLocales.pas',
  KM_ResMapElements in 'src\res\KM_ResMapElements.pas',
  KM_ResPalettes in 'src\res\KM_ResPalettes.pas',
  KM_ResSound in 'src\res\KM_ResSound.pas',
  KM_ResSprites in 'src\res\KM_ResSprites.pas',
  KM_ResTexts in 'src\res\KM_ResTexts.pas',
  KM_ResTileset in 'src\res\KM_ResTileset.pas',
  KM_ResUnits in 'src\res\KM_ResUnits.pas',
  KM_ResWares in 'src\res\KM_ResWares.pas',

  KM_Saves in 'src\KM_Saves.pas',
  KM_Scripting in 'src\KM_Scripting.pas',
  KM_ScriptingESA in 'src\KM_ScriptingESA.pas',
  KM_ScriptingIdCache in 'src\KM_ScriptingIdCache.pas',
  KM_ServerQuery in 'src\KM_ServerQuery.pas',
  KM_Settings in 'src\KM_Settings.pas',
  KM_Sound in 'src\KM_Sound.pas',
  KM_Terrain in 'src\KM_Terrain.pas',
  KM_TerrainDeposits in 'src\KM_TerrainDeposits.pas',
  KM_TerrainFinder in 'src\KM_TerrainFinder.pas',
  KM_TerrainPainter in 'src\KM_TerrainPainter.pas',
  KM_TerrainSelection in 'src\KM_TerrainSelection.pas',
  KM_TerrainWalkConnect in 'src\KM_TerrainWalkConnect.pas',

  KM_UnitActionAbandonWalk in 'src\units\KM_UnitActionAbandonWalk.pas',
  KM_UnitActionFight in 'src\units\KM_UnitActionFight.pas',
  KM_UnitActionGoInOut in 'src\units\KM_UnitActionGoInOut.pas',
  KM_UnitActionStay in 'src\units\KM_UnitActionStay.pas',
  KM_UnitActionSteer in 'src\units\KM_UnitActionSteer.pas',
  KM_UnitActionStormAttack in 'src\units\KM_UnitActionStormAttack.pas',
  KM_UnitActionWalkTo in 'src\units\KM_UnitActionWalkTo.pas',
  KM_UnitGroups in 'src\units\KM_UnitGroups.pas',
  KM_Units in 'src\units\KM_Units.pas',
  KM_UnitsCollection in 'src\units\KM_UnitsCollection.pas',
  KM_Units_Warrior in 'src\units\KM_Units_Warrior.pas',
  KM_Units_WorkPlan in 'src\units\KM_Units_WorkPlan.pas',
  KM_UnitTaskAttackHouse in 'src\units\KM_UnitTaskAttackHouse.pas',
  KM_UnitTaskBuild in 'src\units\KM_UnitTaskBuild.pas',
  KM_UnitTaskDelivery in 'src\units\KM_UnitTaskDelivery.pas',
  KM_UnitTaskDie in 'src\units\KM_UnitTaskDie.pas',
  KM_UnitTaskGoEat in 'src\units\KM_UnitTaskGoEat.pas',
  KM_UnitTaskGoHome in 'src\units\KM_UnitTaskGoHome.pas',
  KM_UnitTaskGoOutShowHungry in 'src\units\KM_UnitTaskGoOutShowHungry.pas',
  KM_UnitTaskMining in 'src\units\KM_UnitTaskMining.pas',
  KM_UnitTaskSelfTrain in 'src\units\KM_UnitTaskSelfTrain.pas',
  KM_UnitTaskThrowRock in 'src\units\KM_UnitTaskThrowRock.pas',
  KM_Utils in 'src\KM_Utils.pas',
  KM_Viewport in 'src\KM_Viewport.pas';

{$IFDEF WDC}
  {$R KaM_Remake_Icon.res KaM_Remake_Icon.rc} //Keeps the Icon
  {$R KaM_Manifest.res KaM_Manifest.rc}
{$ENDIF}
{$IFDEF MSWindows}
  {$IFDEF FPC}
    {$R KaM_Remake_Icon.res}
    {$R KaM_Manifest.res}
  {$ENDIF}
{$ENDIF}

begin
  Application.Initialize;
  Application.Title := 'KaM Remake';

  fMain := TKMMain.Create;
  fMain.Start;

  Application.Run;

  fMain.Free; //Prevents memory leak of TKMMain showing up in FastMM
end.
