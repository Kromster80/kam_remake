
    (*********************************************************************
     *  DSPack 2.3.3                                                     *
     *                                                                   *
     *  home page : http://www.progdigy.com                              *
     *  email     : hgourvest@progdigy.com                               *
     *   Thanks to Michael Andersen. (DSVideoWindowEx)                   *
     *                                                                   *
     *  date      : 2003-09-08                                           *
     *                                                                   *
     *  The contents of this file are used with permission, subject to   *
     *  the Mozilla Public License Version 1.1 (the "License"); you may  *
     *  not use this file except in compliance with the License. You may *
     *  obtain a copy of the License at                                  *
     *  http://www.mozilla.org/MPL/MPL-1.1.html                          *
     *                                                                   *
     *  Software distributed under the License is distributed on an      *
     *  "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   *
     *  implied. See the License for the specific language governing     *
     *  rights and limitations under the License.                        *
     *                                                                   *
     *  Contributor(s)                                                   *
     *    Peter J. Haas     <DSPack@pjh2.de>                             *
     *    Andriy Nevhasymyy <a.n@email.com>                              *
     *    Milenko Mitrovic  <dcoder@dsp-worx.de>                         *
     *    Michael Andersen  <michael@mechdata.dk>                        *
     *    Martin Offenwanger <coder@dsplayer.de>                         *
     *                                                                   *
     *********************************************************************)

{
  @abstract(DSPack Components.)
  @author(Henri Gourvest: hgourvest@progdigy.com)
  @created(Mar 14, 2002)
  @lastmod(Oct 24, 2003)
}
{$I jedi.inc}
{$IFDEF COMPILER6_UP}
  {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
{$IFDEF COMPILER7_UP}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}
{$ALIGN ON}
{$MINENUMSIZE 4}

unit DSPack;

interface
uses
  Windows, Classes, SysUtils, Messages, Graphics, Forms, Controls, ActiveX, DirectShow9,
  DirectDraw, DSUtil, ComCtrls, MMSystem, Math, Consts, ExtCtrls,
  MultiMon, Dialogs, Registry, SyncObjs, Direct3D9, WMF9;

const
  { Filter Graph message identifier. }
  WM_GRAPHNOTIFY = WM_APP + 1;
  { Sample Grabber message identifier. }
  WM_CAPTURE_BITMAP = WM_APP + 2;

type

  { Video mode to use with @link(TVideoWindow). }
  TVideoMode = (
    vmNormal,
    vmVMR
  );

  { Graph Mode to use with @link(TFilterGraph).}
  TGraphMode = (
    gmNormal,
    gmCapture,
    gmDVD
  );

  { Render device returned by then OnGraphVMRRenderDevice event. }
{$IFDEF VER140}
  TVMRRenderDevice = (
    rdOverlay = 1,
    rdVidMem  = 2,
    rdSysMem  = 4
  );
{$ELSE}
  TVMRRenderDevice = Integer;
  const
    rdOverlay = 1;
    rdVidMem  = 2;
    rdSysMem  = 4;
type
{$ENDIF}

  {@exclude}
  TGraphState = (
    gsUninitialized,
    gsStopped,
    gsPaused,
    gsPlaying
  );

  { Specifies the seeking capabilities of a media stream. }
  TSeekingCap = (
    CanSeekAbsolute,   // The stream can seek to an absolute position.
    CanSeekForwards,   // The stream can seek forward.
    CanSeekBackwards,  // The stream can seek backward.
    CanGetCurrentPos,  // The stream can report its current position.
    CanGetStopPos,     // The stream can report its stop position.
    CanGetDuration,    // The stream can report its duration.
    CanPlayBackwards,  // The stream can play backward.
    CanDoSegments,     // The stream can do seamless looping (see IMediaSeeking.SetPositions).
    Source             // Reserved.
  );
  { Specifies the seeking capabilities of a media stream. }
  TSeekingCaps = set of TSeekingCap;

  { Video Mixer Render Preferences: <br>
    <b>vpForceOffscreen:</b> Indicates that the VMR should use only offscreen surfaces for rendering.<br>
    <b>vpForceOverlays:</b> Indicates that the VMR should fail if no overlay surfaces are available.<br>
    <b>vpForceMixer:</b> Indicates that the VMR must use Mixer when the number of streams is 1.<br>
    <b>vpDoNotRenderColorKeyAndBorder:</b> Indicates that the application is responsible for painting the color keys.<br>
    <b>vpRestrictToInitialMonitor:</b> Indicates that the VMR should output only to the initial monitor.<br>
    <b>vpPreferAGPMemWhenMixing:</b> Indicates that the VMR should attempt to use AGP memory when allocating texture surfaces.}
  TVMRPreference = (
    vpForceOffscreen,
    vpForceOverlays,
    vpForceMixer,
    vpDoNotRenderColorKeyAndBorder,
    vpRestrictToInitialMonitor,
    vpPreferAGPMemWhenMixing
  );

  { Pointer to @link(TVMRPreferences).}
  PVMRPreferences = ^TVMRPreferences;
  { Set of @link(TVMRPreference).}
  TVMRPreferences = set of TVMRPreference;

  TOnDSEvent                   = procedure(sender: TComponent; Event, Param1, Param2: Integer) of object;
                                                                                                                                   {@exclude}
  TOnGraphBufferingData        = procedure(sender: TObject; Buffering: boolean) of object ;                                        {@exclude}
  TOnGraphComplete             = procedure(sender: TObject; Result: HRESULT; Renderer: IBaseFilter) of object ;                    {@exclude}
  TOnGraphDeviceLost           = procedure(sender: TObject; Device: IUnknown; Removed: Boolean) of object ;                        {@exclude}
  TOnGraphEndOfSegment         = procedure(sender: TObject; StreamTime: TReferenceTime; NumSegment: Cardinal) of object ;         {@exclude}
  TOnDSResult                  = procedure(sender: TObject; Result: HRESULT) of object ;                                           {@exclude}
  TOnGraphFullscreenLost       = procedure(sender: TObject; Renderer: IBaseFilter) of object ;                                     {@exclude}
  TOnGraphOleEvent             = procedure(sender: TObject; String1, String2: WideString) of object ;                              {@exclude}
  TOnGraphOpeningFile          = procedure(sender: TObject; opening: boolean) of object ;                                          {@exclude}
  TOnGraphSNDDevError          = procedure(sender: TObject; OccurWhen: TSndDevErr; ErrorCode: LongWord) of object ;               {@exclude}
  TOnGraphStreamControl        = procedure(sender: TObject; PinSender: IPin; Cookie: LongWord) of object ;                         {@exclude}
  TOnGraphStreamError          = procedure(sender: TObject; Operation: HRESULT; Value: LongWord) of object ;                       {@exclude}
  TOnGraphVideoSizeChanged     = procedure(sender: TObject; Width, height: word) of object ;                                       {@exclude}
  TOnGraphTimeCodeAvailable    = procedure(sender: TObject; From: IBaseFilter; DeviceID: LongWord) of object ;                     {@exclude}
  TOnGraphEXTDeviceModeChange  = procedure(sender: TObject; NewMode, DeviceID: LongWord) of object ;                               {@exclude}
  TOnGraphVMRRenderDevice      = procedure(sender: TObject; RenderDevice: TVMRRenderDevice) of object;
                                                                                                                                   {@exclude}
  TOnDVDAudioStreamChange      = procedure(sender: TObject; stream, lcid: Integer; Lang: string) of object;                        {@exclude}
  TOnDVDCurrentTime            = procedure(sender: TObject; Hours, minutes,seconds,frames,frate : Integer) of object;              {@exclude}
  TOnDVDTitleChange            = procedure(sender: TObject; title: Integer) of object;                                             {@exclude}
  TOnDVDChapterStart           = procedure(sender: TObject; chapter: Integer) of object;                                           {@exclude}
  TOnDVDValidUOPSChange        = procedure(sender: TObject; UOPS: Integer) of object;                                              {@exclude}
  TOnDVDChange                 = procedure(sender: TObject; total,current: Integer) of object;                                     {@exclude}
  TOnDVDStillOn                = procedure(sender: TObject; NoButtonAvailable: boolean; seconds: Integer) of object;               {@exclude}
  TOnDVDSubpictureStreamChange = procedure(sender: TObject; SubNum, lcid: Integer; Lang: string) of object;                        {@exclude}
  TOnDVDPlaybackRateChange     = procedure(sender: TObject; rate:  single) of object;                                              {@exclude}
  TOnDVDParentalLevelChange    = procedure(sender: TObject; level: Integer) of object;                                             {@exclude}
  TOnDVDAnglesAvailable        = procedure(sender: TObject; available: boolean) of object;                                         {@exclude}
  TOnDVDButtonAutoActivated    = procedure(sender: TObject; Button: Cardinal) of object;                                           {@exclude}
  TOnDVDCMD                    = procedure(sender: TObject; CmdID: Cardinal) of object;                                            {@exclude}
  TOnDVDCurrentHMSFTime        = procedure(sender: TObject; HMSFTimeCode: TDVDHMSFTimeCode; TimeCode: TDVDTimeCode) of object;  {@exclude}
  TOnDVDKaraokeMode            = procedure(sender: TObject; Played: boolean) of object;
  {@exclude}
  TOnBuffer = procedure(sender: TObject; SampleTime: Double; pBuffer: Pointer; BufferLen: longint) of object ;

  TOnSelectedFilter = function (Moniker: IMoniker; FilterName: WideString; ClassID: TGuid): Boolean of Object;
  TOnCreatedFilter  = function (Filter: IBaseFilter; ClassID: TGuid): Boolean of Object;
  TOnUnableToRender = function (Pin: IPin): Boolean of Object;
// *****************************************************************************
//  IFilter
// *****************************************************************************

  {@exclude}
  TFilterOperation = (
    foAdding,    // Before the filter is added to graph.
    foAdded,     // After the filter is added to graph.
    foRemoving,  // Before the filter is removed from graph.
    foRemoved,   // After the filter is removed from graph.
    foRefresh    // Designer notification to Refresh the filter .
  );


  {@exclude}
  IFilter = interface
  ['{887F94DA-29E9-44C6-B48E-1FBF0FB59878}']
    { Return the IBaseFilter Interface (All DirectShow filters expose this interface). }
    function GetFilter: IBaseFilter;
    { Return the filter name (generally the component name). }
    function GetName: string;
    { Called by the @link(TFilterGraph) component, this method receive notifications
      on what the TFilterGraph is doing. if Operation = foGraphEvent then Param is the
      event code received by the FilterGraph.}
    procedure NotifyFilter(operation: TFilterOperation; Param: integer = 0);
  end;

  {@exclude}
  TControlEvent = (
    cePlay,
    cePause,
    ceStop,
    ceFileRendering,
    ceFileRendered,
    ceDVDRendering,
    ceDVDRendered,
    ceActive
  );

  {@exclude}
  IEvent = interface
  ['{6C0DCD7B-1A98-44EF-A6D5-E23CBC24E620}']
    { FilterGraph events. }
    procedure GraphEvent(Event, Param1, Param2: integer);
    { Control Events. }
    procedure ControlEvent(Event: TControlEvent; Param: integer = 0);
  end;



// *****************************************************************************
//  TFilterGraph
// *****************************************************************************

  { This component is the central component in DirectShow, the Filter Graph
    handle synchronization, event notification, and other aspects of the
    controlling the filter graph. }
  TFilterGraph = class(TComponent, IAMGraphBuilderCallback, IAMFilterGraphCallback,
                       IServiceProvider)
  private
    FActive     : boolean;
    FAutoCreate : boolean;
    FHandle     : THandle; // to capture events
    FMode       : TGraphMode;

    FVolume     : integer;
    FBalance    : integer;
    FRate       : Double;
    FLinearVolume : Boolean;

    FFilters: TInterfaceList;
    FGraphEvents: TInterfaceList;

    // builders
    FFilterGraph  : IGraphBuilder;
    FCaptureGraph : ICaptureGraphBuilder2;
    FDVDGraph     : IDvdGraphBuilder;

    // events interface
    FMediaEventEx : IMediaEventEx;

    // Graphedit
    FGraphEdit    : boolean;
    FGraphEditID  : Integer;

    // Log File
    FLogFileName: String;
    FLogFile: TFileStream;

    FOnActivate: TNotifyEvent;

    // All Events Code
    FOnDSEvent : TOnDSEvent;
    // Generic Graph Events
    FOnGraphBufferingData           : TOnGraphBufferingData;
    FOnGraphClockChanged            : TNotifyEvent;
    FOnGraphComplete                : TOnGraphComplete;
    FOnGraphDeviceLost              : TOnGraphDeviceLost;
    FOnGraphEndOfSegment            : TOnGraphEndOfSegment;
    FOnGraphErrorStillPlaying       : TOnDSResult;
    FOnGraphErrorAbort              : TOnDSResult;
    FOnGraphFullscreenLost          : TOnGraphFullscreenLost;
    FOnGraphChanged                 : TNotifyEvent;
    FOnGraphOleEvent                : TOnGraphOleEvent;
    FOnGraphOpeningFile             : TOnGraphOpeningFile;
    FOnGraphPaletteChanged          : TNotifyEvent;
    FOnGraphPaused                  : TOnDSResult;
    FOnGraphQualityChange           : TNotifyEvent;
    FOnGraphSNDDevInError           : TOnGraphSNDDevError;
    FOnGraphSNDDevOutError          : TOnGraphSNDDevError;
    FOnGraphStepComplete            : TNotifyEvent;
    FOnGraphStreamControlStarted    : TOnGraphStreamControl;
    FOnGraphStreamControlStopped    : TOnGraphStreamControl;
    FOnGraphStreamErrorStillPlaying : TOnGraphStreamError;
    FOnGraphStreamErrorStopped      : TOnGraphStreamError;
    FOnGraphUserAbort               : TNotifyEvent;
    FOnGraphVideoSizeChanged        : TOnGraphVideoSizeChanged;
    FOnGraphTimeCodeAvailable       : TOnGraphTimeCodeAvailable;
    FOnGraphEXTDeviceModeChange     : TOnGraphEXTDeviceModeChange;
    FOnGraphClockUnset              : TNotifyEvent;
    FOnGraphVMRRenderDevice         : TOnGraphVMRRenderDevice;

    FOnDVDAudioStreamChange       : TOnDVDAudioStreamChange;
    FOnDVDCurrentTime             : TOnDVDCurrentTime;
    FOnDVDTitleChange             : TOnDVDTitleChange;
    FOnDVDChapterStart            : TOnDVDChapterStart;
    FOnDVDAngleChange             : TOnDVDChange;
    FOnDVDValidUOPSChange         : TOnDVDValidUOPSChange;
    FOnDVDButtonChange            : TOnDVDChange;
    FOnDVDChapterAutoStop         : TNotifyEvent;
    FOnDVDStillOn                 : TOnDVDStillOn;
    FOnDVDStillOff                : TNotifyEvent;
    FOnDVDSubpictureStreamChange  : TOnDVDSubpictureStreamChange;
    FOnDVDNoFP_PGC                : TNotifyEvent;
    FOnDVDPlaybackRateChange      : TOnDVDPlaybackRateChange;
    FOnDVDParentalLevelChange     : TOnDVDParentalLevelChange;
    FOnDVDPlaybackStopped         : TNotifyEvent;
    FOnDVDAnglesAvailable         : TOnDVDAnglesAvailable;
    FOnDVDPlayPeriodAutoStop      : TNotifyEvent;
    FOnDVDButtonAutoActivated     : TOnDVDButtonAutoActivated;
    FOnDVDCMDStart                : TOnDVDCMD;
    FOnDVDCMDEnd                  : TOnDVDCMD;
    FOnDVDDiscEjected             : TNotifyEvent;
    FOnDVDDiscInserted            : TNotifyEvent;
    FOnDVDCurrentHMSFTime         : TOnDVDCurrentHMSFTime;
    FOnDVDKaraokeMode             : TOnDVDKaraokeMode;
    // DVD Warning
    FOnDVDWarningInvalidDVD1_0Disc  : TNotifyEvent;//=1,
    FOnDVDWarningFormatNotSupported : TNotifyEvent;//=2,
    FOnDVDWarningIllegalNavCommand  : TNotifyEvent;//=3
    FOnDVDWarningOpen               : TNotifyEvent;//=4
    FOnDVDWarningSeek               : TNotifyEvent;//=5
    FOnDVDWarningRead               : TNotifyEvent;//=6
    // DVDDomain
    FOnDVDDomainFirstPlay         : TNotifyEvent;
    FOnDVDDomainVideoManagerMenu  : TNotifyEvent;
    FOnDVDDomainVideoTitleSetMenu : TNotifyEvent;
    FOnDVDDomainTitle             : TNotifyEvent;
    FOnDVDDomainStop              : TNotifyEvent;
    // DVDError
    FOnDVDErrorUnexpected                          : TNotifyEvent;
    FOnDVDErrorCopyProtectFail                     : TNotifyEvent;
    FOnDVDErrorInvalidDVD1_0Disc                   : TNotifyEvent;
    FOnDVDErrorInvalidDiscRegion                   : TNotifyEvent;
    FOnDVDErrorLowParentalLevel                    : TNotifyEvent;
    FOnDVDErrorMacrovisionFail                     : TNotifyEvent;
    FOnDVDErrorIncompatibleSystemAndDecoderRegions : TNotifyEvent;
    FOnDVDErrorIncompatibleDiscAndDecoderRegions   : TNotifyEvent;

    FOnSelectedFilter: TOnSelectedFilter;
    FOnCreatedFilter: TOnCreatedFilter;
    FOnUnableToRender: TOnUnableToRender;

    procedure HandleEvents;
    procedure WndProc(var Msg: TMessage);
    procedure SetActive(Activate: boolean);
    procedure SetGraphMode(Mode: TGraphMode);
    procedure SetGraphEdit(enable: boolean);
    procedure ClearOwnFilters;
    procedure AddOwnFilters;
    procedure GraphEvents(Event, Param1, Param2: integer);
    procedure ControlEvents(Event: TControlEvent; Param: integer = 0);
    procedure SetLogFile(FileName: String);
    function GetState: TGraphState;
    procedure SetState(Value: TGraphState);
    procedure SetVolume(Volume: Integer);
    procedure SetBalance(Balance: integer);
    function GetSeekCaps: TSeekingCaps;
    procedure SetRate(Rate: double);
    function GetDuration: integer;
    procedure SetLinearVolume(aEnabled: Boolean);
    procedure UpdateGraph;

    // IAMGraphBuilderCallback
    function SelectedFilter(pMon: IMoniker): HResult; stdcall;
    function CreatedFilter(pFil: IBaseFilter): HResult; stdcall;

    // IAMFilterGraphCallback
    function UnableToRender(ph1, ph2: integer; pPin: IPin): HResult; // thiscall
  protected
    {@exclude}
    procedure DoEvent(Event, Param1, Param2: Integer); virtual;
    {@exclude}
    procedure InsertFilter(AFilter: IFilter);
    {@exclude}
    procedure RemoveFilter(AFilter: IFilter);
    {@exclude}
    procedure InsertEventNotifier(AEvent: IEvent);
    {@exclude}
    procedure RemoveEventNotifier(AEvent: IEvent);
    {@exclude}
    function QueryService(const rsid, iid: TGuid; out Obj): HResult; stdcall;
  public
    { Retrieve the total duration of a stream. }
    property Duration: Integer read GetDuration;
    { Retrieve/Set the rate. }
    property Rate: Double read fRate write SetRate;
    { Retrieve the seeking capabilities. }
    property SeekCapabilities: TSeekingCaps read GetSeekCaps;
    { The volume balance. }
    property Balance: integer read fBalance write SetBalance;
    { The volume. }
    property Volume: integer read fVolume write SetVolume;
    { Current state of the filter graph. }
    property State: TGraphState read GetState write SetState;
    { TFilterGraph constructor. }
    constructor Create(AOwner: TComponent); override;
    { TFilterGraph destructor. }
    destructor Destroy; override;
    { @exclude}
    procedure Loaded; override;
    { Retrieve an Interface from the current Graph.<br>
      <b>ex: </b> (FilterGraph <b>as</b> IGraphBuilder).RenderFile('C:\speedis.avi', <b>nil</b>);<br>
      <b>Remark: </b> The interfaces you can Query depend of the @link(Mode) you
      have defined.<br>
      <b>gmNormal: </b>IAMGraphStreams, IAMStats, IBasicAudio, IBasicVideo,
                       IBasicVideo2, IFilterChain, IFilterGraph, IFilterGraph2,
                       IFilterMapper2, IGraphBuilder, IGraphConfig, IGraphVersion,
                       IMediaControl, IMediaEvent, IMediaEventEx, IMediaEventSink,
                       IMediaFilter, IMediaPosition, IMediaSeeking, IQueueCommand,
                       IRegisterServiceProvider, IResourceManager, IServiceProvider,
                       IVideoFrameStep, IVideoWindow. <br>
      <b>gmCapture: </b> all gmNormal interfaces and ICaptureGraphBuilder2.<br>
      <b>gmDVD: </b> all gmNormal interfaces and IDvdGraphBuilder, IDvdControl2,
                     IDvdInfo2, IAMLine21Decoder.}
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
    { The Run method runs all the filters in the filter graph. While the graph
      is running, data moves through the graph and is rendered. }
    function Play: boolean;
    { The Pause method pauses all the filters in the filter graph. }
    function Pause: boolean;
    { The Stop method stops all the filters in the graph. }
    function Stop: boolean;
    { This method disconnect all pins.}
    procedure DisconnectFilters;
    { Disconnect and remove all filters from the filter graph excepting the custom components. }
    procedure ClearGraph;
    { Render a single file. }
    function RenderFile(FileName: WideString): HRESULT;
    function RenderFileEx(FileName: WideString): HRESULT;
    { Render a DVD Video Volume or a File Name if specified. }
    function RenderDVD(out status: TAMDVDRenderStatus;
      FileName: WideString = ''; Mode: Integer = AM_DVD_HWDEC_PREFER): HRESULT;
    { Save the current state and position of a DVD movie to a file.<br>
      See also: @link(DVDRestoreBookmark).}
    procedure DVDSaveBookmark(BookMarkFile: WideString);
    { Restore the State and position of a DVD movie saved by @link(DVDSaveBookmark).}
    procedure DVDRestoreBookmark(BookMarkFile: WideString);
  published

    { Specify a File Name to save the Filter Graph Log. }
    property LogFile: String read FLogFileName write SetLogFile;

    { Activate the Filter Graph.}
    property Active: boolean read FActive write SetActive default False;

    { Auto-Activate the Filter Graph when component is created.}
    property AutoCreate: boolean read FAutoCreate write FAutoCreate default False;

    { There is 3 modes: gmNormal, gmCapture and gmDVD. <br>
      See also: @link(GraphInterFace).}
    property Mode: TGraphMode read FMode write SetGraphMode default gmNormal;

    { if true you can use GraphEdit application to connect with the Filter Graph.}
    property GraphEdit: boolean read FGraphEdit write SetGraphEdit;

    { if true, Volume and Balance is set by using a linear algorythm instead of
      logatithmic. }
    property LinearVolume: Boolean read FLinearVolume write SetLinearVolume;

    // -------------------------------------------------------------------------
    // Events
    // -------------------------------------------------------------------------

    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;

    { Generic Filter Graph event.<br>
        <b>Event:</b> message sent.<br>
        <b>Param1:</b> first message parameter.<br>
        <b>Param2:</b> second message parameter.}
    property OnDSEvent: TOnDSEvent read FOnDSEvent write FOnDSEvent;

    { The graph is buffering data, or has stopped buffering data.
      A filter can send this event if it needs to buffer data from an external
      source. (for example, it might be loading data from a network.)
      The application can use this event to adjust its user interface.<br>
      <b>buffering:</b> TRUE if the graph is starting to buffer, or FALSE if
      the graph has stopped buffering. }
    property OnGraphBufferingData: TOnGraphBufferingData read FOnGraphBufferingData write FOnGraphBufferingData;

    { The reference clock has changed. The filter graph manager sends this event
      when its IMediaFilter.SetSyncSource method is called.}
    property OnGraphClockChanged: TNotifyEvent read FOnGraphClockChanged write FOnGraphClockChanged;

    { All data from a particular stream has been rendered.
      By default, the filter graph manager does not forward this event to the
      application. However, after all the streams in the graph report EC_COMPLETE,
      the filter graph manager posts a separate EC_COMPLETE event to the application.<br>
        <b>Result:</b> HRESULT value; can be S_OK.<br>
        <b>Renderer:</b> nil, or a reference to the renderer's IBaseFilter interface.}
    property OnGraphComplete: TOnGraphComplete read FOnGraphComplete write FOnGraphComplete;

    { A Plug and Play device was removed or became available again. When the
      device becomes available again, the previous state of the device filter is
      no longer valid. The application must rebuild the graph in order to use the device.<br>
        <b>Device:</b> IUnknown interface of the filter that represents the device.<br>
        <b>Removed:</b> True if the device was removed, or False if the device is available again.}
    property OnGraphDeviceLost: TOnGraphDeviceLost read FOnGraphDeviceLost write FOnGraphDeviceLost;

    { The end of a segment was reached.
      This event code supports seamless looping. When a call to the IMediaSeeking.SetPositions
      method includes the AM_SEEKING_Segment flag, the source filter sends this
      event code instead of calling IPin.EndOfStream.<br>
        <b>StreamTime:</b> TREFERENCE_TIME value that specifies the accumulated stream time since the start of the segment.<br>
        <b>NumSegment:</b> Cardinal value indicating the segment number (zero-based).}
    property OnGraphEndOfSegment: TOnGraphEndOfSegment read FOnGraphEndOfSegment write FOnGraphEndOfSegment;

    { An asynchronous command to run the graph has failed.
      if the filter graph manager issues an asynchronous run command that fails,
      it sends this event to the application. The graph remains in a running state.
      The state of the underlying filters is indeterminate. Some filters might be
      running, others might not.<br>
        <b>Result:</b> value of the operation that failed.}
    property OnGraphErrorStillPlaying: TOnDSResult read FOnGraphErrorStillPlaying write FOnGraphErrorStillPlaying;

    { An operation was aborted because of an error.<br>
        <b>Result:</b> value of the operation that failed.}
    property OnGraphErrorAbort: TOnDSResult read FOnGraphErrorAbort write FOnGraphErrorAbort;

    { The video renderer is switching out of full-screen mode.
      When the Full Screen Renderer loses activation, it sends this event. When
      another video renderer switches out of full-screen mode, the filter graph
      manager sends this event, in response to an EC_ACTIVATE event from the renderer.<br>
        <b>Renderer:</b> the video renderer's IBaseFilter interface, or nil.}
    property OnGraphFullscreenLost: TOnGraphFullscreenLost read FOnGraphFullscreenLost write FOnGraphFullscreenLost;

    { The filter graph has changed.
      This event code is intended for debugging. It is not sent for all graph changes.}
    property OnGraphChanged: TNotifyEvent read FOnGraphChanged write FOnGraphChanged;

    { A filter is passing a text string to the application.
      By convention, the first parameter contains type information (for example, Text)
      and the second parameter contains the text string.<br>
        <b>String1, String2:</b> Wide Strings}
    property OnGraphOleEvent: TOnGraphOleEvent read FOnGraphOleEvent write FOnGraphOleEvent;

    { The graph is opening a file, or has finished opening a file.
      A filter can send this event if it spends significant time opening a file.
      (for example, the file might be located on a network.) The application can use
      this event to adjust its user interface.<br>
        <b>opening:</b> TRUE if the graph is starting to open a file, or FALSE
                        if the graph is no longer opening the file.}
    property OnGraphOpeningFile: TOnGraphOpeningFile read FOnGraphOpeningFile write FOnGraphOpeningFile;

    {  The video palette has changed.
       Video renderers send this event if they detect a palette change in the stream.}
    property OnGraphPaletteChanged: TNotifyEvent read FOnGraphPaletteChanged write FOnGraphPaletteChanged;

    { A pause request has completed.
      The filter graph manager sends this event when it completes an asynchronous pause command.<br>
        <b>Result:</b> value that indicates the result of the transition. if the
                       value is S_OK, the filter graph is now in a paused state.}
    property OnGraphPaused: TOnDSResult read FOnGraphPaused write FOnGraphPaused;

    { The graph is dropping samples, for quality control.
      A filter sends this event if it drops samples in response to a quality control
      message. It sends the event only when it adjusts the quality level, not for each
      sample that it drops. }
    property OnGraphQualityChange: TNotifyEvent read FOnGraphQualityChange write FOnGraphQualityChange;

    { An audio device error occurred on an input pin.<br>
        <b>OccurWhen:</b> value from the TSNDDEV_ERR enumerated type, indicating how the device was being accessed when the failure occurred.<br>
        <b>ErrorCode:</b> value indicating the error returned from the sound device call.}
    property OnGraphSNDDevInError: TOnGraphSNDDevError read FOnGraphSNDDevInError write FOnGraphSNDDevInError;

    { An audio device error occurred on an output pin.<br>
        <b>OccurWhen:</b> value from the TSNDDEV_ERR enumerated type, indicating how the device was being accessed when the failure occurred.<br>
        <b>ErrorCode:</b> value indicating the error returned from the sound device call.}
    property OnGraphSNDDevOutError: TOnGraphSNDDevError read FOnGraphSNDDevOutError write FOnGraphSNDDevOutError;

    { A filter has completed frame stepping.
      The filter graph manager pauses the graph and passes the event to the application.}
    property OnGraphStepComplete: TNotifyEvent read FOnGraphStepComplete write FOnGraphStepComplete;

    { A stream-control start command has taken effect.
      Filters send this event in response to the IAMStreamControl.StartAt method.
      This method specifies a reference time for a pin to begin streaming.
      When streaming does begin, the filter sends this event.<br>
        <b>PinSender</b> parameter specifies the pin that executes the start command.
                         Depending on the implementation, it might not be the pin that
                         received the StartAt call.<br>
        <b>Cookie</b> parameter is specified by the application in the StartAt method.
                      This parameter enables the application to track multiple calls to the method.}
    property OnGraphStreamControlStarted: TOnGraphStreamControl read FOnGraphStreamControlStarted  write FOnGraphStreamControlStarted;

    { A stream-control start command has taken effect.
      Filters send this event in response to the IAMStreamControl.StopAt method.
      This method specifies a reference time for a pin to stop streaming.
      When streaming does halt, the filter sends this event.<br>
        <b>PinSender</b> parameter specifies the pin that executes the stop command.
                         Depending on the implementation, it might not be the pin
                         that received the StopAt call.<br>
        <b>Cookie</b> parameter is specified by the application in the StopAt method.
                      This parameter enables the application to track multiple calls to the method.}
    property OnGraphStreamControlStopped: TOnGraphStreamControl read FOnGraphStreamControlStopped write FOnGraphStreamControlStopped;

    { An error occurred in a stream, but the stream is still playing.<br>
        <b>Operation:</b> HRESULT of the operation that failed.<br>
        <b>Value:</b> LongWord value, generally zero. }
    property OnGraphStreamErrorStillPlaying : TOnGraphStreamError read FOnGraphStreamErrorStillPlaying write FOnGraphStreamErrorStillPlaying;

    { A stream has stopped because of an error.<br>
        <b>Operation:</b> HRESULT of the operation that failed.<br>
        <b>Value:</b> LongWord value, generally zero. }
    property OnGraphStreamErrorStopped: TOnGraphStreamError read FOnGraphStreamErrorStopped write FOnGraphStreamErrorStopped;

    { The user has terminated playback.<br>
      This event code signals that the user has terminated normal graph playback.
      for example, video renderers send this event if the user closes the video window.<br>
      After sending this event, the filter should reject all samples and not send
      any EC_REPAINT events, until the filter stops and is reset.}
    property OnGraphUserAbort: TNotifyEvent read FOnGraphUserAbort write FOnGraphUserAbort;

    { The native video size has changed.<br>
        <b>width:</b> new width, in pixels.<br>
        <b>height:</b> new height, in pixels. }
    property OnGraphVideoSizeChanged: TOnGraphVideoSizeChanged read FOnGraphVideoSizeChanged write FOnGraphVideoSizeChanged;

    { Sent by filter supporting timecode.<br>
        <b>From:</b> sending object.<br>
        <b>DeviceID:</b> device ID of the sending object}
    property OnGraphTimeCodeAvailable: TOnGraphTimeCodeAvailable read FOnGraphTimeCodeAvailable write FOnGraphTimeCodeAvailable;

    { Sent by filter supporting IAMExtDevice.<br>
        <b>NewMode:</b> the new mode<br>
        <b>DeviceID:</b> the device ID of the sending object}
    property OnGraphEXTDeviceModeChange: TOnGraphEXTDeviceModeChange read FOnGraphEXTDeviceModeChange write FOnGraphEXTDeviceModeChange;

    { The clock provider was disconnected.<br>
      KSProxy signals this event when the pin of a clock-providing filter is disconnected.}
    property OnGraphClockUnset: TNotifyEvent read FOnGraphClockUnset write FOnGraphClockUnset;

    { Identifies the type of rendering mechanism the VMR is using to display video.}
    property OnGraphVMRRenderDevice: TOnGraphVMRRenderDevice read FOnGraphVMRRenderDevice write FOnGraphVMRRenderDevice;

    { Signals that the current audio stream number changed for the main title.<br>
      The current audio stream can change automatically with a navigation command
      authored on the disc as well as through application control by using the IDvdControl2 interface.<br>
      <b>stream:</b> value indicating the new user audio stream number. Audio stream numbers
        range from 0 to 7. Stream $FFFFFFFF indicates that no stream is selected.<br>
      <b>lcid:</b> Language identifier.<br>
      <b>Lang:</b> Language string. }
    property OnDVDAudioStreamChange: TOnDVDAudioStreamChange read FOnDVDAudioStreamChange write FOnDVDAudioStreamChange;

    { Deprecated, use @link(OnDVDCurrentHMSFTime) instead.<br>
      Signals the beginning of every video object unit (VOBU), a video segment
      which is 0.4 to 1.0 seconds in length.<br> }
    property OnDVDCurrentTime: TOnDVDCurrentTime read FOnDVDCurrentTime write FOnDVDCurrentTime;

    { Indicates when the current title number changes.<br>
      Title numbers range from 1 to 99. This number indicates the TTN, which is
      the title number with respect to the whole disc, not the VTS_TTN which is
      the title number with respect to just a current VTS.<br>
      <b>Title:</b> value indicating the new title number.}
    property OnDVDTitleChange: TOnDVDTitleChange read FOnDVDTitleChange write FOnDVDTitleChange;

    { Signals that the DVD player started playback of a new program in the
      DVD_DOMAIN_Title domain.<br>
      Only simple linear movies signal this event.<br>
      <b>chapter:</b> value indicating the new chapter (program) number.}
    property OnDVDChapterStart: TOnDVDChapterStart read FOnDVDChapterStart write FOnDVDChapterStart;

    { Signals that either the number of available angles changed or that the
      current angle number changed.<br>
      Angle numbers range from 1 to 9. The current angle number can change
      automatically with a navigation command authored on the disc as well as
      through application control by using the IDvdControl2 interface.<br>
      <b>total:</b> value indicating the number of available angles. When the
         number of available angles is 1, the current video is not multiangle.<br>
      <b>current:</b> value indicating the current angle number.}
    property OnDVDAngleChange: TOnDVDChange read FOnDVDAngleChange write FOnDVDAngleChange;

    { Signals that the available set of IDvdControl2 interface methods has changed.<br>
      <b>UOPS:</b> value representing a ULONG whose bits indicate which IDvdControl2
      commands the DVD disc explicitly disabled. }
    property OnDVDValidUOPSChange: TOnDVDValidUOPSChange read FOnDVDValidUOPSChange write FOnDVDValidUOPSChange;

    { Signals that either the number of available buttons changed or that the
      currently selected button number changed.<br>
      This event can signal any of the available button numbers. These numbers
      do not always correspond to button numbers used for
      IDvdControl2.SelectAndActivateButton because that method can activate only
      a subset of buttons.<br>
      <b>total:</b> value indicating the number of available buttons.<br>
      <b>current:</b> value indicating the currently selected button number.
        Selected button number zero implies that no button is selected.}
    property OnDVDButtonChange: TOnDVDChange read FOnDVDButtonChange write FOnDVDButtonChange;

    { Indicates that playback stopped as the result of a call to the
      IDvdControl2.PlayChaptersAutoStop method.}
    property OnDVDChapterAutoStop: TNotifyEvent read FOnDVDChapterAutoStop write FOnDVDChapterAutoStop;

    { Signals the beginning of any still (PGC, Cell, or VOBU).
      All combinations of buttons and still are possible (buttons on with still
      on, buttons on with still off, button off with still on, button off with still off).<br>
      <b>NoButtonAvailable</b>: Boolean value indicating whether buttons are
      available. False indicates buttons are available so the IDvdControl2.StillOff
      method won't work. True indicates no buttons are available, so IDvdControl2.StillOff will work.<br>
      <b>seconds</b>: value indicating the number of seconds the still will last.
      $FFFFFFFF indicates an infinite still, meaning wait until the user presses
      a button or until the application calls IDvdControl2.StillOff.}
    property OnDVDStillOn: TOnDVDStillOn read FOnDVDStillOn write FOnDVDStillOn;

    { Signals the end of any still (PGC, Cell, or VOBU).<br>
      This event indicates that any currently active still has been released.}
    property OnDVDStillOff: TNotifyEvent read FOnDVDStillOff write FOnDVDStillOff;

    { Signals that the current subpicture stream number changed for the main title.<br>
      The subpicture can change automatically with a navigation command authored
      on disc as well as through application control using IDvdControl2.<br>
      <b>SubNum:</b> value indicating the new user subpicture stream number.
        Subpicture stream numbers range from 0 to 31. Stream $FFFFFFFF indicates
        that no stream is selected.<br>
      <b>lcid:</b> Language identifier.<br>
      <b>Lang:</b> Language string.}
    property OnDVDSubpictureStreamChange: TOnDVDSubpictureStreamChange read FOnDVDSubpictureStreamChange write FOnDVDSubpictureStreamChange;

    { Signals that the DVD disc does not have a FP_PGC (First Play Program Chain)
      and that the DVD Navigator will not automatically load any PGC and start playback.}
    property OnDVDNoFP_PGC: TNotifyEvent read FOnDVDNoFP_PGC write FOnDVDNoFP_PGC;

    { Signals that a rate change in the playback has been initiated.
      <b>rate:</b> indicate the new playback rate. rate < 0 indicates reverse playback
      mode. rate > 0 indicates forward playback mode.}
    property OnDVDPlaybackRateChange: TOnDVDPlaybackRateChange read FOnDVDPlaybackRateChange write FOnDVDPlaybackRateChange;

    { Signals that the parental level of the authored content is about to change.<br>
      The DVD Navigator source filter does not currently support "on the fly"
      parental level changes in response to SetTmpPML commands on a DVD disc.<br>
      <b>level:</b> value representing the new parental level set in the player.}
    property OnDVDParentalLevelChange: TOnDVDParentalLevelChange read FOnDVDParentalLevelChange write FOnDVDParentalLevelChange;

    { Indicates that playback has been stopped. The DVD Navigator has completed
      playback of the title or chapter and did not find any other branching
      instruction for subsequent playback. }
    property OnDVDPlaybackStopped: TNotifyEvent read FOnDVDPlaybackStopped write FOnDVDPlaybackStopped;

    { Indicates whether an angle block is being played and angle changes can be performed.<br>
      Angle changes are not restricted to angle blocks and the manifestation of
      the angle change can be seen only in an angle block.<br>
      <b>available:</b> Boolean value that indicates if an angle block is being
      played back. False indicates that playback is not in an angle block and
      angles are not available, True indicates that an angle block is being played
      back and angle changes can be performed.}
    property OnDVDAnglesAvailable: TOnDVDAnglesAvailable read FOnDVDAnglesAvailable write FOnDVDAnglesAvailable;

    { Indicates that the Navigator has finished playing the segment specified
      in a call to PlayPeriodInTitleAutoStop.}
    property OnDVDPlayPeriodAutoStop: TNotifyEvent read FOnDVDPlayPeriodAutoStop write FOnDVDPlayPeriodAutoStop;

    { Signals that a menu button has been automatically activated per instructions
      on the disc. This occurs when a menu times out and the disc has specified a
      button to be automatically activated.<br>
      <b>Button</b>: value indicating the button that was activated.}
    property OnDVDButtonAutoActivated: TOnDVDButtonAutoActivated read FOnDVDButtonAutoActivated write FOnDVDButtonAutoActivated;

    { Signals that a particular command has begun.<br>
      <b>CmdID:</b> The Command ID and the HRESULT return value.}
    property OnDVDCMDStart: TOnDVDCMD read FOnDVDCMDStart Write FOnDVDCMDStart;

    { Signals that a particular command has completed.<br>
      <b>CmdID</b> The Command ID and the completion result.}
    property OnDVDCMDEnd: TOnDVDCMD read FOnDVDCMDEnd Write FOnDVDCMDEnd;

    { Signals that a disc was ejected.<br>
      Playback automatically stops when a disc is ejected. The application does
      not have to take any special action in response to this event.}
    property OnDVDDiscEjected: TNotifyEvent read FOnDVDDiscEjected Write FOnDVDDiscEjected;

    { Signals that a disc was inserted into the drive.<br>
      Playback automatically begins when a disc is inserted. The application does
      not have to take any special action in response to this event.}
    property OnDVDDiscInserted: TNotifyEvent read FOnDVDDiscInserted write FOnDVDDiscInserted;

    { Signals the current time, in DVD_HMSF_TIMECODE format, relative to the start
      of the title. This event is triggered at the beginning of every VOBU, which
      occurs every 0.4 to 1.0 seconds.<br>
      The TDVD_HMSF_TIMECODE format is intended to replace the old BCD format that
      is returned in OnDVDCurrentTime events. The HMSF timecodes are easier to
      work with. To have the Navigator send EC_DVD_CURRENT_HMSF_TIME events instead
      of EC_DVD_CURRENT_TIME events, an application must call
      IDvdControl2.SetOption(DVD_HMSF_TimeCodeEvents, TRUE). When this flag is set,
      the Navigator will also expect all time parameters in the IDvdControl2 and
      IDvdInfo2 methods to be passed as TDVD_HMSF_TIMECODEs.<br>
      <b>HMSFTimeCode:</b> HMS Time code structure.<br>
      <b>TimeCode:</b> old time format, do not use. }
    property OnDVDCurrentHMSFTime: TOnDVDCurrentHMSFTime read FOnDVDCurrentHMSFTime write FOnDVDCurrentHMSFTime;

    { Indicates that the Navigator has either begun playing or finished playing karaoke data.<br>
      The DVD player signals this event whenever it changes domains.<br>
      <b>Played:</b> TRUE means that a karaoke track is being played and FALSE means
        that no karaoke data is being played. }
    property OnDVDKaraokeMode: TOnDVDKaraokeMode read FOnDVDKaraokeMode write FOnDVDKaraokeMode;

    { Performing default initialization of a DVD disc.}
    property OnDVDDomainFirstPlay: TNotifyEvent read FOnDVDDomainFirstPlay write FOnDVDDomainFirstPlay;

    { Displaying menus for whole disc. }
    property OnDVDDomainVideoManagerMenu: TNotifyEvent read FOnDVDDomainVideoManagerMenu write FOnDVDDomainVideoManagerMenu;

    { Displaying menus for current title set. }
    property OnDVDDomainVideoTitleSetMenu: TNotifyEvent read FOnDVDDomainVideoTitleSetMenu write FOnDVDDomainVideoTitleSetMenu;

    { Displaying the current title. }
    property OnDVDDomainTitle: TNotifyEvent read FOnDVDDomainTitle write FOnDVDDomainTitle;

    { The DVD Navigator is in the DVD Stop domain.}
    property OnDVDDomainStop: TNotifyEvent read FOnDVDDomainStop write FOnDVDDomainStop;

    { Something unexpected happened; perhaps content is authored incorrectly.
      Playback is stopped.}
    property OnDVDErrorUnexpected: TNotifyEvent read FOnDVDErrorUnexpected write FOnDVDErrorUnexpected;

    { Key exchange for DVD copy protection failed. Playback is stopped. }
    property OnDVDErrorCopyProtectFail: TNotifyEvent read FOnDVDErrorCopyProtectFail write FOnDVDErrorCopyProtectFail;

    { DVD-Video disc is authored incorrectly for specification version 1.x.
      Playback is stopped.}
    property OnDVDErrorInvalidDVD1_0Disc: TNotifyEvent read FOnDVDErrorInvalidDVD1_0Disc write FOnDVDErrorInvalidDVD1_0Disc;

    { DVD-Video disc cannot be played because the disc is not authored to play in
      the system region. }
    property OnDVDErrorInvalidDiscRegion: TNotifyEvent read FOnDVDErrorInvalidDiscRegion write FOnDVDErrorInvalidDiscRegion;

    { Player parental level is lower than the lowest parental level available in
      the DVD content. Playback is stopped. }
    property OnDVDErrorLowParentalLevel: TNotifyEvent read FOnDVDErrorLowParentalLevel write FOnDVDErrorLowParentalLevel;

    { Macrovision® distribution failed. Playback stopped. }
    property OnDVDErrorMacrovisionFail: TNotifyEvent read FOnDVDErrorMacrovisionFail write FOnDVDErrorMacrovisionFail;

    { No discs can be played because the system region does not match the decoder region. }
    property OnDVDErrorIncompatibleSystemAndDecoderRegions: TNotifyEvent read FOnDVDErrorIncompatibleSystemAndDecoderRegions write FOnDVDErrorIncompatibleSystemAndDecoderRegions;

    { The disc cannot be played because the disc is not authored to be played in
      the decoder's region. }
    property OnDVDErrorIncompatibleDiscAndDecoderRegions: TNotifyEvent read FOnDVDErrorIncompatibleDiscAndDecoderRegions  write FOnDVDErrorIncompatibleDiscAndDecoderRegions;

    { DVD-Video disc is authored incorrectly. Playback can continue, but unexpected
      behavior might occur. }
    property OnDVDWarningInvalidDVD1_0Disc: TNotifyEvent read FOnDVDWarningInvalidDVD1_0Disc write FOnDVDWarningInvalidDVD1_0Disc;

    { A decoder would not support the current format. Playback of a stream
     (audio, video or subpicture) might not function. }
    property OnDVDWarningFormatNotSupported : TNotifyEvent read FOnDVDWarningFormatNotSupported write FOnDVDWarningFormatNotSupported;

    { The internal DVD navigation command processor attempted to process an illegal command.}
    property OnDVDWarningIllegalNavCommand  : TNotifyEvent read FOnDVDWarningIllegalNavCommand  write FOnDVDWarningIllegalNavCommand;

    { File Open failed. }
    property OnDVDWarningOpen: TNotifyEvent read FOnDVDWarningOpen write FOnDVDWarningOpen;

    { File Seek failed. }
    property OnDVDWarningSeek: TNotifyEvent read FOnDVDWarningSeek write FOnDVDWarningSeek;

    { File Read failed. }
    property OnDVDWarningRead: TNotifyEvent read FOnDVDWarningRead write FOnDVDWarningRead;

    { Notifys when a Moniker has been found for a MediaType of a Pin in the Graph.
      Return True to allow this Filter to be added, otherwise return False.
      Note: The Guid might not be the real Filter Class ID, but a Group ID.
      eg: Renderer Filters. }
    property OnSelectedFilter: TOnSelectedFilter read FOnSelectedFilter write FOnSelectedFilter;

    { Notifys when a Filter has been created and is about to enter the Graph.
      Return True to allow this Filter to be added, otherwise return False. }
    property OnCreatedFilter: TOnCreatedFilter read FOnCreatedFilter write FOnCreatedFilter;

    { Notifys about a Pin that couldn't be Rendered. Return True to try it again,
      otherwise return False. }
    property OnUnableToRender: TOnUnableToRender read FOnUnableToRender write FOnUnableToRender;
  end;


// *****************************************************************************
//   TVMROptions
// *****************************************************************************

  {@exclude}
  TVideoWindow = class;

  { See VRMOptions.<br>}
  TVMRVideoMode = (
    vmrWindowed,
    vmrWindowless,
    vmrRenderless
  );

  { Video Mixer Renderer property editor. }
  TVMROptions = class(TPersistent)
  private
    FOwner: TVideoWindow;
    FStreams: cardinal;
    FPreferences: TVMRPreferences;
    FMode: TVMRVideoMode;
    FKeepAspectRatio: boolean;
    procedure SetStreams(Streams: cardinal);
    procedure SetPreferences(Preferences: TVMRPreferences);
    procedure SetMode(AMode: TVMRVideoMode);
    procedure SetKeepAspectRatio(Keep: boolean);
  public
    { Constructor method. }
    constructor Create(AOwner: TVideoWindow);
  published
    { Windowed or WindowLess}
    property Mode: TVMRVideoMode read FMode write SetMode;
    { Sets the number of streams to be mixed. }
    property Streams: Cardinal read FStreams write SetStreams default 4;
    { Sets various application preferences related to video rendering. }
    property Preferences: TVMRPreferences read FPreferences write SetPreferences default [vpForceMixer];
    { Keep Aspect Ration on the video window. }
    property KeepAspectRatio: boolean read FKeepAspectRatio write SetKeepAspectRatio default True;
  end;

// *****************************************************************************
//   TVideoWindow
// *****************************************************************************

  TAbstractAllocator = class(TInterfacedObject)
    constructor Create(out hr: HResult; wnd: THandle; d3d: IDirect3D9 = nil; d3dd: IDirect3DDevice9 = nil); virtual; abstract;
  end;
  TAbstractAllocatorClass = class of TAbstractAllocator;

  { Manage a Video Renderer or a Video Mixer Renderer (VMR) Filter to display
    a video in your application. }
  TVideoWindow = class(TCustomControl, IFilter, IEvent)
  private
    FMode          : TVideoMode;
    FVMROptions    : TVMROptions;
    FBaseFilter    : IBaseFilter;
    FVideoWindow   : IVideoWindow; // VMR Windowed & Normal
    FWindowLess    : IVMRWindowlessControl9; // VMR Windowsless

    FFullScreen    : boolean;
    FFilterGraph   : TFilterGraph;
    FWindowStyle   : LongWord;
    FWindowStyleEx : LongWord;
    FTopMost       : boolean;
    FIsFullScreen  : boolean;
    FOnPaint       : TNotifyEvent;
    FKeepAspectRatio: boolean;
    FAllocatorClass: TAbstractAllocatorClass;
    FCurrentAllocator: TAbstractAllocator;
    FRenderLessUserID: Cardinal;
    procedure SetVideoMode(AMode: TVideoMode);
    procedure SetFilterGraph(AFilterGraph: TFilterGraph);
    procedure SetFullScreen(Value: boolean);
    procedure NotifyFilter(operation: TFilterOperation; Param: integer = 0);
    procedure GraphEvent(Event, Param1, Param2: integer);
    function GetName: string;
    function GetVideoHandle: THandle;
    procedure ControlEvent(Event: TControlEvent; Param: integer = 0);
    procedure SetTopMost(TopMost: boolean);
    function GetVisible: boolean;
    procedure SetVisible(Vis: boolean);
  protected
    FIsVideoWindowOwner: Boolean;
    {@exclude}
    procedure Loaded; override;
    {@exclude}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    {@exclude}
    procedure Resize; override;
    {@exclude}
    procedure ConstrainedResize(var MinWidth, MinHeight, MaxWidth, MaxHeight: Integer); override;
    {@exclude}
    function GetFilter: IBaseFilter;
    {@exclude}
    procedure WndProc(var Message: TMessage); override;
    {@exclude}
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    {@exclude}
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    {@exclude}
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    {@exclude}
    procedure Paint; override;
  public
    {@exclude}
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
    { Constructor. }
    constructor Create(AOwner: TComponent);override;
    { Destructor. }
    destructor Destroy; override;
    { Check if the Video Mixer Renderer is available (Windows XP). }
    class function CheckVMR: boolean;
    { Retrieve the current bitmap, only in WindowLess VMR Mode. }
    function VMRGetBitmap(Stream: TStream): boolean;
    function CheckInputPinsConnected: boolean;
    procedure SetAllocator(Allocator: TAbstractAllocatorClass; UserID: Cardinal);
    property IsVideoWindowOwner: Boolean read FIsVideoWindowOwner write FIsVideoWindowOwner;
  published
    { VMR/WindowsLess Mode only.}
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    { The video Window stay on Top in FullScreen Mode. }
    property FullScreenTopMost: boolean read FTopMost write SetTopMost default false;
    { Video Mode, you can use Normal mode or VMR mode (VMR is only available on WindowsXP). }
    property Mode: TVideoMode read FMode write SetVideoMode default vmNormal;
    { The @link(TFilterGraph) component }
    property FilterGraph: TFilterGraph read FFilterGraph write SetFilterGraph;
    { Return the Handle where the video is displayed. }
    property VideoHandle: THandle read GetVideoHandle;
    { Video Mixer Renderer property editor. }
    property VMROptions: TVMROptions read FVMROptions write FVMROptions;
    { Set the full screen mode. }
    property FullScreen: boolean read FFullScreen write SetFullScreen default false;
    { Common properties & Events }
                                   {@exclude}
    property Color;                {@exclude}
    property Visible: boolean read GetVisible write SetVisible default True; {@exclude}
    property ShowHint;             {@exclude}
    property Anchors;              {@exclude}
    property Canvas;               {@exclude}
    property PopupMenu;            {@exclude}
    property Align;                {@exclude}
    property TabStop default True; {@exclude}
    property OnEnter;              {@exclude}
    property OnExit;               {@exclude}
    property OnKeyDown;            {@exclude}
    property OnKeyPress;           {@exclude}
    property OnKeyUp;              {@exclude}
    property OnCanResize;          {@exclude}
    property OnClick;              {@exclude}
    property OnConstrainedResize;  {@exclude}
    property OnDblClick;           {@exclude}
    property OnMouseDown;          {@exclude}
    property OnMouseMove;          {@exclude}
    property OnMouseUp;            {@exclude}
    property OnMouseWheel;         {@exclude}
    property OnMouseWheelDown;     {@exclude}
    property OnMouseWheelUp;       {@exclude}
    property OnResize;
  end;

//******************************************************************************
//
//  TFilterSampleGrabber declaration
//  description: Sample Grabber Wrapper Filter
//
//******************************************************************************
  {@exclude}
  TSampleGrabber = class;

  { This class is designed make a snapshoot of Video or Audio Datas.
    WARNING: There is know problems with some DIVX movies, so use RGB32 Media Type
    instead of RBG24.}
  TSampleGrabber = class(TComponent, IFilter, ISampleGrabberCB)
  private
    FOnBuffer: TOnBuffer;
    FBaseFilter: IBaseFilter;
    FFilterGraph : TFilterGraph;
    FMediaType: TMediaType;
    // [pjh, 2003-07-14] delete BMPInfo field
    // BMPInfo : PBitmapInfo;
    FCriticalSection: TCriticalSection;
    function GetFilter: IBaseFilter;
    function GetName: string;
    procedure NotifyFilter(operation: TFilterOperation; Param: integer = 0);
    procedure SetFilterGraph(AFilterGraph: TFilterGraph);
    function  SampleCB(SampleTime: Double; pSample: IMediaSample): HResult; stdcall;
    function  BufferCB(SampleTime: Double; pBuffer: PByte; BufferLen: longint): HResult; stdcall;
  protected
    {@exclude}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { ISampleGrabber Interface to control the SampleGrabber Filter.
      The FilterGraph must be active.}
    SampleGrabber: ISampleGrabber;
    { The Input Pin.
      The FilterGraph must be active.}
    InPutPin  : IPin;
    { The Output Pin.
      The FilterGraph must be active.}
    OutPutPin : IPin;
    { Constructor method. }
    constructor Create(AOwner: TComponent); override;
    { Destructor method. }
    destructor Destroy; override;
    { Configure the filter to cature the specified MediaType.
      This method disconnect the Input pin if connected.
      The FilterGraph must be active. }
    procedure UpdateMediaType;
    {@exclude}
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
    { Configure the MediaType according to the Source MediaType to be compatible with the BMP format.
      if Source = nil then this method use the default value to set the resolution: 1..32.
      The MediaType is auto configured to RGB24.}
    procedure SetBMPCompatible(Source: PAMMediaType; SetDefault: cardinal);
    { This method read the buffer received in the OnBuffer event and paint the bitmap.}
    function GetBitmap(Bitmap: TBitmap; Buffer: Pointer; BufferLen: Integer): boolean; overload;
    { This method read the current buffer from the Sample Grabber Filter and paint the bitmap.}
    function GetBitmap(Bitmap: TBitmap): boolean; overload;
    { This method check if the Sample Grabber Filter is correctly registered on the system. }
    class function CheckFilter: boolean;
  published
    { Receive the Buffer from the Sample Grabber Filter. }
    property OnBuffer: TOnBuffer read FOnBuffer write FOnBuffer;
    { The filter must connected to a TFilterGraph component.}
    property FilterGraph: TFilterGraph read FFilterGraph write SetFilterGraph;
    { The media type to capture. You can capture audio or video data. }
    property MediaType: TMediaType read FMediaType write FMediaType;
  end;

// *****************************************************************************
//  TFilter
// *****************************************************************************

  { This component is an easy way to add a specific filter to a filter graph.
    You can retrieve an interface using the <b>as</b> operator whith D6 :)}
  TFilter = class(TComponent, IFilter)
  private
    FFilterGraph : TFilterGraph;
    FBaseFilter: TBaseFilter;
    FFilter: IBaseFilter;
    function GetFilter: IBaseFilter;
    function GetName: string;
    procedure NotifyFilter(operation: TFilterOperation; Param: integer = 0);
    procedure SetFilterGraph(AFilterGraph: TFilterGraph);
  protected
    {@exclude}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Constructor method. }
    constructor Create(AOwner: TComponent); override;
    { Destructor method. }
    destructor Destroy; override;
    { Retrieve a filter interface. }
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
  published
    { This is the Filter Editor .}
    property BaseFilter: TBaseFilter read FBaseFilter write FBaseFilter;
    { The filter must be connected to a TFilterGraph component.}
    property FilterGraph: TFilterGraph read FFilterGraph write SetFilterGraph;
  end;

// *****************************************************************************
//  TASFWriter
// *****************************************************************************

  { This component is designed to create a ASF file or to stream over a network.}
  TASFWriter = class(TComponent, IFilter)
  private
    FFilterGraph : TFilterGraph;
    FFilter      : IBaseFilter;
    FPort        : Cardinal;
    FMaxUsers    : Cardinal;
    FProfile     : TWMPofiles8;
    FFileName    : WideString;
    FAutoIndex   : boolean;
    FMultiPass   : boolean;
    FDontCompress: boolean;
    function  GetProfile: TWMPofiles8;
    procedure SetProfile(profile: TWMPofiles8);
    function  GetFileName: String;
    procedure SetFileName(FileName: String);
    function  GetFilter: IBaseFilter;
    function  GetName: string;
    procedure NotifyFilter(operation: TFilterOperation; Param: integer = 0);
    procedure SetFilterGraph(AFilterGraph: TFilterGraph);
  protected
    {@exclude}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Sink configuration. }
    WriterAdvanced2      : IWMWriterAdvanced2;
    { NetWork streaming configuration. }
    WriterNetworkSink    : IWMWriterNetworkSink;
    { The Audio Input Pin. }
    AudioInput           : IPin;
    { The Video Input Pin. }
    VideoInput           : IPin;
    { Audio Input configuration. }
    AudioStreamConfig    : IAMStreamConfig;
    { VideoInput configuration}
    VideoStreamConfig    : IAMStreamConfig;
    { Destructor method. }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {@exclude}
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
  published
    { The filter must be connected to a TFilterGraph component.}
    property FilterGraph: TFilterGraph read FFilterGraph write SetFilterGraph;
    { Windows media profile to use. }
    property Profile: TWMPofiles8 read GetProfile write SetProfile;
    { Destination file name to write the compressed file. }
    property FileName: String read GetFileName write SetFileName;
    { Port number to stream.}
    property Port: DWORD read FPort write FPort;
    { The max number of connections. }
    property MaxUsers: DWORD read FMaxUsers write FMaxUsers;
    property AutoIndex   : boolean read FAutoIndex write FAutoIndex default True;
    property MultiPass   : boolean read FMultiPass write FMultiPass default False;
    property DontCompress: boolean read FDontCompress write FDontCompress default False;

  end;

// *****************************************************************************
//  TDSTrackBar
// *****************************************************************************
  {@exclude}
  TTimerEvent = procedure(sender: TObject; CurrentPos, StopPos: Cardinal) of object ;

  { This control implement a seek bar for a media-player application.
    The seek bar is implemented as a TTrackbar control. }
  TDSTrackBar = class(TTrackBar, IEvent)
  private
    FFilterGraph: TFilterGraph;
    FMediaSeeking: IMediaSeeking;
    FWindowHandle: HWND;
    FInterval: Cardinal;
    FOnTimer: TTimerEvent;
    FEnabled: Boolean;
    FMouseDown: boolean;
    procedure UpdateTimer;
    procedure SetTimerEnabled(Value: Boolean);
    procedure SetInterval(Value: Cardinal);
    procedure SetOnTimer(Value: TTimerEvent);
    procedure SetFilterGraph(AFilterGraph: TFilterGraph);
    procedure GraphEvent(Event, Param1, Param2: integer);
    procedure ControlEvent(Event: TControlEvent; Param: integer = 0);
    procedure TimerWndProc(var Msg: TMessage);
    property TimerEnabled: Boolean read FEnabled write SetTimerEnabled;
  protected
    {@exclude}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    {@exclude}
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    {@exclude}
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    {@exclude}
    procedure Timer; dynamic;
  public
    { constructor method. }
    constructor Create(AOwner: TComponent); override;
    { destructor method. }
    destructor Destroy; override;
  published
    { Select the filtergraph to seek. }
    property FilterGraph: TFilterGraph read FFilterGraph Write SetFilterGraph;
    { Select the time interval in miliseconds. default = 1000 mls. }
    property TimerInterval: Cardinal read FInterval write SetInterval default 1000;
    { OnTimer event, you can retrieve the current and stop positions here. }
    property OnTimer: TTimerEvent read FOnTimer write SetOnTimer;
  end;

  { @exclude }
  TDSVideoWindowEx2 = class;

// *****************************************************************************
//  TColorControl
// *****************************************************************************

  { Set and Get ColorControls from DSVideoWindowEx's OverlayMixer.
    This is Hardware based so your graphic card has to support it.
    Check DSVideoWindowEx's Capabilities if your card support a given
    colorcontrol.}
  TColorControl = class(TPersistent)
  private
    FBrightness : Integer;
    FContrast   : Integer;
    FHue        : Integer;
    FSaturation : Integer;
    FSharpness  : Integer;
    FGamma      : Integer;
    FUtilColor  : Boolean;
    FDefault    : TDDColorControl;
  protected
    { Protected declarations }
    { @exclude }
    FOwner : TDSVideoWindowEx2;
    { @exclude }
    Procedure SetBrightness(Value : Integer);
    { @exclude }
    Procedure SetContrast(Value : Integer);
    { @exclude }
    procedure SetHue(Value : Integer);
    { @exclude }
    procedure SetSaturation(Value : Integer);
    { @exclude }
    procedure SetSharpness(Value : Integer);
    { @exclude }
    procedure SetGamma(Value : Integer);
    { @exclude }
    procedure SetUtilColor(Value : Boolean);

    { @exclude }
    function GetBrightness : Integer;
    { @exclude }
    function GetContrast : Integer;
    { @exclude }
    function GetHue : Integer;
    { @exclude }
    function GetSaturation : Integer;
    { @exclude }
    function GetSharpness : Integer;
    { @exclude }
    function GetGamma : Integer;
    { @exclude }
    function GetUtilColor : Boolean;
    { @exclude }
    Procedure ReadDefault;
    { @exclude }
    procedure UpdateColorControls;
    { @exclude }
    procedure GetColorControls;
  public
    { Public declarations }
    { @exclude }
    constructor Create(AOwner: TDSVideoWindowEx2);  virtual;
    { Restore the colorcontrols to there (Default) values.
      Default is the value the colorcontrol hat, just after we initilized the overlay Mixer. }
    procedure RestoreDefault;
  published
    { The Brightness property defines the luminance intensity, in IRE units, multiplied by 100.
      The possible range is from 0 to 10,000 with a default of 750.}
    property Brightness : Integer read GetBrightness write SetBrightness;

    { The Contrast property defines the relative difference between higher and lower luminance values, in IRE units, multiplied by 100.
      The possible range is from 0 to 20,000 with a default value of 10,000. }
    property Contrast : Integer read GetContrast write SetContrast;

    { The Hue property defines the phase relationship, in degrees, of the chrominance components.
      The possible range is from -180 to 180, with a default of 0.}
    property Hue : Integer read GetHue write SetHue;

    { The Saturation property defines the color intensity, in IRE units, multiplied by 100.
      The possible range is 0 to 20,000, with a default value of 10,000.}
    property Saturation : Integer read GetSaturation write SetSaturation;

    { The Sharpness property defines the sharpness, in arbitrary units, of an image.
      The possible range is 0 to 10, with a default value of 5.}
    property Sharpness : Integer read GetSharpness write SetSharpness;

    { The Gamma property defines the amount, in gamma units, of gamma correction applied to the luminance values.
      The possible range is from 1 to 500, with a default of 1.}
    property Gamma : Integer read GetGamma write SetGamma;

    { The ColorEnable property defines whether color is utilized or not.
      Color is used if this property is 1. Color is not used if this property is 0. The default value is 1.}
    property ColorEnable : Boolean read GetUtilColor write SetUtilColor;
  end;

// *****************************************************************************
//  TDSVideoWindowEx2Caps
// *****************************************************************************

  { Check capability of DSVideoWindowEx. }
  TDSVideoWindowEx2Caps = class(TPersistent)
  protected
    { Protected declarations }
    Owner : TDSVideoWindowEx2;
    function GetCanOverlay : Boolean;
    function GetCanControlBrigtness : Boolean;
    function GetCanControlContrast : Boolean;
    function GetCanControlHue : Boolean;
    function GetCanControlSaturation : Boolean;
    function GetCanControlSharpness : Boolean;
    function GetCanControlGamma : Boolean;
    function GetCanControlUtilizedColor : Boolean;
  public
    { Public declarations }
    { @exclude }
    constructor Create(AOwner: TDSVideoWindowEx2);  virtual;
  published
    { if CanOverlayGraphics return true, you draw on DSVideoWindowEx's canvas and the
      graphic will bee ontop of the Video.}
    property CanOverlayGraphic : Boolean read GetCanOverlay;

    { Repport if you can control Brightness on the video overlay }
    property CanControlBrigtness : Boolean read GetCanControlBrigtness;
    { Repport if you can control Contrast on the video overlay }
    property CanControlContrast : Boolean read GetCanControlContrast;
    { Repport if you can control Hue on the video overlay }
    property CanControlHue : Boolean read GetCanControlHue;
    { Repport if you can control Saturation on the video overlay }
    property CanControlSaturation : Boolean read GetCanControlSaturation;
    { Repport if you can control Sharpness on the video overlay }
    property CanControlSharpness : Boolean read GetCanControlSharpness;
    { Repport if you can control Gamma on the video overlay }
    property CanControlGamma : Boolean read GetCanControlGamma;
    { Repport if you can control ColorEnabled on the video overlay }
    property CanControlColorEnabled : Boolean read GetCanControlUtilizedColor;
  end;

// *****************************************************************************
//  TOverlayCallback
// *****************************************************************************

  { @exclude }
  TOverlayCallback = class(TInterfacedObject, IDDrawExclModeVideoCallBack)
    AOwner : TObject;
    constructor Create(Owner : TObject); virtual;
    function OnUpdateOverlay(bBefore: BOOL; dwFlags: DWORD; bOldVisible: BOOL;
             var prcOldSrc, prcOldDest: TRECT; bNewVisible: BOOL; var prcNewSrc, prcNewDest: TRECT): HRESULT; stdcall;
    function OnUpdateColorKey(var pKey: TCOLORKEY; dwColor: DWORD): HRESULT; stdcall;
    function OnUpdateSize(dwWidth, dwHeight, dwARWidth, dwARHeight: DWORD): HRESULT; stdcall;
  end;

// *****************************************************************************
//  TDSVideoWindowEx2
// *****************************************************************************

  { @exclude }
  TRatioModes  = (rmStretched, rmLetterBox, rmCrop);

  { @exclude }
  TOverlayVisibleEvent = procedure (Sender: TObject; Visible : Boolean) of object;

  { @exclude }
  TCursorVisibleEvent = procedure (Sender: TObject; Visible : Boolean) of object;

  { A alternative to the regular Video Renderer (TVideoWindow), that give a easy way to overlay graphics
    onto your video in your application. }
  TDSVideoWindowEx2 = class(TCustomControl, IFilter, IEvent)
  private
    FVideoWindow       : IVideoWindow;
    FFilterGraph       : TFilterGraph;
    FBaseFilter        : IBaseFilter;
    FOverlayMixer      : IBaseFilter;
    FVideoRenderer     : IBaseFilter;
    FDDXM              : IDDrawExclModeVideo;
    FFullScreen        : Boolean;
    FTopMost           : Boolean;
    FColorKey          : TColor;
    FWindowStyle       : LongWord;
    FWindowStyleEx     : LongWord;
    FVideoRect         : TRect;
    FOnPaint           : TNotifyEvent;
    FOnColorKey        : TNotifyEvent;
    FOnCursorVisible   : TCursorVisibleEvent;
    FOnOverlay         : TOverlayVisibleEvent;
    FColorControl      : TColorControl;
    FCaps              : TDSVideoWindowEx2Caps;
    FZoom              : Integer;
    FAspectMode        : TRatioModes;
    FNoScreenSaver     : Boolean;
    FIdleCursor        : Integer;
    FMonitor           : TMonitor;
    FFullscreenControl : TForm;
    GraphWasUpdatet    : Boolean;
    FOldParent         : TWinControl;
    OverlayCallback    : TOverlayCallback;
    GraphBuildOK       : Boolean;
    FVideoWindowHandle : HWND;
    LMousePos          : TPoint;
    LCursorMov         : DWord;
    RememberCursor     : TCursor;
    IsHidden           : Bool;
    FOverlayVisible    : Boolean;
    OldDesktopColor    : Longint;
    OldDesktopPic      : String;
    FDesktopPlay       : Boolean;
    procedure NotifyFilter(operation: TFilterOperation; Param: integer = 0);
    procedure GraphEvent(Event, Param1, Param2: integer);
    function GetName: string;
    procedure ControlEvent(Event: TControlEvent; Param: integer = 0);
    procedure SetFilterGraph(AFilterGraph: TFilterGraph);
    procedure SetTopMost(TopMost: boolean);
    procedure SetZoom(Value : Integer);
    function UpdateGraph : HResult;
    function GetVideoInfo : HResult;
    procedure SetAspectMode(Value : TRatioModes);
    procedure FullScreenCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SetVideoZOrder;
  protected
    FIsVideoWindowOwner: Boolean;
    {@exclude}
    function GetFilter: IBaseFilter;
    {@exclude}
    procedure resize; override;
    {@exclude}
    procedure Loaded; override;
    {@exclude}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    {@exclude}
    procedure WndProc(var Message: TMessage); override;
    {@exclude}
    procedure Paint; override;
    {@exclude}
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    {@exclude}
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    {@exclude}
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    {@exclude}
    procedure MyIdleHandler(Sender: TObject; var Done: Boolean);
    {@exclude}
    procedure RefreshVideoWindow;
  public
    { constructor method. }
    constructor Create(AOwner: TComponent); override;
    { destructor method. }
    destructor Destroy; override;

    {@exclude}
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;

    { Clear the graphic ontop of DSVideoWindowEx. }
    procedure ClearBack;

    { Use your Desktop as the Video renderer.
      The video will display as a "wallpaper" on your Desktop }
    procedure StartDesktopPlayback; overload;

    { Use your Desktop as the Video renderer.
      The video will display as a "wallpaper" on your Desktop on the
      specifyed monitor}
    procedure StartDesktopPlayBack(OnMonitor : TMonitor); overload;

    { Return to normal window playback from Fullscreen or Desktop mode. }
    procedure NormalPlayback;

    { Start playback in fullscreen }
    procedure StartFullScreen; overload;

    { Start playback in fullscreen on specifyed Monitor}
    procedure StartFullScreen(OnMonitor : TMonitor); overload;

    { repporting if you are currently playing in fullscreen. }
    property FullScreen: boolean read FFullScreen;

    { repporting if you are currently playing on the Desktop. }
    property DesktopPlayback : Boolean Read FDesktopPlay;
    { @inherited }
    property Canvas;

    { The Colorkey is the color that the Overlay Mixer Filter used by DSVideoWindowEx sees
      as transparent, when you draw ontop of the movie always set the canvas’s brush
      color to this color or set the style to bsclear.
      Note: The colors returned through this method vary depending on the current display mode.
      if the colors are 8-bit palettized, they will be bright system colors (such as magenta).
      if the display is in a true-color mode, they will be shades of black. }
    property ColorKey : TColor read FColorKey;

    { @link(TDSVideoWindowEx2Caps) }
    property Capabilities : TDSVideoWindowEx2Caps read FCaps;

    { Check this property to see if the overlay is visible when you draw on DSVideoWindowEx's
      canvas, if it is visible you should set your brush color to the same as the VideoColor and
      if not set your brush to the same color as DSVideoWindowEx color. }
    property OverlayVisible : Boolean read FOverlayVisible;
    property IsVideoWindowOwner: Boolean read FIsVideoWindowOwner write FIsVideoWindowOwner;
  published
    { The AspectRatio property sets the aspect ratio correction mode for window resizing.
        rmSTRETCHED : No aspect ratio correction.
        rmLETTERBOX : Put the video in letterbox format. Paint background color in the
                      excess region  so the video is not distorted.
        rmCROP      : Crop the video to the correct aspect ratio. }
    property AspectRatio : TRatioModes read FAspectMode write SetAspectMode;

    { Set the amounts of milliseconds befor the cursor is hidden, if it is not moved.
      Setting the value to 0 will disable this feature. }
    property AutoHideCursor : Integer read FIdleCursor write FIdleCursor;

    { Specify a Zoom factor from 0 to 99 procent. }
    property DigitalZoom : Integer read FZoom write SetZoom;

    { The @link(TFilterGraph) component }
    property FilterGraph: TFilterGraph read FFilterGraph write SetFilterGraph;

    { Select if the VideoWindow it topmost or not. }
    property FullScreenTopMost: boolean read FTopMost write SetTopMost default false;

    { Event to tell the main application that the Colorkey has changed.
      Note: if you have controls placed ontop of your VideoWindow that need to act as
            transparent, set there color to the same as the Colorkey.}
    property OnColorKeyChanged: TNotifyEvent read FOnColorKey write FOnColorKey;

    { @link(TColorControl) }
    property ColorControl : TColorControl read FColorControl write FColorControl;

    { Setting this to true will prevent the screen to go into screensaver or powerdown. }
    property NoScreenSaver : Boolean read FNoScreenSaver write FNoScreenSaver;

    { This event accure when the Visible state of the overlay changes
      Note: Most used to hide the video in the player window when going to
            DesktopPlayback. }
    property OnOverlayVisible : TOverlayVisibleEvent read FOnOverlay write FOnOverlay;

    property OnPaint : TNotifyevent read FOnPaint Write FOnPaint;

    { This event accure when the cursor change from showing to hiding or from hiding to showing. }
    property OnCursorShowHide : TCursorVisibleEvent read FOnCursorVisible write FOnCursorVisible;

    property Color;                {@exclude}
    property Visible;              {@exclude}
    property ShowHint;             {@exclude}
    property Anchors;              {@exclude}
    property PopupMenu;            {@exclude}
    property Align;                {@exclude}
    property TabStop default True; {@exclude}
    property OnEnter;              {@exclude}
    property OnExit;               {@exclude}
    property OnKeyDown;            {@exclude}
    property OnKeyPress;           {@exclude}
    property OnKeyUp;              {@exclude}
    property OnCanResize;          {@exclude}
    property OnClick;              {@exclude}
    property OnConstrainedResize;  {@exclude}
    property OnDblClick;           {@exclude}
    property OnMouseDown;          {@exclude}
    property OnMouseMove;          {@exclude}
    property OnMouseUp;            {@exclude}
    property OnMouseWheel;         {@exclude}
    property OnMouseWheelDown;     {@exclude}
    property OnMouseWheelUp;       {@exclude}
    property OnResize;
  end;

////////////////////////////////////////////////////////////////////////////////
//
// TVMRBitmap Class
//
////////////////////////////////////////////////////////////////////////////////
type

  { vmrbDisable: Disable the alpha bitmap.
    vmrbSrcColorKey: Enable ColorKey.
    vmrbSrcRect: Indicates that the Dest property is valid and specifies
    a sub-rectangle of the original image to be blended. }

  TVMRBitmapOption = (
    vmrbDisable,
    vmrbSrcColorKey,
    vmrbSrcRect
  );
  TVMRBitmapOptions = set of TVMRBitmapOption;

  TVMRBitmap = class
  private
    FVideoWindow: TVideoWindow;
    FCanvas: TCanvas;
    FVMRALPHABITMAP: TVMR9ALPHABITMAP;
    FOptions: TVMRBitmapOptions;
    FBMPOld: HBITMAP;
    procedure SetOptions(Options: TVMRBitmapOptions);
    procedure ResetBitmap;
    procedure SetAlpha(const Value: Single);
    procedure SetColorKey(const Value: COLORREF);
    procedure SetDest(const Value: TVMR9NormalizedRect);
    procedure SetDestBottom(const Value: Single);
    procedure SetDestLeft(const Value: Single);
    procedure SetDestRight(const Value: Single);
    procedure SetDestTop(const Value: Single);
    procedure SetSource(const Value: TRect);
    function GetAlpha: Single;
    function GetColorKey: COLORREF;
    function GetDest: TVMR9NormalizedRect;
    function GetDestBottom: Single;
    function GetDestLeft: Single;
    function GetDestRight: Single;
    function GetDestTop: Single;
    function GetSource: TRect;
  public
    // Contructor, set the video Window where the bitmat must be paint.
    constructor Create(VideoWindow: TVideoWindow);
    // Cleanup
    destructor Destroy; override;
    // Load a Bitmap from a TBitmap class.
    procedure LoadBitmap(Bitmap: TBitmap);
    // Initialize with an empty bitmap.
    procedure LoadEmptyBitmap(Width, Height: Integer; PixelFormat: TPixelFormat; Color: TColor);
    // Draw the bitmap to the Video Window.
    procedure Draw;
    // Draw the bitmap on a particular position.
    procedure DrawTo(Left, Top, Right, Bottom, Alpha: Single; doUpdate: boolean = false);
    // update the video window with the current bitmap
    procedure Update;
    // Uses this property to draw on the internal bitmap.
    property Canvas: TCanvas read FCanvas write FCanvas;
    // Change Alpha Blending
    property Alpha: Single read GetAlpha write SetAlpha;
    // set the source rectangle
    property Source: TRect read GetSource write SetSource;
    // Destination Left
    property DestLeft   : Single read GetDestLeft write SetDestLeft;
    // Destination Top
    property DestTop    : Single read GetDestTop write SetDestTop;
    // Destination Right
    property DestRight  : Single read GetDestRight write SetDestRight;
    // Destination Bottom
    property DestBottom : Single read GetDestBottom write SetDestBottom;
    // Destination
    property Dest: TVMR9NormalizedRect read GetDest write SetDest;
    // Set the color key for transparency.
    property ColorKey: COLORREF read GetColorKey write SetColorKey;
    // VMR Bitmap Options.
    property Options: TVMRBitmapOptions read FOptions write SetOptions;
  end;

implementation

uses ComObj;

const
  CLSID_FilterGraphCallback: TGUID = '{C7CAA944-C191-4AB1-ABA7-D8B40EF4D5B2}';

// *****************************************************************************
//  TFilterGraph
// *****************************************************************************


  constructor TFilterGraph.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    FHandle := AllocateHWnd(WndProc);
    FVolume := 10000;
    FBalance := 0;
    FRate  := 1.0;
    FLinearVolume := True;
  end;

  destructor TFilterGraph.Destroy;
  begin
    SetActive(False);
    DeallocateHWnd(FHandle);
    inherited Destroy;
  end;

  procedure TFilterGraph.SetGraphMode(Mode: TGraphMode);
  var WasActive: boolean;
  begin
    if FMode = Mode then exit;
    WasActive := Active;
    Active := False;
    FMode := Mode;
    Active := WasActive;
  end;

  procedure TFilterGraph.SetActive(Activate: boolean);
  var
    obj: IObjectWithSite;
    fgcb: IAMFilterGraphCallback;
    gbcb: IAMGraphBuilderCallback;
  const
    IID_IObjectWithSite: TGuid = '{FC4801A3-2BA9-11CF-A229-00AA003D7352}';
  begin
    if Activate = FActive then exit;
    case Activate of
      true :
        begin
          case FMode of
            gmNormal : CoCreateInstance(CLSID_FilterGraph, nil, CLSCTX_INPROC_SERVER, IID_IFilterGraph2, FFilterGraph);
            gmCapture: begin
                         CoCreateInstance(CLSID_CaptureGraphBuilder2, nil, CLSCTX_INPROC_SERVER, IID_ICaptureGraphBuilder2, FCaptureGraph);
                         CoCreateInstance(CLSID_FilterGraph, nil, CLSCTX_INPROC_SERVER, IID_IFilterGraph2, FFilterGraph);
                         FCaptureGraph.SetFiltergraph(IGraphBuilder(FFilterGraph));
                       end;
            gmDVD    : begin
                         CoCreateInstance(CLSID_DvdGraphBuilder, nil, CLSCTX_INPROC_SERVER, IID_IDvdGraphBuilder, FDvdGraph);
                         FDvdGraph.GetFiltergraph(IGraphBuilder(FFilterGraph));
                       end;
          end;
          FActive := true;
          // Events
          if Succeeded(QueryInterface(IMediaEventEx, FMediaEventEx)) then
          begin
            FMediaEventEx.SetNotifyFlags(0); // enable events notification
            FMediaEventEx.SetNotifyWindow(FHandle,WM_GRAPHNOTIFY,ULONG(FMediaEventEx));
          end;

          // Callbacks
          if Succeeded(QueryInterface(IID_IObjectWithSite,obj)) then
          begin
            QueryInterface(IID_IAMGraphBuilderCallback,gbcb);
            if Assigned(gbcb) then
            begin
              obj.SetSite(gbcb);
              gbcb := nil;
            end;
            QueryInterface(IID_IAMFilterGraphCallback,fgcb);
            if Assigned(fgcb) then
            begin
              obj.SetSite(fgcb);
              fgcb := nil;
            end;
            obj := nil;
          end;

          // Remote Object Table
          GraphEdit := FGraphEdit; // Add the Filter Graph to the ROT if needed.
          // Log File
          SetLogFile(FLogFileName);
          // Load Filters
          AddOwnFilters;
          // Notify Controlers
          if assigned(FOnActivate) then FOnActivate(self);
          ControlEvents(ceActive, 1);
        end;
      false:
        begin
          ControlEvents(ceActive, 0);
          ClearOwnFilters;
          if FMediaEventEx <> nil then
          begin
            FMediaEventEx.SetNotifyFlags(AM_MEDIAEVENT_NONOTIFY); // disable events notification
            FMediaEventEx := nil;
          end;
          if FGraphEditID <> 0 then
          begin
            RemoveGraphFromRot(FGraphEditID);
            FGraphEditID := 0;
          end;
          FFilterGraph.SetLogFile(0);
          if Assigned(FLogFile) then FreeAndNil(FLogFile);

          FFilterGraph  := nil;
          FCaptureGraph := nil;
          FDVDGraph     := nil;
          FActive := false;
        end;
    end;
  end;

  procedure TFilterGraph.Loaded;
  begin
    if AutoCreate and (not (csDesigning in ComponentState)) then SetActive(True);
    inherited Loaded;
  end;

  procedure TFilterGraph.WndProc(var Msg: TMessage);
  begin
  with Msg do
    if Msg = WM_GRAPHNOTIFY then
      try
        HandleEvents;
      except
        Application.HandleException(Self);
      end
    else
      Result := DefWindowProc(FHandle, Msg, wParam, lParam);
  end;

  procedure TFilterGraph.HandleEvents;
  var hr: HRESULT;
      Event, Param1, Param2: Integer;
  begin
    if assigned(FMediaEventEx) then
    begin
      hr := FMediaEventEx.GetEvent(Event, Param1, Param2, 0);
      while (hr = S_OK) do
      begin
        DoEvent(Event, Param1, Param2);
        FMediaEventEx.FreeEventParams(Event, Param1, Param2);
        hr := FMediaEventEx.GetEvent(Event, Param1, Param2, 0);
      end;
    end;
  end;

  procedure TFilterGraph.DoEvent(Event, Param1, Param2: Integer);
  type
    TVideoSize = record
      Width : WORD;
      Height: WORD;
    end;
  var
    lcid    : cardinal;
    achLang : array[0..MAX_PATH] of Char;
    tc      : TDVDTimeCode;
    frate   : integer;
    hmsftc  : TDVDHMSFTimeCode;
    DVDInfo2: IDVDInfo2;
  begin
    GraphEvents(Event, Param1, Param2);
    if assigned(FOnDSEvent) then FOnDSEvent(self, Event, Param1, Param2);
    case Event of
      EC_BUFFERING_DATA            : if assigned(FOnGraphBufferingData)           then FOnGraphBufferingData(self,(Param1 = 1));
      EC_CLOCK_CHANGED             : if assigned(FOnGraphClockChanged)            then FOnGraphClockChanged(self);
      EC_COMPLETE                  : if assigned(FOnGraphComplete)                then FOnGraphComplete(self, Param1, IBaseFilter(Param2));
      EC_DEVICE_LOST               : if assigned(FOnGraphDeviceLost)              then FOnGraphDeviceLost(self,IUnKnown(Param1),(Param2 = 1));
      EC_END_OF_SEGMENT            : if assigned(FOnGraphEndOfSegment)            then FOnGraphEndOfSegment(self, PReferenceTime(Param1)^, Param2);
      EC_ERROR_STILLPLAYING        : if assigned(FOnGraphErrorStillPlaying)       then FOnGraphErrorStillPlaying(self, Param1);
      EC_ERRORABORT                : if assigned(FOnGraphErrorAbort)              then FOnGraphErrorAbort(self, Param1);
      EC_FULLSCREEN_LOST           : if assigned(FOnGraphFullscreenLost)          then FOnGraphFullscreenLost(self, IBaseFilter(Param2));
      EC_GRAPH_CHANGED             : if assigned(FOnGraphChanged)                 then FOnGraphChanged(self);
      EC_OLE_EVENT                 : if assigned(FOnGraphOleEvent)                then FOnGraphOleEvent(self, WideString(Param1), WideString(Param2));
      EC_OPENING_FILE              : if assigned(FOnGraphOpeningFile)             then FOnGraphOpeningFile(self, (Param1 = 1));
      EC_PALETTE_CHANGED           : if assigned(FOnGraphPaletteChanged)          then FOnGraphPaletteChanged(self);
      EC_PAUSED                    : if assigned(FOnGraphPaused)                  then FOnGraphPaused(self, Param1);
      EC_QUALITY_CHANGE            : if assigned(FOnGraphQualityChange)           then FOnGraphQualityChange(self);
      EC_SNDDEV_IN_ERROR           : if assigned(FOnGraphSNDDevInError)           then FOnGraphSNDDevInError(self, TSndDevErr(Param1), Param2);
      EC_SNDDEV_OUT_ERROR          : if assigned(FOnGraphSNDDevOutError)          then FOnGraphSNDDevOutError(self, TSndDevErr(Param1), Param2);
      EC_STEP_COMPLETE             : if assigned(FOnGraphStepComplete)            then FOnGraphStepComplete(self);
      EC_STREAM_CONTROL_STARTED    : if assigned(FOnGraphStreamControlStarted)    then FOnGraphStreamControlStarted(self, IPin(Param1), Param2);
      EC_STREAM_CONTROL_STOPPED    : if assigned(FOnGraphStreamControlStopped)    then FOnGraphStreamControlStopped(self, IPin(Param1), Param2);
      EC_STREAM_ERROR_STILLPLAYING : if assigned(FOnGraphStreamErrorStillPlaying) then FOnGraphStreamErrorStillPlaying(self, Param1, Param2);
      EC_STREAM_ERROR_STOPPED      : if assigned(FOnGraphStreamErrorStopped)      then FOnGraphStreamErrorStopped(self, Param1, Param2);
      EC_USERABORT                 : if assigned(FOnGraphUserAbort)               then FOnGraphUserAbort(self);
      EC_VIDEO_SIZE_CHANGED        : if assigned(FOnGraphVideoSizeChanged)        then FOnGraphVideoSizeChanged(self, TVideoSize(Param1).Width, TVideoSize(Param1).Height);
      EC_TIMECODE_AVAILABLE        : if assigned(FOnGraphTimeCodeAvailable)       then FOnGraphTimeCodeAvailable(self,IBaseFilter(Param1), Param2);
      EC_EXTDEVICE_MODE_CHANGE     : if assigned(FOnGraphEXTDeviceModeChange)     then FOnGraphEXTDeviceModeChange(self, Param1, Param2);
      EC_CLOCK_UNSET               : if assigned(FOnGraphClockUnset)              then FOnGraphClockUnset(self);
      EC_VMR_RENDERDEVICE_SET      : if assigned(FOnGraphVMRRenderDevice)         then FOnGraphVMRRenderDevice(self, TVMRRenderDevice(Param1)) ;

      EC_DVD_ANGLE_CHANGE            : if Assigned(FOnDVDAngleChange) then FOnDVDAngleChange(self,Param1,Param2);
      EC_DVD_AUDIO_STREAM_CHANGE     :
        begin
          if Assigned(FOnDVDAudioStreamChange) then
          if Succeeded(QueryInterFace(IDVDInfo2,DVDInfo2)) then
          begin
            CheckDSError(DvdInfo2.GetAudioLanguage(Param1, lcid));
            GetLocaleInfo(lcid, LOCALE_SENGLANGUAGE, achLang, MAX_PATH);
            FOnDVDAudioStreamChange(self, Param1, lcid, string(achLang));
            DVDInfo2 := nil;
          end;
        end;
      EC_DVD_BUTTON_CHANGE           : if Assigned(FOnDVDButtonChange) then FOnDVDButtonChange(self, Param1, Param2);
      EC_DVD_CHAPTER_AUTOSTOP        : if Assigned(FOnDVDChapterAutoStop) then FOnDVDChapterAutoStop(self);
      EC_DVD_CHAPTER_START           : if Assigned(FOnDVDChapterStart) then FOnDVDChapterStart(self, Param1);
      EC_DVD_CURRENT_TIME            :
        begin
          if Assigned(FOnDVDCurrentTime) then
          begin
            tc := IntToTimeCode(Param1);
            case tc.FrameRateCode of
              1 : frate := 25;
              3 : frate := 30;
            else
              frate := 0;
            end;
            FOnDVDCurrentTime(self,tc.Hours1+tc.Hours10*10,tc.Minutes1+tc.Minutes10*10,tc.Seconds1+tc.Seconds10*10,tc.Frames1+tc.Frames10*10,frate);
          end;
        end;
      EC_DVD_DOMAIN_CHANGE           :
        begin
          case Param1 of
            1 : if Assigned(FOnDVDDomainFirstPlay) then FOnDVDDomainFirstPlay(self);
            2 : if Assigned(FOnDVDDomainVideoManagerMenu) then FOnDVDDomainVideoManagerMenu(self);
            3 : if Assigned(FOnDVDDomainVideoTitleSetMenu) then FOnDVDDomainVideoTitleSetMenu(self);
            4 : if Assigned(FOnDVDDomainTitle) then FOnDVDDomainTitle(self);
            5 : if Assigned(FOnDVDDomainStop) then FOnDVDDomainStop(self);
          end;
        end;
      EC_DVD_ERROR                   :
        begin
          case Param1 of
            1 : if Assigned(FOnDVDErrorUnexpected) then FOnDVDErrorUnexpected(self);
            2 : if Assigned(FOnDVDErrorCopyProtectFail) then FOnDVDErrorCopyProtectFail(self);
            3 : if Assigned(FOnDVDErrorInvalidDVD1_0Disc) then FOnDVDErrorInvalidDVD1_0Disc(self);
            4 : if Assigned(FOnDVDErrorInvalidDiscRegion) then FOnDVDErrorInvalidDiscRegion(self);
            5 : if Assigned(FOnDVDErrorLowParentalLevel) then FOnDVDErrorLowParentalLevel(self);
            6 : if Assigned(FOnDVDErrorMacrovisionFail) then FOnDVDErrorMacrovisionFail(self);
            7 : if Assigned(FOnDVDErrorIncompatibleSystemAndDecoderRegions) then FOnDVDErrorIncompatibleSystemAndDecoderRegions(self);
            8 : if Assigned(FOnDVDErrorIncompatibleDiscAndDecoderRegions) then FOnDVDErrorIncompatibleDiscAndDecoderRegions(self);
          end;
        end;
      EC_DVD_NO_FP_PGC               : if Assigned(FOnDVDNoFP_PGC) then FOnDVDNoFP_PGC(self);
      EC_DVD_STILL_OFF               : if Assigned(FOnDVDStillOff) then FOnDVDStillOff(self);
      EC_DVD_STILL_ON                : if Assigned(FOnDVDStillOn) then FOnDVDStillOn(self,(Param1 = 1), Param2);
      EC_DVD_SUBPICTURE_STREAM_CHANGE:
        begin
          if Assigned(FOnDVDSubpictureStreamChange) and Succeeded(QueryInterFace(IDVDInfo2,DVDInfo2)) then
          begin
            DvdInfo2.GetSubpictureLanguage(Param1,lcid);
            GetLocaleInfo(lcid,LOCALE_SENGLANGUAGE,achLang,MAX_PATH);
            FOnDVDSubpictureStreamChange(self,Param1,lcid,string(achLang));
            DVDInfo2 := nil;
          end;
        end;
      EC_DVD_TITLE_CHANGE            : if Assigned(FOnDVDTitleChange) then FOnDVDTitleChange(self,Param1);
      EC_DVD_VALID_UOPS_CHANGE       : if Assigned(FOnDVDValidUOPSChange) then FOnDVDValidUOPSChange(self, Param1);
      EC_DVD_WARNING                 :
        begin
          case Param1 of
            1 : if Assigned(FOnDVDWarningInvalidDVD1_0Disc)  then FOnDVDWarningInvalidDVD1_0Disc(self);
            2 : if Assigned(FOnDVDWarningFormatNotSupported) then FOnDVDWarningFormatNotSupported(self);
            3 : if Assigned(FOnDVDWarningIllegalNavCommand)  then FOnDVDWarningIllegalNavCommand(self);
            4 : if Assigned(FOnDVDWarningOpen)  then FOnDVDWarningOpen(self);
            5 : if Assigned(FOnDVDWarningSeek)  then FOnDVDWarningSeek(self);
            6 : if Assigned(FOnDVDWarningRead)  then FOnDVDWarningRead(self);
          end;
        end;
      EC_DVD_PLAYBACK_RATE_CHANGE    : if Assigned(FOnDVDPlaybackRateChange) then FOnDVDPlaybackRateChange(self, Param1/10000);
      EC_DVD_PARENTAL_LEVEL_CHANGE   : if Assigned(FOnDVDParentalLevelChange) then FOnDVDParentalLevelChange(self,Param1);
      EC_DVD_PLAYBACK_STOPPED        : if Assigned(FOnDVDPlaybackStopped) then FOnDVDPlaybackStopped(self);
      EC_DVD_ANGLES_AVAILABLE        : if Assigned(FOnDVDAnglesAvailable) then FOnDVDAnglesAvailable(self,(Param1 = 1));
      EC_DVD_PLAYPERIOD_AUTOSTOP     : if Assigned(FOnDVDPlayPeriodAutoStop) then FOnDVDPlayPeriodAutoStop(self);
      EC_DVD_BUTTON_AUTO_ACTIVATED   : if Assigned(FOnDVDButtonAutoActivated) then FOnDVDButtonAutoActivated(self,Param1);
      EC_DVD_CMD_START               : if Assigned(FOnDVDCMDStart) then FOnDVDCMDStart(self,Param1);
      EC_DVD_CMD_END                 : if Assigned(FOnDVDCMDEnd) then FOnDVDCMDEnd(self,Param1);
      EC_DVD_DISC_EJECTED            : if Assigned(FOnDVDDiscEjected) then FOnDVDDiscEjected(self);
      EC_DVD_DISC_INSERTED           : if Assigned(FOnDVDDiscInserted) then FOnDVDDiscInserted(self);
      EC_DVD_CURRENT_HMSF_TIME       :
        begin
          if assigned(FOnDVDCurrentHMSFTime) then
            begin
              hmsftc := TDVDHMSFTimeCode(param1);
              tc := IntToTimeCode(Param2);
              FOnDVDCurrentHMSFTime(self,hmsftc,tc);
            end;
        end;
      EC_DVD_KARAOKE_MODE            : if assigned(FOnDVDKaraokeMode) then FOnDVDKaraokeMode(self,BOOL(Param1));
    end;
  end;

  function TFilterGraph.QueryInterface(const IID: TGUID; out Obj): HResult;
  begin
    result := inherited QueryInterface(IID, Obj);
    if (not Succeeded(result)) and Active then
    case FMode of
      gmNormal  : result := FFilterGraph.QueryInterface(IID, Obj);
      gmCapture : begin
                    result := FCaptureGraph.QueryInterface(IID, Obj);
                    if not Succeeded(result) then result := FFilterGraph.QueryInterface(IID, Obj);
                  end;
      gmDVD     : begin
                    result := FDvdGraph.QueryInterface(IID, Obj);
                    if not Succeeded(result) then result := FDvdGraph.GetDvdInterface(IID, Obj);
                    if not Succeeded(result) then result := FFilterGraph.QueryInterface(IID, Obj);
                  end;
    end;
  end;

  procedure TFilterGraph.SetGraphEdit(enable: boolean);
  begin
    case enable of
      true  :
        begin
          if FGraphEditID = 0 then
            if Active then
              AddGraphToRot(IFilterGraph2(FFilterGraph) , FGraphEditID);
        end;
      false :
        begin
          if FGraphEditID <> 0 then
          begin
            RemoveGraphFromRot(FGraphEditID);
            FGraphEditID := 0;
          end;
        end;
    end;
    FGraphEdit := enable;
  end;

  procedure TFilterGraph.InsertFilter(AFilter: IFilter);
  var FilterName: WideString;
  begin
    if FFilters = nil then FFilters := TInterfaceList.Create;
    FFilters.Add(AFilter);
    if active then
      begin
        AFilter.NotifyFilter(foAdding);
        FilterName := AFilter.GetName;
        FFilterGraph.AddFilter(AFilter.GetFilter, PWideChar(FilterName));
        AFilter.NotifyFilter(foAdded);
      end;
  end;

  procedure TFilterGraph.RemoveFilter(AFilter: IFilter);
  begin
    FFilters.Remove(AFilter);
    if active then
      begin
        AFilter.NotifyFilter(foRemoving);
        FFilterGraph.RemoveFilter(AFilter.GetFilter);
        AFilter.NotifyFilter(foRemoved);
      end;
    if FFilters.Count = 0 then
       FreeAndNil(FFilters);
  end;

  procedure TFilterGraph.InsertEventNotifier(AEvent: IEvent);
  begin
    if FGraphEvents = nil then FGraphEvents := TInterFaceList.Create;
    FGraphEvents.Add(AEvent);
  end;

  procedure TFilterGraph.RemoveEventNotifier(AEvent: IEvent);
  begin
    if FGraphEvents <> nil then
    begin
      FGraphEvents.Remove(AEvent);
      if FGraphEvents.Count = 0 then FreeAndNil(FGraphEvents);
    end;
  end;

  procedure TFilterGraph.ClearOwnFilters;
  var i: integer;
  begin
    if Active and (FFilters <> nil) then
      for i := 0 to FFilters.Count - 1 do
        begin
          IFilter(FFilters.Items[i]).NotifyFilter(foRemoving);
          FFilterGraph.RemoveFilter(IFilter(FFilters.Items[i]).GetFilter);
          IFilter(FFilters.Items[i]).NotifyFilter(foRemoved);
        end;
  end;

  procedure TFilterGraph.AddOwnFilters;
  var
    i: integer;
    FilterName: WideString;
  begin
    if Active and (FFilters <> nil) then
      for i := 0 to FFilters.Count - 1 do
        begin
          IFilter(FFilters.Items[i]).NotifyFilter(foAdding);
          FilterName := IFilter(FFilters.Items[i]).GetName;
          FFilterGraph.AddFilter(IFilter(FFilters.Items[i]).GetFilter, PWideChar(FilterName));
          IFilter(FFilters.Items[i]).NotifyFilter(foAdded);
        end;
  end;

{
  procedure TFilterGraph.NotifyFilters(operation: TFilterOperation; Param: integer);
  var i: integer;
  begin
    if FFilters <> nil then
      for i := 0 to FFilters.Count - 1 do
        IFilter(FFilters.Items[i]).NotifyFilter(operation, Param);

  end;
}

  procedure TFilterGraph.GraphEvents(Event, Param1, Param2: integer);
  var i: integer;
  begin
    if FGraphEvents <> nil then
      for i := 0 to FGraphEvents.Count - 1 do
         IEvent(FGraphEvents.Items[i]).GraphEvent(Event, Param1, Param2);
  end;

  procedure TFilterGraph.ControlEvents(Event: TControlEvent; Param: integer = 0);
  var i: integer;
  begin
    if FGraphEvents <> nil then
      for i := 0 to FGraphEvents.Count - 1 do
         IEvent(FGraphEvents.Items[i]).ControlEvent(Event, param);
  end;

  function TFilterGraph.Play: boolean;
  var MediaControl: IMediaControl;
  begin
    result := false;
    if Succeeded(QueryInterface(IMediaControl, MediaControl)) then
    begin
      ControlEvents(cePlay);
      result := Succeeded((CheckDSError(MediaControl.Run)));
      MediaControl := nil;
    end;
  end;

  function TFilterGraph.Pause: boolean;
  var MediaControl: IMediaControl;
  begin
    result := false;
    if Succeeded(QueryInterface(IMediaControl, MediaControl)) then
    begin
      ControlEvents(cePause);
      result := (CheckDSError(MediaControl.Pause) = S_OK);
      MediaControl := nil;
    end;
  end;

  function TFilterGraph.Stop: boolean;
  var MediaControl: IMediaControl;
  begin
    result := false;
    if Succeeded(QueryInterface(IMediaControl, MediaControl)) then
    begin
      ControlEvents(ceStop);
      result := (CheckDSError(MediaControl.Stop) = S_OK);
      MediaControl := nil;
    end;
  end;

  procedure TFilterGraph.SetLogFile(FileName: String);
  begin
    if Active then
    begin
      FFilterGraph.SetLogFile(0);
      if Assigned(FLogFile) then FreeAndNil(FLogFile);
      if FileName <> '' then
      try

        FLogFile := TFileStream.Create(FileName, fmCreate{$IFDEF VER140}, fmShareDenyNone{$ENDIF});

        FFilterGraph.SetLogFile(FLogFile.Handle);
      except
        FFilterGraph.SetLogFile(0);
        if Assigned(FLogFile) then FreeAndNil(FLogFile);
        exit;
      end;
    end;
    FLogFileName := FileName;
  end;

  procedure TFilterGraph.DisconnectFilters;
  var
    FilterList: TFilterList;
    PinList: TPinList;
    BaseFilter: IBaseFilter;
    i, j: integer;
  begin
    if assigned(FFilterGraph) then
    begin
      FilterList:= TFilterList.Create(FFilterGraph);
      if FilterList.Count > 0 then
        for i := 0 to FilterList.Count - 1 do
          begin
            BaseFilter := FilterList.Items[i] as IBaseFilter;
            PinList := TPinList.Create(BaseFilter);
            if PinList.Count > 0 then
            for j := 0 to PinList.Count - 1 do
              CheckDSError(IPin(PinList.Items[j]).Disconnect);
            PinList.Free;
            BaseFilter := nil;
          end;
      FilterList.Free;
    end;
  end;

  procedure TFilterGraph.ClearGraph;
  var
    i: integer;
    FilterList: TFilterList;
  begin
    if Assigned(FFilterGraph) then
    begin
      Stop;
      DisconnectFilters;
      FilterList:= TFilterList.Create(FFilterGraph);
      if assigned(FFilters) then
        if FFilters.Count > 0 then
          for i := 0 to FFilters.count - 1 do
            FilterList.Remove(IFilter(FFilters.Items[i]).GetFilter);
      if FilterList.count > 0 then
      for i := 0 to FilterList.Count - 1 do
        CheckDSError(FFilterGraph.RemoveFilter(FilterList.Items[i]));
      FilterList.Free;
    end;
  end;

  function TFilterGraph.GetState: TGraphState;
  var
    AState: TFilterState;
    MediaControl: IMediaControl;
  begin
    result := gsUninitialized;
    if Succeeded(QueryInterface(IMediaControl, MediaControl)) then
    begin
      MediaControl.GetState(0,AState);
      case AState of
        State_Stopped : result := gsStopped;
        State_Paused  : result := gsPaused;
        State_Running : result := gsPlaying;
      end;
      MediaControl := nil;
    end;
  end;

  procedure TFilterGraph.SetState(Value: TGraphState);
  var
    MediaControl: IMediaControl;
    hr: HResult;
  begin
    if Succeeded(QueryInterface(IMediaControl, MediaControl)) then
    begin
      case Value of
        gsStopped: hr := MediaControl.Stop;
        gsPaused : hr := MediaControl.Pause;
        gsPlaying: hr := MediaControl.Run;
      else
        hr := S_OK;
      end;
      MediaControl := nil;
      CheckDSError(hr);
    end;
  end;

  procedure TFilterGraph.SetVolume(Volume: Integer);
  var
    BasicAudio: IBasicAudio;
  begin
    FVolume := EnsureRange(Volume,0,10000);
    if Succeeded(QueryInterface(IBasicAudio, BasicAudio)) then
    begin
      if FLinearVolume
        then BasicAudio.put_Volume(SetBasicAudioVolume(FVolume))
        else BasicAudio.put_Volume(FVolume-10000);
      BasicAudio := nil;
    end;
  end;

  procedure TFilterGraph.SetBalance(Balance: integer);
  var BasicAudio: IBasicAudio;
  begin
    FBalance := EnsureRange(Balance,-10000,10000);
    if Succeeded(QueryInterface(IBasicAudio, BasicAudio)) then
    begin
      if FLinearVolume
        then BasicAudio.put_Balance(SetBasicAudioPan(FBalance))
        else BasicAudio.put_Balance(FBalance);
      BasicAudio := nil;
    end;
  end;

  function TFilterGraph.GetSeekCaps: TSeekingCaps;
  var
    MediaSeeking: IMediaSeeking;
    Flags: Cardinal;
  begin
    result := [];
    if Succeeded(QueryInterface(IMediaSeeking, MediaSeeking)) then
    begin
      MediaSeeking.GetCapabilities(Flags);
      PByte(@Result)^ := Flags;
      MediaSeeking := nil;
    end;
  end;

  function TFilterGraph.RenderFile(FileName: WideString): HRESULT;
  begin
    result := S_FALSE;
    if assigned(FFilterGraph) then
    begin
      ControlEvents(ceFileRendering);
      result := CheckDSError(FFilterGraph.RenderFile(PWideChar(FileName), nil));
      if Succeeded(result) then
      begin
        UpdateGraph;
        ControlEvents(ceFileRendered);
      end;
    end;
  end;

  { TODO -oHG : Add the audio rendering }
  function TFilterGraph.RenderFileEx(FileName: WideString): HRESULT;
  var
    SourceFilter: IBaseFilter;
    PinList: TPinList;
    i: Integer;
  begin
    result := S_FALSE;
    if assigned(FFilterGraph) then
    begin
      ControlEvents(ceFileRendering);
      CheckDSError(FFilterGraph.AddSourceFilter(PWideChar(FileName), PWideChar(FileName), SourceFilter));
      PinList := TPinList.Create(SourceFilter);
      try
        for i := 0 to PinList.Count - 1 do
        begin
          CheckDSError(IFilterGraph2(FFilterGraph).RenderEx(PinList.Items[i],
            AM_RENDEREX_RENDERTOEXISTINGRENDERERS, nil));
        end;
      finally
        PinList.Free;
      end;
      if Succeeded(result) then
      begin
        ControlEvents(ceFileRendered);
        UpdateGraph;
      end;
    end;
  end;

  function TFilterGraph.RenderDVD(out status: TAMDVDRenderStatus;
    FileName: WideString = ''; Mode: Integer = AM_DVD_HWDEC_PREFER): HRESULT;
  begin
    result := HRESULT(VFW_E_DVD_RENDERFAIL);
    if assigned(FDVDGraph) then
    begin
      ControlEvents(ceDVDRendering, Mode);
      if FileName <> '' then
        result := CheckDSError(FDVDGraph.RenderDvdVideoVolume(PWideChar(FileName), Mode, Status))
      else
        result := CheckDSError(FDVDGraph.RenderDvdVideoVolume(nil, Mode, Status));
      if result in [S_OK..S_FALSE] then
      begin
        ControlEvents(ceDVDRendered, Mode);
        UpdateGraph;
      end;
    end;
  end;

  procedure TFilterGraph.SetRate(Rate: double);
  var MediaSeeking: IMediaSeeking;
  begin
    FRate := Rate;
    if Succeeded(QueryInterface(IMediaSeeking, MediaSeeking)) then
    begin
      MediaSeeking.SetRate(FRate);
      MediaSeeking := nil;
    end;
  end;

  function TFilterGraph.GetDuration: integer;
  var
    MediaSeeking: IMediaSeeking;
    RefTime: int64;
  begin
    if Succeeded(QueryInterface(IMediaSeeking, MediaSeeking)) then
    begin
      MediaSeeking.GetDuration(RefTime);
      result := RefTimeToMiliSec(RefTime);
      MediaSeeking := nil;
    end
    else
      result := 0;
  end;

  procedure TFilterGraph.DVDSaveBookmark(BookMarkFile: WideString);
  var
    DVDInfo2: IDVDInfo2;
    Bookmark: IDvdState;
    pStorage: IStorage;
    pStream : IStream;
    PersistStream : IPersistStream;
  begin
    if Active and (Mode = gmDVD) then
    if Succeeded(QueryInterface(IDVDInfo2, DVDInfo2)) then
    begin
      DVDInfo2.GetState(Bookmark);
      StgCreateDocfile(PWideChar(BookMarkFile), STGM_CREATE or STGM_WRITE or STGM_SHARE_EXCLUSIVE, 0, pStorage);
      pStorage.CreateStream('BookMark', STGM_CREATE or STGM_WRITE or STGM_SHARE_EXCLUSIVE, 0, 0, pStream);
      if Succeeded(Bookmark.QueryInterface(IID_IPersistStream,PersistStream)) then
        begin
          OleSaveToStream(PersistStream,pStream);
          PersistStream := nil;
       end
       else
       begin
         PersistStream := nil;
         DVDInfo2      := nil;
         exit;
       end;
     DVDInfo2 := nil;
    end;
  end;

  procedure TFilterGraph.DVDRestoreBookmark(BookMarkFile: WideString);
  var
    DVDControl2: IDvdControl2;
    pStorage : IStorage;
    pStream  : IStream;
    pBookmark: IDvdState;
    hr       : HRESULT;
    obj      : IDVDCmd;
  begin
    if Succeeded(QueryInterface(IDvdControl2, DvdControl2)) then
    begin
      StgOpenStorage(PWideChar(BookMarkFile), nil, STGM_READ or STGM_SHARE_EXCLUSIVE, nil , 0, pStorage);
      pStorage.OpenStream('BookMark', nil, STGM_READ or STGM_SHARE_EXCLUSIVE, 0, pStream);
      OleLoadFromStream(pStream, IID_IDvdState, pBookmark);
      hr := CheckDSError(DVDControl2.SetState(pBookmark, DVD_CMD_FLAG_None, obj));
      if not (failed(hr)) then
        begin
          obj.WaitForEnd;
          obj := nil;
        end;
      DvdControl2 := nil;
    end;
  end;

  procedure TFilterGraph.SetLinearVolume(aEnabled: Boolean);
  begin
    if FLinearVolume = aEnabled then Exit;
    FLinearVolume := aEnabled;
    SetVolume(FVolume);
    SetBalance(FBalance);
  end;

  procedure TFilterGraph.UpdateGraph;
  begin
    SetVolume(FVolume);
    SetBalance(FBalance);
    SetRate(FRate);
  end;

  function TFilterGraph.SelectedFilter(pMon: IMoniker): HResult; stdcall;
  var
    PropBag: IPropertyBag;
    Name: OleVariant;
    vGuid: OleVariant;
    Guid: TGUID;
  begin
    if Assigned(FOnSelectedFilter) then
    begin
      pMon.BindToStorage(nil, nil, IID_IPropertyBag, PropBag);
      if PropBag.Read('CLSID',vGuid,nil) = S_OK then Guid := StringToGUID(vGuid)
                                                else Guid := GUID_NULL;
      if PropBag.Read('FriendlyName', Name, nil) <> S_OK then Name := '';

      PropBag := nil;

      if FOnSelectedFilter(pMon,Name,Guid)
        then Result := S_OK
        else Result := E_FAIL;
    end else
      Result := S_OK;
  end;

  function TFilterGraph.CreatedFilter(pFil: IBaseFilter): HResult; stdcall;
  var
    guid: TGuid;
  begin
    if Assigned(FOnCreatedFilter) then
    begin
      pfil.GetClassID(guid);
      if FOnCreatedFilter(pFil,guid)
        then Result := S_OK
        else Result := E_FAIL;
    end else
      Result := S_OK;
  end;

  function TFilterGraph.UnableToRender(ph1, ph2: integer; pPin: IPin): HResult;
  var
    graph: TFilterGraph;
    PinInfo: TPinInfo;
    FilterInfo: TFilterInfo;
    serviceProvider: IServiceProvider;
  begin
    Result := S_FALSE;

    if (pPin.QueryPinInfo(PinInfo) = S_OK) and
       (Assigned(PinInfo.pFilter)) and
       (PinInfo.pFilter.QueryFilterInfo(FilterInfo) = S_OK) and
       (Assigned(FilterInfo.pGraph)) and
       (FilterInfo.pGraph.QueryInterface(IServiceProvider, serviceProvider) = S_OK) and
       (serviceProvider.QueryService(CLSID_FilterGraphCallback, CLSID_FilterGraphCallback, graph) = S_OK) and
       (Assigned(graph) and Assigned(graph.FOnUnableToRender)) and
       (graph.FOnUnableToRender(pPin))
       then Result := S_OK;

    PinInfo.pFilter := nil;
    FilterInfo.pGraph := nil;
    serviceProvider := nil;
  end;

  function TFilterGraph.QueryService(const rsid, iid: TGuid; out Obj): HResult;
  begin
    if IsEqualGUID(CLSID_FilterGraphCallback, rsid) and
       IsEqualGUID(CLSID_FilterGraphCallback, iid) then
    begin
      Pointer(Obj) := Pointer(Self);
      Result := S_OK;
    end else
      Result := E_NOINTERFACE;
  end;

//******************************************************************************
// TVMROptions
//******************************************************************************

  constructor TVMROptions.Create(AOwner: TVideoWindow);
  begin
    FPreferences := [vpForceMixer];
    FStreams := 4;
    FOwner   := AOwner;
    FMode    := vmrWindowed;
    FKeepAspectRatio := True;
  end;

  procedure TVMROptions.SetStreams(Streams: cardinal);
  begin
    if Streams in [1..16] then FStreams := Streams else FStreams := 1;
    with FOwner do
    begin
      if (mode <> vmVMR) or (FilterGraph = nil) then exit;
      if not FilterGraph.Active then exit;
      // need to reconnect
      FilterGraph.RemoveFilter(FOwner);
      FilterGraph.InsertFilter(FOwner);
    end;
  end;

  procedure TVMROptions.SetPreferences(Preferences: TVMRPreferences);
  begin
    FPreferences := Preferences;
    with FOwner do
    begin
      if (mode <> vmVMR) or (FilterGraph = nil) then exit;
      if not FilterGraph.Active then exit;
      // need to reconnect
      FilterGraph.RemoveFilter(FOwner);
      FilterGraph.InsertFilter(FOwner);
    end;
  end;

  procedure TVMROptions.SetMode(AMode: TVMRVideoMode);
  begin
    FMode := AMode;
    with FOwner do
    begin
      if (mode <> vmVMR) or (FilterGraph = nil) then exit;
      if not FilterGraph.Active then exit;
      // need to reconnect
      FilterGraph.RemoveFilter(FOwner);
      FilterGraph.InsertFilter(FOwner);
    end;
  end;

  procedure TVMROptions.SetKeepAspectRatio(Keep: boolean);
  var AspectRatioControl: IVMRAspectRatioControl9;
  begin
    FKeepAspectRatio := Keep;
    case Mode of
      vmrWindowed, vmrWindowless:
        begin
          if Succeeded(FOwner.QueryInterface(IVMRAspectRatioControl9, AspectRatioControl)) then
          case Keep of
            true: CheckDSError(AspectRatioControl.SetAspectRatioMode(VMR_ARMODE_LETTER_BOX));
            false: CheckDSError(AspectRatioControl.SetAspectRatioMode(VMR_ARMODE_NONE));
          end;

        end;
      vmrRenderless: {TODO};
    end;
  end;


//******************************************************************************
// TVideoWindow
//******************************************************************************

  constructor TVideoWindow.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    FVMROptions:= TVMROptions.Create(self);
    ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csDoubleClicks, csReflector];
    TabStop   := true;
    Height    := 120;
    Width     := 160;
    color     := $000000;
    FIsFullScreen := false;
    FKeepAspectRatio := True;
  end;

  destructor TVideoWindow.Destroy;
  begin
    FVMROptions.Free;
    FilterGraph := nil;
    inherited destroy;
  end;

  procedure TVideoWindow.SetVideoMode(AMode: TVideoMode);
  begin
    if (AMode = vmVMR) and (not CheckVMR)
      then FMode := vmNormal
      else FMode := AMode;
    if FilterGraph = nil then exit;
    if not FilterGraph.Active then exit;
    // need to reconnect
    FilterGraph.RemoveFilter(self);
    FilterGraph.InsertFilter(self);
  end;

  procedure TVideoWindow.Loaded;
  begin
    inherited Loaded;
    FWindowStyle   := GetWindowLong(Handle, GWL_STYLE);
    FWindowStyleEx := GetWindowLong(Handle, GWL_EXSTYLE);
  end;

  procedure TVideoWindow.Notification(AComponent: TComponent;
    Operation: TOperation);
  begin
    inherited Notification(AComponent, Operation);
    if ((AComponent = FFilterGraph) and (Operation = opRemove)) then
        FFilterGraph := nil;
  end;

  procedure TVideoWindow.SetFilterGraph(AFilterGraph: TFilterGraph);
  begin
    if AFilterGraph = FFilterGraph then exit;
    if FFilterGraph <> nil then
    begin
      FFilterGraph.RemoveFilter(self);
      FFilterGraph.RemoveEventNotifier(self);
    end;
    if AFilterGraph <> nil then
    begin
      AFilterGraph.InsertFilter(self);
      AFilterGraph.InsertEventNotifier(self);
    end;
    FFilterGraph := AFilterGraph;
  end;

  function TVideoWindow.GetFilter: IBaseFilter;
  begin
    result := FBaseFilter;
  end;

  function TVideoWindow.GetName: string;
  begin
    result := name;
  end;

  procedure TVideoWindow.NotifyFilter(operation: TFilterOperation; Param: integer);
  var
    EnumPins: TPinList;
    VMRFilterConfig: IVMRFilterConfig9;
    VMRSurfaceAllocatorNotify: IVMRSurfaceAllocatorNotify9;
    VMRSurfaceAllocator: IVMRSurfaceAllocator9;
    MyPrefs: TVMRPreferences;
    APrefs: cardinal;
    i: integer;
    CW: Word;
    hr: HResult;
    DSPackException: EDSPackException;

    procedure UpdatePreferences;
    begin
      // VMR9 preferences
      MyPrefs := FVMROptions.FPreferences - [vpForceMixer];
      CheckDSError(VMRFilterConfig.SetRenderingPrefs(PByte(@MyPrefs)^));
      APrefs := 0;
      CheckDSError(VMRFilterConfig.GetRenderingPrefs(APrefs));
      if (vpForceMixer in FVMROptions.FPreferences) then
        FVMROptions.FPreferences := PVMRPreferences(@APrefs)^ + [vpForceMixer]
      else
        FVMROptions.FPreferences := PVMRPreferences(@APrefs)^;
    end;
  begin
    case operation of
      foAdding:
        begin
          case mode of
            vmVMR    :
              begin
                CW := Get8087CW;
                try
                  CoCreateInstance(CLSID_VideoMixingRenderer9, nil, CLSCTX_INPROC, IID_IBaseFilter ,FBaseFilter);
                  FBaseFilter.QueryInterface(IVMRFilterConfig9, VMRFilterConfig);
                  case FVMROptions.Mode of
                  vmrWindowed: CheckDSError(VMRFilterConfig.SetRenderingMode(VMR9Mode_Windowed));
                  vmrWindowless: CheckDSError(VMRFilterConfig.SetRenderingMode(VMR9Mode_Windowless));
                  vmrRenderless:
                    begin
                      if (FAllocatorClass = nil) then
                        raise EDSPackException.Create('Allocator class not set.');

                      FCurrentAllocator := FAllocatorClass.Create(hr, Handle);
                      if failed(hr) then
                      begin
                        DSPackException := EDSPackException.Create('Error Creating Allocator');
                        DSPackException.ErrorCode := hr;
                        raise DSPackException;
                      end;

                      CheckDSError(VMRFilterConfig.SetRenderingMode(VMR9Mode_Renderless));
                      CheckDSError(FBaseFilter.QueryInterface(IID_IVMRSurfaceAllocatorNotify9, VMRSurfaceAllocatorNotify));
                      CheckDSError(FCurrentAllocator.QueryInterface(IID_IVMRSurfaceAllocator9, VMRSurfaceAllocator));

                      VMRSurfaceAllocatorNotify.AdviseSurfaceAllocator(FRenderLessUserID, VMRSurfaceAllocator);
                      VMRSurfaceAllocator._AddRef; // manual increment;
                      VMRSurfaceAllocator.AdviseNotify(VMRSurfaceAllocatorNotify);
                    end;
                  end;
                  VMRFilterConfig := nil;
                finally
                  Set8087CW(CW);
                end;
              end;
            vmNormal : CoCreateInstance(CLSID_VideoRenderer, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter ,FBaseFilter);
          end;
        end;
      foAdded:
        begin
          case mode of
            vmVMR:
              begin
                if (FBaseFilter <> nil) then
                  if CheckDSError(FBaseFilter.QueryInterface(IVMRFilterConfig9, VMRFilterConfig)) = S_OK then
                  begin
                    if (FVMROptions.FStreams <> 4)
                      or (vpForceMixer in FVMROptions.FPreferences) then
                    begin
                      CheckDSError(VMRFilterConfig.SetNumberOfStreams(FVMROptions.FStreams));
                      CheckDSError(VMRFilterConfig.GetNumberOfStreams(FVMROptions.FStreams));
                    end;


                    case FVMROptions.Mode of
                      vmrWindowed   :
                        begin

                          CheckDSError(FBaseFilter.QueryInterface(IVideoWindow, FVideoWindow));
                          UpdatePreferences;
                        end;
                      vmrWindowless :
                        begin

                          CheckDSError(FBaseFilter.QueryInterface(IVMRWindowlessControl9, FWindowLess));
                          CheckDSError(FWindowLess.SetVideoClippingWindow(Handle));
                          UpdatePreferences;
                          Resize;
                        end;
                      vmrRenderless :
                        begin
                          //Assert(False, 'not yet imlemented.');
                          //CheckDSError(FBaseFilter.QueryInterface(IVMRWindowlessControl9, FWindowLess));
                          //CheckDSError(FWindowLess.SetVideoClippingWindow(Handle));
                        end;

                    end;
                    VMRFilterConfig := nil;
                    VMROptions.SetKeepAspectRatio(VMROptions.FKeepAspectRatio);
                  end;
              end;
            vmNormal: CheckDSError(FBaseFilter.QueryInterface(IVideoWindow, FVideoWindow));
          end;
        end;
      foRemoving:
        if FBaseFilter <> nil then
          begin
            // it's important to stop and disconnect the filter before removing the VMR filter.
            CheckDSError(FBaseFilter.Stop);
            EnumPins := TPinList.Create(FBaseFilter);
            if EnumPins.Count > 0 then
              for i := 0 to EnumPins.Count - 1 do
                CheckDSError(EnumPins.Items[i].Disconnect);
            EnumPins.Free;
            if (FCurrentAllocator <> nil) and (mode = vmVMR) and (VMROptions.Mode = vmrRenderless) then
            begin
              IUnKnown(FCurrentAllocator)._Release;
              FCurrentAllocator := nil;
            end;
          end;
      foRemoved :
        begin
          FVideoWindow     := nil;
          FWindowLess      := nil;
          FBaseFilter      := nil;
        end;
    end;
  end;

  procedure TVideoWindow.Paint;
  begin
    inherited Paint;
    if Assigned(FOnPaint) then FOnPaint(self);
  end;

  procedure TVideoWindow.Resize;
  var ARect: TRect;
  begin
    inherited Resize;
    case FMode of
      vmNormal:
        begin
          if (FVideoWindow <> nil) and (not FullScreen) then
            if FIsVideoWindowOwner then
              FVideoWindow.SetWindowPosition(0, 0, Width, Height)
            else
              FVideoWindow.SetWindowPosition(Left, Top, Width, Height);
        end;
      vmVMR:
        case FVMROptions.Mode of
          vmrWindowed:
            begin
              if (FVideoWindow <> nil) and (not FullScreen) then
                if FIsVideoWindowOwner then
                  FVideoWindow.SetWindowPosition(0, 0, Width, Height)
                else
                  FVideoWindow.SetWindowPosition(Left, Top, Width, Height);
            end;
          vmrWindowless:
            if FWindowLess <> nil then
            begin
              ARect := Rect(0,0, width, height);
              FWindowLess.SetVideoPosition(nil, @ARect);
            end;
        end;
    end;

  end;

  procedure TVideoWindow.ConstrainedResize(var MinWidth, MinHeight, MaxWidth, MaxHeight: Integer);
  begin
    inherited ConstrainedResize(MinWidth, MinHeight, MaxWidth, MaxHeight);
    Resize;
  end;

  function TVideoWindow.GetVideoHandle: THandle;
  begin
    if FVideoWindow <> nil then
      result := FindWindowEx(Parent.Handle,0,Pchar('VideoRenderer'), Pchar(name))
    else
      Result := Canvas.Handle;
  end;

  class function TVideoWindow.CheckVMR: boolean;
  var
    AFilter: IBaseFilter;
    CW: Word;
  begin
    CW := Get8087CW;
    try
    result := (CoCreateInstance(CLSID_VideoMixingRenderer9, nil, CLSCTX_INPROC, IID_IBaseFilter ,AFilter) = S_OK);
    finally
      Set8087CW(CW);
      AFilter := nil;
    end;
  end;

  procedure TVideoWindow.SetFullScreen(Value: boolean);
  var
    StyleEX: LongWord;
  begin
    if (FVideoWindow <> nil) and CheckInputPinsConnected then
      case Value of
        true:
          begin
            CheckDSError(FVideoWindow.put_Owner(0));
            CheckDSError(FVideoWindow.put_WindowStyle(FWindowStyle and not(WS_BORDER or WS_CAPTION or WS_THICKFRAME)));
            StyleEX := FWindowStyleEx and not(WS_EX_CLIENTEDGE or WS_EX_STATICEDGE
              or WS_EX_WINDOWEDGE or WS_EX_DLGMODALFRAME);
            if FTopMost then StyleEX := StyleEX or WS_EX_TOPMOST;
            CheckDSError(FVideoWindow.put_WindowStyleEx(StyleEX));
            CheckDSError(FVideoWindow.SetWindowPosition(0, 0, Screen.Width, Screen.Height));
            FIsFullScreen := True;
          end;
        false:
          begin
            if FIsVideoWindowOwner then
              CheckDSError(FVideoWindow.put_Owner(Handle))
            else
              CheckDSError(FVideoWindow.put_Owner(Parent.Handle));
            CheckDSError(FVideoWindow.put_WindowStyle(FWindowStyle or WS_CHILD or WS_CLIPSIBLINGS));
            CheckDSError(FVideoWindow.put_WindowStyleEx(FWindowStyleEx));
            if FIsVideoWindowOwner then
              CheckDSError(FVideoWindow.SetWindowPosition(0, 0, Self.Width, Self.Height))
            else
              CheckDSError(FVideoWindow.SetWindowPosition(Self.Left, Self.Top, Self.Width, Self.Height));
            FIsFullScreen := false;
          end;
      end;

    if FWindowLess <> nil then
      FIsFullScreen := false;

    FFullScreen := Value;
  end;


  function TVideoWindow.QueryInterface(const IID: TGUID; out Obj): HResult;
  begin
    if IsEqualGUID(IID_IVMRWindowlessControl9, IID) and (FWindowLess <> nil) then
    begin
      result := S_OK;
      IunKnown(Obj) := FWindowLess;
      exit;
    end;
    result := inherited QueryInterface(IID, Obj);
    if failed(result) and assigned(FBaseFilter) then
      result := FBaseFilter.QueryInterface(IID, Obj);
  end;

  procedure TVideoWindow.GraphEvent(Event, Param1, Param2: integer);
  begin
    case Event of
      EC_PALETTE_CHANGED:
        if FVideoWindow <> nil then
          begin
            SetFullScreen(FFullScreen);
            if Name <> '' then
              CheckDSError(FVideoWindow.put_Caption(Name));
            CheckDSError(FVideoWindow.put_MessageDrain(Handle));
          end;
      EC_VMR_RENDERDEVICE_SET:
        begin
          if (FVMROptions.FMode = vmrWindowed) and (FVideoWindow <> nil) then
          begin
            if Name <> '' then
              CheckDSError(FVideoWindow.put_Caption(Name));
            CheckDSError(FVideoWindow.put_MessageDrain(Handle));
          end;
        end;
    end;
  end;

  function TVideoWindow.CheckInputPinsConnected: boolean;
  var
    PinList: TPinList;
    i: Integer;
  begin
    result := False;
    if (FBaseFilter = nil) then Exit;
    PinList := TPinList.Create(FBaseFilter);
    try
      for i := 0 to PinList.Count - 1 do
        if PinList.Connected[i] then
        begin
          Result := True;
          Break;
        end;
    finally
      PinList.Free;
    end;
  end;


 procedure TVideoWindow.ControlEvent(Event: TControlEvent; Param: integer = 0);
 var
   FilterInfo: TFilterInfo;
   FilterList: TFilterList;
   i: integer;
   GUID: TGUID;
 begin
   case Event of
      ceDVDRendered: // mean our Video Filter have been removed
        begin
          ZeroMemory(@FilterInfo, SizeOf(TFilterInfo));
          CheckDSError(FBaseFilter.QueryFilterInfo(FilterInfo));
          if not assigned(FilterInfo.pGraph) then
          begin
            FilterList:= TFilterList.Create(FilterGraph.FFilterGraph);
            if FilterList.Count > 0 then
              for i := 0 to FilterList.Count - 1 do
              begin
                FilterList.Items[i].GetClassID(GUID);
                if ISEqualGUID(GUID, CLSID_VideoRenderer) and (Mode = vmNormal) then
                  begin
                    FBaseFilter  := nil;
                    FVideoWindow := nil;
                    FWindowLess  := nil;
                    FBaseFilter := FilterList.Items[i];
                    FBaseFilter.QueryInterface(IVideoWindow, FVideoWindow);
                    break;
                  end;
              end;
          end;
        end;
      cePlay:
      begin
        if CheckInputPinsConnected then
        begin
          case FMode of
            vmNormal:
              if FVideoWindow <> nil then
                begin
                  SetFullScreen(FFullScreen);
                  if Name <> '' then
                    CheckDSError(FVideoWindow.put_Caption(Name));
                  CheckDSError(FVideoWindow.put_MessageDrain(Handle));
                end;
            vmVMR: SetFullScreen(FFullScreen);
          end;
        end;
      end;

   end;
 end;

  procedure TVideoWindow.WndProc(var Message: TMessage);
  begin
    if ((Message.Msg = WM_CONTEXTMENU) and FullScreen) then
      begin
        if assigned(PopupMenu) then
          if PopupMenu.AutoPopup then
          begin
            PopupMenu.Popup(mouse.CursorPos.X, mouse.CursorPos.Y);
            Message.Result := 1;
          end;
      end
    else
      inherited WndProc(Message);
  end;

  procedure TVideoWindow.SetTopMost(TopMost: boolean);
  begin
    FTopMost := TopMost;
    if FFullScreen then SetFullScreen(true);
  end;

  procedure TVideoWindow.MouseDown(Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
  begin
    if FIsFullScreen then
      inherited MouseDown(Button, Shift, mouse.CursorPos.X, mouse.CursorPos.Y)
    else
      inherited MouseDown(Button, Shift, X, Y)
  end;

  procedure TVideoWindow.MouseMove(Shift: TShiftState; X, Y: Integer);
  begin
    if Fisfullscreen then
      inherited MouseMove(Shift, mouse.CursorPos.X, mouse.CursorPos.Y)
    else
      inherited MouseMove(Shift, X, Y)
  end;

  procedure TVideoWindow.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
    if Fisfullscreen then
      inherited MouseUp(Button, Shift, mouse.CursorPos.X, mouse.CursorPos.Y)
    else
      inherited MouseUp(Button, Shift, X, Y)
  end;

  function TVideoWindow.VMRGetBitMap(Stream: TStream): boolean;
  var
    Image: PBitmapInfoHeader;
    BFH: TBITMAPFILEHEADER;
    function DibSize: cardinal; begin result := (Image.biSize + Image.biSizeImage + Image.biClrUsed * sizeof(TRGBQUAD)); end;
    function DibNumColors: cardinal;
    begin if (image.biClrUsed = 0) and (image.biBitCount <= 8) then
          result := 1 shl integer(image.biBitCount) else
          result := image.biClrUsed; end;
    function DibPaletteSize: cardinal; begin result := (DibNumColors * sizeof(TRGBQUAD)) end;
  begin
    assert(assigned(Stream));
    result := false;
    if FWindowLess <> nil then
    if Succeeded(FWindowLess.GetCurrentImage(PByte(image))) then
    begin
      BFH.bfType      := $4d42; // BM
      BFH.bfSize      := DibSize + sizeof(TBITMAPFILEHEADER);
      BFH.bfReserved1 := 0;
      BFH.bfReserved2 := 0;
      BFH.bfOffBits   := sizeof(TBITMAPFILEHEADER) + image.biSize + DibPaletteSize;
      Stream.Write(BFH, SizeOf(TBITMAPFILEHEADER));
      Stream.Write(image^, BFH.bfSize);
      Stream.Position :=0;
      CoTaskMemFree(image);
      result := true;
    end;
  end;

  function TVideoWindow.GetVisible: boolean;
  begin
    result := inherited visible;
  end;

  procedure TVideoWindow.SetVisible(Vis: boolean);
  begin
    inherited Visible := Vis;
    if assigned(FVideoWindow) then CheckDSError(FVideoWindow.put_Visible(vis));
  end;

  procedure TVideoWindow.SetAllocator(Allocator: TAbstractAllocatorClass; UserID: Cardinal);
  begin
    FAllocatorClass := Allocator;
    FRenderLessUserID := UserID;
  end;

// *****************************************************************************
//  TSampleGrabber
// *****************************************************************************

  procedure TSampleGrabber.SetFilterGraph(AFilterGraph: TFilterGraph);
  begin
    if AFilterGraph = FFilterGraph then exit;
    if FFilterGraph <> nil then FFilterGraph.RemoveFilter(self);
    if AFilterGraph <> nil then AFilterGraph.InsertFilter(self);
    FFilterGraph := AFilterGraph;
  end;

  function TSampleGrabber.GetFilter: IBaseFilter;
  begin
    result := FBaseFilter;
  end;

  function TSampleGrabber.GetName: string;
  begin
    result := name;
  end;

  procedure TSampleGrabber.NotifyFilter(operation: TFilterOperation; Param: integer = 0);
  var
    EnumPins: IEnumPins;
  begin
    case operation of
      foAdding    : Cocreateinstance(CLSID_SampleGrabber, nil, CLSCTX_INPROC ,IID_IBASEFilter, FBaseFilter);
      foAdded     :
         begin
           FBaseFilter.QueryInterface(IID_ISampleGrabber,SampleGrabber);
           FBaseFilter.EnumPins(EnumPins);
           EnumPins.Next(1,InPutPin,nil);
           EnumPins.Next(1,OutPutPin,nil);
           EnumPins := nil;
           UpdateMediaType;
           SampleGrabber.SetBufferSamples(true);
           SampleGrabber.SetCallback(Self ,1);
         end;
      foRemoving  :
        begin
          FBaseFilter.Stop;
          InPutPin.Disconnect;
          OutPutPin.Disconnect;
        end;
      foRemoved   :
        begin
          SampleGrabber.SetCallback(nil ,1);
          SampleGrabber.SetBufferSamples(false);
          FBaseFilter   := nil;
          SampleGrabber := nil;
          InPutPin      := nil;
          OutPutPin     := nil;
        end;
      foRefresh: UpdateMediaType;
    end;
  end;

  constructor TSampleGrabber.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    FCriticalSection := TCriticalSection.Create;
    assert(CheckFilter, 'The SampleGrabber Filter is not available on this system.');
    FMediaType := TMediaType.Create(MEDIATYPE_Video);
    FMediaType.SubType := MEDIASUBTYPE_RGB24;
    FMediaType.FormatType := FORMAT_VideoInfo;
    // [pjh, 2003-07-14] BMPInfo local
    // new(BMPInfo);
  end;

  destructor TSampleGrabber.Destroy;
  begin
    FilterGraph := nil;
    FMediaType.Free;
    // [pjh, 2003-07-14] BMPInfo local
    // Dispose(BMPInfo);
    FCriticalSection.Free;
    inherited destroy;
  end;

  class function TSampleGrabber.CheckFilter: boolean;
  var
    AFilter: IBaseFilter;
  begin
    result := Cocreateinstance(CLSID_SampleGrabber, nil, CLSCTX_INPROC ,IID_IBASEFilter, AFilter) = S_OK;
    AFilter := nil;
  end;

  procedure TSampleGrabber.Notification(AComponent: TComponent; Operation: TOperation);
  begin
    inherited Notification(AComponent, Operation);
    if ((AComponent = FFilterGraph) and (Operation = opRemove)) then
        FFilterGraph := nil;
  end;

  procedure TSampleGrabber.UpdateMediaType;
  begin
    if assigned(SampleGrabber) then
    begin
      FBaseFilter.Stop;
      InPutPin.Disconnect;
      SampleGrabber.SetMediaType(MediaType.AMMediaType^);
    end;
  end;

  procedure TSampleGrabber.SetBMPCompatible(Source: PAMMediaType; SetDefault: cardinal);
  var
    SubType : TGUID;
    BitCount: LongWord;
  begin
    BitCount := SetDefault;
      MediaType.ResetFormatBuffer;
      ZeroMemory(MediaType.AMMediaType, sizeof(TAMMediaType));
      MediaType.majortype := MEDIATYPE_Video;
      MediaType.formattype := FORMAT_VideoInfo;
      if Source = nil then
      begin
        case SetDefault of
          0      : MediaType.subtype := MEDIASUBTYPE_RGB24;
          1      : MediaType.subtype := MEDIASUBTYPE_RGB1;
          2 ..4  : MediaType.subtype := MEDIASUBTYPE_RGB4;
          5 ..8  : MediaType.subtype := MEDIASUBTYPE_RGB8;
          9 ..16 : MediaType.subtype := MEDIASUBTYPE_RGB555;
          17..24 : MediaType.subtype := MEDIASUBTYPE_RGB24;
          25..32 : MediaType.subtype := MEDIASUBTYPE_RGB32
        else
          MediaType.subtype := MEDIASUBTYPE_RGB32;
        end;
        UpdateMediaType;
        exit;
      end;

      SubType := Source.subtype;
      if (IsEqualGUID(SubType, MEDIASUBTYPE_RGB1)   or
          IsEqualGUID(SubType, MEDIASUBTYPE_RGB4)   or
          IsEqualGUID(SubType, MEDIASUBTYPE_RGB8)   or
          IsEqualGUID(SubType, MEDIASUBTYPE_RGB555) or
          IsEqualGUID(SubType, MEDIASUBTYPE_RGB24)  or
          IsEqualGUID(SubType, MEDIASUBTYPE_RGB32)) then
            MediaType.subtype := SubType // no change
      else
      begin
        // get bitcount
        if assigned(Source.pbFormat) then
        if IsEqualGUID(Source.formattype, FORMAT_VideoInfo) then
          BitCount := PVideoInfoHeader(Source.pbFormat)^.bmiHeader.biBitCount    else
        if IsEqualGUID(Source.formattype, FORMAT_VideoInfo2) then
          BitCount := PVideoInfoHeader2(Source.pbFormat)^.bmiHeader.biBitCount   else
        if IsEqualGUID(Source.formattype, FORMAT_MPEGVideo) then
          BitCount := PMPEG1VideoInfo(Source.pbFormat)^.hdr.bmiHeader.biBitCount else
        if IsEqualGUID(Source.formattype, FORMAT_MPEG2Video) then
          BitCount := PMPEG2VideoInfo(Source.pbFormat)^.hdr.bmiHeader.biBitCount;
        case BitCount of
          0      : MediaType.subtype := MEDIASUBTYPE_RGB24;
          1      : MediaType.subtype := MEDIASUBTYPE_RGB1;
          2 ..4  : MediaType.subtype := MEDIASUBTYPE_RGB4;
          5 ..8  : MediaType.subtype := MEDIASUBTYPE_RGB8;
          9 ..16 : MediaType.subtype := MEDIASUBTYPE_RGB555;
          17..24 : MediaType.subtype := MEDIASUBTYPE_RGB24;
          25..32 : MediaType.subtype := MEDIASUBTYPE_RGB32
        else
          MediaType.subtype := MEDIASUBTYPE_RGB32;
        end;
      end;
      UpdateMediaType;
  end;

  function GetDIBLineSize(BitCount, Width: Integer): Integer;
  begin
    if BitCount = 15 then
      BitCount := 16;
    Result := ((BitCount * Width + 31) div 32) * 4;
  end;

  // [pjh, 2003-07-17] modified
  // Buffer =  Nil -> Get the data from SampleGrabber
  // Buffer <> Nil -> Copy the DIB from buffer to Bitmap
  function TSampleGrabber.GetBitmap(Bitmap: TBitmap; Buffer: Pointer; BufferLen: Integer): Boolean;
  var
    hr: HRESULT;
    BIHeaderPtr: PBitmapInfoHeader;
    MediaType: TAMMediaType;
    BitmapHandle: HBitmap;
    DIBPtr: Pointer;
    DIBSize: LongInt;
  begin
    Result := False;
    if not Assigned(Bitmap) then
      Exit;
    if Assigned(Buffer) and (BufferLen = 0) then
      Exit;
    hr := SampleGrabber.GetConnectedMediaType(MediaType);
    if hr <> S_OK then
      Exit;
    try
      if IsEqualGUID(MediaType.majortype, MEDIATYPE_Video) then
      begin
        BIHeaderPtr := Nil;
        if IsEqualGUID(MediaType.formattype, FORMAT_VideoInfo) then
        begin
          if MediaType.cbFormat = SizeOf(TVideoInfoHeader) then  // check size
            BIHeaderPtr := @(PVideoInfoHeader(MediaType.pbFormat)^.bmiHeader);
        end
        else if IsEqualGUID(MediaType.formattype, FORMAT_VideoInfo2) then
        begin
          if MediaType.cbFormat = SizeOf(TVideoInfoHeader2) then  // check size
            BIHeaderPtr := @(PVideoInfoHeader2(MediaType.pbFormat)^.bmiHeader);
        end;
        // check, whether format is supported by TSampleGrabber
        if not Assigned(BIHeaderPtr) then
          Exit;
        BitmapHandle := CreateDIBSection(0, PBitmapInfo(BIHeaderPtr)^,
                                         DIB_RGB_COLORS, DIBPtr, 0, 0);
        if BitmapHandle <> 0 then
        begin
          try
            if DIBPtr = Nil then
              Exit;
            // get DIB size
            DIBSize := BIHeaderPtr^.biSizeImage;
            if DIBSize = 0 then
            begin
              with BIHeaderPtr^ do
                DIBSize := GetDIBLineSize(biBitCount, biWidth) * biHeight * biPlanes;
            end;
            // copy DIB
            if not Assigned(Buffer) then
            begin
              // get buffer size
              BufferLen := 0;
              hr := SampleGrabber.GetCurrentBuffer(BufferLen, Nil);
              if (hr <> S_OK) or (BufferLen <= 0) then
                Exit;
              // copy buffer to DIB
              if BufferLen > DIBSize then  // copy Min(BufferLen, DIBSize)
                BufferLen := DIBSize;
              hr := SampleGrabber.GetCurrentBuffer(BufferLen, DIBPtr);
              if hr <> S_OK then
                Exit;
            end
            else
            begin
              if BufferLen > DIBSize then  // copy Min(BufferLen, DIBSize)
                BufferLen := DIBSize;
              Move(Buffer^, DIBPtr^, BufferLen);
            end;
            Bitmap.Handle := BitmapHandle;
            Result := True;
          finally
            if Bitmap.Handle <> BitmapHandle then  // preserve for any changes in Graphics.pas
              DeleteObject(BitmapHandle);
          end;
        end;
      end;
    finally
      FreeMediaType(@MediaType);
    end;
  end;

  function TSampleGrabber.GetBitmap(Bitmap: TBitmap): Boolean;
  begin
    Result := GetBitmap(Bitmap, Nil, 0);
  end;

  function TSampleGrabber.QueryInterface(const IID: TGUID; out Obj): HResult;
  begin
    result := inherited QueryInterface(IID, Obj);
    if failed(result) and assigned(FBaseFilter) then
      result := FBaseFilter.QueryInterface(IID, Obj);
  end;

  function TSampleGrabber.BufferCB(SampleTime: Double; pBuffer: PByte;
    BufferLen: Integer): HResult;
  begin
    if assigned(FOnBuffer) then
    begin
      FCriticalSection.Enter;
      try
        FOnBuffer(self, SampleTime, pBuffer, BufferLen);
      finally
        FCriticalSection.Leave;
      end;
    end;
    result := S_OK;
  end;

  function TSampleGrabber.SampleCB(SampleTime: Double;
    pSample: IMediaSample): HResult;
  begin
    result := S_OK;
  end;
  
// *****************************************************************************
//  TFilter
// *****************************************************************************

  function TFilter.GetFilter: IBaseFilter;
  begin
    result := FFilter;
  end;

  function TFilter.GetName: string;
  begin
    result := name;
  end;

  procedure TFilter.NotifyFilter(operation: TFilterOperation; Param: integer = 0);
  var
    State : TFilterState;
  begin
    case operation of
      foAdding: FFilter := BaseFilter.CreateFilter;
      foRemoving: if (FFilter <> nil) and (FFilter.GetState(0,State) = S_OK) then
                   case State of
                     State_Paused,
                     State_Running: FFilter.Stop;
                   end;
      foRemoved: FFilter := nil;
      foRefresh: if assigned(FFilterGraph) then
                   begin
                     FFilterGraph.RemoveFilter(self);
                     FFilterGraph.InsertFilter(self);
                   end;
    end;
  end;

  constructor TFilter.Create(AOwner: TComponent); 
  begin
    inherited Create(AOwner);
    FBaseFilter := TBaseFilter.Create;
  end;

  destructor TFilter.Destroy;
  begin
    FBaseFilter.Free;
    FilterGraph := nil;
    inherited Destroy;
  end;

  procedure TFilter.SetFilterGraph(AFilterGraph: TFilterGraph);
  begin
    if AFilterGraph = FFilterGraph then exit;
    if FFilterGraph <> nil then FFilterGraph.RemoveFilter(self);
    if AFilterGraph <> nil then AFilterGraph.InsertFilter(self);
    FFilterGraph := AFilterGraph;
  end;

  procedure TFilter.Notification(AComponent: TComponent; Operation: TOperation);
  begin
    inherited Notification(AComponent, Operation);
    if ((AComponent = FFilterGraph) and (Operation = opRemove)) then
        FFilterGraph := nil;
  end;

  function TFilter.QueryInterface(const IID: TGUID; out Obj): HResult; 
  begin
    result := inherited QueryInterface(IID, Obj);
    if not Succeeded(Result) then
      if Assigned(FFilter) then
        result := FFilter.QueryInterface(IID, Obj);
  end;

// *****************************************************************************
//  TASFWriter
// *****************************************************************************

  constructor TASFWriter.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    FAutoIndex := true;
    FMultiPass := False;
    FDontCompress := False;
  end;

  destructor TASFWriter.Destroy;
  begin
    FilterGraph := nil;
    inherited Destroy;
  end;

  procedure TASFWriter.SetFilterGraph(AFilterGraph: TFilterGraph);
  begin
    if AFilterGraph = FFilterGraph then exit;
    if FFilterGraph <> nil then FFilterGraph.RemoveFilter(self);
    if AFilterGraph <> nil then AFilterGraph.InsertFilter(self);
    FFilterGraph := AFilterGraph;
  end;

  function TASFWriter.GetFilter: IBaseFilter;
  begin
    result := FFilter;
  end;

  function TASFWriter.GetName: string;
  begin
    result := name;
  end;

  procedure TASFWriter.NotifyFilter(operation: TFilterOperation; Param: integer = 0);
  var
    PinList: TPinList;
    ServiceProvider: IServiceProvider;
    FAsfConfig: IConfigAsfWriter2;
  begin
    case operation of
      foAdding: cocreateinstance(CLSID_WMAsfWriter, nil, CLSCTX_INPROC ,IBaseFilter, FFilter);
      foAdded : begin
                  if assigned(FFilter) then
                  begin
                    SetProfile(FProfile);
                    SetFileName(FFileName);
                    if Succeeded(FFilter.QueryInterface(IID_IConfigAsfWriter2, FAsfConfig)) then
                    begin
                      FAsfConfig.SetParam(AM_CONFIGASFWRITER_PARAM_AUTOINDEX, Cardinal(FAutoIndex), 0);
                      FAsfConfig.SetParam(AM_CONFIGASFWRITER_PARAM_MULTIPASS, Cardinal(FMultiPass), 0);
                      FAsfConfig.SetParam(AM_CONFIGASFWRITER_PARAM_DONTCOMPRESS, Cardinal(FDontCompress), 0);
                    end;

                    PinList:= TPinList.Create(FFilter);
                    try
                      if PinList.Count >= 1 then
                      begin
                        AudioInput := PinList.Items[0];
                        if PinList.Count = 2 then
                        begin
                          VideoInput := PinList.Items[1];
                          VideoInput.QueryInterface(IID_IAMStreamConfig, VideoStreamConfig);
                        end;
                        AudioInput.QueryInterface(IID_IAMStreamConfig, AudioStreamConfig);
                        if Succeeded(QueryInterface(IServiceProvider, ServiceProvider)) then
                        begin
                          ServiceProvider.QueryService(IID_IWMWriterAdvanced2, IID_IWMWriterAdvanced2, WriterAdvanced2);
                          ServiceProvider := nil;
                        end;
                        if ((FPort > 0) and (FMaxUsers > 0)) then
                        if Succeeded(WMCreateWriterNetworkSink(WriterNetworkSink)) then
                        begin
                          WriterNetworkSink.SetNetworkProtocol(WMT_PROTOCOL_HTTP);
                          WriterNetworkSink.SetMaximumClients(FMaxUsers);
                          WriterNetworkSink.Open(FPort);
                          WriterAdvanced2.AddSink(WriterNetworkSink);
                        end;
                      end;
                    finally
                      PinList.Free;
                    end;

                  end;
                end;
      foRemoving: begin
                    if assigned(FFilter) then FFilter.Stop;
                    if assigned(WriterNetworkSink) then
                    begin
                      WriterNetworkSink.Disconnect;
                      WriterNetworkSink.Close;
                    end;
                    if assigned(AudioInput) then AudioInput.Disconnect;
                    if assigned(VideoInput) then VideoInput.Disconnect;
                  end;

      foRemoved: begin
                   WriterAdvanced2      := nil;
                   WriterNetworkSink    := nil;
                   AudioInput           := nil;
                   VideoInput           := nil;
                   AudioStreamConfig    := nil;
                   VideoStreamConfig    := nil;
                   FFilter              := nil;
                 end;
    end;
  end;

  procedure TASFWriter.Notification(AComponent: TComponent; Operation: TOperation);
  begin
    inherited Notification(AComponent, Operation);
    if ((AComponent = FFilterGraph) and (Operation = opRemove)) then
        FFilterGraph := nil;
  end;

  function TASFWriter.GetProfile: TWMPofiles8;
  var
    GUIDProf: TGUID;
    ConfigAsfWriter: IConfigAsfWriter;
  begin
    if Succeeded(QueryInterface(IConfigAsfWriter, ConfigAsfWriter)) then
    begin
      ConfigAsfWriter.GetCurrentProfileGuid(GUIDProf);
      result := ProfileFromGUID(GUIDProf);
      ConfigAsfWriter := nil;
    end
    else
      result := FProfile
  end;

  procedure TASFWriter.SetProfile(profile: TWMPofiles8);
  var
    ConfigAsfWriter: IConfigAsfWriter;
  begin
    if Succeeded(QueryInterface(IConfigAsfWriter, ConfigAsfWriter)) then
    begin
      ConfigAsfWriter.ConfigureFilterUsingProfileGuid(WMProfiles8[profile]);
      ConfigAsfWriter := nil;
    end
    else
      FProfile := profile;
  end;

  function TASFWriter.GetFileName: String;
  var
    F: PWideChar;
    FileSinkFilter2: IFileSinkFilter2;
  begin
    if Succeeded(QueryInterface(IFileSinkFilter2, FileSinkFilter2)) then
    begin
      FileSinkFilter2.GetCurFile(F,nil);
      FileSinkFilter2 := nil;
      result := F;
    end
    else
      result := FFileName;
  end;

  procedure TASFWriter.SetFileName(FileName: String);
  var FileSinkFilter2: IFileSinkFilter2;
  begin
    FFileName := FileName;
    if Succeeded(QueryInterface(IFileSinkFilter2, FileSinkFilter2)) then
    begin
      FileSinkFilter2.SetFileName(PWideChar(FFileName),nil);
      FileSinkFilter2 := nil;
    end;
  end;

  function TASFWriter.QueryInterface(const IID: TGUID; out Obj): HResult;
  begin
    result := inherited QueryInterface(IID, Obj);
    if failed(result) and assigned(FFilter) then
      result := FFilter.QueryInterface(IID, Obj);
  end;

// *****************************************************************************
//  TDSTrackBar
// *****************************************************************************

  procedure TDSTrackBar.SetFilterGraph(AFilterGraph: TFilterGraph);
  begin
    if AFilterGraph = FFilterGraph then exit;
    if FFilterGraph <> nil then FFilterGraph.RemoveEventNotifier(self);
    if AFilterGraph <> nil then AFilterGraph.InsertEventNotifier(self);
    FFilterGraph := AFilterGraph;
  end;

  constructor TDSTrackBar.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    FMouseDown    := false;
    FEnabled      := false;
    FInterval     := 1000;
    FWindowHandle := AllocateHWnd(TimerWndProc);
  end;

  destructor TDSTrackBar.Destroy;
  begin
    FEnabled := False;
    UpdateTimer;
    FilterGraph := nil;
    DeallocateHWnd(FWindowHandle);
    FMediaSeeking := nil;
    inherited Destroy;
  end;

  procedure TDSTrackBar.Notification(AComponent: TComponent;
    Operation: TOperation);
  begin
    inherited Notification(AComponent, Operation);
    if ((AComponent = FFilterGraph) and (Operation = opRemove)) then
      begin
        FMediaSeeking := nil;
        FFilterGraph  := nil;
      end;
  end;

  procedure TDSTrackBar.GraphEvent(Event, Param1, Param2: integer);
  var
    Duration: int64;
    Zero: int64;
  begin
    case Event of
      EC_CLOCK_CHANGED: if assigned(FMediaSeeking) then
        begin
          Zero := 0;
          FMediaSeeking.GetDuration(Duration);
          FMediaSeeking.SetPositions(Zero, AM_SEEKING_AbsolutePositioning,
                                     Duration   , AM_SEEKING_NoPositioning);
        end;
     end;
  end;

  procedure TDSTrackBar.ControlEvent(Event: TControlEvent; Param: integer = 0);
  begin
    case event of
      cePlay: TimerEnabled := Enabled;
      cePause..ceStop: TimerEnabled := false;
      ceActive: case Param of
                  0: FMediaSeeking := nil;
                  1: FFilterGraph.QueryInterface(IMediaSeeking, FMediaSeeking);
                end;
    end;
  end;

  procedure TDSTrackBar.SetTimerEnabled(Value: Boolean);
  begin
    if Value <> FEnabled then
    begin
      FEnabled := Value;
      UpdateTimer;
    end;
  end;

  procedure TDSTrackBar.SetInterval(Value: Cardinal);
  begin
    if Value <> FInterval then
    begin
      FInterval := Value;
      UpdateTimer;
    end;
  end;

  procedure TDSTrackBar.SetOnTimer(Value: TTimerEvent);
  begin
    FOnTimer := Value;
    UpdateTimer;
  end;

  procedure TDSTrackBar.UpdateTimer;
  begin
    KillTimer(FWindowHandle, 1);
    if (FInterval <> 0) and FEnabled then
      if SetTimer(FWindowHandle, 1, FInterval, nil) = 0 then
        raise EOutOfResources.Create(SNoTimers);
  end;

  procedure TDSTrackBar.Timer;
  var
    CurrentPos, StopPos: int64;
    MlsCurrentPos, MlsStopPos: Cardinal;
  begin
    if assigned(FMediaSeeking) and (not FMouseDown) then
      if Succeeded(FMediaSeeking.GetDuration(StopPos)) then
      if Succeeded(FMediaSeeking.GetCurrentPosition(CurrentPos)) then
      begin
        MlsCurrentPos := RefTimeToMiliSec(CurrentPos);
        MlsStopPos    := RefTimeToMiliSec(StopPos);
        min := 0;
        max := MlsStopPos div TimerInterval;
        Position := MlsCurrentPos div TimerInterval;
        if Assigned(FOnTimer) then FOnTimer(Self, MlsCurrentPos, MlsStopPos);
      end;
  end;

  procedure TDSTrackBar.TimerWndProc(var Msg: TMessage);
  begin
    with Msg do
      if Msg = WM_TIMER then
        try
          Timer;
        except
          Application.HandleException(Self);
        end
      else
        Result := DefWindowProc(FWindowHandle, Msg, wParam, lParam);
  end;

  procedure TDSTrackBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  var
    StopPosition, CurrentPosition: int64;
  begin
    inherited MouseUp(Button, Shift, X, Y);
    if Button = mbLeft then
     if assigned(FMediaSeeking) then
       begin
         FMediaSeeking.GetStopPosition(StopPosition);
         CurrentPosition := (StopPosition * Position) div max ;
         FMediaSeeking.SetPositions(CurrentPosition, AM_SEEKING_AbsolutePositioning,
                                    StopPosition   , AM_SEEKING_NoPositioning);

       end;
    FMouseDown := False;
  end;

  procedure TDSTrackBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
    inherited MouseDown(Button, Shift, X, Y);
    if Button = mbLeft then FMouseDown := true;
  end;

  // --------------------------- Color Control -------------------------------
  constructor TColorControl.Create(AOwner: TDSVideoWindowEx2);
  begin
    inherited Create;
    FOwner := AOwner;
    ZeroMemory(@FDefault,SizeOf(TDDColorControl));
    with FDefault do
    begin
      dwSize := SizeOf(TDDCOLORCONTROL);
      dwFlags := DDCOLOR_BRIGHTNESS or DDCOLOR_CONTRAST or DDCOLOR_HUE
                 or DDCOLOR_SATURATION or DDCOLOR_GAMMA or DDCOLOR_SHARPNESS
                 or DDCOLOR_COLORENABLE;
      lBrightness := 750;
      lContrast := 10000;
      lGamma := 1;
      lHue := 0;
      lSaturation := 10000;
      lSharpness := 5;
      lColorEnable := integer(True);
      dwReserved1 := 0;
    end;
    FBrightness := FDefault.lBrightness;
    FContrast := FDefault.lContrast;
    FGamma := FDefault.lGamma;
    FHue := FDefault.lHue;
    FSaturation := FDefault.lSaturation;
    FSharpness := FDefault.lSharpness;
    FUtilColor := Bool(FDefault.lColorEnable);
  end;

  procedure TColorControl.ReadDefault;
  var
    EnumPins   : IEnumPins;
    Pin        : IPin;
    ul         : cardinal;
    pd         : TPinDirection;
    MPC        : IMixerPinConfig2;
    Tel        : Integer;
    FG         : IFilterGraph;
    FilterList : TFilterList;
    Hr         : HResult;
    OVM        : IBaseFilter;
    FClass     : TGuid;
    Tmp        : TDDColorControl;
  begin
    if (csDesigning in TDSVideoWindowEx2(FOwner).ComponentState) or
       (TDSVideoWindowEx2(FOwner).FFilterGraph = nil) or
       (TDSVideoWindowEx2(FOwner).FFilterGraph.Active = False) then Exit;

    MPC := nil;
    OVM := nil;
    FG  := nil;
    FG := TDSVideoWindowEx2(FOwner).FFilterGraph.FFilterGraph;
    FilterList := TFilterList.Create(FG);
    try
      for Tel := 0 to FilterList.Count -1 do
      begin
        FilterList[Tel].GetClassID(FClass);
        if IsEqualGuid(FClass, CLSID_OverlayMixer) then
          OVM := FilterList[Tel];
        if IsEqualGuid(FClass, CLSID_OverlayMixer2) then
          OVM := FilterList[Tel];
      end;

      if OVM = nil then Exit;
      Hr := OVM.EnumPins(EnumPins);
      if Failed(Hr) then Exit;

      Tel := 0;
      while (EnumPins.Next(1, Pin, @ul) = S_OK) and (ul = 1) and (Tel = 0) do
      begin
        Hr := Pin.QueryDirection(pd);
        if Failed(Hr) then Exit;

        if pd = PINDIR_INPUT then
        begin
          Hr := Pin.QueryInterface(IID_IMixerPinConfig2, MPC);
          if Failed(Hr) then Exit;
          Inc(Tel);
        end;
        Pin := nil;
      end;
      EnumPins := nil;

      ZeroMemory(@Tmp,SizeOf(TDDColorControl));
      Tmp.dwSize:=SizeOf(TDDCOLORCONTROL);

      Hr := MPC.GetOverlaySurfaceColorControls(Tmp);
      if Failed(Hr) then Exit;

      FDefault := Tmp;
    finally
      FilterList.Free;
      FG         := nil;
      OVM        := nil;
      EnumPins  := nil;
      Pin       := nil;
      MPC       := nil;
    end;
  end;

  procedure TColorControl.UpdateColorControls;
  var
    EnumPins   : IEnumPins;
    Pin        : IPin;
    ul         : cardinal;
    pd         : TPinDirection;
    MPC        : IMixerPinConfig2;
    Tel        : Integer;
    FG         : IFilterGraph;
    FilterList : TFilterList;
    Hr         : HResult;
    OVM        : IBaseFilter;
    FClass     : TGuid;
    Tmp        : TDDColorControl;
  begin
    if (csDesigning in TDSVideoWindowEx2(FOwner).ComponentState) or
       (TDSVideoWindowEx2(FOwner).FFilterGraph = nil) or
       (TDSVideoWindowEx2(FOwner).FFilterGraph.Active = False) then Exit;

    MPC := nil;
    OVM  := nil;
    FG   := nil;
    FG := TDSVideoWindowEx2(FOwner).FFilterGraph.FFilterGraph;
    FilterList := TFilterList.Create(FG);
    try
      for Tel := 0 to FilterList.Count -1 do
      begin
        FilterList[Tel].GetClassID(FClass);
        if IsEqualGuid(FClass, CLSID_OverlayMixer) then
          OVM := FilterList[Tel];
        if IsEqualGuid(FClass, CLSID_OverlayMixer2) then
          OVM := FilterList[Tel];
      end;

      if OVM = nil then Exit;
      Hr := OVM.EnumPins(EnumPins);
      if Failed(Hr) then Exit;

      Tel := 0;
      while (EnumPins.Next(1, Pin, @ul) = S_OK) and (ul = 1) and (Tel = 0) do
      begin
        Hr := Pin.QueryDirection(pd);
        if Failed(Hr) then Exit;

        if pd = PINDIR_INPUT then
        begin
          Hr := Pin.QueryInterface(IID_IMixerPinConfig2, MPC);
          if Failed(Hr) then Exit;
          Inc(Tel);
        end;
        Pin := nil;
      end;
      EnumPins := nil;

      Tmp.dwSize := SizeOf(TDDCOLORCONTROL);
      Tmp.dwFlags := DDCOLOR_BRIGHTNESS or DDCOLOR_CONTRAST or DDCOLOR_HUE or DDCOLOR_SATURATION or DDCOLOR_GAMMA or DDCOLOR_SHARPNESS or DDCOLOR_COLORENABLE;
      Tmp.lBrightness := FBrightness;
      Tmp.lContrast := FContrast;
      Tmp.lHue := FHue;
      Tmp.lSaturation := FSaturation;
      Tmp.lSharpness := FSharpness;
      Tmp.lGamma := FGamma;
      Tmp.lColorEnable := integer(FUtilColor);
      Tmp.dwReserved1 := 0;

      Hr := MPC.setOverlaySurfaceColorControls(Pointer(@Tmp));
      if Failed(Hr) then Exit;
    finally
      FilterList.Free;
      FG         := nil;
      OVM        := nil;
      EnumPins  := nil;
      Pin       := nil;
      MPC       := nil;
    end;
  end;

  procedure TColorControl.GetColorControls;
  var
    EnumPins   : IEnumPins;
    Pin        : IPin;
    ul         : cardinal;
    pd         : TPinDirection;
    MPC        : IMixerPinConfig2;
    Tel        : Integer;
    FG         : IFilterGraph;
    FilterList : TFilterList;
    Hr         : HResult;
    OVM        : IBaseFilter;
    FClass     : TGuid;
    Tmp        : TDDColorControl;
  begin
    if (csDesigning in TDSVideoWindowEx2(FOwner).ComponentState) or
       (TDSVideoWindowEx2(FOwner).FFilterGraph = nil) or
       (TDSVideoWindowEx2(FOwner).FFilterGraph.Active = False) then Exit;

    MPC := nil;
    OVM := nil;
    FG  := nil;
    FG := TDSVideoWindowEx2(FOwner).FFilterGraph.FFilterGraph;
    FilterList := TFilterList.Create(FG);
    try
      for Tel := 0 to FilterList.Count -1 do
      begin
        FilterList[Tel].GetClassID(FClass);
        if IsEqualGuid(FClass, CLSID_OverlayMixer) then
          OVM := FilterList[Tel];
        if IsEqualGuid(FClass, CLSID_OverlayMixer2) then
          OVM := FilterList[Tel];
      end;

      if OVM = nil then Exit;
      Hr := OVM.EnumPins(EnumPins);
      if Failed(Hr) then Exit;

      Tel := 0;
      while (EnumPins.Next(1, Pin, @ul) = S_OK) and (ul = 1) and (Tel = 0) do
      begin
        Hr := Pin.QueryDirection(pd);
        if Failed(Hr) then Exit;

        if pd = PINDIR_INPUT then
        begin
          Hr := Pin.QueryInterface(IID_IMixerPinConfig2, MPC);
          if Failed(Hr) then Exit;
          Inc(Tel);
        end;
        Pin := nil;
      end;
      EnumPins := nil;

      ZeroMemory(@Tmp,SizeOf(TDDColorControl));
      Tmp.dwSize := SizeOf(TDDCOLORCONTROL);

      Hr := MPC.GetOverlaySurfaceColorControls(Tmp);
      if Failed(Hr) then
      begin
        FBrightness := 750;
        FContrast := 10000;
        FHue := 0;
        FSaturation := 10000;
        FSharpness := 5;
        FGamma := 1;
        FUtilColor := True;
        Exit;
      end
      else
      begin
        FBrightness := Tmp.lBrightness;
        FContrast := Tmp.lContrast;
        FHue := Tmp.lHue;
        FSaturation := Tmp.lSaturation;
        FSharpness := Tmp.lSharpness;
        FGamma := Tmp.lGamma;
        FUtilColor := Bool(Tmp.lColorEnable);
      end;
    finally
      FilterList.Free;
      FG        := nil;
      OVM       := nil;
      EnumPins  := nil;
      Pin       := nil;
      MPC       := nil;
    end;
  end;

  procedure TColorControl.RestoreDefault;
  begin
    FBrightness := FDefault.lBrightness;
    FContrast := FDefault.lContrast;
    FHue := FDefault.lHue;
    FSaturation := FDefault.lSaturation;
    FSharpness := FDefault.lSharpness;
    FGamma := FDefault.lGamma;
    FUtilColor := Bool(FDefault.lColorEnable);
    if (not (csDesigning in TDSVideoWindowEx2(FOwner).ComponentState)) and
       (TDSVideoWindowEx2(FOwner).FFilterGraph <> nil) and
       (TDSVideoWindowEx2(FOwner).FFilterGraph.Active = True) then
         UpdateColorControls;
  end;

  Procedure TColorControl.SetBrightness(Value : Integer);
  begin
    if (Value > -1) and (Value < 10001) then
    begin
      if Value <> FBrightness then
        FBrightness := Value;
      if (not (csDesigning in TDSVideoWindowEx2(FOwner).ComponentState)) and
         (TDSVideoWindowEx2(FOwner).FFilterGraph <> nil) and
         (TDSVideoWindowEx2(FOwner).FFilterGraph.Active = True) then
           UpdateColorControls;
    end
    else
      raise Exception.CreateFmt('Value %d out of range. Value must bee between 0 -> 10.000', [Value]);
  end;

  Procedure TColorControl.SetContrast(Value : Integer);
  begin
    if (Value > -1) and (Value < 20001) then
    begin
      if Value <> FContrast then
        FContrast := Value;
      if (not (csDesigning in TDSVideoWindowEx2(FOwner).ComponentState)) and
         (TDSVideoWindowEx2(FOwner).FFilterGraph <> nil) and
         (TDSVideoWindowEx2(FOwner).FFilterGraph.Active = True) then
           UpdateColorControls;
    end
    else
      raise Exception.CreateFmt('Value %d out of range. Value must bee between 0 -> 20.000', [Value]);
  end;

  procedure TColorControl.SetHue(Value : Integer);
  begin
    if (Value > -181) and (Value < 182) then
    begin
      if Value <> FHue then
        FHue := Value;
      if (not (csDesigning in TDSVideoWindowEx2(FOwner).ComponentState)) and
         (TDSVideoWindowEx2(FOwner).FFilterGraph <> nil) and
         (TDSVideoWindowEx2(FOwner).FFilterGraph.Active = True) then
           UpdateColorControls;
    end
    else
      raise Exception.CreateFmt('Value %d out of range. Value must bee between -180 -> 180', [Value]);
  end;

  procedure TColorControl.SetSaturation(Value : Integer);
  begin
    if (Value > -1) and (Value < 20001) then
    begin
      if Value <> FSaturation then
        FSaturation := Value;
      if (not (csDesigning in TDSVideoWindowEx2(FOwner).ComponentState)) and
         (TDSVideoWindowEx2(FOwner).FFilterGraph <> nil) and
         (TDSVideoWindowEx2(FOwner).FFilterGraph.Active = True) then
           UpdateColorControls;
    end
    else
      raise Exception.CreateFmt('Value %d out of range. Value must bee between 0 -> 20.000', [Value]);
  end;

  procedure TColorControl.SetSharpness(Value : Integer);
  begin
    if (Value > -1) and (Value < 11) then
    begin
      if Value <> FSharpness then
        FSharpness := Value;
      if (not (csDesigning in TDSVideoWindowEx2(FOwner).ComponentState)) and
         (TDSVideoWindowEx2(FOwner).FFilterGraph <> nil) and
         (TDSVideoWindowEx2(FOwner).FFilterGraph.Active = True) then
           UpdateColorControls;
    end
    else
      raise Exception.CreateFmt('Value %d out of range. Value must bee between 0 -> 10', [Value]);
  end;

  procedure TColorControl.SetGamma(Value : Integer);
  begin
    if (Value > 0) and (Value < 501) then
    begin
      if Value <> FGamma then
        FGamma := Value;
      if (not (csDesigning in TDSVideoWindowEx2(FOwner).ComponentState)) and
         (TDSVideoWindowEx2(FOwner).FFilterGraph <> nil) and
         (TDSVideoWindowEx2(FOwner).FFilterGraph.Active = True) then
           UpdateColorControls;
    end
    else
      raise Exception.CreateFmt('Value %d out of range. Value must bee between 1 -> 500', [Value]);
  end;

  procedure TColorControl.SetUtilColor(Value : Boolean);
  begin
    FUtilColor := Value;
    if (not (csDesigning in TDSVideoWindowEx2(FOwner).ComponentState)) and
       (TDSVideoWindowEx2(FOwner).FFilterGraph <> nil) and
       (TDSVideoWindowEx2(FOwner).FFilterGraph.Active = True) then
         UpdateColorControls;
  end;

  function TColorControl.GetBrightness : Integer;
  begin
    if (not (csDesigning in TDSVideoWindowEx2(FOwner).ComponentState)) and
       (TDSVideoWindowEx2(FOwner).FFilterGraph <> nil) and
       (TDSVideoWindowEx2(FOwner).FFilterGraph.Active = True) then
         GetColorControls;
    Result := fBrightness;
  end;

  function TColorControl.GetContrast : Integer;
  begin
    if (not (csDesigning in TDSVideoWindowEx2(FOwner).ComponentState)) and
       (TDSVideoWindowEx2(FOwner).FFilterGraph <> nil) and
       (TDSVideoWindowEx2(FOwner).FFilterGraph.Active = True) then
         GetColorControls;
    Result := fContrast;
  end;

  function TColorControl.GetHue : Integer;
  begin
    if (not (csDesigning in TDSVideoWindowEx2(FOwner).ComponentState)) and
       (TDSVideoWindowEx2(FOwner).FFilterGraph <> nil) and
       (TDSVideoWindowEx2(FOwner).FFilterGraph.Active = True) then
         GetColorControls;
    Result := fHue;
  end;

  function TColorControl.GetSaturation : Integer;
  begin
    if (not (csDesigning in TDSVideoWindowEx2(FOwner).ComponentState)) and
       (TDSVideoWindowEx2(FOwner).FFilterGraph <> nil) and
       (TDSVideoWindowEx2(FOwner).FFilterGraph.Active = True) then
         GetColorControls;
    Result := fSaturation;
  end;

  function TColorControl.GetSharpness : Integer;
  begin
    if (not (csDesigning in TDSVideoWindowEx2(FOwner).ComponentState)) and
       (TDSVideoWindowEx2(FOwner).FFilterGraph <> nil) and
       (TDSVideoWindowEx2(FOwner).FFilterGraph.Active = True) then
         GetColorControls;
    Result := fSharpness;
  end;

  function TColorControl.GetGamma : Integer;
  begin
    if (not (csDesigning in TDSVideoWindowEx2(FOwner).ComponentState)) and
       (TDSVideoWindowEx2(FOwner).FFilterGraph <> nil) and
       (TDSVideoWindowEx2(FOwner).FFilterGraph.Active = True) then
         GetColorControls;
    Result := fGamma;
  end;

  function TColorControl.GetUtilColor : Boolean;
  begin
    if (not (csDesigning in TDSVideoWindowEx2(FOwner).ComponentState)) and
       (TDSVideoWindowEx2(FOwner).FFilterGraph <> nil) and
       (TDSVideoWindowEx2(FOwner).FFilterGraph.Active = True) then
         GetColorControls;
    Result := fUtilColor;
  end;

  // ---------------------- DSVideoWindowEx2Capabilities -------------------

  constructor TDSVideoWindowEx2Caps.create(AOwner: TDSVideoWindowEx2);
  begin
    inherited Create;
    Owner := AOwner;
  end;

  function TDSVideoWindowEx2Caps.GetCanOverlay : Boolean;
  begin
    Result := TDSVideoWindowEx2(Owner).FOverlayMixer <> nil;
  end;

  function TDSVideoWindowEx2Caps.GetCanControlBrigtness : Boolean;
  begin
    if TDSVideoWindowEx2(Owner).FColorControl <> nil then
      Result := TDSVideoWindowEx2(Owner).FColorControl.FDefault.dwFlags and DDCOLOR_BRIGHTNESS = DDCOLOR_BRIGHTNESS
    else
      Result := False;
  end;

  function TDSVideoWindowEx2Caps.GetCanControlContrast : Boolean;
  begin
    if TDSVideoWindowEx2(Owner).FColorControl <> nil then
      Result := TDSVideoWindowEx2(Owner).FColorControl.FDefault.dwFlags and DDCOLOR_CONTRAST = DDCOLOR_CONTRAST
    else
      Result := False;
  end;

  function TDSVideoWindowEx2Caps.GetCanControlHue : Boolean;
  begin
    if TDSVideoWindowEx2(Owner).FColorControl <> nil then
      Result := TDSVideoWindowEx2(Owner).FColorControl.FDefault.dwFlags and DDCOLOR_HUE = DDCOLOR_HUE
    else
      Result := False;
  end;

  function TDSVideoWindowEx2Caps.GetCanControlSaturation : Boolean;
  begin
    if TDSVideoWindowEx2(Owner).FColorControl <> nil then
      Result := TDSVideoWindowEx2(Owner).FColorControl.FDefault.dwFlags and DDCOLOR_SATURATION = DDCOLOR_SATURATION
    else
      Result := False;
  end;

  function TDSVideoWindowEx2Caps.GetCanControlSharpness : Boolean;
  begin
    if TDSVideoWindowEx2(Owner).FColorControl <> nil then
      Result := TDSVideoWindowEx2(Owner).FColorControl.FDefault.dwFlags and DDCOLOR_SHARPNESS = DDCOLOR_SHARPNESS
    else
      Result := False;
  end;

  function TDSVideoWindowEx2Caps.GetCanControlGamma : Boolean;
  begin
    if TDSVideoWindowEx2(Owner).FColorControl <> nil then
      Result := TDSVideoWindowEx2(Owner).FColorControl.FDefault.dwFlags and DDCOLOR_GAMMA = DDCOLOR_GAMMA
    else
      Result := False;
  end;

  function TDSVideoWindowEx2Caps.GetCanControlUtilizedColor : Boolean;
  begin
    if TDSVideoWindowEx2(Owner).FColorControl <> nil then
      Result := TDSVideoWindowEx2(Owner).FColorControl.FDefault.dwFlags and DDCOLOR_COLORENABLE = DDCOLOR_COLORENABLE
    else
      Result := False;
  end;

  // ----------------------------------- Overlay Callback ------------------

  constructor TOverlayCallBack.Create(Owner : TObject);
  begin
    AOwner := Owner;
  end;

  function TOverlayCallback.OnUpdateOverlay(bBefore: BOOL; dwFlags: DWORD; bOldVisible: BOOL;
               var prcOldSrc, prcOldDest: TRECT; bNewVisible: BOOL; var prcNewSrc, prcNewDest: TRECT): HRESULT; stdcall;
  begin
    Result := S_OK;
  end;

  function TOverlayCallback.OnUpdateColorKey(var pKey: TCOLORKEY; dwColor: DWORD): HRESULT; stdcall;
  begin
    TDSVideoWindowEx2(AOwner).FColorKey := pKey.HighColorValue;
    if Assigned(TDSVideoWindowEx2(AOwner).FOnColorKey) then
      TDSVideoWindowEx2(AOwner).FOnColorKey(Self);
    Result := S_OK;
  end;

  function TOverlayCallback.OnUpdateSize(dwWidth, dwHeight, dwARWidth, dwARHeight: DWORD): HRESULT; stdcall;
  begin
    if (AOwner = nil) then
    begin
      Result := S_OK;
      Exit;
    end;
    TDSVideoWindowEx2(AOwner).GetVideoInfo;
    TDSVideoWindowEx2(AOwner).Clearback;
    Result := S_OK;
  end;

  // ------------------------------ DSVideoWindowEx -------------------------

  procedure TDSVideoWindowEx2.NotifyFilter(operation: TFilterOperation; Param: integer);
  var
    i: integer;
    EnumPins: TPinList;
    pGB : IGraphBuilder;
  begin
    EnumPins := nil;
    pGB := nil;
    try
      case operation of
        foAdding:   begin
                      GraphWasUpdatet  := False;
                      CoCreateInstance(CLSID_VideoRenderer, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter ,FBaseFilter);
                    end;
        foAdded:    begin
                      FBaseFilter.QueryInterface(IVideoWindow, FVideoWindow);
                    end;
        foRemoving: begin
                      if FOverlayMixer <> nil then
                      begin
                        FColorControl.RestoreDefault;
                        FBaseFilter.Stop;
                        EnumPins := TPinList.Create(FOverlayMixer);
                        if EnumPins.Count > 0 then
                          for i := 0 to EnumPins.Count - 1 do
                            EnumPins.Items[i].Disconnect;
                      end;
                      if FBaseFilter <> nil then
                      begin
                        FBaseFilter.Stop;
                        EnumPins := TPinList.Create(FBaseFilter);
                        if EnumPins.Count > 0 then
                          for i := 0 to EnumPins.Count - 1 do
                            EnumPins.Items[i].Disconnect;
                      end;
                      if FDDXM <> nil then
                        FDDXM.SetCallbackInterface(nil, 0);
                      if OverlayCallback <> nil then
                        OverlayCallback := nil;
                    end;
        foRemoved : begin
                      GraphWasUpdatet  := False;
                      FDDXM          := nil;
                      FOverlayMixer    := nil;
                      FVideoRenderer   := nil;
                      FVideoWindow     := nil;
                      FBaseFilter      := nil;
                    end;
      end;
    finally
      if EnumPins <> nil then
        EnumPins.Free;
      pGB := nil;
    end;
  end;

  procedure TDSVideoWindowEx2.GraphEvent(Event, Param1, Param2: integer);
  begin
    case Event of
      EC_PALETTE_CHANGED : RefreshVideoWindow;
      EC_CLOCK_CHANGED   : begin
                             if GraphBuildOk then SetVideoZOrder;
                             SetZoom(FZoom);
                             SetAspectMode(FAspectMode);
                             if GraphBuildOk then ClearBack;
                           end;

      end;
  end;

  function TDSVideoWindowEx2.GetName: string;
  begin
    result := name;
  end;

 procedure TDSVideoWindowEx2.ControlEvent(Event: TControlEvent; Param: integer = 0);
 var
   FilterInfo: TFilterInfo;
   FilterList: TFilterList;
   i: integer;
   GUID: TGUID;
   TmpName : WideString;
 begin
   FilterList := nil;
   try
     case Event of
        ceDVDRendered: begin // mean our Video Filter have been removed
                         ZeroMemory(@FilterInfo, SizeOf(TFilterInfo));
                         FBaseFilter.QueryFilterInfo(FilterInfo);
                         if not assigned(FilterInfo.pGraph) then
                         begin
                           FilterList:= TFilterList.Create(FilterGraph.FFilterGraph);
                           if FilterList.Count > 0 then
                             for i := 0 to FilterList.Count - 1 do
                             begin
                               FilterList.Items[i].GetClassID(GUID);
                               if ISEqualGUID(GUID, CLSID_VideoRenderer) then
                               begin
                                 FOverlayMixer  := nil;
                                 FBaseFilter    := nil;
                                 FVideoWindow   := nil;
                                 FVideoRenderer := nil;
                                 FBaseFilter := FilterList.Items[i];
                                 FBaseFilter.QueryInterface(IVideoWindow, FVideoWindow);
                                 GraphBuildOk := Succeeded(UpdateGraph);
                                 if GraphBuildOk then
                                 begin
                                   FColorControl.ReadDefault; // Read the Colorcontrols settings of the OverlayMixer.
                                   FColorControl.UpdateColorControls; // Apply our settings to the ColorControls.
                                 end;
                                 RefreshVideoWindow;
                                 break;
                               end
                               else
                               if ISEqualGUID(GUID, CLSID_VideoMixingRenderer) then
                               begin
                                 FOverlayMixer  := nil;
                                 FBaseFilter    := nil;
                                 FVideoRenderer := nil;
                                 TmpName := Name;
                                 if FVideoWindow <> nil then
                                   FilterGraph.FFilterGraph.AddFilter(FVideoWindow as IBaseFilter, PWideChar(TmpName));
                                 FBaseFilter := FVideoWindow as IBaseFilter;
                                 GraphBuildOk := Succeeded(UpdateGraph);
                                 if GraphBuildOk then
                                 begin
                                   FColorControl.ReadDefault; // Read the Colorcontrols settings of the OverlayMixer.
                                   FColorControl.UpdateColorControls; // Apply our settings to the ColorControls.
                                 end;
                                 RefreshVideoWindow;
                                 break;
                               end;
                             end;
                         end;
                       end;
        cePlay:        begin
                         if not GraphWasUpdatet then
                         begin
                           GraphBuildOk := Succeeded(UpdateGraph);
                           if GraphBuildOk then
                           begin
                             FColorControl.ReadDefault; // Read the Colorcontrols settings of the OverlayMixer.
                             FColorControl.UpdateColorControls; // Apply our settings to the ColorControls.
                           end;
                           RefreshVideoWindow;
                         end;
                         if GraphBuildOk then
                         begin
                           if (not FOverlayVisible) and (not FDesktopPlay) then
                           begin
                             FOverlayVisible := True;
                             if Assigned(FOnOverlay) then
                               FOnOverlay(Self, True);
                             Clearback;
                           end;
                         end;
                       end;
        cePause:       begin
                         if not GraphWasUpdatet then
                         begin
                           GraphBuildOk := Succeeded(UpdateGraph);
                           if GraphBuildOk then
                           begin
                             FColorControl.ReadDefault; // Read the Colorcontrols settings of the OverlayMixer.
                             FColorControl.UpdateColorControls; // Apply our settings to the ColorControls.
                           end;
                           RefreshVideoWindow;
                         end;
                         if GraphBuildOk then
                           if (not FOverlayVisible) and (not FDesktopPlay) then
                           begin
                             FOverlayVisible := True;
                             if Assigned(FOnOverlay) then
                               FOnOverlay(Self, True);
                             Clearback;
                           end;
                       end;
        ceStop:        begin
                         if not GraphWasUpdatet then
                         begin
                           GraphBuildOk := Succeeded(UpdateGraph);
                           if GraphBuildOk then
                           begin
                             FColorControl.ReadDefault; // Read the Colorcontrols settings of the OverlayMixer.
                             FColorControl.UpdateColorControls; // Apply our settings to the ColorControls.
                           end;
                           RefreshVideoWindow;
                         end;
                         if GraphBuildOk then
                           if FOverlayVisible then
                           begin
                             FOverlayVisible := False;
                             Clearback;
                             if Assigned(FOnOverlay) then
                               FOnOverlay(Self, False);
                           end;
                       end;
       ceFileRendered: begin
                         GraphBuildOk := Succeeded(UpdateGraph);
                         if GraphBuildOk then
                         begin
                           FColorControl.ReadDefault; // Read the Colorcontrols settings of the OverlayMixer.
                           FColorControl.UpdateColorControls; // Apply our settings to the ColorControls.
                         end;
                         RefreshVideoWindow;
                       end;
     end;
   finally
     if FilterList <> nil then
       FilterList.Free;
   end;
  end;

  procedure TDSVideoWindowEx2.RefreshVideoWindow;
  begin
    if FVideoWindow <> nil then
      with FVideoWindow do
      begin
        if FIsVideoWindowOwner then
          put_Owner(Handle)
        else
          put_Owner(Parent.Handle);
        put_WindowStyle(FWindowStyle or WS_CHILD or WS_CLIPSIBLINGS);
        put_WindowStyleEx(FWindowStyleEx);
        if FIsVideoWindowOwner then
          FVideoWindow.SetWindowPosition(0, 0, Width, Height)
        else
          FVideoWindow.SetWindowPosition(Left, Top, Width, Height);
        if Name <> '' then
          put_Caption(Name);
        put_MessageDrain(Handle);
        Application.ProcessMessages;
        put_AutoShow(not FDesktopPlay);
      end;
  end;

  function TDSVideoWindowEx2.GetFilter: IBaseFilter;
  begin
    result := FBaseFilter;
  end;

  constructor TDSVideoWindowEx2.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csDoubleClicks, csReflector];
    TabStop   := true;
    Height    := 240;
    Width     := 320;
    color     := $000000;
    FColorKey := $100010; //clNone;
    FFullScreen := false;
    FColorControl := TColorControl.create(Self);
    FCaps := TDSVideoWindowEx2Caps.Create(Self);
    AspectRatio := rmLetterBox;
    DigitalZoom := 0;
    GraphBuildOK := False;
    FNoScreenSaver := False;
    FIdleCursor := 0;
    if (csDesigning in componentstate) then Exit;
    FFullScreenControl := TForm.Create(nil);
    FFullScreenControl.Color := Color;
    FFullScreenControl.DefaultMonitor := dmDesktop;
    FFullScreenControl.BorderStyle := bsNone;
    FFullScreenControl.OnCloseQuery := FullScreenCloseQuery;
    FOldParent := nil;
    FMonitor := nil;
    FVideoWindowHandle := 0;
    GraphWasUpdatet := False;
    Application.OnIdle := MyIdleHandler;
  end;

  destructor TDSVideoWindowEx2.Destroy;
  begin
    if DesktopPlayback then
      NormalPlayback;

    if FDDXM <> nil then
      FDDXM.SetCallbackInterface(nil, 0);
    OverlayCallback := nil;
    FOverlayMixer   := nil;
    FFilterGraph    := nil;
    FVideoWindow    := nil;
    FVideoRenderer  := nil;
    FCaps.Free;
    FColorControl.Free;
    inherited Destroy;
  end;

  procedure TDSVideoWindowEx2.resize;
  begin
    if (FVideoWindow <> nil) and (not FFullScreen) and (not DesktopPlayback) then
      if FIsVideoWindowOwner then
        FVideoWindow.SetWindowPosition(0, 0, Width, Height)
      else
        FVideoWindow.SetWindowPosition(Left, Top, Width, Height);
  end;

  procedure TDSVideoWindowEx2.Loaded;
  begin
    inherited Loaded;
    FWindowStyle   := GetWindowLong(Handle, GWL_STYLE);
    FWindowStyleEx := GetWindowLong(Handle, GWL_EXSTYLE);
  end;

  procedure TDSVideoWindowEx2.Notification(AComponent: TComponent;
    Operation: TOperation);
  begin
    inherited Notification(AComponent, Operation);
    if ((AComponent = FFilterGraph) and (Operation = opRemove)) then
        FFilterGraph := nil;
  end;

  procedure TDSVideoWindowEx2.SetFilterGraph(AFilterGraph: TFilterGraph);
  begin
    if AFilterGraph = FFilterGraph then exit;
    if FFilterGraph <> nil then
    begin
      FFilterGraph.RemoveFilter(self);
      FFilterGraph.RemoveEventNotifier(self);
    end;
    if AFilterGraph <> nil then
    begin
      AFilterGraph.InsertFilter(self);
      AFilterGraph.InsertEventNotifier(self);
    end;
    FFilterGraph := AFilterGraph;
  end;

  procedure TDSVideoWindowEx2.SetTopMost(TopMost: boolean);
  begin
    FTopMost := TopMost;
  end;

  procedure TDSVideoWindowEx2.SetVideoZOrder;
  var
    input      : IPin;
    enum       : IEnumPins;
    ColorKey   : TColorKey;
    dwColorKey : DWord;
    MPC       : IMixerPinConfig;
  begin
    if not GraphBuildOK then Exit;
    try
      ColorKey.KeyType := CK_INDEX or CK_RGB;
      ColorKey.PaletteIndex := 0;
      ColorKey.LowColorValue := $000F000F;
      ColorKey.HighColorValue := $000F000F;

      FVideoWindowHandle := findWindowEx(Parent.handle, 0, 'VideoRenderer', pchar(name));
      if FVideoWindowHandle = 0 then
        FVideoWindowHandle := findWindowEx(0, 0, 'VideoRenderer', pchar(name));
      if FVideoWindowHandle = 0 then Exit;
      SetWindowPos(FVideoWindowHandle, Handle, 0, 0, 0, 0, SWP_SHOWWINDOW or SWP_NOSIZE or SWP_NOMOVE or SWP_NOCOPYBITS or SWP_NOACTIVATE);
      if (FVideoWindowHandle <> 0) then
      begin
        FOverlayMixer.EnumPins(Enum);
        Enum.Next(1, Input, nil);

        if Succeeded(Input.QueryInterface(IID_IMixerPinConfig2, MPC)) then
        begin
          MPC.GetColorKey(ColorKey, dwColorKey);
          FColorKey := ColorKey.HighColorValue;
          if Assigned(FOnColorKey) then
            FOnColorKey(Self);
        end;
      end;
    finally
      Input := nil;
      Enum  := nil;
      MPC   := nil;
    end;
  end;

  function TDSVideoWindowEx2.QueryInterface(const IID: TGUID; out Obj): HResult;
  begin
    result := inherited QueryInterface(IID, Obj);
    if failed(result) and assigned(FBaseFilter) then
      result := FBaseFilter.QueryInterface(IID, Obj);
  end;

  function TDSVideoWindowEx2.UpdateGraph : HResult;
  Type
    TConnectAction = (caConnect, caDisConnect);

    PConnection = ^TConnection;
    TConnection = record
      FromPin : IPin;
      ToPin   : IPin;
      Action  : TConnectAction;
    end;

  var
    FilterList    : TFilterList;
    VMRPinList    : TPinList;
    OVMPinList    : TPinList;
    TmpPinList    : TPinList;
    OrigConnections : TList;
    TmpVMRPinList : TPinList;
    Connection : pConnection;

    i, a: integer;
    GUID: TGUID;
    pGB : IGraphBuilder;
    VRInputPin,
    VRConnectedToPin : IPin;
    OVMInputPin      : IPin;
    OVMOutputPin     : IPIN;
    Pin              : IPin;
    pEnumPins        : IEnumPins;
    ul               : cardinal;
    pd               : TPinDirection;
    PinInfo          : TPinInfo;
    Hr               : HResult;
    VMR              : IBaseFilter;
    Line21Dec,
    Line21Dec2       : IBaseFilter;
    OVMInConected    : Boolean;
    OVMOutConected   : Boolean;
    Found            : Boolean;
  label
    FailedSoReconnect, Cleanup, SetDrawExclMode;
  begin
    // Check if we are using Overlay.
    FOverlayMixer := nil;
    FVideoRenderer := nil;
    VMR            := nil;
    Line21Dec      := nil;
    Line21Dec2     := nil;

    GraphWasUpdatet := True;
    OrigConnections := TList.Create;
    FilterList:= TFilterList.Create(FilterGraph.FFilterGraph);
    if FilterList.Count > 0 then
    for i := 0 to FilterList.Count - 1 do
    begin
      FilterList.Items[i].GetClassID(GUID);
      if ISEqualGUID(GUID, CLSID_OverlayMixer) then
        FOverlayMixer := FilterList.Items[i];
      if ISEqualGUID(GUID, CLSID_VideoMixingRenderer) then
        VMR := FilterList.Items[i];
      if ISEqualGUID(GUID, CLSID_VideoRenderer) then
        FVideoRenderer := FilterList.Items[i];
    end;

    // The Graph holds no overlay mixer filter, Let's add one.
    Result := FFilterGraph.QueryInterface(IID_IGraphBuilder, pGB);
    if Failed(Result) then
    begin
      Goto Cleanup;
    end;

    if FOverlayMixer <> nil then
    begin
      // Check if The Overlay Mixer that already exists is connected
      // correct to out VideoWindow
      OVMInConected  := False;
      OVMOutConected := False;
      OVMPinList := TPinList.Create(FOverlayMixer);
      for i := 0 To OVMPinList.Count -1 do
      begin
        OVMPinList.Items[i].QueryDirection(pd);
        if pd = PINDIR_OUTPUT then
        begin
          if Succeeded(OVMPinlist.Items[i].ConnectedTo(Pin)) then
          begin
            Pin.QueryPinInfo(PinInfo);
            if PinInfo.pFilter = FVideoRenderer then
              OVMOutConected := True;
          end;
        end
        else
        begin
          if Succeeded(OVMPinlist.Items[i].ConnectedTo(Pin)) then
            OVMInConected := True;
        end;
      end;
      if (not OVMOutConected) or (not OVMInConected) then
      begin
        Result := E_FAIL;
        Goto Cleanup;
      end
      else
      begin
        // Everything looks okay stop here.
        OVMPinList.Free;
        Goto SetDrawExclMode;
      end;
    end;

    Result := CoCreateInstance(CLSID_OverlayMixer, nil, CLSCTX_INPROC, IID_IBaseFilter, FOverlayMixer);
    if Failed(Result) then goto Cleanup;

    Result := pGB.AddFilter(fOverlayMixer, 'Overlay Mixer');
    if Failed(Result) then goto Cleanup;

    if FVideoRenderer = nil then
    begin
      Result := E_Fail;
      Goto Cleanup;
    end;

    Result := FVideoRenderer.EnumPins(pEnumPins);
    if Failed(Result) then goto Cleanup;

    Result := pEnumPins.Next(1, VRInputPin, @ul);
    if Failed(Result) then goto Cleanup;

    Result := VRInputPin.QueryDirection(pd);
    if (Failed(Result)) or (PD <> PINDIR_INPUT) then goto Cleanup;

    if VMR <> nil then
    begin
      // The Graph Uses the new VideoMixerRenderer let's try to connect
      // all filter connected to the VideoMixerRenderer to the Overlay
      // Mixer filter instead.
      VMRPinList := TPinList.Create(VMR);
      OVMPinList := TPinList.Create(FOverlayMixer);
      TmpVMRPinList := TPinList.Create;

      I := 0;
      while (i < VMRPinList.Count) and (Succeeded(VMRPinList.Items[i].ConnectedTo(Pin))) do
      begin
        // Let's find the first Input Pin on the overlay mixer not
        // connected to anything.

        Result := Pin.Disconnect;
        if Failed(Result) then goto FailedSoReconnect;

        Result := VMRPinList.Items[i].Disconnect;
        if Failed(Result) then goto FailedSoReconnect;

        New(Connection);
        Connection^.FromPin := VMRPinList.Items[i];
        Connection^.ToPin := Pin;
        Connection^.Action := caDisconnect;
        OrigConnections.Add(Connection);

        TmpVMRPinList.Add(Pin);
        VMRPinList.Update;
        Inc(i);
      end;

      i := 0;
      Repeat
        Pin := TmpVMRPinList[i];
        a := 0;
        Found := False;
        Repeat
          OVMPinList.Items[a].QueryDirection(pd);
          if pd = PINDIR_INPUT then
          begin
            OVMInputPin := OVMPinList.Items[a];
            if Failed(OVMPinList.Items[a].ConnectedTo(OVMOutputPin)) then
            begin
              Found := True;
            end;
          end;
          OVMPinList.Update;
          inc(a);
        until (a >= OVMPinList.count) or (Found);
        if not Found then
        begin
          VMRPinList.Free;
          OVMPinList.Free;
          Result := E_Fail;
          goto FailedSoReconnect;
        end;

        // Before connecting we need to check if the filter we ar working on is a Line21 Decoder2
        // And the exchange it with a Line21 Decoder because The Overlay Mixer Filter cannot connect
        // with a Line21 Decoder2
        Pin.QueryPinInfo(PinInfo);
        PinInfo.pFilter.GetClassID(GUID);

        if ISEqualGUID(GUID, CLSID_Line21Decoder2) then
        begin
          Line21Dec2 := PinInfo.pFilter;

          TmpPinList := TPinList.Create(Line21Dec2);
          Result := TmpPinList.Items[0].ConnectedTo(Pin);
          if Failed(Result) then goto FailedSoReconnect;

          Result := TmpPinList.Items[0].Disconnect;
          if Failed(Result) then goto FailedSoReconnect;

          Result := Pin.Disconnect;
          if Failed(Result) then goto FailedSoReconnect;

          New(Connection);
          Connection^.FromPin := Pin;
          Connection^.ToPin := TmpPinList.Items[0];
          Connection^.Action := caDisconnect;
          OrigConnections.Add(Connection);
          TmpPinList.Free;

          Result := CoCreateInstance(CLSID_Line21Decoder, nil, CLSCTX_INPROC, IID_IBaseFilter, Line21Dec);
          if Failed(Result) then goto Cleanup;

          Result := FilterGraph.FFilterGraph.AddFilter(Line21Dec, 'Line21 Decoder');
          if Failed(Result) then goto Cleanup;

          TmpPinList := TPinList.Create(Line21Dec);

          Result := FilterGraph.FFilterGraph.Connect(Pin, TmpPinList.Items[0]);
          if Failed(Result) then goto Cleanup;

          New(Connection);
          Connection^.FromPin := Pin;
          Connection^.ToPin := TmpPinList.Items[0];
          Connection^.Action := caConnect;
          OrigConnections.Add(Connection);

          Pin := TmpPinList.Items[1];
          TmpPinList.Free;

          Result := pGB.Connect(Pin, OVMInputPin);
          if Failed(Result) then
          begin
            VMRPinList.Free;
            OVMPinList.Free;
            Goto Failedsoreconnect;
          end;

          New(Connection);
          Connection^.FromPin := Pin;
          Connection^.ToPin := OVMInputPin;
          Connection^.Action := caConnect;
          OrigConnections.Add(Connection);
        end
        else
        begin
          Result := pGB.Connect(Pin, OVMInputPin);
          if Failed(Result) then
          begin
            VMRPinList.Free;
            OVMPinList.Free;
            Goto Failedsoreconnect;
          end;

          New(Connection);
          Connection^.FromPin := Pin;
          Connection^.ToPin := OVMInputPin;
          Connection^.Action := caConnect;
          OrigConnections.Add(Connection);
        end;

        OVMPinList.Update;
        inc(i);
      until I >= TmpVMRPinList.Count;

      VMRPinList.Free;
      OVMPinList.Free;
      TmpVMRPinList.Free;
    end
    else
    begin
      Result := VRInputPin.ConnectedTo(VRConnectedToPin);
      if Failed(Result) then goto FailedSoReconnect;

      Result := VRInputPin.Disconnect;
      if Failed(Result) then goto FailedSoReconnect;

      Result := VRConnectedToPin.Disconnect;
      if Failed(Result) then goto FailedSoReconnect;

      New(Connection);
      Connection^.FromPin := VRInputPin;
      Connection^.ToPin := VRConnectedToPin;
      Connection^.Action := caDisconnect;
      OrigConnections.Add(Connection);

      OVMPinList := TPinList.Create(FOverlayMixer);
      a := 0;
      Found := False;
      Repeat
        OVMPinList.Items[a].QueryDirection(pd);
        if pd = PINDIR_INPUT then
        begin
          OVMInputPin := OVMPinList.Items[a];
          if Failed(OVMPinList.Items[a].ConnectedTo(Pin)) then
            Found := True;
        end;
        inc(a);
      until (a >= OVMPinList.count) or (Found);
      if not Found then
      begin
        OVMPinList.Free;
        Result := E_Fail;
        Goto Cleanup;
      end;

      result := pGB.Connect(VRConnectedToPin, OVMInputPin);
      if Failed(Result) then
      begin
        OVMPinList.Free;
        Goto FailedSoReconnect;
      end;

      New(Connection);
      Connection^.FromPin := VRConnectedToPin;
      Connection^.ToPin := OVMInputPin;
      Connection^.Action := caConnect;
      OrigConnections.Add(Connection);

      OVMPinList.Free;
    end;

    Result := FOverlayMixer.FindPin('Output', OVMOutputPin);
    if Failed(Result) then goto FailedSoReconnect;

    Result := pGB.Connect(OVMOutputPin, VRInputPin);
    if Failed(Result) then goto FailedSoReconnect;

    New(Connection);
    Connection^.FromPin := OVMOutputPin;
    Connection^.ToPin := VRInputPin;
    Connection^.Action := caConnect;
    OrigConnections.Add(Connection);

  SetDrawExclMode:

    Result := FOverlayMixer.QueryInterface(IID_IDDrawExclModeVideo, FDDXM);
    if Failed(Result) then goto FailedSoReconnect;

    OverlayCallback := TOverlayCallback.Create(Self);

    Result := FDDXM.SetCallbackInterface(OverlayCallBack, 0);
    if Failed(Result) then goto FailedSoReconnect;

    if Line21Dec2 <> nil then
      filtergraph.FFilterGraph.RemoveFilter(Line21Dec2);

    if VMR <> nil then
      filtergraph.FFilterGraph.RemoveFilter(VMR);

    Goto Cleanup;

    FailedSoReconnect:
      for i := OrigConnections.Count -1 downto 0 do
      begin
        Connection := OrigConnections[i];
        Case Connection^.Action of
          caConnect    : begin
                           Connection^.FromPin.Disconnect;
                           Connection^.ToPin.Disconnect;
                         end;
          caDisconnect : begin
                           pGB.Connect(Connection^.FromPin, Connection^.ToPin);
                         end;
        end;
      end;

      if Line21Dec <> nil then
        FilterGraph.FFilterGraph.RemoveFilter(Line21Dec);

      Hr := pGB.RemoveFilter(FOverlayMixer);
      if Failed(Hr) then
      begin
        Result := Hr;
        Goto CleanUp;
      end;

      FOverlayMixer := nil;

      if VMR <> nil then
      begin
        pGB.RemoveFilter((FVideoWindow as IBaseFilter));
        FVideoWindow := nil;
        FVideoRenderer := VMR;
        FVideoWindow := (VMR as IVIdeoWindow);
      end;

    Cleanup:
      for i := 0 to OrigConnections.Count -1 do
      begin
        Connection := OrigConnections[i];
        Connection^.FromPin := nil;
        Connection^.ToPin := nil;
      end;

      VMR := nil;
      pEnumPins := nil;
      OVMInputpin := nil;
      OVMOutputPin := nil;
      VRInputPin := nil;
      VRConnectedToPin := nil;
      Line21Dec := nil;
      Line21Dec2 := nil;
      OrigConnections.Free;
      FilterList.Free;
  end;

  procedure TDSVideoWindowEx2.WndProc(var Message: TMessage);
  begin
    if (csDesigning in ComponentState) then
    begin
      inherited WndProc(Message);
      Exit;
    end;

    if ((Message.Msg = WM_CONTEXTMENU) and FullScreen) then
      begin
        if assigned(PopupMenu) then
          if PopupMenu.AutoPopup then
          begin
            PopupMenu.Popup(mouse.CursorPos.X, mouse.CursorPos.Y);
            Message.Result := 1;
          end;

        inherited WndProc(Message);
        Exit;
      end;

    if (Message.Msg = WM_ERASEBKGND) and (GraphBuildOk) then
    begin
      Message.Result := -1;
      Exit;
    end;

    if FNoScreenSaver then
      if (Message.Msg = SC_SCREENSAVE) or (Message.Msg = SC_MONITORPOWER) then
      begin
        Message.Result := 0;
        Exit;
      end;

    inherited WndProc(Message);
  end;

  procedure TDSVideoWindowEx2.ClearBack;
  var
    DC, MemDC: HDC;
    MemBitmap, OldBitmap: HBITMAP;
    BackBrush, OverlayBrush : HBrush;
  begin
    BackBrush := 0;
    OverlayBrush := 0;
    if (csDestroying in componentstate) then exit;
    DC := GetDC(0);
    MemBitmap := CreateCompatibleBitmap(DC, ClientRect.Right, ClientRect.Bottom);
    ReleaseDC(0, DC);
    MemDC := CreateCompatibleDC(0);
    OldBitmap := SelectObject(MemDC, MemBitmap);
    try
      DC := GetDC(Handle);
      BackBrush := CreateSolidBrush(Color);
      FillRect(MemDC, Rect(0,0, ClientRect.Right, ClientRect.Bottom), BackBrush);
      if not (csDesigning in ComponentState) then
      begin
        if Succeeded(GetVideoInfo) and (FOverlayVisible) then
        begin
          OverlayBrush := CreateSolidBrush(FColorKey);
          FillRect(MemDC, FVideoRect, OverlayBrush);
        end;
      end;
      BitBlt(DC, 0, 0, Self.ClientRect.Right, Self.ClientRect.Bottom, MemDC, 0, 0, SRCCOPY);
    finally
      SelectObject(MemDC, OldBitmap);
      DeleteDC(MemDC);
      DeleteObject(MemBitmap);
      DeleteObject(BackBrush);
      DeleteObject(OverlayBrush);
      ReleaseDC(Handle, DC);
    end;
    if Assigned(FOnPaint) then FOnPaint(self);
  end;

  procedure TDSVideoWindowEx2.Paint;
  begin
    inherited Paint;
    clearback;
  end;

  function TDSVideoWindowEx2.GetVideoInfo : HResult;
  Var
    BasicVideo : IBasicVideo2;
    AspX, AspY : DWord;
    VideoWidth, VideoHeight : DWord;
  begin
    Result := E_Fail;
    if (FVideoWindow = nil) or (FBaseFilter = nil) or (FDDXM = nil) or
       (FVideoRenderer = nil) or (FOverlayMixer = nil) then Exit;

    try
      if FAspectMode = rmLetterbox then
      begin
        FDDXM.GetNativeVideoProps(VideoWidth, VideoHeight, AspX, AspY);
        FVideoRect := StretchRect(ClientRect, Rect(0,0, AspX, AspY));
      end
      else
        FVideoRect := ClientRect;
      Result := S_OK;
    finally
      BasicVideo := nil;
    end;
  end;

  Procedure TDSVideoWindowEx2.StartDesktopPlayback;
  type
   TMonitorDefaultTo = (mdNearest, mdNull, mdPrimary);
  const
    MonitorDefaultFlags: array[TMonitorDefaultTo] of DWORD = (MONITOR_DEFAULTTONEAREST,
                                                              MONITOR_DEFAULTTONULL,
                                                              MONITOR_DEFAULTTOPRIMARY);
    function FindMonitor(Handle: HMONITOR): TMonitor;
    var
      I: Integer;
    begin
      Result := nil;
      for I := 0 to Screen.MonitorCount - 1 do
      if HMonitor(Screen.Monitors[I].Handle) = HMonitor(Handle) then
      begin
        Result := Screen.Monitors[I];
        break;
      end;
    end;

    function MonitorFromWindow(const Handle: THandle;
      MonitorDefault: TMonitorDefaultTo = mdNearest): TMonitor;
    begin
      Result := FindMonitor(MultiMon.MonitorFromWindow(Handle,
        MonitorDefaultFlags[MonitorDefault]));
    end;
  begin
    StartDesktopPlayback(MonitorfromWindow(Self.Handle));
  end;

  procedure TDSVideoWindowEx2.StartDesktopPlayBack(OnMonitor : TMonitor);

    procedure SetWallpaper(sWallpaperBMPPath : String);
    var
      reg : TRegistry;
    begin
      reg := TRegistry.Create;
      with reg do
      begin
        RootKey := HKEY_CURRENT_USER;
        if KeyExists('\Control Panel\Desktop') then
          if OpenKey('\Control Panel\Desktop', False) then
          begin
            if ValueExists('WallPaper') then
              WriteString('WallPaper', sWallpaperBMPPath);
          end;
      end;
      reg.Free;
      SystemParametersInfo(SPI_SETDESKWALLPAPER, 0, nil, SPIF_SENDWININICHANGE );
    end;

    function GetWallpaper : String;
    var
      reg : TRegistry;
    begin
      Result := '';
      reg := TRegistry.Create;
      with reg do
      begin
        RootKey := HKEY_CURRENT_USER;
        if KeyExists('\Control Panel\Desktop') then
          if OpenKey('\Control Panel\Desktop', False) then
          begin
            if ValueExists('WallPaper') then
              Result := ReadString('Wallpaper');
          end;
      end;
      reg.Free;
    end;
  var
    ColorIndex : Integer;
    Color : Longint;
  begin
    if DesktopPlayback then Exit;

    FMonitor := OnMonitor;
    OldDesktopPic := GetWallpaper;
    ColorIndex:=COLOR_DESKTOP;
    OldDesktopColor := GetSysColor(ColorIndex);

    SetWallPaper('');
    Color := ColorTorgb(FColorKey);
    SetSysColors(1, ColorIndex, Color);

    if FullScreen then
      NormalPlayback;

    FOldParent := Parent;

    Parent := FFullScreenControl;

    FFullScreenControl.BoundsRect := rect(OnMonitor.Left,
                                          OnMonitor.Top,
                                          OnMonitor.Left + OnMonitor.Width,
                                          OnMonitor.Top + OnMonitor.Height);

    FFullScreenControl.Show;

    FDesktopPlay := True;

    RefreshVideoWindow;
    if GraphBuildOk then SetVideoZOrder;

    FFullScreenControl.Hide;
    FOverlayVisible := False;
    ClearBack;
    if Assigned(FOnOverlay) then
      FOnOverlay(Self, False);
  end;

  procedure TDSVideoWindowEx2.NormalPlayback;

    procedure SetWallpaper(sWallpaperBMPPath : String);
    var
      reg : TRegistry;
    begin
      reg := TRegistry.Create;
      with reg do
      begin
        RootKey := HKEY_CURRENT_USER;
        if KeyExists('\Control Panel\Desktop') then
          if OpenKey('\Control Panel\Desktop', False) then
          begin
            if ValueExists('WallPaper') then
              WriteString('WallPaper', sWallpaperBMPPath);
          end;
      end;
      reg.Free;
      SystemParametersInfo(SPI_SETDESKWALLPAPER, 0, nil, SPIF_SENDWININICHANGE );
    end;

  var
    ColorIndex : Integer;
  begin
    if DesktopPlayback then
    begin
      ColorIndex := COLOR_DESKTOP;

      SetWallPaper(OldDesktopPic);
      SetSysColors(1, ColorIndex, OldDesktopColor);

      FDesktopPlay := False;
      if (csDestroying in componentstate) then exit;
    end;

    if FoldParent <> nil then
      Parent := FOldParent;

    if FullScreen then
    begin
      FFullScreenControl.Hide;
      FFullScreenControl.Invalidate;
      FFullScreen := False;
    end;
    RefreshVideoWindow;
    if GraphBuildOk then SetVideoZOrder;
    FOverlayVisible := True;
    ClearBack;
    if Assigned(FOnOverlay) then
      FOnOverlay(Self, True);
    FMonitor := nil;
  end;

  procedure TDSVideoWindowEx2.StartFullScreen;
  type
   TMonitorDefaultTo = (mdNearest, mdNull, mdPrimary);
  const
    MonitorDefaultFlags: array[TMonitorDefaultTo] of DWORD = (MONITOR_DEFAULTTONEAREST,
                                                              MONITOR_DEFAULTTONULL,
                                                              MONITOR_DEFAULTTOPRIMARY);
    function FindMonitor(Handle: HMONITOR): TMonitor;
    var
      I: Integer;
    begin
      Result := nil;
      for I := 0 to Screen.MonitorCount - 1 do
      if HMonitor(Screen.Monitors[I].Handle) = HMonitor(Handle) then
      begin
        Result := Screen.Monitors[I];
        break;
      end;
    end;

    function MonitorFromWindow(const Handle: THandle;
      MonitorDefault: TMonitorDefaultTo = mdNearest): TMonitor;
    begin
      Result := FindMonitor(MultiMon.MonitorFromWindow(Handle,
        MonitorDefaultFlags[MonitorDefault]));
    end;
  begin
    StartFullScreen(MonitorfromWindow(Self.Handle));
  end;

  procedure TDSVideoWindowEx2.StartFullScreen(OnMonitor : TMonitor);
  begin
    if FFullscreen then Exit;

    if DesktopPlayback then
      NormalPlayback;

    FMonitor := OnMonitor;
    FOldParent := Parent;

    Parent := FFullScreenControl;

    FFullScreenControl.BoundsRect := rect(OnMonitor.Left,
                                          OnMonitor.Top,
                                          OnMonitor.Left + OnMonitor.Width,
                                          OnMonitor.Top + OnMonitor.Height);

    if FTopMost then
      FFullScreenControl.FormStyle := fsStayOnTop
    Else
      FFullScreenControl.FormStyle := fsNormal;

    FFullScreenControl.Show;

    FFullScreen := True;

    RefreshVideoWindow;
    if GraphBuildOk then SetVideoZOrder;
  end;

  procedure TDSVideoWindowEx2.FullScreenCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
    if csDestroying in componentstate then
    begin
      NormalPlayback;
      CanClose := True;
    end
    else
      CanClose := False;
  end;

  procedure TDSVideoWindowEx2.SetZoom(Value : Integer);
  var
    Ratio : Real;
    TmpX, TmpY : Real;
    TmpLeft, TmpTop : Real;
    BasicVideo2 : IBasicVideo2;
    SLeft, STop, SWidth, SHeight : Integer;
  begin
    // Set DigitalZoom
    if (Value < 0) or (Value > 99) then
    begin
      raise Exception.CreateFmt('Value %d out of range. Value must bee between 0 -> 99', [Value]);
      Exit;
    end;

    if (csDesigning in ComponentState) or (FVideoRenderer = nil) then
    begin
      FZoom := Value;
      Exit;
    end;

    BasicVideo2 := nil;
    try
      if (FVideoRenderer.QueryInterface(IID_IBasicVideo2, BasicVideo2) = S_OK) then
      begin
        BasicVideo2.SetDefaultSourcePosition;
        BasicVideo2.get_SourceLeft(SLeft);
        BasicVideo2.get_SourceTop(STop);
        BasicVideo2.get_SourceWidth(SWidth);
        BasicVideo2.get_SourceHeight(SHeight);

        Ratio := SHeight / SWidth;

        TmpX := SWidth - ((Value * Swidth) / 100);
        TmpY := TmpX * Ratio;

        TmpLeft := (SWidth - TmpX) / 2;
        TmpTop := (SHeight - TmpY) / 2;

        BasicVideo2.put_SourceWidth(Trunc(TmpX));
        BasicVideo2.put_SourceHeight(Trunc(TmpY));
        BasicVideo2.put_SourceLeft(Trunc(TmpLeft));
        BasicVideo2.put_SourceTop(Trunc(TmpTop));
      end;
      FZoom := Value;
    finally
      BasicVideo2 := nil;
    end;
  end;

  procedure TDSVideoWindowEx2.SetAspectMode(Value : TRatioModes);
  var
    input      : IPin;
    enum       : IEnumPins;
    pMPC       : IMixerPinConfig2;
  begin
    if (csDesigning in ComponentState) or (FVideoRenderer = nil) or (FOverlayMixer = nil) then
    begin
      FAspectMode := Value;
      Exit;
    end;

    try
      FOverlayMixer.EnumPins(Enum);
      Enum.Next(1, Input, nil);

      if Succeeded(Input.QueryInterface(IID_IMixerPinConfig2, pMPC)) then
        if Succeeded(pMPC.SetAspectRatioMode(TAMAspectRatioMode(integer(Value)))) then
          FAspectMode := Value;
    finally
      input := nil;
      enum  := nil;
      pMPC  := nil;
    end;
    if (GraphBuildOk) and (not FDesktopPlay) then Clearback;
  end;

  procedure TDSVideoWindowEx2.MouseDown(Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
  Var
    MPos : TPoint;
  begin
    if Ffullscreen then
      MPos := Point(mouse.CursorPos.X, mouse.CursorPos.Y)
    else
      MPos := Point(X, Y);

    if FVideoWindow <> nil then
    begin
      if GraphBuildOK then
      begin
        if Self.Cursor = crnone then
        begin
          Self.Cursor := RememberCursor;
          LMousePos.X := MPos.X;
          LMousePos.Y := MPos.Y;
          LCursorMov := GetTickCount;
          if Assigned(FOnCursorVisible) then
            FOnCursorVisible(Self, True);
        end;
      end
      else
      begin
        FVideoWindow.IsCursorHidden(IsHidden);
        if IsHidden then
        begin
          FVideoWindow.HideCursor(False);
          LMousePos.X := MPos.X;
          LMousePos.Y := MPos.Y;
          LCursorMov := GetTickCount;
          IsHidden := False;
          if Assigned(FOnCursorVisible) then
            FOnCursorVisible(Self, True);
        end;
      end;
    end;

    inherited MouseDown(Button, Shift, MPos.X, MPos.Y);
  end;

  procedure TDSVideoWindowEx2.MouseMove(Shift: TShiftState; X, Y: Integer);
  var
    MPos : TPoint;
  begin
    if Ffullscreen then
      MPos := Point(mouse.CursorPos.X, mouse.CursorPos.Y)
    else
      MPos := Point(X, Y);

    if (LMousePos.X <> MPos.X) or (LMousePos.Y <> MPos.Y) then
    begin
      LMousePos.X := MPos.X;
      LMousePos.Y := MPos.Y;
      LCursorMov := GetTickCount;
      if FVideoWindow <> nil then
      begin
        if GraphBuildOk then
        begin
          if Self.Cursor = crnone then
          begin
            Self.Cursor := RememberCursor;
            if Assigned(FOnCursorVisible) then
              FOnCursorVisible(Self, True);
          end;
        end
        else
        begin
          FVideoWindow.IsCursorHidden(IsHidden);
          if IsHidden then
          begin
            FVideoWindow.HideCursor(False);
            IsHidden := False;
            if Assigned(FOnCursorVisible) then
              FOnCursorVisible(Self, True);
          end;
        end;
      end;
    end;

    inherited MouseMove(Shift, MPos.X, MPos.Y);
  end;

  procedure TDSVideoWindowEx2.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  var
    MPos : TPoint;
  begin
    if Ffullscreen then
      MPos := Point(mouse.CursorPos.X, mouse.CursorPos.Y)
    else
      MPos := Point(X, Y);

    if FVideoWindow <> nil then
    begin
      if GraphBuildOK then
      begin
        if Self.Cursor = crnone then
        begin
          Self.Cursor := RememberCursor;
          LMousePos.X := MPos.X;
          LMousePos.Y := MPos.Y;
          LCursorMov := GetTickCount;
          if Assigned(FOnCursorVisible) then
            FOnCursorVisible(Self, True);
        end;
      end
      else
      begin
        FVideoWindow.IsCursorHidden(IsHidden);
        if IsHidden then
        begin
          FVideoWindow.HideCursor(False);
          LMousePos.X := MPos.X;
          LMousePos.Y := MPos.Y;
          LCursorMov := GetTickCount;
          IsHidden := False;
          if Assigned(FOnCursorVisible) then
            FOnCursorVisible(Self, True);
        end;
      end;
    end;
    inherited MouseUp(Button, Shift, MPos.X, MPos.Y);
  end;

  procedure TDSVideoWindowEx2.MyIdleHandler(Sender: TObject; var Done: Boolean);
  var
    pt : TPoint;
  begin
    Done := True;
    if (FIdleCursor = 0) or (csDesigning in ComponentState) then exit;
    if (GetTickCount - LCursorMov >= Cardinal(FIdleCursor)) and (FVideoWindow <> nil) then
    begin
      if GraphBuildOK then
      begin
        if Self.Cursor <> crNone then
        begin
          RememberCursor := Self.Cursor;
          Self.Cursor := crNone;
          GetCursorPos(pt);
          SetCursorPos(pt.x, pt.y);
          if Assigned(FOnCursorVisible) then
            FOnCursorVisible(Self, False);
        end;
      end
      else
      begin
        FVideoWindow.IsCursorHidden(IsHidden);
        if not IsHidden then
        begin
          FVideoWindow.HideCursor(True);
          IsHidden := True;
          GetCursorPos(pt);
          SetCursorPos(pt.x, pt.y);
          if Assigned(FOnCursorVisible) then
            FOnCursorVisible(Self, False);
        end;
      end;
    end;
  end;

{ TVMRBitmap }

  constructor TVMRBitmap.Create(VideoWindow: TVideoWindow);
  begin
    Assert(Assigned(VideoWindow),'No valid video Window.');
    FCanvas := TCanvas.Create;
    FVideoWindow := VideoWindow;
    FillChar(FVMRALPHABITMAP, SizeOf(FVMRALPHABITMAP), 0);
    Options := [];
    FVMRALPHABITMAP.hdc := 0;
    FVMRALPHABITMAP.fAlpha := 1;
  end;

  destructor TVMRBitmap.Destroy;
  begin
    ResetBitmap;
    FCanvas.Free;
  end;

  procedure TVMRBitmap.Draw;
  var VMRMixerBitmap: IVMRMixerBitmap9;
  begin
    if Succeeded(FVideoWindow.QueryInterface(IVMRMixerBitmap9, VMRMixerBitmap)) then
      VMRMixerBitmap.SetAlphaBitmap(@FVMRALPHABITMAP);
  end;

  procedure TVMRBitmap.DrawTo(Left, Top, Right, Bottom, Alpha: Single; doUpdate: boolean = false);
  begin
    with FVMRALPHABITMAP do
    begin
      rDest.left := Left;
      rDest.top := Top;
      rDest.right := Right;
      rDest.bottom := Bottom;
      fAlpha := Alpha;
    end;
    if doUpdate then Update else Draw;
  end;

  function TVMRBitmap.GetAlpha: Single;
begin
  result := FVMRALPHABITMAP.fAlpha;
end;

function TVMRBitmap.GetColorKey: COLORREF;
begin
  Result := FVMRALPHABITMAP.clrSrcKey;
end;

function TVMRBitmap.GetDest: TVMR9NormalizedRect;
begin
  Result := FVMRALPHABITMAP.rDest;
end;

function TVMRBitmap.GetDestBottom: Single;
begin
  Result := FVMRALPHABITMAP.rDest.bottom;
end;

function TVMRBitmap.GetDestLeft: Single;
begin
  Result := FVMRALPHABITMAP.rDest.Left;
end;

function TVMRBitmap.GetDestRight: Single;
begin
 Result := FVMRALPHABITMAP.rDest.right
end;

function TVMRBitmap.GetDestTop: Single;
begin
  Result := FVMRALPHABITMAP.rDest.top;
end;

function TVMRBitmap.GetSource: TRect;
begin
  result := FVMRALPHABITMAP.rSrc;
end;

procedure TVMRBitmap.LoadBitmap(Bitmap: TBitmap);
var
  TmpHDC, HdcBMP: HDC;
  BMP: Windows.TBITMAP;
begin
  Assert(Assigned(Bitmap),'Invalid Bitmap.');
  ResetBitmap;
  TmpHDC := GetDC(FVideoWindow.Handle);
  if (TmpHDC = 0) then Exit;
  HdcBMP := CreateCompatibleDC(TmpHDC);
  ReleaseDC(FVideoWindow.Handle, TmpHDC);
  if (HdcBMP = 0) then Exit;
  if (0 = GetObject(Bitmap.Handle, sizeof(BMP), @BMP)) then exit;
  FBMPOld := SelectObject(HdcBMP, Bitmap.Handle);
  if (FBMPOld = 0) then Exit;
  FVMRALPHABITMAP.hdc := HdcBMP;
  FCanvas.Handle := HdcBMP;
end;

procedure TVMRBitmap.LoadEmptyBitmap(Width, Height: Integer;
  PixelFormat: TPixelFormat; Color: TColor);
var Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.Width := Width;
    Bitmap.Height := Height;
    Bitmap.PixelFormat := PixelFormat;
    Bitmap.Canvas.Brush.Color := Color;
    Bitmap.Canvas.FillRect(Bitmap.Canvas.ClipRect);
    LoadBitmap(Bitmap);
  finally
    Bitmap.Free;
  end;
end;

procedure TVMRBitmap.ResetBitmap;
begin
  FCanvas.Handle := 0;
  if FVMRALPHABITMAP.hdc <> 0 then
  begin
    DeleteObject(SelectObject(FVMRALPHABITMAP.hdc, FBMPOld));
    DeleteDC(FVMRALPHABITMAP.hdc);
    FVMRALPHABITMAP.hdc := 0;
  end;
end;

procedure TVMRBitmap.SetAlpha(const Value: Single);
begin
  FVMRALPHABITMAP.fAlpha := Value;
end;

procedure TVMRBitmap.SetColorKey(const Value: COLORREF);
begin
  FVMRALPHABITMAP.clrSrcKey := Value;
end;

procedure TVMRBitmap.SetDest(const Value: TVMR9NormalizedRect);
begin
  FVMRALPHABITMAP.rDest := Value;
end;

procedure TVMRBitmap.SetDestBottom(const Value: Single);
begin
  FVMRALPHABITMAP.rDest.bottom := Value;
end;

procedure TVMRBitmap.SetDestLeft(const Value: Single);
begin
  FVMRALPHABITMAP.rDest.Left := Value;
end;

procedure TVMRBitmap.SetDestRight(const Value: Single);
begin
  FVMRALPHABITMAP.rDest.right := Value;
end;

procedure TVMRBitmap.SetDestTop(const Value: Single);
begin
  FVMRALPHABITMAP.rDest.top := Value;
end;

procedure TVMRBitmap.SetOptions(Options: TVMRBitmapOptions);
begin
  FOptions := Options;
  FVMRALPHABITMAP.dwFlags := VMR9AlphaBitmap_hDC;
  if vmrbDisable in Options then FVMRALPHABITMAP.dwFlags := FVMRALPHABITMAP.dwFlags or VMR9AlphaBitmap_Disable;
  if vmrbSrcColorKey in Options then FVMRALPHABITMAP.dwFlags := FVMRALPHABITMAP.dwFlags or VMR9AlphaBitmap_SrcColorKey;
  if vmrbSrcRect in Options then FVMRALPHABITMAP.dwFlags := FVMRALPHABITMAP.dwFlags or VMR9AlphaBitmap_SrcRect;
end;

procedure TVMRBitmap.SetSource(const Value: TRect);
begin
  FVMRALPHABITMAP.rSrc := Value;
end;

procedure TVMRBitmap.Update;
  var VMRMixerBitmap: IVMRMixerBitmap9;
  begin
    if Succeeded(FVideoWindow.QueryInterface(IVMRMixerBitmap9, VMRMixerBitmap)) then
      VMRMixerBitmap.UpdateAlphaBitmapParameters(@FVMRALPHABITMAP);
  end;

end.
