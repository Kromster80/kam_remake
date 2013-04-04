
    (*********************************************************************
     *  DSPack 2.3.3                                                     *
     *                                                                   *
     *  home page : http://www.progdigy.com                              *
     *  email     : hgourvest@progdigy.com                               *
     *   Thanks to Michael Andersen. (DSVideoWindowEx)                   *
     *                                                                   *
     *  date      : 21-02-2003                                           *
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
  @abstract(Methods & usefull Class for Direct Show programming.)
  @author(Henri Gourvest: hgourvest@progdigy.com)
  @created(Mar 14, 2002)
  @lastmod(Oct 24, 2003)
}

unit DSUtil;
{$B-}  // needed at least for TSysDevEnum.FilterIndexOfFriendlyName
{$I jedi.inc}
{$IFDEF COMPILER7_UP}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

interface

uses
  {$IFDEF COMPILER6_UP} Variants, {$ENDIF}
  Windows, Controls, SysUtils, ActiveX, Classes, MMSystem, DirectShow9, WMF9,
  DirectDraw;

const
  IID_IPropertyBag          : TGUID = '{55272A00-42CB-11CE-8135-00AA004BB851}';
  IID_ISpecifyPropertyPages : TGUID = '{B196B28B-BAB4-101A-B69C-00AA00341D07}';
  IID_IPersistStream        : TGUID = '{00000109-0000-0000-C000-000000000046}';
  IID_IMoniker              : TGUID = '{0000000F-0000-0000-C000-000000000046}';

  // MS Mepg4 DMO
  MEDIASUBTYPE_MP42         : TGUID = '{3234504D-0000-0010-8000-00AA00389B71}';
  // DIVX
  MEDIASUBTYPE_DIVX         : TGUID = '{58564944-0000-0010-8000-00AA00389B71}';
  // VoxWare MetaSound
  MEDIASUBTYPE_VOXWARE      : TGUID = '{00000075-0000-0010-8000-00AA00389B71}';

  MiliSecPerDay : Cardinal = 86400000;
  MAX_TIME : Int64 = $7FFFFFFFFFFFFFFF;

  bits555: array[0..2] of DWord = ($007C00, $0003E0, $00001F);
  bits565: array[0..2] of DWord = ($00F800, $0007E0, $00001F);
  bits888: array[0..2] of DWord = ($FF0000, $00FF00, $0000FF);

////////////////////////////////////////////////////////////////////////////////
// DIVX ressources translated from latest OpenDivx DirectX Codec

  // divx
  CLSID_DIVX    : TGUID = '{78766964-0000-0010-8000-00aa00389b71}';
  // DIVX
  CLSID_DivX_U  : TGUID = '{58564944-0000-0010-8000-00aa00389b71}';
  // dvx1
  CLSID_DivX_   : TGUID = '{31787664-0000-0010-8000-00aa00389b71}';
  // DVX1
  CLSID_DivX__U : TGUID = '{31585644-0000-0010-8000-00aa00389b71}';
  // dx50
  CLSID_dx50    : TGUID = '{30357864-0000-0010-8000-00aa00389b71}';
  // DX50
  CLSID_DX50_   : TGUID = '{30355844-0000-0010-8000-00aa00389b71}';
  // div6
  CLSID_div6    : TGUID = '{36766964-0000-0010-8000-00aa00389b71}';
  // DIV6
  CLSID_DIV6_   : TGUID = '{36564944-0000-0010-8000-00aa00389b71}';
  // div5
  CLSID_div5    : TGUID = '{35766964-0000-0010-8000-00aa00389b71}';
  // DIV5
  CLSID_DIV5_   : TGUID = '{35564944-0000-0010-8000-00aa00389b71}';
  // div4
  CLSID_div4    : TGUID = '{34766964-0000-0010-8000-00aa00389b71}';
  // DIV4
  CLSID_DIV4_   : TGUID = '{34564944-0000-0010-8000-00aa00389b71}';
  // div3
  CLSID_div3    : TGUID = '{33766964-0000-0010-8000-00aa00389b71}';
  // DIV3
  CLSID_DIV3_   : TGUID = '{33564944-0000-0010-8000-00aa00389b71}';

  CLSID_DIVXCodec           : TGUID = '{78766964-0000-0010-8000-00aa00389b71}';
  IID_IIDivXFilterInterface : TGUID = '{D132EE97-3E38-4030-8B17-59163B30A1F5}';
  CLSID_DivXPropertiesPage  : TGUID = '{310e42a0-f913-11d4-887c-006008dc5c26}';

type
{$IFDEF VER130}
  PPointer = ^Pointer;
{$ENDIF}

  { Interface to control the Divx Decoder filter.
    TODO: discover the last function ... }
  IDivXFilterInterface = interface(IUnknown)
	['{D132EE97-3E38-4030-8B17-59163B30A1F5}']
    { OpenDivx }
    // current postprocessing level 0..100
    function get_PPLevel(out PPLevel: integer): HRESULT; stdcall;
    // new postprocessing level 0..100
    function put_PPLevel(PPLevel: integer): HRESULT; stdcall;
    // Put the default postprocessing = 0
    function put_DefaultPPLevel: HRESULT; stdcall;
    { DIVX }
    function put_MaxDelayAllowed(maxdelayallowed: integer): HRESULT; stdcall;
    function put_Brightness(brightness: integer): HRESULT; stdcall;
    function put_Contrast(contrast: integer): HRESULT; stdcall;
    function put_Saturation(saturation: integer): HRESULT; stdcall;
    function get_MaxDelayAllowed(out maxdelayallowed: integer): HRESULT; stdcall;
    function get_Brightness(out brightness: integer): HRESULT; stdcall;
    function get_Contrast(out contrast: integer): HRESULT; stdcall;
    function get_Saturation(out saturation: integer): HRESULT; stdcall;
    function put_AspectRatio(x, y: integer): HRESULT; stdcall;
    function get_AspectRatio(out x, y: integer): HRESULT; stdcall;
  end;

////////////////////////////////////////////////////////////////////////////////
// Ogg Vorbis

type
  TVORBISFORMAT = record
    nChannels:        WORD;
    nSamplesPerSec:   Longword;
    nMinBitsPerSec:   Longword;
    nAvgBitsPerSec:   Longword;
    nMaxBitsPerSec:   Longword;
    fQuality:         Double;
  end;

const

  // f07e245f-5a1f-4d1e-8bff-dc31d84a55ab
  CLSID_OggSplitter: TGUID = '{f07e245f-5a1f-4d1e-8bff-dc31d84a55ab}';

  // {078C3DAA-9E58-4d42-9E1C-7C8EE79539C5}
  CLSID_OggSplitPropPage: TGUID = '{078C3DAA-9E58-4d42-9E1C-7C8EE79539C5}';

  // 8cae96b7-85b1-4605-b23c-17ff5262b296
  CLSID_OggMux: TGUID = '{8cae96b7-85b1-4605-b23c-17ff5262b296}';

  // {AB97AFC3-D08E-4e2d-98E0-AEE6D4634BA4}
  CLSID_OggMuxPropPage: TGUID = '{AB97AFC3-D08E-4e2d-98E0-AEE6D4634BA4}';

  // {889EF574-0656-4B52-9091-072E52BB1B80}
  CLSID_VorbisEnc: TGUID = '{889EF574-0656-4B52-9091-072E52BB1B80}';

  // {c5379125-fd36-4277-a7cd-fab469ef3a2f}
  CLSID_VorbisEncPropPage: TGUID = '{c5379125-fd36-4277-a7cd-fab469ef3a2f}';

  // 02391f44-2767-4e6a-a484-9b47b506f3a4
  CLSID_VorbisDec: TGUID = '{02391f44-2767-4e6a-a484-9b47b506f3a4}';

  // 77983549-ffda-4a88-b48f-b924e8d1f01c
  CLSID_OggDSAboutPage: TGUID = '{77983549-ffda-4a88-b48f-b924e8d1f01c}';

  // {D2855FA9-61A7-4db0-B979-71F297C17A04}
  MEDIASUBTYPE_Ogg: TGUID = '{D2855FA9-61A7-4db0-B979-71F297C17A04}';

  // cddca2d5-6d75-4f98-840e-737bedd5c63b
  MEDIASUBTYPE_Vorbis: TGUID = '{cddca2d5-6d75-4f98-840e-737bedd5c63b}';

  // 6bddfa7e-9f22-46a9-ab5e-884eff294d9f
  FORMAT_VorbisFormat: TGUID = '{6bddfa7e-9f22-46a9-ab5e-884eff294d9f}';


////////////////////////////////////////////////////////////////////////////////
// WMF9 Utils
type
  TWMPofiles8 = (
    wmp_V80_255VideoPDA,
    wmp_V80_150VideoPDA,
    wmp_V80_28856VideoMBR,
    wmp_V80_100768VideoMBR,
    wmp_V80_288100VideoMBR,
    wmp_V80_288Video,
    wmp_V80_56Video,
    wmp_V80_100Video,
    wmp_V80_256Video,
    wmp_V80_384Video,
    wmp_V80_768Video,
    wmp_V80_700NTSCVideo,
    wmp_V80_1400NTSCVideo,
    wmp_V80_384PALVideo,
    wmp_V80_700PALVideo,
    wmp_V80_288MonoAudio,
    wmp_V80_288StereoAudio,
    wmp_V80_32StereoAudio,
    wmp_V80_48StereoAudio,
    wmp_V80_64StereoAudio,
    wmp_V80_96StereoAudio,
    wmp_V80_128StereoAudio,
    wmp_V80_288VideoOnly,
    wmp_V80_56VideoOnly,
    wmp_V80_FAIRVBRVideo,
    wmp_V80_HIGHVBRVideo,
    wmp_V80_BESTVBRVideo
  );

const
   WMProfiles8 : array[TWMPofiles8] of TGUID =
    ('{FEEDBCDF-3FAC-4c93-AC0D-47941EC72C0B}',
     '{AEE16DFA-2C14-4a2f-AD3F-A3034031784F}',
     '{D66920C4-C21F-4ec8-A0B4-95CF2BD57FC4}',
     '{5BDB5A0E-979E-47d3-9596-73B386392A55}',
     '{D8722C69-2419-4b36-B4E0-6E17B60564E5}',
     '{3DF678D9-1352-4186-BBF8-74F0C19B6AE2}',
     '{254E8A96-2612-405c-8039-F0BF725CED7D}',
     '{A2E300B4-C2D4-4fc0-B5DD-ECBD948DC0DF}',
     '{BBC75500-33D2-4466-B86B-122B201CC9AE}',
     '{29B00C2B-09A9-48bd-AD09-CDAE117D1DA7}',
     '{74D01102-E71A-4820-8F0D-13D2EC1E4872}',
     '{C8C2985F-E5D9-4538-9E23-9B21BF78F745}',
     '{931D1BEE-617A-4bcd-9905-CCD0786683EE}',
     '{9227C692-AE62-4f72-A7EA-736062D0E21E}',
     '{EC298949-639B-45e2-96FD-4AB32D5919C2}',
     '{7EA3126D-E1BA-4716-89AF-F65CEE0C0C67}',
     '{7E4CAB5C-35DC-45bb-A7C0-19B28070D0CC}',
     '{60907F9F-B352-47e5-B210-0EF1F47E9F9D}',
     '{5EE06BE5-492B-480a-8A8F-12F373ECF9D4}',
     '{09BB5BC4-3176-457f-8DD6-3CD919123E2D}',
     '{1FC81930-61F2-436f-9D33-349F2A1C0F10}',
     '{407B9450-8BDC-4ee5-88B8-6F527BD941F2}',
     '{8C45B4C7-4AEB-4f78-A5EC-88420B9DADEF}',
     '{6E2A6955-81DF-4943-BA50-68A986A708F6}',
     '{3510A862-5850-4886-835F-D78EC6A64042}',
     '{0F10D9D3-3B04-4fb0-A3D3-88D4AC854ACC}',
     '{048439BA-309C-440e-9CB4-3DCCA3756423}');


  function ProfileFromGUID(const GUID: TGUID): TWMPofiles8;
////////////////////////////////////////////////////////////////////////////////

  { Frees an object reference and replaces the reference with Nil. (Delphi4 compatibility)}
  procedure FreeAndNil(var Obj);

  { Enable Graphedit to connect with a filter graph.<br>
    The application must register the filter graph instance in the Running Object
    Table (ROT). The ROT is a globally accessible look-up table that keeps track
    of running objects. Objects are registered in the ROT by moniker. To connect
    to the graph, GraphEdit searches the ROT for monikers whose display name matches
    a particular format: !FilterGraph X pid Y.<br>
    <b>Graph:</b> a graph interface (IGraphBuilder, IFilterGraph, IFilterGraph2).<br>
    <b>ID:</b> return the ROT identifier.}
  function AddGraphToRot(Graph: IFilterGraph; out ID: integer): HRESULT;

  { Disable Graphedit to connect with your filter graph.<br>
    <b>ID:</b> identifier provided by the @link(AddGraphToRot) method.}
  function RemoveGraphFromRot(ID: integer): HRESULT;

  { deprecated, convert a Time code event to TDVD_TimeCode record. }
  function IntToTimeCode(x : longint): TDVDTimeCode;

  { Return a string explaining a filter graph event. }
  function  GetEventCodeDef(code: longint): string;

  { General purpose function to delete a heap allocated TAM_MEDIA_TYPE structure
    which is useful when calling IEnumMediaTypes.Next as the interface
    implementation allocates the structures which you must later delete
    the format block may also be a pointer to an interface to release. }
  procedure DeleteMediaType(pmt: PAMMediaType);

  { The CreateMediaType function allocates a new AM_MEDIA_TYPE structure,
    including the format block. This also comes in useful when using the
    IEnumMediaTypes interface so that you can copy a media type, you can do
    nearly the same by creating a TMediaType class but as soon as it goes out
    of scope the destructor will delete the memory it allocated
    (this takes a copy of the memory). }
  function  CreateMediaType(pSrc: PAMMediaType): PAMMediaType;

  { The CopyMediaType function copies an AM_MEDIA_TYPE structure into another
    structure, including the format block. This function allocates the memory
    for the format block. If the pmtTarget parameter already contains an allocated
    format block, a memory leak will occur. To avoid a memory leak, call
    FreeMediaType before calling this function. }
  procedure CopyMediaType(pmtTarget: PAMMediaType; pmtSource: PAMMediaType);

  { The FreeMediaType function frees the format block in an AM_MEDIA_TYPE structure.
    Use this function to free just the format block. To delete the AM_MEDIA_TYPE
    structure, call DeleteMediaType. }
  procedure FreeMediaType(mt: PAMMediaType);

  { The CreateAudioMediaType function initializes a media type from a TWAVEFORMATEX structure.
    If the bSetFormat parameter is TRUE, the method allocates the memory for the format
    block. If the pmt parameter already contains an allocated format block, a memory
    leak will occur. To avoid a memory leak, call FreeMediaType before calling this function.
    After the method returns, call FreeMediaType again to free the format block. }
  function CreateAudioMediaType(pwfx: PWaveFormatEx; pmt: PAMMediaType; bSetFormat: boolean): HRESULT;

  { The FOURCCMap function provides conversion between GUID media subtypes and
    old-style FOURCC 32-bit media tags. In the original Microsoft® Windows®
    multimedia APIs, media types were tagged with 32-bit values created from
    four 8-bit characters and were known as FOURCCs. Microsoft DirectShow® media
    types have GUIDs for the subtype, partly because these are simpler to create
    (creation of a new FOURCC requires its registration with Microsoft).
    Because FOURCCs are unique, a one-to-one mapping has been made possible by
    allocating a range of 4,000 million GUIDs representing FOURCCs. This range
    is all GUIDs of the form: XXXXXXXX-0000-0010-8000-00AA00389B71. }
  function FOURCCMap(Fourcc: Cardinal): TGUID;

  { Find the four-character codes wich identifi a codec. }
  function GetFOURCC(Fourcc: Cardinal): string;

  { Convert a FCC (Four Char Codes) to Cardinal. A FCC identifie a media type.}
  function FCC(str: String): Cardinal;

  { Create the four-character codes from a Cardinal value. }
  function MAKEFOURCC(ch0, ch1, ch2, ch3: char): Cardinal;

  { The GetErrorString function retrieves the error message for a given return
    code, using the current language setting.}
  function GetErrorString(hr: HRESULT): string;

  { This function examine a media type and return a short description like GraphEdit. }
  function GetMediaTypeDescription(MediaType: PAMMediaType): string;

  { Retrieve the Size needed to store a bitmat }
  function GetBitmapSize(Header: PBitmapInfoHeader): DWORD;

  function GetBitmapSubtype(bmiHeader: PBitmapInfoHeader): TGUID; stdcall;

  function GetTrueColorType(bmiHeader: PBitmapInfoHeader): TGUID; stdcall;

  function GetDXSDKMediaPath : String;

  function CopyScreenToBitmap(Rect : TRect; pData : PByte; pHeader : PBitmapInfo) : HBitmap;


type
  { Property pages.<br>See also: @link(ShowFilterPropertyPage), @link:(HaveFilterPropertyPage).}
  TPropertyPage = (
    ppDefault,       // Simple property page.
    ppVFWCapDisplay, // Capture Video source dialog box.
    ppVFWCapFormat,  // Capture Video format dialog box.
    ppVFWCapSource,  // Capture Video source dialog box.
    ppVFWCompConfig, // Compress Configure dialog box.
    ppVFWCompAbout   // Compress About Dialog box.
  );

  { Show the property page associated with the Filter.
    A property page is one way for a filter to support properties that the user can set.
    Many of the filters provided with DirectShow support property pages, they are
    intended for debugging purposes, and are not recommended for application use.
    In most cases the equivalent functionality is provided through a custom interface
    on the filter. An application should control these filters programatically,
    rather than expose their property pages to users. }
  function ShowFilterPropertyPage(parent: THandle; Filter: IBaseFilter;
    PropertyPage: TPropertyPage = ppDefault): HRESULT;

  { Return true if the specified property page is provided by the Filter.}
  function HaveFilterPropertyPage(Filter: IBaseFilter;
    PropertyPage: TPropertyPage = ppDefault): boolean;

  { Show the property page associated with the Pin. <br>
    <b>See also: </b> @link:(ShowFilterPropertyPage).}
  function ShowPinPropertyPage(parent: THandle; Pin: IPin): HRESULT;

  { Convert 100 nano sec unit to milisecondes. }
  function RefTimeToMiliSec(RefTime: Int64): Cardinal;

  { Convert milisecondes to 100 nano sec unit}
  function MiliSecToRefTime(Milisec: int64): Int64;

{  The mechanism for describing a bitmap format is with the BITMAPINFOHEADER
   This is really messy to deal with because it invariably has fields that
   follow it holding bit fields, palettes and the rest. This function gives
   the number of bytes required to hold a VIDEOINFO that represents it. This
   count includes the prefix information (like the rcSource rectangle) the
   BITMAPINFOHEADER field, and any other colour information on the end.

   WARNING If you want to copy a BITMAPINFOHEADER into a VIDEOINFO always make
   sure that you use the HEADER macro because the BITMAPINFOHEADER field isn't
   right at the start of the VIDEOINFO (there are a number of other fields),

       CopyMemory(HEADER(pVideoInfo),pbmi,sizeof(BITMAPINFOHEADER)); }
  function GetBitmapFormatSize(const Header: TBitmapInfoHeader): Integer;

  { Retrieve original source rectangle from a TAM_Media_type record.}
  function GetSourceRectFromMediaType(const MediaType: TAMMediaType): TRect;

  { TODO -oMichael Andersen: make documentation }
  function StretchRect(R, IR: TRect): TRect;

  // raise @link(EDirectShowException) exception if failed.
  function CheckDSError(HR: HRESULT): HRESULT;

// milenko start (added functions from dshowutil.cpp)
  function FindRenderer(pGB: IGraphBuilder; const mediatype: PGUID; out ppFilter: IBaseFilter): HRESULT;
  function FindAudioRenderer(pGB: IGraphBuilder; out ppFilter: IBaseFilter): HRESULT;
  function FindVideoRenderer(pGB: IGraphBuilder; out ppFilter: IBaseFilter): HRESULT;
  function CountFilterPins(pFilter: IBaseFilter; out pulInPins: Cardinal; out pulOutPins: Cardinal): HRESULT;
  function CountTotalFilterPins(pFilter: IBaseFilter; out pulPins: Cardinal): HRESULT;
  function GetPin(pFilter: IBaseFilter; dirrequired: TPinDirection; iNum: integer; out ppPin: IPin): HRESULT;
  function GetInPin(pFilter: IBaseFilter; nPin: integer): IPin;
  function GetOutPin(pFilter: IBaseFilter; nPin: integer): IPin;
  function FindOtherSplitterPin(pPinIn: IPin; guid: TGUID; nStream: integer; out ppSplitPin: IPin): HRESULT;
  function SeekNextFrame(pSeeking: IMediaSeeking; FPS: Double; Frame: LongInt): HRESULT;
  procedure ShowFilenameByCLSID(clsid: TGUID; out szFilename: WideString);
  function GetFileDurationString(pMS: IMediaSeeking; out szDuration: WideString): HRESULT;
  function CanFrameStep(pGB: IGraphBuilder): Boolean;
  procedure UtilFreeMediaType(pmt: PAMMediaType);
  procedure UtilDeleteMediaType(pmt: PAMMediaType);
  function SaveGraphFile(pGraph: IGraphBuilder; wszPath: WideString): HRESULT;
  function LoadGraphFile(pGraph: IGraphBuilder; const wszName: WideString): HRESULT;
// milenko end

  // Added by Michael. Used to Detect installed DirectX Version. (Source from getdxver.cpp)
  function GetDXVersion(var pdwDirectXVersion :  DWORD; out strDirectXVersion : String) : HResult;

type
  // DirectShow Exception class
  EDirectShowException = class(Exception)
    ErrorCode: Integer;
  end;

  EDSPackException = class(Exception)
    ErrorCode: Integer;
  end;

// *****************************************************************************
//  TSysDevEnum
// *****************************************************************************
  {@exclude}
  PFilCatNode = ^TFilCatNode;
  {@exclude}
  TFilCatNode = record
    FriendlyName : Shortstring;
    CLSID        : TGUID;
  end;

  { Usefull class to enumerate availables filters.
    See "Filter Enumerator" sample. }
  TSysDevEnum = class
  private
    FGUID       : TGUID;
    FCategories : TList;
    FFilters    : TList;
    ACategory   : PFilCatNode;
    procedure   GetCat(catlist: TList; CatGUID: TGUID);
    function    GetCountCategories: integer;
    function    GetCountFilters: integer;
    function    GetCategory(item: integer): TFilCatNode;
    function    GetFilter(item: integer): TFilCatNode;
  public
    { Select the main category by GUID. For example CLSID_VideoCompressorCategory
      to enumerate Video Compressors. }
    procedure SelectGUIDCategory(GUID: TGUID);
    { Select the main category by Index. }
    procedure SelectIndexCategory(index: integer);
    { Call CountCategories to retrieve categories count.}
    property CountCategories: integer read GetCountCategories;
    { Call CountFilters to retrieve the number of Filte within a Category. }
    property CountFilters: integer read GetCountFilters;
    { Call Categories to read Category Name and GUID. }
    property Categories[item: integer]: TFilCatNode read GetCategory;
    { Call Filters to read Filter Name and GUID. }
    property Filters[item: integer]: TFilCatNode read GetFilter;
    { Find filter index by FriendlyName; -1, if not found }
    function FilterIndexOfFriendlyName(const FriendlyName: string): Integer;
    { Call GetBaseFilter to retrieve the IBaseFilter interface corresponding to index. }
    function GetBaseFilter(index: integer): IBaseFilter; overload;
    { Call GetBaseFilter to retrieve the IBaseFilter interface corresponding to GUID. }
    function GetBaseFilter(GUID: TGUID): IBaseFilter; overload;
    { Call GetMoniker to retrieve the IMoniker interface corresponding to index.
      This interface can be used to store a filter with the @link(TBaseFiter) class. }
    function GetMoniker(index: integer): IMoniker;
    { constructor }
    constructor Create; overload;
    { constructor. Create the class and initialize the main category with the GUID. }
    constructor Create(guid: TGUID); overload;
    { destructor }
    destructor Destroy; override;
  end;

// *****************************************************************************
//  TFilterList
// *****************************************************************************

  { This class can enumerate all filters in a FilterGraph. }
  TFilterList = class(TInterfaceList)
  private
    Graph : IFilterGraph;
    function  GetFilter(Index: Integer): IBaseFilter;
    procedure PutFilter(Index: Integer; Item: IBaseFilter);
    function  GetFilterInfo(index: integer): TFilterInfo;
  public
    { Create a list based on a FilterGraph. }
    constructor Create(FilterGraph: IFilterGraph); overload;
    { Destructor. }
    destructor Destroy; override;
    { Update the list. }
    procedure Update;
    { Reload the list from another FilterGraph.}
    procedure Assign(FilterGraph: IFilterGraph);
    { Call First to obtain the first interface in the list. }
    function First: IBaseFilter;
    { Call IndexOf to obtain the index of an interface. }
    function IndexOf(Item: IBaseFilter): Integer;
    { Call Add to add an interface to the list. }
    function Add(Item: IBaseFilter): Integer;
    { Call Insert to insert an interface into the list. Item is the interface to
      insert, and Index indicates the position (zero-offset) where the interface
      should be added. }
    procedure Insert(Index: Integer; Item: IBaseFilter);
    { Call Last to obtain the last interface in the list. }
    function Last: IBaseFilter;
    { Call Remove to remove an interface from the list. Remove returns the index
      of the removed interface, or –1 if the interface was not found. }
    function Remove(Item: IBaseFilter): Integer;
    { Use Items to directly access an interface in the list. Index identifies each
      interface by its position in the list. }
    property Items[Index: Integer]: IBaseFilter read GetFilter write PutFilter; default;
    { call FilterInfo to retrieve the Filer name and his FilterGraph. }
    property FilterInfo[Index: Integer] : TFilterInfo read GetFilterInfo;
  end;

//******************************************************************************
//  TPinList
//******************************************************************************

  {Helper class to enumerate pins on a filter. }
  TPinList = class(TInterfaceList)
  private
    Filter: IBaseFilter;
    function  GetPin(Index: Integer): IPin;
    procedure PutPin(Index: Integer; Item: IPin);
    function  GetPinInfo(index: integer): TPinInfo;
    function GetConnected(Index: Integer): boolean;
  public
    { Create a Pin list from the IBaseFilter interface. }
    constructor Create(BaseFilter: IBaseFilter); overload;
    { Destructor. }
    destructor Destroy; override;
    { Update the Pin list. }
    procedure Update;
    { Load a Pin list from the IBaseFilter Interface. }
    procedure Assign(BaseFilter: IBaseFilter);
    { Return the First Pin from in the list. }
    function First: IPin;
    { Return the index of Pin in the list. }
    function IndexOf(Item: IPin): Integer;
    { Add A Pin to the list. }
    function Add(Item: IPin): Integer;
    { Insert a pin at the given position. }
    procedure Insert(Index: Integer; Item: IPin);
    { Return the last pin in the list. }
    function Last: IPin;
    { Remove a pin from the lis. }
    function Remove(Item: IPin): Integer;
    { Return the the pin interface at the defined position. }
    property Items[Index: Integer]: IPin read GetPin write PutPin; default;
    { Retrieve informations on a pin. }
    property PinInfo[Index: Integer]: TPinInfo read GetPinInfo;
    property Connected[Index: Integer]: boolean read GetConnected;
  end;

// *****************************************************************************
//  TMediaType
// *****************************************************************************

  { Uses TMediaType to configure media types. This class have a special property editor.
    See @link(TSampleGrabber)}
  TMediaType = class(TPersistent)
  private
    function GetMajorType: TGUID;
    procedure SetMajorType(MT: TGUID);
    function GetSubType: TGUID;
    procedure SetSubType(ST: TGUID);
    procedure SetFormatType(const GUID: TGUID);
    function GetFormatType: TGUID;
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
  protected
    { @exclude}
    procedure DefineProperties(Filer: TFiler); override;
  public
    { Local copy of the Media Type. }
    AMMediaType: PAMMediaType;
    { Destructor method. }
    destructor Destroy; override;
    { Constructor method. }
    constructor Create; overload;
    { Constructor method. Initialised with majortype. }
    constructor Create(majortype: TGUID); overload;
    { Constructor method. Initialised with another media type. }
    constructor Create(mediatype: PAMMediaType); overload;
    { Constructor method. Initialised with another TMediaType}
    constructor Create(MTClass: TMediaType); overload;
    { Copy from another TMediaType. }
    procedure Assign(Source: TPersistent); override;
    { Copy from another PAM_MEDIA_TYPE. }
    procedure Read(mediatype: PAMMediaType);
    { Tests for equality between TMediaType objects.<br>
      <b>rt:</b> Reference to the TMediaType object to compare.<br>
      Returns TRUE if rt is equal to this object. Otherwise, returns FALSE. }
    function Equal(MTClass: TMediaType): boolean; overload;
    { Tests for inequality between TMediaType objects.<br>
      <b>rt:</b> Reference to the TMediaType object to compare.<br>
      Returns TRUE if rt is not equal to this object. Otherwise, returns FALSE. }
    function NotEqual(MTClass: TMediaType): boolean; overload;
    { The IsValid method determines whether a major type has been assigned to this object.
      Returns TRUE if a major type has been assigned to this object. Otherwise, returns FALSE.
      By default, TMediaType objects are initialized with a major type of GUID_NULL.
      Call this method to determine whether the object has been correctly initialized.}
    function IsValid: boolean;
    { The IsFixedSize method determines if the samples have a fixed size or a variable size.
      Returns the value of the bFixedSizeSamples member.}
    function IsFixedSize: boolean;
    { The IsTemporalCompressed method determines if the stream uses temporal compression.
      Returns the value of the bTemporalCompression member. }
    function IsTemporalCompressed: boolean;
    { The GetSampleSize method retrieves the sample size.
      If the sample size is fixed, returns the sample size in bytes. Otherwise,
      returns zero. }
    function GetSampleSize: ULONG;
    { The SetSampleSize method specifies a fixed sample size, or specifies that
      samples have a variable size. If value of sz is zero, the media type uses
      variable sample sizes. Otherwise, the sample size is fixed at sz bytes. }
    procedure SetSampleSize(SZ: ULONG);
    { The SetVariableSize method specifies that samples do not have a fixed size.
      This method sets the bFixedSizeSamples member to FALSE. Subsequent calls to the TMediaType.GetSampleSize method return zero. }
    procedure SetVariableSize;
    { The SetTemporalCompression method specifies whether samples are compressed
      using temporal (interframe) compression. }
    procedure SetTemporalCompression(bCompressed: boolean);
    { read/write pointer to format - can't change length without
      calling SetFormat, AllocFormatBuffer or ReallocFormatBuffer}
    function Format: pointer;
    { The FormatLength method retrieves the length of the format block. }
    function FormatLength: ULONG;
    { The SetFormat method specifies the format block.<br>
      <b>pFormat:</b> Pointer to a block of memory that contains the format block.<br>
      <b>length:</b> Length of the format block, in bytes. }
    function SetFormat(pFormat: pointer; length: ULONG): boolean;
    { The ResetFormatBuffer method deletes the format block. }
    procedure ResetFormatBuffer;
    { The AllocFormatBuffer method allocates memory for the format block.<br>
      <b>length:</b> Size required for the format block, in bytes.<br>
      Returns a pointer to the new block if successful. Otherwise, returns nil.<br>
      If the method successfully allocates a new format block, it frees the existing
      format block. If the allocation fails, the method leaves the existing format block. }
    function AllocFormatBuffer(length: ULONG): pointer;
    { The ReallocFormatBuffer method reallocates the format block to a new size.<br>
      <b>length:</b> New size required for the format block, in bytes. Must be greater
      than zero.<br>
      Returns a pointer to the new block if successful. Otherwise, returns either
      a pointer to the old format block, or nil.
      This method allocates a new format block. It copies as much of the existing
      format block as possible into the new format block. If the new block is
      smaller than the existing block, the existing format block is truncated.
      If the new block is larger, the contents of the additional space are undefined.
      They are not explicitly set to zero. }
    function ReallocFormatBuffer(length: ULONG): pointer;
    { The InitMediaType method initializes the media type.
      This method zeroes the object's memory, sets the fixed-sample-size property
      to TRUE, and sets the sample size to 1. }
    procedure InitMediaType;
    { The MatchesPartial method determines if this media type matches a partially
      specified media type. The media type specified by ppartial can have a value
      of GUID_NULL for the major type, subtype, or format type. Any members with
      GUID_NULL values are not tested. (In effect, GUID_NULL acts as a wildcard.)
      Members with values other than GUID_NULL must match for the media type to match.}
    function MatchesPartial(ppartial: TMediaType): boolean;
    { The IsPartiallySpecified method determines if the media type is partially
      defined. A media type is partial if the major type, subtype, or format type
      is GUID_NULL. The IPin.Connect method can accept partial media types.
      The implementation does not actually test the subtype. If there is a specified
      format type, the media type is not considered partial, even if the subtype is GUID_NULL. }
    function IsPartiallySpecified: boolean;
    { Set or retrieve the MajorType GUID. }
    property MajorType: TGUID read GetMajorType write SetMajorType;
    { Set or retrieve the SubType GUID. }
    property SubType: TGUID read GetSubType write SetSubType;
    { Set or retrieve the FormatType GUID. }
    property FormatType: TGUID read GetFormatType write SetFormatType;
  end;

// *****************************************************************************
//  TEnumMediaType
// *****************************************************************************

  { This class can retrieve all media types from a pin, a file or an IEnumMediaTypes interface. }
  TEnumMediaType = class(TObject)
  private
    FList      : TList;
    function   GetItem(Index: Integer): TMediaType;
    procedure  SetItem(Index: Integer; Item: TMediaType);
    function   GetMediaDescription(Index: Integer): string;
    function   GetCount: integer;
  public
    { Constructor method.}
    constructor Create; overload;
    { Constructor method enumerating all media types on a pin. }
    constructor Create(Pin: IPin); overload;
    { Constructor method enumerating media types provided by a IEnumMediaType interface. }
    constructor Create(EnumMT: IEnumMediaTypes); overload;
    { Constructor method enumerating all media types availables in a media file.
      Support WMF files. }
    constructor Create(FileName: TFileName); overload;
    { Destructor method. }
    destructor  Destroy; override;
    { Enumerate all media types on a pin.}
    procedure   Assign(Pin: IPin); overload;
    { Enumerate media types provided by a IEnumMediaType interface. }
    procedure   Assign(EnumMT: IEnumMediaTypes); overload;
    { Enumerate all media types availables in a media file. Support WMF files. }
    procedure   Assign(FileName: TFileName); overload;
    { Add a media type to the list. }
    function    Add(Item: TMediaType): Integer;
    { Clear the list. }
    procedure   Clear;
    { Remove a media type from the list. }
    procedure   Delete(Index: Integer);
    { Retrieve a mediaa type. }
    property    Items[Index: Integer]: TMediaType read GetItem write SetItem;
    { Return a string describing the media type. }
    property    MediaDescription[Index: Integer]: string read GetMediaDescription;
    { Number of items in the list. }
    property    Count: integer read GetCount;
  end;

// *****************************************************************************
//  TPersistentMemory
// *****************************************************************************

  { For internal use. This class is designed to store a custom memory stream with
    a form. It is the ancestor of @link(TBaseFilter).}
  TPersistentMemory = class(TPersistent)
    private
      FData: pointer;
      FDataLength: Cardinal;
      procedure ReadData(Stream: TStream);
      procedure WriteData(Stream: TStream);
      function Equal(Memory: TPersistentMemory): boolean;
      procedure AllocateMemory(ALength: Cardinal);
    protected
      { @exclude }
      procedure AssignTo(Dest: TPersistent); override;
      { @exclude }
      procedure DefineProperties(Filer: TFiler); override;
    public
      { Set/Get the buffer length. }
      property DataLength: Cardinal read FDataLength write AllocateMemory;
      { Pointer to buffer. }
      property Data: Pointer read FData;
      { Constructor }
      constructor Create; virtual;
      { Destructor }
      destructor Destroy; override;
      { Call Assign to copy the properties or other attributes of one object from another. }
      procedure Assign(Source: TPersistent); override;
  end;

// *****************************************************************************
//  TBaseFilter
// *****************************************************************************

  { This class can store a custom filter as a moniker within the dfm file. }
  TBaseFilter = class(TPersistentMemory)
  private
    procedure SetMoniker(Moniker: IMoniker);
    function GetMoniker: IMoniker;
  public
    { Set or retrieve the moniker interface.}
    property Moniker: IMoniker read GetMoniker write SetMoniker;
    { Read a property bag. For example you can read the GUID identifier (PropertyBag('CLSID'))}
    function PropertyBag(Name: WideString): OleVariant;
    {Return the IBaseFilter interface corresponding to filter.}
    function CreateFilter: IBaseFilter;
  end;

// *****************************************************************************
//  DxDiag.h
// *****************************************************************************

const
  // This identifier is passed to IDxDiagProvider::Initialize in order to ensure that an
  // application was built against the correct header files. This number is
  // incremented whenever a header (or other) change would require applications
  // to be rebuilt. If the version doesn't match, IDxDiagProvider::Initialize will fail.
  // (The number itself has no meaning.)
  DXDIAG_DX9_SDK_VERSION       = 111;

 (****************************************************************************
  *
  * DxDiag Errors
  *
  ****************************************************************************)
  DXDIAG_E_INSUFFICIENT_BUFFER = HResult($8007007A);

 (****************************************************************************
  *
  * DxDiag CLSIDs
  *
  ****************************************************************************)
  CLSID_DxDiagProvider         : TGUID = '{A65B8071-3BFE-4213-9A5B-491DA4461CA7}';

 (****************************************************************************
  *
  * DxDiag Interface IIDs
  *
  ****************************************************************************)
  IID_IDxDiagProvider          : TGUID = '{9C6B4CB0-23F8-49CC-A3ED-45A55000A6D2}';
  IID_IDxDiagContainer         : TGUID = '{7D0F462F-4064-4862-BC7F-933E5058C10F}';

Type
 (****************************************************************************
  *
  * DxDiag Structures
  *
  ****************************************************************************)

  PDXDIAG_INIT_PARAMS = ^TDxDiagInitParams;
  _DXDIAG_INIT_PARAMS = record
    dwSize                : DWORD;   // Size of this structure.
    dwDxDiagHeaderVersion : DWORD;   // Pass in DXDIAG_DX9_SDK_VERSION.  This verifies
                                     // the header and dll are correctly matched.
    bAllowWHQLChecks      : Boolean; // If true, allow dxdiag to check if drivers are
                                     // digital signed as logo'd by WHQL which may
                                     // connect via internet to update WHQL certificates.
    pReserved             : Pointer; // Reserved. Must be NULL.
  End;

  {$EXTERNALSYM _DXDIAG_INIT_PARAMS}
  DXDIAG_INIT_PARAMS = _DXDIAG_INIT_PARAMS;
  {$EXTERNALSYM DXDIAG_INIT_PARAMS}
  TDxDiagInitParams = _DXDIAG_INIT_PARAMS;

 (****************************************************************************
  *
  * DxDiag Application Interfaces
  *
  ****************************************************************************)

  IDxDiagProvider  = interface;
  IDxDiagContainer = interface;

  IDxDiagProvider = interface(IUnknown)
    ['{9C6B4CB0-23F8-49CC-A3ED-45A55000A6D2}']
    // *** IDxDiagProvider methods *** //
    function Initialize(pParams : PDXDIAG_INIT_PARAMS): HResult; stdcall;
    function GetRootContainer(Out ppInstance : IDxDiagContainer): HResult; stdcall;
  End;

  IDxDiagContainer = interface(IUnknown)
    ['{7D0F462F-4064-4862-BC7F-933E5058C10F}']
    // *** IDxDiagContainer methods *** //
    function GetNumberOfChildContainers(Out pdwCount : dword) : HResult; stdcall;
    function EnumChildContainerNames(dwIndex : dword; pwszContainer : PWideChar; cchContainer : DWord) : HResult; stdcall;
    function GetChildContainer(pwszContainer : PWideChar; Out ppInstance : IDxDiagContainer) : Hresult; stdcall;
    function GetNumberOfProps(Out pdwCount : dword) : HResult; stdcall;
    function EnumPropNames(dwIndex : dword; pwszPropName : PWideChar; cchPropName : dword) : HResult; stdcall;
    function GetProp(pwszPropName : PWideChar; Out pvarProp : OleVariant) : HResult; stdcall;
  End;

// milenko start DMO TMediaBuffer implementation
  TMediaBuffer = class(TObject, IMediaBuffer, IUnKnown)
  private
    FRefCount: integer;
    FLength: DWORD;
    FMaxLength: DWORD;
    FData: PByte;
  public
    constructor Create(MaxLen: DWORD);
    destructor Destroy; override;
    class function CreateBuffer(MaxLen: DWORD; const IID: TGUID; out Obj): HRESULT;
    // IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    // IMediaBuffer methods
    function SetLength(cbLength: DWORD): HResult; stdcall;
    function GetMaxLength(out pcbMaxLength: DWORD): HResult; stdcall;
    function GetBufferAndLength(out ppBuffer: PByte; // not filled if NULL
                                out pcbLength: DWORD    // not filled if NULL
                                ): HResult; stdcall;
  end;
// milenko end

// milenko start wxutil implementation
const
  RESOLUTION = DWORD(1);                  // High resolution timer
  ADVISE_CACHE = integer(4);              // Default cache size

  MILLISECONDS = LONGLONG(1000);         // 10 ^ 3
  NANOSECONDS = LONGLONG(1000000000);    // 10 ^ 9
  UNITS = LONGLONG(NANOSECONDS div 100); // 10 ^ 7

  TimeZero = LONGLONG(0);

type
  DWORDLONG = LONGLONG;  // Should be unsigned Int64 !!!
  ULONGLONG = DWORDLONG; // Should be unsigned Int64 !!!

  function UInt32x32To64(a, b: DWORD): ULONGLONG;
  function Int64x32Div32(a: LONGLONG; b, c, d: LongInt): LONGLONG;
  function Int32x32To64(a, b: integer): Int64;
  function MILLISECONDS_TO_100NS_UNITS(Ms: LONGLONG): LONGLONG;

  function llMulDiv(a, b, c, d: LONGLONG): LONGLONG;
  function AmGetLastErrorToHResult: HRESULT;
  function IsEqualObject(pFirst, pSecond: IUnknown): Boolean;
// milenko end

// milenko start namedguid implementation
const
  IID_IDirectDrawKernel       : TGUID = '{8D56C120-6A08-11D0-9B06-00A0C903A3B8}';
  IID_IDirectDrawSurfaceKernel: TGUID = '{60755DA0-6A40-11D0-9B06-00A0C903A3B8}';

  function GetGUIDString(GUID: TGUID): String;
// milenko end

// milenko start (usefull functions to get linear amplification)
  function GetBasicAudioVolume(Value : integer) : integer;
  function SetBasicAudioVolume(Value : integer) : integer;
  function GetBasicAudioPan(Value : integer) : integer;
  function SetBasicAudioPan(Value : integer) : integer;
// milenko end

// milenok start (yet another delphi5 compatibility ...)
{$IFDEF VER130}
  function GUIDToString(const GUID: TGUID): string;
  function StringToGUID(const S: string): TGUID;
  function EnsureRange(const AValue, AMin, AMax: Integer): Integer;  
{$ENDIF}
// milenko end

{$IFNDEF COMPILER6_UP}
  procedure Set8087CW(NewCW: Word);
  function Get8087CW: Word;
{$ENDIF}

// previously TMPEGHeaderBitsWrapper
  function MPEGHeaderBitsGetSectionLength(Header: PMPEGHeaderBits) : Word;
  function MPEGHeaderBitsGetReserved(Header: PMPEGHeaderBits): WORD;
  function MPEGHeaderBitsGetPrivateIndicator(Header: PMPEGHeaderBits): WORD;
  function MPEGHeaderBitsGetSectionSyntaxIndicator(Header: PMPEGHeaderBits): WORD;
  procedure MPEGHeaderBitsSetSectionLength(Header: PMPEGHeaderBits; AValue: WORD);
  procedure MPEGHeaderBitsSetReserved(Header: PMPEGHeaderBits; AValue: WORD);
  procedure MPEGHeaderBitsSetPrivateIndicator(Header: PMPEGHeaderBits; AValue: WORD);
  procedure MPEGHeaderBitsSetSectionSyntaxIndicator(Header: PMPEGHeaderBits; AValue: WORD);

// previously TPIDBitsWrapper
  function PIDBitsGetReserved(PIDBits: PPIDBits): WORD;
  function PIDBitsGetProgramId(PIDBits: PPIDBits): WORD;
  procedure PIDBitsSetReserved(PIDBits: PPIDBits; AValue: WORD);
  procedure PIDBitsSetProgramId(PIDBits: PPIDBits; AValue: WORD);

// previously TPIDBitsWrapper
  function MPEGHeaderVersionBitsGetCurrentNextIndicator(MPEGHeaderVersionBits: PMPEGHeaderVersionBits): Byte;
  function MPEGHeaderVersionBitsGetVersionNumber(MPEGHeaderVersionBits: PMPEGHeaderVersionBits): Byte;
  function MPEGHeaderVersionBitsGetReserved(MPEGHeaderVersionBits: PMPEGHeaderVersionBits): Byte;
  procedure MPEGHeaderVersionBitsSetCurrentNextIndicator(MPEGHeaderVersionBits: PMPEGHeaderVersionBits; AValue: Byte);
  procedure MPEGHeaderVersionBitsSetVersionNumber(MPEGHeaderVersionBits: PMPEGHeaderVersionBits; AValue: Byte);
  procedure MPEGHeaderVersionBitsSetReserved(MPEGHeaderVersionBits: PMPEGHeaderVersionBits; AValue: Byte);

implementation
uses
  DirectSound, Math, ComObj, Registry;
{$IFNDEF COMPILER6_UP}
var
  Default8087CW: Word = $1372;

  procedure Set8087CW(NewCW: Word);
  begin
    Default8087CW := NewCW;
    asm
      FNCLEX
      FLDCW Default8087CW
    end;
  end;

  function Get8087CW: Word;
  asm
    PUSH   0
    FNSTCW [ESP].Word
    POP    EAX
  end;
{$ENDIF}

  function ProfileFromGUID(const GUID: TGUID): TWMPofiles8;
  begin
    for result := low(TWMPofiles8) to high(TWMPofiles8) do
      if IsEqualGUID(GUID, WMProfiles8[result]) then Exit;
    Result := TWMPofiles8(-1);
  end;

 //----------------------------------------------------------------------------
 // Retrieve the Size needed to store a bitmat
 //----------------------------------------------------------------------------
  function GetBitmapSize(Header: PBitmapInfoHeader): DWORD;

    function WIDTHBYTES(bits: DWORD): DWORD;
    begin
      result := DWORD((bits+31) and (not 31)) div 8;
    end;

    function DIBWIDTHBYTES(bi: PBITMAPINFOHEADER): DWORD;
    begin
      result := DWORD(WIDTHBYTES(DWORD(bi.biWidth) * DWORD(bi.biBitCount)));
    end;

    function _DIBSIZE(bi: PBITMAPINFOHEADER): DWORD;
    begin
      result := DIBWIDTHBYTES(bi) * DWORD(bi.biHeight);
    end;
    
  begin
    if (Header.biHeight < 0) then result := -1 * _DIBSIZE(Header)
    else result := _DIBSIZE(Header);
  end;


// This is called if the header has a 16 bit colour depth and needs to work
// out the detailed type from the bit fields (either RGB 565 or RGB 555)

function GetTrueColorType(bmiHeader: PBitmapInfoHeader): TGUID; stdcall;
var
  bmInfo: PBitmapInfo;
begin
  bmInfo := PBitmapInfo(bmiHeader);
  ASSERT(bmiHeader.biBitCount = 16);

  // If its BI_RGB then it's RGB 555 by default
  if (bmiHeader.biCompression = BI_RGB) then
  begin
    Result := MEDIASUBTYPE_RGB555;
    Exit;
  end;

  if CompareMem(@bmInfo.bmiColors, @bits555, SizeOf(bits555)) then
    Result := MEDIASUBTYPE_RGB555 else
    if CompareMem(@bmInfo.bmiColors, @bits565, SizeOf(bits565)) then
      Result := MEDIASUBTYPE_RGB565 else
      Result := GUID_NULL;
end;

// Given a BITMAPINFOHEADER structure this returns the GUID sub type that is
// used to describe it in format negotiations. For example a video codec fills
// in the format block with a VIDEOINFO structure, it also fills in the major
// type with MEDIATYPE_VIDEO and the subtype with a GUID that matches the bit
// count, for example if it is an eight bit image then MEDIASUBTYPE_RGB8

function GetBitmapSubtype(bmiHeader: PBitmapInfoHeader): TGUID; stdcall;
begin
  ASSERT(bmiHeader <> nil);
  // If it's not RGB then create a GUID from the compression type
  if (bmiHeader.biCompression <> BI_RGB) then
    if (bmiHeader.biCompression <> BI_BITFIELDS) then
    begin
      Result := FourCCMap(bmiHeader.biCompression);
      Exit;
    end;

  // Map the RGB DIB bit depth to a image GUID
  case (bmiHeader.biBitCount) of
    1  :   result := MEDIASUBTYPE_RGB1;
    4  :   result := MEDIASUBTYPE_RGB4;
    8  :   result := MEDIASUBTYPE_RGB8;
    16 :   result := GetTrueColorType(bmiHeader);
    24 :   result := MEDIASUBTYPE_RGB24;
    32 :   result := MEDIASUBTYPE_RGB32;
  else
    result := GUID_NULL;
  end;

end;

 //----------------------------------------------------------------------------
 // Frees an object reference and replaces the reference with Nil.
 //----------------------------------------------------------------------------
  procedure FreeAndNil(var Obj);
  var
    Temp: TObject;
  begin
    Temp := TObject(Obj);
    Pointer(Obj) := nil;
    Temp.Free;
  end;

  //----------------------------------------------------------------------------
  // Enable Graphedit to connect with your filter graph
  //----------------------------------------------------------------------------
  function AddGraphToRot(Graph: IFilterGraph; out ID: integer): HRESULT;
  var
    Moniker: IMoniker;
    ROT    : IRunningObjectTable;
    wsz    : WideString;
  begin
    result := GetRunningObjectTable(0, ROT);
    if (result <> S_OK) then exit;
    wsz := format('FilterGraph %p pid %x',[pointer(graph),GetCurrentProcessId()]);
    result  := CreateItemMoniker('!', PWideChar(wsz), Moniker);
    if (result <> S_OK) then exit;
    result  := ROT.Register(0, Graph, Moniker, ID);
    Moniker := nil;
  end;

  //----------------------------------------------------------------------------
  // Disable Graphedit to connect with your filter graph
  //----------------------------------------------------------------------------
  function RemoveGraphFromRot(ID: integer): HRESULT;
  var ROT: IRunningObjectTable;
  begin
    result := GetRunningObjectTable(0, ROT);
    if (result <> S_OK) then exit;
    result := ROT.Revoke(ID);
    ROT := nil;
  end;

  function IntToTimeCode(x : longint): TDVDTimeCode;
  begin
    Result.Hours1        := (x and $F0000000) shr 28;
    Result.Hours10       := (x and $0F000000) shr 24;
    Result.Minutes1      := (x and $00F00000) shr 20;
    Result.Minutes10     := (x and $000F0000) shr 16;
    Result.Seconds1      := (x and $0000F000) shr 12;
    Result.Seconds10     := (x and $00000F00) shr 08;
    Result.Frames1       := (x and $000000F0) shr 04;
    Result.Frames10      := (x and $0000000C) shr 02;
    Result.FrameRateCode := (x and $00000003) shr 00;
  end;

  function  GetEventCodeDef(code: longint): string;
  begin
    case code of
      EC_ACTIVATE                  : result:= 'EC_ACTIVATE - A video window is being activated or deactivated.';
      EC_BUFFERING_DATA            : result:= 'EC_BUFFERING_DATA - The graph is buffering data, or has stopped buffering data.';
      EC_CLOCK_CHANGED             : result:= 'EC_CLOCK_CHANGED - The reference clock has changed.';
      EC_COMPLETE                  : result:= 'EC_COMPLETE - All data from a particular stream has been rendered.';
      EC_DEVICE_LOST               : result:= 'EC_DEVICE_LOST - A Plug and Play device was removed or has become available again.';
      EC_DISPLAY_CHANGED           : result:= 'EC_DISPLAY_CHANGED - The display mode has changed.';
      EC_END_OF_SEGMENT            : result:= 'EC_END_OF_SEGMENT - The end of a segment has been reached.';
      EC_ERROR_STILLPLAYING        : result:= 'EC_ERROR_STILLPLAYING - An asynchronous command to run the graph has failed.';
      EC_ERRORABORT                : result:= 'EC_ERRORABORT - An operation was aborted because of an error.';
      EC_FULLSCREEN_LOST           : result:= 'EC_FULLSCREEN_LOST - The video renderer is switching out of full-screen mode.';
      EC_GRAPH_CHANGED             : result:= 'EC_GRAPH_CHANGED - The filter graph has changed.';
      EC_NEED_RESTART              : result:= 'EC_NEED_RESTART - A filter is requesting that the graph be restarted.';
      EC_NOTIFY_WINDOW             : result:= 'EC_NOTIFY_WINDOW - Notifies a filter of the video renderer''s window.';
      EC_OLE_EVENT                 : result:= 'EC_OLE_EVENT - A filter is passing a text string to the application.';
      EC_OPENING_FILE              : result:= 'EC_OPENING_FILE - The graph is opening a file, or has finished opening a file.';
      EC_PALETTE_CHANGED           : result:= 'EC_PALETTE_CHANGED - The video palette has changed.';
      EC_PAUSED                    : result:= 'EC_PAUSED - A pause request has completed.';
      EC_QUALITY_CHANGE            : result:= 'EC_QUALITY_CHANGE - The graph is dropping samples, for quality control.';
      EC_REPAINT                   : result:= 'EC_REPAINT - A video renderer requires a repaint.';
      EC_SEGMENT_STARTED           : result:= 'EC_SEGMENT_STARTED - A new segment has started.';
      EC_SHUTTING_DOWN             : result:= 'EC_SHUTTING_DOWN - The filter graph is shutting down, prior to being destroyed.';
      EC_SNDDEV_IN_ERROR           : result:= 'EC_SNDDEV_IN_ERROR - An audio device error has occurred on an input pin.';
      EC_SNDDEV_OUT_ERROR          : result:= 'EC_SNDDEV_OUT_ERROR - An audio device error has occurred on an output pin.';
      EC_STARVATION                : result:= 'EC_STARVATION - A filter is not receiving enough data.';
      EC_STEP_COMPLETE             : result:= 'EC_STEP_COMPLETE - A filter performing frame stepping has stepped the specified number of frames.';
      EC_STREAM_CONTROL_STARTED    : result:= 'EC_STREAM_CONTROL_STARTED - A stream-control start command has taken effect.';
      EC_STREAM_CONTROL_STOPPED    : result:= 'EC_STREAM_CONTROL_STOPPED - A stream-control start command has taken effect.';
      EC_STREAM_ERROR_STILLPLAYING : result:= 'EC_STREAM_ERROR_STILLPLAYING - An error has occurred in a stream. The stream is still playing.';
      EC_STREAM_ERROR_STOPPED      : result:= 'EC_STREAM_ERROR_STOPPED - A stream has stopped because of an error.';
      EC_USERABORT                 : result:= 'EC_USERABORT - The user has terminated playback.';
      EC_VIDEO_SIZE_CHANGED        : result:= 'EC_VIDEO_SIZE_CHANGED - The native video size has changed.';
      EC_WINDOW_DESTROYED          : result:= 'EC_WINDOW_DESTROYED - The video renderer was destroyed or removed from the graph.';
      EC_TIMECODE_AVAILABLE        : result:= 'EC_TIMECODE_AVAILABLE- Sent by filter supporting timecode.';
      EC_EXTDEVICE_MODE_CHANGE     : result:= 'EC_EXTDEVICE_MODE_CHANGE - Sent by filter supporting IAMExtDevice.';
      EC_CLOCK_UNSET               : result:= 'EC_CLOCK_UNSET - notify the filter graph to unset the current graph clock.';
      EC_TIME                      : result:= 'EC_TIME - The requested reference time occurred (currently not used).';
      EC_VMR_RENDERDEVICE_SET      : result:= 'EC_VMR_RENDERDEVICE_SET - Identifies the type of rendering mechanism the VMR is using to display video.';

      EC_DVD_ANGLE_CHANGE              : result:= 'EC_DVD_ANGLE_CHANGE - Signals that either the number of available angles changed or that the current angle number changed.';
      EC_DVD_ANGLES_AVAILABLE          : result:= 'EC_DVD_ANGLES_AVAILABLE - Indicates whether an angle block is being played and angle changes can be performed.';
      EC_DVD_AUDIO_STREAM_CHANGE       : result:= 'EC_DVD_AUDIO_STREAM_CHANGE - Signals that the current audio stream number changed for the main title.';
      EC_DVD_BUTTON_AUTO_ACTIVATED     : result:= 'EC_DVD_BUTTON_AUTO_ACTIVATED - Signals that a menu button has been automatically activated per instructions on the disc.';
      EC_DVD_BUTTON_CHANGE             : result:= 'EC_DVD_BUTTON_CHANGE - Signals that either the number of available buttons changed or that the currently selected button number changed.';
      EC_DVD_CHAPTER_AUTOSTOP          : result:= 'EC_DVD_CHAPTER_AUTOSTOP - Indicates that playback stopped as the result of a call to the IDvdControl2::PlayChaptersAutoStop method.';
      EC_DVD_CHAPTER_START             : result:= 'EC_DVD_CHAPTER_START - Signals that the DVD Navigator started playback of a new chapter in the current title.';
      EC_DVD_CMD_START                 : result:= 'EC_DVD_CMD_START - Signals that a particular command has begun.';
      EC_DVD_CMD_END                   : result:= 'EC_DVD_CMD_END - Signals that a particular command has completed.';
      EC_DVD_CURRENT_HMSF_TIME         : result:= 'EC_DVD_CURRENT_HMSF_TIME - Signals the current time in DVD_HMSF_TIMECODE format at the beginning of every VOBU, which occurs every .4 to 1.0 sec.';
      EC_DVD_CURRENT_TIME              : result:= 'EC_DVD_CURRENT_TIME - Signals the beginning of every video object unit (VOBU), a video segment which is 0.4 to 1.0 seconds in length.';
      EC_DVD_DISC_EJECTED              : result:= 'EC_DVD_DISC_EJECTED - Signals that a disc has been ejected from the drive.';
      EC_DVD_DISC_INSERTED             : result:= 'EC_DVD_DISC_INSERTED - Signals that a disc has been inserted into the drive.';
      EC_DVD_DOMAIN_CHANGE             : result:= 'EC_DVD_DOMAIN_CHANGE - Indicates the DVD Navigator''s new domain.';
      EC_DVD_ERROR                     : result:= 'EC_DVD_ERROR - Signals a DVD error condition.';
      EC_DVD_KARAOKE_MODE              : result:= 'EC_DVD_KARAOKE_MODE - Indicates that the Navigator has either begun playing or finished playing karaoke data.';
      EC_DVD_NO_FP_PGC                 : result:= 'EC_DVD_NO_FP_PGC - Indicates that the DVD disc does not have a FP_PGC (First Play Program Chain).';
      EC_DVD_PARENTAL_LEVEL_CHANGE     : result:= 'EC_DVD_PARENTAL_LEVEL_CHANGE - Signals that the parental level of the authored content is about to change.';
      EC_DVD_PLAYBACK_RATE_CHANGE      : result:= 'EC_DVD_PLAYBACK_RATE_CHANGE - Indicates that a playback rate change has been initiated and the new rate is in the parameter.';
      EC_DVD_PLAYBACK_STOPPED          : result:= 'EC_DVD_PLAYBACK_STOPPED - Indicates that playback has been stopped. The DVD Navigator has completed playback of the title and did not find any other branching instruction for subsequent playback.';
      EC_DVD_PLAYPERIOD_AUTOSTOP       : result:= 'EC_DVD_PLAYPERIOD_AUTOSTOP - Indicates that the Navigator has finished playing the segment specified in a call to PlayPeriodInTitleAutoStop.';
      EC_DVD_STILL_OFF                 : result:= 'EC_DVD_STILL_OFF - Signals the end of any still.';
      EC_DVD_STILL_ON                  : result:= 'EC_DVD_STILL_ON - Signals the beginning of any still.';
      EC_DVD_SUBPICTURE_STREAM_CHANGE  : result:= 'EC_DVD_SUBPICTURE_STREAM_CHANGE - Signals that the current subpicture stream number changed for the main title.';
      EC_DVD_TITLE_CHANGE              : result:= 'EC_DVD_TITLE_CHANGE - Indicates when the current title number changes.';
      EC_DVD_VALID_UOPS_CHANGE         : result:= 'EC_DVD_VALID_UOPS_CHANGE - Signals that the available set of IDVDControl2 interface methods has changed.';
      EC_DVD_WARNING                   : result:= 'EC_DVD_WARNING - Signals a DVD warning condition.'
    else
      result := format('Unknow Graph Event ($%x)',[code]);
    end;
  end;

  // general purpose function to delete a heap allocated AM_MEDIA_TYPE structure
  // which is useful when calling IEnumMediaTypes::Next as the interface
  // implementation allocates the structures which you must later delete
  // the format block may also be a pointer to an interface to release
  procedure DeleteMediaType(pmt: PAMMediaType);
  begin
    // allow nil pointers for coding simplicity
    if (pmt = nil) then exit;
    FreeMediaType(pmt);
    CoTaskMemFree(pmt);
  end;

  // this also comes in useful when using the IEnumMediaTypes interface so
  // that you can copy a media type, you can do nearly the same by creating
  // a CMediaType object but as soon as it goes out of scope the destructor
  // will delete the memory it allocated (this takes a copy of the memory)
  function  CreateMediaType(pSrc: PAMMediaType): PAMMediaType;
  var pMediaType: PAMMediaType;
  begin
    ASSERT(pSrc<>nil);

    // Allocate a block of memory for the media type
    pMediaType := CoTaskMemAlloc(sizeof(TAMMediaType));
    if (pMediaType = nil) then
    begin
      result := nil;
      exit;
    end;

    // Copy the variable length format block
    CopyMediaType(pMediaType,pSrc);
    result := pMediaType;
  end;

  //----------------------------------------------------------------------------
  // Copies a task-allocated AM_MEDIA_TYPE structure.
  //----------------------------------------------------------------------------
  procedure CopyMediaType(pmtTarget: PAMMediaType; pmtSource: PAMMediaType);
  begin
    //  We'll leak if we copy onto one that already exists - there's one
    //  case we can check like that - copying to itself.
    ASSERT(pmtSource <> pmtTarget);
    //pmtTarget^ := pmtSource^;
    move(pmtSource^, pmtTarget^, SizeOf(TAMMediaType));
    if (pmtSource.cbFormat <> 0) then
    begin
      ASSERT(pmtSource.pbFormat <> nil);
      pmtTarget.pbFormat := CoTaskMemAlloc(pmtSource.cbFormat);
      if (pmtTarget.pbFormat = nil) then
        pmtTarget.cbFormat := 0
      else
        CopyMemory(pmtTarget.pbFormat, pmtSource.pbFormat, pmtTarget.cbFormat);
    end;
    if (pmtTarget.pUnk <> nil) then  pmtTarget.pUnk._AddRef;
  end;

  procedure FreeMediaType(mt: PAMMediaType);
  begin
    if (mt^.cbFormat <> 0) then
    begin
      CoTaskMemFree(mt^.pbFormat);
      // Strictly unnecessary but tidier
      mt^.cbFormat := 0;
      mt^.pbFormat := nil;
    end;
    if (mt^.pUnk <> nil) then mt^.pUnk := nil;
  end;

  //----------------------------------------------------------------------------
  //  Initializes a media type structure given a wave format structure.
  //----------------------------------------------------------------------------
  function CreateAudioMediaType(pwfx: PWaveFormatEx; pmt: PAMMediaType; bSetFormat: boolean): HRESULT;
  begin
    pmt.majortype := MEDIATYPE_Audio;
    if (pwfx.wFormatTag = WAVE_FORMAT_EXTENSIBLE) then
      pmt.subtype := PWAVEFORMATEXTENSIBLE(pwfx).SubFormat
    else
      pmt.subtype := FOURCCMap(pwfx.wFormatTag);
    pmt.formattype           := FORMAT_WaveFormatEx;
    pmt.bFixedSizeSamples    := TRUE;
    pmt.bTemporalCompression := FALSE;
    pmt.lSampleSize          := pwfx.nBlockAlign;
    pmt.pUnk                 := nil;
    if (bSetFormat) then
    begin
      if (pwfx.wFormatTag = WAVE_FORMAT_PCM) then
        pmt.cbFormat := sizeof(TWAVEFORMATEX)
      else
        pmt.cbFormat := sizeof(TWAVEFORMATEX) + pwfx.cbSize;
      pmt.pbFormat := CoTaskMemAlloc(pmt.cbFormat);
      if (pmt.pbFormat = nil) then
      begin
        result := E_OUTOFMEMORY;
        exit;
      end;
      if (pwfx.wFormatTag = WAVE_FORMAT_PCM) then
      begin
        CopyMemory(pmt.pbFormat, pwfx, sizeof(PCMWAVEFORMAT));
        PWAVEFORMATEX(pmt.pbFormat).cbSize := 0;
      end
      else
      begin
        CopyMemory(pmt.pbFormat, pwfx, pmt.cbFormat);
      end;
    end;
    result := S_OK;
  end;

  function  FOURCCMap(Fourcc: Cardinal): TGUID;
  const tmpguid : TGUID = '{00000000-0000-0010-8000-00AA00389B71}';
  begin
    result := tmpguid;
    result.D1 := Fourcc;
  end;

  { Convert a FCC (Four Char Codes) to Cardinal. A FCC identifie a media type.}
  {$NODEFINE FCC}
  function FCC(str: String): Cardinal;
  begin
    Assert(Length(str) >= 4);
    result := PDWORD(str)^;
  end;

  function GetFOURCC(Fourcc: Cardinal): string;
  type TFOURCC= array[0..3] of char;
  var  CC: TFOURCC;
  begin
    case Fourcc of
      0 : result := 'RGB';
      1 : result := 'RLE8';
      2 : result := 'RLE4';
      3 : result := 'BITFIELDS';   
    else
      PDWORD(@CC)^ := Fourcc; // abracadabra
      result := CC;
    end;
  end;

  {$NODEFINE MAKEFOURCC}
  function MAKEFOURCC(ch0, ch1, ch2, ch3: char): Cardinal;
  begin
    result := Cardinal(BYTE(ch0)) or
    (Cardinal(BYTE(ch1)) shl 8)   or
    (Cardinal(BYTE(ch2)) shl 16)  or
    (Cardinal(BYTE(ch3)) shl 24)
  end;

  function GetErrorString(hr: HRESULT): string;
  var buffer: array[0..254] of char;
  begin
    AMGetErrorText(hr,@buffer,255);
    result := buffer;
  end;

  function GetMediaTypeDescription(MediaType: PAMMediaType): string;
  begin
    // major types
    result := 'Major Type: ';
    if IsEqualGUID(MediaType.majortype,MEDIATYPE_AnalogAudio)   then result := result+'AnalogAudio'   else
    if IsEqualGUID(MediaType.majortype,MEDIATYPE_AnalogVideo)   then result := result+'Analogvideo'   else
    if IsEqualGUID(MediaType.majortype,MEDIATYPE_Audio)         then result := result+'Audio'         else
    if IsEqualGUID(MediaType.majortype,MEDIATYPE_AUXLine21Data) then result := result+'AUXLine21Data' else
    if IsEqualGUID(MediaType.majortype,MEDIATYPE_File)          then result := result+'File'          else
    if IsEqualGUID(MediaType.majortype,MEDIATYPE_Interleaved)   then result := result+'Interleaved'   else
    if IsEqualGUID(MediaType.majortype,MEDIATYPE_LMRT)          then result := result+'LMRT'          else
    if IsEqualGUID(MediaType.majortype,MEDIATYPE_Midi)          then result := result+'Midi'          else
    if IsEqualGUID(MediaType.majortype,MEDIATYPE_MPEG2_PES)     then result := result+'MPEG2_PES'     else
    if IsEqualGUID(MediaType.majortype,MEDIATYPE_ScriptCommand) then result := result+'ScriptCommand' else
    if IsEqualGUID(MediaType.majortype,MEDIATYPE_Stream)        then result := result+'Stream'        else
    if IsEqualGUID(MediaType.majortype,MEDIATYPE_Text)          then result := result+'Text'          else
    if IsEqualGUID(MediaType.majortype,MEDIATYPE_Timecode)      then result := result+'Timecode'      else
    if IsEqualGUID(MediaType.majortype,MEDIATYPE_URL_STREAM)    then result := result+'URL_STREAM'    else
    if IsEqualGUID(MediaType.majortype,MEDIATYPE_Video)         then result := result+'Video'         else
       result := result+'UnKnown ';
    // sub types
    result := result + ' - Sub Type: ';
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_CLPL) then result := result+'CLPL' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_YUYV) then result := result+'YUYV' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_IYUV) then result := result+'IYUV' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_YVU9) then result := result+'YVU9' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_Y411) then result := result+'Y411' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_Y41P) then result := result+'Y41P' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_YUY2) then result := result+'YUY2' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_YVYU) then result := result+'YVYU' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_UYVY) then result := result+'UYVY' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_Y211) then result := result+'Y211' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_YV12) then result := result+'YV12' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_CLJR) then result := result+'CLJR' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_IF09) then result := result+'IF09' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_CPLA) then result := result+'CPLA' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_MJPG) then result := result+'MJPG' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_TVMJ) then result := result+'TVMJ' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_WAKE) then result := result+'WAKE' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_CFCC) then result := result+'CFCC' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_IJPG) then result := result+'IJPG' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_Plum) then result := result+'Plum' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_DVCS) then result := result+'DVCS' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_DVSD) then result := result+'DVSD' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_MDVF) then result := result+'MDVF' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_RGB1) then result := result+'RGB1' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_RGB4) then result := result+'RGB4' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_RGB8) then result := result+'RGB8' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_RGB565) then result := result+'RGB565' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_RGB555) then result := result+'RGB555' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_RGB24) then result := result+'RGB24' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_RGB32) then result := result+'RGB32' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_ARGB32) then result := result+'ARGB32' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_Overlay) then result := result+'Overlay' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_MPEG1Packet) then result := result+'MPEG1Packet' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_MPEG1Payload) then result := result+'MPEG1Payload' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_MPEG1AudioPayload) then result := result+'MPEG1AudioPayload' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_MPEG1System) then result := result+'MPEG1System' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_MPEG1VideoCD) then result := result+'MPEG1VideoCD' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_MPEG1Video) then result := result+'MPEG1Video' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_MPEG1Audio) then result := result+'MPEG1Audio' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_Avi) then result := result+'Avi' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_Asf) then result := result+'Asf' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_QTMovie) then result := result+'QTMovie' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_QTRpza) then result := result+'QTRpza' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_QTSmc) then result := result+'QTSmc' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_QTRle) then result := result+'QTRle' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_QTJpeg) then result := result+'QTJpeg' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_PCMAudio_Obsolete) then result := result+'PCMAudio_Obsolete' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_PCM) then result := result+'PCM' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_WAVE) then result := result+'WAVE' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_AU) then result := result+'AU' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_AIFF) then result := result+'AIFF' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_dvsd_) then result := result+'dvsd_' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_dvhd) then result := result+'dvhd' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_dvsl) then result := result+'dvsl' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_Line21_BytePair) then result := result+'Line21_BytePair' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_Line21_GOPPacket) then result := result+'Line21_GOPPacket' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_Line21_VBIRawData) then result := result+'Line21_VBIRawData' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_DRM_Audio) then result := result+'DRM_Audio' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_IEEE_FLOAT) then result := result+'IEEE_FLOAT' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_DOLBY_AC3_SPDIF) then result := result+'DOLBY_AC3_SPDIF' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_RAW_SPORT) then result := result+'RAW_SPORT' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_SPDIF_TAG_241h) then result := result+'SPDIF_TAG_241h' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_DssVideo) then result := result+'DssVideo' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_DssAudio) then result := result+'DssAudio' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_VPVideo) then result := result+'VPVideo' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_VPVBI) then result := result+'VPVBI' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_AnalogVideo_NTSC_M) then result := result+'AnalogVideo_NTSC_M' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_AnalogVideo_PAL_B) then result := result+'AnalogVideo_PAL_B' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_AnalogVideo_PAL_D) then result := result+'AnalogVideo_PAL_D' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_AnalogVideo_PAL_G) then result := result+'AnalogVideo_PAL_G' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_AnalogVideo_PAL_H) then result := result+'AnalogVideo_PAL_H' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_AnalogVideo_PAL_I) then result := result+'AnalogVideo_PAL_I' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_AnalogVideo_PAL_M) then result := result+'AnalogVideo_PAL_M' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_AnalogVideo_PAL_N) then result := result+'AnalogVideo_PAL_N' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_AnalogVideo_PAL_N_COMBO) then result := result+'AnalogVideo_PAL_N_COMBO' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_AnalogVideo_SECAM_B) then result := result+'AnalogVideo_SECAM_B' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_AnalogVideo_SECAM_D) then result := result+'AnalogVideo_SECAM_D' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_AnalogVideo_SECAM_G) then result := result+'AnalogVideo_SECAM_G' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_AnalogVideo_SECAM_H) then result := result+'AnalogVideo_SECAM_H' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_AnalogVideo_SECAM_K) then result := result+'AnalogVideo_SECAM_K' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_AnalogVideo_SECAM_K1) then result := result+'AnalogVideo_SECAM_K1' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_AnalogVideo_SECAM_L) then result := result+'AnalogVideo_SECAM_L' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_MPEG2_VIDEO) then result := result+'MPEG2_VIDEO' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_MPEG2_PROGRAM) then result := result+'MPEG2_PROGRAM' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_MPEG2_TRANSPORT) then result := result+'MPEG2_TRANSPORT' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_MPEG2_AUDIO) then result := result+'MPEG2_AUDIO' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_DOLBY_AC3) then result := result+'DOLBY_AC3' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_DVD_SUBPICTURE) then result := result+'DVD_SUBPICTURE' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_DVD_LPCM_AUDIO) then result := result+'DVD_LPCM_AUDIO' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_DTS) then result := result+'DTS' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_SDDS) then result := result+'SDDS' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_DVD_NAVIGATION_PCI) then result := result+'PCI' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_DVD_NAVIGATION_DSI) then result := result+'DSI' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_DVD_NAVIGATION_PROVIDER) then result := result+'PROVIDER' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_MP42) then result := result+'MS-MPEG4' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_DIVX) then result := result+'DIVX' else
    if IsEqualGUID(MediaType.subtype,MEDIASUBTYPE_VOXWARE) then result := result+'VOXWARE_MetaSound' else
       result := result+'UnKnown ';

  // format
    result := result+ ' Format: ';
    if IsEqualGUID(MediaType.formattype,FORMAT_VideoInfo) then
    begin
      result := result+'VideoInfo ';
      if ((MediaType.cbFormat > 0) and assigned(MediaType.pbFormat)) then
      with PVideoInfoHeader(MediaType.pbFormat)^.bmiHeader do
      result := result + format('%s %dX%d, %d bits',
        [GetFOURCC(biCompression), biWidth, biHeight, biBitCount]);
    end
    else
    begin
      if IsEqualGUID(MediaType.formattype,FORMAT_VideoInfo2) then
      begin
        result := result+'VideoInfo2 ';
        if ((MediaType.cbFormat > 0) and assigned(MediaType.pbFormat)) then
        with PVideoInfoHeader2(MediaType.pbFormat)^.bmiHeader do
        result := result + format('%s %dX%d, %d bits',
          [GetFOURCC(biCompression), biWidth, biHeight, biBitCount]);
      end
      else
      begin
        if IsEqualGUID(MediaType.formattype,FORMAT_WaveFormatEx) then
        begin
          result := result+'WaveFormatEx: ';
          if ((MediaType.cbFormat > 0) and assigned(MediaType.pbFormat)) then
          begin
            case PWaveFormatEx(MediaType.pbFormat)^.wFormatTag of
              $0001: result := result+'PCM';  // common
              $0002: result := result+'ADPCM';
              $0003: result := result+'IEEE_FLOAT';
              $0005: result := result+'IBM_CVSD';
              $0006: result := result+'ALAW';
              $0007: result := result+'MULAW';
              $0010: result := result+'OKI_ADPCM';
              $0011: result := result+'DVI_ADPCM';
              $0012: result := result+'MEDIASPACE_ADPCM';
              $0013: result := result+'SIERRA_ADPCM';
              $0014: result := result+'G723_ADPCM';
              $0015: result := result+'DIGISTD';
              $0016: result := result+'DIGIFIX';
              $0017: result := result+'DIALOGIC_OKI_ADPCM';
              $0018: result := result+'MEDIAVISION_ADPCM';
              $0020: result := result+'YAMAHA_ADPCM';
              $0021: result := result+'SONARC';
              $0022: result := result+'DSPGROUP_TRUESPEECH';
              $0023: result := result+'ECHOSC1';
              $0024: result := result+'AUDIOFILE_AF36';
              $0025: result := result+'APTX';
              $0026: result := result+'AUDIOFILE_AF10';
              $0030: result := result+'DOLBY_AC2';
              $0031: result := result+'GSM610';
              $0032: result := result+'MSNAUDIO';
              $0033: result := result+'ANTEX_ADPCME';
              $0034: result := result+'CONTROL_RES_VQLPC';
              $0035: result := result+'DIGIREAL';
              $0036: result := result+'DIGIADPCM';
              $0037: result := result+'CONTROL_RES_CR10';
              $0038: result := result+'NMS_VBXADPCM';
              $0039: result := result+'CS_IMAADPCM';
              $003A: result := result+'ECHOSC3';
              $003B: result := result+'ROCKWELL_ADPCM';
              $003C: result := result+'ROCKWELL_DIGITALK';
              $003D: result := result+'XEBEC';
              $0040: result := result+'G721_ADPCM';
              $0041: result := result+'G728_CELP';
              $0050: result := result+'MPEG';
              $0055: result := result+'MPEGLAYER3';
              $0060: result := result+'CIRRUS';
              $0061: result := result+'ESPCM';
              $0062: result := result+'VOXWARE';
              $0063: result := result+'CANOPUS_ATRAC';
              $0064: result := result+'G726_ADPCM';
              $0065: result := result+'G722_ADPCM';
              $0066: result := result+'DSAT';
              $0067: result := result+'DSAT_DISPLAY';
              $0075: result := result+'VOXWARE'; // aditionnal  ???
              $0080: result := result+'SOFTSOUND';
              $0100: result := result+'RHETOREX_ADPCM';
              $0200: result := result+'CREATIVE_ADPCM';
              $0202: result := result+'CREATIVE_FASTSPEECH8';
              $0203: result := result+'CREATIVE_FASTSPEECH10';
              $0220: result := result+'QUARTERDECK';
              $0300: result := result+'FM_TOWNS_SND';
              $0400: result := result+'BTV_DIGITAL';
              $1000: result := result+'OLIGSM';
              $1001: result := result+'OLIADPCM';
              $1002: result := result+'OLICELP';
              $1003: result := result+'OLISBC';
              $1004: result := result+'OLIOPR';
              $1100: result := result+'LH_CODEC';
              $1400: result := result+'NORRIS';
            else
              result := result+'Unknown';
            end;

            with PWaveFormatEx(MediaType.pbFormat)^ do
            result := result + format(', %d Hertz, %d Bits, %d Channels',
              [nSamplesPerSec, wBitsPerSample, nChannels]);
          end;
        end
        else
        begin
          if IsEqualGUID(MediaType.formattype,FORMAT_MPEGVideo) then
          begin
            result := result+'MPEGVideo ';
            if ((MediaType.cbFormat > 0) and assigned(MediaType.pbFormat)) then
            with PMPEG1VIDEOINFO(MediaType.pbFormat)^.hdr.bmiHeader do
              result := result + format('%s %dX%d, %d bits',
              [GetFOURCC(biCompression), biWidth, biHeight, biBitCount]);

          end
          else
          begin
            if IsEqualGUID(MediaType.formattype,FORMAT_MPEG2Video) then
            begin
              result := result+'MPEGStreams ';
              if ((MediaType.cbFormat > 0) and assigned(MediaType.pbFormat)) then
              with PMPEG2VIDEOINFO(MediaType.pbFormat)^.hdr.bmiHeader do
                result := result + format('%s %dX%d, %d bits',
                [GetFOURCC(biCompression), biWidth, biHeight, biBitCount]);
            end
            else
            begin  // todo
              if IsEqualGUID(MediaType.formattype,FORMAT_DvInfo)        then result := result+'DvInfo' else
              if IsEqualGUID(MediaType.formattype,FORMAT_MPEGStreams)   then result := result+'MPEGStreams' else
              if IsEqualGUID(MediaType.formattype,FORMAT_DolbyAC3)      then result := result+'DolbyAC3' else
              if IsEqualGUID(MediaType.formattype,FORMAT_MPEG2Audio)    then result := result+'MPEG2Audio' else
              if IsEqualGUID(MediaType.formattype,FORMAT_DVD_LPCMAudio) then result := result+'DVD_LPCMAudio' else
                result := result+'Unknown';
            end;
          end;
        end;
      end;
    end;
  end;

  function ShowFilterPropertyPage(parent: THandle; Filter: IBaseFilter;
    PropertyPage: TPropertyPage = ppDefault): HRESULT;
  var
    SpecifyPropertyPages : ISpecifyPropertyPages;
    CaptureDialog : IAMVfwCaptureDialogs;
    CompressDialog: IAMVfwCompressDialogs;
    CAGUID  :TCAGUID;
    FilterInfo: TFilterInfo;
    Code: Integer;
  begin
    result := S_FALSE;
    code := 0;
    if Filter = nil then exit;

    ZeroMemory(@FilterInfo, SizeOf(TFilterInfo));

    case PropertyPage of
      ppVFWCapDisplay: code := VfwCaptureDialog_Display;
      ppVFWCapFormat : code := VfwCaptureDialog_Format;
      ppVFWCapSource : code := VfwCaptureDialog_Source;
      ppVFWCompConfig: code := VfwCompressDialog_Config;
      ppVFWCompAbout : code := VfwCompressDialog_About;
    end;

    case PropertyPage of
      ppDefault:
        begin
          result := Filter.QueryInterface(IID_ISpecifyPropertyPages, SpecifyPropertyPages);
          if result <> S_OK then exit;
          result := SpecifyPropertyPages.GetPages(CAGUID);
          if result <> S_OK then exit;
          result := Filter.QueryFilterInfo(FilterInfo);
          if result = S_OK then
          begin
            result := OleCreatePropertyFrame(parent, 0, 0, FilterInfo.achName, 1, @Filter, CAGUID.cElems, CAGUID.pElems, 0, 0, nil );
            FilterInfo.pGraph := nil;
          end;
          if Assigned(CAGUID.pElems) then CoTaskMemFree(CAGUID.pElems);
          SpecifyPropertyPages := nil;
        end;
      ppVFWCapDisplay..ppVFWCapSource:
        begin
          result := Filter.QueryInterface(IID_IAMVfwCaptureDialogs,CaptureDialog);
          if (result <> S_OK) then exit;
          result := CaptureDialog.HasDialog(code);
          if result <> S_OK then exit;
          result := CaptureDialog.ShowDialog(code,parent);
          CaptureDialog := nil;
        end;
      ppVFWCompConfig..ppVFWCompAbout:
        begin
          result := Filter.QueryInterface(IID_IAMVfwCompressDialogs, CompressDialog);
          if (result <> S_OK) then exit;
          case PropertyPage of
            ppVFWCompConfig: result := CompressDialog.ShowDialog(VfwCompressDialog_QueryConfig, 0);
            ppVFWCompAbout : result := CompressDialog.ShowDialog(VfwCompressDialog_QueryAbout, 0);
          end;
          if result = S_OK then result := CompressDialog.ShowDialog(code,parent);
          CompressDialog := nil;
        end;
    end;
  end;

  function HaveFilterPropertyPage(Filter: IBaseFilter;
    PropertyPage: TPropertyPage = ppDefault): boolean;
  var
    SpecifyPropertyPages : ISpecifyPropertyPages;
    CaptureDialog : IAMVfwCaptureDialogs;
    CompressDialog: IAMVfwCompressDialogs;
    Code: Integer;
    HR: HRESULT;
  begin
    result := false;
    code := 0;
    if Filter = nil then exit;

    case PropertyPage of
      ppVFWCapDisplay: code := VfwCaptureDialog_Display;
      ppVFWCapFormat : code := VfwCaptureDialog_Format;
      ppVFWCapSource : code := VfwCaptureDialog_Source;
      ppVFWCompConfig: code := VfwCompressDialog_QueryConfig;
      ppVFWCompAbout : code := VfwCompressDialog_QueryAbout;
    end;

    case PropertyPage of
      ppDefault:
      begin
        result := Succeeded(Filter.QueryInterface(IID_ISpecifyPropertyPages, SpecifyPropertyPages));
        SpecifyPropertyPages := nil;
      end;
      ppVFWCapDisplay..ppVFWCapSource:
        begin
          HR := Filter.QueryInterface(IID_IAMVfwCaptureDialogs,CaptureDialog);
          if (HR <> S_OK) then exit;
          result := Succeeded(CaptureDialog.HasDialog(code));
          CaptureDialog := nil;
        end;
      ppVFWCompConfig..ppVFWCompAbout:
        begin
          HR := Filter.QueryInterface(IID_IAMVfwCompressDialogs, CompressDialog);
          if (HR <> S_OK) then exit;
          result := Succeeded(CompressDialog.ShowDialog(code,0));
          CompressDialog := nil;
        end;
    end;
  end;

  function ShowPinPropertyPage(parent: THandle; Pin: IPin): HRESULT;
  var
    SpecifyPropertyPages: ISpecifyPropertyPages;
    CAGUID :TCAGUID;
    PinInfo: TPinInfo;
  begin
    result := S_FALSE;
    if Pin = nil then exit;
    result := Pin.QueryInterface(IID_ISpecifyPropertyPages, SpecifyPropertyPages);
    if result <> S_OK then exit;
    result := SpecifyPropertyPages.GetPages(CAGUID);
    if result <> S_OK then
    begin
      SpecifyPropertyPages := nil;
      Exit;
    end;
    result := Pin.QueryPinInfo(PinInfo);
    if result <> S_OK then exit;
    try
      result := OleCreatePropertyFrame(parent, 0, 0, PinInfo.achName, 1, @Pin,
                                       CAGUID.cElems, CAGUID.pElems, 0, 0, nil);
    finally
      CoTaskMemFree(CAGUID.pElems);
      PinInfo.pFilter := nil;
    end;
    SpecifyPropertyPages := nil;
  end;

  function RefTimeToMiliSec(RefTime: int64): Cardinal;
  begin
    result := Cardinal(RefTime div 10000);
  end;

  function MiliSecToRefTime(Milisec: int64): Int64;
  begin
    result := Milisec * 10000;
  end;

// The mechanism for describing a bitmap format is with the BITMAPINFOHEADER
// This is really messy to deal with because it invariably has fields that
// follow it holding bit fields, palettes and the rest. This function gives
// the number of bytes required to hold a VIDEOINFO that represents it. This
// count includes the prefix information (like the rcSource rectangle) the
// BITMAPINFOHEADER field, and any other colour information on the end.
//
// WARNING If you want to copy a BITMAPINFOHEADER into a VIDEOINFO always make
// sure that you use the HEADER macro because the BITMAPINFOHEADER field isn't
// right at the start of the VIDEOINFO (there are a number of other fields),
//
//     CopyMemory(HEADER(pVideoInfo),pbmi,sizeof(BITMAPINFOHEADER));
//

  function GetBitmapFormatSize(const Header: TBitmapInfoHeader): Integer;
  var Size, Entries: Integer;
  begin
    // Everyone has this to start with this
    Size := SIZE_PREHEADER + Header.biSize;

    ASSERT(Header.biSize >= sizeof(TBitmapInfoHeader));
    
    // Does this format use a palette, if the number of colours actually used
    // is zero then it is set to the maximum that are allowed for that colour
    // depth (an example is 256 for eight bits). Truecolour formats may also
    // pass a palette with them in which case the used count is non zero

    // This would scare me.
    ASSERT((Header.biBitCount <= iPALETTE) or (Header.biClrUsed = 0));

    if ((Header.biBitCount <= iPALETTE) or BOOL(Header.biClrUsed)) then
    begin
        Entries := DWORD(1) shl Header.biBitCount;
        if BOOL(Header.biClrUsed) then Entries := Header.biClrUsed;
        Size := Size + Entries * sizeof(RGBQUAD);
    end;

    // Truecolour formats may have a BI_BITFIELDS specifier for compression
    // type which means that room for three DWORDs should be allocated that
    // specify where in each pixel the RGB colour components may be found

    if (Header.biCompression = BI_BITFIELDS) then Size := Size + SIZE_MASKS;
    result := Size;
  end;


  function GetSourceRectFromMediaType(const MediaType: TAMMediaType): TRect;
    function GetbmiHeader(const MediaType: TAMMediaType): PBitmapInfoHeader;
    begin
      result := nil;
      if MediaType.pbFormat = nil then exit;
      if (IsEqualGUID(MediaType.formattype, FORMAT_VideoInfo) and
          (MediaType.cbFormat >= sizeof(TVIDEOINFOHEADER))) then
        result := @PVIDEOINFOHEADER(MediaType.pbFormat)^.bmiHeader
      else if (IsEqualGUID(MediaType.formattype, FORMAT_VideoInfo2) and
               (MediaType.cbFormat >= sizeof(TVIDEOINFOHEADER2))) then
        result := @PVIDEOINFOHEADER2(MediaType.pbFormat)^.bmiHeader;
    end;
  var bih: PBITMAPINFOHEADER;
  begin
    ZeroMemory(@Result,SizeOf(TRect));
    if MediaType.pbFormat = nil then exit;
    if (IsEqualGUID(MediaType.formattype, FORMAT_VideoInfo) and
        (MediaType.cbFormat >= sizeof(TVIDEOINFOHEADER))) then
      result := PVideoInfoHeader(MediaType.pbFormat)^.rcSource
    else if (IsEqualGUID(MediaType.formattype, FORMAT_VideoInfo2) and
             (MediaType.cbFormat >= sizeof(TVIDEOINFOHEADER2))) then
      result := PVIDEOINFOHEADER2(MediaType.pbFormat)^.rcSource;
    if IsRectEmpty(result) then
    begin
      bih := GetbmiHeader(MediaType);
      if bih <> nil then
        SetRect(result, 0, 0, abs(bih.biWidth), abs(bih.biHeight));
    end;
  end;

  function StretchRect(R, IR: TRect): TRect;
  var
    iW, iH: Integer;
    rW, rH: Integer;
  begin
    iW := IR.Right - IR.Left;
    iH := IR.Bottom - IR.Top;
    rW := R.Right - R.Left;
    rH := R.Bottom - R.Top;
    if (rW / iW) < (rH / iH) then
      begin
        iH := MulDiv(iH, rW, iW);
        iW := MulDiv(iW, rW, iW);
      end
    else
      begin
        iW := MulDiv(iW, rH, iH);
        iH := MulDiv(iH, rH, iH);
      end;
    SetRect(Result, 0, 0, iW, iH);
    OffsetRect(Result, R.Left + (rW - iW) div 2, R.Top + (rH - iH) div 2);
  end;

  function CheckDSError(HR: HRESULT): HRESULT;
  var Excep: EDirectShowException;
  begin
    Result := HR;
    if Failed(HR) then
    begin
      Excep := EDirectShowException.Create(format(GetErrorString(HR)+' ($%x).',[HR]));
      Excep.ErrorCode := HR;
      raise Excep;
    end;
  end;


  //-----------------------------------------------------------------------------
  // Name: DXUtil_GetDXSDKMediaPath()
  // Desc: Returns the DirectX SDK media path
  //-----------------------------------------------------------------------------
  function GetDXSDKMediaPath : String;
  var
    strPath : array[1..MAX_PATH+10] of Char;
    Key : HKey;
    dwType, dwSize : DWord;
    lr : Longint;

  begin
    Key := 0;
    dwType := 0;
    dwSize := MAX_PATH;

    // Initialize to NULL
    strPath[1] := #0;

    // Open the appropriate registry key
    lr := RegOpenKeyEx(HKEY_LOCAL_MACHINE, 'Software\Microsoft\DirectX SDK',
      0, KEY_READ, Key);
    if(ERROR_SUCCESS <> lr) then
    begin
      Result := '';
      Exit;
    end;

    lr := RegQueryValueEx(Key, 'DX9SDK Samples Path', nil,
      @dwType, @strPath, @dwSize);

    if (ERROR_SUCCESS <> lr) then
    begin
      // Reset size field
      dwSize := MAX_PATH;
      lr := RegQueryValueEx(Key, 'DX81SDK Samples Path', nil,
        @dwType, @strPath, @dwSize);

      if (ERROR_SUCCESS <> lr) then
      begin
        // Reset size field
        dwSize := MAX_PATH;
        lr := RegQueryValueEx(Key, 'DX8SDK Samples Path', nil,
          @dwType, @strPath, @dwSize);

        if (ERROR_SUCCESS <> lr) then
        begin
          RegCloseKey(Key);
          Result := '';
          Exit;
        end;
      end;
    end;

    RegCloseKey(Key);
    Result := PChar(@strPath);
    Result := Result + '\Media\';
  end;

  function CopyScreenToBitmap(Rect : TRect; pData : PByte;
    pHeader : PBitmapInfo) : HBitmap;
  var
    // screen DC and memory DC
    hScrDC, hMemDC : HDC;
    // handles to deice-dependent bitmaps
    hBmp, hOldBmp : HBitmap;
     // coordinates of rectangle to grab
    nX, nY, nX2, nY2,
    // DIB width and height
    nWidth, nHeight,
    // screen resolution
    xScrn, yScrn : Integer;

  begin
    // check for an empty rectangle
    if IsRectEmpty(Rect) then
    begin
      Result := 0;
      Exit;
    end;

    // create a DC for the screen and create
    // a memory DC compatible to screen DC
    hScrDC := CreateDC('DISPLAY', nil, nil, nil);
    hMemDC := CreateCompatibleDC(hScrDC);

    // get points of rectangle to grab
    nX := Rect.Left;
    nY := Rect.Top;
    nX2:= Rect.Right;
    nY2:= Rect.Bottom;

    // get screen resolution
    xScrn := GetDeviceCaps(hScrDC, HORZRES);
    yScrn := GetDeviceCaps(hScrDC, VERTRES);

    //make sure bitmap rectangle is visible
    if (nX < 0) then
      nX := 0;
    if (nY < 0) then
      nY := 0;
    if (nX2 > xScrn) then
      nX2 := xScrn;
    if (nY2 > yScrn) then
      nY2 := yScrn;

    nWidth  := nX2 - nX;
    nHeight := nY2 - nY;

    // create a bitmap compatible with the screen DC
    hBmp := CreateCompatibleBitmap(hScrDC, nWidth, nHeight);

    // select new bitmap into memory DC
    hOldBmp := SelectObject(hMemDC, hBmp);

    // bitblt screen DC to memory DC
    BitBlt(hMemDC, 0, 0, nWidth, nHeight, hScrDC, nX, nY, SRCCOPY);

    // select old bitmap back into memory DC and get handle to
    // bitmap of the screen
    hBmp := SelectObject(hMemDC, hOldBmp);

    // Copy the bitmap data into the provided BYTE buffer
    GetDIBits(hScrDC, hBmp, 0, nHeight, pData, pHeader^, DIB_RGB_COLORS);

    // clean up
    DeleteDC(hScrDC);
    DeleteDC(hMemDC);

    // return handle to the bitmap
    Result := hBmp;
  end;


// *****************************************************************************
//  TSysDevEnum
// *****************************************************************************

  procedure TSysDevEnum.GetCat(catlist: TList; CatGUID: TGUID);
  var
    SysDevEnum : ICreateDevEnum;
    EnumCat    : IEnumMoniker;
    Moniker    : IMoniker;
    Fetched    : ULONG;
    PropBag    : IPropertyBag;
    Name       : olevariant;
    hr         : HRESULT;
    i          : integer;
  begin
    if catList.Count > 0 then
      for i := 0 to (catList.Count - 1) do if assigned(catList.Items[i]) then Dispose(catList.Items[i]);
    catList.Clear;
    CocreateInstance(CLSID_SystemDeviceEnum, nil, CLSCTX_INPROC, IID_ICreateDevEnum, SysDevEnum);
    hr := SysDevEnum.CreateClassEnumerator(CatGUID, EnumCat, 0);
    if (hr = S_OK) then
    begin
      while(EnumCat.Next(1, Moniker, @Fetched) = S_OK) do
        begin
          Moniker.BindToStorage(nil, nil, IID_IPropertyBag, PropBag);
          new(ACategory);
          PropBag.Read('FriendlyName', Name, nil);
          ACategory^.FriendlyName := Name;
          if (PropBag.Read('CLSID',Name,nil) = S_OK) then
            ACategory^.CLSID := StringToGUID(Name)
          else
            ACategory^.CLSID := GUID_NULL;
          catlist.Add(ACategory);
          PropBag := nil;
          Moniker := nil;
        end;
    end;
    EnumCat :=nil;
    SysDevEnum :=nil;
  end;

  constructor TSysDevEnum.Create;
  begin
    FCategories := TList.Create;
    FFilters    := TList.Create;
    getcat(FCategories,CLSID_ActiveMovieCategories);
  end;

  constructor TSysDevEnum.create(guid: TGUID);
  begin
    FCategories := TList.Create;
    FFilters    := TList.Create;
    getcat(FCategories,CLSID_ActiveMovieCategories);
    SelectGUIDCategory(guid);
  end;

  destructor TSysDevEnum.Destroy;
  var i: integer;
  begin
    inherited Destroy;
    if FCategories.Count > 0 then
      for i := 0 to (FCategories.Count - 1) do
        if assigned(FCategories.Items[i]) then Dispose(FCategories.items[i]);
    FCategories.Clear;
    FreeAndNil(FCategories);
    if FFilters.Count > 0 then
      for i := 0 to (FFilters.Count - 1) do
        if assigned(FFilters.Items[i]) then Dispose(FFilters.Items[i]);
    FFilters.Clear;
    FreeAndNil(FFilters);
  end;

  function TSysDevEnum.GetCategory(item: integer): TFilCatNode;
  var PCategory: PFilCatNode;
  begin
    PCategory := FCategories.Items[item];
    result := PCategory^;
  end;

  function TSysDevEnum.GetFilter(item: integer): TFilCatNode;
  var PCategory: PFilCatNode;
  begin
    PCategory := FFilters.Items[item];
    result := PCategory^;
  end;

  function TSysDevEnum.GetCountCategories: integer;
  begin
    result := FCategories.Count;
  end;

  function TSysDevEnum.GetCountFilters: integer;
  begin
    result := FFilters.Count;
  end;

  // Find filter index by FriendlyName; -1, if not found
  function TSysDevEnum.FilterIndexOfFriendlyName(const FriendlyName: string): Integer;
  begin
    Result := FFilters.Count - 1;
    while (Result >= 0) and
          (AnsiCompareText(PFilCatNode(FFilters.Items[Result])^.FriendlyName, FriendlyName) <> 0) do
      Dec(Result);
  end;

  procedure TSysDevEnum.SelectGUIDCategory(GUID: TGUID);
  begin
    FGUID := GUID;
    getcat(FFilters,FGUID);
  end;

  procedure TSysDevEnum.SelectIndexCategory(index: integer);
  begin
    SelectGUIDCategory(Categories[index].CLSID);
  end;

  function TSysDevEnum.GetMoniker(index: integer): IMoniker;
  var
    SysDevEnum  : ICreateDevEnum;
    EnumCat     : IEnumMoniker;
  begin
    result := nil;
   if ((index < CountFilters) and (index >= 0)) then
      begin
        CocreateInstance(CLSID_SystemDeviceEnum, nil, CLSCTX_INPROC, IID_ICreateDevEnum, SysDevEnum);
        SysDevEnum.CreateClassEnumerator(FGUID, EnumCat, 0);
        EnumCat.Skip(index);
        EnumCat.Next(1, Result, nil);
        EnumCat.Reset;
        SysDevEnum := nil;
        EnumCat    := nil;
      end
  end;

  function TSysDevEnum.GetBaseFilter(index: integer): IBaseFilter;
  var
    SysDevEnum  : ICreateDevEnum;
    EnumCat     : IEnumMoniker;
    Moniker     : IMoniker;
  begin
    result := nil;
   if ((index < CountFilters) and (index >= 0)) then
      begin
        CocreateInstance(CLSID_SystemDeviceEnum, nil, CLSCTX_INPROC, IID_ICreateDevEnum, SysDevEnum);
        SysDevEnum.CreateClassEnumerator(FGUID, EnumCat, 0);
        EnumCat.Skip(index);
        EnumCat.Next(1, Moniker, nil);
        Moniker.BindToObject(nil, nil, IID_IBaseFilter, result);
        EnumCat.Reset;
        SysDevEnum := nil;
        EnumCat    := nil;
        Moniker    := nil;
      end
  end;

  function TSysDevEnum.GetBaseFilter(GUID: TGUID): IBaseFilter;
  var
    i: integer;
  begin
    result := nil;
    if countFilters > 0 then
    for i := 0 to CountFilters - 1 do
      if IsEqualGUID(GUID,Filters[i].CLSID) then
      begin
        result := GetBaseFilter(i);
        exit;
      end;
  end;

//******************************************************************************
//
//  TMediaType implementation
//
//******************************************************************************

  destructor TMediaType.Destroy;
  begin
    FreeMediaType(AMMediaType);
    dispose(AMMediaType);
    inherited Destroy;
  end;

  // copy constructor does a deep copy of the format block

  constructor TMediaType.Create;
  begin
    InitMediaType;
  end;

  constructor TMediaType.Create(majortype: TGUID);
  begin
    InitMediaType;
    AMMediaType.majortype := majortype;
  end;

  constructor TMediaType.Create(mediatype: PAMMediaType);
  begin
    InitMediaType;
    CopyMediaType(AMMediaType, mediatype);
  end;

  constructor TMediaType.Create(MTClass: TMediaType);
  begin
    InitMediaType;
    CopyMediaType(AMMediaType, MTClass.AMMediaType);
  end;

  procedure TMediaType.DefineProperties(Filer: TFiler);
    function DoWrite: Boolean;
    begin
      result := true;
      if Filer.Ancestor <> nil then
      begin
        Result := True;
        if Filer.Ancestor is TMediaType then
          Result := not Equal(TMediaType(Filer.Ancestor))
      end;
    end;
  begin
    Filer.DefineBinaryProperty('data', ReadData, WriteData, DoWrite);
  end;

  procedure TMediaType.ReadData(Stream: TStream);
  begin
    ResetFormatBuffer;
    Stream.Read(AMMediaType^, SizeOf(TAMMediaType));
    if FormatLength > 0 then
    begin
      AMMediaType.pbFormat := CoTaskMemAlloc(FormatLength);
      Stream.Read(AMMediaType.pbFormat^, FormatLength)
    end;
  end;

  procedure TMediaType.WriteData(Stream: TStream);
  begin
    Stream.Write(AMMediaType^, SizeOf(TAMMediaType));
    if FormatLength > 0 then
      Stream.Write(AMMediaType.pbFormat^, FormatLength);
  end;

  // copy MTClass.AMMediaType to current AMMediaType
  procedure TMediaType.Assign(Source: TPersistent);
  begin
    if Source is TMediaType then
    begin
      if (Source <> self) then
      begin
        FreeMediaType(AMMediaType);
        CopyMediaType(AMMediaType, TMediaType(Source).AMMediaType);
      end;
    end
    else
      inherited Assign(Source);
  end;

  // this class inherits publicly from AM_MEDIA_TYPE so the compiler could generate
  // the following assignment operator itself, however it could introduce some
  // memory conflicts and leaks in the process because the structure contains
  // a dynamically allocated block (pbFormat) which it will not copy correctly
  procedure TMediaType.Read(mediatype: PAMMediaType);
  begin
    if (mediatype <> self.AMMediaType) then
    begin
      FreeMediaType(AMMediaType);
      CopyMediaType(AMMediaType, mediatype);
    end;
  end;

  function TMediaType.Equal(MTClass: TMediaType): boolean;
  begin
    // I don't believe we need to check sample size or
    // temporal compression flags, since I think these must
    // be represented in the type, subtype and format somehow. They
    // are pulled out as separate flags so that people who don't understand
    // the particular format representation can still see them, but
    // they should duplicate information in the format block.
    result := ((IsEqualGUID(AMMediaType.majortype,MTClass.AMMediaType.majortype) = TRUE) and
        (IsEqualGUID(AMMediaType.subtype,MTClass.AMMediaType.subtype) = TRUE) and
        (IsEqualGUID(AMMediaType.formattype,MTClass.AMMediaType.formattype) = TRUE) and
        (AMMediaType.cbFormat = MTClass.AMMediaType.cbFormat) and
        ( (AMMediaType.cbFormat = 0) or
          (CompareMem(AMMediaType.pbFormat, MTClass.AMMediaType.pbFormat, AMMediaType.cbFormat))));
  end;

  // Check to see if they are equal
  function TMediaType.NotEqual(MTClass: TMediaType): boolean;
  begin
    if (self = MTClass) then
     result := FALSE
    else
     result := TRUE;
  end;

  // By default, TDSMediaType objects are initialized with a major type of GUID_NULL.
  // Call this method to determine whether the object has been correctly initialized.
  function TMediaType.IsValid: boolean;
  begin
    result := not IsEqualGUID(AMMediaType.majortype,GUID_NULL);
  end;

  // Determines if the samples have a fixed size or a variable size.
  function TMediaType.IsFixedSize: boolean;
  begin
    result := AMMediaType.bFixedSizeSamples;
  end;

  // Determines if the stream uses temporal compression.
  function TMediaType.IsTemporalCompressed: boolean;
  begin
    result := AMMediaType.bTemporalCompression;
  end;

  // If the sample size is fixed, returns the sample size in bytes. Otherwise,
  // returns zero.
  function TMediaType.GetSampleSize: ULONG;
  begin
    if IsFixedSize then
      result := AMMediaType.lSampleSize
    else
      result := 0;
  end;

  // If value of sz is zero, the media type uses variable sample sizes. Otherwise,
  // the sample size is fixed at sz bytes.
  procedure TMediaType.SetSampleSize(SZ: ULONG);
  begin
    if (sz = 0) then
    begin
      SetVariableSize;
    end
    else
    begin
      AMMediaType.bFixedSizeSamples := TRUE;
      AMMediaType.lSampleSize := sz;
    end;
  end;

  // Specifies that samples do not have a fixed size.
  procedure TMediaType.SetVariableSize;
  begin
    AMMediaType.bFixedSizeSamples := FALSE;
  end;

  // Specifies whether samples are compressed using temporal compression
  procedure TMediaType.SetTemporalCompression(bCompressed: boolean);
  begin
    AMMediaType.bTemporalCompression := bCompressed;
  end;

  // Retrieves a pointer to the format block.
  function TMediaType.Format: pointer;
  begin
    result := AMMediaType.pbFormat;
  end;

  //Retrieves the length of the format block.
  function TMediaType.FormatLength: ULONG;
  begin
    result := AMMediaType.cbFormat;
  end;

  function TMediaType.SetFormat(pFormat: pointer; length: ULONG): boolean;
  begin
    if (nil = AllocFormatBuffer(length)) then
    begin
       result := false;
       exit;
    end;
    ASSERT(AMMediatype.pbFormat<>nil);
    CopyMemory(AMMediatype.pbFormat,pFormat,length);
    result := true;
  end;

  // reset the format buffer
  procedure TMediaType.ResetFormatBuffer;
  begin
    if (AMMediaType.cbFormat <> 0) then
      CoTaskMemFree(AMMediaType.pbFormat);
    AMMediaType.cbFormat := 0;
    AMMediaType.pbFormat := nil;
  end;

  // allocate length bytes for the format and return a read/write pointer
  // If we cannot allocate the new block of memory we return NULL leaving
  // the original block of memory untouched (as does ReallocFormatBuffer)
  function TMediaType.AllocFormatBuffer(length: ULONG): pointer;
  var pNewFormat : pointer;
  begin
    ASSERT(length<>0);

    // do the types have the same buffer size
    if (AMMediaType.cbFormat = length) then
    begin
      result := AMMediaType.pbFormat;
      exit;
    end;

    // allocate the new format buffer
    pNewFormat := CoTaskMemAlloc(length);
    if (pNewFormat = nil) then
    begin
      if (length <= AMMediaType.cbFormat) then
      begin
        result :=  AMMediatype.pbFormat; //reuse the old block anyway.
        exit;
      end
      else
      begin
        result := nil;
        exit;
      end;
    end;

    // delete the old format
    if (AMMediaType.cbFormat <> 0) then
    begin
      ASSERT(AMMediaType.pbFormat<>nil);
      CoTaskMemFree(AMMediaType.pbFormat);
    end;

    AMMediaType.cbFormat := length;
    AMMediaType.pbFormat := pNewFormat;
    result := AMMediaType.pbFormat;
  end;

  // reallocate length bytes for the format and return a read/write pointer
  // to it. We keep as much information as we can given the new buffer size
  // if this fails the original format buffer is left untouched. The caller
  // is responsible for ensuring the size of memory required is non zero
  function TMediaType.ReallocFormatBuffer(length: ULONG): pointer;
  var pNewFormat: pointer;
  begin
    ASSERT(length<>0);

    // do the types have the same buffer size
    if (AMMediaType.cbFormat = length) then
    begin
      result := AMMediaType.pbFormat;
      exit;
    end;

    // allocate the new format buffer
    pNewFormat := CoTaskMemAlloc(length);
    if (pNewFormat = nil) then
    begin
      if (length <= AMMediaType.cbFormat) then
      begin
        result := AMMediaType.pbFormat; //reuse the old block anyway.
        exit;
      end
      else
      begin
        result := nil;
        exit;
      end;
    end;

    // copy any previous format (or part of if new is smaller)
    // delete the old format and replace with the new one
    if (AMMediaType.cbFormat <> 0) then
    begin
      ASSERT(AMMediaType.pbFormat<>nil);
      CopyMemory(pNewFormat, AMMediaType.pbFormat, min(length,AMMediaType.cbFormat));
      CoTaskMemFree(AMMediaType.pbFormat);
    end;

    AMMediaType.cbFormat := length;
    AMMediaType.pbFormat := pNewFormat;
    result := pNewFormat;
  end;

  // initialise a media type structure
  procedure TMediaType.InitMediaType;
  begin
    new(AMMediaType);
    ZeroMemory(AMMediaType, sizeof(TAMMediaType));
    AMMediaType.lSampleSize := 1;
    AMMediaType.bFixedSizeSamples := TRUE;
  end;

  //Determines if this media type matches a partially specified media type.
  function TMediaType.MatchesPartial(ppartial: TMediaType): boolean;
  begin
    if (not IsEqualGUID(ppartial.AMMediaType.majortype, GUID_NULL) and
        not IsEqualGUID(AMMediaType.majortype, ppartial.AMMediaType.majortype)) then
    begin
      result := false;
      exit;
    end;
    if (not IsEqualGUID(ppartial.AMMediaType.subtype, GUID_NULL) and
        not IsEqualGUID(AMMediaType.subtype, ppartial.AMMediaType.subtype)) then
    begin
      result := false;
      exit;
    end;

    if not IsEqualGUID(ppartial.AMMediaType.formattype, GUID_NULL) then
    begin
      // if the format block is specified then it must match exactly
      if not IsEqualGUID(AMMediaType.formattype, ppartial.AMMediaType.formattype) then
      begin
        result := FALSE;
        exit;
      end;
      if (AMMediaType.cbFormat <> ppartial.AMMediaType.cbFormat) then
      begin
        result := FALSE;
        exit;
      end;
        if ((AMMediaType.cbFormat <> 0) and
            (CompareMem(AMMediaType.pbFormat, ppartial.AMMediaType.pbFormat, AMMediaType.cbFormat) <> false)) then
        begin
          result := FALSE;
          exit;
        end;
    end;
    result := TRUE;
  end;

  // a partially specified media type can be passed to IPin::Connect
  // as a constraint on the media type used in the connection.
  // the type, subtype or format type can be null.
  function TMediaType.IsPartiallySpecified: boolean;
  begin
    if (IsEqualGUID(AMMediaType.majortype, GUID_NULL) or
        IsEqualGUID(AMMediaType.formattype, GUID_NULL)) then
    begin
      result := TRUE;
      exit;
    end
    else
    begin
      result := FALSE;
      exit;
    end;
  end;

  function TMediaType.GetMajorType: TGUID;
  begin
    result := AMMediaType.majortype;
  end;

  procedure TMediaType.SetMajorType(MT: TGUID);
  begin
    AMMediaType.majortype := MT;
  end;

  function TMediaType.GetSubType: TGUID;
  begin
    result := AMMediaType.subtype;
  end;

  procedure TMediaType.SetSubType(ST: TGUID);
  begin
    AMMediaType.subtype := ST;
  end;

  // set the type of the media type format block, this type defines what you
  // will actually find in the format pointer. For example FORMAT_VideoInfo or
  // FORMAT_WaveFormatEx. In the future this may be an interface pointer to a
  // property set. Before sending out media types this should be filled in.
  procedure TMediaType.SetFormatType(const GUID: TGUID);
  begin
    AMMediaType.formattype := GUID;
  end;

  function TMediaType.GetFormatType: TGUID;
  begin
    result := AMMediaType.formattype;
  end;

//******************************************************************************
//
//  TDSEnumMediaType Implementation
//
//******************************************************************************

  constructor TEnumMediaType.Create;
  begin
    FList      := TList.Create;
  end;

  constructor TEnumMediaType.Create(Pin: IPin);
  var EnumMT : IEnumMediaTypes;
      hr     : HRESULT;
  begin
    FList      := TList.Create;
    assert(pin <> nil,'IPin not assigned');
    hr := pin.EnumMediaTypes(EnumMT);
    if (hr <> S_OK) then exit;
    Create(ENumMT);
  end;

  constructor TEnumMediaType.Create(EnumMT: IEnumMediaTypes);
  var pmt: PAMMediaType;
  begin
    if (FList = nil) then FList := TList.Create;
    assert(EnumMT <> nil,'IEnumMediaType not assigned');
    while (EnumMT.Next(1,pmt,nil)= S_OK) do
    begin
      FList.Add(TMediaType.Create(pmt));
    end;
  end;

  constructor TEnumMediaType.Create(FileName: TFileName);
  begin
    FList := TList.Create;
    Assign(FileName);
  end;

  destructor TEnumMediaType.Destroy;
  begin
    Clear;
    FList.Free;
  end;

  procedure TEnumMediaType.Assign(Pin: IPin);
  var EnumMT : IEnumMediaTypes;
      hr     : HRESULT;
  begin
    Clear;
    assert(pin <> nil,'IPin not assigned');
    hr := pin.EnumMediaTypes(EnumMT);
    if (hr <> S_OK) then exit;
    Assign(ENumMT);
  end;

  procedure TEnumMediaType.Assign(EnumMT: IEnumMediaTypes);
  var pmt: PAMMediaType;
  begin
    if (count <> 0) then Clear;
    assert(EnumMT <> nil,'IEnumMediaType not assigned');
    while (EnumMT.Next(1,pmt,nil)= S_OK) do
    begin
      FList.Add(TMediaType.Create(pmt));
    end;
  end;

  procedure TEnumMediaType.Assign(FileName: TFileName);
  var
    MediaDet: IMediaDet;
    KeyProvider : IServiceProvider;
    hr: HRESULT;
    Streams: LongInt;
    i: longint;
    MediaType: TAMMediaType;
  begin
    Clear;
    hr := CoCreateInstance(CLSID_MediaDet, nil, CLSCTX_INPROC, IID_IMediaDet, MediaDet);
    // milenko start get rid of compiler warnings ...
    if (hr = S_OK) then
    begin
    end;
    // milenko end;
    assert(hr = S_OK, 'Media Detector not available');
    hr := MediaDet.put_Filename(FileName);
    if hr <> S_OK then
    begin
      MediaDet := nil;
      Exit;
    end;
    MediaDet.get_OutputStreams(Streams);
    if streams > 0 then
    begin
      for i := 0 to (streams - 1) do
      begin
        MediaDet.put_CurrentStream(i);
        MediaDet.get_StreamMediaType(MediaType);
        FList.Add(TMediaType.Create(@MediaType));
      end;
    end;
    KeyProvider := nil;
    MediaDet := nil;
  end;

  function TEnumMediaType.GetItem(Index: Integer): TMediaType;
  begin
    result := TMediaType(Flist.Items[index]);
  end;

  function TEnumMediaType.GetMediaDescription(Index: Integer): string;
  begin
    result := '';
    if ((index < count) and (index > -1)) then
      result := GetMediaTypeDescription(TMediaType(Flist.Items[index]).AMMediaType);
  end;

  procedure TEnumMediaType.SetItem(Index: Integer; Item: TMediaType);
  begin
    TMediaType(Flist.Items[index]).Assign(item);
  end;

  function TEnumMediaType.GetCount: integer;
  begin
    assert(FList<>nil,'TDSEnumMediaType not created');
    if (FList <> nil) then
      result := FList.Count
    else
      result := 0;
  end;

  function TEnumMediaType.Add(Item: TMediaType): Integer;
  begin
    result := FList.Add(Item);
  end;

  procedure TEnumMediaType.Clear;
  var i: Integer;
  begin
    if count <> 0 then
    for i := 0 to (count -1) do
    begin
      if (FList.Items[i]<>nil) then TMediaType(FList.Items[i]).Free;
    end;
    FList.Clear;
  end;

  procedure TEnumMediaType.Delete(Index: Integer);
  begin
    if (FList.Items[index]<>nil) then TMediaType(FList.Items[index]).Free;
    FList.Delete(index);
  end;

// *****************************************************************************
//  TDSFilterList implementation
// *****************************************************************************

  constructor TFilterList.Create(FilterGraph: IFilterGraph);
  begin
    inherited Create;
    Graph := FilterGraph;
    Update;
  end;

  destructor TFilterList.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TFilterList.Update;
  var EnumFilters: IEnumFilters;
      Filter: IBaseFilter;
  begin
    if assigned(Graph) then
    Graph.EnumFilters(EnumFilters);
    while (EnumFilters.Next(1, Filter, nil) = S_OK) do add(Filter);
    EnumFilters := nil;
  end;

  procedure TFilterList.Assign(FilterGraph: IFilterGraph);
  begin
    Clear;
    Graph := FilterGraph;
    Update;
  end;

  function TFilterList.GetFilter(Index: Integer): IBaseFilter;
  begin
    result := get(index) as IBaseFilter;
  end;

  procedure TFilterList.PutFilter(Index: Integer; Item: IBaseFilter);
  begin
    put(index,Item);
  end;

  function TFilterList.First: IBaseFilter;
  begin
    result := GetFilter(0);
  end;

  function TFilterList.IndexOf(Item: IBaseFilter): Integer;
  begin
     result := inherited IndexOf(Item);
  end;

  function TFilterList.Add(Item: IBaseFilter): Integer;
  begin
    result := inherited Add(Item);
  end;

  procedure TFilterList.Insert(Index: Integer; Item: IBaseFilter);
  begin
    inherited Insert(index,item);
  end;

  function TFilterList.Last: IBaseFilter;
  begin
    result := inherited Last as IBaseFilter;
  end;

  function TFilterList.Remove(Item: IBaseFilter): Integer;
  begin
    result := inherited Remove(Item);
  end;

  function TFilterList.GetFilterInfo(index: integer): TFilterInfo;
  begin
    if assigned(items[index]) then items[index].QueryFilterInfo(result);
  end;

// *****************************************************************************
//  TPinList
// *****************************************************************************

  constructor TPinList.Create(BaseFilter: IBaseFilter);
  begin
    inherited Create;
    Filter := BaseFilter;
    Update;
  end;

  destructor TPinList.Destroy;
  begin
    Filter := nil;
    inherited Destroy;
  end;

  procedure TPinList.Update;
  var
    EnumPins : IEnumPins;
    Pin      : IPin;
  begin
    clear;
    if assigned(Filter) then Filter.EnumPins(EnumPins) else exit;
    while (EnumPins.Next(1, pin, nil) = S_OK) do add(Pin);
    EnumPins := nil;
  end;

  procedure TPinList.Assign(BaseFilter: IBaseFilter);
  begin
    Clear;
    Filter := BaseFilter;
    if Filter <> nil then Update;
  end;

  function TPinList.GetConnected(Index: Integer): boolean;
  var Pin: IPin;
  begin
    Items[Index].ConnectedTo(Pin);
    Result := (Pin <> nil); 
  end;

  function TPinList.GetPin(Index: Integer): IPin;
  begin
    result := get(index) as IPin;
  end;

  procedure TPinList.PutPin(Index: Integer; Item: IPin);
  begin
    put(index,Item);
  end;

  function TPinList.First: IPin;
  begin
    result := GetPin(0);
  end;

  function TPinList.IndexOf(Item: IPin): Integer;
  begin
     result := inherited IndexOf(Item);
  end;

  function TPinList.Add(Item: IPin): Integer;
  begin
    result := inherited Add(Item);
  end;

  procedure TPinList.Insert(Index: Integer; Item: IPin);
  begin
    inherited Insert(index,item);
  end;

  function TPinList.Last: IPin;
  begin
    result := inherited Last as IPin;
  end;

  function TPinList.Remove(Item: IPin): Integer;
  begin
    result := inherited Remove(Item);
  end;

  function TPinList.GetPinInfo(index: integer): TPinInfo;
  begin
    if assigned(Items[index]) then Items[index].QueryPinInfo(result);
  end;

// *****************************************************************************
//  TPersistentMemory
// *****************************************************************************

  constructor TPersistentMemory.Create;
  begin
    FData := nil;
    FDataLength := 0;
  end;

  destructor TPersistentMemory.Destroy;
  begin
    AllocateMemory(0);
    inherited destroy;
  end;

  procedure TPersistentMemory.AllocateMemory(ALength: Cardinal);
  begin
    if (FDataLength > 0) and (FData <> nil) then
    begin
      FreeMem(FData, FDataLength);
      FData := nil;
      FDataLength := 0;
    end;
    if ALength > 0 then
      begin
        GetMem(FData, ALength);
        ZeroMemory(FData, ALength);
        FDataLength := ALength;
      end
  end;

  procedure TPersistentMemory.ReadData(Stream: TStream);
  var ALength: Cardinal;
  begin
    Stream.Read(ALength, SizeOf(Cardinal));
    AllocateMemory(ALength);
    if ALength > 0 then
      Stream.Read(FData^, ALength);
  end;

  procedure TPersistentMemory.WriteData(Stream: TStream);
  begin
    Stream.Write(FDataLength, SizeOf(Cardinal));
    if FDataLength > 0 then
      Stream.Write(FData^, FDataLength);
  end;

  procedure TPersistentMemory.Assign(Source: TPersistent);
  begin
    if Source is TPersistentMemory then
    begin
      if (Source <> self) then
      begin
        AllocateMemory(TPersistentMemory(Source).FDataLength);
        if FDataLength > 0 then
          move(TPersistentMemory(Source).FData^, FData^, FDataLength);
      end;
    end
    else
      inherited Assign(Source);
  end;

  procedure TPersistentMemory.AssignTo(Dest: TPersistent);
  begin
    Dest.Assign(self);
  end;

  function TPersistentMemory.Equal(Memory: TPersistentMemory): boolean;
  begin
    result := false;
    if (Memory.FDataLength > 0) and (Memory.FDataLength = FDataLength) and
       (Memory.FData <> nil) and (FData <> nil) then
    result := comparemem(Memory.FData, FData, FDataLength);
  end;

  procedure TPersistentMemory.DefineProperties(Filer: TFiler);
    function DoWrite: Boolean;
    begin
      result := true;
      if Filer.Ancestor <> nil then
      begin
        Result := True;
        if Filer.Ancestor is TPersistentMemory then
          Result := not Equal(TPersistentMemory(Filer.Ancestor))
      end;
    end;

  begin
    Filer.DefineBinaryProperty('data', ReadData, WriteData, DoWrite);
  end;

// *****************************************************************************
//  TBaseFilter
// *****************************************************************************

  procedure TBaseFilter.SetMoniker(Moniker: IMoniker);
  var
    MemStream    : TMemoryStream;
    AdaStream    : TStreamAdapter;
  begin
    if Moniker = nil then
    begin
      DataLength := 0;
      exit;
    end;
    MemStream := TMemoryStream.Create;
    AdaStream := TStreamAdapter.Create(MemStream, soReference);
    OleSaveToStream(Moniker, AdaStream);
    DataLength := MemStream.Size;
    move(MemStream.Memory^, Data^, DataLength);
    AdaStream.Free;
    MemStream.Free;
  end;

  function TBaseFilter.GetMoniker: IMoniker;
  var
    MemStream    : TMemoryStream;
    AdaStream    : TStreamAdapter;
  begin
    if DataLength > 0 then
      begin
        MemStream := TMemoryStream.Create;
        MemStream.SetSize(DataLength);
        move(Data^, MemStream.Memory^, DataLength);
        AdaStream := TStreamAdapter.Create(MemStream, soReference);
        OleLoadFromStream(AdaStream, IMoniker, result);
        AdaStream.Free;
        MemStream.Free;
      end
    else
      result := nil;
  end;

  function TBaseFilter.CreateFilter: IBaseFilter;
  var
    AMoniker     : IMoniker;
  begin
    AMoniker := Moniker;
    if AMoniker <> nil then
      begin
        AMoniker.BindToObject(nil, nil, IBaseFilter, result);
        AMoniker := nil;
      end
    else
      result := nil;
  end;

  function TBaseFilter.PropertyBag(Name: WideString): OleVariant;
  var
    AMoniker : IMoniker;
    PropBag  : IPropertyBag;
  begin
    AMoniker := Moniker;
    if AMoniker <> nil then
      begin
        AMoniker.BindToStorage(nil, nil, IID_IPropertyBag, PropBag);
        if PropBag <> nil then PropBag.Read(PWideChar(Name), result, nil);
        PropBag  := nil;
        AMoniker := nil;
      end
    else
      result := NULL;
  end;

// milenko start (added functions from dshowutil.cpp)
function FindRenderer(pGB: IGraphBuilder; const mediatype: PGUID; out ppFilter: IBaseFilter): HRESULT;
var
  Enum  : IEnumFilters;
  Filter: IBaseFilter;
  Pin   : IPin;
  Fetched,
  InPins,
  OutPins: Cardinal;
  Found: Boolean;
  MediaType_: TAMMediaType;
begin
  Found := False;

  // Verify graph builder interface
  if not Assigned(pGB) then
  begin
    Result := E_NOINTERFACE;
    Exit;
  end;

  // Verify that a media type was passed
  if not Assigned(mediatype) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  // Clear the filter pointer in case there is no match
  if Assigned(ppFilter) then ppFilter := nil;

  // Get filter enumerator
  Result := pGB.EnumFilters(Enum);
  if FAILED(Result) then Exit;

  Enum.Reset;

  // Enumerate all filters in the graph
  while((not Found) and  (Enum.Next(1, Filter, @Fetched) = S_OK)) do
  begin
    // Find a filter with one input and no output pins
    Result := CountFilterPins(Filter, InPins, OutPins);
    if FAILED(Result) then break;

    if ((InPins = 1) and (OutPins = 0)) then
    begin
      // Get the first pin on the filter
      Pin := nil;
      Pin := GetInPin(Filter, 0);

      // Read this pin's major media type
      Result := Pin.ConnectionMediaType(MediaType_);
      if FAILED(Result) then  break;

      // Is this pin's media type the requested type?
      // If so, then this is the renderer for which we are searching.
      // Copy the interface pointer and return.
      if IsEqualGUID(MediaType_.majortype,mediatype^) then
      begin
        // Found our filter
        ppFilter := Filter;
        Found := True;
      end else
      begin
        // This is not the renderer, so release the interface.
        Filter := nil;
      end;

      // Delete memory allocated by ConnectionMediaType()
      UtilFreeMediaType(@MediaType_);
    end else
    begin
      // No match, so release the interface
      Filter := nil;
    end;
  end;

  Enum := nil;
end;

function FindAudioRenderer(pGB: IGraphBuilder; out ppFilter: IBaseFilter): HRESULT;
begin
  Result := FindRenderer(pGB, @MEDIATYPE_Audio, ppFilter);
end;

function FindVideoRenderer(pGB: IGraphBuilder; out ppFilter: IBaseFilter): HRESULT;
begin
  Result := FindRenderer(pGB, @MEDIATYPE_Video, ppFilter);
end;

function CountFilterPins(pFilter: IBaseFilter; out pulInPins: Cardinal; out pulOutPins: Cardinal): HRESULT;
var
  Enum: IEnumPins;
  Found: Cardinal;
  Pin: IPin;
  PinDir: TPinDirection;
begin
  // Verify input
  if (not Assigned(pFilter) or not Assigned(@pulInPins) or not Assigned(@pulOutPins)) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  // Clear number of pins found
  pulInPins := 0;
  pulOutPins := 0;

  // Get pin enumerator
  Result := pFilter.EnumPins(Enum);
  if FAILED(Result) then Exit;

  Enum.Reset;

  // Count every pin on the filter
  while(S_OK = Enum.Next(1, Pin, @Found)) do
  begin
    Result := Pin.QueryDirection(PinDir);
    if (PinDir = PINDIR_INPUT) then inc(pulInPins)
                               else inc(pulOutPins);
    Pin := nil;
  end;

  Enum := nil;
end;

function CountTotalFilterPins(pFilter: IBaseFilter; out pulPins: Cardinal): HRESULT;
var
  Enum: IEnumPins;
  Found: Cardinal;
  Pin: IPin;
begin
  // Verify input
  if (not Assigned(pFilter) or not Assigned(@pulPins)) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  // Clear number of pins found
  pulPins := 0;

  // Get pin enumerator
  Result := pFilter.EnumPins(Enum);
  if FAILED(Result) then Exit;

  // Count every pin on the filter, ignoring direction
  while(S_OK = Enum.Next(1, Pin, @Found)) do
  begin
    inc(pulPins);
    Pin := nil;
  end;

  Enum := nil;
end;

function GetPin(pFilter: IBaseFilter; dirrequired: TPinDirection; iNum: integer; out ppPin: IPin): HRESULT;
var
  Enum: IEnumPins;
  Found: Cardinal;
  Pin: IPin;
  PinDir: TPinDirection;
begin
  ppPin := nil;

  if not Assigned(pFilter) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  Result := pFilter.EnumPins(Enum);
  if FAILED(Result) then Exit;

  Result := E_FAIL;

  while(S_OK = Enum.Next(1, Pin, @Found)) do
  begin
    Pin.QueryDirection(PinDir);
    if (PinDir = dirrequired) then
    begin
      if (iNum = 0) then
      begin
        ppPin := Pin;  // Return the pin's interface
        Result := S_OK;      // Found requested pin, so clear error
        break;
      end;
      inc(iNum);
    end;
    Pin := nil;
  end;
  Enum := nil;
end;

function GetInPin(pFilter: IBaseFilter; nPin: integer): IPin;
begin
  GetPin(pFilter, PINDIR_INPUT, nPin, Result);
end;

function GetOutPin(pFilter: IBaseFilter; nPin: integer): IPin;
begin
  GetPin(pFilter, PINDIR_OUTPUT, nPin, Result);
end;

function FindOtherSplitterPin(pPinIn: IPin; guid: TGUID; nStream: integer; out ppSplitPin: IPin): HRESULT;
var
  PinOut: IPin;
  ThisPinInfo,
  pi: TPinInfo;
  EnumPins: IEnumPins;
  Fetched: Cardinal;
  Pin: IPin;
  MediaEnum: IEnumMediaTypes;
  MediaType: PAMMediaType;
begin
  if not Assigned(ppSplitPin) then
  begin
    Result := E_POINTER;
    Exit;
  end;                        

  PinOut := pPinIn;

  while Assigned(PinOut) do
  begin
    PinOut.QueryPinInfo(ThisPinInfo);
    if Assigned(ThisPinInfo.pFilter) then ThisPinInfo.pFilter := nil;

    PinOut := nil;
    ThisPinInfo.pFilter.EnumPins(EnumPins);
    if not Assigned(EnumPins) then
    begin
      // return NULL; ???
      Result := S_FALSE;
      Exit;
    end;

    // look at every pin on the current filter...
    while True do
    begin
      Fetched := 0;
      ASSERT(not Assigned(Pin)); // is it out of scope?
      EnumPins.Next(1, Pin, @Fetched);
      if not BOOL(Fetched) then break;

      Pin.QueryPinInfo(pi);
      if Assigned(pi.pFilter) then pi.pFilter := nil;

      // if it's an input pin...
      if (pi.dir = PINDIR_INPUT) then
      begin
        // continue searching upstream from this pin
        Pin.ConnectedTo(PinOut);

        // a pin that supports the required media type is the
        // splitter pin we are looking for!  We are done
      end else
      begin
        Pin.EnumMediaTypes(MediaEnum);
        if Assigned(MediaEnum) then
        begin
          Fetched := 0;
          MediaEnum.Next(1, MediaType, @Fetched);
          if BOOL(Fetched) then
          begin
            if IsEqualGUID(MediaType.majortype,guid) then
            begin
              dec(nStream);
              if(nStream = 0) then
              begin
                UtilDeleteMediaType(MediaType);
                ppSplitPin := Pin;
                Result := S_OK;
                Exit;
              end;
            end;
            UtilDeleteMediaType(MediaType);
          end;
        end;
      end;
      // go try the next pin
    end; // while
  end;

  ASSERT(False);
  Result := E_FAIL;
end;

function SeekNextFrame(pSeeking: IMediaSeeking; FPS: Double; Frame: LongInt): HRESULT;
var
  Pos: TReferenceTime;
begin
  // try seeking by frames first
  Pos := 0;
  Result := pSeeking.SetTimeFormat(TIME_FORMAT_FRAME);
  if not FAILED(Result) then
  begin
    pSeeking.GetCurrentPosition(Pos);
    inc(Pos);
  end else
  begin
    // couldn't seek by frames, use Frame and FPS to calculate time
    Pos := Round(Frame * UNITS / FPS);
    // add a half-frame to seek to middle of the frame
    Pos := Pos + Round(UNITS * 0.5 / FPS);
  end;

  Result :=  pSeeking.SetPositions(Pos, AM_SEEKING_AbsolutePositioning,
                                   Pos, AM_SEEKING_NoPositioning);
end;

procedure ShowFilenameByCLSID(clsid: TGUID; out szFilename: WideString);
begin
  szFilename := '<Unknown>';
  with TRegistry.Create do
  begin
    RootKey := HKEY_LOCAL_MACHINE;
    if KeyExists('Software\Classes\CLSID\' + GUIDToString(clsid) + 'InprocServer32') then
    begin
      if OpenKeyReadOnly('Software\Classes\CLSID\' + GUIDToString(clsid) + 'InprocServer32') then
      begin
        szFilename := ReadString('');
        CloseKey;
      end;
    end;
    Free;
  end;
end;

function GetFileDurationString(pMS: IMediaSeeking; out szDuration: WideString): HRESULT;
var
  guidOriginalFormat: TGUID;
  Duration: Int64;
  TotalMS: Cardinal;
  MS: integer;
  Seconds: integer;
  Minutes: integer;
begin
  if not Assigned(pMS) then
  begin
    Result := E_NOINTERFACE;
    Exit;
  end;

  if not Assigned(@szDuration) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  // Initialize the display in case we can't read the duration
  szDuration := '<00:00.000>';

  // Is media time supported for this file?
  if (S_OK <> pMS.IsFormatSupported(TIME_FORMAT_MEDIA_TIME)) then
  begin
    Result := E_NOINTERFACE;
    Exit;
  end;

  // Read the time format to restore later
  Result := pMS.GetTimeFormat(guidOriginalFormat);
  if FAILED(Result) then Exit;

  // Ensure media time format for easy display
  Result := pMS.SetTimeFormat(TIME_FORMAT_MEDIA_TIME);
  if FAILED(Result) then Exit;

  // Read the file's duration
  Result := pMS.GetDuration(Duration);
  if FAILED(Result) then Exit;

  // Return to the original format
  if not IsEqualGUID(guidOriginalFormat,TIME_FORMAT_MEDIA_TIME) then
  begin
    Result := pMS.SetTimeFormat(guidOriginalFormat);
    if FAILED(Result) then Exit;
  end;

  // Convert the LONGLONG duration into human-readable format
  TotalMS := Cardinal(Duration div 10000); // 100ns -> ms
  MS := TotalMS mod 1000;
  Seconds := TotalMS div 1000;
  Minutes := Seconds div 60;
  Seconds := Seconds mod 60;

  // Update the string
  szDuration := inttostr(Minutes) + 'm:' + inttostr(Seconds) + '.' + inttostr(MS) + 's';
end;

function CanFrameStep(pGB: IGraphBuilder): Boolean;
var
  pFS: IVideoFrameStep;
  hr: HRESULT;
begin
  // Get frame step interface
  hr := pGB.QueryInterface(IID_IVideoFrameStep, pFS);
  if FAILED(hr) then
  begin
    Result := False;
    Exit;
  end;

  // Check if this decoder can step
  hr := pFS.CanStep(0, nil);

  // Release frame step interface
  pFS := nil;

  Result := hr = S_OK;
end;

procedure UtilDeleteMediaType(pmt: PAMMediaType);
begin
  // Allow NULL pointers for coding simplicity
  if (pmt = nil) then Exit;

  // Free media type's format data
  if (pmt.cbFormat <> 0) then
  begin
    CoTaskMemFree(pmt.pbFormat);
    // Strictly unnecessary but tidier
    pmt.cbFormat := 0;
    pmt.pbFormat := nil;
  end;

  // Release interface
  if (pmt.pUnk <> nil) then pmt.pUnk := nil;

  // Free media type
  CoTaskMemFree(pmt);
end;

procedure UtilFreeMediaType(pmt: PAMMediaType);
begin
  if (pmt.cbFormat <> 0) then
  begin
    CoTaskMemFree(pmt.pbFormat);
    // Strictly unnecessary but tidier
    pmt.cbFormat := 0;
    pmt.pbFormat := nil;
  end;

  if (pmt.pUnk <> nil) then pmt.pUnk := nil;
end;

const
  wszStreamName: WideString = 'ActiveMovieGraph';

function SaveGraphFile(pGraph: IGraphBuilder; wszPath: WideString): HRESULT;
var
  Storage: IStorage;
  Stream: IStream;
  Persist: IPersistStream;
begin
  Result := StgCreateDocfile(
        PWideChar(wszPath),
        STGM_CREATE or STGM_TRANSACTED or STGM_READWRITE or STGM_SHARE_EXCLUSIVE,
        0, Storage);
  if FAILED(Result) then Exit;

  Result := Storage.CreateStream(
        PWideChar(wszStreamName),
        STGM_WRITE or STGM_CREATE or STGM_SHARE_EXCLUSIVE,
        0, 0, Stream);
  if FAILED(Result) then Exit;

  pGraph.QueryInterface(IID_IPersistStream, Persist);
  Result := Persist.Save(Stream, True);
  Stream := nil;
  Persist := nil;
  if SUCCEEDED(Result) then Result := Storage.Commit(STGC_DEFAULT);
  Storage := nil;
end;

function LoadGraphFile(pGraph: IGraphBuilder; const wszName: WideString): HRESULT;
var
  Storage: IStorage;
  Stream: IStream;
  PersistStream: IPersistStream;
begin
  if (S_OK <> StgIsStorageFile(PWideChar(wszName))) then
  begin
    Result := E_FAIL;
    Exit;
  end;

  Result := StgOpenStorage(PWideChar(wszName), nil,
        STGM_TRANSACTED or STGM_READ or STGM_SHARE_DENY_WRITE,
        nil, 0, Storage);

  if FAILED(Result) then Exit;

  Result := pGraph.QueryInterface(IID_IPersistStream, PersistStream);

  if (SUCCEEDED(Result)) then
  begin
    Result := Storage.OpenStream(PWideChar(wszStreamName), nil,
            STGM_READ or STGM_SHARE_EXCLUSIVE, 0, Stream);
    if SUCCEEDED(Result) then
    begin
      Result := PersistStream.Load(Stream);
      Stream := nil;
    end;
    PersistStream := nil;
  end;

  Storage := nil;
end;
// milenko end


// Michael Start.
//-----------------------------------------------------------------------------
// Name: GetDXVersion()
// Desc: This function returns the DirectX version.
// Arguments: 
//      pdwDirectXVersion - This can be NULL.  If non-NULL, the return value is:
//              0x00000000 = No DirectX installed
//              0x00010000 = DirectX 1.0 installed
//              0x00020000 = DirectX 2.0 installed
//              0x00030000 = DirectX 3.0 installed
//              0x00030001 = DirectX 3.0a installed
//              0x00050000 = DirectX 5.0 installed
//              0x00060000 = DirectX 6.0 installed
//              0x00060100 = DirectX 6.1 installed
//              0x00060101 = DirectX 6.1a installed
//              0x00070000 = DirectX 7.0 installed
//              0x00070001 = DirectX 7.0a installed
//              0x00080000 = DirectX 8.0 installed
//              0x00080100 = DirectX 8.1 installed
//              0x00080101 = DirectX 8.1a installed
//              0x00080102 = DirectX 8.1b installed
//              0x00080200 = DirectX 8.2 installed
//              0x00090000 = DirectX 9.0 installed
//              0x00090001 = DirectX 9.0a installed
//              0x00090002 = DirectX 9.0b installed
//      strDirectXVersion - Destination string to receive a string name of the DirectX Version.  Can be NULL.
//      cchDirectXVersion - Size of destination buffer in characters.  Length should be at least 10 chars.
// Returns: S_OK if the function succeeds.  
//          E_FAIL if the DirectX version info couldn't be determined.
//
// Please note that this code is intended as a general guideline. Your
// app will probably be able to simply query for functionality (via
// QueryInterface) for one or two components.
//
// Also please ensure your app will run on future releases of DirectX.
// For example:
//     "if( dwDirectXVersion != 0x00080100 ) return false;" is VERY BAD.
//     "if( dwDirectXVersion < 0x00080100 ) return false;" is MUCH BETTER.
//-----------------------------------------------------------------------------
function GetDXVersion(var pdwDirectXVersion :  DWORD; out strDirectXVersion : String) : HResult;
function GetDirectXVersionViaDxDiag(var pdwDirectXVersionMajor : dword;
                                    var pdwDirectXVersionMinor : dword;
                                    var pcDirectXVersionLetter : char) : HResult;

{$IFDEF VER130}
function FindVarData(const V: Variant): PVarData;
begin
  Result := @TVarData(V);
  while Result.VType = varByRef or varVariant do
    Result := PVarData(Result.VPointer);
end;

function VarIsType(const V: Variant; AVarType: TVarType): Boolean;
begin
  Result := FindVarData(V)^.VType = AVarType;
end;
{$ENDIF}

var
  hr                    : HRESULT;
  bCleanupCOM           : Boolean;
  bSuccessGettingMajor  : Boolean;
  bSuccessGettingMinor  : Boolean;
  bSuccessGettingLetter : Boolean;
  bGotDirectXVersion    : Boolean;
  pDxDiagProvider       : IDxDiagProvider;
  dxDiagInitParam       : TDXDIAGINITPARAMS;
  pDxDiagRoot           : IDxDiagContainer;
  pDxDiagSystemInfo     : IDxDiagContainer;
  va                    : OleVariant;
  strDestination        : String;
Begin
  bSuccessGettingMajor := false;
  bSuccessGettingMinor := false;
  bSuccessGettingLetter := false;

  // Init COM.  COM may fail if its already been inited with a different
  // concurrency model.  And if it fails you shouldn't release it.
  hr := CoInitialize(nil);
  bCleanupCOM := SUCCEEDED(hr);

  // Get an IDxDiagProvider
  bGotDirectXVersion := false;

  pDxDiagProvider := Nil;

  hr := CoCreateInstance(CLSID_DxDiagProvider, Nil, CLSCTX_INPROC_SERVER, IID_IDxDiagProvider, pDxDiagProvider);
  if SUCCEEDED(hr) then
  Begin
    // Fill out a DXDIAG_INIT_PARAMS struct
    dxDiagInitParam.dwSize                 := sizeof(TDXDIAGINITPARAMS);
    dxDiagInitParam.dwDxDiagHeaderVersion  := DXDIAG_DX9_SDK_VERSION;
    dxDiagInitParam.bAllowWHQLChecks       := false;
    dxDiagInitParam.pReserved              := Nil;

    // Init the m_pDxDiagProvider
    hr := pDxDiagProvider.Initialize(@dxDiagInitParam);
    if SUCCEEDED(hr) then
    Begin
      pDxDiagRoot := Nil;
      pDxDiagSystemInfo := Nil;

      // Get the DxDiag root container
      hr := pDxDiagProvider.GetRootContainer(pDxDiagRoot);
      if SUCCEEDED(hr) then
      Begin
        // Get the object called DxDiag_SystemInfo
        hr := pDxDiagRoot.GetChildContainer('DxDiag_SystemInfo', pDxDiagSystemInfo);
        if SUCCEEDED(hr) then
        Begin
          // Get the "dwDirectXVersionMajor" property
          VariantInit(Va);
          hr := pDxDiagSystemInfo.GetProp('dwDirectXVersionMajor', va);
          if (SUCCEEDED(hr)) and (VarIsType(va, VT_UI4)) then
          Begin
            pdwDirectXVersionMajor := Va;
            bSuccessGettingMajor := true;
          End;
          VariantClear(va);

          // Get the "dwDirectXVersionMinor" property
          hr := pDxDiagSystemInfo.GetProp('dwDirectXVersionMinor', va);
          if (SUCCEEDED(hr)) and (VarIsType(va ,VT_UI4)) then
          Begin
            pdwDirectXVersionMinor := va;
            bSuccessGettingMinor := true;
          End;
          VariantClear(va);

          // Get the "szDirectXVersionLetter" property
          hr := pDxDiagSystemInfo.GetProp('szDirectXVersionLetter', va);
          If (SUCCEEDED(hr)) and (VarIsType(va , VT_BSTR)) then
          Begin
            strDestination := WideCharToString(TVarData(va).VOleStr);
            pcDirectXVersionLetter := StrDestination[1];
            bSuccessGettingLetter := true;
          End;
          VariantClear(va);

          // If it all worked right, then mark it down
          bGotDirectXVersion := bSuccessGettingMajor and bSuccessGettingMinor and bSuccessGettingLetter;
          pDxDiagSystemInfo := Nil;
        end;
        pDxDiagRoot := Nil;
      End;
    end;
    pDxDiagProvider := Nil;
  end;

  if bCleanupCOM then
    CoUninitialize;

  if bGotDirectXVersion then
    result := S_OK
  else
    result := E_FAIL;
end;

//-----------------------------------------------------------------------------
// Name: GetDirectXVerionViaFileVersions()
// Desc: Tries to get the DirectX version by looking at DirectX file versions
//-----------------------------------------------------------------------------
function GetDirectXVerionViaFileVersions(var pdwDirectXVersionMajor : DWORD;
                                         var pdwDirectXVersionMinor : DWORD;
                                         var pcDirectXVersionLetter : Char) : HResult;
type
  TFileVersion = record
    Major : integer;
    Minor : integer;
    Release : integer;
    Build : integer;
  End;

function CompareFileVersion(Version1, Version2 : TFileVersion) : Integer;
var
  TmpStr1,
  TmpStr2 : String;
Begin
  TmpStr1 := Format('%4.4d%4.4d%8.8d%4.4d', [Version1.Major, Version1.Minor, Version1.Release, Version1.Build]);
  TmpStr2 := Format('%4.4d%4.4d%8.8d%4.4d', [Version2.Major, Version2.Minor, Version2.Release, Version2.Build]);
  // milenko start (delph 5 compatibility)
  // Result := CompareValue(Strtoint64(TmpStr1),Strtoint64(TmpStr2));
  Result := Strtoint64(TmpStr1) - Strtoint64(TmpStr2);
  if Result > 0 then Result := 1
  else if Result < 0 then Result := -1;
  // milenko end
End;

function ReadVersionInfo(Filename: string) : TFileVersion;
var
  Info : PVSFixedFileInfo;
{$ifdef VER120}
  InfoSize : Cardinal;
{$else}
  InfoSize : UINT;
{$endif}
  nHwnd : DWORD;
  BufferSize : DWORD;
  Buffer : Pointer;
begin
  ZeroMemory(@Result, Sizeof(TFileVersion));
  If Not FileExists(Filename) then Exit;
  BufferSize := GetFileVersionInfoSize(pchar(filename),nHWnd); // Get buffer size
  if BufferSize <> 0 then // if zero, there is no version info
  begin
    GetMem( Buffer, BufferSize); // allocate buffer memory
    try
      if GetFileVersionInfo(PChar(filename),nHWnd,BufferSize,Buffer) then
      begin
        // got version info
        if VerQueryValue(Buffer, '\', Pointer(Info), InfoSize) then
        begin
          // got root block version information
          Result.Major := HiWord(Info^.dwFileVersionMS); // extract major version
          Result.Minor := LoWord(Info^.dwFileVersionMS); // extract minor version
          Result.Release := HiWord(Info^.dwFileVersionLS); // extract release version
          Result.Build := LoWord(Info^.dwFileVersionLS); // extract build version
        end;
      end;
    finally
      FreeMem(Buffer, BufferSize); // release buffer memory
    end;
  end;
end;

function FileVersion(Major, Minor, Release, Build : integer) : TFileVersion;
Begin
  Result.Major := Major;
  Result.Minor := Minor;
  Result.Release := Release;
  Result.Build := Build;
End;

var
  szPath : PChar;
  Path : String;

  ddraw_Version,
  d3drg8x_Version,
  dplayx_Version,
  dinput_Version,
  d3d8_Version,
  mpg2splt_Version,
  dpnet_Version,
  d3d9_Version : TFileVersion;
begin
  pdwDirectXVersionMajor := 0;
  pdwDirectXVersionMinor := 0;
  pcDirectXVersionLetter := ' ';

  Result := E_Fail;
  szPath := GetMemory(MAX_PATH);
  If GetSystemDirectory(szPath, MAX_PATH) = 0 then
  Begin
    FreeMemory(szPath);
    Exit;
  End;
  Path := StrPas(szPath);
  FreeMemory(szPath);
  If Path[length(Path)] <> '\' then
    Path := Path + '\';

  ddraw_Version := ReadVersionInfo(Path+'ddraw.dll');
  d3drg8x_Version := ReadVersionInfo(Path+'d3drg8x.dll');
  dplayx_Version := ReadVersionInfo(Path+'dplayx.dll');
  dinput_Version := ReadVersionInfo(Path+'dinput.dll');
  d3d8_Version := ReadVersionInfo(Path+'d3d8.dll');
  mpg2splt_Version := ReadVersionInfo(Path+'mpg2splt.ax');
  dpnet_Version := ReadVersionInfo(Path+'dpnet.dll');
  d3d9_Version := ReadVersionInfo(Path+'d3d9.dll');

  If CompareFileVersion(ddraw_Version, FileVersion(4,2,0,95)) >= 0 then // Win9x version
  Begin
    // ddraw.dll is >= DX1.0 version, so we must be at least DX1.0
    pdwDirectXVersionMajor := 1;
    pdwDirectXVersionMinor := 0;
    pcDirectXVersionLetter := ' ';
  End;

  If CompareFileVersion(ddraw_Version, FileVersion(4, 3, 0, 1096)) >= 0 then // Win9x version
  Begin
    // ddraw.dll is is >= DX2.0 version, so we must DX2.0 or DX2.0a (no redist change)
    pdwDirectXVersionMajor := 2;
    pdwDirectXVersionMinor := 0;
    pcDirectXVersionLetter := ' ';
  End;

  If CompareFileVersion(ddraw_Version, FileVersion(4, 4, 0, 68)) >= 0 then // Win9x version
  Begin
    // ddraw.dll is >= DX3.0 version, so we must be at least DX3.0
    pdwDirectXVersionMajor := 3;
    pdwDirectXVersionMinor := 0;
    pcDirectXVersionLetter := ' ';
  End;

  If CompareFileVersion(d3drg8x_Version, FileVersion(4, 4, 0, 70)) >= 0 then // Win9x version
  Begin
    // d3drg8x.dll is the DX3.0a version, so we must be DX3.0a or DX3.0b  (no redist change)
    pdwDirectXVersionMajor := 3;
    pdwDirectXVersionMinor := 0;
    pcDirectXVersionLetter := 'a';
  End;

  If CompareFileVersion(ddraw_Version, FileVersion(4, 5, 0, 155)) >= 0 then // Win9x version
  Begin
    // ddraw.dll is the DX5.0 version, so we must be DX5.0 or DX5.2 (no redist change)
    pdwDirectXVersionMajor := 5;
    pdwDirectXVersionMinor := 0;
    pcDirectXVersionLetter := ' ';
  End;

  If CompareFileVersion(ddraw_Version, FileVersion(4, 6, 0, 318)) >= 0 then // Win9x version
  Begin
    // ddraw.dll is the DX6.0 version, so we must be at least DX6.0
    pdwDirectXVersionMajor := 6;
    pdwDirectXVersionMinor := 0;
    pcDirectXVersionLetter := ' ';
  End;

  If CompareFileVersion(ddraw_Version, FileVersion(4, 6, 0, 436)) >= 0 then // Win9x version
  Begin
    // ddraw.dll is the DX6.1 version, so we must be at least DX6.1
    pdwDirectXVersionMajor := 6;
    pdwDirectXVersionMinor := 1;
    pcDirectXVersionLetter := ' ';
  End;

  If CompareFileVersion(dplayx_Version, FileVersion(4, 6, 3, 518)) >= 0 then // Win9x version
  Begin
    // dplayx.dll is the DX6.1a version, so we must be at least DX6.1a
    pdwDirectXVersionMajor := 6;
    pdwDirectXVersionMinor := 1;
    pcDirectXVersionLetter := 'a';
  End;

  If CompareFileVersion(ddraw_Version, FileVersion(4, 7, 0, 700)) >= 0 then // Win9x version
  Begin
    // TODO: find win2k version

    // ddraw.dll is the DX7.0 version, so we must be at least DX7.0
    pdwDirectXVersionMajor := 7;
    pdwDirectXVersionMinor := 0;
    pcDirectXVersionLetter := ' ';
  End;

  If CompareFileVersion(dinput_Version, FileVersion(4, 7, 0, 716)) >= 0 then // Win9x version
  Begin
    // TODO: find win2k version

    // dinput.dll is the DX7.0a version, so we must be at least DX7.0a
    pdwDirectXVersionMajor := 7;
    pdwDirectXVersionMinor := 0;
    pcDirectXVersionLetter := 'a';
  End;

  If ((ddraw_Version.Major = 4) and (CompareFileVersion(ddraw_Version, FileVersion(4, 8, 0, 400)) >= 0)) or // Win9x version
     ((ddraw_Version.Major = 5) and (CompareFileVersion(ddraw_Version, FileVersion(5, 1, 2258, 400)) >= 0)) then // Win2k/WinXP version
  Begin
    // ddraw.dll is the DX8.0 version, so we must be at least DX8.0 or DX8.0a (no redist change)
    //
    // DirectX 8.0a contains updates for issues with international installs on Windows 2000 and issues where
    // input devices could have buttons disabled that were enabled with previous DirectX releases.
    // There are no other changes.
    pdwDirectXVersionMajor := 8;
    pdwDirectXVersionMinor := 0;
    pcDirectXVersionLetter := ' ';
  End;

  If ((d3d8_Version.Major = 4) and (CompareFileVersion(d3d8_Version, FileVersion(4, 8, 1, 881)) >= 0)) or // Win9x version
     ((d3d8_Version.Major = 5) and (CompareFileVersion(d3d8_Version, FileVersion(5, 1, 2600, 881)) >= 0)) then // Win2k/WinXP version
  Begin
    // d3d8.dll is the DX8.1 version, so we must be at least DX8.1
    pdwDirectXVersionMajor := 8;
    pdwDirectXVersionMinor := 1;
    pcDirectXVersionLetter := ' ';
  End;

  If ((d3d8_Version.Major = 4) and (CompareFileVersion(d3d8_Version, FileVersion(4, 8, 1, 901)) >= 0)) or // Win9x version
     ((d3d8_Version.Major = 5) and (CompareFileVersion(d3d8_Version, FileVersion(5, 1, 2600, 901)) >= 0)) then // Win2k/WinXP version
  Begin
    // d3d8.dll is the DX8.1 version, so we must be at least DX8.1
    pdwDirectXVersionMajor := 8;
    pdwDirectXVersionMinor := 1;
    pcDirectXVersionLetter := 'a';
  End;

  If (CompareFileVersion(mpg2splt_Version, FileVersion(6, 3, 1, 885)) >= 0) then // Win9x/Win2k/WinXP version
  Begin
    // quartz.dll is the DX8.1b version, so we must be at least DX8.1b
    pdwDirectXVersionMajor := 8;
    pdwDirectXVersionMinor := 1;
    pcDirectXVersionLetter := 'b';
  End;

  If ((dpnet_Version.Major = 4) and (CompareFileVersion(dpnet_Version, FileVersion(4, 9, 0, 134)) >= 0)) or // Win9x version
     ((dpnet_Version.Major = 5) and (CompareFileVersion(dpnet_Version, FileVersion(5, 2, 3677, 134)) >= 0)) then // Win2k/WinXP version
  Begin
    // dpnet.dll is the DX8.2 version, so we must be at least DX8.2
    pdwDirectXVersionMajor := 8;
    pdwDirectXVersionMinor := 2;
    pcDirectXVersionLetter := ' ';
  End;

  If ((d3d9_Version.Major = 4) and (CompareFileVersion(d3d9_Version, FileVersion(4, 9, 0, 900)) >= 0)) or // Win9x version
     ((d3d9_Version.Major = 5) and (CompareFileVersion(d3d9_Version, FileVersion(5, 3, 0, 900)) >= 0)) then // Win2k/WinXP version
  Begin
    // d3d9.dll is the DX9.0 version, so we must be at least DX9.0
    pdwDirectXVersionMajor := 9;
    pdwDirectXVersionMinor := 0;
    pcDirectXVersionLetter := ' ';
  End;

  If ((d3d9_Version.Major = 4) and (CompareFileVersion(d3d9_Version, FileVersion(4, 9, 0, 901)) >= 0)) or // Win9x version
     ((d3d9_Version.Major = 5) and (CompareFileVersion(d3d9_Version, FileVersion(5, 3, 0, 901)) >= 0)) then // Win2k/WinXP version
  Begin
    // d3d9.dll is the DX9.0a version, so we must be at least DX9.0a
    pdwDirectXVersionMajor := 9;
    pdwDirectXVersionMinor := 0;
    pcDirectXVersionLetter := 'a';
  End;

  If ((d3d9_Version.Major = 4) and (CompareFileVersion(d3d9_Version, FileVersion(4, 9, 0, 902)) >= 0)) or // Win9x version
     ((d3d9_Version.Major = 5) and (CompareFileVersion(d3d9_Version, FileVersion(5, 3, 0, 902)) >= 0)) then // Win2k/WinXP version
  Begin
    // d3d9.dll is the DX9.0b version, so we must be at least DX9.0b
    pdwDirectXVersionMajor := 9;
    pdwDirectXVersionMinor := 0;
    pcDirectXVersionLetter := 'b';
  End;
  Result := s_OK;
end;

var
  dwDirectXVersionMajor : DWORD;
  dwDirectXVersionMinor : DWORD;
  cDirectXVersionLetter : CHAR;

  dwDirectXVersion : DWORD;
Begin
  // Init values to unknown
  pdwDirectXVersion := 0;
  strDirectXVersion := '';

  dwDirectXVersionMajor := 0;
  dwDirectXVersionMinor := 0;
  cDirectXVersionLetter := ' ';

  // First, try to use dxdiag's COM interface to get the DirectX version.
  // The only downside is this will only work on DX9 or later.
  Result := GetDirectXVersionViaDxDiag(dwDirectXVersionMajor, dwDirectXVersionMinor, cDirectXVersionLetter);

  If Result = E_Fail then
    // Getting the DirectX version info from DxDiag failed,
    // so most likely we are on DX8.x or earlier
    Result := GetDirectXVerionViaFileVersions(dwDirectXVersionMajor, dwDirectXVersionMinor, cDirectXVersionLetter);

  // If both techniques failed, then return E_FAIL
  If Result = E_Fail then
    Exit;

  // Set the output values to what we got and return
  // like 0x00080102 which would represent DX8.1b
  dwDirectXVersion := dwDirectXVersionMajor;
  dwDirectXVersion := dwDirectXVersion shl 8;
  dwDirectXVersion := dwDirectXVersion + dwDirectXVersionMinor;
  dwDirectXVersion := dwDirectXVersion shl 8;
  if (Ord(cDirectXVersionLetter) >= 97) and (Ord(cDirectXVersionLetter) <= 122) then
    dwDirectXVersion := dwDirectXVersion + int64(Ord(cDirectXVersionLetter) - 96);
  pdwDirectXVersion := dwDirectXVersion;

  If dwDirectXVersion > 0 then
  Begin
    if cDirectXVersionLetter = ' ' then
      strDirectXVersion := Format('%d.%d', [dwDirectXVersionMajor, dwDirectXVersionMinor])
    else
      strDirectXVersion := Format('%d.%d%s', [dwDirectXVersionMajor, dwDirectXVersionMinor, cDirectXVersionLetter]);
  End;

  Result := S_OK;
End;

// Michael End.

// milenko start DMO TMediaBuffer implementation
constructor TMediaBuffer.Create(MaxLen: DWORD);
begin
  inherited Create;
  FRefCount := 0;
  FMaxLength := MaxLen;
  FLength := 0;
  FData := nil;
  FData := AllocMem(MaxLen);
  Assert(Assigned(FData));
end;

destructor TMediaBuffer.Destroy;
begin
  if Assigned(FData) then
  begin
    FreeMem(FData);
    FData := nil;
  end;
end;

class function TMediaBuffer.CreateBuffer(MaxLen: DWORD; const IID: TGUID; out Obj): HRESULT;
var
  pBuffer: TMediaBuffer;
begin
  try
    pBuffer := TMediaBuffer.Create(MaxLen);
    Result := pBuffer.QueryInterface(IID, Obj);
  except
    Result := E_OUTOFMEMORY;
  end;
end;

function TMediaBuffer.QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
begin
  if not Assigned(@Obj) then
  begin
    Result := E_POINTER;
    Exit;
  end else
  if IsEqualGUID(IID, IID_IMediaBuffer) or IsEqualGUID(IID, IUnknown) then
  begin
    if GetInterface(IID,Obj) then
    begin
      Result := S_OK;
      Exit;
    end
  end;
  Pointer(Obj) := nil;
  Result := E_NOINTERFACE;
end;

function TMediaBuffer._AddRef: Integer; stdcall;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TMediaBuffer._Release: Integer; stdcall;
begin
  Result := InterlockedDecrement(FRefCount);
  if (Result = 0) then Free;
end;

function TMediaBuffer.SetLength(cbLength: DWORD): HResult; stdcall;
begin
  if (cbLength > FMaxLength) then
  begin
    Result := E_INVALIDARG;
  end else
  begin
    FLength := cbLength;
    Result := S_OK;
  end;
end;

function TMediaBuffer.GetMaxLength(out pcbMaxLength: DWORD): HResult; stdcall;
begin
  if not Assigned(@pcbMaxLength) then
  begin
    Result := E_POINTER;
    Exit;
  end else
  begin
    pcbMaxLength := FMaxLength;
    Result := S_OK;
  end;
end;

function TMediaBuffer.GetBufferAndLength(out ppBuffer: PByte; // not filled if NULL
                                         out pcbLength: DWORD    // not filled if NULL
                                         ): HResult; stdcall;
begin
  if not Assigned(@ppBuffer) or not Assigned(@pcbLength) then
  begin
    Result := E_POINTER;
    Exit;
  end;
  ppBuffer := FData;
  pcbLength := FLength;
  Result := S_OK;
end;
// milenko end

// milenko start wxutil implementation
function EnlargedUnsignedDivide(Dividend: ULARGE_INTEGER; Divisor: ULONG; Remainder: PULONG): ULONG; stdcall;
asm
        mov      eax, Dividend.LowPart
        mov      edx, Dividend.HighPart
        mov      ecx, Remainder
        div      Divisor
        or       ecx,ecx
        jz       @@End
        mov      [ecx],edx
@@End:
end;

function Int32x32To64(a, b: integer): Int64;
asm
        imul     b
end;

function MILLISECONDS_TO_100NS_UNITS(Ms: LONGLONG): LONGLONG;
begin
  Result := Int32x32To64((Ms), (UNITS div MILLISECONDS))
end;

function UInt32x32To64(a, b: DWORD): ULONGLONG;
asm
        mul      b
end;

function llMulDiv(a, b, c, d: LONGLONG): LONGLONG;
var
  ua, ub : ULARGE_INTEGER;
  uc :   DWORDLONG;
  bSign : BOOL;
  p, ud : array[0..1] of ULARGE_INTEGER;
  x : ULARGE_INTEGER;
  uliTotal : ULARGE_INTEGER;
  ullResult : DWORDLONG;
  ulic : ULARGE_INTEGER;
  uliDividend : ULARGE_INTEGER;
  uliResult : ULARGE_INTEGER;
  dwDivisor : DWORD;
  i : integer;
begin
  if a >= 0 then ua.QuadPart := DWORDLONG(a)
            else ua.QuadPart := DWORDLONG(-a);
  if b >= 0 then ub.QuadPart := DWORDLONG(b)
            else ua.QuadPart := DWORDLONG(-b);
  if c >= 0 then uc := DWORDLONG(c)
            else uc := DWORDLONG(-c);
  bSign := (a < 0) xor (b < 0);

  p[0].QuadPart := UInt32x32To64(ua.LowPart, ub.LowPart);

  x.QuadPart := UInt32x32To64(ua.LowPart, ub.HighPart) +
                UInt32x32To64(ua.HighPart, ub.LowPart) +
                p[0].HighPart;

  p[0].HighPart := x.LowPart;
  p[1].QuadPart := UInt32x32To64(ua.HighPart, ub.HighPart) + x.HighPart;

  if (d <> 0) then
  begin
    if (bSign) then
    begin
      ud[0].QuadPart := DWORDLONG(-d);
      if (d > 0) then ud[1].QuadPart := $FFFFFFFFFFFFFFFF
                 else ud[1].QuadPart := DWORDLONG(0);
    end else
    begin
      ud[0].QuadPart := DWORDLONG(d);
      if (d < 0) then ud[1].QuadPart := $FFFFFFFFFFFFFFFF
                 else ud[1].QuadPart := DWORDLONG(0);
    end;

    uliTotal.QuadPart  := DWORDLONG(ud[0].LowPart) + p[0].LowPart;
    p[0].LowPart       := uliTotal.LowPart;

    uliTotal.LowPart   := uliTotal.HighPart;
    uliTotal.HighPart  := 0;

    uliTotal.QuadPart  := uliTotal.QuadPart + (DWORDLONG(ud[0].HighPart) + p[0].HighPart);
    p[0].HighPart      := uliTotal.LowPart;

    uliTotal.LowPart   := uliTotal.HighPart;
    uliTotal.HighPart  := 0;

    p[1].QuadPart      := p[1].QuadPart + ud[1].QuadPart + uliTotal.QuadPart;

    if (LongInt(p[1].HighPart) < 0) then
    begin
      bSign := not bSign;
      p[0].QuadPart := not p[0].QuadPart;
      p[1].QuadPart := not p[1].QuadPart;
      p[0].QuadPart := p[0].QuadPart + 1;
      p[1].QuadPart := p[1].QuadPart + LongInt(p[0].QuadPart = 0);
    end;
  end;

  if (c < 0) then bSign := not bSign;

  if (uc <= p[1].QuadPart) then
  begin
    if bSign then Result := LONGLONG($8000000000000000)
             else Result := LONGLONG($7FFFFFFFFFFFFFFF);
    Exit;
  end;

  if (p[1].QuadPart = 0) then
  begin
    ullResult := p[0].QuadPart div uc;
    if bSign then Result := -LONGLONG(ullResult)
             else Result :=  LONGLONG(ullResult);
    Exit;
  end;

  ulic.QuadPart := uc;
  if (ulic.HighPart = 0) then
  begin
    dwDivisor := DWORD(uc);
    uliDividend.HighPart := p[1].LowPart;
    uliDividend.LowPart := p[0].HighPart;
    if (uliDividend.QuadPart >= DWORDLONG(dwDivisor))
      then uliResult.HighPart := EnlargedUnsignedDivide(uliDividend,dwDivisor,@p[0].HighPart)
      else uliResult.HighPart := 0;
    uliResult.LowPart := EnlargedUnsignedDivide(p[0],dwDivisor,nil);
    if bSign then Result := -LONGLONG(uliResult.QuadPart)
             else Result :=  LONGLONG(uliResult.QuadPart);
    Exit;
  end;

  ullResult := 0;

  for i := 0 to 63 do
  begin
    ullResult := ullResult shl 1;
    p[1].QuadPart := p[1].QuadPart shl 1;
    if ((p[0].HighPart and $80000000) <> 0) then p[1].LowPart := p[1].LowPart + 1;
    p[0].QuadPart := p[0].QuadPart shl 1;
    if (uc <= p[1].QuadPart) then
    begin
      p[1].QuadPart := p[1].QuadPart - uc;
      ullResult := ullResult + 1;
    end;
  end;

  if bSign then Result := -LONGLONG(ullResult)
           else Result :=  LONGLONG(ullResult);
end;

function Int64x32Div32(a: LONGLONG; b, c, d: LongInt): LONGLONG;
var
  ua : ULARGE_INTEGER;
  ub :  DWORD;
  uc :  DWORD;
  bSign : BOOL;
  p0 : ULARGE_INTEGER;
  p1 : DWORD;
  x : ULARGE_INTEGER;
  ud0 : ULARGE_INTEGER;
  ud1 : DWORD;
  uliTotal : ULARGE_INTEGER;
  uliDividend : ULARGE_INTEGER;
  uliResult : ULARGE_INTEGER;
  dwDivisor : DWORD;
begin
  if a >= 0 then ua.QuadPart := DWORDLONG(a)
            else ua.QuadPart := DWORDLONG(-a);
  if b >= 0 then ub := DWORD(b)
            else ub := DWORD(-b);
  if c >= 0 then uc := DWORD(c)
            else uc := DWORD(-c);
  bSign := (a < 0) xor (b < 0);

  p0.QuadPart := UInt32x32To64(ua.LowPart, ub);

  if (ua.HighPart <> 0) then
  begin
    x.QuadPart := UInt32x32To64(ua.HighPart, ub) + p0.HighPart;
    p0.HighPart := x.LowPart;
    p1 := x.HighPart;
  end else
  begin
    p1 := 0;
  end;

  if (d <> 0) then
  begin
    if bSign then
    begin
      ud0.QuadPart := DWORDLONG(-(LONGLONG(d)));
      if (d > 0) then ud1 := DWORD(-1)
                 else ud1 := DWORD(0);
    end else
    begin
      ud0.QuadPart := DWORDLONG(d);
      if (d < 0) then ud1 := DWORD(-1)
                 else ud1 := DWORD(0);
    end;
    uliTotal.QuadPart := DWORDLONG(ud0.LowPart) + p0.LowPart;
    p0.LowPart := uliTotal.LowPart;

    uliTotal.LowPart := uliTotal.HighPart;
    uliTotal.HighPart := 0;

    uliTotal.QuadPart := uliTotal.QuadPart + (DWORDLONG(ud0.HighPart) + p0.HighPart);
    p0.HighPart := uliTotal.LowPart;

    p1 := p1 + ud1 + uliTotal.HighPart;

    if (LongInt(p1) < 0) then
    begin
      bSign := not bSign;

      p0.QuadPart := not p0.QuadPart;
      p1 := not p1;
      p0.QuadPart := p0.QuadPart + 1;
      p1 := p1 + DWORD(p0.QuadPart = 0);
    end;
  end;

  dwDivisor := uc;

  if (c < 0) then bSign := not bSign;

  if (uc <= p1) then
  begin
    if bSign then Result := LONGLONG($8000000000000000)
             else Result := LONGLONG($7FFFFFFFFFFFFFFF);
    Exit;
  end;

  uliDividend.HighPart := p1;
  uliDividend.LowPart := p0.HighPart;
  if (uliDividend.QuadPart >= DWORDLONG(dwDivisor)) then
  begin
    uliResult.HighPart := EnlargedUnsignedDivide(uliDividend, dwDivisor, @p0.HighPart);
  end else
  begin
    uliResult.HighPart := 0;
  end;

  uliResult.LowPart := EnlargedUnsignedDivide(p0, dwDivisor, nil);

  if bSign then Result := -LONGLONG(uliResult.QuadPart)
           else Result :=  LONGLONG(uliResult.QuadPart);
end;

function HRESULT_FROM_WIN32(x: DWORD): HRESULT;
begin
  if HRESULT(x) <= 0 then
    Result := HRESULT(x)
  else
    Result := HRESULT((x and $0000FFFF) or (FACILITY_WIN32 shl 16) or $80000000);
end;

function AmGetLastErrorToHResult: HRESULT;
var
  LastError: DWORD;
begin
  LastError := GetLastError;
  if(LastError <> 0) then Result := HRESULT_FROM_WIN32(LastError)
                     else Result := E_FAIL;
end;

function IsEqualObject(pFirst, pSecond: IUnknown): Boolean;
var
  pUnknown1,           // Retrieve the IUnknown interface
  pUnknown2: IUnknown; // Retrieve the other IUnknown interface
begin
  // Different objects can't have the same interface pointer for
  // any interface

  if (pFirst = pSecond) then
  begin
    Result := True;
    Exit;
  end;

  // OK - do it the hard way - check if they have the same
  // IUnknown pointers - a single object can only have one of these

  ASSERT(pFirst <> nil);
  ASSERT(pSecond <> nil);

  // See if the IUnknown pointers match

  Result := Succeeded(pFirst.QueryInterface(IUnknown,pUnknown1));
  if (Result) then
  begin
  end;
  ASSERT(Result);
  ASSERT(pUnknown1 <> nil);

  Result := Succeeded(pSecond.QueryInterface(IUnknown,pUnknown2));

  // get rid of Delphi compiler warnings ..
  if (Result) then
  begin
  end;

  ASSERT(Result);
  ASSERT(pUnknown2 <> nil);

  // Release the extra interfaces we hold

  Result := (pUnknown1 = pUnknown2);
  pUnknown1 := nil;
  pUnknown2 := nil;
end;
// milenko end

// milenko start namedguid implementation
function GetGUIDString(GUID: TGUID): String;
begin
  if IsEqualGUID(GUID,MEDIASUBTYPE_AIFF) then Result := 'MEDIASUBTYPE_AIFF'
  else if IsEqualGUID(GUID,MEDIASUBTYPE_AU) then Result := 'MEDIASUBTYPE_AU'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_AnalogVideo_NTSC_M) then Result := 'MEDIASUBTYPE_AnalogVideo_NTSC_M'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_AnalogVideo_PAL_B) then Result := 'MEDIASUBTYPE_AnalogVideo_PAL_B'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_AnalogVideo_PAL_D) then Result := 'MEDIASUBTYPE_AnalogVideo_PAL_D'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_AnalogVideo_PAL_G) then Result := 'MEDIASUBTYPE_AnalogVideo_PAL_G'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_AnalogVideo_PAL_H) then Result := 'MEDIASUBTYPE_AnalogVideo_PAL_H'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_AnalogVideo_PAL_I) then Result := 'MEDIASUBTYPE_AnalogVideo_PAL_I'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_AnalogVideo_PAL_M) then Result := 'MEDIASUBTYPE_AnalogVideo_PAL_M'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_AnalogVideo_PAL_N) then Result := 'MEDIASUBTYPE_AnalogVideo_PAL_N'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_AnalogVideo_SECAM_B) then Result := 'MEDIASUBTYPE_AnalogVideo_SECAM_B'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_AnalogVideo_SECAM_D) then Result := 'MEDIASUBTYPE_AnalogVideo_SECAM_D'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_AnalogVideo_SECAM_G) then Result := 'MEDIASUBTYPE_AnalogVideo_SECAM_G'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_AnalogVideo_SECAM_H) then Result := 'MEDIASUBTYPE_AnalogVideo_SECAM_H'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_AnalogVideo_SECAM_K) then Result := 'MEDIASUBTYPE_AnalogVideo_SECAM_K'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_AnalogVideo_SECAM_K1) then Result := 'MEDIASUBTYPE_AnalogVideo_SECAM_K1'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_AnalogVideo_SECAM_L) then Result := 'MEDIASUBTYPE_AnalogVideo_SECAM_L'

  else if IsEqualGuid(GUID,MEDIASUBTYPE_ARGB1555) then Result := 'MEDIASUBTYPE_ARGB1555'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_ARGB4444) then Result := 'MEDIASUBTYPE_ARGB4444'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_ARGB32) then Result := 'MEDIASUBTYPE_ARGB32'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_A2R10G10B10) then Result := 'MEDIASUBTYPE_A2R10G10B10'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_A2B10G10R10) then Result := 'MEDIASUBTYPE_A2B10G10R10'

  else if IsEqualGuid(GUID,MEDIASUBTYPE_AYUV) then Result := 'MEDIASUBTYPE_AYUV'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_AI44) then Result := 'MEDIASUBTYPE_AI44'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_IA44) then Result := 'MEDIASUBTYPE_IA44'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_NV12) then Result := 'MEDIASUBTYPE_NV12'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_IMC1) then Result := 'MEDIASUBTYPE_IMC1'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_IMC2) then Result := 'MEDIASUBTYPE_IMC2'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_IMC3) then Result := 'MEDIASUBTYPE_IMC3'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_IMC4) then Result := 'MEDIASUBTYPE_IMC4'

  else if IsEqualGuid(GUID,MEDIASUBTYPE_Asf) then Result := 'MEDIASUBTYPE_Asf'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_Avi) then Result := 'MEDIASUBTYPE_Avi'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_CFCC) then Result := 'MEDIASUBTYPE_CFCC'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_CLJR) then Result := 'MEDIASUBTYPE_CLJR'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_CPLA) then Result := 'MEDIASUBTYPE_CPLA'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_CLPL) then Result := 'MEDIASUBTYPE_CLPL'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_DOLBY_AC3) then Result := 'MEDIASUBTYPE_DOLBY_AC3'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_DOLBY_AC3_SPDIF) then Result := 'MEDIASUBTYPE_DOLBY_AC3_SPDIF'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_DVCS) then Result := 'MEDIASUBTYPE_DVCS'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_DVD_LPCM_AUDIO) then Result := 'MEDIASUBTYPE_DVD_LPCM_AUDIO'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_DVD_NAVIGATION_DSI) then Result := 'MEDIASUBTYPE_DVD_NAVIGATION_DSI'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_DVD_NAVIGATION_PCI) then Result := 'MEDIASUBTYPE_DVD_NAVIGATION_PCI'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_DVD_NAVIGATION_PROVIDER) then Result := 'MEDIASUBTYPE_DVD_NAVIGATION_PROVIDER'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_DVD_SUBPICTURE) then Result := 'MEDIASUBTYPE_DVD_SUBPICTURE'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_DVSD) then Result := 'MEDIASUBTYPE_DVSD'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_DRM_Audio) then Result := 'MEDIASUBTYPE_DRM_Audio'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_DssAudio) then Result := 'MEDIASUBTYPE_DssAudio'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_DssVideo) then Result := 'MEDIASUBTYPE_DssVideo'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_IF09) then Result := 'MEDIASUBTYPE_IF09'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_IEEE_FLOAT) then Result := 'MEDIASUBTYPE_IEEE_FLOAT'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_IJPG) then Result := 'MEDIASUBTYPE_IJPG'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_IYUV) then Result := 'MEDIASUBTYPE_IYUV'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_Line21_BytePair) then Result := 'MEDIASUBTYPE_Line21_BytePair'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_Line21_GOPPacket) then Result := 'MEDIASUBTYPE_Line21_GOPPacket'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_Line21_VBIRawData) then Result := 'MEDIASUBTYPE_Line21_VBIRawData'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_MDVF) then Result := 'MEDIASUBTYPE_MDVF'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_MJPG) then Result := 'MEDIASUBTYPE_MJPG'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_MPEG1Audio) then Result := 'MEDIASUBTYPE_MPEG1Audio'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_MPEG1AudioPayload) then Result := 'MEDIASUBTYPE_MPEG1AudioPayload'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_MPEG1Packet) then Result := 'MEDIASUBTYPE_MPEG1Packet'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_MPEG1Payload) then Result := 'MEDIASUBTYPE_MPEG1Payload'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_MPEG1System) then Result := 'MEDIASUBTYPE_MPEG1System'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_MPEG1Video) then Result := 'MEDIASUBTYPE_MPEG1Video'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_MPEG1VideoCD) then Result := 'MEDIASUBTYPE_MPEG1VideoCD'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_MPEG2_AUDIO) then Result := 'MEDIASUBTYPE_MPEG2_AUDIO'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_MPEG2_PROGRAM) then Result := 'MEDIASUBTYPE_MPEG2_PROGRAM'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_MPEG2_TRANSPORT) then Result := 'MEDIASUBTYPE_MPEG2_TRANSPORT'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_MPEG2_VIDEO) then Result := 'MEDIASUBTYPE_MPEG2_VIDEO'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_None) then Result := 'MEDIASUBTYPE_None'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_Overlay) then Result := 'MEDIASUBTYPE_Overlay'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_PCM) then Result := 'MEDIASUBTYPE_PCM'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_PCMAudio_Obsolete) then Result := 'MEDIASUBTYPE_PCMAudio_Obsolete'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_Plum) then Result := 'MEDIASUBTYPE_Plum'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_QTJpeg) then Result := 'MEDIASUBTYPE_QTJpeg'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_QTMovie) then Result := 'MEDIASUBTYPE_QTMovie'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_QTRle) then Result := 'MEDIASUBTYPE_QTRle'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_QTRpza) then Result := 'MEDIASUBTYPE_QTRpza'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_QTSmc) then Result := 'MEDIASUBTYPE_QTSmc'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_RAW_SPORT) then Result := 'MEDIASUBTYPE_RAW_SPORT'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_RGB1) then Result := 'MEDIASUBTYPE_RGB1'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_RGB24) then Result := 'MEDIASUBTYPE_RGB24'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_RGB32) then Result := 'MEDIASUBTYPE_RGB32'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_RGB4) then Result := 'MEDIASUBTYPE_RGB4'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_RGB555) then Result := 'MEDIASUBTYPE_RGB555'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_RGB565) then Result := 'MEDIASUBTYPE_RGB565'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_RGB8) then Result := 'MEDIASUBTYPE_RGB8'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_SPDIF_TAG_241h) then Result := 'MEDIASUBTYPE_SPDIF_TAG_241h'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_TELETEXT) then Result := 'MEDIASUBTYPE_TELETEXT'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_TVMJ) then Result := 'MEDIASUBTYPE_TVMJ'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_UYVY) then Result := 'MEDIASUBTYPE_UYVY'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_VPVBI) then Result := 'MEDIASUBTYPE_VPVBI'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_VPVideo) then Result := 'MEDIASUBTYPE_VPVideo'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_WAKE) then Result := 'MEDIASUBTYPE_WAKE'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_WAVE) then Result := 'MEDIASUBTYPE_WAVE'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_Y211) then Result := 'MEDIASUBTYPE_Y211'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_Y411) then Result := 'MEDIASUBTYPE_Y411'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_Y41P) then Result := 'MEDIASUBTYPE_Y41P'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_YUY2) then Result := 'MEDIASUBTYPE_YUY2'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_YV12) then Result := 'MEDIASUBTYPE_YV12'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_YVU9) then Result := 'MEDIASUBTYPE_YVU9'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_YVYU) then Result := 'MEDIASUBTYPE_YVYU'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_YUYV) then Result := 'MEDIASUBTYPE_YUYV'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_dvhd) then Result := 'MEDIASUBTYPE_dvhd'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_dvsd) then Result := 'MEDIASUBTYPE_dvsd'
  else if IsEqualGuid(GUID,MEDIASUBTYPE_dvsl) then Result := 'MEDIASUBTYPE_dvsl'

  else if IsEqualGuid(GUID,MEDIATYPE_AUXLine21Data) then Result := 'MEDIATYPE_AUXLine21Data'
  else if IsEqualGuid(GUID,MEDIATYPE_AnalogAudio) then Result := 'MEDIATYPE_AnalogAudio'
  else if IsEqualGuid(GUID,MEDIATYPE_AnalogVideo) then Result := 'MEDIATYPE_AnalogVideo'
  else if IsEqualGuid(GUID,MEDIATYPE_Audio) then Result := 'MEDIATYPE_Audio'
  else if IsEqualGuid(GUID,MEDIATYPE_DVD_ENCRYPTED_PACK) then Result := 'MEDIATYPE_DVD_ENCRYPTED_PACK'
  else if IsEqualGuid(GUID,MEDIATYPE_DVD_NAVIGATION) then Result := 'MEDIATYPE_DVD_NAVIGATION'
  else if IsEqualGuid(GUID,MEDIATYPE_File) then Result := 'MEDIATYPE_File'
  else if IsEqualGuid(GUID,MEDIATYPE_Interleaved) then Result := 'MEDIATYPE_Interleaved'
  else if IsEqualGuid(GUID,MEDIATYPE_LMRT) then Result := 'MEDIATYPE_LMRT'
  else if IsEqualGuid(GUID,MEDIATYPE_MPEG1SystemStream) then Result := 'MEDIATYPE_MPEG1SystemStream'
  else if IsEqualGuid(GUID,MEDIATYPE_MPEG2_PES) then Result := 'MEDIATYPE_MPEG2_PES'
  else if IsEqualGuid(GUID,MEDIATYPE_Midi) then Result := 'MEDIATYPE_Midi'
  else if IsEqualGuid(GUID,MEDIATYPE_ScriptCommand) then Result := 'MEDIATYPE_ScriptCommand'
  else if IsEqualGuid(GUID,MEDIATYPE_Stream) then Result := 'MEDIATYPE_Stream'
  else if IsEqualGuid(GUID,MEDIATYPE_Text) then Result := 'MEDIATYPE_Text'
  else if IsEqualGuid(GUID,MEDIATYPE_Timecode) then Result := 'MEDIATYPE_Timecode'
  else if IsEqualGuid(GUID,MEDIATYPE_URL_STREAM) then Result := 'MEDIATYPE_URL_STREAM'
  else if IsEqualGuid(GUID,MEDIATYPE_VBI) then Result := 'MEDIATYPE_VBI'
  else if IsEqualGuid(GUID,MEDIATYPE_Video) then Result := 'MEDIATYPE_Video'

  else if IsEqualGuid(GUID,WMMEDIATYPE_Audio) then Result := 'WMMEDIATYPE_Audio'
  else if IsEqualGuid(GUID,WMMEDIATYPE_Video) then Result := 'WMMEDIATYPE_Video'
  else if IsEqualGuid(GUID,WMMEDIATYPE_Script) then Result := 'WMMEDIATYPE_Script'
  else if IsEqualGuid(GUID,WMMEDIATYPE_Image) then Result := 'WMMEDIATYPE_Image'
  else if IsEqualGuid(GUID,WMMEDIATYPE_FileTransfer) then Result := 'WMMEDIATYPE_FileTransfer'
  else if IsEqualGuid(GUID,WMMEDIATYPE_Text) then Result := 'WMMEDIATYPE_Text'

  else if IsEqualGuid(GUID,WMMEDIASUBTYPE_Base) then Result := 'WMMEDIASUBTYPE_Base'
  else if IsEqualGuid(GUID,WMMEDIASUBTYPE_RGB1) then Result := 'WMMEDIASUBTYPE_RGB1'
  else if IsEqualGuid(GUID,WMMEDIASUBTYPE_RGB4) then Result := 'WMMEDIASUBTYPE_RGB4'
  else if IsEqualGuid(GUID,WMMEDIASUBTYPE_RGB8) then Result := 'WMMEDIASUBTYPE_RGB8'
  else if IsEqualGuid(GUID,WMMEDIASUBTYPE_RGB565) then Result := 'WMMEDIASUBTYPE_RGB565'
  else if IsEqualGuid(GUID,WMMEDIASUBTYPE_RGB555) then Result := 'WMMEDIASUBTYPE_RGB555'
  else if IsEqualGuid(GUID,WMMEDIASUBTYPE_RGB24) then Result := 'WMMEDIASUBTYPE_RGB24'
  else if IsEqualGuid(GUID,WMMEDIASUBTYPE_RGB32) then Result := 'WMMEDIASUBTYPE_RGB32'
  else if IsEqualGuid(GUID,WMMEDIASUBTYPE_I420) then Result := 'WMMEDIASUBTYPE_I420'
  else if IsEqualGuid(GUID,WMMEDIASUBTYPE_IYUV) then Result := 'WMMEDIASUBTYPE_IYUV'
  else if IsEqualGuid(GUID,WMMEDIASUBTYPE_YV12) then Result := 'WMMEDIASUBTYPE_YV12'
  else if IsEqualGuid(GUID,WMMEDIASUBTYPE_YUY2) then Result := 'WMMEDIASUBTYPE_YUY2'
  else if IsEqualGuid(GUID,WMMEDIASUBTYPE_UYVY) then Result := 'WMMEDIASUBTYPE_UYVY'
  else if IsEqualGuid(GUID,WMMEDIASUBTYPE_YVYU) then Result := 'WMMEDIASUBTYPE_YVYU'
  else if IsEqualGuid(GUID,WMMEDIASUBTYPE_YVU9) then Result := 'WMMEDIASUBTYPE_YVU9'
  else if IsEqualGuid(GUID,WMMEDIASUBTYPE_MP43) then Result := 'WMMEDIASUBTYPE_MP43'
  else if IsEqualGuid(GUID,WMMEDIASUBTYPE_MP4S) then Result := 'WMMEDIASUBTYPE_MP4S'

  else if IsEqualGuid(GUID,WMMEDIASUBTYPE_WMV1) then Result := 'WMMEDIASUBTYPE_WMV1'
  else if IsEqualGuid(GUID,WMMEDIASUBTYPE_WMV2) then Result := 'WMMEDIASUBTYPE_WMV2'
  else if IsEqualGuid(GUID,WMMEDIASUBTYPE_WMV3) then Result := 'WMMEDIASUBTYPE_WMV3'
  else if IsEqualGuid(GUID,WMMEDIASUBTYPE_MSS1) then Result := 'WMMEDIASUBTYPE_MSS1'
  else if IsEqualGuid(GUID,WMMEDIASUBTYPE_MSS2) then Result := 'WMMEDIASUBTYPE_MSS2'
  else if IsEqualGuid(GUID,WMMEDIASUBTYPE_MPEG2_VIDEO) then Result := 'WMMEDIASUBTYPE_MPEG2_VIDEO'
  else if IsEqualGuid(GUID,WMMEDIASUBTYPE_PCM) then Result := 'WMMEDIASUBTYPE_PCM'
  else if IsEqualGuid(GUID,WMMEDIASUBTYPE_DRM) then Result := 'WMMEDIASUBTYPE_DRM'
  else if IsEqualGuid(GUID,WMMEDIASUBTYPE_WMAudioV9) then Result := 'WMMEDIASUBTYPE_WMAudioV9'
  else if IsEqualGuid(GUID,WMMEDIASUBTYPE_WMAudio_Lossless) then Result := 'WMMEDIASUBTYPE_WMAudio_Lossless'
  else if IsEqualGuid(GUID,WMMEDIASUBTYPE_WMAudioV8) then Result := 'WMMEDIASUBTYPE_WMAudioV8'
  else if IsEqualGuid(GUID,WMMEDIASUBTYPE_WMAudioV7) then Result := 'WMMEDIASUBTYPE_WMAudioV7'
  else if IsEqualGuid(GUID,WMMEDIASUBTYPE_WMAudioV2) then Result := 'WMMEDIASUBTYPE_WMAudioV2'
  else if IsEqualGuid(GUID,WMMEDIASUBTYPE_ACELPnet) then Result := 'WMMEDIASUBTYPE_ACELPnet'
  else if IsEqualGuid(GUID,WMMEDIASUBTYPE_WMSP1) then Result := 'WMMEDIASUBTYPE_WMSP1'

  else if IsEqualGuid(GUID,WMFORMAT_VideoInfo) then Result := 'WMFORMAT_VideoInfo'
  else if IsEqualGuid(GUID,WMFORMAT_WaveFormatEx) then Result := 'WMFORMAT_WaveFormatEx'
  else if IsEqualGuid(GUID,WMFORMAT_Script) then Result := 'WMFORMAT_Script'
  else if IsEqualGuid(GUID,WMFORMAT_MPEG2Video) then Result := 'WMFORMAT_MPEG2Video'

  else if IsEqualGuid(GUID,WMSCRIPTTYPE_TwoStrings) then Result := 'WMSCRIPTTYPE_TwoStrings'

  else if IsEqualGuid(GUID,PIN_CATEGORY_ANALOGVIDEOIN) then Result := 'PIN_CATEGORY_ANALOGVIDEOIN'
  else if IsEqualGuid(GUID,PIN_CATEGORY_CAPTURE) then Result := 'PIN_CATEGORY_CAPTURE'
  else if IsEqualGuid(GUID,PIN_CATEGORY_CC) then Result := 'PIN_CATEGORY_CC'
  else if IsEqualGuid(GUID,PIN_CATEGORY_EDS) then Result := 'PIN_CATEGORY_EDS'
  else if IsEqualGuid(GUID,PIN_CATEGORY_NABTS) then Result := 'PIN_CATEGORY_NABTS'
  else if IsEqualGuid(GUID,PIN_CATEGORY_PREVIEW) then Result := 'PIN_CATEGORY_PREVIEW'
  else if IsEqualGuid(GUID,PIN_CATEGORY_STILL) then Result := 'PIN_CATEGORY_STILL'
  else if IsEqualGuid(GUID,PIN_CATEGORY_TELETEXT) then Result := 'PIN_CATEGORY_TELETEXT'
  else if IsEqualGuid(GUID,PIN_CATEGORY_TIMECODE) then Result := 'PIN_CATEGORY_TIMECODE'
  else if IsEqualGuid(GUID,PIN_CATEGORY_VBI) then Result := 'PIN_CATEGORY_VBI'
  else if IsEqualGuid(GUID,PIN_CATEGORY_VIDEOPORT) then Result := 'PIN_CATEGORY_VIDEOPORT'
  else if IsEqualGuid(GUID,PIN_CATEGORY_VIDEOPORT_VBI) then Result := 'PIN_CATEGORY_VIDEOPORT_VBI'

  else if IsEqualGuid(GUID,CLSID_ACMWrapper) then Result := 'CLSID_ACMWrapper'
  else if IsEqualGuid(GUID,CLSID_AVICo) then Result := 'CLSID_AVICo'
  else if IsEqualGuid(GUID,CLSID_AVIDec) then Result := 'CLSID_AVIDec'
  else if IsEqualGuid(GUID,CLSID_AVIDoc) then Result := 'CLSID_AVIDoc'
  else if IsEqualGuid(GUID,CLSID_AVIDraw) then Result := 'CLSID_AVIDraw'
  else if IsEqualGuid(GUID,CLSID_AVIMIDIRender) then Result := 'CLSID_AVIMIDIRender'
  else if IsEqualGuid(GUID,CLSID_ActiveMovieCategories) then Result := 'CLSID_ActiveMovieCategories'
  else if IsEqualGuid(GUID,CLSID_AnalogVideoDecoderPropertyPage) then Result := 'CLSID_AnalogVideoDecoderPropertyPage'
  else if IsEqualGuid(GUID,CLSID_WMAsfReader) then Result := 'CLSID_WMAsfReader'
  else if IsEqualGuid(GUID,CLSID_WMAsfWriter) then Result := 'CLSID_WMAsfWriter'
  else if IsEqualGuid(GUID,CLSID_AsyncReader) then Result := 'CLSID_AsyncReader'
  else if IsEqualGuid(GUID,CLSID_AudioCompressorCategory) then Result := 'CLSID_AudioCompressorCategory'
  else if IsEqualGuid(GUID,CLSID_AudioInputDeviceCategory) then Result := 'CLSID_AudioInputDeviceCategory'
  else if IsEqualGuid(GUID,CLSID_AudioProperties) then Result := 'CLSID_AudioProperties'
  else if IsEqualGuid(GUID,CLSID_AudioRecord) then Result := 'CLSID_AudioRecord'
  else if IsEqualGuid(GUID,CLSID_AudioRender) then Result := 'CLSID_AudioRender'
  else if IsEqualGuid(GUID,CLSID_AudioRendererCategory) then Result := 'CLSID_AudioRendererCategory'
  else if IsEqualGuid(GUID,CLSID_AviDest) then Result := 'CLSID_AviDest'
  else if IsEqualGuid(GUID,CLSID_AviMuxProptyPage) then Result := 'CLSID_AviMuxProptyPage'
  else if IsEqualGuid(GUID,CLSID_AviMuxProptyPage1) then Result := 'CLSID_AviMuxProptyPage1'
  else if IsEqualGuid(GUID,CLSID_AviReader) then Result := 'CLSID_AviReader'
  else if IsEqualGuid(GUID,CLSID_AviSplitter) then Result := 'CLSID_AviSplitter'
  else if IsEqualGuid(GUID,CLSID_CAcmCoClassManager) then Result := 'CLSID_CAcmCoClassManager'
  else if IsEqualGuid(GUID,CLSID_CDeviceMoniker) then Result := 'CLSID_CDeviceMoniker'
  else if IsEqualGuid(GUID,CLSID_CIcmCoClassManager) then Result := 'CLSID_CIcmCoClassManager'
  else if IsEqualGuid(GUID,CLSID_CMidiOutClassManager) then Result := 'CLSID_CMidiOutClassManager'
  else if IsEqualGuid(GUID,CLSID_CMpegAudioCodec) then Result := 'CLSID_CMpegAudioCodec'
  else if IsEqualGuid(GUID,CLSID_CMpegVideoCodec) then Result := 'CLSID_CMpegVideoCodec'
  else if IsEqualGuid(GUID,CLSID_CQzFilterClassManager) then Result := 'CLSID_CQzFilterClassManager'
  else if IsEqualGuid(GUID,CLSID_CVidCapClassManager) then Result := 'CLSID_CVidCapClassManager'
  else if IsEqualGuid(GUID,CLSID_CWaveOutClassManager) then Result := 'CLSID_CWaveOutClassManager'
  else if IsEqualGuid(GUID,CLSID_CWaveinClassManager) then Result := 'CLSID_CWaveinClassManager'
  else if IsEqualGuid(GUID,CLSID_CameraControlPropertyPage) then Result := 'CLSID_CameraControlPropertyPage'
  else if IsEqualGuid(GUID,CLSID_CaptureGraphBuilder) then Result := 'CLSID_CaptureGraphBuilder'
  else if IsEqualGuid(GUID,CLSID_CaptureProperties) then Result := 'CLSID_CaptureProperties'
  else if IsEqualGuid(GUID,CLSID_Colour) then Result := 'CLSID_Colour'
  else if IsEqualGuid(GUID,CLSID_CrossbarFilterPropertyPage) then Result := 'CLSID_CrossbarFilterPropertyPage'
  else if IsEqualGuid(GUID,CLSID_DSoundRender) then Result := 'CLSID_DSoundRender'
  else if IsEqualGuid(GUID,CLSID_DVDHWDecodersCategory) then Result := 'CLSID_DVDHWDecodersCategory'
  else if IsEqualGuid(GUID,CLSID_DVDNavigator) then Result := 'CLSID_DVDNavigator'
  else if IsEqualGuid(GUID,CLSID_DVDecPropertiesPage) then Result := 'CLSID_DVDecPropertiesPage'
  else if IsEqualGuid(GUID,CLSID_DVEncPropertiesPage) then Result := 'CLSID_DVEncPropertiesPage'
  else if IsEqualGuid(GUID,CLSID_DVMux) then Result := 'CLSID_DVMux'
  else if IsEqualGuid(GUID,CLSID_DVMuxPropertyPage) then Result := 'CLSID_DVMuxPropertyPage'
  else if IsEqualGuid(GUID,CLSID_DVSplitter) then Result := 'CLSID_DVSplitter'
  else if IsEqualGuid(GUID,CLSID_DVVideoCodec) then Result := 'CLSID_DVVideoCodec'
  else if IsEqualGuid(GUID,CLSID_DVVideoEnc) then Result := 'CLSID_DVVideoEnc'
  else if IsEqualGuid(GUID,CLSID_DirectDraw) then Result := 'CLSID_DirectDraw'
  else if IsEqualGuid(GUID,CLSID_DirectDrawClipper) then Result := 'CLSID_DirectDrawClipper'
  else if IsEqualGuid(GUID,CLSID_DirectDrawProperties) then Result := 'CLSID_DirectDrawProperties'
  else if IsEqualGuid(GUID,CLSID_Dither) then Result := 'CLSID_Dither'
  else if IsEqualGuid(GUID,CLSID_DvdGraphBuilder) then Result := 'CLSID_DvdGraphBuilder'
  else if IsEqualGuid(GUID,CLSID_FGControl) then Result := 'CLSID_FGControl'
  else if IsEqualGuid(GUID,CLSID_FileSource) then Result := 'CLSID_FileSource'
  else if IsEqualGuid(GUID,CLSID_FileWriter) then Result := 'CLSID_FileWriter'
  else if IsEqualGuid(GUID,CLSID_FilterGraph) then Result := 'CLSID_FilterGraph'
  else if IsEqualGuid(GUID,CLSID_FilterGraphNoThread) then Result := 'CLSID_FilterGraphNoThread'
  else if IsEqualGuid(GUID,CLSID_FilterMapper) then Result := 'CLSID_FilterMapper'
  else if IsEqualGuid(GUID,CLSID_FilterMapper2) then Result := 'CLSID_FilterMapper2'
  else if IsEqualGuid(GUID,CLSID_InfTee) then Result := 'CLSID_InfTee'
  else if IsEqualGuid(GUID,CLSID_LegacyAmFilterCategory) then Result := 'CLSID_LegacyAmFilterCategory'
  else if IsEqualGuid(GUID,CLSID_Line21Decoder) then Result := 'CLSID_Line21Decoder'
  else if IsEqualGuid(GUID,CLSID_MOVReader) then Result := 'CLSID_MOVReader'
  else if IsEqualGuid(GUID,CLSID_MPEG1Doc) then Result := 'CLSID_MPEG1Doc'
  else if IsEqualGuid(GUID,CLSID_MPEG1PacketPlayer) then Result := 'CLSID_MPEG1PacketPlayer'
  else if IsEqualGuid(GUID,CLSID_MPEG1Splitter) then Result := 'CLSID_MPEG1Splitter'
  else if IsEqualGuid(GUID,CLSID_MediaPropertyBag) then Result := 'CLSID_MediaPropertyBag'
  else if IsEqualGuid(GUID,CLSID_MemoryAllocator) then Result := 'CLSID_MemoryAllocator'
  else if IsEqualGuid(GUID,CLSID_MidiRendererCategory) then Result := 'CLSID_MidiRendererCategory'
  else if IsEqualGuid(GUID,CLSID_ModexProperties) then Result := 'CLSID_ModexProperties'
  else if IsEqualGuid(GUID,CLSID_ModexRenderer) then Result := 'CLSID_ModexRenderer'
  else if IsEqualGuid(GUID,CLSID_OverlayMixer) then Result := 'CLSID_OverlayMixer'
  else if IsEqualGuid(GUID,CLSID_PerformanceProperties) then Result := 'CLSID_PerformanceProperties'
  else if IsEqualGuid(GUID,CLSID_PersistMonikerPID) then Result := 'CLSID_PersistMonikerPID'
  else if IsEqualGuid(GUID,CLSID_ProtoFilterGraph) then Result := 'CLSID_ProtoFilterGraph'
  else if IsEqualGuid(GUID,CLSID_QualityProperties) then Result := 'CLSID_QualityProperties'
  else if IsEqualGuid(GUID,CLSID_SeekingPassThru) then Result := 'CLSID_SeekingPassThru'
  else if IsEqualGuid(GUID,CLSID_SmartTee) then Result := 'CLSID_SmartTee'
  else if IsEqualGuid(GUID,CLSID_SystemClock) then Result := 'CLSID_SystemClock'
  else if IsEqualGuid(GUID,CLSID_SystemDeviceEnum) then Result := 'CLSID_SystemDeviceEnum'
  else if IsEqualGuid(GUID,CLSID_TVAudioFilterPropertyPage) then Result := 'CLSID_TVAudioFilterPropertyPage'
  else if IsEqualGuid(GUID,CLSID_TVTunerFilterPropertyPage) then Result := 'CLSID_TVTunerFilterPropertyPage'
  else if IsEqualGuid(GUID,CLSID_TextRender) then Result := 'CLSID_TextRender'
  else if IsEqualGuid(GUID,CLSID_URLReader) then Result := 'CLSID_URLReader'
  else if IsEqualGuid(GUID,CLSID_VBISurfaces) then Result := 'CLSID_VBISurfaces'
  else if IsEqualGuid(GUID,CLSID_VPObject) then Result := 'CLSID_VPObject'
  else if IsEqualGuid(GUID,CLSID_VPVBIObject) then Result := 'CLSID_VPVBIObject'
  else if IsEqualGuid(GUID,CLSID_VfwCapture) then Result := 'CLSID_VfwCapture'
  else if IsEqualGuid(GUID,CLSID_VideoCompressorCategory) then Result := 'CLSID_VideoCompressorCategory'
  else if IsEqualGuid(GUID,CLSID_VideoInputDeviceCategory) then Result := 'CLSID_VideoInputDeviceCategory'
  else if IsEqualGuid(GUID,CLSID_VideoProcAmpPropertyPage) then Result := 'CLSID_VideoProcAmpPropertyPage'
  else if IsEqualGuid(GUID,CLSID_VideoRenderer) then Result := 'CLSID_VideoRenderer'
  else if IsEqualGuid(GUID,CLSID_VideoStreamConfigPropertyPage) then Result := 'CLSID_VideoStreamConfigPropertyPage'

  else if IsEqualGuid(GUID,CLSID_WMMUTEX_Language) then Result := 'CLSID_WMMUTEX_Language'
  else if IsEqualGuid(GUID,CLSID_WMMUTEX_Bitrate) then Result := 'CLSID_WMMUTEX_Bitrate'
  else if IsEqualGuid(GUID,CLSID_WMMUTEX_Presentation) then Result := 'CLSID_WMMUTEX_Presentation'
  else if IsEqualGuid(GUID,CLSID_WMMUTEX_Unknown) then Result := 'CLSID_WMMUTEX_Unknown'

  else if IsEqualGuid(GUID,CLSID_WMBandwidthSharing_Exclusive) then Result := 'CLSID_WMBandwidthSharing_Exclusive'
  else if IsEqualGuid(GUID,CLSID_WMBandwidthSharing_Partial) then Result := 'CLSID_WMBandwidthSharing_Partial'

  else if IsEqualGuid(GUID,FORMAT_AnalogVideo) then Result := 'FORMAT_AnalogVideo'
  else if IsEqualGuid(GUID,FORMAT_DVD_LPCMAudio) then Result := 'FORMAT_DVD_LPCMAudio'
  else if IsEqualGuid(GUID,FORMAT_DolbyAC3) then Result := 'FORMAT_DolbyAC3'
  else if IsEqualGuid(GUID,FORMAT_DvInfo) then Result := 'FORMAT_DvInfo'
  else if IsEqualGuid(GUID,FORMAT_MPEG2Audio) then Result := 'FORMAT_MPEG2Audio'
  else if IsEqualGuid(GUID,FORMAT_MPEG2Video) then Result := 'FORMAT_MPEG2Video'
  else if IsEqualGuid(GUID,FORMAT_MPEG2_VIDEO) then Result := 'FORMAT_MPEG2_VIDEO'
  else if IsEqualGuid(GUID,FORMAT_MPEGStreams) then Result := 'FORMAT_MPEGStreams'
  else if IsEqualGuid(GUID,FORMAT_MPEGVideo) then Result := 'FORMAT_MPEGVideo'
  else if IsEqualGuid(GUID,FORMAT_None) then Result := 'FORMAT_None'
  else if IsEqualGuid(GUID,FORMAT_VIDEOINFO2) then Result := 'FORMAT_VIDEOINFO2'
  else if IsEqualGuid(GUID,FORMAT_VideoInfo) then Result := 'FORMAT_VideoInfo'
  else if IsEqualGuid(GUID,FORMAT_VideoInfo2) then Result := 'FORMAT_VideoInfo2'
  else if IsEqualGuid(GUID,FORMAT_WaveFormatEx) then Result := 'FORMAT_WaveFormatEx'

  else if IsEqualGuid(GUID,TIME_FORMAT_BYTE) then Result := 'TIME_FORMAT_BYTE'
  else if IsEqualGuid(GUID,TIME_FORMAT_FIELD) then Result := 'TIME_FORMAT_FIELD'
  else if IsEqualGuid(GUID,TIME_FORMAT_FRAME) then Result := 'TIME_FORMAT_FRAME'
  else if IsEqualGuid(GUID,TIME_FORMAT_MEDIA_TIME) then Result := 'TIME_FORMAT_MEDIA_TIME'
  else if IsEqualGuid(GUID,TIME_FORMAT_SAMPLE) then Result := 'TIME_FORMAT_SAMPLE'

  else if IsEqualGuid(GUID,AMPROPSETID_Pin) then Result := 'AMPROPSETID_Pin'
  else if IsEqualGuid(GUID,AM_INTERFACESETID_Standard) then Result := 'AM_INTERFACESETID_Standard'
  else if IsEqualGuid(GUID,AM_KSCATEGORY_AUDIO) then Result := 'AM_KSCATEGORY_AUDIO'
  else if IsEqualGuid(GUID,AM_KSCATEGORY_CAPTURE) then Result := 'AM_KSCATEGORY_CAPTURE'
  else if IsEqualGuid(GUID,AM_KSCATEGORY_CROSSBAR) then Result := 'AM_KSCATEGORY_CROSSBAR'
  else if IsEqualGuid(GUID,AM_KSCATEGORY_DATACOMPRESSOR) then Result := 'AM_KSCATEGORY_DATACOMPRESSOR'
  else if IsEqualGuid(GUID,AM_KSCATEGORY_RENDER) then Result := 'AM_KSCATEGORY_RENDER'
  else if IsEqualGuid(GUID,AM_KSCATEGORY_TVAUDIO) then Result := 'AM_KSCATEGORY_TVAUDIO'
  else if IsEqualGuid(GUID,AM_KSCATEGORY_TVTUNER) then Result := 'AM_KSCATEGORY_TVTUNER'
  else if IsEqualGuid(GUID,AM_KSCATEGORY_VIDEO) then Result := 'AM_KSCATEGORY_VIDEO'
  else if IsEqualGuid(GUID,AM_KSPROPSETID_AC3) then Result := 'AM_KSPROPSETID_AC3'
  else if IsEqualGuid(GUID,AM_KSPROPSETID_CopyProt) then Result := 'AM_KSPROPSETID_CopyProt'
  else if IsEqualGuid(GUID,AM_KSPROPSETID_DvdSubPic) then Result := 'AM_KSPROPSETID_DvdSubPic'
  else if IsEqualGuid(GUID,AM_KSPROPSETID_TSRateChange) then Result := 'AM_KSPROPSETID_TSRateChange'

  else if IsEqualGuid(GUID,IID_IAMDirectSound) then Result := 'IID_IAMDirectSound'
  else if IsEqualGuid(GUID,IID_IAMLine21Decoder) then Result := 'IID_IAMLine21Decoder'
  else if IsEqualGuid(GUID,IID_IBaseVideoMixer) then Result := 'IID_IBaseVideoMixer'
  else if IsEqualGuid(GUID,IID_IDDVideoPortContainer) then Result := 'IID_IDDVideoPortContainer'
  else if IsEqualGuid(GUID,IID_IDirectDraw) then Result := 'IID_IDirectDraw'
  else if IsEqualGuid(GUID,IID_IDirectDraw2) then Result := 'IID_IDirectDraw2'
  else if IsEqualGuid(GUID,IID_IDirectDrawClipper) then Result := 'IID_IDirectDrawClipper'
  else if IsEqualGuid(GUID,IID_IDirectDrawColorControl) then Result := 'IID_IDirectDrawColorControl'
  else if IsEqualGuid(GUID,IID_IDirectDrawKernel) then Result := 'IID_IDirectDrawKernel'
  else if IsEqualGuid(GUID,IID_IDirectDrawPalette) then Result := 'IID_IDirectDrawPalette'
  else if IsEqualGuid(GUID,IID_IDirectDrawSurface) then Result := 'IID_IDirectDrawSurface'
  else if IsEqualGuid(GUID,IID_IDirectDrawSurface2) then Result := 'IID_IDirectDrawSurface2'
  else if IsEqualGuid(GUID,IID_IDirectDrawSurface3) then Result := 'IID_IDirectDrawSurface3'
  else if IsEqualGuid(GUID,IID_IDirectDrawSurfaceKernel) then Result := 'IID_IDirectDrawSurfaceKernel'
  else if IsEqualGuid(GUID,IID_IDirectDrawVideo) then Result := 'IID_IDirectDrawVideo'
  else if IsEqualGuid(GUID,IID_IFullScreenVideo) then Result := 'IID_IFullScreenVideo'
  else if IsEqualGuid(GUID,IID_IFullScreenVideoEx) then Result := 'IID_IFullScreenVideoEx'
  else if IsEqualGuid(GUID,IID_IKsDataTypeHandler) then Result := 'IID_IKsDataTypeHandler'
  else if IsEqualGuid(GUID,IID_IKsInterfaceHandler) then Result := 'IID_IKsInterfaceHandler'
  else if IsEqualGuid(GUID,IID_IKsPin) then Result := 'IID_IKsPin'
  else if IsEqualGuid(GUID,IID_IMixerPinConfig) then Result := 'IID_IMixerPinConfig'
  else if IsEqualGuid(GUID,IID_IMixerPinConfig2) then Result := 'IID_IMixerPinConfig2'
  else if IsEqualGuid(GUID,IID_IMpegAudioDecoder) then Result := 'IID_IMpegAudioDecoder'
  else if IsEqualGuid(GUID,IID_IQualProp) then Result := 'IID_IQualProp'
  else if IsEqualGuid(GUID,IID_IVPConfig) then Result := 'IID_IVPConfig'
  else if IsEqualGuid(GUID,IID_IVPControl) then Result := 'IID_IVPControl'
  else if IsEqualGuid(GUID,IID_IVPNotify) then Result := 'IID_IVPNotify'
  else if IsEqualGuid(GUID,IID_IVPNotify2) then Result := 'IID_IVPNotify2'
  else if IsEqualGuid(GUID,IID_IVPObject) then Result := 'IID_IVPObject'
  else if IsEqualGuid(GUID,IID_IVPVBIConfig) then Result := 'IID_IVPVBIConfig'
  else if IsEqualGuid(GUID,IID_IVPVBINotify) then Result := 'IID_IVPVBINotify'
  else if IsEqualGuid(GUID,IID_IVPVBIObject) then Result := 'IID_IVPVBIObject'

  else if IsEqualGuid(GUID,LOOK_DOWNSTREAM_ONLY) then Result := 'LOOK_DOWNSTREAM_ONLY'
  else if IsEqualGuid(GUID,LOOK_UPSTREAM_ONLY) then Result := 'LOOK_UPSTREAM_ONLY'
  else Result := '';
end;
// milenko end

// milenko start (usefull functions to get linear amplification)
// improved by XXX
function GetBasicAudioVolume(Value : integer) : integer;
begin
 Inc(Value, 10000);
 Result := Round(Exp(Value / 1085.73) - 1);
end;

function SetBasicAudioVolume(Value : integer) : integer;
begin
 Inc(Value);
 Result := Round(1085.73*ln(Value)) - 10000;
end;

function GetBasicAudioPan(Value : integer) : integer;
begin
 Result := Round(Exp(abs(Value) / 1085.73) - 1);
 if Value <= 0 then Result := -Result;
end;

function SetBasicAudioPan(Value : integer) : integer;
begin
 Result := Round(1085.73*ln(abs(Value)+1));
 if Value >= 0 then Result := -Result;
end;
// milenko end

// milenok start (yet another delphi5 compatibility ...)
{$IFDEF VER130}
function StringToGUID(const S: string): TGUID;
begin
  if not Succeeded(CLSIDFromString(PWideChar(WideString(S)), Result))
    then raise Exception.Create('StringToGUID: Error converting String');
end;

function GUIDToString(const GUID: TGUID): string;
var
  P: PWideChar;
begin
  if not Succeeded(StringFromCLSID(GUID, P))
    then raise Exception.Create('GUIDToString: Error converting GUID');
  Result := P;
  CoTaskMemFree(P);
end;

function EnsureRange(const AValue, AMin, AMax: Integer): Integer;
begin
  Result := AValue;
  assert(AMin <= AMax);
  if Result < AMin then
    Result := AMin;
  if Result > AMax then
    Result := AMax;
end;
{$ENDIF}
// milenko end

const
  SectionLengthMask           = $FFF;   // 0000111111111111
  ReservedMask                = $3000;  // 0011000000000000
  PrivateIndicatorMask        = $4000;  // 0100000000000000
  SectionSyntaxIndicatorMask  = $8000;  // 1000000000000000

function MPEGHeaderBitsGetSectionLength(Header: PMPEGHeaderBits): WORD;
begin
  Result := Header.Bits and SectionLengthMask;
end;

function MPEGHeaderBitsGetReserved(Header: PMPEGHeaderBits): WORD;
begin
  Result := (Header.Bits and ReservedMask) shr 12;
end;

function MPEGHeaderBitsGetPrivateIndicator(Header: PMPEGHeaderBits): WORD;
begin
  Result := (Header.Bits and PrivateIndicatorMask) shr 14;
end;

function MPEGHeaderBitsGetSectionSyntaxIndicator(Header: PMPEGHeaderBits): WORD;
begin
  Result := (Header.Bits and SectionSyntaxIndicatorMask) shr 15;
end;

procedure MPEGHeaderBitsSetSectionLength(Header: PMPEGHeaderBits; AValue: WORD);
begin
  Header.Bits := Header.Bits or (AValue and SectionLengthMask);
end;

procedure MPEGHeaderBitsSetReserved(Header: PMPEGHeaderBits; AValue: WORD);
begin
  Header.Bits := Header.Bits or ((AValue shl 12) and ReservedMask);
end;

procedure MPEGHeaderBitsSetPrivateIndicator(Header: PMPEGHeaderBits; AValue: WORD);
begin
  Header.Bits := Header.Bits or ((AValue shl 14) and PrivateIndicatorMask);
end;

procedure MPEGHeaderBitsSetSectionSyntaxIndicator(Header: PMPEGHeaderBits; AValue: WORD);
begin
  Header.Bits := Header.Bits or ((AValue shl 15) and SectionSyntaxIndicatorMask);
end;

const
  PIDBitsReservedMask         = $7;     // 0000000000000111
  PIDBitsProgramId            = $FFF8;  // 1111111111111000

function PIDBitsGetReserved(PIDBits: PPIDBits): WORD;
begin
  Result := PIDBits.Bits and PIDBitsReservedMask;
end;

function PIDBitsGetProgramId(PIDBits: PPIDBits): WORD;
begin
  Result := (PIDBits.Bits and PIDBitsProgramId) shr 3;
end;

procedure PIDBitsSetReserved(PIDBits: PPIDBits; AValue: WORD);
begin
  PIDBits.Bits := PIDBits.Bits or (AValue and PIDBitsReservedMask);
end;

procedure PIDBitsSetProgramId(PIDBits: PPIDBits; AValue: WORD);
begin
  PIDBits.Bits := PIDBits.Bits or ((AValue shl 3) and PIDBitsProgramId);
end;

const
  MHBCurrentNextIndicator     = $1;     // 00000001
  MHBVersionNumber            = $3E;    // 00111110
  MHBReserved                 = $C0;    // 11000000

function MPEGHeaderVersionBitsGetCurrentNextIndicator(MPEGHeaderVersionBits: PMPEGHeaderVersionBits): Byte;
begin
  Result := MPEGHeaderVersionBits.Bits and MHBCurrentNextIndicator;
end;

function MPEGHeaderVersionBitsGetVersionNumber(MPEGHeaderVersionBits: PMPEGHeaderVersionBits): Byte;
begin
  Result := (MPEGHeaderVersionBits.Bits and MHBVersionNumber) shr 1;
end;

function MPEGHeaderVersionBitsGetReserved(MPEGHeaderVersionBits: PMPEGHeaderVersionBits): Byte;
begin
  Result := (MPEGHeaderVersionBits.Bits and MHBReserved) shr 6;
end;

procedure MPEGHeaderVersionBitsSetCurrentNextIndicator(MPEGHeaderVersionBits: PMPEGHeaderVersionBits; AValue: Byte);
begin
  MPEGHeaderVersionBits.Bits := MPEGHeaderVersionBits.Bits or (AValue and MHBCurrentNextIndicator);
end;

procedure MPEGHeaderVersionBitsSetVersionNumber(MPEGHeaderVersionBits: PMPEGHeaderVersionBits; AValue: Byte);
begin
  MPEGHeaderVersionBits.Bits := MPEGHeaderVersionBits.Bits or ((AValue shl 1) and MHBVersionNumber);
end;

procedure MPEGHeaderVersionBitsSetReserved(MPEGHeaderVersionBits: PMPEGHeaderVersionBits; AValue: Byte);
begin
  MPEGHeaderVersionBits.Bits := MPEGHeaderVersionBits.Bits or ((AValue shl 6) and MHBReserved);
end;

end.
