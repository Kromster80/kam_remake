    (*********************************************************************
     *  Windows Media Format 9 API                                       *
     *                                                                   *
     * home page : http://www.progdigy.com                               *
     * email     : hgourvest@progdigy.com                                *
     *                                                                   *
     * date      : 21-02-2003                                            *
     *                                                                   *
     * The contents of this file are used with permission, subject to    *
     * the Mozilla Public License Version 1.1 (the "License"); you may   *
     * not use this file except in compliance with the License. You may  *
     * obtain a copy of the License at                                   *
     * http://www.mozilla.org/MPL/MPL-1.1.html                           *
     *                                                                   *
     * wmsdkvalidate.h, wmdxva.idl, wmnetsourcecreator.idl,              *
     * wmsbuffer.idl, drmexternals.id, asferr.h, wmsysprf.h,             *
     * WMSInternalAdminNetSource.idl, wmsdkidl.idl, dshowasf.idl         *
     *                                                                   *
     * Software distributed under the License is distributed on an       *
     * "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or    *
     * implied. See the License for the specific language governing      *
     * rights and limitations under the License.                         *
     *                                                                   *
     *********************************************************************)

unit WMF9;

interface
{$I DirectX.inc}

{$IFDEF DYNAMIC_LINK_ALL}
  {$DEFINE WMF9_DYNAMIC_LINK}
{$ENDIF}

{$HPPEMIT '#include "asferr.h"'}
{$HPPEMIT '#include "drmexternals.h"'}
{$HPPEMIT '#include "dshowasf.h"'}
{$HPPEMIT '#include "nserror.h"'}
{$HPPEMIT '#include "wmdxva.h"'}
{$HPPEMIT '#include "wmnetsourcecreator.h"'}
{$HPPEMIT '#include "wmsbuffer.h"'}
{$HPPEMIT '#include "wmsdk.h"'}
{$HPPEMIT '#include "wmsdkidl.h"'}
{$HPPEMIT '#include "wmsdkvalidate.h"'}
{$HPPEMIT '#include "wmsinternaladminnetsource.h"'}
{$HPPEMIT '#include "wmsysprf.h"'}

uses Windows, ActiveX, DirectShow9;


//*@@@+++@@@@******************************************************************
//
// Microsoft Windows Media
// Copyright (C) Microsoft Corporation. All rights reserved.
//
//*@@@---@@@@******************************************************************

const
  WMProfile_V40_DialUpMBR              : TGUID = '{fd7f47f1-72a6-45a4-80f0-3aecefc32c07}';
  {$EXTERNALSYM WMProfile_V40_DialUpMBR}
  WMProfile_V40_IntranetMBR            : TGUID = '{82cd3321-a94a-4ffc-9c2b-092c10ca16e7}';
  {$EXTERNALSYM WMProfile_V40_IntranetMBR}
  WMProfile_V40_2856100MBR             : TGUID = '{5a1c2206-dc5e-4186-beb2-4c5a994b132e}';
  {$EXTERNALSYM WMProfile_V40_2856100MBR}
  WMProfile_V40_6VoiceAudio            : TGUID = '{D508978A-11A0-4d15-B0DA-ACDC99D4F890}';
  {$EXTERNALSYM WMProfile_V40_6VoiceAudio}
  WMProfile_V40_16AMRadio              : TGUID = '{0f4be81f-d57d-41e1-b2e3-2fad986bfec2}';
  {$EXTERNALSYM WMProfile_V40_16AMRadio}
  WMProfile_V40_288FMRadioMono         : TGUID = '{7fa57fc8-6ea4-4645-8abf-b6e5a8f814a1}';
  {$EXTERNALSYM WMProfile_V40_288FMRadioMono}
  WMProfile_V40_288FMRadioStereo       : TGUID = '{22fcf466-aa40-431f-a289-06d0ea1a1e40}';
  {$EXTERNALSYM WMProfile_V40_288FMRadioStereo}
  WMProfile_V40_56DialUpStereo         : TGUID = '{e8026f87-e905-4594-a3c7-00d00041d1d9}';
  {$EXTERNALSYM WMProfile_V40_56DialUpStereo}
  WMProfile_V40_64Audio                : TGUID = '{4820b3f7-cbec-41dc-9391-78598714c8e5}';
  {$EXTERNALSYM WMProfile_V40_64Audio}
  WMProfile_V40_96Audio                : TGUID = '{0efa0ee3-9e64-41e2-837f-3c0038f327ba}';
  {$EXTERNALSYM WMProfile_V40_96Audio}
  WMProfile_V40_128Audio               : TGUID = '{93ddbe12-13dc-4e32-a35e-40378e34279a}';
  {$EXTERNALSYM WMProfile_V40_128Audio}
  WMProfile_V40_288VideoVoice          : TGUID = '{bb2bc274-0eb6-4da9-b550-ecf7f2b9948f}';
  {$EXTERNALSYM WMProfile_V40_288VideoVoice}
  WMProfile_V40_288VideoAudio          : TGUID = '{ac617f2d-6cbe-4e84-8e9a-ce151a12a354}';
  {$EXTERNALSYM WMProfile_V40_288VideoAudio}
  WMProfile_V40_288VideoWebServer      : TGUID = '{abf2f00d-d555-4815-94ce-8275f3a70bfe}';
  {$EXTERNALSYM WMProfile_V40_288VideoWebServer}
  WMProfile_V40_56DialUpVideo          : TGUID = '{e21713bb-652f-4dab-99de-71e04400270f}';
  {$EXTERNALSYM WMProfile_V40_56DialUpVideo}
  WMProfile_V40_56DialUpVideoWebServer : TGUID = '{b756ff10-520f-4749-a399-b780e2fc9250}';
  {$EXTERNALSYM WMProfile_V40_56DialUpVideoWebServer}
  WMProfile_V40_100Video               : TGUID = '{8f99ddd8-6684-456b-a0a3-33e1316895f0}';
  {$EXTERNALSYM WMProfile_V40_100Video}
  WMProfile_V40_250Video               : TGUID = '{541841c3-9339-4f7b-9a22-b11540894e42}';
  {$EXTERNALSYM WMProfile_V40_250Video}
  WMProfile_V40_512Video               : TGUID = '{70440e6d-c4ef-4f84-8cd0-d5c28686e784}';
  {$EXTERNALSYM WMProfile_V40_512Video}
  WMProfile_V40_1MBVideo               : TGUID = '{b4482a4c-cc17-4b07-a94e-9818d5e0f13f}';
  {$EXTERNALSYM WMProfile_V40_1MBVideo}
  WMProfile_V40_3MBVideo               : TGUID = '{55374ac0-309b-4396-b88f-e6e292113f28}';
  {$EXTERNALSYM WMProfile_V40_3MBVideo}

  WMProfile_V70_DialUpMBR              : TGUID = '{5B16E74B-4068-45b5-B80E-7BF8C80D2C2F}';
  {$EXTERNALSYM WMProfile_V70_DialUpMBR}
  WMProfile_V70_IntranetMBR            : TGUID = '{045880DC-34B6-4ca9-A326-73557ED143F3}';
  {$EXTERNALSYM WMProfile_V70_IntranetMBR}
  WMProfile_V70_2856100MBR             : TGUID = '{07DF7A25-3FE2-4a5b-8B1E-348B0721CA70}';
  {$EXTERNALSYM WMProfile_V70_2856100MBR}
  WMProfile_V70_288VideoVoice          : TGUID = '{B952F38E-7DBC-4533-A9CA-B00B1C6E9800}';
  {$EXTERNALSYM WMProfile_V70_288VideoVoice}
  WMProfile_V70_288VideoAudio          : TGUID = '{58BBA0EE-896A-4948-9953-85B736F83947}';
  {$EXTERNALSYM WMProfile_V70_288VideoAudio}
  WMProfile_V70_288VideoWebServer      : TGUID = '{70A32E2B-E2DF-4ebd-9105-D9CA194A2D50}';
  {$EXTERNALSYM WMProfile_V70_288VideoWebServer}
  WMProfile_V70_56VideoWebServer       : TGUID = '{DEF99E40-57BC-4ab3-B2D1-B6E3CAF64257}';
  {$EXTERNALSYM WMProfile_V70_56VideoWebServer}
  WMProfile_V70_64VideoISDN            : TGUID = '{C2B7A7E9-7B8E-4992-A1A1-068217A3B311}';
  {$EXTERNALSYM WMProfile_V70_64VideoISDN}
  WMProfile_V70_100Video               : TGUID = '{D9F3C932-5EA9-4c6d-89B4-2686E515426E}';
  {$EXTERNALSYM WMProfile_V70_100Video}
  WMProfile_V70_256Video               : TGUID = '{AFE69B3A-403F-4a1b-8007-0E21CFB3DF84}';
  {$EXTERNALSYM WMProfile_V70_256Video}
  WMProfile_V70_384Video               : TGUID = '{F3D45FBB-8782-44df-97C6-8678E2F9B13D}';
  {$EXTERNALSYM WMProfile_V70_384Video}
  WMProfile_V70_768Video               : TGUID = '{0326EBB6-F76E-4964-B0DB-E729978D35EE}';
  {$EXTERNALSYM WMProfile_V70_768Video}
  WMProfile_V70_1500Video              : TGUID = '{0B89164A-5490-4686-9E37-5A80884E5146}';
  {$EXTERNALSYM WMProfile_V70_1500Video}
  WMProfile_V70_2000Video              : TGUID = '{AA980124-BF10-4e4f-9AFD-4329A7395CFF}';
  {$EXTERNALSYM WMProfile_V70_2000Video}
  WMProfile_V70_700FilmContentVideo    : TGUID = '{7A747920-2449-4d76-99CB-FDB0C90484D4}';
  {$EXTERNALSYM WMProfile_V70_700FilmContentVideo}
  WMProfile_V70_1500FilmContentVideo   : TGUID = '{F6A5F6DF-EE3F-434c-A433-523CE55F516B}';
  {$EXTERNALSYM WMProfile_V70_1500FilmContentVideo}
  WMProfile_V70_6VoiceAudio            : TGUID = '{EABA9FBF-B64F-49b3-AA0C-73FBDD150AD0}';
  {$EXTERNALSYM WMProfile_V70_6VoiceAudio}
  WMProfile_V70_288FMRadioMono         : TGUID = '{C012A833-A03B-44a5-96DC-ED95CC65582D}';
  {$EXTERNALSYM WMProfile_V70_288FMRadioMono}
  WMProfile_V70_288FMRadioStereo       : TGUID = '{E96D67C9-1A39-4dc4-B900-B1184DC83620}';
  {$EXTERNALSYM WMProfile_V70_288FMRadioStereo}
  WMProfile_V70_56DialUpStereo         : TGUID = '{674EE767-0949-4fac-875E-F4C9C292013B}';
  {$EXTERNALSYM WMProfile_V70_56DialUpStereo}
  WMProfile_V70_64AudioISDN            : TGUID = '{91DEA458-9D60-4212-9C59-D40919C939E4}';
  {$EXTERNALSYM WMProfile_V70_64AudioISDN}
  WMProfile_V70_64Audio                : TGUID = '{B29CFFC6-F131-41db-B5E8-99D8B0B945F4}';
  {$EXTERNALSYM WMProfile_V70_64Audio}
  WMProfile_V70_96Audio                : TGUID = '{A9D4B819-16CC-4a59-9F37-693DBB0302D6}';
  {$EXTERNALSYM WMProfile_V70_96Audio}
  WMProfile_V70_128Audio               : TGUID = '{C64CF5DA-DF45-40d3-8027-DE698D68DC66}';
  {$EXTERNALSYM WMProfile_V70_128Audio}
  WMProfile_V70_225VideoPDA            : TGUID = '{F55EA573-4C02-42b5-9026-A8260C438A9F}';
  {$EXTERNALSYM WMProfile_V70_225VideoPDA}
  WMProfile_V70_150VideoPDA            : TGUID = '{0F472967-E3C6-4797-9694-F0304C5E2F17}';
  {$EXTERNALSYM WMProfile_V70_150VideoPDA}

  WMProfile_V80_255VideoPDA            : TGUID = '{FEEDBCDF-3FAC-4c93-AC0D-47941EC72C0B}';
  {$EXTERNALSYM WMProfile_V80_255VideoPDA}
  WMProfile_V80_150VideoPDA            : TGUID = '{AEE16DFA-2C14-4a2f-AD3F-A3034031784F}';
  {$EXTERNALSYM WMProfile_V80_150VideoPDA}
  WMProfile_V80_28856VideoMBR          : TGUID = '{D66920C4-C21F-4ec8-A0B4-95CF2BD57FC4}';
  {$EXTERNALSYM WMProfile_V80_28856VideoMBR}
  WMProfile_V80_100768VideoMBR         : TGUID = '{5BDB5A0E-979E-47d3-9596-73B386392A55}';
  {$EXTERNALSYM WMProfile_V80_100768VideoMBR}
  WMProfile_V80_288100VideoMBR         : TGUID = '{D8722C69-2419-4b36-B4E0-6E17B60564E5}';
  {$EXTERNALSYM WMProfile_V80_288100VideoMBR}
  WMProfile_V80_288Video               : TGUID = '{3DF678D9-1352-4186-BBF8-74F0C19B6AE2}';
  {$EXTERNALSYM WMProfile_V80_288Video}
  WMProfile_V80_56Video                : TGUID = '{254E8A96-2612-405c-8039-F0BF725CED7D}';
  {$EXTERNALSYM WMProfile_V80_56Video}
  WMProfile_V80_100Video               : TGUID = '{A2E300B4-C2D4-4fc0-B5DD-ECBD948DC0DF}';
  {$EXTERNALSYM WMProfile_V80_100Video}
  WMProfile_V80_256Video               : TGUID = '{BBC75500-33D2-4466-B86B-122B201CC9AE}';
  {$EXTERNALSYM WMProfile_V80_256Video}
  WMProfile_V80_384Video               : TGUID = '{29B00C2B-09A9-48bd-AD09-CDAE117D1DA7}';
  {$EXTERNALSYM WMProfile_V80_384Video}
  WMProfile_V80_768Video               : TGUID = '{74D01102-E71A-4820-8F0D-13D2EC1E4872}';
  {$EXTERNALSYM WMProfile_V80_768Video}
  WMProfile_V80_700NTSCVideo           : TGUID = '{C8C2985F-E5D9-4538-9E23-9B21BF78F745}';
  {$EXTERNALSYM WMProfile_V80_700NTSCVideo}
  WMProfile_V80_1400NTSCVideo          : TGUID = '{931D1BEE-617A-4bcd-9905-CCD0786683EE}';
  {$EXTERNALSYM WMProfile_V80_1400NTSCVideo}
  WMProfile_V80_384PALVideo            : TGUID = '{9227C692-AE62-4f72-A7EA-736062D0E21E}';
  {$EXTERNALSYM WMProfile_V80_384PALVideo}
  WMProfile_V80_700PALVideo            : TGUID = '{EC298949-639B-45e2-96FD-4AB32D5919C2}';
  {$EXTERNALSYM WMProfile_V80_700PALVideo}
  WMProfile_V80_288MonoAudio           : TGUID = '{7EA3126D-E1BA-4716-89AF-F65CEE0C0C67}';
  {$EXTERNALSYM WMProfile_V80_288MonoAudio}
  WMProfile_V80_288StereoAudio         : TGUID = '{7E4CAB5C-35DC-45bb-A7C0-19B28070D0CC}';
  {$EXTERNALSYM WMProfile_V80_288StereoAudio}
  WMProfile_V80_32StereoAudio          : TGUID = '{60907F9F-B352-47e5-B210-0EF1F47E9F9D}';
  {$EXTERNALSYM WMProfile_V80_32StereoAudio}
  WMProfile_V80_48StereoAudio          : TGUID = '{5EE06BE5-492B-480a-8A8F-12F373ECF9D4}';
  {$EXTERNALSYM WMProfile_V80_48StereoAudio}
  WMProfile_V80_64StereoAudio          : TGUID = '{09BB5BC4-3176-457f-8DD6-3CD919123E2D}';
  {$EXTERNALSYM WMProfile_V80_64StereoAudio}
  WMProfile_V80_96StereoAudio          : TGUID = '{1FC81930-61F2-436f-9D33-349F2A1C0F10}';
  {$EXTERNALSYM WMProfile_V80_96StereoAudio}
  WMProfile_V80_128StereoAudio         : TGUID = '{407B9450-8BDC-4ee5-88B8-6F527BD941F2}';
  {$EXTERNALSYM WMProfile_V80_128StereoAudio}
  WMProfile_V80_288VideoOnly           : TGUID = '{8C45B4C7-4AEB-4f78-A5EC-88420B9DADEF}';
  {$EXTERNALSYM WMProfile_V80_288VideoOnly}
  WMProfile_V80_56VideoOnly            : TGUID = '{6E2A6955-81DF-4943-BA50-68A986A708F6}';
  {$EXTERNALSYM WMProfile_V80_56VideoOnly}
  WMProfile_V80_FAIRVBRVideo           : TGUID = '{3510A862-5850-4886-835F-D78EC6A64042}';
  {$EXTERNALSYM WMProfile_V80_FAIRVBRVideo}
  WMProfile_V80_HIGHVBRVideo           : TGUID = '{0F10D9D3-3B04-4fb0-A3D3-88D4AC854ACC}';
  {$EXTERNALSYM WMProfile_V80_HIGHVBRVideo}
  WMProfile_V80_BESTVBRVideo           : TGUID = '{048439BA-309C-440e-9CB4-3DCCA3756423}';
  {$EXTERNALSYM WMProfile_V80_BESTVBRVideo}

//*@@@+++@@@@******************************************************************
//
// Microsoft Windows Media
// Copyright (C) Microsoft Corporation. All rights reserved.
//
//*@@@---@@@@******************************************************************
//

//import "mediaobj.idl";
//import "videoacc.idl"; // IAMVideoAccelerator
const
  IID_IWMPlayerTimestampHook     : TGUID = '{28580dda-d98e-48d0-b7ae-69e473a02825}';
  {$EXTERNALSYM IID_IWMPlayerTimestampHook}
  IID_IWMCodecVideoAccelerator   : TGUID = '{990641b0-739f-4e94-a808-9888da8f75af}';
  {$EXTERNALSYM IID_IWMCodecVideoAccelerator}
  IID_IWMCodecAMVideoAccelerator : TGUID = '{d98ee251-34e0-4a2d-9312-9b4c788d9fa1}';
  {$EXTERNALSYM IID_IWMCodecAMVideoAccelerator}

type
// Implemented by the player            
  {$HPPEMIT 'typedef System::DelphiInterface<IWMPlayerTimestampHook> _di_IWMPlayerTimestampHook;'}
  {$EXTERNALSYM IWMPlayerTimestampHook}
  IWMPlayerTimestampHook = interface(IUnknown)
  ['{28580dda-d98e-48d0-b7ae-69e473a02825}']
  (*** IWMPlayerTimestampHook methods ***)
    function MapTimestamp(rtIn: TReferenceTime;
      out prtOut: TReferenceTime): HRESULT; stdcall;
  end;

// Implemeted by video decoder DMOs for DXVA support
  {$HPPEMIT 'typedef System::DelphiInterface<IWMCodecAMVideoAccelerator> _di_IWMCodecAMVideoAccelerator;'}
  {$EXTERNALSYM IWMCodecAMVideoAccelerator}
  IWMCodecAMVideoAccelerator = interface(IUnknown)
  ['{d98ee251-34e0-4a2d-9312-9b4c788d9fa1}']
  (*** IWMCodecAMVideoAccelerator methods ***)
    function SetAcceleratorInterface(pIAMVA: IAMVideoAccelerator): HRESULT; stdcall;
    function NegotiateConnection(pMediaType: PDMOMediaType ): HRESULT; stdcall;
    function SetPlayerNotify(pHook: IWMPlayerTimestampHook): HRESULT; stdcall;
  end;

// Outdated version of IWMCodecAMVideoAccelerator
  {$HPPEMIT 'typedef System::DelphiInterface<IWMCodecVideoAccelerator> _di_IWMCodecVideoAccelerator;'}
  {$EXTERNALSYM IWMCodecVideoAccelerator}
  IWMCodecVideoAccelerator = interface(IUnknown)
  ['{990641b0-739f-4e94-a808-9888da8f75af}']
  (*** IWMCodecVideoAccelerator methods ***)
    function NegotiateConnection(pIAMVA: IAMVideoAccelerator;
      pMediaType: PDMOMediaType): HRESULT;
    function SetPlayerNotify(pHook: IWMPlayerTimestampHook): HRESULT;
  end;

//*****************************************************************************
//
// Microsoft Windows Media
// Copyright (C) Microsoft Corporation. All rights reserved.
//
// FileName:            WMNetSourceCreator.idl
//
// Abstract:
//
//*****************************************************************************

const
  CLSID_ClientNetManager  : TGUID = '{CD12A3CE-9C42-11D2-BEED-0060082F2054}';
  {$EXTERNALSYM CLSID_ClientNetManager}
  IID_INSNetSourceCreator : TGUID = '{0C0E4080-9081-11d2-BEEC-0060082F2054}';
  {$EXTERNALSYM IID_INSNetSourceCreator}

//////////////////////////////////////////////////////////////////////////////
//
// Interface for creating a NetSource plugin.
//
type
  {$HPPEMIT 'typedef System::DelphiInterface<INSNetSourceCreator> _di_INSNetSourceCreator;'}
  {$EXTERNALSYM INSNetSourceCreator}
  INSNetSourceCreator = interface(IUnknown)
  ['{0C0E4080-9081-11d2-BEEC-0060082F2054}']
  (*** INSNetSourceCreator methods ***)
    function Initialize: HRESULT; stdcall;

    //
    // This method is called to create an instance of the a particular
    // Network Source plugin
    //
    function CreateNetSource(pszStreamName: PWideChar; pMonitor: IUnknown;
      pData: PBYTE; pUserContext, pCallback: IUnknown; qwContext: Int64): HRESULT; stdcall;

    //
    // This method returns the namespace node for the properties of the
    // the Network Source plugin that would be used given the
    // provided stream name.
    //
    function GetNetSourceProperties(pszStreamName: PWideChar;
      out ppPropertiesNode: IUnknown): HRESULT; stdcall;

    //
    // Returns part of the Namespace that is used for values that are
    // shared among all Network Source plugins
    //
    function GetNetSourceSharedNamespace(out ppSharedNamespace: IUnknown): HRESULT; stdcall;

    function GetNetSourceAdminInterface(pszStreamName: PWideChar;
      out pVal: OLEVARIANT): HRESULT; stdcall;

    //
    // Iterate through the network protocols supported
    //
    function GetNumProtocolsSupported(out pcProtocols: LongWord): HRESULT; stdcall;

    function GetProtocolName(dwProtocolNum: LongWord; {out} pwszProtocolName: PWideChar;
      var pcchProtocolName: Word): HRESULT; stdcall;

    function Shutdown: HRESULT; stdcall;
  end;

//*@@@+++@@@@******************************************************************
//
// Microsoft Windows Media
// Copyright (C) Microsoft Corporation. All rights reserved.
//
//*@@@---@@@@******************************************************************
const
  IID_INSSBuffer           : TGUID = '{E1CD3524-03D7-11d2-9EED-006097D2D7CF}';
  {$EXTERNALSYM IID_INSSBuffer}
  IID_IWMSBuffer           : TGUID = '{E1CD3524-03D7-11d2-9EED-006097D2D7CF}';
  {$EXTERNALSYM IID_IWMSBuffer}
  IID_INSSBuffer2          : TGUID = '{4f528693-1035-43fe-b428-757561ad3a68}';
  {$EXTERNALSYM IID_INSSBuffer2}
  IID_INSSBuffer3          : TGUID = '{c87ceaaf-75be-4bc4-84eb-ac2798507672}';
  {$EXTERNALSYM IID_INSSBuffer3}
  IID_INSSBuffer4          : TGUID = '{b6b8fd5a-32e2-49d4-a910-c26cc85465ed}';
  {$EXTERNALSYM IID_INSSBuffer4}
  IID_IWMSBufferAllocator  : TGUID = '{61103CA4-2033-11d2-9EF1-006097D2D7CF}';
  {$EXTERNALSYM IID_IWMSBufferAllocator}

type
  {$HPPEMIT 'typedef System::DelphiInterface<INSSBuffer> _di_INSSBuffer;'}
  {$EXTERNALSYM INSSBuffer}
  INSSBuffer = interface(IUnknown)
  ['{E1CD3524-03D7-11d2-9EED-006097D2D7CF}']
  (*** INSSBuffer methods ***)
    function GetLength(out pdwLength: LongWord): HRESULT; stdcall;
    function SetLength(dwLength: LongWord): HRESULT; stdcall;
    function GetMaxLength(out pdwLength: LongWord): HRESULT; stdcall;
    function GetBuffer(out ppdwBuffer: PBYTE): HRESULT; stdcall;
    function GetBufferAndLength(out ppdwBuffer: PBYTE; out pdwLength: LongWord): HRESULT; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<INSSBuffer2> _di_INSSBuffer2;'}
  {$EXTERNALSYM INSSBuffer2}
  INSSBuffer2 = interface(INSSBuffer)
  ['{4F528693-1035-43fe-B428-757561AD3A68}']
  (*** INSSBuffer2 methods ***)
    function GetSampleProperties(cbProperties: LongWord; {out} pbProperties: PBYTE): HRESULT; stdcall;
    function SetSampleProperties(cbProperties: LongWord; pbProperties: PBYTE): HRESULT; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<INSSBuffer3> _di_INSSBuffer3;'}
  {$EXTERNALSYM INSSBuffer3}
  INSSBuffer3 = interface(INSSBuffer2)
  ['{C87CEAAF-75BE-4bc4-84EB-AC2798507672}']
  (*** INSSBuffer3 methods ***)
    function SetProperty(guidBufferProperty: TGUID; pvBufferProperty: Pointer;
      dwBufferPropertySize: LongWord): HRESULT; stdcall;
    function GetProperty(guidBufferProperty: TGUID; {out} pvBufferProperty: Pointer;
      var pdwBufferPropertySize: LongWord): HRESULT; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<INSSBuffer4> _di_INSSBuffer4;'}
  {$EXTERNALSYM INSSBuffer4}
  INSSBuffer4 = interface(INSSBuffer3)
  ['{B6B8FD5A-32E2-49d4-A910-C26CC85465ED}']
  (*** INSSBuffer4 methods ***)
    function GetPropertyCount(out pcBufferProperties: LongWord): HRESULT; stdcall;
    function GetPropertyByIndex(dwBufferPropertyIndex: LongWord;
      out pguidBufferProperty: TGUID; pvBufferProperty: pointer;
      var pdwBufferPropertySize: LongWord): HRESULT; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMSBufferAllocator> _di_IWMSBufferAllocator;'}
  {$EXTERNALSYM IWMSBufferAllocator}
  IWMSBufferAllocator = interface(IUnknown)
  ['{61103CA4-2033-11d2-9EF1-006097D2D7CF}']
  (*** IWMSBufferAllocator methods ***)
    function AllocateBuffer(dwMaxBufferSize: LongWord; out ppBuffer: INSSBuffer): HRESULT; stdcall;
    function AllocatePageSizeBuffer(dwMaxBufferSize: LongWord; out ppBuffer: INSSBuffer): HRESULT; stdcall;
  end;

//*@@@+++@@@@******************************************************************
//
// Microsoft Windows Media
// Copyright (C) Microsoft Corporation. All rights reserved.
//
//*@@@---@@@@******************************************************************
//

// The following is a partial list of DRM actions that are of interest to applications
const
  g_wszWMDRM_RIGHT_PLAYBACK                    = WideString('Play');
  {$EXTERNALSYM g_wszWMDRM_RIGHT_PLAYBACK}
  g_wszWMDRM_RIGHT_COPY_TO_CD                  = WideString('Print.redbook');
  {$EXTERNALSYM g_wszWMDRM_RIGHT_COPY_TO_CD}
  g_wszWMDRM_RIGHT_COPY_TO_SDMI_DEVICE         = WideString('Transfer.SDMI');
  {$EXTERNALSYM g_wszWMDRM_RIGHT_COPY_TO_SDMI_DEVICE}
  g_wszWMDRM_RIGHT_COPY_TO_NON_SDMI_DEVICE     = WideString('Transfer.NONSDMI');
  {$EXTERNALSYM g_wszWMDRM_RIGHT_COPY_TO_NON_SDMI_DEVICE}
  g_wszWMDRM_RIGHT_BACKUP                      = WideString('Backup');
  {$EXTERNALSYM g_wszWMDRM_RIGHT_BACKUP}

  g_wszWMDRM_IsDRM                             = WideString('IsDRM');
  {$EXTERNALSYM g_wszWMDRM_IsDRM}
  g_wszWMDRM_IsDRMCached                       = WideString('IsDRMCached');
  {$EXTERNALSYM g_wszWMDRM_IsDRMCached}
  g_wszWMDRM_BaseLicenseAcqURL                 = WideString('BaseLAURL');
  {$EXTERNALSYM g_wszWMDRM_BaseLicenseAcqURL}
  g_wszWMDRM_Rights                            = WideString('Rights');
  {$EXTERNALSYM g_wszWMDRM_Rights}
  g_wszWMDRM_LicenseID                         = WideString('LID');
  {$EXTERNALSYM g_wszWMDRM_LicenseID}

  g_wszWMDRM_ActionAllowed                     = WideString('ActionAllowed.');
  {$EXTERNALSYM g_wszWMDRM_ActionAllowed}
  g_wszWMDRM_ActionAllowed_Playback            = WideString('ActionAllowed.Play');
  {$EXTERNALSYM g_wszWMDRM_ActionAllowed_Playback}
  g_wszWMDRM_ActionAllowed_CopyToCD            = WideString('ActionAllowed.Print.redbook');
  {$EXTERNALSYM g_wszWMDRM_ActionAllowed_CopyToCD}
  g_wszWMDRM_ActionAllowed_CopyToSDMIDevice    = WideString('ActionAllowed.Transfer.SDMI');
  {$EXTERNALSYM g_wszWMDRM_ActionAllowed_CopyToSDMIDevice}
  g_wszWMDRM_ActionAllowed_CopyToNonSDMIDevice = WideString('ActionAllowed.Transfer.NONSDMI');
  {$EXTERNALSYM g_wszWMDRM_ActionAllowed_CopyToNonSDMIDevice}
  g_wszWMDRM_ActionAllowed_Backup              = WideString('ActionAllowed.Backup');
  {$EXTERNALSYM g_wszWMDRM_ActionAllowed_Backup}

  g_wszWMDRM_LicenseState                      = WideString('LicenseStateData.');
  {$EXTERNALSYM g_wszWMDRM_LicenseState}
  g_wszWMDRM_LicenseState_Playback             = WideString('LicenseStateData.Play');
  {$EXTERNALSYM g_wszWMDRM_LicenseState_Playback}
  g_wszWMDRM_LicenseState_CopyToCD             = WideString('LicenseStateData.Print.redbook');
  {$EXTERNALSYM g_wszWMDRM_LicenseState_CopyToCD}
  g_wszWMDRM_LicenseState_CopyToSDMIDevice     = WideString('LicenseStateData.Transfer.SDMI');
  {$EXTERNALSYM g_wszWMDRM_LicenseState_CopyToSDMIDevice}
  g_wszWMDRM_LicenseState_CopyToNonSDMIDevice  = WideString('LicenseStateData.Transfer.NONSDMI');
  {$EXTERNALSYM g_wszWMDRM_LicenseState_CopyToNonSDMIDevice}

  g_wszWMDRM_DRMHeader                         = WideString('DRMHeader.');
  {$EXTERNALSYM g_wszWMDRM_DRMHeader}
  g_wszWMDRM_DRMHeader_KeyID                   = WideString('DRMHeader.KID');
  {$EXTERNALSYM g_wszWMDRM_DRMHeader_KeyID}
  g_wszWMDRM_DRMHeader_LicenseAcqURL           = WideString('DRMHeader.LAINFO');
  {$EXTERNALSYM g_wszWMDRM_DRMHeader_LicenseAcqURL}
  g_wszWMDRM_DRMHeader_ContentID               = WideString('DRMHeader.CID');
  {$EXTERNALSYM g_wszWMDRM_DRMHeader_ContentID}
  g_wszWMDRM_DRMHeader_IndividualizedVersion   = WideString('DRMHeader.SECURITYVERSION');
  {$EXTERNALSYM g_wszWMDRM_DRMHeader_IndividualizedVersion}
  g_wszWMDRM_DRMHeader_ContentDistributor      = WideString('DRMHeader.ContentDistributor');
  {$EXTERNALSYM g_wszWMDRM_DRMHeader_ContentDistributor}
  g_wszWMDRM_DRMHeader_SubscriptionContentID   = WideString('DRMHeader.SubscriptionContentID');
  {$EXTERNALSYM g_wszWMDRM_DRMHeader_SubscriptionContentID}


// This enum indicates the category for each possible output string to be displayed.
// 0 -  Playback not permitted.
// 1 -  Playback unlimited.
// 2 -  Playback valid 5 times.
// 3 -  Playback valid from 7/12/00.
// 4 -  Playback valid until 7/12/00.
// 5 -  Playback valid from 5/12 to 9/12.
// 6 -  Playback valid 5 times from 7/12/00.
// 7 -  Playback valid 5 times until 7/12/00.
// 8 -  Playback valid 5 times from 5/12 to 9/12.
// 9 -  Playback valid for 24 hours from first use.
type
  DRM_LICENSE_STATE_CATEGORY = (
    WM_DRM_LICENSE_STATE_NORIGHT,
    WM_DRM_LICENSE_STATE_UNLIM,
    WM_DRM_LICENSE_STATE_COUNT,
    WM_DRM_LICENSE_STATE_FROM,
    WM_DRM_LICENSE_STATE_UNTIL,
    WM_DRM_LICENSE_STATE_FROM_UNTIL,
    WM_DRM_LICENSE_STATE_COUNT_FROM,
    WM_DRM_LICENSE_STATE_COUNT_UNTIL,
    WM_DRM_LICENSE_STATE_COUNT_FROM_UNTIL,
    WM_DRM_LICENSE_STATE_EXPIRATION_AFTER_FIRSTUSE
  );
  {$EXTERNALSYM DRM_LICENSE_STATE_CATEGORY}
  TDRMLicenseStateCategory = DRM_LICENSE_STATE_CATEGORY;

  PDRMLicenseStateData = ^TDRMLicenseStateData;
  _DRM_LICENSE_STATE_DATA = packed record
    dwStreamId  : LongWord;                   // 0 -> All streams, != 0 -> A particular stream.
    dwCategory  : TDRMLicenseStateCategory;   // Indicates the category of string to be displayed.
    dwNumCounts : LongWord;                   // Number of items supplied in dwCount.
    dwCount     : array[0..3] of LongWord;    // Up to 4 counts.
    dwNumDates  : LongWord;                   // Number of items supplied in dwDate.
    datetime    : array [0..3] of FILETIME;   // Up to 4 dates.
    dwVague     : LongWord;                   // 0 -> certain, 1 -> atleast.  (There could be more
                                              //               licenses. Aggregation not possible.)
  end;
  {$EXTERNALSYM _DRM_LICENSE_STATE_DATA}
  DRM_LICENSE_STATE_DATA = _DRM_LICENSE_STATE_DATA;
  {$EXTERNALSYM DRM_LICENSE_STATE_DATA}
  TDRMLicenseStateData = _DRM_LICENSE_STATE_DATA;

  DRM_HTTP_STATUS = (
    HTTP_NOTINITIATED,
    HTTP_CONNECTING,
    HTTP_REQUESTING,
    HTTP_RECEIVING,
    HTTP_COMPLETED
  );
  {$EXTERNALSYM DRM_HTTP_STATUS}
  TDRMHTTPStatus = DRM_HTTP_STATUS;

  //  The various states individualization can be in:
{$IFNDEF COMPILER6_UP}
  DRM_INDIVIDUALIZATION_STATUS = {$IFDEF TYPE_IDENTITY}type {$ENDIF} LongWord;
  const
    INDI_UNDEFINED = $00;
    {$EXTERNALSYM INDI_UNDEFINED}
    INDI_BEGIN     = $01;
    {$EXTERNALSYM INDI_BEGIN}
    INDI_SUCCEED   = $02;
    {$EXTERNALSYM INDI_SUCCEED}
    INDI_FAIL      = $04;
    {$EXTERNALSYM INDI_FAIL}
    INDI_CANCEL    = $08;
    {$EXTERNALSYM INDI_CANCEL}
    INDI_DOWNLOAD  = $10;
    {$EXTERNALSYM INDI_DOWNLOAD}
    INDI_INSTALL   = $20;
    {$EXTERNALSYM INDI_INSTALL}
{$ELSE}
type
  DRM_INDIVIDUALIZATION_STATUS = (
    INDI_UNDEFINED = $00,
    INDI_BEGIN     = $01,
    INDI_SUCCEED   = $02,
    INDI_FAIL      = $04,
    INDI_CANCEL    = $08,
    INDI_DOWNLOAD  = $10,
    INDI_INSTALL   = $20
  );
{$ENDIF}
type
  {$EXTERNALSYM DRM_INDIVIDUALIZATION_STATUS}
  TDRMIndividualizationStatus = DRM_INDIVIDUALIZATION_STATUS;

type
  PWMIndividualizeStatus = ^TWMIndividualizeStatus;
  _WMIndividualizeStatus = packed record
    hr                 : HRESULT;
    enIndiStatus       : TDRMIndividualizationStatus;
    pszIndiRespUrl     : PChar;
    dwHTTPRequest      : LongWord;
    enHTTPStatus       : TDRMHTTPStatus;
    dwHTTPReadProgress : LongWord;
    dwHTTPReadTotal    : LongWord;
  end;
  {$EXTERNALSYM _WMIndividualizeStatus}
  WM_INDIVIDUALIZE_STATUS = _WMIndividualizeStatus;
  {$EXTERNALSYM WM_INDIVIDUALIZE_STATUS}
  TWMIndividualizeStatus = _WMIndividualizeStatus;

  PWMGetLicenseData = ^TWMGetLicenseData;
  _WMGetLicenseData = packed record
    dwSize           : LongWord;
    hr               : HRESULT;
    wszURL           : PWideChar;
    wszLocalFilename : PWideChar;
    pbPostData       : PBYTE;
    dwPostDataSize   : LongWord;
  end;
  {$EXTERNALSYM _WMGetLicenseData}
  WM_GET_LICENSE_DATA = _WMGetLicenseData;
  {$EXTERNALSYM WM_GET_LICENSE_DATA}
  TWMGetLicenseData = _WMGetLicenseData;


//*****************************************************************************
//
// Microsoft Windows Media
// Copyright (C) Microsoft Corporation. All rights reserved.
//
// FileName:            WMSInternalAdminNetSource.idl
//
// Abstract:
//
//*****************************************************************************

const
  IID_IWMSInternalAdminNetSource  : TGUID = '{8BB23E5F-D127-4afb-8D02-AE5B66D54C78}';
  {$EXTERNALSYM IID_IWMSInternalAdminNetSource}
  IID_IWMSInternalAdminNetSource2 : TGUID = '{E74D58C3-CF77-4b51-AF17-744687C43EAE}';
  {$EXTERNALSYM IID_IWMSInternalAdminNetSource2}
  IID_IWMSInternalAdminNetSource3 : TGUID = '{6b63d08e-4590-44af-9eb3-57ff1e73bf80}';
  {$EXTERNALSYM IID_IWMSInternalAdminNetSource3}

type
  NETSOURCE_URLCREDPOLICY_SETTINGS = (
    NETSOURCE_URLCREDPOLICY_SETTING_SILENTLOGONOK,
    NETSOURCE_URLCREDPOLICY_SETTING_MUSTPROMPTUSER,
    NETSOURCE_URLCREDPOLICY_SETTING_ANONYMOUSONLY
  );
  {$EXTERNALSYM NETSOURCE_URLCREDPOLICY_SETTINGS}
 TNetSourceURLCredpolicySettings = NETSOURCE_URLCREDPOLICY_SETTINGS;

////////////////////////////////////////////////////////////////////////////////////////////////////
//
// This interface is used by WMP 7 and MPXP
//

  {$HPPEMIT 'typedef System::DelphiInterface<IWMSInternalAdminNetSource> _di_IWMSInternalAdminNetSource;'}
  {$EXTERNALSYM IWMSInternalAdminNetSource}
  IWMSInternalAdminNetSource = interface(IUnknown)
  ['{8BB23E5F-D127-4afb-8D02-AE5B66D54C78}']
  (*** IWMSInternalAdminNetSource methods ***)
    function Initialize(pSharedNamespace,
                       pNamespaceNode: IUnknown;
                       pNetSourceCreator: INSNetSourceCreator;
                       fEmbeddedInServer: BOOL): HRESULT; stdcall;

    function GetNetSourceCreator(out ppNetSourceCreator: INSNetSourceCreator): HRESULT; stdcall;

    // Authentication
    function SetCredentials(bstrRealm, bstrName, bstrPassword: WideString; fPersist, fConfirmedGood: BOOL): HRESULT; stdcall;
    function GetCredentials(bstrRealm: WideString; out pbstrName, pbstrPassword: WideString; out pfConfirmedGood: BOOL): HRESULT; stdcall;
    function DeleteCredentials(bstrRealm: WideString): HRESULT; stdcall;
    function GetCredentialFlags(out lpdwFlags: LongWord): HRESULT; stdcall;
    function SetCredentialFlags(dwFlags: LongWord): HRESULT; stdcall;

    // Proxy
    function FindProxyForURL(bstrProtocol, bstrHost: WideString;
                             out pfProxyEnabled: BOOL;
                             out pbstrProxyServer: WideString;
                             out pdwProxyPort: LongWord;
                             var pdwProxyContext: LongWord): HRESULT; stdcall;

    function RegisterProxyFailure(hrParam: HRESULT; dwProxyContext: LongWord): HRESULT; stdcall;
    function ShutdownProxyContext(dwProxyContext: LongWord): HRESULT; stdcall;
    function IsUsingIE(dwProxyContext: LongWord; out pfIsUsingIE: BOOL): HRESULT; stdcall;
  end;


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// This interface is used by WMP 7 and MPXP
//

  {$HPPEMIT 'typedef System::DelphiInterface<IWMSInternalAdminNetSource2> _di_IWMSInternalAdminNetSource2;'}
  {$EXTERNALSYM IWMSInternalAdminNetSource2}
  IWMSInternalAdminNetSource2 = interface(IUnknown)
  ['{E74D58C3-CF77-4b51-AF17-744687C43EAE}']
  (*** IWMSInternalAdminNetSource2 methods ***)
    // Authentication
    function SetCredentialsEx(bstrRealm      : WideString;
                              bstrUrl        : WideString;
                              fProxy         : BOOL;
                              bstrName       : WideString;
                              bstrPassword   : WideString;
                              fPersist       : BOOL;
                              fConfirmedGood : BOOL): HRESULT; stdcall;

    function GetCredentialsEx(bstrRealm           : WideString;
                              bstrUrl             : WideString;
                              fProxy              : BOOL;
                              out pdwUrlPolicy    : TNetSourceURLCredpolicySettings;
                              out pbstrName       : WideString;
                              out pbstrPassword   : WideString;
                              out pfConfirmedGood : BOOL): HRESULT; stdcall;

    function DeleteCredentialsEx(bstrRealm, bstrUrl: WideString; fProxy: BOOL): HRESULT; stdcall;

    function FindProxyForURLEx(bstrProtocol         : WideString;
                               bstrHost             : WideString;
                               bstrUrl              : WideString;
                               out pfProxyEnabled   : BOOL;
                               out pbstrProxyServer : WideString;
                               out pdwProxyPort     : LongWord;
                               var pdwProxyContext  : LongWord): HRESULT; stdcall;

  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMSInternalAdminNetSource3> _di_IWMSInternalAdminNetSource3;'}
  {$EXTERNALSYM IWMSInternalAdminNetSource3}
  IWMSInternalAdminNetSource3 = interface(IWMSInternalAdminNetSource2)
  ['{6b63d08e-4590-44af-9eb3-57ff1e73bf80}']
  (*** IWMSInternalAdminNetSource3 methods ***)
    function GetNetSourceCreator2(out ppNetSourceCreator: IUnknown): HRESULT; stdcall;

    // Proxy
    function FindProxyForURLEx2(bstrProtocol         : WideString;
                                bstrHost             : WideString;
                                bstrUrl              : WideString;
                                out pfProxyEnabled   : BOOL;
                                out pbstrProxyServer : WideString;
                                out pdwProxyPort     : LongWord;
                                var pqwProxyContext  : Int64): HRESULT; stdcall;

    function RegisterProxyFailure2(hrParam: HRESULT; qwProxyContext: Int64): HRESULT; stdcall;
    function ShutdownProxyContext2(qwProxyContext: Int64): HRESULT; stdcall;
    function IsUsingIE2(qwProxyContext: Int64; out pfIsUsingIE: BOOL): HRESULT; stdcall;

    // Authentication
    function SetCredentialsEx2(bstrRealm                : WideString;
                               bstrUrl                  : WideString;
                               fProxy                   : BOOL;
                               bstrName                 : WideString;
                               bstrPassword             : WideString;
                               fPersist                 : BOOL;
                               fConfirmedGood           : BOOL;
                               fClearTextAuthentication : BOOL): HRESULT; stdcall;

    function GetCredentialsEx2(bstrRealm                : WideString;
                               bstrUrl                  : WideString;
                               fProxy                   : BOOL;
                               fClearTextAuthentication : BOOL;
                               out pdwUrlPolicy         : TNetSourceURLCredpolicySettings;
                               out pbstrName            : WideString;
                               out pbstrPassword        : WideString;
                               out pfConfirmedGood      : BOOL): HRESULT; stdcall;
  end;

//*@@@+++@@@@******************************************************************
//
// Microsoft Windows Media
// Copyright (C) Microsoft Corporation. All rights reserved.
//
//*@@@---@@@@******************************************************************
//

///////////////////////////////////////////////////////////////////////////////
//
// Enumerations and constants used by the SDK.
//
///////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////
//
// Imports, typedefs and forward declarations
//

///////////////////////////////////////////////////////////////////////////////
//
// Attributes
//
///////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////
//
// These are the special case attributes that give information
// about the Windows Media file.
//
const
  g_dwWMSpecialAttributes       = LongWord(20);
  {$EXTERNALSYM g_dwWMSpecialAttributes}
  g_wszWMDuration               = WideString('Duration');
  {$EXTERNALSYM g_wszWMDuration}
  g_wszWMBitrate                = WideString('Bitrate');
  {$EXTERNALSYM g_wszWMBitrate}
  g_wszWMSeekable               = WideString('Seekable');
  {$EXTERNALSYM g_wszWMSeekable}
  g_wszWMStridable              = WideString('Stridable');
  {$EXTERNALSYM g_wszWMStridable}
  g_wszWMBroadcast              = WideString('Broadcast');
  {$EXTERNALSYM g_wszWMBroadcast}
  g_wszWMProtected              = WideString('Is_Protected');
  {$EXTERNALSYM g_wszWMProtected}
  g_wszWMTrusted                = WideString('Is_Trusted');
  {$EXTERNALSYM g_wszWMTrusted}
  g_wszWMSignature_Name         = WideString('Signature_Name');
  {$EXTERNALSYM g_wszWMSignature_Name}
  g_wszWMHasAudio               = WideString('HasAudio');
  {$EXTERNALSYM g_wszWMHasAudio}
  g_wszWMHasImage               = WideString('HasImage');
  {$EXTERNALSYM g_wszWMHasImage}
  g_wszWMHasScript              = WideString('HasScript');
  {$EXTERNALSYM g_wszWMHasScript}
  g_wszWMHasVideo               = WideString('HasVideo');
  {$EXTERNALSYM g_wszWMHasVideo}
  g_wszWMCurrentBitrate         = WideString('CurrentBitrate');
  {$EXTERNALSYM g_wszWMCurrentBitrate}
  g_wszWMOptimalBitrate         = WideString('OptimalBitrate');
  {$EXTERNALSYM g_wszWMOptimalBitrate}
  g_wszWMHasAttachedImages      = WideString('HasAttachedImages');
  {$EXTERNALSYM g_wszWMHasAttachedImages}
  g_wszWMSkipBackward           = WideString('Can_Skip_Backward');
  {$EXTERNALSYM g_wszWMSkipBackward}
  g_wszWMSkipForward            = WideString('Can_Skip_Forward');
  {$EXTERNALSYM g_wszWMSkipForward}
  g_wszWMNumberOfFrames         = WideString('NumberOfFrames');
  {$EXTERNALSYM g_wszWMNumberOfFrames}
  g_wszWMFileSize               = WideString('FileSize');
  {$EXTERNALSYM g_wszWMFileSize}
  g_wszWMHasArbitraryDataStream = WideString('HasArbitraryDataStream');
  {$EXTERNALSYM g_wszWMHasArbitraryDataStream}
  g_wszWMHasFileTransferStream  = WideString('HasFileTransferStream');
  {$EXTERNALSYM g_wszWMHasFileTransferStream}
  g_wszWMContainerFormat        = WideString('WM/ContainerFormat');
  {$EXTERNALSYM g_wszWMContainerFormat}

////////////////////////////////////////////////////////////////
//
// The content description object supports 5 basic attributes.
//

  g_dwWMContentAttributes = LongWord(5);
  {$EXTERNALSYM g_dwWMContentAttributes}
  g_wszWMTitle        = WideString('Title');
  {$EXTERNALSYM g_wszWMTitle}
  g_wszWMAuthor       = WideString('Author');
  {$EXTERNALSYM g_wszWMAuthor}
  g_wszWMDescription  = WideString('Description');
  {$EXTERNALSYM g_wszWMDescription}
  g_wszWMRating       = WideString('Rating');
  {$EXTERNALSYM g_wszWMRating}
  g_wszWMCopyright    = WideString('Copyright');
  {$EXTERNALSYM g_wszWMCopyright}

////////////////////////////////////////////////////////////////
//
// These attributes are used to configure and query DRM settings in the reader and writer.
//

  g_wszWMUse_DRM                   = WideString('Use_DRM');
  {$EXTERNALSYM g_wszWMUse_DRM}
  g_wszWMDRM_Flags                 = WideString('DRM_Flags');
  {$EXTERNALSYM g_wszWMDRM_Flags}
  g_wszWMDRM_Level                 = WideString('DRM_Level');
  {$EXTERNALSYM g_wszWMDRM_Level}
  g_wszWMUse_Advanced_DRM          = WideString('Use_Advanced_DRM');
  {$EXTERNALSYM g_wszWMUse_Advanced_DRM}
  g_wszWMDRM_KeySeed               = WideString('DRM_KeySeed');
  {$EXTERNALSYM g_wszWMDRM_KeySeed}
  g_wszWMDRM_KeyID                 = WideString('DRM_KeyID');
  {$EXTERNALSYM g_wszWMDRM_KeyID}
  g_wszWMDRM_ContentID             = WideString('DRM_ContentID');
  {$EXTERNALSYM g_wszWMDRM_ContentID}
  g_wszWMDRM_IndividualizedVersion = WideString('DRM_IndividualizedVersion');
  {$EXTERNALSYM g_wszWMDRM_IndividualizedVersion}
  g_wszWMDRM_LicenseAcqURL         = WideString('DRM_LicenseAcqURL');
  {$EXTERNALSYM g_wszWMDRM_LicenseAcqURL}
  g_wszWMDRM_V1LicenseAcqURL       = WideString('DRM_V1LicenseAcqURL');
  {$EXTERNALSYM g_wszWMDRM_V1LicenseAcqURL}
  g_wszWMDRM_HeaderSignPrivKey     = WideString('DRM_HeaderSignPrivKey');
  {$EXTERNALSYM g_wszWMDRM_HeaderSignPrivKey}
  g_wszWMDRM_LASignaturePrivKey    = WideString('DRM_LASignaturePrivKey');
  {$EXTERNALSYM g_wszWMDRM_LASignaturePrivKey}
  g_wszWMDRM_LASignatureCert       = WideString('DRM_LASignatureCert');
  {$EXTERNALSYM g_wszWMDRM_LASignatureCert}
  g_wszWMDRM_LASignatureLicSrvCert = WideString('DRM_LASignatureLicSrvCert');
  {$EXTERNALSYM g_wszWMDRM_LASignatureLicSrvCert}
  g_wszWMDRM_LASignatureRootCert   = WideString('DRM_LASignatureRootCert');
  {$EXTERNALSYM g_wszWMDRM_LASignatureRootCert}

////////////////////////////////////////////////////////////////
//
// These are the additional attributes defined in the WM attribute
// namespace that give information about the content.
//

  g_wszWMAlbumTitle    = WideString('WM/AlbumTitle');
  {$EXTERNALSYM g_wszWMAlbumTitle}
  g_wszWMTrack         = WideString('WM/Track');
  {$EXTERNALSYM g_wszWMTrack}
  g_wszWMPromotionURL  = WideString('WM/PromotionURL');
  {$EXTERNALSYM g_wszWMPromotionURL}
  g_wszWMAlbumCoverURL = WideString('WM/AlbumCoverURL');
  {$EXTERNALSYM g_wszWMAlbumCoverURL}
  g_wszWMGenre         = WideString('WM/Genre');
  {$EXTERNALSYM g_wszWMGenre}
  g_wszWMYear          = WideString('WM/Year');
  {$EXTERNALSYM g_wszWMYear}
  g_wszWMGenreID       = WideString('WM/GenreID');
  {$EXTERNALSYM g_wszWMGenreID}
  g_wszWMMCDI          = WideString('WM/MCDI');
  {$EXTERNALSYM g_wszWMMCDI}
  g_wszWMComposer      = WideString('WM/Composer');
  {$EXTERNALSYM g_wszWMComposer}
  g_wszWMLyrics        = WideString('WM/Lyrics');
  {$EXTERNALSYM g_wszWMLyrics}
  g_wszWMTrackNumber   = WideString('WM/TrackNumber');
  {$EXTERNALSYM g_wszWMTrackNumber}
  g_wszWMToolName      = WideString('WM/ToolName');
  {$EXTERNALSYM g_wszWMToolName}
  g_wszWMToolVersion   = WideString('WM/ToolVersion');
  {$EXTERNALSYM g_wszWMToolVersion}
  g_wszWMIsVBR         = WideString('IsVBR');
  {$EXTERNALSYM g_wszWMIsVBR}

//
// WM/AlbumArtist is a potentially different value than Author
//
  g_wszWMAlbumArtist = WideString('WM/AlbumArtist');
  {$EXTERNALSYM g_wszWMAlbumArtist}

////////////////////////////////////////////////////////////////
//
// These optional attributes may be used to give information
// about the branding of the content.
//

  g_wszWMBannerImageType = WideString('BannerImageType');
  {$EXTERNALSYM g_wszWMBannerImageType}
  g_wszWMBannerImageData = WideString('BannerImageData');
  {$EXTERNALSYM g_wszWMBannerImageData}
  g_wszWMBannerImageURL  = WideString('BannerImageURL');
  {$EXTERNALSYM g_wszWMBannerImageURL}
  g_wszWMCopyrightURL    = WideString('CopyrightURL');
  {$EXTERNALSYM g_wszWMCopyrightURL}

////////////////////////////////////////////////////////////////
//
// Optional attributes, used to give information
// about video stream properties.
//

  g_wszWMAspectRatioX = WideString('AspectRatioX');
  {$EXTERNALSYM g_wszWMAspectRatioX}
  g_wszWMAspectRatioY = WideString('AspectRatioY');
  {$EXTERNALSYM g_wszWMAspectRatioY}

////////////////////////////////////////////////////////////////
//
// Optional attributes, used to give information
// about the overall streaming properties of VBR files.
// This attribute takes the format:
//  Word wReserved (must be 0)
//  WM_LEAKY_BUCKET_PAIR pair1
//  WM_LEAKY_BUCKET_PAIR pair2
//  ...
//
  g_wszASFLeakyBucketPairs = WideString('ASFLeakyBucketPairs');
  {$EXTERNALSYM g_wszASFLeakyBucketPairs}

////////////////////////////////////////////////////////////////
//
// The NSC file supports the following attributes.
//
  g_dwWMNSCAttributes = LongWord(5);
  {$EXTERNALSYM g_dwWMNSCAttributes}
  g_wszWMNSCName        = WideString('NSC_Name');
  {$EXTERNALSYM g_wszWMNSCName}
  g_wszWMNSCAddress     = WideString('NSC_Address');
  {$EXTERNALSYM g_wszWMNSCAddress}
  g_wszWMNSCPhone       = WideString('NSC_Phone');
  {$EXTERNALSYM g_wszWMNSCPhone}
  g_wszWMNSCEmail       = WideString('NSC_Email');
  {$EXTERNALSYM g_wszWMNSCEmail}
  g_wszWMNSCDescription = WideString('NSC_Description');
  {$EXTERNALSYM g_wszWMNSCDescription}


///////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////
//
// Attributes introduced in V9
//
  g_wszWMWriter                  = WideString('WM/Writer');
  {$EXTERNALSYM g_wszWMWriter}
  g_wszWMConductor               = WideString('WM/Conductor');
  {$EXTERNALSYM g_wszWMConductor}
  g_wszWMProducer                = WideString('WM/Producer');
  {$EXTERNALSYM g_wszWMProducer}
  g_wszWMDirector                = WideString('WM/Director');
  {$EXTERNALSYM g_wszWMDirector}
  g_wszWMContentGroupDescription = WideString('WM/ContentGroupDescription');
  {$EXTERNALSYM g_wszWMContentGroupDescription}
  g_wszWMSubTitle                = WideString('WM/SubTitle');
  {$EXTERNALSYM g_wszWMSubTitle}
  g_wszWMPartOfSet               = WideString('WM/PartOfSet');
  {$EXTERNALSYM g_wszWMPartOfSet}
  g_wszWMProtectionType          = WideString('WM/ProtectionType');
  {$EXTERNALSYM g_wszWMProtectionType}
  g_wszWMVideoHeight             = WideString('WM/VideoHeight');
  {$EXTERNALSYM g_wszWMVideoHeight}
  g_wszWMVideoWidth              = WideString('WM/VideoWidth');
  {$EXTERNALSYM g_wszWMVideoWidth}
  g_wszWMVideoFrameRate          = WideString('WM/VideoFrameRate');
  {$EXTERNALSYM g_wszWMVideoFrameRate}
  g_wszWMMediaClassPrimaryID     = WideString('WM/MediaClassPrimaryID');
  {$EXTERNALSYM g_wszWMMediaClassPrimaryID}
  g_wszWMMediaClassSecondaryID   = WideString('WM/MediaClassSecondaryID');
  {$EXTERNALSYM g_wszWMMediaClassSecondaryID}
  g_wszWMPeriod                  = WideString('WM/Period');
  {$EXTERNALSYM g_wszWMPeriod}
  g_wszWMCategory                = WideString('WM/Category');
  {$EXTERNALSYM g_wszWMCategory}
  g_wszWMPicture                 = WideString('WM/Picture');
  {$EXTERNALSYM g_wszWMPicture}
  g_wszWMLyrics_Synchronised     = WideString('WM/Lyrics_Synchronised');
  {$EXTERNALSYM g_wszWMLyrics_Synchronised}
  g_wszWMOriginalLyricist        = WideString('WM/OriginalLyricist');
  {$EXTERNALSYM g_wszWMOriginalLyricist}
  g_wszWMOriginalArtist          = WideString('WM/OriginalArtist');
  {$EXTERNALSYM g_wszWMOriginalArtist}
  g_wszWMOriginalAlbumTitle      = WideString('WM/OriginalAlbumTitle');
  {$EXTERNALSYM g_wszWMOriginalAlbumTitle}
  g_wszWMOriginalReleaseYear     = WideString('WM/OriginalReleaseYear');
  {$EXTERNALSYM g_wszWMOriginalReleaseYear}
  g_wszWMOriginalFilename        = WideString('WM/OriginalFilename');
  {$EXTERNALSYM g_wszWMOriginalFilename}
  g_wszWMPublisher               = WideString('WM/Publisher');
  {$EXTERNALSYM g_wszWMPublisher}
  g_wszWMEncodedBy               = WideString('WM/EncodedBy');
  {$EXTERNALSYM g_wszWMEncodedBy}
  g_wszWMEncodingSettings        = WideString('WM/EncodingSettings');
  {$EXTERNALSYM g_wszWMEncodingSettings}
  g_wszWMEncodingTime            = WideString('WM/EncodingTime');
  {$EXTERNALSYM g_wszWMEncodingTime}
  g_wszWMAuthorURL               = WideString('WM/AuthorURL');
  {$EXTERNALSYM g_wszWMAuthorURL}
  g_wszWMUserWebURL              = WideString('WM/UserWebURL');
  {$EXTERNALSYM g_wszWMUserWebURL}
  g_wszWMAudioFileURL            = WideString('WM/AudioFileURL');
  {$EXTERNALSYM g_wszWMAudioFileURL}
  g_wszWMAudioSourceURL          = WideString('WM/AudioSourceURL');
  {$EXTERNALSYM g_wszWMAudioSourceURL}
  g_wszWMLanguage                = WideString('WM/Language');
  {$EXTERNALSYM g_wszWMLanguage}
  g_wszWMParentalRating          = WideString('WM/ParentalRating');
  {$EXTERNALSYM g_wszWMParentalRating}
  g_wszWMBeatsPerMinute          = WideString('WM/BeatsPerMinute');
  {$EXTERNALSYM g_wszWMBeatsPerMinute}
  g_wszWMInitialKey              = WideString('WM/InitialKey');
  {$EXTERNALSYM g_wszWMInitialKey}
  g_wszWMMood                    = WideString('WM/Mood');
  {$EXTERNALSYM g_wszWMMood}
  g_wszWMText                    = WideString('WM/Text');
  {$EXTERNALSYM g_wszWMText}
  g_wszWMDVDID                   = WideString('WM/DVDID');
  {$EXTERNALSYM g_wszWMDVDID}
  g_wszWMWMContentID             = WideString('WM/WMContentID');
  {$EXTERNALSYM g_wszWMWMContentID}
  g_wszWMWMCollectionID          = WideString('WM/WMCollectionID');
  {$EXTERNALSYM g_wszWMWMCollectionID}
  g_wszWMWMCollectionGroupID     = WideString('WM/WMCollectionGroupID');
  {$EXTERNALSYM g_wszWMWMCollectionGroupID}
  g_wszWMUniqueFileIdentifier    = WideString('WM/UniqueFileIdentifier');
  {$EXTERNALSYM g_wszWMUniqueFileIdentifier}
  g_wszWMModifiedBy              = WideString('WM/ModifiedBy');
  {$EXTERNALSYM g_wszWMModifiedBy}
  g_wszWMRadioStationName        = WideString('WM/RadioStationName');
  {$EXTERNALSYM g_wszWMRadioStationName}
  g_wszWMRadioStationOwner       = WideString('WM/RadioStationOwner');
  {$EXTERNALSYM g_wszWMRadioStationOwner}
  g_wszWMPlaylistDelay           = WideString('WM/PlaylistDelay');
  {$EXTERNALSYM g_wszWMPlaylistDelay}
  g_wszWMCodec                   = WideString('WM/Codec');
  {$EXTERNALSYM g_wszWMCodec}
  g_wszWMDRM                     = WideString('WM/DRM');
  {$EXTERNALSYM g_wszWMDRM}
  g_wszWMISRC                    = WideString('WM/ISRC');
  {$EXTERNALSYM g_wszWMISRC}
  g_wszWMProvider                = WideString('WM/Provider');
  {$EXTERNALSYM g_wszWMProvider}
  g_wszWMProviderRating          = WideString('WM/ProviderRating');
  {$EXTERNALSYM g_wszWMProviderRating}
  g_wszWMProviderStyle           = WideString('WM/ProviderStyle');
  {$EXTERNALSYM g_wszWMProviderStyle}
  g_wszWMContentDistributor      = WideString('WM/ContentDistributor');
  {$EXTERNALSYM g_wszWMContentDistributor}
  g_wszWMSubscriptionContentID   = WideString('WM/SubscriptionContentID');
  {$EXTERNALSYM g_wszWMSubscriptionContentID}
  g_wszWMWMADRCPeakReference     = WideString('WM/WMADRCPeakReference');
  {$EXTERNALSYM g_wszWMWMADRCPeakReference}
  g_wszWMWMADRCPeakTarget        = WideString('WM/WMADRCPeakTarget');
  {$EXTERNALSYM g_wszWMWMADRCPeakTarget}
  g_wszWMWMADRCAverageReference  = WideString('WM/WMADRCAverageReference');
  {$EXTERNALSYM g_wszWMWMADRCAverageReference}
  g_wszWMWMADRCAverageTarget     = WideString('WM/WMADRCAverageTarget');
  {$EXTERNALSYM g_wszWMWMADRCAverageTarget}

///////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////
//
// These are setting names for use in Get/SetOutputSetting
//
  g_wszEarlyDataDelivery       = WideString('EarlyDataDelivery');
  {$EXTERNALSYM g_wszEarlyDataDelivery}
  g_wszJustInTimeDecode        = WideString('JustInTimeDecode');
  {$EXTERNALSYM g_wszJustInTimeDecode}
  g_wszSingleOutputBuffer      = WideString('SingleOutputBuffer');
  {$EXTERNALSYM g_wszSingleOutputBuffer}
  g_wszSoftwareScaling         = WideString('SoftwareScaling');
  {$EXTERNALSYM g_wszSoftwareScaling}
  g_wszDeliverOnReceive        = WideString('DeliverOnReceive');
  {$EXTERNALSYM g_wszDeliverOnReceive}
  g_wszScrambledAudio          = WideString('ScrambledAudio');
  {$EXTERNALSYM g_wszScrambledAudio}
  g_wszDedicatedDeliveryThread = WideString('DedicatedDeliveryThread');
  {$EXTERNALSYM g_wszDedicatedDeliveryThread}
  g_wszEnableDiscreteOutput    = WideString('EnableDiscreteOutput');
  {$EXTERNALSYM g_wszEnableDiscreteOutput}
  g_wszSpeakerConfig           = WideString('SpeakerConfig');
  {$EXTERNALSYM g_wszSpeakerConfig}
  g_wszDynamicRangeControl     = WideString('DynamicRangeControl');
  {$EXTERNALSYM g_wszDynamicRangeControl}
  g_wszAllowInterlacedOutput   = WideString('AllowInterlacedOutput');
  {$EXTERNALSYM g_wszAllowInterlacedOutput}
  g_wszVideoSampleDurations    = WideString('VideoSampleDurations');
  {$EXTERNALSYM g_wszVideoSampleDurations}
  g_wszStreamLanguage          = WideString('StreamLanguage');
  {$EXTERNALSYM g_wszStreamLanguage}


///////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////
//
// These are setting names for use in Get/SetInputSetting
//
  g_wszDeinterlaceMode                  = WideString('DeinterlaceMode');
  {$EXTERNALSYM g_wszDeinterlaceMode}
  g_wszInitialPatternForInverseTelecine = WideString('InitialPatternForInverseTelecine');
  {$EXTERNALSYM g_wszInitialPatternForInverseTelecine}
  g_wszJPEGCompressionQuality           = WideString('JPEGCompressionQuality');
  {$EXTERNALSYM g_wszJPEGCompressionQuality}
  g_wszWatermarkCLSID                   = WideString('WatermarkCLSID');
  {$EXTERNALSYM g_wszWatermarkCLSID}
  g_wszWatermarkConfig                  = WideString('WatermarkConfig');
  {$EXTERNALSYM g_wszWatermarkConfig}
  g_wszInterlacedCoding                 = WideString('InterlacedCoding');
  {$EXTERNALSYM g_wszInterlacedCoding}
  g_wszFixedFrameRate                   = WideString('FixedFrameRate');
  {$EXTERNALSYM g_wszFixedFrameRate}


////////////////////////////////////////////////////////////////
//
// All known IWMPropertyVault property names
//
// g_wszOriginalSourceFormatTag is obsolete and has been superceded by g_wszOriginalWaveFormat
  g_wszOriginalSourceFormatTag    = WideString('_SOURCEFORMATTAG');
  {$EXTERNALSYM g_wszOriginalSourceFormatTag}
  g_wszOriginalWaveFormat         = WideString('_ORIGINALWAVEFORMAT');
  {$EXTERNALSYM g_wszOriginalWaveFormat}
  g_wszEDL                        = WideString('_EDL');
  {$EXTERNALSYM g_wszEDL}
  g_wszComplexity                 = WideString('_COMPLEXITYEX');
  {$EXTERNALSYM g_wszComplexity}
  g_wszDecoderComplexityRequested = WideString('_DECODERCOMPLEXITYPROFILE');
  {$EXTERNALSYM g_wszDecoderComplexityRequested}


////////////////////////////////////////////////////////////////
//
// All known IWMIStreamProps property names
//
  g_wszReloadIndexOnSeek            = WideString('ReloadIndexOnSeek');
  {$EXTERNALSYM g_wszReloadIndexOnSeek}
  g_wszStreamNumIndexObjects        = WideString('StreamNumIndexObjects');
  {$EXTERNALSYM g_wszStreamNumIndexObjects}
  g_wszFailSeekOnError              = WideString('FailSeekOnError');
  {$EXTERNALSYM g_wszFailSeekOnError}
  g_wszPermitSeeksBeyondEndOfStream = WideString('PermitSeeksBeyondEndOfStream');
  {$EXTERNALSYM g_wszPermitSeeksBeyondEndOfStream}
  g_wszUsePacketAtSeekPoint         = WideString('UsePacketAtSeekPoint');
  {$EXTERNALSYM g_wszUsePacketAtSeekPoint}
  g_wszSourceBufferTime             = WideString('SourceBufferTime');
  {$EXTERNALSYM g_wszSourceBufferTime}
  g_wszSourceMaxBytesAtOnce         = WideString('SourceMaxBytesAtOnce');
  {$EXTERNALSYM g_wszSourceMaxBytesAtOnce}


///////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////
//
// VBR encoding settings
//
  g_wszVBREnabled         = WideString('_VBRENABLED');
  {$EXTERNALSYM g_wszVBREnabled}
  g_wszVBRQuality         = WideString('_VBRQUALITY');
  {$EXTERNALSYM g_wszVBRQuality}
  g_wszVBRBitrateMax      = WideString('_RMAX');
  {$EXTERNALSYM g_wszVBRBitrateMax}
  g_wszVBRBufferWindowMax = WideString('_BMAX');
  {$EXTERNALSYM g_wszVBRBufferWindowMax}


////////////////////////////////////////////////////////////////
//
// VBR Video settings
//
  g_wszVBRPeak       = WideString('VBR Peak');
  {$EXTERNALSYM g_wszVBRPeak}
  g_wszBufferAverage = WideString('Buffer Average');
  {$EXTERNALSYM g_wszBufferAverage}


////////////////////////////////////////////////////////////////
//
// Codec encoding complexity settings
//
// g_wszComplexity should be used to set desired encoding complexity on the
// stream's IWMPropertyVault (see above for definition)
// The below settings can be queried from IWMCodecInfo3::GetCodecProp()
//
  g_wszComplexityMax     = WideString('_COMPLEXITYEXMAX');
  {$EXTERNALSYM g_wszComplexityMax}
  g_wszComplexityOffline = WideString('_COMPLEXITYEXOFFLINE');
  {$EXTERNALSYM g_wszComplexityOffline}
  g_wszComplexityLive    = WideString('_COMPLEXITYEXLIVE');
  {$EXTERNALSYM g_wszComplexityLive}
  g_wszIsVBRSupported    = WideString('_ISVBRSUPPORTED');
  {$EXTERNALSYM g_wszIsVBRSupported}


////////////////////////////////////////////////////////////////
//
// Codec enumeration settings
//
// g_wszVBREnabled can be used as a codec enumeration setting (see above for definition)
  g_wszNumPasses = WideString('_PASSESUSED');
  {$EXTERNALSYM g_wszNumPasses}


////////////////////////////////////////////////////////////////
//
// These are WMA Voice V9 attribute names and values
//
  g_wszMusicSpeechClassMode = WideString('MusicSpeechClassMode');
  {$EXTERNALSYM g_wszMusicSpeechClassMode}
  g_wszMusicClassMode       = WideString('MusicClassMode');
  {$EXTERNALSYM g_wszMusicClassMode}
  g_wszSpeechClassMode      = WideString('SpeechClassMode');
  {$EXTERNALSYM g_wszSpeechClassMode}
  g_wszMixedClassMode       = WideString('MixedClassMode');
  {$EXTERNALSYM g_wszMixedClassMode}


////////////////////////////////////////////////////////////////
//
// The WMA Voice V9 supports the following format property.
//
  g_wszSpeechCaps = WideString('SpeechFormatCap');
  {$EXTERNALSYM g_wszSpeechCaps}


////////////////////////////////////////////////////////////////
//
// Multi-channel WMA properties
//
  g_wszPeakValue              = WideString('PeakValue');
  {$EXTERNALSYM g_wszPeakValue}
  g_wszAverageLevel           = WideString('AverageLevel');
  {$EXTERNALSYM g_wszAverageLevel}
  g_wszFold6To2Channels3      = WideString('Fold6To2Channels3');
  {$EXTERNALSYM g_wszFold6To2Channels3}
  g_wszFoldToChannelsTemplate = WideString('Fold%luTo%luChannels%lu');
  {$EXTERNALSYM g_wszFoldToChannelsTemplate}


////////////////////////////////////////////////////////////////
//
// Complexity profile description strings
//
  g_wszDeviceConformanceTemplate = WideString('DeviceConformanceTemplate');
  {$EXTERNALSYM g_wszDeviceConformanceTemplate}


////////////////////////////////////////////////////////////////
//
// Frame interpolation on video decode
//
  g_wszEnableFrameInterpolation = WideString('EnableFrameInterpolation');
  {$EXTERNALSYM g_wszEnableFrameInterpolation}


////////////////////////////////////////////////////////////////
//
// Needs previous sample for Delta frame on video decode
//
  g_wszNeedsPreviousSample = WideString('NeedsPreviousSample');
  {$EXTERNALSYM g_wszNeedsPreviousSample}


///////////////////////////////////////////////////////////////////////////////
//
// Enumerations and flags used by the SDK.
//
///////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////
//
// Flags that can be passed into the Start method of IWMReader
//
  WM_START_CURRENTPOSITION = Int64(-1);
  {$EXTERNALSYM WM_START_CURRENTPOSITION}

//
// Flag to force overwrite of existing license backup
//
  WM_BACKUP_OVERWRITE       = LongWord($00000001);
  {$EXTERNALSYM WM_BACKUP_OVERWRITE}
  WM_RESTORE_INDIVIDUALIZE  = LongWord($00000002);
  {$EXTERNALSYM WM_RESTORE_INDIVIDUALIZE}

//
// Wave format ex type
//
  WAVE_FORMAT_DRM           = $0009;
  {$EXTERNALSYM WAVE_FORMAT_DRM}

//
// Sample type ids for Web Streams
//

  WEBSTREAM_SAMPLE_TYPE_FILE           = $1;
  {$EXTERNALSYM WEBSTREAM_SAMPLE_TYPE_FILE}
  WEBSTREAM_SAMPLE_TYPE_RENDER         = $2;
  {$EXTERNALSYM WEBSTREAM_SAMPLE_TYPE_RENDER}

//
// The flags that can be set on a sample (input or output).
//

  WM_SF_CLEANPOINT    = $1;
  {$EXTERNALSYM WM_SF_CLEANPOINT}
  WM_SF_DISCONTINUITY = $2;
  {$EXTERNALSYM WM_SF_DISCONTINUITY}
  WM_SF_DATALOSS      = $4;
  {$EXTERNALSYM WM_SF_DATALOSS}

//
// These flags might be set for a call to the IWMReaderAllocatorEx methods.
//
  WM_SFEX_NOTASYNCPOINT = $2;
  {$EXTERNALSYM WM_SFEX_NOTASYNCPOINT}
  WM_SFEX_DATALOSS      = $4;
  {$EXTERNALSYM WM_SFEX_DATALOSS}

//
// Status messages that the reader and index objects can
// pass in the OnStatus call.
//
type
  WMT_STATUS = (
    WMT_ERROR,
    WMT_OPENED,
    WMT_BUFFERING_START,
    WMT_BUFFERING_STOP,
    WMT_END_OF_FILE,
{$IFDEF COMPILER6_UP}
    WMT_EOF = 4, 
{$ENDIF}
    WMT_END_OF_SEGMENT,
    WMT_END_OF_STREAMING,
    WMT_LOCATING,
    WMT_CONNECTING,
    WMT_NO_RIGHTS,
    WMT_MISSING_CODEC,
    WMT_STARTED,
    WMT_STOPPED,
    WMT_CLOSED,
    WMT_STRIDING,
    WMT_TIMER,
    WMT_INDEX_PROGRESS,
    WMT_SAVEAS_START,
    WMT_SAVEAS_STOP,
    WMT_NEW_SOURCEFLAGS,
    WMT_NEW_METADATA,
    WMT_BACKUPRESTORE_BEGIN,
    WMT_SOURCE_SWITCH,
    WMT_ACQUIRE_LICENSE,
    WMT_INDIVIDUALIZE,
    WMT_NEEDS_INDIVIDUALIZATION,
    WMT_NO_RIGHTS_EX,
    WMT_BACKUPRESTORE_END,
    WMT_BACKUPRESTORE_CONNECTING,
    WMT_BACKUPRESTORE_DISCONNECTING,
    WMT_ERROR_WITHURL,
    WMT_RESTRICTED_LICENSE,
    WMT_CLIENT_CONNECT,
    WMT_CLIENT_DISCONNECT,
    WMT_NATIVE_OUTPUT_PROPS_CHANGED,
    WMT_RECONNECT_START,
    WMT_RECONNECT_END,
    WMT_CLIENT_CONNECT_EX,
    WMT_CLIENT_DISCONNECT_EX,
    WMT_SET_FEC_SPAN,
    WMT_PREROLL_READY,
    WMT_PREROLL_COMPLETE,
    WMT_CLIENT_PROPERTIES,
    WMT_LICENSEURL_SIGNATURE_STATE
  );
  {$EXTERNALSYM WMT_STATUS}
  TWMTStatus = WMT_STATUS;

{$IFNDEF COMPILER6_UP}
const
  WMT_EOF = WMT_END_OF_FILE;
  {$NODEFINE WMT_EOF}
{$ENDIF}


type
  WMT_RIGHTS = {$IFDEF TYPE_IDENTITY}type {$ENDIF} LongWord;
  {$EXTERNALSYM WMT_RIGHTS}
  const
    WMT_RIGHT_PLAYBACK                  = $00000001;
    {$EXTERNALSYM WMT_RIGHT_PLAYBACK}
    WMT_RIGHT_COPY_TO_NON_SDMI_DEVICE   = $00000002;
    {$EXTERNALSYM WMT_RIGHT_COPY_TO_NON_SDMI_DEVICE}
    WMT_RIGHT_COPY_TO_CD                = $00000008;
    {$EXTERNALSYM WMT_RIGHT_COPY_TO_CD}
    WMT_RIGHT_COPY_TO_SDMI_DEVICE       = $00000010;
    {$EXTERNALSYM WMT_RIGHT_COPY_TO_SDMI_DEVICE}
    WMT_RIGHT_ONE_TIME                  = $00000020;
    {$EXTERNALSYM WMT_RIGHT_ONE_TIME}
    WMT_RIGHT_SAVE_STREAM_PROTECTED     = $00000040;
    {$EXTERNALSYM WMT_RIGHT_SAVE_STREAM_PROTECTED}
    WMT_RIGHT_SDMI_TRIGGER              = $00010000;
    {$EXTERNALSYM WMT_RIGHT_SDMI_TRIGGER}
    WMT_RIGHT_SDMI_NOMORECOPIES         = $00020000;
    {$EXTERNALSYM WMT_RIGHT_SDMI_NOMORECOPIES}

//
// Stream selection statuses (stati?).
//
type
  PWMTStreamSelection = ^TWMTStreamSelection;
  WMT_STREAM_SELECTION = (
    WMT_OFF,
    WMT_CLEANPOINT_ONLY,
    WMT_ON
  );
  {$EXTERNALSYM WMT_STREAM_SELECTION}
  TWMTStreamSelection = WMT_STREAM_SELECTION;

//
// Image types (used with CBO)
//
  WMT_IMAGE_TYPE = (
    WMT_IT_NONE,
    WMT_IT_BITMAP,
    WMT_IT_JPEG,
    WMT_IT_GIF
  );
  {$EXTERNALSYM WMT_IMAGE_TYPE}
  TWMTImageType = WMT_IMAGE_TYPE;

//
// Attribute datatypes.
//
  WMT_ATTR_DATATYPE = (
    WMT_TYPE_DWORD,
    WMT_TYPE_STRING,
    WMT_TYPE_BINARY,
    WMT_TYPE_BOOL,
    WMT_TYPE_QWORD,
    WMT_TYPE_WORD,
    WMT_TYPE_GUID
  );
  {$EXTERNALSYM WMT_ATTR_DATATYPE}
  TWMTAttrDataType = WMT_ATTR_DATATYPE;

//
// Types of images that can be stored in the header of a Windows Media File.
//
  WMT_ATTR_IMAGETYPE = (
{$IFNDEF COMPILER6_UP}
    WMT_IMAGETYPE_INVALID_0,
    WMT_IMAGETYPE_BITMAP,
{$ELSE}
    WMT_IMAGETYPE_BITMAP = 1,
{$ENDIF}
    WMT_IMAGETYPE_JPEG,
    WMT_IMAGETYPE_GIF
  );
  {$EXTERNALSYM WMT_ATTR_IMAGETYPE}
  TWMTAttrImageType = WMT_ATTR_IMAGETYPE;

//
// Windows Media versions.
//
  WMT_VERSION = {$IFDEF TYPE_IDENTITY}type {$ENDIF} LongWord;
  {$EXTERNALSYM WMT_VERSION}
  const
    WMT_VER_4_0 = $00040000;
    {$EXTERNALSYM WMT_VER_4_0}
    WMT_VER_7_0 = $00070000;
    {$EXTERNALSYM WMT_VER_7_0}
    WMT_VER_8_0 = $00080000;
    {$EXTERNALSYM WMT_VER_8_0}
    WMT_VER_9_0 = $00090000;
    {$EXTERNALSYM WMT_VER_9_0}

//
// Storage formats.  These are the values returned when querying
// the WM/ContainerFormat attribute (g_wszWMContainerFormat).
//
type
  tagWMT_STORAGE_FORMAT = (
    WMT_Storage_Format_MP3,
    WMT_Storage_Format_V1
  );
  {$EXTERNALSYM tagWMT_STORAGE_FORMAT}
  WMT_STORAGE_FORMAT = tagWMT_STORAGE_FORMAT;
  {$EXTERNALSYM WMT_STORAGE_FORMAT}
  TWMTStorageFormat = tagWMT_STORAGE_FORMAT;

  tagWMT_DRMLA_TRUST = (
    WMT_DRMLA_UNTRUSTED,
    WMT_DRMLA_TRUSTED,
    WMT_DRMLA_TAMPERED
  );
  {$EXTERNALSYM tagWMT_DRMLA_TRUST}
  WMT_DRMLA_TRUST = tagWMT_DRMLA_TRUST;
  {$EXTERNALSYM WMT_DRMLA_TRUST}
  TWMTDRMLATrust = tagWMT_DRMLA_TRUST;

//
// Type of data communication protocol (reliable or unreliable)
//
  tagWMT_TRANSPORT_TYPE = (
   WMT_Transport_Type_Unreliable,
   WMT_Transport_Type_Reliable
  );
  {$EXTERNALSYM tagWMT_TRANSPORT_TYPE}
  WMT_TRANSPORT_TYPE = tagWMT_TRANSPORT_TYPE;
  {$EXTERNALSYM WMT_TRANSPORT_TYPE}
  TWMTTransportType = tagWMT_TRANSPORT_TYPE;

//
// Protocols that the network sink supports.
//
  WMT_NET_PROTOCOL = (
    WMT_PROTOCOL_HTTP
  );
  {$EXTERNALSYM WMT_NET_PROTOCOL}
  TWMTNetProtocol = WMT_NET_PROTOCOL;

//
// The reader supports a number of different types of playback, each with
// slightly different characteristics.
//
  WMT_PLAY_MODE = (
    WMT_PLAY_MODE_AUTOSELECT,
    WMT_PLAY_MODE_LOCAL,
    WMT_PLAY_MODE_DOWNLOAD,
    WMT_PLAY_MODE_STREAMING
  );
  {$EXTERNALSYM WMT_PLAY_MODE}
  TWMTPlayMode = WMT_PLAY_MODE;

//
// Network Proxy settings for the reader
//
  WMT_PROXY_SETTINGS = (
    WMT_PROXY_SETTING_NONE,
    WMT_PROXY_SETTING_MANUAL,
    WMT_PROXY_SETTING_AUTO,
    WMT_PROXY_SETTING_BROWSER,       // Only valid for HTTP
    WMT_PROXY_SETTING_MAX
  );
  {$EXTERNALSYM WMT_PROXY_SETTINGS}
  TWMTProxySettings = WMT_PROXY_SETTINGS;

  WMT_CODEC_INFO_TYPE = {$IFDEF TYPE_IDENTITY}type {$ENDIF} LongWord;
  {$EXTERNALSYM WMT_CODEC_INFO_TYPE}
  const
    WMT_CODECINFO_AUDIO = 0;            // codec info is a Word = wFormatTag
    {$EXTERNALSYM WMT_CODECINFO_AUDIO}
    WMT_CODECINFO_VIDEO = 1;            // codec info is a LongWord = biCompression
    {$EXTERNALSYM WMT_CODECINFO_VIDEO}
    WMT_CODECINFO_UNKNOWN = $FFFFFFFF;  // codec info is undefined
    {$EXTERNALSYM WMT_CODECINFO_UNKNOWN}

//
// These values can be passed in when setting the DeinterlaceMode
// setting on the writer
//
  WM_DM_NOTINTERLACED                          = LongWord(0);
  {$EXTERNALSYM WM_DM_NOTINTERLACED}
  WM_DM_DEINTERLACE_NORMAL                     = LongWord(1);
  {$EXTERNALSYM WM_DM_DEINTERLACE_NORMAL}
  WM_DM_DEINTERLACE_HALFSIZE                   = LongWord(2);
  {$EXTERNALSYM WM_DM_DEINTERLACE_HALFSIZE}
  WM_DM_DEINTERLACE_HALFSIZEDOUBLERATE         = LongWord(3);
  {$EXTERNALSYM WM_DM_DEINTERLACE_HALFSIZEDOUBLERATE}
  WM_DM_DEINTERLACE_INVERSETELECINE            = LongWord(4);
  {$EXTERNALSYM WM_DM_DEINTERLACE_INVERSETELECINE}
  WM_DM_DEINTERLACE_VERTICALHALFSIZEDOUBLERATE = LongWord(5);
  {$EXTERNALSYM WM_DM_DEINTERLACE_VERTICALHALFSIZEDOUBLERATE}

//
// These values can be passed to further configure the inverse
// telecine process
//

  WM_DM_IT_DISABLE_COHERENT_MODE              = LongWord(0);
  {$EXTERNALSYM WM_DM_IT_DISABLE_COHERENT_MODE}
  WM_DM_IT_FIRST_FRAME_IN_CLIP_IS_AA_TOP      = LongWord(1);
  {$EXTERNALSYM WM_DM_IT_FIRST_FRAME_IN_CLIP_IS_AA_TOP}
  WM_DM_IT_FIRST_FRAME_IN_CLIP_IS_BB_TOP      = LongWord(2);
  {$EXTERNALSYM WM_DM_IT_FIRST_FRAME_IN_CLIP_IS_BB_TOP}
  WM_DM_IT_FIRST_FRAME_IN_CLIP_IS_BC_TOP      = LongWord(3);
  {$EXTERNALSYM WM_DM_IT_FIRST_FRAME_IN_CLIP_IS_BC_TOP}
  WM_DM_IT_FIRST_FRAME_IN_CLIP_IS_CD_TOP      = LongWord(4);
  {$EXTERNALSYM WM_DM_IT_FIRST_FRAME_IN_CLIP_IS_CD_TOP}
  WM_DM_IT_FIRST_FRAME_IN_CLIP_IS_DD_TOP      = LongWord(5);
  {$EXTERNALSYM WM_DM_IT_FIRST_FRAME_IN_CLIP_IS_DD_TOP}
  WM_DM_IT_FIRST_FRAME_IN_CLIP_IS_AA_BOTTOM   = LongWord(6);
  {$EXTERNALSYM WM_DM_IT_FIRST_FRAME_IN_CLIP_IS_AA_BOTTOM}
  WM_DM_IT_FIRST_FRAME_IN_CLIP_IS_BB_BOTTOM   = LongWord(7);
  {$EXTERNALSYM WM_DM_IT_FIRST_FRAME_IN_CLIP_IS_BB_BOTTOM}
  WM_DM_IT_FIRST_FRAME_IN_CLIP_IS_BC_BOTTOM   = LongWord(8);
  {$EXTERNALSYM WM_DM_IT_FIRST_FRAME_IN_CLIP_IS_BC_BOTTOM}
  WM_DM_IT_FIRST_FRAME_IN_CLIP_IS_CD_BOTTOM   = LongWord(9);
  {$EXTERNALSYM WM_DM_IT_FIRST_FRAME_IN_CLIP_IS_CD_BOTTOM}
  WM_DM_IT_FIRST_FRAME_IN_CLIP_IS_DD_BOTTOM   = LongWord(10);
  {$EXTERNALSYM WM_DM_IT_FIRST_FRAME_IN_CLIP_IS_DD_BOTTOM}


//
// Frame level access data structures
//
type
  tagWMT_OFFSET_FORMAT = (
    WMT_OFFSET_FORMAT_100NS,
    WMT_OFFSET_FORMAT_FRAME_NUMBERS,
    WMT_OFFSET_FORMAT_PLAYLIST_OFFSET,
    WMT_OFFSET_FORMAT_TIMECODE
  );
  {$EXTERNALSYM tagWMT_OFFSET_FORMAT}
  WMT_OFFSET_FORMAT = tagWMT_OFFSET_FORMAT;
  {$EXTERNALSYM WMT_OFFSET_FORMAT}
  TWMTOffsetFormat = tagWMT_OFFSET_FORMAT;

//
// Indexer type and index type
//
  tagWMT_INDEXER_TYPE = (
    WMT_IT_PRESENTATION_TIME,
    WMT_IT_FRAME_NUMBERS,
    WMT_IT_TIMECODE
  );
  {$EXTERNALSYM tagWMT_INDEXER_TYPE}
  WMT_INDEXER_TYPE = tagWMT_INDEXER_TYPE;
  {$EXTERNALSYM WMT_INDEXER_TYPE}
  TWMTIndexerType = tagWMT_INDEXER_TYPE;

  tagWMT_INDEX_TYPE = (
  {$IFNDEF COMPILER6_UP}
    WMT_IT_NEAREST_INVALID_0,
    WMT_IT_NEAREST_DATA_UNIT,
  {$ELSE}
    WMT_IT_NEAREST_DATA_UNIT = 1,
  {$ENDIF}
    WMT_IT_NEAREST_OBJECT,
    WMT_IT_NEAREST_CLEAN_POINT
  );
  {$EXTERNALSYM tagWMT_INDEX_TYPE}
  WMT_INDEX_TYPE = tagWMT_INDEX_TYPE;
  {$EXTERNALSYM WMT_INDEX_TYPE}
  TWMTIndexType = tagWMT_INDEX_TYPE;

//
// The types of input accepted by the file sink
//
  WMT_FILESINK_MODE = {$IFDEF TYPE_IDENTITY}type {$ENDIF} LongWord;
  {$EXTERNALSYM WMT_FILESINK_MODE}
  const
    WMT_FM_SINGLE_BUFFERS       = $00000001;
    {$EXTERNALSYM WMT_FM_SINGLE_BUFFERS}
    WMT_FM_FILESINK_DATA_UNITS  = $00000002;
    {$EXTERNALSYM WMT_FM_FILESINK_DATA_UNITS}
    WMT_FM_FILESINK_UNBUFFERED  = $00000004;
    {$EXTERNALSYM WMT_FM_FILESINK_UNBUFFERED}

//
// WMA Voice V9 supports several type of compression,
// profile settting are mapped to these constants
//
type
  tagWMT_MUSICSPEECH_CLASS_MODE = (
    WMT_MS_CLASS_MUSIC,
    WMT_MS_CLASS_SPEECH,
    WMT_MS_CLASS_MIXED
  );
  {$EXTERNALSYM tagWMT_MUSICSPEECH_CLASS_MODE}
  WMT_MUSICSPEECH_CLASS_MODE = tagWMT_MUSICSPEECH_CLASS_MODE;
  {$EXTERNALSYM WMT_MUSICSPEECH_CLASS_MODE}
  TWMTMusicSpeechClassMode = tagWMT_MUSICSPEECH_CLASS_MODE;

  tagWMT_WATERMARK_ENTRY_TYPE = (
  {$IFNDEF COMPILER6_UP}
    WMT_WMETYPE_INVALID_0,
    WMT_WMETYPE_AUDIO,
  {$ELSE}
    WMT_WMETYPE_AUDIO = 1,
  {$ENDIF}
    WMT_WMETYPE_VIDEO
  );
  {$EXTERNALSYM tagWMT_WATERMARK_ENTRY_TYPE}
  WMT_WATERMARK_ENTRY_TYPE = tagWMT_WATERMARK_ENTRY_TYPE;
  {$EXTERNALSYM WMT_WATERMARK_ENTRY_TYPE}
  TWMTWatermarkEntryType = tagWMT_WATERMARK_ENTRY_TYPE;

//
// Dynamic Range Control values for playback
//
const
  WM_PLAYBACK_DRC_HIGH    = 0;
  {$EXTERNALSYM WM_PLAYBACK_DRC_HIGH}
  WM_PLAYBACK_DRC_MEDIUM  = 1;
  {$EXTERNALSYM WM_PLAYBACK_DRC_MEDIUM}
  WM_PLAYBACK_DRC_LOW     = 2;
  {$EXTERNALSYM WM_PLAYBACK_DRC_LOW}

//
// Timecode frame rates.  These are meant to be used as the
// values for the stream-based frame rate metadata attribute.
// See the WM Format SDK docs for more details.
//
  WMT_TIMECODE_FRAMERATE_30      = 0;
  {$EXTERNALSYM WMT_TIMECODE_FRAMERATE_30}
  WMT_TIMECODE_FRAMERATE_30DROP  = 1;
  {$EXTERNALSYM WMT_TIMECODE_FRAMERATE_30DROP}
  WMT_TIMECODE_FRAMERATE_25      = 2;
  {$EXTERNALSYM WMT_TIMECODE_FRAMERATE_25}
  WMT_TIMECODE_FRAMERATE_24      = 3;
  {$EXTERNALSYM WMT_TIMECODE_FRAMERATE_24}


//
// Flags that can be specified in IWMCredentialCallback::AcquireCredentials()
//
type
  WMT_CREDENTIAL_FLAGS = {$IFDEF TYPE_IDENTITY}type {$ENDIF} LongWord;
  {$EXTERNALSYM WMT_CREDENTIAL_FLAGS}
  const
    WMT_CREDENTIAL_SAVE                 = $00000001;       // Save the credentials in a persistent manner
    {$EXTERNALSYM WMT_CREDENTIAL_SAVE}
    WMT_CREDENTIAL_DONT_CACHE           = $00000002;       // Don't cache the credentials in memory
    {$EXTERNALSYM WMT_CREDENTIAL_DONT_CACHE}
    WMT_CREDENTIAL_CLEAR_TEXT           = $00000004;       // Credentials will be sent in clear text
    {$EXTERNALSYM WMT_CREDENTIAL_CLEAR_TEXT}
    WMT_CREDENTIAL_PROXY                = $00000008;       // Credentials are for a proxy server
    {$EXTERNALSYM WMT_CREDENTIAL_PROXY}
    WMT_CREDENTIAL_ENCRYPT              = $00000010;       // Encryption supported/used (in request/response, respectively)
    {$EXTERNALSYM WMT_CREDENTIAL_ENCRYPT}

type
  WM_AETYPE = {$IFDEF TYPE_IDENTITY}type {$ENDIF} LongWord;
  {$EXTERNALSYM WM_AETYPE}
  const
    WM_AETYPE_INCLUDE   = $69;
    {$EXTERNALSYM WM_AETYPE_INCLUDE}
    WM_AETYPE_EXCLUDE   = $65;
    {$EXTERNALSYM WM_AETYPE_EXCLUDE}


///////////////////////////////////////////////////////////////////////////////
//
// Structures used by the SDK.
//
///////////////////////////////////////////////////////////////////////////////

//
// Stream prioritization struct
//
type
  PWMStreamPrioritizationRecord = ^TWMStreamPrioritizationRecord;
  _WMStreamPrioritizationRecord = packed record
    wStreamNumber: Word;
    fMandatory   : BOOL;
  end;
  {$EXTERNALSYM _WMStreamPrioritizationRecord}
  WM_STREAM_PRIORITY_RECORD = _WMStreamPrioritizationRecord;
  {$EXTERNALSYM WM_STREAM_PRIORITY_RECORD}
  TWMStreamPrioritizationRecord = _WMStreamPrioritizationRecord;

//
// Writer Statistics struct
//
  PWMWriterStatistics = ^TWMWriterStatistics;
  _WMWriterStatistics = packed record
    qwSampleCount        : Int64;
    qwByteCount          : Int64;

    qwDroppedSampleCount : Int64;
    qwDroppedByteCount   : Int64;

    dwCurrentBitrate     : LongWord;
    dwAverageBitrate     : LongWord;
    dwExpectedBitrate    : LongWord;

    //
    // Sample rates are given as 1000 * (samples / second).
    //
    dwCurrentSampleRate  : LongWord;
    dwAverageSampleRate  : LongWord;
    dwExpectedSampleRate : LongWord;
  end;
  {$EXTERNALSYM _WMWriterStatistics}
  WM_WRITER_STATISTICS = _WMWriterStatistics;
  {$EXTERNALSYM WM_WRITER_STATISTICS}
  TWMWriterStatistics = _WMWriterStatistics;

//
// Extended Writer Statistics
//
  PWMWriterStatisticsEx = ^TWMWriterStatisticsEx;
  _WMWriterStatisticsEx = packed record
    //
    // Bitrate that includes overhead
    //
    dwBitratePlusOverhead : LongWord;

    //
    // Sample rates are given as 1000 * (samples / second)
    //
    dwCurrentSampleDropRateInQueue       : LongWord;
    dwCurrentSampleDropRateInCodec       : LongWord;
    dwCurrentSampleDropRateInMultiplexer : LongWord;

    //
    // Cumulative sample drops
    //
    dwTotalSampleDropsInQueue       : LongWord;
    dwTotalSampleDropsInCodec       : LongWord;
    dwTotalSampleDropsInMultiplexer : LongWord;
  end;
  {$EXTERNALSYM _WMWriterStatisticsEx}
  WM_WRITER_STATISTICS_EX = _WMWriterStatisticsEx;
  {$EXTERNALSYM WM_WRITER_STATISTICS_EX}
  TWMWriterStatisticsEx = _WMWriterStatisticsEx;

//
// Reader Statistics struct
//
  PWMReaderStatistics = ^TWMReaderStatistics;
  _WMReaderStatistics = packed record
    cbSize            : LongWord;
    dwBandwidth       : LongWord;
    cPacketsReceived  : LongWord;
    cPacketsRecovered : LongWord;
    cPacketsLost      : LongWord;
    wQuality          : Word;
  end;
  {$EXTERNALSYM _WMReaderStatistics}
  WM_READER_STATISTICS = _WMReaderStatistics;
  {$EXTERNALSYM WM_READER_STATISTICS}
  TWMReaderStatistics = _WMReaderStatistics;

//
// Reader Client Info struct
//
  PWMReaderClientInfo = ^TWMReaderClientInfo;
  _WMReaderClientInfo = packed record
    cbSize              : LongWord;
    wszLang             : PWideChar; // 2-3 letter language code
    wszBrowserUserAgent : PWideChar; // Embedded browser's user-agent string
    wszBrowserWebPage   : PWideChar; // The web page that contains the plugin
    qwReserved          : Int64;     // Reserved
    pReserved           : PLongInt;  // Reserved
    wszHostExe          : PWideChar; // iexplore.exe, netscape.exe dshow.exe, etc
    qwHostVersion       : Int64;     // Version of the host application e.g.:4.70.12.15
    wszPlayerUserAgent  : PWideChar; // String identifying the player application, e.g. WMPlayer/9.0.0.0
  end;
  {$EXTERNALSYM _WMReaderClientInfo}
  WM_READER_CLIENTINFO = _WMReaderClientInfo;
  {$EXTERNALSYM WM_READER_CLIENTINFO}
  TWMReaderClientInfo = _WMReaderClientInfo;


//
// This structure is returned with the WMT_CLIENT_CONNECT and
// WMT_CLIENT_DISCONNECT status notifications.  It is also used
// by IWMClientConnections.
//
  PWMClientProperties = ^TWMClientProperties;
  _WMClientProperties = packed record
    dwIPAddress : LongWord;
    dwPort      : LongWord;
  end;
  {$EXTERNALSYM _WMClientProperties}
  WM_CLIENT_PROPERTIES = _WMClientProperties;
  {$EXTERNALSYM WM_CLIENT_PROPERTIES}
  TWMClientProperties = _WMClientProperties;

//
// This structure is returned with the WMT_CLIENT_CONNECT_EX, WMT_CLIENT_DISCONNECT_EX
// and WMT_CLIENT_PROPERTIES status notifications.
//
  PWMClientPropertiesEx = ^TWMClientPropertiesEx;
  _WMClientPropertiesEx = packed record
    cbSize        : LongWord;     // size of structure
    pwszIPAddress : PWideChar; // IP address in dot notation
    pwszPort      : PWideChar; // Client's port number
    pwszDNSName   : PWideChar; // DNS name of client, if known
  end;
  {$EXTERNALSYM _WMClientPropertiesEx}
  WM_CLIENT_PROPERTIES_EX = _WMClientPropertiesEx;
  {$EXTERNALSYM WM_CLIENT_PROPERTIES_EX}
  TWMClientPropertiesEx = _WMClientPropertiesEx;

//
// Inclusive port number range.
// Used by IWMReaderNetworkConfig.
//
  PWMPortNumberRange = ^TWMPortNumberRange;
  _WMPortNumberRange = packed record
    wPortBegin : Word;
    wPortEnd   : Word;
  end;
  {$EXTERNALSYM _WMPortNumberRange}
  WM_PORT_NUMBER_RANGE = _WMPortNumberRange;
  {$EXTERNALSYM WM_PORT_NUMBER_RANGE}
  TWMPortNumberRange = _WMPortNumberRange;

//
// For passing data units to the file sink
//
  PWMTBufferSegment = ^TWMTBufferSegment;
  _WMT_BUFFER_SEGMENT = packed record
    pBuffer  : INSSBuffer;
    cbOffset : LongWord;
    cbLength : LongWord;
  end;
  {$EXTERNALSYM _WMT_BUFFER_SEGMENT}
  WMT_BUFFER_SEGMENT = _WMT_BUFFER_SEGMENT;
  {$EXTERNALSYM WMT_BUFFER_SEGMENT}
  TWMTBufferSegment = _WMT_BUFFER_SEGMENT;

  PWMTPayloadFragment = ^TWMTPayloadFragment;
  _WMT_PAYLOAD_FRAGMENT = packed record
    dwPayloadIndex : LongWord;
    segmentData    : TWMTBufferSegment;
  end;
  {$EXTERNALSYM _WMT_PAYLOAD_FRAGMENT}
  WMT_PAYLOAD_FRAGMENT = _WMT_PAYLOAD_FRAGMENT;
  {$EXTERNALSYM WMT_PAYLOAD_FRAGMENT}
  TWMTPayloadFragment = _WMT_PAYLOAD_FRAGMENT;

  PWMTFileSinkDataUnit = ^TWMTFileSinkDataUnit;
  _WMT_FILESINK_DATA_UNIT = packed record
    packetHeaderBuffer    : TWMTBufferSegment;

    cPayloads             : LongWord;
    pPayloadHeaderBuffers : PWMTBufferSegment;

    cPayloadDataFragments : LongWord;
    pPayloadDataFragments : PWMTPayloadFragment;
  end;
  {$EXTERNALSYM _WMT_FILESINK_DATA_UNIT}
  WMT_FILESINK_DATA_UNIT = _WMT_FILESINK_DATA_UNIT;
  {$EXTERNALSYM WMT_FILESINK_DATA_UNIT}
  TWMTFileSinkDataUnit = _WMT_FILESINK_DATA_UNIT;

  PWMTWebStreamFormat = ^TWMTWebStreamFormat;
  _WMT_WEBSTREAM_FORMAT = packed record
   cbSize                  : Word; // Set to sizeof( WMT_WEBSTREAM_FORMAT )
   cbSampleHeaderFixedData : Word; // Length of the fixed part of the sample header should be
                                   // set to sizeof( WMT_WEBSTREAM_SAMPLE_HEADER )
   wVersion                : Word;
   wReserved               : Word; // Reserved. Should be set to 0.
  end;
  {$EXTERNALSYM _WMT_WEBSTREAM_FORMAT}
  WMT_WEBSTREAM_FORMAT = _WMT_WEBSTREAM_FORMAT;
  {$EXTERNALSYM WMT_WEBSTREAM_FORMAT}
  TWMTWebStreamFormat = WMT_WEBSTREAM_FORMAT;

  PWMTWebStreamSampleHeader = ^TWMTWebStreamSampleHeader;
  _WMT_WEBSTREAM_SAMPLE_HEADER = packed record
    cbLength    : Word;                 // Length of this header including string data
                                        // should be set to sizeof( WMT_WEBSTREAM_SAMPLE_HEADER ) +
                                        // wcslen( wszURL ) * sizeof( WCHAR )
    wPart       : Word;                 // Current part. From 0 to cTotalParts - 1
    cTotalParts : Word;                 // Total number of parts for this file must be at least 1.
    wSampleType : Word;                 // Sample type. Should be set to
                                        //     WEBSTREAM_SAMPLE_TYPE_FILE = 0x0001 or
                                        //     WEBSTREAM_SAMPLE_TYPE_RENDER = 0x0002
    wszURL      : array[0..0] of WCHAR; // Variable length string data containing file url
  end;
  {$EXTERNALSYM _WMT_WEBSTREAM_SAMPLE_HEADER}
  WMT_WEBSTREAM_SAMPLE_HEADER = _WMT_WEBSTREAM_SAMPLE_HEADER;
  {$EXTERNALSYM WMT_WEBSTREAM_SAMPLE_HEADER}
  TWMTWebStreamSampleHeader = _WMT_WEBSTREAM_SAMPLE_HEADER;

  PWMAddressAccessEntry = ^TWMAddressAccessEntry;
  _WMAddressAccessEntry = packed record
    dwIPAddress : LongWord;
    dwMask      : LongWord;
  end;
  {$EXTERNALSYM _WMAddressAccessEntry}
  WM_ADDRESS_ACCESSENTRY = _WMAddressAccessEntry;
  {$EXTERNALSYM WM_ADDRESS_ACCESSENTRY}
  TWMAddressAccessEntry = _WMAddressAccessEntry;

///////////////////////////////////////////////////////////////////////////////
//
// Structures for complex metadata attributes
//
///////////////////////////////////////////////////////////////////////////////

  PWMPicture = ^TWMPicture;
  _WMPicture = packed record
    pwszMIMEType    : PWideChar;
    bPictureType    : BYTE;
    pwszDescription : PWideChar;
    dwDataLen       : LongWord;
    pbData          : PBYTE;
  end;
  {$EXTERNALSYM _WMPicture}
  WM_PICTURE = _WMPicture;
  {$EXTERNALSYM WM_PICTURE}
  TWMPicture = _WMPicture;

  PWMSynchronisedLyrics = ^TWMSynchronisedLyrics;
  _WMSynchronisedLyrics = packed record
    bTimeStampFormat      : BYTE;
    bContentType          : BYTE;
    pwszContentDescriptor : PWideChar;
    dwLyricsLen           : LongWord;
    pbLyrics              : PBYTE;
  end;
  {$EXTERNALSYM _WMSynchronisedLyrics}
  WM_SYNCHRONISED_LYRICS = _WMSynchronisedLyrics;
  {$EXTERNALSYM WM_SYNCHRONISED_LYRICS}
  TWMSynchronisedLyrics = _WMSynchronisedLyrics;

  PWMUserWebURL = ^TWMUserWebURL;
  _WMUserWebURL = packed record
    pwszDescription : PWideChar;
    pwszURL         : PWideChar;
  end;
  {$EXTERNALSYM _WMUserWebURL}
  WM_USER_WEB_URL = _WMUserWebURL;
  {$EXTERNALSYM WM_USER_WEB_URL}
  TWMUserWebURL = _WMUserWebURL;

  PWMUserText = ^TWMUserText;
  _WMUserText = packed record
    pwszDescription : PWideChar;
    pwszText        : PWideChar;
  end;
  {$EXTERNALSYM _WMUserText}
  WM_USER_TEXT = _WMUserText;
  {$EXTERNALSYM WM_USER_TEXT}
  TWMUserText = _WMUserText;

  PWMLeakyBucketPair = ^TWMLeakyBucketPair;
  _WMLeakyBucketPair = packed record
    dwBitrate      : LongWord;
    msBufferWindow : LongWord;
  end;
  {$EXTERNALSYM _WMLeakyBucketPair}
  WM_LEAKY_BUCKET_PAIR = _WMLeakyBucketPair;
  {$EXTERNALSYM WM_LEAKY_BUCKET_PAIR}
  TWMLeakyBucketPair = _WMLeakyBucketPair;

///////////////////////////////////////////////////////////////////////////////
//
// Structure needed for using GetDRMProperty.
//
///////////////////////////////////////////////////////////////////////////////

  PWMLicenseStateData = ^TWMLicenseStateData;
  _WM_LICENSE_STATE_DATA = packed record
     dwSize      : LongWord; // Size of the entire structure.
     dwNumStates : LongWord; // Number of state data passed back in array of structure below
     stateData   : array[0..0] of TDRMLicenseStateData;
  end;
  {$EXTERNALSYM _WM_LICENSE_STATE_DATA}
  WM_LICENSE_STATE_DATA = _WM_LICENSE_STATE_DATA;
  {$EXTERNALSYM WM_LICENSE_STATE_DATA}
  TWMLicenseStateData = _WM_LICENSE_STATE_DATA;

///////////////////////////////////////////////////////////////////////////////
// Watermark entry
///////////////////////////////////////////////////////////////////////////////

  PWMTWatermarkEntry = ^TWMTWatermarkEntry;
  __WMT_WATERMARK_ENTRY = packed record
     wmetType        : TWMTWatermarkEntryType;
     clsid           : TGUID;
     cbDisplayName   : UINT;
     pwszDisplayName : PWideChar;
  end;
  {$EXTERNALSYM __WMT_WATERMARK_ENTRY}
  WMT_WATERMARK_ENTRY = __WMT_WATERMARK_ENTRY;
  {$EXTERNALSYM WMT_WATERMARK_ENTRY}
  TWMTWatermarkEntry = __WMT_WATERMARK_ENTRY;

///////////////////////////////////////////////////////////////////////////////
// VideoImage structs
///////////////////////////////////////////////////////////////////////////////

//
// dwControlFlags
//
const
  WMT_VIDEOIMAGE_SAMPLE_INPUT_FRAME               = 1; // sample has input frame
  {$EXTERNALSYM WMT_VIDEOIMAGE_SAMPLE_INPUT_FRAME}
  WMT_VIDEOIMAGE_SAMPLE_OUTPUT_FRAME              = 2; // sample produces output frame
  {$EXTERNALSYM WMT_VIDEOIMAGE_SAMPLE_OUTPUT_FRAME}
  WMT_VIDEOIMAGE_SAMPLE_USES_CURRENT_INPUT_FRAME  = 4;
  {$EXTERNALSYM WMT_VIDEOIMAGE_SAMPLE_USES_CURRENT_INPUT_FRAME}
  WMT_VIDEOIMAGE_SAMPLE_USES_PREVIOUS_INPUT_FRAME = 8;
  {$EXTERNALSYM WMT_VIDEOIMAGE_SAMPLE_USES_PREVIOUS_INPUT_FRAME}

//
// dwInputFlags
//
  WMT_VIDEOIMAGE_SAMPLE_MOTION       = 1; // acef used (includes resizing)
  {$EXTERNALSYM WMT_VIDEOIMAGE_SAMPLE_MOTION}
  WMT_VIDEOIMAGE_SAMPLE_ROTATION     = 2; // bd also used (not valid without acef)
  {$EXTERNALSYM WMT_VIDEOIMAGE_SAMPLE_ROTATION}
  WMT_VIDEOIMAGE_SAMPLE_BLENDING     = 4; // BlendCoef1 used
  {$EXTERNALSYM WMT_VIDEOIMAGE_SAMPLE_BLENDING}
  WMT_VIDEOIMAGE_SAMPLE_ADV_BLENDING = 8; // BlendCoef2 also used (not valid without BlendCoef1)
  {$EXTERNALSYM WMT_VIDEOIMAGE_SAMPLE_ADV_BLENDING}

  WMT_VIDEOIMAGE_INTEGER_DENOMINATOR = 65536;
  {$EXTERNALSYM WMT_VIDEOIMAGE_INTEGER_DENOMINATOR}

  WMT_VIDEOIMAGE_MAGIC_NUMBER = $1d4a45f2;
  {$EXTERNALSYM WMT_VIDEOIMAGE_MAGIC_NUMBER}

type
  PWMTVideoImageSample = ^TWMTVideoImageSample;
  __WMT_VIDEOIMAGE_SAMPLE = packed record
    dwMagic        : LongWord;
    cbStruct       : ULONG; // size of structure; incudes dwMagic and cbStruct
    dwControlFlags : LongWord;

    //
    // most recent input frame
    //
    dwInputFlagsCur   : LongWord;
    lCurMotionXtoX    : LongInt; // a
    lCurMotionYtoX    : LongInt; // b
    lCurMotionXoffset : LongInt; // c
    lCurMotionXtoY    : LongInt; // d
    lCurMotionYtoY    : LongInt; // e
    lCurMotionYoffset : LongInt; // f
    lCurBlendCoef1    : LongInt;
    lCurBlendCoef2    : LongInt;

    //
    // second most recent input frame
    //
    dwInputFlagsPrev   : LongWord;
    lPrevMotionXtoX    : LongInt; // a
    lPrevMotionYtoX    : LongInt; // b
    lPrevMotionXoffset : LongInt; // c
    lPrevMotionXtoY    : LongInt; // d
    lPrevMotionYtoY    : LongInt; // e
    lPrevMotionYoffset : LongInt; // f
    lPrevBlendCoef1    : LongInt;
    lPrevBlendCoef2    : LongInt;
  end;
  {$EXTERNALSYM __WMT_VIDEOIMAGE_SAMPLE}
  WMT_VIDEOIMAGE_SAMPLE = __WMT_VIDEOIMAGE_SAMPLE;
  {$EXTERNALSYM WMT_VIDEOIMAGE_SAMPLE}
  TWMTVideoImageSample = __WMT_VIDEOIMAGE_SAMPLE;

///////////////////////////////////////////////////////////////////////////////
//
// Media-type structures and GUIDs.
//
///////////////////////////////////////////////////////////////////////////////

//
// We use DirectShow media types in this SDK. However, to avoid conflict with
// their names, we define our own version of the structure. This is exactly
// the same as an AM_MEDIA_TYPE!
//
  PWMMediaType = ^TWMMediaType;
  _WMMediaType = packed record
    majortype            : TGUID;
    subtype              : TGUID;
    bFixedSizeSamples    : BOOL;
    bTemporalCompression : BOOL;
    lSampleSize          : ULONG;
    formattype           : TGUID;
    pUnk                 : IUnknown;
    cbFormat             : ULONG;
    pbFormat             : PBYTE; // size_is(cbFormat)
  end;
  {$EXTERNALSYM _WMMediaType}
  WM_MEDIA_TYPE = _WMMediaType;
  {$EXTERNALSYM WM_MEDIA_TYPE}
  TWMMediaType = _WMMediaType;

  PWMVideoInfoHeader = ^TWMVideoInfoHeader;
  tagWMVIDEOINFOHEADER = packed record
    rcSource        : TRECT;    // The bit we really want to use.
    rcTarget        : TRECT;    // Where the video should go.
    dwBitRate       : LongWord; // Approximate bit data rate.
    dwBitErrorRate  : LongWord; // Bit error rate for this stream.
    AvgTimePerFrame : Int64;    // Average time per frame (100ns units).
    bmiHeader       : TBitmapInfoHeader;
  end;
  {$EXTERNALSYM tagWMVIDEOINFOHEADER}
  WMVIDEOINFOHEADER = tagWMVIDEOINFOHEADER;
  {$EXTERNALSYM WMVIDEOINFOHEADER}
  TWMVideoInfoHeader = tagWMVIDEOINFOHEADER;

  PWMVideoInfoHeader2 = ^TWMVideoInfoHeader2;
  tagWMVIDEOINFOHEADER2 = packed record
    rcSource           : TRECT;     // The bit we really want to use.
    rcTarget           : TRECT;     // Where the video should go.
    dwBitRate          : LongWord;  // Approximate bit data rate.
    dwBitErrorRate     : LongWord;  // Bit error rate for this stream.
    AvgTimePerFrame    : Int64;     // Average time per frame (100ns units).
    dwInterlaceFlags   : LongWord;  // Use AMINTERLACE_* defines. Reject connection if undefined bits are not 0.
    dwCopyProtectFlags : LongWord;  // use AMCOPYPROTECT_* defines. Reject connection if undefined bits are not 0.
    dwPictAspectRatioX : LongWord;  // X dimension of picture aspect ratio, e.g. 16 for 16x9 display.
    dwPictAspectRatioY : LongWord;  // Y dimension of picture aspect ratio, e.g.  9 for 16x9 display.
    dwReserved1        : LongWord;  // Must be 0; reject connection otherwise.
    dwReserved2        : LongWord;  // Must be 0; reject connection otherwise.
    bmiHeader          : TBitmapInfoHeader;
  end;
  {$EXTERNALSYM tagWMVIDEOINFOHEADER2}
  WMVIDEOINFOHEADER2 = tagWMVIDEOINFOHEADER2;
  {$EXTERNALSYM WMVIDEOINFOHEADER2}
  TWMVideoInfoHeader2 = tagWMVIDEOINFOHEADER2;

  PWMMPEG2VideoInfo = ^TWMMPEG2VideoInfo;
  tagWMMPEG2VIDEOINFO = packed record
    hdr              : TWMVideoInfoHeader2;     // Video info header2.
    dwStartTimeCode  : LongWord;                // Not used for DVD.
    cbSequenceHeader : LongWord;                // Is 0 for DVD (no sequence header).
    dwProfile        : LongWord;                // Use enum MPEG2Profile.
    dwLevel          : LongWord;                // Use enum MPEG2Level.
    dwFlags          : LongWord;                // Use AMMPEG2_* defines.  Reject connection if undefined bits are not 0.
    dwSequenceHeader : array[0..0] of LongWord; // Sequence header.
  end;
  {$EXTERNALSYM tagWMMPEG2VIDEOINFO}
  WMMPEG2VIDEOINFO = tagWMMPEG2VIDEOINFO;
  {$EXTERNALSYM WMMPEG2VIDEOINFO}
  TWMMPEG2VideoInfo = tagWMMPEG2VIDEOINFO;

  PWMScriptFormat = ^TWMScriptFormat;
  tagWMSCRIPTFORMAT = packed record
    scriptType: TGUID;
  end;
  {$EXTERNALSYM tagWMSCRIPTFORMAT}
  WMSCRIPTFORMAT = tagWMSCRIPTFORMAT;
  {$EXTERNALSYM WMSCRIPTFORMAT}
  TWMScriptFormat = tagWMSCRIPTFORMAT;

//
// This special GUID is used to create a subtype from an audio format tag, or
// video four-character code. Just fill in the first LongWord of the GUID with
// the appropriate value.
//
const
  WMMEDIASUBTYPE_Base             : TGUID = '{00000000-0000-0010-8000-00AA00389B71}';
  {$EXTERNALSYM WMMEDIASUBTYPE_Base}
  WMMEDIATYPE_Video               : TGUID = '{73646976-0000-0010-8000-00AA00389B71}'; // 'vids'
  {$EXTERNALSYM WMMEDIATYPE_Video}
  WMMEDIASUBTYPE_RGB1             : TGUID = '{e436eb78-524f-11ce-9f53-0020af0ba770}';
  {$EXTERNALSYM WMMEDIASUBTYPE_RGB1}
  WMMEDIASUBTYPE_RGB4             : TGUID = '{e436eb79-524f-11ce-9f53-0020af0ba770}';
  {$EXTERNALSYM WMMEDIASUBTYPE_RGB4}
  WMMEDIASUBTYPE_RGB8             : TGUID = '{e436eb7a-524f-11ce-9f53-0020af0ba770}';
  {$EXTERNALSYM WMMEDIASUBTYPE_RGB8}
  WMMEDIASUBTYPE_RGB565           : TGUID = '{e436eb7b-524f-11ce-9f53-0020af0ba770}';
  {$EXTERNALSYM WMMEDIASUBTYPE_RGB565}
  WMMEDIASUBTYPE_RGB555           : TGUID = '{e436eb7c-524f-11ce-9f53-0020af0ba770}';
  {$EXTERNALSYM WMMEDIASUBTYPE_RGB555}
  WMMEDIASUBTYPE_RGB24            : TGUID = '{e436eb7d-524f-11ce-9f53-0020af0ba770}';
  {$EXTERNALSYM WMMEDIASUBTYPE_RGB24}
  WMMEDIASUBTYPE_RGB32            : TGUID = '{e436eb7e-524f-11ce-9f53-0020af0ba770}';
  {$EXTERNALSYM WMMEDIASUBTYPE_RGB32}
  WMMEDIASUBTYPE_I420             : TGUID = '{30323449-0000-0010-8000-00AA00389B71}'; // 'YV12'
  {$EXTERNALSYM WMMEDIASUBTYPE_I420}
  WMMEDIASUBTYPE_IYUV             : TGUID = '{56555949-0000-0010-8000-00AA00389B71}'; // 'YV12'
  {$EXTERNALSYM WMMEDIASUBTYPE_IYUV}
  WMMEDIASUBTYPE_YV12             : TGUID = '{31313259-0000-0010-8000-00AA00389B71}'; // 'YV12'
  {$EXTERNALSYM WMMEDIASUBTYPE_YV12}
  WMMEDIASUBTYPE_YUY2             : TGUID = '{32595559-0000-0010-8000-00AA00389B71}'; // 'YUY2'
  {$EXTERNALSYM WMMEDIASUBTYPE_YUY2}
  WMMEDIASUBTYPE_UYVY             : TGUID = '{59565955-0000-0010-8000-00AA00389B71}'; // 'UYVY'
  {$EXTERNALSYM WMMEDIASUBTYPE_UYVY}
  WMMEDIASUBTYPE_YVYU             : TGUID = '{55595659-0000-0010-8000-00AA00389B71}'; // 'YVYU'
  {$EXTERNALSYM WMMEDIASUBTYPE_YVYU}
  WMMEDIASUBTYPE_YVU9             : TGUID = '{39555659-0000-0010-8000-00AA00389B71}'; // 'YVU9'
  {$EXTERNALSYM WMMEDIASUBTYPE_YVU9}
  WMMEDIASUBTYPE_VIDEOIMAGE       : TGUID = '{1d4a45f2-e5f6-4b44-8388-f0ae5c0e0c37}';
  {$EXTERNALSYM WMMEDIASUBTYPE_VIDEOIMAGE}
  WMMEDIASUBTYPE_MP43             : TGUID = '{3334504D-0000-0010-8000-00AA00389B71}';
  {$EXTERNALSYM WMMEDIASUBTYPE_MP43}
  WMMEDIASUBTYPE_MP4S             : TGUID = '{5334504D-0000-0010-8000-00AA00389B71}';
  {$EXTERNALSYM WMMEDIASUBTYPE_MP4S}
  WMMEDIASUBTYPE_WMV1             : TGUID = '{31564D57-0000-0010-8000-00AA00389B71}';
  {$EXTERNALSYM WMMEDIASUBTYPE_WMV1}
  WMMEDIASUBTYPE_WMV2             : TGUID = '{32564D57-0000-0010-8000-00AA00389B71}';
  {$EXTERNALSYM WMMEDIASUBTYPE_WMV2}
  WMMEDIASUBTYPE_MSS1             : TGUID = '{3153534D-0000-0010-8000-00AA00389B71}';
  {$EXTERNALSYM WMMEDIASUBTYPE_MSS1}
  WMMEDIASUBTYPE_MPEG2_VIDEO      : TGUID = '{e06d8026-db46-11cf-b4d1-00805f6cbbea}';
  {$EXTERNALSYM WMMEDIASUBTYPE_MPEG2_VIDEO}
  WMMEDIATYPE_Audio               : TGUID = '{73647561-0000-0010-8000-00AA00389B71}'; // 'auds'
  {$EXTERNALSYM WMMEDIATYPE_Audio}
  WMMEDIASUBTYPE_PCM              : TGUID = '{00000001-0000-0010-8000-00AA00389B71}';
  {$EXTERNALSYM WMMEDIASUBTYPE_PCM}
  WMMEDIASUBTYPE_DRM              : TGUID = '{00000009-0000-0010-8000-00AA00389B71}';
  {$EXTERNALSYM WMMEDIASUBTYPE_DRM}
  WMMEDIASUBTYPE_WMAudioV9        : TGUID = '{00000162-0000-0010-8000-00AA00389B71}';
  {$EXTERNALSYM WMMEDIASUBTYPE_WMAudioV9}
  WMMEDIASUBTYPE_WMAudio_Lossless : TGUID = '{00000163-0000-0010-8000-00AA00389B71}';
  {$EXTERNALSYM WMMEDIASUBTYPE_WMAudio_Lossless}
  WMMEDIASUBTYPE_MSS2             : TGUID = '{3253534D-0000-0010-8000-00AA00389B71}';
  {$EXTERNALSYM WMMEDIASUBTYPE_MSS2}
  WMMEDIASUBTYPE_WMSP1            : TGUID = '{0000000A-0000-0010-8000-00AA00389B71}';
  {$EXTERNALSYM WMMEDIASUBTYPE_WMSP1}
  WMMEDIASUBTYPE_WMV3             : TGUID = '{33564D57-0000-0010-8000-00AA00389B71}';
  {$EXTERNALSYM WMMEDIASUBTYPE_WMV3}
  WMMEDIASUBTYPE_WMVP             : TGUID = '{50564D57-0000-0010-8000-00AA00389B71}';

  {$EXTERNALSYM WMMEDIASUBTYPE_WMVP}

//
// WM Audio v2 and v7 are actually compatible bitstreams.
//
  WMMEDIASUBTYPE_WMAudioV8 : TGUID = '{00000161-0000-0010-8000-00AA00389B71}';
  {$EXTERNALSYM WMMEDIASUBTYPE_WMAudioV8}
  WMMEDIASUBTYPE_WMAudioV7 : TGUID = '{00000161-0000-0010-8000-00AA00389B71}';
  {$EXTERNALSYM WMMEDIASUBTYPE_WMAudioV7}
  WMMEDIASUBTYPE_WMAudioV2 : TGUID = '{00000161-0000-0010-8000-00AA00389B71}';
  {$EXTERNALSYM WMMEDIASUBTYPE_WMAudioV2}
  WMMEDIASUBTYPE_ACELPnet  : TGUID = '{00000130-0000-0010-8000-00AA00389B71}';
  {$EXTERNALSYM WMMEDIASUBTYPE_ACELPnet}
  WMMEDIASUBTYPE_MP3       : TGUID = '{00000050-0000-0010-8000-00AA00389B71}';
  {$EXTERNALSYM WMMEDIASUBTYPE_MP3}
  WMMEDIASUBTYPE_WebStream : TGUID = '{776257d4-c627-41cb-8f81-7ac7ff1c40cc}';
  {$EXTERNALSYM WMMEDIASUBTYPE_WebStream}
  WMMEDIATYPE_Script       : TGUID = '{73636d64-0000-0010-8000-00AA00389B71}'; // 'scmd'
  {$EXTERNALSYM WMMEDIATYPE_Script}
  WMMEDIATYPE_Image        : TGUID = '{34A50FD8-8AA5-4386-81FE-A0EFE0488E31}'; // 'imag'
  {$EXTERNALSYM WMMEDIATYPE_Image}
  WMMEDIATYPE_FileTransfer : TGUID = '{D9E47579-930E-4427-ADFC-AD80F290E470}'; // 'fxfr'
  {$EXTERNALSYM WMMEDIATYPE_FileTransfer}
  WMMEDIATYPE_Text         : TGUID = '{9BBA1EA7-5AB2-4829-BA57-0940209BCF3E}'; // 'text'
  {$EXTERNALSYM WMMEDIATYPE_Text}
  WMFORMAT_VideoInfo       : TGUID = '{05589f80-c356-11ce-bf01-00aa0055595a}';
  {$EXTERNALSYM WMFORMAT_VideoInfo}
  WMFORMAT_MPEG2Video      : TGUID = '{e06d80e3-db46-11cf-b4d1-00805f6cbbea}';
  {$EXTERNALSYM WMFORMAT_MPEG2Video}
  WMFORMAT_WaveFormatEx    : TGUID = '{05589f81-c356-11ce-bf01-00aa0055595a}';
  {$EXTERNALSYM WMFORMAT_WaveFormatEx}
  WMFORMAT_Script          : TGUID = '{5C8510F2-DEBE-4ca7-BBA5-F07A104F8DFF}';
  {$EXTERNALSYM WMFORMAT_Script}
  WMFORMAT_WebStream       : TGUID = '{da1e6b13-8359-4050-b398-388e965bf00c}';
  {$EXTERNALSYM WMFORMAT_WebStream}
  WMSCRIPTTYPE_TwoStrings  : TGUID = '{82f38a70-c29f-11d1-97ad-00a0c95ea850}';
  {$EXTERNALSYM WMSCRIPTTYPE_TwoStrings}

//////////////////////////////////////////////////////////////////////////////
//
// GUIDs for common buffer properties
//
  WM_SampleExtensionGUID_OutputCleanPoint : TGUID = '{f72a3c6f-6eb4-4ebc-b192-09ad9759e828}';
  {$EXTERNALSYM WM_SampleExtensionGUID_OutputCleanPoint}
  WM_SampleExtensionGUID_Timecode         : TGUID = '{399595ec-8667-4e2d-8fdb-98814ce76c1e}';
  {$EXTERNALSYM WM_SampleExtensionGUID_Timecode}


//
// This buffer property is used in conjunction with file transfer streams.
// It is the name of the file being transfered (in WCHARs).
//
  WM_SampleExtensionGUID_FileName          : TGUID = '{e165ec0e-19ed-45d7-b4a7-25cbd1e28e9b}';
  {$EXTERNALSYM WM_SampleExtensionGUID_FileName}

  WM_SampleExtensionGUID_ContentType       : TGUID = '{d590dc20-07bc-436c-9cf7-f3bbfbf1a4dc}';
  {$EXTERNALSYM WM_SampleExtensionGUID_ContentType}
  WM_SampleExtensionGUID_PixelAspectRatio  : TGUID = '{1b1ee554-f9ea-4bc8-821a-376b74e4c4b8}';
  {$EXTERNALSYM WM_SampleExtensionGUID_PixelAspectRatio}
  WM_SampleExtensionGUID_SampleDuration    : TGUID = '{c6bd9450-867f-4907-83a3-c77921b733ad}';
  {$EXTERNALSYM WM_SampleExtensionGUID_SampleDuration}

//
// Constants for use with the corresponding sample extension properties
//
  WM_SampleExtension_ContentType_Size      = 1;
  {$EXTERNALSYM WM_SampleExtension_ContentType_Size}
  WM_SampleExtension_PixelAspectRatio_Size = 2;
  {$EXTERNALSYM WM_SampleExtension_PixelAspectRatio_Size}
  WM_SampleExtension_Timecode_Size         = 14;
  {$EXTERNALSYM WM_SampleExtension_Timecode_Size}
  WM_SampleExtension_SampleDuration_Size   = 2;
  {$EXTERNALSYM WM_SampleExtension_SampleDuration_Size}

///////////////////////////////////////////////////////////////////////////////
//
// Content Types to be used with WM_SampleExtensionGUID_ContentType
// on INSSBuffer3::SetProperty/GetProperty
//
///////////////////////////////////////////////////////////////////////////////

  WM_CT_INTERLACED         = 128;
  {$EXTERNALSYM WM_CT_INTERLACED}
  WM_CT_BOTTOM_FIELD_FIRST = 32;
  {$EXTERNALSYM WM_CT_BOTTOM_FIELD_FIRST}
  WM_CT_TOP_FIELD_FIRST    = 64;
  {$EXTERNALSYM WM_CT_TOP_FIELD_FIRST}

///////////////////////////////////////////////////////////////////////////////
//
// Timecode structures to be used with WM_SampleExtensionGUID_Timecode
// on INSSBuffer3::SetProperty/GetProperty.  The timecode LongWord
// contains the following timecode representation:
//
// BYTE  MSB             LSB
// ----------------------------------------------
// 1     Tens of hour    Hour
// 2     Tens of minute  Minute
// 3     Tens of second  Second
// 4     Tens of frame   Frame
//
///////////////////////////////////////////////////////////////////////////////

type
  PWMTTimeCodeExtensionData = ^TWMTTimeCodeExtensionData;
  _WMT_TIMECODE_EXTENSION_DATA = packed record
    wRange     : Word;
    dwTimecode : LongWord;
    dwUserbits : LongWord;
    dwAmFlags  : LongWord;
  end;
  {$EXTERNALSYM _WMT_TIMECODE_EXTENSION_DATA}
  WMT_TIMECODE_EXTENSION_DATA = _WMT_TIMECODE_EXTENSION_DATA;
  {$EXTERNALSYM WMT_TIMECODE_EXTENSION_DATA}
  TWMTTimeCodeExtensionData = _WMT_TIMECODE_EXTENSION_DATA;

///////////////////////////////////////////////////////////////////////////////
//
// IID GUIDs defined here.
//
///////////////////////////////////////////////////////////////////////////////
const
  IID_IWMMediaProps             : TGUID = '{96406bce-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMMediaProps}
  IID_IWMVideoMediaProps        : TGUID = '{96406bcf-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMVideoMediaProps}
  IID_IWMWriter                 : TGUID = '{96406bd4-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMWriter}
  IID_IWMInputMediaProps        : TGUID = '{96406bd5-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMInputMediaProps}
  IID_IWMReader                 : TGUID = '{96406bd6-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMReader}
  IID_IWMSyncReader             : TGUID = '{9397f121-7705-4dc9-b049-98b698188414}';
  {$EXTERNALSYM IID_IWMSyncReader}
  IID_IWMSyncReader2            : TGUID = '{faed3d21-1b6b-4af7-8cb6-3e189bbc187b}';
  {$EXTERNALSYM IID_IWMSyncReader2}
  IID_IWMOutputMediaProps       : TGUID = '{96406bd7-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMOutputMediaProps}
  IID_IWMStatusCallback         : TGUID = '{6d7cdc70-9888-11d3-8edc-00c04f6109cf}';
  {$EXTERNALSYM IID_IWMStatusCallback}
  IID_IWMReaderCallback         : TGUID = '{96406bd8-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMReaderCallback}
  IID_IWMCredentialCallback     : TGUID = '{342e0eb7-e651-450c-975b-2ace2c90c48e}';
  {$EXTERNALSYM IID_IWMCredentialCallback}
  IID_IWMMetadataEditor         : TGUID = '{96406bd9-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMMetadataEditor}
  IID_IWMMetadataEditor2        : TGUID = '{203cffe3-2e18-4fdf-b59d-6e71530534cf}';
  {$EXTERNALSYM IID_IWMMetadataEditor2}
  IID_IWMDRMEditor              : TGUID = '{FF130EBC-A6C3-42A6-B401-C3382C3E08B3}';
  {$EXTERNALSYM IID_IWMDRMEditor}
  IID_IWMHeaderInfo             : TGUID = '{96406bda-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMHeaderInfo}
  IID_IWMHeaderInfo2            : TGUID = '{15cf9781-454e-482e-b393-85fae487a810}';
  {$EXTERNALSYM IID_IWMHeaderInfo2}
  IID_IWMHeaderInfo3            : TGUID = '{15CC68E3-27CC-4ecd-B222-3F5D02D80BD5}';
  {$EXTERNALSYM IID_IWMHeaderInfo3}
  IID_IWMProfileManager         : TGUID = '{d16679f2-6ca0-472d-8d31-2f5d55aee155}';
  {$EXTERNALSYM IID_IWMProfileManager}
  IID_IWMProfileManager2        : TGUID = '{7a924e51-73c1-494d-8019-23d37ed9b89a}';
  {$EXTERNALSYM IID_IWMProfileManager2}
  IID_IWMProfileManagerLanguage : TGUID = '{ba4dcc78-7ee0-4ab8-b27a-dbce8bc51454}';
  {$EXTERNALSYM IID_IWMProfileManagerLanguage}
  IID_IWMProfile                : TGUID = '{96406bdb-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMProfile}
  IID_IWMProfile2               : TGUID = '{07e72d33-d94e-4be7-8843-60ae5ff7e5f5}';
  {$EXTERNALSYM IID_IWMProfile2}
  IID_IWMProfile3               : TGUID = '{00ef96cc-a461-4546-8bcd-c9a28f0e06f5}';
  {$EXTERNALSYM IID_IWMProfile3}
  IID_IWMStreamConfig           : TGUID = '{96406bdc-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMStreamConfig}
  IID_IWMStreamConfig2          : TGUID = '{7688d8cb-fc0d-43bd-9459-5a8dec200cfa}';
  {$EXTERNALSYM IID_IWMStreamConfig2}
  IID_IWMStreamConfig3          : TGUID = '{cb164104-3aa9-45a7-9ac9-4daee131d6e1}';
  {$EXTERNALSYM IID_IWMStreamConfig3}
  IID_IWMStreamList             : TGUID = '{96406bdd-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMStreamList}
  IID_IWMMutualExclusion        : TGUID = '{96406bde-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMMutualExclusion}
  IID_IWMMutualExclusion2       : TGUID = '{0302b57d-89d1-4ba2-85c9-166f2c53eb91}';
  {$EXTERNALSYM IID_IWMMutualExclusion2}
  IID_IWMBandwidthSharing       : TGUID = '{ad694af1-f8d9-42f8-bc47-70311b0c4f9e}';
  {$EXTERNALSYM IID_IWMBandwidthSharing}
  IID_IWMStreamPrioritization   : TGUID = '{8c1c6090-f9a8-4748-8ec3-dd1108ba1e77}';
  {$EXTERNALSYM IID_IWMStreamPrioritization}
  IID_IWMWriterAdvanced         : TGUID = '{96406be3-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMWriterAdvanced}
  IID_IWMWriterAdvanced2        : TGUID = '{962dc1ec-c046-4db8-9cc7-26ceae500817}';
  {$EXTERNALSYM IID_IWMWriterAdvanced2}
  IID_IWMWriterAdvanced3        : TGUID = '{2cd6492d-7c37-4e76-9d3b-59261183a22e}';
  {$EXTERNALSYM IID_IWMWriterAdvanced3}
  IID_IWMWriterPreprocess       : TGUID = '{fc54a285-38c4-45b5-aa23-85b9f7cb424b}';
  {$EXTERNALSYM IID_IWMWriterPreprocess}
  IID_IWMWriterSink             : TGUID = '{96406be4-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMWriterSink}
  IID_IWMWriterFileSink         : TGUID = '{96406be5-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMWriterFileSink}
  IID_IWMWriterFileSink2        : TGUID = '{14282ba7-4aef-4205-8ce5-c229035a05bc}';
  {$EXTERNALSYM IID_IWMWriterFileSink2}
  IID_IWMWriterFileSink3        : TGUID = '{3fea4feb-2945-47a7-a1dd-c53a8fc4c45c}';
  {$EXTERNALSYM IID_IWMWriterFileSink3}
  IID_IWMWriterNetworkSink      : TGUID = '{96406be7-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMWriterNetworkSink}
  IID_IWMClientConnections      : TGUID = '{73c66010-a299-41df-b1f0-ccf03b09c1c6}';
  {$EXTERNALSYM IID_IWMClientConnections}
  IID_IWMClientConnections2     : TGUID = '{4091571e-4701-4593-bb3d-d5f5f0c74246}';
  {$EXTERNALSYM IID_IWMClientConnections2}
  IID_IWMReaderAdvanced         : TGUID = '{96406bea-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMReaderAdvanced}
  IID_IWMReaderAdvanced2        : TGUID = '{ae14a945-b90c-4d0d-9127-80d665f7d73e}';
  {$EXTERNALSYM IID_IWMReaderAdvanced2}
  IID_IWMReaderAdvanced3        : TGUID = '{5dc0674b-f04b-4a4e-9f2a-b1afde2c8100}';
  {$EXTERNALSYM IID_IWMReaderAdvanced3}
  IID_IWMReaderAdvanced4        : TGUID = '{945a76a2-12ae-4d48-bd3c-cd1d90399b85}';
  {$EXTERNALSYM IID_IWMReaderAdvanced4}
  IID_IWMDRMReader              : TGUID = '{d2827540-3ee7-432c-b14c-dc17f085d3b3}';
  {$EXTERNALSYM IID_IWMDRMReader}
  IID_IWMReaderCallbackAdvanced : TGUID = '{96406beb-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMReaderCallbackAdvanced}
  IID_IWMReaderNetworkConfig    : TGUID = '{96406bec-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMReaderNetworkConfig}
  IID_IWMReaderStreamClock      : TGUID = '{96406bed-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMReaderStreamClock}
  IID_IWMIndexer                : TGUID = '{6d7cdc71-9888-11d3-8edc-00c04f6109cf}';
  {$EXTERNALSYM IID_IWMIndexer}
  IID_IWMIndexer2               : TGUID = '{b70f1e42-6255-4df0-a6b9-02b212d9e2bb}';
  {$EXTERNALSYM IID_IWMIndexer2}
  IID_IWMReaderAllocatorEx      : TGUID = '{9f762fa7-a22e-428d-93c9-ac82f3aafe5a}';
  {$EXTERNALSYM IID_IWMReaderAllocatorEx}
  IID_IWMReaderTypeNegotiation  : TGUID = '{fdbe5592-81a1-41ea-93bd-735cad1adc05}';
  {$EXTERNALSYM IID_IWMReaderTypeNegotiation}
  IID_IWMLicenseBackup          : TGUID = '{05E5AC9F-3FB6-4508-BB43-A4067BA1EBE8}';
  {$EXTERNALSYM IID_IWMLicenseBackup}
  IID_IWMLicenseRestore         : TGUID = '{C70B6334-a22e-4efb-A245-15E65A004A13}';
  {$EXTERNALSYM IID_IWMLicenseRestore}
  IID_IWMBackupRestoreProps     : TGUID = '{3C8E0DA6-996F-4ff3-A1AF-4838F9377e2e}';
  {$EXTERNALSYM IID_IWMBackupRestoreProps}
  IID_IWMPacketSize             : TGUID = '{cdfb97ab-188f-40b3-b643-5b7903975c59}';
  {$EXTERNALSYM IID_IWMPacketSize}
  IID_IWMPacketSize2            : TGUID = '{8bfc2b9e-b646-4233-a877-1c6a079669dc}';
  {$EXTERNALSYM IID_IWMPacketSize2}
  IID_IWMRegisterCallback       : TGUID = '{cf4b1f99-4de2-4e49-a363-252740d99bc1}';
  {$EXTERNALSYM IID_IWMRegisterCallback}
  IID_IWMWriterPostView         : TGUID = '{81e20ce4-75ef-491a-8004-fc53c45bdc3e}';
  {$EXTERNALSYM IID_IWMWriterPostView}
  IID_IWMWriterPostViewCallback : TGUID = '{d9d6549d-a193-4f24-b308-03123d9b7f8d}';
  {$EXTERNALSYM IID_IWMWriterPostViewCallback}
  IID_IWMCodecInfo              : TGUID = '{a970f41e-34de-4a98-b3ba-e4b3ca7528f0}';
  {$EXTERNALSYM IID_IWMCodecInfo}
  IID_IWMCodecInfo2             : TGUID = '{aa65e273-b686-4056-91ec-dd768d4df710}';
  {$EXTERNALSYM IID_IWMCodecInfo2}
  IID_IWMCodecInfo3             : TGUID = '{7e51f487-4d93-4f98-8ab4-27d0565adc51}';
  {$EXTERNALSYM IID_IWMCodecInfo3}
  IID_IWMPropertyVault          : TGUID = '{72995A79-5090-42a4-9C8C-D9D0B6D34BE5}';
  {$EXTERNALSYM IID_IWMPropertyVault}
  IID_IWMIStreamProps           : TGUID = '{6816dad3-2b4b-4c8e-8149-874c3483a753}';
  {$EXTERNALSYM IID_IWMIStreamProps}
  IID_IWMLanguageList           : TGUID = '{df683f00-2d49-4d8e-92b7-fb19f6a0dc57}';
  {$EXTERNALSYM IID_IWMLanguageList}
  IID_IWMDRMWriter              : TGUID = '{d6ea5dd0-12a0-43f4-90ab-a3fd451e6a07}';
  {$EXTERNALSYM IID_IWMDRMWriter}
  IID_IWMWriterPushSink         : TGUID = '{dc10e6a5-072c-467d-bf57-6330a9dde12a}';
  {$EXTERNALSYM IID_IWMWriterPushSink}
  IID_IWMReaderNetworkConfig2   : TGUID = '{d979a853-042b-4050-8387-c939db22013f}';
  {$EXTERNALSYM IID_IWMReaderNetworkConfig2}
  IID_IWMWatermarkInfo          : TGUID = '{6f497062-f2e2-4624-8ea7-9dd40d81fc8d}';
  {$EXTERNALSYM IID_IWMWatermarkInfo}
  IID_IWMReaderAccelerator      : TGUID = '{bddc4d08-944d-4d52-a612-46c3fda07dd4}';
  {$EXTERNALSYM IID_IWMReaderAccelerator}
  IID_IWMReaderTimecode         : TGUID = '{f369e2f0-e081-4fe6-8450-b810b2f410d1}';
  {$EXTERNALSYM IID_IWMReaderTimecode}
  IID_IWMImageInfo              : TGUID = '{9f0aa3b6-7267-4d89-88f2-ba915aa5c4c6}';
  {$EXTERNALSYM IID_IWMImageInfo}
  IID_IWMAddressAccess          : TGUID = '{BB3C6389-1633-4e92-AF14-9F3173BA39D0}';
  {$EXTERNALSYM IID_IWMAddressAccess}
  IID_IWMAddressAccess2         : TGUID = '{65a83fc2-3e98-4d4d-81b5-2a742886b33d}';
  {$EXTERNALSYM IID_IWMAddressAccess2}

///////////////////////////////////////////////////////////////////////////////
//
// Other GUIDs defined here
//
///////////////////////////////////////////////////////////////////////////////

  CLSID_WMMUTEX_Language     : TGUID = '{D6E22A00-35DA-11D1-9034-00A0C90349BE}';
  {$EXTERNALSYM CLSID_WMMUTEX_Language}
  CLSID_WMMUTEX_Bitrate      : TGUID = '{D6E22A01-35DA-11D1-9034-00A0C90349BE}';
  {$EXTERNALSYM CLSID_WMMUTEX_Bitrate}
  CLSID_WMMUTEX_Presentation : TGUID = '{D6E22A02-35DA-11D1-9034-00A0C90349BE}';
  {$EXTERNALSYM CLSID_WMMUTEX_Presentation}
  CLSID_WMMUTEX_Unknown      : TGUID = '{D6E22A03-35DA-11D1-9034-00A0C90349BE}';
  {$EXTERNALSYM CLSID_WMMUTEX_Unknown}

  CLSID_WMBandwidthSharing_Exclusive : TGUID = '{af6060aa-5197-11d2-b6af-00c04fd908e9}';
  {$EXTERNALSYM CLSID_WMBandwidthSharing_Exclusive}
  CLSID_WMBandwidthSharing_Partial   : TGUID = '{af6060ab-5197-11d2-b6af-00c04fd908e9}';
  {$EXTERNALSYM CLSID_WMBandwidthSharing_Partial}

  WMT_DMOCATEGORY_AUDIO_WATERMARK : TGUID = '{B42CDE2B-6178-4a2c-A375-89DD3FD7F497}';
  {$EXTERNALSYM WMT_DMOCATEGORY_AUDIO_WATERMARK}
  WMT_DMOCATEGORY_VIDEO_WATERMARK : TGUID = '{E77797C6-18AF-4458-BBDD-492D3F78FC8F}';
  {$EXTERNALSYM WMT_DMOCATEGORY_VIDEO_WATERMARK}

///////////////////////////////////////////////////////////////////////////////
//
// Max Video Streams / Bands
//
///////////////////////////////////////////////////////////////////////////////

  WM_MAX_VIDEO_STREAMS           = $3f;
  {$EXTERNALSYM WM_MAX_VIDEO_STREAMS}
  WM_MAX_STREAMS                 = $3f;
  {$EXTERNALSYM WM_MAX_STREAMS}

///////////////////////////////////////////////////////////////////////////////
//
// Creation functions.
//
// The SDK supports 3 major objects:
// - CLSID_WMWriter - For writing out WM content.
// - CLSID_WMReader - For playing back WM content.
// - CLSID_WMMetadataEditor - For getting and editing header metadata in WM
//   content.
//
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
//
// The following interfaces define the media types that this SDK supports.
// These media types are used by the writer, the reader, and the profile
// object, to identify the media-type specific properties of a media stream.
//
// The main media type is stored in the WM_MEDIA_TYPE structure. Some
// interesting (option) parameters may exist for particular stream types;
// in that case, an IWM<x>MediaProps interface can be used to get and set
// these additional parameters.
//
type
  {$HPPEMIT 'typedef System::DelphiInterface<IWMMediaProps> _di_IWMMediaProps;'}
  {$EXTERNALSYM IWMMediaProps}
  IWMMediaProps = interface(IUnknown)
  ['{96406BCE-2B2B-11d3-B36B-00C04F6108FF}']
  (*** IWMMediaProps methods ***)
    //
    // GetType is provided for convenience; it returns the same as the
    // majortype of the WM_MEDIA_TYPE.
    //
    function GetType(out pguidType: TGUID): HRESULT; stdcall;
    function GetMediaType(pType: PWMMediaType;
                          var pcbType: LongWord): HRESULT; stdcall;
    function SetMediaType(pType: PWMMediaType): HRESULT; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMVideoMediaProps> _di_IWMVideoMediaProps;'}
  {$EXTERNALSYM IWMVideoMediaProps}
  IWMVideoMediaProps = interface(IWMMediaProps)
  ['{96406BCF-2B2B-11d3-B36B-00C04F6108FF}']
  (*** IWMVideoMediaProps methods ***)
    function GetMaxKeyFrameSpacing(out pllTime: Int64): HRESULT; stdcall;
    function SetMaxKeyFrameSpacing(llTime: Int64): HRESULT; stdcall;
    function GetQuality(out pdwQuality: LongWord): HRESULT; stdcall;
    function SetQuality(dwQuality: LongWord): HRESULT; stdcall;
  end;

///////////////////////////////////////////////////////////////////////////////
//
// The CLSID_WMWriter basic interfaces.
//
// Usage scenario is as follows:
// 1) SetProfile to define the configuration.
// 2) Set the outputs.
// 3) Call GetInputCount (which is valid after (1)), and GetInputProps for
//    each stream. Get the default input format, and change it if desired.
// 3.5) Call SetAttribute to add metadata to the header
// At this point, the writer has been configured.
// 4) Call WriteSample repeatedly, until done. (Note that the AllocateSample
//    call is just provided for convenience. You are welcome to pass in your
//    own samples.)
// 5) Call Flush to write out any buffered data, and update the header and
//    index.
//
///////////////////////////////////////////////////////////////////////////////
  IWMProfile = interface;
  IWMInputMediaProps = interface;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMWriter> _di_IWMWriter;'}
  {$EXTERNALSYM IWMWriter}
  IWMWriter = interface(IUnknown)
  ['{96406BD4-2B2B-11d3-B36B-00C04F6108FF}']
  (*** IWMWriter methods ***)
    // This interface QI's for IWMHeaderInfo, and IWMWriterAdvanced.

    //
    // There are 3 options for setting the profile on the writer. Note that
    // setting the profile means that the profile is copied into the writer.
    // Further editing of that profile object will have no effect, unless
    // SetProfile() is called again.
    //
    // Calling SetProfile() removes any previously set header attribute info
    //
    function SetProfileByID(const guidProfile: TGUID): HRESULT; stdcall;

    function SetProfile(pProfile: IWMProfile): HRESULT; stdcall;

    //
    // The easiest way to use the writer is just to write to file.
    //
    function SetOutputFilename(pwszFilename: PWideChar): HRESULT; stdcall;

    //
    // The user can enumerate through the various inputs, and get the input
    // format. Note that these are not ASF streams; one input stream may map
    // to multiple ASF streams in a MEB scenario.
    //
    // Manipulating the IWMInputMediaProps has no effect on the writer, unless
    // the user calls SetInputProps to configure the input.
    //
    function GetInputCount(out pcInputs: LongWord): HRESULT; stdcall;

    function GetInputProps(dwInputNum: LongWord; out ppInput: IWMInputMediaProps): HRESULT; stdcall;

    function SetInputProps(dwInputNum: LongWord; pInput: IWMInputMediaProps): HRESULT; stdcall;

    //
    // Used for determining all possible format types supported by this
    // input on the writer.
    //
    function GetInputFormatCount(dwInputNumber: LongWord; out pcFormats: LongWord): HRESULT; stdcall;

    function GetInputFormat(dwInputNumber, dwFormatNumber: LongWord;
       out pProps: IWMInputMediaProps): HRESULT; stdcall;

    //
    // You must call BeginWriting before sending any samples, and
    // you must call EndWriting when you're done sending samples.
    //
    function BeginWriting: HRESULT; stdcall;

    //
    // EndWriting flushes everything, updates indices and headers,
    // and closes the file.
    //
    function EndWriting: HRESULT; stdcall;

    //
    // Allocate a sample. This is optional; the user is welcome to allocate
    // their own buffer class.
    //
    function AllocateSample(dwSampleSize: LongWord; out ppSample: INSSBuffer): HRESULT; stdcall;

    function WriteSample(dwInputNum: LongWord; cnsSampleTime: Int64; dwFlags: LongWord;
      pSample: INSSBuffer): HRESULT; stdcall;

    //
    // Flush() will flush the writer, but leaves the writer prepared to run
    // again, when WriteSample() is called again.
    // Flush() also causes an updated header to be sent to the sink.
    //
    function Flush: HRESULT; stdcall;
  end;

//
// The writer can be QI'd for this interface, which provides helpers
// to generate DRM keys.
//
  {$HPPEMIT 'typedef System::DelphiInterface<IWMDRMWriter> _di_IWMDRMWriter;'}
  {$EXTERNALSYM IWMDRMWriter}
  IWMDRMWriter = interface(IUnknown)
  ['{d6ea5dd0-12a0-43f4-90ab-a3fd451e6a07}']
  (*** IWMDRMWriter methods ***)
    function GenerateKeySeed({out} pwszKeySeed: PWideChar; var pcwchLength: LongWord): HRESULT; stdcall;
    function GenerateKeyID({out} pwszKeyID: PWideChar; var pcwchLength: LongWord): HRESULT; stdcall;
    function GenerateSigningKeyPair({out} pwszPrivKey: PWideChar;
                                    var pcwchPrivKeyLength: LongWord;
                                    {out} pwszPubKey: PWideChar;
                                    var pcwchPubKeyLength: LongWord): HRESULT; stdcall;
    function SetDRMAttribute(wStreamNum: Word; pszName: PWideChar; Type_: TWMTAttrDataType;
      pValue: PBYTE; cbLength: Word): HRESULT; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMInputMediaProps> _di_IWMInputMediaProps;'}
  {$EXTERNALSYM IWMInputMediaProps}
  IWMInputMediaProps = interface(IWMMediaProps)
  ['{96406BD5-2B2B-11d3-B36B-00C04F6108FF}']
  (*** IWMInputMediaProps methods ***)
    function GetConnectionName({out} pwszName: PWideChar; var pcchName: Word): HRESULT; stdcall;
    function GetGroupName({out} pwszName: PWideChar; var pcchName: Word): HRESULT; stdcall;
  end;


  {$HPPEMIT 'typedef System::DelphiInterface<IWMPropertyVault> _di_IWMPropertyVault;'}
  {$EXTERNALSYM IWMPropertyVault}
  IWMPropertyVault = interface(IUnknown)
  ['{72995A79-5090-42a4-9C8C-D9D0B6D34BE5}']
  (*** IWMPropertyVault methods ***)
    function GetPropertyCount(out pdwCount: LongWord): HRESULT; stdcall;
    function GetPropertyByName(pszName: PWideChar; out pType: TWMTAttrDataType;
      {out} pValue: PBYTE;  var pdwSize: LongWord): HRESULT; stdcall;
    function SetProperty(pszName: PWideChar; pType: TWMTAttrDataType;
      pValue: PBYTE; dwSize: LongWord): HRESULT; stdcall;
    function GetPropertyByIndex(dwIndex: LongWord; {out} pszName: PWideChar;
      var pdwNameLen: LongWord; out pType: TWMTAttrDataType;
      {out} pValue: PBYTE; var pdwSize: LongWord): HRESULT; stdcall;
    function CopyPropertiesFrom(pIWMPropertyVault: IWMPropertyVault): HRESULT; stdcall;
    function Clear: HRESULT; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMIStreamProps> _di_IWMIStreamProps;'}
  {$EXTERNALSYM IWMIStreamProps}
  IWMIStreamProps = interface(IUnknown)
  ['{6816dad3-2b4b-4c8e-8149-874c3483a753}']
  (*** IWMIStreamProps methods ***)
    function GetProperty(pszName: PWideChar; out pType: TWMTAttrDataType;
      {out} pValue: PBYTE; var pdwSize: LongWord): HRESULT; stdcall;
  end;

///////////////////////////////////////////////////////////////////////////////
//
// The CLSID_WMReader basic interfaces.
//
// Usage is as follows:
// 1) Call Open with a URL (possibly a local filename) and a user-supplied
//    callback. After open has completed, the file has been opened and parsed.
// 2) Call GetOutputCount, and GetOutputProps for each output. This
//    is valid after (1). This allows the user to get the output format for
//    each output.
// 3) Call Start. Status messages and samples will begin arriving in the
//    callback function.
// 4) Continue with any combination of Start/Stop/Pause, until finished.
//
///////////////////////////////////////////////////////////////////////////////

  IWMReaderCallback = interface;
  IWMOutputMediaProps = interface;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMReader> _di_IWMReader;'}
  {$EXTERNALSYM IWMReader}
  IWMReader = interface(IUnknown)
  ['{96406BD6-2B2B-11d3-B36B-00C04F6108FF}']
  (*** IWMReader methods ***)
    //
    // This interface QI's for IWMHeaderInfo, IWMProfile, IWMReaderAdvanced,
    // IWMReaderAdvanced2, and IWMReaderAdvanced3.
    //

    //
    // Open is an asynch call; it returns almost immediately (if the URL
    // is valid), and the user should wait for appropriate OnStatus calls to
    // be sent to the callback.
    //
    function Open(pwszURL: PWideChar; pCallback: IWMReaderCallback;
      pvContext: Pointer): HRESULT; stdcall;

    function Close: HRESULT; stdcall;

    //
    // The user can enumerate through the various outputs, and get the
    // output format for that data.
    //
    // Manipulating the IWMOutputMediaProps has no effect on the output, unless
    // the user also calls SetOutputProps.
    //
    function GetOutputCount(out pcOutputs: LongWord): HRESULT; stdcall;

    function GetOutputProps(dwOutputNum: LongWord; out ppOutput: IWMOutputMediaProps): HRESULT; stdcall;

    function SetOutputProps(dwOutputNum: LongWord; pOutput: IWMOutputMediaProps): HRESULT; stdcall;

    //
    // Used for determining all possible format types supported by this
    // output on the reader.
    //
    function GetOutputFormatCount(dwOutputNumber: LongWord; out pcFormats: LongWord): HRESULT; stdcall;

    function GetOutputFormat(dwOutputNumber, dwFormatNumber: LongWord;
      out ppProps: IWMOutputMediaProps): HRESULT; stdcall;

    //
    // If duration is 0, play to the end of the file.
    // If msStart is set to WM_START_CURRENTPOSITION then don't perform a seek
    // operation.  A good use for this is when you want to change the rate but
    // not the current file position.
    //
    // Note that any call to start while Paused will be treated as a seek.
    // Even calls to Start( WM_START_CURRENTPOSITION, ... ).  If your intention
    // is to seek (which will incur the buffering penalty from network files)
    // then you can go ahead and call Start.  However, if your intention was
    // to continue playing from where the user paused, you should call Resume
    // instead.
    //
    function Start(cnsStart, cnsDuration: Int64; fRate: Single; pvContext: Pointer): HRESULT; stdcall;
    function Stop: HRESULT; stdcall;
    function Pause: HRESULT; stdcall;
    function Resume: HRESULT; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMSyncReader> _di_IWMSyncReader;'}
  {$EXTERNALSYM IWMSyncReader}
  IWMSyncReader = interface(IUnknown)
  ['{9397F121-7705-4dc9-B049-98B698188414}']
  (*** IWMSyncReader methods ***)
    //
    // This interface can be QI'ed for IWMProfile, IWMHeaderInfo and
    // IWMReaderTimecode interfaces
    //

    //
    // Open is an synchronous call. We do not support streaming at this time.
    // All open with http:// etc would fail. We do support MP3 or Windows
    // media files from both local file or UNC path.
    //
    function Open(pwszFilename: PWideChar): HRESULT; stdcall;

    function Close: HRESULT; stdcall;

    //
    // SetRange is basically a seek method, you can only set same range
    // for all streams. Use cnsDuration=0 to specify reading to end of file.
    //
    function SetRange(cnsStartTime, cnsDuration: Int64): HRESULT; stdcall;

    //
    // SetRangeByFrame is frame-based access, the file has to be frame
    // indexed to succeeded this call. If the call is successful, all
    // streams are synchronized to the same position based on time.
    // Also use cFramesToRead=0 to specify reading to end of file.
    //
    function SetRangeByFrame(wStreamNum: Word; qwFrameNumber, cFramesToRead: Int64 ): HRESULT; stdcall;

    //
    // If a valid stream number is specified, next sample from that stream
    // will be returned, pwStreamNum can be NULL in this case. Otherwise,
    // GetNextSample returns the next sample in time line (regardless of
    // which stream). The sample's stream number will be returned.
    // Time line is presentation time if no output setting is specified.
    // To get early delivery for some stream, use SetOutputSetting
    //
    function GetNextSample(wStreamNum: Word; out ppSample: INSSBuffer;
      out pcnsSampleTime: Int64; out pcnsDuration: Int64;
      out pdwFlags: LongWord; out pdwOutputNum: LongWord;
      out pwStreamNum: Word): HRESULT; stdcall;

    //
    // Stream selection methods are the same as asynchronous interface
    //
    function SetStreamsSelected(cStreamCount: Word; pwStreamNumbers: PWORD;
      pSelections: PWMTStreamSelection): HRESULT; stdcall;

    function GetStreamSelected(wStreamNum: Word; out pSelection: TWMTStreamSelection): HRESULT; stdcall;

    function SetReadStreamSamples(wStreamNum: Word; fCompressed: BOOL): HRESULT; stdcall;

    function GetReadStreamSamples(wStreamNum: Word; out pfCompressed: BOOL): HRESULT; stdcall;

    //
    // The following two methods are the same as the ones in
    // IWMReaderAdvanced2 interface. We don't support JustInTimeDecode
    // in this interface
    //
    function GetOutputSetting(dwOutputNum: LongWord; pszName: PWideChar;
      out pType: TWMTAttrDataType; {out} pValue: PBYTE;
      var pcbLength: Word): HRESULT; stdcall;

    //
    // Sets a named setting for a particular output
    //
    function SetOutputSetting(dwOutputNum: LongWord; pszName: PWideChar;
      Type_: TWMTAttrDataType; {in} pValue: PBYTE; cbLength: Word): HRESULT; stdcall;

    //
    // The following methods are important for receiving uncompressed samples,
    // they are identical to methods in asynchronous Reader interface.
    //
    function GetOutputCount(out pcOutputs: LongWord): HRESULT; stdcall;

    function GetOutputProps(dwOutputNum: LongWord; out ppOutput: IWMOutputMediaProps): HRESULT; stdcall;

    function SetOutputProps(dwOutputNum: LongWord; pOutput: IWMOutputMediaProps): HRESULT; stdcall;

    //
    // Used for determining all possible format types supported by this
    // output on the reader.
    //
    function GetOutputFormatCount(dwOutputNum: LongWord; out pcFormats: LongWord): HRESULT; stdcall;

    function GetOutputFormat(dwOutputNum, dwFormatNum: LongWord; out ppProps: IWMOutputMediaProps): HRESULT; stdcall;

    //
    // Methods provided to relate output numbers with stream numbers
    //
    function GetOutputNumberForStream(wStreamNum: Word; out pdwOutputNum: LongWord): HRESULT; stdcall;

    function GetStreamNumberForOutput(dwOutputNum: LongWord; out pwStreamNum: Word): HRESULT; stdcall;

    function GetMaxOutputSampleSize(dwOutput: LongWord; out pcbMax: LongWord): HRESULT; stdcall;

    function GetMaxStreamSampleSize(wStream: Word; out pcbMax: LongWord): HRESULT; stdcall;

    //
    // Same as IWMSyncReader::Open but takes an IStream interface pointer
    // instead of an file name to be opened. This method is typically
    // used for custom source.
    //
    function OpenStream(pStream: IStream): HRESULT; stdcall;
  end;

  IWMReaderAllocatorEx = interface;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMSyncReader2> _di_IWMSyncReader2;'}
  {$EXTERNALSYM IWMSyncReader2}
  IWMSyncReader2 = interface(IWMSyncReader)
  ['{faed3d21-1b6b-4af7-8cb6-3e189bbc187b}']
  (*** IWMSyncReader2 methods ***)
    //
    // SetRangeByTimecode is timecode-based access, the file has to be timecode
    // indexed to succeeded this call. If the call is successful, all
    // streams are synchronized to the same position based on presentation
    // time.
    //
    function SetRangeByTimecode(wStreamNum: Word;
      var pStart, pEnd: TWMTTimeCodeExtensionData): HRESULT; stdcall;

    //
    // SetRangeByFrame is frame-based access, the file has to be frame
    // indexed to succeeded this call. If the call is successful, all
    // streams are synchronized to the same position based on time.
    // Also use cFramesToRead=0 to specify reading to end of file.
    //
    function SetRangeByFrameEx(wStreamNum: Word; qwFrameNumber, cFramesToRead: Int64;
      out pcnsStartTime: Int64): HRESULT; stdcall;

    //
    // The following four methods are for user buffer allocations
    //
    function SetAllocateForOutput(dwOutputNum: LongWord; pAllocator: IWMReaderAllocatorEx): HRESULT; stdcall;
    function GetAllocateForOutput(dwOutputNum: LongWord; out ppAllocator: IWMReaderAllocatorEx): HRESULT; stdcall;
    function SetAllocateForStream(wStreamNum: Word; pAllocator: IWMReaderAllocatorEx): HRESULT; stdcall;
    function GetAllocateForStream(dwSreamNum: Word; out ppAllocator: IWMReaderAllocatorEx): HRESULT; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMOutputMediaProps> _di_IWMOutputMediaProps;'}
  {$EXTERNALSYM IWMOutputMediaProps}
  IWMOutputMediaProps = interface(IWMMediaProps)
  ['{96406BD7-2B2B-11d3-B36B-00C04F6108FF}']
  (*** IWMOutputMediaProps methods ***)
    //
    // A Stream Group and type together uniquely identify each output. (The
    // type is on IWMMediaProps).
    //
    function GetStreamGroupName({out} pwszName: PWideChar; var pcchName: Word): HRESULT; stdcall;
    function GetConnectionName({out} pwszName: PWideChar; var pcchName: Word): HRESULT; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMStatusCallback> _di_IWMStatusCallback;'}
  {$EXTERNALSYM IWMStatusCallback}
  IWMStatusCallback = interface(IUnknown)
  ['{6d7cdc70-9888-11d3-8edc-00c04f6109cf}']
  (*** IWMStatusCallback methods ***)
    // The contents of pValue depends on the Status.
    function OnStatus(Status: TWMTStatus; hr: HRESULT; dwType: TWMTAttrDataType;
      pValue: PBYTE; pvContext: Pointer): HRESULT; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMReaderCallback> _di_IWMReaderCallback;'}
  {$EXTERNALSYM IWMReaderCallback}
  IWMReaderCallback = interface(IWMStatusCallback)
  ['{96406BD8-2B2B-11d3-B36B-00C04F6108FF}']
  (*** IWMReaderCallback methods ***)
    // cnsSampleDuration will be 0 for most media types.
    function OnSample(dwOutputNum: LongWord; cnsSampleTime, cnsSampleDuration: Int64;
      dwFlags: LongWord; pSample: INSSBuffer; pvContext: Pointer): HRESULT; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMCredentialCallback> _di_IWMCredentialCallback;'}
  {$EXTERNALSYM IWMCredentialCallback}
  IWMCredentialCallback = interface(IUnknown)
  ['{342e0eb7-e651-450c-975b-2ace2c90c48e}']
  (*** IWMCredentialCallback methods ***)
    function AcquireCredentials(pwszRealm, pwszSite: PWideChar; {out} pwszUser: PWideChar;
      cchUser: LongWord; {out} pwszPassword: PWideChar; cchPassword: LongWord;
      hrStatus: HRESULT; out pdwFlags: LongWord): HRESULT; stdcall; // Combination of WMT_CREDENTIAL_FLAGS
  end;

///////////////////////////////////////////////////////////////////////////////
//
// The CLSID_WMMetadataEditor basic interfaces.
//
// Usage:
// 1) Call Open with a filename.
// 2) QI for IWMHeaderInfo, and use that to get and set attributes as
//    needed.
// 3) Flush() will cause any changes to be written back to disk (if possible).
// 4) Close() closes the file without writing any changes to disk.
//
///////////////////////////////////////////////////////////////////////////////

  {$HPPEMIT 'typedef System::DelphiInterface<IWMMetadataEditor> _di_IWMMetadataEditor;'}
  {$EXTERNALSYM IWMMetadataEditor}
  IWMMetadataEditor = interface(IUnknown)
  ['{96406BD9-2B2B-11d3-B36B-00C04F6108FF}']
  (*** IWMMetadataEditor methods ***)
    // QI this for IWMHeaderInfo to edit the header attributes.
    // Manage the file
    function Open(pwszFilename: PWideChar): HRESULT; stdcall;
    function Close: HRESULT; stdcall;
    function Flush: HRESULT; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMMetadataEditor2> _di_IWMMetadataEditor2;'}
  {$EXTERNALSYM IWMMetadataEditor2}
  IWMMetadataEditor2 = interface(IWMMetadataEditor)
  ['{203CFFE3-2E18-4fdf-B59D-6E71530534CF}']
  (*** IWMMetadataEditor2 methods ***)
    //
    // The flags to dwDesiredAccess and dwShareMode match the flags to
    // (the win32 API) CreateFile.
    //
    // Supported combinations are:
    //
    //   dwDesiredAccess               dwShareMode
    //   ----------------------------  -----------
    //   GENERIC_READ | GENERIC_WRITE  0
    //   GENERIC_READ | GENERIC_WRITE  FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE
    //   GENERIC_READ                  0
    //   GENERIC_READ                  FILE_SHARE_READ
    //   GENERIC_READ                  FILE_SHARE_DELETE
    //   GENERIC_READ                  FILE_SHARE_READ | FILE_SHARE_DELETE
    //
    function OpenEx(pwszFilename: PWideChar; dwDesiredAccess, dwShareMode: LongWord): HRESULT; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMDRMEditor> _di_IWMDRMEditor;'}
  {$EXTERNALSYM IWMDRMEditor}
  IWMDRMEditor = interface(IUnknown)
  ['{FF130EBC-A6C3-42A6-B401-C3382C3E08B3}']
  (*** IWMDRMEditor methods ***)
    // For accessing DRM properties from the metadata editor.
    function GetDRMProperty(pwstrName: PWideChar; out pdwType: TWMTAttrDataType;
      {out} pValue: PBYTE; var pcbLength: Word): HRESULT; stdcall;
  end;

///////////////////////////////////////////////////////////////////////////////
//
// Below are utility interfaces used across all 3 of the main objects.
//
///////////////////////////////////////////////////////////////////////////////


//
// The 3 main interface (IWMWriter, IWMReader, and
// IWMMetadataEditor) can be QI'd for this interface to get and set
// header attributes, and markers.
//

  {$HPPEMIT 'typedef System::DelphiInterface<IWMHeaderInfo> _di_IWMHeaderInfo;'}
  {$EXTERNALSYM IWMHeaderInfo}
  IWMHeaderInfo = interface(IUnknown)
  ['{96406BDA-2B2B-11d3-B36B-00C04F6108FF}']
  (*** IWMHeaderInfo methods ***)
    // For attributes, the stream number passed in means:
    // -1 (0xffff) to specifies "any or no stream".
    // 0 specifies "no stream".
    // Any other value indicates the stream number.
    //
    // Windows Media version 4 and earlier does not support per stream
    // attributes, so any stream number other than 0 will fail.
    //
    function GetAttributeCount(wStreamNum: Word; out pcAttributes: Word): HRESULT; stdcall;

    function GetAttributeByIndex(wIndex: Word; var pwStreamNum: Word;
      {out} pwszName: PWideChar; var pcchNameLen: Word;
      out pType: TWMTAttrDataType; {out} pValue: PBYTE;
      var pcbLength: Word): HRESULT; stdcall;

    function GetAttributeByName(var pwStreamNum: Word; pszName: PWideChar;
      out pType: TWMTAttrDataType; {out} pValue: PBYTE;
      var pcbLength: Word): HRESULT; stdcall;

    function SetAttribute(wStreamNum: Word; pszName: PWideChar;
      Type_: TWMTAttrDataType; {in} pValue: PBYTE;
      cbLength: Word): HRESULT; stdcall;

    // Marker methods.
    function GetMarkerCount(out pcMarkers: Word): HRESULT; stdcall;

    function GetMarker(wIndex: Word; {out} pwszMarkerName: PWideChar;
      var pcchMarkerNameLen: Word; out pcnsMarkerTime: Int64): HRESULT; stdcall;

    function AddMarker(pwszMarkerName: PWideChar; cnsMarkerTime: Int64): HRESULT; stdcall;

    function RemoveMarker(wIndex: Word): HRESULT; stdcall;

    // Script command methods.
    function GetScriptCount(out pcScripts: Word): HRESULT; stdcall;

    function GetScript(wIndex: Word; {out} pwszType: PWideChar;
      var pcchTypeLen: Word; {out} pwszCommand: PWideChar;
      var pcchCommandLen: Word; out pcnsScriptTime: Int64): HRESULT; stdcall;

    function AddScript(pwszType, pwszCommand: PWideChar;
      cnsScriptTime: Int64): HRESULT; stdcall;

    function RemoveScript(wIndex: Word): HRESULT; stdcall;
  end;

// The 3 main interface (IWMWriter, IWMReader, and
// IWMMetadataEditor) can be QI'd for this interface to get and set
// header attributes, and markers.

  {$HPPEMIT 'typedef System::DelphiInterface<IWMHeaderInfo2> _di_IWMHeaderInfo2;'}
  {$EXTERNALSYM IWMHeaderInfo2}
  IWMHeaderInfo2 = interface(IWMHeaderInfo)
  ['{15CF9781-454E-482e-B393-85FAE487A810}']
  (*** IWMHeaderInfo2 methods ***)
    function GetCodecInfoCount(out pcCodecInfos: LongWord): HRESULT; stdcall;

    function GetCodecInfo(wIndex: LongWord; var pcchName: Word;
      {out} pwszName: PWideChar; var pcchDescription: Word;
      {out} pwszDescription: PWideChar; out pCodecType: WMT_CODEC_INFO_TYPE;
      var pcbCodecInfo: Word; {out} pbCodecInfo: PBYTE): HRESULT; stdcall;
  end;

///////////////////////////////////////////////////////////////////////////////
//
// IWMHeaderInfo3 - V9 metadata features. This interface extends IWMHeaderInfo
// functionality:
//  - allows >64k attributes;
//  - allows multiple attributes with the same name;
//  - works with attribute languages.
//
///////////////////////////////////////////////////////////////////////////////

  {$HPPEMIT 'typedef System::DelphiInterface<IWMHeaderInfo3> _di_IWMHeaderInfo3;'}
  {$EXTERNALSYM IWMHeaderInfo3}
  IWMHeaderInfo3 = interface(IWMHeaderInfo2)
  ['{15CC68E3-27CC-4ecd-B222-3F5D02D80BD5}']
  (*** IWMHeaderInfo3 methods ***)
    function GetAttributeCountEx(wStreamNum: Word; out pcAttributes: Word): HRESULT; stdcall;

    function GetAttributeIndices(wStreamNum: Word; pwszName: PWideChar;
      pwLangIndex: PWORD; {out} pwIndices: PWORD; var pwCount: Word): HRESULT; stdcall;

   function GetAttributeByIndexEx(wStreamNum: Word; wIndex: Word; {out} pwszName: PWideChar;
     var  pwNameLen: Word; out  pType: TWMTAttrDataType; out  pwLangIndex: Word;
    {out} pValue: PBYTE; var pdwDataLength: LongWord): HRESULT; stdcall;

    function ModifyAttribute(wStreamNum: Word; wIndex: Word; Type_: TWMTAttrDataType;
      wLangIndex: Word; pValue: PBYTE; dwLength: LongWord): HRESULT; stdcall;

    function AddAttribute(wStreamNum: Word; pszName: PWideChar;
      out pwIndex: Word; Type_: TWMTAttrDataType; wLangIndex: Word;
      pValue: PBYTE; dwLength: LongWord): HRESULT; stdcall;

    function DeleteAttribute(wStreamNum, wIndex: Word): HRESULT; stdcall;

    function AddCodecInfo(pwszName: PWideChar; pwszDescription: PWideChar;
      codecType: WMT_CODEC_INFO_TYPE; cbCodecInfo: Word;
      pbCodecInfo: PBYTE): HRESULT; stdcall;
  end;

///////////////////////////////////////////////////////////////////////////////
//
// The profile API. The intention is that most users don't touch the profile
// API, but just use pre-existing profiles.
//
// Profiles define authoring configurations, such as stream types, bitrates,
// etc.
//
///////////////////////////////////////////////////////////////////////////////

  {$HPPEMIT 'typedef System::DelphiInterface<IWMProfileManager> _di_IWMProfileManager;'}
  {$EXTERNALSYM IWMProfileManager}
  IWMProfileManager = interface(IUnknown)
  ['{d16679f2-6ca0-472d-8d31-2f5d55aee155}']
  (*** IWMProfileManager methods ***)
    // Create a profile with nothing in it.
    function CreateEmptyProfile(dwVersion: WMT_VERSION; out ppProfile: IWMProfile): HRESULT; stdcall;

    // Load a system profile given its ID.
    function LoadProfileByID(const guidProfile: TGUID; out ppProfile: IWMProfile): HRESULT; stdcall;

    // Load a profile from a stored string.
    function LoadProfileByData(pwszProfile: PWideChar; out ppProfile: IWMProfile): HRESULT; stdcall;

    // Save profile specified by its string.
    function SaveProfile(pIWMProfile: IWMProfile; pwszProfile: PWideChar;
      var pdwLength: LongWord): HRESULT; stdcall;

    // Iterate through the system profiles.
    function GetSystemProfileCount(out pcProfiles: LongWord): HRESULT; stdcall;

    function LoadSystemProfile(dwProfileIndex: LongWord; out ppProfile: IWMProfile): HRESULT; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMProfileManager2> _di_IWMProfileManager2;'}
  {$EXTERNALSYM IWMProfileManager2}
  IWMProfileManager2 = interface(IWMProfileManager)
  ['{7A924E51-73C1-494d-8019-23D37ED9B89A}']
  (*** IWMProfileManager2 methods ***)
    // Set the version number of the system profiles that the profile manager
    // will enumerate. WMT_VER_4_0 is the default, for compatibility reasons,
    // so be sure to set this to the latest version if that is the desired
    // result.
    function GetSystemProfileVersion(out pdwVersion: WMT_VERSION): HRESULT; stdcall;
    function SetSystemProfileVersion(dwVersion: WMT_VERSION): HRESULT; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMProfileManagerLanguage> _di_IWMProfileManagerLanguage;'}
  {$EXTERNALSYM IWMProfileManagerLanguage}
  IWMProfileManagerLanguage = interface(IUnknown)
  ['{BA4DCC78-7EE0-4ab8-B27A-DBCE8BC51454}']
  (*** IWMProfileManagerLanguage methods ***)
    // Set the language ID that user prefers to use, it must be a valid
    // LANGID we support and the corresponding localized profile must
    // present. Or we'll use English as default
    function GetUserLanguageID(out wLangID: Word): HRESULT; stdcall;
    function SetUserLanguageID(wLangID: Word): HRESULT; stdcall;
  end;

  IWMStreamConfig = interface;
  IWMMutualExclusion = interface;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMProfile> _di_IWMProfile;'}
  {$EXTERNALSYM IWMProfile}
  IWMProfile = interface(IUnknown)
  ['{96406BDB-2B2B-11d3-B36B-00C04F6108FF}']
  (*** IWMProfile methods ***)
    // By default, when the user creates a profile, it will use the latest
    // version of Windows Media. To create a backward-compatible profile,
    // call IWMProfileManager::CreateEmptyProfile with the appropriate version
    // number.
    function GetVersion(out pdwVersion: WMT_VERSION): HRESULT; stdcall;

    // Profiles have names and descriptions, for use when displaying lists
    // of profiles, etc.
    function GetName({out} pwszName: PWideChar; var pcchName: LongWord): HRESULT; stdcall;
    function SetName(pwszName: PWideChar): HRESULT; stdcall;

    function GetDescription({out} pwszDescription: PWideChar;
      var pcchDescription: LongWord): HRESULT; stdcall;

    function SetDescription(pwszDescription: PWideChar): HRESULT; stdcall;

    // Methods for enumerating the streams. Note that updating the
    // returned IWMStreamConfig has no effect on the profile until you
    // call ReconfigStream().
    function GetStreamCount(out pcStreams: LongWord): HRESULT; stdcall;

    function GetStream(dwStreamIndex: LongWord; out ppConfig: IWMStreamConfig): HRESULT; stdcall;

    function GetStreamByNumber(wStreamNum: Word; out ppConfig: IWMStreamConfig): HRESULT; stdcall;

    // Remove a stream.
    function RemoveStream(pConfig: IWMStreamConfig): HRESULT; stdcall;

    function RemoveStreamByNumber(wStreamNum: Word): HRESULT; stdcall;

    // Adding a stream copies the config into the profile.
    function AddStream(pConfig: IWMStreamConfig): HRESULT; stdcall;

    function ReconfigStream(pConfig: IWMStreamConfig): HRESULT; stdcall;

    // Create a new stream config object (avoiding the need to CoCreate).
    // This will still need to be added to the profile using the AddStream()
    // call (but only after it has been configured).
    function CreateNewStream(const guidStreamType: TGUID;
      out ppConfig: IWMStreamConfig): HRESULT; stdcall;

    // Mutual Exclusion. As above, only Add and Remove actual change the
    // profile.
    function GetMutualExclusionCount(out pcME: LongWord): HRESULT; stdcall;

    function GetMutualExclusion(dwMEIndex: LongWord; out ppME: IWMMutualExclusion): HRESULT; stdcall;

    function RemoveMutualExclusion(pME: IWMMutualExclusion): HRESULT; stdcall;

    function AddMutualExclusion(pME: IWMMutualExclusion): HRESULT; stdcall;

    function CreateNewMutualExclusion(out ppME: IWMMutualExclusion): HRESULT; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMProfile2> _di_IWMProfile2;'}
  {$EXTERNALSYM IWMProfile2}
  IWMProfile2 = interface(IWMProfile)
  ['{07E72D33-D94E-4be7-8843-60AE5FF7E5F5}']
  (*** IWMProfile2 methods ***)
    // Get/set this profile's unique identifier
    function GetProfileID(out pguidID: TGUID): HRESULT; stdcall;
  end;

  IWMBandwidthSharing = interface;
  IWMStreamPrioritization = interface;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMProfile3> _di_IWMProfile3;'}
  {$EXTERNALSYM IWMProfile3}
  IWMProfile3 = interface(IWMProfile2)
  ['{00EF96CC-A461-4546-8BCD-C9A28F0E06F5}']
  (*** IWMProfile3 methods ***)
    // Get/set the storage format.  These calls have been deprecated.
    // Query the WM/ContainerFormat attribute instead.
    //
    function GetStorageFormat(out pnStorageFormat: TWMTStorageFormat): HRESULT; stdcall;

    function SetStorageFormat(nStorageFormat: TWMTStorageFormat): HRESULT; stdcall;

    //
    // Bandwidth sharing. Only Add and Remove actually change the
    // profile.
    //
    function GetBandwidthSharingCount(out pcBS: LongWord): HRESULT; stdcall;

    function GetBandwidthSharing(dwBSIndex: LongWord; out ppBS: IWMBandwidthSharing): HRESULT; stdcall;

    function RemoveBandwidthSharing(pBS: IWMBandwidthSharing): HRESULT; stdcall;

    function AddBandwidthSharing(pBS: IWMBandwidthSharing): HRESULT; stdcall;

    function CreateNewBandwidthSharing(out ppBS: IWMBandwidthSharing): HRESULT; stdcall;

    //
    // Stream prioritization.  Only Set and Remove actually
    // change the profile.
    // Note that there can be at most one such object.
    //
    function GetStreamPrioritization(out ppSP: IWMStreamPrioritization): HRESULT; stdcall;

    function SetStreamPrioritization(pSP: IWMStreamPrioritization): HRESULT; stdcall;

    function RemoveStreamPrioritization: HRESULT; stdcall;

    function CreateNewStreamPrioritization(out ppSP: IWMStreamPrioritization): HRESULT; stdcall;

    //
    // Get information about how the muxing will be done
    //
    function GetExpectedPacketCount(msDuration: Int64; out pcPackets: Int64): HRESULT; stdcall;
  end;

//
// IWMStreamConfig represents an ASF stream.
//
  {$HPPEMIT 'typedef System::DelphiInterface<IWMStreamConfig> _di_IWMStreamConfig;'}
  {$EXTERNALSYM IWMStreamConfig}
  IWMStreamConfig = interface(IUnknown)
  ['{96406BDC-2B2B-11d3-B36B-00C04F6108FF}']
  (*** IWMStreamConfig methods ***)
    // This interface QI's for IWMMediaProps and one of it's inheritors.
    // (IWMVideoMediaProps, for instance).
    function GetStreamType(out pguidStreamType: TGUID): HRESULT; stdcall;
    function GetStreamNumber(out pwStreamNum: Word): HRESULT; stdcall;
    function SetStreamNumber(wStreamNum: Word): HRESULT; stdcall;
    function GetStreamName({out} pwszStreamName: PWideChar; var pcchStreamName: Word): HRESULT; stdcall;
    function SetStreamName(pwszStreamName: PWideChar): HRESULT; stdcall;
    function GetConnectionName({out} pwszInputName: PWideChar; var pcchInputName: Word): HRESULT; stdcall;
    function SetConnectionName(pwszInputName: PWideChar): HRESULT; stdcall;
    function GetBitrate(out pdwBitrate: LongWord): HRESULT; stdcall;
    function SetBitrate(pdwBitrate: LongWord): HRESULT; stdcall;

    //
    // A buffer window of -1 (0xffffffff) indicates that the buffer window
    // is unknown. On the writer side, this means the writer can use whatever
    // buffer window it chooses.
    //
    function GetBufferWindow(out pmsBufferWindow: LongWord): HRESULT; stdcall;
    function SetBufferWindow(msBufferWindow: LongWord): HRESULT; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMStreamConfig2> _di_IWMStreamConfig2;'}
  {$EXTERNALSYM IWMStreamConfig2}
  IWMStreamConfig2 = interface(IWMStreamConfig)
  ['{7688D8CB-FC0D-43BD-9459-5A8DEC200CFA}']
  (*** IWMStreamConfig2 methods ***)
    // Get/set the type of data communication protocol (reliable or unreliable)
    function GetTransportType (out pnTransportType: TWMTTransportType): HRESULT; stdcall;

    function SetTransportType(nTransportType: TWMTTransportType): HRESULT; stdcall;

    //
    // Add/Get data unit extension systems (for attaching user-defined
    // data to samples in the output file)
    //
    function AddDataUnitExtension(const guidExtensionSystemID: TGUID;
      cbExtensionDataSize: Word; pbExtensionSystemInfo: PBYTE;
      cbExtensionSystemInfo: LongWord): HRESULT; stdcall;

    function GetDataUnitExtensionCount(out pcDataUnitExtensions: Word): HRESULT; stdcall;

    function GetDataUnitExtension(wDataUnitExtensionNumber: Word;
      out pguidExtensionSystemID: TGUID; out pcbExtensionDataSize: Word;
      {out} pbExtensionSystemInfo: PBYTE; var pcbExtensionSystemInfo: LongWord): HRESULT; stdcall;

    function RemoveAllDataUnitExtensions: HRESULT; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMStreamConfig3> _di_IWMStreamConfig3;'}
  {$EXTERNALSYM IWMStreamConfig3}
  IWMStreamConfig3 = interface(IWMStreamConfig2)
  ['{CB164104-3AA9-45a7-9AC9-4DAEE131D6E1}']
  (*** IWMStreamConfig3 methods ***)
    // Get/set language info for this stream as an RFC1766 string.
    function GetLanguage({out} pwszLanguageString: PWideChar; var pcchLanguageStringLength: Word): HRESULT; stdcall;
    function SetLanguage(pwszLanguageString: PWideChar): HRESULT; stdcall;
  end;

  // Controls how big packets can get in an ASF file.
  {$HPPEMIT 'typedef System::DelphiInterface<IWMPacketSize> _di_IWMPacketSize;'}
  {$EXTERNALSYM IWMPacketSize}
  IWMPacketSize = interface(IUnknown)
  ['{CDFB97AB-188F-40b3-B643-5B7903975C59}']
  (*** IWMPacketSize methods ***)
    function GetMaxPacketSize(out pdwMaxPacketSize: LongWord): HRESULT; stdcall;
    function SetMaxPacketSize(dwMaxPacketSize: LongWord): HRESULT; stdcall;
  end;

  // Controls how small packets can get in an ASF file.
  {$HPPEMIT 'typedef System::DelphiInterface<IWMPacketSize2> _di_IWMPacketSize2;'}
  {$EXTERNALSYM IWMPacketSize2}
  IWMPacketSize2 = interface(IWMPacketSize)
  ['{8BFC2B9E-B646-4233-A877-1C6A079669DC}']
  (*** IWMPacketSize2 methods ***)
    function GetMinPacketSize(out pdwMinPacketSize: LongWord): HRESULT; stdcall;
    function SetMinPacketSize(dwMinPacketSize: LongWord): HRESULT; stdcall;
  end;

  // IWMStreamList is used for the various objects that define relationships
  // between streams.
  {$HPPEMIT 'typedef System::DelphiInterface<IWMStreamList> _di_IWMStreamList;'}
  {$EXTERNALSYM IWMStreamList}
  IWMStreamList = interface(IUnknown)
  ['{96406BDD-2B2B-11d3-B36B-00C04F6108FF}']
  (*** IWMStreamList methods ***)
    function GetStreams({out} pwStreamNumArray: PWORD; var pcStreams: PWORD): HRESULT; stdcall;
    function AddStream(wStreamNum: Word): HRESULT; stdcall;
    function RemoveStream(wStreamNum: Word): HRESULT; stdcall;
  end;

  // IWMMutualExclusion specifies a group of streams, of which only one can
  // be played at once. These are used to do MEB (stream selection based on
  // bandwidth).
  {$HPPEMIT 'typedef System::DelphiInterface<IWMMutualExclusion> _di_IWMMutualExclusion;'}
  {$EXTERNALSYM IWMMutualExclusion}
  IWMMutualExclusion = interface(IWMStreamList)
  ['{96406BDE-2B2B-11d3-B36B-00C04F6108FF}']
  (*** IWMMutualExclusion methods ***)
    // The possible types of mutual exclusion are defined in the ASF
    // header.
    function GetType(out pguidType: TGUID): HRESULT; stdcall;
    function SetType(guidType: TGUID): HRESULT; stdcall;
  end;

  // IWMMutualExclusion2 extends IWMMutualExclusion to incorporate more advanced
  // mutual exclusion concepts.
  //
  {$HPPEMIT 'typedef System::DelphiInterface<IWMMutualExclusion2> _di_IWMMutualExclusion2;'}
  {$EXTERNALSYM IWMMutualExclusion2}
  IWMMutualExclusion2 = interface(IWMMutualExclusion)
  ['{0302B57D-89D1-4ba2-85C9-166F2C53EB91}']
  (*** IWMMutualExclusion2 methods ***)
    // Methods to get and set the name of the mutual exclusion.
    function GetName({out} pwszName: PWideChar; var pcchName: Word): HRESULT; stdcall;
    function SetName(pwszName: PWideChar): HRESULT; stdcall;

    // Methods to query and manipulate the records in the object.
    function GetRecordCount(out pwRecordCount: Word): HRESULT; stdcall;
    function AddRecord: HRESULT; stdcall;
    function RemoveRecord(wRecordNumber: Word ): HRESULT; stdcall;

    // Methods to get and set the name associated with a record.
    function GetRecordName(wRecordNumber: Word; {out} pwszRecordName: PWideChar;
      var pcchRecordName: Word): HRESULT; stdcall;
    function SetRecordName(wRecordNumber: Word; pwszRecordName: PWideChar): HRESULT; stdcall;

    // Methods to get and set the streams in a record.
    function GetStreamsForRecord(wRecordNumber: Word; {out} pwStreamNumArray: PWORD;
      var pcStreams: Word): HRESULT; stdcall;
    function AddStreamForRecord(wRecordNumber, wStreamNumber: Word): HRESULT; stdcall;
    function RemoveStreamForRecord(wRecordNumber, wStreamNumber: Word): HRESULT; stdcall;
  end;

  // IWMBandwidthSharing specifies a group of streams whose combined bitrate
  // and buffer size are not to exceed the parameters given to this interface,
  // even though the sum of their individual bitrates may be more.
  {$HPPEMIT 'typedef System::DelphiInterface<IWMBandwidthSharing> _di_IWMBandwidthSharing;'}
  {$EXTERNALSYM IWMBandwidthSharing}
  IWMBandwidthSharing = interface(IWMStreamList)
  ['{AD694AF1-F8D9-42F8-BC47-70311B0C4F9E}']
  (*** IWMBandwidthSharing methods ***)
    // The possible types of bandwidth are defined in the ASF
    // header.
    function GetType(out pguidType: TGUID): HRESULT; stdcall;
    function SetType(const guidType: TGUID): HRESULT; stdcall;

    // Methods to get and set properties of the combined streams.
    // Buffer windows are given in milliseconds
    function GetBandwidth(out pdwBitrate, pmsBufferWindow: LongWord): HRESULT; stdcall;
    function SetBandwidth(dwBitrate, msBufferWindow: LongWord): HRESULT; stdcall;
  end;

  // IWMStreamPrioritization specifies the author's intentions as to which
  // streams should or should not be dropped in response to various network
  // congestion situations.
  {$HPPEMIT 'typedef System::DelphiInterface<IWMStreamPrioritization> _di_IWMStreamPrioritization;'}
  {$EXTERNALSYM IWMStreamPrioritization}
  IWMStreamPrioritization = interface(IUnknown)
    ['{8C1C6090-F9A8-4748-8EC3-DD1108BA1E77}']
    (*** IWMStreamPrioritization methods ***)
    // Records in a stream prioritization object are given in order of
    // decreasing priority
    function GetPriorityRecords({out} pRecordArray: PWMStreamPrioritizationRecord; var cRecords: Word): HResult; stdcall;
    function SetPriorityRecords(pRecordArray: PWMStreamPrioritizationRecord; cRecords: Word): HResult; stdcall;
  end;

///////////////////////////////////////////////////////////////////////////////
//
// Advanced features.
//
///////////////////////////////////////////////////////////////////////////////

  IWMWriterSink = interface;

  // The writer can be QI'd for this interface, which provides advanced writing
  // functionality.
  {$HPPEMIT 'typedef System::DelphiInterface<IWMWriterAdvanced> _di_IWMWriterAdvanced;'}
  {$EXTERNALSYM IWMWriterAdvanced}
  IWMWriterAdvanced = interface(IUnknown)
    ['{96406BE3-2B2B-11D3-B36B-00C04F6108FF}']
    (*** IWMWriterAdvanced methods ***)
    // Sinks are where the output ASF data goes.
    function GetSinkCount(out pcSinks: LongWord): HResult; stdcall;
    function GetSink(dwSinkNum: LongWord; out ppSink: IWMWriterSink): HResult; stdcall;
    function AddSink(pSink: IWMWriterSink): HResult; stdcall;
    function RemoveSink(pSink: IWMWriterSink): HResult; stdcall;
    // By default, the user provides samples to an input on the
    // IWMWriter interface, and the samples may be compressed, put
    // into a MEB stream, etc. However, the user can use this interface to
    // put the samples directly into the ASF, with no compression etc.
    function WriteStreamSample(wStreamNum: Word; cnsSampleTime: Int64;
                               msSampleSendTime: LongWord; cnsSampleDuration: Int64;
                               dwFlags: LongWord; pSample: INSSBuffer): HResult; stdcall;
    // The writer may be running in real-time. If so, it's interesting to
    // get the current time from the writer.
    function SetLiveSource(fIsLiveSource: BOOL): HResult; stdcall;
    function IsRealTime(out pfRealTime: BOOL): HResult; stdcall;
    function GetWriterTime(out pcnsCurrentTime: Int64): HResult; stdcall;
    // To get statistics, pass in a WM_WRITER_STATISTICS structure, which
    // will be filled out by the GetStatistics() call with the requested
    // stats.
    //
    // Pass in a stream number to get statistics for a specific stream, or
    // pass 0 to get statistics for the entire ASF file.
    function GetStatistics(wStreamNum: Word; out pStats: TWMWriterStatistics): HResult; stdcall;
    // Sync tolerance determines how far out of sync the inputs will be allowed
    // to get before samples are thrown away.  Default is 3000 ms.
    function SetSyncTolerance(msWindow: LongWord): HResult; stdcall;
    function GetSyncTolerance(out pmsWindow: LongWord): HResult; stdcall;
  end;

  // The writer can be QI'd for this interface, which provides advanced writing
  // functionality.
  {$HPPEMIT 'typedef System::DelphiInterface<IWMWriterAdvanced2> _di_IWMWriterAdvanced2;'}
  {$EXTERNALSYM IWMWriterAdvanced2}
  IWMWriterAdvanced2 = interface(IWMWriterAdvanced)
    ['{962DC1EC-C046-4DB8-9CC7-26CEAE500817}']
    (*** IWMWriterAdvanced2 methods ***)
    // Retrieves a setting for a particular output by name
    function GetInputSetting(dwInputNum: LongWord; pszName: PWideChar;
       out pType: TWMTAttrDataType; {out} pValue: PByte; var pcbLength: Word): HResult; stdcall;
    // Sets a named setting for a particular input
    function SetInputSetting(dwInputNum: LongWord; pszName: PWideChar; Type_: TWMTAttrDataType;
      pValue: PByte; cbLength: Word): HResult; stdcall;
  end;

  // The writer can be QI'd for this interface, which provides advanced writing
  // functionality.
  {$HPPEMIT 'typedef System::DelphiInterface<IWMWriterAdvanced3> _di_IWMWriterAdvanced3;'}
  {$EXTERNALSYM IWMWriterAdvanced3}
  IWMWriterAdvanced3 = interface(IWMWriterAdvanced2)
    ['{2CD6492D-7C37-4E76-9D3B-59261183A22E}']
    (*** IWMWriterAdvanced3 methods ***)
    // To get extended statistics, pass in a WM_WRITER_STATISTICS_EX structure, which
    // will be filled out by the GetStatisticsEx() call with the requested
    // stats.
    //
    // Pass in a stream number to get extended statistics for a specific stream, or
    // pass 0 to get extended statistics for the entire ASF file.
    function GetStatisticsEx(wStreamNum: Word; out pStats: TWMWriterStatisticsEx): HResult; stdcall;
    function SetNonBlocking: HResult; stdcall;
  end;

  // The writer can be QI'd for this interface, which provides multi-pass
  // encoding functionality.
  {$HPPEMIT 'typedef System::DelphiInterface<IWMWriterPreprocess> _di_IWMWriterPreprocess;'}
  {$EXTERNALSYM IWMWriterPreprocess}
  IWMWriterPreprocess = interface(IUnknown)
    ['{FC54A285-38C4-45B5-AA23-85B9F7CB424B}']
    (*** IWMWriterPreprocess methods ***)
    function GetMaxPreprocessingPasses(dwInputNum: LongWord; dwFlags: LongWord;
      out pdwMaxNumPasses: LongWord): HResult; stdcall;
    function SetNumPreprocessingPasses(dwInputNum: LongWord; dwFlags: LongWord;
      dwNumPasses: LongWord): HResult; stdcall;
    function BeginPreprocessingPass(dwInputNum: LongWord; dwFlags: LongWord): HResult; stdcall;
    function PreprocessSample(dwInputNum: LongWord; cnsSampleTime: Int64; dwFlags: LongWord;
      pSample: INSSBuffer): HResult; stdcall;
    function EndPreprocessingPass(dwInputNum: LongWord; dwFlags: LongWord): HResult; stdcall;
  end;

  // This is the interface that receives uncompressed samples from the writer
  // to preview (well, postview) what the codec is doing.
  {$HPPEMIT 'typedef System::DelphiInterface<IWMWriterPostViewCallback> _di_IWMWriterPostViewCallback;'}
  {$EXTERNALSYM IWMWriterPostViewCallback}
  IWMWriterPostViewCallback = interface(IWMStatusCallback)
    ['{D9D6549D-A193-4F24-B308-03123D9B7F8D}']
    (*** IWMWriterPostViewCallback methods ***)
    // cnsSampleDuration will be 0 for most media types.
    function OnPostViewSample(wStreamNumber: Word; cnsSampleTime: Int64;
                              cnsSampleDuration: Int64; dwFlags: LongWord;
                              pSample: INSSBuffer; pvContext: Pointer): HResult; stdcall;
    function AllocateForPostView(wStreamNum: Word; cbBuffer: LongWord; out ppBuffer: INSSBuffer;
                                 pvContext: Pointer): HResult; stdcall;
  end;

  // The writer can be QI'd for this interface, which provides advanced writing
  // functionality.
  {$HPPEMIT 'typedef System::DelphiInterface<IWMWriterPostView> _di_IWMWriterPostView;'}
  {$EXTERNALSYM IWMWriterPostView}
  IWMWriterPostView = interface(IUnknown)
    ['{81E20CE4-75EF-491A-8004-FC53C45BDC3E}']
    (*** IWMWriterPostView methods ***)
    // Specify the callback to use for PostView
    function SetPostViewCallback(pCallback: IWMWriterPostViewCallback; pvContext: Pointer): HResult; stdcall;
    // Turns on delivery of postview samples for a given stream
    function SetReceivePostViewSamples(wStreamNum: Word; fReceivePostViewSamples: BOOL): HResult; stdcall;
    function GetReceivePostViewSamples(wStreamNum: Word; out pfReceivePostViewSamples: BOOL): HResult; stdcall;
    // The user can enumerate through the various outputs, and get the
    // output format for that data.
    //
    // Manipulating the IWMOutputMediaProps has no effect on the output, unless
    // the user also calls SetOutputProps.
    function GetPostViewProps(wStreamNumber: Word; out ppOutput: IWMMediaProps): HResult; stdcall;
    function SetPostViewProps(wStreamNumber: Word; pOutput: IWMMediaProps): HResult; stdcall;
    // Used for determining all possible format types supported by this
    // output on the reader.
    function GetPostViewFormatCount(wStreamNumber: Word; out pcFormats: LongWord): HResult; stdcall;
    function GetPostViewFormat(wStreamNumber: Word; dwFormatNumber: LongWord;
                               out ppProps: IWMMediaProps): HResult; stdcall;
    // The user can register himself to provide buffers for any of the outputs
    // (for instance, DDraw buffers). The actual allocation is on the
    // IWMReaderCallbackAdvanced interface.
    function SetAllocateForPostView(wStreamNumber: Word; fAllocate: BOOL): HResult; stdcall;
    function GetAllocateForPostView(wStreamNumber: Word; out pfAllocate: BOOL): HResult; stdcall;
  end;

  // This is the interface that receives raw ASF from the writer.
  {$HPPEMIT 'typedef System::DelphiInterface<IWMWriterSink> _di_IWMWriterSink;'}
  {$EXTERNALSYM IWMWriterSink}
  IWMWriterSink = interface(IUnknown)
    ['{96406BE4-2B2B-11D3-B36B-00C04F6108FF}']
    (*** IWMWriterSink methods ***)
    function OnHeader(pHeader: INSSBuffer): HResult; stdcall;
    // Some sinks require that data be fed to them in real-time.
    function IsRealTime(out pfRealTime: BOOL): HResult; stdcall;
    function AllocateDataUnit(cbDataUnit: LongWord; out ppDataUnit: INSSBuffer): HResult; stdcall;
    function OnDataUnit(pDataUnit: INSSBuffer): HResult; stdcall;
    // This function is called when the writer is done sending data.
    function OnEndWriting: HResult; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMRegisterCallback> _di_IWMRegisterCallback;'}
  {$EXTERNALSYM IWMRegisterCallback}
  IWMRegisterCallback = interface(IUnknown)
    ['{CF4B1F99-4DE2-4E49-A363-252740D99BC1}']
    (*** IWMRegisterCallback methods ***)
    function Advise(pCallback: IWMStatusCallback; pvContext: Pointer): HResult; stdcall;
    function Unadvise(pCallback: IWMStatusCallback; pvContext: Pointer): HResult; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMWriterFileSink> _di_IWMWriterFileSink;'}
  {$EXTERNALSYM IWMWriterFileSink}
  IWMWriterFileSink = interface(IWMWriterSink)
    ['{96406BE5-2B2B-11D3-B36B-00C04F6108FF}']
    (*** IWMWriterFileSink methods ***)
    function Open(pwszFilename: PWideChar): HResult; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMWriterFileSink2> _di_IWMWriterFileSink2;'}
  {$EXTERNALSYM IWMWriterFileSink2}
  IWMWriterFileSink2 = interface(IWMWriterFileSink)
    ['{14282BA7-4AEF-4205-8CE5-C229035A05BC}']
    (*** IWMWriterFileSink2 methods ***)
    function Start(cnsStartTime: Int64): HResult; stdcall;
    function Stop(cnsStopTime: Int64): HResult; stdcall;
    function IsStopped(out pfStopped: BOOL): HResult; stdcall;
    function GetFileDuration(out pcnsDuration: Int64): HResult; stdcall;
    function GetFileSize(out pcbFile: Int64): HResult; stdcall;
    function Close: HResult; stdcall;
    function IsClosed(out pfClosed: BOOL): HResult; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMWriterFileSink3> _di_IWMWriterFileSink3;'}
  {$EXTERNALSYM IWMWriterFileSink3}
  IWMWriterFileSink3 = interface(IWMWriterFileSink2)
    ['{3FEA4FEB-2945-47A7-A1DD-C53A8FC4C45C}']
    (*** IWMWriterFileSink3 methods ***)
    function SetAutoIndexing(fDoAutoIndexing: BOOL): HResult; stdcall;
    function GetAutoIndexing(out pfAutoIndexing: BOOL): HResult; stdcall;
    function SetControlStream(wStreamNumber: Word; fShouldControlStartAndStop: BOOL): HResult; stdcall;
    function GetMode(out pdwFileSinkMode: LongWord): HResult; stdcall;
    function OnDataUnitEx(pFileSinkDataUnit: PWMTFileSinkDataUnit): HResult; stdcall;
    function SetUnbufferedIO(fUnbufferedIO: BOOL; fRestrictMemUsage: BOOL): HResult; stdcall;
    function GetUnbufferedIO(out pfUnbufferedIO: BOOL): HResult; stdcall;
    function CompleteOperations: HResult; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMWriterNetworkSink> _di_IWMWriterNetworkSink;'}
  {$EXTERNALSYM IWMWriterNetworkSink}
  IWMWriterNetworkSink = interface(IWMWriterSink)
    ['{96406BE7-2B2B-11D3-B36B-00C04F6108FF}']
    (*** IWMWriterNetworkSink methods ***)
    // Determine the maximum number of clients that can connect to this sink.
    // Default is 5.
    function SetMaximumClients(dwMaxClients: LongWord): HResult; stdcall;
    function GetMaximumClients(out pdwMaxClients: LongWord): HResult; stdcall;
    // The network protocol that the network sink will use.
    function SetNetworkProtocol(protocol: TWMTNetProtocol): HResult; stdcall;
    function GetNetworkProtocol(out pProtocol: TWMTNetProtocol): HResult; stdcall;
    // Find out the name of the URL on which we're broadcasting
    function GetHostURL({out} pwszURL: PWideChar; var pcchURL: LongWord): HResult; stdcall;
    // The method claims the network port number. Close the sink to release
    // the port.
    //
    // Specify 0 for the port number and the sink will select a port for
    // the user.
    function Open(var pdwPortNum: LongWord): HResult; stdcall;
    // Disconnect all connected clients.
    function Disconnect: HResult; stdcall;
    // Close and release the open port.
    function Close: HResult; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMClientConnections> _di_IWMClientConnections;'}
  {$EXTERNALSYM IWMClientConnections}
  IWMClientConnections = interface(IUnknown)
    ['{73C66010-A299-41DF-B1F0-CCF03B09C1C6}']
    (*** IWMClientConnections methods ***)
    // Determine the number of connected clients
    function GetClientCount(out pcClients: LongWord): HResult; stdcall;
    // Get information about a connected client
    function GetClientProperties(dwClientNum: LongWord; out pClientProperties: TWMClientProperties): HResult; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMClientConnections2> _di_IWMClientConnections2;'}
  {$EXTERNALSYM IWMClientConnections2}
  IWMClientConnections2 = interface(IWMClientConnections)
    ['{4091571E-4701-4593-BB3D-D5F5F0C74246}']
    (*** IWMClientConnections2 methods ***)
    // Get information about a connected client
    function GetClientInfo(dwClientNum: LongWord; {out} pwszNetworkAddress: PWideChar;
      var pcchNetworkAddress: LongWord; {out} pwszPort: PWideChar;
      var pcchPort: LongWord; {out} pwszDNSName: PWideChar;
      var pcchDNSName: LongWord): HResult; stdcall;
  end;

// The reader can be QI'd for this interface for advanced functionality.
  {$HPPEMIT 'typedef System::DelphiInterface<IWMReaderAdvanced> _di_IWMReaderAdvanced;'}
  {$EXTERNALSYM IWMReaderAdvanced}
  IWMReaderAdvanced = interface(IUnknown)
    ['{96406BEA-2B2B-11D3-B36B-00C04F6108FF}']
    (*** IWMReaderAdvanced methods ***)
    // The user may want to drive the clock himself, particularly if he wants
    // to source from a file faster-than-realtime. This call may fail if the
    // current source does not support user-driven clocks.
    //
    // The proper way for a user to drive the clock is for the user to call
    // DeliverTime, and then wait for the OnTime call on
    // IWMReaderCallbackAdvanced to reach the time he specified.
    function SetUserProvidedClock(fUserClock: BOOL): HResult; stdcall;
    function GetUserProvidedClock(out pfUserClock: BOOL): HResult; stdcall;
    function DeliverTime(cnsTime: Int64): HResult; stdcall;
    // The user can select streams manually, instead of relying on the
    // automatic bandwidth stream selection that the reader will
    // normally do. To figure out what streams are in this ASF and what their
    // numbers are, QI for IWMProfile.
    //
    // When SetManualStreamSelection( TRUE ) is called, all streams are
    // selected by default.
    function SetManualStreamSelection(fSelection: BOOL): HResult; stdcall;
    function GetManualStreamSelection(out pfSelection: BOOL): HResult; stdcall;
    function SetStreamsSelected(cStreamCount: Word; pwStreamNumbers: PWord;
                                pSelections: PWMTStreamSelection): HResult; stdcall;
    function GetStreamSelected(wStreamNum: Word; out pSelection: TWMTStreamSelection): HResult; stdcall;
    // The user can also choose to get callbacks when automatic stream
    // selection occurs.
    function SetReceiveSelectionCallbacks(fGetCallbacks: BOOL): HResult; stdcall;
    function GetReceiveSelectionCallbacks(out pfGetCallbacks: BOOL): HResult; stdcall;
    // The user can register himself to receive samples directly from the
    // ASF streams, rather than letting the Reader decompress them. Note that
    // to do this, the IWMReaderCallback (supplied by the user) must support
    // IWMReaderCallbackAdvanced.
    //
    // To get actual information about the contents of a stream, QI the
    // object for IWMProfile.
    function SetReceiveStreamSamples(wStreamNum: Word; fReceiveStreamSamples: BOOL): HResult; stdcall;
    function GetReceiveStreamSamples(wStreamNum: Word; out pfReceiveStreamSamples: BOOL): HResult; stdcall;
    // The user can register himself to provide buffers for any of the outputs
    // (for instance, DDraw buffers). The actual allocation is on the
    // IWMReaderCallbackAdvanced interface.
    function SetAllocateForOutput(dwOutputNum: LongWord; fAllocate: BOOL): HResult; stdcall;
    function GetAllocateForOutput(dwOutputNum: LongWord; out pfAllocate: BOOL): HResult; stdcall;
    function SetAllocateForStream(wStreamNum: Word; fAllocate: BOOL): HResult; stdcall;
    function GetAllocateForStream(dwSreamNum: Word; out pfAllocate: BOOL): HResult; stdcall;
    // Get statistics on demand
    function GetStatistics(var pStatistics: TWMReaderStatistics): HResult; stdcall;
    // Set client side information used for logging
    function SetClientInfo(pClientInfo: PWMReaderClientInfo): HResult; stdcall;
    // Get the maximum required buffer sizes that the SDK will allocate.
    // The first is for output buffers, the second for stream buffers.
    function GetMaxOutputSampleSize(dwOutput: LongWord; out pcbMax: LongWord): HResult; stdcall;
    function GetMaxStreamSampleSize(wStream: Word; out pcbMax: LongWord): HResult; stdcall;
    // Used to notify the reader that it's delivering data
    // too slowly to the client.  The reader will try to speed
    // up.
    function NotifyLateDelivery(cnsLateness: Int64): HResult; stdcall;
  end;

  // The reader can be QI'd for this interface for advanced functionality.
  {$HPPEMIT 'typedef System::DelphiInterface<IWMReaderAdvanced2> _di_IWMReaderAdvanced2;'}
  {$EXTERNALSYM IWMReaderAdvanced2}
  IWMReaderAdvanced2 = interface(IWMReaderAdvanced)
    ['{AE14A945-B90C-4D0D-9127-80D665F7D73E}']
    (*** IWMReaderAdvanced2 methods ***)
    // Set the play mode to WMT_PLAY_MODE_AUTOSELECT to allow the reader
    // to pick the mode. (This is the default). If you select a play mode that
    // is impossible for the requested URL, an error will be returned when
    // the URL is opened.
    function SetPlayMode(Mode: TWMTPlayMode): HResult; stdcall;
    // Get the current play mode.
    function GetPlayMode(out pMode: TWMTPlayMode): HResult; stdcall;
    // Between WMT_BUFFERING_START and WMT_BUFFERING_STOP this call will
    // return progress values. pdwPercent returns the percentage of buffering
    // that has completed, and pcnsBuffering returns the amount of buffering
    // remaining.
    function GetBufferProgress(out pdwPercent: LongWord; out pcnsBuffering: Int64): HResult; stdcall;
    // When the play mode is WMT_PLAY_MODE_DOWNLOAD, this call will return
    // progress values. pdwPercent returns the percentage of the download
    // that has completed, pqwBytesDownloaded returns the number of bytes
    // that have been downloaded, and pcnsDownload returns the amount of
    // downloading remaining.
    function GetDownloadProgress(out pdwPercent: LongWord; out pqwBytesDownloaded: Int64;
                                 out pcnsDownload: Int64): HResult; stdcall;
    // When saving a file, the operation may take awhile. Between
    // WMT_SAVEAS_START and WMT_SAVEAS_STOP, this call will return progress
    // values. pdwPercent returns the percentage of the save as that has
    // completed.
    function GetSaveAsProgress(out pdwPercent: LongWord): HResult; stdcall;
    // Save the current file. This only works for WMT_PLAY_MODE_DOWNLOAD.
    // This operation is asynchronous; WMT_SAVEAS_STOP indicates that the
    // save has completed. Closing the reader will abort a save operation
    // that has not completed.
    function SaveFileAs(pwszFilename: PWideChar): HResult; stdcall;
    // Returns the name of the protocol that is currently being used.
    // The protocol name is a URL scheme, such as "mmsu", "http", "file", etc.
    // Note, however, that the protocol name may differ from the URL scheme
    // that was specified in IWMReader::Open().
    // This method may return an empty string if the protocol name cannot be determined.
    function GetProtocolName({out} pwszProtocol: PWideChar; var pcchProtocol: LongWord): HResult; stdcall;
    // Same as IWMReader::Start, but uses a marker index instead of a time value.
    function StartAtMarker(wMarkerIndex: Word; cnsDuration: Int64; fRate: Single;
                           pvContext: Pointer): HResult; stdcall;
    // Retrieves a setting for a particular output by name
    function GetOutputSetting(dwOutputNum: LongWord; pszName: PWideChar;
                              out pType: TWMTAttrDataType; {out} pValue: PByte; var pcbLength: Word): HResult; stdcall;
    // Sets a named setting for a particular output
    function SetOutputSetting(dwOutputNum: LongWord; pszName: PWideChar; Type_: TWMTAttrDataType;
                              pValue: PByte; cbLength: Word): HResult; stdcall;
    // Used to begin prerolling the reader.  Call this method
    // when you know you're going to call Start soon and want
    // to start buffering data in advance.  cnsStart, cnsDuration
    // and fRate should be the same as the values you plan to pass
    // to Start in the future.
    function Preroll(cnsStart: Int64; cnsDuration: Int64; fRate: Single): HResult; stdcall;
    // Specifies whether the SDK should send the client's unique identifier
    // to the server when streaming.
    function SetLogClientID(fLogClientID: BOOL): HResult; stdcall;
    function GetLogClientID(out pfLogClientID: BOOL): HResult; stdcall;
    // This method requests that the Reader send WMT_BUFFERING_STOP as soon
    // as possible. The Reader will only honor the request if it is currently
    // buffering, i.e., it has sent a WMT_BUFFERING_START, but not yet sent the
    // corresponding WMT_BUFFERING_STOP.
    function StopBuffering: HResult; stdcall;
    // Same as IWMReader::Open but takes an IStream interface pointer instead
    // of an URL to be opened
    function OpenStream(const pStream: IStream; pCallback: IWMReaderCallback;
                        pvContext: Pointer): HResult; stdcall;
  end;

  // The reader can be QI'ed for this interface for frame access functionaly.
  {$HPPEMIT 'typedef System::DelphiInterface<IWMReaderAdvanced3> _di_IWMReaderAdvanced3;'}
  {$EXTERNALSYM IWMReaderAdvanced3}
  IWMReaderAdvanced3 = interface(IWMReaderAdvanced2)
    ['{5DC0674B-F04B-4A4E-9F2A-B1AFDE2C8100}']
    (*** IWMReaderAdvanced3 methods ***)
    // This method is used when you want to stop net streaming right away but
    // continue to receive samples that SDK have gotten so far.
    //
    // If it's successful, user should receive an END_OF_STREAMING quickly.
    function StopNetStreaming: HResult; stdcall;
    // This method supports extended start functionality. The currently
    // support start formats are:
    //
    // WMT_OFFSET_FORMAT_100NS
    // WMT_OFFSET_FORMAT_FRAME_NUMBERS
    // WMT_OFFSET_FORMAT_PLAYLIST_OFFSET
    // WMT_OFFSET_FORMAT_TIMECODE
    function StartAtPosition(wStreamNum: Word; pvOffsetStart: Pointer; pvDuration: Pointer;
                             dwOffsetFormat: TWMTOffsetFormat; fRate: Single;
                             pvContext: Pointer): HResult; stdcall;
  end;

  // The reader can be QI'ed for this interface for frame access functionaly.
  {$HPPEMIT 'typedef System::DelphiInterface<IWMReaderAdvanced4> _di_IWMReaderAdvanced4;'}
  {$EXTERNALSYM IWMReaderAdvanced4}
  IWMReaderAdvanced4 = interface(IWMReaderAdvanced3)
    ['{945A76A2-12AE-4D48-BD3C-CD1D90399B85}']
    (*** IWMReaderAdvanced4 methods ***)
    function GetLanguageCount(dwOutputNum: LongWord; out pwLanguageCount: Word): HResult; stdcall;
    function GetLanguage(dwOutputNum: LongWord; wLanguage: Word; {out} pwszLanguageString: PWideChar;
                         var pcchLanguageStringLength: Word): HResult; stdcall;
    function GetMaxSpeedFactor(out pdblFactor: Double): HResult; stdcall;
    function IsUsingFastCache(out pfUsingFastCache: BOOL): HResult; stdcall;
    function AddLogParam(wszNameSpace: PWideChar; wszName: PWideChar; wszValue: PWideChar): HResult; stdcall;
    function SendLogParams: HResult; stdcall;
    // Sets output parameter to TRUE if IWMReaderAdvanced2::SaveFileAs() is
    // can be invoked for the current content.
    function CanSaveFileAs(out pfCanSave: BOOL): HResult; stdcall;
    // Cancels the current FileSaveAs operation
    function CancelSaveFileAs: HResult; stdcall;
    // This method returns the URL currently being played.  This URL might be
    // different from the URL passed in to IWMReader::Open, because the Reader
    // may have been redirected to an alternate URL.
    function GetURL(pwszURL: PWideChar; var pcchURL: LongWord): HResult; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMReaderAllocatorEx> _di_IWMReaderAllocatorEx;'}
  {$EXTERNALSYM IWMReaderAllocatorEx}
  IWMReaderAllocatorEx = interface(IUnknown)
    ['{9F762FA7-A22E-428D-93C9-AC82F3AAFE5A}']
    (*** IWMReaderAllocatorEx methods ***)
    function AllocateForStreamEx(wStreamNum: Word; cbBuffer: LongWord; out ppBuffer: INSSBuffer;
                                 dwFlags: LongWord; cnsSampleTime: Int64;
                                 cnsSampleDuration: Int64; pvContext: Pointer): HResult; stdcall;
    function AllocateForOutputEx(dwOutputNum: LongWord; cbBuffer: LongWord;
                                 out ppBuffer: INSSBuffer; dwFlags: LongWord;
                                 cnsSampleTime: Int64; cnsSampleDuration: Int64;
                                 pvContext: Pointer): HResult; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMReaderTypeNegotiation> _di_IWMReaderTypeNegotiation;'}
  {$EXTERNALSYM IWMReaderTypeNegotiation}
  IWMReaderTypeNegotiation = interface(IUnknown)
    ['{FDBE5592-81A1-41EA-93BD-735CAD1ADC05}']
    (*** IWMReaderTypeNegotiation methods ***)
    function TryOutputProps(dwOutputNum: LongWord; pOutput: IWMOutputMediaProps): HResult; stdcall;
  end;

  // For some advanced functionality, the IWMReaderCallback must support this
  // interface.
  {$HPPEMIT 'typedef System::DelphiInterface<IWMReaderCallbackAdvanced> _di_IWMReaderCallbackAdvanced;'}
  {$EXTERNALSYM IWMReaderCallbackAdvanced}
  IWMReaderCallbackAdvanced = interface(IUnknown)
    ['{96406BEB-2B2B-11D3-B36B-00C04F6108FF}']
    (*** IWMReaderCallbackAdvanced methods ***)
    // Receive a sample directly from the ASF. To get this call, the user
    // must register himself to receive samples for a particular stream.
    function OnStreamSample(wStreamNum: Word; cnsSampleTime: Int64;
                            cnsSampleDuration: Int64; dwFlags: LongWord;
                            pSample: INSSBuffer; pvContext: Pointer): HResult; stdcall;
    // In some cases, the user may want to get callbacks telling what the
    // reader thinks the current time is. This is interesting in 2 cases:
    // - If the ASF has gaps in it; say no audio for 10 seconds. This call
    //   will continue to be called, while OnSample won't be called.
    // - If the user is driving the clock, the reader needs to communicate
    //   back to the user its time, to avoid the user overrunning the reader.
    function OnTime(cnsCurrentTime: Int64; pvContext: Pointer): HResult; stdcall;
    // The user can also get callbacks when stream selection occurs.
    function OnStreamSelection(wStreamCount: Word; pStreamNumbers: PWord;
                               pSelections: PWMTStreamSelection; pvContext: Pointer): HResult; stdcall;
    // Will be called if the user got an async result from their
    // call to SetOutputProps.  The next sample you receive for
    // this output will have these properties.  The contents of the
    // media type after calling SetOutputProps and before receiving
    // an OutputPropsChanged notification are undefined.
    function OnOutputPropsChanged(dwOutputNum: LongWord; pMediaType: PWMMediaType;
                                  pvContext: Pointer): HResult; stdcall;
    // If the user has registered to allocate buffers, this is where he must
    // do it.
    function AllocateForStream(wStreamNum: Word; cbBuffer: LongWord; out ppBuffer: INSSBuffer;
                               pvContext: Pointer): HResult; stdcall;
    function AllocateForOutput(dwOutputNum: LongWord; cbBuffer: LongWord; out ppBuffer: INSSBuffer;
                               pvContext: Pointer): HResult; stdcall;
  end;

  // Used to access advanced DRM functionality.
  {$HPPEMIT 'typedef System::DelphiInterface<IWMDRMReader> _di_IWMDRMReader;'}
  {$EXTERNALSYM IWMDRMReader}
  IWMDRMReader = interface(IUnknown)
    ['{D2827540-3EE7-432C-B14C-DC17F085D3B3}']
    (*** IWMDRMReader methods ***)
    function AcquireLicense(dwFlags: LongWord): HResult; stdcall;
    function CancelLicenseAcquisition: HResult; stdcall;
    function Individualize(dwFlags: LongWord): HResult; stdcall;
    function CancelIndividualization: HResult; stdcall;
    function MonitorLicenseAcquisition: HResult; stdcall;
    function CancelMonitorLicenseAcquisition: HResult; stdcall;
    function SetDRMProperty(pwstrName: PWideChar; dwType: TWMTAttrDataType; pValue: PByte;
                            cbLength: Word): HResult; stdcall;
    function GetDRMProperty(pwstrName: PWideChar; out pdwType: TWMTAttrDataType; pValue: PByte;
                            var pcbLength: Word): HResult; stdcall;
  end;

  // Used to configure the network.  This interface is implemented by
  // the IWMReader object.
  {$HPPEMIT 'typedef System::DelphiInterface<IWMReaderNetworkConfig> _di_IWMReaderNetworkConfig;'}
  {$EXTERNALSYM IWMReaderNetworkConfig}
  IWMReaderNetworkConfig = interface(IUnknown)
    ['{96406BEC-2B2B-11D3-B36B-00C04F6108FF}']
    (*** IWMReaderNetworkConfig methods ***)
    // Get and set the amount of time the network source will buffer
    // data before rendering it.
    function GetBufferingTime(out pcnsBufferingTime: Int64): HResult; stdcall;
    function SetBufferingTime(cnsBufferingTime: Int64): HResult; stdcall;
    // Returns the UDP port number ranges that will be used for receiving
    // data.  If no ranges are available, random UDP port numbers will be used.
    function GetUDPPortRanges({out} pRangeArray: PWMPortNumberRange; var pcRanges: LongWord): HResult; stdcall;
    // Sets the UDP port number ranges that can be used for receiving data.
    // If no ranges are specified, random UDP port numbers will be used.
    function SetUDPPortRanges({in} pRangeArray: PWMPortNumberRange; cRanges: LongWord): HResult; stdcall;
    // Proxy settings: Manual proxy, Autodetect, UseBrowser (only for HTTP), or No Proxy.
    function GetProxySettings(pwszProtocol: PWideChar; out pProxySetting: TWMTProxySettings): HResult; stdcall;
    function SetProxySettings(pwszProtocol: PWideChar; ProxySetting: TWMTProxySettings): HResult; stdcall;
    // The host to use as the proxy.
    function GetProxyHostName(pwszProtocol: PWideChar; {out} pwszHostName: PWideChar;
                              var pcchHostName: LongWord): HResult; stdcall;
    function SetProxyHostName(pwszProtocol: PWideChar; pwszHostName: PWideChar): HResult; stdcall;
    // The port to use as the proxy.
    function GetProxyPort(pwszProtocol: PWideChar; out pdwPort: LongWord): HResult; stdcall;
    function SetProxyPort(pwszProtocol: PWideChar; dwPort: LongWord): HResult; stdcall;
    // Get and set the proxy exception list.
    function GetProxyExceptionList(pwszProtocol: PWideChar; {out} pwszExceptionList: PWideChar;
                                   var pcchExceptionList: LongWord): HResult; stdcall;
    function SetProxyExceptionList(pwszProtocol: PWideChar; pwszExceptionList: PWideChar): HResult; stdcall;
    // Whether or not to bypass proxy for local hosts
    function GetProxyBypassForLocal(pwszProtocol: PWideChar; out pfBypassForLocal: BOOL): HResult; stdcall;
    function SetProxyBypassForLocal(pwszProtocol: PWideChar; fBypassForLocal: BOOL): HResult; stdcall;
    // Whether to force a wpad discovery on the next run
    function GetForceRerunAutoProxyDetection(out pfForceRerunDetection: BOOL): HResult; stdcall;
    function SetForceRerunAutoProxyDetection(fForceRerunDetection: BOOL): HResult; stdcall;
    // Whether or not to use multicast, http, tcp, or udp
    function GetEnableMulticast(out pfEnableMulticast: BOOL): HResult; stdcall;
    function SetEnableMulticast(fEnableMulticast: BOOL): HResult; stdcall;
    function GetEnableHTTP(out pfEnableHTTP: BOOL): HResult; stdcall;
    function SetEnableHTTP(fEnableHTTP: BOOL): HResult; stdcall;
    function GetEnableUDP(out pfEnableUDP: BOOL): HResult; stdcall;
    function SetEnableUDP(fEnableUDP: BOOL): HResult; stdcall;
    function GetEnableTCP(out pfEnableTCP: BOOL): HResult; stdcall;
    function SetEnableTCP(fEnableTCP: BOOL): HResult; stdcall;
    // Forgets automatic protocol detection settings and redetects next time.
    function ResetProtocolRollover: HResult; stdcall;
    // Return or set the client's link bandwidth in bps.  This is an optional
    // setting.  By default, the SDK will automatically detect its connection
    // bandwidth to the streaming media server.
    function GetConnectionBandwidth(out pdwConnectionBandwidth: LongWord): HResult; stdcall;
    function SetConnectionBandwidth(dwConnectionBandwidth: LongWord): HResult; stdcall;
    // Iterate through the network protocols supported by this reader
    function GetNumProtocolsSupported(out pcProtocols: LongWord): HResult; stdcall;
    function GetSupportedProtocolName(dwProtocolNum: LongWord; {out} pwszProtocolName: PWideChar;
                                      var pcchProtocolName: LongWord): HResult; stdcall;
    // Adds the specified pszUrl to the list of URL's to recieve logging data.
    // This list is in addition to the origin server.
    function AddLoggingUrl(pwszURL: PWideChar): HResult; stdcall;
    // Fills the buffer with the URL corresponding to index dwIndex.
    function GetLoggingUrl(dwIndex: LongWord; {out} pwszURL: PWideChar; var pcchURL: LongWord): HResult; stdcall;
    // Returns the number of URLs in the current list of logging URLs.
    function GetLoggingUrlCount(out pdwUrlCount: LongWord): HResult; stdcall;
    // Clears the list of logging URLs
    function ResetLoggingUrlList: HResult; stdcall;
  end;

  // Used to configure the network.  This interface is implemented by
  // the IWMReader object.
  {$HPPEMIT 'typedef System::DelphiInterface<IWMReaderNetworkConfig2> _di_IWMReaderNetworkConfig2;'}
  {$EXTERNALSYM IWMReaderNetworkConfig2}
  IWMReaderNetworkConfig2 = interface(IWMReaderNetworkConfig)
    ['{D979A853-042B-4050-8387-C939DB22013F}']
    (*** IWMReaderNetworkConfig2 methods ***)
    // If enabled, allows streaming content (WMT_PLAY_MODE_STREAMING)
    // to be cached locally.
    function GetEnableContentCaching(out pfEnableContentCaching: BOOL): HResult; stdcall;
    function SetEnableContentCaching(fEnableContentCaching: BOOL): HResult; stdcall;
    // If enabled, allows streaming content to be streamed at a rate higher
    // than the playback rate.  This feature requires ContentCaching to be
    // enabled as well.
    function GetEnableFastCache(out pfEnableFastCache: BOOL): HResult; stdcall;
    function SetEnableFastCache(fEnableFastCache: BOOL): HResult; stdcall;
    function GetAcceleratedStreamingDuration(out pcnsAccelDuration: Int64): HResult; stdcall;
    function SetAcceleratedStreamingDuration(cnsAccelDuration: Int64): HResult; stdcall;
    function GetAutoReconnectLimit(out pdwAutoReconnectLimit: LongWord): HResult; stdcall;
    function SetAutoReconnectLimit(dwAutoReconnectLimit: LongWord): HResult; stdcall;
    function GetEnableResends(out pfEnableResends: BOOL): HResult; stdcall;
    function SetEnableResends(fEnableResends: BOOL): HResult; stdcall;
    function GetEnableThinning(out pfEnableThinning: BOOL): HResult; stdcall;
    function SetEnableThinning(fEnableThinning: BOOL): HResult; stdcall;
    function GetMaxNetPacketSize(out pdwMaxNetPacketSize: LongWord): HResult; stdcall;
  end;

  // Used to configure the network.  This interface is implemented by
  // the IWMReader object.
  {$HPPEMIT 'typedef System::DelphiInterface<IWMReaderStreamClock> _di_IWMReaderStreamClock;'}
  {$EXTERNALSYM IWMReaderStreamClock}
  IWMReaderStreamClock = interface(IUnknown)
    ['{96406BED-2B2B-11D3-B36B-00C04F6108FF}']
    (*** IWMReaderStreamClock methods ***)
    // Get the current value of the stream clock
    function GetTime(pcnsNow: PInt64): HResult; stdcall;
    // Set or kill a timer.  All timers are automatically
    // killed whenever you stop the Reader.  When a timer
    // expires, you'll receive a WMT_TIMER OnStatus callback
    // with hr == S_OK, pValue = TimerId and pvContext == pvParam.
    function SetTimer(cnsWhen: Int64; pvParam: Pointer; out pdwTimerId: LongWord): HResult; stdcall;
    function KillTimer(dwTimerId: LongWord): HResult; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMIndexer> _di_IWMIndexer;'}
  {$EXTERNALSYM IWMIndexer}
  IWMIndexer = interface(IUnknown)
    ['{6D7CDC71-9888-11D3-8EDC-00C04F6109CF}']
    (*** IWMIndexer methods ***)
    // Start is an asynchronous call; it returns almost immediately and the user
    // should wait for appropriate OnStatus calls to be sent to the callback.
    function StartIndexing(pwszURL: PWideChar; pCallback: IWMStatusCallback;
                           pvContext: Pointer): HResult; stdcall;
    function Cancel: HResult; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMIndexer2> _di_IWMIndexer2;'}
  {$EXTERNALSYM IWMIndexer2}
  IWMIndexer2 = interface(IWMIndexer)
    ['{B70F1E42-6255-4DF0-A6B9-02B212D9E2BB}']
    (*** IWMIndexer2 methods ***)
    function Configure(wStreamNum: Word; nIndexerType: TWMTIndexType;
                       pvInterval: Pointer; pvIndexType: Pointer): HResult; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMLicenseBackup> _di_IWMLicenseBackup;'}
  {$EXTERNALSYM IWMLicenseBackup}
  IWMLicenseBackup = interface(IUnknown)
    ['{05E5AC9F-3FB6-4508-BB43-A4067BA1EBE8}']
    (*** IWMLicenseBackup methods ***)
    function BackupLicenses(dwFlags: LongWord; pCallback: IWMStatusCallback): HResult; stdcall;
    function CancelLicenseBackup: HResult; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMLicenseRestore> _di_IWMLicenseRestore;'}
  {$EXTERNALSYM IWMLicenseRestore}
  IWMLicenseRestore = interface(IUnknown)
    ['{C70B6334-A22E-4EFB-A245-15E65A004A13}']
    (*** IWMLicenseRestore methods ***)
    function RestoreLicenses(dwFlags: LongWord; pCallback: IWMStatusCallback): HResult; stdcall;
    function CancelLicenseRestore: HResult; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMBackupRestoreProps> _di_IWMBackupRestoreProps;'}
  {$EXTERNALSYM IWMBackupRestoreProps}
  IWMBackupRestoreProps = interface(IUnknown)
    ['{3C8E0DA6-996F-4FF3-A1AF-4838F9377E2E}']
    (*** IWMBackupRestoreProps methods ***)
    function GetPropCount(out pcProps: Word): HResult; stdcall;
    function GetPropByIndex(wIndex: Word; {out} pwszName: PWideChar; var pcchNameLen: Word;
                            out pType: TWMTAttrDataType; {out} pValue: PByte; var pcbLength: Word): HResult; stdcall;
    function GetPropByName(pszName: PWideChar; out pType: TWMTAttrDataType; {out} pValue: PByte;
                           var pcbLength: Word): HResult; stdcall;
    function SetProp(pszName: PWideChar; Type_: TWMTAttrDataType; pValue: PByte; cbLength: Word): HResult; stdcall;
    function RemoveProp(pcwszName: PWideChar): HResult; stdcall;
    function RemoveAllProps: HResult; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMCodecInfo> _di_IWMCodecInfo;'}
  {$EXTERNALSYM IWMCodecInfo}
  IWMCodecInfo = interface(IUnknown)
    ['{A970F41E-34DE-4A98-B3BA-E4B3CA7528F0}']
    (*** IWMCodecInfo methods ***)
    function GetCodecInfoCount(const guidType: TGUID; out pcCodecs: LongWord): HResult; stdcall;
    function GetCodecFormatCount(const guidType: TGUID; dwCodecIndex: LongWord; out pcFormat: LongWord): HResult; stdcall;
    function GetCodecFormat(const guidType: TGUID; dwCodecIndex: LongWord; dwFormatIndex: LongWord;
                            out ppIStreamConfig: IWMStreamConfig): HResult; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMCodecInfo2> _di_IWMCodecInfo2;'}
  {$EXTERNALSYM IWMCodecInfo2}
  IWMCodecInfo2 = interface(IWMCodecInfo)
    ['{AA65E273-B686-4056-91EC-DD768D4DF710}']
    (*** IWMCodecInfo2 methods ***)
    function GetCodecName(const guidType: TGUID; dwCodecIndex: LongWord; {out} wszName: PWideChar;
                          var pcchName: LongWord): HResult; stdcall;
    function GetCodecFormatDesc(const guidType: TGUID; dwCodecIndex: LongWord;
                                dwFormatIndex: LongWord; out ppIStreamConfig: IWMStreamConfig;
                                {out} wszDesc: PWideChar; var pcchDesc: LongWord): HResult; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMCodecInfo3> _di_IWMCodecInfo3;'}
  {$EXTERNALSYM IWMCodecInfo3}
  IWMCodecInfo3 = interface(IWMCodecInfo2)
    ['{7E51F487-4D93-4F98-8AB4-27D0565ADC51}']
    (*** IWMCodecInfo3 methods ***)
    function GetCodecFormatProp(const guidType: TGUID; dwCodecIndex: LongWord;
                                dwFormatIndex: LongWord; pszName: PWideChar; 
                                out pType: TWMTAttrDataType; {out} pValue: PByte;
                                var pdwSize: LongWord): HResult; stdcall;
    function GetCodecProp(const guidType: TGUID; dwCodecIndex: LongWord; pszName: PWideChar;
                          out pType: TWMTAttrDataType; {out} pValue: PByte; var pdwSize: LongWord): HResult; stdcall;
    function SetCodecEnumerationSetting(const guidType: TGUID; dwCodecIndex: LongWord;
                                        pszName: PWideChar; Type_: TWMTAttrDataType;
                                        {in} pValue: PByte; dwSize: LongWord): HResult; stdcall;
    function GetCodecEnumerationSetting(const guidType: TGUID; dwCodecIndex: LongWord;
                                        pszName: PWideChar; out pType: TWMTAttrDataType;
                                        {out} pValue: PByte; var pdwSize: LongWord): HResult; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMLanguageList> _di_IWMLanguageList;'}
  {$EXTERNALSYM IWMLanguageList}
  IWMLanguageList = interface(IUnknown)
    ['{DF683F00-2D49-4D8E-92B7-FB19F6A0DC57}']
    (*** IWMLanguageList methods ***)
    function GetLanguageCount(out pwCount: Word): HResult; stdcall;
    function GetLanguageDetails(wIndex: Word; {out} pwszLanguageString: PWideChar;
                                var pcchLanguageStringLength: Word): HResult; stdcall;
    function AddLanguageByRFC1766String(pwszLanguageString: PWideChar; out pwIndex: Word): HResult; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMWriterPushSink> _di_IWMWriterPushSink;'}
  {$EXTERNALSYM IWMWriterPushSink}
  IWMWriterPushSink = interface(IWMWriterSink)
    ['{DC10E6A5-072C-467D-BF57-6330A9DDE12A}']
    (*** IWMWriterPushSink methods ***)
    // Connect to a publishing point on the server.
    // If the publishing point does not exist, and pwszTemplateURL is
    // non-NULL, an attempt will be made to create a new publishing point based
    // on the template URL.
    function Connect(pwszURL: PWideChar; pwszTemplateURL: PWideChar; fAutoDestroy: BOOL): HResult; stdcall;
    // Terminate the control connection with the downstream server. The data path on the
    // downstream server remains active for 5 minutes after which it is cleaned up.
    function Disconnect: HResult; stdcall;
    // Gracefully end the push distribution session. This shuts down the data path on
    // the server for the publishing point.
    function EndSession: HResult; stdcall;
  end;

  // The writer can be QI'd for this interface to enumerate all of the
  // watermark DMOs installed on a system.
  {$HPPEMIT 'typedef System::DelphiInterface<IWMWatermarkInfo> _di_IWMWatermarkInfo;'}
  {$EXTERNALSYM IWMWatermarkInfo}
  IWMWatermarkInfo = interface(IUnknown)
    ['{6F497062-F2E2-4624-8EA7-9DD40D81FC8D}']
    (*** IWMWatermarkInfo methods ***)
    function GetWatermarkEntryCount(wmetType: TWMTWatermarkEntryType; out pdwCount: LongWord): HResult; stdcall;
    function GetWatermarkEntry(wmetType: TWMTWatermarkEntryType; dwEntryNum: LongWord;
                               out pEntry: TWMTWatermarkEntry): HResult; stdcall;
  end;

  // The Reader can be QI'ed for this interface for DirectX-VA support
  {$HPPEMIT 'typedef System::DelphiInterface<IWMReaderAccelerator> _di_IWMReaderAccelerator;'}
  {$EXTERNALSYM IWMReaderAccelerator}
  IWMReaderAccelerator = interface(IUnknown)
    ['{BDDC4D08-944D-4D52-A612-46C3FDA07DD4}']
    (*** IWMReaderAccelerator methods ***)
    function GetCodecInterface(dwOutputNum: LongWord; const riid: TGUID;
                               out ppvCodecInterface: Pointer): HResult; stdcall;
    function Notify(dwOutputNum: LongWord; pSubtype: PWMMediaType): HResult; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMReaderTimecode> _di_IWMReaderTimecode;'}
  {$EXTERNALSYM IWMReaderTimecode}
  IWMReaderTimecode = interface(IUnknown)
    ['{F369E2F0-E081-4FE6-8450-B810B2F410D1}']
    (*** IWMReaderTimecode methods ***)
    // Get the number of ranges of timecode data for a given stream in the file.
    // Returns 0 if there is no timecode index for the specified stream.
    function GetTimecodeRangeCount(wStreamNum: Word; out pwRangeCount: Word): HResult; stdcall;
    // Gets the first and last timecode available for a given range.
    function GetTimecodeRangeBounds(wStreamNum: Word; wRangeNum: Word;
                                    out pStartTimecode: LongWord; out pEndTimecode: LongWord): HResult; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMAddressAccess> _di_IWMAddressAccess;'}
  {$EXTERNALSYM IWMAddressAccess}
  IWMAddressAccess = interface(IUnknown)
    ['{BB3C6389-1633-4E92-AF14-9F3173BA39D0}']
    (*** IWMAddressAccess methods ***)
    // Determine the number of access list entries (by type)
    function GetAccessEntryCount(aeType: WM_AETYPE; out pcEntries: LongWord): HResult; stdcall;
    // Retrieve a single address access entry (by type)
    function GetAccessEntry(aeType: WM_AETYPE; dwEntryNum: LongWord;
                            out pAddrAccessEntry: TWMAddressAccessEntry): HResult; stdcall;
    // Add a single address access entry
    function AddAccessEntry(aeType: WM_AETYPE; pAddrAccessEntry: PWMAddressAccessEntry): HResult; stdcall;
    // Remove a single address access entry (by type)
    function RemoveAccessEntry(aeType: WM_AETYPE; dwEntryNum: LongWord): HResult; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMAddressAccess2> _di_IWMAddressAccess2;'}
  {$EXTERNALSYM IWMAddressAccess2}
  IWMAddressAccess2 = interface(IWMAddressAccess)
    ['{65A83FC2-3E98-4D4D-81B5-2A742886B33D}']
    (*** IWMAddressAccess2 methods ***)
    // Retrieve a single address access entry (by type)
    function GetAccessEntryEx(aeType: WM_AETYPE; dwEntryNum: LongWord;
                              out pbstrAddress: WideString; out pbstrMask: WideString): HResult; stdcall;
    // Add a single address access entry
    function AddAccessEntryEx(aeType: WM_AETYPE; bstrAddress: WideString;
                              bstrMask: WideString): HResult; stdcall;
  end;

  // Used to get "APIC" ID3v2 frames
  {$HPPEMIT 'typedef System::DelphiInterface<IWMImageInfo> _di_IWMImageInfo;'}
  {$EXTERNALSYM IWMImageInfo}
  IWMImageInfo = interface(IUnknown)
    ['{9F0AA3B6-7267-4D89-88F2-BA915AA5C4C6}']
    (*** IWMImageInfo methods ***)
    function GetImageCount(out pcImages: LongWord): HResult; stdcall;
    function GetImage(wIndex: LongWord; var pcchMIMEType: Word; {out} pwszMIMEType: PWideChar;
                      var pcchDescription: Word; {out} pwszDescription: PWideChar; out pImageType: Word;
                      var pcbImageData: LongWord; {out} pbImageData: PByte): HResult; stdcall;
  end;

//*@@@+++@@@@******************************************************************
//
// Microsoft Windows Media
// Copyright (C) Microsoft Corporation. All rights reserved.
//
//*@@@---@@@@******************************************************************
//

const
  IID_IConfigAsfWriter2       : TGUID = '{7989ccaa-53f0-44f0-884a-f3b03f6ae066}';
  {$EXTERNALSYM IID_IConfigAsfWriter2}
  IID_IConfigAsfWriter        : TGUID = '{45086030-F7E4-486a-B504-826BB5792A3B}';
  {$EXTERNALSYM IID_IConfigAsfWriter}

  IID_IAMWMBufferPass         : TGUID = '{6DD816D7-E740-4123-9E24-2444412644D8}';
  {$EXTERNALSYM IID_IAMWMBufferPass}
  IID_IAMWMBufferPassCallback : TGUID = '{B25B8372-D2D2-44b2-8653-1B8DAE332489}';
  {$EXTERNALSYM IID_IAMWMBufferPassCallback}

  EC_PREPROCESS_COMPLETE = $56;
  {$EXTERNALSYM EC_PREPROCESS_COMPLETE}

type
  IAMWMBufferPassCallback = interface;

  // Interface used to setup callback for app to set/get properties on WindowsMedia buffers that
  // are passed by the ASF reader and writer filters
  {$HPPEMIT 'typedef System::DelphiInterface<IAMWMBufferPass> _di_IAMWMBufferPass;'}
  {$EXTERNALSYM IAMWMBufferPass}
  IAMWMBufferPass = interface(IUnknown)
  ['{6DD816D7-E740-4123-9E24-2444412644D8}']
  (*** IAMWMBufferPass methods ***)
    // set the interface to use for callback notification
    function SetNotify(pCallback: IAMWMBufferPassCallback): HRESULT; stdcall;
  end;

  // Callback interface used to notify that a WindowMedia buffer is being passed. Typically implemented
  // by the app and called by the ASF writer and reader filters.
  {$HPPEMIT 'typedef System::DelphiInterface<IAMWMBufferPassCallback> _di_IAMWMBufferPassCallback;'}
  {$EXTERNALSYM IAMWMBufferPassCallback}
  IAMWMBufferPassCallback = interface(IUnknown)
  ['{B25B8372-D2D2-44b2-8653-1B8DAE332489}']
  (*** IAMWMBufferPassCallback methods ***)
    //  give the callback receiver a chance to examine
    //  (and act on) the INSSBuffer3 before passing on
    function Notify(pNSSBuffer3: INSSBuffer3; pPin: IPin;
      prtStart, prtEnd: PReferenceTime): HRESULT; stdcall;
  end;

  //  ASF Writer filter paramaters for IConfigAsfWriter2 interface
  _AM_ASFWRITERCONFIG_PARAM = (
  {$IFNDEF COMPILER6_UP}
    AM_CONFIGASFWRITER_PARAM_INVALID_0,
    AM_CONFIGASFWRITER_PARAM_AUTOINDEX,        // dynamic indexing, lParam1 boolean, lParam2
  {$ELSE}
    AM_CONFIGASFWRITER_PARAM_AUTOINDEX = 1,
  {$ENDIF}
    AM_CONFIGASFWRITER_PARAM_MULTIPASS,        // multipass encoding, app should be able
                                               // to handle EC_PREPROCESS_COMPLETE events
    AM_CONFIGASFWRITER_PARAM_DONTCOMPRESS      // configure writer to take audio and video input data
                                               // as-is, without any recompression, useful for repackaging
                                               // content in the ASF container
  );
  {$EXTERNALSYM _AM_ASFWRITERCONFIG_PARAM}
  TAMASFWriterConfigParam = _AM_ASFWRITERCONFIG_PARAM;

  // Interface to control the ASF writer (version 2)
  {$HPPEMIT 'typedef System::DelphiInterface<IConfigAsfWriter2> _di_IConfigAsfWriter2;'}
  {$EXTERNALSYM IConfigAsfWriter2}
  IConfigAsfWriter2 = interface(IConfigAsfWriter)
  ['{7989CCAA-53F0-44f0-884A-F3B03F6AE066}']
  (*** IConfigAsfWriter2 methods ***)
    // Helper method to allow caller to pass in a pin pointer and get back the associated
    // stream number. This is helpful when using WMF SDK interfaces directly to work with
    // stream-specific properties.
    function StreamNumFromPin(pPin: IPin; out pwStreamNum: WORD): HRESULT; stdcall;
    // Set value corresponding to the passed in parameter id
    function SetParam(dwParam: TAMASFWriterConfigParam; dwParam1, dwParam2 {not used, must be 0}: LongWord): HRESULT; stdcall;
    // Get value corresponding to the passed in parameter id
    function GetParam(dwParam: LongWord; out pdwParam1, pdwParam2 {not used, must be 0}: LongWord): HRESULT; stdcall;
    // Multipass encoding
    function ResetMultiPassState: HRESULT; stdcall;
  end;

  // Interface to control the ASF writer
  {$HPPEMIT 'typedef System::DelphiInterface<IConfigAsfWriter> _di_IConfigAsfWriter;'}
  {$EXTERNALSYM IConfigAsfWriter}
  IConfigAsfWriter = interface(IUnknown)
  ['{45086030-F7E4-486a-B504-826BB5792A3B}']
  (*** IConfigAsfWriter methods ***)
    // The user is expected to enumerate profiles using the wmsdk IWMProfileManager
    // method and then pass the desired profile index to the ASF Writer filter via this
    // method. The filter will then try to configure itself for the selected profile.
    //
    // NOTE: The following 2 XXXProfileId methods are obsolete and their use is not recommended.
    //       Once the WMF SDK added the notion of profile versions, their behavior became ambiguous.
    //       At the time of this release (Corona) the methods will assume use of the default profile
    //       version number and make no attempt to override that. For instance, in the Corona release
    //       version 9 profiles are assumed.
    //
    //       Instead, it is recommended that apps use the XXXProfile methods which take the IWMProfile*
    //       directly or the ProfileGuid methods which take a profile GUID.
    function ConfigureFilterUsingProfileId(dwProfileId: LongWord): HRESULT; stdcall;
    function GetCurrentProfileId(out pdwProfileId: LongWord): HRESULT; stdcall;

    //
    // configure using a pre-defined wmsdk profile guid
    //
    function ConfigureFilterUsingProfileGuid(const guidProfile: TGUID): HRESULT; stdcall;
    function GetCurrentProfileGuid(out pProfileGuid: TGUID): HRESULT; stdcall;

    //
    // Use these methods when a custom profile setup is preferred
    //
    function ConfigureFilterUsingProfile(pProfile: IWMProfile): HRESULT; stdcall;
    function GetCurrentProfile(out ppProfile: IWMProfile): HRESULT; stdcall;

    //
    // allow app to control whether or not to index file
    //
    function SetIndexMode(bIndexFile: BOOL): HRESULT; stdcall;
    function GetIndexMode(out pbIndexFile: BOOL): HRESULT; stdcall;
  end;

//    Microsoft Windows Media Technology
//    Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//  Module Name:
//      nserror.h
//
//  Abstract:
//      Definitions for Windows Media events.
//
//   Here are the commented error ranges for the Windows Media Technologies Group
//
//   LEGACY RANGES
//
//       0  -  199 = General NetShow errors
//     200  -  399 = NetShow error events
//     400  -  599 = NetShow monitor events
//     600  -  799 = NetShow IMmsAutoServer errors
//    1000  - 1199 = NetShow MCMADM errors
//
//
//   NEW RANGES
//
//    2000 -  2999 = ASF (defined in ASFERR.MC)
//    3000 -  3999 = Windows Media SDK
//    4000 -  4999 = Windows Media Player
//    5000 -  5999 = Windows Media Server
//    6000 -  6999 = Windows Media HTTP/RTSP result codes (defined in NETERROR.MC)
//    7000 -  7999 = Windows Media Tools
//    8000 -  8999 = Windows Media Content Discovery
//    9000 -  9999 = Windows Media Real Time Collaboration
//   10000 - 10999 = Windows Media Digital Rights Management
//   11000 - 11999 = Windows Media Setup
//   12000 - 12999 = Windows Media Networking
//   13000 - 13999 = Windows Media Client Media Services


//#define STATUS_SEVERITY(hr)  (((hr) >> 30) & 0x3)

/////////////////////////////////////////////////////////////////////////
//
// NETSHOW Success Events
//
/////////////////////////////////////////////////////////////////////////

//
//  Values are 32 bit values layed out as follows:
//
//   3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1
//   1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
//  +---+-+-+-----------------------+-------------------------------+
//  |Sev|C|R|     Facility          |               Code            |
//  +---+-+-+-----------------------+-------------------------------+
//
//  where
//
//      Sev - is the severity code
//
//          00 - Success
//          01 - Informational
//          10 - Warning
//          11 - Error
//
//      C - is the Customer code flag
//
//      R - is a reserved bit
//
//      Facility - is the facility code
//
//      Code - is the facility's status code
//
//
// Define the facility codes
//
const
  FACILITY_NS_WIN32               = $7;
  {$EXTERNALSYM FACILITY_NS_WIN32}
  FACILITY_NS                     = $D;
  {$EXTERNALSYM FACILITY_NS}


//
// Define the severity codes
//
  STATUS_SEVERITY_WARNING          = $2;
  {$EXTERNALSYM STATUS_SEVERITY_WARNING}
  STATUS_SEVERITY_SUCCESS          = $0;
  {$EXTERNALSYM STATUS_SEVERITY_SUCCESS}
  STATUS_SEVERITY_INFORMATIONAL    = $1;
  {$EXTERNALSYM STATUS_SEVERITY_INFORMATIONAL}
  STATUS_SEVERITY_ERROR            = $3;
  {$EXTERNALSYM STATUS_SEVERITY_ERROR}


//
// MessageId: NS_S_CALLPENDING
//
// MessageText:
//
//  The requested operation is pending completion.%0
//
  NS_S_CALLPENDING                 = HRESULT($000D0000);
  {$EXTERNALSYM NS_S_CALLPENDING}

//
// MessageId: NS_S_CALLABORTED
//
// MessageText:
//
//  The requested operation was aborted by the client.%0
  NS_S_CALLABORTED                 = HRESULT($000D0001);
  {$EXTERNALSYM NS_S_CALLABORTED}

//
// MessageId: NS_S_STREAM_TRUNCATED
//
// MessageText:
//
//  The stream was purposefully stopped before completion.%0
//
  NS_S_STREAM_TRUNCATED            = HRESULT($000D0002);
  {$EXTERNALSYM NS_S_STREAM_TRUNCATED}


/////////////////////////////////////////////////////////////////////////
//
// NETSHOW Warning Events
//
/////////////////////////////////////////////////////////////////////////

//
// MessageId: NS_W_SERVER_BANDWIDTH_LIMIT
//
// MessageText:
//
//  The maximum filebitrate value specified is greater than the server's configured maximum bandwidth.%0
//
  NS_W_SERVER_BANDWIDTH_LIMIT      = HRESULT($800D0003);
  {$EXTERNALSYM NS_W_SERVER_BANDWIDTH_LIMIT}

//
// MessageId: NS_W_FILE_BANDWIDTH_LIMIT
//
// MessageText:
//
//  The maximum bandwidth value specified is less than the maximum filebitrate.%0
//
  NS_W_FILE_BANDWIDTH_LIMIT        = HRESULT($800D0004);
  {$EXTERNALSYM NS_W_FILE_BANDWIDTH_LIMIT}


/////////////////////////////////////////////////////////////////////////
//
// NETSHOW Error Events
//
/////////////////////////////////////////////////////////////////////////

//
// MessageId: NS_E_NOCONNECTION
//
// MessageText:
//
//  There is no connection established with the Windows Media server. The operation failed.%0
//
  NS_E_NOCONNECTION                = HRESULT($C00D0005);
  {$EXTERNALSYM NS_E_NOCONNECTION}

//
// MessageId: NS_E_CANNOTCONNECT
//
// MessageText:
//
//  Unable to establish a connection to the server.%0
//
  NS_E_CANNOTCONNECT               = HRESULT($C00D0006);
  {$EXTERNALSYM NS_E_CANNOTCONNECT}

//
// MessageId: NS_E_CANNOTDESTROYTITLE
//
// MessageText:
//
//  Unable to destroy the title.%0
//
  NS_E_CANNOTDESTROYTITLE          = HRESULT($C00D0007);
  {$EXTERNALSYM NS_E_CANNOTDESTROYTITLE}

//
// MessageId: NS_E_CANNOTRENAMETITLE
//
// MessageText:
//
//  Unable to rename the title.%0
//
  NS_E_CANNOTRENAMETITLE           = HRESULT($C00D0008);
  {$EXTERNALSYM NS_E_CANNOTRENAMETITLE}

//
// MessageId: NS_E_CANNOTOFFLINEDISK
//
// MessageText:
//
//  Unable to offline disk.%0
//
  NS_E_CANNOTOFFLINEDISK           = HRESULT($C00D0009);
  {$EXTERNALSYM NS_E_CANNOTOFFLINEDISK}

//
// MessageId: NS_E_CANNOTONLINEDISK
//
// MessageText:
//
//  Unable to online disk.%0
//
  NS_E_CANNOTONLINEDISK            = HRESULT($C00D000A);
  {$EXTERNALSYM NS_E_CANNOTONLINEDISK}

//
// MessageId: NS_E_NOREGISTEREDWALKER
//
// MessageText:
//
//  There is no file parser registered for this type of file.%0
//
  NS_E_NOREGISTEREDWALKER          = HRESULT($C00D000B);
  {$EXTERNALSYM NS_E_NOREGISTEREDWALKER}

//
// MessageId: NS_E_NOFUNNEL
//
// MessageText:
//
//  There is no data connection established.%0
//
  NS_E_NOFUNNEL                    = HRESULT($C00D000C);
  {$EXTERNALSYM NS_E_NOFUNNEL}

//
// MessageId: NS_E_NO_LOCALPLAY
//
// MessageText:
//
//  Failed to load the local play DLL.%0
//
  NS_E_NO_LOCALPLAY                = HRESULT($C00D000D);
  {$EXTERNALSYM NS_E_NO_LOCALPLAY}

//
// MessageId: NS_E_NETWORK_BUSY
//
// MessageText:
//
//  The network is busy.%0
//
  NS_E_NETWORK_BUSY                = HRESULT($C00D000E);
  {$EXTERNALSYM NS_E_NETWORK_BUSY}

//
// MessageId: NS_E_TOO_MANY_SESS
//
// MessageText:
//
//  The server session limit was exceeded.%0
//
  NS_E_TOO_MANY_SESS               = HRESULT($C00D000F);
  {$EXTERNALSYM NS_E_TOO_MANY_SESS}

//
// MessageId: NS_E_ALREADY_CONNECTED
//
// MessageText:
//
//  The network connection already exists.%0
//
  NS_E_ALREADY_CONNECTED           = HRESULT($C00D0010);
  {$EXTERNALSYM NS_E_ALREADY_CONNECTED}

//
// MessageId: NS_E_INVALID_INDEX
//
// MessageText:
//
//  Index %1 is invalid.%0
//
  NS_E_INVALID_INDEX               = HRESULT($C00D0011);
  {$EXTERNALSYM NS_E_INVALID_INDEX}

//
// MessageId: NS_E_PROTOCOL_MISMATCH
//
// MessageText:
//
//  There is no protocol or protocol version supported by both the client and the server.%0
//
  NS_E_PROTOCOL_MISMATCH           = HRESULT($C00D0012);
  {$EXTERNALSYM NS_E_PROTOCOL_MISMATCH}

//
// MessageId: NS_E_TIMEOUT
//
// MessageText:
//
//  The server, a computer set up to offer multimedia content to other computers, could not handle your request for multimedia content in a timely manner.  Please try again later.%0
//
  NS_E_TIMEOUT                     = HRESULT($C00D0013);
  {$EXTERNALSYM NS_E_TIMEOUT}

//
// MessageId: NS_E_NET_WRITE
//
// MessageText:
//
//  Error writing to the network.%0
//
  NS_E_NET_WRITE                   = HRESULT($C00D0014);
  {$EXTERNALSYM NS_E_NET_WRITE}

//
// MessageId: NS_E_NET_READ
//
// MessageText:
//
//  Error reading from the network.%0
//
  NS_E_NET_READ                    = HRESULT($C00D0015);
  {$EXTERNALSYM NS_E_NET_READ}

//
// MessageId: NS_E_DISK_WRITE
//
// MessageText:
//
//  Error writing to a disk.%0
//
  NS_E_DISK_WRITE                  = HRESULT($C00D0016);
  {$EXTERNALSYM NS_E_DISK_WRITE}

//
// MessageId: NS_E_DISK_READ
//
// MessageText:
//
//  Error reading from a disk.%0
//
  NS_E_DISK_READ                   = HRESULT($C00D0017);
  {$EXTERNALSYM NS_E_DISK_READ}

//
// MessageId: NS_E_FILE_WRITE
//
// MessageText:
//
//  Error writing to a file.%0
//
  NS_E_FILE_WRITE                  = HRESULT($C00D0018);
  {$EXTERNALSYM NS_E_FILE_WRITE}

//
// MessageId: NS_E_FILE_READ
//
// MessageText:
//
//  Error reading from a file.%0
//
  NS_E_FILE_READ                   = HRESULT($C00D0019);
  {$EXTERNALSYM NS_E_FILE_READ}

//
// MessageId: NS_E_FILE_NOT_FOUND
//
// MessageText:
//
//  The system cannot find the file specified.%0
//
  NS_E_FILE_NOT_FOUND              = HRESULT($C00D001A);
  {$EXTERNALSYM NS_E_FILE_NOT_FOUND}

//
// MessageId: NS_E_FILE_EXISTS
//
// MessageText:
//
//  The file already exists.%0
//
  NS_E_FILE_EXISTS                 = HRESULT($C00D001B);
  {$EXTERNALSYM NS_E_FILE_EXISTS}

//
// MessageId: NS_E_INVALID_NAME
//
// MessageText:
//
//  The file name, directory name, or volume label syntax is incorrect.%0
//
  NS_E_INVALID_NAME                = HRESULT($C00D001C);
  {$EXTERNALSYM NS_E_INVALID_NAME}

//
// MessageId: NS_E_FILE_OPEN_FAILED
//
// MessageText:
//
//  Failed to open a file.%0
//
  NS_E_FILE_OPEN_FAILED            = HRESULT($C00D001D);
  {$EXTERNALSYM NS_E_FILE_OPEN_FAILED}

//
// MessageId: NS_E_FILE_ALLOCATION_FAILED
//
// MessageText:
//
//  Unable to allocate a file.%0
//
  NS_E_FILE_ALLOCATION_FAILED      = HRESULT($C00D001E);
  {$EXTERNALSYM NS_E_FILE_ALLOCATION_FAILED}

//
// MessageId: NS_E_FILE_INIT_FAILED
//
// MessageText:
//
//  Unable to initialize a file.%0
//
  NS_E_FILE_INIT_FAILED            = HRESULT($C00D001F);
  {$EXTERNALSYM NS_E_FILE_INIT_FAILED}

//
// MessageId: NS_E_FILE_PLAY_FAILED
//
// MessageText:
//
//  Unable to play a file.%0
//
  NS_E_FILE_PLAY_FAILED            = HRESULT($C00D0020);
  {$EXTERNALSYM NS_E_FILE_PLAY_FAILED}

//
// MessageId: NS_E_SET_DISK_UID_FAILED
//
// MessageText:
//
//  Could not set the disk UID.%0
//
  NS_E_SET_DISK_UID_FAILED         = HRESULT($C00D0021);
  {$EXTERNALSYM NS_E_SET_DISK_UID_FAILED}

//
// MessageId: NS_E_INDUCED
//
// MessageText:
//
//  An error was induced for testing purposes.%0
//
  NS_E_INDUCED                     = HRESULT($C00D0022);
  {$EXTERNALSYM NS_E_INDUCED}

//
// MessageId: NS_E_CCLINK_DOWN
//
// MessageText:
//
//  Two Content Servers failed to communicate.%0
//
  NS_E_CCLINK_DOWN                 = HRESULT($C00D0023);
  {$EXTERNALSYM NS_E_CCLINK_DOWN}

//
// MessageId: NS_E_INTERNAL
//
// MessageText:
//
//  An unknown error occurred.%0
//
  NS_E_INTERNAL                    = HRESULT($C00D0024);
  {$EXTERNALSYM NS_E_INTERNAL}

//
// MessageId: NS_E_BUSY
//
// MessageText:
//
//  The requested resource is in use.%0
//
  NS_E_BUSY                        = HRESULT($C00D0025);
  {$EXTERNALSYM NS_E_BUSY}

//
// MessageId: NS_E_UNRECOGNIZED_STREAM_TYPE
//
// MessageText:
//
//  The specified protocol is not recognized. Be sure that the file name and syntax, such as slashes, are correct for the protocol.%0
//
  NS_E_UNRECOGNIZED_STREAM_TYPE    = HRESULT($C00D0026);
  {$EXTERNALSYM NS_E_UNRECOGNIZED_STREAM_TYPE}

//
// MessageId: NS_E_NETWORK_SERVICE_FAILURE
//
// MessageText:
//
//  The network service provider failed.%0
//
  NS_E_NETWORK_SERVICE_FAILURE     = HRESULT($C00D0027);
  {$EXTERNALSYM NS_E_NETWORK_SERVICE_FAILURE}

//
// MessageId: NS_E_NETWORK_RESOURCE_FAILURE
//
// MessageText:
//
//  An attempt to acquire a network resource failed.%0
//
  NS_E_NETWORK_RESOURCE_FAILURE    = HRESULT($C00D0028);
  {$EXTERNALSYM NS_E_NETWORK_RESOURCE_FAILURE}

//
// MessageId: NS_E_CONNECTION_FAILURE
//
// MessageText:
//
//  The network connection has failed.%0
//
  NS_E_CONNECTION_FAILURE          = HRESULT($C00D0029);
  {$EXTERNALSYM NS_E_CONNECTION_FAILURE}

//
// MessageId: NS_E_SHUTDOWN
//
// MessageText:
//
//  The session is being terminated locally.%0
//
  NS_E_SHUTDOWN                    = HRESULT($C00D002A);
  {$EXTERNALSYM NS_E_SHUTDOWN}

//
// MessageId: NS_E_INVALID_REQUEST
//
// MessageText:
//
//  The request is invalid in the current state.%0
//
  NS_E_INVALID_REQUEST             = HRESULT($C00D002B);
  {$EXTERNALSYM NS_E_INVALID_REQUEST}

//
// MessageId: NS_E_INSUFFICIENT_BANDWIDTH
//
// MessageText:
//
//  There is insufficient bandwidth available to fulfill the request.%0
//
  NS_E_INSUFFICIENT_BANDWIDTH      = HRESULT($C00D002C);
  {$EXTERNALSYM NS_E_INSUFFICIENT_BANDWIDTH}

//
// MessageId: NS_E_NOT_REBUILDING
//
// MessageText:
//
//  The disk is not rebuilding.%0
//
  NS_E_NOT_REBUILDING              = HRESULT($C00D002D);
  {$EXTERNALSYM NS_E_NOT_REBUILDING}

//
// MessageId: NS_E_LATE_OPERATION
//
// MessageText:
//
//  An operation requested for a particular time could not be carried out on schedule.%0
//
  NS_E_LATE_OPERATION              = HRESULT($C00D002E);
  {$EXTERNALSYM NS_E_LATE_OPERATION}

//
// MessageId: NS_E_INVALID_DATA
//
// MessageText:
//
//  Invalid or corrupt data was encountered.%0
//
  NS_E_INVALID_DATA                = HRESULT($C00D002F);
  {$EXTERNALSYM NS_E_INVALID_DATA}

//
// MessageId: NS_E_FILE_BANDWIDTH_LIMIT
//
// MessageText:
//
//  The bandwidth required to stream a file is higher than the maximum file bandwidth allowed on the server.%0
//
  NS_E_FILE_BANDWIDTH_LIMIT        = HRESULT($C00D0030);
  {$EXTERNALSYM NS_E_FILE_BANDWIDTH_LIMIT}

//
// MessageId: NS_E_OPEN_FILE_LIMIT
//
// MessageText:
//
//  The client cannot have any more files open simultaneously.%0
//
  NS_E_OPEN_FILE_LIMIT             = HRESULT($C00D0031);
  {$EXTERNALSYM NS_E_OPEN_FILE_LIMIT}

//
// MessageId: NS_E_BAD_CONTROL_DATA
//
// MessageText:
//
//  The server received invalid data from the client on the control connection.%0
//
  NS_E_BAD_CONTROL_DATA            = HRESULT($C00D0032);
  {$EXTERNALSYM NS_E_BAD_CONTROL_DATA}

//
// MessageId: NS_E_NO_STREAM
//
// MessageText:
//
//  There is no stream available.%0
//
  NS_E_NO_STREAM                   = HRESULT($C00D0033);
  {$EXTERNALSYM NS_E_NO_STREAM}

//
// MessageId: NS_E_STREAM_END
//
// MessageText:
//
//  There is no more data in the stream.%0
//
  NS_E_STREAM_END                  = HRESULT($C00D0034);
  {$EXTERNALSYM NS_E_STREAM_END}

//
// MessageId: NS_E_SERVER_NOT_FOUND
//
// MessageText:
//
//  The specified server could not be found.%0
//
  NS_E_SERVER_NOT_FOUND            = HRESULT($C00D0035);
  {$EXTERNALSYM NS_E_SERVER_NOT_FOUND}

//
// MessageId: NS_E_DUPLICATE_NAME
//
// MessageText:
//
//  The specified name is already in use.
//
  NS_E_DUPLICATE_NAME              = HRESULT($C00D0036);
  {$EXTERNALSYM NS_E_DUPLICATE_NAME}

//
// MessageId: NS_E_DUPLICATE_ADDRESS
//
// MessageText:
//
//  The specified address is already in use.
//
  NS_E_DUPLICATE_ADDRESS           = HRESULT($C00D0037);
  {$EXTERNALSYM NS_E_DUPLICATE_ADDRESS}

//
// MessageId: NS_E_BAD_MULTICAST_ADDRESS
//
// MessageText:
//
//  The specified address is not a valid multicast address.
//
  NS_E_BAD_MULTICAST_ADDRESS       = HRESULT($C00D0038);
  {$EXTERNALSYM NS_E_BAD_MULTICAST_ADDRESS}

//
// MessageId: NS_E_BAD_ADAPTER_ADDRESS
//
// MessageText:
//
//  The specified adapter address is invalid.
//
  NS_E_BAD_ADAPTER_ADDRESS         = HRESULT($C00D0039);
  {$EXTERNALSYM NS_E_BAD_ADAPTER_ADDRESS}

//
// MessageId: NS_E_BAD_DELIVERY_MODE
//
// MessageText:
//
//  The specified delivery mode is invalid.
//
  NS_E_BAD_DELIVERY_MODE           = HRESULT($C00D003A);
  {$EXTERNALSYM NS_E_BAD_DELIVERY_MODE}

//
// MessageId: NS_E_INVALID_CHANNEL
//
// MessageText:
//
//  The specified station does not exist.
//
  NS_E_INVALID_CHANNEL             = HRESULT($C00D003B);
  {$EXTERNALSYM NS_E_INVALID_CHANNEL}

//
// MessageId: NS_E_INVALID_STREAM
//
// MessageText:
//
//  The specified stream does not exist.
//
  NS_E_INVALID_STREAM              = HRESULT($C00D003C);
  {$EXTERNALSYM NS_E_INVALID_STREAM}

//
// MessageId: NS_E_INVALID_ARCHIVE
//
// MessageText:
//
//  The specified archive could not be opened.
//
  NS_E_INVALID_ARCHIVE             = HRESULT($C00D003D);
  {$EXTERNALSYM NS_E_INVALID_ARCHIVE}

//
// MessageId: NS_E_NOTITLES
//
// MessageText:
//
//  The system cannot find any titles on the server.%0
//
  NS_E_NOTITLES                    = HRESULT($C00D003E);
  {$EXTERNALSYM NS_E_NOTITLES}

//
// MessageId: NS_E_INVALID_CLIENT
//
// MessageText:
//
//  The system cannot find the client specified.%0
//
  NS_E_INVALID_CLIENT              = HRESULT($C00D003F);
  {$EXTERNALSYM NS_E_INVALID_CLIENT}

//
// MessageId: NS_E_INVALID_BLACKHOLE_ADDRESS
//
// MessageText:
//
//  The Blackhole Address is not initialized.%0
//
  NS_E_INVALID_BLACKHOLE_ADDRESS   = HRESULT($C00D0040);
  {$EXTERNALSYM NS_E_INVALID_BLACKHOLE_ADDRESS}

//
// MessageId: NS_E_INCOMPATIBLE_FORMAT
//
// MessageText:
//
//  The station does not support the stream format.
//
  NS_E_INCOMPATIBLE_FORMAT         = HRESULT($C00D0041);
  {$EXTERNALSYM NS_E_INCOMPATIBLE_FORMAT}

//
// MessageId: NS_E_INVALID_KEY
//
// MessageText:
//
//  The specified key is not valid.
//
  NS_E_INVALID_KEY                 = HRESULT($C00D0042);
  {$EXTERNALSYM NS_E_INVALID_KEY}

//
// MessageId: NS_E_INVALID_PORT
//
// MessageText:
//
//  The specified port is not valid.
//
  NS_E_INVALID_PORT                = HRESULT($C00D0043);
  {$EXTERNALSYM NS_E_INVALID_PORT}

//
// MessageId: NS_E_INVALID_TTL
//
// MessageText:
//
//  The specified TTL is not valid.
//
  NS_E_INVALID_TTL                 = HRESULT($C00D0044);
  {$EXTERNALSYM NS_E_INVALID_TTL}

//
// MessageId: NS_E_STRIDE_REFUSED
//
// MessageText:
//
//  The request to fast forward or rewind could not be fulfilled.
//
  NS_E_STRIDE_REFUSED              = HRESULT($C00D0045);
  {$EXTERNALSYM NS_E_STRIDE_REFUSED}

//
// IMmsAutoServer Errors
//
//
// MessageId: NS_E_MMSAUTOSERVER_CANTFINDWALKER
//
// MessageText:
//
//  Unable to load the appropriate file parser.%0
//
  NS_E_MMSAUTOSERVER_CANTFINDWALKER = HRESULT($C00D0046);
  {$EXTERNALSYM NS_E_MMSAUTOSERVER_CANTFINDWALKER}

//
// MessageId: NS_E_MAX_BITRATE
//
// MessageText:
//
//  Cannot exceed the maximum bandwidth limit.%0
//
  NS_E_MAX_BITRATE                 = HRESULT($C00D0047);
  {$EXTERNALSYM NS_E_MAX_BITRATE}

//
// MessageId: NS_E_LOGFILEPERIOD
//
// MessageText:
//
//  Invalid value for LogFilePeriod.%0
//
  NS_E_LOGFILEPERIOD               = HRESULT($C00D0048);
  {$EXTERNALSYM NS_E_LOGFILEPERIOD}

//
// MessageId: NS_E_MAX_CLIENTS
//
// MessageText:
//
//  Cannot exceed the maximum client limit.%0
//  
//
  NS_E_MAX_CLIENTS                 = HRESULT($C00D0049);
  {$EXTERNALSYM NS_E_MAX_CLIENTS}

//
// MessageId: NS_E_LOG_FILE_SIZE
//
// MessageText:
//
//  The maximum log file size has been reached.%0
//  
//
  NS_E_LOG_FILE_SIZE               = HRESULT($C00D004A);
  {$EXTERNALSYM NS_E_LOG_FILE_SIZE}

//
// MessageId: NS_E_MAX_FILERATE
//
// MessageText:
//
//  Cannot exceed the maximum file rate.%0
//
  NS_E_MAX_FILERATE                = HRESULT($C00D004B);
  {$EXTERNALSYM NS_E_MAX_FILERATE}

//
// File Walker Errors
//
//
// MessageId: NS_E_WALKER_UNKNOWN
//
// MessageText:
//
//  Unknown file type.%0
//
  NS_E_WALKER_UNKNOWN              = HRESULT($C00D004C);
  {$EXTERNALSYM NS_E_WALKER_UNKNOWN}

//
// MessageId: NS_E_WALKER_SERVER
//
// MessageText:
//
//  The specified file, %1, cannot be loaded onto the specified server, %2.%0
//
  NS_E_WALKER_SERVER               = HRESULT($C00D004D);
  {$EXTERNALSYM NS_E_WALKER_SERVER}

//
// MessageId: NS_E_WALKER_USAGE
//
// MessageText:
//
//  There was a usage error with file parser.%0
//
  NS_E_WALKER_USAGE                = HRESULT($C00D004E);
  {$EXTERNALSYM NS_E_WALKER_USAGE}


/////////////////////////////////////////////////////////////////////////
//
// NETSHOW Monitor Events
//
/////////////////////////////////////////////////////////////////////////


 // Tiger Events

 // %1 is the tiger name

//
// MessageId: NS_I_TIGER_START
//
// MessageText:
//
//  The Title Server %1 is running.%0
//
  NS_I_TIGER_START                 = HRESULT($400D004F);
  {$EXTERNALSYM NS_I_TIGER_START}

//
// MessageId: NS_E_TIGER_FAIL
//
// MessageText:
//
//  The Title Server %1 has failed.%0
//
  NS_E_TIGER_FAIL                  = HRESULT($C00D0050);
  {$EXTERNALSYM NS_E_TIGER_FAIL}


 // Cub Events

 // %1 is the cub ID
 // %2 is the cub name

//
// MessageId: NS_I_CUB_START
//
// MessageText:
//
//  Content Server %1 (%2) is starting.%0
//
  NS_I_CUB_START                   = HRESULT($400D0051);
  {$EXTERNALSYM NS_I_CUB_START}

//
// MessageId: NS_I_CUB_RUNNING
//
// MessageText:
//
//  Content Server %1 (%2) is running.%0
//
  NS_I_CUB_RUNNING                 = HRESULT($400D0052);
  {$EXTERNALSYM NS_I_CUB_RUNNING}

//
// MessageId: NS_E_CUB_FAIL
//
// MessageText:
//
//  Content Server %1 (%2) has failed.%0
//
  NS_E_CUB_FAIL                    = HRESULT($C00D0053);
  {$EXTERNALSYM NS_E_CUB_FAIL}


 // Disk Events

 // %1 is the tiger disk ID
 // %2 is the device name
 // %3 is the cub ID
//
// MessageId: NS_I_DISK_START
//
// MessageText:
//
//  Disk %1 ( %2 ) on Content Server %3, is running.%0
//
  NS_I_DISK_START                  = HRESULT($400D0054);
  {$EXTERNALSYM NS_I_DISK_START}

//
// MessageId: NS_E_DISK_FAIL
//
// MessageText:
//
//  Disk %1 ( %2 ) on Content Server %3, has failed.%0
//
  NS_E_DISK_FAIL                   = HRESULT($C00D0055);
  {$EXTERNALSYM NS_E_DISK_FAIL}

//
// MessageId: NS_I_DISK_REBUILD_STARTED
//
// MessageText:
//
//  Started rebuilding disk %1 ( %2 ) on Content Server %3.%0
//
  NS_I_DISK_REBUILD_STARTED        = HRESULT($400D0056);
  {$EXTERNALSYM NS_I_DISK_REBUILD_STARTED}

//
// MessageId: NS_I_DISK_REBUILD_FINISHED
//
// MessageText:
//
//  Finished rebuilding disk %1 ( %2 ) on Content Server %3.%0
//
  NS_I_DISK_REBUILD_FINISHED       = HRESULT($400D0057);
  {$EXTERNALSYM NS_I_DISK_REBUILD_FINISHED}

//
// MessageId: NS_I_DISK_REBUILD_ABORTED
//
// MessageText:
//
//  Aborted rebuilding disk %1 ( %2 ) on Content Server %3.%0
//
  NS_I_DISK_REBUILD_ABORTED        = HRESULT($400D0058);
  {$EXTERNALSYM NS_I_DISK_REBUILD_ABORTED}


 // Admin Events

//
// MessageId: NS_I_LIMIT_FUNNELS
//
// MessageText:
//
//  A NetShow administrator at network location %1 set the data stream limit to %2 streams.%0
//
  NS_I_LIMIT_FUNNELS               = HRESULT($400D0059);
  {$EXTERNALSYM NS_I_LIMIT_FUNNELS}

//
// MessageId: NS_I_START_DISK
//
// MessageText:
//
//  A NetShow administrator at network location %1 started disk %2.%0
//
  NS_I_START_DISK                  = HRESULT($400D005A);
  {$EXTERNALSYM NS_I_START_DISK}

//
// MessageId: NS_I_STOP_DISK
//
// MessageText:
//
//  A NetShow administrator at network location %1 stopped disk %2.%0
//
  NS_I_STOP_DISK                   = HRESULT($400D005B);
  {$EXTERNALSYM NS_I_STOP_DISK}

//
// MessageId: NS_I_STOP_CUB
//
// MessageText:
//
//  A NetShow administrator at network location %1 stopped Content Server %2.%0
//
  NS_I_STOP_CUB                    = HRESULT($400D005C);
  {$EXTERNALSYM NS_I_STOP_CUB}

//
// MessageId: NS_I_KILL_USERSESSION
//
// MessageText:
//
//  A NetShow administrator at network location %1 aborted user session %2 from the system.%0
//
  NS_I_KILL_USERSESSION            = HRESULT($400D005D);
  {$EXTERNALSYM NS_I_KILL_USERSESSION}

//
// MessageId: NS_I_KILL_CONNECTION
//
// MessageText:
//
//  A NetShow administrator at network location %1 aborted obsolete connection %2 from the system.%0
//
  NS_I_KILL_CONNECTION             = HRESULT($400D005E);
  {$EXTERNALSYM NS_I_KILL_CONNECTION}

//
// MessageId: NS_I_REBUILD_DISK
//
// MessageText:
//
//  A NetShow administrator at network location %1 started rebuilding disk %2.%0
//
  NS_I_REBUILD_DISK                = HRESULT($400D005F);
  {$EXTERNALSYM NS_I_REBUILD_DISK}

//
// MessageId: NS_W_UNKNOWN_EVENT
//
// MessageText:
//
//  Unknown %1 event encountered.%0
//
  NS_W_UNKNOWN_EVENT               = HRESULT($800D0060);
  {$EXTERNALSYM NS_W_UNKNOWN_EVENT}


 // Alerts

//
// MessageId: NS_E_MAX_FUNNELS_ALERT
//
// MessageText:
//
//  The NetShow data stream limit of %1 streams was reached.%0
//
  NS_E_MAX_FUNNELS_ALERT           = HRESULT($C00D0060);
  {$EXTERNALSYM NS_E_MAX_FUNNELS_ALERT}

//
// MessageId: NS_E_ALLOCATE_FILE_FAIL
//
// MessageText:
//
//  The NetShow Video Server was unable to allocate a %1 block file named %2.%0
//
  NS_E_ALLOCATE_FILE_FAIL          = HRESULT($C00D0061);
  {$EXTERNALSYM NS_E_ALLOCATE_FILE_FAIL}

//
// MessageId: NS_E_PAGING_ERROR
//
// MessageText:
//
//  A Content Server was unable to page a block.%0
//
  NS_E_PAGING_ERROR                = HRESULT($C00D0062);
  {$EXTERNALSYM NS_E_PAGING_ERROR}

//
// MessageId: NS_E_BAD_BLOCK0_VERSION
//
// MessageText:
//
//  Disk %1 has unrecognized control block version %2.%0
//
  NS_E_BAD_BLOCK0_VERSION          = HRESULT($C00D0063);
  {$EXTERNALSYM NS_E_BAD_BLOCK0_VERSION}

//
// MessageId: NS_E_BAD_DISK_UID
//
// MessageText:
//
//  Disk %1 has incorrect uid %2.%0
//
  NS_E_BAD_DISK_UID                = HRESULT($C00D0064);
  {$EXTERNALSYM NS_E_BAD_DISK_UID}

//
// MessageId: NS_E_BAD_FSMAJOR_VERSION
//
// MessageText:
//
//  Disk %1 has unsupported file system major version %2.%0
//
  NS_E_BAD_FSMAJOR_VERSION         = HRESULT($C00D0065);
  {$EXTERNALSYM NS_E_BAD_FSMAJOR_VERSION}

//
// MessageId: NS_E_BAD_STAMPNUMBER
//
// MessageText:
//
//  Disk %1 has bad stamp number in control block.%0
//
  NS_E_BAD_STAMPNUMBER             = HRESULT($C00D0066);
  {$EXTERNALSYM NS_E_BAD_STAMPNUMBER}

//
// MessageId: NS_E_PARTIALLY_REBUILT_DISK
//
// MessageText:
//
//  Disk %1 is partially reconstructed.%0
//
  NS_E_PARTIALLY_REBUILT_DISK      = HRESULT($C00D0067);
  {$EXTERNALSYM NS_E_PARTIALLY_REBUILT_DISK}

//
// MessageId: NS_E_ENACTPLAN_GIVEUP
//
// MessageText:
//
//  EnactPlan gives up.%0
//
  NS_E_ENACTPLAN_GIVEUP            = HRESULT($C00D0068);
  {$EXTERNALSYM NS_E_ENACTPLAN_GIVEUP}


 // MCMADM warnings/errors

//
// MessageId: MCMADM_I_NO_EVENTS
//
// MessageText:
//
//  Event initialization failed, there will be no MCM events.%0
//
  MCMADM_I_NO_EVENTS               = HRESULT($400D0069);
  {$EXTERNALSYM MCMADM_I_NO_EVENTS}

//
// MessageId: MCMADM_E_REGKEY_NOT_FOUND
//
// MessageText:
//
//  The key was not found in the registry.%0
//
  MCMADM_E_REGKEY_NOT_FOUND        = HRESULT($C00D006A);
  {$EXTERNALSYM MCMADM_E_REGKEY_NOT_FOUND}

//
// MessageId: NS_E_NO_FORMATS
//
// MessageText:
//
//  The publishing point cannot be started because the server does not have the appropriate stream formats. Use the Multicast Announcement Wizard to create a new announcement for this publishing point.%0
//
  NS_E_NO_FORMATS                  = HRESULT($C00D006B);
  {$EXTERNALSYM NS_E_NO_FORMATS}

//
// MessageId: NS_E_NO_REFERENCES
//
// MessageText:
//
//  No reference URLs were found in an ASX file.%0
//
  NS_E_NO_REFERENCES               = HRESULT($C00D006C);
  {$EXTERNALSYM NS_E_NO_REFERENCES}

//
// MessageId: NS_E_WAVE_OPEN
//
// MessageText:
//
//  Error opening wave device, the device might be in use.%0
//
  NS_E_WAVE_OPEN                   = HRESULT($C00D006D);
  {$EXTERNALSYM NS_E_WAVE_OPEN}

//
// MessageId: NS_I_LOGGING_FAILED
//
// MessageText:
//
//  The logging operation failed.
//
  NS_I_LOGGING_FAILED              = HRESULT($400D006E);
  {$EXTERNALSYM NS_I_LOGGING_FAILED}

//
// MessageId: NS_E_CANNOTCONNECTEVENTS
//
// MessageText:
//
//  Unable to establish a connection to the NetShow event monitor service.%0
//
  NS_E_CANNOTCONNECTEVENTS         = HRESULT($C00D006F);
  {$EXTERNALSYM NS_E_CANNOTCONNECTEVENTS}

//
// MessageId: NS_I_LIMIT_BANDWIDTH
//
// MessageText:
//
//  A NetShow administrator at network location %1 set the maximum bandwidth limit to %2 bps.%0
//
  NS_I_LIMIT_BANDWIDTH             = HRESULT($400D0070);
  {$EXTERNALSYM NS_I_LIMIT_BANDWIDTH}

//
// MessageId: NS_E_NO_DEVICE
//
// MessageText:
//
//  No device driver is present on the system.%0
//
  NS_E_NO_DEVICE                   = HRESULT($C00D0071);
  {$EXTERNALSYM NS_E_NO_DEVICE}

//
// MessageId: NS_E_NO_SPECIFIED_DEVICE
//
// MessageText:
//
//  No specified device driver is present.%0
//
  NS_E_NO_SPECIFIED_DEVICE         = HRESULT($C00D0072);
  {$EXTERNALSYM NS_E_NO_SPECIFIED_DEVICE}


// NOTENOTE!!!
//
// Due to legacy problems these error codes live inside the ASF error code range
//
//
// MessageId: NS_E_NOTHING_TO_DO
//
// MessageText:
//
//  NS_E_NOTHING_TO_DO
//
  NS_E_NOTHING_TO_DO               = HRESULT($C00D07F1);
  {$EXTERNALSYM NS_E_NOTHING_TO_DO}

//
// MessageId: NS_E_NO_MULTICAST
//
// MessageText:
//
//  Not receiving data from the server.%0
//
  NS_E_NO_MULTICAST                = HRESULT($C00D07F2);
  {$EXTERNALSYM NS_E_NO_MULTICAST}


/////////////////////////////////////////////////////////////////////////
//
// NETSHOW Error Events
//
// IdRange = 200..399
//
/////////////////////////////////////////////////////////////////////////

//
// MessageId: NS_E_MONITOR_GIVEUP
//
// MessageText:
//
//  Netshow Events Monitor is not operational and has been disconnected.%0
//
  NS_E_MONITOR_GIVEUP              = HRESULT($C00D00C8);
  {$EXTERNALSYM NS_E_MONITOR_GIVEUP}

//
// MessageId: NS_E_REMIRRORED_DISK
//
// MessageText:
//
//  Disk %1 is remirrored.%0
//
  NS_E_REMIRRORED_DISK             = HRESULT($C00D00C9);
  {$EXTERNALSYM NS_E_REMIRRORED_DISK}

//
// MessageId: NS_E_INSUFFICIENT_DATA
//
// MessageText:
//
//  Insufficient data found.%0
//
  NS_E_INSUFFICIENT_DATA           = HRESULT($C00D00CA);
  {$EXTERNALSYM NS_E_INSUFFICIENT_DATA}

//
// MessageId: NS_E_ASSERT
//
// MessageText:
//
//  %1 failed in file %2 line %3.%0
//
  NS_E_ASSERT                      = HRESULT($C00D00CB);
  {$EXTERNALSYM NS_E_ASSERT}

//
// MessageId: NS_E_BAD_ADAPTER_NAME
//
// MessageText:
//
//  The specified adapter name is invalid.%0
//
  NS_E_BAD_ADAPTER_NAME            = HRESULT($C00D00CC);
  {$EXTERNALSYM NS_E_BAD_ADAPTER_NAME}

//
// MessageId: NS_E_NOT_LICENSED
//
// MessageText:
//
//  The application is not licensed for this feature.%0
//
  NS_E_NOT_LICENSED                = HRESULT($C00D00CD);
  {$EXTERNALSYM NS_E_NOT_LICENSED}

//
// MessageId: NS_E_NO_SERVER_CONTACT
//
// MessageText:
//
//  Unable to contact the server.%0
//
  NS_E_NO_SERVER_CONTACT           = HRESULT($C00D00CE);
  {$EXTERNALSYM NS_E_NO_SERVER_CONTACT}

//
// MessageId: NS_E_TOO_MANY_TITLES
//
// MessageText:
//
//  Maximum number of titles exceeded.%0
//
  NS_E_TOO_MANY_TITLES             = HRESULT($C00D00CF);
  {$EXTERNALSYM NS_E_TOO_MANY_TITLES}

//
// MessageId: NS_E_TITLE_SIZE_EXCEEDED
//
// MessageText:
//
//  Maximum size of a title exceeded.%0
//
  NS_E_TITLE_SIZE_EXCEEDED         = HRESULT($C00D00D0);
  {$EXTERNALSYM NS_E_TITLE_SIZE_EXCEEDED}

//
// MessageId: NS_E_UDP_DISABLED
//
// MessageText:
//
//  UDP protocol not enabled. Not trying %1!ls!.%0
//
  NS_E_UDP_DISABLED                = HRESULT($C00D00D1);
  {$EXTERNALSYM NS_E_UDP_DISABLED}

//
// MessageId: NS_E_TCP_DISABLED
//
// MessageText:
//
//  TCP protocol not enabled. Not trying %1!ls!.%0
//
  NS_E_TCP_DISABLED                = HRESULT($C00D00D2);
  {$EXTERNALSYM NS_E_TCP_DISABLED}

//
// MessageId: NS_E_HTTP_DISABLED
//
// MessageText:
//
//  HTTP protocol not enabled. Not trying %1!ls!.%0
//
  NS_E_HTTP_DISABLED               = HRESULT($C00D00D3);
  {$EXTERNALSYM NS_E_HTTP_DISABLED}

//
// MessageId: NS_E_LICENSE_EXPIRED
//
// MessageText:
//
//  The product license has expired.%0
//
  NS_E_LICENSE_EXPIRED             = HRESULT($C00D00D4);
  {$EXTERNALSYM NS_E_LICENSE_EXPIRED}

//
// MessageId: NS_E_TITLE_BITRATE
//
// MessageText:
//
//  Source file exceeds the per title maximum bitrate. See NetShow Theater documentation for more information.%0
//
  NS_E_TITLE_BITRATE               = HRESULT($C00D00D5);
  {$EXTERNALSYM NS_E_TITLE_BITRATE}

//
// MessageId: NS_E_EMPTY_PROGRAM_NAME
//
// MessageText:
//
//  The program name cannot be empty.%0
//
  NS_E_EMPTY_PROGRAM_NAME          = HRESULT($C00D00D6);
  {$EXTERNALSYM NS_E_EMPTY_PROGRAM_NAME}

//
// MessageId: NS_E_MISSING_CHANNEL
//
// MessageText:
//
//  Station %1 does not exist.%0
//
  NS_E_MISSING_CHANNEL             = HRESULT($C00D00D7);
  {$EXTERNALSYM NS_E_MISSING_CHANNEL}

//
// MessageId: NS_E_NO_CHANNELS
//
// MessageText:
//
//  You need to define at least one station before this operation can complete.%0
//
  NS_E_NO_CHANNELS                 = HRESULT($C00D00D8);
  {$EXTERNALSYM NS_E_NO_CHANNELS}


/////////////////////////////////////////////////////////////////////
// This error message is to replace previous NS_E_INVALID_INDEX which
// takes an index value for the error message string.  For some application
// obtain the idex value at reporting error time is very difficult, so we
// use this string to avoid the problem.
//////////////////////////////////////////////////////////////////////

//
// MessageId: NS_E_INVALID_INDEX2
//
// MessageText:
//
//  The index specified is invalid.%0
//
  NS_E_INVALID_INDEX2              = HRESULT($C00D00D9);
  {$EXTERNALSYM NS_E_INVALID_INDEX2}


/////////////////////////////////////////////////////////////////////////
//
// NETSHOW Monitor Events
//
// IdRange = 400..599
//
// Admin Events:
//
// Alerts:
//
// Title Server:
//      %1 is the Title Server name
//
// Content Server:
//      %1 is the Content Server ID
//      %2 is the Content Server name
//      %3 is the Peer Content Server name (optiona);
//
// Disks:
//      %1 is the Title Server disk ID
//      %2 is the device name
//      %3 is the Content Server ID
//
/////////////////////////////////////////////////////////////////////////

//
// MessageId: NS_E_CUB_FAIL_LINK
//
// MessageText:
//
//  Content Server %1 (%2) has failed its link to Content Server %3.%0
//
  NS_E_CUB_FAIL_LINK               = HRESULT($C00D0190);
  {$EXTERNALSYM NS_E_CUB_FAIL_LINK}

//
// MessageId: NS_I_CUB_UNFAIL_LINK
//
// MessageText:
//
//  Content Server %1 (%2) has established its link to Content Server %3.%0
//
  NS_I_CUB_UNFAIL_LINK             = HRESULT($400D0191);
  {$EXTERNALSYM NS_I_CUB_UNFAIL_LINK}

//
// MessageId: NS_E_BAD_CUB_UID
//
// MessageText:
//
//  Content Server %1 (%2) has incorrect uid %3.%0
//
  NS_E_BAD_CUB_UID                 = HRESULT($C00D0192);
  {$EXTERNALSYM NS_E_BAD_CUB_UID}

//
// MessageId: NS_I_RESTRIPE_START
//
// MessageText:
//
//  Restripe operation has started.%0
//
  NS_I_RESTRIPE_START              = HRESULT($400D0193);
  {$EXTERNALSYM NS_I_RESTRIPE_START}

//
// MessageId: NS_I_RESTRIPE_DONE
//
// MessageText:
//
//  Restripe operation has completed.%0
//
  NS_I_RESTRIPE_DONE               = HRESULT($400D0194);
  {$EXTERNALSYM NS_I_RESTRIPE_DONE}

//
// MessageId: NS_E_GLITCH_MODE
//
// MessageText:
//
//  Server unreliable because multiple components failed.%0
//
  NS_E_GLITCH_MODE                 = HRESULT($C00D0195);
  {$EXTERNALSYM NS_E_GLITCH_MODE}

//
// MessageId: NS_I_RESTRIPE_DISK_OUT
//
// MessageText:
//
//  Content disk %1 (%2) on Content Server %3 has been restriped out.%0
//
  NS_I_RESTRIPE_DISK_OUT           = HRESULT($400D0196);
  {$EXTERNALSYM NS_I_RESTRIPE_DISK_OUT}

//
// MessageId: NS_I_RESTRIPE_CUB_OUT
//
// MessageText:
//
//  Content server %1 (%2) has been restriped out.%0
//
  NS_I_RESTRIPE_CUB_OUT            = HRESULT($400D0197);
  {$EXTERNALSYM NS_I_RESTRIPE_CUB_OUT}

//
// MessageId: NS_I_DISK_STOP
//
// MessageText:
//
//  Disk %1 ( %2 ) on Content Server %3, has been offlined.%0
//
  NS_I_DISK_STOP                   = HRESULT($400D0198);
  {$EXTERNALSYM NS_I_DISK_STOP}

//
// MessageId: NS_I_CATATONIC_FAILURE
//
// MessageText:
//
//  Disk %1 ( %2 ) on Content Server %3, will be failed because it is catatonic.%0
//
  NS_I_CATATONIC_FAILURE           = HRESULT($800D0199);
  {$EXTERNALSYM NS_I_CATATONIC_FAILURE}

//
// MessageId: NS_I_CATATONIC_AUTO_UNFAIL
//
// MessageText:
//
//  Disk %1 ( %2 ) on Content Server %3, auto online from catatonic state.%0
//
  NS_I_CATATONIC_AUTO_UNFAIL       = HRESULT($800D019A);
  {$EXTERNALSYM NS_I_CATATONIC_AUTO_UNFAIL}

//
// MessageId: NS_E_NO_MEDIA_PROTOCOL
//
// MessageText:
//
//  Content Server %1 (%2) is unable to communicate with the Media System Network Protocol.%0
//
  NS_E_NO_MEDIA_PROTOCOL           = HRESULT($C00D019B);
  {$EXTERNALSYM NS_E_NO_MEDIA_PROTOCOL}


//
// Advanced Streaming Format (ASF) codes occupy MessageIds 2000-2999
//
// See ASFErr.mc for more details - please do not define any symbols
// in that range in this file.
//


/////////////////////////////////////////////////////////////////////////
//
// Windows Media SDK Errors
//
// IdRange = 3000-3199
//
/////////////////////////////////////////////////////////////////////////

//
// MessageId: NS_E_INVALID_INPUT_FORMAT
//
// MessageText:
//
//  The input media format is invalid.%0
//
  NS_E_INVALID_INPUT_FORMAT        = HRESULT($C00D0BB8);
  {$EXTERNALSYM NS_E_INVALID_INPUT_FORMAT}

//
// MessageId: NS_E_MSAUDIO_NOT_INSTALLED
//
// MessageText:
//
//  The MSAudio codec is not installed on this system.%0
//
  NS_E_MSAUDIO_NOT_INSTALLED       = HRESULT($C00D0BB9);
  {$EXTERNALSYM NS_E_MSAUDIO_NOT_INSTALLED}

//
// MessageId: NS_E_UNEXPECTED_MSAUDIO_ERROR
//
// MessageText:
//
//  An unexpected error occurred with the MSAudio codec.%0
//
  NS_E_UNEXPECTED_MSAUDIO_ERROR    = HRESULT($C00D0BBA);
  {$EXTERNALSYM NS_E_UNEXPECTED_MSAUDIO_ERROR}

//
// MessageId: NS_E_INVALID_OUTPUT_FORMAT
//
// MessageText:
//
//  The output media format is invalid.%0
//
  NS_E_INVALID_OUTPUT_FORMAT       = HRESULT($C00D0BBB);
  {$EXTERNALSYM NS_E_INVALID_OUTPUT_FORMAT}

//
// MessageId: NS_E_NOT_CONFIGURED
//
// MessageText:
//
//  The object must be fully configured before audio samples can be processed.%0
//
  NS_E_NOT_CONFIGURED              = HRESULT($C00D0BBC);
  {$EXTERNALSYM NS_E_NOT_CONFIGURED}

//
// MessageId: NS_E_PROTECTED_CONTENT
//
// MessageText:
//
//  You need a license to perform the requested operation on this media file.%0
//
  NS_E_PROTECTED_CONTENT           = HRESULT($C00D0BBD);
  {$EXTERNALSYM NS_E_PROTECTED_CONTENT}

//
// MessageId: NS_E_LICENSE_REQUIRED
//
// MessageText:
//
//  You need a license to perform the requested operation on this media file.%0
//
  NS_E_LICENSE_REQUIRED            = HRESULT($C00D0BBE);
  {$EXTERNALSYM NS_E_LICENSE_REQUIRED}

//
// MessageId: NS_E_TAMPERED_CONTENT
//
// MessageText:
//
//  This media file is corrupted or invalid. Contact the content provider for a new file.%0
//
  NS_E_TAMPERED_CONTENT            = HRESULT($C00D0BBF);
  {$EXTERNALSYM NS_E_TAMPERED_CONTENT}

//
// MessageId: NS_E_LICENSE_OUTOFDATE
//
// MessageText:
//
//  The license for this media file has expired. Get a new license or contact the content provider for further assistance.%0
//
  NS_E_LICENSE_OUTOFDATE           = HRESULT($C00D0BC0);
  {$EXTERNALSYM NS_E_LICENSE_OUTOFDATE}

//
// MessageId: NS_E_LICENSE_INCORRECT_RIGHTS
//
// MessageText:
//
//  You are not allowed to open this file. Contact the content provider for further assistance.%0
//
  NS_E_LICENSE_INCORRECT_RIGHTS    = HRESULT($C00D0BC1);
  {$EXTERNALSYM NS_E_LICENSE_INCORRECT_RIGHTS}

//
// MessageId: NS_E_AUDIO_CODEC_NOT_INSTALLED
//
// MessageText:
//
//  The requested audio codec is not installed on this system.%0
//
  NS_E_AUDIO_CODEC_NOT_INSTALLED   = HRESULT($C00D0BC2);
  {$EXTERNALSYM NS_E_AUDIO_CODEC_NOT_INSTALLED}

//
// MessageId: NS_E_AUDIO_CODEC_ERROR
//
// MessageText:
//
//  An unexpected error occurred with the audio codec.%0
//
  NS_E_AUDIO_CODEC_ERROR           = HRESULT($C00D0BC3);
  {$EXTERNALSYM NS_E_AUDIO_CODEC_ERROR}

//
// MessageId: NS_E_VIDEO_CODEC_NOT_INSTALLED
//
// MessageText:
//
//  The requested video codec is not installed on this system.%0
//
  NS_E_VIDEO_CODEC_NOT_INSTALLED   = HRESULT($C00D0BC4);
  {$EXTERNALSYM NS_E_VIDEO_CODEC_NOT_INSTALLED}

//
// MessageId: NS_E_VIDEO_CODEC_ERROR
//
// MessageText:
//
//  An unexpected error occurred with the video codec.%0
//
  NS_E_VIDEO_CODEC_ERROR           = HRESULT($C00D0BC5);
  {$EXTERNALSYM NS_E_VIDEO_CODEC_ERROR}

//
// MessageId: NS_E_INVALIDPROFILE
//
// MessageText:
//
//  The Profile is invalid.%0
//
  NS_E_INVALIDPROFILE              = HRESULT($C00D0BC6);
  {$EXTERNALSYM NS_E_INVALIDPROFILE}

//
// MessageId: NS_E_INCOMPATIBLE_VERSION
//
// MessageText:
//
//  A new version of the SDK is needed to play the requested content.%0
//
  NS_E_INCOMPATIBLE_VERSION        = HRESULT($C00D0BC7);
  {$EXTERNALSYM NS_E_INCOMPATIBLE_VERSION}

//
// MessageId: NS_S_REBUFFERING
//
// MessageText:
//
//  The requested operation has caused the source to rebuffer.%0
//
  NS_S_REBUFFERING                 = HRESULT($000D0BC8);
  {$EXTERNALSYM NS_S_REBUFFERING}

//
// MessageId: NS_S_DEGRADING_QUALITY
//
// MessageText:
//
//  The requested operation has caused the source to degrade codec quality.%0
//
  NS_S_DEGRADING_QUALITY           = HRESULT($000D0BC9);
  {$EXTERNALSYM NS_S_DEGRADING_QUALITY}

//
// MessageId: NS_E_OFFLINE_MODE
//
// MessageText:
//
//  The requested URL is not available in offline mode.%0
//
  NS_E_OFFLINE_MODE                = HRESULT($C00D0BCA);
  {$EXTERNALSYM NS_E_OFFLINE_MODE}

//
// MessageId: NS_E_NOT_CONNECTED
//
// MessageText:
//
//  The requested URL cannot be accessed because there is no network connection.%0
//
  NS_E_NOT_CONNECTED               = HRESULT($C00D0BCB);
  {$EXTERNALSYM NS_E_NOT_CONNECTED}

//
// MessageId: NS_E_TOO_MUCH_DATA
//
// MessageText:
//
//  The encoding process was unable to keep up with the amount of supplied data.%0
//
  NS_E_TOO_MUCH_DATA               = HRESULT($C00D0BCC);
  {$EXTERNALSYM NS_E_TOO_MUCH_DATA}

//
// MessageId: NS_E_UNSUPPORTED_PROPERTY
//
// MessageText:
//
//  The given property is not supported.%0
//
  NS_E_UNSUPPORTED_PROPERTY        = HRESULT($C00D0BCD);
  {$EXTERNALSYM NS_E_UNSUPPORTED_PROPERTY}

//
// MessageId: NS_E_8BIT_WAVE_UNSUPPORTED
//
// MessageText:
//
//  Windows Media Player cannot copy the files to the CD because they are 8-bit. Convert the files to 16-bit, 44-kHz stereo files by using Sound Recorder or another audio-processing program, and then try again.%0
//
  NS_E_8BIT_WAVE_UNSUPPORTED       = HRESULT($C00D0BCE);
  {$EXTERNALSYM NS_E_8BIT_WAVE_UNSUPPORTED}

//
// MessageId: NS_E_NO_MORE_SAMPLES
//
// MessageText:
//
//  There are no more samples in the current range.%0
//
  NS_E_NO_MORE_SAMPLES             = HRESULT($C00D0BCF);
  {$EXTERNALSYM NS_E_NO_MORE_SAMPLES}

//
// MessageId: NS_E_INVALID_SAMPLING_RATE
//
// MessageText:
//
//  The given sampling rate is invalid.%0
//
  NS_E_INVALID_SAMPLING_RATE       = HRESULT($C00D0BD0);
  {$EXTERNALSYM NS_E_INVALID_SAMPLING_RATE}

//
// MessageId: NS_E_MAX_PACKET_SIZE_TOO_SMALL
//
// MessageText:
//
//  The given maximum packet size is too small to accommodate this profile
//
  NS_E_MAX_PACKET_SIZE_TOO_SMALL   = HRESULT($C00D0BD1);
  {$EXTERNALSYM NS_E_MAX_PACKET_SIZE_TOO_SMALL}

//
// MessageId: NS_E_LATE_PACKET
//
// MessageText:
//
//  The packet arrived too late to be of use
//
  NS_E_LATE_PACKET                 = HRESULT($C00D0BD2);
  {$EXTERNALSYM NS_E_LATE_PACKET}

//
// MessageId: NS_E_DUPLICATE_PACKET
//
// MessageText:
//
//  The packet is a duplicate of one received before
//
  NS_E_DUPLICATE_PACKET            = HRESULT($C00D0BD3);
  {$EXTERNALSYM NS_E_DUPLICATE_PACKET}

//
// MessageId: NS_E_SDK_BUFFERTOOSMALL
//
// MessageText:
//
//  Supplied buffer is too small
//
  NS_E_SDK_BUFFERTOOSMALL          = HRESULT($C00D0BD4);
  {$EXTERNALSYM NS_E_SDK_BUFFERTOOSMALL}

//
// MessageId: NS_E_INVALID_NUM_PASSES
//
// MessageText:
//
//  The wrong number of preprocessing passes was used for the stream's output type
//
  NS_E_INVALID_NUM_PASSES          = HRESULT($C00D0BD5);
  {$EXTERNALSYM NS_E_INVALID_NUM_PASSES}

//
// MessageId: NS_E_ATTRIBUTE_READ_ONLY
//
// MessageText:
//
//  An attempt was made to add, modify, or delete a read only attribute
//
  NS_E_ATTRIBUTE_READ_ONLY         = HRESULT($C00D0BD6);
  {$EXTERNALSYM NS_E_ATTRIBUTE_READ_ONLY}

//
// MessageId: NS_E_ATTRIBUTE_NOT_ALLOWED
//
// MessageText:
//
//  An attempt was made to add attribute that is not allowed for the given media type
//
  NS_E_ATTRIBUTE_NOT_ALLOWED       = HRESULT($C00D0BD7);
  {$EXTERNALSYM NS_E_ATTRIBUTE_NOT_ALLOWED}

//
// MessageId: NS_E_INVALID_EDL
//
// MessageText:
//
//  The EDL provided is invalid
//
  NS_E_INVALID_EDL                 = HRESULT($C00D0BD8);
  {$EXTERNALSYM NS_E_INVALID_EDL}

//
// MessageId: NS_E_DATA_UNIT_EXTENSION_TOO_LARGE
//
// MessageText:
//
//  The Data Unit Extension data was too large to be used.%0
//
  NS_E_DATA_UNIT_EXTENSION_TOO_LARGE = HRESULT($C00D0BD9);
  {$EXTERNALSYM NS_E_DATA_UNIT_EXTENSION_TOO_LARGE}

//
// MessageId: NS_E_CODEC_DMO_ERROR
//
// MessageText:
//
//  An unexpected error occurred with a DMO codec.%0
//
  NS_E_CODEC_DMO_ERROR             = HRESULT($C00D0BDA);
  {$EXTERNALSYM NS_E_CODEC_DMO_ERROR}



/////////////////////////////////////////////////////////////////////////
//
// Windows Media Player Errors
//
// IdRange = 4000 - 4999
//
/////////////////////////////////////////////////////////////////////////

//
// WMP CD Filter Error codes
//
//
// MessageId: NS_E_NO_CD
//
// MessageText:
//
//  There is no CD in the CD-ROM drive. Insert a CD, and try again.%0
//
  NS_E_NO_CD                       = HRESULT($C00D0FA0);
  {$EXTERNALSYM NS_E_NO_CD}

//
// MessageId: NS_E_CANT_READ_DIGITAL
//
// MessageText:
//
//  Unable to perform digital reads on this compact disc drive.  Please try analog playback via the Tools Options menu.%0
//
  NS_E_CANT_READ_DIGITAL           = HRESULT($C00D0FA1);
  {$EXTERNALSYM NS_E_CANT_READ_DIGITAL}

//
// MessageId: NS_E_DEVICE_DISCONNECTED
//
// MessageText:
//
//  Windows Media Player no longer detects a connected portable device. Reconnect your portable device, and then try copying the file again.%0
//
  NS_E_DEVICE_DISCONNECTED         = HRESULT($C00D0FA2);
  {$EXTERNALSYM NS_E_DEVICE_DISCONNECTED}

//
// MessageId: NS_E_DEVICE_NOT_SUPPORT_FORMAT
//
// MessageText:
//
//  Windows Media Player cannot play the file. The portable device does not support the specified format.%0
//
  NS_E_DEVICE_NOT_SUPPORT_FORMAT   = HRESULT($C00D0FA3);
  {$EXTERNALSYM NS_E_DEVICE_NOT_SUPPORT_FORMAT}

//
// MessageId: NS_E_SLOW_READ_DIGITAL
//
// MessageText:
//
//  Windows Media Player encountered a problem while attempting to play the CD using digital playback. The Player has automatically switched the CD-ROM drive to analog playback. To switch back to digital CD playback, use the Devices tab.%0
//
  NS_E_SLOW_READ_DIGITAL           = HRESULT($C00D0FA4);
  {$EXTERNALSYM NS_E_SLOW_READ_DIGITAL}

//
// MessageId: NS_E_MIXER_INVALID_LINE
//
// MessageText:
//
//  An invalid line error occurred in the mixer.%0
//
  NS_E_MIXER_INVALID_LINE          = HRESULT($C00D0FA5);
  {$EXTERNALSYM NS_E_MIXER_INVALID_LINE}

//
// MessageId: NS_E_MIXER_INVALID_CONTROL
//
// MessageText:
//
//  An invalid control error occurred in the mixer.%0
//
  NS_E_MIXER_INVALID_CONTROL       = HRESULT($C00D0FA6);
  {$EXTERNALSYM NS_E_MIXER_INVALID_CONTROL}

//
// MessageId: NS_E_MIXER_INVALID_VALUE
//
// MessageText:
//
//  An invalid value error occurred in the mixer.%0
//
  NS_E_MIXER_INVALID_VALUE         = HRESULT($C00D0FA7);
  {$EXTERNALSYM NS_E_MIXER_INVALID_VALUE}

//
// MessageId: NS_E_MIXER_UNKNOWN_MMRESULT
//
// MessageText:
//
//  An unrecognized MMRESULT occurred in the mixer.%0
//
  NS_E_MIXER_UNKNOWN_MMRESULT      = HRESULT($C00D0FA8);
  {$EXTERNALSYM NS_E_MIXER_UNKNOWN_MMRESULT}

//
// MessageId: NS_E_USER_STOP
//
// MessageText:
//
//  User has stopped the operation.%0
//
  NS_E_USER_STOP                   = HRESULT($C00D0FA9);
  {$EXTERNALSYM NS_E_USER_STOP}

//
// MessageId: NS_E_MP3_FORMAT_NOT_FOUND
//
// MessageText:
//
//  Windows Media Player cannot copy the file because a compatible MP3 encoder is not installed on your computer. Install a compatible MP3 encoder, or choose a different format to copy to (such as Windows Media Audio).%0
//
  NS_E_MP3_FORMAT_NOT_FOUND        = HRESULT($C00D0FAA);
  {$EXTERNALSYM NS_E_MP3_FORMAT_NOT_FOUND}

//
// MessageId: NS_E_CD_READ_ERROR_NO_CORRECTION
//
// MessageText:
//
//  Windows Media Player cannot read the CD. It may contain flaws. Turn on error correction and try again.%0
//
  NS_E_CD_READ_ERROR_NO_CORRECTION = HRESULT($C00D0FAB);
  {$EXTERNALSYM NS_E_CD_READ_ERROR_NO_CORRECTION}

//
// MessageId: NS_E_CD_READ_ERROR
//
// MessageText:
//
//  Windows Media Player cannot read the CD. Be sure the CD is free of dirt and scratches and the CD-ROM drive is functioning properly.%0
//
  NS_E_CD_READ_ERROR               = HRESULT($C00D0FAC);
  {$EXTERNALSYM NS_E_CD_READ_ERROR}

//
// MessageId: NS_E_CD_SLOW_COPY
//
// MessageText:
//
//  To speed up the copy process, do not play CD tracks while copying.%0
//
  NS_E_CD_SLOW_COPY                = HRESULT($C00D0FAD);
  {$EXTERNALSYM NS_E_CD_SLOW_COPY}

//
// MessageId: NS_E_CD_COPYTO_CD
//
// MessageText:
//
//  Cannot copy directly from a CDROM to a CD drive.%0
//
  NS_E_CD_COPYTO_CD                = HRESULT($C00D0FAE);
  {$EXTERNALSYM NS_E_CD_COPYTO_CD}

//
// MessageId: NS_E_MIXER_NODRIVER
//
// MessageText:
//
//  Could not open a sound mixer driver.%0
//
  NS_E_MIXER_NODRIVER              = HRESULT($C00D0FAF);
  {$EXTERNALSYM NS_E_MIXER_NODRIVER}

//
// MessageId: NS_E_REDBOOK_ENABLED_WHILE_COPYING
//
// MessageText:
//
//  Windows Media Player has detected that a setting for the CD-ROM drive will cause audio CDs to copy incorrectly; no audio is copied. Change the CD-ROM drive setting in Device Manager, and then try again.%0
//
  NS_E_REDBOOK_ENABLED_WHILE_COPYING = HRESULT($C00D0FB0);
  {$EXTERNALSYM NS_E_REDBOOK_ENABLED_WHILE_COPYING}

//
// MessageId: NS_E_CD_REFRESH
//
// MessageText:
//
//  Trying to refresh the CD playlist.%0
//
  NS_E_CD_REFRESH                  = HRESULT($C00D0FB1);
  {$EXTERNALSYM NS_E_CD_REFRESH}

//
// MessageId: NS_E_CD_DRIVER_PROBLEM
//
// MessageText:
//
//  Windows Media Player must switch to analog  mode  because there is a problem reading the CD-ROM drive in digital mode. Verify that the CD-ROM drive is installed correctly or try to update the drivers for the CD-ROM drive, and then try to use digital mode again.%0
//
  NS_E_CD_DRIVER_PROBLEM           = HRESULT($C00D0FB2);
  {$EXTERNALSYM NS_E_CD_DRIVER_PROBLEM}

//
// MessageId: NS_E_WONT_DO_DIGITAL
//
// MessageText:
//
//  Windows Media Player must switch to analog mode because there is a problem reading the CD-ROM drive  in digital mode.%0
//
  NS_E_WONT_DO_DIGITAL             = HRESULT($C00D0FB3);
  {$EXTERNALSYM NS_E_WONT_DO_DIGITAL}

//
// WMP IWMPXMLParser Error codes
//
//
// MessageId: NS_E_WMPXML_NOERROR
//
// MessageText:
//
//  A call was made to GetParseError on the XML parser but there was no error to retrieve.%0
//
  NS_E_WMPXML_NOERROR              = HRESULT($C00D0FB4);
  {$EXTERNALSYM NS_E_WMPXML_NOERROR}

//
// MessageId: NS_E_WMPXML_ENDOFDATA
//
// MessageText:
//
//  The XML Parser ran out of data while parsing.%0
//
  NS_E_WMPXML_ENDOFDATA            = HRESULT($C00D0FB5);
  {$EXTERNALSYM NS_E_WMPXML_ENDOFDATA}

//
// MessageId: NS_E_WMPXML_PARSEERROR
//
// MessageText:
//
//  A generic parse error occurred in the XML parser but no information is available.%0
//
  NS_E_WMPXML_PARSEERROR           = HRESULT($C00D0FB6);
  {$EXTERNALSYM NS_E_WMPXML_PARSEERROR}

//
// MessageId: NS_E_WMPXML_ATTRIBUTENOTFOUND
//
// MessageText:
//
//  A call get GetNamedAttribute or GetNamedAttributeIndex on the XML parser resulted in the index not being found.%0
//
  NS_E_WMPXML_ATTRIBUTENOTFOUND    = HRESULT($C00D0FB7);
  {$EXTERNALSYM NS_E_WMPXML_ATTRIBUTENOTFOUND}

//
// MessageId: NS_E_WMPXML_PINOTFOUND
//
// MessageText:
//
//  A call was made go GetNamedPI on the XML parser, but the requested Processing Instruction was not found.%0
//
  NS_E_WMPXML_PINOTFOUND           = HRESULT($C00D0FB8);
  {$EXTERNALSYM NS_E_WMPXML_PINOTFOUND}

//
// MessageId: NS_E_WMPXML_EMPTYDOC
//
// MessageText:
//
//  Persist was called on the XML parser, but the parser has no data to persist.%0
//
  NS_E_WMPXML_EMPTYDOC             = HRESULT($C00D0FB9);
  {$EXTERNALSYM NS_E_WMPXML_EMPTYDOC}

//
// Miscellaneous Media Player Error codes
//
//
// MessageId: NS_E_WMP_WINDOWSAPIFAILURE
//
// MessageText:
//
//  A Windows API call failed but no error information was available.%0
//
  NS_E_WMP_WINDOWSAPIFAILURE       = HRESULT($C00D0FC8);
  {$EXTERNALSYM NS_E_WMP_WINDOWSAPIFAILURE}

//
// MessageId: NS_E_WMP_RECORDING_NOT_ALLOWED
//
// MessageText:
//
//  Windows Media Player cannot copy the file. Either the license restricts copying, or you must obtain a license to copy the file.%0
//
  NS_E_WMP_RECORDING_NOT_ALLOWED   = HRESULT($C00D0FC9);
  {$EXTERNALSYM NS_E_WMP_RECORDING_NOT_ALLOWED}

//
// MessageId: NS_E_DEVICE_NOT_READY
//
// MessageText:
//
//  Windows Media Player no longer detects a connected portable device. Reconnect your portable device, and try again.%0
//
  NS_E_DEVICE_NOT_READY            = HRESULT($C00D0FCA);
  {$EXTERNALSYM NS_E_DEVICE_NOT_READY}

//
// MessageId: NS_E_DAMAGED_FILE
//
// MessageText:
//
//  Windows Media Player cannot play the file because it is either damaged or corrupt.%0
//
  NS_E_DAMAGED_FILE                = HRESULT($C00D0FCB);
  {$EXTERNALSYM NS_E_DAMAGED_FILE}

//
// MessageId: NS_E_MPDB_GENERIC
//
// MessageText:
//
//  An error occurred when the Player was attempting to access information in your media library. Try closing and then reopening the Player.%0
//
  NS_E_MPDB_GENERIC                = HRESULT($C00D0FCC);
  {$EXTERNALSYM NS_E_MPDB_GENERIC}

//
// MessageId: NS_E_FILE_FAILED_CHECKS
//
// MessageText:
//
//  The file cannot be added to Media Library because it is smaller than the minimum-size requirement. Adjust the size requirements, and then try again.%0
//
  NS_E_FILE_FAILED_CHECKS          = HRESULT($C00D0FCD);
  {$EXTERNALSYM NS_E_FILE_FAILED_CHECKS}

//
// MessageId: NS_E_MEDIA_LIBRARY_FAILED
//
// MessageText:
//
//  Windows Media Player could not create Media Library. Check with your system administrator to get the necessary permissions to create Media Library on your computer, and then try installing the Player again.%0
//
  NS_E_MEDIA_LIBRARY_FAILED        = HRESULT($C00D0FCE);
  {$EXTERNALSYM NS_E_MEDIA_LIBRARY_FAILED}

//
// MessageId: NS_E_SHARING_VIOLATION
//
// MessageText:
//
//  The file is already in use. Close other programs that may be using the file, or stop playing the file, and try again.%0
//
  NS_E_SHARING_VIOLATION           = HRESULT($C00D0FCF);
  {$EXTERNALSYM NS_E_SHARING_VIOLATION}

//
// MessageId: NS_E_NO_ERROR_STRING_FOUND
//
// MessageText:
//
//  Windows Media Player has encountered an unknown error.%0
//
  NS_E_NO_ERROR_STRING_FOUND       = HRESULT($C00D0FD0);
  {$EXTERNALSYM NS_E_NO_ERROR_STRING_FOUND}

//
// MessageId: NS_E_WMPOCX_NO_REMOTE_CORE
//
// MessageText:
//
//  The Windows Media Player control was unable to connect to remote media services, but will continue with local media services.%0
//
  NS_E_WMPOCX_NO_REMOTE_CORE       = HRESULT($C00D0FD1);
  {$EXTERNALSYM NS_E_WMPOCX_NO_REMOTE_CORE}

//
// MessageId: NS_E_WMPOCX_NO_ACTIVE_CORE
//
// MessageText:
//
//  The requested method or property is not available because the Windows Media Player control has not been properly activated.%0
//
  NS_E_WMPOCX_NO_ACTIVE_CORE       = HRESULT($C00D0FD2);
  {$EXTERNALSYM NS_E_WMPOCX_NO_ACTIVE_CORE}

//
// MessageId: NS_E_WMPOCX_NOT_RUNNING_REMOTELY
//
// MessageText:
//
//  Windows Media Player ActiveX control is not running in remote mode.%0
//
  NS_E_WMPOCX_NOT_RUNNING_REMOTELY = HRESULT($C00D0FD3);
  {$EXTERNALSYM NS_E_WMPOCX_NOT_RUNNING_REMOTELY}

//
// MessageId: NS_E_WMPOCX_NO_REMOTE_WINDOW
//
// MessageText:
//
//  An error occurred when trying to get remote Windows Media Player window.%0
//
  NS_E_WMPOCX_NO_REMOTE_WINDOW     = HRESULT($C00D0FD4);
  {$EXTERNALSYM NS_E_WMPOCX_NO_REMOTE_WINDOW}

//
// MessageId: NS_E_WMPOCX_ERRORMANAGERNOTAVAILABLE
//
// MessageText:
//
//  Windows Media Player has encountered an unknown error.%0
//
  NS_E_WMPOCX_ERRORMANAGERNOTAVAILABLE = HRESULT($C00D0FD5);
  {$EXTERNALSYM NS_E_WMPOCX_ERRORMANAGERNOTAVAILABLE}

//
// MessageId: NS_E_PLUGIN_NOTSHUTDOWN
//
// MessageText:
//
//  Windows Media Player was not closed properly. A damaged or incompatible plug-in may have caused the problem to occur. As a precaution, all third-party plug-ins have been disabled.%0
//
  NS_E_PLUGIN_NOTSHUTDOWN          = HRESULT($C00D0FD6);
  {$EXTERNALSYM NS_E_PLUGIN_NOTSHUTDOWN}

//
// MessageId: NS_E_WMP_CANNOT_FIND_FOLDER
//
// MessageText:
//
//  Windows Media Player cannot find the specified path. Be sure the path is typed correctly. If it is, the path does not exist in the specified location, or the computer where the path is located is offline.%0
//
  NS_E_WMP_CANNOT_FIND_FOLDER      = HRESULT($C00D0FD7);
  {$EXTERNALSYM NS_E_WMP_CANNOT_FIND_FOLDER}

//
// MessageId: NS_E_WMP_STREAMING_RECORDING_NOT_ALLOWED
//
// MessageText:
//
//  Windows Media Player cannot copy streaming media.%0
//
  NS_E_WMP_STREAMING_RECORDING_NOT_ALLOWED = HRESULT($C00D0FD8);
  {$EXTERNALSYM NS_E_WMP_STREAMING_RECORDING_NOT_ALLOWED}

//
// MessageId: NS_E_WMP_PLUGINDLL_NOTFOUND
//
// MessageText:
//
//  Windows Media Player cannot find the selected plug-in. The Player will try to remove it from the menu. To use this plug-in, install it again.%0
//
  NS_E_WMP_PLUGINDLL_NOTFOUND      = HRESULT($C00D0FD9);
  {$EXTERNALSYM NS_E_WMP_PLUGINDLL_NOTFOUND}

//
// MessageId: NS_E_NEED_TO_ASK_USER
//
// MessageText:
//
//  Action requires input from the user.%0
//
  NS_E_NEED_TO_ASK_USER            = HRESULT($C00D0FDA);
  {$EXTERNALSYM NS_E_NEED_TO_ASK_USER}

//
// MessageId: NS_E_WMPOCX_PLAYER_NOT_DOCKED
//
// MessageText:
//
//  The Windows Media Player control must be in a docked state for this action to succeed.%0
//
  NS_E_WMPOCX_PLAYER_NOT_DOCKED    = HRESULT($C00D0FDB);
  {$EXTERNALSYM NS_E_WMPOCX_PLAYER_NOT_DOCKED}

//
// MessageId: NS_E_WMP_EXTERNAL_NOTREADY
//
// MessageText:
//
//  Media Player external object is not ready.%0
//
  NS_E_WMP_EXTERNAL_NOTREADY       = HRESULT($C00D0FDC);
  {$EXTERNALSYM NS_E_WMP_EXTERNAL_NOTREADY}

//
// MessageId: NS_E_WMP_MLS_STALE_DATA
//
// MessageText:
//
//  Metadata is stale. The operation failed.%0
//
  NS_E_WMP_MLS_STALE_DATA          = HRESULT($C00D0FDD);
  {$EXTERNALSYM NS_E_WMP_MLS_STALE_DATA}

//
// Generic Media PlayerUI error codes
//
//
// MessageId: NS_E_WMP_UI_SUBCONTROLSNOTSUPPORTED
//
// MessageText:
//
//  The control (%s) does not support creation of sub-controls, yet (%d) sub-controls have been specified.%0
//
  NS_E_WMP_UI_SUBCONTROLSNOTSUPPORTED = HRESULT($C00D0FDE);
  {$EXTERNALSYM NS_E_WMP_UI_SUBCONTROLSNOTSUPPORTED}

//
// MessageId: NS_E_WMP_UI_VERSIONMISMATCH
//
// MessageText:
//
//  Version mismatch: (%.1f required, %.1f found).%0
//
  NS_E_WMP_UI_VERSIONMISMATCH      = HRESULT($C00D0FDF);
  {$EXTERNALSYM NS_E_WMP_UI_VERSIONMISMATCH}

//
// MessageId: NS_E_WMP_UI_NOTATHEMEFILE
//
// MessageText:
//
//  The layout manager was given valid XML that wasn't a theme file.%0
//
  NS_E_WMP_UI_NOTATHEMEFILE        = HRESULT($C00D0FE0);
  {$EXTERNALSYM NS_E_WMP_UI_NOTATHEMEFILE}

//
// MessageId: NS_E_WMP_UI_SUBELEMENTNOTFOUND
//
// MessageText:
//
//  The %s subelement could not be found on the %s object.%0
//
  NS_E_WMP_UI_SUBELEMENTNOTFOUND   = HRESULT($C00D0FE1);
  {$EXTERNALSYM NS_E_WMP_UI_SUBELEMENTNOTFOUND}

//
// MessageId: NS_E_WMP_UI_VERSIONPARSE
//
// MessageText:
//
//  An error occurred parsing the version tag.\nValid version tags are of the form:\n\n\t<?wmp version='1.0'?>.%0
//
  NS_E_WMP_UI_VERSIONPARSE         = HRESULT($C00D0FE2);
  {$EXTERNALSYM NS_E_WMP_UI_VERSIONPARSE}

//
// MessageId: NS_E_WMP_UI_VIEWIDNOTFOUND
//
// MessageText:
//
//  The view specified in for the 'currentViewID' property (%s) was not found in this theme file.%0
//
  NS_E_WMP_UI_VIEWIDNOTFOUND       = HRESULT($C00D0FE3);
  {$EXTERNALSYM NS_E_WMP_UI_VIEWIDNOTFOUND}

//
// MessageId: NS_E_WMP_UI_PASSTHROUGH
//
// MessageText:
//
//  This error used internally for hit testing.%0
//
  NS_E_WMP_UI_PASSTHROUGH          = HRESULT($C00D0FE4);
  {$EXTERNALSYM NS_E_WMP_UI_PASSTHROUGH}

//
// MessageId: NS_E_WMP_UI_OBJECTNOTFOUND
//
// MessageText:
//
//  Attributes were specified for the %s object, but the object was not available to send them to.%0
//
  NS_E_WMP_UI_OBJECTNOTFOUND       = HRESULT($C00D0FE5);
  {$EXTERNALSYM NS_E_WMP_UI_OBJECTNOTFOUND}

//
// MessageId: NS_E_WMP_UI_SECONDHANDLER
//
// MessageText:
//
//  The %s event already has a handler, the second handler was ignored.%0
//
  NS_E_WMP_UI_SECONDHANDLER        = HRESULT($C00D0FE6);
  {$EXTERNALSYM NS_E_WMP_UI_SECONDHANDLER}

//
// MessageId: NS_E_WMP_UI_NOSKININZIP
//
// MessageText:
//
//  No .wms file found in skin archive.%0
//
  NS_E_WMP_UI_NOSKININZIP          = HRESULT($C00D0FE7);
  {$EXTERNALSYM NS_E_WMP_UI_NOSKININZIP}

//
// MessageId: NS_S_WMP_UI_VERSIONMISMATCH
//
// MessageText:
//
//  An upgrade may be needed for the theme manager to correctly show this skin. Skin reports version: %.1f.%0
//
  NS_S_WMP_UI_VERSIONMISMATCH      = HRESULT($000D0FE8);
  {$EXTERNALSYM NS_S_WMP_UI_VERSIONMISMATCH}

//
// MessageId: NS_S_WMP_EXCEPTION
//
// MessageText:
//
//  An error occurred in one of the UI components.%0
//
  NS_S_WMP_EXCEPTION               = HRESULT($000D0FE9);
  {$EXTERNALSYM NS_S_WMP_EXCEPTION}

//
// MessageId: NS_E_WMP_URLDOWNLOADFAILED
//
// MessageText:
//
//  Windows Media Player cannot save the file.%0
//
  NS_E_WMP_URLDOWNLOADFAILED       = HRESULT($C00D0FEA);
  {$EXTERNALSYM NS_E_WMP_URLDOWNLOADFAILED}

//
// MessageId: NS_E_WMPOCX_UNABLE_TO_LOAD_SKIN
//
// MessageText:
//
//  The Windows Media Player Control was unable to load the requested uiMode and could not successfully roll back to the existing uiMode.%0
//
  NS_E_WMPOCX_UNABLE_TO_LOAD_SKIN  = HRESULT($C00D0FEB);
  {$EXTERNALSYM NS_E_WMPOCX_UNABLE_TO_LOAD_SKIN}

//
// MessageId: NS_E_WMP_INVALID_SKIN
//
// MessageText:
//
//  The skin file is invalid.%0
//
  NS_E_WMP_INVALID_SKIN            = HRESULT($C00D0FEC);
  {$EXTERNALSYM NS_E_WMP_INVALID_SKIN}

//
// MessageId: NS_E_WMP_SENDMAILFAILED
//
// MessageText:
//
//  Windows Media Player cannot send the link because your e-mail program is not responding. Verify that your e-mail program is configured properly, and then try again. For more information about e-mail, see Windows Help%0
//
  NS_E_WMP_SENDMAILFAILED          = HRESULT($C00D0FED);
  {$EXTERNALSYM NS_E_WMP_SENDMAILFAILED}

//Save As
//
// MessageId: NS_E_WMP_SAVEAS_READONLY
//
// MessageText:
//
//  The Windows Media Player cannot overwrite a read only file. Choose another file to save as or change the file attributes.%0
//
  NS_E_WMP_SAVEAS_READONLY         = HRESULT($C00D0FF0);
  {$EXTERNALSYM NS_E_WMP_SAVEAS_READONLY}

//
// WMP Regional button control
//
//
// MessageId: NS_E_WMP_RBC_JPGMAPPINGIMAGE
//
// MessageText:
//
//  JPG Images are not recommended for use as a mappingImage.%0
//
  NS_E_WMP_RBC_JPGMAPPINGIMAGE     = HRESULT($C00D1004);
  {$EXTERNALSYM NS_E_WMP_RBC_JPGMAPPINGIMAGE}

//
// MessageId: NS_E_WMP_JPGTRANSPARENCY
//
// MessageText:
//
//  JPG Images are not recommended when using a transparencyColor.%0
//
  NS_E_WMP_JPGTRANSPARENCY         = HRESULT($C00D1005);
  {$EXTERNALSYM NS_E_WMP_JPGTRANSPARENCY}

//
// WMP Slider control
//
//
// MessageId: NS_E_WMP_INVALID_MAX_VAL
//
// MessageText:
//
//  The Max property cannot be less than Min property.%0
//
  NS_E_WMP_INVALID_MAX_VAL         = HRESULT($C00D1009);
  {$EXTERNALSYM NS_E_WMP_INVALID_MAX_VAL}

//
// MessageId: NS_E_WMP_INVALID_MIN_VAL
//
// MessageText:
//
//  The Min property cannot be greater than Max property.%0
//
  NS_E_WMP_INVALID_MIN_VAL         = HRESULT($C00D100A);
  {$EXTERNALSYM NS_E_WMP_INVALID_MIN_VAL}

//
// WMP CustomSlider control
//
//
// MessageId: NS_E_WMP_CS_JPGPOSITIONIMAGE
//
// MessageText:
//
//  JPG Images are not recommended for use as a positionImage.%0
//
  NS_E_WMP_CS_JPGPOSITIONIMAGE     = HRESULT($C00D100E);
  {$EXTERNALSYM NS_E_WMP_CS_JPGPOSITIONIMAGE}

//
// MessageId: NS_E_WMP_CS_NOTEVENLYDIVISIBLE
//
// MessageText:
//
//  The (%s) image's size is not evenly divisible by the positionImage's size.%0
//
  NS_E_WMP_CS_NOTEVENLYDIVISIBLE   = HRESULT($C00D100F);
  {$EXTERNALSYM NS_E_WMP_CS_NOTEVENLYDIVISIBLE}

//
// WMP ZIP Decoder
//
//
// MessageId: NS_E_WMPZIP_NOTAZIPFILE
//
// MessageText:
//
//  The ZIP reader opened a file and its signature didn't match that of ZIP files.%0
//
  NS_E_WMPZIP_NOTAZIPFILE          = HRESULT($C00D1018);
  {$EXTERNALSYM NS_E_WMPZIP_NOTAZIPFILE}

//
// MessageId: NS_E_WMPZIP_CORRUPT
//
// MessageText:
//
//  The ZIP reader has detected that the file is corrupt.%0
//
  NS_E_WMPZIP_CORRUPT              = HRESULT($C00D1019);
  {$EXTERNALSYM NS_E_WMPZIP_CORRUPT}

//
// MessageId: NS_E_WMPZIP_FILENOTFOUND
//
// MessageText:
//
//  GetFileStream, SaveToFile, or SaveTemp file was called on the ZIP reader with a filename that was not found in the zip file.%0
//
  NS_E_WMPZIP_FILENOTFOUND         = HRESULT($C00D101A);
  {$EXTERNALSYM NS_E_WMPZIP_FILENOTFOUND}

//
// WMP Image Decoding Error codes
//
//
// MessageId: NS_E_WMP_IMAGE_FILETYPE_UNSUPPORTED
//
// MessageText:
//
//  Image type not supported.%0
//
  NS_E_WMP_IMAGE_FILETYPE_UNSUPPORTED = HRESULT($C00D1022);
  {$EXTERNALSYM NS_E_WMP_IMAGE_FILETYPE_UNSUPPORTED}

//
// MessageId: NS_E_WMP_IMAGE_INVALID_FORMAT
//
// MessageText:
//
//  Image file may be corrupt.%0
//
  NS_E_WMP_IMAGE_INVALID_FORMAT    = HRESULT($C00D1023);
  {$EXTERNALSYM NS_E_WMP_IMAGE_INVALID_FORMAT}

//
// MessageId: NS_E_WMP_GIF_UNEXPECTED_ENDOFFILE
//
// MessageText:
//
//  Unexpected end of file. GIF file may be corrupt.%0
//
  NS_E_WMP_GIF_UNEXPECTED_ENDOFFILE = HRESULT($C00D1024);
  {$EXTERNALSYM NS_E_WMP_GIF_UNEXPECTED_ENDOFFILE}

//
// MessageId: NS_E_WMP_GIF_INVALID_FORMAT
//
// MessageText:
//
//  Invalid GIF file.%0
//
  NS_E_WMP_GIF_INVALID_FORMAT      = HRESULT($C00D1025);
  {$EXTERNALSYM NS_E_WMP_GIF_INVALID_FORMAT}

//
// MessageId: NS_E_WMP_GIF_BAD_VERSION_NUMBER
//
// MessageText:
//
//  Invalid GIF version. Only 87a or 89a supported.%0
//
  NS_E_WMP_GIF_BAD_VERSION_NUMBER  = HRESULT($C00D1026);
  {$EXTERNALSYM NS_E_WMP_GIF_BAD_VERSION_NUMBER}

//
// MessageId: NS_E_WMP_GIF_NO_IMAGE_IN_FILE
//
// MessageText:
//
//  No images found in GIF file.%0
//
  NS_E_WMP_GIF_NO_IMAGE_IN_FILE    = HRESULT($C00D1027);
  {$EXTERNALSYM NS_E_WMP_GIF_NO_IMAGE_IN_FILE}

//
// MessageId: NS_E_WMP_PNG_INVALIDFORMAT
//
// MessageText:
//
//  Invalid PNG image file format.%0
//
  NS_E_WMP_PNG_INVALIDFORMAT       = HRESULT($C00D1028);
  {$EXTERNALSYM NS_E_WMP_PNG_INVALIDFORMAT}

//
// MessageId: NS_E_WMP_PNG_UNSUPPORTED_BITDEPTH
//
// MessageText:
//
//  PNG bitdepth not supported.%0
//
  NS_E_WMP_PNG_UNSUPPORTED_BITDEPTH = HRESULT($C00D1029);
  {$EXTERNALSYM NS_E_WMP_PNG_UNSUPPORTED_BITDEPTH}

//
// MessageId: NS_E_WMP_PNG_UNSUPPORTED_COMPRESSION
//
// MessageText:
//
//  Compression format defined in PNG file not supported,%0
//
  NS_E_WMP_PNG_UNSUPPORTED_COMPRESSION = HRESULT($C00D102A);
  {$EXTERNALSYM NS_E_WMP_PNG_UNSUPPORTED_COMPRESSION}

//
// MessageId: NS_E_WMP_PNG_UNSUPPORTED_FILTER
//
// MessageText:
//
//  Filter method defined in PNG file not supported.%0
//
  NS_E_WMP_PNG_UNSUPPORTED_FILTER  = HRESULT($C00D102B);
  {$EXTERNALSYM NS_E_WMP_PNG_UNSUPPORTED_FILTER}

//
// MessageId: NS_E_WMP_PNG_UNSUPPORTED_INTERLACE
//
// MessageText:
//
//  Interlace method defined in PNG file not supported.%0
//
  NS_E_WMP_PNG_UNSUPPORTED_INTERLACE = HRESULT($C00D102C);
  {$EXTERNALSYM NS_E_WMP_PNG_UNSUPPORTED_INTERLACE}

//
// MessageId: NS_E_WMP_PNG_UNSUPPORTED_BAD_CRC
//
// MessageText:
//
//  Bad CRC in PNG file.%0
//
  NS_E_WMP_PNG_UNSUPPORTED_BAD_CRC = HRESULT($C00D102D);
  {$EXTERNALSYM NS_E_WMP_PNG_UNSUPPORTED_BAD_CRC}

//
// MessageId: NS_E_WMP_BMP_INVALID_BITMASK
//
// MessageText:
//
//  Invalid bitmask in BMP file.%0
//
  NS_E_WMP_BMP_INVALID_BITMASK     = HRESULT($C00D102E);
  {$EXTERNALSYM NS_E_WMP_BMP_INVALID_BITMASK}

//
// MessageId: NS_E_WMP_BMP_TOPDOWN_DIB_UNSUPPORTED
//
// MessageText:
//
//  Topdown DIB not supported.%0
//
  NS_E_WMP_BMP_TOPDOWN_DIB_UNSUPPORTED = HRESULT($C00D102F);
  {$EXTERNALSYM NS_E_WMP_BMP_TOPDOWN_DIB_UNSUPPORTED}

//
// MessageId: NS_E_WMP_BMP_BITMAP_NOT_CREATED
//
// MessageText:
//
//  Bitmap could not be created.%0
//
  NS_E_WMP_BMP_BITMAP_NOT_CREATED  = HRESULT($C00D1030);
  {$EXTERNALSYM NS_E_WMP_BMP_BITMAP_NOT_CREATED}

//
// MessageId: NS_E_WMP_BMP_COMPRESSION_UNSUPPORTED
//
// MessageText:
//
//  Compression format defined in BMP not supported.%0
//
  NS_E_WMP_BMP_COMPRESSION_UNSUPPORTED = HRESULT($C00D1031);
  {$EXTERNALSYM NS_E_WMP_BMP_COMPRESSION_UNSUPPORTED}

//
// MessageId: NS_E_WMP_BMP_INVALID_FORMAT
//
// MessageText:
//
//  Invalid Bitmap format.%0
//
  NS_E_WMP_BMP_INVALID_FORMAT      = HRESULT($C00D1032);
  {$EXTERNALSYM NS_E_WMP_BMP_INVALID_FORMAT}

//
// MessageId: NS_E_WMP_JPG_JERR_ARITHCODING_NOTIMPL
//
// MessageText:
//
//  JPEG Arithmetic coding not supported.%0
//
  NS_E_WMP_JPG_JERR_ARITHCODING_NOTIMPL = HRESULT($C00D1033);
  {$EXTERNALSYM NS_E_WMP_JPG_JERR_ARITHCODING_NOTIMPL}

//
// MessageId: NS_E_WMP_JPG_INVALID_FORMAT
//
// MessageText:
//
//  Invalid JPEG format.%0
//
  NS_E_WMP_JPG_INVALID_FORMAT      = HRESULT($C00D1034);
  {$EXTERNALSYM NS_E_WMP_JPG_INVALID_FORMAT}

//
// MessageId: NS_E_WMP_JPG_BAD_DCTSIZE
//
// MessageText:
//
//  Invalid JPEG format.%0
//
  NS_E_WMP_JPG_BAD_DCTSIZE         = HRESULT($C00D1035);
  {$EXTERNALSYM NS_E_WMP_JPG_BAD_DCTSIZE}

//
// MessageId: NS_E_WMP_JPG_BAD_VERSION_NUMBER
//
// MessageText:
//
//  Internal version error. Unexpected JPEG library version.%0
//
  NS_E_WMP_JPG_BAD_VERSION_NUMBER  = HRESULT($C00D1036);
  {$EXTERNALSYM NS_E_WMP_JPG_BAD_VERSION_NUMBER}

//
// MessageId: NS_E_WMP_JPG_BAD_PRECISION
//
// MessageText:
//
//  Internal JPEG Library error. Unsupported JPEG data precision.%0
//
  NS_E_WMP_JPG_BAD_PRECISION       = HRESULT($C00D1037);
  {$EXTERNALSYM NS_E_WMP_JPG_BAD_PRECISION}

//
// MessageId: NS_E_WMP_JPG_CCIR601_NOTIMPL
//
// MessageText:
//
//  JPEG CCIR601 not supported.%0
//
  NS_E_WMP_JPG_CCIR601_NOTIMPL     = HRESULT($C00D1038);
  {$EXTERNALSYM NS_E_WMP_JPG_CCIR601_NOTIMPL}

//
// MessageId: NS_E_WMP_JPG_NO_IMAGE_IN_FILE
//
// MessageText:
//
//  No image found in JPEG file.%0
//
  NS_E_WMP_JPG_NO_IMAGE_IN_FILE    = HRESULT($C00D1039);
  {$EXTERNALSYM NS_E_WMP_JPG_NO_IMAGE_IN_FILE}

//
// MessageId: NS_E_WMP_JPG_READ_ERROR
//
// MessageText:
//
//  Could not read JPEG file.%0
//
  NS_E_WMP_JPG_READ_ERROR          = HRESULT($C00D103A);
  {$EXTERNALSYM NS_E_WMP_JPG_READ_ERROR}

//
// MessageId: NS_E_WMP_JPG_FRACT_SAMPLE_NOTIMPL
//
// MessageText:
//
//  JPEG Fractional sampling not supported.%0
//
  NS_E_WMP_JPG_FRACT_SAMPLE_NOTIMPL = HRESULT($C00D103B);
  {$EXTERNALSYM NS_E_WMP_JPG_FRACT_SAMPLE_NOTIMPL}

//
// MessageId: NS_E_WMP_JPG_IMAGE_TOO_BIG
//
// MessageText:
//
//  JPEG image too large. Maximum image size supported is 65500 X 65500.%0
//
  NS_E_WMP_JPG_IMAGE_TOO_BIG       = HRESULT($C00D103C);
  {$EXTERNALSYM NS_E_WMP_JPG_IMAGE_TOO_BIG}

//
// MessageId: NS_E_WMP_JPG_UNEXPECTED_ENDOFFILE
//
// MessageText:
//
//  Unexpected end of file reached in JPEG file.%0
//
  NS_E_WMP_JPG_UNEXPECTED_ENDOFFILE = HRESULT($C00D103D);
  {$EXTERNALSYM NS_E_WMP_JPG_UNEXPECTED_ENDOFFILE}

//
// MessageId: NS_E_WMP_JPG_SOF_UNSUPPORTED
//
// MessageText:
//
//  Unsupported JPEG SOF marker found.%0
//
  NS_E_WMP_JPG_SOF_UNSUPPORTED     = HRESULT($C00D103E);
  {$EXTERNALSYM NS_E_WMP_JPG_SOF_UNSUPPORTED}

//
// MessageId: NS_E_WMP_JPG_UNKNOWN_MARKER
//
// MessageText:
//
//  Unknown JPEG marker found.%0
//
  NS_E_WMP_JPG_UNKNOWN_MARKER      = HRESULT($C00D103F);
  {$EXTERNALSYM NS_E_WMP_JPG_UNKNOWN_MARKER}

//
// MessageId: NS_S_WMP_LOADED_GIF_IMAGE
//
// MessageText:
//
//  Successfully loaded a GIF file.%0
//
  NS_S_WMP_LOADED_GIF_IMAGE        = HRESULT($000D1040);
  {$EXTERNALSYM NS_S_WMP_LOADED_GIF_IMAGE}

//
// MessageId: NS_S_WMP_LOADED_PNG_IMAGE
//
// MessageText:
//
//  Successfully loaded a PNG file.%0
//
  NS_S_WMP_LOADED_PNG_IMAGE        = HRESULT($000D1041);
  {$EXTERNALSYM NS_S_WMP_LOADED_PNG_IMAGE}

//
// MessageId: NS_S_WMP_LOADED_BMP_IMAGE
//
// MessageText:
//
//  Successfully loaded a BMP file.%0
//
  NS_S_WMP_LOADED_BMP_IMAGE        = HRESULT($000D1042);
  {$EXTERNALSYM NS_S_WMP_LOADED_BMP_IMAGE}

//
// MessageId: NS_S_WMP_LOADED_JPG_IMAGE
//
// MessageText:
//
//  Successfully loaded a JPG file.%0
//
  NS_S_WMP_LOADED_JPG_IMAGE        = HRESULT($000D1043);
  {$EXTERNALSYM NS_S_WMP_LOADED_JPG_IMAGE}

//
// WMP WM Runtime Error codes
//
//
// MessageId: NS_E_WMG_RATEUNAVAILABLE
//
// MessageText:
//
//  The requested playback rate is unavailable on this content.%0
//
  NS_E_WMG_RATEUNAVAILABLE         = HRESULT($C00D104A);
  {$EXTERNALSYM NS_E_WMG_RATEUNAVAILABLE}

//
// MessageId: NS_E_WMG_PLUGINUNAVAILABLE
//
// MessageText:
//
//  The rendering or digital signal processing plugin could not be instantiated.%0
//
  NS_E_WMG_PLUGINUNAVAILABLE       = HRESULT($C00D104B);
  {$EXTERNALSYM NS_E_WMG_PLUGINUNAVAILABLE}

//
// MessageId: NS_E_WMG_CANNOTQUEUE
//
// MessageText:
//
//  The file cannot be queued for seamless playback.%0
//
  NS_E_WMG_CANNOTQUEUE             = HRESULT($C00D104C);
  {$EXTERNALSYM NS_E_WMG_CANNOTQUEUE}

//
// MessageId: NS_E_WMG_PREROLLLICENSEACQUISITIONNOTALLOWED
//
// MessageText:
//
//  Windows Media Player cannot acquire the license for a file that is being prerolled.%0
//
  NS_E_WMG_PREROLLLICENSEACQUISITIONNOTALLOWED = HRESULT($C00D104D);
  {$EXTERNALSYM NS_E_WMG_PREROLLLICENSEACQUISITIONNOTALLOWED}

//
// MessageId: NS_E_WMG_UNEXPECTEDPREROLLSTATUS
//
// MessageText:
//
//  Windows Media Player received an unexpected message while attempting to preroll a file.%0
//
  NS_E_WMG_UNEXPECTEDPREROLLSTATUS = HRESULT($C00D104E);
  {$EXTERNALSYM NS_E_WMG_UNEXPECTEDPREROLLSTATUS}

//
// MessageId: NS_E_WMG_INVALIDSTATE
//
// MessageText:
//
//  Operation attempted in an invalid graph state.%0
//
  NS_E_WMG_INVALIDSTATE            = HRESULT($C00D1054);
  {$EXTERNALSYM NS_E_WMG_INVALIDSTATE}

//
// MessageId: NS_E_WMG_SINKALREADYEXISTS
//
// MessageText:
//
//  A renderer cannot be inserted in a stream while one already exists.%0
//
  NS_E_WMG_SINKALREADYEXISTS       = HRESULT($C00D1055);
  {$EXTERNALSYM NS_E_WMG_SINKALREADYEXISTS}

//
// MessageId: NS_E_WMG_NOSDKINTERFACE
//
// MessageText:
//
//  A necessary WM SDK interface to complete the operation doesn't exist at this time.%0
//
  NS_E_WMG_NOSDKINTERFACE          = HRESULT($C00D1056);
  {$EXTERNALSYM NS_E_WMG_NOSDKINTERFACE}

//
// MessageId: NS_E_WMG_NOTALLOUTPUTSRENDERED
//
// MessageText:
//
//  Windows Media Player cannot play the file. The file may be formatted with an unsupported codec, or the Player could not download the codec.%0
//
  NS_E_WMG_NOTALLOUTPUTSRENDERED   = HRESULT($C00D1057);
  {$EXTERNALSYM NS_E_WMG_NOTALLOUTPUTSRENDERED}

//
// MessageId: NS_E_WMG_FILETRANSFERNOTALLOWED
//
// MessageText:
//
//  File transfer streams are not allowed in the stand alone player.%0
//
  NS_E_WMG_FILETRANSFERNOTALLOWED  = HRESULT($C00D1058);
  {$EXTERNALSYM NS_E_WMG_FILETRANSFERNOTALLOWED}

//
// MessageId: NS_E_WMR_UNSUPPORTEDSTREAM
//
// MessageText:
//
//  Windows Media Player cannot play the file. The Player does not support the format you are trying to play.%0
//
  NS_E_WMR_UNSUPPORTEDSTREAM       = HRESULT($C00D1059);
  {$EXTERNALSYM NS_E_WMR_UNSUPPORTEDSTREAM}

//
// MessageId: NS_E_WMR_PINNOTFOUND
//
// MessageText:
//
//  An operation was attempted on a pin that doesn't exist in the DirectShow filter graph.%0
//
  NS_E_WMR_PINNOTFOUND             = HRESULT($C00D105A);
  {$EXTERNALSYM NS_E_WMR_PINNOTFOUND}

//
// MessageId: NS_E_WMR_WAITINGONFORMATSWITCH
//
// MessageText:
//
//  Specified operation cannot be completed while waiting for a media format change from the SDK.%0
//
  NS_E_WMR_WAITINGONFORMATSWITCH   = HRESULT($C00D105B);
  {$EXTERNALSYM NS_E_WMR_WAITINGONFORMATSWITCH}

//
// MessageId: NS_E_WMR_NOSOURCEFILTER
//
// MessageText:
//
//  Specified operation cannot be completed because the source filter does not exist.%0
//
  NS_E_WMR_NOSOURCEFILTER          = HRESULT($C00D105C);
  {$EXTERNALSYM NS_E_WMR_NOSOURCEFILTER}

//
// MessageId: NS_E_WMR_PINTYPENOMATCH
//
// MessageText:
//
//  The specified type does not match this pin.%0
//
  NS_E_WMR_PINTYPENOMATCH          = HRESULT($C00D105D);
  {$EXTERNALSYM NS_E_WMR_PINTYPENOMATCH}

//
// MessageId: NS_E_WMR_NOCALLBACKAVAILABLE
//
// MessageText:
//
//  The WMR Source Filter does not have a callback available.%0
//
  NS_E_WMR_NOCALLBACKAVAILABLE     = HRESULT($C00D105E);
  {$EXTERNALSYM NS_E_WMR_NOCALLBACKAVAILABLE}

//
// MessageId: NS_S_WMR_ALREADYRENDERED
//
// MessageText:
//
//  The specified stream has already been rendered.%0
//
  NS_S_WMR_ALREADYRENDERED         = HRESULT($000D105F);
  {$EXTERNALSYM NS_S_WMR_ALREADYRENDERED}

//
// MessageId: NS_S_WMR_PINTYPEPARTIALMATCH
//
// MessageText:
//
//  The specified type partially matches this pin type.%0
//
  NS_S_WMR_PINTYPEPARTIALMATCH     = HRESULT($000D1060);
  {$EXTERNALSYM NS_S_WMR_PINTYPEPARTIALMATCH}

//
// MessageId: NS_S_WMR_PINTYPEFULLMATCH
//
// MessageText:
//
//  The specified type fully matches this pin type.%0
//
  NS_S_WMR_PINTYPEFULLMATCH        = HRESULT($000D1061);
  {$EXTERNALSYM NS_S_WMR_PINTYPEFULLMATCH}

//
// MessageId: NS_E_WMR_SAMPLEPROPERTYNOTSET
//
// MessageText:
//
//  The specified property has not been set on this sample.%0
//
  NS_E_WMR_SAMPLEPROPERTYNOTSET    = HRESULT($C00D1062);
  {$EXTERNALSYM NS_E_WMR_SAMPLEPROPERTYNOTSET}

//
// MessageId: NS_E_WMR_CANNOT_RENDER_BINARY_STREAM
//
// MessageText:
//
//  A plugin is required to correctly play this file. To determine if this plugin is available to download from the Web, click Web Help.%0
//
  NS_E_WMR_CANNOT_RENDER_BINARY_STREAM = HRESULT($C00D1063);
  {$EXTERNALSYM NS_E_WMR_CANNOT_RENDER_BINARY_STREAM}

//
// MessageId: NS_E_WMG_LICENSE_TAMPERED
//
// MessageText:
//
//  The file cannot be played, the content has been tampered.%0
//
  NS_E_WMG_LICENSE_TAMPERED        = HRESULT($C00D1064);
  {$EXTERNALSYM NS_E_WMG_LICENSE_TAMPERED}

//
// MessageId: NS_E_WMR_WILLNOT_RENDER_BINARY_STREAM
//
// MessageText:
//
//  The content you are trying to play is protected content and the player will not render binary streams from protected content.%0
//
  NS_E_WMR_WILLNOT_RENDER_BINARY_STREAM = HRESULT($C00D1065);
  {$EXTERNALSYM NS_E_WMR_WILLNOT_RENDER_BINARY_STREAM}

//
// WMP Playlist Error codes
//
//
// MessageId: NS_E_WMX_UNRECOGNIZED_PLAYLIST_FORMAT
//
// MessageText:
//
//  The format of this file was not recognized as a valid playlist format.%0
//
  NS_E_WMX_UNRECOGNIZED_PLAYLIST_FORMAT = HRESULT($C00D1068);
  {$EXTERNALSYM NS_E_WMX_UNRECOGNIZED_PLAYLIST_FORMAT}

//
// MessageId: NS_E_ASX_INVALIDFORMAT
//
// MessageText:
//
//  This file was believed to be an ASX playlist, but the format was not recognized.%0
//
  NS_E_ASX_INVALIDFORMAT           = HRESULT($C00D1069);
  {$EXTERNALSYM NS_E_ASX_INVALIDFORMAT}

//
// MessageId: NS_E_ASX_INVALIDVERSION
//
// MessageText:
//
//  The version of this playlist is not supported. Click More Information to go to the Microsoft web site and see if there is a newer version of the player to install.%0
//
  NS_E_ASX_INVALIDVERSION          = HRESULT($C00D106A);
  {$EXTERNALSYM NS_E_ASX_INVALIDVERSION}

//
// MessageId: NS_E_ASX_INVALID_REPEAT_BLOCK
//
// MessageText:
//
//  Format of a REPEAT loop within the current playlist file is invalid.%0
//
  NS_E_ASX_INVALID_REPEAT_BLOCK    = HRESULT($C00D106B);
  {$EXTERNALSYM NS_E_ASX_INVALID_REPEAT_BLOCK}

//
// MessageId: NS_E_ASX_NOTHING_TO_WRITE
//
// MessageText:
//
//  Windows Media Player cannot export the playlist because it is empty.%0
//
  NS_E_ASX_NOTHING_TO_WRITE        = HRESULT($C00D106C);
  {$EXTERNALSYM NS_E_ASX_NOTHING_TO_WRITE}

//
// MessageId: NS_E_URLLIST_INVALIDFORMAT
//
// MessageText:
//
//  Windows Media Player does not recognize this file as a supported playlist.%0
//
  NS_E_URLLIST_INVALIDFORMAT       = HRESULT($C00D106D);
  {$EXTERNALSYM NS_E_URLLIST_INVALIDFORMAT}

//
// MessageId: NS_E_WMX_ATTRIBUTE_DOES_NOT_EXIST
//
// MessageText:
//
//  The specified attribute does not exist.%0
//
  NS_E_WMX_ATTRIBUTE_DOES_NOT_EXIST = HRESULT($C00D106E);
  {$EXTERNALSYM NS_E_WMX_ATTRIBUTE_DOES_NOT_EXIST}

//
// MessageId: NS_E_WMX_ATTRIBUTE_ALREADY_EXISTS
//
// MessageText:
//
//  The specified attribute already exists.%0
//
  NS_E_WMX_ATTRIBUTE_ALREADY_EXISTS = HRESULT($C00D106F);
  {$EXTERNALSYM NS_E_WMX_ATTRIBUTE_ALREADY_EXISTS}

//
// MessageId: NS_E_WMX_ATTRIBUTE_UNRETRIEVABLE
//
// MessageText:
//
//  Can not retrieve the specified attribute.%0
//
  NS_E_WMX_ATTRIBUTE_UNRETRIEVABLE = HRESULT($C00D1070);
  {$EXTERNALSYM NS_E_WMX_ATTRIBUTE_UNRETRIEVABLE}

//
// MessageId: NS_E_WMX_ITEM_DOES_NOT_EXIST
//
// MessageText:
//
//  The specified item does not exist in the current playlist.%0
//
  NS_E_WMX_ITEM_DOES_NOT_EXIST     = HRESULT($C00D1071);
  {$EXTERNALSYM NS_E_WMX_ITEM_DOES_NOT_EXIST}

//
// MessageId: NS_E_WMX_ITEM_TYPE_ILLEGAL
//
// MessageText:
//
//  Items of the specified type can not be created within the current playlist.%0
//
  NS_E_WMX_ITEM_TYPE_ILLEGAL       = HRESULT($C00D1072);
  {$EXTERNALSYM NS_E_WMX_ITEM_TYPE_ILLEGAL}

//
// MessageId: NS_E_WMX_ITEM_UNSETTABLE
//
// MessageText:
//
//  The specified item can not be set in the current playlist.%0
//
  NS_E_WMX_ITEM_UNSETTABLE         = HRESULT($C00D1073);
  {$EXTERNALSYM NS_E_WMX_ITEM_UNSETTABLE}

//
// MessageId: NS_E_WMX_PLAYLIST_EMPTY
//
// MessageText:
//
//  The specified playlist is empty.%0
//
  NS_E_WMX_PLAYLIST_EMPTY          = HRESULT($C00D1074);
  {$EXTERNALSYM NS_E_WMX_PLAYLIST_EMPTY}

//
// MessageId: NS_E_MLS_SMARTPLAYLIST_FILTER_NOT_REGISTERED
//
// MessageText:
//
//  Playlist load error: The specified autoplaylist contains a filter type which is either invalid or is not installed on this computer%0
//
  NS_E_MLS_SMARTPLAYLIST_FILTER_NOT_REGISTERED = HRESULT($C00D1075);
  {$EXTERNALSYM NS_E_MLS_SMARTPLAYLIST_FILTER_NOT_REGISTERED}

//
// MessageId: NS_E_WMX_INVALID_FORMAT_OVER_NESTING
//
// MessageText:
//
//  Windows Media Player cannot play the file because the associated Windows Media metafile playlist is not valid.%0
//
  NS_E_WMX_INVALID_FORMAT_OVER_NESTING = HRESULT($C00D1076);
  {$EXTERNALSYM NS_E_WMX_INVALID_FORMAT_OVER_NESTING}

//
// WMP Core  Error codes
//
//
// MessageId: NS_E_WMPCORE_NOSOURCEURLSTRING
//
// MessageText:
//
//  Windows Media Player cannot find the file. Be sure the path is typed correctly. If it is, the file may not exist in the specified location, or the computer where the file is stored may be offline.%0
//
  NS_E_WMPCORE_NOSOURCEURLSTRING   = HRESULT($C00D107C);
  {$EXTERNALSYM NS_E_WMPCORE_NOSOURCEURLSTRING}

//
// MessageId: NS_E_WMPCORE_COCREATEFAILEDFORGITOBJECT
//
// MessageText:
//
//  Failed to create the Global Interface Table.%0
//
  NS_E_WMPCORE_COCREATEFAILEDFORGITOBJECT = HRESULT($C00D107D);
  {$EXTERNALSYM NS_E_WMPCORE_COCREATEFAILEDFORGITOBJECT}

//
// MessageId: NS_E_WMPCORE_FAILEDTOGETMARSHALLEDEVENTHANDLERINTERFACE
//
// MessageText:
//
//  Failed to get the marshaled graph event handler interface.%0
//
  NS_E_WMPCORE_FAILEDTOGETMARSHALLEDEVENTHANDLERINTERFACE = HRESULT($C00D107E);
  {$EXTERNALSYM NS_E_WMPCORE_FAILEDTOGETMARSHALLEDEVENTHANDLERINTERFACE}

//
// MessageId: NS_E_WMPCORE_BUFFERTOOSMALL
//
// MessageText:
//
//  Buffer is too small for copying media type.%0
//
  NS_E_WMPCORE_BUFFERTOOSMALL      = HRESULT($C00D107F);
  {$EXTERNALSYM NS_E_WMPCORE_BUFFERTOOSMALL}

//
// MessageId: NS_E_WMPCORE_UNAVAILABLE
//
// MessageText:
//
//  Current state of the player does not allow the operation.%0
//
  NS_E_WMPCORE_UNAVAILABLE         = HRESULT($C00D1080);
  {$EXTERNALSYM NS_E_WMPCORE_UNAVAILABLE}

//
// MessageId: NS_E_WMPCORE_INVALIDPLAYLISTMODE
//
// MessageText:
//
//  Playlist manager does not understand the current play mode (shuffle, normal etc).%0
//
  NS_E_WMPCORE_INVALIDPLAYLISTMODE = HRESULT($C00D1081);
  {$EXTERNALSYM NS_E_WMPCORE_INVALIDPLAYLISTMODE}

//
// MessageId: NS_E_WMPCORE_ITEMNOTINPLAYLIST
//
// MessageText:
//
//  The item is not in the playlist.%0
//
  NS_E_WMPCORE_ITEMNOTINPLAYLIST   = HRESULT($C00D1086);
  {$EXTERNALSYM NS_E_WMPCORE_ITEMNOTINPLAYLIST}

//
// MessageId: NS_E_WMPCORE_PLAYLISTEMPTY
//
// MessageText:
//
//  There are no items in this playlist. Add items to the playlist, and try again.%0
//
  NS_E_WMPCORE_PLAYLISTEMPTY       = HRESULT($C00D1087);
  {$EXTERNALSYM NS_E_WMPCORE_PLAYLISTEMPTY}

//
// MessageId: NS_E_WMPCORE_NOBROWSER
//
// MessageText:
//
//  The Web site cannot be accessed. A Web browser is not detected on your computer.%0
//
  NS_E_WMPCORE_NOBROWSER           = HRESULT($C00D1088);
  {$EXTERNALSYM NS_E_WMPCORE_NOBROWSER}

//
// MessageId: NS_E_WMPCORE_UNRECOGNIZED_MEDIA_URL
//
// MessageText:
//
//  Windows Media Player cannot find the specified file. Be sure the path is typed correctly. If it is, the file does not exist in the specified location, or the computer where the file is stored is offline.%0
//
  NS_E_WMPCORE_UNRECOGNIZED_MEDIA_URL = HRESULT($C00D1089);
  {$EXTERNALSYM NS_E_WMPCORE_UNRECOGNIZED_MEDIA_URL}

//
// MessageId: NS_E_WMPCORE_GRAPH_NOT_IN_LIST
//
// MessageText:
//
//  Graph with the specified URL was not found in the prerolled graph list.%0
//
  NS_E_WMPCORE_GRAPH_NOT_IN_LIST   = HRESULT($C00D108A);
  {$EXTERNALSYM NS_E_WMPCORE_GRAPH_NOT_IN_LIST}

//
// MessageId: NS_E_WMPCORE_PLAYLIST_EMPTY_OR_SINGLE_MEDIA
//
// MessageText:
//
//  There is only one item in the playlist.%0
//
  NS_E_WMPCORE_PLAYLIST_EMPTY_OR_SINGLE_MEDIA = HRESULT($C00D108B);
  {$EXTERNALSYM NS_E_WMPCORE_PLAYLIST_EMPTY_OR_SINGLE_MEDIA}

//
// MessageId: NS_E_WMPCORE_ERRORSINKNOTREGISTERED
//
// MessageText:
//
//  An error sink was never registered for the calling object.%0
//
  NS_E_WMPCORE_ERRORSINKNOTREGISTERED = HRESULT($C00D108C);
  {$EXTERNALSYM NS_E_WMPCORE_ERRORSINKNOTREGISTERED}

//
// MessageId: NS_E_WMPCORE_ERRORMANAGERNOTAVAILABLE
//
// MessageText:
//
//  The error manager is not available to respond to errors.%0
//
  NS_E_WMPCORE_ERRORMANAGERNOTAVAILABLE = HRESULT($C00D108D);
  {$EXTERNALSYM NS_E_WMPCORE_ERRORMANAGERNOTAVAILABLE}

//
// MessageId: NS_E_WMPCORE_WEBHELPFAILED
//
// MessageText:
//
//  Failed launching WebHelp URL.%0
//
  NS_E_WMPCORE_WEBHELPFAILED       = HRESULT($C00D108E);
  {$EXTERNALSYM NS_E_WMPCORE_WEBHELPFAILED}

//
// MessageId: NS_E_WMPCORE_MEDIA_ERROR_RESUME_FAILED
//
// MessageText:
//
//  Could not resume playing next item in playlist.%0
//
  NS_E_WMPCORE_MEDIA_ERROR_RESUME_FAILED = HRESULT($C00D108F);
  {$EXTERNALSYM NS_E_WMPCORE_MEDIA_ERROR_RESUME_FAILED}

//
// MessageId: NS_E_WMPCORE_NO_REF_IN_ENTRY
//
// MessageText:
//
//  Windows Media Player cannot play the file because the associated Windows Media metafile playlist is not valid.%0
//
  NS_E_WMPCORE_NO_REF_IN_ENTRY     = HRESULT($C00D1090);
  {$EXTERNALSYM NS_E_WMPCORE_NO_REF_IN_ENTRY}

//
// MessageId: NS_E_WMPCORE_WMX_LIST_ATTRIBUTE_NAME_EMPTY
//
// MessageText:
//
//  An empty string for playlist attribute name was found.%0
//
  NS_E_WMPCORE_WMX_LIST_ATTRIBUTE_NAME_EMPTY = HRESULT($C00D1091);
  {$EXTERNALSYM NS_E_WMPCORE_WMX_LIST_ATTRIBUTE_NAME_EMPTY}

//
// MessageId: NS_E_WMPCORE_WMX_LIST_ATTRIBUTE_NAME_ILLEGAL
//
// MessageText:
//
//  An invalid playlist attribute name was found.%0
//
  NS_E_WMPCORE_WMX_LIST_ATTRIBUTE_NAME_ILLEGAL = HRESULT($C00D1092);
  {$EXTERNALSYM NS_E_WMPCORE_WMX_LIST_ATTRIBUTE_NAME_ILLEGAL}

//
// MessageId: NS_E_WMPCORE_WMX_LIST_ATTRIBUTE_VALUE_EMPTY
//
// MessageText:
//
//  An empty string for a playlist attribute value was found.%0
//
  NS_E_WMPCORE_WMX_LIST_ATTRIBUTE_VALUE_EMPTY = HRESULT($C00D1093);
  {$EXTERNALSYM NS_E_WMPCORE_WMX_LIST_ATTRIBUTE_VALUE_EMPTY}

//
// MessageId: NS_E_WMPCORE_WMX_LIST_ATTRIBUTE_VALUE_ILLEGAL
//
// MessageText:
//
//  An illegal value for a playlist attribute was found.%0
//
  NS_E_WMPCORE_WMX_LIST_ATTRIBUTE_VALUE_ILLEGAL = HRESULT($C00D1094);
  {$EXTERNALSYM NS_E_WMPCORE_WMX_LIST_ATTRIBUTE_VALUE_ILLEGAL}

//
// MessageId: NS_E_WMPCORE_WMX_LIST_ITEM_ATTRIBUTE_NAME_EMPTY
//
// MessageText:
//
//  An empty string for a playlist item attribute name was found.%0
//
  NS_E_WMPCORE_WMX_LIST_ITEM_ATTRIBUTE_NAME_EMPTY = HRESULT($C00D1095);
  {$EXTERNALSYM NS_E_WMPCORE_WMX_LIST_ITEM_ATTRIBUTE_NAME_EMPTY}

//
// MessageId: NS_E_WMPCORE_WMX_LIST_ITEM_ATTRIBUTE_NAME_ILLEGAL
//
// MessageText:
//
//  An illegal value for a playlist item attribute name was found.%0
//
  NS_E_WMPCORE_WMX_LIST_ITEM_ATTRIBUTE_NAME_ILLEGAL = HRESULT($C00D1096);
  {$EXTERNALSYM NS_E_WMPCORE_WMX_LIST_ITEM_ATTRIBUTE_NAME_ILLEGAL}

//
// MessageId: NS_E_WMPCORE_WMX_LIST_ITEM_ATTRIBUTE_VALUE_EMPTY
//
// MessageText:
//
//  An illegal value for a playlist item attribute was found.%0
//
  NS_E_WMPCORE_WMX_LIST_ITEM_ATTRIBUTE_VALUE_EMPTY = HRESULT($C00D1097);
  {$EXTERNALSYM NS_E_WMPCORE_WMX_LIST_ITEM_ATTRIBUTE_VALUE_EMPTY}

//
// MessageId: NS_E_WMPCORE_LIST_ENTRY_NO_REF
//
// MessageText:
//
//  No entries found in the playlist file.%0
//
  NS_E_WMPCORE_LIST_ENTRY_NO_REF   = HRESULT($C00D1098);
  {$EXTERNALSYM NS_E_WMPCORE_LIST_ENTRY_NO_REF}

//
// MessageId: NS_E_WMPCORE_MISNAMED_FILE
//
// MessageText:
//
//  Windows Media Player cannot play the file. The file is either corrupt or the Player does not support the format you are trying to play.%0
//
  NS_E_WMPCORE_MISNAMED_FILE       = HRESULT($C00D1099);
  {$EXTERNALSYM NS_E_WMPCORE_MISNAMED_FILE}

//
// MessageId: NS_E_WMPCORE_CODEC_NOT_TRUSTED
//
// MessageText:
//
//  The codec downloaded for this media does not appear to be properly signed. Installation is not possible.%0
//
  NS_E_WMPCORE_CODEC_NOT_TRUSTED   = HRESULT($C00D109A);
  {$EXTERNALSYM NS_E_WMPCORE_CODEC_NOT_TRUSTED}

//
// MessageId: NS_E_WMPCORE_CODEC_NOT_FOUND
//
// MessageText:
//
//  Windows Media Player cannot play the file. One or more codecs required to play the file could not be found.%0
//
  NS_E_WMPCORE_CODEC_NOT_FOUND     = HRESULT($C00D109B);
  {$EXTERNALSYM NS_E_WMPCORE_CODEC_NOT_FOUND}

//
// MessageId: NS_E_WMPCORE_CODEC_DOWNLOAD_NOT_ALLOWED
//
// MessageText:
//
//  Some of the codecs required by this media are not installed on your system. Since the option for automatic codec acquisition is disabled, no codecs will be downloaded.%0
//
  NS_E_WMPCORE_CODEC_DOWNLOAD_NOT_ALLOWED = HRESULT($C00D109C);
  {$EXTERNALSYM NS_E_WMPCORE_CODEC_DOWNLOAD_NOT_ALLOWED}

//
// MessageId: NS_E_WMPCORE_ERROR_DOWNLOADING_PLAYLIST
//
// MessageText:
//
//  Failed to download the playlist file.%0
//
  NS_E_WMPCORE_ERROR_DOWNLOADING_PLAYLIST = HRESULT($C00D109D);
  {$EXTERNALSYM NS_E_WMPCORE_ERROR_DOWNLOADING_PLAYLIST}

//
// MessageId: NS_E_WMPCORE_FAILED_TO_BUILD_PLAYLIST
//
// MessageText:
//
//  Failed to build the playlist.%0
//
  NS_E_WMPCORE_FAILED_TO_BUILD_PLAYLIST = HRESULT($C00D109E);
  {$EXTERNALSYM NS_E_WMPCORE_FAILED_TO_BUILD_PLAYLIST}

//
// MessageId: NS_E_WMPCORE_PLAYLIST_ITEM_ALTERNATE_NONE
//
// MessageText:
//
//  Playlist has no alternates to switch into.%0
//
  NS_E_WMPCORE_PLAYLIST_ITEM_ALTERNATE_NONE = HRESULT($C00D109F);
  {$EXTERNALSYM NS_E_WMPCORE_PLAYLIST_ITEM_ALTERNATE_NONE}

//
// MessageId: NS_E_WMPCORE_PLAYLIST_ITEM_ALTERNATE_EXHAUSTED
//
// MessageText:
//
//  No more playlist alternates available to switch to.%0
//
  NS_E_WMPCORE_PLAYLIST_ITEM_ALTERNATE_EXHAUSTED = HRESULT($C00D10A0);
  {$EXTERNALSYM NS_E_WMPCORE_PLAYLIST_ITEM_ALTERNATE_EXHAUSTED}

//
// MessageId: NS_E_WMPCORE_PLAYLIST_ITEM_ALTERNATE_NAME_NOT_FOUND
//
// MessageText:
//
//  Could not find the name of the alternate playlist to switch into.%0
//
  NS_E_WMPCORE_PLAYLIST_ITEM_ALTERNATE_NAME_NOT_FOUND = HRESULT($C00D10A1);
  {$EXTERNALSYM NS_E_WMPCORE_PLAYLIST_ITEM_ALTERNATE_NAME_NOT_FOUND}

//
// MessageId: NS_E_WMPCORE_PLAYLIST_ITEM_ALTERNATE_MORPH_FAILED
//
// MessageText:
//
//  Failed to switch to an alternate for this media.%0
//
  NS_E_WMPCORE_PLAYLIST_ITEM_ALTERNATE_MORPH_FAILED = HRESULT($C00D10A2);
  {$EXTERNALSYM NS_E_WMPCORE_PLAYLIST_ITEM_ALTERNATE_MORPH_FAILED}

//
// MessageId: NS_E_WMPCORE_PLAYLIST_ITEM_ALTERNATE_INIT_FAILED
//
// MessageText:
//
//  Failed to initialize an alternate for the media.%0
//
  NS_E_WMPCORE_PLAYLIST_ITEM_ALTERNATE_INIT_FAILED = HRESULT($C00D10A3);
  {$EXTERNALSYM NS_E_WMPCORE_PLAYLIST_ITEM_ALTERNATE_INIT_FAILED}

//
// MessageId: NS_E_WMPCORE_MEDIA_ALTERNATE_REF_EMPTY
//
// MessageText:
//
//  No URL specified for the roll over Refs in the playlist file.%0
//
  NS_E_WMPCORE_MEDIA_ALTERNATE_REF_EMPTY = HRESULT($C00D10A4);
  {$EXTERNALSYM NS_E_WMPCORE_MEDIA_ALTERNATE_REF_EMPTY}

//
// MessageId: NS_E_WMPCORE_PLAYLIST_NO_EVENT_NAME
//
// MessageText:
//
//  Encountered a playlist with no name.%0
//
  NS_E_WMPCORE_PLAYLIST_NO_EVENT_NAME = HRESULT($C00D10A5);
  {$EXTERNALSYM NS_E_WMPCORE_PLAYLIST_NO_EVENT_NAME}

//
// MessageId: NS_E_WMPCORE_PLAYLIST_EVENT_ATTRIBUTE_ABSENT
//
// MessageText:
//
//  A required attribute in the event block of the playlist was not found.%0
//
  NS_E_WMPCORE_PLAYLIST_EVENT_ATTRIBUTE_ABSENT = HRESULT($C00D10A6);
  {$EXTERNALSYM NS_E_WMPCORE_PLAYLIST_EVENT_ATTRIBUTE_ABSENT}

//
// MessageId: NS_E_WMPCORE_PLAYLIST_EVENT_EMPTY
//
// MessageText:
//
//  No items were found in the event block of the playlist.%0
//
  NS_E_WMPCORE_PLAYLIST_EVENT_EMPTY = HRESULT($C00D10A7);
  {$EXTERNALSYM NS_E_WMPCORE_PLAYLIST_EVENT_EMPTY}

//
// MessageId: NS_E_WMPCORE_PLAYLIST_STACK_EMPTY
//
// MessageText:
//
//  No playlist was found while returning from a nested playlist.%0
//
  NS_E_WMPCORE_PLAYLIST_STACK_EMPTY = HRESULT($C00D10A8);
  {$EXTERNALSYM NS_E_WMPCORE_PLAYLIST_STACK_EMPTY}

//
// MessageId: NS_E_WMPCORE_CURRENT_MEDIA_NOT_ACTIVE
//
// MessageText:
//
//  The media item is not active currently.%0
//
  NS_E_WMPCORE_CURRENT_MEDIA_NOT_ACTIVE = HRESULT($C00D10A9);
  {$EXTERNALSYM NS_E_WMPCORE_CURRENT_MEDIA_NOT_ACTIVE}

//
// MessageId: NS_E_WMPCORE_USER_CANCEL
//
// MessageText:
//
//  Open was aborted by user.%0
//
  NS_E_WMPCORE_USER_CANCEL         = HRESULT($C00D10AB);
  {$EXTERNALSYM NS_E_WMPCORE_USER_CANCEL}

//
// MessageId: NS_E_WMPCORE_PLAYLIST_REPEAT_EMPTY
//
// MessageText:
//
//  No items were found inside the playlist repeat block.%0
//
  NS_E_WMPCORE_PLAYLIST_REPEAT_EMPTY = HRESULT($C00D10AC);
  {$EXTERNALSYM NS_E_WMPCORE_PLAYLIST_REPEAT_EMPTY}

//
// MessageId: NS_E_WMPCORE_PLAYLIST_REPEAT_START_MEDIA_NONE
//
// MessageText:
//
//  Media object corresponding to start of a playlist repeat block was not found.%0
//
  NS_E_WMPCORE_PLAYLIST_REPEAT_START_MEDIA_NONE = HRESULT($C00D10AD);
  {$EXTERNALSYM NS_E_WMPCORE_PLAYLIST_REPEAT_START_MEDIA_NONE}

//
// MessageId: NS_E_WMPCORE_PLAYLIST_REPEAT_END_MEDIA_NONE
//
// MessageText:
//
//  Media object corresponding to the end of a playlist repeat block was not found.%0
//
  NS_E_WMPCORE_PLAYLIST_REPEAT_END_MEDIA_NONE = HRESULT($C00D10AE);
  {$EXTERNALSYM NS_E_WMPCORE_PLAYLIST_REPEAT_END_MEDIA_NONE}

//
// MessageId: NS_E_WMPCORE_INVALID_PLAYLIST_URL
//
// MessageText:
//
//  Playlist URL supplied to the playlist manager is invalid.%0
//
  NS_E_WMPCORE_INVALID_PLAYLIST_URL = HRESULT($C00D10AF);
  {$EXTERNALSYM NS_E_WMPCORE_INVALID_PLAYLIST_URL}

//
// MessageId: NS_E_WMPCORE_MISMATCHED_RUNTIME
//
// MessageText:
//
//  Windows Media Player cannot play the file because it is corrupted.%0
//
  NS_E_WMPCORE_MISMATCHED_RUNTIME  = HRESULT($C00D10B0);
  {$EXTERNALSYM NS_E_WMPCORE_MISMATCHED_RUNTIME}

//
// MessageId: NS_E_WMPCORE_PLAYLIST_IMPORT_FAILED_NO_ITEMS
//
// MessageText:
//
//  Windows Media Player cannot import the playlist to Media Library because the playlist is empty.%0
//
  NS_E_WMPCORE_PLAYLIST_IMPORT_FAILED_NO_ITEMS = HRESULT($C00D10B1);
  {$EXTERNALSYM NS_E_WMPCORE_PLAYLIST_IMPORT_FAILED_NO_ITEMS}

//
// MessageId: NS_E_WMPCORE_VIDEO_TRANSFORM_FILTER_INSERTION
//
// MessageText:
//
//  An error has occurred that could prevent the changing of the video contrast on this media.%0
//
  NS_E_WMPCORE_VIDEO_TRANSFORM_FILTER_INSERTION = HRESULT($C00D10B2);
  {$EXTERNALSYM NS_E_WMPCORE_VIDEO_TRANSFORM_FILTER_INSERTION}

//
// MessageId: NS_E_WMPCORE_MEDIA_UNAVAILABLE
//
// MessageText:
//
//  Windows Media Player cannot play this file. Connect to the Internet or insert the removable media on which the file is located, and then try to play the file again.%0
//
  NS_E_WMPCORE_MEDIA_UNAVAILABLE   = HRESULT($C00D10B3);
  {$EXTERNALSYM NS_E_WMPCORE_MEDIA_UNAVAILABLE}

//
// MessageId: NS_E_WMPCORE_WMX_ENTRYREF_NO_REF
//
// MessageText:
//
//  The playlist contains an ENTRYREF for which no href was parsed. Check the syntax of playlist file.%0
//
  NS_E_WMPCORE_WMX_ENTRYREF_NO_REF = HRESULT($C00D10B4);
  {$EXTERNALSYM NS_E_WMPCORE_WMX_ENTRYREF_NO_REF}

//
// MessageId: NS_E_WMPCORE_NO_PLAYABLE_MEDIA_IN_PLAYLIST
//
// MessageText:
//
//  Windows Media Player cannot play any items in this playlist. For additional information, right-click an item that cannot be played, and then click Error Details.%0
//
  NS_E_WMPCORE_NO_PLAYABLE_MEDIA_IN_PLAYLIST = HRESULT($C00D10B5);
  {$EXTERNALSYM NS_E_WMPCORE_NO_PLAYABLE_MEDIA_IN_PLAYLIST}

//
// MessageId: NS_E_WMPCORE_PLAYLIST_EMPTY_NESTED_PLAYLIST_SKIPPED_ITEMS
//
// MessageText:
//
//  Windows Media Player cannot play some or all of the playlist items.%0
//
  NS_E_WMPCORE_PLAYLIST_EMPTY_NESTED_PLAYLIST_SKIPPED_ITEMS = HRESULT($C00D10B6);
  {$EXTERNALSYM NS_E_WMPCORE_PLAYLIST_EMPTY_NESTED_PLAYLIST_SKIPPED_ITEMS}

//
// MessageId: NS_E_WMPCORE_BUSY
//
// MessageText:
//
//  Windows Media Player cannot play the file at this time. Try again later.%0
//
  NS_E_WMPCORE_BUSY                = HRESULT($C00D10B7);
  {$EXTERNALSYM NS_E_WMPCORE_BUSY}

//
// MessageId: NS_E_WMPCORE_MEDIA_CHILD_PLAYLIST_UNAVAILABLE
//
// MessageText:
//
//  There is no child playlist available for this media item at this time.%0
//
  NS_E_WMPCORE_MEDIA_CHILD_PLAYLIST_UNAVAILABLE = HRESULT($C00D10B8);
  {$EXTERNALSYM NS_E_WMPCORE_MEDIA_CHILD_PLAYLIST_UNAVAILABLE}

//
// MessageId: NS_E_WMPCORE_MEDIA_NO_CHILD_PLAYLIST
//
// MessageText:
//
//  There is no child playlist for this media item.%0
//
  NS_E_WMPCORE_MEDIA_NO_CHILD_PLAYLIST = HRESULT($C00D10B9);
  {$EXTERNALSYM NS_E_WMPCORE_MEDIA_NO_CHILD_PLAYLIST}

//
// MessageId: NS_E_WMPCORE_FILE_NOT_FOUND
//
// MessageText:
//
//  Windows Media Player cannot play one or more files. Right-click the file, and then click Error Details to view information about the error.%0
//
  NS_E_WMPCORE_FILE_NOT_FOUND      = HRESULT($C00D10BA);
  {$EXTERNALSYM NS_E_WMPCORE_FILE_NOT_FOUND}

//
// MessageId: NS_E_WMPCORE_TEMP_FILE_NOT_FOUND
//
// MessageText:
//
//  The temporary file was not found.%0
//
  NS_E_WMPCORE_TEMP_FILE_NOT_FOUND = HRESULT($C00D10BB);
  {$EXTERNALSYM NS_E_WMPCORE_TEMP_FILE_NOT_FOUND}

//
// MessageId: NS_E_WMDM_REVOKED
//
// MessageText:
//
//  Windows Media Player cannot transfer media to the portable device without an update.  Please click More Information to find out how to update your device.%0
//
  NS_E_WMDM_REVOKED                = HRESULT($C00D10BC);
  {$EXTERNALSYM NS_E_WMDM_REVOKED}

//
// MessageId: NS_E_DDRAW_GENERIC
//
// MessageText:
//
//  Windows Media Player cannot play the video stream because of a problem with your video card.%0
//
  NS_E_DDRAW_GENERIC               = HRESULT($C00D10BD);
  {$EXTERNALSYM NS_E_DDRAW_GENERIC}

//
// MessageId: NS_E_DISPLAY_MODE_CHANGE_FAILED
//
// MessageText:
//
//  Windows Media Player failed to change the screen mode for fullscreen video playback.%0
//
  NS_E_DISPLAY_MODE_CHANGE_FAILED  = HRESULT($C00D10BE);
  {$EXTERNALSYM NS_E_DISPLAY_MODE_CHANGE_FAILED}

//
// MessageId: NS_E_PLAYLIST_CONTAINS_ERRORS
//
// MessageText:
//
//  One or more items in the playlist cannot be played. For more details, right-click an item in the playlist, and then click Error Details.%0
//
  NS_E_PLAYLIST_CONTAINS_ERRORS    = HRESULT($C00D10BF);
  {$EXTERNALSYM NS_E_PLAYLIST_CONTAINS_ERRORS}

//
// MessageId: NS_E_CHANGING_PROXY_NAME
//
// MessageText:
//
//  Can't change proxy name if the proxy setting is not set to custom.%0
//
  NS_E_CHANGING_PROXY_NAME         = HRESULT($C00D10C0);
  {$EXTERNALSYM NS_E_CHANGING_PROXY_NAME}

//
// MessageId: NS_E_CHANGING_PROXY_PORT
//
// MessageText:
//
//  Can't change proxy port if the proxy setting is not set to custom.%0
//
  NS_E_CHANGING_PROXY_PORT         = HRESULT($C00D10C1);
  {$EXTERNALSYM NS_E_CHANGING_PROXY_PORT}

//
// MessageId: NS_E_CHANGING_PROXY_EXCEPTIONLIST
//
// MessageText:
//
//  Can't change proxy exception list if the proxy setting is not set to custom.%0
//
  NS_E_CHANGING_PROXY_EXCEPTIONLIST = HRESULT($C00D10C2);
  {$EXTERNALSYM NS_E_CHANGING_PROXY_EXCEPTIONLIST}

//
// MessageId: NS_E_CHANGING_PROXYBYPASS
//
// MessageText:
//
//  Can't change proxy bypass flag if the proxy setting is not set to custom.%0
//
  NS_E_CHANGING_PROXYBYPASS        = HRESULT($C00D10C3);
  {$EXTERNALSYM NS_E_CHANGING_PROXYBYPASS}

//
// MessageId: NS_E_CHANGING_PROXY_PROTOCOL_NOT_FOUND
//
// MessageText:
//
//  Can't find specified protocol.%0
//
  NS_E_CHANGING_PROXY_PROTOCOL_NOT_FOUND = HRESULT($C00D10C4);
  {$EXTERNALSYM NS_E_CHANGING_PROXY_PROTOCOL_NOT_FOUND}

//
// MessageId: NS_E_GRAPH_NOAUDIOLANGUAGE
//
// MessageText:
//
//  Can't change language settings.  Either the graph has no audio, or the audio only supports one language.%0
//
  NS_E_GRAPH_NOAUDIOLANGUAGE       = HRESULT($C00D10C5);
  {$EXTERNALSYM NS_E_GRAPH_NOAUDIOLANGUAGE}

//
// MessageId: NS_E_GRAPH_NOAUDIOLANGUAGESELECTED
//
// MessageText:
//
//  The graph has no audio language selected.%0
//
  NS_E_GRAPH_NOAUDIOLANGUAGESELECTED = HRESULT($C00D10C6);
  {$EXTERNALSYM NS_E_GRAPH_NOAUDIOLANGUAGESELECTED}

//
// MessageId: NS_E_CORECD_NOTAMEDIACD
//
// MessageText:
//
//  This is not a media CD.%0
//
  NS_E_CORECD_NOTAMEDIACD          = HRESULT($C00D10C7);
  {$EXTERNALSYM NS_E_CORECD_NOTAMEDIACD}

//
// MessageId: NS_E_WMPCORE_MEDIA_URL_TOO_LONG
//
// MessageText:
//
//  Windows Media Player cannot play this file because the URL is too long.%0
//
  NS_E_WMPCORE_MEDIA_URL_TOO_LONG  = HRESULT($C00D10C8);
  {$EXTERNALSYM NS_E_WMPCORE_MEDIA_URL_TOO_LONG}

//
// MessageId: NS_E_WMPFLASH_CANT_FIND_COM_SERVER
//
// MessageText:
//
//  Windows Media Player needs the Macromedia Flash Player to play this content. Windows Media Player was not able to detect the Flash player on your system. To play the selected item, you must install the Macromedia Flash Player from the Macromedia Web site, and then try to play the item again.%0
//
  NS_E_WMPFLASH_CANT_FIND_COM_SERVER = HRESULT($C00D10C9);
  {$EXTERNALSYM NS_E_WMPFLASH_CANT_FIND_COM_SERVER}

//
// MessageId: NS_E_WMPFLASH_INCOMPATIBLEVERSION
//
// MessageText:
//
//  To play the selected item, you must install an updated version of the Macromedia Flash Player from the Macromedia Web site, and then try to play the item again.%0
//
  NS_E_WMPFLASH_INCOMPATIBLEVERSION = HRESULT($C00D10CA);
  {$EXTERNALSYM NS_E_WMPFLASH_INCOMPATIBLEVERSION}

//
// MessageId: NS_E_WMPOCXGRAPH_IE_DISALLOWS_ACTIVEX_CONTROLS
//
// MessageText:
//
//  The use of ActiveX controls has been turned off in Internet Explorer. As a result Windows Media Player will not be able to playback this content.%0
//
  NS_E_WMPOCXGRAPH_IE_DISALLOWS_ACTIVEX_CONTROLS = HRESULT($C00D10CB);
  {$EXTERNALSYM NS_E_WMPOCXGRAPH_IE_DISALLOWS_ACTIVEX_CONTROLS}

//
// MessageId: NS_E_NEED_CORE_REFERENCE
//
// MessageText:
//
//  The use of this method requires an existing reference to the Player object.%0
//
  NS_E_NEED_CORE_REFERENCE         = HRESULT($C00D10CC);
  {$EXTERNALSYM NS_E_NEED_CORE_REFERENCE}

//
// MessageId: NS_E_MEDIACD_READ_ERROR
//
// MessageText:
//
//  There was an error reading from the CD-ROM.%0
//
  NS_E_MEDIACD_READ_ERROR          = HRESULT($C00D10CD);
  {$EXTERNALSYM NS_E_MEDIACD_READ_ERROR}

//
// MessageId: NS_E_IE_DISALLOWS_ACTIVEX_CONTROLS
//
// MessageText:
//
//  Internet Explorer is set to disallow ActiveX controls.%0
//
  NS_E_IE_DISALLOWS_ACTIVEX_CONTROLS = HRESULT($C00D10CE);
  {$EXTERNALSYM NS_E_IE_DISALLOWS_ACTIVEX_CONTROLS}

//
// MessageId: NS_E_FLASH_PLAYBACK_NOT_ALLOWED
//
// MessageText:
//
//  Flash playback has been turned off in Windows Media Player.%0
//
  NS_E_FLASH_PLAYBACK_NOT_ALLOWED  = HRESULT($C00D10CF);
  {$EXTERNALSYM NS_E_FLASH_PLAYBACK_NOT_ALLOWED}

//
// MessageId: NS_E_UNABLE_TO_CREATE_RIP_LOCATION
//
// MessageText:
//
//  Media Player was unable to create a valid location to copy the CD track.%0
//
  NS_E_UNABLE_TO_CREATE_RIP_LOCATION = HRESULT($C00D10D0);
  {$EXTERNALSYM NS_E_UNABLE_TO_CREATE_RIP_LOCATION}

//
// MessageId: NS_E_WMPCORE_SOME_CODECS_MISSING
//
// MessageText:
//
//  One or more codecs required to open this content could not be found.%0
//
  NS_E_WMPCORE_SOME_CODECS_MISSING = HRESULT($C00D10D1);
  {$EXTERNALSYM NS_E_WMPCORE_SOME_CODECS_MISSING}

//
// WMP Core  Success codes
//
//
// MessageId: NS_S_WMPCORE_PLAYLISTCLEARABORT
//
// MessageText:
//
//  Failed to clear playlist because it was aborted by user.%0
//
  NS_S_WMPCORE_PLAYLISTCLEARABORT  = HRESULT($000D10FE);
  {$EXTERNALSYM NS_S_WMPCORE_PLAYLISTCLEARABORT}

//
// MessageId: NS_S_WMPCORE_PLAYLISTREMOVEITEMABORT
//
// MessageText:
//
//  Failed to remove item in the playlist since it was aborted by user.%0
//
  NS_S_WMPCORE_PLAYLISTREMOVEITEMABORT = HRESULT($000D10FF);
  {$EXTERNALSYM NS_S_WMPCORE_PLAYLISTREMOVEITEMABORT}

//
// MessageId: NS_S_WMPCORE_PLAYLIST_CREATION_PENDING
//
// MessageText:
//
//  Playlist is being generated asynchronously.%0
//
  NS_S_WMPCORE_PLAYLIST_CREATION_PENDING = HRESULT($000D1102);
  {$EXTERNALSYM NS_S_WMPCORE_PLAYLIST_CREATION_PENDING}

//
// MessageId: NS_S_WMPCORE_MEDIA_VALIDATION_PENDING
//
// MessageText:
//
//  Validation of the media is pending...%0
//
  NS_S_WMPCORE_MEDIA_VALIDATION_PENDING = HRESULT($000D1103);
  {$EXTERNALSYM NS_S_WMPCORE_MEDIA_VALIDATION_PENDING}

//
// MessageId: NS_S_WMPCORE_PLAYLIST_REPEAT_SECONDARY_SEGMENTS_IGNORED
//
// MessageText:
//
//  Encountered more than one Repeat block during ASX processing.%0
//
  NS_S_WMPCORE_PLAYLIST_REPEAT_SECONDARY_SEGMENTS_IGNORED = HRESULT($000D1104);
  {$EXTERNALSYM NS_S_WMPCORE_PLAYLIST_REPEAT_SECONDARY_SEGMENTS_IGNORED}

//
// MessageId: NS_S_WMPCORE_COMMAND_NOT_AVAILABLE
//
// MessageText:
//
//  Current state of WMP disallows calling this method or property.%0
//
  NS_S_WMPCORE_COMMAND_NOT_AVAILABLE = HRESULT($000D1105);
  {$EXTERNALSYM NS_S_WMPCORE_COMMAND_NOT_AVAILABLE}

//
// MessageId: NS_S_WMPCORE_PLAYLIST_NAME_AUTO_GENERATED
//
// MessageText:
//
//  Name for the playlist has been auto generated.%0
//
  NS_S_WMPCORE_PLAYLIST_NAME_AUTO_GENERATED = HRESULT($000D1106);
  {$EXTERNALSYM NS_S_WMPCORE_PLAYLIST_NAME_AUTO_GENERATED}

//
// MessageId: NS_S_WMPCORE_PLAYLIST_IMPORT_MISSING_ITEMS
//
// MessageText:
//
//  The imported playlist does not contain all items from the original.%0
//
  NS_S_WMPCORE_PLAYLIST_IMPORT_MISSING_ITEMS = HRESULT($000D1107);
  {$EXTERNALSYM NS_S_WMPCORE_PLAYLIST_IMPORT_MISSING_ITEMS}

//
// MessageId: NS_S_WMPCORE_PLAYLIST_COLLAPSED_TO_SINGLE_MEDIA
//
// MessageText:
//
//  The M3U playlist has been ignored because it only contains one item.%0
//
  NS_S_WMPCORE_PLAYLIST_COLLAPSED_TO_SINGLE_MEDIA = HRESULT($000D1108);
  {$EXTERNALSYM NS_S_WMPCORE_PLAYLIST_COLLAPSED_TO_SINGLE_MEDIA}

//
// MessageId: NS_S_WMPCORE_MEDIA_CHILD_PLAYLIST_OPEN_PENDING
//
// MessageText:
//
//  The open for the child playlist associated with this media is pending.%0
//
  NS_S_WMPCORE_MEDIA_CHILD_PLAYLIST_OPEN_PENDING = HRESULT($000D1109);
  {$EXTERNALSYM NS_S_WMPCORE_MEDIA_CHILD_PLAYLIST_OPEN_PENDING}

//
// MessageId: NS_S_WMPCORE_MORE_NODES_AVAIABLE
//
// MessageText:
//
//  More nodes support the interface requested, but the array for returning them is full.%0
//
  NS_S_WMPCORE_MORE_NODES_AVAIABLE = HRESULT($000D110A);
  {$EXTERNALSYM NS_S_WMPCORE_MORE_NODES_AVAIABLE}

//
// WMP Internet Manager error codes
//
//
// MessageId: NS_E_WMPIM_USEROFFLINE
//
// MessageText:
//
//  Windows Media Player has detected that you are not connected to the Internet. Connect to the Internet, and then try again.%0
//
  NS_E_WMPIM_USEROFFLINE           = HRESULT($C00D1126);
  {$EXTERNALSYM NS_E_WMPIM_USEROFFLINE}

//
// MessageId: NS_E_WMPIM_USERCANCELED
//
// MessageText:
//
//  User cancelled attempt to connect to the Internet.%0
//
  NS_E_WMPIM_USERCANCELED          = HRESULT($C00D1127);
  {$EXTERNALSYM NS_E_WMPIM_USERCANCELED}

//
// MessageId: NS_E_WMPIM_DIALUPFAILED
//
// MessageText:
//
//  Attempt to dial connection to the Internet failed.%0
//
  NS_E_WMPIM_DIALUPFAILED          = HRESULT($C00D1128);
  {$EXTERNALSYM NS_E_WMPIM_DIALUPFAILED}

//
// MessageId: NS_E_WINSOCK_ERROR_STRING
//
// MessageText:
//
//  Windows Media Player has encountered an unknown network error.%0
//
  NS_E_WINSOCK_ERROR_STRING        = HRESULT($C00D1129);
  {$EXTERNALSYM NS_E_WINSOCK_ERROR_STRING}

//
// WMP Backup and restore error and success codes
//
//
// MessageId: NS_E_WMPBR_NOLISTENER
//
// MessageText:
//
//  No window is currently listening to Backup and Restore events.%0
//
  NS_E_WMPBR_NOLISTENER            = HRESULT($C00D1130);
  {$EXTERNALSYM NS_E_WMPBR_NOLISTENER}

//
// MessageId: NS_E_WMPBR_BACKUPCANCEL
//
// MessageText:
//
//  Backup of your licenses has been cancelled.  Please try again to ensure license backup.%0
//
  NS_E_WMPBR_BACKUPCANCEL          = HRESULT($C00D1131);
  {$EXTERNALSYM NS_E_WMPBR_BACKUPCANCEL}

//
// MessageId: NS_E_WMPBR_RESTORECANCEL
//
// MessageText:
//
//  The licenses were not restored because the restoration was cancelled.%0
//
  NS_E_WMPBR_RESTORECANCEL         = HRESULT($C00D1132);
  {$EXTERNALSYM NS_E_WMPBR_RESTORECANCEL}

//
// MessageId: NS_E_WMPBR_ERRORWITHURL
//
// MessageText:
//
//  An error occurred during the backup or restore operation that requires a web page be displayed to the user.%0
//
  NS_E_WMPBR_ERRORWITHURL          = HRESULT($C00D1133);
  {$EXTERNALSYM NS_E_WMPBR_ERRORWITHURL}

//
// MessageId: NS_E_WMPBR_NAMECOLLISION
//
// MessageText:
//
//  The licenses were not backed up because the backup was cancelled.%0
//
  NS_E_WMPBR_NAMECOLLISION         = HRESULT($C00D1134);
  {$EXTERNALSYM NS_E_WMPBR_NAMECOLLISION}

//
// MessageId: NS_S_WMPBR_SUCCESS
//
// MessageText:
//
//  Backup or Restore successful!.%0
//
  NS_S_WMPBR_SUCCESS               = HRESULT($000D1135);
  {$EXTERNALSYM NS_S_WMPBR_SUCCESS}

//
// MessageId: NS_S_WMPBR_PARTIALSUCCESS
//
// MessageText:
//
//  Transfer complete with limitations.%0
//
  NS_S_WMPBR_PARTIALSUCCESS        = HRESULT($000D1136);
  {$EXTERNALSYM NS_S_WMPBR_PARTIALSUCCESS}

//
// MessageId: NS_E_WMPBR_DRIVE_INVALID
//
// MessageText:
//
//  The location specified for restoring licenses is not valid. Choose another location, and then try again.%0
//
  NS_E_WMPBR_DRIVE_INVALID         = HRESULT($C00D1137);
  {$EXTERNALSYM NS_E_WMPBR_DRIVE_INVALID}

//
// WMP Effects Success codes
//
//
// MessageId: NS_S_WMPEFFECT_TRANSPARENT
//
// MessageText:
//
//  Request to the effects control to change transparency status to transparent.%0
//
  NS_S_WMPEFFECT_TRANSPARENT       = HRESULT($000D1144);
  {$EXTERNALSYM NS_S_WMPEFFECT_TRANSPARENT}

//
// MessageId: NS_S_WMPEFFECT_OPAQUE
//
// MessageText:
//
//  Request to the effects control to change transparency status to opaque.%0
//
  NS_S_WMPEFFECT_OPAQUE            = HRESULT($000D1145);
  {$EXTERNALSYM NS_S_WMPEFFECT_OPAQUE}

//
// WMP Application Success codes
//
//
// MessageId: NS_S_OPERATION_PENDING
//
// MessageText:
//
//  The requested application pane is performing an operation and will not be relased.%0
//
  NS_S_OPERATION_PENDING           = HRESULT($000D114E);
  {$EXTERNALSYM NS_S_OPERATION_PENDING}

//
// WMP DVD Error Codes
//
//
// MessageId: NS_E_DVD_NO_SUBPICTURE_STREAM
//
// MessageText:
//
//  Windows Media Player cannot display subtitles or highlights in menus. Reinstall
//  the DVD decoder or contact your device manufacturer to obtain an updated decoder,
//  and then try again.%0
//
  NS_E_DVD_NO_SUBPICTURE_STREAM    = HRESULT($C00D1162);
  {$EXTERNALSYM NS_E_DVD_NO_SUBPICTURE_STREAM}

//
// MessageId: NS_E_DVD_COPY_PROTECT
//
// MessageText:
//
//  Windows Media Player cannot play this DVD because a problem occurred with digital copyright protection.%0
//
  NS_E_DVD_COPY_PROTECT            = HRESULT($C00D1163);
  {$EXTERNALSYM NS_E_DVD_COPY_PROTECT}

//
// MessageId: NS_E_DVD_AUTHORING_PROBLEM
//
// MessageText:
//
//  Windows Media Player cannot play this DVD because the disc is incompatible with the Player.%0
//
  NS_E_DVD_AUTHORING_PROBLEM       = HRESULT($C00D1164);
  {$EXTERNALSYM NS_E_DVD_AUTHORING_PROBLEM}

//
// MessageId: NS_E_DVD_INVALID_DISC_REGION
//
// MessageText:
//
//  Windows Media Player cannot play this DVD because the disc prohibits playback in your region of the world. You must obtain a disc that is intended for your geographic region.%0
//
  NS_E_DVD_INVALID_DISC_REGION     = HRESULT($C00D1165);
  {$EXTERNALSYM NS_E_DVD_INVALID_DISC_REGION}

//
// MessageId: NS_E_DVD_COMPATIBLE_VIDEO_CARD
//
// MessageText:
//
//  Windows Media Player cannot play this DVD because your video card does not support DVD playback.%0
//
  NS_E_DVD_COMPATIBLE_VIDEO_CARD   = HRESULT($C00D1166);
  {$EXTERNALSYM NS_E_DVD_COMPATIBLE_VIDEO_CARD}

//
// MessageId: NS_E_DVD_MACROVISION
//
// MessageText:
//
//  Windows Media Player cannot play this DVD because a problem occurred with copyright protection.%0
//
  NS_E_DVD_MACROVISION             = HRESULT($C00D1167);
  {$EXTERNALSYM NS_E_DVD_MACROVISION}

//
// MessageId: NS_E_DVD_SYSTEM_DECODER_REGION
//
// MessageText:
//
//  Windows Media Player cannot play this DVD because the region assigned to your DVD drive does not match the region assigned to your DVD decoder.%0
//
  NS_E_DVD_SYSTEM_DECODER_REGION   = HRESULT($C00D1168);
  {$EXTERNALSYM NS_E_DVD_SYSTEM_DECODER_REGION}

//
// MessageId: NS_E_DVD_DISC_DECODER_REGION
//
// MessageText:
//
//  Windows Media Player cannot play this DVD because the disc prohibits playback
// in your region of the world. To play the disc by using the Player, you must
// obtain a disc that is intended for your geographic region.%0
//
  NS_E_DVD_DISC_DECODER_REGION     = HRESULT($C00D1169);
  {$EXTERNALSYM NS_E_DVD_DISC_DECODER_REGION}

//
// MessageId: NS_E_DVD_NO_VIDEO_STREAM
//
// MessageText:
//
// Windows Media Player is currently unable to play DVD video. Try decreasing
// the number of colors displayed on your monitor or decreasing the screen
// resolution. For additional solutions, click More Information to access the
// DVD Troubleshooter.%0
//
  NS_E_DVD_NO_VIDEO_STREAM         = HRESULT($C00D116A);
  {$EXTERNALSYM NS_E_DVD_NO_VIDEO_STREAM}

//
// MessageId: NS_E_DVD_NO_AUDIO_STREAM
//
// MessageText:
//
//  Windows Media Player cannot play DVD audio. Verify that your sound card is set up correctly, and then try again.%0
//
  NS_E_DVD_NO_AUDIO_STREAM         = HRESULT($C00D116B);
  {$EXTERNALSYM NS_E_DVD_NO_AUDIO_STREAM}

//
// MessageId: NS_E_DVD_GRAPH_BUILDING
//
// MessageText:
//
//  Windows Media Player cannot play DVD video. Close any open files and quit any other running programs, and then try again. If the problem continues, restart your computer.%0
//
  NS_E_DVD_GRAPH_BUILDING          = HRESULT($C00D116C);
  {$EXTERNALSYM NS_E_DVD_GRAPH_BUILDING}

//
// MessageId: NS_E_DVD_NO_DECODER
//
// MessageText:
//
//  Windows Media Player cannot play this DVD because a compatible DVD decoder is not installed on your computer.%0
//
  NS_E_DVD_NO_DECODER              = HRESULT($C00D116D);
  {$EXTERNALSYM NS_E_DVD_NO_DECODER}

//
// MessageId: NS_E_DVD_PARENTAL
//
// MessageText:
//
//  Windows Media Player cannot play this DVD segment because the segment has a parental rating higher than the rating you are authorized to view.%0
//
  NS_E_DVD_PARENTAL                = HRESULT($C00D116E);
  {$EXTERNALSYM NS_E_DVD_PARENTAL}

//
// MessageId: NS_E_DVD_CANNOT_JUMP
//
// MessageText:
//
//  Windows Media Player cannot skip to the requested location in the DVD at this time.%0
//
  NS_E_DVD_CANNOT_JUMP             = HRESULT($C00D116F);
  {$EXTERNALSYM NS_E_DVD_CANNOT_JUMP}

//
// MessageId: NS_E_DVD_DEVICE_CONTENTION
//
// MessageText:
//
//  Windows Media Player cannot play this DVD because it is currently in use by another program. Quit the other program that is using the DVD, and then try to play it again.%0
//
  NS_E_DVD_DEVICE_CONTENTION       = HRESULT($C00D1170);
  {$EXTERNALSYM NS_E_DVD_DEVICE_CONTENTION}

//
// MessageId: NS_E_DVD_NO_VIDEO_MEMORY
//
// MessageText:
//
//  Windows Media Player cannot play DVD video. Double-click Display in Control Panel to lower your screen resolution and color quality settings.%0
//
  NS_E_DVD_NO_VIDEO_MEMORY         = HRESULT($C00D1171);
  {$EXTERNALSYM NS_E_DVD_NO_VIDEO_MEMORY}

//
// MessageId: NS_E_DVD_CANNOT_COPY_PROTECTED
//
// MessageText:
//
//  Windows Media Player cannot copy this DVD because it is copy protected.%0
//
  NS_E_DVD_CANNOT_COPY_PROTECTED   = HRESULT($C00D1172);
  {$EXTERNALSYM NS_E_DVD_CANNOT_COPY_PROTECTED}

//
// MessageId: NS_E_DVD_REQUIRED_PROPERTY_NOT_SET
//
// MessageText:
//
//  One of more of the required properties has not been set.%0
//
  NS_E_DVD_REQUIRED_PROPERTY_NOT_SET = HRESULT($C00D1173);
  {$EXTERNALSYM NS_E_DVD_REQUIRED_PROPERTY_NOT_SET}

//
// MessageId: NS_E_DVD_INVALID_TITLE_CHAPTER
//
// MessageText:
//
//  The specified title and/or chapter number does not exist on this DVD.%0
//
  NS_E_DVD_INVALID_TITLE_CHAPTER   = HRESULT($C00D1174);
  {$EXTERNALSYM NS_E_DVD_INVALID_TITLE_CHAPTER}

//
// WMP PDA Error codes
//
//
// MessageId: NS_E_NO_CD_BURNER
//
// MessageText:
//
//  A CD recorder (burner) was not detected. Connect a CD recorder, and try copying again.%0
//
  NS_E_NO_CD_BURNER                = HRESULT($C00D1176);
  {$EXTERNALSYM NS_E_NO_CD_BURNER}

//
// MessageId: NS_E_DEVICE_IS_NOT_READY
//
// MessageText:
//
//  Windows Media Player does not detect any storage media in the selected device. Insert media into the device.%0
//
  NS_E_DEVICE_IS_NOT_READY         = HRESULT($C00D1177);
  {$EXTERNALSYM NS_E_DEVICE_IS_NOT_READY}

//
// MessageId: NS_E_PDA_UNSUPPORTED_FORMAT
//
// MessageText:
//
//  Windows Media Player cannot play the specified file. Your portable device does not support the specified format.%0
//
  NS_E_PDA_UNSUPPORTED_FORMAT      = HRESULT($C00D1178);
  {$EXTERNALSYM NS_E_PDA_UNSUPPORTED_FORMAT}

//
// MessageId: NS_E_NO_PDA
//
// MessageText:
//
//  Windows Media Player cannot detect a connected portable device. Connect your portable device, and try again.%0
//
  NS_E_NO_PDA                      = HRESULT($C00D1179);
  {$EXTERNALSYM NS_E_NO_PDA}

//
// MessageId: NS_E_PDA_UNSPECIFIED_ERROR
//
// MessageText:
//
//  Windows Media Player has encountered an error on the portable device.%0
//
  NS_E_PDA_UNSPECIFIED_ERROR       = HRESULT($C00D117A);
  {$EXTERNALSYM NS_E_PDA_UNSPECIFIED_ERROR}

//
// MessageId: NS_E_MEMSTORAGE_BAD_DATA
//
// MessageText:
//
//  Windows Media Player encountered an internal error in accessing a memory-based storage during a CD burning task.%0
//
  NS_E_MEMSTORAGE_BAD_DATA         = HRESULT($C00D117B);
  {$EXTERNALSYM NS_E_MEMSTORAGE_BAD_DATA}

//
// MessageId: NS_E_PDA_FAIL_SELECT_DEVICE
//
// MessageText:
//
//  Windows Media Player encountered an internal error when selecting a PDA or CD device.%0
//
  NS_E_PDA_FAIL_SELECT_DEVICE      = HRESULT($C00D117C);
  {$EXTERNALSYM NS_E_PDA_FAIL_SELECT_DEVICE}

//
// MessageId: NS_E_PDA_FAIL_READ_WAVE_FILE
//
// MessageText:
//
//  Windows Media Player failed to open or read data from a wave file.%0
//
  NS_E_PDA_FAIL_READ_WAVE_FILE     = HRESULT($C00D117D);
  {$EXTERNALSYM NS_E_PDA_FAIL_READ_WAVE_FILE}

//
// MessageId: NS_E_IMAPI_LOSSOFSTREAMING
//
// MessageText:
//
//  Windows Media Player did not copy all the selected items. The Player has
//  reduced the recording speed of your CD drive to solve the problem. To copy
//  all the selected items, insert a blank CD in the drive, and try again.%0
//
  NS_E_IMAPI_LOSSOFSTREAMING       = HRESULT($C00D117E);
  {$EXTERNALSYM NS_E_IMAPI_LOSSOFSTREAMING}

//
// MessageId: NS_E_PDA_DEVICE_FULL
//
// MessageText:
//
//  There is not enough storage space on the portable device to complete this operation. Delete some unneeded files on the portable device, and then try again.%0
//
  NS_E_PDA_DEVICE_FULL             = HRESULT($C00D117F);
  {$EXTERNALSYM NS_E_PDA_DEVICE_FULL}

//
// MessageId: NS_E_FAIL_LAUNCH_ROXIO_PLUGIN
//
// MessageText:
//
//  Windows Media Player cannot copy the files to the CD. Verify that a CD-R or
//  CD-RW drive is connected to your computer, and then try again. If the problem
//  continues, reinstall the Player.%0
//
  NS_E_FAIL_LAUNCH_ROXIO_PLUGIN    = HRESULT($C00D1180);
  {$EXTERNALSYM NS_E_FAIL_LAUNCH_ROXIO_PLUGIN}

//
// MessageId: NS_E_PDA_DEVICE_FULL_IN_SESSION
//
// MessageText:
//
//  Windows Media Player failed to copy some files to device because the device is out of space.%0
//
  NS_E_PDA_DEVICE_FULL_IN_SESSION  = HRESULT($C00D1181);
  {$EXTERNALSYM NS_E_PDA_DEVICE_FULL_IN_SESSION}

//
// MessageId: NS_E_IMAPI_MEDIUM_INVALIDTYPE
//
// MessageText:
//
//  The medium in the drive is invalid. Please insert a blank CD-R or a CD-RW into the drive, and then try again.%0
//
  NS_E_IMAPI_MEDIUM_INVALIDTYPE    = HRESULT($C00D1182);
  {$EXTERNALSYM NS_E_IMAPI_MEDIUM_INVALIDTYPE}

//
// General Remapped Error codes in WMP
//
//
// MessageId: NS_E_WMP_PROTOCOL_PROBLEM
//
// MessageText:
//
//  Windows Media Player could not open the specified URL. Be sure Windows Media Player is configured to use all available protocols, and then try again.%0
//
  NS_E_WMP_PROTOCOL_PROBLEM        = HRESULT($C00D1194);
  {$EXTERNALSYM NS_E_WMP_PROTOCOL_PROBLEM}

//
// MessageId: NS_E_WMP_NO_DISK_SPACE
//
// MessageText:
//
//  Windows Media Player cannot open the file because there is not enough disk space on your computer. Delete some unneeded files on your hard disk, and then try again.%0
//
  NS_E_WMP_NO_DISK_SPACE           = HRESULT($C00D1195);
  {$EXTERNALSYM NS_E_WMP_NO_DISK_SPACE}

//
// MessageId: NS_E_WMP_LOGON_FAILURE
//
// MessageText:
//
//  Windows Media Player cannot copy or play the file because the server denied access to it. Verify that you have access rights to the file, and then try again.%0
//
  NS_E_WMP_LOGON_FAILURE           = HRESULT($C00D1196);
  {$EXTERNALSYM NS_E_WMP_LOGON_FAILURE}

//
// MessageId: NS_E_WMP_CANNOT_FIND_FILE
//
// MessageText:
//
//  Windows Media Player cannot find the specified file. Be sure the path is
//  typed correctly. If it is, the file does not exist at the specified location,
//  or the computer where the file is stored is offline.%0
//
  NS_E_WMP_CANNOT_FIND_FILE        = HRESULT($C00D1197);
  {$EXTERNALSYM NS_E_WMP_CANNOT_FIND_FILE}

//
// MessageId: NS_E_WMP_SERVER_INACCESSIBLE
//
// MessageText:
//
//  Windows Media Player cannot connect to the server. The server name may be incorrect or the server is busy. Try again later.%0
//
  NS_E_WMP_SERVER_INACCESSIBLE     = HRESULT($C00D1198);
  {$EXTERNALSYM NS_E_WMP_SERVER_INACCESSIBLE}

//
// MessageId: NS_E_WMP_UNSUPPORTED_FORMAT
//
// MessageText:
//
//  Windows Media Player cannot play the file. The file is either corrupt or the Player does not support the format you are trying to play.%0
//
  NS_E_WMP_UNSUPPORTED_FORMAT      = HRESULT($C00D1199);
  {$EXTERNALSYM NS_E_WMP_UNSUPPORTED_FORMAT}

//
// MessageId: NS_E_WMP_DSHOW_UNSUPPORTED_FORMAT
//
// MessageText:
//
//  Windows Media Player cannot play the file. The file may be formatted with
//  an unsupported codec, or the Internet security setting on your computer is
//  set too high. Lower your browser's security setting, and then try again.%0
//
  NS_E_WMP_DSHOW_UNSUPPORTED_FORMAT = HRESULT($C00D119A);
  {$EXTERNALSYM NS_E_WMP_DSHOW_UNSUPPORTED_FORMAT}

//
// MessageId: NS_E_WMP_PLAYLIST_EXISTS
//
// MessageText:
//
//  Windows Media Player cannot create the playlist because the name already exists. Type a different playlist name.%0
//
  NS_E_WMP_PLAYLIST_EXISTS         = HRESULT($C00D119B);
  {$EXTERNALSYM NS_E_WMP_PLAYLIST_EXISTS}

//
// MessageId: NS_E_WMP_NONMEDIA_FILES
//
// MessageText:
//
//  Windows Media Player could not delete the playlist because it contains
//  non-digital media files. Any digital media files in the playlist were deleted.
//  Use Windows Explorer to delete non-digital media files, and then try
//  deleting the playlist again.%0
//
  NS_E_WMP_NONMEDIA_FILES          = HRESULT($C00D119C);
  {$EXTERNALSYM NS_E_WMP_NONMEDIA_FILES}

//
// MessageId: NS_E_WMP_INVALID_ASX
//
// MessageText:
//
//  Windows Media Player cannot play the file because the associated playlist is not valid.%0
//
  NS_E_WMP_INVALID_ASX             = HRESULT($C00D119D);
  {$EXTERNALSYM NS_E_WMP_INVALID_ASX}

//
// MessageId: NS_E_WMP_ALREADY_IN_USE
//
// MessageText:
//
//  Windows Media Player is already in use. Stop playing any content and close all Player dialog boxes and then try again.%0
//
  NS_E_WMP_ALREADY_IN_USE          = HRESULT($C00D119E);
  {$EXTERNALSYM NS_E_WMP_ALREADY_IN_USE}

//
// MessageId: NS_E_WMP_IMAPI_FAILURE
//
// MessageText:
//
//  Windows Media Player has encountered an unknown error with your recordable disc.%0
//
  NS_E_WMP_IMAPI_FAILURE           = HRESULT($C00D119F);
  {$EXTERNALSYM NS_E_WMP_IMAPI_FAILURE}

//
// MessageId: NS_E_WMP_WMDM_FAILURE
//
// MessageText:
//
//  Windows Media Player has encountered an unknown error with your portable device.  Reconnect your portable device and try again.%0
//
  NS_E_WMP_WMDM_FAILURE            = HRESULT($C00D11A0);
  {$EXTERNALSYM NS_E_WMP_WMDM_FAILURE}

//
// MessageId: NS_E_WMP_CODEC_NEEDED_WITH_4CC
//
// MessageText:
//
//  The %s codec is required to play this file. To determine if this codec is available to download from the Web, click Web Help.%0
//
  NS_E_WMP_CODEC_NEEDED_WITH_4CC   = HRESULT($C00D11A1);
  {$EXTERNALSYM NS_E_WMP_CODEC_NEEDED_WITH_4CC}

//
// MessageId: NS_E_WMP_CODEC_NEEDED_WITH_FORMATTAG
//
// MessageText:
//
//  The audio codec identified by the format tag %s is required to play this file. To determine if this codec is available to download from the Web, click Web Help.%0
//
  NS_E_WMP_CODEC_NEEDED_WITH_FORMATTAG = HRESULT($C00D11A2);
  {$EXTERNALSYM NS_E_WMP_CODEC_NEEDED_WITH_FORMATTAG}

//
// MessageId: NS_E_WMP_MSSAP_NOT_AVAILABLE
//
// MessageText:
//
//  To play this file, you must install the latest Windows XP service pack.  To install the service pack from the Windows Update Web site, click Web Help.%0
//
  NS_E_WMP_MSSAP_NOT_AVAILABLE     = HRESULT($C00D11A3);
  {$EXTERNALSYM NS_E_WMP_MSSAP_NOT_AVAILABLE}

//
// MessageId: NS_E_WMP_WMDM_INTERFACEDEAD
//
// MessageText:
//
//  Windows Media Player no longer detects a portable device. Reconnect your portable device, and try again.%0
//
  NS_E_WMP_WMDM_INTERFACEDEAD      = HRESULT($C00D11A4);
  {$EXTERNALSYM NS_E_WMP_WMDM_INTERFACEDEAD}

//
// MessageId: NS_E_WMP_WMDM_NOTCERTIFIED
//
// MessageText:
//
//  Windows Media Player cannot copy the file because the portable device does not support protected files.%0
//
  NS_E_WMP_WMDM_NOTCERTIFIED       = HRESULT($C00D11A5);
  {$EXTERNALSYM NS_E_WMP_WMDM_NOTCERTIFIED}

//
// MessageId: NS_E_WMP_WMDM_LICENSE_NOTEXIST
//
// MessageText:
//
//  Windows Media Player cannot copy the file because a license for this file
// could not be found on your computer. If you obtained this file from a Web site,
// return to the site, and try downloading the file again.%0
//
  NS_E_WMP_WMDM_LICENSE_NOTEXIST   = HRESULT($C00D11A6);
  {$EXTERNALSYM NS_E_WMP_WMDM_LICENSE_NOTEXIST}

//
// MessageId: NS_E_WMP_WMDM_LICENSE_EXPIRED
//
// MessageText:
//
//  Windows Media Player cannot copy the file because the license for this file
//  has expired. If you obtained this file from a Web site, return to the site,
//  and try downloading the file again.%0
//
  NS_E_WMP_WMDM_LICENSE_EXPIRED    = HRESULT($C00D11A7);
  {$EXTERNALSYM NS_E_WMP_WMDM_LICENSE_EXPIRED}

//
// MessageId: NS_E_WMP_WMDM_BUSY
//
// MessageText:
//
//  The portable device is already in use. Wait until the current process finishes or quit other programs that may be using the portable device, and then try again.%0
//
  NS_E_WMP_WMDM_BUSY               = HRESULT($C00D11A8);
  {$EXTERNALSYM NS_E_WMP_WMDM_BUSY}

//
// MessageId: NS_E_WMP_WMDM_NORIGHTS
//
// MessageText:
//
//  Windows Media Player cannot copy the file because the license or device restricts it.%0
//
  NS_E_WMP_WMDM_NORIGHTS           = HRESULT($C00D11A9);
  {$EXTERNALSYM NS_E_WMP_WMDM_NORIGHTS}

//
// MessageId: NS_E_WMP_WMDM_INCORRECT_RIGHTS
//
// MessageText:
//
//  Windows Media Player cannot copy the file because the license associated with the file restricts it.%0
//
  NS_E_WMP_WMDM_INCORRECT_RIGHTS   = HRESULT($C00D11AA);
  {$EXTERNALSYM NS_E_WMP_WMDM_INCORRECT_RIGHTS}

//
// MessageId: NS_E_WMP_IMAPI_GENERIC
//
// MessageText:
//
//  Windows Media Player cannot copy files to the recordable disc.%0
//
  NS_E_WMP_IMAPI_GENERIC           = HRESULT($C00D11AB);
  {$EXTERNALSYM NS_E_WMP_IMAPI_GENERIC}

//
// MessageId: NS_E_WMP_IMAPI_DEVICE_NOTPRESENT
//
// MessageText:
//
//  Windows Media Player cannot copy files to the recordable disc. Verify that the CD-R or CD-RW drive is connected, and then try again.%0
//
  NS_E_WMP_IMAPI_DEVICE_NOTPRESENT = HRESULT($C00D11AD);
  {$EXTERNALSYM NS_E_WMP_IMAPI_DEVICE_NOTPRESENT}

//
// MessageId: NS_E_WMP_IMAPI_STASHINUSE
//
// MessageText:
//
//  The CD-R or CD-RW drive may already be in use. Wait until the current process finishes or quit other programs that may be using the CD-R or CD-RW drive, and then try again.%0
//
  NS_E_WMP_IMAPI_STASHINUSE        = HRESULT($C00D11AE);
  {$EXTERNALSYM NS_E_WMP_IMAPI_STASHINUSE}

//
// MessageId: NS_E_WMP_IMAPI_LOSS_OF_STREAMING
//
// MessageText:
//
//  Windows Media Player cannot copy files to the recordable disc.%0
//
  NS_E_WMP_IMAPI_LOSS_OF_STREAMING = HRESULT($C00D11AF);
  {$EXTERNALSYM NS_E_WMP_IMAPI_LOSS_OF_STREAMING}

//
// MessageId: NS_E_WMP_SERVER_UNAVAILABLE
//
// MessageText:
//
//  Windows Media Player cannot play the file because the server is busy. Try again later.%0
//
  NS_E_WMP_SERVER_UNAVAILABLE      = HRESULT($C00D11B0);
  {$EXTERNALSYM NS_E_WMP_SERVER_UNAVAILABLE}

//
// MessageId: NS_E_WMP_FILE_OPEN_FAILED
//
// MessageText:
//
//  Windows Media Player cannot open the file.%0
//
  NS_E_WMP_FILE_OPEN_FAILED        = HRESULT($C00D11B1);
  {$EXTERNALSYM NS_E_WMP_FILE_OPEN_FAILED}

//
// MessageId: NS_E_WMP_VERIFY_ONLINE
//
// MessageText:
//
//  To play the file, Windows Media Player must obtain a license from the Internet. Connect to the Internet, and then try again.%0
//
  NS_E_WMP_VERIFY_ONLINE           = HRESULT($C00D11B2);
  {$EXTERNALSYM NS_E_WMP_VERIFY_ONLINE}

//
// MessageId: NS_E_WMP_SERVER_NOT_RESPONDING
//
// MessageText:
//
// Windows Media Player cannot play the file because the server is not responding.
// If you entered a URL or path to play the file, verify that it is correct. If you
// clicked a link to play the file, the link may not be valid.%0
//
  NS_E_WMP_SERVER_NOT_RESPONDING   = HRESULT($C00D11B3);
  {$EXTERNALSYM NS_E_WMP_SERVER_NOT_RESPONDING}

//
// MessageId: NS_E_WMP_DRM_CORRUPT_BACKUP
//
// MessageText:
//
//  Windows Media Player cannot restore your licenses because no backed up licenses were found on your computer.%0
//
  NS_E_WMP_DRM_CORRUPT_BACKUP      = HRESULT($C00D11B4);
  {$EXTERNALSYM NS_E_WMP_DRM_CORRUPT_BACKUP}

//
// MessageId: NS_E_WMP_DRM_LICENSE_SERVER_UNAVAILABLE
//
// MessageText:
//
//  To play the file, Windows Media Player must obtain a license from the Internet. However, the license server is currently not available. Try again later.%0
//
  NS_E_WMP_DRM_LICENSE_SERVER_UNAVAILABLE = HRESULT($C00D11B5);
  {$EXTERNALSYM NS_E_WMP_DRM_LICENSE_SERVER_UNAVAILABLE}

//
// MessageId: NS_E_WMP_NETWORK_FIREWALL
//
// MessageText:
//
//  Windows Media Player cannot play the file. A network firewall may be preventing
//  the Player from opening the file by using the UDP transport protocol. To play
//  this file, try opening the file without specifying UDP.%0
//
  NS_E_WMP_NETWORK_FIREWALL        = HRESULT($C00D11B6);
  {$EXTERNALSYM NS_E_WMP_NETWORK_FIREWALL}

//
// MessageId: NS_E_WMP_NO_REMOVABLE_MEDIA
//
// MessageText:
//
//  Insert the removable media, and then try again.%0
//
  NS_E_WMP_NO_REMOVABLE_MEDIA      = HRESULT($C00D11B7);
  {$EXTERNALSYM NS_E_WMP_NO_REMOVABLE_MEDIA}

//
// MessageId: NS_E_WMP_PROXY_CONNECT_TIMEOUT
//
// MessageText:
//
//  Windows Media Player cannot play the file because the proxy server is not
//  responding. The proxy server may be temporarily unavailable or your Player
//  proxy settings may not be valid.%0
//
  NS_E_WMP_PROXY_CONNECT_TIMEOUT   = HRESULT($C00D11B8);
  {$EXTERNALSYM NS_E_WMP_PROXY_CONNECT_TIMEOUT}

//
// MessageId: NS_E_WMP_NEED_UPGRADE
//
// MessageText:
//
//  To play this file, you must upgrade to the latest version of Windows Media Player.
//  To upgrade the Player, on the Help menu, click Check For Player Updates, and then
//  follow the instructions.%0
//
  NS_E_WMP_NEED_UPGRADE            = HRESULT($C00D11B9);
  {$EXTERNALSYM NS_E_WMP_NEED_UPGRADE}

//
// MessageId: NS_E_WMP_AUDIO_HW_PROBLEM
//
// MessageText:
//
//  Windows Media Player cannot play the file because there is a problem with your
//  sound device. There may not be a sound device installed on your computer, it may
//  be in use by another program, or it may not be functioning properly.%0
//
  NS_E_WMP_AUDIO_HW_PROBLEM        = HRESULT($C00D11BA);
  {$EXTERNALSYM NS_E_WMP_AUDIO_HW_PROBLEM}

//
// MessageId: NS_E_WMP_INVALID_PROTOCOL
//
// MessageText:
//
//  Windows Media Player cannot play the file because the specified protocol is not
//  supported. In the Open URL dialog, try opening the file using a different transport
//  protocol (for example, "http:" or "rtsp:").%0
//
  NS_E_WMP_INVALID_PROTOCOL        = HRESULT($C00D11BB);
  {$EXTERNALSYM NS_E_WMP_INVALID_PROTOCOL}

//
// MessageId: NS_E_WMP_INVALID_LIBRARY_ADD
//
// MessageText:
//
//  Windows Media Player cannot add the file to Media Library because the file format is not supported.%0
//
  NS_E_WMP_INVALID_LIBRARY_ADD     = HRESULT($C00D11BC);
  {$EXTERNALSYM NS_E_WMP_INVALID_LIBRARY_ADD}

//
// MessageId: NS_E_WMP_MMS_NOT_SUPPORTED
//
// MessageText:
//
//  Windows Media Player cannot play the file because the specified protocol is not
//  supported. In the Open URL dialog, try opening the file using a different transport
//  protocol (for example, "mms:").%0
//
  NS_E_WMP_MMS_NOT_SUPPORTED       = HRESULT($C00D11BD);
  {$EXTERNALSYM NS_E_WMP_MMS_NOT_SUPPORTED}

//
// MessageId: NS_E_WMP_NO_PROTOCOLS_SELECTED
//
// MessageText:
//
//  Windows Media Player cannot play the file because there are no streaming protocols selected. Select one or more protocols, and then try again.%0
//
  NS_E_WMP_NO_PROTOCOLS_SELECTED   = HRESULT($C00D11BE);
  {$EXTERNALSYM NS_E_WMP_NO_PROTOCOLS_SELECTED}

//
// MessageId: NS_E_WMP_GOFULLSCREEN_FAILED
//
// MessageText:
//
//  To use the Full Screen command, you may need to adjust your Windows display settings. Open Display in Control Panel, and try setting Hardware acceleration to Full.%0
//
  NS_E_WMP_GOFULLSCREEN_FAILED     = HRESULT($C00D11BF);
  {$EXTERNALSYM NS_E_WMP_GOFULLSCREEN_FAILED}

//
// MessageId: NS_E_WMP_NETWORK_ERROR
//
// MessageText:
//
//  Windows Media Player cannot play the file because a network error occurred.%0
//
  NS_E_WMP_NETWORK_ERROR           = HRESULT($C00D11C0);
  {$EXTERNALSYM NS_E_WMP_NETWORK_ERROR}

//
// MessageId: NS_E_WMP_CONNECT_TIMEOUT
//
// MessageText:
//
//  Windows Media Player cannot play the file because the server is not responding. Verify that you are connected to the network, and then try again later.%0
//
  NS_E_WMP_CONNECT_TIMEOUT         = HRESULT($C00D11C1);
  {$EXTERNALSYM NS_E_WMP_CONNECT_TIMEOUT}

//
// MessageId: NS_E_WMP_MULTICAST_DISABLED
//
// MessageText:
//
//  Windows Media Player cannot play the file because the multicast protocol is
//  not enabled. On the Tools menu, click Options, click the Network tab, and then
//  select the Multicast check box.%0
//
  NS_E_WMP_MULTICAST_DISABLED      = HRESULT($C00D11C2);
  {$EXTERNALSYM NS_E_WMP_MULTICAST_DISABLED}

//
// MessageId: NS_E_WMP_SERVER_DNS_TIMEOUT
//
// MessageText:
//
//  Windows Media Player cannot play the file because a network problem occurred. Verify that you are connected to the network, and then try again later.%0
//
  NS_E_WMP_SERVER_DNS_TIMEOUT      = HRESULT($C00D11C3);
  {$EXTERNALSYM NS_E_WMP_SERVER_DNS_TIMEOUT}

//
// MessageId: NS_E_WMP_PROXY_NOT_FOUND
//
// MessageText:
//
//  Windows Media Player cannot play the file because the network proxy server could not be found. Verify your proxy settings, and then try again.%0
//
  NS_E_WMP_PROXY_NOT_FOUND         = HRESULT($C00D11C4);
  {$EXTERNALSYM NS_E_WMP_PROXY_NOT_FOUND}

//
// MessageId: NS_E_WMP_TAMPERED_CONTENT
//
// MessageText:
//
//  Windows Media Player cannot play the file because it is damaged or corrupted.%0
//
  NS_E_WMP_TAMPERED_CONTENT        = HRESULT($C00D11C5);
  {$EXTERNALSYM NS_E_WMP_TAMPERED_CONTENT}

//
// MessageId: NS_E_WMP_OUTOFMEMORY
//
// MessageText:
//
//  Your computer is running low on memory. Quit other programs, and then try again.%0
//
  NS_E_WMP_OUTOFMEMORY             = HRESULT($C00D11C6);
  {$EXTERNALSYM NS_E_WMP_OUTOFMEMORY}

//
// MessageId: NS_E_WMP_AUDIO_CODEC_NOT_INSTALLED
//
// MessageText:
//
//  Windows Media Player cannot play the file because the required audio codec is not installed on your computer.%0
//
  NS_E_WMP_AUDIO_CODEC_NOT_INSTALLED = HRESULT($C00D11C7);
  {$EXTERNALSYM NS_E_WMP_AUDIO_CODEC_NOT_INSTALLED}

//
// MessageId: NS_E_WMP_VIDEO_CODEC_NOT_INSTALLED
//
// MessageText:
//
//  Windows Media Player cannot play the file because the required video codec is not installed on your computer.%0
//
  NS_E_WMP_VIDEO_CODEC_NOT_INSTALLED = HRESULT($C00D11C8);
  {$EXTERNALSYM NS_E_WMP_VIDEO_CODEC_NOT_INSTALLED}

//
// MessageId: NS_E_WMP_IMAPI_DEVICE_INVALIDTYPE
//
// MessageText:
//
//  Windows Media Player cannot copy the files to the recordable disc.%0
//
  NS_E_WMP_IMAPI_DEVICE_INVALIDTYPE = HRESULT($C00D11C9);
  {$EXTERNALSYM NS_E_WMP_IMAPI_DEVICE_INVALIDTYPE}

//
// MessageId: NS_E_WMP_DRM_DRIVER_AUTH_FAILURE
//
// MessageText:
//
//  Windows Media Player cannot play the licensed file because there is a problem with your sound device. Try installing a new device driver or use a different sound device.%0
//
  NS_E_WMP_DRM_DRIVER_AUTH_FAILURE = HRESULT($C00D11CA);
  {$EXTERNALSYM NS_E_WMP_DRM_DRIVER_AUTH_FAILURE}

//
// MessageId: NS_E_WMP_NETWORK_RESOURCE_FAILURE
//
// MessageText:
//
//  Windows Media Player encountered a network problem. Restart the Player.%0
//
  NS_E_WMP_NETWORK_RESOURCE_FAILURE = HRESULT($C00D11CB);
  {$EXTERNALSYM NS_E_WMP_NETWORK_RESOURCE_FAILURE}

//
// MessageId: NS_E_WMP_UPGRADE_APPLICATION
//
// MessageText:
//
//  Windows Media Player is not installed properly. Reinstall the Player.%0
//
  NS_E_WMP_UPGRADE_APPLICATION     = HRESULT($C00D11CC);
  {$EXTERNALSYM NS_E_WMP_UPGRADE_APPLICATION}

//
// MessageId: NS_E_WMP_UNKNOWN_ERROR
//
// MessageText:
//
//  Windows Media Player encountered an unknown error.%0
//
  NS_E_WMP_UNKNOWN_ERROR           = HRESULT($C00D11CD);
  {$EXTERNALSYM NS_E_WMP_UNKNOWN_ERROR}

//
// MessageId: NS_E_WMP_INVALID_KEY
//
// MessageText:
//
//  Windows Media Player cannot play the file because the required codec is not valid.%0
//
  NS_E_WMP_INVALID_KEY             = HRESULT($C00D11CE);
  {$EXTERNALSYM NS_E_WMP_INVALID_KEY}

//
// MessageId: NS_E_WMP_CD_ANOTHER_USER
//
// MessageText:
//
//  The CD drive is in use by another user. Wait for the operation to complete, and then try again.%0
//
  NS_E_WMP_CD_ANOTHER_USER         = HRESULT($C00D11CF);
  {$EXTERNALSYM NS_E_WMP_CD_ANOTHER_USER}

//
// MessageId: NS_E_WMP_DRM_NEEDS_AUTHORIZATION
//
// MessageText:
//
//  Windows Media Player cannot play the licensed file because the required security upgrade is not available for download.%0
//
  NS_E_WMP_DRM_NEEDS_AUTHORIZATION = HRESULT($C00D11D0);
  {$EXTERNALSYM NS_E_WMP_DRM_NEEDS_AUTHORIZATION}

//
// MessageId: NS_E_WMP_BAD_DRIVER
//
// MessageText:
//
//  Windows Media Player cannot play the file because there may be a problem with your sound or video device. Try installing a new device driver.%0
//
  NS_E_WMP_BAD_DRIVER              = HRESULT($C00D11D1);
  {$EXTERNALSYM NS_E_WMP_BAD_DRIVER}

//
// MessageId: NS_E_WMP_ACCESS_DENIED
//
// MessageText:
//
//  Windows Media Player cannot access the file.%0
//
  NS_E_WMP_ACCESS_DENIED           = HRESULT($C00D11D2);
  {$EXTERNALSYM NS_E_WMP_ACCESS_DENIED}

//
// MessageId: NS_E_WMP_LICENSE_RESTRICTS
//
// MessageText:
//
//  Windows Media Player cannot copy the file to the device because the license restricts it.%0
//
  NS_E_WMP_LICENSE_RESTRICTS       = HRESULT($C00D11D3);
  {$EXTERNALSYM NS_E_WMP_LICENSE_RESTRICTS}

//
// MessageId: NS_E_WMP_INVALID_REQUEST
//
// MessageText:
//
//  Windows Media Player cannot perform the requested action at this time.%0
//
  NS_E_WMP_INVALID_REQUEST         = HRESULT($C00D11D4);
  {$EXTERNALSYM NS_E_WMP_INVALID_REQUEST}

//
// MessageId: NS_E_WMP_CD_STASH_NO_SPACE
//
// MessageText:
//
//  Windows Media Player cannot copy the files because there is not enough free
//  disk space to store the temporary files. Delete some unneeded files on your hard
//  disk, and then try again.%0
//
  NS_E_WMP_CD_STASH_NO_SPACE       = HRESULT($C00D11D5);
  {$EXTERNALSYM NS_E_WMP_CD_STASH_NO_SPACE}

//
// MessageId: NS_E_WMP_DRM_NEW_HARDWARE
//
// MessageText:
//
//  Windows Media Player cannot play the file because the associated license is
//  either corrupted or not valid. The license may no longer be valid if you have
//  replaced hardware components in your computer.%0
//
  NS_E_WMP_DRM_NEW_HARDWARE        = HRESULT($C00D11D6);
  {$EXTERNALSYM NS_E_WMP_DRM_NEW_HARDWARE}

//
// MessageId: NS_E_WMP_DRM_INVALID_SIG
//
// MessageText:
//
//  The required security upgrade cannot be validated. Try installing the latest
//  Internet Explorer service pack. To install the service pack from the Windows
//  Update Web site, click Web Help.%0
//
  NS_E_WMP_DRM_INVALID_SIG         = HRESULT($C00D11D7);
  {$EXTERNALSYM NS_E_WMP_DRM_INVALID_SIG}

//
// MessageId: NS_E_WMP_DRM_CANNOT_RESTORE
//
// MessageText:
//
//  Windows Media Player cannot restore your licenses because you have exceeded the restore limit for the day. Try again tomorrow.%0
//
  NS_E_WMP_DRM_CANNOT_RESTORE      = HRESULT($C00D11D8);
  {$EXTERNALSYM NS_E_WMP_DRM_CANNOT_RESTORE}

//
// WMP CD Filter Error codes extension
//
//
// MessageId: NS_E_CD_NO_BUFFERS_READ
//
// MessageText:
//
//  Windows Media Player encountered an error when reading the CD-ROM drive in digital mode.
//  You can try to use digital mode again, or you can switch the Player to analog  mode.%0
//
  NS_E_CD_NO_BUFFERS_READ          = HRESULT($C00D11F8);
  {$EXTERNALSYM NS_E_CD_NO_BUFFERS_READ}

//
// MessageId: NS_E_CD_EMPTY_TRACK_QUEUE
//
// MessageText:
//
//  No CD track was specified for playback.%0
//
  NS_E_CD_EMPTY_TRACK_QUEUE        = HRESULT($C00D11F9);
  {$EXTERNALSYM NS_E_CD_EMPTY_TRACK_QUEUE}

//
// MessageId: NS_E_CD_NO_READER
//
// MessageText:
//
//  The CD filter was not able to create the CD reader.%0
//
  NS_E_CD_NO_READER                = HRESULT($C00D11FA);
  {$EXTERNALSYM NS_E_CD_NO_READER}

//
// MessageId: NS_E_CD_ISRC_INVALID
//
// MessageText:
//
//  Invalid ISRC code.%0
//
  NS_E_CD_ISRC_INVALID             = HRESULT($C00D11FB);
  {$EXTERNALSYM NS_E_CD_ISRC_INVALID}

//
// MessageId: NS_E_CD_MEDIA_CATALOG_NUMBER_INVALID
//
// MessageText:
//
//  Invalid Media Catalog Number.%0
//
  NS_E_CD_MEDIA_CATALOG_NUMBER_INVALID = HRESULT($C00D11FC);
  {$EXTERNALSYM NS_E_CD_MEDIA_CATALOG_NUMBER_INVALID}

//
// MessageId: NS_E_SLOW_READ_DIGITAL_WITH_ERRORCORRECTION
//
// MessageText:
//
//  Media Player has detected that your CD drive cannot play back audio CDs
//  correctly because the drive is too slow with error correction turned on. Please
//  turn off error correction for this drive.%0
//
  NS_E_SLOW_READ_DIGITAL_WITH_ERRORCORRECTION = HRESULT($C00D11FD);
  {$EXTERNALSYM NS_E_SLOW_READ_DIGITAL_WITH_ERRORCORRECTION}

//
// MessageId: NS_E_CD_SPEEDDETECT_NOT_ENOUGH_READS
//
// MessageText:
//
//  The CD track is too small to make a good estimate of the CD's speed.%0
//
  NS_E_CD_SPEEDDETECT_NOT_ENOUGH_READS = HRESULT($C00D11FE);
  {$EXTERNALSYM NS_E_CD_SPEEDDETECT_NOT_ENOUGH_READS}

//
// MessageId: NS_E_CD_QUEUEING_DISABLED
//
// MessageText:
//
//  Cannot queue the given CD track as queuing is disabled.%0
//
  NS_E_CD_QUEUEING_DISABLED        = HRESULT($C00D11FF);
  {$EXTERNALSYM NS_E_CD_QUEUEING_DISABLED}

//
// WMP Policy error codes
//
//
// MessageId: NS_E_WMP_POLICY_VALUE_NOT_CONFIGURED
//
// MessageText:
//
//  Media Player failed to read a policy. This can happen when the policy is not present in the registry or when there is a failure to read from the registry.%0
//
  NS_E_WMP_POLICY_VALUE_NOT_CONFIGURED = HRESULT($C00D122A);
  {$EXTERNALSYM NS_E_WMP_POLICY_VALUE_NOT_CONFIGURED}

//
//Background download plugin
//
//
// MessageId: NS_E_WMP_HWND_NOTFOUND
//
// MessageText:
//
//  Windows Media Player main window not found. The download manager needs to find it to work properly. Please try to run Windows Media Player again.%0
//
  NS_E_WMP_HWND_NOTFOUND           = HRESULT($C00D125C);
  {$EXTERNALSYM NS_E_WMP_HWND_NOTFOUND}

//
// MessageId: NS_E_BKGDOWNLOAD_WRONG_NO_FILES
//
// MessageText:
//
//  Windows media player encountered a download with wrong number of files.
//  This may happen if some other application is trying to create jobs with same
//  signature as Windows Media Player.%0
//
  NS_E_BKGDOWNLOAD_WRONG_NO_FILES  = HRESULT($C00D125D);
  {$EXTERNALSYM NS_E_BKGDOWNLOAD_WRONG_NO_FILES}

//
// MessageId: NS_E_BKGDOWNLOAD_COMPLETECANCELLEDJOB
//
// MessageText:
//
//  Windows Media Player tried to complete a download that was already cancelled. The file will not be available.%0
//
  NS_E_BKGDOWNLOAD_COMPLETECANCELLEDJOB = HRESULT($C00D125E);
  {$EXTERNALSYM NS_E_BKGDOWNLOAD_COMPLETECANCELLEDJOB}

//
// MessageId: NS_E_BKGDOWNLOAD_CANCELCOMPLETEDJOB
//
// MessageText:
//
//  Windows Media Player tried to cancel a download that was already completed. The file will not be removed.%0
//
  NS_E_BKGDOWNLOAD_CANCELCOMPLETEDJOB = HRESULT($C00D125F);
  {$EXTERNALSYM NS_E_BKGDOWNLOAD_CANCELCOMPLETEDJOB}

//
// MessageId: NS_E_BKGDOWNLOAD_NOJOBPOINTER
//
// MessageText:
//
//  Windows Media Player is trying to access an invalid download.%0
//
  NS_E_BKGDOWNLOAD_NOJOBPOINTER    = HRESULT($C00D1260);
  {$EXTERNALSYM NS_E_BKGDOWNLOAD_NOJOBPOINTER}

//
// MessageId: NS_E_BKGDOWNLOAD_INVALIDJOBSIGNATURE
//
// MessageText:
//
//  This download was not created by Windows Media Player.%0
//
  NS_E_BKGDOWNLOAD_INVALIDJOBSIGNATURE = HRESULT($C00D1261);
  {$EXTERNALSYM NS_E_BKGDOWNLOAD_INVALIDJOBSIGNATURE}

//
// MessageId: NS_E_BKGDOWNLOAD_FAILED_TO_CREATE_TEMPFILE
//
// MessageText:
//
//  The Windows Media Player download manager failed to create a temporary file name.
//  This may happen if the path in invalid or if the disk is full. Please check
//  your system and try again.%0
//
  NS_E_BKGDOWNLOAD_FAILED_TO_CREATE_TEMPFILE = HRESULT($C00D1262);
  {$EXTERNALSYM NS_E_BKGDOWNLOAD_FAILED_TO_CREATE_TEMPFILE}

//
// MessageId: NS_E_BKGDOWNLOAD_PLUGIN_FAILEDINITIALIZE
//
// MessageText:
//
//  The Windows Media Player download manager plugin failed to initialize. This may happen if the system is out of resources. Please check your system and try again.%0
//
  NS_E_BKGDOWNLOAD_PLUGIN_FAILEDINITIALIZE = HRESULT($C00D1263);
  {$EXTERNALSYM NS_E_BKGDOWNLOAD_PLUGIN_FAILEDINITIALIZE}

//
// MessageId: NS_E_BKGDOWNLOAD_PLUGIN_FAILEDTOMOVEFILE
//
// MessageText:
//
//  The Windows Media Player download manager failed to move the file.%0
//
  NS_E_BKGDOWNLOAD_PLUGIN_FAILEDTOMOVEFILE = HRESULT($C00D1264);
  {$EXTERNALSYM NS_E_BKGDOWNLOAD_PLUGIN_FAILEDTOMOVEFILE}

//
// MessageId: NS_E_BKGDOWNLOAD_CALLFUNCFAILED
//
// MessageText:
//
//  Download manager failed to accomplish a task. This happened because the system has no resources to allocate.%0
//
  NS_E_BKGDOWNLOAD_CALLFUNCFAILED  = HRESULT($C00D1265);
  {$EXTERNALSYM NS_E_BKGDOWNLOAD_CALLFUNCFAILED}

//
// MessageId: NS_E_BKGDOWNLOAD_CALLFUNCTIMEOUT
//
// MessageText:
//
//  The Windows Media Player download manager failed to accomplish a task because the task took too long to execute.%0
//
  NS_E_BKGDOWNLOAD_CALLFUNCTIMEOUT = HRESULT($C00D1266);
  {$EXTERNALSYM NS_E_BKGDOWNLOAD_CALLFUNCTIMEOUT}

//
// MessageId: NS_E_BKGDOWNLOAD_CALLFUNCENDED
//
// MessageText:
//
//  The Windows Media Player download manager failed to accomplish a task because
//  Windows Media Player is terminating the service. The task will be recovered when
//  Windows Media Player starts again.%0
//
  NS_E_BKGDOWNLOAD_CALLFUNCENDED   = HRESULT($C00D1267);
  {$EXTERNALSYM NS_E_BKGDOWNLOAD_CALLFUNCENDED}

//
// MessageId: NS_E_BKGDOWNLOAD_WMDUNPACKFAILED
//
// MessageText:
//
//  The Windows Media Player download manager failed to unpack a WMD package that
//  was transferred. The file will be deleted and the operation will not be
//  successfully completed.%0
//
  NS_E_BKGDOWNLOAD_WMDUNPACKFAILED = HRESULT($C00D1268);
  {$EXTERNALSYM NS_E_BKGDOWNLOAD_WMDUNPACKFAILED}

//
// MessageId: NS_E_BKGDOWNLOAD_FAILEDINITIALIZE
//
// MessageText:
//
//  The Windows Media Player download manager failed to initialize. This may happen if the system is out of resources. Please check your system and try again.%0
//
  NS_E_BKGDOWNLOAD_FAILEDINITIALIZE = HRESULT($C00D1269);
  {$EXTERNALSYM NS_E_BKGDOWNLOAD_FAILEDINITIALIZE}

//
// MessageId: NS_E_INTERFACE_NOT_REGISTERED_IN_GIT
//
// MessageText:
//
//  Windows Media Player failed to access a required functionality. This could be caused by wrong system or media player dlls being loaded.%0
//
  NS_E_INTERFACE_NOT_REGISTERED_IN_GIT = HRESULT($C00D126A);
  {$EXTERNALSYM NS_E_INTERFACE_NOT_REGISTERED_IN_GIT}

//
// MessageId: NS_E_BKGDOWNLOAD_INVALID_FILE_NAME
//
// MessageText:
//
//  Windows Media Player failed to get the file name of the requested download. The requested download will be canceled.%0
//
  NS_E_BKGDOWNLOAD_INVALID_FILE_NAME = HRESULT($C00D126B);
  {$EXTERNALSYM NS_E_BKGDOWNLOAD_INVALID_FILE_NAME}

//
//Image Graph Errors 4750 -- 4800
//
//
// MessageId: NS_E_IMAGE_DOWNLOAD_FAILED
//
// MessageText:
//
//  An error was encountered downloading the image.%0
//
  NS_E_IMAGE_DOWNLOAD_FAILED       = HRESULT($C00D128E);
  {$EXTERNALSYM NS_E_IMAGE_DOWNLOAD_FAILED}

//
// UDRM errors
//
//
// MessageId: NS_E_WMP_UDRM_NOUSERLIST
//
// MessageText:
//
//  There was a problem while trying to get the list of activated users for this machine. The License acquisition process will be stopped.%0
//
  NS_E_WMP_UDRM_NOUSERLIST         = HRESULT($C00D12C0);
  {$EXTERNALSYM NS_E_WMP_UDRM_NOUSERLIST}

//
// MessageId: NS_E_WMP_DRM_NOT_ACQUIRING
//
// MessageText:
//
//  The Windows Media Player is trying to acquire a license for a file that is not being used anymore. The License acquisition process will stop.%0
//
  NS_E_WMP_DRM_NOT_ACQUIRING       = HRESULT($C00D12C1);
  {$EXTERNALSYM NS_E_WMP_DRM_NOT_ACQUIRING}

//
// String is too large
//
//
// MessageId: NS_E_WMP_BSTR_TOO_LONG
//
// MessageText:
//
//  The parameter is invalid.%0
//
  NS_E_WMP_BSTR_TOO_LONG           = HRESULT($C00D12F2);
  {$EXTERNALSYM NS_E_WMP_BSTR_TOO_LONG}

//
// Autoplay errors 4860 --- 4870
//
//
// MessageId: NS_E_WMP_AUTOPLAY_INVALID_STATE
//
// MessageText:
//
//  Invalid state for this request.%0
//
  NS_E_WMP_AUTOPLAY_INVALID_STATE  = HRESULT($C00D12FC);
  {$EXTERNALSYM NS_E_WMP_AUTOPLAY_INVALID_STATE}

//
// CURL Errors 4900 -- 4949
//
//
// MessageId: NS_E_CURL_NOTSAFE
//
// MessageText:
//
//  The URL is not safe for the operation specified.%0
//
  NS_E_CURL_NOTSAFE                = HRESULT($C00D1324);
  {$EXTERNALSYM NS_E_CURL_NOTSAFE}

//
// MessageId: NS_E_CURL_INVALIDCHAR
//
// MessageText:
//
//  The URL contains one or more invalid characters.%0
//
  NS_E_CURL_INVALIDCHAR            = HRESULT($C00D1325);
  {$EXTERNALSYM NS_E_CURL_INVALIDCHAR}

//
// MessageId: NS_E_CURL_INVALIDHOSTNAME
//
// MessageText:
//
//  The URL contains an invalid hostname.%0
//
  NS_E_CURL_INVALIDHOSTNAME        = HRESULT($C00D1326);
  {$EXTERNALSYM NS_E_CURL_INVALIDHOSTNAME}

//
// MessageId: NS_E_CURL_INVALIDPATH
//
// MessageText:
//
//  The URL contains an invalid path.%0
//
  NS_E_CURL_INVALIDPATH            = HRESULT($C00D1327);
  {$EXTERNALSYM NS_E_CURL_INVALIDPATH}

//
// MessageId: NS_E_CURL_INVALIDSCHEME
//
// MessageText:
//
//  The URL contains an invalid scheme.%0
//
  NS_E_CURL_INVALIDSCHEME          = HRESULT($C00D1328);
  {$EXTERNALSYM NS_E_CURL_INVALIDSCHEME}

//
// MessageId: NS_E_CURL_INVALIDURL
//
// MessageText:
//
//  The URL is invalid.%0
//
  NS_E_CURL_INVALIDURL             = HRESULT($C00D1329);
  {$EXTERNALSYM NS_E_CURL_INVALIDURL}

//
// MessageId: NS_E_CURL_CANTWALK
//
// MessageText:
//
//  The URL would change the root.%0
//
  NS_E_CURL_CANTWALK               = HRESULT($C00D132B);
  {$EXTERNALSYM NS_E_CURL_CANTWALK}

//
// MessageId: NS_E_CURL_INVALIDPORT
//
// MessageText:
//
//  The URL port is invalid.%0
//
  NS_E_CURL_INVALIDPORT            = HRESULT($C00D132C);
  {$EXTERNALSYM NS_E_CURL_INVALIDPORT}

//
// MessageId: NS_E_CURLHELPER_NOTADIRECTORY
//
// MessageText:
//
//  The URL is not a directory.%0
//
  NS_E_CURLHELPER_NOTADIRECTORY    = HRESULT($C00D132D);
  {$EXTERNALSYM NS_E_CURLHELPER_NOTADIRECTORY}

//
// MessageId: NS_E_CURLHELPER_NOTAFILE
//
// MessageText:
//
//  The URL is not a file.%0
//
  NS_E_CURLHELPER_NOTAFILE         = HRESULT($C00D132E);
  {$EXTERNALSYM NS_E_CURLHELPER_NOTAFILE}

//
// MessageId: NS_E_CURL_CANTDECODE
//
// MessageText:
//
//  The URL contains characters that could not be decoded.  The URL may be truncated or incomplete.%0
//
  NS_E_CURL_CANTDECODE             = HRESULT($C00D132F);
  {$EXTERNALSYM NS_E_CURL_CANTDECODE}

//
// MessageId: NS_E_CURLHELPER_NOTRELATIVE
//
// MessageText:
//
//  The specified relative URL is actually not a relative URL.%0
//
  NS_E_CURLHELPER_NOTRELATIVE      = HRESULT($C00D1330);
  {$EXTERNALSYM NS_E_CURLHELPER_NOTRELATIVE}

//
// MessageId: NS_E_CURL_INVALIDBUFFERSIZE
//
// MessageText:
//
//  The buffer is smaller than the size specified.%0
//
  NS_E_CURL_INVALIDBUFFERSIZE      = HRESULT($C00D1355);
  {$EXTERNALSYM NS_E_CURL_INVALIDBUFFERSIZE}

//
// Subscription Service Errors 4950 -- 4969
//
//
// MessageId: NS_E_SUBSCRIPTIONSERVICE_PLAYBACK_DISALLOWED
//
// MessageText:
//
//  This content is provided by a music service. The content cannot be played, possibly because a valid license does not exist. Please contact the music service with questions.%0
//
  NS_E_SUBSCRIPTIONSERVICE_PLAYBACK_DISALLOWED = HRESULT($C00D1356);
  {$EXTERNALSYM NS_E_SUBSCRIPTIONSERVICE_PLAYBACK_DISALLOWED}

//
// Advanced Edit Dialog Errors 4970 -- 4989
//
//
// MessageId: NS_E_ADVANCEDEDIT_TOO_MANY_PICTURES
//
// MessageText:
//
//  Not all your images were saved to the file. Only 7MB of images may be saved to a media file.%0
//
  NS_E_ADVANCEDEDIT_TOO_MANY_PICTURES = HRESULT($C00D136A);
  {$EXTERNALSYM NS_E_ADVANCEDEDIT_TOO_MANY_PICTURES}



/////////////////////////////////////////////////////////////////////////
//
// Windows Media Server Errors
//
// IdRange = 5000 - 5999
//
/////////////////////////////////////////////////////////////////////////

//
// MessageId: NS_E_REDIRECT
//
// MessageText:
//
//  The client redirected to another server.%0
//
  NS_E_REDIRECT                    = HRESULT($C00D1388);
  {$EXTERNALSYM NS_E_REDIRECT}

//
// MessageId: NS_E_STALE_PRESENTATION
//
// MessageText:
//
//  The streaming media description is no longer current.%0
//
  NS_E_STALE_PRESENTATION          = HRESULT($C00D1389);
  {$EXTERNALSYM NS_E_STALE_PRESENTATION}


// Namespace Errors

//
// MessageId: NS_E_NAMESPACE_WRONG_PERSIST
//
// MessageText:
//
//  It is not possible to create a persistent namespace node under a transient parent node.%0
//
  NS_E_NAMESPACE_WRONG_PERSIST     = HRESULT($C00D138A);
  {$EXTERNALSYM NS_E_NAMESPACE_WRONG_PERSIST}

//
// MessageId: NS_E_NAMESPACE_WRONG_TYPE
//
// MessageText:
//
//  It is not possible to store a value in a namespace node that has a different value type.%0
//
  NS_E_NAMESPACE_WRONG_TYPE        = HRESULT($C00D138B);
  {$EXTERNALSYM NS_E_NAMESPACE_WRONG_TYPE}

//
// MessageId: NS_E_NAMESPACE_NODE_CONFLICT
//
// MessageText:
//
//  It is not possible to remove the root namespace node.%0
//
  NS_E_NAMESPACE_NODE_CONFLICT     = HRESULT($C00D138C);
  {$EXTERNALSYM NS_E_NAMESPACE_NODE_CONFLICT}

//
// MessageId: NS_E_NAMESPACE_NODE_NOT_FOUND
//
// MessageText:
//
//  The specified namespace node could not be found.%0
//
  NS_E_NAMESPACE_NODE_NOT_FOUND    = HRESULT($C00D138D);
  {$EXTERNALSYM NS_E_NAMESPACE_NODE_NOT_FOUND}

//
// MessageId: NS_E_NAMESPACE_BUFFER_TOO_SMALL
//
// MessageText:
//
//  The buffer supplied to hold namespace node string is too small.%0
//
  NS_E_NAMESPACE_BUFFER_TOO_SMALL  = HRESULT($C00D138E);
  {$EXTERNALSYM NS_E_NAMESPACE_BUFFER_TOO_SMALL}

//
// MessageId: NS_E_NAMESPACE_TOO_MANY_CALLBACKS
//
// MessageText:
//
//  The callback list on a namespace node is at the maximum size.%0
//
  NS_E_NAMESPACE_TOO_MANY_CALLBACKS = HRESULT($C00D138F);
  {$EXTERNALSYM NS_E_NAMESPACE_TOO_MANY_CALLBACKS}

//
// MessageId: NS_E_NAMESPACE_DUPLICATE_CALLBACK
//
// MessageText:
//
//  It is not possible to register an already-registered callback on a namespace node.%0
//
  NS_E_NAMESPACE_DUPLICATE_CALLBACK = HRESULT($C00D1390);
  {$EXTERNALSYM NS_E_NAMESPACE_DUPLICATE_CALLBACK}

//
// MessageId: NS_E_NAMESPACE_CALLBACK_NOT_FOUND
//
// MessageText:
//
//  Cannot find the callback in the namespace when attempting to remove the callback.%0
//
  NS_E_NAMESPACE_CALLBACK_NOT_FOUND = HRESULT($C00D1391);
  {$EXTERNALSYM NS_E_NAMESPACE_CALLBACK_NOT_FOUND}

//
// MessageId: NS_E_NAMESPACE_NAME_TOO_LONG
//
// MessageText:
//
//  The namespace node name exceeds the allowed maximum length.%0
//
  NS_E_NAMESPACE_NAME_TOO_LONG     = HRESULT($C00D1392);
  {$EXTERNALSYM NS_E_NAMESPACE_NAME_TOO_LONG}

//
// MessageId: NS_E_NAMESPACE_DUPLICATE_NAME
//
// MessageText:
//
//  Cannot create a namespace node that already exists.%0
//
  NS_E_NAMESPACE_DUPLICATE_NAME    = HRESULT($C00D1393);
  {$EXTERNALSYM NS_E_NAMESPACE_DUPLICATE_NAME}

//
// MessageId: NS_E_NAMESPACE_EMPTY_NAME
//
// MessageText:
//
//  The namespace node name cannot be a null string.%0
//
  NS_E_NAMESPACE_EMPTY_NAME        = HRESULT($C00D1394);
  {$EXTERNALSYM NS_E_NAMESPACE_EMPTY_NAME}

//
// MessageId: NS_E_NAMESPACE_INDEX_TOO_LARGE
//
// MessageText:
//
//  Finding a child namespace node by index failed because the index exceeded the number of children.%0
//
  NS_E_NAMESPACE_INDEX_TOO_LARGE   = HRESULT($C00D1395);
  {$EXTERNALSYM NS_E_NAMESPACE_INDEX_TOO_LARGE}

//
// MessageId: NS_E_NAMESPACE_BAD_NAME
//
// MessageText:
//
//  The namespace node name is invalid.%0
//
  NS_E_NAMESPACE_BAD_NAME          = HRESULT($C00D1396);
  {$EXTERNALSYM NS_E_NAMESPACE_BAD_NAME}

//
// MessageId: NS_E_NAMESPACE_WRONG_SECURITY
//
// MessageText:
//
//  It is not possible to store a value in a namespace node that has a different security type.%0
//
  NS_E_NAMESPACE_WRONG_SECURITY    = HRESULT($C00D1397);
  {$EXTERNALSYM NS_E_NAMESPACE_WRONG_SECURITY}


// Cache Errors 5100-5199

//
// MessageId: NS_E_CACHE_ARCHIVE_CONFLICT
//
// MessageText:
//
//  The archive request conflicts with other requests in progress.%0
//
  NS_E_CACHE_ARCHIVE_CONFLICT      = HRESULT($C00D13EC);
  {$EXTERNALSYM NS_E_CACHE_ARCHIVE_CONFLICT}

//
// MessageId: NS_E_CACHE_ORIGIN_SERVER_NOT_FOUND
//
// MessageText:
//
//  The specified origin server cannot be found.%0
//
  NS_E_CACHE_ORIGIN_SERVER_NOT_FOUND = HRESULT($C00D13ED);
  {$EXTERNALSYM NS_E_CACHE_ORIGIN_SERVER_NOT_FOUND}

//
// MessageId: NS_E_CACHE_ORIGIN_SERVER_TIMEOUT
//
// MessageText:
//
//  The specified origin server is not responding.%0
//
  NS_E_CACHE_ORIGIN_SERVER_TIMEOUT = HRESULT($C00D13EE);
  {$EXTERNALSYM NS_E_CACHE_ORIGIN_SERVER_TIMEOUT}

//
// MessageId: NS_E_CACHE_NOT_BROADCAST
//
// MessageText:
//
//  The internal code for HTTP status code 412 Precondition Failed due to not broadcast type.%0
//
  NS_E_CACHE_NOT_BROADCAST         = HRESULT($C00D13EF);
  {$EXTERNALSYM NS_E_CACHE_NOT_BROADCAST}

//
// MessageId: NS_E_CACHE_CANNOT_BE_CACHED
//
// MessageText:
//
//  The internal code for HTTP status code 403 Forbidden due to not cacheable.%0
//
  NS_E_CACHE_CANNOT_BE_CACHED      = HRESULT($C00D13F0);
  {$EXTERNALSYM NS_E_CACHE_CANNOT_BE_CACHED}

//
// MessageId: NS_E_CACHE_NOT_MODIFIED
//
// MessageText:
//
//  The internal code for HTTP status code 304 Not Modified.%0
//
  NS_E_CACHE_NOT_MODIFIED          = HRESULT($C00D13F1);
  {$EXTERNALSYM NS_E_CACHE_NOT_MODIFIED}


// Object Model Errors 5200-5299

//
// MessageId: NS_E_CANNOT_REMOVE_PUBLISHING_POINT
//
// MessageText:
//
//  It is not possible to remove a cache or proxy publishing point.%0
//
  NS_E_CANNOT_REMOVE_PUBLISHING_POINT = HRESULT($C00D1450);
  {$EXTERNALSYM NS_E_CANNOT_REMOVE_PUBLISHING_POINT}

//
// MessageId: NS_E_CANNOT_REMOVE_PLUGIN
//
// MessageText:
//
//  It is not possible to remove the last instance of a type of plug-in.%0
//
  NS_E_CANNOT_REMOVE_PLUGIN        = HRESULT($C00D1451);
  {$EXTERNALSYM NS_E_CANNOT_REMOVE_PLUGIN}

//
// MessageId: NS_E_WRONG_PUBLISHING_POINT_TYPE
//
// MessageText:
//
//  Cache and proxy publishing points do not support this property or method.%0
//
  NS_E_WRONG_PUBLISHING_POINT_TYPE = HRESULT($C00D1452);
  {$EXTERNALSYM NS_E_WRONG_PUBLISHING_POINT_TYPE}

//
// MessageId: NS_E_UNSUPPORTED_LOAD_TYPE
//
// MessageText:
//
//  The plug-in does not support the specified load type.%0
//
  NS_E_UNSUPPORTED_LOAD_TYPE       = HRESULT($C00D1453);
  {$EXTERNALSYM NS_E_UNSUPPORTED_LOAD_TYPE}

//
// MessageId: NS_E_INVALID_PLUGIN_LOAD_TYPE_CONFIGURATION
//
// MessageText:
//
//  The plug-in does not support any load types. The plug-in must support at least one load type.%0
//
  NS_E_INVALID_PLUGIN_LOAD_TYPE_CONFIGURATION = HRESULT($C00D1454);
  {$EXTERNALSYM NS_E_INVALID_PLUGIN_LOAD_TYPE_CONFIGURATION}

//
// MessageId: NS_E_INVALID_PUBLISHING_POINT_NAME
//
// MessageText:
//
//  The publishing point name is invalid.%0
//
  NS_E_INVALID_PUBLISHING_POINT_NAME = HRESULT($C00D1455);
  {$EXTERNALSYM NS_E_INVALID_PUBLISHING_POINT_NAME}

//
// MessageId: NS_E_TOO_MANY_MULTICAST_SINKS
//
// MessageText:
//
//  Only one multicast data writer plug-in can be enabled for a publishing point.%0
//
  NS_E_TOO_MANY_MULTICAST_SINKS    = HRESULT($C00D1456);
  {$EXTERNALSYM NS_E_TOO_MANY_MULTICAST_SINKS}

//
// MessageId: NS_E_PUBLISHING_POINT_INVALID_REQUEST_WHILE_STARTED
//
// MessageText:
//
//  The requested operation cannot be completed while the publishing point is started.%0
//
  NS_E_PUBLISHING_POINT_INVALID_REQUEST_WHILE_STARTED = HRESULT($C00D1457);
  {$EXTERNALSYM NS_E_PUBLISHING_POINT_INVALID_REQUEST_WHILE_STARTED}

//
// MessageId: NS_E_MULTICAST_PLUGIN_NOT_ENABLED
//
// MessageText:
//
//  A multicast data writer plug-in must be enabled in order for this operation to be completed.%0
//
  NS_E_MULTICAST_PLUGIN_NOT_ENABLED = HRESULT($C00D1458);
  {$EXTERNALSYM NS_E_MULTICAST_PLUGIN_NOT_ENABLED}

//
// MessageId: NS_E_INVALID_OPERATING_SYSTEM_VERSION
//
// MessageText:
//
//  This feature requires Windows .NET Enterprise Server.%0
//
  NS_E_INVALID_OPERATING_SYSTEM_VERSION = HRESULT($C00D1459);
  {$EXTERNALSYM NS_E_INVALID_OPERATING_SYSTEM_VERSION}

//
// MessageId: NS_E_PUBLISHING_POINT_REMOVED
//
// MessageText:
//
//  The requested operation cannot be completed because the specified publishing point has been removed.%0
//
  NS_E_PUBLISHING_POINT_REMOVED    = HRESULT($C00D145A);
  {$EXTERNALSYM NS_E_PUBLISHING_POINT_REMOVED}

//
// MessageId: NS_E_INVALID_PUSH_PUBLISHING_POINT_START_REQUEST
//
// MessageText:
//
//  Push publishing points are started when the encoder starts pushing the stream. This publishing point cannot be started by the server administrator.%0
//
  NS_E_INVALID_PUSH_PUBLISHING_POINT_START_REQUEST = HRESULT($C00D145B);
  {$EXTERNALSYM NS_E_INVALID_PUSH_PUBLISHING_POINT_START_REQUEST}

//
// MessageId: NS_E_UNSUPPORTED_LANGUAGE
//
// MessageText:
//
//  The specified language is not supported.%0
//
  NS_E_UNSUPPORTED_LANGUAGE        = HRESULT($C00D145C);
  {$EXTERNALSYM NS_E_UNSUPPORTED_LANGUAGE}

//
// MessageId: NS_E_WRONG_OS_VERSION
//
// MessageText:
//
//  Windows Media Services will only run on Windows .NET Server and Windows .NET Enterprise Server.%0
//
  NS_E_WRONG_OS_VERSION            = HRESULT($C00D145D);
  {$EXTERNALSYM NS_E_WRONG_OS_VERSION}

//
// MessageId: NS_E_PUBLISHING_POINT_STOPPED
//
// MessageText:
//
//  The operation cannot be completed because the publishing point has been stopped.%0
//
  NS_E_PUBLISHING_POINT_STOPPED    = HRESULT($C00D145E);
  {$EXTERNALSYM NS_E_PUBLISHING_POINT_STOPPED}


// Playlist Errors 5300-5399

//
// MessageId: NS_E_PLAYLIST_ENTRY_ALREADY_PLAYING
//
// MessageText:
//
//  The playlist entry is already playing.%0
//
  NS_E_PLAYLIST_ENTRY_ALREADY_PLAYING = HRESULT($C00D14B4);
  {$EXTERNALSYM NS_E_PLAYLIST_ENTRY_ALREADY_PLAYING}

//
// MessageId: NS_E_EMPTY_PLAYLIST
//
// MessageText:
//
//  The playlist or directory you are requesting does not contain content.%0
//
  NS_E_EMPTY_PLAYLIST              = HRESULT($C00D14B5);
  {$EXTERNALSYM NS_E_EMPTY_PLAYLIST}

//
// MessageId: NS_E_PLAYLIST_PARSE_FAILURE
//
// MessageText:
//
//  The server was unable to parse the requested playlist file.%0
//
  NS_E_PLAYLIST_PARSE_FAILURE      = HRESULT($C00D14B6);
  {$EXTERNALSYM NS_E_PLAYLIST_PARSE_FAILURE}

//
// MessageId: NS_E_PLAYLIST_UNSUPPORTED_ENTRY
//
// MessageText:
//
//  The requested operation is not supported for this type of playlist entry.%0
//
  NS_E_PLAYLIST_UNSUPPORTED_ENTRY  = HRESULT($C00D14B7);
  {$EXTERNALSYM NS_E_PLAYLIST_UNSUPPORTED_ENTRY}

//
// MessageId: NS_E_PLAYLIST_ENTRY_NOT_IN_PLAYLIST
//
// MessageText:
//
//  Cannot jump to a playlist entry that is not inserted in the playlist.%0
//
  NS_E_PLAYLIST_ENTRY_NOT_IN_PLAYLIST = HRESULT($C00D14B8);
  {$EXTERNALSYM NS_E_PLAYLIST_ENTRY_NOT_IN_PLAYLIST}

//
// MessageId: NS_E_PLAYLIST_ENTRY_SEEK
//
// MessageText:
//
//  Cannot seek to the desired playlist entry.%0
//
  NS_E_PLAYLIST_ENTRY_SEEK         = HRESULT($C00D14B9);
  {$EXTERNALSYM NS_E_PLAYLIST_ENTRY_SEEK}

//
// MessageId: NS_E_PLAYLIST_RECURSIVE_PLAYLISTS
//
// MessageText:
//
//  Cannot play recursive playlist.%0
//
  NS_E_PLAYLIST_RECURSIVE_PLAYLISTS = HRESULT($C00D14BA);
  {$EXTERNALSYM NS_E_PLAYLIST_RECURSIVE_PLAYLISTS}

//
// MessageId: NS_E_PLAYLIST_TOO_MANY_NESTED_PLAYLISTS
//
// MessageText:
//
//  The number of nested playlists exceeded the limit the server can handle.%0
//
  NS_E_PLAYLIST_TOO_MANY_NESTED_PLAYLISTS = HRESULT($C00D14BB);
  {$EXTERNALSYM NS_E_PLAYLIST_TOO_MANY_NESTED_PLAYLISTS}

//
// MessageId: NS_E_PLAYLIST_SHUTDOWN
//
// MessageText:
//
//  Cannot execute the requested operation because the playlist has been shut down by the Media Server.%0
//
  NS_E_PLAYLIST_SHUTDOWN           = HRESULT($C00D14BC);
  {$EXTERNALSYM NS_E_PLAYLIST_SHUTDOWN}

//
// MessageId: NS_E_PLAYLIST_END_RECEDING
//
// MessageText:
//
//  The playlist has ended while receding.%0
//
  NS_E_PLAYLIST_END_RECEDING       = HRESULT($C00D14BD);
  {$EXTERNALSYM NS_E_PLAYLIST_END_RECEDING}

//
// MessageId: NS_I_PLAYLIST_CHANGE_RECEDING
//
// MessageText:
//
//  The playlist change occurred while receding.%0
//
  NS_I_PLAYLIST_CHANGE_RECEDING    = HRESULT($400D14BE);
  {$EXTERNALSYM NS_I_PLAYLIST_CHANGE_RECEDING}


// Datapath Errors -- 5400 - 5499

//
// MessageId: NS_E_DATAPATH_NO_SINK
//
// MessageText:
//
//  The data path does not have an associated data writer plug-in.%0
//
  NS_E_DATAPATH_NO_SINK            = HRESULT($C00D1518);
  {$EXTERNALSYM NS_E_DATAPATH_NO_SINK}

//
// MessageId: NS_S_PUBLISHING_POINT_STARTED_WITH_FAILED_SINKS
//
// MessageText:
//
//  The publishing point successfully started, but one or more of the requested data writer plug-ins failed.%0
//
  NS_S_PUBLISHING_POINT_STARTED_WITH_FAILED_SINKS = HRESULT($000D1519);
  {$EXTERNALSYM NS_S_PUBLISHING_POINT_STARTED_WITH_FAILED_SINKS}

//
// MessageId: NS_E_INVALID_PUSH_TEMPLATE
//
// MessageText:
//
//  The specified push template is invalid.%0
//
  NS_E_INVALID_PUSH_TEMPLATE       = HRESULT($C00D151A);
  {$EXTERNALSYM NS_E_INVALID_PUSH_TEMPLATE}

//
// MessageId: NS_E_INVALID_PUSH_PUBLISHING_POINT
//
// MessageText:
//
//  The specified push publishing point is invalid.%0
//
  NS_E_INVALID_PUSH_PUBLISHING_POINT = HRESULT($C00D151B);
  {$EXTERNALSYM NS_E_INVALID_PUSH_PUBLISHING_POINT}

//
// MessageId: NS_E_CRITICAL_ERROR
//
// MessageText:
//
//  The requested operation cannot be performed because the server or publishing point is in a critical error state.%0
//
  NS_E_CRITICAL_ERROR              = HRESULT($C00D151C);
  {$EXTERNALSYM NS_E_CRITICAL_ERROR}

//
// MessageId: NS_E_NO_NEW_CONNECTIONS
//
// MessageText:
//
//  The content can not be played because the server is not currently accepting connections. Try connecting at a later time.%0
//
  NS_E_NO_NEW_CONNECTIONS          = HRESULT($C00D151D);
  {$EXTERNALSYM NS_E_NO_NEW_CONNECTIONS}

//
// MessageId: NS_E_WSX_INVALID_VERSION
//
// MessageText:
//
//  The version of this playlist is not supported by the server.%0
//
  NS_E_WSX_INVALID_VERSION         = HRESULT($C00D151E);
  {$EXTERNALSYM NS_E_WSX_INVALID_VERSION}

//
// MessageId: NS_E_HEADER_MISMATCH
//
// MessageText:
//
//  The command does not apply to the current media header user by a server component.%0
//
  NS_E_HEADER_MISMATCH             = HRESULT($C00D151F);
  {$EXTERNALSYM NS_E_HEADER_MISMATCH}

//
// MessageId: NS_E_PUSH_DUPLICATE_PUBLISHING_POINT_NAME
//
// MessageText:
//
//  The specified publishing point name is already in use.%0
//
  NS_E_PUSH_DUPLICATE_PUBLISHING_POINT_NAME = HRESULT($C00D1520);
  {$EXTERNALSYM NS_E_PUSH_DUPLICATE_PUBLISHING_POINT_NAME}


// Plugin Errors -- 5500 - 5599

//
// MessageId: NS_E_NO_SCRIPT_ENGINE
//
// MessageText:
//
//  There is no script engine available for this file.%0
//
  NS_E_NO_SCRIPT_ENGINE            = HRESULT($C00D157C);
  {$EXTERNALSYM NS_E_NO_SCRIPT_ENGINE}

//
// MessageId: NS_E_PLUGIN_ERROR_REPORTED
//
// MessageText:
//
//  The plug-in has reported an error. See the Troubleshooting tab or the NT Application Event Log for details.%0
//
  NS_E_PLUGIN_ERROR_REPORTED       = HRESULT($C00D157D);
  {$EXTERNALSYM NS_E_PLUGIN_ERROR_REPORTED}

//
// MessageId: NS_E_SOURCE_PLUGIN_NOT_FOUND
//
// MessageText:
//
//  No enabled data source plug-in is available to access the requested content.%0
//
  NS_E_SOURCE_PLUGIN_NOT_FOUND     = HRESULT($C00D157E);
  {$EXTERNALSYM NS_E_SOURCE_PLUGIN_NOT_FOUND}

//
// MessageId: NS_E_PLAYLIST_PLUGIN_NOT_FOUND
//
// MessageText:
//
//  No enabled playlist parser plug-in is available to access the requested content.%0
//
  NS_E_PLAYLIST_PLUGIN_NOT_FOUND   = HRESULT($C00D157F);
  {$EXTERNALSYM NS_E_PLAYLIST_PLUGIN_NOT_FOUND}

//
// MessageId: NS_E_DATA_SOURCE_ENUMERATION_NOT_SUPPORTED
//
// MessageText:
//
//  The data source plug-in does not support enumeration.%0
//
  NS_E_DATA_SOURCE_ENUMERATION_NOT_SUPPORTED = HRESULT($C00D1580);
  {$EXTERNALSYM NS_E_DATA_SOURCE_ENUMERATION_NOT_SUPPORTED}

//
// MessageId: NS_E_MEDIA_PARSER_INVALID_FORMAT
//
// MessageText:
//
//  The server cannot stream the selected file because it is either damaged or corrupt. Select a different file.%0
//
  NS_E_MEDIA_PARSER_INVALID_FORMAT = HRESULT($C00D1581);
  {$EXTERNALSYM NS_E_MEDIA_PARSER_INVALID_FORMAT}

//
// MessageId: NS_E_SCRIPT_DEBUGGER_NOT_INSTALLED
//
// MessageText:
//
//  The plug-in cannot be enabled because a compatible script debugger is not installed
//  on this system.  Install a script debugger, or disable the script debugger option on the general
//  tab of the plug-in's properties page and try again.%0
//
  NS_E_SCRIPT_DEBUGGER_NOT_INSTALLED = HRESULT($C00D1582);
  {$EXTERNALSYM NS_E_SCRIPT_DEBUGGER_NOT_INSTALLED}

//
// MessageId: NS_E_FEATURE_REQUIRES_ENTERPRISE_SERVER
//
// MessageText:
//
//  The plug-in cannot be loaded because it requires Windows .NET Enterprise Server.%0
//
  NS_E_FEATURE_REQUIRES_ENTERPRISE_SERVER = HRESULT($C00D1583);
  {$EXTERNALSYM NS_E_FEATURE_REQUIRES_ENTERPRISE_SERVER}

//
// MessageId: NS_E_WIZARD_RUNNING
//
// MessageText:
//
//  Another wizard is currently running. Please close the other wizard or wait until it finishes before attempting to run this wizard again.%0
//
  NS_E_WIZARD_RUNNING              = HRESULT($C00D1584);
  {$EXTERNALSYM NS_E_WIZARD_RUNNING}

//
// MessageId: NS_E_INVALID_LOG_URL
//
// MessageText:
//
//  Invalid log URL. Multicast logging URL must look like "http://servername/isapibackend.dll" .%0
//
  NS_E_INVALID_LOG_URL             = HRESULT($C00D1585);
  {$EXTERNALSYM NS_E_INVALID_LOG_URL}

//
// MessageId: NS_E_INVALID_MTU_RANGE
//
// MessageText:
//
//  Invalid MTU specified. The valid range for maximum packet size is between 36  and 65507 bytes .%0
//
  NS_E_INVALID_MTU_RANGE           = HRESULT($C00D1586);
  {$EXTERNALSYM NS_E_INVALID_MTU_RANGE}

//
// MessageId: NS_E_INVALID_PLAY_STATISTICS
//
// MessageText:
//
//  Invalid play statistics for logging .%0
//
  NS_E_INVALID_PLAY_STATISTICS     = HRESULT($C00D1587);
  {$EXTERNALSYM NS_E_INVALID_PLAY_STATISTICS}

//
// MessageId: NS_E_LOG_NEED_TO_BE_SKIPPED
//
// MessageText:
//
//  The log needs to be skipped .%0
//
  NS_E_LOG_NEED_TO_BE_SKIPPED      = HRESULT($C00D1588);
  {$EXTERNALSYM NS_E_LOG_NEED_TO_BE_SKIPPED}

//
// MessageId: NS_E_HTTP_TEXT_DATACONTAINER_SIZE_LIMIT_EXCEEDED
//
// MessageText:
//
//  The size of the data exceeded the limit the WMS HTTP Download Data Source plugin can handle.%0
//
  NS_E_HTTP_TEXT_DATACONTAINER_SIZE_LIMIT_EXCEEDED = HRESULT($C00D1589);
  {$EXTERNALSYM NS_E_HTTP_TEXT_DATACONTAINER_SIZE_LIMIT_EXCEEDED}

//
// MessageId: NS_E_PORT_IN_USE
//
// MessageText:
//
//  One usage of each socket address (protocol/network address/port) is permitted.
//  Verify that other services or applications are not attempting to use the same
//  port and then try to enable the plug-in again.%0
//
  NS_E_PORT_IN_USE                 = HRESULT($C00D158A);
  {$EXTERNALSYM NS_E_PORT_IN_USE}

//
// MessageId: NS_E_PORT_IN_USE_HTTP
//
// MessageText:
//
//  One usage of each socket address (protocol/network address/port) is permitted.
//  Verify that other services (such as IIS) or applications are not attempting to use
//  the same port and then try to enable the plug-in again.%0
//
  NS_E_PORT_IN_USE_HTTP            = HRESULT($C00D158B);
  {$EXTERNALSYM NS_E_PORT_IN_USE_HTTP}

//
// MessageId: NS_E_HTTP_TEXT_DATACONTAINER_INVALID_SERVER_RESPONSE
//
// MessageText:
//
//  The WMS HTTP Download Data Source plugin was unable to receive the remote server's response.%0
//
  NS_E_HTTP_TEXT_DATACONTAINER_INVALID_SERVER_RESPONSE = HRESULT($C00D158C);
  {$EXTERNALSYM NS_E_HTTP_TEXT_DATACONTAINER_INVALID_SERVER_RESPONSE}

//
// MessageId: NS_E_ARCHIVE_REACH_QUOTA
//
// MessageText:
//
//  The archive plug-in has reached its quota.%0
//
  NS_E_ARCHIVE_REACH_QUOTA         = HRESULT($C00D158D);
  {$EXTERNALSYM NS_E_ARCHIVE_REACH_QUOTA}

//
// MessageId: NS_E_ARCHIVE_ABORT_DUE_TO_BCAST
//
// MessageText:
//
//  The archive plug-in aborted because the source was from broadcast.%0
//
  NS_E_ARCHIVE_ABORT_DUE_TO_BCAST  = HRESULT($C00D158E);
  {$EXTERNALSYM NS_E_ARCHIVE_ABORT_DUE_TO_BCAST}

//
// MessageId: NS_E_ARCHIVE_GAP_DETECTED
//
// MessageText:
//
//  The archive plug-in detected an interrupt in the source.%0
//
  NS_E_ARCHIVE_GAP_DETECTED        = HRESULT($C00D158F);
  {$EXTERNALSYM NS_E_ARCHIVE_GAP_DETECTED}



/////////////////////////////////////////////////////////////////////////
//
// Windows Media Tools Errors
//
// IdRange = 7000 - 7999
//
/////////////////////////////////////////////////////////////////////////

//
// MessageId: NS_E_BAD_MARKIN
//
// MessageText:
//
//  The mark-in time should be greater than 0 and less than the mark-out time.%0
//
  NS_E_BAD_MARKIN                  = HRESULT($C00D1B58);
  {$EXTERNALSYM NS_E_BAD_MARKIN}

//
// MessageId: NS_E_BAD_MARKOUT
//
// MessageText:
//
//  The mark-out time should be greater than the mark-in time and less than the file duration.%0
//
  NS_E_BAD_MARKOUT                 = HRESULT($C00D1B59);
  {$EXTERNALSYM NS_E_BAD_MARKOUT}

//
// MessageId: NS_E_NOMATCHING_MEDIASOURCE
//
// MessageText:
//
//  No matching media type is found in the source %1.%0
//
  NS_E_NOMATCHING_MEDIASOURCE      = HRESULT($C00D1B5A);
  {$EXTERNALSYM NS_E_NOMATCHING_MEDIASOURCE}

//
// MessageId: NS_E_UNSUPPORTED_SOURCETYPE
//
// MessageText:
//
//  The specified source type is not supported.%0
//
  NS_E_UNSUPPORTED_SOURCETYPE      = HRESULT($C00D1B5B);
  {$EXTERNALSYM NS_E_UNSUPPORTED_SOURCETYPE}

//
// MessageId: NS_E_TOO_MANY_AUDIO
//
// MessageText:
//
//  It is not possible to specify more than one audio input.%0
//
  NS_E_TOO_MANY_AUDIO              = HRESULT($C00D1B5C);
  {$EXTERNALSYM NS_E_TOO_MANY_AUDIO}

//
// MessageId: NS_E_TOO_MANY_VIDEO
//
// MessageText:
//
//  It is not possible to specify more than two video inputs.%0
//
  NS_E_TOO_MANY_VIDEO              = HRESULT($C00D1B5D);
  {$EXTERNALSYM NS_E_TOO_MANY_VIDEO}

//
// MessageId: NS_E_NOMATCHING_ELEMENT
//
// MessageText:
//
//  No matching element is found in the list.%0
//
  NS_E_NOMATCHING_ELEMENT          = HRESULT($C00D1B5E);
  {$EXTERNALSYM NS_E_NOMATCHING_ELEMENT}

//
// MessageId: NS_E_MISMATCHED_MEDIACONTENT
//
// MessageText:
//
//  The profile's media types must match the media types defined for the session.%0
//
  NS_E_MISMATCHED_MEDIACONTENT     = HRESULT($C00D1B5F);
  {$EXTERNALSYM NS_E_MISMATCHED_MEDIACONTENT}

//
// MessageId: NS_E_CANNOT_DELETE_ACTIVE_SOURCEGROUP
//
// MessageText:
//
//  It is not possible to remove an active source while encoding.%0
//
  NS_E_CANNOT_DELETE_ACTIVE_SOURCEGROUP = HRESULT($C00D1B60);
  {$EXTERNALSYM NS_E_CANNOT_DELETE_ACTIVE_SOURCEGROUP}

//
// MessageId: NS_E_AUDIODEVICE_BUSY
//
// MessageText:
//
//  It is not possible to open the specified audio capture device because it is currently in use.%0
//
  NS_E_AUDIODEVICE_BUSY            = HRESULT($C00D1B61);
  {$EXTERNALSYM NS_E_AUDIODEVICE_BUSY}

//
// MessageId: NS_E_AUDIODEVICE_UNEXPECTED
//
// MessageText:
//
//  It is not possible to open the specified audio capture device because an unexpected error has occurred.%0
//
  NS_E_AUDIODEVICE_UNEXPECTED      = HRESULT($C00D1B62);
  {$EXTERNALSYM NS_E_AUDIODEVICE_UNEXPECTED}

//
// MessageId: NS_E_AUDIODEVICE_BADFORMAT
//
// MessageText:
//
//  The audio capture device does not support the specified audio format.%0
//
  NS_E_AUDIODEVICE_BADFORMAT       = HRESULT($C00D1B63);
  {$EXTERNALSYM NS_E_AUDIODEVICE_BADFORMAT}

//
// MessageId: NS_E_VIDEODEVICE_BUSY
//
// MessageText:
//
//  It is not possible to open the specified video capture device because it is currently in use.%0
//
  NS_E_VIDEODEVICE_BUSY            = HRESULT($C00D1B64);
  {$EXTERNALSYM NS_E_VIDEODEVICE_BUSY}

//
// MessageId: NS_E_VIDEODEVICE_UNEXPECTED
//
// MessageText:
//
//  It is not possible to open the specified video capture device because an unexpected error has occurred.%0
//
  NS_E_VIDEODEVICE_UNEXPECTED      = HRESULT($C00D1B65);
  {$EXTERNALSYM NS_E_VIDEODEVICE_UNEXPECTED}

//
// MessageId: NS_E_INVALIDCALL_WHILE_ENCODER_RUNNING
//
// MessageText:
//
//  This operation is not allowed while encoding.%0
//
  NS_E_INVALIDCALL_WHILE_ENCODER_RUNNING = HRESULT($C00D1B66);
  {$EXTERNALSYM NS_E_INVALIDCALL_WHILE_ENCODER_RUNNING}

//
// MessageId: NS_E_NO_PROFILE_IN_SOURCEGROUP
//
// MessageText:
//
//  No profile is set for the source.%0
//
  NS_E_NO_PROFILE_IN_SOURCEGROUP   = HRESULT($C00D1B67);
  {$EXTERNALSYM NS_E_NO_PROFILE_IN_SOURCEGROUP}

//
// MessageId: NS_E_VIDEODRIVER_UNSTABLE
//
// MessageText:
//
//  The video capture driver returned an unrecoverable error.  It is now in an unstable state.%0
//
  NS_E_VIDEODRIVER_UNSTABLE        = HRESULT($C00D1B68);
  {$EXTERNALSYM NS_E_VIDEODRIVER_UNSTABLE}

//
// MessageId: NS_E_VIDCAPSTARTFAILED
//
// MessageText:
//
//  It was not possible to start the video device.%0
//
  NS_E_VIDCAPSTARTFAILED           = HRESULT($C00D1B69);
  {$EXTERNALSYM NS_E_VIDCAPSTARTFAILED}

//
// MessageId: NS_E_VIDSOURCECOMPRESSION
//
// MessageText:
//
//  The video source does not support the requested output format or color depth.%0
//
  NS_E_VIDSOURCECOMPRESSION        = HRESULT($C00D1B6A);
  {$EXTERNALSYM NS_E_VIDSOURCECOMPRESSION}

//
// MessageId: NS_E_VIDSOURCESIZE
//
// MessageText:
//
//  The video source does not support the requested capture size.%0
//
  NS_E_VIDSOURCESIZE               = HRESULT($C00D1B6B);
  {$EXTERNALSYM NS_E_VIDSOURCESIZE}

//
// MessageId: NS_E_ICMQUERYFORMAT
//
// MessageText:
//
//  It was not possible to obtain output information from the video compressor.%0
//
  NS_E_ICMQUERYFORMAT              = HRESULT($C00D1B6C);
  {$EXTERNALSYM NS_E_ICMQUERYFORMAT}

//
// MessageId: NS_E_VIDCAPCREATEWINDOW
//
// MessageText:
//
//  It was not possible to create a video capture window.%0
//
  NS_E_VIDCAPCREATEWINDOW          = HRESULT($C00D1B6D);
  {$EXTERNALSYM NS_E_VIDCAPCREATEWINDOW}

//
// MessageId: NS_E_VIDCAPDRVINUSE
//
// MessageText:
//
//  There is already a stream active on this video device.%0
//
  NS_E_VIDCAPDRVINUSE              = HRESULT($C00D1B6E);
  {$EXTERNALSYM NS_E_VIDCAPDRVINUSE}

//
// MessageId: NS_E_NO_MEDIAFORMAT_IN_SOURCE
//
// MessageText:
//
//  No media format is set in source.%0
//
  NS_E_NO_MEDIAFORMAT_IN_SOURCE    = HRESULT($C00D1B6F);
  {$EXTERNALSYM NS_E_NO_MEDIAFORMAT_IN_SOURCE}

//
// MessageId: NS_E_NO_VALID_OUTPUT_STREAM
//
// MessageText:
//
//  Cannot find a valid output stream from the source.%0
//
  NS_E_NO_VALID_OUTPUT_STREAM      = HRESULT($C00D1B70);
  {$EXTERNALSYM NS_E_NO_VALID_OUTPUT_STREAM}

//
// MessageId: NS_E_NO_VALID_SOURCE_PLUGIN
//
// MessageText:
//
//  It was not possible to find a valid source plug-in for the specified source.%0
//
  NS_E_NO_VALID_SOURCE_PLUGIN      = HRESULT($C00D1B71);
  {$EXTERNALSYM NS_E_NO_VALID_SOURCE_PLUGIN}

//
// MessageId: NS_E_NO_ACTIVE_SOURCEGROUP
//
// MessageText:
//
//  No source is currently active.%0
//
  NS_E_NO_ACTIVE_SOURCEGROUP       = HRESULT($C00D1B72);
  {$EXTERNALSYM NS_E_NO_ACTIVE_SOURCEGROUP}

//
// MessageId: NS_E_NO_SCRIPT_STREAM
//
// MessageText:
//
//  No script stream is set in the current source.%0
//
  NS_E_NO_SCRIPT_STREAM            = HRESULT($C00D1B73);
  {$EXTERNALSYM NS_E_NO_SCRIPT_STREAM}

//
// MessageId: NS_E_INVALIDCALL_WHILE_ARCHIVAL_RUNNING
//
// MessageText:
//
//  This operation is not allowed while archiving.%0
//
  NS_E_INVALIDCALL_WHILE_ARCHIVAL_RUNNING = HRESULT($C00D1B74);
  {$EXTERNALSYM NS_E_INVALIDCALL_WHILE_ARCHIVAL_RUNNING}

//
// MessageId: NS_E_INVALIDPACKETSIZE
//
// MessageText:
//
//  The setting for the maximum packet size is not valid.%0
//
  NS_E_INVALIDPACKETSIZE           = HRESULT($C00D1B75);
  {$EXTERNALSYM NS_E_INVALIDPACKETSIZE}

//
// MessageId: NS_E_PLUGIN_CLSID_INVALID
//
// MessageText:
//
//  The plug-in CLSID specified is not valid.%0
//
  NS_E_PLUGIN_CLSID_INVALID        = HRESULT($C00D1B76);
  {$EXTERNALSYM NS_E_PLUGIN_CLSID_INVALID}

//
// MessageId: NS_E_UNSUPPORTED_ARCHIVETYPE
//
// MessageText:
//
//  This archive type is not supported.%0
//
  NS_E_UNSUPPORTED_ARCHIVETYPE     = HRESULT($C00D1B77);
  {$EXTERNALSYM NS_E_UNSUPPORTED_ARCHIVETYPE}

//
// MessageId: NS_E_UNSUPPORTED_ARCHIVEOPERATION
//
// MessageText:
//
//  This archive operation is not supported.%0
//
  NS_E_UNSUPPORTED_ARCHIVEOPERATION = HRESULT($C00D1B78);
  {$EXTERNALSYM NS_E_UNSUPPORTED_ARCHIVEOPERATION}

//
// MessageId: NS_E_ARCHIVE_FILENAME_NOTSET
//
// MessageText:
//
//  The local archive file name was not set.%0
//
  NS_E_ARCHIVE_FILENAME_NOTSET     = HRESULT($C00D1B79);
  {$EXTERNALSYM NS_E_ARCHIVE_FILENAME_NOTSET}

//
// MessageId: NS_E_SOURCEGROUP_NOTPREPARED
//
// MessageText:
//
//  The source is not yet prepared.%0
//
  NS_E_SOURCEGROUP_NOTPREPARED     = HRESULT($C00D1B7A);
  {$EXTERNALSYM NS_E_SOURCEGROUP_NOTPREPARED}

//
// MessageId: NS_E_PROFILE_MISMATCH
//
// MessageText:
//
//  Profiles on the sources do not match.%0
//
  NS_E_PROFILE_MISMATCH            = HRESULT($C00D1B7B);
  {$EXTERNALSYM NS_E_PROFILE_MISMATCH}

//
// MessageId: NS_E_INCORRECTCLIPSETTINGS
//
// MessageText:
//
//  The specified crop values are not valid.%0
//
  NS_E_INCORRECTCLIPSETTINGS       = HRESULT($C00D1B7C);
  {$EXTERNALSYM NS_E_INCORRECTCLIPSETTINGS}

//
// MessageId: NS_E_NOSTATSAVAILABLE
//
// MessageText:
//
//  No statistics are available at this time.%0
//
  NS_E_NOSTATSAVAILABLE            = HRESULT($C00D1B7D);
  {$EXTERNALSYM NS_E_NOSTATSAVAILABLE}

//
// MessageId: NS_E_NOTARCHIVING
//
// MessageText:
//
//  The encoder is not archiving.%0
//
  NS_E_NOTARCHIVING                = HRESULT($C00D1B7E);
  {$EXTERNALSYM NS_E_NOTARCHIVING}

//
// MessageId: NS_E_INVALIDCALL_WHILE_ENCODER_STOPPED
//
// MessageText:
//
//  This operation is only allowed during encoding.%0
//
  NS_E_INVALIDCALL_WHILE_ENCODER_STOPPED = HRESULT($C00D1B7F);
  {$EXTERNALSYM NS_E_INVALIDCALL_WHILE_ENCODER_STOPPED}

//
// MessageId: NS_E_NOSOURCEGROUPS
//
// MessageText:
//
//  This SourceGroupCollection doesn't contain any SourceGroups.%0
//
  NS_E_NOSOURCEGROUPS              = HRESULT($C00D1B80);
  {$EXTERNALSYM NS_E_NOSOURCEGROUPS}

//
// MessageId: NS_E_INVALIDINPUTFPS
//
// MessageText:
//
//  This source does not have a frame rate of 30 fps. Therefore, it is not possible to apply the inverse telecine filter to the source.%0
//
  NS_E_INVALIDINPUTFPS             = HRESULT($C00D1B81);
  {$EXTERNALSYM NS_E_INVALIDINPUTFPS}

//
// MessageId: NS_E_NO_DATAVIEW_SUPPORT
//
// MessageText:
//
//  It is not possible to display your source or output video in the Video panel.%0
//
  NS_E_NO_DATAVIEW_SUPPORT         = HRESULT($C00D1B82);
  {$EXTERNALSYM NS_E_NO_DATAVIEW_SUPPORT}

//
// MessageId: NS_E_CODEC_UNAVAILABLE
//
// MessageText:
//
//  One or more codecs required to open this content could not be found.%0
//
  NS_E_CODEC_UNAVAILABLE           = HRESULT($C00D1B83);
  {$EXTERNALSYM NS_E_CODEC_UNAVAILABLE}

//
// MessageId: NS_E_ARCHIVE_SAME_AS_INPUT
//
// MessageText:
//
//  The archive file has the same name as an input file. Change one of the names before continuing.%0
//
  NS_E_ARCHIVE_SAME_AS_INPUT       = HRESULT($C00D1B84);
  {$EXTERNALSYM NS_E_ARCHIVE_SAME_AS_INPUT}

//
// MessageId: NS_E_SOURCE_NOTSPECIFIED
//
// MessageText:
//
//  The source has not been set up completely.%0
//
  NS_E_SOURCE_NOTSPECIFIED         = HRESULT($C00D1B85);
  {$EXTERNALSYM NS_E_SOURCE_NOTSPECIFIED}

//
// MessageId: NS_E_NO_REALTIME_TIMECOMPRESSION
//
// MessageText:
//
//  It is not possible to apply time compression to a broadcast session.%0
//
  NS_E_NO_REALTIME_TIMECOMPRESSION = HRESULT($C00D1B86);
  {$EXTERNALSYM NS_E_NO_REALTIME_TIMECOMPRESSION}

//
// MessageId: NS_E_UNSUPPORTED_ENCODER_DEVICE
//
// MessageText:
//
//  It is not possible to open this device.%0
//
  NS_E_UNSUPPORTED_ENCODER_DEVICE  = HRESULT($C00D1B87);
  {$EXTERNALSYM NS_E_UNSUPPORTED_ENCODER_DEVICE}

//
// MessageId: NS_E_UNEXPECTED_DISPLAY_SETTINGS
//
// MessageText:
//
//  It is not possible to start encoding because the display size or color has changed since the current session was defined. Restore the previous settings or create a new session.%0
//
  NS_E_UNEXPECTED_DISPLAY_SETTINGS = HRESULT($C00D1B88);
  {$EXTERNALSYM NS_E_UNEXPECTED_DISPLAY_SETTINGS}

//
// MessageId: NS_E_NO_AUDIODATA
//
// MessageText:
//
//  No audio data has been received for several seconds. Check the audio source and restart the encoder.%0
//
  NS_E_NO_AUDIODATA                = HRESULT($C00D1B89);
  {$EXTERNALSYM NS_E_NO_AUDIODATA}

//
// MessageId: NS_E_INPUTSOURCE_PROBLEM
//
// MessageText:
//
//  One or all of the specified sources are not working properly. Check that the sources are configured correctly.%0
//
  NS_E_INPUTSOURCE_PROBLEM         = HRESULT($C00D1B8A);
  {$EXTERNALSYM NS_E_INPUTSOURCE_PROBLEM}

//
// MessageId: NS_E_WME_VERSION_MISMATCH
//
// MessageText:
//
//  The supplied configuration file is not supported by this version of the encoder.%0
//
  NS_E_WME_VERSION_MISMATCH        = HRESULT($C00D1B8B);
  {$EXTERNALSYM NS_E_WME_VERSION_MISMATCH}

//
// MessageId: NS_E_NO_REALTIME_PREPROCESS
//
// MessageText:
//
//  It is not possible to use image preprocessing with live encoding.%0
//
  NS_E_NO_REALTIME_PREPROCESS      = HRESULT($C00D1B8C);
  {$EXTERNALSYM NS_E_NO_REALTIME_PREPROCESS}

//
// MessageId: NS_E_NO_REPEAT_PREPROCESS
//
// MessageText:
//
//  It is not possible to use two-pass encoding when the source is set to loop.%0
//
  NS_E_NO_REPEAT_PREPROCESS        = HRESULT($C00D1B8D);
  {$EXTERNALSYM NS_E_NO_REPEAT_PREPROCESS}

//
// MessageId: NS_E_CANNOT_PAUSE_LIVEBROADCAST
//
// MessageText:
//
//  It is not possible to pause encoding during a broadcast.%0
//
  NS_E_CANNOT_PAUSE_LIVEBROADCAST  = HRESULT($C00D1B8E);
  {$EXTERNALSYM NS_E_CANNOT_PAUSE_LIVEBROADCAST}

//
// MessageId: NS_E_DRM_PROFILE_NOT_SET
//
// MessageText:
//
//  A DRM profile has not been set for the current session.%0
//
  NS_E_DRM_PROFILE_NOT_SET         = HRESULT($C00D1B8F);
  {$EXTERNALSYM NS_E_DRM_PROFILE_NOT_SET}

//
// MessageId: NS_E_DUPLICATE_DRMPROFILE
//
// MessageText:
//
//  The profile ID is already used by a DRM profile. Specify a different profile ID.%0
//
  NS_E_DUPLICATE_DRMPROFILE        = HRESULT($C00D1B90);
  {$EXTERNALSYM NS_E_DUPLICATE_DRMPROFILE}

//
// MessageId: NS_E_INVALID_DEVICE
//
// MessageText:
//
//  The setting of the selected device does not support control for playing back tapes.%0
//
  NS_E_INVALID_DEVICE              = HRESULT($C00D1B91);
  {$EXTERNALSYM NS_E_INVALID_DEVICE}

//
// MessageId: NS_E_SPEECHEDL_ON_NON_MIXEDMODE
//
// MessageText:
//
//  You must specify a mixed voice and audio mode in order to use an optimization definition file.%0
//
  NS_E_SPEECHEDL_ON_NON_MIXEDMODE  = HRESULT($C00D1B92);
  {$EXTERNALSYM NS_E_SPEECHEDL_ON_NON_MIXEDMODE}

//
// MessageId: NS_E_DRM_PASSWORD_TOO_LONG
//
// MessageText:
//
//  The specified password is too long. Type a password with fewer than 8 characters.%0
//
  NS_E_DRM_PASSWORD_TOO_LONG       = HRESULT($C00D1B93);
  {$EXTERNALSYM NS_E_DRM_PASSWORD_TOO_LONG}

//
// MessageId: NS_E_DEVCONTROL_FAILED_SEEK
//
// MessageText:
//
//  It is not possible to seek to the specified mark-in point.%0
//
  NS_E_DEVCONTROL_FAILED_SEEK      = HRESULT($C00D1B94);
  {$EXTERNALSYM NS_E_DEVCONTROL_FAILED_SEEK}

//
// MessageId: NS_E_INTERLACE_REQUIRE_SAMESIZE
//
// MessageText:
//
//  When you choose to maintain the interlacing in your video, the output video size must match the input video size.%0
//
  NS_E_INTERLACE_REQUIRE_SAMESIZE  = HRESULT($C00D1B95);
  {$EXTERNALSYM NS_E_INTERLACE_REQUIRE_SAMESIZE}

//
// MessageId: NS_E_TOO_MANY_DEVICECONTROL
//
// MessageText:
//
//  Only one device control plug-in can control a device.%0
//
  NS_E_TOO_MANY_DEVICECONTROL      = HRESULT($C00D1B96);
  {$EXTERNALSYM NS_E_TOO_MANY_DEVICECONTROL}

//
// MessageId: NS_E_NO_MULTIPASS_FOR_LIVEDEVICE
//
// MessageText:
//
//  You must also enable storing content to hard disk temporarily in order to use two-pass encoding with the input device.%0
//
  NS_E_NO_MULTIPASS_FOR_LIVEDEVICE = HRESULT($C00D1B97);
  {$EXTERNALSYM NS_E_NO_MULTIPASS_FOR_LIVEDEVICE}

//
// MessageId: NS_E_MISSING_AUDIENCE
//
// MessageText:
//
//  An audience is missing from the output stream configuration.%0
//
  NS_E_MISSING_AUDIENCE            = HRESULT($C00D1B98);
  {$EXTERNALSYM NS_E_MISSING_AUDIENCE}

//
// MessageId: NS_E_AUDIENCE_CONTENTTYPE_MISMATCH
//
// MessageText:
//
//  All audiences in the output tree must have the same content type.%0
//
  NS_E_AUDIENCE_CONTENTTYPE_MISMATCH = HRESULT($C00D1B99);
  {$EXTERNALSYM NS_E_AUDIENCE_CONTENTTYPE_MISMATCH}

//
// MessageId: NS_E_MISSING_SOURCE_INDEX
//
// MessageText:
//
//  A source index is missing from the output stream configuration.%0
//
  NS_E_MISSING_SOURCE_INDEX        = HRESULT($C00D1B9A);
  {$EXTERNALSYM NS_E_MISSING_SOURCE_INDEX}

//
// MessageId: NS_E_NUM_LANGUAGE_MISMATCH
//
// MessageText:
//
//  The same source index in different audiences should have the same number of languages.%0
//
  NS_E_NUM_LANGUAGE_MISMATCH       = HRESULT($C00D1B9B);
  {$EXTERNALSYM NS_E_NUM_LANGUAGE_MISMATCH}

//
// MessageId: NS_E_LANGUAGE_MISMATCH
//
// MessageText:
//
//  The same source index in different audiences should have the same languages.%0
//
  NS_E_LANGUAGE_MISMATCH           = HRESULT($C00D1B9C);
  {$EXTERNALSYM NS_E_LANGUAGE_MISMATCH}

//
// MessageId: NS_E_VBRMODE_MISMATCH
//
// MessageText:
//
//  The same source index in different audiences should use the same VBR encoding mode.%0
//
  NS_E_VBRMODE_MISMATCH            = HRESULT($C00D1B9D);
  {$EXTERNALSYM NS_E_VBRMODE_MISMATCH}

//
// MessageId: NS_E_INVALID_INPUT_AUDIENCE_INDEX
//
// MessageText:
//
//  The bit rate index specified is not valid.%0
//
  NS_E_INVALID_INPUT_AUDIENCE_INDEX = HRESULT($C00D1B9E);
  {$EXTERNALSYM NS_E_INVALID_INPUT_AUDIENCE_INDEX}

//
// MessageId: NS_E_INVALID_INPUT_LANGUAGE
//
// MessageText:
//
//  The specified language is not valid.%0
//
  NS_E_INVALID_INPUT_LANGUAGE      = HRESULT($C00D1B9F);
  {$EXTERNALSYM NS_E_INVALID_INPUT_LANGUAGE}

//
// MessageId: NS_E_INVALID_INPUT_STREAM
//
// MessageText:
//
//  The specified source type is not valid.%0
//
  NS_E_INVALID_INPUT_STREAM        = HRESULT($C00D1BA0);
  {$EXTERNALSYM NS_E_INVALID_INPUT_STREAM}

//
// MessageId: NS_E_EXPECT_MONO_WAV_INPUT
//
// MessageText:
//
//  The source must be a mono channel .wav file.%0
//
  NS_E_EXPECT_MONO_WAV_INPUT       = HRESULT($C00D1BA1);
  {$EXTERNALSYM NS_E_EXPECT_MONO_WAV_INPUT}

//
// MessageId: NS_E_INPUT_WAVFORMAT_MISMATCH
//
// MessageText:
//
//  All the source .wav files must have the same format.%0
//
  NS_E_INPUT_WAVFORMAT_MISMATCH    = HRESULT($C00D1BA2);
  {$EXTERNALSYM NS_E_INPUT_WAVFORMAT_MISMATCH}

//
// MessageId: NS_E_RECORDQ_DISK_FULL
//
// MessageText:
//
//  The hard disk being used for temporary storage of content has reached the minimum allowed disk space. Create more space on the hard disk and restart encoding.%0
//
  NS_E_RECORDQ_DISK_FULL           = HRESULT($C00D1BA3);
  {$EXTERNALSYM NS_E_RECORDQ_DISK_FULL}

//
// MessageId: NS_E_NO_PAL_INVERSE_TELECINE
//
// MessageText:
//
//  It is not possible to apply the inverse telecine feature to PAL content.%0
//
  NS_E_NO_PAL_INVERSE_TELECINE     = HRESULT($C00D1BA4);
  {$EXTERNALSYM NS_E_NO_PAL_INVERSE_TELECINE}

//
// MessageId: NS_E_ACTIVE_SG_DEVICE_DISCONNECTED
//
// MessageText:
//
//  A capture device in the current active source is no longer available.%0
//
  NS_E_ACTIVE_SG_DEVICE_DISCONNECTED = HRESULT($C00D1BA5);
  {$EXTERNALSYM NS_E_ACTIVE_SG_DEVICE_DISCONNECTED}

//
// MessageId: NS_E_ACTIVE_SG_DEVICE_CONTROL_DISCONNECTED
//
// MessageText:
//
//  A device used in the current active source for device control is no longer available.%0
//
  NS_E_ACTIVE_SG_DEVICE_CONTROL_DISCONNECTED = HRESULT($C00D1BA6);
  {$EXTERNALSYM NS_E_ACTIVE_SG_DEVICE_CONTROL_DISCONNECTED}

//
// MessageId: NS_E_NO_FRAMES_SUBMITTED_TO_ANALYZER
//
// MessageText:
//
//  No frames have been submitted to the analyzer for analysis.%0
//
  NS_E_NO_FRAMES_SUBMITTED_TO_ANALYZER = HRESULT($C00D1BA7);
  {$EXTERNALSYM NS_E_NO_FRAMES_SUBMITTED_TO_ANALYZER}

//
// MessageId: NS_E_INPUT_DOESNOT_SUPPORT_SMPTE
//
// MessageText:
//
//  The source video does not support time codes.%0
//
  NS_E_INPUT_DOESNOT_SUPPORT_SMPTE = HRESULT($C00D1BA8);
  {$EXTERNALSYM NS_E_INPUT_DOESNOT_SUPPORT_SMPTE}

//
// MessageId: NS_E_NO_SMPTE_WITH_MULTIPLE_SOURCEGROUPS
//
// MessageText:
//
//  It is not possible to generate a time code when there are multiple sources in a session.%0
//
  NS_E_NO_SMPTE_WITH_MULTIPLE_SOURCEGROUPS = HRESULT($C00D1BA9);
  {$EXTERNALSYM NS_E_NO_SMPTE_WITH_MULTIPLE_SOURCEGROUPS}

//
// MessageId: NS_E_BAD_CONTENTEDL
//
// MessageText:
//
//  The voice codec optimization definition file can not be found or is corrupted.%0
//
  NS_E_BAD_CONTENTEDL              = HRESULT($C00D1BAA);
  {$EXTERNALSYM NS_E_BAD_CONTENTEDL}

//
// MessageId: NS_E_INTERLACEMODE_MISMATCH
//
// MessageText:
//
//  The same source index in different audiences should have the same interlace mode.%0
//
  NS_E_INTERLACEMODE_MISMATCH      = HRESULT($C00D1BAB);
  {$EXTERNALSYM NS_E_INTERLACEMODE_MISMATCH}

//
// MessageId: NS_E_NONSQUAREPIXELMODE_MISMATCH
//
// MessageText:
//
//  The same source index in different audiences should have the same nonsquare pixel mode.%0
//
  NS_E_NONSQUAREPIXELMODE_MISMATCH = HRESULT($C00D1BAC);
  {$EXTERNALSYM NS_E_NONSQUAREPIXELMODE_MISMATCH}

//
// MessageId: NS_E_SMPTEMODE_MISMATCH
//
// MessageText:
//
//  The same source index in different audiences should have the same time code mode.%0
//
  NS_E_SMPTEMODE_MISMATCH          = HRESULT($C00D1BAD);
  {$EXTERNALSYM NS_E_SMPTEMODE_MISMATCH}

//
// MessageId: NS_E_END_OF_TAPE
//
// MessageText:
//
//  Either the end of the tape has been reached or there is no tape. Check the device and tape.%0
//
  NS_E_END_OF_TAPE                 = HRESULT($C00D1BAE);
  {$EXTERNALSYM NS_E_END_OF_TAPE}

//
// MessageId: NS_E_NO_MEDIA_IN_AUDIENCE
//
// MessageText:
//
//  No audio or video input has been specified.%0
//
  NS_E_NO_MEDIA_IN_AUDIENCE        = HRESULT($C00D1BAF);
  {$EXTERNALSYM NS_E_NO_MEDIA_IN_AUDIENCE}

//
// MessageId: NS_E_NO_AUDIENCES
//
// MessageText:
//
//  The profile must contain a bit rate.%0
//
  NS_E_NO_AUDIENCES                = HRESULT($C00D1BB0);
  {$EXTERNALSYM NS_E_NO_AUDIENCES}

//
// MessageId: NS_E_NO_AUDIO_COMPAT
//
// MessageText:
//
//  You must specify at least one audio stream to be compatible with Windows Media Player 7.1.%0
//
  NS_E_NO_AUDIO_COMPAT             = HRESULT($C00D1BB1);
  {$EXTERNALSYM NS_E_NO_AUDIO_COMPAT}

//
// MessageId: NS_E_INVALID_VBR_COMPAT
//
// MessageText:
//
//  Using a VBR encoding mode is not compatible with Windows Media Player 7.1.%0
//
  NS_E_INVALID_VBR_COMPAT          = HRESULT($C00D1BB2);
  {$EXTERNALSYM NS_E_INVALID_VBR_COMPAT}

//
// MessageId: NS_E_NO_PROFILE_NAME
//
// MessageText:
//
//  You must specify a profile name.%0
//
  NS_E_NO_PROFILE_NAME             = HRESULT($C00D1BB3);
  {$EXTERNALSYM NS_E_NO_PROFILE_NAME}

//
// MessageId: NS_E_INVALID_VBR_WITH_UNCOMP
//
// MessageText:
//
//  It is not possible to use a VBR encoding mode with uncompressed audio or video.%0
//
  NS_E_INVALID_VBR_WITH_UNCOMP     = HRESULT($C00D1BB4);
  {$EXTERNALSYM NS_E_INVALID_VBR_WITH_UNCOMP}

//
// MessageId: NS_E_MULTIPLE_VBR_AUDIENCES
//
// MessageText:
//
//  It is not possible to use MBR encoding with VBR encoding.%0
//
  NS_E_MULTIPLE_VBR_AUDIENCES      = HRESULT($C00D1BB5);
  {$EXTERNALSYM NS_E_MULTIPLE_VBR_AUDIENCES}

//
// MessageId: NS_E_UNCOMP_COMP_COMBINATION
//
// MessageText:
//
//  It is not possible to mix uncompressed and compressed content in a session.%0
//
  NS_E_UNCOMP_COMP_COMBINATION     = HRESULT($C00D1BB6);
  {$EXTERNALSYM NS_E_UNCOMP_COMP_COMBINATION}

//
// MessageId: NS_E_MULTIPLE_AUDIO_CODECS
//
// MessageText:
//
//  All audiences must use the same audio codec.%0
//
  NS_E_MULTIPLE_AUDIO_CODECS       = HRESULT($C00D1BB7);
  {$EXTERNALSYM NS_E_MULTIPLE_AUDIO_CODECS}

//
// MessageId: NS_E_MULTIPLE_AUDIO_FORMATS
//
// MessageText:
//
//  All audiences should use the same audio format to be compatible with Windows Media Player 7.1.%0
//
  NS_E_MULTIPLE_AUDIO_FORMATS      = HRESULT($C00D1BB8);
  {$EXTERNALSYM NS_E_MULTIPLE_AUDIO_FORMATS}

//
// MessageId: NS_E_AUDIO_BITRATE_STEPDOWN
//
// MessageText:
//
//  The audio bit rate for an audience with a higher total bit rate must be greater than one with a lower total bit rate.%0
//
  NS_E_AUDIO_BITRATE_STEPDOWN      = HRESULT($C00D1BB9);
  {$EXTERNALSYM NS_E_AUDIO_BITRATE_STEPDOWN}

//
// MessageId: NS_E_INVALID_AUDIO_PEAKRATE
//
// MessageText:
//
//  The audio peak bit rate setting is not valid.%0
//
  NS_E_INVALID_AUDIO_PEAKRATE      = HRESULT($C00D1BBA);
  {$EXTERNALSYM NS_E_INVALID_AUDIO_PEAKRATE}

//
// MessageId: NS_E_INVALID_AUDIO_PEAKRATE_2
//
// MessageText:
//
//  The audio peak bit rate setting must be greater than the audio bit rate setting.%0
//
  NS_E_INVALID_AUDIO_PEAKRATE_2    = HRESULT($C00D1BBB);
  {$EXTERNALSYM NS_E_INVALID_AUDIO_PEAKRATE_2}

//
// MessageId: NS_E_INVALID_AUDIO_BUFFERMAX
//
// MessageText:
//
//  The setting for the maximum buffer size for audio is not valid.%0
//
  NS_E_INVALID_AUDIO_BUFFERMAX     = HRESULT($C00D1BBC);
  {$EXTERNALSYM NS_E_INVALID_AUDIO_BUFFERMAX}

//
// MessageId: NS_E_MULTIPLE_VIDEO_CODECS
//
// MessageText:
//
//  All audiences must use the same video codec.%0
//
  NS_E_MULTIPLE_VIDEO_CODECS       = HRESULT($C00D1BBD);
  {$EXTERNALSYM NS_E_MULTIPLE_VIDEO_CODECS}

//
// MessageId: NS_E_MULTIPLE_VIDEO_SIZES
//
// MessageText:
//
//  All audiences should use the same video size to be compatible with Windows Media Player 7.1.%0
//
  NS_E_MULTIPLE_VIDEO_SIZES        = HRESULT($C00D1BBE);
  {$EXTERNALSYM NS_E_MULTIPLE_VIDEO_SIZES}

//
// MessageId: NS_E_INVALID_VIDEO_BITRATE
//
// MessageText:
//
//  The video bit rate setting is not valid.%0
//
  NS_E_INVALID_VIDEO_BITRATE       = HRESULT($C00D1BBF);
  {$EXTERNALSYM NS_E_INVALID_VIDEO_BITRATE}

//
// MessageId: NS_E_VIDEO_BITRATE_STEPDOWN
//
// MessageText:
//
//  The video bit rate for an audience with a higher total bit rate must be greater than one with a lower total bit rate.%0
//
  NS_E_VIDEO_BITRATE_STEPDOWN      = HRESULT($C00D1BC0);
  {$EXTERNALSYM NS_E_VIDEO_BITRATE_STEPDOWN}

//
// MessageId: NS_E_INVALID_VIDEO_PEAKRATE
//
// MessageText:
//
//  The video peak bit rate setting is not valid.%0
//
  NS_E_INVALID_VIDEO_PEAKRATE      = HRESULT($C00D1BC1);
  {$EXTERNALSYM NS_E_INVALID_VIDEO_PEAKRATE}

//
// MessageId: NS_E_INVALID_VIDEO_PEAKRATE_2
//
// MessageText:
//
//  The video peak bit rate setting must be greater than the video bit rate setting.%0
//
  NS_E_INVALID_VIDEO_PEAKRATE_2    = HRESULT($C00D1BC2);
  {$EXTERNALSYM NS_E_INVALID_VIDEO_PEAKRATE_2}

//
// MessageId: NS_E_INVALID_VIDEO_WIDTH
//
// MessageText:
//
//  The video width setting is not valid.%0
//
  NS_E_INVALID_VIDEO_WIDTH         = HRESULT($C00D1BC3);
  {$EXTERNALSYM NS_E_INVALID_VIDEO_WIDTH}

//
// MessageId: NS_E_INVALID_VIDEO_HEIGHT
//
// MessageText:
//
//  The video height setting is not valid.%0
//
  NS_E_INVALID_VIDEO_HEIGHT        = HRESULT($C00D1BC4);
  {$EXTERNALSYM NS_E_INVALID_VIDEO_HEIGHT}

//
// MessageId: NS_E_INVALID_VIDEO_FPS
//
// MessageText:
//
//  The video frame rate setting is not valid.%0
//
  NS_E_INVALID_VIDEO_FPS           = HRESULT($C00D1BC5);
  {$EXTERNALSYM NS_E_INVALID_VIDEO_FPS}

//
// MessageId: NS_E_INVALID_VIDEO_KEYFRAME
//
// MessageText:
//
//  The video key frame setting is not valid.%0
//
  NS_E_INVALID_VIDEO_KEYFRAME      = HRESULT($C00D1BC6);
  {$EXTERNALSYM NS_E_INVALID_VIDEO_KEYFRAME}

//
// MessageId: NS_E_INVALID_VIDEO_IQUALITY
//
// MessageText:
//
//  The video image quality setting is not valid.%0
//
  NS_E_INVALID_VIDEO_IQUALITY      = HRESULT($C00D1BC7);
  {$EXTERNALSYM NS_E_INVALID_VIDEO_IQUALITY}

//
// MessageId: NS_E_INVALID_VIDEO_CQUALITY
//
// MessageText:
//
//  The video codec quality setting is not valid.%0
//
  NS_E_INVALID_VIDEO_CQUALITY      = HRESULT($C00D1BC8);
  {$EXTERNALSYM NS_E_INVALID_VIDEO_CQUALITY}

//
// MessageId: NS_E_INVALID_VIDEO_BUFFER
//
// MessageText:
//
//  The video buffer setting is not valid.%0
//
  NS_E_INVALID_VIDEO_BUFFER        = HRESULT($C00D1BC9);
  {$EXTERNALSYM NS_E_INVALID_VIDEO_BUFFER}

//
// MessageId: NS_E_INVALID_VIDEO_BUFFERMAX
//
// MessageText:
//
//  The setting for the maximum buffer size for video is not valid.%0
//
  NS_E_INVALID_VIDEO_BUFFERMAX     = HRESULT($C00D1BCA);
  {$EXTERNALSYM NS_E_INVALID_VIDEO_BUFFERMAX}

//
// MessageId: NS_E_INVALID_VIDEO_BUFFERMAX_2
//
// MessageText:
//
//  The value of the video maximum buffer size setting must be greater than the video buffer size setting.%0
//
  NS_E_INVALID_VIDEO_BUFFERMAX_2   = HRESULT($C00D1BCB);
  {$EXTERNALSYM NS_E_INVALID_VIDEO_BUFFERMAX_2}

//
// MessageId: NS_E_INVALID_VIDEO_WIDTH_ALIGN
//
// MessageText:
//
//  The alignment of the video width is not valid.%0
//
  NS_E_INVALID_VIDEO_WIDTH_ALIGN   = HRESULT($C00D1BCC);
  {$EXTERNALSYM NS_E_INVALID_VIDEO_WIDTH_ALIGN}

//
// MessageId: NS_E_INVALID_VIDEO_HEIGHT_ALIGN
//
// MessageText:
//
//  The alignment of the video height is not valid.%0
//
  NS_E_INVALID_VIDEO_HEIGHT_ALIGN  = HRESULT($C00D1BCD);
  {$EXTERNALSYM NS_E_INVALID_VIDEO_HEIGHT_ALIGN}

//
// MessageId: NS_E_MULTIPLE_SCRIPT_BITRATES
//
// MessageText:
//
//  All bit rates must have the same script bit rate.%0
//
  NS_E_MULTIPLE_SCRIPT_BITRATES    = HRESULT($C00D1BCE);
  {$EXTERNALSYM NS_E_MULTIPLE_SCRIPT_BITRATES}

//
// MessageId: NS_E_INVALID_SCRIPT_BITRATE
//
// MessageText:
//
//  The script bit rate specified is not valid.%0
//
  NS_E_INVALID_SCRIPT_BITRATE      = HRESULT($C00D1BCF);
  {$EXTERNALSYM NS_E_INVALID_SCRIPT_BITRATE}

//
// MessageId: NS_E_MULTIPLE_FILE_BITRATES
//
// MessageText:
//
//  All bit rates must have the same file transfer bit rate.%0
//
  NS_E_MULTIPLE_FILE_BITRATES      = HRESULT($C00D1BD0);
  {$EXTERNALSYM NS_E_MULTIPLE_FILE_BITRATES}

//
// MessageId: NS_E_INVALID_FILE_BITRATE
//
// MessageText:
//
//  The file transfer bit rate is not valid.%0
//
  NS_E_INVALID_FILE_BITRATE        = HRESULT($C00D1BD1);
  {$EXTERNALSYM NS_E_INVALID_FILE_BITRATE}

//
// MessageId: NS_E_SAME_AS_INPUT_COMBINATION
//
// MessageText:
//
//  All audiences in a profile should either be same as input or have video width and height specified.%0
//
  NS_E_SAME_AS_INPUT_COMBINATION   = HRESULT($C00D1BD2);
  {$EXTERNALSYM NS_E_SAME_AS_INPUT_COMBINATION}

//
// MessageId: NS_E_SOURCE_CANNOT_LOOP
//
// MessageText:
//
//  This source type does not support looping.%0
//
  NS_E_SOURCE_CANNOT_LOOP          = HRESULT($C00D1BD3);
  {$EXTERNALSYM NS_E_SOURCE_CANNOT_LOOP}

//
// MessageId: NS_E_INVALID_FOLDDOWN_COEFFICIENTS
//
// MessageText:
//
//  The fold-down value needs to be between -144 and 0.%0
//
  NS_E_INVALID_FOLDDOWN_COEFFICIENTS = HRESULT($C00D1BD4);
  {$EXTERNALSYM NS_E_INVALID_FOLDDOWN_COEFFICIENTS}

//
// MessageId: NS_E_DRMPROFILE_NOTFOUND
//
// MessageText:
//
//  The specified DRM profile does not exist in the system.%0
//
  NS_E_DRMPROFILE_NOTFOUND         = HRESULT($C00D1BD5);
  {$EXTERNALSYM NS_E_DRMPROFILE_NOTFOUND}

//
// MessageId: NS_E_INVALID_TIMECODE
//
// MessageText:
//
//  The specified time code is not valid.%0
//
  NS_E_INVALID_TIMECODE            = HRESULT($C00D1BD6);
  {$EXTERNALSYM NS_E_INVALID_TIMECODE}

//
// MessageId: NS_E_NO_AUDIO_TIMECOMPRESSION
//
// MessageText:
//
//  It is not possible to apply time compression to a video-only session.%0
//
  NS_E_NO_AUDIO_TIMECOMPRESSION    = HRESULT($C00D1BD7);
  {$EXTERNALSYM NS_E_NO_AUDIO_TIMECOMPRESSION}

//
// MessageId: NS_E_NO_TWOPASS_TIMECOMPRESSION
//
// MessageText:
//
//  It is not possible to apply time compression to a session that is using two-pass encoding.%0
//
  NS_E_NO_TWOPASS_TIMECOMPRESSION  = HRESULT($C00D1BD8);
  {$EXTERNALSYM NS_E_NO_TWOPASS_TIMECOMPRESSION}

//
// MessageId: NS_E_TIMECODE_REQUIRES_VIDEOSTREAM
//
// MessageText:
//
//  It is not possible to generate a time code for an audio-only session.%0
//
  NS_E_TIMECODE_REQUIRES_VIDEOSTREAM = HRESULT($C00D1BD9);
  {$EXTERNALSYM NS_E_TIMECODE_REQUIRES_VIDEOSTREAM}

//
// MessageId: NS_E_NO_MBR_WITH_TIMECODE
//
// MessageText:
//
//  It is not possible to generate a time code when you are encoding content at multiple bit rates.%0
//
  NS_E_NO_MBR_WITH_TIMECODE        = HRESULT($C00D1BDA);
  {$EXTERNALSYM NS_E_NO_MBR_WITH_TIMECODE}

//
// MessageId: NS_E_INVALID_INTERLACEMODE
//
// MessageText:
//
//  The video codec selected does not support maintaining interlacing in video.%0
//
  NS_E_INVALID_INTERLACEMODE       = HRESULT($C00D1BDB);
  {$EXTERNALSYM NS_E_INVALID_INTERLACEMODE}

//
// MessageId: NS_E_INVALID_INTERLACE_COMPAT
//
// MessageText:
//
//  Maintaining interlacing in video is not compatible with Windows Media Player 7.1.%0
//
  NS_E_INVALID_INTERLACE_COMPAT    = HRESULT($C00D1BDC);
  {$EXTERNALSYM NS_E_INVALID_INTERLACE_COMPAT}

//
// MessageId: NS_E_INVALID_NONSQUAREPIXEL_COMPAT
//
// MessageText:
//
//  Allowing nonsquare pixel output is not compatible with Windows Media Player 7.1.%0
//
  NS_E_INVALID_NONSQUAREPIXEL_COMPAT = HRESULT($C00D1BDD);
  {$EXTERNALSYM NS_E_INVALID_NONSQUAREPIXEL_COMPAT}

//
// MessageId: NS_E_INVALID_SOURCE_WITH_DEVICE_CONTROL
//
// MessageText:
//
//  Only capture devices can be used with device control.%0
//
  NS_E_INVALID_SOURCE_WITH_DEVICE_CONTROL = HRESULT($C00D1BDE);
  {$EXTERNALSYM NS_E_INVALID_SOURCE_WITH_DEVICE_CONTROL}

//
// MessageId: NS_E_CANNOT_GENERATE_BROADCAST_INFO_FOR_QUALITYVBR
//
// MessageText:
//
//  It is not possible to generate the stream format file if you are using
//  quality-based VBR encoding for the audio or video stream. Instead use the
//  Windows Media file generated after
//  encoding to create the announcement file.%0
//
  NS_E_CANNOT_GENERATE_BROADCAST_INFO_FOR_QUALITYVBR = HRESULT($C00D1BDF);
  {$EXTERNALSYM NS_E_CANNOT_GENERATE_BROADCAST_INFO_FOR_QUALITYVBR}

//
// MessageId: NS_E_EXCEED_MAX_DRM_PROFILE_LIMIT
//
// MessageText:
//
//  It is not possible to create a DRM profile because the maximum number of profiles has been reached. You must delete some DRM profiles before creating new ones.%0
//
  NS_E_EXCEED_MAX_DRM_PROFILE_LIMIT = HRESULT($C00D1BE0);
  {$EXTERNALSYM NS_E_EXCEED_MAX_DRM_PROFILE_LIMIT}

//
// MessageId: NS_E_DEVICECONTROL_UNSTABLE
//
// MessageText:
//
//  The device is in an unstable state. Check that the device is functioning properly and a tape is in place.
//
  NS_E_DEVICECONTROL_UNSTABLE      = HRESULT($C00D1BE1);
  {$EXTERNALSYM NS_E_DEVICECONTROL_UNSTABLE}

//
// MessageId: NS_E_INVALID_PIXEL_ASPECT_RATIO
//
// MessageText:
//
//  The pixel aspect ratio value must be between 1 and 255.
//
  NS_E_INVALID_PIXEL_ASPECT_RATIO  = HRESULT($C00D1BE2);
  {$EXTERNALSYM NS_E_INVALID_PIXEL_ASPECT_RATIO}

//
// MessageId: NS_E_AUDIENCE__LANGUAGE_CONTENTTYPE_MISMATCH
//
// MessageText:
//
//  All streams with different languages in the same audience must have same properties.%0
//
  NS_E_AUDIENCE__LANGUAGE_CONTENTTYPE_MISMATCH = HRESULT($C00D1BE3);
  {$EXTERNALSYM NS_E_AUDIENCE__LANGUAGE_CONTENTTYPE_MISMATCH}

//
// MessageId: NS_E_INVALID_PROFILE_CONTENTTYPE
//
// MessageText:
//
//  The profile must contain at least one audio or video stream.%0
//
  NS_E_INVALID_PROFILE_CONTENTTYPE = HRESULT($C00D1BE4);
  {$EXTERNALSYM NS_E_INVALID_PROFILE_CONTENTTYPE}

//
// MessageId: NS_E_TRANSFORM_PLUGIN_NOT_FOUND
//
// MessageText:
//
//  The transform plug-in could not be found.%0
//
  NS_E_TRANSFORM_PLUGIN_NOT_FOUND  = HRESULT($C00D1BE5);
  {$EXTERNALSYM NS_E_TRANSFORM_PLUGIN_NOT_FOUND}

//
// MessageId: NS_E_TRANSFORM_PLUGIN_INVALID
//
// MessageText:
//
//  The transform plug-in is not valid. It may be damaged or you may not have the required permissions to access the plug-in.%0
//
  NS_E_TRANSFORM_PLUGIN_INVALID    = HRESULT($C00D1BE6);
  {$EXTERNALSYM NS_E_TRANSFORM_PLUGIN_INVALID}

//
// MessageId: NS_E_EDL_REQUIRED_FOR_DEVICE_MULTIPASS
//
// MessageText:
//
//  To use two-pass encoding, you must enable device control and setup an edit decision list (ED); that has at least one entry.%0
//
  NS_E_EDL_REQUIRED_FOR_DEVICE_MULTIPASS = HRESULT($C00D1BE7);
  {$EXTERNALSYM NS_E_EDL_REQUIRED_FOR_DEVICE_MULTIPASS}

//
// MessageId: NS_E_INVALID_VIDEO_WIDTH_FOR_INTERLACED_ENCODING
//
// MessageText:
//
//  When you choose to maintain the interlacing in your video, the output video size must be a multiple of 4.%0
//
  NS_E_INVALID_VIDEO_WIDTH_FOR_INTERLACED_ENCODING = HRESULT($C00D1BE8);
  {$EXTERNALSYM NS_E_INVALID_VIDEO_WIDTH_FOR_INTERLACED_ENCODING}


/////////////////////////////////////////////////////////////////////////
//
// DRM Specific Errors
//
// IdRange = 10000..10999
/////////////////////////////////////////////////////////////////////////
//
// MessageId: NS_E_DRM_INVALID_APPLICATION
//
// MessageText:
//
//  A problem has occurred in the Digital Rights Management component. Contact product support for this application.%0
//
  NS_E_DRM_INVALID_APPLICATION     = HRESULT($C00D2711);
  {$EXTERNALSYM NS_E_DRM_INVALID_APPLICATION}

//
// MessageId: NS_E_DRM_LICENSE_STORE_ERROR
//
// MessageText:
//
//  License storage is not working. Contact Microsoft product support.%0
//
  NS_E_DRM_LICENSE_STORE_ERROR     = HRESULT($C00D2712);
  {$EXTERNALSYM NS_E_DRM_LICENSE_STORE_ERROR}

//
// MessageId: NS_E_DRM_SECURE_STORE_ERROR
//
// MessageText:
//
//  Secure storage is not working. Contact Microsoft product support.%0
//
  NS_E_DRM_SECURE_STORE_ERROR      = HRESULT($C00D2713);
  {$EXTERNALSYM NS_E_DRM_SECURE_STORE_ERROR}

//
// MessageId: NS_E_DRM_LICENSE_STORE_SAVE_ERROR
//
// MessageText:
//
//  License acquisition did not work. Acquire a new license or contact the content provider for further assistance.%0
//
  NS_E_DRM_LICENSE_STORE_SAVE_ERROR = HRESULT($C00D2714);
  {$EXTERNALSYM NS_E_DRM_LICENSE_STORE_SAVE_ERROR}

//
// MessageId: NS_E_DRM_SECURE_STORE_UNLOCK_ERROR
//
// MessageText:
//
//  A problem has occurred in the Digital Rights Management component. Contact Microsoft product support.%0
//
  NS_E_DRM_SECURE_STORE_UNLOCK_ERROR = HRESULT($C00D2715);
  {$EXTERNALSYM NS_E_DRM_SECURE_STORE_UNLOCK_ERROR}

//
// MessageId: NS_E_DRM_INVALID_CONTENT
//
// MessageText:
//
//  The media file is corrupted. Contact the content provider to get a new file.%0
//
  NS_E_DRM_INVALID_CONTENT         = HRESULT($C00D2716);
  {$EXTERNALSYM NS_E_DRM_INVALID_CONTENT}

//
// MessageId: NS_E_DRM_UNABLE_TO_OPEN_LICENSE
//
// MessageText:
//
//  The license is corrupted. Acquire a new license.%0
//
  NS_E_DRM_UNABLE_TO_OPEN_LICENSE  = HRESULT($C00D2717);
  {$EXTERNALSYM NS_E_DRM_UNABLE_TO_OPEN_LICENSE}

//
// MessageId: NS_E_DRM_INVALID_LICENSE
//
// MessageText:
//
//  The license is corrupted or invalid. Acquire a new license%0
//
  NS_E_DRM_INVALID_LICENSE         = HRESULT($C00D2718);
  {$EXTERNALSYM NS_E_DRM_INVALID_LICENSE}

//
// MessageId: NS_E_DRM_INVALID_MACHINE
//
// MessageText:
//
//  Licenses cannot be copied from one computer to another. Use License Management to transfer licenses, or get a new license for the media file.%0
//
  NS_E_DRM_INVALID_MACHINE         = HRESULT($C00D2719);
  {$EXTERNALSYM NS_E_DRM_INVALID_MACHINE}

//
// MessageId: NS_E_DRM_ENUM_LICENSE_FAILED
//
// MessageText:
//
//  License storage is not working. Contact Microsoft product support.%0
//
  NS_E_DRM_ENUM_LICENSE_FAILED     = HRESULT($C00D271B);
  {$EXTERNALSYM NS_E_DRM_ENUM_LICENSE_FAILED}

//
// MessageId: NS_E_DRM_INVALID_LICENSE_REQUEST
//
// MessageText:
//
//  The media file is corrupted. Contact the content provider to get a new file.%0
//
  NS_E_DRM_INVALID_LICENSE_REQUEST = HRESULT($C00D271C);
  {$EXTERNALSYM NS_E_DRM_INVALID_LICENSE_REQUEST}

//
// MessageId: NS_E_DRM_UNABLE_TO_INITIALIZE
//
// MessageText:
//
//  A problem has occurred in the Digital Rights Management component. Contact Microsoft product support.%0
//
  NS_E_DRM_UNABLE_TO_INITIALIZE    = HRESULT($C00D271D);
  {$EXTERNALSYM NS_E_DRM_UNABLE_TO_INITIALIZE}

//
// MessageId: NS_E_DRM_UNABLE_TO_ACQUIRE_LICENSE
//
// MessageText:
//
//  The license could not be acquired. Try again later.%0
//
  NS_E_DRM_UNABLE_TO_ACQUIRE_LICENSE = HRESULT($C00D271E);
  {$EXTERNALSYM NS_E_DRM_UNABLE_TO_ACQUIRE_LICENSE}

//
// MessageId: NS_E_DRM_INVALID_LICENSE_ACQUIRED
//
// MessageText:
//
//  License acquisition did not work. Acquire a new license or contact the content provider for further assistance.%0
//
  NS_E_DRM_INVALID_LICENSE_ACQUIRED = HRESULT($C00D271F);
  {$EXTERNALSYM NS_E_DRM_INVALID_LICENSE_ACQUIRED}

//
// MessageId: NS_E_DRM_NO_RIGHTS
//
// MessageText:
//
//  The requested operation cannot be performed on this file.%0
//
  NS_E_DRM_NO_RIGHTS               = HRESULT($C00D2720);
  {$EXTERNALSYM NS_E_DRM_NO_RIGHTS}

//
// MessageId: NS_E_DRM_KEY_ERROR
//
// MessageText:
//
//  A problem has occurred in the Digital Rights Management component. Contact Microsoft product support.%0.
//
  NS_E_DRM_KEY_ERROR               = HRESULT($C00D2721);
  {$EXTERNALSYM NS_E_DRM_KEY_ERROR}

//
// MessageId: NS_E_DRM_ENCRYPT_ERROR
//
// MessageText:
//
//  A problem has occurred in the Digital Rights Management component. Contact Microsoft product support.%0
//
  NS_E_DRM_ENCRYPT_ERROR           = HRESULT($C00D2722);
  {$EXTERNALSYM NS_E_DRM_ENCRYPT_ERROR}

//
// MessageId: NS_E_DRM_DECRYPT_ERROR
//
// MessageText:
//
//  The media file is corrupted. Contact the content provider to get a new file.%0
//
  NS_E_DRM_DECRYPT_ERROR           = HRESULT($C00D2723);
  {$EXTERNALSYM NS_E_DRM_DECRYPT_ERROR}

//
// MessageId: NS_E_DRM_LICENSE_INVALID_XML
//
// MessageText:
//
//  The license is corrupted. Acquire a new license.%0
//
  NS_E_DRM_LICENSE_INVALID_XML     = HRESULT($C00D2725);
  {$EXTERNALSYM NS_E_DRM_LICENSE_INVALID_XML}

//
// MessageId: NS_S_DRM_LICENSE_ACQUIRED
//
// MessageText:
//
//  Status message: The license was acquired.%0
//
  NS_S_DRM_LICENSE_ACQUIRED        = HRESULT($000D2726);
  {$EXTERNALSYM NS_S_DRM_LICENSE_ACQUIRED}

//
// MessageId: NS_S_DRM_INDIVIDUALIZED
//
// MessageText:
//
//  Status message: The security upgrade has been completed.%0
//
  NS_S_DRM_INDIVIDUALIZED          = HRESULT($000D2727);
  {$EXTERNALSYM NS_S_DRM_INDIVIDUALIZED}

//
// MessageId: NS_E_DRM_NEEDS_INDIVIDUALIZATION
//
// MessageText:
//
//  A security upgrade is required to perform the operation on this media file.%0
//
  NS_E_DRM_NEEDS_INDIVIDUALIZATION = HRESULT($C00D2728);
  {$EXTERNALSYM NS_E_DRM_NEEDS_INDIVIDUALIZATION}

//
// MessageId: NS_E_DRM_ALREADY_INDIVIDUALIZED
//
// MessageText:
//
//  You already have the latest security components. No upgrade is necessary at this time.%0
//
  NS_E_DRM_ALREADY_INDIVIDUALIZED  = HRESULT($C00D2729);
  {$EXTERNALSYM NS_E_DRM_ALREADY_INDIVIDUALIZED}

//
// MessageId: NS_E_DRM_ACTION_NOT_QUERIED
//
// MessageText:
//
//  The application cannot perform this action. Contact product support for this application.%0
//
  NS_E_DRM_ACTION_NOT_QUERIED      = HRESULT($C00D272A);
  {$EXTERNALSYM NS_E_DRM_ACTION_NOT_QUERIED}

//
// MessageId: NS_E_DRM_ACQUIRING_LICENSE
//
// MessageText:
//
//  You cannot begin a new license acquisition process until the current one has been completed.%0
//
  NS_E_DRM_ACQUIRING_LICENSE       = HRESULT($C00D272B);
  {$EXTERNALSYM NS_E_DRM_ACQUIRING_LICENSE}

//
// MessageId: NS_E_DRM_INDIVIDUALIZING
//
// MessageText:
//
//  You cannot begin a new security upgrade until the current one has been completed.%0
//
  NS_E_DRM_INDIVIDUALIZING         = HRESULT($C00D272C);
  {$EXTERNALSYM NS_E_DRM_INDIVIDUALIZING}

//
// MessageId: NS_E_DRM_PARAMETERS_MISMATCHED
//
// MessageText:
//
//  A problem has occurred in the Digital Rights Management component. Contact Microsoft product support.%0.
//
  NS_E_DRM_PARAMETERS_MISMATCHED   = HRESULT($C00D272F);
  {$EXTERNALSYM NS_E_DRM_PARAMETERS_MISMATCHED}

//
// MessageId: NS_E_DRM_UNABLE_TO_CREATE_LICENSE_OBJECT
//
// MessageText:
//
//  A license cannot be created for this media file. Reinstall the application.%0
//
  NS_E_DRM_UNABLE_TO_CREATE_LICENSE_OBJECT = HRESULT($C00D2730);
  {$EXTERNALSYM NS_E_DRM_UNABLE_TO_CREATE_LICENSE_OBJECT}

//
// MessageId: NS_E_DRM_UNABLE_TO_CREATE_INDI_OBJECT
//
// MessageText:
//
//  A problem has occurred in the Digital Rights Management component. Contact Microsoft product support.%0.
//
  NS_E_DRM_UNABLE_TO_CREATE_INDI_OBJECT = HRESULT($C00D2731);
  {$EXTERNALSYM NS_E_DRM_UNABLE_TO_CREATE_INDI_OBJECT}

//
// MessageId: NS_E_DRM_UNABLE_TO_CREATE_ENCRYPT_OBJECT
//
// MessageText:
//
//  A problem has occurred in the Digital Rights Management component. Contact Microsoft product support.%0.
//
  NS_E_DRM_UNABLE_TO_CREATE_ENCRYPT_OBJECT = HRESULT($C00D2732);
  {$EXTERNALSYM NS_E_DRM_UNABLE_TO_CREATE_ENCRYPT_OBJECT}

//
// MessageId: NS_E_DRM_UNABLE_TO_CREATE_DECRYPT_OBJECT
//
// MessageText:
//
//  A problem has occurred in the Digital Rights Management component. Contact Microsoft product support.%0.
//
  NS_E_DRM_UNABLE_TO_CREATE_DECRYPT_OBJECT = HRESULT($C00D2733);
  {$EXTERNALSYM NS_E_DRM_UNABLE_TO_CREATE_DECRYPT_OBJECT}

//
// MessageId: NS_E_DRM_UNABLE_TO_CREATE_PROPERTIES_OBJECT
//
// MessageText:
//
//  A problem has occurred in the Digital Rights Management component. Contact Microsoft product support.%0.
//
  NS_E_DRM_UNABLE_TO_CREATE_PROPERTIES_OBJECT = HRESULT($C00D2734);
  {$EXTERNALSYM NS_E_DRM_UNABLE_TO_CREATE_PROPERTIES_OBJECT}

//
// MessageId: NS_E_DRM_UNABLE_TO_CREATE_BACKUP_OBJECT
//
// MessageText:
//
//  A problem has occurred in the Digital Rights Management component. Contact Microsoft product support.%0.
//
  NS_E_DRM_UNABLE_TO_CREATE_BACKUP_OBJECT = HRESULT($C00D2735);
  {$EXTERNALSYM NS_E_DRM_UNABLE_TO_CREATE_BACKUP_OBJECT}

//
// MessageId: NS_E_DRM_INDIVIDUALIZE_ERROR
//
// MessageText:
//
//  The security upgrade failed. Try again later.%0
//
  NS_E_DRM_INDIVIDUALIZE_ERROR     = HRESULT($C00D2736);
  {$EXTERNALSYM NS_E_DRM_INDIVIDUALIZE_ERROR}

//
// MessageId: NS_E_DRM_LICENSE_OPEN_ERROR
//
// MessageText:
//
//  License storage is not working. Contact Microsoft product support.%0
//
  NS_E_DRM_LICENSE_OPEN_ERROR      = HRESULT($C00D2737);
  {$EXTERNALSYM NS_E_DRM_LICENSE_OPEN_ERROR}

//
// MessageId: NS_E_DRM_LICENSE_CLOSE_ERROR
//
// MessageText:
//
//  License storage is not working. Contact Microsoft product support.%0
//
  NS_E_DRM_LICENSE_CLOSE_ERROR     = HRESULT($C00D2738);
  {$EXTERNALSYM NS_E_DRM_LICENSE_CLOSE_ERROR}

//
// MessageId: NS_E_DRM_GET_LICENSE_ERROR
//
// MessageText:
//
//  License storage is not working. Contact Microsoft product support.%0
//
  NS_E_DRM_GET_LICENSE_ERROR       = HRESULT($C00D2739);
  {$EXTERNALSYM NS_E_DRM_GET_LICENSE_ERROR}

//
// MessageId: NS_E_DRM_QUERY_ERROR
//
// MessageText:
//
//  A problem has occurred in the Digital Rights Management component. Contact Microsoft product support.%0.
//
  NS_E_DRM_QUERY_ERROR             = HRESULT($C00D273A);
  {$EXTERNALSYM NS_E_DRM_QUERY_ERROR}

//
// MessageId: NS_E_DRM_REPORT_ERROR
//
// MessageText:
//
//  A problem has occurred in the Digital Rights Management component. Contact product support for this application.%0
//
  NS_E_DRM_REPORT_ERROR            = HRESULT($C00D273B);
  {$EXTERNALSYM NS_E_DRM_REPORT_ERROR}

//
// MessageId: NS_E_DRM_GET_LICENSESTRING_ERROR
//
// MessageText:
//
//  License storage is not working. Contact Microsoft product support.%0
//
  NS_E_DRM_GET_LICENSESTRING_ERROR = HRESULT($C00D273C);
  {$EXTERNALSYM NS_E_DRM_GET_LICENSESTRING_ERROR}

//
// MessageId: NS_E_DRM_GET_CONTENTSTRING_ERROR
//
// MessageText:
//
//  The media file is corrupted. Contact the content provider to get a new file.%0
//
  NS_E_DRM_GET_CONTENTSTRING_ERROR = HRESULT($C00D273D);
  {$EXTERNALSYM NS_E_DRM_GET_CONTENTSTRING_ERROR}

//
// MessageId: NS_E_DRM_MONITOR_ERROR
//
// MessageText:
//
//  A problem has occurred in the Digital Rights Management component. Try again later.%0
//
  NS_E_DRM_MONITOR_ERROR           = HRESULT($C00D273E);
  {$EXTERNALSYM NS_E_DRM_MONITOR_ERROR}

//
// MessageId: NS_E_DRM_UNABLE_TO_SET_PARAMETER
//
// MessageText:
//
//  The application has made an invalid call to the Digital Rights Management component. Contact product support for this application.%0
//
  NS_E_DRM_UNABLE_TO_SET_PARAMETER = HRESULT($C00D273F);
  {$EXTERNALSYM NS_E_DRM_UNABLE_TO_SET_PARAMETER}

//
// MessageId: NS_E_DRM_INVALID_APPDATA
//
// MessageText:
//
//  A problem has occurred in the Digital Rights Management component. Contact Microsoft product support.%0.
//
  NS_E_DRM_INVALID_APPDATA         = HRESULT($C00D2740);
  {$EXTERNALSYM NS_E_DRM_INVALID_APPDATA}

//
// MessageId: NS_E_DRM_INVALID_APPDATA_VERSION
//
// MessageText:
//
//  A problem has occurred in the Digital Rights Management component. Contact product support for this application.%0.
//
  NS_E_DRM_INVALID_APPDATA_VERSION = HRESULT($C00D2741);
  {$EXTERNALSYM NS_E_DRM_INVALID_APPDATA_VERSION}

//
// MessageId: NS_E_DRM_BACKUP_EXISTS
//
// MessageText:
//
//  Licenses are already backed up in this location.%0
//
  NS_E_DRM_BACKUP_EXISTS           = HRESULT($C00D2742);
  {$EXTERNALSYM NS_E_DRM_BACKUP_EXISTS}

//
// MessageId: NS_E_DRM_BACKUP_CORRUPT
//
// MessageText:
//
//  One or more backed-up licenses are missing or corrupt.%0
//
  NS_E_DRM_BACKUP_CORRUPT          = HRESULT($C00D2743);
  {$EXTERNALSYM NS_E_DRM_BACKUP_CORRUPT}

//
// MessageId: NS_E_DRM_BACKUPRESTORE_BUSY
//
// MessageText:
//
//  You cannot begin a new backup process until the current process has been completed.%0
//
  NS_E_DRM_BACKUPRESTORE_BUSY      = HRESULT($C00D2744);
  {$EXTERNALSYM NS_E_DRM_BACKUPRESTORE_BUSY}

//
// MessageId: NS_S_DRM_MONITOR_CANCELLED
//
// MessageText:
//
//  Status message: License monitoring has been cancelled.%0
//
  NS_S_DRM_MONITOR_CANCELLED       = HRESULT($000D2746);
  {$EXTERNALSYM NS_S_DRM_MONITOR_CANCELLED}

//
// MessageId: NS_S_DRM_ACQUIRE_CANCELLED
//
// MessageText:
//
//  Status message: License acquisition has been cancelled.%0
//
  NS_S_DRM_ACQUIRE_CANCELLED       = HRESULT($000D2747);
  {$EXTERNALSYM NS_S_DRM_ACQUIRE_CANCELLED}

//
// MessageId: NS_E_DRM_LICENSE_UNUSABLE
//
// MessageText:
//
//  The license is invalid. Contact the content provider for further assistance.%0
//
  NS_E_DRM_LICENSE_UNUSABLE        = HRESULT($C00D2748);
  {$EXTERNALSYM NS_E_DRM_LICENSE_UNUSABLE}

//
// MessageId: NS_E_DRM_INVALID_PROPERTY
//
// MessageText:
//
//  A required property was not set by the application. Contact product support for this application.%0.
//
  NS_E_DRM_INVALID_PROPERTY        = HRESULT($C00D2749);
  {$EXTERNALSYM NS_E_DRM_INVALID_PROPERTY}

//
// MessageId: NS_E_DRM_SECURE_STORE_NOT_FOUND
//
// MessageText:
//
//  A problem has occurred in the Digital Rights Management component of this application. Try to acquire a license again.%0
//
  NS_E_DRM_SECURE_STORE_NOT_FOUND  = HRESULT($C00D274A);
  {$EXTERNALSYM NS_E_DRM_SECURE_STORE_NOT_FOUND}

//
// MessageId: NS_E_DRM_CACHED_CONTENT_ERROR
//
// MessageText:
//
//  A license cannot be found for this media file. Use License Management to transfer a license for this file from the original computer, or acquire a new license.%0
//
  NS_E_DRM_CACHED_CONTENT_ERROR    = HRESULT($C00D274B);
  {$EXTERNALSYM NS_E_DRM_CACHED_CONTENT_ERROR}

//
// MessageId: NS_E_DRM_INDIVIDUALIZATION_INCOMPLETE
//
// MessageText:
//
//  A problem occurred during the security upgrade. Try again later.%0
//
  NS_E_DRM_INDIVIDUALIZATION_INCOMPLETE = HRESULT($C00D274C);
  {$EXTERNALSYM NS_E_DRM_INDIVIDUALIZATION_INCOMPLETE}

//
// MessageId: NS_E_DRM_DRIVER_AUTH_FAILURE
//
// MessageText:
//
//  Certified driver components are required to play this media file. Contact Windows Update to see whether updated drivers are available for your hardware.%0
//
  NS_E_DRM_DRIVER_AUTH_FAILURE     = HRESULT($C00D274D);
  {$EXTERNALSYM NS_E_DRM_DRIVER_AUTH_FAILURE}

//
// MessageId: NS_E_DRM_NEED_UPGRADE_MSSAP
//
// MessageText:
//
//  One or more of the Secure Audio Path components were not found or an entry point in those components was not found.%0
//
  NS_E_DRM_NEED_UPGRADE_MSSAP      = HRESULT($C00D274E);
  {$EXTERNALSYM NS_E_DRM_NEED_UPGRADE_MSSAP}

//
// MessageId: NS_E_DRM_REOPEN_CONTENT
//
// MessageText:
//
//  Status message: Reopen the file.%0
//
  NS_E_DRM_REOPEN_CONTENT          = HRESULT($C00D274F);
  {$EXTERNALSYM NS_E_DRM_REOPEN_CONTENT}

//
// MessageId: NS_E_DRM_DRIVER_DIGIOUT_FAILURE
//
// MessageText:
//
//  Certain driver functionality is required to play this media file. Contact Windows Update to see whether updated drivers are available for your hardware.%0
//
  NS_E_DRM_DRIVER_DIGIOUT_FAILURE  = HRESULT($C00D2750);
  {$EXTERNALSYM NS_E_DRM_DRIVER_DIGIOUT_FAILURE}

//
// MessageId: NS_E_DRM_INVALID_SECURESTORE_PASSWORD
//
// MessageText:
//
//  A problem has occurred in the Digital Rights Management component. Contact Microsoft product support.%0.
//
  NS_E_DRM_INVALID_SECURESTORE_PASSWORD = HRESULT($C00D2751);
  {$EXTERNALSYM NS_E_DRM_INVALID_SECURESTORE_PASSWORD}

//
// MessageId: NS_E_DRM_APPCERT_REVOKED
//
// MessageText:
//
//  A problem has occurred in the Digital Rights Management component. Contact Microsoft product support.%0.
//
  NS_E_DRM_APPCERT_REVOKED         = HRESULT($C00D2752);
  {$EXTERNALSYM NS_E_DRM_APPCERT_REVOKED}

//
// MessageId: NS_E_DRM_RESTORE_FRAUD
//
// MessageText:
//
//  You cannot restore your license(s).%0
//
  NS_E_DRM_RESTORE_FRAUD           = HRESULT($C00D2753);
  {$EXTERNALSYM NS_E_DRM_RESTORE_FRAUD}

//
// MessageId: NS_E_DRM_HARDWARE_INCONSISTENT
//
// MessageText:
//
//  The licenses for your media files are corrupted. Contact Microsoft product support.%0
//
  NS_E_DRM_HARDWARE_INCONSISTENT   = HRESULT($C00D2754);
  {$EXTERNALSYM NS_E_DRM_HARDWARE_INCONSISTENT}

//
// MessageId: NS_E_DRM_SDMI_TRIGGER
//
// MessageText:
//
//  To transfer this media file, you must upgrade the application.%0
//
  NS_E_DRM_SDMI_TRIGGER            = HRESULT($C00D2755);
  {$EXTERNALSYM NS_E_DRM_SDMI_TRIGGER}

//
// MessageId: NS_E_DRM_SDMI_NOMORECOPIES
//
// MessageText:
//
//  You cannot make any more copies of this media file.%0
//
  NS_E_DRM_SDMI_NOMORECOPIES       = HRESULT($C00D2756);
  {$EXTERNALSYM NS_E_DRM_SDMI_NOMORECOPIES}

//
// MessageId: NS_E_DRM_UNABLE_TO_CREATE_HEADER_OBJECT
//
// MessageText:
//
//  A problem has occurred in the Digital Rights Management component. Contact Microsoft product support.%0.
//
  NS_E_DRM_UNABLE_TO_CREATE_HEADER_OBJECT = HRESULT($C00D2757);
  {$EXTERNALSYM NS_E_DRM_UNABLE_TO_CREATE_HEADER_OBJECT}

//
// MessageId: NS_E_DRM_UNABLE_TO_CREATE_KEYS_OBJECT
//
// MessageText:
//
//  A problem has occurred in the Digital Rights Management component. Contact Microsoft product support.%0.
//
  NS_E_DRM_UNABLE_TO_CREATE_KEYS_OBJECT = HRESULT($C00D2758);
  {$EXTERNALSYM NS_E_DRM_UNABLE_TO_CREATE_KEYS_OBJECT}

// This error is never shown to user but needed for program logic.
//
// MessageId: NS_E_DRM_LICENSE_NOTACQUIRED
//
// MessageText:
//
//  Unable to obtain license.%0
//
  NS_E_DRM_LICENSE_NOTACQUIRED     = HRESULT($C00D2759);
  {$EXTERNALSYM NS_E_DRM_LICENSE_NOTACQUIRED}

//
// MessageId: NS_E_DRM_UNABLE_TO_CREATE_CODING_OBJECT
//
// MessageText:
//
//  A problem has occurred in the Digital Rights Management component. Contact Microsoft product support.%0.
//
  NS_E_DRM_UNABLE_TO_CREATE_CODING_OBJECT = HRESULT($C00D275A);
  {$EXTERNALSYM NS_E_DRM_UNABLE_TO_CREATE_CODING_OBJECT}

//
// MessageId: NS_E_DRM_UNABLE_TO_CREATE_STATE_DATA_OBJECT
//
// MessageText:
//
//  A problem has occurred in the Digital Rights Management component. Contact Microsoft product support.%0.
//
  NS_E_DRM_UNABLE_TO_CREATE_STATE_DATA_OBJECT = HRESULT($C00D275B);
  {$EXTERNALSYM NS_E_DRM_UNABLE_TO_CREATE_STATE_DATA_OBJECT}

//
// MessageId: NS_E_DRM_BUFFER_TOO_SMALL
//
// MessageText:
//
//  The buffer supplied is not sufficient.%0.
//
  NS_E_DRM_BUFFER_TOO_SMALL        = HRESULT($C00D275C);
  {$EXTERNALSYM NS_E_DRM_BUFFER_TOO_SMALL}

//
// MessageId: NS_E_DRM_UNSUPPORTED_PROPERTY
//
// MessageText:
//
//  The property requested is not supported.%0.
//
  NS_E_DRM_UNSUPPORTED_PROPERTY    = HRESULT($C00D275D);
  {$EXTERNALSYM NS_E_DRM_UNSUPPORTED_PROPERTY}

//
// MessageId: NS_E_DRM_ERROR_BAD_NET_RESP
//
// MessageText:
//
//  The specified server cannot perform the requested operation.%0.
//
  NS_E_DRM_ERROR_BAD_NET_RESP      = HRESULT($C00D275E);
  {$EXTERNALSYM NS_E_DRM_ERROR_BAD_NET_RESP}

//
// MessageId: NS_E_DRM_STORE_NOTALLSTORED
//
// MessageText:
//
//  Some of the licenses could not be stored.%0.
//
  NS_E_DRM_STORE_NOTALLSTORED      = HRESULT($C00D275F);
  {$EXTERNALSYM NS_E_DRM_STORE_NOTALLSTORED}

//
// MessageId: NS_E_DRM_SECURITY_COMPONENT_SIGNATURE_INVALID
//
// MessageText:
//
//  The Digital Rights Management security upgrade component could not be validated. Contact Microsoft product support.%0
//
  NS_E_DRM_SECURITY_COMPONENT_SIGNATURE_INVALID = HRESULT($C00D2760);
  {$EXTERNALSYM NS_E_DRM_SECURITY_COMPONENT_SIGNATURE_INVALID}

//
// MessageId: NS_E_DRM_INVALID_DATA
//
// MessageText:
//
//  Invalid or corrupt data was encountered.%0
//
  NS_E_DRM_INVALID_DATA            = HRESULT($C00D2761);
  {$EXTERNALSYM NS_E_DRM_INVALID_DATA}

//
// MessageId: NS_E_DRM_UNABLE_TO_CONTACT_SERVER
//
// MessageText:
//
//  Unable to contact the server for the requested operation.%0
//
  NS_E_DRM_UNABLE_TO_CONTACT_SERVER = HRESULT($C00D2762);
  {$EXTERNALSYM NS_E_DRM_UNABLE_TO_CONTACT_SERVER}

//
// MessageId: NS_E_DRM_UNABLE_TO_CREATE_AUTHENTICATION_OBJECT
//
// MessageText:
//
//  A problem has occurred in the Digital Rights Management component. Contact Microsoft product support.%0.
//
  NS_E_DRM_UNABLE_TO_CREATE_AUTHENTICATION_OBJECT = HRESULT($C00D2763);
  {$EXTERNALSYM NS_E_DRM_UNABLE_TO_CREATE_AUTHENTICATION_OBJECT}

//
// MessageId: NS_E_DRM_NOT_CONFIGURED
//
// MessageText:
//
//  Not all of the necessary properties for DRM have been set.%0
//
  NS_E_DRM_NOT_CONFIGURED          = HRESULT($C00D2764);
  {$EXTERNALSYM NS_E_DRM_NOT_CONFIGURED}

//
// MessageId: NS_E_DRM_DEVICE_ACTIVATION_CANCELED
//
// MessageText:
//
//  The portable device does not have the security required to copy protected files
//  to it. To obtain the additional security, try to copy the file to your portable
//  device again. When a message appears, click OK.%0
//
  NS_E_DRM_DEVICE_ACTIVATION_CANCELED = HRESULT($C00D2765);
  {$EXTERNALSYM NS_E_DRM_DEVICE_ACTIVATION_CANCELED}


//
// License Reasons Section
// Error Codes why a license is not usable. Reserve 10200..10300 for this purpose.
// 10200..10249 is for license reported reasons. 10250..10300 is for client detected reasons.
//

//
// MessageId: NS_E_DRM_LICENSE_EXPIRED
//
// MessageText:
//
//  The license for this file has expired and is no longer valid. Contact your content provider for further assistance.%0
//
  NS_E_DRM_LICENSE_EXPIRED         = HRESULT($C00D27D8);
  {$EXTERNALSYM NS_E_DRM_LICENSE_EXPIRED}

//
// MessageId: NS_E_DRM_LICENSE_NOTENABLED
//
// MessageText:
//
//  The license for this file is not valid yet, but will be at a future date.%0
//
  NS_E_DRM_LICENSE_NOTENABLED      = HRESULT($C00D27D9);
  {$EXTERNALSYM NS_E_DRM_LICENSE_NOTENABLED}

//
// MessageId: NS_E_DRM_LICENSE_APPSECLOW
//
// MessageText:
//
//  The license for this file requires a higher level of security than the player
//  you are currently using has. Try using a different player or download a newer
//  version of your current player.%0
//
  NS_E_DRM_LICENSE_APPSECLOW       = HRESULT($C00D27DA);
  {$EXTERNALSYM NS_E_DRM_LICENSE_APPSECLOW}

//
// MessageId: NS_E_DRM_STORE_NEEDINDI
//
// MessageText:
//
//  The license cannot be stored as it requires security upgrade of Digital Rights Management component.%0.
//
  NS_E_DRM_STORE_NEEDINDI          = HRESULT($C00D27DB);
  {$EXTERNALSYM NS_E_DRM_STORE_NEEDINDI}

//
// MessageId: NS_E_DRM_STORE_NOTALLOWED
//
// MessageText:
//
//  Your machine does not meet the requirements for storing the license.%0.
//
  NS_E_DRM_STORE_NOTALLOWED        = HRESULT($C00D27DC);
  {$EXTERNALSYM NS_E_DRM_STORE_NOTALLOWED}

//
// MessageId: NS_E_DRM_LICENSE_APP_NOTALLOWED
//
// MessageText:
//
//  The license for this file requires an upgraded version of your player or a different player.%0.
//
  NS_E_DRM_LICENSE_APP_NOTALLOWED  = HRESULT($C00D27DD);
  {$EXTERNALSYM NS_E_DRM_LICENSE_APP_NOTALLOWED}

//
// MessageId: NS_S_DRM_NEEDS_INDIVIDUALIZATION
//
// MessageText:
//
//  A security upgrade is required to perform the operation on this media file.%0
//
  NS_S_DRM_NEEDS_INDIVIDUALIZATION = HRESULT($000D27DE);
  {$EXTERNALSYM NS_S_DRM_NEEDS_INDIVIDUALIZATION}

//
// MessageId: NS_E_DRM_LICENSE_CERT_EXPIRED
//
// MessageText:
//
//  The license server's certificate expired. Make sure your system clock is set correctly. Contact your content provider for further assistance. %0.
//
  NS_E_DRM_LICENSE_CERT_EXPIRED    = HRESULT($C00D27DF);
  {$EXTERNALSYM NS_E_DRM_LICENSE_CERT_EXPIRED}

//
// MessageId: NS_E_DRM_LICENSE_SECLOW
//
// MessageText:
//
//  The license for this file requires a higher level of security than the player
//  you are currently using has. Try using a different player or download a newer
//  version of your current player.%0
//
  NS_E_DRM_LICENSE_SECLOW          = HRESULT($C00D27E0);
  {$EXTERNALSYM NS_E_DRM_LICENSE_SECLOW}

//
// MessageId: NS_E_DRM_LICENSE_CONTENT_REVOKED
//
// MessageText:
//
//  The content owner for the license you just acquired is no longer supporting their content. Contact the content owner for a newer version of the content.%0
//
  NS_E_DRM_LICENSE_CONTENT_REVOKED = HRESULT($C00D27E1);
  {$EXTERNALSYM NS_E_DRM_LICENSE_CONTENT_REVOKED}

//
// MessageId: NS_E_DRM_LICENSE_NOSAP
//
// MessageText:
//
//  The license for this file requires a feature that is not supported in your
//  current player or operating system. You can try with newer version of your current
//  player or contact your content provider for further assistance.%0
//
  NS_E_DRM_LICENSE_NOSAP           = HRESULT($C00D280A);
  {$EXTERNALSYM NS_E_DRM_LICENSE_NOSAP}

//
// MessageId: NS_E_DRM_LICENSE_NOSVP
//
// MessageText:
//
//  The license for this file requires a feature that is not supported in your current player or operating system. You can try with newer version of your current player or contact your content provider for further assistance.%0
//
  NS_E_DRM_LICENSE_NOSVP           = HRESULT($C00D280B);
  {$EXTERNALSYM NS_E_DRM_LICENSE_NOSVP}

//
// MessageId: NS_E_DRM_LICENSE_NOWDM
//
// MessageText:
//
//  The license for this file requires Windows Driver Model (WDM) audio drivers. Contact your sound card manufacturer for further assistance.%0
//
  NS_E_DRM_LICENSE_NOWDM           = HRESULT($C00D280C);
  {$EXTERNALSYM NS_E_DRM_LICENSE_NOWDM}

//
// MessageId: NS_E_DRM_LICENSE_NOTRUSTEDCODEC
//
// MessageText:
//
//  The license for this file requires a higher level of security than the player you are currently using has. Try using a different player or download a newer version of your current player.%0
//
  NS_E_DRM_LICENSE_NOTRUSTEDCODEC  = HRESULT($C00D280D);
  {$EXTERNALSYM NS_E_DRM_LICENSE_NOTRUSTEDCODEC}


//
// End of License Reasons Section
//

//
// MessageId: NS_E_DRM_NEEDS_UPGRADE_TEMPFILE
//
// MessageText:
//
//  An updated version of your media player is required to play the selected content.%0
//
  NS_E_DRM_NEEDS_UPGRADE_TEMPFILE  = HRESULT($C00D283D);
  {$EXTERNALSYM NS_E_DRM_NEEDS_UPGRADE_TEMPFILE}

//
// MessageId: NS_E_DRM_NEED_UPGRADE_PD
//
// MessageText:
//
//  A new version of the Digital Rights Management component is required. Contact product support for this application to get the latest version.%0
//
  NS_E_DRM_NEED_UPGRADE_PD         = HRESULT($C00D283E);
  {$EXTERNALSYM NS_E_DRM_NEED_UPGRADE_PD}

//
// MessageId: NS_E_DRM_SIGNATURE_FAILURE
//
// MessageText:
//
//  Failed to either create or verify the content header.%0
//
  NS_E_DRM_SIGNATURE_FAILURE       = HRESULT($C00D283F);
  {$EXTERNALSYM NS_E_DRM_SIGNATURE_FAILURE}

//
// MessageId: NS_E_DRM_LICENSE_SERVER_INFO_MISSING
//
// MessageText:
//
//  Could not read the necessary information from the system registry.%0
//
  NS_E_DRM_LICENSE_SERVER_INFO_MISSING = HRESULT($C00D2840);
  {$EXTERNALSYM NS_E_DRM_LICENSE_SERVER_INFO_MISSING}

//
// MessageId: NS_E_DRM_BUSY
//
// MessageText:
//
//  The DRM subsystem is currently locked by another application or user.  Try again later.%0
//
  NS_E_DRM_BUSY                    = HRESULT($C00D2841);
  {$EXTERNALSYM NS_E_DRM_BUSY}

//
// MessageId: NS_E_DRM_PD_TOO_MANY_DEVICES
//
// MessageText:
//
//  There are too many target devices registered on the portable media.%0
//
  NS_E_DRM_PD_TOO_MANY_DEVICES     = HRESULT($C00D2842);
  {$EXTERNALSYM NS_E_DRM_PD_TOO_MANY_DEVICES}

//
// MessageId: NS_E_DRM_INDIV_FRAUD
//
// MessageText:
//
//  The security upgrade cannot be completed because the allowed number of daily upgrades has been exceeded. Try again tomorrow.%0
//
  NS_E_DRM_INDIV_FRAUD             = HRESULT($C00D2843);
  {$EXTERNALSYM NS_E_DRM_INDIV_FRAUD}

//
// MessageId: NS_E_DRM_INDIV_NO_CABS
//
// MessageText:
//
//  The security upgrade cannot be completed because the server is unable to perform the operation. Try again later.%0
//
  NS_E_DRM_INDIV_NO_CABS           = HRESULT($C00D2844);
  {$EXTERNALSYM NS_E_DRM_INDIV_NO_CABS}

//
// MessageId: NS_E_DRM_INDIV_SERVICE_UNAVAILABLE
//
// MessageText:
//
//  The security upgrade cannot be performed because the server is not available. Try again later.%0
//
  NS_E_DRM_INDIV_SERVICE_UNAVAILABLE = HRESULT($C00D2845);
  {$EXTERNALSYM NS_E_DRM_INDIV_SERVICE_UNAVAILABLE}

//
// MessageId: NS_E_DRM_RESTORE_SERVICE_UNAVAILABLE
//
// MessageText:
//
//  Windows Media Player cannot restore your licenses because the server is not available. Try again later.%0
//
  NS_E_DRM_RESTORE_SERVICE_UNAVAILABLE = HRESULT($C00D2846);
  {$EXTERNALSYM NS_E_DRM_RESTORE_SERVICE_UNAVAILABLE}



/////////////////////////////////////////////////////////////////////////
//
// Windows Media Setup Specific Errors
//
// IdRange = 11000..11999
/////////////////////////////////////////////////////////////////////////
//
// MessageId: NS_S_REBOOT_RECOMMENDED
//
// MessageText:
//
//  The requested operation is successful.  Some cleanup will not be complete until the system is rebooted.%0
//
  NS_S_REBOOT_RECOMMENDED          = HRESULT($000D2AF8);
  {$EXTERNALSYM NS_S_REBOOT_RECOMMENDED}

//
// MessageId: NS_S_REBOOT_REQUIRED
//
// MessageText:
//
//  The requested operation is successful.  The system will not function correctly until the system is rebooted.%0
//
  NS_S_REBOOT_REQUIRED             = HRESULT($000D2AF9);
  {$EXTERNALSYM NS_S_REBOOT_REQUIRED}

//
// MessageId: NS_E_REBOOT_RECOMMENDED
//
// MessageText:
//
//  The requested operation failed.  Some cleanup will not be complete until the system is rebooted.%0
//
  NS_E_REBOOT_RECOMMENDED          = HRESULT($C00D2AFA);
  {$EXTERNALSYM NS_E_REBOOT_RECOMMENDED}

//
// MessageId: NS_E_REBOOT_REQUIRED
//
// MessageText:
//
//  The requested operation failed.  The system will not function correctly until the system is rebooted.%0
//
  NS_E_REBOOT_REQUIRED             = HRESULT($C00D2AFB);
  {$EXTERNALSYM NS_E_REBOOT_REQUIRED}


/////////////////////////////////////////////////////////////////////////
//
// Windows Media Networking Errors
//
// IdRange = 12000..12999
/////////////////////////////////////////////////////////////////////////
//
// MessageId: NS_E_UNKNOWN_PROTOCOL
//
// MessageText:
//
//  The specified protocol is not supported.%0
//
  NS_E_UNKNOWN_PROTOCOL            = HRESULT($C00D2EE0);
  {$EXTERNALSYM NS_E_UNKNOWN_PROTOCOL}

//
// MessageId: NS_E_REDIRECT_TO_PROXY
//
// MessageText:
//
//  The client is redirected to a proxy server.%0
//
  NS_E_REDIRECT_TO_PROXY           = HRESULT($C00D2EE1);
  {$EXTERNALSYM NS_E_REDIRECT_TO_PROXY}

//
// MessageId: NS_E_INTERNAL_SERVER_ERROR
//
// MessageText:
//
//  The server encountered an unexpected condition which prevented it from fulfilling the request.%0
//
  NS_E_INTERNAL_SERVER_ERROR       = HRESULT($C00D2EE2);
  {$EXTERNALSYM NS_E_INTERNAL_SERVER_ERROR}

//
// MessageId: NS_E_BAD_REQUEST
//
// MessageText:
//
//  The request could not be understood by the server.%0
//
  NS_E_BAD_REQUEST                 = HRESULT($C00D2EE3);
  {$EXTERNALSYM NS_E_BAD_REQUEST}

//
// MessageId: NS_E_ERROR_FROM_PROXY
//
// MessageText:
//
//  The proxy experienced an error while attempting to contact the media server.%0
//
  NS_E_ERROR_FROM_PROXY            = HRESULT($C00D2EE4);
  {$EXTERNALSYM NS_E_ERROR_FROM_PROXY}

//
// MessageId: NS_E_PROXY_TIMEOUT
//
// MessageText:
//
//  The proxy did not receive a timely response while attempting to contact the media server.%0
//
  NS_E_PROXY_TIMEOUT               = HRESULT($C00D2EE5);
  {$EXTERNALSYM NS_E_PROXY_TIMEOUT}

//
// MessageId: NS_E_SERVER_UNAVAILABLE
//
// MessageText:
//
//  The server is currently unable to handle the request due to a temporary overloading or maintenance of the server.%0
//
  NS_E_SERVER_UNAVAILABLE          = HRESULT($C00D2EE6);
  {$EXTERNALSYM NS_E_SERVER_UNAVAILABLE}

//
// MessageId: NS_E_REFUSED_BY_SERVER
//
// MessageText:
//
//  The server is refusing to fulfill the requested operation.%0
//
  NS_E_REFUSED_BY_SERVER           = HRESULT($C00D2EE7);
  {$EXTERNALSYM NS_E_REFUSED_BY_SERVER}

//
// MessageId: NS_E_INCOMPATIBLE_SERVER
//
// MessageText:
//
//  The server is not a compatible streaming media server.%0
//
  NS_E_INCOMPATIBLE_SERVER         = HRESULT($C00D2EE8);
  {$EXTERNALSYM NS_E_INCOMPATIBLE_SERVER}

//
// MessageId: NS_E_MULTICAST_DISABLED
//
// MessageText:
//
//  The content cannot be streamed because the Multicast protocol has been disabled.%0
//
  NS_E_MULTICAST_DISABLED          = HRESULT($C00D2EE9);
  {$EXTERNALSYM NS_E_MULTICAST_DISABLED}

//
// MessageId: NS_E_INVALID_REDIRECT
//
// MessageText:
//
//  The server redirected the player to an invalid location.%0
//
  NS_E_INVALID_REDIRECT            = HRESULT($C00D2EEA);
  {$EXTERNALSYM NS_E_INVALID_REDIRECT}

//
// MessageId: NS_E_ALL_PROTOCOLS_DISABLED
//
// MessageText:
//
//  The content cannot be streamed because all protocols have been disabled.%0
//
  NS_E_ALL_PROTOCOLS_DISABLED      = HRESULT($C00D2EEB);
  {$EXTERNALSYM NS_E_ALL_PROTOCOLS_DISABLED}

//
// MessageId: NS_E_MSBD_NO_LONGER_SUPPORTED
//
// MessageText:
//
//  The MSBD protocol is no longer supported. Please use HTTP to connect to the Windows Media stream.%0
//
  NS_E_MSBD_NO_LONGER_SUPPORTED    = HRESULT($C00D2EEC);
  {$EXTERNALSYM NS_E_MSBD_NO_LONGER_SUPPORTED}

//
// MessageId: NS_E_PROXY_NOT_FOUND
//
// MessageText:
//
//  The proxy server could not be located. Please check your proxy server configuration.%0
//
  NS_E_PROXY_NOT_FOUND             = HRESULT($C00D2EED);
  {$EXTERNALSYM NS_E_PROXY_NOT_FOUND}

//
// MessageId: NS_E_CANNOT_CONNECT_TO_PROXY
//
// MessageText:
//
//  Unable to establish a connection to the proxy server. Please check your proxy server configuration.%0
//
  NS_E_CANNOT_CONNECT_TO_PROXY     = HRESULT($C00D2EEE);
  {$EXTERNALSYM NS_E_CANNOT_CONNECT_TO_PROXY}

//
// MessageId: NS_E_SERVER_DNS_TIMEOUT
//
// MessageText:
//
//  Unable to locate the media server. The operation timed out.%0
//
  NS_E_SERVER_DNS_TIMEOUT          = HRESULT($C00D2EEF);
  {$EXTERNALSYM NS_E_SERVER_DNS_TIMEOUT}

//
// MessageId: NS_E_PROXY_DNS_TIMEOUT
//
// MessageText:
//
//  Unable to locate the proxy server. The operation timed out.%0
//
  NS_E_PROXY_DNS_TIMEOUT           = HRESULT($C00D2EF0);
  {$EXTERNALSYM NS_E_PROXY_DNS_TIMEOUT}

//
// MessageId: NS_E_CLOSED_ON_SUSPEND
//
// MessageText:
//
//  Media closed because Windows was shut down.%0
//
  NS_E_CLOSED_ON_SUSPEND           = HRESULT($C00D2EF1);
  {$EXTERNALSYM NS_E_CLOSED_ON_SUSPEND}

//
// MessageId: NS_E_CANNOT_READ_PLAYLIST_FROM_MEDIASERVER
//
// MessageText:
//
//  Unable to read the contents of a playlist file from a media server.%0
//
  NS_E_CANNOT_READ_PLAYLIST_FROM_MEDIASERVER = HRESULT($C00D2EF2);
  {$EXTERNALSYM NS_E_CANNOT_READ_PLAYLIST_FROM_MEDIASERVER}

//
// MessageId: NS_E_SESSION_NOT_FOUND
//
// MessageText:
//
//  Session not found.%0
//
  NS_E_SESSION_NOT_FOUND           = HRESULT($C00D2EF3);
  {$EXTERNALSYM NS_E_SESSION_NOT_FOUND}

//
// MessageId: NS_E_REQUIRE_STREAMING_CLIENT
//
// MessageText:
//
//  Content requires a streaming media client.%0
//
  NS_E_REQUIRE_STREAMING_CLIENT    = HRESULT($C00D2EF4);
  {$EXTERNALSYM NS_E_REQUIRE_STREAMING_CLIENT}

//
// MessageId: NS_E_PLAYLIST_ENTRY_HAS_CHANGED
//
// MessageText:
//
//  A command applies to a previous playlist entry.%0
//
  NS_E_PLAYLIST_ENTRY_HAS_CHANGED  = HRESULT($C00D2EF5);
  {$EXTERNALSYM NS_E_PLAYLIST_ENTRY_HAS_CHANGED}

//
// MessageId: NS_E_PROXY_ACCESSDENIED
//
// MessageText:
//
//  The proxy server is denying access.  The username and/or password might be incorrect.%0
//
  NS_E_PROXY_ACCESSDENIED          = HRESULT($C00D2EF6);
  {$EXTERNALSYM NS_E_PROXY_ACCESSDENIED}

//
// MessageId: NS_E_PROXY_SOURCE_ACCESSDENIED
//
// MessageText:
//
//  The proxy could not provide valid authentication credentials to the media server.%0
//
  NS_E_PROXY_SOURCE_ACCESSDENIED   = HRESULT($C00D2EF7);
  {$EXTERNALSYM NS_E_PROXY_SOURCE_ACCESSDENIED}

//
// MessageId: NS_E_NETWORK_SINK_WRITE
//
// MessageText:
//
//  The network sink failed to write data to the network.%0
//
  NS_E_NETWORK_SINK_WRITE          = HRESULT($C00D2EF8);
  {$EXTERNALSYM NS_E_NETWORK_SINK_WRITE}

//
// MessageId: NS_E_FIREWALL
//
// MessageText:
//
//  Packets are not being received from the server. The packets might be blocked by a filtering device, such as a network firewall.%0
//
  NS_E_FIREWALL                    = HRESULT($C00D2EF9);
  {$EXTERNALSYM NS_E_FIREWALL}

//
// MessageId: NS_E_MMS_NOT_SUPPORTED
//
// MessageText:
//
//  The MMS protocol is not supported. Please use HTTP or RTSP to connect to the Windows Media stream.%0
//
  NS_E_MMS_NOT_SUPPORTED           = HRESULT($C00D2EFA);
  {$EXTERNALSYM NS_E_MMS_NOT_SUPPORTED}

//
// MessageId: NS_E_SERVER_ACCESSDENIED
//
// MessageText:
//
//  The Windows Media server is denying access.  The username and/or password might be incorrect.%0
//
  NS_E_SERVER_ACCESSDENIED         = HRESULT($C00D2EFB);
  {$EXTERNALSYM NS_E_SERVER_ACCESSDENIED}

//
// MessageId: NS_E_RESOURCE_GONE
//
// MessageText:
//
//  The Publishing Point or file on the Windows Media Server is no longer available.%0
//
  NS_E_RESOURCE_GONE               = HRESULT($C00D2EFC);
  {$EXTERNALSYM NS_E_RESOURCE_GONE}

//
// MessageId: NS_E_NO_EXISTING_PACKETIZER
//
// MessageText:
//
//  There is no existing packetizer plugin for a stream.%0
//
  NS_E_NO_EXISTING_PACKETIZER      = HRESULT($C00D2EFD);
  {$EXTERNALSYM NS_E_NO_EXISTING_PACKETIZER}

//
// MessageId: NS_E_BAD_SYNTAX_IN_SERVER_RESPONSE
//
// MessageText:
//
//  The response from the media server could not be understood. This might be caused by an incompatible proxy server or media server.%0
//
  NS_E_BAD_SYNTAX_IN_SERVER_RESPONSE = HRESULT($C00D2EFE);
  {$EXTERNALSYM NS_E_BAD_SYNTAX_IN_SERVER_RESPONSE}

//
// MessageId: NS_I_RECONNECTED
//
// MessageText:
//
//  The client is reconnected.%0
//
  NS_I_RECONNECTED                 = HRESULT($400D2EFF);
  {$EXTERNALSYM NS_I_RECONNECTED}

//
// MessageId: NS_E_RESET_SOCKET_CONNECTION
//
// MessageText:
//
//  The Windows Media Server reset the network connection.%0
//
  NS_E_RESET_SOCKET_CONNECTION     = HRESULT($C00D2F00);
  {$EXTERNALSYM NS_E_RESET_SOCKET_CONNECTION}

//
// MessageId: NS_I_NOLOG_STOP
//
// MessageText:
//
//  Forcing a switch to a pending header on start.%0
//
  NS_I_NOLOG_STOP                  = HRESULT($400D2F01);
  {$EXTERNALSYM NS_I_NOLOG_STOP}

//
// MessageId: NS_E_TOO_MANY_HOPS
//
// MessageText:
//
//  The request could not reach the media server (too many hops).%0
//
  NS_E_TOO_MANY_HOPS               = HRESULT($C00D2F02);
  {$EXTERNALSYM NS_E_TOO_MANY_HOPS}

//
// MessageId: NS_I_EXISTING_PACKETIZER
//
// MessageText:
//
//  There is already an existing packetizer plugin for the stream.%0
//
  NS_I_EXISTING_PACKETIZER         = HRESULT($400D2F03);
  {$EXTERNALSYM NS_I_EXISTING_PACKETIZER}

//
// MessageId: NS_I_MANUAL_PROXY
//
// MessageText:
//
//  The proxy setting is manual.%0
//
  NS_I_MANUAL_PROXY                = HRESULT($400D2F04);
  {$EXTERNALSYM NS_I_MANUAL_PROXY}

//
// MessageId: NS_E_TOO_MUCH_DATA_FROM_SERVER
//
// MessageText:
//
//  The server is sending too much data. The connection has been terminated.%0
//
  NS_E_TOO_MUCH_DATA_FROM_SERVER   = HRESULT($C00D2F05);
  {$EXTERNALSYM NS_E_TOO_MUCH_DATA_FROM_SERVER}

//
// MessageId: NS_E_CONNECT_TIMEOUT
//
// MessageText:
//
//  It was not possible to establish a connection to the media server in a timely manner. The media server may be down for maintenance, or it may be necessary to use a proxy server to access this media server.%0
//
  NS_E_CONNECT_TIMEOUT             = HRESULT($C00D2F06);
  {$EXTERNALSYM NS_E_CONNECT_TIMEOUT}

//
// MessageId: NS_E_PROXY_CONNECT_TIMEOUT
//
// MessageText:
//
//  It was not possible to establish a connection to the proxy server in a timely manner. Please check your proxy server configuration.%0
//
  NS_E_PROXY_CONNECT_TIMEOUT       = HRESULT($C00D2F07);
  {$EXTERNALSYM NS_E_PROXY_CONNECT_TIMEOUT}

//
// MessageId: NS_E_SESSION_INVALID
//
// MessageText:
//
//  Session not found.%0
//
  NS_E_SESSION_INVALID             = HRESULT($C00D2F08);
  {$EXTERNALSYM NS_E_SESSION_INVALID}

//
// MessageId: NS_S_EOSRECEDING
//
// MessageText:
//
//  EOS hit during rewinding.%0
//
  NS_S_EOSRECEDING                 = HRESULT($000D2F09);
  {$EXTERNALSYM NS_S_EOSRECEDING}

//
// MessageId: NS_E_PACKETSINK_UNKNOWN_FEC_STREAM
//
// MessageText:
//
//  Unknown packet sink stream.%0
//
  NS_E_PACKETSINK_UNKNOWN_FEC_STREAM = HRESULT($C00D2F0A);
  {$EXTERNALSYM NS_E_PACKETSINK_UNKNOWN_FEC_STREAM}

//
// MessageId: NS_E_PUSH_CANNOTCONNECT
//
// MessageText:
//
//  Unable to establish a connection to the server. Ensure Windows Media Services is started and the HTTP Server control protocol is properly enabled.%0
//
  NS_E_PUSH_CANNOTCONNECT          = HRESULT($C00D2F0B);
  {$EXTERNALSYM NS_E_PUSH_CANNOTCONNECT}

//
// MessageId: NS_E_INCOMPATIBLE_PUSH_SERVER
//
// MessageText:
//
//  The Server service that received the HTTP push request is not a compatible version of Windows Media Services (WMS).  This error may indicate the push request was received by IIS instead of WMS.  Ensure WMS is started and has the HTTP Server control protocol properly enabled and try again.%0
//
  NS_E_INCOMPATIBLE_PUSH_SERVER    = HRESULT($C00D2F0C);
  {$EXTERNALSYM NS_E_INCOMPATIBLE_PUSH_SERVER}

//
// MessageId: NS_S_CHANGENOTICE
//
// MessageText:
//
//  Internal.%0
//
  NS_S_CHANGENOTICE                = HRESULT($000D2F0D);
  {$EXTERNALSYM NS_S_CHANGENOTICE}


/////////////////////////////////////////////////////////////////////////
//
// Windows Media Client Media Services
//
// IdRange = 13000..13999 (0x32C8-0x36AF)
/////////////////////////////////////////////////////////////////////////
//
// MessageId: NS_E_END_OF_PLAYLIST
//
// MessageText:
//
//  The playlist has reached its end.%0
//
  NS_E_END_OF_PLAYLIST             = HRESULT($C00D32C8);
  {$EXTERNALSYM NS_E_END_OF_PLAYLIST}

//
// MessageId: NS_E_USE_FILE_SOURCE
//
// MessageText:
//
//  Use file source.%0
//
  NS_E_USE_FILE_SOURCE             = HRESULT($C00D32C9);
  {$EXTERNALSYM NS_E_USE_FILE_SOURCE}

//
// MessageId: NS_E_PROPERTY_NOT_FOUND
//
// MessageText:
//
//  The property was not found.%0
//
  NS_E_PROPERTY_NOT_FOUND          = HRESULT($C00D32CA);
  {$EXTERNALSYM NS_E_PROPERTY_NOT_FOUND}

//
// MessageId: NS_E_PROPERTY_READ_ONLY
//
// MessageText:
//
//  The property is read only.%0
//
  NS_E_PROPERTY_READ_ONLY          = HRESULT($C00D32CC);
  {$EXTERNALSYM NS_E_PROPERTY_READ_ONLY}

//
// MessageId: NS_E_TABLE_KEY_NOT_FOUND
//
// MessageText:
//
//  The table key was not found.%0
//
  NS_E_TABLE_KEY_NOT_FOUND         = HRESULT($C00D32CD);
  {$EXTERNALSYM NS_E_TABLE_KEY_NOT_FOUND}

//
// MessageId: NS_E_INVALID_QUERY_OPERATOR
//
// MessageText:
//
//  Invalid query operator.%0
//
  NS_E_INVALID_QUERY_OPERATOR      = HRESULT($C00D32CF);
  {$EXTERNALSYM NS_E_INVALID_QUERY_OPERATOR}

//
// MessageId: NS_E_INVALID_QUERY_PROPERTY
//
// MessageText:
//
//  Invalid query property.%0
//
  NS_E_INVALID_QUERY_PROPERTY      = HRESULT($C00D32D0);
  {$EXTERNALSYM NS_E_INVALID_QUERY_PROPERTY}

//
// MessageId: NS_E_PROPERTY_NOT_SUPPORTED
//
// MessageText:
//
//  The property is not supported.%0
//
  NS_E_PROPERTY_NOT_SUPPORTED      = HRESULT($C00D32D2);
  {$EXTERNALSYM NS_E_PROPERTY_NOT_SUPPORTED}

//
// MessageId: NS_E_SCHEMA_CLASSIFY_FAILURE
//
// MessageText:
//
//  Schema classification failure.%0
//
  NS_E_SCHEMA_CLASSIFY_FAILURE     = HRESULT($C00D32D4);
  {$EXTERNALSYM NS_E_SCHEMA_CLASSIFY_FAILURE}

//
// MessageId: NS_E_METADATA_FORMAT_NOT_SUPPORTED
//
// MessageText:
//
//  The metadata format is not supported.%0
//
  NS_E_METADATA_FORMAT_NOT_SUPPORTED = HRESULT($C00D32D5);
  {$EXTERNALSYM NS_E_METADATA_FORMAT_NOT_SUPPORTED}

//
// MessageId: NS_E_METADATA_NO_EDITING_CAPABILITY
//
// MessageText:
//
//  Cannot edit the metadata.%0
//
  NS_E_METADATA_NO_EDITING_CAPABILITY = HRESULT($C00D32D6);
  {$EXTERNALSYM NS_E_METADATA_NO_EDITING_CAPABILITY}

//
// MessageId: NS_E_METADATA_CANNOT_SET_LOCALE
//
// MessageText:
//
//  Cannot set the locale id.%0
//
  NS_E_METADATA_CANNOT_SET_LOCALE  = HRESULT($C00D32D7);
  {$EXTERNALSYM NS_E_METADATA_CANNOT_SET_LOCALE}

//
// MessageId: NS_E_METADATA_LANGUAGE_NOT_SUPORTED
//
// MessageText:
//
//  The language is not supported in the format.%0
//
  NS_E_METADATA_LANGUAGE_NOT_SUPORTED = HRESULT($C00D32D8);
  {$EXTERNALSYM NS_E_METADATA_LANGUAGE_NOT_SUPORTED}

//
// MessageId: NS_E_METADATA_NO_RFC1766_NAME_FOR_LOCALE
//
// MessageText:
//
//  There is no RFC1766 name translation for the supplied locale id.%0
//
  NS_E_METADATA_NO_RFC1766_NAME_FOR_LOCALE = HRESULT($C00D32D9);
  {$EXTERNALSYM NS_E_METADATA_NO_RFC1766_NAME_FOR_LOCALE}

//
// MessageId: NS_E_METADATA_NOT_AVAILABLE
//
// MessageText:
//
//  The metadata (or metadata item) is not available.%0
//
  NS_E_METADATA_NOT_AVAILABLE      = HRESULT($C00D32DA);
  {$EXTERNALSYM NS_E_METADATA_NOT_AVAILABLE}

//
// MessageId: NS_E_METADATA_CACHE_DATA_NOT_AVAILABLE
//
// MessageText:
//
//  The cached metadata (or metadata item) is not available.%0
//
  NS_E_METADATA_CACHE_DATA_NOT_AVAILABLE = HRESULT($C00D32DB);
  {$EXTERNALSYM NS_E_METADATA_CACHE_DATA_NOT_AVAILABLE}

//
// MessageId: NS_E_METADATA_INVALID_DOCUMENT_TYPE
//
// MessageText:
//
//  The metadata document is invalid.%0
//
  NS_E_METADATA_INVALID_DOCUMENT_TYPE = HRESULT($C00D32DC);
  {$EXTERNALSYM NS_E_METADATA_INVALID_DOCUMENT_TYPE}

//
// MessageId: NS_E_METADATA_IDENTIFIER_NOT_AVAILABLE
//
// MessageText:
//
//  The metadata content identifier is not available.%0
//
  NS_E_METADATA_IDENTIFIER_NOT_AVAILABLE = HRESULT($C00D32DD);
  {$EXTERNALSYM NS_E_METADATA_IDENTIFIER_NOT_AVAILABLE}

//
// MessageId: NS_E_METADATA_CANNOT_RETRIEVE_FROM_OFFLINE_CACHE
//
// MessageText:
//
//  Cannot retrieve metadata from the offline metadata cache.%0
//
  NS_E_METADATA_CANNOT_RETRIEVE_FROM_OFFLINE_CACHE = HRESULT($C00D32DE);
  {$EXTERNALSYM NS_E_METADATA_CANNOT_RETRIEVE_FROM_OFFLINE_CACHE}



///////////////////////////////////////////////////////////////////////////
//
// ASFErr.h - definition of ASF HRESULT codes
//
//=========================================================================
//
//  Microsoft Windows Media Technologies
//  Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//=========================================================================


// #define STATUS_SEVERITY(hr)  (((hr) >> 30) & 0x3)

///////////////////////////////////////////////////////////////////////////
//
// Advanced Streaming Format (ASF) Errors (2000 - 2999)
//
//
//  Values are 32 bit values layed out as follows:
//
//   3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1
//   1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
//  +---+-+-+-----------------------+-------------------------------+
//  |Sev|C|R|     Facility          |               Code            |
//  +---+-+-+-----------------------+-------------------------------+
//
//  where
//
//      Sev - is the severity code
//
//          00 - Success
//          01 - Informational
//          10 - Warning
//          11 - Error
//
//      C - is the Customer code flag
//
//      R - is a reserved bit
//
//      Facility - is the facility code
//
//      Code - is the facility's status code
//
//

//
// MessageId: ASF_E_BUFFEROVERRUN
//
// MessageText:
//
//  An attempt was made to seek or position past the end of a buffer.%0
//
  ASF_E_BUFFEROVERRUN             = HRESULT($C00D07D0);
  {$EXTERNALSYM ASF_E_BUFFEROVERRUN}

//
// MessageId: ASF_E_BUFFERTOOSMALL
//
// MessageText:
//
//  The supplied input or output buffer was too small.%0
//
  ASF_E_BUFFERTOOSMALL            = HRESULT($C00D07D1);
  {$EXTERNALSYM ASF_E_BUFFERTOOSMALL}

//
// MessageId: ASF_E_BADLANGUAGEID
//
// MessageText:
//
//  The language ID was not found.%0
//
  ASF_E_BADLANGUAGEID             = HRESULT($C00D07D2);
  {$EXTERNALSYM ASF_E_BADLANGUAGEID}

//
// MessageId: ASF_E_NOPAYLOADLENGTH
//
// MessageText:
//
//  The multiple payload packet is missing the payload length.%0
//
  ASF_E_NOPAYLOADLENGTH           = HRESULT($C00D07DB);
  {$EXTERNALSYM ASF_E_NOPAYLOADLENGTH}

//
// MessageId: ASF_E_TOOMANYPAYLOADS
//
// MessageText:
//
//  The packet contains too many payloads.%0
//
  ASF_E_TOOMANYPAYLOADS           = HRESULT($C00D07DC);
  {$EXTERNALSYM ASF_E_TOOMANYPAYLOADS}

//
// MessageId: ASF_E_PACKETCONTENTTOOLARGE
//
// MessageText:
//
//  ASF_E_PACKETCONTENTTOOLARGE
//
  ASF_E_PACKETCONTENTTOOLARGE     = HRESULT($C00D07DE);
  {$EXTERNALSYM ASF_E_PACKETCONTENTTOOLARGE}

//
// MessageId: ASF_E_UNKNOWNPACKETSIZE
//
// MessageText:
//
//  Expecting a fixed packet size but min. and max. are not equal.%0
//
  ASF_E_UNKNOWNPACKETSIZE         = HRESULT($C00D07E0);
  {$EXTERNALSYM ASF_E_UNKNOWNPACKETSIZE}

//
// MessageId: ASF_E_INVALIDHEADER
//
// MessageText:
//
//  ASF_E_INVALIDHEADER
//
  ASF_E_INVALIDHEADER             = HRESULT($C00D07E2);
  {$EXTERNALSYM ASF_E_INVALIDHEADER}

//
// MessageId: ASF_E_NOCLOCKOBJECT
//
// MessageText:
//
//  The object does not have a valid clock object.%0
//
  ASF_E_NOCLOCKOBJECT              = HRESULT($C00D07E6);
  {$EXTERNALSYM ASF_E_NOCLOCKOBJECT}

//
// MessageId: ASF_E_UNKNOWNCLOCKTYPE
//
// MessageText:
//
//  ASF_E_UNKNOWNCLOCKTYPE
//
  ASF_E_UNKNOWNCLOCKTYPE           = HRESULT($C00D07EB);
  {$EXTERNALSYM ASF_E_UNKNOWNCLOCKTYPE}

//
// MessageId: ASF_E_OPAQUEPACKET
//
// MessageText:
//
//  An attempt was made to restore or access an opaque packet.%0
//
  ASF_E_OPAQUEPACKET               = HRESULT($C00D07ED);
  {$EXTERNALSYM ASF_E_OPAQUEPACKET}

//
// MessageId: ASF_E_WRONGVERSION
//
// MessageText:
//
//  ASF_E_WRONGVERSION
//
  ASF_E_WRONGVERSION               = HRESULT($C00D07EE);
  {$EXTERNALSYM ASF_E_WRONGVERSION}

//
// MessageId: ASF_E_OVERFLOW
//
// MessageText:
//
//  An attempt was made to store a value which was larger than then destination's maximum value.%0
//
  ASF_E_OVERFLOW                   = HRESULT($C00D07EF);
  {$EXTERNALSYM ASF_E_OVERFLOW}

//
// MessageId: ASF_E_NOTFOUND
//
// MessageText:
//
//  The object was not found.%0
//
  ASF_E_NOTFOUND                   = HRESULT($C00D07F0);
  {$EXTERNALSYM ASF_E_NOTFOUND}

//
// Someone else is using MessageIds 2033 & 2034, so we skip them
//
// 2033 = NS_E_NOTHING_TO_DO
// 2034 = NS_E_NO_MULTICAST

//
// MessageId: ASF_E_OBJECTTOOBIG
//
// MessageText:
//
//  The object is too large to be processed in the requested manner.%0
//
  ASF_E_OBJECTTOOBIG               = HRESULT($C00D07F3);
  {$EXTERNALSYM ASF_E_OBJECTTOOBIG}

//
// MessageId: ASF_E_UNEXPECTEDVALUE
//
// MessageText:
//
//  A value was not set as expected.%0
//
  ASF_E_UNEXPECTEDVALUE            = HRESULT($C00D07F4);
  {$EXTERNALSYM ASF_E_UNEXPECTEDVALUE}

//
// MessageId: ASF_E_INVALIDSTATE
//
// MessageText:
//
//  The request is not valid in the object's current state.%0
//
  ASF_E_INVALIDSTATE               = HRESULT($C00D07F5);
  {$EXTERNALSYM ASF_E_INVALIDSTATE}

//
// MessageId: ASF_E_NOLIBRARY
//
// MessageText:
//
//  This object does not have a valid library pointer; it was not properly created or it has been Shutdown().%0
//
  ASF_E_NOLIBRARY                  = HRESULT($C00D07F6);
  {$EXTERNALSYM ASF_E_NOLIBRARY}

//
// MessageId: ASF_E_ALREADYINITIALIZED
//
// MessageText:
//
//  This object has already been initialized; the setting cannot be changed.%0
//
  ASF_E_ALREADYINITIALIZED         = HRESULT($C00D07F7);
  {$EXTERNALSYM ASF_E_ALREADYINITIALIZED}

//
// MessageId: ASF_E_INVALIDINIT
//
// MessageText:
//
//  This object has not been initialized properly; that operation cannot be performed.%0
//
  ASF_E_INVALIDINIT                = HRESULT($C00D07F8);
  {$EXTERNALSYM ASF_E_INVALIDINIT}

//
// MessageId: ASF_E_NOHEADEROBJECT
//
// MessageText:
//
//  The ASF Header object could not be found.%0
//
  ASF_E_NOHEADEROBJECT             = HRESULT($C00D07F9);
  {$EXTERNALSYM ASF_E_NOHEADEROBJECT}

//
// MessageId: ASF_E_NODATAOBJECT
//
// MessageText:
//
//  The ASF Data object could not be found.%0
//
  ASF_E_NODATAOBJECT               = HRESULT($C00D07FA);
  {$EXTERNALSYM ASF_E_NODATAOBJECT}

//
// MessageId: ASF_E_NOINDEXOBJECT
//
// MessageText:
//
//  The ASF Index object could not be found.%0
//
  ASF_E_NOINDEXOBJECT              = HRESULT($C00D07FB);
  {$EXTERNALSYM ASF_E_NOINDEXOBJECT}

//
// MessageId: ASF_E_NOSTREAMPROPS
//
// MessageText:
//
//  A Stream Properties object with the correct stream number could not be found.%0
//
  ASF_E_NOSTREAMPROPS              = HRESULT($C00D07FC);
  {$EXTERNALSYM ASF_E_NOSTREAMPROPS}

//
// MessageId: ASF_E_NOFILEPROPS
//
// MessageText:
//
//  The File Properties object could not be found.%0
//
  ASF_E_NOFILEPROPS                = HRESULT($C00D07FD);
  {$EXTERNALSYM ASF_E_NOFILEPROPS}

//
// MessageId: ASF_E_NOLANGUAGELIST
//
// MessageText:
//
//  The Language List object could not be found.%0
//
  ASF_E_NOLANGUAGELIST             = HRESULT($C00D07FE);
  {$EXTERNALSYM ASF_E_NOLANGUAGELIST}

//
// MessageId: ASF_E_NOINDEXPARAMETERS
//
// MessageText:
//
//  The Index Parameters object could not be found.%0
//
  ASF_E_NOINDEXPARAMETERS          = HRESULT($C00D07FF);
  {$EXTERNALSYM ASF_E_NOINDEXPARAMETERS}

//
// MessageId: ASF_E_UNSUPPORTEDERRORCONCEALMENT
//
// MessageText:
//
//  The requested error concealment strategy is not supported by this component.%0
//
  ASF_E_UNSUPPORTEDERRORCONCEALMENT = HRESULT($C00D0800);
  {$EXTERNALSYM ASF_E_UNSUPPORTEDERRORCONCEALMENT}

//
// MessageId: ASF_E_INVALIDFLAGS
//
// MessageText:
//
//  The flags for this object or set of objects are not properly set.%0
//
  ASF_E_INVALIDFLAGS               = HRESULT($C00D0801);
  {$EXTERNALSYM ASF_E_INVALIDFLAGS}

//
// MessageId: ASF_E_BADDATADESCRIPTOR
//
// MessageText:
//
//  One or more data descriptors is not properly set.%0
//
  ASF_E_BADDATADESCRIPTOR          = HRESULT($C00D0802);
  {$EXTERNALSYM ASF_E_BADDATADESCRIPTOR}

//
// MessageId: ASF_E_BADINDEXINTERVAL
//
// MessageText:
//
//  The index has an invalid time interval (probably zero).%0
//
  ASF_E_BADINDEXINTERVAL           = HRESULT($C00D0803);
  {$EXTERNALSYM ASF_E_BADINDEXINTERVAL}

//
// MessageId: ASF_E_INVALIDTIME
//
// MessageText:
//
//  The given time value is not valid.%0
//
  ASF_E_INVALIDTIME                = HRESULT($C00D0804);
  {$EXTERNALSYM ASF_E_INVALIDTIME}

//
// MessageId: ASF_E_INVALIDINDEX
//
// MessageText:
//
//  The given index value is not valid.%0
//
  ASF_E_INVALIDINDEX               = HRESULT($C00D0805);
  {$EXTERNALSYM ASF_E_INVALIDINDEX}

//
// MessageId: ASF_E_STREAMNUMBERINUSE
//
// MessageText:
//
//  The specified stream number is already in use.%0
//
  ASF_E_STREAMNUMBERINUSE          = HRESULT($C00D0806);
  {$EXTERNALSYM ASF_E_STREAMNUMBERINUSE}

//
// MessageId: ASF_E_BADMEDIATYPE
//
// MessageText:
//
//  The specified media type does not work with this component.%0
//
  ASF_E_BADMEDIATYPE               = HRESULT($C00D0807);
  {$EXTERNALSYM ASF_E_BADMEDIATYPE}

//
// MessageId: ASF_E_WRITEFAILED
//
// MessageText:
//
//  The object could not be written as specified.%0
//
  ASF_E_WRITEFAILED                = HRESULT($C00D0808);
  {$EXTERNALSYM ASF_E_WRITEFAILED}

//
// MessageId: ASF_E_NOTENOUGHDESCRIPTORS
//
// MessageText:
//
//  The given data unit requires a larger number of descriptors to be fully parsed.%0
//
  ASF_E_NOTENOUGHDESCRIPTORS       = HRESULT($C00D0809);
  {$EXTERNALSYM ASF_E_NOTENOUGHDESCRIPTORS}

//
// MessageId: ASF_E_INDEXBLOCKUNLOADED
//
// MessageText:
//
//  The index entries for the specified index block have been unloaded from memory and are not available.%0
//
  ASF_E_INDEXBLOCKUNLOADED         = HRESULT($C00D080A);
  {$EXTERNALSYM ASF_E_INDEXBLOCKUNLOADED}

//
// MessageId: ASF_E_NOTENOUGHBANDWIDTH
//
// MessageText:
//
//  The specified bandwidth is not large enough.%0
//
  ASF_E_NOTENOUGHBANDWIDTH         = HRESULT($C00D080B);
  {$EXTERNALSYM ASF_E_NOTENOUGHBANDWIDTH}

//
// MessageId: ASF_E_EXCEEDEDMAXIMUMOBJECTSIZE
//
// MessageText:
//
//  The object has exceeded its maximum size.%0
//
  ASF_E_EXCEEDEDMAXIMUMOBJECTSIZE  = HRESULT($C00D080C);
  {$EXTERNALSYM ASF_E_EXCEEDEDMAXIMUMOBJECTSIZE}

//
// MessageId: ASF_E_BADDATAUNIT
//
// MessageText:
//
//  The given data unit is corrupted, badly formatted, or otherwise not valid.%0
//
  ASF_E_BADDATAUNIT                = HRESULT($C00D080D);
  {$EXTERNALSYM ASF_E_BADDATAUNIT}

//
// MessageId: ASF_E_HEADERSIZE
//
// MessageText:
//
//  The ASF header has exceeded the specified maximum size.%0
//
  ASF_E_HEADERSIZE                 = HRESULT($C00D080E);
  {$EXTERNALSYM ASF_E_HEADERSIZE}


///////////////////////////////////////////////////////////////////////////
//
// Advanced Streaming Format (ASF) Success Codes (2000 - 2999)
//

//
// MessageId: ASF_S_OPAQUEPACKET
//
// MessageText:
//
//  ASF_S_OPAQUEPACKET
//
  ASF_S_OPAQUEPACKET               = HRESULT($000D07F0);
  {$EXTERNALSYM ASF_S_OPAQUEPACKET}



//*@@@+++@@@@******************************************************************
//
// Microsoft Windows Media
// Copyright (C) Microsoft Corporation. All rights reserved.
//
//*@@@---@@@@******************************************************************

  // This routine examines the file extension in the URL or file name that is passed
  // in as an argument.  The routine returns S_OK if the file extension is included in a
  // set of file extensions that the SDK is typically able to handle. or NS_E_INVALID_NAME
  // if the file extension is not included in that set.
  //
  // This routine cannot be used to determine with absolute certainty if the SDK can
  // handle a particular URL, as this cannot be known until the URL is opened.

  function WMCheckURLExtension(pwszURL: PWideChar): HRESULT; stdcall;
  {$EXTERNALSYM WMCheckURLExtension}

  // This routine examines the URL scheme that is passed in as an argument.  The routine
  // returns S_OK if the URL scheme is included in a set of URL schemes that the SDK is
  // typically able to handle. or NS_E_INVALID_NAME if the URL scheme is not included in
  // that set.
  //
  // This routine cannot be used to determine with absolute certainty if the SDK can
  // handle a particular URL, as this cannot be known until the URL is opened.

  function WMCheckURLScheme(pwszURLScheme: PWideChar): HRESULT; stdcall;
  {$EXTERNALSYM WMCheckURLScheme}

  // This routine returns S_OK if the data buffer looks like a file type that is supported
  // by the SDK.  It returns NS_E_INVALID_DATA if the data buffer cannot be handled by the SDK.
  // This routine may return a false positive, but will not return a false negative.

  function WMValidateData(pbData: PBYTE; var pdwDataSize: LongWord): HRESULT; stdcall;
  {$EXTERNALSYM WMValidateData}

  // This routine validates that a URL can be played in Offline mode.
  // The parameter pwszLanguage can be set to a RFC-1766 language ID, or to NULL if
  // any language is acceptable.
  // The output parameter pfIsAvailableOffline is set to TRUE if the URL can be
  // played in Offline mode.

  function WMIsAvailableOffline(pwszURL, pwszLanguage: PWideChar; out pfIsAvailableOffline: BOOL): HRESULT; stdcall;
  {$EXTERNALSYM WMIsAvailableOffline}

  // This function examines a file and determines whether or not it is DRM protected.
  function WMIsContentProtected(pwszFileName: PWideChar; out pfIsProtected: BOOL): HRESULT; stdcall;
  {$EXTERNALSYM WMIsContentProtected}

  function WMCreateCertificate(out pUnkCert: IUnknown): HRESULT; stdcall;
  {$EXTERNALSYM WMCreateCertificate}
  function WMCreateWriter(pUnkCert: IUnknown; out ppWriter: IWMWriter): HRESULT; stdcall;
  {$EXTERNALSYM WMCreateWriter}
  function WMCreateReader(pUnkCert: IUnknown; dwRights: LongWord; out ppReader: IWMReader): HRESULT; stdcall;
  {$EXTERNALSYM WMCreateReader}
  function WMCreateSyncReader(pUnkCert: IUnknown; dwRights: LongWord; out ppSyncReader: IWMSyncReader): HRESULT; stdcall;
  {$EXTERNALSYM WMCreateSyncReader}
  function WMCreateEditor(out ppEditor: IWMMetadataEditor): HRESULT; stdcall;
  {$EXTERNALSYM WMCreateEditor}
  function WMCreateIndexer(out ppIndexer: IWMIndexer): HRESULT; stdcall;
  {$EXTERNALSYM WMCreateIndexer}
  function WMCreateBackupRestorer(pCallback: IUnknown; out ppBackup: IWMLicenseBackup): HRESULT; stdcall;
  {$EXTERNALSYM WMCreateBackupRestorer}
  function WMCreateProfileManager(out ppProfileManager: IWMProfileManager): HRESULT; stdcall;
  {$EXTERNALSYM WMCreateProfileManager}
  function WMCreateWriterFileSink(out ppSink: IWMWriterFileSink ): HRESULT; stdcall;
  {$EXTERNALSYM WMCreateWriterFileSink}
  function WMCreateWriterNetworkSink(out ppSink: IWMWriterNetworkSink): HRESULT; stdcall;
  {$EXTERNALSYM WMCreateWriterNetworkSink}
  function WMCreateWriterPushSink(out ppSink: IWMWriterPushSink): HRESULT; stdcall;
  {$EXTERNALSYM WMCreateWriterPushSink}


implementation

const
  WMVCORE = 'WMVCORE.DLL';

{$IFDEF WMF9_DYNAMIC_LINK}
var
  WMVCORELib: THandle = 0;

  _WMCheckURLExtension       : function(pwszURL: PWideChar): HRESULT; stdcall;
  _WMCheckURLScheme          : function(pwszURLScheme: PWideChar): HRESULT; stdcall;
  _WMValidateData            : function(pbData: PBYTE; var pdwDataSize: LongWord): HRESULT; stdcall;
  _WMIsAvailableOffline      : function(pwszURL, pwszLanguage: PWideChar; out pfIsAvailableOffline: BOOL): HRESULT; stdcall;
  _WMIsContentProtected      : function(pwszFileName: PWideChar; out pfIsProtected: BOOL): HRESULT; stdcall;
  _WMCreateCertificate       : function(out pUnkCert: IUnknown): HRESULT; stdcall;
  _WMCreateWriter            : function(pUnkCert: IUnknown; out ppWriter: IWMWriter): HRESULT; stdcall;
  _WMCreateReader            : function(pUnkCert: IUnknown; dwRights: LongWord; out ppReader: IWMReader): HRESULT; stdcall;
  _WMCreateSyncReader        : function(pUnkCert: IUnknown; dwRights: LongWord; out ppSyncReader: IWMSyncReader): HRESULT; stdcall;
  _WMCreateEditor            : function(out ppEditor: IWMMetadataEditor): HRESULT; stdcall;
  _WMCreateIndexer           : function(out ppIndexer: IWMIndexer): HRESULT; stdcall;
  _WMCreateBackupRestorer    : function(pCallback: IUnknown; out ppBackup: IWMLicenseBackup): HRESULT; stdcall;
  _WMCreateProfileManager    : function(out ppProfileManager: IWMProfileManager): HRESULT; stdcall;
  _WMCreateWriterFileSink    : function(out ppSink: IWMWriterFileSink ): HRESULT; stdcall;
  _WMCreateWriterNetworkSink : function(out ppSink: IWMWriterNetworkSink): HRESULT; stdcall;
  _WMCreateWriterPushSink    : function(out ppSink: IWMWriterPushSink): HRESULT; stdcall;

  function WMVCORELoaded: boolean;
  begin
    Result := (WMVCORELib <> 0);
  end;

  function UnLoadWMVCORE: Boolean;
  begin
    Result:= True;
    if WMVCORELoaded then
    begin
      Result := FreeLibrary(WMVCORELib);
      _WMCheckURLExtension       := nil;
      _WMCheckURLScheme          := nil;
      _WMValidateData            := nil;
      _WMIsAvailableOffline      := nil;
      _WMIsContentProtected      := nil;
      _WMCreateCertificate       := nil;
      _WMCreateWriter            := nil;
      _WMCreateReader            := nil;
      _WMCreateSyncReader        := nil;
      _WMCreateEditor            := nil;
      _WMCreateIndexer           := nil;
      _WMCreateBackupRestorer    := nil;
      _WMCreateProfileManager    := nil;
      _WMCreateWriterFileSink    := nil;
      _WMCreateWriterNetworkSink := nil;
      _WMCreateWriterPushSink    := nil;
      WMVCORELib := 0;
    end;
  end;

  function LoadWMVCORELib: boolean;
  begin
    Result := WMVCORELoaded;
    if (not Result) then
    begin
      WMVCORELib := LoadLibrary(WMVCORE);
      if WMVCORELoaded then
      begin
        _WMCheckURLExtension       := GetProcAddress(WMVCORELib, 'WMCheckURLExtension');
        _WMCheckURLScheme          := GetProcAddress(WMVCORELib, 'WMCheckURLScheme');
        _WMValidateData            := GetProcAddress(WMVCORELib, 'WMValidateData');
        _WMIsAvailableOffline      := GetProcAddress(WMVCORELib, 'WMIsAvailableOffline');
        _WMIsContentProtected      := GetProcAddress(WMVCORELib, 'WMIsContentProtected');
        _WMCreateCertificate       := GetProcAddress(WMVCORELib, 'WMCreateCertificate');
        _WMCreateWriter            := GetProcAddress(WMVCORELib, 'WMCreateWriter');
        _WMCreateReader            := GetProcAddress(WMVCORELib, 'WMCreateReader');
        _WMCreateSyncReader        := GetProcAddress(WMVCORELib, 'WMCreateSyncReader');
        _WMCreateEditor            := GetProcAddress(WMVCORELib, 'WMCreateEditor');
        _WMCreateIndexer           := GetProcAddress(WMVCORELib, 'WMCreateIndexer');
        _WMCreateBackupRestorer    := GetProcAddress(WMVCORELib, 'WMCreateBackupRestorer');
        _WMCreateProfileManager    := GetProcAddress(WMVCORELib, 'WMCreateProfileManager');
        _WMCreateWriterFileSink    := GetProcAddress(WMVCORELib, 'WMCreateWriterFileSink');
        _WMCreateWriterNetworkSink := GetProcAddress(WMVCORELib, 'WMCreateWriterNetworkSink');
        _WMCreateWriterPushSink    := GetProcAddress(WMVCORELib, 'WMCreateWriterPushSink');
        Result:= Assigned(_WMCheckURLExtension) and Assigned(_WMCheckURLScheme) and
          Assigned(_WMValidateData) and Assigned(_WMIsAvailableOffline) and
          Assigned(_WMIsContentProtected) and Assigned(_WMCreateCertificate) and
          Assigned(_WMCreateWriter) and Assigned(_WMCreateReader) and
          Assigned(_WMCreateSyncReader) and Assigned(_WMCreateEditor) and
          Assigned(_WMCreateIndexer) and Assigned(_WMCreateBackupRestorer) and
          Assigned(_WMCreateProfileManager) and Assigned(_WMCreateWriterFileSink) and
          Assigned(_WMCreateWriterNetworkSink) and Assigned(_WMCreateWriterPushSink);
        if not Result then UnLoadWMVCORE;
      end;
    end;
  end;

  function WMCheckURLExtension(pwszURL: PWideChar): HRESULT; stdcall;
  begin
    LoadWMVCORELib;
    Result := _WMCheckURLExtension(pwszURL);
  end;

  function WMCheckURLScheme(pwszURLScheme: PWideChar): HRESULT; stdcall;
  begin
    LoadWMVCORELib;
    Result := _WMCheckURLScheme(pwszURLScheme);
  end;

  function WMValidateData(pbData: PBYTE; var pdwDataSize: LongWord): HRESULT; stdcall;
  begin
    LoadWMVCORELib;
    Result := _WMValidateData(pbData, pdwDataSize);
  end;

  function WMIsAvailableOffline(pwszURL, pwszLanguage: PWideChar; out pfIsAvailableOffline: BOOL): HRESULT; stdcall;
  begin
    LoadWMVCORELib;
    Result := _WMIsAvailableOffline(pwszURL, pwszLanguage, pfIsAvailableOffline);
  end;

  function WMIsContentProtected(pwszFileName: PWideChar; out pfIsProtected: BOOL): HRESULT; stdcall;
  begin
    LoadWMVCORELib;
    Result := _WMIsContentProtected(pwszFileName, pfIsProtected);
  end;

  function WMCreateCertificate(out pUnkCert: IUnknown): HRESULT; stdcall;
  begin
    LoadWMVCORELib;
    Result := _WMCreateCertificate(pUnkCert);
  end;

  function WMCreateWriter(pUnkCert: IUnknown; out ppWriter: IWMWriter): HRESULT; stdcall;
  begin
    LoadWMVCORELib;
    Result := _WMCreateWriter(pUnkCert, ppWriter);
  end;

  function WMCreateReader(pUnkCert: IUnknown; dwRights: LongWord; out ppReader: IWMReader): HRESULT; stdcall;
  begin
    LoadWMVCORELib;
    Result := _WMCreateReader(pUnkCert, dwRights, ppReader);
  end;

  function WMCreateSyncReader(pUnkCert: IUnknown; dwRights: LongWord; out ppSyncReader: IWMSyncReader): HRESULT; stdcall;
  begin
    LoadWMVCORELib;
    Result := _WMCreateSyncReader(pUnkCert, dwRights, ppSyncReader);
  end;

  function WMCreateEditor(out ppEditor: IWMMetadataEditor): HRESULT; stdcall;
  begin
    LoadWMVCORELib;
    Result := WMCreateEditor(ppEditor);
  end;

  function WMCreateIndexer(out ppIndexer: IWMIndexer): HRESULT; stdcall;
  begin
    LoadWMVCORELib;
    Result := _WMCreateIndexer(ppIndexer);
  end;

  function WMCreateBackupRestorer(pCallback: IUnknown; out ppBackup: IWMLicenseBackup): HRESULT; stdcall;
  begin
    LoadWMVCORELib;
    Result := _WMCreateBackupRestorer(pCallback, ppBackup);
  end;

  function WMCreateProfileManager(out ppProfileManager: IWMProfileManager): HRESULT; stdcall;
  begin
    LoadWMVCORELib;
    Result := _WMCreateProfileManager(ppProfileManager);
  end;

  function WMCreateWriterFileSink(out ppSink: IWMWriterFileSink ): HRESULT; stdcall;
  begin
    LoadWMVCORELib;
    Result := _WMCreateWriterFileSink(ppSink);
  end;

  function WMCreateWriterNetworkSink(out ppSink: IWMWriterNetworkSink): HRESULT; stdcall;
  begin
    LoadWMVCORELib;
    Result := _WMCreateWriterNetworkSink(ppSink);
  end;

  function WMCreateWriterPushSink(out ppSink: IWMWriterPushSink): HRESULT; stdcall;
  begin
    LoadWMVCORELib;
    Result := _WMCreateWriterPushSink(ppSink);
  end;

{$ELSE}
  function WMCheckURLExtension; external WMVCORE name 'WMCheckURLExtension';
  function WMCheckURLScheme; external WMVCORE name 'WMCheckURLScheme';
  function WMValidateData; external WMVCORE name 'WMValidateData';
  function WMIsAvailableOffline; external WMVCORE name 'WMIsAvailableOffline';

  function WMIsContentProtected; external WMVCORE name 'WMIsContentProtected';
  function WMCreateCertificate; external WMVCORE name 'WMCreateCertificate';
  function WMCreateWriter; external WMVCORE name 'WMCreateWriter';
  function WMCreateReader; external WMVCORE name 'WMCreateReader';
  function WMCreateSyncReader; external WMVCORE name 'WMCreateSyncReader';
  function WMCreateEditor; external WMVCORE name 'WMCreateEditor';
  function WMCreateIndexer; external WMVCORE name 'WMCreateIndexer';
  function WMCreateBackupRestorer; external WMVCORE name 'WMCreateBackupRestorer';
  function WMCreateProfileManager; external WMVCORE name 'WMCreateProfileManager';
  function WMCreateWriterFileSink; external WMVCORE name 'WMCreateWriterFileSink';
  function WMCreateWriterNetworkSink; external WMVCORE name 'WMCreateWriterNetworkSink';
  function WMCreateWriterPushSink; external WMVCORE name 'WMCreateWriterPushSink';
{$ENDIF}

initialization
{$IFDEF WMF9_DYNAMIC_LINK}
  _WMCheckURLExtension       := nil;
  _WMCheckURLScheme          := nil;
  _WMValidateData            := nil;
  _WMIsAvailableOffline      := nil;
  _WMIsContentProtected      := nil;
  _WMCreateCertificate       := nil;
  _WMCreateWriter            := nil;
  _WMCreateReader            := nil;
  _WMCreateSyncReader        := nil;
  _WMCreateEditor            := nil;
  _WMCreateIndexer           := nil;
  _WMCreateBackupRestorer    := nil;
  _WMCreateProfileManager    := nil;
  _WMCreateWriterFileSink    := nil;
  _WMCreateWriterNetworkSink := nil;
  _WMCreateWriterPushSink    := nil;
{$ENDIF}

finalization
{$IFDEF WMF9_DYNAMIC_LINK}
  UnLoadWMVCORE;
{$ENDIF}

end.


