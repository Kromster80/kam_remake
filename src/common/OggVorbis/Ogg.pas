unit Ogg;

{************************************************************************}
{                                                                        }
{       Object Pascal Runtime Library                                    }
{       Ogg interface unit                                               }
{                                                                        }
{ The original file is: ogg/ogg.h, released June 2001.                   }
{ The original Pascal code is: Ogg.pas, released 28 Jul 2001.            }
{ The initial developer of the Pascal code is Matthijs Laan              }
{ (matthijsln@xs4all.nl).                                                }
{                                                                        }
{ Portions created by Matthijs Laan are                                  }
{ Copyright (C) 2001 Matthijs Laan.                                      }
{                                                                        }
{ Portions created by Xiph.org are                                       }
{ Copyright (C) 1994-2001 by Xiph.org http://www.xiph.org/               }
{                                                                        }
{       Obtained through:                                                }
{                                                                        }
{       Joint Endeavour of Delphi Innovators (Project JEDI)              }
{                                                                        }
{ You may retrieve the latest version of this file at the Project        }
{ JEDI home page, located at http://delphi-jedi.org                      }
{                                                                        }
{ The contents of this file are used with permission, subject to         }
{ the Mozilla Public License Version 1.1 (the "License"); you may        }
{ not use this file except in compliance with the License. You may       }
{ obtain a copy of the License at                                        }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                }
{                                                                        }
{ Software distributed under the License is distributed on an            }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or         }
{ implied. See the License for the specific language governing           }
{ rights and limitations under the License.                              }
{                                                                        }
{************************************************************************}

interface

uses OSTypes;

const
{$IFDEF WIN32}
  OggLib = 'ogg.dll';
{$ENDIF WIN32}

{$IFDEF UNIX}
  OggLib = 'libogg.so';
{$ENDIF UNIX}

{$IFDEF FPC}
  {$IFDEF WIN32}
    {$PACKRECORDS C}
  {$ENDIF WIN32}
{$ENDIF FPC}

{ ogg/ogg.h }

type
  p_oggpack_buffer = ^oggpack_buffer;
    oggpack_buffer = record
      endbyte: long;
      endbit: int;
      buffer: PChar;
      ptr: PChar;
      storage: long;
    end;

(* ogg_page is used to encapsulate the data in one Ogg bitstream page *****)

  p_ogg_page = ^ogg_page;
    ogg_page = record
      header: Pointer;
      header_len: long;
      body: Pointer;
      body_len: long;
    end;

(* ogg_stream_state contains the current encode/decode state of a logical
   Ogg bitstream **********************************************************)

  p_ogg_stream_state = ^ogg_stream_state;
    ogg_stream_state = record
      body_data: PChar;          (* bytes from packet bodies *)
      body_storage: long;        (* storage elements allocated *)
      body_fill: long;           (* elements stored; fill mark *)
      body_returned: long;       (* elements of fill returned *)

      lacing_vals: p_int_array;                (* The values that will go to the segment table *)
      granule_vals: Pointer; { ogg_int64_t * } (* granulepos values for headers. Not compact
                                                  this way, but it is simple coupled to the
                                                  lacing fifo *)
      lacing_storage: long;
      lacing_fill: long;
      lacing_packet: long;
      lacing_returned: long;

      header: array[0..282] of Char;   (* working space for header encode *)
      header_fill: int;
      e_o_s: int;                      (* set when we have buffered the last packet in the
                                          logical bitstream *)
      b_o_s: int;                      (* set after we've written the initial page
                                          of a logical bitstream *)
      serialno: long;
      pageno: long;
      packetno: ogg_int64_t;           (* sequence number for decode; the framing
                                          knows where there's a hole in the data,
                                          but we need coupling so that the codec
                                          (which is in a seperate abstraction
                                          layer) also knows about the gap *)
      granulepos: ogg_int64_t;
    end;

(* ogg_packet is used to encapsulate the data and metadata belonging
   to a single raw Ogg/Vorbis packet *************************************)

  p_ogg_packet = ^ogg_packet;
    ogg_packet = record
      packet: PChar;
      bytes: long;
      b_o_s: long;
      e_o_s: long;

      granulepos: ogg_int64_t;

      packetno: ogg_int64_t;        (* sequence number for decode; the framing
                                       knows where there's a hole in the data,
                                       but we need coupling so that the codec
                                       (which is in a seperate abstraction
                                       layer) also knows about the gap *)
    end;

  p_ogg_sync_state = ^ogg_sync_state;
    ogg_sync_state = record
      data: PChar;
      storage: int;
      fill: int;
      returned: int;

      unsynced: int;
      headerbytes: int;
      bodybytes: int;
    end;

(* Ogg BITSTREAM PRIMITIVES: bitstream ************************)

procedure oggpack_writeinit(var b: oggpack_buffer); cdecl; external OggLib;
procedure oggpack_reset(var b: oggpack_buffer); cdecl; external OggLib;
procedure oggpack_writeclear(var b: oggpack_buffer); cdecl; external OggLib;
procedure oggpack_readinit(var b: oggpack_buffer; buf: PChar; bytes: int); cdecl; external OggLib;
procedure oggpack_write(var b: oggpack_buffer; value: unsigned_long; bits: int); cdecl; external OggLib;
function oggpack_look(var b: oggpack_buffer; bits: int): long; cdecl; external OggLib;
function oggpack_look_huff(var b: oggpack_buffer; bits: int): long; cdecl; external OggLib;
function oggpack_look1(var b: oggpack_buffer): long; cdecl; external OggLib;
procedure oggpack_adv(var b: oggpack_buffer; bits: int); cdecl; external OggLib;
function oggpack_adv_huff(var b: oggpack_buffer; bits: int): int; cdecl; external OggLib;
procedure oggpack_adv1(var b: oggpack_buffer); cdecl; external OggLib;
function oggpack_read(var b: oggpack_buffer; bits: int): long; cdecl; external OggLib;
function oggpack_read1(var b: oggpack_buffer): long; cdecl; external OggLib;
function oggpack_bytes(var b: oggpack_buffer): long; cdecl; external OggLib;
function oggpack_bits(var b: oggpack_buffer): long; cdecl; external OggLib;
function oggpack_get_buffer(var b: oggpack_buffer): PChar; cdecl; external OggLib;


(* Ogg BITSTREAM PRIMITIVES: encoding **************************)

function ogg_stream_packetin(var os: ogg_stream_state; var op: ogg_packet): int; cdecl; external OggLib;
function ogg_stream_pageout(var os: ogg_stream_state; var og: ogg_page): int; cdecl; external OggLib;
function ogg_stream_flush(var os: ogg_stream_state; var og: ogg_page): int; cdecl; external OggLib;

(* Ogg BITSTREAM PRIMITIVES: decoding **************************)

function ogg_sync_init(var oy: ogg_sync_state): int; cdecl; external OggLib;
function ogg_sync_clear(var oy: ogg_sync_state): int; cdecl; external OggLib;
function ogg_sync_reset(var oy: ogg_sync_state): int; cdecl; external OggLib;
function ogg_sync_destroy(var oy: ogg_sync_state): int; cdecl; external OggLib;

function ogg_sync_buffer(var oy: ogg_sync_state; size: long): PChar; cdecl; external OggLib;
function ogg_sync_wrote(var oy: ogg_sync_state; bytes: long): int; cdecl; external OggLib;
function ogg_sync_pageseek(var oy: ogg_sync_state; var og: ogg_page): long; cdecl; external OggLib;
function ogg_sync_pageout(var oy: ogg_sync_state; var og: ogg_page): int; cdecl; external OggLib;
function ogg_stream_pagein(var os: ogg_stream_state; var og: ogg_page): int; cdecl; external OggLib;
function ogg_stream_packetout(var os: ogg_stream_state; var op: ogg_packet): int; cdecl; external OggLib;
function ogg_stream_packetpeek(var os: ogg_stream_state; var op: ogg_packet): int; cdecl; external OggLib; { New since RC1 }

(* Ogg BITSTREAM PRIMITIVES: general ***************************)

function ogg_stream_init(var os: ogg_stream_state; serialno: int): int; cdecl; external OggLib;
function ogg_stream_clear(var os: ogg_stream_state): int; cdecl; external OggLib;
function ogg_stream_reset(var os: ogg_stream_state): int; cdecl; external OggLib;
function ogg_stream_destroy(var os: ogg_stream_state): int; cdecl; external OggLib;
function ogg_stream_eos(var os: ogg_stream_state): int; cdecl; external OggLib;

function ogg_page_version(var og: ogg_page): int; cdecl; external OggLib;
function ogg_page_continued(var og: ogg_page): int; cdecl; external OggLib;
function ogg_page_bos(var og: ogg_page): int; cdecl; external OggLib;
function ogg_page_eos(var og: ogg_page): int; cdecl; external OggLib;
function ogg_page_granulepos(var og: ogg_page): ogg_int64_t; cdecl; external OggLib;
function ogg_page_serialno(var og: ogg_page): int; cdecl; external OggLib;
function ogg_page_pageno(var og: ogg_page): long; cdecl; external OggLib;
function ogg_page_packets(var og: ogg_page): int; cdecl; external OggLib;

procedure ogg_packet_clear(var op: ogg_packet); cdecl; external OggLib;

implementation

end.