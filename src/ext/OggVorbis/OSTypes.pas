unit OSTypes;

{************************************************************************}
{                                                                        }
{       Object Pascal Runtime Library                                    }
{       Ogg OS types interface unit                                      }
{                                                                        }
{ The original file is: ogg/os_types.h, released June 2001.              }
{ The original Pascal code is: OSTypes.pas, released 28 Jul 2001.        }
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

{ Not all the following types are defined in os_types.h; some are an aid
  for header conversion }

type
  size_t         = cardinal;
  int            = integer;
  long           = integer;
  unsigned_long  = cardinal;
  float          = single;
  signed_char    = shortint;

{ ogg/os_types.h }

  ogg_int64_t  = int64;         { -9223372036854775808..9223372036854775807 } // Tssskk... Delphi can't handle subranges here
  ogg_int32_t  = longint;       { 2147483648..2147483647 }
  ogg_uint32_t = longword;      { 0..4294967295 }                             // Here Delphi can but FPC fails...
  ogg_int16_t  = -32768..32767; { also smallint }

  p_int  = ^int;

{ Array types  }

  p_pchar_array = ^t_pchar_array;
  t_pchar_array = packed array[0..maxint div SizeOf(PChar)-1] of PChar;

  p_int_array = ^t_int_array;
  t_int_array = packed array[0..maxint div SizeOf(int)-1] of int;

  p_float_array = ^t_float_array;
  t_float_array = packed array[0..maxint div SizeOf(float)-1] of float;

  p_p_float_p_float_array = ^p_float_p_float_array;
  p_float_p_float_array = ^t_float_p_float_array;
  t_float_p_float_array = packed array[0..maxint div SizeOf(p_float_array)-1] of p_float_array;

  p_signed_char_array = ^t_signed_char_array;
  t_signed_char_array = packed array[0..maxint div SizeOf(signed_char)-1] of signed_char;

implementation

end.