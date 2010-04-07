{==============================================================================}
{                                                                              }
{       OpenGL 3.0 - Headertranslation (includes GL 1.1 - 3.0)                 }
{       Version 3.0.2                                                          }
{       Date : 08.01.2009                                                      }
{                                                                              }
{       Works with :                                                           }
{        - Delphi 3 and up                                                     }
{        - FreePascal (1.9.3 and up)                                           }
{                                                                              }
{==============================================================================}
{                                                                              }
{       Containts the translations of glext.h, gl_1_1.h, glu.h and weglext.h.  }
{       It also contains some helperfunctions that were inspired by those      }
{       found in Mike Lischke's OpenGL12.pas.                                  }
{                                                                              }
{       Copyright (C) DGL-OpenGL2-Portteam                                     }
{       All Rights Reserved                                                    }
{                                                                              }
{       Obtained through:                                                      }
{       Delphi OpenGL Community(DGL) - www.delphigl.com                        }
{                                                                              }
{       Converted and maintained by DGL's GL2.0-Team :                         }
{         - Sascha Willems                - http://www.delphigl.de             }
{         - Steffen Xonna (Lossy eX)      - http://www.dev-center.de           }
{         - Lars Middendorf               - http://www.3d-seite.de             }
{       Additional input :                                                     }
{         - Martin Waldegger (Mars)       - http://www.basegraph.com           }
{         - Benjamin Rosseaux (BeRo)      - http://www.0ok.de                  }
{       Additional thanks:                                                     }
{           sigsegv (libdl.so)                                                 }
{                                                                              }
{                                                                              }
{==============================================================================}
{ You may retrieve the latest version of this file at the Delphi OpenGL        }
{ Community home page, located at http://www.delphigl.com/                     }
{                                                                              }
{ The contents of this file are used with permission, subject to               }
{ the Mozilla Public License Version 1.1 (the "License"); you may              }
{ not use this file except in compliance with the License. You may             }
{ obtain a copy of the License at                                              }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an                  }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or               }
{ implied. See the License for the specific language governing                 }
{ rights and limitations under the License.                                    }
{                                                                              }
{==============================================================================}
{ History :                                                                    }
{ Version 1.0    Initial Release                                               }
{ Version 1.1    Added PPointer in Tpyessection for compatiblity with Delphi   }
{                versions lower than 7                                    (SW) }
{                Added a function named RaiseLastOSError including a comment   }
{                on how to make it run under Delphi versions lower than 7 (SW) }
{                Added some data types according to the GL-Syntax         (SW) }
{ Version 1.2    Fixed some problems with getting the addresses of some        }
{                Extensions (e.g. glTexImage3D) where the EXT/ARB did work     }
{                but not the core-functions                               (SW) }
{ Version 1.3    A second call to ReadimplementationProperties won't           }
{                revert to the default libs anymore                       (MW) }
{                Libraries now will be released if necessary              (MW) }
{ Version 1.3a   Small fixes for glSlang-functions                        (SW) }
{ Version 1.3b   Fixed a small bug with GL_ARB_shader_objects, that lead       }
{                lead to that extension not loaded correctly              (SW) }
{ Version 1.3c   more GL 1.5 compliance by FOG_COORD_xx and                    }
{                ARB less VBO and occlusion query routines                (MW) }
{ Version 1.3d   Fixed linebreaks (should now be corrected under D5)      (SW) }
{ Version 1.4    Changed header to correspond to the OpenGL-Shading            }
{                Language specification 1.10 :                                 }
{                - Added new GL_SAMPLER_*-Constants                            }
{                - Added Constant GL_SHADING_LANGUAGE_VERSION_ARB              }
{                - Added Constant GL_FRAGMENT_SHADER_DERIVATIVE_HINT_ARB       }
{                - Added Constant GL_MAX_FRAGMENT_UNIFORM_COMPONENTS_ARB  (SW) }
{ Version 1.4a   Fixed a missing stdcall for glBindAttribLocationARB      (SW) }
{ Version 1.4b   Fixed declaration for glUniform*(f/i)vARB (added count)  (MW) }
{                glCompileShaderARB changed from function to procedure    (MW) }
{ Version 1.5    Added support for FreePascal                             (BR) }
{                Added type TGLVectorf3/TGLVector3f                       (SW) }
{ Version 1.6    Added Extension GL_EXT_framebuffer_object                (SX) }
{ Version 1.7    Added Extension GL_ARB_fragment_program_shadow           (SX) }
{                Added Extension GL_ARB_draw_buffers                      (SX) }
{                Added Extension GL_ARB_texture_rectangle                 (SX) }
{                Added Extension GL_ARB_color_buffer_float                (SX) }
{                Added Extension GL_ARB_half_float_pixel                  (SX) }
{                Added Extension GL_ARB_texture_float                     (SX) }
{                Added Extension GL_ARB_pixel_buffer_object               (SX) }
{                Added Extension GL_EXT_depth_bounds_test                 (SX) }
{                Added Extension GL_EXT_texture_mirror_clamp              (SX) }
{                Added Extension GL_EXT_blend_equation_separate           (SX) }
{                Added Extension GL_EXT_pixel_buffer_object               (SX) }
{                Added Extension GL_EXT_texture_compression_dxt1          (SX) }
{                Added Extension GL_NV_fragment_program_option            (SX) }
{                Added Extension GL_NV_fragment_program2                  (SX) }
{                Added Extension GL_NV_vertex_program2_option             (SX) }
{                Added Extension GL_NV_vertex_program3                    (SX) }
{ Version 1.8    Added explicit delegate type definitions                 (LM) }
{                Added .Net 1.1 Support                                   (LM) }
{                Added .Net overloaded functions                          (LM) }
{                Added delayed extension loading and stubs                (LM) }
{                Added automatic InitOpenGL call in CreateRenderingContext(LM) }
{                Added extra Read_* function                              (LM) }
{ Version 2.0    fixed some Problem with version string and damn drivers.      }
{                String 1.15 identified as OpenGL 1.5 not as OpenGL 1.1   (SX) }
{                Removed unexisting extension GL_ARB_texture_mirror_repeat(SX) }
{                Added Extension WGL_ARB_pixel_format_float               (SX) }
{                Added Extension GL_EXT_stencil_clear_tag                 (SX) }
{                Added Extension GL_EXT_texture_rectangle                 (SX) }
{                Added Extension GL_EXT_texture_edge_clamp                (SX) }
{                Some 1.5 Core Consts added (now completed)               (SX) }
{                gluProject need pointer for not .net                     (SX) }
{                gluUnProject need pointer for not .net                   (SX) }
{                wglUseFontOutlines* need pointer for not .net            (SX) }
{                wglSwapMultipleBuffers need pointer for not .net         (SX) }
{                Bug with wglGetExtensionsStringEXT removed                    }
{                different type for .net                                  (SX) }
{                Added OpenGL 2.0 Core                                    (SX) }
{ Version 2.0.1  fixed some problems with glGetActiveAttrib in 2.0 Core   (SX) }
{                fixes some problems with gluProject                      (SX) }
{                fixes some problems with gluUnProject                    (SX) }
{                fixes some problems with gluTessVertex                   (SX) }
{                fixes some problems with gluLoadSamplingMatrices         (SX) }
{ Version 2.1    Removed .NET Support                                     (SX) }
{                Better support for Linux                                 (SX) }
{                Better Codeformation                                     (SX) }
{                Added some more Vector/Matrix types                      (SX) }
{                Added OpenGL 2.1 Core                                    (SX) }
{                Added Extension GL_EXT_packed_depth_stencil              (SX) }
{                Added Extension GL_EXT_texture_sRGB                      (SX) }
{                Added Extension GL_EXT_framebuffer_blit                  (SX) }
{                Added Extension GL_EXT_framebuffer_multisample           (SX) }
{                Added Extension GL_EXT_timer_query                       (SX) }
{                Added Extension GL_EXT_gpu_program_parameters            (SX) }
{                Added Extension GL_EXT_bindable_uniform                  (SX) }
{                Added Extension GL_EXT_draw_buffers2                     (SX) }
{                Added Extension GL_EXT_draw_instanced                    (SX) }
{                Added Extension GL_EXT_framebuffer_sRGB                  (SX) }
{                Added Extension GL_EXT_geometry_shader4                  (SX) }
{                Added Extension GL_EXT_gpu_shader4                       (SX) }
{                Added Extension GL_EXT_packed_float                      (SX) }
{                Added Extension GL_EXT_texture_array                     (SX) }
{                Added Extension GL_EXT_texture_buffer_object             (SX) }
{                Added Extension GL_EXT_texture_compression_latc          (SX) }
{                Added Extension GL_EXT_texture_compression_rgtc          (SX) }
{                Added Extension GL_EXT_texture_integer                   (SX) }
{                Added Extension GL_EXT_texture_shared_exponent           (SX) }
{                Added Extension GL_NV_depth_buffer_float                 (SX) }
{                Added Extension GL_NV_fragment_program4                  (SX) }
{                Added Extension GL_NV_framebuffer_multisample_coverage   (SX) }
{                Added Extension GL_NV_geometry_program4                  (SX) }
{                Added Extension GL_NV_gpu_program4                       (SX) }
{                Added Extension GL_NV_parameter_buffer_object            (SX) }
{                Added Extension GL_NV_transform_feedback                 (SX) }
{                Added Extension GL_NV_vertex_program4                    (SX) }
{ Version 3.0    fixed some const of GL_EXT_texture_shared_exponent       (SX) }
{                possible better support for mac                          (SX) }
{                Added OpenGL 3.0 Core                                    (SX) }
{                Added Extension GL_ARB_depth_buffer_float                (SX) }
{                Added Extension GL_ARB_draw_instanced                    (SX) }
{                Added Extension GL_ARB_framebuffer_object                (SX) }
{                Added Extension GL_ARB_framebuffer_sRGB                  (SX) }
{                Added Extension GL_ARB_geometry_shader4                  (SX) }
{                Added Extension GL_ARB_half_float_vertex                 (SX) }
{                Added Extension GL_ARB_instanced_arrays                  (SX) }
{                Added Extension GL_ARB_map_buffer_range                  (SX) }
{                Added Extension GL_ARB_texture_buffer_object             (SX) }
{                Added Extension GL_ARB_texture_compression_rgtc          (SX) }
{                Added Extension GL_ARB_texture_rg                        (SX) }
{                Added Extension GL_ARB_vertex_array_object               (SX) }
{                Added Extension GL_NV_conditional_render                 (SX) }
{                Added Extension GL_NV_present_video                      (SX) }
{                Added Extension GL_EXT_transform_feedback                (SX) }
{                Added Extension GL_EXT_direct_state_access               (SX) }
{                Added Extension GL_EXT_vertex_array_bgra                 (SX) }
{                Added Extension GL_EXT_texture_swizzle                   (SX) }
{                Added Extension GL_NV_explicit_multisample               (SX) }
{                Added Extension GL_NV_transform_feedback2                (SX) }
{                Added Extension WGL_ARB_create_context                   (SX) }
{                Added Extension WGL_NV_present_video                     (SX) }
{                Added Extension WGL_NV_video_out                         (SX) }
{                Added Extension WGL_NV_swap_group                        (SX) }
{                Added Extension WGL_NV_gpu_affinity                      (SX) }
{                Added define DGL_TINY_HEADER to suppress automatic            }
{                function loading                                         (SX) }
{                glProcedure renamed to dglGetProcAddress and now it's         }
{                visible from outside the unit to custom load functions   (SX) }
{                dglCheckExtension added to check if an extension exists  (SX) }
{                Read_GL_ARB_buffer_object renamed to                          }
{                Read_GL_ARB_vertex_buffer_object                         (SX) }
{ Version 3.0.1  fixed an problem with fpc                                (SX) }
{ Version 3.0.2  fixed an problem with WGL_ARB_create_context             (SX) }
{==============================================================================}


{.$define DGL_TINY_HEADER}
{
  If you enable the define DGL_TINY_HEADER no function automatically will be loaded if you
  call ActivateRenderingContext. This may some bit faster and the smart linker can delete
  all non used functions. This will reduce the filesize of your binary file. But in this
  case you have to load the functions by yourself. There are two ways to do this.
  
  1. You can load whole extension by calling the func Read_Extensionname. But if you do
     this it's possible to load functions you dont use. So you have the same "problem"
     like before. But it's only an bit smaler.
     > Read_GL_ARB_multitexture;

  2. You are able to load only the functions you exactly need. In this case you are able
     to use the variables of the dglOpenGL.pas. So you only need to load the functions
     and you can use the header like before.
     To do this you have to created and activated an opengl context and than you can load
     the needed functions.
     > ActivateRenderingContext(fDC, fRC);
     > glActiveTextureARB := dglGetProcAddress('glActiveTextureARB');
     > glMultiTexCoord2fARB := dglGetProcAddress('glMultiTexCoord2fARB');

     So only the function "glActiveTextureARB" and "glMultiTexCoord2fARB" will be loaded.


  Please notice that the extension variables won't be loaded if this define is active. But
  you can call dglCheckExtension to check if any extension exists. You can assign them to
  the variables of the dglOpenGL.pas so all code they use this will find them.

  > GL_ARB_shading_language_100 := dglCheckExtension('GL_ARB_shading_language_100');
}


unit dglOpenGL;
{$IFDEF FPC}
  {$MODE Delphi}

  {$IFDEF CPUI386}
    {$DEFINE CPU386}
    {$ASMMODE INTEL}
  {$ENDIF}

  {$IFNDEF WIN32}
    {$LINKLIB c}
  {$ENDIF}
{$ENDIF}

interface

{$H+,O+,X+}

{$IFDEF VER140}   // Delphi 6
  {$DEFINE DELPHI6_UP}
{$ENDIF}

{$IFDEF VER150}   // Delphi 7
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
{$ENDIF}

{$IFDEF VER160}   // Delphi 8
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
{$ENDIF}

{$IFDEF VER170}   // Delphi 2005
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
{$ENDIF}

{$IFDEF VER180}   // Delphi 2006
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
{$ENDIF}

{$IFDEF VER185}   // Delphi 2006
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
{$ENDIF}

{$IFDEF VER190}   // Delphi 2007 .Net
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
{$ENDIF}

{$IFDEF VER200}   // Delphi 2009
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
{$ENDIF}

{$IFDEF DELPHI6_UP}
  {$A4}
{$ELSE}
  {$A+}
{$ENDIF}

{$IFDEF DELPHI7_UP}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}


uses
  SysUtils{$IFDEF Win32}, Windows{$ENDIF};

type
  // Needed for Delphi 6 and less (defined in system.pas for Delphi 7)
  PPointer = ^Pointer;

  TGLenum = Cardinal;
  TGLboolean = BYTEBOOL;
  TGLbitfield = Cardinal;
  TGLbyte = Shortint;
  TGLshort = SmallInt;
  TGLint = Integer;
  TGLsizei = Integer;
  TGLubyte = Byte;
  TGLushort = Word;
  TGLuint = Cardinal;
  TGLfloat = Single;
  TGLclampf = Single;
  TGLdouble = Double;
  TGLclampd = Double;
  TGLvoid = Pointer;
  TGLint64 = Int64;
  TGLuint64 = {$IFDEF DELPHI7_UP}UInt64{$ELSE}Int64{$ENDIF};

  GLenum = TGLenum;
  GLboolean = TGLboolean;
  GLbitfield = TGLbitfield;
  GLbyte = TGLbyte;
  GLshort = TGLshort;
  GLint = TGLint;
  GLsizei = TGLsizei;
  GLubyte = TGLubyte;
  GLushort = TGLushort;
  GLuint = TGLuint;
  GLfloat = TGLfloat;
  GLclampf = TGLclampf;
  GLdouble = TGLdouble;
  GLclampd = TGLclampd;
  GLvoid = TGLvoid;
  GLint64 = TGLint64;
  GLuint64 = TGLuint64;

  PGLboolean = ^TGLboolean;
  PGLbyte = ^TGLbyte;
  PGLshort = ^TGLshort;
  PGLint = ^TGLint;
  PGLsizei = ^TGLsizei;
  PGLubyte = ^TGLubyte;
  PGLushort = ^TGLushort;
  PGLuint = ^TGLuint;
  PGLclampf = ^TGLclampf;
  PGLfloat = ^TGLfloat;
  PGLdouble = ^TGLdouble;
  PGLclampd = ^TGLclampd;
  PGLenum = ^TGLenum;
  PGLvoid = Pointer;
  PGLint64 = ^TGLint64;
  PPGLvoid = ^PGLvoid;

  // GL_NV_half_float
  TGLhalfNV = WORD;
  PGLhalfNV = ^TGLhalfNV;

  // GL_ARB_shader_objects
  PGLHandleARB = ^GLHandleARB;
  GLHandleARB = Integer;
  PPGLCharARB = ^PChar;
  PGLCharARB = PChar;
  GLCharARB = Char;

  // GL_VERSION_2_0
  GLHandle = Integer;
  PPGLChar = ^PChar;

  // GL_EXT_timer_query
  GLint64EXT = Int64;
  TGLint64EXT = GLint64EXT;
  PGLint64EXT = ^TGLint64EXT;

  GLuint64EXT = TGLuint64;
  TGLuint64EXT = GLuint64EXT;
  PGLuint64EXT = ^TGLuint64EXT;

  // Datatypes corresponding to GL's types TGL(name)(type)(count)
  TGLVectorub2 = array[0..1] of TGLubyte;
  TGLVectori2  = array[0..1] of TGLint;
  TGLVectorf2  = array[0..1] of TGLfloat;
  TGLVectord2  = array[0..1] of TGLdouble;
  TGLVectorp2  = array[0..1] of Pointer;

  TGLVectorub3 = array[0..2] of TGLubyte;
  TGLVectori3  = array[0..2] of TGLint;
  TGLVectorf3  = array[0..2] of TGLfloat;
  TGLVectord3  = array[0..2] of TGLdouble;
  TGLVectorp3  = array[0..2] of Pointer;

  TGLVectorub4 = array[0..3] of TGLubyte;
  TGLVectori4  = array[0..3] of TGLint;
  TGLVectorf4  = array[0..3] of TGLfloat;
  TGLVectord4  = array[0..3] of TGLdouble;
  TGLVectorp4  = array[0..3] of Pointer;

  TGLArrayf4 = TGLVectorf4;
  TGLArrayf3 = TGLVectorf3;
  TGLArrayd3 = TGLVectord3;
  TGLArrayi4 = TGLVectori4;
  TGLArrayp4 = TGLVectorp4;

  TGlMatrixub3 = array[0..2, 0..2] of TGLubyte;
  TGlMatrixi3  = array[0..2, 0..2] of TGLint;
  TGLMatrixf3  = array[0..2, 0..2] of TGLfloat;
  TGLMatrixd3  = array[0..2, 0..2] of TGLdouble;

  TGlMatrixub4 = array[0..3, 0..3] of TGLubyte;
  TGlMatrixi4  = array[0..3, 0..3] of TGLint;
  TGLMatrixf4  = array[0..3, 0..3] of TGLfloat;
  TGLMatrixd4  = array[0..3, 0..3] of TGLdouble;

  TGLVector3f = TGLVectorf3;

  // Datatypes corresponding to OpenGL12.pas for easy porting
  TVector3d = TGLVectord3;

  TVector4i = TGLVectori4;
  TVector4f = TGLVectorf4;
  TVector4p = TGLVectorp4;

  TMatrix4f = TGLMatrixf4;
  TMatrix4d = TGLMatrixd4;

  PGLMatrixd4 = ^TGLMatrixd4;
  PVector4i = ^TVector4i;

  // WGL_ARB_pbuffer
  HPBUFFERARB = THandle;

  // WGL_EXT_pbuffer
  HPBUFFEREXT = THandle;
  
  // WGL_NV_present_video
  PHVIDEOOUTPUTDEVICENV = ^HVIDEOOUTPUTDEVICENV;
  HVIDEOOUTPUTDEVICENV = THandle;

  // WGL_NV_video_out
  PHPVIDEODEV = ^HPVIDEODEV;
  HPVIDEODEV = THandle;

  // WGL_NV_gpu_affinity
  PHPGPUNV = ^HPGPUNV;
  PHGPUNV = ^HGPUNV;

  HPGPUNV = THandle;
  HGPUNV = THandle;


//{$IFDEF FPC}
//  TRect = packed record
//    Left, Top, Right, Bottom: Longint;
//  end;
//{$ENDIF}


  PGPU_DEVICE = ^GPU_DEVICE;
  GPU_DEVICE = record
    cb: DWORD;
    DeviceName: array [0..31] of char;
    DeviceString: array [0..127] of char;
    Flags: DWORD;
    rcVirtualScreen: TRect;
  end;
  

type
{$IFDEF FPC}
  {$IFDEF WIN32}
    PWGLSwap = ^TWGLSwap;
    {$EXTERNALSYM _WGLSWAP}
      _WGLSWAP = packed record
        hdc: HDC;
        uiFlags: UINT;
      end;

    TWGLSwap = _WGLSWAP;
  {$EXTERNALSYM WGLSWAP}
    WGLSWAP = _WGLSWAP;

  {$ENDIF}
{$ENDIF}

  // GLU types
  TGLUNurbs = record
  end;
  TGLUQuadric = record
  end;
  TGLUTesselator = record
  end;
  PGLUNurbs = ^TGLUNurbs;
  PGLUQuadric = ^TGLUQuadric;
  PGLUTesselator = ^TGLUTesselator;
  // backwards compatibility
  TGLUNurbsObj = TGLUNurbs;
  TGLUQuadricObj = TGLUQuadric;
  TGLUTesselatorObj = TGLUTesselator;
  TGLUTriangulatorObj = TGLUTesselator;
  PGLUNurbsObj = PGLUNurbs;
  PGLUQuadricObj = PGLUQuadric;
  PGLUTesselatorObj = PGLUTesselator;
  PGLUTriangulatorObj = PGLUTesselator;

  // GLUQuadricCallback
  TGLUQuadricErrorProc = procedure(errorCode: TGLEnum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  // GLUTessCallback
  TGLUTessBeginProc = procedure(AType: TGLEnum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TGLUTessEdgeFlagProc = procedure(Flag: TGLboolean); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TGLUTessVertexProc = procedure(VertexData: Pointer); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TGLUTessEndProc = procedure; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TGLUTessErrorProc = procedure(ErrNo: TGLEnum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TGLUTessCombineProc = procedure(Coords: TGLArrayd3; VertexData: TGLArrayp4; Weight: TGLArrayf4; OutData: PPointer); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TGLUTessBeginDataProc = procedure(AType: TGLEnum; UserData: Pointer); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TGLUTessEdgeFlagDataProc = procedure(Flag: TGLboolean; UserData: Pointer); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TGLUTessVertexDataProc = procedure(VertexData: Pointer; UserData: Pointer); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TGLUTessEndDataProc = procedure(UserData: Pointer); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TGLUTessErrorDataProc = procedure(ErrNo: TGLEnum; UserData: Pointer); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TGLUTessCombineDataProc = procedure(Coords: TGLArrayd3; VertexData: TGLArrayp4; Weight: TGLArrayf4; OutData: PPointer; UserData: Pointer); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  // GLUNurbsCallback
  TGLUNurbsErrorProc = procedure(ErrorCode: TGLEnum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

var
  GL_VERSION_1_0,
  GL_VERSION_1_1,
  GL_VERSION_1_2,
  GL_VERSION_1_3,
  GL_VERSION_1_4,
  GL_VERSION_1_5,
  GL_VERSION_2_0,
  GL_VERSION_2_1,
  GL_VERSION_3_0,
  GLU_VERSION_1_1,
  GLU_VERSION_1_2,
  GLU_VERSION_1_3,
  GL_3DFX_multisample,
  GL_3DFX_tbuffer,
  GL_3DFX_texture_compression_FXT1,
  GL_APPLE_client_storage,
  GL_APPLE_element_array,
  GL_APPLE_fence,
  GL_APPLE_specular_vector,
  GL_APPLE_transform_hint,
  GL_APPLE_vertex_array_object,
  GL_APPLE_vertex_array_range,
  GL_APPLE_ycbcr_422,
  GL_ARB_depth_texture,
  GL_ARB_fragment_program,
  GL_ARB_imaging,
  GL_ARB_matrix_palette,
  GL_ARB_multisample,
  GL_ARB_multitexture,
  GL_ARB_point_parameters,
  GL_ARB_shadow,
  GL_ARB_shadow_ambient,
  GL_ARB_texture_border_clamp,
  GL_ARB_texture_compression,
  GL_ARB_texture_cube_map,
  GL_ARB_texture_env_add,
  GL_ARB_texture_env_combine,
  GL_ARB_texture_env_crossbar,
  GL_ARB_texture_env_dot3,
  GL_ARB_texture_mirrored_repeat,
  GL_ARB_transpose_matrix,
  GL_ARB_vertex_blend,
  GL_ARB_vertex_buffer_object,
  GL_ARB_vertex_program,
  GL_ARB_window_pos,
  GL_ARB_shader_objects,
  GL_ARB_vertex_shader,
  GL_ARB_fragment_shader,
  GL_ARB_shading_language_100,
  GL_ARB_occlusion_query,
  GL_ARB_texture_non_power_of_two,
  GL_ARB_point_sprite,
  GL_ARB_fragment_program_shadow,
  GL_ARB_draw_buffers,
  GL_ARB_texture_rectangle,
  GL_ARB_color_buffer_float,
  GL_ARB_half_float_pixel,
  GL_ARB_texture_float,
  GL_ARB_pixel_buffer_object,
  GL_ARB_depth_buffer_float,
  GL_ARB_draw_instanced,
  GL_ARB_framebuffer_object,
  GL_ARB_framebuffer_sRGB,
  GL_ARB_geometry_shader4,
  GL_ARB_half_float_vertex,
  GL_ARB_instanced_arrays,
  GL_ARB_map_buffer_range,
  GL_ARB_texture_buffer_object,
  GL_ARB_texture_compression_rgtc,
  GL_ARB_texture_rg,
  GL_ARB_vertex_array_object,
  GL_ATI_draw_buffers,
  GL_ATI_element_array,
  GL_ATI_envmap_bumpmap,
  GL_ATI_fragment_shader,
  GL_ATI_map_object_buffer,
  GL_ATI_pn_triangles,
  GL_ATI_separate_stencil,
  GL_ATI_text_fragment_shader,
  GL_ATI_texture_env_combine3,
  GL_ATI_texture_float,
  GL_ATI_texture_mirror_once,
  GL_ATI_vertex_array_object,
  GL_ATI_vertex_attrib_array_object,
  GL_ATI_vertex_streams,
  GL_EXT_422_pixels,
  GL_EXT_abgr,
  GL_EXT_bgra,
  GL_EXT_blend_color,
  GL_EXT_blend_func_separate,
  GL_EXT_blend_logic_op,
  GL_EXT_blend_minmax,
  GL_EXT_blend_subtract,
  GL_EXT_clip_volume_hint,
  GL_EXT_cmyka,
  GL_EXT_color_matrix,
  GL_EXT_color_subtable,
  GL_EXT_compiled_vertex_array,
  GL_EXT_convolution,
  GL_EXT_coordinate_frame,
  GL_EXT_copy_texture,
  GL_EXT_cull_vertex,
  GL_EXT_draw_range_elements,
  GL_EXT_fog_coord,
  GL_EXT_framebuffer_object,
  GL_EXT_histogram,
  GL_EXT_index_array_formats,
  GL_EXT_index_func,
  GL_EXT_index_material,
  GL_EXT_index_texture,
  GL_EXT_light_texture,
  GL_EXT_misc_attribute,
  GL_EXT_multi_draw_arrays,
  GL_EXT_multisample,
  GL_EXT_packed_pixels,
  GL_EXT_paletted_texture,
  GL_EXT_pixel_transform,
  GL_EXT_pixel_transform_color_table,
  GL_EXT_point_parameters,
  GL_EXT_polygon_offset,
  GL_EXT_rescale_normal,
  GL_EXT_secondary_color,
  GL_EXT_separate_specular_color,
  GL_EXT_shadow_funcs,
  GL_EXT_shared_texture_palette,
  GL_EXT_stencil_two_side,
  GL_EXT_stencil_wrap,
  GL_EXT_subtexture,
  GL_EXT_texture,
  GL_EXT_texture3D,
  GL_EXT_texture_compression_s3tc,
  GL_EXT_texture_cube_map,
  GL_EXT_texture_edge_clamp,
  GL_EXT_texture_env_add,
  GL_EXT_texture_env_combine,
  GL_EXT_texture_env_dot3,
  GL_EXT_texture_filter_anisotropic,
  GL_EXT_texture_lod_bias,
  GL_EXT_texture_object,
  GL_EXT_texture_perturb_normal,
  GL_EXT_texture_rectangle,
  GL_EXT_vertex_array,
  GL_EXT_vertex_shader,
  GL_EXT_vertex_weighting,
  GL_EXT_depth_bounds_test,
  GL_EXT_texture_mirror_clamp,
  GL_EXT_blend_equation_separate,
  GL_EXT_pixel_buffer_object,
  GL_EXT_texture_compression_dxt1,
  GL_EXT_stencil_clear_tag,
  GL_EXT_packed_depth_stencil,
  GL_EXT_texture_sRGB,
  GL_EXT_framebuffer_blit,
  GL_EXT_framebuffer_multisample,
  GL_EXT_timer_query,
  GL_EXT_gpu_program_parameters,
  GL_EXT_bindable_uniform,
  GL_EXT_draw_buffers2,
  GL_EXT_draw_instanced,
  GL_EXT_framebuffer_sRGB,
  GL_EXT_geometry_shader4,
  GL_EXT_gpu_shader4,
  GL_EXT_packed_float,
  GL_EXT_texture_array,
  GL_EXT_texture_buffer_object,
  GL_EXT_texture_compression_latc,
  GL_EXT_texture_compression_rgtc,
  GL_EXT_texture_integer,
  GL_EXT_texture_shared_exponent,
  GL_EXT_transform_feedback,
  GL_EXT_direct_state_access,
  GL_EXT_vertex_array_bgra,
  GL_EXT_texture_swizzle,
  GL_FfdMaskSGIX,
  GL_HP_convolution_border_modes,
  GL_HP_image_transform,
  GL_HP_occlusion_test,
  GL_HP_texture_lighting,
  GL_IBM_cull_vertex,
  GL_IBM_multimode_draw_arrays,
  GL_IBM_rasterpos_clip,
  GL_IBM_texture_mirrored_repeat,
  GL_IBM_vertex_array_lists,
  GL_INGR_blend_func_separate,
  GL_INGR_color_clamp,
  GL_INGR_interlace_read,
  GL_INGR_palette_buffer,
  GL_INTEL_parallel_arrays,
  GL_INTEL_texture_scissor,
  GL_MESA_resize_buffers,
  GL_MESA_window_pos,
  GL_NV_blend_square,
  GL_NV_copy_depth_to_color,
  GL_NV_depth_clamp,
  GL_NV_evaluators,
  GL_NV_fence,
  GL_NV_float_buffer,
  GL_NV_fog_distance,
  GL_NV_fragment_program,
  GL_NV_half_float,
  GL_NV_light_max_exponent,
  GL_NV_multisample_filter_hint,
  GL_NV_occlusion_query,
  GL_NV_packed_depth_stencil,
  GL_NV_pixel_data_range,
  GL_NV_point_sprite,
  GL_NV_primitive_restart,
  GL_NV_register_combiners,
  GL_NV_register_combiners2,
  GL_NV_texgen_emboss,
  GL_NV_texgen_reflection,
  GL_NV_texture_compression_vtc,
  GL_NV_texture_env_combine4,
  GL_NV_texture_expand_normal,
  GL_NV_texture_rectangle,
  GL_NV_texture_shader,
  GL_NV_texture_shader2,
  GL_NV_texture_shader3,
  GL_NV_vertex_array_range,
  GL_NV_vertex_array_range2,
  GL_NV_vertex_program,
  GL_NV_vertex_program1_1,
  GL_NV_vertex_program2,
  GL_NV_fragment_program_option,
  GL_NV_fragment_program2,
  GL_NV_vertex_program2_option,
  GL_NV_vertex_program3,
  GL_NV_depth_buffer_float,
  GL_NV_fragment_program4,
  GL_NV_framebuffer_multisample_coverage,
  GL_NV_geometry_program4,
  GL_NV_gpu_program4,
  GL_NV_parameter_buffer_object,
  GL_NV_transform_feedback,
  GL_NV_vertex_program4,
  GL_NV_conditional_render,
  GL_NV_present_video,
  GL_NV_explicit_multisample,
  GL_NV_transform_feedback2,
  GL_OML_interlace,
  GL_OML_resample,
  GL_OML_subsample,
  GL_PGI_misc_hints,
  GL_PGI_vertex_hints,
  GL_REND_screen_coordinates,
  GL_S3_s3tc,
  GL_SGIS_detail_texture,
  GL_SGIS_fog_function,
  GL_SGIS_generate_mipmap,
  GL_SGIS_multisample,
  GL_SGIS_pixel_texture,
  GL_SGIS_point_line_texgen,
  GL_SGIS_point_parameters,
  GL_SGIS_sharpen_texture,
  GL_SGIS_texture4D,
  GL_SGIS_texture_border_clamp,
  GL_SGIS_texture_color_mask,
  GL_SGIS_texture_edge_clamp,
  GL_SGIS_texture_filter4,
  GL_SGIS_texture_lod,
  GL_SGIS_texture_select,
  GL_SGIX_async,
  GL_SGIX_async_histogram,
  GL_SGIX_async_pixel,
  GL_SGIX_blend_alpha_minmax,
  GL_SGIX_calligraphic_fragment,
  GL_SGIX_clipmap,
  GL_SGIX_convolution_accuracy,
  GL_SGIX_depth_pass_instrument,
  GL_SGIX_depth_texture,
  GL_SGIX_flush_raster,
  GL_SGIX_fog_offset,
  GL_SGIX_fog_scale,
  GL_SGIX_fragment_lighting,
  GL_SGIX_framezoom,
  GL_SGIX_igloo_interface,
  GL_SGIX_impact_pixel_texture,
  GL_SGIX_instruments,
  GL_SGIX_interlace,
  GL_SGIX_ir_instrument1,
  GL_SGIX_list_priority,
  GL_SGIX_pixel_texture,
  GL_SGIX_pixel_tiles,
  GL_SGIX_polynomial_ffd,
  GL_SGIX_reference_plane,
  GL_SGIX_resample,
  GL_SGIX_scalebias_hint,
  GL_SGIX_shadow,
  GL_SGIX_shadow_ambient,
  GL_SGIX_sprite,
  GL_SGIX_subsample,
  GL_SGIX_tag_sample_buffer,
  GL_SGIX_texture_add_env,
  GL_SGIX_texture_coordinate_clamp,
  GL_SGIX_texture_lod_bias,
  GL_SGIX_texture_multi_buffer,
  GL_SGIX_texture_scale_bias,
  GL_SGIX_texture_select,
  GL_SGIX_vertex_preclip,
  GL_SGIX_ycrcb,
  GL_SGIX_ycrcb_subsample,
  GL_SGIX_ycrcba,
  GL_SGI_color_matrix,
  GL_SGI_color_table,
  GL_SGI_depth_pass_instrument,
  GL_SGI_texture_color_table,
  GL_SUNX_constant_data,
  GL_SUN_convolution_border_modes,
  GL_SUN_global_alpha,
  GL_SUN_mesh_array,
  GL_SUN_slice_accum,
  GL_SUN_triangle_list,
  GL_SUN_vertex,

  GL_WIN_phong_shading,
  GL_WIN_specular_fog,
  WGL_3DFX_multisample,
  WGL_ARB_buffer_region,
  WGL_ARB_extensions_string,
  WGL_ARB_make_current_read,
  WGL_ARB_multisample,
  WGL_ARB_pbuffer,
  WGL_ARB_pixel_format,
  WGL_ARB_pixel_format_float,
  WGL_ARB_render_texture,
  WGL_ARB_create_context,
  WGL_ATI_pixel_format_float,
  WGL_EXT_depth_float,
  WGL_EXT_display_color_table,
  WGL_EXT_extensions_string,
  WGL_EXT_make_current_read,
  WGL_EXT_multisample,
  WGL_EXT_pbuffer,
  WGL_EXT_pixel_format,
  WGL_EXT_swap_control,
  WGL_I3D_digital_video_control,
  WGL_I3D_gamma,
  WGL_I3D_genlock,
  WGL_I3D_image_buffer,
  WGL_I3D_swap_frame_lock,
  WGL_I3D_swap_frame_usage,
  WGL_NV_float_buffer,
  WGL_NV_render_depth_texture,
  WGL_NV_render_texture_rectangle,
  WGL_NV_vertex_array_range,
  WGL_NV_present_video,
  WGL_NV_video_out,
  WGL_NV_swap_group,
  WGL_NV_gpu_affinity,
  WGL_OML_sync_control,
  WIN_draw_range_elements,
  WIN_swap_hint: Boolean;

const
  // GL_VERSION_1_1
  GL_ACCUM = $0100;
  GL_LOAD = $0101;
  GL_RETURN = $0102;
  GL_MULT = $0103;
  GL_ADD = $0104;
  GL_NEVER = $0200;
  GL_LESS = $0201;
  GL_EQUAL = $0202;
  GL_LEQUAL = $0203;
  GL_GREATER = $0204;
  GL_NOTEQUAL = $0205;
  GL_GEQUAL = $0206;
  GL_ALWAYS = $0207;
  GL_CURRENT_BIT = $00000001;
  GL_POINT_BIT = $00000002;
  GL_LINE_BIT = $00000004;
  GL_POLYGON_BIT = $00000008;
  GL_POLYGON_STIPPLE_BIT = $00000010;
  GL_PIXEL_MODE_BIT = $00000020;
  GL_LIGHTING_BIT = $00000040;
  GL_FOG_BIT = $00000080;
  GL_DEPTH_BUFFER_BIT = $00000100;
  GL_ACCUM_BUFFER_BIT = $00000200;
  GL_STENCIL_BUFFER_BIT = $00000400;
  GL_VIEWPORT_BIT = $00000800;
  GL_TRANSFORM_BIT = $00001000;
  GL_ENABLE_BIT = $00002000;
  GL_COLOR_BUFFER_BIT = $00004000;
  GL_HINT_BIT = $00008000;
  GL_EVAL_BIT = $00010000;
  GL_LIST_BIT = $00020000;
  GL_TEXTURE_BIT = $00040000;
  GL_SCISSOR_BIT = $00080000;
  GL_ALL_ATTRIB_BITS = $000FFFFF;
  GL_POINTS = $0000;
  GL_LINES = $0001;
  GL_LINE_LOOP = $0002;
  GL_LINE_STRIP = $0003;
  GL_TRIANGLES = $0004;
  GL_TRIANGLE_STRIP = $0005;
  GL_TRIANGLE_FAN = $0006;
  GL_QUADS = $0007;
  GL_QUAD_STRIP = $0008;
  GL_POLYGON = $0009;
  GL_ZERO = 0;
  GL_ONE = 1;
  GL_SRC_COLOR = $0300;
  GL_ONE_MINUS_SRC_COLOR = $0301;
  GL_SRC_ALPHA = $0302;
  GL_ONE_MINUS_SRC_ALPHA = $0303;
  GL_DST_ALPHA = $0304;
  GL_ONE_MINUS_DST_ALPHA = $0305;
  GL_DST_COLOR = $0306;
  GL_ONE_MINUS_DST_COLOR = $0307;
  GL_SRC_ALPHA_SATURATE = $0308;
  GL_TRUE = 1;
  GL_FALSE = 0;
  GL_CLIP_PLANE0 = $3000;
  GL_CLIP_PLANE1 = $3001;
  GL_CLIP_PLANE2 = $3002;
  GL_CLIP_PLANE3 = $3003;
  GL_CLIP_PLANE4 = $3004;
  GL_CLIP_PLANE5 = $3005;
  GL_BYTE = $1400;
  GL_UNSIGNED_BYTE = $1401;
  GL_SHORT = $1402;
  GL_UNSIGNED_SHORT = $1403;
  GL_INT = $1404;
  GL_UNSIGNED_INT = $1405;
  GL_FLOAT = $1406;
  GL_2_BYTES = $1407;
  GL_3_BYTES = $1408;
  GL_4_BYTES = $1409;
  GL_DOUBLE = $140A;
  GL_NONE = 0;
  GL_FRONT_LEFT = $0400;
  GL_FRONT_RIGHT = $0401;
  GL_BACK_LEFT = $0402;
  GL_BACK_RIGHT = $0403;
  GL_FRONT = $0404;
  GL_BACK = $0405;
  GL_LEFT = $0406;
  GL_RIGHT = $0407;
  GL_FRONT_AND_BACK = $0408;
  GL_AUX0 = $0409;
  GL_AUX1 = $040A;
  GL_AUX2 = $040B;
  GL_AUX3 = $040C;
  GL_NO_ERROR = 0;
  GL_INVALID_ENUM = $0500;
  GL_INVALID_VALUE = $0501;
  GL_INVALID_OPERATION = $0502;
  GL_STACK_OVERFLOW = $0503;
  GL_STACK_UNDERFLOW = $0504;
  GL_OUT_OF_MEMORY = $0505;
  GL_2D = $0600;
  GL_3D = $0601;
  GL_3D_COLOR = $0602;
  GL_3D_COLOR_TEXTURE = $0603;
  GL_4D_COLOR_TEXTURE = $0604;
  GL_PASS_THROUGH_TOKEN = $0700;
  GL_POINT_TOKEN = $0701;
  GL_LINE_TOKEN = $0702;
  GL_POLYGON_TOKEN = $0703;
  GL_BITMAP_TOKEN = $0704;
  GL_DRAW_PIXEL_TOKEN = $0705;
  GL_COPY_PIXEL_TOKEN = $0706;
  GL_LINE_RESET_TOKEN = $0707;
  GL_EXP = $0800;
  GL_EXP2 = $0801;
  GL_CW = $0900;
  GL_CCW = $0901;
  GL_COEFF = $0A00;
  GL_ORDER = $0A01;
  GL_DOMAIN = $0A02;
  GL_CURRENT_COLOR = $0B00;
  GL_CURRENT_INDEX = $0B01;
  GL_CURRENT_NORMAL = $0B02;
  GL_CURRENT_TEXTURE_COORDS = $0B03;
  GL_CURRENT_RASTER_COLOR = $0B04;
  GL_CURRENT_RASTER_INDEX = $0B05;
  GL_CURRENT_RASTER_TEXTURE_COORDS = $0B06;
  GL_CURRENT_RASTER_POSITION = $0B07;
  GL_CURRENT_RASTER_POSITION_VALID = $0B08;
  GL_CURRENT_RASTER_DISTANCE = $0B09;
  GL_POINT_SMOOTH = $0B10;
  GL_POINT_SIZE = $0B11;
  GL_POINT_SIZE_RANGE = $0B12;
  GL_POINT_SIZE_GRANULARITY = $0B13;
  GL_LINE_SMOOTH = $0B20;
  GL_LINE_WIDTH = $0B21;
  GL_LINE_WIDTH_RANGE = $0B22;
  GL_LINE_WIDTH_GRANULARITY = $0B23;
  GL_LINE_STIPPLE = $0B24;
  GL_LINE_STIPPLE_PATTERN = $0B25;
  GL_LINE_STIPPLE_REPEAT = $0B26;
  GL_LIST_MODE = $0B30;
  GL_MAX_LIST_NESTING = $0B31;
  GL_LIST_BASE = $0B32;
  GL_LIST_INDEX = $0B33;
  GL_POLYGON_MODE = $0B40;
  GL_POLYGON_SMOOTH = $0B41;
  GL_POLYGON_STIPPLE = $0B42;
  GL_EDGE_FLAG = $0B43;
  GL_CULL_FACE = $0B44;
  GL_CULL_FACE_MODE = $0B45;
  GL_FRONT_FACE = $0B46;
  GL_LIGHTING = $0B50;
  GL_LIGHT_MODEL_LOCAL_VIEWER = $0B51;
  GL_LIGHT_MODEL_TWO_SIDE = $0B52;
  GL_LIGHT_MODEL_AMBIENT = $0B53;
  GL_SHADE_MODEL = $0B54;
  GL_COLOR_MATERIAL_FACE = $0B55;
  GL_COLOR_MATERIAL_PARAMETER = $0B56;
  GL_COLOR_MATERIAL = $0B57;
  GL_FOG = $0B60;
  GL_FOG_INDEX = $0B61;
  GL_FOG_DENSITY = $0B62;
  GL_FOG_START = $0B63;
  GL_FOG_END = $0B64;
  GL_FOG_MODE = $0B65;
  GL_FOG_COLOR = $0B66;
  GL_DEPTH_RANGE = $0B70;
  GL_DEPTH_TEST = $0B71;
  GL_DEPTH_WRITEMASK = $0B72;
  GL_DEPTH_CLEAR_VALUE = $0B73;
  GL_DEPTH_FUNC = $0B74;
  GL_ACCUM_CLEAR_VALUE = $0B80;
  GL_STENCIL_TEST = $0B90;
  GL_STENCIL_CLEAR_VALUE = $0B91;
  GL_STENCIL_FUNC = $0B92;
  GL_STENCIL_VALUE_MASK = $0B93;
  GL_STENCIL_FAIL = $0B94;
  GL_STENCIL_PASS_DEPTH_FAIL = $0B95;
  GL_STENCIL_PASS_DEPTH_PASS = $0B96;
  GL_STENCIL_REF = $0B97;
  GL_STENCIL_WRITEMASK = $0B98;
  GL_MATRIX_MODE = $0BA0;
  GL_NORMALIZE = $0BA1;
  GL_VIEWPORT = $0BA2;
  GL_MODELVIEW_STACK_DEPTH = $0BA3;
  GL_PROJECTION_STACK_DEPTH = $0BA4;
  GL_TEXTURE_STACK_DEPTH = $0BA5;
  GL_MODELVIEW_MATRIX = $0BA6;
  GL_PROJECTION_MATRIX = $0BA7;
  GL_TEXTURE_MATRIX = $0BA8;
  GL_ATTRIB_STACK_DEPTH = $0BB0;
  GL_CLIENT_ATTRIB_STACK_DEPTH = $0BB1;
  GL_ALPHA_TEST = $0BC0;
  GL_ALPHA_TEST_FUNC = $0BC1;
  GL_ALPHA_TEST_REF = $0BC2;
  GL_DITHER = $0BD0;
  GL_BLEND_DST = $0BE0;
  GL_BLEND_SRC = $0BE1;
  GL_BLEND = $0BE2;
  GL_LOGIC_OP_MODE = $0BF0;
  GL_INDEX_LOGIC_OP = $0BF1;
  GL_COLOR_LOGIC_OP = $0BF2;
  GL_AUX_BUFFERS = $0C00;
  GL_DRAW_BUFFER = $0C01;
  GL_READ_BUFFER = $0C02;
  GL_SCISSOR_BOX = $0C10;
  GL_SCISSOR_TEST = $0C11;
  GL_INDEX_CLEAR_VALUE = $0C20;
  GL_INDEX_WRITEMASK = $0C21;
  GL_COLOR_CLEAR_VALUE = $0C22;
  GL_COLOR_WRITEMASK = $0C23;
  GL_INDEX_MODE = $0C30;
  GL_RGBA_MODE = $0C31;
  GL_DOUBLEBUFFER = $0C32;
  GL_STEREO = $0C33;
  GL_RENDER_MODE = $0C40;
  GL_PERSPECTIVE_CORRECTION_HINT = $0C50;
  GL_POINT_SMOOTH_HINT = $0C51;
  GL_LINE_SMOOTH_HINT = $0C52;
  GL_POLYGON_SMOOTH_HINT = $0C53;
  GL_FOG_HINT = $0C54;
  GL_TEXTURE_GEN_S = $0C60;
  GL_TEXTURE_GEN_T = $0C61;
  GL_TEXTURE_GEN_R = $0C62;
  GL_TEXTURE_GEN_Q = $0C63;
  GL_PIXEL_MAP_I_TO_I = $0C70;
  GL_PIXEL_MAP_S_TO_S = $0C71;
  GL_PIXEL_MAP_I_TO_R = $0C72;
  GL_PIXEL_MAP_I_TO_G = $0C73;
  GL_PIXEL_MAP_I_TO_B = $0C74;
  GL_PIXEL_MAP_I_TO_A = $0C75;
  GL_PIXEL_MAP_R_TO_R = $0C76;
  GL_PIXEL_MAP_G_TO_G = $0C77;
  GL_PIXEL_MAP_B_TO_B = $0C78;
  GL_PIXEL_MAP_A_TO_A = $0C79;
  GL_PIXEL_MAP_I_TO_I_SIZE = $0CB0;
  GL_PIXEL_MAP_S_TO_S_SIZE = $0CB1;
  GL_PIXEL_MAP_I_TO_R_SIZE = $0CB2;
  GL_PIXEL_MAP_I_TO_G_SIZE = $0CB3;
  GL_PIXEL_MAP_I_TO_B_SIZE = $0CB4;
  GL_PIXEL_MAP_I_TO_A_SIZE = $0CB5;
  GL_PIXEL_MAP_R_TO_R_SIZE = $0CB6;
  GL_PIXEL_MAP_G_TO_G_SIZE = $0CB7;
  GL_PIXEL_MAP_B_TO_B_SIZE = $0CB8;
  GL_PIXEL_MAP_A_TO_A_SIZE = $0CB9;
  GL_UNPACK_SWAP_BYTES = $0CF0;
  GL_UNPACK_LSB_FIRST = $0CF1;
  GL_UNPACK_ROW_LENGTH = $0CF2;
  GL_UNPACK_SKIP_ROWS = $0CF3;
  GL_UNPACK_SKIP_PIXELS = $0CF4;
  GL_UNPACK_ALIGNMENT = $0CF5;
  GL_PACK_SWAP_BYTES = $0D00;
  GL_PACK_LSB_FIRST = $0D01;
  GL_PACK_ROW_LENGTH = $0D02;
  GL_PACK_SKIP_ROWS = $0D03;
  GL_PACK_SKIP_PIXELS = $0D04;
  GL_PACK_ALIGNMENT = $0D05;
  GL_MAP_COLOR = $0D10;
  GL_MAP_STENCIL = $0D11;
  GL_INDEX_SHIFT = $0D12;
  GL_INDEX_OFFSET = $0D13;
  GL_RED_SCALE = $0D14;
  GL_RED_BIAS = $0D15;
  GL_ZOOM_X = $0D16;
  GL_ZOOM_Y = $0D17;
  GL_GREEN_SCALE = $0D18;
  GL_GREEN_BIAS = $0D19;
  GL_BLUE_SCALE = $0D1A;
  GL_BLUE_BIAS = $0D1B;
  GL_ALPHA_SCALE = $0D1C;
  GL_ALPHA_BIAS = $0D1D;
  GL_DEPTH_SCALE = $0D1E;
  GL_DEPTH_BIAS = $0D1F;
  GL_MAX_EVAL_ORDER = $0D30;
  GL_MAX_LIGHTS = $0D31;
  GL_MAX_CLIP_PLANES = $0D32;
  GL_MAX_TEXTURE_SIZE = $0D33;
  GL_MAX_PIXEL_MAP_TABLE = $0D34;
  GL_MAX_ATTRIB_STACK_DEPTH = $0D35;
  GL_MAX_MODELVIEW_STACK_DEPTH = $0D36;
  GL_MAX_NAME_STACK_DEPTH = $0D37;
  GL_MAX_PROJECTION_STACK_DEPTH = $0D38;
  GL_MAX_TEXTURE_STACK_DEPTH = $0D39;
  GL_MAX_VIEWPORT_DIMS = $0D3A;
  GL_MAX_CLIENT_ATTRIB_STACK_DEPTH = $0D3B;
  GL_SUBPIXEL_BITS = $0D50;
  GL_INDEX_BITS = $0D51;
  GL_RED_BITS = $0D52;
  GL_GREEN_BITS = $0D53;
  GL_BLUE_BITS = $0D54;
  GL_ALPHA_BITS = $0D55;
  GL_DEPTH_BITS = $0D56;
  GL_STENCIL_BITS = $0D57;
  GL_ACCUM_RED_BITS = $0D58;
  GL_ACCUM_GREEN_BITS = $0D59;
  GL_ACCUM_BLUE_BITS = $0D5A;
  GL_ACCUM_ALPHA_BITS = $0D5B;
  GL_NAME_STACK_DEPTH = $0D70;
  GL_AUTO_NORMAL = $0D80;
  GL_MAP1_COLOR_4 = $0D90;
  GL_MAP1_INDEX = $0D91;
  GL_MAP1_NORMAL = $0D92;
  GL_MAP1_TEXTURE_COORD_1 = $0D93;
  GL_MAP1_TEXTURE_COORD_2 = $0D94;
  GL_MAP1_TEXTURE_COORD_3 = $0D95;
  GL_MAP1_TEXTURE_COORD_4 = $0D96;
  GL_MAP1_VERTEX_3 = $0D97;
  GL_MAP1_VERTEX_4 = $0D98;
  GL_MAP2_COLOR_4 = $0DB0;
  GL_MAP2_INDEX = $0DB1;
  GL_MAP2_NORMAL = $0DB2;
  GL_MAP2_TEXTURE_COORD_1 = $0DB3;
  GL_MAP2_TEXTURE_COORD_2 = $0DB4;
  GL_MAP2_TEXTURE_COORD_3 = $0DB5;
  GL_MAP2_TEXTURE_COORD_4 = $0DB6;
  GL_MAP2_VERTEX_3 = $0DB7;
  GL_MAP2_VERTEX_4 = $0DB8;
  GL_MAP1_GRID_DOMAIN = $0DD0;
  GL_MAP1_GRID_SEGMENTS = $0DD1;
  GL_MAP2_GRID_DOMAIN = $0DD2;
  GL_MAP2_GRID_SEGMENTS = $0DD3;
  GL_TEXTURE_1D = $0DE0;
  GL_TEXTURE_2D = $0DE1;
  GL_FEEDBACK_BUFFER_POINTER = $0DF0;
  GL_FEEDBACK_BUFFER_SIZE = $0DF1;
  GL_FEEDBACK_BUFFER_TYPE = $0DF2;
  GL_SELECTION_BUFFER_POINTER = $0DF3;
  GL_SELECTION_BUFFER_SIZE = $0DF4;
  GL_TEXTURE_WIDTH = $1000;
  GL_TEXTURE_HEIGHT = $1001;
  GL_TEXTURE_INTERNAL_FORMAT = $1003;
  GL_TEXTURE_BORDER_COLOR = $1004;
  GL_TEXTURE_BORDER = $1005;
  GL_DONT_CARE = $1100;
  GL_FASTEST = $1101;
  GL_NICEST = $1102;
  GL_LIGHT0 = $4000;
  GL_LIGHT1 = $4001;
  GL_LIGHT2 = $4002;
  GL_LIGHT3 = $4003;
  GL_LIGHT4 = $4004;
  GL_LIGHT5 = $4005;
  GL_LIGHT6 = $4006;
  GL_LIGHT7 = $4007;
  GL_AMBIENT = $1200;
  GL_DIFFUSE = $1201;
  GL_SPECULAR = $1202;
  GL_POSITION = $1203;
  GL_SPOT_DIRECTION = $1204;
  GL_SPOT_EXPONENT = $1205;
  GL_SPOT_CUTOFF = $1206;
  GL_CONSTANT_ATTENUATION = $1207;
  GL_LINEAR_ATTENUATION = $1208;
  GL_QUADRATIC_ATTENUATION = $1209;
  GL_COMPILE = $1300;
  GL_COMPILE_AND_EXECUTE = $1301;
  GL_CLEAR = $1500;
  GL_AND = $1501;
  GL_AND_REVERSE = $1502;
  GL_COPY = $1503;
  GL_AND_INVERTED = $1504;
  GL_NOOP = $1505;
  GL_XOR = $1506;
  GL_OR = $1507;
  GL_NOR = $1508;
  GL_EQUIV = $1509;
  GL_INVERT = $150A;
  GL_OR_REVERSE = $150B;
  GL_COPY_INVERTED = $150C;
  GL_OR_INVERTED = $150D;
  GL_NAND = $150E;
  GL_SET = $150F;
  GL_EMISSION = $1600;
  GL_SHININESS = $1601;
  GL_AMBIENT_AND_DIFFUSE = $1602;
  GL_COLOR_INDEXES = $1603;
  GL_MODELVIEW = $1700;
  GL_PROJECTION = $1701;
  GL_TEXTURE = $1702;
  GL_COLOR = $1800;
  GL_DEPTH = $1801;
  GL_STENCIL = $1802;
  GL_COLOR_INDEX = $1900;
  GL_STENCIL_INDEX = $1901;
  GL_DEPTH_COMPONENT = $1902;
  GL_RED = $1903;
  GL_GREEN = $1904;
  GL_BLUE = $1905;
  GL_ALPHA = $1906;
  GL_RGB = $1907;
  GL_RGBA = $1908;
  GL_LUMINANCE = $1909;
  GL_LUMINANCE_ALPHA = $190A;
  GL_BITMAP = $1A00;
  GL_POINT = $1B00;
  GL_LINE = $1B01;
  GL_FILL = $1B02;
  GL_RENDER = $1C00;
  GL_FEEDBACK = $1C01;
  GL_SELECT = $1C02;
  GL_FLAT = $1D00;
  GL_SMOOTH = $1D01;
  GL_KEEP = $1E00;
  GL_REPLACE = $1E01;
  GL_INCR = $1E02;
  GL_DECR = $1E03;
  GL_VENDOR = $1F00;
  GL_RENDERER = $1F01;
  GL_VERSION = $1F02;
  GL_EXTENSIONS = $1F03;
  GL_S = $2000;
  GL_T = $2001;
  GL_R = $2002;
  GL_Q = $2003;
  GL_MODULATE = $2100;
  GL_DECAL = $2101;
  GL_TEXTURE_ENV_MODE = $2200;
  GL_TEXTURE_ENV_COLOR = $2201;
  GL_TEXTURE_ENV = $2300;
  GL_EYE_LINEAR = $2400;
  GL_OBJECT_LINEAR = $2401;
  GL_SPHERE_MAP = $2402;
  GL_TEXTURE_GEN_MODE = $2500;
  GL_OBJECT_PLANE = $2501;
  GL_EYE_PLANE = $2502;
  GL_NEAREST = $2600;
  GL_LINEAR = $2601;
  GL_NEAREST_MIPMAP_NEAREST = $2700;
  GL_LINEAR_MIPMAP_NEAREST = $2701;
  GL_NEAREST_MIPMAP_LINEAR = $2702;
  GL_LINEAR_MIPMAP_LINEAR = $2703;
  GL_TEXTURE_MAG_FILTER = $2800;
  GL_TEXTURE_MIN_FILTER = $2801;
  GL_TEXTURE_WRAP_S = $2802;
  GL_TEXTURE_WRAP_T = $2803;
  GL_CLAMP = $2900;
  GL_REPEAT = $2901;
  GL_CLIENT_PIXEL_STORE_BIT = $00000001;
  GL_CLIENT_VERTEX_ARRAY_BIT = $00000002;
  GL_CLIENT_ALL_ATTRIB_BITS = $FFFFFFFF;
  GL_POLYGON_OFFSET_FACTOR = $8038;
  GL_POLYGON_OFFSET_UNITS = $2A00;
  GL_POLYGON_OFFSET_POINT = $2A01;
  GL_POLYGON_OFFSET_LINE = $2A02;
  GL_POLYGON_OFFSET_FILL = $8037;
  GL_ALPHA4 = $803B;
  GL_ALPHA8 = $803C;
  GL_ALPHA12 = $803D;
  GL_ALPHA16 = $803E;
  GL_LUMINANCE4 = $803F;
  GL_LUMINANCE8 = $8040;
  GL_LUMINANCE12 = $8041;
  GL_LUMINANCE16 = $8042;
  GL_LUMINANCE4_ALPHA4 = $8043;
  GL_LUMINANCE6_ALPHA2 = $8044;
  GL_LUMINANCE8_ALPHA8 = $8045;
  GL_LUMINANCE12_ALPHA4 = $8046;
  GL_LUMINANCE12_ALPHA12 = $8047;
  GL_LUMINANCE16_ALPHA16 = $8048;
  GL_INTENSITY = $8049;
  GL_INTENSITY4 = $804A;
  GL_INTENSITY8 = $804B;
  GL_INTENSITY12 = $804C;
  GL_INTENSITY16 = $804D;
  GL_R3_G3_B2 = $2A10;
  GL_RGB4 = $804F;
  GL_RGB5 = $8050;
  GL_RGB8 = $8051;
  GL_RGB10 = $8052;
  GL_RGB12 = $8053;
  GL_RGB16 = $8054;
  GL_RGBA2 = $8055;
  GL_RGBA4 = $8056;
  GL_RGB5_A1 = $8057;
  GL_RGBA8 = $8058;
  GL_RGB10_A2 = $8059;
  GL_RGBA12 = $805A;
  GL_RGBA16 = $805B;
  GL_TEXTURE_RED_SIZE = $805C;
  GL_TEXTURE_GREEN_SIZE = $805D;
  GL_TEXTURE_BLUE_SIZE = $805E;
  GL_TEXTURE_ALPHA_SIZE = $805F;
  GL_TEXTURE_LUMINANCE_SIZE = $8060;
  GL_TEXTURE_INTENSITY_SIZE = $8061;
  GL_PROXY_TEXTURE_1D = $8063;
  GL_PROXY_TEXTURE_2D = $8064;
  GL_TEXTURE_PRIORITY = $8066;
  GL_TEXTURE_RESIDENT = $8067;
  GL_TEXTURE_BINDING_1D = $8068;
  GL_TEXTURE_BINDING_2D = $8069;
  GL_VERTEX_ARRAY = $8074;
  GL_NORMAL_ARRAY = $8075;
  GL_COLOR_ARRAY = $8076;
  GL_INDEX_ARRAY = $8077;
  GL_TEXTURE_COORD_ARRAY = $8078;
  GL_EDGE_FLAG_ARRAY = $8079;
  GL_VERTEX_ARRAY_SIZE = $807A;
  GL_VERTEX_ARRAY_TYPE = $807B;
  GL_VERTEX_ARRAY_STRIDE = $807C;
  GL_NORMAL_ARRAY_TYPE = $807E;
  GL_NORMAL_ARRAY_STRIDE = $807F;
  GL_COLOR_ARRAY_SIZE = $8081;
  GL_COLOR_ARRAY_TYPE = $8082;
  GL_COLOR_ARRAY_STRIDE = $8083;
  GL_INDEX_ARRAY_TYPE = $8085;
  GL_INDEX_ARRAY_STRIDE = $8086;
  GL_TEXTURE_COORD_ARRAY_SIZE = $8088;
  GL_TEXTURE_COORD_ARRAY_TYPE = $8089;
  GL_TEXTURE_COORD_ARRAY_STRIDE = $808A;
  GL_EDGE_FLAG_ARRAY_STRIDE = $808C;
  GL_VERTEX_ARRAY_POINTER = $808E;
  GL_NORMAL_ARRAY_POINTER = $808F;
  GL_COLOR_ARRAY_POINTER = $8090;
  GL_INDEX_ARRAY_POINTER = $8091;
  GL_TEXTURE_COORD_ARRAY_POINTER = $8092;
  GL_EDGE_FLAG_ARRAY_POINTER = $8093;
  GL_V2F = $2A20;
  GL_V3F = $2A21;
  GL_C4UB_V2F = $2A22;
  GL_C4UB_V3F = $2A23;
  GL_C3F_V3F = $2A24;
  GL_N3F_V3F = $2A25;
  GL_C4F_N3F_V3F = $2A26;
  GL_T2F_V3F = $2A27;
  GL_T4F_V4F = $2A28;
  GL_T2F_C4UB_V3F = $2A29;
  GL_T2F_C3F_V3F = $2A2A;
  GL_T2F_N3F_V3F = $2A2B;
  GL_T2F_C4F_N3F_V3F = $2A2C;
  GL_T4F_C4F_N3F_V4F = $2A2D;
  GL_COLOR_TABLE_FORMAT_EXT = $80D8;
  GL_COLOR_TABLE_WIDTH_EXT = $80D9;
  GL_COLOR_TABLE_RED_SIZE_EXT = $80DA;
  GL_COLOR_TABLE_GREEN_SIZE_EXT = $80DB;
  GL_COLOR_TABLE_BLUE_SIZE_EXT = $80DC;
  GL_COLOR_TABLE_ALPHA_SIZE_EXT = $80DD;
  GL_COLOR_TABLE_LUMINANCE_SIZE_EXT = $80DE;
  GL_COLOR_TABLE_INTENSITY_SIZE_EXT = $80DF;
  GL_LOGIC_OP = GL_INDEX_LOGIC_OP;
  GL_TEXTURE_COMPONENTS = GL_TEXTURE_INTERNAL_FORMAT;

  // GL_VERSION_1_2
  GL_UNSIGNED_BYTE_3_3_2 = $8032;
  GL_UNSIGNED_SHORT_4_4_4_4 = $8033;
  GL_UNSIGNED_SHORT_5_5_5_1 = $8034;
  GL_UNSIGNED_INT_8_8_8_8 = $8035;
  GL_UNSIGNED_INT_10_10_10_2 = $8036;
  GL_RESCALE_NORMAL = $803A;
  GL_TEXTURE_BINDING_3D = $806A;
  GL_PACK_SKIP_IMAGES = $806B;
  GL_PACK_IMAGE_HEIGHT = $806C;
  GL_UNPACK_SKIP_IMAGES = $806D;
  GL_UNPACK_IMAGE_HEIGHT = $806E;
  GL_TEXTURE_3D = $806F;
  GL_PROXY_TEXTURE_3D = $8070;
  GL_TEXTURE_DEPTH = $8071;
  GL_TEXTURE_WRAP_R = $8072;
  GL_MAX_3D_TEXTURE_SIZE = $8073;
  GL_UNSIGNED_BYTE_2_3_3_REV = $8362;
  GL_UNSIGNED_SHORT_5_6_5 = $8363;
  GL_UNSIGNED_SHORT_5_6_5_REV = $8364;
  GL_UNSIGNED_SHORT_4_4_4_4_REV = $8365;
  GL_UNSIGNED_SHORT_1_5_5_5_REV = $8366;
  GL_UNSIGNED_INT_8_8_8_8_REV = $8367;
  GL_UNSIGNED_INT_2_10_10_10_REV = $8368;
  GL_BGR = $80E0;
  GL_BGRA = $80E1;
  GL_MAX_ELEMENTS_VERTICES = $80E8;
  GL_MAX_ELEMENTS_INDICES = $80E9;
  GL_CLAMP_TO_EDGE = $812F;
  GL_TEXTURE_MIN_LOD = $813A;
  GL_TEXTURE_MAX_LOD = $813B;
  GL_TEXTURE_BASE_LEVEL = $813C;
  GL_TEXTURE_MAX_LEVEL = $813D;
  GL_LIGHT_MODEL_COLOR_CONTROL = $81F8;
  GL_SINGLE_COLOR = $81F9;
  GL_SEPARATE_SPECULAR_COLOR = $81FA;
  GL_SMOOTH_POINT_SIZE_RANGE = $0B12;
  GL_SMOOTH_POINT_SIZE_GRANULARITY = $0B13;
  GL_SMOOTH_LINE_WIDTH_RANGE = $0B22;
  GL_SMOOTH_LINE_WIDTH_GRANULARITY = $0B23;
  GL_ALIASED_POINT_SIZE_RANGE = $846D;
  GL_ALIASED_LINE_WIDTH_RANGE = $846E;

  // GL_VERSION_1_3
  GL_TEXTURE0 = $84C0;
  GL_TEXTURE1 = $84C1;
  GL_TEXTURE2 = $84C2;
  GL_TEXTURE3 = $84C3;
  GL_TEXTURE4 = $84C4;
  GL_TEXTURE5 = $84C5;
  GL_TEXTURE6 = $84C6;
  GL_TEXTURE7 = $84C7;
  GL_TEXTURE8 = $84C8;
  GL_TEXTURE9 = $84C9;
  GL_TEXTURE10 = $84CA;
  GL_TEXTURE11 = $84CB;
  GL_TEXTURE12 = $84CC;
  GL_TEXTURE13 = $84CD;
  GL_TEXTURE14 = $84CE;
  GL_TEXTURE15 = $84CF;
  GL_TEXTURE16 = $84D0;
  GL_TEXTURE17 = $84D1;
  GL_TEXTURE18 = $84D2;
  GL_TEXTURE19 = $84D3;
  GL_TEXTURE20 = $84D4;
  GL_TEXTURE21 = $84D5;
  GL_TEXTURE22 = $84D6;
  GL_TEXTURE23 = $84D7;
  GL_TEXTURE24 = $84D8;
  GL_TEXTURE25 = $84D9;
  GL_TEXTURE26 = $84DA;
  GL_TEXTURE27 = $84DB;
  GL_TEXTURE28 = $84DC;
  GL_TEXTURE29 = $84DD;
  GL_TEXTURE30 = $84DE;
  GL_TEXTURE31 = $84DF;
  GL_ACTIVE_TEXTURE = $84E0;
  GL_CLIENT_ACTIVE_TEXTURE = $84E1;
  GL_MAX_TEXTURE_UNITS = $84E2;
  GL_TRANSPOSE_MODELVIEW_MATRIX = $84E3;
  GL_TRANSPOSE_PROJECTION_MATRIX = $84E4;
  GL_TRANSPOSE_TEXTURE_MATRIX = $84E5;
  GL_TRANSPOSE_COLOR_MATRIX = $84E6;
  GL_MULTISAMPLE = $809D;
  GL_SAMPLE_ALPHA_TO_COVERAGE = $809E;
  GL_SAMPLE_ALPHA_TO_ONE = $809F;
  GL_SAMPLE_COVERAGE = $80A0;
  GL_SAMPLE_BUFFERS = $80A8;
  GL_SAMPLES = $80A9;
  GL_SAMPLE_COVERAGE_VALUE = $80AA;
  GL_SAMPLE_COVERAGE_INVERT = $80AB;
  GL_MULTISAMPLE_BIT = $20000000;
  GL_NORMAL_MAP = $8511;
  GL_REFLECTION_MAP = $8512;
  GL_TEXTURE_CUBE_MAP = $8513;
  GL_TEXTURE_BINDING_CUBE_MAP = $8514;
  GL_TEXTURE_CUBE_MAP_POSITIVE_X = $8515;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_X = $8516;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Y = $8517;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Y = $8518;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Z = $8519;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Z = $851A;
  GL_PROXY_TEXTURE_CUBE_MAP = $851B;
  GL_MAX_CUBE_MAP_TEXTURE_SIZE = $851C;
  GL_COMPRESSED_ALPHA = $84E9;
  GL_COMPRESSED_LUMINANCE = $84EA;
  GL_COMPRESSED_LUMINANCE_ALPHA = $84EB;
  GL_COMPRESSED_INTENSITY = $84EC;
  GL_COMPRESSED_RGB = $84ED;
  GL_COMPRESSED_RGBA = $84EE;
  GL_TEXTURE_COMPRESSION_HINT = $84EF;
  GL_TEXTURE_COMPRESSED_IMAGE_SIZE = $86A0;
  GL_TEXTURE_COMPRESSED = $86A1;
  GL_NUM_COMPRESSED_TEXTURE_FORMATS = $86A2;
  GL_COMPRESSED_TEXTURE_FORMATS = $86A3;
  GL_CLAMP_TO_BORDER = $812D;
  GL_CLAMP_TO_BORDER_SGIS = $812D;
  GL_COMBINE = $8570;
  GL_COMBINE_RGB = $8571;
  GL_COMBINE_ALPHA = $8572;
  GL_SOURCE0_RGB = $8580;
  GL_SOURCE1_RGB = $8581;
  GL_SOURCE2_RGB = $8582;
  GL_SOURCE0_ALPHA = $8588;
  GL_SOURCE1_ALPHA = $8589;
  GL_SOURCE2_ALPHA = $858A;
  GL_OPERAND0_RGB = $8590;
  GL_OPERAND1_RGB = $8591;
  GL_OPERAND2_RGB = $8592;
  GL_OPERAND0_ALPHA = $8598;
  GL_OPERAND1_ALPHA = $8599;
  GL_OPERAND2_ALPHA = $859A;
  GL_RGB_SCALE = $8573;
  GL_ADD_SIGNED = $8574;
  GL_INTERPOLATE = $8575;
  GL_SUBTRACT = $84E7;
  GL_CONSTANT = $8576;
  GL_PRIMARY_COLOR = $8577;
  GL_PREVIOUS = $8578;
  GL_DOT3_RGB = $86AE;
  GL_DOT3_RGBA = $86AF;

  // GL_VERSION_1_4
  GL_BLEND_DST_RGB = $80C8;
  GL_BLEND_SRC_RGB = $80C9;
  GL_BLEND_DST_ALPHA = $80CA;
  GL_BLEND_SRC_ALPHA = $80CB;
  GL_POINT_SIZE_MIN = $8126;
  GL_POINT_SIZE_MAX = $8127;
  GL_POINT_FADE_THRESHOLD_SIZE = $8128;
  GL_POINT_DISTANCE_ATTENUATION = $8129;
  GL_GENERATE_MIPMAP = $8191;
  GL_GENERATE_MIPMAP_HINT = $8192;
  GL_DEPTH_COMPONENT16 = $81A5;
  GL_DEPTH_COMPONENT24 = $81A6;
  GL_DEPTH_COMPONENT32 = $81A7;
  GL_MIRRORED_REPEAT = $8370;
  GL_FOG_COORDINATE_SOURCE = $8450;
  GL_FOG_COORDINATE = $8451;
  GL_FRAGMENT_DEPTH = $8452;
  GL_CURRENT_FOG_COORDINATE = $8453;
  GL_FOG_COORDINATE_ARRAY_TYPE = $8454;
  GL_FOG_COORDINATE_ARRAY_STRIDE = $8455;
  GL_FOG_COORDINATE_ARRAY_POINTER = $8456;
  GL_FOG_COORDINATE_ARRAY = $8457;
  GL_COLOR_SUM = $8458;
  GL_CURRENT_SECONDARY_COLOR = $8459;
  GL_SECONDARY_COLOR_ARRAY_SIZE = $845A;
  GL_SECONDARY_COLOR_ARRAY_TYPE = $845B;
  GL_SECONDARY_COLOR_ARRAY_STRIDE = $845C;
  GL_SECONDARY_COLOR_ARRAY_POINTER = $845D;
  GL_SECONDARY_COLOR_ARRAY = $845E;
  GL_MAX_TEXTURE_LOD_BIAS = $84FD;
  GL_TEXTURE_FILTER_CONTROL = $8500;
  GL_TEXTURE_LOD_BIAS = $8501;
  GL_INCR_WRAP = $8507;
  GL_DECR_WRAP = $8508;
  GL_TEXTURE_DEPTH_SIZE = $884A;
  GL_DEPTH_TEXTURE_MODE = $884B;
  GL_TEXTURE_COMPARE_MODE = $884C;
  GL_TEXTURE_COMPARE_FUNC = $884D;
  GL_COMPARE_R_TO_TEXTURE = $884E;

  // GL_VERSION_1_5
  GL_BUFFER_SIZE = $8764;
  GL_BUFFER_USAGE = $8765;
  GL_QUERY_COUNTER_BITS = $8864;
  GL_CURRENT_QUERY = $8865;
  GL_QUERY_RESULT = $8866;
  GL_QUERY_RESULT_AVAILABLE = $8867;
  GL_ARRAY_BUFFER = $8892;
  GL_ELEMENT_ARRAY_BUFFER = $8893;
  GL_ARRAY_BUFFER_BINDING = $8894;
  GL_ELEMENT_ARRAY_BUFFER_BINDING = $8895;
  GL_VERTEX_ARRAY_BUFFER_BINDING = $8896;
  GL_NORMAL_ARRAY_BUFFER_BINDING = $8897;
  GL_COLOR_ARRAY_BUFFER_BINDING = $8898;
  GL_INDEX_ARRAY_BUFFER_BINDING = $8899;
  GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING = $889A;
  GL_EDGE_FLAG_ARRAY_BUFFER_BINDING = $889B;
  GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING = $889C;
  GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING = $889D;
  GL_WEIGHT_ARRAY_BUFFER_BINDING = $889E;
  GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING = $889F;
  GL_READ_ONLY = $88B8;
  GL_WRITE_ONLY = $88B9;
  GL_READ_WRITE = $88BA;
  GL_BUFFER_ACCESS = $88BB;
  GL_BUFFER_MAPPED = $88BC;
  GL_BUFFER_MAP_POINTER = $88BD;
  GL_STREAM_DRAW = $88E0;
  GL_STREAM_READ = $88E1;
  GL_STREAM_COPY = $88E2;
  GL_STATIC_DRAW = $88E4;
  GL_STATIC_READ = $88E5;
  GL_STATIC_COPY = $88E6;
  GL_DYNAMIC_DRAW = $88E8;
  GL_DYNAMIC_READ = $88E9;
  GL_DYNAMIC_COPY = $88EA;
  GL_SAMPLES_PASSED = $8914;
  GL_FOG_COORD_SRC = GL_FOG_COORDINATE_SOURCE;
  GL_FOG_COORD = GL_FOG_COORDINATE;
  GL_CURRENT_FOG_COORD = GL_CURRENT_FOG_COORDINATE;
  GL_FOG_COORD_ARRAY_TYPE = GL_FOG_COORDINATE_ARRAY_TYPE;
  GL_FOG_COORD_ARRAY_STRIDE = GL_FOG_COORDINATE_ARRAY_STRIDE;
  GL_FOG_COORD_ARRAY_POINTER = GL_FOG_COORDINATE_ARRAY_POINTER;
  GL_FOG_COORD_ARRAY = GL_FOG_COORDINATE_ARRAY;
  GL_FOG_COORD_ARRAY_BUFFER_BINDING = GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING;
  GL_SRC0_RGB = GL_SOURCE0_RGB;
  GL_SRC1_RGB = GL_SOURCE1_RGB;
  GL_SRC2_RGB = GL_SOURCE2_RGB;
  GL_SRC0_ALPHA = GL_SOURCE0_ALPHA;
  GL_SRC1_ALPHA = GL_SOURCE1_ALPHA;
  GL_SRC2_ALPHA = GL_SOURCE2_ALPHA;

  // GL_VERSION_2_0
  GL_BLEND_EQUATION_RGB = $8009;
  GL_VERTEX_ATTRIB_ARRAY_ENABLED = $8622;
  GL_VERTEX_ATTRIB_ARRAY_SIZE = $8623;
  GL_VERTEX_ATTRIB_ARRAY_STRIDE = $8624;
  GL_VERTEX_ATTRIB_ARRAY_TYPE = $8625;
  GL_CURRENT_VERTEX_ATTRIB = $8626;
  GL_VERTEX_PROGRAM_POINT_SIZE = $8642;
  GL_VERTEX_PROGRAM_TWO_SIDE = $8643;
  GL_VERTEX_ATTRIB_ARRAY_POINTER = $8645;
  GL_STENCIL_BACK_FUNC = $8800;
  GL_STENCIL_BACK_FAIL = $8801;
  GL_STENCIL_BACK_PASS_DEPTH_FAIL = $8802;
  GL_STENCIL_BACK_PASS_DEPTH_PASS = $8803;
  GL_MAX_DRAW_BUFFERS = $8824;
  GL_DRAW_BUFFER0 = $8825;
  GL_DRAW_BUFFER1 = $8826;
  GL_DRAW_BUFFER2 = $8827;
  GL_DRAW_BUFFER3 = $8828;
  GL_DRAW_BUFFER4 = $8829;
  GL_DRAW_BUFFER5 = $882A;
  GL_DRAW_BUFFER6 = $882B;
  GL_DRAW_BUFFER7 = $882C;
  GL_DRAW_BUFFER8 = $882D;
  GL_DRAW_BUFFER9 = $882E;
  GL_DRAW_BUFFER10 = $882F;
  GL_DRAW_BUFFER11 = $8830;
  GL_DRAW_BUFFER12 = $8831;
  GL_DRAW_BUFFER13 = $8832;
  GL_DRAW_BUFFER14 = $8833;
  GL_DRAW_BUFFER15 = $8834;
  GL_BLEND_EQUATION_ALPHA = $883D;
  GL_POINT_SPRITE = $8861;
  GL_COORD_REPLACE = $8862;
  GL_MAX_VERTEX_ATTRIBS = $8869;
  GL_VERTEX_ATTRIB_ARRAY_NORMALIZED = $886A;
  GL_MAX_TEXTURE_COORDS = $8871;
  GL_MAX_TEXTURE_IMAGE_UNITS = $8872;
  GL_FRAGMENT_SHADER = $8B30;
  GL_VERTEX_SHADER = $8B31;
  GL_MAX_FRAGMENT_UNIFORM_COMPONENTS = $8B49;
  GL_MAX_VERTEX_UNIFORM_COMPONENTS = $8B4A;
  GL_MAX_VARYING_FLOATS = $8B4B;
  GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS = $8B4C;
  GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS = $8B4D;
  GL_SHADER_TYPE = $8B4F;
  GL_FLOAT_VEC2 = $8B50;
  GL_FLOAT_VEC3 = $8B51;
  GL_FLOAT_VEC4 = $8B52;
  GL_INT_VEC2 = $8B53;
  GL_INT_VEC3 = $8B54;
  GL_INT_VEC4 = $8B55;
  GL_BOOL = $8B56;
  GL_BOOL_VEC2 = $8B57;
  GL_BOOL_VEC3 = $8B58;
  GL_BOOL_VEC4 = $8B59;
  GL_FLOAT_MAT2 = $8B5A;
  GL_FLOAT_MAT3 = $8B5B;
  GL_FLOAT_MAT4 = $8B5C;
  GL_SAMPLER_1D = $8B5D;
  GL_SAMPLER_2D = $8B5E;
  GL_SAMPLER_3D = $8B5F;
  GL_SAMPLER_CUBE = $8B60;
  GL_SAMPLER_1D_SHADOW = $8B61;
  GL_SAMPLER_2D_SHADOW = $8B62;
  GL_DELETE_STATUS = $8B80;
  GL_COMPILE_STATUS = $8B81;
  GL_LINK_STATUS = $8B82;
  GL_VALIDATE_STATUS = $8B83;
  GL_INFO_LOG_LENGTH = $8B84;
  GL_ATTACHED_SHADERS = $8B85;
  GL_ACTIVE_UNIFORMS = $8B86;
  GL_ACTIVE_UNIFORM_MAX_LENGTH = $8B87;
  GL_SHADER_SOURCE_LENGTH = $8B88;
  GL_ACTIVE_ATTRIBUTES = $8B89;
  GL_ACTIVE_ATTRIBUTE_MAX_LENGTH = $8B8A;
  GL_FRAGMENT_SHADER_DERIVATIVE_HINT = $8B8B;
  GL_SHADING_LANGUAGE_VERSION = $8B8C;
  GL_CURRENT_PROGRAM = $8B8D;
  GL_POINT_SPRITE_COORD_ORIGIN = $8CA0;
  GL_LOWER_LEFT = $8CA1;
  GL_UPPER_LEFT = $8CA2;
  GL_STENCIL_BACK_REF = $8CA3;
  GL_STENCIL_BACK_VALUE_MASK = $8CA4;
  GL_STENCIL_BACK_WRITEMASK = $8CA5;

  // GL_VERSION_2_1
  GL_CURRENT_RASTER_SECONDARY_COLOR = $845F;
  GL_PIXEL_PACK_BUFFER = $88EB;
  GL_PIXEL_UNPACK_BUFFER = $88EC;
  GL_PIXEL_PACK_BUFFER_BINDING = $88ED;
  GL_PIXEL_UNPACK_BUFFER_BINDING = $88EF;
  GL_FLOAT_MAT2x3 = $8B65;
  GL_FLOAT_MAT2x4 = $8B66;
  GL_FLOAT_MAT3x2 = $8B67;
  GL_FLOAT_MAT3x4 = $8B68;
  GL_FLOAT_MAT4x2 = $8B69;
  GL_FLOAT_MAT4x3 = $8B6A;
  GL_SRGB = $8C40;
  GL_SRGB8 = $8C41;
  GL_SRGB_ALPHA = $8C42;
  GL_SRGB8_ALPHA8 = $8C43;
  GL_SLUMINANCE_ALPHA = $8C44;
  GL_SLUMINANCE8_ALPHA8 = $8C45;
  GL_SLUMINANCE = $8C46;
  GL_SLUMINANCE8 = $8C47;
  GL_COMPRESSED_SRGB = $8C48;
  GL_COMPRESSED_SRGB_ALPHA = $8C49;
  GL_COMPRESSED_SLUMINANCE = $8C4A;
  GL_COMPRESSED_SLUMINANCE_ALPHA = $8C4B;

  // GL_VERSION_3_0
  GL_COMPARE_REF_TO_TEXTURE = $884E; // GL_COMPARE_R_TO_TEXTURE_ARB;
  GL_CLIP_DISTANCE0 = GL_CLIP_PLANE0;
  GL_CLIP_DISTANCE1 = GL_CLIP_PLANE1;
  GL_CLIP_DISTANCE2 = GL_CLIP_PLANE2;
  GL_CLIP_DISTANCE3 = GL_CLIP_PLANE3;
  GL_CLIP_DISTANCE4 = GL_CLIP_PLANE4;
  GL_CLIP_DISTANCE5 = GL_CLIP_PLANE5;
  GL_MAX_CLIP_DISTANCES = GL_MAX_CLIP_PLANES;
  GL_MAJOR_VERSION = $821B;
  GL_MINOR_VERSION = $821C;
  GL_NUM_EXTENSIONS = $821D;
  GL_CONTEXT_FLAGS = $821E;
  GL_DEPTH_BUFFER = $8223;
  GL_STENCIL_BUFFER = $8224;
  GL_COMPRESSED_RED = $8225;
  GL_COMPRESSED_RG = $8226;
  GL_CONTEXT_FLAG_FORWARD_COMPATIBLE_BIT = $0001;
  GL_RGBA32F = $8814;
  GL_RGB32F = $8815;
  GL_RGBA16F = $881A;
  GL_RGB16F = $881B;
  GL_VERTEX_ATTRIB_ARRAY_INTEGER = $88FD;
  GL_MAX_ARRAY_TEXTURE_LAYERS = $88FF;
  GL_MIN_PROGRAM_TEXEL_OFFSET = $8904;
  GL_MAX_PROGRAM_TEXEL_OFFSET = $8905;
  GL_CLAMP_VERTEX_COLOR = $891A;
  GL_CLAMP_FRAGMENT_COLOR = $891B;
  GL_CLAMP_READ_COLOR = $891C;
  GL_FIXED_ONLY = $891D;
  GL_MAX_VARYING_COMPONENTS = GL_MAX_VARYING_FLOATS;
  GL_TEXTURE_RED_TYPE = $8C10;
  GL_TEXTURE_GREEN_TYPE = $8C11;
  GL_TEXTURE_BLUE_TYPE = $8C12;
  GL_TEXTURE_ALPHA_TYPE = $8C13;
  GL_TEXTURE_LUMINANCE_TYPE = $8C14;
  GL_TEXTURE_INTENSITY_TYPE = $8C15;
  GL_TEXTURE_DEPTH_TYPE = $8C16;
  GL_UNSIGNED_NORMALIZED = $8C17;
  GL_TEXTURE_1D_ARRAY = $8C18;
  GL_PROXY_TEXTURE_1D_ARRAY = $8C19;
  GL_TEXTURE_2D_ARRAY = $8C1A;
  GL_PROXY_TEXTURE_2D_ARRAY = $8C1B;
  GL_TEXTURE_BINDING_1D_ARRAY = $8C1C;
  GL_TEXTURE_BINDING_2D_ARRAY = $8C1D;
  GL_R11F_G11F_B10F = $8C3A;
  GL_UNSIGNED_INT_10F_11F_11F_REV = $8C3B;
  GL_RGB9_E5 = $8C3D;
  GL_UNSIGNED_INT_5_9_9_9_REV = $8C3E;
  GL_TEXTURE_SHARED_SIZE = $8C3F;
  GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH = $8C76;
  GL_TRANSFORM_FEEDBACK_BUFFER_MODE = $8C7F;
  GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS = $8C80;
  GL_TRANSFORM_FEEDBACK_VARYINGS = $8C83;
  GL_TRANSFORM_FEEDBACK_BUFFER_START = $8C84;
  GL_TRANSFORM_FEEDBACK_BUFFER_SIZE = $8C85;
  GL_PRIMITIVES_GENERATED = $8C87;
  GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN = $8C88;
  GL_RASTERIZER_DISCARD = $8C89;
  GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS = $8C8A;
  GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS = $8C8B;
  GL_INTERLEAVED_ATTRIBS = $8C8C;
  GL_SEPARATE_ATTRIBS = $8C8D;
  GL_TRANSFORM_FEEDBACK_BUFFER = $8C8E;
  GL_TRANSFORM_FEEDBACK_BUFFER_BINDING = $8C8F;
  GL_RGBA32UI = $8D70;
  GL_RGB32UI = $8D71;
  GL_RGBA16UI = $8D76;
  GL_RGB16UI = $8D77;
  GL_RGBA8UI = $8D7C;
  GL_RGB8UI = $8D7D;
  GL_RGBA32I = $8D82;
  GL_RGB32I = $8D83;
  GL_RGBA16I = $8D88;
  GL_RGB16I = $8D89;
  GL_RGBA8I = $8D8E;
  GL_RGB8I = $8D8F;
  GL_RED_INTEGER = $8D94;
  GL_GREEN_INTEGER = $8D95;
  GL_BLUE_INTEGER = $8D96;
  GL_ALPHA_INTEGER = $8D97;
  GL_RGB_INTEGER = $8D98;
  GL_RGBA_INTEGER = $8D99;
  GL_BGR_INTEGER = $8D9A;
  GL_BGRA_INTEGER = $8D9B;
  GL_SAMPLER_1D_ARRAY = $8DC0;
  GL_SAMPLER_2D_ARRAY = $8DC1;
  GL_SAMPLER_1D_ARRAY_SHADOW = $8DC3;
  GL_SAMPLER_2D_ARRAY_SHADOW = $8DC4;
  GL_SAMPLER_CUBE_SHADOW = $8DC5;
  GL_UNSIGNED_INT_VEC2 = $8DC6;
  GL_UNSIGNED_INT_VEC3 = $8DC7;
  GL_UNSIGNED_INT_VEC4 = $8DC8;
  GL_INT_SAMPLER_1D = $8DC9;
  GL_INT_SAMPLER_2D = $8DCA;
  GL_INT_SAMPLER_3D = $8DCB;
  GL_INT_SAMPLER_CUBE = $8DCC;
  GL_INT_SAMPLER_1D_ARRAY = $8DCE;
  GL_INT_SAMPLER_2D_ARRAY = $8DCF;
  GL_UNSIGNED_INT_SAMPLER_1D = $8DD1;
  GL_UNSIGNED_INT_SAMPLER_2D = $8DD2;
  GL_UNSIGNED_INT_SAMPLER_3D = $8DD3;
  GL_UNSIGNED_INT_SAMPLER_CUBE = $8DD4;
  GL_UNSIGNED_INT_SAMPLER_1D_ARRAY = $8DD6;
  GL_UNSIGNED_INT_SAMPLER_2D_ARRAY = $8DD7;
  GL_QUERY_WAIT = $8E13;
  GL_QUERY_NO_WAIT = $8E14;
  GL_QUERY_BY_REGION_WAIT = $8E15;
  GL_QUERY_BY_REGION_NO_WAIT = $8E16;
  { Reuse tokens from ARB_depth_buffer_float }
  { Reuse tokens from ARB_framebuffer_object }
  { Reuse tokens from ARB_framebuffer_sRGB }
  { Reuse tokens from ARB_half_float_vertex }
  { Reuse tokens from ARB_map_buffer_range }
  { Reuse tokens from ARB_texture_compression_rgtc }
  { Reuse tokens from ARB_texture_rg }
  { Reuse tokens from ARB_vertex_array_object }

  // GL_3DFX_multisample
  GL_MULTISAMPLE_3DFX = $86B2;
  GL_SAMPLE_BUFFERS_3DFX = $86B3;
  GL_SAMPLES_3DFX = $86B4;
  GL_MULTISAMPLE_BIT_3DFX = $20000000;

  // GL_3DFX_texture_compression_FXT1
  GL_COMPRESSED_RGB_FXT1_3DFX = $86B0;
  GL_COMPRESSED_RGBA_FXT1_3DFX = $86B1;

  // GL_APPLE_client_storage
  GL_UNPACK_CLIENT_STORAGE_APPLE = $85B2;

  // GL_APPLE_element_array
  GL_ELEMENT_ARRAY_APPLE = $8768;
  GL_ELEMENT_ARRAY_TYPE_APPLE = $8769;
  GL_ELEMENT_ARRAY_POINTER_APPLE = $876A;

  // GL_APPLE_fence
  GL_DRAW_PIXELS_APPLE = $8A0A;
  GL_FENCE_APPLE = $8A0B;

  // GL_APPLE_specular_vector
  GL_LIGHT_MODEL_SPECULAR_VECTOR_APPLE = $85B0;

  // GL_APPLE_transform_hint
  GL_TRANSFORM_HINT_APPLE = $85B1;

  // GL_APPLE_vertex_array_object
  GL_VERTEX_ARRAY_BINDING_APPLE = $85B5;

  // GL_APPLE_vertex_array_range
  GL_VERTEX_ARRAY_RANGE_APPLE = $851D;
  GL_VERTEX_ARRAY_RANGE_LENGTH_APPLE = $851E;
  GL_VERTEX_ARRAY_STORAGE_HINT_APPLE = $851F;
  GL_VERTEX_ARRAY_RANGE_POINTER_APPLE = $8521;
  GL_STORAGE_CACHED_APPLE = $85BE;
  GL_STORAGE_SHARED_APPLE = $85BF;

  // GL_APPLE_ycbcr_422
  GL_YCBCR_422_APPLE = $85B9;
  GL_UNSIGNED_SHORT_8_8_APPLE = $85BA;
  GL_UNSIGNED_SHORT_8_8_REV_APPLE = $85BB;

  // GL_ARB_depth_texture
  GL_DEPTH_COMPONENT16_ARB = $81A5;
  GL_DEPTH_COMPONENT24_ARB = $81A6;
  GL_DEPTH_COMPONENT32_ARB = $81A7;
  GL_TEXTURE_DEPTH_SIZE_ARB = $884A;
  GL_DEPTH_TEXTURE_MODE_ARB = $884B;

  // GL_ARB_fragment_program
  GL_FRAGMENT_PROGRAM_ARB = $8804;
  GL_PROGRAM_ALU_INSTRUCTIONS_ARB = $8805;
  GL_PROGRAM_TEX_INSTRUCTIONS_ARB = $8806;
  GL_PROGRAM_TEX_INDIRECTIONS_ARB = $8807;
  GL_PROGRAM_NATIVE_ALU_INSTRUCTIONS_ARB = $8808;
  GL_PROGRAM_NATIVE_TEX_INSTRUCTIONS_ARB = $8809;
  GL_PROGRAM_NATIVE_TEX_INDIRECTIONS_ARB = $880A;
  GL_MAX_PROGRAM_ALU_INSTRUCTIONS_ARB = $880B;
  GL_MAX_PROGRAM_TEX_INSTRUCTIONS_ARB = $880C;
  GL_MAX_PROGRAM_TEX_INDIRECTIONS_ARB = $880D;
  GL_MAX_PROGRAM_NATIVE_ALU_INSTRUCTIONS_ARB = $880E;
  GL_MAX_PROGRAM_NATIVE_TEX_INSTRUCTIONS_ARB = $880F;
  GL_MAX_PROGRAM_NATIVE_TEX_INDIRECTIONS_ARB = $8810;
  GL_MAX_TEXTURE_COORDS_ARB = $8871;
  GL_MAX_TEXTURE_IMAGE_UNITS_ARB = $8872;

  // GL_ARB_imaging
  GL_CONSTANT_COLOR_ARB = $8001;
  GL_ONE_MINUS_CONSTANT_COLOR = $8002;
  GL_CONSTANT_ALPHA = $8003;
  GL_ONE_MINUS_CONSTANT_ALPHA = $8004;
  GL_BLEND_COLOR = $8005;
  GL_FUNC_ADD = $8006;
  GL_MIN = $8007;
  GL_MAX = $8008;
  GL_BLEND_EQUATION = $8009;
  GL_FUNC_SUBTRACT = $800A;
  GL_FUNC_REVERSE_SUBTRACT = $800B;
  GL_CONVOLUTION_1D = $8010;
  GL_CONVOLUTION_2D = $8011;
  GL_SEPARABLE_2D = $8012;
  GL_CONVOLUTION_BORDER_MODE = $8013;
  GL_CONVOLUTION_FILTER_SCALE = $8014;
  GL_CONVOLUTION_FILTER_BIAS = $8015;
  GL_REDUCE = $8016;
  GL_CONVOLUTION_FORMAT = $8017;
  GL_CONVOLUTION_WIDTH = $8018;
  GL_CONVOLUTION_HEIGHT = $8019;
  GL_MAX_CONVOLUTION_WIDTH = $801A;
  GL_MAX_CONVOLUTION_HEIGHT = $801B;
  GL_POST_CONVOLUTION_RED_SCALE = $801C;
  GL_POST_CONVOLUTION_GREEN_SCALE = $801D;
  GL_POST_CONVOLUTION_BLUE_SCALE = $801E;
  GL_POST_CONVOLUTION_ALPHA_SCALE = $801F;
  GL_POST_CONVOLUTION_RED_BIAS = $8020;
  GL_POST_CONVOLUTION_GREEN_BIAS = $8021;
  GL_POST_CONVOLUTION_BLUE_BIAS = $8022;
  GL_POST_CONVOLUTION_ALPHA_BIAS = $8023;
  GL_HISTOGRAM = $8024;
  GL_PROXY_HISTOGRAM = $8025;
  GL_HISTOGRAM_WIDTH = $8026;
  GL_HISTOGRAM_FORMAT = $8027;
  GL_HISTOGRAM_RED_SIZE = $8028;
  GL_HISTOGRAM_GREEN_SIZE = $8029;
  GL_HISTOGRAM_BLUE_SIZE = $802A;
  GL_HISTOGRAM_ALPHA_SIZE = $802B;
  GL_HISTOGRAM_LUMINANCE_SIZE = $802C;
  GL_HISTOGRAM_SINK = $802D;
  GL_MINMAX = $802E;
  GL_MINMAX_FORMAT = $802F;
  GL_MINMAX_SINK = $8030;
  GL_TABLE_TOO_LARGE = $8031;
  GL_COLOR_MATRIX = $80B1;
  GL_COLOR_MATRIX_STACK_DEPTH = $80B2;
  GL_MAX_COLOR_MATRIX_STACK_DEPTH = $80B3;
  GL_POST_COLOR_MATRIX_RED_SCALE = $80B4;
  GL_POST_COLOR_MATRIX_GREEN_SCALE = $80B5;
  GL_POST_COLOR_MATRIX_BLUE_SCALE = $80B6;
  GL_POST_COLOR_MATRIX_ALPHA_SCALE = $80B7;
  GL_POST_COLOR_MATRIX_RED_BIAS = $80B8;
  GL_POST_COLOR_MATRIX_GREEN_BIAS = $80B9;
  GL_POST_COLOR_MATRIX_BLUE_BIAS = $80BA;
  GL_POST_COLOR_MATRIX_ALPHA_BIAS = $80BB;
  GL_COLOR_TABLE = $80D0;
  GL_POST_CONVOLUTION_COLOR_TABLE = $80D1;
  GL_POST_COLOR_MATRIX_COLOR_TABLE = $80D2;
  GL_PROXY_COLOR_TABLE = $80D3;
  GL_PROXY_POST_CONVOLUTION_COLOR_TABLE = $80D4;
  GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE = $80D5;
  GL_COLOR_TABLE_SCALE = $80D6;
  GL_COLOR_TABLE_BIAS = $80D7;
  GL_COLOR_TABLE_FORMAT = $80D8;
  GL_COLOR_TABLE_WIDTH = $80D9;
  GL_COLOR_TABLE_RED_SIZE = $80DA;
  GL_COLOR_TABLE_GREEN_SIZE = $80DB;
  GL_COLOR_TABLE_BLUE_SIZE = $80DC;
  GL_COLOR_TABLE_ALPHA_SIZE = $80DD;
  GL_COLOR_TABLE_LUMINANCE_SIZE = $80DE;
  GL_COLOR_TABLE_INTENSITY_SIZE = $80DF;
  GL_CONSTANT_BORDER = $8151;
  GL_REPLICATE_BORDER = $8153;
  GL_CONVOLUTION_BORDER_COLOR = $8154;

  // GL_ARB_matrix_palette
  GL_MATRIX_PALETTE_ARB = $8840;
  GL_MAX_MATRIX_PALETTE_STACK_DEPTH_ARB = $8841;
  GL_MAX_PALETTE_MATRICES_ARB = $8842;
  GL_CURRENT_PALETTE_MATRIX_ARB = $8843;
  GL_MATRIX_INDEX_ARRAY_ARB = $8844;
  GL_CURRENT_MATRIX_INDEX_ARB = $8845;
  GL_MATRIX_INDEX_ARRAY_SIZE_ARB = $8846;
  GL_MATRIX_INDEX_ARRAY_TYPE_ARB = $8847;
  GL_MATRIX_INDEX_ARRAY_STRIDE_ARB = $8848;
  GL_MATRIX_INDEX_ARRAY_POINTER_ARB = $8849;

  // GL_ARB_multisample
  GL_MULTISAMPLE_ARB = $809D;
  GL_SAMPLE_ALPHA_TO_COVERAGE_ARB = $809E;
  GL_SAMPLE_ALPHA_TO_ONE_ARB = $809F;
  GL_SAMPLE_COVERAGE_ARB = $80A0;
  GL_SAMPLE_BUFFERS_ARB = $80A8;
  GL_SAMPLES_ARB = $80A9;
  GL_SAMPLE_COVERAGE_VALUE_ARB = $80AA;
  GL_SAMPLE_COVERAGE_INVERT_ARB = $80AB;
  GL_MULTISAMPLE_BIT_ARB = $20000000;

  // GL_ARB_multitexture
  GL_TEXTURE0_ARB = $84C0;
  GL_TEXTURE1_ARB = $84C1;
  GL_TEXTURE2_ARB = $84C2;
  GL_TEXTURE3_ARB = $84C3;
  GL_TEXTURE4_ARB = $84C4;
  GL_TEXTURE5_ARB = $84C5;
  GL_TEXTURE6_ARB = $84C6;
  GL_TEXTURE7_ARB = $84C7;
  GL_TEXTURE8_ARB = $84C8;
  GL_TEXTURE9_ARB = $84C9;
  GL_TEXTURE10_ARB = $84CA;
  GL_TEXTURE11_ARB = $84CB;
  GL_TEXTURE12_ARB = $84CC;
  GL_TEXTURE13_ARB = $84CD;
  GL_TEXTURE14_ARB = $84CE;
  GL_TEXTURE15_ARB = $84CF;
  GL_TEXTURE16_ARB = $84D0;
  GL_TEXTURE17_ARB = $84D1;
  GL_TEXTURE18_ARB = $84D2;
  GL_TEXTURE19_ARB = $84D3;
  GL_TEXTURE20_ARB = $84D4;
  GL_TEXTURE21_ARB = $84D5;
  GL_TEXTURE22_ARB = $84D6;
  GL_TEXTURE23_ARB = $84D7;
  GL_TEXTURE24_ARB = $84D8;
  GL_TEXTURE25_ARB = $84D9;
  GL_TEXTURE26_ARB = $84DA;
  GL_TEXTURE27_ARB = $84DB;
  GL_TEXTURE28_ARB = $84DC;
  GL_TEXTURE29_ARB = $84DD;
  GL_TEXTURE30_ARB = $84DE;
  GL_TEXTURE31_ARB = $84DF;
  GL_ACTIVE_TEXTURE_ARB = $84E0;
  GL_CLIENT_ACTIVE_TEXTURE_ARB = $84E1;
  GL_MAX_TEXTURE_UNITS_ARB = $84E2;

  // GL_ARB_point_parameters
  GL_POINT_SIZE_MIN_ARB = $8126;
  GL_POINT_SIZE_MAX_ARB = $8127;
  GL_POINT_FADE_THRESHOLD_SIZE_ARB = $8128;
  GL_POINT_DISTANCE_ATTENUATION_ARB = $8129;

  // GL_ARB_shadow
  GL_TEXTURE_COMPARE_MODE_ARB = $884C;
  GL_TEXTURE_COMPARE_FUNC_ARB = $884D;
  GL_COMPARE_R_TO_TEXTURE_ARB = $884E;

  // GL_ARB_shadow_ambient
  GL_TEXTURE_COMPARE_FAIL_VALUE_ARB = $80BF;

  // GL_ARB_texture_border_clamp
  GL_CLAMP_TO_BORDER_ARB = $812D;

  // GL_ARB_texture_compression
  GL_COMPRESSED_ALPHA_ARB = $84E9;
  GL_COMPRESSED_LUMINANCE_ARB = $84EA;
  GL_COMPRESSED_LUMINANCE_ALPHA_ARB = $84EB;
  GL_COMPRESSED_INTENSITY_ARB = $84EC;
  GL_COMPRESSED_RGB_ARB = $84ED;
  GL_COMPRESSED_RGBA_ARB = $84EE;
  GL_TEXTURE_COMPRESSION_HINT_ARB = $84EF;
  GL_TEXTURE_COMPRESSED_IMAGE_SIZE_ARB = $86A0;
  GL_TEXTURE_COMPRESSED_ARB = $86A1;
  GL_NUM_COMPRESSED_TEXTURE_FORMATS_ARB = $86A2;
  GL_COMPRESSED_TEXTURE_FORMATS_ARB = $86A3;

  // GL_ARB_texture_cube_map
  GL_NORMAL_MAP_ARB = $8511;
  GL_REFLECTION_MAP_ARB = $8512;
  GL_TEXTURE_CUBE_MAP_ARB = $8513;
  GL_TEXTURE_BINDING_CUBE_MAP_ARB = $8514;
  GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB = $8515;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB = $8516;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB = $8517;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB = $8518;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB = $8519;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB = $851A;
  GL_PROXY_TEXTURE_CUBE_MAP_ARB = $851B;
  GL_MAX_CUBE_MAP_TEXTURE_SIZE_ARB = $851C;

  // GL_ARB_texture_env_combine
  GL_COMBINE_ARB = $8570;
  GL_COMBINE_RGB_ARB = $8571;
  GL_COMBINE_ALPHA_ARB = $8572;
  GL_SOURCE0_RGB_ARB = $8580;
  GL_SOURCE1_RGB_ARB = $8581;
  GL_SOURCE2_RGB_ARB = $8582;
  GL_SOURCE0_ALPHA_ARB = $8588;
  GL_SOURCE1_ALPHA_ARB = $8589;
  GL_SOURCE2_ALPHA_ARB = $858A;
  GL_OPERAND0_RGB_ARB = $8590;
  GL_OPERAND1_RGB_ARB = $8591;
  GL_OPERAND2_RGB_ARB = $8592;
  GL_OPERAND0_ALPHA_ARB = $8598;
  GL_OPERAND1_ALPHA_ARB = $8599;
  GL_OPERAND2_ALPHA_ARB = $859A;
  GL_RGB_SCALE_ARB = $8573;
  GL_ADD_SIGNED_ARB = $8574;
  GL_INTERPOLATE_ARB = $8575;
  GL_SUBTRACT_ARB = $84E7;
  GL_CONSTANT_ARB = $8576;
  GL_PRIMARY_COLOR_ARB = $8577;
  GL_PREVIOUS_ARB = $8578;

  // GL_ARB_texture_env_dot3
  GL_DOT3_RGB_ARB = $86AE;
  GL_DOT3_RGBA_ARB = $86AF;

  // GL_ARB_texture_mirrored_repeat
  GL_MIRRORED_REPEAT_ARB = $8370;

  // GL_ARB_transpose_matrix
  GL_TRANSPOSE_MODELVIEW_MATRIX_ARB = $84E3;
  GL_TRANSPOSE_PROJECTION_MATRIX_ARB = $84E4;
  GL_TRANSPOSE_TEXTURE_MATRIX_ARB = $84E5;
  GL_TRANSPOSE_COLOR_MATRIX_ARB = $84E6;

  // GL_ARB_vertex_blend
  GL_MAX_VERTEX_UNITS_ARB = $86A4;
  GL_ACTIVE_VERTEX_UNITS_ARB = $86A5;
  GL_WEIGHT_SUM_UNITY_ARB = $86A6;
  GL_VERTEX_BLEND_ARB = $86A7;
  GL_CURRENT_WEIGHT_ARB = $86A8;
  GL_WEIGHT_ARRAY_TYPE_ARB = $86A9;
  GL_WEIGHT_ARRAY_STRIDE_ARB = $86AA;
  GL_WEIGHT_ARRAY_SIZE_ARB = $86AB;
  GL_WEIGHT_ARRAY_POINTER_ARB = $86AC;
  GL_WEIGHT_ARRAY_ARB = $86AD;
  GL_MODELVIEW0_ARB = $1700;
  GL_MODELVIEW1_ARB = $850A;
  GL_MODELVIEW2_ARB = $8722;
  GL_MODELVIEW3_ARB = $8723;
  GL_MODELVIEW4_ARB = $8724;
  GL_MODELVIEW5_ARB = $8725;
  GL_MODELVIEW6_ARB = $8726;
  GL_MODELVIEW7_ARB = $8727;
  GL_MODELVIEW8_ARB = $8728;
  GL_MODELVIEW9_ARB = $8729;
  GL_MODELVIEW10_ARB = $872A;
  GL_MODELVIEW11_ARB = $872B;
  GL_MODELVIEW12_ARB = $872C;
  GL_MODELVIEW13_ARB = $872D;
  GL_MODELVIEW14_ARB = $872E;
  GL_MODELVIEW15_ARB = $872F;
  GL_MODELVIEW16_ARB = $8730;
  GL_MODELVIEW17_ARB = $8731;
  GL_MODELVIEW18_ARB = $8732;
  GL_MODELVIEW19_ARB = $8733;
  GL_MODELVIEW20_ARB = $8734;
  GL_MODELVIEW21_ARB = $8735;
  GL_MODELVIEW22_ARB = $8736;
  GL_MODELVIEW23_ARB = $8737;
  GL_MODELVIEW24_ARB = $8738;
  GL_MODELVIEW25_ARB = $8739;
  GL_MODELVIEW26_ARB = $873A;
  GL_MODELVIEW27_ARB = $873B;
  GL_MODELVIEW28_ARB = $873C;
  GL_MODELVIEW29_ARB = $873D;
  GL_MODELVIEW30_ARB = $873E;
  GL_MODELVIEW31_ARB = $873F;

  // GL_ARB_vertex_buffer_object
  GL_BUFFER_SIZE_ARB = $8764;
  GL_BUFFER_USAGE_ARB = $8765;
  GL_ARRAY_BUFFER_ARB = $8892;
  GL_ELEMENT_ARRAY_BUFFER_ARB = $8893;
  GL_ARRAY_BUFFER_BINDING_ARB = $8894;
  GL_ELEMENT_ARRAY_BUFFER_BINDING_ARB = $8895;
  GL_VERTEX_ARRAY_BUFFER_BINDING_ARB = $8896;
  GL_NORMAL_ARRAY_BUFFER_BINDING_ARB = $8897;
  GL_COLOR_ARRAY_BUFFER_BINDING_ARB = $8898;
  GL_INDEX_ARRAY_BUFFER_BINDING_ARB = $8899;
  GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING_ARB = $889A;
  GL_EDGE_FLAG_ARRAY_BUFFER_BINDING_ARB = $889B;
  GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING_ARB = $889C;
  GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING_ARB = $889D;
  GL_WEIGHT_ARRAY_BUFFER_BINDING_ARB = $889E;
  GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING_ARB = $889F;
  GL_READ_ONLY_ARB = $88B8;
  GL_WRITE_ONLY_ARB = $88B9;
  GL_READ_WRITE_ARB = $88BA;
  GL_BUFFER_ACCESS_ARB = $88BB;
  GL_BUFFER_MAPPED_ARB = $88BC;
  GL_BUFFER_MAP_POINTER_ARB = $88BD;
  GL_STREAM_DRAW_ARB = $88E0;
  GL_STREAM_READ_ARB = $88E1;
  GL_STREAM_COPY_ARB = $88E2;
  GL_STATIC_DRAW_ARB = $88E4;
  GL_STATIC_READ_ARB = $88E5;
  GL_STATIC_COPY_ARB = $88E6;
  GL_DYNAMIC_DRAW_ARB = $88E8;
  GL_DYNAMIC_READ_ARB = $88E9;
  GL_DYNAMIC_COPY_ARB = $88EA;

  // GL_ARB_vertex_program
  GL_COLOR_SUM_ARB = $8458;
  GL_VERTEX_PROGRAM_ARB = $8620;
  GL_VERTEX_ATTRIB_ARRAY_ENABLED_ARB = $8622;
  GL_VERTEX_ATTRIB_ARRAY_SIZE_ARB = $8623;
  GL_VERTEX_ATTRIB_ARRAY_STRIDE_ARB = $8624;
  GL_VERTEX_ATTRIB_ARRAY_TYPE_ARB = $8625;
  GL_CURRENT_VERTEX_ATTRIB_ARB = $8626;
  GL_PROGRAM_LENGTH_ARB = $8627;
  GL_PROGRAM_STRING_ARB = $8628;
  GL_MAX_PROGRAM_MATRIX_STACK_DEPTH_ARB = $862E;
  GL_MAX_PROGRAM_MATRICES_ARB = $862F;
  GL_CURRENT_MATRIX_STACK_DEPTH_ARB = $8640;
  GL_CURRENT_MATRIX_ARB = $8641;
  GL_VERTEX_PROGRAM_POINT_SIZE_ARB = $8642;
  GL_VERTEX_PROGRAM_TWO_SIDE_ARB = $8643;
  GL_VERTEX_ATTRIB_ARRAY_POINTER_ARB = $8645;
  GL_PROGRAM_ERROR_POSITION_ARB = $864B;
  GL_PROGRAM_BINDING_ARB = $8677;
  GL_MAX_VERTEX_ATTRIBS_ARB = $8869;
  GL_VERTEX_ATTRIB_ARRAY_NORMALIZED_ARB = $886A;
  GL_PROGRAM_ERROR_STRING_ARB = $8874;
  GL_PROGRAM_FORMAT_ASCII_ARB = $8875;
  GL_PROGRAM_FORMAT_ARB = $8876;
  GL_PROGRAM_INSTRUCTIONS_ARB = $88A0;
  GL_MAX_PROGRAM_INSTRUCTIONS_ARB = $88A1;
  GL_PROGRAM_NATIVE_INSTRUCTIONS_ARB = $88A2;
  GL_MAX_PROGRAM_NATIVE_INSTRUCTIONS_ARB = $88A3;
  GL_PROGRAM_TEMPORARIES_ARB = $88A4;
  GL_MAX_PROGRAM_TEMPORARIES_ARB = $88A5;
  GL_PROGRAM_NATIVE_TEMPORARIES_ARB = $88A6;
  GL_MAX_PROGRAM_NATIVE_TEMPORARIES_ARB = $88A7;
  GL_PROGRAM_PARAMETERS_ARB = $88A8;
  GL_MAX_PROGRAM_PARAMETERS_ARB = $88A9;
  GL_PROGRAM_NATIVE_PARAMETERS_ARB = $88AA;
  GL_MAX_PROGRAM_NATIVE_PARAMETERS_ARB = $88AB;
  GL_PROGRAM_ATTRIBS_ARB = $88AC;
  GL_MAX_PROGRAM_ATTRIBS_ARB = $88AD;
  GL_PROGRAM_NATIVE_ATTRIBS_ARB = $88AE;
  GL_MAX_PROGRAM_NATIVE_ATTRIBS_ARB = $88AF;
  GL_PROGRAM_ADDRESS_REGISTERS_ARB = $88B0;
  GL_MAX_PROGRAM_ADDRESS_REGISTERS_ARB = $88B1;
  GL_PROGRAM_NATIVE_ADDRESS_REGISTERS_ARB = $88B2;
  GL_MAX_PROGRAM_NATIVE_ADDRESS_REGISTERS_ARB = $88B3;
  GL_MAX_PROGRAM_LOCAL_PARAMETERS_ARB = $88B4;
  GL_MAX_PROGRAM_ENV_PARAMETERS_ARB = $88B5;
  GL_PROGRAM_UNDER_NATIVE_LIMITS_ARB = $88B6;
  GL_TRANSPOSE_CURRENT_MATRIX_ARB = $88B7;
  GL_MATRIX0_ARB = $88C0;
  GL_MATRIX1_ARB = $88C1;
  GL_MATRIX2_ARB = $88C2;
  GL_MATRIX3_ARB = $88C3;
  GL_MATRIX4_ARB = $88C4;
  GL_MATRIX5_ARB = $88C5;
  GL_MATRIX6_ARB = $88C6;
  GL_MATRIX7_ARB = $88C7;
  GL_MATRIX8_ARB = $88C8;
  GL_MATRIX9_ARB = $88C9;
  GL_MATRIX10_ARB = $88CA;
  GL_MATRIX11_ARB = $88CB;
  GL_MATRIX12_ARB = $88CC;
  GL_MATRIX13_ARB = $88CD;
  GL_MATRIX14_ARB = $88CE;
  GL_MATRIX15_ARB = $88CF;
  GL_MATRIX16_ARB = $88D0;
  GL_MATRIX17_ARB = $88D1;
  GL_MATRIX18_ARB = $88D2;
  GL_MATRIX19_ARB = $88D3;
  GL_MATRIX20_ARB = $88D4;
  GL_MATRIX21_ARB = $88D5;
  GL_MATRIX22_ARB = $88D6;
  GL_MATRIX23_ARB = $88D7;
  GL_MATRIX24_ARB = $88D8;
  GL_MATRIX25_ARB = $88D9;
  GL_MATRIX26_ARB = $88DA;
  GL_MATRIX27_ARB = $88DB;
  GL_MATRIX28_ARB = $88DC;
  GL_MATRIX29_ARB = $88DD;
  GL_MATRIX30_ARB = $88DE;
  GL_MATRIX31_ARB = $88DF;

  // GL_ARB_draw_buffers
  GL_MAX_DRAW_BUFFERS_ARB = $8824;
  GL_DRAW_BUFFER0_ARB = $8825;
  GL_DRAW_BUFFER1_ARB = $8826;
  GL_DRAW_BUFFER2_ARB = $8827;
  GL_DRAW_BUFFER3_ARB = $8828;
  GL_DRAW_BUFFER4_ARB = $8829;
  GL_DRAW_BUFFER5_ARB = $882A;
  GL_DRAW_BUFFER6_ARB = $882B;
  GL_DRAW_BUFFER7_ARB = $882C;
  GL_DRAW_BUFFER8_ARB = $882D;
  GL_DRAW_BUFFER9_ARB = $882E;
  GL_DRAW_BUFFER10_ARB = $882F;
  GL_DRAW_BUFFER11_ARB = $8830;
  GL_DRAW_BUFFER12_ARB = $8831;
  GL_DRAW_BUFFER13_ARB = $8832;
  GL_DRAW_BUFFER14_ARB = $8833;
  GL_DRAW_BUFFER15_ARB = $8834;

  // GL_ARB_texture_rectangle
  GL_TEXTURE_RECTANGLE_ARB = $84F5;
  GL_TEXTURE_BINDING_RECTANGLE_ARB = $84F6;
  GL_PROXY_TEXTURE_RECTANGLE_ARB = $84F7;
  GL_MAX_RECTANGLE_TEXTURE_SIZE_ARB = $84F8;

  // GL_ARB_color_buffer_float
  GL_RGBA_FLOAT_MODE_ARB = $8820;
  GL_CLAMP_VERTEX_COLOR_ARB = $891A;
  GL_CLAMP_FRAGMENT_COLOR_ARB = $891B;
  GL_CLAMP_READ_COLOR_ARB = $891C;
  GL_FIXED_ONLY_ARB = $891D;
  WGL_TYPE_RGBA_FLOAT_ARB = $21A0;
  GLX_RGBA_FLOAT_TYPE = $20B9;
  GLX_RGBA_FLOAT_BIT = $00000004;

  // GL_ARB_half_float_pixel
  GL_HALF_FLOAT_ARB = $140B;

  // GL_ARB_texture_float
  GL_TEXTURE_RED_TYPE_ARB = $8C10;
  GL_TEXTURE_GREEN_TYPE_ARB = $8C11;
  GL_TEXTURE_BLUE_TYPE_ARB = $8C12;
  GL_TEXTURE_ALPHA_TYPE_ARB = $8C13;
  GL_TEXTURE_LUMINANCE_TYPE_ARB = $8C14;
  GL_TEXTURE_INTENSITY_TYPE_ARB = $8C15;
  GL_TEXTURE_DEPTH_TYPE_ARB = $8C16;
  GL_UNSIGNED_NORMALIZED_ARB = $8C17;
  GL_RGBA32F_ARB = $8814;
  GL_RGB32F_ARB = $8815;
  GL_ALPHA32F_ARB = $8816;
  GL_INTENSITY32F_ARB = $8817;
  GL_LUMINANCE32F_ARB = $8818;
  GL_LUMINANCE_ALPHA32F_ARB = $8819;
  GL_RGBA16F_ARB = $881A;
  GL_RGB16F_ARB = $881B;
  GL_ALPHA16F_ARB = $881C;
  GL_INTENSITY16F_ARB = $881D;
  GL_LUMINANCE16F_ARB = $881E;
  GL_LUMINANCE_ALPHA16F_ARB = $881F;

  // GL_ARB_pixel_buffer_object
  GL_PIXEL_PACK_BUFFER_ARB = $88EB;
  GL_PIXEL_UNPACK_BUFFER_ARB = $88EC;
  GL_PIXEL_PACK_BUFFER_BINDING_ARB = $88ED;
  GL_PIXEL_UNPACK_BUFFER_BINDING_ARB = $88EF;

  // GL_ARB_depth_buffer_float
  GL_DEPTH_COMPONENT32F = $8CAC;
  GL_DEPTH32F_STENCIL8 = $8CAD;
  GL_FLOAT_32_UNSIGNED_INT_24_8_REV = $8DAD;

  // GL_ARB_framebuffer_object
  GL_INVALID_FRAMEBUFFER_OPERATION = $0506;
  GL_FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING = $8210;
  GL_FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE = $8211;
  GL_FRAMEBUFFER_ATTACHMENT_RED_SIZE = $8212;
  GL_FRAMEBUFFER_ATTACHMENT_GREEN_SIZE = $8213;
  GL_FRAMEBUFFER_ATTACHMENT_BLUE_SIZE = $8214;
  GL_FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE = $8215;
  GL_FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE = $8216;
  GL_FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE = $8217;
  GL_FRAMEBUFFER_DEFAULT = $8218;
  GL_FRAMEBUFFER_UNDEFINED = $8219;
  GL_DEPTH_STENCIL_ATTACHMENT = $821A;
  GL_INDEX = $8222;
  GL_MAX_RENDERBUFFER_SIZE = $84E8;
  GL_DEPTH_STENCIL = $84F9;
  GL_UNSIGNED_INT_24_8 = $84FA;
  GL_DEPTH24_STENCIL8 = $88F0;
  GL_TEXTURE_STENCIL_SIZE = $88F1;
  GL_FRAMEBUFFER_BINDING = $8CA6;
  GL_DRAW_FRAMEBUFFER_BINDING = GL_FRAMEBUFFER_BINDING;
  GL_RENDERBUFFER_BINDING = $8CA7;
  GL_READ_FRAMEBUFFER = $8CA8;
  GL_DRAW_FRAMEBUFFER = $8CA9;
  GL_READ_FRAMEBUFFER_BINDING = $8CAA;
  GL_RENDERBUFFER_SAMPLES = $8CAB;
  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE = $8CD0;
  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME = $8CD1;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL = $8CD2;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE = $8CD3;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER = $8CD4;
  GL_FRAMEBUFFER_COMPLETE = $8CD5;
  GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT = $8CD6;
  GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = $8CD7;
  GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER = $8CDB;
  GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER = $8CDC;
  GL_FRAMEBUFFER_UNSUPPORTED = $8CDD;
  GL_MAX_COLOR_ATTACHMENTS = $8CDF;
  GL_COLOR_ATTACHMENT0 = $8CE0;
  GL_COLOR_ATTACHMENT1 = $8CE1;
  GL_COLOR_ATTACHMENT2 = $8CE2;
  GL_COLOR_ATTACHMENT3 = $8CE3;
  GL_COLOR_ATTACHMENT4 = $8CE4;
  GL_COLOR_ATTACHMENT5 = $8CE5;
  GL_COLOR_ATTACHMENT6 = $8CE6;
  GL_COLOR_ATTACHMENT7 = $8CE7;
  GL_COLOR_ATTACHMENT8 = $8CE8;
  GL_COLOR_ATTACHMENT9 = $8CE9;
  GL_COLOR_ATTACHMENT10 = $8CEA;
  GL_COLOR_ATTACHMENT11 = $8CEB;
  GL_COLOR_ATTACHMENT12 = $8CEC;
  GL_COLOR_ATTACHMENT13 = $8CED;
  GL_COLOR_ATTACHMENT14 = $8CEE;
  GL_COLOR_ATTACHMENT15 = $8CEF;
  GL_DEPTH_ATTACHMENT = $8D00;
  GL_STENCIL_ATTACHMENT = $8D20;
  GL_FRAMEBUFFER = $8D40;
  GL_RENDERBUFFER = $8D41;
  GL_RENDERBUFFER_WIDTH = $8D42;
  GL_RENDERBUFFER_HEIGHT = $8D43;
  GL_RENDERBUFFER_INTERNAL_FORMAT = $8D44;
  GL_STENCIL_INDEX1 = $8D46;
  GL_STENCIL_INDEX4 = $8D47;
  GL_STENCIL_INDEX8 = $8D48;
  GL_STENCIL_INDEX16 = $8D49;
  GL_RENDERBUFFER_RED_SIZE = $8D50;
  GL_RENDERBUFFER_GREEN_SIZE = $8D51;
  GL_RENDERBUFFER_BLUE_SIZE = $8D52;
  GL_RENDERBUFFER_ALPHA_SIZE = $8D53;
  GL_RENDERBUFFER_DEPTH_SIZE = $8D54;
  GL_RENDERBUFFER_STENCIL_SIZE = $8D55;
  GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE = $8D56;
  GL_MAX_SAMPLES = $8D57;

  // GL_ARB_framebuffer_sRGB
  GL_FRAMEBUFFER_SRGB = $8DB9;

  // GL_ARB_geometry_shader4
  GL_LINES_ADJACENCY_ARB = $000A;
  GL_LINE_STRIP_ADJACENCY_ARB = $000B;
  GL_TRIANGLES_ADJACENCY_ARB = $000C;
  GL_TRIANGLE_STRIP_ADJACENCY_ARB = $000D;
  GL_PROGRAM_POINT_SIZE_ARB = $8642;
  GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS_ARB = $8C29;
  GL_FRAMEBUFFER_ATTACHMENT_LAYERED_ARB = $8DA7;
  GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS_ARB = $8DA8;
  GL_FRAMEBUFFER_INCOMPLETE_LAYER_COUNT_ARB = $8DA9;
  GL_GEOMETRY_SHADER_ARB = $8DD9;
  GL_GEOMETRY_VERTICES_OUT_ARB = $8DDA;
  GL_GEOMETRY_INPUT_TYPE_ARB = $8DDB;
  GL_GEOMETRY_OUTPUT_TYPE_ARB = $8DDC;
  GL_MAX_GEOMETRY_VARYING_COMPONENTS_ARB = $8DDD;
  GL_MAX_VERTEX_VARYING_COMPONENTS_ARB = $8DDE;
  GL_MAX_GEOMETRY_UNIFORM_COMPONENTS_ARB = $8DDF;
  GL_MAX_GEOMETRY_OUTPUT_VERTICES_ARB = $8DE0;
  GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS_ARB = $8DE1;
  { reuse GL_MAX_VARYING_COMPONENTS }
  { reuse GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER }

  // GL_ARB_half_float_vertex
  GL_HALF_FLOAT = $140B;

  // GL_ARB_map_buffer_range
  GL_MAP_READ_BIT = $0001;
  GL_MAP_WRITE_BIT = $0002;
  GL_MAP_INVALIDATE_RANGE_BIT = $0004;
  GL_MAP_INVALIDATE_BUFFER_BIT = $0008;
  GL_MAP_FLUSH_EXPLICIT_BIT = $0010;
  GL_MAP_UNSYNCHRONIZED_BIT = $0020;

  // GL_ARB_texture_buffer_object
  GL_TEXTURE_BUFFER_ARB = $8C2A;
  GL_MAX_TEXTURE_BUFFER_SIZE_ARB = $8C2B;
  GL_TEXTURE_BINDING_BUFFER_ARB = $8C2C;
  GL_TEXTURE_BUFFER_DATA_STORE_BINDING_ARB = $8C2D;
  GL_TEXTURE_BUFFER_FORMAT_ARB = $8C2E;

  // GL_ARB_texture_compression_rgtc
  GL_COMPRESSED_RED_RGTC1 = $8DBB;
  GL_COMPRESSED_SIGNED_RED_RGTC1 = $8DBC;
  GL_COMPRESSED_RG_RGTC2 = $8DBD;
  GL_COMPRESSED_SIGNED_RG_RGTC2 = $8DBE;

  // GL_ARB_texture_rg
  GL_RG = $8227;
  GL_RG_INTEGER = $8228;
  GL_R8 = $8229;
  GL_R16 = $822A;
  GL_RG8 = $822B;
  GL_RG16 = $822C;
  GL_R16F = $822D;
  GL_R32F = $822E;
  GL_RG16F = $822F;
  GL_RG32F = $8230;
  GL_R8I = $8231;
  GL_R8UI = $8232;
  GL_R16I = $8233;
  GL_R16UI = $8234;
  GL_R32I = $8235;
  GL_R32UI = $8236;
  GL_RG8I = $8237;
  GL_RG8UI = $8238;
  GL_RG16I = $8239;
  GL_RG16UI = $823A;
  GL_RG32I = $823B;
  GL_RG32UI = $823C;

  // GL_ARB_vertex_array_object
  GL_VERTEX_ARRAY_BINDING = $85B5;

  // GL_ATI_draw_buffers
  GL_MAX_DRAW_BUFFERS_ATI = $8824;
  GL_DRAW_BUFFER0_ATI = $8825;
  GL_DRAW_BUFFER1_ATI = $8826;
  GL_DRAW_BUFFER2_ATI = $8827;
  GL_DRAW_BUFFER3_ATI = $8828;
  GL_DRAW_BUFFER4_ATI = $8829;
  GL_DRAW_BUFFER5_ATI = $882A;
  GL_DRAW_BUFFER6_ATI = $882B;
  GL_DRAW_BUFFER7_ATI = $882C;
  GL_DRAW_BUFFER8_ATI = $882D;
  GL_DRAW_BUFFER9_ATI = $882E;
  GL_DRAW_BUFFER10_ATI = $882F;
  GL_DRAW_BUFFER11_ATI = $8830;
  GL_DRAW_BUFFER12_ATI = $8831;
  GL_DRAW_BUFFER13_ATI = $8832;
  GL_DRAW_BUFFER14_ATI = $8833;
  GL_DRAW_BUFFER15_ATI = $8834;

  // GL_ATI_element_array
  GL_ELEMENT_ARRAY_ATI = $8768;
  GL_ELEMENT_ARRAY_TYPE_ATI = $8769;
  GL_ELEMENT_ARRAY_POINTER_ATI = $876A;

  // GL_ATI_envmap_bumpmap
  GL_BUMP_ROT_MATRIX_ATI = $8775;
  GL_BUMP_ROT_MATRIX_SIZE_ATI = $8776;
  GL_BUMP_NUM_TEX_UNITS_ATI = $8777;
  GL_BUMP_TEX_UNITS_ATI = $8778;
  GL_DUDV_ATI = $8779;
  GL_DU8DV8_ATI = $877A;
  GL_BUMP_ENVMAP_ATI = $877B;
  GL_BUMP_TARGET_ATI = $877C;

  // GL_ATI_fragment_shader
  GL_FRAGMENT_SHADER_ATI = $8920;
  GL_REG_0_ATI = $8921;
  GL_REG_1_ATI = $8922;
  GL_REG_2_ATI = $8923;
  GL_REG_3_ATI = $8924;
  GL_REG_4_ATI = $8925;
  GL_REG_5_ATI = $8926;
  GL_REG_6_ATI = $8927;
  GL_REG_7_ATI = $8928;
  GL_REG_8_ATI = $8929;
  GL_REG_9_ATI = $892A;
  GL_REG_10_ATI = $892B;
  GL_REG_11_ATI = $892C;
  GL_REG_12_ATI = $892D;
  GL_REG_13_ATI = $892E;
  GL_REG_14_ATI = $892F;
  GL_REG_15_ATI = $8930;
  GL_REG_16_ATI = $8931;
  GL_REG_17_ATI = $8932;
  GL_REG_18_ATI = $8933;
  GL_REG_19_ATI = $8934;
  GL_REG_20_ATI = $8935;
  GL_REG_21_ATI = $8936;
  GL_REG_22_ATI = $8937;
  GL_REG_23_ATI = $8938;
  GL_REG_24_ATI = $8939;
  GL_REG_25_ATI = $893A;
  GL_REG_26_ATI = $893B;
  GL_REG_27_ATI = $893C;
  GL_REG_28_ATI = $893D;
  GL_REG_29_ATI = $893E;
  GL_REG_30_ATI = $893F;
  GL_REG_31_ATI = $8940;
  GL_CON_0_ATI = $8941;
  GL_CON_1_ATI = $8942;
  GL_CON_2_ATI = $8943;
  GL_CON_3_ATI = $8944;
  GL_CON_4_ATI = $8945;
  GL_CON_5_ATI = $8946;
  GL_CON_6_ATI = $8947;
  GL_CON_7_ATI = $8948;
  GL_CON_8_ATI = $8949;
  GL_CON_9_ATI = $894A;
  GL_CON_10_ATI = $894B;
  GL_CON_11_ATI = $894C;
  GL_CON_12_ATI = $894D;
  GL_CON_13_ATI = $894E;
  GL_CON_14_ATI = $894F;
  GL_CON_15_ATI = $8950;
  GL_CON_16_ATI = $8951;
  GL_CON_17_ATI = $8952;
  GL_CON_18_ATI = $8953;
  GL_CON_19_ATI = $8954;
  GL_CON_20_ATI = $8955;
  GL_CON_21_ATI = $8956;
  GL_CON_22_ATI = $8957;
  GL_CON_23_ATI = $8958;
  GL_CON_24_ATI = $8959;
  GL_CON_25_ATI = $895A;
  GL_CON_26_ATI = $895B;
  GL_CON_27_ATI = $895C;
  GL_CON_28_ATI = $895D;
  GL_CON_29_ATI = $895E;
  GL_CON_30_ATI = $895F;
  GL_CON_31_ATI = $8960;
  GL_MOV_ATI = $8961;
  GL_ADD_ATI = $8963;
  GL_MUL_ATI = $8964;
  GL_SUB_ATI = $8965;
  GL_DOT3_ATI = $8966;
  GL_DOT4_ATI = $8967;
  GL_MAD_ATI = $8968;
  GL_LERP_ATI = $8969;
  GL_CND_ATI = $896A;
  GL_CND0_ATI = $896B;
  GL_DOT2_ADD_ATI = $896C;
  GL_SECONDARY_INTERPOLATOR_ATI = $896D;
  GL_NUM_FRAGMENT_REGISTERS_ATI = $896E;
  GL_NUM_FRAGMENT_CONSTANTS_ATI = $896F;
  GL_NUM_PASSES_ATI = $8970;
  GL_NUM_INSTRUCTIONS_PER_PASS_ATI = $8971;
  GL_NUM_INSTRUCTIONS_TOTAL_ATI = $8972;
  GL_NUM_INPUT_INTERPOLATOR_COMPONENTS_ATI = $8973;
  GL_NUM_LOOPBACK_COMPONENTS_ATI = $8974;
  GL_COLOR_ALPHA_PAIRING_ATI = $8975;
  GL_SWIZZLE_STR_ATI = $8976;
  GL_SWIZZLE_STQ_ATI = $8977;
  GL_SWIZZLE_STR_DR_ATI = $8978;
  GL_SWIZZLE_STQ_DQ_ATI = $8979;
  GL_SWIZZLE_STRQ_ATI = $897A;
  GL_SWIZZLE_STRQ_DQ_ATI = $897B;
  GL_RED_BIT_ATI = $00000001;
  GL_GREEN_BIT_ATI = $00000002;
  GL_BLUE_BIT_ATI = $00000004;
  GL_2X_BIT_ATI = $00000001;
  GL_4X_BIT_ATI = $00000002;
  GL_8X_BIT_ATI = $00000004;
  GL_HALF_BIT_ATI = $00000008;
  GL_QUARTER_BIT_ATI = $00000010;
  GL_EIGHTH_BIT_ATI = $00000020;
  GL_SATURATE_BIT_ATI = $00000040;
  GL_COMP_BIT_ATI = $00000002;
  GL_NEGATE_BIT_ATI = $00000004;
  GL_BIAS_BIT_ATI = $00000008;

  // GL_ATI_pn_triangles
  GL_PN_TRIANGLES_ATI = $87F0;
  GL_MAX_PN_TRIANGLES_TESSELATION_LEVEL_ATI = $87F1;
  GL_PN_TRIANGLES_POINT_MODE_ATI = $87F2;
  GL_PN_TRIANGLES_NORMAL_MODE_ATI = $87F3;
  GL_PN_TRIANGLES_TESSELATION_LEVEL_ATI = $87F4;
  GL_PN_TRIANGLES_POINT_MODE_LINEAR_ATI = $87F5;
  GL_PN_TRIANGLES_POINT_MODE_CUBIC_ATI = $87F6;
  GL_PN_TRIANGLES_NORMAL_MODE_LINEAR_ATI = $87F7;
  GL_PN_TRIANGLES_NORMAL_MODE_QUADRATIC_ATI = $87F8;

  // GL_ATI_separate_stencil
  GL_STENCIL_BACK_FUNC_ATI = $8800;
  GL_STENCIL_BACK_FAIL_ATI = $8801;
  GL_STENCIL_BACK_PASS_DEPTH_FAIL_ATI = $8802;
  GL_STENCIL_BACK_PASS_DEPTH_PASS_ATI = $8803;

  // GL_ATI_text_fragment_shader
  GL_TEXT_FRAGMENT_SHADER_ATI = $8200;

  // GL_ATI_texture_env_combine3
  GL_MODULATE_ADD_ATI = $8744;
  GL_MODULATE_SIGNED_ADD_ATI = $8745;
  GL_MODULATE_SUBTRACT_ATI = $8746;

  // GL_ATI_texture_float
  GL_RGBA_FLOAT32_ATI = $8814;
  GL_RGB_FLOAT32_ATI = $8815;
  GL_ALPHA_FLOAT32_ATI = $8816;
  GL_INTENSITY_FLOAT32_ATI = $8817;
  GL_LUMINANCE_FLOAT32_ATI = $8818;
  GL_LUMINANCE_ALPHA_FLOAT32_ATI = $8819;
  GL_RGBA_FLOAT16_ATI = $881A;
  GL_RGB_FLOAT16_ATI = $881B;
  GL_ALPHA_FLOAT16_ATI = $881C;
  GL_INTENSITY_FLOAT16_ATI = $881D;
  GL_LUMINANCE_FLOAT16_ATI = $881E;
  GL_LUMINANCE_ALPHA_FLOAT16_ATI = $881F;

  // GL_ATI_texture_mirror_once
  GL_MIRROR_CLAMP_ATI = $8742;
  GL_MIRROR_CLAMP_TO_EDGE_ATI = $8743;

  // GL_ATI_vertex_array_object
  GL_STATIC_ATI = $8760;
  GL_DYNAMIC_ATI = $8761;
  GL_PRESERVE_ATI = $8762;
  GL_DISCARD_ATI = $8763;
  GL_OBJECT_BUFFER_SIZE_ATI = $8764;
  GL_OBJECT_BUFFER_USAGE_ATI = $8765;
  GL_ARRAY_OBJECT_BUFFER_ATI = $8766;
  GL_ARRAY_OBJECT_OFFSET_ATI = $8767;

  // GL_ATI_vertex_streams
  GL_MAX_VERTEX_STREAMS_ATI = $876B;
  GL_VERTEX_STREAM0_ATI = $876C;
  GL_VERTEX_STREAM1_ATI = $876D;
  GL_VERTEX_STREAM2_ATI = $876E;
  GL_VERTEX_STREAM3_ATI = $876F;
  GL_VERTEX_STREAM4_ATI = $8770;
  GL_VERTEX_STREAM5_ATI = $8771;
  GL_VERTEX_STREAM6_ATI = $8772;
  GL_VERTEX_STREAM7_ATI = $8773;
  GL_VERTEX_SOURCE_ATI = $8774;

  // GL_EXT_422_pixels
  GL_422_EXT = $80CC;
  GL_422_REV_EXT = $80CD;
  GL_422_AVERAGE_EXT = $80CE;
  GL_422_REV_AVERAGE_EXT = $80CF;

  // GL_EXT_abgr
  GL_ABGR_EXT = $8000;

  // GL_EXT_bgra
  GL_BGR_EXT = $80E0;
  GL_BGRA_EXT = $80E1;

  // GL_EXT_blend_color
  GL_CONSTANT_COLOR_EXT = $8001;
  GL_ONE_MINUS_CONSTANT_COLOR_EXT = $8002;
  GL_CONSTANT_ALPHA_EXT = $8003;
  GL_ONE_MINUS_CONSTANT_ALPHA_EXT = $8004;
  GL_BLEND_COLOR_EXT = $8005;

  // GL_EXT_blend_func_separate
  GL_BLEND_DST_RGB_EXT = $80C8;
  GL_BLEND_SRC_RGB_EXT = $80C9;
  GL_BLEND_DST_ALPHA_EXT = $80CA;
  GL_BLEND_SRC_ALPHA_EXT = $80CB;

  // GL_EXT_blend_minmax
  GL_FUNC_ADD_EXT = $8006;
  GL_MIN_EXT = $8007;
  GL_MAX_EXT = $8008;
  GL_BLEND_EQUATION_EXT = $8009;

  // GL_EXT_blend_subtract
  GL_FUNC_SUBTRACT_EXT = $800A;
  GL_FUNC_REVERSE_SUBTRACT_EXT = $800B;

  // GL_EXT_clip_volume_hint
  GL_CLIP_VOLUME_CLIPPING_HINT_EXT = $80F0;

  // GL_EXT_cmyka
  GL_CMYK_EXT = $800C;
  GL_CMYKA_EXT = $800D;
  GL_PACK_CMYK_HINT_EXT = $800E;
  GL_UNPACK_CMYK_HINT_EXT = $800F;

  // GL_EXT_compiled_vertex_array
  GL_ARRAY_ELEMENT_LOCK_FIRST_EXT = $81A8;
  GL_ARRAY_ELEMENT_LOCK_COUNT_EXT = $81A9;

  // GL_EXT_convolution
  GL_CONVOLUTION_1D_EXT = $8010;
  GL_CONVOLUTION_2D_EXT = $8011;
  GL_SEPARABLE_2D_EXT = $8012;
  GL_CONVOLUTION_BORDER_MODE_EXT = $8013;
  GL_CONVOLUTION_FILTER_SCALE_EXT = $8014;
  GL_CONVOLUTION_FILTER_BIAS_EXT = $8015;
  GL_REDUCE_EXT = $8016;
  GL_CONVOLUTION_FORMAT_EXT = $8017;
  GL_CONVOLUTION_WIDTH_EXT = $8018;
  GL_CONVOLUTION_HEIGHT_EXT = $8019;
  GL_MAX_CONVOLUTION_WIDTH_EXT = $801A;
  GL_MAX_CONVOLUTION_HEIGHT_EXT = $801B;
  GL_POST_CONVOLUTION_RED_SCALE_EXT = $801C;
  GL_POST_CONVOLUTION_GREEN_SCALE_EXT = $801D;
  GL_POST_CONVOLUTION_BLUE_SCALE_EXT = $801E;
  GL_POST_CONVOLUTION_ALPHA_SCALE_EXT = $801F;
  GL_POST_CONVOLUTION_RED_BIAS_EXT = $8020;
  GL_POST_CONVOLUTION_GREEN_BIAS_EXT = $8021;
  GL_POST_CONVOLUTION_BLUE_BIAS_EXT = $8022;
  GL_POST_CONVOLUTION_ALPHA_BIAS_EXT = $8023;

  // GL_EXT_coordinate_frame
  GL_TANGENT_ARRAY_EXT = $8439;
  GL_BINORMAL_ARRAY_EXT = $843A;
  GL_CURRENT_TANGENT_EXT = $843B;
  GL_CURRENT_BINORMAL_EXT = $843C;
  GL_TANGENT_ARRAY_TYPE_EXT = $843E;
  GL_TANGENT_ARRAY_STRIDE_EXT = $843F;
  GL_BINORMAL_ARRAY_TYPE_EXT = $8440;
  GL_BINORMAL_ARRAY_STRIDE_EXT = $8441;
  GL_TANGENT_ARRAY_POINTER_EXT = $8442;
  GL_BINORMAL_ARRAY_POINTER_EXT = $8443;
  GL_MAP1_TANGENT_EXT = $8444;
  GL_MAP2_TANGENT_EXT = $8445;
  GL_MAP1_BINORMAL_EXT = $8446;
  GL_MAP2_BINORMAL_EXT = $8447;

  // GL_EXT_cull_vertex
  GL_CULL_VERTEX_EXT = $81AA;
  GL_CULL_VERTEX_EYE_POSITION_EXT = $81AB;
  GL_CULL_VERTEX_OBJECT_POSITION_EXT = $81AC;

  // GL_EXT_draw_range_elements
  GL_MAX_ELEMENTS_VERTICES_EXT = $80E8;
  GL_MAX_ELEMENTS_INDICES_EXT = $80E9;

  // GL_EXT_fog_coord
  GL_FOG_COORDINATE_SOURCE_EXT = $8450;
  GL_FOG_COORDINATE_EXT = $8451;
  GL_FRAGMENT_DEPTH_EXT = $8452;
  GL_CURRENT_FOG_COORDINATE_EXT = $8453;
  GL_FOG_COORDINATE_ARRAY_TYPE_EXT = $8454;
  GL_FOG_COORDINATE_ARRAY_STRIDE_EXT = $8455;
  GL_FOG_COORDINATE_ARRAY_POINTER_EXT = $8456;
  GL_FOG_COORDINATE_ARRAY_EXT = $8457;

  // GL_EXT_framebuffer_object
  GL_FRAMEBUFFER_EXT = $8D40;
  GL_RENDERBUFFER_EXT = $8D41;
  GL_STENCIL_INDEX_EXT = $8D45;
  GL_STENCIL_INDEX1_EXT = $8D46;
  GL_STENCIL_INDEX4_EXT = $8D47;
  GL_STENCIL_INDEX8_EXT = $8D48;
  GL_STENCIL_INDEX16_EXT = $8D49;
  GL_RENDERBUFFER_WIDTH_EXT = $8D42;
  GL_RENDERBUFFER_HEIGHT_EXT = $8D43;
  GL_RENDERBUFFER_INTERNAL_FORMAT_EXT = $8D44;
  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE_EXT = $8CD0;
  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME_EXT = $8CD1;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL_EXT = $8CD2;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE_EXT = $8CD3;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_3D_ZOFFSET_EXT = $8CD4;
  GL_COLOR_ATTACHMENT0_EXT = $8CE0;
  GL_COLOR_ATTACHMENT1_EXT = $8CE1;
  GL_COLOR_ATTACHMENT2_EXT = $8CE2;
  GL_COLOR_ATTACHMENT3_EXT = $8CE3;
  GL_COLOR_ATTACHMENT4_EXT = $8CE4;
  GL_COLOR_ATTACHMENT5_EXT = $8CE5;
  GL_COLOR_ATTACHMENT6_EXT = $8CE6;
  GL_COLOR_ATTACHMENT7_EXT = $8CE7;
  GL_COLOR_ATTACHMENT8_EXT = $8CE8;
  GL_COLOR_ATTACHMENT9_EXT = $8CE9;
  GL_COLOR_ATTACHMENT10_EXT = $8CEA;
  GL_COLOR_ATTACHMENT11_EXT = $8CEB;
  GL_COLOR_ATTACHMENT12_EXT = $8CEC;
  GL_COLOR_ATTACHMENT13_EXT = $8CED;
  GL_COLOR_ATTACHMENT14_EXT = $8CEE;
  GL_COLOR_ATTACHMENT15_EXT = $8CEF;
  GL_DEPTH_ATTACHMENT_EXT = $8D00;
  GL_STENCIL_ATTACHMENT_EXT = $8D20;
  GL_FRAMEBUFFER_COMPLETE_EXT = $8CD5;
  GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_EXT = $8CD6;
  GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_EXT = $8CD7;
  GL_FRAMEBUFFER_INCOMPLETE_DUPLICATE_ATTACHMENT_EXT = $8CD8;
  GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT = $8CD9;
  GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT = $8CDA;
  GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER_EXT = $8CDB;
  GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER_EXT = $8CDC;
  GL_FRAMEBUFFER_UNSUPPORTED_EXT = $8CDD;
  GL_FRAMEBUFFER_STATUS_ERROR_EXT = $8CDE;
  GL_FRAMEBUFFER_BINDING_EXT = $8CA6;
  GL_RENDERBUFFER_BINDING_EXT = $8CA7;
  GL_MAX_COLOR_ATTACHMENTS_EXT = $8CDF;
  GL_MAX_RENDERBUFFER_SIZE_EXT = $84E8;
  GL_INVALID_FRAMEBUFFER_OPERATION_EXT = $0506;

  // GL_EXT_histogram
  GL_HISTOGRAM_EXT = $8024;
  GL_PROXY_HISTOGRAM_EXT = $8025;
  GL_HISTOGRAM_WIDTH_EXT = $8026;
  GL_HISTOGRAM_FORMAT_EXT = $8027;
  GL_HISTOGRAM_RED_SIZE_EXT = $8028;
  GL_HISTOGRAM_GREEN_SIZE_EXT = $8029;
  GL_HISTOGRAM_BLUE_SIZE_EXT = $802A;
  GL_HISTOGRAM_ALPHA_SIZE_EXT = $802B;
  GL_HISTOGRAM_LUMINANCE_SIZE_EXT = $802C;
  GL_HISTOGRAM_SINK_EXT = $802D;
  GL_MINMAX_EXT = $802E;
  GL_MINMAX_FORMAT_EXT = $802F;
  GL_MINMAX_SINK_EXT = $8030;
  GL_TABLE_TOO_LARGE_EXT = $8031;

  // GL_EXT_index_array_formats
  GL_IUI_V2F_EXT = $81AD;
  GL_IUI_V3F_EXT = $81AE;
  GL_IUI_N3F_V2F_EXT = $81AF;
  GL_IUI_N3F_V3F_EXT = $81B0;
  GL_T2F_IUI_V2F_EXT = $81B1;
  GL_T2F_IUI_V3F_EXT = $81B2;
  GL_T2F_IUI_N3F_V2F_EXT = $81B3;
  GL_T2F_IUI_N3F_V3F_EXT = $81B4;

  // GL_EXT_index_func
  GL_INDEX_TEST_EXT = $81B5;
  GL_INDEX_TEST_FUNC_EXT = $81B6;
  GL_INDEX_TEST_REF_EXT = $81B7;

  // GL_EXT_index_material
  GL_INDEX_MATERIAL_EXT = $81B8;
  GL_INDEX_MATERIAL_PARAMETER_EXT = $81B9;
  GL_INDEX_MATERIAL_FACE_EXT = $81BA;

  // GL_EXT_light_texture
  GL_FRAGMENT_MATERIAL_EXT = $8349;
  GL_FRAGMENT_NORMAL_EXT = $834A;
  GL_FRAGMENT_COLOR_EXT = $834C;
  GL_ATTENUATION_EXT = $834D;
  GL_SHADOW_ATTENUATION_EXT = $834E;
  GL_TEXTURE_APPLICATION_MODE_EXT = $834F;
  GL_TEXTURE_LIGHT_EXT = $8350;
  GL_TEXTURE_MATERIAL_FACE_EXT = $8351;
  GL_TEXTURE_MATERIAL_PARAMETER_EXT = $8352;

  // GL_EXT_multisample
  GL_MULTISAMPLE_EXT = $809D;
  GL_SAMPLE_ALPHA_TO_MASK_EXT = $809E;
  GL_SAMPLE_ALPHA_TO_ONE_EXT = $809F;
  GL_SAMPLE_MASK_EXT = $80A0;
  GL_1PASS_EXT = $80A1;
  GL_2PASS_0_EXT = $80A2;
  GL_2PASS_1_EXT = $80A3;
  GL_4PASS_0_EXT = $80A4;
  GL_4PASS_1_EXT = $80A5;
  GL_4PASS_2_EXT = $80A6;
  GL_4PASS_3_EXT = $80A7;
  GL_SAMPLE_BUFFERS_EXT = $80A8;
  GL_SAMPLES_EXT = $80A9;
  GL_SAMPLE_MASK_VALUE_EXT = $80AA;
  GL_SAMPLE_MASK_INVERT_EXT = $80AB;
  GL_SAMPLE_PATTERN_EXT = $80AC;
  GL_MULTISAMPLE_BIT_EXT = $20000000;

  // GL_EXT_packed_pixels
  GL_UNSIGNED_BYTE_3_3_2_EXT = $8032;
  GL_UNSIGNED_SHORT_4_4_4_4_EXT = $8033;
  GL_UNSIGNED_SHORT_5_5_5_1_EXT = $8034;
  GL_UNSIGNED_INT_8_8_8_8_EXT = $8035;
  GL_UNSIGNED_INT_10_10_10_2_EXT = $8036;

  // GL_EXT_paletted_texture
  GL_COLOR_INDEX1_EXT = $80E2;
  GL_COLOR_INDEX2_EXT = $80E3;
  GL_COLOR_INDEX4_EXT = $80E4;
  GL_COLOR_INDEX8_EXT = $80E5;
  GL_COLOR_INDEX12_EXT = $80E6;
  GL_COLOR_INDEX16_EXT = $80E7;
  GL_TEXTURE_INDEX_SIZE_EXT = $80ED;

  // GL_EXT_pixel_transform
  GL_PIXEL_TRANSFORM_2D_EXT = $8330;
  GL_PIXEL_MAG_FILTER_EXT = $8331;
  GL_PIXEL_MIN_FILTER_EXT = $8332;
  GL_PIXEL_CUBIC_WEIGHT_EXT = $8333;
  GL_CUBIC_EXT = $8334;
  GL_AVERAGE_EXT = $8335;
  GL_PIXEL_TRANSFORM_2D_STACK_DEPTH_EXT = $8336;
  GL_MAX_PIXEL_TRANSFORM_2D_STACK_DEPTH_EXT = $8337;
  GL_PIXEL_TRANSFORM_2D_MATRIX_EXT = $8338;

  // GL_EXT_point_parameters
  GL_POINT_SIZE_MIN_EXT = $8126;
  GL_POINT_SIZE_MAX_EXT = $8127;
  GL_POINT_FADE_THRESHOLD_SIZE_EXT = $8128;
  GL_DISTANCE_ATTENUATION_EXT = $8129;

  // GL_EXT_polygon_offset
  GL_POLYGON_OFFSET_EXT = $8037;
  GL_POLYGON_OFFSET_FACTOR_EXT = $8038;
  GL_POLYGON_OFFSET_BIAS_EXT = $8039;

  // GL_EXT_rescale_normal
  GL_RESCALE_NORMAL_EXT = $803A;

  // GL_EXT_secondary_color
  GL_COLOR_SUM_EXT = $8458;
  GL_CURRENT_SECONDARY_COLOR_EXT = $8459;
  GL_SECONDARY_COLOR_ARRAY_SIZE_EXT = $845A;
  GL_SECONDARY_COLOR_ARRAY_TYPE_EXT = $845B;
  GL_SECONDARY_COLOR_ARRAY_STRIDE_EXT = $845C;
  GL_SECONDARY_COLOR_ARRAY_POINTER_EXT = $845D;
  GL_SECONDARY_COLOR_ARRAY_EXT = $845E;

  // GL_EXT_separate_specular_color
  GL_LIGHT_MODEL_COLOR_CONTROL_EXT = $81F8;
  GL_SINGLE_COLOR_EXT = $81F9;
  GL_SEPARATE_SPECULAR_COLOR_EXT = $81FA;

  // GL_EXT_shared_texture_palette
  GL_SHARED_TEXTURE_PALETTE_EXT = $81FB;

  // GL_EXT_stencil_two_side
  GL_STENCIL_TEST_TWO_SIDE_EXT = $8910;
  GL_ACTIVE_STENCIL_FACE_EXT = $8911;

  // GL_EXT_stencil_wrap
  GL_INCR_WRAP_EXT = $8507;
  GL_DECR_WRAP_EXT = $8508;

  // GL_EXT_texture
  GL_ALPHA4_EXT = $803B;
  GL_ALPHA8_EXT = $803C;
  GL_ALPHA12_EXT = $803D;
  GL_ALPHA16_EXT = $803E;
  GL_LUMINANCE4_EXT = $803F;
  GL_LUMINANCE8_EXT = $8040;
  GL_LUMINANCE12_EXT = $8041;
  GL_LUMINANCE16_EXT = $8042;
  GL_LUMINANCE4_ALPHA4_EXT = $8043;
  GL_LUMINANCE6_ALPHA2_EXT = $8044;
  GL_LUMINANCE8_ALPHA8_EXT = $8045;
  GL_LUMINANCE12_ALPHA4_EXT = $8046;
  GL_LUMINANCE12_ALPHA12_EXT = $8047;
  GL_LUMINANCE16_ALPHA16_EXT = $8048;
  GL_INTENSITY_EXT = $8049;
  GL_INTENSITY4_EXT = $804A;
  GL_INTENSITY8_EXT = $804B;
  GL_INTENSITY12_EXT = $804C;
  GL_INTENSITY16_EXT = $804D;
  GL_RGB2_EXT = $804E;
  GL_RGB4_EXT = $804F;
  GL_RGB5_EXT = $8050;
  GL_RGB8_EXT = $8051;
  GL_RGB10_EXT = $8052;
  GL_RGB12_EXT = $8053;
  GL_RGB16_EXT = $8054;
  GL_RGBA2_EXT = $8055;
  GL_RGBA4_EXT = $8056;
  GL_RGB5_A1_EXT = $8057;
  GL_RGBA8_EXT = $8058;
  GL_RGB10_A2_EXT = $8059;
  GL_RGBA12_EXT = $805A;
  GL_RGBA16_EXT = $805B;
  GL_TEXTURE_RED_SIZE_EXT = $805C;
  GL_TEXTURE_GREEN_SIZE_EXT = $805D;
  GL_TEXTURE_BLUE_SIZE_EXT = $805E;
  GL_TEXTURE_ALPHA_SIZE_EXT = $805F;
  GL_TEXTURE_LUMINANCE_SIZE_EXT = $8060;
  GL_TEXTURE_INTENSITY_SIZE_EXT = $8061;
  GL_REPLACE_EXT = $8062;
  GL_PROXY_TEXTURE_1D_EXT = $8063;
  GL_PROXY_TEXTURE_2D_EXT = $8064;
  GL_TEXTURE_TOO_LARGE_EXT = $8065;

  // GL_EXT_texture3D
  GL_PACK_SKIP_IMAGES_EXT = $806B;
  GL_PACK_IMAGE_HEIGHT_EXT = $806C;
  GL_UNPACK_SKIP_IMAGES_EXT = $806D;
  GL_UNPACK_IMAGE_HEIGHT_EXT = $806E;
  GL_TEXTURE_3D_EXT = $806F;
  GL_PROXY_TEXTURE_3D_EXT = $8070;
  GL_TEXTURE_DEPTH_EXT = $8071;
  GL_TEXTURE_WRAP_R_EXT = $8072;
  GL_MAX_3D_TEXTURE_SIZE_EXT = $8073;

  // GL_EXT_texture_compression_s3tc
  GL_COMPRESSED_RGB_S3TC_DXT1_EXT = $83F0;
  GL_COMPRESSED_RGBA_S3TC_DXT1_EXT = $83F1;
  GL_COMPRESSED_RGBA_S3TC_DXT3_EXT = $83F2;
  GL_COMPRESSED_RGBA_S3TC_DXT5_EXT = $83F3;

  // GL_EXT_texture_cube_map
  GL_NORMAL_MAP_EXT = $8511;
  GL_REFLECTION_MAP_EXT = $8512;
  GL_TEXTURE_CUBE_MAP_EXT = $8513;
  GL_TEXTURE_BINDING_CUBE_MAP_EXT = $8514;
  GL_TEXTURE_CUBE_MAP_POSITIVE_X_EXT = $8515;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_X_EXT = $8516;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Y_EXT = $8517;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_EXT = $8518;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Z_EXT = $8519;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_EXT = $851A;
  GL_PROXY_TEXTURE_CUBE_MAP_EXT = $851B;
  GL_MAX_CUBE_MAP_TEXTURE_SIZE_EXT = $851C;

  // GL_EXT_texture_edge_clamp
  GL_CLAMP_TO_EDGE_EXT = $812F;

  // GL_EXT_texture_env_combine
  GL_COMBINE_EXT = $8570;
  GL_COMBINE_RGB_EXT = $8571;
  GL_COMBINE_ALPHA_EXT = $8572;
  GL_RGB_SCALE_EXT = $8573;
  GL_ADD_SIGNED_EXT = $8574;
  GL_INTERPOLATE_EXT = $8575;
  GL_CONSTANT_EXT = $8576;
  GL_PRIMARY_COLOR_EXT = $8577;
  GL_PREVIOUS_EXT = $8578;
  GL_SOURCE0_RGB_EXT = $8580;
  GL_SOURCE1_RGB_EXT = $8581;
  GL_SOURCE2_RGB_EXT = $8582;
  GL_SOURCE0_ALPHA_EXT = $8588;
  GL_SOURCE1_ALPHA_EXT = $8589;
  GL_SOURCE2_ALPHA_EXT = $858A;
  GL_OPERAND0_RGB_EXT = $8590;
  GL_OPERAND1_RGB_EXT = $8591;
  GL_OPERAND2_RGB_EXT = $8592;
  GL_OPERAND0_ALPHA_EXT = $8598;
  GL_OPERAND1_ALPHA_EXT = $8599;
  GL_OPERAND2_ALPHA_EXT = $859A;

  // GL_EXT_texture_env_dot3
  GL_DOT3_RGB_EXT = $8740;
  GL_DOT3_RGBA_EXT = $8741;

  // GL_EXT_texture_filter_anisotropic
  GL_TEXTURE_MAX_ANISOTROPY_EXT = $84FE;
  GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT = $84FF;

  // GL_EXT_texture_lod_bias
  GL_MAX_TEXTURE_LOD_BIAS_EXT = $84FD;
  GL_TEXTURE_FILTER_CONTROL_EXT = $8500;
  GL_TEXTURE_LOD_BIAS_EXT = $8501;

  // GL_EXT_texture_object
  GL_TEXTURE_PRIORITY_EXT = $8066;
  GL_TEXTURE_RESIDENT_EXT = $8067;
  GL_TEXTURE_1D_BINDING_EXT = $8068;
  GL_TEXTURE_2D_BINDING_EXT = $8069;
  GL_TEXTURE_3D_BINDING_EXT = $806A;

  // GL_EXT_texture_perturb_normal
  GL_PERTURB_EXT = $85AE;
  GL_TEXTURE_NORMAL_EXT = $85AF;

  // GL_EXT_texture_rectangle
  GL_TEXTURE_RECTANGLE_EXT = $84F5;
  GL_TEXTURE_BINDING_RECTANGLE_EXT = $84F6;
  GL_PROXY_TEXTURE_RECTANGLE_EXT = $84F7;
  GL_MAX_RECTANGLE_TEXTURE_SIZE_EXT = $84F8;

  // GL_EXT_vertex_array
  GL_VERTEX_ARRAY_EXT = $8074;
  GL_NORMAL_ARRAY_EXT = $8075;
  GL_COLOR_ARRAY_EXT = $8076;
  GL_INDEX_ARRAY_EXT = $8077;
  GL_TEXTURE_COORD_ARRAY_EXT = $8078;
  GL_EDGE_FLAG_ARRAY_EXT = $8079;
  GL_VERTEX_ARRAY_SIZE_EXT = $807A;
  GL_VERTEX_ARRAY_TYPE_EXT = $807B;
  GL_VERTEX_ARRAY_STRIDE_EXT = $807C;
  GL_VERTEX_ARRAY_COUNT_EXT = $807D;
  GL_NORMAL_ARRAY_TYPE_EXT = $807E;
  GL_NORMAL_ARRAY_STRIDE_EXT = $807F;
  GL_NORMAL_ARRAY_COUNT_EXT = $8080;
  GL_COLOR_ARRAY_SIZE_EXT = $8081;
  GL_COLOR_ARRAY_TYPE_EXT = $8082;
  GL_COLOR_ARRAY_STRIDE_EXT = $8083;
  GL_COLOR_ARRAY_COUNT_EXT = $8084;
  GL_INDEX_ARRAY_TYPE_EXT = $8085;
  GL_INDEX_ARRAY_STRIDE_EXT = $8086;
  GL_INDEX_ARRAY_COUNT_EXT = $8087;
  GL_TEXTURE_COORD_ARRAY_SIZE_EXT = $8088;
  GL_TEXTURE_COORD_ARRAY_TYPE_EXT = $8089;
  GL_TEXTURE_COORD_ARRAY_STRIDE_EXT = $808A;
  GL_TEXTURE_COORD_ARRAY_COUNT_EXT = $808B;
  GL_EDGE_FLAG_ARRAY_STRIDE_EXT = $808C;
  GL_EDGE_FLAG_ARRAY_COUNT_EXT = $808D;
  GL_VERTEX_ARRAY_POINTER_EXT = $808E;
  GL_NORMAL_ARRAY_POINTER_EXT = $808F;
  GL_COLOR_ARRAY_POINTER_EXT = $8090;
  GL_INDEX_ARRAY_POINTER_EXT = $8091;
  GL_TEXTURE_COORD_ARRAY_POINTER_EXT = $8092;
  GL_EDGE_FLAG_ARRAY_POINTER_EXT = $8093;

  // GL_EXT_vertex_shader
  GL_VERTEX_SHADER_EXT = $8780;
  GL_VERTEX_SHADER_BINDING_EXT = $8781;
  GL_OP_INDEX_EXT = $8782;
  GL_OP_NEGATE_EXT = $8783;
  GL_OP_DOT3_EXT = $8784;
  GL_OP_DOT4_EXT = $8785;
  GL_OP_MUL_EXT = $8786;
  GL_OP_ADD_EXT = $8787;
  GL_OP_MADD_EXT = $8788;
  GL_OP_FRAC_EXT = $8789;
  GL_OP_MAX_EXT = $878A;
  GL_OP_MIN_EXT = $878B;
  GL_OP_SET_GE_EXT = $878C;
  GL_OP_SET_LT_EXT = $878D;
  GL_OP_CLAMP_EXT = $878E;
  GL_OP_FLOOR_EXT = $878F;
  GL_OP_ROUND_EXT = $8790;
  GL_OP_EXP_BASE_2_EXT = $8791;
  GL_OP_LOG_BASE_2_EXT = $8792;
  GL_OP_POWER_EXT = $8793;
  GL_OP_RECIP_EXT = $8794;
  GL_OP_RECIP_SQRT_EXT = $8795;
  GL_OP_SUB_EXT = $8796;
  GL_OP_CROSS_PRODUCT_EXT = $8797;
  GL_OP_MULTIPLY_MATRIX_EXT = $8798;
  GL_OP_MOV_EXT = $8799;
  GL_OUTPUT_VERTEX_EXT = $879A;
  GL_OUTPUT_COLOR0_EXT = $879B;
  GL_OUTPUT_COLOR1_EXT = $879C;
  GL_OUTPUT_TEXTURE_COORD0_EXT = $879D;
  GL_OUTPUT_TEXTURE_COORD1_EXT = $879E;
  GL_OUTPUT_TEXTURE_COORD2_EXT = $879F;
  GL_OUTPUT_TEXTURE_COORD3_EXT = $87A0;
  GL_OUTPUT_TEXTURE_COORD4_EXT = $87A1;
  GL_OUTPUT_TEXTURE_COORD5_EXT = $87A2;
  GL_OUTPUT_TEXTURE_COORD6_EXT = $87A3;
  GL_OUTPUT_TEXTURE_COORD7_EXT = $87A4;
  GL_OUTPUT_TEXTURE_COORD8_EXT = $87A5;
  GL_OUTPUT_TEXTURE_COORD9_EXT = $87A6;
  GL_OUTPUT_TEXTURE_COORD10_EXT = $87A7;
  GL_OUTPUT_TEXTURE_COORD11_EXT = $87A8;
  GL_OUTPUT_TEXTURE_COORD12_EXT = $87A9;
  GL_OUTPUT_TEXTURE_COORD13_EXT = $87AA;
  GL_OUTPUT_TEXTURE_COORD14_EXT = $87AB;
  GL_OUTPUT_TEXTURE_COORD15_EXT = $87AC;
  GL_OUTPUT_TEXTURE_COORD16_EXT = $87AD;
  GL_OUTPUT_TEXTURE_COORD17_EXT = $87AE;
  GL_OUTPUT_TEXTURE_COORD18_EXT = $87AF;
  GL_OUTPUT_TEXTURE_COORD19_EXT = $87B0;
  GL_OUTPUT_TEXTURE_COORD20_EXT = $87B1;
  GL_OUTPUT_TEXTURE_COORD21_EXT = $87B2;
  GL_OUTPUT_TEXTURE_COORD22_EXT = $87B3;
  GL_OUTPUT_TEXTURE_COORD23_EXT = $87B4;
  GL_OUTPUT_TEXTURE_COORD24_EXT = $87B5;
  GL_OUTPUT_TEXTURE_COORD25_EXT = $87B6;
  GL_OUTPUT_TEXTURE_COORD26_EXT = $87B7;
  GL_OUTPUT_TEXTURE_COORD27_EXT = $87B8;
  GL_OUTPUT_TEXTURE_COORD28_EXT = $87B9;
  GL_OUTPUT_TEXTURE_COORD29_EXT = $87BA;
  GL_OUTPUT_TEXTURE_COORD30_EXT = $87BB;
  GL_OUTPUT_TEXTURE_COORD31_EXT = $87BC;
  GL_OUTPUT_FOG_EXT = $87BD;
  GL_SCALAR_EXT = $87BE;
  GL_VECTOR_EXT = $87BF;
  GL_MATRIX_EXT = $87C0;
  GL_VARIANT_EXT = $87C1;
  GL_INVARIANT_EXT = $87C2;
  GL_LOCAL_CONSTANT_EXT = $87C3;
  GL_LOCAL_EXT = $87C4;
  GL_MAX_VERTEX_SHADER_INSTRUCTIONS_EXT = $87C5;
  GL_MAX_VERTEX_SHADER_VARIANTS_EXT = $87C6;
  GL_MAX_VERTEX_SHADER_INVARIANTS_EXT = $87C7;
  GL_MAX_VERTEX_SHADER_LOCAL_CONSTANTS_EXT = $87C8;
  GL_MAX_VERTEX_SHADER_LOCALS_EXT = $87C9;
  GL_MAX_OPTIMIZED_VERTEX_SHADER_INSTRUCTIONS_EXT = $87CA;
  GL_MAX_OPTIMIZED_VERTEX_SHADER_VARIANTS_EXT = $87CB;
  GL_MAX_OPTIMIZED_VERTEX_SHADER_LOCAL_CONSTANTS_EXT = $87CC;
  GL_MAX_OPTIMIZED_VERTEX_SHADER_INVARIANTS_EXT = $87CD;
  GL_MAX_OPTIMIZED_VERTEX_SHADER_LOCALS_EXT = $87CE;
  GL_VERTEX_SHADER_INSTRUCTIONS_EXT = $87CF;
  GL_VERTEX_SHADER_VARIANTS_EXT = $87D0;
  GL_VERTEX_SHADER_INVARIANTS_EXT = $87D1;
  GL_VERTEX_SHADER_LOCAL_CONSTANTS_EXT = $87D2;
  GL_VERTEX_SHADER_LOCALS_EXT = $87D3;
  GL_VERTEX_SHADER_OPTIMIZED_EXT = $87D4;
  GL_X_EXT = $87D5;
  GL_Y_EXT = $87D6;
  GL_Z_EXT = $87D7;
  GL_W_EXT = $87D8;
  GL_NEGATIVE_X_EXT = $87D9;
  GL_NEGATIVE_Y_EXT = $87DA;
  GL_NEGATIVE_Z_EXT = $87DB;
  GL_NEGATIVE_W_EXT = $87DC;
  GL_ZERO_EXT = $87DD;
  GL_ONE_EXT = $87DE;
  GL_NEGATIVE_ONE_EXT = $87DF;
  GL_NORMALIZED_RANGE_EXT = $87E0;
  GL_FULL_RANGE_EXT = $87E1;
  GL_CURRENT_VERTEX_EXT = $87E2;
  GL_MVP_MATRIX_EXT = $87E3;
  GL_VARIANT_VALUE_EXT = $87E4;
  GL_VARIANT_DATATYPE_EXT = $87E5;
  GL_VARIANT_ARRAY_STRIDE_EXT = $87E6;
  GL_VARIANT_ARRAY_TYPE_EXT = $87E7;
  GL_VARIANT_ARRAY_EXT = $87E8;
  GL_VARIANT_ARRAY_POINTER_EXT = $87E9;
  GL_INVARIANT_VALUE_EXT = $87EA;
  GL_INVARIANT_DATATYPE_EXT = $87EB;
  GL_LOCAL_CONSTANT_VALUE_EXT = $87EC;
  GL_LOCAL_CONSTANT_DATATYPE_EXT = $87ED;

  // GL_EXT_vertex_weighting
  GL_MODELVIEW0_STACK_DEPTH_EXT = GL_MODELVIEW_STACK_DEPTH;
  GL_MODELVIEW1_STACK_DEPTH_EXT = $8502;
  GL_MODELVIEW0_MATRIX_EXT = GL_MODELVIEW_MATRIX;
  GL_MODELVIEW1_MATRIX_EXT = $8506;
  GL_VERTEX_WEIGHTING_EXT = $8509;
  GL_MODELVIEW0_EXT = GL_MODELVIEW;
  GL_MODELVIEW1_EXT = $850A;
  GL_CURRENT_VERTEX_WEIGHT_EXT = $850B;
  GL_VERTEX_WEIGHT_ARRAY_EXT = $850C;
  GL_VERTEX_WEIGHT_ARRAY_SIZE_EXT = $850D;
  GL_VERTEX_WEIGHT_ARRAY_TYPE_EXT = $850E;
  GL_VERTEX_WEIGHT_ARRAY_STRIDE_EXT = $850F;
  GL_VERTEX_WEIGHT_ARRAY_POINTER_EXT = $8510;

  // GL_EXT_depth_bounds_test
  GL_DEPTH_BOUNDS_TEST_EXT = $8890;
  GL_DEPTH_BOUNDS_EXT = $8891;

  // GL_EXT_texture_mirror_clamp
  GL_MIRROR_CLAMP_EXT = $8742; // (same value as MIRROR_CLAMP_ATI)
  GL_MIRROR_CLAMP_TO_EDGE_EXT = $8743; // (same value as MIRROR_CLAMP_TO_EDGE_ATI)
  GL_MIRROR_CLAMP_TO_BORDER_EXT = $8912;

  // GL_EXT_blend_equation_separate
  GL_BLEND_EQUATION_RGB_EXT = $8009; // (same as BLEND_EQUATION)
  GL_BLEND_EQUATION_ALPHA_EXT = $883D;

  // GL_EXT_pixel_buffer_object
  GL_PIXEL_PACK_BUFFER_EXT = $88EB;
  GL_PIXEL_UNPACK_BUFFER_EXT = $88EC;
  GL_PIXEL_PACK_BUFFER_BINDING_EXT = $88ED;
  GL_PIXEL_UNPACK_BUFFER_BINDING_EXT = $88EF;

  // GL_EXT_stencil_clear_tag
  GL_STENCIL_TAG_BITS_EXT = $88F2;
  GL_STENCIL_CLEAR_TAG_VALUE_EXT = $88F3;

  // GL_EXT_packed_depth_stencil
  GL_DEPTH_STENCIL_EXT = $84F9;
  GL_UNSIGNED_INT_24_8_EXT = $84FA;
  GL_DEPTH24_STENCIL8_EXT = $88F0;
  GL_TEXTURE_STENCIL_SIZE_EXT = $88F1;

  // GL_EXT_texture_sRGB
  GL_SRGB_EXT = $8C40;
  GL_SRGB8_EXT = $8C41;
  GL_SRGB_ALPHA_EXT = $8C42;
  GL_SRGB8_ALPHA8_EXT = $8C43;
  GL_SLUMINANCE_ALPHA_EXT = $8C44;
  GL_SLUMINANCE8_ALPHA8_EXT = $8C45;
  GL_SLUMINANCE_EXT = $8C46;
  GL_SLUMINANCE8_EXT = $8C47;
  GL_COMPRESSED_SRGB_EXT = $8C48;
  GL_COMPRESSED_SRGB_ALPHA_EXT = $8C49;
  GL_COMPRESSED_SLUMINANCE_EXT = $8C4A;
  GL_COMPRESSED_SLUMINANCE_ALPHA_EXT = $8C4B;
  GL_COMPRESSED_SRGB_S3TC_DXT1_EXT = $8C4C;
  GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT = $8C4D;
  GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT = $8C4E;
  GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT = $8C4F;

  // GL_EXT_framebuffer_blit
  GL_READ_FRAMEBUFFER_EXT = $8CA8;
  GL_DRAW_FRAMEBUFFER_EXT = $8CA9;
  GL_READ_FRAMEBUFFER_BINDING_EXT = GL_FRAMEBUFFER_BINDING_EXT;
  GL_DRAW_FRAMEBUFFER_BINDING_EXT = $8CAA;

  // GL_EXT_framebuffer_multisample
  GL_RENDERBUFFER_SAMPLES_EXT = $8CAB;
  GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE_EXT = $8D56;
  GL_MAX_SAMPLES_EXT = $8D57;

  // GL_EXT_timer_query
  GL_TIME_ELAPSED_EXT = $88BF;

  // GL_EXT_bindable_uniform
  GL_MAX_VERTEX_BINDABLE_UNIFORMS_EXT = $8DE2;
  GL_MAX_FRAGMENT_BINDABLE_UNIFORMS_EXT = $8DE3;
  GL_MAX_GEOMETRY_BINDABLE_UNIFORMS_EXT = $8DE4;
  GL_MAX_BINDABLE_UNIFORM_SIZE_EXT = $8DED;
  GL_UNIFORM_BUFFER_BINDING_EXT = $8DEF;

  // GL_EXT_framebuffer_sRGB
  GLX_FRAMEBUFFER_SRGB_CAPABLE_EXT = $20B2;
  WGL_FRAMEBUFFER_SRGB_CAPABLE_EXT = $20A9;
  GL_FRAMEBUFFER_SRGB_EXT = $8DB9;
  GL_FRAMEBUFFER_SRGB_CAPABLE_EXT = $8DBA;

  // GL_EXT_geometry_shader4
  GL_GEOMETRY_SHADER_EXT = $8DD9;
  GL_GEOMETRY_VERTICES_OUT_EXT = $8DDA;
  GL_GEOMETRY_INPUT_TYPE_EXT = $8DDB;
  GL_GEOMETRY_OUTPUT_TYPE_EXT = $8DDC;
  GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS_EXT = $8C29;
  GL_MAX_GEOMETRY_VARYING_COMPONENTS_EXT = $8DDD;
  GL_MAX_VERTEX_VARYING_COMPONENTS_EXT = $8DDE;
  GL_MAX_VARYING_COMPONENTS_EXT = $8B4B;
  GL_MAX_GEOMETRY_UNIFORM_COMPONENTS_EXT = $8DDF;
  GL_MAX_GEOMETRY_OUTPUT_VERTICES_EXT = $8DE0;
  GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS_EXT = $8DE1;
  GL_LINES_ADJACENCY_EXT = $A;
  GL_LINE_STRIP_ADJACENCY_EXT = $B;
  GL_TRIANGLES_ADJACENCY_EXT = $C;
  GL_TRIANGLE_STRIP_ADJACENCY_EXT = $D;
  GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS_EXT = $8DA8;
  GL_FRAMEBUFFER_INCOMPLETE_LAYER_COUNT_EXT = $8DA9;
  GL_FRAMEBUFFER_ATTACHMENT_LAYERED_EXT = $8DA7;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER_EXT = $8CD4;
  GL_PROGRAM_POINT_SIZE_EXT = $8642;

  // GL_EXT_gpu_shader4
  GL_VERTEX_ATTRIB_ARRAY_INTEGER_EXT = $88FD;
  GL_SAMPLER_1D_ARRAY_EXT = $8DC0;
  GL_SAMPLER_2D_ARRAY_EXT = $8DC1;
  GL_SAMPLER_BUFFER_EXT = $8DC2;
  GL_SAMPLER_1D_ARRAY_SHADOW_EXT = $8DC3;
  GL_SAMPLER_2D_ARRAY_SHADOW_EXT = $8DC4;
  GL_SAMPLER_CUBE_SHADOW_EXT = $8DC5;
  GL_UNSIGNED_INT_VEC2_EXT = $8DC6;
  GL_UNSIGNED_INT_VEC3_EXT = $8DC7;
  GL_UNSIGNED_INT_VEC4_EXT = $8DC8;
  GL_INT_SAMPLER_1D_EXT = $8DC9;
  GL_INT_SAMPLER_2D_EXT = $8DCA;
  GL_INT_SAMPLER_3D_EXT = $8DCB;
  GL_INT_SAMPLER_CUBE_EXT = $8DCC;
  GL_INT_SAMPLER_2D_RECT_EXT = $8DCD;
  GL_INT_SAMPLER_1D_ARRAY_EXT = $8DCE;
  GL_INT_SAMPLER_2D_ARRAY_EXT = $8DCF;
  GL_INT_SAMPLER_BUFFER_EXT = $8DD0;
  GL_UNSIGNED_INT_SAMPLER_1D_EXT = $8DD1;
  GL_UNSIGNED_INT_SAMPLER_2D_EXT = $8DD2;
  GL_UNSIGNED_INT_SAMPLER_3D_EXT = $8DD3;
  GL_UNSIGNED_INT_SAMPLER_CUBE_EXT = $8DD4;
  GL_UNSIGNED_INT_SAMPLER_2D_RECT_EXT = $8DD5;
  GL_UNSIGNED_INT_SAMPLER_1D_ARRAY_EXT = $8DD6;
  GL_UNSIGNED_INT_SAMPLER_2D_ARRAY_EXT = $8DD7;
  GL_UNSIGNED_INT_SAMPLER_BUFFER_EXT = $8DD8;
  GL_MIN_PROGRAM_TEXEL_OFFSET_EXT = $8904;
  GL_MAX_PROGRAM_TEXEL_OFFSET_EXT = $8905;

  // GL_EXT_packed_float
  GL_R11F_G11F_B10F_EXT = $8C3A;
  GL_UNSIGNED_INT_10F_11F_11F_REV_EXT = $8C3B;
  RGBA_SIGNED_COMPONENTS_EXT = $8C3C;
  WGL_TYPE_RGBA_UNSIGNED_FLOAT_EXT = $20A8;
  GLX_RGBA_UNSIGNED_FLOAT_TYPE_EXT = $20B1;
  GLX_RGBA_UNSIGNED_FLOAT_BIT_EXT = $00000008;

  // GL_EXT_texture_array
  GL_TEXTURE_1D_ARRAY_EXT = $8C18;
  GL_TEXTURE_2D_ARRAY_EXT = $8C1A;
  GL_PROXY_TEXTURE_2D_ARRAY_EXT = $8C1B;
  GL_PROXY_TEXTURE_1D_ARRAY_EXT = $8C19;
  GL_TEXTURE_BINDING_1D_ARRAY_EXT = $8C1C;
  GL_TEXTURE_BINDING_2D_ARRAY_EXT = $8C1D;
  GL_MAX_ARRAY_TEXTURE_LAYERS_EXT = $88FF;
  GL_COMPARE_REF_DEPTH_TO_TEXTURE_EXT = $884E;

  // GL_EXT_texture_buffer_object
  GL_TEXTURE_BUFFER_EXT = $8C2A;
  GL_MAX_TEXTURE_BUFFER_SIZE_EXT = $8C2B;
  GL_TEXTURE_BINDING_BUFFER_EXT = $8C2C;
  GL_TEXTURE_BUFFER_DATA_STORE_BINDING_EXT = $8C2D;
  GL_TEXTURE_BUFFER_FORMAT_EXT = $8C2E;

  // GL_EXT_texture_compression_latc
  GL_COMPRESSED_LUMINANCE_LATC1_EXT = $8C70;
  GL_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT = $8C71;
  GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT = $8C72;
  GL_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT = $8C73;

  // GL_EXT_texture_compression_rgtc
  GL_COMPRESSED_RED_RGTC1_EXT = $8DBB;
  GL_COMPRESSED_SIGNED_RED_RGTC1_EXT = $8DBC;
  GL_COMPRESSED_RED_GREEN_RGTC2_EXT = $8DBD;
  GL_COMPRESSED_SIGNED_RED_GREEN_RGTC2_EXT = $8DBE;

  // GL_EXT_texture_integer
  GL_RGBA_INTEGER_MODE_EXT = $8D9E;
  GL_RGBA32UI_EXT = $8D70;
  GL_RGB32UI_EXT = $8D71;
  GL_ALPHA32UI_EXT = $8D72;
  GL_INTENSITY32UI_EXT = $8D73;
  GL_LUMINANCE32UI_EXT = $8D74;
  GL_LUMINANCE_ALPHA32UI_EXT = $8D75;
  GL_RGBA16UI_EXT = $8D76;
  GL_RGB16UI_EXT = $8D77;
  GL_ALPHA16UI_EXT = $8D78;
  GL_INTENSITY16UI_EXT = $8D79;
  GL_LUMINANCE16UI_EXT = $8D7A;
  GL_LUMINANCE_ALPHA16UI_EXT = $8D7B;
  GL_RGBA8UI_EXT = $8D7C;
  GL_RGB8UI_EXT = $8D7D;
  GL_ALPHA8UI_EXT = $8D7E;
  GL_INTENSITY8UI_EXT = $8D7F;
  GL_LUMINANCE8UI_EXT = $8D80;
  GL_LUMINANCE_ALPHA8UI_EXT = $8D81;
  GL_RGBA32I_EXT = $8D82;
  GL_RGB32I_EXT = $8D83;
  GL_ALPHA32I_EXT = $8D84;
  GL_INTENSITY32I_EXT = $8D85;
  GL_LUMINANCE32I_EXT = $8D86;
  GL_LUMINANCE_ALPHA32I_EXT = $8D87;
  GL_RGBA16I_EXT = $8D88;
  GL_RGB16I_EXT = $8D89;
  GL_ALPHA16I_EXT = $8D8A;
  GL_INTENSITY16I_EXT = $8D8B;
  GL_LUMINANCE16I_EXT = $8D8C;
  GL_LUMINANCE_ALPHA16I_EXT = $8D8D;
  GL_RGBA8I_EXT = $8D8E;
  GL_RGB8I_EXT = $8D8F;
  GL_ALPHA8I_EXT = $8D90;
  GL_INTENSITY8I_EXT = $8D91;
  GL_LUMINANCE8I_EXT = $8D92;
  GL_LUMINANCE_ALPHA8I_EXT = $8D93;
  GL_RED_INTEGER_EXT = $8D94;
  GL_GREEN_INTEGER_EXT = $8D95;
  GL_BLUE_INTEGER_EXT = $8D96;
  GL_ALPHA_INTEGER_EXT = $8D97;
  GL_RGB_INTEGER_EXT = $8D98;
  GL_RGBA_INTEGER_EXT = $8D99;
  GL_BGR_INTEGER_EXT = $8D9A;
  GL_BGRA_INTEGER_EXT = $8D9B;
  GL_LUMINANCE_INTEGER_EXT = $8D9C;
  GL_LUMINANCE_ALPHA_INTEGER_EXT = $8D9D;

  // GL_EXT_texture_shared_exponent
  GL_RGB9_E5_EXT = $8C3D;
  GL_UNSIGNED_INT_5_9_9_9_REV_EXT = $8C3E;
  GL_TEXTURE_SHARED_SIZE_EXT = $8C3F;

  // GL_EXT_transform_feedback
  GL_TRANSFORM_FEEDBACK_BUFFER_EXT = $8C8E;
  GL_TRANSFORM_FEEDBACK_BUFFER_START_EXT = $8C84;
  GL_TRANSFORM_FEEDBACK_BUFFER_SIZE_EXT = $8C85;
  GL_TRANSFORM_FEEDBACK_BUFFER_BINDING_EXT = $8C8F;
  GL_INTERLEAVED_ATTRIBS_EXT = $8C8C;
  GL_SEPARATE_ATTRIBS_EXT = $8C8D;
  GL_PRIMITIVES_GENERATED_EXT = $8C87;
  GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN_EXT = $8C88;
  GL_RASTERIZER_DISCARD_EXT = $8C89;
  GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS_EXT = $8C8A;
  GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS_EXT = $8C8B;
  GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS_EXT = $8C80;
  GL_TRANSFORM_FEEDBACK_VARYINGS_EXT = $8C83;
  GL_TRANSFORM_FEEDBACK_BUFFER_MODE_EXT = $8C7F;
  GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH_EXT = $8C76;

  // GL_EXT_direct_state_access
  GL_PROGRAM_MATRIX_EXT = $8E2D;
  GL_TRANSPOSE_PROGRAM_MATRIX_EXT = $8E2E;
  GL_PROGRAM_MATRIX_STACK_DEPTH_EXT = $8E2F;

  // GL_EXT_texture_swizzle
  GL_TEXTURE_SWIZZLE_R_EXT = $8E42;
  GL_TEXTURE_SWIZZLE_G_EXT = $8E43;
  GL_TEXTURE_SWIZZLE_B_EXT = $8E44;
  GL_TEXTURE_SWIZZLE_A_EXT = $8E45;
  GL_TEXTURE_SWIZZLE_RGBA_EXT = $8E46;

  // GL_FfdMaskSGIX
  GL_TEXTURE_DEFORMATION_BIT_SGIX = $00000001;
  GL_GEOMETRY_DEFORMATION_BIT_SGIX = $00000002;

  // GL_HP_convolution_border_modes
  GL_IGNORE_BORDER_HP = $8150;
  GL_CONSTANT_BORDER_HP = $8151;
  GL_REPLICATE_BORDER_HP = $8153;
  GL_CONVOLUTION_BORDER_COLOR_HP = $8154;

  // GL_HP_image_transform
  GL_IMAGE_SCALE_X_HP = $8155;
  GL_IMAGE_SCALE_Y_HP = $8156;
  GL_IMAGE_TRANSLATE_X_HP = $8157;
  GL_IMAGE_TRANSLATE_Y_HP = $8158;
  GL_IMAGE_ROTATE_ANGLE_HP = $8159;
  GL_IMAGE_ROTATE_ORIGIN_X_HP = $815A;
  GL_IMAGE_ROTATE_ORIGIN_Y_HP = $815B;
  GL_IMAGE_MAG_FILTER_HP = $815C;
  GL_IMAGE_MIN_FILTER_HP = $815D;
  GL_IMAGE_CUBIC_WEIGHT_HP = $815E;
  GL_CUBIC_HP = $815F;
  GL_AVERAGE_HP = $8160;
  GL_IMAGE_TRANSFORM_2D_HP = $8161;
  GL_POST_IMAGE_TRANSFORM_COLOR_TABLE_HP = $8162;
  GL_PROXY_POST_IMAGE_TRANSFORM_COLOR_TABLE_HP = $8163;

  // GL_HP_occlusion_test
  GL_OCCLUSION_TEST_HP = $8165;
  GL_OCCLUSION_TEST_RESULT_HP = $8166;

  // GL_HP_texture_lighting
  GL_TEXTURE_LIGHTING_MODE_HP = $8167;
  GL_TEXTURE_POST_SPECULAR_HP = $8168;
  GL_TEXTURE_PRE_SPECULAR_HP = $8169;

  // GL_IBM_cull_vertex
  GL_CULL_VERTEX_IBM = 103050;

  // GL_IBM_rasterpos_clip
  GL_RASTER_POSITION_UNCLIPPED_IBM = $19262;

  // GL_IBM_texture_mirrored_repeat
  GL_MIRRORED_REPEAT_IBM = $8370;

  // GL_IBM_vertex_array_lists
  GL_VERTEX_ARRAY_LIST_IBM = 103070;
  GL_NORMAL_ARRAY_LIST_IBM = 103071;
  GL_COLOR_ARRAY_LIST_IBM = 103072;
  GL_INDEX_ARRAY_LIST_IBM = 103073;
  GL_TEXTURE_COORD_ARRAY_LIST_IBM = 103074;
  GL_EDGE_FLAG_ARRAY_LIST_IBM = 103075;
  GL_FOG_COORDINATE_ARRAY_LIST_IBM = 103076;
  GL_SECONDARY_COLOR_ARRAY_LIST_IBM = 103077;
  GL_VERTEX_ARRAY_LIST_STRIDE_IBM = 103080;
  GL_NORMAL_ARRAY_LIST_STRIDE_IBM = 103081;
  GL_COLOR_ARRAY_LIST_STRIDE_IBM = 103082;
  GL_INDEX_ARRAY_LIST_STRIDE_IBM = 103083;
  GL_TEXTURE_COORD_ARRAY_LIST_STRIDE_IBM = 103084;
  GL_EDGE_FLAG_ARRAY_LIST_STRIDE_IBM = 103085;
  GL_FOG_COORDINATE_ARRAY_LIST_STRIDE_IBM = 103086;
  GL_SECONDARY_COLOR_ARRAY_LIST_STRIDE_IBM = 103087;

  // GL_INGR_color_clamp
  GL_RED_MIN_CLAMP_INGR = $8560;
  GL_GREEN_MIN_CLAMP_INGR = $8561;
  GL_BLUE_MIN_CLAMP_INGR = $8562;
  GL_ALPHA_MIN_CLAMP_INGR = $8563;
  GL_RED_MAX_CLAMP_INGR = $8564;
  GL_GREEN_MAX_CLAMP_INGR = $8565;
  GL_BLUE_MAX_CLAMP_INGR = $8566;
  GL_ALPHA_MAX_CLAMP_INGR = $8567;

  // GL_INGR_interlace_read
  GL_INTERLACE_READ_INGR = $8568;

  // GL_INTEL_parallel_arrays
  GL_PARALLEL_ARRAYS_INTEL = $83F4;
  GL_VERTEX_ARRAY_PARALLEL_POINTERS_INTEL = $83F5;
  GL_NORMAL_ARRAY_PARALLEL_POINTERS_INTEL = $83F6;
  GL_COLOR_ARRAY_PARALLEL_POINTERS_INTEL = $83F7;
  GL_TEXTURE_COORD_ARRAY_PARALLEL_POINTERS_INTEL = $83F8;

  // GL_NV_copy_depth_to_color
  GL_DEPTH_STENCIL_TO_RGBA_NV = $886E;
  GL_DEPTH_STENCIL_TO_BGRA_NV = $886F;

  // GL_NV_depth_clamp
  GL_DEPTH_CLAMP_NV = $864F;

  // GL_NV_evaluators
  GL_EVAL_2D_NV = $86C0;
  GL_EVAL_TRIANGULAR_2D_NV = $86C1;
  GL_MAP_TESSELLATION_NV = $86C2;
  GL_MAP_ATTRIB_U_ORDER_NV = $86C3;
  GL_MAP_ATTRIB_V_ORDER_NV = $86C4;
  GL_EVAL_FRACTIONAL_TESSELLATION_NV = $86C5;
  GL_EVAL_VERTEX_ATTRIB0_NV = $86C6;
  GL_EVAL_VERTEX_ATTRIB1_NV = $86C7;
  GL_EVAL_VERTEX_ATTRIB2_NV = $86C8;
  GL_EVAL_VERTEX_ATTRIB3_NV = $86C9;
  GL_EVAL_VERTEX_ATTRIB4_NV = $86CA;
  GL_EVAL_VERTEX_ATTRIB5_NV = $86CB;
  GL_EVAL_VERTEX_ATTRIB6_NV = $86CC;
  GL_EVAL_VERTEX_ATTRIB7_NV = $86CD;
  GL_EVAL_VERTEX_ATTRIB8_NV = $86CE;
  GL_EVAL_VERTEX_ATTRIB9_NV = $86CF;
  GL_EVAL_VERTEX_ATTRIB10_NV = $86D0;
  GL_EVAL_VERTEX_ATTRIB11_NV = $86D1;
  GL_EVAL_VERTEX_ATTRIB12_NV = $86D2;
  GL_EVAL_VERTEX_ATTRIB13_NV = $86D3;
  GL_EVAL_VERTEX_ATTRIB14_NV = $86D4;
  GL_EVAL_VERTEX_ATTRIB15_NV = $86D5;
  GL_MAX_MAP_TESSELLATION_NV = $86D6;
  GL_MAX_RATIONAL_EVAL_ORDER_NV = $86D7;

  // GL_NV_fence
  GL_ALL_COMPLETED_NV = $84F2;
  GL_FENCE_STATUS_NV = $84F3;
  GL_FENCE_CONDITION_NV = $84F4;

  // GL_NV_float_buffer
  GL_FLOAT_R_NV = $8880;
  GL_FLOAT_RG_NV = $8881;
  GL_FLOAT_RGB_NV = $8882;
  GL_FLOAT_RGBA_NV = $8883;
  GL_FLOAT_R16_NV = $8884;
  GL_FLOAT_R32_NV = $8885;
  GL_FLOAT_RG16_NV = $8886;
  GL_FLOAT_RG32_NV = $8887;
  GL_FLOAT_RGB16_NV = $8888;
  GL_FLOAT_RGB32_NV = $8889;
  GL_FLOAT_RGBA16_NV = $888A;
  GL_FLOAT_RGBA32_NV = $888B;
  GL_TEXTURE_FLOAT_COMPONENTS_NV = $888C;
  GL_FLOAT_CLEAR_COLOR_VALUE_NV = $888D;
  GL_FLOAT_RGBA_MODE_NV = $888E;

  // GL_NV_fog_distance
  GL_FOG_DISTANCE_MODE_NV = $855A;
  GL_EYE_RADIAL_NV = $855B;
  GL_EYE_PLANE_ABSOLUTE_NV = $855C;

  // GL_NV_fragment_program
  GL_MAX_FRAGMENT_PROGRAM_LOCAL_PARAMETERS_NV = $8868;
  GL_FRAGMENT_PROGRAM_NV = $8870;
  GL_MAX_TEXTURE_COORDS_NV = $8871;
  GL_MAX_TEXTURE_IMAGE_UNITS_NV = $8872;
  GL_FRAGMENT_PROGRAM_BINDING_NV = $8873;
  GL_PROGRAM_ERROR_STRING_NV = $8874;

  // GL_NV_half_float
  GL_HALF_FLOAT_NV = $140B;

  // GL_NV_light_max_exponent
  GL_MAX_SHININESS_NV = $8504;
  GL_MAX_SPOT_EXPONENT_NV = $8505;

  // GL_NV_multisample_filter_hint
  GL_MULTISAMPLE_FILTER_HINT_NV = $8534;

  // GL_NV_occlusion_query
  GL_PIXEL_COUNTER_BITS_NV = $8864;
  GL_CURRENT_OCCLUSION_QUERY_ID_NV = $8865;
  GL_PIXEL_COUNT_NV = $8866;
  GL_PIXEL_COUNT_AVAILABLE_NV = $8867;

  // GL_NV_packed_depth_stencil
  GL_DEPTH_STENCIL_NV = $84F9;
  GL_UNSIGNED_INT_24_8_NV = $84FA;

  // GL_NV_pixel_data_range
  GL_WRITE_PIXEL_DATA_RANGE_NV = $8878;
  GL_READ_PIXEL_DATA_RANGE_NV = $8879;
  GL_WRITE_PIXEL_DATA_RANGE_LENGTH_NV = $887A;
  GL_READ_PIXEL_DATA_RANGE_LENGTH_NV = $887B;
  GL_WRITE_PIXEL_DATA_RANGE_POINTER_NV = $887C;
  GL_READ_PIXEL_DATA_RANGE_POINTER_NV = $887D;

  // GL_NV_point_sprite
  GL_POINT_SPRITE_NV = $8861;
  GL_COORD_REPLACE_NV = $8862;
  GL_POINT_SPRITE_R_MODE_NV = $8863;

  // GL_NV_primitive_restart
  GL_PRIMITIVE_RESTART_NV = $8558;
  GL_PRIMITIVE_RESTART_INDEX_NV = $8559;

  // GL_NV_register_combiners
  GL_REGISTER_COMBINERS_NV = $8522;
  GL_VARIABLE_A_NV = $8523;
  GL_VARIABLE_B_NV = $8524;
  GL_VARIABLE_C_NV = $8525;
  GL_VARIABLE_D_NV = $8526;
  GL_VARIABLE_E_NV = $8527;
  GL_VARIABLE_F_NV = $8528;
  GL_VARIABLE_G_NV = $8529;
  GL_CONSTANT_COLOR0_NV = $852A;
  GL_CONSTANT_COLOR1_NV = $852B;
  GL_PRIMARY_COLOR_NV = $852C;
  GL_SECONDARY_COLOR_NV = $852D;
  GL_SPARE0_NV = $852E;
  GL_SPARE1_NV = $852F;
  GL_DISCARD_NV = $8530;
  GL_E_TIMES_F_NV = $8531;
  GL_SPARE0_PLUS_SECONDARY_COLOR_NV = $8532;
  GL_UNSIGNED_IDENTITY_NV = $8536;
  GL_UNSIGNED_INVERT_NV = $8537;
  GL_EXPAND_NORMAL_NV = $8538;
  GL_EXPAND_NEGATE_NV = $8539;
  GL_HALF_BIAS_NORMAL_NV = $853A;
  GL_HALF_BIAS_NEGATE_NV = $853B;
  GL_SIGNED_IDENTITY_NV = $853C;
  GL_SIGNED_NEGATE_NV = $853D;
  GL_SCALE_BY_TWO_NV = $853E;
  GL_SCALE_BY_FOUR_NV = $853F;
  GL_SCALE_BY_ONE_HALF_NV = $8540;
  GL_BIAS_BY_NEGATIVE_ONE_HALF_NV = $8541;
  GL_COMBINER_INPUT_NV = $8542;
  GL_COMBINER_MAPPING_NV = $8543;
  GL_COMBINER_COMPONENT_USAGE_NV = $8544;
  GL_COMBINER_AB_DOT_PRODUCT_NV = $8545;
  GL_COMBINER_CD_DOT_PRODUCT_NV = $8546;
  GL_COMBINER_MUX_SUM_NV = $8547;
  GL_COMBINER_SCALE_NV = $8548;
  GL_COMBINER_BIAS_NV = $8549;
  GL_COMBINER_AB_OUTPUT_NV = $854A;
  GL_COMBINER_CD_OUTPUT_NV = $854B;
  GL_COMBINER_SUM_OUTPUT_NV = $854C;
  GL_MAX_GENERAL_COMBINERS_NV = $854D;
  GL_NUM_GENERAL_COMBINERS_NV = $854E;
  GL_COLOR_SUM_CLAMP_NV = $854F;
  GL_COMBINER0_NV = $8550;
  GL_COMBINER1_NV = $8551;
  GL_COMBINER2_NV = $8552;
  GL_COMBINER3_NV = $8553;
  GL_COMBINER4_NV = $8554;
  GL_COMBINER5_NV = $8555;
  GL_COMBINER6_NV = $8556;
  GL_COMBINER7_NV = $8557;

  // GL_NV_register_combiners2
  GL_PER_STAGE_CONSTANTS_NV = $8535;

  // GL_NV_texgen_emboss
  GL_EMBOSS_LIGHT_NV = $855D;
  GL_EMBOSS_CONSTANT_NV = $855E;
  GL_EMBOSS_MAP_NV = $855F;

  // GL_NV_texgen_reflection
  GL_NORMAL_MAP_NV = $8511;
  GL_REFLECTION_MAP_NV = $8512;

  // GL_NV_texture_env_combine4
  GL_COMBINE4_NV = $8503;
  GL_SOURCE3_RGB_NV = $8583;
  GL_SOURCE3_ALPHA_NV = $858B;
  GL_OPERAND3_RGB_NV = $8593;
  GL_OPERAND3_ALPHA_NV = $859B;

  // GL_NV_texture_expand_normal
  GL_TEXTURE_UNSIGNED_REMAP_MODE_NV = $888F;

  // GL_NV_texture_rectangle
  GL_TEXTURE_RECTANGLE_NV = $84F5;
  GL_TEXTURE_BINDING_RECTANGLE_NV = $84F6;
  GL_PROXY_TEXTURE_RECTANGLE_NV = $84F7;
  GL_MAX_RECTANGLE_TEXTURE_SIZE_NV = $84F8;

  // GL_NV_texture_shader
  GL_OFFSET_TEXTURE_RECTANGLE_NV = $864C;
  GL_OFFSET_TEXTURE_RECTANGLE_SCALE_NV = $864D;
  GL_DOT_PRODUCT_TEXTURE_RECTANGLE_NV = $864E;
  GL_RGBA_UNSIGNED_DOT_PRODUCT_MAPPING_NV = $86D9;
  GL_UNSIGNED_INT_S8_S8_8_8_NV = $86DA;
  GL_UNSIGNED_INT_8_8_S8_S8_REV_NV = $86DB;
  GL_DSDT_MAG_INTENSITY_NV = $86DC;
  GL_SHADER_CONSISTENT_NV = $86DD;
  GL_TEXTURE_SHADER_NV = $86DE;
  GL_SHADER_OPERATION_NV = $86DF;
  GL_CULL_MODES_NV = $86E0;
  GL_OFFSET_TEXTURE_MATRIX_NV = $86E1;
  GL_OFFSET_TEXTURE_SCALE_NV = $86E2;
  GL_OFFSET_TEXTURE_BIAS_NV = $86E3;
  GL_OFFSET_TEXTURE_2D_MATRIX_NV = GL_OFFSET_TEXTURE_MATRIX_NV;
  GL_OFFSET_TEXTURE_2D_SCALE_NV = GL_OFFSET_TEXTURE_SCALE_NV;
  GL_OFFSET_TEXTURE_2D_BIAS_NV = GL_OFFSET_TEXTURE_BIAS_NV;
  GL_PREVIOUS_TEXTURE_INPUT_NV = $86E4;
  GL_CONST_EYE_NV = $86E5;
  GL_PASS_THROUGH_NV = $86E6;
  GL_CULL_FRAGMENT_NV = $86E7;
  GL_OFFSET_TEXTURE_2D_NV = $86E8;
  GL_DEPENDENT_AR_TEXTURE_2D_NV = $86E9;
  GL_DEPENDENT_GB_TEXTURE_2D_NV = $86EA;
  GL_DOT_PRODUCT_NV = $86EC;
  GL_DOT_PRODUCT_DEPTH_REPLACE_NV = $86ED;
  GL_DOT_PRODUCT_TEXTURE_2D_NV = $86EE;
  GL_DOT_PRODUCT_TEXTURE_CUBE_MAP_NV = $86F0;
  GL_DOT_PRODUCT_DIFFUSE_CUBE_MAP_NV = $86F1;
  GL_DOT_PRODUCT_REFLECT_CUBE_MAP_NV = $86F2;
  GL_DOT_PRODUCT_CONST_EYE_REFLECT_CUBE_MAP_NV = $86F3;
  GL_HILO_NV = $86F4;
  GL_DSDT_NV = $86F5;
  GL_DSDT_MAG_NV = $86F6;
  GL_DSDT_MAG_VIB_NV = $86F7;
  GL_HILO16_NV = $86F8;
  GL_SIGNED_HILO_NV = $86F9;
  GL_SIGNED_HILO16_NV = $86FA;
  GL_SIGNED_RGBA_NV = $86FB;
  GL_SIGNED_RGBA8_NV = $86FC;
  GL_SIGNED_RGB_NV = $86FE;
  GL_SIGNED_RGB8_NV = $86FF;
  GL_SIGNED_LUMINANCE_NV = $8701;
  GL_SIGNED_LUMINANCE8_NV = $8702;
  GL_SIGNED_LUMINANCE_ALPHA_NV = $8703;
  GL_SIGNED_LUMINANCE8_ALPHA8_NV = $8704;
  GL_SIGNED_ALPHA_NV = $8705;
  GL_SIGNED_ALPHA8_NV = $8706;
  GL_SIGNED_INTENSITY_NV = $8707;
  GL_SIGNED_INTENSITY8_NV = $8708;
  GL_DSDT8_NV = $8709;
  GL_DSDT8_MAG8_NV = $870A;
  GL_DSDT8_MAG8_INTENSITY8_NV = $870B;
  GL_SIGNED_RGB_UNSIGNED_ALPHA_NV = $870C;
  GL_SIGNED_RGB8_UNSIGNED_ALPHA8_NV = $870D;
  GL_HI_SCALE_NV = $870E;
  GL_LO_SCALE_NV = $870F;
  GL_DS_SCALE_NV = $8710;
  GL_DT_SCALE_NV = $8711;
  GL_MAGNITUDE_SCALE_NV = $8712;
  GL_VIBRANCE_SCALE_NV = $8713;
  GL_HI_BIAS_NV = $8714;
  GL_LO_BIAS_NV = $8715;
  GL_DS_BIAS_NV = $8716;
  GL_DT_BIAS_NV = $8717;
  GL_MAGNITUDE_BIAS_NV = $8718;
  GL_VIBRANCE_BIAS_NV = $8719;
  GL_TEXTURE_BORDER_VALUES_NV = $871A;
  GL_TEXTURE_HI_SIZE_NV = $871B;
  GL_TEXTURE_LO_SIZE_NV = $871C;
  GL_TEXTURE_DS_SIZE_NV = $871D;
  GL_TEXTURE_DT_SIZE_NV = $871E;
  GL_TEXTURE_MAG_SIZE_NV = $871F;

  // GL_NV_texture_shader2
  GL_DOT_PRODUCT_TEXTURE_3D_NV = $86EF;

  // GL_NV_texture_shader3
  GL_OFFSET_PROJECTIVE_TEXTURE_2D_NV = $8850;
  GL_OFFSET_PROJECTIVE_TEXTURE_2D_SCALE_NV = $8851;
  GL_OFFSET_PROJECTIVE_TEXTURE_RECTANGLE_NV = $8852;
  GL_OFFSET_PROJECTIVE_TEXTURE_RECTANGLE_SCALE_NV = $8853;
  GL_OFFSET_HILO_TEXTURE_2D_NV = $8854;
  GL_OFFSET_HILO_TEXTURE_RECTANGLE_NV = $8855;
  GL_OFFSET_HILO_PROJECTIVE_TEXTURE_2D_NV = $8856;
  GL_OFFSET_HILO_PROJECTIVE_TEXTURE_RECTANGLE_NV = $8857;
  GL_DEPENDENT_HILO_TEXTURE_2D_NV = $8858;
  GL_DEPENDENT_RGB_TEXTURE_3D_NV = $8859;
  GL_DEPENDENT_RGB_TEXTURE_CUBE_MAP_NV = $885A;
  GL_DOT_PRODUCT_PASS_THROUGH_NV = $885B;
  GL_DOT_PRODUCT_TEXTURE_1D_NV = $885C;
  GL_DOT_PRODUCT_AFFINE_DEPTH_REPLACE_NV = $885D;
  GL_HILO8_NV = $885E;
  GL_SIGNED_HILO8_NV = $885F;
  GL_FORCE_BLUE_TO_ONE_NV = $8860;

  // GL_NV_vertex_array_range
  GL_VERTEX_ARRAY_RANGE_NV = $851D;
  GL_VERTEX_ARRAY_RANGE_LENGTH_NV = $851E;
  GL_VERTEX_ARRAY_RANGE_VALID_NV = $851F;
  GL_MAX_VERTEX_ARRAY_RANGE_ELEMENT_NV = $8520;
  GL_VERTEX_ARRAY_RANGE_POINTER_NV = $8521;

  // GL_NV_vertex_array_range2
  GL_VERTEX_ARRAY_RANGE_WITHOUT_FLUSH_NV = $8533;

  // GL_NV_vertex_program
  GL_VERTEX_PROGRAM_NV = $8620;
  GL_VERTEX_STATE_PROGRAM_NV = $8621;
  GL_ATTRIB_ARRAY_SIZE_NV = $8623;
  GL_ATTRIB_ARRAY_STRIDE_NV = $8624;
  GL_ATTRIB_ARRAY_TYPE_NV = $8625;
  GL_CURRENT_ATTRIB_NV = $8626;
  GL_PROGRAM_LENGTH_NV = $8627;
  GL_PROGRAM_STRING_NV = $8628;
  GL_MODELVIEW_PROJECTION_NV = $8629;
  GL_IDENTITY_NV = $862A;
  GL_INVERSE_NV = $862B;
  GL_TRANSPOSE_NV = $862C;
  GL_INVERSE_TRANSPOSE_NV = $862D;
  GL_MAX_TRACK_MATRIX_STACK_DEPTH_NV = $862E;
  GL_MAX_TRACK_MATRICES_NV = $862F;
  GL_MATRIX0_NV = $8630;
  GL_MATRIX1_NV = $8631;
  GL_MATRIX2_NV = $8632;
  GL_MATRIX3_NV = $8633;
  GL_MATRIX4_NV = $8634;
  GL_MATRIX5_NV = $8635;
  GL_MATRIX6_NV = $8636;
  GL_MATRIX7_NV = $8637;
  GL_CURRENT_MATRIX_STACK_DEPTH_NV = $8640;
  GL_CURRENT_MATRIX_NV = $8641;
  GL_VERTEX_PROGRAM_POINT_SIZE_NV = $8642;
  GL_VERTEX_PROGRAM_TWO_SIDE_NV = $8643;
  GL_PROGRAM_PARAMETER_NV = $8644;
  GL_ATTRIB_ARRAY_POINTER_NV = $8645;
  GL_PROGRAM_TARGET_NV = $8646;
  GL_PROGRAM_RESIDENT_NV = $8647;
  GL_TRACK_MATRIX_NV = $8648;
  GL_TRACK_MATRIX_TRANSFORM_NV = $8649;
  GL_VERTEX_PROGRAM_BINDING_NV = $864A;
  GL_PROGRAM_ERROR_POSITION_NV = $864B;
  GL_VERTEX_ATTRIB_ARRAY0_NV = $8650;
  GL_VERTEX_ATTRIB_ARRAY1_NV = $8651;
  GL_VERTEX_ATTRIB_ARRAY2_NV = $8652;
  GL_VERTEX_ATTRIB_ARRAY3_NV = $8653;
  GL_VERTEX_ATTRIB_ARRAY4_NV = $8654;
  GL_VERTEX_ATTRIB_ARRAY5_NV = $8655;
  GL_VERTEX_ATTRIB_ARRAY6_NV = $8656;
  GL_VERTEX_ATTRIB_ARRAY7_NV = $8657;
  GL_VERTEX_ATTRIB_ARRAY8_NV = $8658;
  GL_VERTEX_ATTRIB_ARRAY9_NV = $8659;
  GL_VERTEX_ATTRIB_ARRAY10_NV = $865A;
  GL_VERTEX_ATTRIB_ARRAY11_NV = $865B;
  GL_VERTEX_ATTRIB_ARRAY12_NV = $865C;
  GL_VERTEX_ATTRIB_ARRAY13_NV = $865D;
  GL_VERTEX_ATTRIB_ARRAY14_NV = $865E;
  GL_VERTEX_ATTRIB_ARRAY15_NV = $865F;
  GL_MAP1_VERTEX_ATTRIB0_4_NV = $8660;
  GL_MAP1_VERTEX_ATTRIB1_4_NV = $8661;
  GL_MAP1_VERTEX_ATTRIB2_4_NV = $8662;
  GL_MAP1_VERTEX_ATTRIB3_4_NV = $8663;
  GL_MAP1_VERTEX_ATTRIB4_4_NV = $8664;
  GL_MAP1_VERTEX_ATTRIB5_4_NV = $8665;
  GL_MAP1_VERTEX_ATTRIB6_4_NV = $8666;
  GL_MAP1_VERTEX_ATTRIB7_4_NV = $8667;
  GL_MAP1_VERTEX_ATTRIB8_4_NV = $8668;
  GL_MAP1_VERTEX_ATTRIB9_4_NV = $8669;
  GL_MAP1_VERTEX_ATTRIB10_4_NV = $866A;
  GL_MAP1_VERTEX_ATTRIB11_4_NV = $866B;
  GL_MAP1_VERTEX_ATTRIB12_4_NV = $866C;
  GL_MAP1_VERTEX_ATTRIB13_4_NV = $866D;
  GL_MAP1_VERTEX_ATTRIB14_4_NV = $866E;
  GL_MAP1_VERTEX_ATTRIB15_4_NV = $866F;
  GL_MAP2_VERTEX_ATTRIB0_4_NV = $8670;
  GL_MAP2_VERTEX_ATTRIB1_4_NV = $8671;
  GL_MAP2_VERTEX_ATTRIB2_4_NV = $8672;
  GL_MAP2_VERTEX_ATTRIB3_4_NV = $8673;
  GL_MAP2_VERTEX_ATTRIB4_4_NV = $8674;
  GL_MAP2_VERTEX_ATTRIB5_4_NV = $8675;
  GL_MAP2_VERTEX_ATTRIB6_4_NV = $8676;
  GL_MAP2_VERTEX_ATTRIB7_4_NV = $8677;
  GL_MAP2_VERTEX_ATTRIB8_4_NV = $8678;
  GL_MAP2_VERTEX_ATTRIB9_4_NV = $8679;
  GL_MAP2_VERTEX_ATTRIB10_4_NV = $867A;
  GL_MAP2_VERTEX_ATTRIB11_4_NV = $867B;
  GL_MAP2_VERTEX_ATTRIB12_4_NV = $867C;
  GL_MAP2_VERTEX_ATTRIB13_4_NV = $867D;
  GL_MAP2_VERTEX_ATTRIB14_4_NV = $867E;
  GL_MAP2_VERTEX_ATTRIB15_4_NV = $867F;

  // GL_NV_fragment_program2 and GL_NV_vertex_program2_option
  GL_MAX_PROGRAM_EXEC_INSTRUCTIONS_NV = $88F4;
  GL_MAX_PROGRAM_CALL_DEPTH_NV = $88F5;

  // GL_NV_fragment_program2
  GL_MAX_PROGRAM_IF_DEPTH_NV = $88F6;
  GL_MAX_PROGRAM_LOOP_DEPTH_NV = $88F7;
  GL_MAX_PROGRAM_LOOP_COUNT_NV = $88F8;

  // GL_NV_vertex_program3
  MAX_VERTEX_TEXTURE_IMAGE_UNITS_ARB = $8B4C;

  // GL_NV_depth_buffer_float
  GL_FLOAT_32_UNSIGNED_INT_24_8_REV_NV = $8DAD;
  GL_DEPTH_BUFFER_FLOAT_MODE_NV = $8DAF;

  // GL_NV_framebuffer_multisample_coverage
  GL_RENDERBUFFER_COVERAGE_SAMPLES_NV = $8CAB;
  GL_RENDERBUFFER_COLOR_SAMPLES_NV = $8E10;

  // GL_NV_geometry_program4
  GL_GEOMETRY_PROGRAM_NV = $8C26;
  GL_MAX_PROGRAM_OUTPUT_VERTICES_NV = $8C27;
  GL_MAX_PROGRAM_TOTAL_OUTPUT_COMPONENTS_NV = $8C28;

  // GL_NV_gpu_program4
  GL_PROGRAM_ATTRIB_COMPONENTS_NV = $8906;
  GL_PROGRAM_RESULT_COMPONENTS_NV = $8907;
  GL_MAX_PROGRAM_ATTRIB_COMPONENTS_NV = $8908;
  GL_MAX_PROGRAM_RESULT_COMPONENTS_NV = $8909;
  GL_MAX_PROGRAM_GENERIC_ATTRIBS_NV = $8DA5;
  GL_MAX_PROGRAM_GENERIC_RESULTS_NV = $8DA6;

  // GL_NV_parameter_buffer_object
  GL_MAX_PROGRAM_PARAMETER_BUFFER_BINDINGS_NV = $8DA0;
  GL_MAX_PROGRAM_PARAMETER_BUFFER_SIZE_NV = $8DA1;
  GL_VERTEX_PROGRAM_PARAMETER_BUFFER_NV = $8DA2;
  GL_GEOMETRY_PROGRAM_PARAMETER_BUFFER_NV = $8DA3;
  GL_FRAGMENT_PROGRAM_PARAMETER_BUFFER_NV = $8DA4;

  // GL_NV_transform_feedback
  GL_TRANSFORM_FEEDBACK_BUFFER_NV = $8C8E;
  GL_TRANSFORM_FEEDBACK_BUFFER_START_NV = $8C84;
  GL_TRANSFORM_FEEDBACK_BUFFER_SIZE_NV = $8C85;
  GL_TRANSFORM_FEEDBACK_RECORD_NV = $8C86;
  GL_TRANSFORM_FEEDBACK_BUFFER_BINDING_NV = $8C8F;
  GL_INTERLEAVED_ATTRIBS_NV = $8C8C;
  GL_SEPARATE_ATTRIBS_NV = $8C8D;
  GL_PRIMITIVES_GENERATED_NV = $8C87;
  GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN_NV = $8C88;
  GL_RASTERIZER_DISCARD_NV = $8C89;
  GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS_NV = $8C8A;
  GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS_NV = $8C8B;
  GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS_NV = $8C80;
  GL_TRANSFORM_FEEDBACK_ATTRIBS_NV = $8C7E;
  GL_ACTIVE_VARYINGS_NV = $8C81;
  GL_ACTIVE_VARYING_MAX_LENGTH_NV = $8C82;
  GL_TRANSFORM_FEEDBACK_VARYINGS_NV = $8C83;
  GL_TRANSFORM_FEEDBACK_BUFFER_MODE_NV = $8C7F;
  GL_BACK_PRIMARY_COLOR_NV = $8C77;
  GL_BACK_SECONDARY_COLOR_NV = $8C78;
  GL_TEXTURE_COORD_NV = $8C79;
  GL_CLIP_DISTANCE_NV = $8C7A;
  GL_VERTEX_ID_NV = $8C7B;
  GL_PRIMITIVE_ID_NV = $8C7C;
  GL_GENERIC_ATTRIB_NV = $8C7D;

  // GL_NV_conditional_render
  GL_QUERY_WAIT_NV = $8E13;
  GL_QUERY_NO_WAIT_NV = $8E14;
  GL_QUERY_BY_REGION_WAIT_NV = $8E15;
  GL_QUERY_BY_REGION_NO_WAIT_NV = $8E16;

  // GL_NV_present_video
  GL_FRAME_NV = $8E26;
  GL_FIELDS_NV = $8E27;
  GL_CURRENT_TIME_NV = $8E28;
  GL_NUM_FILL_STREAMS_NV = $8E29;
  GL_PRESENT_TIME_NV = $8E2A;
  GL_PRESENT_DURATION_NV = $8E2B;

  // GL_NV_explicit_multisample
  GL_SAMPLE_POSITION_NV = $8E50;
  GL_SAMPLE_MASK_NV = $8E51;
  GL_SAMPLE_MASK_VALUE_NV = $8E52;
  GL_TEXTURE_BINDING_RENDERBUFFER_NV = $8E53;
  GL_TEXTURE_RENDERBUFFER_DATA_STORE_BINDING_NV = $8E54;
  GL_MAX_SAMPLE_MASK_WORDS_NV = $8E59;
  GL_TEXTURE_RENDERBUFFER_NV = $8E55;
  GL_SAMPLER_RENDERBUFFER_NV = $8E56;
  GL_INT_SAMPLER_RENDERBUFFER_NV = $8E57;
  GL_UNSIGNED_INT_SAMPLER_RENDERBUFFER_NV = $8E58;

  // GL_NV_transform_feedback2
  GL_TRANSFORM_FEEDBACK_NV = $8E22;
  GL_TRANSFORM_FEEDBACK_BUFFER_PAUSED_NV = $8E23;
  GL_TRANSFORM_FEEDBACK_BUFFER_ACTIVE_NV = $8E24;
  GL_TRANSFORM_FEEDBACK_BINDING_NV = $8E25;

  // GL_OML_interlace
  GL_INTERLACE_OML = $8980;
  GL_INTERLACE_READ_OML = $8981;

  // GL_OML_resample
  GL_PACK_RESAMPLE_OML = $8984;
  GL_UNPACK_RESAMPLE_OML = $8985;
  GL_RESAMPLE_REPLICATE_OML = $8986;
  GL_RESAMPLE_ZERO_FILL_OML = $8987;
  GL_RESAMPLE_AVERAGE_OML = $8988;
  GL_RESAMPLE_DECIMATE_OML = $8989;

  // GL_OML_subsample
  GL_FORMAT_SUBSAMPLE_24_24_OML = $8982;
  GL_FORMAT_SUBSAMPLE_244_244_OML = $8983;

  // GL_PGI_misc_hints
  GL_PREFER_DOUBLEBUFFER_HINT_PGI = $1A1F8;
  GL_CONSERVE_MEMORY_HINT_PGI = $1A1FD;
  GL_RECLAIM_MEMORY_HINT_PGI = $1A1FE;
  GL_NATIVE_GRAPHICS_HANDLE_PGI = $1A202;
  GL_NATIVE_GRAPHICS_BEGIN_HINT_PGI = $1A203;
  GL_NATIVE_GRAPHICS_END_HINT_PGI = $1A204;
  GL_ALWAYS_FAST_HINT_PGI = $1A20C;
  GL_ALWAYS_SOFT_HINT_PGI = $1A20D;
  GL_ALLOW_DRAW_OBJ_HINT_PGI = $1A20E;
  GL_ALLOW_DRAW_WIN_HINT_PGI = $1A20F;
  GL_ALLOW_DRAW_FRG_HINT_PGI = $1A210;
  GL_ALLOW_DRAW_MEM_HINT_PGI = $1A211;
  GL_STRICT_DEPTHFUNC_HINT_PGI = $1A216;
  GL_STRICT_LIGHTING_HINT_PGI = $1A217;
  GL_STRICT_SCISSOR_HINT_PGI = $1A218;
  GL_FULL_STIPPLE_HINT_PGI = $1A219;
  GL_CLIP_NEAR_HINT_PGI = $1A220;
  GL_CLIP_FAR_HINT_PGI = $1A221;
  GL_WIDE_LINE_HINT_PGI = $1A222;
  GL_BACK_NORMALS_HINT_PGI = $1A223;

  // GL_PGI_vertex_hints
  GL_VERTEX_DATA_HINT_PGI = $1A22A;
  GL_VERTEX_CONSISTENT_HINT_PGI = $1A22B;
  GL_MATERIAL_SIDE_HINT_PGI = $1A22C;
  GL_MAX_VERTEX_HINT_PGI = $1A22D;
  GL_COLOR3_BIT_PGI = $00010000;
  GL_COLOR4_BIT_PGI = $00020000;
  GL_EDGEFLAG_BIT_PGI = $00040000;
  GL_INDEX_BIT_PGI = $00080000;
  GL_MAT_AMBIENT_BIT_PGI = $00100000;
  GL_MAT_AMBIENT_AND_DIFFUSE_BIT_PGI = $00200000;
  GL_MAT_DIFFUSE_BIT_PGI = $00400000;
  GL_MAT_EMISSION_BIT_PGI = $00800000;
  GL_MAT_COLOR_INDEXES_BIT_PGI = $01000000;
  GL_MAT_SHININESS_BIT_PGI = $02000000;
  GL_MAT_SPECULAR_BIT_PGI = $04000000;
  GL_NORMAL_BIT_PGI = $08000000;
  GL_TEXCOORD1_BIT_PGI = $10000000;
  GL_TEXCOORD2_BIT_PGI = $20000000;
  GL_TEXCOORD3_BIT_PGI = $40000000;
  GL_TEXCOORD4_BIT_PGI = $80000000;
  GL_VERTEX23_BIT_PGI = $00000004;
  GL_VERTEX4_BIT_PGI = $00000008;

  // GL_REND_screen_coordinates
  GL_SCREEN_COORDINATES_REND = $8490;
  GL_INVERTED_SCREEN_W_REND = $8491;

  // GL_S3_s3tc
  GL_RGB_S3TC = $83A0;
  GL_RGB4_S3TC = $83A1;
  GL_RGBA_S3TC = $83A2;
  GL_RGBA4_S3TC = $83A3;

  // GL_SGIS_detail_texture
  GL_DETAIL_TEXTURE_2D_SGIS = $8095;
  GL_DETAIL_TEXTURE_2D_BINDING_SGIS = $8096;
  GL_LINEAR_DETAIL_SGIS = $8097;
  GL_LINEAR_DETAIL_ALPHA_SGIS = $8098;
  GL_LINEAR_DETAIL_COLOR_SGIS = $8099;
  GL_DETAIL_TEXTURE_LEVEL_SGIS = $809A;
  GL_DETAIL_TEXTURE_MODE_SGIS = $809B;
  GL_DETAIL_TEXTURE_FUNC_POINTS_SGIS = $809C;

  // GL_SGIS_fog_function
  GL_FOG_FUNC_SGIS = $812A;
  GL_FOG_FUNC_POINTS_SGIS = $812B;
  GL_MAX_FOG_FUNC_POINTS_SGIS = $812C;

  // GL_SGIS_generate_mipmap
  GL_GENERATE_MIPMAP_SGIS = $8191;
  GL_GENERATE_MIPMAP_HINT_SGIS = $8192;

  // GL_SGIS_multisample
  GL_MULTISAMPLE_SGIS = $809D;
  GL_SAMPLE_ALPHA_TO_MASK_SGIS = $809E;
  GL_SAMPLE_ALPHA_TO_ONE_SGIS = $809F;
  GL_SAMPLE_MASK_SGIS = $80A0;
  GL_1PASS_SGIS = $80A1;
  GL_2PASS_0_SGIS = $80A2;
  GL_2PASS_1_SGIS = $80A3;
  GL_4PASS_0_SGIS = $80A4;
  GL_4PASS_1_SGIS = $80A5;
  GL_4PASS_2_SGIS = $80A6;
  GL_4PASS_3_SGIS = $80A7;
  GL_SAMPLE_BUFFERS_SGIS = $80A8;
  GL_SAMPLES_SGIS = $80A9;
  GL_SAMPLE_MASK_VALUE_SGIS = $80AA;
  GL_SAMPLE_MASK_INVERT_SGIS = $80AB;
  GL_SAMPLE_PATTERN_SGIS = $80AC;

  // GL_SGIS_pixel_texture
  GL_PIXEL_TEXTURE_SGIS = $8353;
  GL_PIXEL_FRAGMENT_RGB_SOURCE_SGIS = $8354;
  GL_PIXEL_FRAGMENT_ALPHA_SOURCE_SGIS = $8355;
  GL_PIXEL_GROUP_COLOR_SGIS = $8356;

  // GL_SGIS_point_line_texgen
  GL_EYE_DISTANCE_TO_POINT_SGIS = $81F0;
  GL_OBJECT_DISTANCE_TO_POINT_SGIS = $81F1;
  GL_EYE_DISTANCE_TO_LINE_SGIS = $81F2;
  GL_OBJECT_DISTANCE_TO_LINE_SGIS = $81F3;
  GL_EYE_POINT_SGIS = $81F4;
  GL_OBJECT_POINT_SGIS = $81F5;
  GL_EYE_LINE_SGIS = $81F6;
  GL_OBJECT_LINE_SGIS = $81F7;

  // GL_SGIS_point_parameters
  GL_POINT_SIZE_MIN_SGIS = $8126;
  GL_POINT_SIZE_MAX_SGIS = $8127;
  GL_POINT_FADE_THRESHOLD_SIZE_SGIS = $8128;
  GL_DISTANCE_ATTENUATION_SGIS = $8129;

  // GL_SGIS_sharpen_texture
  GL_LINEAR_SHARPEN_SGIS = $80AD;
  GL_LINEAR_SHARPEN_ALPHA_SGIS = $80AE;
  GL_LINEAR_SHARPEN_COLOR_SGIS = $80AF;
  GL_SHARPEN_TEXTURE_FUNC_POINTS_SGIS = $80B0;

  // GL_SGIS_texture4D
  GL_PACK_SKIP_VOLUMES_SGIS = $8130;
  GL_PACK_IMAGE_DEPTH_SGIS = $8131;
  GL_UNPACK_SKIP_VOLUMES_SGIS = $8132;
  GL_UNPACK_IMAGE_DEPTH_SGIS = $8133;
  GL_TEXTURE_4D_SGIS = $8134;
  GL_PROXY_TEXTURE_4D_SGIS = $8135;
  GL_TEXTURE_4DSIZE_SGIS = $8136;
  GL_TEXTURE_WRAP_Q_SGIS = $8137;
  GL_MAX_4D_TEXTURE_SIZE_SGIS = $8138;
  GL_TEXTURE_4D_BINDING_SGIS = $814F;

  // GL_SGIS_texture_color_mask
  GL_TEXTURE_COLOR_WRITEMASK_SGIS = $81EF;

  // GL_SGIS_texture_edge_clamp
  GL_CLAMP_TO_EDGE_SGIS = $812F;

  // GL_SGIS_texture_filter4
  GL_FILTER4_SGIS = $8146;
  GL_TEXTURE_FILTER4_SIZE_SGIS = $8147;

  // GL_SGIS_texture_lod
  GL_TEXTURE_MIN_LOD_SGIS = $813A;
  GL_TEXTURE_MAX_LOD_SGIS = $813B;
  GL_TEXTURE_BASE_LEVEL_SGIS = $813C;
  GL_TEXTURE_MAX_LEVEL_SGIS = $813D;

  // GL_SGIS_texture_select
  GL_DUAL_ALPHA4_SGIS = $8110;
  GL_DUAL_ALPHA8_SGIS = $8111;
  GL_DUAL_ALPHA12_SGIS = $8112;
  GL_DUAL_ALPHA16_SGIS = $8113;
  GL_DUAL_LUMINANCE4_SGIS = $8114;
  GL_DUAL_LUMINANCE8_SGIS = $8115;
  GL_DUAL_LUMINANCE12_SGIS = $8116;
  GL_DUAL_LUMINANCE16_SGIS = $8117;
  GL_DUAL_INTENSITY4_SGIS = $8118;
  GL_DUAL_INTENSITY8_SGIS = $8119;
  GL_DUAL_INTENSITY12_SGIS = $811A;
  GL_DUAL_INTENSITY16_SGIS = $811B;
  GL_DUAL_LUMINANCE_ALPHA4_SGIS = $811C;
  GL_DUAL_LUMINANCE_ALPHA8_SGIS = $811D;
  GL_QUAD_ALPHA4_SGIS = $811E;
  GL_QUAD_ALPHA8_SGIS = $811F;
  GL_QUAD_LUMINANCE4_SGIS = $8120;
  GL_QUAD_LUMINANCE8_SGIS = $8121;
  GL_QUAD_INTENSITY4_SGIS = $8122;
  GL_QUAD_INTENSITY8_SGIS = $8123;
  GL_DUAL_TEXTURE_SELECT_SGIS = $8124;
  GL_QUAD_TEXTURE_SELECT_SGIS = $8125;

  // GL_SGIX_async
  GL_ASYNC_MARKER_SGIX = $8329;

  // GL_SGIX_async_histogram
  GL_ASYNC_HISTOGRAM_SGIX = $832C;
  GL_MAX_ASYNC_HISTOGRAM_SGIX = $832D;

  // GL_SGIX_async_pixel
  GL_ASYNC_TEX_IMAGE_SGIX = $835C;
  GL_ASYNC_DRAW_PIXELS_SGIX = $835D;
  GL_ASYNC_READ_PIXELS_SGIX = $835E;
  GL_MAX_ASYNC_TEX_IMAGE_SGIX = $835F;
  GL_MAX_ASYNC_DRAW_PIXELS_SGIX = $8360;
  GL_MAX_ASYNC_READ_PIXELS_SGIX = $8361;

  // GL_SGIX_blend_alpha_minmax
  GL_ALPHA_MIN_SGIX = $8320;
  GL_ALPHA_MAX_SGIX = $8321;

  // GL_SGIX_calligraphic_fragment
  GL_CALLIGRAPHIC_FRAGMENT_SGIX = $8183;

  // GL_SGIX_clipmap
  GL_LINEAR_CLIPMAP_LINEAR_SGIX = $8170;
  GL_TEXTURE_CLIPMAP_CENTER_SGIX = $8171;
  GL_TEXTURE_CLIPMAP_FRAME_SGIX = $8172;
  GL_TEXTURE_CLIPMAP_OFFSET_SGIX = $8173;
  GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX = $8174;
  GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX = $8175;
  GL_TEXTURE_CLIPMAP_DEPTH_SGIX = $8176;
  GL_MAX_CLIPMAP_DEPTH_SGIX = $8177;
  GL_MAX_CLIPMAP_VIRTUAL_DEPTH_SGIX = $8178;
  GL_NEAREST_CLIPMAP_NEAREST_SGIX = $844D;
  GL_NEAREST_CLIPMAP_LINEAR_SGIX = $844E;
  GL_LINEAR_CLIPMAP_NEAREST_SGIX = $844F;

  // GL_SGIX_convolution_accuracy
  GL_CONVOLUTION_HINT_SGIX = $8316;

  // GL_SGIX_depth_texture
  GL_DEPTH_COMPONENT16_SGIX = $81A5;
  GL_DEPTH_COMPONENT24_SGIX = $81A6;
  GL_DEPTH_COMPONENT32_SGIX = $81A7;

  // GL_SGIX_fog_offset
  GL_FOG_OFFSET_SGIX = $8198;
  GL_FOG_OFFSET_VALUE_SGIX = $8199;

  // GL_SGIX_fog_scale
  GL_FOG_SCALE_SGIX = $81FC;
  GL_FOG_SCALE_VALUE_SGIX = $81FD;

  // GL_SGIX_fragment_lighting
  GL_FRAGMENT_LIGHTING_SGIX = $8400;
  GL_FRAGMENT_COLOR_MATERIAL_SGIX = $8401;
  GL_FRAGMENT_COLOR_MATERIAL_FACE_SGIX = $8402;
  GL_FRAGMENT_COLOR_MATERIAL_PARAMETER_SGIX = $8403;
  GL_MAX_FRAGMENT_LIGHTS_SGIX = $8404;
  GL_MAX_ACTIVE_LIGHTS_SGIX = $8405;
  GL_CURRENT_RASTER_NORMAL_SGIX = $8406;
  GL_LIGHT_ENV_MODE_SGIX = $8407;
  GL_FRAGMENT_LIGHT_MODEL_LOCAL_VIEWER_SGIX = $8408;
  GL_FRAGMENT_LIGHT_MODEL_TWO_SIDE_SGIX = $8409;
  GL_FRAGMENT_LIGHT_MODEL_AMBIENT_SGIX = $840A;
  GL_FRAGMENT_LIGHT_MODEL_NORMAL_INTERPOLATION_SGIX = $840B;
  GL_FRAGMENT_LIGHT0_SGIX = $840C;
  GL_FRAGMENT_LIGHT1_SGIX = $840D;
  GL_FRAGMENT_LIGHT2_SGIX = $840E;
  GL_FRAGMENT_LIGHT3_SGIX = $840F;
  GL_FRAGMENT_LIGHT4_SGIX = $8410;
  GL_FRAGMENT_LIGHT5_SGIX = $8411;
  GL_FRAGMENT_LIGHT6_SGIX = $8412;
  GL_FRAGMENT_LIGHT7_SGIX = $8413;

  // GL_SGIX_framezoom
  GL_FRAMEZOOM_SGIX = $818B;
  GL_FRAMEZOOM_FACTOR_SGIX = $818C;
  GL_MAX_FRAMEZOOM_FACTOR_SGIX = $818D;

  // GL_SGIX_impact_pixel_texture
  GL_PIXEL_TEX_GEN_Q_CEILING_SGIX = $8184;
  GL_PIXEL_TEX_GEN_Q_ROUND_SGIX = $8185;
  GL_PIXEL_TEX_GEN_Q_FLOOR_SGIX = $8186;
  GL_PIXEL_TEX_GEN_ALPHA_REPLACE_SGIX = $8187;
  GL_PIXEL_TEX_GEN_ALPHA_NO_REPLACE_SGIX = $8188;
  GL_PIXEL_TEX_GEN_ALPHA_LS_SGIX = $8189;
  GL_PIXEL_TEX_GEN_ALPHA_MS_SGIX = $818A;

  // GL_SGIX_instruments
  GL_INSTRUMENT_BUFFER_POINTER_SGIX = $8180;
  GL_INSTRUMENT_MEASUREMENTS_SGIX = $8181;

  // GL_SGIX_interlace
  GL_INTERLACE_SGIX = $8094;

  // GL_SGIX_ir_instrument1
  GL_IR_INSTRUMENT1_SGIX = $817F;

  // GL_SGIX_list_priority
  GL_LIST_PRIORITY_SGIX = $8182;

  // GL_SGIX_pixel_texture
  GL_PIXEL_TEX_GEN_SGIX = $8139;
  GL_PIXEL_TEX_GEN_MODE_SGIX = $832B;

  // GL_SGIX_pixel_tiles
  GL_PIXEL_TILE_BEST_ALIGNMENT_SGIX = $813E;
  GL_PIXEL_TILE_CACHE_INCREMENT_SGIX = $813F;
  GL_PIXEL_TILE_WIDTH_SGIX = $8140;
  GL_PIXEL_TILE_HEIGHT_SGIX = $8141;
  GL_PIXEL_TILE_GRID_WIDTH_SGIX = $8142;
  GL_PIXEL_TILE_GRID_HEIGHT_SGIX = $8143;
  GL_PIXEL_TILE_GRID_DEPTH_SGIX = $8144;
  GL_PIXEL_TILE_CACHE_SIZE_SGIX = $8145;

  // GL_SGIX_polynomial_ffd
  GL_GEOMETRY_DEFORMATION_SGIX = $8194;
  GL_TEXTURE_DEFORMATION_SGIX = $8195;
  GL_DEFORMATIONS_MASK_SGIX = $8196;
  GL_MAX_DEFORMATION_ORDER_SGIX = $8197;

  // GL_SGIX_reference_plane
  GL_REFERENCE_PLANE_SGIX = $817D;
  GL_REFERENCE_PLANE_EQUATION_SGIX = $817E;

  // GL_SGIX_resample
  GL_PACK_RESAMPLE_SGIX = $842C;
  GL_UNPACK_RESAMPLE_SGIX = $842D;
  GL_RESAMPLE_REPLICATE_SGIX = $842E;
  GL_RESAMPLE_ZERO_FILL_SGIX = $842F;
  GL_RESAMPLE_DECIMATE_SGIX = $8430;

  // GL_SGIX_scalebias_hint
  GL_SCALEBIAS_HINT_SGIX = $8322;

  // GL_SGIX_shadow
  GL_TEXTURE_COMPARE_SGIX = $819A;
  GL_TEXTURE_COMPARE_OPERATOR_SGIX = $819B;
  GL_TEXTURE_LEQUAL_R_SGIX = $819C;
  GL_TEXTURE_GEQUAL_R_SGIX = $819D;

  // GL_SGIX_shadow_ambient
  GL_SHADOW_AMBIENT_SGIX = $80BF;

  // GL_SGIX_sprite
  GL_SPRITE_SGIX = $8148;
  GL_SPRITE_MODE_SGIX = $8149;
  GL_SPRITE_AXIS_SGIX = $814A;
  GL_SPRITE_TRANSLATION_SGIX = $814B;
  GL_SPRITE_AXIAL_SGIX = $814C;
  GL_SPRITE_OBJECT_ALIGNED_SGIX = $814D;
  GL_SPRITE_EYE_ALIGNED_SGIX = $814E;

  // GL_SGIX_subsample
  GL_PACK_SUBSAMPLE_RATE_SGIX = $85A0;
  GL_UNPACK_SUBSAMPLE_RATE_SGIX = $85A1;
  GL_PIXEL_SUBSAMPLE_4444_SGIX = $85A2;
  GL_PIXEL_SUBSAMPLE_2424_SGIX = $85A3;
  GL_PIXEL_SUBSAMPLE_4242_SGIX = $85A4;

  // GL_SGIX_texture_add_env
  GL_TEXTURE_ENV_BIAS_SGIX = $80BE;

  // GL_SGIX_texture_coordinate_clamp
  GL_TEXTURE_MAX_CLAMP_S_SGIX = $8369;
  GL_TEXTURE_MAX_CLAMP_T_SGIX = $836A;
  GL_TEXTURE_MAX_CLAMP_R_SGIX = $836B;

  // GL_SGIX_texture_lod_bias
  GL_TEXTURE_LOD_BIAS_S_SGIX = $818E;
  GL_TEXTURE_LOD_BIAS_T_SGIX = $818F;
  GL_TEXTURE_LOD_BIAS_R_SGIX = $8190;

  // GL_SGIX_texture_multi_buffer
  GL_TEXTURE_MULTI_BUFFER_HINT_SGIX = $812E;

  // GL_SGIX_texture_scale_bias
  GL_POST_TEXTURE_FILTER_BIAS_SGIX = $8179;
  GL_POST_TEXTURE_FILTER_SCALE_SGIX = $817A;
  GL_POST_TEXTURE_FILTER_BIAS_RANGE_SGIX = $817B;
  GL_POST_TEXTURE_FILTER_SCALE_RANGE_SGIX = $817C;

  // GL_SGIX_vertex_preclip
  GL_VERTEX_PRECLIP_SGIX = $83EE;
  GL_VERTEX_PRECLIP_HINT_SGIX = $83EF;

  // GL_SGIX_ycrcb
  GL_YCRCB_422_SGIX = $81BB;
  GL_YCRCB_444_SGIX = $81BC;

  // GL_SGIX_ycrcba
  GL_YCRCB_SGIX = $8318;
  GL_YCRCBA_SGIX = $8319;

  // GL_SGI_color_matrix
  GL_COLOR_MATRIX_SGI = $80B1;
  GL_COLOR_MATRIX_STACK_DEPTH_SGI = $80B2;
  GL_MAX_COLOR_MATRIX_STACK_DEPTH_SGI = $80B3;
  GL_POST_COLOR_MATRIX_RED_SCALE_SGI = $80B4;
  GL_POST_COLOR_MATRIX_GREEN_SCALE_SGI = $80B5;
  GL_POST_COLOR_MATRIX_BLUE_SCALE_SGI = $80B6;
  GL_POST_COLOR_MATRIX_ALPHA_SCALE_SGI = $80B7;
  GL_POST_COLOR_MATRIX_RED_BIAS_SGI = $80B8;
  GL_POST_COLOR_MATRIX_GREEN_BIAS_SGI = $80B9;
  GL_POST_COLOR_MATRIX_BLUE_BIAS_SGI = $80BA;
  GL_POST_COLOR_MATRIX_ALPHA_BIAS_SGI = $80BB;

  // GL_SGI_color_table
  GL_COLOR_TABLE_SGI = $80D0;
  GL_POST_CONVOLUTION_COLOR_TABLE_SGI = $80D1;
  GL_POST_COLOR_MATRIX_COLOR_TABLE_SGI = $80D2;
  GL_PROXY_COLOR_TABLE_SGI = $80D3;
  GL_PROXY_POST_CONVOLUTION_COLOR_TABLE_SGI = $80D4;
  GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE_SGI = $80D5;
  GL_COLOR_TABLE_SCALE_SGI = $80D6;
  GL_COLOR_TABLE_BIAS_SGI = $80D7;
  GL_COLOR_TABLE_FORMAT_SGI = $80D8;
  GL_COLOR_TABLE_WIDTH_SGI = $80D9;
  GL_COLOR_TABLE_RED_SIZE_SGI = $80DA;
  GL_COLOR_TABLE_GREEN_SIZE_SGI = $80DB;
  GL_COLOR_TABLE_BLUE_SIZE_SGI = $80DC;
  GL_COLOR_TABLE_ALPHA_SIZE_SGI = $80DD;
  GL_COLOR_TABLE_LUMINANCE_SIZE_SGI = $80DE;
  GL_COLOR_TABLE_INTENSITY_SIZE_SGI = $80DF;

  // GL_SGI_depth_pass_instrument
  GL_DEPTH_PASS_INSTRUMENT_SGIX = $8310;
  GL_DEPTH_PASS_INSTRUMENT_COUNTERS_SGIX = $8311;
  GL_DEPTH_PASS_INSTRUMENT_MAX_SGIX = $8312;

  // GL_SGI_texture_color_table
  GL_TEXTURE_COLOR_TABLE_SGI = $80BC;
  GL_PROXY_TEXTURE_COLOR_TABLE_SGI = $80BD;

  // GL_SUNX_constant_data
  GL_UNPACK_CONSTANT_DATA_SUNX = $81D5;
  GL_TEXTURE_CONSTANT_DATA_SUNX = $81D6;

  // GL_SUN_convolution_border_modes
  GL_WRAP_BORDER_SUN = $81D4;

  // GL_SUN_global_alpha
  GL_GLOBAL_ALPHA_SUN = $81D9;
  GL_GLOBAL_ALPHA_FACTOR_SUN = $81DA;

  // GL_SUN_mesh_array
  GL_QUAD_MESH_SUN = $8614;
  GL_TRIANGLE_MESH_SUN = $8615;

  // GL_SUN_slice_accum
  GL_SLICE_ACCUM_SUN = $85CC;

  // GL_SUN_triangle_list
  GL_RESTART_SUN = $0001;
  GL_REPLACE_MIDDLE_SUN = $0002;
  GL_REPLACE_OLDEST_SUN = $0003;
  GL_TRIANGLE_LIST_SUN = $81D7;
  GL_REPLACEMENT_CODE_SUN = $81D8;
  GL_REPLACEMENT_CODE_ARRAY_SUN = $85C0;
  GL_REPLACEMENT_CODE_ARRAY_TYPE_SUN = $85C1;
  GL_REPLACEMENT_CODE_ARRAY_STRIDE_SUN = $85C2;
  GL_REPLACEMENT_CODE_ARRAY_POINTER_SUN = $85C3;
  GL_R1UI_V3F_SUN = $85C4;
  GL_R1UI_C4UB_V3F_SUN = $85C5;
  GL_R1UI_C3F_V3F_SUN = $85C6;
  GL_R1UI_N3F_V3F_SUN = $85C7;
  GL_R1UI_C4F_N3F_V3F_SUN = $85C8;
  GL_R1UI_T2F_V3F_SUN = $85C9;
  GL_R1UI_T2F_N3F_V3F_SUN = $85CA;
  GL_R1UI_T2F_C4F_N3F_V3F_SUN = $85CB;

  // GL_WIN_phong_shading
  GL_PHONG_WIN = $80EA;
  GL_PHONG_HINT_WIN = $80EB;

  // GL_WIN_specular_fog
  GL_FOG_SPECULAR_TEXTURE_WIN = $80EC;

   // GL_ARB_vertex_shader
  GL_VERTEX_SHADER_ARB = $8B31;
  GL_MAX_VERTEX_UNIFORM_COMPONENTS_ARB = $8B4A;
  GL_MAX_VARYING_FLOATS_ARB = $8B4B;
  GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS_ARB = $8B4C;
  GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS_ARB = $8B4D;
  GL_OBJECT_ACTIVE_ATTRIBUTES_ARB = $8B89;
  GL_OBJECT_ACTIVE_ATTRIBUTE_MAX_LENGTH_ARB = $8B8A;

  // GL_ARB_fragment_shader
  GL_FRAGMENT_SHADER_ARB = $8B30;
  GL_MAX_FRAGMENT_UNIFORM_COMPONENTS_ARB = $8B49; // 1.4
  GL_FRAGMENT_SHADER_DERIVATIVE_HINT_ARB = $8B8B; // 1.4

  // GL_ARB_occlusion_query
  GL_SAMPLES_PASSED_ARB = $8914;
  GL_QUERY_COUNTER_BITS_ARB = $8864;
  GL_CURRENT_QUERY_ARB = $8865;
  GL_QUERY_RESULT_ARB = $8866;
  GL_QUERY_RESULT_AVAILABLE_ARB = $8867;

  // GL_ARB_point_sprite
  GL_POINT_SPRITE_ARB = $8861;
  GL_COORD_REPLACE_ARB = $8862;

  // GL_ARB_shading_language_100
  GL_SHADING_LANGUAGE_VERSION_ARB = $8B8C; // 1.4

  // GL_ARB_shader_objects
  GL_PROGRAM_OBJECT_ARB = $8B40;

  GL_OBJECT_TYPE_ARB = $8B4E;
  GL_OBJECT_SUBTYPE_ARB = $8B4F;
  GL_OBJECT_DELETE_STATUS_ARB = $8B80;
  GL_OBJECT_COMPILE_STATUS_ARB = $8B81;
  GL_OBJECT_LINK_STATUS_ARB = $8B82;
  GL_OBJECT_VALIDATE_STATUS_ARB = $8B83;
  GL_OBJECT_INFO_LOG_LENGTH_ARB = $8B84;
  GL_OBJECT_ATTACHED_OBJECTS_ARB = $8B85;
  GL_OBJECT_ACTIVE_UNIFORMS_ARB = $8B86;
  GL_OBJECT_ACTIVE_UNIFORM_MAX_LENGTH_ARB = $8B87;
  GL_OBJECT_SHADER_SOURCE_LENGTH_ARB = $8B88;

  GL_SHADER_OBJECT_ARB = $8B48;

  GL_FLOAT_VEC2_ARB = $8B50;
  GL_FLOAT_VEC3_ARB = $8B51;
  GL_FLOAT_VEC4_ARB = $8B52;
  GL_INT_VEC2_ARB = $8B53;
  GL_INT_VEC3_ARB = $8B54;
  GL_INT_VEC4_ARB = $8B55;
  GL_BOOL_ARB = $8B56;
  GL_BOOL_VEC2_ARB = $8B57;
  GL_BOOL_VEC3_ARB = $8B58;
  GL_BOOL_VEC4_ARB = $8B59;
  GL_FLOAT_MAT2_ARB = $8B5A;
  GL_FLOAT_MAT3_ARB = $8B5B;
  GL_FLOAT_MAT4_ARB = $8B5C;

  // Update for glsl-specification 1.10
  GL_SAMPLER_1D_ARB = $8B5D; // 1.4
  GL_SAMPLER_2D_ARB = $8B5E; // 1.4
  GL_SAMPLER_3D_ARB = $8B5F; // 1.4
  GL_SAMPLER_CUBE_ARB = $8B60; // 1.4
  GL_SAMPLER_1D_SHADOW_ARB = $8B61; // 1.4
  GL_SAMPLER_2D_SHADOW_ARB = $8B62; // 1.4
  GL_SAMPLER_2D_RECT_ARB = $8B63; // 1.4
  GL_SAMPLER_2D_RECT_SHADOW_ARB = $8B64; // 1.4

  // WGL_3DFX_multisample
  WGL_SAMPLE_BUFFERS_3DFX = $2060;
  WGL_SAMPLES_3DFX = $2061;

  // WGL_ARB_buffer_region
  WGL_FRONT_COLOR_BUFFER_BIT_ARB = $00000001;
  WGL_BACK_COLOR_BUFFER_BIT_ARB = $00000002;
  WGL_DEPTH_BUFFER_BIT_ARB = $00000004;
  WGL_STENCIL_BUFFER_BIT_ARB = $00000008;

  // WGL_ARB_make_current_read
  ERROR_INVALID_PIXEL_TYPE_ARB = $2043;
  ERROR_INCOMPATIBLE_DEVICE_CONTEXTS_ARB = $2054;

  // WGL_ARB_multisample
  WGL_SAMPLE_BUFFERS_ARB = $2041;
  WGL_SAMPLES_ARB = $2042;

  // WGL_ARB_pbuffer
  WGL_DRAW_TO_PBUFFER_ARB = $202D;
  WGL_MAX_PBUFFER_PIXELS_ARB = $202E;
  WGL_MAX_PBUFFER_WIDTH_ARB = $202F;
  WGL_MAX_PBUFFER_HEIGHT_ARB = $2030;
  WGL_PBUFFER_LARGEST_ARB = $2033;
  WGL_PBUFFER_WIDTH_ARB = $2034;
  WGL_PBUFFER_HEIGHT_ARB = $2035;
  WGL_PBUFFER_LOST_ARB = $2036;

  // WGL_ARB_pixel_format
  WGL_NUMBER_PIXEL_FORMATS_ARB = $2000;
  WGL_DRAW_TO_WINDOW_ARB = $2001;
  WGL_DRAW_TO_BITMAP_ARB = $2002;
  WGL_ACCELERATION_ARB = $2003;
  WGL_NEED_PALETTE_ARB = $2004;
  WGL_NEED_SYSTEM_PALETTE_ARB = $2005;
  WGL_SWAP_LAYER_BUFFERS_ARB = $2006;
  WGL_SWAP_METHOD_ARB = $2007;
  WGL_NUMBER_OVERLAYS_ARB = $2008;
  WGL_NUMBER_UNDERLAYS_ARB = $2009;
  WGL_TRANSPARENT_ARB = $200A;
  WGL_TRANSPARENT_RED_VALUE_ARB = $2037;
  WGL_TRANSPARENT_GREEN_VALUE_ARB = $2038;
  WGL_TRANSPARENT_BLUE_VALUE_ARB = $2039;
  WGL_TRANSPARENT_ALPHA_VALUE_ARB = $203A;
  WGL_TRANSPARENT_INDEX_VALUE_ARB = $203B;
  WGL_SHARE_DEPTH_ARB = $200C;
  WGL_SHARE_STENCIL_ARB = $200D;
  WGL_SHARE_ACCUM_ARB = $200E;
  WGL_SUPPORT_GDI_ARB = $200F;
  WGL_SUPPORT_OPENGL_ARB = $2010;
  WGL_DOUBLE_BUFFER_ARB = $2011;
  WGL_STEREO_ARB = $2012;
  WGL_PIXEL_TYPE_ARB = $2013;
  WGL_COLOR_BITS_ARB = $2014;
  WGL_RED_BITS_ARB = $2015;
  WGL_RED_SHIFT_ARB = $2016;
  WGL_GREEN_BITS_ARB = $2017;
  WGL_GREEN_SHIFT_ARB = $2018;
  WGL_BLUE_BITS_ARB = $2019;
  WGL_BLUE_SHIFT_ARB = $201A;
  WGL_ALPHA_BITS_ARB = $201B;
  WGL_ALPHA_SHIFT_ARB = $201C;
  WGL_ACCUM_BITS_ARB = $201D;
  WGL_ACCUM_RED_BITS_ARB = $201E;
  WGL_ACCUM_GREEN_BITS_ARB = $201F;
  WGL_ACCUM_BLUE_BITS_ARB = $2020;
  WGL_ACCUM_ALPHA_BITS_ARB = $2021;
  WGL_DEPTH_BITS_ARB = $2022;
  WGL_STENCIL_BITS_ARB = $2023;
  WGL_AUX_BUFFERS_ARB = $2024;
  WGL_NO_ACCELERATION_ARB = $2025;
  WGL_GENERIC_ACCELERATION_ARB = $2026;
  WGL_FULL_ACCELERATION_ARB = $2027;
  WGL_SWAP_EXCHANGE_ARB = $2028;
  WGL_SWAP_COPY_ARB = $2029;
  WGL_SWAP_UNDEFINED_ARB = $202A;
  WGL_TYPE_RGBA_ARB = $202B;
  WGL_TYPE_COLORINDEX_ARB = $202C;

  // WGL_ARB_pixel_format_float
  WGL_RGBA_FLOAT_MODE_ARB = $8820;
  WGL_CLAMP_VERTEX_COLOR_ARB = $891A;
  WGL_CLAMP_FRAGMENT_COLOR_ARB = $891B;
  WGL_CLAMP_READ_COLOR_ARB = $891C;
  WGL_FIXED_ONLY_ARB = $891D;

  // WGL_ARB_render_texture
  WGL_BIND_TO_TEXTURE_RGB_ARB = $2070;
  WGL_BIND_TO_TEXTURE_RGBA_ARB = $2071;
  WGL_TEXTURE_FORMAT_ARB = $2072;
  WGL_TEXTURE_TARGET_ARB = $2073;
  WGL_MIPMAP_TEXTURE_ARB = $2074;
  WGL_TEXTURE_RGB_ARB = $2075;
  WGL_TEXTURE_RGBA_ARB = $2076;
  WGL_NO_TEXTURE_ARB = $2077;
  WGL_TEXTURE_CUBE_MAP_ARB = $2078;
  WGL_TEXTURE_1D_ARB = $2079;
  WGL_TEXTURE_2D_ARB = $207A;
  WGL_MIPMAP_LEVEL_ARB = $207B;
  WGL_CUBE_MAP_FACE_ARB = $207C;
  WGL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB = $207D;
  WGL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB = $207E;
  WGL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB = $207F;
  WGL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB = $2080;
  WGL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB = $2081;
  WGL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB = $2082;
  WGL_FRONT_LEFT_ARB = $2083;
  WGL_FRONT_RIGHT_ARB = $2084;
  WGL_BACK_LEFT_ARB = $2085;
  WGL_BACK_RIGHT_ARB = $2086;
  WGL_AUX0_ARB = $2087;
  WGL_AUX1_ARB = $2088;
  WGL_AUX2_ARB = $2089;
  WGL_AUX3_ARB = $208A;
  WGL_AUX4_ARB = $208B;
  WGL_AUX5_ARB = $208C;
  WGL_AUX6_ARB = $208D;
  WGL_AUX7_ARB = $208E;
  WGL_AUX8_ARB = $208F;
  WGL_AUX9_ARB = $2090;

  // WGL_ARB_create_context
  WGL_CONTEXT_DEBUG_BIT_ARB = $0001;
  WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB = $0002;
  WGL_CONTEXT_MAJOR_VERSION_ARB = $2091;
  WGL_CONTEXT_MINOR_VERSION_ARB = $2092;
  WGL_CONTEXT_LAYER_PLANE_ARB = $2093;
  WGL_CONTEXT_FLAGS_ARB = $2094;
  ERROR_INVALID_VERSION_ARB = $2095;
        
  // WGL_ATI_pixel_format_float
  WGL_TYPE_RGBA_FLOAT_ATI = $21A0;
  GL_TYPE_RGBA_FLOAT_ATI = $8820;
  GL_COLOR_CLEAR_UNCLAMPED_VALUE_ATI = $8835;

  // WGL_EXT_depth_float
  WGL_DEPTH_FLOAT_EXT = $2040;

  // WGL_EXT_make_current_read
  ERROR_INVALID_PIXEL_TYPE_EXT = $2043;

  // WGL_EXT_multisample
  WGL_SAMPLE_BUFFERS_EXT = $2041;
  WGL_SAMPLES_EXT = $2042;

  // WGL_EXT_pbuffer
  WGL_DRAW_TO_PBUFFER_EXT = $202D;
  WGL_MAX_PBUFFER_PIXELS_EXT = $202E;
  WGL_MAX_PBUFFER_WIDTH_EXT = $202F;
  WGL_MAX_PBUFFER_HEIGHT_EXT = $2030;
  WGL_OPTIMAL_PBUFFER_WIDTH_EXT = $2031;
  WGL_OPTIMAL_PBUFFER_HEIGHT_EXT = $2032;
  WGL_PBUFFER_LARGEST_EXT = $2033;
  WGL_PBUFFER_WIDTH_EXT = $2034;
  WGL_PBUFFER_HEIGHT_EXT = $2035;

  // WGL_EXT_pixel_format
  WGL_NUMBER_PIXEL_FORMATS_EXT = $2000;
  WGL_DRAW_TO_WINDOW_EXT = $2001;
  WGL_DRAW_TO_BITMAP_EXT = $2002;
  WGL_ACCELERATION_EXT = $2003;
  WGL_NEED_PALETTE_EXT = $2004;
  WGL_NEED_SYSTEM_PALETTE_EXT = $2005;
  WGL_SWAP_LAYER_BUFFERS_EXT = $2006;
  WGL_SWAP_METHOD_EXT = $2007;
  WGL_NUMBER_OVERLAYS_EXT = $2008;
  WGL_NUMBER_UNDERLAYS_EXT = $2009;
  WGL_TRANSPARENT_EXT = $200A;
  WGL_TRANSPARENT_VALUE_EXT = $200B;
  WGL_SHARE_DEPTH_EXT = $200C;
  WGL_SHARE_STENCIL_EXT = $200D;
  WGL_SHARE_ACCUM_EXT = $200E;
  WGL_SUPPORT_GDI_EXT = $200F;
  WGL_SUPPORT_OPENGL_EXT = $2010;
  WGL_DOUBLE_BUFFER_EXT = $2011;
  WGL_STEREO_EXT = $2012;
  WGL_PIXEL_TYPE_EXT = $2013;
  WGL_COLOR_BITS_EXT = $2014;
  WGL_RED_BITS_EXT = $2015;
  WGL_RED_SHIFT_EXT = $2016;
  WGL_GREEN_BITS_EXT = $2017;
  WGL_GREEN_SHIFT_EXT = $2018;
  WGL_BLUE_BITS_EXT = $2019;
  WGL_BLUE_SHIFT_EXT = $201A;
  WGL_ALPHA_BITS_EXT = $201B;
  WGL_ALPHA_SHIFT_EXT = $201C;
  WGL_ACCUM_BITS_EXT = $201D;
  WGL_ACCUM_RED_BITS_EXT = $201E;
  WGL_ACCUM_GREEN_BITS_EXT = $201F;
  WGL_ACCUM_BLUE_BITS_EXT = $2020;
  WGL_ACCUM_ALPHA_BITS_EXT = $2021;
  WGL_DEPTH_BITS_EXT = $2022;
  WGL_STENCIL_BITS_EXT = $2023;
  WGL_AUX_BUFFERS_EXT = $2024;
  WGL_NO_ACCELERATION_EXT = $2025;
  WGL_GENERIC_ACCELERATION_EXT = $2026;
  WGL_FULL_ACCELERATION_EXT = $2027;
  WGL_SWAP_EXCHANGE_EXT = $2028;
  WGL_SWAP_COPY_EXT = $2029;
  WGL_SWAP_UNDEFINED_EXT = $202A;
  WGL_TYPE_RGBA_EXT = $202B;
  WGL_TYPE_COLORINDEX_EXT = $202C;

  // WGL_I3D_digital_video_control
  WGL_DIGITAL_VIDEO_CURSOR_ALPHA_FRAMEBUFFER_I3D = $2050;
  WGL_DIGITAL_VIDEO_CURSOR_ALPHA_VALUE_I3D = $2051;
  WGL_DIGITAL_VIDEO_CURSOR_INCLUDED_I3D = $2052;
  WGL_DIGITAL_VIDEO_GAMMA_CORRECTED_I3D = $2053;

  // WGL_I3D_gamma
  WGL_GAMMA_TABLE_SIZE_I3D = $204E;
  WGL_GAMMA_EXCLUDE_DESKTOP_I3D = $204F;

  // WGL_I3D_genlock
  WGL_GENLOCK_SOURCE_MULTIVIEW_I3D = $2044;
  WGL_GENLOCK_SOURCE_EXTENAL_SYNC_I3D = $2045;
  WGL_GENLOCK_SOURCE_EXTENAL_FIELD_I3D = $2046;
  WGL_GENLOCK_SOURCE_EXTENAL_TTL_I3D = $2047;
  WGL_GENLOCK_SOURCE_DIGITAL_SYNC_I3D = $2048;
  WGL_GENLOCK_SOURCE_DIGITAL_FIELD_I3D = $2049;
  WGL_GENLOCK_SOURCE_EDGE_FALLING_I3D = $204A;
  WGL_GENLOCK_SOURCE_EDGE_RISING_I3D = $204B;
  WGL_GENLOCK_SOURCE_EDGE_BOTH_I3D = $204C;

  // WGL_I3D_image_buffer
  WGL_IMAGE_BUFFER_MIN_ACCESS_I3D = $00000001;
  WGL_IMAGE_BUFFER_LOCK_I3D = $00000002;

  // WGL_NV_float_buffer
  WGL_FLOAT_COMPONENTS_NV = $20B0;
  WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_R_NV = $20B1;
  WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_RG_NV = $20B2;
  WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_RGB_NV = $20B3;
  WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_RGBA_NV = $20B4;
  WGL_TEXTURE_FLOAT_R_NV = $20B5;
  WGL_TEXTURE_FLOAT_RG_NV = $20B6;
  WGL_TEXTURE_FLOAT_RGB_NV = $20B7;
  WGL_TEXTURE_FLOAT_RGBA_NV = $20B8;

  // WGL_NV_render_depth_texture
  WGL_BIND_TO_TEXTURE_DEPTH_NV = $20A3;
  WGL_BIND_TO_TEXTURE_RECTANGLE_DEPTH_NV = $20A4;
  WGL_DEPTH_TEXTURE_FORMAT_NV = $20A5;
  WGL_TEXTURE_DEPTH_COMPONENT_NV = $20A6;
  WGL_DEPTH_COMPONENT_NV = $20A7;

  // WGL_NV_render_texture_rectangle
  WGL_BIND_TO_TEXTURE_RECTANGLE_RGB_NV = $20A0;
  WGL_BIND_TO_TEXTURE_RECTANGLE_RGBA_NV = $20A1;
  WGL_TEXTURE_RECTANGLE_NV = $20A2;

  // WGL_NV_present_video
  WGL_NUM_VIDEO_SLOTS_NV = $20F0;
  
  // WGL_NV_video_out
  WGL_BIND_TO_VIDEO_RGB_NV = $20C0;
  WGL_BIND_TO_VIDEO_RGBA_NV = $20C1;
  WGL_BIND_TO_VIDEO_RGB_AND_DEPTH_NV = $20C2;
  WGL_VIDEO_OUT_COLOR_NV = $20C3;
  WGL_VIDEO_OUT_ALPHA_NV = $20C4;
  WGL_VIDEO_OUT_DEPTH_NV = $20C5;
  WGL_VIDEO_OUT_COLOR_AND_ALPHA_NV = $20C6;
  WGL_VIDEO_OUT_COLOR_AND_DEPTH_NV = $20C7;
  WGL_VIDEO_OUT_FRAME = $20C8;
  WGL_VIDEO_OUT_FIELD_1 = $20C9;
  WGL_VIDEO_OUT_FIELD_2 = $20CA;
  WGL_VIDEO_OUT_STACKED_FIELDS_1_2 = $20CB;
  WGL_VIDEO_OUT_STACKED_FIELDS_2_1 = $20CC;

  // WGL_NV_gpu_affinity
  WGL_ERROR_INCOMPATIBLE_AFFINITY_MASKS_NV = $20D0;
  WGL_ERROR_MISSING_AFFINITY_MASK_NV = $20D1;

  // WIN_draw_range_elements
  GL_MAX_ELEMENTS_VERTICES_WIN = $80E8;
  GL_MAX_ELEMENTS_INDICES_WIN = $80E9;

  // GLU
  GLU_INVALID_ENUM = 100900;
  GLU_INVALID_VALUE = 100901;
  GLU_OUT_OF_MEMORY = 100902;
  GLU_INCOMPATIBLE_GL_VERSION = 100903;
  GLU_VERSION = 100800;
  GLU_EXTENSIONS = 100801;
  GLU_TRUE = GL_TRUE;
  GLU_FALSE = GL_FALSE;
  GLU_SMOOTH = 100000;
  GLU_FLAT = 100001;
  GLU_NONE = 100002;
  GLU_POINT = 100010;
  GLU_LINE = 100011;
  GLU_FILL = 100012;
  GLU_SILHOUETTE = 100013;
  GLU_OUTSIDE = 100020;
  GLU_INSIDE = 100021;
  GLU_TESS_MAX_COORD = 1.0E150;
  GLU_TESS_WINDING_RULE = 100140;
  GLU_TESS_BOUNDARY_ONLY = 100141;
  GLU_TESS_TOLERANCE = 100142;
  GLU_TESS_WINDING_ODD = 100130;
  GLU_TESS_WINDING_NONZERO = 100131;
  GLU_TESS_WINDING_POSITIVE = 100132;
  GLU_TESS_WINDING_NEGATIVE = 100133;
  GLU_TESS_WINDING_ABS_GEQ_TWO = 100134;
  GLU_TESS_BEGIN = 100100; // TGLUTessBeginProc
  GLU_TESS_VERTEX = 100101; // TGLUTessVertexProc
  GLU_TESS_END = 100102; // TGLUTessEndProc
  GLU_TESS_ERROR = 100103; // TGLUTessErrorProc
  GLU_TESS_EDGE_FLAG = 100104; // TGLUTessEdgeFlagProc
  GLU_TESS_COMBINE = 100105; // TGLUTessCombineProc
  GLU_TESS_BEGIN_DATA = 100106; // TGLUTessBeginDataProc
  GLU_TESS_VERTEX_DATA = 100107; // TGLUTessVertexDataProc
  GLU_TESS_END_DATA = 100108; // TGLUTessEndDataProc
  GLU_TESS_ERROR_DATA = 100109; // TGLUTessErrorDataProc
  GLU_TESS_EDGE_FLAG_DATA = 100110; // TGLUTessEdgeFlagDataProc
  GLU_TESS_COMBINE_DATA = 100111; // TGLUTessCombineDataProc
  GLU_TESS_ERROR1 = 100151;
  GLU_TESS_ERROR2 = 100152;
  GLU_TESS_ERROR3 = 100153;
  GLU_TESS_ERROR4 = 100154;
  GLU_TESS_ERROR5 = 100155;
  GLU_TESS_ERROR6 = 100156;
  GLU_TESS_ERROR7 = 100157;
  GLU_TESS_ERROR8 = 100158;
  GLU_TESS_MISSING_BEGIN_POLYGON = GLU_TESS_ERROR1;
  GLU_TESS_MISSING_BEGIN_CONTOUR = GLU_TESS_ERROR2;
  GLU_TESS_MISSING_END_POLYGON = GLU_TESS_ERROR3;
  GLU_TESS_MISSING_END_CONTOUR = GLU_TESS_ERROR4;
  GLU_TESS_COORD_TOO_LARGE = GLU_TESS_ERROR5;
  GLU_TESS_NEED_COMBINE_CALLBACK = GLU_TESS_ERROR6;
  GLU_AUTO_LOAD_MATRIX = 100200;
  GLU_CULLING = 100201;
  GLU_SAMPLING_TOLERANCE = 100203;
  GLU_DISPLAY_MODE = 100204;
  GLU_PARAMETRIC_TOLERANCE = 100202;
  GLU_SAMPLING_METHOD = 100205;
  GLU_U_STEP = 100206;
  GLU_V_STEP = 100207;
  GLU_PATH_LENGTH = 100215;
  GLU_PARAMETRIC_ERROR = 100216;
  GLU_DOMAIN_DISTANCE = 100217;
  GLU_MAP1_TRIM_2 = 100210;
  GLU_MAP1_TRIM_3 = 100211;
  GLU_OUTLINE_POLYGON = 100240;
  GLU_OUTLINE_PATCH = 100241;
  GLU_NURBS_ERROR1 = 100251;
  GLU_NURBS_ERROR2 = 100252;
  GLU_NURBS_ERROR3 = 100253;
  GLU_NURBS_ERROR4 = 100254;
  GLU_NURBS_ERROR5 = 100255;
  GLU_NURBS_ERROR6 = 100256;
  GLU_NURBS_ERROR7 = 100257;
  GLU_NURBS_ERROR8 = 100258;
  GLU_NURBS_ERROR9 = 100259;
  GLU_NURBS_ERROR10 = 100260;
  GLU_NURBS_ERROR11 = 100261;
  GLU_NURBS_ERROR12 = 100262;
  GLU_NURBS_ERROR13 = 100263;
  GLU_NURBS_ERROR14 = 100264;
  GLU_NURBS_ERROR15 = 100265;
  GLU_NURBS_ERROR16 = 100266;
  GLU_NURBS_ERROR17 = 100267;
  GLU_NURBS_ERROR18 = 100268;
  GLU_NURBS_ERROR19 = 100269;
  GLU_NURBS_ERROR20 = 100270;
  GLU_NURBS_ERROR21 = 100271;
  GLU_NURBS_ERROR22 = 100272;
  GLU_NURBS_ERROR23 = 100273;
  GLU_NURBS_ERROR24 = 100274;
  GLU_NURBS_ERROR25 = 100275;
  GLU_NURBS_ERROR26 = 100276;
  GLU_NURBS_ERROR27 = 100277;
  GLU_NURBS_ERROR28 = 100278;
  GLU_NURBS_ERROR29 = 100279;
  GLU_NURBS_ERROR30 = 100280;
  GLU_NURBS_ERROR31 = 100281;
  GLU_NURBS_ERROR32 = 100282;
  GLU_NURBS_ERROR33 = 100283;
  GLU_NURBS_ERROR34 = 100284;
  GLU_NURBS_ERROR35 = 100285;
  GLU_NURBS_ERROR36 = 100286;
  GLU_NURBS_ERROR37 = 100287;
  GLU_CW = 100120;
  GLU_CCW = 100121;
  GLU_INTERIOR = 100122;
  GLU_EXTERIOR = 100123;
  GLU_UNKNOWN = 100124;
  GLU_BEGIN = GLU_TESS_BEGIN;
  GLU_VERTEX = GLU_TESS_VERTEX;
  GLU_END = GLU_TESS_END;
  GLU_ERROR = GLU_TESS_ERROR;
  GLU_EDGE_FLAG = GLU_TESS_EDGE_FLAG;

type
  // GL_VERSION_1_1
  TglAccum = procedure(op: TGLenum; value: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglAlphaFunc = procedure(func: TGLenum; ref: TGLclampf); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglAreTexturesResident = function(n: TGLsizei; const textures: PGLuint; residences: PGLboolean): TGLboolean; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglArrayElement = procedure(i: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBegin = procedure(mode: TGLenum); {$IFNDEF CLR}{$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}{$ENDIF}
  TglBindTexture = procedure(target: TGLenum; texture: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBitmap = procedure(width: TGLsizei; height: TGLsizei; xorig: TGLfloat; yorig: TGLfloat; xmove: TGLfloat; ymove: TGLfloat; const bitmap: PGLubyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBlendFunc = procedure(sfactor: TGLenum; dfactor: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCallList = procedure(list: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCallLists = procedure(n: TGLsizei; _type: TGLenum; const lists: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglClear = procedure(mask: TGLbitfield); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglClearAccum = procedure(red: TGLfloat; green: TGLfloat; blue: TGLfloat; alpha: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglClearColor = procedure(red: TGLclampf; green: TGLclampf; blue: TGLclampf; alpha: TGLclampf); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglClearDepth = procedure(depth: TGLclampd); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglClearIndex = procedure(c: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglClearStencil = procedure(s: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglClipPlane = procedure(plane: TGLenum; const equation: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor3b = procedure(red: TGLbyte; green: TGLbyte; blue: TGLbyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor3bv = procedure(const v: PGLbyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor3d = procedure(red: TGLdouble; green: TGLdouble; blue: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor3dv = procedure(const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor3f = procedure(red: TGLfloat; green: TGLfloat; blue: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor3fv = procedure(const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor3i = procedure(red: TGLint; green: TGLint; blue: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor3iv = procedure(const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor3s = procedure(red: TGLshort; green: TGLshort; blue: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor3sv = procedure(const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor3ub = procedure(red: TGLubyte; green: TGLubyte; blue: TGLubyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor3ubv = procedure(const v: PGLubyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor3ui = procedure(red: TGLuint; green: TGLuint; blue: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor3uiv = procedure(const v: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor3us = procedure(red: TGLushort; green: TGLushort; blue: TGLushort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor3usv = procedure(const v: PGLushort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor4b = procedure(red: TGLbyte; green: TGLbyte; blue: TGLbyte; alpha: TGLbyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor4bv = procedure(const v: PGLbyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor4d = procedure(red: TGLdouble; green: TGLdouble; blue: TGLdouble; alpha: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor4dv = procedure(const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor4f = procedure(red: TGLfloat; green: TGLfloat; blue: TGLfloat; alpha: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor4fv = procedure(const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor4i = procedure(red: TGLint; green: TGLint; blue: TGLint; alpha: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor4iv = procedure(const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor4s = procedure(red: TGLshort; green: TGLshort; blue: TGLshort; alpha: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor4sv = procedure(const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor4ub = procedure(red: TGLubyte; green: TGLubyte; blue: TGLubyte; alpha: TGLubyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor4ubv = procedure(const v: PGLubyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor4ui = procedure(red: TGLuint; green: TGLuint; blue: TGLuint; alpha: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor4uiv = procedure(const v: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor4us = procedure(red: TGLushort; green: TGLushort; blue: TGLushort; alpha: TGLushort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor4usv = procedure(const v: PGLushort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColorMask = procedure(red: TGLboolean; green: TGLboolean; blue: TGLboolean; alpha: TGLboolean); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColorMaterial = procedure(face: TGLenum; mode: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColorPointer = procedure(size: TGLint; _type: TGLenum; stride: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCopyPixels = procedure(x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei; _type: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCopyTexImage1D = procedure(target: TGLenum; level: TGLint; internalFormat: TGLenum; x: TGLint; y: TGLint; width: TGLsizei; border: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCopyTexImage2D = procedure(target: TGLenum; level: TGLint; internalFormat: TGLenum; x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei; border: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCopyTexSubImage1D = procedure(target: TGLenum; level: TGLint; xoffset: TGLint; x: TGLint; y: TGLint; width: TGLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCopyTexSubImage2D = procedure(target: TGLenum; level: TGLint; xoffset: TGLint; yoffset: TGLint; x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCullFace = procedure(mode: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDeleteLists = procedure(list: TGLuint; range: TGLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDeleteTextures = procedure(n: TGLsizei; const textures: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDepthFunc = procedure(func: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDepthMask = procedure(flag: TGLboolean); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDepthRange = procedure(zNear: TGLclampd; zFar: TGLclampd); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDisable = procedure(cap: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDisableClientState = procedure(_array: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDrawArrays = procedure(mode: TGLenum; first: TGLint; count: TGLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDrawBuffer = procedure(mode: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDrawElements = procedure(mode: TGLenum; count: TGLsizei; _type: TGLenum; const indices: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDrawPixels = procedure(width: TGLsizei; height: TGLsizei; format: TGLenum; _type: TGLenum; const pixels: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglEdgeFlag = procedure(flag: TGLboolean); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglEdgeFlagPointer = procedure(stride: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglEdgeFlagv = procedure(const flag: PGLboolean); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglEnable = procedure(cap: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglEnableClientState = procedure(_array: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglEnd = procedure(); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglEndList = procedure(); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglEvalCoord1d = procedure(u: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglEvalCoord1dv = procedure(const u: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglEvalCoord1f = procedure(u: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglEvalCoord1fv = procedure(const u: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglEvalCoord2d = procedure(u: TGLdouble; v: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglEvalCoord2dv = procedure(const u: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglEvalCoord2f = procedure(u: TGLfloat; v: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglEvalCoord2fv = procedure(const u: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglEvalMesh1 = procedure(mode: TGLenum; i1: TGLint; i2: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglEvalMesh2 = procedure(mode: TGLenum; i1: TGLint; i2: TGLint; j1: TGLint; j2: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglEvalPoint1 = procedure(i: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglEvalPoint2 = procedure(i: TGLint; j: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFeedbackBuffer = procedure(size: TGLsizei; _type: TGLenum; buffer: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFinish = procedure(); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFlush = procedure(); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFogf = procedure(pname: TGLenum; param: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFogfv = procedure(pname: TGLenum; const params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFogi = procedure(pname: TGLenum; param: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFogiv = procedure(pname: TGLenum; const params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFrontFace = procedure(mode: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFrustum = procedure(left: TGLdouble; right: TGLdouble; bottom: TGLdouble; top: TGLdouble; zNear: TGLdouble; zFar: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGenLists = function(range: TGLsizei): TGLuint; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGenTextures = procedure(n: TGLsizei; textures: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetBooleanv = procedure(pname: TGLenum; params: PGLboolean); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetClipPlane = procedure(plane: TGLenum; equation: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetDoublev = procedure(pname: TGLenum; params: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetError = function(): TGLenum; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetFloatv = procedure(pname: TGLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetIntegerv = procedure(pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetLightfv = procedure(light: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetLightiv = procedure(light: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetMapdv = procedure(target: TGLenum; query: TGLenum; v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetMapfv = procedure(target: TGLenum; query: TGLenum; v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetMapiv = procedure(target: TGLenum; query: TGLenum; v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetMaterialfv = procedure(face: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetMaterialiv = procedure(face: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetPixelMapfv = procedure(map: TGLenum; values: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetPixelMapuiv = procedure(map: TGLenum; values: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetPixelMapusv = procedure(map: TGLenum; values: PGLushort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetPointerv = procedure(pname: TGLenum; params: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetPolygonStipple = procedure(mask: PGLubyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetString = function(name: TGLenum): PChar; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetTexEnvfv = procedure(target: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetTexEnviv = procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetTexGendv = procedure(coord: TGLenum; pname: TGLenum; params: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetTexGenfv = procedure(coord: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetTexGeniv = procedure(coord: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetTexImage = procedure(target: TGLenum; level: TGLint; format: TGLenum; _type: TGLenum; pixels: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetTexLevelParameterfv = procedure(target: TGLenum; level: TGLint; pname: TGLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetTexLevelParameteriv = procedure(target: TGLenum; level: TGLint; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetTexParameterfv = procedure(target: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetTexParameteriv = procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglHint = procedure(target: TGLenum; mode: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIndexMask = procedure(mask: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIndexPointer = procedure(_type: TGLenum; stride: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIndexd = procedure(c: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIndexdv = procedure(const c: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIndexf = procedure(c: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIndexfv = procedure(const c: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIndexi = procedure(c: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIndexiv = procedure(const c: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIndexs = procedure(c: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIndexsv = procedure(const c: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIndexub = procedure(c: TGLubyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIndexubv = procedure(const c: PGLubyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglInitNames = procedure(); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglInterleavedArrays = procedure(format: TGLenum; stride: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIsEnabled = function(cap: TGLenum): TGLboolean; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIsList = function(list: TGLuint): TGLboolean; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIsTexture = function(texture: TGLuint): TGLboolean; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglLightModelf = procedure(pname: TGLenum; param: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglLightModelfv = procedure(pname: TGLenum; const params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglLightModeli = procedure(pname: TGLenum; param: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglLightModeliv = procedure(pname: TGLenum; const params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglLightf = procedure(light: TGLenum; pname: TGLenum; param: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglLightfv = procedure(light: TGLenum; pname: TGLenum; const params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglLighti = procedure(light: TGLenum; pname: TGLenum; param: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglLightiv = procedure(light: TGLenum; pname: TGLenum; const params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglLineStipple = procedure(factor: TGLint; pattern: TGLushort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglLineWidth = procedure(width: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglListBase = procedure(base: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglLoadIdentity = procedure(); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglLoadMatrixd = procedure(const m: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglLoadMatrixf = procedure(const m: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglLoadName = procedure(name: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglLogicOp = procedure(opcode: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMap1d = procedure(target: TGLenum; u1: TGLdouble; u2: TGLdouble; stride: TGLint; order: TGLint; const points: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMap1f = procedure(target: TGLenum; u1: TGLfloat; u2: TGLfloat; stride: TGLint; order: TGLint; const points: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMap2d = procedure(target: TGLenum; u1: TGLdouble; u2: TGLdouble; ustride: TGLint; uorder: TGLint; v1: TGLdouble; v2: TGLdouble; vstride: TGLint; vorder: TGLint; const points: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMap2f = procedure(target: TGLenum; u1: TGLfloat; u2: TGLfloat; ustride: TGLint; uorder: TGLint; v1: TGLfloat; v2: TGLfloat; vstride: TGLint; vorder: TGLint; const points: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMapGrid1d = procedure(un: TGLint; u1: TGLdouble; u2: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMapGrid1f = procedure(un: TGLint; u1: TGLfloat; u2: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMapGrid2d = procedure(un: TGLint; u1: TGLdouble; u2: TGLdouble; vn: TGLint; v1: TGLdouble; v2: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMapGrid2f = procedure(un: TGLint; u1: TGLfloat; u2: TGLfloat; vn: TGLint; v1: TGLfloat; v2: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMaterialf = procedure(face: TGLenum; pname: TGLenum; param: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMaterialfv = procedure(face: TGLenum; pname: TGLenum; const params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMateriali = procedure(face: TGLenum; pname: TGLenum; param: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMaterialiv = procedure(face: TGLenum; pname: TGLenum; const params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMatrixMode = procedure(mode: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultMatrixd = procedure(const m: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultMatrixf = procedure(const m: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNewList = procedure(list: TGLuint; mode: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNormal3b = procedure(nx: TGLbyte; ny: TGLbyte; nz: TGLbyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNormal3bv = procedure(const v: PGLbyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNormal3d = procedure(nx: TGLdouble; ny: TGLdouble; nz: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNormal3dv = procedure(const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNormal3f = procedure(nx: TGLfloat; ny: TGLfloat; nz: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNormal3fv = procedure(const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNormal3i = procedure(nx: TGLint; ny: TGLint; nz: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNormal3iv = procedure(const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNormal3s = procedure(nx: TGLshort; ny: TGLshort; nz: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNormal3sv = procedure(const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNormalPointer = procedure(_type: TGLenum; stride: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglOrtho = procedure(left: TGLdouble; right: TGLdouble; bottom: TGLdouble; top: TGLdouble; zNear: TGLdouble; zFar: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPassThrough = procedure(token: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPixelMapfv = procedure(map: TGLenum; mapsize: TGLsizei; const values: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPixelMapuiv = procedure(map: TGLenum; mapsize: TGLsizei; const values: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPixelMapusv = procedure(map: TGLenum; mapsize: TGLsizei; const values: PGLushort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPixelStoref = procedure(pname: TGLenum; param: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPixelStorei = procedure(pname: TGLenum; param: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPixelTransferf = procedure(pname: TGLenum; param: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPixelTransferi = procedure(pname: TGLenum; param: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPixelZoom = procedure(xfactor: TGLfloat; yfactor: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPointSize = procedure(size: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPolygonMode = procedure(face: TGLenum; mode: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPolygonOffset = procedure(factor: TGLfloat; units: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPolygonStipple = procedure(const mask: PGLubyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPopAttrib = procedure(); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPopClientAttrib = procedure(); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPopMatrix = procedure(); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPopName = procedure(); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPrioritizeTextures = procedure(n: TGLsizei; const textures: PGLuint; const priorities: PGLclampf); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPushAttrib = procedure(mask: TGLbitfield); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPushClientAttrib = procedure(mask: TGLbitfield); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPushMatrix = procedure(); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPushName = procedure(name: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRasterPos2d = procedure(x: TGLdouble; y: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRasterPos2dv = procedure(const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRasterPos2f = procedure(x: TGLfloat; y: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRasterPos2fv = procedure(const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRasterPos2i = procedure(x: TGLint; y: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRasterPos2iv = procedure(const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRasterPos2s = procedure(x: TGLshort; y: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRasterPos2sv = procedure(const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRasterPos3d = procedure(x: TGLdouble; y: TGLdouble; z: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRasterPos3dv = procedure(const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRasterPos3f = procedure(x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRasterPos3fv = procedure(const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRasterPos3i = procedure(x: TGLint; y: TGLint; z: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRasterPos3iv = procedure(const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRasterPos3s = procedure(x: TGLshort; y: TGLshort; z: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRasterPos3sv = procedure(const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRasterPos4d = procedure(x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRasterPos4dv = procedure(const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRasterPos4f = procedure(x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRasterPos4fv = procedure(const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRasterPos4i = procedure(x: TGLint; y: TGLint; z: TGLint; w: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRasterPos4iv = procedure(const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRasterPos4s = procedure(x: TGLshort; y: TGLshort; z: TGLshort; w: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRasterPos4sv = procedure(const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglReadBuffer = procedure(mode: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglReadPixels = procedure(x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei; format: TGLenum; _type: TGLenum; pixels: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRectd = procedure(x1: TGLdouble; y1: TGLdouble; x2: TGLdouble; y2: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRectdv = procedure(const v1: PGLdouble; const v2: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRectf = procedure(x1: TGLfloat; y1: TGLfloat; x2: TGLfloat; y2: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRectfv = procedure(const v1: PGLfloat; const v2: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRecti = procedure(x1: TGLint; y1: TGLint; x2: TGLint; y2: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRectiv = procedure(const v1: PGLint; const v2: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRects = procedure(x1: TGLshort; y1: TGLshort; x2: TGLshort; y2: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRectsv = procedure(const v1: PGLshort; const v2: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRenderMode = function(mode: TGLenum): TGLint; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRotated = procedure(angle: TGLdouble; x: TGLdouble; y: TGLdouble; z: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRotatef = procedure(angle: TGLfloat; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglScaled = procedure(x: TGLdouble; y: TGLdouble; z: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglScalef = procedure(x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglScissor = procedure(x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSelectBuffer = procedure(size: TGLsizei; buffer: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglShadeModel = procedure(mode: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglStencilFunc = procedure(func: TGLenum; ref: TGLint; mask: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglStencilMask = procedure(mask: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglStencilOp = procedure(fail: TGLenum; zfail: TGLenum; zpass: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord1d = procedure(s: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord1dv = procedure(const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord1f = procedure(s: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord1fv = procedure(const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord1i = procedure(s: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord1iv = procedure(const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord1s = procedure(s: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord1sv = procedure(const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord2d = procedure(s: TGLdouble; t: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord2dv = procedure(const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord2f = procedure(s: TGLfloat; t: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord2fv = procedure(const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord2i = procedure(s: TGLint; t: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord2iv = procedure(const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord2s = procedure(s: TGLshort; t: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord2sv = procedure(const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord3d = procedure(s: TGLdouble; t: TGLdouble; r: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord3dv = procedure(const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord3f = procedure(s: TGLfloat; t: TGLfloat; r: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord3fv = procedure(const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord3i = procedure(s: TGLint; t: TGLint; r: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord3iv = procedure(const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord3s = procedure(s: TGLshort; t: TGLshort; r: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord3sv = procedure(const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord4d = procedure(s: TGLdouble; t: TGLdouble; r: TGLdouble; q: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord4dv = procedure(const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord4f = procedure(s: TGLfloat; t: TGLfloat; r: TGLfloat; q: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord4fv = procedure(const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord4i = procedure(s: TGLint; t: TGLint; r: TGLint; q: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord4iv = procedure(const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord4s = procedure(s: TGLshort; t: TGLshort; r: TGLshort; q: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord4sv = procedure(const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoordPointer = procedure(size: TGLint; _type: TGLenum; stride: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexEnvf = procedure(target: TGLenum; pname: TGLenum; param: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexEnvfv = procedure(target: TGLenum; pname: TGLenum; const params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexEnvi = procedure(target: TGLenum; pname: TGLenum; param: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexEnviv = procedure(target: TGLenum; pname: TGLenum; const params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexGend = procedure(coord: TGLenum; pname: TGLenum; param: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexGendv = procedure(coord: TGLenum; pname: TGLenum; const params: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexGenf = procedure(coord: TGLenum; pname: TGLenum; param: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexGenfv = procedure(coord: TGLenum; pname: TGLenum; const params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexGeni = procedure(coord: TGLenum; pname: TGLenum; param: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexGeniv = procedure(coord: TGLenum; pname: TGLenum; const params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexImage1D = procedure(target: TGLenum; level: TGLint; internalformat: TGLint; width: TGLsizei; border: TGLint; format: TGLenum; _type: TGLenum; const pixels: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexImage2D = procedure(target: TGLenum; level: TGLint; internalformat: TGLint; width: TGLsizei; height: TGLsizei; border: TGLint; format: TGLenum; _type: TGLenum; const pixels: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexParameterf = procedure(target: TGLenum; pname: TGLenum; param: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexParameterfv = procedure(target: TGLenum; pname: TGLenum; const params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexParameteri = procedure(target: TGLenum; pname: TGLenum; param: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexParameteriv = procedure(target: TGLenum; pname: TGLenum; const params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexSubImage1D = procedure(target: TGLenum; level: TGLint; xoffset: TGLint; width: TGLsizei; format: TGLenum; _type: TGLenum; const pixels: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexSubImage2D = procedure(target: TGLenum; level: TGLint; xoffset: TGLint; yoffset: TGLint; width: TGLsizei; height: TGLsizei; format: TGLenum; _type: TGLenum; const pixels: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTranslated = procedure(x: TGLdouble; y: TGLdouble; z: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTranslatef = procedure(x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertex2d = procedure(x: TGLdouble; y: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertex2dv = procedure(const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertex2f = procedure(x: TGLfloat; y: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertex2fv = procedure(const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertex2i = procedure(x: TGLint; y: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertex2iv = procedure(const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertex2s = procedure(x: TGLshort; y: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertex2sv = procedure(const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertex3d = procedure(x: TGLdouble; y: TGLdouble; z: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertex3dv = procedure(const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertex3f = procedure(x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertex3fv = procedure(const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertex3i = procedure(x: TGLint; y: TGLint; z: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertex3iv = procedure(const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertex3s = procedure(x: TGLshort; y: TGLshort; z: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertex3sv = procedure(const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertex4d = procedure(x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertex4dv = procedure(const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertex4f = procedure(x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertex4fv = procedure(const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertex4i = procedure(x: TGLint; y: TGLint; z: TGLint; w: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertex4iv = procedure(const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertex4s = procedure(x: TGLshort; y: TGLshort; z: TGLshort; w: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertex4sv = procedure(const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexPointer = procedure(size: TGLint; _type: TGLenum; stride: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglViewport = procedure(x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_VERSION_1_2
  TglBlendColor = procedure(red: TGLclampf; green: TGLclampf; blue: TGLclampf; alpha: TGLclampf); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBlendEquation = procedure(mode: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDrawRangeElements = procedure(mode: TGLenum; start: TGLuint; _end: TGLuint; count: TGLsizei; _type: TGLenum; const indices: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColorTable = procedure(target: TGLenum; internalformat: TGLenum; width: TGLsizei; format: TGLenum; _type: TGLenum; const table: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColorTableParameterfv = procedure(target: TGLenum; pname: TGLenum; const params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColorTableParameteriv = procedure(target: TGLenum; pname: TGLenum; const params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCopyColorTable = procedure(target: TGLenum; internalformat: TGLenum; x: TGLint; y: TGLint; width: TGLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetColorTable = procedure(target: TGLenum; format: TGLenum; _type: TGLenum; table: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetColorTableParameterfv = procedure(target: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetColorTableParameteriv = procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColorSubTable = procedure(target: TGLenum; start: TGLsizei; count: TGLsizei; format: TGLenum; _type: TGLenum; const data: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCopyColorSubTable = procedure(target: TGLenum; start: TGLsizei; x: TGLint; y: TGLint; width: TGLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglConvolutionFilter1D = procedure(target: TGLenum; internalformat: TGLenum; width: TGLsizei; format: TGLenum; _type: TGLenum; const image: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglConvolutionFilter2D = procedure(target: TGLenum; internalformat: TGLenum; width: TGLsizei; height: TGLsizei; format: TGLenum; _type: TGLenum; const image: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglConvolutionParameterf = procedure(target: TGLenum; pname: TGLenum; params: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglConvolutionParameterfv = procedure(target: TGLenum; pname: TGLenum; const params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglConvolutionParameteri = procedure(target: TGLenum; pname: TGLenum; params: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglConvolutionParameteriv = procedure(target: TGLenum; pname: TGLenum; const params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCopyConvolutionFilter1D = procedure(target: TGLenum; internalformat: TGLenum; x: TGLint; y: TGLint; width: TGLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCopyConvolutionFilter2D = procedure(target: TGLenum; internalformat: TGLenum; x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetConvolutionFilter = procedure(target: TGLenum; format: TGLenum; _type: TGLenum; image: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetConvolutionParameterfv = procedure(target: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetConvolutionParameteriv = procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetSeparableFilter = procedure(target: TGLenum; format: TGLenum; _type: TGLenum; row: PGLvoid; column: PGLvoid; span: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSeparableFilter2D = procedure(target: TGLenum; internalformat: TGLenum; width: TGLsizei; height: TGLsizei; format: TGLenum; _type: TGLenum; const row: PGLvoid; const column: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetHistogram = procedure(target: TGLenum; reset: TGLboolean; format: TGLenum; _type: TGLenum; values: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetHistogramParameterfv = procedure(target: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetHistogramParameteriv = procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetMinmax = procedure(target: TGLenum; reset: TGLboolean; format: TGLenum; _type: TGLenum; values: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetMinmaxParameterfv = procedure(target: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetMinmaxParameteriv = procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglHistogram = procedure(target: TGLenum; width: TGLsizei; internalformat: TGLenum; sink: TGLboolean); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMinmax = procedure(target: TGLenum; internalformat: TGLenum; sink: TGLboolean); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglResetHistogram = procedure(target: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglResetMinmax = procedure(target: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexImage3D = procedure(target: TGLenum; level: TGLint; internalformat: TGLint; width: TGLsizei; height: TGLsizei; depth: TGLsizei; border: TGLint; format: TGLenum; _type: TGLenum; const pixels: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexSubImage3D = procedure(target: TGLenum; level: TGLint; xoffset: TGLint; yoffset: TGLint; zoffset: TGLint; width: TGLsizei; height: TGLsizei; depth: TGLsizei; format: TGLenum; _type: TGLenum; const pixels: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCopyTexSubImage3D = procedure(target: TGLenum; level: TGLint; xoffset: TGLint; yoffset: TGLint; zoffset: TGLint; x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_VERSION_1_3
  TglActiveTexture = procedure(texture: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglClientActiveTexture = procedure(texture: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord1d = procedure(target: TGLenum; s: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord1dv = procedure(target: TGLenum; const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord1f = procedure(target: TGLenum; s: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord1fv = procedure(target: TGLenum; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord1i = procedure(target: TGLenum; s: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord1iv = procedure(target: TGLenum; const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord1s = procedure(target: TGLenum; s: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord1sv = procedure(target: TGLenum; const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord2d = procedure(target: TGLenum; s: TGLdouble; t: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord2dv = procedure(target: TGLenum; const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord2f = procedure(target: TGLenum; s: TGLfloat; t: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord2fv = procedure(target: TGLenum; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord2i = procedure(target: TGLenum; s: TGLint; t: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord2iv = procedure(target: TGLenum; const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord2s = procedure(target: TGLenum; s: TGLshort; t: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord2sv = procedure(target: TGLenum; const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord3d = procedure(target: TGLenum; s: TGLdouble; t: TGLdouble; r: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord3dv = procedure(target: TGLenum; const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord3f = procedure(target: TGLenum; s: TGLfloat; t: TGLfloat; r: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord3fv = procedure(target: TGLenum; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord3i = procedure(target: TGLenum; s: TGLint; t: TGLint; r: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord3iv = procedure(target: TGLenum; const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord3s = procedure(target: TGLenum; s: TGLshort; t: TGLshort; r: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord3sv = procedure(target: TGLenum; const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord4d = procedure(target: TGLenum; s: TGLdouble; t: TGLdouble; r: TGLdouble; q: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord4dv = procedure(target: TGLenum; const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord4f = procedure(target: TGLenum; s: TGLfloat; t: TGLfloat; r: TGLfloat; q: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord4fv = procedure(target: TGLenum; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord4i = procedure(target: TGLenum; s: TGLint; t: TGLint; r: TGLint; q: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord4iv = procedure(target: TGLenum; const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord4s = procedure(target: TGLenum; s: TGLshort; t: TGLshort; r: TGLshort; q: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord4sv = procedure(target: TGLenum; const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglLoadTransposeMatrixf = procedure(const m: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglLoadTransposeMatrixd = procedure(const m: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultTransposeMatrixf = procedure(const m: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultTransposeMatrixd = procedure(const m: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSampleCoverage = procedure(value: TGLclampf; invert: TGLboolean); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCompressedTexImage3D = procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; width: TGLsizei; height: TGLsizei; depth: TGLsizei; border: TGLint; imageSize: TGLsizei; const data: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCompressedTexImage2D = procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; width: TGLsizei; height: TGLsizei; border: TGLint; imageSize: TGLsizei; const data: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCompressedTexImage1D = procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; width: TGLsizei; border: TGLint; imageSize: TGLsizei; const data: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCompressedTexSubImage3D = procedure(target: TGLenum; level: TGLint; xoffset: TGLint; yoffset: TGLint; zoffset: TGLint; width: TGLsizei; height: TGLsizei; depth: TGLsizei; format: TGLenum; imageSize: TGLsizei; const data: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCompressedTexSubImage2D = procedure(target: TGLenum; level: TGLint; xoffset: TGLint; yoffset: TGLint; width: TGLsizei; height: TGLsizei; format: TGLenum; imageSize: TGLsizei; const data: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCompressedTexSubImage1D = procedure(target: TGLenum; level: TGLint; xoffset: TGLint; width: TGLsizei; format: TGLenum; imageSize: TGLsizei; const data: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetCompressedTexImage = procedure(target: TGLenum; level: TGLint; img: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_VERSION_1_4
  TglBlendFuncSeparate = procedure(sfactorRGB: TGLenum; dfactorRGB: TGLenum; sfactorAlpha: TGLenum; dfactorAlpha: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFogCoordf = procedure(coord: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFogCoordfv = procedure(const coord: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFogCoordd = procedure(coord: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFogCoorddv = procedure(const coord: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFogCoordPointer = procedure(_type: TGLenum; stride: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiDrawArrays = procedure(mode: TGLenum; first: PGLint; count: PGLsizei; primcount: TGLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiDrawElements = procedure(mode: TGLenum; const count: PGLsizei; _type: TGLenum; const indices: PGLvoid; primcount: TGLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPointParameterf = procedure(pname: TGLenum; param: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPointParameterfv = procedure(pname: TGLenum; const params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPointParameteri = procedure(pname: TGLenum; param: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPointParameteriv = procedure(pname: TGLenum; const params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSecondaryColor3b = procedure(red: TGLbyte; green: TGLbyte; blue: TGLbyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSecondaryColor3bv = procedure(const v: PGLbyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSecondaryColor3d = procedure(red: TGLdouble; green: TGLdouble; blue: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSecondaryColor3dv = procedure(const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSecondaryColor3f = procedure(red: TGLfloat; green: TGLfloat; blue: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSecondaryColor3fv = procedure(const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSecondaryColor3i = procedure(red: TGLint; green: TGLint; blue: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSecondaryColor3iv = procedure(const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSecondaryColor3s = procedure(red: TGLshort; green: TGLshort; blue: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSecondaryColor3sv = procedure(const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSecondaryColor3ub = procedure(red: TGLubyte; green: TGLubyte; blue: TGLubyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSecondaryColor3ubv = procedure(const v: PGLubyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSecondaryColor3ui = procedure(red: TGLuint; green: TGLuint; blue: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSecondaryColor3uiv = procedure(const v: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSecondaryColor3us = procedure(red: TGLushort; green: TGLushort; blue: TGLushort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSecondaryColor3usv = procedure(const v: PGLushort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSecondaryColorPointer = procedure(size: TGLint; _type: TGLenum; stride: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos2d = procedure(x: TGLdouble; y: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos2dv = procedure(const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos2f = procedure(x: TGLfloat; y: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos2fv = procedure(const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos2i = procedure(x: TGLint; y: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos2iv = procedure(const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos2s = procedure(x: TGLshort; y: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos2sv = procedure(const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos3d = procedure(x: TGLdouble; y: TGLdouble; z: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos3dv = procedure(const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos3f = procedure(x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos3fv = procedure(const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos3i = procedure(x: TGLint; y: TGLint; z: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos3iv = procedure(const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos3s = procedure(x: TGLshort; y: TGLshort; z: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos3sv = procedure(const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_VERSION_1_5
  TglGenQueries = procedure(n: GLsizei; ids: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDeleteQueries = procedure(n: GLsizei; const ids: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIsQuery = function(id: GLuint): boolean; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBeginQuery = procedure(target: GLenum; id: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglEndQuery = procedure(target: GLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetQueryiv = procedure(target, pname: GLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetQueryObjectiv = procedure(id: GLuint; pname: GLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetQueryObjectuiv = procedure(id: GLuint; pname: GLenum; params: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBindBuffer = procedure(target: TGLenum; buffer: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDeleteBuffers = procedure(n: TGLsizei; const buffers: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGenBuffers = procedure(n: TGLsizei; buffers: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIsBuffer = function(buffer: TGLuint): TGLboolean; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBufferData = procedure(target: TGLenum; size: TGLsizei; const data: PGLvoid; usage: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBufferSubData = procedure(target: TGLenum; offset: TGLsizei; size: TGLsizei; const data: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetBufferSubData = procedure(target: TGLenum; offset: TGLsizei; size: TGLsizei; data: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMapBuffer = function(target: TGLenum; access: TGLenum): PGLvoid; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUnmapBuffer = function(target: TGLenum): TGLboolean; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetBufferParameteriv = procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetBufferPointerv = procedure(target: TGLenum; pname: TGLenum; params: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_VERSION_2_0
  TglBlendEquationSeparate = procedure(modeRGB: GLenum; modeAlpha: GLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDrawBuffers = procedure(n: GLsizei; const bufs: PGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglStencilOpSeparate = procedure(face: GLenum; sfail: GLenum; dpfail: GLenum; dppass: GLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglStencilFuncSeparate = procedure(frontfunc: GLenum; backfunc: GLenum; ref: GLint; mask: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglStencilMaskSeparate = procedure(face: GLenum; mask: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglAttachShader = procedure(programObj, shaderObj: GLhandle); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBindAttribLocation = procedure(programObj: GLhandle; index: GLuint; name: PChar); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCompileShader = procedure(shaderObj: GLhandle); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCreateProgram = function: GLhandle; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCreateShader = function(shaderType: GLenum): GLhandle; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDeleteProgram = procedure(programObj: GLhandle); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDeleteShader = procedure(shaderObj: GLhandle); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDetachShader = procedure(programObj, shaderObj: GLhandle); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDisableVertexAttribArray = procedure(index: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglEnableVertexAttribArray = procedure(index: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetActiveAttrib = procedure(programObj: GLhandle; index: GLuint; maxlength: GLsizei; var length: GLint; var size: GLint; var _type: GLenum; name: PChar); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetActiveUniform = procedure(programObj: GLhandle; index: GLuint; maxLength: GLsizei; var length: GLsizei; var size: GLint; var _type: GLenum; name: PChar); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetAttachedShaders = procedure(programObj: GLhandle; MaxCount: GLsizei; var Count: GLint; shaders: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetAttribLocation = function(programObj: GLhandle; char: string): glint; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetProgramiv = procedure(programObj: GLhandle; pname: TGLenum; params: PGLInt); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetProgramInfoLog = procedure(programObj: GLHandle; maxLength: glsizei; var length: GLint; infoLog: PChar); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetShaderiv = procedure(shaderObj: GLhandle; pname: TGLenum; params: PGLInt); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetShaderInfoLog = procedure(shaderObj: GLHandle; maxLength: glsizei; var length: glint; infoLog: PChar); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetShaderSource = procedure(shaderObj: GLhandle; maxlength: GLsizei; var length: GLsizei; source: PChar); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetUniformLocation = function(programObj: GLhandle; const char: PChar): glint; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetUniformfv = procedure(programObj: GLhandle; location: GLint; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetUniformiv = procedure(programObj: GLhandle; location: GLint; params: PGLInt); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetVertexAttribfv = procedure(index: GLuint; pname: GLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetVertexAttribiv = procedure(index: GLuint; pname: GLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetVertexAttribPointerv = procedure(index: GLuint; pname: GLenum; _pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIsProgram = function(programObj: GLhandle) : TGLboolean; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIsShader = function(shaderObj: GLhandle) : TGLboolean; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglLinkProgram = procedure(programObj: GLHandle); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglShaderSource = procedure(shaderObj: GLHandle; count: glsizei; _string: PPGLChar; lengths: pglint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUseProgram = procedure(programObj: GLhandle); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform1f = procedure(location: GLint; v0: GLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform2f = procedure(location: GLint; v0, v1: GLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform3f = procedure(location: GLint; v0, v1, v2: GLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform4f = procedure(location: GLint; v0, v1, v2, v3: GLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform1i = procedure(location: GLint; v0: GLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform2i = procedure(location: GLint; v0, v1: GLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform3i = procedure(location: GLint; v0, v1, v2: GLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform4i = procedure(location: GLint; v0, v1, v2, v3: GLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform1fv = procedure(location: GLint; count: GLsizei; value: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform2fv = procedure(location: GLint; count: GLsizei; value: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform3fv = procedure(location: GLint; count: GLsizei; value: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform4fv = procedure(location: GLint; count: GLsizei; value: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform1iv = procedure(location: GLint; count: GLsizei; value: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform2iv = procedure(location: GLint; count: GLsizei; value: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform3iv = procedure(location: GLint; count: GLsizei; value: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform4iv = procedure(location: GLint; count: GLsizei; value: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniformMatrix2fv = procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniformMatrix3fv = procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniformMatrix4fv = procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglValidateProgram = procedure(programObj: GLhandle); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib1d = procedure(index: GLuint; x: GLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib1dv = procedure(index: GLuint; const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib1f = procedure(index: GLuint; x: GLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib1fv = procedure(index: GLuint; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib1s = procedure(index: GLuint; x: GLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib1sv = procedure(index: GLuint; const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib2d = procedure(index: GLuint; x: GLdouble; y: GLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib2dv = procedure(index: GLuint; const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib2f = procedure(index: GLuint; x: GLfloat; y: GLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib2fv = procedure(index: GLuint; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib2s = procedure(index: GLuint; x: GLshort; y: GLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib2sv = procedure(index: GLuint; const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib3d = procedure(index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib3dv = procedure(index: GLuint; const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib3f = procedure(index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib3fv = procedure(index: GLuint; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib3s = procedure(index: GLuint; x: GLshort; y: GLshort; z: GLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib3sv = procedure(index: GLuint; const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4Nbv = procedure(index: GLuint; const v: PGLbyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4Niv = procedure(index: GLuint; const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4Nsv = procedure(index: GLuint; const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4Nub = procedure(index: GLuint; x: GLubyte; y: GLubyte; z: GLubyte; w: GLubyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4Nubv = procedure(index: GLuint; const v: PGLubyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4Nuiv = procedure(index: GLuint; const v: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4Nusv = procedure(index: GLuint; const v: PGLushort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4bv = procedure(index: GLuint; const v: PGLbyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4d = procedure(index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4dv = procedure(index: GLuint; const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4f = procedure(index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4fv = procedure(index: GLuint; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4iv = procedure(index: GLuint; const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4s = procedure(index: GLuint; x: GLshort; y: GLshort; z: GLshort; w: GLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4sv = procedure(index: GLuint; const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4ubv = procedure(index: GLuint; const v: PGLubyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4uiv = procedure(index: GLuint; const v: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4usv = procedure(index: GLuint; const v: PGLushort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribPointer = procedure(index: GLuint; size: GLint; _type: GLenum; normalized: GLboolean; stride: GLsizei; const _pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_VERSION_2_1
  TglUniformMatrix2x3fv = procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniformMatrix3x2fv = procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniformMatrix2x4fv = procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniformMatrix4x2fv = procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniformMatrix3x4fv = procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniformMatrix4x3fv = procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_VERSION_3_0
  { OpenGL 3.0 also reuses entry points from these extensions: }
  { ARB_framebuffer_object }
  { ARB_map_buffer_range }
  { ARB_vertex_array_object }
  TglColorMaski = procedure(index_: GLuint; r: GLboolean; g: GLboolean; b: GLboolean; a: GLboolean); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetBooleani_v = procedure(target: GLenum; index_: GLuint; data: PGLboolean); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetIntegeri_v = procedure(target: GLenum; index_: GLuint; data: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglEnablei = procedure(target: GLenum; index_: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDisablei = procedure(target: GLenum; index_: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIsEnabledi = function(target: GLenum; index_: GLuint): GLboolean; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBeginTransformFeedback = procedure(primitiveMode: GLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglEndTransformFeedback = procedure(); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBindBufferRange = procedure(target: GLenum; index_: GLuint; buffer: GLuint; offset: GLsizei; size: GLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBindBufferBase = procedure(target: GLenum; index_: GLuint; buffer: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTransformFeedbackVaryings = procedure(program_: GLuint; count: GLsizei; const locations: PGLint; bufferMode: GLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetTransformFeedbackVarying = procedure(program_: GLuint; index_: GLuint; locations: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglClampColor = procedure(targe: GLenum; clamp: GLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBeginConditionalRender = procedure(id: GLuint; mode: GLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglEndConditionalRender = procedure(); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI1i = procedure(index_: GLuint; x: GLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI2i = procedure(index_: GLuint; x: GLint; y: GLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI3i = procedure(index_: GLuint; x: GLint; y: GLint; z: GLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI4i = procedure(index_: GLuint; x: GLint; y: GLint; z: GLint; w: GLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI1ui = procedure(index_: GLuint; x: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI2ui = procedure(index_: GLuint; x: GLuint; y: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI3ui = procedure(index_: GLuint; x: GLuint; y: GLuint; z: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI4ui = procedure(index_: GLuint; x: GLuint; y: GLuint; z: GLuint; w: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI1iv = procedure(index_: GLuint; const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI2iv = procedure(index_: GLuint; const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI3iv = procedure(index_: GLuint; const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI4iv = procedure(index_: GLuint; const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI1uiv = procedure(index_: GLuint; const v: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI2uiv = procedure(index_: GLuint; const v: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI3uiv = procedure(index_: GLuint; const v: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI4uiv = procedure(index_: GLuint; const v: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI4bv = procedure(index_: GLuint; const v: PGLbyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI4sv = procedure(index_: GLuint; const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI4ubv = procedure(index_: GLuint; const v: PGLubyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI4usv = procedure(index_: GLuint; const v: PGLushort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribIPointer = procedure(index_: GLuint; size: GLint; type_: GLenum; stride: GLsizei; const pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetVertexAttribIiv = procedure(index_: GLuint; pname: GLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetVertexAttribIuiv = procedure(index_: GLuint; pname: GLenum; params: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetUniformuiv = procedure(program_: GLuint; location: GLint; params: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBindFragDataLocation = procedure(program_: GLuint; color: GLuint; const name: PChar); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetFragDataLocation = function(program_: GLuint; const name: PChar): GLint; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform1ui = procedure(location: GLint; v0: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform2ui = procedure(location: GLint; v0: GLuint; v1: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform3ui = procedure(location: GLint; v0: GLuint; v1: GLuint; v2: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform4ui = procedure(location: GLint; v0: GLuint; v1: GLuint; v2: GLuint; v3: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform1uiv = procedure(location: GLint; count: GLsizei; const value: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform2uiv = procedure(location: GLint; count: GLsizei; const value: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform3uiv = procedure(location: GLint; count: GLsizei; const value: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform4uiv = procedure(location: GLint; count: GLsizei; const value: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexParameterIiv = procedure(target: GLenum; pname: GLenum; const params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexParameterIuiv = procedure(target: GLenum; pname: GLenum; const params: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetTexParameterIiv = procedure(target: GLenum; pname: GLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetTexParameterIuiv = procedure(target: GLenum; pname: GLenum; params: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglClearBufferiv = procedure(buffer: GLenum; drawbuffer: GLint; const value: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglClearBufferuiv = procedure(buffer: GLenum; drawbuffer: GLint; const value: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglClearBufferfv = procedure(buffer: GLenum; drawbuffer: GLint; const value: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglClearBufferfi = procedure(buffer: GLenum; drawbuffer: GLint; depth: GLfloat; stencil: GLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetStringi = function(name: GLenum; index: GLuint): PGLubyte; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_3DFX_tbuffer
  TglTbufferMask3DFX = procedure(mask: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_APPLE_element_array
  TglElementPointerAPPLE = procedure(_type: TGLenum; const _pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDrawElementArrayAPPLE = procedure(mode: TGLenum; first: TGLint; count: TGLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDrawRangeElementArrayAPPLE = procedure(mode: TGLenum; start: TGLuint; _end: TGLuint; first: TGLint; count: TGLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiDrawElementArrayAPPLE = procedure(mode: TGLenum; const first: PGLint; const count: PGLsizei; primcount: TGLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiDrawRangeElementArrayAPPLE = procedure(mode: TGLenum; start: TGLuint; _end: TGLuint; const first: PGLint; const count: PGLsizei; primcount: TGLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_APPLE_fence
  TglGenFencesAPPLE = procedure(n: TGLsizei; fences: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDeleteFencesAPPLE = procedure(n: TGLsizei; const fences: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSetFenceAPPLE = procedure(fence: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIsFenceAPPLE = function(fence: TGLuint): TGLboolean; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTestFenceAPPLE = function(fence: TGLuint): TGLboolean; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFinishFenceAPPLE = procedure(fence: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTestObjectAPPLE = function(_object: TGLenum; name: TGLuint): TGLboolean; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFinishObjectAPPLE = procedure(_object: TGLenum; name: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_APPLE_vertex_array_object
  TglBindVertexArrayAPPLE = procedure(_array: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDeleteVertexArraysAPPLE = procedure(n: TGLsizei; const arrays: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGenVertexArraysAPPLE = procedure(n: TGLsizei; const arrays: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIsVertexArrayAPPLE = function(_array: TGLuint): TGLboolean; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_APPLE_vertex_array_range
  TglVertexArrayRangeAPPLE = procedure(length: TGLsizei; _pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFlushVertexArrayRangeAPPLE = procedure(length: TGLsizei; _pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexArrayParameteriAPPLE = procedure(pname: TGLenum; param: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_ARB_matrix_palette
  TglCurrentPaletteMatrixARB = procedure(index: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMatrixIndexubvARB = procedure(size: TGLint; const indices: PGLubyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMatrixIndexusvARB = procedure(size: TGLint; const indices: PGLushort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMatrixIndexuivARB = procedure(size: TGLint; const indices: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMatrixIndexPointerARB = procedure(size: TGLint; _type: TGLenum; stride: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_ARB_multisample
  TglSampleCoverageARB = procedure(value: TGLclampf; invert: TGLboolean); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_ARB_multitexture
  TglActiveTextureARB = procedure(texture: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglClientActiveTextureARB = procedure(texture: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord1dARB = procedure(target: TGLenum; s: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord1dvARB = procedure(target: TGLenum; const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord1fARB = procedure(target: TGLenum; s: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord1fvARB = procedure(target: TGLenum; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord1iARB = procedure(target: TGLenum; s: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord1ivARB = procedure(target: TGLenum; const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord1sARB = procedure(target: TGLenum; s: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord1svARB = procedure(target: TGLenum; const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord2dARB = procedure(target: TGLenum; s: TGLdouble; t: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord2dvARB = procedure(target: TGLenum; const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord2fARB = procedure(target: TGLenum; s: TGLfloat; t: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord2fvARB = procedure(target: TGLenum; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord2iARB = procedure(target: TGLenum; s: TGLint; t: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord2ivARB = procedure(target: TGLenum; const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord2sARB = procedure(target: TGLenum; s: TGLshort; t: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord2svARB = procedure(target: TGLenum; const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord3dARB = procedure(target: TGLenum; s: TGLdouble; t: TGLdouble; r: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord3dvARB = procedure(target: TGLenum; const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord3fARB = procedure(target: TGLenum; s: TGLfloat; t: TGLfloat; r: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord3fvARB = procedure(target: TGLenum; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord3iARB = procedure(target: TGLenum; s: TGLint; t: TGLint; r: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord3ivARB = procedure(target: TGLenum; const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord3sARB = procedure(target: TGLenum; s: TGLshort; t: TGLshort; r: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord3svARB = procedure(target: TGLenum; const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord4dARB = procedure(target: TGLenum; s: TGLdouble; t: TGLdouble; r: TGLdouble; q: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord4dvARB = procedure(target: TGLenum; const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord4fARB = procedure(target: TGLenum; s: TGLfloat; t: TGLfloat; r: TGLfloat; q: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord4fvARB = procedure(target: TGLenum; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord4iARB = procedure(target: TGLenum; s: TGLint; t: TGLint; r: TGLint; q: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord4ivARB = procedure(target: TGLenum; const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord4sARB = procedure(target: TGLenum; s: TGLshort; t: TGLshort; r: TGLshort; q: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord4svARB = procedure(target: TGLenum; const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_ARB_point_parameters
  TglPointParameterfARB = procedure(pname: TGLenum; param: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPointParameterfvARB = procedure(pname: TGLenum; const params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_ARB_texture_compression
  TglCompressedTexImage3DARB = procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; width: TGLsizei; height: TGLsizei; depth: TGLsizei; border: TGLint; imageSize: TGLsizei; const data: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCompressedTexImage2DARB = procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; width: TGLsizei; height: TGLsizei; border: TGLint; imageSize: TGLsizei; const data: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCompressedTexImage1DARB = procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; width: TGLsizei; border: TGLint; imageSize: TGLsizei; const data: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCompressedTexSubImage3DARB = procedure(target: TGLenum; level: TGLint; xoffset: TGLint; yoffset: TGLint; zoffset: TGLint; width: TGLsizei; height: TGLsizei; depth: TGLsizei; format: TGLenum; imageSize: TGLsizei; const data: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCompressedTexSubImage2DARB = procedure(target: TGLenum; level: TGLint; xoffset: TGLint; yoffset: TGLint; width: TGLsizei; height: TGLsizei; format: TGLenum; imageSize: TGLsizei; const data: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCompressedTexSubImage1DARB = procedure(target: TGLenum; level: TGLint; xoffset: TGLint; width: TGLsizei; format: TGLenum; imageSize: TGLsizei; const data: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetCompressedTexImageARB = procedure(target: TGLenum; level: TGLint; img: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_ARB_transpose_matrix
  TglLoadTransposeMatrixfARB = procedure(const m: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglLoadTransposeMatrixdARB = procedure(const m: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultTransposeMatrixfARB = procedure(const m: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultTransposeMatrixdARB = procedure(const m: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_ARB_vertex_blend
  TglWeightbvARB = procedure(size: TGLint; const weights: PGLbyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWeightsvARB = procedure(size: TGLint; const weights: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWeightivARB = procedure(size: TGLint; const weights: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWeightfvARB = procedure(size: TGLint; const weights: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWeightdvARB = procedure(size: TGLint; const weights: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWeightubvARB = procedure(size: TGLint; const weights: PGLubyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWeightusvARB = procedure(size: TGLint; const weights: PGLushort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWeightuivARB = procedure(size: TGLint; const weights: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWeightPointerARB = procedure(size: TGLint; _type: TGLenum; stride: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexBlendARB = procedure(count: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_ARB_vertex_buffer_object
  TglBindBufferARB = procedure(target: TGLenum; buffer: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDeleteBuffersARB = procedure(n: TGLsizei; const buffers: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGenBuffersARB = procedure(n: TGLsizei; buffers: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIsBufferARB = function(buffer: TGLuint): TGLboolean; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBufferDataARB = procedure(target: TGLenum; size: TGLsizei; const data: PGLvoid; usage: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBufferSubDataARB = procedure(target: TGLenum; offset: TGLsizei; size: TGLsizei; const data: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetBufferSubDataARB = procedure(target: TGLenum; offset: TGLsizei; size: TGLsizei; data: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMapBufferARB = function(target: TGLenum; access: TGLenum): PGLvoid; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUnmapBufferARB = function(target: TGLenum): TGLboolean; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetBufferParameterivARB = procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetBufferPointervARB = procedure(target: TGLenum; pname: TGLenum; params: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_ARB_vertex_program
  TglVertexAttrib1dARB = procedure(index: TGLuint; x: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib1dvARB = procedure(index: TGLuint; const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib1fARB = procedure(index: TGLuint; x: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib1fvARB = procedure(index: TGLuint; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib1sARB = procedure(index: TGLuint; x: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib1svARB = procedure(index: TGLuint; const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib2dARB = procedure(index: TGLuint; x: TGLdouble; y: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib2dvARB = procedure(index: TGLuint; const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib2fARB = procedure(index: TGLuint; x: TGLfloat; y: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib2fvARB = procedure(index: TGLuint; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib2sARB = procedure(index: TGLuint; x: TGLshort; y: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib2svARB = procedure(index: TGLuint; const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib3dARB = procedure(index: TGLuint; x: TGLdouble; y: TGLdouble; z: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib3dvARB = procedure(index: TGLuint; const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib3fARB = procedure(index: TGLuint; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib3fvARB = procedure(index: TGLuint; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib3sARB = procedure(index: TGLuint; x: TGLshort; y: TGLshort; z: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib3svARB = procedure(index: TGLuint; const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4NbvARB = procedure(index: TGLuint; const v: PGLbyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4NivARB = procedure(index: TGLuint; const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4NsvARB = procedure(index: TGLuint; const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4NubARB = procedure(index: TGLuint; x: TGLubyte; y: TGLubyte; z: TGLubyte; w: TGLubyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4NubvARB = procedure(index: TGLuint; const v: PGLubyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4NuivARB = procedure(index: TGLuint; const v: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4NusvARB = procedure(index: TGLuint; const v: PGLushort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4bvARB = procedure(index: TGLuint; const v: PGLbyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4dARB = procedure(index: TGLuint; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4dvARB = procedure(index: TGLuint; const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4fARB = procedure(index: TGLuint; x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4fvARB = procedure(index: TGLuint; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4ivARB = procedure(index: TGLuint; const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4sARB = procedure(index: TGLuint; x: TGLshort; y: TGLshort; z: TGLshort; w: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4svARB = procedure(index: TGLuint; const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4ubvARB = procedure(index: TGLuint; const v: PGLubyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4uivARB = procedure(index: TGLuint; const v: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4usvARB = procedure(index: TGLuint; const v: PGLushort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribPointerARB = procedure(index: TGLuint; size: TGLint; _type: TGLenum; normalized: TGLboolean; stride: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglEnableVertexAttribArrayARB = procedure(index: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDisableVertexAttribArrayARB = procedure(index: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramStringARB = procedure(target: TGLenum; format: TGLenum; len: TGLsizei; const _string: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBindProgramARB = procedure(target: TGLenum; _program: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDeleteProgramsARB = procedure(n: TGLsizei; const programs: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGenProgramsARB = procedure(n: TGLsizei; programs: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramEnvParameter4dARB = procedure(target: TGLenum; index: TGLuint; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramEnvParameter4dvARB = procedure(target: TGLenum; index: TGLuint; const params: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramEnvParameter4fARB = procedure(target: TGLenum; index: TGLuint; x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramEnvParameter4fvARB = procedure(target: TGLenum; index: TGLuint; const params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramLocalParameter4dARB = procedure(target: TGLenum; index: TGLuint; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramLocalParameter4dvARB = procedure(target: TGLenum; index: TGLuint; const params: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramLocalParameter4fARB = procedure(target: TGLenum; index: TGLuint; x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramLocalParameter4fvARB = procedure(target: TGLenum; index: TGLuint; const params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetProgramEnvParameterdvARB = procedure(target: TGLenum; index: TGLuint; params: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetProgramEnvParameterfvARB = procedure(target: TGLenum; index: TGLuint; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetProgramLocalParameterdvARB = procedure(target: TGLenum; index: TGLuint; params: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetProgramLocalParameterfvARB = procedure(target: TGLenum; index: TGLuint; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetProgramivARB = procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetProgramStringARB = procedure(target: TGLenum; pname: TGLenum; _string: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetVertexAttribdvARB = procedure(index: TGLuint; pname: TGLenum; params: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetVertexAttribfvARB = procedure(index: TGLuint; pname: TGLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetVertexAttribivARB = procedure(index: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetVertexAttribPointervARB = procedure(index: TGLuint; pname: TGLenum; _pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIsProgramARB = function(_program: TGLuint): TGLboolean; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_ARB_window_pos
  TglWindowPos2dARB = procedure(x: TGLdouble; y: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos2dvARB = procedure(const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos2fARB = procedure(x: TGLfloat; y: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos2fvARB = procedure(const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos2iARB = procedure(x: TGLint; y: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos2ivARB = procedure(const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos2sARB = procedure(x: TGLshort; y: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos2svARB = procedure(const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos3dARB = procedure(x: TGLdouble; y: TGLdouble; z: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos3dvARB = procedure(const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos3fARB = procedure(x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos3fvARB = procedure(const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos3iARB = procedure(x: TGLint; y: TGLint; z: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos3ivARB = procedure(const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos3sARB = procedure(x: TGLshort; y: TGLshort; z: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos3svARB = procedure(const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_ARB_draw_buffers
  TglDrawBuffersARB = procedure(n: TGLsizei; bufs: PGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_ARB_color_buffer_float
  TglClampColorARB = procedure(target: TGLenum; clamp: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_ARB_vertex_shader
  TglGetActiveAttribARB = procedure(programobj: GLhandleARB; index: GLuint; maxLength: GLsizei; var length: GLsizei; var size: GLint; var _type: GLenum; name: PChar); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetAttribLocationARB = function(programObj: GLhandleARB; const char: PChar): glint; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBindAttribLocationARB = procedure(programObj: GLhandleARB; index: GLuint; const name: PChar); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_ARB_shader_objects
  TglDeleteObjectARB = procedure(Obj: GLHandleARB); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetHandleARB = function(pname: GlEnum): GLHandleARB; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDetachObjectARB = procedure(container, attached: GLHandleARB); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCreateShaderObjectARB = function(shaderType: glenum): GLHandleARB; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglShaderSourceARB = procedure(shaderObj: GLHandleARB; count: glsizei; _string: PPGLCharARB; lengths: pglint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCompileShaderARB = procedure(shaderObj: GLHandleARB); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCreateProgramObjectARB = function: GLHandleARB; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglAttachObjectARB = procedure(programObj, shaderObj: GLhandleARB); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglLinkProgramARB = procedure(programObj: GLHandleARB); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUseProgramObjectARB = procedure(programObj: GLHandleARB); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglValidateProgramARB = procedure(programObj: GLhandleARB); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform1fARB = procedure(location: glint; v0: glfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform2fARB = procedure(location: glint; v0, v1: glfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform3fARB = procedure(location: glint; v0, v1, v2: glfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform4fARB = procedure(location: glint; v0, v1, v2, v3: glfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform1iARB = procedure(location: glint; v0: glint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform2iARB = procedure(location: glint; v0, v1: glint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform3iARB = procedure(location: glint; v0, v1, v2: glint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform4iARB = procedure(location: glint; v0, v1, v2, v3: glint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform1fvARB = procedure(location: glint; count: GLsizei; value: pglfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform2fvARB = procedure(location: glint; count: GLsizei; value: pglfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform3fvARB = procedure(location: glint; count: GLsizei; value: pglfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform4fvARB = procedure(location: glint; count: GLsizei; value: pglfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform1ivARB = procedure(location: glint; count: GLsizei; value: pglint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform2ivARB = procedure(location: glint; count: GLsizei; value: pglint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform3ivARB = procedure(location: glint; count: GLsizei; value: pglint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform4ivARB = procedure(location: glint; count: GLsizei; value: pglint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniformMatrix2fvARB = procedure(location: glint; count: glsizei; transpose: glboolean; value: pglfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniformMatrix3fvARB = procedure(location: glint; count: glsizei; transpose: glboolean; value: pglfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniformMatrix4fvARB = procedure(location: glint; count: glsizei; transpose: glboolean; value: pglfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetObjectParameterfvARB = procedure(Obj: GLHandleARB; pname: GLEnum; params: PGLFloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetObjectParameterivARB = procedure(Obj: GLHandleARB; pname: GLEnum; params: PGLInt); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetInfoLogARB = procedure(shaderObj: GLHandleARB; maxLength: glsizei; var length: glint; infoLog: PChar); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetAttachedObjectsARB = procedure(programobj: GLhandleARB; maxCount: GLsizei; var count: GLsizei; objects: PGLhandleARB); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetUniformLocationARB = function(programObj: GLhandleARB; const char: PChar): glint; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetActiveUniformARB = procedure(programobj: GLhandleARB; index: GLuint; maxLength: GLsizei; var length: GLsizei; var size: GLint; var _type: GLenum; name: PChar); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetUniformfvARB = procedure(programObj: GLhandleARB; location: GLint; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetUniformivARB = procedure(programObj: GLhandleARB; location: GLint; params: PGLInt); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetShaderSourceARB = procedure(shader: GLhandleARB; maxLength: GLsizei; var length: GLsizei; source: PChar); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_ARB_Occlusion_Query
  TglGenQueriesARB = procedure(n: GLsizei; ids: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDeleteQueriesARB = procedure(n: GLsizei; const ids: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIsQueryARB = function(id: GLuint): boolean; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBeginQueryARB = procedure(target: GLenum; id: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglEndQueryARB = procedure(target: GLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetQueryivARB = procedure(target, pname: GLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetQueryObjectivARB = procedure(id: GLuint; pname: GLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetQueryObjectuivARB = procedure(id: GLuint; pname: GLenum; params: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_ARB_draw_instanced
  TglDrawArraysInstancedARB = procedure(mode: GLenum; first: GLint; count: GLsizei; primcount: GLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDrawElementsInstancedARB = procedure(mode: GLenum; count: GLsizei; type_: GLenum; const indices: PGLvoid; primcount: GLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_ARB_framebuffer_object
  TglIsRenderbuffer = function(renderbuffer: GLuint): GLboolean; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBindRenderbuffer = procedure(target: GLenum; renderbuffer: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDeleteRenderbuffers = procedure(n: GLsizei; const renderbuffers: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGenRenderbuffers = procedure(n: GLsizei; renderbuffers: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRenderbufferStorage = procedure(target: GLenum; internalformat: GLenum; width: GLsizei; height: GLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetRenderbufferParameteriv = procedure(target: GLenum; pname: GLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIsFramebuffer = function(framebuffer: GLuint): GLboolean; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBindFramebuffer = procedure(target: GLenum; framebuffer: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDeleteFramebuffers = procedure(n: GLsizei; const framebuffers: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGenFramebuffers = procedure(n: GLsizei; framebuffers: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCheckFramebufferStatus = function(target: GLenum): GLenum; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFramebufferTexture1D = procedure(target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFramebufferTexture2D = procedure(target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFramebufferTexture3D = procedure(target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint; zoffset: GLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFramebufferRenderbuffer = procedure(target: GLenum; attachment: GLenum; renderbuffertarget: GLenum; renderbuffer: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetFramebufferAttachmentParameteriv = procedure(target: GLenum; attachment: GLenum; pname: GLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGenerateMipmap = procedure(target: GLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBlitFramebuffer = procedure(srcX0: GLint; srcY0: GLint; srcX1: GLint; srcY1: GLint; dstX0: GLint; dstY0: GLint; dstX1: GLint; dstY1: GLint; mask: GLbitfield; filter: GLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRenderbufferStorageMultisample = procedure(target: GLenum; samples: GLsizei; internalformat: GLenum; width: GLsizei; height: GLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFramebufferTextureLayer = procedure(target: GLenum; attachment: GLenum; texture: GLuint; level: GLint; layer: GLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_ARB_geometry_shader4
  TglProgramParameteriARB = procedure(program_: GLuint; pname: GLenum; value: GLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFramebufferTextureARB = procedure(target: GLenum; attachment: GLenum; texture: GLuint; level: GLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFramebufferTextureLayerARB = procedure(target: GLenum; attachment: GLenum; texture: GLuint; level: GLint; layer: GLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFramebufferTextureFaceARB = procedure(target: GLenum; attachment: GLenum; texture: GLuint; level: GLint; face: GLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_ARB_instanced_arrays
  TglVertexAttribDivisor = procedure(index_: GLuint; divisor: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_ARB_map_buffer_range
  TglMapBufferRange = procedure(target: GLenum; offset: GLsizei; length: GLsizei; access: GLbitfield); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFlushMappedBufferRange = procedure(target: GLenum; offset: GLsizei; length: GLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_ARB_texture_buffer_object
  TglTexBufferARB = procedure(target: GLenum; internalformat: GLenum; buffer: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_ARB_vertex_array_object
  TglBindVertexArray = procedure(array_: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDeleteVertexArrays = procedure(n: GLsizei; const arrays: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGenVertexArrays = procedure(n: GLsizei; arrays: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIsVertexArray = function(array_: GLuint): GLboolean; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_ATI_draw_buffers
  TglDrawBuffersATI = procedure(n: TGLsizei; const bufs: PGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_ATI_element_array
  TglElementPointerATI = procedure(_type: TGLenum; const _pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDrawElementArrayATI = procedure(mode: TGLenum; count: TGLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDrawRangeElementArrayATI = procedure(mode: TGLenum; start: TGLuint; _end: TGLuint; count: TGLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_ATI_envmap_bumpmap
  TglTexBumpParameterivATI = procedure(pname: TGLenum; const param: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexBumpParameterfvATI = procedure(pname: TGLenum; const param: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetTexBumpParameterivATI = procedure(pname: TGLenum; param: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetTexBumpParameterfvATI = procedure(pname: TGLenum; param: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_ATI_fragment_shader
  TglGenFragmentShadersATI = function(range: TGLuint): TGLuint; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBindFragmentShaderATI = procedure(id: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDeleteFragmentShaderATI = procedure(id: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBeginFragmentShaderATI = procedure(); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglEndFragmentShaderATI = procedure(); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPassTexCoordATI = procedure(dst: TGLuint; coord: TGLuint; swizzle: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSampleMapATI = procedure(dst: TGLuint; interp: TGLuint; swizzle: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColorFragmentOp1ATI = procedure(op: TGLenum; dst: TGLuint; dstMask: TGLuint; dstMod: TGLuint; arg1: TGLuint; arg1Rep: TGLuint; arg1Mod: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColorFragmentOp2ATI = procedure(op: TGLenum; dst: TGLuint; dstMask: TGLuint; dstMod: TGLuint; arg1: TGLuint; arg1Rep: TGLuint; arg1Mod: TGLuint; arg2: TGLuint; arg2Rep: TGLuint; arg2Mod: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColorFragmentOp3ATI = procedure(op: TGLenum; dst: TGLuint; dstMask: TGLuint; dstMod: TGLuint; arg1: TGLuint; arg1Rep: TGLuint; arg1Mod: TGLuint; arg2: TGLuint; arg2Rep: TGLuint; arg2Mod: TGLuint; arg3: TGLuint; arg3Rep: TGLuint; arg3Mod: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglAlphaFragmentOp1ATI = procedure(op: TGLenum; dst: TGLuint; dstMod: TGLuint; arg1: TGLuint; arg1Rep: TGLuint; arg1Mod: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglAlphaFragmentOp2ATI = procedure(op: TGLenum; dst: TGLuint; dstMod: TGLuint; arg1: TGLuint; arg1Rep: TGLuint; arg1Mod: TGLuint; arg2: TGLuint; arg2Rep: TGLuint; arg2Mod: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglAlphaFragmentOp3ATI = procedure(op: TGLenum; dst: TGLuint; dstMod: TGLuint; arg1: TGLuint; arg1Rep: TGLuint; arg1Mod: TGLuint; arg2: TGLuint; arg2Rep: TGLuint; arg2Mod: TGLuint; arg3: TGLuint; arg3Rep: TGLuint; arg3Mod: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSetFragmentShaderConstantATI = procedure(dst: TGLuint; const value: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_ATI_map_object_buffer
  TglMapObjectBufferATI = function(buffer: TGLuint): PGLvoid; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUnmapObjectBufferATI = procedure(buffer: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_ATI_pn_triangles
  TglPNTrianglesiATI = procedure(pname: TGLenum; param: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPNTrianglesfATI = procedure(pname: TGLenum; param: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_ATI_separate_stencil
  TglStencilOpSeparateATI = procedure(face: TGLenum; sfail: TGLenum; dpfail: TGLenum; dppass: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglStencilFuncSeparateATI = procedure(frontfunc: TGLenum; backfunc: TGLenum; ref: TGLint; mask: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_ATI_vertex_array_object
  TglNewObjectBufferATI = function(size: TGLsizei; const _pointer: PGLvoid; usage: TGLenum): TGLuint; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIsObjectBufferATI = function(buffer: TGLuint): TGLboolean; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUpdateObjectBufferATI = procedure(buffer: TGLuint; offset: TGLuint; size: TGLsizei; const _pointer: PGLvoid; preserve: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetObjectBufferfvATI = procedure(buffer: TGLuint; pname: TGLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetObjectBufferivATI = procedure(buffer: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFreeObjectBufferATI = procedure(buffer: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglArrayObjectATI = procedure(_array: TGLenum; size: TGLint; _type: TGLenum; stride: TGLsizei; buffer: TGLuint; offset: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetArrayObjectfvATI = procedure(_array: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetArrayObjectivATI = procedure(_array: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVariantArrayObjectATI = procedure(id: TGLuint; _type: TGLenum; stride: TGLsizei; buffer: TGLuint; offset: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetVariantArrayObjectfvATI = procedure(id: TGLuint; pname: TGLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetVariantArrayObjectivATI = procedure(id: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_ATI_vertex_attrib_array_object
  TglVertexAttribArrayObjectATI = procedure(index: TGLuint; size: TGLint; _type: TGLenum; normalized: TGLboolean; stride: TGLsizei; buffer: TGLuint; offset: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetVertexAttribArrayObjectfvATI = procedure(index: TGLuint; pname: TGLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetVertexAttribArrayObjectivATI = procedure(index: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_ATI_vertex_streams
  TglVertexStream1sATI = procedure(stream: TGLenum; x: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexStream1svATI = procedure(stream: TGLenum; const coords: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexStream1iATI = procedure(stream: TGLenum; x: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexStream1ivATI = procedure(stream: TGLenum; const coords: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexStream1fATI = procedure(stream: TGLenum; x: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexStream1fvATI = procedure(stream: TGLenum; const coords: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexStream1dATI = procedure(stream: TGLenum; x: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexStream1dvATI = procedure(stream: TGLenum; const coords: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexStream2sATI = procedure(stream: TGLenum; x: TGLshort; y: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexStream2svATI = procedure(stream: TGLenum; const coords: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexStream2iATI = procedure(stream: TGLenum; x: TGLint; y: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexStream2ivATI = procedure(stream: TGLenum; const coords: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexStream2fATI = procedure(stream: TGLenum; x: TGLfloat; y: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexStream2fvATI = procedure(stream: TGLenum; const coords: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexStream2dATI = procedure(stream: TGLenum; x: TGLdouble; y: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexStream2dvATI = procedure(stream: TGLenum; const coords: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexStream3sATI = procedure(stream: TGLenum; x: TGLshort; y: TGLshort; z: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexStream3svATI = procedure(stream: TGLenum; const coords: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexStream3iATI = procedure(stream: TGLenum; x: TGLint; y: TGLint; z: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexStream3ivATI = procedure(stream: TGLenum; const coords: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexStream3fATI = procedure(stream: TGLenum; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexStream3fvATI = procedure(stream: TGLenum; const coords: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexStream3dATI = procedure(stream: TGLenum; x: TGLdouble; y: TGLdouble; z: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexStream3dvATI = procedure(stream: TGLenum; const coords: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexStream4sATI = procedure(stream: TGLenum; x: TGLshort; y: TGLshort; z: TGLshort; w: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexStream4svATI = procedure(stream: TGLenum; const coords: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexStream4iATI = procedure(stream: TGLenum; x: TGLint; y: TGLint; z: TGLint; w: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexStream4ivATI = procedure(stream: TGLenum; const coords: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexStream4fATI = procedure(stream: TGLenum; x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexStream4fvATI = procedure(stream: TGLenum; const coords: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexStream4dATI = procedure(stream: TGLenum; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexStream4dvATI = procedure(stream: TGLenum; const coords: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNormalStream3bATI = procedure(stream: TGLenum; nx: TGLbyte; ny: TGLbyte; nz: TGLbyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNormalStream3bvATI = procedure(stream: TGLenum; const coords: PGLbyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNormalStream3sATI = procedure(stream: TGLenum; nx: TGLshort; ny: TGLshort; nz: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNormalStream3svATI = procedure(stream: TGLenum; const coords: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNormalStream3iATI = procedure(stream: TGLenum; nx: TGLint; ny: TGLint; nz: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNormalStream3ivATI = procedure(stream: TGLenum; const coords: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNormalStream3fATI = procedure(stream: TGLenum; nx: TGLfloat; ny: TGLfloat; nz: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNormalStream3fvATI = procedure(stream: TGLenum; const coords: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNormalStream3dATI = procedure(stream: TGLenum; nx: TGLdouble; ny: TGLdouble; nz: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNormalStream3dvATI = procedure(stream: TGLenum; const coords: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglClientActiveVertexStreamATI = procedure(stream: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexBlendEnviATI = procedure(pname: TGLenum; param: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexBlendEnvfATI = procedure(pname: TGLenum; param: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_blend_color
  TglBlendColorEXT = procedure(red: TGLclampf; green: TGLclampf; blue: TGLclampf; alpha: TGLclampf); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_blend_func_separate
  TglBlendFuncSeparateEXT = procedure(sfactorRGB: TGLenum; dfactorRGB: TGLenum; sfactorAlpha: TGLenum; dfactorAlpha: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_blend_minmax
  TglBlendEquationEXT = procedure(mode: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_color_subtable
  TglColorSubTableEXT = procedure(target: TGLenum; start: TGLsizei; count: TGLsizei; format: TGLenum; _type: TGLenum; const data: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCopyColorSubTableEXT = procedure(target: TGLenum; start: TGLsizei; x: TGLint; y: TGLint; width: TGLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_compiled_vertex_array
  TglLockArraysEXT = procedure(first: TGLint; count: TGLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUnlockArraysEXT = procedure(); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_convolution
  TglConvolutionFilter1DEXT = procedure(target: TGLenum; internalformat: TGLenum; width: TGLsizei; format: TGLenum; _type: TGLenum; const image: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglConvolutionFilter2DEXT = procedure(target: TGLenum; internalformat: TGLenum; width: TGLsizei; height: TGLsizei; format: TGLenum; _type: TGLenum; const image: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglConvolutionParameterfEXT = procedure(target: TGLenum; pname: TGLenum; params: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglConvolutionParameterfvEXT = procedure(target: TGLenum; pname: TGLenum; const params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglConvolutionParameteriEXT = procedure(target: TGLenum; pname: TGLenum; params: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglConvolutionParameterivEXT = procedure(target: TGLenum; pname: TGLenum; const params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCopyConvolutionFilter1DEXT = procedure(target: TGLenum; internalformat: TGLenum; x: TGLint; y: TGLint; width: TGLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCopyConvolutionFilter2DEXT = procedure(target: TGLenum; internalformat: TGLenum; x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetConvolutionFilterEXT = procedure(target: TGLenum; format: TGLenum; _type: TGLenum; image: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetConvolutionParameterfvEXT = procedure(target: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetConvolutionParameterivEXT = procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetSeparableFilterEXT = procedure(target: TGLenum; format: TGLenum; _type: TGLenum; row: PGLvoid; column: PGLvoid; span: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSeparableFilter2DEXT = procedure(target: TGLenum; internalformat: TGLenum; width: TGLsizei; height: TGLsizei; format: TGLenum; _type: TGLenum; const row: PGLvoid; const column: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_coordinate_frame
  TglTangent3bEXT = procedure(tx: TGLbyte; ty: TGLbyte; tz: TGLbyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTangent3bvEXT = procedure(const v: PGLbyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTangent3dEXT = procedure(tx: TGLdouble; ty: TGLdouble; tz: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTangent3dvEXT = procedure(const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTangent3fEXT = procedure(tx: TGLfloat; ty: TGLfloat; tz: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTangent3fvEXT = procedure(const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTangent3iEXT = procedure(tx: TGLint; ty: TGLint; tz: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTangent3ivEXT = procedure(const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTangent3sEXT = procedure(tx: TGLshort; ty: TGLshort; tz: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTangent3svEXT = procedure(const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBinormal3bEXT = procedure(bx: TGLbyte; by: TGLbyte; bz: TGLbyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBinormal3bvEXT = procedure(const v: PGLbyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBinormal3dEXT = procedure(bx: TGLdouble; by: TGLdouble; bz: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBinormal3dvEXT = procedure(const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBinormal3fEXT = procedure(bx: TGLfloat; by: TGLfloat; bz: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBinormal3fvEXT = procedure(const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBinormal3iEXT = procedure(bx: TGLint; by: TGLint; bz: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBinormal3ivEXT = procedure(const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBinormal3sEXT = procedure(bx: TGLshort; by: TGLshort; bz: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBinormal3svEXT = procedure(const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTangentPointerEXT = procedure(_type: TGLenum; stride: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBinormalPointerEXT = procedure(_type: TGLenum; stride: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_copy_texture
  TglCopyTexImage1DEXT = procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; x: TGLint; y: TGLint; width: TGLsizei; border: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCopyTexImage2DEXT = procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei; border: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCopyTexSubImage1DEXT = procedure(target: TGLenum; level: TGLint; xoffset: TGLint; x: TGLint; y: TGLint; width: TGLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCopyTexSubImage2DEXT = procedure(target: TGLenum; level: TGLint; xoffset: TGLint; yoffset: TGLint; x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCopyTexSubImage3DEXT = procedure(target: TGLenum; level: TGLint; xoffset: TGLint; yoffset: TGLint; zoffset: TGLint; x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_cull_vertex
  TglCullParameterdvEXT = procedure(pname: TGLenum; params: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCullParameterfvEXT = procedure(pname: TGLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_draw_range_elements
  TglDrawRangeElementsEXT = procedure(mode: TGLenum; start: TGLuint; _end: TGLuint; count: TGLsizei; _type: TGLenum; const indices: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_fog_coord
  TglFogCoordfEXT = procedure(coord: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFogCoordfvEXT = procedure(const coord: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFogCoorddEXT = procedure(coord: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFogCoorddvEXT = procedure(const coord: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFogCoordPointerEXT = procedure(_type: TGLenum; stride: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_framebuffer_object
  TglIsRenderbufferEXT = function(renderbuffer: TGLuint): Boolean; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBindRenderbufferEXT = procedure(target: TGLenum; renderbuffer: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDeleteRenderbuffersEXT = procedure(n: TGLsizei; const renderbuffers: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGenRenderbuffersEXT = procedure(n: TGLsizei; renderbuffers: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRenderbufferStorageEXT = procedure(target: TGLenum; internalformat: TGLenum; width: TGLsizei; height: TGLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetRenderbufferParameterivEXT = procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIsFramebufferEXT = function(framebuffer: TGLuint): Boolean; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBindFramebufferEXT = procedure(target: TGLenum; framebuffer: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDeleteFramebuffersEXT = procedure(n: TGLsizei; const framebuffers: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGenFramebuffersEXT = procedure(n: TGLsizei; framebuffers: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCheckFramebufferStatusEXT = function(target: TGLenum): TGLenum; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFramebufferTexture1DEXT = procedure(target: TGLenum; attachment: TGLenum; textarget: TGLenum; texture: TGLuint; level: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFramebufferTexture2DEXT = procedure(target: TGLenum; attachment: TGLenum; textarget: TGLenum; texture: TGLuint; level: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFramebufferTexture3DEXT = procedure(target: TGLenum; attachment: TGLenum; textarget: TGLenum; texture: TGLuint; level: TGLint; zoffset: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFramebufferRenderbufferEXT = procedure(target: TGLenum; attachment: TGLenum; renderbuffertarget: TGLenum; renderbuffer: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetFramebufferAttachmentParameterivEXT = procedure(target: TGLenum; attachment: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGenerateMipmapEXT = procedure(target: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_histogram
  TglGetHistogramEXT = procedure(target: TGLenum; reset: TGLboolean; format: TGLenum; _type: TGLenum; values: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetHistogramParameterfvEXT = procedure(target: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetHistogramParameterivEXT = procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetMinmaxEXT = procedure(target: TGLenum; reset: TGLboolean; format: TGLenum; _type: TGLenum; values: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetMinmaxParameterfvEXT = procedure(target: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetMinmaxParameterivEXT = procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglHistogramEXT = procedure(target: TGLenum; width: TGLsizei; internalformat: TGLenum; sink: TGLboolean); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMinmaxEXT = procedure(target: TGLenum; internalformat: TGLenum; sink: TGLboolean); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglResetHistogramEXT = procedure(target: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglResetMinmaxEXT = procedure(target: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_index_func
  TglIndexFuncEXT = procedure(func: TGLenum; ref: TGLclampf); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_index_material
  TglIndexMaterialEXT = procedure(face: TGLenum; mode: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_light_texture
  TglApplyTextureEXT = procedure(mode: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTextureLightEXT = procedure(pname: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTextureMaterialEXT = procedure(face: TGLenum; mode: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_multi_draw_arrays
  TglMultiDrawArraysEXT = procedure(mode: TGLenum; first: PGLint; count: PGLsizei; primcount: TGLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiDrawElementsEXT = procedure(mode: TGLenum; const count: PGLsizei; _type: TGLenum; const indices: PGLvoid; primcount: TGLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_multisample
  TglSampleMaskEXT = procedure(value: TGLclampf; invert: TGLboolean); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSamplePatternEXT = procedure(pattern: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_paletted_texture
  TglColorTableEXT = procedure(target: TGLenum; internalFormat: TGLenum; width: TGLsizei; format: TGLenum; _type: TGLenum; const table: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetColorTableEXT = procedure(target: TGLenum; format: TGLenum; _type: TGLenum; data: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetColorTableParameterivEXT = procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetColorTableParameterfvEXT = procedure(target: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_pixel_transform
  TglPixelTransformParameteriEXT = procedure(target: TGLenum; pname: TGLenum; param: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPixelTransformParameterfEXT = procedure(target: TGLenum; pname: TGLenum; param: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPixelTransformParameterivEXT = procedure(target: TGLenum; pname: TGLenum; const params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPixelTransformParameterfvEXT = procedure(target: TGLenum; pname: TGLenum; const params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_point_parameters
  TglPointParameterfEXT = procedure(pname: TGLenum; param: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPointParameterfvEXT = procedure(pname: TGLenum; const params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_polygon_offset
  TglPolygonOffsetEXT = procedure(factor: TGLfloat; bias: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_secondary_color
  TglSecondaryColor3bEXT = procedure(red: TGLbyte; green: TGLbyte; blue: TGLbyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSecondaryColor3bvEXT = procedure(const v: PGLbyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSecondaryColor3dEXT = procedure(red: TGLdouble; green: TGLdouble; blue: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSecondaryColor3dvEXT = procedure(const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSecondaryColor3fEXT = procedure(red: TGLfloat; green: TGLfloat; blue: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSecondaryColor3fvEXT = procedure(const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSecondaryColor3iEXT = procedure(red: TGLint; green: TGLint; blue: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSecondaryColor3ivEXT = procedure(const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSecondaryColor3sEXT = procedure(red: TGLshort; green: TGLshort; blue: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSecondaryColor3svEXT = procedure(const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSecondaryColor3ubEXT = procedure(red: TGLubyte; green: TGLubyte; blue: TGLubyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSecondaryColor3ubvEXT = procedure(const v: PGLubyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSecondaryColor3uiEXT = procedure(red: TGLuint; green: TGLuint; blue: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSecondaryColor3uivEXT = procedure(const v: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSecondaryColor3usEXT = procedure(red: TGLushort; green: TGLushort; blue: TGLushort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSecondaryColor3usvEXT = procedure(const v: PGLushort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSecondaryColorPointerEXT = procedure(size: TGLint; _type: TGLenum; stride: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_stencil_two_side
  TglActiveStencilFaceEXT = procedure(face: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_subtexture
  TglTexSubImage1DEXT = procedure(target: TGLenum; level: TGLint; xoffset: TGLint; width: TGLsizei; format: TGLenum; _type: TGLenum; const pixels: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexSubImage2DEXT = procedure(target: TGLenum; level: TGLint; xoffset: TGLint; yoffset: TGLint; width: TGLsizei; height: TGLsizei; format: TGLenum; _type: TGLenum; const pixels: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_texture3D
  TglTexImage3DEXT = procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; width: TGLsizei; height: TGLsizei; depth: TGLsizei; border: TGLint; format: TGLenum; _type: TGLenum; const pixels: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexSubImage3DEXT = procedure(target: TGLenum; level: TGLint; xoffset: TGLint; yoffset: TGLint; zoffset: TGLint; width: TGLsizei; height: TGLsizei; depth: TGLsizei; format: TGLenum; _type: TGLenum; const pixels: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_texture_object
  TglAreTexturesResidentEXT = function(n: TGLsizei; const textures: PGLuint; residences: PGLboolean): TGLboolean; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBindTextureEXT = procedure(target: TGLenum; texture: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDeleteTexturesEXT = procedure(n: TGLsizei; const textures: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGenTexturesEXT = procedure(n: TGLsizei; textures: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIsTextureEXT = function(texture: TGLuint): TGLboolean; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPrioritizeTexturesEXT = procedure(n: TGLsizei; const textures: PGLuint; const priorities: PGLclampf); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_texture_perturb_normal
  TglTextureNormalEXT = procedure(mode: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_vertex_array
  TglArrayElementEXT = procedure(i: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColorPointerEXT = procedure(size: TGLint; _type: TGLenum; stride: TGLsizei; count: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDrawArraysEXT = procedure(mode: TGLenum; first: TGLint; count: TGLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglEdgeFlagPointerEXT = procedure(stride: TGLsizei; count: TGLsizei; const _pointer: PGLboolean); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetPointervEXT = procedure(pname: TGLenum; params: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIndexPointerEXT = procedure(_type: TGLenum; stride: TGLsizei; count: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNormalPointerEXT = procedure(_type: TGLenum; stride: TGLsizei; count: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoordPointerEXT = procedure(size: TGLint; _type: TGLenum; stride: TGLsizei; count: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexPointerEXT = procedure(size: TGLint; _type: TGLenum; stride: TGLsizei; count: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_vertex_shader
  TglBeginVertexShaderEXT = procedure(); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglEndVertexShaderEXT = procedure(); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBindVertexShaderEXT = procedure(id: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGenVertexShadersEXT = function(range: TGLuint): TGLuint; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDeleteVertexShaderEXT = procedure(id: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglShaderOp1EXT = procedure(op: TGLenum; res: TGLuint; arg1: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglShaderOp2EXT = procedure(op: TGLenum; res: TGLuint; arg1: TGLuint; arg2: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglShaderOp3EXT = procedure(op: TGLenum; res: TGLuint; arg1: TGLuint; arg2: TGLuint; arg3: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSwizzleEXT = procedure(res: TGLuint; _in: TGLuint; outX: TGLenum; outY: TGLenum; outZ: TGLenum; outW: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWriteMaskEXT = procedure(res: TGLuint; _in: TGLuint; outX: TGLenum; outY: TGLenum; outZ: TGLenum; outW: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglInsertComponentEXT = procedure(res: TGLuint; src: TGLuint; num: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglExtractComponentEXT = procedure(res: TGLuint; src: TGLuint; num: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGenSymbolsEXT = function(datatype: TGLenum; storagetype: TGLenum; range: TGLenum; components: TGLuint): TGLuint; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSetInvariantEXT = procedure(id: TGLuint; _type: TGLenum; const addr: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSetLocalConstantEXT = procedure(id: TGLuint; _type: TGLenum; const addr: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVariantbvEXT = procedure(id: TGLuint; const addr: PGLbyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVariantsvEXT = procedure(id: TGLuint; const addr: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVariantivEXT = procedure(id: TGLuint; const addr: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVariantfvEXT = procedure(id: TGLuint; const addr: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVariantdvEXT = procedure(id: TGLuint; const addr: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVariantubvEXT = procedure(id: TGLuint; const addr: PGLubyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVariantusvEXT = procedure(id: TGLuint; const addr: PGLushort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVariantuivEXT = procedure(id: TGLuint; const addr: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVariantPointerEXT = procedure(id: TGLuint; _type: TGLenum; stride: TGLuint; const addr: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglEnableVariantClientStateEXT = procedure(id: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDisableVariantClientStateEXT = procedure(id: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBindLightParameterEXT = function(light: TGLenum; value: TGLenum): TGLuint; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBindMaterialParameterEXT = function(face: TGLenum; value: TGLenum): TGLuint; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBindTexGenParameterEXT = function(_unit: TGLenum; coord: TGLenum; value: TGLenum): TGLuint; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBindTextureUnitParameterEXT = function(_unit: TGLenum; value: TGLenum): TGLuint; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBindParameterEXT = function(value: TGLenum): TGLuint; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIsVariantEnabledEXT = function(id: TGLuint; cap: TGLenum): TGLboolean; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetVariantBooleanvEXT = procedure(id: TGLuint; value: TGLenum; data: PGLboolean); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetVariantIntegervEXT = procedure(id: TGLuint; value: TGLenum; data: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetVariantFloatvEXT = procedure(id: TGLuint; value: TGLenum; data: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetVariantPointervEXT = procedure(id: TGLuint; value: TGLenum; data: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetInvariantBooleanvEXT = procedure(id: TGLuint; value: TGLenum; data: PGLboolean); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetInvariantIntegervEXT = procedure(id: TGLuint; value: TGLenum; data: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetInvariantFloatvEXT = procedure(id: TGLuint; value: TGLenum; data: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetLocalConstantBooleanvEXT = procedure(id: TGLuint; value: TGLenum; data: PGLboolean); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetLocalConstantIntegervEXT = procedure(id: TGLuint; value: TGLenum; data: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetLocalConstantFloatvEXT = procedure(id: TGLuint; value: TGLenum; data: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_vertex_weighting
  TglVertexWeightfEXT = procedure(weight: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexWeightfvEXT = procedure(const weight: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexWeightPointerEXT = procedure(size: TGLsizei; _type: TGLenum; stride: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_stencil_clear_tag
  TglStencilClearTagEXT = procedure(stencilTagBits: TGLsizei; stencilClearTag: Tgluint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_framebuffer_blit
  TglBlitFramebufferEXT = procedure(srcX0: TGLint; srcY0: TGLint; srcX1: TGLint; srcY1: TGLint; dstX0: TGLint; dstY0: TGLint; dstX1: TGLint; dstY1: TGLint; mask: TGLbitfield; filter: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_framebuffer_multisample
  TglRenderbufferStorageMultisampleEXT = procedure(target: TGLenum; samples: TGLsizei; internalformat: TGLenum; width: TGLsizei; height: TGLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_timer_query
  TglGetQueryObjecti64vEXT = procedure(id: TGLuint; pname: TGLenum; params: PGLint64EXT); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetQueryObjectui64vEXT = procedure(id: TGLuint; pname: TGLenum; params: PGLuint64EXT); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_gpu_program_parameters
  TglProgramEnvParameters4fvEXT = procedure(target: TGLenum; index: TGLuint; count: TGLsizei; const params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramLocalParameters4fvEXT = procedure(target: TGLenum; index: TGLuint; count: TGLsizei; const params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_bindable_uniform
  TglUniformBufferEXT = procedure(_program: TGLuint; location: TGLint; buffer: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetUniformBufferSizeEXT = function(_program: TGLuint; location: TGLint): TGLint; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetUniformOffsetEXT = function(_program: TGLuint; location: TGLint): PGLInt; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_draw_buffers2
  TglColorMaskIndexedEXT = procedure(buf: TGLuint; r: TGLboolean; g: TGLboolean; b: TGLboolean; a: TGLboolean); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetBooleanIndexedvEXT = procedure(value: TGLenum; index: TGLuint; data: PGLboolean); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetIntegerIndexedvEXT = procedure(value: TGLenum; index: TGLuint; data: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglEnableIndexedEXT = procedure(target: TGLenum; index: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDisableIndexedEXT = procedure(target: TGLenum; index: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIsEnabledIndexedEXT = function(target: TGLenum; index: TGLuint): TGLboolean; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_draw_instanced
  TglDrawArraysInstancedEXT = procedure(mode: TGLenum; first: TGLint; count: TGLsizei; primcount: TGLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDrawElementsInstancedEXT = procedure(mode: TGLenum; count: TGLsizei; _type: TGLenum; const indices: Pointer; primcount: TGLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_geometry_shader4
  TglProgramParameteriEXT = procedure (_program: TGLuint; pname: TGLenum; value: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFramebufferTextureEXT = procedure(target: TGLenum; attachment: TGLenum; texture: TGLuint; level: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
//  TglFramebufferTextureLayerEXT = procedure(target: TGLenum; attachment: TGLenum; texture: TGLuint; level: TGLint; layer: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFramebufferTextureFaceEXT = procedure(target: TGLenum; attachment: TGLenum; texture: TGLuint; level: TGLint; face: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_gpu_shader4
  TglVertexAttribI1iEXT = procedure(index: TGLuint; x: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI2iEXT = procedure(index: TGLuint; x: TGLint; y: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI3iEXT = procedure(index: TGLuint; x: TGLint; y: TGLint; z: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI4iEXT = procedure(index: TGLuint; x: TGLint; y: TGLint; z: TGLint; w: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI1uiEXT = procedure(index: TGLuint; x: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI2uiEXT = procedure(index: TGLuint; x: TGLuint; y: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI3uiEXT = procedure(index: TGLuint; x: TGLuint; y: TGLuint; z: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI4uiEXT = procedure(index: TGLuint; x: TGLuint; y: TGLuint; z: TGLuint; w: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI1ivEXT = procedure(index: TGLuint; const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI2ivEXT = procedure(index: TGLuint; const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI3ivEXT = procedure(index: TGLuint; const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI4ivEXT = procedure(index: TGLuint; const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI1uivEXT = procedure(index: TGLuint; const v: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI2uivEXT = procedure(index: TGLuint; const v: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI3uivEXT = procedure(index: TGLuint; const v: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI4uivEXT = procedure(index: TGLuint; const v: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI4bvEXT = procedure(index: TGLuint; const v: PGLbyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI4svEXT = procedure(index: TGLuint; const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI4ubvEXT = procedure(index: TGLuint; const v: PGLubyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribI4usvEXT = procedure(index: TGLuint; const v: PGLushort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribIPointerEXT = procedure(index: TGLuint; size: TGLint; _type: TGLenum; stride: TGLsizei; const _pointer: Pointer); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetVertexAttribIivEXT = procedure(index: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetVertexAttribIuivEXT = procedure(index: TGLuint; pname: TGLenum; params: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform1uiEXT = procedure(location: TGLint; v0: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform2uiEXT = procedure(location: TGLint; v0: TGLuint; v1: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform3uiEXT = procedure(location: TGLint; v0: TGLuint; v1: TGLuint; v2: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform4uiEXT = procedure(location: TGLint; v0: TGLuint; v1: TGLuint; v2: TGLuint; v3: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform1uivEXT = procedure(location: TGLint; count: TGLsizei; const value: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform2uivEXT = procedure(location: TGLint; count: TGLsizei; const value: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform3uivEXT = procedure(location: TGLint; count: TGLsizei; const value: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUniform4uivEXT = procedure(location: TGLint; count: TGLsizei; const value: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetUniformuivEXT = procedure(_program: TGLuint; location: TGLint; params: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBindFragDataLocationEXT = procedure(_program: TGLuint; colorNumber: TGLuint; const name: PChar); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetFragDataLocationEXT = function(_program: TGLuint; const name: PChar): TGLint;

  // GL_EXT_texture_array
  TglFramebufferTextureLayerEXT = procedure(target: TGLenum; attachment: TGLenum; texture: TGLuint; level: TGLint; layer: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_texture_buffer_object
  TglTexBufferEXT = procedure(target: TGLenum; internalformat: TGLenum; buffer: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_texture_integer
  TglClearColorIiEXT = procedure(r: TGLint; g: TGLint; b: TGLint; a: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglClearColorIuiEXT = procedure(r: TGLuint; g: TGLuint; b: TGLuint; a: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexParameterIivEXT = procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexParameterIuivEXT = procedure(target: TGLenum; pname: TGLenum; params: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetTexParameterIivEXT = procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetTexParameterIiuvEXT = procedure(target: TGLenum; pname: TGLenum; params: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_HP_image_transform
  TglImageTransformParameteriHP = procedure(target: TGLenum; pname: TGLenum; param: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglImageTransformParameterfHP = procedure(target: TGLenum; pname: TGLenum; param: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglImageTransformParameterivHP = procedure(target: TGLenum; pname: TGLenum; const params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglImageTransformParameterfvHP = procedure(target: TGLenum; pname: TGLenum; const params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetImageTransformParameterivHP = procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetImageTransformParameterfvHP = procedure(target: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_depth_bounds_test
  TglDepthBoundsEXT = procedure(zmin: TGLclampd; zmax: TGLclampd); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_blend_equation_separate
  TglBlendEquationSeparateEXT = procedure(modeRGB: TGLenum; modeAlpha: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_transform_feedback
  TglBeginTransformFeedbackEXT = procedure(primitiveMode: GLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglEndTransformFeedbackEXT = procedure(); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBindBufferRangeEXT = procedure(target: GLenum; index_: GLuint; buffer: GLuint; offset: GLsizei; size: GLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBindBufferOffsetEXT = procedure(target: GLenum; index_: GLuint; buffer: GLuint; offset: GLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBindBufferBaseEXT = procedure(target: GLenum; index_: GLuint; buffer: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTransformFeedbackVaryingsEXT = procedure(program_: GLuint; count: GLsizei; const locations: PGLint; bufferMode: GLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetTransformFeedbackVaryingEXT = procedure(program_: GLuint; index_: GLuint; location: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_EXT_direct_state_access
  TglClientAttribDefaultEXT = procedure(mask: GLbitfield); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPushClientAttribDefaultEXT = procedure(mask: GLbitfield); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMatrixLoadfEXT = procedure(mode: GLenum; const m: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMatrixLoaddEXT = procedure(mode: GLenum; const m: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMatrixMultfEXT = procedure(mode: GLenum; const m: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMatrixMultdEXT = procedure(mode: GLenum; const m: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMatrixLoadIdentityEXT = procedure(mode: GLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMatrixRotatefEXT = procedure(mode: GLenum; angle: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMatrixRotatedEXT = procedure(mode: GLenum; angle: GLdouble; x: GLdouble; y: GLdouble; z: GLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMatrixScalefEXT = procedure(mode: GLenum; x: GLfloat; y: GLfloat; z: GLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMatrixScaledEXT = procedure(mode: GLenum; x: GLdouble; y: GLdouble; z: GLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMatrixTranslatefEXT = procedure(mode: GLenum; x: GLfloat; y: GLfloat; z: GLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMatrixTranslatedEXT = procedure(mode: GLenum; x: GLdouble; y: GLdouble; z: GLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMatrixFrustumEXT = procedure(mode: GLenum; left: GLdouble; right: GLdouble; bottom: GLdouble; top: GLdouble; zNear: GLdouble; zFar: GLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMatrixOrthoEXT = procedure(mode: GLenum; left: GLdouble; right: GLdouble; bottom: GLdouble; top: GLdouble; zNear: GLdouble; zFar: GLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMatrixPopEXT = procedure(mode: GLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMatrixPushEXT = procedure(mode: GLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMatrixLoadTransposefEXT = procedure(mode: GLenum; const m: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMatrixLoadTransposedEXT = procedure(mode: GLenum; const m: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMatrixMultTransposefEXT = procedure(mode: GLenum; const m: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMatrixMultTransposedEXT = procedure(mode: GLenum; const m: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTextureParameterfEXT = procedure(texture: GLuint; target: GLenum; pname: GLenum; param: GLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTextureParameterfvEXT = procedure(texture: GLuint; target: GLenum; pname: GLenum; const params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTextureParameteriEXT = procedure(texture: GLuint; target: GLenum; pname: GLenum; param: GLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTextureParameterivEXT = procedure(texture: GLuint; target: GLenum; pname: GLenum; const params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTextureImage1DEXT = procedure(texture: GLuint; target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; border: GLint; format: GLenum; type_: GLenum; const pixels: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTextureImage2DEXT = procedure(texture: GLuint; target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; border: GLint; format: GLenum; type_: GLenum; const pixels: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTextureSubImage1DEXT = procedure(texture: GLuint; target: GLenum; level: GLint; xoffset: GLint; width: GLsizei; format: GLenum; type_: GLenum; const pixels: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTextureSubImage2DEXT = procedure(texture: GLuint; target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; width: GLsizei; height: GLsizei; format: GLenum; type_: GLenum; const pixels: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCopyTextureImage1DEXT = procedure(texture: GLuint; target: GLenum; level: GLint; internalformat: GLenum; x: GLint; y: GLint; width: GLsizei; border: GLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCopyTextureImage2DEXT = procedure(texture: GLuint; target: GLenum; level: GLint; internalformat: GLenum; x: GLint; y: GLint; width: GLsizei; height: GLsizei; border: GLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCopyTextureSubImage1DEXT = procedure(texture: GLuint; target: GLenum; level: GLint; xoffset: GLint; x: GLint; y: GLint; width: GLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCopyTextureSubImage2DEXT = procedure(texture: GLuint; target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; x: GLint; y: GLint; width: GLsizei; height: GLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetTextureImageEXT = procedure(texture: GLuint; target: GLenum; level: GLint; format: GLenum; type_: GLenum; pixels: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetTextureParameterfvEXT = procedure(texture: GLuint; target: GLenum; pname: GLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetTextureParameterivEXT = procedure(texture: GLuint; target: GLenum; pname: GLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetTextureLevelParameterfvEXT = procedure(texture: GLuint; target: GLenum; level: GLint; pname: GLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetTextureLevelParameterivEXT = procedure(texture: GLuint; target: GLenum; level: GLint; pname: GLenum; params: GLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTextureImage3DEXT = procedure(texture: GLuint; target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; depth: GLsizei; border: GLint; format: GLenum; type_: GLenum; const pixels: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTextureSubImage3DEXT = procedure(texture: GLuint; target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; format: GLenum; type_: GLenum; const pixels: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCopyTextureSubImage3DEXT = procedure(texture: GLuint; target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; x: GLint; y: GLint; width: GLsizei; height: GLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexParameterfEXT = procedure(texunit: GLenum; target: GLenum; pname: GLenum; param: GLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexParameterfvEXT = procedure(texunit: GLenum; target: GLenum; pname: GLenum; const params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexParameteriEXT = procedure(texunit: GLenum; target: GLenum; pname: GLenum; param: GLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexParameterivEXT = procedure(texunit: GLenum; target: GLenum; pname: GLenum; const params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexImage1DEXT = procedure(texunit: GLenum; target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; border: GLint; format: GLenum; type_: GLenum; const pixels: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexImage2DEXT = procedure(texunit: GLenum; target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; border: GLint; format: GLenum; type_: GLenum; const pixels: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexSubImage1DEXT = procedure(texunit: GLenum; target: GLenum; level: GLint; xoffset: GLint; width: GLsizei; format: GLenum; type_: GLenum; const pixels: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexSubImage2DEXT = procedure(texunit: GLenum; target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; width: GLsizei; height: GLsizei; format: GLenum; type_: GLenum; const pixels: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCopyMultiTexImage1DEXT = procedure(texunit: GLenum; target: GLenum; level: GLint; internalformat: GLenum; x: GLint; y: GLint; width: GLsizei; border: GLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCopyMultiTexImage2DEXT = procedure(texunit: GLenum; target: GLenum; level: GLint; internalformat: GLenum; x: GLint; y: GLint; width: GLsizei; height: GLsizei; border: GLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCopyMultiTexSubImage1DEXT = procedure(texunit: GLenum; target: GLenum; level: GLint; xoffset: GLint; x: GLint; y: GLint; width: GLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCopyMultiTexSubImage2DEXT = procedure(texunit: GLenum; target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; x: GLint; y: GLint; width: GLsizei; height: GLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetMultiTexImageEXT = procedure(texunit: GLenum; target: GLenum; level: GLint; format: GLenum; type_: GLenum; pixels: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetMultiTexParameterfvEXT = procedure(texunit: GLenum; target: GLenum; pname: GLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetMultiTexParameterivEXT = procedure(texunit: GLenum; target: GLenum; pname: GLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetMultiTexLevelParameterfvEXT = procedure(texunit: GLenum; target: GLenum; level: GLint; pname: GLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetMultiTexLevelParameterivEXT = procedure(texunit: GLenum; target: GLenum; level: GLint; pname: GLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexImage3DEXT = procedure(texunit: GLenum; target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; depth: GLsizei; border: GLint; format: GLenum; type_: GLenum; const pixels: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexSubImage3DEXT = procedure(texunit: GLenum; target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; format: GLenum; type_: GLenum; const pixels:PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCopyMultiTexSubImage3DEXT = procedure(texunit: GLenum; target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; x: GLint; y: GLint; width: GLsizei; height: GLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBindMultiTextureEXT = procedure(texunit: GLenum; target: GLenum; texture: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglEnableClientStateIndexedEXT = procedure(array_: GLenum; index_: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDisableClientStateIndexedEXT = procedure(array_: GLenum; index_: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoordPointerEXT = procedure(texunit: GLenum; size: GLint; type_: GLenum; stride: GLsizei; const pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexEnvfEXT = procedure(texunit: GLenum; target: GLenum; pname: GLenum; param: GLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexEnvfvEXT = procedure(texunit: GLenum; target: GLenum; pname: GLenum; const params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexEnviEXT = procedure(texunit: GLenum; target: GLenum; pname: GLenum; param: GLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexEnvivEXT = procedure(texunit: GLenum; target: GLenum; pname: GLenum; const params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexGendEXT = procedure(texunit: GLenum; target: GLenum; pname: GLenum; param: GLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexGendvEXT = procedure(texunit: GLenum; target: GLenum; pname: GLenum; const params: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexGenfEXT = procedure(texunit: GLenum; target: GLenum; pname: GLenum; param: GLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexGenfvEXT = procedure(texunit: GLenum; target: GLenum; pname: GLenum; const params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexGeniEXT = procedure(texunit: GLenum; target: GLenum; pname: GLenum; param: GLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexGenivEXT = procedure(texunit: GLenum; target: GLenum; pname: GLenum; const params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetMultiTexEnvfvEXT = procedure(texunit: GLenum; target: GLenum; pname: GLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetMultiTexEnvivEXT = procedure(texunit: GLenum; target: GLenum; pname: GLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetMultiTexGendvEXT = procedure(texunit: GLenum; coord: GLenum; pname: GLenum; params: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetMultiTexGenfvEXT = procedure(texunit: GLenum; coord: GLenum; pname: GLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetMultiTexGenivEXT = procedure(texunit: GLenum; coord: GLenum; pname: GLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetFloatIndexedvEXT = procedure(target: GLenum; index_: GLuint; data: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetDoubleIndexedvEXT = procedure(target: GLenum; index_: GLuint; data: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetPointerIndexedvEXT = procedure(target: GLenum; index_: GLuint; data: PPGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCompressedTextureImage3DEXT = procedure(texture: GLuint; target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; depth: GLsizei; border: GLint; imageSize: GLsizei; const bits: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCompressedTextureImage2DEXT = procedure(texture: GLuint; target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; border: GLint; imageSize: GLsizei; const bits: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCompressedTextureImage1DEXT = procedure(texture: GLuint; target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; border: GLint; imageSize: GLsizei; const bits: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCompressedTextureSubImage3DEXT = procedure(texture: GLuint; target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; format: GLenum; imageSize: GLsizei; const bits: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCompressedTextureSubImage2DEXT = procedure(texture: GLuint; target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; width: GLsizei; height: GLsizei; format: GLenum; imageSize: GLsizei; const bits: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCompressedTextureSubImage1DEXT = procedure(texture: GLuint; target: GLenum; level: GLint; xoffset: GLint; width: GLsizei; format: GLenum; imageSize: GLsizei; const bits: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetCompressedTextureImageEXT = procedure(texture: GLuint; target: GLenum; lod: GLint; img: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCompressedMultiTexImage3DEXT = procedure(texunit: GLenum; target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; depth: GLsizei; border: GLint; imageSize: GLsizei; const bits: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCompressedMultiTexImage2DEXT = procedure(texunit: GLenum; target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; border: GLint; imageSize: GLsizei; const bits: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCompressedMultiTexImage1DEXT = procedure(texunit: GLenum; target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; border: GLint; imageSize: GLsizei; const bits: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCompressedMultiTexSubImage3DEXT = procedure(texunit: GLenum; target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; zoffset: GLint; width: GLsizei; height: GLsizei; depth: GLsizei; format: GLenum; imageSize: GLsizei; const bits: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCompressedMultiTexSubImage2DEXT = procedure(texunit: GLenum; target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; width: GLsizei; height: GLsizei; format: GLenum; imageSize: GLsizei; const bits: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCompressedMultiTexSubImage1DEXT = procedure(texunit: GLenum; target: GLenum; level: GLint; xoffset: GLint; width: GLsizei; format: GLenum; imageSize: GLsizei; const bits: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetCompressedMultiTexImageEXT = procedure(texunit: GLenum; target: GLenum; lod: GLint; img: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNamedProgramStringEXT = procedure(program_: GLuint; target: GLenum; format: GLenum; len: GLsizei; const string_: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNamedProgramLocalParameter4dEXT = procedure(program_: GLuint; target: GLenum; index_: GLuint; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNamedProgramLocalParameter4dvEXT = procedure(program_: GLuint; target: GLenum; index_: GLuint; const params: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNamedProgramLocalParameter4fEXT = procedure(program_: GLuint; target: GLenum; index_: GLuint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNamedProgramLocalParameter4fvEXT = procedure(program_: GLuint; target: GLenum; index_: GLuint; const params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetNamedProgramLocalParameterdvEXT = procedure(program_: GLuint; target: GLenum; index_: GLuint; params: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetNamedProgramLocalParameterfvEXT = procedure(program_: GLuint; target: GLenum; index_: GLuint; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetNamedProgramivEXT = procedure(program_: GLuint; target: GLenum; pname: GLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetNamedProgramStringEXT = procedure(program_: GLuint; target: GLenum; pname: GLenum; string_: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNamedProgramLocalParameters4fvEXT = procedure(program_: GLuint; target: GLenum; index_: GLuint; count: GLsizei; const params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNamedProgramLocalParameterI4iEXT = procedure(program_: GLuint; target: GLenum; index_: GLuint; x: GLint; y: GLint; z: GLint; w: GLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNamedProgramLocalParameterI4ivEXT = procedure(program_: GLuint; target: GLenum; index_: GLuint; const params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNamedProgramLocalParametersI4ivEXT = procedure(program_: GLuint; target: GLenum; index_: GLuint; count: GLsizei; const params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNamedProgramLocalParameterI4uiEXT = procedure(program_: GLuint; target: GLenum; index_: GLuint; x: GLuint; y: GLuint; z: GLuint; w: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNamedProgramLocalParameterI4uivEXT = procedure(program_: GLuint; target: GLenum; index_: GLuint; const params: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNamedProgramLocalParametersI4uivEXT = procedure(program_: GLuint; target: GLenum; index_: GLuint; count: GLsizei; const params: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetNamedProgramLocalParameterIivEXT = procedure(program_: GLuint; target: GLenum; index_: GLuint; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetNamedProgramLocalParameterIuivEXT = procedure(program_: GLuint; target: GLenum; index_: GLuint; params: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTextureParameterIivEXT = procedure(texture: GLuint; target: GLenum; pname: GLenum; const params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTextureParameterIuivEXT = procedure(texture: GLuint; target: GLenum; pname: GLenum; const params: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetTextureParameterIivEXT = procedure(texture: GLuint; target: GLenum; pname: GLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetTextureParameterIuivEXT = procedure(texture: GLuint; target: GLenum; pname: GLenum; params: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexParameterIivEXT = procedure(texture: GLuint; target: GLenum; pname: GLenum; const params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexParameterIuivEXT = procedure(texture: GLuint; target: GLenum; pname: GLenum; const params: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetMultiTexParameterIivEXT = procedure(texture: GLuint; target: GLenum; pname: GLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetMultiTexParameterIuivEXT = procedure(texture: GLuint; target: GLenum; pname: GLenum; params: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramUniform1fEXT = procedure(program_: GLuint; location: GLint; v0: GLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramUniform2fEXT = procedure(program_: GLuint; location: GLint; v0: GLfloat; v1: GLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramUniform3fEXT = procedure(program_: GLuint; location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramUniform4fEXT = procedure(program_: GLuint; location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat; v3: GLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramUniform1iEXT = procedure(program_: GLuint; location: GLint; v0: GLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramUniform2iEXT = procedure(program_: GLuint; location: GLint; v0: GLint; v1: GLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramUniform3iEXT = procedure(program_: GLuint; location: GLint; v0: GLint; v1: GLint; v2: GLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramUniform4iEXT = procedure(program_: GLuint; location: GLint; v0: GLint; v1: GLint; v2: GLint; v3: GLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramUniform1fvEXT = procedure(program_: GLuint; location: GLint; count: GLsizei; const value: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramUniform2fvEXT = procedure(program_: GLuint; location: GLint; count: GLsizei; const value: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramUniform3fvEXT = procedure(program_: GLuint; location: GLint; count: GLsizei; const value: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramUniform4fvEXT = procedure(program_: GLuint; location: GLint; count: GLsizei; const value: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramUniform1ivEXT = procedure(program_: GLuint; location: GLint; count: GLsizei; const value: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramUniform2ivEXT = procedure(program_: GLuint; location: GLint; count: GLsizei; const value: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramUniform3ivEXT = procedure(program_: GLuint; location: GLint; count: GLsizei; const value: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramUniform4ivEXT = procedure(program_: GLuint; location: GLint; count: GLsizei; const value: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramUniformMatrix2fvEXT = procedure(program_: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramUniformMatrix3fvEXT = procedure(program_: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramUniformMatrix4fvEXT = procedure(program_: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramUniformMatrix2x3fvEXT = procedure(program_: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramUniformMatrix3x2fvEXT = procedure(program_: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramUniformMatrix2x4fvEXT = procedure(program_: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramUniformMatrix4x2fvEXT = procedure(program_: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramUniformMatrix3x4fvEXT = procedure(program_: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramUniformMatrix4x3fvEXT = procedure(program_: GLuint; location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramUniform1uiEXT = procedure(program_: GLuint; location: GLint; v0: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramUniform2uiEXT = procedure(program_: GLuint; location: GLint; v0: GLuint; v1: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramUniform3uiEXT = procedure(program_: GLuint; location: GLint; v0: GLuint; v1: GLuint; v2: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramUniform4uiEXT = procedure(program_: GLuint; location: GLint; v0: GLuint; v1: GLuint; v2: GLuint; v3: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramUniform1uivEXT = procedure(program_: GLuint; location: GLint; count: GLsizei; const value: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramUniform2uivEXT = procedure(program_: GLuint; location: GLint; count: GLsizei; const value: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramUniform3uivEXT = procedure(program_: GLuint; location: GLint; count: GLsizei; const value: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramUniform4uivEXT = procedure(program_: GLuint; location: GLint; count: GLsizei; const value: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNamedBufferDataEXT = procedure(buffer: GLuint; size: GLsizei; const data: PGLvoid; usage: GLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNamedBufferSubDataEXT = procedure(buffer: GLuint; offset: GLsizei; size: GLsizei; const data: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMapNamedBufferEXT = function(buffer: GLuint; access: GLenum): PGLvoid; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglUnmapNamedBufferEXT = function(buffer: GLuint): GLboolean; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetNamedBufferParameterivEXT = procedure(buffer: GLuint; pname: GLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetNamedBufferPointervEXT = procedure(buffer: GLuint; pname: GLenum; params: PPGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetNamedBufferSubDataEXT = procedure(buffer: GLuint; offset: GLsizei; size: GLsizei; data: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTextureBufferEXT = procedure(texture: GLuint; target: GLenum; internalformat: GLenum; buffer: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexBufferEXT = procedure(texunit: GLenum; target: GLenum; interformat: GLenum; buffer: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNamedRenderbufferStorageEXT = procedure(renderbuffer: GLuint; interformat: GLenum; width: GLsizei; height: GLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetNamedRenderbufferParameterivEXT = procedure(renderbuffer: GLuint; pname: GLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCheckNamedFramebufferStatusEXT = function(framebuffer: GLuint; target: GLenum): GLenum; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNamedFramebufferTexture1DEXT = procedure(framebuffer: GLuint; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNamedFramebufferTexture2DEXT = procedure(framebuffer: GLuint; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNamedFramebufferTexture3DEXT = procedure(framebuffer: GLuint; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint; zoffset: GLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNamedFramebufferRenderbufferEXT = procedure(framebuffer: GLuint; attachment: GLenum; renderbuffertarget: GLenum; renderbuffer: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetNamedFramebufferAttachmentParameterivEXT = procedure(framebuffer: GLuint; attachment: GLenum; pname: GLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGenerateTextureMipmapEXT = procedure(texture: GLuint; target: GLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGenerateMultiTexMipmapEXT = procedure(texunit: GLenum; target: GLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFramebufferDrawBufferEXT = procedure(framebuffer: GLuint; mode: GLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFramebufferDrawBuffersEXT = procedure(framebuffer: GLuint; n: GLsizei; const bufs: PGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFramebufferReadBufferEXT = procedure(framebuffer: GLuint; mode: GLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetFramebufferParameterivEXT = procedure(framebuffer: GLuint; pname: GLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNamedRenderbufferStorageMultisampleEXT = procedure(renderbuffer: GLuint; samples: GLsizei; internalformat: GLenum; width: GLsizei; height: GLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNamedRenderbufferStorageMultisampleCoverageEXT = procedure(renderbuffer: GLuint; coverageSamples: GLsizei; colorSamples: GLsizei; internalformat: GLenum; width: GLsizei; height: GLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNamedFramebufferTextureEXT = procedure(framebuffer: GLuint; attachment: GLenum; texture: GLuint; level: GLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNamedFramebufferTextureLayerEXT = procedure(framebuffer: GLuint; attachment: GLenum; texture: GLuint; level: GLint; layer: GLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNamedFramebufferTextureFaceEXT = procedure(framebuffer: GLuint; attachment: GLenum; texture: GLuint; level: GLint; face: GLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTextureRenderbufferEXT = procedure(texture: GLuint; target: GLenum; renderbuffer: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexRenderbufferEXT = procedure(texunit: GLenum; target: GLenum; renderbuffer: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_IBM_multimode_draw_arrays
  TglMultiModeDrawArraysIBM = procedure(mode: TGLenum; const first: PGLint; const count: PGLsizei; primcount: TGLsizei; modestride: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiModeDrawElementsIBM = procedure(const mode: PGLenum; const count: PGLsizei; _type: TGLenum; const indices: PGLvoid; primcount: TGLsizei; modestride: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_IBM_vertex_array_lists
  TglColorPointerListIBM = procedure(size: TGLint; _type: TGLenum; stride: TGLint; const _pointer: PGLvoid; ptrstride: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSecondaryColorPointerListIBM = procedure(size: TGLint; _type: TGLenum; stride: TGLint; const _pointer: PGLvoid; ptrstride: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglEdgeFlagPointerListIBM = procedure(stride: TGLint; const _pointer: PGLboolean; ptrstride: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFogCoordPointerListIBM = procedure(_type: TGLenum; stride: TGLint; const _pointer: PGLvoid; ptrstride: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIndexPointerListIBM = procedure(_type: TGLenum; stride: TGLint; const _pointer: PGLvoid; ptrstride: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNormalPointerListIBM = procedure(_type: TGLenum; stride: TGLint; const _pointer: PGLvoid; ptrstride: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoordPointerListIBM = procedure(size: TGLint; _type: TGLenum; stride: TGLint; const _pointer: PGLvoid; ptrstride: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexPointerListIBM = procedure(size: TGLint; _type: TGLenum; stride: TGLint; const _pointer: PGLvoid; ptrstride: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_INGR_blend_func_separate
  TglBlendFuncSeparateINGR = procedure(sfactorRGB: TGLenum; dfactorRGB: TGLenum; sfactorAlpha: TGLenum; dfactorAlpha: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_INTEL_parallel_arrays
  TglVertexPointervINTEL = procedure(size: TGLint; _type: TGLenum; const _pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNormalPointervINTEL = procedure(_type: TGLenum; const _pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColorPointervINTEL = procedure(size: TGLint; _type: TGLenum; const _pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoordPointervINTEL = procedure(size: TGLint; _type: TGLenum; const _pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_MESA_resize_buffers
  TglResizeBuffersMESA = procedure(); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_MESA_window_pos
  TglWindowPos2dMESA = procedure(x: TGLdouble; y: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos2dvMESA = procedure(const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos2fMESA = procedure(x: TGLfloat; y: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos2fvMESA = procedure(const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos2iMESA = procedure(x: TGLint; y: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos2ivMESA = procedure(const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos2sMESA = procedure(x: TGLshort; y: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos2svMESA = procedure(const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos3dMESA = procedure(x: TGLdouble; y: TGLdouble; z: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos3dvMESA = procedure(const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos3fMESA = procedure(x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos3fvMESA = procedure(const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos3iMESA = procedure(x: TGLint; y: TGLint; z: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos3ivMESA = procedure(const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos3sMESA = procedure(x: TGLshort; y: TGLshort; z: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos3svMESA = procedure(const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos4dMESA = procedure(x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos4dvMESA = procedure(const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos4fMESA = procedure(x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos4fvMESA = procedure(const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos4iMESA = procedure(x: TGLint; y: TGLint; z: TGLint; w: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos4ivMESA = procedure(const v: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos4sMESA = procedure(x: TGLshort; y: TGLshort; z: TGLshort; w: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglWindowPos4svMESA = procedure(const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_NV_evaluators
  TglMapControlPointsNV = procedure(target: TGLenum; index: TGLuint; _type: TGLenum; ustride: TGLsizei; vstride: TGLsizei; uorder: TGLint; vorder: TGLint; _packed: TGLboolean; const points: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMapParameterivNV = procedure(target: TGLenum; pname: TGLenum; const params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMapParameterfvNV = procedure(target: TGLenum; pname: TGLenum; const params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetMapControlPointsNV = procedure(target: TGLenum; index: TGLuint; _type: TGLenum; ustride: TGLsizei; vstride: TGLsizei; _packed: TGLboolean; points: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetMapParameterivNV = procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetMapParameterfvNV = procedure(target: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetMapAttribParameterivNV = procedure(target: TGLenum; index: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetMapAttribParameterfvNV = procedure(target: TGLenum; index: TGLuint; pname: TGLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglEvalMapsNV = procedure(target: TGLenum; mode: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_NV_fence
  TglDeleteFencesNV = procedure(n: TGLsizei; const fences: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGenFencesNV = procedure(n: TGLsizei; fences: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIsFenceNV = function(fence: TGLuint): TGLboolean; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTestFenceNV = function(fence: TGLuint): TGLboolean; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetFenceivNV = procedure(fence: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFinishFenceNV = procedure(fence: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSetFenceNV = procedure(fence: TGLuint; condition: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_NV_fragment_program
  TglProgramNamedParameter4fNV = procedure(id: TGLuint; len: TGLsizei; const name: PGLubyte; x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramNamedParameter4dNV = procedure(id: TGLuint; len: TGLsizei; const name: PGLubyte; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramNamedParameter4fvNV = procedure(id: TGLuint; len: TGLsizei; const name: PGLubyte; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramNamedParameter4dvNV = procedure(id: TGLuint; len: TGLsizei; const name: PGLubyte; const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetProgramNamedParameterfvNV = procedure(id: TGLuint; len: TGLsizei; const name: PGLubyte; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetProgramNamedParameterdvNV = procedure(id: TGLuint; len: TGLsizei; const name: PGLubyte; params: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_NV_half_float
  TglVertex2hNV = procedure(x: TGLhalfNV; y: TGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertex2hvNV = procedure(const v: PGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertex3hNV = procedure(x: TGLhalfNV; y: TGLhalfNV; z: TGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertex3hvNV = procedure(const v: PGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertex4hNV = procedure(x: TGLhalfNV; y: TGLhalfNV; z: TGLhalfNV; w: TGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertex4hvNV = procedure(const v: PGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNormal3hNV = procedure(nx: TGLhalfNV; ny: TGLhalfNV; nz: TGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNormal3hvNV = procedure(const v: PGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor3hNV = procedure(red: TGLhalfNV; green: TGLhalfNV; blue: TGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor3hvNV = procedure(const v: PGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor4hNV = procedure(red: TGLhalfNV; green: TGLhalfNV; blue: TGLhalfNV; alpha: TGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor4hvNV = procedure(const v: PGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord1hNV = procedure(s: TGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord1hvNV = procedure(const v: PGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord2hNV = procedure(s: TGLhalfNV; t: TGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord2hvNV = procedure(const v: PGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord3hNV = procedure(s: TGLhalfNV; t: TGLhalfNV; r: TGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord3hvNV = procedure(const v: PGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord4hNV = procedure(s: TGLhalfNV; t: TGLhalfNV; r: TGLhalfNV; q: TGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord4hvNV = procedure(const v: PGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord1hNV = procedure(target: TGLenum; s: TGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord1hvNV = procedure(target: TGLenum; const v: PGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord2hNV = procedure(target: TGLenum; s: TGLhalfNV; t: TGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord2hvNV = procedure(target: TGLenum; const v: PGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord3hNV = procedure(target: TGLenum; s: TGLhalfNV; t: TGLhalfNV; r: TGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord3hvNV = procedure(target: TGLenum; const v: PGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord4hNV = procedure(target: TGLenum; s: TGLhalfNV; t: TGLhalfNV; r: TGLhalfNV; q: TGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglMultiTexCoord4hvNV = procedure(target: TGLenum; const v: PGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFogCoordhNV = procedure(fog: TGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFogCoordhvNV = procedure(const fog: PGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSecondaryColor3hNV = procedure(red: TGLhalfNV; green: TGLhalfNV; blue: TGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSecondaryColor3hvNV = procedure(const v: PGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexWeighthNV = procedure(weight: TGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexWeighthvNV = procedure(const weight: PGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib1hNV = procedure(index: TGLuint; x: TGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib1hvNV = procedure(index: TGLuint; const v: PGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib2hNV = procedure(index: TGLuint; x: TGLhalfNV; y: TGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib2hvNV = procedure(index: TGLuint; const v: PGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib3hNV = procedure(index: TGLuint; x: TGLhalfNV; y: TGLhalfNV; z: TGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib3hvNV = procedure(index: TGLuint; const v: PGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4hNV = procedure(index: TGLuint; x: TGLhalfNV; y: TGLhalfNV; z: TGLhalfNV; w: TGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4hvNV = procedure(index: TGLuint; const v: PGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribs1hvNV = procedure(index: TGLuint; n: TGLsizei; const v: PGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribs2hvNV = procedure(index: TGLuint; n: TGLsizei; const v: PGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribs3hvNV = procedure(index: TGLuint; n: TGLsizei; const v: PGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribs4hvNV = procedure(index: TGLuint; n: TGLsizei; const v: PGLhalfNV); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_NV_occlusion_query
  TglGenOcclusionQueriesNV = procedure(n: TGLsizei; ids: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDeleteOcclusionQueriesNV = procedure(n: TGLsizei; const ids: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIsOcclusionQueryNV = function(id: TGLuint): TGLboolean; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBeginOcclusionQueryNV = procedure(id: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglEndOcclusionQueryNV = procedure(); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetOcclusionQueryivNV = procedure(id: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetOcclusionQueryuivNV = procedure(id: TGLuint; pname: TGLenum; params: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_NV_pixel_data_range
  TglPixelDataRangeNV = procedure(target: TGLenum; length: TGLsizei; _pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFlushPixelDataRangeNV = procedure(target: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_NV_point_sprite
  TglPointParameteriNV = procedure(pname: TGLenum; param: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPointParameterivNV = procedure(pname: TGLenum; const params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_NV_primitive_restart
  TglPrimitiveRestartNV = procedure(); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPrimitiveRestartIndexNV = procedure(index: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_NV_register_combiners
  TglCombinerParameterfvNV = procedure(pname: TGLenum; const params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCombinerParameterfNV = procedure(pname: TGLenum; param: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCombinerParameterivNV = procedure(pname: TGLenum; const params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCombinerParameteriNV = procedure(pname: TGLenum; param: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCombinerInputNV = procedure(stage: TGLenum; portion: TGLenum; variable: TGLenum; input: TGLenum; mapping: TGLenum; componentUsage: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCombinerOutputNV = procedure(stage: TGLenum; portion: TGLenum; abOutput: TGLenum; cdOutput: TGLenum; sumOutput: TGLenum; scale: TGLenum; bias: TGLenum; abDotProduct: TGLboolean; cdDotProduct: TGLboolean; muxSum: TGLboolean); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFinalCombinerInputNV = procedure(variable: TGLenum; input: TGLenum; mapping: TGLenum; componentUsage: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetCombinerInputParameterfvNV = procedure(stage: TGLenum; portion: TGLenum; variable: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetCombinerInputParameterivNV = procedure(stage: TGLenum; portion: TGLenum; variable: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetCombinerOutputParameterfvNV = procedure(stage: TGLenum; portion: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetCombinerOutputParameterivNV = procedure(stage: TGLenum; portion: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetFinalCombinerInputParameterfvNV = procedure(variable: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetFinalCombinerInputParameterivNV = procedure(variable: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_NV_register_combiners2
  TglCombinerStageParameterfvNV = procedure(stage: TGLenum; pname: TGLenum; const params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetCombinerStageParameterfvNV = procedure(stage: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_NV_vertex_array_range
  TglFlushVertexArrayRangeNV = procedure(); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexArrayRangeNV = procedure(length: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_NV_vertex_program
  TglAreProgramsResidentNV = function(n: TGLsizei; const programs: PGLuint; residences: PGLboolean): TGLboolean; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBindProgramNV = procedure(target: TGLenum; id: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDeleteProgramsNV = procedure(n: TGLsizei; const programs: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglExecuteProgramNV = procedure(target: TGLenum; id: TGLuint; const params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGenProgramsNV = procedure(n: TGLsizei; programs: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetProgramParameterdvNV = procedure(target: TGLenum; index: TGLuint; pname: TGLenum; params: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetProgramParameterfvNV = procedure(target: TGLenum; index: TGLuint; pname: TGLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetProgramivNV = procedure(id: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetProgramStringNV = procedure(id: TGLuint; pname: TGLenum; _program: PGLubyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetTrackMatrixivNV = procedure(target: TGLenum; address: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetVertexAttribdvNV = procedure(index: TGLuint; pname: TGLenum; params: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetVertexAttribfvNV = procedure(index: TGLuint; pname: TGLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetVertexAttribivNV = procedure(index: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetVertexAttribPointervNV = procedure(index: TGLuint; pname: TGLenum; _pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIsProgramNV = function(id: TGLuint): TGLboolean; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglLoadProgramNV = procedure(target: TGLenum; id: TGLuint; len: TGLsizei; const _program: PGLubyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramParameter4dNV = procedure(target: TGLenum; index: TGLuint; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramParameter4dvNV = procedure(target: TGLenum; index: TGLuint; const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramParameter4fNV = procedure(target: TGLenum; index: TGLuint; x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramParameter4fvNV = procedure(target: TGLenum; index: TGLuint; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramParameters4dvNV = procedure(target: TGLenum; index: TGLuint; count: TGLuint; const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramParameters4fvNV = procedure(target: TGLenum; index: TGLuint; count: TGLuint; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglRequestResidentProgramsNV = procedure(n: TGLsizei; const programs: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTrackMatrixNV = procedure(target: TGLenum; address: TGLuint; matrix: TGLenum; transform: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribPointerNV = procedure(index: TGLuint; fsize: TGLint; _type: TGLenum; stride: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib1dNV = procedure(index: TGLuint; x: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib1dvNV = procedure(index: TGLuint; const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib1fNV = procedure(index: TGLuint; x: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib1fvNV = procedure(index: TGLuint; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib1sNV = procedure(index: TGLuint; x: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib1svNV = procedure(index: TGLuint; const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib2dNV = procedure(index: TGLuint; x: TGLdouble; y: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib2dvNV = procedure(index: TGLuint; const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib2fNV = procedure(index: TGLuint; x: TGLfloat; y: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib2fvNV = procedure(index: TGLuint; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib2sNV = procedure(index: TGLuint; x: TGLshort; y: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib2svNV = procedure(index: TGLuint; const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib3dNV = procedure(index: TGLuint; x: TGLdouble; y: TGLdouble; z: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib3dvNV = procedure(index: TGLuint; const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib3fNV = procedure(index: TGLuint; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib3fvNV = procedure(index: TGLuint; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib3sNV = procedure(index: TGLuint; x: TGLshort; y: TGLshort; z: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib3svNV = procedure(index: TGLuint; const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4dNV = procedure(index: TGLuint; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4dvNV = procedure(index: TGLuint; const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4fNV = procedure(index: TGLuint; x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4fvNV = procedure(index: TGLuint; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4sNV = procedure(index: TGLuint; x: TGLshort; y: TGLshort; z: TGLshort; w: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4svNV = procedure(index: TGLuint; const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4ubNV = procedure(index: TGLuint; x: TGLubyte; y: TGLubyte; z: TGLubyte; w: TGLubyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttrib4ubvNV = procedure(index: TGLuint; const v: PGLubyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribs1dvNV = procedure(index: TGLuint; count: TGLsizei; const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribs1fvNV = procedure(index: TGLuint; count: TGLsizei; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribs1svNV = procedure(index: TGLuint; count: TGLsizei; const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribs2dvNV = procedure(index: TGLuint; count: TGLsizei; const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribs2fvNV = procedure(index: TGLuint; count: TGLsizei; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribs2svNV = procedure(index: TGLuint; count: TGLsizei; const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribs3dvNV = procedure(index: TGLuint; count: TGLsizei; const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribs3fvNV = procedure(index: TGLuint; count: TGLsizei; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribs3svNV = procedure(index: TGLuint; count: TGLsizei; const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribs4dvNV = procedure(index: TGLuint; count: TGLsizei; const v: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribs4fvNV = procedure(index: TGLuint; count: TGLsizei; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribs4svNV = procedure(index: TGLuint; count: TGLsizei; const v: PGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglVertexAttribs4ubvNV = procedure(index: TGLuint; count: TGLsizei; const v: PGLubyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_NV_depth_buffer_float
  TglDepthRangedNV = procedure(n: TGLdouble; f: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglClearDepthdNV = procedure(d: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDepthBoundsdNV = procedure(zmin: TGLdouble; zmax: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_NV_framebuffer_multisample_coverage
  TglRenderbufferStorageMultsampleCoverageNV = procedure(target: TGLenum; coverageSamples: TGLsizei; colorSamples: TGLsizei; internalformat: TGLenum; width: TGLsizei; height: TGLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_NV_geometry_program4
  TglProgramVertexLimitNV = procedure(target: TGLenum; limit: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_NV_gpu_program4
  TglProgramLocalParameterI4iNV = procedure(target: TGLenum; index: TGLuint; x: TGLint; y: TGLint; z: TGLint; w: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramLocalParameterI4ivNV = procedure(target: TGLenum; index: TGLuint; const params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramLocalParametersI4ivNV = procedure(target: TGLenum; index: TGLuint; count: TGLsizei; const params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramLocalParameterI4uiNV = procedure(target: TGLenum; index: TGLuint; x: TGLuint; y: TGLuint; z: TGLuint; w: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramLocalParameterI4uivNV = procedure(target: TGLenum; index: TGLuint; const params: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramLocalParametersI4uivNV = procedure(target: TGLenum; index: TGLuint; count: TGLsizei; const params: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramEnvParameterI4iNV = procedure(target: TGLenum; index: TGLuint; x: TGLint; y: TGLint; z: TGLint; w: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramEnvParameterI4ivNV = procedure(target: TGLenum; index: TGLuint; const params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramEnvParametersI4ivNV = procedure(target: TGLenum; index: TGLuint; count: TGLsizei; const params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramEnvParameterI4uiNV = procedure(target: TGLenum; index: TGLuint; x: TGLuint; y: TGLuint; z: TGLuint; w: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramEnvParameterI4uivNV = procedure(target: TGLenum; index: TGLuint; const params: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramEnvParametersI4uivNV = procedure(target: TGLenum; index: TGLuint; count: TGLsizei; const params: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetProgramLocalParameterIivNV = procedure(target: TGLenum; index: TGLuint; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetProgramLocalParameterIuivNV = procedure(target: TGLenum; index: TGLuint; params: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetProgramEnvParameterIivNV = procedure(target: TGLenum; index: TGLuint; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetProgramEnvParameterIuivNV = procedure(target: TGLenum; index: TGLuint; params: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_NV_parameter_buffer_object
  TglBindBufferRangeNV = procedure(target: TGLenum; index: TGLuint; buffer: TGLuint; offset: TGLint; size: TGLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBindBufferOffsetNV = procedure(target: TGLenum; index: TGLuint; buffer: TGLuint; offset: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBindBufferBaseNV = procedure(target: TGLenum; index: TGLuint; buffer: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramBufferParametersfvNV = procedure(target: TGLenum; buffer: TGLuint; index: TGLuint; count: TGLsizei; const params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramBufferParametersIivNV = procedure(target: TGLenum; buffer: TGLuint; index: TGLuint; count: TGLsizei; const params: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglProgramBufferParametersIuivNV = procedure(target: TGLenum; buffer: TGLuint; index: TGLuint; count: TGLuint; const params: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_NV_transform_feedback
  TglTransformFeedbackAttribsNV = procedure(count: TGLsizei; const attribs: TGLint; bufferMode: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTransformFeedbackVaryingsNV = procedure(_program: TGLuint; count: TGLsizei; const locations: PGLint; bufferMode: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglBeginTransformFeedbackNV = procedure(primitiveMode: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglEndTransformFeedbackNV = procedure(); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetVaryingLocationNV = function(_program: TGLuint; const name: Pchar): TGLint; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetActiveVaryingNV = procedure(_program: TGLuint; index: TGLuint; bufSize: TGLsizei; length: PGLsizei; size: PGLsizei; _type: PGLenum; name: Pchar); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglActiveVaryingNV = procedure(_program: TGLuint; const name: Pchar); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetTransformFeedbackVaryingNV = procedure(_program: TGLuint; index: TGLuint; location: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_NV_conditional_render
  TglBeginConditionalRenderNV = procedure(id: GLuint; mode: GLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglEndConditionalRenderNV = procedure(); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_NV_explicit_multisample
  TglGetMultisamplefvNV = procedure (pname: GLenum; index: GLuint; val: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSampleMaskIndexedNV = procedure (index: GLuint; mask: GLbitfield); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexRenderbufferNV = procedure (target: GLenum; renderbuffer: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_NV_transform_feedback2
  TglBindTransformFeedbackNV = procedure(target: GLenum; id: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDeleteTransformFeedbacksNV = procedure(n: GLsizei; ids: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGenTransformFeedbacksNV = procedure(n: GLsizei; ids: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIsTransformFeedbackNV = function (id: GLuint): GLboolean; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPauseTransformFeedbackNV = procedure(); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglResumeTransformFeedbackNV = procedure(); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDrawTransformFeedbackNV = procedure(mode: GLenum; id: GLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_PGI_misc_hints
  TglHintPGI = procedure(target: TGLenum; mode: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_SGIS_detail_texture
  TglDetailTexFuncSGIS = procedure(target: TGLenum; n: TGLsizei; const points: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetDetailTexFuncSGIS = procedure(target: TGLenum; points: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_SGIS_fog_function
  TglFogFuncSGIS = procedure(n: TGLsizei; const points: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetFogFuncSGIS = procedure(points: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_SGIS_multisample
  TglSampleMaskSGIS = procedure(value: TGLclampf; invert: TGLboolean); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSamplePatternSGIS = procedure(pattern: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_SGIS_pixel_texture
  TglPixelTexGenParameteriSGIS = procedure(pname: TGLenum; param: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPixelTexGenParameterivSGIS = procedure(pname: TGLenum; const params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPixelTexGenParameterfSGIS = procedure(pname: TGLenum; param: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPixelTexGenParameterfvSGIS = procedure(pname: TGLenum; const params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetPixelTexGenParameterivSGIS = procedure(pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetPixelTexGenParameterfvSGIS = procedure(pname: TGLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_SGIS_point_parameters
  TglPointParameterfSGIS = procedure(pname: TGLenum; param: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPointParameterfvSGIS = procedure(pname: TGLenum; const params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_SGIS_sharpen_texture
  TglSharpenTexFuncSGIS = procedure(target: TGLenum; n: TGLsizei; const points: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetSharpenTexFuncSGIS = procedure(target: TGLenum; points: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_SGIS_texture4D
  TglTexImage4DSGIS = procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; width: TGLsizei; height: TGLsizei; depth: TGLsizei; size4d: TGLsizei; border: TGLint; format: TGLenum; _type: TGLenum; const pixels: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexSubImage4DSGIS = procedure(target: TGLenum; level: TGLint; xoffset: TGLint; yoffset: TGLint; zoffset: TGLint; woffset: TGLint; width: TGLsizei; height: TGLsizei; depth: TGLsizei; size4d: TGLsizei; format: TGLenum; _type: TGLenum; const pixels: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_SGIS_texture_color_mask
  TglTextureColorMaskSGIS = procedure(red: TGLboolean; green: TGLboolean; blue: TGLboolean; alpha: TGLboolean); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_SGIS_texture_filter4
  TglGetTexFilterFuncSGIS = procedure(target: TGLenum; filter: TGLenum; weights: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexFilterFuncSGIS = procedure(target: TGLenum; filter: TGLenum; n: TGLsizei; const weights: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_SGIX_async
  TglAsyncMarkerSGIX = procedure(marker: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFinishAsyncSGIX = function(markerp: PGLuint): TGLint; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPollAsyncSGIX = function(markerp: PGLuint): TGLint; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGenAsyncMarkersSGIX = function(range: TGLsizei): TGLuint; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDeleteAsyncMarkersSGIX = procedure(marker: TGLuint; range: TGLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglIsAsyncMarkerSGIX = function(marker: TGLuint): TGLboolean; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_SGIX_flush_raster
  TglFlushRasterSGIX = procedure(); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_SGIX_fragment_lighting
  TglFragmentColorMaterialSGIX = procedure(face: TGLenum; mode: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFragmentLightfSGIX = procedure(light: TGLenum; pname: TGLenum; param: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFragmentLightfvSGIX = procedure(light: TGLenum; pname: TGLenum; const params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFragmentLightiSGIX = procedure(light: TGLenum; pname: TGLenum; param: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFragmentLightivSGIX = procedure(light: TGLenum; pname: TGLenum; const params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFragmentLightModelfSGIX = procedure(pname: TGLenum; param: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFragmentLightModelfvSGIX = procedure(pname: TGLenum; const params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFragmentLightModeliSGIX = procedure(pname: TGLenum; param: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFragmentLightModelivSGIX = procedure(pname: TGLenum; const params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFragmentMaterialfSGIX = procedure(face: TGLenum; pname: TGLenum; param: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFragmentMaterialfvSGIX = procedure(face: TGLenum; pname: TGLenum; const params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFragmentMaterialiSGIX = procedure(face: TGLenum; pname: TGLenum; param: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglFragmentMaterialivSGIX = procedure(face: TGLenum; pname: TGLenum; const params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetFragmentLightfvSGIX = procedure(light: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetFragmentLightivSGIX = procedure(light: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetFragmentMaterialfvSGIX = procedure(face: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetFragmentMaterialivSGIX = procedure(face: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglLightEnviSGIX = procedure(pname: TGLenum; param: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_SGIX_framezoom
  TglFrameZoomSGIX = procedure(factor: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_SGIX_igloo_interface
  TglIglooInterfaceSGIX = procedure(pname: TGLenum; const params: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_SGIX_instruments
  TglGetInstrumentsSGIX = function(): TGLint; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglInstrumentsBufferSGIX = procedure(size: TGLsizei; buffer: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglPollInstrumentsSGIX = function(marker_p: PGLint): TGLint; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglReadInstrumentsSGIX = procedure(marker: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglStartInstrumentsSGIX = procedure(); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglStopInstrumentsSGIX = procedure(marker: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_SGIX_list_priority
  TglGetListParameterfvSGIX = procedure(list: TGLuint; pname: TGLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetListParameterivSGIX = procedure(list: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglListParameterfSGIX = procedure(list: TGLuint; pname: TGLenum; param: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglListParameterfvSGIX = procedure(list: TGLuint; pname: TGLenum; const params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglListParameteriSGIX = procedure(list: TGLuint; pname: TGLenum; param: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglListParameterivSGIX = procedure(list: TGLuint; pname: TGLenum; const params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_SGIX_pixel_texture
  TglPixelTexGenSGIX = procedure(mode: TGLenum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_SGIX_polynomial_ffd
  TglDeformationMap3dSGIX = procedure(target: TGLenum; u1: TGLdouble; u2: TGLdouble; ustride: TGLint; uorder: TGLint; v1: TGLdouble; v2: TGLdouble; vstride: TGLint; vorder: TGLint; w1: TGLdouble; w2: TGLdouble; wstride: TGLint; worder: TGLint; const points: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDeformationMap3fSGIX = procedure(target: TGLenum; u1: TGLfloat; u2: TGLfloat; ustride: TGLint; uorder: TGLint; v1: TGLfloat; v2: TGLfloat; vstride: TGLint; vorder: TGLint; w1: TGLfloat; w2: TGLfloat; wstride: TGLint; worder: TGLint; const points: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglDeformSGIX = procedure(mask: TGLbitfield); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglLoadIdentityDeformationMapSGIX = procedure(mask: TGLbitfield); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_SGIX_reference_plane
  TglReferencePlaneSGIX = procedure(const equation: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_SGIX_sprite
  TglSpriteParameterfSGIX = procedure(pname: TGLenum; param: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSpriteParameterfvSGIX = procedure(pname: TGLenum; const params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSpriteParameteriSGIX = procedure(pname: TGLenum; param: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglSpriteParameterivSGIX = procedure(pname: TGLenum; const params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_SGIX_tag_sample_buffer
  TglTagSampleBufferSGIX = procedure(); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_SGI_color_table
  TglColorTableSGI = procedure(target: TGLenum; internalformat: TGLenum; width: TGLsizei; format: TGLenum; _type: TGLenum; const table: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColorTableParameterfvSGI = procedure(target: TGLenum; pname: TGLenum; const params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColorTableParameterivSGI = procedure(target: TGLenum; pname: TGLenum; const params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglCopyColorTableSGI = procedure(target: TGLenum; internalformat: TGLenum; x: TGLint; y: TGLint; width: TGLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetColorTableSGI = procedure(target: TGLenum; format: TGLenum; _type: TGLenum; table: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetColorTableParameterfvSGI = procedure(target: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGetColorTableParameterivSGI = procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_SUNX_constant_data
  TglFinishTextureSUNX = procedure(); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_SUN_global_alpha
  TglGlobalAlphaFactorbSUN = procedure(factor: TGLbyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGlobalAlphaFactorsSUN = procedure(factor: TGLshort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGlobalAlphaFactoriSUN = procedure(factor: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGlobalAlphaFactorfSUN = procedure(factor: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGlobalAlphaFactordSUN = procedure(factor: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGlobalAlphaFactorubSUN = procedure(factor: TGLubyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGlobalAlphaFactorusSUN = procedure(factor: TGLushort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglGlobalAlphaFactoruiSUN = procedure(factor: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_SUN_mesh_array
  TglDrawMeshArraysSUN = procedure(mode: TGLenum; first: TGLint; count: TGLsizei; width: TGLsizei); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_SUN_triangle_list
  TglReplacementCodeuiSUN = procedure(code: TGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglReplacementCodeusSUN = procedure(code: TGLushort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglReplacementCodeubSUN = procedure(code: TGLubyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglReplacementCodeuivSUN = procedure(const code: PGLuint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglReplacementCodeusvSUN = procedure(const code: PGLushort); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglReplacementCodeubvSUN = procedure(const code: PGLubyte); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglReplacementCodePointerSUN = procedure(_type: TGLenum; stride: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // GL_SUN_vertex
  TglColor4ubVertex2fSUN = procedure(r: TGLubyte; g: TGLubyte; b: TGLubyte; a: TGLubyte; x: TGLfloat; y: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor4ubVertex2fvSUN = procedure(const c: PGLubyte; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor4ubVertex3fSUN = procedure(r: TGLubyte; g: TGLubyte; b: TGLubyte; a: TGLubyte; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor4ubVertex3fvSUN = procedure(const c: PGLubyte; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor3fVertex3fSUN = procedure(r: TGLfloat; g: TGLfloat; b: TGLfloat; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor3fVertex3fvSUN = procedure(const c: PGLfloat; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNormal3fVertex3fSUN = procedure(nx: TGLfloat; ny: TGLfloat; nz: TGLfloat; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglNormal3fVertex3fvSUN = procedure(const n: PGLfloat; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor4fNormal3fVertex3fSUN = procedure(r: TGLfloat; g: TGLfloat; b: TGLfloat; a: TGLfloat; nx: TGLfloat; ny: TGLfloat; nz: TGLfloat; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglColor4fNormal3fVertex3fvSUN = procedure(const c: PGLfloat; const n: PGLfloat; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord2fVertex3fSUN = procedure(s: TGLfloat; t: TGLfloat; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord2fVertex3fvSUN = procedure(const tc: PGLfloat; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord4fVertex4fSUN = procedure(s: TGLfloat; t: TGLfloat; p: TGLfloat; q: TGLfloat; x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord4fVertex4fvSUN = procedure(const tc: PGLfloat; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord2fColor4ubVertex3fSUN = procedure(s: TGLfloat; t: TGLfloat; r: TGLubyte; g: TGLubyte; b: TGLubyte; a: TGLubyte; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord2fColor4ubVertex3fvSUN = procedure(const tc: PGLfloat; const c: PGLubyte; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord2fColor3fVertex3fSUN = procedure(s: TGLfloat; t: TGLfloat; r: TGLfloat; g: TGLfloat; b: TGLfloat; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord2fColor3fVertex3fvSUN = procedure(const tc: PGLfloat; const c: PGLfloat; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord2fNormal3fVertex3fSUN = procedure(s: TGLfloat; t: TGLfloat; nx: TGLfloat; ny: TGLfloat; nz: TGLfloat; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord2fNormal3fVertex3fvSUN = procedure(const tc: PGLfloat; const n: PGLfloat; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord2fColor4fNormal3fVertex3fSUN = procedure(s: TGLfloat; t: TGLfloat; r: TGLfloat; g: TGLfloat; b: TGLfloat; a: TGLfloat; nx: TGLfloat; ny: TGLfloat; nz: TGLfloat; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord2fColor4fNormal3fVertex3fvSUN = procedure(const tc: PGLfloat; const c: PGLfloat; const n: PGLfloat; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord4fColor4fNormal3fVertex4fSUN = procedure(s: TGLfloat; t: TGLfloat; p: TGLfloat; q: TGLfloat; r: TGLfloat; g: TGLfloat; b: TGLfloat; a: TGLfloat; nx: TGLfloat; ny: TGLfloat; nz: TGLfloat; x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglTexCoord4fColor4fNormal3fVertex4fvSUN = procedure(const tc: PGLfloat; const c: PGLfloat; const n: PGLfloat; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglReplacementCodeuiVertex3fSUN = procedure(rc: TGLuint; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglReplacementCodeuiVertex3fvSUN = procedure(const rc: PGLuint; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglReplacementCodeuiColor4ubVertex3fSUN = procedure(rc: TGLuint; r: TGLubyte; g: TGLubyte; b: TGLubyte; a: TGLubyte; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglReplacementCodeuiColor4ubVertex3fvSUN = procedure(const rc: PGLuint; const c: PGLubyte; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglReplacementCodeuiColor3fVertex3fSUN = procedure(rc: TGLuint; r: TGLfloat; g: TGLfloat; b: TGLfloat; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglReplacementCodeuiColor3fVertex3fvSUN = procedure(const rc: PGLuint; const c: PGLfloat; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglReplacementCodeuiNormal3fVertex3fSUN = procedure(rc: TGLuint; nx: TGLfloat; ny: TGLfloat; nz: TGLfloat; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglReplacementCodeuiNormal3fVertex3fvSUN = procedure(const rc: PGLuint; const n: PGLfloat; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglReplacementCodeuiColor4fNormal3fVertex3fSUN = procedure(rc: TGLuint; r: TGLfloat; g: TGLfloat; b: TGLfloat; a: TGLfloat; nx: TGLfloat; ny: TGLfloat; nz: TGLfloat; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglReplacementCodeuiColor4fNormal3fVertex3fvSUN = procedure(const rc: PGLuint; const c: PGLfloat; const n: PGLfloat; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglReplacementCodeuiTexCoord2fVertex3fSUN = procedure(rc: TGLuint; s: TGLfloat; t: TGLfloat; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglReplacementCodeuiTexCoord2fVertex3fvSUN = procedure(const rc: PGLuint; const tc: PGLfloat; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglReplacementCodeuiTexCoord2fNormal3fVertex3fSUN = procedure(rc: TGLuint; s: TGLfloat; t: TGLfloat; nx: TGLfloat; ny: TGLfloat; nz: TGLfloat; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN = procedure(const rc: PGLuint; const tc: PGLfloat; const n: PGLfloat; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN = procedure(rc: TGLuint; s: TGLfloat; t: TGLfloat; r: TGLfloat; g: TGLfloat; b: TGLfloat; a: TGLfloat; nx: TGLfloat; ny: TGLfloat; nz: TGLfloat; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TglReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN = procedure(const rc: PGLuint; const tc: PGLfloat; const c: PGLfloat; const n: PGLfloat; const v: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

  // window support functions
{$IFDEF Win32}
  TwglGetProcAddress = function(ProcName: PChar): Pointer; stdcall;
  TwglCopyContext = function(p1: HGLRC; p2: HGLRC; p3: Cardinal): BOOL; stdcall;
  TwglCreateContext = function(DC: HDC): HGLRC; stdcall;
  TwglCreateLayerContext = function(p1: HDC; p2: Integer): HGLRC; stdcall;
  TwglDeleteContext = function(p1: HGLRC): BOOL; stdcall;
  TwglDescribeLayerPlane = function(p1: HDC; p2, p3: Integer; p4: Cardinal; p5: PLayerPlaneDescriptor): BOOL; stdcall;
  TwglGetCurrentContext = function: HGLRC; stdcall;
  TwglGetCurrentDC = function: HDC; stdcall;
  TwglGetLayerPaletteEntries = function(p1: HDC; p2, p3, p4: Integer; var pcr): Integer; stdcall;
  TwglMakeCurrent = function(DC: HDC; p2: HGLRC): BOOL; stdcall;
  TwglRealizeLayerPalette = function(p1: HDC; p2: Integer; p3: BOOL): BOOL; stdcall;
  TwglSetLayerPaletteEntries = function(p1: HDC; p2, p3, p4: Integer; var pcr): Integer; stdcall;
  TwglShareLists = function(p1, p2: HGLRC): BOOL; stdcall;
  TwglSwapLayerBuffers = function(p1: HDC; p2: Cardinal): BOOL; stdcall;
  TwglSwapMultipleBuffers = function(p1: UINT; const p2: PWGLSWAP): DWORD; stdcall;
  TwglUseFontBitmapsA = function(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall;
  TwglUseFontBitmapsW = function(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall;
  TwglUseFontBitmaps = function(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall;

  TwglUseFontOutlinesA = function(p1: HDC; p2, p3, p4: DWORD; p5, p6: Single; p7: Integer; p8: PGlyphMetricsFloat): BOOL; stdcall;
  TwglUseFontOutlinesW = function(p1: HDC; p2, p3, p4: DWORD; p5, p6: Single; p7: Integer; p8: PGlyphMetricsFloat): BOOL; stdcall;
  TwglUseFontOutlines = function(p1: HDC; p2, p3, p4: DWORD; p5, p6: Single; p7: Integer; p8: PGlyphMetricsFloat): BOOL; stdcall;


  // WGL_ARB_buffer_region
  TwglCreateBufferRegionARB = function(hDC: HDC; iLayerPlane: TGLint; uType: TGLuint): THandle; stdcall;
  TwglDeleteBufferRegionARB = procedure(hRegion: THandle); stdcall;
  TwglSaveBufferRegionARB = function(hRegion: THandle; x: TGLint; y: TGLint; width: TGLint; height: TGLint): Boolean; stdcall;
  TwglRestoreBufferRegionARB = function(hRegion: THandle; x: TGLint; y: TGLint; width: TGLint; height: TGLint; xSrc: TGLint; ySrc: TGLint): Boolean; stdcall;

  // WGL_ARB_extensions_string
  TwglGetExtensionsStringARB = function(hdc: HDC): PChar; stdcall;

  // WGL_ARB_make_current_read
  TwglMakeContextCurrentARB = function(hDrawDC: HDC; hReadDC: HDC; hglrc: HGLRC): Boolean; stdcall;
  TwglGetCurrentReadDCARB = function(): HDC; stdcall;

  // WGL_ARB_pbuffer
  TwglCreatePbufferARB = function(hDC: HDC; iPixelFormat: TGLint; iWidth: TGLint; iHeight: TGLint; const piAttribList: PGLint): HPBUFFERARB; stdcall;
  TwglGetPbufferDCARB = function(hPbuffer: HPBUFFERARB): HDC; stdcall;
  TwglReleasePbufferDCARB = function(hPbuffer: HPBUFFERARB; hDC: HDC): TGLint; stdcall;
  TwglDestroyPbufferARB = function(hPbuffer: HPBUFFERARB): Boolean; stdcall;
  TwglQueryPbufferARB = function(hPbuffer: HPBUFFERARB; iAttribute: TGLint; piValue: PGLint): Boolean; stdcall;

  // WGL_ARB_pixel_format
  TwglGetPixelFormatAttribivARB = function(hdc: HDC; iPixelFormat: TGLint; iLayerPlane: TGLint; nAttributes: TGLuint; const piAttributes: PGLint; piValues: PGLint): Boolean; stdcall;
  TwglGetPixelFormatAttribfvARB = function(hdc: HDC; iPixelFormat: TGLint; iLayerPlane: TGLint; nAttributes: TGLuint; const piAttributes: PGLint; pfValues: PGLfloat): Boolean; stdcall;
  TwglChoosePixelFormatARB = function(hdc: HDC; const piAttribIList: PGLint; const pfAttribFList: PGLfloat; nMaxFormats: TGLuint; piFormats: PGLint; nNumFormats: PGLuint): BOOL; stdcall;

  // WGL_ARB_color_buffer_float
  TwglClampColorARB = procedure(target: TGLenum; clamp: TGLenum); stdcall;

  // WGL_ARB_render_texture
  TwglBindTexImageARB = function(hPbuffer: HPBUFFERARB; iBuffer: TGLint): Boolean; stdcall;
  TwglReleaseTexImageARB = function(hPbuffer: HPBUFFERARB; iBuffer: TGLint): Boolean; stdcall;
  TwglSetPbufferAttribARB = function(hPbuffer: HPBUFFERARB; const piAttribList: PGLint): Boolean; stdcall;

  // WGL_ARB_create_context
  TwglCreateContextAttribsARB = function(hDC: HDC; hShareContext: HGLRC; const attribList: PGLint): HGLRC; stdcall;

  // WGL_EXT_display_color_table
  TwglCreateDisplayColorTableEXT = function(id: TGLushort): TGLboolean; stdcall;
  TwglLoadDisplayColorTableEXT = function(const table: PGLushort; length: TGLuint): TGLboolean; stdcall;
  TwglBindDisplayColorTableEXT = function(id: TGLushort): TGLboolean; stdcall;
  TwglDestroyDisplayColorTableEXT = procedure(id: TGLushort); stdcall;

  // WGL_EXT_extensions_string
  TwglGetExtensionsStringEXT = function(): PChar; stdcall;

  // WGL_EXT_make_current_read
  TwglMakeContextCurrentEXT = function(hDrawDC: HDC; hReadDC: HDC; hglrc: HGLRC): Boolean; stdcall;
  TwglGetCurrentReadDCEXT = function(): HDC; stdcall;

  // WGL_EXT_pbuffer
  TwglCreatePbufferEXT = function(hDC: HDC; iPixelFormat: TGLint; iWidth: TGLint; iHeight: TGLint; const piAttribList: PGLint): HPBUFFEREXT; stdcall;
  TwglGetPbufferDCEXT = function(hPbuffer: HPBUFFEREXT): HDC; stdcall;
  TwglReleasePbufferDCEXT = function(hPbuffer: HPBUFFEREXT; hDC: HDC): TGLint; stdcall;
  TwglDestroyPbufferEXT = function(hPbuffer: HPBUFFEREXT): Boolean; stdcall;
  TwglQueryPbufferEXT = function(hPbuffer: HPBUFFEREXT; iAttribute: TGLint; piValue: PGLint): Boolean; stdcall;

  // WGL_EXT_pixel_format
  TwglGetPixelFormatAttribivEXT = function(hdc: HDC; iPixelFormat: TGLint; iLayerPlane: TGLint; nAttributes: TGLuint; piAttributes: PGLint; piValues: PGLint): Boolean; stdcall;
  TwglGetPixelFormatAttribfvEXT = function(hdc: HDC; iPixelFormat: TGLint; iLayerPlane: TGLint; nAttributes: TGLuint; piAttributes: PGLint; pfValues: PGLfloat): Boolean; stdcall;
  TwglChoosePixelFormatEXT = function(hdc: HDC; const piAttribIList: PGLint; const pfAttribFList: PGLfloat; nMaxFormats: TGLuint; piFormats: PGLint; nNumFormats: PGLuint): Boolean; stdcall;

  // WGL_EXT_swap_control
  TwglSwapIntervalEXT = function(interval: TGLint): Boolean; stdcall;
  TwglGetSwapIntervalEXT = function(): TGLint; stdcall;

  // WGL_I3D_digital_video_control
  TwglGetDigitalVideoParametersI3D = function(hDC: HDC; iAttribute: TGLint; piValue: PGLint): Boolean; stdcall;
  TwglSetDigitalVideoParametersI3D = function(hDC: HDC; iAttribute: TGLint; const piValue: PGLint): Boolean; stdcall;

  // WGL_I3D_gamma
  TwglGetGammaTableParametersI3D = function(hDC: HDC; iAttribute: TGLint; piValue: PGLint): Boolean; stdcall;
  TwglSetGammaTableParametersI3D = function(hDC: HDC; iAttribute: TGLint; const piValue: PGLint): Boolean; stdcall;
  TwglGetGammaTableI3D = function(hDC: HDC; iEntries: TGLint; puRed: PGLushort; puGreen: PGLushort; puBlue: PGLushort): Boolean; stdcall;
  TwglSetGammaTableI3D = function(hDC: HDC; iEntries: TGLint; const puRed: PGLushort; const puGreen: PGLushort; const puBlue: PGLushort): Boolean; stdcall;

  // WGL_I3D_genlock
  TwglEnableGenlockI3D = function(hDC: HDC): Boolean; stdcall;
  TwglDisableGenlockI3D = function(hDC: HDC): Boolean; stdcall;
  TwglIsEnabledGenlockI3D = function(hDC: HDC; pFlag: Boolean): Boolean; stdcall;
  TwglGenlockSourceI3D = function(hDC: HDC; uSource: TGLuint): Boolean; stdcall;
  TwglGetGenlockSourceI3D = function(hDC: HDC; uSource: PGLuint): Boolean; stdcall;
  TwglGenlockSourceEdgeI3D = function(hDC: HDC; uEdge: TGLuint): Boolean; stdcall;
  TwglGetGenlockSourceEdgeI3D = function(hDC: HDC; uEdge: PGLuint): Boolean; stdcall;
  TwglGenlockSampleRateI3D = function(hDC: HDC; uRate: TGLuint): Boolean; stdcall;
  TwglGetGenlockSampleRateI3D = function(hDC: HDC; uRate: PGLuint): Boolean; stdcall;
  TwglGenlockSourceDelayI3D = function(hDC: HDC; uDelay: TGLuint): Boolean; stdcall;
  TwglGetGenlockSourceDelayI3D = function(hDC: HDC; uDelay: PGLuint): Boolean; stdcall;
  TwglQueryGenlockMaxSourceDelayI3D = function(hDC: HDC; uMaxLineDelay: PGLuint; uMaxPixelDelay: PGLuint): Boolean; stdcall;

  // WGL_I3D_image_buffer
  TwglCreateImageBufferI3D = function(hDC: HDC; dwSize: TGLuint; uFlags: TGLuint): TGLvoid; stdcall;
  TwglDestroyImageBufferI3D = function(hDC: HDC; pAddress: TGLvoid): Boolean; stdcall;
  TwglAssociateImageBufferEventsI3D = function(hDC: HDC; const pEvent: THandle; const pAddress: PGLvoid; const pSize: PGLuint; count: TGLuint): Boolean; stdcall;
  TwglReleaseImageBufferEventsI3D = function(hDC: HDC; const pAddress: PGLvoid; count: TGLuint): Boolean; stdcall;

  // WGL_I3D_swap_frame_lock
  TwglEnableFrameLockI3D = function(): Boolean; stdcall;
  TwglDisableFrameLockI3D = function(): Boolean; stdcall;
  TwglIsEnabledFrameLockI3D = function(pFlag: Boolean): Boolean; stdcall;
  TwglQueryFrameLockMasterI3D = function(pFlag: Boolean): Boolean; stdcall;

  // WGL_I3D_swap_frame_usage
  TwglGetFrameUsageI3D = function(pUsage: PGLfloat): Boolean; stdcall;
  TwglBeginFrameTrackingI3D = function(): Boolean; stdcall;
  TwglEndFrameTrackingI3D = function(): Boolean; stdcall;
  TwglQueryFrameTrackingI3D = function(pFrameCount: PGLuint; pMissedFrames: PGLuint; pLastMissedUsage: PGLfloat): Boolean; stdcall;

  // WGL_NV_vertex_array_range
  TwglAllocateMemoryNV = procedure(size: TGLsizei; readfreq: TGLfloat; writefreq: TGLfloat; priority: TGLfloat); stdcall;
  TwglFreeMemoryNV = procedure(_pointer: Pointer); stdcall;

  // WGL_NV_present_video
  TwglEnumerateVideoDevicesNV = function(hdc: HDC; phDeviceList: PHVIDEOOUTPUTDEVICENV): Integer; stdcall;
  TwglBindVideoDeviceNV = function(hd: HDC; uVideoSlot: Cardinal; hVideoDevice: HVIDEOOUTPUTDEVICENV; piAttribList: PInteger): Boolean; stdcall;
  TwglQueryCurrentContextNV = function(iAttribute: Integer; piValue: PInteger): Boolean; stdcall;

  // WGL_NV_video_out
  TwglGetVideoDeviceNV = function(hDC: HDC; numDevices: Integer; hVideoDevice: PHPVIDEODEV): Boolean; stdcall;
  TwglReleaseVideoDeviceNV = function(hVideoDevice: HPVIDEODEV): Boolean; stdcall;
  TwglBindVideoImageNV = function(hVideoDevice: HPVIDEODEV; hPbuffer: HPBUFFERARB; iVideoBuffer: Integer): Boolean; stdcall;
  TwglReleaseVideoImageNV = function(hPbuffer: HPBUFFERARB; iVideoBuffer: Integer): Boolean; stdcall;
  TwglSendPbufferToVideoNV = function(hPbuffer: HPBUFFERARB; iBufferType: Integer; pulCounterPbuffer: PCardinal; bBlock: Boolean): Boolean; stdcall;
  TwglGetVideoInfoNV = function(hpVideoDevice: HPVIDEODEV; pulCounterOutputPbuffer: PCardinal; pulCounterOutputVideo: PCardinal): Boolean; stdcall;

  // WGL_NV_swap_group
  TwglJoinSwapGroupNV = function(hDC: HDC; group: GLuint): Boolean; stdcall;
  TwglBindSwapBarrierNV = function(group: GLuint; barrier: GLuint): Boolean; stdcall;
  TwglQuerySwapGroupNV = function(hDC: HDC; group: PGLuint; barrier: PGLuint): Boolean; stdcall;
  TwglQueryMaxSwapGroupsNV = function(hDC: HDC; mxGroups: PGLuint; maxBarriers: PGLuint): Boolean; stdcall;
  TwglQueryFrameCountNV = function(hDC: HDC; count: PGLuint): Boolean; stdcall;
  TwglResetFrameCountNV = function(hDC: HDC): Boolean; stdcall;

  // WGL_NV_gpu_affinity
  TwglEnumGpusNV = function(iGpuIndex: Cardinal; phGpu: PHGPUNV): Boolean; stdcall;
  TwglEnumGpuDevicesNV = function(hGpu: HGPUNV; iDeviceIndex: Cardinal; lpGpuDevice: PGPU_DEVICE): Boolean; stdcall;
  TwglCreateAffinityDCNV = function(const phGpuList: PHGPUNV): HDC; stdcall;
  TwglEnumGpusFromAffinityDCNV = function(hAffinityDC: HDC; iGpuIndex: Cardinal; hGpu: PHGPUNV): Boolean; stdcall;
  TwglDeleteDCNV = function(hDC: HDC): Boolean; stdcall;

  // WGL_OML_sync_control
  TwglGetSyncValuesOML = function(hdc: HDC; ust: PGLint64; msc: PGLint64; sbc: PGLint64): Boolean; stdcall;
  TwglGetMscRateOML = function(hdc: HDC; numerator: PGLint; denominator: PGLint): Boolean; stdcall;
  TwglSwapBuffersMscOML = function(hdc: HDC; target_msc: TGLint64; divisor: TGLint64; remainder: TGLint64): TGLint64; stdcall;
  TwglSwapLayerBuffersMscOML = function(hdc: HDC; fuPlanes: TGLint; target_msc: TGLint64; divisor: TGLint64; remainder: TGLint64): TGLint64; stdcall;
  TwglWaitForMscOML = function(hdc: HDC; target_msc: TGLint64; divisor: TGLint64; remainder: TGLint64; ust: PGLint64; msc: PGLint64; sbc: PGLint64): Boolean; stdcall;
  TwglWaitForSbcOML = function(hdc: HDC; target_sbc: TGLint64; ust: PGLint64; msc: PGLint64; sbc: PGLint64): Boolean; stdcall;

  // WIN_draw_range_elements
  TglDrawRangeElementsWIN = procedure(mode: TGLenum; start: TGLuint; _end: TGLuint; count: TGLsizei; _type: TGLenum; const indices: PGLvoid); stdcall;

  // WIN_swap_hint
  TglAddSwapHintRectWIN = procedure(x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei); stdcall;
{$ELSE}
  // GLX_VERSION_1_4
  TglXGetProcAddress = function(const name: pchar): pointer; cdecl;

  // GLX_ARB_get_proc_address
  TglXGetProcAddressARB = function(const name: pchar): pointer; cdecl;
{$ENDIF}


  // GL utility functions and procedures
  TgluErrorString = function(errCode: TGLEnum): PChar; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluGetString = function(name: TGLEnum): PChar; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluOrtho2D = procedure(left, right, bottom, top: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluPerspective = procedure(fovy, aspect, zNear, zFar: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluPickMatrix = procedure(x, y, width, height: TGLdouble; const viewport: TVector4i); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluLookAt = procedure(eyex, eyey, eyez, centerx, centery, centerz, upx, upy, upz: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluProject = function(objx, objy, objz: TGLdouble; const modelMatrix: TGLMatrixd4; const projMatrix: TGLMatrixd4; const viewport: TVector4i; winx, winy, winz: PGLdouble): TGLint; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluUnProject = function(winx, winy, winz: TGLdouble; const modelMatrix: TGLMatrixd4; const projMatrix: TGLMatrixd4; const viewport: TVector4i; objx, objy, objz: PGLdouble): TGLint; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluScaleImage = function(format: TGLEnum; widthin, heightin: TGLint; typein: TGLEnum; datain: Pointer; widthout, heightout: TGLint; typeout: TGLEnum; const dataout: Pointer): TGLint; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluBuild1DMipmaps = function(target: TGLEnum; components, width: TGLint; format, atype: TGLEnum; const data: Pointer): TGLint; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluBuild2DMipmaps = function(target: TGLEnum; components, width, height: TGLint; format, atype: TGLEnum; const Data: Pointer): TGLint; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluNewQuadric = function: PGLUquadric; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluDeleteQuadric = procedure(state: PGLUquadric); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluQuadricNormals = procedure(quadObject: PGLUquadric; normals: TGLEnum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluQuadricTexture = procedure(quadObject: PGLUquadric; textureCoords: TGLboolean); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluQuadricOrientation = procedure(quadObject: PGLUquadric; orientation: TGLEnum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluQuadricDrawStyle = procedure(quadObject: PGLUquadric; drawStyle: TGLEnum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluCylinder = procedure(quadObject: PGLUquadric; baseRadius, topRadius, height: TGLdouble; slices, stacks: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluDisk = procedure(quadObject: PGLUquadric; innerRadius, outerRadius: TGLdouble; slices, loops: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluPartialDisk = procedure(quadObject: PGLUquadric; innerRadius, outerRadius: TGLdouble; slices, loops: TGLint; startAngle, sweepAngle: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluSphere = procedure(quadObject: PGLUquadric; radius: TGLdouble; slices, stacks: TGLint); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluQuadricCallback = procedure(quadObject: PGLUquadric; which: TGLEnum; fn: TGLUQuadricErrorProc); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluNewTess = function: PGLUtesselator; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluDeleteTess = procedure(tess: PGLUtesselator); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluTessBeginPolygon = procedure(tess: PGLUtesselator; polygon_data: Pointer); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluTessBeginContour = procedure(tess: PGLUtesselator); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluTessVertex = procedure(tess: PGLUtesselator; const coords: TGLArrayd3; data: Pointer); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluTessEndContour = procedure(tess: PGLUtesselator); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluTessEndPolygon = procedure(tess: PGLUtesselator); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluTessProperty = procedure(tess: PGLUtesselator; which: TGLEnum; value: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluTessNormal = procedure(tess: PGLUtesselator; x, y, z: TGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluTessCallback = procedure(tess: PGLUtesselator; which: TGLEnum; fn: Pointer); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluGetTessProperty = procedure(tess: PGLUtesselator; which: TGLEnum; value: PGLdouble); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluNewNurbsRenderer = function: PGLUnurbs; {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluDeleteNurbsRenderer = procedure(nobj: PGLUnurbs); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluBeginSurface = procedure(nobj: PGLUnurbs); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluBeginCurve = procedure(nobj: PGLUnurbs); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluEndCurve = procedure(nobj: PGLUnurbs); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluEndSurface = procedure(nobj: PGLUnurbs); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluBeginTrim = procedure(nobj: PGLUnurbs); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluEndTrim = procedure(nobj: PGLUnurbs); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluPwlCurve = procedure(nobj: PGLUnurbs; count: TGLint; points: PGLfloat; stride: TGLint; atype: TGLEnum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluNurbsCurve = procedure(nobj: PGLUnurbs; nknots: TGLint; knot: PGLfloat; stride: TGLint; ctlarray: PGLfloat; order: TGLint; atype: TGLEnum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluNurbsSurface = procedure(nobj: PGLUnurbs; sknot_count: TGLint; sknot: PGLfloat; tknot_count: TGLint; tknot: PGLfloat; s_stride, t_stride: TGLint; ctlarray: PGLfloat; sorder, torder: TGLint; atype: TGLEnum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluLoadSamplingMatrices = procedure(nobj: PGLUnurbs; const modelMatrix, projMatrix: TGLMatrixf4; const viewport: TVector4i); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluNurbsProperty = procedure(nobj: PGLUnurbs; aproperty: TGLEnum; value: TGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluGetNurbsProperty = procedure(nobj: PGLUnurbs; aproperty: TGLEnum; value: PGLfloat); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluNurbsCallback = procedure(nobj: PGLUnurbs; which: TGLEnum; fn: TGLUNurbsErrorProc); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluBeginPolygon = procedure(tess: PGLUtesselator); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluNextContour = procedure(tess: PGLUtesselator; atype: TGLEnum); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}
  TgluEndPolygon = procedure(tess: PGLUtesselator); {$IFDEF Win32}stdcall; {$ELSE}cdecl; {$ENDIF}

var
  // GL_VERSION_1_1
  glAccum: TglAccum;
  glAlphaFunc: TglAlphaFunc;
  glAreTexturesResident: TglAreTexturesResident;
  glArrayElement: TglArrayElement;
  glBegin: TglBegin;
  glBindTexture: TglBindTexture;
  glBitmap: TglBitmap;
  glBlendFunc: TglBlendFunc;
  glCallList: TglCallList;
  glCallLists: TglCallLists;
  glClear: TglClear;
  glClearAccum: TglClearAccum;
  glClearColor: TglClearColor;
  glClearDepth: TglClearDepth;
  glClearIndex: TglClearIndex;
  glClearStencil: TglClearStencil;
  glClipPlane: TglClipPlane;
  glColor3b: TglColor3b;
  glColor3bv: TglColor3bv;
  glColor3d: TglColor3d;
  glColor3dv: TglColor3dv;
  glColor3f: TglColor3f;
  glColor3fv: TglColor3fv;
  glColor3i: TglColor3i;
  glColor3iv: TglColor3iv;
  glColor3s: TglColor3s;
  glColor3sv: TglColor3sv;
  glColor3ub: TglColor3ub;
  glColor3ubv: TglColor3ubv;
  glColor3ui: TglColor3ui;
  glColor3uiv: TglColor3uiv;
  glColor3us: TglColor3us;
  glColor3usv: TglColor3usv;
  glColor4b: TglColor4b;
  glColor4bv: TglColor4bv;
  glColor4d: TglColor4d;
  glColor4dv: TglColor4dv;
  glColor4f: TglColor4f;
  glColor4fv: TglColor4fv;
  glColor4i: TglColor4i;
  glColor4iv: TglColor4iv;
  glColor4s: TglColor4s;
  glColor4sv: TglColor4sv;
  glColor4ub: TglColor4ub;
  glColor4ubv: TglColor4ubv;
  glColor4ui: TglColor4ui;
  glColor4uiv: TglColor4uiv;
  glColor4us: TglColor4us;
  glColor4usv: TglColor4usv;
  glColorMask: TglColorMask;
  glColorMaterial: TglColorMaterial;
  glColorPointer: TglColorPointer;
  glCopyPixels: TglCopyPixels;
  glCopyTexImage1D: TglCopyTexImage1D;
  glCopyTexImage2D: TglCopyTexImage2D;
  glCopyTexSubImage1D: TglCopyTexSubImage1D;
  glCopyTexSubImage2D: TglCopyTexSubImage2D;
  glCullFace: TglCullFace;
  glDeleteLists: TglDeleteLists;
  glDeleteTextures: TglDeleteTextures;
  glDepthFunc: TglDepthFunc;
  glDepthMask: TglDepthMask;
  glDepthRange: TglDepthRange;
  glDisable: TglDisable;
  glDisableClientState: TglDisableClientState;
  glDrawArrays: TglDrawArrays;
  glDrawBuffer: TglDrawBuffer;
  glDrawElements: TglDrawElements;
  glDrawPixels: TglDrawPixels;
  glEdgeFlag: TglEdgeFlag;
  glEdgeFlagPointer: TglEdgeFlagPointer;
  glEdgeFlagv: TglEdgeFlagv;
  glEnable: TglEnable;
  glEnableClientState: TglEnableClientState;
  glEnd: TglEnd;
  glEndList: TglEndList;
  glEvalCoord1d: TglEvalCoord1d;
  glEvalCoord1dv: TglEvalCoord1dv;
  glEvalCoord1f: TglEvalCoord1f;
  glEvalCoord1fv: TglEvalCoord1fv;
  glEvalCoord2d: TglEvalCoord2d;
  glEvalCoord2dv: TglEvalCoord2dv;
  glEvalCoord2f: TglEvalCoord2f;
  glEvalCoord2fv: TglEvalCoord2fv;
  glEvalMesh1: TglEvalMesh1;
  glEvalMesh2: TglEvalMesh2;
  glEvalPoint1: TglEvalPoint1;
  glEvalPoint2: TglEvalPoint2;
  glFeedbackBuffer: TglFeedbackBuffer;
  glFinish: TglFinish;
  glFlush: TglFlush;
  glFogf: TglFogf;
  glFogfv: TglFogfv;
  glFogi: TglFogi;
  glFogiv: TglFogiv;
  glFrontFace: TglFrontFace;
  glFrustum: TglFrustum;
  glGenLists: TglGenLists;
  glGenTextures: TglGenTextures;
  glGetBooleanv: TglGetBooleanv;
  glGetClipPlane: TglGetClipPlane;
  glGetDoublev: TglGetDoublev;
  glGetError: TglGetError;
  glGetFloatv: TglGetFloatv;
  glGetIntegerv: TglGetIntegerv;
  glGetLightfv: TglGetLightfv;
  glGetLightiv: TglGetLightiv;
  glGetMapdv: TglGetMapdv;
  glGetMapfv: TglGetMapfv;
  glGetMapiv: TglGetMapiv;
  glGetMaterialfv: TglGetMaterialfv;
  glGetMaterialiv: TglGetMaterialiv;
  glGetPixelMapfv: TglGetPixelMapfv;
  glGetPixelMapuiv: TglGetPixelMapuiv;
  glGetPixelMapusv: TglGetPixelMapusv;
  glGetPointerv: TglGetPointerv;
  glGetPolygonStipple: TglGetPolygonStipple;
  glGetString: TglGetString;
  glGetTexEnvfv: TglGetTexEnvfv;
  glGetTexEnviv: TglGetTexEnviv;
  glGetTexGendv: TglGetTexGendv;
  glGetTexGenfv: TglGetTexGenfv;
  glGetTexGeniv: TglGetTexGeniv;
  glGetTexImage: TglGetTexImage;
  glGetTexLevelParameterfv: TglGetTexLevelParameterfv;
  glGetTexLevelParameteriv: TglGetTexLevelParameteriv;
  glGetTexParameterfv: TglGetTexParameterfv;
  glGetTexParameteriv: TglGetTexParameteriv;
  glHint: TglHint;
  glIndexMask: TglIndexMask;
  glIndexPointer: TglIndexPointer;
  glIndexd: TglIndexd;
  glIndexdv: TglIndexdv;
  glIndexf: TglIndexf;
  glIndexfv: TglIndexfv;
  glIndexi: TglIndexi;
  glIndexiv: TglIndexiv;
  glIndexs: TglIndexs;
  glIndexsv: TglIndexsv;
  glIndexub: TglIndexub;
  glIndexubv: TglIndexubv;
  glInitNames: TglInitNames;
  glInterleavedArrays: TglInterleavedArrays;
  glIsEnabled: TglIsEnabled;
  glIsList: TglIsList;
  glIsTexture: TglIsTexture;
  glLightModelf: TglLightModelf;
  glLightModelfv: TglLightModelfv;
  glLightModeli: TglLightModeli;
  glLightModeliv: TglLightModeliv;
  glLightf: TglLightf;
  glLightfv: TglLightfv;
  glLighti: TglLighti;
  glLightiv: TglLightiv;
  glLineStipple: TglLineStipple;
  glLineWidth: TglLineWidth;
  glListBase: TglListBase;
  glLoadIdentity: TglLoadIdentity;
  glLoadMatrixd: TglLoadMatrixd;
  glLoadMatrixf: TglLoadMatrixf;
  glLoadName: TglLoadName;
  glLogicOp: TglLogicOp;
  glMap1d: TglMap1d;
  glMap1f: TglMap1f;
  glMap2d: TglMap2d;
  glMap2f: TglMap2f;
  glMapGrid1d: TglMapGrid1d;
  glMapGrid1f: TglMapGrid1f;
  glMapGrid2d: TglMapGrid2d;
  glMapGrid2f: TglMapGrid2f;
  glMaterialf: TglMaterialf;
  glMaterialfv: TglMaterialfv;
  glMateriali: TglMateriali;
  glMaterialiv: TglMaterialiv;
  glMatrixMode: TglMatrixMode;
  glMultMatrixd: TglMultMatrixd;
  glMultMatrixf: TglMultMatrixf;
  glNewList: TglNewList;
  glNormal3b: TglNormal3b;
  glNormal3bv: TglNormal3bv;
  glNormal3d: TglNormal3d;
  glNormal3dv: TglNormal3dv;
  glNormal3f: TglNormal3f;
  glNormal3fv: TglNormal3fv;
  glNormal3i: TglNormal3i;
  glNormal3iv: TglNormal3iv;
  glNormal3s: TglNormal3s;
  glNormal3sv: TglNormal3sv;
  glNormalPointer: TglNormalPointer;
  glOrtho: TglOrtho;
  glPassThrough: TglPassThrough;
  glPixelMapfv: TglPixelMapfv;
  glPixelMapuiv: TglPixelMapuiv;
  glPixelMapusv: TglPixelMapusv;
  glPixelStoref: TglPixelStoref;
  glPixelStorei: TglPixelStorei;
  glPixelTransferf: TglPixelTransferf;
  glPixelTransferi: TglPixelTransferi;
  glPixelZoom: TglPixelZoom;
  glPointSize: TglPointSize;
  glPolygonMode: TglPolygonMode;
  glPolygonOffset: TglPolygonOffset;
  glPolygonStipple: TglPolygonStipple;
  glPopAttrib: TglPopAttrib;
  glPopClientAttrib: TglPopClientAttrib;
  glPopMatrix: TglPopMatrix;
  glPopName: TglPopName;
  glPrioritizeTextures: TglPrioritizeTextures;
  glPushAttrib: TglPushAttrib;
  glPushClientAttrib: TglPushClientAttrib;
  glPushMatrix: TglPushMatrix;
  glPushName: TglPushName;
  glRasterPos2d: TglRasterPos2d;
  glRasterPos2dv: TglRasterPos2dv;
  glRasterPos2f: TglRasterPos2f;
  glRasterPos2fv: TglRasterPos2fv;
  glRasterPos2i: TglRasterPos2i;
  glRasterPos2iv: TglRasterPos2iv;
  glRasterPos2s: TglRasterPos2s;
  glRasterPos2sv: TglRasterPos2sv;
  glRasterPos3d: TglRasterPos3d;
  glRasterPos3dv: TglRasterPos3dv;
  glRasterPos3f: TglRasterPos3f;
  glRasterPos3fv: TglRasterPos3fv;
  glRasterPos3i: TglRasterPos3i;
  glRasterPos3iv: TglRasterPos3iv;
  glRasterPos3s: TglRasterPos3s;
  glRasterPos3sv: TglRasterPos3sv;
  glRasterPos4d: TglRasterPos4d;
  glRasterPos4dv: TglRasterPos4dv;
  glRasterPos4f: TglRasterPos4f;
  glRasterPos4fv: TglRasterPos4fv;
  glRasterPos4i: TglRasterPos4i;
  glRasterPos4iv: TglRasterPos4iv;
  glRasterPos4s: TglRasterPos4s;
  glRasterPos4sv: TglRasterPos4sv;
  glReadBuffer: TglReadBuffer;
  glReadPixels: TglReadPixels;
  glRectd: TglRectd;
  glRectdv: TglRectdv;
  glRectf: TglRectf;
  glRectfv: TglRectfv;
  glRecti: TglRecti;
  glRectiv: TglRectiv;
  glRects: TglRects;
  glRectsv: TglRectsv;
  glRenderMode: TglRenderMode;
  glRotated: TglRotated;
  glRotatef: TglRotatef;
  glScaled: TglScaled;
  glScalef: TglScalef;
  glScissor: TglScissor;
  glSelectBuffer: TglSelectBuffer;
  glShadeModel: TglShadeModel;
  glStencilFunc: TglStencilFunc;
  glStencilMask: TglStencilMask;
  glStencilOp: TglStencilOp;
  glTexCoord1d: TglTexCoord1d;
  glTexCoord1dv: TglTexCoord1dv;
  glTexCoord1f: TglTexCoord1f;
  glTexCoord1fv: TglTexCoord1fv;
  glTexCoord1i: TglTexCoord1i;
  glTexCoord1iv: TglTexCoord1iv;
  glTexCoord1s: TglTexCoord1s;
  glTexCoord1sv: TglTexCoord1sv;
  glTexCoord2d: TglTexCoord2d;
  glTexCoord2dv: TglTexCoord2dv;
  glTexCoord2f: TglTexCoord2f;
  glTexCoord2fv: TglTexCoord2fv;
  glTexCoord2i: TglTexCoord2i;
  glTexCoord2iv: TglTexCoord2iv;
  glTexCoord2s: TglTexCoord2s;
  glTexCoord2sv: TglTexCoord2sv;
  glTexCoord3d: TglTexCoord3d;
  glTexCoord3dv: TglTexCoord3dv;
  glTexCoord3f: TglTexCoord3f;
  glTexCoord3fv: TglTexCoord3fv;
  glTexCoord3i: TglTexCoord3i;
  glTexCoord3iv: TglTexCoord3iv;
  glTexCoord3s: TglTexCoord3s;
  glTexCoord3sv: TglTexCoord3sv;
  glTexCoord4d: TglTexCoord4d;
  glTexCoord4dv: TglTexCoord4dv;
  glTexCoord4f: TglTexCoord4f;
  glTexCoord4fv: TglTexCoord4fv;
  glTexCoord4i: TglTexCoord4i;
  glTexCoord4iv: TglTexCoord4iv;
  glTexCoord4s: TglTexCoord4s;
  glTexCoord4sv: TglTexCoord4sv;
  glTexCoordPointer: TglTexCoordPointer;
  glTexEnvf: TglTexEnvf;
  glTexEnvfv: TglTexEnvfv;
  glTexEnvi: TglTexEnvi;
  glTexEnviv: TglTexEnviv;
  glTexGend: TglTexGend;
  glTexGendv: TglTexGendv;
  glTexGenf: TglTexGenf;
  glTexGenfv: TglTexGenfv;
  glTexGeni: TglTexGeni;
  glTexGeniv: TglTexGeniv;
  glTexImage1D: TglTexImage1D;
  glTexImage2D: TglTexImage2D;
  glTexParameterf: TglTexParameterf;
  glTexParameterfv: TglTexParameterfv;
  glTexParameteri: TglTexParameteri;
  glTexParameteriv: TglTexParameteriv;
  glTexSubImage1D: TglTexSubImage1D;
  glTexSubImage2D: TglTexSubImage2D;
  glTranslated: TglTranslated;
  glTranslatef: TglTranslatef;
  glVertex2d: TglVertex2d;
  glVertex2dv: TglVertex2dv;
  glVertex2f: TglVertex2f;
  glVertex2fv: TglVertex2fv;
  glVertex2i: TglVertex2i;
  glVertex2iv: TglVertex2iv;
  glVertex2s: TglVertex2s;
  glVertex2sv: TglVertex2sv;
  glVertex3d: TglVertex3d;
  glVertex3dv: TglVertex3dv;
  glVertex3f: TglVertex3f;
  glVertex3fv: TglVertex3fv;
  glVertex3i: TglVertex3i;
  glVertex3iv: TglVertex3iv;
  glVertex3s: TglVertex3s;
  glVertex3sv: TglVertex3sv;
  glVertex4d: TglVertex4d;
  glVertex4dv: TglVertex4dv;
  glVertex4f: TglVertex4f;
  glVertex4fv: TglVertex4fv;
  glVertex4i: TglVertex4i;
  glVertex4iv: TglVertex4iv;
  glVertex4s: TglVertex4s;
  glVertex4sv: TglVertex4sv;
  glVertexPointer: TglVertexPointer;
  glViewport: TglViewport;

  // GL_VERSION_1_2
  glBlendColor: TglBlendColor;
  glBlendEquation: TglBlendEquation;
  glDrawRangeElements: TglDrawRangeElements;
  glColorTable: TglColorTable;
  glColorTableParameterfv: TglColorTableParameterfv;
  glColorTableParameteriv: TglColorTableParameteriv;
  glCopyColorTable: TglCopyColorTable;
  glGetColorTable: TglGetColorTable;
  glGetColorTableParameterfv: TglGetColorTableParameterfv;
  glGetColorTableParameteriv: TglGetColorTableParameteriv;
  glColorSubTable: TglColorSubTable;
  glCopyColorSubTable: TglCopyColorSubTable;
  glConvolutionFilter1D: TglConvolutionFilter1D;
  glConvolutionFilter2D: TglConvolutionFilter2D;
  glConvolutionParameterf: TglConvolutionParameterf;
  glConvolutionParameterfv: TglConvolutionParameterfv;
  glConvolutionParameteri: TglConvolutionParameteri;
  glConvolutionParameteriv: TglConvolutionParameteriv;
  glCopyConvolutionFilter1D: TglCopyConvolutionFilter1D;
  glCopyConvolutionFilter2D: TglCopyConvolutionFilter2D;
  glGetConvolutionFilter: TglGetConvolutionFilter;
  glGetConvolutionParameterfv: TglGetConvolutionParameterfv;
  glGetConvolutionParameteriv: TglGetConvolutionParameteriv;
  glGetSeparableFilter: TglGetSeparableFilter;
  glSeparableFilter2D: TglSeparableFilter2D;
  glGetHistogram: TglGetHistogram;
  glGetHistogramParameterfv: TglGetHistogramParameterfv;
  glGetHistogramParameteriv: TglGetHistogramParameteriv;
  glGetMinmax: TglGetMinmax;
  glGetMinmaxParameterfv: TglGetMinmaxParameterfv;
  glGetMinmaxParameteriv: TglGetMinmaxParameteriv;
  glHistogram: TglHistogram;
  glMinmax: TglMinmax;
  glResetHistogram: TglResetHistogram;
  glResetMinmax: TglResetMinmax;
  glTexImage3D: TglTexImage3D;
  glTexSubImage3D: TglTexSubImage3D;
  glCopyTexSubImage3D: TglCopyTexSubImage3D;

  // GL_VERSION_1_3
  glActiveTexture: TglActiveTexture;
  glClientActiveTexture: TglClientActiveTexture;
  glMultiTexCoord1d: TglMultiTexCoord1d;
  glMultiTexCoord1dv: TglMultiTexCoord1dv;
  glMultiTexCoord1f: TglMultiTexCoord1f;
  glMultiTexCoord1fv: TglMultiTexCoord1fv;
  glMultiTexCoord1i: TglMultiTexCoord1i;
  glMultiTexCoord1iv: TglMultiTexCoord1iv;
  glMultiTexCoord1s: TglMultiTexCoord1s;
  glMultiTexCoord1sv: TglMultiTexCoord1sv;
  glMultiTexCoord2d: TglMultiTexCoord2d;
  glMultiTexCoord2dv: TglMultiTexCoord2dv;
  glMultiTexCoord2f: TglMultiTexCoord2f;
  glMultiTexCoord2fv: TglMultiTexCoord2fv;
  glMultiTexCoord2i: TglMultiTexCoord2i;
  glMultiTexCoord2iv: TglMultiTexCoord2iv;
  glMultiTexCoord2s: TglMultiTexCoord2s;
  glMultiTexCoord2sv: TglMultiTexCoord2sv;
  glMultiTexCoord3d: TglMultiTexCoord3d;
  glMultiTexCoord3dv: TglMultiTexCoord3dv;
  glMultiTexCoord3f: TglMultiTexCoord3f;
  glMultiTexCoord3fv: TglMultiTexCoord3fv;
  glMultiTexCoord3i: TglMultiTexCoord3i;
  glMultiTexCoord3iv: TglMultiTexCoord3iv;
  glMultiTexCoord3s: TglMultiTexCoord3s;
  glMultiTexCoord3sv: TglMultiTexCoord3sv;
  glMultiTexCoord4d: TglMultiTexCoord4d;
  glMultiTexCoord4dv: TglMultiTexCoord4dv;
  glMultiTexCoord4f: TglMultiTexCoord4f;
  glMultiTexCoord4fv: TglMultiTexCoord4fv;
  glMultiTexCoord4i: TglMultiTexCoord4i;
  glMultiTexCoord4iv: TglMultiTexCoord4iv;
  glMultiTexCoord4s: TglMultiTexCoord4s;
  glMultiTexCoord4sv: TglMultiTexCoord4sv;
  glLoadTransposeMatrixf: TglLoadTransposeMatrixf;
  glLoadTransposeMatrixd: TglLoadTransposeMatrixd;
  glMultTransposeMatrixf: TglMultTransposeMatrixf;
  glMultTransposeMatrixd: TglMultTransposeMatrixd;
  glSampleCoverage: TglSampleCoverage;
  glCompressedTexImage3D: TglCompressedTexImage3D;
  glCompressedTexImage2D: TglCompressedTexImage2D;
  glCompressedTexImage1D: TglCompressedTexImage1D;
  glCompressedTexSubImage3D: TglCompressedTexSubImage3D;
  glCompressedTexSubImage2D: TglCompressedTexSubImage2D;
  glCompressedTexSubImage1D: TglCompressedTexSubImage1D;
  glGetCompressedTexImage: TglGetCompressedTexImage;

  // GL_VERSION_1_4
  glBlendFuncSeparate: TglBlendFuncSeparate;
  glFogCoordf: TglFogCoordf;
  glFogCoordfv: TglFogCoordfv;
  glFogCoordd: TglFogCoordd;
  glFogCoorddv: TglFogCoorddv;
  glFogCoordPointer: TglFogCoordPointer;
  glMultiDrawArrays: TglMultiDrawArrays;
  glMultiDrawElements: TglMultiDrawElements;
  glPointParameterf: TglPointParameterf;
  glPointParameterfv: TglPointParameterfv;
  glPointParameteri: TglPointParameteri;
  glPointParameteriv: TglPointParameteriv;
  glSecondaryColor3b: TglSecondaryColor3b;
  glSecondaryColor3bv: TglSecondaryColor3bv;
  glSecondaryColor3d: TglSecondaryColor3d;
  glSecondaryColor3dv: TglSecondaryColor3dv;
  glSecondaryColor3f: TglSecondaryColor3f;
  glSecondaryColor3fv: TglSecondaryColor3fv;
  glSecondaryColor3i: TglSecondaryColor3i;
  glSecondaryColor3iv: TglSecondaryColor3iv;
  glSecondaryColor3s: TglSecondaryColor3s;
  glSecondaryColor3sv: TglSecondaryColor3sv;
  glSecondaryColor3ub: TglSecondaryColor3ub;
  glSecondaryColor3ubv: TglSecondaryColor3ubv;
  glSecondaryColor3ui: TglSecondaryColor3ui;
  glSecondaryColor3uiv: TglSecondaryColor3uiv;
  glSecondaryColor3us: TglSecondaryColor3us;
  glSecondaryColor3usv: TglSecondaryColor3usv;
  glSecondaryColorPointer: TglSecondaryColorPointer;
  glWindowPos2d: TglWindowPos2d;
  glWindowPos2dv: TglWindowPos2dv;
  glWindowPos2f: TglWindowPos2f;
  glWindowPos2fv: TglWindowPos2fv;
  glWindowPos2i: TglWindowPos2i;
  glWindowPos2iv: TglWindowPos2iv;
  glWindowPos2s: TglWindowPos2s;
  glWindowPos2sv: TglWindowPos2sv;
  glWindowPos3d: TglWindowPos3d;
  glWindowPos3dv: TglWindowPos3dv;
  glWindowPos3f: TglWindowPos3f;
  glWindowPos3fv: TglWindowPos3fv;
  glWindowPos3i: TglWindowPos3i;
  glWindowPos3iv: TglWindowPos3iv;
  glWindowPos3s: TglWindowPos3s;
  glWindowPos3sv: TglWindowPos3sv;

  // GL_VERSION_1_5
  glGenQueries: TglGenQueries;
  glDeleteQueries: TglDeleteQueries;
  glIsQuery: TglIsQuery;
  glBeginQuery: TglBeginQuery;
  glEndQuery: TglEndQuery;
  glGetQueryiv: TglGetQueryiv;
  glGetQueryObjectiv: TglGetQueryObjectiv;
  glGetQueryObjectuiv: TglGetQueryObjectuiv;
  glBindBuffer: TglBindBuffer;
  glDeleteBuffers: TglDeleteBuffers;
  glGenBuffers: TglGenBuffers;
  glIsBuffer: TglIsBuffer;
  glBufferData: TglBufferData;
  glBufferSubData: TglBufferSubData;
  glGetBufferSubData: TglGetBufferSubData;
  glMapBuffer: TglMapBuffer;
  glUnmapBuffer: TglUnmapBuffer;
  glGetBufferParameteriv: TglGetBufferParameteriv;
  glGetBufferPointerv: TglGetBufferPointerv;

  // GL_VERSION_2_0
  glBlendEquationSeparate: TglBlendEquationSeparate;
  glDrawBuffers: TglDrawBuffers;
  glStencilOpSeparate: TglStencilOpSeparate;
  glStencilFuncSeparate: TglStencilFuncSeparate;
  glStencilMaskSeparate: TglStencilMaskSeparate;
  glAttachShader: TglAttachShader;
  glBindAttribLocation: TglBindAttribLocation;
  glCompileShader: TglCompileShader;
  glCreateProgram: TglCreateProgram;
  glCreateShader: TglCreateShader;
  glDeleteProgram: TglDeleteProgram;
  glDeleteShader: TglDeleteShader;
  glDetachShader: TglDetachShader;
  glDisableVertexAttribArray: TglDisableVertexAttribArray;
  glEnableVertexAttribArray: TglEnableVertexAttribArray;
  glGetActiveAttrib: TglGetActiveAttrib;
  glGetActiveUniform: TglGetActiveUniform;
  glGetAttachedShaders: TglGetAttachedShaders;
  glGetAttribLocation: TglGetAttribLocation;
  glGetProgramiv: TglGetProgramiv;
  glGetProgramInfoLog: TglGetProgramInfoLog;
  glGetShaderiv: TglGetShaderiv;
  glGetShaderInfoLog: TglGetShaderInfoLog;
  glGetShaderSource: TglGetShaderSource;
  glGetUniformLocation: TglGetUniformLocation;
  glGetUniformfv: TglGetUniformfv;
  glGetUniformiv: TglGetUniformiv;
  glGetVertexAttribfv: TglGetVertexAttribfv;
  glGetVertexAttribiv: TglGetVertexAttribiv;
  glGetVertexAttribPointerv: TglGetVertexAttribPointerv;
  glIsProgram: TglIsProgram;
  glIsShader: TglIsShader;
  glLinkProgram: TglLinkProgram;
  glShaderSource: TglShaderSource;
  glUseProgram: TglUseProgram;
  glUniform1f: TglUniform1f;
  glUniform2f: TglUniform2f;
  glUniform3f: TglUniform3f;
  glUniform4f: TglUniform4f;
  glUniform1i: TglUniform1i;
  glUniform2i: TglUniform2i;
  glUniform3i: TglUniform3i;
  glUniform4i: TglUniform4i;
  glUniform1fv: TglUniform1fv;
  glUniform2fv: TglUniform2fv;
  glUniform3fv: TglUniform3fv;
  glUniform4fv: TglUniform4fv;
  glUniform1iv: TglUniform1iv;
  glUniform2iv: TglUniform2iv;
  glUniform3iv: TglUniform3iv;
  glUniform4iv: TglUniform4iv;
  glUniformMatrix2fv: TglUniformMatrix2fv;
  glUniformMatrix3fv: TglUniformMatrix3fv;
  glUniformMatrix4fv: TglUniformMatrix4fv;
  glValidateProgram: TglValidateProgram;
  glVertexAttrib1d: TglVertexAttrib1d;
  glVertexAttrib1dv: TglVertexAttrib1dv;
  glVertexAttrib1f: TglVertexAttrib1f;
  glVertexAttrib1fv: TglVertexAttrib1fv;
  glVertexAttrib1s: TglVertexAttrib1s;
  glVertexAttrib1sv: TglVertexAttrib1sv;
  glVertexAttrib2d: TglVertexAttrib2d;
  glVertexAttrib2dv: TglVertexAttrib2dv;
  glVertexAttrib2f: TglVertexAttrib2f;
  glVertexAttrib2fv: TglVertexAttrib2fv;
  glVertexAttrib2s: TglVertexAttrib2s;
  glVertexAttrib2sv: TglVertexAttrib2sv;
  glVertexAttrib3d: TglVertexAttrib3d;
  glVertexAttrib3dv: TglVertexAttrib3dv;
  glVertexAttrib3f: TglVertexAttrib3f;
  glVertexAttrib3fv: TglVertexAttrib3fv;
  glVertexAttrib3s: TglVertexAttrib3s;
  glVertexAttrib3sv: TglVertexAttrib3sv;
  glVertexAttrib4Nbv: TglVertexAttrib4Nbv;
  glVertexAttrib4Niv: TglVertexAttrib4Niv;
  glVertexAttrib4Nsv: TglVertexAttrib4Nsv;
  glVertexAttrib4Nub: TglVertexAttrib4Nub;
  glVertexAttrib4Nubv: TglVertexAttrib4Nubv;
  glVertexAttrib4Nuiv: TglVertexAttrib4Nuiv;
  glVertexAttrib4Nusv: TglVertexAttrib4Nusv;
  glVertexAttrib4bv: TglVertexAttrib4bv;
  glVertexAttrib4d: TglVertexAttrib4d;
  glVertexAttrib4dv: TglVertexAttrib4dv;
  glVertexAttrib4f: TglVertexAttrib4f;
  glVertexAttrib4fv: TglVertexAttrib4fv;
  glVertexAttrib4iv: TglVertexAttrib4iv;
  glVertexAttrib4s: TglVertexAttrib4s;
  glVertexAttrib4sv: TglVertexAttrib4sv;
  glVertexAttrib4ubv: TglVertexAttrib4ubv;
  glVertexAttrib4uiv: TglVertexAttrib4uiv;
  glVertexAttrib4usv: TglVertexAttrib4usv;
  glVertexAttribPointer: TglVertexAttribPointer;

  // GL_VERSION_2_1
  glUniformMatrix2x3fv: TglUniformMatrix2x3fv;
  glUniformMatrix3x2fv: TglUniformMatrix3x2fv;
  glUniformMatrix2x4fv: TglUniformMatrix2x4fv;
  glUniformMatrix4x2fv: TglUniformMatrix4x2fv;
  glUniformMatrix3x4fv: TglUniformMatrix3x4fv;
  glUniformMatrix4x3fv: TglUniformMatrix4x3fv;

  // GL_VERSION_3_0
  glColorMaski: TglColorMaski;
  glGetBooleani_v: TglGetBooleani_v;
  glGetIntegeri_v: TglGetIntegeri_v;
  glEnablei: TglEnablei;
  glDisablei: TglDisablei;
  glIsEnabledi: TglIsEnabledi;
  glBeginTransformFeedback: TglBeginTransformFeedback;
  glEndTransformFeedback: TglEndTransformFeedback;
  glBindBufferRange: TglBindBufferRange;
  glBindBufferBase: TglBindBufferBase;
  glTransformFeedbackVaryings: TglTransformFeedbackVaryings;
  glGetTransformFeedbackVarying: TglGetTransformFeedbackVarying;
  glClampColor: TglClampColor;
  glBeginConditionalRender: TglBeginConditionalRender;
  glEndConditionalRender: TglEndConditionalRender;
  glVertexAttribI1i: TglVertexAttribI1i;
  glVertexAttribI2i: TglVertexAttribI2i;
  glVertexAttribI3i: TglVertexAttribI3i;
  glVertexAttribI4i: TglVertexAttribI4i;
  glVertexAttribI1ui: TglVertexAttribI1ui;
  glVertexAttribI2ui: TglVertexAttribI2ui;
  glVertexAttribI3ui: TglVertexAttribI3ui;
  glVertexAttribI4ui: TglVertexAttribI4ui;
  glVertexAttribI1iv: TglVertexAttribI1iv;
  glVertexAttribI2iv: TglVertexAttribI2iv;
  glVertexAttribI3iv: TglVertexAttribI3iv;
  glVertexAttribI4iv: TglVertexAttribI4iv;
  glVertexAttribI1uiv: TglVertexAttribI1uiv;
  glVertexAttribI2uiv: TglVertexAttribI2uiv;
  glVertexAttribI3uiv: TglVertexAttribI3uiv;
  glVertexAttribI4uiv: TglVertexAttribI4uiv;
  glVertexAttribI4bv: TglVertexAttribI4bv;
  glVertexAttribI4sv: TglVertexAttribI4sv;
  glVertexAttribI4ubv: TglVertexAttribI4ubv;
  glVertexAttribI4usv: TglVertexAttribI4usv;
  glVertexAttribIPointer: TglVertexAttribIPointer;
  glGetVertexAttribIiv: TglGetVertexAttribIiv;
  glGetVertexAttribIuiv: TglGetVertexAttribIuiv;
  glGetUniformuiv: TglGetUniformuiv;
  glBindFragDataLocation: TglBindFragDataLocation;
  glGetFragDataLocation: TglGetFragDataLocation;
  glUniform1ui: TglUniform1ui;
  glUniform2ui: TglUniform2ui;
  glUniform3ui: TglUniform3ui;
  glUniform4ui: TglUniform4ui;
  glUniform1uiv: TglUniform1uiv;
  glUniform2uiv: TglUniform2uiv;
  glUniform3uiv: TglUniform3uiv;
  glUniform4uiv: TglUniform4uiv;
  glTexParameterIiv: TglTexParameterIiv;
  glTexParameterIuiv: TglTexParameterIuiv;
  glGetTexParameterIiv: TglGetTexParameterIiv;
  glGetTexParameterIuiv: TglGetTexParameterIuiv;
  glClearBufferiv: TglClearBufferiv;
  glClearBufferuiv: TglClearBufferuiv;
  glClearBufferfv: TglClearBufferfv;
  glClearBufferfi: TglClearBufferfi;
  glGetStringi: TglGetStringi;

  // GL_3DFX_tbuffer
  glTbufferMask3DFX: TglTbufferMask3DFX;

  // GL_APPLE_element_array
  glElementPointerAPPLE: TglElementPointerAPPLE;
  glDrawElementArrayAPPLE: TglDrawElementArrayAPPLE;
  glDrawRangeElementArrayAPPLE: TglDrawRangeElementArrayAPPLE;
  glMultiDrawElementArrayAPPLE: TglMultiDrawElementArrayAPPLE;
  glMultiDrawRangeElementArrayAPPLE: TglMultiDrawRangeElementArrayAPPLE;

  // GL_APPLE_fence
  glGenFencesAPPLE: TglGenFencesAPPLE;
  glDeleteFencesAPPLE: TglDeleteFencesAPPLE;
  glSetFenceAPPLE: TglSetFenceAPPLE;
  glIsFenceAPPLE: TglIsFenceAPPLE;
  glTestFenceAPPLE: TglTestFenceAPPLE;
  glFinishFenceAPPLE: TglFinishFenceAPPLE;
  glTestObjectAPPLE: TglTestObjectAPPLE;
  glFinishObjectAPPLE: TglFinishObjectAPPLE;

  // GL_APPLE_vertex_array_object
  glBindVertexArrayAPPLE: TglBindVertexArrayAPPLE;
  glDeleteVertexArraysAPPLE: TglDeleteVertexArraysAPPLE;
  glGenVertexArraysAPPLE: TglGenVertexArraysAPPLE;
  glIsVertexArrayAPPLE: TglIsVertexArrayAPPLE;

  // GL_APPLE_vertex_array_range
  glVertexArrayRangeAPPLE: TglVertexArrayRangeAPPLE;
  glFlushVertexArrayRangeAPPLE: TglFlushVertexArrayRangeAPPLE;
  glVertexArrayParameteriAPPLE: TglVertexArrayParameteriAPPLE;

  // GL_ARB_matrix_palette
  glCurrentPaletteMatrixARB: TglCurrentPaletteMatrixARB;
  glMatrixIndexubvARB: TglMatrixIndexubvARB;
  glMatrixIndexusvARB: TglMatrixIndexusvARB;
  glMatrixIndexuivARB: TglMatrixIndexuivARB;
  glMatrixIndexPointerARB: TglMatrixIndexPointerARB;

  // GL_ARB_multisample
  glSampleCoverageARB: TglSampleCoverageARB;

  // GL_ARB_multitexture
  glActiveTextureARB: TglActiveTextureARB;
  glClientActiveTextureARB: TglClientActiveTextureARB;
  glMultiTexCoord1dARB: TglMultiTexCoord1dARB;
  glMultiTexCoord1dvARB: TglMultiTexCoord1dvARB;
  glMultiTexCoord1fARB: TglMultiTexCoord1fARB;
  glMultiTexCoord1fvARB: TglMultiTexCoord1fvARB;
  glMultiTexCoord1iARB: TglMultiTexCoord1iARB;
  glMultiTexCoord1ivARB: TglMultiTexCoord1ivARB;
  glMultiTexCoord1sARB: TglMultiTexCoord1sARB;
  glMultiTexCoord1svARB: TglMultiTexCoord1svARB;
  glMultiTexCoord2dARB: TglMultiTexCoord2dARB;
  glMultiTexCoord2dvARB: TglMultiTexCoord2dvARB;
  glMultiTexCoord2fARB: TglMultiTexCoord2fARB;
  glMultiTexCoord2fvARB: TglMultiTexCoord2fvARB;
  glMultiTexCoord2iARB: TglMultiTexCoord2iARB;
  glMultiTexCoord2ivARB: TglMultiTexCoord2ivARB;
  glMultiTexCoord2sARB: TglMultiTexCoord2sARB;
  glMultiTexCoord2svARB: TglMultiTexCoord2svARB;
  glMultiTexCoord3dARB: TglMultiTexCoord3dARB;
  glMultiTexCoord3dvARB: TglMultiTexCoord3dvARB;
  glMultiTexCoord3fARB: TglMultiTexCoord3fARB;
  glMultiTexCoord3fvARB: TglMultiTexCoord3fvARB;
  glMultiTexCoord3iARB: TglMultiTexCoord3iARB;
  glMultiTexCoord3ivARB: TglMultiTexCoord3ivARB;
  glMultiTexCoord3sARB: TglMultiTexCoord3sARB;
  glMultiTexCoord3svARB: TglMultiTexCoord3svARB;
  glMultiTexCoord4dARB: TglMultiTexCoord4dARB;
  glMultiTexCoord4dvARB: TglMultiTexCoord4dvARB;
  glMultiTexCoord4fARB: TglMultiTexCoord4fARB;
  glMultiTexCoord4fvARB: TglMultiTexCoord4fvARB;
  glMultiTexCoord4iARB: TglMultiTexCoord4iARB;
  glMultiTexCoord4ivARB: TglMultiTexCoord4ivARB;
  glMultiTexCoord4sARB: TglMultiTexCoord4sARB;
  glMultiTexCoord4svARB: TglMultiTexCoord4svARB;

  // GL_ARB_point_parameters
  glPointParameterfARB: TglPointParameterfARB;
  glPointParameterfvARB: TglPointParameterfvARB;

  // GL_ARB_texture_compression
  glCompressedTexImage3DARB: TglCompressedTexImage3DARB;
  glCompressedTexImage2DARB: TglCompressedTexImage2DARB;
  glCompressedTexImage1DARB: TglCompressedTexImage1DARB;
  glCompressedTexSubImage3DARB: TglCompressedTexSubImage3DARB;
  glCompressedTexSubImage2DARB: TglCompressedTexSubImage2DARB;
  glCompressedTexSubImage1DARB: TglCompressedTexSubImage1DARB;
  glGetCompressedTexImageARB: TglGetCompressedTexImageARB;

  // GL_ARB_transpose_matrix
  glLoadTransposeMatrixfARB: TglLoadTransposeMatrixfARB;
  glLoadTransposeMatrixdARB: TglLoadTransposeMatrixdARB;
  glMultTransposeMatrixfARB: TglMultTransposeMatrixfARB;
  glMultTransposeMatrixdARB: TglMultTransposeMatrixdARB;

  // GL_ARB_vertex_blend
  glWeightbvARB: TglWeightbvARB;
  glWeightsvARB: TglWeightsvARB;
  glWeightivARB: TglWeightivARB;
  glWeightfvARB: TglWeightfvARB;
  glWeightdvARB: TglWeightdvARB;
  glWeightubvARB: TglWeightubvARB;
  glWeightusvARB: TglWeightusvARB;
  glWeightuivARB: TglWeightuivARB;
  glWeightPointerARB: TglWeightPointerARB;
  glVertexBlendARB: TglVertexBlendARB;

  // GL_ARB_vertex_buffer_object
  glBindBufferARB: TglBindBufferARB;
  glDeleteBuffersARB: TglDeleteBuffersARB;
  glGenBuffersARB: TglGenBuffersARB;
  glIsBufferARB: TglIsBufferARB;
  glBufferDataARB: TglBufferDataARB;
  glBufferSubDataARB: TglBufferSubData;
  glGetBufferSubDataARB: TglGetBufferSubDataARB;
  glMapBufferARB: TglMapBufferARB;
  glUnmapBufferARB: TglUnmapBufferARB;
  glGetBufferParameterivARB: TglGetBufferParameterivARB;
  glGetBufferPointervARB: TglGetBufferPointervARB;

  // GL_ARB_vertex_program
  glVertexAttrib1dARB: TglVertexAttrib1dARB;
  glVertexAttrib1dvARB: TglVertexAttrib1dvARB;
  glVertexAttrib1fARB: TglVertexAttrib1fARB;
  glVertexAttrib1fvARB: TglVertexAttrib1fvARB;
  glVertexAttrib1sARB: TglVertexAttrib1sARB;
  glVertexAttrib1svARB: TglVertexAttrib1svARB;
  glVertexAttrib2dARB: TglVertexAttrib2dARB;
  glVertexAttrib2dvARB: TglVertexAttrib2dvARB;
  glVertexAttrib2fARB: TglVertexAttrib2fARB;
  glVertexAttrib2fvARB: TglVertexAttrib2fvARB;
  glVertexAttrib2sARB: TglVertexAttrib2sARB;
  glVertexAttrib2svARB: TglVertexAttrib2svARB;
  glVertexAttrib3dARB: TglVertexAttrib3dARB;
  glVertexAttrib3dvARB: TglVertexAttrib3dvARB;
  glVertexAttrib3fARB: TglVertexAttrib3fARB;
  glVertexAttrib3fvARB: TglVertexAttrib3fvARB;
  glVertexAttrib3sARB: TglVertexAttrib3sARB;
  glVertexAttrib3svARB: TglVertexAttrib3svARB;
  glVertexAttrib4NbvARB: TglVertexAttrib4NbvARB;
  glVertexAttrib4NivARB: TglVertexAttrib4NivARB;
  glVertexAttrib4NsvARB: TglVertexAttrib4NsvARB;
  glVertexAttrib4NubARB: TglVertexAttrib4NubARB;
  glVertexAttrib4NubvARB: TglVertexAttrib4NubvARB;
  glVertexAttrib4NuivARB: TglVertexAttrib4NuivARB;
  glVertexAttrib4NusvARB: TglVertexAttrib4NusvARB;
  glVertexAttrib4bvARB: TglVertexAttrib4bvARB;
  glVertexAttrib4dARB: TglVertexAttrib4dARB;
  glVertexAttrib4dvARB: TglVertexAttrib4dvARB;
  glVertexAttrib4fARB: TglVertexAttrib4fARB;
  glVertexAttrib4fvARB: TglVertexAttrib4fvARB;
  glVertexAttrib4ivARB: TglVertexAttrib4ivARB;
  glVertexAttrib4sARB: TglVertexAttrib4sARB;
  glVertexAttrib4svARB: TglVertexAttrib4svARB;
  glVertexAttrib4ubvARB: TglVertexAttrib4ubvARB;
  glVertexAttrib4uivARB: TglVertexAttrib4uivARB;
  glVertexAttrib4usvARB: TglVertexAttrib4usvARB;
  glVertexAttribPointerARB: TglVertexAttribPointerARB;
  glEnableVertexAttribArrayARB: TglEnableVertexAttribArrayARB;
  glDisableVertexAttribArrayARB: TglDisableVertexAttribArrayARB;
  glProgramStringARB: TglProgramStringARB;
  glBindProgramARB: TglBindProgramARB;
  glDeleteProgramsARB: TglDeleteProgramsARB;
  glGenProgramsARB: TglGenProgramsARB;

  glProgramEnvParameter4dARB: TglProgramEnvParameter4dARB;
  glProgramEnvParameter4dvARB: TglProgramEnvParameter4dvARB;
  glProgramEnvParameter4fARB: TglProgramEnvParameter4fARB;
  glProgramEnvParameter4fvARB: TglProgramEnvParameter4fvARB;
  glProgramLocalParameter4dARB: TglProgramLocalParameter4dARB;
  glProgramLocalParameter4dvARB: TglProgramLocalParameter4dvARB;
  glProgramLocalParameter4fARB: TglProgramLocalParameter4fARB;
  glProgramLocalParameter4fvARB: TglProgramLocalParameter4fvARB;
  glGetProgramEnvParameterdvARB: TglGetProgramEnvParameterdvARB;
  glGetProgramEnvParameterfvARB: TglGetProgramEnvParameterfvARB;
  glGetProgramLocalParameterdvARB: TglGetProgramLocalParameterdvARB;
  glGetProgramLocalParameterfvARB: TglGetProgramLocalParameterfvARB;
  glGetProgramivARB: TglGetProgramivARB;
  glGetProgramStringARB: TglGetProgramStringARB;
  glGetVertexAttribdvARB: TglGetVertexAttribdvARB;
  glGetVertexAttribfvARB: TglGetVertexAttribfvARB;
  glGetVertexAttribivARB: TglGetVertexAttribivARB;
  glGetVertexAttribPointervARB: TglGetVertexAttribPointervARB;
  glIsProgramARB: TglIsProgramARB;

  // GL_ARB_window_pos
  glWindowPos2dARB: TglWindowPos2dARB;
  glWindowPos2dvARB: TglWindowPos2dvARB;
  glWindowPos2fARB: TglWindowPos2fARB;
  glWindowPos2fvARB: TglWindowPos2fvARB;
  glWindowPos2iARB: TglWindowPos2iARB;
  glWindowPos2ivARB: TglWindowPos2ivARB;
  glWindowPos2sARB: TglWindowPos2sARB;
  glWindowPos2svARB: TglWindowPos2svARB;
  glWindowPos3dARB: TglWindowPos3dARB;
  glWindowPos3dvARB: TglWindowPos3dvARB;
  glWindowPos3fARB: TglWindowPos3fARB;
  glWindowPos3fvARB: TglWindowPos3fvARB;
  glWindowPos3iARB: TglWindowPos3iARB;
  glWindowPos3ivARB: TglWindowPos3ivARB;
  glWindowPos3sARB: TglWindowPos3sARB;
  glWindowPos3svARB: TglWindowPos3svARB;

  // GL_ARB_draw_buffers
  glDrawBuffersARB: TglDrawBuffersARB;

  // GL_ARB_color_buffer_float
  glClampColorARB: TglClampColorARB;

  // GL_ARB_vertex_shader
  glGetActiveAttribARB: TglGetActiveAttribARB;
  glGetAttribLocationARB: TglGetAttribLocationARB;
  glBindAttribLocationARB: TglBindAttribLocationARB;

  // GL_ARB_shader_objects
  glDeleteObjectARB: TglDeleteObjectARB;
  glGetHandleARB: TglGetHandleARB;
  glDetachObjectARB: TglDetachObjectARB;
  glCreateShaderObjectARB: TglCreateShaderObjectARB;
  glShaderSourceARB: TglShaderSourceARB;
  glCompileShaderARB: TglCompileShaderARB;
  glCreateProgramObjectARB: TglCreateProgramObjectARB;
  glAttachObjectARB: TglAttachObjectARB;
  glLinkProgramARB: TglLinkProgramARB;
  glUseProgramObjectARB: TglUseProgramObjectARB;
  glValidateProgramARB: TglValidateProgramARB;
  glUniform1fARB: TglUniform1fARB;
  glUniform2fARB: TglUniform2fARB;
  glUniform3fARB: TglUniform3fARB;
  glUniform4fARB: TglUniform4fARB;
  glUniform1iARB: TglUniform1iARB;
  glUniform2iARB: TglUniform2iARB;
  glUniform3iARB: TglUniform3iARB;
  glUniform4iARB: TglUniform4iARB;
  glUniform1fvARB: TglUniform1fvARB;
  glUniform2fvARB: TglUniform2fvARB;
  glUniform3fvARB: TglUniform3fvARB;
  glUniform4fvARB: TglUniform4fvARB;
  glUniform1ivARB: TglUniform1ivARB;
  glUniform2ivARB: TglUniform2ivARB;
  glUniform3ivARB: TglUniform3ivARB;
  glUniform4ivARB: TglUniform4ivARB;
  glUniformMatrix2fvARB: TglUniformMatrix2fvARB;
  glUniformMatrix3fvARB: TglUniformMatrix3fvARB;
  glUniformMatrix4fvARB: TglUniformMatrix4fvARB;
  glGetObjectParameterfvARB: TglGetObjectParameterfvARB;
  glGetObjectParameterivARB: TglGetObjectParameterivARB;
  glGetInfoLogARB: TglGetInfoLogARB;
  glGetAttachedObjectsARB: TglGetAttachedObjectsARB;
  glGetUniformLocationARB: TglGetUniformLocationARB;
  glGetActiveUniformARB: TglGetActiveUniformARB;
  glGetUniformfvARB: TglGetUniformfvARB;
  glGetUniformivARB: TglGetUniformivARB;
  glGetShaderSourceARB: TglGetShaderSourceARB;

  // GL_ARB_Occlusion_Query
  glGenQueriesARB: TglGenQueriesARB;
  glDeleteQueriesARB: TglDeleteQueriesARB;
  glIsQueryARB: TglIsQueryARB;
  glBeginQueryARB: TglBeginQueryARB;
  glEndQueryARB: TglEndQueryARB;
  glGetQueryivARB: TglGetQueryivARB;
  glGetQueryObjectivARB: TglGetQueryObjectivARB;
  glGetQueryObjectuivARB: TglGetQueryObjectuivARB;

  // GL_ARB_draw_instanced
  glDrawArraysInstancedARB: TglDrawArraysInstancedARB;
  glDrawElementsInstancedARB: TglDrawElementsInstancedARB;

  // GL_ARB_framebuffer_object
  glIsRenderbuffer: TglIsRenderbuffer;
  glBindRenderbuffer: TglBindRenderbuffer;
  glDeleteRenderbuffers: TglDeleteRenderbuffers;
  glGenRenderbuffers: TglGenRenderbuffers;
  glRenderbufferStorage: TglRenderbufferStorage;
  glGetRenderbufferParameteriv: TglGetRenderbufferParameteriv;
  glIsFramebuffer: TglIsFramebuffer;
  glBindFramebuffer: TglBindFramebuffer;
  glDeleteFramebuffers: TglDeleteFramebuffers;
  glGenFramebuffers: TglGenFramebuffers;
  glCheckFramebufferStatus: TglCheckFramebufferStatus;
  glFramebufferTexture1D: TglFramebufferTexture1D;
  glFramebufferTexture2D: TglFramebufferTexture2D;
  glFramebufferTexture3D: TglFramebufferTexture3D;
  glFramebufferRenderbuffer: TglFramebufferRenderbuffer;
  glGetFramebufferAttachmentParameteriv: TglGetFramebufferAttachmentParameteriv;
  glGenerateMipmap: TglGenerateMipmap;
  glBlitFramebuffer: TglBlitFramebuffer;
  glRenderbufferStorageMultisample: TglRenderbufferStorageMultisample;
  glFramebufferTextureLayer: TglFramebufferTextureLayer;

  // GL_ARB_geometry_shader4
  glProgramParameteriARB: TglProgramParameteriARB;
  glFramebufferTextureARB: TglFramebufferTextureARB;
  glFramebufferTextureLayerARB: TglFramebufferTextureLayerARB;
  glFramebufferTextureFaceARB: TglFramebufferTextureFaceARB;

  // GL_ARB_instanced_arrays
  glVertexAttribDivisor: TglVertexAttribDivisor;

  // GL_ARB_map_buffer_range
  glMapBufferRange: TglMapBufferRange;
  glFlushMappedBufferRange: TglFlushMappedBufferRange;

  // GL_ARB_texture_buffer_object
  glTexBufferARB: TglTexBufferARB;

  // GL_ARB_vertex_array_object
  glBindVertexArray: TglBindVertexArray;
  glDeleteVertexArrays: TglDeleteVertexArrays;
  glGenVertexArrays: TglGenVertexArrays;
  glIsVertexArray: TglIsVertexArray;

  // GL_ATI_draw_buffers
  glDrawBuffersATI: TglDrawBuffersATI;

  // GL_ATI_element_array
  glElementPointerATI: TglElementPointerATI;
  glDrawElementArrayATI: TglDrawElementArrayATI;
  glDrawRangeElementArrayATI: TglDrawRangeElementArrayATI;

  // GL_ATI_envmap_bumpmap
  glTexBumpParameterivATI: TglTexBumpParameterivATI;
  glTexBumpParameterfvATI: TglTexBumpParameterfvATI;
  glGetTexBumpParameterivATI: TglGetTexBumpParameterivATI;
  glGetTexBumpParameterfvATI: TglGetTexBumpParameterfvATI;

  // GL_ATI_fragment_shader
  glGenFragmentShadersATI: TglGenFragmentShadersATI;
  glBindFragmentShaderATI: TglBindFragmentShaderATI;
  glDeleteFragmentShaderATI: TglDeleteFragmentShaderATI;
  glBeginFragmentShaderATI: TglBeginFragmentShaderATI;
  glEndFragmentShaderATI: TglEndFragmentShaderATI;
  glPassTexCoordATI: TglPassTexCoordATI;
  glSampleMapATI: TglSampleMapATI;
  glColorFragmentOp1ATI: TglColorFragmentOp1ATI;
  glColorFragmentOp2ATI: TglColorFragmentOp2ATI;
  glColorFragmentOp3ATI: TglColorFragmentOp3ATI;
  glAlphaFragmentOp1ATI: TglAlphaFragmentOp1ATI;
  glAlphaFragmentOp2ATI: TglAlphaFragmentOp2ATI;
  glAlphaFragmentOp3ATI: TglAlphaFragmentOp3ATI;
  glSetFragmentShaderConstantATI: TglSetFragmentShaderConstantATI;

  // GL_ATI_map_object_buffer
  glMapObjectBufferATI: TglMapObjectBufferATI;
  glUnmapObjectBufferATI: TglUnmapObjectBufferATI;

  // GL_ATI_pn_triangles
  glPNTrianglesiATI: TglPNTrianglesiATI;
  glPNTrianglesfATI: TglPNTrianglesfATI;

  // GL_ATI_separate_stencil
  glStencilOpSeparateATI: TglStencilOpSeparateATI;
  glStencilFuncSeparateATI: TglStencilFuncSeparateATI;

  // GL_ATI_vertex_array_object
  glNewObjectBufferATI: TglNewObjectBufferATI;
  glIsObjectBufferATI: TglIsObjectBufferATI;
  glUpdateObjectBufferATI: TglUpdateObjectBufferATI;
  glGetObjectBufferfvATI: TglGetObjectBufferfvATI;
  glGetObjectBufferivATI: TglGetObjectBufferivATI;
  glFreeObjectBufferATI: TglFreeObjectBufferATI;
  glArrayObjectATI: TglArrayObjectATI;
  glGetArrayObjectfvATI: TglGetArrayObjectfvATI;
  glGetArrayObjectivATI: TglGetArrayObjectivATI;
  glVariantArrayObjectATI: TglVariantArrayObjectATI;
  glGetVariantArrayObjectfvATI: TglGetVariantArrayObjectfvATI;
  glGetVariantArrayObjectivATI: TglGetVariantArrayObjectivATI;
  glVertexAttribArrayObjectATI: TglVertexAttribArrayObjectATI;
  glGetVertexAttribArrayObjectfvATI: TglGetVertexAttribArrayObjectfvATI;
  glGetVertexAttribArrayObjectivATI: TglGetVertexAttribArrayObjectivATI;

  // GL_ATI_vertex_streams
  glVertexStream1sATI: TglVertexStream1sATI;
  glVertexStream1svATI: TglVertexStream1svATI;
  glVertexStream1iATI: TglVertexStream1iATI;
  glVertexStream1ivATI: TglVertexStream1ivATI;
  glVertexStream1fATI: TglVertexStream1fATI;
  glVertexStream1fvATI: TglVertexStream1fvATI;
  glVertexStream1dATI: TglVertexStream1dATI;
  glVertexStream1dvATI: TglVertexStream1dvATI;
  glVertexStream2sATI: TglVertexStream2sATI;
  glVertexStream2svATI: TglVertexStream2svATI;
  glVertexStream2iATI: TglVertexStream2iATI;
  glVertexStream2ivATI: TglVertexStream2ivATI;
  glVertexStream2fATI: TglVertexStream2fATI;
  glVertexStream2fvATI: TglVertexStream2fvATI;
  glVertexStream2dATI: TglVertexStream2dATI;
  glVertexStream2dvATI: TglVertexStream2dvATI;
  glVertexStream3sATI: TglVertexStream3sATI;
  glVertexStream3svATI: TglVertexStream3svATI;
  glVertexStream3iATI: TglVertexStream3iATI;
  glVertexStream3ivATI: TglVertexStream3ivATI;
  glVertexStream3fATI: TglVertexStream3fATI;
  glVertexStream3fvATI: TglVertexStream3fvATI;
  glVertexStream3dATI: TglVertexStream3dATI;
  glVertexStream3dvATI: TglVertexStream3dvATI;
  glVertexStream4sATI: TglVertexStream4sATI;
  glVertexStream4svATI: TglVertexStream4svATI;
  glVertexStream4iATI: TglVertexStream4iATI;
  glVertexStream4ivATI: TglVertexStream4ivATI;
  glVertexStream4fATI: TglVertexStream4fATI;
  glVertexStream4fvATI: TglVertexStream4fvATI;
  glVertexStream4dATI: TglVertexStream4dATI;
  glVertexStream4dvATI: TglVertexStream4dvATI;
  glNormalStream3bATI: TglNormalStream3bATI;
  glNormalStream3bvATI: TglNormalStream3bvATI;
  glNormalStream3sATI: TglNormalStream3sATI;
  glNormalStream3svATI: TglNormalStream3svATI;
  glNormalStream3iATI: TglNormalStream3iATI;
  glNormalStream3ivATI: TglNormalStream3ivATI;
  glNormalStream3fATI: TglNormalStream3fATI;
  glNormalStream3fvATI: TglNormalStream3fvATI;
  glNormalStream3dATI: TglNormalStream3dATI;
  glNormalStream3dvATI: TglNormalStream3dvATI;
  glClientActiveVertexStreamATI: TglClientActiveVertexStreamATI;
  glVertexBlendEnviATI: TglVertexBlendEnviATI;
  glVertexBlendEnvfATI: TglVertexBlendEnvfATI;

  // GL_EXT_blend_color
  glBlendColorEXT: TglBlendColorEXT;

  // GL_EXT_blend_func_separate
  glBlendFuncSeparateEXT: TglBlendFuncSeparateEXT;

  // GL_EXT_blend_minmax
  glBlendEquationEXT: TglBlendEquationEXT;

  // GL_EXT_color_subtable
  glColorSubTableEXT: TglColorSubTableEXT;
  glCopyColorSubTableEXT: TglCopyColorSubTableEXT;

  // GL_EXT_compiled_vertex_array
  glLockArraysEXT: TglLockArraysEXT;
  glUnlockArraysEXT: TglUnlockArraysEXT;

  // GL_EXT_convolution
  glConvolutionFilter1DEXT: TglConvolutionFilter1DEXT;
  glConvolutionFilter2DEXT: TglConvolutionFilter2DEXT;
  glConvolutionParameterfEXT: TglConvolutionParameterfEXT;
  glConvolutionParameterfvEXT: TglConvolutionParameterfvEXT;
  glConvolutionParameteriEXT: TglConvolutionParameteriEXT;
  glConvolutionParameterivEXT: TglConvolutionParameterivEXT;
  glCopyConvolutionFilter1DEXT: TglCopyConvolutionFilter1DEXT;
  glCopyConvolutionFilter2DEXT: TglCopyConvolutionFilter2DEXT;
  glGetConvolutionFilterEXT: TglGetConvolutionFilterEXT;
  glGetConvolutionParameterfvEXT: TglGetConvolutionParameterfvEXT;
  glGetConvolutionParameterivEXT: TglGetConvolutionParameterivEXT;
  glGetSeparableFilterEXT: TglGetSeparableFilterEXT;
  glSeparableFilter2DEXT: TglSeparableFilter2DEXT;

  // GL_EXT_coordinate_frame
  glTangent3bEXT: TglTangent3bEXT;
  glTangent3bvEXT: TglTangent3bvEXT;
  glTangent3dEXT: TglTangent3dEXT;
  glTangent3dvEXT: TglTangent3dvEXT;
  glTangent3fEXT: TglTangent3fEXT;
  glTangent3fvEXT: TglTangent3fvEXT;
  glTangent3iEXT: TglTangent3iEXT;
  glTangent3ivEXT: TglTangent3ivEXT;
  glTangent3sEXT: TglTangent3sEXT;
  glTangent3svEXT: TglTangent3svEXT;
  glBinormal3bEXT: TglBinormal3bEXT;
  glBinormal3bvEXT: TglBinormal3bvEXT;
  glBinormal3dEXT: TglBinormal3dEXT;
  glBinormal3dvEXT: TglBinormal3dvEXT;
  glBinormal3fEXT: TglBinormal3fEXT;
  glBinormal3fvEXT: TglBinormal3fvEXT;
  glBinormal3iEXT: TglBinormal3iEXT;
  glBinormal3ivEXT: TglBinormal3ivEXT;
  glBinormal3sEXT: TglBinormal3sEXT;
  glBinormal3svEXT: TglBinormal3svEXT;
  glTangentPointerEXT: TglTangentPointerEXT;
  glBinormalPointerEXT: TglBinormalPointerEXT;

  // GL_EXT_copy_texture
  glCopyTexImage1DEXT: TglCopyTexImage1DEXT;
  glCopyTexImage2DEXT: TglCopyTexImage2DEXT;
  glCopyTexSubImage1DEXT: TglCopyTexSubImage1DEXT;
  glCopyTexSubImage2DEXT: TglCopyTexSubImage2DEXT;
  glCopyTexSubImage3DEXT: TglCopyTexSubImage3DEXT;

  // GL_EXT_cull_vertex
  glCullParameterdvEXT: TglCullParameterdvEXT;
  glCullParameterfvEXT: TglCullParameterfvEXT;

  // GL_EXT_draw_range_elements
  glDrawRangeElementsEXT: TglDrawRangeElementsEXT;

  // GL_EXT_fog_coord
  glFogCoordfEXT: TglFogCoordfEXT;
  glFogCoordfvEXT: TglFogCoordfvEXT;
  glFogCoorddEXT: TglFogCoorddEXT;
  glFogCoorddvEXT: TglFogCoorddvEXT;
  glFogCoordPointerEXT: TglFogCoordPointerEXT;

  // GL_EXT_framebuffer_object
  glIsRenderbufferEXT: TglIsRenderbufferEXT;
  glBindRenderbufferEXT: TglBindRenderbufferEXT;
  glDeleteRenderbuffersEXT: TglDeleteRenderbuffersEXT;
  glGenRenderbuffersEXT: TglGenRenderbuffersEXT;
  glRenderbufferStorageEXT: TglRenderbufferStorageEXT;
  glGetRenderbufferParameterivEXT: TglGetRenderbufferParameterivEXT;
  glIsFramebufferEXT: TglIsFramebufferEXT;
  glBindFramebufferEXT: TglBindFramebufferEXT;
  glDeleteFramebuffersEXT: TglDeleteFramebuffersEXT;
  glGenFramebuffersEXT: TglGenFramebuffersEXT;
  glCheckFramebufferStatusEXT: TglCheckFramebufferStatusEXT;
  glFramebufferTexture1DEXT: TglFramebufferTexture1DEXT;
  glFramebufferTexture2DEXT: TglFramebufferTexture2DEXT;
  glFramebufferTexture3DEXT: TglFramebufferTexture3DEXT;
  glFramebufferRenderbufferEXT: TglFramebufferRenderbufferEXT;
  glGetFramebufferAttachmentParameterivEXT: TglGetFramebufferAttachmentParameterivEXT;
  glGenerateMipmapEXT: TglGenerateMipmapEXT;

  // GL_EXT_histogram
  glGetHistogramEXT: TglGetHistogramEXT;
  glGetHistogramParameterfvEXT: TglGetHistogramParameterfvEXT;
  glGetHistogramParameterivEXT: TglGetHistogramParameterivEXT;
  glGetMinmaxEXT: TglGetMinmaxEXT;
  glGetMinmaxParameterfvEXT: TglGetMinmaxParameterfvEXT;
  glGetMinmaxParameterivEXT: TglGetMinmaxParameterivEXT;
  glHistogramEXT: TglHistogramEXT;
  glMinmaxEXT: TglMinmaxEXT;
  glResetHistogramEXT: TglResetHistogramEXT;
  glResetMinmaxEXT: TglResetMinmaxEXT;

  // GL_EXT_index_func
  glIndexFuncEXT: TglIndexFuncEXT;

  // GL_EXT_index_material
  glIndexMaterialEXT: TglIndexMaterialEXT;

  // GL_EXT_light_texture
  glApplyTextureEXT: TglApplyTextureEXT;
  glTextureLightEXT: TglTextureLightEXT;
  glTextureMaterialEXT: TglTextureMaterialEXT;

  // GL_EXT_multi_draw_arrays
  glMultiDrawArraysEXT: TglMultiDrawArraysEXT;
  glMultiDrawElementsEXT: TglMultiDrawElementsEXT;

  // GL_EXT_multisample
  glSampleMaskEXT: TglSampleMaskEXT;
  glSamplePatternEXT: TglSamplePatternEXT;

  // GL_EXT_paletted_texture
  glColorTableEXT: TglColorTableEXT;
  glGetColorTableEXT: TglGetColorTableEXT;
  glGetColorTableParameterivEXT: TglGetColorTableParameterivEXT;
  glGetColorTableParameterfvEXT: TglGetColorTableParameterfvEXT;

  // GL_EXT_pixel_transform
  glPixelTransformParameteriEXT: TglPixelTransformParameteriEXT;
  glPixelTransformParameterfEXT: TglPixelTransformParameterfEXT;
  glPixelTransformParameterivEXT: TglPixelTransformParameterivEXT;
  glPixelTransformParameterfvEXT: TglPixelTransformParameterfvEXT;

  // GL_EXT_point_parameters
  glPointParameterfEXT: TglPointParameterfEXT;
  glPointParameterfvEXT: TglPointParameterfvEXT;

  // GL_EXT_polygon_offset
  glPolygonOffsetEXT: TglPolygonOffsetEXT;

  // GL_EXT_secondary_color
  glSecondaryColor3bEXT: TglSecondaryColor3bEXT;
  glSecondaryColor3bvEXT: TglSecondaryColor3bvEXT;
  glSecondaryColor3dEXT: TglSecondaryColor3dEXT;
  glSecondaryColor3dvEXT: TglSecondaryColor3dvEXT;
  glSecondaryColor3fEXT: TglSecondaryColor3fEXT;
  glSecondaryColor3fvEXT: TglSecondaryColor3fvEXT;
  glSecondaryColor3iEXT: TglSecondaryColor3iEXT;
  glSecondaryColor3ivEXT: TglSecondaryColor3ivEXT;
  glSecondaryColor3sEXT: TglSecondaryColor3sEXT;
  glSecondaryColor3svEXT: TglSecondaryColor3svEXT;
  glSecondaryColor3ubEXT: TglSecondaryColor3ubEXT;
  glSecondaryColor3ubvEXT: TglSecondaryColor3ubvEXT;
  glSecondaryColor3uiEXT: TglSecondaryColor3uiEXT;
  glSecondaryColor3uivEXT: TglSecondaryColor3uivEXT;
  glSecondaryColor3usEXT: TglSecondaryColor3usEXT;
  glSecondaryColor3usvEXT: TglSecondaryColor3usvEXT;
  glSecondaryColorPointerEXT: TglSecondaryColorPointerEXT;

  // GL_EXT_stencil_two_side
  glActiveStencilFaceEXT: TglActiveStencilFaceEXT;

  // GL_EXT_subtexture
  glTexSubImage1DEXT: TglTexSubImage1DEXT;
  glTexSubImage2DEXT: TglTexSubImage2DEXT;

  // GL_EXT_texture3D
  glTexImage3DEXT: TglTexImage3DEXT;
  glTexSubImage3DEXT: TglTexSubImage3DEXT;

  // GL_EXT_texture_object
  glAreTexturesResidentEXT: TglAreTexturesResidentEXT;
  glBindTextureEXT: TglBindTextureEXT;
  glDeleteTexturesEXT: TglDeleteTexturesEXT;
  glGenTexturesEXT: TglGenTexturesEXT;
  glIsTextureEXT: TglIsTextureEXT;
  glPrioritizeTexturesEXT: TglPrioritizeTexturesEXT;

  // GL_EXT_texture_perturb_normal
  glTextureNormalEXT: TglTextureNormalEXT;

  // GL_EXT_vertex_array
  glArrayElementEXT: TglArrayElementEXT;
  glColorPointerEXT: TglColorPointerEXT;
  glDrawArraysEXT: TglDrawArraysEXT;
  glEdgeFlagPointerEXT: TglEdgeFlagPointerEXT;
  glGetPointervEXT: TglGetPointervEXT;
  glIndexPointerEXT: TglIndexPointerEXT;
  glNormalPointerEXT: TglNormalPointerEXT;
  glTexCoordPointerEXT: TglTexCoordPointerEXT;
  glVertexPointerEXT: TglVertexPointerEXT;

  // GL_EXT_vertex_shader
  glBeginVertexShaderEXT: TglBeginVertexShaderEXT;
  glEndVertexShaderEXT: TglEndVertexShaderEXT;
  glBindVertexShaderEXT: TglBindVertexShaderEXT;
  glGenVertexShadersEXT: TglGenVertexShadersEXT;
  glDeleteVertexShaderEXT: TglDeleteVertexShaderEXT;
  glShaderOp1EXT: TglShaderOp1EXT;
  glShaderOp2EXT: TglShaderOp2EXT;
  glShaderOp3EXT: TglShaderOp3EXT;
  glSwizzleEXT: TglSwizzleEXT;
  glWriteMaskEXT: TglWriteMaskEXT;
  glInsertComponentEXT: TglInsertComponentEXT;
  glExtractComponentEXT: TglExtractComponentEXT;
  glGenSymbolsEXT: TglGenSymbolsEXT;
  glSetInvariantEXT: TglSetInvariantEXT;
  glSetLocalConstantEXT: TglSetLocalConstantEXT;
  glVariantbvEXT: TglVariantbvEXT;
  glVariantsvEXT: TglVariantsvEXT;
  glVariantivEXT: TglVariantivEXT;
  glVariantfvEXT: TglVariantfvEXT;
  glVariantdvEXT: TglVariantdvEXT;
  glVariantubvEXT: TglVariantubvEXT;
  glVariantusvEXT: TglVariantusvEXT;
  glVariantuivEXT: TglVariantuivEXT;
  glVariantPointerEXT: TglVariantPointerEXT;
  glEnableVariantClientStateEXT: TglEnableVariantClientStateEXT;
  glDisableVariantClientStateEXT: TglDisableVariantClientStateEXT;
  glBindLightParameterEXT: TglBindLightParameterEXT;
  glBindMaterialParameterEXT: TglBindMaterialParameterEXT;
  glBindTexGenParameterEXT: TglBindTexGenParameterEXT;
  glBindTextureUnitParameterEXT: TglBindTextureUnitParameterEXT;
  glBindParameterEXT: TglBindParameterEXT;
  glIsVariantEnabledEXT: TglIsVariantEnabledEXT;
  glGetVariantBooleanvEXT: TglGetVariantBooleanvEXT;
  glGetVariantIntegervEXT: TglGetVariantIntegervEXT;
  glGetVariantFloatvEXT: TglGetVariantFloatvEXT;
  glGetVariantPointervEXT: TglGetVariantPointervEXT;
  glGetInvariantBooleanvEXT: TglGetInvariantBooleanvEXT;
  glGetInvariantIntegervEXT: TglGetInvariantIntegervEXT;
  glGetInvariantFloatvEXT: TglGetInvariantFloatvEXT;
  glGetLocalConstantBooleanvEXT: TglGetLocalConstantBooleanvEXT;
  glGetLocalConstantIntegervEXT: TglGetLocalConstantIntegervEXT;
  glGetLocalConstantFloatvEXT: TglGetLocalConstantFloatvEXT;

  // GL_EXT_vertex_weighting
  glVertexWeightfEXT: TglVertexWeightfEXT;
  glVertexWeightfvEXT: TglVertexWeightfvEXT;
  glVertexWeightPointerEXT: TglVertexWeightPointerEXT;

  // GL_EXT_stencil_clear_tag
  glStencilClearTagEXT: TglStencilClearTagEXT;

  // GL_EXT_framebuffer_blit
  glBlitFramebufferEXT: TglBlitFramebufferEXT;

  // GL_EXT_framebuffer_multisample
  glRenderbufferStorageMultisampleEXT: TglRenderbufferStorageMultisampleEXT;

  // GL_EXT_timer_query
  glGetQueryObjecti64vEXT: TglGetQueryObjecti64vEXT;
  glGetQueryObjectui64vEXT: TglGetQueryObjectui64vEXT;

  // GL_EXT_gpu_program_parameters
  glProgramEnvParameters4fvEXT: TglProgramEnvParameters4fvEXT;
  glProgramLocalParameters4fvEXT: TglProgramLocalParameters4fvEXT;

  // GL_EXT_bindable_uniform
  glUniformBufferEXT: TglUniformBufferEXT;
  glGetUniformBufferSizeEXT: TglGetUniformBufferSizeEXT;
  glGetUniformOffsetEXT: TglGetUniformOffsetEXT;

  // GL_EXT_draw_buffers2
  glColorMaskIndexedEXT: TglColorMaskIndexedEXT;
  glGetBooleanIndexedvEXT: TglGetBooleanIndexedvEXT;
  glGetIntegerIndexedvEXT: TglGetIntegerIndexedvEXT;
  glEnableIndexedEXT: TglEnableIndexedEXT;
  glDisableIndexedEXT: TglDisableIndexedEXT;
  glIsEnabledIndexedEXT: TglIsEnabledIndexedEXT;

  // GL_EXT_draw_instanced
  glDrawArraysInstancedEXT: TglDrawArraysInstancedEXT;
  glDrawElementsInstancedEXT: TglDrawElementsInstancedEXT;

  // GL_EXT_geometry_shader4
  glProgramParameteriEXT: TglProgramParameteriEXT;
  glFramebufferTextureEXT: TglFramebufferTextureEXT;
//  glFramebufferTextureLayerEXT: TglFramebufferTextureLayerEXT;
  glFramebufferTextureFaceEXT: TglFramebufferTextureFaceEXT;

  // GL_EXT_gpu_shader4
  glVertexAttribI1iEXT: TglVertexAttribI1iEXT;
  glVertexAttribI2iEXT: TglVertexAttribI2iEXT;
  glVertexAttribI3iEXT: TglVertexAttribI3iEXT;
  glVertexAttribI4iEXT: TglVertexAttribI4iEXT;
  glVertexAttribI1uiEXT: TglVertexAttribI1uiEXT;
  glVertexAttribI2uiEXT: TglVertexAttribI2uiEXT;
  glVertexAttribI3uiEXT: TglVertexAttribI3uiEXT;
  glVertexAttribI4uiEXT: TglVertexAttribI4uiEXT;
  glVertexAttribI1ivEXT: TglVertexAttribI1ivEXT;
  glVertexAttribI2ivEXT: TglVertexAttribI2ivEXT;
  glVertexAttribI3ivEXT: TglVertexAttribI3ivEXT;
  glVertexAttribI4ivEXT: TglVertexAttribI4ivEXT;
  glVertexAttribI1uivEXT: TglVertexAttribI1uivEXT;
  glVertexAttribI2uivEXT: TglVertexAttribI2uivEXT;
  glVertexAttribI3uivEXT: TglVertexAttribI3uivEXT;
  glVertexAttribI4uivEXT: TglVertexAttribI4uivEXT;
  glVertexAttribI4bvEXT:TglVertexAttribI4bvEXT ;
  glVertexAttribI4svEXT: TglVertexAttribI4svEXT;
  glVertexAttribI4ubvEXT: TglVertexAttribI4ubvEXT;
  glVertexAttribI4usvEXT: TglVertexAttribI4usvEXT;
  glVertexAttribIPointerEXT: TglVertexAttribIPointerEXT;
  glGetVertexAttribIivEXT: TglGetVertexAttribIivEXT;
  glGetVertexAttribIuivEXT: TglGetVertexAttribIuivEXT;
  glUniform1uiEXT: TglUniform1uiEXT;
  glUniform2uiEXT: TglUniform2uiEXT;
  glUniform3uiEXT: TglUniform3uiEXT;
  glUniform4uiEXT: TglUniform4uiEXT;
  glUniform1uivEXT: TglUniform1uivEXT;
  glUniform2uivEXT: TglUniform2uivEXT;
  glUniform3uivEXT: TglUniform3uivEXT;
  glUniform4uivEXT: TglUniform4uivEXT;
  glGetUniformuivEXT: TglGetUniformuivEXT;
  glBindFragDataLocationEXT: TglBindFragDataLocationEXT;
  glGetFragDataLocationEXT: TglGetFragDataLocationEXT;

  // GL_EXT_texture_array
  glFramebufferTextureLayerEXT: TglFramebufferTextureLayerEXT;

  // GL_EXT_texture_buffer_object
  glTexBufferEXT: TglTexBufferEXT;

  // GL_EXT_texture_integer
  glClearColorIiEXT: TglClearColorIiEXT;
  glClearColorIuiEXT: TglClearColorIuiEXT;
  glTexParameterIivEXT: TglTexParameterIivEXT;
  glTexParameterIuivEXT: TglTexParameterIuivEXT;
  glGetTexParameterIivEXT: TglGetTexParameterIivEXT;
  glGetTexParameterIiuvEXT: TglGetTexParameterIiuvEXT;

  // GL_EXT_transform_feedback
  glBeginTransformFeedbackEXT: TglBeginTransformFeedbackEXT;
  glEndTransformFeedbackEXT: TglEndTransformFeedbackEXT;
  glBindBufferRangeEXT: TglBindBufferRangeEXT;
  glBindBufferOffsetEXT: TglBindBufferOffsetEXT;
  glBindBufferBaseEXT: TglBindBufferBaseEXT;
  glTransformFeedbackVaryingsEXT: TglTransformFeedbackVaryingsEXT;
  glGetTransformFeedbackVaryingEXT: TglGetTransformFeedbackVaryingEXT;

  // GL_EXT_direct_state_access
  glClientAttribDefaultEXT: TglClientAttribDefaultEXT;
  glPushClientAttribDefaultEXT: TglPushClientAttribDefaultEXT;
  glMatrixLoadfEXT: TglMatrixLoadfEXT;
  glMatrixLoaddEXT: TglMatrixLoaddEXT;
  glMatrixMultfEXT: TglMatrixMultfEXT;
  glMatrixMultdEXT: TglMatrixMultdEXT;
  glMatrixLoadIdentityEXT: TglMatrixLoadIdentityEXT;
  glMatrixRotatefEXT: TglMatrixRotatefEXT;
  glMatrixRotatedEXT: TglMatrixRotatedEXT;
  glMatrixScalefEXT: TglMatrixScalefEXT;
  glMatrixScaledEXT: TglMatrixScaledEXT;
  glMatrixTranslatefEXT: TglMatrixTranslatefEXT;
  glMatrixTranslatedEXT: TglMatrixTranslatedEXT;
  glMatrixFrustumEXT: TglMatrixFrustumEXT;
  glMatrixOrthoEXT: TglMatrixOrthoEXT;
  glMatrixPopEXT: TglMatrixPopEXT;
  glMatrixPushEXT: TglMatrixPushEXT;
  glMatrixLoadTransposefEXT: TglMatrixLoadTransposefEXT;
  glMatrixLoadTransposedEXT: TglMatrixLoadTransposedEXT;
  glMatrixMultTransposefEXT: TglMatrixMultTransposefEXT;
  glMatrixMultTransposedEXT: TglMatrixMultTransposedEXT;
  glTextureParameterfEXT: TglTextureParameterfEXT;
  glTextureParameterfvEXT: TglTextureParameterfvEXT;
  glTextureParameteriEXT: TglTextureParameteriEXT;
  glTextureParameterivEXT: TglTextureParameterivEXT;
  glTextureImage1DEXT: TglTextureImage1DEXT;
  glTextureImage2DEXT: TglTextureImage2DEXT;
  glTextureSubImage1DEXT: TglTextureSubImage1DEXT;
  glTextureSubImage2DEXT: TglTextureSubImage2DEXT;
  glCopyTextureImage1DEXT: TglCopyTextureImage1DEXT;
  glCopyTextureImage2DEXT: TglCopyTextureImage2DEXT;
  glCopyTextureSubImage1DEXT: TglCopyTextureSubImage1DEXT;
  glCopyTextureSubImage2DEXT: TglCopyTextureSubImage2DEXT;
  glGetTextureImageEXT: TglGetTextureImageEXT;
  glGetTextureParameterfvEXT: TglGetTextureParameterfvEXT;
  glGetTextureParameterivEXT: TglGetTextureParameterivEXT;
  glGetTextureLevelParameterfvEXT: TglGetTextureLevelParameterfvEXT;
  glGetTextureLevelParameterivEXT: TglGetTextureLevelParameterivEXT;
  glTextureImage3DEXT: TglTextureImage3DEXT;
  glTextureSubImage3DEXT: TglTextureSubImage3DEXT;
  glCopyTextureSubImage3DEXT: TglCopyTextureSubImage3DEXT;
  glMultiTexParameterfEXT: TglMultiTexParameterfEXT;
  glMultiTexParameterfvEXT: TglMultiTexParameterfvEXT;
  glMultiTexParameteriEXT: TglMultiTexParameteriEXT;
  glMultiTexParameterivEXT: TglMultiTexParameterivEXT;
  glMultiTexImage1DEXT: TglMultiTexImage1DEXT;
  glMultiTexImage2DEXT: TglMultiTexImage2DEXT;
  glMultiTexSubImage1DEXT: TglMultiTexSubImage1DEXT;
  glMultiTexSubImage2DEXT: TglMultiTexSubImage2DEXT;
  glCopyMultiTexImage1DEXT: TglCopyMultiTexImage1DEXT;
  glCopyMultiTexImage2DEXT: TglCopyMultiTexImage2DEXT;
  glCopyMultiTexSubImage1DEXT: TglCopyMultiTexSubImage1DEXT;
  glCopyMultiTexSubImage2DEXT: TglCopyMultiTexSubImage2DEXT;
  glGetMultiTexImageEXT: TglGetMultiTexImageEXT;
  glGetMultiTexParameterfvEXT: TglGetMultiTexParameterfvEXT;
  glGetMultiTexParameterivEXT: TglGetMultiTexParameterivEXT;
  glGetMultiTexLevelParameterfvEXT: TglGetMultiTexLevelParameterfvEXT;
  glGetMultiTexLevelParameterivEXT: TglGetMultiTexLevelParameterivEXT;
  glMultiTexImage3DEXT: TglMultiTexImage3DEXT;
  glMultiTexSubImage3DEXT: TglMultiTexSubImage3DEXT;
  glCopyMultiTexSubImage3DEXT: TglCopyMultiTexSubImage3DEXT;
  glBindMultiTextureEXT: TglBindMultiTextureEXT;
  glEnableClientStateIndexedEXT: TglEnableClientStateIndexedEXT;
  glDisableClientStateIndexedEXT: TglDisableClientStateIndexedEXT;
  glMultiTexCoordPointerEXT: TglMultiTexCoordPointerEXT;
  glMultiTexEnvfEXT: TglMultiTexEnvfEXT;
  glMultiTexEnvfvEXT: TglMultiTexEnvfvEXT;
  glMultiTexEnviEXT: TglMultiTexEnviEXT;
  glMultiTexEnvivEXT: TglMultiTexEnvivEXT;
  glMultiTexGendEXT: TglMultiTexGendEXT;
  glMultiTexGendvEXT: TglMultiTexGendvEXT;
  glMultiTexGenfEXT: TglMultiTexGenfEXT;
  glMultiTexGenfvEXT: TglMultiTexGenfvEXT;
  glMultiTexGeniEXT: TglMultiTexGeniEXT;
  glMultiTexGenivEXT: TglMultiTexGenivEXT;
  glGetMultiTexEnvfvEXT: TglGetMultiTexEnvfvEXT;
  glGetMultiTexEnvivEXT: TglGetMultiTexEnvivEXT;
  glGetMultiTexGendvEXT: TglGetMultiTexGendvEXT;
  glGetMultiTexGenfvEXT: TglGetMultiTexGenfvEXT;
  glGetMultiTexGenivEXT: TglGetMultiTexGenivEXT;
  glGetFloatIndexedvEXT: TglGetFloatIndexedvEXT;
  glGetDoubleIndexedvEXT: TglGetDoubleIndexedvEXT;
  glGetPointerIndexedvEXT: TglGetPointerIndexedvEXT;
  glCompressedTextureImage3DEXT: TglCompressedTextureImage3DEXT;
  glCompressedTextureImage2DEXT: TglCompressedTextureImage2DEXT;
  glCompressedTextureImage1DEXT: TglCompressedTextureImage1DEXT;
  glCompressedTextureSubImage3DEXT: TglCompressedTextureSubImage3DEXT;
  glCompressedTextureSubImage2DEXT: TglCompressedTextureSubImage2DEXT;
  glCompressedTextureSubImage1DEXT: TglCompressedTextureSubImage1DEXT;
  glGetCompressedTextureImageEXT: TglGetCompressedTextureImageEXT;
  glCompressedMultiTexImage3DEXT: TglCompressedMultiTexImage3DEXT;
  glCompressedMultiTexImage2DEXT: TglCompressedMultiTexImage2DEXT;
  glCompressedMultiTexImage1DEXT: TglCompressedMultiTexImage1DEXT;
  glCompressedMultiTexSubImage3DEXT: TglCompressedMultiTexSubImage3DEXT;
  glCompressedMultiTexSubImage2DEXT: TglCompressedMultiTexSubImage2DEXT;
  glCompressedMultiTexSubImage1DEXT: TglCompressedMultiTexSubImage1DEXT;
  glGetCompressedMultiTexImageEXT: TglGetCompressedMultiTexImageEXT;
  glNamedProgramStringEXT: TglNamedProgramStringEXT;
  glNamedProgramLocalParameter4dEXT: TglNamedProgramLocalParameter4dEXT;
  glNamedProgramLocalParameter4dvEXT: TglNamedProgramLocalParameter4dvEXT;
  glNamedProgramLocalParameter4fEXT: TglNamedProgramLocalParameter4fEXT;
  glNamedProgramLocalParameter4fvEXT: TglNamedProgramLocalParameter4fvEXT;
  glGetNamedProgramLocalParameterdvEXT: TglGetNamedProgramLocalParameterdvEXT;
  glGetNamedProgramLocalParameterfvEXT: TglGetNamedProgramLocalParameterfvEXT;
  glGetNamedProgramivEXT: TglGetNamedProgramivEXT;
  glGetNamedProgramStringEXT: TglGetNamedProgramStringEXT;
  glNamedProgramLocalParameters4fvEXT: TglNamedProgramLocalParameters4fvEXT;
  glNamedProgramLocalParameterI4iEXT: TglNamedProgramLocalParameterI4iEXT;
  glNamedProgramLocalParameterI4ivEXT: TglNamedProgramLocalParameterI4ivEXT;
  glNamedProgramLocalParametersI4ivEXT: TglNamedProgramLocalParametersI4ivEXT;
  glNamedProgramLocalParameterI4uiEXT: TglNamedProgramLocalParameterI4uiEXT;
  glNamedProgramLocalParameterI4uivEXT: TglNamedProgramLocalParameterI4uivEXT;
  glNamedProgramLocalParametersI4uivEXT: TglNamedProgramLocalParametersI4uivEXT;
  glGetNamedProgramLocalParameterIivEXT: TglGetNamedProgramLocalParameterIivEXT;
  glGetNamedProgramLocalParameterIuivEXT: TglGetNamedProgramLocalParameterIuivEXT;
  glTextureParameterIivEXT: TglTextureParameterIivEXT;
  glTextureParameterIuivEXT: TglTextureParameterIuivEXT;
  glGetTextureParameterIivEXT: TglGetTextureParameterIivEXT;
  glGetTextureParameterIuivEXT: TglGetTextureParameterIuivEXT;
  glMultiTexParameterIivEXT: TglMultiTexParameterIivEXT;
  glMultiTexParameterIuivEXT: TglMultiTexParameterIuivEXT;
  glGetMultiTexParameterIivEXT: TglGetMultiTexParameterIivEXT;
  glGetMultiTexParameterIuivEXT: TglGetMultiTexParameterIuivEXT;
  glProgramUniform1fEXT: TglProgramUniform1fEXT;
  glProgramUniform2fEXT: TglProgramUniform2fEXT;
  glProgramUniform3fEXT: TglProgramUniform3fEXT;
  glProgramUniform4fEXT: TglProgramUniform4fEXT;
  glProgramUniform1iEXT: TglProgramUniform1iEXT;
  glProgramUniform2iEXT: TglProgramUniform2iEXT;
  glProgramUniform3iEXT: TglProgramUniform3iEXT;
  glProgramUniform4iEXT: TglProgramUniform4iEXT;
  glProgramUniform1fvEXT: TglProgramUniform1fvEXT;
  glProgramUniform2fvEXT: TglProgramUniform2fvEXT;
  glProgramUniform3fvEXT: TglProgramUniform3fvEXT;
  glProgramUniform4fvEXT: TglProgramUniform4fvEXT;
  glProgramUniform1ivEXT: TglProgramUniform1ivEXT;
  glProgramUniform2ivEXT: TglProgramUniform2ivEXT;
  glProgramUniform3ivEXT: TglProgramUniform3ivEXT;
  glProgramUniform4ivEXT: TglProgramUniform4ivEXT;
  glProgramUniformMatrix2fvEXT: TglProgramUniformMatrix2fvEXT;
  glProgramUniformMatrix3fvEXT: TglProgramUniformMatrix3fvEXT;
  glProgramUniformMatrix4fvEXT: TglProgramUniformMatrix4fvEXT;
  glProgramUniformMatrix2x3fvEXT: TglProgramUniformMatrix2x3fvEXT;
  glProgramUniformMatrix3x2fvEXT: TglProgramUniformMatrix3x2fvEXT;
  glProgramUniformMatrix2x4fvEXT: TglProgramUniformMatrix2x4fvEXT;
  glProgramUniformMatrix4x2fvEXT: TglProgramUniformMatrix4x2fvEXT;
  glProgramUniformMatrix3x4fvEXT: TglProgramUniformMatrix3x4fvEXT;
  glProgramUniformMatrix4x3fvEXT: TglProgramUniformMatrix4x3fvEXT;
  glProgramUniform1uiEXT: TglProgramUniform1uiEXT;
  glProgramUniform2uiEXT: TglProgramUniform2uiEXT;
  glProgramUniform3uiEXT: TglProgramUniform3uiEXT;
  glProgramUniform4uiEXT: TglProgramUniform4uiEXT;
  glProgramUniform1uivEXT: TglProgramUniform1uivEXT;
  glProgramUniform2uivEXT: TglProgramUniform2uivEXT;
  glProgramUniform3uivEXT: TglProgramUniform3uivEXT;
  glProgramUniform4uivEXT: TglProgramUniform4uivEXT;
  glNamedBufferDataEXT: TglNamedBufferDataEXT;
  glNamedBufferSubDataEXT: TglNamedBufferSubDataEXT;
  glMapNamedBufferEXT: TglMapNamedBufferEXT;
  glUnmapNamedBufferEXT: TglUnmapNamedBufferEXT;
  glGetNamedBufferParameterivEXT: TglGetNamedBufferParameterivEXT;
  glGetNamedBufferPointervEXT: TglGetNamedBufferPointervEXT;
  glGetNamedBufferSubDataEXT: TglGetNamedBufferSubDataEXT;
  glTextureBufferEXT: TglTextureBufferEXT;
  glMultiTexBufferEXT: TglMultiTexBufferEXT;
  glNamedRenderbufferStorageEXT: TglNamedRenderbufferStorageEXT;
  glGetNamedRenderbufferParameterivEXT: TglGetNamedRenderbufferParameterivEXT;
  glCheckNamedFramebufferStatusEXT: TglCheckNamedFramebufferStatusEXT;
  glNamedFramebufferTexture1DEXT: TglNamedFramebufferTexture1DEXT;
  glNamedFramebufferTexture2DEXT: TglNamedFramebufferTexture2DEXT;
  glNamedFramebufferTexture3DEXT: TglNamedFramebufferTexture3DEXT;
  glNamedFramebufferRenderbufferEXT: TglNamedFramebufferRenderbufferEXT;
  glGetNamedFramebufferAttachmentParameterivEXT: TglGetNamedFramebufferAttachmentParameterivEXT;
  glGenerateTextureMipmapEXT: TglGenerateTextureMipmapEXT;
  glGenerateMultiTexMipmapEXT: TglGenerateMultiTexMipmapEXT;
  glFramebufferDrawBufferEXT: TglFramebufferDrawBufferEXT;
  glFramebufferDrawBuffersEXT: TglFramebufferDrawBuffersEXT;
  glFramebufferReadBufferEXT: TglFramebufferReadBufferEXT;
  glGetFramebufferParameterivEXT: TglGetFramebufferParameterivEXT;
  glNamedRenderbufferStorageMultisampleEXT: TglNamedRenderbufferStorageMultisampleEXT;
  glNamedRenderbufferStorageMultisampleCoverageEXT: TglNamedRenderbufferStorageMultisampleCoverageEXT;
  glNamedFramebufferTextureEXT: TglNamedFramebufferTextureEXT;
  glNamedFramebufferTextureLayerEXT: TglNamedFramebufferTextureLayerEXT;
  glNamedFramebufferTextureFaceEXT: TglNamedFramebufferTextureFaceEXT;
  glTextureRenderbufferEXT: TglTextureRenderbufferEXT;
  glMultiTexRenderbufferEXT: TglMultiTexRenderbufferEXT;

  // GL_HP_image_transform
  glImageTransformParameteriHP: TglImageTransformParameteriHP;
  glImageTransformParameterfHP: TglImageTransformParameterfHP;
  glImageTransformParameterivHP: TglImageTransformParameterivHP;
  glImageTransformParameterfvHP: TglImageTransformParameterfvHP;
  glGetImageTransformParameterivHP: TglGetImageTransformParameterivHP;
  glGetImageTransformParameterfvHP: TglGetImageTransformParameterfvHP;

  // GL_EXT_depth_bounds_test
  glDepthBoundsEXT: TglDepthBoundsEXT;

  // GL_EXT_blend_equation_separate
  glBlendEquationSeparateEXT: TglBlendEquationSeparateEXT;

  // GL_IBM_multimode_draw_arrays
  glMultiModeDrawArraysIBM: TglMultiModeDrawArraysIBM;
  glMultiModeDrawElementsIBM: TglMultiModeDrawElementsIBM;

  // GL_IBM_vertex_array_lists
  glColorPointerListIBM: TglColorPointerListIBM;
  glSecondaryColorPointerListIBM: TglSecondaryColorPointerListIBM;
  glEdgeFlagPointerListIBM: TglEdgeFlagPointerListIBM;
  glFogCoordPointerListIBM: TglFogCoordPointerListIBM;
  glIndexPointerListIBM: TglIndexPointerListIBM;
  glNormalPointerListIBM: TglNormalPointerListIBM;
  glTexCoordPointerListIBM: TglTexCoordPointerListIBM;
  glVertexPointerListIBM: TglVertexPointerListIBM;

  // GL_INGR_blend_func_separate
  glBlendFuncSeparateINGR: TglBlendFuncSeparateINGR;

  // GL_INTEL_parallel_arrays
  glVertexPointervINTEL: TglVertexPointervINTEL;
  glNormalPointervINTEL: TglNormalPointervINTEL;
  glColorPointervINTEL: TglColorPointervINTEL;
  glTexCoordPointervINTEL: TglTexCoordPointervINTEL;

  // GL_MESA_resize_buffers
  glResizeBuffersMESA: TglResizeBuffersMESA;

  // GL_MESA_window_pos
  glWindowPos2dMESA: TglWindowPos2dMESA;
  glWindowPos2dvMESA: TglWindowPos2dvMESA;
  glWindowPos2fMESA: TglWindowPos2fMESA;
  glWindowPos2fvMESA: TglWindowPos2fvMESA;
  glWindowPos2iMESA: TglWindowPos2iMESA;
  glWindowPos2ivMESA: TglWindowPos2ivMESA;
  glWindowPos2sMESA: TglWindowPos2sMESA;
  glWindowPos2svMESA: TglWindowPos2svMESA;
  glWindowPos3dMESA: TglWindowPos3dMESA;
  glWindowPos3dvMESA: TglWindowPos3dvMESA;
  glWindowPos3fMESA: TglWindowPos3fMESA;
  glWindowPos3fvMESA: TglWindowPos3fvMESA;
  glWindowPos3iMESA: TglWindowPos3iMESA;
  glWindowPos3ivMESA: TglWindowPos3ivMESA;
  glWindowPos3sMESA: TglWindowPos3sMESA;
  glWindowPos3svMESA: TglWindowPos3svMESA;
  glWindowPos4dMESA: TglWindowPos4dMESA;
  glWindowPos4dvMESA: TglWindowPos4dvMESA;
  glWindowPos4fMESA: TglWindowPos4fMESA;
  glWindowPos4fvMESA: TglWindowPos4fvMESA;
  glWindowPos4iMESA: TglWindowPos4iMESA;
  glWindowPos4ivMESA: TglWindowPos4ivMESA;
  glWindowPos4sMESA: TglWindowPos4sMESA;
  glWindowPos4svMESA: TglWindowPos4svMESA;

  // GL_NV_evaluators
  glMapControlPointsNV: TglMapControlPointsNV;
  glMapParameterivNV: TglMapParameterivNV;
  glMapParameterfvNV: TglMapParameterfvNV;
  glGetMapControlPointsNV: TglGetMapControlPointsNV;
  glGetMapParameterivNV: TglGetMapParameterivNV;
  glGetMapParameterfvNV: TglGetMapParameterfvNV;
  glGetMapAttribParameterivNV: TglGetMapAttribParameterivNV;
  glGetMapAttribParameterfvNV: TglGetMapAttribParameterfvNV;
  glEvalMapsNV: TglEvalMapsNV;

  // GL_NV_fence
  glDeleteFencesNV: TglDeleteFencesNV;
  glGenFencesNV: TglGenFencesNV;
  glIsFenceNV: TglIsFenceNV;
  glTestFenceNV: TglTestFenceNV;
  glGetFenceivNV: TglGetFenceivNV;
  glFinishFenceNV: TglFinishFenceNV;
  glSetFenceNV: TglSetFenceNV;

  // GL_NV_fragment_program
  glProgramNamedParameter4fNV: TglProgramNamedParameter4fNV;
  glProgramNamedParameter4dNV: TglProgramNamedParameter4dNV;
  glProgramNamedParameter4fvNV: TglProgramNamedParameter4fvNV;
  glProgramNamedParameter4dvNV: TglProgramNamedParameter4dvNV;
  glGetProgramNamedParameterfvNV: TglGetProgramNamedParameterfvNV;
  glGetProgramNamedParameterdvNV: TglGetProgramNamedParameterdvNV;

  // GL_NV_half_float
  glVertex2hNV: TglVertex2hNV;
  glVertex2hvNV: TglVertex2hvNV;
  glVertex3hNV: TglVertex3hNV;
  glVertex3hvNV: TglVertex3hvNV;
  glVertex4hNV: TglVertex4hNV;
  glVertex4hvNV: TglVertex4hvNV;
  glNormal3hNV: TglNormal3hNV;
  glNormal3hvNV: TglNormal3hvNV;
  glColor3hNV: TglColor3hNV;
  glColor3hvNV: TglColor3hvNV;
  glColor4hNV: TglColor4hNV;
  glColor4hvNV: TglColor4hvNV;
  glTexCoord1hNV: TglTexCoord1hNV;
  glTexCoord1hvNV: TglTexCoord1hvNV;
  glTexCoord2hNV: TglTexCoord2hNV;
  glTexCoord2hvNV: TglTexCoord2hvNV;
  glTexCoord3hNV: TglTexCoord3hNV;
  glTexCoord3hvNV: TglTexCoord3hvNV;
  glTexCoord4hNV: TglTexCoord4hNV;
  glTexCoord4hvNV: TglTexCoord4hvNV;
  glMultiTexCoord1hNV: TglMultiTexCoord1hNV;
  glMultiTexCoord1hvNV: TglMultiTexCoord1hvNV;
  glMultiTexCoord2hNV: TglMultiTexCoord2hNV;
  glMultiTexCoord2hvNV: TglMultiTexCoord2hvNV;
  glMultiTexCoord3hNV: TglMultiTexCoord3hNV;
  glMultiTexCoord3hvNV: TglMultiTexCoord3hvNV;
  glMultiTexCoord4hNV: TglMultiTexCoord4hNV;
  glMultiTexCoord4hvNV: TglMultiTexCoord4hvNV;
  glFogCoordhNV: TglFogCoordhNV;
  glFogCoordhvNV: TglFogCoordhvNV;
  glSecondaryColor3hNV: TglSecondaryColor3hNV;
  glSecondaryColor3hvNV: TglSecondaryColor3hvNV;
  glVertexWeighthNV: TglVertexWeighthNV;
  glVertexWeighthvNV: TglVertexWeighthvNV;
  glVertexAttrib1hNV: TglVertexAttrib1hNV;
  glVertexAttrib1hvNV: TglVertexAttrib1hvNV;
  glVertexAttrib2hNV: TglVertexAttrib2hNV;
  glVertexAttrib2hvNV: TglVertexAttrib2hvNV;
  glVertexAttrib3hNV: TglVertexAttrib3hNV;
  glVertexAttrib3hvNV: TglVertexAttrib3hvNV;
  glVertexAttrib4hNV: TglVertexAttrib4hNV;
  glVertexAttrib4hvNV: TglVertexAttrib4hvNV;
  glVertexAttribs1hvNV: TglVertexAttribs1hvNV;
  glVertexAttribs2hvNV: TglVertexAttribs2hvNV;
  glVertexAttribs3hvNV: TglVertexAttribs3hvNV;
  glVertexAttribs4hvNV: TglVertexAttribs4hvNV;

  // GL_NV_occlusion_query
  glGenOcclusionQueriesNV: TglGenOcclusionQueriesNV;
  glDeleteOcclusionQueriesNV: TglDeleteOcclusionQueriesNV;
  glIsOcclusionQueryNV: TglIsOcclusionQueryNV;
  glBeginOcclusionQueryNV: TglBeginOcclusionQueryNV;
  glEndOcclusionQueryNV: TglEndOcclusionQueryNV;
  glGetOcclusionQueryivNV: TglGetOcclusionQueryivNV;
  glGetOcclusionQueryuivNV: TglGetOcclusionQueryuivNV;

  // GL_NV_pixel_data_range
  glPixelDataRangeNV: TglPixelDataRangeNV;
  glFlushPixelDataRangeNV: TglFlushPixelDataRangeNV;

  // GL_NV_point_sprite
  glPointParameteriNV: TglPointParameteriNV;
  glPointParameterivNV: TglPointParameterivNV;

  // GL_NV_primitive_restart
  glPrimitiveRestartNV: TglPrimitiveRestartNV;
  glPrimitiveRestartIndexNV: TglPrimitiveRestartIndexNV;

  // GL_NV_register_combiners
  glCombinerParameterfvNV: TglCombinerParameterfvNV;
  glCombinerParameterfNV: TglCombinerParameterfNV;
  glCombinerParameterivNV: TglCombinerParameterivNV;
  glCombinerParameteriNV: TglCombinerParameteriNV;
  glCombinerInputNV: TglCombinerInputNV;
  glCombinerOutputNV: TglCombinerOutputNV;
  glFinalCombinerInputNV: TglFinalCombinerInputNV;
  glGetCombinerInputParameterfvNV: TglGetCombinerInputParameterfvNV;
  glGetCombinerInputParameterivNV: TglGetCombinerInputParameterivNV;
  glGetCombinerOutputParameterfvNV: TglGetCombinerOutputParameterfvNV;
  glGetCombinerOutputParameterivNV: TglGetCombinerOutputParameterivNV;
  glGetFinalCombinerInputParameterfvNV: TglGetFinalCombinerInputParameterfvNV;
  glGetFinalCombinerInputParameterivNV: TglGetFinalCombinerInputParameterivNV;

  // GL_NV_register_combiners2
  glCombinerStageParameterfvNV: TglCombinerStageParameterfvNV;
  glGetCombinerStageParameterfvNV: TglGetCombinerStageParameterfvNV;

  // GL_NV_vertex_array_range
  glFlushVertexArrayRangeNV: TglFlushVertexArrayRangeNV;
  glVertexArrayRangeNV: TglVertexArrayRangeNV;

  // GL_NV_vertex_program
  glAreProgramsResidentNV: TglAreProgramsResidentNV;
  glBindProgramNV: TglBindProgramNV;
  glDeleteProgramsNV: TglDeleteProgramsNV;
  glExecuteProgramNV: TglExecuteProgramNV;
  glGenProgramsNV: TglGenProgramsNV;
  glGetProgramParameterdvNV: TglGetProgramParameterdvNV;
  glGetProgramParameterfvNV: TglGetProgramParameterfvNV;
  glGetProgramivNV: TglGetProgramivNV;
  glGetProgramStringNV: TglGetProgramStringNV;
  glGetTrackMatrixivNV: TglGetTrackMatrixivNV;
  glGetVertexAttribdvNV: TglGetVertexAttribdvNV;
  glGetVertexAttribfvNV: TglGetVertexAttribfvNV;
  glGetVertexAttribivNV: TglGetVertexAttribivNV;
  glGetVertexAttribPointervNV: TglGetVertexAttribPointervNV;
  glIsProgramNV: TglIsProgramNV;
  glLoadProgramNV: TglLoadProgramNV;
  glProgramParameter4dNV: TglProgramParameter4dNV;
  glProgramParameter4dvNV: TglProgramParameter4dvNV;
  glProgramParameter4fNV: TglProgramParameter4fNV;
  glProgramParameter4fvNV: TglProgramParameter4fvNV;
  glProgramParameters4dvNV: TglProgramParameters4dvNV;
  glProgramParameters4fvNV: TglProgramParameters4fvNV;
  glRequestResidentProgramsNV: TglRequestResidentProgramsNV;
  glTrackMatrixNV: TglTrackMatrixNV;
  glVertexAttribPointerNV: TglVertexAttribPointerNV;
  glVertexAttrib1dNV: TglVertexAttrib1dNV;
  glVertexAttrib1dvNV: TglVertexAttrib1dvNV;
  glVertexAttrib1fNV: TglVertexAttrib1fNV;
  glVertexAttrib1fvNV: TglVertexAttrib1fvNV;
  glVertexAttrib1sNV: TglVertexAttrib1sNV;
  glVertexAttrib1svNV: TglVertexAttrib1svNV;
  glVertexAttrib2dNV: TglVertexAttrib2dNV;
  glVertexAttrib2dvNV: TglVertexAttrib2dvNV;
  glVertexAttrib2fNV: TglVertexAttrib2fNV;
  glVertexAttrib2fvNV: TglVertexAttrib2fvNV;
  glVertexAttrib2sNV: TglVertexAttrib2sNV;
  glVertexAttrib2svNV: TglVertexAttrib2svNV;
  glVertexAttrib3dNV: TglVertexAttrib3dNV;
  glVertexAttrib3dvNV: TglVertexAttrib3dvNV;
  glVertexAttrib3fNV: TglVertexAttrib3fNV;
  glVertexAttrib3fvNV: TglVertexAttrib3fvNV;
  glVertexAttrib3sNV: TglVertexAttrib3sNV;
  glVertexAttrib3svNV: TglVertexAttrib3svNV;
  glVertexAttrib4dNV: TglVertexAttrib4dNV;
  glVertexAttrib4dvNV: TglVertexAttrib4dvNV;
  glVertexAttrib4fNV: TglVertexAttrib4fNV;
  glVertexAttrib4fvNV: TglVertexAttrib4fvNV;
  glVertexAttrib4sNV: TglVertexAttrib4sNV;
  glVertexAttrib4svNV: TglVertexAttrib4svNV;
  glVertexAttrib4ubNV: TglVertexAttrib4ubNV;
  glVertexAttrib4ubvNV: TglVertexAttrib4ubvNV;
  glVertexAttribs1dvNV: TglVertexAttribs1dvNV;
  glVertexAttribs1fvNV: TglVertexAttribs1fvNV;
  glVertexAttribs1svNV: TglVertexAttribs1svNV;
  glVertexAttribs2dvNV: TglVertexAttribs2dvNV;
  glVertexAttribs2fvNV: TglVertexAttribs2fvNV;
  glVertexAttribs2svNV: TglVertexAttribs2svNV;
  glVertexAttribs3dvNV: TglVertexAttribs3dvNV;
  glVertexAttribs3fvNV: TglVertexAttribs3fvNV;
  glVertexAttribs3svNV: TglVertexAttribs3svNV;
  glVertexAttribs4dvNV: TglVertexAttribs4dvNV;
  glVertexAttribs4fvNV: TglVertexAttribs4fvNV;
  glVertexAttribs4svNV: TglVertexAttribs4svNV;
  glVertexAttribs4ubvNV: TglVertexAttribs4ubvNV;

  // GL_NV_depth_buffer_float
  glDepthRangedNV: TglDepthRangedNV;
  glClearDepthdNV: TglClearDepthdNV;
  glDepthBoundsdNV: TglDepthBoundsdNV;

  // GL_NV_framebuffer_multisample_coverage
  glRenderbufferStorageMultsampleCoverageNV: TglRenderbufferStorageMultsampleCoverageNV;

  // GL_NV_geometry_program4
  glProgramVertexLimitNV: TglProgramVertexLimitNV;

  // GL_NV_gpu_program4
  glProgramLocalParameterI4iNV: TglProgramLocalParameterI4iNV;
  glProgramLocalParameterI4ivNV: TglProgramLocalParameterI4ivNV;
  glProgramLocalParametersI4ivNV: TglProgramLocalParametersI4ivNV;
  glProgramLocalParameterI4uiNV: TglProgramLocalParameterI4uiNV;
  glProgramLocalParameterI4uivNV: TglProgramLocalParameterI4uivNV;
  glProgramLocalParametersI4uivNV: TglProgramLocalParametersI4uivNV;
  glProgramEnvParameterI4iNV: TglProgramEnvParameterI4iNV;
  glProgramEnvParameterI4ivNV: TglProgramEnvParameterI4ivNV;
  glProgramEnvParametersI4ivNV: TglProgramEnvParametersI4ivNV;
  glProgramEnvParameterI4uiNV: TglProgramEnvParameterI4uiNV;
  glProgramEnvParameterI4uivNV: TglProgramEnvParameterI4uivNV;
  glProgramEnvParametersI4uivNV: TglProgramEnvParametersI4uivNV;
  glGetProgramLocalParameterIivNV: TglGetProgramLocalParameterIivNV;
  glGetProgramLocalParameterIuivNV: TglGetProgramLocalParameterIuivNV;
  glGetProgramEnvParameterIivNV: TglGetProgramEnvParameterIivNV;
  glGetProgramEnvParameterIuivNV: TglGetProgramEnvParameterIuivNV;

  // GL_NV_parameter_buffer_object
  glBindBufferRangeNV: TglBindBufferRangeNV;
  glBindBufferOffsetNV: TglBindBufferOffsetNV;
  glBindBufferBaseNV: TglBindBufferBaseNV;
  glProgramBufferParametersfvNV: TglProgramBufferParametersfvNV;
  glProgramBufferParametersIivNV: TglProgramBufferParametersIivNV;
  glProgramBufferParametersIuivNV: TglProgramBufferParametersIuivNV;

  // GL_NV_transform_feedback
  glTransformFeedbackAttribsNV: TglTransformFeedbackAttribsNV;
  glTransformFeedbackVaryingsNV: TglTransformFeedbackVaryingsNV;
  glBeginTransformFeedbackNV: TglBeginTransformFeedbackNV;
  glEndTransformFeedbackNV: TglEndTransformFeedbackNV;
  glGetVaryingLocationNV: TglGetVaryingLocationNV;
  glGetActiveVaryingNV: TglGetActiveVaryingNV;
  glActiveVaryingNV: TglActiveVaryingNV;
  glGetTransformFeedbackVaryingNV: TglGetTransformFeedbackVaryingNV;

  // GL_NV_conditional_render
  glBeginConditionalRenderNV: TglBeginConditionalRenderNV;
  glEndConditionalRenderNV: TglEndConditionalRenderNV;

  // GL_NV_explicit_multisample
  glGetMultisamplefvNV: TglGetMultisamplefvNV;
  glSampleMaskIndexedNV: TglSampleMaskIndexedNV;
  glTexRenderbufferNV: TglTexRenderbufferNV;

  // GL_NV_transform_feedback2
  glBindTransformFeedbackNV: TglBindTransformFeedbackNV;
  glDeleteTransformFeedbacksNV: TglDeleteTransformFeedbacksNV;
  glGenTransformFeedbacksNV: TglGenTransformFeedbacksNV;
  glIsTransformFeedbackNV: TglIsTransformFeedbackNV;
  glPauseTransformFeedbackNV: TglPauseTransformFeedbackNV;
  glResumeTransformFeedbackNV: TglResumeTransformFeedbackNV;
  glDrawTransformFeedbackNV: TglDrawTransformFeedbackNV;

  // GL_PGI_misc_hints
  glHintPGI: TglHintPGI;

  // GL_SGIS_detail_texture
  glDetailTexFuncSGIS: TglDetailTexFuncSGIS;
  glGetDetailTexFuncSGIS: TglGetDetailTexFuncSGIS;

  // GL_SGIS_fog_function
  glFogFuncSGIS: TglFogFuncSGIS;
  glGetFogFuncSGIS: TglGetFogFuncSGIS;

  // GL_SGIS_multisample
  glSampleMaskSGIS: TglSampleMaskSGIS;
  glSamplePatternSGIS: TglSamplePatternSGIS;

  // GL_SGIS_pixel_texture
  glPixelTexGenParameteriSGIS: TglPixelTexGenParameteriSGIS;
  glPixelTexGenParameterivSGIS: TglPixelTexGenParameterivSGIS;
  glPixelTexGenParameterfSGIS: TglPixelTexGenParameterfSGIS;
  glPixelTexGenParameterfvSGIS: TglPixelTexGenParameterfvSGIS;
  glGetPixelTexGenParameterivSGIS: TglGetPixelTexGenParameterivSGIS;
  glGetPixelTexGenParameterfvSGIS: TglGetPixelTexGenParameterfvSGIS;

  // GL_SGIS_point_parameters
  glPointParameterfSGIS: TglPointParameterfSGIS;
  glPointParameterfvSGIS: TglPointParameterfvSGIS;

  // GL_SGIS_sharpen_texture
  glSharpenTexFuncSGIS: TglSharpenTexFuncSGIS;
  glGetSharpenTexFuncSGIS: TglGetSharpenTexFuncSGIS;

  // GL_SGIS_texture4D
  glTexImage4DSGIS: TglTexImage4DSGIS;
  glTexSubImage4DSGIS: TglTexSubImage4DSGIS;

  // GL_SGIS_texture_color_mask
  glTextureColorMaskSGIS: TglTextureColorMaskSGIS;

  // GL_SGIS_texture_filter4
  glGetTexFilterFuncSGIS: TglGetTexFilterFuncSGIS;
  glTexFilterFuncSGIS: TglTexFilterFuncSGIS;

  // GL_SGIX_async
  glAsyncMarkerSGIX: TglAsyncMarkerSGIX;
  glFinishAsyncSGIX: TglFinishAsyncSGIX;
  glPollAsyncSGIX: TglPollAsyncSGIX;
  glGenAsyncMarkersSGIX: TglGenAsyncMarkersSGIX;
  glDeleteAsyncMarkersSGIX: TglDeleteAsyncMarkersSGIX;
  glIsAsyncMarkerSGIX: TglIsAsyncMarkerSGIX;

  // GL_SGIX_flush_raster
  glFlushRasterSGIX: TglFlushRasterSGIX;

  // GL_SGIX_fragment_lighting
  glFragmentColorMaterialSGIX: TglFragmentColorMaterialSGIX;
  glFragmentLightfSGIX: TglFragmentLightfSGIX;
  glFragmentLightfvSGIX: TglFragmentLightfvSGIX;
  glFragmentLightiSGIX: TglFragmentLightiSGIX;
  glFragmentLightivSGIX: TglFragmentLightivSGIX;
  glFragmentLightModelfSGIX: TglFragmentLightModelfSGIX;
  glFragmentLightModelfvSGIX: TglFragmentLightModelfvSGIX;
  glFragmentLightModeliSGIX: TglFragmentLightModeliSGIX;
  glFragmentLightModelivSGIX: TglFragmentLightModelivSGIX;
  glFragmentMaterialfSGIX: TglFragmentMaterialfSGIX;
  glFragmentMaterialfvSGIX: TglFragmentMaterialfvSGIX;
  glFragmentMaterialiSGIX: TglFragmentMaterialiSGIX;
  glFragmentMaterialivSGIX: TglFragmentMaterialivSGIX;
  glGetFragmentLightfvSGIX: TglGetFragmentLightfvSGIX;
  glGetFragmentLightivSGIX: TglGetFragmentLightivSGIX;
  glGetFragmentMaterialfvSGIX: TglGetFragmentMaterialfvSGIX;
  glGetFragmentMaterialivSGIX: TglGetFragmentMaterialivSGIX;
  glLightEnviSGIX: TglLightEnviSGIX;

  // GL_SGIX_framezoom
  glFrameZoomSGIX: TglFrameZoomSGIX;

  // GL_SGIX_igloo_interface
  glIglooInterfaceSGIX: TglIglooInterfaceSGIX;

  // GL_SGIX_instruments
  glGetInstrumentsSGIX: TglGetInstrumentsSGIX;
  glInstrumentsBufferSGIX: TglInstrumentsBufferSGIX;
  glPollInstrumentsSGIX: TglPollInstrumentsSGIX;
  glReadInstrumentsSGIX: TglReadInstrumentsSGIX;
  glStartInstrumentsSGIX: TglStartInstrumentsSGIX;
  glStopInstrumentsSGIX: TglStopInstrumentsSGIX;

  // GL_SGIX_list_priority
  glGetListParameterfvSGIX: TglGetListParameterfvSGIX;
  glGetListParameterivSGIX: TglGetListParameterivSGIX;
  glListParameterfSGIX: TglListParameterfSGIX;
  glListParameterfvSGIX: TglListParameterfvSGIX;
  glListParameteriSGIX: TglListParameteriSGIX;
  glListParameterivSGIX: TglListParameterivSGIX;

  // GL_SGIX_pixel_texture
  glPixelTexGenSGIX: TglPixelTexGenSGIX;

  // GL_SGIX_polynomial_ffd
  glDeformationMap3dSGIX: TglDeformationMap3dSGIX;
  glDeformationMap3fSGIX: TglDeformationMap3fSGIX;
  glDeformSGIX: TglDeformSGIX;
  glLoadIdentityDeformationMapSGIX: TglLoadIdentityDeformationMapSGIX;

  // GL_SGIX_reference_plane
  glReferencePlaneSGIX: TglReferencePlaneSGIX;

  // GL_SGIX_sprite
  glSpriteParameterfSGIX: TglSpriteParameterfSGIX;
  glSpriteParameterfvSGIX: TglSpriteParameterfvSGIX;
  glSpriteParameteriSGIX: TglSpriteParameteriSGIX;
  glSpriteParameterivSGIX: TglSpriteParameterivSGIX;

  // GL_SGIX_tag_sample_buffer
  glTagSampleBufferSGIX: TglTagSampleBufferSGIX;

  // GL_SGI_color_table
  glColorTableSGI: TglColorTableSGI;
  glColorTableParameterfvSGI: TglColorTableParameterfvSGI;
  glColorTableParameterivSGI: TglColorTableParameterivSGI;
  glCopyColorTableSGI: TglCopyColorTableSGI;
  glGetColorTableSGI: TglGetColorTableSGI;
  glGetColorTableParameterfvSGI: TglGetColorTableParameterfvSGI;
  glGetColorTableParameterivSGI: TglGetColorTableParameterivSGI;

  // GL_SUNX_constant_data
  glFinishTextureSUNX: TglFinishTextureSUNX;

  // GL_SUN_global_alpha
  glGlobalAlphaFactorbSUN: TglGlobalAlphaFactorbSUN;
  glGlobalAlphaFactorsSUN: TglGlobalAlphaFactorsSUN;
  glGlobalAlphaFactoriSUN: TglGlobalAlphaFactoriSUN;
  glGlobalAlphaFactorfSUN: TglGlobalAlphaFactorfSUN;
  glGlobalAlphaFactordSUN: TglGlobalAlphaFactordSUN;
  glGlobalAlphaFactorubSUN: TglGlobalAlphaFactorubSUN;
  glGlobalAlphaFactorusSUN: TglGlobalAlphaFactorusSUN;
  glGlobalAlphaFactoruiSUN: TglGlobalAlphaFactoruiSUN;

  // GL_SUN_mesh_array
  glDrawMeshArraysSUN: TglDrawMeshArraysSUN;

  // GL_SUN_triangle_list
  glReplacementCodeuiSUN: TglReplacementCodeuiSUN;
  glReplacementCodeusSUN: TglReplacementCodeusSUN;
  glReplacementCodeubSUN: TglReplacementCodeubSUN;
  glReplacementCodeuivSUN: TglReplacementCodeuivSUN;
  glReplacementCodeusvSUN: TglReplacementCodeusvSUN;
  glReplacementCodeubvSUN: TglReplacementCodeubvSUN;
  glReplacementCodePointerSUN: TglReplacementCodePointerSUN;

  // GL_SUN_vertex
  glColor4ubVertex2fSUN: TglColor4ubVertex2fSUN;
  glColor4ubVertex2fvSUN: TglColor4ubVertex2fvSUN;
  glColor4ubVertex3fSUN: TglColor4ubVertex3fSUN;
  glColor4ubVertex3fvSUN: TglColor4ubVertex3fvSUN;
  glColor3fVertex3fSUN: TglColor3fVertex3fSUN;
  glColor3fVertex3fvSUN: TglColor3fVertex3fvSUN;
  glNormal3fVertex3fSUN: TglNormal3fVertex3fSUN;
  glNormal3fVertex3fvSUN: TglNormal3fVertex3fvSUN;
  glColor4fNormal3fVertex3fSUN: TglColor4fNormal3fVertex3fSUN;
  glColor4fNormal3fVertex3fvSUN: TglColor4fNormal3fVertex3fvSUN;
  glTexCoord2fVertex3fSUN: TglTexCoord2fVertex3fSUN;
  glTexCoord2fVertex3fvSUN: TglTexCoord2fVertex3fvSUN;
  glTexCoord4fVertex4fSUN: TglTexCoord4fVertex4fSUN;
  glTexCoord4fVertex4fvSUN: TglTexCoord4fVertex4fvSUN;
  glTexCoord2fColor4ubVertex3fSUN: TglTexCoord2fColor4ubVertex3fSUN;
  glTexCoord2fColor4ubVertex3fvSUN: TglTexCoord2fColor4ubVertex3fvSUN;
  glTexCoord2fColor3fVertex3fSUN: TglTexCoord2fColor3fVertex3fSUN;
  glTexCoord2fColor3fVertex3fvSUN: TglTexCoord2fColor3fVertex3fvSUN;
  glTexCoord2fNormal3fVertex3fSUN: TglTexCoord2fNormal3fVertex3fSUN;
  glTexCoord2fNormal3fVertex3fvSUN: TglTexCoord2fNormal3fVertex3fvSUN;
  glTexCoord2fColor4fNormal3fVertex3fSUN: TglTexCoord2fColor4fNormal3fVertex3fSUN;
  glTexCoord2fColor4fNormal3fVertex3fvSUN: TglTexCoord2fColor4fNormal3fVertex3fvSUN;
  glTexCoord4fColor4fNormal3fVertex4fSUN: TglTexCoord4fColor4fNormal3fVertex4fSUN;
  glTexCoord4fColor4fNormal3fVertex4fvSUN: TglTexCoord4fColor4fNormal3fVertex4fvSUN;
  glReplacementCodeuiVertex3fSUN: TglReplacementCodeuiVertex3fSUN;
  glReplacementCodeuiVertex3fvSUN: TglReplacementCodeuiVertex3fvSUN;
  glReplacementCodeuiColor4ubVertex3fSUN: TglReplacementCodeuiColor4ubVertex3fSUN;
  glReplacementCodeuiColor4ubVertex3fvSUN: TglReplacementCodeuiColor4ubVertex3fvSUN;
  glReplacementCodeuiColor3fVertex3fSUN: TglReplacementCodeuiColor3fVertex3fSUN;
  glReplacementCodeuiColor3fVertex3fvSUN: TglReplacementCodeuiColor3fVertex3fvSUN;
  glReplacementCodeuiNormal3fVertex3fSUN: TglReplacementCodeuiNormal3fVertex3fSUN;
  glReplacementCodeuiNormal3fVertex3fvSUN: TglReplacementCodeuiNormal3fVertex3fvSUN;
  glReplacementCodeuiColor4fNormal3fVertex3fSUN: TglReplacementCodeuiColor4fNormal3fVertex3fSUN;
  glReplacementCodeuiColor4fNormal3fVertex3fvSUN: TglReplacementCodeuiColor4fNormal3fVertex3fvSUN;
  glReplacementCodeuiTexCoord2fVertex3fSUN: TglReplacementCodeuiTexCoord2fVertex3fSUN;
  glReplacementCodeuiTexCoord2fVertex3fvSUN: TglReplacementCodeuiTexCoord2fVertex3fvSUN;
  glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN: TglReplacementCodeuiTexCoord2fNormal3fVertex3fSUN;
  glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN: TglReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN;
  glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN: TglReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN;
  glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN: TglReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN;

{$IFDEF Win32}
  wglGetProcAddress: TwglGetProcAddress;
  wglCopyContext: TwglCopyContext;
  wglCreateContext: TwglCreateContext;
  wglCreateLayerContext: TwglCreateLayerContext;
  wglDeleteContext: TwglDeleteContext;
  wglDescribeLayerPlane: TwglDescribeLayerPlane;
  wglGetCurrentContext: TwglGetCurrentContext;
  wglGetCurrentDC: TwglGetCurrentDC;
  wglGetLayerPaletteEntries: TwglGetLayerPaletteEntries;
  wglMakeCurrent: TwglMakeCurrent;
  wglRealizeLayerPalette: TwglRealizeLayerPalette;
  wglSetLayerPaletteEntries: TwglSetLayerPaletteEntries;
  wglShareLists: TwglShareLists;
  wglSwapLayerBuffers: TwglSwapLayerBuffers;
  wglSwapMultipleBuffers: TwglSwapMultipleBuffers;
  wglUseFontBitmapsA: TwglUseFontBitmapsA;
  wglUseFontOutlinesA: TwglUseFontOutlinesA;
  wglUseFontBitmapsW: TwglUseFontBitmapsW;
  wglUseFontOutlinesW: TwglUseFontOutlinesW;
  wglUseFontBitmaps: TwglUseFontBitmaps;
  wglUseFontOutlines: TwglUseFontOutlines;

   // WGL_ARB_buffer_region
  wglCreateBufferRegionARB: TwglCreateBufferRegionARB;
  wglDeleteBufferRegionARB: TwglDeleteBufferRegionARB;
  wglSaveBufferRegionARB: TwglSaveBufferRegionARB;
  wglRestoreBufferRegionARB: TwglRestoreBufferRegionARB;

  // WGL_ARB_extensions_string
  wglGetExtensionsStringARB: TwglGetExtensionsStringARB;

  // WGL_ARB_make_current_read
  wglMakeContextCurrentARB: TwglMakeContextCurrentARB;
  wglGetCurrentReadDCARB: TwglGetCurrentReadDCARB;

  // WGL_ARB_pbuffer
  wglCreatePbufferARB: TwglCreatePbufferARB;
  wglGetPbufferDCARB: TwglGetPbufferDCARB;
  wglReleasePbufferDCARB: TwglReleasePbufferDCARB;
  wglDestroyPbufferARB: TwglDestroyPbufferARB;
  wglQueryPbufferARB: TwglQueryPbufferARB;

  // WGL_ARB_pixel_format
  wglGetPixelFormatAttribivARB: TwglGetPixelFormatAttribivARB;
  wglGetPixelFormatAttribfvARB: TwglGetPixelFormatAttribfvARB;
  wglChoosePixelFormatARB: TwglChoosePixelFormatARB;
  // WGL_ARB_color_buffer_float
  wglClampColorARB: TwglClampColorARB;

  // WGL_ARB_render_texture
  wglBindTexImageARB: TwglBindTexImageARB;
  wglReleaseTexImageARB: TwglReleaseTexImageARB;
  wglSetPbufferAttribARB: TwglSetPbufferAttribARB;

  // WGL_ARB_create_context
  wglCreateContextAttribsARB: TwglCreateContextAttribsARB;

  // WGL_EXT_display_color_table
  wglCreateDisplayColorTableEXT: TwglCreateDisplayColorTableEXT;
  wglLoadDisplayColorTableEXT: TwglLoadDisplayColorTableEXT;
  wglBindDisplayColorTableEXT: TwglBindDisplayColorTableEXT;
  wglDestroyDisplayColorTableEXT: TwglDestroyDisplayColorTableEXT;

  // WGL_EXT_extensions_string
  wglGetExtensionsStringEXT: TwglGetExtensionsStringEXT;

  // WGL_EXT_make_current_read
  wglMakeContextCurrentEXT: TwglMakeContextCurrentEXT;
  wglGetCurrentReadDCEXT: TwglGetCurrentReadDCEXT;

  // WGL_EXT_pbuffer
  wglCreatePbufferEXT: TwglCreatePbufferEXT;
  wglGetPbufferDCEXT: TwglGetPbufferDCEXT;
  wglReleasePbufferDCEXT: TwglReleasePbufferDCEXT;
  wglDestroyPbufferEXT: TwglDestroyPbufferEXT;
  wglQueryPbufferEXT: TwglQueryPbufferEXT;

  // WGL_EXT_pixel_format
  wglGetPixelFormatAttribivEXT: TwglGetPixelFormatAttribivEXT;
  wglGetPixelFormatAttribfvEXT: TwglGetPixelFormatAttribfvEXT;
  wglChoosePixelFormatEXT: TwglChoosePixelFormatEXT;

  // WGL_EXT_swap_control
  wglSwapIntervalEXT: TwglSwapIntervalEXT;
  wglGetSwapIntervalEXT: TwglGetSwapIntervalEXT;

  // WGL_I3D_digital_video_control
  wglGetDigitalVideoParametersI3D: TwglGetDigitalVideoParametersI3D;
  wglSetDigitalVideoParametersI3D: TwglSetDigitalVideoParametersI3D;

  // WGL_I3D_gamma
  wglGetGammaTableParametersI3D: TwglGetGammaTableParametersI3D;
  wglSetGammaTableParametersI3D: TwglSetGammaTableParametersI3D;
  wglGetGammaTableI3D: TwglGetGammaTableI3D;
  wglSetGammaTableI3D: TwglSetGammaTableI3D;

  // WGL_I3D_genlock
  wglEnableGenlockI3D: TwglEnableGenlockI3D;
  wglDisableGenlockI3D: TwglDisableGenlockI3D;
  wglIsEnabledGenlockI3D: TwglIsEnabledGenlockI3D;
  wglGenlockSourceI3D: TwglGenlockSourceI3D;
  wglGetGenlockSourceI3D: TwglGetGenlockSourceI3D;
  wglGenlockSourceEdgeI3D: TwglGenlockSourceEdgeI3D;
  wglGetGenlockSourceEdgeI3D: TwglGetGenlockSourceEdgeI3D;
  wglGenlockSampleRateI3D: TwglGenlockSampleRateI3D;
  wglGetGenlockSampleRateI3D: TwglGetGenlockSampleRateI3D;
  wglGenlockSourceDelayI3D: TwglGenlockSourceDelayI3D;
  wglGetGenlockSourceDelayI3D: TwglGetGenlockSourceDelayI3D;
  wglQueryGenlockMaxSourceDelayI3D: TwglQueryGenlockMaxSourceDelayI3D;

  // WGL_I3D_image_buffer
  wglCreateImageBufferI3D: TwglCreateImageBufferI3D;
  wglDestroyImageBufferI3D: TwglDestroyImageBufferI3D;
  wglAssociateImageBufferEventsI3D: TwglAssociateImageBufferEventsI3D;
  wglReleaseImageBufferEventsI3D: TwglReleaseImageBufferEventsI3D;

  // WGL_I3D_swap_frame_lock
  wglEnableFrameLockI3D: TwglEnableFrameLockI3D;
  wglDisableFrameLockI3D: TwglDisableFrameLockI3D;
  wglIsEnabledFrameLockI3D: TwglIsEnabledFrameLockI3D;
  wglQueryFrameLockMasterI3D: TwglQueryFrameLockMasterI3D;

  // WGL_I3D_swap_frame_usage
  wglGetFrameUsageI3D: TwglGetFrameUsageI3D;
  wglBeginFrameTrackingI3D: TwglBeginFrameTrackingI3D;
  wglEndFrameTrackingI3D: TwglEndFrameTrackingI3D;
  wglQueryFrameTrackingI3D: TwglQueryFrameTrackingI3D;

  // WGL_NV_vertex_array_range
  wglAllocateMemoryNV: TwglAllocateMemoryNV;
  wglFreeMemoryNV: TwglFreeMemoryNV;

  // WGL_NV_present_video
  wglEnumerateVideoDevicesNV: TwglEnumerateVideoDevicesNV;
  wglBindVideoDeviceNV: TwglBindVideoDeviceNV;
  wglQueryCurrentContextNV: TwglQueryCurrentContextNV;

  // WGL_NV_video_out
  wglGetVideoDeviceNV: TwglGetVideoDeviceNV;
  wglReleaseVideoDeviceNV: TwglReleaseVideoDeviceNV;
  wglBindVideoImageNV: TwglBindVideoImageNV;
  wglReleaseVideoImageNV: TwglReleaseVideoImageNV;
  wglSendPbufferToVideoNV: TwglSendPbufferToVideoNV;
  wglGetVideoInfoNV: TwglGetVideoInfoNV;

  // WGL_NV_swap_group
  wglJoinSwapGroupNV: TwglJoinSwapGroupNV;
  wglBindSwapBarrierNV: TwglBindSwapBarrierNV;
  wglQuerySwapGroupNV: TwglQuerySwapGroupNV;
  wglQueryMaxSwapGroupsNV: TwglQueryMaxSwapGroupsNV;
  wglQueryFrameCountNV: TwglQueryFrameCountNV;
  wglResetFrameCountNV: TwglResetFrameCountNV;

  // WGL_NV_gpu_affinity
  wglEnumGpusNV: TwglEnumGpusNV;
  wglEnumGpuDevicesNV: TwglEnumGpuDevicesNV;
  wglCreateAffinityDCNV: TwglCreateAffinityDCNV;
  wglEnumGpusFromAffinityDCNV: TwglEnumGpusFromAffinityDCNV;
  wglDeleteDCNV: TwglDeleteDCNV;

  // WGL_OML_sync_control
  wglGetSyncValuesOML: TwglGetSyncValuesOML;
  wglGetMscRateOML: TwglGetMscRateOML;
  wglSwapBuffersMscOML: TwglSwapBuffersMscOML;
  wglSwapLayerBuffersMscOML: TwglSwapLayerBuffersMscOML;
  wglWaitForMscOML: TwglWaitForMscOML;
  wglWaitForSbcOML: TwglWaitForSbcOML;

  // WIN_draw_range_elements
  glDrawRangeElementsWIN: TglDrawRangeElementsWIN;

  // WIN_swap_hint
  glAddSwapHintRectWIN: TglAddSwapHintRectWIN;

{$ELSE}
  // GLX_VERSION_1_4
  glXGetProcAddress: TglXGetProcAddress;

  // GLX_ARB_get_proc_address
  glXGetProcAddressARB: TglXGetProcAddressARB;
{$ENDIF}

  // GL utility functions and procedures
  gluErrorString: TgluErrorString;
  gluGetString: TgluGetString;
  gluOrtho2D: TgluOrtho2D;
  gluPerspective: TgluPerspective;
  gluPickMatrix: TgluPickMatrix;
  gluLookAt: TgluLookAt;
  gluProject: TgluProject;
  gluUnProject: TgluUnProject;
  gluScaleImage: TgluScaleImage;
  gluBuild1DMipmaps: TgluBuild1DMipmaps;
  gluBuild2DMipmaps: TgluBuild2DMipmaps;
  gluNewQuadric: TgluNewQuadric;
  gluDeleteQuadric: TgluDeleteQuadric;
  gluQuadricNormals: TgluQuadricNormals;
  gluQuadricTexture: TgluQuadricTexture;
  gluQuadricOrientation: TgluQuadricOrientation;
  gluQuadricDrawStyle: TgluQuadricDrawStyle;
  gluCylinder: TgluCylinder;
  gluDisk: TgluDisk;
  gluPartialDisk: TgluPartialDisk;
  gluSphere: TgluSphere;
  gluQuadricCallback: TgluQuadricCallback;
  gluNewTess: TgluNewTess;
  gluDeleteTess: TgluDeleteTess;
  gluTessBeginPolygon: TgluTessBeginPolygon;
  gluTessBeginContour: TgluTessBeginContour;
  gluTessVertex: TgluTessVertex;
  gluTessEndContour: TgluTessEndContour;
  gluTessEndPolygon: TgluTessEndPolygon;
  gluTessProperty: TgluTessProperty;
  gluTessNormal: TgluTessNormal;
  gluTessCallback: TgluTessCallback;
  gluGetTessProperty: TgluGetTessProperty;
  gluNewNurbsRenderer: TgluNewNurbsRenderer;
  gluDeleteNurbsRenderer: TgluDeleteNurbsRenderer;
  gluBeginSurface: TgluBeginSurface;
  gluBeginCurve: TgluBeginCurve;
  gluEndCurve: TgluEndCurve;
  gluEndSurface: TgluEndSurface;
  gluBeginTrim: TgluBeginTrim;
  gluEndTrim: TgluEndTrim;
  gluPwlCurve: TgluPwlCurve;
  gluNurbsCurve: TgluNurbsCurve;
  gluNurbsSurface: TgluNurbsSurface;
  gluLoadSamplingMatrices: TgluLoadSamplingMatrices;
  gluNurbsProperty: TgluNurbsProperty;
  gluGetNurbsProperty: TgluGetNurbsProperty;
  gluNurbsCallback: TgluNurbsCallback;
  gluBeginPolygon: TgluBeginPolygon;
  gluNextContour: TgluNextContour;
  gluEndPolygon: TgluEndPolygon;


type
  TRCOptions = set of (opDoubleBuffered, opGDI, opStereo);

var
  LibHandle: THandle = 0;
  GLULibHandle: THandle = 0;
  LastPixelFormat: Integer;
  ExtensionsRead: Boolean;
  ImplementationRead: Boolean;

{$IFDEF Win32}
  function InitOpenGL(LibName: string = 'OpenGL32.dll'; GLULibName: string = 'GLU32.dll'): Boolean;
{$ELSE}
  {$IFDEF darwin}
    function InitOpenGL(LibName: string = 'libGL.dylib'; GLULibName: string = 'libGLU.dylib'): Boolean;
  {$ELSE}
    function InitOpenGL(LibName: string = 'libGL.so.1'; GLULibName: string = 'libGLU.so.1'): Boolean;
  {$ENDIF}
{$ENDIF}

function dglGetProcAddress(ProcName: PChar): Pointer;
function dglCheckExtension(Extension: String): Boolean;

procedure ReadExtensions;
procedure ReadImplementationProperties;

// =============================================================================
// Helper-Functions
// =============================================================================
{$IFDEF Win32}
  function CreateRenderingContext(DC: HDC; Options: TRCOptions; ColorBits, ZBits, StencilBits, AccumBits, AuxBuffers: Integer; Layer: Integer): HGLRC;
  procedure DestroyRenderingContext(RC: HGLRC);

  procedure ActivateRenderingContext(DC: HDC; RC: HGLRC; loadext: boolean = true);
  procedure DeactivateRenderingContext;
{$ENDIF}


procedure ReadOpenGLCore;
procedure Read_GL_3DFX_tbuffer;
procedure Read_GL_APPLE_element_array;
procedure Read_GL_APPLE_fence;
procedure Read_GL_APPLE_vertex_array_object;
procedure Read_GL_APPLE_vertex_array_range;
procedure Read_GL_ARB_matrix_palette;
procedure Read_GL_ARB_multitexture;
procedure Read_GL_ARB_point_parameters;
procedure Read_GL_ARB_texture_compression;
procedure Read_GL_ARB_transpose_matrix;
procedure Read_GL_ARB_vertex_blend;
procedure Read_GL_ARB_vertex_buffer_object;
procedure Read_GL_ARB_vertex_program;
procedure Read_GL_ARB_window_pos;
procedure Read_GL_ARB_color_buffer_float;
procedure Read_GL_ARB_Shader_Objects;
procedure Read_GL_ARB_occlusion_query;
procedure Read_GL_ARB_draw_instanced;
procedure Read_GL_ARB_framebuffer_object;
procedure Read_GL_ARB_geometry_shader4;
procedure Read_GL_ARB_instanced_arrays;
procedure Read_GL_ARB_map_buffer_range;
procedure Read_GL_ARB_texture_buffer_object;
procedure Read_GL_ARB_vertex_array_object;
procedure Read_GL_ATI_draw_buffers;
procedure Read_GL_ATI_element_array;
procedure Read_GL_ATI_envmap_bumpmap;
procedure Read_GL_ATI_fragment_shader;
procedure Read_GL_ATI_map_object_buffer;
procedure Read_GL_ATI_pn_triangles;
procedure Read_GL_ATI_separate_stencil;
procedure Read_GL_ATI_vertex_array_object;
procedure Read_GL_ATI_vertex_attrib_array_object;
procedure Read_GL_ATI_vertex_streams;
procedure Read_GL_EXT_blend_color;
procedure Read_GL_EXT_blend_func_separate;
procedure Read_GL_EXT_blend_minmax;
procedure Read_GL_EXT_color_subtable;
procedure Read_GL_EXT_compiled_vertex_array;
procedure Read_GL_EXT_convolution;
procedure Read_GL_EXT_coordinate_frame;
procedure Read_GL_EXT_copy_texture;
procedure Read_GL_EXT_cull_vertex;
procedure Read_GL_EXT_draw_range_elements;
procedure Read_GL_EXT_fog_coord;
procedure Read_GL_EXT_framebuffer_object;
procedure Read_GL_EXT_histogram;
procedure Read_GL_EXT_index_func;
procedure Read_GL_EXT_index_material;
procedure Read_GL_EXT_multi_draw_arrays;
procedure Read_GL_EXT_multisample;
procedure Read_GL_EXT_paletted_texture;
procedure Read_GL_EXT_pixel_transform;
procedure Read_GL_EXT_point_parameters;
procedure Read_GL_EXT_polygon_offset;
procedure Read_GL_EXT_secondary_color;
procedure Read_GL_EXT_stencil_two_side;
procedure Read_GL_EXT_subtexture;
procedure Read_GL_EXT_texture3D;
procedure Read_GL_EXT_texture_object;
procedure Read_GL_EXT_texture_perturb_normal;
procedure Read_GL_EXT_vertex_array;
procedure Read_GL_EXT_vertex_shader;
procedure Read_GL_EXT_vertex_weighting;
procedure Read_GL_EXT_depth_bounds_test;
procedure Read_GL_EXT_blend_equation_separate;
procedure Read_GL_EXT_stencil_clear_tag;
procedure Read_GL_EXT_framebuffer_blit;
procedure Read_GL_EXT_framebuffer_multisample;
procedure Read_GL_EXT_timer_query;
procedure Read_GL_EXT_gpu_program_parameters;
procedure Read_GL_EXT_bindable_uniform;
procedure Read_GL_EXT_draw_buffers2;
procedure Read_GL_EXT_draw_instanced;
procedure Read_GL_EXT_geometry_shader4;
procedure Read_GL_EXT_gpu_shader4;
procedure Read_GL_EXT_texture_array;
procedure Read_GL_EXT_texture_buffer_object;
procedure Read_GL_EXT_texture_integer;
procedure Read_GL_EXT_transform_feedback;
procedure Read_GL_EXT_direct_state_access;
procedure Read_GL_HP_image_transform;
procedure Read_GL_IBM_multimode_draw_arrays;
procedure Read_GL_IBM_vertex_array_lists;
procedure Read_GL_INGR_blend_func_separate;
procedure Read_GL_INTEL_parallel_arrays;
procedure Read_GL_MESA_resize_buffers;
procedure Read_GL_MESA_window_pos;
procedure Read_GL_NV_evaluators;
procedure Read_GL_NV_fence;
procedure Read_GL_NV_fragment_program;
procedure Read_GL_NV_half_float;
procedure Read_GL_NV_occlusion_query;
procedure Read_GL_NV_pixel_data_range;
procedure Read_GL_NV_point_sprite;
procedure Read_GL_NV_primitive_restart;
procedure Read_GL_NV_register_combiners;
procedure Read_GL_NV_register_combiners2;
procedure Read_GL_NV_vertex_array_range;
procedure Read_GL_NV_vertex_program;
procedure Read_GL_NV_depth_buffer_float;
procedure Read_GL_NV_framebuffer_multisample_coverage;
procedure Read_GL_NV_geometry_program4;
procedure Read_GL_NV_gpu_program4;
procedure Read_GL_NV_parameter_buffer_object;
procedure Read_GL_NV_transform_feedback;
procedure Read_GL_NV_conditional_render;
procedure Read_GL_NV_explicit_multisample;
procedure Read_GL_NV_transform_feedback2;
procedure Read_GL_PGI_misc_hints;
procedure Read_GL_SGIS_detail_texture;
procedure Read_GL_SGIS_fog_function;
procedure Read_GL_SGIS_multisample;
procedure Read_GL_SGIS_pixel_texture;
procedure Read_GL_SGIS_point_parameters;
procedure Read_GL_SGIS_sharpen_texture;
procedure Read_GL_SGIS_texture4D;
procedure Read_GL_SGIS_texture_color_mask;
procedure Read_GL_SGIS_texture_filter4;
procedure Read_GL_SGIX_async;
procedure Read_GL_SGIX_flush_raster;
procedure Read_GL_SGIX_fragment_lighting;
procedure Read_GL_SGIX_framezoom;
procedure Read_GL_SGIX_igloo_interface;
procedure Read_GL_SGIX_instruments;
procedure Read_GL_SGIX_list_priority;
procedure Read_GL_SGIX_pixel_texture;
procedure Read_GL_SGIX_polynomial_ffd;
procedure Read_GL_SGIX_reference_plane;
procedure Read_GL_SGIX_sprite;
procedure Read_GL_SGIX_tag_sample_buffer;
procedure Read_GL_SGI_color_table;
procedure Read_GL_SUNX_constant_data;
procedure Read_GL_SUN_global_alpha;
procedure Read_GL_SUN_mesh_array;
procedure Read_GL_SUN_triangle_list;
procedure Read_GL_SUN_vertex;

{$IFDEF Win32}
procedure Read_WGL_ARB_buffer_region;
procedure Read_WGL_ARB_extensions_string;
procedure Read_WGL_ARB_make_current_read;
procedure Read_WGL_ARB_pbuffer;
procedure Read_WGL_ARB_pixel_format;
procedure Read_WGL_ARB_pixel_format_float;
procedure Read_WGL_ARB_render_texture;
procedure Read_WGL_ARB_create_context;
procedure Read_WGL_EXT_display_color_table;
procedure Read_WGL_EXT_extensions_string;
procedure Read_WGL_EXT_make_current_read;
procedure Read_WGL_EXT_pbuffer;
procedure Read_WGL_EXT_pixel_format;
procedure Read_WGL_EXT_swap_control;
procedure Read_WGL_I3D_digital_video_control;
procedure Read_WGL_I3D_gamma;
procedure Read_WGL_I3D_genlock;
procedure Read_WGL_I3D_image_buffer;
procedure Read_WGL_I3D_swap_frame_lock;
procedure Read_WGL_I3D_swap_frame_usage;
procedure Read_WGL_NV_vertex_array_range;
procedure Read_WGL_NV_present_video;
procedure Read_WGL_NV_video_out;
procedure Read_WGL_NV_swap_group;
procedure Read_WGL_NV_gpu_affinity;
procedure Read_WGL_OML_sync_control;

procedure Read_WIN_draw_range_elements;
procedure Read_WIN_swap_hint;
{$ENDIF}


implementation


{$IFNDEF Win32}
const
  RTLD_LAZY = $001;
  RTLD_NOW = $002;
  RTLD_BINDING_MASK = $003;

  // Seems to work on Debian / Fedora
  LibraryLib = {$IFDEF Linux} 'libdl.so.2'{$ELSE} 'c'{$ENDIF};

function dlopen(Name: PChar; Flags: LongInt): Pointer; cdecl; external LibraryLib name 'dlopen';
function dlclose(Lib: Pointer): LongInt; cdecl; external LibraryLib name 'dlclose';

function dlsym(Lib: Pointer; Name: PChar): Pointer; cdecl; external LibraryLib name 'dlsym';


function LoadLibrary(Name: PChar): THandle;
begin
  Result := THandle(dlopen(Name, RTLD_LAZY));
end;


function FreeLibrary(LibHandle: THandle): Boolean;
begin
  if LibHandle = 0 then
    Result := False
  else
    Result := dlclose(Pointer(LibHandle)) = 0;
end;


function GetProcAddress(LibHandle: THandle; ProcName: PChar): Pointer;
begin
  Result := dlsym(Pointer(LibHandle), ProcName);
end;
{$ENDIF}


function dglGetProcAddress(ProcName: PChar): Pointer;
begin
  Result := GetProcAddress(LibHandle, ProcName);

  if result <> nil then
    exit;

  {$IFDEF Win32}
    if Addr(wglGetProcAddress) <> nil then
      Result := wglGetProcAddress(ProcName);
  {$ELSE}
    if Addr(glXGetProcAddress) <> nil then
      Result := glXGetProcAddress(ProcName);

    if result <> nil then
      exit;

    if Addr(glXGetProcAddressARB) <> nil then
      Result := glXGetProcAddressARB(ProcName);
  {$ENDIF}
end;


function Int_GetExtensionString: String;
begin
  Result := glGetString(GL_EXTENSIONS);

  if (LibHandle <> 0) then begin
    {$IFDEF Win32}
      wglGetExtensionsStringEXT := dglGetProcAddress('wglGetExtensionsStringEXT');
      wglGetExtensionsStringARB := dglGetProcAddress('wglGetExtensionsStringARB');

      if Assigned(@wglGetExtensionsStringEXT) then
        Result := Result + #32 + wglGetExtensionsStringEXT;

      if Assigned(@wglGetExtensionsStringARB) then
        Result := Result + #32 + wglGetExtensionsStringARB(wglGetCurrentDC);
    {$ENDIF}
  end;

  Result := #32 + Result + #32;
end;


function Int_CheckExtension(AllExtensions, CheckExtension: AnsiString): Boolean;
begin
  Result := Pos(#32 + CheckExtension + #32, AllExtensions) > 0;
end;


function dglCheckExtension(Extension: String): Boolean;
var
  Extensions: String;
begin
  Extensions := Int_GetExtensionString;
  Result := Int_CheckExtension(Extensions, Extension);
end;



function InitOpenGL(LibName: string; GLULibName: string): Boolean;
begin
  Result := False;

  if LibHandle <> 0 then
    FreeLibrary(LibHandle);

  if GLULibHandle <> 0 then
    FreeLibrary(GLULibHandle);

  LibHandle := LoadLibrary(PChar(LibName));
  GLULibHandle := LoadLibrary(PChar(GLULibName));

  {$IFDEF Win32}
    wglGetProcAddress := dglGetProcAddress('wglGetProcAddress');
  {$ELSE}
    glXGetProcAddress := dglGetProcAddress('glXGetProcAddress');
    glXGetProcAddressARB := dglGetProcAddress('glXGetProcAddressARB');
  {$ENDIF}

  if (LibHandle <> 0) then
  begin
    glGetString := TglGetString(dglGetProcAddress('glGetString'));

    // window support routines (WGL) ==============================================
    {$IFDEF Win32}
      wglCopyContext := dglGetProcAddress('wglCopyContext');
      wglCreateLayerContext := dglGetProcAddress('wglCreateLayerContext');
      wglCreateContext := dglGetProcAddress('wglCreateContext');
      wglDeleteContext := dglGetProcAddress('wglDeleteContext');
      wglDescribeLayerPlane := dglGetProcAddress('wglDescribeLayerPlane');
      wglGetCurrentContext := dglGetProcAddress('wglGetCurrentContext');
      wglGetCurrentDC := dglGetProcAddress('wglGetCurrentDC');
      wglGetLayerPaletteEntries := dglGetProcAddress('wglGetLayerPaletteEntries');
      wglMakeCurrent := dglGetProcAddress('wglMakeCurrent');
      wglRealizeLayerPalette := dglGetProcAddress('wglRealizeLayerPalette');
      wglSetLayerPaletteEntries := dglGetProcAddress('wglSetLayerPaletteEntries');
      wglShareLists := dglGetProcAddress('wglShareLists');
      wglSwapLayerBuffers := dglGetProcAddress('wglSwapLayerBuffers');
      wglSwapMultipleBuffers := dglGetProcAddress('wglSwapMultipleBuffers');
      wglUseFontBitmapsA := dglGetProcAddress('wglUseFontBitmapsA');
      wglUseFontOutlinesA := dglGetProcAddress('wglUseFontOutlinesA');
      wglUseFontBitmapsW := dglGetProcAddress('wglUseFontBitmapsW');
      wglUseFontOutlinesW := dglGetProcAddress('wglUseFontOutlinesW');
      wglUseFontBitmaps := dglGetProcAddress('wglUseFontBitmapsA');
      wglUseFontOutlines := dglGetProcAddress('wglUseFontOutlinesA');
    {$ENDIF}
    Result := True;
  end;

  if GLULibHandle <> 0 then
  begin
    // GLU ========================================================================
    gluBeginCurve := TgluBeginCurve(GetProcAddress(GLULibHandle, 'gluBeginCurve'));
    gluBeginPolygon := TgluBeginPolygon(GetProcAddress(GLULibHandle, 'gluBeginPolygon'));
    gluBeginSurface := TgluBeginSurface(GetProcAddress(GLULibHandle, 'gluBeginSurface'));
    gluBeginTrim := TgluBeginTrim(GetProcAddress(GLULibHandle, 'gluBeginTrim'));
    gluBuild1DMipmaps := TgluBuild1DMipmaps(GetProcAddress(GLULibHandle, 'gluBuild1DMipmaps'));
    gluBuild2DMipmaps := TgluBuild2DMipmaps(GetProcAddress(GLULibHandle, 'gluBuild2DMipmaps'));
    gluCylinder := TgluCylinder(GetProcAddress(GLULibHandle, 'gluCylinder'));
    gluDeleteNurbsRenderer := TgluDeleteNurbsRenderer(GetProcAddress(GLULibHandle, 'gluDeleteNurbsRenderer'));
    gluDeleteQuadric := TgluDeleteQuadric(GetProcAddress(GLULibHandle, 'gluDeleteQuadric'));
    gluDeleteTess := TgluDeleteTess(GetProcAddress(GLULibHandle, 'gluDeleteTess'));
    gluDisk := TgluDisk(GetProcAddress(GLULibHandle, 'gluDisk'));
    gluEndCurve := TgluEndCurve(GetProcAddress(GLULibHandle, 'gluEndCurve'));
    gluEndPolygon := TgluEndPolygon(GetProcAddress(GLULibHandle, 'gluEndPolygon'));
    gluEndSurface := TgluEndSurface(GetProcAddress(GLULibHandle, 'gluEndSurface'));
    gluEndTrim := TgluEndTrim(GetProcAddress(GLULibHandle, 'gluEndTrim'));
    gluErrorString := TgluErrorString(GetProcAddress(GLULibHandle, 'gluErrorString'));
    gluGetNurbsProperty := TgluGetNurbsProperty(GetProcAddress(GLULibHandle, 'gluGetNurbsProperty'));
    gluGetString := TgluGetString(GetProcAddress(GLULibHandle, 'gluGetString'));
    gluGetTessProperty := TgluGetTessProperty(GetProcAddress(GLULibHandle, 'gluGetTessProperty'));
    gluLoadSamplingMatrices := TgluLoadSamplingMatrices(GetProcAddress(GLULibHandle, 'gluLoadSamplingMatrices'));
    gluLookAt := TgluLookAt(GetProcAddress(GLULibHandle, 'gluLookAt'));
    gluNewNurbsRenderer := TgluNewNurbsRenderer(GetProcAddress(GLULibHandle, 'gluNewNurbsRenderer'));
    gluNewQuadric := TgluNewQuadric(GetProcAddress(GLULibHandle, 'gluNewQuadric'));
    gluNewTess := TgluNewTess(GetProcAddress(GLULibHandle, 'gluNewTess'));
    gluNextContour := TgluNextContour(GetProcAddress(GLULibHandle, 'gluNextContour'));
    gluNurbsCallback := TgluNurbsCallback(GetProcAddress(GLULibHandle, 'gluNurbsCallback'));
    gluNurbsCurve := TgluNurbsCurve(GetProcAddress(GLULibHandle, 'gluNurbsCurve'));
    gluNurbsProperty := TgluNurbsProperty(GetProcAddress(GLULibHandle, 'gluNurbsProperty'));
    gluNurbsSurface := TgluNurbsSurface(GetProcAddress(GLULibHandle, 'gluNurbsSurface'));
    gluOrtho2D := TgluOrtho2D(GetProcAddress(GLULibHandle, 'gluOrtho2D'));
    gluPartialDisk := TgluPartialDisk(GetProcAddress(GLULibHandle, 'gluPartialDisk'));
    gluPerspective := TgluPerspective(GetProcAddress(GLULibHandle, 'gluPerspective'));
    gluPickMatrix := TgluPickMatrix(GetProcAddress(GLULibHandle, 'gluPickMatrix'));
    gluProject := TgluProject(GetProcAddress(GLULibHandle, 'gluProject'));
    gluPwlCurve := TgluPwlCurve(GetProcAddress(GLULibHandle, 'gluPwlCurve'));
    gluQuadricCallback := TgluQuadricCallback(GetProcAddress(GLULibHandle, 'gluQuadricCallback'));
    gluQuadricDrawStyle := TgluQuadricDrawStyle(GetProcAddress(GLULibHandle, 'gluQuadricDrawStyle'));
    gluQuadricNormals := TgluQuadricNormals(GetProcAddress(GLULibHandle, 'gluQuadricNormals'));
    gluQuadricOrientation := TgluQuadricOrientation(GetProcAddress(GLULibHandle, 'gluQuadricOrientation'));
    gluQuadricTexture := TgluQuadricTexture(GetProcAddress(GLULibHandle, 'gluQuadricTexture'));
    gluScaleImage := TgluScaleImage(GetProcAddress(GLULibHandle, 'gluScaleImage'));
    gluSphere := TgluSphere(GetProcAddress(GLULibHandle, 'gluSphere'));
    gluTessBeginContour := TgluTessBeginContour(GetProcAddress(GLULibHandle, 'gluTessBeginContour'));
    gluTessBeginPolygon := TgluTessBeginPolygon(GetProcAddress(GLULibHandle, 'gluTessBeginPolygon'));
    gluTessCallback := TgluTessCallback(GetProcAddress(GLULibHandle, 'gluTessCallback'));
    gluTessEndContour := TgluTessEndContour(GetProcAddress(GLULibHandle, 'gluTessEndContour'));
    gluTessEndPolygon := TgluTessEndPolygon(GetProcAddress(GLULibHandle, 'gluTessEndPolygon'));
    gluTessNormal := TgluTessNormal(GetProcAddress(GLULibHandle, 'gluTessNormal'));
    gluTessProperty := TgluTessProperty(GetProcAddress(GLULibHandle, 'gluTessProperty'));
    gluTessVertex := TgluTessVertex(GetProcAddress(GLULibHandle, 'gluTessVertex'));
    gluUnProject := TgluUnProject(GetProcAddress(GLULibHandle, 'gluUnProject'));
  end;
end;

procedure ReadOpenGLCore;
begin
  if LibHandle = 0 then
    exit;

  // GL_VERSION_1_1
  glAccum := dglGetProcAddress('glAccum');
  glAlphaFunc := dglGetProcAddress('glAlphaFunc');
  glAreTexturesResident := dglGetProcAddress('glAreTexturesResident');
  glArrayElement := dglGetProcAddress('glArrayElement');
  glBegin := dglGetProcAddress('glBegin');
  glBindTexture := dglGetProcAddress('glBindTexture');
  glBitmap := dglGetProcAddress('glBitmap');
  glBlendFunc := dglGetProcAddress('glBlendFunc');
  glCallList := dglGetProcAddress('glCallList');
  glCallLists := dglGetProcAddress('glCallLists');
  glClear := dglGetProcAddress('glClear');
  glClearAccum := dglGetProcAddress('glClearAccum');
  glClearColor := dglGetProcAddress('glClearColor');
  glClearDepth := dglGetProcAddress('glClearDepth');
  glClearIndex := dglGetProcAddress('glClearIndex');
  glClearStencil := dglGetProcAddress('glClearStencil');
  glClipPlane := dglGetProcAddress('glClipPlane');
  glColor3b := dglGetProcAddress('glColor3b');
  glColor3bv := dglGetProcAddress('glColor3bv');
  glColor3d := dglGetProcAddress('glColor3d');
  glColor3dv := dglGetProcAddress('glColor3dv');
  glColor3f := dglGetProcAddress('glColor3f');
  glColor3fv := dglGetProcAddress('glColor3fv');
  glColor3i := dglGetProcAddress('glColor3i');
  glColor3iv := dglGetProcAddress('glColor3iv');
  glColor3s := dglGetProcAddress('glColor3s');
  glColor3sv := dglGetProcAddress('glColor3sv');
  glColor3ub := dglGetProcAddress('glColor3ub');
  glColor3ubv := dglGetProcAddress('glColor3ubv');
  glColor3ui := dglGetProcAddress('glColor3ui');
  glColor3uiv := dglGetProcAddress('glColor3uiv');
  glColor3us := dglGetProcAddress('glColor3us');
  glColor3usv := dglGetProcAddress('glColor3usv');
  glColor4b := dglGetProcAddress('glColor4b');
  glColor4bv := dglGetProcAddress('glColor4bv');
  glColor4d := dglGetProcAddress('glColor4d');
  glColor4dv := dglGetProcAddress('glColor4dv');
  glColor4f := dglGetProcAddress('glColor4f');
  glColor4fv := dglGetProcAddress('glColor4fv');
  glColor4i := dglGetProcAddress('glColor4i');
  glColor4iv := dglGetProcAddress('glColor4iv');
  glColor4s := dglGetProcAddress('glColor4s');
  glColor4sv := dglGetProcAddress('glColor4sv');
  glColor4ub := dglGetProcAddress('glColor4ub');
  glColor4ubv := dglGetProcAddress('glColor4ubv');
  glColor4ui := dglGetProcAddress('glColor4ui');
  glColor4uiv := dglGetProcAddress('glColor4uiv');
  glColor4us := dglGetProcAddress('glColor4us');
  glColor4usv := dglGetProcAddress('glColor4usv');
  glColorMask := dglGetProcAddress('glColorMask');
  glColorMaterial := dglGetProcAddress('glColorMaterial');
  glColorPointer := dglGetProcAddress('glColorPointer');
  glCopyPixels := dglGetProcAddress('glCopyPixels');
  glCopyTexImage1D := dglGetProcAddress('glCopyTexImage1D');
  glCopyTexImage2D := dglGetProcAddress('glCopyTexImage2D');
  glCopyTexSubImage1D := dglGetProcAddress('glCopyTexSubImage1D');
  glCopyTexSubImage2D := dglGetProcAddress('glCopyTexSubImage2D');
  glCullFace := dglGetProcAddress('glCullFace');
  glDeleteLists := dglGetProcAddress('glDeleteLists');
  glDeleteTextures := dglGetProcAddress('glDeleteTextures');
  glDepthFunc := dglGetProcAddress('glDepthFunc');
  glDepthMask := dglGetProcAddress('glDepthMask');
  glDepthRange := dglGetProcAddress('glDepthRange');
  glDisable := dglGetProcAddress('glDisable');
  glDisableClientState := dglGetProcAddress('glDisableClientState');
  glDrawArrays := dglGetProcAddress('glDrawArrays');
  glDrawBuffer := dglGetProcAddress('glDrawBuffer');
  glDrawElements := dglGetProcAddress('glDrawElements');
  glDrawPixels := dglGetProcAddress('glDrawPixels');
  glEdgeFlag := dglGetProcAddress('glEdgeFlag');
  glEdgeFlagPointer := dglGetProcAddress('glEdgeFlagPointer');
  glEdgeFlagv := dglGetProcAddress('glEdgeFlagv');
  glEnable := dglGetProcAddress('glEnable');
  glEnableClientState := dglGetProcAddress('glEnableClientState');
  glEnd := dglGetProcAddress('glEnd');
  glEndList := dglGetProcAddress('glEndList');
  glEvalCoord1d := dglGetProcAddress('glEvalCoord1d');
  glEvalCoord1dv := dglGetProcAddress('glEvalCoord1dv');
  glEvalCoord1f := dglGetProcAddress('glEvalCoord1f');
  glEvalCoord1fv := dglGetProcAddress('glEvalCoord1fv');
  glEvalCoord2d := dglGetProcAddress('glEvalCoord2d');
  glEvalCoord2dv := dglGetProcAddress('glEvalCoord2dv');
  glEvalCoord2f := dglGetProcAddress('glEvalCoord2f');
  glEvalCoord2fv := dglGetProcAddress('glEvalCoord2fv');
  glEvalMesh1 := dglGetProcAddress('glEvalMesh1');
  glEvalMesh2 := dglGetProcAddress('glEvalMesh2');
  glEvalPoint1 := dglGetProcAddress('glEvalPoint1');
  glEvalPoint2 := dglGetProcAddress('glEvalPoint2');
  glFeedbackBuffer := dglGetProcAddress('glFeedbackBuffer');
  glFinish := dglGetProcAddress('glFinish');
  glFlush := dglGetProcAddress('glFlush');
  glFogf := dglGetProcAddress('glFogf');
  glFogfv := dglGetProcAddress('glFogfv');
  glFogi := dglGetProcAddress('glFogi');
  glFogiv := dglGetProcAddress('glFogiv');
  glFrontFace := dglGetProcAddress('glFrontFace');
  glFrustum := dglGetProcAddress('glFrustum');
  glGenLists := dglGetProcAddress('glGenLists');
  glGenTextures := dglGetProcAddress('glGenTextures');
  glGetBooleanv := dglGetProcAddress('glGetBooleanv');
  glGetClipPlane := dglGetProcAddress('glGetClipPlane');
  glGetDoublev := dglGetProcAddress('glGetDoublev');
  glGetError := dglGetProcAddress('glGetError');
  glGetFloatv := dglGetProcAddress('glGetFloatv');
  glGetIntegerv := dglGetProcAddress('glGetIntegerv');
  glGetLightfv := dglGetProcAddress('glGetLightfv');
  glGetLightiv := dglGetProcAddress('glGetLightiv');
  glGetMapdv := dglGetProcAddress('glGetMapdv');
  glGetMapfv := dglGetProcAddress('glGetMapfv');
  glGetMapiv := dglGetProcAddress('glGetMapiv');
  glGetMaterialfv := dglGetProcAddress('glGetMaterialfv');
  glGetMaterialiv := dglGetProcAddress('glGetMaterialiv');
  glGetPixelMapfv := dglGetProcAddress('glGetPixelMapfv');
  glGetPixelMapuiv := dglGetProcAddress('glGetPixelMapuiv');
  glGetPixelMapusv := dglGetProcAddress('glGetPixelMapusv');
  glGetPointerv := dglGetProcAddress('glGetPointerv');
  glGetPolygonStipple := dglGetProcAddress('glGetPolygonStipple');
  glGetTexEnvfv := dglGetProcAddress('glGetTexEnvfv');
  glGetTexEnviv := dglGetProcAddress('glGetTexEnviv');
  glGetTexGendv := dglGetProcAddress('glGetTexGendv');
  glGetTexGenfv := dglGetProcAddress('glGetTexGenfv');
  glGetTexGeniv := dglGetProcAddress('glGetTexGeniv');
  glGetTexImage := dglGetProcAddress('glGetTexImage');
  glGetTexLevelParameterfv := dglGetProcAddress('glGetTexLevelParameterfv');
  glGetTexLevelParameteriv := dglGetProcAddress('glGetTexLevelParameteriv');
  glGetTexParameterfv := dglGetProcAddress('glGetTexParameterfv');
  glGetTexParameteriv := dglGetProcAddress('glGetTexParameteriv');
  glHint := dglGetProcAddress('glHint');
  glIndexMask := dglGetProcAddress('glIndexMask');
  glIndexPointer := dglGetProcAddress('glIndexPointer');
  glIndexd := dglGetProcAddress('glIndexd');
  glIndexdv := dglGetProcAddress('glIndexdv');
  glIndexf := dglGetProcAddress('glIndexf');
  glIndexfv := dglGetProcAddress('glIndexfv');
  glIndexi := dglGetProcAddress('glIndexi');
  glIndexiv := dglGetProcAddress('glIndexiv');
  glIndexs := dglGetProcAddress('glIndexs');
  glIndexsv := dglGetProcAddress('glIndexsv');
  glIndexub := dglGetProcAddress('glIndexub');
  glIndexubv := dglGetProcAddress('glIndexubv');
  glInitNames := dglGetProcAddress('glInitNames');
  glInterleavedArrays := dglGetProcAddress('glInterleavedArrays');
  glIsEnabled := dglGetProcAddress('glIsEnabled');
  glIsList := dglGetProcAddress('glIsList');
  glIsTexture := dglGetProcAddress('glIsTexture');
  glLightModelf := dglGetProcAddress('glLightModelf');
  glLightModelfv := dglGetProcAddress('glLightModelfv');
  glLightModeli := dglGetProcAddress('glLightModeli');
  glLightModeliv := dglGetProcAddress('glLightModeliv');
  glLightf := dglGetProcAddress('glLightf');
  glLightfv := dglGetProcAddress('glLightfv');
  glLighti := dglGetProcAddress('glLighti');
  glLightiv := dglGetProcAddress('glLightiv');
  glLineStipple := dglGetProcAddress('glLineStipple');
  glLineWidth := dglGetProcAddress('glLineWidth');
  glListBase := dglGetProcAddress('glListBase');
  glLoadIdentity := dglGetProcAddress('glLoadIdentity');
  glLoadMatrixd := dglGetProcAddress('glLoadMatrixd');
  glLoadMatrixf := dglGetProcAddress('glLoadMatrixf');
  glLoadName := dglGetProcAddress('glLoadName');
  glLogicOp := dglGetProcAddress('glLogicOp');
  glMap1d := dglGetProcAddress('glMap1d');
  glMap1f := dglGetProcAddress('glMap1f');
  glMap2d := dglGetProcAddress('glMap2d');
  glMap2f := dglGetProcAddress('glMap2f');
  glMapGrid1d := dglGetProcAddress('glMapGrid1d');
  glMapGrid1f := dglGetProcAddress('glMapGrid1f');
  glMapGrid2d := dglGetProcAddress('glMapGrid2d');
  glMapGrid2f := dglGetProcAddress('glMapGrid2f');
  glMaterialf := dglGetProcAddress('glMaterialf');
  glMaterialfv := dglGetProcAddress('glMaterialfv');
  glMateriali := dglGetProcAddress('glMateriali');
  glMaterialiv := dglGetProcAddress('glMaterialiv');
  glMatrixMode := dglGetProcAddress('glMatrixMode');
  glMultMatrixd := dglGetProcAddress('glMultMatrixd');
  glMultMatrixf := dglGetProcAddress('glMultMatrixf');
  glNewList := dglGetProcAddress('glNewList');
  glNormal3b := dglGetProcAddress('glNormal3b');
  glNormal3bv := dglGetProcAddress('glNormal3bv');
  glNormal3d := dglGetProcAddress('glNormal3d');
  glNormal3dv := dglGetProcAddress('glNormal3dv');
  glNormal3f := dglGetProcAddress('glNormal3f');
  glNormal3fv := dglGetProcAddress('glNormal3fv');
  glNormal3i := dglGetProcAddress('glNormal3i');
  glNormal3iv := dglGetProcAddress('glNormal3iv');
  glNormal3s := dglGetProcAddress('glNormal3s');
  glNormal3sv := dglGetProcAddress('glNormal3sv');
  glNormalPointer := dglGetProcAddress('glNormalPointer');
  glOrtho := dglGetProcAddress('glOrtho');
  glPassThrough := dglGetProcAddress('glPassThrough');
  glPixelMapfv := dglGetProcAddress('glPixelMapfv');
  glPixelMapuiv := dglGetProcAddress('glPixelMapuiv');
  glPixelMapusv := dglGetProcAddress('glPixelMapusv');
  glPixelStoref := dglGetProcAddress('glPixelStoref');
  glPixelStorei := dglGetProcAddress('glPixelStorei');
  glPixelTransferf := dglGetProcAddress('glPixelTransferf');
  glPixelTransferi := dglGetProcAddress('glPixelTransferi');
  glPixelZoom := dglGetProcAddress('glPixelZoom');
  glPointSize := dglGetProcAddress('glPointSize');
  glPolygonMode := dglGetProcAddress('glPolygonMode');
  glPolygonOffset := dglGetProcAddress('glPolygonOffset');
  glPolygonStipple := dglGetProcAddress('glPolygonStipple');
  glPopAttrib := dglGetProcAddress('glPopAttrib');
  glPopClientAttrib := dglGetProcAddress('glPopClientAttrib');
  glPopMatrix := dglGetProcAddress('glPopMatrix');
  glPopName := dglGetProcAddress('glPopName');
  glPrioritizeTextures := dglGetProcAddress('glPrioritizeTextures');
  glPushAttrib := dglGetProcAddress('glPushAttrib');
  glPushClientAttrib := dglGetProcAddress('glPushClientAttrib');
  glPushMatrix := dglGetProcAddress('glPushMatrix');
  glPushName := dglGetProcAddress('glPushName');
  glRasterPos2d := dglGetProcAddress('glRasterPos2d');
  glRasterPos2dv := dglGetProcAddress('glRasterPos2dv');
  glRasterPos2f := dglGetProcAddress('glRasterPos2f');
  glRasterPos2fv := dglGetProcAddress('glRasterPos2fv');
  glRasterPos2i := dglGetProcAddress('glRasterPos2i');
  glRasterPos2iv := dglGetProcAddress('glRasterPos2iv');
  glRasterPos2s := dglGetProcAddress('glRasterPos2s');
  glRasterPos2sv := dglGetProcAddress('glRasterPos2sv');
  glRasterPos3d := dglGetProcAddress('glRasterPos3d');
  glRasterPos3dv := dglGetProcAddress('glRasterPos3dv');
  glRasterPos3f := dglGetProcAddress('glRasterPos3f');
  glRasterPos3fv := dglGetProcAddress('glRasterPos3fv');
  glRasterPos3i := dglGetProcAddress('glRasterPos3i');
  glRasterPos3iv := dglGetProcAddress('glRasterPos3iv');
  glRasterPos3s := dglGetProcAddress('glRasterPos3s');
  glRasterPos3sv := dglGetProcAddress('glRasterPos3sv');
  glRasterPos4d := dglGetProcAddress('glRasterPos4d');
  glRasterPos4dv := dglGetProcAddress('glRasterPos4dv');
  glRasterPos4f := dglGetProcAddress('glRasterPos4f');
  glRasterPos4fv := dglGetProcAddress('glRasterPos4fv');
  glRasterPos4i := dglGetProcAddress('glRasterPos4i');
  glRasterPos4iv := dglGetProcAddress('glRasterPos4iv');
  glRasterPos4s := dglGetProcAddress('glRasterPos4s');
  glRasterPos4sv := dglGetProcAddress('glRasterPos4sv');
  glReadBuffer := dglGetProcAddress('glReadBuffer');
  glReadPixels := dglGetProcAddress('glReadPixels');
  glRectd := dglGetProcAddress('glRectd');
  glRectdv := dglGetProcAddress('glRectdv');
  glRectf := dglGetProcAddress('glRectf');
  glRectfv := dglGetProcAddress('glRectfv');
  glRecti := dglGetProcAddress('glRecti');
  glRectiv := dglGetProcAddress('glRectiv');
  glRects := dglGetProcAddress('glRects');
  glRectsv := dglGetProcAddress('glRectsv');
  glRenderMode := dglGetProcAddress('glRenderMode');
  glRotated := dglGetProcAddress('glRotated');
  glRotatef := dglGetProcAddress('glRotatef');
  glScaled := dglGetProcAddress('glScaled');
  glScalef := dglGetProcAddress('glScalef');
  glScissor := dglGetProcAddress('glScissor');
  glSelectBuffer := dglGetProcAddress('glSelectBuffer');
  glShadeModel := dglGetProcAddress('glShadeModel');
  glStencilFunc := dglGetProcAddress('glStencilFunc');
  glStencilMask := dglGetProcAddress('glStencilMask');
  glStencilOp := dglGetProcAddress('glStencilOp');
  glTexCoord1d := dglGetProcAddress('glTexCoord1d');
  glTexCoord1dv := dglGetProcAddress('glTexCoord1dv');
  glTexCoord1f := dglGetProcAddress('glTexCoord1f');
  glTexCoord1fv := dglGetProcAddress('glTexCoord1fv');
  glTexCoord1i := dglGetProcAddress('glTexCoord1i');
  glTexCoord1iv := dglGetProcAddress('glTexCoord1iv');
  glTexCoord1s := dglGetProcAddress('glTexCoord1s');
  glTexCoord1sv := dglGetProcAddress('glTexCoord1sv');
  glTexCoord2d := dglGetProcAddress('glTexCoord2d');
  glTexCoord2dv := dglGetProcAddress('glTexCoord2dv');
  glTexCoord2f := dglGetProcAddress('glTexCoord2f');
  glTexCoord2fv := dglGetProcAddress('glTexCoord2fv');
  glTexCoord2i := dglGetProcAddress('glTexCoord2i');
  glTexCoord2iv := dglGetProcAddress('glTexCoord2iv');
  glTexCoord2s := dglGetProcAddress('glTexCoord2s');
  glTexCoord2sv := dglGetProcAddress('glTexCoord2sv');
  glTexCoord3d := dglGetProcAddress('glTexCoord3d');
  glTexCoord3dv := dglGetProcAddress('glTexCoord3dv');
  glTexCoord3f := dglGetProcAddress('glTexCoord3f');
  glTexCoord3fv := dglGetProcAddress('glTexCoord3fv');
  glTexCoord3i := dglGetProcAddress('glTexCoord3i');
  glTexCoord3iv := dglGetProcAddress('glTexCoord3iv');
  glTexCoord3s := dglGetProcAddress('glTexCoord3s');
  glTexCoord3sv := dglGetProcAddress('glTexCoord3sv');
  glTexCoord4d := dglGetProcAddress('glTexCoord4d');
  glTexCoord4dv := dglGetProcAddress('glTexCoord4dv');
  glTexCoord4f := dglGetProcAddress('glTexCoord4f');
  glTexCoord4fv := dglGetProcAddress('glTexCoord4fv');
  glTexCoord4i := dglGetProcAddress('glTexCoord4i');
  glTexCoord4iv := dglGetProcAddress('glTexCoord4iv');
  glTexCoord4s := dglGetProcAddress('glTexCoord4s');
  glTexCoord4sv := dglGetProcAddress('glTexCoord4sv');
  glTexCoordPointer := dglGetProcAddress('glTexCoordPointer');
  glTexEnvf := dglGetProcAddress('glTexEnvf');
  glTexEnvfv := dglGetProcAddress('glTexEnvfv');
  glTexEnvi := dglGetProcAddress('glTexEnvi');
  glTexEnviv := dglGetProcAddress('glTexEnviv');
  glTexGend := dglGetProcAddress('glTexGend');
  glTexGendv := dglGetProcAddress('glTexGendv');
  glTexGenf := dglGetProcAddress('glTexGenf');
  glTexGenfv := dglGetProcAddress('glTexGenfv');
  glTexGeni := dglGetProcAddress('glTexGeni');
  glTexGeniv := dglGetProcAddress('glTexGeniv');
  glTexImage1D := dglGetProcAddress('glTexImage1D');
  glTexImage2D := dglGetProcAddress('glTexImage2D');
  glTexParameterf := dglGetProcAddress('glTexParameterf');
  glTexParameterfv := dglGetProcAddress('glTexParameterfv');
  glTexParameteri := dglGetProcAddress('glTexParameteri');
  glTexParameteriv := dglGetProcAddress('glTexParameteriv');
  glTexSubImage1D := dglGetProcAddress('glTexSubImage1D');
  glTexSubImage2D := dglGetProcAddress('glTexSubImage2D');
  glTranslated := dglGetProcAddress('glTranslated');
  glTranslatef := dglGetProcAddress('glTranslatef');
  glVertex2d := dglGetProcAddress('glVertex2d');
  glVertex2dv := dglGetProcAddress('glVertex2dv');
  glVertex2f := dglGetProcAddress('glVertex2f');
  glVertex2fv := dglGetProcAddress('glVertex2fv');
  glVertex2i := dglGetProcAddress('glVertex2i');
  glVertex2iv := dglGetProcAddress('glVertex2iv');
  glVertex2s := dglGetProcAddress('glVertex2s');
  glVertex2sv := dglGetProcAddress('glVertex2sv');
  glVertex3d := dglGetProcAddress('glVertex3d');
  glVertex3dv := dglGetProcAddress('glVertex3dv');
  glVertex3f := dglGetProcAddress('glVertex3f');
  glVertex3fv := dglGetProcAddress('glVertex3fv');
  glVertex3i := dglGetProcAddress('glVertex3i');
  glVertex3iv := dglGetProcAddress('glVertex3iv');
  glVertex3s := dglGetProcAddress('glVertex3s');
  glVertex3sv := dglGetProcAddress('glVertex3sv');
  glVertex4d := dglGetProcAddress('glVertex4d');
  glVertex4dv := dglGetProcAddress('glVertex4dv');
  glVertex4f := dglGetProcAddress('glVertex4f');
  glVertex4fv := dglGetProcAddress('glVertex4fv');
  glVertex4i := dglGetProcAddress('glVertex4i');
  glVertex4iv := dglGetProcAddress('glVertex4iv');
  glVertex4s := dglGetProcAddress('glVertex4s');
  glVertex4sv := dglGetProcAddress('glVertex4sv');
  glVertexPointer := dglGetProcAddress('glVertexPointer');
  glViewport := dglGetProcAddress('glViewport');

  // GL_VERSION_1_2
  glBlendColor := dglGetProcAddress('glBlendColor');
  glBlendEquation := dglGetProcAddress('glBlendEquation');
  glDrawRangeElements := dglGetProcAddress('glDrawRangeElements');
  glColorTable := dglGetProcAddress('glColorTable');
  glColorTableParameterfv := dglGetProcAddress('glColorTableParameterfv');
  glColorTableParameteriv := dglGetProcAddress('glColorTableParameteriv');
  glCopyColorTable := dglGetProcAddress('glCopyColorTable');
  glGetColorTable := dglGetProcAddress('glGetColorTable');
  glGetColorTableParameterfv := dglGetProcAddress('glGetColorTableParameterfv');
  glGetColorTableParameteriv := dglGetProcAddress('glGetColorTableParameteriv');
  glColorSubTable := dglGetProcAddress('glColorSubTable');
  glCopyColorSubTable := dglGetProcAddress('glCopyColorSubTable');
  glConvolutionFilter1D := dglGetProcAddress('glConvolutionFilter1D');
  glConvolutionFilter2D := dglGetProcAddress('glConvolutionFilter2D');
  glConvolutionParameterf := dglGetProcAddress('glConvolutionParameterf');
  glConvolutionParameterfv := dglGetProcAddress('glConvolutionParameterfv');
  glConvolutionParameteri := dglGetProcAddress('glConvolutionParameteri');
  glConvolutionParameteriv := dglGetProcAddress('glConvolutionParameteriv');
  glCopyConvolutionFilter1D := dglGetProcAddress('glCopyConvolutionFilter1D');
  glCopyConvolutionFilter2D := dglGetProcAddress('glCopyConvolutionFilter2D');
  glGetConvolutionFilter := dglGetProcAddress('glGetConvolutionFilter');
  glGetConvolutionParameterfv := dglGetProcAddress('glGetConvolutionParameterfv');
  glGetConvolutionParameteriv := dglGetProcAddress('glGetConvolutionParameteriv');
  glGetSeparableFilter := dglGetProcAddress('glGetSeparableFilter');
  glSeparableFilter2D := dglGetProcAddress('glSeparableFilter2D');
  glGetHistogram := dglGetProcAddress('glGetHistogram');
  glGetHistogramParameterfv := dglGetProcAddress('glGetHistogramParameterfv');
  glGetHistogramParameteriv := dglGetProcAddress('glGetHistogramParameteriv');
  glGetMinmax := dglGetProcAddress('glGetMinmax');
  glGetMinmaxParameterfv := dglGetProcAddress('glGetMinmaxParameterfv');
  glGetMinmaxParameteriv := dglGetProcAddress('glGetMinmaxParameteriv');
  glHistogram := dglGetProcAddress('glHistogram');
  glMinmax := dglGetProcAddress('glMinmax');
  glResetHistogram := dglGetProcAddress('glResetHistogram');
  glResetMinmax := dglGetProcAddress('glResetMinmax');
  glTexImage3D := dglGetProcAddress('glTexImage3D');
  glTexSubImage3D := dglGetProcAddress('glTexSubImage3D');
  glCopyTexSubImage3D := dglGetProcAddress('glCopyTexSubImage3D');

  // GL_VERSION_1_3
  glActiveTexture := dglGetProcAddress('glActiveTexture');
  glClientActiveTexture := dglGetProcAddress('glClientActiveTexture');
  glMultiTexCoord1d := dglGetProcAddress('glMultiTexCoord1d');
  glMultiTexCoord1dv := dglGetProcAddress('glMultiTexCoord1dv');
  glMultiTexCoord1f := dglGetProcAddress('glMultiTexCoord1f');
  glMultiTexCoord1fv := dglGetProcAddress('glMultiTexCoord1fv');
  glMultiTexCoord1i := dglGetProcAddress('glMultiTexCoord1i');
  glMultiTexCoord1iv := dglGetProcAddress('glMultiTexCoord1iv');
  glMultiTexCoord1s := dglGetProcAddress('glMultiTexCoord1s');
  glMultiTexCoord1sv := dglGetProcAddress('glMultiTexCoord1sv');
  glMultiTexCoord2d := dglGetProcAddress('glMultiTexCoord2d');
  glMultiTexCoord2dv := dglGetProcAddress('glMultiTexCoord2dv');
  glMultiTexCoord2f := dglGetProcAddress('glMultiTexCoord2f');
  glMultiTexCoord2fv := dglGetProcAddress('glMultiTexCoord2fv');
  glMultiTexCoord2i := dglGetProcAddress('glMultiTexCoord2i');
  glMultiTexCoord2iv := dglGetProcAddress('glMultiTexCoord2iv');
  glMultiTexCoord2s := dglGetProcAddress('glMultiTexCoord2s');
  glMultiTexCoord2sv := dglGetProcAddress('glMultiTexCoord2sv');
  glMultiTexCoord3d := dglGetProcAddress('glMultiTexCoord3d');
  glMultiTexCoord3dv := dglGetProcAddress('glMultiTexCoord3dv');
  glMultiTexCoord3f := dglGetProcAddress('glMultiTexCoord3f');
  glMultiTexCoord3fv := dglGetProcAddress('glMultiTexCoord3fv');
  glMultiTexCoord3i := dglGetProcAddress('glMultiTexCoord3i');
  glMultiTexCoord3iv := dglGetProcAddress('glMultiTexCoord3iv');
  glMultiTexCoord3s := dglGetProcAddress('glMultiTexCoord3s');
  glMultiTexCoord3sv := dglGetProcAddress('glMultiTexCoord3sv');
  glMultiTexCoord4d := dglGetProcAddress('glMultiTexCoord4d');
  glMultiTexCoord4dv := dglGetProcAddress('glMultiTexCoord4dv');
  glMultiTexCoord4f := dglGetProcAddress('glMultiTexCoord4f');
  glMultiTexCoord4fv := dglGetProcAddress('glMultiTexCoord4fv');
  glMultiTexCoord4i := dglGetProcAddress('glMultiTexCoord4i');
  glMultiTexCoord4iv := dglGetProcAddress('glMultiTexCoord4iv');
  glMultiTexCoord4s := dglGetProcAddress('glMultiTexCoord4s');
  glMultiTexCoord4sv := dglGetProcAddress('glMultiTexCoord4sv');
  glLoadTransposeMatrixf := dglGetProcAddress('glLoadTransposeMatrixf');
  glLoadTransposeMatrixd := dglGetProcAddress('glLoadTransposeMatrixd');
  glMultTransposeMatrixf := dglGetProcAddress('glMultTransposeMatrixf');
  glMultTransposeMatrixd := dglGetProcAddress('glMultTransposeMatrixd');
  glSampleCoverage := dglGetProcAddress('glSampleCoverage');
  glCompressedTexImage3D := dglGetProcAddress('glCompressedTexImage3D');
  glCompressedTexImage2D := dglGetProcAddress('glCompressedTexImage2D');
  glCompressedTexImage1D := dglGetProcAddress('glCompressedTexImage1D');
  glCompressedTexSubImage3D := dglGetProcAddress('glCompressedTexSubImage3D');
  glCompressedTexSubImage2D := dglGetProcAddress('glCompressedTexSubImage2D');
  glCompressedTexSubImage1D := dglGetProcAddress('glCompressedTexSubImage1D');
  glGetCompressedTexImage := dglGetProcAddress('glGetCompressedTexImage');

  // GL_VERSION_1_4
  glBlendFuncSeparate := dglGetProcAddress('glBlendFuncSeparate');
  glFogCoordf := dglGetProcAddress('glFogCoordf');
  glFogCoordfv := dglGetProcAddress('glFogCoordfv');
  glFogCoordd := dglGetProcAddress('glFogCoordd');
  glFogCoorddv := dglGetProcAddress('glFogCoorddv');
  glFogCoordPointer := dglGetProcAddress('glFogCoordPointer');
  glMultiDrawArrays := dglGetProcAddress('glMultiDrawArrays');
  glMultiDrawElements := dglGetProcAddress('glMultiDrawElements');
  glPointParameterf := dglGetProcAddress('glPointParameterf');
  glPointParameterfv := dglGetProcAddress('glPointParameterfv');
  glPointParameteri := dglGetProcAddress('glPointParameteri');
  glPointParameteriv := dglGetProcAddress('glPointParameteriv');
  glSecondaryColor3b := dglGetProcAddress('glSecondaryColor3b');
  glSecondaryColor3bv := dglGetProcAddress('glSecondaryColor3bv');
  glSecondaryColor3d := dglGetProcAddress('glSecondaryColor3d');
  glSecondaryColor3dv := dglGetProcAddress('glSecondaryColor3dv');
  glSecondaryColor3f := dglGetProcAddress('glSecondaryColor3f');
  glSecondaryColor3fv := dglGetProcAddress('glSecondaryColor3fv');
  glSecondaryColor3i := dglGetProcAddress('glSecondaryColor3i');
  glSecondaryColor3iv := dglGetProcAddress('glSecondaryColor3iv');
  glSecondaryColor3s := dglGetProcAddress('glSecondaryColor3s');
  glSecondaryColor3sv := dglGetProcAddress('glSecondaryColor3sv');
  glSecondaryColor3ub := dglGetProcAddress('glSecondaryColor3ub');
  glSecondaryColor3ubv := dglGetProcAddress('glSecondaryColor3ubv');
  glSecondaryColor3ui := dglGetProcAddress('glSecondaryColor3ui');
  glSecondaryColor3uiv := dglGetProcAddress('glSecondaryColor3uiv');
  glSecondaryColor3us := dglGetProcAddress('glSecondaryColor3us');
  glSecondaryColor3usv := dglGetProcAddress('glSecondaryColor3usv');
  glSecondaryColorPointer := dglGetProcAddress('glSecondaryColorPointer');
  glWindowPos2d := dglGetProcAddress('glWindowPos2d');
  glWindowPos2dv := dglGetProcAddress('glWindowPos2dv');
  glWindowPos2f := dglGetProcAddress('glWindowPos2f');
  glWindowPos2fv := dglGetProcAddress('glWindowPos2fv');
  glWindowPos2i := dglGetProcAddress('glWindowPos2i');
  glWindowPos2iv := dglGetProcAddress('glWindowPos2iv');
  glWindowPos2s := dglGetProcAddress('glWindowPos2s');
  glWindowPos2sv := dglGetProcAddress('glWindowPos2sv');
  glWindowPos3d := dglGetProcAddress('glWindowPos3d');
  glWindowPos3dv := dglGetProcAddress('glWindowPos3dv');
  glWindowPos3f := dglGetProcAddress('glWindowPos3f');
  glWindowPos3fv := dglGetProcAddress('glWindowPos3fv');
  glWindowPos3i := dglGetProcAddress('glWindowPos3i');
  glWindowPos3iv := dglGetProcAddress('glWindowPos3iv');
  glWindowPos3s := dglGetProcAddress('glWindowPos3s');
  glWindowPos3sv := dglGetProcAddress('glWindowPos3sv');

  // GL_VERSION_1_5
  glGenQueries := dglGetProcAddress('glGenQueries');
  glDeleteQueries := dglGetProcAddress('glDeleteQueries');
  glIsQuery := dglGetProcAddress('glIsQuery');
  glBeginQuery := dglGetProcAddress('glBeginQuery');
  glEndQuery := dglGetProcAddress('glEndQuery');
  glGetQueryiv := dglGetProcAddress('glGetQueryiv');
  glGetQueryObjectiv := dglGetProcAddress('glGetQueryObjectiv');
  glGetQueryObjectuiv := dglGetProcAddress('glGetQueryObjectuiv');
  glBindBuffer := dglGetProcAddress('glBindBuffer');
  glDeleteBuffers := dglGetProcAddress('glDeleteBuffers');
  glGenBuffers := dglGetProcAddress('glGenBuffers');
  glIsBuffer := dglGetProcAddress('glIsBuffer');
  glBufferData := dglGetProcAddress('glBufferData');
  glBufferSubData := dglGetProcAddress('glBufferSubData');
  glGetBufferSubData := dglGetProcAddress('glGetBufferSubData');
  glMapBuffer := dglGetProcAddress('glMapBuffer');
  glUnmapBuffer := dglGetProcAddress('glUnmapBuffer');
  glGetBufferParameteriv := dglGetProcAddress('glGetBufferParameteriv');
  glGetBufferPointerv := dglGetProcAddress('glGetBufferPointerv');

  // GL_VERSION_2_0
  glBlendEquationSeparate := dglGetProcAddress('glBlendEquationSeparate');
  glDrawBuffers := dglGetProcAddress('glDrawBuffers');
  glStencilOpSeparate := dglGetProcAddress('glStencilOpSeparate');
  glStencilFuncSeparate := dglGetProcAddress('glStencilFuncSeparate');
  glStencilMaskSeparate := dglGetProcAddress('glStencilMaskSeparate');
  glAttachShader := dglGetProcAddress('glAttachShader');
  glBindAttribLocation := dglGetProcAddress('glBindAttribLocation');
  glCompileShader := dglGetProcAddress('glCompileShader');
  glCreateProgram := dglGetProcAddress('glCreateProgram');
  glCreateShader := dglGetProcAddress('glCreateShader');
  glDeleteProgram := dglGetProcAddress('glDeleteProgram');
  glDeleteShader := dglGetProcAddress('glDeleteShader');
  glDetachShader := dglGetProcAddress('glDetachShader');
  glDisableVertexAttribArray := dglGetProcAddress('glDisableVertexAttribArray');
  glEnableVertexAttribArray := dglGetProcAddress('glEnableVertexAttribArray');
  glGetActiveAttrib := dglGetProcAddress('glGetActiveAttrib');
  glGetActiveUniform := dglGetProcAddress('glGetActiveUniform');
  glGetAttachedShaders := dglGetProcAddress('glGetAttachedShaders');
  glGetAttribLocation := dglGetProcAddress('glGetAttribLocation');
  glGetProgramiv := dglGetProcAddress('glGetProgramiv');
  glGetProgramInfoLog := dglGetProcAddress('glGetProgramInfoLog');
  glGetShaderiv := dglGetProcAddress('glGetShaderiv');
  glGetShaderInfoLog := dglGetProcAddress('glGetShaderInfoLog');
  glGetShaderSource := dglGetProcAddress('glGetShaderSource');
  glGetUniformLocation := dglGetProcAddress('glGetUniformLocation');
  glGetUniformfv := dglGetProcAddress('glGetUniformfv');
  glGetUniformiv := dglGetProcAddress('glGetUniformiv');
  glGetVertexAttribfv := dglGetProcAddress('glGetVertexAttribfv');
  glGetVertexAttribiv := dglGetProcAddress('glGetVertexAttribiv');
  glGetVertexAttribPointerv := dglGetProcAddress('glGetVertexAttribPointerv');
  glIsProgram := dglGetProcAddress('glIsProgram');
  glIsShader := dglGetProcAddress('glIsShader');
  glLinkProgram := dglGetProcAddress('glLinkProgram');
  glShaderSource := dglGetProcAddress('glShaderSource');
  glUseProgram := dglGetProcAddress('glUseProgram');
  glUniform1f := dglGetProcAddress('glUniform1f');
  glUniform2f := dglGetProcAddress('glUniform2f');
  glUniform3f := dglGetProcAddress('glUniform3f');
  glUniform4f := dglGetProcAddress('glUniform4f');
  glUniform1i := dglGetProcAddress('glUniform1i');
  glUniform2i := dglGetProcAddress('glUniform2i');
  glUniform3i := dglGetProcAddress('glUniform3i');
  glUniform4i := dglGetProcAddress('glUniform4i');
  glUniform1fv := dglGetProcAddress('glUniform1fv');
  glUniform2fv := dglGetProcAddress('glUniform2fv');
  glUniform3fv := dglGetProcAddress('glUniform3fv');
  glUniform4fv := dglGetProcAddress('glUniform4fv');
  glUniform1iv := dglGetProcAddress('glUniform1iv');
  glUniform2iv := dglGetProcAddress('glUniform2iv');
  glUniform3iv := dglGetProcAddress('glUniform3iv');
  glUniform4iv := dglGetProcAddress('glUniform4iv');
  glUniformMatrix2fv := dglGetProcAddress('glUniformMatrix2fv');
  glUniformMatrix3fv := dglGetProcAddress('glUniformMatrix3fv');
  glUniformMatrix4fv := dglGetProcAddress('glUniformMatrix4fv');
  glValidateProgram := dglGetProcAddress('glValidateProgram');
  glVertexAttrib1d := dglGetProcAddress('glVertexAttrib1d');
  glVertexAttrib1dv := dglGetProcAddress('glVertexAttrib1dv');
  glVertexAttrib1f := dglGetProcAddress('glVertexAttrib1f');
  glVertexAttrib1fv := dglGetProcAddress('glVertexAttrib1fv');
  glVertexAttrib1s := dglGetProcAddress('glVertexAttrib1s');
  glVertexAttrib1sv := dglGetProcAddress('glVertexAttrib1sv');
  glVertexAttrib2d := dglGetProcAddress('glVertexAttrib2d');
  glVertexAttrib2dv := dglGetProcAddress('glVertexAttrib2dv');
  glVertexAttrib2f := dglGetProcAddress('glVertexAttrib2f');
  glVertexAttrib2fv := dglGetProcAddress('glVertexAttrib2fv');
  glVertexAttrib2s := dglGetProcAddress('glVertexAttrib2s');
  glVertexAttrib2sv := dglGetProcAddress('glVertexAttrib2sv');
  glVertexAttrib3d := dglGetProcAddress('glVertexAttrib3d');
  glVertexAttrib3dv := dglGetProcAddress('glVertexAttrib3dv');
  glVertexAttrib3f := dglGetProcAddress('glVertexAttrib3f');
  glVertexAttrib3fv := dglGetProcAddress('glVertexAttrib3fv');
  glVertexAttrib3s := dglGetProcAddress('glVertexAttrib3s');
  glVertexAttrib3sv := dglGetProcAddress('glVertexAttrib3sv');
  glVertexAttrib4Nbv := dglGetProcAddress('glVertexAttrib4Nbv');
  glVertexAttrib4Niv := dglGetProcAddress('glVertexAttrib4Niv');
  glVertexAttrib4Nsv := dglGetProcAddress('glVertexAttrib4Nsv');
  glVertexAttrib4Nub := dglGetProcAddress('glVertexAttrib4Nub');
  glVertexAttrib4Nubv := dglGetProcAddress('glVertexAttrib4Nubv');
  glVertexAttrib4Nuiv := dglGetProcAddress('glVertexAttrib4Nuiv');
  glVertexAttrib4Nusv := dglGetProcAddress('glVertexAttrib4Nusv');
  glVertexAttrib4bv := dglGetProcAddress('glVertexAttrib4bv');
  glVertexAttrib4d := dglGetProcAddress('glVertexAttrib4d');
  glVertexAttrib4dv := dglGetProcAddress('glVertexAttrib4dv');
  glVertexAttrib4f := dglGetProcAddress('glVertexAttrib4f');
  glVertexAttrib4fv := dglGetProcAddress('glVertexAttrib4fv');
  glVertexAttrib4iv := dglGetProcAddress('glVertexAttrib4iv');
  glVertexAttrib4s := dglGetProcAddress('glVertexAttrib4s');
  glVertexAttrib4sv := dglGetProcAddress('glVertexAttrib4sv');
  glVertexAttrib4ubv := dglGetProcAddress('glVertexAttrib4ubv');
  glVertexAttrib4uiv := dglGetProcAddress('glVertexAttrib4uiv');
  glVertexAttrib4usv := dglGetProcAddress('glVertexAttrib4usv');
  glVertexAttribPointer := dglGetProcAddress('glVertexAttribPointer');

  // GL_VERSION_2_1
  glUniformMatrix2x3fv := dglGetProcAddress('glUniformMatrix2x3fv');
  glUniformMatrix3x2fv := dglGetProcAddress('glUniformMatrix3x2fv');
  glUniformMatrix2x4fv := dglGetProcAddress('glUniformMatrix2x4fv');
  glUniformMatrix4x2fv := dglGetProcAddress('glUniformMatrix4x2fv');
  glUniformMatrix3x4fv := dglGetProcAddress('glUniformMatrix3x4fv');
  glUniformMatrix4x3fv := dglGetProcAddress('glUniformMatrix4x3fv');
  
  // GL_VERSION_3_0
  glColorMaski := dglGetProcAddress('glColorMaski');
  glGetBooleani_v := dglGetProcAddress('glGetBooleani_v');
  glGetIntegeri_v := dglGetProcAddress('glGetIntegeri_v');
  glEnablei := dglGetProcAddress('glEnablei');
  glDisablei := dglGetProcAddress('glDisablei');
  glIsEnabledi := dglGetProcAddress('glIsEnabledi');
  glBeginTransformFeedback := dglGetProcAddress('glBeginTransformFeedback');
  glEndTransformFeedback := dglGetProcAddress('glEndTransformFeedback');
  glBindBufferRange := dglGetProcAddress('glBindBufferRange');
  glBindBufferBase := dglGetProcAddress('glBindBufferBase');
  glTransformFeedbackVaryings := dglGetProcAddress('glTransformFeedbackVaryings');
  glGetTransformFeedbackVarying := dglGetProcAddress('glGetTransformFeedbackVarying');
  glClampColor := dglGetProcAddress('glClampColor');
  glBeginConditionalRender := dglGetProcAddress('glBeginConditionalRender');
  glEndConditionalRender := dglGetProcAddress('glEndConditionalRender');
  glVertexAttribI1i := dglGetProcAddress('glVertexAttribI1i');
  glVertexAttribI2i := dglGetProcAddress('glVertexAttribI2i');
  glVertexAttribI3i := dglGetProcAddress('glVertexAttribI3i');
  glVertexAttribI4i := dglGetProcAddress('glVertexAttribI4i');
  glVertexAttribI1ui := dglGetProcAddress('glVertexAttribI1ui');
  glVertexAttribI2ui := dglGetProcAddress('glVertexAttribI2ui');
  glVertexAttribI3ui := dglGetProcAddress('glVertexAttribI3ui');
  glVertexAttribI4ui := dglGetProcAddress('glVertexAttribI4ui');
  glVertexAttribI1iv := dglGetProcAddress('glVertexAttribI1iv');
  glVertexAttribI2iv := dglGetProcAddress('glVertexAttribI2iv');
  glVertexAttribI3iv := dglGetProcAddress('glVertexAttribI3iv');
  glVertexAttribI4iv := dglGetProcAddress('glVertexAttribI4iv');
  glVertexAttribI1uiv := dglGetProcAddress('glVertexAttribI1uiv');
  glVertexAttribI2uiv := dglGetProcAddress('glVertexAttribI2uiv');
  glVertexAttribI3uiv := dglGetProcAddress('glVertexAttribI3uiv');
  glVertexAttribI4uiv := dglGetProcAddress('glVertexAttribI4uiv');
  glVertexAttribI4bv := dglGetProcAddress('glVertexAttribI4bv');
  glVertexAttribI4sv := dglGetProcAddress('glVertexAttribI4sv');
  glVertexAttribI4ubv := dglGetProcAddress('glVertexAttribI4ubv');
  glVertexAttribI4usv := dglGetProcAddress('glVertexAttribI4usv');
  glVertexAttribIPointer := dglGetProcAddress('glVertexAttribIPointer');
  glGetVertexAttribIiv := dglGetProcAddress('glGetVertexAttribIiv');
  glGetVertexAttribIuiv := dglGetProcAddress('glGetVertexAttribIuiv');
  glGetUniformuiv := dglGetProcAddress('glGetUniformuiv');
  glBindFragDataLocation := dglGetProcAddress('glBindFragDataLocation');
  glGetFragDataLocation := dglGetProcAddress('glGetFragDataLocation');
  glUniform1ui := dglGetProcAddress('glUniform1ui');
  glUniform2ui := dglGetProcAddress('glUniform2ui');
  glUniform3ui := dglGetProcAddress('glUniform3ui');
  glUniform4ui := dglGetProcAddress('glUniform4ui');
  glUniform1uiv := dglGetProcAddress('glUniform1uiv');
  glUniform2uiv := dglGetProcAddress('glUniform2uiv');
  glUniform3uiv := dglGetProcAddress('glUniform3uiv');
  glUniform4uiv := dglGetProcAddress('glUniform4uiv');
  glTexParameterIiv := dglGetProcAddress('glTexParameterIiv');
  glTexParameterIuiv := dglGetProcAddress('glTexParameterIuiv');
  glGetTexParameterIiv := dglGetProcAddress('glGetTexParameterIiv');
  glGetTexParameterIuiv := dglGetProcAddress('glGetTexParameterIuiv');
  glClearBufferiv := dglGetProcAddress('glClearBufferiv');
  glClearBufferuiv := dglGetProcAddress('glClearBufferuiv');
  glClearBufferfv := dglGetProcAddress('glClearBufferfv');
  glClearBufferfi := dglGetProcAddress('glClearBufferfi');
  glGetStringi := dglGetProcAddress('glGetStringi');
  
  // Following extensions seems to be part of OpenGL 3.0 and are available as seperate extensions
  Read_GL_ARB_framebuffer_object;
  Read_GL_ARB_instanced_arrays;
  Read_GL_ARB_map_buffer_range;
  Read_GL_ARB_vertex_array_object;
end;

procedure Read_GL_3DFX_tbuffer;
begin
  glTbufferMask3DFX := dglGetProcAddress('glTbufferMask3DFX');
end;

procedure Read_GL_APPLE_element_array;
begin
  glElementPointerAPPLE := dglGetProcAddress('glElementPointerAPPLE');
  glDrawElementArrayAPPLE := dglGetProcAddress('glDrawElementArrayAPPLE');
  glDrawRangeElementArrayAPPLE := dglGetProcAddress('glDrawRangeElementArrayAPPLE');
  glMultiDrawElementArrayAPPLE := dglGetProcAddress('glMultiDrawElementArrayAPPLE');
  glMultiDrawRangeElementArrayAPPLE := dglGetProcAddress('glMultiDrawRangeElementArrayAPPLE');
end;

procedure Read_GL_APPLE_fence;
begin
  glGenFencesAPPLE := dglGetProcAddress('glGenFencesAPPLE');
  glDeleteFencesAPPLE := dglGetProcAddress('glDeleteFencesAPPLE');
  glSetFenceAPPLE := dglGetProcAddress('glSetFenceAPPLE');
  glIsFenceAPPLE := dglGetProcAddress('glIsFenceAPPLE');
  glTestFenceAPPLE := dglGetProcAddress('glTestFenceAPPLE');
  glFinishFenceAPPLE := dglGetProcAddress('glFinishFenceAPPLE');
  glTestObjectAPPLE := dglGetProcAddress('glTestObjectAPPLE');
  glFinishObjectAPPLE := dglGetProcAddress('glFinishObjectAPPLE');
end;

procedure Read_GL_APPLE_vertex_array_object;
begin
  glBindVertexArrayAPPLE := dglGetProcAddress('glBindVertexArrayAPPLE');
  glDeleteVertexArraysAPPLE := dglGetProcAddress('glDeleteVertexArraysAPPLE');
  glGenVertexArraysAPPLE := dglGetProcAddress('glGenVertexArraysAPPLE');
  glIsVertexArrayAPPLE := dglGetProcAddress('glIsVertexArrayAPPLE');
end;

procedure Read_GL_APPLE_vertex_array_range;
begin
  glVertexArrayRangeAPPLE := dglGetProcAddress('glVertexArrayRangeAPPLE');
  glFlushVertexArrayRangeAPPLE := dglGetProcAddress('glFlushVertexArrayRangeAPPLE');
  glVertexArrayParameteriAPPLE := dglGetProcAddress('glVertexArrayParameteriAPPLE');
end;

procedure Read_GL_ARB_matrix_palette;
begin
  glCurrentPaletteMatrixARB := dglGetProcAddress('glCurrentPaletteMatrixARB');
  glMatrixIndexubvARB := dglGetProcAddress('glMatrixIndexubvARB');
  glMatrixIndexusvARB := dglGetProcAddress('glMatrixIndexusvARB');
  glMatrixIndexuivARB := dglGetProcAddress('glMatrixIndexuivARB');
  glMatrixIndexPointerARB := dglGetProcAddress('glMatrixIndexPointerARB');
end;

procedure Read_GL_ARB_multisample;
begin
  glSampleCoverageARB := dglGetProcAddress('glSampleCoverageARB');
end;

procedure Read_GL_ARB_multitexture;
begin
  glActiveTextureARB := dglGetProcAddress('glActiveTextureARB');
  glClientActiveTextureARB := dglGetProcAddress('glClientActiveTextureARB');
  glMultiTexCoord1dARB := dglGetProcAddress('glMultiTexCoord1dARB');
  glMultiTexCoord1dvARB := dglGetProcAddress('glMultiTexCoord1dvARB');
  glMultiTexCoord1fARB := dglGetProcAddress('glMultiTexCoord1fARB');
  glMultiTexCoord1fvARB := dglGetProcAddress('glMultiTexCoord1fvARB');
  glMultiTexCoord1iARB := dglGetProcAddress('glMultiTexCoord1iARB');
  glMultiTexCoord1ivARB := dglGetProcAddress('glMultiTexCoord1ivARB');
  glMultiTexCoord1sARB := dglGetProcAddress('glMultiTexCoord1sARB');
  glMultiTexCoord1svARB := dglGetProcAddress('glMultiTexCoord1svARB');
  glMultiTexCoord2dARB := dglGetProcAddress('glMultiTexCoord2dARB');
  glMultiTexCoord2dvARB := dglGetProcAddress('glMultiTexCoord2dvARB');
  glMultiTexCoord2fARB := dglGetProcAddress('glMultiTexCoord2fARB');
  glMultiTexCoord2fvARB := dglGetProcAddress('glMultiTexCoord2fvARB');
  glMultiTexCoord2iARB := dglGetProcAddress('glMultiTexCoord2iARB');
  glMultiTexCoord2ivARB := dglGetProcAddress('glMultiTexCoord2ivARB');
  glMultiTexCoord2sARB := dglGetProcAddress('glMultiTexCoord2sARB');
  glMultiTexCoord2svARB := dglGetProcAddress('glMultiTexCoord2svARB');
  glMultiTexCoord3dARB := dglGetProcAddress('glMultiTexCoord3dARB');
  glMultiTexCoord3dvARB := dglGetProcAddress('glMultiTexCoord3dvARB');
  glMultiTexCoord3fARB := dglGetProcAddress('glMultiTexCoord3fARB');
  glMultiTexCoord3fvARB := dglGetProcAddress('glMultiTexCoord3fvARB');
  glMultiTexCoord3iARB := dglGetProcAddress('glMultiTexCoord3iARB');
  glMultiTexCoord3ivARB := dglGetProcAddress('glMultiTexCoord3ivARB');
  glMultiTexCoord3sARB := dglGetProcAddress('glMultiTexCoord3sARB');
  glMultiTexCoord3svARB := dglGetProcAddress('glMultiTexCoord3svARB');
  glMultiTexCoord4dARB := dglGetProcAddress('glMultiTexCoord4dARB');
  glMultiTexCoord4dvARB := dglGetProcAddress('glMultiTexCoord4dvARB');
  glMultiTexCoord4fARB := dglGetProcAddress('glMultiTexCoord4fARB');
  glMultiTexCoord4fvARB := dglGetProcAddress('glMultiTexCoord4fvARB');
  glMultiTexCoord4iARB := dglGetProcAddress('glMultiTexCoord4iARB');
  glMultiTexCoord4ivARB := dglGetProcAddress('glMultiTexCoord4ivARB');
  glMultiTexCoord4sARB := dglGetProcAddress('glMultiTexCoord4sARB');
  glMultiTexCoord4svARB := dglGetProcAddress('glMultiTexCoord4svARB');
end;

procedure Read_GL_ARB_point_parameters;
begin
  glPointParameterfARB := dglGetProcAddress('glPointParameterfARB');
  glPointParameterfvARB := dglGetProcAddress('glPointParameterfvARB');
end;

procedure Read_GL_ARB_texture_compression;
begin
  glCompressedTexImage3DARB := dglGetProcAddress('glCompressedTexImage3DARB');
  glCompressedTexImage2DARB := dglGetProcAddress('glCompressedTexImage2DARB');
  glCompressedTexImage1DARB := dglGetProcAddress('glCompressedTexImage1DARB');
  glCompressedTexSubImage3DARB := dglGetProcAddress('glCompressedTexSubImage3DARB');
  glCompressedTexSubImage2DARB := dglGetProcAddress('glCompressedTexSubImage2DARB');
  glCompressedTexSubImage1DARB := dglGetProcAddress('glCompressedTexSubImage1DARB');
  glGetCompressedTexImageARB := dglGetProcAddress('glGetCompressedTexImageARB');
end;

procedure Read_GL_ARB_transpose_matrix;
begin
  glLoadTransposeMatrixfARB := dglGetProcAddress('glLoadTransposeMatrixfARB');
  glLoadTransposeMatrixdARB := dglGetProcAddress('glLoadTransposeMatrixdARB');
  glMultTransposeMatrixfARB := dglGetProcAddress('glMultTransposeMatrixfARB');
  glMultTransposeMatrixdARB := dglGetProcAddress('glMultTransposeMatrixdARB');
end;

procedure Read_GL_ARB_vertex_blend;
begin
  glWeightbvARB := dglGetProcAddress('glWeightbvARB');
  glWeightsvARB := dglGetProcAddress('glWeightsvARB');
  glWeightivARB := dglGetProcAddress('glWeightivARB');
  glWeightfvARB := dglGetProcAddress('glWeightfvARB');
  glWeightdvARB := dglGetProcAddress('glWeightdvARB');
  glWeightubvARB := dglGetProcAddress('glWeightubvARB');
  glWeightusvARB := dglGetProcAddress('glWeightusvARB');
  glWeightuivARB := dglGetProcAddress('glWeightuivARB');
  glWeightPointerARB := dglGetProcAddress('glWeightPointerARB');
  glVertexBlendARB := dglGetProcAddress('glVertexBlendARB');
end;

procedure Read_GL_ARB_vertex_buffer_object;
begin
  glBindBufferARB := dglGetProcAddress('glBindBufferARB');
  glDeleteBuffersARB := dglGetProcAddress('glDeleteBuffersARB');
  glGenBuffersARB := dglGetProcAddress('glGenBuffersARB');
  glIsBufferARB := dglGetProcAddress('glIsBufferARB');
  glBufferDataARB := dglGetProcAddress('glBufferDataARB');
  glBufferSubDataARB := dglGetProcAddress('glBufferSubDataARB');
  glGetBufferSubDataARB := dglGetProcAddress('glGetBufferSubDataARB');
  glMapBufferARB := dglGetProcAddress('glMapBufferARB');
  glUnmapBufferARB := dglGetProcAddress('glUnmapBufferARB');
  glGetBufferParameterivARB := dglGetProcAddress('glGetBufferParameterivARB');
  glGetBufferPointervARB := dglGetProcAddress('glGetBufferPointervARB');
end;

procedure Read_GL_ARB_vertex_program;
begin
  glVertexAttrib1dARB := dglGetProcAddress('glVertexAttrib1dARB');
  glVertexAttrib1dvARB := dglGetProcAddress('glVertexAttrib1dvARB');
  glVertexAttrib1fARB := dglGetProcAddress('glVertexAttrib1fARB');
  glVertexAttrib1fvARB := dglGetProcAddress('glVertexAttrib1fvARB');
  glVertexAttrib1sARB := dglGetProcAddress('glVertexAttrib1sARB');
  glVertexAttrib1svARB := dglGetProcAddress('glVertexAttrib1svARB');
  glVertexAttrib2dARB := dglGetProcAddress('glVertexAttrib2dARB');
  glVertexAttrib2dvARB := dglGetProcAddress('glVertexAttrib2dvARB');
  glVertexAttrib2fARB := dglGetProcAddress('glVertexAttrib2fARB');
  glVertexAttrib2fvARB := dglGetProcAddress('glVertexAttrib2fvARB');
  glVertexAttrib2sARB := dglGetProcAddress('glVertexAttrib2sARB');
  glVertexAttrib2svARB := dglGetProcAddress('glVertexAttrib2svARB');
  glVertexAttrib3dARB := dglGetProcAddress('glVertexAttrib3dARB');
  glVertexAttrib3dvARB := dglGetProcAddress('glVertexAttrib3dvARB');
  glVertexAttrib3fARB := dglGetProcAddress('glVertexAttrib3fARB');
  glVertexAttrib3fvARB := dglGetProcAddress('glVertexAttrib3fvARB');
  glVertexAttrib3sARB := dglGetProcAddress('glVertexAttrib3sARB');
  glVertexAttrib3svARB := dglGetProcAddress('glVertexAttrib3svARB');
  glVertexAttrib4NbvARB := dglGetProcAddress('glVertexAttrib4NbvARB');
  glVertexAttrib4NivARB := dglGetProcAddress('glVertexAttrib4NivARB');
  glVertexAttrib4NsvARB := dglGetProcAddress('glVertexAttrib4NsvARB');
  glVertexAttrib4NubARB := dglGetProcAddress('glVertexAttrib4NubARB');
  glVertexAttrib4NubvARB := dglGetProcAddress('glVertexAttrib4NubvARB');
  glVertexAttrib4NuivARB := dglGetProcAddress('glVertexAttrib4NuivARB');
  glVertexAttrib4NusvARB := dglGetProcAddress('glVertexAttrib4NusvARB');
  glVertexAttrib4bvARB := dglGetProcAddress('glVertexAttrib4bvARB');
  glVertexAttrib4dARB := dglGetProcAddress('glVertexAttrib4dARB');
  glVertexAttrib4dvARB := dglGetProcAddress('glVertexAttrib4dvARB');
  glVertexAttrib4fARB := dglGetProcAddress('glVertexAttrib4fARB');
  glVertexAttrib4fvARB := dglGetProcAddress('glVertexAttrib4fvARB');
  glVertexAttrib4ivARB := dglGetProcAddress('glVertexAttrib4ivARB');
  glVertexAttrib4sARB := dglGetProcAddress('glVertexAttrib4sARB');
  glVertexAttrib4svARB := dglGetProcAddress('glVertexAttrib4svARB');
  glVertexAttrib4ubvARB := dglGetProcAddress('glVertexAttrib4ubvARB');
  glVertexAttrib4uivARB := dglGetProcAddress('glVertexAttrib4uivARB');
  glVertexAttrib4usvARB := dglGetProcAddress('glVertexAttrib4usvARB');
  glVertexAttribPointerARB := dglGetProcAddress('glVertexAttribPointerARB');
  glEnableVertexAttribArrayARB := dglGetProcAddress('glEnableVertexAttribArrayARB');
  glDisableVertexAttribArrayARB := dglGetProcAddress('glDisableVertexAttribArrayARB');
  glProgramStringARB := dglGetProcAddress('glProgramStringARB');
  glBindProgramARB := dglGetProcAddress('glBindProgramARB');
  glDeleteProgramsARB := dglGetProcAddress('glDeleteProgramsARB');
  glGenProgramsARB := dglGetProcAddress('glGenProgramsARB');
  glProgramEnvParameter4dARB := dglGetProcAddress('glProgramEnvParameter4dARB');
  glProgramEnvParameter4dvARB := dglGetProcAddress('glProgramEnvParameter4dvARB');
  glProgramEnvParameter4fARB := dglGetProcAddress('glProgramEnvParameter4fARB');
  glProgramEnvParameter4fvARB := dglGetProcAddress('glProgramEnvParameter4fvARB');
  glProgramLocalParameter4dARB := dglGetProcAddress('glProgramLocalParameter4dARB');
  glProgramLocalParameter4dvARB := dglGetProcAddress('glProgramLocalParameter4dvARB');
  glProgramLocalParameter4fARB := dglGetProcAddress('glProgramLocalParameter4fARB');
  glProgramLocalParameter4fvARB := dglGetProcAddress('glProgramLocalParameter4fvARB');
  glGetProgramEnvParameterdvARB := dglGetProcAddress('glGetProgramEnvParameterdvARB');
  glGetProgramEnvParameterfvARB := dglGetProcAddress('glGetProgramEnvParameterfvARB');
  glGetProgramLocalParameterdvARB := dglGetProcAddress('glGetProgramLocalParameterdvARB');
  glGetProgramLocalParameterfvARB := dglGetProcAddress('glGetProgramLocalParameterfvARB');
  glGetProgramivARB := dglGetProcAddress('glGetProgramivARB');
  glGetProgramStringARB := dglGetProcAddress('glGetProgramStringARB');
  glGetVertexAttribdvARB := dglGetProcAddress('glGetVertexAttribdvARB');
  glGetVertexAttribfvARB := dglGetProcAddress('glGetVertexAttribfvARB');
  glGetVertexAttribivARB := dglGetProcAddress('glGetVertexAttribivARB');
  glGetVertexAttribPointervARB := dglGetProcAddress('glGetVertexAttribPointervARB');
  glIsProgramARB := dglGetProcAddress('glIsProgramARB');
end;

procedure Read_GL_ARB_window_pos;
begin
  glWindowPos2dARB := dglGetProcAddress('glWindowPos2dARB');
  glWindowPos2dvARB := dglGetProcAddress('glWindowPos2dvARB');
  glWindowPos2fARB := dglGetProcAddress('glWindowPos2fARB');
  glWindowPos2fvARB := dglGetProcAddress('glWindowPos2fvARB');
  glWindowPos2iARB := dglGetProcAddress('glWindowPos2iARB');
  glWindowPos2ivARB := dglGetProcAddress('glWindowPos2ivARB');
  glWindowPos2sARB := dglGetProcAddress('glWindowPos2sARB');
  glWindowPos2svARB := dglGetProcAddress('glWindowPos2svARB');
  glWindowPos3dARB := dglGetProcAddress('glWindowPos3dARB');
  glWindowPos3dvARB := dglGetProcAddress('glWindowPos3dvARB');
  glWindowPos3fARB := dglGetProcAddress('glWindowPos3fARB');
  glWindowPos3fvARB := dglGetProcAddress('glWindowPos3fvARB');
  glWindowPos3iARB := dglGetProcAddress('glWindowPos3iARB');
  glWindowPos3ivARB := dglGetProcAddress('glWindowPos3ivARB');
  glWindowPos3sARB := dglGetProcAddress('glWindowPos3sARB');
  glWindowPos3svARB := dglGetProcAddress('glWindowPos3svARB');
end;

procedure Read_GL_ARB_draw_buffers;
begin
  glDrawBuffersARB := dglGetProcAddress('glDrawBuffersARB');
end;

procedure Read_GL_ARB_color_buffer_float;
begin
  glClampColorARB := dglGetProcAddress('glClampColorARB');
end;

procedure Read_GL_ARB_Shader_Objects;
begin
  // GL_ARB_Shader_Objects
  glCreateShaderObjectARB := dglGetProcAddress('glCreateShaderObjectARB');
  glShaderSourceARB := dglGetProcAddress('glShaderSourceARB');
  glCompileShaderARB := dglGetProcAddress('glCompileShaderARB');
  glDeleteObjectARB := dglGetProcAddress('glDeleteObjectARB');
  glGetHandleARB := dglGetProcAddress('glGetHandleARB');
  glDetachObjectARB := dglGetProcAddress('glDetachObjectARB');
  glCreateProgramObjectARB := dglGetProcAddress('glCreateProgramObjectARB');
  glAttachObjectARB := dglGetProcAddress('glAttachObjectARB');
  glLinkProgramARB := dglGetProcAddress('glLinkProgramARB');
  glUseProgramObjectARB := dglGetProcAddress('glUseProgramObjectARB');
  glValidateProgramARB := dglGetProcAddress('glValidateProgramARB');
  glGetObjectParameterfvARB := dglGetProcAddress('glGetObjectParameterfvARB');
  glGetObjectParameterivARB := dglGetProcAddress('glGetObjectParameterivARB');
  glGetActiveUniformARB := dglGetProcAddress('glGetActiveUniformARB');
  glGetAttachedObjectsARB := dglGetProcAddress('glGetAttachedObjectsARB');
  glGetShaderSourceARB := dglGetProcAddress('glGetShaderSourceARB');
  glGetUniformfvARB := dglGetProcAddress('glGetUniformfvARB');
  glGetUniformivARB := dglGetProcAddress('glGetUniformivARB');
  glGetUniformLocationARB := dglGetProcAddress('glGetUniformLocationARB');
  glGetInfoLogARB := dglGetProcAddress('glGetInfoLogARB');
  glUniform1fARB := dglGetProcAddress('glUniform1fARB');
  glUniform2fARB := dglGetProcAddress('glUniform2fARB');
  glUniform3fARB := dglGetProcAddress('glUniform3fARB');
  glUniform4fARB := dglGetProcAddress('glUniform4fARB');
  glUniform1iARB := dglGetProcAddress('glUniform1iARB');
  glUniform2iARB := dglGetProcAddress('glUniform2iARB');
  glUniform3iARB := dglGetProcAddress('glUniform3iARB');
  glUniform4iARB := dglGetProcAddress('glUniform4iARB');
  glUniform1fvARB := dglGetProcAddress('glUniform1fvARB');
  glUniform2fvARB := dglGetProcAddress('glUniform2fvARB');
  glUniform3fvARB := dglGetProcAddress('glUniform3fvARB');
  glUniform4fvARB := dglGetProcAddress('glUniform4fvARB');
  glUniform1ivARB := dglGetProcAddress('glUniform1ivARB');
  glUniform2ivARB := dglGetProcAddress('glUniform2ivARB');
  glUniform3ivARB := dglGetProcAddress('glUniform3ivARB');
  glUniform4ivARB := dglGetProcAddress('glUniform4ivARB');
  glUniformMatrix2fvARB := dglGetProcAddress('glUniformMatrix2fvARB');
  glUniformMatrix3fvARB := dglGetProcAddress('glUniformMatrix3fvARB');
  glUniformMatrix4fvARB := dglGetProcAddress('glUniformMatrix4fvARB');

  // GL_ARB_vertex_shader
  glGetActiveAttribARB := dglGetProcAddress('glGetActiveAttribARB');
  glGetAttribLocationARB := dglGetProcAddress('glGetAttribLocationARB');
  glBindAttribLocationARB := dglGetProcAddress('glBindAttribLocationARB');
  glGetVertexAttribPointervARB := dglGetProcAddress('glGetVertexAttribPointervARB');
end;

procedure Read_GL_ARB_occlusion_query;
begin
  glGenQueriesARB := dglGetProcAddress('glGenQueriesARB');
  glDeleteQueriesARB := dglGetProcAddress('glDeleteQueriesARB');
  glIsQueryARB := dglGetProcAddress('glIsQueryARB');
  glBeginQueryARB := dglGetProcAddress('glBeginQueryARB');
  glEndQueryARB := dglGetProcAddress('glEndQueryARB');
  glGetQueryivARB := dglGetProcAddress('glGetQueryivARB');
  glGetQueryObjectivARB := dglGetProcAddress('glGetQueryObjectivARB');
  glGetQueryObjectuivARB := dglGetProcAddress('glGetQueryObjectuivARB');
end;

procedure Read_GL_ARB_draw_instanced;
begin
  glDrawArraysInstancedARB := dglGetProcAddress('glDrawArraysInstancedARB');
  glDrawElementsInstancedARB := dglGetProcAddress('glDrawElementsInstancedARB');
end;

procedure Read_GL_ARB_framebuffer_object;
begin
  glIsRenderbuffer := dglGetProcAddress('glIsRenderbuffer');
  glBindRenderbuffer := dglGetProcAddress('glBindRenderbuffer');
  glDeleteRenderbuffers := dglGetProcAddress('glDeleteRenderbuffers');
  glGenRenderbuffers := dglGetProcAddress('glGenRenderbuffers');
  glRenderbufferStorage := dglGetProcAddress('glRenderbufferStorage');
  glGetRenderbufferParameteriv := dglGetProcAddress('glGetRenderbufferParameteriv');
  glIsFramebuffer := dglGetProcAddress('glIsFramebuffer');
  glBindFramebuffer := dglGetProcAddress('glBindFramebuffer');
  glDeleteFramebuffers := dglGetProcAddress('glDeleteFramebuffers');
  glGenFramebuffers := dglGetProcAddress('glGenFramebuffers');
  glCheckFramebufferStatus := dglGetProcAddress('glCheckFramebufferStatus');
  glFramebufferTexture1D := dglGetProcAddress('glFramebufferTexture1D');
  glFramebufferTexture2D := dglGetProcAddress('glFramebufferTexture2D');
  glFramebufferTexture3D := dglGetProcAddress('glFramebufferTexture3D');
  glFramebufferRenderbuffer := dglGetProcAddress('glFramebufferRenderbuffer');
  glGetFramebufferAttachmentParameteriv := dglGetProcAddress('glGetFramebufferAttachmentParameteriv');
  glGenerateMipmap := dglGetProcAddress('glGenerateMipmap');
  glBlitFramebuffer := dglGetProcAddress('glBlitFramebuffer');
  glRenderbufferStorageMultisample := dglGetProcAddress('glRenderbufferStorageMultisample');
  glFramebufferTextureLayer := dglGetProcAddress('glFramebufferTextureLayer');
end;

procedure Read_GL_ARB_geometry_shader4;
begin
  glProgramParameteriARB := dglGetProcAddress('glProgramParameteriARB');
  glFramebufferTextureARB := dglGetProcAddress('glFramebufferTextureARB');
  glFramebufferTextureLayerARB := dglGetProcAddress('glFramebufferTextureLayerARB');
  glFramebufferTextureFaceARB := dglGetProcAddress('glFramebufferTextureFaceARB');
end;

procedure Read_GL_ARB_instanced_arrays;
begin
  glVertexAttribDivisor := dglGetProcAddress('glVertexAttribDivisor');
end;

procedure Read_GL_ARB_map_buffer_range;
begin
  glMapBufferRange := dglGetProcAddress('glMapBufferRange');
  glFlushMappedBufferRange := dglGetProcAddress('glFlushMappedBufferRange');
end;

procedure Read_GL_ARB_texture_buffer_object;
begin
  glTexBufferARB := dglGetProcAddress('glTexBufferARB');
end;

procedure Read_GL_ARB_vertex_array_object;
begin
  glBindVertexArray := dglGetProcAddress('glBindVertexArray');
  glDeleteVertexArrays := dglGetProcAddress('glDeleteVertexArrays');
  glGenVertexArrays := dglGetProcAddress('glGenVertexArrays');
  glIsVertexArray := dglGetProcAddress('glIsVertexArray');
end;

procedure Read_GL_ATI_draw_buffers;
begin
  glDrawBuffersATI := dglGetProcAddress('glDrawBuffersATI');
end;

procedure Read_GL_ATI_element_array;
begin
  glElementPointerATI := dglGetProcAddress('glElementPointerATI');
  glDrawElementArrayATI := dglGetProcAddress('glDrawElementArrayATI');
  glDrawRangeElementArrayATI := dglGetProcAddress('glDrawRangeElementArrayATI');
end;

procedure Read_GL_ATI_envmap_bumpmap;
begin
  glTexBumpParameterivATI := dglGetProcAddress('glTexBumpParameterivATI');
  glTexBumpParameterfvATI := dglGetProcAddress('glTexBumpParameterfvATI');
  glGetTexBumpParameterivATI := dglGetProcAddress('glGetTexBumpParameterivATI');
  glGetTexBumpParameterfvATI := dglGetProcAddress('glGetTexBumpParameterfvATI');
end;

procedure Read_GL_ATI_fragment_shader;
begin
  glGenFragmentShadersATI := dglGetProcAddress('glGenFragmentShadersATI');
  glBindFragmentShaderATI := dglGetProcAddress('glBindFragmentShaderATI');
  glDeleteFragmentShaderATI := dglGetProcAddress('glDeleteFragmentShaderATI');
  glBeginFragmentShaderATI := dglGetProcAddress('glBeginFragmentShaderATI');
  glEndFragmentShaderATI := dglGetProcAddress('glEndFragmentShaderATI');
  glPassTexCoordATI := dglGetProcAddress('glPassTexCoordATI');
  glSampleMapATI := dglGetProcAddress('glSampleMapATI');
  glColorFragmentOp1ATI := dglGetProcAddress('glColorFragmentOp1ATI');
  glColorFragmentOp2ATI := dglGetProcAddress('glColorFragmentOp2ATI');
  glColorFragmentOp3ATI := dglGetProcAddress('glColorFragmentOp3ATI');
  glAlphaFragmentOp1ATI := dglGetProcAddress('glAlphaFragmentOp1ATI');
  glAlphaFragmentOp2ATI := dglGetProcAddress('glAlphaFragmentOp2ATI');
  glAlphaFragmentOp3ATI := dglGetProcAddress('glAlphaFragmentOp3ATI');
  glSetFragmentShaderConstantATI := dglGetProcAddress('glSetFragmentShaderConstantATI');
end;

procedure Read_GL_ATI_map_object_buffer;
begin
  glMapObjectBufferATI := dglGetProcAddress('glMapObjectBufferATI');
  glUnmapObjectBufferATI := dglGetProcAddress('glUnmapObjectBufferATI');
end;

procedure Read_GL_ATI_pn_triangles;
begin
  glPNTrianglesiATI := dglGetProcAddress('glPNTrianglesiATI');
  glPNTrianglesfATI := dglGetProcAddress('glPNTrianglesfATI');
end;

procedure Read_GL_ATI_separate_stencil;
begin
  glStencilOpSeparateATI := dglGetProcAddress('glStencilOpSeparateATI');
  glStencilFuncSeparateATI := dglGetProcAddress('glStencilFuncSeparateATI');
end;

procedure Read_GL_ATI_vertex_array_object;
begin
  glNewObjectBufferATI := dglGetProcAddress('glNewObjectBufferATI');
  glIsObjectBufferATI := dglGetProcAddress('glIsObjectBufferATI');
  glUpdateObjectBufferATI := dglGetProcAddress('glUpdateObjectBufferATI');
  glGetObjectBufferfvATI := dglGetProcAddress('glGetObjectBufferfvATI');
  glGetObjectBufferivATI := dglGetProcAddress('glGetObjectBufferivATI');
  glFreeObjectBufferATI := dglGetProcAddress('glFreeObjectBufferATI');
  glArrayObjectATI := dglGetProcAddress('glArrayObjectATI');
  glGetArrayObjectfvATI := dglGetProcAddress('glGetArrayObjectfvATI');
  glGetArrayObjectivATI := dglGetProcAddress('glGetArrayObjectivATI');
  glVariantArrayObjectATI := dglGetProcAddress('glVariantArrayObjectATI');
  glGetVariantArrayObjectfvATI := dglGetProcAddress('glGetVariantArrayObjectfvATI');
  glGetVariantArrayObjectivATI := dglGetProcAddress('glGetVariantArrayObjectivATI');

end;

procedure Read_GL_ATI_vertex_attrib_array_object;
begin
  glVertexAttribArrayObjectATI := dglGetProcAddress('glVertexAttribArrayObjectATI');
  glGetVertexAttribArrayObjectfvATI := dglGetProcAddress('glGetVertexAttribArrayObjectfvATI');
  glGetVertexAttribArrayObjectivATI := dglGetProcAddress('glGetVertexAttribArrayObjectivATI');
end;

procedure Read_GL_ATI_vertex_streams;
begin
  glVertexStream1sATI := dglGetProcAddress('glVertexStream1sATI');
  glVertexStream1svATI := dglGetProcAddress('glVertexStream1svATI');
  glVertexStream1iATI := dglGetProcAddress('glVertexStream1iATI');
  glVertexStream1ivATI := dglGetProcAddress('glVertexStream1ivATI');
  glVertexStream1fATI := dglGetProcAddress('glVertexStream1fATI');
  glVertexStream1fvATI := dglGetProcAddress('glVertexStream1fvATI');
  glVertexStream1dATI := dglGetProcAddress('glVertexStream1dATI');
  glVertexStream1dvATI := dglGetProcAddress('glVertexStream1dvATI');
  glVertexStream2sATI := dglGetProcAddress('glVertexStream2sATI');
  glVertexStream2svATI := dglGetProcAddress('glVertexStream2svATI');
  glVertexStream2iATI := dglGetProcAddress('glVertexStream2iATI');
  glVertexStream2ivATI := dglGetProcAddress('glVertexStream2ivATI');
  glVertexStream2fATI := dglGetProcAddress('glVertexStream2fATI');
  glVertexStream2fvATI := dglGetProcAddress('glVertexStream2fvATI');
  glVertexStream2dATI := dglGetProcAddress('glVertexStream2dATI');
  glVertexStream2dvATI := dglGetProcAddress('glVertexStream2dvATI');
  glVertexStream3sATI := dglGetProcAddress('glVertexStream3sATI');
  glVertexStream3svATI := dglGetProcAddress('glVertexStream3svATI');
  glVertexStream3iATI := dglGetProcAddress('glVertexStream3iATI');
  glVertexStream3ivATI := dglGetProcAddress('glVertexStream3ivATI');
  glVertexStream3fATI := dglGetProcAddress('glVertexStream3fATI');
  glVertexStream3fvATI := dglGetProcAddress('glVertexStream3fvATI');
  glVertexStream3dATI := dglGetProcAddress('glVertexStream3dATI');
  glVertexStream3dvATI := dglGetProcAddress('glVertexStream3dvATI');
  glVertexStream4sATI := dglGetProcAddress('glVertexStream4sATI');
  glVertexStream4svATI := dglGetProcAddress('glVertexStream4svATI');
  glVertexStream4iATI := dglGetProcAddress('glVertexStream4iATI');
  glVertexStream4ivATI := dglGetProcAddress('glVertexStream4ivATI');
  glVertexStream4fATI := dglGetProcAddress('glVertexStream4fATI');
  glVertexStream4fvATI := dglGetProcAddress('glVertexStream4fvATI');
  glVertexStream4dATI := dglGetProcAddress('glVertexStream4dATI');
  glVertexStream4dvATI := dglGetProcAddress('glVertexStream4dvATI');
  glNormalStream3bATI := dglGetProcAddress('glNormalStream3bATI');
  glNormalStream3bvATI := dglGetProcAddress('glNormalStream3bvATI');
  glNormalStream3sATI := dglGetProcAddress('glNormalStream3sATI');
  glNormalStream3svATI := dglGetProcAddress('glNormalStream3svATI');
  glNormalStream3iATI := dglGetProcAddress('glNormalStream3iATI');
  glNormalStream3ivATI := dglGetProcAddress('glNormalStream3ivATI');
  glNormalStream3fATI := dglGetProcAddress('glNormalStream3fATI');
  glNormalStream3fvATI := dglGetProcAddress('glNormalStream3fvATI');
  glNormalStream3dATI := dglGetProcAddress('glNormalStream3dATI');
  glNormalStream3dvATI := dglGetProcAddress('glNormalStream3dvATI');
  glClientActiveVertexStreamATI := dglGetProcAddress('glClientActiveVertexStreamATI');
  glVertexBlendEnviATI := dglGetProcAddress('glVertexBlendEnviATI');
  glVertexBlendEnvfATI := dglGetProcAddress('glVertexBlendEnvfATI');
end;

procedure Read_GL_EXT_blend_color;
begin
  glBlendColorEXT := dglGetProcAddress('glBlendColorEXT');
end;

procedure Read_GL_EXT_blend_func_separate;
begin
  glBlendFuncSeparateEXT := dglGetProcAddress('glBlendFuncSeparateEXT');
end;

procedure Read_GL_EXT_blend_minmax;
begin
  glBlendEquationEXT := dglGetProcAddress('glBlendEquationEXT');
end;

procedure Read_GL_EXT_color_subtable;
begin
  glColorSubTableEXT := dglGetProcAddress('glColorSubTableEXT');
  glCopyColorSubTableEXT := dglGetProcAddress('glCopyColorSubTableEXT');
end;

procedure Read_GL_EXT_compiled_vertex_array;
begin
  glLockArraysEXT := dglGetProcAddress('glLockArraysEXT');
  glUnlockArraysEXT := dglGetProcAddress('glUnlockArraysEXT');
end;

procedure Read_GL_EXT_convolution;
begin
  glConvolutionFilter1DEXT := dglGetProcAddress('glConvolutionFilter1DEXT');
  glConvolutionFilter2DEXT := dglGetProcAddress('glConvolutionFilter2DEXT');
  glConvolutionParameterfEXT := dglGetProcAddress('glConvolutionParameterfEXT');
  glConvolutionParameterfvEXT := dglGetProcAddress('glConvolutionParameterfvEXT');
  glConvolutionParameteriEXT := dglGetProcAddress('glConvolutionParameteriEXT');
  glConvolutionParameterivEXT := dglGetProcAddress('glConvolutionParameterivEXT');
  glCopyConvolutionFilter1DEXT := dglGetProcAddress('glCopyConvolutionFilter1DEXT');
  glCopyConvolutionFilter2DEXT := dglGetProcAddress('glCopyConvolutionFilter2DEXT');
  glGetConvolutionFilterEXT := dglGetProcAddress('glGetConvolutionFilterEXT');
  glGetConvolutionParameterfvEXT := dglGetProcAddress('glGetConvolutionParameterfvEXT');
  glGetConvolutionParameterivEXT := dglGetProcAddress('glGetConvolutionParameterivEXT');
  glGetSeparableFilterEXT := dglGetProcAddress('glGetSeparableFilterEXT');
  glSeparableFilter2DEXT := dglGetProcAddress('glSeparableFilter2DEXT');
end;

procedure Read_GL_EXT_coordinate_frame;
begin
  glTangent3bEXT := dglGetProcAddress('glTangent3bEXT');
  glTangent3bvEXT := dglGetProcAddress('glTangent3bvEXT');
  glTangent3dEXT := dglGetProcAddress('glTangent3dEXT');
  glTangent3dvEXT := dglGetProcAddress('glTangent3dvEXT');
  glTangent3fEXT := dglGetProcAddress('glTangent3fEXT');
  glTangent3fvEXT := dglGetProcAddress('glTangent3fvEXT');
  glTangent3iEXT := dglGetProcAddress('glTangent3iEXT');
  glTangent3ivEXT := dglGetProcAddress('glTangent3ivEXT');
  glTangent3sEXT := dglGetProcAddress('glTangent3sEXT');
  glTangent3svEXT := dglGetProcAddress('glTangent3svEXT');
  glBinormal3bEXT := dglGetProcAddress('glBinormal3bEXT');
  glBinormal3bvEXT := dglGetProcAddress('glBinormal3bvEXT');
  glBinormal3dEXT := dglGetProcAddress('glBinormal3dEXT');
  glBinormal3dvEXT := dglGetProcAddress('glBinormal3dvEXT');
  glBinormal3fEXT := dglGetProcAddress('glBinormal3fEXT');
  glBinormal3fvEXT := dglGetProcAddress('glBinormal3fvEXT');
  glBinormal3iEXT := dglGetProcAddress('glBinormal3iEXT');
  glBinormal3ivEXT := dglGetProcAddress('glBinormal3ivEXT');
  glBinormal3sEXT := dglGetProcAddress('glBinormal3sEXT');
  glBinormal3svEXT := dglGetProcAddress('glBinormal3svEXT');
  glTangentPointerEXT := dglGetProcAddress('glTangentPointerEXT');
  glBinormalPointerEXT := dglGetProcAddress('glBinormalPointerEXT');
end;

procedure Read_GL_EXT_copy_texture;
begin
  glCopyTexImage1DEXT := dglGetProcAddress('glCopyTexImage1DEXT');
  glCopyTexImage2DEXT := dglGetProcAddress('glCopyTexImage2DEXT');
  glCopyTexSubImage1DEXT := dglGetProcAddress('glCopyTexSubImage1DEXT');
  glCopyTexSubImage2DEXT := dglGetProcAddress('glCopyTexSubImage2DEXT');
  glCopyTexSubImage3DEXT := dglGetProcAddress('glCopyTexSubImage3DEXT');
end;

procedure Read_GL_EXT_cull_vertex;
begin
  glCullParameterdvEXT := dglGetProcAddress('glCullParameterdvEXT');
  glCullParameterfvEXT := dglGetProcAddress('glCullParameterfvEXT');
end;

procedure Read_GL_EXT_draw_range_elements;
begin
  glDrawRangeElementsEXT := dglGetProcAddress('glDrawRangeElementsEXT');
end;

procedure Read_GL_EXT_fog_coord;
begin
  glFogCoordfEXT := dglGetProcAddress('glFogCoordfEXT');
  glFogCoordfvEXT := dglGetProcAddress('glFogCoordfvEXT');
  glFogCoorddEXT := dglGetProcAddress('glFogCoorddEXT');
  glFogCoorddvEXT := dglGetProcAddress('glFogCoorddvEXT');
  glFogCoordPointerEXT := dglGetProcAddress('glFogCoordPointerEXT');
end;

procedure Read_GL_EXT_framebuffer_object;
begin
  glIsRenderbufferEXT := dglGetProcAddress('glIsRenderbufferEXT');
  glBindRenderbufferEXT := dglGetProcAddress('glBindRenderbufferEXT');
  glDeleteRenderbuffersEXT := dglGetProcAddress('glDeleteRenderbuffersEXT');
  glGenRenderbuffersEXT := dglGetProcAddress('glGenRenderbuffersEXT');
  glRenderbufferStorageEXT := dglGetProcAddress('glRenderbufferStorageEXT');
  glGetRenderbufferParameterivEXT := dglGetProcAddress('glGetRenderbufferParameterivEXT');
  glIsFramebufferEXT := dglGetProcAddress('glIsFramebufferEXT');
  glBindFramebufferEXT := dglGetProcAddress('glBindFramebufferEXT');
  glDeleteFramebuffersEXT := dglGetProcAddress('glDeleteFramebuffersEXT');
  glGenFramebuffersEXT := dglGetProcAddress('glGenFramebuffersEXT');
  glCheckFramebufferStatusEXT := dglGetProcAddress('glCheckFramebufferStatusEXT');
  glFramebufferTexture1DEXT := dglGetProcAddress('glFramebufferTexture1DEXT');
  glFramebufferTexture2DEXT := dglGetProcAddress('glFramebufferTexture2DEXT');
  glFramebufferTexture3DEXT := dglGetProcAddress('glFramebufferTexture3DEXT');
  glFramebufferRenderbufferEXT := dglGetProcAddress('glFramebufferRenderbufferEXT');
  glGetFramebufferAttachmentParameterivEXT := dglGetProcAddress('glGetFramebufferAttachmentParameterivEXT');
  glGenerateMipmapEXT := dglGetProcAddress('glGenerateMipmapEXT');
end;

procedure Read_GL_EXT_histogram;
begin
  glGetHistogramEXT := dglGetProcAddress('glGetHistogramEXT');
  glGetHistogramParameterfvEXT := dglGetProcAddress('glGetHistogramParameterfvEXT');
  glGetHistogramParameterivEXT := dglGetProcAddress('glGetHistogramParameterivEXT');
  glGetMinmaxEXT := dglGetProcAddress('glGetMinmaxEXT');
  glGetMinmaxParameterfvEXT := dglGetProcAddress('glGetMinmaxParameterfvEXT');
  glGetMinmaxParameterivEXT := dglGetProcAddress('glGetMinmaxParameterivEXT');
  glHistogramEXT := dglGetProcAddress('glHistogramEXT');
  glMinmaxEXT := dglGetProcAddress('glMinmaxEXT');
  glResetHistogramEXT := dglGetProcAddress('glResetHistogramEXT');
  glResetMinmaxEXT := dglGetProcAddress('glResetMinmaxEXT');
end;

procedure Read_GL_EXT_index_func;
begin
  glIndexFuncEXT := dglGetProcAddress('glIndexFuncEXT');
end;

procedure Read_GL_EXT_index_material;
begin
  glIndexMaterialEXT := dglGetProcAddress('glIndexMaterialEXT');
end;

procedure Read_GL_EXT_light_texture;
begin
  glApplyTextureEXT := dglGetProcAddress('glApplyTextureEXT');
  glTextureLightEXT := dglGetProcAddress('glTextureLightEXT');
  glTextureMaterialEXT := dglGetProcAddress('glTextureMaterialEXT');
end;

procedure Read_GL_EXT_multi_draw_arrays;
begin
  glMultiDrawArraysEXT := dglGetProcAddress('glMultiDrawArraysEXT');
  glMultiDrawElementsEXT := dglGetProcAddress('glMultiDrawElementsEXT');
end;

procedure Read_GL_EXT_multisample;
begin
  glSampleMaskEXT := dglGetProcAddress('glSampleMaskEXT');
  glSamplePatternEXT := dglGetProcAddress('glSamplePatternEXT');
end;

procedure Read_GL_EXT_paletted_texture;
begin
  glColorTableEXT := dglGetProcAddress('glColorTableEXT');
  glGetColorTableEXT := dglGetProcAddress('glGetColorTableEXT');
  glGetColorTableParameterivEXT := dglGetProcAddress('glGetColorTableParameterivEXT');
  glGetColorTableParameterfvEXT := dglGetProcAddress('glGetColorTableParameterfvEXT');
end;

procedure Read_GL_EXT_pixel_transform;
begin
  glPixelTransformParameteriEXT := dglGetProcAddress('glPixelTransformParameteriEXT');
  glPixelTransformParameterfEXT := dglGetProcAddress('glPixelTransformParameterfEXT');
  glPixelTransformParameterivEXT := dglGetProcAddress('glPixelTransformParameterivEXT');
  glPixelTransformParameterfvEXT := dglGetProcAddress('glPixelTransformParameterfvEXT');
end;

procedure Read_GL_EXT_point_parameters;
begin
  glPointParameterfEXT := dglGetProcAddress('glPointParameterfEXT');
  glPointParameterfvEXT := dglGetProcAddress('glPointParameterfvEXT');
end;

procedure Read_GL_EXT_polygon_offset;
begin
  glPolygonOffsetEXT := dglGetProcAddress('glPolygonOffsetEXT');
end;

procedure Read_GL_EXT_secondary_color;
begin
  glSecondaryColor3bEXT := dglGetProcAddress('glSecondaryColor3bEXT');
  glSecondaryColor3bvEXT := dglGetProcAddress('glSecondaryColor3bvEXT');
  glSecondaryColor3dEXT := dglGetProcAddress('glSecondaryColor3dEXT');
  glSecondaryColor3dvEXT := dglGetProcAddress('glSecondaryColor3dvEXT');
  glSecondaryColor3fEXT := dglGetProcAddress('glSecondaryColor3fEXT');
  glSecondaryColor3fvEXT := dglGetProcAddress('glSecondaryColor3fvEXT');
  glSecondaryColor3iEXT := dglGetProcAddress('glSecondaryColor3iEXT');
  glSecondaryColor3ivEXT := dglGetProcAddress('glSecondaryColor3ivEXT');
  glSecondaryColor3sEXT := dglGetProcAddress('glSecondaryColor3sEXT');
  glSecondaryColor3svEXT := dglGetProcAddress('glSecondaryColor3svEXT');
  glSecondaryColor3ubEXT := dglGetProcAddress('glSecondaryColor3ubEXT');
  glSecondaryColor3ubvEXT := dglGetProcAddress('glSecondaryColor3ubvEXT');
  glSecondaryColor3uiEXT := dglGetProcAddress('glSecondaryColor3uiEXT');
  glSecondaryColor3uivEXT := dglGetProcAddress('glSecondaryColor3uivEXT');
  glSecondaryColor3usEXT := dglGetProcAddress('glSecondaryColor3usEXT');
  glSecondaryColor3usvEXT := dglGetProcAddress('glSecondaryColor3usvEXT');
  glSecondaryColorPointerEXT := dglGetProcAddress('glSecondaryColorPointerEXT');
end;

procedure Read_GL_EXT_stencil_two_side;
begin
  glActiveStencilFaceEXT := dglGetProcAddress('glActiveStencilFaceEXT');
end;

procedure Read_GL_EXT_subtexture;
begin
  glTexSubImage1DEXT := dglGetProcAddress('glTexSubImage1DEXT');
  glTexSubImage2DEXT := dglGetProcAddress('glTexSubImage2DEXT');
end;

procedure Read_GL_EXT_texture3D;
begin
  glTexImage3DEXT := dglGetProcAddress('glTexImage3DEXT');
  glTexSubImage3DEXT := dglGetProcAddress('glTexSubImage3DEXT');
end;

procedure Read_GL_EXT_texture_object;
begin
  glAreTexturesResidentEXT := dglGetProcAddress('glAreTexturesResidentEXT');
  glBindTextureEXT := dglGetProcAddress('glBindTextureEXT');
  glDeleteTexturesEXT := dglGetProcAddress('glDeleteTexturesEXT');
  glGenTexturesEXT := dglGetProcAddress('glGenTexturesEXT');
  glIsTextureEXT := dglGetProcAddress('glIsTextureEXT');
  glPrioritizeTexturesEXT := dglGetProcAddress('glPrioritizeTexturesEXT');
end;

procedure Read_GL_EXT_texture_perturb_normal;
begin
  glTextureNormalEXT := dglGetProcAddress('glTextureNormalEXT');
end;

procedure Read_GL_EXT_vertex_array;
begin
  glArrayElementEXT := dglGetProcAddress('glArrayElementEXT');
  glColorPointerEXT := dglGetProcAddress('glColorPointerEXT');
  glDrawArraysEXT := dglGetProcAddress('glDrawArraysEXT');
  glEdgeFlagPointerEXT := dglGetProcAddress('glEdgeFlagPointerEXT');
  glGetPointervEXT := dglGetProcAddress('glGetPointervEXT');
  glIndexPointerEXT := dglGetProcAddress('glIndexPointerEXT');
  glNormalPointerEXT := dglGetProcAddress('glNormalPointerEXT');
  glTexCoordPointerEXT := dglGetProcAddress('glTexCoordPointerEXT');
  glVertexPointerEXT := dglGetProcAddress('glVertexPointerEXT');
end;

procedure Read_GL_EXT_vertex_shader;
begin
  glBeginVertexShaderEXT := dglGetProcAddress('glBeginVertexShaderEXT');
  glEndVertexShaderEXT := dglGetProcAddress('glEndVertexShaderEXT');
  glBindVertexShaderEXT := dglGetProcAddress('glBindVertexShaderEXT');
  glGenVertexShadersEXT := dglGetProcAddress('glGenVertexShadersEXT');
  glDeleteVertexShaderEXT := dglGetProcAddress('glDeleteVertexShaderEXT');
  glShaderOp1EXT := dglGetProcAddress('glShaderOp1EXT');
  glShaderOp2EXT := dglGetProcAddress('glShaderOp2EXT');
  glShaderOp3EXT := dglGetProcAddress('glShaderOp3EXT');
  glSwizzleEXT := dglGetProcAddress('glSwizzleEXT');
  glWriteMaskEXT := dglGetProcAddress('glWriteMaskEXT');
  glInsertComponentEXT := dglGetProcAddress('glInsertComponentEXT');
  glExtractComponentEXT := dglGetProcAddress('glExtractComponentEXT');
  glGenSymbolsEXT := dglGetProcAddress('glGenSymbolsEXT');
  glSetInvariantEXT := dglGetProcAddress('glSetInvariantEXT');
  glSetLocalConstantEXT := dglGetProcAddress('glSetLocalConstantEXT');
  glVariantbvEXT := dglGetProcAddress('glVariantbvEXT');
  glVariantsvEXT := dglGetProcAddress('glVariantsvEXT');
  glVariantivEXT := dglGetProcAddress('glVariantivEXT');
  glVariantfvEXT := dglGetProcAddress('glVariantfvEXT');
  glVariantdvEXT := dglGetProcAddress('glVariantdvEXT');
  glVariantubvEXT := dglGetProcAddress('glVariantubvEXT');
  glVariantusvEXT := dglGetProcAddress('glVariantusvEXT');
  glVariantuivEXT := dglGetProcAddress('glVariantuivEXT');
  glVariantPointerEXT := dglGetProcAddress('glVariantPointerEXT');
  glEnableVariantClientStateEXT := dglGetProcAddress('glEnableVariantClientStateEXT');
  glDisableVariantClientStateEXT := dglGetProcAddress('glDisableVariantClientStateEXT');
  glBindLightParameterEXT := dglGetProcAddress('glBindLightParameterEXT');
  glBindMaterialParameterEXT := dglGetProcAddress('glBindMaterialParameterEXT');
  glBindTexGenParameterEXT := dglGetProcAddress('glBindTexGenParameterEXT');
  glBindTextureUnitParameterEXT := dglGetProcAddress('glBindTextureUnitParameterEXT');
  glBindParameterEXT := dglGetProcAddress('glBindParameterEXT');
  glIsVariantEnabledEXT := dglGetProcAddress('glIsVariantEnabledEXT');
  glGetVariantBooleanvEXT := dglGetProcAddress('glGetVariantBooleanvEXT');
  glGetVariantIntegervEXT := dglGetProcAddress('glGetVariantIntegervEXT');
  glGetVariantFloatvEXT := dglGetProcAddress('glGetVariantFloatvEXT');
  glGetVariantPointervEXT := dglGetProcAddress('glGetVariantPointervEXT');
  glGetInvariantBooleanvEXT := dglGetProcAddress('glGetInvariantBooleanvEXT');
  glGetInvariantIntegervEXT := dglGetProcAddress('glGetInvariantIntegervEXT');
  glGetInvariantFloatvEXT := dglGetProcAddress('glGetInvariantFloatvEXT');
  glGetLocalConstantBooleanvEXT := dglGetProcAddress('glGetLocalConstantBooleanvEXT');
  glGetLocalConstantIntegervEXT := dglGetProcAddress('glGetLocalConstantIntegervEXT');
  glGetLocalConstantFloatvEXT := dglGetProcAddress('glGetLocalConstantFloatvEXT');
end;

procedure Read_GL_EXT_vertex_weighting;
begin
  glVertexWeightfEXT := dglGetProcAddress('glVertexWeightfEXT');
  glVertexWeightfvEXT := dglGetProcAddress('glVertexWeightfvEXT');
  glVertexWeightPointerEXT := dglGetProcAddress('glVertexWeightPointerEXT');
end;

procedure Read_GL_EXT_depth_bounds_test;
begin
  glImageTransformParameteriHP := dglGetProcAddress('glImageTransformParameteriHP');
  glDepthBoundsEXT := dglGetProcAddress('glDepthBoundsEXT');
end;

procedure Read_GL_EXT_blend_equation_separate;
begin
  glBlendEquationSeparateEXT := dglGetProcAddress('glBlendEquationSeparateEXT');
end;

procedure Read_GL_EXT_stencil_clear_tag;
begin
  glStencilClearTagEXT := dglGetProcAddress('glStencilClearTagEXT');
end;

procedure Read_GL_EXT_framebuffer_blit;
begin
  glBlitFramebufferEXT := dglGetProcAddress('glBlitFramebufferEXT');
end;

procedure Read_GL_EXT_framebuffer_multisample;
begin
  glRenderbufferStorageMultisampleEXT := dglGetProcAddress('glRenderbufferStorageMultisampleEXT');
end;

procedure Read_GL_EXT_timer_query;
begin
  glGetQueryObjecti64vEXT := dglGetProcAddress('glGetQueryObjecti64vEXT');
  glGetQueryObjectui64vEXT := dglGetProcAddress('glGetQueryObjectui64vEXT');
end;

procedure Read_GL_EXT_gpu_program_parameters;
begin
  glProgramEnvParameters4fvEXT := dglGetProcAddress('glProgramEnvParameters4fvEXT');
  glProgramLocalParameters4fvEXT := dglGetProcAddress('glProgramLocalParameters4fvEXT');
end;

procedure Read_GL_EXT_bindable_uniform;
begin
  glUniformBufferEXT := dglGetProcAddress('glUniformBufferEXT');
  glGetUniformBufferSizeEXT := dglGetProcAddress('glGetUniformBufferSizeEXT');
  glGetUniformOffsetEXT := dglGetProcAddress('glGetUniformOffsetEXT');
end;

procedure Read_GL_EXT_draw_buffers2;
begin
  glColorMaskIndexedEXT := dglGetProcAddress('glColorMaskIndexedEXT');
  glGetBooleanIndexedvEXT := dglGetProcAddress('glGetBooleanIndexedvEXT');
  glGetIntegerIndexedvEXT := dglGetProcAddress('glGetIntegerIndexedvEXT');
  glEnableIndexedEXT := dglGetProcAddress('glEnableIndexedEXT');
  glDisableIndexedEXT := dglGetProcAddress('glDisableIndexedEXT');
  glIsEnabledIndexedEXT := dglGetProcAddress('glIsEnabledIndexedEXT');
end;

procedure Read_GL_EXT_draw_instanced;
begin
  glDrawArraysInstancedEXT := dglGetProcAddress('glDrawArraysInstancedEXT');
  glDrawElementsInstancedEXT := dglGetProcAddress('glDrawElementsInstancedEXT');
end;

procedure Read_GL_EXT_geometry_shader4;
begin
  glProgramParameteriEXT := dglGetProcAddress('glProgramParameteriEXT');
  glFramebufferTextureEXT := dglGetProcAddress('glFramebufferTextureEXT');
//  glFramebufferTextureLayerEXT := dglGetProcAddress('glFramebufferTextureLayerEXT');
  glFramebufferTextureFaceEXT := dglGetProcAddress('glFramebufferTextureFaceEXT');
end;

procedure Read_GL_EXT_gpu_shader4;
begin
  glVertexAttribI1iEXT := dglGetProcAddress('glVertexAttribI1iEXT');
  glVertexAttribI2iEXT := dglGetProcAddress('glVertexAttribI2iEXT');
  glVertexAttribI3iEXT := dglGetProcAddress('glVertexAttribI3iEXT');
  glVertexAttribI4iEXT := dglGetProcAddress('glVertexAttribI4iEXT');
  glVertexAttribI1uiEXT := dglGetProcAddress('glVertexAttribI1uiEXT');
  glVertexAttribI2uiEXT := dglGetProcAddress('glVertexAttribI2uiEXT');
  glVertexAttribI3uiEXT := dglGetProcAddress('glVertexAttribI3uiEXT');
  glVertexAttribI4uiEXT := dglGetProcAddress('glVertexAttribI4uiEXT');
  glVertexAttribI1ivEXT := dglGetProcAddress('glVertexAttribI1ivEXT');
  glVertexAttribI2ivEXT := dglGetProcAddress('glVertexAttribI2ivEXT');
  glVertexAttribI3ivEXT := dglGetProcAddress('glVertexAttribI3ivEXT');
  glVertexAttribI4ivEXT := dglGetProcAddress('glVertexAttribI4ivEXT');
  glVertexAttribI1uivEXT := dglGetProcAddress('glVertexAttribI1uivEXT');
  glVertexAttribI2uivEXT := dglGetProcAddress('glVertexAttribI2uivEXT');
  glVertexAttribI3uivEXT := dglGetProcAddress('glVertexAttribI3uivEXT');
  glVertexAttribI4uivEXT := dglGetProcAddress('glVertexAttribI4uivEXT');
  glVertexAttribI4bvEXT := dglGetProcAddress('glVertexAttribI4bvEXT');
  glVertexAttribI4svEXT := dglGetProcAddress('glVertexAttribI4svEXT');
  glVertexAttribI4ubvEXT := dglGetProcAddress('glVertexAttribI4ubvEXT');
  glVertexAttribI4usvEXT := dglGetProcAddress('glVertexAttribI4usvEXT');
  glVertexAttribIPointerEXT := dglGetProcAddress('glVertexAttribIPointerEXT');
  glGetVertexAttribIivEXT := dglGetProcAddress('glGetVertexAttribIivEXT');
  glGetVertexAttribIuivEXT := dglGetProcAddress('glGetVertexAttribIuivEXT');
  glUniform1uiEXT := dglGetProcAddress('glUniform1uiEXT');
  glUniform2uiEXT := dglGetProcAddress('glUniform2uiEXT');
  glUniform3uiEXT := dglGetProcAddress('glUniform3uiEXT');
  glUniform4uiEXT := dglGetProcAddress('glUniform4uiEXT');
  glUniform1uivEXT := dglGetProcAddress('glUniform1uivEXT');
  glUniform2uivEXT := dglGetProcAddress('glUniform2uivEXT');
  glUniform3uivEXT := dglGetProcAddress('glUniform3uivEXT');
  glUniform4uivEXT := dglGetProcAddress('glUniform4uivEXT');
  glGetUniformuivEXT := dglGetProcAddress('glGetUniformuivEXT');
  glBindFragDataLocationEXT := dglGetProcAddress('glBindFragDataLocationEXT');
  glGetFragDataLocationEXT := dglGetProcAddress('glGetFragDataLocationEXT');
end;

procedure Read_GL_EXT_texture_array;
begin
  glFramebufferTextureLayerEXT := dglGetProcAddress('glFramebufferTextureLayerEXT');
end;

procedure Read_GL_EXT_texture_buffer_object;
begin
  glTexBufferEXT := dglGetProcAddress('glTexBufferEXT');
end;

procedure Read_GL_EXT_texture_integer;
begin
  glClearColorIiEXT := dglGetProcAddress('glClearColorIiEXT');
  glClearColorIuiEXT := dglGetProcAddress('glClearColorIuiEXT');
  glTexParameterIivEXT := dglGetProcAddress('glTexParameterIivEXT');
  glTexParameterIuivEXT := dglGetProcAddress('glTexParameterIuivEXT');
  glGetTexParameterIivEXT := dglGetProcAddress('glGetTexParameterIivEXT');
  glGetTexParameterIiuvEXT := dglGetProcAddress('glGetTexParameterIiuvEXT');
end;

procedure Read_GL_EXT_transform_feedback;
begin
  glBeginTransformFeedbackEXT := dglGetProcAddress('lBeginTransformFeedbackEXT');
  glEndTransformFeedbackEXT := dglGetProcAddress('glEndTransformFeedbackEXT');
  glBindBufferRangeEXT := dglGetProcAddress('glBindBufferRangeEXT');
  glBindBufferOffsetEXT := dglGetProcAddress('glBindBufferOffsetEXT');
  glBindBufferBaseEXT := dglGetProcAddress('glBindBufferBaseEXT');
  glTransformFeedbackVaryingsEXT := dglGetProcAddress('glTransformFeedbackVaryingsEXT');
  glGetTransformFeedbackVaryingEXT := dglGetProcAddress('glGetTransformFeedbackVaryingEXT');
end;

procedure Read_GL_EXT_direct_state_access;
begin
  glClientAttribDefaultEXT := dglGetProcAddress('glClientAttribDefaultEXT');
  glPushClientAttribDefaultEXT := dglGetProcAddress('glPushClientAttribDefaultEXT');
  glMatrixLoadfEXT := dglGetProcAddress('glMatrixLoadfEXT');
  glMatrixLoaddEXT := dglGetProcAddress('glMatrixLoaddEXT');
  glMatrixMultfEXT := dglGetProcAddress('glMatrixMultfEXT');
  glMatrixMultdEXT := dglGetProcAddress('glMatrixMultdEXT');
  glMatrixLoadIdentityEXT := dglGetProcAddress('glMatrixLoadIdentityEXT');
  glMatrixRotatefEXT := dglGetProcAddress('glMatrixRotatefEXT');
  glMatrixRotatedEXT := dglGetProcAddress('glMatrixRotatedEXT');
  glMatrixScalefEXT := dglGetProcAddress('glMatrixScalefEXT');
  glMatrixScaledEXT := dglGetProcAddress('glMatrixScaledEXT');
  glMatrixTranslatefEXT := dglGetProcAddress('glMatrixTranslatefEXT');
  glMatrixTranslatedEXT := dglGetProcAddress('glMatrixTranslatedEXT');
  glMatrixFrustumEXT := dglGetProcAddress('glMatrixFrustumEXT');
  glMatrixOrthoEXT := dglGetProcAddress('glMatrixOrthoEXT');
  glMatrixPopEXT := dglGetProcAddress('glMatrixPopEXT');
  glMatrixPushEXT := dglGetProcAddress('glMatrixPushEXT');
  glMatrixLoadTransposefEXT := dglGetProcAddress('glMatrixLoadTransposefEXT');
  glMatrixLoadTransposedEXT := dglGetProcAddress('glMatrixLoadTransposedEXT');
  glMatrixMultTransposefEXT := dglGetProcAddress('glMatrixMultTransposefEXT');
  glMatrixMultTransposedEXT := dglGetProcAddress('glMatrixMultTransposedEXT');
  glTextureParameterfEXT := dglGetProcAddress('glTextureParameterfEXT');
  glTextureParameterfvEXT := dglGetProcAddress('glTextureParameterfvEXT');
  glTextureParameteriEXT := dglGetProcAddress('glTextureParameteriEXT');
  glTextureParameterivEXT := dglGetProcAddress('glTextureParameterivEXT');
  glTextureImage1DEXT := dglGetProcAddress('glTextureImage1DEXT');
  glTextureImage2DEXT := dglGetProcAddress('glTextureImage2DEXT');
  glTextureSubImage1DEXT := dglGetProcAddress('glTextureSubImage1DEXT');
  glTextureSubImage2DEXT := dglGetProcAddress('glTextureSubImage2DEXT');
  glCopyTextureImage1DEXT := dglGetProcAddress('glCopyTextureImage1DEXT');
  glCopyTextureImage2DEXT := dglGetProcAddress('glCopyTextureImage2DEXT');
  glCopyTextureSubImage1DEXT := dglGetProcAddress('glCopyTextureSubImage1DEXT');
  glCopyTextureSubImage2DEXT := dglGetProcAddress('glCopyTextureSubImage2DEXT');
  glGetTextureImageEXT := dglGetProcAddress('glGetTextureImageEXT');
  glGetTextureParameterfvEXT := dglGetProcAddress('glGetTextureParameterfvEXT');
  glGetTextureParameterivEXT := dglGetProcAddress('glGetTextureParameterivEXT');
  glGetTextureLevelParameterfvEXT := dglGetProcAddress('glGetTextureLevelParameterfvEXT');
  glGetTextureLevelParameterivEXT := dglGetProcAddress('glGetTextureLevelParameterivEXT');
  glTextureImage3DEXT := dglGetProcAddress('glTextureImage3DEXT');
  glTextureSubImage3DEXT := dglGetProcAddress('glTextureSubImage3DEXT');
  glCopyTextureSubImage3DEXT := dglGetProcAddress('glCopyTextureSubImage3DEXT');
  glMultiTexParameterfEXT := dglGetProcAddress('glMultiTexParameterfEXT');
  glMultiTexParameterfvEXT := dglGetProcAddress('glMultiTexParameterfvEXT');
  glMultiTexParameteriEXT := dglGetProcAddress('glMultiTexParameteriEXT');
  glMultiTexParameterivEXT := dglGetProcAddress('glMultiTexParameterivEXT');
  glMultiTexImage1DEXT := dglGetProcAddress('glMultiTexImage1DEXT');
  glMultiTexImage2DEXT := dglGetProcAddress('glMultiTexImage2DEXT');
  glMultiTexSubImage1DEXT := dglGetProcAddress('glMultiTexSubImage1DEXT');
  glMultiTexSubImage2DEXT := dglGetProcAddress('glMultiTexSubImage2DEXT');
  glCopyMultiTexImage1DEXT := dglGetProcAddress('glCopyMultiTexImage1DEXT');
  glCopyMultiTexImage2DEXT := dglGetProcAddress('glCopyMultiTexImage2DEXT');
  glCopyMultiTexSubImage1DEXT := dglGetProcAddress('glCopyMultiTexSubImage1DEXT');
  glCopyMultiTexSubImage2DEXT := dglGetProcAddress('glCopyMultiTexSubImage2DEXT');
  glGetMultiTexImageEXT := dglGetProcAddress('glGetMultiTexImageEXT');
  glGetMultiTexParameterfvEXT := dglGetProcAddress('glGetMultiTexParameterfvEXT');
  glGetMultiTexParameterivEXT := dglGetProcAddress('glGetMultiTexParameterivEXT');
  glGetMultiTexLevelParameterfvEXT := dglGetProcAddress('glGetMultiTexLevelParameterfvEXT');
  glGetMultiTexLevelParameterivEXT := dglGetProcAddress('glGetMultiTexLevelParameterivEXT');
  glMultiTexImage3DEXT := dglGetProcAddress('glMultiTexImage3DEXT');
  glMultiTexSubImage3DEXT := dglGetProcAddress('glMultiTexSubImage3DEXT');
  glCopyMultiTexSubImage3DEXT := dglGetProcAddress('glCopyMultiTexSubImage3DEXT');
  glBindMultiTextureEXT := dglGetProcAddress('glBindMultiTextureEXT');
  glEnableClientStateIndexedEXT := dglGetProcAddress('glEnableClientStateIndexedEXT');
  glDisableClientStateIndexedEXT := dglGetProcAddress('glDisableClientStateIndexedEXT');
  glMultiTexCoordPointerEXT := dglGetProcAddress('glMultiTexCoordPointerEXT');
  glMultiTexEnvfEXT := dglGetProcAddress('glMultiTexEnvfEXT');
  glMultiTexEnvfvEXT := dglGetProcAddress('glMultiTexEnvfvEXT');
  glMultiTexEnviEXT := dglGetProcAddress('glMultiTexEnviEXT');
  glMultiTexEnvivEXT := dglGetProcAddress('glMultiTexEnvivEXT');
  glMultiTexGendEXT := dglGetProcAddress('glMultiTexGendEXT');
  glMultiTexGendvEXT := dglGetProcAddress('glMultiTexGendvEXT');
  glMultiTexGenfEXT := dglGetProcAddress('glMultiTexGenfEXT');
  glMultiTexGenfvEXT := dglGetProcAddress('glMultiTexGenfvEXT');
  glMultiTexGeniEXT := dglGetProcAddress('glMultiTexGeniEXT');
  glMultiTexGenivEXT := dglGetProcAddress('glMultiTexGenivEXT');
  glGetMultiTexEnvfvEXT := dglGetProcAddress('glGetMultiTexEnvfvEXT');
  glGetMultiTexEnvivEXT := dglGetProcAddress('glGetMultiTexEnvivEXT');
  glGetMultiTexGendvEXT := dglGetProcAddress('glGetMultiTexGendvEXT');
  glGetMultiTexGenfvEXT := dglGetProcAddress('glGetMultiTexGenfvEXT');
  glGetMultiTexGenivEXT := dglGetProcAddress('glGetMultiTexGenivEXT');
  glGetFloatIndexedvEXT := dglGetProcAddress('glGetFloatIndexedvEXT');
  glGetDoubleIndexedvEXT := dglGetProcAddress('glGetDoubleIndexedvEXT');
  glGetPointerIndexedvEXT := dglGetProcAddress('glGetPointerIndexedvEXT');
  glCompressedTextureImage3DEXT := dglGetProcAddress('glCompressedTextureImage3DEXT');
  glCompressedTextureImage2DEXT := dglGetProcAddress('glCompressedTextureImage2DEXT');
  glCompressedTextureImage1DEXT := dglGetProcAddress('glCompressedTextureImage1DEXT');
  glCompressedTextureSubImage3DEXT := dglGetProcAddress('glCompressedTextureSubImage3DEXT');
  glCompressedTextureSubImage2DEXT := dglGetProcAddress('glCompressedTextureSubImage2DEXT');
  glCompressedTextureSubImage1DEXT := dglGetProcAddress('glCompressedTextureSubImage1DEXT');
  glGetCompressedTextureImageEXT := dglGetProcAddress('glGetCompressedTextureImageEXT');
  glCompressedMultiTexImage3DEXT := dglGetProcAddress('glCompressedMultiTexImage3DEXT');
  glCompressedMultiTexImage2DEXT := dglGetProcAddress('glCompressedMultiTexImage2DEXT');
  glCompressedMultiTexImage1DEXT := dglGetProcAddress('glCompressedMultiTexImage1DEXT');
  glCompressedMultiTexSubImage3DEXT := dglGetProcAddress('glCompressedMultiTexSubImage3DEXT');
  glCompressedMultiTexSubImage2DEXT := dglGetProcAddress('glCompressedMultiTexSubImage2DEXT');
  glCompressedMultiTexSubImage1DEXT := dglGetProcAddress('glCompressedMultiTexSubImage1DEXT');
  glGetCompressedMultiTexImageEXT := dglGetProcAddress('glGetCompressedMultiTexImageEXT');
  glNamedProgramStringEXT := dglGetProcAddress('glNamedProgramStringEXT');
  glNamedProgramLocalParameter4dEXT := dglGetProcAddress('glNamedProgramLocalParameter4dEXT');
  glNamedProgramLocalParameter4dvEXT := dglGetProcAddress('glNamedProgramLocalParameter4dvEXT');
  glNamedProgramLocalParameter4fEXT := dglGetProcAddress('glNamedProgramLocalParameter4fEXT');
  glNamedProgramLocalParameter4fvEXT := dglGetProcAddress('glNamedProgramLocalParameter4fvEXT');
  glGetNamedProgramLocalParameterdvEXT := dglGetProcAddress('glGetNamedProgramLocalParameterdvEXT');
  glGetNamedProgramLocalParameterfvEXT := dglGetProcAddress('glGetNamedProgramLocalParameterfvEXT');
  glGetNamedProgramivEXT := dglGetProcAddress('glGetNamedProgramivEXT');
  glGetNamedProgramStringEXT := dglGetProcAddress('glGetNamedProgramStringEXT');
  glNamedProgramLocalParameters4fvEXT := dglGetProcAddress('glNamedProgramLocalParameters4fvEXT');
  glNamedProgramLocalParameterI4iEXT := dglGetProcAddress('glNamedProgramLocalParameterI4iEXT');
  glNamedProgramLocalParameterI4ivEXT := dglGetProcAddress('glNamedProgramLocalParameterI4ivEXT');
  glNamedProgramLocalParametersI4ivEXT := dglGetProcAddress('glNamedProgramLocalParametersI4ivEXT');
  glNamedProgramLocalParameterI4uiEXT := dglGetProcAddress('glNamedProgramLocalParameterI4uiEXT');
  glNamedProgramLocalParameterI4uivEXT := dglGetProcAddress('glNamedProgramLocalParameterI4uivEXT');
  glNamedProgramLocalParametersI4uivEXT := dglGetProcAddress('glNamedProgramLocalParametersI4uivEXT');
  glGetNamedProgramLocalParameterIivEXT := dglGetProcAddress('glGetNamedProgramLocalParameterIivEXT');
  glGetNamedProgramLocalParameterIuivEXT := dglGetProcAddress('glGetNamedProgramLocalParameterIuivEXT');
  glTextureParameterIivEXT := dglGetProcAddress('glTextureParameterIivEXT');
  glTextureParameterIuivEXT := dglGetProcAddress('glTextureParameterIuivEXT');
  glGetTextureParameterIivEXT := dglGetProcAddress('glGetTextureParameterIivEXT');
  glGetTextureParameterIuivEXT := dglGetProcAddress('glGetTextureParameterIuivEXT');
  glMultiTexParameterIivEXT := dglGetProcAddress('glMultiTexParameterIivEXT');
  glMultiTexParameterIuivEXT := dglGetProcAddress('glMultiTexParameterIuivEXT');
  glGetMultiTexParameterIivEXT := dglGetProcAddress('glGetMultiTexParameterIivEXT');
  glGetMultiTexParameterIuivEXT := dglGetProcAddress('glGetMultiTexParameterIuivEXT');
  glProgramUniform1fEXT := dglGetProcAddress('glProgramUniform1fEXT');
  glProgramUniform2fEXT := dglGetProcAddress('glProgramUniform2fEXT');
  glProgramUniform3fEXT := dglGetProcAddress('glProgramUniform3fEXT');
  glProgramUniform4fEXT := dglGetProcAddress('glProgramUniform4fEXT');
  glProgramUniform1iEXT := dglGetProcAddress('glProgramUniform1iEXT');
  glProgramUniform2iEXT := dglGetProcAddress('glProgramUniform2iEXT');
  glProgramUniform3iEXT := dglGetProcAddress('glProgramUniform3iEXT');
  glProgramUniform4iEXT := dglGetProcAddress('glProgramUniform4iEXT');
  glProgramUniform1fvEXT := dglGetProcAddress('glProgramUniform1fvEXT');
  glProgramUniform2fvEXT := dglGetProcAddress('glProgramUniform2fvEXT');
  glProgramUniform3fvEXT := dglGetProcAddress('glProgramUniform3fvEXT');
  glProgramUniform4fvEXT := dglGetProcAddress('glProgramUniform4fvEXT');
  glProgramUniform1ivEXT := dglGetProcAddress('glProgramUniform1ivEXT');
  glProgramUniform2ivEXT := dglGetProcAddress('glProgramUniform2ivEXT');
  glProgramUniform3ivEXT := dglGetProcAddress('glProgramUniform3ivEXT');
  glProgramUniform4ivEXT := dglGetProcAddress('glProgramUniform4ivEXT');
  glProgramUniformMatrix2fvEXT := dglGetProcAddress('glProgramUniformMatrix2fvEXT');
  glProgramUniformMatrix3fvEXT := dglGetProcAddress('glProgramUniformMatrix3fvEXT');
  glProgramUniformMatrix4fvEXT := dglGetProcAddress('glProgramUniformMatrix4fvEXT');
  glProgramUniformMatrix2x3fvEXT := dglGetProcAddress('glProgramUniformMatrix2x3fvEXT');
  glProgramUniformMatrix3x2fvEXT := dglGetProcAddress('glProgramUniformMatrix3x2fvEXT');
  glProgramUniformMatrix2x4fvEXT := dglGetProcAddress('glProgramUniformMatrix2x4fvEXT');
  glProgramUniformMatrix4x2fvEXT := dglGetProcAddress('glProgramUniformMatrix4x2fvEXT');
  glProgramUniformMatrix3x4fvEXT := dglGetProcAddress('glProgramUniformMatrix3x4fvEXT');
  glProgramUniformMatrix4x3fvEXT := dglGetProcAddress('glProgramUniformMatrix4x3fvEXT');
  glProgramUniform1uiEXT := dglGetProcAddress('glProgramUniform1uiEXT');
  glProgramUniform2uiEXT := dglGetProcAddress('glProgramUniform2uiEXT');
  glProgramUniform3uiEXT := dglGetProcAddress('glProgramUniform3uiEXT');
  glProgramUniform4uiEXT := dglGetProcAddress('glProgramUniform4uiEXT');
  glProgramUniform1uivEXT := dglGetProcAddress('glProgramUniform1uivEXT');
  glProgramUniform2uivEXT := dglGetProcAddress('glProgramUniform2uivEXT');
  glProgramUniform3uivEXT := dglGetProcAddress('glProgramUniform3uivEXT');
  glProgramUniform4uivEXT := dglGetProcAddress('glProgramUniform4uivEXT');
  glNamedBufferDataEXT := dglGetProcAddress('glNamedBufferDataEXT');
  glNamedBufferSubDataEXT := dglGetProcAddress('glNamedBufferSubDataEXT');
  glMapNamedBufferEXT := dglGetProcAddress('glMapNamedBufferEXT');
  glUnmapNamedBufferEXT := dglGetProcAddress('glUnmapNamedBufferEXT');
  glGetNamedBufferParameterivEXT := dglGetProcAddress('glGetNamedBufferParameterivEXT');
  glGetNamedBufferPointervEXT := dglGetProcAddress('glGetNamedBufferPointervEXT');
  glGetNamedBufferSubDataEXT := dglGetProcAddress('glGetNamedBufferSubDataEXT');
  glTextureBufferEXT := dglGetProcAddress('glTextureBufferEXT');
  glMultiTexBufferEXT := dglGetProcAddress('glMultiTexBufferEXT');
  glNamedRenderbufferStorageEXT := dglGetProcAddress('glNamedRenderbufferStorageEXT');
  glGetNamedRenderbufferParameterivEXT := dglGetProcAddress('glGetNamedRenderbufferParameterivEXT');
  glCheckNamedFramebufferStatusEXT := dglGetProcAddress('glCheckNamedFramebufferStatusEXT');
  glNamedFramebufferTexture1DEXT := dglGetProcAddress('glNamedFramebufferTexture1DEXT');
  glNamedFramebufferTexture2DEXT := dglGetProcAddress('glNamedFramebufferTexture2DEXT');
  glNamedFramebufferTexture3DEXT := dglGetProcAddress('glNamedFramebufferTexture3DEXT');
  glNamedFramebufferRenderbufferEXT := dglGetProcAddress('glNamedFramebufferRenderbufferEXT');
  glGetNamedFramebufferAttachmentParameterivEXT := dglGetProcAddress('glGetNamedFramebufferAttachmentParameterivEXT');
  glGenerateTextureMipmapEXT := dglGetProcAddress('glGenerateTextureMipmapEXT');
  glGenerateMultiTexMipmapEXT := dglGetProcAddress('glGenerateMultiTexMipmapEXT');
  glFramebufferDrawBufferEXT := dglGetProcAddress('glFramebufferDrawBufferEXT');
  glFramebufferDrawBuffersEXT := dglGetProcAddress('glFramebufferDrawBuffersEXT');
  glFramebufferReadBufferEXT := dglGetProcAddress('glFramebufferReadBufferEXT');
  glGetFramebufferParameterivEXT := dglGetProcAddress('glGetFramebufferParameterivEXT');
  glNamedRenderbufferStorageMultisampleEXT := dglGetProcAddress('glNamedRenderbufferStorageMultisampleEXT');
  glNamedRenderbufferStorageMultisampleCoverageEXT := dglGetProcAddress('glNamedRenderbufferStorageMultisampleCoverageEXT');
  glNamedFramebufferTextureEXT := dglGetProcAddress('glNamedFramebufferTextureEXT');
  glNamedFramebufferTextureLayerEXT := dglGetProcAddress('glNamedFramebufferTextureLayerEXT');
  glNamedFramebufferTextureFaceEXT := dglGetProcAddress('glNamedFramebufferTextureFaceEXT');
  glTextureRenderbufferEXT := dglGetProcAddress('glTextureRenderbufferEXT');
  glMultiTexRenderbufferEXT := dglGetProcAddress('glMultiTexRenderbufferEXT');
end;

procedure Read_GL_HP_image_transform;
begin
  glImageTransformParameteriHP := dglGetProcAddress('glImageTransformParameteriHP');
  glImageTransformParameterfHP := dglGetProcAddress('glImageTransformParameterfHP');
  glImageTransformParameterivHP := dglGetProcAddress('glImageTransformParameterivHP');
  glImageTransformParameterfvHP := dglGetProcAddress('glImageTransformParameterfvHP');
  glGetImageTransformParameterivHP := dglGetProcAddress('glGetImageTransformParameterivHP');
  glGetImageTransformParameterfvHP := dglGetProcAddress('glGetImageTransformParameterfvHP');
end;

procedure Read_GL_IBM_multimode_draw_arrays;
begin
  glMultiModeDrawArraysIBM := dglGetProcAddress('glMultiModeDrawArraysIBM');
  glMultiModeDrawElementsIBM := dglGetProcAddress('glMultiModeDrawElementsIBM');
end;

procedure Read_GL_IBM_vertex_array_lists;
begin
  glColorPointerListIBM := dglGetProcAddress('glColorPointerListIBM');
  glSecondaryColorPointerListIBM := dglGetProcAddress('glSecondaryColorPointerListIBM');
  glEdgeFlagPointerListIBM := dglGetProcAddress('glEdgeFlagPointerListIBM');
  glFogCoordPointerListIBM := dglGetProcAddress('glFogCoordPointerListIBM');
  glIndexPointerListIBM := dglGetProcAddress('glIndexPointerListIBM');
  glNormalPointerListIBM := dglGetProcAddress('glNormalPointerListIBM');
  glTexCoordPointerListIBM := dglGetProcAddress('glTexCoordPointerListIBM');
  glVertexPointerListIBM := dglGetProcAddress('glVertexPointerListIBM');
end;

procedure Read_GL_INGR_blend_func_separate;
begin
  glBlendFuncSeparateINGR := dglGetProcAddress('glBlendFuncSeparateINGR');
end;

procedure Read_GL_INTEL_parallel_arrays;
begin
  glVertexPointervINTEL := dglGetProcAddress('glVertexPointervINTEL');
  glNormalPointervINTEL := dglGetProcAddress('glNormalPointervINTEL');
  glColorPointervINTEL := dglGetProcAddress('glColorPointervINTEL');
  glTexCoordPointervINTEL := dglGetProcAddress('glTexCoordPointervINTEL');
end;

procedure Read_GL_MESA_resize_buffers;
begin
  glResizeBuffersMESA := dglGetProcAddress('glResizeBuffersMESA');
end;

procedure Read_GL_MESA_window_pos;
begin
  glWindowPos2dMESA := dglGetProcAddress('glWindowPos2dMESA');
  glWindowPos2dvMESA := dglGetProcAddress('glWindowPos2dvMESA');
  glWindowPos2fMESA := dglGetProcAddress('glWindowPos2fMESA');
  glWindowPos2fvMESA := dglGetProcAddress('glWindowPos2fvMESA');
  glWindowPos2iMESA := dglGetProcAddress('glWindowPos2iMESA');
  glWindowPos2ivMESA := dglGetProcAddress('glWindowPos2ivMESA');
  glWindowPos2sMESA := dglGetProcAddress('glWindowPos2sMESA');
  glWindowPos2svMESA := dglGetProcAddress('glWindowPos2svMESA');
  glWindowPos3dMESA := dglGetProcAddress('glWindowPos3dMESA');
  glWindowPos3dvMESA := dglGetProcAddress('glWindowPos3dvMESA');
  glWindowPos3fMESA := dglGetProcAddress('glWindowPos3fMESA');
  glWindowPos3fvMESA := dglGetProcAddress('glWindowPos3fvMESA');
  glWindowPos3iMESA := dglGetProcAddress('glWindowPos3iMESA');
  glWindowPos3ivMESA := dglGetProcAddress('glWindowPos3ivMESA');
  glWindowPos3sMESA := dglGetProcAddress('glWindowPos3sMESA');
  glWindowPos3svMESA := dglGetProcAddress('glWindowPos3svMESA');
  glWindowPos4dMESA := dglGetProcAddress('glWindowPos4dMESA');
  glWindowPos4dvMESA := dglGetProcAddress('glWindowPos4dvMESA');
  glWindowPos4fMESA := dglGetProcAddress('glWindowPos4fMESA');
  glWindowPos4fvMESA := dglGetProcAddress('glWindowPos4fvMESA');
  glWindowPos4iMESA := dglGetProcAddress('glWindowPos4iMESA');
  glWindowPos4ivMESA := dglGetProcAddress('glWindowPos4ivMESA');
  glWindowPos4sMESA := dglGetProcAddress('glWindowPos4sMESA');
  glWindowPos4svMESA := dglGetProcAddress('glWindowPos4svMESA');
end;

procedure Read_GL_NV_evaluators;
begin
  glMapControlPointsNV := dglGetProcAddress('glMapControlPointsNV');
  glMapParameterivNV := dglGetProcAddress('glMapParameterivNV');
  glMapParameterfvNV := dglGetProcAddress('glMapParameterfvNV');
  glGetMapControlPointsNV := dglGetProcAddress('glGetMapControlPointsNV');
  glGetMapParameterivNV := dglGetProcAddress('glGetMapParameterivNV');
  glGetMapParameterfvNV := dglGetProcAddress('glGetMapParameterfvNV');
  glGetMapAttribParameterivNV := dglGetProcAddress('glGetMapAttribParameterivNV');
  glGetMapAttribParameterfvNV := dglGetProcAddress('glGetMapAttribParameterfvNV');
  glEvalMapsNV := dglGetProcAddress('glEvalMapsNV');
end;

procedure Read_GL_NV_fence;
begin
  glDeleteFencesNV := dglGetProcAddress('glDeleteFencesNV');
  glGenFencesNV := dglGetProcAddress('glGenFencesNV');
  glIsFenceNV := dglGetProcAddress('glIsFenceNV');
  glTestFenceNV := dglGetProcAddress('glTestFenceNV');
  glGetFenceivNV := dglGetProcAddress('glGetFenceivNV');
  glFinishFenceNV := dglGetProcAddress('glFinishFenceNV');
  glSetFenceNV := dglGetProcAddress('glSetFenceNV');
end;

procedure Read_GL_NV_fragment_program;
begin
  glProgramNamedParameter4fNV := dglGetProcAddress('glProgramNamedParameter4fNV');
  glProgramNamedParameter4dNV := dglGetProcAddress('glProgramNamedParameter4dNV');
  glProgramNamedParameter4fvNV := dglGetProcAddress('glProgramNamedParameter4fvNV');
  glProgramNamedParameter4dvNV := dglGetProcAddress('glProgramNamedParameter4dvNV');
  glGetProgramNamedParameterfvNV := dglGetProcAddress('glGetProgramNamedParameterfvNV');
  glGetProgramNamedParameterdvNV := dglGetProcAddress('glGetProgramNamedParameterdvNV');
end;

procedure Read_GL_NV_half_float;
begin
  glVertex2hNV := dglGetProcAddress('glVertex2hNV');
  glVertex2hvNV := dglGetProcAddress('glVertex2hvNV');
  glVertex3hNV := dglGetProcAddress('glVertex3hNV');
  glVertex3hvNV := dglGetProcAddress('glVertex3hvNV');
  glVertex4hNV := dglGetProcAddress('glVertex4hNV');
  glVertex4hvNV := dglGetProcAddress('glVertex4hvNV');
  glNormal3hNV := dglGetProcAddress('glNormal3hNV');
  glNormal3hvNV := dglGetProcAddress('glNormal3hvNV');
  glColor3hNV := dglGetProcAddress('glColor3hNV');
  glColor3hvNV := dglGetProcAddress('glColor3hvNV');
  glColor4hNV := dglGetProcAddress('glColor4hNV');
  glColor4hvNV := dglGetProcAddress('glColor4hvNV');
  glTexCoord1hNV := dglGetProcAddress('glTexCoord1hNV');
  glTexCoord1hvNV := dglGetProcAddress('glTexCoord1hvNV');
  glTexCoord2hNV := dglGetProcAddress('glTexCoord2hNV');
  glTexCoord2hvNV := dglGetProcAddress('glTexCoord2hvNV');
  glTexCoord3hNV := dglGetProcAddress('glTexCoord3hNV');
  glTexCoord3hvNV := dglGetProcAddress('glTexCoord3hvNV');
  glTexCoord4hNV := dglGetProcAddress('glTexCoord4hNV');
  glTexCoord4hvNV := dglGetProcAddress('glTexCoord4hvNV');
  glMultiTexCoord1hNV := dglGetProcAddress('glMultiTexCoord1hNV');
  glMultiTexCoord1hvNV := dglGetProcAddress('glMultiTexCoord1hvNV');
  glMultiTexCoord2hNV := dglGetProcAddress('glMultiTexCoord2hNV');
  glMultiTexCoord2hvNV := dglGetProcAddress('glMultiTexCoord2hvNV');
  glMultiTexCoord3hNV := dglGetProcAddress('glMultiTexCoord3hNV');
  glMultiTexCoord3hvNV := dglGetProcAddress('glMultiTexCoord3hvNV');
  glMultiTexCoord4hNV := dglGetProcAddress('glMultiTexCoord4hNV');
  glMultiTexCoord4hvNV := dglGetProcAddress('glMultiTexCoord4hvNV');
  glFogCoordhNV := dglGetProcAddress('glFogCoordhNV');
  glFogCoordhvNV := dglGetProcAddress('glFogCoordhvNV');
  glSecondaryColor3hNV := dglGetProcAddress('glSecondaryColor3hNV');
  glSecondaryColor3hvNV := dglGetProcAddress('glSecondaryColor3hvNV');
  glVertexWeighthNV := dglGetProcAddress('glVertexWeighthNV');
  glVertexWeighthvNV := dglGetProcAddress('glVertexWeighthvNV');
  glVertexAttrib1hNV := dglGetProcAddress('glVertexAttrib1hNV');
  glVertexAttrib1hvNV := dglGetProcAddress('glVertexAttrib1hvNV');
  glVertexAttrib2hNV := dglGetProcAddress('glVertexAttrib2hNV');
  glVertexAttrib2hvNV := dglGetProcAddress('glVertexAttrib2hvNV');
  glVertexAttrib3hNV := dglGetProcAddress('glVertexAttrib3hNV');
  glVertexAttrib3hvNV := dglGetProcAddress('glVertexAttrib3hvNV');
  glVertexAttrib4hNV := dglGetProcAddress('glVertexAttrib4hNV');
  glVertexAttrib4hvNV := dglGetProcAddress('glVertexAttrib4hvNV');
  glVertexAttribs1hvNV := dglGetProcAddress('glVertexAttribs1hvNV');
  glVertexAttribs2hvNV := dglGetProcAddress('glVertexAttribs2hvNV');
  glVertexAttribs3hvNV := dglGetProcAddress('glVertexAttribs3hvNV');
  glVertexAttribs4hvNV := dglGetProcAddress('glVertexAttribs4hvNV');
end;

procedure Read_GL_NV_occlusion_query;
begin
  glGenOcclusionQueriesNV := dglGetProcAddress('glGenOcclusionQueriesNV');
  glDeleteOcclusionQueriesNV := dglGetProcAddress('glDeleteOcclusionQueriesNV');
  glIsOcclusionQueryNV := dglGetProcAddress('glIsOcclusionQueryNV');
  glBeginOcclusionQueryNV := dglGetProcAddress('glBeginOcclusionQueryNV');
  glEndOcclusionQueryNV := dglGetProcAddress('glEndOcclusionQueryNV');
  glGetOcclusionQueryivNV := dglGetProcAddress('glGetOcclusionQueryivNV');
  glGetOcclusionQueryuivNV := dglGetProcAddress('glGetOcclusionQueryuivNV');
end;

procedure Read_GL_NV_pixel_data_range;
begin
  glPixelDataRangeNV := dglGetProcAddress('glPixelDataRangeNV');
  glFlushPixelDataRangeNV := dglGetProcAddress('glFlushPixelDataRangeNV');
end;

procedure Read_GL_NV_point_sprite;
begin
  glPointParameteriNV := dglGetProcAddress('glPointParameteriNV');
  glPointParameterivNV := dglGetProcAddress('glPointParameterivNV');
end;

procedure Read_GL_NV_primitive_restart;
begin
  glPrimitiveRestartNV := dglGetProcAddress('glPrimitiveRestartNV');
  glPrimitiveRestartIndexNV := dglGetProcAddress('glPrimitiveRestartIndexNV');
end;

procedure Read_GL_NV_register_combiners;
begin
  glCombinerParameterfvNV := dglGetProcAddress('glCombinerParameterfvNV');
  glCombinerParameterfNV := dglGetProcAddress('glCombinerParameterfNV');
  glCombinerParameterivNV := dglGetProcAddress('glCombinerParameterivNV');
  glCombinerParameteriNV := dglGetProcAddress('glCombinerParameteriNV');
  glCombinerInputNV := dglGetProcAddress('glCombinerInputNV');
  glCombinerOutputNV := dglGetProcAddress('glCombinerOutputNV');
  glFinalCombinerInputNV := dglGetProcAddress('glFinalCombinerInputNV');
  glGetCombinerInputParameterfvNV := dglGetProcAddress('glGetCombinerInputParameterfvNV');
  glGetCombinerInputParameterivNV := dglGetProcAddress('glGetCombinerInputParameterivNV');
  glGetCombinerOutputParameterfvNV := dglGetProcAddress('glGetCombinerOutputParameterfvNV');
  glGetCombinerOutputParameterivNV := dglGetProcAddress('glGetCombinerOutputParameterivNV');
  glGetFinalCombinerInputParameterfvNV := dglGetProcAddress('glGetFinalCombinerInputParameterfvNV');
  glGetFinalCombinerInputParameterivNV := dglGetProcAddress('glGetFinalCombinerInputParameterivNV');
end;

procedure Read_GL_NV_register_combiners2;
begin
  glCombinerStageParameterfvNV := dglGetProcAddress('glCombinerStageParameterfvNV');
  glGetCombinerStageParameterfvNV := dglGetProcAddress('glGetCombinerStageParameterfvNV');
end;

procedure Read_GL_NV_vertex_array_range;
begin
  glFlushVertexArrayRangeNV := dglGetProcAddress('glFlushVertexArrayRangeNV');
  glVertexArrayRangeNV := dglGetProcAddress('glVertexArrayRangeNV');
end;

procedure Read_GL_NV_vertex_program;
begin
  glAreProgramsResidentNV := dglGetProcAddress('glAreProgramsResidentNV');
  glBindProgramNV := dglGetProcAddress('glBindProgramNV');
  glDeleteProgramsNV := dglGetProcAddress('glDeleteProgramsNV');
  glExecuteProgramNV := dglGetProcAddress('glExecuteProgramNV');
  glGenProgramsNV := dglGetProcAddress('glGenProgramsNV');
  glGetProgramParameterdvNV := dglGetProcAddress('glGetProgramParameterdvNV');
  glGetProgramParameterfvNV := dglGetProcAddress('glGetProgramParameterfvNV');
  glGetProgramivNV := dglGetProcAddress('glGetProgramivNV');
  glGetProgramStringNV := dglGetProcAddress('glGetProgramStringNV');
  glGetTrackMatrixivNV := dglGetProcAddress('glGetTrackMatrixivNV');
  glGetVertexAttribdvNV := dglGetProcAddress('glGetVertexAttribdvNV');
  glGetVertexAttribfvNV := dglGetProcAddress('glGetVertexAttribfvNV');
  glGetVertexAttribivNV := dglGetProcAddress('glGetVertexAttribivNV');
  glGetVertexAttribPointervNV := dglGetProcAddress('glGetVertexAttribPointervNV');
  glIsProgramNV := dglGetProcAddress('glIsProgramNV');
  glLoadProgramNV := dglGetProcAddress('glLoadProgramNV');
  glProgramParameter4dNV := dglGetProcAddress('glProgramParameter4dNV');
  glProgramParameter4dvNV := dglGetProcAddress('glProgramParameter4dvNV');
  glProgramParameter4fNV := dglGetProcAddress('glProgramParameter4fNV');
  glProgramParameter4fvNV := dglGetProcAddress('glProgramParameter4fvNV');
  glProgramParameters4dvNV := dglGetProcAddress('glProgramParameters4dvNV');
  glProgramParameters4fvNV := dglGetProcAddress('glProgramParameters4fvNV');
  glRequestResidentProgramsNV := dglGetProcAddress('glRequestResidentProgramsNV');
  glTrackMatrixNV := dglGetProcAddress('glTrackMatrixNV');
  glVertexAttribPointerNV := dglGetProcAddress('glVertexAttribPointerNV');
  glVertexAttrib1dNV := dglGetProcAddress('glVertexAttrib1dNV');
  glVertexAttrib1dvNV := dglGetProcAddress('glVertexAttrib1dvNV');
  glVertexAttrib1fNV := dglGetProcAddress('glVertexAttrib1fNV');
  glVertexAttrib1fvNV := dglGetProcAddress('glVertexAttrib1fvNV');
  glVertexAttrib1sNV := dglGetProcAddress('glVertexAttrib1sNV');
  glVertexAttrib1svNV := dglGetProcAddress('glVertexAttrib1svNV');
  glVertexAttrib2dNV := dglGetProcAddress('glVertexAttrib2dNV');
  glVertexAttrib2dvNV := dglGetProcAddress('glVertexAttrib2dvNV');
  glVertexAttrib2fNV := dglGetProcAddress('glVertexAttrib2fNV');
  glVertexAttrib2fvNV := dglGetProcAddress('glVertexAttrib2fvNV');
  glVertexAttrib2sNV := dglGetProcAddress('glVertexAttrib2sNV');
  glVertexAttrib2svNV := dglGetProcAddress('glVertexAttrib2svNV');
  glVertexAttrib3dNV := dglGetProcAddress('glVertexAttrib3dNV');
  glVertexAttrib3dvNV := dglGetProcAddress('glVertexAttrib3dvNV');
  glVertexAttrib3fNV := dglGetProcAddress('glVertexAttrib3fNV');
  glVertexAttrib3fvNV := dglGetProcAddress('glVertexAttrib3fvNV');
  glVertexAttrib3sNV := dglGetProcAddress('glVertexAttrib3sNV');
  glVertexAttrib3svNV := dglGetProcAddress('glVertexAttrib3svNV');
  glVertexAttrib4dNV := dglGetProcAddress('glVertexAttrib4dNV');
  glVertexAttrib4dvNV := dglGetProcAddress('glVertexAttrib4dvNV');
  glVertexAttrib4fNV := dglGetProcAddress('glVertexAttrib4fNV');
  glVertexAttrib4fvNV := dglGetProcAddress('glVertexAttrib4fvNV');
  glVertexAttrib4sNV := dglGetProcAddress('glVertexAttrib4sNV');
  glVertexAttrib4svNV := dglGetProcAddress('glVertexAttrib4svNV');
  glVertexAttrib4ubNV := dglGetProcAddress('glVertexAttrib4ubNV');
  glVertexAttrib4ubvNV := dglGetProcAddress('glVertexAttrib4ubvNV');
  glVertexAttribs1dvNV := dglGetProcAddress('glVertexAttribs1dvNV');
  glVertexAttribs1fvNV := dglGetProcAddress('glVertexAttribs1fvNV');
  glVertexAttribs1svNV := dglGetProcAddress('glVertexAttribs1svNV');
  glVertexAttribs2dvNV := dglGetProcAddress('glVertexAttribs2dvNV');
  glVertexAttribs2fvNV := dglGetProcAddress('glVertexAttribs2fvNV');
  glVertexAttribs2svNV := dglGetProcAddress('glVertexAttribs2svNV');
  glVertexAttribs3dvNV := dglGetProcAddress('glVertexAttribs3dvNV');
  glVertexAttribs3fvNV := dglGetProcAddress('glVertexAttribs3fvNV');
  glVertexAttribs3svNV := dglGetProcAddress('glVertexAttribs3svNV');
  glVertexAttribs4dvNV := dglGetProcAddress('glVertexAttribs4dvNV');
  glVertexAttribs4fvNV := dglGetProcAddress('glVertexAttribs4fvNV');
  glVertexAttribs4svNV := dglGetProcAddress('glVertexAttribs4svNV');
  glVertexAttribs4ubvNV := dglGetProcAddress('glVertexAttribs4ubvNV');
end;

procedure Read_GL_NV_depth_buffer_float;
begin
  glDepthRangedNV := dglGetProcAddress('glDepthRangedNV');
  glClearDepthdNV := dglGetProcAddress('glClearDepthdNV');
  glDepthBoundsdNV := dglGetProcAddress('glDepthBoundsdNV');
end;

procedure Read_GL_NV_framebuffer_multisample_coverage;
begin
  glRenderbufferStorageMultsampleCoverageNV := dglGetProcAddress('glRenderbufferStorageMultsampleCoverageNV');
end;

procedure Read_GL_NV_geometry_program4;
begin
  glProgramVertexLimitNV := dglGetProcAddress('glProgramVertexLimitNV');
end;

procedure Read_GL_NV_gpu_program4;
begin
  glProgramLocalParameterI4iNV := dglGetProcAddress('glProgramLocalParameterI4iNV');
  glProgramLocalParameterI4ivNV := dglGetProcAddress('glProgramLocalParameterI4ivNV');
  glProgramLocalParametersI4ivNV := dglGetProcAddress('glProgramLocalParametersI4ivNV');
  glProgramLocalParameterI4uiNV := dglGetProcAddress('glProgramLocalParameterI4uiNV');
  glProgramLocalParameterI4uivNV := dglGetProcAddress('glProgramLocalParameterI4uivNV');
  glProgramLocalParametersI4uivNV := dglGetProcAddress('glProgramLocalParametersI4uivNV');
  glProgramEnvParameterI4iNV := dglGetProcAddress('glProgramEnvParameterI4iNV');
  glProgramEnvParameterI4ivNV := dglGetProcAddress('glProgramEnvParameterI4ivNV');
  glProgramEnvParametersI4ivNV := dglGetProcAddress('glProgramEnvParametersI4ivNV');
  glProgramEnvParameterI4uiNV := dglGetProcAddress('glProgramEnvParameterI4uiNV');
  glProgramEnvParameterI4uivNV := dglGetProcAddress('glProgramEnvParameterI4uivNV');
  glProgramEnvParametersI4uivNV := dglGetProcAddress('glProgramEnvParametersI4uivNV');
  glGetProgramLocalParameterIivNV := dglGetProcAddress('glGetProgramLocalParameterIivNV');
  glGetProgramLocalParameterIuivNV := dglGetProcAddress('glGetProgramLocalParameterIuivNV');
  glGetProgramEnvParameterIivNV := dglGetProcAddress('glGetProgramEnvParameterIivNV');
  glGetProgramEnvParameterIuivNV := dglGetProcAddress('glGetProgramEnvParameterIuivNV');
end;

procedure Read_GL_NV_parameter_buffer_object;
begin
  glBindBufferRangeNV := dglGetProcAddress('glBindBufferRangeNV');
  glBindBufferOffsetNV := dglGetProcAddress('glBindBufferOffsetNV');
  glBindBufferBaseNV := dglGetProcAddress('glBindBufferBaseNV');
  glProgramBufferParametersfvNV := dglGetProcAddress('glProgramBufferParametersfvNV');
  glProgramBufferParametersIivNV := dglGetProcAddress('glProgramBufferParametersIivNV');
  glProgramBufferParametersIuivNV := dglGetProcAddress('glProgramBufferParametersIuivNV');
end;

procedure Read_GL_NV_transform_feedback;
begin
  glTransformFeedbackAttribsNV := dglGetProcAddress('glTransformFeedbackAttribsNV');
  glTransformFeedbackVaryingsNV := dglGetProcAddress('glTransformFeedbackVaryingsNV');
  glBeginTransformFeedbackNV := dglGetProcAddress('glBeginTransformFeedbackNV');
  glEndTransformFeedbackNV := dglGetProcAddress('glEndTransformFeedbackNV');
  glGetVaryingLocationNV := dglGetProcAddress('glGetVaryingLocationNV');
  glGetActiveVaryingNV := dglGetProcAddress('glGetActiveVaryingNV');
  glActiveVaryingNV := dglGetProcAddress('glActiveVaryingNV');
  glGetTransformFeedbackVaryingNV := dglGetProcAddress('glGetTransformFeedbackVaryingNV');
end;

procedure Read_GL_NV_conditional_render;
begin
  glBeginConditionalRenderNV := dglGetProcAddress('glBeginConditionalRenderNV');
  glEndConditionalRenderNV := dglGetProcAddress('glEndConditionalRenderNV');
end;

procedure Read_GL_NV_explicit_multisample;
begin
  glGetMultisamplefvNV := dglGetProcAddress('glGetMultisamplefvNV');
  glSampleMaskIndexedNV := dglGetProcAddress('glSampleMaskIndexedNV');
  glTexRenderbufferNV := dglGetProcAddress('glTexRenderbufferNV');
end;

procedure Read_GL_NV_transform_feedback2;
begin
  glBindTransformFeedbackNV := dglGetProcAddress('glBindTransformFeedbackNV');
  glDeleteTransformFeedbacksNV := dglGetProcAddress('glDeleteTransformFeedbacksNV');
  glGenTransformFeedbacksNV := dglGetProcAddress('glGenTransformFeedbacksNV');
  glIsTransformFeedbackNV := dglGetProcAddress('glIsTransformFeedbackNV');
  glPauseTransformFeedbackNV := dglGetProcAddress('glPauseTransformFeedbackNV');
  glResumeTransformFeedbackNV := dglGetProcAddress('glResumeTransformFeedbackNV');
  glDrawTransformFeedbackNV := dglGetProcAddress('glDrawTransformFeedbackNV');
end;

procedure Read_GL_PGI_misc_hints;
begin
  glHintPGI := dglGetProcAddress('glHintPGI');
end;

procedure Read_GL_SGIS_detail_texture;
begin
  glDetailTexFuncSGIS := dglGetProcAddress('glDetailTexFuncSGIS');
  glGetDetailTexFuncSGIS := dglGetProcAddress('glGetDetailTexFuncSGIS');
end;

procedure Read_GL_SGIS_fog_function;
begin
  glFogFuncSGIS := dglGetProcAddress('glFogFuncSGIS');
  glGetFogFuncSGIS := dglGetProcAddress('glGetFogFuncSGIS');
end;

procedure Read_GL_SGIS_multisample;
begin
  glSampleMaskSGIS := dglGetProcAddress('glSampleMaskSGIS');
  glSamplePatternSGIS := dglGetProcAddress('glSamplePatternSGIS');
end;

procedure Read_GL_SGIS_pixel_texture;
begin
  glPixelTexGenParameteriSGIS := dglGetProcAddress('glPixelTexGenParameteriSGIS');
  glPixelTexGenParameterivSGIS := dglGetProcAddress('glPixelTexGenParameterivSGIS');
  glPixelTexGenParameterfSGIS := dglGetProcAddress('glPixelTexGenParameterfSGIS');
  glPixelTexGenParameterfvSGIS := dglGetProcAddress('glPixelTexGenParameterfvSGIS');
  glGetPixelTexGenParameterivSGIS := dglGetProcAddress('glGetPixelTexGenParameterivSGIS');
  glGetPixelTexGenParameterfvSGIS := dglGetProcAddress('glGetPixelTexGenParameterfvSGIS');
end;

procedure Read_GL_SGIS_point_parameters;
begin
  glPointParameterfSGIS := dglGetProcAddress('glPointParameterfSGIS');
  glPointParameterfvSGIS := dglGetProcAddress('glPointParameterfvSGIS');
end;

procedure Read_GL_SGIS_sharpen_texture;
begin
  glSharpenTexFuncSGIS := dglGetProcAddress('glSharpenTexFuncSGIS');
  glGetSharpenTexFuncSGIS := dglGetProcAddress('glGetSharpenTexFuncSGIS');
end;

procedure Read_GL_SGIS_texture4D;
begin
  glTexImage4DSGIS := dglGetProcAddress('glTexImage4DSGIS');
  glTexSubImage4DSGIS := dglGetProcAddress('glTexSubImage4DSGIS');
end;

procedure Read_GL_SGIS_texture_color_mask;
begin
  glTextureColorMaskSGIS := dglGetProcAddress('glTextureColorMaskSGIS');
end;

procedure Read_GL_SGIS_texture_filter4;
begin
  glGetTexFilterFuncSGIS := dglGetProcAddress('glGetTexFilterFuncSGIS');
  glTexFilterFuncSGIS := dglGetProcAddress('glTexFilterFuncSGIS');
end;

procedure Read_GL_SGIX_async;
begin
  glAsyncMarkerSGIX := dglGetProcAddress('glAsyncMarkerSGIX');
  glFinishAsyncSGIX := dglGetProcAddress('glFinishAsyncSGIX');
  glPollAsyncSGIX := dglGetProcAddress('glPollAsyncSGIX');
  glGenAsyncMarkersSGIX := dglGetProcAddress('glGenAsyncMarkersSGIX');
  glDeleteAsyncMarkersSGIX := dglGetProcAddress('glDeleteAsyncMarkersSGIX');
  glIsAsyncMarkerSGIX := dglGetProcAddress('glIsAsyncMarkerSGIX');
end;

procedure Read_GL_SGIX_flush_raster;
begin
  glFlushRasterSGIX := dglGetProcAddress('glFlushRasterSGIX');
end;

procedure Read_GL_SGIX_fragment_lighting;
begin
  glFragmentColorMaterialSGIX := dglGetProcAddress('glFragmentColorMaterialSGIX');
  glFragmentLightfSGIX := dglGetProcAddress('glFragmentLightfSGIX');
  glFragmentLightfvSGIX := dglGetProcAddress('glFragmentLightfvSGIX');
  glFragmentLightiSGIX := dglGetProcAddress('glFragmentLightiSGIX');
  glFragmentLightivSGIX := dglGetProcAddress('glFragmentLightivSGIX');
  glFragmentLightModelfSGIX := dglGetProcAddress('glFragmentLightModelfSGIX');
  glFragmentLightModelfvSGIX := dglGetProcAddress('glFragmentLightModelfvSGIX');
  glFragmentLightModeliSGIX := dglGetProcAddress('glFragmentLightModeliSGIX');
  glFragmentLightModelivSGIX := dglGetProcAddress('glFragmentLightModelivSGIX');
  glFragmentMaterialfSGIX := dglGetProcAddress('glFragmentMaterialfSGIX');
  glFragmentMaterialfvSGIX := dglGetProcAddress('glFragmentMaterialfvSGIX');
  glFragmentMaterialiSGIX := dglGetProcAddress('glFragmentMaterialiSGIX');
  glFragmentMaterialivSGIX := dglGetProcAddress('glFragmentMaterialivSGIX');
  glGetFragmentLightfvSGIX := dglGetProcAddress('glGetFragmentLightfvSGIX');
  glGetFragmentLightivSGIX := dglGetProcAddress('glGetFragmentLightivSGIX');
  glGetFragmentMaterialfvSGIX := dglGetProcAddress('glGetFragmentMaterialfvSGIX');
  glGetFragmentMaterialivSGIX := dglGetProcAddress('glGetFragmentMaterialivSGIX');
  glLightEnviSGIX := dglGetProcAddress('glLightEnviSGIX');
end;

procedure Read_GL_SGIX_framezoom;
begin
  glFrameZoomSGIX := dglGetProcAddress('glFrameZoomSGIX');
end;

procedure Read_GL_SGIX_igloo_interface;
begin
  glIglooInterfaceSGIX := dglGetProcAddress('glIglooInterfaceSGIX');
end;

procedure Read_GL_SGIX_instruments;
begin
  glGetInstrumentsSGIX := dglGetProcAddress('glGetInstrumentsSGIX');
  glInstrumentsBufferSGIX := dglGetProcAddress('glInstrumentsBufferSGIX');
  glPollInstrumentsSGIX := dglGetProcAddress('glPollInstrumentsSGIX');
  glReadInstrumentsSGIX := dglGetProcAddress('glReadInstrumentsSGIX');
  glStartInstrumentsSGIX := dglGetProcAddress('glStartInstrumentsSGIX');
  glStopInstrumentsSGIX := dglGetProcAddress('glStopInstrumentsSGIX');
end;

procedure Read_GL_SGIX_list_priority;
begin
  glGetListParameterfvSGIX := dglGetProcAddress('glGetListParameterfvSGIX');
  glGetListParameterivSGIX := dglGetProcAddress('glGetListParameterivSGIX');
  glListParameterfSGIX := dglGetProcAddress('glListParameterfSGIX');
  glListParameterfvSGIX := dglGetProcAddress('glListParameterfvSGIX');
  glListParameteriSGIX := dglGetProcAddress('glListParameteriSGIX');
  glListParameterivSGIX := dglGetProcAddress('glListParameterivSGIX');
end;

procedure Read_GL_SGIX_pixel_texture;
begin
  glPixelTexGenSGIX := dglGetProcAddress('glPixelTexGenSGIX');
end;

procedure Read_GL_SGIX_polynomial_ffd;
begin
  glDeformationMap3dSGIX := dglGetProcAddress('glDeformationMap3dSGIX');
  glDeformationMap3fSGIX := dglGetProcAddress('glDeformationMap3fSGIX');
  glDeformSGIX := dglGetProcAddress('glDeformSGIX');
  glLoadIdentityDeformationMapSGIX := dglGetProcAddress('glLoadIdentityDeformationMapSGIX');
end;

procedure Read_GL_SGIX_reference_plane;
begin
  glReferencePlaneSGIX := dglGetProcAddress('glReferencePlaneSGIX');
end;

procedure Read_GL_SGIX_sprite;
begin
  glSpriteParameterfSGIX := dglGetProcAddress('glSpriteParameterfSGIX');
  glSpriteParameterfvSGIX := dglGetProcAddress('glSpriteParameterfvSGIX');
  glSpriteParameteriSGIX := dglGetProcAddress('glSpriteParameteriSGIX');
  glSpriteParameterivSGIX := dglGetProcAddress('glSpriteParameterivSGIX');
end;

procedure Read_GL_SGIX_tag_sample_buffer;
begin
  glTagSampleBufferSGIX := dglGetProcAddress('glTagSampleBufferSGIX');
end;

procedure Read_GL_SGI_color_table;
begin
  glColorTableSGI := dglGetProcAddress('glColorTableSGI');
  glColorTableParameterfvSGI := dglGetProcAddress('glColorTableParameterfvSGI');
  glColorTableParameterivSGI := dglGetProcAddress('glColorTableParameterivSGI');
  glCopyColorTableSGI := dglGetProcAddress('glCopyColorTableSGI');
  glGetColorTableSGI := dglGetProcAddress('glGetColorTableSGI');
  glGetColorTableParameterfvSGI := dglGetProcAddress('glGetColorTableParameterfvSGI');
  glGetColorTableParameterivSGI := dglGetProcAddress('glGetColorTableParameterivSGI');
end;

procedure Read_GL_SUNX_constant_data;
begin
  glFinishTextureSUNX := dglGetProcAddress('glFinishTextureSUNX');
end;

procedure Read_GL_SUN_global_alpha;
begin
  glGlobalAlphaFactorbSUN := dglGetProcAddress('glGlobalAlphaFactorbSUN');
  glGlobalAlphaFactorsSUN := dglGetProcAddress('glGlobalAlphaFactorsSUN');
  glGlobalAlphaFactoriSUN := dglGetProcAddress('glGlobalAlphaFactoriSUN');
  glGlobalAlphaFactorfSUN := dglGetProcAddress('glGlobalAlphaFactorfSUN');
  glGlobalAlphaFactordSUN := dglGetProcAddress('glGlobalAlphaFactordSUN');
  glGlobalAlphaFactorubSUN := dglGetProcAddress('glGlobalAlphaFactorubSUN');
  glGlobalAlphaFactorusSUN := dglGetProcAddress('glGlobalAlphaFactorusSUN');
  glGlobalAlphaFactoruiSUN := dglGetProcAddress('glGlobalAlphaFactoruiSUN');
end;

procedure Read_GL_SUN_mesh_array;
begin
  glDrawMeshArraysSUN := dglGetProcAddress('glDrawMeshArraysSUN');
end;

procedure Read_GL_SUN_triangle_list;
begin
  glReplacementCodeuiSUN := dglGetProcAddress('glReplacementCodeuiSUN');
  glReplacementCodeusSUN := dglGetProcAddress('glReplacementCodeusSUN');
  glReplacementCodeubSUN := dglGetProcAddress('glReplacementCodeubSUN');
  glReplacementCodeuivSUN := dglGetProcAddress('glReplacementCodeuivSUN');
  glReplacementCodeusvSUN := dglGetProcAddress('glReplacementCodeusvSUN');
  glReplacementCodeubvSUN := dglGetProcAddress('glReplacementCodeubvSUN');
  glReplacementCodePointerSUN := dglGetProcAddress('glReplacementCodePointerSUN');
end;

procedure Read_GL_SUN_vertex;
begin
  glColor4ubVertex2fSUN := dglGetProcAddress('glColor4ubVertex2fSUN');
  glColor4ubVertex2fvSUN := dglGetProcAddress('glColor4ubVertex2fvSUN');
  glColor4ubVertex3fSUN := dglGetProcAddress('glColor4ubVertex3fSUN');
  glColor4ubVertex3fvSUN := dglGetProcAddress('glColor4ubVertex3fvSUN');
  glColor3fVertex3fSUN := dglGetProcAddress('glColor3fVertex3fSUN');
  glColor3fVertex3fvSUN := dglGetProcAddress('glColor3fVertex3fvSUN');
  glNormal3fVertex3fSUN := dglGetProcAddress('glNormal3fVertex3fSUN');
  glNormal3fVertex3fvSUN := dglGetProcAddress('glNormal3fVertex3fvSUN');
  glColor4fNormal3fVertex3fSUN := dglGetProcAddress('glColor4fNormal3fVertex3fSUN');
  glColor4fNormal3fVertex3fvSUN := dglGetProcAddress('glColor4fNormal3fVertex3fvSUN');
  glTexCoord2fVertex3fSUN := dglGetProcAddress('glTexCoord2fVertex3fSUN');
  glTexCoord2fVertex3fvSUN := dglGetProcAddress('glTexCoord2fVertex3fvSUN');
  glTexCoord4fVertex4fSUN := dglGetProcAddress('glTexCoord4fVertex4fSUN');
  glTexCoord4fVertex4fvSUN := dglGetProcAddress('glTexCoord4fVertex4fvSUN');
  glTexCoord2fColor4ubVertex3fSUN := dglGetProcAddress('glTexCoord2fColor4ubVertex3fSUN');
  glTexCoord2fColor4ubVertex3fvSUN := dglGetProcAddress('glTexCoord2fColor4ubVertex3fvSUN');
  glTexCoord2fColor3fVertex3fSUN := dglGetProcAddress('glTexCoord2fColor3fVertex3fSUN');
  glTexCoord2fColor3fVertex3fvSUN := dglGetProcAddress('glTexCoord2fColor3fVertex3fvSUN');
  glTexCoord2fNormal3fVertex3fSUN := dglGetProcAddress('glTexCoord2fNormal3fVertex3fSUN');
  glTexCoord2fNormal3fVertex3fvSUN := dglGetProcAddress('glTexCoord2fNormal3fVertex3fvSUN');
  glTexCoord2fColor4fNormal3fVertex3fSUN := dglGetProcAddress('glTexCoord2fColor4fNormal3fVertex3fSUN');
  glTexCoord2fColor4fNormal3fVertex3fvSUN := dglGetProcAddress('glTexCoord2fColor4fNormal3fVertex3fvSUN');
  glTexCoord4fColor4fNormal3fVertex4fSUN := dglGetProcAddress('glTexCoord4fColor4fNormal3fVertex4fSUN');
  glTexCoord4fColor4fNormal3fVertex4fvSUN := dglGetProcAddress('glTexCoord4fColor4fNormal3fVertex4fvSUN');
  glReplacementCodeuiVertex3fSUN := dglGetProcAddress('glReplacementCodeuiVertex3fSUN');
  glReplacementCodeuiVertex3fvSUN := dglGetProcAddress('glReplacementCodeuiVertex3fvSUN');
  glReplacementCodeuiColor4ubVertex3fSUN := dglGetProcAddress('glReplacementCodeuiColor4ubVertex3fSUN');
  glReplacementCodeuiColor4ubVertex3fvSUN := dglGetProcAddress('glReplacementCodeuiColor4ubVertex3fvSUN');
  glReplacementCodeuiColor3fVertex3fSUN := dglGetProcAddress('glReplacementCodeuiColor3fVertex3fSUN');
  glReplacementCodeuiColor3fVertex3fvSUN := dglGetProcAddress('glReplacementCodeuiColor3fVertex3fvSUN');
  glReplacementCodeuiNormal3fVertex3fSUN := dglGetProcAddress('glReplacementCodeuiNormal3fVertex3fSUN');
  glReplacementCodeuiNormal3fVertex3fvSUN := dglGetProcAddress('glReplacementCodeuiNormal3fVertex3fvSUN');
  glReplacementCodeuiColor4fNormal3fVertex3fSUN := dglGetProcAddress('glReplacementCodeuiColor4fNormal3fVertex3fSUN');
  glReplacementCodeuiColor4fNormal3fVertex3fvSUN := dglGetProcAddress('glReplacementCodeuiColor4fNormal3fVertex3fvSUN');
  glReplacementCodeuiTexCoord2fVertex3fSUN := dglGetProcAddress('glReplacementCodeuiTexCoord2fVertex3fSUN');
  glReplacementCodeuiTexCoord2fVertex3fvSUN := dglGetProcAddress('glReplacementCodeuiTexCoord2fVertex3fvSUN');
  glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN := dglGetProcAddress('glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN');
  glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN := dglGetProcAddress('glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN');
  glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN := dglGetProcAddress('glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN');
  glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN := dglGetProcAddress('glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN');
end;

{$IFDEF Win32}
procedure Read_WGL_ARB_buffer_region;
begin
  wglCreateBufferRegionARB := dglGetProcAddress('wglCreateBufferRegionARB');
  wglDeleteBufferRegionARB := dglGetProcAddress('wglDeleteBufferRegionARB');
  wglSaveBufferRegionARB := dglGetProcAddress('wglSaveBufferRegionARB');
  wglRestoreBufferRegionARB := dglGetProcAddress('wglRestoreBufferRegionARB');
end;

procedure Read_WGL_ARB_extensions_string;
begin
  wglGetExtensionsStringARB := dglGetProcAddress('wglGetExtensionsStringARB');
end;

procedure Read_WGL_ARB_make_current_read;
begin
  wglMakeContextCurrentARB := dglGetProcAddress('wglMakeContextCurrentARB');
  wglGetCurrentReadDCARB := dglGetProcAddress('wglGetCurrentReadDCARB');
end;

procedure Read_WGL_ARB_pbuffer;
begin
  wglCreatePbufferARB := dglGetProcAddress('wglCreatePbufferARB');
  wglGetPbufferDCARB := dglGetProcAddress('wglGetPbufferDCARB');
  wglReleasePbufferDCARB := dglGetProcAddress('wglReleasePbufferDCARB');
  wglDestroyPbufferARB := dglGetProcAddress('wglDestroyPbufferARB');
  wglQueryPbufferARB := dglGetProcAddress('wglQueryPbufferARB');
end;

procedure Read_WGL_ARB_pixel_format;
begin
  wglGetPixelFormatAttribivARB := dglGetProcAddress('wglGetPixelFormatAttribivARB');
  wglGetPixelFormatAttribfvARB := dglGetProcAddress('wglGetPixelFormatAttribfvARB');
  wglChoosePixelFormatARB := dglGetProcAddress('wglChoosePixelFormatARB');
end;

procedure Read_WGL_ARB_pixel_format_float;
begin
  wglClampColorARB := dglGetProcAddress('wglClampColorARB');
end;

procedure Read_WGL_ARB_render_texture;
begin
  wglBindTexImageARB := dglGetProcAddress('wglBindTexImageARB');
  wglReleaseTexImageARB := dglGetProcAddress('wglReleaseTexImageARB');
  wglSetPbufferAttribARB := dglGetProcAddress('wglSetPbufferAttribARB');
end;

procedure Read_WGL_ARB_create_context;
begin
  wglCreateContextAttribsARB := dglGetProcAddress('wglCreateContextAttribsARB');
end;

procedure Read_WGL_EXT_display_color_table;
begin
  wglCreateDisplayColorTableEXT := dglGetProcAddress('wglCreateDisplayColorTableEXT');
  wglLoadDisplayColorTableEXT := dglGetProcAddress('wglLoadDisplayColorTableEXT');
  wglBindDisplayColorTableEXT := dglGetProcAddress('wglBindDisplayColorTableEXT');
  wglDestroyDisplayColorTableEXT := dglGetProcAddress('wglDestroyDisplayColorTableEXT');
end;

procedure Read_WGL_EXT_extensions_string;
begin
  wglGetExtensionsStringEXT := dglGetProcAddress('wglGetExtensionsStringEXT');
end;

procedure Read_WGL_EXT_make_current_read;
begin
  wglMakeContextCurrentEXT := dglGetProcAddress('wglMakeContextCurrentEXT');
  wglGetCurrentReadDCEXT := dglGetProcAddress('wglGetCurrentReadDCEXT');
end;

procedure Read_WGL_EXT_pbuffer;
begin
  wglCreatePbufferEXT := dglGetProcAddress('wglCreatePbufferEXT');
  wglGetPbufferDCEXT := dglGetProcAddress('wglGetPbufferDCEXT');
  wglReleasePbufferDCEXT := dglGetProcAddress('wglReleasePbufferDCEXT');
  wglDestroyPbufferEXT := dglGetProcAddress('wglDestroyPbufferEXT');
  wglQueryPbufferEXT := dglGetProcAddress('wglQueryPbufferEXT');
end;

procedure Read_WGL_EXT_pixel_format;
begin
  wglGetPixelFormatAttribivEXT := dglGetProcAddress('wglGetPixelFormatAttribivEXT');
  wglGetPixelFormatAttribfvEXT := dglGetProcAddress('wglGetPixelFormatAttribfvEXT');
  wglChoosePixelFormatEXT := dglGetProcAddress('wglChoosePixelFormatEXT');
end;

procedure Read_WGL_EXT_swap_control;
begin
  wglSwapIntervalEXT := dglGetProcAddress('wglSwapIntervalEXT');
  wglGetSwapIntervalEXT := dglGetProcAddress('wglGetSwapIntervalEXT');
end;

procedure Read_WGL_I3D_digital_video_control;
begin
  wglGetDigitalVideoParametersI3D := dglGetProcAddress('wglGetDigitalVideoParametersI3D');
  wglSetDigitalVideoParametersI3D := dglGetProcAddress('wglSetDigitalVideoParametersI3D');
end;

procedure Read_WGL_I3D_gamma;
begin
  wglGetGammaTableParametersI3D := dglGetProcAddress('wglGetGammaTableParametersI3D');
  wglSetGammaTableParametersI3D := dglGetProcAddress('wglSetGammaTableParametersI3D');
  wglGetGammaTableI3D := dglGetProcAddress('wglGetGammaTableI3D');
  wglSetGammaTableI3D := dglGetProcAddress('wglSetGammaTableI3D');
end;

procedure Read_WGL_I3D_genlock;
begin
  wglEnableGenlockI3D := dglGetProcAddress('wglEnableGenlockI3D');
  wglDisableGenlockI3D := dglGetProcAddress('wglDisableGenlockI3D');
  wglIsEnabledGenlockI3D := dglGetProcAddress('wglIsEnabledGenlockI3D');
  wglGenlockSourceI3D := dglGetProcAddress('wglGenlockSourceI3D');
  wglGetGenlockSourceI3D := dglGetProcAddress('wglGetGenlockSourceI3D');
  wglGenlockSourceEdgeI3D := dglGetProcAddress('wglGenlockSourceEdgeI3D');
  wglGetGenlockSourceEdgeI3D := dglGetProcAddress('wglGetGenlockSourceEdgeI3D');
  wglGenlockSampleRateI3D := dglGetProcAddress('wglGenlockSampleRateI3D');
  wglGetGenlockSampleRateI3D := dglGetProcAddress('wglGetGenlockSampleRateI3D');
  wglGenlockSourceDelayI3D := dglGetProcAddress('wglGenlockSourceDelayI3D');
  wglGetGenlockSourceDelayI3D := dglGetProcAddress('wglGetGenlockSourceDelayI3D');
  wglQueryGenlockMaxSourceDelayI3D := dglGetProcAddress('wglQueryGenlockMaxSourceDelayI3D');
end;

procedure Read_WGL_I3D_image_buffer;
begin
  wglCreateImageBufferI3D := dglGetProcAddress('wglCreateImageBufferI3D');
  wglDestroyImageBufferI3D := dglGetProcAddress('wglDestroyImageBufferI3D');
  wglAssociateImageBufferEventsI3D := dglGetProcAddress('wglAssociateImageBufferEventsI3D');
  wglReleaseImageBufferEventsI3D := dglGetProcAddress('wglReleaseImageBufferEventsI3D');
end;

procedure Read_WGL_I3D_swap_frame_lock;
begin
  wglEnableFrameLockI3D := dglGetProcAddress('wglEnableFrameLockI3D');
  wglDisableFrameLockI3D := dglGetProcAddress('wglDisableFrameLockI3D');
  wglIsEnabledFrameLockI3D := dglGetProcAddress('wglIsEnabledFrameLockI3D');
  wglQueryFrameLockMasterI3D := dglGetProcAddress('wglQueryFrameLockMasterI3D');
end;

procedure Read_WGL_I3D_swap_frame_usage;
begin
  wglGetFrameUsageI3D := dglGetProcAddress('wglGetFrameUsageI3D');
  wglBeginFrameTrackingI3D := dglGetProcAddress('wglBeginFrameTrackingI3D');
  wglEndFrameTrackingI3D := dglGetProcAddress('wglEndFrameTrackingI3D');
  wglQueryFrameTrackingI3D := dglGetProcAddress('wglQueryFrameTrackingI3D');
end;

procedure Read_WGL_NV_vertex_array_range;
begin
  wglAllocateMemoryNV := dglGetProcAddress('wglAllocateMemoryNV');
  wglFreeMemoryNV := dglGetProcAddress('wglFreeMemoryNV');
end;

procedure Read_WGL_NV_present_video;
begin
  wglEnumerateVideoDevicesNV := dglGetProcAddress('wglEnumerateVideoDevicesNV');
  wglBindVideoDeviceNV := dglGetProcAddress('wglBindVideoDeviceNV');
  wglQueryCurrentContextNV := dglGetProcAddress('wglQueryCurrentContextNV');
end;

procedure Read_WGL_NV_video_out;
begin
  wglGetVideoDeviceNV := dglGetProcAddress('wglGetVideoDeviceNV');
  wglReleaseVideoDeviceNV := dglGetProcAddress('wglReleaseVideoDeviceNV');
  wglBindVideoImageNV := dglGetProcAddress('wglBindVideoImageNV');
  wglReleaseVideoImageNV := dglGetProcAddress('wglReleaseVideoImageNV');
  wglSendPbufferToVideoNV := dglGetProcAddress('wglSendPbufferToVideoNV');
  wglGetVideoInfoNV := dglGetProcAddress('wglGetVideoInfoNV');
end;

procedure Read_WGL_NV_swap_group;
begin
  wglJoinSwapGroupNV := dglGetProcAddress('wglJoinSwapGroupNV');
  wglBindSwapBarrierNV := dglGetProcAddress('wglBindSwapBarrierNV');
  wglQuerySwapGroupNV := dglGetProcAddress('wglQuerySwapGroupNV');
  wglQueryMaxSwapGroupsNV := dglGetProcAddress('wglQueryMaxSwapGroupsNV');
  wglQueryFrameCountNV := dglGetProcAddress('wglQueryFrameCountNV');
  wglResetFrameCountNV := dglGetProcAddress('wglResetFrameCountNV');
end;

procedure Read_WGL_NV_gpu_affinity;
begin
  wglEnumGpusNV := dglGetProcAddress('wglEnumGpusNV');
  wglEnumGpuDevicesNV := dglGetProcAddress('wglEnumGpuDevicesNV');
  wglCreateAffinityDCNV := dglGetProcAddress('wglCreateAffinityDCNV');
  wglEnumGpusFromAffinityDCNV := dglGetProcAddress('wglEnumGpusFromAffinityDCNV');
  wglDeleteDCNV := dglGetProcAddress('wglDeleteDCNV');
end;

procedure Read_WGL_OML_sync_control;
begin
  wglGetSyncValuesOML := dglGetProcAddress('wglGetSyncValuesOML');
  wglGetMscRateOML := dglGetProcAddress('wglGetMscRateOML');
  wglSwapBuffersMscOML := dglGetProcAddress('wglSwapBuffersMscOML');
  wglSwapLayerBuffersMscOML := dglGetProcAddress('wglSwapLayerBuffersMscOML');
  wglWaitForMscOML := dglGetProcAddress('wglWaitForMscOML');
  wglWaitForSbcOML := dglGetProcAddress('wglWaitForSbcOML');
end;

procedure Read_WIN_draw_range_elements;
begin
  glDrawRangeElementsWIN := dglGetProcAddress('glDrawRangeElementsWIN');
end;

procedure Read_WIN_swap_hint;
begin
  glAddSwapHintRectWIN := dglGetProcAddress('glAddSwapHintRectWIN');
end;
{$ENDIF}


procedure ReadExtensions;
begin
  ReadOpenGLCore;
  Read_GL_3DFX_tbuffer;
  Read_GL_APPLE_element_array;
  Read_GL_APPLE_fence;
  Read_GL_APPLE_vertex_array_object;
  Read_GL_APPLE_vertex_array_range;
  Read_GL_ARB_matrix_palette;
  Read_GL_ARB_multitexture;
  Read_GL_ARB_point_parameters;
  Read_GL_ARB_texture_compression;
  Read_GL_ARB_transpose_matrix;
  Read_GL_ARB_vertex_blend;
  Read_GL_ARB_vertex_buffer_object;
  Read_GL_ARB_vertex_program;
  Read_GL_ARB_window_pos;
  Read_GL_ARB_color_buffer_float;
  Read_GL_ARB_draw_instanced;
  Read_GL_ARB_framebuffer_object;
  Read_GL_ARB_geometry_shader4;
  Read_GL_ARB_instanced_arrays;
  Read_GL_ARB_map_buffer_range;
  Read_GL_ARB_texture_buffer_object;
  Read_GL_ARB_vertex_array_object;
  Read_GL_ATI_draw_buffers;
  Read_GL_ATI_element_array;
  Read_GL_ATI_envmap_bumpmap;
  Read_GL_ATI_fragment_shader;
  Read_GL_ATI_map_object_buffer;
  Read_GL_ATI_pn_triangles;
  Read_GL_ATI_separate_stencil;
  Read_GL_ATI_vertex_array_object;
  Read_GL_ATI_vertex_attrib_array_object;
  Read_GL_ATI_vertex_streams;
  Read_GL_EXT_blend_color;
  Read_GL_EXT_blend_func_separate;
  Read_GL_EXT_blend_minmax;
  Read_GL_EXT_color_subtable;
  Read_GL_EXT_compiled_vertex_array;
  Read_GL_EXT_convolution;
  Read_GL_EXT_coordinate_frame;
  Read_GL_EXT_copy_texture;
  Read_GL_EXT_cull_vertex;
  Read_GL_EXT_draw_range_elements;
  Read_GL_EXT_fog_coord;
  Read_GL_EXT_framebuffer_object;
  Read_GL_EXT_histogram;
  Read_GL_EXT_index_func;
  Read_GL_EXT_index_material;
  Read_GL_EXT_multi_draw_arrays;
  Read_GL_EXT_multisample;
  Read_GL_EXT_paletted_texture;
  Read_GL_EXT_pixel_transform;
  Read_GL_EXT_point_parameters;
  Read_GL_EXT_polygon_offset;
  Read_GL_EXT_secondary_color;
  Read_GL_EXT_stencil_two_side;
  Read_GL_EXT_subtexture;
  Read_GL_EXT_texture3D;
  Read_GL_EXT_texture_object;
  Read_GL_EXT_texture_perturb_normal;
  Read_GL_EXT_vertex_array;
  Read_GL_EXT_vertex_shader;
  Read_GL_EXT_vertex_weighting;
  Read_GL_EXT_depth_bounds_test;
  Read_GL_EXT_blend_equation_separate;
  Read_GL_EXT_stencil_clear_tag;
  Read_GL_EXT_framebuffer_blit;
  Read_GL_EXT_framebuffer_multisample;
  Read_GL_EXT_timer_query;
  Read_GL_EXT_gpu_program_parameters;
  Read_GL_EXT_bindable_uniform;
  Read_GL_EXT_draw_buffers2;
  Read_GL_EXT_draw_instanced;
  Read_GL_EXT_geometry_shader4;
  Read_GL_EXT_gpu_shader4;
  Read_GL_EXT_texture_array;
  Read_GL_EXT_texture_buffer_object;
  Read_GL_EXT_texture_integer;
  Read_GL_EXT_transform_feedback;
  Read_GL_EXT_direct_state_access;
  Read_GL_HP_image_transform;
  Read_GL_IBM_multimode_draw_arrays;
  Read_GL_IBM_vertex_array_lists;
  Read_GL_INGR_blend_func_separate;
  Read_GL_INTEL_parallel_arrays;
  Read_GL_MESA_resize_buffers;
  Read_GL_MESA_window_pos;
  Read_GL_NV_evaluators;
  Read_GL_NV_fence;
  Read_GL_NV_fragment_program;
  Read_GL_NV_half_float;
  Read_GL_NV_occlusion_query;
  Read_GL_NV_pixel_data_range;
  Read_GL_NV_point_sprite;
  Read_GL_NV_primitive_restart;
  Read_GL_NV_register_combiners;
  Read_GL_NV_register_combiners2;
  Read_GL_NV_vertex_array_range;
  Read_GL_NV_vertex_program;
  Read_GL_NV_depth_buffer_float;
  Read_GL_NV_framebuffer_multisample_coverage;
  Read_GL_NV_geometry_program4;
  Read_GL_NV_gpu_program4;
  Read_GL_NV_parameter_buffer_object;
  Read_GL_NV_transform_feedback;
  Read_GL_NV_conditional_render;
  Read_GL_NV_explicit_multisample;
  Read_GL_NV_transform_feedback2;
  Read_GL_PGI_misc_hints;
  Read_GL_SGIS_detail_texture;
  Read_GL_SGIS_fog_function;
  Read_GL_SGIS_multisample;
  Read_GL_SGIS_pixel_texture;
  Read_GL_SGIS_point_parameters;
  Read_GL_SGIS_sharpen_texture;
  Read_GL_SGIS_texture4D;
  Read_GL_SGIS_texture_color_mask;
  Read_GL_SGIS_texture_filter4;
  Read_GL_SGIX_async;
  Read_GL_SGIX_flush_raster;
  Read_GL_SGIX_fragment_lighting;
  Read_GL_SGIX_framezoom;
  Read_GL_SGIX_igloo_interface;
  Read_GL_SGIX_instruments;
  Read_GL_SGIX_list_priority;
  Read_GL_SGIX_pixel_texture;
  Read_GL_SGIX_polynomial_ffd;
  Read_GL_SGIX_reference_plane;
  Read_GL_SGIX_sprite;
  Read_GL_SGIX_tag_sample_buffer;
  Read_GL_SGI_color_table;
  Read_GL_SUNX_constant_data;
  Read_GL_SUN_global_alpha;
  Read_GL_SUN_mesh_array;
  Read_GL_SUN_triangle_list;
  Read_GL_SUN_vertex;

{$IFDEF Win32}
  Read_WGL_ARB_buffer_region;
  Read_WGL_ARB_extensions_string;
  Read_WGL_ARB_make_current_read;
  Read_WGL_ARB_pbuffer;
  Read_WGL_ARB_pixel_format;
  Read_WGL_ARB_pixel_format_float;
  Read_WGL_ARB_render_texture;
  Read_WGL_ARB_create_context;
  Read_WGL_EXT_display_color_table;
  Read_WGL_EXT_extensions_string;
  Read_WGL_EXT_make_current_read;
  Read_WGL_EXT_pbuffer;
  Read_WGL_EXT_pixel_format;
  Read_WGL_EXT_swap_control;
  Read_WGL_I3D_digital_video_control;
  Read_WGL_I3D_gamma;
  Read_WGL_I3D_genlock;
  Read_WGL_I3D_image_buffer;
  Read_WGL_I3D_swap_frame_lock;
  Read_WGL_I3D_swap_frame_usage;
  Read_WGL_NV_vertex_array_range;
  Read_WGL_NV_present_video;
  Read_WGL_NV_video_out;
  Read_WGL_NV_swap_group;
  Read_WGL_NV_gpu_affinity;
  Read_WGL_OML_sync_control;

  Read_WIN_draw_range_elements;
  Read_WIN_swap_hint;
{$ENDIF}
  
  Read_GL_ARB_Shader_Objects;
  Read_GL_ARB_occlusion_query;

  ExtensionsRead := True;
end;

// =============================================================================
//  ReadCoreVersion
// =============================================================================

procedure ReadCoreVersion;
var
  Buffer: string;
  MajorVersion, MinorVersion: Integer;

  procedure TrimAndSplitVersionString(Buffer: string; var Max, Min: Integer);
    // Peels out the X.Y form from the given Buffer which must contain a version string like "text Minor.Major.Build text"
    // at least however "Major.Minor".
  var
    Separator: Integer;
  begin
    try
      // There must be at least one dot to separate major and minor version number.
      Separator := Pos('.', Buffer);
      // At least one number must be before and one after the dot.
      if (Separator > 1) and (Separator < Length(Buffer)) and (AnsiChar(Buffer[Separator - 1]) in ['0'..'9']) and
      (AnsiChar(Buffer[Separator + 1]) in ['0'..'9']) then
      begin
        // OK, it's a valid version string. Now remove unnecessary parts.
        Dec(Separator);
        // Find last non-numeric character before version number.
        while (Separator > 0) and (AnsiChar(Buffer[Separator]) in ['0'..'9']) do
          Dec(Separator);
        // Delete leading characters which do not belong to the version string.
        Delete(Buffer, 1, Separator);
        Separator := Pos('.', Buffer) + 1;
        // Find first non-numeric character after version number
        while (Separator <= Length(Buffer)) and (AnsiChar(Buffer[Separator]) in ['0'..'9']) do
          Inc(Separator);
        // delete trailing characters not belonging to the version string
        Delete(Buffer, Separator, 255);
        // Now translate the numbers.
        Separator := Pos('.', Buffer); // This is necessary because the buffer length might have changed.
        Max := StrToInt(Copy(Buffer, 1, Separator - 1));
        Min := StrToInt(Copy(Buffer, Separator + 1, 1));
      end
      else
        Abort;
    except
      Min := 0;
      Max := 0;
    end;
  end;


begin
  // determine version of implementation
  // GL
  Buffer := glGetString(GL_VERSION);
  TrimAndSplitVersionString(Buffer, MajorVersion, MinorVersion);

  GL_VERSION_1_0 := True;
  GL_VERSION_1_1 := False;
  GL_VERSION_1_2 := False;
  GL_VERSION_1_3 := False;
  GL_VERSION_1_4 := False;
  GL_VERSION_1_5 := False;
  GL_VERSION_2_0 := False;
  GL_VERSION_2_1 := False;
  GL_VERSION_3_0 := False;

  if MajorVersion = 1 then
  begin
    if MinorVersion >= 1 then
      GL_VERSION_1_1 := True;
    if MinorVersion >= 2 then
      GL_VERSION_1_2 := True;
    if MinorVersion >= 3 then
      GL_VERSION_1_3 := True;
    if MinorVersion >= 4 then
      GL_VERSION_1_4 := True;
    if MinorVersion >= 5 then
      GL_VERSION_1_5 := True;
  end;

  if MajorVersion >= 2 then
  begin
    GL_VERSION_1_1 := True;
    GL_VERSION_1_2 := True;
    GL_VERSION_1_3 := True;
    GL_VERSION_1_4 := True;
    GL_VERSION_1_5 := True;
    GL_VERSION_2_0 := True;

    if MinorVersion >= 1 then
      GL_VERSION_2_1 := True;
  end;

  if MajorVersion >= 3 then
  begin
    GL_VERSION_2_1 := True;
    GL_VERSION_3_0 := True;
  end;


  // GLU
  GLU_VERSION_1_1 := False;
  GLU_VERSION_1_2 := False;
  GLU_VERSION_1_3 := False;

  if Assigned(gluGetString) then begin
    Buffer := gluGetString(GLU_VERSION);

    TrimAndSplitVersionString(Buffer, Majorversion, MinorVersion);

    GLU_VERSION_1_1 := True;

    if MinorVersion >= 2 then
      GLU_VERSION_1_2 := True;

    if MinorVersion >= 3 then
      GLU_VERSION_1_3 := True;
  end;
end;


// =============================================================================
//  ReadImplementationProperties
// =============================================================================

procedure ReadImplementationProperties;
var
  Buffer: string;
begin
  ReadCoreVersion;

  // Check all extensions
  Buffer := Int_GetExtensionString;

  // === 3DFX ====================================================================
  GL_3DFX_multisample := Int_CheckExtension(Buffer, 'GL_3DFX_multisample');
  GL_3DFX_tbuffer := Int_CheckExtension(Buffer, 'GL_3DFX_tbuffer');
  GL_3DFX_texture_compression_FXT1 := Int_CheckExtension(Buffer, 'GL_3DFX_texture_compression_FXT1');

  // === APPLE ===================================================================
  GL_APPLE_client_storage := Int_CheckExtension(Buffer, 'GL_APPLE_client_storage');
  GL_APPLE_element_array := Int_CheckExtension(Buffer, 'GL_APPLE_element_array');
  GL_APPLE_fence := Int_CheckExtension(Buffer, 'GL_APPLE_fence');
  GL_APPLE_specular_vector := Int_CheckExtension(Buffer, 'GL_APPLE_specular_vector');
  GL_APPLE_transform_hint := Int_CheckExtension(Buffer, 'GL_APPLE_transform_hint');
  GL_APPLE_vertex_array_object := Int_CheckExtension(Buffer, 'GL_APPLE_vertex_array_object');
  GL_APPLE_vertex_array_range := Int_CheckExtension(Buffer, 'GL_APPLE_vertex_array_range');
  GL_APPLE_ycbcr_422 := Int_CheckExtension(Buffer, 'GL_APPLE_ycbcr_422');

  // === ARB =====================================================================
  GL_ARB_depth_texture := Int_CheckExtension(Buffer, 'GL_ARB_depth_texture');
  GL_ARB_fragment_program := Int_CheckExtension(Buffer, 'GL_ARB_fragment_program');
  GL_ARB_imaging := Int_CheckExtension(Buffer, 'GL_ARB_imaging');
  GL_ARB_matrix_palette := Int_CheckExtension(Buffer, 'GL_ARB_matrix_palette');
  GL_ARB_multisample := Int_CheckExtension(Buffer, 'GL_ARB_multisample');
  GL_ARB_multitexture := Int_CheckExtension(Buffer, 'GL_ARB_multitexture');
  GL_ARB_point_parameters := Int_CheckExtension(Buffer, 'GL_ARB_point_parameters');
  GL_ARB_shadow := Int_CheckExtension(Buffer, 'GL_ARB_shadow');
  GL_ARB_shadow_ambient := Int_CheckExtension(Buffer, 'GL_ARB_shadow_ambient');
  GL_ARB_texture_border_clamp := Int_CheckExtension(Buffer, 'GL_ARB_texture_border_clamp');
  GL_ARB_texture_compression := Int_CheckExtension(Buffer, 'GL_ARB_texture_compression');
  GL_ARB_texture_cube_map := Int_CheckExtension(Buffer, 'GL_ARB_texture_cube_map');
  GL_ARB_texture_env_add := Int_CheckExtension(Buffer, 'GL_ARB_texture_env_add');
  GL_ARB_texture_env_combine := Int_CheckExtension(Buffer, 'GL_ARB_texture_env_combine');
  GL_ARB_texture_env_crossbar := Int_CheckExtension(Buffer, 'GL_ARB_texture_env_crossbar');
  GL_ARB_texture_env_dot3 := Int_CheckExtension(Buffer, 'GL_ARB_texture_env_dot3');
  GL_ARB_texture_mirrored_repeat := Int_CheckExtension(Buffer, 'GL_ARB_texture_mirrored_repeat');
  GL_ARB_transpose_matrix := Int_CheckExtension(Buffer, 'GL_ARB_transpose_matrix');
  GL_ARB_vertex_blend := Int_CheckExtension(Buffer, 'GL_ARB_vertex_blend');
  GL_ARB_vertex_buffer_object := Int_CheckExtension(Buffer, 'GL_ARB_vertex_buffer_object');
  GL_ARB_vertex_program := Int_CheckExtension(Buffer, 'GL_ARB_vertex_program');
  GL_ARB_window_pos := Int_CheckExtension(Buffer, 'GL_ARB_window_pos');
  GL_ARB_shader_objects := Int_CheckExtension(Buffer, 'GL_ARB_shader_objects');
  GL_ARB_vertex_shader := Int_CheckExtension(Buffer, 'GL_ARB_vertex_shader');
  GL_ARB_fragment_shader := Int_CheckExtension(Buffer, 'GL_ARB_fragment_shader');
  GL_ARB_occlusion_query := Int_CheckExtension(Buffer, 'GL_ARB_occlusion_query');
  GL_ARB_shading_language_100 := Int_CheckExtension(Buffer, 'GL_ARB_shading_language_100');
  GL_ARB_point_sprite := Int_CheckExtension(Buffer, 'GL_ARB_point_sprite');
  GL_ARB_texture_non_power_of_two := Int_CheckExtension(Buffer, 'GL_ARB_texture_non_power_of_two');
  GL_ARB_fragment_program_shadow := Int_CheckExtension(Buffer, 'GL_ARB_fragment_program_shadow');
  GL_ARB_draw_buffers := Int_CheckExtension(Buffer, 'GL_ARB_draw_buffers');
  GL_ARB_texture_rectangle := Int_CheckExtension(Buffer, 'GL_ARB_texture_rectangle');
  GL_ARB_color_buffer_float := Int_CheckExtension(Buffer, 'GL_ARB_color_buffer_float');
  GL_ARB_half_float_pixel := Int_CheckExtension(Buffer, 'GL_ARB_half_float_pixel');
  GL_ARB_texture_float := Int_CheckExtension(Buffer, 'GL_ARB_texture_float');
  GL_ARB_pixel_buffer_object := Int_CheckExtension(Buffer, 'GL_ARB_pixel_buffer_object');
  GL_ARB_depth_buffer_float := Int_CheckExtension(Buffer, 'GL_ARB_depth_buffer_float');
  GL_ARB_draw_instanced := Int_CheckExtension(Buffer, 'GL_ARB_draw_instanced');
  GL_ARB_framebuffer_object := Int_CheckExtension(Buffer, 'GL_ARB_framebuffer_object');
  GL_ARB_framebuffer_sRGB := Int_CheckExtension(Buffer, 'GL_ARB_framebuffer_sRGB');
  GL_ARB_geometry_shader4 := Int_CheckExtension(Buffer, 'GL_ARB_geometry_shader4');
  GL_ARB_half_float_vertex := Int_CheckExtension(Buffer, 'GL_ARB_half_float_vertex');
  GL_ARB_instanced_arrays := Int_CheckExtension(Buffer, 'GL_ARB_instanced_arrays');
  GL_ARB_map_buffer_range := Int_CheckExtension(Buffer, 'GL_ARB_map_buffer_range');
  GL_ARB_texture_buffer_object := Int_CheckExtension(Buffer, 'GL_ARB_texture_buffer_object');
  GL_ARB_texture_compression_rgtc := Int_CheckExtension(Buffer, 'GL_ARB_texture_compression_rgtc');
  GL_ARB_texture_rg := Int_CheckExtension(Buffer, 'GL_ARB_texture_rg');
  GL_ARB_vertex_array_object := Int_CheckExtension(Buffer, 'GL_ARB_vertex_array_object');

  // === ATI =====================================================================
  GL_ATI_draw_buffers := Int_CheckExtension(Buffer, 'GL_ATI_draw_buffers');
  GL_ATI_element_array := Int_CheckExtension(Buffer, 'GL_ATI_element_array');
  GL_ATI_envmap_bumpmap := Int_CheckExtension(Buffer, 'GL_ATI_envmap_bumpmap');
  GL_ATI_fragment_shader := Int_CheckExtension(Buffer, 'GL_ATI_fragment_shader');
  GL_ATI_map_object_buffer := Int_CheckExtension(Buffer, 'GL_ATI_map_object_buffer');
  GL_ATI_pn_triangles := Int_CheckExtension(Buffer, 'GL_ATI_pn_triangles');
  GL_ATI_separate_stencil := Int_CheckExtension(Buffer, 'GL_ATI_separate_stencil');
  GL_ATI_text_fragment_shader := Int_CheckExtension(Buffer, 'GL_ATI_text_fragment_shader');
  GL_ATI_texture_env_combine3 := Int_CheckExtension(Buffer, 'GL_ATI_texture_env_combine3');
  GL_ATI_texture_float := Int_CheckExtension(Buffer, 'GL_ATI_texture_float');
  GL_ATI_texture_mirror_once := Int_CheckExtension(Buffer, 'GL_ATI_texture_mirror_once');
  GL_ATI_vertex_array_object := Int_CheckExtension(Buffer, 'GL_ATI_vertex_array_object');
  GL_ATI_vertex_attrib_array_object := Int_CheckExtension(Buffer, 'GL_ATI_vertex_attrib_array_object');
  GL_ATI_vertex_streams := Int_CheckExtension(Buffer, 'GL_ATI_vertex_streams');

  // === EXT =====================================================================
  GL_EXT_422_pixels := Int_CheckExtension(Buffer, 'GL_EXT_422_pixels');
  GL_EXT_abgr := Int_CheckExtension(Buffer, 'GL_EXT_abgr');
  GL_EXT_bgra := Int_CheckExtension(Buffer, 'GL_EXT_bgra');
  GL_EXT_blend_color := Int_CheckExtension(Buffer, 'GL_EXT_blend_color');
  GL_EXT_blend_func_separate := Int_CheckExtension(Buffer, 'GL_EXT_blend_func_separate');
  GL_EXT_blend_logic_op := Int_CheckExtension(Buffer, 'GL_EXT_blend_logic_op');
  GL_EXT_blend_minmax := Int_CheckExtension(Buffer, 'GL_EXT_blend_minmax');
  GL_EXT_blend_subtract := Int_CheckExtension(Buffer, 'GL_EXT_blend_subtract');
  GL_EXT_clip_volume_hint := Int_CheckExtension(Buffer, 'GL_EXT_clip_volume_hint');
  GL_EXT_cmyka := Int_CheckExtension(Buffer, 'GL_EXT_cmyka');
  GL_EXT_color_matrix := Int_CheckExtension(Buffer, 'GL_EXT_color_matrix');
  GL_EXT_color_subtable := Int_CheckExtension(Buffer, 'GL_EXT_color_subtable');
  GL_EXT_compiled_vertex_array := Int_CheckExtension(Buffer, 'GL_EXT_compiled_vertex_array');
  GL_EXT_convolution := Int_CheckExtension(Buffer, 'GL_EXT_convolution');
  GL_EXT_coordinate_frame := Int_CheckExtension(Buffer, 'GL_EXT_coordinate_frame');
  GL_EXT_copy_texture := Int_CheckExtension(Buffer, 'GL_EXT_copy_texture');
  GL_EXT_cull_vertex := Int_CheckExtension(Buffer, 'GL_EXT_cull_vertex');
  GL_EXT_draw_range_elements := Int_CheckExtension(Buffer, 'GL_EXT_draw_range_elements');
  GL_EXT_fog_coord := Int_CheckExtension(Buffer, 'GL_EXT_fog_coord');
  GL_EXT_framebuffer_object := Int_CheckExtension(Buffer, 'GL_EXT_framebuffer_object');
  GL_EXT_histogram := Int_CheckExtension(Buffer, 'GL_EXT_histogram');
  GL_EXT_index_array_formats := Int_CheckExtension(Buffer, 'GL_EXT_index_array_formats');
  GL_EXT_index_func := Int_CheckExtension(Buffer, 'GL_EXT_index_func');
  GL_EXT_index_material := Int_CheckExtension(Buffer, 'GL_EXT_index_material');
  GL_EXT_index_texture := Int_CheckExtension(Buffer, 'GL_EXT_index_texture');
  GL_EXT_light_texture := Int_CheckExtension(Buffer, 'GL_EXT_light_texture');
  GL_EXT_misc_attribute := Int_CheckExtension(Buffer, 'GL_EXT_misc_attribute');
  GL_EXT_multi_draw_arrays := Int_CheckExtension(Buffer, 'GL_EXT_multi_draw_arrays');
  GL_EXT_multisample := Int_CheckExtension(Buffer, 'GL_EXT_multisample');
  GL_EXT_packed_pixels := Int_CheckExtension(Buffer, 'GL_EXT_packed_pixels');
  GL_EXT_paletted_texture := Int_CheckExtension(Buffer, 'GL_EXT_paletted_texture');
  GL_EXT_pixel_transform := Int_CheckExtension(Buffer, 'GL_EXT_pixel_transform');
  GL_EXT_pixel_transform_color_table := Int_CheckExtension(Buffer, 'GL_EXT_pixel_transform_color_table');
  GL_EXT_point_parameters := Int_CheckExtension(Buffer, 'GL_EXT_point_parameters');
  GL_EXT_polygon_offset := Int_CheckExtension(Buffer, 'GL_EXT_polygon_offset');
  GL_EXT_rescale_normal := Int_CheckExtension(Buffer, 'GL_EXT_rescale_normal');
  GL_EXT_secondary_color := Int_CheckExtension(Buffer, 'GL_EXT_secondary_color');
  GL_EXT_separate_specular_color := Int_CheckExtension(Buffer, 'GL_EXT_separate_specular_color');
  GL_EXT_shadow_funcs := Int_CheckExtension(Buffer, 'GL_EXT_shadow_funcs');
  GL_EXT_shared_texture_palette := Int_CheckExtension(Buffer, 'GL_EXT_shared_texture_palette');
  GL_EXT_stencil_two_side := Int_CheckExtension(Buffer, 'GL_EXT_stencil_two_side');
  GL_EXT_stencil_wrap := Int_CheckExtension(Buffer, 'GL_EXT_stencil_wrap');
  GL_EXT_subtexture := Int_CheckExtension(Buffer, 'GL_EXT_subtexture');
  GL_EXT_texture := Int_CheckExtension(Buffer, 'GL_EXT_texture');
  GL_EXT_texture3D := Int_CheckExtension(Buffer, 'GL_EXT_texture3D');
  GL_EXT_texture_compression_s3tc := Int_CheckExtension(Buffer, 'GL_EXT_texture_compression_s3tc');
  GL_EXT_texture_cube_map := Int_CheckExtension(Buffer, 'GL_EXT_texture_cube_map');
  GL_EXT_texture_edge_clamp := Int_CheckExtension(Buffer, 'GL_EXT_texture_edge_clamp');
  GL_EXT_texture_env_add := Int_CheckExtension(Buffer, 'GL_EXT_texture_env_add');
  GL_EXT_texture_env_combine := Int_CheckExtension(Buffer, 'GL_EXT_texture_env_combine');
  GL_EXT_texture_env_dot3 := Int_CheckExtension(Buffer, 'GL_EXT_texture_env_dot3');
  GL_EXT_texture_filter_anisotropic := Int_CheckExtension(Buffer, 'GL_EXT_texture_filter_anisotropic');
  GL_EXT_texture_lod_bias := Int_CheckExtension(Buffer, 'GL_EXT_texture_lod_bias');
  GL_EXT_texture_object := Int_CheckExtension(Buffer, 'GL_EXT_texture_object');
  GL_EXT_texture_perturb_normal := Int_CheckExtension(Buffer, 'GL_EXT_texture_perturb_normal');
  GL_EXT_texture_rectangle := Int_CheckExtension(Buffer, 'GL_EXT_texture_rectangle');
  GL_EXT_vertex_array := Int_CheckExtension(Buffer, 'GL_EXT_vertex_array');
  GL_EXT_vertex_shader := Int_CheckExtension(Buffer, 'GL_EXT_vertex_shader');
  GL_EXT_vertex_weighting := Int_CheckExtension(Buffer, 'GL_EXT_vertex_weighting');
  GL_EXT_depth_bounds_test := Int_CheckExtension(Buffer, 'GL_EXT_depth_bounds_test');
  GL_EXT_texture_mirror_clamp := Int_CheckExtension(Buffer, 'GL_EXT_texture_mirror_clamp');
  GL_EXT_blend_equation_separate := Int_CheckExtension(Buffer, 'GL_EXT_blend_equation_separate');
  GL_EXT_pixel_buffer_object := Int_CheckExtension(Buffer, 'GL_EXT_pixel_buffer_object');
  GL_EXT_texture_compression_dxt1 := Int_CheckExtension(Buffer, 'GL_EXT_texture_compression_dxt1');
  GL_EXT_stencil_clear_tag := Int_CheckExtension(Buffer, 'GL_EXT_stencil_clear_tag');
  GL_EXT_packed_depth_stencil := Int_CheckExtension(Buffer, 'GL_EXT_packed_depth_stencil');
  GL_EXT_texture_sRGB := Int_CheckExtension(Buffer, 'GL_EXT_texture_sRGB');
  GL_EXT_framebuffer_blit := Int_CheckExtension(Buffer, 'GL_EXT_framebuffer_blit');
  GL_EXT_framebuffer_multisample := Int_CheckExtension(Buffer, 'GL_EXT_framebuffer_multisample');
  GL_EXT_timer_query := Int_CheckExtension(Buffer, 'GL_EXT_timer_query');
  GL_EXT_gpu_program_parameters := Int_CheckExtension(Buffer, 'GL_EXT_gpu_program_parameters');
  GL_EXT_bindable_uniform := Int_CheckExtension(Buffer, 'GL_EXT_bindable_uniform');
  GL_EXT_draw_buffers2 := Int_CheckExtension(Buffer, 'GL_EXT_draw_buffers2');
  GL_EXT_draw_instanced := Int_CheckExtension(Buffer, 'GL_EXT_draw_instanced');
  GL_EXT_framebuffer_sRGB := Int_CheckExtension(Buffer, 'GL_EXT_framebuffer_sRGB');
  GL_EXT_geometry_shader4 := Int_CheckExtension(Buffer, 'GL_EXT_geometry_shader4');
  GL_EXT_gpu_shader4 := Int_CheckExtension(Buffer, 'GL_EXT_gpu_shader4');
  GL_EXT_packed_float := Int_CheckExtension(Buffer, 'GL_EXT_packed_float');
  GL_EXT_texture_array := Int_CheckExtension(Buffer, 'GL_EXT_texture_array');
  GL_EXT_texture_buffer_object := Int_CheckExtension(Buffer, 'GL_EXT_texture_buffer_object');
  GL_EXT_texture_compression_latc := Int_CheckExtension(Buffer, 'GL_EXT_texture_compression_latc');
  GL_EXT_texture_compression_rgtc := Int_CheckExtension(Buffer, 'GL_EXT_texture_compression_rgtc');
  GL_EXT_texture_integer := Int_CheckExtension(Buffer, 'GL_EXT_texture_integer');
  GL_EXT_texture_shared_exponent := Int_CheckExtension(Buffer, 'GL_EXT_texture_shared_exponent');
  GL_EXT_transform_feedback := Int_CheckExtension(Buffer, 'GL_EXT_transform_feedback');
  GL_EXT_direct_state_access := Int_CheckExtension(Buffer, 'GL_EXT_direct_state_access');
  GL_EXT_vertex_array_bgra := Int_CheckExtension(Buffer, 'GL_EXT_vertex_array_bgra');
  GL_EXT_texture_swizzle := Int_CheckExtension(Buffer, 'GL_EXT_texture_swizzle');

  // === HP ======================================================================
  GL_HP_convolution_border_modes := Int_CheckExtension(Buffer, 'GL_HP_convolution_border_modes');
  GL_HP_image_transform := Int_CheckExtension(Buffer, 'GL_HP_image_transform');
  GL_HP_occlusion_test := Int_CheckExtension(Buffer, 'GL_HP_occlusion_test');
  GL_HP_texture_lighting := Int_CheckExtension(Buffer, 'GL_HP_texture_lighting');

  // === IBM =====================================================================
  GL_IBM_cull_vertex := Int_CheckExtension(Buffer, 'GL_IBM_cull_vertex');
  GL_IBM_multimode_draw_arrays := Int_CheckExtension(Buffer, 'GL_IBM_multimode_draw_arrays');
  GL_IBM_rasterpos_clip := Int_CheckExtension(Buffer, 'GL_IBM_rasterpos_clip');
  GL_IBM_texture_mirrored_repeat := Int_CheckExtension(Buffer, 'GL_IBM_texture_mirrored_repeat');
  GL_IBM_vertex_array_lists := Int_CheckExtension(Buffer, 'GL_IBM_vertex_array_lists');

  // === INGR ====================================================================
  GL_INGR_blend_func_separate := Int_CheckExtension(Buffer, 'GL_INGR_blend_func_separate');
  GL_INGR_color_clamp := Int_CheckExtension(Buffer, 'GL_INGR_color_clamp');
  GL_INGR_interlace_read := Int_CheckExtension(Buffer, 'GL_INGR_interlace_read');
  GL_INGR_palette_buffer := Int_CheckExtension(Buffer, 'GL_INGR_palette_buffer');

  // === INTEL ===================================================================
  GL_INTEL_parallel_arrays := Int_CheckExtension(Buffer, 'GL_INTEL_parallel_arrays');
  GL_INTEL_texture_scissor := Int_CheckExtension(Buffer, 'GL_INTEL_texture_scissor');

  // === MESA ====================================================================
  GL_MESA_resize_buffers := Int_CheckExtension(Buffer, 'GL_MESA_resize_buffers');
  GL_MESA_window_pos := Int_CheckExtension(Buffer, 'GL_MESA_window_pos');

  // === NVIDIA ==================================================================
  GL_NV_blend_square := Int_CheckExtension(Buffer, 'GL_NV_blend_square');
  GL_NV_copy_depth_to_color := Int_CheckExtension(Buffer, 'GL_NV_copy_depth_to_color');
  GL_NV_depth_clamp := Int_CheckExtension(Buffer, 'GL_NV_depth_clamp');
  GL_NV_evaluators := Int_CheckExtension(Buffer, 'GL_NV_evaluators');
  GL_NV_fence := Int_CheckExtension(Buffer, 'GL_NV_fence');
  GL_NV_float_buffer := Int_CheckExtension(Buffer, 'GL_NV_float_buffer');
  GL_NV_fog_distance := Int_CheckExtension(Buffer, 'GL_NV_fog_distance');
  GL_NV_fragment_program := Int_CheckExtension(Buffer, 'GL_NV_fragment_program');
  GL_NV_half_float := Int_CheckExtension(Buffer, 'GL_NV_half_float');
  GL_NV_light_max_exponent := Int_CheckExtension(Buffer, 'GL_NV_light_max_exponent');
  GL_NV_multisample_filter_hint := Int_CheckExtension(Buffer, 'GL_NV_multisample_filter_hint');
  GL_NV_occlusion_query := Int_CheckExtension(Buffer, 'GL_NV_occlusion_query');
  GL_NV_packed_depth_stencil := Int_CheckExtension(Buffer, 'GL_NV_packed_depth_stencil');
  GL_NV_pixel_data_range := Int_CheckExtension(Buffer, 'GL_NV_pixel_data_range');
  GL_NV_point_sprite := Int_CheckExtension(Buffer, 'GL_NV_point_sprite');
  GL_NV_primitive_restart := Int_CheckExtension(Buffer, 'GL_NV_primitive_restart');
  GL_NV_register_combiners := Int_CheckExtension(Buffer, 'GL_NV_register_combiners');
  GL_NV_register_combiners2 := Int_CheckExtension(Buffer, 'GL_NV_register_combiners2');
  GL_NV_texgen_emboss := Int_CheckExtension(Buffer, 'GL_NV_texgen_emboss');
  GL_NV_texgen_reflection := Int_CheckExtension(Buffer, 'GL_NV_texgen_reflection');
  GL_NV_texture_compression_vtc := Int_CheckExtension(Buffer, 'GL_NV_texture_compression_vtc');
  GL_NV_texture_env_combine4 := Int_CheckExtension(Buffer, 'GL_NV_texture_env_combine4');
  GL_NV_texture_expand_normal := Int_CheckExtension(Buffer, 'GL_NV_texture_expand_normal');
  GL_NV_texture_rectangle := Int_CheckExtension(Buffer, 'GL_NV_texture_rectangle');
  GL_NV_texture_shader := Int_CheckExtension(Buffer, 'GL_NV_texture_shader');
  GL_NV_texture_shader2 := Int_CheckExtension(Buffer, 'GL_NV_texture_shader2');
  GL_NV_texture_shader3 := Int_CheckExtension(Buffer, 'GL_NV_texture_shader3');
  GL_NV_vertex_array_range := Int_CheckExtension(Buffer, 'GL_NV_vertex_array_range');
  GL_NV_vertex_array_range2 := Int_CheckExtension(Buffer, 'GL_NV_vertex_array_range2');
  GL_NV_vertex_program := Int_CheckExtension(Buffer, 'GL_NV_vertex_program');
  GL_NV_vertex_program1_1 := Int_CheckExtension(Buffer, 'GL_NV_vertex_program1_1');
  GL_NV_vertex_program2 := Int_CheckExtension(Buffer, 'GL_NV_vertex_program2');
  GL_NV_fragment_program_option := Int_CheckExtension(Buffer, 'GL_NV_fragment_program_option');
  GL_NV_fragment_program2 := Int_CheckExtension(Buffer, 'GL_NV_fragment_program2');
  GL_NV_vertex_program2_option := Int_CheckExtension(Buffer, 'GL_NV_vertex_program2_option');
  GL_NV_vertex_program3 := Int_CheckExtension(Buffer, 'GL_NV_vertex_program3');
  GL_NV_depth_buffer_float := Int_CheckExtension(Buffer, 'GL_NV_depth_buffer_float');
  GL_NV_fragment_program4 := Int_CheckExtension(Buffer, 'GL_NV_fragment_program4');
  GL_NV_framebuffer_multisample_coverage := Int_CheckExtension(Buffer, 'GL_NV_framebuffer_multisample_coverage');
  GL_NV_geometry_program4 := Int_CheckExtension(Buffer, 'GL_NV_geometry_program4');
  GL_NV_gpu_program4 := Int_CheckExtension(Buffer, 'GL_NV_gpu_program4');
  GL_NV_parameter_buffer_object := Int_CheckExtension(Buffer, 'GL_NV_parameter_buffer_object');
  GL_NV_transform_feedback := Int_CheckExtension(Buffer, 'GL_NV_transform_feedback');
  GL_NV_vertex_program4 := Int_CheckExtension(Buffer, 'GL_NV_vertex_program4');
  GL_NV_conditional_render := Int_CheckExtension(Buffer, 'GL_NV_conditional_render');
  GL_NV_present_video := Int_CheckExtension(Buffer, 'GL_NV_present_video');
  GL_NV_explicit_multisample := Int_CheckExtension(Buffer, 'GL_NV_explicit_multisample');
  GL_NV_transform_feedback2 := Int_CheckExtension(Buffer, 'GL_NV_transform_feedback2');

  // === OML =====================================================================
  GL_OML_interlace := Int_CheckExtension(Buffer, 'GL_OML_interlace');
  GL_OML_resample := Int_CheckExtension(Buffer, 'GL_OML_resample');
  GL_OML_subsample := Int_CheckExtension(Buffer, 'GL_OML_subsample');

  // === PGI =====================================================================
  GL_PGI_misc_hints := Int_CheckExtension(Buffer, 'GL_PGI_misc_hints');
  GL_PGI_vertex_hints := Int_CheckExtension(Buffer, 'GL_PGI_vertex_hints');

  // === REND ====================================================================
  GL_REND_screen_coordinates := Int_CheckExtension(Buffer, 'GL_REND_screen_coordinates');

  // === S3 ======================================================================
  GL_S3_s3tc := Int_CheckExtension(Buffer, 'GL_S3_s3tc');

  // === SGIS ====================================================================
  GL_SGIS_detail_texture := Int_CheckExtension(Buffer, 'GL_SGIS_detail_texture');
  GL_SGIS_fog_function := Int_CheckExtension(Buffer, 'GL_SGIS_fog_function');
  GL_SGIS_generate_mipmap := Int_CheckExtension(Buffer, 'GL_SGIS_generate_mipmap');
  GL_SGIS_multisample := Int_CheckExtension(Buffer, 'GL_SGIS_multisample');
  GL_SGIS_pixel_texture := Int_CheckExtension(Buffer, 'GL_SGIS_pixel_texture');
  GL_SGIS_point_line_texgen := Int_CheckExtension(Buffer, 'GL_SGIS_point_line_texgen');
  GL_SGIS_point_parameters := Int_CheckExtension(Buffer, 'GL_SGIS_point_parameters');
  GL_SGIS_sharpen_texture := Int_CheckExtension(Buffer, 'GL_SGIS_sharpen_texture');
  GL_SGIS_texture4D := Int_CheckExtension(Buffer, 'GL_SGIS_texture4D');
  GL_SGIS_texture_border_clamp := Int_CheckExtension(Buffer, 'GL_SGIS_texture_border_clamp');
  GL_SGIS_texture_color_mask := Int_CheckExtension(Buffer, 'GL_SGIS_texture_color_mask');
  GL_SGIS_texture_edge_clamp := Int_CheckExtension(Buffer, 'GL_SGIS_texture_edge_clamp');
  GL_SGIS_texture_filter4 := Int_CheckExtension(Buffer, 'GL_SGIS_texture_filter4');
  GL_SGIS_texture_lod := Int_CheckExtension(Buffer, 'GL_SGIS_texture_lod');
  GL_SGIS_texture_select := Int_CheckExtension(Buffer, 'GL_SGIS_texture_select');

  // === SGIX ====================================================================
  GL_FfdMaskSGIX := Int_CheckExtension(Buffer, 'GL_FfdMaskSGIX');
  GL_SGIX_async := Int_CheckExtension(Buffer, 'GL_SGIX_async');
  GL_SGIX_async_histogram := Int_CheckExtension(Buffer, 'GL_SGIX_async_histogram');
  GL_SGIX_async_pixel := Int_CheckExtension(Buffer, 'GL_SGIX_async_pixel');
  GL_SGIX_blend_alpha_minmax := Int_CheckExtension(Buffer, 'GL_SGIX_blend_alpha_minmax');
  GL_SGIX_calligraphic_fragment := Int_CheckExtension(Buffer, 'GL_SGIX_calligraphic_fragment');
  GL_SGIX_clipmap := Int_CheckExtension(Buffer, 'GL_SGIX_clipmap');
  GL_SGIX_convolution_accuracy := Int_CheckExtension(Buffer, 'GL_SGIX_convolution_accuracy');
  GL_SGIX_depth_pass_instrument := Int_CheckExtension(Buffer, 'GL_SGIX_depth_pass_instrument');
  GL_SGIX_depth_texture := Int_CheckExtension(Buffer, 'GL_SGIX_depth_texture');
  GL_SGIX_flush_raster := Int_CheckExtension(Buffer, 'GL_SGIX_flush_raster');
  GL_SGIX_fog_offset := Int_CheckExtension(Buffer, 'GL_SGIX_fog_offset');
  GL_SGIX_fog_scale := Int_CheckExtension(Buffer, 'GL_SGIX_fog_scale');
  GL_SGIX_fragment_lighting := Int_CheckExtension(Buffer, 'GL_SGIX_fragment_lighting');
  GL_SGIX_framezoom := Int_CheckExtension(Buffer, 'GL_SGIX_framezoom');
  GL_SGIX_igloo_interface := Int_CheckExtension(Buffer, 'GL_SGIX_igloo_interface');
  GL_SGIX_impact_pixel_texture := Int_CheckExtension(Buffer, 'GL_SGIX_impact_pixel_texture');
  GL_SGIX_instruments := Int_CheckExtension(Buffer, 'GL_SGIX_instruments');
  GL_SGIX_interlace := Int_CheckExtension(Buffer, 'GL_SGIX_interlace');
  GL_SGIX_ir_instrument1 := Int_CheckExtension(Buffer, 'GL_SGIX_ir_instrument1');
  GL_SGIX_list_priority := Int_CheckExtension(Buffer, 'GL_SGIX_list_priority');
  GL_SGIX_pixel_texture := Int_CheckExtension(Buffer, 'GL_SGIX_pixel_texture');
  GL_SGIX_pixel_tiles := Int_CheckExtension(Buffer, 'GL_SGIX_pixel_tiles');
  GL_SGIX_polynomial_ffd := Int_CheckExtension(Buffer, 'GL_SGIX_polynomial_ffd');
  GL_SGIX_reference_plane := Int_CheckExtension(Buffer, 'GL_SGIX_reference_plane');
  GL_SGIX_resample := Int_CheckExtension(Buffer, 'GL_SGIX_resample');
  GL_SGIX_scalebias_hint := Int_CheckExtension(Buffer, 'GL_SGIX_scalebias_hint');
  GL_SGIX_shadow := Int_CheckExtension(Buffer, 'GL_SGIX_shadow');
  GL_SGIX_shadow_ambient := Int_CheckExtension(Buffer, 'GL_SGIX_shadow_ambient');
  GL_SGIX_sprite := Int_CheckExtension(Buffer, 'GL_SGIX_sprite');
  GL_SGIX_subsample := Int_CheckExtension(Buffer, 'GL_SGIX_subsample');
  GL_SGIX_tag_sample_buffer := Int_CheckExtension(Buffer, 'GL_SGIX_tag_sample_buffer');
  GL_SGIX_texture_add_env := Int_CheckExtension(Buffer, 'GL_SGIX_texture_add_env');
  GL_SGIX_texture_coordinate_clamp := Int_CheckExtension(Buffer, 'GL_SGIX_texture_coordinate_clamp');
  GL_SGIX_texture_lod_bias := Int_CheckExtension(Buffer, 'GL_SGIX_texture_lod_bias');
  GL_SGIX_texture_multi_buffer := Int_CheckExtension(Buffer, 'GL_SGIX_texture_multi_buffer');
  GL_SGIX_texture_scale_bias := Int_CheckExtension(Buffer, 'GL_SGIX_texture_scale_bias');
  GL_SGIX_texture_select := Int_CheckExtension(Buffer, 'GL_SGIX_texture_select');
  GL_SGIX_vertex_preclip := Int_CheckExtension(Buffer, 'GL_SGIX_vertex_preclip');
  GL_SGIX_ycrcb := Int_CheckExtension(Buffer, 'GL_SGIX_ycrcb');
  GL_SGIX_ycrcb_subsample := Int_CheckExtension(Buffer, 'GL_SGIX_ycrcb_subsample');
  GL_SGIX_ycrcba := Int_CheckExtension(Buffer, 'GL_SGIX_ycrcba');

  // === SGI =====================================================================
  GL_SGI_color_matrix := Int_CheckExtension(Buffer, 'GL_SGI_color_matrix');
  GL_SGI_color_table := Int_CheckExtension(Buffer, 'GL_SGI_color_table');
  GL_SGI_depth_pass_instrument := Int_CheckExtension(Buffer, 'GL_SGI_depth_pass_instrument');
  GL_SGI_texture_color_table := Int_CheckExtension(Buffer, 'GL_SGI_texture_color_table');

  // === SUN =====================================================================
  GL_SUNX_constant_data := Int_CheckExtension(Buffer, 'GL_SUNX_constant_data');
  GL_SUN_convolution_border_modes := Int_CheckExtension(Buffer, 'GL_SUN_convolution_border_modes');
  GL_SUN_global_alpha := Int_CheckExtension(Buffer, 'GL_SUN_global_alpha');
  GL_SUN_mesh_array := Int_CheckExtension(Buffer, 'GL_SUN_mesh_array');
  GL_SUN_slice_accum := Int_CheckExtension(Buffer, 'GL_SUN_slice_accum');
  GL_SUN_triangle_list := Int_CheckExtension(Buffer, 'GL_SUN_triangle_list');
  GL_SUN_vertex := Int_CheckExtension(Buffer, 'GL_SUN_vertex');

  // === WIN =====================================================================
  GL_WIN_phong_shading := Int_CheckExtension(Buffer, 'GL_WIN_phong_shading');
  GL_WIN_specular_fog := Int_CheckExtension(Buffer, 'GL_WIN_specular_fog');

  {$IFDEF WIN32}
  // === WGL =====================================================================
  WGL_3DFX_multisample := Int_CheckExtension(Buffer, 'WGL_3DFX_multisample');
  WGL_ARB_buffer_region := Int_CheckExtension(Buffer, 'WGL_ARB_buffer_region');
  WGL_ARB_extensions_string := Int_CheckExtension(Buffer, 'WGL_ARB_extensions_string');
  WGL_ARB_make_current_read := Int_CheckExtension(Buffer, 'WGL_ARB_make_current_read');
  WGL_ARB_multisample := Int_CheckExtension(Buffer, 'WGL_ARB_multisample');
  WGL_ARB_pbuffer := Int_CheckExtension(Buffer, 'WGL_ARB_pbuffer');
  WGL_ARB_pixel_format := Int_CheckExtension(Buffer, 'WGL_ARB_pixel_format');
  WGL_ARB_pixel_format_float := Int_CheckExtension(Buffer, 'WGL_ARB_pixel_format_float');
  WGL_ARB_render_texture := Int_CheckExtension(Buffer, 'WGL_ARB_render_texture');
  WGL_ARB_create_context := Int_CheckExtension(Buffer, 'WGL_ARB_create_context');
  WGL_ATI_pixel_format_float := Int_CheckExtension(Buffer, 'WGL_ATI_pixel_format_float');
  WGL_EXT_depth_float := Int_CheckExtension(Buffer, 'WGL_EXT_depth_float');
  WGL_EXT_display_color_table := Int_CheckExtension(Buffer, 'WGL_EXT_display_color_table');
  WGL_EXT_extensions_string := Int_CheckExtension(Buffer, 'WGL_EXT_extensions_string');
  WGL_EXT_make_current_read := Int_CheckExtension(Buffer, 'WGL_EXT_make_current_read');
  WGL_EXT_multisample := Int_CheckExtension(Buffer, 'WGL_EXT_multisample');
  WGL_EXT_pbuffer := Int_CheckExtension(Buffer, 'WGL_EXT_pbuffer');
  WGL_EXT_pixel_format := Int_CheckExtension(Buffer, 'WGL_EXT_pixel_format');
  WGL_EXT_swap_control := Int_CheckExtension(Buffer, 'WGL_EXT_swap_control');
  WGL_I3D_digital_video_control := Int_CheckExtension(Buffer, 'WGL_I3D_digital_video_control');
  WGL_I3D_gamma := Int_CheckExtension(Buffer, 'WGL_I3D_gamma');
  WGL_I3D_genlock := Int_CheckExtension(Buffer, 'WGL_I3D_genlock');
  WGL_I3D_image_buffer := Int_CheckExtension(Buffer, 'WGL_I3D_image_buffer');
  WGL_I3D_swap_frame_lock := Int_CheckExtension(Buffer, 'WGL_I3D_swap_frame_lock');
  WGL_I3D_swap_frame_usage := Int_CheckExtension(Buffer, 'WGL_I3D_swap_frame_usage');
  WGL_NV_float_buffer := Int_CheckExtension(Buffer, 'WGL_NV_float_buffer');
  WGL_NV_render_depth_texture := Int_CheckExtension(Buffer, 'WGL_NV_render_depth_texture');
  WGL_NV_render_texture_rectangle := Int_CheckExtension(Buffer, 'WGL_NV_render_texture_rectangle');
  WGL_NV_vertex_array_range := Int_CheckExtension(Buffer, 'WGL_NV_vertex_array_range');
  WGL_NV_present_video := Int_CheckExtension(Buffer, 'WGL_NV_present_video');
  WGL_NV_video_out := Int_CheckExtension(Buffer, 'WGL_NV_video_out');
  WGL_NV_swap_group := Int_CheckExtension(Buffer, 'WGL_NV_swap_group');
  WGL_NV_gpu_affinity := Int_CheckExtension(Buffer, 'WGL_NV_gpu_affinity');
  WGL_OML_sync_control := Int_CheckExtension(Buffer, 'WGL_OML_sync_control');

  WIN_draw_range_elements := Int_CheckExtension(Buffer, 'WIN_draw_range_elements');
  WIN_swap_hint := Int_CheckExtension(Buffer, 'WIN_swap_hint');
  {$ENDIF}

  ImplementationRead := True;
end;

{$IFDEF WIN32}
// =============================================================================
// RaiseLastOSError
// =============================================================================
// Needed for compatibility with older Delphiversions
// =============================================================================

procedure RaiseLastOSError;
begin
{$IFDEF FPC}
  raise Exception.Create('RaiseLastOSError!'); // To-Do: find a better solution
{$ELSE}
  {$IFDEF DELPHI6_UP} // If Delphi 6 or later
    SysUtils.RaiseLastOSError;
  {$ELSE}
    SysUtils.RaiseLastWin32Error;
  {$ENDIF}
{$ENDIF}
end;

// =============================================================================
// CreateRenderingContext
// =============================================================================

function CreateRenderingContext(DC: HDC; Options: TRCOptions; ColorBits, ZBits, StencilBits, AccumBits, AuxBuffers: Integer; Layer: Integer): HGLRC;
const
  OBJ_MEMDC = 10;
  OBJ_ENHMETADC = 12;
  OBJ_METADC = 4;
  PFD_DOUBLEBUFFER = $00000001;
  PFD_STEREO = $00000002;
  PFD_DRAW_TO_WINDOW = $00000004;
  PFD_DRAW_TO_BITMAP = $00000008;
  PFD_SUPPORT_GDI = $00000010;
  PFD_SUPPORT_OPENGL = $00000020;
  PFD_TYPE_RGBA = 0;
  PFD_MAIN_PLANE = 0;
  PFD_OVERLAY_PLANE = 1;
  PFD_UNDERLAY_PLANE = LongWord(-1);
  MemoryDCs = [OBJ_MEMDC, OBJ_METADC, OBJ_ENHMETADC];
var
  PFDescriptor: TPixelFormatDescriptor;
  PixelFormat: Integer;
  AType: DWORD;
begin
  if LibHandle = 0 then
    InitOpenGL;

  FillChar(PFDescriptor, SizeOf(PFDescriptor), 0);

  with PFDescriptor do
  begin
    nSize := SizeOf(PFDescriptor);
    nVersion := 1;
    dwFlags := PFD_SUPPORT_OPENGL;

    AType := GetObjectType(DC);

    if AType = 0 then
      RaiseLastOSError;

    if AType in MemoryDCs then
      dwFlags := dwFlags or PFD_DRAW_TO_BITMAP
    else
      dwFlags := dwFlags or PFD_DRAW_TO_WINDOW;

    if opDoubleBuffered in Options then
      dwFlags := dwFlags or PFD_DOUBLEBUFFER;

    if opGDI in Options then
      dwFlags := dwFlags or PFD_SUPPORT_GDI;

    if opStereo in Options then
      dwFlags := dwFlags or PFD_STEREO;

    iPixelType := PFD_TYPE_RGBA;
    cColorBits := ColorBits;
    cDepthBits := zBits;
    cStencilBits := StencilBits;
    cAccumBits := AccumBits;
    cAuxBuffers := AuxBuffers;

    if Layer = 0 then
      iLayerType := PFD_MAIN_PLANE
    else
    if Layer > 0 then
      iLayerType := PFD_OVERLAY_PLANE
    else
      iLayerType := Byte(PFD_UNDERLAY_PLANE);
  end;

  PixelFormat := ChoosePixelFormat(DC, @PFDescriptor);

  if PixelFormat = 0 then
    RaiseLastOSError;

  if GetPixelFormat(DC) <> PixelFormat then
    if not SetPixelFormat(DC, PixelFormat, @PFDescriptor) then
      RaiseLastOSError;

  DescribePixelFormat(DC, PixelFormat, SizeOf(PFDescriptor), PFDescriptor);

  Result := wglCreateContext(DC);

  if Result = 0 then
    RaiseLastOSError
  else
    LastPixelFormat := 0;
end;

// =============================================================================
// DestroyRenderingContext
// =============================================================================

procedure DestroyRenderingContext(RC: HGLRC);
begin
  wglDeleteContext(RC);
end;


// =============================================================================
// ActivateRenderingContext
// =============================================================================

procedure ActivateRenderingContext(DC: HDC; RC: HGLRC; loadext: boolean = true);
begin
  Assert((DC <> 0), 'DC must not be 0');
  Assert((RC <> 0), 'RC must not be 0');

  wglMakeCurrent(DC, RC);

  {$ifdef DGL_TINY_HEADER}
  ReadCoreVersion;
  {$else}
  ReadImplementationProperties;

  if (loadext) then
    ReadExtensions;
  {$endif}
end;

// =============================================================================
// DeactivateRenderingContext
// =============================================================================

procedure DeactivateRenderingContext;
begin
  wglMakeCurrent(0, 0);
end;
{$ENDIF}

{$IFDEF FPC}
  {$IFDEF CPU386}

const
  Default8087CW: Word = $1332;

procedure Set8087CW(NewCW: Word); assembler;
asm
  MOV Default8087CW, AX
end;

  {$ENDIF}
{$ENDIF}

initialization

{$IFDEF CPU386}
  Set8087CW($133F);
{$ENDIF}

finalization

end.

