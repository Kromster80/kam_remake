{==============================================================================}
{                                                                              }
{       OpenGL1.5 - Headertranslation (includes GL1.1-1.5)                     }
{       Version 1.3c                                                           }
{       Date : 12.02.2004                                                      }
{                                                                              }
{==============================================================================}
{                                                                              }
{       Containts the translations of glext.h, gl_1_1.h, glu.h and weglext.h.  }
{       It also contains some helperfunctions that were inspired by those      }
{       found in Mike Lischke's OpenGL12.pas.                                  }
{                                                                              }
{       Also contains the new OpenGL1.5-Extensions :                           }
{         - GL_ARB_occlusion_query                                             }
{         - GL_ARB_shader_objects                                              }
{         - GL_ARB_vertex_shader                                               }
{         - GL_ARB_fragment_shader                                             }
{         - GL_ARB_shading_language_100                                        }
{         - GL_ARB_texture_non_power_of_two                                    }
{         - GL_ARB_point_sprite                                                }
{                                                                              }
{                                                                              }
{       Copyright (C) DGL-OpenGL2-Portteam                                     }
{       All Rights Reserved                                                    }
{                                                                              }
{       Obtained through:                                                      }
{       Delphi OpenGL Community(DGL) - www.delphigl.com                        }
{                                                                              }
{       Converted and maintained by DGL's GL2.0-Team :                         }
{         - Sascha Willems (Son of Satan) - http://www.delphigl.de             }
{         - Steffen Xonna (LossyEx)       - http://www.dev-center.de           }
{         - Lars Middendorf               - http://www.3d-seite.de             }
{       Additional input :                                                     }
{         - Martin Waldegger (Mars)       - http://www.basegraph.com           }
{                                                                              }
{==============================================================================}
{      If you have problems on using our unit, then please take a look at      }
{      the "how to use.txt".                                                   }
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
{ Version 1.0  - Initial Release                                               }
{ Version 1.1  - Added PPointer in Tpyessection for compatiblity with Delphi   }
{                versions lower than 7                                    (SW) }
{              - Added a function named RaiseLastOSError including a comment   }
{                on how to make it run under Delphi versions lower than 7 (SW) }
{              - Added some data types according to the GL-Syntax         (SW) }
{ Version 1.2  - Fixed some problems with getting the addresses of some        }
{                Extensions (e.g. glTexImage3D) where the EXT/ARB did work     }
{                but not the core-functions                               (SW) }
{ Version 1.3  - A second call to ReadimplementationProperties won't           }
{                revert to the default libs anymore                       (MW) }
{              - Libraries now will be released if necessary              (MW) }
{ Version 1.3a - Small fixes for glSlang-functions                        (SW) }
{ Version 1.3b - Fixed a small bug with GL_ARB_shader_objects, that lead       }
{                lead to that extension not loaded correctly              (SW) }
{ Version 1.3c - more GL 1.5 compliance by FOG_COORD_xx and                    }
{                ARB less VBO and occlusion query routines                (MW) }
{==============================================================================}

unit dglOpenGL;

interface

uses
  SysUtils,
  Windows;

type
  // Needed for Delphi 6 and less (defined in system.pas for Delphi 7)
  PPointer    = ^Pointer;

  TGLenum     = Cardinal;
  TGLboolean  = BYTEBOOL;
  TGLbitfield = Cardinal;
  TGLbyte     = Shortint;
  TGLshort    = SmallInt;
  TGLint      = Integer;
  TGLsizei    = Integer;
  TGLubyte    = Byte;
  TGLushort   = Word;
  TGLuint     = Cardinal;
  TGLfloat    = Single;
  TGLclampf   = Single;
  TGLdouble   = Double;
  TGLclampd   = Double;
  TGLvoid     = Pointer;
  TGLint64    = Int64;

  GLenum      = Cardinal;
  GLboolean   = BYTEBOOL;
  GLbitfield  = Cardinal;
  GLbyte      = Shortint;
  GLshort     = SmallInt;
  GLint       = Integer;
  GLsizei     = Integer;
  GLubyte     = Byte;
  GLushort    = Word;
  GLuint      = Cardinal;
  GLfloat     = Single;
  GLclampf    = Single;
  GLdouble    = Double;
  GLclampd    = Double;
  GLvoid      = Pointer;
  GLint64     = Int64;

  PGLBoolean  = ^TGLboolean;
  PGLByte     = ^TGLbyte;
  PGLShort    = ^TGLshort;
  PGLInt      = ^TGLint;
  PGLSizei    = ^TGLsizei;
  PGLubyte    = ^TGLubyte;
  PGLushort   = ^TGLushort;
  PGLuint     = ^TGLuint;
  PGLclampf   = ^TGLclampf;
  PGLfloat    = ^TGLfloat;
  PGLdouble   = ^TGLdouble;
  PGLclampd   = ^TGLclampd;
  PGLenum     = ^TGLenum;
  PGLvoid     = Pointer;
  PGLint64    = ^TGLint64;


  // Datatypes corresponding to GL's types TGL(name)(type)(count)
  TGLVectori4 = array[0..3] of TGLInt;
  TGLVectorf4 = array[0..3] of TGLFloat;
  TGLVectord3 = array[0..2] of TGLDouble;
  TGLVectord4 = array[0..3] of TGLDouble;
  TGLVectorp4 = array[0..3] of Pointer;

  TGLArrayf4  = array [0..3] of TGLFloat;
  TGLArrayf3  = array [0..2] of TGLFloat;
  TGLArrayd3  = array [0..2] of TGLDouble;
  TGLArrayi4  = array [0..3] of TGLint;
  TGLArrayp4  = array [0..3] of Pointer;

  TGLMatrixf4 = array[0..3, 0..3] of Single;
  TGLMatrixd4 = array[0..3, 0..3] of Double;
  TGlMatrixi4 = array[0..3, 0..3] of Integer;

  // Datatypes corresponding to OpenGL12.pas for easy porting
  TVector3d = TGLVectord3;

  TVector4i = TGLVectori4;
  TVector4f = TGLVectorf4;
  TVector4p = TGLVectorp4;

  TMatrix4f = TGLMatrixf4;
  TMatrix4d = TGLMatrixd4;


  // WGL_ARB_pbuffer
  HPBUFFERARB = THandle;

  // WGL_EXT_pbuffer
  HPBUFFEREXT = THandle;

  // GL_NV_half_float
  TGLhalfNV = WORD;
  PGLhalfNV = ^TGLhalfNV;

  // GL_ARB_SHADER_OBJECTS
  PGLHandleARB = ^GLHandleARB;
  GLHandleARB  = Integer;
  PPGLCharARB  = ^PChar;
  PGLCharARB   = PChar;
  GLCharARB    = Char;

type
  // GLU types
  TGLUNurbs      = record end;
  TGLUQuadric    = record end;
  TGLUTesselator = record end;
  PGLUNurbs      = ^TGLUNurbs;
  PGLUQuadric    = ^TGLUQuadric;
  PGLUTesselator = ^TGLUTesselator;
  // backwards compatibility
  TGLUNurbsObj        = TGLUNurbs;
  TGLUQuadricObj      = TGLUQuadric;
  TGLUTesselatorObj   = TGLUTesselator;
  TGLUTriangulatorObj = TGLUTesselator;
  PGLUNurbsObj        = PGLUNurbs;
  PGLUQuadricObj      = PGLUQuadric;
  PGLUTesselatorObj   = PGLUTesselator;
  PGLUTriangulatorObj = PGLUTesselator;
  // GLUQuadricCallback
  TGLUQuadricErrorProc     = procedure(errorCode: TGLEnum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  // GLUTessCallback
  TGLUTessBeginProc        = procedure(AType: TGLEnum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  TGLUTessEdgeFlagProc     = procedure(Flag: TGLboolean); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  TGLUTessVertexProc       = procedure(VertexData: Pointer); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  TGLUTessEndProc          = procedure; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  TGLUTessErrorProc        = procedure(ErrNo: TGLEnum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  TGLUTessCombineProc      = procedure(Coords: TGLArrayd3; VertexData: TGLArrayp4; Weight: TGLArrayf4; OutData: PPointer); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  TGLUTessBeginDataProc    = procedure(AType: TGLEnum; UserData: Pointer); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  TGLUTessEdgeFlagDataProc = procedure(Flag: TGLboolean; UserData: Pointer); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  TGLUTessVertexDataProc   = procedure(VertexData: Pointer; UserData: Pointer); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  TGLUTessEndDataProc      = procedure(UserData: Pointer); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  TGLUTessErrorDataProc    = procedure(ErrNo: TGLEnum; UserData: Pointer); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  TGLUTessCombineDataProc  = procedure(Coords: TGLArrayd3; VertexData: TGLArrayp4; Weight: TGLArrayf4; OutData: PPointer; UserData: Pointer); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  // GLUNurbsCallback
  TGLUNurbsErrorProc       = procedure(ErrorCode: TGLEnum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

var
  GL_VERSION_1_0,
  GL_VERSION_1_1,
  GL_VERSION_1_2,
  GL_VERSION_1_3,
  GL_VERSION_1_4,
  GL_VERSION_1_5,
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
  GL_ARB_texture_mirror_repeat,
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
  GL_EXT_texture_env_add,
  GL_EXT_texture_env_combine,
  GL_EXT_texture_env_dot3,
  GL_EXT_texture_filter_anisotropic,
  GL_EXT_texture_lod_bias,
  GL_EXT_texture_object,
  GL_EXT_texture_perturb_normal,
  GL_EXT_vertex_array,
  GL_EXT_vertex_shader,
  GL_EXT_vertex_weighting,
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
  WGL_ARB_render_texture,
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
  WGL_OML_sync_control,
  WIN_draw_range_elements,
  WIN_swap_hint: Boolean;

const
  // GL_VERSION_1_1
  GL_ACCUM                                           = $0100;
  GL_LOAD                                            = $0101;
  GL_RETURN                                          = $0102;
  GL_MULT                                            = $0103;
  GL_ADD                                             = $0104;
  GL_NEVER                                           = $0200;
  GL_LESS                                            = $0201;
  GL_EQUAL                                           = $0202;
  GL_LEQUAL                                          = $0203;
  GL_GREATER                                         = $0204;
  GL_NOTEQUAL                                        = $0205;
  GL_GEQUAL                                          = $0206;
  GL_ALWAYS                                          = $0207;
  GL_CURRENT_BIT                                     = $00000001;
  GL_POINT_BIT                                       = $00000002;
  GL_LINE_BIT                                        = $00000004;
  GL_POLYGON_BIT                                     = $00000008;
  GL_POLYGON_STIPPLE_BIT                             = $00000010;
  GL_PIXEL_MODE_BIT                                  = $00000020;
  GL_LIGHTING_BIT                                    = $00000040;
  GL_FOG_BIT                                         = $00000080;
  GL_DEPTH_BUFFER_BIT                                = $00000100;
  GL_ACCUM_BUFFER_BIT                                = $00000200;
  GL_STENCIL_BUFFER_BIT                              = $00000400;
  GL_VIEWPORT_BIT                                    = $00000800;
  GL_TRANSFORM_BIT                                   = $00001000;
  GL_ENABLE_BIT                                      = $00002000;
  GL_COLOR_BUFFER_BIT                                = $00004000;
  GL_HINT_BIT                                        = $00008000;
  GL_EVAL_BIT                                        = $00010000;
  GL_LIST_BIT                                        = $00020000;
  GL_TEXTURE_BIT                                     = $00040000;
  GL_SCISSOR_BIT                                     = $00080000;
  GL_ALL_ATTRIB_BITS                                 = $000fffff;
  GL_POINTS                                          = $0000;
  GL_LINES                                           = $0001;
  GL_LINE_LOOP                                       = $0002;
  GL_LINE_STRIP                                      = $0003;
  GL_TRIANGLES                                       = $0004;
  GL_TRIANGLE_STRIP                                  = $0005;
  GL_TRIANGLE_FAN                                    = $0006;
  GL_QUADS                                           = $0007;
  GL_QUAD_STRIP                                      = $0008;
  GL_POLYGON                                         = $0009;
  GL_ZERO                                            = 0;
  GL_ONE                                             = 1;
  GL_SRC_COLOR                                       = $0300;
  GL_ONE_MINUS_SRC_COLOR                             = $0301;
  GL_SRC_ALPHA                                       = $0302;
  GL_ONE_MINUS_SRC_ALPHA                             = $0303;
  GL_DST_ALPHA                                       = $0304;
  GL_ONE_MINUS_DST_ALPHA                             = $0305;
  GL_DST_COLOR                                       = $0306;
  GL_ONE_MINUS_DST_COLOR                             = $0307;
  GL_SRC_ALPHA_SATURATE                              = $0308;
  GL_TRUE                                            = 1;
  GL_FALSE                                           = 0;
  GL_CLIP_PLANE0                                     = $3000;
  GL_CLIP_PLANE1                                     = $3001;
  GL_CLIP_PLANE2                                     = $3002;
  GL_CLIP_PLANE3                                     = $3003;
  GL_CLIP_PLANE4                                     = $3004;
  GL_CLIP_PLANE5                                     = $3005;
  GL_BYTE                                            = $1400;
  GL_UNSIGNED_BYTE                                   = $1401;
  GL_SHORT                                           = $1402;
  GL_UNSIGNED_SHORT                                  = $1403;
  GL_INT                                             = $1404;
  GL_UNSIGNED_INT                                    = $1405;
  GL_FLOAT                                           = $1406;
  GL_2_BYTES                                         = $1407;
  GL_3_BYTES                                         = $1408;
  GL_4_BYTES                                         = $1409;
  GL_DOUBLE                                          = $140A;
  GL_NONE                                            = 0;
  GL_FRONT_LEFT                                      = $0400;
  GL_FRONT_RIGHT                                     = $0401;
  GL_BACK_LEFT                                       = $0402;
  GL_BACK_RIGHT                                      = $0403;
  GL_FRONT                                           = $0404;
  GL_BACK                                            = $0405;
  GL_LEFT                                            = $0406;
  GL_RIGHT                                           = $0407;
  GL_FRONT_AND_BACK                                  = $0408;
  GL_AUX0                                            = $0409;
  GL_AUX1                                            = $040A;
  GL_AUX2                                            = $040B;
  GL_AUX3                                            = $040C;
  GL_NO_ERROR                                        = 0;
  GL_INVALID_ENUM                                    = $0500;
  GL_INVALID_VALUE                                   = $0501;
  GL_INVALID_OPERATION                               = $0502;
  GL_STACK_OVERFLOW                                  = $0503;
  GL_STACK_UNDERFLOW                                 = $0504;
  GL_OUT_OF_MEMORY                                   = $0505;
  GL_2D                                              = $0600;
  GL_3D                                              = $0601;
  GL_3D_COLOR                                        = $0602;
  GL_3D_COLOR_TEXTURE                                = $0603;
  GL_4D_COLOR_TEXTURE                                = $0604;
  GL_PASS_THROUGH_TOKEN                              = $0700;
  GL_POINT_TOKEN                                     = $0701;
  GL_LINE_TOKEN                                      = $0702;
  GL_POLYGON_TOKEN                                   = $0703;
  GL_BITMAP_TOKEN                                    = $0704;
  GL_DRAW_PIXEL_TOKEN                                = $0705;
  GL_COPY_PIXEL_TOKEN                                = $0706;
  GL_LINE_RESET_TOKEN                                = $0707;
  GL_EXP                                             = $0800;
  GL_EXP2                                            = $0801;
  GL_CW                                              = $0900;
  GL_CCW                                             = $0901;
  GL_COEFF                                           = $0A00;
  GL_ORDER                                           = $0A01;
  GL_DOMAIN                                          = $0A02;
  GL_CURRENT_COLOR                                   = $0B00;
  GL_CURRENT_INDEX                                   = $0B01;
  GL_CURRENT_NORMAL                                  = $0B02;
  GL_CURRENT_TEXTURE_COORDS                          = $0B03;
  GL_CURRENT_RASTER_COLOR                            = $0B04;
  GL_CURRENT_RASTER_INDEX                            = $0B05;
  GL_CURRENT_RASTER_TEXTURE_COORDS                   = $0B06;
  GL_CURRENT_RASTER_POSITION                         = $0B07;
  GL_CURRENT_RASTER_POSITION_VALID                   = $0B08;
  GL_CURRENT_RASTER_DISTANCE                         = $0B09;
  GL_POINT_SMOOTH                                    = $0B10;
  GL_POINT_SIZE                                      = $0B11;
  GL_POINT_SIZE_RANGE                                = $0B12;
  GL_POINT_SIZE_GRANULARITY                          = $0B13;
  GL_LINE_SMOOTH                                     = $0B20;
  GL_LINE_WIDTH                                      = $0B21;
  GL_LINE_WIDTH_RANGE                                = $0B22;
  GL_LINE_WIDTH_GRANULARITY                          = $0B23;
  GL_LINE_STIPPLE                                    = $0B24;
  GL_LINE_STIPPLE_PATTERN                            = $0B25;
  GL_LINE_STIPPLE_REPEAT                             = $0B26;
  GL_LIST_MODE                                       = $0B30;
  GL_MAX_LIST_NESTING                                = $0B31;
  GL_LIST_BASE                                       = $0B32;
  GL_LIST_INDEX                                      = $0B33;
  GL_POLYGON_MODE                                    = $0B40;
  GL_POLYGON_SMOOTH                                  = $0B41;
  GL_POLYGON_STIPPLE                                 = $0B42;
  GL_EDGE_FLAG                                       = $0B43;
  GL_CULL_FACE                                       = $0B44;
  GL_CULL_FACE_MODE                                  = $0B45;
  GL_FRONT_FACE                                      = $0B46;
  GL_LIGHTING                                        = $0B50;
  GL_LIGHT_MODEL_LOCAL_VIEWER                        = $0B51;
  GL_LIGHT_MODEL_TWO_SIDE                            = $0B52;
  GL_LIGHT_MODEL_AMBIENT                             = $0B53;
  GL_SHADE_MODEL                                     = $0B54;
  GL_COLOR_MATERIAL_FACE                             = $0B55;
  GL_COLOR_MATERIAL_PARAMETER                        = $0B56;
  GL_COLOR_MATERIAL                                  = $0B57;
  GL_FOG                                             = $0B60;
  GL_FOG_INDEX                                       = $0B61;
  GL_FOG_DENSITY                                     = $0B62;
  GL_FOG_START                                       = $0B63;
  GL_FOG_END                                         = $0B64;
  GL_FOG_MODE                                        = $0B65;
  GL_FOG_COLOR                                       = $0B66;
  GL_DEPTH_RANGE                                     = $0B70;
  GL_DEPTH_TEST                                      = $0B71;
  GL_DEPTH_WRITEMASK                                 = $0B72;
  GL_DEPTH_CLEAR_VALUE                               = $0B73;
  GL_DEPTH_FUNC                                      = $0B74;
  GL_ACCUM_CLEAR_VALUE                               = $0B80;
  GL_STENCIL_TEST                                    = $0B90;
  GL_STENCIL_CLEAR_VALUE                             = $0B91;
  GL_STENCIL_FUNC                                    = $0B92;
  GL_STENCIL_VALUE_MASK                              = $0B93;
  GL_STENCIL_FAIL                                    = $0B94;
  GL_STENCIL_PASS_DEPTH_FAIL                         = $0B95;
  GL_STENCIL_PASS_DEPTH_PASS                         = $0B96;
  GL_STENCIL_REF                                     = $0B97;
  GL_STENCIL_WRITEMASK                               = $0B98;
  GL_MATRIX_MODE                                     = $0BA0;
  GL_NORMALIZE                                       = $0BA1;
  GL_VIEWPORT                                        = $0BA2;
  GL_MODELVIEW_STACK_DEPTH                           = $0BA3;
  GL_PROJECTION_STACK_DEPTH                          = $0BA4;
  GL_TEXTURE_STACK_DEPTH                             = $0BA5;
  GL_MODELVIEW_MATRIX                                = $0BA6;
  GL_PROJECTION_MATRIX                               = $0BA7;
  GL_TEXTURE_MATRIX                                  = $0BA8;
  GL_ATTRIB_STACK_DEPTH                              = $0BB0;
  GL_CLIENT_ATTRIB_STACK_DEPTH                       = $0BB1;
  GL_ALPHA_TEST                                      = $0BC0;
  GL_ALPHA_TEST_FUNC                                 = $0BC1;
  GL_ALPHA_TEST_REF                                  = $0BC2;
  GL_DITHER                                          = $0BD0;
  GL_BLEND_DST                                       = $0BE0;
  GL_BLEND_SRC                                       = $0BE1;
  GL_BLEND                                           = $0BE2;
  GL_LOGIC_OP_MODE                                   = $0BF0;
  GL_INDEX_LOGIC_OP                                  = $0BF1;
  GL_COLOR_LOGIC_OP                                  = $0BF2;
  GL_AUX_BUFFERS                                     = $0C00;
  GL_DRAW_BUFFER                                     = $0C01;
  GL_READ_BUFFER                                     = $0C02;
  GL_SCISSOR_BOX                                     = $0C10;
  GL_SCISSOR_TEST                                    = $0C11;
  GL_INDEX_CLEAR_VALUE                               = $0C20;
  GL_INDEX_WRITEMASK                                 = $0C21;
  GL_COLOR_CLEAR_VALUE                               = $0C22;
  GL_COLOR_WRITEMASK                                 = $0C23;
  GL_INDEX_MODE                                      = $0C30;
  GL_RGBA_MODE                                       = $0C31;
  GL_DOUBLEBUFFER                                    = $0C32;
  GL_STEREO                                          = $0C33;
  GL_RENDER_MODE                                     = $0C40;
  GL_PERSPECTIVE_CORRECTION_HINT                     = $0C50;
  GL_POINT_SMOOTH_HINT                               = $0C51;
  GL_LINE_SMOOTH_HINT                                = $0C52;
  GL_POLYGON_SMOOTH_HINT                             = $0C53;
  GL_FOG_HINT                                        = $0C54;
  GL_TEXTURE_GEN_S                                   = $0C60;
  GL_TEXTURE_GEN_T                                   = $0C61;
  GL_TEXTURE_GEN_R                                   = $0C62;
  GL_TEXTURE_GEN_Q                                   = $0C63;
  GL_PIXEL_MAP_I_TO_I                                = $0C70;
  GL_PIXEL_MAP_S_TO_S                                = $0C71;
  GL_PIXEL_MAP_I_TO_R                                = $0C72;
  GL_PIXEL_MAP_I_TO_G                                = $0C73;
  GL_PIXEL_MAP_I_TO_B                                = $0C74;
  GL_PIXEL_MAP_I_TO_A                                = $0C75;
  GL_PIXEL_MAP_R_TO_R                                = $0C76;
  GL_PIXEL_MAP_G_TO_G                                = $0C77;
  GL_PIXEL_MAP_B_TO_B                                = $0C78;
  GL_PIXEL_MAP_A_TO_A                                = $0C79;
  GL_PIXEL_MAP_I_TO_I_SIZE                           = $0CB0;
  GL_PIXEL_MAP_S_TO_S_SIZE                           = $0CB1;
  GL_PIXEL_MAP_I_TO_R_SIZE                           = $0CB2;
  GL_PIXEL_MAP_I_TO_G_SIZE                           = $0CB3;
  GL_PIXEL_MAP_I_TO_B_SIZE                           = $0CB4;
  GL_PIXEL_MAP_I_TO_A_SIZE                           = $0CB5;
  GL_PIXEL_MAP_R_TO_R_SIZE                           = $0CB6;
  GL_PIXEL_MAP_G_TO_G_SIZE                           = $0CB7;
  GL_PIXEL_MAP_B_TO_B_SIZE                           = $0CB8;
  GL_PIXEL_MAP_A_TO_A_SIZE                           = $0CB9;
  GL_UNPACK_SWAP_BYTES                               = $0CF0;
  GL_UNPACK_LSB_FIRST                                = $0CF1;
  GL_UNPACK_ROW_LENGTH                               = $0CF2;
  GL_UNPACK_SKIP_ROWS                                = $0CF3;
  GL_UNPACK_SKIP_PIXELS                              = $0CF4;
  GL_UNPACK_ALIGNMENT                                = $0CF5;
  GL_PACK_SWAP_BYTES                                 = $0D00;
  GL_PACK_LSB_FIRST                                  = $0D01;
  GL_PACK_ROW_LENGTH                                 = $0D02;
  GL_PACK_SKIP_ROWS                                  = $0D03;
  GL_PACK_SKIP_PIXELS                                = $0D04;
  GL_PACK_ALIGNMENT                                  = $0D05;
  GL_MAP_COLOR                                       = $0D10;
  GL_MAP_STENCIL                                     = $0D11;
  GL_INDEX_SHIFT                                     = $0D12;
  GL_INDEX_OFFSET                                    = $0D13;
  GL_RED_SCALE                                       = $0D14;
  GL_RED_BIAS                                        = $0D15;
  GL_ZOOM_X                                          = $0D16;
  GL_ZOOM_Y                                          = $0D17;
  GL_GREEN_SCALE                                     = $0D18;
  GL_GREEN_BIAS                                      = $0D19;
  GL_BLUE_SCALE                                      = $0D1A;
  GL_BLUE_BIAS                                       = $0D1B;
  GL_ALPHA_SCALE                                     = $0D1C;
  GL_ALPHA_BIAS                                      = $0D1D;
  GL_DEPTH_SCALE                                     = $0D1E;
  GL_DEPTH_BIAS                                      = $0D1F;
  GL_MAX_EVAL_ORDER                                  = $0D30;
  GL_MAX_LIGHTS                                      = $0D31;
  GL_MAX_CLIP_PLANES                                 = $0D32;
  GL_MAX_TEXTURE_SIZE                                = $0D33;
  GL_MAX_PIXEL_MAP_TABLE                             = $0D34;
  GL_MAX_ATTRIB_STACK_DEPTH                          = $0D35;
  GL_MAX_MODELVIEW_STACK_DEPTH                       = $0D36;
  GL_MAX_NAME_STACK_DEPTH                            = $0D37;
  GL_MAX_PROJECTION_STACK_DEPTH                      = $0D38;
  GL_MAX_TEXTURE_STACK_DEPTH                         = $0D39;
  GL_MAX_VIEWPORT_DIMS                               = $0D3A;
  GL_MAX_CLIENT_ATTRIB_STACK_DEPTH                   = $0D3B;
  GL_SUBPIXEL_BITS                                   = $0D50;
  GL_INDEX_BITS                                      = $0D51;
  GL_RED_BITS                                        = $0D52;
  GL_GREEN_BITS                                      = $0D53;
  GL_BLUE_BITS                                       = $0D54;
  GL_ALPHA_BITS                                      = $0D55;
  GL_DEPTH_BITS                                      = $0D56;
  GL_STENCIL_BITS                                    = $0D57;
  GL_ACCUM_RED_BITS                                  = $0D58;
  GL_ACCUM_GREEN_BITS                                = $0D59;
  GL_ACCUM_BLUE_BITS                                 = $0D5A;
  GL_ACCUM_ALPHA_BITS                                = $0D5B;
  GL_NAME_STACK_DEPTH                                = $0D70;
  GL_AUTO_NORMAL                                     = $0D80;
  GL_MAP1_COLOR_4                                    = $0D90;
  GL_MAP1_INDEX                                      = $0D91;
  GL_MAP1_NORMAL                                     = $0D92;
  GL_MAP1_TEXTURE_COORD_1                            = $0D93;
  GL_MAP1_TEXTURE_COORD_2                            = $0D94;
  GL_MAP1_TEXTURE_COORD_3                            = $0D95;
  GL_MAP1_TEXTURE_COORD_4                            = $0D96;
  GL_MAP1_VERTEX_3                                   = $0D97;
  GL_MAP1_VERTEX_4                                   = $0D98;
  GL_MAP2_COLOR_4                                    = $0DB0;
  GL_MAP2_INDEX                                      = $0DB1;
  GL_MAP2_NORMAL                                     = $0DB2;
  GL_MAP2_TEXTURE_COORD_1                            = $0DB3;
  GL_MAP2_TEXTURE_COORD_2                            = $0DB4;
  GL_MAP2_TEXTURE_COORD_3                            = $0DB5;
  GL_MAP2_TEXTURE_COORD_4                            = $0DB6;
  GL_MAP2_VERTEX_3                                   = $0DB7;
  GL_MAP2_VERTEX_4                                   = $0DB8;
  GL_MAP1_GRID_DOMAIN                                = $0DD0;
  GL_MAP1_GRID_SEGMENTS                              = $0DD1;
  GL_MAP2_GRID_DOMAIN                                = $0DD2;
  GL_MAP2_GRID_SEGMENTS                              = $0DD3;
  GL_TEXTURE_1D                                      = $0DE0;
  GL_TEXTURE_2D                                      = $0DE1;
  GL_FEEDBACK_BUFFER_POINTER                         = $0DF0;
  GL_FEEDBACK_BUFFER_SIZE                            = $0DF1;
  GL_FEEDBACK_BUFFER_TYPE                            = $0DF2;
  GL_SELECTION_BUFFER_POINTER                        = $0DF3;
  GL_SELECTION_BUFFER_SIZE                           = $0DF4;
  GL_TEXTURE_WIDTH                                   = $1000;
  GL_TEXTURE_HEIGHT                                  = $1001;
  GL_TEXTURE_INTERNAL_FORMAT                         = $1003;
  GL_TEXTURE_BORDER_COLOR                            = $1004;
  GL_TEXTURE_BORDER                                  = $1005;
  GL_DONT_CARE                                       = $1100;
  GL_FASTEST                                         = $1101;
  GL_NICEST                                          = $1102;
  GL_LIGHT0                                          = $4000;
  GL_LIGHT1                                          = $4001;
  GL_LIGHT2                                          = $4002;
  GL_LIGHT3                                          = $4003;
  GL_LIGHT4                                          = $4004;
  GL_LIGHT5                                          = $4005;
  GL_LIGHT6                                          = $4006;
  GL_LIGHT7                                          = $4007;
  GL_AMBIENT                                         = $1200;
  GL_DIFFUSE                                         = $1201;
  GL_SPECULAR                                        = $1202;
  GL_POSITION                                        = $1203;
  GL_SPOT_DIRECTION                                  = $1204;
  GL_SPOT_EXPONENT                                   = $1205;
  GL_SPOT_CUTOFF                                     = $1206;
  GL_CONSTANT_ATTENUATION                            = $1207;
  GL_LINEAR_ATTENUATION                              = $1208;
  GL_QUADRATIC_ATTENUATION                           = $1209;
  GL_COMPILE                                         = $1300;
  GL_COMPILE_AND_EXECUTE                             = $1301;
  GL_CLEAR                                           = $1500;
  GL_AND                                             = $1501;
  GL_AND_REVERSE                                     = $1502;
  GL_COPY                                            = $1503;
  GL_AND_INVERTED                                    = $1504;
  GL_NOOP                                            = $1505;
  GL_XOR                                             = $1506;
  GL_OR                                              = $1507;
  GL_NOR                                             = $1508;
  GL_EQUIV                                           = $1509;
  GL_INVERT                                          = $150A;
  GL_OR_REVERSE                                      = $150B;
  GL_COPY_INVERTED                                   = $150C;
  GL_OR_INVERTED                                     = $150D;
  GL_NAND                                            = $150E;
  GL_SET                                             = $150F;
  GL_EMISSION                                        = $1600;
  GL_SHININESS                                       = $1601;
  GL_AMBIENT_AND_DIFFUSE                             = $1602;
  GL_COLOR_INDEXES                                   = $1603;
  GL_MODELVIEW                                       = $1700;
  GL_PROJECTION                                      = $1701;
  GL_TEXTURE                                         = $1702;
  GL_COLOR                                           = $1800;
  GL_DEPTH                                           = $1801;
  GL_STENCIL                                         = $1802;
  GL_COLOR_INDEX                                     = $1900;
  GL_STENCIL_INDEX                                   = $1901;
  GL_DEPTH_COMPONENT                                 = $1902;
  GL_RED                                             = $1903;
  GL_GREEN                                           = $1904;
  GL_BLUE                                            = $1905;
  GL_ALPHA                                           = $1906;
  GL_RGB                                             = $1907;
  GL_RGBA                                            = $1908;
  GL_LUMINANCE                                       = $1909;
  GL_LUMINANCE_ALPHA                                 = $190A;
  GL_BITMAP                                          = $1A00;
  GL_POINT                                           = $1B00;
  GL_LINE                                            = $1B01;
  GL_FILL                                            = $1B02;
  GL_RENDER                                          = $1C00;
  GL_FEEDBACK                                        = $1C01;
  GL_SELECT                                          = $1C02;
  GL_FLAT                                            = $1D00;
  GL_SMOOTH                                          = $1D01;
  GL_KEEP                                            = $1E00;
  GL_REPLACE                                         = $1E01;
  GL_INCR                                            = $1E02;
  GL_DECR                                            = $1E03;
  GL_VENDOR                                          = $1F00;
  GL_RENDERER                                        = $1F01;
  GL_VERSION                                         = $1F02;
  GL_EXTENSIONS                                      = $1F03;
  GL_S                                               = $2000;
  GL_T                                               = $2001;
  GL_R                                               = $2002;
  GL_Q                                               = $2003;
  GL_MODULATE                                        = $2100;
  GL_DECAL                                           = $2101;
  GL_TEXTURE_ENV_MODE                                = $2200;
  GL_TEXTURE_ENV_COLOR                               = $2201;
  GL_TEXTURE_ENV                                     = $2300;
  GL_EYE_LINEAR                                      = $2400;
  GL_OBJECT_LINEAR                                   = $2401;
  GL_SPHERE_MAP                                      = $2402;
  GL_TEXTURE_GEN_MODE                                = $2500;
  GL_OBJECT_PLANE                                    = $2501;
  GL_EYE_PLANE                                       = $2502;
  GL_NEAREST                                         = $2600;
  GL_LINEAR                                          = $2601;
  GL_NEAREST_MIPMAP_NEAREST                          = $2700;
  GL_LINEAR_MIPMAP_NEAREST                           = $2701;
  GL_NEAREST_MIPMAP_LINEAR                           = $2702;
  GL_LINEAR_MIPMAP_LINEAR                            = $2703;
  GL_TEXTURE_MAG_FILTER                              = $2800;
  GL_TEXTURE_MIN_FILTER                              = $2801;
  GL_TEXTURE_WRAP_S                                  = $2802;
  GL_TEXTURE_WRAP_T                                  = $2803;
  GL_CLAMP                                           = $2900;
  GL_REPEAT                                          = $2901;
  GL_CLIENT_PIXEL_STORE_BIT                          = $00000001;
  GL_CLIENT_VERTEX_ARRAY_BIT                         = $00000002;
  GL_CLIENT_ALL_ATTRIB_BITS                          = $ffffffff;
  GL_POLYGON_OFFSET_FACTOR                           = $8038;
  GL_POLYGON_OFFSET_UNITS                            = $2A00;
  GL_POLYGON_OFFSET_POINT                            = $2A01;
  GL_POLYGON_OFFSET_LINE                             = $2A02;
  GL_POLYGON_OFFSET_FILL                             = $8037;
  GL_ALPHA4                                          = $803B;
  GL_ALPHA8                                          = $803C;
  GL_ALPHA12                                         = $803D;
  GL_ALPHA16                                         = $803E;
  GL_LUMINANCE4                                      = $803F;
  GL_LUMINANCE8                                      = $8040;
  GL_LUMINANCE12                                     = $8041;
  GL_LUMINANCE16                                     = $8042;
  GL_LUMINANCE4_ALPHA4                               = $8043;
  GL_LUMINANCE6_ALPHA2                               = $8044;
  GL_LUMINANCE8_ALPHA8                               = $8045;
  GL_LUMINANCE12_ALPHA4                              = $8046;
  GL_LUMINANCE12_ALPHA12                             = $8047;
  GL_LUMINANCE16_ALPHA16                             = $8048;
  GL_INTENSITY                                       = $8049;
  GL_INTENSITY4                                      = $804A;
  GL_INTENSITY8                                      = $804B;
  GL_INTENSITY12                                     = $804C;
  GL_INTENSITY16                                     = $804D;
  GL_R3_G3_B2                                        = $2A10;
  GL_RGB4                                            = $804F;
  GL_RGB5                                            = $8050;
  GL_RGB8                                            = $8051;
  GL_RGB10                                           = $8052;
  GL_RGB12                                           = $8053;
  GL_RGB16                                           = $8054;
  GL_RGBA2                                           = $8055;
  GL_RGBA4                                           = $8056;
  GL_RGB5_A1                                         = $8057;
  GL_RGBA8                                           = $8058;
  GL_RGB10_A2                                        = $8059;
  GL_RGBA12                                          = $805A;
  GL_RGBA16                                          = $805B;
  GL_TEXTURE_RED_SIZE                                = $805C;
  GL_TEXTURE_GREEN_SIZE                              = $805D;
  GL_TEXTURE_BLUE_SIZE                               = $805E;
  GL_TEXTURE_ALPHA_SIZE                              = $805F;
  GL_TEXTURE_LUMINANCE_SIZE                          = $8060;
  GL_TEXTURE_INTENSITY_SIZE                          = $8061;
  GL_PROXY_TEXTURE_1D                                = $8063;
  GL_PROXY_TEXTURE_2D                                = $8064;
  GL_TEXTURE_PRIORITY                                = $8066;
  GL_TEXTURE_RESIDENT                                = $8067;
  GL_TEXTURE_BINDING_1D                              = $8068;
  GL_TEXTURE_BINDING_2D                              = $8069;
  GL_VERTEX_ARRAY                                    = $8074;
  GL_NORMAL_ARRAY                                    = $8075;
  GL_COLOR_ARRAY                                     = $8076;
  GL_INDEX_ARRAY                                     = $8077;
  GL_TEXTURE_COORD_ARRAY                             = $8078;
  GL_EDGE_FLAG_ARRAY                                 = $8079;
  GL_VERTEX_ARRAY_SIZE                               = $807A;
  GL_VERTEX_ARRAY_TYPE                               = $807B;
  GL_VERTEX_ARRAY_STRIDE                             = $807C;
  GL_NORMAL_ARRAY_TYPE                               = $807E;
  GL_NORMAL_ARRAY_STRIDE                             = $807F;
  GL_COLOR_ARRAY_SIZE                                = $8081;
  GL_COLOR_ARRAY_TYPE                                = $8082;
  GL_COLOR_ARRAY_STRIDE                              = $8083;
  GL_INDEX_ARRAY_TYPE                                = $8085;
  GL_INDEX_ARRAY_STRIDE                              = $8086;
  GL_TEXTURE_COORD_ARRAY_SIZE                        = $8088;
  GL_TEXTURE_COORD_ARRAY_TYPE                        = $8089;
  GL_TEXTURE_COORD_ARRAY_STRIDE                      = $808A;
  GL_EDGE_FLAG_ARRAY_STRIDE                          = $808C;
  GL_VERTEX_ARRAY_POINTER                            = $808E;
  GL_NORMAL_ARRAY_POINTER                            = $808F;
  GL_COLOR_ARRAY_POINTER                             = $8090;
  GL_INDEX_ARRAY_POINTER                             = $8091;
  GL_TEXTURE_COORD_ARRAY_POINTER                     = $8092;
  GL_EDGE_FLAG_ARRAY_POINTER                         = $8093;
  GL_V2F                                             = $2A20;
  GL_V3F                                             = $2A21;
  GL_C4UB_V2F                                        = $2A22;
  GL_C4UB_V3F                                        = $2A23;
  GL_C3F_V3F                                         = $2A24;
  GL_N3F_V3F                                         = $2A25;
  GL_C4F_N3F_V3F                                     = $2A26;
  GL_T2F_V3F                                         = $2A27;
  GL_T4F_V4F                                         = $2A28;
  GL_T2F_C4UB_V3F                                    = $2A29;
  GL_T2F_C3F_V3F                                     = $2A2A;
  GL_T2F_N3F_V3F                                     = $2A2B;
  GL_T2F_C4F_N3F_V3F                                 = $2A2C;
  GL_T4F_C4F_N3F_V4F                                 = $2A2D;
  GL_COLOR_TABLE_FORMAT_EXT                          = $80D8;
  GL_COLOR_TABLE_WIDTH_EXT                           = $80D9;
  GL_COLOR_TABLE_RED_SIZE_EXT                        = $80DA;
  GL_COLOR_TABLE_GREEN_SIZE_EXT                      = $80DB;
  GL_COLOR_TABLE_BLUE_SIZE_EXT                       = $80DC;
  GL_COLOR_TABLE_ALPHA_SIZE_EXT                      = $80DD;
  GL_COLOR_TABLE_LUMINANCE_SIZE_EXT                  = $80DE;
  GL_COLOR_TABLE_INTENSITY_SIZE_EXT                  = $80DF;
  GL_LOGIC_OP                                        = GL_INDEX_LOGIC_OP;
  GL_TEXTURE_COMPONENTS                              = GL_TEXTURE_INTERNAL_FORMAT;

  // GL_VERSION_1_2
  GL_UNSIGNED_BYTE_3_3_2                             = $8032;
  GL_UNSIGNED_SHORT_4_4_4_4                          = $8033;
  GL_UNSIGNED_SHORT_5_5_5_1                          = $8034;
  GL_UNSIGNED_INT_8_8_8_8                            = $8035;
  GL_UNSIGNED_INT_10_10_10_2                         = $8036;
  GL_RESCALE_NORMAL                                  = $803A;
  GL_TEXTURE_BINDING_3D                              = $806A;
  GL_PACK_SKIP_IMAGES                                = $806B;
  GL_PACK_IMAGE_HEIGHT                               = $806C;
  GL_UNPACK_SKIP_IMAGES                              = $806D;
  GL_UNPACK_IMAGE_HEIGHT                             = $806E;
  GL_TEXTURE_3D                                      = $806F;
  GL_PROXY_TEXTURE_3D                                = $8070;
  GL_TEXTURE_DEPTH                                   = $8071;
  GL_TEXTURE_WRAP_R                                  = $8072;
  GL_MAX_3D_TEXTURE_SIZE                             = $8073;
  GL_UNSIGNED_BYTE_2_3_3_REV                         = $8362;
  GL_UNSIGNED_SHORT_5_6_5                            = $8363;
  GL_UNSIGNED_SHORT_5_6_5_REV                        = $8364;
  GL_UNSIGNED_SHORT_4_4_4_4_REV                      = $8365;
  GL_UNSIGNED_SHORT_1_5_5_5_REV                      = $8366;
  GL_UNSIGNED_INT_8_8_8_8_REV                        = $8367;
  GL_UNSIGNED_INT_2_10_10_10_REV                     = $8368;
  GL_BGR                                             = $80E0;
  GL_BGRA                                            = $80E1;
  GL_MAX_ELEMENTS_VERTICES                           = $80E8;
  GL_MAX_ELEMENTS_INDICES                            = $80E9;
  GL_CLAMP_TO_EDGE                                   = $812F;
  GL_TEXTURE_MIN_LOD                                 = $813A;
  GL_TEXTURE_MAX_LOD                                 = $813B;
  GL_TEXTURE_BASE_LEVEL                              = $813C;
  GL_TEXTURE_MAX_LEVEL                               = $813D;
  GL_LIGHT_MODEL_COLOR_CONTROL                       = $81F8;
  GL_SINGLE_COLOR                                    = $81F9;
  GL_SEPARATE_SPECULAR_COLOR                         = $81FA;
  GL_SMOOTH_POINT_SIZE_RANGE                         = $0B12;
  GL_SMOOTH_POINT_SIZE_GRANULARITY                   = $0B13;
  GL_SMOOTH_LINE_WIDTH_RANGE                         = $0B22;
  GL_SMOOTH_LINE_WIDTH_GRANULARITY                   = $0B23;
  GL_ALIASED_POINT_SIZE_RANGE                        = $846D;
  GL_ALIASED_LINE_WIDTH_RANGE                        = $846E;

  // GL_VERSION_1_3
  GL_TEXTURE0                                        = $84C0;
  GL_TEXTURE1                                        = $84C1;
  GL_TEXTURE2                                        = $84C2;
  GL_TEXTURE3                                        = $84C3;
  GL_TEXTURE4                                        = $84C4;
  GL_TEXTURE5                                        = $84C5;
  GL_TEXTURE6                                        = $84C6;
  GL_TEXTURE7                                        = $84C7;
  GL_TEXTURE8                                        = $84C8;
  GL_TEXTURE9                                        = $84C9;
  GL_TEXTURE10                                       = $84CA;
  GL_TEXTURE11                                       = $84CB;
  GL_TEXTURE12                                       = $84CC;
  GL_TEXTURE13                                       = $84CD;
  GL_TEXTURE14                                       = $84CE;
  GL_TEXTURE15                                       = $84CF;
  GL_TEXTURE16                                       = $84D0;
  GL_TEXTURE17                                       = $84D1;
  GL_TEXTURE18                                       = $84D2;
  GL_TEXTURE19                                       = $84D3;
  GL_TEXTURE20                                       = $84D4;
  GL_TEXTURE21                                       = $84D5;
  GL_TEXTURE22                                       = $84D6;
  GL_TEXTURE23                                       = $84D7;
  GL_TEXTURE24                                       = $84D8;
  GL_TEXTURE25                                       = $84D9;
  GL_TEXTURE26                                       = $84DA;
  GL_TEXTURE27                                       = $84DB;
  GL_TEXTURE28                                       = $84DC;
  GL_TEXTURE29                                       = $84DD;
  GL_TEXTURE30                                       = $84DE;
  GL_TEXTURE31                                       = $84DF;
  GL_ACTIVE_TEXTURE                                  = $84E0;
  GL_CLIENT_ACTIVE_TEXTURE                           = $84E1;
  GL_MAX_TEXTURE_UNITS                               = $84E2;
  GL_TRANSPOSE_MODELVIEW_MATRIX                      = $84E3;
  GL_TRANSPOSE_PROJECTION_MATRIX                     = $84E4;
  GL_TRANSPOSE_TEXTURE_MATRIX                        = $84E5;
  GL_TRANSPOSE_COLOR_MATRIX                          = $84E6;
  GL_MULTISAMPLE                                     = $809D;
  GL_SAMPLE_ALPHA_TO_COVERAGE                        = $809E;
  GL_SAMPLE_ALPHA_TO_ONE                             = $809F;
  GL_SAMPLE_COVERAGE                                 = $80A0;
  GL_SAMPLE_BUFFERS                                  = $80A8;
  GL_SAMPLES                                         = $80A9;
  GL_SAMPLE_COVERAGE_VALUE                           = $80AA;
  GL_SAMPLE_COVERAGE_INVERT                          = $80AB;
  GL_MULTISAMPLE_BIT                                 = $20000000;
  GL_NORMAL_MAP                                      = $8511;
  GL_REFLECTION_MAP                                  = $8512;
  GL_TEXTURE_CUBE_MAP                                = $8513;
  GL_TEXTURE_BINDING_CUBE_MAP                        = $8514;
  GL_TEXTURE_CUBE_MAP_POSITIVE_X                     = $8515;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_X                     = $8516;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Y                     = $8517;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Y                     = $8518;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Z                     = $8519;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Z                     = $851A;
  GL_PROXY_TEXTURE_CUBE_MAP                          = $851B;
  GL_MAX_CUBE_MAP_TEXTURE_SIZE                       = $851C;
  GL_COMPRESSED_ALPHA                                = $84E9;
  GL_COMPRESSED_LUMINANCE                            = $84EA;
  GL_COMPRESSED_LUMINANCE_ALPHA                      = $84EB;
  GL_COMPRESSED_INTENSITY                            = $84EC;
  GL_COMPRESSED_RGB                                  = $84ED;
  GL_COMPRESSED_RGBA                                 = $84EE;
  GL_TEXTURE_COMPRESSION_HINT                        = $84EF;
  GL_TEXTURE_COMPRESSED_IMAGE_SIZE                   = $86A0;
  GL_TEXTURE_COMPRESSED                              = $86A1;
  GL_NUM_COMPRESSED_TEXTURE_FORMATS                  = $86A2;
  GL_COMPRESSED_TEXTURE_FORMATS                      = $86A3;
  GL_CLAMP_TO_BORDER                                 = $812D;
  GL_CLAMP_TO_BORDER_SGIS                            = $812D;
  GL_COMBINE                                         = $8570;
  GL_COMBINE_RGB                                     = $8571;
  GL_COMBINE_ALPHA                                   = $8572;
  GL_SOURCE0_RGB                                     = $8580;
  GL_SOURCE1_RGB                                     = $8581;
  GL_SOURCE2_RGB                                     = $8582;
  GL_SOURCE0_ALPHA                                   = $8588;
  GL_SOURCE1_ALPHA                                   = $8589;
  GL_SOURCE2_ALPHA                                   = $858A;
  GL_OPERAND0_RGB                                    = $8590;
  GL_OPERAND1_RGB                                    = $8591;
  GL_OPERAND2_RGB                                    = $8592;
  GL_OPERAND0_ALPHA                                  = $8598;
  GL_OPERAND1_ALPHA                                  = $8599;
  GL_OPERAND2_ALPHA                                  = $859A;
  GL_RGB_SCALE                                       = $8573;
  GL_ADD_SIGNED                                      = $8574;
  GL_INTERPOLATE                                     = $8575;
  GL_SUBTRACT                                        = $84E7;
  GL_CONSTANT                                        = $8576;
  GL_PRIMARY_COLOR                                   = $8577;
  GL_PREVIOUS                                        = $8578;
  GL_DOT3_RGB                                        = $86AE;
  GL_DOT3_RGBA                                       = $86AF;

  // GL_VERSION_1_4
  GL_BLEND_DST_RGB                                   = $80C8;
  GL_BLEND_SRC_RGB                                   = $80C9;
  GL_BLEND_DST_ALPHA                                 = $80CA;
  GL_BLEND_SRC_ALPHA                                 = $80CB;
  GL_POINT_SIZE_MIN                                  = $8126;
  GL_POINT_SIZE_MAX                                  = $8127;
  GL_POINT_FADE_THRESHOLD_SIZE                       = $8128;
  GL_POINT_DISTANCE_ATTENUATION                      = $8129;
  GL_GENERATE_MIPMAP                                 = $8191;
  GL_GENERATE_MIPMAP_HINT                            = $8192;
  GL_DEPTH_COMPONENT16                               = $81A5;
  GL_DEPTH_COMPONENT24                               = $81A6;
  GL_DEPTH_COMPONENT32                               = $81A7;
  GL_MIRRORED_REPEAT                                 = $8370;
  GL_FOG_COORDINATE_SOURCE                           = $8450;
  GL_FOG_COORD_SOURCE                                = $8450;
  GL_FOG_COORDINATE                                  = $8451;
  GL_FOG_COORD                                       = $8451;
  GL_FRAGMENT_DEPTH                                  = $8452;
  GL_CURRENT_FOG_COORDINATE                          = $8453;
  GL_CURRENT_FOG_COORD                               = $8453;
  GL_FOG_COORDINATE_ARRAY_TYPE                       = $8454;
  GL_FOG_COORD_ARRAY_TYPE                            = $8454;
  GL_FOG_COORDINATE_ARRAY_STRIDE                     = $8455;
  GL_FOG_COORD_ARRAY_STRIDE                          = $8455;
  GL_FOG_COORDINATE_ARRAY_POINTER                    = $8456;
  GL_FOG_COORD_ARRAY_POINTER                         = $8456;
  GL_FOG_COORDINATE_ARRAY                            = $8457;
  GL_FOG_COORD_ARRAY                                 = $8457;
  GL_COLOR_SUM                                       = $8458;
  GL_CURRENT_SECONDARY_COLOR                         = $8459;
  GL_SECONDARY_COLOR_ARRAY_SIZE                      = $845A;
  GL_SECONDARY_COLOR_ARRAY_TYPE                      = $845B;
  GL_SECONDARY_COLOR_ARRAY_STRIDE                    = $845C;
  GL_SECONDARY_COLOR_ARRAY_POINTER                   = $845D;
  GL_SECONDARY_COLOR_ARRAY                           = $845E;
  GL_MAX_TEXTURE_LOD_BIAS                            = $84FD;
  GL_TEXTURE_FILTER_CONTROL                          = $8500;
  GL_TEXTURE_LOD_BIAS                                = $8501;
  GL_INCR_WRAP                                       = $8507;
  GL_DECR_WRAP                                       = $8508;
  GL_TEXTURE_DEPTH_SIZE                              = $884A;
  GL_DEPTH_TEXTURE_MODE                              = $884B;
  GL_TEXTURE_COMPARE_MODE                            = $884C;
  GL_TEXTURE_COMPARE_FUNC                            = $884D;
  GL_COMPARE_R_TO_TEXTURE                            = $884E;

  // GL_3DFX_multisample
  GL_MULTISAMPLE_3DFX                                = $86B2;
  GL_SAMPLE_BUFFERS_3DFX                             = $86B3;
  GL_SAMPLES_3DFX                                    = $86B4;
  GL_MULTISAMPLE_BIT_3DFX                            = $20000000;

  // GL_3DFX_texture_compression_FXT1
  GL_COMPRESSED_RGB_FXT1_3DFX                        = $86B0;
  GL_COMPRESSED_RGBA_FXT1_3DFX                       = $86B1;

  // GL_APPLE_client_storage
  GL_UNPACK_CLIENT_STORAGE_APPLE                     = $85B2;

  // GL_APPLE_element_array
  GL_ELEMENT_ARRAY_APPLE                             = $8768;
  GL_ELEMENT_ARRAY_TYPE_APPLE                        = $8769;
  GL_ELEMENT_ARRAY_POINTER_APPLE                     = $876A;

  // GL_APPLE_fence
  GL_DRAW_PIXELS_APPLE                               = $8A0A;
  GL_FENCE_APPLE                                     = $8A0B;

  // GL_APPLE_specular_vector
  GL_LIGHT_MODEL_SPECULAR_VECTOR_APPLE               = $85B0;

  // GL_APPLE_transform_hint
  GL_TRANSFORM_HINT_APPLE                            = $85B1;

  // GL_APPLE_vertex_array_object
  GL_VERTEX_ARRAY_BINDING_APPLE                      = $85B5;

  // GL_APPLE_vertex_array_range
  GL_VERTEX_ARRAY_RANGE_APPLE                        = $851D;
  GL_VERTEX_ARRAY_RANGE_LENGTH_APPLE                 = $851E;
  GL_VERTEX_ARRAY_STORAGE_HINT_APPLE                 = $851F;
  GL_VERTEX_ARRAY_RANGE_POINTER_APPLE                = $8521;
  GL_STORAGE_CACHED_APPLE                            = $85BE;
  GL_STORAGE_SHARED_APPLE                            = $85BF;

  // GL_APPLE_ycbcr_422
  GL_YCBCR_422_APPLE                                 = $85B9;
  GL_UNSIGNED_SHORT_8_8_APPLE                        = $85BA;
  GL_UNSIGNED_SHORT_8_8_REV_APPLE                    = $85BB;

  // GL_ARB_depth_texture
  GL_DEPTH_COMPONENT16_ARB                           = $81A5;
  GL_DEPTH_COMPONENT24_ARB                           = $81A6;
  GL_DEPTH_COMPONENT32_ARB                           = $81A7;
  GL_TEXTURE_DEPTH_SIZE_ARB                          = $884A;
  GL_DEPTH_TEXTURE_MODE_ARB                          = $884B;

  // GL_ARB_fragment_program
  GL_FRAGMENT_PROGRAM_ARB                            = $8804;
  GL_PROGRAM_ALU_INSTRUCTIONS_ARB                    = $8805;
  GL_PROGRAM_TEX_INSTRUCTIONS_ARB                    = $8806;
  GL_PROGRAM_TEX_INDIRECTIONS_ARB                    = $8807;
  GL_PROGRAM_NATIVE_ALU_INSTRUCTIONS_ARB             = $8808;
  GL_PROGRAM_NATIVE_TEX_INSTRUCTIONS_ARB             = $8809;
  GL_PROGRAM_NATIVE_TEX_INDIRECTIONS_ARB             = $880A;
  GL_MAX_PROGRAM_ALU_INSTRUCTIONS_ARB                = $880B;
  GL_MAX_PROGRAM_TEX_INSTRUCTIONS_ARB                = $880C;
  GL_MAX_PROGRAM_TEX_INDIRECTIONS_ARB                = $880D;
  GL_MAX_PROGRAM_NATIVE_ALU_INSTRUCTIONS_ARB         = $880E;
  GL_MAX_PROGRAM_NATIVE_TEX_INSTRUCTIONS_ARB         = $880F;
  GL_MAX_PROGRAM_NATIVE_TEX_INDIRECTIONS_ARB         = $8810;
  GL_MAX_TEXTURE_COORDS_ARB                          = $8871;
  GL_MAX_TEXTURE_IMAGE_UNITS_ARB                     = $8872;

  // GL_ARB_imaging
  GL_CONSTANT_COLOR                                  = $8001;
  GL_ONE_MINUS_CONSTANT_COLOR                        = $8002;
  GL_CONSTANT_ALPHA                                  = $8003;
  GL_ONE_MINUS_CONSTANT_ALPHA                        = $8004;
  GL_BLEND_COLOR                                     = $8005;
  GL_FUNC_ADD                                        = $8006;
  GL_MIN                                             = $8007;
  GL_MAX                                             = $8008;
  GL_BLEND_EQUATION                                  = $8009;
  GL_FUNC_SUBTRACT                                   = $800A;
  GL_FUNC_REVERSE_SUBTRACT                           = $800B;
  GL_CONVOLUTION_1D                                  = $8010;
  GL_CONVOLUTION_2D                                  = $8011;
  GL_SEPARABLE_2D                                    = $8012;
  GL_CONVOLUTION_BORDER_MODE                         = $8013;
  GL_CONVOLUTION_FILTER_SCALE                        = $8014;
  GL_CONVOLUTION_FILTER_BIAS                         = $8015;
  GL_REDUCE                                          = $8016;
  GL_CONVOLUTION_FORMAT                              = $8017;
  GL_CONVOLUTION_WIDTH                               = $8018;
  GL_CONVOLUTION_HEIGHT                              = $8019;
  GL_MAX_CONVOLUTION_WIDTH                           = $801A;
  GL_MAX_CONVOLUTION_HEIGHT                          = $801B;
  GL_POST_CONVOLUTION_RED_SCALE                      = $801C;
  GL_POST_CONVOLUTION_GREEN_SCALE                    = $801D;
  GL_POST_CONVOLUTION_BLUE_SCALE                     = $801E;
  GL_POST_CONVOLUTION_ALPHA_SCALE                    = $801F;
  GL_POST_CONVOLUTION_RED_BIAS                       = $8020;
  GL_POST_CONVOLUTION_GREEN_BIAS                     = $8021;
  GL_POST_CONVOLUTION_BLUE_BIAS                      = $8022;
  GL_POST_CONVOLUTION_ALPHA_BIAS                     = $8023;
  GL_HISTOGRAM                                       = $8024;
  GL_PROXY_HISTOGRAM                                 = $8025;
  GL_HISTOGRAM_WIDTH                                 = $8026;
  GL_HISTOGRAM_FORMAT                                = $8027;
  GL_HISTOGRAM_RED_SIZE                              = $8028;
  GL_HISTOGRAM_GREEN_SIZE                            = $8029;
  GL_HISTOGRAM_BLUE_SIZE                             = $802A;
  GL_HISTOGRAM_ALPHA_SIZE                            = $802B;
  GL_HISTOGRAM_LUMINANCE_SIZE                        = $802C;
  GL_HISTOGRAM_SINK                                  = $802D;
  GL_MINMAX                                          = $802E;
  GL_MINMAX_FORMAT                                   = $802F;
  GL_MINMAX_SINK                                     = $8030;
  GL_TABLE_TOO_LARGE                                 = $8031;
  GL_COLOR_MATRIX                                    = $80B1;
  GL_COLOR_MATRIX_STACK_DEPTH                        = $80B2;
  GL_MAX_COLOR_MATRIX_STACK_DEPTH                    = $80B3;
  GL_POST_COLOR_MATRIX_RED_SCALE                     = $80B4;
  GL_POST_COLOR_MATRIX_GREEN_SCALE                   = $80B5;
  GL_POST_COLOR_MATRIX_BLUE_SCALE                    = $80B6;
  GL_POST_COLOR_MATRIX_ALPHA_SCALE                   = $80B7;
  GL_POST_COLOR_MATRIX_RED_BIAS                      = $80B8;
  GL_POST_COLOR_MATRIX_GREEN_BIAS                    = $80B9;
  GL_POST_COLOR_MATRIX_BLUE_BIAS                     = $80BA;
  GL_POST_COLOR_MATRIX_ALPHA_BIAS                    = $80BB;
  GL_COLOR_TABLE                                     = $80D0;
  GL_POST_CONVOLUTION_COLOR_TABLE                    = $80D1;
  GL_POST_COLOR_MATRIX_COLOR_TABLE                   = $80D2;
  GL_PROXY_COLOR_TABLE                               = $80D3;
  GL_PROXY_POST_CONVOLUTION_COLOR_TABLE              = $80D4;
  GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE             = $80D5;
  GL_COLOR_TABLE_SCALE                               = $80D6;
  GL_COLOR_TABLE_BIAS                                = $80D7;
  GL_COLOR_TABLE_FORMAT                              = $80D8;
  GL_COLOR_TABLE_WIDTH                               = $80D9;
  GL_COLOR_TABLE_RED_SIZE                            = $80DA;
  GL_COLOR_TABLE_GREEN_SIZE                          = $80DB;
  GL_COLOR_TABLE_BLUE_SIZE                           = $80DC;
  GL_COLOR_TABLE_ALPHA_SIZE                          = $80DD;
  GL_COLOR_TABLE_LUMINANCE_SIZE                      = $80DE;
  GL_COLOR_TABLE_INTENSITY_SIZE                      = $80DF;
  GL_CONSTANT_BORDER                                 = $8151;
  GL_REPLICATE_BORDER                                = $8153;
  GL_CONVOLUTION_BORDER_COLOR                        = $8154;

  // GL_ARB_matrix_palette
  GL_MATRIX_PALETTE_ARB                              = $8840;
  GL_MAX_MATRIX_PALETTE_STACK_DEPTH_ARB              = $8841;
  GL_MAX_PALETTE_MATRICES_ARB                        = $8842;
  GL_CURRENT_PALETTE_MATRIX_ARB                      = $8843;
  GL_MATRIX_INDEX_ARRAY_ARB                          = $8844;
  GL_CURRENT_MATRIX_INDEX_ARB                        = $8845;
  GL_MATRIX_INDEX_ARRAY_SIZE_ARB                     = $8846;
  GL_MATRIX_INDEX_ARRAY_TYPE_ARB                     = $8847;
  GL_MATRIX_INDEX_ARRAY_STRIDE_ARB                   = $8848;
  GL_MATRIX_INDEX_ARRAY_POINTER_ARB                  = $8849;

  // GL_ARB_multisample
  GL_MULTISAMPLE_ARB                                 = $809D;
  GL_SAMPLE_ALPHA_TO_COVERAGE_ARB                    = $809E;
  GL_SAMPLE_ALPHA_TO_ONE_ARB                         = $809F;
  GL_SAMPLE_COVERAGE_ARB                             = $80A0;
  GL_SAMPLE_BUFFERS_ARB                              = $80A8;
  GL_SAMPLES_ARB                                     = $80A9;
  GL_SAMPLE_COVERAGE_VALUE_ARB                       = $80AA;
  GL_SAMPLE_COVERAGE_INVERT_ARB                      = $80AB;
  GL_MULTISAMPLE_BIT_ARB                             = $20000000;

  // GL_ARB_multitexture
  GL_TEXTURE0_ARB                                    = $84C0;
  GL_TEXTURE1_ARB                                    = $84C1;
  GL_TEXTURE2_ARB                                    = $84C2;
  GL_TEXTURE3_ARB                                    = $84C3;
  GL_TEXTURE4_ARB                                    = $84C4;
  GL_TEXTURE5_ARB                                    = $84C5;
  GL_TEXTURE6_ARB                                    = $84C6;
  GL_TEXTURE7_ARB                                    = $84C7;
  GL_TEXTURE8_ARB                                    = $84C8;
  GL_TEXTURE9_ARB                                    = $84C9;
  GL_TEXTURE10_ARB                                   = $84CA;
  GL_TEXTURE11_ARB                                   = $84CB;
  GL_TEXTURE12_ARB                                   = $84CC;
  GL_TEXTURE13_ARB                                   = $84CD;
  GL_TEXTURE14_ARB                                   = $84CE;
  GL_TEXTURE15_ARB                                   = $84CF;
  GL_TEXTURE16_ARB                                   = $84D0;
  GL_TEXTURE17_ARB                                   = $84D1;
  GL_TEXTURE18_ARB                                   = $84D2;
  GL_TEXTURE19_ARB                                   = $84D3;
  GL_TEXTURE20_ARB                                   = $84D4;
  GL_TEXTURE21_ARB                                   = $84D5;
  GL_TEXTURE22_ARB                                   = $84D6;
  GL_TEXTURE23_ARB                                   = $84D7;
  GL_TEXTURE24_ARB                                   = $84D8;
  GL_TEXTURE25_ARB                                   = $84D9;
  GL_TEXTURE26_ARB                                   = $84DA;
  GL_TEXTURE27_ARB                                   = $84DB;
  GL_TEXTURE28_ARB                                   = $84DC;
  GL_TEXTURE29_ARB                                   = $84DD;
  GL_TEXTURE30_ARB                                   = $84DE;
  GL_TEXTURE31_ARB                                   = $84DF;
  GL_ACTIVE_TEXTURE_ARB                              = $84E0;
  GL_CLIENT_ACTIVE_TEXTURE_ARB                       = $84E1;
  GL_MAX_TEXTURE_UNITS_ARB                           = $84E2;

  // GL_ARB_point_parameters
  GL_POINT_SIZE_MIN_ARB                              = $8126;
  GL_POINT_SIZE_MAX_ARB                              = $8127;
  GL_POINT_FADE_THRESHOLD_SIZE_ARB                   = $8128;
  GL_POINT_DISTANCE_ATTENUATION_ARB                  = $8129;

  // GL_ARB_shadow
  GL_TEXTURE_COMPARE_MODE_ARB                        = $884C;
  GL_TEXTURE_COMPARE_FUNC_ARB                        = $884D;
  GL_COMPARE_R_TO_TEXTURE_ARB                        = $884E;

  // GL_ARB_shadow_ambient
  GL_TEXTURE_COMPARE_FAIL_VALUE_ARB                  = $80BF;

  // GL_ARB_texture_border_clamp
  GL_CLAMP_TO_BORDER_ARB                             = $812D;

  // GL_ARB_texture_compression
  GL_COMPRESSED_ALPHA_ARB                            = $84E9;
  GL_COMPRESSED_LUMINANCE_ARB                        = $84EA;
  GL_COMPRESSED_LUMINANCE_ALPHA_ARB                  = $84EB;
  GL_COMPRESSED_INTENSITY_ARB                        = $84EC;
  GL_COMPRESSED_RGB_ARB                              = $84ED;
  GL_COMPRESSED_RGBA_ARB                             = $84EE;
  GL_TEXTURE_COMPRESSION_HINT_ARB                    = $84EF;
  GL_TEXTURE_COMPRESSED_IMAGE_SIZE_ARB               = $86A0;
  GL_TEXTURE_COMPRESSED_ARB                          = $86A1;
  GL_NUM_COMPRESSED_TEXTURE_FORMATS_ARB              = $86A2;
  GL_COMPRESSED_TEXTURE_FORMATS_ARB                  = $86A3;

  // GL_ARB_texture_cube_map
  GL_NORMAL_MAP_ARB                                  = $8511;
  GL_REFLECTION_MAP_ARB                              = $8512;
  GL_TEXTURE_CUBE_MAP_ARB                            = $8513;
  GL_TEXTURE_BINDING_CUBE_MAP_ARB                    = $8514;
  GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB                 = $8515;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB                 = $8516;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB                 = $8517;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB                 = $8518;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB                 = $8519;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB                 = $851A;
  GL_PROXY_TEXTURE_CUBE_MAP_ARB                      = $851B;
  GL_MAX_CUBE_MAP_TEXTURE_SIZE_ARB                   = $851C;

  // GL_ARB_texture_env_combine
  GL_COMBINE_ARB                                     = $8570;
  GL_COMBINE_RGB_ARB                                 = $8571;
  GL_COMBINE_ALPHA_ARB                               = $8572;
  GL_SOURCE0_RGB_ARB                                 = $8580;
  GL_SOURCE1_RGB_ARB                                 = $8581;
  GL_SOURCE2_RGB_ARB                                 = $8582;
  GL_SOURCE0_ALPHA_ARB                               = $8588;
  GL_SOURCE1_ALPHA_ARB                               = $8589;
  GL_SOURCE2_ALPHA_ARB                               = $858A;
  GL_OPERAND0_RGB_ARB                                = $8590;
  GL_OPERAND1_RGB_ARB                                = $8591;
  GL_OPERAND2_RGB_ARB                                = $8592;
  GL_OPERAND0_ALPHA_ARB                              = $8598;
  GL_OPERAND1_ALPHA_ARB                              = $8599;
  GL_OPERAND2_ALPHA_ARB                              = $859A;
  GL_RGB_SCALE_ARB                                   = $8573;
  GL_ADD_SIGNED_ARB                                  = $8574;
  GL_INTERPOLATE_ARB                                 = $8575;
  GL_SUBTRACT_ARB                                    = $84E7;
  GL_CONSTANT_ARB                                    = $8576;
  GL_PRIMARY_COLOR_ARB                               = $8577;
  GL_PREVIOUS_ARB                                    = $8578;

  // GL_ARB_texture_env_dot3
  GL_DOT3_RGB_ARB                                    = $86AE;
  GL_DOT3_RGBA_ARB                                   = $86AF;

  // GL_ARB_texture_mirrored_repeat
  GL_MIRRORED_REPEAT_ARB                             = $8370;

  // GL_ARB_transpose_matrix
  GL_TRANSPOSE_MODELVIEW_MATRIX_ARB                  = $84E3;
  GL_TRANSPOSE_PROJECTION_MATRIX_ARB                 = $84E4;
  GL_TRANSPOSE_TEXTURE_MATRIX_ARB                    = $84E5;
  GL_TRANSPOSE_COLOR_MATRIX_ARB                      = $84E6;

  // GL_ARB_vertex_blend
  GL_MAX_VERTEX_UNITS_ARB                            = $86A4;
  GL_ACTIVE_VERTEX_UNITS_ARB                         = $86A5;
  GL_WEIGHT_SUM_UNITY_ARB                            = $86A6;
  GL_VERTEX_BLEND_ARB                                = $86A7;
  GL_CURRENT_WEIGHT_ARB                              = $86A8;
  GL_WEIGHT_ARRAY_TYPE_ARB                           = $86A9;
  GL_WEIGHT_ARRAY_STRIDE_ARB                         = $86AA;
  GL_WEIGHT_ARRAY_SIZE_ARB                           = $86AB;
  GL_WEIGHT_ARRAY_POINTER_ARB                        = $86AC;
  GL_WEIGHT_ARRAY_ARB                                = $86AD;
  GL_MODELVIEW0_ARB                                  = $1700;
  GL_MODELVIEW1_ARB                                  = $850A;
  GL_MODELVIEW2_ARB                                  = $8722;
  GL_MODELVIEW3_ARB                                  = $8723;
  GL_MODELVIEW4_ARB                                  = $8724;
  GL_MODELVIEW5_ARB                                  = $8725;
  GL_MODELVIEW6_ARB                                  = $8726;
  GL_MODELVIEW7_ARB                                  = $8727;
  GL_MODELVIEW8_ARB                                  = $8728;
  GL_MODELVIEW9_ARB                                  = $8729;
  GL_MODELVIEW10_ARB                                 = $872A;
  GL_MODELVIEW11_ARB                                 = $872B;
  GL_MODELVIEW12_ARB                                 = $872C;
  GL_MODELVIEW13_ARB                                 = $872D;
  GL_MODELVIEW14_ARB                                 = $872E;
  GL_MODELVIEW15_ARB                                 = $872F;
  GL_MODELVIEW16_ARB                                 = $8730;
  GL_MODELVIEW17_ARB                                 = $8731;
  GL_MODELVIEW18_ARB                                 = $8732;
  GL_MODELVIEW19_ARB                                 = $8733;
  GL_MODELVIEW20_ARB                                 = $8734;
  GL_MODELVIEW21_ARB                                 = $8735;
  GL_MODELVIEW22_ARB                                 = $8736;
  GL_MODELVIEW23_ARB                                 = $8737;
  GL_MODELVIEW24_ARB                                 = $8738;
  GL_MODELVIEW25_ARB                                 = $8739;
  GL_MODELVIEW26_ARB                                 = $873A;
  GL_MODELVIEW27_ARB                                 = $873B;
  GL_MODELVIEW28_ARB                                 = $873C;
  GL_MODELVIEW29_ARB                                 = $873D;
  GL_MODELVIEW30_ARB                                 = $873E;
  GL_MODELVIEW31_ARB                                 = $873F;

  // GL_ARB_vertex_buffer_object
  GL_BUFFER_SIZE_ARB                                 = $8764;
  GL_BUFFER_USAGE_ARB                                = $8765;
  GL_ARRAY_BUFFER_ARB                                = $8892;
  GL_ELEMENT_ARRAY_BUFFER_ARB                        = $8893;
  GL_ARRAY_BUFFER_BINDING_ARB                        = $8894;
  GL_ELEMENT_ARRAY_BUFFER_BINDING_ARB                = $8895;
  GL_VERTEX_ARRAY_BUFFER_BINDING_ARB                 = $8896;
  GL_NORMAL_ARRAY_BUFFER_BINDING_ARB                 = $8897;
  GL_COLOR_ARRAY_BUFFER_BINDING_ARB                  = $8898;
  GL_INDEX_ARRAY_BUFFER_BINDING_ARB                  = $8899;
  GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING_ARB          = $889A;
  GL_EDGE_FLAG_ARRAY_BUFFER_BINDING_ARB              = $889B;
  GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING_ARB        = $889C;
  GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING_ARB         = $889D;
  GL_WEIGHT_ARRAY_BUFFER_BINDING_ARB                 = $889E;
  GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING_ARB          = $889F;
  GL_READ_ONLY_ARB                                   = $88B8;
  GL_WRITE_ONLY_ARB                                  = $88B9;
  GL_READ_WRITE_ARB                                  = $88BA;
  GL_BUFFER_ACCESS_ARB                               = $88BB;
  GL_BUFFER_MAPPED_ARB                               = $88BC;
  GL_BUFFER_MAP_POINTER_ARB                          = $88BD;
  GL_STREAM_DRAW_ARB                                 = $88E0;
  GL_STREAM_READ_ARB                                 = $88E1;
  GL_STREAM_COPY_ARB                                 = $88E2;
  GL_STATIC_DRAW_ARB                                 = $88E4;
  GL_STATIC_READ_ARB                                 = $88E5;
  GL_STATIC_COPY_ARB                                 = $88E6;
  GL_DYNAMIC_DRAW_ARB                                = $88E8;
  GL_DYNAMIC_READ_ARB                                = $88E9;
  GL_DYNAMIC_COPY_ARB                                = $88EA;

  // GL 1.5 ARB less version
  GL_BUFFER_SIZE                                 = $8764;
  GL_BUFFER_USAGE                                = $8765;
  GL_ARRAY_BUFFER                                = $8892;
  GL_ELEMENT_ARRAY_BUFFER                        = $8893;
  GL_ARRAY_BUFFER_BINDING                        = $8894;
  GL_ELEMENT_ARRAY_BUFFER_BINDING                = $8895;
  GL_VERTEX_ARRAY_BUFFER_BINDING                 = $8896;
  GL_NORMAL_ARRAY_BUFFER_BINDING                 = $8897;
  GL_COLOR_ARRAY_BUFFER_BINDING                  = $8898;
  GL_INDEX_ARRAY_BUFFER_BINDING                  = $8899;
  GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING          = $889A;
  GL_EDGE_FLAG_ARRAY_BUFFER_BINDING              = $889B;
  GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING        = $889C;
  GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING         = $889D;
  GL_WEIGHT_ARRAY_BUFFER_BINDING                 = $889E;
  GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING          = $889F;
  GL_READ_ONLY                                   = $88B8;
  GL_WRITE_ONLY                                  = $88B9;
  GL_READ_WRITE                                  = $88BA;
  GL_BUFFER_ACCESS                               = $88BB;
  GL_BUFFER_MAPPED                               = $88BC;
  GL_BUFFER_MAP_POINTER                          = $88BD;
  GL_STREAM_DRAW                                 = $88E0;
  GL_STREAM_READ                                 = $88E1;
  GL_STREAM_COPY                                 = $88E2;
  GL_STATIC_DRAW                                 = $88E4;
  GL_STATIC_READ                                 = $88E5;
  GL_STATIC_COPY                                 = $88E6;
  GL_DYNAMIC_DRAW                                = $88E8;
  GL_DYNAMIC_READ                                = $88E9;
  GL_DYNAMIC_COPY                                = $88EA;

  // GL_ARB_vertex_program
  GL_COLOR_SUM_ARB                                   = $8458;
  GL_VERTEX_PROGRAM_ARB                              = $8620;
  GL_VERTEX_ATTRIB_ARRAY_ENABLED_ARB                 = $8622;
  GL_VERTEX_ATTRIB_ARRAY_SIZE_ARB                    = $8623;
  GL_VERTEX_ATTRIB_ARRAY_STRIDE_ARB                  = $8624;
  GL_VERTEX_ATTRIB_ARRAY_TYPE_ARB                    = $8625;
  GL_CURRENT_VERTEX_ATTRIB_ARB                       = $8626;
  GL_PROGRAM_LENGTH_ARB                              = $8627;
  GL_PROGRAM_STRING_ARB                              = $8628;
  GL_MAX_PROGRAM_MATRIX_STACK_DEPTH_ARB              = $862E;
  GL_MAX_PROGRAM_MATRICES_ARB                        = $862F;
  GL_CURRENT_MATRIX_STACK_DEPTH_ARB                  = $8640;
  GL_CURRENT_MATRIX_ARB                              = $8641;
  GL_VERTEX_PROGRAM_POINT_SIZE_ARB                   = $8642;
  GL_VERTEX_PROGRAM_TWO_SIDE_ARB                     = $8643;
  GL_VERTEX_ATTRIB_ARRAY_POINTER_ARB                 = $8645;
  GL_PROGRAM_ERROR_POSITION_ARB                      = $864B;
  GL_PROGRAM_BINDING_ARB                             = $8677;
  GL_MAX_VERTEX_ATTRIBS_ARB                          = $8869;
  GL_VERTEX_ATTRIB_ARRAY_NORMALIZED_ARB              = $886A;
  GL_PROGRAM_ERROR_STRING_ARB                        = $8874;
  GL_PROGRAM_FORMAT_ASCII_ARB                        = $8875;
  GL_PROGRAM_FORMAT_ARB                              = $8876;
  GL_PROGRAM_INSTRUCTIONS_ARB                        = $88A0;
  GL_MAX_PROGRAM_INSTRUCTIONS_ARB                    = $88A1;
  GL_PROGRAM_NATIVE_INSTRUCTIONS_ARB                 = $88A2;
  GL_MAX_PROGRAM_NATIVE_INSTRUCTIONS_ARB             = $88A3;
  GL_PROGRAM_TEMPORARIES_ARB                         = $88A4;
  GL_MAX_PROGRAM_TEMPORARIES_ARB                     = $88A5;
  GL_PROGRAM_NATIVE_TEMPORARIES_ARB                  = $88A6;
  GL_MAX_PROGRAM_NATIVE_TEMPORARIES_ARB              = $88A7;
  GL_PROGRAM_PARAMETERS_ARB                          = $88A8;
  GL_MAX_PROGRAM_PARAMETERS_ARB                      = $88A9;
  GL_PROGRAM_NATIVE_PARAMETERS_ARB                   = $88AA;
  GL_MAX_PROGRAM_NATIVE_PARAMETERS_ARB               = $88AB;
  GL_PROGRAM_ATTRIBS_ARB                             = $88AC;
  GL_MAX_PROGRAM_ATTRIBS_ARB                         = $88AD;
  GL_PROGRAM_NATIVE_ATTRIBS_ARB                      = $88AE;
  GL_MAX_PROGRAM_NATIVE_ATTRIBS_ARB                  = $88AF;
  GL_PROGRAM_ADDRESS_REGISTERS_ARB                   = $88B0;
  GL_MAX_PROGRAM_ADDRESS_REGISTERS_ARB               = $88B1;
  GL_PROGRAM_NATIVE_ADDRESS_REGISTERS_ARB            = $88B2;
  GL_MAX_PROGRAM_NATIVE_ADDRESS_REGISTERS_ARB        = $88B3;
  GL_MAX_PROGRAM_LOCAL_PARAMETERS_ARB                = $88B4;
  GL_MAX_PROGRAM_ENV_PARAMETERS_ARB                  = $88B5;
  GL_PROGRAM_UNDER_NATIVE_LIMITS_ARB                 = $88B6;
  GL_TRANSPOSE_CURRENT_MATRIX_ARB                    = $88B7;
  GL_MATRIX0_ARB                                     = $88C0;
  GL_MATRIX1_ARB                                     = $88C1;
  GL_MATRIX2_ARB                                     = $88C2;
  GL_MATRIX3_ARB                                     = $88C3;
  GL_MATRIX4_ARB                                     = $88C4;
  GL_MATRIX5_ARB                                     = $88C5;
  GL_MATRIX6_ARB                                     = $88C6;
  GL_MATRIX7_ARB                                     = $88C7;
  GL_MATRIX8_ARB                                     = $88C8;
  GL_MATRIX9_ARB                                     = $88C9;
  GL_MATRIX10_ARB                                    = $88CA;
  GL_MATRIX11_ARB                                    = $88CB;
  GL_MATRIX12_ARB                                    = $88CC;
  GL_MATRIX13_ARB                                    = $88CD;
  GL_MATRIX14_ARB                                    = $88CE;
  GL_MATRIX15_ARB                                    = $88CF;
  GL_MATRIX16_ARB                                    = $88D0;
  GL_MATRIX17_ARB                                    = $88D1;
  GL_MATRIX18_ARB                                    = $88D2;
  GL_MATRIX19_ARB                                    = $88D3;
  GL_MATRIX20_ARB                                    = $88D4;
  GL_MATRIX21_ARB                                    = $88D5;
  GL_MATRIX22_ARB                                    = $88D6;
  GL_MATRIX23_ARB                                    = $88D7;
  GL_MATRIX24_ARB                                    = $88D8;
  GL_MATRIX25_ARB                                    = $88D9;
  GL_MATRIX26_ARB                                    = $88DA;
  GL_MATRIX27_ARB                                    = $88DB;
  GL_MATRIX28_ARB                                    = $88DC;
  GL_MATRIX29_ARB                                    = $88DD;
  GL_MATRIX30_ARB                                    = $88DE;
  GL_MATRIX31_ARB                                    = $88DF;

  // GL_ATI_draw_buffers
  GL_MAX_DRAW_BUFFERS_ATI                            = $8824;
  GL_DRAW_BUFFER0_ATI                                = $8825;
  GL_DRAW_BUFFER1_ATI                                = $8826;
  GL_DRAW_BUFFER2_ATI                                = $8827;
  GL_DRAW_BUFFER3_ATI                                = $8828;
  GL_DRAW_BUFFER4_ATI                                = $8829;
  GL_DRAW_BUFFER5_ATI                                = $882A;
  GL_DRAW_BUFFER6_ATI                                = $882B;
  GL_DRAW_BUFFER7_ATI                                = $882C;
  GL_DRAW_BUFFER8_ATI                                = $882D;
  GL_DRAW_BUFFER9_ATI                                = $882E;
  GL_DRAW_BUFFER10_ATI                               = $882F;
  GL_DRAW_BUFFER11_ATI                               = $8830;
  GL_DRAW_BUFFER12_ATI                               = $8831;
  GL_DRAW_BUFFER13_ATI                               = $8832;
  GL_DRAW_BUFFER14_ATI                               = $8833;
  GL_DRAW_BUFFER15_ATI                               = $8834;

  // GL_ATI_element_array
  GL_ELEMENT_ARRAY_ATI                               = $8768;
  GL_ELEMENT_ARRAY_TYPE_ATI                          = $8769;
  GL_ELEMENT_ARRAY_POINTER_ATI                       = $876A;

  // GL_ATI_envmap_bumpmap
  GL_BUMP_ROT_MATRIX_ATI                             = $8775;
  GL_BUMP_ROT_MATRIX_SIZE_ATI                        = $8776;
  GL_BUMP_NUM_TEX_UNITS_ATI                          = $8777;
  GL_BUMP_TEX_UNITS_ATI                              = $8778;
  GL_DUDV_ATI                                        = $8779;
  GL_DU8DV8_ATI                                      = $877A;
  GL_BUMP_ENVMAP_ATI                                 = $877B;
  GL_BUMP_TARGET_ATI                                 = $877C;

  // GL_ATI_fragment_shader
  GL_FRAGMENT_SHADER_ATI                             = $8920;
  GL_REG_0_ATI                                       = $8921;
  GL_REG_1_ATI                                       = $8922;
  GL_REG_2_ATI                                       = $8923;
  GL_REG_3_ATI                                       = $8924;
  GL_REG_4_ATI                                       = $8925;
  GL_REG_5_ATI                                       = $8926;
  GL_REG_6_ATI                                       = $8927;
  GL_REG_7_ATI                                       = $8928;
  GL_REG_8_ATI                                       = $8929;
  GL_REG_9_ATI                                       = $892A;
  GL_REG_10_ATI                                      = $892B;
  GL_REG_11_ATI                                      = $892C;
  GL_REG_12_ATI                                      = $892D;
  GL_REG_13_ATI                                      = $892E;
  GL_REG_14_ATI                                      = $892F;
  GL_REG_15_ATI                                      = $8930;
  GL_REG_16_ATI                                      = $8931;
  GL_REG_17_ATI                                      = $8932;
  GL_REG_18_ATI                                      = $8933;
  GL_REG_19_ATI                                      = $8934;
  GL_REG_20_ATI                                      = $8935;
  GL_REG_21_ATI                                      = $8936;
  GL_REG_22_ATI                                      = $8937;
  GL_REG_23_ATI                                      = $8938;
  GL_REG_24_ATI                                      = $8939;
  GL_REG_25_ATI                                      = $893A;
  GL_REG_26_ATI                                      = $893B;
  GL_REG_27_ATI                                      = $893C;
  GL_REG_28_ATI                                      = $893D;
  GL_REG_29_ATI                                      = $893E;
  GL_REG_30_ATI                                      = $893F;
  GL_REG_31_ATI                                      = $8940;
  GL_CON_0_ATI                                       = $8941;
  GL_CON_1_ATI                                       = $8942;
  GL_CON_2_ATI                                       = $8943;
  GL_CON_3_ATI                                       = $8944;
  GL_CON_4_ATI                                       = $8945;
  GL_CON_5_ATI                                       = $8946;
  GL_CON_6_ATI                                       = $8947;
  GL_CON_7_ATI                                       = $8948;
  GL_CON_8_ATI                                       = $8949;
  GL_CON_9_ATI                                       = $894A;
  GL_CON_10_ATI                                      = $894B;
  GL_CON_11_ATI                                      = $894C;
  GL_CON_12_ATI                                      = $894D;
  GL_CON_13_ATI                                      = $894E;
  GL_CON_14_ATI                                      = $894F;
  GL_CON_15_ATI                                      = $8950;
  GL_CON_16_ATI                                      = $8951;
  GL_CON_17_ATI                                      = $8952;
  GL_CON_18_ATI                                      = $8953;
  GL_CON_19_ATI                                      = $8954;
  GL_CON_20_ATI                                      = $8955;
  GL_CON_21_ATI                                      = $8956;
  GL_CON_22_ATI                                      = $8957;
  GL_CON_23_ATI                                      = $8958;
  GL_CON_24_ATI                                      = $8959;
  GL_CON_25_ATI                                      = $895A;
  GL_CON_26_ATI                                      = $895B;
  GL_CON_27_ATI                                      = $895C;
  GL_CON_28_ATI                                      = $895D;
  GL_CON_29_ATI                                      = $895E;
  GL_CON_30_ATI                                      = $895F;
  GL_CON_31_ATI                                      = $8960;
  GL_MOV_ATI                                         = $8961;
  GL_ADD_ATI                                         = $8963;
  GL_MUL_ATI                                         = $8964;
  GL_SUB_ATI                                         = $8965;
  GL_DOT3_ATI                                        = $8966;
  GL_DOT4_ATI                                        = $8967;
  GL_MAD_ATI                                         = $8968;
  GL_LERP_ATI                                        = $8969;
  GL_CND_ATI                                         = $896A;
  GL_CND0_ATI                                        = $896B;
  GL_DOT2_ADD_ATI                                    = $896C;
  GL_SECONDARY_INTERPOLATOR_ATI                      = $896D;
  GL_NUM_FRAGMENT_REGISTERS_ATI                      = $896E;
  GL_NUM_FRAGMENT_CONSTANTS_ATI                      = $896F;
  GL_NUM_PASSES_ATI                                  = $8970;
  GL_NUM_INSTRUCTIONS_PER_PASS_ATI                   = $8971;
  GL_NUM_INSTRUCTIONS_TOTAL_ATI                      = $8972;
  GL_NUM_INPUT_INTERPOLATOR_COMPONENTS_ATI           = $8973;
  GL_NUM_LOOPBACK_COMPONENTS_ATI                     = $8974;
  GL_COLOR_ALPHA_PAIRING_ATI                         = $8975;
  GL_SWIZZLE_STR_ATI                                 = $8976;
  GL_SWIZZLE_STQ_ATI                                 = $8977;
  GL_SWIZZLE_STR_DR_ATI                              = $8978;
  GL_SWIZZLE_STQ_DQ_ATI                              = $8979;
  GL_SWIZZLE_STRQ_ATI                                = $897A;
  GL_SWIZZLE_STRQ_DQ_ATI                             = $897B;
  GL_RED_BIT_ATI                                     = $00000001;
  GL_GREEN_BIT_ATI                                   = $00000002;
  GL_BLUE_BIT_ATI                                    = $00000004;
  GL_2X_BIT_ATI                                      = $00000001;
  GL_4X_BIT_ATI                                      = $00000002;
  GL_8X_BIT_ATI                                      = $00000004;
  GL_HALF_BIT_ATI                                    = $00000008;
  GL_QUARTER_BIT_ATI                                 = $00000010;
  GL_EIGHTH_BIT_ATI                                  = $00000020;
  GL_SATURATE_BIT_ATI                                = $00000040;
  GL_COMP_BIT_ATI                                    = $00000002;
  GL_NEGATE_BIT_ATI                                  = $00000004;
  GL_BIAS_BIT_ATI                                    = $00000008;

  // GL_ATI_pn_triangles
  GL_PN_TRIANGLES_ATI                                = $87F0;
  GL_MAX_PN_TRIANGLES_TESSELATION_LEVEL_ATI          = $87F1;
  GL_PN_TRIANGLES_POINT_MODE_ATI                     = $87F2;
  GL_PN_TRIANGLES_NORMAL_MODE_ATI                    = $87F3;
  GL_PN_TRIANGLES_TESSELATION_LEVEL_ATI              = $87F4;
  GL_PN_TRIANGLES_POINT_MODE_LINEAR_ATI              = $87F5;
  GL_PN_TRIANGLES_POINT_MODE_CUBIC_ATI               = $87F6;
  GL_PN_TRIANGLES_NORMAL_MODE_LINEAR_ATI             = $87F7;
  GL_PN_TRIANGLES_NORMAL_MODE_QUADRATIC_ATI          = $87F8;

  // GL_ATI_separate_stencil
  GL_STENCIL_BACK_FUNC_ATI                           = $8800;
  GL_STENCIL_BACK_FAIL_ATI                           = $8801;
  GL_STENCIL_BACK_PASS_DEPTH_FAIL_ATI                = $8802;
  GL_STENCIL_BACK_PASS_DEPTH_PASS_ATI                = $8803;

  // GL_ATI_text_fragment_shader
  GL_TEXT_FRAGMENT_SHADER_ATI                        = $8200;

  // GL_ATI_texture_env_combine3
  GL_MODULATE_ADD_ATI                                = $8744;
  GL_MODULATE_SIGNED_ADD_ATI                         = $8745;
  GL_MODULATE_SUBTRACT_ATI                           = $8746;

  // GL_ATI_texture_float
  GL_RGBA_FLOAT32_ATI                                = $8814;
  GL_RGB_FLOAT32_ATI                                 = $8815;
  GL_ALPHA_FLOAT32_ATI                               = $8816;
  GL_INTENSITY_FLOAT32_ATI                           = $8817;
  GL_LUMINANCE_FLOAT32_ATI                           = $8818;
  GL_LUMINANCE_ALPHA_FLOAT32_ATI                     = $8819;
  GL_RGBA_FLOAT16_ATI                                = $881A;
  GL_RGB_FLOAT16_ATI                                 = $881B;
  GL_ALPHA_FLOAT16_ATI                               = $881C;
  GL_INTENSITY_FLOAT16_ATI                           = $881D;
  GL_LUMINANCE_FLOAT16_ATI                           = $881E;
  GL_LUMINANCE_ALPHA_FLOAT16_ATI                     = $881F;

  // GL_ATI_texture_mirror_once
  GL_MIRROR_CLAMP_ATI                                = $8742;
  GL_MIRROR_CLAMP_TO_EDGE_ATI                        = $8743;

  // GL_ATI_vertex_array_object
  GL_STATIC_ATI                                      = $8760;
  GL_DYNAMIC_ATI                                     = $8761;
  GL_PRESERVE_ATI                                    = $8762;
  GL_DISCARD_ATI                                     = $8763;
  GL_OBJECT_BUFFER_SIZE_ATI                          = $8764;
  GL_OBJECT_BUFFER_USAGE_ATI                         = $8765;
  GL_ARRAY_OBJECT_BUFFER_ATI                         = $8766;
  GL_ARRAY_OBJECT_OFFSET_ATI                         = $8767;

  // GL_ATI_vertex_streams
  GL_MAX_VERTEX_STREAMS_ATI                          = $876B;
  GL_VERTEX_STREAM0_ATI                              = $876C;
  GL_VERTEX_STREAM1_ATI                              = $876D;
  GL_VERTEX_STREAM2_ATI                              = $876E;
  GL_VERTEX_STREAM3_ATI                              = $876F;
  GL_VERTEX_STREAM4_ATI                              = $8770;
  GL_VERTEX_STREAM5_ATI                              = $8771;
  GL_VERTEX_STREAM6_ATI                              = $8772;
  GL_VERTEX_STREAM7_ATI                              = $8773;
  GL_VERTEX_SOURCE_ATI                               = $8774;

  // GL_EXT_422_pixels
  GL_422_EXT                                         = $80CC;
  GL_422_REV_EXT                                     = $80CD;
  GL_422_AVERAGE_EXT                                 = $80CE;
  GL_422_REV_AVERAGE_EXT                             = $80CF;

  // GL_EXT_abgr
  GL_ABGR_EXT                                        = $8000;

  // GL_EXT_bgra
  GL_BGR_EXT                                         = $80E0;
  GL_BGRA_EXT                                        = $80E1;

  // GL_EXT_blend_color
  GL_CONSTANT_COLOR_EXT                              = $8001;
  GL_ONE_MINUS_CONSTANT_COLOR_EXT                    = $8002;
  GL_CONSTANT_ALPHA_EXT                              = $8003;
  GL_ONE_MINUS_CONSTANT_ALPHA_EXT                    = $8004;
  GL_BLEND_COLOR_EXT                                 = $8005;

  // GL_EXT_blend_func_separate
  GL_BLEND_DST_RGB_EXT                               = $80C8;
  GL_BLEND_SRC_RGB_EXT                               = $80C9;
  GL_BLEND_DST_ALPHA_EXT                             = $80CA;
  GL_BLEND_SRC_ALPHA_EXT                             = $80CB;

  // GL_EXT_blend_minmax
  GL_FUNC_ADD_EXT                                    = $8006;
  GL_MIN_EXT                                         = $8007;
  GL_MAX_EXT                                         = $8008;
  GL_BLEND_EQUATION_EXT                              = $8009;

  // GL_EXT_blend_subtract
  GL_FUNC_SUBTRACT_EXT                               = $800A;
  GL_FUNC_REVERSE_SUBTRACT_EXT                       = $800B;

  // GL_EXT_clip_volume_hint
  GL_CLIP_VOLUME_CLIPPING_HINT_EXT                   = $80F0;

  // GL_EXT_cmyka
  GL_CMYK_EXT                                        = $800C;
  GL_CMYKA_EXT                                       = $800D;
  GL_PACK_CMYK_HINT_EXT                              = $800E;
  GL_UNPACK_CMYK_HINT_EXT                            = $800F;

  // GL_EXT_compiled_vertex_array
  GL_ARRAY_ELEMENT_LOCK_FIRST_EXT                    = $81A8;
  GL_ARRAY_ELEMENT_LOCK_COUNT_EXT                    = $81A9;

  // GL_EXT_convolution
  GL_CONVOLUTION_1D_EXT                              = $8010;
  GL_CONVOLUTION_2D_EXT                              = $8011;
  GL_SEPARABLE_2D_EXT                                = $8012;
  GL_CONVOLUTION_BORDER_MODE_EXT                     = $8013;
  GL_CONVOLUTION_FILTER_SCALE_EXT                    = $8014;
  GL_CONVOLUTION_FILTER_BIAS_EXT                     = $8015;
  GL_REDUCE_EXT                                      = $8016;
  GL_CONVOLUTION_FORMAT_EXT                          = $8017;
  GL_CONVOLUTION_WIDTH_EXT                           = $8018;
  GL_CONVOLUTION_HEIGHT_EXT                          = $8019;
  GL_MAX_CONVOLUTION_WIDTH_EXT                       = $801A;
  GL_MAX_CONVOLUTION_HEIGHT_EXT                      = $801B;
  GL_POST_CONVOLUTION_RED_SCALE_EXT                  = $801C;
  GL_POST_CONVOLUTION_GREEN_SCALE_EXT                = $801D;
  GL_POST_CONVOLUTION_BLUE_SCALE_EXT                 = $801E;
  GL_POST_CONVOLUTION_ALPHA_SCALE_EXT                = $801F;
  GL_POST_CONVOLUTION_RED_BIAS_EXT                   = $8020;
  GL_POST_CONVOLUTION_GREEN_BIAS_EXT                 = $8021;
  GL_POST_CONVOLUTION_BLUE_BIAS_EXT                  = $8022;
  GL_POST_CONVOLUTION_ALPHA_BIAS_EXT                 = $8023;

  // GL_EXT_coordinate_frame
  GL_TANGENT_ARRAY_EXT                               = $8439;
  GL_BINORMAL_ARRAY_EXT                              = $843A;
  GL_CURRENT_TANGENT_EXT                             = $843B;
  GL_CURRENT_BINORMAL_EXT                            = $843C;
  GL_TANGENT_ARRAY_TYPE_EXT                          = $843E;
  GL_TANGENT_ARRAY_STRIDE_EXT                        = $843F;
  GL_BINORMAL_ARRAY_TYPE_EXT                         = $8440;
  GL_BINORMAL_ARRAY_STRIDE_EXT                       = $8441;
  GL_TANGENT_ARRAY_POINTER_EXT                       = $8442;
  GL_BINORMAL_ARRAY_POINTER_EXT                      = $8443;
  GL_MAP1_TANGENT_EXT                                = $8444;
  GL_MAP2_TANGENT_EXT                                = $8445;
  GL_MAP1_BINORMAL_EXT                               = $8446;
  GL_MAP2_BINORMAL_EXT                               = $8447;

  // GL_EXT_cull_vertex
  GL_CULL_VERTEX_EXT                                 = $81AA;
  GL_CULL_VERTEX_EYE_POSITION_EXT                    = $81AB;
  GL_CULL_VERTEX_OBJECT_POSITION_EXT                 = $81AC;

  // GL_EXT_draw_range_elements
  GL_MAX_ELEMENTS_VERTICES_EXT                       = $80E8;
  GL_MAX_ELEMENTS_INDICES_EXT                        = $80E9;

  // GL_EXT_fog_coord
  GL_FOG_COORDINATE_SOURCE_EXT                       = $8450;
  GL_FOG_COORDINATE_EXT                              = $8451;
  GL_FRAGMENT_DEPTH_EXT                              = $8452;
  GL_CURRENT_FOG_COORDINATE_EXT                      = $8453;
  GL_FOG_COORDINATE_ARRAY_TYPE_EXT                   = $8454;
  GL_FOG_COORDINATE_ARRAY_STRIDE_EXT                 = $8455;
  GL_FOG_COORDINATE_ARRAY_POINTER_EXT                = $8456;
  GL_FOG_COORDINATE_ARRAY_EXT                        = $8457;

  // GL_EXT_histogram
  GL_HISTOGRAM_EXT                                   = $8024;
  GL_PROXY_HISTOGRAM_EXT                             = $8025;
  GL_HISTOGRAM_WIDTH_EXT                             = $8026;
  GL_HISTOGRAM_FORMAT_EXT                            = $8027;
  GL_HISTOGRAM_RED_SIZE_EXT                          = $8028;
  GL_HISTOGRAM_GREEN_SIZE_EXT                        = $8029;
  GL_HISTOGRAM_BLUE_SIZE_EXT                         = $802A;
  GL_HISTOGRAM_ALPHA_SIZE_EXT                        = $802B;
  GL_HISTOGRAM_LUMINANCE_SIZE_EXT                    = $802C;
  GL_HISTOGRAM_SINK_EXT                              = $802D;
  GL_MINMAX_EXT                                      = $802E;
  GL_MINMAX_FORMAT_EXT                               = $802F;
  GL_MINMAX_SINK_EXT                                 = $8030;
  GL_TABLE_TOO_LARGE_EXT                             = $8031;

  // GL_EXT_index_array_formats
  GL_IUI_V2F_EXT                                     = $81AD;
  GL_IUI_V3F_EXT                                     = $81AE;
  GL_IUI_N3F_V2F_EXT                                 = $81AF;
  GL_IUI_N3F_V3F_EXT                                 = $81B0;
  GL_T2F_IUI_V2F_EXT                                 = $81B1;
  GL_T2F_IUI_V3F_EXT                                 = $81B2;
  GL_T2F_IUI_N3F_V2F_EXT                             = $81B3;
  GL_T2F_IUI_N3F_V3F_EXT                             = $81B4;

  // GL_EXT_index_func
  GL_INDEX_TEST_EXT                                  = $81B5;
  GL_INDEX_TEST_FUNC_EXT                             = $81B6;
  GL_INDEX_TEST_REF_EXT                              = $81B7;

  // GL_EXT_index_material
  GL_INDEX_MATERIAL_EXT                              = $81B8;
  GL_INDEX_MATERIAL_PARAMETER_EXT                    = $81B9;
  GL_INDEX_MATERIAL_FACE_EXT                         = $81BA;

  // GL_EXT_light_texture
  GL_FRAGMENT_MATERIAL_EXT                           = $8349;
  GL_FRAGMENT_NORMAL_EXT                             = $834A;
  GL_FRAGMENT_COLOR_EXT                              = $834C;
  GL_ATTENUATION_EXT                                 = $834D;
  GL_SHADOW_ATTENUATION_EXT                          = $834E;
  GL_TEXTURE_APPLICATION_MODE_EXT                    = $834F;
  GL_TEXTURE_LIGHT_EXT                               = $8350;
  GL_TEXTURE_MATERIAL_FACE_EXT                       = $8351;
  GL_TEXTURE_MATERIAL_PARAMETER_EXT                  = $8352;

  // GL_EXT_multisample
  GL_MULTISAMPLE_EXT                                 = $809D;
  GL_SAMPLE_ALPHA_TO_MASK_EXT                        = $809E;
  GL_SAMPLE_ALPHA_TO_ONE_EXT                         = $809F;
  GL_SAMPLE_MASK_EXT                                 = $80A0;
  GL_1PASS_EXT                                       = $80A1;
  GL_2PASS_0_EXT                                     = $80A2;
  GL_2PASS_1_EXT                                     = $80A3;
  GL_4PASS_0_EXT                                     = $80A4;
  GL_4PASS_1_EXT                                     = $80A5;
  GL_4PASS_2_EXT                                     = $80A6;
  GL_4PASS_3_EXT                                     = $80A7;
  GL_SAMPLE_BUFFERS_EXT                              = $80A8;
  GL_SAMPLES_EXT                                     = $80A9;
  GL_SAMPLE_MASK_VALUE_EXT                           = $80AA;
  GL_SAMPLE_MASK_INVERT_EXT                          = $80AB;
  GL_SAMPLE_PATTERN_EXT                              = $80AC;
  GL_MULTISAMPLE_BIT_EXT                             = $20000000;

  // GL_EXT_packed_pixels
  GL_UNSIGNED_BYTE_3_3_2_EXT                         = $8032;
  GL_UNSIGNED_SHORT_4_4_4_4_EXT                      = $8033;
  GL_UNSIGNED_SHORT_5_5_5_1_EXT                      = $8034;
  GL_UNSIGNED_INT_8_8_8_8_EXT                        = $8035;
  GL_UNSIGNED_INT_10_10_10_2_EXT                     = $8036;

  // GL_EXT_paletted_texture
  GL_COLOR_INDEX1_EXT                                = $80E2;
  GL_COLOR_INDEX2_EXT                                = $80E3;
  GL_COLOR_INDEX4_EXT                                = $80E4;
  GL_COLOR_INDEX8_EXT                                = $80E5;
  GL_COLOR_INDEX12_EXT                               = $80E6;
  GL_COLOR_INDEX16_EXT                               = $80E7;
  GL_TEXTURE_INDEX_SIZE_EXT                          = $80ED;

  // GL_EXT_pixel_transform
  GL_PIXEL_TRANSFORM_2D_EXT                          = $8330;
  GL_PIXEL_MAG_FILTER_EXT                            = $8331;
  GL_PIXEL_MIN_FILTER_EXT                            = $8332;
  GL_PIXEL_CUBIC_WEIGHT_EXT                          = $8333;
  GL_CUBIC_EXT                                       = $8334;
  GL_AVERAGE_EXT                                     = $8335;
  GL_PIXEL_TRANSFORM_2D_STACK_DEPTH_EXT              = $8336;
  GL_MAX_PIXEL_TRANSFORM_2D_STACK_DEPTH_EXT          = $8337;
  GL_PIXEL_TRANSFORM_2D_MATRIX_EXT                   = $8338;

  // GL_EXT_point_parameters
  GL_POINT_SIZE_MIN_EXT                              = $8126;
  GL_POINT_SIZE_MAX_EXT                              = $8127;
  GL_POINT_FADE_THRESHOLD_SIZE_EXT                   = $8128;
  GL_DISTANCE_ATTENUATION_EXT                        = $8129;

  // GL_EXT_polygon_offset
  GL_POLYGON_OFFSET_EXT                              = $8037;
  GL_POLYGON_OFFSET_FACTOR_EXT                       = $8038;
  GL_POLYGON_OFFSET_BIAS_EXT                         = $8039;

  // GL_EXT_rescale_normal
  GL_RESCALE_NORMAL_EXT                              = $803A;

  // GL_EXT_secondary_color
  GL_COLOR_SUM_EXT                                   = $8458;
  GL_CURRENT_SECONDARY_COLOR_EXT                     = $8459;
  GL_SECONDARY_COLOR_ARRAY_SIZE_EXT                  = $845A;
  GL_SECONDARY_COLOR_ARRAY_TYPE_EXT                  = $845B;
  GL_SECONDARY_COLOR_ARRAY_STRIDE_EXT                = $845C;
  GL_SECONDARY_COLOR_ARRAY_POINTER_EXT               = $845D;
  GL_SECONDARY_COLOR_ARRAY_EXT                       = $845E;

  // GL_EXT_separate_specular_color
  GL_LIGHT_MODEL_COLOR_CONTROL_EXT                   = $81F8;
  GL_SINGLE_COLOR_EXT                                = $81F9;
  GL_SEPARATE_SPECULAR_COLOR_EXT                     = $81FA;

  // GL_EXT_shared_texture_palette
  GL_SHARED_TEXTURE_PALETTE_EXT                      = $81FB;

  // GL_EXT_stencil_two_side
  GL_STENCIL_TEST_TWO_SIDE_EXT                       = $8910;
  GL_ACTIVE_STENCIL_FACE_EXT                         = $8911;

  // GL_EXT_stencil_wrap
  GL_INCR_WRAP_EXT                                   = $8507;
  GL_DECR_WRAP_EXT                                   = $8508;

  // GL_EXT_texture
  GL_ALPHA4_EXT                                      = $803B;
  GL_ALPHA8_EXT                                      = $803C;
  GL_ALPHA12_EXT                                     = $803D;
  GL_ALPHA16_EXT                                     = $803E;
  GL_LUMINANCE4_EXT                                  = $803F;
  GL_LUMINANCE8_EXT                                  = $8040;
  GL_LUMINANCE12_EXT                                 = $8041;
  GL_LUMINANCE16_EXT                                 = $8042;
  GL_LUMINANCE4_ALPHA4_EXT                           = $8043;
  GL_LUMINANCE6_ALPHA2_EXT                           = $8044;
  GL_LUMINANCE8_ALPHA8_EXT                           = $8045;
  GL_LUMINANCE12_ALPHA4_EXT                          = $8046;
  GL_LUMINANCE12_ALPHA12_EXT                         = $8047;
  GL_LUMINANCE16_ALPHA16_EXT                         = $8048;
  GL_INTENSITY_EXT                                   = $8049;
  GL_INTENSITY4_EXT                                  = $804A;
  GL_INTENSITY8_EXT                                  = $804B;
  GL_INTENSITY12_EXT                                 = $804C;
  GL_INTENSITY16_EXT                                 = $804D;
  GL_RGB2_EXT                                        = $804E;
  GL_RGB4_EXT                                        = $804F;
  GL_RGB5_EXT                                        = $8050;
  GL_RGB8_EXT                                        = $8051;
  GL_RGB10_EXT                                       = $8052;
  GL_RGB12_EXT                                       = $8053;
  GL_RGB16_EXT                                       = $8054;
  GL_RGBA2_EXT                                       = $8055;
  GL_RGBA4_EXT                                       = $8056;
  GL_RGB5_A1_EXT                                     = $8057;
  GL_RGBA8_EXT                                       = $8058;
  GL_RGB10_A2_EXT                                    = $8059;
  GL_RGBA12_EXT                                      = $805A;
  GL_RGBA16_EXT                                      = $805B;
  GL_TEXTURE_RED_SIZE_EXT                            = $805C;
  GL_TEXTURE_GREEN_SIZE_EXT                          = $805D;
  GL_TEXTURE_BLUE_SIZE_EXT                           = $805E;
  GL_TEXTURE_ALPHA_SIZE_EXT                          = $805F;
  GL_TEXTURE_LUMINANCE_SIZE_EXT                      = $8060;
  GL_TEXTURE_INTENSITY_SIZE_EXT                      = $8061;
  GL_REPLACE_EXT                                     = $8062;
  GL_PROXY_TEXTURE_1D_EXT                            = $8063;
  GL_PROXY_TEXTURE_2D_EXT                            = $8064;
  GL_TEXTURE_TOO_LARGE_EXT                           = $8065;

  // GL_EXT_texture3D
  GL_PACK_SKIP_IMAGES_EXT                            = $806B;
  GL_PACK_IMAGE_HEIGHT_EXT                           = $806C;
  GL_UNPACK_SKIP_IMAGES_EXT                          = $806D;
  GL_UNPACK_IMAGE_HEIGHT_EXT                         = $806E;
  GL_TEXTURE_3D_EXT                                  = $806F;
  GL_PROXY_TEXTURE_3D_EXT                            = $8070;
  GL_TEXTURE_DEPTH_EXT                               = $8071;
  GL_TEXTURE_WRAP_R_EXT                              = $8072;
  GL_MAX_3D_TEXTURE_SIZE_EXT                         = $8073;

  // GL_EXT_texture_compression_s3tc
  GL_COMPRESSED_RGB_S3TC_DXT1_EXT                    = $83F0;
  GL_COMPRESSED_RGBA_S3TC_DXT1_EXT                   = $83F1;
  GL_COMPRESSED_RGBA_S3TC_DXT3_EXT                   = $83F2;
  GL_COMPRESSED_RGBA_S3TC_DXT5_EXT                   = $83F3;

  // GL_EXT_texture_cube_map
  GL_NORMAL_MAP_EXT                                  = $8511;
  GL_REFLECTION_MAP_EXT                              = $8512;
  GL_TEXTURE_CUBE_MAP_EXT                            = $8513;
  GL_TEXTURE_BINDING_CUBE_MAP_EXT                    = $8514;
  GL_TEXTURE_CUBE_MAP_POSITIVE_X_EXT                 = $8515;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_X_EXT                 = $8516;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Y_EXT                 = $8517;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_EXT                 = $8518;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Z_EXT                 = $8519;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_EXT                 = $851A;
  GL_PROXY_TEXTURE_CUBE_MAP_EXT                      = $851B;
  GL_MAX_CUBE_MAP_TEXTURE_SIZE_EXT                   = $851C;

  // GL_EXT_texture_env_combine
  GL_COMBINE_EXT                                     = $8570;
  GL_COMBINE_RGB_EXT                                 = $8571;
  GL_COMBINE_ALPHA_EXT                               = $8572;
  GL_RGB_SCALE_EXT                                   = $8573;
  GL_ADD_SIGNED_EXT                                  = $8574;
  GL_INTERPOLATE_EXT                                 = $8575;
  GL_CONSTANT_EXT                                    = $8576;
  GL_PRIMARY_COLOR_EXT                               = $8577;
  GL_PREVIOUS_EXT                                    = $8578;
  GL_SOURCE0_RGB_EXT                                 = $8580;
  GL_SOURCE1_RGB_EXT                                 = $8581;
  GL_SOURCE2_RGB_EXT                                 = $8582;
  GL_SOURCE0_ALPHA_EXT                               = $8588;
  GL_SOURCE1_ALPHA_EXT                               = $8589;
  GL_SOURCE2_ALPHA_EXT                               = $858A;
  GL_OPERAND0_RGB_EXT                                = $8590;
  GL_OPERAND1_RGB_EXT                                = $8591;
  GL_OPERAND2_RGB_EXT                                = $8592;
  GL_OPERAND0_ALPHA_EXT                              = $8598;
  GL_OPERAND1_ALPHA_EXT                              = $8599;
  GL_OPERAND2_ALPHA_EXT                              = $859A;

  // GL_EXT_texture_env_dot3
  GL_DOT3_RGB_EXT                                    = $8740;
  GL_DOT3_RGBA_EXT                                   = $8741;

  // GL_EXT_texture_filter_anisotropic
  GL_TEXTURE_MAX_ANISOTROPY_EXT                      = $84FE;
  GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT                  = $84FF;

  // GL_EXT_texture_lod_bias
  GL_MAX_TEXTURE_LOD_BIAS_EXT                        = $84FD;
  GL_TEXTURE_FILTER_CONTROL_EXT                      = $8500;
  GL_TEXTURE_LOD_BIAS_EXT                            = $8501;

  // GL_EXT_texture_object
  GL_TEXTURE_PRIORITY_EXT                            = $8066;
  GL_TEXTURE_RESIDENT_EXT                            = $8067;
  GL_TEXTURE_1D_BINDING_EXT                          = $8068;
  GL_TEXTURE_2D_BINDING_EXT                          = $8069;
  GL_TEXTURE_3D_BINDING_EXT                          = $806A;

  // GL_EXT_texture_perturb_normal
  GL_PERTURB_EXT                                     = $85AE;
  GL_TEXTURE_NORMAL_EXT                              = $85AF;

  // GL_EXT_vertex_array
  GL_VERTEX_ARRAY_EXT                                = $8074;
  GL_NORMAL_ARRAY_EXT                                = $8075;
  GL_COLOR_ARRAY_EXT                                 = $8076;
  GL_INDEX_ARRAY_EXT                                 = $8077;
  GL_TEXTURE_COORD_ARRAY_EXT                         = $8078;
  GL_EDGE_FLAG_ARRAY_EXT                             = $8079;
  GL_VERTEX_ARRAY_SIZE_EXT                           = $807A;
  GL_VERTEX_ARRAY_TYPE_EXT                           = $807B;
  GL_VERTEX_ARRAY_STRIDE_EXT                         = $807C;
  GL_VERTEX_ARRAY_COUNT_EXT                          = $807D;
  GL_NORMAL_ARRAY_TYPE_EXT                           = $807E;
  GL_NORMAL_ARRAY_STRIDE_EXT                         = $807F;
  GL_NORMAL_ARRAY_COUNT_EXT                          = $8080;
  GL_COLOR_ARRAY_SIZE_EXT                            = $8081;
  GL_COLOR_ARRAY_TYPE_EXT                            = $8082;
  GL_COLOR_ARRAY_STRIDE_EXT                          = $8083;
  GL_COLOR_ARRAY_COUNT_EXT                           = $8084;
  GL_INDEX_ARRAY_TYPE_EXT                            = $8085;
  GL_INDEX_ARRAY_STRIDE_EXT                          = $8086;
  GL_INDEX_ARRAY_COUNT_EXT                           = $8087;
  GL_TEXTURE_COORD_ARRAY_SIZE_EXT                    = $8088;
  GL_TEXTURE_COORD_ARRAY_TYPE_EXT                    = $8089;
  GL_TEXTURE_COORD_ARRAY_STRIDE_EXT                  = $808A;
  GL_TEXTURE_COORD_ARRAY_COUNT_EXT                   = $808B;
  GL_EDGE_FLAG_ARRAY_STRIDE_EXT                      = $808C;
  GL_EDGE_FLAG_ARRAY_COUNT_EXT                       = $808D;
  GL_VERTEX_ARRAY_POINTER_EXT                        = $808E;
  GL_NORMAL_ARRAY_POINTER_EXT                        = $808F;
  GL_COLOR_ARRAY_POINTER_EXT                         = $8090;
  GL_INDEX_ARRAY_POINTER_EXT                         = $8091;
  GL_TEXTURE_COORD_ARRAY_POINTER_EXT                 = $8092;
  GL_EDGE_FLAG_ARRAY_POINTER_EXT                     = $8093;

  // GL_EXT_vertex_shader
  GL_VERTEX_SHADER_EXT                               = $8780;
  GL_VERTEX_SHADER_BINDING_EXT                       = $8781;
  GL_OP_INDEX_EXT                                    = $8782;
  GL_OP_NEGATE_EXT                                   = $8783;
  GL_OP_DOT3_EXT                                     = $8784;
  GL_OP_DOT4_EXT                                     = $8785;
  GL_OP_MUL_EXT                                      = $8786;
  GL_OP_ADD_EXT                                      = $8787;
  GL_OP_MADD_EXT                                     = $8788;
  GL_OP_FRAC_EXT                                     = $8789;
  GL_OP_MAX_EXT                                      = $878A;
  GL_OP_MIN_EXT                                      = $878B;
  GL_OP_SET_GE_EXT                                   = $878C;
  GL_OP_SET_LT_EXT                                   = $878D;
  GL_OP_CLAMP_EXT                                    = $878E;
  GL_OP_FLOOR_EXT                                    = $878F;
  GL_OP_ROUND_EXT                                    = $8790;
  GL_OP_EXP_BASE_2_EXT                               = $8791;
  GL_OP_LOG_BASE_2_EXT                               = $8792;
  GL_OP_POWER_EXT                                    = $8793;
  GL_OP_RECIP_EXT                                    = $8794;
  GL_OP_RECIP_SQRT_EXT                               = $8795;
  GL_OP_SUB_EXT                                      = $8796;
  GL_OP_CROSS_PRODUCT_EXT                            = $8797;
  GL_OP_MULTIPLY_MATRIX_EXT                          = $8798;
  GL_OP_MOV_EXT                                      = $8799;
  GL_OUTPUT_VERTEX_EXT                               = $879A;
  GL_OUTPUT_COLOR0_EXT                               = $879B;
  GL_OUTPUT_COLOR1_EXT                               = $879C;
  GL_OUTPUT_TEXTURE_COORD0_EXT                       = $879D;
  GL_OUTPUT_TEXTURE_COORD1_EXT                       = $879E;
  GL_OUTPUT_TEXTURE_COORD2_EXT                       = $879F;
  GL_OUTPUT_TEXTURE_COORD3_EXT                       = $87A0;
  GL_OUTPUT_TEXTURE_COORD4_EXT                       = $87A1;
  GL_OUTPUT_TEXTURE_COORD5_EXT                       = $87A2;
  GL_OUTPUT_TEXTURE_COORD6_EXT                       = $87A3;
  GL_OUTPUT_TEXTURE_COORD7_EXT                       = $87A4;
  GL_OUTPUT_TEXTURE_COORD8_EXT                       = $87A5;
  GL_OUTPUT_TEXTURE_COORD9_EXT                       = $87A6;
  GL_OUTPUT_TEXTURE_COORD10_EXT                      = $87A7;
  GL_OUTPUT_TEXTURE_COORD11_EXT                      = $87A8;
  GL_OUTPUT_TEXTURE_COORD12_EXT                      = $87A9;
  GL_OUTPUT_TEXTURE_COORD13_EXT                      = $87AA;
  GL_OUTPUT_TEXTURE_COORD14_EXT                      = $87AB;
  GL_OUTPUT_TEXTURE_COORD15_EXT                      = $87AC;
  GL_OUTPUT_TEXTURE_COORD16_EXT                      = $87AD;
  GL_OUTPUT_TEXTURE_COORD17_EXT                      = $87AE;
  GL_OUTPUT_TEXTURE_COORD18_EXT                      = $87AF;
  GL_OUTPUT_TEXTURE_COORD19_EXT                      = $87B0;
  GL_OUTPUT_TEXTURE_COORD20_EXT                      = $87B1;
  GL_OUTPUT_TEXTURE_COORD21_EXT                      = $87B2;
  GL_OUTPUT_TEXTURE_COORD22_EXT                      = $87B3;
  GL_OUTPUT_TEXTURE_COORD23_EXT                      = $87B4;
  GL_OUTPUT_TEXTURE_COORD24_EXT                      = $87B5;
  GL_OUTPUT_TEXTURE_COORD25_EXT                      = $87B6;
  GL_OUTPUT_TEXTURE_COORD26_EXT                      = $87B7;
  GL_OUTPUT_TEXTURE_COORD27_EXT                      = $87B8;
  GL_OUTPUT_TEXTURE_COORD28_EXT                      = $87B9;
  GL_OUTPUT_TEXTURE_COORD29_EXT                      = $87BA;
  GL_OUTPUT_TEXTURE_COORD30_EXT                      = $87BB;
  GL_OUTPUT_TEXTURE_COORD31_EXT                      = $87BC;
  GL_OUTPUT_FOG_EXT                                  = $87BD;
  GL_SCALAR_EXT                                      = $87BE;
  GL_VECTOR_EXT                                      = $87BF;
  GL_MATRIX_EXT                                      = $87C0;
  GL_VARIANT_EXT                                     = $87C1;
  GL_INVARIANT_EXT                                   = $87C2;
  GL_LOCAL_CONSTANT_EXT                              = $87C3;
  GL_LOCAL_EXT                                       = $87C4;
  GL_MAX_VERTEX_SHADER_INSTRUCTIONS_EXT              = $87C5;
  GL_MAX_VERTEX_SHADER_VARIANTS_EXT                  = $87C6;
  GL_MAX_VERTEX_SHADER_INVARIANTS_EXT                = $87C7;
  GL_MAX_VERTEX_SHADER_LOCAL_CONSTANTS_EXT           = $87C8;
  GL_MAX_VERTEX_SHADER_LOCALS_EXT                    = $87C9;
  GL_MAX_OPTIMIZED_VERTEX_SHADER_INSTRUCTIONS_EXT    = $87CA;
  GL_MAX_OPTIMIZED_VERTEX_SHADER_VARIANTS_EXT        = $87CB;
  GL_MAX_OPTIMIZED_VERTEX_SHADER_LOCAL_CONSTANTS_EXT = $87CC;
  GL_MAX_OPTIMIZED_VERTEX_SHADER_INVARIANTS_EXT      = $87CD;
  GL_MAX_OPTIMIZED_VERTEX_SHADER_LOCALS_EXT          = $87CE;
  GL_VERTEX_SHADER_INSTRUCTIONS_EXT                  = $87CF;
  GL_VERTEX_SHADER_VARIANTS_EXT                      = $87D0;
  GL_VERTEX_SHADER_INVARIANTS_EXT                    = $87D1;
  GL_VERTEX_SHADER_LOCAL_CONSTANTS_EXT               = $87D2;
  GL_VERTEX_SHADER_LOCALS_EXT                        = $87D3;
  GL_VERTEX_SHADER_OPTIMIZED_EXT                     = $87D4;
  GL_X_EXT                                           = $87D5;
  GL_Y_EXT                                           = $87D6;
  GL_Z_EXT                                           = $87D7;
  GL_W_EXT                                           = $87D8;
  GL_NEGATIVE_X_EXT                                  = $87D9;
  GL_NEGATIVE_Y_EXT                                  = $87DA;
  GL_NEGATIVE_Z_EXT                                  = $87DB;
  GL_NEGATIVE_W_EXT                                  = $87DC;
  GL_ZERO_EXT                                        = $87DD;
  GL_ONE_EXT                                         = $87DE;
  GL_NEGATIVE_ONE_EXT                                = $87DF;
  GL_NORMALIZED_RANGE_EXT                            = $87E0;
  GL_FULL_RANGE_EXT                                  = $87E1;
  GL_CURRENT_VERTEX_EXT                              = $87E2;
  GL_MVP_MATRIX_EXT                                  = $87E3;
  GL_VARIANT_VALUE_EXT                               = $87E4;
  GL_VARIANT_DATATYPE_EXT                            = $87E5;
  GL_VARIANT_ARRAY_STRIDE_EXT                        = $87E6;
  GL_VARIANT_ARRAY_TYPE_EXT                          = $87E7;
  GL_VARIANT_ARRAY_EXT                               = $87E8;
  GL_VARIANT_ARRAY_POINTER_EXT                       = $87E9;
  GL_INVARIANT_VALUE_EXT                             = $87EA;
  GL_INVARIANT_DATATYPE_EXT                          = $87EB;
  GL_LOCAL_CONSTANT_VALUE_EXT                        = $87EC;
  GL_LOCAL_CONSTANT_DATATYPE_EXT                     = $87ED;

  // GL_EXT_vertex_weighting
  GL_MODELVIEW0_STACK_DEPTH_EXT                      = GL_MODELVIEW_STACK_DEPTH;
  GL_MODELVIEW1_STACK_DEPTH_EXT                      = $8502;
  GL_MODELVIEW0_MATRIX_EXT                           = GL_MODELVIEW_MATRIX;
  GL_MODELVIEW1_MATRIX_EXT                           = $8506;
  GL_VERTEX_WEIGHTING_EXT                            = $8509;
  GL_MODELVIEW0_EXT                                  = GL_MODELVIEW;
  GL_MODELVIEW1_EXT                                  = $850A;
  GL_CURRENT_VERTEX_WEIGHT_EXT                       = $850B;
  GL_VERTEX_WEIGHT_ARRAY_EXT                         = $850C;
  GL_VERTEX_WEIGHT_ARRAY_SIZE_EXT                    = $850D;
  GL_VERTEX_WEIGHT_ARRAY_TYPE_EXT                    = $850E;
  GL_VERTEX_WEIGHT_ARRAY_STRIDE_EXT                  = $850F;
  GL_VERTEX_WEIGHT_ARRAY_POINTER_EXT                 = $8510;

  // GL_FfdMaskSGIX
  GL_TEXTURE_DEFORMATION_BIT_SGIX                    = $00000001;
  GL_GEOMETRY_DEFORMATION_BIT_SGIX                   = $00000002;

  // GL_HP_convolution_border_modes
  GL_IGNORE_BORDER_HP                                = $8150;
  GL_CONSTANT_BORDER_HP                              = $8151;
  GL_REPLICATE_BORDER_HP                             = $8153;
  GL_CONVOLUTION_BORDER_COLOR_HP                     = $8154;

  // GL_HP_image_transform
  GL_IMAGE_SCALE_X_HP                                = $8155;
  GL_IMAGE_SCALE_Y_HP                                = $8156;
  GL_IMAGE_TRANSLATE_X_HP                            = $8157;
  GL_IMAGE_TRANSLATE_Y_HP                            = $8158;
  GL_IMAGE_ROTATE_ANGLE_HP                           = $8159;
  GL_IMAGE_ROTATE_ORIGIN_X_HP                        = $815A;
  GL_IMAGE_ROTATE_ORIGIN_Y_HP                        = $815B;
  GL_IMAGE_MAG_FILTER_HP                             = $815C;
  GL_IMAGE_MIN_FILTER_HP                             = $815D;
  GL_IMAGE_CUBIC_WEIGHT_HP                           = $815E;
  GL_CUBIC_HP                                        = $815F;
  GL_AVERAGE_HP                                      = $8160;
  GL_IMAGE_TRANSFORM_2D_HP                           = $8161;
  GL_POST_IMAGE_TRANSFORM_COLOR_TABLE_HP             = $8162;
  GL_PROXY_POST_IMAGE_TRANSFORM_COLOR_TABLE_HP       = $8163;

  // GL_HP_occlusion_test
  GL_OCCLUSION_TEST_HP                               = $8165;
  GL_OCCLUSION_TEST_RESULT_HP                        = $8166;

  // GL_HP_texture_lighting
  GL_TEXTURE_LIGHTING_MODE_HP                        = $8167;
  GL_TEXTURE_POST_SPECULAR_HP                        = $8168;
  GL_TEXTURE_PRE_SPECULAR_HP                         = $8169;

  // GL_IBM_cull_vertex
  GL_CULL_VERTEX_IBM                                 = 103050;

  // GL_IBM_rasterpos_clip
  GL_RASTER_POSITION_UNCLIPPED_IBM                   = $19262;

  // GL_IBM_texture_mirrored_repeat
  GL_MIRRORED_REPEAT_IBM                             = $8370;

  // GL_IBM_vertex_array_lists
  GL_VERTEX_ARRAY_LIST_IBM                           = 103070;
  GL_NORMAL_ARRAY_LIST_IBM                           = 103071;
  GL_COLOR_ARRAY_LIST_IBM                            = 103072;
  GL_INDEX_ARRAY_LIST_IBM                            = 103073;
  GL_TEXTURE_COORD_ARRAY_LIST_IBM                    = 103074;
  GL_EDGE_FLAG_ARRAY_LIST_IBM                        = 103075;
  GL_FOG_COORDINATE_ARRAY_LIST_IBM                   = 103076;
  GL_SECONDARY_COLOR_ARRAY_LIST_IBM                  = 103077;
  GL_VERTEX_ARRAY_LIST_STRIDE_IBM                    = 103080;
  GL_NORMAL_ARRAY_LIST_STRIDE_IBM                    = 103081;
  GL_COLOR_ARRAY_LIST_STRIDE_IBM                     = 103082;
  GL_INDEX_ARRAY_LIST_STRIDE_IBM                     = 103083;
  GL_TEXTURE_COORD_ARRAY_LIST_STRIDE_IBM             = 103084;
  GL_EDGE_FLAG_ARRAY_LIST_STRIDE_IBM                 = 103085;
  GL_FOG_COORDINATE_ARRAY_LIST_STRIDE_IBM            = 103086;
  GL_SECONDARY_COLOR_ARRAY_LIST_STRIDE_IBM           = 103087;

  // GL_INGR_color_clamp
  GL_RED_MIN_CLAMP_INGR                              = $8560;
  GL_GREEN_MIN_CLAMP_INGR                            = $8561;
  GL_BLUE_MIN_CLAMP_INGR                             = $8562;
  GL_ALPHA_MIN_CLAMP_INGR                            = $8563;
  GL_RED_MAX_CLAMP_INGR                              = $8564;
  GL_GREEN_MAX_CLAMP_INGR                            = $8565;
  GL_BLUE_MAX_CLAMP_INGR                             = $8566;
  GL_ALPHA_MAX_CLAMP_INGR                            = $8567;

  // GL_INGR_interlace_read
  GL_INTERLACE_READ_INGR                             = $8568;

  // GL_INTEL_parallel_arrays
  GL_PARALLEL_ARRAYS_INTEL                           = $83F4;
  GL_VERTEX_ARRAY_PARALLEL_POINTERS_INTEL            = $83F5;
  GL_NORMAL_ARRAY_PARALLEL_POINTERS_INTEL            = $83F6;
  GL_COLOR_ARRAY_PARALLEL_POINTERS_INTEL             = $83F7;
  GL_TEXTURE_COORD_ARRAY_PARALLEL_POINTERS_INTEL     = $83F8;

  // GL_NV_copy_depth_to_color
  GL_DEPTH_STENCIL_TO_RGBA_NV                        = $886E;
  GL_DEPTH_STENCIL_TO_BGRA_NV                        = $886F;

  // GL_NV_depth_clamp
  GL_DEPTH_CLAMP_NV                                  = $864F;

  // GL_NV_evaluators
  GL_EVAL_2D_NV                                      = $86C0;
  GL_EVAL_TRIANGULAR_2D_NV                           = $86C1;
  GL_MAP_TESSELLATION_NV                             = $86C2;
  GL_MAP_ATTRIB_U_ORDER_NV                           = $86C3;
  GL_MAP_ATTRIB_V_ORDER_NV                           = $86C4;
  GL_EVAL_FRACTIONAL_TESSELLATION_NV                 = $86C5;
  GL_EVAL_VERTEX_ATTRIB0_NV                          = $86C6;
  GL_EVAL_VERTEX_ATTRIB1_NV                          = $86C7;
  GL_EVAL_VERTEX_ATTRIB2_NV                          = $86C8;
  GL_EVAL_VERTEX_ATTRIB3_NV                          = $86C9;
  GL_EVAL_VERTEX_ATTRIB4_NV                          = $86CA;
  GL_EVAL_VERTEX_ATTRIB5_NV                          = $86CB;
  GL_EVAL_VERTEX_ATTRIB6_NV                          = $86CC;
  GL_EVAL_VERTEX_ATTRIB7_NV                          = $86CD;
  GL_EVAL_VERTEX_ATTRIB8_NV                          = $86CE;
  GL_EVAL_VERTEX_ATTRIB9_NV                          = $86CF;
  GL_EVAL_VERTEX_ATTRIB10_NV                         = $86D0;
  GL_EVAL_VERTEX_ATTRIB11_NV                         = $86D1;
  GL_EVAL_VERTEX_ATTRIB12_NV                         = $86D2;
  GL_EVAL_VERTEX_ATTRIB13_NV                         = $86D3;
  GL_EVAL_VERTEX_ATTRIB14_NV                         = $86D4;
  GL_EVAL_VERTEX_ATTRIB15_NV                         = $86D5;
  GL_MAX_MAP_TESSELLATION_NV                         = $86D6;
  GL_MAX_RATIONAL_EVAL_ORDER_NV                      = $86D7;

  // GL_NV_fence
  GL_ALL_COMPLETED_NV                                = $84F2;
  GL_FENCE_STATUS_NV                                 = $84F3;
  GL_FENCE_CONDITION_NV                              = $84F4;

  // GL_NV_float_buffer
  GL_FLOAT_R_NV                                      = $8880;
  GL_FLOAT_RG_NV                                     = $8881;
  GL_FLOAT_RGB_NV                                    = $8882;
  GL_FLOAT_RGBA_NV                                   = $8883;
  GL_FLOAT_R16_NV                                    = $8884;
  GL_FLOAT_R32_NV                                    = $8885;
  GL_FLOAT_RG16_NV                                   = $8886;
  GL_FLOAT_RG32_NV                                   = $8887;
  GL_FLOAT_RGB16_NV                                  = $8888;
  GL_FLOAT_RGB32_NV                                  = $8889;
  GL_FLOAT_RGBA16_NV                                 = $888A;
  GL_FLOAT_RGBA32_NV                                 = $888B;
  GL_TEXTURE_FLOAT_COMPONENTS_NV                     = $888C;
  GL_FLOAT_CLEAR_COLOR_VALUE_NV                      = $888D;
  GL_FLOAT_RGBA_MODE_NV                              = $888E;

  // GL_NV_fog_distance
  GL_FOG_DISTANCE_MODE_NV                            = $855A;
  GL_EYE_RADIAL_NV                                   = $855B;
  GL_EYE_PLANE_ABSOLUTE_NV                           = $855C;

  // GL_NV_fragment_program
  GL_MAX_FRAGMENT_PROGRAM_LOCAL_PARAMETERS_NV        = $8868;
  GL_FRAGMENT_PROGRAM_NV                             = $8870;
  GL_MAX_TEXTURE_COORDS_NV                           = $8871;
  GL_MAX_TEXTURE_IMAGE_UNITS_NV                      = $8872;
  GL_FRAGMENT_PROGRAM_BINDING_NV                     = $8873;
  GL_PROGRAM_ERROR_STRING_NV                         = $8874;

  // GL_NV_half_float
  GL_HALF_FLOAT_NV                                   = $140B;

  // GL_NV_light_max_exponent
  GL_MAX_SHININESS_NV                                = $8504;
  GL_MAX_SPOT_EXPONENT_NV                            = $8505;

  // GL_NV_multisample_filter_hint
  GL_MULTISAMPLE_FILTER_HINT_NV                      = $8534;

  // GL_NV_occlusion_query
  GL_PIXEL_COUNTER_BITS_NV                           = $8864;
  GL_CURRENT_OCCLUSION_QUERY_ID_NV                   = $8865;
  GL_PIXEL_COUNT_NV                                  = $8866;
  GL_PIXEL_COUNT_AVAILABLE_NV                        = $8867;

  // GL_NV_packed_depth_stencil
  GL_DEPTH_STENCIL_NV                                = $84F9;
  GL_UNSIGNED_INT_24_8_NV                            = $84FA;

  // GL_NV_pixel_data_range
  GL_WRITE_PIXEL_DATA_RANGE_NV                       = $8878;
  GL_READ_PIXEL_DATA_RANGE_NV                        = $8879;
  GL_WRITE_PIXEL_DATA_RANGE_LENGTH_NV                = $887A;
  GL_READ_PIXEL_DATA_RANGE_LENGTH_NV                 = $887B;
  GL_WRITE_PIXEL_DATA_RANGE_POINTER_NV               = $887C;
  GL_READ_PIXEL_DATA_RANGE_POINTER_NV                = $887D;

  // GL_NV_point_sprite
  GL_POINT_SPRITE_NV                                 = $8861;
  GL_COORD_REPLACE_NV                                = $8862;
  GL_POINT_SPRITE_R_MODE_NV                          = $8863;

  // GL_NV_primitive_restart
  GL_PRIMITIVE_RESTART_NV                            = $8558;
  GL_PRIMITIVE_RESTART_INDEX_NV                      = $8559;

  // GL_NV_register_combiners
  GL_REGISTER_COMBINERS_NV                           = $8522;
  GL_VARIABLE_A_NV                                   = $8523;
  GL_VARIABLE_B_NV                                   = $8524;
  GL_VARIABLE_C_NV                                   = $8525;
  GL_VARIABLE_D_NV                                   = $8526;
  GL_VARIABLE_E_NV                                   = $8527;
  GL_VARIABLE_F_NV                                   = $8528;
  GL_VARIABLE_G_NV                                   = $8529;
  GL_CONSTANT_COLOR0_NV                              = $852A;
  GL_CONSTANT_COLOR1_NV                              = $852B;
  GL_PRIMARY_COLOR_NV                                = $852C;
  GL_SECONDARY_COLOR_NV                              = $852D;
  GL_SPARE0_NV                                       = $852E;
  GL_SPARE1_NV                                       = $852F;
  GL_DISCARD_NV                                      = $8530;
  GL_E_TIMES_F_NV                                    = $8531;
  GL_SPARE0_PLUS_SECONDARY_COLOR_NV                  = $8532;
  GL_UNSIGNED_IDENTITY_NV                            = $8536;
  GL_UNSIGNED_INVERT_NV                              = $8537;
  GL_EXPAND_NORMAL_NV                                = $8538;
  GL_EXPAND_NEGATE_NV                                = $8539;
  GL_HALF_BIAS_NORMAL_NV                             = $853A;
  GL_HALF_BIAS_NEGATE_NV                             = $853B;
  GL_SIGNED_IDENTITY_NV                              = $853C;
  GL_SIGNED_NEGATE_NV                                = $853D;
  GL_SCALE_BY_TWO_NV                                 = $853E;
  GL_SCALE_BY_FOUR_NV                                = $853F;
  GL_SCALE_BY_ONE_HALF_NV                            = $8540;
  GL_BIAS_BY_NEGATIVE_ONE_HALF_NV                    = $8541;
  GL_COMBINER_INPUT_NV                               = $8542;
  GL_COMBINER_MAPPING_NV                             = $8543;
  GL_COMBINER_COMPONENT_USAGE_NV                     = $8544;
  GL_COMBINER_AB_DOT_PRODUCT_NV                      = $8545;
  GL_COMBINER_CD_DOT_PRODUCT_NV                      = $8546;
  GL_COMBINER_MUX_SUM_NV                             = $8547;
  GL_COMBINER_SCALE_NV                               = $8548;
  GL_COMBINER_BIAS_NV                                = $8549;
  GL_COMBINER_AB_OUTPUT_NV                           = $854A;
  GL_COMBINER_CD_OUTPUT_NV                           = $854B;
  GL_COMBINER_SUM_OUTPUT_NV                          = $854C;
  GL_MAX_GENERAL_COMBINERS_NV                        = $854D;
  GL_NUM_GENERAL_COMBINERS_NV                        = $854E;
  GL_COLOR_SUM_CLAMP_NV                              = $854F;
  GL_COMBINER0_NV                                    = $8550;
  GL_COMBINER1_NV                                    = $8551;
  GL_COMBINER2_NV                                    = $8552;
  GL_COMBINER3_NV                                    = $8553;
  GL_COMBINER4_NV                                    = $8554;
  GL_COMBINER5_NV                                    = $8555;
  GL_COMBINER6_NV                                    = $8556;
  GL_COMBINER7_NV                                    = $8557;

  // GL_NV_register_combiners2
  GL_PER_STAGE_CONSTANTS_NV                          = $8535;

  // GL_NV_texgen_emboss
  GL_EMBOSS_LIGHT_NV                                 = $855D;
  GL_EMBOSS_CONSTANT_NV                              = $855E;
  GL_EMBOSS_MAP_NV                                   = $855F;

  // GL_NV_texgen_reflection
  GL_NORMAL_MAP_NV                                   = $8511;
  GL_REFLECTION_MAP_NV                               = $8512;

  // GL_NV_texture_env_combine4
  GL_COMBINE4_NV                                     = $8503;
  GL_SOURCE3_RGB_NV                                  = $8583;
  GL_SOURCE3_ALPHA_NV                                = $858B;
  GL_OPERAND3_RGB_NV                                 = $8593;
  GL_OPERAND3_ALPHA_NV                               = $859B;

  // GL_NV_texture_expand_normal
  GL_TEXTURE_UNSIGNED_REMAP_MODE_NV                  = $888F;

  // GL_NV_texture_rectangle
  GL_TEXTURE_RECTANGLE_NV                            = $84F5;
  GL_TEXTURE_BINDING_RECTANGLE_NV                    = $84F6;
  GL_PROXY_TEXTURE_RECTANGLE_NV                      = $84F7;
  GL_MAX_RECTANGLE_TEXTURE_SIZE_NV                   = $84F8;

  // GL_NV_texture_shader
  GL_OFFSET_TEXTURE_RECTANGLE_NV                     = $864C;
  GL_OFFSET_TEXTURE_RECTANGLE_SCALE_NV               = $864D;
  GL_DOT_PRODUCT_TEXTURE_RECTANGLE_NV                = $864E;
  GL_RGBA_UNSIGNED_DOT_PRODUCT_MAPPING_NV            = $86D9;
  GL_UNSIGNED_INT_S8_S8_8_8_NV                       = $86DA;
  GL_UNSIGNED_INT_8_8_S8_S8_REV_NV                   = $86DB;
  GL_DSDT_MAG_INTENSITY_NV                           = $86DC;
  GL_SHADER_CONSISTENT_NV                            = $86DD;
  GL_TEXTURE_SHADER_NV                               = $86DE;
  GL_SHADER_OPERATION_NV                             = $86DF;
  GL_CULL_MODES_NV                                   = $86E0;
  GL_OFFSET_TEXTURE_MATRIX_NV                        = $86E1;
  GL_OFFSET_TEXTURE_SCALE_NV                         = $86E2;
  GL_OFFSET_TEXTURE_BIAS_NV                          = $86E3;
  GL_OFFSET_TEXTURE_2D_MATRIX_NV                     = GL_OFFSET_TEXTURE_MATRIX_NV;
  GL_OFFSET_TEXTURE_2D_SCALE_NV                      = GL_OFFSET_TEXTURE_SCALE_NV;
  GL_OFFSET_TEXTURE_2D_BIAS_NV                       = GL_OFFSET_TEXTURE_BIAS_NV;
  GL_PREVIOUS_TEXTURE_INPUT_NV                       = $86E4;
  GL_CONST_EYE_NV                                    = $86E5;
  GL_PASS_THROUGH_NV                                 = $86E6;
  GL_CULL_FRAGMENT_NV                                = $86E7;
  GL_OFFSET_TEXTURE_2D_NV                            = $86E8;
  GL_DEPENDENT_AR_TEXTURE_2D_NV                      = $86E9;
  GL_DEPENDENT_GB_TEXTURE_2D_NV                      = $86EA;
  GL_DOT_PRODUCT_NV                                  = $86EC;
  GL_DOT_PRODUCT_DEPTH_REPLACE_NV                    = $86ED;
  GL_DOT_PRODUCT_TEXTURE_2D_NV                       = $86EE;
  GL_DOT_PRODUCT_TEXTURE_CUBE_MAP_NV                 = $86F0;
  GL_DOT_PRODUCT_DIFFUSE_CUBE_MAP_NV                 = $86F1;
  GL_DOT_PRODUCT_REFLECT_CUBE_MAP_NV                 = $86F2;
  GL_DOT_PRODUCT_CONST_EYE_REFLECT_CUBE_MAP_NV       = $86F3;
  GL_HILO_NV                                         = $86F4;
  GL_DSDT_NV                                         = $86F5;
  GL_DSDT_MAG_NV                                     = $86F6;
  GL_DSDT_MAG_VIB_NV                                 = $86F7;
  GL_HILO16_NV                                       = $86F8;
  GL_SIGNED_HILO_NV                                  = $86F9;
  GL_SIGNED_HILO16_NV                                = $86FA;
  GL_SIGNED_RGBA_NV                                  = $86FB;
  GL_SIGNED_RGBA8_NV                                 = $86FC;
  GL_SIGNED_RGB_NV                                   = $86FE;
  GL_SIGNED_RGB8_NV                                  = $86FF;
  GL_SIGNED_LUMINANCE_NV                             = $8701;
  GL_SIGNED_LUMINANCE8_NV                            = $8702;
  GL_SIGNED_LUMINANCE_ALPHA_NV                       = $8703;
  GL_SIGNED_LUMINANCE8_ALPHA8_NV                     = $8704;
  GL_SIGNED_ALPHA_NV                                 = $8705;
  GL_SIGNED_ALPHA8_NV                                = $8706;
  GL_SIGNED_INTENSITY_NV                             = $8707;
  GL_SIGNED_INTENSITY8_NV                            = $8708;
  GL_DSDT8_NV                                        = $8709;
  GL_DSDT8_MAG8_NV                                   = $870A;
  GL_DSDT8_MAG8_INTENSITY8_NV                        = $870B;
  GL_SIGNED_RGB_UNSIGNED_ALPHA_NV                    = $870C;
  GL_SIGNED_RGB8_UNSIGNED_ALPHA8_NV                  = $870D;
  GL_HI_SCALE_NV                                     = $870E;
  GL_LO_SCALE_NV                                     = $870F;
  GL_DS_SCALE_NV                                     = $8710;
  GL_DT_SCALE_NV                                     = $8711;
  GL_MAGNITUDE_SCALE_NV                              = $8712;
  GL_VIBRANCE_SCALE_NV                               = $8713;
  GL_HI_BIAS_NV                                      = $8714;
  GL_LO_BIAS_NV                                      = $8715;
  GL_DS_BIAS_NV                                      = $8716;
  GL_DT_BIAS_NV                                      = $8717;
  GL_MAGNITUDE_BIAS_NV                               = $8718;
  GL_VIBRANCE_BIAS_NV                                = $8719;
  GL_TEXTURE_BORDER_VALUES_NV                        = $871A;
  GL_TEXTURE_HI_SIZE_NV                              = $871B;
  GL_TEXTURE_LO_SIZE_NV                              = $871C;
  GL_TEXTURE_DS_SIZE_NV                              = $871D;
  GL_TEXTURE_DT_SIZE_NV                              = $871E;
  GL_TEXTURE_MAG_SIZE_NV                             = $871F;

  // GL_NV_texture_shader2
  GL_DOT_PRODUCT_TEXTURE_3D_NV                       = $86EF;

  // GL_NV_texture_shader3
  GL_OFFSET_PROJECTIVE_TEXTURE_2D_NV                 = $8850;
  GL_OFFSET_PROJECTIVE_TEXTURE_2D_SCALE_NV           = $8851;
  GL_OFFSET_PROJECTIVE_TEXTURE_RECTANGLE_NV          = $8852;
  GL_OFFSET_PROJECTIVE_TEXTURE_RECTANGLE_SCALE_NV    = $8853;
  GL_OFFSET_HILO_TEXTURE_2D_NV                       = $8854;
  GL_OFFSET_HILO_TEXTURE_RECTANGLE_NV                = $8855;
  GL_OFFSET_HILO_PROJECTIVE_TEXTURE_2D_NV            = $8856;
  GL_OFFSET_HILO_PROJECTIVE_TEXTURE_RECTANGLE_NV     = $8857;
  GL_DEPENDENT_HILO_TEXTURE_2D_NV                    = $8858;
  GL_DEPENDENT_RGB_TEXTURE_3D_NV                     = $8859;
  GL_DEPENDENT_RGB_TEXTURE_CUBE_MAP_NV               = $885A;
  GL_DOT_PRODUCT_PASS_THROUGH_NV                     = $885B;
  GL_DOT_PRODUCT_TEXTURE_1D_NV                       = $885C;
  GL_DOT_PRODUCT_AFFINE_DEPTH_REPLACE_NV             = $885D;
  GL_HILO8_NV                                        = $885E;
  GL_SIGNED_HILO8_NV                                 = $885F;
  GL_FORCE_BLUE_TO_ONE_NV                            = $8860;

  // GL_NV_vertex_array_range
  GL_VERTEX_ARRAY_RANGE_NV                           = $851D;
  GL_VERTEX_ARRAY_RANGE_LENGTH_NV                    = $851E;
  GL_VERTEX_ARRAY_RANGE_VALID_NV                     = $851F;
  GL_MAX_VERTEX_ARRAY_RANGE_ELEMENT_NV               = $8520;
  GL_VERTEX_ARRAY_RANGE_POINTER_NV                   = $8521;

  // GL_NV_vertex_array_range2
  GL_VERTEX_ARRAY_RANGE_WITHOUT_FLUSH_NV             = $8533;

  // GL_NV_vertex_program
  GL_VERTEX_PROGRAM_NV                               = $8620;
  GL_VERTEX_STATE_PROGRAM_NV                         = $8621;
  GL_ATTRIB_ARRAY_SIZE_NV                            = $8623;
  GL_ATTRIB_ARRAY_STRIDE_NV                          = $8624;
  GL_ATTRIB_ARRAY_TYPE_NV                            = $8625;
  GL_CURRENT_ATTRIB_NV                               = $8626;
  GL_PROGRAM_LENGTH_NV                               = $8627;
  GL_PROGRAM_STRING_NV                               = $8628;
  GL_MODELVIEW_PROJECTION_NV                         = $8629;
  GL_IDENTITY_NV                                     = $862A;
  GL_INVERSE_NV                                      = $862B;
  GL_TRANSPOSE_NV                                    = $862C;
  GL_INVERSE_TRANSPOSE_NV                            = $862D;
  GL_MAX_TRACK_MATRIX_STACK_DEPTH_NV                 = $862E;
  GL_MAX_TRACK_MATRICES_NV                           = $862F;
  GL_MATRIX0_NV                                      = $8630;
  GL_MATRIX1_NV                                      = $8631;
  GL_MATRIX2_NV                                      = $8632;
  GL_MATRIX3_NV                                      = $8633;
  GL_MATRIX4_NV                                      = $8634;
  GL_MATRIX5_NV                                      = $8635;
  GL_MATRIX6_NV                                      = $8636;
  GL_MATRIX7_NV                                      = $8637;
  GL_CURRENT_MATRIX_STACK_DEPTH_NV                   = $8640;
  GL_CURRENT_MATRIX_NV                               = $8641;
  GL_VERTEX_PROGRAM_POINT_SIZE_NV                    = $8642;
  GL_VERTEX_PROGRAM_TWO_SIDE_NV                      = $8643;
  GL_PROGRAM_PARAMETER_NV                            = $8644;
  GL_ATTRIB_ARRAY_POINTER_NV                         = $8645;
  GL_PROGRAM_TARGET_NV                               = $8646;
  GL_PROGRAM_RESIDENT_NV                             = $8647;
  GL_TRACK_MATRIX_NV                                 = $8648;
  GL_TRACK_MATRIX_TRANSFORM_NV                       = $8649;
  GL_VERTEX_PROGRAM_BINDING_NV                       = $864A;
  GL_PROGRAM_ERROR_POSITION_NV                       = $864B;
  GL_VERTEX_ATTRIB_ARRAY0_NV                         = $8650;
  GL_VERTEX_ATTRIB_ARRAY1_NV                         = $8651;
  GL_VERTEX_ATTRIB_ARRAY2_NV                         = $8652;
  GL_VERTEX_ATTRIB_ARRAY3_NV                         = $8653;
  GL_VERTEX_ATTRIB_ARRAY4_NV                         = $8654;
  GL_VERTEX_ATTRIB_ARRAY5_NV                         = $8655;
  GL_VERTEX_ATTRIB_ARRAY6_NV                         = $8656;
  GL_VERTEX_ATTRIB_ARRAY7_NV                         = $8657;
  GL_VERTEX_ATTRIB_ARRAY8_NV                         = $8658;
  GL_VERTEX_ATTRIB_ARRAY9_NV                         = $8659;
  GL_VERTEX_ATTRIB_ARRAY10_NV                        = $865A;
  GL_VERTEX_ATTRIB_ARRAY11_NV                        = $865B;
  GL_VERTEX_ATTRIB_ARRAY12_NV                        = $865C;
  GL_VERTEX_ATTRIB_ARRAY13_NV                        = $865D;
  GL_VERTEX_ATTRIB_ARRAY14_NV                        = $865E;
  GL_VERTEX_ATTRIB_ARRAY15_NV                        = $865F;
  GL_MAP1_VERTEX_ATTRIB0_4_NV                        = $8660;
  GL_MAP1_VERTEX_ATTRIB1_4_NV                        = $8661;
  GL_MAP1_VERTEX_ATTRIB2_4_NV                        = $8662;
  GL_MAP1_VERTEX_ATTRIB3_4_NV                        = $8663;
  GL_MAP1_VERTEX_ATTRIB4_4_NV                        = $8664;
  GL_MAP1_VERTEX_ATTRIB5_4_NV                        = $8665;
  GL_MAP1_VERTEX_ATTRIB6_4_NV                        = $8666;
  GL_MAP1_VERTEX_ATTRIB7_4_NV                        = $8667;
  GL_MAP1_VERTEX_ATTRIB8_4_NV                        = $8668;
  GL_MAP1_VERTEX_ATTRIB9_4_NV                        = $8669;
  GL_MAP1_VERTEX_ATTRIB10_4_NV                       = $866A;
  GL_MAP1_VERTEX_ATTRIB11_4_NV                       = $866B;
  GL_MAP1_VERTEX_ATTRIB12_4_NV                       = $866C;
  GL_MAP1_VERTEX_ATTRIB13_4_NV                       = $866D;
  GL_MAP1_VERTEX_ATTRIB14_4_NV                       = $866E;
  GL_MAP1_VERTEX_ATTRIB15_4_NV                       = $866F;
  GL_MAP2_VERTEX_ATTRIB0_4_NV                        = $8670;
  GL_MAP2_VERTEX_ATTRIB1_4_NV                        = $8671;
  GL_MAP2_VERTEX_ATTRIB2_4_NV                        = $8672;
  GL_MAP2_VERTEX_ATTRIB3_4_NV                        = $8673;
  GL_MAP2_VERTEX_ATTRIB4_4_NV                        = $8674;
  GL_MAP2_VERTEX_ATTRIB5_4_NV                        = $8675;
  GL_MAP2_VERTEX_ATTRIB6_4_NV                        = $8676;
  GL_MAP2_VERTEX_ATTRIB7_4_NV                        = $8677;
  GL_MAP2_VERTEX_ATTRIB8_4_NV                        = $8678;
  GL_MAP2_VERTEX_ATTRIB9_4_NV                        = $8679;
  GL_MAP2_VERTEX_ATTRIB10_4_NV                       = $867A;
  GL_MAP2_VERTEX_ATTRIB11_4_NV                       = $867B;
  GL_MAP2_VERTEX_ATTRIB12_4_NV                       = $867C;
  GL_MAP2_VERTEX_ATTRIB13_4_NV                       = $867D;
  GL_MAP2_VERTEX_ATTRIB14_4_NV                       = $867E;
  GL_MAP2_VERTEX_ATTRIB15_4_NV                       = $867F;

  // GL_OML_interlace
  GL_INTERLACE_OML                                   = $8980;
  GL_INTERLACE_READ_OML                              = $8981;

  // GL_OML_resample
  GL_PACK_RESAMPLE_OML                               = $8984;
  GL_UNPACK_RESAMPLE_OML                             = $8985;
  GL_RESAMPLE_REPLICATE_OML                          = $8986;
  GL_RESAMPLE_ZERO_FILL_OML                          = $8987;
  GL_RESAMPLE_AVERAGE_OML                            = $8988;
  GL_RESAMPLE_DECIMATE_OML                           = $8989;

  // GL_OML_subsample
  GL_FORMAT_SUBSAMPLE_24_24_OML                      = $8982;
  GL_FORMAT_SUBSAMPLE_244_244_OML                    = $8983;

  // GL_PGI_misc_hints
  GL_PREFER_DOUBLEBUFFER_HINT_PGI                    = $1A1F8;
  GL_CONSERVE_MEMORY_HINT_PGI                        = $1A1FD;
  GL_RECLAIM_MEMORY_HINT_PGI                         = $1A1FE;
  GL_NATIVE_GRAPHICS_HANDLE_PGI                      = $1A202;
  GL_NATIVE_GRAPHICS_BEGIN_HINT_PGI                  = $1A203;
  GL_NATIVE_GRAPHICS_END_HINT_PGI                    = $1A204;
  GL_ALWAYS_FAST_HINT_PGI                            = $1A20C;
  GL_ALWAYS_SOFT_HINT_PGI                            = $1A20D;
  GL_ALLOW_DRAW_OBJ_HINT_PGI                         = $1A20E;
  GL_ALLOW_DRAW_WIN_HINT_PGI                         = $1A20F;
  GL_ALLOW_DRAW_FRG_HINT_PGI                         = $1A210;
  GL_ALLOW_DRAW_MEM_HINT_PGI                         = $1A211;
  GL_STRICT_DEPTHFUNC_HINT_PGI                       = $1A216;
  GL_STRICT_LIGHTING_HINT_PGI                        = $1A217;
  GL_STRICT_SCISSOR_HINT_PGI                         = $1A218;
  GL_FULL_STIPPLE_HINT_PGI                           = $1A219;
  GL_CLIP_NEAR_HINT_PGI                              = $1A220;
  GL_CLIP_FAR_HINT_PGI                               = $1A221;
  GL_WIDE_LINE_HINT_PGI                              = $1A222;
  GL_BACK_NORMALS_HINT_PGI                           = $1A223;

  // GL_PGI_vertex_hints
  GL_VERTEX_DATA_HINT_PGI                            = $1A22A;
  GL_VERTEX_CONSISTENT_HINT_PGI                      = $1A22B;
  GL_MATERIAL_SIDE_HINT_PGI                          = $1A22C;
  GL_MAX_VERTEX_HINT_PGI                             = $1A22D;
  GL_COLOR3_BIT_PGI                                  = $00010000;
  GL_COLOR4_BIT_PGI                                  = $00020000;
  GL_EDGEFLAG_BIT_PGI                                = $00040000;
  GL_INDEX_BIT_PGI                                   = $00080000;
  GL_MAT_AMBIENT_BIT_PGI                             = $00100000;
  GL_MAT_AMBIENT_AND_DIFFUSE_BIT_PGI                 = $00200000;
  GL_MAT_DIFFUSE_BIT_PGI                             = $00400000;
  GL_MAT_EMISSION_BIT_PGI                            = $00800000;
  GL_MAT_COLOR_INDEXES_BIT_PGI                       = $01000000;
  GL_MAT_SHININESS_BIT_PGI                           = $02000000;
  GL_MAT_SPECULAR_BIT_PGI                            = $04000000;
  GL_NORMAL_BIT_PGI                                  = $08000000;
  GL_TEXCOORD1_BIT_PGI                               = $10000000;
  GL_TEXCOORD2_BIT_PGI                               = $20000000;
  GL_TEXCOORD3_BIT_PGI                               = $40000000;
  GL_TEXCOORD4_BIT_PGI                               = $80000000;
  GL_VERTEX23_BIT_PGI                                = $00000004;
  GL_VERTEX4_BIT_PGI                                 = $00000008;

  // GL_REND_screen_coordinates
  GL_SCREEN_COORDINATES_REND                         = $8490;
  GL_INVERTED_SCREEN_W_REND                          = $8491;

  // GL_S3_s3tc
  GL_RGB_S3TC                                        = $83A0;
  GL_RGB4_S3TC                                       = $83A1;
  GL_RGBA_S3TC                                       = $83A2;
  GL_RGBA4_S3TC                                      = $83A3;

  // GL_SGIS_detail_texture
  GL_DETAIL_TEXTURE_2D_SGIS                          = $8095;
  GL_DETAIL_TEXTURE_2D_BINDING_SGIS                  = $8096;
  GL_LINEAR_DETAIL_SGIS                              = $8097;
  GL_LINEAR_DETAIL_ALPHA_SGIS                        = $8098;
  GL_LINEAR_DETAIL_COLOR_SGIS                        = $8099;
  GL_DETAIL_TEXTURE_LEVEL_SGIS                       = $809A;
  GL_DETAIL_TEXTURE_MODE_SGIS                        = $809B;
  GL_DETAIL_TEXTURE_FUNC_POINTS_SGIS                 = $809C;

  // GL_SGIS_fog_function
  GL_FOG_FUNC_SGIS                                   = $812A;
  GL_FOG_FUNC_POINTS_SGIS                            = $812B;
  GL_MAX_FOG_FUNC_POINTS_SGIS                        = $812C;

  // GL_SGIS_generate_mipmap
  GL_GENERATE_MIPMAP_SGIS                            = $8191;
  GL_GENERATE_MIPMAP_HINT_SGIS                       = $8192;

  // GL_SGIS_multisample
  GL_MULTISAMPLE_SGIS                                = $809D;
  GL_SAMPLE_ALPHA_TO_MASK_SGIS                       = $809E;
  GL_SAMPLE_ALPHA_TO_ONE_SGIS                        = $809F;
  GL_SAMPLE_MASK_SGIS                                = $80A0;
  GL_1PASS_SGIS                                      = $80A1;
  GL_2PASS_0_SGIS                                    = $80A2;
  GL_2PASS_1_SGIS                                    = $80A3;
  GL_4PASS_0_SGIS                                    = $80A4;
  GL_4PASS_1_SGIS                                    = $80A5;
  GL_4PASS_2_SGIS                                    = $80A6;
  GL_4PASS_3_SGIS                                    = $80A7;
  GL_SAMPLE_BUFFERS_SGIS                             = $80A8;
  GL_SAMPLES_SGIS                                    = $80A9;
  GL_SAMPLE_MASK_VALUE_SGIS                          = $80AA;
  GL_SAMPLE_MASK_INVERT_SGIS                         = $80AB;
  GL_SAMPLE_PATTERN_SGIS                             = $80AC;

  // GL_SGIS_pixel_texture
  GL_PIXEL_TEXTURE_SGIS                              = $8353;
  GL_PIXEL_FRAGMENT_RGB_SOURCE_SGIS                  = $8354;
  GL_PIXEL_FRAGMENT_ALPHA_SOURCE_SGIS                = $8355;
  GL_PIXEL_GROUP_COLOR_SGIS                          = $8356;

  // GL_SGIS_point_line_texgen
  GL_EYE_DISTANCE_TO_POINT_SGIS                      = $81F0;
  GL_OBJECT_DISTANCE_TO_POINT_SGIS                   = $81F1;
  GL_EYE_DISTANCE_TO_LINE_SGIS                       = $81F2;
  GL_OBJECT_DISTANCE_TO_LINE_SGIS                    = $81F3;
  GL_EYE_POINT_SGIS                                  = $81F4;
  GL_OBJECT_POINT_SGIS                               = $81F5;
  GL_EYE_LINE_SGIS                                   = $81F6;
  GL_OBJECT_LINE_SGIS                                = $81F7;

  // GL_SGIS_point_parameters
  GL_POINT_SIZE_MIN_SGIS                             = $8126;
  GL_POINT_SIZE_MAX_SGIS                             = $8127;
  GL_POINT_FADE_THRESHOLD_SIZE_SGIS                  = $8128;
  GL_DISTANCE_ATTENUATION_SGIS                       = $8129;

  // GL_SGIS_sharpen_texture
  GL_LINEAR_SHARPEN_SGIS                             = $80AD;
  GL_LINEAR_SHARPEN_ALPHA_SGIS                       = $80AE;
  GL_LINEAR_SHARPEN_COLOR_SGIS                       = $80AF;
  GL_SHARPEN_TEXTURE_FUNC_POINTS_SGIS                = $80B0;

  // GL_SGIS_texture4D
  GL_PACK_SKIP_VOLUMES_SGIS                          = $8130;
  GL_PACK_IMAGE_DEPTH_SGIS                           = $8131;
  GL_UNPACK_SKIP_VOLUMES_SGIS                        = $8132;
  GL_UNPACK_IMAGE_DEPTH_SGIS                         = $8133;
  GL_TEXTURE_4D_SGIS                                 = $8134;
  GL_PROXY_TEXTURE_4D_SGIS                           = $8135;
  GL_TEXTURE_4DSIZE_SGIS                             = $8136;
  GL_TEXTURE_WRAP_Q_SGIS                             = $8137;
  GL_MAX_4D_TEXTURE_SIZE_SGIS                        = $8138;
  GL_TEXTURE_4D_BINDING_SGIS                         = $814F;

  // GL_SGIS_texture_color_mask
  GL_TEXTURE_COLOR_WRITEMASK_SGIS                    = $81EF;

  // GL_SGIS_texture_edge_clamp
  GL_CLAMP_TO_EDGE_SGIS                              = $812F;

  // GL_SGIS_texture_filter4
  GL_FILTER4_SGIS                                    = $8146;
  GL_TEXTURE_FILTER4_SIZE_SGIS                       = $8147;

  // GL_SGIS_texture_lod
  GL_TEXTURE_MIN_LOD_SGIS                            = $813A;
  GL_TEXTURE_MAX_LOD_SGIS                            = $813B;
  GL_TEXTURE_BASE_LEVEL_SGIS                         = $813C;
  GL_TEXTURE_MAX_LEVEL_SGIS                          = $813D;

  // GL_SGIS_texture_select
  GL_DUAL_ALPHA4_SGIS                                = $8110;
  GL_DUAL_ALPHA8_SGIS                                = $8111;
  GL_DUAL_ALPHA12_SGIS                               = $8112;
  GL_DUAL_ALPHA16_SGIS                               = $8113;
  GL_DUAL_LUMINANCE4_SGIS                            = $8114;
  GL_DUAL_LUMINANCE8_SGIS                            = $8115;
  GL_DUAL_LUMINANCE12_SGIS                           = $8116;
  GL_DUAL_LUMINANCE16_SGIS                           = $8117;
  GL_DUAL_INTENSITY4_SGIS                            = $8118;
  GL_DUAL_INTENSITY8_SGIS                            = $8119;
  GL_DUAL_INTENSITY12_SGIS                           = $811A;
  GL_DUAL_INTENSITY16_SGIS                           = $811B;
  GL_DUAL_LUMINANCE_ALPHA4_SGIS                      = $811C;
  GL_DUAL_LUMINANCE_ALPHA8_SGIS                      = $811D;
  GL_QUAD_ALPHA4_SGIS                                = $811E;
  GL_QUAD_ALPHA8_SGIS                                = $811F;
  GL_QUAD_LUMINANCE4_SGIS                            = $8120;
  GL_QUAD_LUMINANCE8_SGIS                            = $8121;
  GL_QUAD_INTENSITY4_SGIS                            = $8122;
  GL_QUAD_INTENSITY8_SGIS                            = $8123;
  GL_DUAL_TEXTURE_SELECT_SGIS                        = $8124;
  GL_QUAD_TEXTURE_SELECT_SGIS                        = $8125;

  // GL_SGIX_async
  GL_ASYNC_MARKER_SGIX                               = $8329;

  // GL_SGIX_async_histogram
  GL_ASYNC_HISTOGRAM_SGIX                            = $832C;
  GL_MAX_ASYNC_HISTOGRAM_SGIX                        = $832D;

  // GL_SGIX_async_pixel
  GL_ASYNC_TEX_IMAGE_SGIX                            = $835C;
  GL_ASYNC_DRAW_PIXELS_SGIX                          = $835D;
  GL_ASYNC_READ_PIXELS_SGIX                          = $835E;
  GL_MAX_ASYNC_TEX_IMAGE_SGIX                        = $835F;
  GL_MAX_ASYNC_DRAW_PIXELS_SGIX                      = $8360;
  GL_MAX_ASYNC_READ_PIXELS_SGIX                      = $8361;

  // GL_SGIX_blend_alpha_minmax
  GL_ALPHA_MIN_SGIX                                  = $8320;
  GL_ALPHA_MAX_SGIX                                  = $8321;

  // GL_SGIX_calligraphic_fragment
  GL_CALLIGRAPHIC_FRAGMENT_SGIX                      = $8183;

  // GL_SGIX_clipmap
  GL_LINEAR_CLIPMAP_LINEAR_SGIX                      = $8170;
  GL_TEXTURE_CLIPMAP_CENTER_SGIX                     = $8171;
  GL_TEXTURE_CLIPMAP_FRAME_SGIX                      = $8172;
  GL_TEXTURE_CLIPMAP_OFFSET_SGIX                     = $8173;
  GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX              = $8174;
  GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX                 = $8175;
  GL_TEXTURE_CLIPMAP_DEPTH_SGIX                      = $8176;
  GL_MAX_CLIPMAP_DEPTH_SGIX                          = $8177;
  GL_MAX_CLIPMAP_VIRTUAL_DEPTH_SGIX                  = $8178;
  GL_NEAREST_CLIPMAP_NEAREST_SGIX                    = $844D;
  GL_NEAREST_CLIPMAP_LINEAR_SGIX                     = $844E;
  GL_LINEAR_CLIPMAP_NEAREST_SGIX                     = $844F;

  // GL_SGIX_convolution_accuracy
  GL_CONVOLUTION_HINT_SGIX                           = $8316;

  // GL_SGIX_depth_texture
  GL_DEPTH_COMPONENT16_SGIX                          = $81A5;
  GL_DEPTH_COMPONENT24_SGIX                          = $81A6;
  GL_DEPTH_COMPONENT32_SGIX                          = $81A7;

  // GL_SGIX_fog_offset
  GL_FOG_OFFSET_SGIX                                 = $8198;
  GL_FOG_OFFSET_VALUE_SGIX                           = $8199;

  // GL_SGIX_fog_scale
  GL_FOG_SCALE_SGIX                                  = $81FC;
  GL_FOG_SCALE_VALUE_SGIX                            = $81FD;

  // GL_SGIX_fragment_lighting
  GL_FRAGMENT_LIGHTING_SGIX                          = $8400;
  GL_FRAGMENT_COLOR_MATERIAL_SGIX                    = $8401;
  GL_FRAGMENT_COLOR_MATERIAL_FACE_SGIX               = $8402;
  GL_FRAGMENT_COLOR_MATERIAL_PARAMETER_SGIX          = $8403;
  GL_MAX_FRAGMENT_LIGHTS_SGIX                        = $8404;
  GL_MAX_ACTIVE_LIGHTS_SGIX                          = $8405;
  GL_CURRENT_RASTER_NORMAL_SGIX                      = $8406;
  GL_LIGHT_ENV_MODE_SGIX                             = $8407;
  GL_FRAGMENT_LIGHT_MODEL_LOCAL_VIEWER_SGIX          = $8408;
  GL_FRAGMENT_LIGHT_MODEL_TWO_SIDE_SGIX              = $8409;
  GL_FRAGMENT_LIGHT_MODEL_AMBIENT_SGIX               = $840A;
  GL_FRAGMENT_LIGHT_MODEL_NORMAL_INTERPOLATION_SGIX  = $840B;
  GL_FRAGMENT_LIGHT0_SGIX                            = $840C;
  GL_FRAGMENT_LIGHT1_SGIX                            = $840D;
  GL_FRAGMENT_LIGHT2_SGIX                            = $840E;
  GL_FRAGMENT_LIGHT3_SGIX                            = $840F;
  GL_FRAGMENT_LIGHT4_SGIX                            = $8410;
  GL_FRAGMENT_LIGHT5_SGIX                            = $8411;
  GL_FRAGMENT_LIGHT6_SGIX                            = $8412;
  GL_FRAGMENT_LIGHT7_SGIX                            = $8413;

  // GL_SGIX_framezoom
  GL_FRAMEZOOM_SGIX                                  = $818B;
  GL_FRAMEZOOM_FACTOR_SGIX                           = $818C;
  GL_MAX_FRAMEZOOM_FACTOR_SGIX                       = $818D;

  // GL_SGIX_impact_pixel_texture
  GL_PIXEL_TEX_GEN_Q_CEILING_SGIX                    = $8184;
  GL_PIXEL_TEX_GEN_Q_ROUND_SGIX                      = $8185;
  GL_PIXEL_TEX_GEN_Q_FLOOR_SGIX                      = $8186;
  GL_PIXEL_TEX_GEN_ALPHA_REPLACE_SGIX                = $8187;
  GL_PIXEL_TEX_GEN_ALPHA_NO_REPLACE_SGIX             = $8188;
  GL_PIXEL_TEX_GEN_ALPHA_LS_SGIX                     = $8189;
  GL_PIXEL_TEX_GEN_ALPHA_MS_SGIX                     = $818A;

  // GL_SGIX_instruments
  GL_INSTRUMENT_BUFFER_POINTER_SGIX                  = $8180;
  GL_INSTRUMENT_MEASUREMENTS_SGIX                    = $8181;

  // GL_SGIX_interlace
  GL_INTERLACE_SGIX                                  = $8094;

  // GL_SGIX_ir_instrument1
  GL_IR_INSTRUMENT1_SGIX                             = $817F;

  // GL_SGIX_list_priority
  GL_LIST_PRIORITY_SGIX                              = $8182;

  // GL_SGIX_pixel_texture
  GL_PIXEL_TEX_GEN_SGIX                              = $8139;
  GL_PIXEL_TEX_GEN_MODE_SGIX                         = $832B;

  // GL_SGIX_pixel_tiles
  GL_PIXEL_TILE_BEST_ALIGNMENT_SGIX                  = $813E;
  GL_PIXEL_TILE_CACHE_INCREMENT_SGIX                 = $813F;
  GL_PIXEL_TILE_WIDTH_SGIX                           = $8140;
  GL_PIXEL_TILE_HEIGHT_SGIX                          = $8141;
  GL_PIXEL_TILE_GRID_WIDTH_SGIX                      = $8142;
  GL_PIXEL_TILE_GRID_HEIGHT_SGIX                     = $8143;
  GL_PIXEL_TILE_GRID_DEPTH_SGIX                      = $8144;
  GL_PIXEL_TILE_CACHE_SIZE_SGIX                      = $8145;

  // GL_SGIX_polynomial_ffd
  GL_GEOMETRY_DEFORMATION_SGIX                       = $8194;
  GL_TEXTURE_DEFORMATION_SGIX                        = $8195;
  GL_DEFORMATIONS_MASK_SGIX                          = $8196;
  GL_MAX_DEFORMATION_ORDER_SGIX                      = $8197;

  // GL_SGIX_reference_plane
  GL_REFERENCE_PLANE_SGIX                            = $817D;
  GL_REFERENCE_PLANE_EQUATION_SGIX                   = $817E;

  // GL_SGIX_resample
  GL_PACK_RESAMPLE_SGIX                              = $842C;
  GL_UNPACK_RESAMPLE_SGIX                            = $842D;
  GL_RESAMPLE_REPLICATE_SGIX                         = $842E;
  GL_RESAMPLE_ZERO_FILL_SGIX                         = $842F;
  GL_RESAMPLE_DECIMATE_SGIX                          = $8430;

  // GL_SGIX_scalebias_hint
  GL_SCALEBIAS_HINT_SGIX                             = $8322;

  // GL_SGIX_shadow
  GL_TEXTURE_COMPARE_SGIX                            = $819A;
  GL_TEXTURE_COMPARE_OPERATOR_SGIX                   = $819B;
  GL_TEXTURE_LEQUAL_R_SGIX                           = $819C;
  GL_TEXTURE_GEQUAL_R_SGIX                           = $819D;

  // GL_SGIX_shadow_ambient
  GL_SHADOW_AMBIENT_SGIX                             = $80BF;

  // GL_SGIX_sprite
  GL_SPRITE_SGIX                                     = $8148;
  GL_SPRITE_MODE_SGIX                                = $8149;
  GL_SPRITE_AXIS_SGIX                                = $814A;
  GL_SPRITE_TRANSLATION_SGIX                         = $814B;
  GL_SPRITE_AXIAL_SGIX                               = $814C;
  GL_SPRITE_OBJECT_ALIGNED_SGIX                      = $814D;
  GL_SPRITE_EYE_ALIGNED_SGIX                         = $814E;

  // GL_SGIX_subsample
  GL_PACK_SUBSAMPLE_RATE_SGIX                        = $85A0;
  GL_UNPACK_SUBSAMPLE_RATE_SGIX                      = $85A1;
  GL_PIXEL_SUBSAMPLE_4444_SGIX                       = $85A2;
  GL_PIXEL_SUBSAMPLE_2424_SGIX                       = $85A3;
  GL_PIXEL_SUBSAMPLE_4242_SGIX                       = $85A4;

  // GL_SGIX_texture_add_env
  GL_TEXTURE_ENV_BIAS_SGIX                           = $80BE;

  // GL_SGIX_texture_coordinate_clamp
  GL_TEXTURE_MAX_CLAMP_S_SGIX                        = $8369;
  GL_TEXTURE_MAX_CLAMP_T_SGIX                        = $836A;
  GL_TEXTURE_MAX_CLAMP_R_SGIX                        = $836B;

  // GL_SGIX_texture_lod_bias
  GL_TEXTURE_LOD_BIAS_S_SGIX                         = $818E;
  GL_TEXTURE_LOD_BIAS_T_SGIX                         = $818F;
  GL_TEXTURE_LOD_BIAS_R_SGIX                         = $8190;

  // GL_SGIX_texture_multi_buffer
  GL_TEXTURE_MULTI_BUFFER_HINT_SGIX                  = $812E;

  // GL_SGIX_texture_scale_bias
  GL_POST_TEXTURE_FILTER_BIAS_SGIX                   = $8179;
  GL_POST_TEXTURE_FILTER_SCALE_SGIX                  = $817A;
  GL_POST_TEXTURE_FILTER_BIAS_RANGE_SGIX             = $817B;
  GL_POST_TEXTURE_FILTER_SCALE_RANGE_SGIX            = $817C;

  // GL_SGIX_vertex_preclip
  GL_VERTEX_PRECLIP_SGIX                             = $83EE;
  GL_VERTEX_PRECLIP_HINT_SGIX                        = $83EF;

  // GL_SGIX_ycrcb
  GL_YCRCB_422_SGIX                                  = $81BB;
  GL_YCRCB_444_SGIX                                  = $81BC;

  // GL_SGIX_ycrcba
  GL_YCRCB_SGIX                                      = $8318;
  GL_YCRCBA_SGIX                                     = $8319;

  // GL_SGI_color_matrix
  GL_COLOR_MATRIX_SGI                                = $80B1;
  GL_COLOR_MATRIX_STACK_DEPTH_SGI                    = $80B2;
  GL_MAX_COLOR_MATRIX_STACK_DEPTH_SGI                = $80B3;
  GL_POST_COLOR_MATRIX_RED_SCALE_SGI                 = $80B4;
  GL_POST_COLOR_MATRIX_GREEN_SCALE_SGI               = $80B5;
  GL_POST_COLOR_MATRIX_BLUE_SCALE_SGI                = $80B6;
  GL_POST_COLOR_MATRIX_ALPHA_SCALE_SGI               = $80B7;
  GL_POST_COLOR_MATRIX_RED_BIAS_SGI                  = $80B8;
  GL_POST_COLOR_MATRIX_GREEN_BIAS_SGI                = $80B9;
  GL_POST_COLOR_MATRIX_BLUE_BIAS_SGI                 = $80BA;
  GL_POST_COLOR_MATRIX_ALPHA_BIAS_SGI                = $80BB;

  // GL_SGI_color_table
  GL_COLOR_TABLE_SGI                                 = $80D0;
  GL_POST_CONVOLUTION_COLOR_TABLE_SGI                = $80D1;
  GL_POST_COLOR_MATRIX_COLOR_TABLE_SGI               = $80D2;
  GL_PROXY_COLOR_TABLE_SGI                           = $80D3;
  GL_PROXY_POST_CONVOLUTION_COLOR_TABLE_SGI          = $80D4;
  GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE_SGI         = $80D5;
  GL_COLOR_TABLE_SCALE_SGI                           = $80D6;
  GL_COLOR_TABLE_BIAS_SGI                            = $80D7;
  GL_COLOR_TABLE_FORMAT_SGI                          = $80D8;
  GL_COLOR_TABLE_WIDTH_SGI                           = $80D9;
  GL_COLOR_TABLE_RED_SIZE_SGI                        = $80DA;
  GL_COLOR_TABLE_GREEN_SIZE_SGI                      = $80DB;
  GL_COLOR_TABLE_BLUE_SIZE_SGI                       = $80DC;
  GL_COLOR_TABLE_ALPHA_SIZE_SGI                      = $80DD;
  GL_COLOR_TABLE_LUMINANCE_SIZE_SGI                  = $80DE;
  GL_COLOR_TABLE_INTENSITY_SIZE_SGI                  = $80DF;

  // GL_SGI_depth_pass_instrument
  GL_DEPTH_PASS_INSTRUMENT_SGIX                      = $8310;
  GL_DEPTH_PASS_INSTRUMENT_COUNTERS_SGIX             = $8311;
  GL_DEPTH_PASS_INSTRUMENT_MAX_SGIX                  = $8312;

  // GL_SGI_texture_color_table
  GL_TEXTURE_COLOR_TABLE_SGI                         = $80BC;
  GL_PROXY_TEXTURE_COLOR_TABLE_SGI                   = $80BD;

  // GL_SUNX_constant_data
  GL_UNPACK_CONSTANT_DATA_SUNX                       = $81D5;
  GL_TEXTURE_CONSTANT_DATA_SUNX                      = $81D6;

  // GL_SUN_convolution_border_modes
  GL_WRAP_BORDER_SUN                                 = $81D4;

  // GL_SUN_global_alpha
  GL_GLOBAL_ALPHA_SUN                                = $81D9;
  GL_GLOBAL_ALPHA_FACTOR_SUN                         = $81DA;

  // GL_SUN_mesh_array
  GL_QUAD_MESH_SUN                                   = $8614;
  GL_TRIANGLE_MESH_SUN                               = $8615;

  // GL_SUN_slice_accum
  GL_SLICE_ACCUM_SUN                                 = $85CC;

  // GL_SUN_triangle_list
  GL_RESTART_SUN                                     = $0001;
  GL_REPLACE_MIDDLE_SUN                              = $0002;
  GL_REPLACE_OLDEST_SUN                              = $0003;
  GL_TRIANGLE_LIST_SUN                               = $81D7;
  GL_REPLACEMENT_CODE_SUN                            = $81D8;
  GL_REPLACEMENT_CODE_ARRAY_SUN                      = $85C0;
  GL_REPLACEMENT_CODE_ARRAY_TYPE_SUN                 = $85C1;
  GL_REPLACEMENT_CODE_ARRAY_STRIDE_SUN               = $85C2;
  GL_REPLACEMENT_CODE_ARRAY_POINTER_SUN              = $85C3;
  GL_R1UI_V3F_SUN                                    = $85C4;
  GL_R1UI_C4UB_V3F_SUN                               = $85C5;
  GL_R1UI_C3F_V3F_SUN                                = $85C6;
  GL_R1UI_N3F_V3F_SUN                                = $85C7;
  GL_R1UI_C4F_N3F_V3F_SUN                            = $85C8;
  GL_R1UI_T2F_V3F_SUN                                = $85C9;
  GL_R1UI_T2F_N3F_V3F_SUN                            = $85CA;
  GL_R1UI_T2F_C4F_N3F_V3F_SUN                        = $85CB;

  // GL_WIN_phong_shading
  GL_PHONG_WIN                                       = $80EA;
  GL_PHONG_HINT_WIN                                  = $80EB;

  // GL_WIN_specular_fog
  GL_FOG_SPECULAR_TEXTURE_WIN                        = $80EC;

  // ===== OpenGL 1.5 ==========================================================
   // GL_ARB_vertex_shader                            OpenGL 1.5
  GL_VERTEX_SHADER_ARB                               = $8B31;
  GL_MAX_VERTEX_UNIFORM_COMPONENTS_ARB               = $8B4A;
  GL_MAX_VARYING_FLOATS_ARB                          = $8B4B;
  GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS_ARB              = $8B4C;
  GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS_ARB            = $8B4D;
  GL_OBJECT_ACTIVE_ATTRIBUTES_ARB                    = $8B89;
  GL_OBJECT_ACTIVE_ATTRIBUTE_MAX_LENGTH_ARB          = $8B8A;

  // GL_ARB_fragment_shader                          OpenGL 1.5
  GL_FRAGMENT_SHADER_ARB                             = $8B30;

  // GL_ARB_occlusion_query                          OpenGL 1.5
  GL_SAMPLES_PASSED_ARB                              = $8914;
  GL_QUERY_COUNTER_BITS_ARB                          = $8864;
  GL_CURRENT_QUERY_ARB                               = $8865;
  GL_QUERY_RESULT_ARB                                = $8866;
  GL_QUERY_RESULT_AVAILABLE_ARB                      = $8867;

  // GL_ARB_occlusion_query ARB less version         OpenGL 1.5
  GL_SAMPLES_PASSED                              = $8914;
  GL_QUERY_COUNTER_BITS                          = $8864;
  GL_CURRENT_QUERY                               = $8865;
  GL_QUERY_RESULT                                = $8866;
  GL_QUERY_RESULT_AVAILABLE                      = $8867;

  // GL_ARB_point_sprite                             OpenGL 1.5
  GL_POINT_SPRITE_ARB                                = $8861;
  GL_COORD_REPLACE_ARB                               = $8862;

  // GL_ARB_SHADER_OBJECTS                           OpenGL 1.5
  GL_PROGRAM_OBJECT_ARB                              = $8B40;

  GL_OBJECT_TYPE_ARB                                 = $8B4E;
  GL_OBJECT_SUBTYPE_ARB                              = $8B4F;
  GL_OBJECT_DELETE_STATUS_ARB                        = $8B80;
  GL_OBJECT_COMPILE_STATUS_ARB                       = $8B81;
  GL_OBJECT_LINK_STATUS_ARB                          = $8B82;
  GL_OBJECT_VALIDATE_STATUS_ARB                      = $8B83;
  GL_OBJECT_INFO_LOG_LENGTH_ARB                      = $8B84;
  GL_OBJECT_ATTACHED_OBJECTS_ARB                     = $8B85;
  GL_OBJECT_ACTIVE_UNIFORMS_ARB                      = $8B86;
  GL_OBJECT_ACTIVE_UNIFORM_MAX_LENGTH_ARB            = $8B87;
  GL_OBJECT_SHADER_SOURCE_LENGTH_ARB                 = $8B88;

  GL_SHADER_OBJECT_ARB                               = $8B48;

  GL_FLOAT_VEC2_ARB                                  = $8B50;
  GL_FLOAT_VEC3_ARB                                  = $8B51;
  GL_FLOAT_VEC4_ARB                                  = $8B52;
  GL_INT_VEC2_ARB                                    = $8B53;
  GL_INT_VEC3_ARB                                    = $8B54;
  GL_INT_VEC4_ARB                                    = $8B55;
  GL_BOOL_ARB                                        = $8B56;
  GL_BOOL_VEC2_ARB                                   = $8B57;
  GL_BOOL_VEC3_ARB                                   = $8B58;
  GL_BOOL_VEC4_ARB                                   = $8B59;
  GL_FLOAT_MAT2_ARB                                  = $8B5A;
  GL_FLOAT_MAT3_ARB                                  = $8B5B;
  GL_FLOAT_MAT4_ARB                                  = $8B5C;

  // ===== OpenGL 1.5 = End ====================================================

  // WGL_3DFX_multisample
  WGL_SAMPLE_BUFFERS_3DFX                            = $2060;
  WGL_SAMPLES_3DFX                                   = $2061;

  // WGL_ARB_buffer_region
  WGL_FRONT_COLOR_BUFFER_BIT_ARB                     = $00000001;
  WGL_BACK_COLOR_BUFFER_BIT_ARB                      = $00000002;
  WGL_DEPTH_BUFFER_BIT_ARB                           = $00000004;
  WGL_STENCIL_BUFFER_BIT_ARB                         = $00000008;

  // WGL_ARB_make_current_read
  ERROR_INVALID_PIXEL_TYPE_ARB                       = $2043;
  ERROR_INCOMPATIBLE_DEVICE_CONTEXTS_ARB             = $2054;

  // WGL_ARB_multisample
  WGL_SAMPLE_BUFFERS_ARB                             = $2041;
  WGL_SAMPLES_ARB                                    = $2042;

  // WGL_ARB_pbuffer
  WGL_DRAW_TO_PBUFFER_ARB                            = $202D;
  WGL_MAX_PBUFFER_PIXELS_ARB                         = $202E;
  WGL_MAX_PBUFFER_WIDTH_ARB                          = $202F;
  WGL_MAX_PBUFFER_HEIGHT_ARB                         = $2030;
  WGL_PBUFFER_LARGEST_ARB                            = $2033;
  WGL_PBUFFER_WIDTH_ARB                              = $2034;
  WGL_PBUFFER_HEIGHT_ARB                             = $2035;
  WGL_PBUFFER_LOST_ARB                               = $2036;

  // WGL_ARB_pixel_format
  WGL_NUMBER_PIXEL_FORMATS_ARB                       = $2000;
  WGL_DRAW_TO_WINDOW_ARB                             = $2001;
  WGL_DRAW_TO_BITMAP_ARB                             = $2002;
  WGL_ACCELERATION_ARB                               = $2003;
  WGL_NEED_PALETTE_ARB                               = $2004;
  WGL_NEED_SYSTEM_PALETTE_ARB                        = $2005;
  WGL_SWAP_LAYER_BUFFERS_ARB                         = $2006;
  WGL_SWAP_METHOD_ARB                                = $2007;
  WGL_NUMBER_OVERLAYS_ARB                            = $2008;
  WGL_NUMBER_UNDERLAYS_ARB                           = $2009;
  WGL_TRANSPARENT_ARB                                = $200A;
  WGL_TRANSPARENT_RED_VALUE_ARB                      = $2037;
  WGL_TRANSPARENT_GREEN_VALUE_ARB                    = $2038;
  WGL_TRANSPARENT_BLUE_VALUE_ARB                     = $2039;
  WGL_TRANSPARENT_ALPHA_VALUE_ARB                    = $203A;
  WGL_TRANSPARENT_INDEX_VALUE_ARB                    = $203B;
  WGL_SHARE_DEPTH_ARB                                = $200C;
  WGL_SHARE_STENCIL_ARB                              = $200D;
  WGL_SHARE_ACCUM_ARB                                = $200E;
  WGL_SUPPORT_GDI_ARB                                = $200F;
  WGL_SUPPORT_OPENGL_ARB                             = $2010;
  WGL_DOUBLE_BUFFER_ARB                              = $2011;
  WGL_STEREO_ARB                                     = $2012;
  WGL_PIXEL_TYPE_ARB                                 = $2013;
  WGL_COLOR_BITS_ARB                                 = $2014;
  WGL_RED_BITS_ARB                                   = $2015;
  WGL_RED_SHIFT_ARB                                  = $2016;
  WGL_GREEN_BITS_ARB                                 = $2017;
  WGL_GREEN_SHIFT_ARB                                = $2018;
  WGL_BLUE_BITS_ARB                                  = $2019;
  WGL_BLUE_SHIFT_ARB                                 = $201A;
  WGL_ALPHA_BITS_ARB                                 = $201B;
  WGL_ALPHA_SHIFT_ARB                                = $201C;
  WGL_ACCUM_BITS_ARB                                 = $201D;
  WGL_ACCUM_RED_BITS_ARB                             = $201E;
  WGL_ACCUM_GREEN_BITS_ARB                           = $201F;
  WGL_ACCUM_BLUE_BITS_ARB                            = $2020;
  WGL_ACCUM_ALPHA_BITS_ARB                           = $2021;
  WGL_DEPTH_BITS_ARB                                 = $2022;
  WGL_STENCIL_BITS_ARB                               = $2023;
  WGL_AUX_BUFFERS_ARB                                = $2024;
  WGL_NO_ACCELERATION_ARB                            = $2025;
  WGL_GENERIC_ACCELERATION_ARB                       = $2026;
  WGL_FULL_ACCELERATION_ARB                          = $2027;
  WGL_SWAP_EXCHANGE_ARB                              = $2028;
  WGL_SWAP_COPY_ARB                                  = $2029;
  WGL_SWAP_UNDEFINED_ARB                             = $202A;
  WGL_TYPE_RGBA_ARB                                  = $202B;
  WGL_TYPE_COLORINDEX_ARB                            = $202C;

  // WGL_ARB_render_texture
  WGL_BIND_TO_TEXTURE_RGB_ARB                        = $2070;
  WGL_BIND_TO_TEXTURE_RGBA_ARB                       = $2071;
  WGL_TEXTURE_FORMAT_ARB                             = $2072;
  WGL_TEXTURE_TARGET_ARB                             = $2073;
  WGL_MIPMAP_TEXTURE_ARB                             = $2074;
  WGL_TEXTURE_RGB_ARB                                = $2075;
  WGL_TEXTURE_RGBA_ARB                               = $2076;
  WGL_NO_TEXTURE_ARB                                 = $2077;
  WGL_TEXTURE_CUBE_MAP_ARB                           = $2078;
  WGL_TEXTURE_1D_ARB                                 = $2079;
  WGL_TEXTURE_2D_ARB                                 = $207A;
  WGL_MIPMAP_LEVEL_ARB                               = $207B;
  WGL_CUBE_MAP_FACE_ARB                              = $207C;
  WGL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB                = $207D;
  WGL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB                = $207E;
  WGL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB                = $207F;
  WGL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB                = $2080;
  WGL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB                = $2081;
  WGL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB                = $2082;
  WGL_FRONT_LEFT_ARB                                 = $2083;
  WGL_FRONT_RIGHT_ARB                                = $2084;
  WGL_BACK_LEFT_ARB                                  = $2085;
  WGL_BACK_RIGHT_ARB                                 = $2086;
  WGL_AUX0_ARB                                       = $2087;
  WGL_AUX1_ARB                                       = $2088;
  WGL_AUX2_ARB                                       = $2089;
  WGL_AUX3_ARB                                       = $208A;
  WGL_AUX4_ARB                                       = $208B;
  WGL_AUX5_ARB                                       = $208C;
  WGL_AUX6_ARB                                       = $208D;
  WGL_AUX7_ARB                                       = $208E;
  WGL_AUX8_ARB                                       = $208F;
  WGL_AUX9_ARB                                       = $2090;


  // WGL_ATI_pixel_format_float
  WGL_TYPE_RGBA_FLOAT_ATI                            = $21A0;
  GL_TYPE_RGBA_FLOAT_ATI                             = $8820;
  GL_COLOR_CLEAR_UNCLAMPED_VALUE_ATI                 = $8835;

  // WGL_EXT_depth_float
  WGL_DEPTH_FLOAT_EXT                                = $2040;

  // WGL_EXT_make_current_read
  ERROR_INVALID_PIXEL_TYPE_EXT                       = $2043;

  // WGL_EXT_multisample
  WGL_SAMPLE_BUFFERS_EXT                             = $2041;
  WGL_SAMPLES_EXT                                    = $2042;

  // WGL_EXT_pbuffer
  WGL_DRAW_TO_PBUFFER_EXT                            = $202D;
  WGL_MAX_PBUFFER_PIXELS_EXT                         = $202E;
  WGL_MAX_PBUFFER_WIDTH_EXT                          = $202F;
  WGL_MAX_PBUFFER_HEIGHT_EXT                         = $2030;
  WGL_OPTIMAL_PBUFFER_WIDTH_EXT                      = $2031;
  WGL_OPTIMAL_PBUFFER_HEIGHT_EXT                     = $2032;
  WGL_PBUFFER_LARGEST_EXT                            = $2033;
  WGL_PBUFFER_WIDTH_EXT                              = $2034;
  WGL_PBUFFER_HEIGHT_EXT                             = $2035;

  // WGL_EXT_pixel_format
  WGL_NUMBER_PIXEL_FORMATS_EXT                       = $2000;
  WGL_DRAW_TO_WINDOW_EXT                             = $2001;
  WGL_DRAW_TO_BITMAP_EXT                             = $2002;
  WGL_ACCELERATION_EXT                               = $2003;
  WGL_NEED_PALETTE_EXT                               = $2004;
  WGL_NEED_SYSTEM_PALETTE_EXT                        = $2005;
  WGL_SWAP_LAYER_BUFFERS_EXT                         = $2006;
  WGL_SWAP_METHOD_EXT                                = $2007;
  WGL_NUMBER_OVERLAYS_EXT                            = $2008;
  WGL_NUMBER_UNDERLAYS_EXT                           = $2009;
  WGL_TRANSPARENT_EXT                                = $200A;
  WGL_TRANSPARENT_VALUE_EXT                          = $200B;
  WGL_SHARE_DEPTH_EXT                                = $200C;
  WGL_SHARE_STENCIL_EXT                              = $200D;
  WGL_SHARE_ACCUM_EXT                                = $200E;
  WGL_SUPPORT_GDI_EXT                                = $200F;
  WGL_SUPPORT_OPENGL_EXT                             = $2010;
  WGL_DOUBLE_BUFFER_EXT                              = $2011;
  WGL_STEREO_EXT                                     = $2012;
  WGL_PIXEL_TYPE_EXT                                 = $2013;
  WGL_COLOR_BITS_EXT                                 = $2014;
  WGL_RED_BITS_EXT                                   = $2015;
  WGL_RED_SHIFT_EXT                                  = $2016;
  WGL_GREEN_BITS_EXT                                 = $2017;
  WGL_GREEN_SHIFT_EXT                                = $2018;
  WGL_BLUE_BITS_EXT                                  = $2019;
  WGL_BLUE_SHIFT_EXT                                 = $201A;
  WGL_ALPHA_BITS_EXT                                 = $201B;
  WGL_ALPHA_SHIFT_EXT                                = $201C;
  WGL_ACCUM_BITS_EXT                                 = $201D;
  WGL_ACCUM_RED_BITS_EXT                             = $201E;
  WGL_ACCUM_GREEN_BITS_EXT                           = $201F;
  WGL_ACCUM_BLUE_BITS_EXT                            = $2020;
  WGL_ACCUM_ALPHA_BITS_EXT                           = $2021;
  WGL_DEPTH_BITS_EXT                                 = $2022;
  WGL_STENCIL_BITS_EXT                               = $2023;
  WGL_AUX_BUFFERS_EXT                                = $2024;
  WGL_NO_ACCELERATION_EXT                            = $2025;
  WGL_GENERIC_ACCELERATION_EXT                       = $2026;
  WGL_FULL_ACCELERATION_EXT                          = $2027;
  WGL_SWAP_EXCHANGE_EXT                              = $2028;
  WGL_SWAP_COPY_EXT                                  = $2029;
  WGL_SWAP_UNDEFINED_EXT                             = $202A;
  WGL_TYPE_RGBA_EXT                                  = $202B;
  WGL_TYPE_COLORINDEX_EXT                            = $202C;

  // WGL_I3D_digital_video_control
  WGL_DIGITAL_VIDEO_CURSOR_ALPHA_FRAMEBUFFER_I3D     = $2050;
  WGL_DIGITAL_VIDEO_CURSOR_ALPHA_VALUE_I3D           = $2051;
  WGL_DIGITAL_VIDEO_CURSOR_INCLUDED_I3D              = $2052;
  WGL_DIGITAL_VIDEO_GAMMA_CORRECTED_I3D              = $2053;

  // WGL_I3D_gamma
  WGL_GAMMA_TABLE_SIZE_I3D                           = $204E;
  WGL_GAMMA_EXCLUDE_DESKTOP_I3D                      = $204F;

  // WGL_I3D_genlock
  WGL_GENLOCK_SOURCE_MULTIVIEW_I3D                   = $2044;
  WGL_GENLOCK_SOURCE_EXTENAL_SYNC_I3D                = $2045;
  WGL_GENLOCK_SOURCE_EXTENAL_FIELD_I3D               = $2046;
  WGL_GENLOCK_SOURCE_EXTENAL_TTL_I3D                 = $2047;
  WGL_GENLOCK_SOURCE_DIGITAL_SYNC_I3D                = $2048;
  WGL_GENLOCK_SOURCE_DIGITAL_FIELD_I3D               = $2049;
  WGL_GENLOCK_SOURCE_EDGE_FALLING_I3D                = $204A;
  WGL_GENLOCK_SOURCE_EDGE_RISING_I3D                 = $204B;
  WGL_GENLOCK_SOURCE_EDGE_BOTH_I3D                   = $204C;

  // WGL_I3D_image_buffer
  WGL_IMAGE_BUFFER_MIN_ACCESS_I3D                    = $00000001;
  WGL_IMAGE_BUFFER_LOCK_I3D                          = $00000002;

  // WGL_NV_float_buffer
  WGL_FLOAT_COMPONENTS_NV                            = $20B0;
  WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_R_NV           = $20B1;
  WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_RG_NV          = $20B2;
  WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_RGB_NV         = $20B3;
  WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_RGBA_NV        = $20B4;
  WGL_TEXTURE_FLOAT_R_NV                             = $20B5;
  WGL_TEXTURE_FLOAT_RG_NV                            = $20B6;
  WGL_TEXTURE_FLOAT_RGB_NV                           = $20B7;
  WGL_TEXTURE_FLOAT_RGBA_NV                          = $20B8;

  // WGL_NV_render_depth_texture
  WGL_BIND_TO_TEXTURE_DEPTH_NV                       = $20A3;
  WGL_BIND_TO_TEXTURE_RECTANGLE_DEPTH_NV             = $20A4;
  WGL_DEPTH_TEXTURE_FORMAT_NV                        = $20A5;
  WGL_TEXTURE_DEPTH_COMPONENT_NV                     = $20A6;
  WGL_DEPTH_COMPONENT_NV                             = $20A7;

  // WGL_NV_render_texture_rectangle
  WGL_BIND_TO_TEXTURE_RECTANGLE_RGB_NV               = $20A0;
  WGL_BIND_TO_TEXTURE_RECTANGLE_RGBA_NV              = $20A1;
  WGL_TEXTURE_RECTANGLE_NV                           = $20A2;

  // WIN_draw_range_elements
  GL_MAX_ELEMENTS_VERTICES_WIN                       = $80E8;
  GL_MAX_ELEMENTS_INDICES_WIN                        = $80E9;

  // GLU
  GLU_INVALID_ENUM                                  = 100900;
  GLU_INVALID_VALUE                                 = 100901;
  GLU_OUT_OF_MEMORY                                 = 100902;
  GLU_INCOMPATIBLE_GL_VERSION                       = 100903;
  GLU_VERSION                                       = 100800;
  GLU_EXTENSIONS                                    = 100801;
  GLU_TRUE                                          = GL_TRUE;
  GLU_FALSE                                         = GL_FALSE;
  GLU_SMOOTH                                        = 100000;
  GLU_FLAT                                          = 100001;
  GLU_NONE                                          = 100002;
  GLU_POINT                                         = 100010;
  GLU_LINE                                          = 100011;
  GLU_FILL                                          = 100012;
  GLU_SILHOUETTE                                    = 100013;
  GLU_OUTSIDE                                       = 100020;
  GLU_INSIDE                                        = 100021;
  GLU_TESS_MAX_COORD                                = 1.0e150;
  GLU_TESS_WINDING_RULE                             = 100140;
  GLU_TESS_BOUNDARY_ONLY                            = 100141;
  GLU_TESS_TOLERANCE                                = 100142;
  GLU_TESS_WINDING_ODD                              = 100130;
  GLU_TESS_WINDING_NONZERO                          = 100131;
  GLU_TESS_WINDING_POSITIVE                         = 100132;
  GLU_TESS_WINDING_NEGATIVE                         = 100133;
  GLU_TESS_WINDING_ABS_GEQ_TWO                      = 100134;
  GLU_TESS_BEGIN                                    = 100100; // TGLUTessBeginProc
  GLU_TESS_VERTEX                                   = 100101; // TGLUTessVertexProc
  GLU_TESS_END                                      = 100102; // TGLUTessEndProc
  GLU_TESS_ERROR                                    = 100103; // TGLUTessErrorProc
  GLU_TESS_EDGE_FLAG                                = 100104; // TGLUTessEdgeFlagProc
  GLU_TESS_COMBINE                                  = 100105; // TGLUTessCombineProc
  GLU_TESS_BEGIN_DATA                               = 100106; // TGLUTessBeginDataProc
  GLU_TESS_VERTEX_DATA                              = 100107; // TGLUTessVertexDataProc
  GLU_TESS_END_DATA                                 = 100108; // TGLUTessEndDataProc
  GLU_TESS_ERROR_DATA                               = 100109; // TGLUTessErrorDataProc
  GLU_TESS_EDGE_FLAG_DATA                           = 100110; // TGLUTessEdgeFlagDataProc
  GLU_TESS_COMBINE_DATA                             = 100111; // TGLUTessCombineDataProc
  GLU_TESS_ERROR1                                   = 100151;
  GLU_TESS_ERROR2                                   = 100152;
  GLU_TESS_ERROR3                                   = 100153;
  GLU_TESS_ERROR4                                   = 100154;
  GLU_TESS_ERROR5                                   = 100155;
  GLU_TESS_ERROR6                                   = 100156;
  GLU_TESS_ERROR7                                   = 100157;
  GLU_TESS_ERROR8                                   = 100158;
  GLU_TESS_MISSING_BEGIN_POLYGON                    = GLU_TESS_ERROR1;
  GLU_TESS_MISSING_BEGIN_CONTOUR                    = GLU_TESS_ERROR2;
  GLU_TESS_MISSING_END_POLYGON                      = GLU_TESS_ERROR3;
  GLU_TESS_MISSING_END_CONTOUR                      = GLU_TESS_ERROR4;
  GLU_TESS_COORD_TOO_LARGE                          = GLU_TESS_ERROR5;
  GLU_TESS_NEED_COMBINE_CALLBACK                    = GLU_TESS_ERROR6;
  GLU_AUTO_LOAD_MATRIX                              = 100200;
  GLU_CULLING                                       = 100201;
  GLU_SAMPLING_TOLERANCE                            = 100203;
  GLU_DISPLAY_MODE                                  = 100204;
  GLU_PARAMETRIC_TOLERANCE                          = 100202;
  GLU_SAMPLING_METHOD                               = 100205;
  GLU_U_STEP                                        = 100206;
  GLU_V_STEP                                        = 100207;
  GLU_PATH_LENGTH                                   = 100215;
  GLU_PARAMETRIC_ERROR                              = 100216;
  GLU_DOMAIN_DISTANCE                               = 100217;
  GLU_MAP1_TRIM_2                                   = 100210;
  GLU_MAP1_TRIM_3                                   = 100211;
  GLU_OUTLINE_POLYGON                               = 100240;
  GLU_OUTLINE_PATCH                                 = 100241;
  GLU_NURBS_ERROR1                                  = 100251;
  GLU_NURBS_ERROR2                                  = 100252;
  GLU_NURBS_ERROR3                                  = 100253;
  GLU_NURBS_ERROR4                                  = 100254;
  GLU_NURBS_ERROR5                                  = 100255;
  GLU_NURBS_ERROR6                                  = 100256;
  GLU_NURBS_ERROR7                                  = 100257;
  GLU_NURBS_ERROR8                                  = 100258;
  GLU_NURBS_ERROR9                                  = 100259;
  GLU_NURBS_ERROR10                                 = 100260;
  GLU_NURBS_ERROR11                                 = 100261;
  GLU_NURBS_ERROR12                                 = 100262;
  GLU_NURBS_ERROR13                                 = 100263;
  GLU_NURBS_ERROR14                                 = 100264;
  GLU_NURBS_ERROR15                                 = 100265;
  GLU_NURBS_ERROR16                                 = 100266;
  GLU_NURBS_ERROR17                                 = 100267;
  GLU_NURBS_ERROR18                                 = 100268;
  GLU_NURBS_ERROR19                                 = 100269;
  GLU_NURBS_ERROR20                                 = 100270;
  GLU_NURBS_ERROR21                                 = 100271;
  GLU_NURBS_ERROR22                                 = 100272;
  GLU_NURBS_ERROR23                                 = 100273;
  GLU_NURBS_ERROR24                                 = 100274;
  GLU_NURBS_ERROR25                                 = 100275;
  GLU_NURBS_ERROR26                                 = 100276;
  GLU_NURBS_ERROR27                                 = 100277;
  GLU_NURBS_ERROR28                                 = 100278;
  GLU_NURBS_ERROR29                                 = 100279;
  GLU_NURBS_ERROR30                                 = 100280;
  GLU_NURBS_ERROR31                                 = 100281;
  GLU_NURBS_ERROR32                                 = 100282;
  GLU_NURBS_ERROR33                                 = 100283;
  GLU_NURBS_ERROR34                                 = 100284;
  GLU_NURBS_ERROR35                                 = 100285;
  GLU_NURBS_ERROR36                                 = 100286;
  GLU_NURBS_ERROR37                                 = 100287;
  GLU_CW                                            = 100120;
  GLU_CCW                                           = 100121;
  GLU_INTERIOR                                      = 100122;
  GLU_EXTERIOR                                      = 100123;
  GLU_UNKNOWN                                       = 100124;
  GLU_BEGIN                                         = GLU_TESS_BEGIN;
  GLU_VERTEX                                        = GLU_TESS_VERTEX;
  GLU_END                                           = GLU_TESS_END;
  GLU_ERROR                                         = GLU_TESS_ERROR;
  GLU_EDGE_FLAG                                     = GLU_TESS_EDGE_FLAG;

var
  // GL_VERSION_1_1
  glAccum: procedure(op: TGLenum; value: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glAlphaFunc: procedure(func: TGLenum; ref: TGLclampf); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glAreTexturesResident: function(n: TGLsizei; const textures: PGLuint; residences: PGLboolean): TGLboolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glArrayElement: procedure(i: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glBegin: procedure(mode: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glBindTexture: procedure(target: TGLenum; texture: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glBitmap: procedure(width: TGLsizei; height: TGLsizei; xorig: TGLfloat; yorig: TGLfloat; xmove: TGLfloat; ymove: TGLfloat; const bitmap: PGLubyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glBlendFunc: procedure(sfactor: TGLenum; dfactor: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glCallList: procedure(list: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glCallLists: procedure(n: TGLsizei; _type: TGLenum; const lists: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glClear: procedure(mask: TGLbitfield); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glClearAccum: procedure(red: TGLfloat; green: TGLfloat; blue: TGLfloat; alpha: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glClearColor: procedure(red: TGLclampf; green: TGLclampf; blue: TGLclampf; alpha: TGLclampf); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glClearDepth: procedure(depth: TGLclampd); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glClearIndex: procedure(c: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glClearStencil: procedure(s: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glClipPlane: procedure(plane: TGLenum; const equation: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor3b: procedure(red: TGLbyte; green: TGLbyte; blue: TGLbyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor3bv: procedure(const v: PGLbyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor3d: procedure(red: TGLdouble; green: TGLdouble; blue: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor3dv: procedure(const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor3f: procedure(red: TGLfloat; green: TGLfloat; blue: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor3fv: procedure(const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor3i: procedure(red: TGLint; green: TGLint; blue: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor3iv: procedure(const v: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor3s: procedure(red: TGLshort; green: TGLshort; blue: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor3sv: procedure(const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor3ub: procedure(red: TGLubyte; green: TGLubyte; blue: TGLubyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor3ubv: procedure(const v: PGLubyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor3ui: procedure(red: TGLuint; green: TGLuint; blue: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor3uiv: procedure(const v: PGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor3us: procedure(red: TGLushort; green: TGLushort; blue: TGLushort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor3usv: procedure(const v: PGLushort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor4b: procedure(red: TGLbyte; green: TGLbyte; blue: TGLbyte; alpha: TGLbyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor4bv: procedure(const v: PGLbyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor4d: procedure(red: TGLdouble; green: TGLdouble; blue: TGLdouble; alpha: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor4dv: procedure(const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor4f: procedure(red: TGLfloat; green: TGLfloat; blue: TGLfloat; alpha: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor4fv: procedure(const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor4i: procedure(red: TGLint; green: TGLint; blue: TGLint; alpha: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor4iv: procedure(const v: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor4s: procedure(red: TGLshort; green: TGLshort; blue: TGLshort; alpha: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor4sv: procedure(const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor4ub: procedure(red: TGLubyte; green: TGLubyte; blue: TGLubyte; alpha: TGLubyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor4ubv: procedure(const v: PGLubyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor4ui: procedure(red: TGLuint; green: TGLuint; blue: TGLuint; alpha: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor4uiv: procedure(const v: PGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor4us: procedure(red: TGLushort; green: TGLushort; blue: TGLushort; alpha: TGLushort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor4usv: procedure(const v: PGLushort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColorMask: procedure(red: TGLboolean; green: TGLboolean; blue: TGLboolean; alpha: TGLboolean); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColorMaterial: procedure(face: TGLenum; mode: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColorPointer: procedure(size: TGLint; _type: TGLenum; stride: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glCopyPixels: procedure(x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei; _type: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glCopyTexImage1D: procedure(target: TGLenum; level: TGLint; internalFormat: TGLenum; x: TGLint; y: TGLint; width: TGLsizei; border: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glCopyTexImage2D: procedure(target: TGLenum; level: TGLint; internalFormat: TGLenum; x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei; border: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glCopyTexSubImage1D: procedure(target: TGLenum; level: TGLint; xoffset: TGLint; x: TGLint; y: TGLint; width: TGLsizei); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glCopyTexSubImage2D: procedure(target: TGLenum; level: TGLint; xoffset: TGLint; yoffset: TGLint; x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glCullFace: procedure(mode: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glDeleteLists: procedure(list: TGLuint; range: TGLsizei); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glDeleteTextures: procedure(n: TGLsizei; const textures: PGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glDepthFunc: procedure(func: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glDepthMask: procedure(flag: TGLboolean); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glDepthRange: procedure(zNear: TGLclampd; zFar: TGLclampd); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glDisable: procedure(cap: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glDisableClientState: procedure(_array: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glDrawArrays: procedure(mode: TGLenum; first: TGLint; count: TGLsizei); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glDrawBuffer: procedure(mode: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glDrawElements: procedure(mode: TGLenum; count: TGLsizei; _type: TGLenum; const indices: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glDrawPixels: procedure(width: TGLsizei; height: TGLsizei; format: TGLenum; _type: TGLenum; const pixels: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glEdgeFlag: procedure(flag: TGLboolean); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glEdgeFlagPointer: procedure(stride: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glEdgeFlagv: procedure(const flag: PGLboolean); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glEnable: procedure(cap: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glEnableClientState: procedure(_array: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glEnd: procedure(); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glEndList: procedure(); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glEvalCoord1d: procedure(u: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glEvalCoord1dv: procedure(const u: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glEvalCoord1f: procedure(u: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glEvalCoord1fv: procedure(const u: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glEvalCoord2d: procedure(u: TGLdouble; v: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glEvalCoord2dv: procedure(const u: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glEvalCoord2f: procedure(u: TGLfloat; v: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glEvalCoord2fv: procedure(const u: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glEvalMesh1: procedure(mode: TGLenum; i1: TGLint; i2: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glEvalMesh2: procedure(mode: TGLenum; i1: TGLint; i2: TGLint; j1: TGLint; j2: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glEvalPoint1: procedure(i: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glEvalPoint2: procedure(i: TGLint; j: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFeedbackBuffer: procedure(size: TGLsizei; _type: TGLenum; buffer: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFinish: procedure(); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFlush: procedure(); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFogf: procedure(pname: TGLenum; param: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFogfv: procedure(pname: TGLenum; const params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFogi: procedure(pname: TGLenum; param: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFogiv: procedure(pname: TGLenum; const params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFrontFace: procedure(mode: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFrustum: procedure(left: TGLdouble; right: TGLdouble; bottom: TGLdouble; top: TGLdouble; zNear: TGLdouble; zFar: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGenLists: function(range: TGLsizei): TGLuint; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGenTextures: procedure(n: TGLsizei; textures: PGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetBooleanv: procedure(pname: TGLenum; params: PGLboolean); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetClipPlane: procedure(plane: TGLenum; equation: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetDoublev: procedure(pname: TGLenum; params: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetError: function(): TGLenum; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetFloatv: procedure(pname: TGLenum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetIntegerv: procedure(pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetLightfv: procedure(light: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetLightiv: procedure(light: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetMapdv: procedure(target: TGLenum; query: TGLenum; v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetMapfv: procedure(target: TGLenum; query: TGLenum; v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetMapiv: procedure(target: TGLenum; query: TGLenum; v: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetMaterialfv: procedure(face: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetMaterialiv: procedure(face: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetPixelMapfv: procedure(map: TGLenum; values: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetPixelMapuiv: procedure(map: TGLenum; values: PGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetPixelMapusv: procedure(map: TGLenum; values: PGLushort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetPointerv: procedure(pname: TGLenum; params: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetPolygonStipple: procedure(mask: PGLubyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetString: function(name: TGLenum): PChar; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetTexEnvfv: procedure(target: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetTexEnviv: procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetTexGendv: procedure(coord: TGLenum; pname: TGLenum; params: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetTexGenfv: procedure(coord: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetTexGeniv: procedure(coord: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetTexImage: procedure(target: TGLenum; level: TGLint; format: TGLenum; _type: TGLenum; pixels: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetTexLevelParameterfv: procedure(target: TGLenum; level: TGLint; pname: TGLenum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetTexLevelParameteriv: procedure(target: TGLenum; level: TGLint; pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetTexParameterfv: procedure(target: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetTexParameteriv: procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glHint: procedure(target: TGLenum; mode: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glIndexMask: procedure(mask: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glIndexPointer: procedure(_type: TGLenum; stride: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glIndexd: procedure(c: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glIndexdv: procedure(const c: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glIndexf: procedure(c: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glIndexfv: procedure(const c: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glIndexi: procedure(c: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glIndexiv: procedure(const c: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glIndexs: procedure(c: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glIndexsv: procedure(const c: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glIndexub: procedure(c: TGLubyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glIndexubv: procedure(const c: PGLubyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glInitNames: procedure(); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glInterleavedArrays: procedure(format: TGLenum; stride: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glIsEnabled: function(cap: TGLenum): TGLboolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glIsList: function(list: TGLuint): TGLboolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glIsTexture: function(texture: TGLuint): TGLboolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glLightModelf: procedure(pname: TGLenum; param: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glLightModelfv: procedure(pname: TGLenum; const params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glLightModeli: procedure(pname: TGLenum; param: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glLightModeliv: procedure(pname: TGLenum; const params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glLightf: procedure(light: TGLenum; pname: TGLenum; param: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glLightfv: procedure(light: TGLenum; pname: TGLenum; const params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glLighti: procedure(light: TGLenum; pname: TGLenum; param: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glLightiv: procedure(light: TGLenum; pname: TGLenum; const params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glLineStipple: procedure(factor: TGLint; pattern: TGLushort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glLineWidth: procedure(width: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glListBase: procedure(base: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glLoadIdentity: procedure(); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glLoadMatrixd: procedure(const m: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glLoadMatrixf: procedure(const m: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glLoadName: procedure(name: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glLogicOp: procedure(opcode: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMap1d: procedure(target: TGLenum; u1: TGLdouble; u2: TGLdouble; stride: TGLint; order: TGLint; const points: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMap1f: procedure(target: TGLenum; u1: TGLfloat; u2: TGLfloat; stride: TGLint; order: TGLint; const points: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMap2d: procedure(target: TGLenum; u1: TGLdouble; u2: TGLdouble; ustride: TGLint; uorder: TGLint; v1: TGLdouble; v2: TGLdouble; vstride: TGLint; vorder: TGLint; const points: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMap2f: procedure(target: TGLenum; u1: TGLfloat; u2: TGLfloat; ustride: TGLint; uorder: TGLint; v1: TGLfloat; v2: TGLfloat; vstride: TGLint; vorder: TGLint; const points: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMapGrid1d: procedure(un: TGLint; u1: TGLdouble; u2: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMapGrid1f: procedure(un: TGLint; u1: TGLfloat; u2: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMapGrid2d: procedure(un: TGLint; u1: TGLdouble; u2: TGLdouble; vn: TGLint; v1: TGLdouble; v2: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMapGrid2f: procedure(un: TGLint; u1: TGLfloat; u2: TGLfloat; vn: TGLint; v1: TGLfloat; v2: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMaterialf: procedure(face: TGLenum; pname: TGLenum; param: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMaterialfv: procedure(face: TGLenum; pname: TGLenum; const params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMateriali: procedure(face: TGLenum; pname: TGLenum; param: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMaterialiv: procedure(face: TGLenum; pname: TGLenum; const params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMatrixMode: procedure(mode: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultMatrixd: procedure(const m: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultMatrixf: procedure(const m: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glNewList: procedure(list: TGLuint; mode: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glNormal3b: procedure(nx: TGLbyte; ny: TGLbyte; nz: TGLbyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glNormal3bv: procedure(const v: PGLbyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glNormal3d: procedure(nx: TGLdouble; ny: TGLdouble; nz: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glNormal3dv: procedure(const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glNormal3f: procedure(nx: TGLfloat; ny: TGLfloat; nz: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glNormal3fv: procedure(const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glNormal3i: procedure(nx: TGLint; ny: TGLint; nz: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glNormal3iv: procedure(const v: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glNormal3s: procedure(nx: TGLshort; ny: TGLshort; nz: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glNormal3sv: procedure(const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glNormalPointer: procedure(_type: TGLenum; stride: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glOrtho: procedure(left: TGLdouble; right: TGLdouble; bottom: TGLdouble; top: TGLdouble; zNear: TGLdouble; zFar: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPassThrough: procedure(token: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPixelMapfv: procedure(map: TGLenum; mapsize: TGLsizei; const values: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPixelMapuiv: procedure(map: TGLenum; mapsize: TGLsizei; const values: PGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPixelMapusv: procedure(map: TGLenum; mapsize: TGLsizei; const values: PGLushort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPixelStoref: procedure(pname: TGLenum; param: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPixelStorei: procedure(pname: TGLenum; param: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPixelTransferf: procedure(pname: TGLenum; param: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPixelTransferi: procedure(pname: TGLenum; param: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPixelZoom: procedure(xfactor: TGLfloat; yfactor: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPointSize: procedure(size: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPolygonMode: procedure(face: TGLenum; mode: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPolygonOffset: procedure(factor: TGLfloat; units: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPolygonStipple: procedure(const mask: PGLubyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPopAttrib: procedure(); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPopClientAttrib: procedure(); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPopMatrix: procedure(); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPopName: procedure(); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPrioritizeTextures: procedure(n: TGLsizei; const textures: PGLuint; const priorities: PGLclampf); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPushAttrib: procedure(mask: TGLbitfield); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPushClientAttrib: procedure(mask: TGLbitfield); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPushMatrix: procedure(); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPushName: procedure(name: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glRasterPos2d: procedure(x: TGLdouble; y: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glRasterPos2dv: procedure(const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glRasterPos2f: procedure(x: TGLfloat; y: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glRasterPos2fv: procedure(const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glRasterPos2i: procedure(x: TGLint; y: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glRasterPos2iv: procedure(const v: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glRasterPos2s: procedure(x: TGLshort; y: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glRasterPos2sv: procedure(const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glRasterPos3d: procedure(x: TGLdouble; y: TGLdouble; z: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glRasterPos3dv: procedure(const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glRasterPos3f: procedure(x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glRasterPos3fv: procedure(const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glRasterPos3i: procedure(x: TGLint; y: TGLint; z: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glRasterPos3iv: procedure(const v: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glRasterPos3s: procedure(x: TGLshort; y: TGLshort; z: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glRasterPos3sv: procedure(const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glRasterPos4d: procedure(x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glRasterPos4dv: procedure(const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glRasterPos4f: procedure(x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glRasterPos4fv: procedure(const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glRasterPos4i: procedure(x: TGLint; y: TGLint; z: TGLint; w: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glRasterPos4iv: procedure(const v: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glRasterPos4s: procedure(x: TGLshort; y: TGLshort; z: TGLshort; w: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glRasterPos4sv: procedure(const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glReadBuffer: procedure(mode: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glReadPixels: procedure(x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei; format: TGLenum; _type: TGLenum; pixels: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glRectd: procedure(x1: TGLdouble; y1: TGLdouble; x2: TGLdouble; y2: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glRectdv: procedure(const v1: PGLdouble; const v2: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glRectf: procedure(x1: TGLfloat; y1: TGLfloat; x2: TGLfloat; y2: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glRectfv: procedure(const v1: PGLfloat; const v2: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glRecti: procedure(x1: TGLint; y1: TGLint; x2: TGLint; y2: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glRectiv: procedure(const v1: PGLint; const v2: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glRects: procedure(x1: TGLshort; y1: TGLshort; x2: TGLshort; y2: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glRectsv: procedure(const v1: PGLshort; const v2: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glRenderMode: function(mode: TGLenum): TGLint; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glRotated: procedure(angle: TGLdouble; x: TGLdouble; y: TGLdouble; z: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glRotatef: procedure(angle: TGLfloat; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glScaled: procedure(x: TGLdouble; y: TGLdouble; z: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glScalef: procedure(x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glScissor: procedure(x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSelectBuffer: procedure(size: TGLsizei; buffer: PGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glShadeModel: procedure(mode: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glStencilFunc: procedure(func: TGLenum; ref: TGLint; mask: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glStencilMask: procedure(mask: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glStencilOp: procedure(fail: TGLenum; zfail: TGLenum; zpass: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord1d: procedure(s: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord1dv: procedure(const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord1f: procedure(s: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord1fv: procedure(const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord1i: procedure(s: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord1iv: procedure(const v: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord1s: procedure(s: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord1sv: procedure(const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord2d: procedure(s: TGLdouble; t: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord2dv: procedure(const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord2f: procedure(s: TGLfloat; t: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord2fv: procedure(const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord2i: procedure(s: TGLint; t: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord2iv: procedure(const v: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord2s: procedure(s: TGLshort; t: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord2sv: procedure(const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord3d: procedure(s: TGLdouble; t: TGLdouble; r: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord3dv: procedure(const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord3f: procedure(s: TGLfloat; t: TGLfloat; r: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord3fv: procedure(const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord3i: procedure(s: TGLint; t: TGLint; r: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord3iv: procedure(const v: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord3s: procedure(s: TGLshort; t: TGLshort; r: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord3sv: procedure(const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord4d: procedure(s: TGLdouble; t: TGLdouble; r: TGLdouble; q: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord4dv: procedure(const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord4f: procedure(s: TGLfloat; t: TGLfloat; r: TGLfloat; q: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord4fv: procedure(const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord4i: procedure(s: TGLint; t: TGLint; r: TGLint; q: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord4iv: procedure(const v: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord4s: procedure(s: TGLshort; t: TGLshort; r: TGLshort; q: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord4sv: procedure(const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoordPointer: procedure(size: TGLint; _type: TGLenum; stride: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexEnvf: procedure(target: TGLenum; pname: TGLenum; param: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexEnvfv: procedure(target: TGLenum; pname: TGLenum; const params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexEnvi: procedure(target: TGLenum; pname: TGLenum; param: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexEnviv: procedure(target: TGLenum; pname: TGLenum; const params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexGend: procedure(coord: TGLenum; pname: TGLenum; param: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexGendv: procedure(coord: TGLenum; pname: TGLenum; const params: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexGenf: procedure(coord: TGLenum; pname: TGLenum; param: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexGenfv: procedure(coord: TGLenum; pname: TGLenum; const params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexGeni: procedure(coord: TGLenum; pname: TGLenum; param: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexGeniv: procedure(coord: TGLenum; pname: TGLenum; const params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexImage1D: procedure(target: TGLenum; level: TGLint; internalformat: TGLint; width: TGLsizei; border: TGLint; format: TGLenum; _type: TGLenum; const pixels: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexImage2D: procedure(target: TGLenum; level: TGLint; internalformat: TGLint; width: TGLsizei; height: TGLsizei; border: TGLint; format: TGLenum; _type: TGLenum; const pixels: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexParameterf: procedure(target: TGLenum; pname: TGLenum; param: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexParameterfv: procedure(target: TGLenum; pname: TGLenum; const params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexParameteri: procedure(target: TGLenum; pname: TGLenum; param: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexParameteriv: procedure(target: TGLenum; pname: TGLenum; const params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexSubImage1D: procedure(target: TGLenum; level: TGLint; xoffset: TGLint; width: TGLsizei; format: TGLenum; _type: TGLenum; const pixels: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexSubImage2D: procedure(target: TGLenum; level: TGLint; xoffset: TGLint; yoffset: TGLint; width: TGLsizei; height: TGLsizei; format: TGLenum; _type: TGLenum; const pixels: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTranslated: procedure(x: TGLdouble; y: TGLdouble; z: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTranslatef: procedure(x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertex2d: procedure(x: TGLdouble; y: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertex2dv: procedure(const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertex2f: procedure(x: TGLfloat; y: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertex2fv: procedure(const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertex2i: procedure(x: TGLint; y: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertex2iv: procedure(const v: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertex2s: procedure(x: TGLshort; y: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertex2sv: procedure(const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertex3d: procedure(x: TGLdouble; y: TGLdouble; z: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertex3dv: procedure(const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertex3f: procedure(x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertex3fv: procedure(const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertex3i: procedure(x: TGLint; y: TGLint; z: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertex3iv: procedure(const v: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertex3s: procedure(x: TGLshort; y: TGLshort; z: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertex3sv: procedure(const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertex4d: procedure(x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertex4dv: procedure(const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertex4f: procedure(x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertex4fv: procedure(const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertex4i: procedure(x: TGLint; y: TGLint; z: TGLint; w: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertex4iv: procedure(const v: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertex4s: procedure(x: TGLshort; y: TGLshort; z: TGLshort; w: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertex4sv: procedure(const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexPointer: procedure(size: TGLint; _type: TGLenum; stride: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glViewport: procedure(x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_VERSION_1_2
  glBlendColor: procedure(red: TGLclampf; green: TGLclampf; blue: TGLclampf; alpha: TGLclampf); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glBlendEquation: procedure(mode: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glDrawRangeElements: procedure(mode: TGLenum; start: TGLuint; _end: TGLuint; count: TGLsizei; _type: TGLenum; const indices: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColorTable: procedure(target: TGLenum; internalformat: TGLenum; width: TGLsizei; format: TGLenum; _type: TGLenum; const table: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColorTableParameterfv: procedure(target: TGLenum; pname: TGLenum; const params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColorTableParameteriv: procedure(target: TGLenum; pname: TGLenum; const params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glCopyColorTable: procedure(target: TGLenum; internalformat: TGLenum; x: TGLint; y: TGLint; width: TGLsizei); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetColorTable: procedure(target: TGLenum; format: TGLenum; _type: TGLenum; table: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetColorTableParameterfv: procedure(target: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetColorTableParameteriv: procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColorSubTable: procedure(target: TGLenum; start: TGLsizei; count: TGLsizei; format: TGLenum; _type: TGLenum; const data: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glCopyColorSubTable: procedure(target: TGLenum; start: TGLsizei; x: TGLint; y: TGLint; width: TGLsizei); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glConvolutionFilter1D: procedure(target: TGLenum; internalformat: TGLenum; width: TGLsizei; format: TGLenum; _type: TGLenum; const image: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glConvolutionFilter2D: procedure(target: TGLenum; internalformat: TGLenum; width: TGLsizei; height: TGLsizei; format: TGLenum; _type: TGLenum; const image: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glConvolutionParameterf: procedure(target: TGLenum; pname: TGLenum; params: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glConvolutionParameterfv: procedure(target: TGLenum; pname: TGLenum; const params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glConvolutionParameteri: procedure(target: TGLenum; pname: TGLenum; params: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glConvolutionParameteriv: procedure(target: TGLenum; pname: TGLenum; const params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glCopyConvolutionFilter1D: procedure(target: TGLenum; internalformat: TGLenum; x: TGLint; y: TGLint; width: TGLsizei); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glCopyConvolutionFilter2D: procedure(target: TGLenum; internalformat: TGLenum; x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetConvolutionFilter: procedure(target: TGLenum; format: TGLenum; _type: TGLenum; image: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetConvolutionParameterfv: procedure(target: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetConvolutionParameteriv: procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetSeparableFilter: procedure(target: TGLenum; format: TGLenum; _type: TGLenum; row: PGLvoid; column: PGLvoid; span: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSeparableFilter2D: procedure(target: TGLenum; internalformat: TGLenum; width: TGLsizei; height: TGLsizei; format: TGLenum; _type: TGLenum; const row: PGLvoid; const column: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetHistogram: procedure(target: TGLenum; reset: TGLboolean; format: TGLenum; _type: TGLenum; values: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetHistogramParameterfv: procedure(target: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetHistogramParameteriv: procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetMinmax: procedure(target: TGLenum; reset: TGLboolean; format: TGLenum; _type: TGLenum; values: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetMinmaxParameterfv: procedure(target: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetMinmaxParameteriv: procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glHistogram: procedure(target: TGLenum; width: TGLsizei; internalformat: TGLenum; sink: TGLboolean); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMinmax: procedure(target: TGLenum; internalformat: TGLenum; sink: TGLboolean); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glResetHistogram: procedure(target: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glResetMinmax: procedure(target: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexImage3D: procedure(target: TGLenum; level: TGLint; internalformat: TGLint; width: TGLsizei; height: TGLsizei; depth: TGLsizei; border: TGLint; format: TGLenum; _type: TGLenum; const pixels: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexSubImage3D: procedure(target: TGLenum; level: TGLint; xoffset: TGLint; yoffset: TGLint; zoffset: TGLint; width: TGLsizei; height: TGLsizei; depth: TGLsizei; format: TGLenum; _type: TGLenum; const pixels: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glCopyTexSubImage3D: procedure(target: TGLenum; level: TGLint; xoffset: TGLint; yoffset: TGLint; zoffset: TGLint; x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_VERSION_1_3
  glActiveTexture: procedure(texture: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glClientActiveTexture: procedure(texture: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord1d: procedure(target: TGLenum; s: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord1dv: procedure(target: TGLenum; const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord1f: procedure(target: TGLenum; s: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord1fv: procedure(target: TGLenum; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord1i: procedure(target: TGLenum; s: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord1iv: procedure(target: TGLenum; const v: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord1s: procedure(target: TGLenum; s: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord1sv: procedure(target: TGLenum; const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord2d: procedure(target: TGLenum; s: TGLdouble; t: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord2dv: procedure(target: TGLenum; const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord2f: procedure(target: TGLenum; s: TGLfloat; t: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord2fv: procedure(target: TGLenum; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord2i: procedure(target: TGLenum; s: TGLint; t: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord2iv: procedure(target: TGLenum; const v: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord2s: procedure(target: TGLenum; s: TGLshort; t: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord2sv: procedure(target: TGLenum; const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord3d: procedure(target: TGLenum; s: TGLdouble; t: TGLdouble; r: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord3dv: procedure(target: TGLenum; const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord3f: procedure(target: TGLenum; s: TGLfloat; t: TGLfloat; r: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord3fv: procedure(target: TGLenum; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord3i: procedure(target: TGLenum; s: TGLint; t: TGLint; r: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord3iv: procedure(target: TGLenum; const v: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord3s: procedure(target: TGLenum; s: TGLshort; t: TGLshort; r: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord3sv: procedure(target: TGLenum; const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord4d: procedure(target: TGLenum; s: TGLdouble; t: TGLdouble; r: TGLdouble; q: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord4dv: procedure(target: TGLenum; const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord4f: procedure(target: TGLenum; s: TGLfloat; t: TGLfloat; r: TGLfloat; q: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord4fv: procedure(target: TGLenum; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord4i: procedure(target: TGLenum; s: TGLint; t: TGLint; r: TGLint; q: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord4iv: procedure(target: TGLenum; const v: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord4s: procedure(target: TGLenum; s: TGLshort; t: TGLshort; r: TGLshort; q: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord4sv: procedure(target: TGLenum; const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glLoadTransposeMatrixf: procedure(const m: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glLoadTransposeMatrixd: procedure(const m: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultTransposeMatrixf: procedure(const m: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultTransposeMatrixd: procedure(const m: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSampleCoverage: procedure(value: TGLclampf; invert: TGLboolean); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glCompressedTexImage3D: procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; width: TGLsizei; height: TGLsizei; depth: TGLsizei; border: TGLint; imageSize: TGLsizei; const data: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glCompressedTexImage2D: procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; width: TGLsizei; height: TGLsizei; border: TGLint; imageSize: TGLsizei; const data: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glCompressedTexImage1D: procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; width: TGLsizei; border: TGLint; imageSize: TGLsizei; const data: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glCompressedTexSubImage3D: procedure(target: TGLenum; level: TGLint; xoffset: TGLint; yoffset: TGLint; zoffset: TGLint; width: TGLsizei; height: TGLsizei; depth: TGLsizei; format: TGLenum; imageSize: TGLsizei; const data: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glCompressedTexSubImage2D: procedure(target: TGLenum; level: TGLint; xoffset: TGLint; yoffset: TGLint; width: TGLsizei; height: TGLsizei; format: TGLenum; imageSize: TGLsizei; const data: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glCompressedTexSubImage1D: procedure(target: TGLenum; level: TGLint; xoffset: TGLint; width: TGLsizei; format: TGLenum; imageSize: TGLsizei; const data: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetCompressedTexImage: procedure(target: TGLenum; level: TGLint; img: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_VERSION_1_4
  glBlendFuncSeparate: procedure(sfactorRGB: TGLenum; dfactorRGB: TGLenum; sfactorAlpha: TGLenum; dfactorAlpha: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFogCoordf: procedure(coord: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFogCoordfv: procedure(const coord: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFogCoordd: procedure(coord: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFogCoorddv: procedure(const coord: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFogCoordPointer: procedure(_type: TGLenum; stride: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiDrawArrays: procedure(mode: TGLenum; first: PGLint; count: PGLsizei; primcount: TGLsizei); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiDrawElements: procedure(mode: TGLenum; const count: PGLsizei; _type: TGLenum; const indices: PGLvoid; primcount: TGLsizei); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPointParameterf: procedure(pname: TGLenum; param: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPointParameterfv: procedure(pname: TGLenum; const params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPointParameteri: procedure(pname: TGLenum; param: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPointParameteriv: procedure(pname: TGLenum; const params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSecondaryColor3b: procedure(red: TGLbyte; green: TGLbyte; blue: TGLbyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSecondaryColor3bv: procedure(const v: PGLbyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSecondaryColor3d: procedure(red: TGLdouble; green: TGLdouble; blue: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSecondaryColor3dv: procedure(const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSecondaryColor3f: procedure(red: TGLfloat; green: TGLfloat; blue: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSecondaryColor3fv: procedure(const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSecondaryColor3i: procedure(red: TGLint; green: TGLint; blue: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSecondaryColor3iv: procedure(const v: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSecondaryColor3s: procedure(red: TGLshort; green: TGLshort; blue: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSecondaryColor3sv: procedure(const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSecondaryColor3ub: procedure(red: TGLubyte; green: TGLubyte; blue: TGLubyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSecondaryColor3ubv: procedure(const v: PGLubyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSecondaryColor3ui: procedure(red: TGLuint; green: TGLuint; blue: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSecondaryColor3uiv: procedure(const v: PGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSecondaryColor3us: procedure(red: TGLushort; green: TGLushort; blue: TGLushort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSecondaryColor3usv: procedure(const v: PGLushort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSecondaryColorPointer: procedure(size: TGLint; _type: TGLenum; stride: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos2d: procedure(x: TGLdouble; y: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos2dv: procedure(const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos2f: procedure(x: TGLfloat; y: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos2fv: procedure(const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos2i: procedure(x: TGLint; y: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos2iv: procedure(const v: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos2s: procedure(x: TGLshort; y: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos2sv: procedure(const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos3d: procedure(x: TGLdouble; y: TGLdouble; z: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos3dv: procedure(const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos3f: procedure(x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos3fv: procedure(const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos3i: procedure(x: TGLint; y: TGLint; z: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos3iv: procedure(const v: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos3s: procedure(x: TGLshort; y: TGLshort; z: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos3sv: procedure(const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_3DFX_tbuffer
  glTbufferMask3DFX: procedure(mask: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_APPLE_element_array
  glElementPointerAPPLE: procedure(_type: TGLenum; const _pointer: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glDrawElementArrayAPPLE: procedure(mode: TGLenum; first: TGLint; count: TGLsizei); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glDrawRangeElementArrayAPPLE: procedure(mode: TGLenum; start: TGLuint; _end: TGLuint; first: TGLint; count: TGLsizei); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiDrawElementArrayAPPLE: procedure(mode: TGLenum; const first: PGLint; const count: PGLsizei; primcount: TGLsizei); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiDrawRangeElementArrayAPPLE: procedure(mode: TGLenum; start: TGLuint; _end: TGLuint; const first: PGLint; const count: PGLsizei; primcount: TGLsizei); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_APPLE_fence
  glGenFencesAPPLE: procedure(n: TGLsizei; fences: PGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glDeleteFencesAPPLE: procedure(n: TGLsizei; const fences: PGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSetFenceAPPLE: procedure(fence: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glIsFenceAPPLE: function(fence: TGLuint): TGLboolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTestFenceAPPLE: function(fence: TGLuint): TGLboolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFinishFenceAPPLE: procedure(fence: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTestObjectAPPLE: function(_object: TGLenum; name: TGLuint): TGLboolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFinishObjectAPPLE: procedure(_object: TGLenum; name: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_APPLE_vertex_array_object
  glBindVertexArrayAPPLE: procedure(_array: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glDeleteVertexArraysAPPLE: procedure(n: TGLsizei; const arrays: PGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGenVertexArraysAPPLE: procedure(n: TGLsizei; const arrays: PGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glIsVertexArrayAPPLE: function(_array: TGLuint): TGLboolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_APPLE_vertex_array_range
  glVertexArrayRangeAPPLE: procedure(length: TGLsizei; _pointer: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFlushVertexArrayRangeAPPLE: procedure(length: TGLsizei; _pointer: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexArrayParameteriAPPLE: procedure(pname: TGLenum; param: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_ARB_matrix_palette
  glCurrentPaletteMatrixARB: procedure(index: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMatrixIndexubvARB: procedure(size: TGLint; const indices: PGLubyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMatrixIndexusvARB: procedure(size: TGLint; const indices: PGLushort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMatrixIndexuivARB: procedure(size: TGLint; const indices: PGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMatrixIndexPointerARB: procedure(size: TGLint; _type: TGLenum; stride: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_ARB_multisample
  glSampleCoverageARB: procedure(value: TGLclampf; invert: TGLboolean); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_ARB_multitexture
  glActiveTextureARB: procedure(texture: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glClientActiveTextureARB: procedure(texture: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord1dARB: procedure(target: TGLenum; s: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord1dvARB: procedure(target: TGLenum; const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord1fARB: procedure(target: TGLenum; s: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord1fvARB: procedure(target: TGLenum; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord1iARB: procedure(target: TGLenum; s: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord1ivARB: procedure(target: TGLenum; const v: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord1sARB: procedure(target: TGLenum; s: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord1svARB: procedure(target: TGLenum; const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord2dARB: procedure(target: TGLenum; s: TGLdouble; t: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord2dvARB: procedure(target: TGLenum; const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord2fARB: procedure(target: TGLenum; s: TGLfloat; t: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord2fvARB: procedure(target: TGLenum; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord2iARB: procedure(target: TGLenum; s: TGLint; t: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord2ivARB: procedure(target: TGLenum; const v: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord2sARB: procedure(target: TGLenum; s: TGLshort; t: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord2svARB: procedure(target: TGLenum; const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord3dARB: procedure(target: TGLenum; s: TGLdouble; t: TGLdouble; r: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord3dvARB: procedure(target: TGLenum; const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord3fARB: procedure(target: TGLenum; s: TGLfloat; t: TGLfloat; r: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord3fvARB: procedure(target: TGLenum; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord3iARB: procedure(target: TGLenum; s: TGLint; t: TGLint; r: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord3ivARB: procedure(target: TGLenum; const v: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord3sARB: procedure(target: TGLenum; s: TGLshort; t: TGLshort; r: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord3svARB: procedure(target: TGLenum; const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord4dARB: procedure(target: TGLenum; s: TGLdouble; t: TGLdouble; r: TGLdouble; q: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord4dvARB: procedure(target: TGLenum; const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord4fARB: procedure(target: TGLenum; s: TGLfloat; t: TGLfloat; r: TGLfloat; q: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord4fvARB: procedure(target: TGLenum; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord4iARB: procedure(target: TGLenum; s: TGLint; t: TGLint; r: TGLint; q: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord4ivARB: procedure(target: TGLenum; const v: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord4sARB: procedure(target: TGLenum; s: TGLshort; t: TGLshort; r: TGLshort; q: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord4svARB: procedure(target: TGLenum; const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_ARB_point_parameters
  glPointParameterfARB: procedure(pname: TGLenum; param: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPointParameterfvARB: procedure(pname: TGLenum; const params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_ARB_texture_compression
  glCompressedTexImage3DARB: procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; width: TGLsizei; height: TGLsizei; depth: TGLsizei; border: TGLint; imageSize: TGLsizei; const data: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glCompressedTexImage2DARB: procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; width: TGLsizei; height: TGLsizei; border: TGLint; imageSize: TGLsizei; const data: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glCompressedTexImage1DARB: procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; width: TGLsizei; border: TGLint; imageSize: TGLsizei; const data: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glCompressedTexSubImage3DARB: procedure(target: TGLenum; level: TGLint; xoffset: TGLint; yoffset: TGLint; zoffset: TGLint; width: TGLsizei; height: TGLsizei; depth: TGLsizei; format: TGLenum; imageSize: TGLsizei; const data: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glCompressedTexSubImage2DARB: procedure(target: TGLenum; level: TGLint; xoffset: TGLint; yoffset: TGLint; width: TGLsizei; height: TGLsizei; format: TGLenum; imageSize: TGLsizei; const data: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glCompressedTexSubImage1DARB: procedure(target: TGLenum; level: TGLint; xoffset: TGLint; width: TGLsizei; format: TGLenum; imageSize: TGLsizei; const data: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetCompressedTexImageARB: procedure(target: TGLenum; level: TGLint; img: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_ARB_transpose_matrix
  glLoadTransposeMatrixfARB: procedure(const m: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glLoadTransposeMatrixdARB: procedure(const m: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultTransposeMatrixfARB: procedure(const m: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultTransposeMatrixdARB: procedure(const m: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_ARB_vertex_blend
  glWeightbvARB: procedure(size: TGLint; const weights: PGLbyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWeightsvARB: procedure(size: TGLint; const weights: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWeightivARB: procedure(size: TGLint; const weights: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWeightfvARB: procedure(size: TGLint; const weights: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWeightdvARB: procedure(size: TGLint; const weights: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWeightubvARB: procedure(size: TGLint; const weights: PGLubyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWeightusvARB: procedure(size: TGLint; const weights: PGLushort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWeightuivARB: procedure(size: TGLint; const weights: PGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWeightPointerARB: procedure(size: TGLint; _type: TGLenum; stride: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexBlendARB: procedure(count: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_ARB_vertex_buffer_object
  glBindBufferARB: procedure(target: TGLenum; buffer: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glDeleteBuffersARB: procedure(n: TGLsizei; const buffers: PGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGenBuffersARB: procedure(n: TGLsizei; buffers: PGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glIsBufferARB: function(buffer: TGLuint): TGLboolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glBufferDataARB: procedure(target: TGLenum; size: TGLsizei; const data: PGLvoid; usage: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glBufferSubDataARB: procedure(target: TGLenum; offset: TGLsizei; size: TGLsizei; const data: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetBufferSubDataARB: procedure(target: TGLenum; offset: TGLsizei; size: TGLsizei; data: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMapBufferARB: function(target: TGLenum; access: TGLenum): PGLvoid; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glUnmapBufferARB: function(target: TGLenum): TGLboolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetBufferParameterivARB: procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetBufferPointervARB: procedure(target: TGLenum; pname: TGLenum; params: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // ARB less version fo GL 1.5
  glBindBuffer: procedure(target: TGLenum; buffer: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glDeleteBuffers: procedure(n: TGLsizei; const buffers: PGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGenBuffers: procedure(n: TGLsizei; buffers: PGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glIsBuffer: function(buffer: TGLuint): TGLboolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glBufferData: procedure(target: TGLenum; size: TGLsizei; const data: PGLvoid; usage: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glBufferSubData: procedure(target: TGLenum; offset: TGLsizei; size: TGLsizei; const data: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetBufferSubData: procedure(target: TGLenum; offset: TGLsizei; size: TGLsizei; data: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMapBuffer: function(target: TGLenum; access: TGLenum): PGLvoid; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glUnmapBuffer: function(target: TGLenum): TGLboolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetBufferParameteriv: procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetBufferPointerv: procedure(target: TGLenum; pname: TGLenum; params: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_ARB_vertex_program
  glVertexAttrib1dARB: procedure(index: TGLuint; x: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib1dvARB: procedure(index: TGLuint; const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib1fARB: procedure(index: TGLuint; x: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib1fvARB: procedure(index: TGLuint; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib1sARB: procedure(index: TGLuint; x: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib1svARB: procedure(index: TGLuint; const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib2dARB: procedure(index: TGLuint; x: TGLdouble; y: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib2dvARB: procedure(index: TGLuint; const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib2fARB: procedure(index: TGLuint; x: TGLfloat; y: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib2fvARB: procedure(index: TGLuint; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib2sARB: procedure(index: TGLuint; x: TGLshort; y: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib2svARB: procedure(index: TGLuint; const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib3dARB: procedure(index: TGLuint; x: TGLdouble; y: TGLdouble; z: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib3dvARB: procedure(index: TGLuint; const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib3fARB: procedure(index: TGLuint; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib3fvARB: procedure(index: TGLuint; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib3sARB: procedure(index: TGLuint; x: TGLshort; y: TGLshort; z: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib3svARB: procedure(index: TGLuint; const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib4NbvARB: procedure(index: TGLuint; const v: PGLbyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib4NivARB: procedure(index: TGLuint; const v: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib4NsvARB: procedure(index: TGLuint; const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib4NubARB: procedure(index: TGLuint; x: TGLubyte; y: TGLubyte; z: TGLubyte; w: TGLubyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib4NubvARB: procedure(index: TGLuint; const v: PGLubyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib4NuivARB: procedure(index: TGLuint; const v: PGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib4NusvARB: procedure(index: TGLuint; const v: PGLushort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib4bvARB: procedure(index: TGLuint; const v: PGLbyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib4dARB: procedure(index: TGLuint; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib4dvARB: procedure(index: TGLuint; const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib4fARB: procedure(index: TGLuint; x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib4fvARB: procedure(index: TGLuint; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib4ivARB: procedure(index: TGLuint; const v: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib4sARB: procedure(index: TGLuint; x: TGLshort; y: TGLshort; z: TGLshort; w: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib4svARB: procedure(index: TGLuint; const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib4ubvARB: procedure(index: TGLuint; const v: PGLubyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib4uivARB: procedure(index: TGLuint; const v: PGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib4usvARB: procedure(index: TGLuint; const v: PGLushort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttribPointerARB: procedure(index: TGLuint; size: TGLint; _type: TGLenum; normalized: TGLboolean; stride: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glEnableVertexAttribArrayARB: procedure(index: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glDisableVertexAttribArrayARB: procedure(index: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glProgramStringARB: procedure(target: TGLenum; format: TGLenum; len: TGLsizei; const _string: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glBindProgramARB: procedure(target: TGLenum; _program: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glDeleteProgramsARB: procedure(n: TGLsizei; const programs: PGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGenProgramsARB: procedure(n: TGLsizei; programs: PGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glProgramEnvParameter4dARB: procedure(target: TGLenum; index: TGLuint; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glProgramEnvParameter4dvARB: procedure(target: TGLenum; index: TGLuint; const params: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glProgramEnvParameter4fARB: procedure(target: TGLenum; index: TGLuint; x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glProgramEnvParameter4fvARB: procedure(target: TGLenum; index: TGLuint; const params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glProgramLocalParameter4dARB: procedure(target: TGLenum; index: TGLuint; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glProgramLocalParameter4dvARB: procedure(target: TGLenum; index: TGLuint; const params: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glProgramLocalParameter4fARB: procedure(target: TGLenum; index: TGLuint; x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glProgramLocalParameter4fvARB: procedure(target: TGLenum; index: TGLuint; const params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetProgramEnvParameterdvARB: procedure(target: TGLenum; index: TGLuint; params: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetProgramEnvParameterfvARB: procedure(target: TGLenum; index: TGLuint; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetProgramLocalParameterdvARB: procedure(target: TGLenum; index: TGLuint; params: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetProgramLocalParameterfvARB: procedure(target: TGLenum; index: TGLuint; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetProgramivARB: procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetProgramStringARB: procedure(target: TGLenum; pname: TGLenum; _string: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetVertexAttribdvARB: procedure(index: TGLuint; pname: TGLenum; params: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetVertexAttribfvARB: procedure(index: TGLuint; pname: TGLenum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetVertexAttribivARB: procedure(index: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetVertexAttribPointervARB: procedure(index: TGLuint; pname: TGLenum; _pointer: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glIsProgramARB: function(_program: TGLuint): TGLboolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_ARB_window_pos
  glWindowPos2dARB: procedure(x: TGLdouble; y: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos2dvARB: procedure(const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos2fARB: procedure(x: TGLfloat; y: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos2fvARB: procedure(const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos2iARB: procedure(x: TGLint; y: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos2ivARB: procedure(const v: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos2sARB: procedure(x: TGLshort; y: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos2svARB: procedure(const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos3dARB: procedure(x: TGLdouble; y: TGLdouble; z: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos3dvARB: procedure(const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos3fARB: procedure(x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos3fvARB: procedure(const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos3iARB: procedure(x: TGLint; y: TGLint; z: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos3ivARB: procedure(const v: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos3sARB: procedure(x: TGLshort; y: TGLshort; z: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos3svARB: procedure(const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_ATI_draw_buffers
  glDrawBuffersATI: procedure(n: TGLsizei; const bufs: PGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_ATI_element_array
  glElementPointerATI: procedure(_type: TGLenum; const _pointer: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glDrawElementArrayATI: procedure(mode: TGLenum; count: TGLsizei); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glDrawRangeElementArrayATI: procedure(mode: TGLenum; start: TGLuint; _end: TGLuint; count: TGLsizei); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_ATI_envmap_bumpmap
  glTexBumpParameterivATI: procedure(pname: TGLenum; const param: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexBumpParameterfvATI: procedure(pname: TGLenum; const param: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetTexBumpParameterivATI: procedure(pname: TGLenum; param: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetTexBumpParameterfvATI: procedure(pname: TGLenum; param: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_ATI_fragment_shader
  glGenFragmentShadersATI: function(range: TGLuint): TGLuint; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glBindFragmentShaderATI: procedure(id: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glDeleteFragmentShaderATI: procedure(id: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glBeginFragmentShaderATI: procedure(); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glEndFragmentShaderATI: procedure(); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPassTexCoordATI: procedure(dst: TGLuint; coord: TGLuint; swizzle: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSampleMapATI: procedure(dst: TGLuint; interp: TGLuint; swizzle: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColorFragmentOp1ATI: procedure(op: TGLenum; dst: TGLuint; dstMask: TGLuint; dstMod: TGLuint; arg1: TGLuint; arg1Rep: TGLuint; arg1Mod: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColorFragmentOp2ATI: procedure(op: TGLenum; dst: TGLuint; dstMask: TGLuint; dstMod: TGLuint; arg1: TGLuint; arg1Rep: TGLuint; arg1Mod: TGLuint; arg2: TGLuint; arg2Rep: TGLuint; arg2Mod: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColorFragmentOp3ATI: procedure(op: TGLenum; dst: TGLuint; dstMask: TGLuint; dstMod: TGLuint; arg1: TGLuint; arg1Rep: TGLuint; arg1Mod: TGLuint; arg2: TGLuint; arg2Rep: TGLuint; arg2Mod: TGLuint; arg3: TGLuint; arg3Rep: TGLuint; arg3Mod: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glAlphaFragmentOp1ATI: procedure(op: TGLenum; dst: TGLuint; dstMod: TGLuint; arg1: TGLuint; arg1Rep: TGLuint; arg1Mod: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glAlphaFragmentOp2ATI: procedure(op: TGLenum; dst: TGLuint; dstMod: TGLuint; arg1: TGLuint; arg1Rep: TGLuint; arg1Mod: TGLuint; arg2: TGLuint; arg2Rep: TGLuint; arg2Mod: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glAlphaFragmentOp3ATI: procedure(op: TGLenum; dst: TGLuint; dstMod: TGLuint; arg1: TGLuint; arg1Rep: TGLuint; arg1Mod: TGLuint; arg2: TGLuint; arg2Rep: TGLuint; arg2Mod: TGLuint; arg3: TGLuint; arg3Rep: TGLuint; arg3Mod: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSetFragmentShaderConstantATI: procedure(dst: TGLuint; const value: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_ATI_map_object_buffer
  glMapObjectBufferATI: function(buffer: TGLuint): PGLvoid; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glUnmapObjectBufferATI: procedure(buffer: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_ATI_pn_triangles
  glPNTrianglesiATI: procedure(pname: TGLenum; param: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPNTrianglesfATI: procedure(pname: TGLenum; param: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_ATI_separate_stencil
  glStencilOpSeparateATI: procedure(face: TGLenum; sfail: TGLenum; dpfail: TGLenum; dppass: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glStencilFuncSeparateATI: procedure(frontfunc: TGLenum; backfunc: TGLenum; ref: TGLint; mask: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_ATI_vertex_array_object
  glNewObjectBufferATI: function(size: TGLsizei; const _pointer: PGLvoid; usage: TGLenum): TGLuint; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glIsObjectBufferATI: function(buffer: TGLuint): TGLboolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glUpdateObjectBufferATI: procedure(buffer: TGLuint; offset: TGLuint; size: TGLsizei; const _pointer: PGLvoid; preserve: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetObjectBufferfvATI: procedure(buffer: TGLuint; pname: TGLenum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetObjectBufferivATI: procedure(buffer: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFreeObjectBufferATI: procedure(buffer: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glArrayObjectATI: procedure(_array: TGLenum; size: TGLint; _type: TGLenum; stride: TGLsizei; buffer: TGLuint; offset: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetArrayObjectfvATI: procedure(_array: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetArrayObjectivATI: procedure(_array: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVariantArrayObjectATI: procedure(id: TGLuint; _type: TGLenum; stride: TGLsizei; buffer: TGLuint; offset: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetVariantArrayObjectfvATI: procedure(id: TGLuint; pname: TGLenum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetVariantArrayObjectivATI: procedure(id: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_ATI_vertex_attrib_array_object
  glVertexAttribArrayObjectATI: procedure(index: TGLuint; size: TGLint; _type: TGLenum; normalized: TGLboolean; stride: TGLsizei; buffer: TGLuint; offset: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetVertexAttribArrayObjectfvATI: procedure(index: TGLuint; pname: TGLenum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetVertexAttribArrayObjectivATI: procedure(index: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_ATI_vertex_streams
  glVertexStream1sATI: procedure(stream: TGLenum; x: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexStream1svATI: procedure(stream: TGLenum; const coords: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexStream1iATI: procedure(stream: TGLenum; x: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexStream1ivATI: procedure(stream: TGLenum; const coords: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexStream1fATI: procedure(stream: TGLenum; x: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexStream1fvATI: procedure(stream: TGLenum; const coords: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexStream1dATI: procedure(stream: TGLenum; x: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexStream1dvATI: procedure(stream: TGLenum; const coords: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexStream2sATI: procedure(stream: TGLenum; x: TGLshort; y: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexStream2svATI: procedure(stream: TGLenum; const coords: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexStream2iATI: procedure(stream: TGLenum; x: TGLint; y: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexStream2ivATI: procedure(stream: TGLenum; const coords: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexStream2fATI: procedure(stream: TGLenum; x: TGLfloat; y: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexStream2fvATI: procedure(stream: TGLenum; const coords: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexStream2dATI: procedure(stream: TGLenum; x: TGLdouble; y: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexStream2dvATI: procedure(stream: TGLenum; const coords: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexStream3sATI: procedure(stream: TGLenum; x: TGLshort; y: TGLshort; z: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexStream3svATI: procedure(stream: TGLenum; const coords: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexStream3iATI: procedure(stream: TGLenum; x: TGLint; y: TGLint; z: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexStream3ivATI: procedure(stream: TGLenum; const coords: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexStream3fATI: procedure(stream: TGLenum; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexStream3fvATI: procedure(stream: TGLenum; const coords: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexStream3dATI: procedure(stream: TGLenum; x: TGLdouble; y: TGLdouble; z: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexStream3dvATI: procedure(stream: TGLenum; const coords: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexStream4sATI: procedure(stream: TGLenum; x: TGLshort; y: TGLshort; z: TGLshort; w: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexStream4svATI: procedure(stream: TGLenum; const coords: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexStream4iATI: procedure(stream: TGLenum; x: TGLint; y: TGLint; z: TGLint; w: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexStream4ivATI: procedure(stream: TGLenum; const coords: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexStream4fATI: procedure(stream: TGLenum; x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexStream4fvATI: procedure(stream: TGLenum; const coords: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexStream4dATI: procedure(stream: TGLenum; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexStream4dvATI: procedure(stream: TGLenum; const coords: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glNormalStream3bATI: procedure(stream: TGLenum; nx: TGLbyte; ny: TGLbyte; nz: TGLbyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glNormalStream3bvATI: procedure(stream: TGLenum; const coords: PGLbyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glNormalStream3sATI: procedure(stream: TGLenum; nx: TGLshort; ny: TGLshort; nz: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glNormalStream3svATI: procedure(stream: TGLenum; const coords: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glNormalStream3iATI: procedure(stream: TGLenum; nx: TGLint; ny: TGLint; nz: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glNormalStream3ivATI: procedure(stream: TGLenum; const coords: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glNormalStream3fATI: procedure(stream: TGLenum; nx: TGLfloat; ny: TGLfloat; nz: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glNormalStream3fvATI: procedure(stream: TGLenum; const coords: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glNormalStream3dATI: procedure(stream: TGLenum; nx: TGLdouble; ny: TGLdouble; nz: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glNormalStream3dvATI: procedure(stream: TGLenum; const coords: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glClientActiveVertexStreamATI: procedure(stream: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexBlendEnviATI: procedure(pname: TGLenum; param: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexBlendEnvfATI: procedure(pname: TGLenum; param: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_EXT_blend_color
  glBlendColorEXT: procedure(red: TGLclampf; green: TGLclampf; blue: TGLclampf; alpha: TGLclampf); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_EXT_blend_func_separate
  glBlendFuncSeparateEXT: procedure(sfactorRGB: TGLenum; dfactorRGB: TGLenum; sfactorAlpha: TGLenum; dfactorAlpha: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_EXT_blend_minmax
  glBlendEquationEXT: procedure(mode: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_EXT_color_subtable
  glColorSubTableEXT: procedure(target: TGLenum; start: TGLsizei; count: TGLsizei; format: TGLenum; _type: TGLenum; const data: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glCopyColorSubTableEXT: procedure(target: TGLenum; start: TGLsizei; x: TGLint; y: TGLint; width: TGLsizei); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_EXT_compiled_vertex_array
  glLockArraysEXT: procedure(first: TGLint; count: TGLsizei); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glUnlockArraysEXT: procedure(); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_EXT_convolution
  glConvolutionFilter1DEXT: procedure(target: TGLenum; internalformat: TGLenum; width: TGLsizei; format: TGLenum; _type: TGLenum; const image: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glConvolutionFilter2DEXT: procedure(target: TGLenum; internalformat: TGLenum; width: TGLsizei; height: TGLsizei; format: TGLenum; _type: TGLenum; const image: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glConvolutionParameterfEXT: procedure(target: TGLenum; pname: TGLenum; params: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glConvolutionParameterfvEXT: procedure(target: TGLenum; pname: TGLenum; const params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glConvolutionParameteriEXT: procedure(target: TGLenum; pname: TGLenum; params: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glConvolutionParameterivEXT: procedure(target: TGLenum; pname: TGLenum; const params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glCopyConvolutionFilter1DEXT: procedure(target: TGLenum; internalformat: TGLenum; x: TGLint; y: TGLint; width: TGLsizei); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glCopyConvolutionFilter2DEXT: procedure(target: TGLenum; internalformat: TGLenum; x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetConvolutionFilterEXT: procedure(target: TGLenum; format: TGLenum; _type: TGLenum; image: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetConvolutionParameterfvEXT: procedure(target: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetConvolutionParameterivEXT: procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetSeparableFilterEXT: procedure(target: TGLenum; format: TGLenum; _type: TGLenum; row: PGLvoid; column: PGLvoid; span: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSeparableFilter2DEXT: procedure(target: TGLenum; internalformat: TGLenum; width: TGLsizei; height: TGLsizei; format: TGLenum; _type: TGLenum; const row: PGLvoid; const column: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_EXT_coordinate_frame
  glTangent3bEXT: procedure(tx: TGLbyte; ty: TGLbyte; tz: TGLbyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTangent3bvEXT: procedure(const v: PGLbyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTangent3dEXT: procedure(tx: TGLdouble; ty: TGLdouble; tz: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTangent3dvEXT: procedure(const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTangent3fEXT: procedure(tx: TGLfloat; ty: TGLfloat; tz: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTangent3fvEXT: procedure(const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTangent3iEXT: procedure(tx: TGLint; ty: TGLint; tz: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTangent3ivEXT: procedure(const v: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTangent3sEXT: procedure(tx: TGLshort; ty: TGLshort; tz: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTangent3svEXT: procedure(const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glBinormal3bEXT: procedure(bx: TGLbyte; by: TGLbyte; bz: TGLbyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glBinormal3bvEXT: procedure(const v: PGLbyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glBinormal3dEXT: procedure(bx: TGLdouble; by: TGLdouble; bz: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glBinormal3dvEXT: procedure(const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glBinormal3fEXT: procedure(bx: TGLfloat; by: TGLfloat; bz: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glBinormal3fvEXT: procedure(const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glBinormal3iEXT: procedure(bx: TGLint; by: TGLint; bz: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glBinormal3ivEXT: procedure(const v: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glBinormal3sEXT: procedure(bx: TGLshort; by: TGLshort; bz: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glBinormal3svEXT: procedure(const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTangentPointerEXT: procedure(_type: TGLenum; stride: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glBinormalPointerEXT: procedure(_type: TGLenum; stride: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_EXT_copy_texture
  glCopyTexImage1DEXT: procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; x: TGLint; y: TGLint; width: TGLsizei; border: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glCopyTexImage2DEXT: procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei; border: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glCopyTexSubImage1DEXT: procedure(target: TGLenum; level: TGLint; xoffset: TGLint; x: TGLint; y: TGLint; width: TGLsizei); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glCopyTexSubImage2DEXT: procedure(target: TGLenum; level: TGLint; xoffset: TGLint; yoffset: TGLint; x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glCopyTexSubImage3DEXT: procedure(target: TGLenum; level: TGLint; xoffset: TGLint; yoffset: TGLint; zoffset: TGLint; x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_EXT_cull_vertex
  glCullParameterdvEXT: procedure(pname: TGLenum; params: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glCullParameterfvEXT: procedure(pname: TGLenum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_EXT_draw_range_elements
  glDrawRangeElementsEXT: procedure(mode: TGLenum; start: TGLuint; _end: TGLuint; count: TGLsizei; _type: TGLenum; const indices: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_EXT_fog_coord
  glFogCoordfEXT: procedure(coord: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFogCoordfvEXT: procedure(const coord: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFogCoorddEXT: procedure(coord: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFogCoorddvEXT: procedure(const coord: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFogCoordPointerEXT: procedure(_type: TGLenum; stride: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_EXT_histogram
  glGetHistogramEXT: procedure(target: TGLenum; reset: TGLboolean; format: TGLenum; _type: TGLenum; values: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetHistogramParameterfvEXT: procedure(target: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetHistogramParameterivEXT: procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetMinmaxEXT: procedure(target: TGLenum; reset: TGLboolean; format: TGLenum; _type: TGLenum; values: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetMinmaxParameterfvEXT: procedure(target: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetMinmaxParameterivEXT: procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glHistogramEXT: procedure(target: TGLenum; width: TGLsizei; internalformat: TGLenum; sink: TGLboolean); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMinmaxEXT: procedure(target: TGLenum; internalformat: TGLenum; sink: TGLboolean); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glResetHistogramEXT: procedure(target: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glResetMinmaxEXT: procedure(target: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_EXT_index_func
  glIndexFuncEXT: procedure(func: TGLenum; ref: TGLclampf); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_EXT_index_material
  glIndexMaterialEXT: procedure(face: TGLenum; mode: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_EXT_light_texture
  glApplyTextureEXT: procedure(mode: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTextureLightEXT: procedure(pname: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTextureMaterialEXT: procedure(face: TGLenum; mode: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_EXT_multi_draw_arrays
  glMultiDrawArraysEXT: procedure(mode: TGLenum; first: PGLint; count: PGLsizei; primcount: TGLsizei); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiDrawElementsEXT: procedure(mode: TGLenum; const count: PGLsizei; _type: TGLenum; const indices: PGLvoid; primcount: TGLsizei); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_EXT_multisample
  glSampleMaskEXT: procedure(value: TGLclampf; invert: TGLboolean); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSamplePatternEXT: procedure(pattern: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_EXT_paletted_texture
  glColorTableEXT: procedure(target: TGLenum; internalFormat: TGLenum; width: TGLsizei; format: TGLenum; _type: TGLenum; const table: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetColorTableEXT: procedure(target: TGLenum; format: TGLenum; _type: TGLenum; data: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetColorTableParameterivEXT: procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetColorTableParameterfvEXT: procedure(target: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_EXT_pixel_transform
  glPixelTransformParameteriEXT: procedure(target: TGLenum; pname: TGLenum; param: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPixelTransformParameterfEXT: procedure(target: TGLenum; pname: TGLenum; param: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPixelTransformParameterivEXT: procedure(target: TGLenum; pname: TGLenum; const params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPixelTransformParameterfvEXT: procedure(target: TGLenum; pname: TGLenum; const params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_EXT_point_parameters
  glPointParameterfEXT: procedure(pname: TGLenum; param: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPointParameterfvEXT: procedure(pname: TGLenum; const params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_EXT_polygon_offset
  glPolygonOffsetEXT: procedure(factor: TGLfloat; bias: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_EXT_secondary_color
  glSecondaryColor3bEXT: procedure(red: TGLbyte; green: TGLbyte; blue: TGLbyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSecondaryColor3bvEXT: procedure(const v: PGLbyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSecondaryColor3dEXT: procedure(red: TGLdouble; green: TGLdouble; blue: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSecondaryColor3dvEXT: procedure(const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSecondaryColor3fEXT: procedure(red: TGLfloat; green: TGLfloat; blue: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSecondaryColor3fvEXT: procedure(const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSecondaryColor3iEXT: procedure(red: TGLint; green: TGLint; blue: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSecondaryColor3ivEXT: procedure(const v: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSecondaryColor3sEXT: procedure(red: TGLshort; green: TGLshort; blue: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSecondaryColor3svEXT: procedure(const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSecondaryColor3ubEXT: procedure(red: TGLubyte; green: TGLubyte; blue: TGLubyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSecondaryColor3ubvEXT: procedure(const v: PGLubyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSecondaryColor3uiEXT: procedure(red: TGLuint; green: TGLuint; blue: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSecondaryColor3uivEXT: procedure(const v: PGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSecondaryColor3usEXT: procedure(red: TGLushort; green: TGLushort; blue: TGLushort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSecondaryColor3usvEXT: procedure(const v: PGLushort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSecondaryColorPointerEXT: procedure(size: TGLint; _type: TGLenum; stride: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_EXT_stencil_two_side
  glActiveStencilFaceEXT: procedure(face: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_EXT_subtexture
  glTexSubImage1DEXT: procedure(target: TGLenum; level: TGLint; xoffset: TGLint; width: TGLsizei; format: TGLenum; _type: TGLenum; const pixels: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexSubImage2DEXT: procedure(target: TGLenum; level: TGLint; xoffset: TGLint; yoffset: TGLint; width: TGLsizei; height: TGLsizei; format: TGLenum; _type: TGLenum; const pixels: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_EXT_texture3D
  glTexImage3DEXT: procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; width: TGLsizei; height: TGLsizei; depth: TGLsizei; border: TGLint; format: TGLenum; _type: TGLenum; const pixels: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexSubImage3DEXT: procedure(target: TGLenum; level: TGLint; xoffset: TGLint; yoffset: TGLint; zoffset: TGLint; width: TGLsizei; height: TGLsizei; depth: TGLsizei; format: TGLenum; _type: TGLenum; const pixels: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_EXT_texture_object
  glAreTexturesResidentEXT: function(n: TGLsizei; const textures: PGLuint; residences: PGLboolean): TGLboolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glBindTextureEXT: procedure(target: TGLenum; texture: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glDeleteTexturesEXT: procedure(n: TGLsizei; const textures: PGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGenTexturesEXT: procedure(n: TGLsizei; textures: PGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glIsTextureEXT: function(texture: TGLuint): TGLboolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPrioritizeTexturesEXT: procedure(n: TGLsizei; const textures: PGLuint; const priorities: PGLclampf); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_EXT_texture_perturb_normal
  glTextureNormalEXT: procedure(mode: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_EXT_vertex_array
  glArrayElementEXT: procedure(i: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColorPointerEXT: procedure(size: TGLint; _type: TGLenum; stride: TGLsizei; count: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glDrawArraysEXT: procedure(mode: TGLenum; first: TGLint; count: TGLsizei); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glEdgeFlagPointerEXT: procedure(stride: TGLsizei; count: TGLsizei; const _pointer: PGLboolean); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetPointervEXT: procedure(pname: TGLenum; params: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glIndexPointerEXT: procedure(_type: TGLenum; stride: TGLsizei; count: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glNormalPointerEXT: procedure(_type: TGLenum; stride: TGLsizei; count: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoordPointerEXT: procedure(size: TGLint; _type: TGLenum; stride: TGLsizei; count: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexPointerEXT: procedure(size: TGLint; _type: TGLenum; stride: TGLsizei; count: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_EXT_vertex_shader
  glBeginVertexShaderEXT: procedure(); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glEndVertexShaderEXT: procedure(); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glBindVertexShaderEXT: procedure(id: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGenVertexShadersEXT: function(range: TGLuint): TGLuint; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glDeleteVertexShaderEXT: procedure(id: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glShaderOp1EXT: procedure(op: TGLenum; res: TGLuint; arg1: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glShaderOp2EXT: procedure(op: TGLenum; res: TGLuint; arg1: TGLuint; arg2: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glShaderOp3EXT: procedure(op: TGLenum; res: TGLuint; arg1: TGLuint; arg2: TGLuint; arg3: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSwizzleEXT: procedure(res: TGLuint; _in: TGLuint; outX: TGLenum; outY: TGLenum; outZ: TGLenum; outW: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWriteMaskEXT: procedure(res: TGLuint; _in: TGLuint; outX: TGLenum; outY: TGLenum; outZ: TGLenum; outW: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glInsertComponentEXT: procedure(res: TGLuint; src: TGLuint; num: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glExtractComponentEXT: procedure(res: TGLuint; src: TGLuint; num: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGenSymbolsEXT: function(datatype: TGLenum; storagetype: TGLenum; range: TGLenum; components: TGLuint): TGLuint; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSetInvariantEXT: procedure(id: TGLuint; _type: TGLenum; const addr: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSetLocalConstantEXT: procedure(id: TGLuint; _type: TGLenum; const addr: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVariantbvEXT: procedure(id: TGLuint; const addr: PGLbyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVariantsvEXT: procedure(id: TGLuint; const addr: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVariantivEXT: procedure(id: TGLuint; const addr: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVariantfvEXT: procedure(id: TGLuint; const addr: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVariantdvEXT: procedure(id: TGLuint; const addr: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVariantubvEXT: procedure(id: TGLuint; const addr: PGLubyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVariantusvEXT: procedure(id: TGLuint; const addr: PGLushort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVariantuivEXT: procedure(id: TGLuint; const addr: PGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVariantPointerEXT: procedure(id: TGLuint; _type: TGLenum; stride: TGLuint; const addr: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glEnableVariantClientStateEXT: procedure(id: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glDisableVariantClientStateEXT: procedure(id: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glBindLightParameterEXT: function(light: TGLenum; value: TGLenum): TGLuint; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glBindMaterialParameterEXT: function(face: TGLenum; value: TGLenum): TGLuint; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glBindTexGenParameterEXT: function(_unit: TGLenum; coord: TGLenum; value: TGLenum): TGLuint; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glBindTextureUnitParameterEXT: function(_unit: TGLenum; value: TGLenum): TGLuint; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glBindParameterEXT: function(value: TGLenum): TGLuint; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glIsVariantEnabledEXT: function(id: TGLuint; cap: TGLenum): TGLboolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetVariantBooleanvEXT: procedure(id: TGLuint; value: TGLenum; data: PGLboolean); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetVariantIntegervEXT: procedure(id: TGLuint; value: TGLenum; data: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetVariantFloatvEXT: procedure(id: TGLuint; value: TGLenum; data: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetVariantPointervEXT: procedure(id: TGLuint; value: TGLenum; data: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetInvariantBooleanvEXT: procedure(id: TGLuint; value: TGLenum; data: PGLboolean); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetInvariantIntegervEXT: procedure(id: TGLuint; value: TGLenum; data: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetInvariantFloatvEXT: procedure(id: TGLuint; value: TGLenum; data: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetLocalConstantBooleanvEXT: procedure(id: TGLuint; value: TGLenum; data: PGLboolean); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetLocalConstantIntegervEXT: procedure(id: TGLuint; value: TGLenum; data: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetLocalConstantFloatvEXT: procedure(id: TGLuint; value: TGLenum; data: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_EXT_vertex_weighting
  glVertexWeightfEXT: procedure(weight: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexWeightfvEXT: procedure(const weight: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexWeightPointerEXT: procedure(size: TGLsizei; _type: TGLenum; stride: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_HP_image_transform
  glImageTransformParameteriHP: procedure(target: TGLenum; pname: TGLenum; param: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glImageTransformParameterfHP: procedure(target: TGLenum; pname: TGLenum; param: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glImageTransformParameterivHP: procedure(target: TGLenum; pname: TGLenum; const params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glImageTransformParameterfvHP: procedure(target: TGLenum; pname: TGLenum; const params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetImageTransformParameterivHP: procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetImageTransformParameterfvHP: procedure(target: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_IBM_multimode_draw_arrays
  glMultiModeDrawArraysIBM: procedure(mode: TGLenum; const first: PGLint; const count: PGLsizei; primcount: TGLsizei; modestride: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiModeDrawElementsIBM: procedure(const mode: PGLenum; const count: PGLsizei; _type: TGLenum; const indices: PGLvoid; primcount: TGLsizei; modestride: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_IBM_vertex_array_lists
  glColorPointerListIBM: procedure(size: TGLint; _type: TGLenum; stride: TGLint; const _pointer: PGLvoid; ptrstride: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSecondaryColorPointerListIBM: procedure(size: TGLint; _type: TGLenum; stride: TGLint; const _pointer: PGLvoid; ptrstride: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glEdgeFlagPointerListIBM: procedure(stride: TGLint; const _pointer: PGLboolean; ptrstride: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFogCoordPointerListIBM: procedure(_type: TGLenum; stride: TGLint; const _pointer: PGLvoid; ptrstride: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glIndexPointerListIBM: procedure(_type: TGLenum; stride: TGLint; const _pointer: PGLvoid; ptrstride: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glNormalPointerListIBM: procedure(_type: TGLenum; stride: TGLint; const _pointer: PGLvoid; ptrstride: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoordPointerListIBM: procedure(size: TGLint; _type: TGLenum; stride: TGLint; const _pointer: PGLvoid; ptrstride: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexPointerListIBM: procedure(size: TGLint; _type: TGLenum; stride: TGLint; const _pointer: PGLvoid; ptrstride: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_INGR_blend_func_separate
  glBlendFuncSeparateINGR: procedure(sfactorRGB: TGLenum; dfactorRGB: TGLenum; sfactorAlpha: TGLenum; dfactorAlpha: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_INTEL_parallel_arrays
  glVertexPointervINTEL: procedure(size: TGLint; _type: TGLenum; const _pointer: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glNormalPointervINTEL: procedure(_type: TGLenum; const _pointer: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColorPointervINTEL: procedure(size: TGLint; _type: TGLenum; const _pointer: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoordPointervINTEL: procedure(size: TGLint; _type: TGLenum; const _pointer: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_MESA_resize_buffers
  glResizeBuffersMESA: procedure(); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_MESA_window_pos
  glWindowPos2dMESA: procedure(x: TGLdouble; y: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos2dvMESA: procedure(const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos2fMESA: procedure(x: TGLfloat; y: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos2fvMESA: procedure(const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos2iMESA: procedure(x: TGLint; y: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos2ivMESA: procedure(const v: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos2sMESA: procedure(x: TGLshort; y: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos2svMESA: procedure(const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos3dMESA: procedure(x: TGLdouble; y: TGLdouble; z: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos3dvMESA: procedure(const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos3fMESA: procedure(x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos3fvMESA: procedure(const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos3iMESA: procedure(x: TGLint; y: TGLint; z: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos3ivMESA: procedure(const v: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos3sMESA: procedure(x: TGLshort; y: TGLshort; z: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos3svMESA: procedure(const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos4dMESA: procedure(x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos4dvMESA: procedure(const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos4fMESA: procedure(x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos4fvMESA: procedure(const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos4iMESA: procedure(x: TGLint; y: TGLint; z: TGLint; w: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos4ivMESA: procedure(const v: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos4sMESA: procedure(x: TGLshort; y: TGLshort; z: TGLshort; w: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glWindowPos4svMESA: procedure(const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_NV_evaluators
  glMapControlPointsNV: procedure(target: TGLenum; index: TGLuint; _type: TGLenum; ustride: TGLsizei; vstride: TGLsizei; uorder: TGLint; vorder: TGLint; _packed: TGLboolean; const points: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMapParameterivNV: procedure(target: TGLenum; pname: TGLenum; const params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMapParameterfvNV: procedure(target: TGLenum; pname: TGLenum; const params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetMapControlPointsNV: procedure(target: TGLenum; index: TGLuint; _type: TGLenum; ustride: TGLsizei; vstride: TGLsizei; _packed: TGLboolean; points: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetMapParameterivNV: procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetMapParameterfvNV: procedure(target: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetMapAttribParameterivNV: procedure(target: TGLenum; index: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetMapAttribParameterfvNV: procedure(target: TGLenum; index: TGLuint; pname: TGLenum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glEvalMapsNV: procedure(target: TGLenum; mode: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_NV_fence
  glDeleteFencesNV: procedure(n: TGLsizei; const fences: PGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGenFencesNV: procedure(n: TGLsizei; fences: PGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glIsFenceNV: function(fence: TGLuint): TGLboolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTestFenceNV: function(fence: TGLuint): TGLboolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetFenceivNV: procedure(fence: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFinishFenceNV: procedure(fence: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSetFenceNV: procedure(fence: TGLuint; condition: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_NV_fragment_program
  glProgramNamedParameter4fNV: procedure(id: TGLuint; len: TGLsizei; const name: PGLubyte; x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glProgramNamedParameter4dNV: procedure(id: TGLuint; len: TGLsizei; const name: PGLubyte; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glProgramNamedParameter4fvNV: procedure(id: TGLuint; len: TGLsizei; const name: PGLubyte; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glProgramNamedParameter4dvNV: procedure(id: TGLuint; len: TGLsizei; const name: PGLubyte; const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetProgramNamedParameterfvNV: procedure(id: TGLuint; len: TGLsizei; const name: PGLubyte; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetProgramNamedParameterdvNV: procedure(id: TGLuint; len: TGLsizei; const name: PGLubyte; params: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_NV_half_float
  glVertex2hNV: procedure(x: TGLhalfNV; y: TGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertex2hvNV: procedure(const v: PGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertex3hNV: procedure(x: TGLhalfNV; y: TGLhalfNV; z: TGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertex3hvNV: procedure(const v: PGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertex4hNV: procedure(x: TGLhalfNV; y: TGLhalfNV; z: TGLhalfNV; w: TGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertex4hvNV: procedure(const v: PGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glNormal3hNV: procedure(nx: TGLhalfNV; ny: TGLhalfNV; nz: TGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glNormal3hvNV: procedure(const v: PGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor3hNV: procedure(red: TGLhalfNV; green: TGLhalfNV; blue: TGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor3hvNV: procedure(const v: PGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor4hNV: procedure(red: TGLhalfNV; green: TGLhalfNV; blue: TGLhalfNV; alpha: TGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor4hvNV: procedure(const v: PGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord1hNV: procedure(s: TGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord1hvNV: procedure(const v: PGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord2hNV: procedure(s: TGLhalfNV; t: TGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord2hvNV: procedure(const v: PGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord3hNV: procedure(s: TGLhalfNV; t: TGLhalfNV; r: TGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord3hvNV: procedure(const v: PGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord4hNV: procedure(s: TGLhalfNV; t: TGLhalfNV; r: TGLhalfNV; q: TGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord4hvNV: procedure(const v: PGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord1hNV: procedure(target: TGLenum; s: TGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord1hvNV: procedure(target: TGLenum; const v: PGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord2hNV: procedure(target: TGLenum; s: TGLhalfNV; t: TGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord2hvNV: procedure(target: TGLenum; const v: PGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord3hNV: procedure(target: TGLenum; s: TGLhalfNV; t: TGLhalfNV; r: TGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord3hvNV: procedure(target: TGLenum; const v: PGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord4hNV: procedure(target: TGLenum; s: TGLhalfNV; t: TGLhalfNV; r: TGLhalfNV; q: TGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glMultiTexCoord4hvNV: procedure(target: TGLenum; const v: PGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFogCoordhNV: procedure(fog: TGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFogCoordhvNV: procedure(const fog: PGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSecondaryColor3hNV: procedure(red: TGLhalfNV; green: TGLhalfNV; blue: TGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSecondaryColor3hvNV: procedure(const v: PGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexWeighthNV: procedure(weight: TGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexWeighthvNV: procedure(const weight: PGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib1hNV: procedure(index: TGLuint; x: TGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib1hvNV: procedure(index: TGLuint; const v: PGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib2hNV: procedure(index: TGLuint; x: TGLhalfNV; y: TGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib2hvNV: procedure(index: TGLuint; const v: PGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib3hNV: procedure(index: TGLuint; x: TGLhalfNV; y: TGLhalfNV; z: TGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib3hvNV: procedure(index: TGLuint; const v: PGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib4hNV: procedure(index: TGLuint; x: TGLhalfNV; y: TGLhalfNV; z: TGLhalfNV; w: TGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib4hvNV: procedure(index: TGLuint; const v: PGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttribs1hvNV: procedure(index: TGLuint; n: TGLsizei; const v: PGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttribs2hvNV: procedure(index: TGLuint; n: TGLsizei; const v: PGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttribs3hvNV: procedure(index: TGLuint; n: TGLsizei; const v: PGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttribs4hvNV: procedure(index: TGLuint; n: TGLsizei; const v: PGLhalfNV); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_NV_occlusion_query
  glGenOcclusionQueriesNV: procedure(n: TGLsizei; ids: PGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glDeleteOcclusionQueriesNV: procedure(n: TGLsizei; const ids: PGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glIsOcclusionQueryNV: function(id: TGLuint): TGLboolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glBeginOcclusionQueryNV: procedure(id: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glEndOcclusionQueryNV: procedure(); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetOcclusionQueryivNV: procedure(id: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetOcclusionQueryuivNV: procedure(id: TGLuint; pname: TGLenum; params: PGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_NV_pixel_data_range
  glPixelDataRangeNV: procedure(target: TGLenum; length: TGLsizei; _pointer: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFlushPixelDataRangeNV: procedure(target: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_NV_point_sprite
  glPointParameteriNV: procedure(pname: TGLenum; param: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPointParameterivNV: procedure(pname: TGLenum; const params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_NV_primitive_restart
  glPrimitiveRestartNV: procedure(); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPrimitiveRestartIndexNV: procedure(index: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_NV_register_combiners
  glCombinerParameterfvNV: procedure(pname: TGLenum; const params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glCombinerParameterfNV: procedure(pname: TGLenum; param: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glCombinerParameterivNV: procedure(pname: TGLenum; const params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glCombinerParameteriNV: procedure(pname: TGLenum; param: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glCombinerInputNV: procedure(stage: TGLenum; portion: TGLenum; variable: TGLenum; input: TGLenum; mapping: TGLenum; componentUsage: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glCombinerOutputNV: procedure(stage: TGLenum; portion: TGLenum; abOutput: TGLenum; cdOutput: TGLenum; sumOutput: TGLenum; scale: TGLenum; bias: TGLenum; abDotProduct: TGLboolean; cdDotProduct: TGLboolean; muxSum: TGLboolean); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFinalCombinerInputNV: procedure(variable: TGLenum; input: TGLenum; mapping: TGLenum; componentUsage: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetCombinerInputParameterfvNV: procedure(stage: TGLenum; portion: TGLenum; variable: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetCombinerInputParameterivNV: procedure(stage: TGLenum; portion: TGLenum; variable: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetCombinerOutputParameterfvNV: procedure(stage: TGLenum; portion: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetCombinerOutputParameterivNV: procedure(stage: TGLenum; portion: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetFinalCombinerInputParameterfvNV: procedure(variable: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetFinalCombinerInputParameterivNV: procedure(variable: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_NV_register_combiners2
  glCombinerStageParameterfvNV: procedure(stage: TGLenum; pname: TGLenum; const params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetCombinerStageParameterfvNV: procedure(stage: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_NV_vertex_array_range
  glFlushVertexArrayRangeNV: procedure(); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexArrayRangeNV: procedure(length: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_NV_vertex_program
  glAreProgramsResidentNV: function(n: TGLsizei; const programs: PGLuint; residences: PGLboolean): TGLboolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glBindProgramNV: procedure(target: TGLenum; id: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glDeleteProgramsNV: procedure(n: TGLsizei; const programs: PGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glExecuteProgramNV: procedure(target: TGLenum; id: TGLuint; const params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGenProgramsNV: procedure(n: TGLsizei; programs: PGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetProgramParameterdvNV: procedure(target: TGLenum; index: TGLuint; pname: TGLenum; params: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetProgramParameterfvNV: procedure(target: TGLenum; index: TGLuint; pname: TGLenum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetProgramivNV: procedure(id: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetProgramStringNV: procedure(id: TGLuint; pname: TGLenum; _program: PGLubyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetTrackMatrixivNV: procedure(target: TGLenum; address: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetVertexAttribdvNV: procedure(index: TGLuint; pname: TGLenum; params: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetVertexAttribfvNV: procedure(index: TGLuint; pname: TGLenum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetVertexAttribivNV: procedure(index: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetVertexAttribPointervNV: procedure(index: TGLuint; pname: TGLenum; _pointer: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glIsProgramNV: function(id: TGLuint): TGLboolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glLoadProgramNV: procedure(target: TGLenum; id: TGLuint; len: TGLsizei; const _program: PGLubyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glProgramParameter4dNV: procedure(target: TGLenum; index: TGLuint; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glProgramParameter4dvNV: procedure(target: TGLenum; index: TGLuint; const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glProgramParameter4fNV: procedure(target: TGLenum; index: TGLuint; x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glProgramParameter4fvNV: procedure(target: TGLenum; index: TGLuint; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glProgramParameters4dvNV: procedure(target: TGLenum; index: TGLuint; count: TGLuint; const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glProgramParameters4fvNV: procedure(target: TGLenum; index: TGLuint; count: TGLuint; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glRequestResidentProgramsNV: procedure(n: TGLsizei; const programs: PGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTrackMatrixNV: procedure(target: TGLenum; address: TGLuint; matrix: TGLenum; transform: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttribPointerNV: procedure(index: TGLuint; fsize: TGLint; _type: TGLenum; stride: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib1dNV: procedure(index: TGLuint; x: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib1dvNV: procedure(index: TGLuint; const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib1fNV: procedure(index: TGLuint; x: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib1fvNV: procedure(index: TGLuint; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib1sNV: procedure(index: TGLuint; x: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib1svNV: procedure(index: TGLuint; const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib2dNV: procedure(index: TGLuint; x: TGLdouble; y: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib2dvNV: procedure(index: TGLuint; const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib2fNV: procedure(index: TGLuint; x: TGLfloat; y: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib2fvNV: procedure(index: TGLuint; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib2sNV: procedure(index: TGLuint; x: TGLshort; y: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib2svNV: procedure(index: TGLuint; const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib3dNV: procedure(index: TGLuint; x: TGLdouble; y: TGLdouble; z: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib3dvNV: procedure(index: TGLuint; const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib3fNV: procedure(index: TGLuint; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib3fvNV: procedure(index: TGLuint; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib3sNV: procedure(index: TGLuint; x: TGLshort; y: TGLshort; z: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib3svNV: procedure(index: TGLuint; const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib4dNV: procedure(index: TGLuint; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib4dvNV: procedure(index: TGLuint; const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib4fNV: procedure(index: TGLuint; x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib4fvNV: procedure(index: TGLuint; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib4sNV: procedure(index: TGLuint; x: TGLshort; y: TGLshort; z: TGLshort; w: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib4svNV: procedure(index: TGLuint; const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib4ubNV: procedure(index: TGLuint; x: TGLubyte; y: TGLubyte; z: TGLubyte; w: TGLubyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttrib4ubvNV: procedure(index: TGLuint; const v: PGLubyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttribs1dvNV: procedure(index: TGLuint; count: TGLsizei; const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttribs1fvNV: procedure(index: TGLuint; count: TGLsizei; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttribs1svNV: procedure(index: TGLuint; count: TGLsizei; const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttribs2dvNV: procedure(index: TGLuint; count: TGLsizei; const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttribs2fvNV: procedure(index: TGLuint; count: TGLsizei; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttribs2svNV: procedure(index: TGLuint; count: TGLsizei; const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttribs3dvNV: procedure(index: TGLuint; count: TGLsizei; const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttribs3fvNV: procedure(index: TGLuint; count: TGLsizei; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttribs3svNV: procedure(index: TGLuint; count: TGLsizei; const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttribs4dvNV: procedure(index: TGLuint; count: TGLsizei; const v: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttribs4fvNV: procedure(index: TGLuint; count: TGLsizei; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttribs4svNV: procedure(index: TGLuint; count: TGLsizei; const v: PGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glVertexAttribs4ubvNV: procedure(index: TGLuint; count: TGLsizei; const v: PGLubyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_PGI_misc_hints
  glHintPGI: procedure(target: TGLenum; mode: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_SGIS_detail_texture
  glDetailTexFuncSGIS: procedure(target: TGLenum; n: TGLsizei; const points: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetDetailTexFuncSGIS: procedure(target: TGLenum; points: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_SGIS_fog_function
  glFogFuncSGIS: procedure(n: TGLsizei; const points: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetFogFuncSGIS: procedure(points: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_SGIS_multisample
  glSampleMaskSGIS: procedure(value: TGLclampf; invert: TGLboolean); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSamplePatternSGIS: procedure(pattern: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_SGIS_pixel_texture
  glPixelTexGenParameteriSGIS: procedure(pname: TGLenum; param: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPixelTexGenParameterivSGIS: procedure(pname: TGLenum; const params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPixelTexGenParameterfSGIS: procedure(pname: TGLenum; param: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPixelTexGenParameterfvSGIS: procedure(pname: TGLenum; const params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetPixelTexGenParameterivSGIS: procedure(pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetPixelTexGenParameterfvSGIS: procedure(pname: TGLenum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_SGIS_point_parameters
  glPointParameterfSGIS: procedure(pname: TGLenum; param: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPointParameterfvSGIS: procedure(pname: TGLenum; const params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_SGIS_sharpen_texture
  glSharpenTexFuncSGIS: procedure(target: TGLenum; n: TGLsizei; const points: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetSharpenTexFuncSGIS: procedure(target: TGLenum; points: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_SGIS_texture4D
  glTexImage4DSGIS: procedure(target: TGLenum; level: TGLint; internalformat: TGLenum; width: TGLsizei; height: TGLsizei; depth: TGLsizei; size4d: TGLsizei; border: TGLint; format: TGLenum; _type: TGLenum; const pixels: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexSubImage4DSGIS: procedure(target: TGLenum; level: TGLint; xoffset: TGLint; yoffset: TGLint; zoffset: TGLint; woffset: TGLint; width: TGLsizei; height: TGLsizei; depth: TGLsizei; size4d: TGLsizei; format: TGLenum; _type: TGLenum; const pixels: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_SGIS_texture_color_mask
  glTextureColorMaskSGIS: procedure(red: TGLboolean; green: TGLboolean; blue: TGLboolean; alpha: TGLboolean); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_SGIS_texture_filter4
  glGetTexFilterFuncSGIS: procedure(target: TGLenum; filter: TGLenum; weights: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexFilterFuncSGIS: procedure(target: TGLenum; filter: TGLenum; n: TGLsizei; const weights: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_SGIX_async
  glAsyncMarkerSGIX: procedure(marker: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFinishAsyncSGIX: function(markerp: PGLuint): TGLint; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPollAsyncSGIX: function(markerp: PGLuint): TGLint; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGenAsyncMarkersSGIX: function(range: TGLsizei): TGLuint; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glDeleteAsyncMarkersSGIX: procedure(marker: TGLuint; range: TGLsizei); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glIsAsyncMarkerSGIX: function(marker: TGLuint): TGLboolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_SGIX_flush_raster
  glFlushRasterSGIX: procedure(); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_SGIX_fragment_lighting
  glFragmentColorMaterialSGIX: procedure(face: TGLenum; mode: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFragmentLightfSGIX: procedure(light: TGLenum; pname: TGLenum; param: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFragmentLightfvSGIX: procedure(light: TGLenum; pname: TGLenum; const params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFragmentLightiSGIX: procedure(light: TGLenum; pname: TGLenum; param: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFragmentLightivSGIX: procedure(light: TGLenum; pname: TGLenum; const params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFragmentLightModelfSGIX: procedure(pname: TGLenum; param: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFragmentLightModelfvSGIX: procedure(pname: TGLenum; const params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFragmentLightModeliSGIX: procedure(pname: TGLenum; param: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFragmentLightModelivSGIX: procedure(pname: TGLenum; const params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFragmentMaterialfSGIX: procedure(face: TGLenum; pname: TGLenum; param: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFragmentMaterialfvSGIX: procedure(face: TGLenum; pname: TGLenum; const params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFragmentMaterialiSGIX: procedure(face: TGLenum; pname: TGLenum; param: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glFragmentMaterialivSGIX: procedure(face: TGLenum; pname: TGLenum; const params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetFragmentLightfvSGIX: procedure(light: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetFragmentLightivSGIX: procedure(light: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetFragmentMaterialfvSGIX: procedure(face: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetFragmentMaterialivSGIX: procedure(face: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glLightEnviSGIX: procedure(pname: TGLenum; param: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_SGIX_framezoom
  glFrameZoomSGIX: procedure(factor: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_SGIX_igloo_interface
  glIglooInterfaceSGIX: procedure(pname: TGLenum; const params: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_SGIX_instruments
  glGetInstrumentsSGIX: function(): TGLint; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glInstrumentsBufferSGIX: procedure(size: TGLsizei; buffer: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glPollInstrumentsSGIX: function(marker_p: PGLint): TGLint; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glReadInstrumentsSGIX: procedure(marker: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glStartInstrumentsSGIX: procedure(); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glStopInstrumentsSGIX: procedure(marker: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_SGIX_list_priority
  glGetListParameterfvSGIX: procedure(list: TGLuint; pname: TGLenum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetListParameterivSGIX: procedure(list: TGLuint; pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glListParameterfSGIX: procedure(list: TGLuint; pname: TGLenum; param: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glListParameterfvSGIX: procedure(list: TGLuint; pname: TGLenum; const params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glListParameteriSGIX: procedure(list: TGLuint; pname: TGLenum; param: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glListParameterivSGIX: procedure(list: TGLuint; pname: TGLenum; const params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_SGIX_pixel_texture
  glPixelTexGenSGIX: procedure(mode: TGLenum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_SGIX_polynomial_ffd
  glDeformationMap3dSGIX: procedure(target: TGLenum; u1: TGLdouble; u2: TGLdouble; ustride: TGLint; uorder: TGLint; v1: TGLdouble; v2: TGLdouble; vstride: TGLint; vorder: TGLint; w1: TGLdouble; w2: TGLdouble; wstride: TGLint; worder: TGLint; const points: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glDeformationMap3fSGIX: procedure(target: TGLenum; u1: TGLfloat; u2: TGLfloat; ustride: TGLint; uorder: TGLint; v1: TGLfloat; v2: TGLfloat; vstride: TGLint; vorder: TGLint; w1: TGLfloat; w2: TGLfloat; wstride: TGLint; worder: TGLint; const points: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glDeformSGIX: procedure(mask: TGLbitfield); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glLoadIdentityDeformationMapSGIX: procedure(mask: TGLbitfield); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_SGIX_reference_plane
  glReferencePlaneSGIX: procedure(const equation: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_SGIX_sprite
  glSpriteParameterfSGIX: procedure(pname: TGLenum; param: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSpriteParameterfvSGIX: procedure(pname: TGLenum; const params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSpriteParameteriSGIX: procedure(pname: TGLenum; param: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glSpriteParameterivSGIX: procedure(pname: TGLenum; const params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_SGIX_tag_sample_buffer
  glTagSampleBufferSGIX: procedure(); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_SGI_color_table
  glColorTableSGI: procedure(target: TGLenum; internalformat: TGLenum; width: TGLsizei; format: TGLenum; _type: TGLenum; const table: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColorTableParameterfvSGI: procedure(target: TGLenum; pname: TGLenum; const params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColorTableParameterivSGI: procedure(target: TGLenum; pname: TGLenum; const params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glCopyColorTableSGI: procedure(target: TGLenum; internalformat: TGLenum; x: TGLint; y: TGLint; width: TGLsizei); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetColorTableSGI: procedure(target: TGLenum; format: TGLenum; _type: TGLenum; table: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetColorTableParameterfvSGI: procedure(target: TGLenum; pname: TGLenum; params: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGetColorTableParameterivSGI: procedure(target: TGLenum; pname: TGLenum; params: PGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_SUNX_constant_data
  glFinishTextureSUNX: procedure(); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_SUN_global_alpha
  glGlobalAlphaFactorbSUN: procedure(factor: TGLbyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGlobalAlphaFactorsSUN: procedure(factor: TGLshort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGlobalAlphaFactoriSUN: procedure(factor: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGlobalAlphaFactorfSUN: procedure(factor: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGlobalAlphaFactordSUN: procedure(factor: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGlobalAlphaFactorubSUN: procedure(factor: TGLubyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGlobalAlphaFactorusSUN: procedure(factor: TGLushort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glGlobalAlphaFactoruiSUN: procedure(factor: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_SUN_mesh_array
  glDrawMeshArraysSUN: procedure(mode: TGLenum; first: TGLint; count: TGLsizei; width: TGLsizei); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_SUN_triangle_list
  glReplacementCodeuiSUN: procedure(code: TGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glReplacementCodeusSUN: procedure(code: TGLushort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glReplacementCodeubSUN: procedure(code: TGLubyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glReplacementCodeuivSUN: procedure(const code: PGLuint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glReplacementCodeusvSUN: procedure(const code: PGLushort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glReplacementCodeubvSUN: procedure(const code: PGLubyte); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glReplacementCodePointerSUN: procedure(_type: TGLenum; stride: TGLsizei; const _pointer: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_SUN_vertex
  glColor4ubVertex2fSUN: procedure(r: TGLubyte; g: TGLubyte; b: TGLubyte; a: TGLubyte; x: TGLfloat; y: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor4ubVertex2fvSUN: procedure(const c: PGLubyte; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor4ubVertex3fSUN: procedure(r: TGLubyte; g: TGLubyte; b: TGLubyte; a: TGLubyte; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor4ubVertex3fvSUN: procedure(const c: PGLubyte; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor3fVertex3fSUN: procedure(r: TGLfloat; g: TGLfloat; b: TGLfloat; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor3fVertex3fvSUN: procedure(const c: PGLfloat; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glNormal3fVertex3fSUN: procedure(nx: TGLfloat; ny: TGLfloat; nz: TGLfloat; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glNormal3fVertex3fvSUN: procedure(const n: PGLfloat; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor4fNormal3fVertex3fSUN: procedure(r: TGLfloat; g: TGLfloat; b: TGLfloat; a: TGLfloat; nx: TGLfloat; ny: TGLfloat; nz: TGLfloat; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glColor4fNormal3fVertex3fvSUN: procedure(const c: PGLfloat; const n: PGLfloat; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord2fVertex3fSUN: procedure(s: TGLfloat; t: TGLfloat; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord2fVertex3fvSUN: procedure(const tc: PGLfloat; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord4fVertex4fSUN: procedure(s: TGLfloat; t: TGLfloat; p: TGLfloat; q: TGLfloat; x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord4fVertex4fvSUN: procedure(const tc: PGLfloat; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord2fColor4ubVertex3fSUN: procedure(s: TGLfloat; t: TGLfloat; r: TGLubyte; g: TGLubyte; b: TGLubyte; a: TGLubyte; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord2fColor4ubVertex3fvSUN: procedure(const tc: PGLfloat; const c: PGLubyte; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord2fColor3fVertex3fSUN: procedure(s: TGLfloat; t: TGLfloat; r: TGLfloat; g: TGLfloat; b: TGLfloat; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord2fColor3fVertex3fvSUN: procedure(const tc: PGLfloat; const c: PGLfloat; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord2fNormal3fVertex3fSUN: procedure(s: TGLfloat; t: TGLfloat; nx: TGLfloat; ny: TGLfloat; nz: TGLfloat; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord2fNormal3fVertex3fvSUN: procedure(const tc: PGLfloat; const n: PGLfloat; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord2fColor4fNormal3fVertex3fSUN: procedure(s: TGLfloat; t: TGLfloat; r: TGLfloat; g: TGLfloat; b: TGLfloat; a: TGLfloat; nx: TGLfloat; ny: TGLfloat; nz: TGLfloat; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord2fColor4fNormal3fVertex3fvSUN: procedure(const tc: PGLfloat; const c: PGLfloat; const n: PGLfloat; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord4fColor4fNormal3fVertex4fSUN: procedure(s: TGLfloat; t: TGLfloat; p: TGLfloat; q: TGLfloat; r: TGLfloat; g: TGLfloat; b: TGLfloat; a: TGLfloat; nx: TGLfloat; ny: TGLfloat; nz: TGLfloat; x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glTexCoord4fColor4fNormal3fVertex4fvSUN: procedure(const tc: PGLfloat; const c: PGLfloat; const n: PGLfloat; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glReplacementCodeuiVertex3fSUN: procedure(rc: TGLuint; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glReplacementCodeuiVertex3fvSUN: procedure(const rc: PGLuint; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glReplacementCodeuiColor4ubVertex3fSUN: procedure(rc: TGLuint; r: TGLubyte; g: TGLubyte; b: TGLubyte; a: TGLubyte; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glReplacementCodeuiColor4ubVertex3fvSUN: procedure(const rc: PGLuint; const c: PGLubyte; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glReplacementCodeuiColor3fVertex3fSUN: procedure(rc: TGLuint; r: TGLfloat; g: TGLfloat; b: TGLfloat; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glReplacementCodeuiColor3fVertex3fvSUN: procedure(const rc: PGLuint; const c: PGLfloat; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glReplacementCodeuiNormal3fVertex3fSUN: procedure(rc: TGLuint; nx: TGLfloat; ny: TGLfloat; nz: TGLfloat; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glReplacementCodeuiNormal3fVertex3fvSUN: procedure(const rc: PGLuint; const n: PGLfloat; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glReplacementCodeuiColor4fNormal3fVertex3fSUN: procedure(rc: TGLuint; r: TGLfloat; g: TGLfloat; b: TGLfloat; a: TGLfloat; nx: TGLfloat; ny: TGLfloat; nz: TGLfloat; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glReplacementCodeuiColor4fNormal3fVertex3fvSUN: procedure(const rc: PGLuint; const c: PGLfloat; const n: PGLfloat; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glReplacementCodeuiTexCoord2fVertex3fSUN: procedure(rc: TGLuint; s: TGLfloat; t: TGLfloat; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glReplacementCodeuiTexCoord2fVertex3fvSUN: procedure(const rc: PGLuint; const tc: PGLfloat; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN: procedure(rc: TGLuint; s: TGLfloat; t: TGLfloat; nx: TGLfloat; ny: TGLfloat; nz: TGLfloat; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN: procedure(const rc: PGLuint; const tc: PGLfloat; const n: PGLfloat; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN: procedure(rc: TGLuint; s: TGLfloat; t: TGLfloat; r: TGLfloat; g: TGLfloat; b: TGLfloat; a: TGLfloat; nx: TGLfloat; ny: TGLfloat; nz: TGLfloat; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN: procedure(const rc: PGLuint; const tc: PGLfloat; const c: PGLfloat; const n: PGLfloat; const v: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // window support functions
  {$IFDEF Win32}
  wglGetProcAddress: function(ProcName: PChar): Pointer; stdcall;
  wglCopyContext: function(p1: HGLRC; p2: HGLRC; p3: Cardinal): BOOL; stdcall;
  wglCreateContext: function(DC: HDC): HGLRC; stdcall;
  wglCreateLayerContext: function(p1: HDC; p2: Integer): HGLRC; stdcall;
  wglDeleteContext: function(p1: HGLRC): BOOL; stdcall;
  wglDescribeLayerPlane:function(p1: HDC; p2, p3: Integer; p4: Cardinal; var p5: TLayerPlaneDescriptor): BOOL; stdcall;
  wglGetCurrentContext: function: HGLRC; stdcall;
  wglGetCurrentDC: function: HDC; stdcall;
  wglGetLayerPaletteEntries: function(p1: HDC; p2, p3, p4: Integer; var pcr): Integer; stdcall;
  wglMakeCurrent: function(DC: HDC; p2: HGLRC): BOOL; stdcall;
  wglRealizeLayerPalette: function(p1: HDC; p2: Integer; p3: BOOL): BOOL; stdcall;
  wglSetLayerPaletteEntries: function(p1: HDC; p2, p3, p4: Integer; var pcr): Integer; stdcall;
  wglShareLists: function(p1, p2: HGLRC): BOOL; stdcall;
  wglSwapLayerBuffers: function(p1: HDC; p2: Cardinal): BOOL; stdcall;
  wglSwapMultipleBuffers: function(p1: UINT; const p2: PWGLSwap): DWORD; stdcall;
  wglUseFontBitmapsA: function(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall;
  wglUseFontOutlinesA: function (p1: HDC; p2, p3, p4: DWORD; p5, p6: Single; p7: Integer; p8: PGlyphMetricsFloat): BOOL; stdcall;
  wglUseFontBitmapsW: function(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall;
  wglUseFontOutlinesW: function (p1: HDC; p2, p3, p4: DWORD; p5, p6: Single; p7: Integer; p8: PGlyphMetricsFloat): BOOL; stdcall;
  wglUseFontBitmaps: function(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall;
  wglUseFontOutlines: function(p1: HDC; p2, p3, p4: DWORD; p5, p6: Single; p7: Integer; p8: PGlyphMetricsFloat): BOOL; stdcall;
  {$ENDIF}

  // WGL_ARB_buffer_region
  wglCreateBufferRegionARB: function(hDC: HDC; iLayerPlane: TGLint; uType: TGLuint): THandle; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglDeleteBufferRegionARB: procedure(hRegion: THandle); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglSaveBufferRegionARB: function(hRegion: THandle; x: TGLint; y: TGLint; width: TGLint; height: TGLint): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglRestoreBufferRegionARB: function(hRegion: THandle; x: TGLint; y: TGLint; width: TGLint; height: TGLint; xSrc: TGLint; ySrc: TGLint): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // WGL_ARB_extensions_string
  wglGetExtensionsStringARB: function(hdc: HDC): PChar; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // WGL_ARB_make_current_read
  wglMakeContextCurrentARB: function(hDrawDC: HDC; hReadDC: HDC; hglrc: HGLRC): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglGetCurrentReadDCARB: function(): HDC; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // WGL_ARB_pbuffer
  wglCreatePbufferARB: function(hDC: HDC; iPixelFormat: TGLint; iWidth: TGLint; iHeight: TGLint; const piAttribList: PGLint): HPBUFFERARB; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglGetPbufferDCARB: function(hPbuffer: HPBUFFERARB): HDC; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglReleasePbufferDCARB: function(hPbuffer: HPBUFFERARB; hDC: HDC): TGLint; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglDestroyPbufferARB: function(hPbuffer: HPBUFFERARB): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglQueryPbufferARB: function(hPbuffer: HPBUFFERARB; iAttribute: TGLint; piValue: PGLint): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // WGL_ARB_pixel_format
  wglGetPixelFormatAttribivARB: function(hdc: HDC; iPixelFormat: TGLint; iLayerPlane: TGLint; nAttributes: TGLuint; const piAttributes: PGLint; piValues: PGLint): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglGetPixelFormatAttribfvARB: function(hdc: HDC; iPixelFormat: TGLint; iLayerPlane: TGLint; nAttributes: TGLuint; const piAttributes: PGLint; pfValues: PGLfloat): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglChoosePixelFormatARB: function(hdc: HDC; const piAttribIList: PGLint; const pfAttribFList: PGLfloat; nMaxFormats: TGLuint; piFormats: PGLint; nNumFormats: PGLuint): BOOL; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // WGL_ARB_render_texture
  wglBindTexImageARB: function(hPbuffer: HPBUFFERARB; iBuffer: TGLint): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglReleaseTexImageARB: function(hPbuffer: HPBUFFERARB; iBuffer: TGLint): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglSetPbufferAttribARB: function(hPbuffer: HPBUFFERARB; const piAttribList: PGLint): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // WGL_EXT_display_color_table
  wglCreateDisplayColorTableEXT: function(id: TGLushort): TGLboolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglLoadDisplayColorTableEXT: function(const table: PGLushort; length: TGLuint): TGLboolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglBindDisplayColorTableEXT: function(id: TGLushort): TGLboolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglDestroyDisplayColorTableEXT: procedure(id: TGLushort); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // WGL_EXT_extensions_string
  wglGetExtensionsStringEXT: function(): PChar; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // WGL_EXT_make_current_read
  wglMakeContextCurrentEXT: function(hDrawDC: HDC; hReadDC: HDC; hglrc: HGLRC): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglGetCurrentReadDCEXT: function(): HDC; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // WGL_EXT_pbuffer
  wglCreatePbufferEXT: function(hDC: HDC; iPixelFormat: TGLint; iWidth: TGLint; iHeight: TGLint; const piAttribList: PGLint): HPBUFFEREXT; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglGetPbufferDCEXT: function(hPbuffer: HPBUFFEREXT): HDC; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglReleasePbufferDCEXT: function(hPbuffer: HPBUFFEREXT; hDC: HDC): TGLint; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglDestroyPbufferEXT: function(hPbuffer: HPBUFFEREXT): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglQueryPbufferEXT: function(hPbuffer: HPBUFFEREXT; iAttribute: TGLint; piValue: PGLint): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // WGL_EXT_pixel_format
  wglGetPixelFormatAttribivEXT: function(hdc: HDC; iPixelFormat: TGLint; iLayerPlane: TGLint; nAttributes: TGLuint; piAttributes: PGLint; piValues: PGLint): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglGetPixelFormatAttribfvEXT: function(hdc: HDC; iPixelFormat: TGLint; iLayerPlane: TGLint; nAttributes: TGLuint; piAttributes: PGLint; pfValues: PGLfloat): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglChoosePixelFormatEXT: function(hdc: HDC; const piAttribIList: PGLint; const pfAttribFList: PGLfloat; nMaxFormats: TGLuint; piFormats: PGLint; nNumFormats: PGLuint): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // WGL_EXT_swap_control
  wglSwapIntervalEXT: function(interval: TGLint): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglGetSwapIntervalEXT: function(): TGLint; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // WGL_I3D_digital_video_control
  wglGetDigitalVideoParametersI3D: function(hDC: HDC; iAttribute: TGLint; piValue: PGLint): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglSetDigitalVideoParametersI3D: function(hDC: HDC; iAttribute: TGLint; const piValue: PGLint): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // WGL_I3D_gamma
  wglGetGammaTableParametersI3D: function(hDC: HDC; iAttribute: TGLint; piValue: PGLint): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglSetGammaTableParametersI3D: function(hDC: HDC; iAttribute: TGLint; const piValue: PGLint): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglGetGammaTableI3D: function(hDC: HDC; iEntries: TGLint; puRed: PGLushort; puGreen: PGLushort; puBlue: PGLushort): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglSetGammaTableI3D: function(hDC: HDC; iEntries: TGLint; const puRed: PGLushort; const puGreen: PGLushort; const puBlue: PGLushort): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // WGL_I3D_genlock
  wglEnableGenlockI3D: function(hDC: HDC): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglDisableGenlockI3D: function(hDC: HDC): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglIsEnabledGenlockI3D: function(hDC: HDC; pFlag: Boolean): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglGenlockSourceI3D: function(hDC: HDC; uSource: TGLuint): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglGetGenlockSourceI3D: function(hDC: HDC; uSource: PGLuint): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglGenlockSourceEdgeI3D: function(hDC: HDC; uEdge: TGLuint): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglGetGenlockSourceEdgeI3D: function(hDC: HDC; uEdge: PGLuint): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglGenlockSampleRateI3D: function(hDC: HDC; uRate: TGLuint): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglGetGenlockSampleRateI3D: function(hDC: HDC; uRate: PGLuint): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglGenlockSourceDelayI3D: function(hDC: HDC; uDelay: TGLuint): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglGetGenlockSourceDelayI3D: function(hDC: HDC; uDelay: PGLuint): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglQueryGenlockMaxSourceDelayI3D: function(hDC: HDC; uMaxLineDelay: PGLuint; uMaxPixelDelay: PGLuint): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // WGL_I3D_image_buffer
  wglCreateImageBufferI3D: function(hDC: HDC; dwSize: TGLuint; uFlags: TGLuint): TGLvoid; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglDestroyImageBufferI3D: function(hDC: HDC; pAddress: TGLvoid): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglAssociateImageBufferEventsI3D: function(hDC: HDC; const pEvent: THandle; const pAddress: PGLvoid; const pSize: PGLuint; count: TGLuint): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglReleaseImageBufferEventsI3D: function(hDC: HDC; const pAddress: PGLvoid; count: TGLuint): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // WGL_I3D_swap_frame_lock
  wglEnableFrameLockI3D: function(): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglDisableFrameLockI3D: function(): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglIsEnabledFrameLockI3D: function(pFlag: Boolean): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglQueryFrameLockMasterI3D: function(pFlag: Boolean): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // WGL_I3D_swap_frame_usage
  wglGetFrameUsageI3D: function(pUsage: PGLfloat): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglBeginFrameTrackingI3D: function(): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglEndFrameTrackingI3D: function(): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglQueryFrameTrackingI3D: function(pFrameCount: PGLuint; pMissedFrames: PGLuint; pLastMissedUsage: PGLfloat): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // WGL_NV_vertex_array_range
  wglAllocateMemoryNV: procedure(size: TGLsizei; readfreq: TGLfloat; writefreq: TGLfloat; priority: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglFreeMemoryNV: procedure(_pointer: Pointer); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // WGL_OML_sync_control
  wglGetSyncValuesOML: function(hdc: HDC; ust: PGLint64; msc: PGLint64; sbc: PGLint64): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglGetMscRateOML: function(hdc: HDC; numerator: PGLint; denominator: PGLint): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglSwapBuffersMscOML: function(hdc: HDC; target_msc: TGLint64; divisor: TGLint64; remainder: TGLint64): TGLint64; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglSwapLayerBuffersMscOML: function(hdc: HDC; fuPlanes: TGLint; target_msc: TGLint64; divisor: TGLint64; remainder: TGLint64): TGLint64; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglWaitForMscOML: function(hdc: HDC; target_msc: TGLint64; divisor: TGLint64; remainder: TGLint64; ust: PGLint64; msc: PGLint64; sbc: PGLint64): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  wglWaitForSbcOML: function(hdc: HDC; target_sbc: TGLint64; ust: PGLint64; msc: PGLint64; sbc: PGLint64): Boolean; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // WIN_draw_range_elements
  glDrawRangeElementsWIN: procedure(mode: TGLenum; start: TGLuint; _end: TGLuint; count: TGLsizei; _type: TGLenum; const indices: PGLvoid); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // WIN_swap_hint
  glAddSwapHintRectWIN: procedure(x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}

  // GL_ARB_vertex_shader
  glGetActiveAttribARB: procedure(programobj:GLhandleARB;index:GLuint;maxLength:GLsizei;var length:GLsizei;var size:GLint;var _type:GLenum;name:PChar);stdcall;
  glGetAttribLocationARB: function(programObj:GLhandleARB;const char:PChar):glint;stdcall;
  glBindAttribLocationARB: procedure(programObj:GLhandleARB;index:GLuint;const name:PChar);
  //glGetVertexAttribPointervARB: procedure(index: gluint; pname: glenum; p:PPointer); stdcall;

  // GL_ARB_SHADER_OBJECTS
  glDeleteObjectARB: procedure(Obj: GLHandleARB); stdcall;
  glGetHandleARB: function(pname: GlEnum):GLHandleARB; stdcall;
  glDetachObjectARB: procedure(container, attached: GLHandleARB); stdcall;
  glCreateShaderObjectARB: function(shaderType: glenum):GLHandleARB; stdcall;
  glShaderSourceARB: procedure(shaderObj :GLHandleARB; count:glsizei; _string:PPGLCharARB; lengths:pglint);stdcall;
  glCompileShaderARB: function(shaderObj: GLHandleARB):glboolean; stdcall;
  glCreateProgramObjectARB: function:GLHandleARB; stdcall;
  glAttachObjectARB: procedure(programObj, shaderObj:GLhandleARB); stdcall;
  glLinkProgramARB: procedure(programObj: GLHandleARB); stdcall;
  glUseProgramObjectARB: procedure(programObj:GLHandleARB); stdcall;
  glValidateProgramARB: procedure(programObj: GLhandleARB); stdcall;
  glUniform1fARB: procedure(location:glint;v0:glfloat); stdcall;
  glUniform2fARB: procedure(location:glint;v0,v1:glfloat); stdcall;
  glUniform3fARB: procedure(location:glint;v0,v1,v2:glfloat); stdcall;
  glUniform4fARB: procedure(location:glint;v0,v1,v2,v3:glfloat); stdcall;
  glUniform1iARB: procedure(location:glint;v0:glint); stdcall;
  glUniform2iARB: procedure(location:glint;v0,v1:glint); stdcall;
  glUniform3iARB: procedure(location:glint;v0,v1,v2:glint); stdcall;
  glUniform4iARB: procedure(location:glint;v0,v1,v2,v3:glint); stdcall;
  glUniform1fvARB: procedure(location:glint;value:pglfloat); stdcall;
  glUniform2fvARB: procedure(location:glint;value:pglfloat); stdcall;
  glUniform3fvARB: procedure(location:glint;value:pglfloat); stdcall;
  glUniform4fvARB: procedure(location:glint;value:pglfloat); stdcall;
  glUniform1ivARB: procedure(location:glint;value:pglint); stdcall;
  glUniform2ivARB: procedure(location:glint;value:pglint); stdcall;
  glUniform3ivARB: procedure(location:glint;value:pglint); stdcall;
  glUniform4ivARB: procedure(location:glint;value:pglint); stdcall;
  glUniformMatrix2fvARB: procedure(location:glint;count:glsizei;transpose:glboolean;value:pglfloat); stdcall;
  glUniformMatrix3fvARB: procedure(location:glint;count:glsizei;transpose:glboolean;value:pglfloat); stdcall;
  glUniformMatrix4fvARB: procedure(location:glint;count:glsizei;transpose:glboolean;value:pglfloat); stdcall;
  glGetObjectParameterfvARB: procedure(Obj:GLHandleARB; pname:GLEnum; params:PGLFloat); stdcall;
  glGetObjectParameterivARB: procedure(Obj:GLHandleARB; pname:GLEnum; params:PGLInt); stdcall;
  glGetInfoLogARB: procedure(shaderObj:GLHandleARB; maxLength:glsizei; var length:glint;infoLog:PChar); stdcall;
  glGetAttachedObjectsARB: procedure(programobj:GLhandleARB; maxCount:GLsizei; var count:GLsizei;objects:PGLhandleARB); stdcall;
  glGetUniformLocationARB: function(programObj:GLhandleARB; const char:PChar):glint; stdcall;
  glGetActiveUniformARB:procedure(programobj:GLhandleARB;index:GLuint;maxLength:GLsizei;var length:GLsizei;var size:GLint;var _type:GLenum;name:PChar); stdcall;
  glGetUniformfvARB: procedure(programObj: GLhandleARB; location:GLint; params:PGLfloat); stdcall;
  glGetUniformivARB: procedure(programObj: GLhandleARB; location:GLint; params:PGLInt); stdcall;
  glGetShaderSourceARB: procedure(shader:GLhandleARB; maxLength:GLsizei; var length:GLsizei; source:PChar); stdcall;

  // GL_ARB_Occlusion_Query
  glGenQueriesARB:procedure(n:GLsizei;ids:PGLuint);stdcall;
  glDeleteQueriesARB:procedure(n:GLsizei;const ids:PGLuint);stdcall;
  glIsQueryARB:function(id:GLuint):boolean;stdcall;
  glBeginQueryARB:procedure(target:GLenum;id:GLuint);stdcall;
  glEndQueryARB:procedure(target:GLenum);stdcall;
  glGetQueryivARB:procedure(target,pname:GLenum;params:PGLint);stdcall;
  glGetQueryObjectivARB:procedure(id:GLuint;pname:GLenum;params:PGLint);stdcall;
  glGetQueryObjectuivARB:procedure(id:GLuint;pname:GLenum;params:PGLuint);stdcall;

  // ARB less version for GL 1.5
  glGenQueries:procedure(n:GLsizei;ids:PGLuint);stdcall;
  glDeleteQueries:procedure(n:GLsizei;const ids:PGLuint);stdcall;
  glIsQuery:function(id:GLuint):boolean;stdcall;
  glBeginQuery:procedure(target:GLenum;id:GLuint);stdcall;
  glEndQuery:procedure(target:GLenum);stdcall;
  glGetQueryiv:procedure(target,pname:GLenum;params:PGLint);stdcall;
  glGetQueryObjectiv:procedure(id:GLuint;pname:GLenum;params:PGLint);stdcall;
  glGetQueryObjectuiv:procedure(id:GLuint;pname:GLenum;params:PGLuint);stdcall;

  // GL utility functions and procedures
  gluErrorString: function(errCode: TGLEnum): PChar; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluGetString: function(name: TGLEnum): PChar; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluOrtho2D: procedure(left, right, bottom, top: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluPerspective: procedure(fovy, aspect, zNear, zFar: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluPickMatrix: procedure(x, y, width, height: TGLdouble; viewport: TVector4i); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluLookAt: procedure(eyex, eyey, eyez, centerx, centery, centerz, upx, upy, upz: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluProject: function(objx, objy, objz: TGLdouble; modelMatrix: TGLMatrixd4; projMatrix: TGLMatrixd4; viewport: TVector4i; winx, winy, winz: PGLdouble): TGLint; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluUnProject: function(winx, winy, winz: TGLdouble; modelMatrix: TGLMatrixd4; projMatrix: TGLMatrixd4; viewport: TVector4i;   objx, objy, objz: PGLdouble): TGLint; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluScaleImage: function(format: TGLEnum; widthin, heightin: TGLint; typein: TGLEnum; datain: Pointer; widthout, heightout: TGLint; typeout: TGLEnum; dataout: Pointer): TGLint; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluBuild1DMipmaps: function(target: TGLEnum; components, width: TGLint; format, atype: TGLEnum; data: Pointer): TGLint; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluBuild2DMipmaps: function(target: TGLEnum; components, width, height: TGLint; format, atype: TGLEnum; Data: Pointer): TGLint; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluNewQuadric: function: PGLUquadric; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluDeleteQuadric: procedure(state: PGLUquadric); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluQuadricNormals: procedure(quadObject: PGLUquadric; normals: TGLEnum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluQuadricTexture: procedure(quadObject: PGLUquadric; textureCoords: TGLboolean); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluQuadricOrientation: procedure(quadObject: PGLUquadric; orientation: TGLEnum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluQuadricDrawStyle: procedure(quadObject: PGLUquadric; drawStyle: TGLEnum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluCylinder: procedure(quadObject: PGLUquadric; baseRadius, topRadius, height: TGLdouble; slices, stacks: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluDisk: procedure(quadObject: PGLUquadric; innerRadius, outerRadius: TGLdouble; slices, loops: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluPartialDisk: procedure(quadObject: PGLUquadric; innerRadius, outerRadius: TGLdouble; slices, loops: TGLint; startAngle, sweepAngle: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluSphere: procedure(quadObject: PGLUquadric; radius: TGLdouble; slices, stacks: TGLint); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluQuadricCallback: procedure(quadObject: PGLUquadric; which: TGLEnum; fn: TGLUQuadricErrorProc); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluNewTess: function: PGLUtesselator; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluDeleteTess: procedure(tess: PGLUtesselator); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluTessBeginPolygon: procedure(tess: PGLUtesselator; polygon_data: Pointer); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluTessBeginContour: procedure(tess: PGLUtesselator); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluTessVertex: procedure(tess: PGLUtesselator; coords: TGLArrayd3; data: Pointer); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluTessEndContour: procedure(tess: PGLUtesselator); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluTessEndPolygon: procedure(tess: PGLUtesselator); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluTessProperty: procedure(tess: PGLUtesselator; which: TGLEnum; value: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluTessNormal: procedure(tess: PGLUtesselator; x, y, z: TGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluTessCallback: procedure(tess: PGLUtesselator; which: TGLEnum; fn: Pointer); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluGetTessProperty: procedure(tess: PGLUtesselator; which: TGLEnum; value: PGLdouble); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluNewNurbsRenderer: function: PGLUnurbs; {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluDeleteNurbsRenderer: procedure(nobj: PGLUnurbs); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluBeginSurface: procedure(nobj: PGLUnurbs); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluBeginCurve: procedure(nobj: PGLUnurbs); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluEndCurve: procedure(nobj: PGLUnurbs); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluEndSurface: procedure(nobj: PGLUnurbs); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluBeginTrim: procedure(nobj: PGLUnurbs); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluEndTrim: procedure(nobj: PGLUnurbs); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluPwlCurve: procedure(nobj: PGLUnurbs; count: TGLint; points: PGLfloat; stride: TGLint; atype: TGLEnum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluNurbsCurve: procedure(nobj: PGLUnurbs; nknots: TGLint; knot: PGLfloat; stride: TGLint; ctlarray: PGLfloat; order: TGLint; atype: TGLEnum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluNurbsSurface: procedure(nobj: PGLUnurbs; sknot_count: TGLint; sknot: PGLfloat; tknot_count: TGLint; tknot: PGLfloat; s_stride, t_stride: TGLint; ctlarray: PGLfloat; sorder, torder: TGLint; atype: TGLEnum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluLoadSamplingMatrices: procedure(nobj: PGLUnurbs; modelMatrix, projMatrix: TGLMatrixf4; viewport: TVector4i); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluNurbsProperty: procedure(nobj: PGLUnurbs; aproperty: TGLEnum; value: TGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluGetNurbsProperty: procedure(nobj: PGLUnurbs; aproperty: TGLEnum; value: PGLfloat); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluNurbsCallback: procedure(nobj: PGLUnurbs; which: TGLEnum; fn: TGLUNurbsErrorProc); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluBeginPolygon: procedure(tess: PGLUtesselator); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluNextContour: procedure(tess: PGLUtesselator; atype: TGLEnum); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}
  gluEndPolygon: procedure(tess: PGLUtesselator); {$IFDEF Win32} stdcall; {$ENDIF} {$IFDEF LINUX} cdecl; {$ENDIF}


type
 TRCOptions = set of (opDoubleBuffered,opGDI,opStereo);

var
  LibHandle          : THandle = 0;
  GLULibHandle       : THandle = 0;
  LastPixelFormat    : Integer;
  ExtensionsRead     : Boolean;
  ImplementationRead : Boolean;

function InitOpenGL(LibName: String = 'OpenGL32.dll';GLULibName : String = 'GLU32.dll'): Boolean;
procedure ClearExtensions;
procedure ReadExtensions;
procedure ReadImplementationProperties;
// =============================================================================
// Helper-Functions
// =============================================================================
function CreateRenderingContext(DC : HDC;Options : TRCOptions;ColorBits,ZBits,StencilBits,AccumBits,AuxBuffers : Integer;Layer : Integer) : HGLRC;
procedure ActivateRenderingContext(DC : HDC;RC : HGLRC);
procedure DeactivateRenderingContext;
procedure DestroyRenderingContext(RC : HGLRC);

implementation

// Thanks Mars!
function glProcedure(ProcName : PChar) : Pointer;
begin
Result := NIL;
if Addr(wglGetProcAddress) <> NIL then
 Result := wglGetProcAddress(ProcName);
if result <> NIL then
 exit;
Result := GetProcAddress(LibHandle, ProcName);
end;

function InitOpenGL(LibName: String; GLULibName : String): Boolean;
begin
Result       := False;
if LibHandle<>0 then FreeLibrary(LibHandle);
if GLULibHandle<>0 then FreeLibrary(GLULibHandle);
LibHandle    := LoadLibrary(PChar(LibName));
GLULibHandle := LoadLibrary(PChar(GLULibName));

wglGetProcAddress := GetProcAddress(LibHandle, 'wglGetProcAddress');

if (LibHandle <> 0) then
 begin
 // window support routines (WGL) ==============================================
 wglCopyContext              := glProcedure('wglCopyContext');
 wglCreateLayerContext       := glProcedure('wglCreateLayerContext');
 wglCreateContext            := glProcedure('wglCreateContext');
 wglDeleteContext            := glProcedure('wglDeleteContext');
 wglDescribeLayerPlane       := glProcedure('wglDescribeLayerPlane');
 wglGetCurrentContext        := glProcedure('wglGetCurrentContext');
 wglGetCurrentDC             := glProcedure('wglGetCurrentDC');
 wglGetLayerPaletteEntries   := glProcedure('wglGetLayerPaletteEntries');
 wglMakeCurrent              := glProcedure('wglMakeCurrent');
 wglRealizeLayerPalette      := glProcedure('wglRealizeLayerPalette');
 wglSetLayerPaletteEntries   := glProcedure('wglSetLayerPaletteEntries');
 wglShareLists               := glProcedure('wglShareLists');
 wglSwapLayerBuffers         := glProcedure('wglSwapLayerBuffers');
 wglSwapMultipleBuffers      := glProcedure('wglSwapMultipleBuffers');
 wglUseFontBitmapsA          := glProcedure('wglUseFontBitmapsA');
 wglUseFontOutlinesA         := glProcedure('wglUseFontOutlinesA');
 wglUseFontBitmapsW          := glProcedure('wglUseFontBitmapsW');
 wglUseFontOutlinesW         := glProcedure('wglUseFontOutlinesW');
 wglUseFontBitmaps           := glProcedure('wglUseFontBitmapsA');
 wglUseFontOutlines          := glProcedure('wglUseFontOutlinesA');

 Result := True;
 end;

if GLULibHandle <> 0 then
 begin
 // GLU ========================================================================
 gluBeginCurve               := GetProcAddress(GLULibHandle, 'gluBeginCurve');
 gluBeginPolygon             := GetProcAddress(GLULibHandle, 'gluBeginPolygon');
 gluBeginSurface             := GetProcAddress(GLULibHandle, 'gluBeginSurface');
 gluBeginTrim                := GetProcAddress(GLULibHandle, 'gluBeginTrim');
 gluBuild1DMipmaps           := GetProcAddress(GLULibHandle, 'gluBuild1DMipmaps');
 gluBuild2DMipmaps           := GetProcAddress(GLULibHandle, 'gluBuild2DMipmaps');
 gluCylinder                 := GetProcAddress(GLULibHandle, 'gluCylinder');
 gluDeleteNurbsRenderer      := GetProcAddress(GLULibHandle, 'gluDeleteNurbsRenderer');
 gluDeleteQuadric            := GetProcAddress(GLULibHandle, 'gluDeleteQuadric');
 gluDeleteTess               := GetProcAddress(GLULibHandle, 'gluDeleteTess');
 gluDisk                     := GetProcAddress(GLULibHandle, 'gluDisk');
 gluEndCurve                 := GetProcAddress(GLULibHandle, 'gluEndCurve');
 gluEndPolygon               := GetProcAddress(GLULibHandle, 'gluEndPolygon');
 gluEndSurface               := GetProcAddress(GLULibHandle, 'gluEndSurface');
 gluEndTrim                  := GetProcAddress(GLULibHandle, 'gluEndTrim');
 gluErrorString              := GetProcAddress(GLULibHandle, 'gluErrorString');
 gluGetNurbsProperty         := GetProcAddress(GLULibHandle, 'gluGetNurbsProperty');
 gluGetString                := GetProcAddress(GLULibHandle, 'gluGetString');
 gluGetTessProperty          := GetProcAddress(GLULibHandle, 'gluGetTessProperty');
 gluLoadSamplingMatrices     := GetProcAddress(GLULibHandle, 'gluLoadSamplingMatrices');
 gluLookAt                   := GetProcAddress(GLULibHandle, 'gluLookAt');
 gluNewNurbsRenderer         := GetProcAddress(GLULibHandle, 'gluNewNurbsRenderer');
 gluNewQuadric               := GetProcAddress(GLULibHandle, 'gluNewQuadric');
 gluNewTess                  := GetProcAddress(GLULibHandle, 'gluNewTess');
 gluNextContour              := GetProcAddress(GLULibHandle, 'gluNextContour');
 gluNurbsCallback            := GetProcAddress(GLULibHandle, 'gluNurbsCallback');
 gluNurbsCurve               := GetProcAddress(GLULibHandle, 'gluNurbsCurve');
 gluNurbsProperty            := GetProcAddress(GLULibHandle, 'gluNurbsProperty');
 gluNurbsSurface             := GetProcAddress(GLULibHandle, 'gluNurbsSurface');
 gluOrtho2D                  := GetProcAddress(GLULibHandle, 'gluOrtho2D');
 gluPartialDisk              := GetProcAddress(GLULibHandle, 'gluPartialDisk');
 gluPerspective              := GetProcAddress(GLULibHandle, 'gluPerspective');
 gluPickMatrix               := GetProcAddress(GLULibHandle, 'gluPickMatrix');
 gluProject                  := GetProcAddress(GLULibHandle, 'gluProject');
 gluPwlCurve                 := GetProcAddress(GLULibHandle, 'gluPwlCurve');
 gluQuadricCallback          := GetProcAddress(GLULibHandle, 'gluQuadricCallback');
 gluQuadricDrawStyle         := GetProcAddress(GLULibHandle, 'gluQuadricDrawStyle');
 gluQuadricNormals           := GetProcAddress(GLULibHandle, 'gluQuadricNormals');
 gluQuadricOrientation       := GetProcAddress(GLULibHandle, 'gluQuadricOrientation');
 gluQuadricTexture           := GetProcAddress(GLULibHandle, 'gluQuadricTexture');
 gluScaleImage               := GetProcAddress(GLULibHandle, 'gluScaleImage');
 gluSphere                   := GetProcAddress(GLULibHandle, 'gluSphere');
 gluTessBeginContour         := GetProcAddress(GLULibHandle, 'gluTessBeginContour');
 gluTessBeginPolygon         := GetProcAddress(GLULibHandle, 'gluTessBeginPolygon');
 gluTessCallback             := GetProcAddress(GLULibHandle, 'gluTessCallback');
 gluTessEndContour           := GetProcAddress(GLULibHandle, 'gluTessEndContour');
 gluTessEndPolygon           := GetProcAddress(GLULibHandle, 'gluTessEndPolygon');
 gluTessNormal               := GetProcAddress(GLULibHandle, 'gluTessNormal');
 gluTessProperty             := GetProcAddress(GLULibHandle, 'gluTessProperty');
 gluTessVertex               := GetProcAddress(GLULibHandle, 'gluTessVertex');
 gluUnProject                := GetProcAddress(GLULibHandle, 'gluUnProject');
 end;
end;

// =============================================================================
// ClearExtensions
// =============================================================================
procedure ClearExtensions;
begin
// GL_3DFX_tbuffer
glTbufferMask3DFX := nil;
// GL_APPLE_element_array
glElementPointerAPPLE             := nil;
glDrawElementArrayAPPLE           := nil;
glDrawRangeElementArrayAPPLE      := nil;
glMultiDrawElementArrayAPPLE      := nil;
glMultiDrawRangeElementArrayAPPLE := nil;
// GL_APPLE_fence
glGenFencesAPPLE    := nil;
glDeleteFencesAPPLE := nil;
glSetFenceAPPLE     := nil;
glIsFenceAPPLE      := nil;
glTestFenceAPPLE    := nil;
glFinishFenceAPPLE  := nil;
glTestObjectAPPLE   := nil;
glFinishObjectAPPLE := nil;
// GL_APPLE_vertex_array_object
glBindVertexArrayAPPLE    := nil;
glDeleteVertexArraysAPPLE := nil;
glGenVertexArraysAPPLE    := nil;
glIsVertexArrayAPPLE      := nil;
// GL_APPLE_vertex_array_range
glVertexArrayRangeAPPLE      := nil;
glFlushVertexArrayRangeAPPLE := nil;
glVertexArrayParameteriAPPLE := nil;
// GL_ARB_matrix_palette
glCurrentPaletteMatrixARB := nil;
glMatrixIndexubvARB       := nil;
glMatrixIndexusvARB       := nil;
glMatrixIndexuivARB       := nil;
glMatrixIndexPointerARB   := nil;
// GL_ARB_multisample
glSampleCoverageARB := nil;
// GL_ARB_multitexture
glActiveTextureARB       := nil;
glClientActiveTextureARB := nil;
glMultiTexCoord1dARB     := nil;
glMultiTexCoord1dvARB    := nil;
glMultiTexCoord1fARB     := nil;
glMultiTexCoord1fvARB    := nil;
glMultiTexCoord1iARB     := nil;
glMultiTexCoord1ivARB    := nil;
glMultiTexCoord1sARB     := nil;
glMultiTexCoord1svARB    := nil;
glMultiTexCoord2dARB     := nil;
glMultiTexCoord2dvARB    := nil;
glMultiTexCoord2fARB     := nil;
glMultiTexCoord2fvARB    := nil;
glMultiTexCoord2iARB     := nil;
glMultiTexCoord2ivARB    := nil;
glMultiTexCoord2sARB     := nil;
glMultiTexCoord2svARB    := nil;
glMultiTexCoord3dARB     := nil;
glMultiTexCoord3dvARB    := nil;
glMultiTexCoord3fARB     := nil;
glMultiTexCoord3fvARB    := nil;
glMultiTexCoord3iARB     := nil;
glMultiTexCoord3ivARB    := nil;
glMultiTexCoord3sARB     := nil;
glMultiTexCoord3svARB    := nil;
glMultiTexCoord4dARB     := nil;
glMultiTexCoord4dvARB    := nil;
glMultiTexCoord4fARB     := nil;
glMultiTexCoord4fvARB    := nil;
glMultiTexCoord4iARB     := nil;
glMultiTexCoord4ivARB    := nil;
glMultiTexCoord4sARB     := nil;
glMultiTexCoord4svARB    := nil;
// GL_ARB_point_parameters
glPointParameterfARB  := nil;
glPointParameterfvARB := nil;
// GL_ARB_texture_compression
glCompressedTexImage3DARB    := nil;
glCompressedTexImage2DARB    := nil;
glCompressedTexImage1DARB    := nil;
glCompressedTexSubImage3DARB := nil;
glCompressedTexSubImage2DARB := nil;
glCompressedTexSubImage1DARB := nil;
glGetCompressedTexImageARB   := nil;
// GL_ARB_transpose_matrix
glLoadTransposeMatrixfARB := nil;
glLoadTransposeMatrixdARB := nil;
glMultTransposeMatrixfARB := nil;
glMultTransposeMatrixdARB := nil;
// GL_ARB_vertex_blend
glWeightbvARB      := nil;
glWeightsvARB      := nil;
glWeightivARB      := nil;
glWeightfvARB      := nil;
glWeightdvARB      := nil;
glWeightubvARB     := nil;
glWeightusvARB     := nil;
glWeightuivARB     := nil;
glWeightPointerARB := nil;
glVertexBlendARB   := nil;
// GL_ARB_vertex_buffer_object
glBindBufferARB           := nil;
glDeleteBuffersARB        := nil;
glGenBuffersARB           := nil;
glIsBufferARB             := nil;
glBufferDataARB           := nil;
glBufferSubDataARB        := nil;
glGetBufferSubDataARB     := nil;
glMapBufferARB            := nil;
glUnmapBufferARB          := nil;
glGetBufferParameterivARB := nil;
glGetBufferPointervARB    := nil;
// ARB less version fo GL 1.5
glBindBuffer           := nil;
glDeleteBuffers        := nil;
glGenBuffers           := nil;
glIsBuffer             := nil;
glBufferData           := nil;
glBufferSubData        := nil;
glGetBufferSubData     := nil;
glMapBuffer            := nil;
glUnmapBuffer          := nil;
glGetBufferParameteriv := nil;
glGetBufferPointerv    := nil;
// GL_ARB_vertex_program
glVertexAttrib1dARB             := nil;
glVertexAttrib1dvARB            := nil;
glVertexAttrib1fARB             := nil;
glVertexAttrib1fvARB            := nil;
glVertexAttrib1sARB             := nil;
glVertexAttrib1svARB            := nil;
glVertexAttrib2dARB             := nil;
glVertexAttrib2dvARB            := nil;
glVertexAttrib2fARB             := nil;
glVertexAttrib2fvARB            := nil;
glVertexAttrib2sARB             := nil;
glVertexAttrib2svARB            := nil;
glVertexAttrib3dARB             := nil;
glVertexAttrib3dvARB            := nil;
glVertexAttrib3fARB             := nil;
glVertexAttrib3fvARB            := nil;
glVertexAttrib3sARB             := nil;
glVertexAttrib3svARB            := nil;
glVertexAttrib4NbvARB           := nil;
glVertexAttrib4NivARB           := nil;
glVertexAttrib4NsvARB           := nil;
glVertexAttrib4NubARB           := nil;
glVertexAttrib4NubvARB          := nil;
glVertexAttrib4NuivARB          := nil;
glVertexAttrib4NusvARB          := nil;
glVertexAttrib4bvARB            := nil;
glVertexAttrib4dARB             := nil;
glVertexAttrib4dvARB            := nil;
glVertexAttrib4fARB             := nil;
glVertexAttrib4fvARB            := nil;
glVertexAttrib4ivARB            := nil;
glVertexAttrib4sARB             := nil;
glVertexAttrib4svARB            := nil;
glVertexAttrib4ubvARB           := nil;
glVertexAttrib4uivARB           := nil;
glVertexAttrib4usvARB           := nil;
glVertexAttribPointerARB        := nil;
glEnableVertexAttribArrayARB    := nil;
glDisableVertexAttribArrayARB   := nil;
glProgramStringARB              := nil;
glBindProgramARB                := nil;
glDeleteProgramsARB             := nil;
glGenProgramsARB                := nil;
glProgramEnvParameter4dARB      := nil;
glProgramEnvParameter4dvARB     := nil;
glProgramEnvParameter4fARB      := nil;
glProgramEnvParameter4fvARB     := nil;
glProgramLocalParameter4dARB    := nil;
glProgramLocalParameter4dvARB   := nil;
glProgramLocalParameter4fARB    := nil;
glProgramLocalParameter4fvARB   := nil;
glGetProgramEnvParameterdvARB   := nil;
glGetProgramEnvParameterfvARB   := nil;
glGetProgramLocalParameterdvARB := nil;
glGetProgramLocalParameterfvARB := nil;
glGetProgramivARB               := nil;
glGetProgramStringARB           := nil;
glGetVertexAttribdvARB          := nil;
glGetVertexAttribfvARB          := nil;
glGetVertexAttribivARB          := nil;
glGetVertexAttribPointervARB    := nil;
glIsProgramARB                  := nil;
// GL_ARB_window_pos
glWindowPos2dARB  := nil;
glWindowPos2dvARB := nil;
glWindowPos2fARB  := nil;
glWindowPos2fvARB := nil;
glWindowPos2iARB  := nil;
glWindowPos2ivARB := nil;
glWindowPos2sARB  := nil;
glWindowPos2svARB := nil;
glWindowPos3dARB  := nil;
glWindowPos3dvARB := nil;
glWindowPos3fARB  := nil;
glWindowPos3fvARB := nil;
glWindowPos3iARB  := nil;
glWindowPos3ivARB := nil;
glWindowPos3sARB  := nil;
glWindowPos3svARB := nil;
// GL_ARB_occlusion_query
glGenQueriesARB              := nil;
glDeleteQueriesARB           := nil;
glIsQueryARB                 := nil;
glBeginQueryARB              := nil;
glEndQueryARB                := nil;
glGetQueryivARB              := nil;
glGetQueryObjectivARB        := nil;
glGetQueryObjectuivARB       := nil;

// ARB less version for GL 1.5
glGenQueries              := nil;
glDeleteQueries           := nil;
glIsQuery                 := nil;
glBeginQuery              := nil;
glEndQuery                := nil;
glGetQueryiv              := nil;
glGetQueryObjectiv        := nil;
glGetQueryObjectuiv       := nil;
// GL_ATI_draw_buffers
glDrawBuffersATI := nil;
// GL_ATI_element_array
glElementPointerATI        := nil;
glDrawElementArrayATI      := nil;
glDrawRangeElementArrayATI := nil;
// GL_ATI_envmap_bumpmap
glTexBumpParameterivATI    := nil;
glTexBumpParameterfvATI    := nil;
glGetTexBumpParameterivATI := nil;
glGetTexBumpParameterfvATI := nil;
// GL_ATI_fragment_shader
glGenFragmentShadersATI        := nil;
glBindFragmentShaderATI        := nil;
glDeleteFragmentShaderATI      := nil;
glBeginFragmentShaderATI       := nil;
glEndFragmentShaderATI         := nil;
glPassTexCoordATI              := nil;
glSampleMapATI                 := nil;
glColorFragmentOp1ATI          := nil;
glColorFragmentOp2ATI          := nil;
glColorFragmentOp3ATI          := nil;
glAlphaFragmentOp1ATI          := nil;
glAlphaFragmentOp2ATI          := nil;
glAlphaFragmentOp3ATI          := nil;
glSetFragmentShaderConstantATI := nil;
// GL_ATI_map_object_buffer
glMapObjectBufferATI   := nil;
glUnmapObjectBufferATI := nil;
// GL_ATI_pn_triangles
glPNTrianglesiATI := nil;
glPNTrianglesfATI := nil;
// GL_ATI_separate_stencil
glStencilOpSeparateATI   := nil;
glStencilFuncSeparateATI := nil;
// GL_ATI_vertex_array_object
glNewObjectBufferATI         := nil;
glIsObjectBufferATI          := nil;
glUpdateObjectBufferATI      := nil;
glGetObjectBufferfvATI       := nil;
glGetObjectBufferivATI       := nil;
glFreeObjectBufferATI        := nil;
glArrayObjectATI             := nil;
glGetArrayObjectfvATI        := nil;
glGetArrayObjectivATI        := nil;
glVariantArrayObjectATI      := nil;
glGetVariantArrayObjectfvATI := nil;
glGetVariantArrayObjectivATI := nil;
// GL_ATI_vertex_attrib_array_object
glVertexAttribArrayObjectATI      := nil;
glGetVertexAttribArrayObjectfvATI := nil;
glGetVertexAttribArrayObjectivATI := nil;
// GL_ATI_vertex_streams
glVertexStream1sATI           := nil;
glVertexStream1svATI          := nil;
glVertexStream1iATI           := nil;
glVertexStream1ivATI          := nil;
glVertexStream1fATI           := nil;
glVertexStream1fvATI          := nil;
glVertexStream1dATI           := nil;
glVertexStream1dvATI          := nil;
glVertexStream2sATI           := nil;
glVertexStream2svATI          := nil;
glVertexStream2iATI           := nil;
glVertexStream2ivATI          := nil;
glVertexStream2fATI           := nil;
glVertexStream2fvATI          := nil;
glVertexStream2dATI           := nil;
glVertexStream2dvATI          := nil;
glVertexStream3sATI           := nil;
glVertexStream3svATI          := nil;
glVertexStream3iATI           := nil;
glVertexStream3ivATI          := nil;
glVertexStream3fATI           := nil;
glVertexStream3fvATI          := nil;
glVertexStream3dATI           := nil;
glVertexStream3dvATI          := nil;
glVertexStream4sATI           := nil;
glVertexStream4svATI          := nil;
glVertexStream4iATI           := nil;
glVertexStream4ivATI          := nil;
glVertexStream4fATI           := nil;
glVertexStream4fvATI          := nil;
glVertexStream4dATI           := nil;
glVertexStream4dvATI          := nil;
glNormalStream3bATI           := nil;
glNormalStream3bvATI          := nil;
glNormalStream3sATI           := nil;
glNormalStream3svATI          := nil;
glNormalStream3iATI           := nil;
glNormalStream3ivATI          := nil;
glNormalStream3fATI           := nil;
glNormalStream3fvATI          := nil;
glNormalStream3dATI           := nil;
glNormalStream3dvATI          := nil;
glClientActiveVertexStreamATI := nil;
glVertexBlendEnviATI          := nil;
glVertexBlendEnvfATI          := nil;
// GL_EXT_blend_color
glBlendColorEXT := nil;
// GL_EXT_blend_func_separate
glBlendFuncSeparateEXT := nil;
// GL_EXT_blend_minmax
glBlendEquationEXT := nil;
// GL_EXT_color_subtable
glColorSubTableEXT     := nil;
glCopyColorSubTableEXT := nil;
// GL_EXT_compiled_vertex_array
glLockArraysEXT   := nil;
glUnlockArraysEXT := nil;
// GL_EXT_convolution
glConvolutionFilter1DEXT       := nil;
glConvolutionFilter2DEXT       := nil;
glConvolutionParameterfEXT     := nil;
glConvolutionParameterfvEXT    := nil;
glConvolutionParameteriEXT     := nil;
glConvolutionParameterivEXT    := nil;
glCopyConvolutionFilter1DEXT   := nil;
glCopyConvolutionFilter2DEXT   := nil;
glGetConvolutionFilterEXT      := nil;
glGetConvolutionParameterfvEXT := nil;
glGetConvolutionParameterivEXT := nil;
glGetSeparableFilterEXT        := nil;
glSeparableFilter2DEXT         := nil;
// GL_EXT_coordinate_frame
glTangent3bEXT       := nil;
glTangent3bvEXT      := nil;
glTangent3dEXT       := nil;
glTangent3dvEXT      := nil;
glTangent3fEXT       := nil;
glTangent3fvEXT      := nil;
glTangent3iEXT       := nil;
glTangent3ivEXT      := nil;
glTangent3sEXT       := nil;
glTangent3svEXT      := nil;
glBinormal3bEXT      := nil;
glBinormal3bvEXT     := nil;
glBinormal3dEXT      := nil;
glBinormal3dvEXT     := nil;
glBinormal3fEXT      := nil;
glBinormal3fvEXT     := nil;
glBinormal3iEXT      := nil;
glBinormal3ivEXT     := nil;
glBinormal3sEXT      := nil;
glBinormal3svEXT     := nil;
glTangentPointerEXT  := nil;
glBinormalPointerEXT := nil;
// GL_EXT_copy_texture
glCopyTexImage1DEXT    := nil;
glCopyTexImage2DEXT    := nil;
glCopyTexSubImage1DEXT := nil;
glCopyTexSubImage2DEXT := nil;
glCopyTexSubImage3DEXT := nil;
// GL_EXT_cull_vertex
glCullParameterdvEXT := nil;
glCullParameterfvEXT := nil;
// GL_EXT_draw_range_elements
glDrawRangeElementsEXT := nil;
// GL_EXT_fog_coord
glFogCoordfEXT       := nil;
glFogCoordfvEXT      := nil;
glFogCoorddEXT       := nil;
glFogCoorddvEXT      := nil;
glFogCoordPointerEXT := nil;
// GL_EXT_histogram
glGetHistogramEXT            := nil;
glGetHistogramParameterfvEXT := nil;
glGetHistogramParameterivEXT := nil;
glGetMinmaxEXT               := nil;
glGetMinmaxParameterfvEXT    := nil;
glGetMinmaxParameterivEXT    := nil;
glHistogramEXT               := nil;
glMinmaxEXT                  := nil;
glResetHistogramEXT          := nil;
glResetMinmaxEXT             := nil;
// GL_EXT_index_func
glIndexFuncEXT := nil;
// GL_EXT_index_material
glIndexMaterialEXT := nil;
// GL_EXT_light_texture
glApplyTextureEXT := nil;
glTextureLightEXT := nil;
glTextureMaterialEXT := nil;
// GL_EXT_multi_draw_arrays
glMultiDrawArraysEXT := nil;
glMultiDrawElementsEXT := nil;
// GL_EXT_multisample
glSampleMaskEXT := nil;
glSamplePatternEXT := nil;
// GL_EXT_paletted_texture
glColorTableEXT := nil;
glGetColorTableEXT := nil;
glGetColorTableParameterivEXT := nil;
glGetColorTableParameterfvEXT := nil;

  // GL_EXT_pixel_transform
  glPixelTransformParameteriEXT := nil;
  glPixelTransformParameterfEXT := nil;
  glPixelTransformParameterivEXT := nil;
  glPixelTransformParameterfvEXT := nil;

  // GL_EXT_point_parameters
  glPointParameterfEXT := nil;
  glPointParameterfvEXT := nil;

  // GL_EXT_polygon_offset
  glPolygonOffsetEXT := nil;

  // GL_EXT_secondary_color
  glSecondaryColor3bEXT := nil;
  glSecondaryColor3bvEXT := nil;
  glSecondaryColor3dEXT := nil;
  glSecondaryColor3dvEXT := nil;
  glSecondaryColor3fEXT := nil;
  glSecondaryColor3fvEXT := nil;
  glSecondaryColor3iEXT := nil;
  glSecondaryColor3ivEXT := nil;
  glSecondaryColor3sEXT := nil;
  glSecondaryColor3svEXT := nil;
  glSecondaryColor3ubEXT := nil;
  glSecondaryColor3ubvEXT := nil;
  glSecondaryColor3uiEXT := nil;
  glSecondaryColor3uivEXT := nil;
  glSecondaryColor3usEXT := nil;
  glSecondaryColor3usvEXT := nil;
  glSecondaryColorPointerEXT := nil;

  // GL_EXT_stencil_two_side
  glActiveStencilFaceEXT := nil;

  // GL_EXT_subtexture
  glTexSubImage1DEXT := nil;
  glTexSubImage2DEXT := nil;

  // GL_EXT_texture3D
  glTexImage3DEXT := nil;
  glTexSubImage3DEXT := nil;

  // GL_EXT_texture_object
  glAreTexturesResidentEXT := nil;
  glBindTextureEXT := nil;
  glDeleteTexturesEXT := nil;
  glGenTexturesEXT := nil;
  glIsTextureEXT := nil;
  glPrioritizeTexturesEXT := nil;

  // GL_EXT_texture_perturb_normal
  glTextureNormalEXT := nil;

  // GL_EXT_vertex_array
  glArrayElementEXT := nil;
  glColorPointerEXT := nil;
  glDrawArraysEXT := nil;
  glEdgeFlagPointerEXT := nil;
  glGetPointervEXT := nil;
  glIndexPointerEXT := nil;
  glNormalPointerEXT := nil;
  glTexCoordPointerEXT := nil;
  glVertexPointerEXT := nil;

  // GL_EXT_vertex_shader
  glBeginVertexShaderEXT := nil;
  glEndVertexShaderEXT := nil;
  glBindVertexShaderEXT := nil;
  glGenVertexShadersEXT := nil;
  glDeleteVertexShaderEXT := nil;
  glShaderOp1EXT := nil;
  glShaderOp2EXT := nil;
  glShaderOp3EXT := nil;
  glSwizzleEXT := nil;
  glWriteMaskEXT := nil;
  glInsertComponentEXT := nil;
  glExtractComponentEXT := nil;
  glGenSymbolsEXT := nil;
  glSetInvariantEXT := nil;
  glSetLocalConstantEXT := nil;
  glVariantbvEXT := nil;
  glVariantsvEXT := nil;
  glVariantivEXT := nil;
  glVariantfvEXT := nil;
  glVariantdvEXT := nil;
  glVariantubvEXT := nil;
  glVariantusvEXT := nil;
  glVariantuivEXT := nil;
  glVariantPointerEXT := nil;
  glEnableVariantClientStateEXT := nil;
  glDisableVariantClientStateEXT := nil;
  glBindLightParameterEXT := nil;
  glBindMaterialParameterEXT := nil;
  glBindTexGenParameterEXT := nil;
  glBindTextureUnitParameterEXT := nil;
  glBindParameterEXT := nil;
  glIsVariantEnabledEXT := nil;
  glGetVariantBooleanvEXT := nil;
  glGetVariantIntegervEXT := nil;
  glGetVariantFloatvEXT := nil;
  glGetVariantPointervEXT := nil;
  glGetInvariantBooleanvEXT := nil;
  glGetInvariantIntegervEXT := nil;
  glGetInvariantFloatvEXT := nil;
  glGetLocalConstantBooleanvEXT := nil;
  glGetLocalConstantIntegervEXT := nil;
  glGetLocalConstantFloatvEXT := nil;

  // GL_EXT_vertex_weighting
  glVertexWeightfEXT := nil;
  glVertexWeightfvEXT := nil;
  glVertexWeightPointerEXT := nil;

  // GL_HP_image_transform
  glImageTransformParameteriHP := nil;
  glImageTransformParameterfHP := nil;
  glImageTransformParameterivHP := nil;
  glImageTransformParameterfvHP := nil;
  glGetImageTransformParameterivHP := nil;
  glGetImageTransformParameterfvHP := nil;

  // GL_IBM_multimode_draw_arrays
  glMultiModeDrawArraysIBM := nil;
  glMultiModeDrawElementsIBM := nil;

  // GL_IBM_vertex_array_lists
  glColorPointerListIBM := nil;
  glSecondaryColorPointerListIBM := nil;
  glEdgeFlagPointerListIBM := nil;
  glFogCoordPointerListIBM := nil;
  glIndexPointerListIBM := nil;
  glNormalPointerListIBM := nil;
  glTexCoordPointerListIBM := nil;
  glVertexPointerListIBM := nil;

  // GL_INGR_blend_func_separate
  glBlendFuncSeparateINGR := nil;

  // GL_INTEL_parallel_arrays
  glVertexPointervINTEL := nil;
  glNormalPointervINTEL := nil;
  glColorPointervINTEL := nil;
  glTexCoordPointervINTEL := nil;

  // GL_MESA_resize_buffers
  glResizeBuffersMESA := nil;

  // GL_MESA_window_pos
  glWindowPos2dMESA := nil;
  glWindowPos2dvMESA := nil;
  glWindowPos2fMESA := nil;
  glWindowPos2fvMESA := nil;
  glWindowPos2iMESA := nil;
  glWindowPos2ivMESA := nil;
  glWindowPos2sMESA := nil;
  glWindowPos2svMESA := nil;
  glWindowPos3dMESA := nil;
  glWindowPos3dvMESA := nil;
  glWindowPos3fMESA := nil;
  glWindowPos3fvMESA := nil;
  glWindowPos3iMESA := nil;
  glWindowPos3ivMESA := nil;
  glWindowPos3sMESA := nil;
  glWindowPos3svMESA := nil;
  glWindowPos4dMESA := nil;
  glWindowPos4dvMESA := nil;
  glWindowPos4fMESA := nil;
  glWindowPos4fvMESA := nil;
  glWindowPos4iMESA := nil;
  glWindowPos4ivMESA := nil;
  glWindowPos4sMESA := nil;
  glWindowPos4svMESA := nil;

  // GL_NV_evaluators
  glMapControlPointsNV := nil;
  glMapParameterivNV := nil;
  glMapParameterfvNV := nil;
  glGetMapControlPointsNV := nil;
  glGetMapParameterivNV := nil;
  glGetMapParameterfvNV := nil;
  glGetMapAttribParameterivNV := nil;
  glGetMapAttribParameterfvNV := nil;
  glEvalMapsNV := nil;

  // GL_NV_fence
  glDeleteFencesNV := nil;
  glGenFencesNV := nil;
  glIsFenceNV := nil;
  glTestFenceNV := nil;
  glGetFenceivNV := nil;
  glFinishFenceNV := nil;
  glSetFenceNV := nil;

  // GL_NV_fragment_program
  glProgramNamedParameter4fNV := nil;
  glProgramNamedParameter4dNV := nil;
  glProgramNamedParameter4fvNV := nil;
  glProgramNamedParameter4dvNV := nil;
  glGetProgramNamedParameterfvNV := nil;
  glGetProgramNamedParameterdvNV := nil;

  // GL_NV_half_float
  glVertex2hNV := nil;
  glVertex2hvNV := nil;
  glVertex3hNV := nil;
  glVertex3hvNV := nil;
  glVertex4hNV := nil;
  glVertex4hvNV := nil;
  glNormal3hNV := nil;
  glNormal3hvNV := nil;
  glColor3hNV := nil;
  glColor3hvNV := nil;
  glColor4hNV := nil;
  glColor4hvNV := nil;
  glTexCoord1hNV := nil;
  glTexCoord1hvNV := nil;
  glTexCoord2hNV := nil;
  glTexCoord2hvNV := nil;
  glTexCoord3hNV := nil;
  glTexCoord3hvNV := nil;
  glTexCoord4hNV := nil;
  glTexCoord4hvNV := nil;
  glMultiTexCoord1hNV := nil;
  glMultiTexCoord1hvNV := nil;
  glMultiTexCoord2hNV := nil;
  glMultiTexCoord2hvNV := nil;
  glMultiTexCoord3hNV := nil;
  glMultiTexCoord3hvNV := nil;
  glMultiTexCoord4hNV := nil;
  glMultiTexCoord4hvNV := nil;
  glFogCoordhNV := nil;
  glFogCoordhvNV := nil;
  glSecondaryColor3hNV := nil;
  glSecondaryColor3hvNV := nil;
  glVertexWeighthNV := nil;
  glVertexWeighthvNV := nil;
  glVertexAttrib1hNV := nil;
  glVertexAttrib1hvNV := nil;
  glVertexAttrib2hNV := nil;
  glVertexAttrib2hvNV := nil;
  glVertexAttrib3hNV := nil;
  glVertexAttrib3hvNV := nil;
  glVertexAttrib4hNV := nil;
  glVertexAttrib4hvNV := nil;
  glVertexAttribs1hvNV := nil;
  glVertexAttribs2hvNV := nil;
  glVertexAttribs3hvNV := nil;
  glVertexAttribs4hvNV := nil;

  // GL_NV_occlusion_query
  glGenOcclusionQueriesNV := nil;
  glDeleteOcclusionQueriesNV := nil;
  glIsOcclusionQueryNV := nil;
  glBeginOcclusionQueryNV := nil;
  glEndOcclusionQueryNV := nil;
  glGetOcclusionQueryivNV := nil;
  glGetOcclusionQueryuivNV := nil;

  // GL_NV_pixel_data_range
  glPixelDataRangeNV := nil;
  glFlushPixelDataRangeNV := nil;

  // GL_NV_point_sprite
  glPointParameteriNV := nil;
  glPointParameterivNV := nil;

  // GL_NV_primitive_restart
  glPrimitiveRestartNV := nil;
  glPrimitiveRestartIndexNV := nil;

  // GL_NV_register_combiners
  glCombinerParameterfvNV := nil;
  glCombinerParameterfNV := nil;
  glCombinerParameterivNV := nil;
  glCombinerParameteriNV := nil;
  glCombinerInputNV := nil;
  glCombinerOutputNV := nil;
  glFinalCombinerInputNV := nil;
  glGetCombinerInputParameterfvNV := nil;
  glGetCombinerInputParameterivNV := nil;
  glGetCombinerOutputParameterfvNV := nil;
  glGetCombinerOutputParameterivNV := nil;
  glGetFinalCombinerInputParameterfvNV := nil;
  glGetFinalCombinerInputParameterivNV := nil;

  // GL_NV_register_combiners2
  glCombinerStageParameterfvNV := nil;
  glGetCombinerStageParameterfvNV := nil;

  // GL_NV_vertex_array_range
  glFlushVertexArrayRangeNV := nil;
  glVertexArrayRangeNV := nil;

  // GL_NV_vertex_program
  glAreProgramsResidentNV := nil;
  glBindProgramNV := nil;
  glDeleteProgramsNV := nil;
  glExecuteProgramNV := nil;
  glGenProgramsNV := nil;
  glGetProgramParameterdvNV := nil;
  glGetProgramParameterfvNV := nil;
  glGetProgramivNV := nil;
  glGetProgramStringNV := nil;
  glGetTrackMatrixivNV := nil;
  glGetVertexAttribdvNV := nil;
  glGetVertexAttribfvNV := nil;
  glGetVertexAttribivNV := nil;
  glGetVertexAttribPointervNV := nil;
  glIsProgramNV := nil;
  glLoadProgramNV := nil;
  glProgramParameter4dNV := nil;
  glProgramParameter4dvNV := nil;
  glProgramParameter4fNV := nil;
  glProgramParameter4fvNV := nil;
  glProgramParameters4dvNV := nil;
  glProgramParameters4fvNV := nil;
  glRequestResidentProgramsNV := nil;
  glTrackMatrixNV := nil;
  glVertexAttribPointerNV := nil;
  glVertexAttrib1dNV := nil;
  glVertexAttrib1dvNV := nil;
  glVertexAttrib1fNV := nil;
  glVertexAttrib1fvNV := nil;
  glVertexAttrib1sNV := nil;
  glVertexAttrib1svNV := nil;
  glVertexAttrib2dNV := nil;
  glVertexAttrib2dvNV := nil;
  glVertexAttrib2fNV := nil;
  glVertexAttrib2fvNV := nil;
  glVertexAttrib2sNV := nil;
  glVertexAttrib2svNV := nil;
  glVertexAttrib3dNV := nil;
  glVertexAttrib3dvNV := nil;
  glVertexAttrib3fNV := nil;
  glVertexAttrib3fvNV := nil;
  glVertexAttrib3sNV := nil;
  glVertexAttrib3svNV := nil;
  glVertexAttrib4dNV := nil;
  glVertexAttrib4dvNV := nil;
  glVertexAttrib4fNV := nil;
  glVertexAttrib4fvNV := nil;
  glVertexAttrib4sNV := nil;
  glVertexAttrib4svNV := nil;
  glVertexAttrib4ubNV := nil;
  glVertexAttrib4ubvNV := nil;
  glVertexAttribs1dvNV := nil;
  glVertexAttribs1fvNV := nil;
  glVertexAttribs1svNV := nil;
  glVertexAttribs2dvNV := nil;
  glVertexAttribs2fvNV := nil;
  glVertexAttribs2svNV := nil;
  glVertexAttribs3dvNV := nil;
  glVertexAttribs3fvNV := nil;
  glVertexAttribs3svNV := nil;
  glVertexAttribs4dvNV := nil;
  glVertexAttribs4fvNV := nil;
  glVertexAttribs4svNV := nil;
  glVertexAttribs4ubvNV := nil;

  // GL_PGI_misc_hints
  glHintPGI := nil;

  // GL_SGIS_detail_texture
  glDetailTexFuncSGIS := nil;
  glGetDetailTexFuncSGIS := nil;

  // GL_SGIS_fog_function
  glFogFuncSGIS := nil;
  glGetFogFuncSGIS := nil;

  // GL_SGIS_multisample
  glSampleMaskSGIS := nil;
  glSamplePatternSGIS := nil;

  // GL_SGIS_pixel_texture
  glPixelTexGenParameteriSGIS := nil;
  glPixelTexGenParameterivSGIS := nil;
  glPixelTexGenParameterfSGIS := nil;
  glPixelTexGenParameterfvSGIS := nil;
  glGetPixelTexGenParameterivSGIS := nil;
  glGetPixelTexGenParameterfvSGIS := nil;

  // GL_SGIS_point_parameters
  glPointParameterfSGIS := nil;
  glPointParameterfvSGIS := nil;

  // GL_SGIS_sharpen_texture
  glSharpenTexFuncSGIS := nil;
  glGetSharpenTexFuncSGIS := nil;

  // GL_SGIS_texture4D
  glTexImage4DSGIS := nil;
  glTexSubImage4DSGIS := nil;

  // GL_SGIS_texture_color_mask
  glTextureColorMaskSGIS := nil;

  // GL_SGIS_texture_filter4
  glGetTexFilterFuncSGIS := nil;
  glTexFilterFuncSGIS := nil;

  // GL_SGIX_async
  glAsyncMarkerSGIX := nil;
  glFinishAsyncSGIX := nil;
  glPollAsyncSGIX := nil;
  glGenAsyncMarkersSGIX := nil;
  glDeleteAsyncMarkersSGIX := nil;
  glIsAsyncMarkerSGIX := nil;

  // GL_SGIX_flush_raster
  glFlushRasterSGIX := nil;

  // GL_SGIX_fragment_lighting
  glFragmentColorMaterialSGIX := nil;
  glFragmentLightfSGIX := nil;
  glFragmentLightfvSGIX := nil;
  glFragmentLightiSGIX := nil;
  glFragmentLightivSGIX := nil;
  glFragmentLightModelfSGIX := nil;
  glFragmentLightModelfvSGIX := nil;
  glFragmentLightModeliSGIX := nil;
  glFragmentLightModelivSGIX := nil;
  glFragmentMaterialfSGIX := nil;
  glFragmentMaterialfvSGIX := nil;
  glFragmentMaterialiSGIX := nil;
  glFragmentMaterialivSGIX := nil;
  glGetFragmentLightfvSGIX := nil;
  glGetFragmentLightivSGIX := nil;
  glGetFragmentMaterialfvSGIX := nil;
  glGetFragmentMaterialivSGIX := nil;
  glLightEnviSGIX := nil;

  // GL_SGIX_framezoom
  glFrameZoomSGIX := nil;

  // GL_SGIX_igloo_interface
  glIglooInterfaceSGIX := nil;

  // GL_SGIX_instruments
  glGetInstrumentsSGIX := nil;
  glInstrumentsBufferSGIX := nil;
  glPollInstrumentsSGIX := nil;
  glReadInstrumentsSGIX := nil;
  glStartInstrumentsSGIX := nil;
  glStopInstrumentsSGIX := nil;

  // GL_SGIX_list_priority
  glGetListParameterfvSGIX := nil;
  glGetListParameterivSGIX := nil;
  glListParameterfSGIX := nil;
  glListParameterfvSGIX := nil;
  glListParameteriSGIX := nil;
  glListParameterivSGIX := nil;

  // GL_SGIX_pixel_texture
  glPixelTexGenSGIX := nil;

  // GL_SGIX_polynomial_ffd
  glDeformationMap3dSGIX := nil;
  glDeformationMap3fSGIX := nil;
  glDeformSGIX := nil;
  glLoadIdentityDeformationMapSGIX := nil;

  // GL_SGIX_reference_plane
  glReferencePlaneSGIX := nil;

  // GL_SGIX_sprite
  glSpriteParameterfSGIX := nil;
  glSpriteParameterfvSGIX := nil;
  glSpriteParameteriSGIX := nil;
  glSpriteParameterivSGIX := nil;

  // GL_SGIX_tag_sample_buffer
  glTagSampleBufferSGIX := nil;

  // GL_SGI_color_table
  glColorTableSGI := nil;
  glColorTableParameterfvSGI := nil;
  glColorTableParameterivSGI := nil;
  glCopyColorTableSGI := nil;
  glGetColorTableSGI := nil;
  glGetColorTableParameterfvSGI := nil;
  glGetColorTableParameterivSGI := nil;

  // GL_SUNX_constant_data
  glFinishTextureSUNX := nil;

  // GL_SUN_global_alpha
  glGlobalAlphaFactorbSUN := nil;
  glGlobalAlphaFactorsSUN := nil;
  glGlobalAlphaFactoriSUN := nil;
  glGlobalAlphaFactorfSUN := nil;
  glGlobalAlphaFactordSUN := nil;
  glGlobalAlphaFactorubSUN := nil;
  glGlobalAlphaFactorusSUN := nil;
  glGlobalAlphaFactoruiSUN := nil;

  // GL_SUN_mesh_array
  glDrawMeshArraysSUN := nil;

  // GL_SUN_triangle_list
  glReplacementCodeuiSUN := nil;
  glReplacementCodeusSUN := nil;
  glReplacementCodeubSUN := nil;
  glReplacementCodeuivSUN := nil;
  glReplacementCodeusvSUN := nil;
  glReplacementCodeubvSUN := nil;
  glReplacementCodePointerSUN := nil;

  // GL_SUN_vertex
  glColor4ubVertex2fSUN := nil;
  glColor4ubVertex2fvSUN := nil;
  glColor4ubVertex3fSUN := nil;
  glColor4ubVertex3fvSUN := nil;
  glColor3fVertex3fSUN := nil;
  glColor3fVertex3fvSUN := nil;
  glNormal3fVertex3fSUN := nil;
  glNormal3fVertex3fvSUN := nil;
  glColor4fNormal3fVertex3fSUN := nil;
  glColor4fNormal3fVertex3fvSUN := nil;
  glTexCoord2fVertex3fSUN := nil;
  glTexCoord2fVertex3fvSUN := nil;
  glTexCoord4fVertex4fSUN := nil;
  glTexCoord4fVertex4fvSUN := nil;
  glTexCoord2fColor4ubVertex3fSUN := nil;
  glTexCoord2fColor4ubVertex3fvSUN := nil;
  glTexCoord2fColor3fVertex3fSUN := nil;
  glTexCoord2fColor3fVertex3fvSUN := nil;
  glTexCoord2fNormal3fVertex3fSUN := nil;
  glTexCoord2fNormal3fVertex3fvSUN := nil;
  glTexCoord2fColor4fNormal3fVertex3fSUN := nil;
  glTexCoord2fColor4fNormal3fVertex3fvSUN := nil;
  glTexCoord4fColor4fNormal3fVertex4fSUN := nil;
  glTexCoord4fColor4fNormal3fVertex4fvSUN := nil;
  glReplacementCodeuiVertex3fSUN := nil;
  glReplacementCodeuiVertex3fvSUN := nil;
  glReplacementCodeuiColor4ubVertex3fSUN := nil;
  glReplacementCodeuiColor4ubVertex3fvSUN := nil;
  glReplacementCodeuiColor3fVertex3fSUN := nil;
  glReplacementCodeuiColor3fVertex3fvSUN := nil;
  glReplacementCodeuiNormal3fVertex3fSUN := nil;
  glReplacementCodeuiNormal3fVertex3fvSUN := nil;
  glReplacementCodeuiColor4fNormal3fVertex3fSUN := nil;
  glReplacementCodeuiColor4fNormal3fVertex3fvSUN := nil;
  glReplacementCodeuiTexCoord2fVertex3fSUN := nil;
  glReplacementCodeuiTexCoord2fVertex3fvSUN := nil;
  glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN := nil;
  glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN := nil;
  glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN := nil;
  glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN := nil;

  // WGL_ARB_buffer_region
  wglCreateBufferRegionARB := nil;
  wglDeleteBufferRegionARB := nil;
  wglSaveBufferRegionARB := nil;
  wglRestoreBufferRegionARB := nil;

  // WGL_ARB_extensions_string
  wglGetExtensionsStringARB := nil;

  // WGL_ARB_make_current_read
  wglMakeContextCurrentARB := nil;
  wglGetCurrentReadDCARB := nil;

  // WGL_ARB_pbuffer
  wglCreatePbufferARB := nil;
  wglGetPbufferDCARB := nil;
  wglReleasePbufferDCARB := nil;
  wglDestroyPbufferARB := nil;
  wglQueryPbufferARB := nil;

  // WGL_ARB_pixel_format
  wglGetPixelFormatAttribivARB := nil;
  wglGetPixelFormatAttribfvARB := nil;
  wglChoosePixelFormatARB := nil;

  // WGL_ARB_render_texture
  wglBindTexImageARB := nil;
  wglReleaseTexImageARB := nil;
  wglSetPbufferAttribARB := nil;

  // WGL_EXT_display_color_table
  wglCreateDisplayColorTableEXT := nil;
  wglLoadDisplayColorTableEXT := nil;
  wglBindDisplayColorTableEXT := nil;
  wglDestroyDisplayColorTableEXT := nil;

  // WGL_EXT_extensions_string
  wglGetExtensionsStringEXT := nil;

  // WGL_EXT_make_current_read
  wglMakeContextCurrentEXT := nil;
  wglGetCurrentReadDCEXT := nil;

  // WGL_EXT_pbuffer
  wglCreatePbufferEXT := nil;
  wglGetPbufferDCEXT := nil;
  wglReleasePbufferDCEXT := nil;
  wglDestroyPbufferEXT := nil;
  wglQueryPbufferEXT := nil;

  // WGL_EXT_pixel_format
  wglGetPixelFormatAttribivEXT := nil;
  wglGetPixelFormatAttribfvEXT := nil;
  wglChoosePixelFormatEXT := nil;

  // WGL_EXT_swap_control
  wglSwapIntervalEXT := nil;
  wglGetSwapIntervalEXT := nil;

  // WGL_I3D_digital_video_control
  wglGetDigitalVideoParametersI3D := nil;
  wglSetDigitalVideoParametersI3D := nil;

  // WGL_I3D_gamma
  wglGetGammaTableParametersI3D := nil;
  wglSetGammaTableParametersI3D := nil;
  wglGetGammaTableI3D := nil;
  wglSetGammaTableI3D := nil;

  // WGL_I3D_genlock
  wglEnableGenlockI3D := nil;
  wglDisableGenlockI3D := nil;
  wglIsEnabledGenlockI3D := nil;
  wglGenlockSourceI3D := nil;
  wglGetGenlockSourceI3D := nil;
  wglGenlockSourceEdgeI3D := nil;
  wglGetGenlockSourceEdgeI3D := nil;
  wglGenlockSampleRateI3D := nil;
  wglGetGenlockSampleRateI3D := nil;
  wglGenlockSourceDelayI3D := nil;
  wglGetGenlockSourceDelayI3D := nil;
  wglQueryGenlockMaxSourceDelayI3D := nil;

  // WGL_I3D_image_buffer
  wglCreateImageBufferI3D := nil;
  wglDestroyImageBufferI3D := nil;
  wglAssociateImageBufferEventsI3D := nil;
  wglReleaseImageBufferEventsI3D := nil;

  // WGL_I3D_swap_frame_lock
  wglEnableFrameLockI3D := nil;
  wglDisableFrameLockI3D := nil;
  wglIsEnabledFrameLockI3D := nil;
  wglQueryFrameLockMasterI3D := nil;

  // WGL_I3D_swap_frame_usage
  wglGetFrameUsageI3D := nil;
  wglBeginFrameTrackingI3D := nil;
  wglEndFrameTrackingI3D := nil;
  wglQueryFrameTrackingI3D := nil;

  // WGL_NV_vertex_array_range
  wglAllocateMemoryNV := nil;
  wglFreeMemoryNV := nil;

  // WGL_OML_sync_control
  wglGetSyncValuesOML := nil;
  wglGetMscRateOML := nil;
  wglSwapBuffersMscOML := nil;
  wglSwapLayerBuffersMscOML := nil;
  wglWaitForMscOML := nil;
  wglWaitForSbcOML := nil;

  // WIN_draw_range_elements
  glDrawRangeElementsWIN := nil;

  // WIN_swap_hint
  glAddSwapHintRectWIN := nil;

  // GLU
  gluBeginCurve := nil;
  gluBeginPolygon := nil;
  gluBeginSurface := nil;
  gluBeginTrim := nil;
  gluBuild1DMipmaps := nil;
  gluBuild2DMipmaps := nil;
  gluCylinder := nil;
  gluDeleteNurbsRenderer := nil;
  gluDeleteQuadric := nil;
  gluDeleteTess := nil;
  gluDisk := nil;
  gluEndCurve := nil;
  gluEndPolygon := nil;
  gluEndSurface := nil;
  gluEndTrim := nil;
  gluErrorString := nil;
  gluGetNurbsProperty := nil;
  gluGetString := nil;
  gluGetTessProperty := nil;
  gluLoadSamplingMatrices := nil;
  gluLookAt := nil;
  gluNewNurbsRenderer := nil;
  gluNewQuadric := nil;
  gluNewTess := nil;
  gluNextContour := nil;
  gluNurbsCallback := nil;
  gluNurbsCurve := nil;
  gluNurbsProperty := nil;
  gluNurbsSurface := nil;
  gluOrtho2D := nil;
  gluPartialDisk := nil;
  gluPerspective := nil;
  gluPickMatrix := nil;
  gluProject := nil;
  gluPwlCurve := nil;
  gluQuadricCallback := nil;
  gluQuadricDrawStyle := nil;
  gluQuadricNormals := nil;
  gluQuadricOrientation := nil;
  gluQuadricTexture := nil;
  gluScaleImage := nil;
  gluSphere := nil;
  gluTessBeginContour := nil;
  gluTessBeginPolygon := nil;
  gluTessCallback := nil;
  gluTessEndContour := nil;
  gluTessEndPolygon := nil;
  gluTessNormal := nil;
  gluTessProperty := nil;
  gluTessVertex := nil;
  gluUnProject := nil;

//  wglGetProcAddress := nil;
  wglCopyContext := nil;
  wglCreateContext := nil;
  wglCreateLayerContext := nil;
  wglDeleteContext := nil;
  wglDescribeLayerPlane := nil;
  wglGetCurrentContext := nil;
  wglGetCurrentDC := nil;
  wglGetLayerPaletteEntries := nil;
  wglMakeCurrent := nil;
  wglRealizeLayerPalette := nil;
  wglSetLayerPaletteEntries := nil;
  wglShareLists := nil;
  wglSwapLayerBuffers := nil;
  wglSwapMultipleBuffers := nil;
  wglUseFontBitmapsA := nil;
  wglUseFontOutlinesA := nil;
  wglUseFontBitmapsW := nil;
  wglUseFontOutlinesW := nil;
  wglUseFontBitmaps := nil;
  wglUseFontOutlines := nil;
end;


// =============================================================================
//  ReadExtensions
// =============================================================================
procedure ReadExtensions;
begin
  if LibHandle <> 0 then
   begin
 // GL_VERSION_1_1 =============================================================
 glAccum                     := glProcedure('glAccum');
 glAlphaFunc                 := glProcedure('glAlphaFunc');
 glAreTexturesResident       := glProcedure('glAreTexturesResident');
 glArrayElement              := glProcedure('glArrayElement');
 glBegin                     := glProcedure('glBegin');
 glBindTexture               := glProcedure('glBindTexture');
 glBitmap                    := glProcedure('glBitmap');
 glBlendFunc                 := glProcedure('glBlendFunc');
 glCallList                  := glProcedure('glCallList');
 glCallLists                 := glProcedure('glCallLists');
 glClear                     := glProcedure('glClear');
 glClearAccum                := glProcedure('glClearAccum');
 glClearColor                := glProcedure('glClearColor');
 glClearDepth                := glProcedure('glClearDepth');
 glClearIndex                := glProcedure('glClearIndex');
 glClearStencil              := glProcedure('glClearStencil');
 glClipPlane                 := glProcedure('glClipPlane');
 glColor3b                   := glProcedure('glColor3b');
 glColor3bv                  := glProcedure('glColor3bv');
 glColor3d                   := glProcedure('glColor3d');
 glColor3dv                  := glProcedure('glColor3dv');
 glColor3f                   := glProcedure('glColor3f');
 glColor3fv                  := glProcedure('glColor3fv');
 glColor3i                   := glProcedure('glColor3i');
 glColor3iv                  := glProcedure('glColor3iv');
 glColor3s                   := glProcedure('glColor3s');
 glColor3sv                  := glProcedure('glColor3sv');
 glColor3ub                  := glProcedure('glColor3ub');
 glColor3ubv                 := glProcedure('glColor3ubv');
 glColor3ui                  := glProcedure('glColor3ui');
 glColor3uiv                 := glProcedure('glColor3uiv');
 glColor3us                  := glProcedure('glColor3us');
 glColor3usv                 := glProcedure('glColor3usv');
 glColor4b                   := glProcedure('glColor4b');
 glColor4bv                  := glProcedure('glColor4bv');
 glColor4d                   := glProcedure('glColor4d');
 glColor4dv                  := glProcedure('glColor4dv');
 glColor4f                   := glProcedure('glColor4f');
 glColor4fv                  := glProcedure('glColor4fv');
 glColor4i                   := glProcedure('glColor4i');
 glColor4iv                  := glProcedure('glColor4iv');
 glColor4s                   := glProcedure('glColor4s');
 glColor4sv                  := glProcedure('glColor4sv');
 glColor4ub                  := glProcedure('glColor4ub');
 glColor4ubv                 := glProcedure('glColor4ubv');
 glColor4ui                  := glProcedure('glColor4ui');
 glColor4uiv                 := glProcedure('glColor4uiv');
 glColor4us                  := glProcedure('glColor4us');
 glColor4usv                 := glProcedure('glColor4usv');
 glColorMask                 := glProcedure('glColorMask');
 glColorMaterial             := glProcedure('glColorMaterial');
 glColorPointer              := glProcedure('glColorPointer');
 glCopyPixels                := glProcedure('glCopyPixels');
 glCopyTexImage1D            := glProcedure('glCopyTexImage1D');
 glCopyTexImage2D            := glProcedure('glCopyTexImage2D');
 glCopyTexSubImage1D         := glProcedure('glCopyTexSubImage1D');
 glCopyTexSubImage2D         := glProcedure('glCopyTexSubImage2D');
 glCullFace                  := glProcedure('glCullFace');
 glDeleteLists               := glProcedure('glDeleteLists');
 glDeleteTextures            := glProcedure('glDeleteTextures');
 glDepthFunc                 := glProcedure('glDepthFunc');
 glDepthMask                 := glProcedure('glDepthMask');
 glDepthRange                := glProcedure('glDepthRange');
 glDisable                   := glProcedure('glDisable');
 glDisableClientState        := glProcedure('glDisableClientState');
 glDrawArrays                := glProcedure('glDrawArrays');
 glDrawBuffer                := glProcedure('glDrawBuffer');
 glDrawElements              := glProcedure('glDrawElements');
 glDrawPixels                := glProcedure('glDrawPixels');
 glEdgeFlag                  := glProcedure('glEdgeFlag');
 glEdgeFlagPointer           := glProcedure('glEdgeFlagPointer');
 glEdgeFlagv                 := glProcedure('glEdgeFlagv');
 glEnable                    := glProcedure('glEnable');
 glEnableClientState         := glProcedure('glEnableClientState');
 glEnd                       := glProcedure('glEnd');
 glEndList                   := glProcedure('glEndList');
 glEvalCoord1d               := glProcedure('glEvalCoord1d');
 glEvalCoord1dv              := glProcedure('glEvalCoord1dv');
 glEvalCoord1f               := glProcedure('glEvalCoord1f');
 glEvalCoord1fv              := glProcedure('glEvalCoord1fv');
 glEvalCoord2d               := glProcedure('glEvalCoord2d');
 glEvalCoord2dv              := glProcedure('glEvalCoord2dv');
 glEvalCoord2f               := glProcedure('glEvalCoord2f');
 glEvalCoord2fv              := glProcedure('glEvalCoord2fv');
 glEvalMesh1                 := glProcedure('glEvalMesh1');
 glEvalMesh2                 := glProcedure('glEvalMesh2');
 glEvalPoint1                := glProcedure('glEvalPoint1');
 glEvalPoint2                := glProcedure('glEvalPoint2');
 glFeedbackBuffer            := glProcedure('glFeedbackBuffer');
 glFinish                    := glProcedure('glFinish');
 glFlush                     := glProcedure('glFlush');
 glFogf                      := glProcedure('glFogf');
 glFogfv                     := glProcedure('glFogfv');
 glFogi                      := glProcedure('glFogi');
 glFogiv                     := glProcedure('glFogiv');
 glFrontFace                 := glProcedure('glFrontFace');
 glFrustum                   := glProcedure('glFrustum');
 glGenLists                  := glProcedure('glGenLists');
 glGenTextures               := glProcedure('glGenTextures');
 glGetBooleanv               := glProcedure('glGetBooleanv');
 glGetClipPlane              := glProcedure('glGetClipPlane');
 glGetDoublev                := glProcedure('glGetDoublev');
 glGetError                  := glProcedure('glGetError');
 glGetFloatv                 := glProcedure('glGetFloatv');
 glGetIntegerv               := glProcedure('glGetIntegerv');
 glGetLightfv                := glProcedure('glGetLightfv');
 glGetLightiv                := glProcedure('glGetLightiv');
 glGetMapdv                  := glProcedure('glGetMapdv');
 glGetMapfv                  := glProcedure('glGetMapfv');
 glGetMapiv                  := glProcedure('glGetMapiv');
 glGetMaterialfv             := glProcedure('glGetMaterialfv');
 glGetMaterialiv             := glProcedure('glGetMaterialiv');
 glGetPixelMapfv             := glProcedure('glGetPixelMapfv');
 glGetPixelMapuiv            := glProcedure('glGetPixelMapuiv');
 glGetPixelMapusv            := glProcedure('glGetPixelMapusv');
 glGetPointerv               := glProcedure('glGetPointerv');
 glGetPolygonStipple         := glProcedure('glGetPolygonStipple');
 glGetString                 := glProcedure('glGetString');
 glGetTexEnvfv               := glProcedure('glGetTexEnvfv');
 glGetTexEnviv               := glProcedure('glGetTexEnviv');
 glGetTexGendv               := glProcedure('glGetTexGendv');
 glGetTexGenfv               := glProcedure('glGetTexGenfv');
 glGetTexGeniv               := glProcedure('glGetTexGeniv');
 glGetTexImage               := glProcedure('glGetTexImage');
 glGetTexLevelParameterfv    := glProcedure('glGetTexLevelParameterfv');
 glGetTexLevelParameteriv    := glProcedure('glGetTexLevelParameteriv');
 glGetTexParameterfv         := glProcedure('glGetTexParameterfv');
 glGetTexParameteriv         := glProcedure('glGetTexParameteriv');
 glHint                      := glProcedure('glHint');
 glIndexMask                 := glProcedure('glIndexMask');
 glIndexPointer              := glProcedure('glIndexPointer');
 glIndexd                    := glProcedure('glIndexd');
 glIndexdv                   := glProcedure('glIndexdv');
 glIndexf                    := glProcedure('glIndexf');
 glIndexfv                   := glProcedure('glIndexfv');
 glIndexi                    := glProcedure('glIndexi');
 glIndexiv                   := glProcedure('glIndexiv');
 glIndexs                    := glProcedure('glIndexs');
 glIndexsv                   := glProcedure('glIndexsv');
 glIndexub                   := glProcedure('glIndexub');
 glIndexubv                  := glProcedure('glIndexubv');
 glInitNames                 := glProcedure('glInitNames');
 glInterleavedArrays         := glProcedure('glInterleavedArrays');
 glIsEnabled                 := glProcedure('glIsEnabled');
 glIsList                    := glProcedure('glIsList');
 glIsTexture                 := glProcedure('glIsTexture');
 glLightModelf               := glProcedure('glLightModelf');
 glLightModelfv              := glProcedure('glLightModelfv');
 glLightModeli               := glProcedure('glLightModeli');
 glLightModeliv              := glProcedure('glLightModeliv');
 glLightf                    := glProcedure('glLightf');
 glLightfv                   := glProcedure('glLightfv');
 glLighti                    := glProcedure('glLighti');
 glLightiv                   := glProcedure('glLightiv');
 glLineStipple               := glProcedure('glLineStipple');
 glLineWidth                 := glProcedure('glLineWidth');
 glListBase                  := glProcedure('glListBase');
 glLoadIdentity              := glProcedure('glLoadIdentity');
 glLoadMatrixd               := glProcedure('glLoadMatrixd');
 glLoadMatrixf               := glProcedure('glLoadMatrixf');
 glLoadName                  := glProcedure('glLoadName');
 glLogicOp                   := glProcedure('glLogicOp');
 glMap1d                     := glProcedure('glMap1d');
 glMap1f                     := glProcedure('glMap1f');
 glMap2d                     := glProcedure('glMap2d');
 glMap2f                     := glProcedure('glMap2f');
 glMapGrid1d                 := glProcedure('glMapGrid1d');
 glMapGrid1f                 := glProcedure('glMapGrid1f');
 glMapGrid2d                 := glProcedure('glMapGrid2d');
 glMapGrid2f                 := glProcedure('glMapGrid2f');
 glMaterialf                 := glProcedure('glMaterialf');
 glMaterialfv                := glProcedure('glMaterialfv');
 glMateriali                 := glProcedure('glMateriali');
 glMaterialiv                := glProcedure('glMaterialiv');
 glMatrixMode                := glProcedure('glMatrixMode');
 glMultMatrixd               := glProcedure('glMultMatrixd');
 glMultMatrixf               := glProcedure('glMultMatrixf');
 glNewList                   := glProcedure('glNewList');
 glNormal3b                  := glProcedure('glNormal3b');
 glNormal3bv                 := glProcedure('glNormal3bv');
 glNormal3d                  := glProcedure('glNormal3d');
 glNormal3dv                 := glProcedure('glNormal3dv');
 glNormal3f                  := glProcedure('glNormal3f');
 glNormal3fv                 := glProcedure('glNormal3fv');
 glNormal3i                  := glProcedure('glNormal3i');
 glNormal3iv                 := glProcedure('glNormal3iv');
 glNormal3s                  := glProcedure('glNormal3s');
 glNormal3sv                 := glProcedure('glNormal3sv');
 glNormalPointer             := glProcedure('glNormalPointer');
 glOrtho                     := glProcedure('glOrtho');
 glPassThrough               := glProcedure('glPassThrough');
 glPixelMapfv                := glProcedure('glPixelMapfv');
 glPixelMapuiv               := glProcedure('glPixelMapuiv');
 glPixelMapusv               := glProcedure('glPixelMapusv');
 glPixelStoref               := glProcedure('glPixelStoref');
 glPixelStorei               := glProcedure('glPixelStorei');
 glPixelTransferf            := glProcedure('glPixelTransferf');
 glPixelTransferi            := glProcedure('glPixelTransferi');
 glPixelZoom                 := glProcedure('glPixelZoom');
 glPointSize                 := glProcedure('glPointSize');
 glPolygonMode               := glProcedure('glPolygonMode');
 glPolygonOffset             := glProcedure('glPolygonOffset');
 glPolygonStipple            := glProcedure('glPolygonStipple');
 glPopAttrib                 := glProcedure('glPopAttrib');
 glPopClientAttrib           := glProcedure('glPopClientAttrib');
 glPopMatrix                 := glProcedure('glPopMatrix');
 glPopName                   := glProcedure('glPopName');
 glPrioritizeTextures        := glProcedure('glPrioritizeTextures');
 glPushAttrib                := glProcedure('glPushAttrib');
 glPushClientAttrib          := glProcedure('glPushClientAttrib');
 glPushMatrix                := glProcedure('glPushMatrix');
 glPushName                  := glProcedure('glPushName');
 glRasterPos2d               := glProcedure('glRasterPos2d');
 glRasterPos2dv              := glProcedure('glRasterPos2dv');
 glRasterPos2f               := glProcedure('glRasterPos2f');
 glRasterPos2fv              := glProcedure('glRasterPos2fv');
 glRasterPos2i               := glProcedure('glRasterPos2i');
 glRasterPos2iv              := glProcedure('glRasterPos2iv');
 glRasterPos2s               := glProcedure('glRasterPos2s');
 glRasterPos2sv              := glProcedure('glRasterPos2sv');
 glRasterPos3d               := glProcedure('glRasterPos3d');
 glRasterPos3dv              := glProcedure('glRasterPos3dv');
 glRasterPos3f               := glProcedure('glRasterPos3f');
 glRasterPos3fv              := glProcedure('glRasterPos3fv');
 glRasterPos3i               := glProcedure('glRasterPos3i');
 glRasterPos3iv              := glProcedure('glRasterPos3iv');
 glRasterPos3s               := glProcedure('glRasterPos3s');
 glRasterPos3sv              := glProcedure('glRasterPos3sv');
 glRasterPos4d               := glProcedure('glRasterPos4d');
 glRasterPos4dv              := glProcedure('glRasterPos4dv');
 glRasterPos4f               := glProcedure('glRasterPos4f');
 glRasterPos4fv              := glProcedure('glRasterPos4fv');
 glRasterPos4i               := glProcedure('glRasterPos4i');
 glRasterPos4iv              := glProcedure('glRasterPos4iv');
 glRasterPos4s               := glProcedure('glRasterPos4s');
 glRasterPos4sv              := glProcedure('glRasterPos4sv');
 glReadBuffer                := glProcedure('glReadBuffer');
 glReadPixels                := glProcedure('glReadPixels');
 glRectd                     := glProcedure('glRectd');
 glRectdv                    := glProcedure('glRectdv');
 glRectf                     := glProcedure('glRectf');
 glRectfv                    := glProcedure('glRectfv');
 glRecti                     := glProcedure('glRecti');
 glRectiv                    := glProcedure('glRectiv');
 glRects                     := glProcedure('glRects');
 glRectsv                    := glProcedure('glRectsv');
 glRenderMode                := glProcedure('glRenderMode');
 glRotated                   := glProcedure('glRotated');
 glRotatef                   := glProcedure('glRotatef');
 glScaled                    := glProcedure('glScaled');
 glScalef                    := glProcedure('glScalef');
 glScissor                   := glProcedure('glScissor');
 glSelectBuffer              := glProcedure('glSelectBuffer');
 glShadeModel                := glProcedure('glShadeModel');
 glStencilFunc               := glProcedure('glStencilFunc');
 glStencilMask               := glProcedure('glStencilMask');
 glStencilOp                 := glProcedure('glStencilOp');
 glTexCoord1d                := glProcedure('glTexCoord1d');
 glTexCoord1dv               := glProcedure('glTexCoord1dv');
 glTexCoord1f                := glProcedure('glTexCoord1f');
 glTexCoord1fv               := glProcedure('glTexCoord1fv');
 glTexCoord1i                := glProcedure('glTexCoord1i');
 glTexCoord1iv               := glProcedure('glTexCoord1iv');
 glTexCoord1s                := glProcedure('glTexCoord1s');
 glTexCoord1sv               := glProcedure('glTexCoord1sv');
 glTexCoord2d                := glProcedure('glTexCoord2d');
 glTexCoord2dv               := glProcedure('glTexCoord2dv');
 glTexCoord2f                := glProcedure('glTexCoord2f');
 glTexCoord2fv               := glProcedure('glTexCoord2fv');
 glTexCoord2i                := glProcedure('glTexCoord2i');
 glTexCoord2iv               := glProcedure('glTexCoord2iv');
 glTexCoord2s                := glProcedure('glTexCoord2s');
 glTexCoord2sv               := glProcedure('glTexCoord2sv');
 glTexCoord3d                := glProcedure('glTexCoord3d');
 glTexCoord3dv               := glProcedure('glTexCoord3dv');
 glTexCoord3f                := glProcedure('glTexCoord3f');
 glTexCoord3fv               := glProcedure('glTexCoord3fv');
 glTexCoord3i                := glProcedure('glTexCoord3i');
 glTexCoord3iv               := glProcedure('glTexCoord3iv');
 glTexCoord3s                := glProcedure('glTexCoord3s');
 glTexCoord3sv               := glProcedure('glTexCoord3sv');
 glTexCoord4d                := glProcedure('glTexCoord4d');
 glTexCoord4dv               := glProcedure('glTexCoord4dv');
 glTexCoord4f                := glProcedure('glTexCoord4f');
 glTexCoord4fv               := glProcedure('glTexCoord4fv');
 glTexCoord4i                := glProcedure('glTexCoord4i');
 glTexCoord4iv               := glProcedure('glTexCoord4iv');
 glTexCoord4s                := glProcedure('glTexCoord4s');
 glTexCoord4sv               := glProcedure('glTexCoord4sv');
 glTexCoordPointer           := glProcedure('glTexCoordPointer');
 glTexEnvf                   := glProcedure('glTexEnvf');
 glTexEnvfv                  := glProcedure('glTexEnvfv');
 glTexEnvi                   := glProcedure('glTexEnvi');
 glTexEnviv                  := glProcedure('glTexEnviv');
 glTexGend                   := glProcedure('glTexGend');
 glTexGendv                  := glProcedure('glTexGendv');
 glTexGenf                   := glProcedure('glTexGenf');
 glTexGenfv                  := glProcedure('glTexGenfv');
 glTexGeni                   := glProcedure('glTexGeni');
 glTexGeniv                  := glProcedure('glTexGeniv');
 glTexImage1D                := glProcedure('glTexImage1D');
 glTexImage2D                := glProcedure('glTexImage2D');
 glTexParameterf             := glProcedure('glTexParameterf');
 glTexParameterfv            := glProcedure('glTexParameterfv');
 glTexParameteri             := glProcedure('glTexParameteri');
 glTexParameteriv            := glProcedure('glTexParameteriv');
 glTexSubImage1D             := glProcedure('glTexSubImage1D');
 glTexSubImage2D             := glProcedure('glTexSubImage2D');
 glTranslated                := glProcedure('glTranslated');
 glTranslatef                := glProcedure('glTranslatef');
 glVertex2d                  := glProcedure('glVertex2d');
 glVertex2dv                 := glProcedure('glVertex2dv');
 glVertex2f                  := glProcedure('glVertex2f');
 glVertex2fv                 := glProcedure('glVertex2fv');
 glVertex2i                  := glProcedure('glVertex2i');
 glVertex2iv                 := glProcedure('glVertex2iv');
 glVertex2s                  := glProcedure('glVertex2s');
 glVertex2sv                 := glProcedure('glVertex2sv');
 glVertex3d                  := glProcedure('glVertex3d');
 glVertex3dv                 := glProcedure('glVertex3dv');
 glVertex3f                  := glProcedure('glVertex3f');
 glVertex3fv                 := glProcedure('glVertex3fv');
 glVertex3i                  := glProcedure('glVertex3i');
 glVertex3iv                 := glProcedure('glVertex3iv');
 glVertex3s                  := glProcedure('glVertex3s');
 glVertex3sv                 := glProcedure('glVertex3sv');
 glVertex4d                  := glProcedure('glVertex4d');
 glVertex4dv                 := glProcedure('glVertex4dv');
 glVertex4f                  := glProcedure('glVertex4f');
 glVertex4fv                 := glProcedure('glVertex4fv');
 glVertex4i                  := glProcedure('glVertex4i');
 glVertex4iv                 := glProcedure('glVertex4iv');
 glVertex4s                  := glProcedure('glVertex4s');
 glVertex4sv                 := glProcedure('glVertex4sv');
 glVertexPointer             := glProcedure('glVertexPointer');
 glViewport                  := glProcedure('glViewport');
 // GL_VERSION_1_2 =============================================================
 glBlendColor                := glProcedure('glBlendColor');
 glBlendEquation             := glProcedure('glBlendEquation');
 glDrawRangeElements         := glProcedure('glDrawRangeElements');
 glColorTable                := glProcedure('glColorTable');
 glColorTableParameterfv     := glProcedure('glColorTableParameterfv');
 glColorTableParameteriv     := glProcedure('glColorTableParameteriv');
 glCopyColorTable            := glProcedure('glCopyColorTable');
 glGetColorTable             := glProcedure('glGetColorTable');
 glGetColorTableParameterfv  := glProcedure('glGetColorTableParameterfv');
 glGetColorTableParameteriv  := glProcedure('glGetColorTableParameteriv');
 glColorSubTable             := glProcedure('glColorSubTable');
 glCopyColorSubTable         := glProcedure('glCopyColorSubTable');
 glConvolutionFilter1D       := glProcedure('glConvolutionFilter1D');
 glConvolutionFilter2D       := glProcedure('glConvolutionFilter2D');
 glConvolutionParameterf     := glProcedure('glConvolutionParameterf');
 glConvolutionParameterfv    := glProcedure('glConvolutionParameterfv');
 glConvolutionParameteri     := glProcedure('glConvolutionParameteri');
 glConvolutionParameteriv    := glProcedure('glConvolutionParameteriv');
 glCopyConvolutionFilter1D   := glProcedure('glCopyConvolutionFilter1D');
 glCopyConvolutionFilter2D   := glProcedure('glCopyConvolutionFilter2D');
 glGetConvolutionFilter      := glProcedure('glGetConvolutionFilter');
 glGetConvolutionParameterfv := glProcedure('glGetConvolutionParameterfv');
 glGetConvolutionParameteriv := glProcedure('glGetConvolutionParameteriv');
 glGetSeparableFilter        := glProcedure('glGetSeparableFilter');
 glSeparableFilter2D         := glProcedure('glSeparableFilter2D');
 glGetHistogram              := glProcedure('glGetHistogram');
 glGetHistogramParameterfv   := glProcedure('glGetHistogramParameterfv');
 glGetHistogramParameteriv   := glProcedure('glGetHistogramParameteriv');
 glGetMinmax                 := glProcedure('glGetMinmax');
 glGetMinmaxParameterfv      := glProcedure('glGetMinmaxParameterfv');
 glGetMinmaxParameteriv      := glProcedure('glGetMinmaxParameteriv');
 glHistogram                 := glProcedure('glHistogram');
 glMinmax                    := glProcedure('glMinmax');
 glResetHistogram            := glProcedure('glResetHistogram');
 glResetMinmax               := glProcedure('glResetMinmax');
 glTexImage3D                := glProcedure('glTexImage3D');
 glTexSubImage3D             := glProcedure('glTexSubImage3D');
 glCopyTexSubImage3D         := glProcedure('glCopyTexSubImage3D');
 // GL_VERSION_1_3 =============================================================
 glActiveTexture             := glProcedure('glActiveTexture');
 glClientActiveTexture       := glProcedure('glClientActiveTexture');
 glMultiTexCoord1d           := glProcedure('glMultiTexCoord1d');
 glMultiTexCoord1dv          := glProcedure('glMultiTexCoord1dv');
 glMultiTexCoord1f           := glProcedure('glMultiTexCoord1f');
 glMultiTexCoord1fv          := glProcedure('glMultiTexCoord1fv');
 glMultiTexCoord1i           := glProcedure('glMultiTexCoord1i');
 glMultiTexCoord1iv          := glProcedure('glMultiTexCoord1iv');
 glMultiTexCoord1s           := glProcedure('glMultiTexCoord1s');
 glMultiTexCoord1sv          := glProcedure('glMultiTexCoord1sv');
 glMultiTexCoord2d           := glProcedure('glMultiTexCoord2d');
 glMultiTexCoord2dv          := glProcedure('glMultiTexCoord2dv');
 glMultiTexCoord2f           := glProcedure('glMultiTexCoord2f');
 glMultiTexCoord2fv          := glProcedure('glMultiTexCoord2fv');
 glMultiTexCoord2i           := glProcedure('glMultiTexCoord2i');
 glMultiTexCoord2iv          := glProcedure('glMultiTexCoord2iv');
 glMultiTexCoord2s           := glProcedure('glMultiTexCoord2s');
 glMultiTexCoord2sv          := glProcedure('glMultiTexCoord2sv');
 glMultiTexCoord3d           := glProcedure('glMultiTexCoord3d');
 glMultiTexCoord3dv          := glProcedure('glMultiTexCoord3dv');
 glMultiTexCoord3f           := glProcedure('glMultiTexCoord3f');
 glMultiTexCoord3fv          := glProcedure('glMultiTexCoord3fv');
 glMultiTexCoord3i           := glProcedure('glMultiTexCoord3i');
 glMultiTexCoord3iv          := glProcedure('glMultiTexCoord3iv');
 glMultiTexCoord3s           := glProcedure('glMultiTexCoord3s');
 glMultiTexCoord3sv          := glProcedure('glMultiTexCoord3sv');
 glMultiTexCoord4d           := glProcedure('glMultiTexCoord4d');
 glMultiTexCoord4dv          := glProcedure('glMultiTexCoord4dv');
 glMultiTexCoord4f           := glProcedure('glMultiTexCoord4f');
 glMultiTexCoord4fv          := glProcedure('glMultiTexCoord4fv');
 glMultiTexCoord4i           := glProcedure('glMultiTexCoord4i');
 glMultiTexCoord4iv          := glProcedure('glMultiTexCoord4iv');
 glMultiTexCoord4s           := glProcedure('glMultiTexCoord4s');
 glMultiTexCoord4sv          := glProcedure('glMultiTexCoord4sv');
 glLoadTransposeMatrixf      := glProcedure('glLoadTransposeMatrixf');
 glLoadTransposeMatrixd      := glProcedure('glLoadTransposeMatrixd');
 glMultTransposeMatrixf      := glProcedure('glMultTransposeMatrixf');
 glMultTransposeMatrixd      := glProcedure('glMultTransposeMatrixd');
 glSampleCoverage            := glProcedure('glSampleCoverage');
 glCompressedTexImage3D      := glProcedure('glCompressedTexImage3D');
 glCompressedTexImage2D      := glProcedure('glCompressedTexImage2D');
 glCompressedTexImage1D      := glProcedure('glCompressedTexImage1D');
 glCompressedTexSubImage3D   := glProcedure('glCompressedTexSubImage3D');
 glCompressedTexSubImage2D   := glProcedure('glCompressedTexSubImage2D');
 glCompressedTexSubImage1D   := glProcedure('glCompressedTexSubImage1D');
 glGetCompressedTexImage     := glProcedure('glGetCompressedTexImage');
 // GL_VERSION_1_4 =============================================================
 glBlendFuncSeparate         := glProcedure('glBlendFuncSeparate');
 glFogCoordf                 := glProcedure('glFogCoordf');
 glFogCoordfv                := glProcedure('glFogCoordfv');
 glFogCoordd                 := glProcedure('glFogCoordd');
 glFogCoorddv                := glProcedure('glFogCoorddv');
 glFogCoordPointer           := glProcedure('glFogCoordPointer');
 glMultiDrawArrays           := glProcedure('glMultiDrawArrays');
 glMultiDrawElements         := glProcedure('glMultiDrawElements');
 glPointParameterf           := glProcedure('glPointParameterf');
 glPointParameterfv          := glProcedure('glPointParameterfv');
 glPointParameteri           := glProcedure('glPointParameteri');
 glPointParameteriv          := glProcedure('glPointParameteriv');
 glSecondaryColor3b          := glProcedure('glSecondaryColor3b');
 glSecondaryColor3bv         := glProcedure('glSecondaryColor3bv');
 glSecondaryColor3d          := glProcedure('glSecondaryColor3d');
 glSecondaryColor3dv         := glProcedure('glSecondaryColor3dv');
 glSecondaryColor3f          := glProcedure('glSecondaryColor3f');
 glSecondaryColor3fv         := glProcedure('glSecondaryColor3fv');
 glSecondaryColor3i          := glProcedure('glSecondaryColor3i');
 glSecondaryColor3iv         := glProcedure('glSecondaryColor3iv');
 glSecondaryColor3s          := glProcedure('glSecondaryColor3s');
 glSecondaryColor3sv         := glProcedure('glSecondaryColor3sv');
 glSecondaryColor3ub         := glProcedure('glSecondaryColor3ub');
 glSecondaryColor3ubv        := glProcedure('glSecondaryColor3ubv');
 glSecondaryColor3ui         := glProcedure('glSecondaryColor3ui');
 glSecondaryColor3uiv        := glProcedure('glSecondaryColor3uiv');
 glSecondaryColor3us         := glProcedure('glSecondaryColor3us');
 glSecondaryColor3usv        := glProcedure('glSecondaryColor3usv');
 glSecondaryColorPointer     := glProcedure('glSecondaryColorPointer');
 glWindowPos2d               := glProcedure('glWindowPos2d');
 glWindowPos2dv              := glProcedure('glWindowPos2dv');
 glWindowPos2f               := glProcedure('glWindowPos2f');
 glWindowPos2fv              := glProcedure('glWindowPos2fv');
 glWindowPos2i               := glProcedure('glWindowPos2i');
 glWindowPos2iv              := glProcedure('glWindowPos2iv');
 glWindowPos2s               := glProcedure('glWindowPos2s');
 glWindowPos2sv              := glProcedure('glWindowPos2sv');
 glWindowPos3d               := glProcedure('glWindowPos3d');
 glWindowPos3dv              := glProcedure('glWindowPos3dv');
 glWindowPos3f               := glProcedure('glWindowPos3f');
 glWindowPos3fv              := glProcedure('glWindowPos3fv');
 glWindowPos3i               := glProcedure('glWindowPos3i');
 glWindowPos3iv              := glProcedure('glWindowPos3iv');
 glWindowPos3s               := glProcedure('glWindowPos3s');
 glWindowPos3sv              := glProcedure('glWindowPos3sv');

    // GL_3DFX_tbuffer =========================================================
    glTbufferMask3DFX := wglGetProcAddress('glTbufferMask3DFX');

    // GL_APPLE_element_array ==================================================
    glElementPointerAPPLE             := wglGetProcAddress('glElementPointerAPPLE');
    glDrawElementArrayAPPLE           := wglGetProcAddress('glDrawElementArrayAPPLE');
    glDrawRangeElementArrayAPPLE      := wglGetProcAddress('glDrawRangeElementArrayAPPLE');
    glMultiDrawElementArrayAPPLE      := wglGetProcAddress('glMultiDrawElementArrayAPPLE');
    glMultiDrawRangeElementArrayAPPLE := wglGetProcAddress('glMultiDrawRangeElementArrayAPPLE');

    // GL_APPLE_fence ==========================================================
    glGenFencesAPPLE    := wglGetProcAddress('glGenFencesAPPLE');
    glDeleteFencesAPPLE := wglGetProcAddress('glDeleteFencesAPPLE');
    glSetFenceAPPLE     := wglGetProcAddress('glSetFenceAPPLE');
    glIsFenceAPPLE      := wglGetProcAddress('glIsFenceAPPLE');
    glTestFenceAPPLE    := wglGetProcAddress('glTestFenceAPPLE');
    glFinishFenceAPPLE  := wglGetProcAddress('glFinishFenceAPPLE');
    glTestObjectAPPLE   := wglGetProcAddress('glTestObjectAPPLE');
    glFinishObjectAPPLE := wglGetProcAddress('glFinishObjectAPPLE');

    // GL_APPLE_vertex_array_object ============================================
    glBindVertexArrayAPPLE    := wglGetProcAddress('glBindVertexArrayAPPLE');
    glDeleteVertexArraysAPPLE := wglGetProcAddress('glDeleteVertexArraysAPPLE');
    glGenVertexArraysAPPLE    := wglGetProcAddress('glGenVertexArraysAPPLE');
    glIsVertexArrayAPPLE      := wglGetProcAddress('glIsVertexArrayAPPLE');

    // GL_APPLE_vertex_array_range =============================================
    glVertexArrayRangeAPPLE      := wglGetProcAddress('glVertexArrayRangeAPPLE');
    glFlushVertexArrayRangeAPPLE := wglGetProcAddress('glFlushVertexArrayRangeAPPLE');
    glVertexArrayParameteriAPPLE := wglGetProcAddress('glVertexArrayParameteriAPPLE');

    // GL_ARB_matrix_palette ===================================================
    glCurrentPaletteMatrixARB := wglGetProcAddress('glCurrentPaletteMatrixARB');
    glMatrixIndexubvARB       := wglGetProcAddress('glMatrixIndexubvARB');
    glMatrixIndexusvARB       := wglGetProcAddress('glMatrixIndexusvARB');
    glMatrixIndexuivARB       := wglGetProcAddress('glMatrixIndexuivARB');
    glMatrixIndexPointerARB   := wglGetProcAddress('glMatrixIndexPointerARB');

    // GL_ARB_multisample ======================================================
    glSampleCoverageARB := wglGetProcAddress('glSampleCoverageARB');

    // GL_ARB_multitexture =====================================================
    glActiveTextureARB       := wglGetProcAddress('glActiveTextureARB');
    glClientActiveTextureARB := wglGetProcAddress('glClientActiveTextureARB');
    glMultiTexCoord1dARB     := wglGetProcAddress('glMultiTexCoord1dARB');
    glMultiTexCoord1dvARB    := wglGetProcAddress('glMultiTexCoord1dvARB');
    glMultiTexCoord1fARB     := wglGetProcAddress('glMultiTexCoord1fARB');
    glMultiTexCoord1fvARB    := wglGetProcAddress('glMultiTexCoord1fvARB');
    glMultiTexCoord1iARB     := wglGetProcAddress('glMultiTexCoord1iARB');
    glMultiTexCoord1ivARB    := wglGetProcAddress('glMultiTexCoord1ivARB');
    glMultiTexCoord1sARB     := wglGetProcAddress('glMultiTexCoord1sARB');
    glMultiTexCoord1svARB    := wglGetProcAddress('glMultiTexCoord1svARB');
    glMultiTexCoord2dARB     := wglGetProcAddress('glMultiTexCoord2dARB');
    glMultiTexCoord2dvARB    := wglGetProcAddress('glMultiTexCoord2dvARB');
    glMultiTexCoord2fARB     := wglGetProcAddress('glMultiTexCoord2fARB');
    glMultiTexCoord2fvARB    := wglGetProcAddress('glMultiTexCoord2fvARB');
    glMultiTexCoord2iARB     := wglGetProcAddress('glMultiTexCoord2iARB');
    glMultiTexCoord2ivARB    := wglGetProcAddress('glMultiTexCoord2ivARB');
    glMultiTexCoord2sARB     := wglGetProcAddress('glMultiTexCoord2sARB');
    glMultiTexCoord2svARB    := wglGetProcAddress('glMultiTexCoord2svARB');
    glMultiTexCoord3dARB     := wglGetProcAddress('glMultiTexCoord3dARB');
    glMultiTexCoord3dvARB    := wglGetProcAddress('glMultiTexCoord3dvARB');
    glMultiTexCoord3fARB     := wglGetProcAddress('glMultiTexCoord3fARB');
    glMultiTexCoord3fvARB    := wglGetProcAddress('glMultiTexCoord3fvARB');
    glMultiTexCoord3iARB     := wglGetProcAddress('glMultiTexCoord3iARB');
    glMultiTexCoord3ivARB    := wglGetProcAddress('glMultiTexCoord3ivARB');
    glMultiTexCoord3sARB     := wglGetProcAddress('glMultiTexCoord3sARB');
    glMultiTexCoord3svARB    := wglGetProcAddress('glMultiTexCoord3svARB');
    glMultiTexCoord4dARB     := wglGetProcAddress('glMultiTexCoord4dARB');
    glMultiTexCoord4dvARB    := wglGetProcAddress('glMultiTexCoord4dvARB');
    glMultiTexCoord4fARB     := wglGetProcAddress('glMultiTexCoord4fARB');
    glMultiTexCoord4fvARB    := wglGetProcAddress('glMultiTexCoord4fvARB');
    glMultiTexCoord4iARB     := wglGetProcAddress('glMultiTexCoord4iARB');
    glMultiTexCoord4ivARB    := wglGetProcAddress('glMultiTexCoord4ivARB');
    glMultiTexCoord4sARB     := wglGetProcAddress('glMultiTexCoord4sARB');
    glMultiTexCoord4svARB    := wglGetProcAddress('glMultiTexCoord4svARB');

    // GL_ARB_point_parameters =================================================
    glPointParameterfARB  := wglGetProcAddress('glPointParameterfARB');
    glPointParameterfvARB := wglGetProcAddress('glPointParameterfvARB');

    // GL_ARB_texture_compression ==============================================
    glCompressedTexImage3DARB    := wglGetProcAddress('glCompressedTexImage3DARB');
    glCompressedTexImage2DARB    := wglGetProcAddress('glCompressedTexImage2DARB');
    glCompressedTexImage1DARB    := wglGetProcAddress('glCompressedTexImage1DARB');
    glCompressedTexSubImage3DARB := wglGetProcAddress('glCompressedTexSubImage3DARB');
    glCompressedTexSubImage2DARB := wglGetProcAddress('glCompressedTexSubImage2DARB');
    glCompressedTexSubImage1DARB := wglGetProcAddress('glCompressedTexSubImage1DARB');
    glGetCompressedTexImageARB   := wglGetProcAddress('glGetCompressedTexImageARB');

    // GL_ARB_transpose_matrix =================================================
    glLoadTransposeMatrixfARB := wglGetProcAddress('glLoadTransposeMatrixfARB');
    glLoadTransposeMatrixdARB := wglGetProcAddress('glLoadTransposeMatrixdARB');
    glMultTransposeMatrixfARB := wglGetProcAddress('glMultTransposeMatrixfARB');
    glMultTransposeMatrixdARB := wglGetProcAddress('glMultTransposeMatrixdARB');

    // GL_ARB_vertex_blend =====================================================
    glWeightbvARB      := wglGetProcAddress('glWeightbvARB');
    glWeightsvARB      := wglGetProcAddress('glWeightsvARB');
    glWeightivARB      := wglGetProcAddress('glWeightivARB');
    glWeightfvARB      := wglGetProcAddress('glWeightfvARB');
    glWeightdvARB      := wglGetProcAddress('glWeightdvARB');
    glWeightubvARB     := wglGetProcAddress('glWeightubvARB');
    glWeightusvARB     := wglGetProcAddress('glWeightusvARB');
    glWeightuivARB     := wglGetProcAddress('glWeightuivARB');
    glWeightPointerARB := wglGetProcAddress('glWeightPointerARB');
    glVertexBlendARB   := wglGetProcAddress('glVertexBlendARB');

    // GL_ARB_vertex_buffer_object =============================================
    glBindBufferARB           := wglGetProcAddress('glBindBufferARB');
    glDeleteBuffersARB        := wglGetProcAddress('glDeleteBuffersARB');
    glGenBuffersARB           := wglGetProcAddress('glGenBuffersARB');
    glIsBufferARB             := wglGetProcAddress('glIsBufferARB');
    glBufferDataARB           := wglGetProcAddress('glBufferDataARB');
    glBufferSubDataARB        := wglGetProcAddress('glBufferSubDataARB');
    glGetBufferSubDataARB     := wglGetProcAddress('glGetBufferSubDataARB');
    glMapBufferARB            := wglGetProcAddress('glMapBufferARB');
    glUnmapBufferARB          := wglGetProcAddress('glUnmapBufferARB');
    glGetBufferParameterivARB := wglGetProcAddress('glGetBufferParameterivARB');
    glGetBufferPointervARB    := wglGetProcAddress('glGetBufferPointervARB');

    // ARB less version for GL 1.5 =============================================
    glBindBuffer           := wglGetProcAddress('glBindBuffer');
    glDeleteBuffers        := wglGetProcAddress('glDeleteBuffers');
    glGenBuffers           := wglGetProcAddress('glGenBuffers');
    glIsBuffer             := wglGetProcAddress('glIsBuffer');
    glBufferData           := wglGetProcAddress('glBufferData');
    glBufferSubData        := wglGetProcAddress('glBufferSubData');
    glGetBufferSubData     := wglGetProcAddress('glGetBufferSubData');
    glMapBuffer            := wglGetProcAddress('glMapBuffer');
    glUnmapBuffer          := wglGetProcAddress('glUnmapBuffer');
    glGetBufferParameteriv := wglGetProcAddress('glGetBufferParameteriv');
    glGetBufferPointerv    := wglGetProcAddress('glGetBufferPointerv');

    // GL_ARB_vertex_program ===================================================
    glVertexAttrib1dARB              := wglGetProcAddress( 'glVertexAttrib1dARB');
    glVertexAttrib1dvARB             := wglGetProcAddress( 'glVertexAttrib1dvARB');
    glVertexAttrib1fARB              := wglGetProcAddress( 'glVertexAttrib1fARB');
    glVertexAttrib1fvARB             := wglGetProcAddress( 'glVertexAttrib1fvARB');
    glVertexAttrib1sARB              := wglGetProcAddress( 'glVertexAttrib1sARB');
    glVertexAttrib1svARB             := wglGetProcAddress( 'glVertexAttrib1svARB');
    glVertexAttrib2dARB              := wglGetProcAddress( 'glVertexAttrib2dARB');
    glVertexAttrib2dvARB             := wglGetProcAddress( 'glVertexAttrib2dvARB');
    glVertexAttrib2fARB              := wglGetProcAddress( 'glVertexAttrib2fARB');
    glVertexAttrib2fvARB             := wglGetProcAddress( 'glVertexAttrib2fvARB');
    glVertexAttrib2sARB              := wglGetProcAddress( 'glVertexAttrib2sARB');
    glVertexAttrib2svARB             := wglGetProcAddress( 'glVertexAttrib2svARB');
    glVertexAttrib3dARB              := wglGetProcAddress( 'glVertexAttrib3dARB');
    glVertexAttrib3dvARB             := wglGetProcAddress( 'glVertexAttrib3dvARB');
    glVertexAttrib3fARB              := wglGetProcAddress( 'glVertexAttrib3fARB');
    glVertexAttrib3fvARB             := wglGetProcAddress( 'glVertexAttrib3fvARB');
    glVertexAttrib3sARB              := wglGetProcAddress( 'glVertexAttrib3sARB');
    glVertexAttrib3svARB             := wglGetProcAddress( 'glVertexAttrib3svARB');
    glVertexAttrib4NbvARB            := wglGetProcAddress( 'glVertexAttrib4NbvARB');
    glVertexAttrib4NivARB            := wglGetProcAddress( 'glVertexAttrib4NivARB');
    glVertexAttrib4NsvARB            := wglGetProcAddress( 'glVertexAttrib4NsvARB');
    glVertexAttrib4NubARB            := wglGetProcAddress( 'glVertexAttrib4NubARB');
    glVertexAttrib4NubvARB           := wglGetProcAddress( 'glVertexAttrib4NubvARB');
    glVertexAttrib4NuivARB           := wglGetProcAddress( 'glVertexAttrib4NuivARB');
    glVertexAttrib4NusvARB           := wglGetProcAddress( 'glVertexAttrib4NusvARB');
    glVertexAttrib4bvARB             := wglGetProcAddress( 'glVertexAttrib4bvARB');
    glVertexAttrib4dARB              := wglGetProcAddress( 'glVertexAttrib4dARB');
    glVertexAttrib4dvARB             := wglGetProcAddress( 'glVertexAttrib4dvARB');
    glVertexAttrib4fARB              := wglGetProcAddress( 'glVertexAttrib4fARB');
    glVertexAttrib4fvARB             := wglGetProcAddress( 'glVertexAttrib4fvARB');
    glVertexAttrib4ivARB             := wglGetProcAddress( 'glVertexAttrib4ivARB');
    glVertexAttrib4sARB              := wglGetProcAddress( 'glVertexAttrib4sARB');
    glVertexAttrib4svARB             := wglGetProcAddress( 'glVertexAttrib4svARB');
    glVertexAttrib4ubvARB            := wglGetProcAddress( 'glVertexAttrib4ubvARB');
    glVertexAttrib4uivARB            := wglGetProcAddress( 'glVertexAttrib4uivARB');
    glVertexAttrib4usvARB            := wglGetProcAddress( 'glVertexAttrib4usvARB');
    glVertexAttribPointerARB         := wglGetProcAddress( 'glVertexAttribPointerARB');
    glEnableVertexAttribArrayARB     := wglGetProcAddress( 'glEnableVertexAttribArrayARB');
    glDisableVertexAttribArrayARB    := wglGetProcAddress( 'glDisableVertexAttribArrayARB');
    glProgramStringARB               := wglGetProcAddress( 'glProgramStringARB');
    glBindProgramARB                 := wglGetProcAddress( 'glBindProgramARB');
    glDeleteProgramsARB              := wglGetProcAddress( 'glDeleteProgramsARB');
    glGenProgramsARB                 := wglGetProcAddress( 'glGenProgramsARB');
    glProgramEnvParameter4dARB       := wglGetProcAddress( 'glProgramEnvParameter4dARB');
    glProgramEnvParameter4dvARB      := wglGetProcAddress( 'glProgramEnvParameter4dvARB');
    glProgramEnvParameter4fARB       := wglGetProcAddress( 'glProgramEnvParameter4fARB');
    glProgramEnvParameter4fvARB      := wglGetProcAddress( 'glProgramEnvParameter4fvARB');
    glProgramLocalParameter4dARB     := wglGetProcAddress( 'glProgramLocalParameter4dARB');
    glProgramLocalParameter4dvARB    := wglGetProcAddress( 'glProgramLocalParameter4dvARB');
    glProgramLocalParameter4fARB     := wglGetProcAddress( 'glProgramLocalParameter4fARB');
    glProgramLocalParameter4fvARB    := wglGetProcAddress( 'glProgramLocalParameter4fvARB');
    glGetProgramEnvParameterdvARB    := wglGetProcAddress( 'glGetProgramEnvParameterdvARB');
    glGetProgramEnvParameterfvARB    := wglGetProcAddress( 'glGetProgramEnvParameterfvARB');
    glGetProgramLocalParameterdvARB  := wglGetProcAddress( 'glGetProgramLocalParameterdvARB');
    glGetProgramLocalParameterfvARB  := wglGetProcAddress( 'glGetProgramLocalParameterfvARB');
    glGetProgramivARB                := wglGetProcAddress( 'glGetProgramivARB');
    glGetProgramStringARB            := wglGetProcAddress( 'glGetProgramStringARB');
    glGetVertexAttribdvARB           := wglGetProcAddress( 'glGetVertexAttribdvARB');
    glGetVertexAttribfvARB           := wglGetProcAddress( 'glGetVertexAttribfvARB');
    glGetVertexAttribivARB           := wglGetProcAddress( 'glGetVertexAttribivARB');
    glGetVertexAttribPointervARB     := wglGetProcAddress( 'glGetVertexAttribPointervARB');
    glIsProgramARB                   := wglGetProcAddress( 'glIsProgramARB');

    // GL_ARB_window_pos =======================================================
    glWindowPos2dARB  := wglGetProcAddress( 'glWindowPos2dARB');
    glWindowPos2dvARB := wglGetProcAddress( 'glWindowPos2dvARB');
    glWindowPos2fARB  := wglGetProcAddress( 'glWindowPos2fARB');
    glWindowPos2fvARB := wglGetProcAddress( 'glWindowPos2fvARB');
    glWindowPos2iARB  := wglGetProcAddress( 'glWindowPos2iARB');
    glWindowPos2ivARB := wglGetProcAddress( 'glWindowPos2ivARB');
    glWindowPos2sARB  := wglGetProcAddress( 'glWindowPos2sARB');
    glWindowPos2svARB := wglGetProcAddress( 'glWindowPos2svARB');
    glWindowPos3dARB  := wglGetProcAddress( 'glWindowPos3dARB');
    glWindowPos3dvARB := wglGetProcAddress( 'glWindowPos3dvARB');
    glWindowPos3fARB  := wglGetProcAddress( 'glWindowPos3fARB');
    glWindowPos3fvARB := wglGetProcAddress( 'glWindowPos3fvARB');
    glWindowPos3iARB  := wglGetProcAddress( 'glWindowPos3iARB');
    glWindowPos3ivARB := wglGetProcAddress( 'glWindowPos3ivARB');
    glWindowPos3sARB  := wglGetProcAddress( 'glWindowPos3sARB');
    glWindowPos3svARB := wglGetProcAddress( 'glWindowPos3svARB');

    // GL_ATI_draw_buffers =====================================================
    glDrawBuffersATI := wglGetProcAddress( 'glDrawBuffersATI');

    // GL_ATI_element_array ====================================================
    glElementPointerATI        := wglGetProcAddress( 'glElementPointerATI');
    glDrawElementArrayATI      := wglGetProcAddress( 'glDrawElementArrayATI');
    glDrawRangeElementArrayATI := wglGetProcAddress( 'glDrawRangeElementArrayATI');

    // GL_ATI_envmap_bumpmap ===================================================
    glTexBumpParameterivATI    := wglGetProcAddress( 'glTexBumpParameterivATI');
    glTexBumpParameterfvATI    := wglGetProcAddress( 'glTexBumpParameterfvATI');
    glGetTexBumpParameterivATI := wglGetProcAddress( 'glGetTexBumpParameterivATI');
    glGetTexBumpParameterfvATI := wglGetProcAddress( 'glGetTexBumpParameterfvATI');

    // GL_ATI_fragment_shader ==================================================
    glGenFragmentShadersATI        := wglGetProcAddress( 'glGenFragmentShadersATI');
    glBindFragmentShaderATI        := wglGetProcAddress( 'glBindFragmentShaderATI');
    glDeleteFragmentShaderATI      := wglGetProcAddress( 'glDeleteFragmentShaderATI');
    glBeginFragmentShaderATI       := wglGetProcAddress( 'glBeginFragmentShaderATI');
    glEndFragmentShaderATI         := wglGetProcAddress( 'glEndFragmentShaderATI');
    glPassTexCoordATI              := wglGetProcAddress( 'glPassTexCoordATI');
    glSampleMapATI                 := wglGetProcAddress( 'glSampleMapATI');
    glColorFragmentOp1ATI          := wglGetProcAddress( 'glColorFragmentOp1ATI');
    glColorFragmentOp2ATI          := wglGetProcAddress( 'glColorFragmentOp2ATI');
    glColorFragmentOp3ATI          := wglGetProcAddress( 'glColorFragmentOp3ATI');
    glAlphaFragmentOp1ATI          := wglGetProcAddress( 'glAlphaFragmentOp1ATI');
    glAlphaFragmentOp2ATI          := wglGetProcAddress( 'glAlphaFragmentOp2ATI');
    glAlphaFragmentOp3ATI          := wglGetProcAddress( 'glAlphaFragmentOp3ATI');
    glSetFragmentShaderConstantATI := wglGetProcAddress( 'glSetFragmentShaderConstantATI');

    // GL_ATI_map_object_buffer ================================================
    glMapObjectBufferATI   := wglGetProcAddress( 'glMapObjectBufferATI');
    glUnmapObjectBufferATI := wglGetProcAddress( 'glUnmapObjectBufferATI');

    // GL_ATI_pn_triangles =====================================================
    glPNTrianglesiATI := wglGetProcAddress( 'glPNTrianglesiATI');
    glPNTrianglesfATI := wglGetProcAddress( 'glPNTrianglesfATI');

    // GL_ATI_separate_stencil =================================================
    glStencilOpSeparateATI   := wglGetProcAddress( 'glStencilOpSeparateATI');
    glStencilFuncSeparateATI := wglGetProcAddress( 'glStencilFuncSeparateATI');

    // GL_ATI_vertex_array_object ==============================================
    glNewObjectBufferATI         := wglGetProcAddress( 'glNewObjectBufferATI');
    glIsObjectBufferATI          := wglGetProcAddress( 'glIsObjectBufferATI');
    glUpdateObjectBufferATI      := wglGetProcAddress( 'glUpdateObjectBufferATI');
    glGetObjectBufferfvATI       := wglGetProcAddress( 'glGetObjectBufferfvATI');
    glGetObjectBufferivATI       := wglGetProcAddress( 'glGetObjectBufferivATI');
    glFreeObjectBufferATI        := wglGetProcAddress( 'glFreeObjectBufferATI');
    glArrayObjectATI             := wglGetProcAddress( 'glArrayObjectATI');
    glGetArrayObjectfvATI        := wglGetProcAddress( 'glGetArrayObjectfvATI');
    glGetArrayObjectivATI        := wglGetProcAddress( 'glGetArrayObjectivATI');
    glVariantArrayObjectATI      := wglGetProcAddress( 'glVariantArrayObjectATI');
    glGetVariantArrayObjectfvATI := wglGetProcAddress( 'glGetVariantArrayObjectfvATI');
    glGetVariantArrayObjectivATI := wglGetProcAddress( 'glGetVariantArrayObjectivATI');

    // GL_ATI_vertex_attrib_array_object =======================================
    glVertexAttribArrayObjectATI      := wglGetProcAddress( 'glVertexAttribArrayObjectATI');
    glGetVertexAttribArrayObjectfvATI := wglGetProcAddress( 'glGetVertexAttribArrayObjectfvATI');
    glGetVertexAttribArrayObjectivATI := wglGetProcAddress( 'glGetVertexAttribArrayObjectivATI');

    // GL_ATI_vertex_streams ===================================================
    glVertexStream1sATI           := wglGetProcAddress( 'glVertexStream1sATI');
    glVertexStream1svATI          := wglGetProcAddress( 'glVertexStream1svATI');
    glVertexStream1iATI           := wglGetProcAddress( 'glVertexStream1iATI');
    glVertexStream1ivATI          := wglGetProcAddress( 'glVertexStream1ivATI');
    glVertexStream1fATI           := wglGetProcAddress( 'glVertexStream1fATI');
    glVertexStream1fvATI          := wglGetProcAddress( 'glVertexStream1fvATI');
    glVertexStream1dATI           := wglGetProcAddress( 'glVertexStream1dATI');
    glVertexStream1dvATI          := wglGetProcAddress( 'glVertexStream1dvATI');
    glVertexStream2sATI           := wglGetProcAddress( 'glVertexStream2sATI');
    glVertexStream2svATI          := wglGetProcAddress( 'glVertexStream2svATI');
    glVertexStream2iATI           := wglGetProcAddress( 'glVertexStream2iATI');
    glVertexStream2ivATI          := wglGetProcAddress( 'glVertexStream2ivATI');
    glVertexStream2fATI           := wglGetProcAddress( 'glVertexStream2fATI');
    glVertexStream2fvATI          := wglGetProcAddress( 'glVertexStream2fvATI');
    glVertexStream2dATI           := wglGetProcAddress( 'glVertexStream2dATI');
    glVertexStream2dvATI          := wglGetProcAddress( 'glVertexStream2dvATI');
    glVertexStream3sATI           := wglGetProcAddress( 'glVertexStream3sATI');
    glVertexStream3svATI          := wglGetProcAddress( 'glVertexStream3svATI');
    glVertexStream3iATI           := wglGetProcAddress( 'glVertexStream3iATI');
    glVertexStream3ivATI          := wglGetProcAddress( 'glVertexStream3ivATI');
    glVertexStream3fATI           := wglGetProcAddress( 'glVertexStream3fATI');
    glVertexStream3fvATI          := wglGetProcAddress( 'glVertexStream3fvATI');
    glVertexStream3dATI           := wglGetProcAddress( 'glVertexStream3dATI');
    glVertexStream3dvATI          := wglGetProcAddress( 'glVertexStream3dvATI');
    glVertexStream4sATI           := wglGetProcAddress( 'glVertexStream4sATI');
    glVertexStream4svATI          := wglGetProcAddress( 'glVertexStream4svATI');
    glVertexStream4iATI           := wglGetProcAddress( 'glVertexStream4iATI');
    glVertexStream4ivATI          := wglGetProcAddress( 'glVertexStream4ivATI');
    glVertexStream4fATI           := wglGetProcAddress( 'glVertexStream4fATI');
    glVertexStream4fvATI          := wglGetProcAddress( 'glVertexStream4fvATI');
    glVertexStream4dATI           := wglGetProcAddress( 'glVertexStream4dATI');
    glVertexStream4dvATI          := wglGetProcAddress( 'glVertexStream4dvATI');
    glNormalStream3bATI           := wglGetProcAddress( 'glNormalStream3bATI');
    glNormalStream3bvATI          := wglGetProcAddress( 'glNormalStream3bvATI');
    glNormalStream3sATI           := wglGetProcAddress( 'glNormalStream3sATI');
    glNormalStream3svATI          := wglGetProcAddress( 'glNormalStream3svATI');
    glNormalStream3iATI           := wglGetProcAddress( 'glNormalStream3iATI');
    glNormalStream3ivATI          := wglGetProcAddress( 'glNormalStream3ivATI');
    glNormalStream3fATI           := wglGetProcAddress( 'glNormalStream3fATI');
    glNormalStream3fvATI          := wglGetProcAddress( 'glNormalStream3fvATI');
    glNormalStream3dATI           := wglGetProcAddress( 'glNormalStream3dATI');
    glNormalStream3dvATI          := wglGetProcAddress( 'glNormalStream3dvATI');
    glClientActiveVertexStreamATI := wglGetProcAddress( 'glClientActiveVertexStreamATI');
    glVertexBlendEnviATI          := wglGetProcAddress( 'glVertexBlendEnviATI');
    glVertexBlendEnvfATI          := wglGetProcAddress( 'glVertexBlendEnvfATI');

    // GL_EXT_blend_color ======================================================
    glBlendColorEXT := wglGetProcAddress( 'glBlendColorEXT');

    // GL_EXT_blend_func_separate ==============================================
    glBlendFuncSeparateEXT := wglGetProcAddress( 'glBlendFuncSeparateEXT');

    // GL_EXT_blend_minmax =====================================================
    glBlendEquationEXT := wglGetProcAddress( 'glBlendEquationEXT');

    // GL_EXT_color_subtable ===================================================
    glColorSubTableEXT     := wglGetProcAddress( 'glColorSubTableEXT');
    glCopyColorSubTableEXT := wglGetProcAddress( 'glCopyColorSubTableEXT');

    // GL_EXT_compiled_vertex_array ============================================
    glLockArraysEXT   := wglGetProcAddress( 'glLockArraysEXT');
    glUnlockArraysEXT := wglGetProcAddress( 'glUnlockArraysEXT');

    // GL_EXT_convolution ======================================================
    glConvolutionFilter1DEXT       := wglGetProcAddress( 'glConvolutionFilter1DEXT');
    glConvolutionFilter2DEXT       := wglGetProcAddress( 'glConvolutionFilter2DEXT');
    glConvolutionParameterfEXT     := wglGetProcAddress( 'glConvolutionParameterfEXT');
    glConvolutionParameterfvEXT    := wglGetProcAddress( 'glConvolutionParameterfvEXT');
    glConvolutionParameteriEXT     := wglGetProcAddress( 'glConvolutionParameteriEXT');
    glConvolutionParameterivEXT    := wglGetProcAddress( 'glConvolutionParameterivEXT');
    glCopyConvolutionFilter1DEXT   := wglGetProcAddress( 'glCopyConvolutionFilter1DEXT');
    glCopyConvolutionFilter2DEXT   := wglGetProcAddress( 'glCopyConvolutionFilter2DEXT');
    glGetConvolutionFilterEXT      := wglGetProcAddress( 'glGetConvolutionFilterEXT');
    glGetConvolutionParameterfvEXT := wglGetProcAddress( 'glGetConvolutionParameterfvEXT');
    glGetConvolutionParameterivEXT := wglGetProcAddress( 'glGetConvolutionParameterivEXT');
    glGetSeparableFilterEXT        := wglGetProcAddress( 'glGetSeparableFilterEXT');
    glSeparableFilter2DEXT         := wglGetProcAddress( 'glSeparableFilter2DEXT');

    // GL_EXT_coordinate_frame =================================================
    glTangent3bEXT       := wglGetProcAddress( 'glTangent3bEXT');
    glTangent3bvEXT      := wglGetProcAddress( 'glTangent3bvEXT');
    glTangent3dEXT       := wglGetProcAddress( 'glTangent3dEXT');
    glTangent3dvEXT      := wglGetProcAddress( 'glTangent3dvEXT');
    glTangent3fEXT       := wglGetProcAddress( 'glTangent3fEXT');
    glTangent3fvEXT      := wglGetProcAddress( 'glTangent3fvEXT');
    glTangent3iEXT       := wglGetProcAddress( 'glTangent3iEXT');
    glTangent3ivEXT      := wglGetProcAddress( 'glTangent3ivEXT');
    glTangent3sEXT       := wglGetProcAddress( 'glTangent3sEXT');
    glTangent3svEXT      := wglGetProcAddress( 'glTangent3svEXT');
    glBinormal3bEXT      := wglGetProcAddress( 'glBinormal3bEXT');
    glBinormal3bvEXT     := wglGetProcAddress( 'glBinormal3bvEXT');
    glBinormal3dEXT      := wglGetProcAddress( 'glBinormal3dEXT');
    glBinormal3dvEXT     := wglGetProcAddress( 'glBinormal3dvEXT');
    glBinormal3fEXT      := wglGetProcAddress( 'glBinormal3fEXT');
    glBinormal3fvEXT     := wglGetProcAddress( 'glBinormal3fvEXT');
    glBinormal3iEXT      := wglGetProcAddress( 'glBinormal3iEXT');
    glBinormal3ivEXT     := wglGetProcAddress( 'glBinormal3ivEXT');
    glBinormal3sEXT      := wglGetProcAddress( 'glBinormal3sEXT');
    glBinormal3svEXT     := wglGetProcAddress( 'glBinormal3svEXT');
    glTangentPointerEXT  := wglGetProcAddress( 'glTangentPointerEXT');
    glBinormalPointerEXT := wglGetProcAddress( 'glBinormalPointerEXT');

    // GL_EXT_copy_texture =====================================================
    glCopyTexImage1DEXT    := wglGetProcAddress( 'glCopyTexImage1DEXT');
    glCopyTexImage2DEXT    := wglGetProcAddress( 'glCopyTexImage2DEXT');
    glCopyTexSubImage1DEXT := wglGetProcAddress( 'glCopyTexSubImage1DEXT');
    glCopyTexSubImage2DEXT := wglGetProcAddress( 'glCopyTexSubImage2DEXT');
    glCopyTexSubImage3DEXT := wglGetProcAddress( 'glCopyTexSubImage3DEXT');

    // GL_EXT_cull_vertex ======================================================
    glCullParameterdvEXT := wglGetProcAddress( 'glCullParameterdvEXT');
    glCullParameterfvEXT := wglGetProcAddress( 'glCullParameterfvEXT');

    // GL_EXT_draw_range_elements ==============================================
    glDrawRangeElementsEXT := wglGetProcAddress( 'glDrawRangeElementsEXT');

    // GL_EXT_fog_coord ========================================================
    glFogCoordfEXT       := wglGetProcAddress( 'glFogCoordfEXT');
    glFogCoordfvEXT      := wglGetProcAddress( 'glFogCoordfvEXT');
    glFogCoorddEXT       := wglGetProcAddress( 'glFogCoorddEXT');
    glFogCoorddvEXT      := wglGetProcAddress( 'glFogCoorddvEXT');
    glFogCoordPointerEXT := wglGetProcAddress( 'glFogCoordPointerEXT');

    // GL_EXT_histogram ========================================================
    glGetHistogramEXT            := wglGetProcAddress( 'glGetHistogramEXT');
    glGetHistogramParameterfvEXT := wglGetProcAddress( 'glGetHistogramParameterfvEXT');
    glGetHistogramParameterivEXT := wglGetProcAddress( 'glGetHistogramParameterivEXT');
    glGetMinmaxEXT               := wglGetProcAddress( 'glGetMinmaxEXT');
    glGetMinmaxParameterfvEXT    := wglGetProcAddress( 'glGetMinmaxParameterfvEXT');
    glGetMinmaxParameterivEXT    := wglGetProcAddress( 'glGetMinmaxParameterivEXT');
    glHistogramEXT               := wglGetProcAddress( 'glHistogramEXT');
    glMinmaxEXT                  := wglGetProcAddress( 'glMinmaxEXT');
    glResetHistogramEXT          := wglGetProcAddress( 'glResetHistogramEXT');
    glResetMinmaxEXT             := wglGetProcAddress( 'glResetMinmaxEXT');

    // GL_EXT_index_func =======================================================
    glIndexFuncEXT := wglGetProcAddress( 'glIndexFuncEXT');

    // GL_EXT_index_material ===================================================
    glIndexMaterialEXT := wglGetProcAddress( 'glIndexMaterialEXT');

    // GL_EXT_light_texture ====================================================
    glApplyTextureEXT    := wglGetProcAddress( 'glApplyTextureEXT');
    glTextureLightEXT    := wglGetProcAddress( 'glTextureLightEXT');
    glTextureMaterialEXT := wglGetProcAddress( 'glTextureMaterialEXT');

    // GL_EXT_multi_draw_arrays ================================================
    glMultiDrawArraysEXT   := wglGetProcAddress( 'glMultiDrawArraysEXT');
    glMultiDrawElementsEXT := wglGetProcAddress( 'glMultiDrawElementsEXT');

    // GL_EXT_multisample ======================================================
    glSampleMaskEXT    := wglGetProcAddress( 'glSampleMaskEXT');
    glSamplePatternEXT := wglGetProcAddress( 'glSamplePatternEXT');

    // GL_EXT_paletted_texture =================================================
    glColorTableEXT               := wglGetProcAddress( 'glColorTableEXT');
    glGetColorTableEXT            := wglGetProcAddress( 'glGetColorTableEXT');
    glGetColorTableParameterivEXT := wglGetProcAddress( 'glGetColorTableParameterivEXT');
    glGetColorTableParameterfvEXT := wglGetProcAddress( 'glGetColorTableParameterfvEXT');

    // GL_EXT_pixel_transform ==================================================
    glPixelTransformParameteriEXT  := wglGetProcAddress( 'glPixelTransformParameteriEXT');
    glPixelTransformParameterfEXT  := wglGetProcAddress( 'glPixelTransformParameterfEXT');
    glPixelTransformParameterivEXT := wglGetProcAddress( 'glPixelTransformParameterivEXT');
    glPixelTransformParameterfvEXT := wglGetProcAddress( 'glPixelTransformParameterfvEXT');

    // GL_EXT_point_parameters =================================================
    glPointParameterfEXT  := wglGetProcAddress( 'glPointParameterfEXT');
    glPointParameterfvEXT := wglGetProcAddress( 'glPointParameterfvEXT');

    // GL_EXT_polygon_offset ===================================================
    glPolygonOffsetEXT := wglGetProcAddress( 'glPolygonOffsetEXT');

    // GL_EXT_secondary_color ==================================================
    glSecondaryColor3bEXT      := wglGetProcAddress( 'glSecondaryColor3bEXT');
    glSecondaryColor3bvEXT     := wglGetProcAddress( 'glSecondaryColor3bvEXT');
    glSecondaryColor3dEXT      := wglGetProcAddress( 'glSecondaryColor3dEXT');
    glSecondaryColor3dvEXT     := wglGetProcAddress( 'glSecondaryColor3dvEXT');
    glSecondaryColor3fEXT      := wglGetProcAddress( 'glSecondaryColor3fEXT');
    glSecondaryColor3fvEXT     := wglGetProcAddress( 'glSecondaryColor3fvEXT');
    glSecondaryColor3iEXT      := wglGetProcAddress( 'glSecondaryColor3iEXT');
    glSecondaryColor3ivEXT     := wglGetProcAddress( 'glSecondaryColor3ivEXT');
    glSecondaryColor3sEXT      := wglGetProcAddress( 'glSecondaryColor3sEXT');
    glSecondaryColor3svEXT     := wglGetProcAddress( 'glSecondaryColor3svEXT');
    glSecondaryColor3ubEXT     := wglGetProcAddress( 'glSecondaryColor3ubEXT');
    glSecondaryColor3ubvEXT    := wglGetProcAddress( 'glSecondaryColor3ubvEXT');
    glSecondaryColor3uiEXT     := wglGetProcAddress( 'glSecondaryColor3uiEXT');
    glSecondaryColor3uivEXT    := wglGetProcAddress( 'glSecondaryColor3uivEXT');
    glSecondaryColor3usEXT     := wglGetProcAddress( 'glSecondaryColor3usEXT');
    glSecondaryColor3usvEXT    := wglGetProcAddress( 'glSecondaryColor3usvEXT');
    glSecondaryColorPointerEXT := wglGetProcAddress( 'glSecondaryColorPointerEXT');

    // GL_EXT_stencil_two_side =================================================
    glActiveStencilFaceEXT := wglGetProcAddress( 'glActiveStencilFaceEXT');

    // GL_EXT_subtexture =======================================================
    glTexSubImage1DEXT := wglGetProcAddress( 'glTexSubImage1DEXT');
    glTexSubImage2DEXT := wglGetProcAddress( 'glTexSubImage2DEXT');

    // GL_EXT_texture3D ========================================================
    glTexImage3DEXT    := wglGetProcAddress( 'glTexImage3DEXT');
    glTexSubImage3DEXT := wglGetProcAddress( 'glTexSubImage3DEXT');

    // GL_EXT_texture_object ===================================================
    glAreTexturesResidentEXT := wglGetProcAddress( 'glAreTexturesResidentEXT');
    glBindTextureEXT         := wglGetProcAddress( 'glBindTextureEXT');
    glDeleteTexturesEXT      := wglGetProcAddress( 'glDeleteTexturesEXT');
    glGenTexturesEXT         := wglGetProcAddress( 'glGenTexturesEXT');
    glIsTextureEXT           := wglGetProcAddress( 'glIsTextureEXT');
    glPrioritizeTexturesEXT  := wglGetProcAddress( 'glPrioritizeTexturesEXT');

    // GL_EXT_texture_perturb_normal ===========================================
    glTextureNormalEXT := wglGetProcAddress( 'glTextureNormalEXT');

    // GL_EXT_vertex_array =====================================================
    glArrayElementEXT    := wglGetProcAddress( 'glArrayElementEXT');
    glColorPointerEXT    := wglGetProcAddress( 'glColorPointerEXT');
    glDrawArraysEXT      := wglGetProcAddress( 'glDrawArraysEXT');
    glEdgeFlagPointerEXT := wglGetProcAddress( 'glEdgeFlagPointerEXT');
    glGetPointervEXT     := wglGetProcAddress( 'glGetPointervEXT');
    glIndexPointerEXT    := wglGetProcAddress( 'glIndexPointerEXT');
    glNormalPointerEXT   := wglGetProcAddress( 'glNormalPointerEXT');
    glTexCoordPointerEXT := wglGetProcAddress( 'glTexCoordPointerEXT');
    glVertexPointerEXT   := wglGetProcAddress( 'glVertexPointerEXT');

    // GL_EXT_vertex_shader ====================================================
    glBeginVertexShaderEXT         := wglGetProcAddress( 'glBeginVertexShaderEXT');
    glEndVertexShaderEXT           := wglGetProcAddress( 'glEndVertexShaderEXT');
    glBindVertexShaderEXT          := wglGetProcAddress( 'glBindVertexShaderEXT');
    glGenVertexShadersEXT          := wglGetProcAddress( 'glGenVertexShadersEXT');
    glDeleteVertexShaderEXT        := wglGetProcAddress( 'glDeleteVertexShaderEXT');
    glShaderOp1EXT                 := wglGetProcAddress( 'glShaderOp1EXT');
    glShaderOp2EXT                 := wglGetProcAddress( 'glShaderOp2EXT');
    glShaderOp3EXT                 := wglGetProcAddress( 'glShaderOp3EXT');
    glSwizzleEXT                   := wglGetProcAddress( 'glSwizzleEXT');
    glWriteMaskEXT                 := wglGetProcAddress( 'glWriteMaskEXT');
    glInsertComponentEXT           := wglGetProcAddress( 'glInsertComponentEXT');
    glExtractComponentEXT          := wglGetProcAddress( 'glExtractComponentEXT');
    glGenSymbolsEXT                := wglGetProcAddress( 'glGenSymbolsEXT');
    glSetInvariantEXT              := wglGetProcAddress( 'glSetInvariantEXT');
    glSetLocalConstantEXT          := wglGetProcAddress( 'glSetLocalConstantEXT');
    glVariantbvEXT                 := wglGetProcAddress( 'glVariantbvEXT');
    glVariantsvEXT                 := wglGetProcAddress( 'glVariantsvEXT');
    glVariantivEXT                 := wglGetProcAddress( 'glVariantivEXT');
    glVariantfvEXT                 := wglGetProcAddress( 'glVariantfvEXT');
    glVariantdvEXT                 := wglGetProcAddress( 'glVariantdvEXT');
    glVariantubvEXT                := wglGetProcAddress( 'glVariantubvEXT');
    glVariantusvEXT                := wglGetProcAddress( 'glVariantusvEXT');
    glVariantuivEXT                := wglGetProcAddress( 'glVariantuivEXT');
    glVariantPointerEXT            := wglGetProcAddress( 'glVariantPointerEXT');
    glEnableVariantClientStateEXT  := wglGetProcAddress( 'glEnableVariantClientStateEXT');
    glDisableVariantClientStateEXT := wglGetProcAddress( 'glDisableVariantClientStateEXT');
    glBindLightParameterEXT        := wglGetProcAddress( 'glBindLightParameterEXT');
    glBindMaterialParameterEXT     := wglGetProcAddress( 'glBindMaterialParameterEXT');
    glBindTexGenParameterEXT       := wglGetProcAddress( 'glBindTexGenParameterEXT');
    glBindTextureUnitParameterEXT  := wglGetProcAddress( 'glBindTextureUnitParameterEXT');
    glBindParameterEXT             := wglGetProcAddress( 'glBindParameterEXT');
    glIsVariantEnabledEXT          := wglGetProcAddress( 'glIsVariantEnabledEXT');
    glGetVariantBooleanvEXT        := wglGetProcAddress( 'glGetVariantBooleanvEXT');
    glGetVariantIntegervEXT        := wglGetProcAddress( 'glGetVariantIntegervEXT');
    glGetVariantFloatvEXT          := wglGetProcAddress( 'glGetVariantFloatvEXT');
    glGetVariantPointervEXT        := wglGetProcAddress( 'glGetVariantPointervEXT');
    glGetInvariantBooleanvEXT      := wglGetProcAddress( 'glGetInvariantBooleanvEXT');
    glGetInvariantIntegervEXT      := wglGetProcAddress( 'glGetInvariantIntegervEXT');
    glGetInvariantFloatvEXT        := wglGetProcAddress( 'glGetInvariantFloatvEXT');
    glGetLocalConstantBooleanvEXT  := wglGetProcAddress( 'glGetLocalConstantBooleanvEXT');
    glGetLocalConstantIntegervEXT  := wglGetProcAddress( 'glGetLocalConstantIntegervEXT');
    glGetLocalConstantFloatvEXT    := wglGetProcAddress( 'glGetLocalConstantFloatvEXT');

    // GL_EXT_vertex_weighting =================================================
    glVertexWeightfEXT       := wglGetProcAddress( 'glVertexWeightfEXT');
    glVertexWeightfvEXT      := wglGetProcAddress( 'glVertexWeightfvEXT');
    glVertexWeightPointerEXT := wglGetProcAddress( 'glVertexWeightPointerEXT');

    // GL_HP_image_transform ===================================================
    glImageTransformParameteriHP     := wglGetProcAddress( 'glImageTransformParameteriHP');
    glImageTransformParameterfHP     := wglGetProcAddress( 'glImageTransformParameterfHP');
    glImageTransformParameterivHP    := wglGetProcAddress( 'glImageTransformParameterivHP');
    glImageTransformParameterfvHP    := wglGetProcAddress( 'glImageTransformParameterfvHP');
    glGetImageTransformParameterivHP := wglGetProcAddress( 'glGetImageTransformParameterivHP');
    glGetImageTransformParameterfvHP := wglGetProcAddress( 'glGetImageTransformParameterfvHP');

    // GL_IBM_multimode_draw_arrays ============================================
    glMultiModeDrawArraysIBM   := wglGetProcAddress( 'glMultiModeDrawArraysIBM');
    glMultiModeDrawElementsIBM := wglGetProcAddress( 'glMultiModeDrawElementsIBM');

    // GL_IBM_vertex_array_lists ===============================================
    glColorPointerListIBM          := wglGetProcAddress( 'glColorPointerListIBM');
    glSecondaryColorPointerListIBM := wglGetProcAddress( 'glSecondaryColorPointerListIBM');
    glEdgeFlagPointerListIBM       := wglGetProcAddress( 'glEdgeFlagPointerListIBM');
    glFogCoordPointerListIBM       := wglGetProcAddress( 'glFogCoordPointerListIBM');
    glIndexPointerListIBM          := wglGetProcAddress( 'glIndexPointerListIBM');
    glNormalPointerListIBM         := wglGetProcAddress( 'glNormalPointerListIBM');
    glTexCoordPointerListIBM       := wglGetProcAddress( 'glTexCoordPointerListIBM');
    glVertexPointerListIBM         := wglGetProcAddress( 'glVertexPointerListIBM');

    // GL_INGR_blend_func_separate =============================================
    glBlendFuncSeparateINGR := wglGetProcAddress( 'glBlendFuncSeparateINGR');

    // GL_INTEL_parallel_arrays ================================================
    glVertexPointervINTEL   := wglGetProcAddress( 'glVertexPointervINTEL');
    glNormalPointervINTEL   := wglGetProcAddress( 'glNormalPointervINTEL');
    glColorPointervINTEL    := wglGetProcAddress( 'glColorPointervINTEL');
    glTexCoordPointervINTEL := wglGetProcAddress( 'glTexCoordPointervINTEL');

    // GL_MESA_resize_buffers ==================================================
    glResizeBuffersMESA := wglGetProcAddress( 'glResizeBuffersMESA');

    // GL_MESA_window_pos ======================================================
    glWindowPos2dMESA  := wglGetProcAddress( 'glWindowPos2dMESA');
    glWindowPos2dvMESA := wglGetProcAddress( 'glWindowPos2dvMESA');
    glWindowPos2fMESA  := wglGetProcAddress( 'glWindowPos2fMESA');
    glWindowPos2fvMESA := wglGetProcAddress( 'glWindowPos2fvMESA');
    glWindowPos2iMESA  := wglGetProcAddress( 'glWindowPos2iMESA');
    glWindowPos2ivMESA := wglGetProcAddress( 'glWindowPos2ivMESA');
    glWindowPos2sMESA  := wglGetProcAddress( 'glWindowPos2sMESA');
    glWindowPos2svMESA := wglGetProcAddress( 'glWindowPos2svMESA');
    glWindowPos3dMESA  := wglGetProcAddress( 'glWindowPos3dMESA');
    glWindowPos3dvMESA := wglGetProcAddress( 'glWindowPos3dvMESA');
    glWindowPos3fMESA  := wglGetProcAddress( 'glWindowPos3fMESA');
    glWindowPos3fvMESA := wglGetProcAddress( 'glWindowPos3fvMESA');
    glWindowPos3iMESA  := wglGetProcAddress( 'glWindowPos3iMESA');
    glWindowPos3ivMESA := wglGetProcAddress( 'glWindowPos3ivMESA');
    glWindowPos3sMESA  := wglGetProcAddress( 'glWindowPos3sMESA');
    glWindowPos3svMESA := wglGetProcAddress( 'glWindowPos3svMESA');
    glWindowPos4dMESA  := wglGetProcAddress( 'glWindowPos4dMESA');
    glWindowPos4dvMESA := wglGetProcAddress( 'glWindowPos4dvMESA');
    glWindowPos4fMESA  := wglGetProcAddress( 'glWindowPos4fMESA');
    glWindowPos4fvMESA := wglGetProcAddress( 'glWindowPos4fvMESA');
    glWindowPos4iMESA  := wglGetProcAddress( 'glWindowPos4iMESA');
    glWindowPos4ivMESA := wglGetProcAddress( 'glWindowPos4ivMESA');
    glWindowPos4sMESA  := wglGetProcAddress( 'glWindowPos4sMESA');
    glWindowPos4svMESA := wglGetProcAddress( 'glWindowPos4svMESA');

    // GL_NV_evaluators ========================================================
    glMapControlPointsNV        := wglGetProcAddress( 'glMapControlPointsNV');
    glMapParameterivNV          := wglGetProcAddress( 'glMapParameterivNV');
    glMapParameterfvNV          := wglGetProcAddress( 'glMapParameterfvNV');
    glGetMapControlPointsNV     := wglGetProcAddress( 'glGetMapControlPointsNV');
    glGetMapParameterivNV       := wglGetProcAddress( 'glGetMapParameterivNV');
    glGetMapParameterfvNV       := wglGetProcAddress( 'glGetMapParameterfvNV');
    glGetMapAttribParameterivNV := wglGetProcAddress( 'glGetMapAttribParameterivNV');
    glGetMapAttribParameterfvNV := wglGetProcAddress( 'glGetMapAttribParameterfvNV');
    glEvalMapsNV                := wglGetProcAddress( 'glEvalMapsNV');

    // GL_NV_fence =============================================================
    glDeleteFencesNV := wglGetProcAddress( 'glDeleteFencesNV');
    glGenFencesNV    := wglGetProcAddress( 'glGenFencesNV');
    glIsFenceNV      := wglGetProcAddress( 'glIsFenceNV');
    glTestFenceNV    := wglGetProcAddress( 'glTestFenceNV');
    glGetFenceivNV   := wglGetProcAddress( 'glGetFenceivNV');
    glFinishFenceNV  := wglGetProcAddress( 'glFinishFenceNV');
    glSetFenceNV     := wglGetProcAddress( 'glSetFenceNV');

    // GL_NV_fragment_program ==================================================
    glProgramNamedParameter4fNV    := wglGetProcAddress( 'glProgramNamedParameter4fNV');
    glProgramNamedParameter4dNV    := wglGetProcAddress( 'glProgramNamedParameter4dNV');
    glProgramNamedParameter4fvNV   := wglGetProcAddress( 'glProgramNamedParameter4fvNV');
    glProgramNamedParameter4dvNV   := wglGetProcAddress( 'glProgramNamedParameter4dvNV');
    glGetProgramNamedParameterfvNV := wglGetProcAddress( 'glGetProgramNamedParameterfvNV');
    glGetProgramNamedParameterdvNV := wglGetProcAddress( 'glGetProgramNamedParameterdvNV');

    // GL_NV_half_float ========================================================
    glVertex2hNV          := wglGetProcAddress( 'glVertex2hNV');
    glVertex2hvNV         := wglGetProcAddress( 'glVertex2hvNV');
    glVertex3hNV          := wglGetProcAddress( 'glVertex3hNV');
    glVertex3hvNV         := wglGetProcAddress( 'glVertex3hvNV');
    glVertex4hNV          := wglGetProcAddress( 'glVertex4hNV');
    glVertex4hvNV         := wglGetProcAddress( 'glVertex4hvNV');
    glNormal3hNV          := wglGetProcAddress( 'glNormal3hNV');
    glNormal3hvNV         := wglGetProcAddress( 'glNormal3hvNV');
    glColor3hNV           := wglGetProcAddress( 'glColor3hNV');
    glColor3hvNV          := wglGetProcAddress( 'glColor3hvNV');
    glColor4hNV           := wglGetProcAddress( 'glColor4hNV');
    glColor4hvNV          := wglGetProcAddress( 'glColor4hvNV');
    glTexCoord1hNV        := wglGetProcAddress( 'glTexCoord1hNV');
    glTexCoord1hvNV       := wglGetProcAddress( 'glTexCoord1hvNV');
    glTexCoord2hNV        := wglGetProcAddress( 'glTexCoord2hNV');
    glTexCoord2hvNV       := wglGetProcAddress( 'glTexCoord2hvNV');
    glTexCoord3hNV        := wglGetProcAddress( 'glTexCoord3hNV');
    glTexCoord3hvNV       := wglGetProcAddress( 'glTexCoord3hvNV');
    glTexCoord4hNV        := wglGetProcAddress( 'glTexCoord4hNV');
    glTexCoord4hvNV       := wglGetProcAddress( 'glTexCoord4hvNV');
    glMultiTexCoord1hNV   := wglGetProcAddress( 'glMultiTexCoord1hNV');
    glMultiTexCoord1hvNV  := wglGetProcAddress( 'glMultiTexCoord1hvNV');
    glMultiTexCoord2hNV   := wglGetProcAddress( 'glMultiTexCoord2hNV');
    glMultiTexCoord2hvNV  := wglGetProcAddress( 'glMultiTexCoord2hvNV');
    glMultiTexCoord3hNV   := wglGetProcAddress( 'glMultiTexCoord3hNV');
    glMultiTexCoord3hvNV  := wglGetProcAddress( 'glMultiTexCoord3hvNV');
    glMultiTexCoord4hNV   := wglGetProcAddress( 'glMultiTexCoord4hNV');
    glMultiTexCoord4hvNV  := wglGetProcAddress( 'glMultiTexCoord4hvNV');
    glFogCoordhNV         := wglGetProcAddress( 'glFogCoordhNV');
    glFogCoordhvNV        := wglGetProcAddress( 'glFogCoordhvNV');
    glSecondaryColor3hNV  := wglGetProcAddress( 'glSecondaryColor3hNV');
    glSecondaryColor3hvNV := wglGetProcAddress( 'glSecondaryColor3hvNV');
    glVertexWeighthNV     := wglGetProcAddress( 'glVertexWeighthNV');
    glVertexWeighthvNV    := wglGetProcAddress( 'glVertexWeighthvNV');
    glVertexAttrib1hNV    := wglGetProcAddress( 'glVertexAttrib1hNV');
    glVertexAttrib1hvNV   := wglGetProcAddress( 'glVertexAttrib1hvNV');
    glVertexAttrib2hNV    := wglGetProcAddress( 'glVertexAttrib2hNV');
    glVertexAttrib2hvNV   := wglGetProcAddress( 'glVertexAttrib2hvNV');
    glVertexAttrib3hNV    := wglGetProcAddress( 'glVertexAttrib3hNV');
    glVertexAttrib3hvNV   := wglGetProcAddress( 'glVertexAttrib3hvNV');
    glVertexAttrib4hNV    := wglGetProcAddress( 'glVertexAttrib4hNV');
    glVertexAttrib4hvNV   := wglGetProcAddress( 'glVertexAttrib4hvNV');
    glVertexAttribs1hvNV  := wglGetProcAddress( 'glVertexAttribs1hvNV');
    glVertexAttribs2hvNV  := wglGetProcAddress( 'glVertexAttribs2hvNV');
    glVertexAttribs3hvNV  := wglGetProcAddress( 'glVertexAttribs3hvNV');
    glVertexAttribs4hvNV  := wglGetProcAddress( 'glVertexAttribs4hvNV');

    // GL_NV_occlusion_query ===================================================
    glGenOcclusionQueriesNV    := wglGetProcAddress( 'glGenOcclusionQueriesNV');
    glDeleteOcclusionQueriesNV := wglGetProcAddress( 'glDeleteOcclusionQueriesNV');
    glIsOcclusionQueryNV       := wglGetProcAddress( 'glIsOcclusionQueryNV');
    glBeginOcclusionQueryNV    := wglGetProcAddress( 'glBeginOcclusionQueryNV');
    glEndOcclusionQueryNV      := wglGetProcAddress( 'glEndOcclusionQueryNV');
    glGetOcclusionQueryivNV    := wglGetProcAddress( 'glGetOcclusionQueryivNV');
    glGetOcclusionQueryuivNV   := wglGetProcAddress( 'glGetOcclusionQueryuivNV');

    // GL_NV_pixel_data_range ==================================================
    glPixelDataRangeNV      := wglGetProcAddress( 'glPixelDataRangeNV');
    glFlushPixelDataRangeNV := wglGetProcAddress( 'glFlushPixelDataRangeNV');

    // GL_NV_point_sprite ======================================================
    glPointParameteriNV  := wglGetProcAddress( 'glPointParameteriNV');
    glPointParameterivNV := wglGetProcAddress( 'glPointParameterivNV');

    // GL_NV_primitive_restart =================================================
    glPrimitiveRestartNV      := wglGetProcAddress( 'glPrimitiveRestartNV');
    glPrimitiveRestartIndexNV := wglGetProcAddress( 'glPrimitiveRestartIndexNV');

    // GL_NV_register_combiners ================================================
    glCombinerParameterfvNV              := wglGetProcAddress( 'glCombinerParameterfvNV');
    glCombinerParameterfNV               := wglGetProcAddress( 'glCombinerParameterfNV');
    glCombinerParameterivNV              := wglGetProcAddress( 'glCombinerParameterivNV');
    glCombinerParameteriNV               := wglGetProcAddress( 'glCombinerParameteriNV');
    glCombinerInputNV                    := wglGetProcAddress( 'glCombinerInputNV');
    glCombinerOutputNV                   := wglGetProcAddress( 'glCombinerOutputNV');
    glFinalCombinerInputNV               := wglGetProcAddress( 'glFinalCombinerInputNV');
    glGetCombinerInputParameterfvNV      := wglGetProcAddress( 'glGetCombinerInputParameterfvNV');
    glGetCombinerInputParameterivNV      := wglGetProcAddress( 'glGetCombinerInputParameterivNV');
    glGetCombinerOutputParameterfvNV     := wglGetProcAddress( 'glGetCombinerOutputParameterfvNV');
    glGetCombinerOutputParameterivNV     := wglGetProcAddress( 'glGetCombinerOutputParameterivNV');
    glGetFinalCombinerInputParameterfvNV := wglGetProcAddress( 'glGetFinalCombinerInputParameterfvNV');
    glGetFinalCombinerInputParameterivNV := wglGetProcAddress( 'glGetFinalCombinerInputParameterivNV');

    // GL_NV_register_combiners2 ===============================================
    glCombinerStageParameterfvNV    := wglGetProcAddress( 'glCombinerStageParameterfvNV');
    glGetCombinerStageParameterfvNV := wglGetProcAddress( 'glGetCombinerStageParameterfvNV');

    // GL_NV_vertex_array_range ================================================
    glFlushVertexArrayRangeNV := wglGetProcAddress( 'glFlushVertexArrayRangeNV');
    glVertexArrayRangeNV      := wglGetProcAddress( 'glVertexArrayRangeNV');

    // GL_NV_vertex_program ====================================================
    glAreProgramsResidentNV     := wglGetProcAddress( 'glAreProgramsResidentNV');
    glBindProgramNV             := wglGetProcAddress( 'glBindProgramNV');
    glDeleteProgramsNV          := wglGetProcAddress( 'glDeleteProgramsNV');
    glExecuteProgramNV          := wglGetProcAddress( 'glExecuteProgramNV');
    glGenProgramsNV             := wglGetProcAddress( 'glGenProgramsNV');
    glGetProgramParameterdvNV   := wglGetProcAddress( 'glGetProgramParameterdvNV');
    glGetProgramParameterfvNV   := wglGetProcAddress( 'glGetProgramParameterfvNV');
    glGetProgramivNV            := wglGetProcAddress( 'glGetProgramivNV');
    glGetProgramStringNV        := wglGetProcAddress( 'glGetProgramStringNV');
    glGetTrackMatrixivNV        := wglGetProcAddress( 'glGetTrackMatrixivNV');
    glGetVertexAttribdvNV       := wglGetProcAddress( 'glGetVertexAttribdvNV');
    glGetVertexAttribfvNV       := wglGetProcAddress( 'glGetVertexAttribfvNV');
    glGetVertexAttribivNV       := wglGetProcAddress( 'glGetVertexAttribivNV');
    glGetVertexAttribPointervNV := wglGetProcAddress( 'glGetVertexAttribPointervNV');
    glIsProgramNV               := wglGetProcAddress( 'glIsProgramNV');
    glLoadProgramNV             := wglGetProcAddress( 'glLoadProgramNV');
    glProgramParameter4dNV      := wglGetProcAddress( 'glProgramParameter4dNV');
    glProgramParameter4dvNV     := wglGetProcAddress( 'glProgramParameter4dvNV');
    glProgramParameter4fNV      := wglGetProcAddress( 'glProgramParameter4fNV');
    glProgramParameter4fvNV     := wglGetProcAddress( 'glProgramParameter4fvNV');
    glProgramParameters4dvNV    := wglGetProcAddress( 'glProgramParameters4dvNV');
    glProgramParameters4fvNV    := wglGetProcAddress( 'glProgramParameters4fvNV');
    glRequestResidentProgramsNV := wglGetProcAddress( 'glRequestResidentProgramsNV');
    glTrackMatrixNV             := wglGetProcAddress( 'glTrackMatrixNV');
    glVertexAttribPointerNV     := wglGetProcAddress( 'glVertexAttribPointerNV');
    glVertexAttrib1dNV          := wglGetProcAddress( 'glVertexAttrib1dNV');
    glVertexAttrib1dvNV         := wglGetProcAddress( 'glVertexAttrib1dvNV');
    glVertexAttrib1fNV          := wglGetProcAddress( 'glVertexAttrib1fNV');
    glVertexAttrib1fvNV         := wglGetProcAddress( 'glVertexAttrib1fvNV');
    glVertexAttrib1sNV          := wglGetProcAddress( 'glVertexAttrib1sNV');
    glVertexAttrib1svNV         := wglGetProcAddress( 'glVertexAttrib1svNV');
    glVertexAttrib2dNV          := wglGetProcAddress( 'glVertexAttrib2dNV');
    glVertexAttrib2dvNV         := wglGetProcAddress( 'glVertexAttrib2dvNV');
    glVertexAttrib2fNV          := wglGetProcAddress( 'glVertexAttrib2fNV');
    glVertexAttrib2fvNV         := wglGetProcAddress( 'glVertexAttrib2fvNV');
    glVertexAttrib2sNV          := wglGetProcAddress( 'glVertexAttrib2sNV');
    glVertexAttrib2svNV         := wglGetProcAddress( 'glVertexAttrib2svNV');
    glVertexAttrib3dNV          := wglGetProcAddress( 'glVertexAttrib3dNV');
    glVertexAttrib3dvNV         := wglGetProcAddress( 'glVertexAttrib3dvNV');
    glVertexAttrib3fNV          := wglGetProcAddress( 'glVertexAttrib3fNV');
    glVertexAttrib3fvNV         := wglGetProcAddress( 'glVertexAttrib3fvNV');
    glVertexAttrib3sNV          := wglGetProcAddress( 'glVertexAttrib3sNV');
    glVertexAttrib3svNV         := wglGetProcAddress( 'glVertexAttrib3svNV');
    glVertexAttrib4dNV          := wglGetProcAddress( 'glVertexAttrib4dNV');
    glVertexAttrib4dvNV         := wglGetProcAddress( 'glVertexAttrib4dvNV');
    glVertexAttrib4fNV          := wglGetProcAddress( 'glVertexAttrib4fNV');
    glVertexAttrib4fvNV         := wglGetProcAddress( 'glVertexAttrib4fvNV');
    glVertexAttrib4sNV          := wglGetProcAddress( 'glVertexAttrib4sNV');
    glVertexAttrib4svNV         := wglGetProcAddress( 'glVertexAttrib4svNV');
    glVertexAttrib4ubNV         := wglGetProcAddress( 'glVertexAttrib4ubNV');
    glVertexAttrib4ubvNV        := wglGetProcAddress( 'glVertexAttrib4ubvNV');
    glVertexAttribs1dvNV        := wglGetProcAddress( 'glVertexAttribs1dvNV');
    glVertexAttribs1fvNV        := wglGetProcAddress( 'glVertexAttribs1fvNV');
    glVertexAttribs1svNV        := wglGetProcAddress( 'glVertexAttribs1svNV');
    glVertexAttribs2dvNV        := wglGetProcAddress( 'glVertexAttribs2dvNV');
    glVertexAttribs2fvNV        := wglGetProcAddress( 'glVertexAttribs2fvNV');
    glVertexAttribs2svNV        := wglGetProcAddress( 'glVertexAttribs2svNV');
    glVertexAttribs3dvNV        := wglGetProcAddress( 'glVertexAttribs3dvNV');
    glVertexAttribs3fvNV        := wglGetProcAddress( 'glVertexAttribs3fvNV');
    glVertexAttribs3svNV        := wglGetProcAddress( 'glVertexAttribs3svNV');
    glVertexAttribs4dvNV        := wglGetProcAddress( 'glVertexAttribs4dvNV');
    glVertexAttribs4fvNV        := wglGetProcAddress( 'glVertexAttribs4fvNV');
    glVertexAttribs4svNV        := wglGetProcAddress( 'glVertexAttribs4svNV');
    glVertexAttribs4ubvNV       := wglGetProcAddress( 'glVertexAttribs4ubvNV');

    // GL_PGI_misc_hints =======================================================
    glHintPGI := wglGetProcAddress( 'glHintPGI');

    // GL_SGIS_detail_texture ==================================================
    glDetailTexFuncSGIS    := wglGetProcAddress( 'glDetailTexFuncSGIS');
    glGetDetailTexFuncSGIS := wglGetProcAddress( 'glGetDetailTexFuncSGIS');

    // GL_SGIS_fog_function ====================================================
    glFogFuncSGIS    := wglGetProcAddress( 'glFogFuncSGIS');
    glGetFogFuncSGIS := wglGetProcAddress( 'glGetFogFuncSGIS');

    // GL_SGIS_multisample =====================================================
    glSampleMaskSGIS    := wglGetProcAddress( 'glSampleMaskSGIS');
    glSamplePatternSGIS := wglGetProcAddress( 'glSamplePatternSGIS');

    // GL_SGIS_pixel_texture ===================================================
    glPixelTexGenParameteriSGIS     := wglGetProcAddress( 'glPixelTexGenParameteriSGIS');
    glPixelTexGenParameterivSGIS    := wglGetProcAddress( 'glPixelTexGenParameterivSGIS');
    glPixelTexGenParameterfSGIS     := wglGetProcAddress( 'glPixelTexGenParameterfSGIS');
    glPixelTexGenParameterfvSGIS    := wglGetProcAddress( 'glPixelTexGenParameterfvSGIS');
    glGetPixelTexGenParameterivSGIS := wglGetProcAddress( 'glGetPixelTexGenParameterivSGIS');
    glGetPixelTexGenParameterfvSGIS := wglGetProcAddress( 'glGetPixelTexGenParameterfvSGIS');

    // GL_SGIS_point_parameters ================================================
    glPointParameterfSGIS  := wglGetProcAddress( 'glPointParameterfSGIS');
    glPointParameterfvSGIS := wglGetProcAddress( 'glPointParameterfvSGIS');

    // GL_SGIS_sharpen_texture =================================================
    glSharpenTexFuncSGIS    := wglGetProcAddress( 'glSharpenTexFuncSGIS');
    glGetSharpenTexFuncSGIS := wglGetProcAddress( 'glGetSharpenTexFuncSGIS');

    // GL_SGIS_texture4D =======================================================
    glTexImage4DSGIS    := wglGetProcAddress( 'glTexImage4DSGIS');
    glTexSubImage4DSGIS := wglGetProcAddress( 'glTexSubImage4DSGIS');

    // GL_SGIS_texture_color_mask ==============================================
    glTextureColorMaskSGIS := wglGetProcAddress( 'glTextureColorMaskSGIS');

    // GL_SGIS_texture_filter4 =================================================
    glGetTexFilterFuncSGIS := wglGetProcAddress( 'glGetTexFilterFuncSGIS');
    glTexFilterFuncSGIS    := wglGetProcAddress( 'glTexFilterFuncSGIS');

    // GL_SGIX_async ===========================================================
    glAsyncMarkerSGIX        := wglGetProcAddress( 'glAsyncMarkerSGIX');
    glFinishAsyncSGIX        := wglGetProcAddress( 'glFinishAsyncSGIX');
    glPollAsyncSGIX          := wglGetProcAddress( 'glPollAsyncSGIX');
    glGenAsyncMarkersSGIX    := wglGetProcAddress( 'glGenAsyncMarkersSGIX');
    glDeleteAsyncMarkersSGIX := wglGetProcAddress( 'glDeleteAsyncMarkersSGIX');
    glIsAsyncMarkerSGIX      := wglGetProcAddress( 'glIsAsyncMarkerSGIX');

    // GL_SGIX_flush_raster ====================================================
    glFlushRasterSGIX := wglGetProcAddress( 'glFlushRasterSGIX');

    // GL_SGIX_fragment_lighting ===============================================
    glFragmentColorMaterialSGIX := wglGetProcAddress( 'glFragmentColorMaterialSGIX');
    glFragmentLightfSGIX        := wglGetProcAddress( 'glFragmentLightfSGIX');
    glFragmentLightfvSGIX       := wglGetProcAddress( 'glFragmentLightfvSGIX');
    glFragmentLightiSGIX        := wglGetProcAddress( 'glFragmentLightiSGIX');
    glFragmentLightivSGIX       := wglGetProcAddress( 'glFragmentLightivSGIX');
    glFragmentLightModelfSGIX   := wglGetProcAddress( 'glFragmentLightModelfSGIX');
    glFragmentLightModelfvSGIX  := wglGetProcAddress( 'glFragmentLightModelfvSGIX');
    glFragmentLightModeliSGIX   := wglGetProcAddress( 'glFragmentLightModeliSGIX');
    glFragmentLightModelivSGIX  := wglGetProcAddress( 'glFragmentLightModelivSGIX');
    glFragmentMaterialfSGIX     := wglGetProcAddress( 'glFragmentMaterialfSGIX');
    glFragmentMaterialfvSGIX    := wglGetProcAddress( 'glFragmentMaterialfvSGIX');
    glFragmentMaterialiSGIX     := wglGetProcAddress( 'glFragmentMaterialiSGIX');
    glFragmentMaterialivSGIX    := wglGetProcAddress( 'glFragmentMaterialivSGIX');
    glGetFragmentLightfvSGIX    := wglGetProcAddress( 'glGetFragmentLightfvSGIX');
    glGetFragmentLightivSGIX    := wglGetProcAddress( 'glGetFragmentLightivSGIX');
    glGetFragmentMaterialfvSGIX := wglGetProcAddress( 'glGetFragmentMaterialfvSGIX');
    glGetFragmentMaterialivSGIX := wglGetProcAddress( 'glGetFragmentMaterialivSGIX');
    glLightEnviSGIX             := wglGetProcAddress( 'glLightEnviSGIX');

    // GL_SGIX_framezoom =======================================================
    glFrameZoomSGIX := wglGetProcAddress( 'glFrameZoomSGIX');

    // GL_SGIX_igloo_interface =================================================
    glIglooInterfaceSGIX := wglGetProcAddress( 'glIglooInterfaceSGIX');

    // GL_SGIX_instruments =====================================================
    glGetInstrumentsSGIX    := wglGetProcAddress( 'glGetInstrumentsSGIX');
    glInstrumentsBufferSGIX := wglGetProcAddress( 'glInstrumentsBufferSGIX');
    glPollInstrumentsSGIX   := wglGetProcAddress( 'glPollInstrumentsSGIX');
    glReadInstrumentsSGIX   := wglGetProcAddress( 'glReadInstrumentsSGIX');
    glStartInstrumentsSGIX  := wglGetProcAddress( 'glStartInstrumentsSGIX');
    glStopInstrumentsSGIX   := wglGetProcAddress( 'glStopInstrumentsSGIX');

    // GL_SGIX_list_priority ===================================================
    glGetListParameterfvSGIX := wglGetProcAddress( 'glGetListParameterfvSGIX');
    glGetListParameterivSGIX := wglGetProcAddress( 'glGetListParameterivSGIX');
    glListParameterfSGIX     := wglGetProcAddress( 'glListParameterfSGIX');
    glListParameterfvSGIX    := wglGetProcAddress( 'glListParameterfvSGIX');
    glListParameteriSGIX     := wglGetProcAddress( 'glListParameteriSGIX');
    glListParameterivSGIX    := wglGetProcAddress( 'glListParameterivSGIX');

    // GL_SGIX_pixel_texture ===================================================
    glPixelTexGenSGIX := wglGetProcAddress( 'glPixelTexGenSGIX');

    // GL_SGIX_polynomial_ffd ==================================================
    glDeformationMap3dSGIX           := wglGetProcAddress( 'glDeformationMap3dSGIX');
    glDeformationMap3fSGIX           := wglGetProcAddress( 'glDeformationMap3fSGIX');
    glDeformSGIX                     := wglGetProcAddress( 'glDeformSGIX');
    glLoadIdentityDeformationMapSGIX := wglGetProcAddress( 'glLoadIdentityDeformationMapSGIX');

    // GL_SGIX_reference_plane =================================================
    glReferencePlaneSGIX := wglGetProcAddress( 'glReferencePlaneSGIX');

    // GL_SGIX_sprite ==========================================================
    glSpriteParameterfSGIX  := wglGetProcAddress( 'glSpriteParameterfSGIX');
    glSpriteParameterfvSGIX := wglGetProcAddress( 'glSpriteParameterfvSGIX');
    glSpriteParameteriSGIX  := wglGetProcAddress( 'glSpriteParameteriSGIX');
    glSpriteParameterivSGIX := wglGetProcAddress( 'glSpriteParameterivSGIX');

    // GL_SGIX_tag_sample_buffer ===============================================
    glTagSampleBufferSGIX := wglGetProcAddress( 'glTagSampleBufferSGIX');

    // GL_SGI_color_table ======================================================
    glColorTableSGI               := wglGetProcAddress( 'glColorTableSGI');
    glColorTableParameterfvSGI    := wglGetProcAddress( 'glColorTableParameterfvSGI');
    glColorTableParameterivSGI    := wglGetProcAddress( 'glColorTableParameterivSGI');
    glCopyColorTableSGI           := wglGetProcAddress( 'glCopyColorTableSGI');
    glGetColorTableSGI            := wglGetProcAddress( 'glGetColorTableSGI');
    glGetColorTableParameterfvSGI := wglGetProcAddress( 'glGetColorTableParameterfvSGI');
    glGetColorTableParameterivSGI := wglGetProcAddress( 'glGetColorTableParameterivSGI');

    // GL_SUNX_constant_data ===================================================
    glFinishTextureSUNX := wglGetProcAddress( 'glFinishTextureSUNX');

    // GL_SUN_global_alpha =====================================================
    glGlobalAlphaFactorbSUN  := wglGetProcAddress( 'glGlobalAlphaFactorbSUN');
    glGlobalAlphaFactorsSUN  := wglGetProcAddress( 'glGlobalAlphaFactorsSUN');
    glGlobalAlphaFactoriSUN  := wglGetProcAddress( 'glGlobalAlphaFactoriSUN');
    glGlobalAlphaFactorfSUN  := wglGetProcAddress( 'glGlobalAlphaFactorfSUN');
    glGlobalAlphaFactordSUN  := wglGetProcAddress( 'glGlobalAlphaFactordSUN');
    glGlobalAlphaFactorubSUN := wglGetProcAddress( 'glGlobalAlphaFactorubSUN');
    glGlobalAlphaFactorusSUN := wglGetProcAddress( 'glGlobalAlphaFactorusSUN');
    glGlobalAlphaFactoruiSUN := wglGetProcAddress( 'glGlobalAlphaFactoruiSUN');

    // GL_SUN_mesh_array =======================================================
    glDrawMeshArraysSUN := wglGetProcAddress( 'glDrawMeshArraysSUN');

    // GL_SUN_triangle_list ====================================================
    glReplacementCodeuiSUN      := wglGetProcAddress( 'glReplacementCodeuiSUN');
    glReplacementCodeusSUN      := wglGetProcAddress( 'glReplacementCodeusSUN');
    glReplacementCodeubSUN      := wglGetProcAddress( 'glReplacementCodeubSUN');
    glReplacementCodeuivSUN     := wglGetProcAddress( 'glReplacementCodeuivSUN');
    glReplacementCodeusvSUN     := wglGetProcAddress( 'glReplacementCodeusvSUN');
    glReplacementCodeubvSUN     := wglGetProcAddress( 'glReplacementCodeubvSUN');
    glReplacementCodePointerSUN := wglGetProcAddress( 'glReplacementCodePointerSUN');

    // GL_SUN_vertex ===========================================================
    glColor4ubVertex2fSUN                                    := wglGetProcAddress( 'glColor4ubVertex2fSUN');
    glColor4ubVertex2fvSUN                                   := wglGetProcAddress( 'glColor4ubVertex2fvSUN');
    glColor4ubVertex3fSUN                                    := wglGetProcAddress( 'glColor4ubVertex3fSUN');
    glColor4ubVertex3fvSUN                                   := wglGetProcAddress( 'glColor4ubVertex3fvSUN');
    glColor3fVertex3fSUN                                     := wglGetProcAddress( 'glColor3fVertex3fSUN');
    glColor3fVertex3fvSUN                                    := wglGetProcAddress( 'glColor3fVertex3fvSUN');
    glNormal3fVertex3fSUN                                    := wglGetProcAddress( 'glNormal3fVertex3fSUN');
    glNormal3fVertex3fvSUN                                   := wglGetProcAddress( 'glNormal3fVertex3fvSUN');
    glColor4fNormal3fVertex3fSUN                             := wglGetProcAddress( 'glColor4fNormal3fVertex3fSUN');
    glColor4fNormal3fVertex3fvSUN                            := wglGetProcAddress( 'glColor4fNormal3fVertex3fvSUN');
    glTexCoord2fVertex3fSUN                                  := wglGetProcAddress( 'glTexCoord2fVertex3fSUN');
    glTexCoord2fVertex3fvSUN                                 := wglGetProcAddress( 'glTexCoord2fVertex3fvSUN');
    glTexCoord4fVertex4fSUN                                  := wglGetProcAddress( 'glTexCoord4fVertex4fSUN');
    glTexCoord4fVertex4fvSUN                                 := wglGetProcAddress( 'glTexCoord4fVertex4fvSUN');
    glTexCoord2fColor4ubVertex3fSUN                          := wglGetProcAddress( 'glTexCoord2fColor4ubVertex3fSUN');
    glTexCoord2fColor4ubVertex3fvSUN                         := wglGetProcAddress( 'glTexCoord2fColor4ubVertex3fvSUN');
    glTexCoord2fColor3fVertex3fSUN                           := wglGetProcAddress( 'glTexCoord2fColor3fVertex3fSUN');
    glTexCoord2fColor3fVertex3fvSUN                          := wglGetProcAddress( 'glTexCoord2fColor3fVertex3fvSUN');
    glTexCoord2fNormal3fVertex3fSUN                          := wglGetProcAddress( 'glTexCoord2fNormal3fVertex3fSUN');
    glTexCoord2fNormal3fVertex3fvSUN                         := wglGetProcAddress( 'glTexCoord2fNormal3fVertex3fvSUN');
    glTexCoord2fColor4fNormal3fVertex3fSUN                   := wglGetProcAddress( 'glTexCoord2fColor4fNormal3fVertex3fSUN');
    glTexCoord2fColor4fNormal3fVertex3fvSUN                  := wglGetProcAddress( 'glTexCoord2fColor4fNormal3fVertex3fvSUN');
    glTexCoord4fColor4fNormal3fVertex4fSUN                   := wglGetProcAddress( 'glTexCoord4fColor4fNormal3fVertex4fSUN');
    glTexCoord4fColor4fNormal3fVertex4fvSUN                  := wglGetProcAddress( 'glTexCoord4fColor4fNormal3fVertex4fvSUN');
    glReplacementCodeuiVertex3fSUN                           := wglGetProcAddress( 'glReplacementCodeuiVertex3fSUN');
    glReplacementCodeuiVertex3fvSUN                          := wglGetProcAddress( 'glReplacementCodeuiVertex3fvSUN');
    glReplacementCodeuiColor4ubVertex3fSUN                   := wglGetProcAddress( 'glReplacementCodeuiColor4ubVertex3fSUN');
    glReplacementCodeuiColor4ubVertex3fvSUN                  := wglGetProcAddress( 'glReplacementCodeuiColor4ubVertex3fvSUN');
    glReplacementCodeuiColor3fVertex3fSUN                    := wglGetProcAddress( 'glReplacementCodeuiColor3fVertex3fSUN');
    glReplacementCodeuiColor3fVertex3fvSUN                   := wglGetProcAddress( 'glReplacementCodeuiColor3fVertex3fvSUN');
    glReplacementCodeuiNormal3fVertex3fSUN                   := wglGetProcAddress( 'glReplacementCodeuiNormal3fVertex3fSUN');
    glReplacementCodeuiNormal3fVertex3fvSUN                  := wglGetProcAddress( 'glReplacementCodeuiNormal3fVertex3fvSUN');
    glReplacementCodeuiColor4fNormal3fVertex3fSUN            := wglGetProcAddress( 'glReplacementCodeuiColor4fNormal3fVertex3fSUN');
    glReplacementCodeuiColor4fNormal3fVertex3fvSUN           := wglGetProcAddress( 'glReplacementCodeuiColor4fNormal3fVertex3fvSUN');
    glReplacementCodeuiTexCoord2fVertex3fSUN                 := wglGetProcAddress( 'glReplacementCodeuiTexCoord2fVertex3fSUN');
    glReplacementCodeuiTexCoord2fVertex3fvSUN                := wglGetProcAddress( 'glReplacementCodeuiTexCoord2fVertex3fvSUN');
    glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN         := wglGetProcAddress( 'glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN');
    glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN        := wglGetProcAddress( 'glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN');
    glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN  := wglGetProcAddress( 'glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN');
    glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN := wglGetProcAddress( 'glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN');

    // WGL_ARB_buffer_region ===================================================
    wglCreateBufferRegionARB         := wglGetProcAddress('wglCreateBufferRegionARB');
    wglDeleteBufferRegionARB         := wglGetProcAddress('wglDeleteBufferRegionARB');
    wglSaveBufferRegionARB           := wglGetProcAddress('wglSaveBufferRegionARB');
    wglRestoreBufferRegionARB        := wglGetProcAddress('wglRestoreBufferRegionARB');
    // WGL_ARB_extensions_string ===============================================
    wglGetExtensionsStringARB        := wglGetProcAddress('wglGetExtensionsStringARB');
    // WGL_ARB_make_current_read ===============================================
    wglMakeContextCurrentARB         := wglGetProcAddress('wglMakeContextCurrentARB');
    wglGetCurrentReadDCARB           := wglGetProcAddress('wglGetCurrentReadDCARB');
    // WGL_ARB_pbuffer =========================================================
    wglCreatePbufferARB              := wglGetProcAddress('wglCreatePbufferARB');
    wglGetPbufferDCARB               := wglGetProcAddress('wglGetPbufferDCARB');
    wglReleasePbufferDCARB           := wglGetProcAddress('wglReleasePbufferDCARB');
    wglDestroyPbufferARB             := wglGetProcAddress('wglDestroyPbufferARB');
    wglQueryPbufferARB               := wglGetProcAddress('wglQueryPbufferARB');
    // WGL_ARB_pixel_format ====================================================
    wglGetPixelFormatAttribivARB     := wglGetProcAddress('wglGetPixelFormatAttribivARB');
    wglGetPixelFormatAttribfvARB     := wglGetProcAddress('wglGetPixelFormatAttribfvARB');
    wglChoosePixelFormatARB          := wglGetProcAddress('wglChoosePixelFormatARB');
    // WGL_ARB_render_texture ==================================================
    wglBindTexImageARB               := wglGetProcAddress('wglBindTexImageARB');
    wglReleaseTexImageARB            := wglGetProcAddress('wglReleaseTexImageARB');
    wglSetPbufferAttribARB           := wglGetProcAddress('wglSetPbufferAttribARB');
    // WGL_EXT_display_color_table =============================================
    wglCreateDisplayColorTableEXT    := wglGetProcAddress('wglCreateDisplayColorTableEXT');
    wglLoadDisplayColorTableEXT      := wglGetProcAddress('wglLoadDisplayColorTableEXT');
    wglBindDisplayColorTableEXT      := wglGetProcAddress('wglBindDisplayColorTableEXT');
    wglDestroyDisplayColorTableEXT   := wglGetProcAddress('wglDestroyDisplayColorTableEXT');
    // WGL_EXT_extensions_string ===============================================
    wglGetExtensionsStringEXT        := wglGetProcAddress('wglGetExtensionsStringEXT');
    // WGL_EXT_make_current_read ===============================================
    wglMakeContextCurrentEXT         := wglGetProcAddress('wglMakeContextCurrentEXT');
    wglGetCurrentReadDCEXT           := wglGetProcAddress('wglGetCurrentReadDCEXT');
    // WGL_EXT_pbuffer =========================================================
    wglCreatePbufferEXT              := wglGetProcAddress('wglCreatePbufferEXT');
    wglGetPbufferDCEXT               := wglGetProcAddress('wglGetPbufferDCEXT');
    wglReleasePbufferDCEXT           := wglGetProcAddress('wglReleasePbufferDCEXT');
    wglDestroyPbufferEXT             := wglGetProcAddress('wglDestroyPbufferEXT');
    wglQueryPbufferEXT               := wglGetProcAddress('wglQueryPbufferEXT');
    // WGL_EXT_pixel_format ====================================================
    wglGetPixelFormatAttribivEXT     := wglGetProcAddress('wglGetPixelFormatAttribivEXT');
    wglGetPixelFormatAttribfvEXT     := wglGetProcAddress('wglGetPixelFormatAttribfvEXT');
    wglChoosePixelFormatEXT          := wglGetProcAddress('wglChoosePixelFormatEXT');
    // WGL_EXT_swap_control ====================================================
    wglSwapIntervalEXT               := wglGetProcAddress('wglSwapIntervalEXT');
    wglGetSwapIntervalEXT            := wglGetProcAddress('wglGetSwapIntervalEXT');
    // WGL_I3D_digital_video_control ===========================================
    wglGetDigitalVideoParametersI3D  := wglGetProcAddress('wglGetDigitalVideoParametersI3D');
    wglSetDigitalVideoParametersI3D  := wglGetProcAddress('wglSetDigitalVideoParametersI3D');
    // WGL_I3D_gamma ===========================================================
    wglGetGammaTableParametersI3D    := wglGetProcAddress('wglGetGammaTableParametersI3D');
    wglSetGammaTableParametersI3D    := wglGetProcAddress('wglSetGammaTableParametersI3D');
    wglGetGammaTableI3D              := wglGetProcAddress('wglGetGammaTableI3D');
    wglSetGammaTableI3D              := wglGetProcAddress('wglSetGammaTableI3D');
    // WGL_I3D_genlock =========================================================
    wglEnableGenlockI3D              := wglGetProcAddress('wglEnableGenlockI3D');
    wglDisableGenlockI3D             := wglGetProcAddress('wglDisableGenlockI3D');
    wglIsEnabledGenlockI3D           := wglGetProcAddress('wglIsEnabledGenlockI3D');
    wglGenlockSourceI3D              := wglGetProcAddress('wglGenlockSourceI3D');
    wglGetGenlockSourceI3D           := wglGetProcAddress('wglGetGenlockSourceI3D');
    wglGenlockSourceEdgeI3D          := wglGetProcAddress('wglGenlockSourceEdgeI3D');
    wglGetGenlockSourceEdgeI3D       := wglGetProcAddress('wglGetGenlockSourceEdgeI3D');
    wglGenlockSampleRateI3D          := wglGetProcAddress('wglGenlockSampleRateI3D');
    wglGetGenlockSampleRateI3D       := wglGetProcAddress('wglGetGenlockSampleRateI3D');
    wglGenlockSourceDelayI3D         := wglGetProcAddress('wglGenlockSourceDelayI3D');
    wglGetGenlockSourceDelayI3D      := wglGetProcAddress('wglGetGenlockSourceDelayI3D');
    wglQueryGenlockMaxSourceDelayI3D := wglGetProcAddress('wglQueryGenlockMaxSourceDelayI3D');
    // WGL_I3D_image_buffer ====================================================
    wglCreateImageBufferI3D          := wglGetProcAddress('wglCreateImageBufferI3D');
    wglDestroyImageBufferI3D         := wglGetProcAddress('wglDestroyImageBufferI3D');
    wglAssociateImageBufferEventsI3D := wglGetProcAddress('wglAssociateImageBufferEventsI3D');
    wglReleaseImageBufferEventsI3D   := wglGetProcAddress('wglReleaseImageBufferEventsI3D');
    // WGL_I3D_swap_frame_lock =================================================
    wglEnableFrameLockI3D            := wglGetProcAddress('wglEnableFrameLockI3D');
    wglDisableFrameLockI3D           := wglGetProcAddress('wglDisableFrameLockI3D');
    wglIsEnabledFrameLockI3D         := wglGetProcAddress('wglIsEnabledFrameLockI3D');
    wglQueryFrameLockMasterI3D       := wglGetProcAddress('wglQueryFrameLockMasterI3D');
    // WGL_I3D_swap_frame_usage ================================================
    wglGetFrameUsageI3D              := wglGetProcAddress('wglGetFrameUsageI3D');
    wglBeginFrameTrackingI3D         := wglGetProcAddress('wglBeginFrameTrackingI3D');
    wglEndFrameTrackingI3D           := wglGetProcAddress('wglEndFrameTrackingI3D');
    wglQueryFrameTrackingI3D         := wglGetProcAddress('wglQueryFrameTrackingI3D');
    // WGL_NV_vertex_array_range ===============================================
    wglAllocateMemoryNV              := wglGetProcAddress('wglAllocateMemoryNV');
    wglFreeMemoryNV                  := wglGetProcAddress('wglFreeMemoryNV');
    // WGL_OML_sync_control ====================================================
    wglGetSyncValuesOML              := wglGetProcAddress('wglGetSyncValuesOML');
    wglGetMscRateOML                 := wglGetProcAddress('wglGetMscRateOML');
    wglSwapBuffersMscOML             := wglGetProcAddress('wglSwapBuffersMscOML');
    wglSwapLayerBuffersMscOML        := wglGetProcAddress('wglSwapLayerBuffersMscOML');
    wglWaitForMscOML                 := wglGetProcAddress('wglWaitForMscOML');
    wglWaitForSbcOML                 := wglGetProcAddress('wglWaitForSbcOML');

    // WIN_draw_range_elements =================================================
    glDrawRangeElementsWIN           := wglGetProcAddress( 'glDrawRangeElementsWIN');
    // WIN_swap_hint ===========================================================
    glAddSwapHintRectWIN             := wglGetProcAddress( 'glAddSwapHintRectWIN');

    // ==== OpenGL 1.5 =========================================================
    // GL_ARB_Shader_Objects ===================================================
    glCreateShaderObjectARB      := wglGetProcAddress('glCreateShaderObjectARB');
    glShaderSourceARB            := wglGetProcAddress('glShaderSourceARB');
    glCompileShaderARB           := wglGetProcAddress('glCompileShaderARB');
    glDeleteObjectARB            := wglGetProcAddress('glDeleteObjectARB');
    glGetHandleARB               := wglGetProcAddress('glGetHandleARB');
    glDetachObjectARB            := wglGetProcAddress('glDetachObjectARB');
    glCreateProgramObjectARB     := wglGetProcAddress('glCreateProgramObjectARB');
    glAttachObjectARB            := wglGetProcAddress('glAttachObjectARB');
    glLinkProgramARB             := wglGetProcAddress('glLinkProgramARB');
    glUseProgramObjectARB        := wglGetProcAddress('glUseProgramObjectARB');
    glValidateProgramARB         := wglGetProcAddress('glValidateProgramARB');
    glGetObjectParameterfvARB    := wglGetProcAddress('glGetObjectParameterfvARB');
    glGetObjectParameterivARB    := wglGetProcAddress('glGetObjectParameterivARB');
    glGetActiveUniformARB        := wglGetProcAddress('glGetActiveUniformARB');
    glGetAttachedObjectsARB      := wglGetProcAddress('glGetAttachedObjectsARB');
    glGetShaderSourceARB         := wglGetProcAddress('glGetShaderSourceARB');
    glGetUniformfvARB            := wglGetProcAddress('glGetUniformfvARB');
    glGetUniformivARB            := wglGetProcAddress('glGetUniformivARB');
    glGetUniformLocationARB      := wglGetProcAddress('glGetUniformLocationARB');
    glGetInfoLogARB              := wglGetProcAddress('glGetInfoLogARB');
    glUniform1fARB               := wglGetProcAddress('glUniform1fARB');
    glUniform2fARB               := wglGetProcAddress('glUniform2fARB');
    glUniform3fARB               := wglGetProcAddress('glUniform3fARB');
    glUniform4fARB               := wglGetProcAddress('glUniform4fARB');
    glUniform1iARB               := wglGetProcAddress('glUniform1iARB');
    glUniform2iARB               := wglGetProcAddress('glUniform2iARB');
    glUniform3iARB               := wglGetProcAddress('glUniform3iARB');
    glUniform4iARB               := wglGetProcAddress('glUniform4iARB');
    glUniform1fvARB              := wglGetProcAddress('glUniform1fvARB');
    glUniform2fvARB              := wglGetProcAddress('glUniform2fvARB');
    glUniform3fvARB              := wglGetProcAddress('glUniform3fvARB');
    glUniform4fvARB              := wglGetProcAddress('glUniform4fvARB');
    glUniform1ivARB              := wglGetProcAddress('glUniform1ivARB');
    glUniform2ivARB              := wglGetProcAddress('glUniform2ivARB');
    glUniform3ivARB              := wglGetProcAddress('glUniform3ivARB');
    glUniform4ivARB              := wglGetProcAddress('glUniform4ivARB');
    glUniformMatrix2fvARB        := wglGetProcAddress('glUniformMatrix2fvARB');
    glUniformMatrix3fvARB        := wglGetProcAddress('glUniformMatrix3fvARB');
    glUniformMatrix4fvARB        := wglGetProcAddress('glUniformMatrix4fvARB');
    // GL_ARB_vertex_shader ====================================================
    glGetActiveAttribARB         := wglGetProcAddress('glGetActiveAttribARB');
    glGetAttribLocationARB       := wglGetProcAddress('glGetAttribLocationARB');
    glBindAttribLocationARB      := wglGetProcAddress('glBindAttribLocationARB');
    glGetVertexAttribPointervARB := wglGetProcAddress('glGetVertexAttribPointervARB');
    // GL_ARB_occlusion_query ==================================================
    glGenQueriesARB              := wglGetProcAddress('glGenQueriesARB');
    glDeleteQueriesARB           := wglGetProcAddress('glDeleteQueriesARB');
    glIsQueryARB                 := wglGetProcAddress('glIsQueryARB');
    glBeginQueryARB              := wglGetProcAddress('glBeginQueryARB');
    glEndQueryARB                := wglGetProcAddress('glEndQueryARB');
    glGetQueryivARB              := wglGetProcAddress('glGetQueryivARB');
    glGetQueryObjectivARB        := wglGetProcAddress('glGetQueryObjectivARB');
    glGetQueryObjectuivARB       := wglGetProcAddress('glGetQueryObjectuivARB');
    // ARB less version for GL 1.5==============================================
    glGenQueries              := wglGetProcAddress('glGenQueries');
    glDeleteQueries           := wglGetProcAddress('glDeleteQueries');
    glIsQuery                 := wglGetProcAddress('glIsQuery');
    glBeginQuery              := wglGetProcAddress('glBeginQuery');
    glEndQuery                := wglGetProcAddress('glEndQuery');
    glGetQueryiv              := wglGetProcAddress('glGetQueryiv');
    glGetQueryObjectiv        := wglGetProcAddress('glGetQueryObjectiv');
    glGetQueryObjectuiv       := wglGetProcAddress('glGetQueryObjectuiv');

    ExtensionsRead               := True;
  end;
end;



// =============================================================================
//  ReadImplementationProperties
// =============================================================================
procedure ReadImplementationProperties;
var
 Buffer                     : string;
 MajorVersion, MinorVersion : Integer;

  procedure TrimAndSplitVersionString(Buffer: String; var Max, Min: Integer);
  // Peels out the X.Y form from the given Buffer which must contain a version string like "text Minor.Major.Build text"
  // at least however "Major.Minor".
  var
    Separator: Integer;
  begin
    try
      // There must be at least one dot to separate major and minor version number.
      Separator := Pos('.', Buffer);
      // At least one number must be before and one after the dot.
      if (Separator > 1) and (Separator < Length(Buffer)) and (Buffer[Separator - 1] in ['0'..'9']) and
        (Buffer[Separator + 1] in ['0'..'9']) then
      begin
        // OK, it's a valid version string. Now remove unnecessary parts.
        Dec(Separator);
        // Find last non-numeric character before version number.
        while (Separator > 0) and (Buffer[Separator] in ['0'..'9']) do
          Dec(Separator);
        // Delete leading characters which do not belong to the version string.
        Delete(Buffer, 1, Separator);
        Separator := Pos('.', Buffer) + 1;
        // Find first non-numeric character after version number
        while (Separator <= Length(Buffer)) and (Buffer[Separator] in ['0'..'9']) do
          Inc(Separator);
        // delete trailing characters not belonging to the version string
        Delete(Buffer, Separator, 255);
        // Now translate the numbers.
        Separator := Pos('.', Buffer); // This is necessary because the buffer length might have changed.
        Max := StrToInt(Copy(Buffer, 1, Separator - 1));
        Min := StrToInt(Copy(Buffer, Separator + 1, 255));
      end
      else
        Abort;
    except
      Min := 0;
      Max := 0;
    end;
  end;

  // Checks if the given Extension string is in Buffer.
  function CheckExtension(const Extension: string): Boolean;
  var
    ExtPos: Integer;
  begin
    // First find the position of the extension string as substring in Buffer.
    ExtPos := Pos(Extension, Buffer);
    Result := ExtPos > 0;
    // Now check that it isn't only a substring of another extension.
    if Result then
      Result := ((ExtPos + Length(Extension) - 1) = Length(Buffer)) or
        not (Buffer[ExtPos + Length(Extension)] in ['_', 'A'..'Z', 'a'..'z']);
  end;
begin
// determine version of implementation
// GL
Buffer := glGetString(GL_VERSION);
TrimAndSplitVersionString(Buffer, Majorversion, MinorVersion);
GL_VERSION_1_0 := True;
GL_VERSION_1_1 := False;
GL_VERSION_1_2 := False;
GL_VERSION_1_3 := False;
GL_VERSION_1_4 := False;
GL_VERSION_1_5 := False;
if MajorVersion > 0 then
 begin
 if MinorVersion >= 1 then
  begin
  GL_VERSION_1_1 := True;
  if MinorVersion >= 2
   then GL_VERSION_1_2 := True;
  if MinorVersion >= 3
   then GL_VERSION_1_3 := True;
  if MinorVersion >= 4
   then GL_VERSION_1_4 := True;
  if MinorVersion >= 5
   then GL_VERSION_1_5 := True;
  end;
 end;
// GLU
GLU_VERSION_1_1 := False;
GLU_VERSION_1_2 := False;
GLU_VERSION_1_3 := False;
// gluGetString is valid for version 1.1 or later
if Assigned(gluGetString) then
 begin
 Buffer := gluGetString(GLU_VERSION);
 TrimAndSplitVersionString(Buffer, Majorversion, MinorVersion);
 GLU_VERSION_1_1 := True;
 if MinorVersion >= 2
  then GLU_VERSION_1_2 := True;
 if MinorVersion >= 3
  then GLU_VERSION_1_3 := True;
 end;
// check supported extensions
Buffer := glGetString(GL_EXTENSIONS);
if (LibHandle<>0) then
 begin
 wglGetExtensionsStringEXT := glProcedure('wglGetExtensionsStringEXT');
 if Assigned(@wglGetExtensionsStringEXT) then
  Buffer := Buffer + ' ' + wglGetExtensionsStringEXT
 else
  begin
  wglGetExtensionsStringARB := glProcedure('wglGetExtensionsStringARB');
  if Assigned(@wglGetExtensionsStringARB) then
   Buffer := Buffer + ' ' + wglGetExtensionsStringARB(wglGetCurrentDC);
  end;
 end;
// Check all extensions
// === 3DFX ====================================================================
GL_3DFX_multisample                := CheckExtension('GL_3DFX_multisample');
GL_3DFX_tbuffer                    := CheckExtension('GL_3DFX_tbuffer');
GL_3DFX_texture_compression_FXT1   := CheckExtension('GL_3DFX_texture_compression_FXT1');
// === APPLE ===================================================================
GL_APPLE_client_storage            := CheckExtension('GL_APPLE_client_storage');
GL_APPLE_element_array             := CheckExtension('GL_APPLE_element_array');
GL_APPLE_fence                     := CheckExtension('GL_APPLE_fence');
GL_APPLE_specular_vector           := CheckExtension('GL_APPLE_specular_vector');
GL_APPLE_transform_hint            := CheckExtension('GL_APPLE_transform_hint');
GL_APPLE_vertex_array_object       := CheckExtension('GL_APPLE_vertex_array_object');
GL_APPLE_vertex_array_range        := CheckExtension('GL_APPLE_vertex_array_range');
GL_APPLE_ycbcr_422                 := CheckExtension('GL_APPLE_ycbcr_422');
// === ARB =====================================================================
GL_ARB_depth_texture               := CheckExtension('GL_ARB_depth_texture');
GL_ARB_fragment_program            := CheckExtension('GL_ARB_fragment_program');
GL_ARB_imaging                     := CheckExtension('GL_ARB_imaging');
GL_ARB_matrix_palette              := CheckExtension('GL_ARB_matrix_palette');
GL_ARB_multisample                 := CheckExtension('GL_ARB_multisample');
GL_ARB_multitexture                := CheckExtension('GL_ARB_multitexture');
GL_ARB_point_parameters            := CheckExtension('GL_ARB_point_parameters');
GL_ARB_shadow                      := CheckExtension('GL_ARB_shadow');
GL_ARB_shadow_ambient              := CheckExtension('GL_ARB_shadow_ambient');
GL_ARB_texture_border_clamp        := CheckExtension('GL_ARB_texture_border_clamp');
GL_ARB_texture_compression         := CheckExtension('GL_ARB_texture_compression');
GL_ARB_texture_cube_map            := CheckExtension('GL_ARB_texture_cube_map');
GL_ARB_texture_env_add             := CheckExtension('GL_ARB_texture_env_add');
GL_ARB_texture_env_combine         := CheckExtension('GL_ARB_texture_env_combine');
GL_ARB_texture_env_crossbar        := CheckExtension('GL_ARB_texture_env_crossbar');
GL_ARB_texture_env_dot3            := CheckExtension('GL_ARB_texture_env_dot3');
GL_ARB_texture_mirror_repeat       := CheckExtension('GL_ARB_texture_mirror_repeat');
GL_ARB_texture_mirrored_repeat     := CheckExtension('GL_ARB_texture_mirrored_repeat');
GL_ARB_transpose_matrix            := CheckExtension('GL_ARB_transpose_matrix');
GL_ARB_vertex_blend                := CheckExtension('GL_ARB_vertex_blend');
GL_ARB_vertex_buffer_object        := CheckExtension('GL_ARB_vertex_buffer_object');
GL_ARB_vertex_program              := CheckExtension('GL_ARB_vertex_program');
GL_ARB_window_pos                  := CheckExtension('GL_ARB_window_pos');
GL_ARB_shader_objects              := CheckExtension('GL_ARB_shader_objects');
GL_ARB_vertex_shader               := CheckExtension('GL_ARB_vertex_shader');
GL_ARB_fragment_shader             := CheckExtension('GL_ARB_fragment_shader');
GL_ARB_occlusion_query             := CheckExtension('GL_ARB_occlusion_query');
GL_ARB_shading_language_100        := CheckExtension('GL_ARB_shading_language_100');
GL_ARB_point_sprite                := CheckExtension('GL_ARB_point_sprite');
GL_ARB_texture_non_power_of_two    := CheckExtension('GL_ARB_texture_non_power_of_two');
// === ATI =====================================================================
GL_ATI_draw_buffers                := CheckExtension('GL_ATI_draw_buffers');
GL_ATI_element_array               := CheckExtension('GL_ATI_element_array');
GL_ATI_envmap_bumpmap              := CheckExtension('GL_ATI_envmap_bumpmap');
GL_ATI_fragment_shader             := CheckExtension('GL_ATI_fragment_shader');
GL_ATI_map_object_buffer           := CheckExtension('GL_ATI_map_object_buffer');
GL_ATI_pn_triangles                := CheckExtension('GL_ATI_pn_triangles');
GL_ATI_separate_stencil            := CheckExtension('GL_ATI_separate_stencil');
GL_ATI_text_fragment_shader        := CheckExtension('GL_ATI_text_fragment_shader');
GL_ATI_texture_env_combine3        := CheckExtension('GL_ATI_texture_env_combine3');
GL_ATI_texture_float               := CheckExtension('GL_ATI_texture_float');
GL_ATI_texture_mirror_once         := CheckExtension('GL_ATI_texture_mirror_once');
GL_ATI_vertex_array_object         := CheckExtension('GL_ATI_vertex_array_object');
GL_ATI_vertex_attrib_array_object  := CheckExtension('GL_ATI_vertex_attrib_array_object');
GL_ATI_vertex_streams              := CheckExtension('GL_ATI_vertex_streams');
// === EXT =====================================================================
GL_EXT_422_pixels                  := CheckExtension('GL_EXT_422_pixels');
GL_EXT_abgr                        := CheckExtension('GL_EXT_abgr');
GL_EXT_bgra                        := CheckExtension('GL_EXT_bgra');
GL_EXT_blend_color                 := CheckExtension('GL_EXT_blend_color');
GL_EXT_blend_func_separate         := CheckExtension('GL_EXT_blend_func_separate');
GL_EXT_blend_logic_op              := CheckExtension('GL_EXT_blend_logic_op');
GL_EXT_blend_minmax                := CheckExtension('GL_EXT_blend_minmax');
GL_EXT_blend_subtract              := CheckExtension('GL_EXT_blend_subtract');
GL_EXT_clip_volume_hint            := CheckExtension('GL_EXT_clip_volume_hint');
GL_EXT_cmyka                       := CheckExtension('GL_EXT_cmyka');
GL_EXT_color_matrix                := CheckExtension('GL_EXT_color_matrix');
GL_EXT_color_subtable              := CheckExtension('GL_EXT_color_subtable');
GL_EXT_compiled_vertex_array       := CheckExtension('GL_EXT_compiled_vertex_array');
GL_EXT_convolution                 := CheckExtension('GL_EXT_convolution');
GL_EXT_coordinate_frame            := CheckExtension('GL_EXT_coordinate_frame');
GL_EXT_copy_texture                := CheckExtension('GL_EXT_copy_texture');
GL_EXT_cull_vertex                 := CheckExtension('GL_EXT_cull_vertex');
GL_EXT_draw_range_elements         := CheckExtension('GL_EXT_draw_range_elements');
GL_EXT_fog_coord                   := CheckExtension('GL_EXT_fog_coord');
GL_EXT_histogram                   := CheckExtension('GL_EXT_histogram');
GL_EXT_index_array_formats         := CheckExtension('GL_EXT_index_array_formats');
GL_EXT_index_func                  := CheckExtension('GL_EXT_index_func');
GL_EXT_index_material              := CheckExtension('GL_EXT_index_material');
GL_EXT_index_texture               := CheckExtension('GL_EXT_index_texture');
GL_EXT_light_texture               := CheckExtension('GL_EXT_light_texture');
GL_EXT_misc_attribute              := CheckExtension('GL_EXT_misc_attribute');
GL_EXT_multi_draw_arrays           := CheckExtension('GL_EXT_multi_draw_arrays');
GL_EXT_multisample                 := CheckExtension('GL_EXT_multisample');
GL_EXT_packed_pixels               := CheckExtension('GL_EXT_packed_pixels');
GL_EXT_paletted_texture            := CheckExtension('GL_EXT_paletted_texture');
GL_EXT_pixel_transform             := CheckExtension('GL_EXT_pixel_transform');
GL_EXT_pixel_transform_color_table := CheckExtension('GL_EXT_pixel_transform_color_table');
GL_EXT_point_parameters            := CheckExtension('GL_EXT_point_parameters');
GL_EXT_polygon_offset              := CheckExtension('GL_EXT_polygon_offset');
GL_EXT_rescale_normal              := CheckExtension('GL_EXT_rescale_normal');
GL_EXT_secondary_color             := CheckExtension('GL_EXT_secondary_color');
GL_EXT_separate_specular_color     := CheckExtension('GL_EXT_separate_specular_color');
GL_EXT_shadow_funcs                := CheckExtension('GL_EXT_shadow_funcs');
GL_EXT_shared_texture_palette      := CheckExtension('GL_EXT_shared_texture_palette');
GL_EXT_stencil_two_side            := CheckExtension('GL_EXT_stencil_two_side');
GL_EXT_stencil_wrap                := CheckExtension('GL_EXT_stencil_wrap');
GL_EXT_subtexture                  := CheckExtension('GL_EXT_subtexture');
GL_EXT_texture                     := CheckExtension('GL_EXT_texture');
GL_EXT_texture3D                   := CheckExtension('GL_EXT_texture3D');
GL_EXT_texture_compression_s3tc    := CheckExtension('GL_EXT_texture_compression_s3tc');
GL_EXT_texture_cube_map            := CheckExtension('GL_EXT_texture_cube_map');
GL_EXT_texture_env_add             := CheckExtension('GL_EXT_texture_env_add');
GL_EXT_texture_env_combine         := CheckExtension('GL_EXT_texture_env_combine');
GL_EXT_texture_env_dot3            := CheckExtension('GL_EXT_texture_env_dot3');
GL_EXT_texture_filter_anisotropic  := CheckExtension('GL_EXT_texture_filter_anisotropic');
GL_EXT_texture_lod_bias            := CheckExtension('GL_EXT_texture_lod_bias');
GL_EXT_texture_object              := CheckExtension('GL_EXT_texture_object');
GL_EXT_texture_perturb_normal      := CheckExtension('GL_EXT_texture_perturb_normal');
GL_EXT_vertex_array                := CheckExtension('GL_EXT_vertex_array');
GL_EXT_vertex_shader               := CheckExtension('GL_EXT_vertex_shader');
GL_EXT_vertex_weighting            := CheckExtension('GL_EXT_vertex_weighting');
GL_FfdMaskSGIX                     := CheckExtension('GL_FfdMaskSGIX');
// === HP ======================================================================
GL_HP_convolution_border_modes     := CheckExtension('GL_HP_convolution_border_modes');
GL_HP_image_transform              := CheckExtension('GL_HP_image_transform');
GL_HP_occlusion_test               := CheckExtension('GL_HP_occlusion_test');
GL_HP_texture_lighting             := CheckExtension('GL_HP_texture_lighting');
// === IBM =====================================================================
GL_IBM_cull_vertex                 := CheckExtension('GL_IBM_cull_vertex');
GL_IBM_multimode_draw_arrays       := CheckExtension('GL_IBM_multimode_draw_arrays');
GL_IBM_rasterpos_clip              := CheckExtension('GL_IBM_rasterpos_clip');
GL_IBM_texture_mirrored_repeat     := CheckExtension('GL_IBM_texture_mirrored_repeat');
GL_IBM_vertex_array_lists          := CheckExtension('GL_IBM_vertex_array_lists');
// === INGR ====================================================================
GL_INGR_blend_func_separate        := CheckExtension('GL_INGR_blend_func_separate');
GL_INGR_color_clamp                := CheckExtension('GL_INGR_color_clamp');
GL_INGR_interlace_read             := CheckExtension('GL_INGR_interlace_read');
GL_INGR_palette_buffer             := CheckExtension('GL_INGR_palette_buffer');
// === INTEL ===================================================================
GL_INTEL_parallel_arrays           := CheckExtension('GL_INTEL_parallel_arrays');
GL_INTEL_texture_scissor           := CheckExtension('GL_INTEL_texture_scissor');
// === MESA ====================================================================
GL_MESA_resize_buffers             := CheckExtension('GL_MESA_resize_buffers');
GL_MESA_window_pos                 := CheckExtension('GL_MESA_window_pos');
// === NVIDIA ==================================================================
GL_NV_blend_square                 := CheckExtension('GL_NV_blend_square');
GL_NV_copy_depth_to_color          := CheckExtension('GL_NV_copy_depth_to_color');
GL_NV_depth_clamp                  := CheckExtension('GL_NV_depth_clamp');
GL_NV_evaluators                   := CheckExtension('GL_NV_evaluators');
GL_NV_fence                        := CheckExtension('GL_NV_fence');
GL_NV_float_buffer                 := CheckExtension('GL_NV_float_buffer');
GL_NV_fog_distance                 := CheckExtension('GL_NV_fog_distance');
GL_NV_fragment_program             := CheckExtension('GL_NV_fragment_program');
GL_NV_half_float                   := CheckExtension('GL_NV_half_float');
GL_NV_light_max_exponent           := CheckExtension('GL_NV_light_max_exponent');
GL_NV_multisample_filter_hint      := CheckExtension('GL_NV_multisample_filter_hint');
GL_NV_occlusion_query              := CheckExtension('GL_NV_occlusion_query');
GL_NV_packed_depth_stencil         := CheckExtension('GL_NV_packed_depth_stencil');
GL_NV_pixel_data_range             := CheckExtension('GL_NV_pixel_data_range');
GL_NV_point_sprite                 := CheckExtension('GL_NV_point_sprite');
GL_NV_primitive_restart            := CheckExtension('GL_NV_primitive_restart');
GL_NV_register_combiners           := CheckExtension('GL_NV_register_combiners');
GL_NV_register_combiners2          := CheckExtension('GL_NV_register_combiners2');
GL_NV_texgen_emboss                := CheckExtension('GL_NV_texgen_emboss');
GL_NV_texgen_reflection            := CheckExtension('GL_NV_texgen_reflection');
GL_NV_texture_compression_vtc      := CheckExtension('GL_NV_texture_compression_vtc');
GL_NV_texture_env_combine4         := CheckExtension('GL_NV_texture_env_combine4');
GL_NV_texture_expand_normal        := CheckExtension('GL_NV_texture_expand_normal');
GL_NV_texture_rectangle            := CheckExtension('GL_NV_texture_rectangle');
GL_NV_texture_shader               := CheckExtension('GL_NV_texture_shader');
GL_NV_texture_shader2              := CheckExtension('GL_NV_texture_shader2');
GL_NV_texture_shader3              := CheckExtension('GL_NV_texture_shader3');
GL_NV_vertex_array_range           := CheckExtension('GL_NV_vertex_array_range');
GL_NV_vertex_array_range2          := CheckExtension('GL_NV_vertex_array_range2');
GL_NV_vertex_program               := CheckExtension('GL_NV_vertex_program');
GL_NV_vertex_program1_1            := CheckExtension('GL_NV_vertex_program1_1');
GL_NV_vertex_program2              := CheckExtension('GL_NV_vertex_program2');
// === OML =====================================================================
GL_OML_interlace                   := CheckExtension('GL_OML_interlace');
GL_OML_resample                    := CheckExtension('GL_OML_resample');
GL_OML_subsample                   := CheckExtension('GL_OML_subsample');
// === PGI =====================================================================
GL_PGI_misc_hints                  := CheckExtension('GL_PGI_misc_hints');
GL_PGI_vertex_hints                := CheckExtension('GL_PGI_vertex_hints');
// === REND ====================================================================
GL_REND_screen_coordinates         := CheckExtension('GL_REND_screen_coordinates');
// === S3 ======================================================================
GL_S3_s3tc                         := CheckExtension('GL_S3_s3tc');
// === SGIS ====================================================================
GL_SGIS_detail_texture             := CheckExtension('GL_SGIS_detail_texture');
GL_SGIS_fog_function               := CheckExtension('GL_SGIS_fog_function');
GL_SGIS_generate_mipmap            := CheckExtension('GL_SGIS_generate_mipmap');
GL_SGIS_multisample                := CheckExtension('GL_SGIS_multisample');
GL_SGIS_pixel_texture              := CheckExtension('GL_SGIS_pixel_texture');
GL_SGIS_point_line_texgen          := CheckExtension('GL_SGIS_point_line_texgen');
GL_SGIS_point_parameters           := CheckExtension('GL_SGIS_point_parameters');
GL_SGIS_sharpen_texture            := CheckExtension('GL_SGIS_sharpen_texture');
GL_SGIS_texture4D                  := CheckExtension('GL_SGIS_texture4D');
GL_SGIS_texture_border_clamp       := CheckExtension('GL_SGIS_texture_border_clamp');
GL_SGIS_texture_color_mask         := CheckExtension('GL_SGIS_texture_color_mask');
GL_SGIS_texture_edge_clamp         := CheckExtension('GL_SGIS_texture_edge_clamp');
GL_SGIS_texture_filter4            := CheckExtension('GL_SGIS_texture_filter4');
GL_SGIS_texture_lod                := CheckExtension('GL_SGIS_texture_lod');
GL_SGIS_texture_select             := CheckExtension('GL_SGIS_texture_select');
// === SGIX ====================================================================
GL_SGIX_async                      := CheckExtension('GL_SGIX_async');
GL_SGIX_async_histogram            := CheckExtension('GL_SGIX_async_histogram');
GL_SGIX_async_pixel                := CheckExtension('GL_SGIX_async_pixel');
GL_SGIX_blend_alpha_minmax         := CheckExtension('GL_SGIX_blend_alpha_minmax');
GL_SGIX_calligraphic_fragment      := CheckExtension('GL_SGIX_calligraphic_fragment');
GL_SGIX_clipmap                    := CheckExtension('GL_SGIX_clipmap');
GL_SGIX_convolution_accuracy       := CheckExtension('GL_SGIX_convolution_accuracy');
GL_SGIX_depth_pass_instrument      := CheckExtension('GL_SGIX_depth_pass_instrument');
GL_SGIX_depth_texture              := CheckExtension('GL_SGIX_depth_texture');
GL_SGIX_flush_raster               := CheckExtension('GL_SGIX_flush_raster');
GL_SGIX_fog_offset                 := CheckExtension('GL_SGIX_fog_offset');
GL_SGIX_fog_scale                  := CheckExtension('GL_SGIX_fog_scale');
GL_SGIX_fragment_lighting          := CheckExtension('GL_SGIX_fragment_lighting');
GL_SGIX_framezoom                  := CheckExtension('GL_SGIX_framezoom');
GL_SGIX_igloo_interface            := CheckExtension('GL_SGIX_igloo_interface');
GL_SGIX_impact_pixel_texture       := CheckExtension('GL_SGIX_impact_pixel_texture');
GL_SGIX_instruments                := CheckExtension('GL_SGIX_instruments');
GL_SGIX_interlace                  := CheckExtension('GL_SGIX_interlace');
GL_SGIX_ir_instrument1             := CheckExtension('GL_SGIX_ir_instrument1');
GL_SGIX_list_priority              := CheckExtension('GL_SGIX_list_priority');
GL_SGIX_pixel_texture              := CheckExtension('GL_SGIX_pixel_texture');
GL_SGIX_pixel_tiles                := CheckExtension('GL_SGIX_pixel_tiles');
GL_SGIX_polynomial_ffd             := CheckExtension('GL_SGIX_polynomial_ffd');
GL_SGIX_reference_plane            := CheckExtension('GL_SGIX_reference_plane');
GL_SGIX_resample                   := CheckExtension('GL_SGIX_resample');
GL_SGIX_scalebias_hint             := CheckExtension('GL_SGIX_scalebias_hint');
GL_SGIX_shadow                     := CheckExtension('GL_SGIX_shadow');
GL_SGIX_shadow_ambient             := CheckExtension('GL_SGIX_shadow_ambient');
GL_SGIX_sprite                     := CheckExtension('GL_SGIX_sprite');
GL_SGIX_subsample                  := CheckExtension('GL_SGIX_subsample');
GL_SGIX_tag_sample_buffer          := CheckExtension('GL_SGIX_tag_sample_buffer');
GL_SGIX_texture_add_env            := CheckExtension('GL_SGIX_texture_add_env');
GL_SGIX_texture_coordinate_clamp   := CheckExtension('GL_SGIX_texture_coordinate_clamp');
GL_SGIX_texture_lod_bias           := CheckExtension('GL_SGIX_texture_lod_bias');
GL_SGIX_texture_multi_buffer       := CheckExtension('GL_SGIX_texture_multi_buffer');
GL_SGIX_texture_scale_bias         := CheckExtension('GL_SGIX_texture_scale_bias');
GL_SGIX_texture_select             := CheckExtension('GL_SGIX_texture_select');
GL_SGIX_vertex_preclip             := CheckExtension('GL_SGIX_vertex_preclip');
GL_SGIX_ycrcb                      := CheckExtension('GL_SGIX_ycrcb');
GL_SGIX_ycrcb_subsample            := CheckExtension('GL_SGIX_ycrcb_subsample');
GL_SGIX_ycrcba                     := CheckExtension('GL_SGIX_ycrcba');
// === SGI =====================================================================
GL_SGI_color_matrix                := CheckExtension('GL_SGI_color_matrix');
GL_SGI_color_table                 := CheckExtension('GL_SGI_color_table');
GL_SGI_depth_pass_instrument       := CheckExtension('GL_SGI_depth_pass_instrument');
GL_SGI_texture_color_table         := CheckExtension('GL_SGI_texture_color_table');
// === SUN =====================================================================
GL_SUNX_constant_data              := CheckExtension('GL_SUNX_constant_data');
GL_SUN_convolution_border_modes    := CheckExtension('GL_SUN_convolution_border_modes');
GL_SUN_global_alpha                := CheckExtension('GL_SUN_global_alpha');
GL_SUN_mesh_array                  := CheckExtension('GL_SUN_mesh_array');
GL_SUN_slice_accum                 := CheckExtension('GL_SUN_slice_accum');
GL_SUN_triangle_list               := CheckExtension('GL_SUN_triangle_list');
GL_SUN_vertex                      := CheckExtension('GL_SUN_vertex');
// === WIN =====================================================================
GL_WIN_phong_shading               := CheckExtension('GL_WIN_phong_shading');
GL_WIN_specular_fog                := CheckExtension('GL_WIN_specular_fog');
// === WGL =====================================================================
WGL_3DFX_multisample               := CheckExtension('WGL_3DFX_multisample');
WGL_ARB_buffer_region              := CheckExtension('WGL_ARB_buffer_region');
WGL_ARB_extensions_string          := CheckExtension('WGL_ARB_extensions_string');
WGL_ARB_make_current_read          := CheckExtension('WGL_ARB_make_current_read');
WGL_ARB_multisample                := CheckExtension('WGL_ARB_multisample');
WGL_ARB_pbuffer                    := CheckExtension('WGL_ARB_pbuffer');
WGL_ARB_pixel_format               := CheckExtension('WGL_ARB_pixel_format');
WGL_ARB_render_texture             := CheckExtension('WGL_ARB_render_texture');
WGL_ATI_pixel_format_float         := CheckExtension('WGL_ATI_pixel_format_float');
WGL_EXT_depth_float                := CheckExtension('WGL_EXT_depth_float');
WGL_EXT_display_color_table        := CheckExtension('WGL_EXT_display_color_table');
WGL_EXT_extensions_string          := CheckExtension('WGL_EXT_extensions_string');
WGL_EXT_make_current_read          := CheckExtension('WGL_EXT_make_current_read');
WGL_EXT_multisample                := CheckExtension('WGL_EXT_multisample');
WGL_EXT_pbuffer                    := CheckExtension('WGL_EXT_pbuffer');
WGL_EXT_pixel_format               := CheckExtension('WGL_EXT_pixel_format');
WGL_EXT_swap_control               := CheckExtension('WGL_EXT_swap_control');
WGL_I3D_digital_video_control      := CheckExtension('WGL_I3D_digital_video_control');
WGL_I3D_gamma                      := CheckExtension('WGL_I3D_gamma');
WGL_I3D_genlock                    := CheckExtension('WGL_I3D_genlock');
WGL_I3D_image_buffer               := CheckExtension('WGL_I3D_image_buffer');
WGL_I3D_swap_frame_lock            := CheckExtension('WGL_I3D_swap_frame_lock');
WGL_I3D_swap_frame_usage           := CheckExtension('WGL_I3D_swap_frame_usage');
WGL_NV_float_buffer                := CheckExtension('WGL_NV_float_buffer');
WGL_NV_render_depth_texture        := CheckExtension('WGL_NV_render_depth_texture');
WGL_NV_render_texture_rectangle    := CheckExtension('WGL_NV_render_texture_rectangle');
WGL_NV_vertex_array_range          := CheckExtension('WGL_NV_vertex_array_range');
WGL_OML_sync_control               := CheckExtension('WGL_OML_sync_control');
WIN_draw_range_elements            := CheckExtension('WIN_draw_range_elements');
WIN_swap_hint                      := CheckExtension('WIN_swap_hint');
ImplementationRead                 := True;
end;

// =============================================================================
// RaiseLastOSError
// =============================================================================
// Needed for compatibility with older Delphiversions
// =============================================================================
procedure RaiseLastOSError;
begin
SysUtils.RaiseLastOSError;
// Use RaiseLastWin32Error when your Delphi version doesn't know the above
// In D7,RaiseLastWin32Error is deprecated by RaiseLastOSError
end;

// =============================================================================
// CreateRenderingContext
// =============================================================================
function CreateRenderingContext(DC : HDC;Options : TRCOptions;ColorBits,ZBits,StencilBits,AccumBits,AuxBuffers : Integer;Layer : Integer) : HGLRC;
const
 MemoryDCs = [OBJ_MEMDC, OBJ_METADC, OBJ_ENHMETADC];
var
 PFDescriptor : TPixelFormatDescriptor;
 PixelFormat  : Integer;
 AType        : DWORD;
begin
FillChar(PFDescriptor, SizeOf(PFDescriptor), 0);
with PFDescriptor do
 begin
 nSize    := SizeOf(PFDescriptor);
 nVersion := 1;
 dwFlags  := PFD_SUPPORT_OPENGL;
 AType    := GetObjectType(DC);
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
 iPixelType   := PFD_TYPE_RGBA;
 cColorBits   := ColorBits;
 cDepthBits   := zBits;
 cStencilBits := StencilBits;
 cAccumBits   := AccumBits;
 cAuxBuffers  := AuxBuffers;
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
Result := wglCreateLayerContext(DC, Layer);
if Result = 0 then
 RaiseLastOSError
else
 LastPixelFormat := 0;
end;

// =============================================================================
// ActivateRenderingContext
// =============================================================================
procedure ActivateRenderingContext(DC : HDC;RC : HGLRC);
begin
Assert((DC <> 0), 'DC must not be 0');
Assert((RC <> 0), 'RC must not be 0');
wglMakeCurrent(DC, RC);
 ReadExtensions;
 ReadImplementationProperties;
end;

// =============================================================================
// DeactivateRenderingContext
// =============================================================================
procedure DeactivateRenderingContext;
begin
wglMakeCurrent(0,0);
end;

// =============================================================================
// DestroyRenderingContext
// =============================================================================
procedure DestroyRenderingContext(RC: HGLRC);
begin
wglDeleteContext(RC);
end;

initialization
 Set8087CW($133F);

finalization

end.
