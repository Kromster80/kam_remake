{ Uncomment next define in order to turn off version checking of OpenSSL       }
{ library, not recommended.                                                    }
{.$DEFINE NO_OSSL_VERSION_CHECK}

{ Uncomment next define in order to support OpenSSL v0.9.7g thru v0.9.8e       }
{ using our own 'hacks' to emulate some C-macros not exported by OpenSSL       }
{ in previous versions. Leave it commented if you want to support OpenSSL      }
{ v0.9.8e and better without using our 'hacks' but newly exported functions,   }
{ default. Ignored in 64-bit and if OPENSSL_NO_TLSEXT is not defined.          }
{.$DEFINE BEFORE_OSSL_098E}

{ Comment next define in order to include Server Name Indication (SNI).        }
{ If commented undefines "BEFORE_OSSL_098E" and sets minimum OSSL version      }
{ to v0.9.8F. Tested with OSSL v0.9.8k and v0.9.8L, both are built including   }
{ the TLS extensions by default.                                               }
{$DEFINE OPENSSL_NO_TLSEXT}

{ Uncommented next define will create files containing debug information if    }
{ the OpenSSL libraries failed to load.                                        }
{ File names: FailedIcsSSLEAY.txt and FailedIcsLIBEAY.txt                      }
{.$DEFINE LOADSSL_ERROR_FILE}

{ Uncomment next define if OpenSSL shall use the Delphi memory manager.        }
{ This is very useful to find memory leaks in our SSL code with a memory       }
{ manager capable to report memory leaks such as FastMM with option            }
{ ReportMemoryLeaksOnShutDown enabled. Note that two leaks were always         }
{ reported on shutdown (13 - 20 bytes: Unknown x 2), those are no leaks in ICS }
{ but obviously in the OpenSSL libraries.                                      }
{.$DEFINE OPENSSL_USE_DELPHI_MM}

{ Uncomment next define to use a few custom resource strings instead of the    }
{ OpenSSL strings. Currently only useful if localized certificate properties   }
{ shall be displayed.                                                          }
{.$DEFINE OPENSSL_USE_RESOURCE_STRINGS}

{ Comment next define to include experimental Engine code.                     }
{ http://openssl.org/docs/crypto/engine.html                                   }
{ The default should be "OPENSSL_NO_ENGINE" it's currently very experimental   }
{$DEFINE OPENSSL_NO_ENGINE}

{ Do not modify below this line                                                }
{ -----------------------------------------------------------------------------}
{$IFDEF CPUX64}
  {$IFDEF BEFORE_OSSL_098E}
    {$UNDEFINE BEFORE_OSSL_098E}
  {$ENDIF}
{$ENDIF}
{$IFNDEF OPENSSL_NO_TLSEXT}
    {$IFDEF BEFORE_OSSL_098E}
        {$UNDEFINE BEFORE_OSSL_098E}
    {$ENDIF}
{$ENDIF}
{$IFDEF BEFORE_OSSL_098E}
  {$IFNDEF OPENSSL_NO_ENGINE}
    {$DEFINE OPENSSL_NO_ENGINE}  // ENGINE code tested since V0.98E
  {$ENDIF}
{$ENDIF}
