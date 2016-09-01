{ Uncomment next define in order to turn off version checking of OpenSSL       }
{ library, not recommended.                                                    }
{.$DEFINE NO_OSSL_VERSION_CHECK}

{ Comment next define in order to include Server Name Indication (SNI).        }
{ If commented undefines "BEFORE_OSSL_098E" and sets minimum OSSL version      }
{ to v0.9.8F. Tested with OSSL v0.9.8k and v0.9.8L, both are built including   }
{ the TLS extensions by default.                                               }
{ Angus - March 2015, no longer supporting 098 so enable by default            }
{.$DEFINE OPENSSL_NO_TLSEXT}

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
{.$DEFINE OPENSSL_NO_ENGINE}

{ Uncomment next define if OpenSSL needs to support the oboslete SSLv2 protocol}
{ OpenSSL 1.0.2g and 1.0.1s and later no longer export SSlv2 methods and will  }
{ not load (unless special DLLs are built).                                     }
{.$DEFINE OPENSSL_ALLOW_SSLV2}

{ Do not modify below this line                                                }
{ -----------------------------------------------------------------------------}
{ Angus - March 2015, no longer supporting 098 so nothing left                 }
{ Angus - June 2015, enable engine support, which are cryptographic            }
{                    modules adding extra algorithms                           }
{ Angus - March 2016, added define OPENSSL_ALLOW_SSLV2                               }


