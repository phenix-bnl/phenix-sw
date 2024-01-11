//-----------------------------------------------------------------------------
//  Tof.hh
//
//  History: 04/10/00  H. Ohnishi   First Version
//           04/27/00  A. Kiyomichi Add more constant parameters
//           05/14/00  T. Chujo     Add parameters
//           07/08/00  T. Chujo     Add parameters
//           08/16/00  A. Kiyomichi Delete PHTimeStamp.h
//           06/09/01  H. Masui     Add parameters
//-----------------------------------------------------------------------------
#ifndef PHENIX_TOF_HH
#define PHENIX_TOF_HH
/**
 Just subsituate class for namespace.
 <p>
 This class is just subsituate class for namespace,
 which is too new to use the current compilers.
 So it should be replaced by namespace eventually

 @author Hiroaki Ohnishi [BNL], Akio Kiyomichi [Univ. of Tsukuba]
 @       Tatsuya Chujo [Univ. of Tsukuba]
 @version April, 10, 2000
**/

const int TOF_NPANEL_ALL = 10;
const int TOF_NPANEL     = TOF_NPANEL_ALL;

const int TOF_NSLAT_ALL   = 960;
const int TOF_NSLAT       = TOF_NSLAT_ALL;
const int TOF_NSLAT_PANEL = 96;

const int TOF_NPMTS_ALL = 1920;
const int TOF_NPMTS     = TOF_NPMTS_ALL;

const int TOF_NCRATE    = 8;            // Number of FEM crates
const int TOF_NBOARD    = 16;           // Number of FEM boards per crate
const int TOF_NCHANNEL  = 16;           // Number of channels per FEM board

const float TOF_SSLAT_LENGTH = 43.39;   // Short Scintillator Length [cm]
const float TOF_LSLAT_LENGTH = 63.77;   // Long Scintillator Length [cm]
const float TOF_SLAT_WIDTH = 1.52654;   // Scintillator Width [cm]
const float TOF_SLAT_ATTEN = 128.0;     // Scintillator Attenuation Length [cm]

const int TOF_HV_NCRATE    = 4;         // Number of HV crates
const int TOF_HV_NBOARD    = 10;        // Number of HV boards per crate
const int TOF_HV_NCHANNEL  = 25;        // Number of channels per HV board

#endif /* PHENIX_TOF_HH */
