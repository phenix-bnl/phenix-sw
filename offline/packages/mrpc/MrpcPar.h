#ifndef __MRPCPAR_H__
#define __MRPCPAR_H__

// ===========================================
//  General parameters for the MRPC detector.
//                   T.Chujo (01/05/2005)
//============================================
// FEM
//________________________________________________________________________
static const int MRPC_PACKET_ID = 7101; // packet id for TOF.W MRPC (Run5)
static const int MRPC_HITF_NON_ZEROSUP = 1007;
static const int MRPC_HITF_ZEROSUP = 1107;
static const int MRPC_NCRATE = 1;
static const int MRPC_NBOARD = 12;
static const int MRPC_NCH = 16;
static const int MRPC_NCH_TOTAL =  MRPC_NCRATE * MRPC_NBOARD * MRPC_NCH;

// Readout Configuration
//________________________________________________________________________
// 
// BOX    : gas volume
// PANNEL : MRPC detector unit
// SLAT   : Readout unit
// TYPE (SLAT) : slat/pad readout 
// TYPE (READ) : top/bottom/pad readout  

static const int MRPC_NBOX         = 2; // # of gas boxs
static const int MRPC_NPANEL_BOX   = 4; // # of MRPC per box
static const int MRPC_NPANEL_TOTAL = MRPC_NBOX * MRPC_NPANEL_BOX; //8 panels_total for Run5
static const int MRPC_NSLAT_PH2 = 8;  // # of slat for PH2
static const int MRPC_NSLAT_PH3 = 24; // # of slat for PH3
static const int MRPC_NSLAT_PH4 = 6;  // # of slat for PH4
static const int MRPC_NSLAT = 88;
static const int MRPC_NTYPE_SLAT = 2; // 2 types of slat (slat,pad) 
static const int MRPC_NTYPE_READ = 3; // 3 types of readout (top,bottom,pad)
static const int MRPC_NTYPE_AD   = 2; // 2 types of timing (analog/digital)

// DB parameters
//________________________________________________________________________
static const int MRPC_ADDRESS_NPAR = 6;

// Geometry
//________________________________________________________________________
static const float MRPC_PH2_PANEL_LENGTH = 57.5;  // [cm]
static const float MRPC_PH3_PANEL_LENGTH = 57.5;  // [cm]  top to bottom
static const float MRPC_PH4_PANEL_LENGTH = 57.5;  // [cm]
static const float MRPC_PH2_PANEL_WIDTH  = 14.9;  // [cm]
static const float MRPC_PH3_PANEL_WIDTH  = 16.3;  // [cm]  side to side
static const float MRPC_PH4_PANEL_WIDTH  = 14.9;  // [cm]

static const float MRPC_PH2_SLAT_LENGTH = 53.5;   // [cm] (active area, total 55.1 cm)
static const float MRPC_PH3_SLAT_LENGTH =  2.0;   // [cm]  top to bottom
static const float MRPC_PH4_SLAT_LENGTH = 53.5;   // [cm] (active area, total 55.1 cm)
static const float MRPC_PH2_SLAT_WIDTH  =  1.3;   // [cm]
static const float MRPC_PH3_SLAT_WIDTH  =  6.2;   // [cm]  side to side
static const float MRPC_PH4_SLAT_WIDTH  =  2.0;   // [cm] 

static const float MRPC_PH2_SLAT_GAP = 0.3;       // [cm]
static const float MRPC_PH3_SLAT_GAP = 0.3;       // [cm]  both Z and Y direction 
static const float MRPC_PH4_SLAT_GAP = 0.3;       // [cm]

static const float MRPC_PH2_SIDE_MARGIN = 1.2;  // [cm]
static const float MRPC_PH3_SIDE_MARGIN = 1.8;  // [cm]  
static const float MRPC_PH4_SIDE_MARGIN = 0.7;  // [cm]
static const float MRPC_PH2_TOP_MARGIN  = 2.0;  // [cm]
static const float MRPC_PH3_TOP_MARGIN  = 1.3;  // [cm]  
static const float MRPC_PH4_TOP_MARGIN  = 2.0;  // [cm]

static const float MRPC_RDIST_SLATPANEL = 0.0;    // [cm]

#endif
