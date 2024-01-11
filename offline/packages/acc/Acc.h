
#ifndef __ACC_H__
#define __ACC_H__

namespace ACC {

const int ACC_PACKET_ID = 17001; // packet id for ACC
const int ACC_PACKET_ID_N = 17001; // packet id for ACC
const int ACC_PACKET_ID_S = 17005; // packet id for ACC

const int ACC_BANKID_GEOM_OLD  = 17100;
const int ACC_BANKID_CALIB_OLD = 17200;
const int ACC_BANKID_GEOM  = 17101;
const int ACC_BANKID_CALIB = 17201;

const int ACC_NBOX =  160; // number of ACC boxes
const int ACC_NCH  = 320;  // number of ACC channels 
const int ACC_NBOX_HALF =  80; // number of ACC boxes
const int ACC_NCH_HALF  = 160; // number of ACC channels 

const int ACC_NROW =  10;  // number of ACC rows 
const int ACC_NCLM =   16; // number of ACC columns (<- for Run5 configuration)
const int ACC_NCLM_HALF =   8; // number of ACC columns (<- for Run4 configuration)

const float ACC_X_MIDDLE = 449.414;  // X [cm], Projection plane 

// North Panel (Run4)
const float ACC_Y_TOP    =  70.8674; // Y [cm], Top edge of Aerogel volume
const float ACC_Y_BTM    = -48.6326; // Y [cm], Bottom edge of Aerogel volume
const float ACC_Z_NORTH  = 185.629;  // Z [cm], North edge of Aerogel volume
const float ACC_Z_SOUTH  =   0.8294; // Z [cm], South edge of Aerogel volume (<- for Run4 configuration)

// North Panel (Run5-)
const float ACC_Y_TOP_N   =  70.8674; // Y [cm], Top edge of Aerogel volume 
const float ACC_Y_BTM_N   = -48.6326; // Y [cm], Bottom edge of Aerogel volume 
const float ACC_Z_NORTH_N = 185.629;  // Z [cm], North edge of Aerogel volume 
const float ACC_Z_SOUTH_N =   0.8294; // Z [cm], South edge of Aerogel volume 

// South Panel (Run5-)
const float ACC_Y_TOP_S   =   69.6953; // Y [cm], Top edge of Aerogel volume 
const float ACC_Y_BTM_S   =  -49.8047; // Y [cm], Bottom edge of Aerogel volume 
const float ACC_Z_NORTH_S =   -1.6366; // Z [cm], North edge of Aerogel volume 
const float ACC_Z_SOUTH_S = -186.4366;  // Z [cm], South edge of Aerogel volume 

const float ACC_Z_MIDDLE     =  -0.4036; // Z [cm], Middle Point
const float ACC_Z_HALFSECTOR = 184.8;    // Z [cm], Sector Half Size

const float ACC_Y_CELL = 11.95; // Y [cm], Cell Size
const float ACC_Z_CELL = 23.10; // Z [cm], Cell Size

const float ACC_Y_HALFCELL =  5.975; // Y [cm], Cell Half Size
const float ACC_Z_HALFCELL = 11.55;  // Z [cm], Cell Half Size


const char* Name();

//------------------------------------------------------------------------
// getBoxID(), getPmtID()
// Convertor from Box id (PMT id) to PMT id (Box id).
//  Note :
//    Counter id is started from "1" in the definition.
//    But we use "0" as start number in this program.
//    If you want to compare the number between here and 
//    the following URL, you should +1 to the number used 
//    in the code.
//
// Details
// http://www.phenix.bnl.gov/WWW/upgrades/highpt/Tech_info/index.html
//   --> Maps 
//     --> PreAmp
//     --> PMT Assignment (config)
//------------------------------------------------------------------------
int getBoxID(const int ipmt);
int getPmtID(int ns, const int ibox);
int convertBoxID(const int ibox);
}

#endif

