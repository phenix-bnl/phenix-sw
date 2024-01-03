#ifndef __RUN12WMAP_H__
#define __RUN12WMAP_H__

#include <TROOT.h>
#include <TH2F.h>
//#include "EmcAnaCommon.h"
/** Hot/Dead/Warn map for Run16     **/
class recoConsts;

namespace my_emc_common {
  const int N_ARM       = 2;             /// copied for Kensuke's (?) EmcAnaCommon class
  const int N_SECTOR    = 4;
  const int N_ARMSECT   = 8;
  const int N_YPOS_PBGL = 48;
  const int N_YPOS_PBSC = 36;
  const int N_ZPOS_PBGL = 96;
  const int N_ZPOS_PBSC = 72;
  const int N_TOWER     = 24768;
};

using namespace std;
using namespace my_emc_common;


class Run16WMap {
     Int_t mywmap[N_ARMSECT][N_YPOS_PBGL][N_ZPOS_PBGL]; /// map read from file
     Int_t wrnmap[N_ARMSECT][N_YPOS_PBGL][N_ZPOS_PBGL]; /// generated map with EMC flags
     TH2F *htwrs[8];   /// histograms for drawing 
     void ReadMap(const Char_t* fname);
     void GenerateWarnmap(); /// generate wrnmap from mywmap
     int debug;
     
  public:
     Run16WMap(const Char_t* fname);
     virtual ~Run16WMap();
     void ResetMap();
 
     unsigned int GetFlag(Int_t as, Int_t y, Int_t z);  /// return warnmap value
     unsigned int IsFlagged(Int_t as, Int_t y, Int_t z);  /// return 0|1 from mywmap
     Int_t IsDead(Int_t as, Int_t y, Int_t z);  /// return direct mywmap value 
     Double_t GetGoodFraction(Int_t as, Int_t bl_include_edge = true);
     void DrawWarnmap(int which = 1, int print=0);    /// draws warnmap which [0|1] = [mywmap|wrnmap], print [0|1] = create image with map [no|yes]
     
     Bool_t IsPbGl(Int_t armsect);  /// copied for EmcAnaCommon class
     Bool_t IsValidYZ(Int_t as, Int_t y, Int_t z);
     Bool_t IsEdgePos(Int_t as, Int_t y, Int_t z);
     Bool_t IsEdgePos(Int_t);
     void TowerID2AsYZ(Int_t towerid, Int_t& as, Int_t& y, Int_t& z);
     Int_t AsYZ2TowerID(Int_t as, Int_t y, Int_t z);
     
 protected:
     ClassDef(Run16WMap, 1)
};

#endif // __RUN12WMAP_H__
