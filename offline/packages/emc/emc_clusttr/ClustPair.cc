
#include "ClustPair.hh"

ClassImp(ClustPair)

//=====================================================================
ClustPair::ClustPair(){

};
//=====================================================================
void ClustPair::Reset(){
  // Global Information
  glb0.Reset();
  glb1.Reset();
  clt_num0 = 0;
  clt_num1 = 0;

  // Pair Information
  m = 0;
  e = 0;
  pt = 0;
  px = 0;
  py = 0;
  pz = 0;
  mv0 = 0;
  m0 = 0;
  cosine = 0;
  asym = 0;

  // 2 gamma Information
  clt0.Reset();
  clt1.Reset();
  clt_id0 = -1;
  clt_id1 = -1;

  // Tracking Information
  trk_num0 = 0;
  trk_num1 = 0;
  int i = 5;
  while( i-- ){
    dist0[i] = 5000;
    dist1[i] = 5000;
    int xyz = 3;
    while( xyz-- ){
      dist0xyz[i][xyz] = 5000;
      dist1xyz[i][xyz] = 5000;
    }
    trk_id0[i] = -1;
    trk_id1[i] = -1;
  }
  trk0.Reset();
  trk1.Reset();

};
//=====================================================================
//

