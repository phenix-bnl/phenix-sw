/**
This code removes the towers which have been tagged as bad. It not only removes the center tower, it also removes a masked area around it. The list is in the DeadWarnRun16.txt 
**/


#include <cstdio>
#include <iostream>
#include <fstream>
#include <cstdlib>
#include "Run16WMap.h"
#include "recoConsts.h"

ClassImp(Run16WMap)

static unsigned int Cut3x3Map = 0x1ce70;

Run16WMap::Run16WMap(const Char_t* fname) {
  recoConsts *rc =  recoConsts::instance();  
  debug = rc->get_IntFlag("EMCNEW_DEBUG", 0);
  
  if(debug>0) cout <<"Run16WMap constructor"<<endl; 
  for (int as=0; as<8; as++) {
    htwrs[as] = NULL;
  }
  ResetMap();
  if(debug>0) cout <<"     map reset, reading from "<<fname<<endl; 
  ReadMap(fname);
  if(debug>0) cout <<"     map read in"<<endl; 
  GenerateWarnmap();
  if(debug>0) cout <<"     map generated"<<endl; 
}

Run16WMap::~Run16WMap(){
  for (int as=0; as<8; as++) {
    if(htwrs[as]) delete htwrs[as];
  }
}

void Run16WMap::ResetMap(){
   memset(mywmap, 0, sizeof(mywmap));
   memset(wrnmap, 0, sizeof(wrnmap));
}

void Run16WMap::ReadMap(const Char_t* fname) {
     const int verb = 1;
     ifstream if_status(fname);
     if (if_status.is_open()) {
        if(verb) cout << "Run16WMap::ReadMap():" << endl 
                      << "  Open '" << fname << "'." << endl;
     } else {
        if(verb) cout << "Run16WMap::ReadMap():" << endl
                      << "  File '" << fname << "' does not exist.  Abort." << endl;
        exit(0);
     }

     Int_t armsect, ypos, zpos, status;
     while (if_status >> armsect >> ypos >> zpos >> status) {
	  if (IsValidYZ(armsect, ypos, zpos)) {
	       mywmap[armsect][ypos][zpos] = status;
	  }
     }
     if_status.close();
     return;
}

int Run16WMap::IsDead(int as, int y, int z) {
/// check for overflows 
  Int_t y_max, z_max;
  if (IsPbGl(as)) {
      y_max = N_YPOS_PBGL;
      z_max = N_ZPOS_PBGL;
  } else {
      y_max = N_YPOS_PBSC;
      z_max = N_ZPOS_PBSC;
  }
  if( y < 0 || y > y_max ) return 98;
  if( z < 0 || z > z_max ) return 99;
/// return raw value from the map
  return mywmap[as][y][z];
}

unsigned int Run16WMap::IsFlagged(int as, int y, int z) {
/// check for overflows 
  Int_t y_max, z_max;
  if (IsPbGl(as)) {
     y_max = N_YPOS_PBGL;
     z_max = N_ZPOS_PBGL;
  } else {
     y_max = N_YPOS_PBSC;
     z_max = N_ZPOS_PBSC;
  }
  if( y < 0 || y > y_max ) return 98;
  if( z < 0 || z > z_max ) return 99;
/// return 1 or 0
  return mywmap[as][y][z]>0 ? 1 : 0;
}

unsigned int Run16WMap::GetFlag(int as, int y, int z) {
   
 if(IsEdgePos(as,y,z)) {        /// edge tower
    unsigned int edgemask = 1<<9;
    return edgemask;
    }
//https://www.phenix.bnl.gov/viewcvs/offline/packages/emc/emcClusterContent.h?rev=2.20&content-type=text/vnd.viewcvs-markup
      // For energy, the bits are:
      // ---------------------
      // |   | 18| 19| 20|   |
      // ---------------------
      // | 13| 14| 15| 16| 17|
      // ---------------------  ^ y
      // | 8 | 9 | 10| 11| 12|  |
      // ---------------------  |
      // | 3 | 4 | 5 | 6 | 7 |  |
      // ---------------------  ------> z(x)
      // |   | 0 | 1 | 2 |   |
  unsigned int deadmap = 0;
  
  // deadmap |= IsFlagged(as, y - 2, z - 1) << 0;
  //deadmap |= IsFlagged(as, y - 2, z    ) << 1;
  //deadmap |= IsFlagged(as, y - 2, z + 1) << 2;
  //deadmap |= IsFlagged(as, y - 1, z - 2) << 3;
  deadmap |= IsFlagged(as, y - 1, z - 1) << 4;
  deadmap |= IsFlagged(as, y - 1, z    ) << 5;
  deadmap |= IsFlagged(as, y - 1, z + 1) << 6;
  //deadmap |= IsFlagged(as, y - 1, z + 2) << 7;
  
  // deadmap |= IsFlagged(as, y    , z - 2) << 8;
  deadmap |= IsFlagged(as, y    , z - 1) << 9;
  deadmap |= IsFlagged(as, y    , z    ) <<10;
  deadmap |= IsFlagged(as, y    , z + 1) <<11;
  //deadmap |= IsFlagged(as, y    , z + 2) <<12;
  
  //deadmap |= IsFlagged(as, y + 1, z - 2) <<13;
  deadmap |= IsFlagged(as, y + 1, z - 1) <<14;
  deadmap |= IsFlagged(as, y + 1, z    ) <<15;
  deadmap |= IsFlagged(as, y + 1, z + 1) <<16;
  //deadmap |= IsFlagged(as, y + 1, z + 2) <<17;
  //deadmap |= IsFlagged(as, y + 2, z - 1) <<18;
  //deadmap |= IsFlagged(as, y + 2, z    ) <<19;
  //deadmap |= IsFlagged(as, y + 2, z + 1) <<20;
  
  return deadmap;
}

void Run16WMap::GenerateWarnmap () {
  for(int as=0; as<8; as++) { /// sector loop
    Int_t y_max, z_max;
    if (IsPbGl(as)) {       /// set sector boudaries
      y_max = N_YPOS_PBGL;
      z_max = N_ZPOS_PBGL;
    } else {
      y_max = N_YPOS_PBSC;
      z_max = N_ZPOS_PBSC;
    }
    for (int y=0; y<y_max; y++) {  /// y loop
      for (int z=0; z<z_max; z++) {     /// z loop
        wrnmap[as][y][z] = GetFlag(as, y, z);
      }                   /// end y loop
    }                     /// end z loop
  }                       /// end sector loop
}

void Run16WMap::DrawWarnmap(int which, int print) {
    
}

void Run16WMap::TowerID2AsYZ(Int_t towerid, Int_t& as, Int_t& y, Int_t& z)
{
   if (towerid >= 15552) { // PbGl
      as = 4 + (towerid -15552) / 4608; // 4 or 5
      y = (towerid - 15552 - 4608 * (as - 4)) / 96;
      z = (towerid - 15552 - 4608 * (as - 4)) % 96;
   } else { // PbSc
      as = towerid / 2592; // 0 to 5 (at end, change 4->6 and 5->7)
      y = (towerid - as * 2592) / 72;
      z = (towerid - as * 2592) % 72;
      if (as >= 4) as += 2;
   }
}

Int_t Run16WMap::AsYZ2TowerID(Int_t as, Int_t y, Int_t z)
{
   Int_t towerid = -1;
   if (0 <= as && as <= 3)      { towerid =         2592*as     + 72*y + z; }
   else if (as == 4 || as == 5) { towerid = 15552 + 4608*(as-4) + 96*y + z; }
   else if (as == 6 || as == 7) { towerid = 10368 + 2592*(as-6) + 72*y + z; }
   return towerid;
}

Bool_t Run16WMap::IsPbGl(Int_t armsect) {
   return ((armsect == 4 || armsect == 5) ? true : false);
}

Bool_t Run16WMap::IsValidYZ(Int_t as, Int_t y, Int_t z) {
   Int_t ret = false;
   if (IsPbGl(as)) {
      if (y >= 0 && y < N_YPOS_PBGL && z >= 0 && z < N_ZPOS_PBGL) ret = true;
   } else {
      if (y >=0 && y < N_YPOS_PBSC && z >=0 && z < N_ZPOS_PBSC)   ret = true;
   }
   return ret;
}

Bool_t Run16WMap::IsEdgePos(Int_t as, Int_t y, Int_t z) {
   Int_t y_max, z_max;
   if (IsPbGl(as)) {
      y_max = N_YPOS_PBGL - 1;
      z_max = N_ZPOS_PBGL - 1;
   } else {
      y_max = N_YPOS_PBSC - 1;
      z_max = N_ZPOS_PBSC - 1;
   }

   if (y == 0 || z == 0 || y == y_max || z == z_max) return true;
   if (y == 1 || z == 1 || y == (y_max-1) || z == (z_max-1)) return true;
   else return false;
}

Bool_t Run16WMap::IsEdgePos(Int_t towerid)
{
   int as, y, z;
   TowerID2AsYZ(towerid, as, y, z);
   return IsEdgePos(as, y, z);
}

Double_t Run16WMap::GetGoodFraction(Int_t as, Int_t bl_include_edge) {
   Int_t ntwr_tot = 0;
   Int_t ntwr_good = 0;
   for (Int_t y = 0; y < N_YPOS_PBGL; y++) {
   for (Int_t z = 0; z < N_ZPOS_PBGL; z++) {
   if (IsValidYZ(as, y, z)) {
      if (! bl_include_edge && IsEdgePos(as, y, z)) continue;
      ntwr_tot++;
//      if (! ( IsDead(as, y, z) & Cut3x3Map)) ntwr_good++;
      if (! ( GetFlag(as, y, z) & Cut3x3Map)) ntwr_good++;
   }
   }
   }
   return (Double_t)ntwr_good/ntwr_tot;
}
