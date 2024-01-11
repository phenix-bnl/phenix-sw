
//INCLUDECHECKER: Removed this line: #include "Acc.h"
#include "AccProj.h"

#include <iostream>

using std::cout;
using std::endl;

// BoxID from projection point 
int AccProj::getBoxIDfromXYZ(const float pc2x, const float pc2y, const float pc2z, 
			     const float pc3x, const float pc3y, const float pc3z, const int boxconfig) const
{

  // Factors of linear interpolation between PC2 and PC3.
  float cy1 = (pc3y - pc2y)/(pc3x - pc2x);
  float cy2 = (pc3x*pc2y - pc2x*pc3y)/(pc3x - pc2x);
  float cz1 = (pc3z - pc2z)/(pc3x - pc2x);
  float cz2 = (pc3x*pc2z - pc2x*pc3z)/(pc3x - pc2x);

  // Projection point on middle plane in X-direction.
  float ay = cy1 * ACC::ACC_X_MIDDLE + cy2;
  float az = cz1 * ACC::ACC_X_MIDDLE + cz2;

  // Edge of Aerogel Volume
  float rACC_Y_TOP,rACC_Y_BTM,rACC_Z_NORTH,rACC_Z_SOUTH;

  if(az > ACC::ACC_Z_MIDDLE) // for North Panel
    {
      rACC_Y_TOP   = ACC::ACC_Y_TOP_N;
      rACC_Y_BTM   = ACC::ACC_Y_BTM_N;
      rACC_Z_NORTH = ACC::ACC_Z_NORTH_N;
      rACC_Z_SOUTH = ACC::ACC_Z_SOUTH_N - ACC::ACC_Z_HALFSECTOR;
    } 
  else // for South Panel
    {
      rACC_Y_TOP   = ACC::ACC_Y_TOP_S;
      rACC_Y_BTM   = ACC::ACC_Y_BTM_S;
      rACC_Z_NORTH = ACC::ACC_Z_NORTH_S + ACC::ACC_Z_HALFSECTOR;
      rACC_Z_SOUTH = ACC::ACC_Z_SOUTH_S;
    }

  // Reference BoxID
  int box_y = (int) ((ay - (rACC_Y_BTM   - ACC::ACC_Y_HALFCELL)) / ACC::ACC_Y_CELL);
  int box_z = (int) ((az - (rACC_Z_SOUTH - ACC::ACC_Z_HALFCELL)) / ACC::ACC_Z_CELL);
  int ref_boxid = (ACC::ACC_NROW - box_y) + (ACC::ACC_NCLM - box_z) * ACC::ACC_NROW;
  int boxid;

  // BoxID (_0, _1, _2, _3) from Reference-BoxID 
  if(ay < (rACC_Y_BTM - ACC::ACC_Y_HALFCELL) || (rACC_Y_TOP + ACC::ACC_Y_HALFCELL) < ay 
     || az < (rACC_Z_SOUTH - ACC::ACC_Z_HALFCELL) || (rACC_Z_NORTH + ACC::ACC_Z_HALFCELL) < az){ // OverRange
    boxid = -999;
  }
  else if((rACC_Y_TOP - ACC::ACC_Y_HALFCELL) < ay && ay < (rACC_Y_TOP + ACC::ACC_Y_HALFCELL)){ // Upper region
    switch(boxconfig){
    case 0: 
      boxid = -999;
      break;
    case 1: 
      boxid = ref_boxid - 10;
      break;
    case 2: 
      boxid = -999;
      break;
    case 3: 
      boxid = ref_boxid;
      break;
    default:
      cout << "Boxconfig (Aerogel) should be 0,1,2,3"<<endl;
      boxid = -999;
      break;
    }
  }
  else if((rACC_Y_BTM - ACC::ACC_Y_HALFCELL) < ay && ay < (rACC_Y_BTM + ACC::ACC_Y_HALFCELL)){ // Lower region
    switch(boxconfig){
    case 0: 
      boxid = ref_boxid - 11;
      break;
    case 1: 
      boxid = -999;
      break;
    case 2: 
      boxid = ref_boxid -1;
      break;
    case 3: 
      boxid = -999;
      break;
    default:
      std::cout << "Boxconfig (Aerogel) should be 0,1,2,3"<<std::endl;
      boxid = -999;
      break;
    }
  }
  else{ // Intermediate region 
    switch(boxconfig){ 
    case 0: 
      boxid = ref_boxid - 11;
      break;
    case 1: 
      boxid = ref_boxid - 10;
      break;
    case 2: 
      boxid = ref_boxid -1;
      break;
    case 3: 
      boxid = ref_boxid;
      break;
    default:
      std::cout << "Boxconfig (Aerogel) should be 0,1,2,3"<<std::endl;
      boxid = -999;
      break;
    }
  }

  return boxid;
}

// BoxID (as HitID) from projection point 
int AccProj::getHitIDfromXYZ(const float pc2x, const float pc2y, const float pc2z, 
			     const float pc3x, const float pc3y, const float pc3z) const
{

  // Factors of linear interpolation between PC2 and PC3.
  float cy1 = (pc3y - pc2y)/(pc3x - pc2x);
  float cy2 = (pc3x*pc2y - pc2x*pc3y)/(pc3x - pc2x);
  float cz1 = (pc3z - pc2z)/(pc3x - pc2x);
  float cz2 = (pc3x*pc2z - pc2x*pc3z)/(pc3x - pc2x);

  // Projection point on middle plane in X-direction.
  float ay = cy1 * ACC::ACC_X_MIDDLE + cy2;
  float az = cz1 * ACC::ACC_X_MIDDLE + cz2;

  // Edge of Aerogel Volume
  float rACC_Y_TOP,rACC_Y_BTM,rACC_Z_NORTH,rACC_Z_SOUTH;

  if(az > ACC::ACC_Z_MIDDLE) // for North Panel
    {
      rACC_Y_TOP   = ACC::ACC_Y_TOP_N;
      rACC_Y_BTM   = ACC::ACC_Y_BTM_N;
      rACC_Z_NORTH = ACC::ACC_Z_NORTH_N;
      rACC_Z_SOUTH = ACC::ACC_Z_SOUTH_N - ACC::ACC_Z_HALFSECTOR;
    } 
  else // for South Panel
    {
      rACC_Y_TOP   = ACC::ACC_Y_TOP_S;
      rACC_Y_BTM   = ACC::ACC_Y_BTM_S;
      rACC_Z_NORTH = ACC::ACC_Z_NORTH_S + ACC::ACC_Z_HALFSECTOR;
      rACC_Z_SOUTH = ACC::ACC_Z_SOUTH_S;
    }
  
  // HitID
  int hitid;

    if(ay < rACC_Y_BTM || ay > rACC_Y_TOP || az < rACC_Z_SOUTH || az > rACC_Z_NORTH){ // OverRange
      hitid = -999;
    }
    else{ // Aerogel Acceptance
      int hit_y = (int) ((ay - rACC_Y_BTM) / ACC::ACC_Y_CELL);
      int hit_z = (int) ((az - rACC_Z_SOUTH ) / ACC::ACC_Z_CELL);
      hitid = (ACC::ACC_NROW - 1 - hit_y) + (ACC::ACC_NCLM - 1 - hit_z) * ACC::ACC_NROW;
    }
  
  return hitid;
}

// HitConfig from boxid and hitid 
int AccProj::getHitConfig(const int boxid_0, const int boxid_1, const int boxid_2, const int boxid_3, const int hitid) const
{

  int hitconfig=-999;

  if(hitid==-999) hitconfig = -999;
  else if(hitid==boxid_0) hitconfig = 0;
  else if(hitid==boxid_1) hitconfig = 1;
  else if(hitid==boxid_2) hitconfig = 2;
  else if(hitid==boxid_3) hitconfig = 3;
  else hitconfig = -999;

  return hitconfig;
}
