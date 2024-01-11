#ifndef __PI0CHECKCOMMON_H__
#define __PI0CHECKCOMMON_H__

#include <TROOT.h>
#include <TVector3.h>
#include <cmath>

namespace EmcAnaCommon {
   const int N_ARM       = 2;
   const int N_SECTOR    = 4;
   const int N_ARMSECT   = 8;
   const int N_YPOS_PBGL = 48;
   const int N_YPOS_PBSC = 36;
   const int N_ZPOS_PBGL = 96;
   const int N_ZPOS_PBSC = 72;
   const int N_TOWER     = 24768;
   const int N_SUPERMOD = 32;

   const int MASK_UNCALIB        = 0x40;
   const int MASK_AROUND_UNCALIB = 0x20;
   const int MASK_HOT            = 0x10;
   const int MASK_DEAD           = 0x08;
   const int MASK_AROUND_HOT     = 0x04;
   const int MASK_AROUND_DEAD    = 0x02;
   const int MASK_EDGE           = 0x01;
   
   const int N_ECORE_RANGE_FOR_NHIT_HIST = 48;
   extern const double ECORE_RANGE_FOR_NHIT_HIST[N_ECORE_RANGE_FOR_NHIT_HIST+1];

   extern const char* SECTOR_NAME[N_ARMSECT];

   void  TowerID2AsYZ(Int_t towerid, Int_t& as, Int_t& y, Int_t& z);
   Int_t AsYZ2TowerID(Int_t as, Int_t y, Int_t z);
   Int_t IsPbGl(Int_t armsect);
   Int_t IsValidYZ(Int_t as, Int_t y, Int_t z);
   Int_t IsEdgePos(Int_t as, Int_t y, Int_t z);
   Int_t GetSuperModule(Int_t as, Int_t y, Int_t z);
   Int_t IsValidSM(Int_t as, Int_t sm);
   Double_t CalcNewCoef(Double_t coef, Double_t mean);
   Double_t CalcNewCoefErr(Double_t coef, Double_t mean, Double_t mean_err);
   Double_t CalculateT4D(Double_t x,  Double_t y, Double_t z, Double_t z0);

//   void FindHitPos(TVector3* p,
//                   Int_t& arm, Int_t& sect, Int_t& ypos, Int_t& zpos);

//   char* GetEnvSure(const char* name);
};

inline void EmcAnaCommon::TowerID2AsYZ(Int_t towerid, Int_t& as, Int_t& y, Int_t& z)
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

inline Int_t EmcAnaCommon::AsYZ2TowerID(Int_t as, Int_t y, Int_t z)
{
   Int_t towerid = -1;
   if (0 <= as && as <= 3)      { towerid =         2592*as     + 72*y + z; }
   else if (as == 4 || as == 5) { towerid = 15552 + 4608*(as-4) + 96*y + z; }
   else if (as == 6 || as == 7) { towerid = 10368 + 2592*(as-6) + 72*y + z; }
   return towerid;
}

inline Int_t EmcAnaCommon::IsPbGl(Int_t armsect)
{
   return ((armsect == 4 || armsect == 5) ? 1 : 0);
}

inline Int_t EmcAnaCommon::IsValidYZ(Int_t as, Int_t y, Int_t z)
{
   Int_t ret = 0;
   if (IsPbGl(as)) {
      if (y >= 0 && y < N_YPOS_PBGL && z >= 0 && z < N_ZPOS_PBGL) ret = 1;
   } else {
      if (y >=0 && y < N_YPOS_PBSC && z >=0 && z < N_ZPOS_PBSC)   ret = 1;
   }
   return ret;
}

inline Int_t EmcAnaCommon::IsEdgePos(Int_t as, Int_t y, Int_t z)
{
   Int_t y_max, z_max;
   if (IsPbGl(as)) {
      y_max = N_YPOS_PBGL - 1;
      z_max = N_ZPOS_PBGL - 1;
   } else {
      y_max = N_YPOS_PBSC - 1;
      z_max = N_ZPOS_PBSC - 1;
   }

   if (y == 0 || z == 0 || y == y_max || z == z_max) return true;
   else return false;
}

inline Int_t EmcAnaCommon::GetSuperModule(Int_t as, Int_t y, Int_t z)
{
  Int_t supermodule;
  
  if (IsPbGl(as)) {
    supermodule = y/12*8 + z/12;
    return supermodule;
  }
  else{
    supermodule = y/12*6 + z/12;
    return supermodule;
  }

}

inline Int_t EmcAnaCommon::IsValidSM(Int_t as, Int_t sm){
  if (IsPbGl(as)&&sm<32){
    return true;
  } else if(sm<18){
    return true;
  } else {return false;}
}


inline Double_t EmcAnaCommon::CalcNewCoef(Double_t coef, Double_t mean)
{
   return coef * 0.1349766 / mean;
}

inline Double_t EmcAnaCommon::CalcNewCoefErr(Double_t coef, Double_t mean, Double_t mean_err)
{
   return coef * ( mean_err / mean );
}


inline Double_t EmcAnaCommon::CalculateT4D(Double_t x,  Double_t y, Double_t z, Double_t z0){
  Double_t c = 29.9792458; //in cm/nanosec
  Double_t t4d = sqrt(pow(x,2)+pow(y,2)+pow(z-z0,2))/c;
  return t4d;
}

#endif // __PI0CHECKCOMMON_H__
