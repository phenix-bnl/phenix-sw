#ifndef __UNCALIBTOWERLIST_H__
#define __UNCALIBTOWERLIST_H__

#include "EmcAnaCommon.h"

#include <cstdio>
#include <fstream>
#include <map>

class TObject;
class TFile;
class TFile;

class Pi0MassFitter;

class UncalibTowerList 
{
 public:
   enum Reason {
     ZERO_STAT = 1, FEW_STAT = 2, NOISY = 3, NO_PEAK = 4, NO_EYE = 5, WARNED = 6
   };

 private:
   typedef std::map<Int_t, Int_t> UncalibListMap_t;
   UncalibListMap_t m_towerid_uncalib;

 public:
   UncalibTowerList();
   virtual ~UncalibTowerList();

   void ReadUncalibList(const Char_t* fn_uncalib);
   void WriteUncalibList(const Char_t* fn_uncalib);
   void AddUncalibList(Int_t towerid, Int_t reason);
   void AddUncalibList(Int_t as, Int_t y, Int_t z, Int_t reason);
   Int_t IsInUncalibList(Int_t towerid);
   Int_t IsInUncalibList(Int_t as, Int_t y, Int_t z);

   void Draw(const Char_t* fname);
   void Print(const Char_t* ofname, const Char_t* fn_warnmap = "");

 protected:
   void ClearUncalibList();
   
};
      
#endif // __UNCALIBTOWERLIST_H__
