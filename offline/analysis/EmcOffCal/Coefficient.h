#ifndef __COEFFICIENT_H__
#define __COEFFICIENT_H__

#include "EmcAnaCommon.h"

class UncalibTowerList;

class Coefficient{
   Double_t* m_coef; //!
   Double_t** m_coef_supermod;
   Double_t* m_coef_err;
   Double_t** m_coef_supermod_err;

   UncalibTowerList* m_uncalib_list;

 public:
   Coefficient();
   virtual ~Coefficient();
  
   void WriteCoef(const Char_t* coef_table, const Char_t* coef_table_supermod);
   void ReadCoef(const Char_t* coef_table, const Char_t* coef_table_supermod);
   Double_t GetCoef(const Int_t towerid);
   Double_t GetCoef(const Int_t as, const Int_t y, const Int_t z);
   Double_t GetCoefSm(const Int_t as, const Int_t sm);
   Double_t GetCoefErr(const Int_t towerid);
   Double_t GetCoefErr(const Int_t as, const Int_t y, const Int_t z);
   Double_t GetCoefSmErr(const Int_t as, const Int_t sm);
   void ResetCoef();
   void SetCoef(const Int_t towerid, const Double_t coef, const Double_t coef_err);
   void SetCoefSm(const Int_t as, const Int_t sm, const Double_t coef, const Double_t coef_err);

 public:
   void Draw(const Char_t* fname_1d, const Char_t* fname_2d, const Char_t* uncalib_list, const Char_t* froot_name = "");
   void DrawDiff(const Char_t* fname_coef_table_base, const Char_t* ofname);
   
};

#endif
