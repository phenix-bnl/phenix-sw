//-----------------------------------------------------------------------------
//  Declaration of class PdbMutLvdCa
//
//  Purpose: Store MuTr Low voltage information(status) 
//
//  Author: DongJo Kim(djkim@bnl.gov)  July 18/2001
//-----------------------------------------------------------------------------
#ifndef __PDBMUTLVDCA_HH__
#define __PDBMUTLVDCA_HH__

#include "PdbCalChan.hh"
#include "PHString.h"
static const int ARMS_MAX = 2;
static const int CALISTSIZE_MAX = 315; // Max

// **SOUTH**
// 10 FEMs per Station 1 quadrant ==> 40 FEMs
//  8 FEMs per Station 2 octant   ==> 64 FEMs
// 10 FEMs per Station 3 octant   ==> 80 FEMs
// 				  ==> total of 184 FEMs
// 50 Glink/Clink LV lines
// 56 Camera LV lines
// 1 20mA interface LV line
// TOTAL of 291 LV lines in SOUTH
// 
// /home/phnxmutr/muon_anc/ca/mutrlvdCaList.h    on phoncs0
// 
// **NORTH**
// 10 FEMs per Station 1 quadrant ==> 40 FEMs
//  9 FEMs per Station 2 octant   ==> 72 FEMs
// 10 FEMs per Station 3 octant   ==> 80 FEMs
// 				  ==> total of 192 FEMs
// 66 Glink/Clink LV lines
// 56 Camera LV lines
// 1 20mA interface LV line
// TOTAL of 315 LV lines in NORTH

class 
PdbMutLvdCa : public PdbCalChan 
{

public:
  PdbMutLvdCa();
  ~PdbMutLvdCa();

  virtual void print() const;

  int   getArm          ()          const {return ArmNum;}
  int   getIndex        ()          const {return Index;}
  char* get_CaList     (int index)     {return CaList[index];}
  int   get_CaValue    (int index)     const {return CaValue[index];}

  void  setArm           (int temp){ ArmNum=temp;}
  void  setIndex       (int temp)   {Index=temp;}
  void  set_CaList     (int index,const PHString & calist) {strcpy(CaList[index],calist.getString());}
  void set_CaValue    (int index,int temp) {CaValue[index]=temp;}

private:
  char  CaList[CALISTSIZE_MAX][25];
  int   CaValue[CALISTSIZE_MAX];
  int   Index;
  int   ArmNum;

  ClassDef(PdbMutLvdCa,1);
};

#endif /* __PDBMUTLVDCA_HH__ */
