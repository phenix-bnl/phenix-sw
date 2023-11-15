//-----------------------------------------------------------------------------
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 2001
//
//  Declaration of class PdbMutCalib
//
//  Purpose: Store MuTr calibration information 
//
//  Description:
//
//  Author: DongJo Kim(djkim@bnl.gov)
//-----------------------------------------------------------------------------
#ifndef __PDBMUTCALIB_HH__
#define __PDBMUTCALIB_HH__

#include "PdbCalChan.hh"
#define MUT_STRIPS_HALF_MAX  187    
// Max # of strips(at each cathode plane per HalfOctant) is 187

class PdbMutCalib : public PdbCalChan {

public:
  PdbMutCalib();
//  PdbMutCalib (const PdbMutCalib& rhs);
  ~PdbMutCalib();

  virtual void print() const;

  int getArm    ()     const {return ArmNum;} 
  int getStation()     const {return StationNum;}
  int getOctant ()     const {return OctantNum;}
  int getHalfOctant () const {return HalfOctantNum;}
  int getGap ()        const {return GapNum;}
  int getPlane  ()     const {return PlaneNum;}
  float get_pedestal(int index) const {return pedestal[index];}
  float get_rms     (int index) const {return      rms[index];}
  float get_gain    (int index) const {return     gain[index];}

  void setArm        (int temp){ ArmNum=temp;}
  void setStation    (int temp){ StationNum=temp;}
  void setOctant     (int temp){ OctantNum=temp;}
  void setHalfOctant (int temp){ HalfOctantNum=temp;}
  void setGap        (int temp){ GapNum=temp;}
  void setPlane      (int temp){ PlaneNum=temp;}
  void set_pedestal(int index,float temp){pedestal[index]=temp;}
  void set_rms     (int index,float temp){     rms[index]=temp;}
  void set_gain    (int index,float temp){     gain[index]=temp;}


private:
  float pedestal[MUT_STRIPS_HALF_MAX];
  float rms[MUT_STRIPS_HALF_MAX];
  float gain[MUT_STRIPS_HALF_MAX];
  int ArmNum; 
  int StationNum;
  int OctantNum;
  int HalfOctantNum;
  int GapNum;
  int PlaneNum;

  ClassDef(PdbMutCalib,1);
};

#endif /* __PDBMUTCALIB_HH__ */
