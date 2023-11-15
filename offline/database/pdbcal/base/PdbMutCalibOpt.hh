//-----------------------------------------------------------------------------
//  Declaration of class PdbMutCalibOpt
//
//  Purpose: Store MuTr calibration information 
//
//  Description: add serval parameters to describe the shape of the gain response
//               to the highest amplitudes to PdbMutCalib
//
//  Author: DongJo Kim(djkim@bnl.gov)  July 18/2001
//-----------------------------------------------------------------------------
#ifndef __PDBMUTCALIBOPT_HH__
#define __PDBMUTCALIBOPT_HH__

#include "PdbCalChan.hh"

// Max # of strips(at each cathode plane per HalfOctant) is 187
static const int MUT_STRIPS_HALF_MAX = 187;    

class 
PdbMutCalibOpt : public PdbCalChan 
{

public:
  PdbMutCalibOpt();
  ~PdbMutCalibOpt();

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
  float get_calpar0    (int index) const {return  calpar0[index];}
  float get_calpar1    (int index) const {return  calpar1[index];}
  float get_calpar2    (int index) const {return  calpar2[index];}
  float get_calpar3    (int index) const {return  calpar3[index];}
  float get_calpar4    (int index) const {return  calpar4[index];}
  float get_calpar5    (int index) const {return  calpar5[index];}
  float get_calpar6    (int index) const {return  calpar6[index];}

  void setArm        (int temp){ ArmNum=temp;}
  void setStation    (int temp){ StationNum=temp;}
  void setOctant     (int temp){ OctantNum=temp;}
  void setHalfOctant (int temp){ HalfOctantNum=temp;}
  void setGap        (int temp){ GapNum=temp;}
  void setPlane      (int temp){ PlaneNum=temp;}
  void set_pedestal(int index,float temp){pedestal[index]=temp;}
  void set_rms     (int index,float temp){     rms[index]=temp;}
  void set_gain    (int index,float temp){     gain[index]=temp;}
  void set_calpar0 (int index,float temp){  calpar0[index]=temp;}
  void set_calpar1 (int index,float temp){  calpar1[index]=temp;}
  void set_calpar2 (int index,float temp){  calpar2[index]=temp;}
  void set_calpar3 (int index,float temp){  calpar3[index]=temp;}
  void set_calpar4 (int index,float temp){  calpar4[index]=temp;}
  void set_calpar5 (int index,float temp){  calpar5[index]=temp;}
  void set_calpar6 (int index,float temp){  calpar6[index]=temp;}

private:
  float pedestal[MUT_STRIPS_HALF_MAX];
  float rms[MUT_STRIPS_HALF_MAX];
  float gain[MUT_STRIPS_HALF_MAX];
  float calpar0[MUT_STRIPS_HALF_MAX];
  float calpar1[MUT_STRIPS_HALF_MAX];
  float calpar2[MUT_STRIPS_HALF_MAX];
  float calpar3[MUT_STRIPS_HALF_MAX];
  float calpar4[MUT_STRIPS_HALF_MAX];
  float calpar5[MUT_STRIPS_HALF_MAX];
  float calpar6[MUT_STRIPS_HALF_MAX];
  int ArmNum; 
  int StationNum;
  int OctantNum;
  int HalfOctantNum;
  int GapNum;
  int PlaneNum;

  ClassDef(PdbMutCalibOpt,1);
};

#endif /* __PDBMUTCALIBOPT_HH__ */
