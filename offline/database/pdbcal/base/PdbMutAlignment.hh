//-----------------------------------------------------------------------------
//  Declaration of class PdbMutAlignment
//
//  Purpose: Store MuTr Alignment information 
//
//  Author: DongJo Kim(djkim@bnl.gov)  July 18/2001
//-----------------------------------------------------------------------------
#ifndef __PDBMUTALIGNMENT_HH__
#define __PDBMUTALIGNMENT_HH__

#include "PdbCalChan.hh"
#include "PHString.h"

#define ARMS_MAX  2

// Each arm has 56 cameras 
static const int MUT_CAMERA_MAX = 56;

class 
PdbMutAlignment : public PdbCalChan 
{

public:
  PdbMutAlignment();
//  PdbMutAlignment (const PdbMutAlignment& rhs);
  ~PdbMutAlignment();

  virtual void print() const;

  int   getArm          ()          const {return ArmNum;}
  int   getCamera       ()          const {return         CameraNum;} 

  float get_Xmean        (int index) const {return  Xmean[index];}
  float get_Xpeak        (int index) const {return  Xpeak[index];}
  float get_Xwidth       (int index) const {return  Xwidth[index];}
  float get_Xoffset      (int index) const {return  Xoffset[index];}
  float get_Ymean        (int index) const {return  Ymean[index];}
  float get_Ypeak        (int index) const {return  Ypeak[index];}
  float get_Ywidth       (int index) const {return  Ywidth[index];}
  float get_Yoffset      (int index) const {return  Yoffset[index];}
  char* get_Time         (int index)  {return  Time[index];}

  void  setArm           (int temp){ ArmNum=temp;}
  void  setCamera        (int temp){ CameraNum=temp;}
  void  set_Xmean        (int index,float temp) { Xmean[index]=temp;}
  void  set_Xpeak        (int index,float temp) { Xpeak[index]=temp;}
  void  set_Xwidth       (int index,float temp) { Xwidth[index]=temp;}
  void  set_Xoffset      (int index,float temp) { Xoffset[index]=temp;}
  void  set_Ymean        (int index,float temp) { Ymean[index]=temp;}
  void  set_Ypeak        (int index,float temp) { Ypeak[index]=temp;}
  void  set_Ywidth       (int index,float temp) { Ywidth[index]=temp;}
  void  set_Yoffset      (int index,float temp) { Yoffset[index]=temp;}
  void  set_Time         (int index,const PHString & time) {strcpy(Time[index],time.getString());}


private:
  float Xmean[MUT_CAMERA_MAX];
  float Xpeak[MUT_CAMERA_MAX];
  float Xwidth[MUT_CAMERA_MAX];
  float Xoffset[MUT_CAMERA_MAX];
  float Ymean[MUT_CAMERA_MAX];
  float Ypeak[MUT_CAMERA_MAX];
  float Ywidth[MUT_CAMERA_MAX];
  float Yoffset[MUT_CAMERA_MAX];
  char  Time[MUT_CAMERA_MAX][20];
  int CameraNum; 
  int ArmNum;

  ClassDef(PdbMutAlignment,1);
};

#endif /* __PDBMUTALIGNMENT_HH__ */
