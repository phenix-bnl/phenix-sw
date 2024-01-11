///////////////////////////////////////////////////////////
//							 //
//   Lvl2 Electron Single Pair Candidate in RUN3 
//							 //
//      Org:  Wei Xie (e-mail: xiewei@rcf2.rhic.bnl.gov) //
//							 //
///////////////////////////////////////////////////////////

#ifndef __L2ElecSnglInvMassLowOcupyMicroV1_H__
#define __L2ElecSnglInvMassLowOcupyMicroV1_H__

#include <PHObject.h>

class L2ElecSnglInvMassLowOcupyMicroV1 : public TObject 
{
  protected:

       bool     KilledByChargeID;  //.. same sign pair
       float    Mass;
       float    Pt;
       int      candID0;  //sequence number of the 1st candidate
       int      candID1;  //sequence number of the 2nd candidate

  public:

    L2ElecSnglInvMassLowOcupyMicroV1();
    virtual ~L2ElecSnglInvMassLowOcupyMicroV1(){}

    void    set_KilledByChargeID(bool input)  { KilledByChargeID = input;}
    void    set_Mass(float input) { Mass = input;}
    void    set_Pt(float input) { Pt = input;}
    void    set_candID0(int input) {candID0  = input;}
    void    set_candID1(int input) { candID1 = input;}

    bool    get_KilledByChargeID() {return KilledByChargeID;}
    float   get_Mass() {return Mass;}
    float   get_Pt() {return Pt;}
    int     get_candID0() {return candID0;}
    int     get_candID1() {return candID1;}


    ClassDef(L2ElecSnglInvMassLowOcupyMicroV1, 1)
};

#endif	// __L2ElecSnglInvMassLowOcupyMicroV1_H__
