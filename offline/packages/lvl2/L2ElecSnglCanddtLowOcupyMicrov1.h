///////////////////////////////////////////////////////////
//							 //
//   Lvl2 Electron Single Candidate in RUN3 
//							 //
//      Org:  Wei Xie (e-mail: xiewei@rcf2.rhic.bnl.gov) //
//							 //
///////////////////////////////////////////////////////////

#ifndef __L2ElecSnglCanddtLowOcupyMicrov1_H__
#define __L2ElecSnglCanddtLowOcupyMicrov1_H__

#include <PHObject.h>

class L2ElecSnglCanddtLowOcupyMicrov1 : public TObject 
{
  protected:

       float    charge;
       float    theta0;
       float    phi0;
       float    ptot;

       float    xrich;
       float    yrich;
       float    zrich;
       int      pmtCent;   //.. pmt ID of center of the ring 
       int      npmt;
       float    npe;
       float    TrkRingDist;  //.. distance between track and associated ring

       float    xemc;
       float    yemc;
       float    zemc;
       float    energy;      //.. associated EMCal cluster energy

       float    xpc1;
       float    ypc1;
       float    zpc1;

  public:

    L2ElecSnglCanddtLowOcupyMicrov1();
    virtual ~L2ElecSnglCanddtLowOcupyMicrov1(){}

    void    set_charge(float input)  {charge = input;}
    void    set_theta0(float input)  {theta0 = input;}
    void    set_phi0(float input)  {phi0 = input;}
    void    set_ptot(float input)  {ptot = input;}
    void    set_xrich(float input)  {xrich = input;}
    void    set_yrich(float input)  {yrich = input;}
    void    set_zrich(float input)  {zrich = input;}
    void    set_pmtCent(int input)  {pmtCent = input;}
    void    set_npmt(int input)  {npmt = input;}
    void    set_npe(float input)  {npe = input;}
    void    set_TrkRingDist(float input)  {TrkRingDist = input;}
    void    set_xemc(float input)  {xemc = input;}
    void    set_yemc(float input)  {yemc = input;}
    void    set_zemc(float input)  {zemc = input;}
    void    set_energy(float input)  {energy = input;}
    void    set_xpc1(float input)  {xpc1 = input;}
    void    set_ypc1(float input)  {ypc1 = input;}
    void    set_zpc1(float input)  {zpc1 = input;}

    float    get_charge()  {return charge;} 
    float    get_theta0()  {return theta0 ;}
    float    get_phi0()    {return phi0 ;}
    float    get_ptot()    {return ptot ;}
    float    get_xrich()   {return xrich ;}
    float    get_yrich()   {return yrich ;}
    float    get_zrich()   {return zrich ;}
    int      get_pmtCent() {return pmtCent ;}
    int      get_npmt()    {return npmt ;}
    float    get_npe()     {return npe ;}
    float    get_TrkRingDist()  {return TrkRingDist ;}
    float    get_xemc()    {return xemc ;}
    float    get_yemc()    {return yemc ;}
    float    get_zemc()    {return zemc ;}
    float    get_energy()  {return energy ;}
    float    get_xpc1()    {return xpc1 ;}
    float    get_ypc1()    {return ypc1 ;}
    float    get_zpc1()    {return zpc1 ;}


    ClassDef(L2ElecSnglCanddtLowOcupyMicrov1, 1)
};

#endif	// __L2ElecSnglCanddtLowOcupyMicrov1_H__
