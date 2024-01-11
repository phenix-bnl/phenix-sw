///////////////////////////////////////////////////////////
//							 //
//   Lvl2 Electron Single Candidate
//							 //
//      Org:  Wei Xie (e-mail: xiewei@rcf2.rhic.bnl.gov) //
//							 //
///////////////////////////////////////////////////////////

#ifndef __L2ElectronSnglCandidateMicrov1_H__
#define __L2ElectronSnglCandidateMicrov1_H__

#include <PHObject.h>

class L2ElectronSnglCandidateMicrov1 : public TObject 
{
  protected:

    Bool_t  KilledByPC3;
    float   chargeLvl2;
    float   theta0Lvl2;
    float   phi0Lvl2;
    float   ptot0Lvl2;
    float   ntowerLvl2;
    float   armLvl2;
    float   sectorLvl2;
    float   ecentLvl2;
    float   iyLvl2;
    float   izLvl2;

  public:

    L2ElectronSnglCandidateMicrov1();
    virtual ~L2ElectronSnglCandidateMicrov1(){}

    void    set_KilledByPC3(Bool_t input)  { KilledByPC3 = input;}
    void    set_ChargeLvl2(float input)  { chargeLvl2 = input;}
    void    set_Theta0Lvl2(float input)  { theta0Lvl2 = input;}
    void    set_Phi0Lvl2(float input)    { phi0Lvl2 = input;}
    void    set_Ptot0Lvl2(float input)   { ptot0Lvl2 = input;}
    void    set_NtowerLvl2(float input)  { ntowerLvl2 = input;}
    void    set_ArmLvl2(float input)     { armLvl2 = input;}
    void    set_SectorLvl2(float input)  { sectorLvl2 = input;}
    void    set_EcentLvl2(float input)   { ecentLvl2 = input;}
    void    set_IyLvl2(float input) 	{ iyLvl2 = input;}
    void    set_IzLvl2(float input) 	{ izLvl2 = input;}

    Bool_t  get_KilledByPC3()       { return KilledByPC3;}
    float   get_ChargeLvl2()        { return chargeLvl2;}
    float   get_Theta0Lvl2()        { return theta0Lvl2;}
    float   get_Phi0Lvl2()          { return phi0Lvl2;}
    float   get_Ptot0Lvl2()         { return ptot0Lvl2;}
    float   get_NtowerLvl2()        { return ntowerLvl2;}
    float   get_ArmLvl2()           { return armLvl2;}
    float   get_SectorLvl2()        { return sectorLvl2;}
    float   get_EcentLvl2()         { return ecentLvl2;}
    float   get_IyLvl2()		{ return iyLvl2;}
    float   get_IzLvl2()		{ return izLvl2;}

    ClassDef(L2ElectronSnglCandidateMicrov1, 1)
};

#endif	// __L2ElectronSnglCandidateMicrov1_H__
