//////////////////////////////////////////////////////////
//  base class for lvl2 electron candidate in DST
//  
//  Org:  Wei Xie   (e-mail: xiewei@rcf2.rhic.bnl.gov)  
////////////////////////////////////////////////////////

#ifndef __L2ElectronCandidateDST_H__
#define __L2ElectronCandidateDST_H__

#include <iostream>
#include <PHObject.h>

class L2ElectronCandidateDST : public PHObject 
{
  public:
    L2ElectronCandidateDST() {}
    virtual ~L2ElectronCandidateDST(){}
  virtual void  identify(std::ostream& os = std::cout) const
    {
      os << "virtual L2ElectronCandidateDST object";
    }
 
    virtual void    Reset() = 0;
    virtual int     isValid() const = 0;

    virtual unsigned int  get_NumCandidate() const  = 0;
    virtual void    set_NumCandidate(unsigned int input)  = 0;
    virtual int     set_TClonesArraySize(unsigned int input) = 0;
    virtual void    AddL2ElecCandidate(unsigned int icandidate) = 0;

    virtual void    set_KilledByPC3(unsigned int i, Bool_t input) = 0; 
    virtual void    set_ChargeLvl2(unsigned int i, float input)  = 0;
    virtual void    set_Theta0Lvl2(unsigned int i, float input)  = 0;
    virtual void    set_Phi0Lvl2(unsigned int i, float input)   = 0;
    virtual void    set_Ptot0Lvl2(unsigned int i, float input)  = 0;
    virtual void    set_NtowerLvl2(unsigned int i, float input) = 0;
    virtual void    set_ArmLvl2(unsigned int i, float input)   = 0;
    virtual void    set_SectorLvl2(unsigned int i, float input) = 0; 
    virtual void    set_EcentLvl2(unsigned int i, float input)  = 0;
    virtual void    set_IyLvl2(unsigned int i, float input)  = 0;
    virtual void    set_IzLvl2(unsigned int i, float input) = 0;

    virtual Bool_t  get_KilledByPC3(unsigned int i) const = 0; 
    virtual float   get_ChargeLvl2(unsigned int i) const = 0; 
    virtual float   get_Theta0Lvl2(unsigned int i)const = 0; 
    virtual float   get_Phi0Lvl2(unsigned int i) const = 0; 
    virtual float   get_Ptot0Lvl2(unsigned int i) const = 0;
    virtual float   get_NtowerLvl2(unsigned int i) const = 0;
    virtual float   get_ArmLvl2(unsigned int i)   const = 0;
    virtual float   get_SectorLvl2(unsigned int i) const = 0;
    virtual float   get_EcentLvl2(unsigned int i)  const = 0;
    virtual float   get_IyLvl2(unsigned int i)   const = 0;
    virtual float   get_IzLvl2(unsigned int i) const = 0;

    ClassDef(L2ElectronCandidateDST, 1)

};
#endif	// __L2ElectronCandidateDST_H__
