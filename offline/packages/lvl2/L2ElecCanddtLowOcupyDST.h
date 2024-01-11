///////////////////////////////////////////////////////////
//							 //
//     Lvl2 Electron Candidate in RUN3 
//							 //
//      Org:  Wei Xie (e-mail: xiewei@rcf.rhic.bnl.gov) //
//							 //
///////////////////////////////////////////////////////////

#ifndef __L2ElecCanddtLowOcupyDST_H__
#define __L2ElecCanddtLowOcupyDST_H__

#include <PHObject.h>

class L2ElecCanddtLowOcupyDST : public PHObject 
{
 public:
  
  L2ElecCanddtLowOcupyDST(){}
  virtual ~L2ElecCanddtLowOcupyDST() {}
  virtual void    identify(std::ostream& os = std::cout) const = 0;
  virtual void    Reset() = 0;
  virtual int     isValid() const = 0;
  
  virtual unsigned int  get_NumCandidate() const = 0;
  virtual void    set_NumCandidate(unsigned int input)  = 0;
  virtual int     set_TClonesArraySize(unsigned int input) = 0;
  virtual void    AddL2ElecCandidate(unsigned int icandidate) = 0;
  
  virtual void    set_charge(unsigned int i, float input) = 0; 
  virtual void    set_theta0(unsigned int i, float input) = 0; 
  virtual void    set_phi0(unsigned int i, float input) = 0; 
  virtual void    set_ptot(unsigned int i, float input) = 0;
  virtual void    set_xrich(unsigned int i, float input) = 0;
  virtual void    set_yrich(unsigned int i, float input) = 0;
  virtual void    set_zrich(unsigned int i, float input) = 0;
  virtual void    set_pmtCent(unsigned int i, int input) = 0;
  virtual void    set_npmt(unsigned int i, int input) = 0;
  virtual void    set_npe(unsigned int i, float input) = 0;
  virtual void    set_TrkRingDist(unsigned int i, float input) = 0;
  virtual void    set_xemc(unsigned int i, float input) = 0;
  virtual void    set_yemc(unsigned int i, float input) = 0;
  virtual void    set_zemc(unsigned int i, float input) = 0;
  virtual void    set_energy(unsigned int i, float input) = 0;
  virtual void    set_xpc1(unsigned int i, float input) = 0;
  virtual void    set_ypc1(unsigned int i, float input) = 0;
  virtual void    set_zpc1(unsigned int i, float input) = 0;
  
  virtual float    get_charge(unsigned int i) const = 0; 
  virtual float    get_theta0(unsigned int i) const = 0; 
  virtual float    get_phi0(unsigned int i) const = 0;
  virtual float    get_ptot(unsigned int i) const = 0;
  virtual float    get_xrich(unsigned int i) const = 0;
  virtual float    get_yrich(unsigned int i) const = 0;
  virtual float    get_zrich(unsigned int i) const = 0;
  virtual int      get_pmtCent(unsigned int i) const = 0;
  virtual int      get_npmt(unsigned int i) const = 0;
  virtual float    get_npe(unsigned int i) const = 0;
  virtual float    get_TrkRingDist(unsigned int i) const = 0;
  virtual float    get_xemc(unsigned int i) const = 0;
  virtual float    get_yemc(unsigned int i) const = 0;
  virtual float    get_zemc(unsigned int i) const = 0;
  virtual float    get_energy(unsigned int i) const = 0;
  virtual float    get_xpc1(unsigned int i) const = 0;
  virtual float    get_ypc1(unsigned int i) const = 0;
  virtual float    get_zpc1(unsigned int i) const = 0;
  
  ClassDef(L2ElecCanddtLowOcupyDST, 1)
};
#endif	// __L2ElecCanddtLowOcupyDST_H__
    
