#ifndef __L2ElecInvMassLowOcupyDST_H__
#define __L2ElecInvMassLowOcupyDST_H__

#include <PHObject.h>

class L2ElecInvMassLowOcupyDST : public PHObject
{
 public:
  
  L2ElecInvMassLowOcupyDST(){}
  virtual ~L2ElecInvMassLowOcupyDST() {}
  virtual void    identify(std::ostream& os = std::cout) const = 0;
  virtual void    Reset() = 0;
  virtual int     isValid() const = 0;
  
  virtual unsigned int  get_NumCandidate() const = 0;
  virtual void    set_NumCandidate(unsigned int input) = 0;
  virtual int     set_TClonesArraySize(unsigned int input) = 0;
  virtual void    AddL2ElecCandidate(unsigned int icandidate) = 0;
  
  virtual void    set_KilledByChargeID(unsigned int i, bool input) = 0; 
  virtual void    set_Mass(unsigned int i, float input) = 0; 
  virtual void    set_Pt(unsigned int i, float input) = 0; 
  virtual void    set_candID0(unsigned int i, int input) = 0; 
  virtual void    set_candID1(unsigned int i, int input) = 0; 
  
  virtual bool    get_KilledByChargeID(unsigned int i) const = 0;
  virtual float    get_Mass(unsigned int i) const = 0;
  virtual float    get_Pt(unsigned int i) const = 0;
  virtual int    get_candID0(unsigned int i) const = 0;
  virtual int    get_candID1(unsigned int i) const = 0;
  
  ClassDef(L2ElecInvMassLowOcupyDST, 1)
};
#endif	// __L2ElecInvMassLowOcupyDST_H__

    
    
