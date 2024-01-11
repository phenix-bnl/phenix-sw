//////////////////////////////////////////////////////////
//  base class for lvl2 PHI candidate in DST
//  
//  Org:  Wei Xie   (e-mail: xiewei@rcf2.rhic.bnl.gov)  
////////////////////////////////////////////////////////

#ifndef __L2PHIeeInvMassDST_H__
#define __L2PHIeeInvMassDST_H__

#include <PHObject.h>

class L2PHIeeInvMassDST : public PHObject 
{
  public:
    L2PHIeeInvMassDST() {}
    virtual ~L2PHIeeInvMassDST(){}
  virtual void  identify(std::ostream& os = std::cout) const
    {
      os << "virtual L2PHIeeInvMassDST object";
    }
 
    virtual void    Reset() = 0;
    virtual int     isValid() const = 0;

    virtual unsigned int  get_NumPHIPair() const  = 0;
    virtual void    set_NumPHIPair(unsigned int input)  = 0;
    virtual int     set_TClonesArraySize(unsigned int input) = 0;
    virtual void    AddL2PHIeePair(unsigned int ipair) = 0;

    virtual void    set_1stCandidateID(unsigned int i, int input) = 0; 
    virtual void    set_2ndCandidateID(unsigned int i, int input)  = 0;
    virtual void    set_Mass(unsigned int i, float input)  = 0;

    virtual int     get_1stCandidateID(unsigned int i) const = 0; 
    virtual int     get_2ndCandidateID(unsigned int i)const = 0; 
    virtual float   get_Mass(unsigned int i) const = 0; 

    ClassDef(L2PHIeeInvMassDST, 1)

};
#endif	// __L2PHIeeInvMassDST_H__
