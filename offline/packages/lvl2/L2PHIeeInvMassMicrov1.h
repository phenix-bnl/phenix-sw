///////////////////////////////////////////////////////////
//							 //
//     Lvl2 PHI candidate 				 //	
//							 //
//      Org:  Wei Xie (e-mail: xiewei@rcf2.rhic.bnl.gov) //
//							 //
///////////////////////////////////////////////////////////

#ifndef __L2PHIeeInvMassMicrov1_H__
#define __L2PHIeeInvMassMicrov1_H__

#include <L2PHIeeInvMassDST.h>
#include <iostream>

class TClonesArray;

class L2PHIeeInvMassMicrov1 : public L2PHIeeInvMassDST 
{
  public:

    L2PHIeeInvMassMicrov1();
    virtual ~L2PHIeeInvMassMicrov1();
    void    identify(std::ostream& os = std::cout) const;
    void    Reset();
    int     isValid() const;

    unsigned int  get_NumPHIPair() const { return NumPHIPair;}
    void    set_NumPHIPair(unsigned int input) {NumPHIPair = input;}
    int     set_TClonesArraySize(unsigned int input);
    void    AddL2PHIeePair(unsigned int ipair);

    void    set_1stCandidateID(unsigned int i, int input); 
    void    set_2ndCandidateID(unsigned int i, int input) ;
    void    set_Mass(unsigned int i, float input) ;

    int     get_1stCandidateID(unsigned int i) const; 
    int     get_2ndCandidateID(unsigned int i)const; 
    float   get_Mass(unsigned int i) const; 

 protected:
 
    TClonesArray *GetL2PHIeePair() const {return L2PHIeePair;}
    unsigned int NumPHIPair;
    TClonesArray *L2PHIeePair;

    ClassDef(L2PHIeeInvMassMicrov1, 1)
};
#endif	// __L2PHIeeInvMassMicrov1_H__
