///////////////////////////////////////////////////////////
//							 //
//     Lvl2 JPsi candidate 				 //	
//							 //
//      Org:  Wei Xie (e-mail: xiewei@rcf2.rhic.bnl.gov) //
//							 //
///////////////////////////////////////////////////////////

#ifndef __L2JPsieeInvMassMicrov1_H__
#define __L2JPsieeInvMassMicrov1_H__

#include <L2JPsieeInvMassDST.h>
#include <iostream>

class TClonesArray;

class L2JPsieeInvMassMicrov1 : public L2JPsieeInvMassDST 
{
  public:

    L2JPsieeInvMassMicrov1();
    virtual ~L2JPsieeInvMassMicrov1();
    void    identify(std::ostream& os = std::cout) const;
    void    Reset();
    int     isValid() const;

    unsigned int  get_NumJPsiPair() const { return NumJPsiPair;}
    void    set_NumJPsiPair(unsigned int input) {NumJPsiPair = input;}
    int     set_TClonesArraySize(unsigned int input);
    void    AddL2JPsieePair(unsigned int ipair);

    void    set_1stCandidateID(unsigned int i, int input); 
    void    set_2ndCandidateID(unsigned int i, int input) ;
    void    set_Mass(unsigned int i, float input) ;

    int     get_1stCandidateID(unsigned int i) const; 
    int     get_2ndCandidateID(unsigned int i)const; 
    float   get_Mass(unsigned int i) const; 

 protected:
 
    TClonesArray *GetL2JPsieePair() const {return L2JPsieePair;}
    unsigned int NumJPsiPair;
    TClonesArray *L2JPsieePair;

    ClassDef(L2JPsieeInvMassMicrov1, 1)
};
#endif	// __L2JPsieeInvMassMicrov1_H__
