///////////////////////////////////////////////////////////
//							 //
//     Lvl2 Electron Pair Candidate in RUN3 
//							 //
//      Org:  Wei Xie (e-mail: xiewei@rcf.rhic.bnl.gov) //
//							 //
///////////////////////////////////////////////////////////

#ifndef __L2ElecInvMassLowOcupyMicroV1_H__
#define __L2ElecInvMassLowOcupyMicroV1_H__

#include <L2ElecInvMassLowOcupyDST.h>
#include <iostream>
#include <phool.h>
#include <TClonesArray.h>


class L2ElecInvMassLowOcupyMicroV1 : public L2ElecInvMassLowOcupyDST
{
  public:

    L2ElecInvMassLowOcupyMicroV1();
    virtual ~L2ElecInvMassLowOcupyMicroV1();
    void    identify(std::ostream& os = std::cout) const;
    void    Reset();
    int     isValid() const;

    unsigned int  get_NumCandidate() const { return NumCandidate;}
    void    set_NumCandidate(unsigned int input) {NumCandidate = input;}
    int     set_TClonesArraySize(unsigned int input);
    void    AddL2ElecCandidate(unsigned int icandidate);

    void    set_KilledByChargeID(unsigned int i, bool input); 
    void    set_Mass(unsigned int i, float input); 
    void    set_Pt(unsigned int i, float input); 
    void    set_candID0(unsigned int i, int input); 
    void    set_candID1(unsigned int i, int input); 

    bool    get_KilledByChargeID(unsigned int i) const;
    float    get_Mass(unsigned int i) const;
    float    get_Pt(unsigned int i) const;
    int    get_candID0(unsigned int i) const;
    int    get_candID1(unsigned int i) const;

 protected:
 
    TClonesArray *GetL2ElecCandidate() const {return L2ElecCandidate;}
    unsigned int NumCandidate;
    TClonesArray *L2ElecCandidate;


    ClassDef(L2ElecInvMassLowOcupyMicroV1, 1)
};
#endif	// __L2ElecInvMassLowOcupyMicroV1_H__
