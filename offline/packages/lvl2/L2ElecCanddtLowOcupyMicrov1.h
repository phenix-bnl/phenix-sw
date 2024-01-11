///////////////////////////////////////////////////////////
//							 //
//     Lvl2 Electron Candidate in RUN3 
//							 //
//      Org:  Wei Xie (e-mail: xiewei@rcf.rhic.bnl.gov) //
//							 //
///////////////////////////////////////////////////////////

#ifndef __L2ElecCanddtLowOcupyMicrov1_H__
#define __L2ElecCanddtLowOcupyMicrov1_H__

#include <L2ElecCanddtLowOcupyDST.h>
#include <iostream>
#include <phool.h>
#include <TClonesArray.h>


class L2ElecCanddtLowOcupyMicrov1 : public L2ElecCanddtLowOcupyDST 
{
  public:

    L2ElecCanddtLowOcupyMicrov1();
    virtual ~L2ElecCanddtLowOcupyMicrov1();
    void    identify(std::ostream& os = std::cout) const;
    void    Reset();
    int     isValid() const;

    unsigned int  get_NumCandidate() const { return NumCandidate;}
    void    set_NumCandidate(unsigned int input) {NumCandidate = input;}
    int     set_TClonesArraySize(unsigned int input);
    void    AddL2ElecCandidate(unsigned int icandidate);

    void    set_charge(unsigned int i, float input); 
    void    set_theta0(unsigned int i, float input); 
    void    set_phi0(unsigned int i, float input); 
    void    set_ptot(unsigned int i, float input);
    void    set_xrich(unsigned int i, float input);
    void    set_yrich(unsigned int i, float input);
    void    set_zrich(unsigned int i, float input);
    void    set_pmtCent(unsigned int i, int input);
    void    set_npmt(unsigned int i, int input);
    void    set_npe(unsigned int i, float input);
    void    set_TrkRingDist(unsigned int i, float input);
    void    set_xemc(unsigned int i, float input);
    void    set_yemc(unsigned int i, float input);
    void    set_zemc(unsigned int i, float input);
    void    set_energy(unsigned int i, float input);
    void    set_xpc1(unsigned int i, float input);
    void    set_ypc1(unsigned int i, float input);
    void    set_zpc1(unsigned int i, float input);

    float    get_charge(unsigned int i) const; 
    float    get_theta0(unsigned int i) const; 
    float    get_phi0(unsigned int i) const;
    float    get_ptot(unsigned int i) const;
    float    get_xrich(unsigned int i) const;
    float    get_yrich(unsigned int i) const;
    float    get_zrich(unsigned int i) const;
    int      get_pmtCent(unsigned int i) const;
    int      get_npmt(unsigned int i) const;
    float    get_npe(unsigned int i) const;
    float    get_TrkRingDist(unsigned int i) const;
    float    get_xemc(unsigned int i) const;
    float    get_yemc(unsigned int i) const;
    float    get_zemc(unsigned int i) const;
    float    get_energy(unsigned int i) const;
    float    get_xpc1(unsigned int i) const;
    float    get_ypc1(unsigned int i) const;
    float    get_zpc1(unsigned int i) const;


 protected:
 
    TClonesArray *GetL2ElecCandidate() const {return L2ElecCandidate;}
    unsigned int NumCandidate;
    TClonesArray *L2ElecCandidate;


    ClassDef(L2ElecCanddtLowOcupyMicrov1, 1)
};
#endif	// __L2ElecCanddtLowOcupyMicrov1_H__
