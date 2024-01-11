///////////////////////////////////////////////////////////
//							 //
//     Lvl2 Electron Candidate  
//							 //
//      Org:  Wei Xie (e-mail: xiewei@rcf2.rhic.bnl.gov) //
//							 //
///////////////////////////////////////////////////////////

#ifndef __L2ElectronCandidateMicrov1__
#define __L2ElectronCandidateMicrov1__

#include <L2ElectronCandidateDST.h>
#include <iostream>

class TClonesArray;

class L2ElectronCandidateMicrov1 : public L2ElectronCandidateDST 
{
  public:

    L2ElectronCandidateMicrov1();
    virtual ~L2ElectronCandidateMicrov1();
    void    identify(std::ostream& os = std::cout) const;
    void    Reset();
    int     isValid() const;

    unsigned int  get_NumCandidate() const { return NumCandidate;}
    void    set_NumCandidate(unsigned int input) {NumCandidate = input;}
    int     set_TClonesArraySize(unsigned int input);
    void    AddL2ElecCandidate(unsigned int icandidate);

    void    set_KilledByPC3(unsigned int i, Bool_t input); 
    void    set_ChargeLvl2(unsigned int i, float input) ;
    void    set_Theta0Lvl2(unsigned int i, float input) ;
    void    set_Phi0Lvl2(unsigned int i, float input)  ;
    void    set_Ptot0Lvl2(unsigned int i, float input) ;
    void    set_NtowerLvl2(unsigned int i, float input);
    void    set_ArmLvl2(unsigned int i, float input)  ;
    void    set_SectorLvl2(unsigned int i, float input); 
    void    set_EcentLvl2(unsigned int i, float input) ;
    void    set_IyLvl2(unsigned int i, float input) ;
    void    set_IzLvl2(unsigned int i, float input);

    Bool_t  get_KilledByPC3(unsigned int i) const; 
    float   get_ChargeLvl2(unsigned int i) const; 
    float   get_Theta0Lvl2(unsigned int i)const; 
    float   get_Phi0Lvl2(unsigned int i) const; 
    float   get_Ptot0Lvl2(unsigned int i) const;
    float   get_NtowerLvl2(unsigned int i) const;
    float   get_ArmLvl2(unsigned int i)   const;
    float   get_SectorLvl2(unsigned int i) const;
    float   get_EcentLvl2(unsigned int i)  const;
    float   get_IyLvl2(unsigned int i)	 const;
    float   get_IzLvl2(unsigned int i) const;

 protected:
 
    TClonesArray *GetL2ElecCandidate() const {return L2ElecCandidate;}
    unsigned int NumCandidate;
    TClonesArray *L2ElecCandidate;


    ClassDef(L2ElectronCandidateMicrov1, 1)
};
#endif	// __L2ElectronCandidateMicrov1__
