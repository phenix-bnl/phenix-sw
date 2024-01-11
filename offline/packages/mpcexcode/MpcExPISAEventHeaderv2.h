#ifndef MPCEXPISAEVENTHEADERV2_H
#define MPCEXPISAEVENTHEADERV2_H

#include "MpcExPISAEventHeader.h"
#include "MpcExConstants.h"

class MpcExPISAEventHeaderv2: public MpcExPISAEventHeader
{
 public:
  MpcExPISAEventHeaderv2();
  virtual ~MpcExPISAEventHeaderv2();
  void Reset() {}
  void setSamplingFraction(int iarm, float sf_in) { if(iarm>=0 && iarm<MpcExConstants::NARMS) sf[iarm] = sf_in;}
  void setGEANTEnergy(int iarm, float e_in) {if(iarm>=0 && iarm<MpcExConstants::NARMS) GEANTEnergy[iarm] = e_in;}
  void setABSEnergy(int iarm, float e_in) {if(iarm>=0 && iarm<MpcExConstants::NARMS) ABSEnergy[iarm] = e_in;}
  void setDeadAreaEnergy(int iarm, float e_in) {if(iarm>=0 && iarm<MpcExConstants::NARMS) DeadAreaEnergy[iarm] = e_in;}
  void setSiEnergy(int iarm, float e_in) {if(iarm>=0 && iarm<MpcExConstants::NARMS) SiEnergy[iarm] = e_in;}
  void setFPLTEnergy(int iarm, float e_in) {if(iarm>=0 && iarm<MpcExConstants::NARMS) FPLTEnergy[iarm] = e_in;}
  void seteLowSat(int iarm, float e_in) {if(iarm>=0 && iarm<MpcExConstants::NARMS) eLowSat[iarm] = e_in;}
  void setABSnHits(int iarm, int nhits){ if(iarm>=0 && iarm<MpcExConstants::NARMS) _n_absorber_hits[iarm] = nhits;}
  void setSinHits(int iarm, int nhits){ if(iarm>=0 && iarm<MpcExConstants::NARMS) _n_si_hits[iarm] = nhits;}
  void setDeadnHits(int iarm, int nhits){ if(iarm>=0 && iarm<MpcExConstants::NARMS) _n_dead_hits[iarm] = nhits;}
  void setFPLTnHits(int iarm, int nhits){ if(iarm>=0 && iarm<MpcExConstants::NARMS) _n_fplt_hits[iarm] = nhits;}
  void setnLowSat(int iarm, int nhits){ if(iarm>=0 && iarm<MpcExConstants::NARMS) _n_low_sat[iarm] = nhits;}

  void setBackLeakage(int iarm, float e_in) {if(iarm>=0 && iarm<MpcExConstants::NARMS) backLeakage[iarm] = e_in;}
  void setSideLeakage(int iarm, float e_in) {if(iarm>=0 && iarm<MpcExConstants::NARMS) sideLeakage[iarm] = e_in;}
  void setInnerEnergy(int iarm, float e_in) {if(iarm>=0 && iarm<MpcExConstants::NARMS) innerEnergy[iarm] = e_in;}
  void setBacknHits(int iarm, int nhits){ if(iarm>=0 && iarm<MpcExConstants::NARMS) _n_back_hits[iarm] = nhits;}
  void setSidenHits(int iarm, int nhits){ if(iarm>=0 && iarm<MpcExConstants::NARMS) _n_side_hits[iarm] = nhits;}
  void setInnernHits(int iarm, int nhits){ if(iarm>=0 && iarm<MpcExConstants::NARMS) _n_inner_hits[iarm] = nhits;}

  float getSamplingFraction(int iarm) const { if(iarm>=0 && iarm<MpcExConstants::NARMS) return sf[iarm]; else return 0.0; }
  float getGEANTEnergy(int iarm) const { if(iarm>=0 && iarm<MpcExConstants::NARMS) return GEANTEnergy[iarm]; else return 0.0;}
  float getABSEnergy(int iarm) const { if(iarm>=0 && iarm<MpcExConstants::NARMS) return ABSEnergy[iarm]; else return 0.0;}
  float getDeadAreaEnergy(int iarm) const { if(iarm>=0 && iarm<MpcExConstants::NARMS) return DeadAreaEnergy[iarm]; else return 0.0;}
  float getSiEnergy(int iarm) const { if(iarm>=0 && iarm<MpcExConstants::NARMS) return SiEnergy[iarm]; else return 0.0;}
  float getFPLTEnergy(int iarm) const { if(iarm>=0 && iarm<MpcExConstants::NARMS) return FPLTEnergy[iarm]; else return 0.0;}
  float geteLowSat(int iarm) const { if(iarm>=0 && iarm<MpcExConstants::NARMS) return eLowSat[iarm]; else return 0.0;}
  int getABSnHits(int iarm) const { if(iarm>=0 && iarm<MpcExConstants::NARMS) return _n_absorber_hits[iarm]; else return 0.0;}
  int getSinHits(int iarm) const {if(iarm>=0 && iarm<MpcExConstants::NARMS) return _n_si_hits[iarm]; else return 0.0;}
  int getDeadnHits(int iarm) const {if(iarm>=0 && iarm<MpcExConstants::NARMS) return _n_dead_hits[iarm]; else return 0.0;}
  int getFPLTnHits(int iarm) const {if(iarm>=0 && iarm<MpcExConstants::NARMS) return _n_fplt_hits[iarm]; else return 0.0;}
  int getnLowSat(int iarm) const {if(iarm>=0 && iarm<MpcExConstants::NARMS) return _n_low_sat[iarm]; else return 0.0;}

  float getBackLeakage(int iarm) const { if(iarm>=0 && iarm<MpcExConstants::NARMS) return backLeakage[iarm]; else return 0.0;}
  float getSideLeakage(int iarm) const { if(iarm>=0 && iarm<MpcExConstants::NARMS) return sideLeakage[iarm]; else return 0.0;}
  float getInnerEnergy(int iarm) const { if(iarm>=0 && iarm<MpcExConstants::NARMS) return innerEnergy[iarm]; else return 0.0;}
  int getBacknHits(int iarm) const { if(iarm>=0 && iarm<MpcExConstants::NARMS) return _n_back_hits[iarm]; else return 0.0;}
  int getSidenHits(int iarm) const { if(iarm>=0 && iarm<MpcExConstants::NARMS) return _n_side_hits[iarm]; else return 0.0;}
  int getInnernHits(int iarm) const { if(iarm>=0 && iarm<MpcExConstants::NARMS) return _n_inner_hits[iarm]; else return 0.0;}

 protected:
  float sf[MpcExConstants::NARMS];
  float GEANTEnergy[MpcExConstants::NARMS]; 
  float ABSEnergy[MpcExConstants::NARMS]; 
  float DeadAreaEnergy[MpcExConstants::NARMS]; 
  float SiEnergy[MpcExConstants::NARMS]; 
  float FPLTEnergy[MpcExConstants::NARMS]; 
  float eLowSat[MpcExConstants::NARMS]; 
  float backLeakage[MpcExConstants::NARMS]; 
  float sideLeakage[MpcExConstants::NARMS]; 
  float innerEnergy[MpcExConstants::NARMS]; 
  
  int _n_si_hits[MpcExConstants::NARMS]; 
  int _n_absorber_hits[MpcExConstants::NARMS]; 
  int _n_dead_hits[MpcExConstants::NARMS]; 
  int _n_fplt_hits[MpcExConstants::NARMS]; 
  int _n_low_sat[MpcExConstants::NARMS]; 
  int _n_back_hits[MpcExConstants::NARMS]; 
  int _n_side_hits[MpcExConstants::NARMS]; 
  int _n_inner_hits[MpcExConstants::NARMS]; 

 private:
  ClassDef(MpcExPISAEventHeaderv2,1)
};

#endif
