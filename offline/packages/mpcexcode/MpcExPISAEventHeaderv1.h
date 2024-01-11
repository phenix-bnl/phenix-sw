#ifndef MPCEXPISAEVENTHEADERV1_H
#define MPCEXPISAEVENTHEADERV1_H

#include "MpcExPISAEventHeader.h"
#include "MpcExConstants.h"

class MpcExPISAEventHeaderv1: public MpcExPISAEventHeader
{
 public:
  MpcExPISAEventHeaderv1();
  virtual ~MpcExPISAEventHeaderv1();
  void Reset() {}
  void setSamplingFraction(int iarm, float sf_in) { if(iarm>=0 && iarm<MpcExConstants::NARMS) sf[iarm] = sf_in;}
  void setGEANTEnergy(int iarm, float e_in) {if(iarm>=0 && iarm<MpcExConstants::NARMS) GEANTEnergy[iarm] = e_in;}
  void setDeadAreaEnergy(int iarm, float e_in) {if(iarm>=0 && iarm<MpcExConstants::NARMS) DeadAreaEnergy[iarm] = e_in;}
  void setSiEnergy(int iarm, float e_in) {if(iarm>=0 && iarm<MpcExConstants::NARMS) SiEnergy[iarm] = e_in;}
  void setFPLTEnergy(int iarm, float e_in) {if(iarm>=0 && iarm<MpcExConstants::NARMS) FPLTEnergy[iarm] = e_in;}
  void seteLowSat(int iarm, float e_in) {if(iarm>=0 && iarm<MpcExConstants::NARMS) eLowSat[iarm] = e_in;}
  void setABSnHits(int iarm, int nhits){ if(iarm>=0 && iarm<MpcExConstants::NARMS) _n_absorber_hits[iarm] = nhits;}
  void setSinHits(int iarm, int nhits){ if(iarm>=0 && iarm<MpcExConstants::NARMS) _n_si_hits[iarm] = nhits;}
  void setDeadnHits(int iarm, int nhits){ if(iarm>=0 && iarm<MpcExConstants::NARMS) _n_dead_hits[iarm] = nhits;}
  void setFPLTnHits(int iarm, int nhits){ if(iarm>=0 && iarm<MpcExConstants::NARMS) _n_fplt_hits[iarm] = nhits;}
  void setnLowSat(int iarm, int nhits){ if(iarm>=0 && iarm<MpcExConstants::NARMS) _n_low_sat[iarm] = nhits;}

  float getSamplingFraction(int iarm) const { if(iarm>=0 && iarm<MpcExConstants::NARMS) return sf[iarm]; else return 0.0; }
  float getGEANTEnergy(int iarm) const { if(iarm>=0 && iarm<MpcExConstants::NARMS) return GEANTEnergy[iarm]; else return 0.0;}
  float getDeadAreaEnergy(int iarm) const { if(iarm>=0 && iarm<MpcExConstants::NARMS) return DeadAreaEnergy[iarm]; else return 0.0;}
  float getSiEnergy(int iarm) const { if(iarm>=0 && iarm<MpcExConstants::NARMS) return SiEnergy[iarm]; else return 0.0;}
  float getFPLTEnergy(int iarm) const { if(iarm>=0 && iarm<MpcExConstants::NARMS) return FPLTEnergy[iarm]; else return 0.0;}
  float geteLowSat(int iarm) const { if(iarm>=0 && iarm<MpcExConstants::NARMS) return eLowSat[iarm]; else return 0.0;}
  int getABSnHits(int iarm) const { if(iarm>=0 && iarm<MpcExConstants::NARMS) return _n_absorber_hits[iarm]; else return 0.0;}
  int getSinHits(int iarm) const {if(iarm>=0 && iarm<MpcExConstants::NARMS) return _n_si_hits[iarm]; else return 0.0;}
  int getDeadnHits(int iarm) const {if(iarm>=0 && iarm<MpcExConstants::NARMS) return _n_dead_hits[iarm]; else return 0.0;}
  int getFPLTnHits(int iarm) const {if(iarm>=0 && iarm<MpcExConstants::NARMS) return _n_fplt_hits[iarm]; else return 0.0;}
  int getnLowSat(int iarm) const {if(iarm>=0 && iarm<MpcExConstants::NARMS) return _n_low_sat[iarm]; else return 0.0;}

 protected:
  float sf[MpcExConstants::NARMS];
  float GEANTEnergy[MpcExConstants::NARMS]; 
  float DeadAreaEnergy[MpcExConstants::NARMS]; 
  float SiEnergy[MpcExConstants::NARMS]; 
  float FPLTEnergy[MpcExConstants::NARMS]; 
  float eLowSat[MpcExConstants::NARMS]; 
  int _n_si_hits[MpcExConstants::NARMS]; 
  int _n_absorber_hits[MpcExConstants::NARMS]; 
  int _n_dead_hits[MpcExConstants::NARMS]; 
  int _n_fplt_hits[MpcExConstants::NARMS]; 
  int _n_low_sat[MpcExConstants::NARMS]; 

 private:
  ClassDef(MpcExPISAEventHeaderv1,1)
};

#endif
