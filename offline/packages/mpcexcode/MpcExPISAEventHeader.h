#ifndef MPCEXPISAEVENTHEADER_H
#define MPCEXPISAEVENTHEADER_H

#include <PHObject.h>

class MpcExPISAEventHeader: public PHObject
{
 public:
  virtual ~MpcExPISAEventHeader() {}
  virtual void Reset() {}

  virtual void setSamplingFraction(int arm, float sf) {}
  virtual void setGEANTEnergy(int arm, float e_in) {}
  virtual void setABSEnergy(int arm, float e_in) {}
  virtual void setDeadAreaEnergy(int arm, float e_in) {}
  virtual void setSiEnergy(int arm, float e_in) {}
  virtual void setFPLTEnergy(int arm, float e_in) {}
  virtual void seteLowSat(int arm, float e_in) {}
  virtual void setABSnHits(int iarm, int nhits){}
  virtual void setSinHits(int iarm, int nhits){}
  virtual void setDeadnHits(int iarm, int nhits){}
  virtual void setFPLTnHits(int iarm, int nhits){}
  virtual void setnLowSat(int iarm, int nhits){}

  virtual void setBackLeakage(int iarm, float e_in){}
  virtual void setSideLeakage(int iarm, float e_in){}
  virtual void setBacknHits(int iarm, int nhits){}
  virtual void setSidenHits(int iarm, int nhits){}
  virtual void setInnerEnergy(int arm, float e_in) {}
  virtual void setInnernHits(int iarm, int nhits){}

  virtual float getSamplingFraction(int arm) const {return 0.0;}
  virtual float getGEANTEnergy(int arm) const {return 0.0;}
  virtual float getABSEnergy(int arm) const {return 0.0;}
  virtual float getDeadAreaEnergy(int arm) const {return 0.0;}
  virtual float getSiEnergy(int arm) const {return 0.0;}
  virtual float getFPLTEnergy(int arm) const {return 0.0;}
  virtual float geteLowSat(int arm) const {return 0.0;}
  virtual int getABSnHits(int iarm) const {return 0;}
  virtual int getSinHits(int iarm) const {return 0;}
  virtual int getDeadnHits(int iarm) const {return 0;}
  virtual int getFPLTnHits(int iarm) const {return 0;}
  virtual int getnLowSat(int iarm) const {return 0;}

  virtual float getBackLeakage(int arm) const {return 0.0;}
  virtual float getSideLeakage(int arm) const {return 0.0;}
  virtual float getInnerEnergy(int arm) const {return 0.0;}
  virtual int getBacknHits(int iarm) const {return 0;}
  virtual int getSidenHits(int iarm) const {return 0;}
  virtual int getInnernHits(int iarm) const {return 0;}

 protected:
  MpcExPISAEventHeader() {}
	 
 private:
  ClassDef(MpcExPISAEventHeader,1)
};

#endif
