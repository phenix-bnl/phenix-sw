#ifndef __EMCTOWERCONTENTV1S_H__
#define __EMCTOWERCONTENTV1S_H__

#ifndef __EMCTOWERCONTENT_H__
#include "emcTowerContent.h"
#endif

/** (VERSION) emcTowerContent version 1 for simulated towers.
@ingroup calibration
*/

class emcTowerContentv1S : public emcTowerContent 
{

public:

  emcTowerContentv1S();
  emcTowerContentv1S(const emcTowerContentv1S&);
  emcTowerContentv1S& operator=(const emcTowerContentv1S&);

  virtual ~emcTowerContentv1S();

  emcTowerContentv1S* clone() const { return new emcTowerContentv1S(*this); }
  emcTowerContentv1S* create() const { return new emcTowerContentv1S; }

  int ADC(void) const { return fADC; }  
  
  int Channel(void) const { return fChannel; }
  
  int DataError(void) const { return 0; }
  
  float Energy(void) const { return fEnergy; }
  
  unsigned int ErrorNeighbours(void) const { return fErrorNeighbours; }
  
  int FEM(void) const { return fFEM; }
  
  bool hasCalib(void) const { return fHasCalib; }
  bool hasDC(void) const { return fHasDC; }
  bool hasGain(void) const { return false; }
  bool hasRaw(void) const { return false; }

  bool canHaveCalib() const { return true; }
  bool canHaveDC() const { return true; }
  bool canHaveGain() const { return false; }
  bool canHaveRaw() const { return false; }

  void identify(std::ostream& os=std::cout) const;
  
  int isValid() const;

  bool isSimulated(void) const { return true; }

  bool isMerged(void) const { return false; }

  bool isZero(void) const;
  
  void print(std::ostream& out=std::cout, int level=0) const;
  
  void Reset();
  
  void SetADCTDC(int adc, int tdc, int hg=0, int lg=0);
  
  void SetCalibrated(float energy, float tof);
  
  void SetDataError(int) {}

  void SetID(int fem, int channel);

  void SetNeighbours(unsigned int error, unsigned int warning);
  
  void SetSimFrac(float) {}

  float SimFrac() const { return 1.0; }

  void SetToF(float tof);

  int TDC(void) const { return fTDC; }
  float ToF(void) const { return fTOF; }
  float UncorrectedToF(void) const { return fUncorrectedTOF; }

  int TowerID(void) const { return fTowerID; }
  
  unsigned int WarnNeighbours(void) const { return fWarnNeighbours; }
  
  void Zero(void);
  
private:
  void copyTo(emcTowerContentv1S& dest) const;

private:
  
  bool fHasCalib;
  bool fHasDC;
  int fFEM;
  int fChannel;
  unsigned int fErrorNeighbours;
  unsigned int fWarnNeighbours;
  int fTDC;
  int fADC;
  int fTowerID;
  float fEnergy;
  float fTOF;
  float fUncorrectedTOF;

  /// Below this value, tower is considered to be empty.
  static float fEnergyThreshold;

  ClassDef(emcTowerContentv1S,1) // EMCAL Tower data Version 1 (Simulation)
};

#endif
