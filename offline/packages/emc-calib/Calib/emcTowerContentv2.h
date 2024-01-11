#ifndef __EMCTOWERCONTENTV2_H__
#define __EMCTOWERCONTENTV2_H__

#ifndef __EMCTOWERCONTENT_H__
#include "emcTowerContent.h"
#endif

/** (VERSION) emcTowerContent version 2.
@ingroup calibration
*/

class emcTowerContentv2 : public emcTowerContent 
{

public:

  emcTowerContentv2();

  virtual ~emcTowerContentv2();
  
  int ADC(void) const { return fADC; }
  
  int AMUPre(void) const { return fAMUPre; }
  
  int AMUPost(void) const { return fAMUPost; }
  
  int AMUTAC(void) const { return fAMUTAC; }
  
  int BeamClock(void) const { return fBeamClock; }
  
  int Channel(void) const { return fChannel; }
  
  int DataError(void) const { return fDataError; }
  
  float Energy(void) const { return fEnergy; }
  
  unsigned int ErrorNeighbours(void) const { return fErrorNeighbours; }
  
  int FEM(void) const { return fFEM; }
  
  bool hasCalib(void) const { return fHasCalib; }
  bool hasDC(void) const { return fHasDC; }
  bool hasGain(void) const { return true; }
  bool hasRaw(void) const { return fHasRaw; }

  bool canHaveCalib() const { return true; }
  bool canHaveDC() const { return true; }
  bool canHaveGain() const { return true; }
  bool canHaveRaw() const { return true; }

  int HG(void) const { return fHG; }
  int HGPost(void) const { return fHGPost; }
  int HGPre(void) const { return fHGPre; }
  
  float Gain(void) const { return fGain; }

  void identify(std::ostream& os=std::cout) const;
  
  int isValid() const;

  bool isSimulated(void) const { return false; }

  bool isMerged(void) const { return false; }

  bool isZero(void) const;
  
  int LG(void) const { return fLG; }
  int LGPost(void) const { return fLGPost; }
  int LGPre(void) const { return fLGPre; }
  
  void print(std::ostream& out=std::cout, int level=0) const;
  
  void Reset();
  
  void SetADCTDC(int adc, int tdc, int hg=0, int lg=0);
  
  void SetCalibrated(float energy, float tof);
  
  void SetDataError(int dataerror);

  void SetGain(float gain);

  void SetID(int fem, int channel);

  void SetNeighbours(unsigned int error, unsigned int warning);
  
  void SetRaw(int hgpost, int hgpre, 
	      int lgpost, int lgpre,
	      int tac,
	      int amupre=0,
	      int amupost=0,
	      int amutac=0,
	      int beamclock=0);
  
  int TAC(void) const { return fTAC; }
  int TDC(void) const { return fTDC; }
  float ToF(void) const { return fTOF; }
  
  int TowerID(void) const { return fTowerID; }
  
  unsigned int WarnNeighbours(void) const { return fWarnNeighbours; }
  
  void Zero(void);
  
private:
  
  bool fHasCalib;
  bool fHasDC;
  bool fHasRaw;
  int fFEM;
  int fChannel;
  int fDataError;
  unsigned int fErrorNeighbours;
  unsigned int fWarnNeighbours;
  int fHGPost;
  int fHGPre;
  int fLGPost;
  int fLGPre;
  int fTAC;
  int fTDC;
  int fADC;
  int fHG;
  int fLG;
  int fTowerID;
  int fBeamClock;
  short fAMUPre;
  short fAMUPost;
  short fAMUTAC;
  float fEnergy;
  float fTOF;
  float fGain;

  ClassDef(emcTowerContentv2,1) // EMCAL Tower data Version 2
};

#endif
