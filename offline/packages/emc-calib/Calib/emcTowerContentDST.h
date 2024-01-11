#ifndef __EMCTOWERCONTENTDST_H__
#define __EMCTOWERCONTENTDST_H__

#ifndef __EMCTOWERCONTENT_H__
#include "emcTowerContent.h"
#endif

/** (VERSION) emcTowerContent version 3.
@ingroup calibration
*/

class emcTowerContentDST : public emcTowerContent 
{

public:

  emcTowerContentDST();
  emcTowerContentDST(const emcTowerContentDST&);
  emcTowerContentDST& operator=(const emcTowerContentDST&);

  virtual ~emcTowerContentDST();

  emcTowerContentDST* clone() const { return new emcTowerContentDST(*this); }
  emcTowerContentDST* create() const { return new emcTowerContentDST; }

  int ADC(void) const { return fADC; }
  
  int AMUPre(void) const { return 0; }
  
  int AMUPost(void) const { return 0; }
  
  int AMUTAC(void) const { return 0; }
  
  int BeamClock(void) const { return 0; }
  
  int Channel(void) const { return 0; }
  
  int DataError(void) const { return 0; }
  
  float Energy(void) const { return fEnergy; }
  
  unsigned int ErrorNeighbours (void) const { return fErrorNeighbours; }
  
  int FEM(void) const { return 0; }
  
  bool hasCalib(void) const { return false; }
  bool hasDC(void) const { return true; }
  bool hasGain(void) const { return false; }
  bool hasRaw(void) const { return false; }

  bool canHaveCalib() const { return false; }
  bool canHaveDC() const { return false; }
  bool canHaveGain() const { return false; }
  bool canHaveRaw() const { return false; }
  
  int HG(void) const { return 0; }
  int HGPost(void) const { return 0; }
  int HGPre(void) const { return 0; }
  
  float Gain(void) const { return 1; }

  void identify(std::ostream& os=std::cout) const;
  
  int isValid() const { return 1;}

  bool isSimulated(void) const { return false; }

  bool isMerged(void) const { return false; }

  bool isZero(void) const { return false;}
  
  int LG(void) const { return 0; }
  int LGPost(void) const { return 0; }
  int LGPre(void) const { return 0; }
  
  void print(std::ostream& out=std::cout, int level=0) const;
  
  void Reset();
  
  void SetADCTDC(int adc, int tdc, int hg=0, int lg=0) 
    {
      fADC=adc;
      fTDC=tdc;
    } ;
  
  void SetCalibrated(float energy, float tof);
  
  void SetDataError(int dataerror) {} ;

  void SetGain(float gain) {};

  void SetTowerID(const int towerid) { fTowerID = towerid; } ;
  void SetClusterid(const unsigned int clusterid) { fClusterID = clusterid; } ;

  void SetID(int fem, int channel) {} ;

  void SetNeighbours(unsigned int error, unsigned int warning);
  
  void SetRaw(int hgpost, int hgpre, 
	      int lgpost, int lgpre,
	      int tac,
	      int amupre=0,
	      int amupost=0,
	      int amutac=0,
	      int beamclock=0) {} ;

  void SetSimFrac(float simfrac) {} ; 

  float SimFrac() const { return 0; }

  void SetToF(float tof);

  int TAC(void) const { return 0; }
  int TDC(void) const { return fTDC; }
  float ToF(void) const { return fTOF; }
  float UncorrectedToF(void) const { return 0; }

  int TowerID(void) const { return fTowerID; }
  unsigned int ClusterId(void) const { return fClusterID; }
  
  unsigned int WarnNeighbours(void) const { return fWarnNeighbours; }
  
  void Zero(void);
  
private:
  void copyTo(emcTowerContentDST& dest) const;

private:
  
  unsigned int fErrorNeighbours;
  unsigned int fWarnNeighbours;
  int fTowerID;
  int fADC;
  int fTDC;
  float fEnergy;
  float fTOF;
  unsigned int fClusterID;

  ClassDef(emcTowerContentDST,1) // EMCAL Tower data Version for DST
};

#endif
