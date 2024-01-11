#ifndef __MNEWDCHSIMULATOR_H__
#define __MNEWDCHSIMULATOR_H__

#include <phool.h>
#include <PHLine.h>
#include <PHPointerList.h>
#include <dDchRawWrapper.h> 

#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>

class PHCompositeNode;

class PHDchAddressObject;
class PHDchCalibrationObject;
class PHDchGeometryObject; 
class PHDchHistogrammer;
class PHDchNoiseObject;

class mNewDchSimulator
{
public:
  mNewDchSimulator(const int iseed = -1);
  virtual ~mNewDchSimulator();
  PHBoolean event( PHCompositeNode *);
  int getWidth();
  float getResolution();
  PHBoolean BackEfficient();
  PHBoolean Efficient();
  void initialize ();
  void setPanFlag (const int dummy) { PanFlag = dummy; } 
  void setBackEffFlag (const int dummy) { BackEffFlag = dummy; }
  void setEffFlag (const int dummy) { EffFlag = dummy; }
  void setMergeFlag (const int dummy) { MergeFlag = dummy; }
  void setResFlag (const int dummy) { ResFlag = dummy; }
  void setNoiseFlag (const int dummy) { AddNoiseFlag = dummy; }
  void setHistogrammer(PHDchHistogrammer* histo) {dchHistogrammer = histo;}
  void MergeHits(int, TABLE_HEAD_ST *dDchRaw_h,DDCHRAW_ST *dDchRaw);
  void SimulateUVBackDrift(const PHLine&, const PHPoint&, const PHVector&, const PHLine&);
  void setSmearConstants( const double, const double, const double, const double);
  
protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
  
public:  
  const PHDchAddressObject* getDAO() const { return dchAddressObject;}
  const PHDchGeometryObject* getDGO() const { return dchGeometryObject;}
  const PHDchCalibrationObject* getDCO() const { return dchCalibrationObject;}
  const PHDchNoiseObject* getDNO() const { return dchNoiseObject;}
  const PHDchHistogrammer* getHistogrammer() const { return dchHistogrammer;}

  gsl_rng * rand;

  PHDchGeometryObject*    dchGeometryObject;
  PHDchAddressObject*     dchAddressObject;
  PHDchCalibrationObject* dchCalibrationObject;
  PHDchNoiseObject*       dchNoiseObject;
  PHDchHistogrammer*      dchHistogrammer;
  
  unsigned int arm; 
  short plane; 
  short cell; 
  short side;
  int globalIndex;
  double alpha;
  double phi;
  double tmpangle;
  double DriftWidth;
  double BackDriftWidth;
  double BackDriftShift;
  double BackDriftCut;
  double RProp;
  double BackInclAngle;
  double dist,dist_b1,dist_b2;
  short width;
  short DistSign;
  double Glob_dv_sig;
  double Key_dv_sig;
  double SW_dv_sig;
  double SW_t0_sig;
  
  PHPoint  wireNorthPoint[2][40][80];
  PHPoint  wireSouthPoint[2][40][80];
  PHVector wireNorthDrift[2][40][80];
  PHVector wireSouthDrift[2][40][80];
  PHPoint  wireNorthPointPISA[2][40][80];
  PHPoint  wireSouthPointPISA[2][40][80];
  PHVector wireNorthDriftPISA[2][40][80];
  PHVector wireSouthDriftPISA[2][40][80];
  double Key_dv_cor[2][40][20];      // Keystone-by-keystone drift velocity corrections
  double Glob_dv_cor;                 // Event-by-Event drift velocity correction
  short DriftSign[2][40][80];
  int nibbleArray[2][2][40][80][48];  // Array for hit digitization. Huge one 


  int PanFlag;
  int BackEffFlag;
  int EffFlag; 
  int ResFlag;
  int MergeFlag;
  int AddNoiseFlag;
};

#endif /*__MNEWDCHSIMULATOR_H__*/















