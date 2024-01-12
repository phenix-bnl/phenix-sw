// $Author: roli $
// $Date: 2022/01/29 03:44:41 $
// $Name:  $
// $Source: /afs/rhic.bnl.gov/phenix/PHENIX_CVS/offline/framework/recal/EmcGenericEScaleRecalReco.h,v $

// the base class SubsysReco provides the stl string variable "ThisName"
// and an int variable verbosity which can be set via
// Verbosity(const int ival);
// It can be used to e.g. configure printouts from the Analysis Module

#ifndef __EMCGENERICESCALERECALRECO_H__
#define __EMCGENERICESCALERECALRECO_H__

#include "Recalibrator.h"

#include <cstring>
#include <vector>

class PHCompositeNode;

class EmcGenericEScaleRecalReco: public Recalibrator
{
 public:
  EmcGenericEScaleRecalReco(const std::string &name="EmcGenericEScaleRecalReco");
  virtual ~EmcGenericEScaleRecalReco() {}

  int InitRun(PHCompositeNode *topNode);  // Initializations which need the run number
  int process_event(PHCompositeNode *topNode); // your analysis code goes here

  int isValidRun(const int runno) const;
  
  int AddSectorCal(const float e0, const float e1, const float e2, const float e3,
                   const float w0, const float w1, const float w2, const float w3,
		   const int runmin, const int runmax=0);

  int CommitSectorCal(const int commit=0, const float e0=0, const float e1=0, const float e2=0, const float e3=0,
                   const float w0=0, const float w1=0, const float w2=0, const float w3=0,
		   const int runmin=0, const int runmax=0);
  int fetchSectorCal(const int runnumber);
  void Print(const std::string &what = "ALL") const;

  //
  // Energy Correction function
  //
  float get_correction(const int arm,const int sec,const int iz,const int iy, float _ecore) const;



 protected:
  //
  // Sector-By-Sector position correction (July 11, 2006)
  //
  void get_pos_correction(int arm, int sec,
                         float inx, float iny, float inz,
                         float& outx, float& outy, float& outz);


  std::vector<float> sector_escale;
  float r10_sector_escale[8];
  float r14_sec_escale[8];
  float r15_sec_escale[8];
  float r16_sec_escale[8];
  float r16dAu200_sec_escale[8];
  float r12_CuAu_sec_escale[8];
  int sector_escale_runmin;
  int sector_escale_runmax;

  int arm;
  int sector;
  int recalemc;
  int recalcnt;
  int recaltwr;
  int run;

  float _e_scale[2][4][96][48][4];
  int _e_scale_status[2][4][96][48];

  float _e_scale_RbyR[2][4][96][48][4];
  int _e_scale_RbyR_status[2][4][96][48];

  float corrposx[2][4];
  float corrposy[2][4];
  float corrposz[2][4];

  int nonlincorr;
  int poscorr;
  int RbyRcorr;
  int SecbySecCorr;
  int Run10RRSSCorrs;
  int Run14Corr;
  int Run15Corr;
  int Run16Corr;
  int Run16dAu200Corr;
  int Run12CuAuCorr;
};

#endif /* __EMCGENERICESCALERECALRECO_H__ */

