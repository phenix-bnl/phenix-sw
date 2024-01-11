#ifndef __CALIBSIMTOWERS_H__
#define __CALIBSIMTOWERS_H__




/*
Beau Meredith 11-11-2010

This is a class that essentially properly puts the noise in the
towers.  One shoudl put the noise level (in logain adc tics) and
calibration error in the constructor.  This module should be applied
just before clustering and after any embeddding that is done.

It goes through the towers and first puts noise into towers that do
not have any energy.  The noise is in the constructor (say 4 logain
adc tics) and this gets multiplied by the gain to determine the noise
in energy.  Additionally, a stochastic error as well as a a
calibration term is added.  It also makes a zero-suppression cut of 32
higain adc tics.  This serves to match the simulated tower energy
distribution to that observed in data.

*/

// c++ classes
#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <vector>
#include <algorithm>
#include <cmath>

//fun4all classes
#include <SubsysReco.h>
#include <PHObject.h>

//root classes
#include <TString.h>

//FYI #include statements are needed if you have non-pointers; forward declarations only are needed for pointers

//root forward declarations

class TFile;
class TTree;
class TLorentzVector;
class TVector3;
class TRandom3;

//General Fun4All Stuff
class Fun4AllHistoManager;
class PHCompositeNode;

//MPC
class MpcMap;
class MpcCalib;
class mpcClusterContainer;
class mpcClusterContent;
class mpcTowerContainer;
class mpcTowerContent;
class mpcRawContainer;
class mpcRawContent;
//END_MPC




class CalibSimTowers: public SubsysReco
{
 public:
  CalibSimTowers(int arm = 2, float t_noise = 0.045, float t_calib=0.08,
		 float adc_tics = -1,
		 const char* infile="/phenix/u/bmeredi2/workarea/devel/offline/analysis/nana/MpcPi0EmbedV2/macros/lists_flat_pt_pp/mpc_amu.root");
  virtual ~CalibSimTowers();
  
  //  For this analysis we only use Init, process_event;
  int Init         (PHCompositeNode *topNode);
  int InitRun      (PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End          (PHCompositeNode *topNode);
  int Reset        (PHCompositeNode *topNode) { return 1; }
  
  int CalibrateSimTowers(int arm);
  int PassAdcCuts(float energy, int ch);
  float GetNoise(int ch, float adc_tics=-1.0);
  
 protected:
  
  int daflag;
  int runnum;

  //these are all in %
  float term_stochastic;  // ~ sqrt(E)
  float term_noise;       // ~ constant
  float term_calib;       // ~ E

  float rms_adc_tics;

  TString fname;
  TFile* amu_file;
  TTree* amu_tree;
  short mpc_post_amu;
  short mpc_pre_amu;

  short mpc_arm;
  //  float calib_array[576];




  //MPC
  MpcMap                     *mpcmap;
  MpcCalib                     *mpccalib;
  mpcRawContainer            *mpcraw;
  mpcTowerContainer          *mpctow;
  //END_MPC

};

#endif /* __CALIBSIMTOWERS_H__ */
