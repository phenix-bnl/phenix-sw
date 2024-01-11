#ifndef __MPCEMBEDPREP_H__
#define __MPCEMBEDPREP_H__


/*
Beau Meredith 11-11-2010

This is a class the prepare the towers for embedding.  For real data,
it puts noise into towers where the noise level is < 32 higain adc
tics.  The reason for this is that the data already has zero
suppression cut put in the data.  For simulations, it adds
mpcTowerContent objects onto the node tree with no noise or energy.
The noise is added in the final step in the module CalibrateSimTowers


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
class mpcNoiseContainer;
//END_MPC




class MpcEmbedPrep: public SubsysReco
{
 public:
  MpcEmbedPrep(int arm = 2, float t_noise = 0.045, float t_calib=0.08,
	       float adc_tics = -1);
  virtual ~MpcEmbedPrep();
  
  //  For this analysis we only use Init, process_event;
  int Init         (PHCompositeNode *topNode);
  int InitRun      (PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End          (PHCompositeNode *topNode);
  int Reset        (PHCompositeNode *topNode) { return 1; }
 
  int CalibrateSimTowers(int arm);
  int CalibrateRealTowers(int arm);
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

  short mpc_arm;



  //MPC
  MpcMap                     *mpcmap;
  MpcCalib                     *mpccalib;
  mpcRawContainer            *mpcraw;
  mpcTowerContainer          *mpctow;
  //  mpcNoiseContainer          *mpcnoise;
  //END_MPC

};

#endif /* __MPCEMBEDPREP__ */
