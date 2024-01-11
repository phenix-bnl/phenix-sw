#ifndef __MPCEVENT_H__
#define __MPCEVENT_H__

#include <string>
#include <ctime>
#include <phool.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <TGraph.h>

class PHCompositeNode;
class Event;
class mpcSampleContainer;
class mpcRawContainer;
class mpcTowerContainer;
class mpcClusterContainer;
class mpcGeaTowerContainer;
class PHGlobal;
class MpcMap;
class MpcCalib;
class PdbMpcShape;
class recoConsts;
class MpcSectorRec;
class PHTimeStamp;
class fkinWrapper;

class TTree;
class TGraph;

// DEBUG
class TFile;
class TH2D; 

class MpcEvent
{
public:
  MpcEvent(PHCompositeNode *topNode = 0);
  virtual ~MpcEvent();

  void Clear();
  void Save();

  PHBoolean SetRawData (Event *);

  PHBoolean ProcessOneEvent( PHCompositeNode *topNode );
  //PHBoolean DoReconstruction( PHCompositeNode *topNode );

  //PHBoolean setEvent (PHCompositeNode *) { return True; }
  PHBoolean GetDCM(PHCompositeNode *);
  PHBoolean CalibrateTowers(PHCompositeNode *);
  PHBoolean CalibrateSimTowers(PHCompositeNode *);
  PHBoolean DoClustering(PHCompositeNode *);
  PHBoolean Cluster_GAMS_Algorithm(const float zvtx);
  PHBoolean Cluster_MpcGAMS_Algorithm(const float zvtx);

  int MakeOriginMap(PHCompositeNode*);

  int  GetEventNumber () const { return (EventNumber); }
  void SetEventNumber (int givenEventNumber) { EventNumber = givenEventNumber; }

  int ReadNewGains(const char *newgainfilename);
  int ApplyNewGains( PHCompositeNode *topNode );

  int ReadNewNoise(const char *newnoisefilename);
  int ApplyNewNoise( PHCompositeNode *topNode );

  //int  GetCalibrations(const PHTimeStamp& tstamp);


private:
  static const int NSAMPLES = 12;
  static const int NFEECH = 576;

  void SubtractDriverNoise( const int noisy_driver );

  PHBoolean GetAmplitudes(PHCompositeNode* topNode);
  //int GetAmplitude_algo1(int adc[12], int ch, float &amp, int &tdc, short &fquality);
  int GetAmplitude_algo2(const int ch, const int *adc, Float_t &amp, Short_t &tdc, Float_t &fquality,
			 Float_t &amp1, Float_t &amp2, Short_t &tdc1, Short_t &tdc2, Float_t &ZSM);
  void GetShapes();                             // Get Shapes for Pulse Fitting
  Double_t SplineFitFcn(Double_t *x, Double_t *par);
  Double_t SplineDoublePulseFitFcn(Double_t *x, Double_t *par);
  Double_t SplineErrFcn(Double_t *x, Double_t *par);
  bool FitOK(); 
  PdbMpcShape *fit_shape;			// Pointer to fit shape
  PdbMpcShape *fit_sherr;			// Pointer to fit shape errors
  PdbMpcShape *fit_pshape[NFEECH];
  PdbMpcShape *fit_psherr[NFEECH];
  PdbMpcShape *fit_np1shape[NFEECH];
  PdbMpcShape *fit_np1sherr[NFEECH];

  //float ped[NFEECH][NSAMPLES];
  //float pedrms[NFEECH][NSAMPLES];

  int RunNumber;
  time_t RunMeanTime;
  float RunEnergyCorr;
  int EventNumber;
  Event *event;
  int ifemchmax;                //Able to handle both packet configurations
  recoConsts *reco_consts;	// stores var's to control reco behavior
  int mpc_reco_mode;		// reconstruction mode
  int mpc_gaincorr_flag;	// whether to apply gain corr, 0=don't apply
  float mpc_recobadvtx;		// zvtx used to reco clusters in events w bad zvtx
				// we otherwise skip these events

  int mpc_newgain_flag;		// whether to apply new private gains, 0=don't apply
  std::string mpc_newgain_file;	// file containing new gains
  float mpc_new_gain[NFEECH];	// the new gains to apply

  int mpc_newnoise_flag;	// whether to apply new private noise levels, 0=don't apply
  std::string mpc_newnoise_file;	// file containing new noise levels
  float mpc_new_noise[NFEECH];	// the new noise to apply, in sigma

  int simulationflag;		// simulation mode
  int mpc_event_display;	// whether to generate evt display
  int mpc_cluster_alg;		// which clustering alg to use
  int mpc_verbosity;		// how much info to print out

  static const int MAX_SIMTRACKS = 200000;
  int my_itorigin[MAX_SIMTRACKS];
  int my_idorigin[MAX_SIMTRACKS];

  MpcMap   *mpcmap;	// channel mapping class
  MpcCalib *mpccalib;	// calibrations class (returns calib values)

  PHGlobal *phglobal;
  fkinWrapper *fkin;	// fkin info

  mpcSampleContainer *mpcsamples;	// Raw Input Object (TDCs,ADCs)
  mpcRawContainer *mpcraw;	// Raw Input Object (TDCs,ADCs)
  mpcRawContainer *mpcraw2;	// New Electronics Raw Input Object
  mpcTowerContainer *mpctower;	// Calibrated Towers (tof,energy)
  mpcClusterContainer *mpcclus;	// MPC Clusters
  mpcGeaTowerContainer *mpcgeatower;	// MC Evaluation Towers

  TGraph *s_cluster_position;	// graph of cluster positions

  // Common Clustering Variables
  float towerThresh;	// min energy threshold for towers
  float peakThresh;	// min energy threshold for peaks
  int   chi2limit;

  // For Clustering Using the GAMS Algorithm
  MpcSectorRec *fMpcSector[2];

  const gsl_rng_type *gsl_rand_type;
  gsl_rng *gsl_rand_gen;

  int first_time; //set to 0 initially, 1 after first call to ProcessOneEvent

  static int max(int a, int b)
  {
    return a > b ? a : b;
  }
  static float max(float a, float b)
  {
    return a > b ? a : b;
  }
  static double max(double a, double b)
  {
    return a > b ? a : b;
  }

  // DEBUG

  bool NewEvent; 
  int canIndex[18][18];

};

#endif	// __MPCEVENT_H__

