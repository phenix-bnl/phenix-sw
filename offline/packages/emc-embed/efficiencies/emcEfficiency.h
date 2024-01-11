#ifndef __emcEfficiency_h__
#define __emcEfficiency_h__

#ifndef __CINT__
# include <map>
# include <string>
# include <vector>
# include <utility> // to get pair<T1,T2>
#endif

#include <iostream>

using namespace std;

class HistogramCollection;
class PHNodeIOManager;
class PHCompositeNode;
class TFile;
class TDirectory;
class EventSorting;
class dEmcGeaTrackWrapper;
class dEmcGeaClusterTrackWrapper;
class EmcGeaParticlev2;
class EmcGeaClusterv2;
class TClonesArray;
class TH1;
class TH2;
class TH1F;
class TF1;
class TNamed;
class TString;
class mEmcGeometryModule;
class effPair;

/**
 *
 * pi0/eta efficiency computation
 * 
 **/

class emcEfficiency
{

public:

  emcEfficiency(int run = 2,int typeOfpart = 7);
  ~emcEfficiency();

  /// Close both input and output files.
  void close(void);

  /// Close input file.
  void closeEmbedFile(void);

  /// Close output file.
  void closeEfficiencyFile(void);

  /// Compute the efficiency histograms for a given cut/centclass  
  void efficiency(const char* cutname, 
		  const size_t iCent, 
		  const double normalization = 1.0);

  // Compute the efficiency histograms for all cuts.
  // An efficiency file must have been opened
  void efficiency_all_cuts(void);

  /// Correct for a wrong randomization of Run-2 Exodus input single-particle generator
  bool fitFluctuation(void);

  /// Open input file (the eval+embed one).
  bool openEmbedFile(const char* inputfilename);

  // Open the output file (the one containing all the histograms
  // needed to compute efficiency, and the efficiencies themselves).
  bool openEfficiencyFile(const char* outputfilename, 
			  const char* opt="RECREATE");

  /// Print out the fitted efficiency correction factors for a given cut
  void outputFinalEfficiencyFactors(const char* cutname, const char* type = "counts", 
				    const double ptminfit = 1., 
				    const double ptmaxfit = 7.,
				    const bool plot = true,
				    ostream& out = cout,
				    const char* format = "c++");

  /// Print out the fitted efficiency correction factors for all cuts
  void outputFinalEfficiencyFactors_all_cuts( const char* type = "counts", 
					      const double ptminfit = 1., 
					      const double ptmaxfit = 7.,
					      ostream& out = cout );

  /// Print the defined cuts
  void printCuts();

  /// Read the efficiency file
  bool readEfficiencyFile(const char* effFileName);

  /// Loop over input events
  void run(const int maxevents=-1);

  /// Set the pT range
  void setPTrange(double ptmin, double ptmax );

  /// Set the fixed/variable pi0/eta extraction window mode
  void setFixedExtractionWindows(bool fixed = true );

  /// Set type of embedding objects: 0 = "old staf tables", 1 = "new justin objs."
  void setEmbedObjectsType( int type = 1 ) { fEmbedObjectsType = type; }

  /// Select only gamma pairs that go towards PbSc (default), PbGl, or both.
  void computeEfficiencyFor( const char *calorimeter = "PbSc" );

  /// Set the fluctuation correction function histogram (to be used to weight the weights...)
  void setRun2ExodusFluctCorrection(const TH1* h, const char* title);

  /// Export and set a fluctuation correction from another file
  bool setRun2ExodusFluctCorrectionFromFile( const char* efficFile1 );

  void setVerbose(int verbose=0) { fVerbose=verbose; }

  /// use "magic factors" (fast MC/embedded) for the dead-warn & fiducial losses
  void useFastMCDeadWarnEfficLoss(const bool loss = true) { fUseFastMCDeadWarnLoss = loss; };

  /// Get the status of the application of Run-2 fluctuation correction
  void statusExodusFluctCorrectionHisto( TString& status );

  /// Get the descriptor of the centrality classes for the effic. object
  EventSorting* getEventSorting(void) { return fEventSorting ; }

  /// Produce the typical invariant-mass fitted plot shown in pi0 AnaNotes
  void subtractLikePlot(const size_t ptbin,
			const char* cutName, 
			const size_t iCent);

  /// Print-out efficiency tables for a given cut/centclass
  std::ostream& table(std::ostream& out,
		      const char* cutName,
		      const size_t iCent,
		      const bool latex=false);

  /// Print-out efficiency tables for a given cut
  std::ostream& table(std::ostream& out,
		      const char* cutName,
		      const bool latex=false);

  void writeEfficiencyFile(void);

  enum EValue { kDetected_count=0, kDetected_fit, kDetected_bckgd,
		kEfficiency_count, kEfficiency_fit, kEfficiency_comb,
		kInitial_pions,
		kIntegrationWindow,
		kFixedIntegrationWindow,
		kSignal2Background,
		kEValueMax};

  //_____________________________________________________________________________

  class emcEffValue
  {
  public:
    emcEffValue() : fValue(0),fValueMin(0),fValueMax(0) {}
    
    emcEffValue(double value, double error)
    { set(value,error); }
    emcEffValue(double value, double valmin, double valmax) 
    { set(value,valmin,valmax); }

    double value(void) const { return fValue; }
    double valueMin(void) const { return fValueMin; }
    double valueMax(void) const { return fValueMax; }
    double error(void) const { return fValueMax-fValueMin; }
    
    emcEffValue& operator*=(double v);

    void set(double value, double error);
    void set(double value, double valmin, double valmax);

  private:
    
    double fValue;
    double fValueMin;
    double fValueMax;
  };

  //_____________________________________________________________________________
  
#ifndef __CINT__

  void setHagWeightPerCentrality(std::vector< pair<double,double> >& weights);

  template<class T>
  T* getHistogram(const std::string cutname, 
		  const std::string histoname,
		  const size_t iCent) const;

private:

  void book(void);
  bool check_cut(const char* cutname);

  void computeEfficiency(const TH1* hInitialPt,
			 TH1* hRecoMinv,
			 const size_t iCent,
			 const size_t ipt,
			 const double normalization,
			 std::vector<emcEffValue>& values);

  void countNumberOfMesons(TH1* hminv, 
			   const size_t iCent,
			   const size_t ipt,
			   const double normalization,
			   std::vector<emcEffValue>& values);

  void createCutCollection(HistogramCollection* col, int full=0);
  void createGeneralCollection(HistogramCollection* col);
  void createSimCollection(HistogramCollection* col);

  void CutDefinition(TNamed* cutname);

  void defaultCentralityClasses(void);
  void defaultRun2ExodusFluctCorrection(void);
  void defaultHagWeightPerCentrality(void);
  void defineCutNames(void);

  TH1* getCombinedEfficHisto(const char* cutname, const size_t iCent);
  void modifyEfficHisto( TH1 *h1 );

  void fill(const effPair& pair);
  void fill(const std::string cutname, 
	    const effPair& pair,
	    const size_t iCent, 
	    const double weight);
  void fill(const std::string cutname, 
	    const effPair& pair, 
	    const double weight);

  void fillPeakAndWidth(const size_t ipt,
			const TH1* hminv, 
			TH1* hPeakPos, 
			TH1* hPeakWidth);
			//TH2* hPeakPos,
			//TH2* hPeakWidth);
  void findPeak(TH1* h, double& xpeak, double& ypeak);
  void fitMesonPeak(TH1* hminv, const double pt);

  TF1* InitFitEfficiency(const double ptminfit = 1.,
			 const double ptmaxfit = 7.);

  TF1* fitEfficiency(const char* cutname, const char* type, 
		     const size_t iCent, 
		     const double ptminfit = 1., 
		     const double ptmaxfit = 7.,
		     const bool plot = true);

  mEmcGeometryModule* geometryModule(void) const;

  template<class T>
  T* getHistogram(const std::string cutname, 
		  const std::string histoname) const;

  bool getInitialParticleInfo(void);
  void getListOfMergedParticles(void);
  void getListOfSimulParticles(void);

  double getNormalization(TH1* hCentClass, const size_t iCent);

  std::string getPath(const std::string cutname, const size_t iCent=0) const;
  std::string getPath(const std::string dir, const std::string subdir) const;

  HistogramCollection* histoCollection(const std::string cutname);

  HistogramCollection* histoCollection(const std::string cutname, 
				       const size_t index);

  void integral(const TH1* h, const int binmin, const int binmax,
		double& integralValue, double& integralError) const;

  bool isInFiducial(const size_t i);
  bool isInPbSc(const size_t i) const;
  bool isInPbGl(const size_t i) const;
  bool isConversionEvt() const;

  void makeCorrectionFactors(ostream& out, std::vector<double>& p0);

  void minvMergedParticles(void);
  void minvSimulParticles(void);

  int passCuts(const effPair& pair) const;

  void printDecodedMask(const int mask);

  void meson_reconstruction(void);

  double mesonHagedornWeight(const double pt, const size_t iCent, double& corr);

  bool readEfficiencyFile(void);

  bool readFixedExtractionWindows(void);

  void reset(void);

  void binary(int n);

private:

  TFile* fEfficiencyFile;

  PHNodeIOManager* fIOmanager ;
  PHCompositeNode* fNodeTOP;

  PHCompositeNode* fNodeEVA;  // old style embed objects
  PHCompositeNode* fNodeUDST; // new style embed objects

  int fEmbedObjectsType; // 0 for "old" STAF style, 1 for new (Justin's) objects

  // old style embedding objects
  dEmcGeaTrackWrapper* fEmcGeaTrack; // simulated tracks in the event
  dEmcGeaClusterTrackWrapper* fEmcGeaClusterTrack; // merged clusters in the event

  // new style embedding objects
  //EmcGeaParticle* getNewGeaParticle(); // simulated particles in the event
  //EmcGeaCluster*  getNewGeaCluster();  // merged clusters in the event

  EmcGeaParticlev2* fEmcGeaPart;    // simulated particles in the event
  EmcGeaClusterv2* fEmcGeaCluster;  // merged clusters in the event

  std::string fEmbedFileName; // input embed file name

  std::string fEfficiencyFileName; // output file name

  std::map<std::string,vector<HistogramCollection*> > fH;
  std::map<std::string,int> fCuts;
  std::map<int,std::string> fCutMaskToString;
  std::vector<pair<double,double> > fHagedornWeightParameters;
  std::vector<std::string> fYZLocalHistoNames;

  std::vector<std::vector<pair<double,double> > > fIntegrationParameters;

  EventSorting* fEventSorting;
 
  TClonesArray* fListOfMergedParticles;
  TClonesArray* fListOfMergedPairs;
  TClonesArray* fListOfSimulParticles;
  TClonesArray* fListOfSimulPairs;

  size_t fNMergedParticles;
  size_t fNMergedPairs;
  size_t fNSimulParticles;
  size_t fNSimulPairs;

  size_t fNEvents;
  size_t fNSuitableEvents;
  size_t fNConvEvents;
  size_t fNConvEvents2;

  size_t fMinBiasIndex;
  size_t fCentralityClassIndex;
 
  TH1* fRun2ExodusFluctCorrection;
  bool fCorrectRun2ExodusFluct;

  int fRUN;

  int fEffParticleCode; // pi0 = GEANTCODE_PIZERO = 7 or eta = GEANTCODE_ETA = 17

  int fVerbose;
  bool fBookedHistos ;

  double fMesonHagedornWeight;
  double fMesonMinbiasHagedornWeight;

  double fMesonpT;
  double fMesonZvertex;
  double fBbcT0;

  bool fUseFastMCDeadWarnLoss;
  double fExtraDeadWarn5x5EfficLoss; 
  double fExtraDeadWarn3x3EfficLoss; 

  bool fFixedExtractionWindows;

  bool fComputePbScEfficiency;
  bool fComputePbGlEfficiency;

  double fMinClusterEnergy ; 
  double fMinvLowCut ; 
  double fMaxAsym1 ;
  double fMaxAsym2 ;
  double fMaxChiSq1 ;
  double fMaxChiSq2 ;
  double fMaxTof1 ;
  double fMaxTof2 ;
  double fRTof ;

  double fPT_MIN ;
  double fPT_MAX ;
  size_t fPT_NBINS ;

  double fMINV_MIN ;
  double fMINV_MAX ;
  size_t fMINV_NBINS ;

  double fFIT_MINV_LO;
  double fFIT_MINV_UP;
  double fBCKGD_MINV_LO;

  double fFiduXmin ;
  double fFiduXmax ;
  double fFiduYmin ;
  double fFiduYmax ;

  //
  // general consts.
  //

  // Effect of the fiducial + warnamp + deadmap cuts in the fast MC:
  // (origin of the discrepancy not yet known, we trust more the fast MC for this ...)

  // [Fidu8_DeadWarn5x5]/[NoFiduNoDeadWarn] = 0.66 
  // [Fidu8_DeadWarn3x3]/[NoFiduNoDeadWarn] = 0.80
  // [Fidu8_DeadWarn5x5]/[Fidu8_DeadWarn3x3] = 0.82
  // 
  // [Fidu1020_DeadWarn5x5]/[NoFiduNoDeadWarn] = 0.61
  // [Fidu1020_DeadWarn5x5]/[Fidu8_DeadWarn5x5] = 0.93
  // If you multiply 0.66*0.93, you get 0.61.  So it is all consistent.

  // For the 10cm fidu and 5x5 dead-warn cuts, Saskia finds 0.61, I find 0.76
  static const double ADDITIONAL_RUN2_FIDU10cm_DEADWARN5x5_EFF_LOSS = 0.61/0.76; // extraloss = 0.80

  // For the 8cm fidu and 5x5 dead-warn cuts, Saskia finds 0.66, I find 0.81
  static const double ADDITIONAL_RUN2_FIDU8cm_DEADWARN5x5_EFF_LOSS = 0.66/0.81; // extraloss = 0.81

  // For the 8cm fidu and 3x3 dead-warn cuts, Saskia finds 0.80, I find 0.86
  static const double ADDITIONAL_RUN2_FIDU8cm_DEADWARN3x3_EFF_LOSS = 0.80/0.86; // extraloss = 0.93

  // PPG014 fiducial cuts
  static const double fFIDUCIAL_LOCAL_X_MIN =  20.; // cm
  static const double fFIDUCIAL_LOCAL_X_MAX = 377.;
  static const double fFIDUCIAL_LOCAL_Y_MIN =  10.;
  static const double fFIDUCIAL_LOCAL_Y_MAX = 189.;

  // Eta & Pi0 vs. Reaction Plane analysis fiducial limits
  static const double fFIDUCIAL_LOCAL_X_MIN2 =  8.; // cm
  static const double fFIDUCIAL_LOCAL_X_MAX2 = 389.;
  static const double fFIDUCIAL_LOCAL_Y_MIN2 =  8.;
  static const double fFIDUCIAL_LOCAL_Y_MAX2 = 191.;

  // Constants used for histogramming

  //static const double fPT_MIN =  0.0;
  //static const double fPT_MAX = 20.0;
  //static const size_t fPT_NBINS = 40;
  static const double fPT_BINWIDTH = 0.5; // GeV/c
  static const double fPT_CORR_BINWIDTH = 0.1; // GeV/c
  
  //static const double fMINV_MIN = 0.0;
  //static const double fMINV_MAX = 0.6;
  //static const size_t   fMINV_NBINS = 120;
  static const double fMINV_BINWIDTH = 0.005; // 5 MeV/c2

  //
  // ANALYSIS CUTS defined here:
  //
  // Convention is: loose cut = "1", tight cut = "2", tighter = "3" ...
  // 

  static const double fMIN_CLUSTER_ENERGY_RUN2 = 0.05 ;
  static const double fMIN_CLUSTER_ENERGY_RUN2_pi0_2ndpass = 0.1 ; 
  static const double fMIN_CLUSTER_ENERGY_RUN1 = 0.1 ; 

  static const double fMINV_LOWCUT_PI0 = 0.0425; // 42.5 MeV/c2
  static const double fMINV_LOWCUT_ETA = 0.200;  // 200 MeV/c2

  static const double fMAX_CHISQ1 = 10.0;
  static const double fMAX_CHISQ2 = 3.0;

  static const double fMAX_ASYMMETRY1 = 0.8;
  static const double fMAX_ASYMMETRY2 = 0.6;
  static const double fMAX_ASYMMETRY2_pi0_2ndpass = 0.2;
  static const double fMAX_ASYMMETRY1_eta = 0.5;
  static const double fMAX_ASYMMETRY2_eta = 0.7;

  static const double fMAX_TOF1_RUN2 = 1.2;
  static const double fMAX_TOF2_RUN2 = 0.9;
  static const double fMAX_TOF1_RUN2_pi0_2ndpass = 3.0;
  static const double fMAX_TOF2_RUN2_pi0_2ndpass = 1.2;

  static const double fMAX_TOF1_RUN1 = 1.2;

  // Constants used for Minv fits

  static const double fFIT_LOWER_BOUND_PI0 = 0.085;
  static const double fFIT_UPPER_BOUND_PI0 = 0.230;
  static const double fBCKGD_ABOVE_PI0 = 0.210;

  static const double fFIT_LOWER_BOUND_ETA = 0.450;
  static const double fFIT_UPPER_BOUND_ETA = 0.900;
  static const double fBCKGD_ABOVE_ETA = 0.750;

  static const int    fMINV_REBIN_FACTOR = 2;

#endif

};

#endif
