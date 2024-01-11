#ifndef _PHDCHHISTOGRAMMER_H
#define _PHDCHHISTOGRAMMER_H

#include <phool.h>
#include <PHPointerList.h>

#include <TH1F.h>

class dDchDCMWrapper;
class DchHitInfo;
class DchHitLineTable;
class DchMcRecoTrack;
class DchRawTable;
class DchTrack;
class DchTrackInfo;

class PHCompositeNode;

class TCanvas;
class TF1;
class TFile;
class TGraph;
class TH2F;
class TNtuple;

class PHDchHistogrammer {
private:
  char filename[40];
  TF1     *leadingRaw[2][7];
  TF1     *leadingTrack[2][7];
  TF1     *trailingRaw[2][7];
  TF1     *trailingTrack[2][7];

public:
  const char * getCVSVersion() { return "$Revision: 1.40 $ ";}
  short   flagInit;
  TCanvas *CellCanvas;
  TH1F *Side0Arm0,*Side1Arm0,*Side0Arm1,*Side1Arm1;  
  
  TFile   *Datafile;
  TH2F *onoff[2][2];

  TNtuple *raw;//  table raw
  TNtuple *hit;//  table hit
  TNtuple *track;// table track
  TNtuple *perfectTrack;
  TNtuple *trackInfo; // track - hit- raw info
  TNtuple *hitInfo;   // hit-raw-track info
  TNtuple *effi;    // efficiency and back efficiency
  TNtuple *noise;
  TNtuple *timeResidual;
  TNtuple *trackCandidate;
  TNtuple *evaluate;
  TNtuple *evaluation;
  TNtuple *completeEvaluation;
  TNtuple *embedevaluation;
  TNtuple *residual;
  TNtuple *vtxBbcCorrelation;
  TNtuple *t0s;

  TNtuple *stereoWires;  // Hemmick's stereo wire Ntuple...
  TNtuple *stereoZed;    // Stereo wire Ntuple to facilitate global alignment
  TNtuple *aligment;     // NTuple used to align the cards in the X section
  TNtuple *UValignment;  // NTuple used to align the wires (individually) in the UV -- TKH 10-28-02

  TH1F    *t0bbc;
  TH1F    *t0zdc;

  TH1F    *timeDistribution[7][2];
  TH1F    *timeDistributionTrack[2];
  TH1F    *timeDistributionScaled[7][2];
  TH1F    *timeDistributionTrackScaled[2];
  TH1F    *distDistributionScaled;
  TH1F    *timeDistanceTrack[2];
  TH1F    *timeDistance[2];

  TH1F    *timeDistributionX1;
  TH1F    *timeDistributionX2;
  TH1F    *timeDistributionU1;
  TH1F    *timeDistributionU2;
  TH1F    *timeDistributionV1;
  TH1F    *timeDistributionV2;
  TH1F    *cellDistribution[2][2][40];  
  
  TCanvas *trackCanvas;

  // Other subsystem histo 

  // Easy Efficiency vs. drift time
  TH1F *supposedEasyEffiTime;
  TH1F *trueEasyEffiTime;
  TH1F *easyTimeResidual;

  // Efficiency vs. Drift distance
  TH1F *supposedEffiAll;
  TH1F *supposedEffiX1;
  TH1F *supposedEffiU1;
  TH1F *supposedEffiV1;
  TH1F *supposedEffiX2;
  TH1F *supposedEffiU2;
  TH1F *supposedEffiV2;
  TH1F *trueEffiAll;
  TH1F *trueEffiX1;
  TH1F *trueEffiU1;
  TH1F *trueEffiV1;
  TH1F *trueEffiX2;
  TH1F *trueEffiU2;
  TH1F *trueEffiV2;
  TH1F *residualX, *residualUV;
 
  /*Efficiency vs. plane */
  TH1F* trueEffiPlane;
  TH1F* supposedEffiPlane;
  TH1F* trueEffiType;
  TH1F* supposedEffiType;
 
  TNtuple *AllData;
  TNtuple *AllHits;
  TNtuple *AllTracks;
  TNtuple *AllResiduals;
  TNtuple *AllEfficiencies;
  TNtuple *DisplayData;
  TNtuple *TrueEfficiencies;

  /*Drift-time (auxiliary) distribution graphics*/
   TCanvas *distributionX, *distributionU, *distributionV, *AllTimes;
   TCanvas *distributionXAUX, *distributionUAUX, *distributionVAUX;
   TH1F *TimeDistribX1, *TimeDistribX2, *TimeDistribU1, *TimeDistribU2;
   TH1F *TimeDistribV1, *TimeDistribV2;
   TH1F *TimeDistribX1AUX, *TimeDistribX2AUX, *TimeDistribU1AUX;
   TH1F *TimeDistribU2AUX, *TimeDistribV1AUX, *TimeDistribV2AUX;
   TH1F *timeDistrib[6];
   TH1F *referenceTime;
  /*Grphics for resolution algorithm*/
   TCanvas *ResolutionDisplay;
   TH1F *residualX1, *residualX2, *residualU1, *residualU2;
   TH1F *residualV1, *residualV2;
  
  /*spatial reference distribution graphics*/
   TCanvas *RefCanv;
   TH1F *NofX_X1, *NofX_X2, *NofX_U1, *NofX_U2, *NofX_V1, *NofX_V2;
     
  /*Graphics for mapping of spatial and time distribtions*/
   TCanvas *Space2TimeX, *Space2TimeU, *Space2TimeV; 
   TGraph *SDTRX1, *SDTRX2, *SDTRV1, *SDTRV2, *SDTRU1, *SDTRU2; 
   
   TCanvas *RefChannelDisplay;
   TH1F *RefChanDistribution;

   TCanvas *houghCanvas;
   TH2F *houghArray;
   /*XT-relation function and gauss function prototype*/
   TF1 *myfit;
   TF1 *Gaussian;
   
  /* Histograms related to the drift chamber vertex fit */
  TH1F *nHist1, *xHist1, *yHist1, *zHist1, *rHist1, *hHist1;
  TH1F *h1Hist1, *gdistHist1, *ghHist1, *bdistHist1, *bhHist1;
  TH1F *distrHist1, *distxHist1, *distyHist1, *distzHist1;
  TH1F *accdistrHist1, *accdistxHist1, *accdistyHist1, *accdistzHist1;
  TH2F *accdistxyHist2, *distxyHist2; 
  TH1F *dvtxHist1, *xvtxHist1, *yvtxHist1, *zvtxHist1;
  TH1F *rvtxHist1, *ngvtxHist1, *nbvtxHist1, *ngeogvtxHist1;
  TH1F *ngeobvtxHist1, *ntvtxHist1, *nfinalHist1;
  TH1F *dxHist1, *dyHist1, *dzHist1;
  TH2F *dxyHist2, *xyvtxHist2;

   /*staf-table wrappers*/
  dDchDCMWrapper *myDCM;
  DchRawTable    *rawTable;
  DchHitLineTable *hitLineTable;
  DchTrack  *trackTable;
  DchTrack  *perfTrackTable;

    /*global variabls*/
  int EventRunNumber;
  int globalEvent;
  int refchannelcounter;
  int NumberOfTimeBins;
  float PropRegWidth;
  float TimeBinSize;
  long reftime;
  int EventCounter;
  int TrackCounter;

  PHCompositeNode *TopNode;

public:
   PHDchHistogrammer();
  virtual ~PHDchHistogrammer() {}
    /*demands filename and title for the output file*/
  
  int initializeDefault();
  int initCanvas();

  PHBoolean fillEvaluate(float,float,float,float, float,float,float,float,float,float);
  PHBoolean fillEvaluation(float*);
  PHBoolean fillCompleteEvaluation(DchMcRecoTrack*);
  PHBoolean fillEmbedEvaluation(float*);
  PHBoolean fillReferenceTime(int time);
  PHBoolean fillStereoWires(PHCompositeNode*);
  PHBoolean fillStereoZed(PHCompositeNode*);
  PHBoolean fillUValignment(PHCompositeNode*);

  PHBoolean fillRawData(PHCompositeNode*);
  PHBoolean fillHits(PHCompositeNode*);
  PHBoolean fillTracks(PHCompositeNode*);
  
  PHBoolean fillTrackInfo(const PHPointerList<DchTrackInfo>* );
  PHBoolean fillHitInfo(const PHPointerList<DchHitInfo>* );

   PHBoolean fillEfficiency(int* eYes, int* eAll, int* bYes, int* bAll);
   PHBoolean fillNoise(int, int, int, int,float,int);
   PHBoolean fillTimeResidual(float, int , int , int , int , int ,int );
  PHBoolean fillTimeDistribution(float*, float*, float*, float*, float*, float*,float*, float*, float*) {return False;}
   PHBoolean fillRawDistributions(PHCompositeNode*);
   PHBoolean fillTimeDistanceCorrelation(float*, float*,float*, float*);
   PHBoolean fillTracksAndCandidates(PHCompositeNode*);
   PHBoolean fillResiduals(float*);
   PHBoolean fillVertexBbcCorrelation(float bbcT0,float bbcVtx, float zdcVtx, float vtx);
   PHBoolean fillT0s(float bbcT0, float zdcT0, float zdcT1, float zdcT2);
  
  TH1F* getTimeDistanceHistogram(short val) { return (timeDistance[val]);}
  TH1F* getTimeDistanceTrackHistogram(short val) {return (timeDistanceTrack[val]);}
  TH1F* getTimeDistributionScaled(short val,short mod) { return (timeDistributionScaled[mod][val]);}
  TH1F* getTimeDistributionTrackScaled(short val) {return (timeDistributionTrackScaled[val]);}
  TH1F* getTimeDistribution(short val,short mod) { return (timeDistribution[mod][val]);}
  TH1F* getTimeDistributionTrack(short val) {return (timeDistributionTrack[val]);}
  
  TNtuple*  getHitNtuple() {return hit;}
  TNtuple*  getTrackNtuple() {return track;} 
  TNtuple*  getTrackInfoNtuple() {return trackInfo;} 
  TNtuple*  getRawNtuple() {return raw;} 

  TNtuple*  getStereoWires() {return stereoWires;}
  TNtuple*  getStereoZed()   {return stereoZed;}

  //the methods for the easy efficiency
  void fillSupposedEasyEffiTime(float time){supposedEasyEffiTime->Fill(time); }
  void fillTrueEasyEffiTime(float time){trueEasyEffiTime->Fill(time); }
  void  fillEasyTimeResidual(float time){ easyTimeResidual->Fill(time);}

  //the methods for the efficiency ~ distance plots
  void  fillSupposedEffiAll(float dist) { supposedEffiAll->Fill(dist); }
  void  fillSupposedEffiX1(float dist) { supposedEffiX1->Fill(dist); }
  void  fillSupposedEffiU1(float dist) { supposedEffiU1->Fill(dist); }
  void  fillSupposedEffiV1(float dist) { supposedEffiV1->Fill(dist); }
  void  fillSupposedEffiX2(float dist) { supposedEffiX2->Fill(dist); }
  void  fillSupposedEffiU2(float dist) { supposedEffiU2->Fill(dist); }
  void  fillSupposedEffiV2(float dist) { supposedEffiV2->Fill(dist); }
  void  fillTrueEffiAll(float dist) {trueEffiAll->Fill(dist);}
  void  fillTrueEffiX1(float dist) {trueEffiX1->Fill(dist);}
  void  fillTrueEffiU1(float dist) {trueEffiU1->Fill(dist);}
  void  fillTrueEffiV1(float dist) {trueEffiV1->Fill(dist);}
  void  fillTrueEffiX2(float dist) {trueEffiX2->Fill(dist);}
  void  fillTrueEffiU2(float dist) {trueEffiU2->Fill(dist);}
  void  fillTrueEffiV2(float dist) {trueEffiV2->Fill(dist);}
  void  fillResidualX(float res) {residualX->Fill(res); }
  void  fillResidualUV(float res) {residualUV->Fill(res);}

  //the methods for efficiency vs. plane
  void  fillSupposedEffiPlane(int plane ) ;
  void  fillTrueEffiPlane(int);

   TF1*  getLeadingRaw(short arm, short mod) { return leadingRaw[arm][mod];}
   TF1*  getTrailingRaw(short arm, short mod) { return trailingRaw[arm][mod];}
   TF1*  getLeadingTrack(short arm, short mod) { return leadingTrack[arm][mod];}
   TF1*  getTrailingTrack(short arm, short mod) { return trailingTrack[arm][mod];}
   void  setLeadingRaw(short arm,short mod,TF1* h) { leadingRaw[arm][mod] = h;}
   void  setTrailingRaw(short arm,short mod,TF1* h) { trailingRaw[arm][mod] = h;}
   void  setLeadingTrack(short arm,short mod, TF1* h) { leadingTrack[arm][mod] = h;}
   void  setTrailingTrack(short arm,short mod,TF1* h) { trailingTrack[arm][mod]= h;}
 
  int fillHoughArray();
  int drawXTHist(int flag);   
  int drawHoughArray();
  int refresh();
  void drawTrackInfo();
  
  int initializeFile(int run = 0);
  int saveToFile(int option);
  int initCellCanvas();
  int fillCellCanvas(int arm,int side,int plane,int cell);
  int initializeVertexFit();
  
};
#endif /* _PHDCHHISTOGRAMMER_H */














