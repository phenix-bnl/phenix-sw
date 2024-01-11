// ----------------------------
// Created by:  Jiangyong Jia
//-----------------------------
#ifndef _PHEMBEDMCEVALUATOR_H
#define _PHEMBEDMCEVALUATOR_H

#include "phool.h"
#include "PHPointerList.h"

#include <iostream>
#include <vector>

class CrkGeometryObject;
class CrkPID;
class CrkPIDout;
class utiMatch;
class AccProj;
class PHCompositeNode;

class trkPair{
public:
  int trkG;
  int trkS,solutionS,sumfoundS;
  int trkR,solutionR,sumfoundR;
  friend std::ostream& operator<<(std::ostream& ,const trkPair&);
};

class PHEmbededEvent;
class PHEmbedMcRecoTrack;
class DchTrackCandidate;

class PHEmbedMCEvaluator{
  
public:
  PHEmbedMCEvaluator();
  virtual ~PHEmbedMCEvaluator();
  PHBoolean event(PHCompositeNode* sngl,
		  PHCompositeNode* real,
		  PHCompositeNode* merged);
  
  void LoadMatchPar();
  void setEventNum(int val) {eventnum = val;}
  int  getEventNum() {return eventnum;}
  void setVerbose(int val)  {verbose = val;}
  int  getVerbose() const {return verbose;}
  int  getEvaluationMode() const  { return mainContributorAnalysis;}
  void setEvaluationMode(const int val)     { mainContributorAnalysis=val;}

  PHBoolean associateDC();
  PHBoolean associatePC(int pc);
  PHBoolean associateTOF();
  PHBoolean associateTOFW();
  PHBoolean associateTEC();
  PHBoolean associateCRK();
  PHBoolean associateEMC();
  PHBoolean associateACC();
  PHBoolean associateMatching();
  PHBoolean fillPHEmbedMcRecoTrack();

protected:
  //DC evaluation functions
  PHBoolean  mainContributorCalculationBetweenReconstructionAndGeant();//RG
  PHBoolean  mainContributorCalculationBetweenSimulationAndGeant();//SG
  PHBoolean  mainContributorCalculationBetweenReconstructionAndSimulation();//RS
  PHBoolean  getAncestryInformationForPerfTracks();
  PHBoolean  fillEvaluationTrackList();
  PHBoolean  fillCompleteEvaluationNtupleForSelectedTracks(std::vector<trkPair>* List, int type);

  utiMatch*       mat;
  PHCompositeNode *node1;
  PHCompositeNode *node2;
  PHCompositeNode *node3;
  PHEmbededEvent* embedEvent;//embeded event tracer!
  PHPointerList<DchTrackCandidate>*  trackCandidateList;
  PHPointerList<PHEmbedMcRecoTrack>* mcRecoTrackList;
  int eventnum;
  int mainContributorAnalysis;
  int isminbias;
  int accepted;
  int verbose;
  
  /*
   * following variables and functions are private variables used to make the evaluation easier.
   */
#define MAX_numOfPerfTrack1  100
#define MAX_numOfDchTrack1   100
#define MAX_numOfDchTrack3   800
  short       mulmainContributorRG[MAX_numOfPerfTrack1];
  short      xmulmainContributorRG[MAX_numOfPerfTrack1];
  short     uvmulmainContributorRG[MAX_numOfPerfTrack1];
  short     TypemainContributorRG[MAX_numOfPerfTrack1][MAX_numOfDchTrack3];
  short  x1countmainContributorRG[MAX_numOfPerfTrack1][MAX_numOfDchTrack3];
  short  x2countmainContributorRG[MAX_numOfPerfTrack1][MAX_numOfDchTrack3];
  short  uvcountmainContributorRG[MAX_numOfPerfTrack1][MAX_numOfDchTrack3];
  float   puritymainContributorRG[MAX_numOfPerfTrack1][MAX_numOfDchTrack3];
  float  xpuritymainContributorRG[MAX_numOfPerfTrack1][MAX_numOfDchTrack3];
  float uvpuritymainContributorRG[MAX_numOfPerfTrack1][MAX_numOfDchTrack3];

  short       mulmainContributorSG[MAX_numOfPerfTrack1];
  short      xmulmainContributorSG[MAX_numOfPerfTrack1];
  short     uvmulmainContributorSG[MAX_numOfPerfTrack1];
  short      TypemainContributorSG[MAX_numOfPerfTrack1][MAX_numOfDchTrack1];
  short   x1countmainContributorSG[MAX_numOfPerfTrack1][MAX_numOfDchTrack1];
  short   x2countmainContributorSG[MAX_numOfPerfTrack1][MAX_numOfDchTrack1];
  short   uvcountmainContributorSG[MAX_numOfPerfTrack1][MAX_numOfDchTrack1];
  float    puritymainContributorSG[MAX_numOfPerfTrack1][MAX_numOfDchTrack1];
  float   xpuritymainContributorSG[MAX_numOfPerfTrack1][MAX_numOfDchTrack1];
  float  uvpuritymainContributorSG[MAX_numOfPerfTrack1][MAX_numOfDchTrack1];

  short       mulmainContributorRS[MAX_numOfDchTrack1];
  short      xmulmainContributorRS[MAX_numOfDchTrack1];
  short     uvmulmainContributorRS[MAX_numOfDchTrack1];
  short      TypemainContributorRS[MAX_numOfDchTrack1][MAX_numOfDchTrack3];
  short   x1countmainContributorRS[MAX_numOfDchTrack1][MAX_numOfDchTrack3];
  short   x2countmainContributorRS[MAX_numOfDchTrack1][MAX_numOfDchTrack3];
  short   uvcountmainContributorRS[MAX_numOfDchTrack1][MAX_numOfDchTrack3];
  float    puritymainContributorRS[MAX_numOfDchTrack1][MAX_numOfDchTrack3];
  float   xpuritymainContributorRS[MAX_numOfDchTrack1][MAX_numOfDchTrack3];
  float  uvpuritymainContributorRS[MAX_numOfDchTrack1][MAX_numOfDchTrack3];
 
 //variables holding ancestry information
  short          idGeant[MAX_numOfPerfTrack1];
  short        armside[MAX_numOfPerfTrack1];
  short           genG[MAX_numOfPerfTrack1];
  short          partidG[MAX_numOfPerfTrack1];
  short          pareidG[MAX_numOfPerfTrack1];
  short          primidG[MAX_numOfPerfTrack1];
  float          evtxG[MAX_numOfPerfTrack1];
  float          evtyG[MAX_numOfPerfTrack1];
  float          evtzG[MAX_numOfPerfTrack1];
  float           pptG[MAX_numOfPerfTrack1];
  float           ppzG[MAX_numOfPerfTrack1];
  float         pthe0G[MAX_numOfPerfTrack1];
  float         pphi0G[MAX_numOfPerfTrack1];
  float         prapiG[MAX_numOfPerfTrack1];
  float          xvtxG[MAX_numOfPerfTrack1];
  float          yvtxG[MAX_numOfPerfTrack1];
  float          zvtxG[MAX_numOfPerfTrack1];
  float          the0G[MAX_numOfPerfTrack1];
  float          phi0G[MAX_numOfPerfTrack1];

  //list which will store the tracks that we will save to ntuples
  std::vector<trkPair> tracksToSaveRG[MAX_numOfPerfTrack1];//type = 1;
  std::vector<trkPair> tracksToSaveSG[MAX_numOfPerfTrack1];//type = 2;
  //std::vector<trkPair> tracksToSaveRSG[MAX_numOfPerfTrack1];//type = 3;

  CrkPID* d_crkpid;
};
#endif
