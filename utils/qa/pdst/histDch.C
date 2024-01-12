/*
 * histDch.C
 * $Id: histDch.C,v 1.15 2009/08/24 14:46:26 phnxbld Exp $
 *
 * Book and fill Drift Chamber histograms
 */

#include "histDch.h"
#include "QADefs.h"

#include "BbcOut.h"
#include "DchTrack.h"
#include "DchHitLineTable.hh"
#include "TriggerHelper.h"
#include "getClass.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"

#include "Fun4AllHistoManager.h"
#include "Fun4AllServer.h"

#include "PreviousEventv1.h"

TH1F *dchHitDriftTimeX1W;	  // Hit drift time for West Arm
TH1F *dchHitDriftTimeX2W;
TH1F *dchHitDriftTimeUV1W;
TH1F *dchHitDriftTimeUV2W;
TH1F *dchHitDriftTimeX1E;	  // Hit drift time for East Arm
TH1F *dchHitDriftTimeX2E;
TH1F *dchHitDriftTimeUV1E;
TH1F *dchHitDriftTimeUV2E;
TH1F *dchHitWidthX1W;	          // Hit width for West Arm
TH1F *dchHitWidthX2W;
TH1F *dchHitWidthUV1W;
TH1F *dchHitWidthUV2W;
TH1F *dchHitWidthX1E;	          // Hit width for East Arm
TH1F *dchHitWidthX2E;
TH1F *dchHitWidthUV1E;
TH1F *dchHitWidthUV2E;
TH1F *dchHitCellX1W;	          // Cell distribution for West Arm
TH1F *dchHitCellX2W;
TH1F *dchHitCellUV1W;
TH1F *dchHitCellUV2W;
TH1F *dchHitCellX1E;	          // Cell distribution for East Arm
TH1F *dchHitCellX2E;
TH1F *dchHitCellUV1E;
TH1F *dchHitCellUV2E;
TH2F *dchHit[4];
//
TH1F *dchTrkNHitsE;    	  // Number of xhits on Track
TH1F *dchTrkNHitsW;
TH1F *dchGoodTrkNHitsE;
TH1F *dchGoodTrkNHitsW;
TH1F *dchTrkQualityE;     // Track Quality
TH1F *dchTrkQualityW;
TH1F *dchGoodTrkQualityE;
TH1F *dchGoodTrkQualityW;
TH1F *dchTrkPhiE;    	  // Track Phi
TH1F *dchTrkPhiW;
TH1F *dchGoodTrkPhiE;
TH1F *dchGoodTrkPhiW;
TH1F *dchTrkAlphaE;    	  // Track Alpha
TH1F *dchTrkAlphaW;
TH1F *dchGoodTrkAlphaE;
TH1F *dchGoodTrkAlphaW;
TH1F *dchTrkAlpha1E;      // Track Alpha1
TH1F *dchTrkAlpha1W;
TH1F *dchGoodTrkAlpha1E;
TH1F *dchGoodTrkAlpha1W;
TH1F *dchTrkAlpha2E;      // Track Alpha2
TH1F *dchTrkAlpha2W;
TH1F *dchGoodTrkAlpha2E;
TH1F *dchGoodTrkAlpha2W;
TH1F *dchTrkZedE;    	  // Track Zed
TH1F *dchTrkZedW;
TH1F *dchGoodTrkZedE;
TH1F *dchGoodTrkZedW;
TH1F *dchTrkBetaE;    	  // Track Beta
TH1F *dchTrkBetaW;
TH1F *dchGoodTrkBetaE;
TH1F *dchGoodTrkBetaW;
TH1F *dchTrkMomE;    	  // Track Momentum
TH1F *dchTrkMomW;
TH1F *dchGoodTrkMomE;
TH1F *dchGoodTrkMomW;
TH1F *dchTrkDistE;    	  // Track Average Distance
TH1F *dchTrkDistW;
TH1F *dchGoodTrkDistE;
TH1F *dchGoodTrkDistW;

TH1F *dchTrkMultE;     	  // Track Multiplicity
TH1F *dchTrkMultW;
TH1F *dchGoodTrkMultE;
TH1F *dchGoodTrkMultW;
TH1F *dchHitMultE;     	  // Hit Multiplicity
TH1F *dchHitMultW;

TH1F *dchTrkMult;     	  // Track Multiplicity (E+W)
TH2F *dchTrkVHitsW;   	  // West Track vs Hit Multiplicity
TH2F *dchTrkVHitsE;   	  // East Track vs Hit Multiplicity
TH2F *dchTrkWVE;      	  // West vs East Track Multiplicity
TH1F *dchTrkAlpha;    	  // Track Alpha
TH1F *dchTrkMom;      	  // Track Momentum
TH1F *dchTrkPhi;      	  // Track Phi
TH1F *dchTrkBeta;     	  // Track Beta
TH1F *dchTrkNHits;    	  // Number of xhits on Track
TH1F *dchTrkDist; 	  // Track Average Distance
TH1F *dchGoodEvent;   	  // huh?

TH3F *dchNoiseEventsEast; // store hits for DCH east arm vs. clockticks
TH3F *dchNoiseEventsWest; // store hits for DCH west arm vs. clockticks

int QADch::InitRun(PHCompositeNode *topNode)
{
  Fun4AllServer *se = Fun4AllServer::instance();

  BbcOut* bbcout = findNode::getClass<BbcOut>(topNode, "BbcOut");
  DchTrack* dchtrack = findNode::getClass<DchTrack>(topNode, "DchTrack");
  DchHitLineTable* dchhitlinetable = findNode::getClass<DchHitLineTable>(topNode, "DchHitLineTable");
  PreviousEvent* prevevt = findNode::getClass<PreviousEvent>(topNode, "PreviousEvent");

  if (!bbcout ||!dchtrack || !dchhitlinetable || !prevevt ) 
    {
      se->unregisterSubsystem(this);
      return 0;
    }

  Fun4AllHistoManager *hm = se->getHistoManager(HistoManagerName);
  if (!hm)
    {
      hm = new Fun4AllHistoManager(HistoManagerName);
      se->registerHistoManager(hm);
    }

  dchHitDriftTimeX1W  = new TH1F("dchHitDriftTimeX1W","dchHitDriftTimeX1W",100,0.,800.);
  hm->registerHisto(dchHitDriftTimeX1W);
  dchHitDriftTimeX2W  = new TH1F("dchHitDriftTimeX2W","dchHitDriftTimeX2W",100,0.,800.);
  hm->registerHisto(dchHitDriftTimeX2W);
  dchHitDriftTimeUV1W = new TH1F("dchHitDriftTimeUV1W","dchHitDriftTimeUV1W",100,0.,800.);
  hm->registerHisto(dchHitDriftTimeUV1W);
  dchHitDriftTimeUV2W = new TH1F("dchHitDriftTimeUV2W","dchHitDriftTimeUV2W",100,0.,800.);
  hm->registerHisto(dchHitDriftTimeUV2W);
  dchHitDriftTimeX1E  = new TH1F("dchHitDriftTimeX1E","dchHitDriftTimeX1E",100,0.,800.);
  hm->registerHisto(dchHitDriftTimeX1E);
  dchHitDriftTimeX2E  = new TH1F("dchHitDriftTimeX2E","dchHitDriftTimeX2E",100,0.,800.);
  hm->registerHisto(dchHitDriftTimeX2E);
  dchHitDriftTimeUV1E = new TH1F("dchHitDriftTimeUV1E","dchHitDriftTimeUV1E",100,0.,800.);
  hm->registerHisto(dchHitDriftTimeUV1E);
  dchHitDriftTimeUV2E = new TH1F("dchHitDriftTimeUV2E","dchHitDriftTimeUV2E",100,0.,800.);
  hm->registerHisto(dchHitDriftTimeUV2E);
  dchHitWidthX1W  = new TH1F("dchHitWidthX1W","dchHitWidthX1W",100,0.,300.);
  hm->registerHisto(dchHitWidthX1W);
  dchHitWidthX2W  = new TH1F("dchHitWidthX2W","dchHitWidthX2W",100,0.,300.);
  hm->registerHisto(dchHitWidthX2W);
  dchHitWidthUV1W = new TH1F("dchHitWidthUV1W","dchHitWidthUV1W",100,0.,300.);
  hm->registerHisto(dchHitWidthUV1W);
  dchHitWidthUV2W = new TH1F("dchHitWidthUV2W","dchHitWidthUV2W",100,0.,300.);
  hm->registerHisto(dchHitWidthUV2W);
  dchHitWidthX1E  = new TH1F("dchHitWidthX1E","dchHitWidthX1E",100,0.,300.);
  hm->registerHisto(dchHitWidthX1E);
  dchHitWidthX2E  = new TH1F("dchHitWidthX2E","dchHitWidthX2E",100,0.,300.);
  hm->registerHisto(dchHitWidthX2E);
  dchHitWidthUV1E = new TH1F("dchHitWidthUV1E","dchHitWidthUV1E",100,0.,300.);
  hm->registerHisto(dchHitWidthUV1E);
  dchHitWidthUV2E = new TH1F("dchHitWidthUV2E","dchHitWidthUV2E",100,0.,300.);
  hm->registerHisto(dchHitWidthUV2E);
  dchHitCellX1W  = new TH1F("dchHitCellX1W","dchHitCellX1W",80,-0.5,79.5);
  hm->registerHisto(dchHitCellX1W);
  dchHitCellX2W  = new TH1F("dchHitCellX2W","dchHitCellX2W",80,-0.5,79.5);
  hm->registerHisto(dchHitCellX2W);
  dchHitCellUV1W = new TH1F("dchHitCellUV1W","dchHitCellUV1W",80,-0.5,79.5);
  hm->registerHisto(dchHitCellUV1W);
  dchHitCellUV2W = new TH1F("dchHitCellUV2W","dchHitCellUV2W",80,-0.5,79.5);
  hm->registerHisto(dchHitCellUV2W);
  dchHitCellX1E  = new TH1F("dchHitCellX1E","dchHitCellX1E",80,-0.5,79.5);
  hm->registerHisto(dchHitCellX1E);
  dchHitCellX2E  = new TH1F("dchHitCellX2E","dchHitCellX2E",80,-0.5,79.5);
  hm->registerHisto(dchHitCellX2E);
  dchHitCellUV1E = new TH1F("dchHitCellUV1E","dchHitCellUV1E",80,-0.5,79.5);
  hm->registerHisto(dchHitCellUV1E);
  dchHitCellUV2E = new TH1F("dchHitCellUV2E","dchHitCellUV2E",80,-0.5,79.5);
  hm->registerHisto(dchHitCellUV2E);

  const char *armsidename[4] = { "ES", "EN", "WS", "WN" };
  for (int iarmside=0; iarmside<4; iarmside++)
    {
      dchHit[iarmside] = new TH2F(Form("dchHit%d",iarmside),
				  Form("DCH Hit Distribution in %s",armsidename[iarmside]),
				  80,-0.5,79.5, 40, -0.5,39.5);
      hm->registerHisto(dchHit[iarmside]);
    }

  dchTrkNHitsE     = new TH1F("dchTrkNHitsE","dchTrkNHitsE",25,0.5,25.5);
  hm->registerHisto(dchTrkNHitsE);
  dchTrkNHitsW     = new TH1F("dchTrkNHitsW","dchTrkNHitsW",25,0.5,25.5);
  hm->registerHisto(dchTrkNHitsW);
  dchGoodTrkNHitsE = new TH1F("dchGoodTrkNHitsE","dchGoodTrkNHitsE",25,0.5,25.5);
  hm->registerHisto(dchGoodTrkNHitsE);
  dchGoodTrkNHitsW = new TH1F("dchGoodTrkNHitsW","dchGoodTrkNHitsW",25,0.5,25.5);
  hm->registerHisto(dchGoodTrkNHitsW);
  dchTrkQualityE     = new TH1F("dchTrkQualityE","dchTrkQualityE",70,0.5,70.5);
  hm->registerHisto(dchTrkQualityE);
  dchTrkQualityW     = new TH1F("dchTrkQualityW","dchTrkQualityW",70,0.5,70.5);
  hm->registerHisto(dchTrkQualityW);
  dchGoodTrkQualityE = new TH1F("dchGoodTrkQualityE","dchGoodTrkQualityE",70,0.5,70.5);
  hm->registerHisto(dchGoodTrkQualityE);
  dchGoodTrkQualityW = new TH1F("dchGoodTrkQualityW","dchGoodTrkQualityW",70,0.5,70.5);
  hm->registerHisto(dchGoodTrkQualityW);
  dchTrkPhiE     = new TH1F("dchTrkPhiE","dchTrkPhiE",100,-1.,4.);
  hm->registerHisto(dchTrkPhiE);
  dchTrkPhiW     = new TH1F("dchTrkPhiW","dchTrkPhiW",100,-1.,4.);
  hm->registerHisto(dchTrkPhiW);
  dchGoodTrkPhiE = new TH1F("dchGoodTrkPhiE","dchGoodTrkPhiE",100,-1.,4.);
  hm->registerHisto(dchGoodTrkPhiE);
  dchGoodTrkPhiW = new TH1F("dchGoodTrkPhiW","dchGoodTrkPhiW",100,-1.,4.);
  hm->registerHisto(dchGoodTrkPhiW);
  dchTrkAlphaE     = new TH1F("dchTrkAlphaE","dchTrkAlphaE",100,-1.,1.);
  hm->registerHisto(dchTrkAlphaE);
  dchTrkAlphaW     = new TH1F("dchTrkAlphaW","dchTrkAlphaW",100,-1.,1.);
  hm->registerHisto(dchTrkAlphaW);
  dchGoodTrkAlphaE = new TH1F("dchGoodTrkAlphaE","dchGoodTrkAlphaE",100,-1.,1.);
  hm->registerHisto(dchGoodTrkAlphaE);
  dchGoodTrkAlphaW = new TH1F("dchGoodTrkAlphaW","dchGoodTrkAlphaW",100,-1.,1.);
  hm->registerHisto(dchGoodTrkAlphaW);
  dchTrkAlpha1E     = new TH1F("dchTrkAlpha1E","dchTrkAlpha1E",150,-3.,3.);
  hm->registerHisto(dchTrkAlpha1E);
  dchTrkAlpha1W     = new TH1F("dchTrkAlpha1W","dchTrkAlpha1W",150,-3.,3.);
  hm->registerHisto(dchTrkAlpha1W);
  dchGoodTrkAlpha1E = new TH1F("dchGoodTrkAlpha1E","dchGoodTrkAlpha1E",150,-3.,3.);
  hm->registerHisto(dchGoodTrkAlpha1E);
  dchGoodTrkAlpha1W = new TH1F("dchGoodTrkAlpha1W","dchGoodTrkAlpha1W",150,-3.,3.);
  hm->registerHisto(dchGoodTrkAlpha1W);
  dchTrkAlpha2E     = new TH1F("dchTrkAlpha2E","dchTrkAlpha2E",150,-3.,3.);
  hm->registerHisto(dchTrkAlpha2E);
  dchTrkAlpha2W     = new TH1F("dchTrkAlpha2W","dchTrkAlpha2W",150,-3.,3.);
  hm->registerHisto(dchTrkAlpha2W);
  dchGoodTrkAlpha2E = new TH1F("dchGoodTrkAlpha2E","dchGoodTrkAlpha2E",150,-3.,3.);
  hm->registerHisto(dchGoodTrkAlpha2E);
  dchGoodTrkAlpha2W = new TH1F("dchGoodTrkAlpha2W","dchGoodTrkAlpha2W",150,-3.,3.);
  hm->registerHisto(dchGoodTrkAlpha2W);
  dchTrkZedE     = new TH1F("dchTrkZedE","dchTrkZedE",100,-100.,100.);
  hm->registerHisto(dchTrkZedE);
  dchTrkZedW     = new TH1F("dchTrkZedW","dchTrkZedW",100,-100.,100.);
  hm->registerHisto(dchTrkZedW);
  dchGoodTrkZedE = new TH1F("dchGoodTrkZedE","dchGoodTrkZedE",100,-100.,100.);
  hm->registerHisto(dchGoodTrkZedE);
  dchGoodTrkZedW = new TH1F("dchGoodTrkZedW","dchGoodTrkZedW",100,-100.,100.);
  hm->registerHisto(dchGoodTrkZedW);
  dchTrkBetaE     = new TH1F("dchTrkBetaE","dchTrkBetaE",160,0.8,2.4);
  hm->registerHisto(dchTrkBetaE);
  dchTrkBetaW     = new TH1F("dchTrkBetaW","dchTrkBetaW",160,0.8,2.4);
  hm->registerHisto(dchTrkBetaW);
  dchGoodTrkBetaE = new TH1F("dchGoodTrkBetaE","dchGoodTrkBetaE",160,0.8,2.4);
  hm->registerHisto(dchGoodTrkBetaE);
  dchGoodTrkBetaW = new TH1F("dchGoodTrkBetaW","dchGoodTrkBetaW",160,0.8,2.4);
  hm->registerHisto(dchGoodTrkBetaW);
  dchTrkMomE     = new TH1F("dchTrkMomE","dchTrkMomE",100,0.,10.);
  hm->registerHisto(dchTrkMomE);
  dchTrkMomW     = new TH1F("dchTrkMomW","dchTrkMomW",100,0.,10.);
  hm->registerHisto(dchTrkMomW);
  dchGoodTrkMomE = new TH1F("dchGoodTrkMomE","dchGoodTrkMomE",100,0.,10.);
  hm->registerHisto(dchGoodTrkMomE);
  dchGoodTrkMomW = new TH1F("dchGoodTrkMomW","dchGoodTrkMomW",100,0.,10.);
  hm->registerHisto(dchGoodTrkMomW);
  dchTrkDistE     = new TH1F("dchTrkDistE","dchTrkDistE",100,0.,5.);
  hm->registerHisto(dchTrkDistE);
  dchTrkDistW     = new TH1F("dchTrkDistW","dchTrkDistW",100,0.,5.);
  hm->registerHisto(dchTrkDistW);
  dchGoodTrkDistE = new TH1F("dchGoodTrkDistE","dchGoodTrkDistE",100,0.,5.);
  hm->registerHisto(dchGoodTrkDistE);
  dchGoodTrkDistW = new TH1F("dchGoodTrkDistW","dchGoodTrkDistW",100,0.,5.);
  hm->registerHisto(dchGoodTrkDistW);

  dchTrkMultE = new TH1F("dchTrkMultE","dchTrkMultE",100,0.,400.);
  hm->registerHisto(dchTrkMultE);
  dchTrkMultW = new TH1F("dchTrkMultW","dchTrkMultW",100,0.,400.);
  hm->registerHisto(dchTrkMultW);
  dchGoodTrkMultE = new TH1F("dchGoodTrkMultE","dchGoodTrkMultE",100,0.,400.);
  hm->registerHisto(dchGoodTrkMultE);
  dchGoodTrkMultW = new TH1F("dchGoodTrkMultW","dchGoodTrkMultW",100,0.,400.);
  hm->registerHisto(dchGoodTrkMultW);
  dchHitMultE = new TH1F("dchHitMultE","dchHitMultE",1000,0.,10000.);
  hm->registerHisto(dchHitMultE);
  dchHitMultW = new TH1F("dchHitMultW","dchHitMultW",1000,0.,10000.);
  hm->registerHisto(dchHitMultW);

  dchTrkMult = new TH1F("dchTrkMult", "DCH Track Multiplicity (E+W)",
      	      	      	200, 0.0, 400.0);
  hm->registerHisto(dchTrkMult);

  dchTrkVHitsW = new TH2F("dchTrkVHitsW", "DCH West Track vs Hit Multiplicity",
      	      	      	 100, 0.0, 10000.0, 100, 0.0, 200.0);
  hm->registerHisto(dchTrkVHitsW);
  dchTrkVHitsW->SetMarkerStyle(4);
  dchTrkVHitsW->SetMarkerSize(0.2);

  dchTrkVHitsE = new TH2F("dchTrkVHitsE", "DCH East Track vs Hit Multiplicity",
      	      	      	 100, 0.0, 10000.0, 100, 0.0, 200.0);
  hm->registerHisto(dchTrkVHitsE);
  dchTrkVHitsE->SetMarkerStyle(4);
  dchTrkVHitsE->SetMarkerSize(0.2);

  dchTrkWVE = new TH2F("dchTrkWVE", "DCH West vs East Track Multiplicity",
      	      	       100, 0.0, 200.0, 100, 0.0, 200.0);
  hm->registerHisto(dchTrkWVE);
  dchTrkWVE->SetMarkerStyle(4);
  dchTrkWVE->SetMarkerSize(0.2);

  dchTrkAlpha = new TH1F("dchTrkAlpha", "DCH Track Alpha", 100, -1.0, 1.0);
  hm->registerHisto(dchTrkAlpha);
  dchTrkMom = new TH1F("dchTrkMom", "DCH Track Momentum", 200, 0.0, 10.0);
  hm->registerHisto(dchTrkMom);
  dchTrkPhi = new TH1F("dchTrkPhi", "DCH Track Phi", 200, -1.5, 4.5);
  hm->registerHisto(dchTrkPhi);
  dchTrkBeta = new TH1F("dchTrkBeta", "DCH Track Beta", 100, 0.5, 2.5);
  hm->registerHisto(dchTrkBeta);

  dchTrkNHits = new TH1F("dchTrkNHits", "DCH Number of xhits on Track",
      	      	      	 41, -0.5, 40.5);
  hm->registerHisto(dchTrkNHits);

  dchTrkDist = new TH1F("dchTrkDist", "DCH Track Average Distance",
      	      	      	 150, 0.0, 3.0);
  hm->registerHisto(dchTrkDist);
  dchGoodEvent = new TH1F("dchGoodEvent", "dchGoodEvent", 200, 0, 400);
  hm->registerHisto(dchGoodEvent);

  dchNoiseEventsEast = new TH3F("dchNoiseEventsEast","dchNoiseEventsEast",40,0.,4000.,40,0.,4000.,2000,0.,2000.);
  hm->registerHisto(dchNoiseEventsEast);
  dchNoiseEventsWest = new TH3F("dchNoiseEventsWest","dchNoiseEventsWest",40,0.,4000.,40,0.,4000.,2000,0.,2000.);
  hm->registerHisto(dchNoiseEventsWest);

  return 0;
}

int QADch::process_event(PHCompositeNode *topNode)
{

  BbcOut* bbcout = findNode::getClass<BbcOut>(topNode, "BbcOut");
  DchTrack* dchtrack = findNode::getClass<DchTrack>(topNode, "DchTrack");
  DchHitLineTable* dchhitlinetable = findNode::getClass<DchHitLineTable>(topNode, "DchHitLineTable");
  PreviousEvent* prevevt = findNode::getClass<PreviousEvent>(topNode, "PreviousEvent");
  int clockticks = prevevt->get_clockticks(0);

  if (!bbcout ||!dchtrack || !dchhitlinetable || !prevevt ) 
    {
      return 0;
    }

  TriggerHelper triggerhelper(topNode);
  if (!triggerhelper.IsEventMinBias())
    return 0;

  int trackW = 0;
  int trackE = 0;
  int goodtrackW = 0;
  int goodtrackE = 0;
  int hitW = 0;
  int hitE = 0;
  int arm, plane, cell, quality;
  float time, width;
  float phi, alpha, alpha1, alpha2, zed, beta, momentum;
  float bbcvertex = bbcout->get_VertexPoint();
  
  for (unsigned int i=0; i < dchtrack->get_DchNTrack(); i++)
  {
    int nxhits=0;
    float aveDist=0;

    // get track information

    arm      = dchtrack->get_arm(i);
    aveDist  = dchtrack->get_dist1(i) +
               dchtrack->get_dist2(i);
    aveDist /= 2.;
    nxhits   = dchtrack->get_nx1hits(i) +
               dchtrack->get_nx2hits(i);
    quality  = dchtrack->get_quality(i);
    phi      = dchtrack->get_phi(i);
    alpha    = dchtrack->get_alpha(i);
    alpha1   = dchtrack->get_alpha1(i);
    alpha2   = dchtrack->get_alpha2(i);
    zed      = dchtrack->get_zed(i);
    beta     = dchtrack->get_beta(i);
    momentum = dchtrack->get_momentum(i);

    if (arm == (int) EAST) {
      trackE++;
      dchTrkNHitsE->Fill(nxhits);
      dchTrkQualityE->Fill(quality);
      dchTrkPhiE->Fill(phi);
      dchTrkAlphaE->Fill(alpha);
      dchTrkAlpha1E->Fill(alpha1);
      dchTrkAlpha2E->Fill(alpha2);
      dchTrkZedE->Fill(zed);
      dchTrkBetaE->Fill(beta);
      dchTrkMomE->Fill(momentum);
      dchTrkDistE->Fill(aveDist);
      if ( (quality&0x3)==3 ) {
        goodtrackE++;
	dchGoodTrkNHitsE->Fill(nxhits);
        dchGoodTrkQualityE->Fill(quality);
        dchGoodTrkPhiE->Fill(phi);
        dchGoodTrkAlphaE->Fill(alpha);
        dchGoodTrkAlpha1E->Fill(alpha1);
        dchGoodTrkAlpha2E->Fill(alpha2);
        dchGoodTrkZedE->Fill(zed);
        dchGoodTrkBetaE->Fill(beta);
        dchGoodTrkMomE->Fill(momentum);
        dchGoodTrkDistE->Fill(aveDist);
      }
    }
    else {
      trackW++;
      dchTrkNHitsW->Fill(nxhits);
      dchTrkQualityW->Fill(quality);
      dchTrkPhiW->Fill(phi);
      dchTrkAlphaW->Fill(alpha);
      dchTrkAlpha1W->Fill(alpha1);
      dchTrkAlpha2W->Fill(alpha2);
      dchTrkZedW->Fill(zed);
      dchTrkBetaW->Fill(beta);
      dchTrkMomW->Fill(momentum);
      dchTrkDistW->Fill(aveDist);
      if ( (quality&0x3)==3 ) {
        goodtrackW++;
	dchGoodTrkNHitsW->Fill(nxhits);
        dchGoodTrkQualityW->Fill(quality);
        dchGoodTrkPhiW->Fill(phi);
        dchGoodTrkAlphaW->Fill(alpha);
        dchGoodTrkAlpha1W->Fill(alpha1);
        dchGoodTrkAlpha2W->Fill(alpha2);
        dchGoodTrkZedW->Fill(zed);
        dchGoodTrkBetaW->Fill(beta);
        dchGoodTrkMomW->Fill(momentum);
        dchGoodTrkDistW->Fill(aveDist);
      }
    }

    dchTrkMom->Fill(dchtrack->get_momentum(i));
    dchTrkPhi->Fill(dchtrack->get_phi(i));
    dchTrkAlpha->Fill(dchtrack->get_alpha(i));
    dchTrkBeta->Fill(dchtrack->get_beta(i));
    dchTrkNHits->Fill((float)nxhits);
    dchTrkDist->Fill(aveDist);
  } // end of loop over tracks
  
  int hits_east_lower=0;
  int hits_east_upper=0;
  int hits_west_lower=0;
  int hits_west_upper=0;

  // count hits in east and west arms
  for (int i=0; i < dchhitlinetable->Entries(); i++)
  {
    arm   = dchhitlinetable->getArm(i); 
    plane = dchhitlinetable->getPlane(i);
    cell  = dchhitlinetable->getCell(i);
    time  = dchhitlinetable->getTime1(i);
    width = dchhitlinetable->getWidth(i);

    int armside = arm*2+dchhitlinetable->getSide(i);
    dchHit[armside]->Fill(cell,plane);

    if (arm == (int) EAST)
    {
      hitE++;
      if ( plane<12) {
	dchHitDriftTimeX1E->Fill(time);
	dchHitWidthX1E->Fill(width);
	dchHitCellX1E->Fill(cell);
      }
      if ( plane>19&&plane<32 ) {
	dchHitDriftTimeX2E->Fill(time);
	dchHitWidthX2E->Fill(width);
	dchHitCellX2E->Fill(cell);
      }
      if ( plane>11&&plane<20 ) {
	dchHitDriftTimeUV1E->Fill(time);
	dchHitWidthUV1E->Fill(width);
	dchHitCellUV1E->Fill(cell);
      }
      if ( plane>31 ) {
	dchHitDriftTimeUV2E->Fill(time);
	dchHitWidthUV2E->Fill(width);
	dchHitCellUV2E->Fill(cell);
      }
      if ( cell < 40 ) hits_east_lower++;
      else hits_east_upper++;
    }
    else
    {
      hitW++;
      if ( plane<12) {
	dchHitDriftTimeX1W->Fill(time);
	dchHitWidthX1W->Fill(width);
	dchHitCellX1W->Fill(cell);
      }
      if ( plane>19&&plane<32 ) {
	dchHitDriftTimeX2W->Fill(time);
	dchHitWidthX2W->Fill(width);
	dchHitCellX2W->Fill(cell);
      }
      if ( plane>11&&plane<20 ) {
	dchHitDriftTimeUV1W->Fill(time);
	dchHitWidthUV1W->Fill(width);
	dchHitCellUV1W->Fill(cell);
      }
      if ( plane>31 ) {
	dchHitDriftTimeUV2W->Fill(time);
	dchHitWidthUV2W->Fill(width);
	dchHitCellUV2W->Fill(cell);
      }
      if ( cell < 40 ) hits_west_lower++;
      else hits_west_upper++;
    }
  }

  dchTrkMultE->Fill(trackE);
  dchTrkMultW->Fill(trackW);
  dchGoodTrkMultE->Fill(goodtrackE);
  dchGoodTrkMultW->Fill(goodtrackW);
  dchHitMultE->Fill(hitE);
  dchHitMultW->Fill(hitW);

  dchTrkMult->Fill((float)(trackW + trackE));
  dchTrkVHitsW->Fill((float)hitW, (float)trackW);
  dchTrkVHitsE->Fill((float)hitE, (float)trackE);
  dchTrkWVE->Fill((float)trackE, (float)trackW);
  
  dchNoiseEventsEast->Fill(hits_east_upper,hits_east_lower,clockticks);
  dchNoiseEventsWest->Fill(hits_west_upper,hits_west_lower,clockticks);

  if( (bbcvertex > -30) && (bbcvertex < 30))
    {
    dchGoodEvent->Fill((float)(trackW + trackE));
    }

  return 0;
}
