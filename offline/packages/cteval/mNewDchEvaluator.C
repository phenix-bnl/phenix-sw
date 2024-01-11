// $Id: mNewDchEvaluator.C,v 1.11 2017/07/13 18:53:08 phnxbld Exp $
/*!
  \file mNewDchEvaluator.C
  \author Federica Messer
  \version $Revision: 1.11 $
  \date $Date: 2017/07/13 18:53:08 $
*/
#include <PHDchHistogrammer.hh>
#include <PHDchGeometryObject.h>
#include <PHDchAddressObject.h>
#include <PHDchCalibrationObject.h>
#include <PHDchNoiseObject.h>
#include <DchAnaPar.h>
#include <tofghitWrapper.h>
#include <dTofGhitGdigiWrapper.h>
#include <dTofGdigiWrapper.h>
#include <dTofGdigiRecWrapper.h>

#include <dcghitWrapper.h>
#include <dDchEvalParWrapper.h>
#include <dDchHit.h>
#include <dDchHitWrapper.h>
#include <dDchGhitHitsWrapper.h>
#include <dDchTracks.h>
#include <dDchTracksWrapper.h>
#include <fkinWrapper.h>

#include <dPHTrackWrapper.h>
#include <VtxOut.h>
#include <BbcOut.h>
#include <dCglTrackWrapper.h>
#include <dPadClusterWrapper.h>
#include <pcghitWrapper.h>
#include <dPadGhitClusWrapper.h>
#include <dEmcClusterLocalExtWrapper.h>
#include <dEmcGeaClusterTrackWrapper.h>
#include <dCrkHitWrapper.h>
#include <dTecTrackWrapper.h>
#include <dTofReconstructedWrapper.h>
#include <dDchTracksExtWrapper.h>

#include <table_header.h>

#include <dio_trk.hh>
#include <PHNode.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHNodeIterator.h>
#include <PHTable.hh>
#include <mNewDchEvaluator.hh>
#include <PHTimeStamp.h>
#include <PdbBankID.hh>
#include <PHIODataNode.h>

#include <iostream>
using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;
typedef PHIODataNode<dcghitWrapper> dcghitNode_t;
typedef PHIODataNode<dDchEvalParWrapper> dDchEvalParNode_t;
typedef PHIODataNode<dDchHitWrapper> dDchHitNode_t;
typedef PHIODataNode<dDchGhitHitsWrapper> dDchGhitHitsNode_t;
typedef PHIODataNode<dDchTracksWrapper> dDchTracksNode_t;
typedef PHIODataNode<fkinWrapper> fkinNode_t;
typedef PHIODataNode<BbcOut> BbcOutNode_t;
typedef PHIODataNode<VtxOut> VtxOutNode_t;

//______________________________________________________
mNewDchEvaluator::mNewDchEvaluator() :
   verbose(0),
   dchGeometryObject(0),
   dchAddressObject(0),
   dchCalibrationObject(0),
   dchNoiseObject(0),
   trackCandidateList(0),
   mcRecoTrackList(0),
   dchHistogrammer(0),
   d_crkpid(-1)
{
 LoadMatchPar();
}

//______________________________________________________
mNewDchEvaluator::~mNewDchEvaluator()
{}

//______________________________________________________
PHBoolean mNewDchEvaluator::event(PHCompositeNode *root) 
{

  PHPointerList<PHNode> nodes;
  PHNodeIterator i(root);
  PHNode *n;
  
  PHDataNode<PHDchAddressObject>* dchDaoNode;
  PHDataNode<PHDchGeometryObject>* dchDgoNode;
  PHDataNode<PHDchCalibrationObject>* dchDcoNode;
  PHDataNode<PHDchNoiseObject>* dchDnoNode;
  PHDataNode<PHPointerList<DchTrackCandidate> >* tmpTrackCandiNode;
  PHDataNode<PHPointerList<DchMcRecoTrack> >* tmpDchMcRecoTrackNode;
  nodes.clear();
  
  // node for temporary storage
  PHCompositeNode* tmpNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "DCHTMP"));
  if (!tmpNode) {
    tmpNode = new PHCompositeNode("DCHTMP");
    root->addNode(tmpNode);
  }

  // look for DchHistogram
  PHDataNode<PHDchHistogrammer>* tmpHistoNode;
  tmpHistoNode = (PHDataNode<PHDchHistogrammer>*)i.findFirst("PHDataNode","DchHisto");
  if( tmpHistoNode) dchHistogrammer = tmpHistoNode->getData();
  else {
  
    // create one if node found.
    // add it to the node tree so that it gets deleted when the tree is cleared
    if (!dchHistogrammer) 
    {
      cout << "Introducing the Histogrammer "<< endl;
      dchHistogrammer = new PHDchHistogrammer();
      dchHistogrammer->initializeFile(1);
      tmpHistoNode = new PHDataNode<PHDchHistogrammer>(dchHistogrammer,"DchHisto");
      tmpNode->addNode( tmpHistoNode );
      cout << "done ..."<< endl;
    }
  }
  
  // extract the candidate Node
  tmpTrackCandiNode = (PHDataNode<PHPointerList<DchTrackCandidate> >*)i.findFirst("PHDataNode","DchTrackCandidate");
  if (tmpTrackCandiNode) {
    trackCandidateList = tmpTrackCandiNode->getData();
  }
  
  tmpDchMcRecoTrackNode = (PHDataNode<PHPointerList<DchMcRecoTrack> >*)i.findFirst("PHDataNode","DchMcRecoTrack");
  if (!tmpDchMcRecoTrackNode) {
    mcRecoTrackList = new PHPointerList<DchMcRecoTrack>(100);
    tmpDchMcRecoTrackNode = new PHDataNode<PHPointerList<DchMcRecoTrack> >(mcRecoTrackList,"DchMcRecoTrack");
    tmpNode->addNode(tmpDchMcRecoTrackNode);
  }else{
    mcRecoTrackList = tmpDchMcRecoTrackNode->getData();
    mcRecoTrackList->clearAndDestroy();
  }

  // extract from the node the DAO -DGO -DCO etc...
  
  dchDaoNode = (PHDataNode<PHDchAddressObject>*)i.findFirst("PHDataNode","DchDAO");
  if (dchDaoNode) 
    dchAddressObject = dchDaoNode->getData();
    
  dchDgoNode = (PHDataNode<PHDchGeometryObject>*)i.findFirst("PHDataNode","DchDGO");
  if (dchDgoNode) 
    dchGeometryObject = dchDgoNode->getData();
   
  dchDcoNode = (PHDataNode<PHDchCalibrationObject>*)i.findFirst("PHDataNode","DchDCO");
  if (dchDcoNode) 
    dchCalibrationObject = dchDcoNode->getData();
  
  dchDnoNode = (PHDataNode<PHDchNoiseObject>*)i.findFirst("PHDataNode","DchDNO"); 
  if (dchDnoNode) 
    dchNoiseObject = dchDnoNode->getData();

  // Insert code here to navigate node hierarchy and find
  // or create specific nodes to pass to physics module...

  n = i.findFirst("PHIODataNode", "dDchEvalPar");
  if (!n) cout << "ERROR:  'in' parameter dDchEvalPar not found" << endl; // to be eliminated
  nodes.append(n);
  
  n = i.findFirst("PHIODataNode", "dDchHit");
  if (!n) cout << "ERROR:  'in' parameter dDchHit not found" << endl; // to be eliminated
  nodes.append(n);

  n = i.findFirst("PHIODataNode", "dDchGhitHits");
  if (!n) cout << "ERROR:  'in' parameter dDchGhitHits not found" << endl; // to be eliminated
  nodes.append(n);

  n = i.findFirst("PHIODataNode", "dcghit");
  if (!n) cout << "ERROR:  'in' parameter dcghit not found" << endl; // to be eliminated
  nodes.append(n);

  n = i.findFirst("PHIODataNode", "dDchTracks");
  if (!n) cout << "ERROR:  'in' parameter dDchTracks not found" << endl; // to be eliminated
  nodes.append(n);

  n = i.findFirst("PHIODataNode", "dDchTracksPerf");
  if (!n) cout << "ERROR:  'in' parameter dDchTracksPerf not found" <<endl; // to be eliminated
  nodes.append(n);

  n = i.findFirst("PHIODataNode", "fkin");
  if (!n) cout << "ERROR:  'in' parameter fkin not found" << endl; // to be eliminated
  nodes.append(n);
  
  return  callPAM(nodes);//
 
}

PHBoolean mNewDchEvaluator::callPAM(PHPointerList<PHNode> &nl)
{

  // compares between perfect tracking and reconstruction
  
  TABLE_HEAD_ST   dDchEvalPar_h;
  DDCHEVALPAR_ST *dDchEvalPar;
  TABLE_HEAD_ST   dDchHit_h;
  DDCHHIT_ST     *dDchHit;
  TABLE_HEAD_ST    dDchGhitHits_h;
  TABLE_HEAD_ST   dcghit_h;
  DCGHIT_ST      *dcghit;
  TABLE_HEAD_ST    dDchTracks_h;
  DDCHTRACKS_ST   *dDchTracks;
  TABLE_HEAD_ST    dDchTracksPerf_h;
  DDCHTRACKS_ST   *dDchTracksPerf;
  TABLE_HEAD_ST    fkin_h;
  FKIN_ST         *fkin;
 
  dDchEvalParNode_t*  n1 = static_cast<dDchEvalParNode_t*>(nl[0]);
  dDchHitNode_t*      n2 = static_cast<dDchHitNode_t*>(nl[1]);
  dDchGhitHitsNode_t* n3 = static_cast<dDchGhitHitsNode_t*>(nl[2]);
  dcghitNode_t*       n4 = static_cast<dcghitNode_t*>(nl[3]);
  dDchTracksNode_t*   n5 = static_cast<dDchTracksNode_t*>(nl[4]);
  dDchTracksNode_t*   n6 = static_cast<dDchTracksNode_t*>(nl[5]);
  fkinNode_t*         n7  = static_cast<fkinNode_t*>(nl[6]);

  dDchEvalPar_h    = n1->getData()->TableHeader();
  dDchEvalPar      = n1->getData()->TableData();
  dDchHit_h        = n2->getData()->TableHeader();
  dDchHit          = n2->getData()->TableData();
  dDchGhitHits_h   = n3->getData()->TableHeader();
  dcghit_h         = n4->getData()->TableHeader();
  dcghit           = n4->getData()->TableData();
  dDchTracks_h     = n5->getData()->TableHeader();
  dDchTracks       = n5->getData()->TableData();
  dDchTracksPerf_h = n6->getData()->TableHeader();
  dDchTracksPerf   = n6->getData()->TableData();
  fkin_h           = n7->getData()->TableHeader();
  fkin             = n7->getData()->TableData();

  verbose = 1;//dDchEvalPar[0].verbose;

  int possibleSolution;

  const unsigned int ARRAY_SIZE = 55;
  float array[ARRAY_SIZE];

  static int EventCounter = 0;

  int mainContributorAnalysis     = dDchEvalPar[0].main; // do or not do the main contributor analysis
  int numberOfPerfectTracks       = dDchTracksPerf_h.nok;
  int numberOfReconstructedTracks = dDchTracks_h.nok;

  EventCounter++;
   if (verbose)
  {
    cout << "main contributor analysis: " << mainContributorAnalysis << endl;
    cout << "event counter: " << EventCounter << endl;
    cout << "perfect tracks:       " << numberOfPerfectTracks << endl;
    cout << "reconstructed tracks: " << numberOfReconstructedTracks << endl;
  }

  // perform the main contributor analysis for a reconstructed track
  int mulmain, mulxmain, muluvmain, ambmain, ambxmain, ambuvmain;
  int main, xmain, uvmain, xhits, uvhits;
  float puremain, purexmain, pureuvmain;
  int                     xHitsRecoTrack[numberOfReconstructedTracks];
  int                    uvHitsRecoTrack[numberOfReconstructedTracks];
  int        MulmainContributorRecoTrack[numberOfReconstructedTracks];
  int       MulxmainContributorRecoTrack[numberOfReconstructedTracks];
  int      MuluvmainContributorRecoTrack[numberOfReconstructedTracks];
  int        AmbmainContributorRecoTrack[numberOfReconstructedTracks];
  int       AmbxmainContributorRecoTrack[numberOfReconstructedTracks];
  int      AmbuvmainContributorRecoTrack[numberOfReconstructedTracks];
  int           mainContributorRecoTrack[numberOfReconstructedTracks];
  int          xmainContributorRecoTrack[numberOfReconstructedTracks];
  int         uvmainContributorRecoTrack[numberOfReconstructedTracks];
  float   PuritymainContributorRecoTrack[numberOfReconstructedTracks];
  float  PurityxmainContributorRecoTrack[numberOfReconstructedTracks];
  float PurityuvmainContributorRecoTrack[numberOfReconstructedTracks];

  for (int m = 0; m<numberOfReconstructedTracks; m++) {
    mainContributorCalculation(nl,m,xhits,uvhits,
			       mulmain,mulxmain,muluvmain,
			       ambmain,ambxmain,ambuvmain,
			       main,xmain,uvmain,
			       puremain,purexmain,pureuvmain);
                      xHitsRecoTrack[m] = xhits;
                     uvHitsRecoTrack[m] = uvhits;
         MulmainContributorRecoTrack[m] = mulmain;
        MulxmainContributorRecoTrack[m] = mulxmain;
       MuluvmainContributorRecoTrack[m] = muluvmain;
         AmbmainContributorRecoTrack[m] = ambmain;
        AmbxmainContributorRecoTrack[m] = ambxmain;
       AmbuvmainContributorRecoTrack[m] = ambuvmain;
            mainContributorRecoTrack[m] = main;
           xmainContributorRecoTrack[m] = xmain;
          uvmainContributorRecoTrack[m] = uvmain;
      PuritymainContributorRecoTrack[m] = puremain;
     PurityxmainContributorRecoTrack[m] = purexmain;
    PurityuvmainContributorRecoTrack[m] = pureuvmain;
  }

  int   itparent, idparent, idpart;
  float ptheta, pphi, r_vertex, z_vertex, phi_vertex;

  float xEventVTX, yEventVTX, zEventVTX;
  float momentumG, momentumR;
  float generation, rVTX, zVTX;
  int   particleID, parentID, primaryID;
  int   qualityG, qualityR;
  float xG, yG, zG;
  float xR, yR, zR;
  float dxG, dyG;
  float dxR, dyR;
  float p1x, p1y;
  float p2x, p2y;
  float slope, intersect, ClosestApproachToBeamAxisG, ClosestApproachToBeamAxisR;
  float Chi2OfFitR;
  int   NumberOfFittedHitsR;
  int   armG, armR;
  float alphaG, alphaR;
  float betaG, betaR;
  float thetaG, phiG;
  float theta0G, phi0G;
  float theta0R, phi0R;
  float phiR;
  float zedG, zedR;
  float dalphaMin, dphiMin;
  
  float deltaZed;   // reconstructed - Geant
  float deltaAlpha; // reconstructed - Geant
  float deltaBeta;  // reconstructed - Geant
  float deltaPhi;   // reconstructed - Geant
  float deltaDistance;

  int noSolution[20];
  int perfectTracks[20];
  int candidatelength = trackCandidateList->length();

  for (int j=0; j<20; ++j) {
    noSolution[j]    = 0;
    perfectTracks[j] = 0;
  }
  DchMcRecoTrack* mcRecoTrack;

  for (int i=0; i< numberOfPerfectTracks; ++i) {
    for (unsigned int k=0; k < ARRAY_SIZE; ++k) {
      array[k] = -99.;
    }

    int idGeantTrack = 0;
    int fkinIndex = 0;

    // find the mctrack id which correspond to this track
    for (int k=0; k<numberOfPlanes; ++k) {
      int dcghitID = dDchTracksPerf[i].hits[k];
      if (dcghitID != -1) {
	idGeantTrack = dcghit[dcghitID].mctrack; // for main contributor Analysis
	break;
      }
    }

    for (int k=0; k<fkin_h.nok; ++k) {
      if ( fkin[k].true_track == idGeantTrack ) {
	fkinIndex = k;
	break;
      }
    }

    momentumG = dDchTracksPerf[i].momentum;
    qualityG  = dDchTracksPerf[i].quality;
    alphaG    = dDchTracksPerf[i].alpha;
    betaG     = dDchTracksPerf[i].beta;
    phiG      = dDchTracksPerf[i].phi;
    zedG      = dDchTracksPerf[i].zed;
    xG        = dDchTracksPerf[i].point[0];
    yG        = dDchTracksPerf[i].point[1];
    zG        = dDchTracksPerf[i].point[2];
    dxG       = dDchTracksPerf[i].direction[0];
    dyG       = dDchTracksPerf[i].direction[1];
    thetaG    = acos(zG/sqrt(xG*xG+yG*yG+zG*zG));

    if(qualityG==0)   (perfectTracks[0])++;
    if(qualityG&0x01) (perfectTracks[1])++;
    if(qualityG&0x02) (perfectTracks[2])++;
    if(qualityG&0x04) (perfectTracks[3])++;
    if(qualityG&0x08) (perfectTracks[4])++;

    if (xG>0) {
      armG = WEST;
    }else {
      armG = EAST;
    }

    p1x = xG;
    p1y = yG;
    p2x = p1x + dxG;
    p2y = p1y + dyG;
    if ( p2x!=p1x ) slope = (p2y-p1y)/(p2x-p1x); else slope = 0.0;
    intersect = p1y-slope*p1x;
    ClosestApproachToBeamAxisG = fabs(intersect/sqrt(1+slope*slope));

    // analyze the ancestry information related to idGeantTrack

    // analyze the ancestry information related to idGeantTrack
    //
    // unfortunately one can't use dio function starting from DST,
    // because those function will attempt to find
    // pisa global variables which doesn't exist here!!!!!!!!!
    //
    
    ptheta    = fkin[fkinIndex].pthet;
    pphi      = fkin[fkinIndex].pphi;
    idparent  = fkin[fkinIndex].idparent;
    idpart    = fkin[fkinIndex].idpart;
    r_vertex  = fkin[fkinIndex].r_vertex;
    z_vertex  = fkin[fkinIndex].z_vertex;
    phi_vertex= fkin[fkinIndex].ph_vertx;
    itparent  = fkin[fkinIndex].itparent;

    theta0G = Pi*ptheta/180.;
    phi0G   = Pi*pphi/180.;
    if (phi0G<-Pi/2.) phi0G+=TwoPi;

    if (idparent == 0) {
      generation = 1.;
    }
    else {
      if (idparent > 0) generation = 2.;
      else generation = 3.;
    }
    rVTX = r_vertex;
    zVTX = z_vertex;
    particleID = idpart;
    if (generation < 3.) {
      parentID = idparent;
    }    else {
      parentID = -idparent;
    }

    if(idGeantTrack!=-itparent){
      xEventVTX = -9999;
      yEventVTX = -9999;
      zEventVTX = -9999;
      primaryID = -9999;
    }else{
      xEventVTX = r_vertex*cos(Pi*phi_vertex/180.); 
      yEventVTX = r_vertex*sin(Pi*phi_vertex/180.); 
      zEventVTX = z_vertex;
      primaryID = idparent;
    }
 
    if (dchHistogrammer) {
	array[0]  = (float)EventCounter;
	array[1]  = (float)xEventVTX;
	array[2]  = (float)yEventVTX;
	array[3]  = (float)zEventVTX;
	array[4]  = (float)i;
	array[5]  = (float)qualityG;
	array[6]  = (float)momentumG;
	array[7]  = (float)thetaG;
	array[8]  = (float)phiG;
	array[9]  = (float)theta0G;
	array[10] = (float)phi0G;
	array[11] = (float)alphaG;
	array[12] = (float)betaG;
	array[13] = (float)zedG;
       	array[14] = (float)generation;
	array[15] = (float)particleID;
	array[16] = (float)parentID;
	array[17] = (float)primaryID;
	array[18] = (float)rVTX;
	array[19] = (float)zVTX;
	array[20] = (float)phi_vertex;
	array[53] = (float)idGeantTrack;
    }

    int numberOfReconstructedTracksWithCorrectMainContributor = 0;
    for (int k=0; k < numberOfReconstructedTracks; ++ k) {
      if (idGeantTrack == mainContributorRecoTrack[k])
	numberOfReconstructedTracksWithCorrectMainContributor++;
    }

    if (dchHistogrammer) {
      array[44] = (float)numberOfReconstructedTracksWithCorrectMainContributor;
    }

    possibleSolution = 0;
    for (int k=0; k < numberOfReconstructedTracks; ++k) {
      momentumR = dDchTracks[k].momentum;
      qualityR  = dDchTracks[k].quality;
      alphaR    = dDchTracks[k].alpha;
      betaR     = dDchTracks[k].beta;
      phiR      = dDchTracks[k].phi;
      zedR      = dDchTracks[k].zed;
      theta0R   = dDchTracks[k].theta0;
      phi0R     = dDchTracks[k].phi0;
      xR        = dDchTracks[k].point[0];
      yR        = dDchTracks[k].point[1];
      zR        = dDchTracks[k].point[2];
      dxR       = dDchTracks[k].direction[0];
      dyR       = dDchTracks[k].direction[1];
      if (xR>0) {
	armR = WEST;
      }else {
	armR = EAST;
      }

      p1x = xR;
      p1y = yR;
      p2x = p1x + dxR;
      p2y = p1y + dyR;
      if ( p2x!=p1x ) slope = (p2y-p1y)/(p2x-p1x); else slope = 0.0;
      intersect = p1y-slope*p1x;
      ClosestApproachToBeamAxisR = fabs(intersect/sqrt(1+slope*slope));

      int numHits = 0;
      float averageDistance = 0.;
      float thisHitsDist = 0.;
      for (int n=0; n<39; n++) {
        int hid = dDchTracks[k].hits[n];
        if ( hid!=-1 ) {
          int planeOfHit = dDchHit[hid].plane;
          short type = wireType[planeOfHit];
          if (type == X1Wire || type == X2Wire)
          {
	    numHits++;
	    thisHitsDist = dDchHit[hid].distance;
	    averageDistance += thisHitsDist;
	  }
        }
      }
      if ( numHits>0 ) {
        averageDistance = averageDistance/(float)numHits;
      }
      else {
	averageDistance = 99999.;
      }

      if (k<candidatelength) {
	NumberOfFittedHitsR = (*trackCandidateList)[k]->getNumberOfFittedHits();
	Chi2OfFitR          = (*trackCandidateList)[k]->getChi2OfFit();
      }else {
	NumberOfFittedHitsR = -9999;
	Chi2OfFitR          = -9999;
      }
      if (armG != armR) continue;
      deltaDistance = sqrt((xG-xR)*(xG-xR) + (yG-yR)*(yG-yR) +(zG-zR)*(zG-zR));
      deltaZed = (zedR - zedG);
      deltaPhi = (float)(phiR - phiG);
      deltaAlpha = (float)(alphaR- alphaG);
      deltaBeta  = (float)(betaR - betaG);

      dalphaMin = 99999.;
      dphiMin   = 99999.;
      for (int l=0; l < numberOfReconstructedTracks; ++l) {
        if ( l==k ) continue;
        if (fabs(phiR-dDchTracks[l].phi)<dphiMin)
          dphiMin = fabs(phiR-dDchTracks[l].phi);
        if (fabs(alphaR-dDchTracks[l].alpha)<dalphaMin)
          dalphaMin = fabs(alphaR-dDchTracks[l].alpha);
      }

      if (dchHistogrammer) {
	array[21] = (float)k;
	array[22] = (float)qualityR;
	array[23] = (float)momentumR;
	array[24] = (float)theta0R;
	array[25] = (float)phi0R;
	array[26] = (float)xHitsRecoTrack[k];
	array[27] = (float)uvHitsRecoTrack[k];
	array[28] = (float)MulmainContributorRecoTrack[k];
	array[29] = (float)MulxmainContributorRecoTrack[k];
	array[30] = (float)MuluvmainContributorRecoTrack[k];
	array[31] = (float)mainContributorRecoTrack[k];
	array[32] = (float)xmainContributorRecoTrack[k];
	array[33] = (float)uvmainContributorRecoTrack[k];
	array[34] = (float)AmbmainContributorRecoTrack[k];
	array[35] = (float)AmbxmainContributorRecoTrack[k];
	array[36] = (float)AmbuvmainContributorRecoTrack[k];
	array[37] = (float)PuritymainContributorRecoTrack[k];
	array[38] = (float)PurityxmainContributorRecoTrack[k];
	array[39] = (float)PurityuvmainContributorRecoTrack[k];
	array[40] = (float)deltaAlpha;
	array[41] = (float)deltaBeta;
	array[42] = (float)deltaPhi;
	array[43] = (float)deltaZed;
	array[44] = (float)deltaDistance;
	array[47] = (float)ClosestApproachToBeamAxisG;
	array[48] = (float)ClosestApproachToBeamAxisR;
	array[49] = (float)Chi2OfFitR;
	array[50] = (float)NumberOfFittedHitsR;
	array[51] = (float)dalphaMin;
	array[52] = (float)dphiMin;
	array[54] = (float)averageDistance;
      }

      if (mainContributorAnalysis) {
	if (idGeantTrack == mainContributorRecoTrack[k]) {
	  possibleSolution++;
	  
	  if (dchHistogrammer) {
	    array[46] = (float)possibleSolution;
	    mcRecoTrack = new DchMcRecoTrack();
	    mcRecoTrackList->append(mcRecoTrack);
	    mcRecoTrack->fill(array);
	    mcRecoTrack->ntrkG = numberOfPerfectTracks;
	    mcRecoTrack->ntrkR = numberOfReconstructedTracks;
	    mcRecoTrack->set_idGeantTrack(idGeantTrack);
	  }
	}
      }else {
	if (fabs(deltaPhi) < 2/ToDegree && (fabs(deltaZed) < 5)) {
	  possibleSolution++;
          
	  if (dchHistogrammer) {
	    array[46] = (float)possibleSolution;
	    mcRecoTrack = new DchMcRecoTrack();
	    mcRecoTrackList->append(mcRecoTrack);
	    mcRecoTrack->fill(array);
	    mcRecoTrack->ntrkG = numberOfPerfectTracks;
	    mcRecoTrack->ntrkR = numberOfReconstructedTracks;
	    mcRecoTrack->set_idGeantTrack(idGeantTrack);
	  }
	}
      }
    } 
    if (!possibleSolution) {
      if(qualityG==0)   (noSolution[0])++;
      if(qualityG&0x01) (noSolution[1])++;
      if(qualityG&0x02) (noSolution[2])++;
      if(qualityG&0x04) (noSolution[3])++;
      if(qualityG&0x08) (noSolution[4])++;
     
      if (dchHistogrammer) {
        array[46] = (float)possibleSolution;
	mcRecoTrack = new DchMcRecoTrack();
	mcRecoTrackList->append(mcRecoTrack);
	mcRecoTrack->fill(array);
	mcRecoTrack->ntrkG = numberOfPerfectTracks;
	mcRecoTrack->ntrkR = numberOfReconstructedTracks;
	mcRecoTrack->set_idGeantTrack(idGeantTrack);
      }
    }
  }

  if (verbose)
  {
    if (perfectTracks[1]>0)
      cout << "Efficiency for tracks with > 2hits: " 
	   << perfectTracks[1] << " " 
	   << 1.0-(float)noSolution[1]/(float)perfectTracks[1] << endl;
    else
      cout << "Efficiency for tracks with > 2hits: 0." << endl;
    if (perfectTracks[2]>0)
      cout << "Efficiency for tracks with intersection: " 
	   << perfectTracks[2] << " " 
	   << 1.0-(float)noSolution[2]/(float)perfectTracks[2] << endl;
    else
      cout << "Efficiency for tracks with intersection: 0" << endl;
    if (perfectTracks[3]>0)
      cout << "Efficiency for tracks with through full DC (>=6 X1 && >=2 V2): " 
	   << perfectTracks[3] << " " 
	   << 1.0-(float)noSolution[3]/(float)perfectTracks[3] << endl;
    else
      cout << "Efficiency for tracks with through full DC (>=6 X1 && >=2 V2): 0" << endl;
    if (perfectTracks[4]>0)
      cout << "Efficiency for tracks with momentum >= 200 MeV/c: " 
	   << perfectTracks[4] << " " 
	   << 1.0-(float)noSolution[4]/(float)perfectTracks[4] << endl;
    else
      cout << "Efficiency for tracks with momentum >= 200 MeV/c: 0" << endl;
  }
  
  return True;   
} 

PHBoolean mNewDchEvaluator::mainContributorCalculation(PHPointerList<PHNode> &nl, int& rid, int& xhitmul, int& uvhitmul, int& mul, int& xmul, int& uvmul, 
int& amb, int& ambx, int& ambuv, int& gid, int& gxid, int& guvid, 
float& pure, float& purex, float& pureuv)
{
  
   TABLE_HEAD_ST   dDchEvalPar_h;
   DDCHEVALPAR_ST *dDchEvalPar;
   TABLE_HEAD_ST   dDchHit_h;
   DDCHHIT_ST     *dDchHit;
   TABLE_HEAD_ST    dDchGhitHits_h;
   DDCHGHITHITS_ST *dDchGhitHits;
   TABLE_HEAD_ST   dcghit_h;
   DCGHIT_ST      *dcghit;
   TABLE_HEAD_ST    dDchTracks_h;
   DDCHTRACKS_ST   *dDchTracks;
   TABLE_HEAD_ST    dDchTracksPerf_h;
   TABLE_HEAD_ST    fkin_h;
 
   dDchEvalParNode_t*  n1 = static_cast<dDchEvalParNode_t*>(nl[0]);
   dDchHitNode_t*      n2 = static_cast<dDchHitNode_t*>(nl[1]);
   dDchGhitHitsNode_t* n3 = static_cast<dDchGhitHitsNode_t*>(nl[2]);
   dcghitNode_t*       n4 = static_cast<dcghitNode_t*>(nl[3]);
   dDchTracksNode_t*   n5 = static_cast<dDchTracksNode_t*>(nl[4]);
   dDchTracksNode_t*   n6 = static_cast<dDchTracksNode_t*>(nl[5]);
   fkinNode_t*         n7 = static_cast<fkinNode_t*>(nl[6]);

   dDchEvalPar_h    = n1->getData()->TableHeader();
   dDchEvalPar      = n1->getData()->TableData();
   dDchHit_h        = n2->getData()->TableHeader();
   dDchHit          = n2->getData()->TableData();
   dDchGhitHits_h   = n3->getData()->TableHeader();
   dDchGhitHits     = n3->getData()->TableData();
   dcghit_h         = n4->getData()->TableHeader();
   dcghit           = n4->getData()->TableData();
   dDchTracks_h     = n5->getData()->TableHeader();
   dDchTracks       = n5->getData()->TableData();
   dDchTracksPerf_h = n6->getData()->TableHeader();
   fkin_h           = n7->getData()->TableHeader();

   short verbose = dDchEvalPar[0].verbose;
   int plane, k;   
   int contribExists, xcontribExists, uvcontribExists;
   int numberOfHits, numberOfXHits, numberOfUVHits;
   int originalTrackIdPerPlane[numberOfPlanes];
   int wireTypePerPlane[numberOfPlanes];
   for (plane=0; plane < numberOfPlanes; plane++) {
     originalTrackIdPerPlane[plane] = -1;
     wireTypePerPlane[plane] = -1;
   }
   
   int mulmain                = 0;
   int mainContributor        = -1;
   int previousCounter        = 0;
   int finalMainContributor   = -1;
   int ambiguous              = 0;
   int mulxmain               = 0;
   int xmainContributor       = -1;
   int xpreviousCounter       = 0;
   int xfinalMainContributor  = -1;
   int xambiguous             = 0;
   int muluvmain              = 0;
   int uvmainContributor      = -1;
   int uvpreviousCounter      = 0;
   int uvfinalMainContributor = -1;
   int uvambiguous            = 0;

   k     = rid;
   numberOfHits   = 0;
   numberOfXHits  = 0;
   numberOfUVHits = 0;
   if (verbose >= 9) cout << endl
			  << "Analysis of reconstructed track id: "
			  << k << endl << endl;
   for(plane=0; plane<numberOfPlanes-1; plane++) {
     int idhit = dDchTracks[k].hits[plane];    // hit id
     if (verbose >= 10) cout << "plane: " << plane << " hit id: "
			     << idhit;
     if (idhit == -1) {if (verbose >=10) cout << endl; continue;}
     int planeOfHit = dDchHit[idhit].plane;
     short type = wireType[planeOfHit];        // type of wire
     if (verbose >= 10 ) cout << " real plane: " << planeOfHit;
     numberOfHits++;
     if (type == X1Wire || type == X2Wire)
     {
       numberOfXHits++;
       if (verbose >= 10) cout << " which is X" << endl;
     }
     else
     {
       numberOfUVHits++;
       if (verbose >= 10) cout << " which is UV" << endl;
     }
     int idgeant = dDchGhitHits[idhit].ghitid; // geant hit id
     if (idgeant == -1 || idgeant >= dcghit_h.nok) 
       cout << "id geant is =-1 " << endl;
     int idtrack = dcghit[idgeant].mctrack;    // geant track id 
     originalTrackIdPerPlane[plane] = idtrack;
     wireTypePerPlane[plane] = type;
     if (verbose >= 10) cout << "ghit id: " << idgeant 
			     << " geant track id: " << idtrack << endl;
     contribExists     = 0;
     xcontribExists    = 0;
     uvcontribExists   = 0;
     for (int temp=0; temp<plane; temp++)      // found this id before?
     {
       if (idtrack == originalTrackIdPerPlane[temp])
       {
	 contribExists = 1;
	 if (wireTypePerPlane[temp] == X1Wire || 
	     wireTypePerPlane[temp] == X2Wire) 
	   xcontribExists = 1;
	 else
	   uvcontribExists = 1;
       }       
     }
     if ( !contribExists )   mulmain++;
     if ( !xcontribExists && 
	  (type == X1Wire || type == X2Wire) )  mulxmain++;
     if ( !uvcontribExists && 
	  (type == UV1Wire || type == UV2Wire) ) muluvmain++;
   }
   if (verbose >= 9)
   {
     cout << "All hits : " << numberOfHits << endl;
     cout << "X   hits : " << numberOfXHits << endl;
     cout << "UV  hits : " << numberOfUVHits << endl;
     cout << "All contributors: " << mulmain << endl;
     cout << "X   contributors: " << mulxmain << endl;
     cout << "UV  contributors: " << muluvmain << endl;
   }

   // total main contributor 
   for (plane=0; plane < numberOfPlanes-1; plane++) {
     int counter = 1;
     int idtrack = originalTrackIdPerPlane[plane];
     if(idtrack == -1) continue;
     for(int ii=plane+1; ii < numberOfPlanes-1; ii++) {
       int idtrack2 = originalTrackIdPerPlane[ii];
       if (idtrack2 == idtrack) {
	 mainContributor = idtrack;
	 counter++;
       }
     }
     if (counter == previousCounter) ambiguous = 1;
     if (counter > previousCounter) {
       ambiguous            = 0;
       previousCounter      = counter;
       finalMainContributor = mainContributor;
     }
   }
   if (verbose >= 9)
   {
     cout << "Main contributor: " << finalMainContributor << endl;
     cout << "Hits contributed: " << previousCounter << endl;
     cout << "Another equally contributing one: " << ambiguous << endl; 
   }

   // x main contributor
   for (plane=0;plane < numberOfPlanes-1; plane++) {
     int xcounter = 1;
     int idtrack = originalTrackIdPerPlane[plane];
     if (idtrack == -1) continue;
     int idhit = dDchTracks[k].hits[plane];
     if (idhit == -1) continue;
     int planeOfHit = dDchHit[idhit].plane;
     short type = wireType[planeOfHit];
     if(type != X1Wire && type != X2Wire) continue;
     for(int ii=plane+1; ii < numberOfPlanes-1; ii++) {
       int idhit2 = dDchTracks[k].hits[ii];
       if (idhit2 == -1) continue;
       int planeOfHit2 = dDchHit[idhit2].plane;
       short type2 = wireType[planeOfHit2];
       if (type2 !=X1Wire && type2 != X2Wire) continue;
       int idtrack2 = originalTrackIdPerPlane[ii];
       if (idtrack2 == idtrack) {
	 xmainContributor = idtrack;
	 xcounter++;
       }
     }
     if (xcounter == xpreviousCounter) xambiguous = 1;
     if (xcounter > xpreviousCounter) {
       xambiguous            = 0;
       xpreviousCounter      = xcounter;
       xfinalMainContributor = xmainContributor;
     }
   }
   if (verbose >= 9)
   {
     cout << "X Main contributor: " << xfinalMainContributor << endl;
     cout << "X Hits contributed: " << xpreviousCounter << endl;
     cout << "Another equally X contributing one: " << xambiguous << endl; 
   }

   // uv main contributor
   for (plane=0;plane < numberOfPlanes-1; plane++) {
     int uvcounter = 1;
     int idtrack = originalTrackIdPerPlane[plane];
     if (idtrack == -1) continue;
     int idhit = dDchTracks[k].hits[plane];
     if (idhit == -1) continue;
     int planeOfHit = dDchHit[idhit].plane;
     short type = wireType[planeOfHit];
     if(type != UV1Wire && type != UV2Wire) continue; 
     for(int ii=plane+1; ii< numberOfPlanes-1; ii++) {
       int idhit2 = dDchTracks[k].hits[ii];
       if (idhit2 == -1) continue;
       int planeOfHit2 = dDchHit[idhit2].plane;
       short type2 = wireType[planeOfHit2];
       if (type2 !=UV1Wire && type2 != UV2Wire) continue;
       int idtrack2 = originalTrackIdPerPlane[ii];
       if (idtrack2 == idtrack) {
	 uvmainContributor = idtrack;
	 uvcounter++;
       }
     }
     if (uvcounter == uvpreviousCounter) uvambiguous = 1;
     if (uvcounter > uvpreviousCounter) {
       uvambiguous            = 0;
       uvpreviousCounter      = uvcounter;
       uvfinalMainContributor = uvmainContributor;
     }
   }
   if (verbose >= 9)
   {
     cout << "UV Main contributor: " << uvfinalMainContributor << endl;
     cout << "UV Hits contributed: " << uvpreviousCounter << endl;
     cout << "Another equally UV contributing one: " << uvambiguous << endl; 
   }

   xhitmul  = numberOfXHits;
   uvhitmul = numberOfUVHits;
   mul      = mulmain;
   xmul     = mulxmain;
   uvmul    = muluvmain;
   amb      = ambiguous;
   ambx     = xambiguous;
   ambuv    = uvambiguous;
   gid      = finalMainContributor;
   gxid     = xfinalMainContributor;
   guvid    = uvfinalMainContributor;
   if ( numberOfHits > 0 )
     pure   = (float)previousCounter/(float)numberOfHits;
   else
     pure = 0.0;
   if ( numberOfXHits > 0 )
     purex  = (float)xpreviousCounter/(float)numberOfXHits;
   else
     purex = 0.0;
   if ( numberOfUVHits > 0 )
     pureuv = (float)uvpreviousCounter/(float)numberOfUVHits;
   else
     pureuv = 0.0;
   
   if (verbose >= 9 )
   {
     cout << "Final values returned:" << endl;
     cout << "mul    : " << mul << endl;
     cout << "xmul   : " << xmul << endl;
     cout << "uvmul  : " << uvmul << endl;
     cout << "amb    : " << amb << endl;
     cout << "ambx   : " << ambx << endl;
     cout << "ambuv  : " << ambuv << endl;
     cout << "gid    : " << gid << endl;
     cout << "gxid   : " << gxid << endl;
     cout << "guvid  : " << guvid << endl;
     cout << "pure   : " << pure << endl;
     cout << "purex  : " << purex << endl;
     cout << "pureuv : " << pureuv << endl;
     cout << "------------------------------" << endl;
     cout << "Hit <Enter> to continue"; cout << endl; 
     cout << endl;
   }   

  return True;   
} 

PHBoolean mNewDchEvaluator::fillCompleteEvaluation()
{
  DchMcRecoTrack* track;
  if (dchHistogrammer) {
    int totalMcRecoTracks = mcRecoTrackList->length();
    for (int i = 0; i< totalMcRecoTracks; i++ ) {
      track = (*mcRecoTrackList)[i];
      dchHistogrammer->fillCompleteEvaluation(track);
    }
    return True;
  }else{
    return False;
  }

}

PHBoolean mNewDchEvaluator::associatePC(PHCompositeNode* root, int pc)
{  
 PHNodeIterator iter(root);
 PHIODataNode<TObject> *CGLTRACKNode;
 PHIODataNode<TObject> *PADNode ;
 PHIODataNode<TObject> *PADGNode;
 PHIODataNode<TObject> *PADGRelNode;
 PHIODataNode<TObject> *PROJNode;

 CGLTRACKNode = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","dCglTrack");

 PROJNode = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","dPHTrack");

 if (pc == 1) {
   PADNode      = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","dPc1Cluster");
   PADGNode     = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","pc1ghit");
   PADGRelNode  = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","dPc1GhitClus"); 
 }else if (pc ==2) {
   PADNode      = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","dPc2Cluster");
   PADGNode     = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","pc2ghit");
   PADGRelNode  = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","dPc2GhitClus"); 
 }else if (pc == 3) {
   PADNode      = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","dPc3Cluster");
   PADGNode     = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","pc3ghit");
   PADGRelNode  = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","dPc3GhitClus"); 
 }else {
   cout << "NO pad chamber with this id: choose between 1,2 or 3 " << endl;
   return False;
 }

 dCglTrackWrapper* cglTrackWrapper = 0;
 dPadClusterWrapper* pcClusterWrapper = 0;
 pcghitWrapper* pcghit = 0;
 dPadGhitClusWrapper* pcghitClus = 0;
 dPHTrackWrapper* phtrack = 0;

 if (!PROJNode) {
   PHMessage("associatePc",PHWarning,"dPHTrack table not found ");
   return False;
 }else {
   phtrack= (dPHTrackWrapper*)(PROJNode->getData());
 }
 if (!CGLTRACKNode) {
   PHMessage("associatePC1",PHWarning,"dCglTrack table not found");
   return False; 
 }else {
   cglTrackWrapper = (dCglTrackWrapper*)(CGLTRACKNode->getData());
 }

 if (!PADNode) {
   PHMessage("associatePC",PHWarning,"dPcXCluster table not found ");
   return False; 
 }else {
   pcClusterWrapper = (dPadClusterWrapper*)(PADNode->getData());
 }

 if (!PADGNode) {
   PHMessage("associatePC",PHWarning,"pcXghit table not found ");
   return False; 
 }else {
   pcghit= (pcghitWrapper*)(PADGNode->getData());
 }
 if (!PADGRelNode) {
   PHMessage("associatePC",PHWarning,"dPcXGhitClus table not found ");
   return False; 
 }else {
   pcghitClus= (dPadGhitClusWrapper*)(PADGRelNode->getData());
 }

 if (!cglTrackWrapper) return False;
 if (!pcClusterWrapper) return False;
 if (!pcghit) return False;
 if (!pcghitClus) return False;
 if (!phtrack) return False;

 int totalDchMcRecoTracks = mcRecoTrackList->length();
 int idRecoTrack = -1;
 int idGeantTrack = -1;
 int pc1RecoId = -1;
 float x,y,z;
 PHPoint recoPoint;
 PHPoint mcPoint;
 PHPoint projPoint;

 DchMcRecoTrack* mcRecoTrack = 0;
 
 for (int i = 0; i <  totalDchMcRecoTracks; i++) {
   mcRecoTrack = (*mcRecoTrackList)[i];
   idRecoTrack = mcRecoTrack->get_recoID();
   if (idRecoTrack<0) continue;
   idGeantTrack = mcRecoTrack->get_idGeantTrack();
   PHPoint null(-99999,-99999,-99999);
   mcPoint = null;
   recoPoint = null;
   projPoint = null;
   
   //------------- RECONSTRUCTION information-------------
   // the idRecoTrack is the gateway for the idCGlTrack
   if (pc ==1 ) {
     pc1RecoId = cglTrackWrapper->get_pc1clusid(idRecoTrack);
   }else if (pc == 2) {
     pc1RecoId = cglTrackWrapper->get_pc2clusid(idRecoTrack);
   }else if (pc == 3){
     pc1RecoId = cglTrackWrapper->get_pc3clusid(idRecoTrack);
   }
   
   if (pc1RecoId >=0) {
     x = pcClusterWrapper->get_xyz(0,pc1RecoId);
     y = pcClusterWrapper->get_xyz(1,pc1RecoId);
     z = pcClusterWrapper->get_xyz(2,pc1RecoId);
     if (pc ==1) {
       mcRecoTrack->set_pc1RecoId(pc1RecoId); // setting the pc1 reco id 
       mcRecoTrack->set_xPc1Reco(x);
       mcRecoTrack->set_yPc1Reco(y);
       mcRecoTrack->set_zPc1Reco(z);
     }else if (pc ==2) {
       mcRecoTrack->set_pc2RecoId(pc1RecoId);
       mcRecoTrack->set_xPc2Reco(x);
       mcRecoTrack->set_yPc2Reco(y);
       mcRecoTrack->set_zPc2Reco(z);
     }else if (pc ==3) {
       mcRecoTrack->set_pc3RecoId(pc1RecoId);
       mcRecoTrack->set_xPc3Reco(x);
       mcRecoTrack->set_yPc3Reco(y);
       mcRecoTrack->set_zPc3Reco(z);
     }
     recoPoint.setX(x);
     recoPoint.setY(y);
     recoPoint.setZ(z);
   }else {
     if (pc ==1) {
       mcRecoTrack->set_pc1RecoId(-1); // setting the pc1 reco id 
       mcRecoTrack->set_xPc1Reco(-9999);
       mcRecoTrack->set_yPc1Reco(-9999);
       mcRecoTrack->set_zPc1Reco(-9999);
     }else if (pc ==2) {
       mcRecoTrack->set_pc2RecoId(-1);
       mcRecoTrack->set_xPc2Reco(-9999);
       mcRecoTrack->set_yPc2Reco(-9999);
       mcRecoTrack->set_zPc2Reco(-9999);
     }else if (pc ==3) {
       mcRecoTrack->set_pc3RecoId(-1);
       mcRecoTrack->set_xPc3Reco(-9999);
       mcRecoTrack->set_yPc3Reco(-9999);
       mcRecoTrack->set_zPc3Reco(-9999);
     }
   }
   if (pc == 1) {
     if (phtrack->get_ifIntersectPc1(idRecoTrack)) {
       x = phtrack->get_projectionPc1(0,idRecoTrack);
       y = phtrack->get_projectionPc1(1,idRecoTrack);
       z = phtrack->get_projectionPc1(2,idRecoTrack);
       projPoint.setX(x);
       projPoint.setY(y);
       projPoint.setZ(z);
       mcRecoTrack->set_xPc1Proj(x);
       mcRecoTrack->set_yPc1Proj(y);
       mcRecoTrack->set_zPc1Proj(z);
     }else {
       mcRecoTrack->set_xPc1Proj(-9999);
       mcRecoTrack->set_yPc1Proj(-9999);
       mcRecoTrack->set_zPc1Proj(-9999);
     }
   }else if (pc ==2) {
     if (phtrack->get_ifIntersectPc2(idRecoTrack)) {
       x = phtrack->get_projectionPc2(0,idRecoTrack);
       y = phtrack->get_projectionPc2(1,idRecoTrack);
       z = phtrack->get_projectionPc2(2,idRecoTrack);
       projPoint.setX(x);
       projPoint.setY(y);
       projPoint.setZ(z);
       mcRecoTrack->set_xPc2Proj(x);
       mcRecoTrack->set_yPc2Proj(y);
       mcRecoTrack->set_zPc2Proj(z);
     }else {
       mcRecoTrack->set_xPc2Proj(-9999);
       mcRecoTrack->set_yPc2Proj(-9999);
       mcRecoTrack->set_zPc2Proj(-9999);
     }
   }else if (pc==3) {
     if (phtrack->get_ifIntersectPc3(idRecoTrack)){
       x = phtrack->get_projectionPc3(0,idRecoTrack);
       y = phtrack->get_projectionPc3(1,idRecoTrack);
       z = phtrack->get_projectionPc3(2,idRecoTrack);
       projPoint.setX(x);
       projPoint.setY(y);
       projPoint.setZ(z);
       mcRecoTrack->set_xPc3Proj(x);
       mcRecoTrack->set_yPc3Proj(y);
       mcRecoTrack->set_zPc3Proj(z);
     }else {
       mcRecoTrack->set_xPc3Proj(-9999);
       mcRecoTrack->set_yPc3Proj(-9999);
       mcRecoTrack->set_zPc3Proj(-9999);
     }
   }

   //------------- MONTE-CARLO information
   // the idGeantTrack and tmpGeantTrack shoudl be equal 
   int tmpRecoId;
   int tmpMcId;
   int tmpGeantTrack = 0;
   x = y = z = -9999;
   if (pc==1) {
     mcRecoTrack->set_xPc1Mc(x);
     mcRecoTrack->set_yPc1Mc(y);
     mcRecoTrack->set_zPc1Mc(z);
   }else if (pc ==2) {
     mcRecoTrack->set_xPc2Mc(x);
     mcRecoTrack->set_yPc2Mc(y);
     mcRecoTrack->set_zPc2Mc(z);
   }else if (pc==3) {
     mcRecoTrack->set_xPc3Mc(x);
     mcRecoTrack->set_yPc3Mc(y);
     mcRecoTrack->set_zPc3Mc(z);
   }
   for (int l=0; l < int(pcghit->RowCount()); l++) {
     tmpGeantTrack = pcghit->get_mctrack(l);
     if (tmpGeantTrack == idGeantTrack) { // this is the right track
       x = pcghit->get_xyzinglo(0,l);   
       y = pcghit->get_xyzinglo(1,l);   
       z = pcghit->get_xyzinglo(2,l);
       mcPoint.setX(x);
       mcPoint.setY(y);
       mcPoint.setZ(z);
       if (pc ==1) {
	 mcRecoTrack->set_xPc1Mc(x);
	 mcRecoTrack->set_yPc1Mc(y);
	 mcRecoTrack->set_zPc1Mc(z);
       }else if (pc ==2 ) {
	 mcRecoTrack->set_xPc2Mc(x);
	 mcRecoTrack->set_yPc2Mc(y);
	 mcRecoTrack->set_zPc2Mc(z);
       }else if (pc == 3) {
	 mcRecoTrack->set_xPc3Mc(x);
	 mcRecoTrack->set_yPc3Mc(y);
	 mcRecoTrack->set_zPc3Mc(z);
       }
     }
   }

   // ---------------- RELATIONAL info  
   if (pc == 1) mcRecoTrack->set_pc1McId(-1);
   if (pc == 2) mcRecoTrack->set_pc2McId(-1);
   if (pc == 3) mcRecoTrack->set_pc3McId(-1);

   for (int k =0; k < int(pcghitClus->RowCount()); k++) {
     tmpRecoId     = pcghitClus->get_clusid(k);
     tmpMcId       = pcghitClus->get_ghitid(k);
     tmpGeantTrack = pcghit->get_mctrack(tmpMcId);
     if (tmpGeantTrack == idGeantTrack) { // 
      if (pc ==1 ) mcRecoTrack->set_pc1McId(tmpRecoId); //
      if (pc ==2 ) mcRecoTrack->set_pc2McId(tmpRecoId);
      if (pc ==3 ) mcRecoTrack->set_pc3McId(tmpRecoId);
     }
   }

   if (0) {
     cout << "PC "<< pc << endl;
     if (pc ==1) {
     cout << "Reco Id  " << mcRecoTrack->get_pc1RecoId() << " == "<< 
       mcRecoTrack->get_pc1McId() << endl;
     }else if (pc ==2) {
       cout << "Reco Id "<< mcRecoTrack->get_pc2RecoId() << " == "<< 
	 mcRecoTrack->get_pc2McId() << endl;
     }else if (pc ==3) {
       cout << "REco id "<< mcRecoTrack->get_pc3RecoId() << " == "<< 
	 mcRecoTrack->get_pc3McId() << endl;
     }
     recoPoint.print();
     projPoint.print();
     mcPoint.print();
   }

   // --------------- MAIN contributor // probably not necessary
   int totalContributors = 0;
   for (int m=0; m < int(pcghitClus->RowCount()); m++) {
     tmpRecoId     = pcghitClus->get_clusid(m);
     tmpMcId       = pcghitClus->get_ghitid(m);
     if (tmpRecoId == pc1RecoId) {
      if (totalContributors < 10) 
       totalContributors++;
     }
   }   
   if (totalContributors > 1) {
     for (int kk=0; kk<10; kk++) {
       
     }
   }else {
   }

 }

 return True;

}

PHBoolean mNewDchEvaluator::extrapolateToVertex(PHCompositeNode* root) 
{  
  VtxOut *vtxout = 0;
  BbcOut *bbcout = 0;

  PHTypedNodeIterator<VtxOut> vtxiter(root);
  VtxOutNode_t *VtxOutNode = vtxiter.find("VtxOut");
  if (VtxOutNode)   {
    vtxout = VtxOutNode->getData();
  }else{
    return False;
  }

  PHTypedNodeIterator<BbcOut> bbciter(root);
  BbcOutNode_t *BbcOutNode = bbciter.find("BbcOut");
  if (BbcOutNode){
    bbcout = BbcOutNode->getData();
  }else{
    return False;
  }

 float bbcvtx = -10000;
 float bbct0  = -10000;
 if( vtxout->isValid()) {
   bbcvtx = vtxout->get_ZVertex();
 }else {
   cout << "mNewDchEvaluator: vtxout is not valid " << endl;
 }
 if (bbcout->isValid() ) {
   bbct0  = bbcout->get_TimeZero();
 }else {
   cout << "mNewDchEvaluator: bbcout is not valid " << endl;
 }
 if (!mcRecoTrackList ) return False;
 int totalTracks =  mcRecoTrackList->length();

 DchMcRecoTrack* mcRecoTrack;

 float n,m,z;
 float x1,y1,z1,r1; // pc1 positions
 float x3,y3,z3,r3; // pc3 positions

 for (int i=0; i < totalTracks; i++) {
   mcRecoTrack = (*mcRecoTrackList)[i];
   mcRecoTrack->set_bbcvtx(bbcvtx);
   mcRecoTrack->set_bbct0(bbct0);

    // Monte-Carlo calculation

    x1 = mcRecoTrack->get_xPc1Mc();
    y1 = mcRecoTrack->get_yPc1Mc();
    z1 = mcRecoTrack->get_zPc1Mc();
    x3 = mcRecoTrack->get_xPc3Mc();
    y3 = mcRecoTrack->get_yPc3Mc();
    z3 = mcRecoTrack->get_zPc3Mc();
    if (x3 > -9000 && y3 > -9000 && z3 > -9000 &&
	x1 > -9000 && y1 > -9000 && z1 > -9000) {
      
      r1 = sqrt(x1*x1+y1*y1);
      r3 = sqrt(x3*x3+y3*y3);
  
      if (z3 != z1) {
	m = (r3 -r1)/(z3-z1);
	n = r3 - (m*z3);
	z = -n/m;
	mcRecoTrack->set_pc13vtxm(z);
      }else {
	mcRecoTrack->set_pc13vtxm(-9999);
      }
    }else {
      mcRecoTrack->set_pc13vtxm(-9999);
    }

    // REconstructed calculation

    x1 = mcRecoTrack->get_xPc1Reco();
    y1 = mcRecoTrack->get_yPc1Reco();
    z1 = mcRecoTrack->get_zPc1Reco();
    x3 = mcRecoTrack->get_xPc3Reco();
    y3 = mcRecoTrack->get_yPc3Reco();
    z3 = mcRecoTrack->get_zPc3Reco();

    if (x3 > -9000 && y3 > -9000 && z3 > -9000 &&
	x1 > -9000 && y1 > -9000 && z1 > -9000) {
      
      r1 = sqrt(x1*x1 + y1*y1);
      r3 = sqrt(x3*x3 + y3*y3);
  
      if (z3 != z1) {
	m = (r3 -r1)/(z3-z1);
	n = r3 - (m*z3);
	z = -n/m;
	mcRecoTrack->set_pc13vtxr(z);
      }else {
	mcRecoTrack->set_pc13vtxr(-9999);
      }
    }else {
      mcRecoTrack->set_pc13vtxr(-9999);
    }

  }

  return True;

}

PHBoolean mNewDchEvaluator::associateTOF(PHCompositeNode* root)
{  
  PHNodeIterator iter(root);
  PHIODataNode<TObject> *CGLTRACKNode;
  PHIODataNode<TObject> *TOFNode;
  PHIODataNode<TObject> *TOFGNode;
  PHIODataNode<TObject> *TOFGRelNode;
  PHIODataNode<TObject> *PROJNode;
  
  CGLTRACKNode = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","dCglTrack");
  PROJNode = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","dPHTrack");
  
  TOFNode  = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","dTofReconstructed");
  TOFGNode = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","dTofGdigi");
  TOFGRelNode = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","dTofGdigiRec");
  
  dPHTrackWrapper*           phtrack = 0;
  dCglTrackWrapper*          cglTrack = 0;
  dTofReconstructedWrapper*  tofReconstructed = 0;
  dTofGdigiWrapper*          tofGdigi = 0;
  dTofGdigiRecWrapper*       tofGdigiRec = 0;
  
  if (!PROJNode) {
    PHMessage("associateTOF",PHWarning,"dPHTrack table not found ");
    return False;
  }else {
    phtrack= (dPHTrackWrapper*)(PROJNode->getData());
  }
  if (!CGLTRACKNode) {
    PHMessage("associateTOF",PHWarning,"dCglTrack table not found");
    return False; 
  }else {
    cglTrack = (dCglTrackWrapper*)(CGLTRACKNode->getData());
  }
  if (!TOFNode) {
    PHMessage("associateTOF",PHWarning,"dTofReconstructed  table not found ");
    return False; 
  }else {
    tofReconstructed = (dTofReconstructedWrapper*)(TOFNode->getData());
  }
  if (!TOFGNode) {
    PHMessage("associateTOF",PHWarning," dTofGdigi table not found ");
    return False; 
  }else {
    tofGdigi = (dTofGdigiWrapper*)(TOFGNode->getData());
  }
  if (!TOFGRelNode) {
    PHMessage("associateTOF",PHWarning," dTofGdigiRec table not found ");
    return False; 
  }else {
    tofGdigiRec  = (dTofGdigiRecWrapper*)(TOFGRelNode->getData());
  }
  
  if (!phtrack) return False;
  if (!cglTrack ) return False;
  if (!tofReconstructed) return False;
  if (!tofGdigi) return False;
  if (!tofGdigiRec) return False;
  
  int totalDchMcRecoTracks = mcRecoTrackList->length();
  int idRecoTrack;
  int idGeantTrack;
  int tofRecoId;
  float x,y,z;
  float path;
  float eloss_reco;
  int   pid_reco;
  float tof_reco;
  
  PHPoint recoPoint;
  PHPoint mcPoint;
  PHPoint projPoint;
  
  DchMcRecoTrack* mcRecoTrack = 0;
  for (int i = 0; i <  totalDchMcRecoTracks; i++) { // Dch track loop
    
    mcRecoTrack = (*mcRecoTrackList)[i];
    idRecoTrack = mcRecoTrack->get_recoID();
    if (idRecoTrack<0) continue;
    idGeantTrack = mcRecoTrack->get_idGeantTrack();
    PHPoint null(-99999,-99999,-99999);
    mcPoint = null;
    recoPoint = null;
    projPoint = null;
    
    //------------- RECONSTRUCTION information-------------
    // the idRecoTrack is the gateway for the idCGlTrack
    
    tofRecoId = cglTrack->get_tofrecid(idRecoTrack);
    
    // TOF McReco
    if (tofRecoId >=0) {
      
      x = tofReconstructed->get_xtof(0,tofRecoId); 
      y = tofReconstructed->get_xtof(1,tofRecoId); 
      z = tofReconstructed->get_xtof(2,tofRecoId); 
      tof_reco = tofReconstructed->get_tof(tofRecoId);
      eloss_reco = tofReconstructed->get_eloss(tofRecoId);
      pid_reco = -1;  // to be introduced 
      
      recoPoint.setX(x);
      recoPoint.setY(y);
      recoPoint.setZ(z);
      mcRecoTrack->set_tofRecoId(tofRecoId); // setting the tof reco id 
      mcRecoTrack->set_xTofReco(x);
      mcRecoTrack->set_yTofReco(y);
      mcRecoTrack->set_zTofReco(z);
      mcRecoTrack->set_tofTofReco(tof_reco);
      mcRecoTrack->set_elossTofReco(eloss_reco);
      mcRecoTrack->set_pidTofReco(pid_reco);
    }else {
      mcRecoTrack->set_tofRecoId(-1); // setting the tof reco id 
      mcRecoTrack->set_xTofReco(-9999);
      mcRecoTrack->set_yTofReco(-9999);
      mcRecoTrack->set_zTofReco(-9999);
      mcRecoTrack->set_tofTofReco(-9999);
      mcRecoTrack->set_elossTofReco(-9999);
      mcRecoTrack->set_pidTofReco(-9999);
    }
    
    // TOF Projection
    if (phtrack->get_ifIntersectTof(idRecoTrack) ) {
      x = phtrack->get_projectionTof(0,idRecoTrack);
      y = phtrack->get_projectionTof(1,idRecoTrack);
      z = phtrack->get_projectionTof(2,idRecoTrack);
      path = phtrack->get_tofPathLength(idRecoTrack);
      
      projPoint.setX(x);
      projPoint.setY(y);
      projPoint.setZ(z);
      mcRecoTrack->set_xTofProj(x);
      mcRecoTrack->set_yTofProj(y);
      mcRecoTrack->set_zTofProj(z);
      mcRecoTrack->set_pathlTof(path);
    }else {
      mcRecoTrack->set_xTofProj(-9999);
      mcRecoTrack->set_yTofProj(-9999);
      mcRecoTrack->set_zTofProj(-9999);
      mcRecoTrack->set_pathlTof(-9999);
    }
    
    //------------- MONTE-CARLO information --------------
    // the idGeantTrack and tmpGeantTrack shoudl be equal 
    // TOF MC (tofGdigi)
    int tmpRecoId;
    int tmpMcId;
    int tmpGeantTrack;
    float tof_mc;
    float dele_mc;
    short partl_mc;

    x = y = z = -9999;
    mcRecoTrack->set_xTofMc(x);
    mcRecoTrack->set_yTofMc(y);
    mcRecoTrack->set_zTofMc(z);
    mcRecoTrack->set_tofTofMc(-9999);
    mcRecoTrack->set_elossTofMc(-9999);
    mcRecoTrack->set_pidTofMc(-9999);

    // Search TOF Gdigi for this track
    for (int l=0; l < int(tofGdigi->RowCount()); l++) {
      tmpGeantTrack = tofGdigi->get_mctrack(l);

      if (tmpGeantTrack == idGeantTrack) { // this is the right track
	x = tofGdigi->get_pos_m(0,l); 
	y = tofGdigi->get_pos_m(1,l); 
	z = tofGdigi->get_pos_m(2,l);
	tof_mc  = tofGdigi->get_tof(l);
	dele_mc = tofGdigi->get_eloss(l);
	partl_mc = tofGdigi->get_partl(l);
	
	mcPoint.setX(x);
	mcPoint.setY(y);
	mcPoint.setZ(z);

	mcRecoTrack->set_xTofMc(x);
	mcRecoTrack->set_yTofMc(y);
	mcRecoTrack->set_zTofMc(z);
	mcRecoTrack->set_tofTofMc(tof_mc);
	mcRecoTrack->set_elossTofMc(dele_mc);
	mcRecoTrack->set_pidTofMc(partl_mc);
      }
    }
    
    //---------------- RELATIONAL info  ----------------
    mcRecoTrack->set_tofMcId(-1);
    
    for (int k =0; k < int(tofGdigiRec->RowCount()); k++){
      tmpRecoId     = tofGdigiRec->get_recid(k);
      tmpMcId       = tofGdigiRec->get_gdigiid(k);
      tmpGeantTrack = tofGdigi->get_mctrack(tmpMcId);
      if (tmpGeantTrack == idGeantTrack) {
	mcRecoTrack->set_tofMcId(tmpRecoId); 
      }
    }
    if (tofRecoId >= 0) {
    }
  } 
  return True;  
}

PHBoolean mNewDchEvaluator::associateEMC(PHCompositeNode* root)
{  
  PHNodeIterator iter(root);
  PHIODataNode<TObject> *CGLTRACKNode;
  PHIODataNode<TObject> *DCHTRKNode;
  PHIODataNode<TObject> *PROJNode;
  PHIODataNode<TObject> *EMCNode;
  PHIODataNode<TObject> *EMCGEANode;
  
  CGLTRACKNode = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","dCglTrack");
  DCHTRKNode = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","dDchTracks");
  PROJNode = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","dPHTrack");
  EMCNode  = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","dEmcClusterLocalExt");
  EMCGEANode  = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","dEmcGeaClusterTrack");
  
  dPHTrackWrapper*            phtrack = 0;
  dCglTrackWrapper*           cglTrack = 0;
  dDchTracksWrapper*          dchtrack = 0;
  dEmcClusterLocalExtWrapper* emcClusterLocalExt = 0;
  dEmcGeaClusterTrackWrapper* emcGeaClusterTrack = 0;
  
  if (!PROJNode) {
    PHMessage("associateEMC",PHWarning,"dPHTrack table not found ");
    return False;
  }else {
    phtrack= (dPHTrackWrapper*)(PROJNode->getData());
  }
  if (!CGLTRACKNode) {
    PHMessage("associateEMC",PHWarning,"dCglTrack table not found");
    return False; 
  }else {
    cglTrack = (dCglTrackWrapper*)(CGLTRACKNode->getData());
  }
  if (!DCHTRKNode) {
    PHMessage("associateCRK",PHWarning,"dDchTracks table not found ");
    return False;
  }else {
    dchtrack= (dDchTracksWrapper*)(DCHTRKNode->getData());
  }
  if (!EMCNode) {
    PHMessage("associateEMC",PHWarning,"dEmcClusterLocalExt table not found ");
    return False; 
  }else {
    emcClusterLocalExt = (dEmcClusterLocalExtWrapper*)(EMCNode->getData());
  }
  if (!EMCGEANode) {
    PHMessage("associateEMC",PHWarning,"dEmcGeaClusterTrack table not found ");
    return False; 
  }else {
    emcGeaClusterTrack = (dEmcGeaClusterTrackWrapper*)(EMCGEANode->getData());
  }
  
  if (!phtrack) return False;
  if (!cglTrack ) return False;
  if (!dchtrack) return False;
  if (!emcClusterLocalExt) return False;
  if (!emcGeaClusterTrack) return False;
  
  int totalDchMcRecoTracks = mcRecoTrackList->length();
  int idRecoTrack;
  int idGeantTrack;
  int emcRecoId;
  
  PHPoint recoPoint;
  PHPoint mcPoint;
  PHPoint projPoint;
  
  DchMcRecoTrack* mcRecoTrack = 0;

  for (int i = 0; i <  totalDchMcRecoTracks; i++) { // Dch track loop
    
    mcRecoTrack = (*mcRecoTrackList)[i];
    idRecoTrack = mcRecoTrack->get_recoID();
    if (idRecoTrack<0) continue;
    idGeantTrack = mcRecoTrack->get_idGeantTrack();
    PHPoint null(-99999,-99999,-99999);
    mcPoint = null;
    recoPoint = null;
    projPoint = null;
    
    //------------- RECONSTRUCTION information-------------
    // the idRecoTrack is the gateway for the idCGlTrack
    
    emcRecoId = cglTrack->get_emcclusid(idRecoTrack);
    if (emcRecoId >=0) {
      mcRecoTrack->set_emcRecoId(emcRecoId);
      mcRecoTrack->set_xEmcReco(emcClusterLocalExt->get_xyz(0,emcRecoId));
      mcRecoTrack->set_yEmcReco(emcClusterLocalExt->get_xyz(1,emcRecoId));
      mcRecoTrack->set_zEmcReco(emcClusterLocalExt->get_xyz(2,emcRecoId));
      int swkey = 100000*emcClusterLocalExt->get_arm(emcRecoId) +
                   10000*emcClusterLocalExt->get_sector(emcRecoId) +
                     100*emcClusterLocalExt->get_ind(1,emcRecoId) +
                         emcClusterLocalExt->get_ind(0,emcRecoId);
      mcRecoTrack->set_emcswkey(swkey);
      mcRecoTrack->set_emcmease(emcClusterLocalExt->get_e(emcRecoId));
      mcRecoTrack->set_emcecore(emcClusterLocalExt->get_ecore(emcRecoId));
      mcRecoTrack->set_emcecorr(emcClusterLocalExt->get_ecorr(emcRecoId));
      mcRecoTrack->set_emcecent(emcClusterLocalExt->get_ecent(emcRecoId));
      mcRecoTrack->set_emctof(emcClusterLocalExt->get_tof(emcRecoId));
      mcRecoTrack->set_emctofcorr(emcClusterLocalExt->get_tofcorr(emcRecoId));
      mcRecoTrack->set_emctofmin(emcClusterLocalExt->get_tofmin(emcRecoId));
      mcRecoTrack->set_emcprobphot(emcClusterLocalExt->get_prob_photon(emcRecoId));
      mcRecoTrack->set_twrhit(emcClusterLocalExt->get_twrhit(emcRecoId));
      mcRecoTrack->set_emcchi2(emcClusterLocalExt->get_chi2(emcRecoId));
      mcRecoTrack->set_emcpartesum0(emcClusterLocalExt->get_partesum(0,emcRecoId));
      mcRecoTrack->set_emcpartesum1(emcClusterLocalExt->get_partesum(1,emcRecoId));
      mcRecoTrack->set_emcpartesum2(emcClusterLocalExt->get_partesum(2,emcRecoId));
      mcRecoTrack->set_emcpartesum3(emcClusterLocalExt->get_partesum(3,emcRecoId));

      for (int i = 0; i<int(emcGeaClusterTrack->RowCount()); i++) {
        if ( emcGeaClusterTrack->get_clusid(i)!=emcRecoId ) continue;
        mcRecoTrack->set_emcAnctrkno0(emcGeaClusterTrack->get_trkno(0,i));
        mcRecoTrack->set_emcAnctrkno1(emcGeaClusterTrack->get_trkno(1,i));
        mcRecoTrack->set_emcAnctrkno2(emcGeaClusterTrack->get_trkno(2,i));
        mcRecoTrack->set_emcAnctwrhit0(emcGeaClusterTrack->get_tracktwrhit(0,i));
	mcRecoTrack->set_emcAnctwrhit1(emcGeaClusterTrack->get_tracktwrhit(1,i));
	mcRecoTrack->set_emcAnctwrhit2(emcGeaClusterTrack->get_tracktwrhit(2,i));
        mcRecoTrack->set_emcAncpid0(lroundf(emcGeaClusterTrack->get_pid(0,i)));
	mcRecoTrack->set_emcAncpid1(lroundf(emcGeaClusterTrack->get_pid(1,i)));
	mcRecoTrack->set_emcAncpid2(lroundf(emcGeaClusterTrack->get_pid(2,i)));
        mcRecoTrack->set_emcAncedep0(emcGeaClusterTrack->get_edep(0,i));
        mcRecoTrack->set_emcAncedep1(emcGeaClusterTrack->get_edep(1,i));
        mcRecoTrack->set_emcAncedep2(emcGeaClusterTrack->get_edep(2,i));
        mcRecoTrack->set_emcAncptot0(emcGeaClusterTrack->get_ptot(0,i));
        mcRecoTrack->set_emcAncptot1(emcGeaClusterTrack->get_ptot(1,i));
        mcRecoTrack->set_emcAncptot2(emcGeaClusterTrack->get_ptot(2,i));
      }
    }
    // EMC Projection not taken from cgl but corrected a la YA

    float alpha = mcRecoTrack->get_dalpha() + mcRecoTrack->get_alphaG();
    float beta  = mcRecoTrack->get_dbeta()  + mcRecoTrack->get_betaG();
    float pemcx, pemcy, pemcz, r, phi, phi_corr, z_corr;
    if (phtrack->get_ifIntersectPbsc(idRecoTrack) ) { 
      pemcx = phtrack->get_projectionPbSc(0,idRecoTrack);
      pemcy = phtrack->get_projectionPbSc(1,idRecoTrack);
      pemcz = phtrack->get_projectionPbSc(2,idRecoTrack);
      phi = atan(pemcy/pemcx);
      r   = sqrt(pemcx*pemcx+pemcy*pemcy);
      phi_corr = phi + 0.1*alpha - 0.0006;
      z_corr   = pemcz + 0.15 -10.6*(beta-1.570796);
      mcRecoTrack->set_xEmcProj(r*cos(phi_corr));
      mcRecoTrack->set_yEmcProj(r*sin(phi_corr));
      mcRecoTrack->set_zEmcProj(z_corr);
      mcRecoTrack->set_pathlEmc(phtrack->get_emcPathLength(idRecoTrack));
    }
    if (phtrack->get_ifIntersectPbgl(idRecoTrack) ) {
      pemcx = phtrack->get_projectionPbGl(0,idRecoTrack);
      pemcy = phtrack->get_projectionPbGl(1,idRecoTrack);
      pemcz = phtrack->get_projectionPbGl(2,idRecoTrack);
      phi = atan(pemcy/pemcx);
      phi_corr = phi + 0.1*alpha +0.007;
      z_corr   = pemcz + 0.33 -6.5*(beta-1.570796);
      mcRecoTrack->set_xEmcProj(pemcx);
      mcRecoTrack->set_yEmcProj(pemcy*tan(phi_corr));
      mcRecoTrack->set_zEmcProj(z_corr);
      mcRecoTrack->set_pathlEmc(phtrack->get_emcPathLength(idRecoTrack));
    }

    for (int i = 0; i<int(emcGeaClusterTrack->RowCount()); i++) {
      int index  = -1; 
      int index0 = emcGeaClusterTrack->get_trkno(0,i);
      int index1 = emcGeaClusterTrack->get_trkno(1,i);
      int index2 = emcGeaClusterTrack->get_trkno(2,i);
      if ( idGeantTrack==index0 || 
	   idGeantTrack==index1 ||
	   idGeantTrack==index2 ) {
	if ( idGeantTrack==index0 ) index=0;
	if ( idGeantTrack==index1 ) index=1;
	if ( idGeantTrack==index2 ) index=2;
	if ( emcGeaClusterTrack->get_efrac(index,i)<0.5 ) continue;
        mcRecoTrack->set_emcMcId(emcGeaClusterTrack->get_clusid(i));
	mcRecoTrack->set_xEmcMc(emcClusterLocalExt->get_xyz(0,emcGeaClusterTrack->get_clusid(i)));
	mcRecoTrack->set_yEmcMc(emcClusterLocalExt->get_xyz(1,emcGeaClusterTrack->get_clusid(i)));
	mcRecoTrack->set_zEmcMc(emcClusterLocalExt->get_xyz(2,emcGeaClusterTrack->get_clusid(i)));
        mcRecoTrack->set_emcMcefrac(emcGeaClusterTrack->get_efrac(index,i));
	mcRecoTrack->set_emcMcecore(emcGeaClusterTrack->get_ecore(i));
	mcRecoTrack->set_emcMcmease(emcGeaClusterTrack->get_mease(i));
	mcRecoTrack->set_emcMctof(emcGeaClusterTrack->get_tof(i));
      }
    }
  } 
  return True;  
}

PHBoolean mNewDchEvaluator::associateCRK(PHCompositeNode* root)
{  
  PHNodeIterator iter(root);
  PHIODataNode<TObject> *CGLTRACKNode;
  PHIODataNode<TObject> *DCHTRKNode;
  PHIODataNode<TObject> *PROJNode;
  PHIODataNode<TObject> *PADNode ;
  PHIODataNode<TObject> *EMCNode;

  CGLTRACKNode = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","dCglTrack");
  PROJNode = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","dPHTrack");
  DCHTRKNode = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","dDchTracks");
  PADNode = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","dPc1Cluster");
  EMCNode = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","dEmcClusterLocalExt");

  dPHTrackWrapper*            phtrack = 0;
  dDchTracksWrapper*          dchtrack = 0;
  dCglTrackWrapper*           cglTrack = 0;
  dPadClusterWrapper*         pcCluster = 0;
  dEmcClusterLocalExtWrapper* emcClusterLocalExt = 0;

  if (!PROJNode) {
    PHMessage("associateCRK",PHWarning,"dPHTrack table not found ");
    return False;
  }else {
    phtrack= (dPHTrackWrapper*)(PROJNode->getData());
  }
  if (!DCHTRKNode) {
    PHMessage("associateCRK",PHWarning,"dDchTracks table not found ");
    return False;
  }else {
    dchtrack= (dDchTracksWrapper*)(DCHTRKNode->getData());
  }
  if (!CGLTRACKNode) {
    PHMessage("associateCRK",PHWarning,"dCglTrack table not found");
    return False; 
  }else {
    cglTrack = (dCglTrackWrapper*)(CGLTRACKNode->getData());
  }
  if (!PADNode) {
    PHMessage("associateCRK",PHWarning,"dPcXCluster table not found ");
    return False; 
  }else {
    pcCluster = (dPadClusterWrapper*)(PADNode->getData());
  }
  if (!EMCNode) {
    PHMessage("associateCRK",PHWarning,"dEmcClusterLocalExt table not found");
     return False; 
  }else {
    emcClusterLocalExt = (dEmcClusterLocalExtWrapper*)(EMCNode->getData());
  }

  if (!phtrack) return False;
  if (!dchtrack) return False;
  if (!cglTrack ) return False;
  if (!pcCluster ) return False;
  if (!emcClusterLocalExt) return False;

  int totalDchMcRecoTracks = mcRecoTrackList->length();
  int idRecoTrack;
  int pc1RecoId, pc3RecoId, emcRecoId;
  PHPoint recoPoint;
  PHPoint mcPoint;

  DchMcRecoTrack* mcRecoTrack = 0;
  for (int i = 0; i <  totalDchMcRecoTracks; i++) { // Dch track loop
    mcRecoTrack = (*mcRecoTrackList)[i];
    idRecoTrack = mcRecoTrack->get_recoID();
    if (idRecoTrack<0) continue;
    PHPoint null(-99999,-99999,-99999);
    mcPoint = null;
    recoPoint = null;
    //------------- RECONSTRUCTION information-------------
    // the idRecoTrack is the gateway for the idCGlTrack
    //-----------------------------------------------------
    pc1RecoId = dchtrack->get_hits(39,idRecoTrack);
    pc3RecoId = cglTrack->get_pc3clusid(idRecoTrack);
    emcRecoId = cglTrack->get_emcclusid(idRecoTrack);
    if ( pc1RecoId<0 || 
         !phtrack->get_ifIntersectPc2(idRecoTrack) ) continue;
    PHPoint pstart(pcCluster->get_xyz(0,pc1RecoId),
                   pcCluster->get_xyz(1,pc1RecoId),
                   pcCluster->get_xyz(2,pc1RecoId));
    PHPoint pend(phtrack->get_projectionPc2(0,idRecoTrack),
                 phtrack->get_projectionPc2(1,idRecoTrack),
                 phtrack->get_projectionPc2(2,idRecoTrack));
    if ( pcCluster->get_xyz(0,pc1RecoId)>0.0 ) {
      if ( emcRecoId>=0 ) {
        if ( emcClusterLocalExt->get_ecore(emcRecoId)>0.1 ) {
          pend = PHPoint(emcClusterLocalExt->get_xyz(0,emcRecoId),
                         emcClusterLocalExt->get_xyz(1,emcRecoId),
                         emcClusterLocalExt->get_xyz(2,emcRecoId));
        }
      }
    } else {
      if ( pc3RecoId>=0 ) {
        pend = PHPoint(pcCluster->get_xyz(0,pc3RecoId),
                       pcCluster->get_xyz(1,pc3RecoId),
                       pcCluster->get_xyz(2,pc3RecoId));
      }
    }
    PHLine rich_proj(pstart,pend);
    CrkPIDout rich;
    d_crkpid.SetCrkHitFromTop(root);
    d_crkpid.AssociateTrack(rich_proj,&rich);
    if ( rich.accepted )
      mcRecoTrack->set_crkacc(1);
    else
      mcRecoTrack->set_crkacc(0);
    mcRecoTrack->set_crknpmt0(rich.npmt0);
    mcRecoTrack->set_crknpmt1(rich.npmt1);
    mcRecoTrack->set_crknpmt3(rich.npmt3);
    mcRecoTrack->set_crknpe0(rich.npe0);
    mcRecoTrack->set_crknpe1(rich.npe1);
    mcRecoTrack->set_crknpe3(rich.npe3);
    mcRecoTrack->set_crkchi2(rich.chi2);
    mcRecoTrack->set_crkdisp(rich.disp);
    mcRecoTrack->set_crkpath(rich.path);
  } 
  return True;  
}
                                                                   
void mNewDchEvaluator::LoadMatchPar(){

  //for MC, we should assume mean 0, the commented value is for real data
  APpc1 = 1.077*(0.01);
  BPpc1 = 1.057*10;
  CPpc1 = 2.423*(0.001);
  DPpc1 = 0;
  
  AZpc1 = 6.6035 ;   
  BZpc1 = 14.891;    
  CZpc1 = 1.0298*(0.1);
  DZpc1 = 0;
  
  m.set_xxx_z_match(0,0,0,0.6,0.7); //PC2_Z
  m.set_xxx_z_match(1,0,0,0.9,0.9); //PC3E_Z
  m.set_xxx_z_match(2,0,0,0.9,0.9); //PC3W_Z
  m.set_xxx_z_match(3,0,0,0.9,1.1); //TOF_Z
  m.set_xxx_phi_match(0,0,0,0,0,0.0017,0.0017); //PC2_P
  m.set_xxx_phi_match(1,0,0,0,0,0.002,0.002); //PC3E_P
  m.set_xxx_phi_match(2,0,0,0,0,0.002,0.002); //PC3W_P
  m.set_xxx_phi_match(3,0,0,0,0,0.0025,0.004); //TOF_P

  //EMC 
  APemc = 2.2457*(0.01);
  BPemc = 4.66;
  CPemc = 5.427*(0.001);
  DPemc = -1.61*(0.0001);
  
  AZemc = 1.21*10 ;  
  BZemc = 4.700;     
  CZemc = 2.7266; 
  DZemc = -5.99*(0.01); 
}

void mNewDchEvaluator::MatchPc1(int type)
{
  // parametrization function (for the sigma vs mom)  is  A*exp(-B*x) + C
  float momentum,pc1x,pc1y,pc1z,ppc1x,ppc1y,ppc1z;

  float pmatchPc1,poffPc1,zmatchPc1,zoffPc1;
  float deltaZ, deltaP;
  float ph,php;
  float nsigmaZ,nsigmaP,nSigmaRadius;
  int i,idRecoTrack,totalDchMcRecoTracks = mcRecoTrackList->length();
  DchMcRecoTrack* mcRecoTrack = 0;
  for (i = 0; i <  totalDchMcRecoTracks; i++){ // Dch track loop
    mcRecoTrack = (*mcRecoTrackList)[i];
    idRecoTrack = mcRecoTrack->get_recoID();
    if (idRecoTrack<0) continue;

    momentum = mcRecoTrack->get_momentumR();
    pc1x     = mcRecoTrack->get_xPc1Reco();
    pc1y     = mcRecoTrack->get_yPc1Reco();
    pc1z     = mcRecoTrack->get_zPc1Reco();
    ppc1x    = mcRecoTrack->get_xPc1Proj();
    ppc1y    = mcRecoTrack->get_yPc1Proj();
    ppc1z    = mcRecoTrack->get_zPc1Proj();
    
    pmatchPc1 = APpc1*exp(-BPpc1*momentum) + CPpc1;
    poffPc1 = DPpc1;
    zmatchPc1 = AZpc1*exp(-BZpc1*momentum) + CZpc1;
    zoffPc1 = DZpc1;
    deltaZ = pc1z - ppc1z;
    if(pc1z<-1000) deltaZ=-9999;

    ph  = atan2(pc1y,pc1x);
    php = atan2(ppc1y,ppc1x);
    deltaP = ph-php;
    if(pc1x<-1000||ppc1x<-1000) deltaP =-9999;
    else{
      if (deltaP > 3.14159) {
	deltaP = 2*3.14159 - deltaP; // 
      }else if (deltaP <-3.14159) {
	deltaP = deltaP +   2*3.14159;
      }
    }
    nsigmaZ = (deltaZ - zoffPc1)/zmatchPc1;
    nsigmaP = (deltaP - poffPc1)/pmatchPc1;
    nSigmaRadius = sqrt(nsigmaP*nsigmaP + nsigmaZ*nsigmaZ);
    
    mcRecoTrack->sigpc1  = nSigmaRadius;
    mcRecoTrack->sigpc1p = nsigmaP;
    mcRecoTrack->sigpc1z = nsigmaZ;
    mcRecoTrack->delpc1p = deltaP;
    mcRecoTrack->delpc1z = deltaZ;
  }
}

void mNewDchEvaluator::MatchPc2(int type){
  float p,pc2x,pc2y,pc2z,ppc2x,ppc2y,ppc2z;
  float deltaZ, deltaP,ph,php,alpha;
  float nsigmaZ,nsigmaP,nSigmaRadius;

  int i,idRecoTrack,totalDchMcRecoTracks = mcRecoTrackList->length();
  DchMcRecoTrack* mcRecoTrack = 0;
  for (i = 0; i <  totalDchMcRecoTracks; i++){ // Dch track loop
    mcRecoTrack = (*mcRecoTrackList)[i];
    idRecoTrack = mcRecoTrack->get_recoID();
    if (idRecoTrack<0) continue;
    pc2x     = mcRecoTrack->get_xPc2Reco();
    pc2y     = mcRecoTrack->get_yPc2Reco();
    pc2z     = mcRecoTrack->get_zPc2Reco();
    ppc2x    = mcRecoTrack->get_xPc2Proj();
    ppc2y    = mcRecoTrack->get_yPc2Proj();
    ppc2z    = mcRecoTrack->get_zPc2Proj();
    if(pc2x<-1000||ppc2x<-1000) continue;
    alpha    = mcRecoTrack->get_dalpha() + mcRecoTrack->get_alphaG();
    p = mcRecoTrack->get_momentumR();
    if(alpha>0) p = -p;

    deltaZ   = pc2z-ppc2z;
    ph  = atan2(pc2y,pc2x);
    php = atan2(ppc2y,ppc2x);
    deltaP = ph-php;
    if (deltaP > 3.14159) {
      deltaP = 2*3.14159 - deltaP; // 
    }else if (deltaP <-3.14159) {
      deltaP = deltaP +   2*3.14159;
    }
    nsigmaZ = m.d_PC2_z_match(p,deltaZ);
    nsigmaP = m.d_PC2_phi_match(p,deltaP);
    nSigmaRadius = sqrt(nsigmaP*nsigmaP + nsigmaZ*nsigmaZ);
    
    mcRecoTrack->sigpc2  = nSigmaRadius;
    mcRecoTrack->sigpc2p = nsigmaP;
    mcRecoTrack->sigpc2z = nsigmaZ;
    mcRecoTrack->delpc2p = deltaP;
    mcRecoTrack->delpc2z = deltaZ;
  }
}

void mNewDchEvaluator::MatchPc3(int type){
  float p,pc3x,pc3y,pc3z,ppc3x,ppc3y,ppc3z;
  float deltaZ, deltaP,ph,php,alpha;
  float nsigmaZ,nsigmaP,nSigmaRadius;

  int i,idRecoTrack,totalDchMcRecoTracks = mcRecoTrackList->length();
  DchMcRecoTrack* mcRecoTrack = 0;
  for (i = 0; i <  totalDchMcRecoTracks; i++){ // Dch track loop
    mcRecoTrack = (*mcRecoTrackList)[i];
    idRecoTrack = mcRecoTrack->get_recoID();
    if (idRecoTrack<0) continue;
    pc3x     = mcRecoTrack->get_xPc3Reco();
    pc3y     = mcRecoTrack->get_yPc3Reco();
    pc3z     = mcRecoTrack->get_zPc3Reco();
    ppc3x    = mcRecoTrack->get_xPc3Proj();
    ppc3y    = mcRecoTrack->get_yPc3Proj();
    ppc3z    = mcRecoTrack->get_zPc3Proj();
    if(pc3x<-1000||ppc3x<-1000) continue;
    alpha    = mcRecoTrack->get_dalpha() + mcRecoTrack->get_alphaG();
    p = mcRecoTrack->get_momentumR();
    if(alpha>0) p = -p;


    deltaZ   = pc3z-ppc3z;
    ph  = atan2(pc3y,pc3x);
    php = atan2(ppc3y,ppc3x);
    deltaP = ph-php;
    if (deltaP > 3.14159) {
      deltaP = 2*3.14159 - deltaP; // 
    }else if (deltaP <-3.14159) {
      deltaP = deltaP +   2*3.14159;
    }
    if(mcRecoTrack->get_phi()<1.57){
      nsigmaZ = m.d_PC3w_z_match(p,deltaZ);
      nsigmaP = m.d_PC3w_phi_match(p,deltaP);
    }else{
      nsigmaZ = m.d_PC3e_z_match(p,deltaZ);
      nsigmaP = m.d_PC3e_phi_match(p,deltaP);
    }
    nSigmaRadius = sqrt(nsigmaP*nsigmaP + nsigmaZ*nsigmaZ);
    
    mcRecoTrack->sigpc3  = nSigmaRadius;
    mcRecoTrack->sigpc3p = nsigmaP;
    mcRecoTrack->sigpc3z = nsigmaZ;
    mcRecoTrack->delpc3p = deltaP;
    mcRecoTrack->delpc3z = deltaZ;
  }
}
void mNewDchEvaluator::MatchTof(int type){
  float p,tofx,tofy,tofz,ptofx,ptofy,ptofz;
  float deltaZ, deltaP,ph,php,alpha;
  float nsigmaZ,nsigmaP,nSigmaRadius;

  int i,idRecoTrack,totalDchMcRecoTracks = mcRecoTrackList->length();
  DchMcRecoTrack* mcRecoTrack = 0;
  for (i = 0; i <  totalDchMcRecoTracks; i++){ // Dch track loop
    mcRecoTrack = (*mcRecoTrackList)[i];
    idRecoTrack = mcRecoTrack->get_recoID();
    if (idRecoTrack<0) continue;
    tofx     = mcRecoTrack->get_xTofReco();
    tofy     = mcRecoTrack->get_yTofReco();
    tofz     = mcRecoTrack->get_zTofReco();
    ptofx    = mcRecoTrack->get_xTofProj();
    ptofy    = mcRecoTrack->get_yTofProj();
    ptofz    = mcRecoTrack->get_zTofProj();
    if(tofx<-1000||ptofx<-1000) continue;
    alpha    = mcRecoTrack->get_dalpha() + mcRecoTrack->get_alphaG();
    p = mcRecoTrack->get_momentumR();
    if(alpha>0) p = -p;


    deltaZ   = tofz-ptofz;
    ph  = atan2(tofy,tofx);
    php = atan2(ptofy,ptofx);
    deltaP = ph-php;
    if (deltaP > 3.14159) {
      deltaP = 2*3.14159 - deltaP; // 
    }else if (deltaP <-3.14159) {
      deltaP = deltaP +   2*3.14159;
    }
    nsigmaZ = m.d_TOF_z_match(p,deltaZ);
    nsigmaP = m.d_TOF_phi_match(p,deltaP);
    nSigmaRadius = sqrt(nsigmaP*nsigmaP + nsigmaZ*nsigmaZ);   
    mcRecoTrack->sigtof  = nSigmaRadius;
    mcRecoTrack->sigtofp = nsigmaP;
    mcRecoTrack->sigtofz = nsigmaZ;
    mcRecoTrack->deltofp = deltaP;
    mcRecoTrack->deltofz = deltaZ;
  }
}

void mNewDchEvaluator::MatchEmc(int type)
{
  // parametrization function (for the sigma vs mom)  is  A*exp(-B*x) + C
  float momentum,emcx,emcy,emcz,pemcx,pemcy,pemcz ;

  float pmatchEmc,poffEmc,zmatchEmc,zoffEmc;
  float deltaZ, deltaP;
  float ph,php;
  float nsigmaZ,nsigmaP,nSigmaRadius;
  int i,idRecoTrack,totalDchMcRecoTracks = mcRecoTrackList->length();
  DchMcRecoTrack* mcRecoTrack = 0;
  for (i = 0; i <  totalDchMcRecoTracks; i++){ // Dch track loop
    mcRecoTrack = (*mcRecoTrackList)[i];
    idRecoTrack = mcRecoTrack->get_recoID();
    if (idRecoTrack<0) continue;

    momentum = mcRecoTrack->get_momentumR();
    emcx     = mcRecoTrack->get_xEmcReco();
    emcy     = mcRecoTrack->get_yEmcReco();
    emcz     = mcRecoTrack->get_zEmcReco();
    pemcx    = mcRecoTrack->get_xEmcProj();
    pemcy    = mcRecoTrack->get_yEmcProj();
    pemcz    = mcRecoTrack->get_zEmcProj();
    
    pmatchEmc = APemc*exp(-BPemc*momentum) + CPemc;
    poffEmc = DPemc;
    zmatchEmc = AZemc*exp(-BZemc*momentum) + CZemc;
    zoffEmc = DZemc;
    deltaZ = emcz - pemcz;
    if(emcz<-1000) deltaZ=-9999;

    ph  = atan2(emcy,emcx);
    php = atan2(pemcy,pemcx);
    deltaP = ph-php;
    if(emcx<-1000||pemcx<-1000) deltaP =-9999;
    else{
      if (deltaP > 3.14159) {
	deltaP = 2*3.14159 - deltaP; // 
      }else if (deltaP <-3.14159) {
	deltaP = deltaP +   2*3.14159;
      }
    }
    nsigmaZ = (deltaZ - zoffEmc)/zmatchEmc;
    nsigmaP = (deltaP - poffEmc)/pmatchEmc;

    nSigmaRadius = sqrt(nsigmaP*nsigmaP + nsigmaZ*nsigmaZ);
    
    mcRecoTrack->sigemc  = nSigmaRadius;
    mcRecoTrack->sigemcp = nsigmaP;
    mcRecoTrack->sigemcz = nsigmaZ;
    mcRecoTrack->delemcp = deltaP;
    mcRecoTrack->delemcz = deltaZ;
  }
}
PHBoolean mNewDchEvaluator::associateDchExt(PHCompositeNode *topNode){
  PHNodeIterator iter(topNode);
  PHIODataNode<TObject> *node;



  dDchTracksExtWrapper* extperf=0,*ext=0;
  dDchTracksWrapper* trk=0,*perf=0;
  dDchHitWrapper* hit=0;
  node = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","dDchTracksExtPerf");
  if(node){
    extperf = (dDchTracksExtWrapper*) node->getData();
  }
  node = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","dDchTracksExt");
  if(node){
    ext = (dDchTracksExtWrapper*) node->getData();
  }
  node = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","dDchTracks");
  if(node){
    trk = (dDchTracksWrapper*) node->getData();
  }
  node = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","dDchHit");
  if(node){
    hit = (dDchHitWrapper*) node->getData();
  }
  node = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","dDchTracksPerf");
  if(node){
    perf = (dDchTracksWrapper*) node->getData();
  }

  dcghitWrapper* dcghit=0;
  fkinWrapper* fkin=0;
  node = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","dcghit");
  if(node){
    dcghit = (dcghitWrapper*)node->getData();
  }
  node = (PHIODataNode<TObject>*)iter.findFirst("PHIODataNode","fkin");
  if(node){
    fkin = (fkinWrapper*)node->getData();
  }
  DchMcRecoTrack* mcRecoTrack = 0,*mcRecoTrack1 = 0;
  int i,m,idRecoTrack,perfID,idGeant,idGeant1,idGeant2,gen,gen1,true_track_parent,num;
  unsigned int j,k;
  int fkinindex[100],idGeantindex[100];
  int totalDchMcRecoTracks = mcRecoTrackList->length();
  int hitid,plane;

  if(ext){
    for(i=0;i<totalDchMcRecoTracks;i++){
      mcRecoTrack = (*mcRecoTrackList)[i];
      idRecoTrack = mcRecoTrack->get_recoID();
      if (idRecoTrack>=0){
	mcRecoTrack->alpha1 = ext->get_alpha1(idRecoTrack);
	mcRecoTrack->alpha2 = ext->get_alpha2(idRecoTrack);
	mcRecoTrack->nx1    = ext->get_nx1hits(idRecoTrack);
	mcRecoTrack->nx2    = ext->get_nx2hits(idRecoTrack);
	mcRecoTrack->mdist1 = ext->get_dist1(idRecoTrack);
	mcRecoTrack->mdist2 = ext->get_dist2(idRecoTrack);
	mcRecoTrack->chi21  = ext->get_chi21(idRecoTrack);
	mcRecoTrack->chi22  = ext->get_chi22(idRecoTrack);
      }
    }
  }
  if(extperf){
    for(i=0;i<totalDchMcRecoTracks;i++){
      mcRecoTrack = (*mcRecoTrackList)[i];
      perfID      = mcRecoTrack->get_perfID();
      if(perfID>=0){
	mcRecoTrack->alpha1G = extperf->get_alpha1(perfID);
	mcRecoTrack->alpha2G = extperf->get_alpha2(perfID);
	mcRecoTrack->nx1G    = extperf->get_nx1hits(perfID);
	mcRecoTrack->nx2G    = extperf->get_nx2hits(perfID);
	mcRecoTrack->mdist1G = extperf->get_dist1(perfID);
	mcRecoTrack->mdist2G = extperf->get_dist2(perfID);
	mcRecoTrack->chi21G  = extperf->get_chi21(perfID);
	mcRecoTrack->chi22G  = extperf->get_chi22(perfID);     
      }
    }
  }
  if(!extperf||!ext)return False;

  //fill parent/decay information from fkin
  for(i=0;i<totalDchMcRecoTracks;i++){
    mcRecoTrack = (*mcRecoTrackList)[i];
    idRecoTrack = mcRecoTrack->get_recoID();
    perfID      = mcRecoTrack->get_perfID();
    idGeant     = mcRecoTrack->get_idGeantTrack();
    gen         = mcRecoTrack->get_generation();
    if(idRecoTrack>=0){//fill hits info
      for(j=0;j<40;j++){
	hitid= trk->get_hits(j,idRecoTrack);
	if(hitid>=0){
	  plane = hit->get_plane(hitid);
	  mcRecoTrack->hitid[plane] = hitid;
	} 
      }
    }
    if(idGeant>=0&&perfID>=0){//fill geant info for parent/decays
      if(gen==1){//primary do nothing
      }else{
	true_track_parent =-1;
	for(j=0;j<fkin->RowCount();j++){
	  if(fkin->get_true_track(j) == idGeant){//find parent
	    //fill parent information
	    true_track_parent    = fkin->get_itparent(j);
	    break;
	  }
	}
	if(true_track_parent>=0){
	  num =0;
	  for(j=0;j<fkin->RowCount();j++){
	    if(fkin->get_true_track(j) == true_track_parent){//find parent
	    //fill parent information
	      mcRecoTrack->_idG0   = true_track_parent;
	      mcRecoTrack->_pid0   = fkin->get_idpart(j);
	      mcRecoTrack->_idpare0= fkin->get_idparent(j) ;
	      mcRecoTrack->_p0     = fkin->get_ptot(j);
	      mcRecoTrack->_pthe0  = fkin->get_pthet(j);
	      mcRecoTrack->_pphi0  = fkin->get_pphi(j);
	      if(mcRecoTrack->_idpare0==0) gen1 =1;
	      else if(mcRecoTrack->_idpare0>0) gen1 =2;
	      else gen1 =3;
	      mcRecoTrack->_gen0   = gen1;
	      break;
	    }
	  }
	  //fill other decay particle information
	  for(j=0;j<fkin->RowCount();j++){
	    if(fkin->get_itparent(j) == true_track_parent&&fkin->get_true_track(j)!=idGeant){
	      fkinindex[num] = j;
	      idGeantindex[num] = fkin->get_true_track(j);
	      num++;
	    }
	  }
	  if(num> 0){//fill first one
	    j = fkinindex[0];
	    mcRecoTrack->_idG1      = fkin->get_true_track(j); 
	    mcRecoTrack->_p1        = fkin->get_ptot(j); 
	    mcRecoTrack->_pthe1     = fkin->get_pthet(j); 
	    mcRecoTrack->_pphi1     = fkin->get_pphi(j); 
	    mcRecoTrack->_pid1      = fkin->get_idpart(j); 
	  }
	  if(num>1){
	    j = fkinindex[1];
	    mcRecoTrack->_idG2      = fkin->get_true_track(j); 
	    mcRecoTrack->_p2        = fkin->get_ptot(j); 
	    mcRecoTrack->_pthe2     = fkin->get_pthet(j); 
	    mcRecoTrack->_pphi2     = fkin->get_pphi(j); 
	    mcRecoTrack->_pid2      = fkin->get_idpart(j); 
	  }
	  //fill other info from perftrack
	  for(k=0;k<perf->RowCount();k++){
	    idGeant1=-1;
	    for (m=0; m<40; m++) {  
	      int dcghitID = perf->get_hits(m,k);
	      if (dcghitID >-1) {
		idGeant1 = dcghit->get_mctrack(m); // for main contributor Analysis
		break;
	      }
	      
	      if(num>0&&idGeant1 == idGeantindex[0]){
		mcRecoTrack->_Q1    = perf->get_quality(k); 
		mcRecoTrack->_phi1  = perf->get_phi(k); 
		mcRecoTrack->_zed1  = perf->get_zed(k); 
		mcRecoTrack->_nx1G1 = extperf->get_nx1hits(k); 
		mcRecoTrack->_nx2G1 = extperf->get_nx2hits(k); 
		mcRecoTrack->_alp1G1 = extperf->get_alpha1(k); 
		mcRecoTrack->_alp2G1 = extperf->get_alpha2(k); 
		for(m=0;m<totalDchMcRecoTracks;m++){
		  mcRecoTrack1 = (*mcRecoTrackList)[m];
		  idGeant2     = mcRecoTrack1->get_idGeantTrack();
 		  if(idGeant1 == idGeant2){
		    mcRecoTrack->_nx11        = mcRecoTrack1->nx1; 
		    mcRecoTrack->_nx21        = mcRecoTrack1->nx2; 
		    mcRecoTrack->_xPc1Reco1   = mcRecoTrack1->get_xPc1Reco(); 
		    mcRecoTrack->_yPc1Reco1   = mcRecoTrack1->get_yPc1Reco(); 
		    mcRecoTrack->_zPc1Reco1   = mcRecoTrack1->get_zPc1Reco(); 
		    mcRecoTrack->_xPc2Reco1   = mcRecoTrack1->get_xPc2Reco(); 
		    mcRecoTrack->_yPc2Reco1   = mcRecoTrack1->get_yPc2Reco(); 
		    mcRecoTrack->_zPc2Reco1   = mcRecoTrack1->get_zPc2Reco(); 
		    mcRecoTrack->_xPc3Reco1   = mcRecoTrack1->get_xPc3Reco(); 
		    mcRecoTrack->_yPc3Reco1   = mcRecoTrack1->get_yPc3Reco(); 
		    mcRecoTrack->_zPc3Reco1   = mcRecoTrack1->get_zPc3Reco(); 
		    mcRecoTrack->_xPc1Mc1     = mcRecoTrack1->get_xPc1Mc(); 
		    mcRecoTrack->_yPc1Mc1     = mcRecoTrack1->get_yPc1Mc(); 
		    mcRecoTrack->_zPc1Mc1     = mcRecoTrack1->get_zPc1Mc(); 
		    mcRecoTrack->_xPc2Mc1     = mcRecoTrack1->get_xPc2Mc(); 
		    mcRecoTrack->_yPc2Mc1     = mcRecoTrack1->get_yPc2Mc(); 
		    mcRecoTrack->_zPc2Mc1     = mcRecoTrack1->get_zPc2Mc(); 
		    mcRecoTrack->_xPc3Mc1     = mcRecoTrack1->get_xPc3Mc(); 
		    mcRecoTrack->_yPc3Mc1     = mcRecoTrack1->get_yPc3Mc(); 
		    mcRecoTrack->_zPc3Mc1     = mcRecoTrack1->get_zPc3Mc(); 
		  }
		}
	      }
	      if(num>1&&idGeant1 == idGeantindex[1]){
		mcRecoTrack->_Q2    = perf->get_quality(k); 
		mcRecoTrack->_phi2  = perf->get_phi(k); 
		mcRecoTrack->_zed2  = perf->get_zed(k); 
		mcRecoTrack->_nx1G2 = extperf->get_nx1hits(k); 
		mcRecoTrack->_nx2G2 = extperf->get_nx2hits(k); 
		mcRecoTrack->_alp1G2 = extperf->get_alpha1(k); 
		mcRecoTrack->_alp2G2 = extperf->get_alpha2(k); 
		for(m=0;m<totalDchMcRecoTracks;m++){
		  mcRecoTrack1 = (*mcRecoTrackList)[m];
		  idGeant2     = mcRecoTrack1->get_idGeantTrack();
 		  if(idGeant1 == idGeant2){
		    mcRecoTrack->_nx12        = mcRecoTrack1->nx1; 
		    mcRecoTrack->_nx22        = mcRecoTrack1->nx2; 
		    mcRecoTrack->_xPc1Reco2   = mcRecoTrack1->get_xPc1Reco(); 
		    mcRecoTrack->_yPc1Reco2   = mcRecoTrack1->get_yPc1Reco(); 
		    mcRecoTrack->_zPc1Reco2   = mcRecoTrack1->get_zPc1Reco(); 
		    mcRecoTrack->_xPc2Reco2   = mcRecoTrack1->get_xPc2Reco(); 
		    mcRecoTrack->_yPc2Reco2   = mcRecoTrack1->get_yPc2Reco(); 
		    mcRecoTrack->_zPc2Reco2   = mcRecoTrack1->get_zPc2Reco(); 
		    mcRecoTrack->_xPc3Reco2   = mcRecoTrack1->get_xPc3Reco(); 
		    mcRecoTrack->_yPc3Reco2   = mcRecoTrack1->get_yPc3Reco(); 
		    mcRecoTrack->_zPc3Reco2   = mcRecoTrack1->get_zPc3Reco(); 
		    mcRecoTrack->_xPc1Mc2     = mcRecoTrack1->get_xPc1Mc(); 
		    mcRecoTrack->_yPc1Mc2     = mcRecoTrack1->get_yPc1Mc(); 
		    mcRecoTrack->_zPc1Mc2     = mcRecoTrack1->get_zPc1Mc(); 
		    mcRecoTrack->_xPc2Mc2     = mcRecoTrack1->get_xPc2Mc(); 
		    mcRecoTrack->_yPc2Mc2     = mcRecoTrack1->get_yPc2Mc(); 
		    mcRecoTrack->_zPc2Mc2     = mcRecoTrack1->get_zPc2Mc(); 
		    mcRecoTrack->_xPc3Mc2     = mcRecoTrack1->get_xPc3Mc(); 
		    mcRecoTrack->_yPc3Mc2     = mcRecoTrack1->get_yPc3Mc(); 
		    mcRecoTrack->_zPc3Mc2     = mcRecoTrack1->get_zPc3Mc(); 
		  }
		}
	      }	
	    }
	  }
	}
      }
    }
  }
  return True;
}

PHBoolean mNewDchEvaluator::associateMatching(){

  MatchPc1();
  MatchPc2();
  MatchPc3();
  MatchTof();
  MatchEmc();
  return true;  
}



