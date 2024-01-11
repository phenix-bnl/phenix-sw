// -----------------------------------------
// Created by:  Jiangyong Jia, Ralf Averbeck
//------------------------------------------

#include "PHEmbedMCEvaluator.hh"
#include "BbcOut.h"
#include "CglTrack.h"
#include "CrkPID.hh"
#include "dEmcGeaClusterTrackWrapper.h"
#include "PHTrackOut.h"
#include "ZdcOut.h"
#include "PHGlobal.h"
#include "PHEmbedHistogrammer.hh"
#include "fkinWrapper.h"
#include "primaryWrapper.h"
#include "DchTrackCandidate.hh"
#include "PHEmbedMcRecoTrack.hh"
#include "PHEmbededEvent.hh"
#include "PHDchTrackOut.h"
#include "PHTrackOut.h"
#include "PHTrackOutv8.h"
#include "dcghitWrapper.h"
#include "dDchTracksWrapper.h"
#include "DchTrack.h"
#include "DchHitLineTable.hh"
#include "pcghitWrapper.h"
#include "PadCluster.h"
#include "dTofGdigiWrapper.h"
#include "dTofGdigiRecWrapper.h" 
#include "TofOut.h"
#include "AccRaw.h"
#include "AccProj.h"
#include "TecOutV1.hh"
#include "Bbc.hh"
#include "emcClusterContainer.h"
#include "emcClusterContent.h"
#include "utiMatch.h"
#include "TofwHit.h"
#include <string>
#include <iostream>

using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;
typedef PHIODataNode<fkinWrapper> fkinNode_t;
typedef PHDataNode<PHEmbededEvent>  PHEmbededEventNode_t;
typedef PHIODataNode<dDchHitWrapper> dDchHitNode_t;
typedef PHIODataNode<dDchGhitHitsWrapper> dDchGhitHitsNode_t;
typedef PHIODataNode<dDchTracksWrapper> dDchTracksNode_t;

ostream& operator<<(ostream& os, const trkPair &p) {
  return (os << "( " << p.trkG 
	  << ", " << p.trkS 
	  << ", " << p.solutionS 
	  << ", " << p.sumfoundS
	  << ", " << p.trkR 
	  << ", " << p.solutionR 
	  << ", " << p.sumfoundR << ") ");
}

// GEANT particle masses. The index is the GEANT code - 1.
static float MASS[18] ={ 0,          0.000511,   0.000511,   
			 0,          0.105658,   0.105658,
			 0.1349764,  0.13957,    0.13957,   
			 0.497672,   0.493677,   0.493677,
			 0.93956563, 0.93827231, 0.93827231, 
			 0.497672,   0.54745,    1.115684 };

PHEmbedMCEvaluator::PHEmbedMCEvaluator() {
  mat   = new utiMatch;
  LoadMatchPar();
  node1 = 0;
  node2 = 0;
  node3 = 0;
  verbose =0;
  mainContributorAnalysis=1;
  d_crkpid = new CrkPID(-1);
  
  embedEvent = 0;
  trackCandidateList = 0;
  mcRecoTrackList = 0;
}

PHEmbedMCEvaluator::~PHEmbedMCEvaluator() {

  delete mat;
  delete d_crkpid;

}

// The comment in utiMatch.h says "run2 functions"
//!!! Should these parameters be updated?
void PHEmbedMCEvaluator::LoadMatchPar() {
  // See offline/packages/uti for default pars, which come from
  // 2-Gaussian fits to raw (dimension-ful) matching variables. 2
  // Gaussians because there is a narrow peak from detector DCH
  // pointing & PC spatial resolution, plus a broad peak from
  // multi-scattering. See eq. 3.5 of Jiangyong's thesis.

  // args for set_xxx_z_match():
  // int detector, float mean1, float mean2,
  // float sig1, float sig2
  mat->set_xxx_z_match(0,0,0,0.6,0.7); // PC2_Z 
  mat->set_xxx_z_match(1,0,0,0.9,0.9); // PC3E_Z
  mat->set_xxx_z_match(2,0,0,0.9,0.9); // PC3W_Z
  mat->set_xxx_z_match(3,0,0,0.9,1.1); // TOF_Z

  // args for set_xxx_phi_match()
  // int detector, float mplus1,float mplus2,float mminus1,
  // float mminus2, float sig1,float sig2
  mat->set_xxx_phi_match(0,0,0,0,0,0.0017,0.0017); // PC2_P
  mat->set_xxx_phi_match(1,0,0,0,0,0.002,0.002);   // PC3E_P
  mat->set_xxx_phi_match(2,0,0,0,0,0.002,0.002);   // PC3W_P
  mat->set_xxx_phi_match(3,0,0,0,0,0.0025,0.004);  // TOF_P
}

PHBoolean PHEmbedMCEvaluator::event(PHCompositeNode* sngl,
				    PHCompositeNode* real,
				    PHCompositeNode* merged) {

  if ((!sngl)||(!real)||(!merged)) {
    cout << PHWHERE << "Single, real, or merged node tree missing." 
	 << endl;
    return False;
  }

  node1 = sngl;
  node2 = real;
  node3 = merged;
  isminbias = 1;
  accepted = 1;

  PHNodeIterator iter(node3);
  PHEmbededEventNode_t *embedEventNode = 
    (PHEmbededEventNode_t*)iter.findFirst("PHDataNode", "PHEmbededEvent");

  if (!embedEventNode) {
    PHMessage("PHEmbedMCEvaluator::event", PHError, 
	      "PHEmbededEvent missing in the tree");
    return False;
  }

  embedEvent = embedEventNode->getData();

  PHDataNode<PHPointerList<PHEmbedMcRecoTrack> >* tmpEmbedMcRecoTrackNode;

  // Why does the node have "TMP" in its name?
  PHCompositeNode* tmpNode = 
    static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DCHTMP"));

  if (!tmpNode) {
    tmpNode = new PHCompositeNode("DCHTMP");
    node3->addNode(tmpNode);
  }

  // Look for the mc track node. If it does not exist, add it.
  tmpEmbedMcRecoTrackNode = 
    (PHDataNode<PHPointerList<PHEmbedMcRecoTrack> >*)iter.findFirst("PHDataNode", 
								    "PHEmbedMcRecoTrack");
  if (!tmpEmbedMcRecoTrackNode) {
    mcRecoTrackList = new PHPointerList<PHEmbedMcRecoTrack>(100);
    tmpEmbedMcRecoTrackNode = 
      new PHDataNode<PHPointerList<PHEmbedMcRecoTrack> >(mcRecoTrackList, 
							 "PHEmbedMcRecoTrack");
    tmpNode->addNode(tmpEmbedMcRecoTrackNode);
  }
  else {
    // Why assign data to the list then immediately clear it?
    mcRecoTrackList = tmpEmbedMcRecoTrackNode->getData();
    mcRecoTrackList->clearAndDestroy(); 
  }

  return True;
}

PHBoolean 
PHEmbedMCEvaluator::mainContributorCalculationBetweenReconstructionAndGeant() {//RG
  DchTrack* dchtrack3       = embedEvent->get_dchtrack3();
  DchHitLineTable* dchhit3  = embedEvent->get_dchit3();
  
  int numOfPerfTrack1 = embedEvent->get_dchperftrack1()->RowCount();
  int numOfDchTrack3  = embedEvent->get_dchtrack3()->get_DchNTrack();
  int recoid,perid;

  //reset variables  
  for (recoid =0 ;recoid<numOfDchTrack3+1;recoid++) {
    mulmainContributorRG[recoid]     = -1;
    xmulmainContributorRG[recoid]    = -1;
    uvmulmainContributorRG[recoid]   = -1;
  }

  for (perid=0;perid<numOfPerfTrack1;perid++) {
    for (recoid =0 ;recoid<numOfDchTrack3+1;recoid++) {
      TypemainContributorRG[perid][recoid]      = 0;
      puritymainContributorRG[perid][recoid]    = 0;
      xpuritymainContributorRG[perid][recoid]   = 0;
      uvpuritymainContributorRG[perid][recoid]  = 0;
      x1countmainContributorRG[perid][recoid]   = 0;
      x2countmainContributorRG[perid][recoid]   = 0;
      uvcountmainContributorRG[perid][recoid]   = 0;
    }
  }
  
  short perfHitsForRecoTrack[numOfPerfTrack1][numOfDchTrack3];
  int xhitsG=0, uvhitsG=0;
  int xhitsR=0, uvhitsR=0, allR=0;
  int xhitsAE=0,uvhitsAE=0;

  for (perid=0; perid<numOfPerfTrack1; perid++) {
    for (recoid =0; recoid<numOfDchTrack3; recoid++) {
      perfHitsForRecoTrack[perid][recoid] = 0;
    }
  }

  // 
  for (recoid=0; recoid<numOfDchTrack3; recoid++) {
    int perftrackid;
    for (int plane=0; plane<39; plane++) {
      int hitid = dchtrack3->get_hits(recoid,plane);
      if (hitid>-1) {
	int ghitid;
	if ((ghitid = embedEvent->recohit2Geanthit(hitid))>=0) {//is a merged hit
	  perftrackid = embedEvent->geanthit2Perfecttrack(ghitid);
	  if (verbose>5)
	    if (perftrackid != embedEvent->recohit2Perfecttrack(hitid)) 
	      cout<<"Not equal"<<endl; 
	  if (perftrackid>=numOfPerfTrack1) 
	    cout<<"Serious error, something worong with perfect tracks"<<endl;
	  else if (perftrackid>-1)perfHitsForRecoTrack[perftrackid][recoid]++;
	}
      }
    }
  }

  int counter,xcounter,uvcounter,uvcounter1,
    counterb,xcounterb,uvcounterb,main,i,j;
  
  for (perid=0;perid<numOfPerfTrack1;perid++) {
    xhitsG   = 
      embedEvent->get_x1hPerfectTrack(perid) + 
      embedEvent->get_x2hPerfectTrack(perid);
    uvhitsG  = 
      embedEvent->get_uv1hPerfectTrack(perid) + 
      embedEvent->get_uv2hPerfectTrack(perid);
    xhitsAE  = 
      embedEvent->get_x1hAfterEmbedForPerfectTrack(perid) + 
      embedEvent->get_x2hAfterEmbedForPerfectTrack(perid);
    uvhitsAE = 
      embedEvent->get_uv1hAfterEmbedForPerfectTrack(perid) + 
      embedEvent->get_uv2hAfterEmbedForPerfectTrack(perid);

    int x1=0, x2=0, total=0, xtotal=0, uvtotal=0;
    int max=0, xmax=0, uvmax=0;
    int mul=0, xmul=0, uvmul=0;  
    int UVHIT[60], count=0, found=0;
    
    for (i=0;i<60;i++) UVHIT[i]=-1;
    
    for (i =0 ;i<numOfDchTrack3;i++) {
      if (perfHitsForRecoTrack[perid][i]<1)continue;
    
      counter=0;
      xcounter=0;
      uvcounter=0;
      uvcounter1=0;
      counterb=0;
      xcounterb=0;
      uvcounterb=0;
      x1=0;
      x2=0;

      // Loop over DCH planes to compare R and G hits
      for (int plane=0; plane<39; plane++) {
	int hitid =  dchtrack3->get_hits(i,plane); // R hit index
	if (hitid>-1) {
	  int realplane = dchhit3->getPlane(hitid);
	  int type = wireType[realplane];
	  
	  if (embedEvent->recohit2Perfecttrack(hitid) == perid) {
	    counter++; // number of merged hits
	    if (type==X1Wire) x1++;
	    else if (type == X2Wire) x2++;
	    else {
	      uvcounter++;
	      found=0;
	      for (j=0;j<count;j++) {
		if (UVHIT[j]==hitid) {
		  found=1;
		  break;
		}
	      }
	      if (!found) {
		UVHIT[count]=hitid;
		uvcounter1++;
		count++;
	      }
	    }
	  }else{ // count un-merged hits with counterb
	    counterb++;
	    if (type==X1Wire || type==X2Wire) xcounterb++;
	    else uvcounterb++;
	  }
	}
      }
      xcounter = x1 + x2;
      if (xcounter>xmax) xmax = xcounter;
      if (uvcounter>uvmax) uvmax = uvcounter;
      if (xcounter+uvcounter>max) max = xcounter + uvcounter;
      xtotal   += xcounter;
      uvtotal  += uvcounter1;
      xhitsR    = xcounter + xcounterb;
      uvhitsR   = uvcounter+ uvcounterb;
      allR      = xhitsR + uvhitsR;
      puritymainContributorRG[perid][i]      = 0;
      xpuritymainContributorRG[perid][i]     = 0;
      uvpuritymainContributorRG[perid][i]    = 0;
      if (allR)     puritymainContributorRG[perid][i] = ((float)counter)/allR;
      if (xhitsR)  xpuritymainContributorRG[perid][i] = ((float)xcounter)/xhitsR;
      if (uvhitsR)uvpuritymainContributorRG[perid][i] = ((float)uvcounter)/uvhitsR;
      main = 0;

      // Here is the actual main contributor comparison...
      // If >= half the hits in a track are from GEANT,
      // GEANT track is the main contributor.

      // Lowest bit: geant track is this track's main contributor
      if (counter>=counterb && counter>0) main |= 0x01;
      // 2nd LSB: geant  track is this track's x main contributor
      if (xcounter>=xcounterb && xcounter>0) main |= 0x02;
      // 3rd LSB: geant  track is this track's uv main contributor
      if (uvcounter>=uvcounterb && uvcounter>0) main |= 0x04;

      TypemainContributorRG[perid][i]    = main;
      x1countmainContributorRG[perid][i] = x1;
      x2countmainContributorRG[perid][i] = x2;
      uvcountmainContributorRG[perid][i] = uvcounter;
      mul++;
      if (xcounter>0)  xmul++;
      if (uvcounter>0) uvmul++;
      if (verbose>10)
	cout << "G->R main contrib. fn: DCH track " << i 
	     << " has " << x1 << " x1hits and " << x2 << " x2hits. \n" 
	     << perfHitsForRecoTrack[perid][i] << "  GEANT hits, " 
	     << xcounter + uvcounter  << " total hits." << endl;    
    }

    // Looks like this never gets executed...
    if (0) {//total>0
      //the last entry is for not associated hits;
      puritymainContributorRG[perid][numOfDchTrack3]      = 0;
      xpuritymainContributorRG[perid][numOfDchTrack3]     = 0;
      uvpuritymainContributorRG[perid][numOfDchTrack3]    = 0;
      TypemainContributorRG[perid][numOfDchTrack3]        = 0;
      mul++;
      if (xtotal>0)  xmul++;
      if (uvtotal>0) uvmul++;    
    }

    if (verbose>5) 
      cout << "mulCollectors " << mul << " " << xmul << " " << uvmul << endl;

    // Total number of unassociated hits of the geant particle (after merging)
    xtotal = xhitsAE-xtotal;
    uvtotal= uvhitsAE-uvtotal;

    if (xtotal<0 || uvtotal<0) {//uvtotal can be less than 0; because they are shared
      cout<<"PHEmbedMCEvaluatorRG: Warning xtotal<0 || uvtotal<0"<<endl;
    }

    if (verbose>20) {
      cout << "***RG*** xhitsG: "     << xhitsG  
	   << ", xhitsAE: "  << xhitsAE  
	   << ", xtotal: "   << xtotal
	   << ", uvhitsG: "  << uvhitsG 
	   << ", uvhitsAE: " << uvhitsAE 
	   << ", uvtotal: "  << uvtotal << endl;
    }
    total = xtotal+uvtotal;
    if (xtotal>xmax)   xmax = xtotal;
    if (uvtotal>uvmax) uvmax = uvtotal;
    if (total>max)     max = total;
  }
  
  //here contributors only includes pertracks
  int typeMain;
  for (perid=0;perid<numOfPerfTrack1;perid++) {
    int mul=0, xmul=0, uvmul=0;
    for ( recoid=0;recoid<numOfDchTrack3;recoid++) {
      typeMain = TypemainContributorRG[perid][recoid];
      if (typeMain&0x01) mul++;
      if (typeMain&0x02) xmul++;
      if (typeMain&0x04) uvmul++;
    }
    mulmainContributorRG[perid]     = mul;
    xmulmainContributorRG[perid]    = xmul;
    uvmulmainContributorRG[perid]   = uvmul;
  }

  return True;
}

PHBoolean 
PHEmbedMCEvaluator::mainContributorCalculationBetweenSimulationAndGeant() {//SG
  DchTrack*           dchtrack1       =   embedEvent->get_dchtrack1();
  DchHitLineTable*    dchhit1         =   embedEvent->get_dchit1();

  int numOfPerfTrack1               =   embedEvent->get_dchperftrack1()->RowCount();
  int numOfDchTrack1                =   embedEvent->get_dchtrack1()->get_DchNTrack();
  int simuid,perid;
  for (simuid =0 ;simuid<numOfDchTrack1+1;simuid++) {
    mulmainContributorSG[simuid]     = -1;
    xmulmainContributorSG[simuid]    = -1;
    uvmulmainContributorSG[simuid]   = -1;
  }
  for (perid=0;perid<numOfPerfTrack1;perid++) {
    for (simuid =0 ;simuid<numOfDchTrack1+1;simuid++) {
      TypemainContributorSG[perid][simuid]      = 0;
      puritymainContributorSG[perid][simuid]    = 0;
      xpuritymainContributorSG[perid][simuid]   = 0;
      uvpuritymainContributorSG[perid][simuid]  = 0;
      x1countmainContributorSG[perid][simuid]   = 0;
      x2countmainContributorSG[perid][simuid]   = 0;
      uvcountmainContributorSG[perid][simuid]   = 0;
    }
  }
  
  int perfHitsForSimulatedTrack[numOfPerfTrack1][numOfDchTrack1];
  int xhitsG=0, uvhitsG=0;
  int xhitsS=0, uvhitsS=0, allS=0;
  int xhitsBE=0,uvhitsBE=0;

  for (perid=0;perid<numOfPerfTrack1;perid++) {
    for (simuid =0 ;simuid<numOfDchTrack1;simuid++) {
      perfHitsForSimulatedTrack[perid][simuid] = 0;
    }
  }
  for (simuid =0 ;simuid<numOfDchTrack1;simuid++) {
    int perftrackid;
    for (int plane =0;plane<39;plane++) {
      int hitid = dchtrack1->get_hits(simuid,plane);
      if (hitid>-1) {
	int ghitid;
	if ((ghitid = embedEvent->simuhit2Geanthit(hitid))>=0) {//is a merged hit
	  perftrackid = embedEvent->geanthit2Perfecttrack(ghitid);
	  if (perftrackid>=numOfPerfTrack1)
	    cout << "SG: Serious error, something worong with perfect tracks" << endl;
	  else if (perftrackid>-1) perfHitsForSimulatedTrack[perftrackid][simuid]++;
	}
      }
    }
  }
  
  for (perid=0;perid<numOfPerfTrack1;perid++) {
    xhitsG   = 
      embedEvent->get_x1hPerfectTrack(perid)   + 
      embedEvent->get_x2hPerfectTrack(perid);
    uvhitsG  = 
      embedEvent->get_uv1hPerfectTrack(perid)  + 
      embedEvent->get_uv2hPerfectTrack(perid);
    xhitsBE  = 
      embedEvent->get_x1hBeforeEmbedForPerfectTrack(perid)  + 
      embedEvent->get_x2hBeforeEmbedForPerfectTrack(perid);
    uvhitsBE = 
      embedEvent->get_uv1hBeforeEmbedForPerfectTrack(perid) + 
      embedEvent->get_uv2hBeforeEmbedForPerfectTrack(perid);

    int    counter,xcounter,uvcounter,counterb,xcounterb,uvcounterb,main;
    int    x1=0,x2=0,total=0,xtotal=0,uvtotal=0;
    int    max=0,xmax=0,uvmax=0;
    int    UVHIT[60],count=0,found=0;
    for (int i=0;i<60;i++) UVHIT[i]=-1;
    
    for (int i =0 ;i<numOfDchTrack1;i++) {
      if (perfHitsForSimulatedTrack[perid][i]<1) continue;
      counter=0;
      xcounter=0;
      uvcounter=0;
      for (int plane=0; plane<39; plane++) {
	int hitid =  dchtrack1->get_hits(i,plane);
	if (hitid>-1) {
	  int realplane = dchhit1->getPlane(hitid);
	  int type = wireType[realplane];

	  if (embedEvent->simuhit2Perfecttrack(hitid)==perid) {
	    counter++;
	    if (type==X1Wire||type == X2Wire) xcounter++;
	    else {
	      found=0;
	      for (int j=0;j<count;j++) {
		if (UVHIT[j]==hitid) {
		  found=1;
		  break;
		}
	      }
	      if (!found) {
		UVHIT[count]=hitid;
		uvcounter++;
		count++;
	      }
	    }
	  }
	}
      }
      if (xcounter>xmax) xmax = xcounter;
      if (uvcounter>uvmax) uvmax = uvcounter;
      if (xcounter+uvcounter>max) max = xcounter + uvcounter;
      xtotal  += xcounter;
      uvtotal += uvcounter;
    }

    // Total number of unassociated hits of the geant particle(after merging)
    xtotal = xhitsBE-xtotal;
    uvtotal= uvhitsBE-uvtotal;
    if (xtotal<0 || uvtotal<0) {//uvtotal can be less than 0; because they are shared
      cout << "PHEmbedMCEvaluatorSG: Warning! xtotal<0 || uvtotal<0" << endl;
    }

    if (verbose>20) {
      cout << "***SG*** xhitsG: "     << xhitsG  
	   << ", xhitsBE: "  << xhitsBE  
	   << ", xtotal: "   << xtotal
	   << ", uvhitsG: "  << uvhitsG 
	   << ", uvhitsBE: " << uvhitsBE 
	   << ", uvtotal: "  << uvtotal << endl;
    }

    total = xtotal+uvtotal;
    if (xtotal>xmax)     xmax = xtotal;
    if (uvtotal>uvmax)   uvmax = uvtotal;
    if (total>max)       max = total;

    int mul=0, xmul=0, uvmul=0;  
    for (int i=0;i<numOfDchTrack1;i++) {
      if (perfHitsForSimulatedTrack[perid][i]<1) continue;
      counter=0;
      xcounter=0;
      uvcounter=0;
      counterb=0;
      xcounterb=0;
      uvcounterb=0;
      x1=0;
      x2=0;
      for (int plane=0; plane<39; plane++) {
	int hitid = dchtrack1->get_hits(i,plane);
	if (hitid>-1) {
	  int realplane = dchhit1->getPlane(hitid);
	  int type = wireType[realplane];
	  if (embedEvent->simuhit2Perfecttrack(hitid) == perid) {
	    counter++;
	    if (type==X1Wire) x1++;
	    else if (type == X2Wire) x2++;
	    else uvcounter++;
	  }else{ // calculate contribution from event.
	    counterb++;
	    if (type==X1Wire || type==X2Wire) xcounterb++;
	    else uvcounterb++;
	  }
	}
      }
      xcounter = x1 + x2;
      if (xcounter>xmax) xmax = xcounter;
      if (uvcounter>uvmax) uvmax = uvcounter;
      if (xcounter+uvcounter>max) max = xcounter + uvcounter;
      xhitsS    = xcounter + xcounterb;
      uvhitsS   = uvcounter+ uvcounterb;
      allS      = xhitsS + uvhitsS;
      puritymainContributorSG[perid][i]      = 0;
      xpuritymainContributorSG[perid][i]     = 0;
      uvpuritymainContributorSG[perid][i]    = 0;
      if (allS)
	puritymainContributorSG[perid][i] = ((float)counter)/allS;
      if (xhitsS)
	xpuritymainContributorSG[perid][i] = ((float)xcounter)/xhitsS;
      if (uvhitsS)
	uvpuritymainContributorSG[perid][i] = ((float)uvcounter)/uvhitsS;
      
      main = 0;
      // geant  track is this track's main contributor
      if (counter>=counterb&&counter>0) main |= 0x01;
      // geant  track is this track's x main contributor
      if (xcounter>=xcounterb&&xcounter>0)main |= 0x02;
      // geant  track is this track's uv main contributor
      if (uvcounter>=uvcounterb&&uvcounter>0)main |= 0x04;
  
      TypemainContributorSG[perid][i]    = main;
      x1countmainContributorSG[perid][i] = x1;
      x2countmainContributorSG[perid][i] = x2;
      uvcountmainContributorSG[perid][i] = uvcounter;
      mul++;
      if (xcounter>0)  xmul++;
      if (uvcounter>0) uvmul++;

      if (verbose>10)
	cout << "G->S main contrib. calc: DCH track " << i 
	     << " has " << x1 << " x1hits and " << x2 << " x2hits. \n" 
	     << " total: " << xcounter + uvcounter << " hits." << endl;    

    }

    // Again, not getting executed...
    if (0) {//total>0
    //the last entry is for not associated hits;
      puritymainContributorSG[perid][numOfDchTrack1]       = 0;
      xpuritymainContributorSG[perid][numOfDchTrack1]      = 0;
      uvpuritymainContributorSG[perid][numOfDchTrack1]     = 0;
      TypemainContributorSG[perid][numOfDchTrack1]         = 0;
      mul++;
      if (xtotal>0)  xmul++;
      if (uvtotal>0) uvmul++;    
    }

    if (verbose>5)
      cout << "SG: mul = " << mul << " xmul = " << xmul << " uvmul = " << uvmul << endl;
  } // End loop over perfect tracks

  //here contributors only includes pertracks
  int typeMain;
  for (perid=0;perid<numOfPerfTrack1;perid++) {
    int mul=0, xmul=0, uvmul=0;  
    for (simuid=0;simuid<numOfDchTrack1;simuid++) {
      typeMain = TypemainContributorSG[perid][simuid];
      if (typeMain&0x01) mul++;
      if (typeMain&0x02) xmul++;
      if (typeMain&0x04) uvmul++;
    }
    mulmainContributorSG[perid]   = mul;
    xmulmainContributorSG[perid]  = xmul;
    uvmulmainContributorSG[perid] = uvmul;
  }
  return True;
}

PHBoolean 
PHEmbedMCEvaluator::mainContributorCalculationBetweenReconstructionAndSimulation() {//RS
  DchTrack* dchtrack3       =   embedEvent->get_dchtrack3();
  DchHitLineTable* dchhit3  =   embedEvent->get_dchit3();

  int numOfDchTrack1        =   embedEvent->get_dchtrack1()->get_DchNTrack();
  int numOfDchTrack3        =   embedEvent->get_dchtrack3()->get_DchNTrack();

  //reset variables  
  int simuid,recoid,i,j;
  for (recoid =0 ;recoid<numOfDchTrack3+1;recoid++) {
    mulmainContributorRS[recoid]     = -1;
    xmulmainContributorRS[recoid]    = -1;
    uvmulmainContributorRS[recoid]   = -1;
  }
  for (simuid=0;simuid<numOfDchTrack1;simuid++) {
    for (recoid =0 ;recoid<numOfDchTrack3+1;recoid++) {
      TypemainContributorRS[simuid][recoid]      = 0;
      puritymainContributorRS[simuid][recoid]    = 0;
      xpuritymainContributorRS[simuid][recoid]   = 0;
      uvpuritymainContributorRS[simuid][recoid]  = 0;
      x1countmainContributorRS[simuid][recoid]   = 0;
      x2countmainContributorRS[simuid][recoid]   = 0;
      uvcountmainContributorRS[simuid][recoid]   = 0;
    }
  }

  char simuHitsForRecoTrack[numOfDchTrack1][numOfDchTrack3];
  int xhitsS=0, uvhitsS=0, allS=0;
  int xhitsR=0, uvhitsR=0, allR=0;
  int xhitsSE=0,uvhitsSE=0;
  int counter,xcounter,uvcounter,uvcounter1,counterb,xcounterb,uvcounterb,main = 0;
  int x1=0,x2=0,total=0,xtotal=0,uvtotal=0;
  int max=0,xmax=0,uvmax=0;
  int UVHIT[60],count=0,found=0;
  int mul=0, xmul=0, uvmul=0;  
  
  for (simuid=0;simuid<numOfDchTrack1;simuid++) {
    for (recoid =0 ;recoid<numOfDchTrack3;recoid++) {
      simuHitsForRecoTrack[simuid][recoid] = 0;
    }
  }
  int simutrackid;
  for (recoid=0;recoid<numOfDchTrack3;recoid++) {
    for (int plane =0;plane<39;plane++) {
      int hitid = dchtrack3->get_hits(recoid,plane);
      if (hitid>-1) {
	int shitid;
	if ((shitid = embedEvent->recohit2Simulatedhit(hitid))>=0) {
	  simutrackid = embedEvent->simuhit2Simulatedtrack(shitid);
	  if (simutrackid>=numOfDchTrack1) 
	    cout << "RS: Serious error, something worong with simulated tracks" << endl;
	  else if (simutrackid>-1)simuHitsForRecoTrack[simutrackid][recoid]++;
	}
      }
    }
  }
  for (simuid =0 ;simuid<numOfDchTrack1;simuid++) {
    xhitsS   = embedEvent->get_x1hS(simuid)   + embedEvent->get_x2hS(simuid);
    uvhitsS  = embedEvent->get_uv1hS(simuid)  + embedEvent->get_uv2hS(simuid);
    xhitsSE  = 
      embedEvent->get_x1hAfterEmbedForSimulatedTrack(simuid)  + 
      embedEvent->get_x2hAfterEmbedForSimulatedTrack(simuid);
    uvhitsSE = 
      embedEvent->get_uv1hAfterEmbedForSimulatedTrack(simuid) + 
      embedEvent->get_uv2hAfterEmbedForSimulatedTrack(simuid);
    for (i=0;i<60;i++) UVHIT[i]=-1;
    x1=0;x2=0;total=0;xtotal=0;uvtotal=0;
    max=0;xmax=0;uvmax=0;count=0;found=0;mul=0; xmul=0; uvmul=0;
    
    for (i =0 ;i<numOfDchTrack3;i++) {
      if (simuHitsForRecoTrack[simuid][i]<1)continue;
      counter=0;
      xcounter=0;
      uvcounter=0;
      uvcounter1=0;
      counterb=0;
      xcounterb=0;
      uvcounterb=0;
      x1=0;
      x2=0;
      for (int plane=0;plane<39;plane++) {
	int hitid =  dchtrack3->get_hits(i,plane);
	if (hitid>-1) {
	  int realplane = dchhit3->getPlane(hitid);
	  int type = wireType[realplane];

	  if (embedEvent->recohit2Simulatedtrack(hitid) == simuid) {
	    counter++;
	    if (type==X1Wire) x1++;
	    else if (type == X2Wire) x2++;
	    else {
	      uvcounter++;
	      found=0;
	      for (j=0;j<count;j++) {
		if (UVHIT[j]==hitid) {
		  found=1;
		  break;
		}
	      }
	      if (!found) {
		UVHIT[count]=hitid;
		uvcounter1++;
		count++;
	      }
	    }
	  }else{ // calculate contribution from other sources
	    counterb++;
	    if (type==X1Wire||type==X2Wire)xcounterb++;
	    else uvcounterb++;
	  }
	}
      }
      xcounter = x1 + x2;
      if (xcounter>xmax) xmax = xcounter;
      if (uvcounter>uvmax) uvmax = uvcounter;
      if (xcounter+uvcounter>max) max = xcounter + uvcounter;
      xtotal     += xcounter;
      uvtotal    += uvcounter1;
      xhitsR    = xcounter + xcounterb;
      uvhitsR   = uvcounter+ uvcounterb;
      allR      = xhitsR + uvhitsR;
      puritymainContributorRS[simuid][i]      = 0;
      xpuritymainContributorRS[simuid][i]     = 0;
      uvpuritymainContributorRS[simuid][i]    = 0;
      if (allS)     puritymainContributorRS[simuid][i] = ((float)counter)/allR;
      if (xhitsS)  xpuritymainContributorRS[simuid][i] = ((float)xcounter)/xhitsR;
      if (uvhitsS)uvpuritymainContributorRS[simuid][i] = ((float)uvcounter)/uvhitsR;
      main =0;

      //simulated  track is this track's main contributor
      if (counter>=counterb && counter>0) main |= 0x01;
      //simulated  track is this track's x main contributor
      if (xcounter>=xcounterb && xcounter>0) main |= 0x02;
      //simulated  track is this track's uv main contributor
      if (uvcounter>=uvcounterb && uvcounter>0) main |= 0x04;

      x1countmainContributorRS[simuid][i] = x1;
      x2countmainContributorRS[simuid][i] = x2;
      uvcountmainContributorRS[simuid][i] = uvcounter;
      TypemainContributorRS[simuid][i]    = main;
      mul++;
      if (xcounter>0)  xmul++;
      if (uvcounter>0) uvmul++;

      if (verbose>10)
	cout << "G->S->R main contrib. calc: DCH track " << i 
	     << " has " << x1 << " x1hits and " << x2 << " x2hits. \n" 
	     << " total: " << xcounter + uvcounter << " hits." << endl;    
    }

    //total of not associated hits of the geant particle(after merging)
    xtotal = xhitsSE-xtotal;
    uvtotal= uvhitsSE-uvtotal;

    // uvtotal can be less than 0; because they are shared
    if (xtotal<0)  cout << "PHEmbedMCEvaluatorRS: Warning! xtotal<0" << endl;
    if (uvtotal<0) cout << "PHEmbedMCEvaluatorRS: Warning! uvtotal<0" << endl;
    
    if (verbose>20) {
      cout << "***RS*** xhitsS: "     << xhitsS  
	   << ", xhitsSE: "  << xhitsSE  
	   << ", xtotal: "   << xtotal
	   << ", uvhitsS: "  << uvhitsS 
	   << ", uvhitsSE: " << uvhitsSE 
	   << ", uvtotal: "  << uvtotal << endl;
    }

    total = xtotal+uvtotal;
    if (xtotal>xmax)     xmax = xtotal;
    if (uvtotal>uvmax)   uvmax = uvtotal;
    if (total>max)       max = total;

    // Not executed...
    if (0) {//total>0
      //the last entry is for not associated hits;
      puritymainContributorRS[simuid][numOfDchTrack3]       = 0;
      xpuritymainContributorRS[simuid][numOfDchTrack3]      = 0;
      uvpuritymainContributorRS[simuid][numOfDchTrack3]     = 0;
      TypemainContributorRS[simuid][numOfDchTrack3]         = 0;
      mul++;
      if (xtotal>0)  xmul++;
      if (uvtotal>0) uvmul++;    
    }

    if (verbose>5)cout << "RS mulcollectors:" << mul << " " << xmul << " " << uvmul << endl;
  }
  //here contributors only includes reconstructed tracks for test particles
  int typeMain;
  for (simuid=0;simuid<numOfDchTrack1;simuid++) {
    mul=0; xmul=0; uvmul=0;  
    for (recoid=0;recoid<numOfDchTrack3;recoid++) {
      typeMain = TypemainContributorRS[simuid][recoid];
      if (typeMain&0x01) mul++;
      if (typeMain&0x02) xmul++;
      if (typeMain&0x04) uvmul++;
    }
    mulmainContributorRS[simuid]     = mul;
    xmulmainContributorRS[simuid]    = xmul;
    uvmulmainContributorRS[simuid]   = uvmul;
  }
  return True;
}

// This function is called in AssociateDC(), after the maincontrib. fns.
PHBoolean PHEmbedMCEvaluator::getAncestryInformationForPerfTracks() {

  //this function need to be double checkedddd!!
  FKIN_ST*          fkin1           =   embedEvent->get_fkin1()->TableData();
  PRIMARY_ST*     primary1          =   embedEvent->get_primary1()->TableData();
  DCGHIT_ST*        dcghit1         =   embedEvent->get_dcghit1()->TableData();
  DDCHTRACKS_ST*    dchperftrack1   =   embedEvent->get_dchperftrack1()->TableData();
  int numOfFkins1                   =   embedEvent->get_fkin1()->RowCount();
  int numOfPrimarys1                =   embedEvent->get_primary1()->RowCount();
  int numOfPerfTrack1               =   embedEvent->get_dchperftrack1()->RowCount();

  
  int   idGeantTrack=-1,Armside=-1,gen=-1,partid=-1;
  float theta=-1000,phi=-1000;
  
  int idparent=-1,primaryID,truegen,itprimary,fkinIndex;
  double ppt= 9999,ppz = 9999,prapi = 9999,pphi = 9999;
  double px= 1000,py= 1000,pz= 1000,e= 9999,mass=9999;

  float momentumG,momentumpt,momentumx,momentumy,momentumz,xparticleVTX,yparticleVTX,zparticleVTX;
  float xprimaryVTX,yprimaryVTX,zprimaryVTX,primarymomentumG;
  float primarymomentumx,primarymomentumy,primarymomentumz,primarymomentumpt;
  float primarytheta,primaryphi;
  
  int i,j,k;

  // What is Primary - is it the GEANT primary particles in this event?  
  // this section below only stores info for the last ith entry
  // does this mean if you run 1 single MC primary particle only, it only ever loops once...
if (numOfPrimarys1>0) {
    for (i=0;i<numOfPrimarys1;i++) {
      px     = primary1[i].px_momentum;
      py     = primary1[i].py_momentum;
      pz     = primary1[i].pz_momentum;
      partid = primary1[i].idpart;

      if (partid<18&&partid>0) mass   = MASS[partid-1];
      else mass = 0;
      e      = sqrt(px*px + py*py + pz*pz + mass*mass);
      ppt    = sqrt(px*px + py*py);
      ppz    = pz;
      prapi  = 0.5*log((e+pz)/(e-pz));
      pphi   = atan2(py,px);
      if (pphi<-M_PI_2) pphi += 2*M_PI;
      if (verbose>50) {
	cout << "particle code " << primary1[i].idpart << " " 
	     << mass << " " << ppt << " " << ppz << " " 
	     << prapi <<  " " << pphi << endl;
      }
    }
  }
    
  // just taking the first DC plane with an associated GEANT particle,
  // and calling that idGeantTrack
  for (i=0; i< numOfPerfTrack1;i++) {
    // loop over 40 planes in the Drift Chamber
    for (int plane=0;plane<40;plane++) {
      int ghitid = dchperftrack1[i].hits[plane];
      if (ghitid>-1) {
	idGeantTrack = dcghit1[ghitid].mctrack;
	break;
      }
    }
    Armside=0;
    if (dchperftrack1[i].point[0]>0) Armside=1;
    if (dchperftrack1[i].side) Armside &= 0x02;
 
    int found=0;
    primaryID = -1;
    itprimary = -1;
    momentumG = -9999;
    momentumpt = -9999;
    momentumx = -9999;
    momentumy = -9999;
    momentumz = -9999;
    xparticleVTX = -9999;
    yparticleVTX = -9999;
    zparticleVTX= -9999;
    xprimaryVTX = -9999;
    yprimaryVTX = -9999;
    zprimaryVTX = -9999;
    primarymomentumG = -9999;
    primarymomentumx = -9999;
    primarymomentumy = -9999;
    primarymomentumz = -9999;
    primarymomentumpt = -9999;
    primarytheta = -9999;
    primaryphi = -9999;
    
    for (k=0; k<numOfFkins1; k++) {
      if (idGeantTrack == fkin1[k].true_track) {
	momentumG   = fkin1[k].ptot;
	theta       = M_PI*fkin1[k].pthet/180.;
	phi         = M_PI*fkin1[k].pphi/180.;
	momentumx   = momentumG*sin(theta)*cos(phi);
	momentumy   = momentumG*sin(theta)*sin(phi);
	momentumz   = momentumG*cos(theta);
	momentumpt  = sqrt(momentumx*momentumx+momentumy*momentumy);
	xparticleVTX= fkin1[k].r_vertex*cos(M_PI*fkin1[k].ph_vertx/180.); 
	yparticleVTX= fkin1[k].r_vertex*sin(M_PI*fkin1[k].ph_vertx/180.); 
	zparticleVTX= fkin1[k].z_vertex;

	partid      = fkin1[k].idpart;
	idparent    = fkin1[k].idparent;
	if (phi<-M_PI_2) phi+=2*M_PI;

	found=1;
	break;
      }
    }
    
    if (!found) cout << "idGeant not found, what is wrong?" << endl;        

    if (idparent == 0) gen = 1;
    else {
      if (idparent > 0) gen = 2;
      else gen = 3;
    }
    truegen =1;


    if (gen>1) {//find ultimate track index;
      fkinIndex = k;
      while ( fkin1[fkinIndex].idparent != 0 ) {
	truegen++;
	itprimary = fkin1[fkinIndex].itparent;
	if ( itprimary<0 )
	  cout << PHWHERE << "error in track ancestry" << endl;
	fkinIndex = 0;
	for (j=0; j<numOfFkins1; j++) {
	  if ( fkin1[j].true_track == itprimary ) {
	    fkinIndex = j;
	    break;
	  }
	}
      }
      primaryID   = fkin1[fkinIndex].idpart;
      xprimaryVTX = fkin1[fkinIndex].r_vertex
	* cos(M_PI*fkin1[fkinIndex].ph_vertx/180.); 
      yprimaryVTX = fkin1[fkinIndex].r_vertex
	* sin(M_PI*fkin1[fkinIndex].ph_vertx/180.); 
      zprimaryVTX = fkin1[fkinIndex].z_vertex;
      primarytheta = M_PI*fkin1[fkinIndex].pthet/180.;
      primaryphi   = M_PI*fkin1[fkinIndex].pphi/180.;
      if ( primaryphi<-M_PI_2 ) primaryphi+=2*M_PI;
      primarymomentumG = fkin1[fkinIndex].ptot;
      primaryID = fkin1[fkinIndex].idpart;
      primarymomentumx   = primarymomentumG*sin(primarytheta)*cos(primaryphi);
      primarymomentumy   = primarymomentumG*sin(primarytheta)*sin(primaryphi);
      primarymomentumz   = primarymomentumG*cos(primarytheta);
      primarymomentumpt  = 
	sqrt(primarymomentumx*primarymomentumx +
	     primarymomentumy*primarymomentumy);
      if (gen == 2 && truegen != gen)
	cout << PHWHERE << " bad genaration error" << endl;
      if (gen == 3) gen = truegen;
    }else{
      primaryID = partid;
      xprimaryVTX   = xparticleVTX;
      yprimaryVTX   = yparticleVTX;
      zprimaryVTX   = zparticleVTX;
      primarymomentumG = momentumG;
      primarymomentumx = momentumx;
      primarymomentumy = momentumy;
      primarymomentumz = momentumz;
      primarymomentumpt= momentumpt;
      primarytheta   = theta;
      primaryphi     = phi;
   }
    //find single event vertex;

    idGeant[i]   = idGeantTrack;
    armside[i]   = Armside;
       genG[i]   = gen;
    partidG[i]   = partid;
    pareidG[i]   = idparent;
    primidG[i]   = primaryID;
      evtxG[i]   = xprimaryVTX;
      evtyG[i]   = yprimaryVTX;
      evtzG[i]   = zprimaryVTX;
      pptG[i]    = primarymomentumpt;
      ppzG[i]    = primarymomentumz;
      pthe0G[i]  = primarytheta;
      pphi0G[i]  = primaryphi;
      prapiG[i]  = prapi;
      //particle information
      xvtxG[i]   = xparticleVTX;
      yvtxG[i]   = yparticleVTX;
      zvtxG[i]   = zparticleVTX;
      the0G[i]   = theta;
      phi0G[i]   = phi;
  }
  return True;
}

// This function fills vectors of trkPair objects, which will get
// passed into the NTuple-filling function below.
PHBoolean 
PHEmbedMCEvaluator::fillEvaluationTrackList() {
  int typeMainRS,typeMainRG,typeMainSG;

  int numOfPerfTrack1          = embedEvent->get_dchperftrack1()->RowCount();
  int numOfDchTrack1           = embedEvent->get_dchtrack1()->get_DchNTrack();
  int numOfDchTrack3           = embedEvent->get_dchtrack3()->get_DchNTrack();

  DDCHTRACKS_ST* dchperftrack1 = embedEvent->get_dchperftrack1()->TableData();
  DchTrack*      dchtrack3     = embedEvent->get_dchtrack3();

  // trkPair is a class used like a struct (public members) to hold
  // indices and counters for a G,R or G,S pair of tracks as
  // associated by the main contributor functions.  trkPair instances
  // get stored in the tracksToSave vectors in this function.
  trkPair pair;
  int solutionS = 0,solutionR = 0;
  int sumfoundS = 0,sumfoundR = 0;
  int filled = 0;
  int i,j,k; // i = G, j = R, k = S. 

  for (i=0; i<numOfPerfTrack1; i++) tracksToSaveRG[i].clear();
  for (i=0; i<numOfPerfTrack1; i++) tracksToSaveSG[i].clear();
  if (mainContributorAnalysis) { // yes, set to 1 in ctor.


    // --- Type 1 track association --- 
    // Loop order is G, R, S.
    for (i=0; i<numOfPerfTrack1; i++) { // G
      sumfoundR = 0;
      
      // Validate RG data and set sumfoundR.
      for (j=0; j<numOfDchTrack3; j++) {
	if (TypemainContributorRG[i][j]<0) {
	  cout << "Error: RG mainContrib. < 0 " << endl;
	  TypemainContributorRG[i][j] = 0;
	}
	typeMainRG = TypemainContributorRG[i][j];
	
	// If GEANT is x main contributor...
	if (typeMainRG & 0x02) sumfoundR++;
      }

      // Initialize track indices
      pair.trkG = i;
      pair.trkS = -1;
      pair.trkR = -1;
      solutionR = 0;

      // Loop over R tracks.
      for (j=0; j<numOfDchTrack3; j++) { // R
	typeMainRG = TypemainContributorRG[i][j];

	// Require GEANT to be x main contributor...
	if (!(typeMainRG & 0x02)) continue;

	pair.trkR = j;
	solutionR++; 
	pair.solutionR = solutionR;
	pair.sumfoundR = sumfoundR;
	
	filled = 0;
	solutionS = 0;
	sumfoundS = 0;
	
	// Validate RS data and set sumfoundS.
	for (k=0; k<numOfDchTrack1; k++) {
	  if (TypemainContributorRS[k][j]<0) {
	    cout << "Error: RS mainContrib. < 0 " << endl;
	    TypemainContributorRS[k][j] = 0;
	  }
	  typeMainRS = TypemainContributorRS[k][j];

	  if (typeMainRS & 0x02) sumfoundS++;
	}

	// Loop over S tracks.
	// We are still inside both G and R loops.
	for (k=0; k<numOfDchTrack1; k++) { // S
	  typeMainRS = TypemainContributorRS[k][j];

	    // Require S to be x main contributor to R...
	  if (typeMainRS & 0x02) {
	    pair.trkS = k;
	    solutionS++; 
	    pair.solutionS = solutionS;
	    pair.sumfoundS = sumfoundS;
	    tracksToSaveRG[i].push_back(pair);
	    filled = 1;
	    break;
	  }
	}
	pair.trkS = -1;
	if (!filled) {
	  pair.solutionS = 0;
	  pair.sumfoundS = 0;
	  tracksToSaveRG[i].push_back(pair);
	}
      } // end R track loop
      
      // at least fill one per event
      pair.trkS = -1; 
      pair.trkR = -1;
      if (tracksToSaveRG[i].size()<=0) {
	pair.solutionS = 0;
	pair.solutionR = 0;
	pair.sumfoundS = 0;
	pair.sumfoundR = 0;
	tracksToSaveRG[i].push_back(pair);
      }
    } // end type 1 assoc. method


    // --- Type 2 track association --- 
    // Loop order is G, S, R.
    for (i=0; i<numOfPerfTrack1; i++) { // G
      sumfoundS = 0;

      // Validate SG data and set sumfoundS
      for (j=0; j<numOfDchTrack1; j++) {
	if (TypemainContributorSG[i][j]<0) {
	  cout << "Error: SG mainContrib. < 0 " << endl;
	  TypemainContributorSG[i][j] = 0;
	}
	typeMainSG = TypemainContributorSG[i][j];
	
	if (typeMainSG & 0x02) sumfoundS++;
      }
      
      pair.trkG = i;
      pair.trkS = -1;
      pair.trkR = -1;
      solutionS = 0;

      for (j=0; j<numOfDchTrack1; j++) { // S
	if (TypemainContributorSG[i][j]<0) {
	  cout << "Error: SG mainContrib. < 0 " << endl;
	  TypemainContributorSG[i][j] = 0;
	}
	typeMainSG = TypemainContributorSG[i][j];

	// Require GEANT to be x main contributor...
	if (typeMainSG & 0x02) {
	  pair.trkS = j;
	  solutionS++; 
	  pair.solutionS = solutionS;
	  pair.sumfoundS = sumfoundS;
	  filled = 0;
	  solutionR = 0;
	  sumfoundR = 0;

	  // Validate RS data and set sumfoundR
	  for (k=0; k<numOfDchTrack3; k++) {
	    if (TypemainContributorRS[j][k]<0) {
	      cout << "Error: RS mainContrib. < 0 " << endl;
	      TypemainContributorRS[j][k] = 0;
	    }
	    typeMainRS = TypemainContributorRS[j][k];
	    if (typeMainRS & 0x02) sumfoundR++;
	  }

	  for (k=0; k<numOfDchTrack3; k++) { // R
	    typeMainRS = TypemainContributorRS[j][k];
	    
	    // Require S to be x main contributor to R...
	    if (typeMainRS & 0x02) {
	      pair.trkR = k;
	      solutionR++;
	      pair.solutionR = solutionR;
	      pair.sumfoundR = sumfoundR;
	      tracksToSaveSG[i].push_back(pair);
	      filled = 1;
	    }
	  }
	  pair.trkR = -1;
	  if (!filled) {
	    pair.solutionR = 0;
	    pair.sumfoundR = 0;
	    tracksToSaveSG[i].push_back(pair);
	  }
	}
      }

      pair.trkS = -1; 
      pair.trkR = -1;
      if (tracksToSaveSG[i].size()<=0) {
	pair.solutionS = 0;
	pair.solutionR = 0;
	pair.sumfoundS = 0;
	pair.sumfoundR = 0;
	tracksToSaveSG[i].push_back(pair);
      }
    } // end type 2 assoc. method

  } // end if (maincontrib. analysis)

  // --- An incomplete stub of the type 3 track association code(?) ---
  else{ // this code is not ready yet don't use
    // here only fill RG list
    float deltaPhi,deltaZed;
    for (int i=0; i<numOfPerfTrack1;i++) {
      pair.trkG = i;
      pair.trkS = -1;
      pair.trkR = -1;
      solutionR = 0;
      for (int j=0;j<numOfDchTrack3;j++) {
	pair.trkR  = j;
	solutionR++; pair.solutionR = solutionR;
	solutionS = 0;
	
	deltaPhi            = dchperftrack1[i].phi        -  dchtrack3->get_phi(j);
	deltaZed            = dchperftrack1[i].zed        -  dchtrack3->get_zed(j);
	if (fabs(deltaPhi) < 2/ToDegree && (fabs(deltaZed) < 5)) {
	  
	  for (int k=0;k<numOfDchTrack1;k++) {
	    //may be record several times,but usually not happen
	    //typeMainRS   = TypemainContributorRS[k][j];
	    //if (!typeMainRS&0x02) continue;
	    pair.trkS  = k;
	    solutionS++; pair.solutionS = solutionS;
   
	    tracksToSaveRG[i].push_back(pair);
	  }
	}
      }
      pair.trkS = -1; pair.trkR = -1;
      if (tracksToSaveRG[i].size()==0) {
	pair.solutionS = 0; pair.solutionR = 0;
	tracksToSaveRG[i].push_back(pair);
      }
    }
  }
  return True;
}

// In AssociateDC() this function gets called like this:
//   fillCompleteEvaluationNtupleForSelectedTracks(tracksToSaveRG, 1); //G->R,    type = 1
//   fillCompleteEvaluationNtupleForSelectedTracks(tracksToSaveSG, 2); //G->S,    type = 2

PHBoolean 
PHEmbedMCEvaluator::fillCompleteEvaluationNtupleForSelectedTracks(vector<trkPair>* List, 
								  int type)
{  
  // Get pointers to relevant containers from PHEmbededEvent
  DDCHTRACKS_ST*    dchperftrack1 =   embedEvent->get_dchperftrack1()->TableData();
  DchTrack*         dchtrack1     =   embedEvent->get_dchtrack1();
  DchTrack*         dchtrack2     =   embedEvent->get_dchtrack2();
  DchTrack*         dchtrack3     =   embedEvent->get_dchtrack3();
  PHDchTrackOut*    phdchtrack1   =   embedEvent->get_phdchtrack1();
  PHDchTrackOut*    phdchtrack3   =   embedEvent->get_phdchtrack3();
  PHGlobal*         global2       =   embedEvent->get_global2();
  BbcOut*           bbcout2       =   embedEvent->get_bbcout2();
  ZdcOut*           zdcout2       =   embedEvent->get_zdcout2();

  // Should add checks here that pointers were properly assigned?

  int numOfPerfTrack1             =   embedEvent->get_dchperftrack1()->RowCount();  
  int numOfDchTrack1              =   embedEvent->get_dchtrack1()->get_DchNTrack();
  int numOfDchTrack2              =   embedEvent->get_dchtrack2()->get_DchNTrack();
  int numOfDchTrack3              =   embedEvent->get_dchtrack3()->get_DchNTrack();

  float bbcvtx  = -9999; // background event vertex 
  float bbct0   = -9999;

 // number of tracks in the background event before and after embedding 
  short ntrkwBR = 0; // west
  short ntrkeBR = 0; // east
  short ntrkwAR = 0; 
  short ntrkeAR = 0;
  float bbce1 = -999, bbce2 = -999, zdce1 = -999, zdce2 = -999;

  if (bbcout2) {
    bbcvtx     = bbcout2->get_VertexPoint();
    bbct0      = bbcout2->get_TimeZero();
    bbce1 = bbcout2->get_ChargeSum(Bbc::North);
    bbce2 = bbcout2->get_ChargeSum(Bbc::South);
    zdce1 = zdcout2->get_Energy(Zdc::North);
    zdce2 = zdcout2->get_Energy(Zdc::South);
    if (verbose > 1) {
      cout << "BBCVTXEVT: RD " << bbcvtx 
	   << " EMB " <<  embedEvent->get_bbcout3()->get_VertexPoint() 
	   << " eval type " << type
	   << endl;
    }
  }

  for (int i=0; i<numOfDchTrack2; i++) {
    if (dchtrack2->get_arm(i)) ntrkwBR++;
    else ntrkeBR++;
  }

  for (int i=0; i<numOfDchTrack3; i++) {
    if (dchtrack3->get_arm(i)) ntrkwAR++;
    else ntrkeAR++;
  }

  float          thetaG,        thetaS,        thetaR;
  float          x1mG,          x1mS,          x1mR;
  float          x2mG,          x2mS,          x2mR;
  short          x1hG,          x1hS,          x1hR,        x1hE;
  short          x2hG,          x2hS,          x2hR,        x2hE;
  short          uv1hG,         uv1hS,         uv1hR,       uv1hE;
  short          uv2hG,         uv2hS,         uv2hR,       uv2hE;
  float          dcchi2S=-1,    dcchi2R=-1;
  int            typeMain;

  trkPair pair; // see fillEvaluationTrackList() and header
  PHEmbedMcRecoTrack* mctrk;
  DDCHTRACKS_ST dctrackG;
  int trkG, trkR, trkS;


  // Main loop over perfect tracks, which are GEANT tracks that were
  // found in the DCH.
  for (int i = 0; i < numOfPerfTrack1; i++) {

    // "List" is a vector<trkPair> passed in to this fn.
    // So Index is the address of the ith trkPair object (?)
    vector<trkPair>& Index = List[i];

    // This is a loop over SG pairs if type = 1
    // or over RG pairs if type = 2
    for (unsigned int j=0; j<Index.size(); j++) {

      pair = Index[j];
      // "<<" is overloaded for trkPair objects.
      // Providing a guide to variable names here.
      if (verbose>5) {
	cout << "(trkG, trkS, solutionS, sumfoundS, trkR, solutionR, sumfoundR): " 
	     << pair << endl;
      }

      trkG = pair.trkG;
      trkR = pair.trkR;
      trkS = pair.trkS;

      if (trkG != i) 
	cout << "not equal " << trkG << " " << i << endl;
      
      dctrackG = dchperftrack1[i];
      thetaG = acos(dctrackG.point[2]/
		    sqrt(dctrackG.point[0]*dctrackG.point[0] +
			 dctrackG.point[1]*dctrackG.point[1] +
			 dctrackG.point[2]*dctrackG.point[2]));
      x1mG   = embedEvent->get_x1mPerfectTrack(i);
      x2mG   = embedEvent->get_x2mPerfectTrack(i);
      x1hG   = embedEvent->get_x1hPerfectTrack(i);
      x2hG   = embedEvent->get_x2hPerfectTrack(i);
      uv1hG  = embedEvent->get_uv1hPerfectTrack(i);
      uv2hG  = embedEvent->get_uv2hPerfectTrack(i);

      // Making a new mctrk initializes all variables to their
      // defaults...
      mctrk  = new PHEmbedMcRecoTrack();
      mctrk->set_type(type);
      // eventnum is never filled and is commented out of
      // PHEmbedHistogrammer::fillPHEmbedMcRecoTrack()
      mctrk->set_evtID(eventnum);       
      mctrk->set_idGeant(idGeant[i]);
      mctrk->set_evtxG(evtxG[i]);       
      mctrk->set_evtyG(evtyG[i]);            
      mctrk->set_evtzG(evtzG[i]);       
      mctrk->set_pptG(pptG[i]);
      mctrk->set_ppzG(ppzG[i]);     
      mctrk->set_pthe0G(pthe0G[i]);     
      mctrk->set_pphi0G(pphi0G[i]);    
      mctrk->set_prapiG(prapiG[i]);
      mctrk->set_genG(genG[i]);
      mctrk->set_partidG(partidG[i]);   
      mctrk->set_pareidG(pareidG[i]);
      mctrk->set_primidG(primidG[i]);   
      mctrk->set_xvtxG(xvtxG[i]);       
      mctrk->set_yvtxG(yvtxG[i]);   
      mctrk->set_zvtxG(zvtxG[i]);
      if (fabs(M_PI*dctrackG.theta0/180-the0G[i])>0.00001)
	cout << M_PI*dctrackG.theta0/180-the0G[i] << endl;
      mctrk->set_ntrkG(numOfPerfTrack1);        
      mctrk->set_dctrkidG(dctrackG.trackid);
      mctrk->set_dctrkQualG(dctrackG.quality);  
      mctrk->set_the0G(the0G[i]);
      mctrk->set_phi0G(phi0G[i]);               
      mctrk->set_alphaG(dctrackG.alpha);
      mctrk->set_thetaG(thetaG);                
      mctrk->set_phiG(dctrackG.phi);
      mctrk->set_betaG(dctrackG.beta);          
      mctrk->set_zedG(dctrackG.zed);
      mctrk->set_momG(dctrackG.momentum);       
      mctrk->set_ptG((dctrackG.momentum)*sin(the0G[i]));//*&
      mctrk->set_x1mG(x1mG);         
      mctrk->set_x2mG(x2mG);
      mctrk->set_x1hG(x1hG);         
      mctrk->set_x2hG(x2hG);
      mctrk->set_uv1hG(uv1hG);       
      mctrk->set_uv2hG(uv2hG);

      if (global2) mctrk -> set_bbccent( global2->getCentrality() );

      mctrk->set_bbcqn(bbce1);
      mctrk->set_zdcen(zdce1);
      mctrk->set_bbcqs(bbce2);
      mctrk->set_zdces(zdce2);
      mctrk->set_bbcvtx(bbcvtx);     
      mctrk->set_bbct0(bbct0);
      mctrk->set_isminbias(isminbias);
      mctrk->set_ntrkb(ntrkwBR+ntrkeBR);//*&
      mctrk->set_ntrka(ntrkwAR+ntrkeAR);//*&
      x1hE   = embedEvent->get_x1hAfterEmbedForPerfectTrack(i);
      x2hE   = embedEvent->get_x2hAfterEmbedForPerfectTrack(i);
      uv1hE  = embedEvent->get_uv1hAfterEmbedForPerfectTrack(i);
      uv2hE  = embedEvent->get_uv1hAfterEmbedForPerfectTrack(i);
      mctrk->set_x1hE(x1hE);           
      mctrk->set_x2hE(x2hE);
      mctrk->set_uv1hE(uv1hE);         
      mctrk->set_uv2hE(uv2hE);
      mctrk->set_sumfound(Index.size());    
      mctrk->set_solution(j+1);//before was j
      mctrk->set_sumfoundS(pair.sumfoundS);
      mctrk->set_solutionS(pair.solutionS);

      if (trkS>-1&&trkR>-1) {
	mctrk->x1hRS = x1countmainContributorRS[trkS][trkR];
	mctrk->x2hRS = x2countmainContributorRS[trkS][trkR];
	mctrk->uvhRS = uvcountmainContributorRS[trkS][trkR];
      }

      if (trkS>-1) {
	PHSphPoint point = dchtrack1->get_point(trkS);
	thetaS      = point.getTheta();
	x1mS        = embedEvent->get_x1mS(trkS);
	x2mS        = embedEvent->get_x2mS(trkS);
	x1hS        = embedEvent->get_x1hS(trkS);
	x2hS        = embedEvent->get_x2hS(trkS);
	uv1hS       = embedEvent->get_uv1hS(trkS);
	uv2hS       = embedEvent->get_uv2hS(trkS);
	mctrk->set_ntrkS(numOfDchTrack1);
	mctrk->set_dctrkidS(dchtrack1->get_trackid(trkS));      
	mctrk->set_dctrkQualS(dchtrack1->get_quality(trkS));
	mctrk->set_the0S(dchtrack1->get_theta0(trkS));          
	mctrk->set_phi0S(dchtrack1->get_phi0(trkS));
	mctrk->set_alphaS(dchtrack1->get_alpha(trkS));          
	mctrk->set_thetaS(thetaS);
	mctrk->set_phiS(dchtrack1->get_phi(trkS));  
	mctrk->set_betaS(dchtrack1->get_beta(trkS));
	mctrk->set_zedS(dchtrack1->get_zed(trkS));
	mctrk->set_dcchi2S(dcchi2S);
	mctrk->set_momS(dchtrack1->get_momentum(trkS));
	mctrk->set_x1mS(x1mS);
	mctrk->set_x2mS(x2mS);
	mctrk->set_x1hS(x1hS);
	mctrk->set_x2hS(x2hS);
	mctrk->set_uv1hS(uv1hS);
	mctrk->set_uv2hS(uv2hS);         

	mctrk->set_mulmainS(mulmainContributorSG[i]);
	mctrk->set_xmulmainS(xmulmainContributorSG[i]);
	mctrk->set_uvmulmainS(uvmulmainContributorSG[i]);

	typeMain = TypemainContributorSG[i][trkS];

	if (typeMain&0x01) mctrk->set_mainIDS(i);
	if (typeMain&0x02) mctrk->set_xmainIDS(i);
	if (typeMain&0x04) mctrk->set_uvmainIDS(i);

	mctrk->x1hSG = x1countmainContributorSG[trkG][trkS];
	mctrk->x2hSG = x2countmainContributorSG[trkG][trkS];
	mctrk->uvhSG = uvcountmainContributorSG[trkG][trkS];
	mctrk->set_purityS(puritymainContributorSG[trkG][trkS]);
	mctrk->set_xpurityS(xpuritymainContributorSG[trkG][trkS]);
	mctrk->set_uvpurityS(uvpuritymainContributorSG[trkG][trkS]);

	//fill model information
	mctrk->set_nx1x2fitS(phdchtrack1->get_numberOfX1X2hitsFitted(trkS));
	mctrk->set_mchi2S(phdchtrack1->get_chi2(trkS));
	mctrk->set_errS(phdchtrack1->get_ErrorCode(trkS));
	mctrk->set_alphafS(phdchtrack1->get_fittedAlpha(trkS));

      }
     
      mctrk->set_sumfoundR(pair.sumfoundR);
      mctrk->set_solutionR(pair.solutionR);

      if (trkR>-1) { //reconstruction information

	int armsidR = dchtrack3->get_arm(trkR);
	if (dchtrack3->get_side(trkR)) armsidR+=2;
	
	dcchi2R     = (*trackCandidateList)[trkR]->getChi2OfFit();

	PHSphPoint point = dchtrack3->get_point(trkR);
	thetaR      = point.getTheta();
	x1mR        = embedEvent->get_x1mR(trkR);
	x2mR        = embedEvent->get_x2mR(trkR);
	x1hR        = embedEvent->get_x1hR(trkR);
	x2hR        = embedEvent->get_x2hR(trkR);
	uv1hR       = embedEvent->get_uv1hR(trkR);
	uv2hR       = embedEvent->get_uv2hR(trkR);
	
	mctrk->set_dctrkidR(dchtrack3->get_trackid(trkR));
	mctrk->set_dctrkQualR(dchtrack3->get_quality(trkR));
	mctrk->set_the0R(dchtrack3->get_theta0(trkR));
	mctrk->set_phi0R(dchtrack3->get_phi0(trkR));  
	mctrk->set_alphaR(dchtrack3->get_alpha(trkR));
	mctrk->set_thetaR(thetaR);	
	mctrk->set_phiR(dchtrack3->get_phi(trkR));
	mctrk->set_betaR(dchtrack3->get_beta(trkR));
	mctrk->set_zedR(dchtrack3->get_zed(trkR));
	mctrk->set_dcchi2R(dcchi2R);
	mctrk->set_momR(dchtrack3->get_momentum(trkR));           
	mctrk->set_x1mR(x1mR);
	mctrk->set_x2mR(x2mR);
	mctrk->set_x1hR(x1hR);
	mctrk->set_x2hR(x2hR);
	mctrk->set_uv1hR(uv1hR);
	mctrk->set_uv2hR(uv2hR);

	mctrk->set_mulmainR(mulmainContributorRG[i]);
	mctrk->set_xmulmainR(xmulmainContributorRG[i]);
	mctrk->set_uvmulmainR(uvmulmainContributorRG[i]);

	typeMain = TypemainContributorRG[i][trkR];
	if (typeMain&0x01) mctrk->set_mainIDR(i);
	if (typeMain&0x02) mctrk->set_xmainIDR(i);
	if (typeMain&0x04) mctrk->set_uvmainIDR(i);

	mctrk->x1hRG = x1countmainContributorRG[trkG][trkR];
	mctrk->x2hRG = x2countmainContributorRG[trkG][trkR];
	mctrk->uvhRG = uvcountmainContributorRG[trkG][trkR];
	mctrk->set_purityR(puritymainContributorRG[trkG][trkR]);
	mctrk->set_xpurityR(xpuritymainContributorRG[trkG][trkR]);
	mctrk->set_uvpurityR(uvpuritymainContributorRG[trkG][trkR]);

	//fill model information
	mctrk->set_nx1x2fitR(phdchtrack3->get_numberOfX1X2hitsFitted(trkR));
	mctrk->set_mchi2R(phdchtrack3->get_chi2(trkR));
	mctrk->set_errR(phdchtrack3->get_ErrorCode(trkR));
	mctrk->set_alphafR(phdchtrack3->get_fittedAlpha(trkR));
      }

      mcRecoTrackList->append(mctrk);

    } // end of loop over trkPair objects

  } // end of loop over PerfTracks (i.e. GEANT particles in the event)

  return True;

}

PHBoolean PHEmbedMCEvaluator::associateDC()
{
  if (!node2||!node3) {
    cout << "PHEmbedMCEvaluator:: missing top nodes" << endl;
    return False;
  }
  PHNodeIterator iter(node3);
  // extract the candidate Node
  PHDataNode<PHPointerList<DchTrackCandidate> >* tmpTrackCandiNode;  
  tmpTrackCandiNode = 
    (PHDataNode<PHPointerList<DchTrackCandidate> >*)iter.findFirst("PHDataNode",
								   "DchTrackCandidate");
  
  if (!tmpTrackCandiNode) {
    PHMessage("PHEmbedMCEvaluator::event",
	      PHError,
	      "DchTrackCandidate missing from tree");
    return False;
  }
  trackCandidateList = tmpTrackCandiNode->getData();

  //so *1 is input Reconstructed information of the particle in test particle node.
  //   *2 is input Reconstructed information in DST node
  //   *3 is merged and Reconstruced information in merged node

  int numOfPerfTrack1 =  embedEvent->get_dchperftrack1()->RowCount();


  /*
  Why create new mctrks and append them to the list if there is no
  perfect track? What is the point of that?

  An answer from Jiangyong 11/13/2008:
  
  Basically, this code save an empty entry for events where the embedded
  geant track was not found in DC (most likely it is outside the detector
  acceptance since we generate the input bigger than PHENIX acceptance).
  It was designed for book keeping purpose. Second reason is that
  currently, we use DC dominate contributor analysis, one might imagine
  using some other detectors, such as PC. In this case one would need to
  find Geant track first in PC then look at DC.
  If this is the reason for see many empty tracks in your evaluation
  ntuple, that simply means that you have many MC tracks didn't enter DC,
  or something else was wrong (since I only fill one empty track for each
  unfound event)
  
  But since we may always use DC as the dominate contributor analysis,
  your could simply comment this block out.
  
  Jiangyong
  */
  
  bool SaveEmptyTracks = false;

  if (SaveEmptyTracks) {
    if (numOfPerfTrack1 <= 0) { // no track passing DC
      PHEmbedMcRecoTrack* mctrk = new PHEmbedMcRecoTrack();
      mctrk->set_type(1);
      mcRecoTrackList->append(mctrk);
      mctrk = new PHEmbedMcRecoTrack();
      mctrk->set_type(2);
      mcRecoTrackList->append(mctrk);    
      //mctrk = new PHEmbedMcRecoTrack();
      //mctrk->set_type(3);
      //mcRecoTrackList->append(mctrk);    
      return False;
    }
  }
  
  mainContributorCalculationBetweenReconstructionAndGeant();
  mainContributorCalculationBetweenSimulationAndGeant();
  // Still need to call this even though type 3 output is not saved out!
  // Otherwise, R and S are not associated in fillEvaluationTrackList().
  mainContributorCalculationBetweenReconstructionAndSimulation();

  getAncestryInformationForPerfTracks();
  
  fillEvaluationTrackList();
  fillCompleteEvaluationNtupleForSelectedTracks(tracksToSaveRG, 1); //G->R,    type = 1
  fillCompleteEvaluationNtupleForSelectedTracks(tracksToSaveSG, 2); //G->S,    type = 2
  //fillCompleteEvaluationNtupleForSelectedTracks(tracksToSaveRSG, 3);//G->R->S, type = 3

  if (verbose > 5) {
    cout << "AssociateDC() summary: " << endl;
    cout << " # DCH perfect tracks: " << embedEvent->get_dchperftrack1()->RowCount()  << endl; 
    cout << " # Sng. MC cgl tracks: " << embedEvent->get_cgltrack1()->get_CglNTrack() << endl; 
    cout << " # Emb. cgl tracks:    " << embedEvent->get_cgltrack3()->get_CglNTrack() << endl; 
    cout << " # in mcRecoTrackList: " << mcRecoTrackList->length() << endl; 
  }

  return True;

}

PHBoolean PHEmbedMCEvaluator::associatePC(int pc) {
  if (verbose>1) {
    cout << "PHEmbedMCEvaluator::associatePC(" << pc << ")" << endl;
  }

  // track1, 2, 3 are single MC, real, and merged tracks, respectively.
  CglTrack*     cgltrack1     = embedEvent->get_cgltrack1();
  PHTrackOut*   phtrack1      = embedEvent->get_phtrack1();
  CglTrack*     cgltrack3     = embedEvent->get_cgltrack3();
  PHTrackOut*   phtrack3      = embedEvent->get_phtrack3();
  
 if (!cgltrack3||!cgltrack1) {
   PHMessage("associatePC",PHWarning,"CglTrack table  not found in merged node tree");
   return False; 
 }
 if (!phtrack3||!phtrack1) {
   PHMessage("associatePC",PHWarning,"PHTrackOut table  not found in merged node tree");
   return False; 
 }
 // pcx means pc1, 2 or 3
 PadCluster* pcxcluster3  = 0;
 PadCluster* pcxcluster1  = 0;
 pcghitWrapper* pcxghit1     = 0;
 if (pc == 1) {
   pcxcluster3  = embedEvent->get_pc1cluster3();
   pcxcluster1  = embedEvent->get_pc1cluster1();
   pcxghit1     = embedEvent->get_pc1ghit1();
 }else if (pc ==2) {
   pcxcluster3  = embedEvent->get_pc2cluster3();
   pcxcluster1  = embedEvent->get_pc2cluster1();
   pcxghit1     = embedEvent->get_pc2ghit1();
 }else if (pc == 3) {
   pcxcluster3  = embedEvent->get_pc3cluster3();
   pcxcluster1  = embedEvent->get_pc3cluster1();
   pcxghit1     = embedEvent->get_pc3ghit1();
 }else {
   cout << "NO pad chamber with this id: choose between 1,2 or 3 " << endl;
   return False;
 }
 int n1 = pcxcluster1->get_PadNCluster();
 int n3 = pcxcluster3->get_PadNCluster();
 if (verbose > 20) {
   cout << n1 << " single MC cluster(s), "
	<< n3 << " merged evt. cluster(s)" << endl;
   
   if (n1 > 0) {
     for (int i=0;i<n1;i++) {
       cout << "MC cluster " << i << " (x, y, z): " << flush;
       cout << pcxcluster1->get_xyz(i,0) << " "
	    << pcxcluster1->get_xyz(i,1) << " "
	    << pcxcluster1->get_xyz(i,2) << endl;
     }
     
     for (int i=0;i<n3;i++) {
       cout << "Merged evt " << i << " (x, y, z): " << flush;
       cout << pcxcluster3->get_xyz(i,0) << " "
	    << pcxcluster3->get_xyz(i,1) << " "
	    << pcxcluster3->get_xyz(i,2) << endl;
     }
   }
 }
 
 int totalMcRecoTracks = mcRecoTrackList->length();
 int idTrackG = -1;
 int idTrackR = -1;
 int idTrackS = -1;
 int pcxidMc  = -1;     // responsed pcid in node1 for test particle
 int pcxidE   = -1;     // embeded pcid in node3 for test particle
 // pcxidMc and pcxidE actually refer to the same cluster!! so their positions are same
 
 int pcxidS   = -1;     // associated pcid in node1 for test particle
 int pcxidR   = -1;     // associated pcid in node3 for test particle
 float x,y,z;
 PHPoint null(9999,9999,9999);
 PHPoint mcPoint;
 PHPoint recoPointS;
 PHPoint projPointS;
 PHPoint recoPointR;
 PHPoint projPointR;
 
 PHEmbedMcRecoTrack* mctrk = 0;
 
 if (verbose > 20)
   cout << "totalMcRecoTracks = " << totalMcRecoTracks << endl;
 
 for (int i = 0; i < totalMcRecoTracks; i++) {
   mctrk       = (*mcRecoTrackList)[i];
   idTrackG    = mctrk->get_idGeant(); //same as mc_track
   idTrackS    = mctrk->get_dctrkidS();//same as dchtrackid in single particle node
   idTrackR    = mctrk->get_dctrkidR();//same as dchtrackid in merged node
   // Initialize...
   pcxidMc = -1;
   pcxidE = -1;
   pcxidS = -1;
   pcxidR = -1;
   mcPoint = null;
   recoPointS = null;
   projPointS = null;
   recoPointR = null;
   projPointR = null;

   //-------------------------------------------
   //------------- MONTE-CARLO information

   if (verbose>10) {
     cout << "Geant track id: " << idTrackG 
	  << ", evaluation type: " << mctrk->get_type() << endl;
   }

   // Set cluster ID for Geant track, post-embedding
   if (idTrackG>-1) {
     pcxidE = embedEvent->get_pcClusterAfterEmbedForGeantTrack(pc,idTrackG);
     if (pc==1) {
       mctrk->set_pc1idE(pcxidE);
     } else if (pc==2) {
       mctrk->set_pc2idE(pcxidE);
     } else if (pc==3) {
       mctrk->set_pc3idE(pcxidE);
     }

     /*
      Geant information is always applied for ideal geometry, 
      so it can happen that geant hits exist, but the responsed hit 
      is not there 
      */

     // Get info for GEANT hits and copy into mctrk
     for (unsigned int j=0; j < pcxghit1->RowCount(); j++) {
       if (idTrackG == pcxghit1->get_mctrack(j)) { // this is the right track
	 pcxidMc   = embedEvent->pcghit2Cluster(pc,pcxghit1->get_id(j));
	 x = pcxghit1->get_xyzinglo(0,j);   
	 y = pcxghit1->get_xyzinglo(1,j);   
	 z = pcxghit1->get_xyzinglo(2,j);
	 mcPoint.setX(x);
	 mcPoint.setY(y);
	 mcPoint.setZ(z);
	 if (pc==1) {
	   mctrk->set_pc1idMc(pcxidMc);
	   mctrk->set_pc1xMc(x);
	   mctrk->set_pc1yMc(y);
	   mctrk->set_pc1zMc(z);
	 } else if (pc==2) {
	   mctrk->set_pc2idMc(pcxidMc);
	   mctrk->set_pc2xMc(x);
	   mctrk->set_pc2yMc(y);
	   mctrk->set_pc2zMc(z);
	 } else if (pc==3) {
	   mctrk->set_pc3idMc(pcxidMc);
	   mctrk->set_pc3xMc(x);
	   mctrk->set_pc3yMc(y);
	   mctrk->set_pc3zMc(z);
	 }
	 break;
       }   
     }     
   }
   //------------- SIMULATION information-------------
   if (idTrackS>-1) {
     if (pc == 1) {
       pcxidS = cgltrack1->get_pc1clusid(idTrackS);
       if (phtrack1->get_ifIntersectPc1(idTrackS)) {
	 x = phtrack1->get_projectionPc1(idTrackS,0);
	 y = phtrack1->get_projectionPc1(idTrackS,1);
	 z = phtrack1->get_projectionPc1(idTrackS,2);
	 projPointS.setX(x);
	 projPointS.setY(y);
	 projPointS.setZ(z);
	 mctrk->set_ppc1xS(x);
	 mctrk->set_ppc1yS(y);
	 mctrk->set_ppc1zS(z);
       }
     } else if (pc == 2) {
       pcxidS = cgltrack1->get_pc2clusid(idTrackS);
       if (phtrack1->get_ifIntersectPc2(idTrackS)) {
	 x = phtrack1->get_projectionPc2(idTrackS,0);
	 y = phtrack1->get_projectionPc2(idTrackS,1);
	 z = phtrack1->get_projectionPc2(idTrackS,2);
	 projPointS.setX(x);
	 projPointS.setY(y);
	 projPointS.setZ(z);
	 mctrk->set_ppc2xS(x);
	 mctrk->set_ppc2yS(y);
	 mctrk->set_ppc2zS(z);
       }
     }else if (pc == 3) {
       pcxidS = cgltrack1->get_pc3clusid(idTrackS);
       if (phtrack1->get_ifIntersectPc3(idTrackS)) {
	 x = phtrack1->get_projectionPc3(idTrackS,0);
	 y = phtrack1->get_projectionPc3(idTrackS,1);
	 z = phtrack1->get_projectionPc3(idTrackS,2);
	 projPointS.setX(x);
	 projPointS.setY(y);
	 projPointS.setZ(z);
	 mctrk->set_ppc3xS(x);
	 mctrk->set_ppc3yS(y);
	 mctrk->set_ppc3zS(z);
       }
     }
     if (pcxidS>-1) {
       x = 9999; y = 9999; z = 9999;
       for (unsigned int k=0;k<pcxcluster1->get_PadNCluster();k++) {
	 if (pcxcluster1->get_id(k) == pcxidS) {
	   x = pcxcluster1->get_xyz(k,0);
	   y = pcxcluster1->get_xyz(k,1);
	   z = pcxcluster1->get_xyz(k,2);
	   break;
	 }
       }
       if (pc == 1) {
	 mctrk->set_pc1idS(pcxidS);
	 mctrk->set_pc1xS(x);
	 mctrk->set_pc1yS(y);
	 mctrk->set_pc1zS(z);
       }else if (pc == 2) {
	 mctrk->set_pc2idS(pcxidS);
	 mctrk->set_pc2xS(x);
	 mctrk->set_pc2yS(y);
	 mctrk->set_pc2zS(z);
       }else if (pc == 3) {
	 mctrk->set_pc3idS(pcxidS);
	 mctrk->set_pc3xS(x);
	 mctrk->set_pc3yS(y);
	 mctrk->set_pc3zS(z);
       }
       recoPointS.setX(x);
       recoPointS.setY(y);
       recoPointS.setZ(z);
     }
   }
   if (verbose>10) {
     cout << "S track ID: " << idTrackS 
	  << ",  pcxSimuid  " << pcxidS << endl;
   }
   //------------- RECONSTRUCTION information-------------
   // the idTrackR is the gateway for the idCGlTrack
   //-----------------------------------------------------
   if (idTrackR>-1) {
     if (pc ==1 ) {
       pcxidR = cgltrack3->get_pc1clusid(idTrackR);
       if (phtrack3->get_ifIntersectPc1(idTrackR)) {
	 x = phtrack3->get_projectionPc1(idTrackR,0);
	 y = phtrack3->get_projectionPc1(idTrackR,1);
	 z = phtrack3->get_projectionPc1(idTrackR,2);
	 projPointR.setX(x);
	 projPointR.setY(y);
	 projPointR.setZ(z);
	 mctrk->set_ppc1xR(x);
	 mctrk->set_ppc1yR(y);
	 mctrk->set_ppc1zR(z);
       }
     }else if (pc == 2) {
       pcxidR = cgltrack3->get_pc2clusid(idTrackR);
       if (phtrack3->get_ifIntersectPc2(idTrackR)) {
	 x = phtrack3->get_projectionPc2(idTrackR,0);
	 y = phtrack3->get_projectionPc2(idTrackR,1);
	 z = phtrack3->get_projectionPc2(idTrackR,2);
	 projPointR.setX(x);
	 projPointR.setY(y);
	 projPointR.setZ(z);
	 mctrk->set_ppc2xR(x);
	 mctrk->set_ppc2yR(y);
	 mctrk->set_ppc2zR(z);
       }
     }else if (pc == 3) {
       pcxidR = cgltrack3->get_pc3clusid(idTrackR);
       if (phtrack3->get_ifIntersectPc3(idTrackR)) {
	 x = phtrack3->get_projectionPc3(idTrackR,0);
	 y = phtrack3->get_projectionPc3(idTrackR,1);
	 z = phtrack3->get_projectionPc3(idTrackR,2);
	 projPointR.setX(x);
	 projPointR.setY(y);
	 projPointR.setZ(z);
	 mctrk->set_ppc3xR(x);
	 mctrk->set_ppc3yR(y);
	 mctrk->set_ppc3zR(z);
       }
     }
     if (pcxidR>-1) {
       x = pcxcluster3->get_xyz(pcxidR,0);
       y = pcxcluster3->get_xyz(pcxidR,1);
       z = pcxcluster3->get_xyz(pcxidR,2);
       if (pc ==1) {
	 mctrk->set_pc1idR(pcxidR);
	 mctrk->set_pc1xR(x);
	 mctrk->set_pc1yR(y);
	 mctrk->set_pc1zR(z);
       }else if (pc ==2) {
	 mctrk->set_pc2idR(pcxidR);
	 mctrk->set_pc2xR(x);
	 mctrk->set_pc2yR(y);
	 mctrk->set_pc2zR(z);
       }else if (pc ==3) {
	 mctrk->set_pc3idR(pcxidR);
	 mctrk->set_pc3xR(x);
	 mctrk->set_pc3yR(y);
	 mctrk->set_pc3zR(z);
       }
       recoPointR.setX(x);
       recoPointR.setY(y);
       recoPointR.setZ(z);
     }
   }
   if (verbose>10) {
     cout << "R track ID: " << idTrackR << ",  pcxRecoId  " << pcxidR << endl;
   }
   //---------------------------------------------
   if (verbose>10) {
     double momg = mctrk->get_momG();
     double moms = mctrk->get_momS();
     double momr = mctrk->get_momR();
     cout << "McRecoTrack " << i << flush; 
     cout <<  ": PC "<< pc << " summary\n--------------------"<< endl;
     if (pc==1) {
       cout << " MC PC ID:  " << mctrk->get_pc1idMc() 
	    << "\n S PC ID: "<< mctrk->get_pc1idS()
	    << "\n E, R PC ID: " << mctrk->get_pc1idE() << ", " << mctrk->get_pc1idR() 
	    << "\n DCH quality (R): " << mctrk->get_dctrkQualR()
	    << "\n momG,S,R: " << momg << ", " << moms << ", " << momr 
	    << endl;
     }
     else if (pc==2) {
       cout << " MC PC ID:  " << mctrk->get_pc2idMc() 
	    << "\n S PC ID: "<< mctrk->get_pc2idS()
	    << "\n E, R PC ID: " << mctrk->get_pc2idE() << ", " << mctrk->get_pc2idR() 
	    << "\n DCH quality (R): " << mctrk->get_dctrkQualR()
	    << "\n momG,S,R: " << momg << ", " << moms << ", " << momr 
	    << endl;
     }
     else if (pc==3) {
       cout << " MC PC ID:  " << mctrk->get_pc3idMc() 
	    << "\n S PC ID: "<< mctrk->get_pc3idS()
	    << "\n E, R PC ID: " << mctrk->get_pc3idE() << ", " << mctrk->get_pc3idR() 
	    << "\n DCH quality (R): " << mctrk->get_dctrkQualR()
	    << "\n momG,S,R: " << momg << ", " << moms << ", " << momr 
	    << endl;
     }

     cout << " mcPoint:    " << flush;
     mcPoint.print();
     cout << " recoPointS: " << flush;
     recoPointS.print();
     cout << " recoPointR: " << flush;
     recoPointR.print();
     cout << " projPointS: " << flush;
     projPointS.print();     
     cout << " projPointR: " << flush;
     projPointR.print();
   } // End print statements

 } // End loop over totalMcRecoTracks (i)

 return True;
}

//--------------------------------------------------------------------------------
// Tec 25-09-2002 Sasha Lebedev (lebedev@iastate.edu)
PHBoolean PHEmbedMCEvaluator::associateTEC() {

  if (verbose>1) cout << "PHEmbedMCEvaluator::associateTEC()" << endl;

  CglTrack*     cgltrack1     = embedEvent->get_cgltrack1();
  CglTrack*     cgltrack3     = embedEvent->get_cgltrack3();

 if (!cgltrack3||!cgltrack1) {
   cerr << PHWHERE << "ERROR: CglTrack not found." << endl;
   return False;
 }

 TecOut*  tecout1 = embedEvent->get_tecout1();
 if (!tecout1) {
   cerr << PHWHERE << "ERROR: TecOut not found in sim node." << endl;
   return False;
 }
 TecOut*  techitout1 = embedEvent->get_techitout1();
 if (!techitout1) {
   cerr << PHWHERE << "ERROR: TecHitOut not found in sim node." << endl;
   return False;
 }
 TecOutV1*  tecout3 = embedEvent->get_tecout3();
 if (!tecout3) {
   cerr << PHWHERE << "ERROR: TecOutV1 not found in merged node." << endl;
   return False;
 }
 if (verbose>50) { 
   cout << "###### associateTEC1 (sim): " << techitout1->getNHits() << " " << tecout1->getNTracks() << endl; 
   cout << "###### associateTEC3 (mix): " << tecout3->getNHits() << " " << tecout3->getNTracks() << endl; 
 }
 int totalMcRecoTracks = mcRecoTrackList->length();
 int idTrackG = -1;
 int idTrackR = -1;
 int idTrackS = -1;
 if (verbose>50) {
   cout << "totalMcRecoTracks = " << totalMcRecoTracks << endl;
 }
 PHEmbedMcRecoTrack* mctrk = 0;

 float xinR=-999, xoutR=-999, yinR=-999, youtR=-999, zinR, zoutR;
 float xinS=-999, xoutS=-999, yinS=-999, youtS=-999, zinS, zoutS;

 for (int i=0; i<totalMcRecoTracks; i++) {

 mctrk = (*mcRecoTrackList)[i];

// if (mctrk->get_type()==1) {

   idTrackG    = mctrk->get_idGeant(); //same as mc_track
   idTrackS    = mctrk->get_dctrkidS();//same as dchtrackid in single particle node
   idTrackR    = mctrk->get_dctrkidR();//same as dchtrackid in merged node
   if (verbose>5) {
     cout << i << " " << idTrackG << " " << idTrackS << " " << idTrackR << " " << mctrk->get_type() << endl;
   }
   if (idTrackS>-1) {
     int tecidS = cgltrack1->get_tectrackid(idTrackS);
     if (verbose>5) {
       cout << " tecidS = " << tecidS << endl;
     }
     if (tecidS >=0) {
        xinS = tecout1->get_xin(tecidS);
        xoutS = tecout1->get_xout(tecidS);
	yinS = tecout1->get_yin(tecidS);
        youtS = tecout1->get_yout(tecidS);
        zinS = 1.;
	  if ((tecout1->get_index(tecidS)%2)==0) zinS = -1.;
        zoutS = zinS;
	if (verbose>20) {
	  cout << "MCxy: " << xinS << " " << yinS << " " << zinS << " " 
	       << xoutS << " " << youtS << " " << zoutS << endl;
	}
	mctrk->set_tecxinS(xinS);
	mctrk->set_tecxoutS(xoutS);
	mctrk->set_tecyinS(yinS);
	mctrk->set_tecyoutS(youtS);
	mctrk->set_teczinS(zinS);
	mctrk->set_teczoutS(zoutS);
     }
   }

   if (idTrackR>-1) {
     int tecidR = cgltrack3->get_tectrackid(idTrackR);
     if (verbose>5) {
       cout << " tecidR = " << tecidR << endl;
     }
     if (tecidR >=0) {
        xinR = tecout3->getTrackXin(tecidR);
        xoutR = tecout3->getTrackXout(tecidR);
        yinR = tecout3->getTrackYin(tecidR);
        youtR = tecout3->getTrackYout(tecidR);
        zinR = 1.;
	  if (tecout3->getTrackSide(tecidR)==0) zinR = -1.;
        zoutR = zinR;
	if (verbose>20) {
	  cout << "  xy: " << xinR << " " << yinR << " " << zinR << " " 
	       << xoutR << " " << youtR << " " << zoutR << endl;
	}
	mctrk->set_tecxinR(xinR);
	mctrk->set_tecxoutR(xoutR);
	mctrk->set_tecyinR(yinR);
	mctrk->set_tecyoutR(youtR);
	mctrk->set_teczinR(zinR);
	mctrk->set_teczoutR(zoutR);
     }
   }

       float distin = sqrt((xinR-xinS)*(xinR-xinS)+(yinR-yinS)*(yinR-yinS));
       float distout = sqrt((xoutR-xoutS)*(xoutR-xoutS)+(youtR-youtS)*(youtR-youtS));
       //float dist = sqrt(distin*distin+distout*distout);
       mctrk->set_tecdin(distin);
       mctrk->set_tecdout(distout);

// }
 } // end loop over mc tracks
 if (verbose>20) { 
   cout << "###### Number of mixed tec tracks = " << tecout3->getNTracks() << endl;
   for (int i=0; i<tecout3->getNTracks(); i++) {
     cout << i << " " << tecout3->getTrackXin(i) << " " << tecout3->getTrackYin(i) << " "
	  << tecout3->getTrackXout(i) << " " << tecout3->getTrackYout(i) << endl;
   }
 }

 return True;
}
//----------------------------------------------------------------------------------

PHBoolean PHEmbedMCEvaluator::associateTOF() {
  if (verbose>1) cout << "PHEmbedMCEvaluator::associateTOF()" << endl;
  CglTrack*     cgltrack1        = embedEvent->get_cgltrack1();
  PHTrackOut*   phtrack1         = embedEvent->get_phtrack1();
  CglTrack*     cgltrack3        = embedEvent->get_cgltrack3();
  PHTrackOut*   phtrack3         = embedEvent->get_phtrack3();

 if (!cgltrack3||!cgltrack1) {
   PHMessage("associatePC",PHWarning,"CglTrack table  not found in merged node tree");
   return False; 
 }
 if (!phtrack3||!phtrack1) {
   PHMessage("associatePC",PHWarning,"PHTrack table  not found in merged node tree");
   return False; 
 }
  TofOut*  tofout1 = embedEvent->get_tofout1();
  TofOut*  tofout3 = embedEvent->get_tofout3();
  dTofGdigiWrapper*          tofGdigi1         = embedEvent->get_tofGdigi1();
  dTofGdigiRecWrapper*       tofGdigiRec1      = embedEvent->get_tofGdigiRec1();
  if (!tofout1||!tofout3) {
    PHMessage("associateTOF",PHWarning,"TofOut not found ");
    return False; 
  }
  if (!tofGdigi1) {
    PHMessage("associateTOF",PHWarning," dTofGdigi table not found ");
    return False; 
  }
  if (!tofGdigiRec1) {
    PHMessage("associateTOF",PHWarning," dTofGdigiRec table not found ");
    return False; 
  }
  int totalMcRecoTracks = mcRecoTrackList->length();
  int idTrackG = -1;
  int idTrackS = -1;
  int idTrackR = -1;
  int tofidMc  = -1;
  int tofidS   = -1;
  int tofidR   = -1;

  int   tofpid;
  float toft; //e_loss
  float tofe; //e_loss
  float pltof;
  float x,y,z;
  PHPoint null(9999,9999,9999);
  PHPoint mcPoint;
  PHPoint recoPointS;
  PHPoint projPointS;
  PHPoint recoPointR;
  PHPoint projPointR;
  
  PHEmbedMcRecoTrack* mctrk = 0;


  /*
  cout << "******************************" << endl;
  cout << "Total Dch Mc Tracks : "  <<  totalDchMcRecoTracks  << endl;
  cout << "******************************" << endl;
  cout << "Number of tofGdigi  : "  <<  tofGdigi->RowCount()  << endl;
  cout << "******************************" << endl;
  cout << endl;
  */

  for (int i = 0; i <  totalMcRecoTracks; i++) { // Dch track loop    
    mctrk = (*mcRecoTrackList)[i];
    idTrackG    = mctrk->get_idGeant(); //same as mc_track
    idTrackS    = mctrk->get_dctrkidS();//same as dchtrackid in single particle node
    idTrackR    = mctrk->get_dctrkidR();//same as dchtrackid in merged node
    if (verbose>10) {
      cout << "idTrackG== " << idTrackG <<" idTrackS== " << idTrackS <<" idTrackR== " << idTrackR  << endl;
    }
    tofidMc = -1;tofidS = -1;tofidR = -1;
    mcPoint = null;recoPointS = null;projPointS = null;recoPointR = null;projPointR = null;
   //-------------------------------------------
   //------------- MONTE-CARLO information
    // Search TOF Gdigi for this track
    for (unsigned int j=0; j < tofGdigi1->RowCount(); j++) {
      if (idTrackG == tofGdigi1->get_mctrack(j)) {
	x       = tofGdigi1->get_pos_m(0,j);
	y       = tofGdigi1->get_pos_m(1,j);
	z       = tofGdigi1->get_pos_m(2,j);
	toft    = tofGdigi1->get_tof(j);
	tofe    = tofGdigi1->get_eloss(j);
	tofpid  = tofGdigi1->get_partl(j);
	mcPoint.setX(x);
	mcPoint.setY(y);
	mcPoint.setZ(z);

	mctrk->set_tofxMc(x);
	mctrk->set_tofyMc(y);
	mctrk->set_tofzMc(z);
	mctrk->set_toftMc(toft);
	mctrk->set_tofeMc(tofe);
	mctrk->set_tofpidMc(tofpid);
	break;
      }
    }
    for (unsigned int k =0; k < tofGdigiRec1->RowCount(); k++) {
      int tofghitid       = tofGdigiRec1->get_gdigiid(k);
      tofidMc             = tofGdigiRec1->get_recid(k);
      if (idTrackG == tofGdigi1->get_mctrack(tofghitid)) {
	//cout  << "=== ID Matched !! ==="  << endl;
	mctrk->set_tofidMc(tofidMc); 
	break;
      }
    }

    //------------- SIMULATION Information-----------------
    if (idTrackS>-1) {
      tofidS = cgltrack1->get_tofrecid(idTrackS);
    
      if (tofidS >=0) {
	x          = tofout1->get_xtof(tofidS,0); 
	y          = tofout1->get_xtof(tofidS,1); 
	z          = tofout1->get_xtof(tofidS,2); 
	toft       = tofout1->get_tof(tofidS);
	tofe       = tofout1->get_eloss(tofidS);
	tofpid     = -1;  // to be introduced 
	
	recoPointS.setX(x);
	recoPointS.setY(y);
	recoPointS.setZ(z);
	mctrk->set_tofidS(tofidS); // setting the tof reco id 
	mctrk->set_tofxS(x);
	mctrk->set_tofyS(y);
	mctrk->set_tofzS(z);
	mctrk->set_toftS(toft);
	mctrk->set_tofeS(tofe);
	mctrk->set_tofpidS(tofpid);
      }
      if (phtrack1->get_ifIntersectTof(idTrackS) ) {
	x      = phtrack1->get_projectionTof(idTrackS,0);
	y      = phtrack1->get_projectionTof(idTrackS,1);
	z      = phtrack1->get_projectionTof(idTrackS,2);
	pltof  = phtrack1->get_tofPathLength(idTrackS);
	projPointS.setX(x);
	projPointS.setY(y);
	projPointS.setZ(z);
	mctrk->set_ptofxS(x);
	mctrk->set_ptofyS(y);
	mctrk->set_ptofzS(z);
	mctrk->set_pltofS(pltof);
      }
    }
    //------------- RECONSTRUCTION information-------------
    if (idTrackR>-1) {
      tofidR = cgltrack3->get_tofrecid(idTrackR);
      if (tofidR >=0) {
	x          = tofout3->get_xtof(tofidR,0); 
	y          = tofout3->get_xtof(tofidR,1); 
	z          = tofout3->get_xtof(tofidR,2); 
	toft       = tofout3->get_tof(tofidR);
	tofe       = tofout3->get_eloss(tofidR);
	tofpid     = -1;  // to be introduced 
	
	recoPointR.setX(x);
	recoPointR.setY(y);
	recoPointR.setZ(z);
	mctrk->set_tofidR(tofidR); // setting the tof reco id 
	mctrk->set_tofxR(x);
	mctrk->set_tofyR(y);
	mctrk->set_tofzR(z);
	mctrk->set_toftR(toft);
	mctrk->set_tofeR(tofe);
	mctrk->set_tofpidR(tofpid);	
      }
      if (phtrack3->get_ifIntersectTof(idTrackR) ) {
	x      = phtrack3->get_projectionTof(idTrackR,0);
	y      = phtrack3->get_projectionTof(idTrackR,1);
	z      = phtrack3->get_projectionTof(idTrackR,2);
	pltof  = phtrack3->get_tofPathLength(idTrackR);
	projPointR.setX(x);
	projPointR.setY(y);
	projPointR.setZ(z);
	mctrk->set_ptofxR(x);
	mctrk->set_ptofyR(y);
	mctrk->set_ptofzR(z);
	mctrk->set_pltofR(pltof);
      }
    }
    if (verbose>20) {
      cout << "tofid : mc " <<  mctrk->get_tofidMc() << " simu " << mctrk->get_tofidS() << " reco " << mctrk->get_tofidR()  << endl;
      cout << mcPoint << " toftMc " << mctrk->get_toftMc() << " deleMce " << 
	mctrk->get_tofeMc() << " tofpidMc " << mctrk->get_tofpidMc() << endl;
      cout << recoPointS << " tofS "  << mctrk->get_toftS()  <<  "deleS : "  << mctrk->get_tofeS()  <<  endl;
      cout << projPointS << " pathlS " << mctrk->get_pltofS() << endl;
      cout << recoPointR << " tofR "  << mctrk->get_toftR()  <<  "deleR : "  << mctrk->get_tofeR()  << endl;
      cout << projPointR << " pathlR " << mctrk->get_pltofR() << endl;
    }
  }
  return True;
}

PHBoolean PHEmbedMCEvaluator::associateTOFW()
{
  //cout<<" associateTOFW "<<endl;
  CglTrack*     cgltrack1        = embedEvent->get_cgltrack1();
  PHTrackOut*   phtrack1         = embedEvent->get_phtrack1();
  CglTrack*     cgltrack3        = embedEvent->get_cgltrack3();
  PHTrackOut*   phtrack3         = embedEvent->get_phtrack3();

  if(!cgltrack3||!cgltrack1)
    {
      PHMessage("associateTOFW",PHWarning,"CglTrack table  not found in merged node tree");
      return False; 
    }
  if(!phtrack3||!phtrack1)
    {
      PHMessage("associateTOFW",PHWarning,"PHTrack table  not found in merged node tree");
      return False; 
    }
  
  TofwHit*  tofwhit1 = embedEvent->get_tofwhit1();
  TofwHit*  tofwhit3 = embedEvent->get_tofwhit3();

  //TofwHit*  tofwhit2 = embedEvent->get_tofwhit2();

  //dTofGdigiWrapper*          tofGdigi1         = embedEvent->get_tofGdigi1();
  //dTofGdigiRecWrapper*       tofGdigiRec1      = embedEvent->get_tofGdigiRec1();

  if(!tofwhit1||!tofwhit3)
    {
      PHMessage("associateTOFW",PHWarning,"TofwHit not found ");
      return False; 
    }
  //if(!tofwGdigi1)
  //  {
  //    PHMessage("associateTOF",PHWarning," dTofGdigi table not found ");
  //    return False; 
  //  }
  //if(!tofwGdigiRec1)
  //  {
  //    PHMessage("associateTOF",PHWarning," dTofGdigiRec table not found ");
  //    return False; 
  //  }

  int totalMcRecoTracks = mcRecoTrackList->length();
  //cout<<"what it is: "<<totalMcRecoTracks<<endl;

  int idTrackG = -1;
  int idTrackS = -1;
  int idTrackR = -1;
  int tofwidS   = -1;
  int tofwidR   = -1;
  
  int   tofwpid;
  float tofwt;
  float tofwe;
  float tofwtdcup;
  float tofwadcup;
  float tofwtdcdn;
  float tofwadcdn;

  float pltofw;
  float x,y,z;
  int strip;
  PHPoint null(9999,9999,9999);
  PHPoint mcPoint;
  PHPoint recoPointS;
  PHPoint projPointS;
  PHPoint recoPointR;
  PHPoint projPointR;
  
  PHEmbedMcRecoTrack* mctrk = 0;


  /*
  cout << "******************************"<<endl;
  cout << "Total Dch Mc Tracks : " << totalDchMcRecoTracks <<endl;
  cout << "******************************"<<endl;
  cout << "Number of tofwGdigi  : " << tofwGdigi->RowCount() <<endl;
  cout << "******************************"<<endl;
  cout << endl;
  */

  for(int i = 0; i <  totalMcRecoTracks; i++)
    { // Dch track loop    
      mctrk = (*mcRecoTrackList)[i];
      idTrackG    = mctrk->get_idGeant(); //same as mc_track
      idTrackS    = mctrk->get_dctrkidS();//same as dchtrackid in single particle node
      idTrackR    = mctrk->get_dctrkidR();//same as dchtrackid in merged node

      int partidG  =  mctrk->get_partidG();
      if(verbose>10)
	{
	  cout << "idTrackG== " << idTrackG <<" idTrackS== " << idTrackS <<" idTrackR== " << idTrackR 
	       <<" partidG== " << partidG <<endl;
	}
      tofwidS = -1;
      tofwidR = -1;
      mcPoint = null;
      recoPointS = null;
      projPointS = null;
      recoPointR = null;
      projPointR = null;



      //------------- SIMULATION Information-----------------
      if(idTrackS>-1)
	{
	  tofwidS = cgltrack1->get_tofwrecid(idTrackS);
	  
	  if(tofwidS>=0)
	    {
	      //need put the status here

	      strip      = tofwhit1->get_stripid(tofwidS);
	      x          = tofwhit1->get_xyz(tofwidS,0); 
	      y          = tofwhit1->get_xyz(tofwidS,1); 
	      z          = tofwhit1->get_xyz(tofwidS,2); 
	      tofwt      = tofwhit1->get_time(tofwidS);
	      tofwe      = tofwhit1->get_charge(tofwidS);
	      tofwtdcup  = tofwhit1->get_rawtdc(tofwidS,0);
	      tofwadcup  = tofwhit1->get_rawadc(tofwidS,0);
	      tofwtdcdn  = tofwhit1->get_rawtdc(tofwidS,1);
	      tofwadcdn  = tofwhit1->get_rawadc(tofwidS,1);


	      tofwpid     = -1;  // to be introduced 
	      
	      recoPointS.setX(x);
	      recoPointS.setY(y);
	      recoPointS.setZ(z);
	      mctrk->set_tofwidS(tofwidS); // setting the tofw reco id 
	      mctrk->set_tofwstripS(strip);
	      mctrk->set_tofwxS(x);
	      mctrk->set_tofwyS(y);
	      mctrk->set_tofwzS(z);
	      mctrk->set_tofwtS(tofwt);
	      mctrk->set_tofweS(tofwe);
	      mctrk->set_tofwtdcupS(tofwtdcup);
	      mctrk->set_tofwadcupS(tofwadcup);
	      mctrk->set_tofwtdcdnS(tofwtdcdn);
	      mctrk->set_tofwadcdnS(tofwadcdn);
	      mctrk->set_tofwpidS(tofwpid);
	      /*
	      cout<<" MC "
		  <<" tofwt= "<<tofwt
		  <<" x= "<<x
                  <<" y= "<<y
		  <<" z= "<<z<<endl;
	      */
	    }
	  if(phtrack1->ifIntersectTofw(idTrackS))//no get_ifIntersectTofw(int) function in cgl
	    {
	      x      = phtrack1->get_projectionTofw(idTrackS,0);
	      y      = phtrack1->get_projectionTofw(idTrackS,1);
	      z      = phtrack1->get_projectionTofw(idTrackS,2);
	      pltofw  = phtrack1->get_tofwPathLength(idTrackS);
	      projPointS.setX(x);
	      projPointS.setY(y);
	      projPointS.setZ(z);
	      mctrk->set_ptofwxS(x);
	      mctrk->set_ptofwyS(y);
	      mctrk->set_ptofwzS(z);
	      mctrk->set_pltofwS(pltofw);
	    }
	}


      //------------- RECONSTRUCTION information-------------
      if(idTrackR>-1)
	{
	  tofwidR = cgltrack3->get_tofwrecid(idTrackR);
	  //cout<<"*************  tofwidR is "<<tofwidR<<endl;

	  if(tofwidR>=0)
	    {
	      strip      = tofwhit3->get_stripid(tofwidR);
	      x          = tofwhit3->get_xyz(tofwidR,0); 
	      y          = tofwhit3->get_xyz(tofwidR,1); 
	      z          = tofwhit3->get_xyz(tofwidR,2); 
	      tofwt      = tofwhit3->get_time(tofwidR);
	      tofwe      = tofwhit3->get_charge(tofwidR);
	      tofwtdcup  = tofwhit1->get_rawtdc(tofwidR,0);
	      tofwadcup  = tofwhit1->get_rawadc(tofwidR,0);
	      tofwtdcdn  = tofwhit1->get_rawtdc(tofwidR,1);
	      tofwadcdn  = tofwhit1->get_rawadc(tofwidR,1);
	      tofwpid     = -1;  // to be introduced 
	
	      recoPointR.setX(x);
	      recoPointR.setY(y);
	      recoPointR.setZ(z);
	      mctrk->set_tofwidR(tofwidR); // setting the tofw reco id 
	      mctrk->set_tofwstripR(strip);
	      mctrk->set_tofwxR(x);
	      mctrk->set_tofwyR(y);
	      mctrk->set_tofwzR(z);
	      mctrk->set_tofwtR(tofwt);
	      mctrk->set_tofweR(tofwe);
	      mctrk->set_tofwtdcupR(tofwtdcup);
	      mctrk->set_tofwadcupR(tofwadcup);
	      mctrk->set_tofwtdcdnR(tofwtdcdn);
	      mctrk->set_tofwadcdnR(tofwadcdn);
	      mctrk->set_tofwpidR(tofwpid);	
	      /*
	      cout<<" RC "
		  <<" tofwt= "<<tofwt
		  <<" x= "<<x
                  <<" y= "<<y
		  <<" z= "<<z<<endl;
	      */
	    }
	  if(phtrack3->ifIntersectTofw(idTrackR))//no get_ifIntersectTofw(int) function in cgl
	    {
	      x      = phtrack3->get_projectionTofw(idTrackR,0);
	      y      = phtrack3->get_projectionTofw(idTrackR,1);
	      z      = phtrack3->get_projectionTofw(idTrackR,2);
	      pltofw  = phtrack3->get_tofwPathLength(idTrackR);
	      projPointR.setX(x);
	      projPointR.setY(y);
	      projPointR.setZ(z);
	      mctrk->set_ptofwxR(x);
	      mctrk->set_ptofwyR(y);
	      mctrk->set_ptofwzR(z);
	      mctrk->set_pltofwR(pltofw);
	    }
	}

      if(verbose>20)
	{
	  cout << "tofwid : mc "<< mctrk->get_tofwidMc()<<" simu "<<mctrk->get_tofwidS()<<" reco "<<mctrk->get_tofwidR() <<endl;
	  cout<<mcPoint<<" tofwtMc "<<mctrk->get_tofwtMc()<<" deleMce "<<
	    mctrk->get_tofweMc()<<" tofwpidMc "<<mctrk->get_tofwpidMc()<<endl;
	  cout<<recoPointS<<" tofwS " <<mctrk->get_tofwtS() << "deleS : " <<mctrk->get_tofweS() << endl;
	  cout<<projPointS<<" pathlS "<<mctrk->get_pltofwS()<<endl;
	  cout<<recoPointR<<" tofwR " <<mctrk->get_tofwtR() << "deleR : " <<mctrk->get_tofweR() <<endl;
	  cout<<projPointR<<" pathlR "<<mctrk->get_pltofwR()<<endl;
	}
    }
  return True;
}

PHBoolean PHEmbedMCEvaluator::associateCRK() {
  if (verbose>1) cout << " associateCRK" << endl;
  CglTrack*           cgltrack1     = embedEvent->get_cgltrack1();
  PHTrackOut*         phtrack1      = embedEvent->get_phtrack1();
  DchTrack*           dchtrack1     = embedEvent->get_dchtrack1();
  PadCluster*         pc1cluster1   = embedEvent->get_pc1cluster1();
  PadCluster*         pc3cluster1   = embedEvent->get_pc3cluster1();
  emcClusterContainer*emccluster1   = embedEvent->get_emccluster1();
  CglTrack*           cgltrack3     = embedEvent->get_cgltrack3();
  PHTrackOut*         phtrack3      = embedEvent->get_phtrack3();
  DchTrack*           dchtrack3     = embedEvent->get_dchtrack3();
  PadCluster*         pc1cluster3   = embedEvent->get_pc1cluster3();
  PadCluster*         pc3cluster3   = embedEvent->get_pc3cluster3();
  emcClusterContainer*emccluster3   = embedEvent->get_emccluster3();
  
  if (!cgltrack3||!cgltrack1) {
    PHMessage("associateCRK",PHWarning,"CglTrack table  not found in merged node tree");
    return False; 
  }
  if (!phtrack3||!phtrack1) {
    PHMessage("associateCRK",PHWarning,"PHTrack table  not found in merged node tree");
    return False; 
  }
  if (!dchtrack3||!dchtrack1) {
    PHMessage("associateCRK",PHWarning,"DchTrack table  not found in merged node tree");
    return False; 
  }
  if (!pc1cluster3||!pc1cluster1) {
    PHMessage("associateCRK",PHWarning,"PC1Cluster table  not found in merged node tree");
    return False; 
  }
  if (!pc3cluster3||!pc3cluster1) {
    PHMessage("associateCRK",PHWarning,"PC3Cluster table  not found in merged node tree");
    return False; 
  }
  if (!emccluster3||!emccluster1) {
    PHMessage("associateCRK",PHWarning,"emcClusterContainer table  not found in merged node tree");
    return False; 
  }

  int totalMcRecoTracks = mcRecoTrackList->length();
  int idTrackG = -1;
  int idTrackS = -1;
  int idTrackR = -1;
  int pc1RecoId1, pc3RecoId1, emcRecoId1;
  int pc1RecoId3, pc3RecoId3, emcRecoId3;
  PHEmbedMcRecoTrack* mctrk = 0;
  for (int i = 0; i< totalMcRecoTracks; i++ ) {
    mctrk = (*mcRecoTrackList)[i];
    idTrackG    = mctrk->get_idGeant(); 
    idTrackS    = mctrk->get_dctrkidS();
    idTrackR    = mctrk->get_dctrkidR();
    if (verbose>30) {
      cout << "idTrackG == " << idTrackG 
	   << " idTrackS == " << idTrackS 
	   << " idTrackR == " << idTrackR <<endl;
    }
    if ( idTrackS>=0 ) {
      PHPoint null(-99999,-99999,-99999);
      
      pc1RecoId1 = dchtrack1->get_hits(idTrackS,39);
      pc3RecoId1 = cgltrack1->get_pc3clusid(idTrackS);
      emcRecoId1 = cgltrack1->get_emcclusid(idTrackS);
      
      if ( pc1RecoId1>=0 && 
	   phtrack1->get_ifIntersectPc2(idTrackS) ) {
	
	PHPoint pstart(pc1cluster1->get_xyz(pc1RecoId1,0),
		       pc1cluster1->get_xyz(pc1RecoId1,1),
		       pc1cluster1->get_xyz(pc1RecoId1,2));
	PHPoint pend(phtrack1->get_projectionPc2(idTrackS,0),
		     phtrack1->get_projectionPc2(idTrackS,1),
		     phtrack1->get_projectionPc2(idTrackS,2));
	
	if ( pc1cluster1->get_xyz(pc1RecoId1,0)>0.0 ) {
	  if ( emcRecoId1>=0 ) {
	    emcClusterContent *clusC = emccluster1->getCluster(emcRecoId1);
	    if ( clusC->ecore()>0.1 ) {
	      pend = PHPoint(clusC->x(),clusC->y(),clusC->z());
	    }
	  }
	} else {
	  if ( pc3RecoId1>=0 ) {
	    pend = PHPoint(pc3cluster1->get_xyz(pc3RecoId1,0),
			   pc3cluster1->get_xyz(pc3RecoId1,1),
			   pc3cluster1->get_xyz(pc3RecoId1,2));
	  }
	}
	
	PHLine rich_proj1(pstart,pend);
	CrkPIDout rich1;
	d_crkpid->SetCrkHitFromTop(node1);
	d_crkpid->AssociateTrack(rich_proj1,&rich1);
	
	if (verbose>30) {
	  cout << "Results of CrkPID SIMULATION:" << endl;
	  cout << "accept: " << rich1.accepted << endl;
	  cout << "npmt0: " << rich1.npmt0 << endl;
	  cout << "npmt1: " << rich1.npmt1 << endl;
	  cout << "npmt3: " << rich1.npmt3 << endl;
	  cout << "npe0: " << rich1.npe0 << endl;
	  cout << "npe1: " << rich1.npe1 << endl;
	  cout << "npe3: " << rich1.npe3 << endl;
	  cout << "chi2: " << rich1.chi2 << endl;
	  cout << "disp: " << rich1.disp << endl;
	  cout << "path: " << rich1.path << endl;
	}
	if ( rich1.accepted )
	  mctrk->set_crkaccS(1);
	else
	  mctrk->set_crkaccS(0);
	mctrk->set_crknpmt0S(rich1.npmt0);
	mctrk->set_crknpmt1S(rich1.npmt1);
	mctrk->set_crknpmt3S(rich1.npmt3);
	mctrk->set_crknpe0S(rich1.npe0);
	mctrk->set_crknpe1S(rich1.npe1);
	mctrk->set_crknpe3S(rich1.npe3);
	mctrk->set_crkchi2S(rich1.chi2);
	mctrk->set_crkdispS(rich1.disp);
	mctrk->set_crkpathS(rich1.path);
      }
    }
    if ( idTrackR>=0 ) {
      pc1RecoId3 = dchtrack3->get_hits(idTrackR,39);
      pc3RecoId3 = cgltrack3->get_pc3clusid(idTrackR);
      emcRecoId3 = cgltrack3->get_emcclusid(idTrackR);
      if ( pc1RecoId3>=0 && 
	   phtrack3->get_ifIntersectPc2(idTrackR) ) {
	
	PHPoint pstart(pc1cluster3->get_xyz(pc1RecoId3,0),
		       pc1cluster3->get_xyz(pc1RecoId3,1),
		       pc1cluster3->get_xyz(pc1RecoId3,2));
	PHPoint pend(phtrack3->get_projectionPc2(idTrackR,0),
		     phtrack3->get_projectionPc2(idTrackR,1),
		     phtrack3->get_projectionPc2(idTrackR,2));
	
	if ( pc1cluster3->get_xyz(pc1RecoId3,0)>0.0 ) {
	  if ( emcRecoId3>=0 ) {
	    emcClusterContent *clusC = emccluster3->getCluster(emcRecoId3);
	    if ( clusC->ecore()>0.1 ) {
	      pend = PHPoint(clusC->x(),clusC->y(),clusC->z());
	    }
	  }
	} else {
	  if ( pc3RecoId3>=0 ) {
	    pend = PHPoint(pc3cluster3->get_xyz(pc3RecoId3,0),
			   pc3cluster3->get_xyz(pc3RecoId3,1),
			   pc3cluster3->get_xyz(pc3RecoId3,2));
	  }
	}
	
	PHLine rich_proj3(pstart,pend);
	CrkPIDout rich3;
	d_crkpid->SetCrkHitFromTop(node3);
	d_crkpid->AssociateTrack(rich_proj3,&rich3);
	
	if (verbose>30) {
	  cout << "Results of CrkPID EMBEDED:" << endl;
	  cout << "accept: " << rich3.accepted << endl;
	  cout << "npmt0: " << rich3.npmt0 << endl;
	  cout << "npmt1: " << rich3.npmt1 << endl;
	  cout << "npmt3: " << rich3.npmt3 << endl;
	  cout << "npe0: " << rich3.npe0 << endl;
	  cout << "npe1: " << rich3.npe1 << endl;
	  cout << "npe3: " << rich3.npe3 << endl;
	  cout << "chi2: " << rich3.chi2 << endl;
	  cout << "disp: " << rich3.disp << endl;
	  cout << "path: " << rich3.path << endl;
	}
	if ( rich3.accepted )
	  mctrk->set_crkaccR(1);
	else
	  mctrk->set_crkaccR(0);
	mctrk->set_crknpmt0R(rich3.npmt0);
	mctrk->set_crknpmt1R(rich3.npmt1);
	mctrk->set_crknpmt3R(rich3.npmt3);
	mctrk->set_crknpe0R(rich3.npe0);
	mctrk->set_crknpe1R(rich3.npe1);
	mctrk->set_crknpe3R(rich3.npe3);
	mctrk->set_crkchi2R(rich3.chi2);
	mctrk->set_crkdispR(rich3.disp);
	mctrk->set_crkpathR(rich3.path);
      }
    }
  }
  return True;
}

PHBoolean PHEmbedMCEvaluator::associateEMC() {
  if (verbose>1) cout << " associateEMC" << endl;
  
  dEmcGeaClusterTrackWrapper* emcgeaclustertrack1 
    = embedEvent->get_emcgeaclustertrack1();
  
  CglTrack* cgltrack1              = embedEvent->get_cgltrack1();
  PHTrackOut* phtrack1             = embedEvent->get_phtrack1();
  emcClusterContainer* emccluster1 = embedEvent->get_emccluster1();
  CglTrack* cgltrack3              = embedEvent->get_cgltrack3();
  PHTrackOut* phtrack3             = embedEvent->get_phtrack3();
  emcClusterContainer* emccluster3 = embedEvent->get_emccluster3();
  
  if (!cgltrack3||!cgltrack1) {
    PHMessage("associateEMC", PHWarning,
	      "CglTrack table  not found in merged node tree");
    return False; 
  }
  if (!phtrack3||!phtrack1) {
    PHMessage("associateEMC", PHWarning,
	      "PHTrack table  not found in merged node tree");
    return False; 
  }
  if (!emccluster3||!emccluster1) {
    PHMessage("associateEMC", PHWarning,
	      "emcClusterContainer not found in merged node tree");
    return False; 
  }
  if (!emcgeaclustertrack1) {
    PHMessage("associateEMC", PHWarning,
	      "EMCGeaCluster table  not found in merged node tree");
    return False; 
  }

  if (verbose > 10) cout << "Getting totalMcRecoTracks..." << flush;
  int totalMcRecoTracks = mcRecoTrackList->length();
  if (verbose > 10) cout << totalMcRecoTracks << endl;
  int idTrackG = -1;
  int idTrackS = -1;
  int idTrackR = -1;
  int emcRecoId1, emcRecoId3;
  float pemcx, pemcy, pemcz;
  float plemc;

  PHEmbedMcRecoTrack* mctrk = 0;
  for (int i = 0; i< totalMcRecoTracks; i++ ) {
    mctrk = (*mcRecoTrackList)[i];
    idTrackG    = mctrk->get_idGeant(); 
    idTrackS    = mctrk->get_dctrkidS();
    idTrackR    = mctrk->get_dctrkidR();
    if (verbose>10) {
      cout << "idTrackG == " << idTrackG 
	   << " idTrackS == " << idTrackS 
	   << " idTrackR == " << idTrackR <<endl;
    }
    if ( idTrackS>=0 ) {
      emcRecoId1 = cgltrack1->get_emcclusid(idTrackS);
      if (emcRecoId1 >= 0) {
	// Ancestry
	for (unsigned int j=0; j<emcgeaclustertrack1->RowCount(); j++) {
	  if ( emcgeaclustertrack1->get_clusid(j)!=emcRecoId1 ) continue;
	  mctrk->set_emctrkno0Mc(emcgeaclustertrack1->get_trkno(0,j));
	  mctrk->set_emctrkno1Mc(emcgeaclustertrack1->get_trkno(1,j));
	  mctrk->set_emctrkno2Mc(emcgeaclustertrack1->get_trkno(2,j));
	  mctrk->set_emctwrhit0Mc(emcgeaclustertrack1->get_tracktwrhit(0,j));
	  mctrk->set_emctwrhit1Mc(emcgeaclustertrack1->get_tracktwrhit(1,j));
	  mctrk->set_emctwrhit2Mc(emcgeaclustertrack1->get_tracktwrhit(2,j));
	  mctrk->set_emcpid0Mc((int)emcgeaclustertrack1->get_pid(0,j));
	  mctrk->set_emcpid1Mc((int) emcgeaclustertrack1->get_pid(1,j));
	  mctrk->set_emcpid2Mc((int) emcgeaclustertrack1->get_pid(2,j));
	  mctrk->set_emcedep0Mc(emcgeaclustertrack1->get_edep(0,j));
	  mctrk->set_emcedep1Mc(emcgeaclustertrack1->get_edep(1,j));
	  mctrk->set_emcedep2Mc(emcgeaclustertrack1->get_edep(2,j));
	  mctrk->set_emcptot0Mc(emcgeaclustertrack1->get_ptot(0,j));
	  mctrk->set_emcptot1Mc(emcgeaclustertrack1->get_ptot(1,j));
	  mctrk->set_emcptot2Mc(emcgeaclustertrack1->get_ptot(2,j));
	}
	// Reconstruction (single particle)
	emcClusterContent *clusC = emccluster1->getCluster(emcRecoId1);
	mctrk->set_emcidS(emcRecoId1);
	mctrk->set_emcxS(clusC->x());
	mctrk->set_emcyS(clusC->y());
	mctrk->set_emczS(clusC->z());

	int swkey = 100000*clusC->arm() +
	  10000*clusC->sector() +
	  100*clusC->iypos()+
	  clusC->izpos();

	mctrk->set_emcswkeyS(swkey);
	mctrk->set_emcmeaseS(clusC->e());
	mctrk->set_emcecoreS(clusC->ecore());
	mctrk->set_emcecorrS(-1);
	mctrk->set_emcecentS(clusC->ecent());
	mctrk->set_emctofS(clusC->tof());
	mctrk->set_emctofcorrS(clusC->tofcorr());
	mctrk->set_emctofminS(clusC->tofmin());
	mctrk->set_emcprobphotS(clusC->prob_photon());
	mctrk->set_emctwrhitS(clusC->multiplicity());
	mctrk->set_emcchi2S(clusC->chi2());
	mctrk->set_emcpartesum0S(-1000);
	mctrk->set_emcpartesum1S(-1000);
	mctrk->set_emcpartesum2S(-1000);
	mctrk->set_emcpartesum3S(-1000);
	if (clusC->multiplicity()>0)        mctrk->set_emcpartesum0S(clusC->partesum(0));
	else if (clusC->multiplicity()>1)   mctrk->set_emcpartesum1S(clusC->partesum(1));
	else if (clusC->multiplicity()>2)   mctrk->set_emcpartesum2S(clusC->partesum(2));
	else if (clusC->multiplicity()>3)   mctrk->set_emcpartesum3S(clusC->partesum(3));
      }
      if (phtrack1->get_ifIntersectPbsc(idTrackS) ) { 
	pemcx = phtrack1->get_projectionPbSc(idTrackS,0);
	pemcy = phtrack1->get_projectionPbSc(idTrackS,1);
	pemcz = phtrack1->get_projectionPbSc(idTrackS,2);
	plemc = phtrack1->get_emcPathLength(idTrackS);
	mctrk->set_pemcxS(pemcx);
	mctrk->set_pemcyS(pemcy);
	mctrk->set_pemczS(pemcz);
	mctrk->set_plemcS(plemc);
      }
      if (phtrack1->get_ifIntersectPbgl(idTrackS) ) { 
	pemcx = phtrack1->get_projectionPbGl(idTrackS,0);
	pemcy = phtrack1->get_projectionPbGl(idTrackS,1);
	pemcz = phtrack1->get_projectionPbGl(idTrackS,2);
	plemc = phtrack1->get_emcPathLength(idTrackS);
	mctrk->set_pemcxS(pemcx);
	mctrk->set_pemcyS(pemcy);
	mctrk->set_pemczS(pemcz);
	mctrk->set_plemcS(plemc);
      }
    }
    if ( idTrackR>=0 ) {
      emcRecoId3 = cgltrack3->get_emcclusid(idTrackR);
      if (emcRecoId3 >= 0) {
	// Reconstruction (merged)
	emcClusterContent *clusC = emccluster3->getCluster(emcRecoId3);
	mctrk->set_emcidR(emcRecoId3);
	mctrk->set_emcxR(clusC->x());
	mctrk->set_emcyR(clusC->y());
	mctrk->set_emczR(clusC->z());
	int swkey = 100000*clusC->arm() +
	  10000*clusC->sector() +
	  100*clusC->iypos()+
	  clusC->izpos();

	mctrk->set_emcswkeyR(swkey);
	mctrk->set_emcmeaseR(clusC->e());
	mctrk->set_emcecoreR(clusC->ecore());
	mctrk->set_emcecorrR(-1);
	mctrk->set_emcecentR(clusC->ecent());
	mctrk->set_emctofR(clusC->tof());
	mctrk->set_emctofcorrR(clusC->tofcorr());
	mctrk->set_emctofminR(clusC->tofmin());
	mctrk->set_emcprobphotR(clusC->prob_photon());
	mctrk->set_emctwrhitR(clusC->multiplicity());
	mctrk->set_emcchi2R(clusC->chi2());

	mctrk->set_emcpartesum0R(-1000);
	mctrk->set_emcpartesum1R(-1000);
	mctrk->set_emcpartesum2R(-1000);
	mctrk->set_emcpartesum3R(-1000);
	if (clusC->multiplicity()>0)        mctrk->set_emcpartesum0R(clusC->partesum(0));
	else if (clusC->multiplicity()>1)   mctrk->set_emcpartesum1R(clusC->partesum(1));
	else if (clusC->multiplicity()>2)   mctrk->set_emcpartesum2R(clusC->partesum(2));
	else if (clusC->multiplicity()>3)   mctrk->set_emcpartesum3R(clusC->partesum(3));
      }
      if (phtrack3->get_ifIntersectPbsc(idTrackR) ) { 
	pemcx = phtrack3->get_projectionPbSc(idTrackR,0);
	pemcy = phtrack3->get_projectionPbSc(idTrackR,1);
	pemcz = phtrack3->get_projectionPbSc(idTrackR,2);
	plemc = phtrack3->get_emcPathLength(idTrackR);
	mctrk->set_pemcxR(pemcx);
	mctrk->set_pemcyR(pemcy);
	mctrk->set_pemczR(pemcz);
	mctrk->set_plemcR(plemc);
      }
      if (phtrack3->get_ifIntersectPbgl(idTrackR) ) { 
	pemcx = phtrack3->get_projectionPbGl(idTrackR,0);
	pemcy = phtrack3->get_projectionPbGl(idTrackR,1);
	pemcz = phtrack3->get_projectionPbGl(idTrackR,2);
	plemc = phtrack3->get_emcPathLength(idTrackR);
	mctrk->set_pemcxR(pemcx);
	mctrk->set_pemcyR(pemcy);
	mctrk->set_pemczR(pemcz);
	mctrk->set_plemcR(plemc);
      }
      if (verbose>30) {
	cout << "Results of associateEMC (ancestry)" << endl;
	cout << "trkno0: " << mctrk->get_emctrkno0Mc() << endl;
	cout << "trkno1: " << mctrk->get_emctrkno1Mc() << endl;
	cout << "trkno2: " << mctrk->get_emctrkno2Mc() << endl;
	cout << "twrhit0: " << mctrk->get_emctwrhit0Mc() << endl;
	cout << "twrhit1: " << mctrk->get_emctwrhit1Mc() << endl;
	cout << "twrhit2: " << mctrk->get_emctwrhit2Mc() << endl;
	cout << "pid0: " << mctrk->get_emcpid0Mc() << endl;
	cout << "pid1: " << mctrk->get_emcpid1Mc() << endl;
	cout << "pid2: " << mctrk->get_emcpid2Mc() << endl;
	cout << "edep0: " << mctrk->get_emcedep0Mc() << endl;
	cout << "edep1: " << mctrk->get_emcedep1Mc() << endl;
	cout << "edep2: " << mctrk->get_emcedep2Mc() << endl;
	cout << "ptot0: " << mctrk->get_emcptot0Mc() << endl;
	cout << "ptot1: " << mctrk->get_emcptot1Mc() << endl;
	cout << "ptot2: " << mctrk->get_emcptot2Mc() << endl;
	cout << "Results of associateEMC (single particle)" << endl;
	cout << "x: " << mctrk->get_emcxS() << endl;
	cout << "y: " << mctrk->get_emcyS() << endl;
	cout << "z: " << mctrk->get_emczS() << endl;
	cout << "px: " << mctrk->get_pemcxS() << endl;
	cout << "py: " << mctrk->get_pemcyS() << endl;
	cout << "pz: " << mctrk->get_pemczS() << endl;
	cout << "swkey: " << mctrk->get_emcswkeyS() << endl;
	cout << "mease: " << mctrk->get_emcmeaseS() << endl;
	cout << "ecore: " << mctrk->get_emcecoreS() << endl;
	cout << "ecent: " << mctrk->get_emcecentS() << endl;
	cout << "ecorr: " << mctrk->get_emcecorrS() << endl;
	cout << "tof: " << mctrk->get_emctofS() << endl;
	cout << "tofcorr: " << mctrk->get_emctofcorrS() << endl;
	cout << "tofmin: " << mctrk->get_emctofminS() << endl;
	cout << "probphot: " << mctrk->get_emcprobphotS() << endl;
	cout << "twrhit: " << mctrk->get_emctwrhitS() << endl;
	cout << "chi2: " << mctrk->get_emcchi2S() << endl;
	cout << "partesum0: " << mctrk->get_emcpartesum0S() << endl;
	cout << "partesum1: " << mctrk->get_emcpartesum1S() << endl;
	cout << "partesum2: " << mctrk->get_emcpartesum2S() << endl;
	cout << "partesum3: " << mctrk->get_emcpartesum3S() << endl;
	cout << "Results of associateEMC (merged)" << endl;
	cout << "x: " << mctrk->get_emcxR() << endl;
	cout << "y: " << mctrk->get_emcyR() << endl;
	cout << "z: " << mctrk->get_emczR() << endl;
	cout << "px: " << mctrk->get_pemcxR() << endl;
	cout << "py: " << mctrk->get_pemcyR() << endl;
	cout << "pz: " << mctrk->get_pemczR() << endl;
	cout << "swkey: " << mctrk->get_emcswkeyR() << endl;
	cout << "mease: " << mctrk->get_emcmeaseR() << endl;
	cout << "ecore: " << mctrk->get_emcecoreR() << endl;
	cout << "ecent: " << mctrk->get_emcecentR() << endl;
	cout << "ecorr: " << mctrk->get_emcecorrR() << endl;
	cout << "tof: " << mctrk->get_emctofR() << endl;
	cout << "tofcorr: " << mctrk->get_emctofcorrR() << endl;
	cout << "tofmin: " << mctrk->get_emctofminR() << endl;
	cout << "probphot: " << mctrk->get_emcprobphotR() << endl;
	cout << "twrhit: " << mctrk->get_emctwrhitR() << endl;
	cout << "chi2: " << mctrk->get_emcchi2R() << endl;
	cout << "partesum0: " << mctrk->get_emcpartesum0R() << endl;
	cout << "partesum1: " << mctrk->get_emcpartesum1R() << endl;
	cout << "partesum2: " << mctrk->get_emcpartesum2R() << endl;
	cout << "partesum3: " << mctrk->get_emcpartesum3R() << endl;
      }
    }
  }
  return True;
}
PHBoolean PHEmbedMCEvaluator::associateACC() {
  if (verbose>1) {
    cout << " associateACC" << endl;
  }

  static const unsigned int NBOXES=4;

  CglTrack*             cgltrack1          = embedEvent->get_cgltrack1();
  PHTrackOut*           phtrack1           = embedEvent->get_phtrack1();
  AccRaw*               accraw1            = embedEvent->get_accraw1();
  CglTrack*             cgltrack3          = embedEvent->get_cgltrack3();
  PHTrackOut*           phtrack3           = embedEvent->get_phtrack3();
  AccRaw*               accraw3            = embedEvent->get_accraw3();

  AccProj* accproj = new AccProj();

  if (!cgltrack3||!cgltrack1) {
    PHMessage("associateACC",PHWarning,"CglTrack table  not found in merged node tree");
    delete accproj;
    return False; 
  }
  if (!phtrack3||!phtrack1) {
    PHMessage("associateACC",PHWarning,"PHTrack table  not found in merged node tree");
    delete accproj;
    return False; 
  }
  if (!accraw3||!accraw1) {
    PHMessage("associateACC",PHWarning,"AccRaw not found in merged node tree");
    delete accproj;
    return False; 
  }
  
  //  OK...Do the analysis...
  int totalMcRecoTracks = mcRecoTrackList->length();
  int idTrackG = -1;
  int idTrackS = -1;
  int idTrackR = -1;
  PHEmbedMcRecoTrack* mctrk = 0;
  for (int i = 0; i< totalMcRecoTracks; i++ ) {
    mctrk = (*mcRecoTrackList)[i];
    idTrackG    = mctrk->get_idGeant(); 
    idTrackS    = mctrk->get_dctrkidS();
    idTrackR    = mctrk->get_dctrkidR();
    if (verbose>10) {
      cout << "idTrackG == " << idTrackG 
	   << " idTrackS == " << idTrackS 
	   << " idTrackR == " << idTrackR <<endl;
    }
    if ( idTrackS>=0 ) {
      float pc2x = phtrack1->get_projectionPc2(idTrackS,0);
      float pc2y = phtrack1->get_projectionPc2(idTrackS,1);
      float pc2z = phtrack1->get_projectionPc2(idTrackS,2);
      float pc3x = phtrack1->get_projectionPc3(idTrackS,0);
      float pc3y = phtrack1->get_projectionPc3(idTrackS,1);
      float pc3z = phtrack1->get_projectionPc3(idTrackS,2);
      if (verbose>10) {
	cout<<"pc2x      pc2y      pc2z"<<endl;
	cout<<pc2x<<"  "<<pc2y<<"  "<<pc2z<<endl;
	cout<<"pc3x      pc3y      pc3z"<<endl;
	cout<<pc3x<<"  "<<pc3y<<"  "<<pc3z<<endl;
      }

      //  OK, now that we know the PC projections...time to get ACC:
      int  boxid[NBOXES];
      PHBoolean     struck = False;
      for (unsigned int i=0; i<NBOXES; i++)
	{
	  boxid[i]  = accproj->getBoxIDfromXYZ( pc2x,  pc2y,  pc2z,  pc3x,  pc3y,  pc3z, i);
	  if (-1< boxid[i] &&  boxid[i]<ACC::ACC_NBOX)     struck = True;
	  if (verbose>10) {cout<<"boxid = "<<boxid[0]<<"  "<<boxid[1]<<"  "<<boxid[2]<<"  "<<boxid[3]<<endl; }
	}
      int hitid      = accproj->getHitIDfromXYZ(pc2x, pc2y, pc2z, pc3x, pc3y, pc3z);
      int hitconfig  = accproj->getHitConfig(boxid[0], boxid[1], boxid[2], boxid[3], hitid);
      if (verbose>10) {cout<<"hitid = "<<hitid<<"    hitconfig = "<<hitconfig<<endl; }
      if ( struck )
	{
	  // HitID
	  if (-1<hitid && hitid<ACC::ACC_NBOX)
	    {
	      mctrk->set_aerboxidS(hitid);
	      mctrk->set_aerph1S(accraw1->get_adc(hitid, 0));
	      mctrk->set_aerph2S(accraw1->get_adc(hitid, 1));
	      if (accraw1->get_adcpost(hitid, 0)==1023) mctrk->set_aerph1S( +9999);
	      if (accraw1->get_adcpost(hitid, 1)==1023) mctrk->set_aerph2S( +9999);
	    }
	  else
	    {
	      mctrk->set_aerph1S( -9999);
	      mctrk->set_aerph2S( -9999);
	    }
	}
    }

    if ( idTrackR>=0 ) {
      float pc2x = phtrack3->get_projectionPc2(idTrackR,0);
      float pc2y = phtrack3->get_projectionPc2(idTrackR,1);
      float pc2z = phtrack3->get_projectionPc2(idTrackR,2);
      float pc3x = phtrack3->get_projectionPc3(idTrackR,0);
      float pc3y = phtrack3->get_projectionPc3(idTrackR,1);
      float pc3z = phtrack3->get_projectionPc3(idTrackR,2);

      //  OK, now that we know the PC projections...time to get ACC:
      int  boxid[NBOXES];
      PHBoolean     struck = False;
      for (unsigned int i=0; i<NBOXES; i++)
	{
	  boxid[i]  = accproj->getBoxIDfromXYZ( pc2x,  pc2y,  pc2z,  pc3x,  pc3y,  pc3z, i);
	  if (-1< boxid[i] &&  boxid[i]<ACC::ACC_NBOX)     struck = True;
	}
      int hitid      = accproj->getHitIDfromXYZ(pc2x, pc2y, pc2z, pc3x, pc3y, pc3z);

      if ( struck )
	{
	  if (-1<hitid && hitid<ACC::ACC_NBOX)
	    {
	      mctrk->set_aerboxidR(hitid);
	      mctrk->set_aerph1R(accraw3->get_adc(hitid, 0));
	      mctrk->set_aerph2R(accraw3->get_adc(hitid, 1));
	      if (accraw3->get_adcpost(hitid, 0)==1023) mctrk->set_aerph1R( +9999);
	      if (accraw3->get_adcpost(hitid, 1)==1023) mctrk->set_aerph2R( +9999);
	    }
	  else
	    {
	      mctrk->set_aerph1R(-9999);
	      mctrk->set_aerph2R(-9999);
	    }
	}
    }
    if (verbose>30) {
      cout << "=========  Results of associateACC (ancestry)  ==========" << endl;
      cout << "aerboxid: " << mctrk->get_aerboxidS() << endl;
      cout << "aerph1: " << mctrk->get_aerph1S() <<endl;
      cout << "aerph2: " << mctrk->get_aerph2S() <<endl;
      cout << "=========  Results of associateACC (merged)    ===========" << endl;
      cout << "aerboxid: " << mctrk->get_aerboxidR() << endl;
      cout << "aerph1: " << mctrk->get_aerph1R() <<endl;
      cout << "aerph2: " << mctrk->get_aerph2R() <<endl;
    }
    delete accproj;
    return True;
  }
  delete accproj;
  return True;
}

PHBoolean PHEmbedMCEvaluator::associateMatching() {
  if (verbose>1) 
    cout << "PHEmbedMCEvaluator::associateMatching()" << endl;
  
  float p,x,y,z,ppx,ppy,ppz;
  float deltaZ, deltaP,ph,php,alpha;
  float nsigmaZ,nsigmaP,nSigmaRadius,beta;

  int i,idTrackS,idTrackR;
  int totalDchMcRecoTracks = mcRecoTrackList->length();

  PHEmbedMcRecoTrack* mctrk = 0;

  // Loop thru tracks that have been saved by fill functions
  for (i = 0; i < totalDchMcRecoTracks; i++) { 
    mctrk = (*mcRecoTrackList)[i];
    idTrackS = mctrk->get_dctrkidS(); // same as dchtrackid in single particle node
    idTrackR = mctrk->get_dctrkidR(); // same as dchtrackid in merged node
    
    if (idTrackS>-1) { // calculate matching for S tracks
      alpha = mctrk->get_alphaS();
      p = mctrk->get_momS();
      if (alpha>0) p = -p;
      //PC2
      x    = mctrk->get_pc2xS();
      y    = mctrk->get_pc2yS();
      z    = mctrk->get_pc2zS();
      ppx  = mctrk->get_ppc2xS();
      ppy  = mctrk->get_ppc2yS();
      ppz  = mctrk->get_ppc2zS();
      if (x > -1000 && ppx > -1000) {
	deltaZ = z-ppz;
	ph  = atan2(y,x);
	php = atan2(ppy,ppx);
	deltaP = ph-php;
	if (deltaP > M_PI) {
	  deltaP = 2*M_PI - deltaP;
	}else if (deltaP <-M_PI) {
	  deltaP = deltaP +   2* M_PI;
	}
	nsigmaZ = mat->d_PC2_z_match(p,deltaZ);
	nsigmaP = mat->d_PC2_phi_match(p,deltaP);
	nSigmaRadius = sqrt(nsigmaP*nsigmaP + nsigmaZ*nsigmaZ);
	mctrk->set_pc2sS(nSigmaRadius);
	mctrk->set_pc2sdphiS(nsigmaP);
	mctrk->set_pc2sdzS(nsigmaZ);
	mctrk->set_pc2dphiS(deltaP);
	mctrk->set_pc2dzS(deltaZ);
      }      
      //PC3
      x    = mctrk->get_pc3xS();
      y    = mctrk->get_pc3yS();
      z    = mctrk->get_pc3zS();
      ppx  = mctrk->get_ppc3xS();
      ppy  = mctrk->get_ppc3yS();
      ppz  = mctrk->get_ppc3zS();
      if (x > -1000 && ppx > -1000) {
	deltaZ = z-ppz;
	ph  = atan2(y,x);
	php = atan2(ppy,ppx);
	deltaP = ph-php;
	if (deltaP > M_PI) {
	  deltaP = 2*M_PI - deltaP; 
	}else if (deltaP <-M_PI) {
	  deltaP = deltaP +   2*M_PI;
	}
	if (mctrk->get_phiS()<M_PI_2) {
	  nsigmaZ = mat->d_PC3w_z_match(p,deltaZ);
	  nsigmaP = mat->d_PC3w_phi_match(p,deltaP);
	}else{
	  nsigmaZ = mat->d_PC3e_z_match(p,deltaZ);
	  nsigmaP = mat->d_PC3e_phi_match(p,deltaP);
	}
	nSigmaRadius = sqrt(nsigmaP*nsigmaP + nsigmaZ*nsigmaZ);
	mctrk->set_pc3sS(nSigmaRadius);
	mctrk->set_pc3sdphiS(nsigmaP);
	mctrk->set_pc3sdzS(nsigmaZ);
	mctrk->set_pc3dphiS(deltaP);
	mctrk->set_pc3dzS(deltaZ);
      }      

      //TOF
      x    = mctrk->get_tofxS();
      y    = mctrk->get_tofyS();
      z    = mctrk->get_tofzS();
      ppx  = mctrk->get_ptofxS();
      ppy  = mctrk->get_ptofyS();
      ppz  = mctrk->get_ptofzS();
      if (x >- 1000 && ppx > -1000) {
	deltaZ   = z-ppz;
	ph  = atan2(y,x);
	php = atan2(ppy,ppx);
	deltaP = ph-php;
	if (deltaP > M_PI) {
	  deltaP = 2*M_PI - deltaP; // 
	}else if (deltaP <-M_PI) {
	  deltaP = deltaP +   2*M_PI;
	}
	nsigmaZ = mat->d_TOF_z_match(p,deltaZ);
	nsigmaP = mat->d_TOF_phi_match(p,deltaP);
	nSigmaRadius = sqrt(nsigmaP*nsigmaP + nsigmaZ*nsigmaZ);
	mctrk->set_tofsS(nSigmaRadius);
	mctrk->set_tofsdphiS(nsigmaP);
	mctrk->set_tofsdzS(nsigmaZ);
	mctrk->set_tofdphiS(deltaP);
	mctrk->set_tofdzS(deltaZ);
      }

      //TOFW - Added by Ron
      x    = mctrk->get_tofwxS();
      y    = mctrk->get_tofwyS();
      z    = mctrk->get_tofwzS();
      ppx  = mctrk->get_ptofwxS();
      ppy  = mctrk->get_ptofwyS();
      ppz  = mctrk->get_ptofwzS();
      if(x>-1000&&ppx>-1000)
	{
	  deltaZ   = z-ppz;
	  ph  = atan2(y,x);
	  php = atan2(ppy,ppx);
	  deltaP = ph-php;
	  if(deltaP > M_PI)
	    {
	      deltaP = 2*M_PI - deltaP; // 
	    }
	  else if(deltaP <-M_PI)
	    {
	      deltaP = deltaP +   2*M_PI;
	    }
	  //nsigmaZ = mat->d_TOFW_z_match(p,deltaZ); // - No utiMatch class function for TOFW
	  //nsigmaP = mat->d_TOFW_phi_match(p,deltaP);
	  //nSigmaRadius = sqrt(nsigmaP*nsigmaP + nsigmaZ*nsigmaZ);
	  //mctrk->set_tofwsS(nSigmaRadius);
	  //mctrk->set_tofwsdphiS(nsigmaP);
	  //mctrk->set_tofwsdzS(nsigmaZ);
	  mctrk->set_tofwdphiS(deltaP);
	  mctrk->set_tofwdzS(deltaZ);
	}


      //EMC
      x    = mctrk->get_emcxS();
      y    = mctrk->get_emcyS();
      z    = mctrk->get_emczS();
      ppx  = mctrk->get_pemcxS();
      ppy  = mctrk->get_pemcyS();
      ppz  = mctrk->get_pemczS();
      if (x>-1000&&ppx>-1000) {
	deltaZ   = z-ppz;
	ph  = atan2(y,x);
	php = atan2(ppy,ppx);
	deltaP = ph-php;
	if (deltaP > M_PI) {
	  deltaP = 2*M_PI - deltaP; // 
	}else if (deltaP <-M_PI) {
	  deltaP = deltaP +   2*M_PI;
	}
	beta = mctrk->get_betaS();
	if (mctrk->get_phiS()<M_PI_2) {//west
	  nsigmaZ = mat->d_PBSCw_z_match(p,beta,deltaZ,3);
	  nsigmaP = mat->d_PBSCw_phi_match(p,deltaP,3);
	}else{
	  if (mctrk->get_phiS()>M_PI) {//west
	    nsigmaZ = mat->d_PBGL_z_match(p,beta,deltaZ,3);
	    nsigmaP = mat->d_PBGL_phi_match(p,deltaP,3);
	  }else{
	    nsigmaZ = mat->d_PBSCe_z_match(p,beta,deltaZ,3);
	    nsigmaP = mat->d_PBSCe_phi_match(p,deltaP,3);	    
	  }
	}
	nSigmaRadius = sqrt(nsigmaP*nsigmaP + nsigmaZ*nsigmaZ);
	mctrk->set_emcsS(nSigmaRadius);
	mctrk->set_emcsdphiS(nsigmaP);
	mctrk->set_emcsdzS(nsigmaZ);
	mctrk->set_emcdphiS(deltaP);
	mctrk->set_emcdzS(deltaZ);
      }
    }

    if (idTrackR>-1) { // calculate matching for R tracks
      alpha    = mctrk->get_alphaR();
      p = mctrk->get_momR();
      if (alpha>0) p = -p;
      //PC2
      x    = mctrk->get_pc2xR();
      y    = mctrk->get_pc2yR();
      z    = mctrk->get_pc2zR();
      ppx  = mctrk->get_ppc2xR();
      ppy  = mctrk->get_ppc2yR();
      ppz  = mctrk->get_ppc2zR();
      if (x>-1000&&ppx>-1000) {
	deltaZ   = z-ppz;
	ph  = atan2(y,x);
	php = atan2(ppy,ppx);
	deltaP = ph-php;
	if (deltaP > M_PI) {
	  deltaP = 2*M_PI - deltaP; // 
	}else if (deltaP <-M_PI) {
	  deltaP = deltaP +   2*M_PI;
	}
	nsigmaZ = mat->d_PC2_z_match(p,deltaZ);
	nsigmaP = mat->d_PC2_phi_match(p,deltaP);
	nSigmaRadius = sqrt(nsigmaP*nsigmaP + nsigmaZ*nsigmaZ);
	mctrk->set_pc2sR(nSigmaRadius);
	mctrk->set_pc2sdphiR(nsigmaP);
	mctrk->set_pc2sdzR(nsigmaZ);
	mctrk->set_pc2dphiR(deltaP);
	mctrk->set_pc2dzR(deltaZ);
      }
      //PC3
      x    = mctrk->get_pc3xR();
      y    = mctrk->get_pc3yR();
      z    = mctrk->get_pc3zR();
      ppx  = mctrk->get_ppc3xR();
      ppy  = mctrk->get_ppc3yR();
      ppz  = mctrk->get_ppc3zR();
      if (x>-1000&&ppx>-1000) {
	deltaZ   = z-ppz;
	ph  = atan2(y,x);
	php = atan2(ppy,ppx);
	deltaP = ph-php;
	if (deltaP > M_PI) {
	  deltaP = 2*M_PI - deltaP; // 
	}else if (deltaP <-M_PI) {
	  deltaP = deltaP +   2*M_PI;
	}
	if (mctrk->get_phiR()<M_PI_2) {
	  nsigmaZ = mat->d_PC3w_z_match(p,deltaZ);
	  nsigmaP = mat->d_PC3w_phi_match(p,deltaP);
	}else{
	  nsigmaZ = mat->d_PC3e_z_match(p,deltaZ);
	  nsigmaP = mat->d_PC3e_phi_match(p,deltaP);
	}
	nSigmaRadius = sqrt(nsigmaP*nsigmaP + nsigmaZ*nsigmaZ);
	mctrk->set_pc3sR(nSigmaRadius);
	mctrk->set_pc3sdphiR(nsigmaP);
	mctrk->set_pc3sdzR(nsigmaZ);
	mctrk->set_pc3dphiR(deltaP);
	mctrk->set_pc3dzR(deltaZ);
      }

      //TOF
      x    = mctrk->get_tofxR();
      y    = mctrk->get_tofyR();
      z    = mctrk->get_tofzR();
      ppx  = mctrk->get_ptofxR();
      ppy  = mctrk->get_ptofyR();
      ppz  = mctrk->get_ptofzR();
      if (x>-1000&&ppx>-1000) {
	deltaZ   = z-ppz;
	ph  = atan2(y,x);
	php = atan2(ppy,ppx);
	deltaP = ph-php;
	if (deltaP > M_PI) {
	  deltaP = 2*M_PI - deltaP; // 
	}else if (deltaP <-M_PI) {
	  deltaP = deltaP +   2*M_PI;
	}
	nsigmaZ = mat->d_TOF_z_match(p,deltaZ);
	nsigmaP = mat->d_TOF_phi_match(p,deltaP);
	nSigmaRadius = sqrt(nsigmaP*nsigmaP + nsigmaZ*nsigmaZ);
	mctrk->set_tofsR(nSigmaRadius);
	mctrk->set_tofsdphiR(nsigmaP);
	mctrk->set_tofsdzR(nsigmaZ);
	mctrk->set_tofdphiR(deltaP);
	mctrk->set_tofdzR(deltaZ);
      }


      //TOFW - Added by Ron
      x    = mctrk->get_tofwxR();
      y    = mctrk->get_tofwyR();
      z    = mctrk->get_tofwzR();
      ppx  = mctrk->get_ptofwxR();
      ppy  = mctrk->get_ptofwyR();
      ppz  = mctrk->get_ptofwzR();
      if(x>-1000&&ppx>-1000)
	{
	  deltaZ   = z-ppz;
	  ph  = atan2(y,x);
	  php = atan2(ppy,ppx);
	  deltaP = ph-php;
	  if(deltaP > M_PI)
	    {
	      deltaP = 2*M_PI - deltaP; // 
	    }
	  else if(deltaP <-M_PI)
	    {
	      deltaP = deltaP +   2*M_PI;
	    }
	  //nsigmaZ = mat->d_TOFW_z_match(p,deltaZ); // No utiMatch class function for TOFW
	  //nsigmaP = mat->d_TOFW_phi_match(p,deltaP);
	  //nSigmaRadius = sqrt(nsigmaP*nsigmaP + nsigmaZ*nsigmaZ);
	  //mctrk->set_tofwsR(nSigmaRadius);
	  //mctrk->set_tofwsdphiR(nsigmaP);
	  //mctrk->set_tofwsdzR(nsigmaZ);
	  mctrk->set_tofwdphiR(deltaP);
	  mctrk->set_tofwdzR(deltaZ);
	}


      //EMC
      x    = mctrk->get_emcxR();
      y    = mctrk->get_emcyR();
      z    = mctrk->get_emczR();
      ppx  = mctrk->get_pemcxR();
      ppy  = mctrk->get_pemcyR();
      ppz  = mctrk->get_pemczR();
      if (x>-1000&&ppx>-1000) {
	deltaZ   = z-ppz;
	ph  = atan2(y,x);
	php = atan2(ppy,ppx);
	deltaP = ph-php;
	if (deltaP > M_PI) {
	  deltaP = 2*M_PI - deltaP; // 
	}else if (deltaP <-M_PI) {
	  deltaP = deltaP +   2*M_PI;
	}
	beta = mctrk->get_betaR();
	if (mctrk->get_phiR()<M_PI_2) {//west
	  nsigmaZ = mat->d_PBSCw_z_match(p,beta,deltaZ,3);
	  nsigmaP = mat->d_PBSCw_phi_match(p,deltaP,3);
	}else{
	  if (mctrk->get_phiR()>M_PI) {//west
	    nsigmaZ = mat->d_PBGL_z_match(p,beta,deltaZ,3);
	    nsigmaP = mat->d_PBGL_phi_match(p,deltaP,3);
	  }else{
	    nsigmaZ = mat->d_PBSCe_z_match(p,beta,deltaZ,3);
	    nsigmaP = mat->d_PBSCe_phi_match(p,deltaP,3);	    
	  }
	}
	nSigmaRadius = sqrt(nsigmaP*nsigmaP + nsigmaZ*nsigmaZ);
	mctrk->set_emcsR(nSigmaRadius);
	mctrk->set_emcsdphiR(nsigmaP);
	mctrk->set_emcsdzR(nsigmaZ);
	mctrk->set_emcdphiR(deltaP);
	mctrk->set_emcdzR(deltaZ);
      }
    } // R tracks

  } // end loop over DchMcRecoTracks

  return True;
}

PHBoolean PHEmbedMCEvaluator::fillPHEmbedMcRecoTrack() {
  if (verbose > 10)
    cout << "PHEmbedMCEvaluator::fillPHEmbedMcRecoTrack(): " << std::flush;
 
  PHEmbedMcRecoTrack* track;
  PHEmbedHistogrammer *histo = PHEmbedHistogrammer::instance();
  int totalMcRecoTracks = mcRecoTrackList->length();
  if (verbose > 10)
    cout << totalMcRecoTracks << " tracks to write." << endl;

  for (int i=0; i<totalMcRecoTracks; i++ ) {
    track = (*mcRecoTrackList)[i];
    histo->fillPHEmbedMcRecoTrack(track);
  }

  return True;
}
