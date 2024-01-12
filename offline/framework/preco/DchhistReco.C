#include <iostream>

// Drift Chamber Objects...
#include "DchTrackCandidate.hh"

// General PHENIX objects...
#include "PHPointerList.h"
#include "PHGeometry.h"
#include "PHCompositeNode.h"
#include "PHDataNode.h"
#include "recoConsts.h"
#include "VtxOut.h"
#include "DchhistReco.h"

#include "getClass.h"

//  Root objects...
#include "TFile.h"
#include "TNtuple.h"

using namespace std;
using namespace PHGeometry;

DchhistReco::DchhistReco(const char *name) : SubsysReco(name)
{
  Datafile = 0;

  //  Use recoConst to control logicals...default to NO.
  recoConsts *rc = recoConsts::instance();
  doStereoWires = (rc->get_IntFlag("DCHHIST_STEREOWIRES",0)==1);
  doStereoZed   = (rc->get_IntFlag("DCHHIST_STEREOZED"  ,0)==1);
  doUValignment = (rc->get_IntFlag("DCHHIST_UVALIGNMENT",0)==1);

}


int DchhistReco::Init(PHCompositeNode *topNode)
{
  //  Here open the output file...
  int compression=3;
  Datafile = new TFile("DchHistograms.root","RECREATE","DchHistograms.root",compression);

  //  Here we initialize the necessary histograms...
  if (doStereoWires) 
    {
      stereoWires = new TNtuple("stereoWires","stereoWires",
				"ncX:ncUV:nX:nUV:nx1:nx2:nuv1:nuv2:npc1:alpha:phi:beta:zed:u11ID:u11IDm:u11Cell:u11Plane:u11Arm:u11Side:u11Dist:u11Width:u11Time1:u11R:u11PHI:u12ID:u12IDm:u12Cell:u12Plane:u12Arm:u12Side:u12Dist:u12Width:u12Time1:u12R:u12PHI:u13ID:u13IDm:u13Cell:u13Plane:u13Arm:u13Side:u13Dist:u13Width:u13Time1:u13R:u13PHI:u14ID:u14IDm:u14Cell:u14Plane:u14Arm:u14Side:u14Dist:u14Width:u14Time1:u14R:u14PHI:v11ID:v11IDm:v11Cell:v11Plane:v11Arm:v11Side:v11Dist:v11Width:v11Time1:v11R:v11PHI:v12ID:v12IDm:v12Cell:v12Plane:v12Arm:v12Side:v12Dist:v12Width:v12Time1:v12R:v12PHI:v13ID:v13IDm:v13Cell:v13Plane:v13Arm:v13Side:v13Dist:v13Width:v13Time1:v13R:v13PHI:v14ID:v14IDm:v14Cell:v14Plane:v14Arm:v14Side:v14Dist:v14Width:v14Time1:v14R:v14PHI:u21ID:u21IDm:u21Cell:u21Plane:u21Arm:u21Side:u21Dist:u21Width:u21Time1:u21R:u21PHI:u22ID:u22IDm:u22Cell:u22Plane:u22Arm:u22Side:u22Dist:u22Width:u22Time1:u22R:u22PHI:u23ID:u23IDm:u23Cell:u23Plane:u23Arm:u23Side:u23Dist:u23Width:u23Time1:u23R:u23PHI:u24ID:u24IDm:u24Cell:u24Plane:u24Arm:u24Side:u24Dist:u24Width:u24Time1:u24R:u24PHI:v21ID:v21IDm:v21Cell:v21Plane:v21Arm:v21Side:v21Dist:v21Width:v21Time1:v21R:v21PHI:v22ID:v22IDm:v22Cell:v22Plane:v22Arm:v22Side:v22Dist:v22Width:v22Time1:v22R:v22PHI:v23ID:v23IDm:v23Cell:v23Plane:v23Arm:v23Side:v23Dist:v23Width:v23Time1:v23R:v23PHI:v24ID:v24IDm:v24Cell:v24Plane:v24Arm:v24Side:v24Dist:v24Width:v24Time1:v24R:v24PHI");
    }

  if (doStereoZed)
    {
      stereoZed = new TNtuple("stereoZed","stereoZed",
			      "arm:side:alpha:phi:beta:zed:vix:viy:vz:pix:piy:pz:u11IDm:u11Dist:u11Width:u11Time1:u11ix:u11iy:u11dx:u12IDm:u12Dist:u12Width:u12Time1:u12ix:u12iy:u12dx:u13IDm:u13Dist:u13Width:u13Time1:u13ix:u13iy:u13dx:u14IDm:u14Dist:u14Width:u14Time1:u14ix:u14iy:u14dx:v11IDm:v11Dist:v11Width:v11Time1:v11ix:v11iy:v11dx:v12IDm:v12Dist:v12Width:v12Time1:v12ix:v12iy:v12dx:v13IDm:v13Dist:v13Width:v13Time1:v13ix:v13iy:v13dx:v14IDm:v14Dist:v14Width:v14Time1:v14ix:v14iy:v14dx:u21IDm:u21Dist:u21Width:u21Time1:u21ix:u21iy:u21dx:u22IDm:u22Dist:u22Width:u22Time1:u22ix:u22iy:u22dx:u23IDm:u23Dist:u23Width:u23Time1:u23ix:u23iy:u23dx:u24IDm:u24Dist:u24Width:u24Time1:u24ix:u24iy:u24dx:v21IDm:v21Dist:v21Width:v21Time1:v21ix:v21iy:v21dx:v22IDm:v22Dist:v22Width:v22Time1:v22ix:v22iy:v22dx:v23IDm:v23Dist:v23Width:v23Time1:v23ix:v23iy:v23dx:v24IDm:v24Dist:v24Width:v24Time1:v24ix:v24iy:v24dx");
    }

  if (doUValignment)
    {
      UValignment = new TNtuple("UValignment","UValignment",
				"arm:side:alpha:phi:beta:zed:zvtx:rpc1:ppc1:zpc1:u11cell:u11IDm:u11Dist:u11Width:u11Time1:u11phi1:u11phi2:u11FitPhi:u11FitZ:u12cell:u12IDm:u12Dist:u12Width:u12Time1:u12phi1:u12phi2:u12FitPhi:u12FitZ:u13cell:u13IDm:u13Dist:u13Width:u13Time1:u13phi1:u13phi2:u13FitPhi:u13FitZ:u14cell:u14IDm:u14Dist:u14Width:u14Time1:u14phi1:u14phi2:u14FitPhi:u14FitZ:v11cell:v11IDm:v11Dist:v11Width:v11Time1:v11phi1:v11phi2:v11FitPhi:v11FitZ:v12cell:v12IDm:v12Dist:v12Width:v12Time1:v12phi1:v12phi2:v12FitPhi:v12FitZ:v13cell:v13IDm:v13Dist:v13Width:v13Time1:v13phi1:v13phi2:v13FitPhi:v13FitZ:v14cell:v14IDm:v14Dist:v14Width:v14Time1:v14phi1:v14phi2:v14FitPhi:v14FitZ:u21cell:u21IDm:u21Dist:u21Width:u21Time1:u21phi1:u21phi2:u21FitPhi:u21FitZ:u22cell:u22IDm:u22Dist:u22Width:u22Time1:u22phi1:u22phi2:u22FitPhi:u22FitZ:u23cell:u23IDm:u23Dist:u23Width:u23Time1:u23phi1:u23phi2:u23FitPhi:u23FitZ:u24cell:u24IDm:u24Dist:u24Width:u24Time1:u24phi1:u24phi2:u24FitPhi:u24FitZ:v21cell:v21IDm:v21Dist:v21Width:v21Time1:v21phi1:v21phi2:v21FitPhi:v21FitZ:v22cell:v22IDm:v22Dist:v22Width:v22Time1:v22phi1:v22phi2:v22FitPhi:v22FitZ:v23cell:v23IDm:v23Dist:v23Width:v23Time1:v23phi1:v23phi2:v23FitPhi:v23FitZ:v24cell:v24IDm:v24Dist:v24Width:v24Time1:v24phi1:v24phi2:v24FitPhi:v24FitZ");
    }

  return 0;
}

int DchhistReco::End(PHCompositeNode* topnode)
{
  cout << "OK...Now I think that I am writing...." << endl;

  Datafile->Write();
  return 0;
}

int DchhistReco::process_event(PHCompositeNode *topNode)
{

  if (doStereoWires) fillStereoWires(topNode);
  if (doStereoZed  ) fillStereoZed  (topNode);
  if (doUValignment) fillUValignment(topNode);

  return 0;
}

void DchhistReco::fillStereoWires(PHCompositeNode *topNode)
{
  float array[189];
  PHNodeIterator topIter(topNode);
  PHDataNode<PHPointerList<DchTrackCandidate> >* CandNode = 0;
  CandNode = (PHDataNode<PHPointerList<DchTrackCandidate> >*)topIter.findFirst("PHDataNode", "DchTrackCandidate");
  PHPointerList<DchTrackCandidate> *candidates = CandNode->getData();
  if (!candidates) return;

  int numberOfCandidates = candidates->length();
  DchTrackCandidate* candi;
  for (int c=0; c<numberOfCandidates; c++) {
    candi = (*candidates)[c];

    float alphac = candi->getLocalAlpha();
    float betac  = candi->getLocalBeta();
    float zedc   = candi->getLocalZed();
    float phic   = candi->getLocalPhi();

    int ncX      = candi->closestX->length();
    int ncUV     = candi->closestUV->length();
    int nX       = candi->X->length();
    int nUV      = candi->UV->length();
    int nx1      = candi->x1->length();
    int nx2      = candi->x2->length();
    int nuv1     = candi->uv1->length();
    int nuv2     = candi->uv2->length();
    int npc1     = candi->pc1->length();
    int m=0;
    array[m++] = ncX;
    array[m++] = ncUV;
    array[m++] = nX;
    array[m++] = nUV;
    array[m++] = nx1;
    array[m++] = nx2;
    array[m++] = nuv1;
    array[m++] = nuv2;
    array[m++] = npc1;
    array[m++] = alphac;
    array[m++] = phic;
    array[m++] = betac;
    array[m++] = zedc;
    
    //"Golden" tracks are those for which a number of conditions are true:
    // There are exactly 12 chosen X hits.
    // There are exactly 4 closest uv1 hits.
    // If and only if I find that the given track is golden
    // I will make a record in the NTUPLE.
    // TKH -- 10-23-2001
    if (nX>10&&npc1==1) {
      DchHitLine * uv[16]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
      DchHitLine * line;
      for (int j=0; j<ncUV; j++) {
	line = (*(candi->closestUV))[j];
	int plane = line->getPlane();
	if ((plane>11)&&(plane<20)) {  //Now doing UV1!!!
	  uv[plane-12] = (*(candi->closestUV))[j];
	}
	if ((plane>31)&&(plane<40)) {  //Now doing UV2!!!
	  uv[plane-24] = (*(candi->closestUV))[j];
	}
      }
      for (int j=0; j<16; j++) {
	if (uv[j]) {
	  array[m++] = uv[j]->getId();
	  array[m++] = uv[j]->getIdmirror();
	  array[m++] = uv[j]->getCell();
	  array[m++] = uv[j]->getPlane();
	  array[m++] = uv[j]->getArm();
	  array[m++] = uv[j]->getSide();
	  array[m++] = uv[j]->getDistance();
	  array[m++] = uv[j]->getWidth();
	  array[m++] = uv[j]->getTime();
	  array[m++] = uv[j]->getLocalRadius();
	  array[m++] = uv[j]->getLocalPhi();
	}
	else {
	  array[m++] = -100;
	  array[m++] = -100;
	  array[m++] = -100;
	  array[m++] = -100;
	  array[m++] = -100;
	  array[m++] = -100;
	  array[m++] = -100;
	  array[m++] = -100;
	  array[m++] = -100;
	  array[m++] = -100;
	  array[m++] = -100;
	}
      }
      stereoWires->Fill(array);
    }
  }
  return;
}

void DchhistReco::fillStereoZed(PHCompositeNode *topNode)
{
  float array[124];
  float u11r=214.85; //This is the ideal radius of the u11 wire.
  
  //  Get the necessary nodes...
  PHNodeIterator topIter(topNode);
  PHDataNode<PHPointerList<DchTrackCandidate> >* CandNode = 0;
  CandNode = (PHDataNode<PHPointerList<DchTrackCandidate> >*)topIter.findFirst("PHDataNode", "DchTrackCandidate");
  PHPointerList<DchTrackCandidate> *candidates = CandNode->getData();
  VtxOut *d_vtx = findNode::getClass<VtxOut>(topNode,"VtxOut");
  if (!candidates || !d_vtx) return;

  //  Initialize vertex information...
  PHPoint vertex(0,0,d_vtx->get_ZVertex());
  PHPoint globalVertexIntersection(0,0,0),vertexIntersection(0,0,0); 

  //  Loop over the candidates and fill the Ntuple...
  int numberOfCandidates = candidates->length();
  DchTrackCandidate* candi;
  for (int c=0; c<numberOfCandidates; c++) {
    candi = (*candidates)[c];

    int iarm    = candi->getArm();
    int iside   = candi->getSide();
    float alpha = candi->getLocalAlpha();
    float beta  = candi->getLocalBeta();
    float zed   = candi->getLocalZed();
    float phi   = candi->getLocalPhi();

    int m=0;
    array[m++] = iarm;
    array[m++] = iside;
    array[m++] = alpha;
    array[m++] = phi;
    array[m++] = beta;
    array[m++] = zed;
    
    PHPlane planeUV = candi->getGlobalPlane(UV1Wire);
    PHLine tmp(vertex,planeUV.getNormal());
    intersectionLinePlane(tmp, planeUV, globalVertexIntersection);
    vertexIntersection = candi->rotateAndTranslate(globalVertexIntersection);

    array[m++] = vertexIntersection.getX();
    array[m++] = vertexIntersection.getY();
    array[m++] = vertex.getZ();

    float vz =vertex.getZ();

    //"Golden" tracks are those for which a number of conditions are true:
    // There are exactly 12 chosen X hits.
    // There are exactly 4 closest uv1 hits.
    // If and only if I find that the given track is golden
    // I will make a record in the NTUPLE.
    // TKH -- 10-23-2001
    int nX       = candi->X->length();
    int npc1     = candi->pc1->length();
    if (nX>10&&npc1==1) {

      DchPc1Hit *phit = (*candi->pc1)[0]; 
      PHPoint inter = candi->rotateAndTranslate(*phit);
      array[m++] = inter.getX();
      array[m++] = inter.getY();
      array[m++] = phit->getZ();

      float pix=inter.getX();
      float pz =phit->getZ();
      PHCylPoint PadCyl(*phit);
      float pr =PadCyl.getR();

      DchHitLine * uv[16]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
      DchHitLine * line;
      for (unsigned int j=0; j<candi->closestUV->length(); j++) {
	line = (*(candi->closestUV))[j];
	int plane = line->getPlane();
	if ((plane>11)&&(plane<20)) {  //Now doing UV1!!!
	  uv[plane-12] = (*(candi->closestUV))[j];
	}
	if ((plane>31)&&(plane<40)) {  //Now doing UV2!!!
	  uv[plane-24] = (*(candi->closestUV))[j];
	}
      }
      for (int j=0; j<16; j++) {
	if (uv[j]) {
	  array[m++] = uv[j]->getIdmirror();
	  array[m++] = uv[j]->getDistance();
	  array[m++] = uv[j]->getWidth();
	  array[m++] = uv[j]->getTime();
	  array[m++] = uv[j]->getLocalIntersectionPoint().getX();
	  array[m++] = uv[j]->getLocalIntersectionPoint().getY();
	  float hix =  uv[j]->getLocalIntersectionPoint().getX();
	  float hr  =  uv[j]->getLocalRadius();
	  float prediction = (pix-vz)/pr * hr + vz;   // The GOOD one...
	  // Parameterized error due to residual bend: TKH
	  // There is no a fundamental justification for the form.
	  prediction = prediction +(0.25*pz*alpha*alpha*(pr-hr)/(pr-u11r));
	  array[m++] = hix - prediction;
	}
	else {
	  array[m++] = -100;
	  array[m++] = -100;
	  array[m++] = -100;
	  array[m++] = -100;
	  array[m++] = -100;
	  array[m++] = -100;
	  array[m++] = -100;
	}
      }
      stereoZed->Fill(array); // Force the writing...
    }
  }
  return;
}

void DchhistReco::fillUValignment(PHCompositeNode *topNode)
{
  int nX, npc1;
  float pr, pp, pz;
  unsigned int j;
  int plane;
  int iarm;
  int iside;
  float alpha;
  float beta;
  float zed;
  float phi;
  int m;
  int c;
  double t;
  double R;
  double z;
  double L;
  double f;
  double r;
  float array[154];

  PHNodeIterator topIter(topNode);
  PHDataNode<PHPointerList<DchTrackCandidate> >* CandNode = 0;
  CandNode = (PHDataNode<PHPointerList<DchTrackCandidate> >*)topIter.findFirst("PHDataNode", "DchTrackCandidate");
  PHPointerList<DchTrackCandidate> *candidates = CandNode->getData();

  VtxOut *d_vtx = findNode::getClass<VtxOut>(topNode,"VtxOut");
  if (!candidates || !d_vtx) return;

  //  Initialize vertex information...
  PHPoint vertex(0,0,d_vtx->get_ZVertex());

  int numberOfCandidates = candidates->length();
  DchTrackCandidate* candi;
  for (c = 0; c < numberOfCandidates; c++)
    {
      candi = (*candidates)[c];
      PHLine theTrackLine(candi->getGlobalPoint(), candi->getGlobalVector());

      iarm = candi->getArm();
      iside = candi->getSide();
      alpha = candi->getGlobalAlpha();
      beta = candi->getGlobalBeta();
      zed = candi->getGlobalZed();
      phi = candi->getGlobalPhi();
      
      m = 0;
      array[m++] = iarm;
      array[m++] = iside;
      array[m++] = alpha;
      array[m++] = phi;
      array[m++] = beta;
      array[m++] = zed;
      array[m++] = vertex.getZ();

      // "Golden" tracks are those for which a number of conditions
      // are true: There are more than 9 chosen X hits.  There are
      // exactly 4 closest uv1 hits.  If and only if I find that the
      // given track is golden I will make a record in the NTUPLE.
      // TKH -- 10-28-2002
      nX = candi->X->length();
      npc1 = candi->pc1->length();
      if (nX > 10 && npc1 == 1)
        {
          DchPc1Hit *phit = (*candi->pc1)[0];
          PHCylPoint PadCyl(*phit);

          pr = PadCyl.getR();
          pp = PadCyl.getPhi();
          pz = PadCyl.getZ();
          array[m++] = pr;
          array[m++] = pp;
          array[m++] = pz;

          //  Assign an array of pointers to the hits on every plane.
          //  NULL pointers for planes that have no hits.
          DchHitLine * uv[16] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
          DchHitLine * line;
          for (j = 0; j < candi->closestUV->length(); j++)
            {
              line = (*(candi->closestUV))[j];
	      plane = line->getPlane();
              if (plane > 11 && plane < 20)
                {  //Now doing UV1!!!
                  uv[plane - 12] = (*(candi->closestUV))[j];
                }
              if (plane > 31 && plane < 40)
                {  //Now doing UV2!!!
                  uv[plane - 24] = (*(candi->closestUV))[j];
                }
            }
	  
          // Ummm...OK.  Now we need to loop through these hits to
          // fill the NTUPLE.  There are two "deduced" values: phi1
          // and phi2.  These are the phi coordinates of the hit at
          // its "endpoints".  Since the hit structure does not
          // actually define endoints, we will instead use the
          // intersection points of the hit at planes located at
          // Z=+100 and Z=-100 cm.  TKH 10-28-2002

          PHVector origin(0, 0, 0);
          PHVector origin1(0, 0, -100);
          PHVector origin2(0, 0, 100);
          PHVector zAxis(0, 0, 1);
          PHVector cylinderAxis(0, 0, 150);
          PHPlane plane1(origin1, zAxis);
          PHPlane plane2(origin2, zAxis);
          PHPoint point1, point2, predPoint1, predPoint2;
          PHCylPoint cylPoint1, cylPoint2, cylPredPoint, cylPredPoint1, cylPredPoint2;
          for (j = 0; j < 16; j++)
            {
              if (uv[j])
                {
                  array[m++] = uv[j]->getCell();
                  array[m++] = uv[j]->getIdmirror();
                  array[m++] = uv[j]->getDistance();
                  array[m++] = uv[j]->getWidth();
                  array[m++] = uv[j]->getTime();

                  //Now get the two intersection points of the hits at the reference plane:
                  intersectionLinePlane( *uv[j], plane1, point1);
                  intersectionLinePlane( *uv[j], plane2, point2);
                  cylPoint1 = point1;
                  cylPoint2 = point2;
                  array[m++] = cylPoint1.getPhi();
                  array[m++] = cylPoint2.getPhi();

                  // With a little bit of trigonometry we can
                  // determine the small correction to the radius at
                  // which a UV wire intersects a given z-plane.  It's
                  // only a matter of a few mm at most.
		  t = (cylPoint1.getPhi() - cylPoint2.getPhi()) / 2.0;
		  R = (cylPoint1.getR() + cylPoint2.getR()) / 2.0;
		  z = phit->getZ() - origin1.getZ();
		  L = origin2.getZ() - origin1.getZ();
		  f = z / L;
		  r = R * sqrt(1.0 + f * (f - 1.0) * 4 * sin(t)*sin(t) );
		  
                  // Now get the prediction from the final line.
                  // Hmmm...probably a dumb way to do it, but I'm
                  // gonna make a cylinder with which I will intersect
                  // the track's line.
                  PHCylinder theCylinder(origin, r, cylinderAxis);
                  intersectionLineCylinder(theTrackLine, theCylinder, predPoint1, predPoint2);
		  
		  // There should be two points of intersection; pick
		  // the one which has the lesser phi difference with
		  // the UV wire.  The other intersection point ought
		  // to be about M_PI away.  This is a big enough
		  // difference that it doesn't matter whether we
		  // choose to compare to cylPoint1 or cylPoint2 or
		  // their average.
                  cylPredPoint1 = predPoint1;
                  cylPredPoint2 = predPoint2;
		  if (abs(cylPredPoint1.getPhi() - cylPoint1.getPhi()) <
		      abs(cylPredPoint2.getPhi() - cylPoint1.getPhi()))
		    {
		      cylPredPoint = predPoint1;
		    }
		  else
		    {
		      cylPredPoint = predPoint2;
		    }
		  array[m++] = cylPredPoint.getPhi();
		  array[m++] = cylPredPoint.getZ();
                }
              else
                {
                  array[m++] = -100;
                  array[m++] = -100;
                  array[m++] = -100;
                  array[m++] = -100;
                  array[m++] = -100;
                  array[m++] = -100;
                  array[m++] = -100;
                  array[m++] = -100;
                  array[m++] = -100;
                }
            }
          UValignment->Fill(array);
        }
    }
  return;
}


