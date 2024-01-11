//
//  TKH--Adjusted to new calibration interface.
//         11-25-2001
//
//
#include "mNewDchAnalyzer.hh"

#include "PHDchGeometryObject.h"
#include "PHDchCalibrationObject.h"

#include "PHDchHistogrammer.hh"

#include "PHGeometry.h"

#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHTable.hh"

#include <iostream>
using namespace std;

int LineFit(float x[],float y[],float w[],int n,float *a,
 	       float *b,float *chisq,float *siga,float *sigb);
 
typedef PHIODataNode<PHObject> PHObjectNode_t;
typedef PHIODataNode<PHTable> TableNode_t;
typedef PHIODataNode<DchTrack> DchTrackNode_t;

mNewDchAnalyzer::mNewDchAnalyzer()
{
   pDchAddress   = 0;
   pDchGeometry  = 0;
   pDchNoise     = 0;
   pDchCalibration = 0;
   pDchHistogrammer   = 0;
   trackTableList = 0;
   hitTableList = 0;
   rawTableList = 0;

}

mNewDchAnalyzer::~mNewDchAnalyzer()
{
  trackTableList->clearAndDestroy();
  hitTableList->clearAndDestroy();
  rawTableList->clearAndDestroy();
}

PHBoolean mNewDchAnalyzer::event(PHCompositeNode *topNode) {
 PHPointerList<PHNode> nodes;
 PHNodeIterator i(topNode);

 PHDataNode<PHDchAddressObject>* pDchDaoNode;
 PHDataNode<PHDchGeometryObject>* pDchDgoNode;
 PHDataNode<PHDchCalibrationObject>* pDchDcoNode;
 PHDataNode<PHDchNoiseObject>* pDchDnoNode;
 PHDataNode<PHDchHistogrammer>* pDchHistoNode;
 
 PHDataNode<PHPointerList<DchCandidate> >* tmpCandiNode;
 PHDataNode<PHPointerList<DchHitInfo> >*   tmpHitNode;
 PHDataNode<PHPointerList<DchRawInfo> >*   tmpRawNode;
 PHDataNode<PHPointerList<DchTrackInfo> >* tmpTrackNode;
 PHDataNode<PHPointerList<DchTrackInfo> >* tmpTrackBestNode;

 pTopNode = topNode;
 PHObjectNode_t *phob;
 nodes.clear();
 
// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

 phob = static_cast < PHObjectNode_t * >(i.findFirst ("PHIODataNode", "DchRawTablev1"));
 rawTable = dynamic_cast < DchRawTable * >(phob->getData ());
 if (!rawTable) {
   cout << "DchRawTablev1 not found "<< endl;
   return False;
 }

 phob = static_cast < PHObjectNode_t * >(i.findFirst ("PHIODataNode", "DchHitLineTablev1"));
 hitLineTable = dynamic_cast < DchHitLineTable * >(phob->getData ());
 if (!hitLineTable) {
   cout << "DchHitLineTablev1 not found "<< endl;
   return False;
 }
 
 PHTypedNodeIterator <DchTrack> dchtrackiter(topNode);
 DchTrackNode_t *DchTrackNode = dchtrackiter.find("DchTrack");
 if (DchTrackNode)
   {
     trackTable = DchTrackNode->getData();
   }
 else
   {
     cout << PHWHERE << "DchTrack Node not found" << endl;
     return False;
   }

  tmpCandiNode = (PHDataNode<PHPointerList<DchCandidate> >*)i.findFirst("PHDataNode","DchCandidate");
  candidateList = tmpCandiNode->getData();
  
  tmpRawNode = (PHDataNode<PHPointerList<DchRawInfo> >*)i.findFirst("PHDataNode","DchRawInfo");
  rawTableList = tmpRawNode->getData();

  tmpHitNode = (PHDataNode<PHPointerList<DchHitInfo> >*)i.findFirst("PHDataNode","DchHitInfo");
  hitTableList = tmpHitNode->getData();

  tmpTrackNode = (PHDataNode<PHPointerList<DchTrackInfo> >*)i.findFirst("PHDataNode","DchTrackInfo");
  trackTableList = tmpTrackNode->getData();
  
  tmpTrackBestNode = (PHDataNode<PHPointerList<DchTrackInfo> >*)i.findFirst("PHDataNode","DchTrackBestInfo");
  trackInfoListBest = tmpTrackBestNode->getData();
    
  // Detector objects for the tree
  
  pDchDaoNode = (PHDataNode<PHDchAddressObject>*)i.findFirst("PHDataNode","DchDAO");
  pDchAddress = pDchDaoNode->getData();

  pDchDgoNode = (PHDataNode<PHDchGeometryObject>*)i.findFirst("PHDataNode","DchDGO");
  pDchGeometry  = pDchDgoNode->getData();
   
  pDchDcoNode = (PHDataNode<PHDchCalibrationObject>*)i.findFirst("PHDataNode","DchDCO");
  pDchCalibration = pDchDcoNode->getData();
   
  pDchDnoNode = (PHDataNode<PHDchNoiseObject>*)i.findFirst("PHDataNode","DchDNO");
  pDchNoise = pDchDnoNode->getData();
 
  pDchHistoNode = (PHDataNode<PHDchHistogrammer>*)i.findFirst("PHDataNode","DchHisto");
  pDchHistogrammer = pDchHistoNode->getData();
  
  return callPAM(nodes);

}

PHBoolean mNewDchAnalyzer::callPAM(PHPointerList<PHNode> &nl)
{
  if (trackTableList && hitTableList && rawTableList) {
    cout << "=====>Hit asso. Residual" << endl;

    calculateHitAssociatedResiduals();

    cout<<"=====> Effiiency vs distance" <<endl;
    calculateEfficiencyDistribution();
  }

  cout << "Finish with analyzer "<< endl;
  
  return True;
}

void mNewDchAnalyzer::fillRawTableList()
{
  DchRawInfo* rawInfo;
  if (rawTable) {
    int totalRaws = rawTable->Entries();
    for (int i=0; i < totalRaws; i++) {
      rawInfo = new DchRawInfo();
      copyRawInfo(rawInfo, i);
      rawTableList->append(rawInfo);
    }
  }else {
    cout << "DchRawTablev1 missing from the Tree "<< endl;
  }
}

void mNewDchAnalyzer::fillHitTableList()
{
  int rawID1,rawID2;
  DchRawInfo* rawInfo1;
  DchRawInfo* rawInfo2;
  DchHitInfo* hitInfo;
 
  if (hitLineTable) {
    int totalHits = hitLineTable->Entries();
    for (int i=0; i < totalHits; i++) {
      hitInfo = new DchHitInfo();
      copyHitInfo(hitInfo, i);
      rawID1 = hitInfo->getIdraw1();
      rawID2 = hitInfo->getIdraw2();
      if (rawTableList) {
	rawInfo1 = (*rawTableList)[rawID1];
	hitInfo->setRaw1(rawInfo1);
	rawInfo2 = (*rawTableList)[rawID2];
	hitInfo->setRaw2(rawInfo2);
      }
      hitTableList->append(hitInfo);
    }
  }else {
    cout << "HitLineTable missing from the Tree "<< endl;
  }
}

void mNewDchAnalyzer::fillTrackTableList()
{
  int hitid;
  DchHitInfo* hitInfo;
  DchTrackInfo* trackInfo;
  if (trackTable) {
    int totalTracks = trackTable->get_DchNTrack();
    for (int i=0; i < totalTracks; i++) {
      trackInfo = new DchTrackInfo();
      copyTrackInfo(trackInfo, i);
      if (hitTableList) {
	for (int iplane=0; iplane < numberOfPlanes; iplane++) {
	  hitid   = trackTable->get_hits(i,iplane);
	  if (hitid > -1) {
	    hitInfo = (*hitTableList)[hitid];
	    trackInfo->appendHitInfo(hitInfo);
	  }
	}
      }
      trackTableList->append(trackInfo);
    }
  }else {
    cout << "trackTable missing from the Tree "<< endl;
  }
}
  
void mNewDchAnalyzer::calculateTrackResolution()
{
}

void mNewDchAnalyzer::calculateTimeResolutionOnTracks()
{ 
  DchRawInfo *rawInfo1;
  DchRawInfo *rawInfo2;
  DchRawInfo *rawInfo3;
  
  DchHitInfo *hitInfo1;
  DchHitInfo *hitInfo2;
  DchHitInfo *hitInfo3;

  DchTrackInfo* trackInfo;

  float timeResidual = 0;
  float timeResidualFlag = 0;
  int cell1,plane1,time1;
  int cell2,plane2,time2;
  int cell3,plane3,time3;
  int totalTracks = trackTableList->length();

  for ( int itrk =0; itrk < totalTracks; itrk++) {
    trackInfo = (*trackTableList)[itrk];
    if(!trackInfo) break;
    int totalHits = (trackInfo->getHitInfoList()).length();
    
    for ( int ihit = 0; ihit < totalHits; ihit++) {
      hitInfo1 = trackInfo->getHitInfo(ihit);
      rawInfo1 = hitInfo1->getRaw1();
      cell1    = rawInfo1->getCell();
      plane1   = rawInfo1->getPlane();
      time1    = rawInfo1->getTime();
      
      if ((plane1 >=12 && plane1 <20) || (plane1>=32 && plane1<40)) continue;
      for (int ihit2 = ihit; ihit2 < totalHits; ihit2++) {
	hitInfo2 = trackInfo->getHitInfo(ihit2);
	rawInfo2 = hitInfo2->getRaw1();
	cell2    = rawInfo2->getCell();
	plane2   = rawInfo2->getPlane();
	time2    = rawInfo2->getTime();
	if ((plane2 >=12 && plane2 <20) || (plane2>=32 && plane2<40)) continue;
        if (cell2 == cell1 && plane1 < plane2) {
	  for (int ihit3 = ihit2; ihit3 < totalHits; ihit3++) {
	    hitInfo3 = trackInfo->getHitInfo(ihit3);
	    rawInfo3 = hitInfo3->getRaw1();
	    cell3    = rawInfo3->getCell();
	    plane3   = rawInfo3->getPlane();
	    time3    = rawInfo3->getTime();
	    if ((plane3 >=12 && plane3 <20) || (plane3>=32 && plane3<40)) continue;
 	    if (cell3 == cell1 && plane1 < plane2 && plane1 < plane3 && plane2 < plane3) {
	      timeResidualFlag = 0;
              if ((plane1 <12 && plane2 <12 && plane3<12)){ // X1 planes
		timeResidual = getTimeResidual(plane1,time1,plane2,time2,plane3, time3);
		timeResidualFlag = 1;
	      }else if(plane1>20 && plane2>20 && plane3>20 ){ // X2 planes
		timeResidual = getTimeResidual(plane1,time1,plane2,time2,plane3, time3);
		timeResidualFlag = 1;
	      }
	      if (timeResidualFlag) {
		if(pDchHistogrammer) {
		  pDchHistogrammer->fillTimeResidual(timeResidual, plane1,time1,plane2,time2,plane3,time3);
		}
	      }
	      break;
	    } // same cell and different planes
	  } // third loop
	  break;
	} // same cell and different planes
      } // second hit loop
    } // hit loop
  }// track loop 
}
  
float mNewDchAnalyzer::getTimeResidual(int plane0,int time0,int plane1,int time1,int plane2,int time2)
{
  float diff;

  diff = ((float)plane1-(float)plane0)/((float)plane2-(float)plane0)*(time2-time0)+time0-time1;

  return diff;
}

void mNewDchAnalyzer::calculateHitAssociatedResidualsOn2DPlane()
{
  cout << "To be debugged "<< endl;
  // to debug 
  int plane;

  int totalTracks = trackTableList->length();

  DchHitInfo* hitInfo;
  DchTrackInfo* trackInfo;
  DchCandidate* candidate;
  
  for(int i=0; i<totalTracks; i++) {
    trackInfo = (*trackTableList)[i];
    candidate = trackInfo->getCandidate();
    PHLine  trackLine = getTrackLine(i);  // trackLine in 3D
    PHLine  trackLine2D;                  // track projected in the track plane
    PHPlane trackPlane;                   // track plane for a particular plane
    PHLine  hitline;
    PHPoint inter;
    if (candidate) {
      int totalHits = (trackInfo->getHitInfoList()).length(); 
      for(int ihit = 0; ihit < totalHits; ihit++)  {   // Loop over the hits associated with the track 
	hitInfo = trackInfo->getHitInfo(ihit);
	plane = hitInfo->getPlane();
	trackPlane = candidate->getPlane(wireType[plane]);
        trackLine2D = PHGeometry::projectLineIntoPlane(trackLine,trackPlane);
	cout << "trackLine @D (plane)" <<  plane << " " << trackLine2D << endl;
	hitline = hitInfo->getLine();
	PHBoolean flaggy = PHGeometry::intersectionLinePlane(hitline,trackPlane,inter);
	if (flaggy) {
           double distance = PHGeometry::distanceLinePoint(trackLine2D,inter);
	   cout << "distance: " << distance << endl;
	   // float hitDistance = hitInfo->getDistance();
	} // found intersection
      } // hit loop
    } // candidate   
  } // end of track loop
}

void mNewDchAnalyzer::calculateHitAssociatedResiduals()
{
  // This function calculates the residuals for the hits which have been associated with  a track
  // as in the tables !!!
  
  int arm, plane;
  int supposedArm, supposedCell, supposedPlane;

  int totalTracks = trackTableList->length();

  PHPoint punchPoint;
  DchHitInfo* hitInfo;
  DchTrackInfo* trackInfo;
  
  for(int i=0; i<totalTracks; i++) {
    trackInfo = (*trackTableList)[i];
    PHLine  trackLine = getTrackLine(i);  
    
    int totalHits = (trackInfo->getHitInfoList()).length(); 
    arm = trackInfo->getArm();
    
    for(int ihit = 0; ihit < totalHits; ihit++)  {   // Loop over the hits associated with the track 
      hitInfo = trackInfo->getHitInfo(ihit);
      plane = hitInfo->getPlane();
    
      PHBoolean found = pDchGeometry->getIntersectionTrackWirePlane(trackLine,plane,arm,punchPoint); // check
      if (! found) {
	hitInfo->setOut(1);
      }else {  //if punch point found !
	hitInfo->setOut(0);
	
	supposedArm   = pDchGeometry->findCorrespondingArm(punchPoint);
	supposedCell  = pDchGeometry->findCorrespondingCell(punchPoint);
	supposedPlane = plane;
	
	PHLine supposedWire = pDchGeometry->getWireAsLine(supposedArm,supposedPlane,supposedCell);
	PHPlane driftPlane  = getDriftPlane(punchPoint,supposedArm,supposedPlane,supposedCell,supposedWire);

	//Get the intersection point between the track and the drift plane and the drift distance 
	//from this point to the wire in the drift plane.
	PHPoint trackIntersectionPoint;
	float trackToWireDistance;

	if(PHGeometry::intersectionLinePlane(trackLine,driftPlane,trackIntersectionPoint)) {
	  trackToWireDistance = PHGeometry::distanceLinePoint(supposedWire,trackIntersectionPoint);
	}else{
	  hitInfo->setResidual(-10000);
	  continue;
	}
	float hitDistance = hitInfo->getDistance();
	float residual = hitDistance - trackToWireDistance;
	//short blind;
        
	if(pDchGeometry->expectSignalFrom(supposedArm,supposedPlane,supposedCell,trackIntersectionPoint) ||
	   trackToWireDistance <pDchGeometry->getDistanceToDriftRegion()){
	  hitInfo->setBlind(0);
	  //blind  = 0;
	} else {
	   hitInfo->setBlind(1);
	   // blind = 1;
	}
	hitInfo->setResidual(residual);
      } // found punch point
    }  // end of hit loop
  } // end of track loop
}

void mNewDchAnalyzer::calculateResiduals()
{
  int arm;
  int supposedArm =0;
  int supposedCell = 0;
  int supposedSide = 0;
  int supposedPlane = 0;
  double multi;
  PHPoint p;

  int totalTracks = trackTableList->length();
  
  PHPoint tmpPoint;
  PHVector tmpVector;

  DchRawInfo   *rawInfo;
  DchHitInfo   *hitInfo,*hitMirror;
  DchTrackInfo *trackInfo;
  
  PHPoint punchPoint;

  // clean up the previous list
  for (int k = trackInfoListBest->length()-1; k >= 0; k--) {
    trackInfo = (*trackInfoListBest)[k];
    for (int ientries= (trackInfo->getHitInfoList().length() -1); ientries >= 0; ientries--) {
      hitInfo = trackInfo->getHitInfo(ientries);
      rawInfo = hitInfo->getRaw1();
      delete rawInfo;
      rawInfo = 0;
      rawInfo = hitInfo->getRaw2();
      delete rawInfo;
      rawInfo = 0;
      hitMirror = hitInfo->getMirror();
      delete hitMirror;
      hitMirror  = 0;
      delete hitInfo;
      hitInfo = 0;
    }
    delete trackInfo;
    trackInfo = 0;
  }
  trackInfoListBest->clear();
  // --------------------------------------------------------------
  
  for(int i=0; i<totalTracks; i++) {
      trackInfo = new DchTrackInfo();
      copyTrackInfo(trackInfo,i);
      trackInfoListBest->append(trackInfo);
          
      PHLine  trackLine = getTrackLine(i);  // get the track as a line
      //move the basepoint of the track !

      cout << "trackLine: " << trackLine << endl;
      
      if (trackLine.getDirection().getX() != 0) {
	multi = -trackLine.getBasepoint().getX()/trackLine.getDirection().getX();
      }else if (trackLine.getDirection().getY() != 0) {
        multi = -trackLine.getBasepoint().getY()/trackLine.getDirection().getY();
      }else if (trackLine.getDirection().getZ() != 0) {
	multi = -trackLine.getBasepoint().getZ()/trackLine.getDirection().getZ();
      } else {
	multi = 0.0;
      }

      p = trackLine.getBasepoint() * (1.0 + multi); 
      trackLine.setBasepoint(p);  

      cout << "trackLine: " << trackLine << endl;
      
      int totalNumberOfPlanes = pDchAddress->getPlane()->getMax() + 1;
      arm = trackInfo->getArm();
      
      for(int plane = 0;plane <totalNumberOfPlanes; plane++)  { //Loop over the planes for each track 
	hitInfo = new DchHitInfo();
	trackInfo->appendHitInfo(hitInfo); // ???????????????
	hitInfo->setPlane(plane);

	PHBoolean found = pDchGeometry->getIntersectionTrackWirePlane(trackLine,plane,arm,punchPoint); // check
	if (! found) {
	  hitInfo->setOut(1);
	}else {  //if punch point found !
	  hitInfo->setOut(0);
	
	  supposedArm    = pDchGeometry->findCorrespondingArm(punchPoint);
	  supposedSide   = pDchGeometry->findCorrespondingSide(punchPoint);
	  supposedCell   = pDchGeometry->findCorrespondingCell(punchPoint);
	  supposedPlane  = plane;

	  PHLine supposedWire = pDchGeometry->getWireAsLine(supposedArm,supposedPlane,supposedCell);  
	  PHPlane driftPlane = getDriftPlane(punchPoint,supposedArm,supposedPlane,supposedCell,supposedWire);

	  PHPoint trackIntersectionPoint;
	  float trackToWireDistance;
	
	  if(PHGeometry::intersectionLinePlane(trackLine,driftPlane,trackIntersectionPoint)) {
	    // calculate the distance of the trackIntersectionPoint from the wire
	    trackToWireDistance = PHGeometry::distanceLinePoint(supposedWire,trackIntersectionPoint);
	  }else{
	    continue;
	  }
       
	  int bestHitId;
	  float bestResidual;
	  getBestResidual(supposedArm,supposedSide,supposedPlane,supposedCell,trackToWireDistance,
			  bestResidual,bestHitId);

	  hitInfo->setId(bestHitId);
	  hitInfo->setResidual(bestResidual);
	  if(pDchGeometry->expectSignalFrom(supposedArm,supposedPlane,supposedCell, trackIntersectionPoint) ||
	     trackToWireDistance <pDchGeometry->getDistanceToDriftRegion()){
	    hitInfo->setBlind(0);
	  }else {
	    hitInfo->setBlind(1);
	  }
	  if (bestHitId != -1) {	
            copyHitInfo(hitInfo, bestHitId);
	    
	    int rawid1 =  hitInfo->getIdraw1();
	    int rawid2 =  hitInfo->getIdraw2();
	    
	    rawInfo = new DchRawInfo();
	    rawInfo->setId(rawid1);
	    if (rawid1 != -1) {
	      copyRawInfo(rawInfo,rawid1);
	    }
	    hitInfo->setRaw1(rawInfo);
	    
	    rawInfo = new DchRawInfo();
	    rawInfo->setId(rawid2);
	    if (rawid2 != -1) {
              copyRawInfo(rawInfo,rawid2);
	    }
	    hitInfo->setRaw2(rawInfo);
	  }// if punch point found
	} // loop plane
      } // loop on track
     
  }
}

void mNewDchAnalyzer::calculateEfficiency()
{
  DchHitInfo *hitInfo;
  DchTrackInfo* trackInfo;

  int plane,iplane ;
  int totalHits;
  int totalTracks = trackInfoListBest->length();

  int effi[40];  // ngood
  int back[40];
  int aeffi[40]; // ntotal
  int aback[40];

  int e[6];
  int b[6];
  int te[6];
  int tb[6];
  
  for ( int i =0; i < totalTracks; i++) {
    for (int ii=0; ii<numberOfPlanes; ii++) {
      effi[ii] = 0;
      back[ii] = 0;
      aeffi[ii] = 0;
      aback[ii] = 0;
    }

    for (int iii=0; iii<6 ;iii++) {
      e[iii] =0;
      te[iii] =0;
      b[iii] =0;
      tb[iii] =0;
    }
    trackInfo = (*trackInfoListBest)[i];
    totalHits = trackInfo->getHitInfoList().length();

    cout << "TotalHits "<< totalHits << endl;
    for ( int ihit = 0; ihit < totalHits; ihit++) {
      hitInfo = trackInfo->getHitInfo(ihit);
      plane = ihit;
      int    hitid    = hitInfo->getId();
      short  blind    = hitInfo->getBlind();
      float  residual = hitInfo->getResidual(); // get trackDIstance - hitDistance
      short out       = hitInfo->getOut();
      if ( out ) continue;   // take next hit if tracks is exiting  ?????????????????
      
      //------------------------------------
      if (blind == 0) { // study efficiency
	aeffi[plane]++;
	if (hitid >= 0 && residual < 0.15) {
	  effi[plane]++; // efficient
	}else {
	  // inefficient
	}
      }else if (blind ==1) { // study back efficiency
	aback[plane]++;
	if (hitid >=0 && residual < 0.15) {
	  back[plane]++;
	}else{
	  // back inefficient
	}
      }      
    } // loop on hit == loop on planes

    for (iplane = 0; iplane< numberOfPlanes; iplane++) {
    
      if (iplane < 12) {
	e[0]  = e[0]  + effi[iplane];
	b[0]  = b[0]  + back[iplane];
	te[0] = te[0] + aeffi[iplane];
	tb[0] = tb[0] + aback[iplane];
      }else if (iplane <16) {
	e[1]  = e[1]  + effi[iplane];
	b[1]  = b[1]  + back[iplane];
	te[1] = te[1] + aeffi[iplane];
	tb[1] = tb[1] + aback[iplane];
      }else if (iplane <20) {
	e[2]  = e[2]  + effi[iplane];
	b[2]  = b[2]  + back[iplane];
	te[2] = te[2] + aeffi[iplane];
	tb[2] = tb[2] + aback[iplane];
      }else if (iplane <32) {
	e[3]  = e[3]  + effi[iplane];
	b[3]  = b[3]  + back[iplane];
	te[3] = te[3] + aeffi[iplane];
	tb[3] = tb[3] + aback[iplane];
      }else if (iplane <36) {
	e[4]  = e[4]  + effi[iplane];
	b[4]  = b[4]  + back[iplane];
	te[4] = te[4] + aeffi[iplane];
	tb[4] = tb[4] + aback[iplane];
      }else if (iplane <40) {
	e[5]  = e[5]  + effi[iplane];
	b[5]  = b[5]  + back[iplane];
	te[5] = te[5] + aeffi[iplane];
	tb[5] = tb[5] + aback[iplane];
      }
    }

    if (pDchHistogrammer) {
      pDchHistogrammer->fillEfficiency(e,te,b,tb);
    }else {
      cout << "No histogrammer "<< endl;
    }
  } // loop on tracks
}

void mNewDchAnalyzer::calculateEasyEfficiencyDistribution()
{
}

void mNewDchAnalyzer::calculateEfficiencyDistribution()
{

  int k; 
  PHBoolean  crossingWire, crossingCell;
  int totalTracks = trackTableList->length();
 
  int arm, plane;
  short blind;
  int supposedArm = 0, supposedCell = 0, supposedPlane;

  int supposedEffi[40];   // number of supposed efficient hits for each plane
  int trueEffi[40];       // number of efficient hit truely "seen" for each plane
  
  PHPoint punchPoint;
  DchHitInfo* hitInfo = 0;
  DchTrackInfo* trackInfo = 0;

  short signOfCrossingCell = 0;
  int planeOfCrossingCell = 0;
  int planeOfCrossingWire = 0;
  int previousCell = 0;

  float distTwoSubTrack;
  //fill the hits which should be seen

  int totalCrossingCellTracks = 0;
  int totalCrossingWireTracks = 0;
  int totalNotCrossingTracks  = 0;

  float t0West = pDchCalibration->getNominalT0(WEST);  
  float vdWest = pDchCalibration->getNominalDriftVelocity(WEST);
  float t0East = pDchCalibration->getNominalT0(EAST);  
  float vdEast = pDchCalibration->getNominalDriftVelocity(EAST);

  cout<<".... t0West= "<<t0West<<" , vdWest = "<<vdWest<<endl;
  cout<<".... t0East= "<<t0East<<" , vdEast = "<<vdEast<<endl;

  for(int i=0; i<totalTracks; i++) {

    float t0, vd;

    crossingWire = False;
    crossingCell = False;
    planeOfCrossingCell = -1;
    planeOfCrossingWire = -1;

    for(k=0; k<40;k++){
      supposedEffi[k] = 0;
      trueEffi[k] = 0;
    }
 
   trackInfo = (*trackTableList)[i];
   if (!trackInfo) continue;

    //add some track parameters to the trackInfo
    trackInfo->setAlpha(trackTable->get_alpha(i));
    trackInfo->setBeta(trackTable->get_betaNoVertex(i));
    trackInfo->setPhi(trackTable->get_phi(i));
    trackInfo->setZed(trackTable->get_zed(i));
  
    float alpha = trackInfo->getAlpha();
    float phi   = trackInfo->getPhi();

    PHLine  trackLine = getTrackLine(i);  
    arm = trackInfo->getArm();

    if(arm==1){
      vd = vdWest;
      t0 = t0West; 
    }else{
      vd = vdEast;
      t0 = t0East;
    }

   //-------------------------------------------------------------------------
   //  Find if crossing or not 
   //---------------------------------------------------------------------------
   for( plane = 0; plane < 40; plane++)  {   // Loop over all the planes
       
     if (wireType[plane] == UV1Wire || wireType[plane] == UV2Wire) continue;
        
      PHBoolean found = pDchGeometry->getIntersectionTrackWirePlane(trackLine,plane,arm,punchPoint); 
      if (! found) {
        continue;
      }
      	      
      supposedArm   = pDchGeometry->findCorrespondingArm(punchPoint);
      supposedCell  = pDchGeometry->findCorrespondingCell(punchPoint);
      supposedPlane = plane;
      
      PHLine supposedWire = pDchGeometry->getWireAsLine(supposedArm,supposedPlane,supposedCell);
      PHPlane driftPlane  = getDriftPlane(punchPoint,supposedArm,supposedPlane,supposedCell,supposedWire);
      
      //Get the intersection point between the track and the drift plane and the drift distance 
      //from this point to the wire in the drift plane.
      PHPoint trackIntersectionPoint;

      if(!PHGeometry::intersectionLinePlane(trackLine,driftPlane,trackIntersectionPoint)) 
	continue;
      
      if (plane == 0) {
	previousCell = supposedCell;
      }
      if (previousCell != supposedCell) {
        planeOfCrossingCell = plane;      
	crossingCell = True;
        signOfCrossingCell = supposedCell - previousCell;
        break;
      }
     
   }//end plane loop

  if (crossingCell){
     totalCrossingCellTracks++; 
   }else{
     int pc=-1;   
     if(isCrossingWire(i,supposedArm, supposedCell,pc)&&pc!=-1){

       //set on some flags
       totalCrossingWireTracks++;
       planeOfCrossingWire = pc;
       crossingWire = True;
     } 
   }

    int pc = 16;

    distTwoSubTrack = discontinuity(i,supposedArm,supposedCell,pc,crossingCell,crossingWire,signOfCrossingCell);

   //---------------------------------------------------------------------------

   // continue the normal analyzis
   
   for( plane = 0; plane < 40; plane++)  {   // Loop over all the planes
       
      PHBoolean found = pDchGeometry->getIntersectionTrackWirePlane(trackLine,plane,arm,punchPoint); 
      if (! found) {
        continue;
      }
      	      
      supposedArm   = pDchGeometry->findCorrespondingArm(punchPoint);
      supposedCell  = pDchGeometry->findCorrespondingCell(punchPoint);
      supposedPlane = plane;
      
      PHLine supposedWire = pDchGeometry->getWireAsLine(supposedArm,supposedPlane,supposedCell);
      PHPlane driftPlane  = getDriftPlane(punchPoint,supposedArm,supposedPlane,supposedCell,supposedWire);
      
      //Get the intersection point between the track and the drift plane and the drift distance 
      //from this point to the wire in the drift plane.
      PHPoint trackIntersectionPoint;
      float trackToWireDistance;

      if(PHGeometry::intersectionLinePlane(trackLine,driftPlane,trackIntersectionPoint)) {
	trackToWireDistance = PHGeometry::distanceLinePoint(supposedWire,trackIntersectionPoint);
      }else{
	continue;
      }

      if(pDchGeometry->expectSignalFrom(supposedArm,supposedPlane,supposedCell,trackIntersectionPoint) ||
	 trackToWireDistance <pDchGeometry->getDistanceToDriftRegion()){
	blind  = 0;
      } else {
	blind  = 1;
      }
      
      //fill the efficient hits which should be seen
      if(blind==0 && trackToWireDistance>0.6 && trackToWireDistance <1.78){ //
	  supposedEffi[plane]++; 
	  pDchHistogrammer->fillSupposedEffiPlane(plane);
      }		   
      
      if(blind==0){
	pDchHistogrammer->fillSupposedEffiAll(trackToWireDistance);
	if(supposedPlane<12) 
	  pDchHistogrammer->fillSupposedEffiX1(trackToWireDistance);
	else if(supposedPlane<16)
	  pDchHistogrammer->fillSupposedEffiU1(trackToWireDistance);
	else if(supposedPlane<20)
	  pDchHistogrammer->fillSupposedEffiV1(trackToWireDistance);
	else if(supposedPlane<32)
	  pDchHistogrammer->fillSupposedEffiX2(trackToWireDistance);
	else if(supposedPlane<36)
	  pDchHistogrammer->fillSupposedEffiU2(trackToWireDistance);
	else if(supposedPlane<40)
	  pDchHistogrammer->fillSupposedEffiV2(trackToWireDistance);
      }  // fill histo
      
      //--------------------------------------------------------
      //  Found the hit for this plane 
      //-------------------------------------------------------
      int foundHit = 0;
      for (unsigned int kk = 0; kk < trackInfo->getHitInfoList().length(); kk++) {
	hitInfo = trackInfo->getHitInfo(kk);
        if (hitInfo->getPlane() == plane) {
          foundHit = 1;
	  break; 
	}
      }
      if (!foundHit) continue;
      float hitDistance = hitInfo->getDistance();
      float residual = trackToWireDistance - hitDistance;

      float array[14];
      int m=0;
      array[m++]  = i;
      array[m++]  = arm;
      array[m++]  = residual;
      array[m++]  = trackToWireDistance;                   
      array[m++]  = plane;
      array[m++]  = crossingCell;
      array[m++]  = crossingWire;
      array[m++]  = planeOfCrossingCell;
      array[m++]  = planeOfCrossingWire;
      array[m++]  = t0; 
      array[m++]  = vd;
      array[m++]  = distTwoSubTrack;
      array[m++]  = alpha;
      array[m++]  = phi;
 
      pDchHistogrammer->fillResiduals(array);

      //-----------------------------------
      // fill the hit residuals 
      //-----------------------------------
      if(blind==0 ){
	if((plane<12)||(plane>19&&plane<32)) {
	  pDchHistogrammer->fillResidualX(residual);      
	}else {
	  pDchHistogrammer->fillResidualUV(residual);
	}
      }
      
      if(blind==0 && trackToWireDistance>0.6 && trackToWireDistance <1.78){ //same condition for supp.
	if (residual < resWin) {
	  trueEffi[plane]++; 
	  pDchHistogrammer->fillTrueEffiPlane(plane);
	}
      }		           
      if (blind == 0  && residual < resWin ) {
	pDchHistogrammer->fillTrueEffiAll(trackToWireDistance);
	
	if(plane<12)
	  pDchHistogrammer->fillTrueEffiX1(trackToWireDistance);
	else if(plane<16)
	  pDchHistogrammer->fillTrueEffiU1(trackToWireDistance);
	else if(plane<20)
	  pDchHistogrammer->fillTrueEffiV1(trackToWireDistance);
	else if(plane<32)
	  pDchHistogrammer->fillTrueEffiX2(trackToWireDistance);
	else if(plane<36)
	  pDchHistogrammer->fillTrueEffiU2(trackToWireDistance);
	else if(plane<40)
	  pDchHistogrammer->fillTrueEffiV2(trackToWireDistance);
      }
      
   } //end of plane loop

  }//end of track loop

  cout << "total Crossing CELL Tracks "<< totalCrossingCellTracks << endl;
  cout << "total Crossing WIRE Tracks "<< totalCrossingWireTracks << endl;
  cout << "total NOT CROSSING Tracks  "<<  totalNotCrossingTracks  << endl;
  cout << "total Tracks  "<< totalTracks   << endl;

}

void mNewDchAnalyzer::calculateBestTrackEfficiency()
{
  /*
 
  */
}

PHLine mNewDchAnalyzer::getTrackLine(int trkCntr)
{
  
  DchTrackInfo* trackInfo = (*trackTableList)[trkCntr];
  PHLine trackLine(trackInfo->getBasepoint(),trackInfo->getDirection());

  return trackLine;
}

PHPlane mNewDchAnalyzer::getDriftPlane(PHPoint punchPoint,int arm,int plane,int cell,PHLine wire)
{

	//Get the local base point for the drift plane  /// restriction ?????
	float wireParameter = (punchPoint.getZ() - wire.getBasepoint().getZ())/wire.getDirection().getZ();
	
	float AUXPunchCoordX = (wire.getBasepoint().getX() + wireParameter * wire.getDirection().getX());
	float AUXPunchCoordY = (wire.getBasepoint().getY() + wireParameter * wire.getDirection().getY());	
	float AUXPunchCoordZ =  punchPoint.getZ();
	
	PHVector localDriftPlane_Basepoint;
        localDriftPlane_Basepoint.setX(AUXPunchCoordX);
	localDriftPlane_Basepoint.setY(AUXPunchCoordY);
	localDriftPlane_Basepoint.setZ(AUXPunchCoordZ);
	
	//Get the drift direction of the specific cell
	PHVector driftNorth = pDchGeometry->getWireDriftDirectionNorth(arm,plane,cell);
	PHVector driftSouth = pDchGeometry->getWireDriftDirectionSouth(arm,plane,cell);		  
	
	//Get the local Normal of the drift plane  // ???? getLengthOfXWire (1)
	float fraction = (punchPoint.getZ() -wire.getBasepoint().getZ())/pDchGeometry->getLengthOfXWire(1);
	PHVector localDriftDirection = (driftNorth + (driftSouth - driftNorth)*fraction);
	PHVector localDriftPlane_Normal = localDriftDirection.cross(wire.getDirection());    
	
	PHPlane driftPlane(localDriftPlane_Basepoint,localDriftPlane_Normal);		  

        return driftPlane;
	
}

void  mNewDchAnalyzer::getBestResidual(int arm, int side, int plane, int cell, float twDist,
				       float& returnResidual, int& returnHitCntr)
{
       float bestResidual = 99999;
       int bestHitCntr = -1;
	//loop over all Hit entries and check whether they come from the right wire, whether the
	//   wire was blind or seeing to that side and whether the hit had the right distance
        int totalHits = hitLineTable->Entries();
	int hitCntr = 0;
	while(hitCntr < totalHits) {
	  if(arm    == hitLineTable->getArm(hitCntr) &&  // Find the hit in the correct cell
	     side   == hitLineTable->getSide(hitCntr)&&
	     plane  == hitLineTable->getPlane(hitCntr)&&
	     cell   == hitLineTable->getCell(hitCntr)) {  

	    float hitDistance = hitLineTable->getDistance(hitCntr);
	    float residual = twDist - hitDistance;
	    if(fabs(residual) < fabs(bestResidual)) {
	      bestResidual = residual;
	      bestHitCntr = hitCntr;
	    }
	  }
	  hitCntr++;
	}

       returnHitCntr = bestHitCntr;
       returnResidual = bestResidual;

}

void  mNewDchAnalyzer::getResidual(int arm, int side, int plane, int cell, int trackID, float twDist,
				       float& returnResidual, int& returnHitCntr)
{

  DchTrackInfo* trackInfo= (*trackTableList)[trackID];

  int totalHits = trackInfo->getHitInfoList().length();
  
  for (int i=0; i< totalHits; i++) {
    DchHitInfo *hit = trackInfo->getHitInfo(i);
    if (hit->getPlane() == plane) {
      float hitDistance = hit->getDistance();
      returnResidual = twDist - hitDistance;
      returnHitCntr = hit->getId();
      hit->setResidual(returnResidual);
      return;
    }
  }
}

//------------------------------------------------------------------
void mNewDchAnalyzer::completeTrackCandidateList()
{
  // need the trackTable list and (eventually) the trackInfoListBest
  int ic,it;
  int numberOfCandidates;
  int numberOfTracks;
  DchCandidate* candidate;
  DchTrackInfo* trackInfo;
  
  if (candidateList && trackTableList) {
    numberOfCandidates = candidateList->length();
    numberOfTracks     = trackTableList->length();
    for(ic=0; ic< numberOfCandidates; ic++) {
      candidate = (*candidateList)[ic];
      if (candidate) {
	it = candidate->getTrackId();
	if (it >=0 && it < numberOfTracks) {
	  trackInfo = (*trackTableList)[it];
	  candidate->setTrackInfo(trackInfo);
	  trackInfo->setCandidate(candidate);
	}
      }
    }
  }
  
  if (candidateList && trackInfoListBest) {
    numberOfCandidates = candidateList->length();
    numberOfTracks     = trackInfoListBest->length();
    for(ic=0; ic< numberOfCandidates; ic++) {
      candidate = (*candidateList)[ic];
      if (candidate) {
	it = candidate->getTrackId();
        if (it>=0 && it < numberOfTracks) {
	  trackInfo = (*trackTableList)[it]; 
	  trackInfo->setCandidate(candidate);
	}
      }
    }   
  }
 
}

void  mNewDchAnalyzer::completeHitTableList()
{
  int hitID;
  if (trackTableList && hitTableList) {
    int totalTracks = trackTableList->length();
    int totalHits   = hitTableList->length();
  
    DchHitInfo*   hitInfo;
  
    PHPoint tmpPoint;
    PHVector tmpVector;
    
    for (int hitid =0; hitid < totalHits; hitid++) {
      hitInfo = (*hitTableList)[hitid];
      for (int trackid = 0; trackid < totalTracks; trackid++) {
	for (int ilocal= 0; ilocal < numberOfPlanes; ilocal++) {
	  hitID = trackTable->get_hits(trackid, ilocal);
	  if (hitID == hitid) {
	    hitInfo->setTrackId(trackid);
	    tmpPoint = trackTable->get_point(trackid);
	    tmpVector= trackTable->get_direction(trackid);
	
	    hitInfo->setTrackBasepoint(tmpPoint);
	    hitInfo->setTrackDirection(tmpVector);
	    
	  } // found hit
	} // loop on plane
      } // loop on tracks  
    } // hit -> fill info
  } // if table exist
}

PHBoolean mNewDchAnalyzer::fillHitInfo()
{
 if (pDchHistogrammer) {
    pDchHistogrammer->fillHitInfo(hitTableList);
    return True;
  }else {
    cout << "No histogrammer "<< endl;
    return False;
  }
}
PHBoolean mNewDchAnalyzer::fillTrackInfo()
{
 if (pDchHistogrammer) {
    pDchHistogrammer->fillTrackInfo(trackTableList);
    return True;
  }else {
    cout << "No histogrammer "<< endl;
    return False;
  }
}
PHBoolean mNewDchAnalyzer::fillTrackInfoBest()
{
 if (pDchHistogrammer) {
    pDchHistogrammer->fillTrackInfo(trackInfoListBest);
    return True;
  }else {
    cout << "No histogrammer "<< endl;
    return False;
  }
}

//------------------------------------------------------------------

PHBoolean  mNewDchAnalyzer::copyTrackInfo(DchTrackInfo *track, int itrack)
{
  PHPoint tmpPoint, errPoint;
  PHVector tmpVector, errVector;
  
  track->setId(trackTable->get_trackid(itrack));
  track->setArm(trackTable->get_arm(itrack));
  track->setSide(trackTable->get_side(itrack));
  
  //copying the tracks' parameters
  track->setAlpha(trackTable->get_alpha(itrack));
  track->setBeta(trackTable->get_betaNoVertex(itrack));
  track->setPhi(trackTable->get_phi(itrack));
  track->setZed(trackTable->get_zed(itrack));
  
  cout<<"alpha, beta, phi, zed"<<track->getAlpha()<<"  "<<track->getBeta()<<"  "<<track->getPhi()<<"  "<<track->getZed()<<endl;
  
  tmpPoint = trackTable->get_point(itrack);
  tmpVector= trackTable->get_direction(itrack);
  track->setBasepoint(tmpPoint);
  track->setDirection(tmpVector);

  /*  Chris Pinkenburg: wait until this is really implemented
  tmpPoint  = trackTable->getErr_Point(itrack);
  tmpVector = trackTable->getErr_Direction(itrack);
  */
  track->setErrorBasepoint(errPoint);
  track->setErrorDirection(errVector);

  return True;
}

PHBoolean mNewDchAnalyzer::copyHitInfo(DchHitInfo* hit, int hitid)
{
  PHPoint tmpPoint;
  PHVector tmpVector;
  int arm,plane,cell;
  float distance;
  
  arm   = hitLineTable->getArm(hitid);
  plane = hitLineTable->getPlane(hitid);
  cell  = hitLineTable->getCell(hitid);
  distance = hitLineTable->getDistance(hitid);

  short sign;
  sign = 1; // the sign info has been introduced in the drift direction
  hit->setId(hitid);
  hit->setArm(hitLineTable->getArm(hitid));
  hit->setSide(hitLineTable->getSide(hitid));
  hit->setPlane(hitLineTable->getPlane(hitid));
  hit->setCell(hitLineTable->getCell(hitid));
  hit->setDistance(sign*distance);
  hit->setWidth(hitLineTable->getWidth(hitid));
  hit->setIdraw1(hitLineTable->getIdraw1(hitid));
  hit->setIdraw2(hitLineTable->getIdraw2(hitid));
  hit->setIdmirror(hitLineTable->getIdmirror(hitid));
  hit->setUsed(hitLineTable->getUsed(hitid));
 
  hit->setNorth(pDchGeometry->transformDistanceToLine(arm,plane,cell,sign*distance).getBasepoint());
  hit->setSouth(hit->getNorth()+pDchGeometry->transformDistanceToLine(arm,plane,cell,sign*distance).getDirection());

  tmpPoint  = hitLineTable->getXYZ(hitid);
  tmpVector = hitLineTable->getVXYZ(hitid);

  hit->setBasepoint(tmpPoint);
  hit->setDirection(tmpVector);
  tmpPoint = hitLineTable->getEXYZ(hitid);
  hit->setError(tmpPoint);

  return True;
}
PHBoolean mNewDchAnalyzer::copyRawInfo(DchRawInfo* raw, int iraw)
{
  raw->setId(rawTable->getId(iraw));
  raw->setArm(rawTable->getArm(iraw));
  raw->setSide(rawTable->getSide(iraw));
  raw->setPlane(rawTable->getPlane(iraw));
  raw->setCell(rawTable->getCell(iraw));
  raw->setEdge(rawTable->getEdge(iraw));
  raw->setTime(rawTable->getTime(iraw));
  return True;
}

int mNewDchAnalyzer::isCrossingWire(int trk, int arm, int cell, int& pCrossing){

  //construct the track 
   PHLine trackLine = getTrackLine(trk);  

  //construct the x wire plane

   PHPoint p1 = pDchGeometry->getWireBasepointNorth(arm,0,cell);
   PHPoint p2 = pDchGeometry->getWireBasepointSouth(arm,0,cell);
   PHPoint p3 = pDchGeometry->getWireBasepointNorth(arm,31,cell);

   PHPlane XWirePlane(p1,p2,p3);

   //get intersection between the track and the wire plane
   PHPoint ip;

   if(PHGeometry::intersectionLinePlane(trackLine,XWirePlane,ip)){
     double radius = sqrt(ip.getX()*ip.getX()+ip.getY()*ip.getY());

     for(int i=0;i<32;i++){
       if(radius > pDchGeometry->getAverageRadius(i)&&
          radius < pDchGeometry->getAverageRadius(i+1)){
	 pCrossing = i+1;
         return 1;
       }
     }
     pCrossing = -1;
     return 1;
   }

    return 0;
}
 
float mNewDchAnalyzer::discontinuity(int i, int arm, int cell, int planeCrossing,PHBoolean cc, PHBoolean cw,int signOfCrossingCell){

  DchTrackInfo *trackInfo;

  if(cc==True&&cw==True) return -999;
  if(cc==False&&cw==False) return -999;

  trackInfo = (*trackTableList)[i];

  int totalX1 = countX1(trackInfo);//number of good X1 hits
  int totalX2 = countX2(trackInfo);//number of good X2 hits

  if(totalX1<4 || totalX2 <4)
    return -999;
  
  if(cc== False && cw==True){
    float timeX1 = fitX1WCLine(trackInfo,planeCrossing);
    float timeX2 = fitX2WCLine(trackInfo,planeCrossing);

    if(timeX1 == -999 || timeX2 == -999) return -999;
  
    return timeX2+timeX1;
  }

  if(cc==True && cw==False)
    return  fitXCCLine(trackInfo,planeCrossing,signOfCrossingCell);

  return -999;
}
   
int mNewDchAnalyzer::countX1(DchTrackInfo* trackInfo){

  int count=0;
  if(!trackInfo) return -1;

  int totalHits = (trackInfo->getHitInfoList()).length();
  DchHitInfo* hitInfo;

  for(int i=0;i<totalHits;i++){
    hitInfo = trackInfo->getHitInfo(i);

    if(hitInfo->getPlane()<12){//X1 hit
      if(hitInfo->getDistance()>0.3 && hitInfo->getDistance()<1.7 && hitInfo->getBlind()==0){
        count++;
      }
    }
  }

  return count;
}
  
int mNewDchAnalyzer::countX2(DchTrackInfo* trackInfo){

  int count=0;
  if(!trackInfo) return -1;

  int totalHits = (trackInfo->getHitInfoList()).length();
  DchHitInfo* hitInfo;

  for(int i=0;i<totalHits;i++){
    hitInfo = trackInfo->getHitInfo(i);

    if(hitInfo->getPlane()>19 && hitInfo->getPlane()<32){//X2 hit
      if(hitInfo->getDistance()>0.3 && hitInfo->getDistance()<1.7 && hitInfo->getBlind()==0){
        count++;
      }
    }
  }

  return count;
}

float mNewDchAnalyzer::fitX1WCLine(DchTrackInfo* trackInfo, int pc){

  float x[12], y[12], w[12];
  int n=0;
  float slope, intercept, chisq, sigSlope, sigIntercept;

  if(!trackInfo)
    PHMessage("mNewDchAnalyzer::fitX1Line",PHError,"no track");

  for(int i=0;i<12;i++)
    x[i]=y[i]=w[i]= 0;

  int totalHits = (trackInfo->getHitInfoList()).length();
  DchHitInfo* hitInfo;

  for(int i=0;i<totalHits;i++){
    hitInfo = trackInfo->getHitInfo(i);

    if(hitInfo->getPlane()<12 ){//X1 hit
      if(hitInfo->getDistance()>0.3 && hitInfo->getDistance()<1.7 && hitInfo->getBlind()==0){
	x[n] = hitInfo->getDistance();
	y[n] =  pDchGeometry->getAverageRadius(hitInfo->getRaw1()->getPlane());
	n++;
      }
    }
  }

  for(int i=0;i<12;i++)
    w[i] = 1.;

  if(LineFit(x,y,w,n,&slope,&intercept,&chisq,&sigSlope,&sigIntercept) == -1)
    return -999;

  float planeCrossing =  pDchGeometry->getAverageRadius(pc);
  float fittedTimeCrossing = (planeCrossing-intercept)/slope; 
  
  return fittedTimeCrossing;

}

float mNewDchAnalyzer::fitX2WCLine(DchTrackInfo* trackInfo, int pc){

  float x[12], y[12], w[12];
  int n=0;
  float slope, intercept, chisq, sigSlope, sigIntercept;

  if(!trackInfo)
    PHMessage("mNewDchAnalyzer::fitX2Line",PHError,"no track");

  for(int i=0;i<12;i++)
    x[i]=y[i]=w[i]= 0;

  int totalHits = (trackInfo->getHitInfoList()).length();
  DchHitInfo* hitInfo;

  for(int i=0;i<totalHits;i++){
    hitInfo = trackInfo->getHitInfo(i);

    if(hitInfo->getPlane()>19 && hitInfo->getPlane()<32 ){//X2 hit
      if(hitInfo->getDistance()>0.3 && hitInfo->getDistance()<1.7 && hitInfo->getBlind()==0){
        x[n] = hitInfo->getDistance();
	y[n] =  pDchGeometry->getAverageRadius(hitInfo->getRaw1()->getPlane());
        n++;
      }
    }
  }
 
  for(int i=0;i<12;i++)
    w[i] = 1.;

  if( LineFit(x,y,w,n,&slope,&intercept,&chisq,&sigSlope,&sigIntercept) == -1)
    return -999;

  float planeCrossing =  pDchGeometry->getAverageRadius(pc);

  float fittedTimeCrossing = (planeCrossing-intercept)/slope; 

  return fittedTimeCrossing;
}

float mNewDchAnalyzer::fitXCCLine(DchTrackInfo* trackInfo,int pc,int signOfCrossingCell){
    
  float x1[12],x2[12], y1[12],y2[12], w[12];
  int n1=0,n2=0;
  float slope, intercept, chisq, sigSlope, sigIntercept;

  if(!trackInfo)
    PHMessage("mNewDchAnalyzer::fitXCCLine",PHError,"no track");

  for(int i=0;i<12;i++)
    x1[i]=x2[i]=y1[i]=y2[i];

  int totalHits = (trackInfo->getHitInfoList()).length();
  DchHitInfo* hitInfo;

  for(int i=0;i<totalHits;i++){
    hitInfo = trackInfo->getHitInfo(i);
    if(hitInfo->getPlane()<12){ //X1 hits
	x1[n1] = hitInfo->getBasepoint().getX();
	y1[n1] =  hitInfo->getBasepoint().getY() ;
        n1++;
    } else if(hitInfo->getPlane()>19 && hitInfo->getPlane()<32 ){//X2 hit
      if(hitInfo->getDistance()>0.3 && hitInfo->getDistance()<1.7 && hitInfo->getBlind()==0){
	x2[n2] = hitInfo->getBasepoint().getX();
	y2[n2] =  hitInfo->getBasepoint().getY() ;
        n2++;
      }
    }
  }
  
  for(int i=0;i<12;i++)
    w[i] = 1.;
  
  if( LineFit(x1,y1,w,n1,&slope,&intercept,&chisq,&sigSlope,&sigIntercept) == -1)
    return -999;

  float r =  pDchGeometry->getAverageRadius(pc);

  float k  = slope;
  float b  = intercept;
  float a1 = -k*b/(1+k*k);
  float a2 = sqrt((1+k*k)*r*r-b*b)/(1+k*k);
  float s  = x1[2]/fabs(x1[2]);
  float x1cross = a1 + s*a2;
  float y1cross = k*x1cross+b;

 for(int i=0;i<12;i++)
    w[i] = 1.;
 
  if( LineFit(x2,y2,w,n2,&slope,&intercept,&chisq,&sigSlope,&sigIntercept) == -1)
    return -999;

   k  = slope;
   b  = intercept;
   a1 = -k*b/(1+k*k);
   a2 = sqrt((1+k*k)*r*r-b*b)/(1+k*k);
   s  = x2[2]/fabs(x2[2]);
  float x2cross = a1 + s*a2;
  float y2cross = k*x1cross+b;

  float phi1 = atan2(y1cross,x1cross);
  float phi2 = atan2(y2cross,x2cross);

  int sg = 0;
  if(signOfCrossingCell>0)
    sg = (phi1>phi2)?+1:-1;
  else
    sg = (phi1>phi2)?-1:+1;
  
  return sg*sqrt((x1cross-x2cross)*(x1cross-x2cross)+(y1cross-y2cross)*(y1cross-y2cross));

}

int LineFit(float x[],float y[],float w[],int n,float *a,float *b,float *chisq,float *siga,float *sigb){

  double sum,sx,sy,sxx,sxy,syy,det;
  float chi;
  int i;

  /* Executable Statements */


  /*     N must be >= 2 for this guy to work */

  if (n < 2) 
    {
      printf("utiLineFit-W: Too few points for line fit \n");
      return(-1);
    }

  /* initialization  */
  
  sum = 0.0;
  sx = 0.0;
  sy = 0.0;
  sxx = 0.0;
  sxy = 0.0;
  syy = 0.0;

  /* find sum , sumx ,sumy, sumxx, sumxy */

  for (i=0; i<n; i++) 
    {
      sum = sum + w[i];
      sx = sx  + (w[i])*(x[i]);
      sy = sy  + (w[i])*(y[i]);
      sxx = sxx + (w[i])*(x[i])*(x[i]);
      sxy = sxy + (w[i])*(x[i])*(y[i]);
      syy = syy + (w[i])*(y[i])*(y[i]);
    }

  det = sum*sxx-sx*sx;
  if (fabs(det) < 1.0e-20) return(-1);

  /* compute the best fitted parameters A,B */

  *a = (sum*sxy-sx*sy)/det;
  *b = (sy*sxx-sxy*sx)/det;
    
  /* calculate chi-square */
  
  chi = 0.0;
  for (i=0; i<n; i++) 
    {
      chi = chi+(w[i])*((y[i])-*a*(x[i])-*b)*
	((y[i])-*a*(x[i])-*b);
    }
  
  /* calculate estimated variance */
  /* varsq=chi/((((float) n))-2.) */
  
  /*  calculate covariance matrix */
  /*  siga=sqrt(varsq*sxx/det) */
  /*  sigb=sqrt(varsq*sum/det) */
  
  *siga = sum/det;
  *sigb = sxx/det;
  
  *chisq = chi;
  return(0);
    
}   /* end utiLineFit */















