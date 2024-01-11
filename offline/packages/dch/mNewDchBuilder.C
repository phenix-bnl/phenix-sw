//INCLUDECHECKER: Removed this line: #include "PHDchHistogrammer.hh"
#include "mNewDchBuilder.hh"

#include "PHDchGeometryObject.h"
//INCLUDECHECKER: Removed this line: #include "PHDchAddressObject.h"
//INCLUDECHECKER: Removed this line: #include "PHDchCalibrationObject.h"
//INCLUDECHECKER: Removed this line: #include "PHDchNoiseObject.h"

//INCLUDECHECKER: Removed this line: #include "DchAnaPar.h"

//INCLUDECHECKER: Removed this line: #include "table_header.h"


//INCLUDECHECKER: Removed this line: #include "PHNode.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNodeIterator.h"
#include "PHTable.hh"
//INCLUDECHECKER: Removed this line: #include "PHTimeStamp.h"
//INCLUDECHECKER: Removed this line: #include "PdbBankID.hh"

#include <iostream>

using namespace std;

typedef PHIODataNode<PHObject> PHObjectNode_t;
typedef PHIODataNode<PHTable> TableNode_t;
typedef PHIODataNode<DchTrack> DchTrackNode_t;

mNewDchBuilder::mNewDchBuilder()
{
   dchAddress   = 0;
   dchGeometry  = 0;
   dchNoise     = 0;
   dchCalibration = 0;
   dchHistogrammer   = 0;
   trackTableList = 0;
   hitTableList = 0;
   rawTableList = 0;
 }

mNewDchBuilder::~mNewDchBuilder()
{
  trackTableList->clearAndDestroy();
  hitTableList->clearAndDestroy();
  rawTableList->clearAndDestroy();
}

PHBoolean mNewDchBuilder::event(PHCompositeNode *topNode) {

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

 pTopNode = topNode;
 PHObjectNode_t* phob;

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

 nodes.clear();
 
// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

 // extract the candidate Node and other tmp nodes
  
 tmpCandiNode = (PHDataNode<PHPointerList<DchCandidate> >*)i.findFirst("PHDataNode","DchCandidate");
 candidateList = tmpCandiNode->getData();
 
 tmpRawNode = (PHDataNode<PHPointerList<DchRawInfo> >*)i.findFirst("PHDataNode","DchRawInfo");
 rawTableList = tmpRawNode->getData();
 rawTableList->clearAndDestroy();
 
 tmpHitNode = (PHDataNode<PHPointerList<DchHitInfo> >*)i.findFirst("PHDataNode","DchHitInfo");
 hitTableList = tmpHitNode->getData();
 hitTableList->clearAndDestroy();

 tmpTrackNode = (PHDataNode<PHPointerList<DchTrackInfo> >*)i.findFirst("PHDataNode","DchTrackInfo");
 trackTableList = tmpTrackNode->getData();
 trackTableList->clearAndDestroy();
  
  // Detector objects for the tree
 
 pDchDaoNode = (PHDataNode<PHDchAddressObject>*)i.findFirst("PHDataNode","DchDAO");
 dchAddress = pDchDaoNode->getData();
 
 pDchDgoNode = (PHDataNode<PHDchGeometryObject>*)i.findFirst("PHDataNode","DchDGO");
 dchGeometry  = pDchDgoNode->getData();
   
 pDchDcoNode = (PHDataNode<PHDchCalibrationObject>*)i.findFirst("PHDataNode","DchDCO");
 dchCalibration = pDchDcoNode->getData();
  
 pDchDnoNode = (PHDataNode<PHDchNoiseObject>*)i.findFirst("PHDataNode","DchDNO");
 dchNoise = pDchDnoNode->getData();
  
 pDchHistoNode = (PHDataNode<PHDchHistogrammer>*)i.findFirst("PHDataNode","DchHisto");
 dchHistogrammer = pDchHistoNode->getData();
  
 return callPAM(nodes);

}

PHBoolean mNewDchBuilder::callPAM(PHPointerList<PHNode> &nl)
{

  fillRawTableList(); 
  fillHitTableList();
  fillTrackTableList();
  completeHitTableList();
  completeTrackCandidateList(); //need the trackTableList and (eventually) the trackInfoListBest

  return True;
}

void mNewDchBuilder::fillRawTableList()
{
  DchRawInfo* rawInfo;
  if (rawTable) {
    int totalRaws = rawTable->Entries();
    for (int i=0; i < totalRaws; i++) {
      rawInfo = new DchRawInfo();
      copyRawInfo(rawInfo, i);
      rawTableList->append(rawInfo);
    }
  }else{
    cout << "Raw Table missing from the Tree "<< endl;
  }
}

void mNewDchBuilder::fillHitTableList()
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
    cout << "Hit Table missing from the Tree "<< endl;
  }
}

void mNewDchBuilder::fillTrackTableList()
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
    cout << "Track Table missing from the Tree "<< endl;
  }
}

//------------------------------------------------------------------
void mNewDchBuilder::completeTrackCandidateList()
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
    
}

void  mNewDchBuilder::completeHitTableList()
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
	    tmpVector =trackTable->get_direction(trackid);

	    hitInfo->setTrackBasepoint(tmpPoint);
	    hitInfo->setTrackDirection(tmpVector);
	    
	  } // found hit
	} // loop on plane
      } // loop on tracks  
    } // hit -> fill info
  } // if table exist
}

//------------------------------------------------------------------

PHBoolean  mNewDchBuilder::copyTrackInfo(DchTrackInfo *track, int itrack)
{
  PHPoint tmpPoint, errPoint;
  PHVector tmpVector, errVector;
  
  track->setId(trackTable->get_trackid(itrack));
  track->setArm(trackTable->get_arm(itrack));
  track->setSide(trackTable->get_side(itrack));
  tmpPoint  = trackTable->get_point(itrack);
  tmpVector = trackTable->get_direction(itrack);

  track->setBasepoint(tmpPoint);
  track->setDirection(tmpVector);
  
  /* Chris Pinkenburg: Wait until this is really implemented
  tmpPoint = trackTable->getErr_Point(itrack);
  tmpVector = trackTable->getErr_Direction(itrack);
  */
  track->setErrorBasepoint(errPoint);
  track->setErrorDirection(errVector);

  return True;
}

PHBoolean mNewDchBuilder::copyHitInfo(DchHitInfo* hit, int hitid)
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
 
  hit->setNorth(dchGeometry->transformDistanceToLine(arm,plane,cell,sign*distance).getBasepoint());
  hit->setSouth(hit->getNorth()+dchGeometry->transformDistanceToLine(arm,plane,cell,sign*distance).getDirection());

  tmpPoint = hitLineTable->getXYZ(hitid);
  tmpVector = hitLineTable->getVXYZ(hitid);

  hit->setBasepoint(tmpPoint);
  hit->setDirection(tmpVector);
  tmpPoint = hitLineTable->getEXYZ(hitid);
  hit->setError(tmpPoint);

  return True;
}
PHBoolean mNewDchBuilder::copyRawInfo(DchRawInfo* raw, int iraw)
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

  
       
