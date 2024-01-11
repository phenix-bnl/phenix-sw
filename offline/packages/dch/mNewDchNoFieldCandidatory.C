#include "mNewDchNoFieldCandidatory.hh"
#include "DchFastCandidateFinder.hh"
#include "DchHitAssociator.hh"
#include "DchLineTracker.hh"
#include "DchHitLineOutv2.hh"
#include "dDchHitWrapper.h"
#include "dDchTracksWrapper.h"

#include <Bbc.hh>
#include <BbcOut.h>
#include <dPadClusterWrapper.h>
#include <VtxOut.h>

#include <PHIODataNode.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHTable.hh>
#include <table_header.h>

#include <iostream>

using namespace std;

typedef PHIODataNode<PHObject> PHObjectNode_t;
typedef PHIODataNode<dDchHitWrapper> dDchHitNode_t;
typedef PHIODataNode<dDchTracksWrapper> dDchTracksNode_t;
typedef PHIODataNode<dPadClusterWrapper> dPadClusterNode_t;

typedef PHIODataNode<PHTable> TableNode_t;
typedef PHIODataNode<BbcOut> BbcOutNode_t;
typedef PHIODataNode<DchTrack> DchTrackNode_t;
typedef PHIODataNode<VtxOut> VtxOutNode_t;

mNewDchNoFieldCandidatory::mNewDchNoFieldCandidatory():
  dchGeometryObject(NULL),
  dchAddressObject(NULL),
  dchCalibrationObject(NULL),
  dchNoiseObject(NULL),
  trackCandidateList(NULL),
  hitLineLists(NULL),
  hitLineTablev1(NULL),
  hitLineTablev2(NULL),
  trackTablev1(NULL)
{
}

mNewDchNoFieldCandidatory::~mNewDchNoFieldCandidatory(){}

PHBoolean mNewDchNoFieldCandidatory::event(PHCompositeNode *root) {

  PHObjectNode_t *phob;
  PHPointerList<PHNode> nodes;
  PHNodeIterator i(root);
  PHNode *n;
  
 PHDataNode<PHDchAddressObject>* dchDaoNode;
 PHDataNode<PHDchGeometryObject>* dchDgoNode;
 PHDataNode<PHDchCalibrationObject>* dchDcoNode;
 PHDataNode<PHDchNoiseObject>* dchDnoNode;
 PHDataNode<PHPointerList<DchTrackCandidate> >* tmpTrackCandiNode;
 PHDataNode<DchHitLineLists>* tmpHitLineListsNode;

 nodes.clear();
 
// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

  n = i.findFirst("PHIODataNode", "dDchHit");
  nodes.append(n);		     
  n = i.findFirst("PHIODataNode", "dDchTracks");
  nodes.append(n);		     
  n = i.findFirst("PHIODataNode", "BbcOut");
  nodes.append(n);		     
  n = i.findFirst("PHIODataNode", "dPc1Cluster");
  nodes.append(n);		     
  n = i.findFirst("PHIODataNode", "VtxOut");
  nodes.append(n);		     

  //-------------
  PHTypedNodeIterator <DchHitLineTable> iter1(root);
PHIODataNode <DchHitLineTable> * node1 = iter1.find("DchHitLineTable");
  if(node1)
{
  hitLineTablev2 = node1->getData();
  }
else
  {
    cout << PHWHERE << "DchHitLineTable Node not found "<< endl;
  }  

  phob = static_cast < PHObjectNode_t * >(i.findFirst ("PHIODataNode", "DchHitLineTablev1"));
  hitLineTablev1 = dynamic_cast < DchHitLineTable * >(phob->getData ());
  if (!hitLineTablev1) {
    cout << "DchHitLineTablev1 not found "<< endl;
    return False;
  }

  PHTypedNodeIterator <DchTrack> dchtrackiter(root);
  DchTrackNode_t *DchTrackNode = dchtrackiter.find("DchTrack");
  if (DchTrackNode)
    {
  trackTablev1 = DchTrackNode->getData();
    }
else
  {
    cout << PHWHERE << "DchTrack Node not found" << endl;
    return False;
  }
  //-------------

  // extract the candidate Node
 
  tmpTrackCandiNode = (PHDataNode<PHPointerList<DchTrackCandidate> >*)i.findFirst("PHDataNode","DchTrackCandidate");
  trackCandidateList = tmpTrackCandiNode->getData();
  trackCandidateList->clearAndDestroy();

  tmpHitLineListsNode = (PHDataNode<DchHitLineLists>*)i.findFirst("PHDataNode","DchHitLineLists");   
  hitLineLists = tmpHitLineListsNode->getData();
  hitLineLists->clearAndDestroy();
  
   // extract from the node the DAO -DGO -DCO etc...
  
  dchDaoNode = (PHDataNode<PHDchAddressObject>*)i.findFirst("PHDataNode","DchDAO");
  dchAddressObject = dchDaoNode->getData();
    
  dchDgoNode = (PHDataNode<PHDchGeometryObject>*)i.findFirst("PHDataNode","DchDGO");
  dchGeometryObject = dchDgoNode->getData();
 
  dchDcoNode = (PHDataNode<PHDchCalibrationObject>*)i.findFirst("PHDataNode","DchDCO");
  dchCalibrationObject = dchDcoNode->getData();
  
  dchDnoNode = (PHDataNode<PHDchNoiseObject>*)i.findFirst("PHDataNode","DchDNO"); 
  dchNoiseObject = dchDnoNode->getData();
   
  return  callPAM(nodes);
 
}

PHBoolean mNewDchNoFieldCandidatory::callPAM(PHPointerList<PHNode> &nl)
{
  dDchHitNode_t*      hitNode        = static_cast<dDchHitNode_t*>(nl[0]);
  dDchTracksNode_t*   trackNode      = static_cast<dDchTracksNode_t*>(nl[1]);
  BbcOutNode_t*       bbcNode        = dynamic_cast<BbcOutNode_t*>(nl[2]);
  dPadClusterNode_t*  pc1Node        = static_cast<dPadClusterNode_t*>(nl[3]);
  VtxOutNode_t*       vtxNode        = dynamic_cast<VtxOutNode_t*>(nl[4]);

  BbcOut *bbcout = 0;
  VtxOut *vtxout = 0;

  TABLE_HEAD_ST dDchHit_h;
  DDCHHIT_ST *dDchHit;
  TABLE_HEAD_ST dDchTracks_h;
  DDCHTRACKS_ST *dDchTracks;  

  TABLE_HEAD_ST dPc1Cluster_h;
  DPADCLUSTER_ST *dPc1Cluster;

  dDchHit_h     = hitNode->getData()->TableHeader();
  dDchHit       = hitNode->getData()->TableData();
  dDchTracks_h  = trackNode->getData()->TableHeader();
  dDchTracks    = trackNode->getData()->TableData();
  dPc1Cluster_h = pc1Node->getData()->TableHeader();
  dPc1Cluster   = pc1Node->getData()->TableData();

  bbcout        = bbcNode->getData();
  vtxout        = vtxNode->getData();

  cout << "------------------------------------------------------------------ " << endl;
  cout << " BBC info:  hits on north side:" << bbcout->get_nPmt(Bbc::North) << endl;
  cout << "            hits on south side:" << bbcout->get_nPmt(Bbc::South) << endl;
  cout << "                     time zero:" << bbcout->get_TimeZero()  << endl;
  cout << "                 bbc vertex   :" << bbcout->get_VertexPoint()  << endl;
  cout << "              vtxout vertex   :" << vtxout->get_ZVertex()  << endl;
  cout << "------------------------------------------------------------------ " << endl;
  cout << " PC1 info: number of PC1 clusters: " << dPc1Cluster_h.nok << endl;

  //----------------------------------------------------------
  // initialize the the candidate finder with the hits
  //----------------------------------------------------------
  DchFastCandidateFinder finder(dchGeometryObject,hitLineLists,&dDchHit_h, dDchHit, hitLineTablev1);
  cout << " step 1 " << endl;
  DchHitAssociator associator(dchGeometryObject, hitLineLists,trackCandidateList);
  DchLineTracker fitter(dchGeometryObject, hitLineLists,trackCandidateList);
  
  // 
  // set vertex for candidate search
  //
  cout << " setting vertex" << endl;
  PHPoint tmp(0.,0.,0.);
  // z should be from the VTX class 
  tmp.setZ(vtxout->get_ZVertex());
  finder.setVertex(tmp);
  cout << finder.getVertex() << endl;
  //----------------------------------------------------------
  // setup analysis parameter
  //----------------------------------------------------------

  finder.setReferenceRadius(220.);
  finder.setCellDifferenceCut(2);  // minimum allowed distance between x1 and x2 hits in unit of cells
                                   // also used in UV candidate search incremented by 2 
  finder.setHoughThresholdOnXCell(10);  // minimum hough array height required for local maximum
  finder.setHoughThresholdOnXMask(15);  // threshold on sum in 3x3 mask arround local maximum
  finder.setHoughThresholdOnUVCell(3);  // same for UV
  finder.setHoughThresholdOnUVMask(6);

  finder.setNumberOfAlphaBins(300);   // feature space in x-y plane
  finder.setNumberOfPhiBins(6000);
  finder.setMaxAlpha(0.8);
  finder.setMinAlpha(-0.8);
  finder.setMaxPhi(1.);
  finder.setMinPhi(-0.65);

  finder.setNumberOfBetaBins(60);     // feature space for UV candidate search
  finder.setNumberOfZedBins(200);
  finder.setMaxBeta(2.5);
  finder.setMinBeta(0.6);
  finder.setMaxZed(100.);
  finder.setMinZed(-100.);

  finder.setDeltaBetaVertexCut(0.2); 
  finder.setMinimumNumberOfHits(4);  // minimum number of closest X hits for candiate
  finder.setMinimumNumberOfXHits(2);  // minimum number of closest X hits for candiate
  finder.setMinimumNumberOfUVHits(0);

  finder.setZMax(110.);
  finder.setZMin(-110.);
  finder.setZAvg(10.);

  associator.setMaxXDistance(0.5);   // width of road arround candidate to associate hits
  associator.setMaxUVDistance(5.);

  // check parameters
  cout << "parameters used in FastCandidateFinder" << endl;
 
  cout << " ReferenceRadius        " <<   finder.getReferenceRadius()<< endl;
  cout << " CellDifferenceCut      " <<   finder.getCellDifferenceCut()<< endl;

  cout << " HoughThresholdOnXCell  " <<   finder.getHoughThresholdOnXCell()<< endl;
  cout << " HoughThresholdOnXMask  " <<   finder.getHoughThresholdOnXMask()<< endl;
  cout << " HoughThresholdOnUVCell " <<   finder.getHoughThresholdOnUVCell()<< endl;
  cout << " HoughThresholdOnUVMask " <<   finder.getHoughThresholdOnUVMask()<< endl;

  cout << " NumberOfAlphaBins      " <<   finder.getNumberOfAlphaBins()<< endl;
  cout << " NumberOfPhiBins        " <<   finder.getNumberOfPhiBins()<< endl;
  cout << " MaxAlpha               " <<   finder.getMaxAlpha()<< endl;
  cout << " MinAlpha               " <<   finder.getMinAlpha()<< endl;
  cout << " MaxPhi                 " <<   finder.getMaxPhi()<< endl;
  cout << " MinPhi                 " <<   finder.getMinPhi()<< endl;

  cout << " NumberOfBetaBins       " <<   finder.getNumberOfBetaBins()<< endl;
  cout << " NumberOfZedBins        " <<   finder.getNumberOfZedBins()<< endl;
  cout << " MaxBeta                " <<   finder.getMaxBeta()<< endl;
  cout << " MinBeta                " <<   finder.getMinBeta()<< endl;
  cout << " MaxZed                 " <<   finder.getMaxZed()<< endl;
  cout << " MinZed                 " <<   finder.getMinZed()<< endl;
  cout << " DeltaBetaVertexCut     " <<   finder.getDeltaBetaVertexCut() << endl;
  cout << " MinimumNumberOfHits    " <<   finder.getMinimumNumberOfHits() << endl;
  cout << " MinimumNumberOfXHits   " <<   finder.getMinimumNumberOfXHits() << endl;
  cout << " MinimumNumberOfUVHits  " <<   finder.getMinimumNumberOfUVHits() << endl;

  cout << " ZMax                   " <<   finder.getZMax() << endl;
  cout << " ZMin                   " <<   finder.getZMin() << endl;
  cout << " ZAvg                   " <<   finder.getZAvg() << endl;

  cout << " MaxXDistance           " <<   associator.getMaxXDistance() << endl;
  cout << " MaxUVDistance          " <<   associator.getMaxUVDistance() << endl;
  
  cout << "-------------------------------------" << endl;

  cout << " attache candidate list " << endl;
  finder.attachCandidateList(trackCandidateList);
  
  // use PC1 hits and vertex to define candidates
  cout << " creating track candidates from vertex and PC1 hits" << endl;
  finder.getPC1Candidates(&dPc1Cluster_h, dPc1Cluster);
  cout << "total number of candidates found: " << trackCandidateList->length() << endl;
  cout << " first hit association" << endl;
  associator.associateX1AndX2HitListForCandidates(0);
  cout << " fine association of X hits" << endl;
  associator.associateXHits(0);
  cout << " fitting XY projection " <<endl; 
  fitter.fitLineToXHits();

  cout << " association of UV hits" << endl;
  associator.associateUVHits();
  cout << " purge candidates with to little assigned hits "<< endl;
  finder.purgeCandidateList();
  cout << " clear associations " << endl;
  associator.clearAssociation();
  cout << " re associate X hits" << endl;
  associator.associateXHits(0);
  cout << " re associate UV hits" << endl;
  associator.associateUVHits();

  //copy hitline to hitlinev2
  DchHitLineTable *Tablev2= dynamic_cast < DchHitLineTable * >(hitLineTablev2);
  DchHitLineOutv2 shortHit;
  Tablev2->Clear();
  for (int i =0; i< hitLineTablev1->Entries(); i++) {
    shortHit.setId(hitLineTablev1->getId(i));
    shortHit.setArm(hitLineTablev1->getArm(i));
    shortHit.setPlane(hitLineTablev1->getPlane(i));
    shortHit.setCell(hitLineTablev1->getCell(i));
    shortHit.setSide(hitLineTablev1->getSide(i));
    shortHit.setTime1(hitLineTablev1->getTime1(i));
    shortHit.setIdmirror(hitLineTablev1->getIdmirror(i));
    shortHit.setWidth(hitLineTablev1->getWidth(i));
    shortHit.setXYZ(hitLineTablev1->getXYZ(i));
    Tablev2->AddHit(&shortHit);
  }

  finder.Flatten(1,&dDchTracks_h,dDchTracks , &dDchHit_h, dDchHit);
  finder.fillOutputTables(trackTablev1, hitLineTablev1);
 
  cout << "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% " << endl;
  cout << "Number Of Hits   from Fast Tracker " << dDchHit_h.nok << endl;
  cout << "Number Of Candidates from Fast Tracker " << trackCandidateList->length() << endl;
  cout << "Number Of Tracks from Fast Tracker " << dDchTracks_h.nok << endl;
  cout << "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% " << endl;
  
  trackNode->getData()->SetRowCount(dDchTracks_h.nok);
  return True;   
} 









