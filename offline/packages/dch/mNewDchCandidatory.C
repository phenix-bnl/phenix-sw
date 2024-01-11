#include "DchFastCandidateFinder.hh"
#include "mNewDchCandidatory.hh"
#include "DchLineTracker.hh"
#include "DchHitLineOutv2.hh"
#include "DchHitAssociator.hh"

#include "dDchHitWrapper.h"
#include "dDchTracksWrapper.h"

#include "PHDchHistogrammer.hh"

#include <PHIODataNode.h>
#include <PHTable.hh>
#include <PHIODataNode.h>


#include <VtxOut.h>
#include <dPadClusterWrapper.h>


#include <iostream>

using namespace std;

typedef PHIODataNode<PHObject> PHObjectNode_t;
typedef PHIODataNode<dDchHitWrapper> dDchHitNode_t;
typedef PHIODataNode<dDchTracksWrapper> dDchTracksNode_t;
typedef PHIODataNode<dDchTracksExtWrapper> dDchTracksExtNode_t;
typedef PHIODataNode<dPadClusterWrapper> dPadClusterNode_t;

typedef PHIODataNode<PHTable> TableNode_t;
typedef PHIODataNode<VtxOut> VtxOutNode_t;
typedef PHIODataNode<DchTrack> DchTrackNode_t;

mNewDchCandidatory::mNewDchCandidatory(int option)
{
  // The constants kwhatever are set to make code using the options
  // more readable.  For example, you can do this: blah = new
  // mNewDchCandidatory(kx1analysis|kpc1analysis|kverbose); much
  // easier to read than a bitmap :) TKH -- 10-19-2001
  // default is mNewDchCandidatory(kx1analysis|kx2analysis|kx1x2analysis|kuvvertexanalysis|kpc1analysis|kfitxanalysis|kvertexanalysis)

  x1ExclusiveAnalysis = (option & kx1ExclusiveAnalysis );
  x2ExclusiveAnalysis = (option & kx2ExclusiveAnalysis );
  x1OddExclusiveAnalysis = (option & kx1OddExclusiveAnalysis );
  x2OddExclusiveAnalysis = (option & kx2OddExclusiveAnalysis );
  x1EvenExclusiveAnalysis = (option & kx1EvenExclusiveAnalysis );
  x2EvenExclusiveAnalysis = (option & kx2EvenExclusiveAnalysis );
  x1analysis = (option & kx1analysis );
  x2analysis = (option & kx2analysis );
  x1x2analysis = (option & kx1x2analysis );
  uvanalysis = (option & kuvanalysis );
  pc13danalysis = (option & kpc13danalysis );
  pc1analysis = (option & kpc1analysis );
  fitxanalysis = (option & kfitxanalysis );
  fit3danalysis = (option & kfit3danalysis );
  vertexanalysis = (option & kvertexanalysis );
  uvvertexanalysis = (option & kuvvertexanalysis);
  uvpc1analysis = (option & kuvpc1analysis );
  // kluge
  verbose = 1; 

  X1CandidateList = new PHPointerList<DchTrackCandidate>(100);
  X2CandidateList = new PHPointerList<DchTrackCandidate>(100);
  X1X2CandidateList = new PHPointerList<DchTrackCandidate>(100);
  trackCandidateList = 0;
  hitLineLists = 0;
  pc1HitLists = 0;

  internalEventCounter = 0;
  movehitstoline = 1;
  histogrammer = 0;
  fillAligment = 0;
  hitLineTablev1 = 0;
  hitLineTablev2 = 0;
  trackTablev1 = 0;
  dchTracksExt = 0;
}

mNewDchCandidatory::mNewDchCandidatory()
{
  verbose = 1;
  x1analysis = 1;
  x2analysis = 1;
  x1x2analysis = 1;
  uvanalysis = 0;
  uvvertexanalysis = 1;
  uvpc1analysis = 0;
  pc13danalysis = 0;
  pc1analysis = 1;
  fitxanalysis = 1;
  fit3danalysis = 0; // need to be by default 0
  vertexanalysis = 1;

  x1ExclusiveAnalysis=0;    
  x2ExclusiveAnalysis=0;    
  x1OddExclusiveAnalysis=0; 
  x2OddExclusiveAnalysis=0; 
  x1EvenExclusiveAnalysis=0;
  x2EvenExclusiveAnalysis=0;

  internalEventCounter = 0;

  X1CandidateList = new PHPointerList<DchTrackCandidate>(100);
  X2CandidateList = new PHPointerList<DchTrackCandidate>(100);
  X1X2CandidateList = new PHPointerList<DchTrackCandidate>(100);
  trackCandidateList = 0;
  hitLineLists = 0;
  pc1HitLists = 0;
  movehitstoline = 1;
  histogrammer = 0;
  fillAligment = 0;
  hitLineTablev1 = 0;
  hitLineTablev2 = 0;
  trackTablev1 = 0;
  dchTracksExt = 0;
}

void
mNewDchCandidatory::clearAnalysisOptions()
{
  verbose = 1;
  x1analysis = 0;
  x2analysis = 0;
  x1x2analysis = 0;
  uvanalysis = 0;
  uvvertexanalysis = 0;
  uvpc1analysis = 0;
  pc13danalysis = 0;
  pc1analysis = 0;
  fitxanalysis = 0;
  fit3danalysis = 0;
  vertexanalysis = 0;

  x1ExclusiveAnalysis=0;    
  x2ExclusiveAnalysis=0;    
  x1OddExclusiveAnalysis=0; 
  x2OddExclusiveAnalysis=0; 
  x1EvenExclusiveAnalysis=0;
  x2EvenExclusiveAnalysis=0;

}

mNewDchCandidatory::~mNewDchCandidatory()
{
  X1CandidateList->clearAndDestroy();
  X2CandidateList->clearAndDestroy();
  X1X2CandidateList->clearAndDestroy();
  
  delete X1CandidateList;
  delete X2CandidateList;
  delete X1X2CandidateList;
}

PHBoolean
mNewDchCandidatory::event(PHCompositeNode *root)
{
  internalEventCounter++;

  topNode = root;
  PHObjectNode_t *phob;
  PHPointerList<PHNode> nodes;
  PHNodeIterator i(root);
  PHNode *n;

  PHDataNode<PHDchHistogrammer>* dchHistoNode;
  PHDataNode<PHDchGeometryObject>* dchDgoNode;
  PHDataNode<PHPointerList<DchTrackCandidate> >* tmpTrackCandiNode;
  PHDataNode<DchHitLineLists>* tmpHitLineListsNode;
  PHDataNode<DchPc1HitLists>* tmpPc1HitListsNode;

  nodes.clear();

  // Insert code here to navigate node hierarchy and find
  // or create specific nodes to pass to physics module...

  n = i.findFirst("PHIODataNode", "dDchHit");
  nodes.append(n);
  n = i.findFirst("PHIODataNode", "dDchTracks");
  nodes.append(n);
  n = i.findFirst("PHIODataNode", "dPc1Cluster");
  nodes.append(n);
  dDchTracksExtNode_t *trackExtNode;
  trackExtNode = (dDchTracksExtNode_t*)i.findFirst("PHIODataNode",
                 "dDchTracksExt");
  if (trackExtNode)
    {
      dchTracksExt = (dDchTracksExtWrapper*)trackExtNode->getData();
    }

  phob = static_cast < PHObjectNode_t * >(i.findFirst ("PHIODataNode",
                                          "DchHitLineTablev1"));
  hitLineTablev1 = dynamic_cast < DchHitLineTable * >(phob->getData ());
  if (!hitLineTablev1)
    {
      cout << PHWHERE
      << "DchHitLineTablev1 not found " << endl;
      return False;
    }

  PHTypedNodeIterator <DchHitLineTable> iter1(root);
  PHIODataNode <DchHitLineTable> * node1 = iter1.find("DchHitLineTable");
  if (node1)
    {
      hitLineTablev2 = node1->getData();
    }
  else
    {
      cout << PHWHERE << "DchHitLineTable Node not found " << endl;
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

  // extract the candidate Node
   tmpTrackCandiNode = (PHDataNode<PHPointerList<DchTrackCandidate> >*)i.findFirst("PHDataNode", "DchTrackCandidate");
   trackCandidateList = tmpTrackCandiNode->getData();

   tmpHitLineListsNode = (PHDataNode<DchHitLineLists>*)i.findFirst("PHDataNode", "DchHitLineLists");
   hitLineLists = tmpHitLineListsNode->getData();

   tmpPc1HitListsNode = (PHDataNode<DchPc1HitLists>*)i.findFirst("PHDataNode", "DchPc1HitLists");
   pc1HitLists = tmpPc1HitListsNode->getData();


  dchHistoNode = (PHDataNode<PHDchHistogrammer>*)i.findFirst("PHDataNode",
							     "DchHisto");

  histogrammer = dchHistoNode->getData();

  dchDgoNode = (PHDataNode<PHDchGeometryObject>*)i.findFirst("PHDataNode",
							     "DchDGO");
  dchGeometryObject = dchDgoNode->getData();

  return callPAM(nodes);
}

int
mNewDchCandidatory::ResetEvent(PHCompositeNode *topNode)
{
  PHNodeIterator i(topNode);
  PHDataNode<PHPointerList<DchTrackCandidate> >* tmpTrackCandiNode = (PHDataNode<PHPointerList<DchTrackCandidate> >*)i.findFirst("PHDataNode", "DchTrackCandidate");
  trackCandidateList = tmpTrackCandiNode->getData();
  trackCandidateList->clearAndDestroy();

  X1CandidateList->clearAndDestroy();
  X2CandidateList->clearAndDestroy();
  X1X2CandidateList->clearAndDestroy();

  PHDataNode<DchHitLineLists>* tmpHitLineListsNode = (PHDataNode<DchHitLineLists>*)i.findFirst("PHDataNode", "DchHitLineLists");
  hitLineLists = tmpHitLineListsNode->getData();
  hitLineLists->clearAndDestroy();
  PHDataNode<DchPc1HitLists>* tmpPc1HitListsNode = (PHDataNode<DchPc1HitLists>*)i.findFirst("PHDataNode", "DchPc1HitLists");
  pc1HitLists = tmpPc1HitListsNode->getData();
  pc1HitLists->clearAndDestroy();

  return 0;
}

float
mNewDchCandidatory::getVertex()
{
  VtxOut *vtxout = 0;
  PHTypedNodeIterator<VtxOut> vtxiter(topNode);
  VtxOutNode_t *VtxOutNode = vtxiter.find("VtxOut");
  if (VtxOutNode)
    {
      vtxout = VtxOutNode->getData();
    }
  else
    {
      PHMessage("mNewDchCandidatory::getVertex", PHWarning,
                "VtxOut table not found ");
      return -9999;
    }

  if (vtxout->isValid())
    {
      vertexPoint = vtxout->get_ZVertex();
    }
  else
    {
      vertexPoint = -9999;
    }

  return vertexPoint;
}

PHBoolean
mNewDchCandidatory::getPc1Hits()
{
  PHNodeIterator nodeIter(topNode);
  PHIODataNode<TObject> *PC1Node = (PHIODataNode<TObject>*)nodeIter.findFirst("PHIODataNode", "dPc1Cluster");
  if (!PC1Node)
    {
      PHMessage("mNewDchCandidatory::getPc1Hits", PHWarning, 
		"dPc1Cluster table not found ");
      return False;
    }
  dPadClusterWrapper* pc1Wrapper = (dPadClusterWrapper*)(PC1Node->getData());

  short id, arm, side, sector;
  DchPc1Hit *pc1hit;
  PHPoint pc1;

  for (unsigned int i = 0; i < pc1Wrapper->RowCount(); i++)
    {
      id = pc1Wrapper->get_id(i);
      arm = pc1Wrapper->get_arm(i);
      sector = pc1Wrapper->get_sector(i);
      pc1.setX( pc1Wrapper->get_xyz(0, i));
      pc1.setY( pc1Wrapper->get_xyz(1, i));
      pc1.setZ( pc1Wrapper->get_xyz(2, i));
      if (pc1.getZ() < 0.0)
        {
          side = SOUTH;
        }
      else
        {
          side = NORTH;
        }
      if (pc1.getX() < 0.0)
        {
          arm = EAST;
        }
      else
        {
          arm = WEST;
        }
      pc1hit = new DchPc1Hit(id, arm, side, sector, pc1);
      pc1HitLists->append(pc1hit);
    }
  
  return True;
}

PHBoolean
mNewDchCandidatory::callPAM(PHPointerList<PHNode> &nl)
{
  dDchHitNode_t* hitNode = static_cast<dDchHitNode_t*>(nl[0]);
  dDchTracksNode_t* trackNode = static_cast<dDchTracksNode_t*>(nl[1]);
  
  TABLE_HEAD_ST dDchHit_h;
  DDCHHIT_ST *dDchHit;
  TABLE_HEAD_ST dDchTracks_h;
  DDCHTRACKS_ST *dDchTracks;
  
  dDchHit_h = hitNode->getData()->TableHeader();
  dDchHit = hitNode->getData()->TableData();
  dDchTracks_h = trackNode->getData()->TableHeader();
  dDchTracks = trackNode->getData()->TableData();
  
  //----------------------------------------------------------
  // initialize the the candidate finder with the hits
  //----------------------------------------------------------
  
  DchFastCandidateFinder finder(dchGeometryObject, hitLineLists, &dDchHit_h, dDchHit, hitLineTablev1);
  finder.setVerbose(verbose);
  if(finder.hasTooManyHits())
    {
      return False;  // numberOfHits>32768
    }
  DchLineTracker fitterX1X2(dchGeometryObject, hitLineLists, X1X2CandidateList);
  fitterX1X2.setVerbose(verbose);
  DchLineTracker fitterX1(dchGeometryObject, hitLineLists, X1CandidateList);
  fitterX1.setVerbose(verbose);
  DchLineTracker fitterX2(dchGeometryObject, hitLineLists, X2CandidateList);
  fitterX2.setVerbose(verbose);
  DchLineTracker fitter(dchGeometryObject, hitLineLists, trackCandidateList);
  fitter.setVerbose(verbose);
  DchHitAssociator associator(dchGeometryObject, hitLineLists, trackCandidateList);
  associator.setVerbose(verbose);
  
  // Set vertex for candidate search
  PHPoint tmp(0.0, 0.0, 0.0);
  PHVector tmpv(0.0, 0.0, 1.0);
  
  if (vertexanalysis)
    {                      
      tmp.setZ(getVertex());
      if (tmp.getZ() < -900)
        {
#ifdef DEBUG
	  cout << " WARNING mNewDchCandidatory vertex not found " << endl;
#endif
        }
    }
  
  finder.setVertex(tmp);
  finder.setBeamAxis(tmpv);
  associator.setVertex(tmp);
  
  if (pc1analysis)
    {
      Pc1Analysis(& associator, & finder);
    }
  
  if (x1x2analysis)
    {
      X1X2Analysis(& associator, & finder, & fitterX1X2);
    }
  if (x1analysis)
    {
      X1Analysis(& associator, & finder, & fitterX1);
    }
  if (x2analysis)
    {
      X2Analysis(& associator, & finder, & fitterX2);
    }
  if ((!x1ExclusiveAnalysis)||(!x2ExclusiveAnalysis))
    {
      
      FreeX1X2TrackHitsForFurtherAnalysis(& associator);
    }
  
  MergeAllCandidateLists();
  
  AssociateFreeXHitsToTrackCandidates(& associator, &finder);
    
  // UV association begins here
  
  finder.setReferenceRadius(220.0);
  associator.setMaxUVDistance(10.0);  //TKH agreement is Z = 10 cm
  
  
  if (uvanalysis || uvpc1analysis || uvvertexanalysis)
    {
      PrepareUVPC1Analysis(& associator, & finder, &fitter);
    }
  
  if (uvanalysis)
    {
      UVAnalysis(& associator, &finder);
    }

  if (uvpc1analysis)
    {
      UVPC1Analysis(& associator, &finder, &fitter);
    }
  
  if (uvvertexanalysis)
    {
      //
      // reconstruct uv projection from vertex + uv hits
      //
      setDefaultParameters(& finder, & associator, & fitter);
      finder.setHoughThresholdOnUVCell(3);   // required if mached with PC hit
      finder.setHoughThresholdOnUVMask(5);
      finder.setHoughThresholdOnUVMaskWithPc1Hit(3);
      finder.setNumberOfZedBins(200);
      finder.getVertexUVCandidates();
      associator.associateUVHits();

#ifdef DEBUG

      if (verbose > 4)
        {
          int counter = 0;
          for (unsigned int i = 0; i < trackCandidateList->length(); i++)
            {
              if ((*trackCandidateList)[i]->getQuality() > 4)
                counter++;
            }
          cout << counter << " found with UV information "
          << float(counter) / float(trackCandidateList->length()) << endl;
        }
#endif

    }

 
  if (fitxanalysis)
    {
      associator.moveHitsToCandidates(&dDchHit_h, dDchHit , hitLineLists, hitLineTablev1 );
      FitXAnalysis(& associator,& finder, &fitter );
    }

  if (fit3danalysis)
    {
      Fit3DAnalysis(& associator,& finder, & fitter);
    }

  finder.calculateClosestApproachToBeamAxis();
  
  
  

  if (fillAligment)
    {
      if (histogrammer && histogrammer->flagInit)
        {
          finder.attachHistogrammer(histogrammer);
          finder.fillAligmentNtuple();
        }
    }
  
  // last step
  // write candidates to track table
  //copy hitline to hitlinev2;
  DchHitLineTable *Tablev2 = dynamic_cast < DchHitLineTable * >(hitLineTablev2);
  DchHitLineOutv2 shortHit;
  Tablev2->Clear();
  for (int i = 0; i < hitLineTablev1->Entries(); i++)
    {
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
  //set pointer, used by flatten function
  finder.setDchTracksExtTable(dchTracksExt);

  finder.Flatten(1, &dDchTracks_h, dDchTracks , &dDchHit_h, dDchHit);
  if (finder.hasTooManyTracks())
    {
      return False; // number of tracks larger than max dchTracksExt length
    }
  finder.fillOutputTables(trackTablev1, hitLineTablev1);


  trackNode->getData()->SetRowCount(dDchTracks_h.nok);
  hitNode->getData()->SetRowCount(dDchHit_h.nok);
  
  return True;
}

void
mNewDchCandidatory::Fit3DAnalysis(DchHitAssociator * associator,DchFastCandidateFinder * finder, DchLineTracker * fitter)
{
  DchTrackCandidate* mycandidate;

  //
  // fit simultaneously x and uv wires with straight line
  //
  fitter->fitLineToAllHits();
  for (unsigned int i = 0; i < trackCandidateList->length(); i++)
    {
      mycandidate = (*trackCandidateList)[i];
      finder->transformCandidateFromLocalToGlobal(mycandidate);
    }
  
  //may be irrelevant
  associator->clearAssociation();
  associator->setMaxXDistance(0.2);
  associator->setMaxUVDistance(10.);
  associator->associateXHits(0);
  associator->associateUVHits();
  fitter->fitLineToAllHits();
  fitter->removeDuplicateCandidates();
  for (unsigned int i = 0; i < trackCandidateList->length(); i++)
    {
      mycandidate = (*trackCandidateList)[i];
      finder->transformCandidateFromLocalToGlobal(mycandidate);
    }
}


void
mNewDchCandidatory::FitXAnalysis(DchHitAssociator * associator,DchFastCandidateFinder * finder, DchLineTracker * fitter)
{
  DchTrackCandidate* mycandidate;

 //
  // move 3rd coordinate of hitline to point of
  // closest approach to candidate
  //
  // Hi gang...
  //    There is a philisophical impasse here.  When we want to
  //  make a publicity plot, each hit should be moved along its
  //  line to the best place w.r.t. the fit.
  //    When we want to debug the code (specifically stereo) we
  //  instead want the point at the intersection of the "X-plane"
  //  since this is the info needed.  I have changed the associator
  //  so that one chooses between these options (the latter is default).
  //                         TKH 12-6-2001

  // Now that the precision alignment of the X wires shows that the
  // wires can be tilted in Z (not perfectly parallel to beam axis)
  // it is NECESSARY to move the hits and then recalculate the final angles.
  //                         TKH 1-22-2002

  fitter->fitLineToXHits();
  for (unsigned int i = 0; i < trackCandidateList->length(); i++)
    {
      mycandidate = (*trackCandidateList)[i];
      finder->transformCandidateFromLocalToGlobal(mycandidate);
    }
  
}

void
mNewDchCandidatory::Pc1Analysis(DchHitAssociator * associator,DchFastCandidateFinder * finder)
{
  getPc1Hits();
  associator->attachPc1HitLists(pc1HitLists); // attach new candidate list
  finder->attachPc1HitLists(pc1HitLists); // attach new candidate list
}

void
mNewDchCandidatory::PrepareUVPC1Analysis(DchHitAssociator * associator,DchFastCandidateFinder * finder, DchLineTracker * fitter)
{
  //
  // common for all UV analyses
  //
  setDefaultParameters(finder, associator, fitter);
  associator->associateUVHitsWithUVPlanes();
  if (pc1analysis)
    {
      associator->associatePc1Hits();
    }

}

void
mNewDchCandidatory::UVPC1Analysis(DchHitAssociator * associator,DchFastCandidateFinder * finder,DchLineTracker * fitter)
{
  //
  // 2nd hough transform to find uv projection of tracks
  // using pc1 hits and uv1+2 hits
  //
  setDefaultParameters(finder, associator, fitter);
  finder->setHoughThresholdOnUVCell(1);
  finder->setHoughThresholdOnUVMaskWithPc1Hit(3);
  finder->getPc1UVCandidates();
  associator->associateUVHits();
  
#ifdef DEBUG
  
  if (verbose > 9)
        {
          long counter = 0;
          for (unsigned int i = 0; i < trackCandidateList->length(); i++)
            {
              if ((*trackCandidateList)[i]->getQuality() > 4)
                counter++;
            }
          cout << counter << " found with UV information "
	       << float(counter) / float(trackCandidateList->length()) << endl;
        }
#endif

}

void
mNewDchCandidatory::UVAnalysis(DchHitAssociator * associator,DchFastCandidateFinder * finder)
{
 
  // 2nd Hough transform to find uv projection of tracks
      
      for (int arm = 0; arm < numberOfArms; arm++)
        {
          for (int side = 0; side < numberOfSides; side++)
            {
              finder->getUVCandidates(arm, side);
            }
        }
      associator->associateUVHits();

      
#ifdef DEBUG
      if (verbose > 9)
        {
          long counter = 0;
          for (unsigned int i = 0; i < trackCandidateList->length(); i++)
            {
              if ((*trackCandidateList)[i]->getQuality() > 4)
                counter++;
            }
          cout << counter << " found with UV information "
	       << float(counter) / float(trackCandidateList->length()) << endl;
        }
#endif
      

}


void
mNewDchCandidatory::FreeX1X2TrackHitsForFurtherAnalysis(DchHitAssociator * associator)
{

  associator->attachCandidateList(X1X2CandidateList);   // set associator candidate list to X1X2 list
  associator->markAssociatedHitsAsUnused();
  
  associator->clearAssociation();

}

void
mNewDchCandidatory::AssociateFreeXHitsToTrackCandidates(DchHitAssociator * associator,DchFastCandidateFinder * finder)
{

  finder->attachCandidateList(trackCandidateList);
  // attach new candidate list
  associator->attachCandidateList(trackCandidateList);   

  associator->associateX1AndX2HitListForCandidates(0);
  associator->setMaxXDistance(0.2);   // .2
  
  associator->associateXHits(0);
  finder->setMinimumNumberOfHits(5);      // number of hits per candidate
  finder->setMinimumNumberOfXHits(5);
  finder->purgeCandidateList();
  associator->clearAssociation();

  associator->associateXHits(0);


}

void
mNewDchCandidatory::MergeAllCandidateLists()
{
  DchTrackCandidate* mycandidate;
  for (unsigned int i = 0; i < X1X2CandidateList->length(); i++)
    {
      mycandidate = (*X1X2CandidateList)[i];
      trackCandidateList->append(mycandidate);
    }
  for (unsigned int i = 0; i < X1CandidateList->length(); i++)
    {
      mycandidate = (*X1CandidateList)[i];
      trackCandidateList->append(mycandidate);
    }
  for (unsigned int i = 0; i < X2CandidateList->length(); i++)
    {
      mycandidate = (*X2CandidateList)[i];
      trackCandidateList->append(mycandidate);
    }
  X1CandidateList->clear();
  X2CandidateList->clear();
  X1X2CandidateList->clear();


}

void
mNewDchCandidatory::X2Analysis(DchHitAssociator * associator,DchFastCandidateFinder * finder,  DchLineTracker * fitter)
{
  DchTrackCandidate* mycandidate;
  // find candidates in X1 only
  finder->attachCandidateList(X2CandidateList);
  //
  // changes from default parameter values
  setDefaultParameters(finder, associator, fitter);
  // minimum hough array height required for local maximum
  finder->setHoughThresholdOnXCell(4);    
  // threshold on sum in 3x3 mask arround local maximum
  finder->setHoughThresholdOnXMask(8);    
  // feature space in x-y plane
  finder->setNumberOfAlphaBins(30);      
  finder->setNumberOfPhiBins(1500);
  finder->setReferenceRadius(229.15);
  
  for (int arm = 0; arm < numberOfArms; arm++)
    {
      for (int side = 0; side < numberOfSides; side++)
	{
	  finder->getX2Candidates(arm, side);
	}
    }
  associator->attachCandidateList(X2CandidateList);   // attach X1 candidate list
  associator->clearAssociation();
  associator->associateX1AndX2HitListForCandidates(2);
  associator->setMaxXDistance(0.4);
  associator->associateXHits(2, 1);
  
  if (fitxanalysis)
    {
      fitter->fitLineToXHits();
      for (unsigned int i = 0; i < X2CandidateList->length(); i++)
	{
	  mycandidate = (*X2CandidateList)[i];
	  finder->transformCandidateFromLocalToGlobal(mycandidate);
	}
      fitter->removeDuplicateCandidates();
    }
  
  associator->clearAssociation();
  
  associator->setMaxXDistance(0.2);
  
  associator->associateXHits(2);
  // number of hits per candidate
  finder->setMinimumNumberOfHits(1);      
  finder->setMinimumNumberOfXHits(1);
  finder->purgeCandidateList();
  associator->clearAssociation();
  
  associator->associateXHits(2);
  finder->setMinimumNumberOfHits(2);      // number of hits per candidate
  finder->setMinimumNumberOfXHits(2);
  finder->purgeCandidateList();
  associator->clearAssociation();
  
  associator->associateXHits(2);
  finder->setMinimumNumberOfHits(3);      // number of hits per candidate
  finder->setMinimumNumberOfXHits(3);
  finder->purgeCandidateList();
  associator->clearAssociation();
  
  associator->associateXHits(2);
  finder->setMinimumNumberOfHits(4);      // number of hits per candidate
  finder->setMinimumNumberOfXHits(4);
  finder->purgeCandidateList();
  associator->clearAssociation();
  
  finder->setQuality(2);

}

void
mNewDchCandidatory::X1Analysis(DchHitAssociator * associator,DchFastCandidateFinder * finder, DchLineTracker * fitter)
{
  DchTrackCandidate* mycandidate;
  // find candidates in X1 only
  finder->attachCandidateList(X1CandidateList);
  //
  // changes from default parameter values
  setDefaultParameters(finder, associator, fitter);
  // minimum hough array height required for local maximum
  finder->setHoughThresholdOnXCell(4);    
  // threshold on sum in 3x3 mask arround local maximum
  finder->setHoughThresholdOnXMask(8);    
  // feature space in x-y plane
  finder->setNumberOfAlphaBins(30);      
  finder->setNumberOfPhiBins(1500);
  finder->setReferenceRadius(207.85);
  
  for (int arm = 0; arm < numberOfArms; arm++)
    {
      for (int side = 0; side < numberOfSides; side++)
	{
	  finder->getX1Candidates(arm, side);
	}
    }
  // attach X1 candidate list
  associator->attachCandidateList(X1CandidateList);   
  associator->clearAssociation();
  associator->associateX1AndX2HitListForCandidates(1);
  associator->setMaxXDistance(0.4);
  associator->associateXHits(1, 1);
  
  if (fitxanalysis)
    {
      fitter->fitLineToXHits();
      for (unsigned int i = 0; i < X1CandidateList->length(); i++)
	{
	  mycandidate = (*X1CandidateList)[i];
	  finder->transformCandidateFromLocalToGlobal(mycandidate);
	}
      fitter->removeDuplicateCandidates();
    }
  associator->clearAssociation();
  
  associator->setMaxXDistance(0.2);
  
  associator->associateXHits(1);
  finder->setMinimumNumberOfHits(1);      // number of hits per candidate
  finder->setMinimumNumberOfXHits(1);
  finder->purgeCandidateList();
  associator->clearAssociation();
  
  associator->associateXHits(1);
  finder->setMinimumNumberOfHits(2);      // number of hits per candidate
  finder->setMinimumNumberOfXHits(2);
  finder->purgeCandidateList();
  associator->clearAssociation();
  
  associator->associateXHits(1);
  finder->setMinimumNumberOfHits(3);      // number of hits per candidate
  finder->setMinimumNumberOfXHits(3);
  finder->purgeCandidateList();
  associator->clearAssociation();
  
  associator->associateXHits(1);
  finder->setMinimumNumberOfHits(4);      // number of hits per candidate
  finder->setMinimumNumberOfXHits(4);
  finder->purgeCandidateList();
  associator->clearAssociation();
  
  finder->setQuality(1);

}

void
mNewDchCandidatory::X1X2Analysis(DchHitAssociator * associator,DchFastCandidateFinder * finder, DchLineTracker * fitter)
{
  // in the first step find x1-x2 candidates uses default parameter
  // values
  DchTrackCandidate* mycandidate;
  setDefaultParameters(finder, associator, fitter);
  finder->setMinimumNumberOfHits(8);      // number of hits per candidate
  finder->setMinimumNumberOfXHits(8);
  
  // attach X1-X2 candidate list
  finder->attachCandidateList(X1X2CandidateList);
  for (int arm = 0; arm < numberOfArms; arm++)
    {
      // loop over arm-side combinations
      for (int side = 0; side < numberOfSides; side++)
	{
	  finder->getXCandidates(arm, side);
	}
    }
  // associate hits within +/- 1 cell
  
  // attach X1-X2 candidate list
  associator->attachCandidateList(X1X2CandidateList);
  
  associator->associateX1AndX2HitListForCandidates(0);
  associator->setMaxXDistance(0.4);
  //  associate hits in MaxXDistance road
  associator->associateXHits(0, 1);            
  
  if (fitxanalysis)
    {
      fitter->fitLineToXHits();
      for (unsigned int i = 0; i < X1X2CandidateList->length(); i++)
	{
	  mycandidate = (*X1X2CandidateList)[i];
	  finder->transformCandidateFromLocalToGlobal(mycandidate);
	}
      fitter->removeDuplicateCandidates();
    }
  associator->clearAssociation();
  
  associator->setMaxXDistance(0.4);
  
  associator->associateXHits(0);
  finder->setMinimumNumberOfHits(4);      // number of hits per candidate
  finder->setMinimumNumberOfXHits(4);
  finder->purgeCandidateList();
  associator->clearAssociation();
  
  associator->associateXHits(0);
  finder->setMinimumNumberOfHits(6);      // number of hits per candidate
  finder->setMinimumNumberOfXHits(6);
  finder->purgeCandidateList();
  associator->clearAssociation();
  
  associator->setMaxXDistance(0.2);
  
  associator->associateXHits(0);
  finder->setMinimumNumberOfHits(8);      // number of hits per candidate
  finder->setMinimumNumberOfXHits(8);
  finder->purgeCandidateList();
  associator->clearAssociation();
  
  associator->markAssociatedHitsAsUsed();
  
  finder->setQuality(3);
}

void 
mNewDchCandidatory::setDefaultParameters(DchFastCandidateFinder * finder, 
					 DchHitAssociator * associator,
					 DchLineTracker  * fitter)
{
  // setup default analysis parameters
  finder->setReferenceRadius(220.);       // radius in local coordinates 
                                         // used to define phi, alpha, beta and zed
  finder->setCellDifferenceCut(4);        // minimum allowed distance between x1 and x2 hits in unit of cells
                                         // also used in UV candidate search incremented by 2 
  finder->setHoughThresholdOnXCell(8);   // minimum hough array height required for local maximum
  finder->setHoughThresholdOnXMask(15);   // threshold on sum in 3x3 mask arround local maximum
  finder->setHoughThresholdOnUVCell(3);   // same for UV 
  finder->setHoughThresholdOnUVMask(5);
  finder->setHoughThresholdOnUVMaskWithPc1Hit(3);

  finder->setNumberOfAlphaBins(150);      // feature space in x-y plane
  finder->setNumberOfPhiBins(3000);
  finder->setMaxAlpha(0.93);
  finder->setMinAlpha(-0.93);
  finder->setMaxPhi(1.);
  finder->setMinPhi(-0.65);

  finder->setNumberOfBetaBins(60);        // feature space for UV candidate search
  finder->setNumberOfZedBins(180);        // sigma(zed) ~ sqrt(2) sigma(hit)/sin(6) ~ 3 mm
  finder->setMaxBeta(2.5);                // sigma(beta) ~ sqrt(2) sigma(hit)/sin(6)*0.3 ~ 10 mrad 
  finder->setMinBeta(0.6);                // zed  range ~ 1800 mm   -> 600  1 sigma bins
  finder->setMaxZed(91.);                 // beta range ~ 3100 mrad -> 300  1 sigma bis
  finder->setMinZed(-91.);                // ratio of #bins should be 2/1

  finder->setDeltaBetaVertexCut(0.2);     // cut on correlation between beta and zed
  finder->setMinimumNumberOfHits(6);      // minimum number of closest X hits for candiate
  finder->setMinimumNumberOfXHits(6);     // minimum number of closest X hits for candiate
  finder->setMinimumNumberOfUVHits(0);

  finder->setZMax(100.);               // z region used to search for UV intesections 
  finder->setZMin(-100.);              // with alpha plane
  finder->setZAvg(10.);
  finder->setMaxUVDistance(10.);   // TKH agreement in Z = 10 cm.

  // associator
  associator->setCellDifferenceCut(4);
  associator->setReferenceRadius(finder->getReferenceRadius());
  associator->setZMax(finder->getZMax());
  associator->setZMin(finder->getZMin());
  associator->setZAvg(finder->getZAvg());

  // width of road arround candidate to associate hits
  associator->setMaxXDistance(0.5);    
  // TKH agreement in Z = 10 cm.
  associator->setMaxUVDistance(10.);   
  // distance of PC1 hit to UV-plane to associate it
  associator->setMaxPc1Distance(2.);   

  // fitter
  fitter->setDeltaAlpha(.001);
  fitter->setDeltaPhi(.001);
}

void
mNewDchCandidatory::printParameters (DchFastCandidateFinder* finder, 
				     DchHitAssociator* associator,
				     DchLineTracker  * fitter)
{
  // check parameters
  cout << "parameters used in FastCandidateFinder"
       << "\n ReferenceRadius        " <<   finder->getReferenceRadius()
       << "\n CellDifferenceCut      " <<   finder->getCellDifferenceCut()
       << "\n HoughThresholdOnXCell  " <<   finder->getHoughThresholdOnXCell()
       << "\n HoughThresholdOnXMask  " <<   finder->getHoughThresholdOnXMask()
       << "\n HoughThresholdOnUVCell " <<   finder->getHoughThresholdOnUVCell()
       << "\n HoughThresholdOnUVMask " <<   finder->getHoughThresholdOnUVMask()
       << "\n NumberOfAlphaBins      " <<   finder->getNumberOfAlphaBins()
       << "\n NumberOfPhiBins        " <<   finder->getNumberOfPhiBins()
       << "\n MaxAlpha               " <<   finder->getMaxAlpha()
       << "\n MinAlpha               " <<   finder->getMinAlpha()
       << "\n MaxPhi                 " <<   finder->getMaxPhi()
       << "\n MinPhi                 " <<   finder->getMinPhi()
       << "\n NumberOfBetaBins       " <<   finder->getNumberOfBetaBins()
       << "\n NumberOfZedBins        " <<   finder->getNumberOfZedBins()
       << "\n MaxBeta                " <<   finder->getMaxBeta()
       << "\n MinBeta                " <<   finder->getMinBeta()
       << "\n MaxZed                 " <<   finder->getMaxZed()
       << "\n MinZed                 " <<   finder->getMinZed()
       << "\n DeltaBetaVertexCut     " <<   finder->getDeltaBetaVertexCut() 
       << "\n MinimumNumberOfHits    " <<   finder->getMinimumNumberOfHits() 
       << "\n MinimumNumberOfXHits   " <<   finder->getMinimumNumberOfXHits() 
       << "\n MinimumNumberOfUVHits  " <<   finder->getMinimumNumberOfUVHits() 
       << "\n ZMax                   " <<   finder->getZMax() 
       << "\n ZMin                   " <<   finder->getZMin() 
       << "\n ZAvg                   " <<   finder->getZAvg() 
       << "\n MaxXDistance           " <<   associator->getMaxXDistance() 
       << "\n MaxUVDistance          " <<   associator->getMaxUVDistance() 
       << endl;
}














