// Class: cglHitAssociate (implementation)
// Author/Contact: Jeffery T. Mitchell (mitchell@bnl.gov)
// Purpose: Class that associates central arm detector hits
//          to a drift chamber track.
//
// Added background calculation Nov. 2001 SL
// Added output to class CglTrack  - April 2002, J.Velkovska
//
//  Changed to EmcClusterLocalExt, dropping obsolete (and no longer filled)
//  Wrapped STAF Tables.
//                                    TKH 2-26-2003
//
// Adapted to brand new EMC Clusters object LA 2-27-2003.
//
// Devolved to use semi-obsolete STAF tables when latest EMC
// objects not found.
//                                    TKH 2-28-2003
//

#include "cglHitAssociate.hh"
#include "cglHitList.hh"
#include "PHTrackOut.h"
#include "PHSnglTrack.h"
#include "CglTrack.h"
#include "CglSnglTrack.h"


#include "PHDchTrack.hh"
#include "PHLineTrack.hh"

#include <dPadClusterWrapper.h>
#include <dDchHitWrapper.h>
#include <dDchTracksWrapper.h>
#include <TecOutV1.hh>
#include <AccRaw.h>
#include <AccGeometry.h>
#include <TofwHit.h>
#include <TofwGeometry.h>
#include <SvxClusterList.h>
//#include <HbdBlobList.h>
#include <HbdMiniCellList.h>
#include <HbdCellList.h>
#include <dTofReconstructedWrapper.h>
#include <emcClusterContainer.h>
#include <utiMatch.h>

#include <getClass.h>

#include <cassert>
#include <cmath>
#include <iostream>
#include <vector>

using namespace std;

// Constructors for cglHitAssociate
cglHitAssociate::cglHitAssociate()
{
  DOSLIDEANDFLIPP = 0;
  SLIDINGFLIPPING = 35;

  Verbose = 0;
  TrackModelFlag = 0;
  PredictMomentum = 0;
  TECZ0Buffer = 0.0;
  TECSlopeCut = 0.0;
  RemoveHits = 0;
  MinPhiWidth = 0.0;
  MinZWidth = 0.0;
  MinDchQuality = 0;
  MaxDchQuality = 0;
  PhiRoadOnly = 0;
  for (int i = 0; i < CGLMAXDET; i++)
    {
      UseFlag[i] = 0;
      PhiRoad[i] = 0.0;
      ZRoad[i] = 0.0;
    }

  TecDchAlphaRatio = 0.505; // Dch reference radius = 220cm, Tec reference radius = 450cm (run4)
  nSigmaTEC = 3.;

  match = new utiMatch();

}
cglHitAssociate::~cglHitAssociate()
{
  delete match;
  return;
}

// Event method for cglHitAssociate
PHBoolean
cglHitAssociate::event(PHCompositeNode* topNode)
{

  Verbose = 2;
  vector<long> iTecRange;
  float Zpro;
  PHVector projVector;
  PHPoint projError, projPoint;
  PHVector projVectorBack;
  PHPoint projErrorBack, projPointBack;

  PHNodeIterator topIter(topNode);
  PHRootNodeIterator DNiter(topNode);

  // Internal variables
  int i;
  int idet, jdet, ndet;
  short iarm, jarm;
  long idc, nTracks;
  long nTecRange, iTec, goodTec, goodTecBg;
  double tecZ;  // TEC coordinates and z-sign
  double Projection[3];          // For PHTrack extraction
  double Error[3];               // For PHTrack extraction
  double Direction[3];           // For PHTrack extraction
  double ProjectionBg[3];
  double ErrorBg[3];
  double DirectionBg[3];
  double zLo, zHi, zMid;   // window z ranges
  double zLoBg, zHiBg, zMidBg;   // window z ranges
  PHAngle phiLo, phiHi, phiMid, phiError;  // window phi ranges
  double phiMidRad, phiErrorRad;
  long nRemove;  // For hits removal
  long detIndex[CGLMAXDET], emcIndex;
  long detIndexBg[CGLMAXDET], emcIndexBg;
  PHBoolean intTest;  // test for intersection
  short dchQualityFlag, dchQualityPass;
  short closeMethod;  // close hit method (0 for phi-z, 1 for phi only)

  // For the drift chamber track model
  PHPoint dchVertex;

  // Find the input tables
  dDchHitWrapper *dDchHit = findNode::getClass<dDchHitWrapper>(topNode,"dDchHit");
  if (!dDchHit)
    {
      cerr << "cglHitAssociate::event - dDchHit not found. No associations with this detector \n";
      return 1;
    }

  dDchTracksWrapper *dDchTracks = findNode::getClass<dDchTracksWrapper>(topNode,"dDchTracks");
  if (!dDchTracks)
    {
      cerr << "cglHitAssociate::event-F: dDchTracks not found.\n";
      return 1;
    }

  dPadClusterWrapper *dPc1Cluster = findNode::getClass<dPadClusterWrapper>(topNode,"dPc1Cluster");
  if (!dPc1Cluster)
    {
      set_UseFlag(2, 0);
    }

  dPadClusterWrapper *dPc2Cluster = findNode::getClass<dPadClusterWrapper>(topNode,"dPc2Cluster");
  if (!dPc2Cluster)
    {
      set_UseFlag(3, 0);
    }

  dPadClusterWrapper *dPc3Cluster = findNode::getClass<dPadClusterWrapper>(topNode,"dPc3Cluster");
  if (!dPc3Cluster)
    {
      set_UseFlag(4, 0);
    }

  TecOutV1 *tecout = findNode::getClass<TecOutV1>(topNode,"TecOutV1");
  if (!tecout)
    {
      set_UseFlag(6, 0);
    }

  // old detectors (tzr,pcr) taken out
      set_UseFlag(10, 0);
      set_UseFlag(11, 0);

  AccRaw *accraw = findNode::getClass<AccRaw>(topNode, "AccRaw");
  if (!accraw)
    {
      set_UseFlag(12,0);
    }

  AccGeometry *accgeo = findNode::getClass<AccGeometry>(topNode, "AccGeometry");
  TofwHit *tofwhit = findNode::getClass<TofwHit>(topNode, "TofwHit");
  if (!tofwhit)
    {
      set_UseFlag(13,0);
    }

  //TofwGeometry *tofwgeo = findNode::getClass<TofwGeometry>(topNode, "TofwGeometry");

  SvxClusterList *svxClusterList = findNode::getClass<SvxClusterList>(topNode, "SvxClusterList");
  if (!svxClusterList)
    {
      set_UseFlag(14,0); set_UseFlag(15,0); set_UseFlag(16,0); set_UseFlag(17,0);
    }

        // 
        // HBD don't need association in production in Run-9
        // removed by T. Sakaguchi, Jan.7, 2008.     
        //   Instead, we keep MiniCell
        // 

//  HbdBlobList *hbdbloblist = findNode::getClass<HbdBlobList>(topNode, "HbdBlobList");
//  if (!hbdbloblist) { set_UseFlag(18,0); }

  // Include hbd track projections if either MiniCellList or CellList is found.
  HbdMiniCellList *hbdmini = findNode::getClass<HbdMiniCellList>(topNode, "HbdMiniCellList");
  HbdCellList *hbd = findNode::getClass<HbdCellList>(topNode,"HbdCellList");
  if (!hbdmini && !hbd)
    {
      set_UseFlag(18,0);
    }

  dTofReconstructedWrapper *dTofReconstructed = findNode::getClass<dTofReconstructedWrapper>(topNode,"dTofReconstructed");
  if (!dTofReconstructed)
    {
      set_UseFlag(7, 0);
    }

  emcClusterContainer* emcClusters = findNode::getClass<emcClusterContainer>(topNode,"emcClusterContainer");
  if (!emcClusters)
    {
      set_UseFlag(8, 0);
      set_UseFlag(9, 0);
    }

  // Set up the PHTrack list in the DST node depending on the selected
  // track model
  PHPointerList<PHLineTrack> *lineTracks = 0;
  PHDataNode< PHPointerList<PHLineTrack> >*
    lineTracksNode = (PHDataNode< PHPointerList<PHLineTrack> >* )
    topIter.findFirst("PHDataNode", "PHLineTrackList");
  if (lineTracksNode)
    {
      lineTracks = lineTracksNode->getData();
    }
  else
    {
      if (TrackModelFlag == 0)
        {
        }
    }


  PHPointerList<PHDchTrack> *phdchTracks = 0;
  PHDataNode<PHPointerList<PHDchTrack> >* phdchTracksNode =
    (PHDataNode<PHPointerList<PHDchTrack> >* )
    topIter.findFirst("PHDataNode", "PHDchTrackList");
  if (phdchTracksNode)
    {
      phdchTracks = phdchTracksNode->getData();
    }
  else
    {
      if (TrackModelFlag == 2)
        {
          cerr << "cglHitAssociate::event-F:  No PHDchTracks available\n";
          return 1;
        }
    }

  PHTrackOut* phtrkOut = findNode::getClass<PHTrackOut>(topNode, "PHTrackOut");
  if (!phtrkOut)
    {
      cerr << PHWHERE << "PHTrackOut Node not found, doing nothing" << endl;
      return 1;
    }

  PHTrackOut* phtrkOutBack  = findNode::getClass<PHTrackOut>(topNode, "PHTrackOutBack");
  if (!phtrkOutBack)
    {
      cout << PHWHERE << "PHTrackOutBack Node not found, doing nothing" << endl;
      return 1;
    }

  CglTrack* cgltrack = findNode::getClass<CglTrack>(topNode, "CglTrack");
  if (!cgltrack)
    {
      cout << PHWHERE << "CglTrack Node not found, doing nothing" << endl;
      return 1;
    }

  CglTrack* cgltrackback = findNode::getClass<CglTrack>(topNode, "CglTrackBack");
  if (!cgltrackback)
    {
      cout << PHWHERE << "CglTrackBack Node not found, doing nothing" << endl;
      return 1;
    }


  // Initialize the output tables.  There will be one set for each
  // PHTrack.
  nTracks = 0;
  if (TrackModelFlag == 0)
    {
      nTracks = (*lineTracks).length();
    }
  else if (TrackModelFlag == 2)
    {
      nTracks = (*phdchTracks).length();
    }
  // Initialize tables

  // initialize output classes
  phtrkOut->set_PHNTrack(nTracks);
  phtrkOut->set_TClonesArraySize(nTracks);
  phtrkOutBack->set_PHNTrack(nTracks);
  phtrkOutBack->set_TClonesArraySize(nTracks);
  cgltrack->set_CglNTrack(nTracks);
  cgltrack->set_TClonesArraySize(nTracks);
  cgltrackback->set_CglNTrack(nTracks);
  cgltrackback->set_TClonesArraySize(nTracks);


  for (idc = 0; idc < nTracks; idc++)
    {
      PHSnglTrack *phsngl = phtrkOut->get_track(idc);
      phsngl->set_trackIndex(idc);
      PHSnglTrack *phsnglbck = phtrkOutBack->get_track(idc);
      phsnglbck->set_trackIndex(idc);
      CglSnglTrack *sngltrack = cgltrack->get_track(idc);
      sngltrack->set_id(idc);
      CglSnglTrack *snglback =cgltrackback->get_track(idc);
      snglback->set_id(idc);
    } // end tracks loop

  // end initialization

  // Loop over each arm



  for (iarm = 0; iarm < 2; iarm++)
    {
      // Create the hit lists for this arm and sort in increasing phi
      cglHitList* dchHitList = new cglHitList(iarm, dDchTracks);
      assert(dchHitList != 0);
      dchHitList->set_Verbose(Verbose);
      dchHitList->SortInPhi();
      cglHitList* pc1HitList = new cglHitList(0, iarm, dPc1Cluster);
      assert(pc1HitList != 0);
      pc1HitList->set_Verbose(Verbose);
      pc1HitList->SortInPhi();
      cglHitList* pc2HitList = new cglHitList(1, iarm, dPc2Cluster);
      assert(pc2HitList != 0);
      pc2HitList->set_Verbose(Verbose);
      pc2HitList->SortInPhi();
      cglHitList* pc3HitList = new cglHitList(2, iarm, dPc3Cluster);
      assert(pc3HitList != 0);
      pc3HitList->set_Verbose(Verbose);
      pc3HitList->SortInPhi();
      cglHitList* tecHitList = new cglHitList(iarm, tecout);
      assert(tecHitList != 0);
      tecHitList->SortInPhi();
      tecHitList->set_Verbose(Verbose);
      cglHitList* tofHitList = new cglHitList(iarm, dTofReconstructed);
      assert(tofHitList != 0);
      tofHitList->SortInPhi();
      tofHitList->set_Verbose(Verbose);
      cglHitList* accHitList = new cglHitList(iarm, accraw, accgeo);
      assert(accHitList != 0);
      accHitList->SortInPhi();
      accHitList->set_Verbose(Verbose);
      cglHitList* tofwHitList = new cglHitList(iarm, tofwhit);
      assert(tofwHitList != 0);
      tofwHitList->SortInPhi();
      tofwHitList->set_Verbose(Verbose);
      cglHitList *svxHitList[SVXLAYERNUMBER];
      for(int ilayer=0; ilayer<SVXLAYERNUMBER; ilayer++){
        svxHitList[ilayer]=0;
        if(UseFlag[14+ilayer]) {
          svxHitList[ilayer] = new cglHitList(ilayer, iarm, svxClusterList);
          assert(svxHitList[ilayer] != 0);
          svxHitList[ilayer]->set_Verbose(Verbose);
          svxHitList[ilayer]->SortInPhi();
        }
      }

        // 
        // HBD don't need association in production in Run-9
        // removed by T. Sakaguchi, Jan.7, 2008.     
        // 
//      cglHitList* hbdBlobList = new cglHitList(iarm, hbdbloblist);
//      assert(hbdBlobList != 0);
//      hbdBlobList->SortInPhi();
//      hbdBlobList->set_Verbose(Verbose);

      cglHitList* pbscHitList=0;
      cglHitList* pbglHitList=0;

      // new emc objects.	
      pbscHitList = new cglHitList(0, iarm, emcClusters); 
      pbglHitList = new cglHitList(1, iarm, emcClusters);
      pbscHitList->SortInPhi();
      pbscHitList->set_Verbose(Verbose);
      pbglHitList->SortInPhi();
      pbglHitList->set_Verbose(Verbose);

      // Loop over PHTracks
      // fill both the tables and the output classes

      for (idc = 0; idc < nTracks; idc++)
        {
          // Point to the proper track
          PHTrack *trackModel = 0;
          if (TrackModelFlag == 0)
            {
              trackModel = (PHLineTrack*)((*lineTracks)[idc]);
            }
          else if (TrackModelFlag == 2)
            {
              trackModel = (PHDchTrack*)((*phdchTracks)[idc]);
            }



          // Check the arm number
          jarm = trackModel->getArm();
          // Check the drift chamber quality.
          dchQualityFlag = (short) dDchTracks->get_quality(idc);
          dchQualityPass = 1;
          if (dchQualityFlag <= MinDchQuality)
            {
              dchQualityPass = 0;
            }


          // Only work with tracks in this arm Only work with tracks
          // that pass the dch quality cut
          if (jarm == iarm && dchQualityPass == 1)
            {
              // Loop over the detector sets.  If a hit candidate is
              // found, set that index to the candidate entry number.
              // If no candidate, leave them all at -1
              for (i = 0; i < CGLMAXDET; i++)
                {
                  detIndex[i] = -1;
                  detIndexBg[i] = -1;
                }

              // Loop over all detectors.  If there is an
              // intersection, then deal with it.
              jdet = 0;  // Points to which projection is being handled
              ndet = trackModel->getIfIntersectLength();
              for (idet = 0; idet < ndet; idet++)
                {
                  intTest = trackModel->getIfIntersectFlag(idet);
                  // Is the association to this detector requested?
                  if (UseFlag[idet] == 1 && intTest == 1)
                    {
                      // Detector-specific projections
                      projPoint = trackModel->getProjectionPoint(jdet);
                      projError = trackModel->getProjectionError(jdet);
                      projVector = trackModel->getDirectionVector(jdet);

                      // Fill the PHTrackOut and PHTrackOutBack tables with the
                      // projection results
                      Projection[0] = projPoint.getX();
                      Projection[1] = projPoint.getY();
                      Projection[2] = projPoint.getZ();
                      Error[0] = projError.getX();
                      Error[1] = projError.getY();
                      Error[2] = projError.getZ();
                      Direction[0] = projVector.getX();
                      Direction[1] = projVector.getY();
                      Direction[2] = projVector.getZ();

                      // Projection with flipped background
                      projPointBack = projPoint;
                      projErrorBack = projError;
                      projVectorBack = projVector;
                      Zpro = projPoint.getZ();
                      // Flip and slide (flip only for Tec)
                      if ((fabs(Zpro) > SLIDINGFLIPPING) || idet == 6)
                        {
                          Zpro = -Zpro;
                        }
                      else if (Zpro >= 0)
                        {
                          Zpro = Zpro - SLIDINGFLIPPING;
                        }
                      else if (Zpro < 0)
                        {
                          Zpro = Zpro + SLIDINGFLIPPING;
                        }
                      projPointBack.setZ(Zpro);
                      ProjectionBg[0] = projPointBack.getX();
                      ProjectionBg[1] = projPointBack.getY();
                      ProjectionBg[2] = projPointBack.getZ();
                      ErrorBg[0] = projErrorBack.getX();
                      ErrorBg[1] = projErrorBack.getY();
                      ErrorBg[2] = projErrorBack.getZ();
                      DirectionBg[0] = projVectorBack.getX();
                      DirectionBg[1] = projVectorBack.getY();
                      DirectionBg[2] = projVectorBack.getZ();
                      PHSnglTrack *phsngl = phtrkOut->get_track(idc);
                      PHSnglTrack *phsnglbck = phtrkOutBack->get_track(idc);

            //          cout<<"Proj Det: "<<idet<<", X: "<<Projection[0]<<endl;

                      switch (idet)
                        {
                        case 0:
                          for (int k = 0;k < 3;k++)
                            {
                              phsngl->set_projectionVtx(k, Projection[k]);
                              phsngl->set_directionVtx(k, Direction[k]);
                              phsnglbck->set_projectionVtx(k, Projection[k]);
                              phsnglbck->set_directionVtx(k, Direction[k]);
                            }
                          break;
                        case 1:
                          for (int k = 0;k < 3;k++)
                            {
                              phsngl->set_projectionDch(k, Projection[k]);
                              phsngl->set_directionDch(k, Direction[k]);
                              phsnglbck->set_projectionDch(k, Projection[k]);
                              phsnglbck->set_directionDch(k, Direction[k]);
                            }
                          break;
                        case 2:
                          for (int k = 0;k < 3;k++)
                            {
                              phsngl->set_projectionPc1(k, Projection[k]);
                              phsngl->set_directionPc1(k, Direction[k]);
                              phsnglbck->set_projectionPc1(k, ProjectionBg[k]);
                              phsnglbck->set_directionPc1(k, DirectionBg[k]);
                            }

                          break;
                        case 3:
                          for (int k = 0;k < 3;k++)
                            {
                              phsngl->set_projectionPc2(k, Projection[k]);
                              phsngl->set_directionPc2(k, Direction[k]);
                              phsnglbck->set_projectionPc2(k, ProjectionBg[k]);
                              phsnglbck->set_directionPc2(k, DirectionBg[k]);
                            }
                          break;
                        case 4:
                          for (int k = 0;k < 3;k++)
                            {
                              phsngl->set_projectionPc3(k, Projection[k]);
                              phsngl->set_directionPc3(k, Direction[k]);
                              phsnglbck->set_projectionPc3(k, ProjectionBg[k]);
                              phsnglbck->set_directionPc3(k, DirectionBg[k]);
                            }

                          break;
                        case 5:
                          for (int k = 0;k < 3;k++)
                            {
                              phsngl->set_projectionCrk(k, Projection[k]);
                              phsngl->set_directionCrk(k, Direction[k]);
                              phsnglbck->set_projectionCrk(k, ProjectionBg[k]);
                              phsnglbck->set_directionCrk(k, DirectionBg[k]);
                            }

                          phsngl->set_crkPathLength(trackModel->pathLengthToCrk());
                          phsnglbck->set_crkPathLength(trackModel->pathLengthToCrk());

                          break;
                        case 6:
                          for (int k = 0;k < 3;k++)
                            {
                              phsngl->set_projectionTec(k, Projection[k]);
                              phsngl->set_directionTec(k, Direction[k]);
                              phsnglbck->set_projectionTec(k, ProjectionBg[k]);
                              phsnglbck->set_directionTec(k, DirectionBg[k]);
                            }

                          break;
                        case 7:
                          for (int k = 0;k < 3;k++)
                            {
                              phsngl->set_projectionTof(k, Projection[k]);
                              phsngl->set_directionTof(k, Direction[k]);
                              phsnglbck->set_projectionTof(k, ProjectionBg[k]);
                              phsnglbck->set_directionTof(k, DirectionBg[k]);
                            }

                          phsngl->set_tofPathLength(trackModel->pathLengthToTof());
                          phsnglbck->set_tofPathLength(trackModel->pathLengthToTof());

                          break;
                        case 8:
                          for (int k = 0;k < 3;k++)
                            {
                              phsngl->set_projectionPbSc(k, Projection[k]);
                              phsngl->set_directionPbSc(k, Direction[k]);
                              phsnglbck->set_projectionPbSc(k, ProjectionBg[k]);
                              phsnglbck->set_directionPbSc(k, DirectionBg[k]);
                            }
                          phsngl->set_emcPathLength(trackModel->pathLengthToEmc());
                          phsnglbck->set_emcPathLength(trackModel->pathLengthToEmc());

                          break;
                        case 9:
                          for (int k = 0;k < 3;k++)
                            {
                              phsngl->set_projectionPbGl(k, Projection[k]);
                              phsngl->set_directionPbGl(k, Direction[k]);
                              phsnglbck->set_projectionPbGl(k, ProjectionBg[k]);
                              phsnglbck->set_directionPbGl(k, DirectionBg[k]);
                            }
                          phsngl->set_emcPathLength(trackModel->pathLengthToEmc());
                          phsnglbck->set_emcPathLength(trackModel->pathLengthToEmc());

                          break;
			  // tzr
                        case 10:
			  // pcr
                        case 11:
                          break;

			case 12:
                          for (int k = 0;k < 3;k++)
                            {
                              phsngl->set_projectionAcc(k, Projection[k]);
                              phsngl->set_directionAcc(k, Direction[k]);
                              phsnglbck->set_projectionAcc(k, ProjectionBg[k]);
                              phsnglbck->set_directionAcc(k, DirectionBg[k]);
                            }

                          break;
			case 13:
                          for (int k = 0;k < 3;k++)
                            {
                              phsngl->set_projectionTofw(k, Projection[k]);
                              phsngl->set_directionTofw(k, Direction[k]);
                              phsnglbck->set_projectionTofw(k, ProjectionBg[k]);
                              phsnglbck->set_directionTofw(k, DirectionBg[k]);
                            }
                          phsngl->set_tofwPathLength(trackModel->pathLengthToTofw());
                          phsnglbck->set_tofwPathLength(trackModel->pathLengthToTofw());
			  
                          break;
                     
                        // 14~17 is reserved for Si barrel layer 1~4
  	                case 14:
                         if(UseFlag[14]) {
  	                  for (int k = 0;k < 3;k++)
  	                    {
  	                       phtrkOut->set_projectionSvx(idc, 0, k, Projection[k]);
  	                       phtrkOut->set_directionSvx(idc, 0, k, Direction[k]);
  	                       phtrkOutBack->set_projectionSvx(idc, 0, k, ProjectionBg[k]);
  	                       phtrkOutBack->set_directionSvx(idc, 0, k, DirectionBg[k]);
  	                    }
                         }
                          break;

                        case 15:
                         if(UseFlag[15]) {
  	                  for (int k = 0;k < 3;k++)
  	                    {
  	                       phtrkOut->set_projectionSvx(idc, 1, k, Projection[k]);
  	                       phtrkOut->set_directionSvx(idc, 1, k, Direction[k]);
  	                       phtrkOutBack->set_projectionSvx(idc, 1, k, ProjectionBg[k]);
  	                       phtrkOutBack->set_directionSvx(idc, 1, k, DirectionBg[k]);
  	                    }
                         }
  	                  break;
  	 
  	                case 16:
                         if(UseFlag[16]) {
  	                  for (int k = 0;k < 3;k++)
  	                    {
  	                       phtrkOut->set_projectionSvx(idc, 2, k, Projection[k]);
  	                       phtrkOut->set_directionSvx(idc, 2, k, Direction[k]);
  	                       phtrkOutBack->set_projectionSvx(idc, 2, k, ProjectionBg[k]);
  	                       phtrkOutBack->set_directionSvx(idc, 2, k, DirectionBg[k]);
  	                    }
                         }
  	                  break;
  	 
  	                case 17:
                         if(UseFlag[17]) {
  	                  for (int k = 0;k < 3;k++)
  	                    {
  	                       phtrkOut->set_projectionSvx(idc, 3, k, Projection[k]);
  	                       phtrkOut->set_directionSvx(idc, 3, k, Direction[k]);
  	                       phtrkOutBack->set_projectionSvx(idc, 3, k, ProjectionBg[k]);
  	                       phtrkOutBack->set_directionSvx(idc, 3, k, DirectionBg[k]);
  	                    }
                         }
			 break;
			 
			case 18:
                          for (int k = 0;k < 3;k++)
                            {
                              phsngl->set_projectionHbd(k, Projection[k]);
                              phsngl->set_directionHbd(k, Direction[k]);
                              phsnglbck->set_projectionHbd(k, ProjectionBg[k]);
                              phsnglbck->set_directionHbd(k, DirectionBg[k]);
                            }
  	                  break;

                        default:
                          break;
                        }

                      // Convert the projection to cylindrical
                      // coordinates.
                      PHCylPoint detPoint(projPoint);
                      PHCylPoint detPointBack(projPointBack);

                      // Define the phi,z window
                      phiMidRad = detPoint.getPhi();
                      phiMid = phiMidRad;  // To PHAngle
                      phiErrorRad = projError.getY();
                      phiErrorRad *= PhiRoad[idet];
                      if (phiErrorRad < MinPhiWidth)
                        phiErrorRad = MinPhiWidth;
                      phiError = phiErrorRad;  // To PHAngle
                      phiHi = phiMid + phiError;
                      phiLo = phiMid - phiError;

                      zMid = detPoint.getZ();
                      Error[2] = projError.getZ();
                      Error[2] *= ZRoad[idet];
                      if (Error[2] < MinZWidth)
                        {
                          Error[2] = MinZWidth;
                        }
                      zHi = zMid + Error[2];
                      zLo = zMid - Error[2];
                      // Background
                      zMidBg = detPointBack.getZ();
                      ErrorBg[2] = projErrorBack.getZ();
                      ErrorBg[2] *= ZRoad[idet];
                      if (ErrorBg[2] < MinZWidth)
                        {
                          ErrorBg[2] = MinZWidth;
                        }
                      zHiBg = zMidBg + ErrorBg[2];
                      zLoBg = zMidBg - ErrorBg[2];

                      // Determine if a phi-z closeness check is
                      // requested, or only a 2-D phi closeness check.
                      // In order to revert to a 2-D search, the
                      // following must hold: 1. The input parameter
                      // PhiRoadOnly must be set 2. dDchTracks.quality
                      // must be below MaxDchQuality
                      closeMethod = 0;
                      if (PhiRoadOnly == 1 && dchQualityFlag <= MaxDchQuality)
                        {
                          closeMethod = 1;
                        }
                      // Detector-specific hit associations
                      switch (idet)
                        {
                        case 2:
                          if (closeMethod == 0)
                            {
                              detIndex[idet] =
                                pc1HitList->PhiZClose(projPoint, phiLo, phiHi, zLo, zHi);
                              detIndexBg[idet] =
                                pc1HitList->PhiZClose(projPointBack, phiLo, phiHi, zLoBg, zHiBg);

                            }
                          else if (closeMethod == 1)
                            {
                              detIndex[idet] =
                                pc1HitList->PhiClose(projPoint, phiLo, phiHi, zLo, zHi);
                              detIndexBg[idet] =
                                pc1HitList->PhiClose(projPointBack, phiLo, phiHi, zLoBg, zHiBg);
                            }
                          if (RemoveHits > 0)
                            {
                              nRemove = pc1HitList->get_nFromIndex(detIndex[idet]);
                              pc1HitList->Remove(nRemove);
                            }
                          break;

                        case 3:
                          if (closeMethod == 0)
                            {
                              detIndex[idet] =
                                pc2HitList->PhiZClose(projPoint, phiLo, phiHi, zLo, zHi);
                              detIndexBg[idet] =
                                pc2HitList->PhiZClose(projPointBack, phiLo, phiHi, zLoBg, zHiBg);
                            }
                          else if (closeMethod == 1)
                            {
                              detIndex[idet] =
                                pc2HitList->PhiClose(projPoint, phiLo, phiHi, zLo, zHi);
                              detIndexBg[idet] =
                                pc2HitList->PhiClose(projPointBack, phiLo, phiHi, zLoBg, zHiBg);
                            }
                          if (RemoveHits > 0)
                            {
                              nRemove = pc2HitList->get_nFromIndex(detIndex[idet]);
                              pc2HitList->Remove(nRemove);
                            }
                          break;
                        case 4:
                          if (closeMethod == 0)
                            {
                              detIndex[idet] =
                                pc3HitList->PhiZClose(projPoint, phiLo, phiHi, zLo, zHi);
                              detIndexBg[idet] =
                                pc3HitList->PhiZClose(projPointBack, phiLo, phiHi, zLoBg, zHiBg);
                            }
                          else if (closeMethod == 1)
                            {
                              detIndex[idet] =
                                pc3HitList->PhiClose(projPoint, phiLo, phiHi, zLo, zHi);
                              detIndexBg[idet] =
                                pc3HitList->PhiClose(projPointBack, phiLo, phiHi, zLoBg, zHiBg);
                            }
                          if (RemoveHits > 0)
                            {
                              nRemove = pc3HitList->get_nFromIndex(detIndex[idet]);
                              pc3HitList->Remove(nRemove);
                            }
                          break;

                        case 6:

			{
                          // TEC is a 2D detector, and needs special
                          // treatment Get the list of all TEC tracks
                          // within the phi range
                          nTecRange = tecHitList->PhiRange(phiLo, phiHi, iTecRange);
			  int nIn3sigmas=0;
			  int listIn3sigmas[999]; for(int l=0; l<999; l++) {listIn3sigmas[l]=-1;}
			  int nIn3sigmasbg=0;
			  int listIn3sigmasbg[999]; for(int l=0; l<999; l++) {listIn3sigmasbg[l]=-1;}

                          // Loop over TEC tracks in range.  Look for
                          // the closest track in 2-dim phi/alpha space
                          // Dch track alpha and sigmas
                          float dchAlpha = dDchTracks->get_alpha(idc);
                          float dchMomentum = dDchTracks->get_momentum(idc);

                          for (iTec = 0; iTec < nTecRange; iTec++) {

                              // Reset if the track is rejected
                              goodTec = 1;
                              goodTecBg = 1;

                              // Check the validity of the TEC track index
                              if (iTecRange[iTec] < 0 || iTecRange[iTec] >= tecout->getNTracks()) {
                                  goodTec = 0;
                                  goodTecBg = 0;
                              }

                              // Check the z-sign of the TEC
                              // track.  Reject this track if
                              // incorrect outside of the z=0 buffer
                              if (goodTec) {
                                  tecZ = 1.0;
                                  if (tecout->getTrackSide(iTecRange[iTec]) == 0) { tecZ = -1.0; }
                                  if (fabs(zMid) > TECZ0Buffer) {
                                      if (zMid > 0.0 && tecZ < 0.0) { goodTec = 0; }
                                      if (zMid < 0.0 && tecZ > 0.0) { goodTec = 0; }
                                  }
                              }
                              if (goodTecBg) {
                                  tecZ = 1.0;
                                  if (tecout->getTrackSide(iTecRange[iTec]) == 0) { tecZ = -1.0; }
                                  if (fabs(zMid) > TECZ0Buffer) {
                                      if (zMid > 0.0 && tecZ > 0.0) { goodTecBg = 0; }
                                      if (zMid < 0.0 && tecZ < 0.0) { goodTecBg = 0; }
                                  }
                              }

			      // First find all tracks in 3 sigma in phi
                              if (goodTec) {
                                  TecTrack* tectrack = (TecTrack*)
				    ((tecout->GetTecTracks())->UncheckedAt(iTecRange[iTec]));
                                  float tecPhi = tectrack->getPhi();
				  float tec_dPhi = match->d_TEC_phi_match(dchMomentum, tecPhi - phiMidRad); 
                                  if (abs(tec_dPhi) < nSigmaTEC) {
                                      listIn3sigmas[nIn3sigmas] = iTecRange[iTec];
				      nIn3sigmas++;
                                  }
                              }
                              if (goodTecBg) {
                                  TecTrack* tectrack = (TecTrack*)
				    ((tecout->GetTecTracks())->UncheckedAt(iTecRange[iTec]));
                                  float tecPhi = tectrack->getPhi();
				  float tec_dPhi = match->d_TEC_phi_match(dchMomentum, tecPhi - phiMidRad); 
                                  if (abs(tec_dPhi) < nSigmaTEC) {
                                      listIn3sigmasbg[nIn3sigmasbg] = iTecRange[iTec];
				      nIn3sigmasbg++;
                                  }
                              }

                              if (RemoveHits > 0 && detIndex[idet] >= 0) {
                                  nRemove = tecHitList->get_nFromIndex(detIndex[idet]);
                                  tecHitList->Remove(nRemove);
                              }

                          } // end loop over tec tracks
			  
			// finally select closest track among tracks which are in 3 sigmas in phi
                          float maxdist = 999.;
                          int finalindex = -1;
			  for(int k=0; k<nIn3sigmas; k++) {
                             TecTrack* tectrack = (TecTrack*) ((tecout->GetTecTracks())->UncheckedAt(listIn3sigmas[k]));
                             float tecPhi = tectrack->getPhi();
                             float tecAlpha = tectrack->getAlpha();
			     float tec_dPhi = match->d_TEC_phi_match(dchMomentum, tecPhi - phiMidRad); 
		             float tec_dAlpha = match->d_TEC_alpha_match(dchMomentum, tecAlpha - dchAlpha*TecDchAlphaRatio); 
                             float tec_dAlphaPhi = sqrt(tec_dPhi * tec_dPhi + tec_dAlpha * tec_dAlpha);
                             if(tec_dAlphaPhi<maxdist) {
                               maxdist=tec_dAlphaPhi;
                               finalindex = listIn3sigmas[k];
                             }
			  }
                          maxdist = 999.;
                          int finalindexbg = -1;
			  for(int k=0; k<nIn3sigmasbg; k++) {
                             TecTrack* tectrack = (TecTrack*) ((tecout->GetTecTracks())->UncheckedAt(listIn3sigmasbg[k]));
                             float tecPhi = tectrack->getPhi();
                             float tecAlpha = tectrack->getAlpha();
			     float tec_dPhi = match->d_TEC_phi_match(dchMomentum, tecPhi - phiMidRad); 
		             float tec_dAlpha = match->d_TEC_alpha_match(dchMomentum, tecAlpha - dchAlpha*TecDchAlphaRatio); 
                             float tec_dAlphaPhi = sqrt(tec_dPhi * tec_dPhi + tec_dAlpha * tec_dAlpha);
                             if(tec_dAlphaPhi<maxdist) {
                               maxdist=tec_dAlphaPhi;
                               finalindexbg = listIn3sigmasbg[k];
                             }
			  }
                             detIndex[idet] = finalindex;
                             detIndexBg[idet] = finalindexbg;

                          break;

                        } // end of tec matching

                        case 7:
                          if (closeMethod == 0)
                            {
                              detIndex[idet] =
                                tofHitList->PhiZClose(projPoint, phiLo, phiHi, zLo, zHi);
                              detIndexBg[idet] =
                                tofHitList->PhiZClose(projPointBack, phiLo, phiHi, zLoBg, zHiBg);
                            }
                          else if (closeMethod == 1)
                            {
                              detIndex[idet] =
                                tofHitList->PhiClose(projPoint, phiLo, phiHi, zLo, zHi);
                              detIndexBg[idet] =
                                tofHitList->PhiClose(projPointBack, phiLo, phiHi, zLoBg, zHiBg);
                            }
                          if (RemoveHits > 0)
                            {
                              nRemove = tofHitList->get_nFromIndex(detIndex[idet]);
                              tofHitList->Remove(nRemove);
                            }
                          break;
                        case 8:
                          if (closeMethod == 0)
                            {
                              detIndex[idet] =
                                pbscHitList->PhiZClose(projPoint, phiLo, phiHi, zLo, zHi);
                              detIndexBg[idet] =
                                pbscHitList->PhiZClose(projPointBack, phiLo, phiHi, zLoBg, zHiBg);
                            }
                          else if (closeMethod == 1)
                            {
                              detIndex[idet] =
                                pbscHitList->PhiClose(projPoint, phiLo, phiHi, zLo, zHi);
                              detIndexBg[idet] =
                                pbscHitList->PhiClose(projPointBack, phiLo, phiHi, zLoBg, zHiBg);
                            }
                          if (RemoveHits > 0)
                            {
                              nRemove = pbscHitList->get_nFromIndex(detIndex[idet]);
                              pbscHitList->Remove(nRemove);
                            }
                          break;
                        case 9:
                          if (closeMethod == 0)
                            {
                              detIndex[idet] =
                                pbglHitList->PhiZClose(projPoint, phiLo, phiHi, zLo, zHi);
                              detIndexBg[idet] =
                                pbglHitList->PhiZClose(projPointBack, phiLo, phiHi, zLoBg, zHiBg);
                            }
                          else if (closeMethod == 1)
                            {
                              detIndex[idet] =
                                pbglHitList->PhiClose(projPoint, phiLo, phiHi, zLo, zHi);
                              detIndexBg[idet] =
                                pbglHitList->PhiClose(projPointBack, phiLo, phiHi, zLoBg, zHiBg);
                            }
                          if (RemoveHits > 0)
                            {
                              nRemove = pbglHitList->get_nFromIndex(detIndex[idet]);
                              pbglHitList->Remove(nRemove);
                            }
                          break;
			  // tzr, pcr taken out
                        case 10:
                        case 11:
                          break;
                        case 12:
                          if (closeMethod == 0)
                            {
                              detIndex[idet] =
                                accHitList->PhiZClose(projPoint, phiLo, phiHi, zLo, zHi);
                              detIndexBg[idet] =
                                accHitList->PhiZClose(projPointBack, phiLo, phiHi, zLoBg, zHiBg);
                            }
                          else if (closeMethod == 1)
                            {
                              detIndex[idet] =
                                accHitList->PhiClose(projPoint, phiLo, phiHi, zLo, zHi);
                              detIndexBg[idet] =
                                accHitList->PhiClose(projPointBack, phiLo, phiHi, zLoBg, zHiBg);
                            }
                          if (RemoveHits > 0)
                            {
                              nRemove = accHitList->get_nFromIndex(detIndex[idet]);
                              accHitList->Remove(nRemove);
                            }
                          break;
                        case 13:
                          if (closeMethod == 0)
                            {
                              detIndex[idet] =
                                tofwHitList->PhiZClose(projPoint, phiLo, phiHi, zLo, zHi);
                              detIndexBg[idet] =
                                tofwHitList->PhiZClose(projPointBack, phiLo, phiHi, zLoBg, zHiBg);
                            }
                          else if (closeMethod == 1)
                            {
                              detIndex[idet] =
                                tofwHitList->PhiClose(projPoint, phiLo, phiHi, zLo, zHi);
                              detIndexBg[idet] =
                                tofwHitList->PhiClose(projPointBack, phiLo, phiHi, zLoBg, zHiBg);
                            }
                          if (RemoveHits > 0)
                            {
                              nRemove = tofwHitList->get_nFromIndex(detIndex[idet]);
                              tofwHitList->Remove(nRemove);
                            }
                          break;

                        case 14:
                         if(UseFlag[14]) {
  	                  if (closeMethod == 0)
  	                    {
  	                      detIndex[idet] =
  	                        svxHitList[0]->PhiZClose(projPoint, phiLo, phiHi, zLo, zHi);
  	                      detIndexBg[idet] =
  	                        svxHitList[0]->PhiZClose(projPointBack, phiLo, phiHi, zLoBg, zHiBg);
  	                    }
  	                  else if (closeMethod == 1)
  	                    {
  	                      detIndex[idet] =
  	                        svxHitList[0]->PhiClose(projPoint, phiLo, phiHi, zLo, zHi);
  	                      detIndexBg[idet] =
  	                        svxHitList[0]->PhiClose(projPointBack, phiLo, phiHi, zLoBg, zHiBg);
  	                    }
  	                  if (RemoveHits > 0)
  	                    {
  	                      nRemove = svxHitList[0]->get_nFromIndex(detIndex[idet]);
  	                      svxHitList[0]->Remove(nRemove);
  	                    }
                         }
  	                  break;

                        case 15:
                         if(UseFlag[15]) {
                          if (closeMethod == 0)
                            {
                              detIndex[idet] =
                                svxHitList[1]->PhiZClose(projPoint, phiLo, phiHi, zLo, zHi);
                              detIndexBg[idet] =
                                svxHitList[1]->PhiZClose(projPointBack, phiLo, phiHi, zLoBg, zHiBg);
                            }
                          else if (closeMethod == 1)
                            {
                              detIndex[idet] =
                                svxHitList[1]->PhiClose(projPoint, phiLo, phiHi, zLo, zHi);
                              detIndexBg[idet] =
                                svxHitList[1]->PhiClose(projPointBack, phiLo, phiHi, zLoBg, zHiBg);
                            }
                          if (RemoveHits > 0)
                            {
                              nRemove = svxHitList[1]->get_nFromIndex(detIndex[idet]);
                              svxHitList[1]->Remove(nRemove);
                            }
                         }
                          break;
                                                                                                                             
                        case 16:
                         if(UseFlag[16]) {
                          if (closeMethod == 0)
                            {
                              detIndex[idet] =
                                svxHitList[2]->PhiZClose(projPoint, phiLo, phiHi, zLo, zHi);
                              detIndexBg[idet] =
                                svxHitList[2]->PhiZClose(projPointBack, phiLo, phiHi, zLoBg, zHiBg);
                            }
                          else if (closeMethod == 1)
                            {
                              detIndex[idet] =
                                svxHitList[2]->PhiClose(projPoint, phiLo, phiHi, zLo, zHi);
                              detIndexBg[idet] =
                                svxHitList[2]->PhiClose(projPointBack, phiLo, phiHi, zLoBg, zHiBg);
                            }
                          if (RemoveHits > 0)
                            {
                              nRemove = svxHitList[2]->get_nFromIndex(detIndex[idet]);
                              svxHitList[2]->Remove(nRemove);
                            }
                         }
                          break;
                                                                                                                             
                        case 17:
                         if(UseFlag[17]) {
                          if (closeMethod == 0)
                            {
                              detIndex[idet] =
                                svxHitList[3]->PhiZClose(projPoint, phiLo, phiHi, zLo, zHi);
                              detIndexBg[idet] =
                                svxHitList[3]->PhiZClose(projPointBack, phiLo, phiHi, zLoBg, zHiBg);
                            }
                          else if (closeMethod == 1)
                            {
                              detIndex[idet] =
                                svxHitList[3]->PhiClose(projPoint, phiLo, phiHi, zLo, zHi);
                              detIndexBg[idet] =
                                svxHitList[3]->PhiClose(projPointBack, phiLo, phiHi, zLoBg, zHiBg);
                            }
                          if (RemoveHits > 0)
                            {
                              nRemove = svxHitList[3]->get_nFromIndex(detIndex[idet]);
                              svxHitList[3]->Remove(nRemove);
                            }
                         }
                          break;

                        case 18:
//
// HBD don't do anything on association in Run-9
//  T. Sakaguchi, Jan 7, 2008
//

/*
                          if (closeMethod == 0)
                            {
                              detIndex[idet] =
                                hbdBlobList->PhiZClose(projPoint, phiLo, phiHi, zLo, zHi);
                              detIndexBg[idet] =
                                hbdBlobList->PhiZClose(projPointBack, phiLo, phiHi, zLoBg, zHiBg);
                            }
                          else if (closeMethod == 1)
                            {
                              detIndex[idet] =
                                hbdBlobList->PhiClose(projPoint, phiLo, phiHi, zLo, zHi);
                              detIndexBg[idet] =
                                hbdBlobList->PhiClose(projPointBack, phiLo, phiHi, zLoBg, zHiBg);
                            }
                          if (RemoveHits > 0)
                            {
                              nRemove = hbdBlobList->get_nFromIndex(detIndex[idet]);
                              hbdBlobList->Remove(nRemove);
                            }
*/
                          break;

                        default:
                          break;
                        } // end associate switch

                    } // ifUseFlag

                  // Increment the projection number
                  if (intTest)
                    {
                      jdet++;
                    }
                } // idet loop end

              // Set the CglTrack values for this track
              emcIndex = detIndex[8];
              if (emcIndex == -1)
                {
                  emcIndex = detIndex[9];
                }
              emcIndexBg = detIndexBg[8];
              if (emcIndexBg == -1)
                {
                  emcIndexBg = detIndexBg[9];
                }

              // fill CglTrack and CglTrackBack output classes here
              CglSnglTrack *sngltrk = cgltrack->get_track(idc);
              sngltrk->set_arm(iarm);
              sngltrk->set_dctracksid(idc);
              sngltrk->set_pc1clusid(detIndex[2]);
              sngltrk->set_pc2clusid(detIndex[3]);
              sngltrk->set_pc3clusid(detIndex[4]);
              sngltrk->set_tectrackid(detIndex[6]);
              sngltrk->set_tofrecid(detIndex[7]);
              sngltrk->set_emcclusid(emcIndex);
              sngltrk->set_richringid(detIndex[5]);
	      sngltrk->set_accrecid(detIndex[12]);
	      sngltrk->set_tofwrecid(detIndex[13]);
              for(int ilayer=0; ilayer<SVXLAYERNUMBER; ilayer++){
                if(UseFlag[14+ilayer]) {
  	          sngltrk->set_svxclusid(ilayer, detIndex[14+ilayer]);
                }
  	      }
	      sngltrk->set_hbdblobid(detIndex[18]);
              sngltrk->set_quality(dDchTracks->get_quality(idc));
              sngltrk->set_trackModel(TrackModelFlag);

              CglSnglTrack *snglback = cgltrackback->get_track(idc);
              snglback->set_arm(iarm);
              snglback->set_dctracksid(idc);
              snglback->set_pc1clusid(detIndexBg[2]);
              snglback->set_pc2clusid(detIndexBg[3]);
              snglback->set_pc3clusid(detIndexBg[4]);
              snglback->set_tectrackid(detIndexBg[6]);
              snglback->set_tofrecid(detIndexBg[7]);
              snglback->set_emcclusid(emcIndexBg);
              snglback->set_richringid(detIndexBg[5]);
	      snglback->set_accrecid(detIndexBg[12]);
	      snglback->set_tofwrecid(detIndexBg[13]);
              for(int ilayer=0; ilayer<SVXLAYERNUMBER; ilayer++){
                if(UseFlag[14+ilayer]) {
  	          snglback->set_svxclusid(ilayer, detIndex[14+ilayer]);
                }
  	      }
	      snglback->set_hbdblobid(detIndexBg[18]);
              snglback->set_quality(dDchTracks->get_quality(idc));
              snglback->set_trackModel(TrackModelFlag);
            } // this arm and dch quality pass
        } // end loop over tracks
      // Delete the hit lists for this arm



      if (dchHitList)
        {
          delete dchHitList;
        }
      if (pc1HitList)
        {
          delete pc1HitList;
        }
      if (pc2HitList)
        {
          delete pc2HitList;
        }
      if (pc3HitList)
        {
          delete pc3HitList;
        }
      if (tecHitList)
        {
          delete tecHitList;
        }
      if (tofHitList)
        {
          delete tofHitList;
        }
      if (pbscHitList)
        {
          delete pbscHitList;
        }
      if (pbglHitList)
        {
          delete pbglHitList;
        }
      if (accHitList)
	{
	  delete accHitList;
	}
      if (tofwHitList)
	{
	  delete tofwHitList;
	}
      for(int ilayer=0; ilayer<SVXLAYERNUMBER; ilayer++)
        {
          if (svxHitList[ilayer]) { delete svxHitList[ilayer]; }
        }
//      if (hbdBlobList) { delete hbdBlobList; }

    } // end loop over arms
  return 0;
}

void
cglHitAssociate::set_Verbose(short setverb)
{
  Verbose = setverb;
}

short
cglHitAssociate::get_Verbose()
{
  return Verbose;
}

void
cglHitAssociate::set_TrackModelFlag(short setmod)
{
  TrackModelFlag = setmod;
}

short
cglHitAssociate::get_TrackModelFlag()
{
  return TrackModelFlag;
}

void
cglHitAssociate::set_PredictMomentum(short setmom)
{
  PredictMomentum = setmom;
}

short
cglHitAssociate::get_PredictMomentum()
{
  return PredictMomentum;
}

void
cglHitAssociate::set_TECZ0Buffer(double settecz0)
{
  TECZ0Buffer = settecz0;
}

double
cglHitAssociate::get_TECZ0Buffer()
{
  return TECZ0Buffer;
}

void
cglHitAssociate::set_TECSlopeCut(double settec)
{
  TECSlopeCut = settec;
}

double
cglHitAssociate::get_TECSlopeCut()
{
  return TECSlopeCut;
}

void
cglHitAssociate::set_RemoveHits(short setrem)
{
  RemoveHits = setrem;
}

short
cglHitAssociate::get_RemoveHits()
{
  return RemoveHits;
}

void
cglHitAssociate::set_UseFlag(short setuse[])
{
  for (int i = 0; i < CGLMAXDET; i++)
    {
      UseFlag[i] = setuse[i];
    }
}

void
cglHitAssociate::get_UseFlag(short getuse[])
{
  for (int i = 0; i < CGLMAXDET; i++)
    {
      getuse[i] = UseFlag[i];
    }
}

void
cglHitAssociate::set_PhiRoad(double setphi[])
{
  for (int i = 0; i < CGLMAXDET; i++)
    {
      PhiRoad[i] = setphi[i];
    }
}

void
cglHitAssociate::get_PhiRoad(double getphi[])
{
  for (int i = 0; i < CGLMAXDET; i++)
    {
      getphi[i] = PhiRoad[i];
    }
}

void
cglHitAssociate::set_ZRoad(double setz[])
{
  for (int i = 0; i < CGLMAXDET; i++)
    {
      ZRoad[i] = setz[i];
    }
}

void
cglHitAssociate::get_ZRoad(double getz[])
{
  for (int i = 0; i < CGLMAXDET; i++)
    {
      getz[i] = ZRoad[i];
    }
}

void
cglHitAssociate::set_MinPhiWidth(double setpr)
{
  MinPhiWidth = setpr;
}

double
cglHitAssociate::get_MinPhiWidth()
{
  return MinPhiWidth;
}

void
cglHitAssociate::set_MinZWidth(double setzr)
{
  MinZWidth = setzr;
}

double
cglHitAssociate::get_MinZWidth()
{
  return MinZWidth;
}

void
cglHitAssociate::set_MinDchQuality(short setqual)
{
  MinDchQuality = setqual;
}

short
cglHitAssociate::get_MinDchQuality()
{
  return MinDchQuality;
}

void
cglHitAssociate::set_PhiRoadOnly(short setpr)
{
  PhiRoadOnly = setpr;
}

short
cglHitAssociate::get_PhiRoadOnly()
{
  return PhiRoadOnly;
}

void
cglHitAssociate::set_MaxDchQuality(short setmax)
{
  MaxDchQuality = setmax;
}

short
cglHitAssociate::get_MaxDchQuality()
{
  return MaxDchQuality;
}

void
cglHitAssociate::PrintParameters()
{
  cout << "cglHitAssociate Parameters:\n";
  cout << "  Verbose = " << Verbose << endl;
  cout << "  TrackModelFlag = " << TrackModelFlag << endl;
  cout << "  PredictMomentum = " << PredictMomentum << endl;
  cout << "  TECZ0Buffer = " << TECZ0Buffer << endl;
  cout << "  TECSlopeCut = " << TECSlopeCut << endl;
  cout << "  RemoveHits = " << RemoveHits << endl;
  cout << "  MinPhiWidth = " << MinPhiWidth << endl;
  cout << "  MinZWidth = " << MinZWidth << endl;
  cout << "  MinDchQuality = " << MinDchQuality << endl;
  cout << "  PhiRoadOnly = " << PhiRoadOnly << endl;
  cout << "  MaxDchQuality = " << MaxDchQuality << endl;
  cout << "  PC1: UseFlag = " << UseFlag[2] << ", PhiRoad = " <<
    PhiRoad[2] << ", ZRoad = " << ZRoad[2] << endl;
  cout << "  PC2: UseFlag = " << UseFlag[3] << ", PhiRoad = " <<
    PhiRoad[3] << ", ZRoad = " << ZRoad[3] << endl;
  cout << "  PC3: UseFlag = " << UseFlag[4] << ", PhiRoad = " <<
    PhiRoad[4] << ", ZRoad = " << ZRoad[4] << endl;
  cout << "  CRK: UseFlag = " << UseFlag[5] << ", PhiRoad = " <<
    PhiRoad[5] << ", ZRoad = " << ZRoad[5] << endl;
  cout << "  TEC: UseFlag = " << UseFlag[6] << ", PhiRoad = " <<
    PhiRoad[6] << ", ZRoad = " << ZRoad[6] << endl;
  cout << "  TOF: UseFlag = " << UseFlag[7] << ", PhiRoad = " <<
    PhiRoad[7] << ", ZRoad = " << ZRoad[7] << endl;
  cout << "  PbSc: UseFlag = " << UseFlag[8] << ", PhiRoad = " <<
    PhiRoad[8] << ", ZRoad = " << ZRoad[8] << endl;
  cout << "  PbGl: UseFlag = " << UseFlag[9] << ", PhiRoad = " <<
    PhiRoad[9] << ", ZRoad = " << ZRoad[9] << endl;
  cout << "  Acc: UseFlag = " << UseFlag[12] << ", PhiRoad = " <<
    PhiRoad[12] << ", ZRoad = " << ZRoad[12] << endl;
  cout << "  Tofw:UseFlag = " << UseFlag[13] << ", PhiRoad = " <<
    PhiRoad[13] << ", ZRoad = " << ZRoad[13] << endl;
  for(int ilayer=0; ilayer<SVXLAYERNUMBER; ilayer++){
    cout << "  SVX: UseFlag = " << UseFlag[14+ilayer] << ", PhiRoad = " <<
      PhiRoad[14+ilayer] << ", ZRoad = " << ZRoad[14+ilayer] << endl;
  }
  cout << "  Hbd:UseFlag = " << UseFlag[18] << ", PhiRoad = " <<
    PhiRoad[18] << ", ZRoad = " << ZRoad[18] << endl;
  cout << endl;
}

short cglHitAssociate::get_UseFlag(short detid)
{
  return UseFlag[detid];
}
void cglHitAssociate::set_UseFlag(short detid, short value)
{
  UseFlag[detid] = value ;
}

