////////////////////////////////////////////////////////////////////////
//
// Written by Jane Burward-Hoy and Steve Johnson
// Module that uses weighted interpolation between values in a
// field integral look-up table in an ascii file to fit tracks from
// the candidatory
//
// What this module does in mPHDchTrackModel::event(PHCompositeNode *)
//   o searches for dDchHit, dDchTracks, VtxOut and
//     cglDetectorGeo nodes in node tree
//   o reads vertex from VtxOut object in the node tree (can also determine
//     the vertex (either bbc, zdc, mvd, or 0) but this is currently not used).
//   o X1 and X2 hits only:  constructs a list of PHLine from the dDchhit
//     table entries
//   o constructs a list of PHDchTrack (using constructor that takes a
//     track index, pointer to tracks table, associated X hits, vertex,
//     and cglDetectorGeo pointer
//   o fits each track by calling PHDchTrack::fitTrack()
//   o writes out theta0, phi0, and momentum into dDchTracks table
//   o writes out PHDchTrack list as a temporary node in the node tree
//     under PHTRACK subnode.
//   o writes PHDchTrack information and projections into the dPHDchTrack table
//     (creates this table if it does not find it in the node tree)
//
// Changes made as of May 23, 2001 by Jane Burward-Hoy
//  Included function getVertexFromVtxOutClass(dstNode) that sets the
//  vertex in mPHDchTrackModel.  The determineVertex(topNode) is no longer
//  used.
//
// Changes made as of May 15, 2001 by Jane Burward-Hoy:
//  Included ZDC vertex.
//
//
// Changes made as of 11/02/00 by Jane Burward-Hoy:
//
// 1.  The dDchTracks table has the PC1 hit stored in the associated hit array.
//     Modified the search for X1 and X2 associated hits so exclude the PC1
//     hit
//
// Changes made as of 08/09/00 by Jane Burward-Hoy:
//
// 1.  Removed dependency on DchLine class which will be eliminated from the
//     Dch library.
// 2.  Modified how X1 and X2 hits are extracted from the table (indexing
//     is different from original mNewDchTracker).
// 3.  The X1 and X2 hit collection for each tracks now occurs outside of
//     PHDchTrack.  These hits are stored in a PHPointerList<PHLine>.
// 4.  Implemented a flag to go between the tracker and the candidatory.
//
// Changes made as of 08/03/00 by Jane Burward-Hoy:
//
// 1.  Searched for and corrected minor bugs (angle calculations)
// 2.  Made mPHDchTrackModel more flexible without having to recompile the
//     code by using flags in the default constructor (in the ini file).  The
//     default (if no flags are set) is 4 iterations of robust fitting.
// 3.  Took an annoying error statement out of a loop.
// 4.  The model no longer writes over the track's basepoint, alpha, and
//     phi values (it only modifies beta).
// 5.  Modified the set_* and get_* to set* and get* in mPHDchTrackModel.
//     I am also printing out more information that is useful for debugging
//     purposes when the verbosity is set to a value greater than 10.
// 6.  The model is now more intelligent with the vertex selection.  It
//     sets the bbc vertex to -40 if the bbc vertex is < -40 and sets
//     it to +40 if the bbc vertex is > +40 with a warning statement when
//     it does so.  If the bbc vertex is <-9000, then it returns false (and
//     sets the vertex to 0).

#include "mPHDchTrackModel.hh"

#include <dDchHitWrapper.h>
#include <dDchTracksWrapper.h>
#include "dPHDchTrackWrapper.h"

#include "PHDchTrack.hh"

#include <VtxOut.h>
#include <DchTrack.h>

#include <getClass.h>

#include <iostream>
#include <limits>

using namespace std;

mPHDchTrackModel::mPHDchTrackModel()
{
  // verbosity flag
  verbose = 0;
  // for debugging and verbosity level
  failedCount = 0;

  // event vertex
  vertex.setX(0.0);
  vertex.setY(0.0);
  vertex.setZ(0.0);
  vertexErr.setX(0.0);
  vertexErr.setY(0.0);
  vertexErr.setZ(0.0);

  // pointerlists used
  hits = new PHPointerList<PHLine>;
  tracks = 0; // initialized in event(PHCompositeNode)

  qualityThresh = 0; // track quality threshold

  robust = 1; // robust fit to hits (least-sqrs fit if 0)
  iterations = 4; // number of iterations
  successfulIterations = -1;

  // pointers initialized in event(PHCompositeNode)
  projWrapper = 0;
  hitWrapper = 0;
  trkWrapper = 0;
  PHcglDetGeo = 0;
  vtxout = 0;
}

mPHDchTrackModel::~mPHDchTrackModel()
{
  delete hits;
}


int
mPHDchTrackModel::ResetEvent(PHCompositeNode *topNode)
{
  if (tracks)
    {
      tracks->clearAndDestroy();
    }
  hits->clearAndDestroy();
  return 0;
}

PHBoolean mPHDchTrackModel::event(PHCompositeNode *topNode)
{
  PHNodeIterator nodeIter(topNode);

  projWrapper = findNode::getClass<dPHDchTrackWrapper>(topNode, "dPHDchTrack");
  if (!projWrapper)
    {
      cerr << PHWHERE << " Could not find dPHDchTrack Node" << endl;
      exit(1);
    }


  PHcglDetGeo = findNode::getClass<cglDetectorGeo>(topNode,  "cglDetectorGeo");
  if (!PHcglDetGeo)
    {
      cerr << PHWHERE << " Could not find cglDetectorGeo Node" << endl;
      exit(1);
    }
  hitWrapper = findNode::getClass<dDchHitWrapper>(topNode, "dDchHit");
  if (!hitWrapper)
    {
      PHMessage("mPHDchTrackModel::event", PHError, "Did not find dDchHitWrapper in node tree: model cannot proceed");
      exit(1);
    }

  trkWrapper = findNode::getClass<dDchTracksWrapper>(topNode,  "dDchTracks");
  if (!trkWrapper)
    {
      PHMessage("mPHDchTrackModel::event", PHError, "Did not find dDchTracksWrapper in node tree: model cannot proceed");
      exit(1);
    }

  PHCompositeNode *trackNode = static_cast<PHCompositeNode*>(nodeIter.findFirst("PHCompositeNode", "PHTRACK"));
  if (!trackNode)
    {
      trackNode = new PHCompositeNode("PHTRACK");
      topNode->addNode(trackNode);
    }

  PHDataNode<PHPointerList<PHDchTrack> >* trackListNode = (PHDataNode<PHPointerList<PHDchTrack> >* )nodeIter.findFirst("PHDataNode", "PHDchTrackList");
  if (!trackListNode)
    {
      tracks = new PHPointerList<PHDchTrack>(trkWrapper->MaxRowCount());
      trackListNode = new PHDataNode<PHPointerList<PHDchTrack> >(tracks, "PHDchTrackList");
      trackNode->addNode(trackListNode);
    }
  else
    {
      tracks = trackListNode->getData();
    }


  if (!getVertexFromVtxOutClass(topNode))
    {
      return 0;
    }

  fitTracks();
  flattenTheProjections();


  //fill DchTrackOut table
  DchTrack* trktable = findNode::getClass<DchTrack>(topNode, "DchTrack");

  if (trkWrapper && trktable)
    {
      for (unsigned int i = 0;i < trkWrapper->RowCount();i++)
        {
          trktable->set_phi0(i, trkWrapper->get_phi0(i));
          trktable->set_theta0(i, trkWrapper->get_theta0(i));
          trktable->set_momentum(i, trkWrapper->get_momentum(i));
        }
    }

  return 1;
}

void
mPHDchTrackModel::flattenTheProjections()
{
  if (!projWrapper)
    {
      return ;
    }

  for (unsigned int k = 0; k < projWrapper->MaxRowCount(); k++)
    {
      projWrapper->set_id(k, 0);
      projWrapper->set_arm(k, 0);
      projWrapper->set_side(k, 0);
      projWrapper->set_charge(k, 0);
      projWrapper->set_numberOfX1X2hitsFitted(k, 0);
      projWrapper->set_numberOfSuccessfulIterations(k, 0);
      projWrapper->set_chi2(k, 0.0);
      projWrapper->set_ErrorCode(k, 0);
      projWrapper->set_momentum(k, 0.0);
      projWrapper->set_alpha(k, 0.0);
      projWrapper->set_zed(k, 0.0);
      projWrapper->set_fittedAlpha(k, 0.0);
      projWrapper->set_fittedPhi(k, 0.0);
      projWrapper->set_fittedPhi0(k, 0.0);
      projWrapper->set_fittedBeta(k, 0.0);
      projWrapper->set_fittedTheta0(k, 0.0);
      projWrapper->set_vertex(0, k, 0.0);
      projWrapper->set_vertex(1, k, 0.0);
      projWrapper->set_vertex(2, k, 0.0);
      projWrapper->set_predictMomentum(0, k, 0.0);
      projWrapper->set_predictMomentum(1, k, 0.0);
      projWrapper->set_predictMomentum(2, k, 0.0);
      projWrapper->set_projectToVertex(0, k, 0.0);
      projWrapper->set_projectToVertex(1, k, 0.0);
      projWrapper->set_projectToVertex(2, k, 0.0);
      projWrapper->set_projectToPc1(0, k, 0.0);
      projWrapper->set_projectToPc1(1, k, 0.0);
      projWrapper->set_projectToPc1(2, k, 0.0);
      projWrapper->set_projectToPc2(0, k, 0.0);
      projWrapper->set_projectToPc2(1, k, 0.0);
      projWrapper->set_projectToPc2(2, k, 0.0);
      projWrapper->set_projectToPc3(0, k, 0.0);
      projWrapper->set_projectToPc3(1, k, 0.0);
      projWrapper->set_projectToPc3(2, k, 0.0);
      projWrapper->set_projectToCrk(0, k, 0.0);
      projWrapper->set_projectToCrk(1, k, 0.0);
      projWrapper->set_projectToCrk(2, k, 0.0);
      projWrapper->set_projectToTec(0, k, 0.0);
      projWrapper->set_projectToTec(1, k, 0.0);
      projWrapper->set_projectToTec(2, k, 0.0);
      projWrapper->set_projectToTof(0, k, 0.0);
      projWrapper->set_projectToTof(1, k, 0.0);
      projWrapper->set_projectToTof(2, k, 0.0);
      projWrapper->set_projectToPbSc(0, k, 0.0);
      projWrapper->set_projectToPbSc(1, k, 0.0);
      projWrapper->set_projectToPbSc(2, k, 0.0);
      projWrapper->set_projectToPbGl(0, k, 0.0);
      projWrapper->set_projectToPbGl(1, k, 0.0);
      projWrapper->set_projectToPbGl(2, k, 0.0);
    }

  int counter = 0;
  PHDchTrack* mytrack;
  PHLine lineProjected;
  PHPoint errProjected;
  PHPoint pointProj;

  for (unsigned int i = 0; i < tracks->length(); i++)
    {
      mytrack = (*tracks)[i];
      if (!mytrack)
        {
          continue;
        }
      counter++;
      projWrapper->SetRowCount(counter);

      projWrapper->set_id(i, mytrack->getTrackIndex());
      projWrapper->set_arm(i, mytrack->getArm());
      projWrapper->set_side(i, mytrack->getSide());
      projWrapper->set_charge(i, mytrack->getCharge());
      projWrapper->set_numberOfX1X2hitsFitted(i, mytrack->getNumberOfFittedHits());
      projWrapper->set_numberOfSuccessfulIterations(i, mytrack->getSuccessfulIterations());
      projWrapper->set_chi2(i, mytrack->getChi2());
      if (mytrack->getFitError())
        {
          if (mytrack->ifMethodGuess() && mytrack->ifTheta0Error())
            {
              projWrapper->set_ErrorCode(i, mytrack->getFitPErr() + mytrack->getFitThet0Err());
            }
          else if (mytrack->ifTheta0Error())
            {
              projWrapper->set_ErrorCode(i, mytrack->getFitThet0Err());
            }
          else if (mytrack->ifMethodGuess())
            {
              projWrapper->set_ErrorCode(i, mytrack->getFitPErr());
            }
        }
      else
        {
          projWrapper->set_ErrorCode(i, 0);
        }

      projWrapper->set_momentum(i, mytrack->getFitP());
      projWrapper->set_alpha(i, mytrack->getAlpha());
      projWrapper->set_zed(i, mytrack->getTrack().getBasepoint().getZ());
      projWrapper->set_fittedAlpha(i, mytrack->getFitAlpha());
      projWrapper->set_fittedPhi(i, mytrack->getFitPhi());
      projWrapper->set_fittedPhi0(i, mytrack->getFitPhi0());
      projWrapper->set_fittedBeta(i, mytrack->getFitBeta());
      projWrapper->set_fittedTheta0(i, mytrack->getFitTheta0());

      projWrapper->set_vertex(0, i, mytrack->getVertex().getX());
      projWrapper->set_vertex(1, i, mytrack->getVertex().getY());
      projWrapper->set_vertex(2, i, mytrack->getVertex().getZ());

      PHPoint p = mytrack->predictMomentum();
      projWrapper->set_predictMomentum(0, i, p.getX());
      projWrapper->set_predictMomentum(1, i, p.getY());
      projWrapper->set_predictMomentum(2, i, p.getZ());

      long count = 0;
      for (unsigned int j = 0; j < mytrack->getIfIntersectLength(); j++)
        {
          pointProj.setX(0.0);
          pointProj.setY(0.0);
          pointProj.setZ(0.0);
          if (mytrack->getIfIntersectFlag(j))
            {
              if (j == 0)
                {
                  // Vertex
                  pointProj = mytrack->getProjectionPoint(count);
                  projWrapper->set_projectToVertex(0, i, pointProj.getX());
                  projWrapper->set_projectToVertex(1, i, pointProj.getY());
                  projWrapper->set_projectToVertex(2, i, pointProj.getZ());
                  count++;
                }
              if (j == 1)
                {
                  // DCH
                  count++;
                }
              if (j == 2)
                {
                  // pc1
                  pointProj = mytrack->getProjectionPoint(count);
                  projWrapper->set_projectToPc1(0, i, pointProj.getX());
                  projWrapper->set_projectToPc1(1, i, pointProj.getY());
                  projWrapper->set_projectToPc1(2, i, pointProj.getZ());
                  count++;
                }
              if (j == 3)
                { // pc2
                  pointProj = mytrack->getProjectionPoint(count);
                  projWrapper->set_projectToPc2(0, i, pointProj.getX());
                  projWrapper->set_projectToPc2(1, i, pointProj.getY());
                  projWrapper->set_projectToPc2(2, i, pointProj.getZ());
                  count++;
                }
              if (j == 4)
                { // pc3
                  pointProj = mytrack->getProjectionPoint(count);
                  projWrapper->set_projectToPc3(0, i, pointProj.getX());
                  projWrapper->set_projectToPc3(1, i, pointProj.getY());
                  projWrapper->set_projectToPc3(2, i, pointProj.getZ());
                  count++;
                }
              if (j == 5)
                { // crk
                  pointProj = mytrack->getProjectionPoint(count);
                  projWrapper->set_projectToCrk(0, i, pointProj.getX());
                  projWrapper->set_projectToCrk(1, i, pointProj.getY());
                  projWrapper->set_projectToCrk(2, i, pointProj.getZ());
                  count++;
                }
              if (j == 6)
                { // tec
                  pointProj = mytrack->getProjectionPoint(count);
                  projWrapper->set_projectToTec(0, i, pointProj.getX());
                  projWrapper->set_projectToTec(1, i, pointProj.getY());
                  projWrapper->set_projectToTec(2, i, pointProj.getZ());
                  count++;
                }
              if (j == 7)
                { // tof
                  pointProj = mytrack->getProjectionPoint(count);
                  projWrapper->set_projectToTof(0, i, pointProj.getX());
                  projWrapper->set_projectToTof(1, i, pointProj.getY());
                  projWrapper->set_projectToTof(2, i, pointProj.getZ());
                  count++;
                }
              if (j == 8)
                { // pbsc
                  pointProj = mytrack->getProjectionPoint(count);
                  projWrapper->set_projectToPbSc(0, i, pointProj.getX());
                  projWrapper->set_projectToPbSc(1, i, pointProj.getY());
                  projWrapper->set_projectToPbSc(2, i, pointProj.getZ());
                  count++;
                }
              if (j == 9)
                {  // pbgl
                  pointProj = mytrack->getProjectionPoint(count);
                  projWrapper->set_projectToPbGl(0, i, pointProj.getX());
                  projWrapper->set_projectToPbGl(1, i, pointProj.getY());
                  projWrapper->set_projectToPbGl(2, i, pointProj.getZ());
                  count++;
                }
            }
        }
    }
}

PHBoolean
mPHDchTrackModel::getVertexFromVtxOutClass(PHCompositeNode* topNode)
{

  vtxout = findNode::getClass<VtxOut>(topNode,"VtxOut") ;
  if (vtxout)
    {
      if (vtxout->isValid())
        {
          // track model currently does not use the error
          vertex = vtxout->get_Vertex();
          vertexErr = vtxout->get_VertexError();
          return True;
        }
      else
        {
          return False;
        }
    }
  else
    {
      PHMessage("mPHDchTrackModel::event", PHError, "Did not find VtxOut in node tree!! Exiting. . .");
      return False;
    }
}

void
mPHDchTrackModel::getAllHits()
{
  PHLine *phline;

  if (hitWrapper)
    {
      // build hits list for event
      hits->clearAndDestroy();
      for (long i = 0; i < (long) hitWrapper->RowCount(); i++)
        {
          PHPoint p(hitWrapper->get_xyz(0, i),
                    hitWrapper->get_xyz(1, i),
                    hitWrapper->get_xyz(2, i));
          PHVector v(hitWrapper->get_vxyz(0, i),
                     hitWrapper->get_vxyz(1, i),
                     hitWrapper->get_vxyz(2, i));
          phline = new PHLine(p, v);
          if (!hits->append(phline))
            {
              PHMessage("mPHDchTrackModel::fillHitTableList", PHError,
                        "cannot append to hit list!");
            }
        }
    }
  else
    {
      PHMessage("mPHDchTrackModel::fillHitTableList", PHError,
                "dDchHitWrapper node is null!");
    }
}

PHBoolean
mPHDchTrackModel::getX1andX2Hits(long &trkIndex)
{
  PHLine *phline;

  if (hitWrapper && trkWrapper)
    {
      // build hits list for event
      hits->clearAndDestroy();
      short wireTypeIndex, hitIndex = -1;
      for (long j = 0; j < numberOfPlanes - 1; j++)
        {
          // last plane is pc1 hit
          hitIndex = trkWrapper->get_hits(j, trkIndex);
          if (hitIndex > -1)
            {
              wireTypeIndex = hitWrapper->get_plane(hitIndex);
              if ( (wireType[wireTypeIndex] == X1Wire) ||
                   (wireType[wireTypeIndex] == X2Wire) )
                {

                  PHPoint p(hitWrapper->get_xyz(0, hitIndex),
                            hitWrapper->get_xyz(1, hitIndex),
                            hitWrapper->get_xyz(2, hitIndex));

                  PHVector v(hitWrapper->get_vxyz(0, hitIndex),
                             hitWrapper->get_vxyz(1, hitIndex),
                             hitWrapper->get_vxyz(2, hitIndex));
                  phline = new PHLine(p, v);
                  if (!hits->append(phline))
                    {
                      PHMessage("mPHDchTrackModel::fillHitTableList", PHError,
                                "cannot append to hit list!");
                    }
                }
            }
        }
      if (hits->length() == 0)
        {
          return False;
        }
      return True;
    }
  return False;
}

void
mPHDchTrackModel::print(long i)
{
  if (i < (long) trkWrapper->RowCount())
    {
      cout << "**** " << " Track " << i << " out of " << trkWrapper->RowCount() << " ****" << endl;
      cout << "PHDchTrack model:" << endl;
      cout << "  track quality = " << trkWrapper->get_quality(i) << endl;
      cout << "  used vertex = " << vertex << endl;
      cout << "  used alpha (p guess) = " << trkWrapper->get_alpha(i) << endl;


      cout << "  p (GeV/c) = " << trkWrapper->get_momentum(i) << endl;
      cout << "  phi (rad) (not changed) = " << trkWrapper->get_phi(i) << endl;
      cout << "  phi0 (rad) = " << trkWrapper->get_phi0(i) << endl;
      cout << "  theta0 (rad) = " << trkWrapper->get_theta0(i) << endl;
      cout << "  beta (rad) = " << trkWrapper->get_beta(i) << endl;
      cout << "  betaNoVertex (rad) (not changed) = " << trkWrapper->get_betaNoVertex(i) << endl;
    }
}

void
mPHDchTrackModel::echoVerboseLevels()
{
  // verbose levels
  //   0 --> nothing
  //   1 --> failed track error messages
  //   2 --> first five tracks fit results
  //   3 --> first five tracks fit results and projections

  cout << "use mPHDchTrackModel::setVerbose(i) where i = " << endl;
  cout << "0 --> nothing" << endl;
  cout << "1 --> failed track error messages" << endl;
  cout << "2 --> first five track fit results" << endl;
  cout << "3 --> first five track fit results and projections" << endl;
  cout << "current verbose level = " << verbose << endl;
  cout << "default level is 0" << endl;
}

void
mPHDchTrackModel::fitTracks()
{
  // o  uses tracker's alpha and tracker's theta as initial
  //    guesses for momentum and theta0
  // o  currently set to single iteration of least sqrs and four iterations
  //    of robust fitting to get the momentum and phi0, then a single
  //    iteration to get theta0
  //
  // Sets the following variables in dDchTracksWrapper:
  // o  phi0
  // o  theta0
  // o  momentum
  // verbose levels
  //   0 --> nothing
  //   1 --> failed track error messages
  //   2 --> first five tracks fit results
  //   3 --> first five tracks fit results and projections

  PHDchTrack* dchtrack;
  if (trkWrapper)
    {
      for (long i = 0; i < (long) trkWrapper->RowCount() ; i++)
        {
          // check quality of track
          if ((trkWrapper->get_quality(i) < qualityThresh)
              || (!getX1andX2Hits(i)))
            {
              continue;
            }
          dchtrack = new PHDchTrack(i, trkWrapper, *hits,
                                    vertex, *PHcglDetGeo);
          if (dchtrack->fitTrack(robust, iterations))
            {
              trkWrapper->set_momentum(i, dchtrack->getFitP());
              trkWrapper->set_phi0(i, dchtrack->getFitPhi0());
              trkWrapper->set_theta0(i, dchtrack->getFitTheta0());
              dchtrack->callProjections();
            }
          else
            {
              if (dchtrack->ifMethodGuess())
                {
                  trkWrapper->set_momentum(i, dchtrack->getMomentumGuess());
                  trkWrapper->set_phi0(i, -std::numeric_limits<double>::infinity());
                }
              else if (dchtrack->ifTheta0Error())
                {
                  trkWrapper->set_momentum(i, dchtrack->getFitP());
                  trkWrapper->set_phi0(i, dchtrack->getFitPhi0());
                }
              else
                {
                  trkWrapper->set_momentum(i, -std::numeric_limits<double>::infinity());
                  trkWrapper->set_phi0(i, -std::numeric_limits<double>::infinity());
                }
              trkWrapper->set_theta0(i, -std::numeric_limits<double>::infinity());

            }
          if (!tracks->append(dchtrack))
            {
              PHMessage("mPHDchTrackModel::fillTrackTableList",
                        PHError, "cannot append PHDchTrack to tracks list!");
            }
        }
    }
  else
    {
      PHMessage("mPHDchTrackModel::fillTrackTableList", PHError,
                "dDchTracksWrapper node is null!");
    }
}

void
mPHDchTrackModel::check(PHCompositeNode *topNode)
{
  PHNodeIterator nodeIter(topNode);

  PHPointerList<PHDchTrack> *tmpTracks;
  PHIODataNode<PHPointerList<PHDchTrack> >* tmpTrackNode = (PHIODataNode<PHPointerList<PHDchTrack> >* )nodeIter.findFirst("PHIODataNode", "PHDchTrackList");
  if (tmpTrackNode)
    {
      tmpTracks = tmpTrackNode->getData();
      PHTrack *testtrack;
      for (unsigned long i = 0; i < (*tmpTracks).length(); i++)
        {
          testtrack = (*tmpTracks)[i];
          int count = 0;
          for (unsigned long k = 0; k < testtrack->getIfIntersectLength(); k++)
            {
              if (testtrack->getIfIntersectFlag(k))
                {
                  switch (k)
                    {
                    case 0:
                      // vertex
                      cout << testtrack->getProjectionPoint(count) << endl;
                      break;
                    case 1:
                      // dch
                      break;
                    case 2:
                      //pc1
                      cout << testtrack->getProjectionPoint(count) << endl;
                      break;
                    case 3:
                      //pc2
                      cout << testtrack->getProjectionPoint(count) << endl;
                      break;
                    case 4:
                      //pc3
                      cout << testtrack->getProjectionPoint(count) << endl;
                      break;
                    case 5:
                      //crk
                      cout << testtrack->getProjectionPoint(count) << endl;
                      break;
                    case 6:
                      //tec
                      cout << testtrack->getProjectionPoint(count) << endl;
                      break;
                    case 7:
                      //tof
                      cout << testtrack->getProjectionPoint(count) << endl;
                      break;
                    case 8:
                      //PbSc
                      cout << testtrack->getProjectionPoint(count) << endl;
                      break;
                    case 9:
                      //PbGl
                      cout << testtrack->getProjectionPoint(count) << endl;
                      break;
                    default:
                      break;
                    }
                  count++;
                }
            }
        }
    }
}

