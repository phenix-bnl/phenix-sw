//  Written  by Federica-Messer (Steve Johnson pioneering)
//
// Description: the algorithm reconstructs geant tracks as they will be
// measured from the detector in the case of an infinite resolution and
// efficiency. The following steps are done in the analysis:
// First, the dghits are sorted in several list each corresponding to a track.
// Second, use the three dim points (dcghit.xyzinglo) to fit 2 2dim lines
// (one on the x-y plane and one on the r-z plane).
// After the intersection of the 3dim line (obtained by the two 2dim lines)
// with the cylinder section describing one arm of the Drift chamber is
// calculated. This intersection point together with the line direction is
// used to calculate the phi,alpha,beta,zed of the track !
// This information can be used to compare directly with the reconstructed
// data. More info can be introduced to describe the original geant info:
// momentum, phi0, theta0, etc...
// Important : the 40 hit ids of the perfect Track are the ids of the dcghit
// and not of the dDchHit !!!
//
// Additions: quality of the Perfect Track is calculated:
// first  bit not set ( <= numberOfFitPoints) (elimated from the list)
// first  bit set (fitting or calcutation performed)
// second bit set (intersection with CylinderSection successful)
// third  bit set (At least 6 X1 planes with a hit and 2 V2 planes with a hit)
// fouth  bit set (momentum at least 200 MeV/c and track originating from the vertex)

#include "mNewDchPerfectTracker.hh"

#include "dDchPerfParWrapper.h"
#include "dcghitWrapper.h"
#include "dDchHitWrapper.h"
#include "dDchTracksWrapper.h"
#include "dDchGhitHitsWrapper.h"
#include "dDchTracksExtWrapper.h"
#include "DchHitLineTablev2.hh"
#include "DchHitLineOutv2.hh"
#include "DchTrack.h"

#include <PHDchGeometryObject.h>

#include <getClass.h>
#include <PHIODataNode.h>
#include <PHGeometry.h>
#include <fkinWrapper.h>

#include <gsl/gsl_fit.h>

#include <iostream>
#include <algorithm>


using namespace std;

typedef PHIODataNode<fkinWrapper> fkinNode_t;
typedef PHIODataNode<dDchHitWrapper> dDchHitNode_t;
typedef PHIODataNode<dDchPerfParWrapper> dDchPerfParNode_t;
typedef PHIODataNode<dcghitWrapper> dcghitNode_t;
typedef PHIODataNode<dDchTracksWrapper> dDchTracksPerfNode_t;
typedef PHIODataNode<dDchGhitHitsWrapper> dDchGhitHitsNode_t;

mNewDchPerfectTracker::mNewDchPerfectTracker()
{
  dchGeometryObject = 0;
  perfectCandidates = 0;
}

mNewDchPerfectTracker::~mNewDchPerfectTracker() {}

PHBoolean mNewDchPerfectTracker::event(PHCompositeNode *root)
{

  PHDataNode<PHPointerList<DchTrackCandidate> >* tmpTrackCandidateNode = 0;
  PHPointerList<PHNode> nodes;
  PHNodeIterator i(root);
  PHNode *n;

  nodes.clear();

  // Insert code here to navigate node hierarchy and find
  // or create specific nodes to pass to physics module...

  tmpTrackCandidateNode = (PHDataNode<PHPointerList<DchTrackCandidate> >*)i.findFirst("PHDataNode", "PerfectDchTrackCandidate");
  perfectCandidates = tmpTrackCandidateNode->getData();
  perfectCandidates->clearAndDestroy();

  dchGeometryObject = findNode::getClass<PHDchGeometryObject>(root, "DchDGO");
  n = i.findFirst("PHIODataNode", "dDchPerfPar");
  nodes.append(n);

  n = i.findFirst("PHIODataNode", "dcghit");
  nodes.append(n);

  n = i.findFirst("PHIODataNode", "dDchTracksPerf");
  nodes.append(n);

  n = i.findFirst("PHIODataNode", "fkin");
  nodes.append(n);

  n = i.findFirst("PHIODataNode", "dDchGhitHits");
  nodes.append(n);
  callPAM(nodes);
  fillDchTracksExtTable(root);
  return True;
}
void mNewDchPerfectTracker::fillDchTracksExtTable(PHCompositeNode *root)
{
  dcghitWrapper * dcghit = findNode::getClass<dcghitWrapper>(root,"dcghit");
  dDchTracksExtWrapper * extperf = findNode::getClass<dDchTracksExtWrapper>(root,"dDchTracksExtPerf");
  dDchTracksWrapper * perf = findNode::getClass<dDchTracksWrapper>(root,"dDchTracksPerf");


  PHPoint point;
  PHCylPoint cylpoint;

  int i, k, hitid;
  int arm;
  float dist1, dist2;
  int nx1, nx2;
  double w1[12], w2[12];
  double x1r[12];
  double x2r[12];
  double y1r[12];
  double y2r[12];
  double r1r[12];
  double r2r[12];
  double p1r[12];
  double p2r[12];
  //  double phi1r[12];
  //  double phi2r[12];
  int multiplicity;

  multiplicity = perf->RowCount();
  extperf ->SetRowCount(multiplicity);
  for ( i = 0; i < multiplicity; i++)
    {
      int odd1, even1;
      int odd2, even2;
      arm  = perf->get_arm(i);
      nx1  = nx2 = 0;
      dist1 = dist2 = 0;
      odd1 = even1 = odd2 = even2 = 0;
      // get the information needed for each hit
      for (k = 0; k < 12; k++)  // perf track table always has the plane right!
        {
          hitid = perf->get_hits(k, i);
          if (hitid > -1)
            {
              point.setX(dcghit->get_xyzinglo(0, hitid));
              point.setY(dcghit->get_xyzinglo(1, hitid));
              point.setZ(dcghit->get_xyzinglo(2, hitid));
              cylpoint = point;
              x1r[nx1] = point.getX();
              y1r[nx1] = point.getY();
              r1r[nx1] = cylpoint.getR();
              w1[nx1] = 0.24;
              p1r[nx1] = k;
              dist1 = dist1 ;
              nx1++;
              if (k % 2)
                {
                  odd1++;
                }
              else
                {
                  even1++;
                }
            }
        }
      for (k = 20; k < 32; k++)
        {
          hitid = perf->get_hits(k, i);
          if (hitid > -1)
            {
              point.setX(dcghit->get_xyzinglo(0, hitid));
              point.setY(dcghit->get_xyzinglo(1, hitid));
              point.setZ(dcghit->get_xyzinglo(2, hitid));
              cylpoint = point;
              x2r[nx2] = point.getX();
              y2r[nx2] = point.getY();
              r2r[nx2] = cylpoint.getR();
              w2[nx2]   = 0.24;
              p2r[nx2] = k;
              dist2 = dist2 ;
              nx2++;
              if (k % 2)
                {
                  odd2++;
                }
              else
                {
                  even2++;
                }
            }
        }
      // fit the hits
      int check1;
      double slope1, intercept1, chi21;
      double sig_slope1, sig_intercept1, corr1;
      int check2;
      double slope2, intercept2, chi22;
      double sig_slope2, sig_intercept2, corr2;
      const size_t stride = 1;

      // fit the radius vs the plane number
      check1 = -1;
      check2 = -1;
      if (nx1 > 1)
        check1 = gsl_fit_wlinear(p1r, stride, w1, stride, r1r, stride,
                                 nx1, &intercept1, &slope1,
                                 &sig_intercept1, &corr1, &sig_slope1,
                                 &chi21);
      if (nx2 > 1)
        check2 = gsl_fit_wlinear(p2r, stride, w2, stride, r2r, stride,
                                 nx2, &intercept2, &slope2,
                                 &sig_intercept2, &corr2, &sig_slope2,
                                 &chi22);

      float rmean1 = 100, rmean2 = 100;
      if (check1 == 0)
        {
          rmean1 = intercept1 + slope1 * 5.5;  // mean radius of the cell
          check1 = gsl_fit_wlinear(x1r, stride, w1, stride, y1r, stride,
                                   nx1, &intercept1, &slope1,
                                   &sig_intercept1, &corr1, &sig_slope1,
                                   &chi21);
        }
      if (check2 == 0)
        {
          rmean2 = intercept2 + slope2 * 5.5;  // mean radius of the cell
          check2 = gsl_fit_wlinear(x2r, stride, w2, stride, y2r, stride,
                                   nx2, &intercept2, &slope2,
                                   &sig_intercept2, &corr2, &sig_slope2,
                                   &chi22);
        }
      float n, m;
      float X_intersection, Y_intersection;
      float X_intersection1, X_intersection2;
      float globalPhi1, tmp1, globalAlpha1;
      float globalPhi2, tmp2, globalAlpha2;

      // evaluate the intersection of the line with the mean radius
      if ( check1 == 0 )
        {
          n = intercept1;
          m = slope1;

          X_intersection1 = (-2 * m * n + sqrt(4 * m * m * n * n - 4 * (1 + m * m) * (n * n - rmean1 * rmean1))) / (2 * (1 + m * m));
          X_intersection2 = (-2 * m * n - sqrt(4 * m * m * n * n - 4 * (1 + m * m) * (n * n - rmean1 * rmean1))) / (2 * (1 + m * m));
          if (fabs(X_intersection1 - x1r[0]) < fabs(X_intersection2 - x1r[0]))
            {
              X_intersection = X_intersection1;
            }
          else
            {
              X_intersection = X_intersection2;
            }
          Y_intersection = m * X_intersection + n;

          globalPhi1 = atan2(Y_intersection, X_intersection);
          if (globalPhi1 < -Pi / 2.0)
            {
              globalPhi1 = globalPhi1 + 2 * Pi;
            }
          if (arm) tmp1 = atan2(slope1, 1);
          else tmp1 = atan2(slope1, -1);
          if (tmp1 < -Pi / 2)
            {
              tmp1 = tmp1 + 2 * Pi;
            }

          globalAlpha1 = globalPhi1 - tmp1;

        }
      else
        {
          globalPhi1 = -100.;
          globalAlpha1 = -100.;
          intercept1 = -100.;
          slope1 = -100.;
          chi21 = -1;
        }

      if ( check2 == 0 )
        {
          n = intercept2;
          m = slope2;
          X_intersection1 = (-2 * m * n + sqrt(4 * m * m * n * n - 4 * (1 + m * m) * (n * n - rmean2 * rmean2))) / (2 * (1 + m * m));
          X_intersection2 = (-2 * m * n - sqrt(4 * m * m * n * n - 4 * (1 + m * m) * (n * n - rmean2 * rmean2))) / (2 * (1 + m * m));
          if (fabs(X_intersection1 - x2r[0]) < fabs(X_intersection2 - x2r[0]))
            {
              X_intersection = X_intersection1;
            }
          else
            {
              X_intersection = X_intersection2;
            }
          Y_intersection = m * X_intersection + n;

          globalPhi2 = atan2(Y_intersection, X_intersection);
          if (globalPhi2 < -Pi / 2.0)
            {
              globalPhi2 = globalPhi2 + 2 * Pi;
            }
          if (arm) tmp2 = atan2(slope2, 1);
          else tmp2 = atan2(slope2, -1);
          if (tmp2 < -Pi / 2)
            {
              tmp2 = tmp2 + 2 * Pi;
            }
          globalAlpha2 = globalPhi2 - tmp2;
        }
      else
        {
          globalPhi2 = -100.;
          globalAlpha2 = -100.;
          intercept2 = -100.;
          slope2 = -100.;
          chi22 = -1;
        }

      int status = 0;
      if (even1 == 0)
        {
          status |= 0x01;
        }
      else if (odd1 == 0)  // if only even
        {
          status |= 0x02;
        }
      if (even2 == 0)   // if only odd
        {
          status |= 0x04;
        }
      else if (odd2 == 0)  // if only even
        {
          status |= 0x08;
        }
      extperf->set_id(i, i);
      extperf->set_status(i, status);
      extperf->set_alpha1(i, globalAlpha1);
      extperf->set_alpha2(i, globalAlpha2);
      extperf->set_chi21(i, chi21);
      extperf->set_chi22(i, chi22);
      extperf->set_nx1hits(i, nx1);
      extperf->set_nx2hits(i, nx2);
    } // loop over candidates
}
PHBoolean mNewDchPerfectTracker::callPAM(PHPointerList<PHNode> &nl)
{

  TABLE_HEAD_ST dDchPerfPar_h;
  DDCHPERFPAR_ST *dDchPerfPar;
  TABLE_HEAD_ST dcghit_h;
  DCGHIT_ST *dcghit;
  TABLE_HEAD_ST dDchTracksPerf_h;
  DDCHTRACKS_ST *dDchTracksPerf;
  TABLE_HEAD_ST fkin_h;
  FKIN_ST *fkin;
  TABLE_HEAD_ST  dDchGhitHits_h;
  DDCHGHITHITS_ST *dDchGhitHits;

  dDchPerfParNode_t*    n1 = static_cast<dDchPerfParNode_t*>(nl[0]);
  dcghitNode_t*         n2 = static_cast<dcghitNode_t*>(nl[1]);
  dDchTracksPerfNode_t* n3 = static_cast<dDchTracksPerfNode_t*>(nl[2]);
  fkinNode_t*           n4 = static_cast<fkinNode_t*>(nl[3]);
  dDchGhitHitsNode_t*   n5 = static_cast<dDchGhitHitsNode_t*>(nl[4]);

  dDchPerfPar_h    = n1->getData()->TableHeader();
  dDchPerfPar      = n1->getData()->TableData();
  dcghit_h         = n2->getData()->TableHeader();
  dcghit           = n2->getData()->TableData();
  dDchTracksPerf_h = n3->getData()->TableHeader();
  dDchTracksPerf   = n3->getData()->TableData();
  fkin_h           = n4->getData()->TableHeader();
  fkin             = n4->getData()->TableData();
  dDchGhitHits_h   = n5->getData()->TableHeader();
  dDchGhitHits     = n5->getData()->TableData();

  ////////////////////////////////////////////////////////////////////


  int numberOfTracks;
  int numOfGhitHits = dDchGhitHits_h.nok;
  short verbose = dDchPerfPar[0].verbose;
  short localInfoStudy = dDchPerfPar[0].localStudy;

  if (verbose > 10)
    {
      cout << " Tracking Perfectly" << endl;
    }

  PHCylinderSection *cylinder = 0;

  map<const int, SimpleTrack > TrackToHits;


  int totalGeantTracksWithIntersection = 0;

  int numberOfHits;
  int GeantTrackIdForThisHit;

  numberOfHits = dcghit_h.nok;
  if (verbose > 10)
    {
      cout << " HITS " << endl;
      cout << " HITS " << numberOfHits << endl;
    }

  for (int HitIndex = 0; HitIndex < numberOfHits; HitIndex++)   // loop over all the dcghits
    {

      GeantTrackIdForThisHit = dcghit[HitIndex].mctrack;  // get the track id
      IDPoint ThisIDPoint;
      ThisIDPoint.id = HitIndex;
      ThisIDPoint.point.setX(dcghit[HitIndex].xyzinglo[0]);
      ThisIDPoint.point.setY(dcghit[HitIndex].xyzinglo[1]);
      ThisIDPoint.point.setZ(dcghit[HitIndex].xyzinglo[2]);

      if (verbose > 10) cout << GeantTrackIdForThisHit << " " << atan2(dcghit[HitIndex].xyzinglo[1], dcghit[HitIndex].xyzinglo[0]) << endl;

      int tracksize = TrackToHits[GeantTrackIdForThisHit].HitList.size();
      if (tracksize > 0)
        {
          TrackToHits[GeantTrackIdForThisHit].HitList.push_back(ThisIDPoint);
        }
      else
        {
          if (verbose > 10) cout << " adding track  "  << GeantTrackIdForThisHit << endl;

          SimpleTrack NewTrack;
          NewTrack.HitList.push_back(ThisIDPoint);
          TrackToHits[GeantTrackIdForThisHit] = NewTrack;
        }
    }

  dDchTracksPerf_h.nok = 0;
  int totalTracksThrough = 0;
  int totalTracksThroughMomVertex = 0;
  int acceptedTrackIndex = -1;

  // now you have a for each track a list of hit id, that is a list of points belonging to the track


  map<const int, SimpleTrack >::iterator FoundTracksIter;


  if (verbose > 10)
    {
      int trackcheck = 0;
      for (FoundTracksIter = TrackToHits.begin(); FoundTracksIter != TrackToHits.end(); ++FoundTracksIter)
        {
          int geant = FoundTracksIter->first;
          SimpleTrack MyTrack = FoundTracksIter->second;
          cout << " Found Track " << trackcheck++ << " " << geant << " " << MyTrack.HitList.size() << endl;
          deque<IDPoint>::const_iterator MyHitIterator;
          cout << " Hits " << endl;
          for (MyHitIterator = MyTrack.HitList.begin(); MyHitIterator != MyTrack.HitList.end(); ++MyHitIterator)
            {
              cout << atan2( (*MyHitIterator).point.getY(), (*MyHitIterator).point.getX()) << "  ";
            }
          cout << endl;
        }
    }

  for (FoundTracksIter = TrackToHits.begin(); FoundTracksIter != TrackToHits.end(); ++FoundTracksIter)
    {
      SimpleTrack MyTrack = FoundTracksIter->second;
      deque<IDPoint>::const_iterator MyHitIterator;
      int numberOfFitPoints = MyTrack.HitList.size();
      short northSide = 0;
      short southSide = 0;
      short eastArm   = 0;
      short westArm   = 0;
      int hit_of_track_counter = 0;
      unsigned int arm;
      int side;
      double slope, intercept, chi2, s_slope, s_intercept, corr;
      double slopeZ, interceptZ, chi2Z, s_slopeZ, s_interceptZ, corrZ;
      const size_t stride = 1;

      PHBoolean outsideRange = (1 == 1);
      if (numberOfFitPoints <= 2)
        {
          FoundTracksIter->second.TrackLocationInArray = -999;
          continue;
        }

      acceptedTrackIndex++;
      FoundTracksIter->second.TrackLocationInArray = acceptedTrackIndex;
      for (int j = 0; j < numberOfPlanes; j++)
        {
          dDchTracksPerf[acceptedTrackIndex].hits[j] = -1;
        }
      double  x[numberOfFitPoints],
	y[numberOfFitPoints],
	z[numberOfFitPoints],
	w[numberOfFitPoints],
	r[numberOfFitPoints],
	rad[numberOfFitPoints];
      int geantID, geantPlane;

      for (MyHitIterator = MyTrack.HitList.begin(); MyHitIterator != MyTrack.HitList.end(); ++MyHitIterator)
        {
          // fill the array for the fit
          x[hit_of_track_counter] = (*MyHitIterator).point.getX();
          y[hit_of_track_counter] = (*MyHitIterator).point.getY();
          z[hit_of_track_counter] = (*MyHitIterator).point.getZ();
          r[hit_of_track_counter] = sqrt(x[hit_of_track_counter] * x[hit_of_track_counter]
                                         + y[hit_of_track_counter] * y[hit_of_track_counter]);
          rad[hit_of_track_counter] = (double) r[hit_of_track_counter];
          w[hit_of_track_counter] = 1;
          if (x[hit_of_track_counter] >= 0 )
            {
              westArm++;
            }
          else
            {
              eastArm++;
            }
          if (z[hit_of_track_counter] >= 0)
            {
              northSide++;
            }
          else
            {
              southSide++;
            }
          hit_of_track_counter++;

          geantID = (*MyHitIterator).id; // hit index
          geantPlane = dcghit[geantID].plane;
          int hitid = -1;
          int m;
          for (m = 0; m < numOfGhitHits; m++)
            {
              if (geantID == dDchGhitHits[m].ghitid)
                {
                  hitid = dDchGhitHits[m].hitsid;
                  break;
                }
            }
          if ((dDchTracksPerf[acceptedTrackIndex].hits[geantPlane] == -1) || (hitid > -1))
            {
              dDchTracksPerf[acceptedTrackIndex].hits[geantPlane] = geantID;
              // first id that belongs to
              // the plane is remembered
              // overwritten in case of match
            }
        }

      arm = (westArm > eastArm) ? 1 : 0;
      side = (northSide >= southSide) ? 1 : 0;

      dDchTracksPerf[acceptedTrackIndex].trackid =  acceptedTrackIndex;

      dDchTracksPerf[acceptedTrackIndex].arm = arm;
      dDchTracksPerf[acceptedTrackIndex].side = side;
      dDchTracksPerf[acceptedTrackIndex].point[0] = -9999;
      dDchTracksPerf[acceptedTrackIndex].point[1] = -9999;
      dDchTracksPerf[acceptedTrackIndex].point[2] = -9999;
      dDchTracksPerf[acceptedTrackIndex].direction[0] = -9999;
      dDchTracksPerf[acceptedTrackIndex].direction[1] = -9999;
      dDchTracksPerf[acceptedTrackIndex].direction[2] = -9999;
      dDchTracksPerf[acceptedTrackIndex].alpha = -9999;
      dDchTracksPerf[acceptedTrackIndex].beta = -9999;
      dDchTracksPerf[acceptedTrackIndex].betaNoVertex = -9999;
      dDchTracksPerf[acceptedTrackIndex].zed = -9999;
      dDchTracksPerf[acceptedTrackIndex].phi = -9999;
      dDchTracksPerf[acceptedTrackIndex].quality = 0;
      dDchTracksPerf[acceptedTrackIndex].hits[39] = -1; // abused by stony brook to store pc1 hit


      dDchTracksPerf_h.nok++;

      int startIndex;
      if (referenceRadius < rad[0] || referenceRadius > rad[numberOfFitPoints-1])
        {
          outsideRange = True;
          startIndex = 0;
        }
      else
        {
          outsideRange = False;
        }

      startIndex = std::lower_bound(rad,
                                    rad + numberOfFitPoints,
                                    referenceRadius) - rad;

      if (!outsideRange && localInfoStudy)
        {
          if (startIndex == -1) startIndex = 0;
          slope = (float) (y[startIndex] - y[startIndex+1]) / (x[startIndex] - x[startIndex+1]);
          intercept = y[startIndex] - slope * x[startIndex];
          slopeZ = (float) (z[startIndex] - z[startIndex+1]) / (r[startIndex] - r[startIndex+1]);
          interceptZ =  z[startIndex] - slopeZ * r[startIndex];
        }
      else
        {
          gsl_fit_wlinear(x, stride, w, stride, y, stride,
			  numberOfFitPoints, &intercept, &slope,
			  &s_intercept, &corr, &s_slope,
			  &chi2);
          gsl_fit_wlinear(r, stride, w, stride, z, stride,
			  numberOfFitPoints, &interceptZ, &slopeZ,
			  &s_interceptZ, &corrZ, &s_slopeZ,
			  &chi2Z);

          // All tracks that make it this far have fitpoints>2
        }
      dDchTracksPerf[acceptedTrackIndex].quality = 1;

      //------------------------------------------------
      //  build a three dim line (at the moment two dim
      // -----------------------------------------------

      // take a point on the line: x=0, y= slope*x+intercept,
      //                           z = slopeZ*r + interceptZ
      float xtmp = 0;
      float ytmp = slope * xtmp + intercept;
      float ztmp = 0; // slopeZ*(sqrt(xtmp*xtmp + ytmp*ytmp)) + interceptZ;
      // for now is not used
      PHPoint point1OnLine(xtmp, ytmp, ztmp);

      xtmp = 1;
      ytmp = slope * xtmp + intercept;
      ztmp = 0; // slopeZ*(sqrt(xtmp*xtmp + ytmp*ytmp)) + interceptZ;
      // for now is not used
      PHPoint point2OnLine(xtmp, ytmp, ztmp);

      PHLine line((PHPoint)point1OnLine, (PHPoint)point2OnLine);

      // --------------------------------------------
      // intersect the line with the cylinder section
      //---------------------------------------------

      if (arm == WEST)
        {
          cylinder = dchGeometryObject->getWestCylinder();
        }
      else
        {
          cylinder = dchGeometryObject->getEastCylinder();
        }
      PHPoint  p1, p2, pfinal;
      PHVector myvector;

      short intersections = PHGeometry::intersectionLineCylinderSection(line, *cylinder, p1, p2);

      if (intersections)
        {
          // if with intersection then set second quality bit
          dDchTracksPerf[acceptedTrackIndex].quality |= 0x02;

          // if found an intersection of the fitted track with the cylinder
          totalGeantTracksWithIntersection++;
          int closest = 0;
          if (!outsideRange) closest = startIndex;
          PHPoint mypoint(x[closest], y[closest], z[closest]);
          if ((PHGeometry::distancePointToPoint(p1, mypoint) < PHGeometry::distancePointToPoint(p2, mypoint)) ||    (fabs(p2.getX()) < 0.001) )
            {
              pfinal = p1;
            }
          else
            {
              pfinal = p2;
            }

          // sometimes 2 intersections are found! Here, we choose the right one
          xtmp = pfinal.getX();
          ytmp = pfinal.getY();
          pfinal.setZ(slopeZ*sqrt(xtmp*xtmp + ytmp*ytmp) + interceptZ);

          xtmp = pfinal.getX() + pfinal.getX() / fabs(pfinal.getX());
          ytmp = slope * xtmp + intercept;
          ztmp = slopeZ * (sqrt(xtmp * xtmp + ytmp * ytmp)) + interceptZ;

          PHPoint tmp(xtmp, ytmp, ztmp);
          myvector = (PHVector)tmp - (PHVector)pfinal;

          dDchTracksPerf[acceptedTrackIndex].point[0] = pfinal.getX();
          dDchTracksPerf[acceptedTrackIndex].point[1] = pfinal.getY();
          dDchTracksPerf[acceptedTrackIndex].point[2] = pfinal.getZ();

          dDchTracksPerf[acceptedTrackIndex].direction[0] = myvector.getX();
          dDchTracksPerf[acceptedTrackIndex].direction[1] = myvector.getY();
          dDchTracksPerf[acceptedTrackIndex].direction[2] = myvector.getZ();

          dDchTracksPerf[acceptedTrackIndex].phi   = atan2(pfinal.getY(), pfinal.getX());
          if (pfinal.getX() < 0.0 && pfinal.getY() < 0.0)
            {
              dDchTracksPerf[acceptedTrackIndex].phi += TwoPi;
            }

          float phi_1 = dDchTracksPerf[acceptedTrackIndex].phi;
          float phi_2 = atan2(myvector.getY(), myvector.getX());
          if (myvector.getX() < 0.0 && myvector.getY() < 0.0)
            {
              phi_2 += TwoPi;
            }

          dDchTracksPerf[acceptedTrackIndex].alpha = phi_1 - phi_2;
          if ( phi_1 > 0.0 && phi_2 < 0.0 && dDchTracksPerf[acceptedTrackIndex].alpha > Pi )
            {
              dDchTracksPerf[acceptedTrackIndex].alpha -= TwoPi;
            }
          if ( phi_1 < 0.0 && phi_2 > 0.0 && dDchTracksPerf[acceptedTrackIndex].alpha < -1.0*Pi )
            {
              dDchTracksPerf[acceptedTrackIndex].alpha += TwoPi;
            }

          if (dDchTracksPerf[acceptedTrackIndex].alpha < -Pi / 2.)
            {
              dDchTracksPerf[acceptedTrackIndex].alpha = dDchTracksPerf[acceptedTrackIndex].alpha + Pi;
            }
          else if (dDchTracksPerf[acceptedTrackIndex].alpha > Pi / 2.)
            {
              dDchTracksPerf[acceptedTrackIndex].alpha = dDchTracksPerf[acceptedTrackIndex].alpha - Pi;
            }

          dDchTracksPerf[acceptedTrackIndex].beta  = atan2(sqrt(myvector.getX() * myvector.getX()
								+ myvector.getY() * myvector.getY()),
              myvector.getZ());

          dDchTracksPerf[acceptedTrackIndex].zed   = pfinal.getZ();
        }
      else
        {
          //
          // The following printout should be controlled by Verbose or an iDebug option (CFM, 5/31/2005)
          // It is being suppressed temporarily after the first 10 uses
          //
          static int noIntersectionPrint = 0;
          if (noIntersectionPrint < 10)
            {
              noIntersectionPrint++;
              cout << PHWHERE << "NO INTERSECTION *****" << endl;
              if (noIntersectionPrint == 10)
                cout << "\n ****No more of these NO INTERSECTION messages will be printed***\n" << endl;
            } // check on 10 printouts already
        }
    } // end of tracks loop

  numberOfTracks = TrackToHits.size();





  for (int ll = 0; ll < fkin_h.nok; ll++)
    {
      SimpleTrack CurrentTrack = TrackToHits[fkin[ll].true_track];
      TrackToHits.erase(fkin[ll].true_track);
      if (CurrentTrack.HitList.size() <= 2)
        {
          continue;
        }
      int primary = 0;
      int totalPlanes   = 0;
      int totalX1Planes = 0;
      int totalV2Planes = 0;
      acceptedTrackIndex = CurrentTrack.TrackLocationInArray;
      if (acceptedTrackIndex == -999)
        {
          continue;
        }
      dDchTracksPerf[acceptedTrackIndex].theta0 = fkin[ll].pthet;
      dDchTracksPerf[acceptedTrackIndex].phi0 = fkin[ll].pphi;
      dDchTracksPerf[acceptedTrackIndex].momentum = fkin[ll].ptot;
      if (fkin[ll].idparent == 0)
        {
          primary = 1;
        }

      for (int k = 0; k < numberOfPlanes; k++)
        {
          if (dDchTracksPerf[acceptedTrackIndex].hits[k] != -1)
            {
              totalPlanes++;
              if (k < 12) totalX1Planes++;
              if (k > 35) totalV2Planes++;
            }
        }

      if (totalX1Planes >= 6 && totalV2Planes >= 2)
        {
          dDchTracksPerf[acceptedTrackIndex].quality |= 0x04;
          totalTracksThrough++;
          if (dDchTracksPerf[acceptedTrackIndex].momentum >= 0.2 && primary == 1)
            {
              dDchTracksPerf[acceptedTrackIndex].quality |= 0x08;
              totalTracksThroughMomVertex++;
            }
        }
    }
  if (verbose > 10)
    {
      cout << "**************** Drift Chamber Event Summary **************" << endl;
      cout << "total tracks: " << numberOfTracks << endl;
      cout << "tracks with > 2hits: " <<  dDchTracksPerf_h.nok << endl;
      cout << "with intersection: " << totalGeantTracksWithIntersection << endl;
      cout << "total tracks through full DC (>=6 X1 && >=2 V2): " << totalTracksThrough << endl;
      cout << "from vertex with momentum >= 200 MeV/c: " << totalTracksThroughMomVertex << endl;
    }
  n1->getData()->SetRowCount(dDchPerfPar_h.nok);
  n2->getData()->SetRowCount(dcghit_h.nok);
  n3->getData()->SetRowCount(dDchTracksPerf_h.nok);
  n4->getData()->SetRowCount(fkin_h.nok);

  return True;
}

PHBoolean mNewDchPerfectTracker::fillTrackCandidateInfo(DchTrackCandidate *candi, int i, DCGHIT_ST* dcghit)
{
  return True;
}


void
mNewDchPerfectTracker::CopyWrapper(PHCompositeNode *topNode)
{
  DchTrack* dchtrack = findNode::getClass<DchTrack>(topNode, "DchTrack");
  if (!dchtrack)
    {
      cout << "DchTrack Object not found" << endl;
      return;
    }
  dDchTracksWrapper *ddchtracks = findNode::getClass<dDchTracksWrapper>(topNode, "dDchTracksPerf");
  dDchTracksExtWrapper *dchext = findNode::getClass<dDchTracksExtWrapper>(topNode, "dDchTracksExtPerf");
  dchtrack->FillFromWrapper(ddchtracks, dchext);
  DchHitLineTable *intable =  findNode::getClass<DchHitLineTable>(topNode,"DchHitLineTablev1");
  DchHitLineTable *outtable =  findNode::getClass<DchHitLineTable>(topNode,"DchHitLineTable");
  DchHitLineOutv2 shortHit;
  outtable ->Clear(); // just to be save in case any module dumped data here
   for (int i = 0; i < intable->Entries(); i++)
    {
      shortHit.setId(intable->getId(i));
      shortHit.setArm(intable->getArm(i));
      shortHit.setPlane(intable->getPlane(i));
      shortHit.setCell(intable->getCell(i));
      shortHit.setSide(intable->getSide(i));
      shortHit.setTime1(intable->getTime1(i));
      shortHit.setIdmirror(intable->getIdmirror(i));
      shortHit.setWidth(intable->getWidth(i));
      shortHit.setXYZ(intable->getXYZ(i));
      outtable->AddHit(&shortHit);
    }
 
   return;
}

