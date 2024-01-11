#include "PHDchTrack.hh"
#include "PHDchTrackFitter.hh"
#include <gsl/gsl_fit.h>

using namespace std;
using namespace PHGeometry;

// Default constructor not currently used
PHDchTrack::PHDchTrack()
{
  setDefaultPolyOrders();
  successfulIterations = 0;
  tukeyConstant = 3.0;
  numOfHits = 0;
  numFitted = 0;
  tofPathLength = 0.0;
  emcPathLength = 0.0;
  crkPathLength = 0.0;
  tofwPathLength = 0.0;
  pc1sector = -1;
  pc2sector = -1;
  pc3sector = -1;
  for(int ilayer=0; ilayer<SVXLAYERNUMBER; ilayer++){
    svxladder[ilayer] = -1;
  }
  tecSector = -1;
  tofSector = -1;
  pbglSector = -1;
  pbscSector = -1;
  accSector = -1;
  tofwSector = -1;
  hbdSector = -1;
  fitError = False;
  ifGuess = False;
  theta0Error = False;
}

PHDchTrack::PHDchTrack(long &index, dDchTracksWrapper* tracksWrapper,
                       const PHPointerList<PHLine> &DchHits,
                       const PHPoint &vtx,
                       const cglDetectorGeo &cglDetGeo)
{
  numOfHits = 0;
  numFitted = 0;
  successfulIterations = 0;
  tukeyConstant = 3.0;
  
  for (int i=0; i<2*numberOfPlanes; i++)
    {
      weights[i] = 0.0;
    }
   
  DDCHTRACKS_ST* dDchTracks = tracksWrapper->TableData();
  if (index < (long) tracksWrapper->RowCount())
    {
      tracksWrapper->set_momentum(index, 0.); 
      tracksWrapper->set_phi0(index, 0.);     
      tracksWrapper->set_theta0(index, 0.);   

      vertex = vtx;
      copyTrackInfo(dDchTracks[index]);
      copyHitInfo(DchHits);
      setDefaultPolyOrders();
    }
  else
    {
      fitError = True;
      PHMessage("PHDchTrack::PHDchTrack", PHError,
                "FATAL: index >= row count in tracks table");
    }
  detGeo = &(cglDetGeo);
  fitter = PHDchTrackFitter::instance();
}

void
PHDchTrack::copyTrackInfo(const DDCHTRACKS_ST &dDchTrack)
{
  trackIndex = dDchTrack.trackid;
  arm = dDchTrack.arm;
  side = dDchTrack.side;

  double zed = dDchTrack.point[2];
  if (dDchTrack.quality < 4)
    {
      // no z information for track
      zed = 0.0;
    }

  PHPoint point((double)dDchTrack.point[0],
                (double)dDchTrack.point[1],
                zed);
  PHVector direction((double)dDchTrack.direction[0],
                     (double)dDchTrack.direction[1],
                     (double)dDchTrack.direction[2]);
  track.setBasepoint(point);
  direction.normalize();
  track.setDirection(direction);
  PHPoint ref = point - vertex;
  refRadius = sqrt(ref.getX() * ref.getX() + ref.getY() * ref.getY());
  if (refRadius == 0.0)
    {
      PHMessage("PHDchTrack::copyTrackInfo", PHError,
                "basepoint of track is zero!");
    }
  theta = calcTheta(refRadius, ref.getZ());
  alpha = dDchTrack.alpha;
  setCharge();
  setGuessP();
  theta0Guess = theta;
  fitError = False;
  ifGuess = False;
  theta0Error = False;

  if (dDchTrack.momentum < -9000. || dDchTrack.theta0 < -9000.)
    {
      fitError = True;
    }
  fitterTrack = track;
  fitterPhi = dDchTrack.phi;
  fitterP = dDchTrack.momentum;
  fitterBeta = dDchTrack.beta;
  fitterDelta = dDchTrack.beta - theta;
  fitterPhi0 = dDchTrack.phi0;
  fitterTheta0 = dDchTrack.theta0;
  fitterAlpha = dDchTrack.alpha;
}

void
PHDchTrack::setGuessP()
{
  double linFitConst = 0.08735;
  if (alpha != 0.0)
    {
      pGuess = linFitConst / fabs(alpha);
    }
  else
    {
      cout << PHWHERE << " Bad Stony Brook Hack to avoid having to find and fix a dch coding problem leading to tracks with alpha=0";
      cout << ", setting momentum guess to a gazillion" << endl;
      pGuess = 1000000; // TKH - avoid uninitialized variable.
    }
}

void
PHDchTrack::copyHitInfo(const PHPointerList<PHLine> &DchHits)
{
  // Assumes HitPlanes array has been filled in copyTrackInfo()
  numOfHits = DchHits.length();
  double x = 0.0, y = 0.0;
  for (long i = 0; i < numOfHits; i++)
    {
      trackHits[i] = *(DchHits[i]);
      x = trackHits[i].getBasepoint().getX();
      y = trackHits[i].getBasepoint().getY();
      hitR[i] = sqrt(x * x + y * y);
      hitPhi[i] = calcPhi(y, x);
      weights[i] = 1.0;
    }
}

double
PHDchTrack::calcPhi(const double &y, const double &x) const
{
  // Calculates phi in range [-Pi/2,Pi/2] for West arm and
  // [Pi/2,3Pi/2] in East arm

  double phi = atan2(y, x);
  if (phi < -M_PI / 2.0)
    {
      phi = 2 * M_PI + phi;
    }

  return phi;
}

double
PHDchTrack::calcTheta(const double &r, const double &z) const
{
  // Calculates polar angle theta in range [0,Pi]. Assumes r is
  // cylindrical radius that can never be negative
  double theta = atan2(r, z);

  return fabs(theta);
}

double
PHDchTrack::getBeta() const
{
  double beta = -99999.99;
  if (track.getDirection().length() != 0.0)
    {
      beta = acos(track.getDirection().getZ() / track.getDirection().length());
    }
  else
    {
      PHMessage("PHDchTrack::getBeta", PHError,
		"Track direction length is zero!");
    }
  return beta;
}

double
PHDchTrack::calcAlpha(const PHLine &trk) const
{
  double alpha = -9999.99;
  if (trk.getDirection().length() != 0.0)
    {
      PHPoint refR = (PHPoint)(trk.getBasepoint() - vertex);
      PHPoint trkDir = (PHPoint)(trk.getDirection());
      alpha = calcPhi(refR.getY(),
		      refR.getX()) - calcPhi(trkDir.getY(), trkDir.getX());
    }
  else
    {
      PHMessage("PHDchTrack::calcAlpha", PHError, "zero direction vector!!");
    }

  return alpha;
}

void
PHDchTrack::setCharge()
{
  charge = (alpha > 0) ? -1 : 1;
}

void
PHDchTrack::setDefaultPolyOrders()
{
  rOrderF = 8;
  rOrderDelta = 8;
  rOrderG = 8;

  pOrderF = 6;
  pOrderDelta = 4;
  pOrderG = 4;

  theta0OrderF = 6;
  theta0OrderDelta = 4;
  theta0OrderG = 4;

  zOrderF = 6;
  zOrderDelta = 4;
  zOrderG = 4;
}

void
PHDchTrack::setPolyOrders(const int &val)
{
  rOrderF = val;
  rOrderG = val;
  rOrderDelta = val;

  pOrderF = val;
  pOrderG = val;
  pOrderDelta = val;

  zOrderF = val;
  zOrderG = val;
  zOrderDelta = val;

  theta0OrderF = val;
  theta0OrderG = val;
  theta0OrderDelta = val;
}

PHBoolean
PHDchTrack::fitTrack(int &robustFlag, int &maxIterations)
{
  double p, phi0;
  double theta0, beta, delta;

  numFitted = 0;

  int max_Reiterations = 5; // Number of times to loop the P/PHI and THETA calculations...

  // Values for first convergence attempt...
  double pTrial = pGuess;
  double thTrial = theta0Guess;
  int reIter;
  for (reIter = 0; reIter <= max_Reiterations; reIter++)
    {

      PHBoolean pConverge = fitTrackP(robustFlag, maxIterations, pTrial, thTrial, p, phi0);

      if (!pConverge)
        goto pFail;
      fitterP = p;
      fitterPhi0 = phi0;
      ifGuess = False;

      PHBoolean thConverge = fitTheta0(fitterP, thTrial, theta0, beta, delta);
      if (!thConverge)
        goto thFail;

      fitterTheta0 = theta0;
      fitterBeta = beta;
      fitterDelta = delta;
      fitError = False;
      theta0Error = False;

      // Reset initial guess values for the next iteration...
      pTrial = fitterP;
      thTrial = fitterTheta0;
    }

  if (calcFitTrack(fitterTrack))
    {
      double vx = fitterTrack.getDirection().getX();
      double vy = fitterTrack.getDirection().getY();
      fitterPhi = calcPhi(vy, vx);
      fitterAlpha = calcAlpha(fitterTrack);
    }

  return True;

 thFail:  // The theta failed to converge on some iteration...
  fitError = True;
  theta0Error = True;
  return False;

 pFail:   // The main momentum routine failed to converge on some iteration...

  fitError = True;
  ifGuess = True;
  fitterP = pGuess;
  return False;

}

void
PHDchTrack::printProjections() const
{
  for (unsigned int i = 0 ; i < projections.numberOfPoints(); i++)
    {
      PHPoint p = *(projections.getPoint(i));
      PHVector v = *(directions[i]);
      cout << "projection(" << i << ") = " << p;
      cout << "  direction(" << i << ") = " << v << endl;
    }
  for (unsigned int i = 0 ; i < ifIntersect.length(); i++)
    {
      cout << "ifIntersect(" << i << ") = " << *(ifIntersect[i]) << endl;
    }
}

void
PHDchTrack::printPolyLine() const
{
  for (unsigned int i = 0 ; i < polyLine.numberOfPoints(); i++)
    {
      PHPoint p = *(polyLine.getPoint(i));
      cout << "polyLine(" << i << ") = " << p << endl;
    }
}

PHBoolean
PHDchTrack::fitTrackP(const int &robustFlag, const int &maxIterations,
                      const double &pStart, const double &theta0,
                      double &p, double &phi0)
{
  successfulIterations = 0;
  // initialize errFitP and chisqr in case leastSqrsFitHits fails straight away
  errFitP = 2;
  chisqr = 9999.9;
  double thisP, thisPhi0;

  PHBoolean fitOkay = leastSqrsFitHits(pStart, theta0,
                                       thisP, thisPhi0);
  double fitPhi0, fitP;
  if (fitOkay)
    {
      for (int i = 0; i < maxIterations; i++)
        {
          if (robustFlag)
            {
              fitOkay = robustFitHits(thisP, thisPhi0,
                                      theta0Guess, fitP, fitPhi0);
            }
          else
            {
              fitOkay = leastSqrsFitHits(thisP, theta0Guess, fitP, fitPhi0);
            }
          if (fitOkay)
            {
              thisP = fitP;
              thisPhi0 = fitPhi0;
              successfulIterations++;
            }
        }
      p = thisP;
      phi0 = thisPhi0;
    }
  return fitOkay;
}

PHBoolean
PHDchTrack::leastSqrsFitHits(const double &p,
                             const double &theta0,
                             double &fitP,
                             double &fitPhi0)
{
  PHBoolean fitOkay = False;
  numFitted = 0;
  if (numOfHits > 2 && fitter != NULL)
    {
      //float field[2*numberOfPlanes];
      double field[2*numberOfPlanes];
      double fval;

      for (long i = 0; i < numOfHits; i++)
        {
          if (fitter->calcInterpF(vertex.getZ(), zOrderF, theta0, theta0OrderF,
                                  hitR[i], rOrderF, theta, p, pOrderF, fval))
            {
              field[i] = fval;
              weights[i] = 1.0;
              numFitted++;
            }
          else
            {
              weights[i] = 0.0;
            }
        }

      if (numFitted > 2)
        {
          double fslope, fintercept, fchi2, fsig_slope, fsig_intercept;
	  double fcorr;
	  const size_t stride = 1;
	  int check = gsl_fit_wlinear(field,stride,
				      weights,stride,
				      hitPhi,stride,
				      numOfHits,
				      &fintercept,&fslope,
				      &fsig_intercept,&fcorr,&fsig_slope,
				      &fchi2);

          if (check != -1)
            {
              fitP = fabs(1.0 / fslope);
              fitPhi0 = fintercept;
              chisqr = fchi2;
              if (fitPhi0 < -M_PI / 2.0)
                {
                  fitPhi0 = fitPhi0 + 2.0 * M_PI;
                }
              fitOkay = True;
            }
        }
      else
        {
          errFitP = fitter->getErrF();
        }
    }

  return fitOkay;
}

PHBoolean
PHDchTrack::robustFitHits(const double &p,
                          const double &phi0,
                          const double &theta0,
                          double &fitP,
                          double &fitPhi0)
{
  PHBoolean fitOkay = False;
  double res_sq = (0.0150 / 220.0) * (0.0150 / 220.0); // "Known" phi resolution squared.
  if (numOfHits > 2 && fitter != NULL)
    {
      numFitted = 0;
      PHBoolean fit = True;

      double w = 0.0;
      for (long i = 0; i < numOfHits; i++)
        {	  
	  w += weights[i];
        }
      double ww = 0.0;
      double f;
      double dPhi[2*numberOfPlanes];
      //float field[2*numberOfPlanes];
      double field[2*numberOfPlanes];

      for (long i = 0; i < numOfHits; i++)
        {
          if (fitter->calcInterpF(vertex.getZ(), zOrderF, theta0, theta0OrderF,
                                  hitR[i], rOrderF, theta, p, pOrderF, f))
            {
              double phi = phi0 + charge * f / p;
              field[i] = f;
              dPhi[i] = hitPhi[i] - phi;
              ww += weights[i] * (dPhi[i] * dPhi[i]) / w;
            }
          else
            {
              fit = False;
              errFitP = fitter->getErrF();
            }
        }
      ww = max(ww, res_sq);
      if (fit)
        {
          const double cT = tukeyConstant;
          for (long i = 0; i < numOfHits; i++)
            {
              if ( fabs(dPhi[i]) < cT*sqrt(ww) )
                {
                  double val = (dPhi[i]) * (dPhi[i]) / (cT * cT);
                  double diff = 1.0 - val / ww;
                  weights[i] = diff * diff;
                  numFitted++;
                }
              else
                {
                  weights[i] = 0.0;
                }
            }

          double fslope, fintercept, fchi2, fsig_slope, fsig_intercept;
	  double fcorr;
	  const size_t stride = 1;
	  int check = gsl_fit_wlinear(field,stride,
				      weights,stride,
				      hitPhi,stride,
				      numOfHits,
				      &fintercept,&fslope,
				      &fsig_intercept,&fcorr,&fsig_slope,
				      &fchi2);
	  
          if (check != -1)
            {
              fitP = fabs(1 / fslope);
              fitPhi0 = fintercept;
              chisqr = fchi2;
              if (fitPhi0 < -M_PI / 2.0)
                {
                  fitPhi0 = fitPhi0 + 2.0 * M_PI;
                }
              fitOkay = True;
            }
        }
    }

  return fitOkay;
}

PHBoolean
PHDchTrack::fitTheta0(const double &p,
                      const double &theta0,
                      double &fitTheta0,
                      double &fitBeta,
                      double &fitDelta)
{
  PHBoolean ifOkay = True;
  PHBoolean interpDeltaOkay, interpGOkay;
  double gInterp, deltaInterp;

  if (p != 0.0 && fitter != NULL)
    {
      interpDeltaOkay = fitter->calcInterpDelta(vertex.getZ(),
						zOrderDelta,
						theta0, theta0OrderDelta,
						refRadius, rOrderDelta,
						theta,
						p, pOrderDelta, deltaInterp);
      interpGOkay = fitter->calcInterpG(vertex.getZ(), zOrderG,
                                        theta0, theta0OrderG,
                                        refRadius, rOrderG,
                                        theta,
                                        p, pOrderG, gInterp);

      if (interpDeltaOkay && interpGOkay)
        {
          double beta = theta + deltaInterp;
          double r = sin(beta);
          double z = cos(beta);
          fitBeta = calcTheta(r, z);

          fitDelta = deltaInterp;

          double tmpTheta0 = fitBeta - gInterp / p;
          r = sin(tmpTheta0);
          z = cos(tmpTheta0);
          fitTheta0 = calcTheta(r, z);
        }
      else
        {
          errFitTheta0 = fitter->getErrDelta() + fitter->getErrG();
          ifOkay = False;
        }
    }
  else
    {
      ifOkay = False;
    }

  return ifOkay;
}

PHBoolean
PHDchTrack::calcTrackSection(const double &innerR,
                             const double &outerR,
                             const int &numPoints)
{
  // 1.  Calculates points from innerR to outerR using
  //     o  the fitted momentum, phi0, and theta0;
  //     o  the field integral values f, delta, and g;

  // 2.  Appends these points to member polyline pointer polyLine

  PHBoolean okay = True;
  if ((outerR <= innerR) || (numPoints <= 1))
    {
      okay = False;
    }
  else
    {
      double zvtx = vertex.getZ();
      double phi0 = fitterPhi0;
      double trktheta = theta;
      double mom = fitterP;
      double theta0 = fitterTheta0;
      double rMin = innerR;
      double rMax = outerR;

      PHPoint* predictPoint;

      polyLine.clearAndDestroy();

      double r;
      PHPoint p;
      for (int i = 0 ; i < numPoints; i++)
        {
          r = rMin + i * (rMax - rMin) / (numPoints - 1);
          if (calcTrackPoint(zvtx, mom, phi0, r, trktheta, theta0, p))
            {
              predictPoint = new PHPoint(p);
              polyLine.appendPoint(predictPoint);
            }
          else
            {
              okay = False;
            }
        }
    }

  return okay;
}

PHBoolean
PHDchTrack::calcFitTrack(PHLine &trk)
{
  // Calculate the basepoint and direction vector of the track at the
  // reference radius

  PHBoolean fitOkay = False;

  if (refRadius != 0.0)
    {
      double r1 = refRadius - 0.5;
      double r3 = refRadius + 0.5;
      int num = 3;

      PHPoint p1, p2, p3;

      if (calcTrackSection(r1, r3, num) )
        {

          p1 = *(polyLine.getPoint(0));
          p2 = *(polyLine.getPoint(1));
          p3 = *(polyLine.getPoint(2));

          trk.setBasepoint(track.getBasepoint());

          PHVector dir = p3 - p1;
          dir.normalize();

          trk.setDirection(dir);

          fitOkay = True;
        }
    }

  return fitOkay;
}

PHBoolean
PHDchTrack::calcTrackPoint(const double &zvtx,
                           const double &p,
                           const double &phi0,
                           const double &r,
                           const double &theta,
                           const double &theta0,
                           PHPoint &predictPoint)
{
  // Calculates track point at r and theta
  //     o  the fitted momentum, phi0, and theta0;
  //     o  the field integral values f, delta, and g;

  PHBoolean okay = False;
  if (!fitError && fitter)
    {
      double f, g;
      double delta;

      PHBoolean fOkay = fitter->calcInterpF(zvtx, zOrderF,
                                            theta0, theta0OrderF,
                                            r, rOrderF,
                                            theta,
                                            p, pOrderF, f);

      PHBoolean deltaOkay = fitter->calcInterpDelta(zvtx, zOrderDelta,
						    theta0, theta0OrderDelta,
						    r, rOrderDelta,
						    theta,
						    p, pOrderDelta, delta);

      PHBoolean gOkay = fitter->calcInterpG(zvtx, zOrderG,
                                            theta0, theta0OrderG,
                                            r, rOrderG,
                                            theta,
                                            p, pOrderG, g);

      if (fOkay && deltaOkay && gOkay)
        {
          double phiVal = phi0 + charge * f / p;
          double thetaVal = fabs(theta0 - delta + g / p);
          PHCylPoint cylPoint(r, phiVal, zvtx + r / tan(thetaVal));
          predictPoint = cylPoint;
          okay = True;
        }
      else
        {
          if (!fOkay)
            {
              errFitP = fitter->getErrF();
            }
          if (!gOkay)
            {
              errFitTheta0 = fitter->getErrG();
            }
          else if (!deltaOkay)
            {
              errFitTheta0 = fitter->getErrDelta();
            }
          else if (!gOkay && !deltaOkay)
            {
              errFitTheta0 = fitter->getErrG() + fitter->getErrDelta();
            }
        }
    }

  return okay;
}

void
PHDchTrack::calcPolyLine()
{
  calcTrackSection(initialR, lastR, numR);
}

PHVector
PHDchTrack::predictMomentum()
{
  double pt = fitterP * sin(fitterTheta0);
  double pz = fitterP * cos(fitterTheta0);
  double px = pt * cos(fitterPhi0);
  double py = pt * sin(fitterPhi0);

  PHVector vector(px, py, pz);

  return vector;
}

PHBoolean
PHDchTrack::projectToVertex(PHLine &proj, PHPoint &error)
{
  double pt = fitterP * sin(fitterTheta0);
  double pz = fitterP * cos(fitterTheta0);
  double px = pt * cos(fitterPhi0);
  double py = pt * sin(fitterPhi0);

  PHVector vector(px, py, pz);

  PHLine segment;
  segment.setDirection(vector);

  PHVector zVector(0, 0, 1);
  PHLine zAxis(vertex, zVector);

  PHPoint p;

  if (calcTrackPoint(vertex.getZ(), fitterP, fitterPhi0, initialR,
                     theta, fitterTheta0, p))
    {
      segment.setBasepoint(p);
      PHPoint closest = closestApproachLineLine(zAxis, segment);
      proj.setBasepoint(closest);
      proj.setDirection(vector);

      error.setX(0.0);
      error.setY(0.0);
      error.setZ(0.0);

      return True;
    }
  else
    {
      return False;
    }
}

PHBoolean
PHDchTrack::projectToDch(PHLine &proj, PHPoint &error)
{
  PHPoint null(0.0, 0.0, 0.0);

  proj = fitterTrack;
  error = null;

  return True;
}

PHBoolean
PHDchTrack::projectToPc1(PHLine &intersection, PHPoint &error)
{
  PHPoint null(0.0, 0.0, 0.0);
  error = null;

  if (&detGeo != NULL)
    {
      PHBoolean ifFind = False;
      PHPanel pc1[padDetectorGeo::PC1MAXSECTPERARM];

      short nsector = detGeo->get_pc1Geo(arm, pc1);
      double pc2Radius = detGeo->get_pc2Radius();
      if (pc2Radius != 0.0)
        {

          double innerR = refRadius;
          double outerR = pc2Radius;
          int num = 10;

          PHBoolean gotPts = calcTrackSection(innerR, outerR, num);

          PHLine potentialLine;
          if (gotPts)
            {
              for (int sector = 0; sector < nsector; sector++)
                {
                  if (intersectionPolyLinePanel(polyLine,
                                                pc1[sector],
                                                potentialLine))
                    {
                      intersection = potentialLine;
                      pc1sector = sector;
                      ifFind = True;
                      return ifFind; 
                   }
                }
            }
        }
      else
        {
          PHMessage("PHDchTrack::projectToPc1", PHError,
                    "pc1 radius is zero!!");
        }
      return ifFind;
    }
  else
    {
      PHMessage("PHDchTrack::projectToPc1:", PHError, "cglDetectorGeo NULL");
      return False;
    }
}

PHBoolean
PHDchTrack::projectToPc2(PHLine &intersection, PHPoint &error)
{
  PHPoint null(0.0, 0.0, 0.0);
  error = null;

  if (&detGeo != NULL)
    {
      PHBoolean ifFind = False;
      PHPanel pc2[padDetectorGeo::CGLMAXSECTPERARM * 2];

      short nsector = detGeo->get_pc2Geo(arm, pc2);

      double pc1Radius = detGeo->get_pc1Radius();
      double pc3Radius = detGeo->get_pc3Radius();

      if (pc1Radius != 0.0 && pc3Radius != 0.0)
        {
          double innerR = pc1Radius;
          double outerR = pc3Radius;
          int num = 10;

          PHBoolean gotPts = calcTrackSection(innerR, outerR, num);

          PHLine potentialLine;

          if (gotPts)
            {
              for (int sector = 0; sector < nsector; sector++)
                {
                  if (intersectionPolyLinePanel(polyLine,
                                                pc2[sector],
                                                potentialLine))
                    {
                      intersection = potentialLine;
                      pc2sector = sector;
                      ifFind = True;
                      return ifFind; 
                    }
                }
            }
        }
      else
        {
          PHMessage("PHDchTrack::projectToPc2", PHError,
                    "pc2 radius is zero!!");
        }
      return ifFind;
    }
  else
    {
      PHMessage("PHDchTrack::projectToPc2:", PHError, "cglDetectorGeo NULL");
      return False;
    }
}

PHBoolean
PHDchTrack::projectToPc3(PHLine &intersection, PHPoint &error)
{
  PHPoint null(0.0, 0.0, 0.0);
  error = null;

  if (&detGeo != NULL)
    {
      PHBoolean ifFind = False;
      //*2 to allow for db format
      PHPanel pc3[padDetectorGeo::CGLMAXSECTPERARM * 2];
      short nsector = detGeo->get_pc3Geo(arm, pc3);

      double pc2Radius = detGeo->get_pc2Radius();
      double pc3Radius = detGeo->get_pc3Radius();
      if (pc2Radius != 0.0 && pc3Radius != 0.0)
        {

          double innerR = pc2Radius;
          double outerR = pc3Radius + 10.0;
          int num = 10;

          PHBoolean gotPts = calcTrackSection(innerR, outerR, num);
          PHLine potentialLine;
          if (gotPts)
            {
              for (int sector = 0; sector < nsector; sector++)
                {
                  if (intersectionPolyLinePanel(polyLine,
                                                pc3[sector],
                                                potentialLine))
                    {
                      intersection = potentialLine;
                      pc3sector = sector;
                      ifFind = True;
                      return ifFind; 
                    }
                }
            }
        }
      else
        {
          PHMessage("PHDchTrack::projectToPc3", PHError,
                    "pc3 radius is zero!!");
        }
      return ifFind;
    }
  else
    {
      PHMessage("PHDchTrack::projectToPc3:", PHError, "cglDetectorGeo NULL");
      return False;
    }
}

PHBoolean
PHDchTrack::projectToSvx(short ilayer, PHLine &intersection, PHPoint &error)
{
  PHPoint null(0.0, 0.0, 0.0);
  error = null;

  if (&detGeo != NULL)
    {
      PHBoolean ifFind = False;
      PHPanel svx[SVXLADDERNUMBER];
      short nladder = detGeo->get_svxGeo(arm, ilayer, svx);

      double svxRadius = detGeo->get_svxRadius(ilayer);
      if (svxRadius != 0.0)
        {
          //hard code projection range to be +/- 2.0cm
          double innerdiff=2.0, outerdiff=2.0;

          double innerR = svxRadius - innerdiff;
          double outerR = svxRadius + outerdiff;
          int num = 20;

          PHBoolean gotPts = calcTrackSection(innerR, outerR, num);
          PHLine potentialLine;

          if (gotPts)
            {
              for (int ladder = 0; ladder < nladder; ladder++)
                {
                  if (intersectionPolyLinePanel(polyLine,
                                                svx[ladder],
                                                potentialLine))
                    {
                      intersection = potentialLine;
                      svxladder[ilayer] = ladder;
                      ifFind = True;
                      return ifFind; 
                    }
                }
            }
        }
      else
        {
          //PHMessage("PHDchTrack::projectToSvx", PHError,
          //          "svx radius is zero!!");
        }
      return ifFind;
    }
  else
    {
      PHMessage("PHDchTrack::projectToSvx:", PHError, "cglDetectorGeo NULL");
      return False;
    }
}

PHBoolean
PHDchTrack::projectToTec(PHLine &intersection, PHPoint &error)
{
  PHPoint null(0.0, 0.0, 0.0);
  error = null;
  PHBoolean ifFind = False;

  double ref = TECREFERENCERADIUS;
  double innerR = ref - 10.0;
  double outerR = ref + 10.0;
  int num = 4;

  // Create Tec reference cylinder. Accept projections
  // even if they miss the edge of Tec slightly
  // (+- 10cm in Z and +-2 degrees in Phi)
  // By construction, there could be only one intersection of
  // polyline with cylinder section.

  float Zwidth = (TECZWIDTH6 + 20.) / 2.;
  PHPoint center(0., 0., 0.);
  PHVector axis(0., 0., Zwidth);
  PHAngle top((TECPHITOPE - 2.)*M_PI / 180.);
  PHAngle bottom((TECPHIBOTE + 2.)*M_PI / 180.);
  PHCylinderSection tecCylinder(center, ref, axis);
  tecCylinder.setPhiRange(top, bottom);

  PHBoolean gotPts = calcTrackSection(innerR, outerR, num);

  PHLine potentialLine1;
  PHLine potentialLine2;

  if (gotPts)
    {

      if (intersectionPolyLineCylinderSection(polyLine, tecCylinder,
                                              potentialLine1, potentialLine2))
        {
          intersection = potentialLine1;
          ifFind = True;
        }

    }

  return ifFind;

}

PHBoolean
PHDchTrack::projectToPbSc(PHLine &intersection, PHPoint &error)
{
  PHPoint null(0.0, 0.0, 0.0);
  error = null;

  if (&detGeo != NULL)
    {
      PHBoolean ifFind = False;
      PHPanel pbsc[padDetectorGeo::CGLMAXSECTPERARM];
      short nsector = detGeo->get_pbscGeo(arm, pbsc);
      // Need to check radius and get appropriate inner/outer r
      double ref = detGeo->get_pbscRadius();
      if (ref != 0.0)
        {
          double innerR = ref - 40.0;
          double outerR = ref + 20.0;
          int num = 4;

          // want to extrapolate and not interpolate since field is
          // approximately constant at this r
          setPolyOrders(3);
          PHBoolean gotPts = calcTrackSection(innerR, outerR, num);

          // reset polynomial orders to default values
          setDefaultPolyOrders();

          PHLine potentialLine;

          if (gotPts)
            {
              for (int sector = 0; sector < nsector; sector++)
                {
                  if (intersectionPolyLinePanel(polyLine,
                                                pbsc[sector],
                                                potentialLine))
                    {
                      intersection = potentialLine;
                      pbscSector = sector;
                      ifFind = True;
                      return ifFind; 
                    }
                }
            }
        }
      else
        {
          PHMessage("PHDchTrack::projectToPbSc", PHError,
                    "PbSc radius is zero!!");
        }
      return ifFind;
    }
  else
    {
      PHMessage("PHDchTrack::projectToPbSc:", PHError, "cglDetectorGeo NULL");
      return False;
    }
}

PHBoolean
PHDchTrack::projectToPbGl(PHLine &intersection, PHPoint &error)
{
  PHPoint null(0.0, 0.0, 0.0);
  error = null;

  if (&detGeo != NULL)
    {
      PHBoolean ifFind = False;
      PHPanel pbgl[padDetectorGeo::CGLMAXSECTPERARM];
      short nsector = detGeo->get_pbglGeo(arm, pbgl);
      double ref = detGeo->get_pbglRadius();
      if (ref != 0.0)
        {
          // Need to check radius and get appropriate inner/outer r
          double innerR = ref - 40.0;
          double outerR = ref + 20.0;
          int num = 4;

          // Want to extrapolate and not interpolate since field is
          // approximately constant at this r
          setPolyOrders(3);
          PHBoolean gotPts = calcTrackSection(innerR, outerR, num);

          // reset to default polynomial orders
          setDefaultPolyOrders();

          PHLine potentialLine;

          if (gotPts)
            {
              for (int sector = 0; sector < nsector; sector++)
                {
                  if (intersectionPolyLinePanel(polyLine,
                                                pbgl[sector],
                                                potentialLine))
                    {
                      intersection = potentialLine;
                      pbglSector = sector;
                      ifFind = True;
                      return ifFind; 
                    }
                }
            }
        }
      else
        {
          PHMessage("PHDchTrack::projectToPbGl", PHError,
                    "PbGl radius is zero!!");
        }
      return ifFind;
    }
  else
    {
      PHMessage("PHDchTrack::projectToPbGl:", PHError, "cglDetectorGeo NULL");
      return False;
    }
}

PHBoolean PHDchTrack::projectToTof(PHLine &intersection, PHPoint &error)
{
  PHPoint null(0.0, 0.0, 0.0);
  error = null;

  if (&detGeo != NULL)
    {
      PHBoolean ifFind = False;
      PHPanel tof[TOFPANELSNUMBER];
      short nsector = detGeo->get_tofGeo(arm, tof);

      double ref = detGeo->get_tofRadius();

      if (ref != 0.0)
        {

          // Need to check radius and get appropriate inner/outer r
          double innerR = ref - 40.0;
          double outerR = ref + 10.0;
          int num = 4;

          PHBoolean gotPts = calcTrackSection(innerR, outerR, num);

          PHLine potentialLine;

          if (gotPts)
            {

              for (int sector = 0; sector < nsector; sector++)
                {
                  if (intersectionPolyLinePanel(polyLine,
                                                tof[sector],
                                                potentialLine))
                    {
                      intersection = potentialLine;
                      tofSector = sector;
                      ifFind = True;
                      return ifFind; 
                    }
                }
            }
        }
      else
        {
          PHMessage("PHDchTrack::projectToTof", PHError,
                    "tof radius is zero!!");
        }
      return ifFind;
    }
  else
    {
      PHMessage("PHDchTrack::projectToTof:", PHError, "cglDetectorGeo NULL");
      return False;
    }
}

PHBoolean PHDchTrack::projectToAcc(PHLine &intersection, PHPoint &error)
{
  PHPoint null(0.0, 0.0, 0.0);
  error = null;

  if (&detGeo != NULL)
    {
      PHBoolean ifFind = False;
      PHPanel acc[ACCPANELSNUMBER];
      short nsector = detGeo->get_accGeo(arm, acc);

      double ref = detGeo->get_accRadius();

      if (ref != 0.0)
        {

          // Need to check radius and get appropriate inner/outer r
          double innerR = ref - 40.0;
          double outerR = ref + 10.0;
          int num = 4;

          PHBoolean gotPts = calcTrackSection(innerR, outerR, num);

          PHLine potentialLine;

          if (gotPts)
            {
              for (int sector = 0; sector < nsector; sector++)
                {
                  if (intersectionPolyLinePanel(polyLine,
                                                acc[sector],
                                                potentialLine))
                    {
                      intersection = potentialLine;
                      accSector = sector;
                      ifFind = True;
                      return ifFind; 
                    }
                }
            }
        }
      else
        {
          PHMessage("PHDchTrack::projectToAcc", PHError,
                    "acc radius is zero!!");
        }
      return ifFind;
    }
  else
    {
      PHMessage("PHDchTrack::projectToAcc:", PHError, "cglDetectorGeo NULL");
      return False;
    }
}

PHBoolean PHDchTrack::projectToTofw(PHLine &intersection, PHPoint &error)
{
  PHPoint null(0.0, 0.0, 0.0);
  error = null;

  if (&detGeo != NULL)
    {
      PHBoolean ifFind = False;
      PHPanel tofw[TOFWPANELSNUMBER];
      short nsector = detGeo->get_tofwGeo(arm, tofw);

      double ref = detGeo->get_tofwRadius();

      if (ref != 0.0)
        {

          // Need to check radius and get appropriate inner/outer r
          double innerR = ref - 40.0;
          double outerR = ref + 10.0;
          int num = 4;

          PHBoolean gotPts = calcTrackSection(innerR, outerR, num);

          PHLine potentialLine;

          if (gotPts)
            {
              for (int sector = 0; sector < nsector; sector++)
                {
                  if (intersectionPolyLinePanel(polyLine,
                                                tofw[sector],
                                                potentialLine))
                    {
                      intersection = potentialLine;
                      tofwSector = sector;
                      ifFind = True;
                      return ifFind; 
                    }
                }
            }
        }
      else
        {
          PHMessage("PHDchTrack::projectToTofw", PHError,
                    "tofw radius is zero!!");
        }
      return ifFind;
    }
  else
    {
      PHMessage("PHDchTrack::projectToTofw:", PHError, "cglDetectorGeo NULL");
      return False;
    }
}

PHBoolean PHDchTrack::projectToHbd(PHLine &intersection, PHPoint &error)
{
  PHPoint null(0.0, 0.0, 0.0);
  error = null;

  if (&detGeo != NULL)
    {
      PHBoolean ifFind = False;
      PHPanel hbd[HBDPANELSNUMBER];
      short nsector = detGeo->get_hbdGeo(arm, hbd);

      double ref = detGeo->get_hbdRadius();

      if (ref != 0.0)
        {

          // Need to check radius and get appropriate inner/outer r
          double innerR = ref - 3.0;
          double outerR = ref + 3.0;
          int num = 10;

          PHBoolean gotPts = calcTrackSection(innerR, outerR, num);

          PHLine potentialLine;

          if (gotPts)
            {
              for (int sector = 0; sector < nsector; sector++)
                {
                  if (intersectionPolyLinePanel(polyLine,
                                                hbd[sector],
                                                potentialLine))
                    {
                      intersection = potentialLine;
                      hbdSector = sector;
                      ifFind = True;
                      return ifFind; 
                    }
                }
            }
        }
      else
        {
          PHMessage("PHDchTrack::projectToHbd", PHError,
                    "hbd radius is zero!!");
        }
      return ifFind;
    }
  else
    {
      PHMessage("PHDchTrack::projectToHbd:", PHError, "cglDetectorGeo NULL");
      return False;
    }
}


PHBoolean
PHDchTrack::projectToCrk(PHLine &intersection, PHPoint &error)
{
  double ref;
  PHBoolean ifFind = False;
  PHLine line1, line2;
  PHCylinderSection crk[2];
  PHPoint null(0.0, 0.0, 0.0);

  error = null;
  if (&detGeo != NULL)
    {
      ref = detGeo->get_crkRadius();
      if (ref != 0.0)
        {
          double innerR = ref - 10.0;
          double outerR = ref + 10.0;
          int num = 4;
          if (calcTrackSection(innerR, outerR, num) &&
              intersectionPolyLineCylinderSection(polyLine,
                                                  crk[arm],
                                                  line1,
                                                  line2))
            {
              intersection = line1;
              ifFind = True;
            }
        }
      else
        {
          PHMessage("PHDchTrack::projectToCrk", PHError,
                    "crk radius is zero!!");
        }
      return ifFind;
    }
  else
    {
      PHMessage("PHDchTrack::projectToCrk:", PHError, "cglDetectorGeo NULL");
    }

  return ifFind;
}

double PHDchTrack::pathLengthToTof()
{
  // Assumes callProjections has been called already.  Returns 0.0 if
  // no projections are found.

  auto_ptr<PHPoint> vtxPt;
  auto_ptr<PHPoint> tofPt;

  polyLine.clearAndDestroy();

  int index = 0;
  for (unsigned int i = 0; i < getIfIntersectLength(); i++)
    {
      if (getIfIntersectFlag(i))
        {
          switch (i)
            {
            case 0:
              vtxPt.reset(new PHPoint(getProjectionPoint(index)));
              break;
            case 7:
              tofPt.reset(new PHPoint(getProjectionPoint(index)));
              break;
            default:
              break;
            }
          index++;
        }
    }

  if (vtxPt.get() && tofPt.get())
    {
      double r = detGeo->get_tofRadius() - 10.0;

      if (calcTrackSection(initialR, r, numR))
        {
          polyLine.insertPointAt(vtxPt.release(), 0);
          polyLine.appendPoint(tofPt.release());
          tofPathLength = polyLine.arcLength();
          return tofPathLength;
        }
    }

  return 0.0;
}

double
PHDchTrack::pathLengthToEmc()
{
  // Assumes callProjections has been called already. Returns 0.0 if
  // no projections found

  auto_ptr<PHPoint> vtxPt;
  auto_ptr<PHPoint> pbscPt;
  auto_ptr<PHPoint> pbglPt;

  polyLine.clearAndDestroy();

  int index = 0;
  for (unsigned int i = 0; i < getIfIntersectLength(); i++)
    {
      if (getIfIntersectFlag(i))
        {
          switch (i)
            {
            case 0:
              vtxPt.reset(new PHPoint(getProjectionPoint(index)));
              break;
            case 8:
              pbscPt.reset(new PHPoint(getProjectionPoint(index)));
              break;
            case 9:
              pbglPt.reset(new PHPoint(getProjectionPoint(index)));
              break;
            default:
              break;
            }
          index++;
        }
    }

  if (vtxPt.get() && (pbscPt.get() || pbglPt.get()))
    {
      double r;

      if (pbscPt.get())
        {
          r = detGeo->get_pbscRadius() - 10.0;
        }
      if (pbglPt.get())
        {
          r = detGeo->get_pbglRadius() - 10.0;
        }
      if (calcTrackSection(initialR, r, numR))
        {
          polyLine.insertPointAt(vtxPt.release(), 0);
          if (pbscPt.get())
            {
              polyLine.appendPoint(pbscPt.release());
            }
          if (pbglPt.get())
            {
              polyLine.appendPoint(pbglPt.release());
            }
          emcPathLength = polyLine.arcLength();
          return emcPathLength;
        }
    }
  
  return 0.0;
}

double
PHDchTrack::pathLengthToCrk()
{
  // Assumes callProjections has been called already. Returns 0.0 if
  // no projections found

  auto_ptr<PHPoint> vtxPt;
  auto_ptr<PHPoint> crkPt;

  polyLine.clearAndDestroy();

  int index = 0;
  for (unsigned int i = 0; i < getIfIntersectLength(); i++)
    {
      if (getIfIntersectFlag(i))
        {
          switch (i)
            {
            case 0:
              vtxPt.reset(new PHPoint(getProjectionPoint(index)));
              break;
            case 5:
              crkPt.reset(new PHPoint(getProjectionPoint(index)));
              break;
            default:
              break;
            }
          index++;
        }
    }
  if (vtxPt.get() && crkPt.get())
    {
      double r = detGeo->get_crkRadius() - 10.0;

      if (calcTrackSection(initialR, r, numR))
        {
          polyLine.insertPointAt(vtxPt.release(), 0);
          polyLine.appendPoint(crkPt.release());
          crkPathLength = polyLine.arcLength();
          return crkPathLength;
        }
    }

  return 0.0;
}


double PHDchTrack::pathLengthToTofw()
{
  // Assumes callProjections has been called already.  Returns 0.0 if
  // no projections are found.

  auto_ptr<PHPoint> vtxPt;
  auto_ptr<PHPoint> tofwPt;

  polyLine.clearAndDestroy();

  int index = 0;
  for (unsigned int i = 0; i < getIfIntersectLength(); i++)
    {
      if (getIfIntersectFlag(i))
        {
          switch (i)
            {
            case 0:
              vtxPt.reset(new PHPoint(getProjectionPoint(index)));
              break;
            case 13:
              tofwPt.reset(new PHPoint(getProjectionPoint(index)));
              break;
            default:
              break;
            }
          index++;
        }
    }

  if (vtxPt.get() && tofwPt.get())
    {
      double r = detGeo->get_tofwRadius() - 10.0;

      if (calcTrackSection(initialR, r, numR))
        {
          polyLine.insertPointAt(vtxPt.release(), 0);
          polyLine.appendPoint(tofwPt.release());
          tofwPathLength = polyLine.arcLength();
          return tofwPathLength;
        }
    }

  return 0.0;
}

PHBoolean
PHDchTrack::projectToPlane(const PHPlane &plane, PHLine &proj)
{
  calcTrackSection(initialR, lastR, numR);
  return intersectionPolyLinePlane(polyLine, plane, proj);
}

short
PHDchTrack::projectToCylinder(const PHCylinder &cyl,
                              PHLine &proj1, PHLine &proj2)
{
  calcTrackSection(initialR, lastR, numR);
  return intersectionPolyLineCylinder(polyLine, cyl,
                                      proj1, proj2);
}

short
PHDchTrack::projectToSphere(const PHSphere &sphere,
                            PHLine &proj1, PHLine &proj2)
{
  calcTrackSection(initialR, lastR, numR);
  return intersectionPolyLineSphere(polyLine, sphere,
                                    proj1, proj2);
}
