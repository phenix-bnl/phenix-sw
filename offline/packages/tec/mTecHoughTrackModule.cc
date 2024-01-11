#ifndef __MTECHOUGHTRACKMODULE_CC__
#define __MTECHOUGHTRACKMODULE_CC__
//#define DEBUG
#include <mTecHoughTrackModule.h>
#include <TecOutV1.hh>
#include <TecWires.hh>
#include <mTecUtilities.h>

#include <PHNode.h>
#include <PHCompositeNode.h>
#include <PHObject.h>
#include <PHIODataNode.h>
#include <PHTypedNodeIterator.h>
#include <getClass.h>

#include <TROOT.h>
#include <TFile.h>
#include <TH1.h>
#include <TH2.h>
#include <TCanvas.h>
#include <TGraph.h>
#include <TLine.h>
#include <TText.h>
#include <TSystem.h>
#include <TBenchmark.h>
#include <TApplication.h>

#include <gsl/gsl_fit.h>
#include <boost/random.hpp>

#include <cmath>
#include <cstdlib>
#include <cstdio>
#include <iostream>
#include <fstream>
#include <algorithm>
#include <memory>
#include <utility>

using namespace std;
using namespace PHGeometry;

typedef PHIODataNode < TecOutV1 > TecOutNode_t;
typedef PHIODataNode < TObject > ObjectNode_t;
typedef PHIODataNode < PHObject > PHObjectNode_t;

// For each sector/side contains list of hits above LowThreshold.
const int MAXTrkPoints = 50000;
const int MAXListofPoints1 = 1000;
const int MAXListofPoints2 = 1000;
const int MAXBufferPoints = 5000;

static int MinBin[TECMAXINDEX];
static int MaxBin[TECMAXINDEX];
static float SinAlpha[TECMAXINDEX];

static boost::mt19937 rng(0);
static boost::random_number_generator<boost::mt19937> generator(rng);

template <typename T>
static T 
sqr(const T& x)
{
  return x * x;
}

struct trkpoints
{
  int Index;
  int Plane;
  float X;
  float Y;
  float Z;
  short bin;
  short wire;
  float Weight;
  float charge;
  short used;
  int hitnumber;
};

trkpoints  TrkPoints[MAXTrkPoints];

// For every track contains a list of associated hits.
struct list_of_points
{
  short bins[MAXListofPoints1];
  short wires[MAXListofPoints1];
  short plane[MAXListofPoints1];
  float X[MAXListofPoints1];
  float Y[MAXListofPoints1];
  short index;
  float ampl[MAXListofPoints1];
  float charge[MAXListofPoints1];
  int shared[MAXListofPoints1];
  int hitnumber[MAXListofPoints1];
};

list_of_points  List_of_Points[MAXListofPoints2];

// For every sector/side contains list of (possibly rebinned) hits
// above (high) BinThreshold to be used in Hough transform.
struct COORDS
{
  float X;
  float Y;
};
typedef struct COORDS BUFFER[6][MAXBufferPoints];

#ifdef DEBUG
TH2F* hdist[48];
#endif

int
mTecHoughTrackModule::GetClusters2(int *Array,
                                   vector<TecTrackCluster> &ClusterQueue,
                                   int maximum,
                                   vector<float> &slope,
                                   vector<float> &intercept,
                                   vector<float> &erslope,
                                   vector<float> &erint,
                                   vector<float> &chi2,
                                   int &ntrk, int index,
                                   float midphi, int Debug)
{
  // Analyses cluster queue and returns vectors of slopes and
  // intercepts for all found tracks
  vector<float> phitrk, phi0trk, alphatrk, x0trk, y0trk;
  vector<float> tmpbufx, tmpbufy, tmpbufa, tmpbufmax;
  vector<int> xmax, ymax;

  float x1 = y11;
  float x2 = y12;
  float y1 = y21;
  float y2 = y22;
  float dx = x2 - x1;
  float dy = y2 - y1;

  ntrk = 0;
  unsigned int sz = ClusterQueue.size();
  //  std::cout << "ClusterQueue.size() = " << ClusterQueue.size() << std::endl;
  for (unsigned int i = 0; i < sz; i++)
    {
      // Start loop over clusters in queue
      int nmaxima = ClusterQueue[i].getNMax();
      for (int j = 0; j < nmaxima; j++)
        {
          int nadc = ClusterQueue[i].getMax(j);
          ymax.push_back(nadc / m1);
          xmax.push_back(nadc % m1);
        }
      float adcsum = 0.0;
      float xsum = 0.0;
      float x2sum = 0.0;
      float ysum = 0.0;
      float y2sum = 0.0;

      // loop over modules in the cluster
      int nMod = ClusterQueue[i].getNMod();
      for (int j = 0; j < nMod; j++)
        {
          int nadc = ClusterQueue[i].getMod(j);
          int yadc = nadc / m1;
          int xadc = nadc % m1;
          int radc = Array[nadc];

          adcsum += radc;
          xsum += radc * xadc;
          x2sum += radc * xadc * xadc;
          ysum += radc * yadc;
          y2sum += radc * yadc * yadc;
          tmpbufx.push_back(xadc);
          tmpbufy.push_back(yadc);
          tmpbufa.push_back(radc);

        }

      xsum = xsum / adcsum;
      x2sum = x2sum / adcsum;
      ysum = ysum / adcsum;
      y2sum = y2sum / adcsum;
      x2sum = x2sum - xsum * xsum;
      y2sum = y2sum - ysum * ysum;
      if (x2sum == 0.)
        {
          x2sum = 0.25;
        }
      if (y2sum == 0.)
        {
          y2sum = 0.25;
        }

      // keep only clusters with large amplitude and not too large slope
      float ampLim = maximum * clPar[3];
      float angle = ((fabs) (ysum - ((float) (m2)) / 2.)) / ((float) (m2)) * (y2 - y1);

      if ((adcsum > ampLim) && (angle < angLim))
        {
          if ((nmaxima > 1) && (adcsum > (((float)(maximum))*clPar[4])))
            {
              // more than 1 maximum. split cluster
              for (int imax = 0; imax < nmaxima; imax++)
                {
                  phitrk.push_back((xmax[imax] + 0.5)*(dx / ((float)(m1))) + x1 + midphi);
                  float erphi = (fabs) (x2sum * dx / ((float) (m1))) / sqrt (((float) (maximum)));
                  alphatrk.push_back((ymax[imax] + 0.5) * (dy / ((float) (m2))) + y1);
                  float eralpha = (fabs) (y2sum * dy / ((float) (m2))) / sqrt (((float) (maximum)));
                  float erphi0 = sqrt (erphi * erphi + eralpha * eralpha);
                  if (index > 7)
                    {
                      phi0trk.push_back(phitrk[ntrk] - alphatrk[ntrk]);
                      x0trk.push_back(Radius0 * cos (phitrk[ntrk]));
                    }
                  else
                    {
                      phi0trk.push_back(alphatrk[ntrk] - phitrk[ntrk]);
                      x0trk.push_back( -1.0 * Radius0 * cos (phitrk[ntrk]));
                    }
                  float erx0 = (fabs) (Radius0 * sin (phitrk[ntrk]) * erphi);
                  y0trk.push_back(Radius0 * sin (phitrk[ntrk]));
                  float ery0 = (fabs) (Radius0 * cos (phitrk[ntrk]) * erphi);
                  slope.push_back(tan(phi0trk[ntrk]));
                  erslope.push_back(erphi0);
                  intercept.push_back(y0trk[ntrk] - slope[ntrk] * x0trk[ntrk]);
                  erint.push_back(sqrt(ery0*ery0 + erslope[ntrk]*erslope[ntrk] + erx0*erx0));
                  chi2.push_back(0.);
                  ntrk++;
                }

            }
          else
            {
              // one maximum or cluster is not big enough to be split
              phitrk.push_back((xsum + 0.5) * (dx / ((float) (m1))) + x1 + midphi);
              float erphi = (fabs) (x2sum * dx / ((float) (m1))) / sqrt (((float) (maximum)));
              alphatrk.push_back((ysum + 0.5) * (dy / ((float) (m2))) + y1);
              float eralpha = (fabs) (y2sum * dy / ((float) (m2))) / sqrt (((float) (maximum)));
              float erphi0 = sqrt (erphi * erphi + eralpha * eralpha);
              if (index > 7)
                {
                  phi0trk.push_back(phitrk[ntrk] - alphatrk[ntrk]);
                  x0trk.push_back(Radius0 * cos (phitrk[ntrk]));
                }
              else
                {
                  phi0trk.push_back(alphatrk[ntrk] - phitrk[ntrk]);
                  x0trk.push_back( -1.0 * Radius0 * cos (phitrk[ntrk]));
                }
              float erx0 = (fabs) (Radius0 * sin (phitrk[ntrk]) * erphi);
              y0trk.push_back(Radius0 * sin (phitrk[ntrk]));
              float ery0 = (fabs) (Radius0 * cos (phitrk[ntrk]) * erphi);
              slope.push_back(tan(phi0trk[ntrk]));
              erslope.push_back(erphi0);
              intercept.push_back(y0trk[ntrk] - slope[ntrk]*x0trk[ntrk]);
              erint.push_back(sqrt(ery0*ery0 + erslope[ntrk]*erslope[ntrk] + erx0*erx0));
              chi2.push_back(0.);
              ntrk++;
            }
          tmpbufmax.push_back(adcsum);
        }

      xmax.clear();
      ymax.clear();

    }

  phitrk.clear();
  phi0trk.clear();
  alphatrk.clear();
  x0trk.clear();
  y0trk.clear();
  tmpbufx.clear();
  tmpbufy.clear();
  tmpbufa.clear();
  tmpbufmax.clear();

  return 0;
}

static int
RefitTracks (vector<float> &slope, vector<float> &intercept,
             int ntrk, vector<int> &numpoints, int Debug, vector<float> &chi2)
{
  // refits tracks after they are found
  int i, j, n = 0;
  double anew = 0., bnew = 0., chisq = 0., siganew = 0., sigbnew = 0., cov = 0.;
  double x[1000], y[1000], w[1000];
  const size_t stride = 1;

  for (i = 0; i < 1000; i++)
    {
      w[i] = 1.0;
      x[i] = 0.;
      y[i] = 0.;
    }

  // loop over all tracks
  for (i = 0; i < ntrk; i++)
    {
      n = numpoints[i];
      if (n > 1000)
        {
          n = 1000;
        }
      for (j = 0; j < n; j++)
        {
          x[j] = List_of_Points[i].X[j];
          y[j] = List_of_Points[i].Y[j];
        }

      if (n > 1)
        {
          gsl_fit_wlinear(x, stride, w, stride, y, stride, n,
			  &bnew, &anew, &sigbnew, &cov, &siganew, &chisq);
          // reassign slope and intercept
          slope[i] = anew;
          intercept[i] = bnew;
          chi2[i] = chisq;
        }
    }

  return 0;

}

static int
GetTracks (vector<float> &slope,
           vector<float> &intercept,
           int ntrk,
           int NumTrkPoints,
           vector<int> &numpoints,
           vector<float> &numpointsW,
           vector<float> &TotalAmp,
           float Dist0,
           vector<int> &numplanes,
           int Debug,
           vector<int> &numpointsP,
           TecOutV1* tecout)
{
  // Associates tracks with time bins of wires, Rejects ghost tracks
  float a, aa, b, x0, y0, dist, weight;
  int i, ii, j, listofplanes[TECMAXPLANE];

  // Correction factor for the maximum distance btw hit and track.
  // This should be monitored to see if there are fluctuations in tha
  // values.  If so, we need to implement a kind of calibration table
  // in database. CLS 10/18/2005
  float corrDist0[TECMAXINDEX] =
    {
      1., 1., 1., 1., 1., 1.,
      1., 1., 1., 1., 1., 1.,
      1.77723, 1.69428, 1.53863, 1.53537, 1.53537, 1.53537,
      1.69389, 1.59863, 1.59863, 1.90306, 2.37376, 2.27629,
      1.75419, 1.66839, 1.52207, 1.44767, 1.41599, 1.41599,
      1.6115, 1.60377, 1.88988, 1.88988, 2.25509, 2.17572,
      1., 1., 1., 1., 1., 1.,
      1., 1., 1., 1., 1., 1.
    };

  // For each point calculate how many times it is used in different tracks
  for (j = 0; j < NumTrkPoints; j++)
    {
      TrkPoints[j].used = 0;
    }
  for (i = 0; i < ntrk; i++)
    {
      a = slope[i];
      b = intercept[i];
      aa = sqrt(a * a + 1.);
      for (j = 0; j < NumTrkPoints; j++)
        {
          int pl = TrkPoints[j].Plane;
          int side = TrkPoints[j].Index % 2;
          int iindex = (TrkPoints[j].Index / 2) * 12 + pl * 2 + side;
          y0 = TrkPoints[j].Y;
          x0 = TrkPoints[j].X;
          dist = (y0 - a * x0 - b) / aa ;

#ifdef DEBUG

          int iwire = TrkPoints[j].wire;
          hdist[iindex]->Fill(dist, iwire);
#endif

          dist = fabs(dist);
          if (dist < Dist0*corrDist0[iindex])
            {
              TrkPoints[j].used++;
            }
        }
    }

  if (ntrk > MAXListofPoints2)
    {
      ntrk = MAXListofPoints2;
    }
  for (i = 0; i < ntrk; i++)
    {
      for (ii = 0; ii < TECMAXPLANE; ii++)
        {
          listofplanes[ii] = 0;
        }
      numplanes.push_back(0);
      numpoints.push_back(0);
      for (int i0 = 0; i0 < TECMAXPLANE; i0++)
        {
          numpointsP.push_back(0);
        }
      numpointsW.push_back(0.);
      TotalAmp.push_back(0.);

      a = slope[i];
      b = intercept[i];
      aa = sqrt(a * a + 1.0);
      for (j = 0; j < NumTrkPoints; j++)
        {
          int pl = TrkPoints[j].Plane;
          int side = TrkPoints[j].Index % 2;
          int iindex = (TrkPoints[j].Index / 2) * 12 + pl * 2 + side;
          y0 = TrkPoints[j].Y;
          x0 = TrkPoints[j].X;
          dist = (fabs) ((y0 - a * x0 - b) / aa);
          if (dist < Dist0*corrDist0[iindex])
            {
              TotalAmp[i] += TrkPoints[j].Weight;
              List_of_Points[i].bins[numpoints[i]] = TrkPoints[j].bin;
              List_of_Points[i].wires[numpoints[i]] = TrkPoints[j].wire;
              List_of_Points[i].plane[numpoints[i]] = TrkPoints[j].Plane;
              List_of_Points[i].X[numpoints[i]] = TrkPoints[j].X;
              List_of_Points[i].Y[numpoints[i]] = TrkPoints[j].Y;
              List_of_Points[i].ampl[numpoints[i]] = TrkPoints[j].Weight;
              List_of_Points[i].charge[numpoints[i]] = TrkPoints[j].charge;
              List_of_Points[i].index = TrkPoints[j].Index;
              List_of_Points[i].hitnumber[numpoints[i]] = TrkPoints[j].hitnumber;

              if (TrkPoints[j].used > 0)
                {
                  weight = 1. / ((float) TrkPoints[j].used);
                }
              else
                {
                  weight = 1.0;
                }
              numpointsW[i] += weight;
              listofplanes[TrkPoints[j].Plane]++;
              numpoints[i]++;
              if (numpoints[i] >= MAXListofPoints1)
                {
                  numpoints[i] = MAXListofPoints1 - 1;
                }
              numpointsP[i*TECMAXPLANE + TrkPoints[j].Plane]++;
            }

        }

      for (ii = 0; ii < TECMAXPLANE; ii++)
        {
          if (listofplanes[ii] > 0)
            {
              numplanes[i]++;
            }
        }

    }

  return 0;
}

static int
Prepare2Track (int *dotracking_ptr, int INDEX)
{
  // Prepares some arrays for tracking
  int dotracking[16], i, Index;

  Index = INDEX;
  for (i = 0; i < 16; i++)
    {
      dotracking[i] = 0;
    }

  if (Index >= 0)
    {
      dotracking[Index] = 1;
    }
  else
    {
      if (Index == -100)
        {                       // sector 0, east arm, both sides
          dotracking[0] = 1;
          dotracking[1] = 1;
        }
      if (Index == -200)
        {                       // sector 1, east arm, both sides
          dotracking[2] = 1;
          dotracking[3] = 1;
        }
      if (Index == -400)
        {                       // sector 2, east arm, both sides
          dotracking[4] = 1;
          dotracking[5] = 1;
        }
      if (Index == -800)
        {                       // sector 3, east arm, both sides
          dotracking[6] = 1;
          dotracking[7] = 1;
        }

      if (Index == -1)
        {			/* east arm, south side */
          dotracking[0] = 1;
          dotracking[2] = 1;
          dotracking[4] = 1;
          dotracking[6] = 1;
        }
      if (Index == -2)
        {			/* east arm, north side */
          dotracking[1] = 1;
          dotracking[3] = 1;
          dotracking[5] = 1;
          dotracking[7] = 1;
        }
      if (Index == -3)
        {			/* west arm, south side */
          dotracking[8] = 1;
          dotracking[10] = 1;
          dotracking[12] = 1;
          dotracking[14] = 1;
        }
      if (Index == -4)
        {			/* west arm, north side */
          dotracking[9] = 1;
          dotracking[11] = 1;
          dotracking[13] = 1;
          dotracking[15] = 1;
        }
      if (Index == -34)
        {			/* west arm */
          dotracking[8] = 1;
          dotracking[10] = 1;
          dotracking[12] = 1;
          dotracking[14] = 1;
          dotracking[9] = 1;
          dotracking[11] = 1;
          dotracking[13] = 1;
          dotracking[15] = 1;
        }
      if (Index == -12)
        {			/* east arm */
          dotracking[0] = 1;
          dotracking[2] = 1;
          dotracking[4] = 1;
          dotracking[6] = 1;
          dotracking[1] = 1;
          dotracking[3] = 1;
          dotracking[5] = 1;
          dotracking[7] = 1;
        }
      if (Index == -13)
        {			/* south side of both arms */
          dotracking[0] = 1;
          dotracking[2] = 1;
          dotracking[4] = 1;
          dotracking[6] = 1;
          dotracking[8] = 1;
          dotracking[10] = 1;
          dotracking[12] = 1;
          dotracking[14] = 1;
        }
      if (Index == -24)
        {			/* north side of both arms */
          dotracking[1] = 1;
          dotracking[3] = 1;
          dotracking[5] = 1;
          dotracking[7] = 1;
          dotracking[9] = 1;
          dotracking[11] = 1;
          dotracking[13] = 1;
          dotracking[15] = 1;
        }
      if (Index == -1234)
        {			/* both arms, both sides */
          for (i = 0; i < 16; i++)
            {
              dotracking[i] = 1;
            }
        }
    }

  for (i = 0; i < 16; i++)
    {
      *(dotracking_ptr + i) = dotracking[i];
    }

  return 0;

}

struct gen
{
  gen(int I)
  {
    i = I;
  }
  int operator()()
  {
    return i++;
  }
  int i;
};

void
FillArray (BUFFER Buffer,
           int *Array,
           const int Stat[],
           const float &midphi,
           const int &iArm,
           const int &nPhi,
           const int &nTheta,
           const float &r0,
           const int &Statistics,
           const float &y11,
           const float &y12,
           const float &y21,
           const float &y22,
           const int &index,
           TecGeometryObject* TGO)
{
  // Fills a two-dimensional histogram with the Hough transform
  // variables phi and theta.  Phi is azimuthal angle at the point
  // where the line formed by two randomly selected hits intersects a
  // reference circle.  Theta is the angle between the line formed by
  // those same hits and the radial direction.
  int p1, p2, np1, np2;
  float dx, dy1, dy2;
  int i, j;
  int iphi, itheta;
  float m, m_sq, b, r_sq, det, det_sq;
  float x1, y1, x2, y2, xi1, xi2, xi, yi;
  float phi, theta, m1, phi0;
  int p[TECMAXPLANE], nPlanes;
  int planeIndex;
  bool active;
  static vector<int> hi[TECMAXPLANE];
  vector<int>::iterator hii[TECMAXPLANE];
  vector<pair<int, int> > pp;
  vector<pair<int, int> >::iterator ppi;

#ifdef DEBUG

  TFile *f = new TFile("fa.root", "RECREATE");
  TH2 *h = new TH2F("h", "h", 50, -M_PI_2, M_PI_2, 50, -0.5, 0.5);
#endif

  // Load up the "planes" array with the indexes of the planes we will
  // use. List of active planes is kept in TecGeometryObject, and
  // you can change plane status by using
  // TecGeometryObject::setActivePlane(int index, int active)
  // function. List of planes may be not contiguous.

  int sector = index / TECMAXSIDE;
  int side = index % TECMAXSIDE;

  nPlanes = 0;
  for (i = 0; i < TECMAXPLANE; i++)
    {
      planeIndex = sector * TECMAXPLANE * TECMAXSIDE + i * TECMAXSIDE + side;
      active = TGO->isActivePlane(planeIndex);
      if (active && Stat[i] > 0)
        {
          p[nPlanes] = i;
          nPlanes++;
        }
    }

  // The vector pp holds a shuffled list of acceptable plane indexes.
  for (i = 0; i < nPlanes - 1; i++)
    {
      for (j = i + 1; j < nPlanes; j++)
        {
          pp.push_back(pair<int, int>(p[i], p[j]));
        }
    }
  random_shuffle(pp.begin(), pp.end(), generator);
  ppi = pp.begin();

  // For each of the active planes, make a list of hit indexes on that
  // plane and then shuffle that.
  for (i = 0; i < nPlanes; i++)
    {
      hi[p[i]].resize(Stat[p[i]]);
      generate_n(hi[p[i]].begin(), Stat[p[i]], gen(0));
      random_shuffle(hi[p[i]].begin(), hi[p[i]].end(), generator);
      hii[p[i]] = hi[p[i]].begin();
    }

  // Reset the Hough array
  fill_n(Array, nPhi * nTheta, 0);

  r_sq = r0 * r0;
  dy1 = y12 - y11;
  dy2 = y22 - y21;

  // Grab random pairs of TEC hits, calculate the Hough transform
  // variables theta and phi, and then fill bins in a two-dimensional
  // array.  Peaks in this array are (just possibly) tracks.  n1 and
  // n2 are indices of p[] array.  np1 and np2 are planes numbers
  // picked from p[] array.
  for (i = 0; i < Statistics; i++)
    {
      // Select a pair of TEC planes.
      np1 = ppi->first;
      np2 = ppi->second;
      if (++ppi == pp.end())
        {
          ppi = pp.begin();
        }

      // Select a pair of hits from those planes.
      p1 = *hii[np1];
      if (++hii[np1] == hi[np1].end())
        {
          hii[np1] = hi[np1].begin();
        }
      p2 = *hii[np2];
      if (++hii[np2] == hi[np2].end())
        {
          hii[np2] = hi[np2].begin();
        }

      x1 = Buffer[np1][p1].X;
      y1 = Buffer[np1][p1].Y;
      x2 = Buffer[np2][p2].X;
      y2 = Buffer[np2][p2].Y;

      dx = x2 - x1;
      float intercept;
      if (dx != 0.0)
        {
          m = (y2 - y1) / dx;
          intercept = y1 - m * x1;
        }
      else
        {
          m = 0.0;
          intercept = 9999.0;
        }

      // Reject pairs with large intercept. If they come from a real track,
      // this track has a very low momentum. A cut r0/2. corresponds to
      // approximately 90 MeV/c. We are not interested in such tracks.
      if (fabs(intercept) < r0 / 2.)
        {
          // Find the intersection of the line and reference circle.
          m_sq = m * m;
          b = y1 - m * x1;
          det_sq = m_sq * r_sq - b * b + r_sq;

          // If det_sq < 0, the line doesn't intersect the circle.  We
          // could change the way we loop to guarantee an intersection
          // and then be able to skip this test.  Two methods have
          // been batted about.  One, pick hits on plane 1 or plane 2
          // and match with hits on plane 3 or plane 4 (the reference
          // circle typically has a radius that puts it right between
          // plane 2 and 3).  Or, increase the radius of the nominal
          // reference circle so that it sits beyond plane 4.  Either
          // way you'd be assured that any line would intersect the
          // reference circle.
          if (det_sq > 0)
            {
              det = sqrt (det_sq);
              xi1 = -m * b - det;
              xi2 = -m * b + det;
              m1 = 1.0 + m_sq;

              // Today, budgets being what they are, we only have a
              // TEC in the east arm.  When we get one in the west
              // arm, these commented-out lines should be enabled.
              // 	  if (iArm > 7)	/* arm 1 (west) */
              // 	    {
              // 	      if (xi1 > xi2)
              // 		{
              // 		  xi = xi1 / m1;
              // 		}
              // 	      else
              // 		{
              // 		  xi = xi2 / m1;
              // 		}
              // 	      phi0 = atan (m);
              // 	    }
              // 	  else			/* arm 0 (east) */
              // 	    {
              if (xi1 < xi2)
                {
                  xi = xi1 / m1;
                }
              else
                {
                  xi = xi2 / m1;
                }
              phi0 = -atan (m);
              // 	    }

              // Phi is azimuthal angle at the point where the line
              // defined by our two randomly selected hits intersects
              // the reference circle.  Theta is the angle between the
              // line and the radial direction.
              yi = m * xi + b;
              phi = atan (yi / fabs (xi));
              theta = phi - phi0;
              phi = phi - midphi;

#ifdef DEBUG

              h->Fill(theta, phi);
#endif

              // Determine bin numbers for theta and phi, and if
              // they're in range, increment the bin contents.
              iphi = (int) (nPhi * (phi - y11) / dy1);
              itheta = (int) (nTheta * (theta - y21) / dy2);
              if ((iphi >= 0)
                  && (iphi < nPhi)
                  && (itheta >= 0)
                  && (itheta < nTheta))
                {
                  Array[iphi + itheta*nPhi]++;
                }
            }
        }
    }

#ifdef DEBUG
  h->Write();
  f->Close();
  exit(0);
#endif

}

mTecHoughTrackModule::mTecHoughTrackModule ()
{
  RandSeed = -123456789;
  Statistics = 80000;           // run4 AuAu (03/18/2004)
  Statistics1 = 4; // divide by this value number of hits squared per sector after cleaner
  // to get Statistics
  // for AuAu number of hits varies from 3k to 30k with average
  // about 18k. Number of hits with adc>4 is approximately half of this.
  Statistics2 = 0; // not used any more
  Statistics3 = 0; // not used any more
  fillRawTrack = 0;
  whichplane = 0;
  minbins = 20;			// (05/30/01)
  minPlanes = 1;
  algorithm = 1;
  INDEX = -1234;
  Rebin = 4;			// (05/30/01)
  Verbose = 0;
  Write2File = 0;
  angLim = 0.6;
  clPar[0] = 0.05;
  clPar[1] = 0.08;
  clPar[2] = 1.50;
  clPar[3] = 0.08;
  clPar[4] = 99.0;
  // Threshold
  clPar[5] = 4.0;                // (08/07/01) back to adc
  Radius0 = TECREFERENCERADIUS;
  Dist0 = 0.6;
  m1 = 460;
  y11 = -0.23;
  y12 = 0.23;
  m2 = 80;
  y21 = -0.8;
  y22 = 0.8;
  fillVector = 0;
  fillVectTrack = 0;
  Refit = 1;
  LowThreshold = 1.0;           // (12/15/2004) lower threshold for run 5
  Beg1 = 0.0;
  Beg2 = 0.0;
  CopyTecOut = 1;
  StripOrphanHits = 1;   // Run5 (Feb 04, 2005)a
  LastEventWithHits = 999999;	// Run3 (Dec 2002)
  occupancy_limit = 3;

#ifdef DEBUG

  char hname[10];
  for (int i = 0; i < 48; i++)
    {
      sprintf(hname, "hdist%d", i);
      hdist[i] = new TH2F(hname, "Hit - Track distance", 320, -10, 10, 500, -.5, 499.5);
    }
#endif

}

void
mTecHoughTrackModule::set_LastEventWithHits(int i)
{
  if (i < 1000)
    i = 1000;
  LastEventWithHits = i;
}

mTecHoughTrackModule::~mTecHoughTrackModule ()
{ }

PHBoolean
mTecHoughTrackModule::event (PHCompositeNode * topNode)
{

  // Find TecGeometryObject and TecCalibrationObject
  PHBoolean status0;
  PHBoolean status1 = False;
  PHBoolean status2 = False;
  TecGeometryObject *TGO;
  TecCalibrationObject *TCO;

  TGO = findNode::getClass<TecGeometryObject>(topNode, "TecGeometry");
  if (!TGO)
    {
      PHMessage ("mTecHoughTrackModule: ", PHError,
                 "TecGeometryObject not found.\n");
      PHMessage ("mTecHoughTrackModule: ", PHError,
                 "Will use default Geant geometry.\n");
      TGO = new TecGeometryObject ();
      status1 = TGO->FetchFromFile ();
    }

  TCO = findNode::getClass<TecCalibrationObject>(topNode, "TecCalibration");
  if (!TCO)
    {
      PHMessage ("mTecHoughTrackModule: ", PHError,
                 "TecCalibrationObject not found.\n");
      PHMessage ("mTecHoughTrackModule: ", PHError,
                 "Will set all Calibration Constatnts to 1.\n");
      TCO = new TecCalibrationObject ();
      status2 = TCO->FetchFromFile ();
    }

  status0 = mTecHoughTrackModule::event (topNode, TGO, TCO);

  if (status1)
    {
      delete TGO;
    }
  if (status2)
    {
      delete TCO;
    }

  return status0;

}

PHBoolean
mTecHoughTrackModule::event (PHCompositeNode * root,
                             TecGeometryObject * TGO,
                             TecCalibrationObject * TCO)
{
  using namespace TecUtilities;
  
  PHNodeIterator iii (root);
  int nHits;
  static int eventNumber = 0;

  if (Verbose > 0)
    {
      cout << "mTecHoughTrackModule: Started..." << endl;
    }

  TecOutV1* tecout;
  PHTypedNodeIterator<TecOutV1> teciter(root);
  TecOutNode_t *TecOutNode = teciter.find("TecOutV1");

  if (TecOutNode)
    {
      tecout = (TecOutV1*)TecOutNode->getData();
    }
  else
    {
      return False;
    }
  int Debug, arm, sector, plane, side, wire;
  int index0, startI, stopI, index;
  int jj, myindex, ntrk;
  unsigned int i, occupancy[TECMAXINDEX][TECMAXTIMEBIN][32];
  int j, k, NumTrkPoints;
  int Stat[TECMAXPLANE], plpl[TECMAXPLANE], plplpl, maximum;
  int nptsMin, ntrkfinal;
  float tmpArray[TECMAXTIMEBIN], BinThreshold;
  float Xwire, Ywire, Xpos, Ypos, midphi[TECMAXINDEX / TECMAXPLANE];
  float relativebin, difference1, difference2;
  float yy1, yy0, xx0, xx1;

  vector<TecTrackCluster> ClusterQueue;

  vector<int> numpoints, numplanes, numpointsP;
  vector<float> slope, intercept, erslope, erint, numpointsW, TotalAmp, chi2;

  float erxx0, erxx1, eryy0, eryy1;
  int dotracking[16];
  float MidPhi[TECMAXINDEX];
  BUFFER Buffer;
  int Array[m1*m2];
  int tracknok, ngoodplanes;

  Debug = Verbose;
  BinThreshold = clPar[5];
  tracknok = 0;

  // Initialize Buffer
  for (int i = 0; i < 6; i++)
    {
      for (int j = 0; j < MAXBufferPoints; j++)
        {
          Buffer[i][j].X = 0.0;
          Buffer[i][j].Y = 0.0;
        }
    }

  // Set Statistics
  nHits = tecout->getNHits();

  Statistics = sqr(nHits) / Statistics1;
  if (Statistics < 10000)
    Statistics = 10000;
  if (Statistics > 500000)
    Statistics = 500000;

  if (nHits < minbins)
    {
      return False;
    }

  // reset occupancy array
  for (int i = 0; i < TECMAXINDEX; i++)
    for (int j = 0; j < TECMAXTIMEBIN; j++)
      for (int k = 0; k < 32; k++)
        occupancy[i][j][k] = 0;

  // Rearrange Tec Hits in tecout wire-by-wire
  vector<TecWire> tecwires;
  TecWire twire;

  int oldindex = -1;
  int oldwire = -1;
  int wirenok = 0;
  // event-by-event noise usually is concentrated in few time x ADC bins
  // I can clean up the noise if I reject those bins where there are more entries than expected
  // CLS 04/11/2005
  for (int i1 = 0; i1 < nHits; i1++)
    {
      int index = tecout->getHitIndex(i1);
      int bin = tecout->getHitTimeBin(i1);
      int adc = tecout->getHitADC(i1);
      occupancy[index][bin][adc] ++;
    }

  for (int i1 = 0; i1 < nHits; i1++)
    {
      int index = tecout->getHitIndex(i1);
      int bin = tecout->getHitTimeBin(i1);
      int adc = tecout->getHitADC(i1);
      if (occupancy[index][bin][adc] > occupancy_limit)
        continue;
      int wire = tecout->getHitWire(i1);
      float charge = tecout->getHitCharge(i1);
      if (oldindex != index || oldwire != wire)
        {
          for (int j1 = 0; j1 < TECMAXTIMEBIN; j1++)
            {
              twire.setADC(j1, 0);
              twire.setHitnumber(j1, -1);
              twire.setCharge(j1, 0.);
            }
          twire.setIndex(index);
          twire.setWire(wire);
          twire.setADC(bin, adc);
          twire.setCharge(bin, charge);
          twire.setHitnumber(bin, i1);
          twire.setX(TGO->getGlobalX(index, wire));
          twire.setY(TGO->getGlobalY(index, wire));
          tecwires.push_back(twire);
          oldindex = index;
          oldwire = wire;
          wirenok++;
        }
      else
        {
          tecwires[wirenok - 1].setADC(bin, adc);
          tecwires[wirenok - 1].setCharge(bin, charge);
          tecwires[wirenok - 1].setHitnumber(bin, i1);
        }
    }

  // Find minimum and maximum time bins in each plane half
  if (Verbose > 0)
    {
      cout << "Calling GetMinMax..." << endl;
    }
  GetMinMax(MinBin, MaxBin, TCO, Debug);

  // Find SinAlpha and midphi for hit coordinate calculation
  if (Verbose > 0)
    {
      cout << "Calling GetMidphi..." << endl;
    }
  GetMidPhi(MidPhi, midphi, SinAlpha, TGO, Debug);

  // Start Tracking

  // do tracking separately for each side of each sector
  startI = 0;
  //stopI = 16;
  stopI = 8;

  Prepare2Track (dotracking, INDEX);

  for (index = startI; index < stopI; index++)
    {
      if (dotracking[index] == 1)
        {
          // Loop over all hits and fill TrkPoints structure
          NumTrkPoints = 0;	// Total number of hits in this sector/side

          if (Verbose > 0)
            cout << "tracking index " << index << endl;

          for (i = 0; i < 6; i++)
            {
              Stat[i] = 0;
            }

          for (i = 0; i < tecwires.size(); i++)
            {
              arm = 0;
              sector = tecwires[i].getIndex() / (TECMAXPLANE * TECMAXSIDE);
              plane = (tecwires[i].getIndex() - sector * (TECMAXPLANE * TECMAXSIDE)) / TECMAXSIDE;
              side = tecwires[i].getIndex() % TECMAXSIDE;
              myindex = arm * TECMAXINDEX + sector * TECMAXPLANE * TECMAXSIDE
		+ plane * TECMAXSIDE + side;
              int activeplane = TGO->isActivePlane(myindex);

              if (activeplane)
                {

                  wire = tecwires[i].getWire();
                  index0 = arm * (TECMAXINDEX / TECMAXPLANE) + sector * TECMAXSIDE + side;

                  if (index0 == index)
                    {
                      // select points only in current zone
                      if ((MaxBin[myindex] != -999) && (MinBin[myindex] != 999) &&
                          (MaxBin[myindex] != MinBin[myindex]))
                        {
                          // get coordinates
                          Xwire = TGO->getGlobalX(sector * TECMAXSIDE * TECMAXPLANE +
                                                  plane * TECMAXSIDE + side, wire);
                          Ywire = TGO->getGlobalY(sector * TECMAXSIDE * TECMAXPLANE +
                                                  plane * TECMAXSIDE + side, wire);

                          // rebin time if requested
                          for (j = 0; j < TECMAXTIMEBIN; j++)
                            {
                              tmpArray[j] = 0.0;
                            }
                          for (j = 0; j < TECMAXTIMEBIN; j++)
                            {
                              jj = j / Rebin;
                              tmpArray[jj] += tecwires[i].getADC(j);
                            }

                          // Fill TrkPoints and Buffer structures for each
                          // time bin.  Buffer is used for filling
                          // 2-dimensional array for Hough transform and
                          // can be rebinned to save cpu time in FillArray
                          // function.  There is additional (higher)
                          // threshold for filling Buffer (BinThreshold).
                          // TrkPoints is used for hit/track association
                          // and is never rebinned.  TrkPoints is filled
                          // for all hits above LowThreshold.
                          difference1 = (MaxBin[myindex] - MinBin[myindex]);

                          // Fill Buffer array first
                          for (j = 0; j < (TECMAXTIMEBIN / Rebin); j++)
                            {
                              unsigned int tmpoccupancy = 0;
                              for (int ibin = j * Rebin; ibin < (j + 1)*Rebin; ibin++)
                                {
                                  int iadc = tecwires[i].getADC(ibin);
                                  tmpoccupancy += occupancy[myindex][ibin][iadc];
                                }
                              if (tmpoccupancy > occupancy_limit*Rebin)
                                continue;

                              // Find number of points in each plane above
                              // BinThreshold
                              if (tmpArray[j] > LowThreshold && tmpArray[j] > BinThreshold)
                                {
                                  difference2 = (((float) j) + 0.5) * ((float) Rebin) -
				    ((float) (MinBin[myindex]));
                                  relativebin = difference2 / difference1;
                                  CalcXYfromBin (relativebin, Xwire, Ywire,
                                                 SinAlpha[myindex],
                                                 Xpos, Ypos, Debug);

                                  Buffer[plane][Stat[plane]].X = Xpos;
                                  Buffer[plane][Stat[plane]].Y = Ypos;

                                  Stat[plane]++;
                                  if (Stat[plane] >= MAXBufferPoints)
                                    {
                                      Stat[plane] = MAXBufferPoints - 1;
                                    }

                                }

                            }

                          int realNhits = 0;
                          for (int iplane = 0; iplane < 6; iplane++)
                            realNhits += Stat[plane];
                          Statistics = realNhits * (realNhits - 1) / Statistics1;

                          // Fill TrkPoints array
                          for (j = 0; j < TECMAXTIMEBIN; j++)
                            {
                              int iadc = tecwires[i].getADC(j);
                              if (occupancy[myindex][j][iadc] > occupancy_limit)
                                continue;

                              if (iadc > LowThreshold)
                                {
                                  difference2 = ((float) j) - ((float)(MinBin[myindex]));
                                  relativebin = difference2 / difference1;
                                  CalcXYfromBin (relativebin, Xwire, Ywire,
                                                 SinAlpha[myindex],
                                                 Xpos, Ypos, Debug);

                                  TrkPoints[NumTrkPoints].Index = arm * 8 + sector * 2 + side;
                                  TrkPoints[NumTrkPoints].Plane = plane;
                                  TrkPoints[NumTrkPoints].X = Xpos;
                                  TrkPoints[NumTrkPoints].Y = Ypos;
                                  TrkPoints[NumTrkPoints].Z = (float) (side);
                                  TrkPoints[NumTrkPoints].Weight = tecwires[i].getADC(j);
                                  TrkPoints[NumTrkPoints].charge = tecwires[i].getCharge(j);
                                  TrkPoints[NumTrkPoints].wire = wire;
                                  TrkPoints[NumTrkPoints].bin = j;
                                  TrkPoints[NumTrkPoints].hitnumber =
                                    tecwires[i].getHitnumber(j);
                                  if (tecwires[i].getHitnumber(j) > -1 &&
                                      tecwires[i].getHitnumber(j) < nHits)
                                    {
                                      tecout->setHitX(tecwires[i].getHitnumber(j), Xpos);
                                      tecout->setHitY(tecwires[i].getHitnumber(j), Ypos);
                                    }
                                  else
                                    {
                                      cerr
					<< "mTecHoughTrack:: wrong hit number = "
					<< tecwires[i].getHitnumber(j) << " "
					<< i << " " << j << endl;
                                    }

                                  NumTrkPoints++;

                                  if (NumTrkPoints >= MAXTrkPoints)
                                    {
                                      NumTrkPoints = MAXTrkPoints - 1;
                                    }
                                }
                            }
                        }
                    }
                } // if active plane
            } // end loop over tecwires

          if (Debug > 0)
            {
              cout << "mTecHoughTrack -> NumTrkPoints = "
		   << NumTrkPoints << endl
		   << "Start tracking for index: " << index << " " << Stat[0]
		   << " " << Stat[1] << " " << Stat[2] << " " << Stat[3]
		   << " " << Stat[4] << " " << Stat[5] << " "
		   << Stat[0] + Stat[1] + Stat[2] + Stat[3] + Stat[4] + Stat[5] << endl;
            }

          // Do tracking only if there are some points in at least two
          // of the TEC planes
          ngoodplanes = 0;
          for (i = 0; i < (int)TECMAXPLANE; i++)
            {
              if (Stat[i] > 12 / Rebin)
                {
                  ngoodplanes++;
                }
            }

          if (ngoodplanes > 1)
            {
              FillArray (Buffer, Array, Stat,
                         midphi[index / 2], index,
                         m1, m2,
                         Radius0, Statistics,
                         y11, y12,
                         y21, y22,
                         index, TGO);
              // Fill cluster queue
              DoClustering2(Array, ClusterQueue, maximum,
			    algorithm, Debug);
              // Get cluster information from the cluster queue
              GetClusters2(Array, ClusterQueue, maximum,
			   slope, intercept, erslope, erint, chi2,
			   ntrk, index, midphi[index / 2], Debug);
	      //	      std::cout << "ntrk = " << ntrk << std::endl;
              if (ntrk > 999)
                {
                  cerr << PHWHERE << " Tracking Failed: too many Tec tracks." << endl;
                  // reset all vectors
                  ClusterQueue.clear();
                  numpoints.clear();
                  numplanes.clear();
                  numpointsP.clear();
                  slope.clear();
                  intercept.clear();
                  erslope.clear();
                  erint.clear();
                  numpointsW.clear();
                  TotalAmp.clear();
                  chi2.clear();
                  if (CopyTecOut)
                    {
                      //copyTecOut(root, eventNumber, LastEventWithHits, StripOrphanHits);
                      copyTecTrackOut(root);
                      copyTecHitOut(root, StripOrphanHits);
                    }
                  return False;
                }

              // Associate tracks with points
              GetTracks (slope, intercept, ntrk, NumTrkPoints,
			 numpoints, numpointsW, TotalAmp, Dist0,
			 numplanes, Debug, numpointsP, tecout);
              nptsMin = minbins;
              // Refit Tracks if requested
              if (Refit != 0)
                {
                  RefitTracks (slope, intercept, ntrk,
			       numpoints, Debug, chi2);
                }
              ntrkfinal = 0;

              // Loop over track candidates and accept only good ones
              for (i = 0; i < (unsigned int)ntrk; i++)
                {
                  if (numpoints[i] > nptsMin
                      && numpointsW[i] > (((float) nptsMin) / clPar[2])
                      && numplanes[i] > minPlanes)
                    {
                      // Tracks accepted after all cuts
                      // Find entry end exit coordinates for each track
                      GetEntryExit (slope[i], intercept[i],
				    TGO, index, Debug,
				    xx0, yy0, xx1, yy1,
				    erxx0, eryy0, erxx1, eryy1);
                      ntrkfinal++;

                      plplpl = 0;
                      int plhits[6], plwire[6], listofwires[10000],
			nlistofwires;
                      int foundwire, currentwire;
                      nlistofwires = 0;
                      for (j = 0; j < 6; j++)
                        {
                          plpl[j] = 0;
                          plhits[j] = 0;
                          plwire[j] = 0;
                        }
                      for (j = 0; j < numpoints[i]; j++)
                        {
                          for (k = 0; k < 6; k++)
                            {
                              if (List_of_Points[i].plane[j] == k)
                                {
                                  plpl[k] = 1;
                                  plhits[k]++;
                                  foundwire = 0;
                                  currentwire =
                                    List_of_Points[i].plane[j] * 1000 +
                                    List_of_Points[i].wires[j];
                                  for (int l = 0; l < nlistofwires; l++)
                                    {
                                      if (listofwires[l] == currentwire)
                                        {
                                          foundwire = 1;
                                        }
                                    }
                                  if (foundwire == 0)
                                    {
                                      plwire[k]++;
                                      listofwires[nlistofwires] = currentwire;
                                      nlistofwires++;
                                      if (nlistofwires > 9999)
                                        {
                                          nlistofwires = 9999;
                                        }
                                    }
                                }
                            }
                        }
                      for (j = 0; j < 6; j++)
                        {
                          plplpl += plpl[j];
                          if (plwire[j] > 9)
                            {
                              plwire[j] = 9;
                            }
                          if (plhits[j] > 99)
                            {
                              plhits[j] = 99;
                            }
                        }


                      // Fill TecOut
                      float xyzi[3], xyzo[3];
                      xyzi[0] = xx0;
                      xyzi[1] = yy0;
                      xyzi[2] = 1.0;
                      if ((index % 2) == 0)
                        {
                          xyzi[2] = -1.0;
                        }
                      xyzo[0] = xx1;
                      xyzo[1] = yy1;
                      xyzo[2] = 1.0;
                      if ((index % 2) == 0)
                        {
                          xyzo[2] = -1.0;
                        }
		      //		      std::cout << "(i, ntrkfinal) = (" << i << ", " << ntrkfinal << ")" << std::endl;
                      tecout->AddTecTrack(xyzi, xyzo);
                      tecout->setTrackNhits(tracknok, numpoints[i]);
                      for (int i1 = 0; i1 < TECMAXPLANE; i1++)
                        {
                          tecout->setTrackNhits(tracknok, i1, plhits[i1]);
                          tecout->setTrackNwires(tracknok, i1, plwire[i1]);
                        }
                      tecout->setTrackIndex(tracknok, index);
                      ((TecTrack*)tecout->GetTecTracks()->UncheckedAt(tracknok))->setEemcMin(chi2[i]);

                      // assign hits to tracks
                      for (int i1 = 0; i1 < numpoints[i]; i1++)
                        {
                          int tmp = List_of_Points[i].hitnumber[i1];
                          if (tmp > -1 && tmp < nHits)
                            {
                              if (tecout->getHitTrackID(tmp, 0) == -1)
                                {
                                  tecout->setHitTrackID(tmp, 0, tracknok);
                                }
                              else
                                {
                                  tecout->setHitTrackID(tmp, 0, -2);
                                }
                            }
                          else
			    {}
                        }

                      tracknok++;
                    }
                }

              if (Debug > 0)
                {
                  printf
		    ("mTecHoughTrack: final # of tracks in %d sector/side = %d \n",
		     index, ntrkfinal);
                }

              // reset all vectors
              ClusterQueue.clear();
              numpoints.clear();
              numplanes.clear();
              numpointsP.clear();
              slope.clear();
              intercept.clear();
              erslope.clear();
              erint.clear();
              numpointsW.clear();
              TotalAmp.clear();
              chi2.clear();
            }
        }
    }

  eventNumber++;

  if (Verbose > 0)
    {
      cout << "mTecHoughTrackModule: Finished event # " << eventNumber <<
	"            " << tecout->getNTracks() << " " << nHits << endl;
    }

  tecwires.clear();

  if (CopyTecOut)
    {
      //copyTecOut(root, eventNumber, LastEventWithHits, StripOrphanHits);
      copyTecTrackOut(root);
      copyTecHitOut(root, StripOrphanHits);
    }

  return True;
}

int
mTecHoughTrackModule::DoClustering2(int *Array,
                                    vector<TecTrackCluster> &ClusterQueue,
                                    int &maximum,
                                    int algorithm,
                                    int Debug)
{
  int value, rjadc, mAdc, radc;
  int offset, nadc, xadc, yadc, mEl_clu, nEl_clu, nGrow;
  unsigned int iadc;
  int jadc, i, j, num_clu, newADC, x, y;
  int low_cutoff_mod = 5;
  int low_cutoff_cent = 8;
  int local_max_constraint = 8;
  int start1, start2, stop1, stop2;

  int buf[m1][m2];
  vector<int> adc_value;
  vector<int> adc_number;
  TecTrackCluster Cluster;
  vector<int> nClu, mClu;

  float par1 = clPar[0];
  float par2 = clPar[1];
  float par3 = clPar[1];

  // find maximum for bins within angular limits
  int maxbin = m2 / 2 + int (float (m2) / 2. * angLim / y22);
  int minbin = m2 / 2 - int (float (m2) / 2. * angLim / y22);
  maximum = 0;
  for (i = 0; i < m1; i++)
    {
      for (j = 0; j < m2; j++)
        {
          if (j >= minbin && j <= maxbin)
            {
              offset = i + j * m1;
              value = Array[offset];
              if (value > maximum)
                {
                  maximum = value;
                }
            }
        }
    }

  if (maximum > 99)
    {
      low_cutoff_mod = (int) (maximum * par1);
      low_cutoff_cent = (int) (maximum * par2);
      local_max_constraint = (int) (maximum * par3);
    }
  if (maximum == 0)
    {
      return -1;
    }

  // fill adc queue
  for (int i = 0; i < m1; i++)
    {
      for (int j = 0; j < m2; j++)
        {
          buf[i][j] = 0;
          int offset = i + j * m1;
          int value = Array[offset];
          if (value > low_cutoff_mod)
            {
              adc_number.push_back(i + j*m1);
              adc_value.push_back(value);
            }
        }
    }

  if (adc_number.size() == 0)
    {
      return -2;
    }

  // start cluster analysis
  num_clu = 0;

  for (iadc = 0; iadc < adc_number.size(); iadc++)
    {
      nadc = adc_number[iadc];
      yadc = nadc / m1;
      xadc = nadc - yadc * m1;
      radc = adc_value[iadc];

      mEl_clu = 0;                      // no maxima found yet

      if (buf[xadc][yadc] == 0)
        {
          // module has not yet been used
          if (radc > low_cutoff_cent)
            { // if pulseheight is big enough

              num_clu++;                // increment cluster counter
              buf[xadc][yadc] = 1;      // mark as used
              nGrow = 1;                // 'run' counter
              nEl_clu = 1;              // the first cluster member has been found
              nClu.push_back(nadc);     // remember adc number

	    label1:
              mAdc = 0;                 // maximum ADC value is 0. LOOP START
              start1 = xadc - 1;
              stop1 = xadc + 1;
              if (start1 < 0)
                {
                  start1 = 0;
                }
              if (stop1 > m1 - 1)
                {
                  stop1 = m1 - 1;
                }
              start2 = yadc - 1;
              stop2 = yadc + 1;
              if (start2 < 0)
                {
                  start2 = 0;
                }
              if (stop2 > m2 - 1)
                {
                  stop2 = m2 - 1;
                }

              if (algorithm == 0)
                {
                  for (x = start1; x <= stop1; x++)
                    {
                      for (y = start2; y <= stop2; y++)
                        {
                          if ((x == xadc) || (y == yadc))
                            {             // different algorithm
                              if ((x != xadc) || (y != yadc))
                                {         // skip module itself
                                  jadc = x + y * m1;
                                  rjadc = Array[jadc];
                                  if (rjadc > low_cutoff_mod)
                                    {
                                      if (rjadc > mAdc)
                                        {
                                          mAdc = rjadc;
                                        }
                                      if (buf[x][y] == 0)
                                        {             // not yet used
                                          nClu.push_back(jadc);         // add module to cluster
                                          buf[x][y] = 1;                // mark it used
                                          nEl_clu++;            // increment number of modules
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
              else
                {
                  for (x = start1; x <= stop1; x++)
                    {
                      for (y = start2; y <= stop2; y++)
                        {
                          if ((x != xadc) || (y != yadc))
                            {     // skip module itself
                              jadc = x + y * m1;
                              rjadc = Array[jadc];
                              if (rjadc > low_cutoff_mod)
                                {
                                  if (rjadc > mAdc)
                                    {
                                      mAdc = rjadc;
                                    }
                                  if (buf[x][y] == 0)
                                    {         // not yet used
                                      nClu.push_back(jadc);     // add module to cluster
                                      buf[x][y] = 1;            // mark it used
                                      nEl_clu++;                // increment number of modules
                                    }
                                }
                            }
                        }
                    }
                }

              if ((radc > local_max_constraint) && (radc >= mAdc))
                {     // accept as local max
                  mClu.push_back(xadc + yadc * m1);
                  mEl_clu++;
                }

              nGrow++;

              if (nGrow > nEl_clu)
		{ }

              else
                {
                  newADC = nClu[nGrow - 1];                 // select next module
                  yadc = newADC / m1;
                  xadc = newADC - yadc * m1;
                  radc = Array[newADC];
                  goto label1;                          // start looking around next module
                }

              // Cluster found. Fill the cluster queue
              if ((int)mClu.size() != mEl_clu || (int)nClu.size() != nEl_clu)
                {
                  cerr << "DoClustering2 ERROR: " << mClu.size()
		       << " " << mEl_clu << " "
		       << nClu.size() << " " << nEl_clu << endl;
                }

              if (mClu.size() > 0)
                {
                  // if there is anything to fill
                  // store maxima
                  for (unsigned int j = 0; j < mClu.size(); j++)
                    {
                      Cluster.AddMaximum(mClu[j]);
                    }
                  // store modules
                  for (unsigned int j = 0; j < nClu.size(); j++)
                    {
                      Cluster.AddModule(nClu[j]);
                    }
                  ClusterQueue.push_back(Cluster);
                }

              Cluster.Clear();
              mClu.clear();
              nClu.clear();

            }
        }

    }

  adc_number.clear();
  adc_value.clear();

  return 0;
}

int
mTecHoughTrackModule::GetEntryExit (float slope,
                                    float intercept,
                                    TecGeometryObject * TGO,
                                    int index,
                                    int Debug,
                                    float &xx0,
                                    float &yy0,
                                    float &xx1,
                                    float &yy1,
                                    float &erxx0,
                                    float &eryy0,
                                    float &erxx1,
                                    float &eryy1)
{
  double xin, yin, xout, yout, zin;

  xin = 0.0;
  yin = intercept + slope * xin;
  zin = (index % 2 == 0) ? -100.0 : 100.0;
  xout = -500.0;
  yout = intercept + slope * xout;
  PHPoint pnt1 = PHPoint (xin, yin, zin);
  PHVector vct1 = PHVector (xout - xin, yout - yin, 0.);
  PHLine ln1 = PHLine (pnt1, vct1);

  int myindex1, myindex2;
  myindex1 = (index % 2 == 0) ? index * TECMAXPLANE
    : (index - 1) * TECMAXPLANE + 1;

  // Calculate "out" point for plane 5 (03/09/2004)
  myindex2 = myindex1 + 5 * TECMAXSIDE;

  PHPanel s1 = TGO->getTecPanel (myindex1);
  PHPlane ss1 = PHPlane (s1.getCenter (), s1.getNormal ());
  PHPanel s2 = TGO->getTecPanel (myindex2);
  PHPlane ss2 = PHPlane (s2.getCenter (), s2.getNormal ());
  PHPoint tmp1 = PHPoint ();
  PHPoint tmp2 = PHPoint ();
  intersectionLinePlane (ln1, ss1, tmp1);
  intersectionLinePlane (ln1, ss2, tmp2);

  xx0 = tmp1.getX ();
  yy0 = tmp1.getY ();
  xx1 = tmp2.getX ();
  yy1 = tmp2.getY ();
  erxx0 = 0.0;
  erxx1 = 0.0;
  eryy0 = 0.0;
  eryy1 = 0.0;

  return 0;
}


#endif /* __MTECHOUGHTRACKMODULE_CC__ */
