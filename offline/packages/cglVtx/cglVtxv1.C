#include "cglVtxv1.h"
#include "phool.h"
#include "DchTrack.h"
#include "CglTrack.h"
#include "PHTrackOut.h" 
#include "PadCluster.h"
#include "PHIODataNode.h"

#include "gsl/gsl_fit.h"
//INCLUDECHECKER: Removed this line: #include "gsl/gsl_rng.h"

//INCLUDECHECKER: Removed this line: #include <cmath>
//INCLUDECHECKER: Removed this line: #include <vector>
#include <valarray>
//INCLUDECHECKER: Removed this line: #include <numeric>

using namespace std;

inline double
sqr(double x)
{
  return x * x;
}

static int
findClusterPosition(valarray<double> &tp, int arm, PadCluster *p, 
		    double cut_2, 
		    valarray<double> &cp)
{
  int j, n;
  double d_2;
  valarray<double> x(3), t(3);
  int N = p->get_PadNCluster();

  cp = 0.0;

  n = 0;
  for (j = 0; j < N; j++)
    {
      if (p->get_arm(j) != arm) continue;

      x[0] = p->get_xyz(j,0);
      x[1] = p->get_xyz(j,1);
      x[2] = p->get_xyz(j,2);
      t = (x - tp) * (x - tp);
      d_2 = t.sum();

      if (d_2 > cut_2) continue;

      n++;
      cp += x;
    }
  if (n == 0) return 1;

  // Determine the average location of the PC3 clusters that are
  // close to the place where this track intersects the chamber.
  cp /= n;
  
  return 0;
}

inline bool
goodPadHit(const valarray<double> &p)
{
  return (p[0] > -9000.0);
}

PHBoolean
cglVtxv1::event(PHCompositeNode *node)
{
  // Our tracking seems to assume that the vertex given by the BBC (or
  // whatever) is *the* true event vertex.  We really have to do the
  // re-projections to the beam axis ourselves to get an independent
  // measure of the vertex position.  The basic plan will be to get
  // the PC1 and PC2 or PC3 hits for each track and project to the
  // beam axis.  Build up a good estimate of the vertex position and
  // then filter tracks that don't point to it.  With just PC1 and PC3
  // you have about a 2cm uncertainty on the vertex pointing for an
  // individual track.  Not so precise, really.

  // Until we decide otherwise, this vertex is invalid.
  _v = false;
  _z = -9999.0;
  _zerr = -9999.0;

  // Declare pointers to our objects on the dst (0 if object is not found)
  PHIODataNode<DchTrack> *dch_n;
  PHIODataNode<CglTrack> *cgl_n;
  PHIODataNode<PHTrackOut> *phtrk_n;
  PHIODataNode<PadCluster> *pc1_n, *pc2_n, *pc3_n;
  
  PHIODataNode<DchTrack>::iterator d_i(node);
  PHIODataNode<CglTrack>::iterator c_i(node);
  PHIODataNode<PHTrackOut>::iterator p_i(node);
  PHIODataNode<PadCluster>::iterator pc1_i(node);
  PHIODataNode<PadCluster>::iterator pc2_i(node);
  PHIODataNode<PadCluster>::iterator pc3_i(node);

  dch_n    = d_i.find("DchTrack");
  cgl_n    = c_i.find("CglTrack");
  phtrk_n  = p_i.find("PHTrackOut");
  pc1_n 	 = pc1_i.find("Pc1Cluster");
  pc2_n 	 = pc2_i.find("Pc2Cluster");
  pc3_n 	 = pc3_i.find("Pc3Cluster");

  // require a PC3 hit within 2.0cm - compared squared values to avoid
  // always taking sqrts.
  const double cut_2 = sqr(2.0); 
  valarray<double> pc1trk(3), pc2trk(3), pc3trk(3);
  valarray<double> pc1clu(3), pc2clu(3), pc3clu(3);
  double r[3], z[3], w[3];
  //  double phi;
  
  int N, n, i;
  int di, quality, arm;
  float zv, zv_err, weight, p;
  //  double pvtx, pvtx_err;
  int nv, pci;
  double c0, c1, cov00, cov01, cov11, chisq;
  // (x,y) location of the beam axis - assume (0,0) until we have
  // better info.
  double x0 = 0.0, y0 = 0.0, z0, z0_err, r0; 

  // We demand tracks that have at worst a possibly ambiguous PC1 hit
  // and a UV match found.  See DchTrack.h for info.
  const unsigned short qualityMask = (unsigned short) 0x011100; 
  r0 = hypot(x0, y0);
  if (!(dch_n && cgl_n && phtrk_n && pc1_n && pc2_n && pc3_n))
    {
      return False;
    }

  DchTrack *dch = dch_n->getData();
  CglTrack *cgl = cgl_n->getData();
  PHTrackOut *phtrk = phtrk_n->getData();
  PadCluster *pc1 = pc1_n->getData();
  PadCluster *pc2 = pc2_n->getData();
  PadCluster *pc3 = pc3_n->getData();
  
  // Find out how many tracks there are in this event.  In a very low
  // multiplicity event, there just might not be enough tracks to
  // improve the vertex estimate beyond that provided by other
  // methods. 
  N = phtrk->get_PHNTrack();

  // Loop over tracks and calculate the average z-vertex of each
  // well-reconstructed track.  Compare this to the vertex from
  // VtxOut. 
  //pvtx = 0.0;
  // pvtx_err = 0.0;
  zv = 0.0;
  zv_err = 0.0;
  nv = 0;
  for (i = 0; i < N; i++)
    {
      di = cgl->get_dctracksid(i);
      quality = dch->get_quality(di);
      p = dch->get_momentum(di);

      // Low quality or low momentum tracks (because of their large
      // bend, even in the "non-bend" plane) just won't do.
      if (quality & qualityMask || p < 1.0) continue;
      
      //phi = dch->get_phi0(di);

      // We start off with 0 points for this track.
      n = 0;

      // The projection to the vertex is sort of worthless since it
      // just parrots back the vertex that was provided by the BBC.
      //pvtx += phtrk->get_projectionVtx(i,2);

      arm = dch->get_arm(di);
      
      // We've selected tracks that have PC1 hits, but we have to hunt
      // for appropriate PC2 and/or PC3 hits.
      pci = dch->get_pc1hit(di);

      if (pci < 0) continue;

      pc1trk[0] = pc1->get_xyz(pci,0);
      pc1trk[1] = pc1->get_xyz(pci,1);
      pc1trk[2] = pc1->get_xyz(pci,2);
      r[n] = hypot(pc1trk[0], pc1trk[1]);
      z[n] = pc1trk[2];
      w[n++] = 1.0 / sqrt(0.24); // I.e., variance = 0.5cm.

      pc2trk[0] = phtrk->get_projectionPc2(i, 0);
      pc2trk[1] = phtrk->get_projectionPc2(i, 1);
      pc2trk[2] = phtrk->get_projectionPc2(i, 2);
	 
      pc3trk[0] = phtrk->get_projectionPc3(i, 0);
      pc3trk[1] = phtrk->get_projectionPc3(i, 1);
      pc3trk[2] = phtrk->get_projectionPc3(i, 2);

      // We know where the track intersects PC2/PC3 (if it intersects
      // either of them at all) let's find the associated cluster.
      if (goodPadHit(pc2trk)) 
	{
	  if (findClusterPosition(pc2trk, arm, pc2, cut_2, pc2clu) == 0)
	    {
	      r[n] = hypot(pc2clu[0], pc2clu[1]);
	      z[n] = pc2clu[2];
	      w[n++] = 1.0 / sqrt(0.46);
	    }
	}
      if (goodPadHit(pc3trk)) 
	{
	  if (findClusterPosition(pc3trk, arm, pc3, cut_2, pc3clu) == 0)
	    {
	      r[n] = hypot(pc3clu[0], pc3clu[1]);
	      z[n] = pc3clu[2];
	      w[n++] = 1.0 / sqrt(0.46);
	    }
	}
      
      if (n < 2) continue;

      // For the following to be true, r0 does need to be on the same
      // side of the (0,0) axis as the PC hits.  It's true if r0 = 0,
      // but not true in general.
      gsl_fit_wlinear(r, 1, w, 1, z, 1, 
		      n, 
		      &c0, &c1, 
		      &cov00, &cov01, &cov11, 
		      &chisq);
      // Estimate the intersection of this best fit line with the beam
      // axis parallel to (x,y) = (0,0).  We're never going to collide
      // things at a large enough angle for us to have to do anything
      // about that aspect here.
      gsl_fit_linear_est(r0, 
			 c0, c1, 
			 cov00, cov01, cov11, 
			 &z0, &z0_err);
      nv++;
      weight = 1.0/ sqr(z0_err);
      zv += weight * z0;
      // z0_err is the standard deviation, sigma.  To accumulate the
      // errors, we add the square of the standard deviations.
      zv_err += weight;
    }

  // Not enough tracks to continue
  if (nv < 5) 
    {
      return True;
    }

  // zv is the average z-vertex position determined from all the
  // tracks in this event.
  zv /= zv_err;

  // Store these values in our data members
  _v = true;
  _z = zv;
  _zerr = zv_err;

  return True;
}
  
