#include <cmath>
#include <algorithm>
using namespace std;

// Data classes for tracking
#include "dDchTracksWrapper.h"
#include "dDchHitWrapper.h"
#include "dCglTrackWrapper.h"
#include "dPadClusterWrapper.h"

// My headers
#include "TCrkProjector.h"
#include "find_Wrapper.h"

#include "gsl/gsl_fit.h"

//
// declarations of helper classes and struct
//

class DcFitter {
public:
  DcFitter(DDCHHIT_ST *hit);
  int fit(const DDCHTRACKS_ST *trk, float *x, float *u);
  virtual ~DcFitter() {}

private:
  DDCHHIT_ST *fHit;
  void CopyHits(const DDCHTRACKS_ST*,int,int,int*,double*,double*,double*,double*);
  void CopyUV(const DDCHTRACKS_ST*,int,int,double,double,int*,double*,double*,double*);
  void remove_worst_point(double *r,double *z,double *w,int n,double a,double b);
};

struct dc2proj {
  DcFitter d_fitter;
  CrkProj    *d_proj;

  dc2proj(DDCHHIT_ST *dchit, CrkProj *proj):d_fitter(dchit),d_proj(proj){}
  void operator()(const DDCHTRACKS_ST &);

  float calc_p(float x[], float u[]);
  void dc2rich(float x[], float u[]);
};

static void normalize3Dvec(float u[]) {
  float ulen_inv = 1.0/sqrt(u[0]*u[0]+u[1]*u[1]+u[2]*u[2]);
  u[0] *= ulen_inv;
  u[1] *= ulen_inv;
  u[2] *= ulen_inv;
}

static void move_base(float x[], float u[]);
//*********************************************
// Implimentations of public methods
//*********************************************
//------------------------
// class TCrkProjector
//------------------------

TCrkProjector::~TCrkProjector() {}

//------------------------
// class TCrkDchProjector
//------------------------
// This is the original Dch projection. The main part of the
// code was developed in 1998(?) in STAF era. Then, the rz plane
// fitting of DchTracks is not very reliable. Thus, I (YA) developed
// my own Dch hit fitting. To improve the RZ plane pointing accuray,
// the code assumes that the event vertex is Z=0
//
// The code is converted to C++ and made a part of pid() method
// of TCrkModule class in November, 1999.
//
// The code was extracted from TCrkModule.cc and made as an independent
// class. This change allows to use several different "projection" object
// for TCrkModule::pid() function. (February 19, 2000). This TCrkDchProjector
// was now used as "default" projector of the pid() function.
//

CrkProj* TCrkDchProjector::projection(PHCompositeNode *top, int *p_nproj)
{
  dDchTracksWrapper *track_wrap =
    find_Wrapper<dDchTracksWrapper>(top, "dDchTracks");
  if (track_wrap == NULL)
    return NULL;
  DDCHTRACKS_ST *track = track_wrap->TableData();
  int ntrack = track_wrap->RowCount();
  DDCHTRACKS_ST *track_end = track + ntrack;

  dDchHitWrapper *dchit_wrap = find_Wrapper<dDchHitWrapper>(top, "dDchHit");
  if (dchit_wrap == NULL)
    return (CrkProj *)0;
  DDCHHIT_ST *dchit = dchit_wrap->TableData();

  CrkProj *proj = new CrkProj[ntrack];
  for_each(track, track_end, dc2proj(dchit, proj));

  *p_nproj = ntrack;
  return proj;
}


//-----------------------------
// class TCrkDchTrackProjector
//-----------------------------
// This is a very simple class. This just copy the
// contents of DDCHTRACKS_ST into CrkProj.
// At this moment (February 2000), the R-Z reconstruction by
// the Drift Chamber is not very good. And as a result, this Projector
// is not very useful. It missed the RICH ring by large distance.
//
//
struct dchtrack2proj {
  CrkProj *d_proj;

  dchtrack2proj(CrkProj *proj):d_proj(proj){}
  void operator()(DDCHTRACKS_ST &t) {
    // note C++ allows this copy of array.
	for(int i=0;i<3;i++)
	  d_proj->x[i] = t.point[i];
	for(int i=0;i<3;i++)
	  d_proj->u[i] = t.direction[i];
    move_base(d_proj->x,d_proj->u);
    ++d_proj;
  }
};

CrkProj *TCrkDchTrackProjector::projection(PHCompositeNode *top, int *p_nproj)
{
  cout << "DchTrackProjector::projection() is called"<<endl;
  dDchTracksWrapper *track_wrap = 
    find_Wrapper<dDchTracksWrapper>(top,"dDchTracks");
  if(track_wrap == NULL) return NULL;
  DDCHTRACKS_ST *track = track_wrap->TableData();
  int ntrack = track_wrap->RowCount();
  DDCHTRACKS_ST *track_end = track + ntrack;

  CrkProj *proj = new CrkProj[ntrack];
  for_each(track,track_end,dchtrack2proj(proj));

  *p_nproj = ntrack;
  return proj;
}


//------------------------
// class TCrkCglProjector
//------------------------
// another attempt for "better" projector method
// From Cgl track, connect DC position and PC2 or PC3
// hits. If both hits are found, connect DC-PC3.
// If only PC2 or PC3 hit found, connect it with DC.
//

struct dCgl2proj {
  CrkProj *d_proj;
  DDCHTRACKS_ST  *d_dc;
  DPADCLUSTER_ST *d_pc1;
  DPADCLUSTER_ST *d_pc2;
  DPADCLUSTER_ST *d_pc3;

  dCgl2proj(CrkProj *proj, DDCHTRACKS_ST *dc,DPADCLUSTER_ST *pc1,
	    DPADCLUSTER_ST *pc2,DPADCLUSTER_ST *pc3)
    :d_proj(proj),d_dc(dc),d_pc1(pc1),d_pc2(pc2),d_pc3(pc3) {}

  void operator()(const DCGLTRACK_ST &t) {
    // set default "bad result"
    d_proj->x[0]=0; d_proj->x[1]=250; d_proj->x[2]=0;
    d_proj->u[0]=0; d_proj->u[1]=1.0; d_proj->u[2]=0;

    if(t.dctracksid < 0) {
      cout << "Error. Cgl without Dch track"<<endl;
    } else { // DC track found. Starting point of the projection line
             // is the point of the DC track.
      DDCHTRACKS_ST *dctrk = d_dc + t.dctracksid;
      if(t.pc1clusid>=0)
	for(int i=0;i<3;i++)
	  d_proj->x[i] = d_pc1[t.pc1clusid].xyz[i];
      else
	for(int i=0;i<3;i++)
	  d_proj->x[i] = dctrk->point[i];
      
      //      cout <<endl<<endl<<"========"<<endl;
      //      cout << "dctrk:x=(";
      //      cout <<dctrk->point[0]<<","<<dctrk->point[1]<<","<<dctrk->point[2];
      //      cout <<"),u=(";
      //      cout <<dctrk->direction[0]<<","<<dctrk->direction[1]<<",";
      //      cout <<dctrk->direction[2]<<")"<<endl;

      if(t.pc3clusid>=0) {
	// PC3 hit found. Connect it with DC point
	//
	//	cout << "PC3 found (";
	//	cout << d_pc3[t.pc3clusid].xyz[0]<<",";
	//	cout << d_pc3[t.pc3clusid].xyz[1]<<",";
	//	cout << d_pc3[t.pc3clusid].xyz[2]<<")"<<endl;
	for(int i=0;i<3;i++)
	  d_proj->u[i] = d_pc3[t.pc3clusid].xyz[i] - d_proj->x[i];
	normalize3Dvec(d_proj->u);
      } else if(t.pc2clusid>=0) {
	// PC2 hit, Connect DC and PC2
	//
	//	cout << "PC2 found (";
	//	cout << d_pc2[t.pc2clusid].xyz[0]<<",";
	//	cout << d_pc2[t.pc2clusid].xyz[1]<<",";
	//	cout << d_pc2[t.pc2clusid].xyz[2]<<")"<<endl;
	for(int i=0;i<3;i++)
	  d_proj->u[i] = d_pc2[t.pc2clusid].xyz[i] - d_proj->x[i];
	normalize3Dvec(d_proj->u);
      } else {
	// No PC2 nor PC3. Copy DC track direction (unit vec)
	//
	//	cout << "No PC "<<endl;
	for(int i=0;i<3;i++)
	  d_proj->u[i] = dctrk->direction[i];
      }
      //      cout<<"projection:x=(";
      //      cout<<d_proj->x[0]<<","<<d_proj->x[1]<<","<<d_proj->x[2]<<") u=(";
      //      cout<<d_proj->u[0]<<","<<d_proj->u[1]<<","<<d_proj->u[2]<<")"<<endl;
      //      cout<<"======="<<endl;
    }
    move_base(d_proj->x, d_proj->u);
    ++d_proj;
  }
};

TCrkCglProjector::TCrkCglProjector(){
  cout << "CglProjector is created"<<endl;
}

TCrkCglProjector::~TCrkCglProjector(){
  cout << "CglProjector is deleted"<<endl;
}

CrkProj *TCrkCglProjector::projection(PHCompositeNode *top, int *p_nproj)
{
  cout << "CglProjector::projection() is called"<<endl;
  dCglTrackWrapper *wrap = 
    find_Wrapper<dCglTrackWrapper>(top,"dCglTrack");
  if(wrap == NULL) return NULL;
  DCGLTRACK_ST *track = wrap->TableData();
  int ntrack = wrap->RowCount();
  DCGLTRACK_ST *track_end = track + ntrack;
  
  // prepare for data nodes that are needed for projection
  dDchTracksWrapper  *dcw =find_Wrapper<dDchTracksWrapper>(top,"dDchTracks");
  dPadClusterWrapper *pc1w=find_Wrapper<dPadClusterWrapper>(top,"dPc1Cluster");
  dPadClusterWrapper *pc2w=find_Wrapper<dPadClusterWrapper>(top,"dPc2Cluster");
  dPadClusterWrapper *pc3w=find_Wrapper<dPadClusterWrapper>(top,"dPc3Cluster");

  if(dcw == NULL || pc2w == NULL || pc3w == NULL) {
    *p_nproj = 0;
    return new CrkProj[1] ;
  }
  DDCHTRACKS_ST  *dc  = dcw->TableData();
  DPADCLUSTER_ST *pc1 = pc1w->TableData();
  DPADCLUSTER_ST *pc2 = pc2w->TableData();
  DPADCLUSTER_ST *pc3 = pc3w->TableData();

  // Now do the projection
  CrkProj *proj = new CrkProj[ntrack];
  
  for_each(track,track_end,dCgl2proj(proj,dc,pc1,pc2,pc3));

  *p_nproj = ntrack;
  return proj;
}
//***********************************************
// Implimentations of helper (private) classes
//***********************************************
//
// helper for functions of TCrkDchProjector
//
void dc2proj::operator()(const DDCHTRACKS_ST &dctrack) {
  static const CrkProj empty_proj = {-1,{0.0,0.0,0.0},{0.0,0.0,0.0}};

  int stat = d_fitter.fit(&dctrack,d_proj->x,d_proj->u);
  if(stat > 0) { //fitting OK
    d_proj->track_id = dctrack.trackid;
    dc2rich(d_proj->x,d_proj->u);
    move_base(d_proj->x,d_proj->u);
  } else {
    *d_proj = empty_proj;
  }
  ++d_proj;
}

float dc2proj::calc_p(float x[], float u[]) {
  float rx = sqrt(x[0]*x[0]+x[1]*x[1]);
  float ru = sqrt(u[0]*u[0]+u[1]*u[1]);
  float beta  = asin((x[0]*u[1] - x[1]*u[0])/(rx*ru));
  float p = (0.08/beta)/
      (1.0197-0.179e-4*x[2]*x[2]+0.976e-9*x[2]*x[2]*x[2]*x[2]);
  p *= (258.8/rx);    // rescale it from rx to R=258.8 cm
  //cout << "("<<x[0]<<","<<x[1]<<","<<x[2]<<")";
  //cout << "("<<u[0]<<","<<u[1]<<","<<u[2]<<")";
  //cout << "beta = "<<beta<<endl;
  return p;
}

void dc2proj::dc2rich(float x[], float u[]) {
  // Additional magnetic bend in xy plane
  // the bending angle is a function of z0.
  // dbeta/beta = 0.075 - 0.000014*z0*z0
  // Also use small angle approximation beta ~ sing(beta)
  //
  float rx = sqrt(x[0]*x[0]+x[1]*x[1]);
  float ru = sqrt(u[0]*u[0]+u[1]*u[1]);
  float beta  = asin((x[0]*u[1] - x[1]*u[0])/(rx*ru));
  float par1,par2;
  float abeta = fabs(beta);

//   if(abeta<0.08) par2 = 0.000012;
//   else if(abeta<0.16) par2 = 0.000012 + (abeta-0.08)*2.5e-5;
//   else par2=0.000014;
//  par1 = 0.075;

  if(abeta<=0.16) par1 = 0.075;
  else par1 = 0.075 - 0.043*(abeta-0.16);

  par2 = 0.000014;

  float dbeta = beta*(par1 - par2*x[2]*x[2]);
  float sin_db = sin(dbeta);
  float cos_db = sqrt(1.0 - sin_db*sin_db);
  float ux = cos_db*u[0] - sin_db*u[1];
  float uy = sin_db*u[0] + cos_db*u[1];
  u[0] = ux;
  u[1] = uy;

// focusing effect correction
//   float duz = -0.004*x[2]*beta*beta;
//   u[2] += duz;
//
// Calcualte u[2] from the Z-position of the track.
// This parameterization is tuned only for Zvtx = 0
// This will be generalized for general Zvtx value later.
  {
    float p = calc_p(x,u);
    float R0 = 258.8 + 3.75/(p*p) + 0.126/(p*p*p*p);
    //    cout << " p = " << p << endl;
    float R = sqrt(x[0]*x[0] + x[1]*x[1]);
    float zz = R0*x[2]/R;
    u[2] = zz/R0 - 0.3e-7*zz*zz*zz;
  }


  // renormalize u vector
  normalize3Dvec(u);
  //  cout << " after norm: p = "<< calc_p(x,u) <<endl;
}

static void move_base(float x[], float u[]) {
  // move the base point x[] at R=258 cm
  float R1 = 258.8;
  float R0 = sqrt(x[0]*x[0]+x[1]*x[1]);
  int ntry = 0;
  do {
    float dl = (R1 - R0)/sqrt(u[0]*u[0]+u[1]*u[1]);
    x[0] = x[0] + dl*u[0];
    x[1] = x[1] + dl*u[1];
    x[2] = x[2] + dl*u[2];
    R0 = sqrt(x[0]*x[0]+x[1]*x[1]);
    ntry++;
  } while( fabs(R0 - R1)>0.001 || ntry < 5);
}

//******************************
// DcFitter member functions   * 
//******************************
// This is a helper class, and it does a straight line fit
// to a Dc track.
//
void DcFitter::remove_worst_point(double *r, double *z, double *w, 
			int n, double a, double b)
{
  // z = ar + b
  int imax = 0;
  double dchi2_max = 0;
  double sum = 0;
  int i;
  for(i=0;i<n;i++) {
    double dz = z[i] - a*r[i] - b;
    double dchi2 = w[i]*dz*dz;
    if(dchi2 > dchi2_max) {
      imax = i;
      dchi2_max = dchi2;
    }
    sum += dchi2;
  }
  if(imax < n-1)
    for(i=imax;i<n-1;i++) {
      r[i] = r[i+1];
      z[i] = z[i+1];
      w[i] = w[i+1];
    }
  //cout<< " REMOVE worst point"<<endl;
  //cout<< "n="<<n<<" chi2 = "<<sum<<" chi2_max="<<dchi2_max<<endl;
}

DcFitter::DcFitter(DDCHHIT_ST *hit) {
  fHit = hit;
  cout << " DcFitter initialized"<<endl;
}

int DcFitter::fit(const DDCHTRACKS_ST *trk, float x0[3], float u0[3]) {
  int n;
  double x[12];
  double y[12];
  double w[12];
  double r[16];
  double z[16];
  double width[16];

  double a1,b1,chi1;
  double a2,b2,chi2;
  double a3,b3,chi3;
  double c00,c01,c11;
  CopyHits(trk,0,11,&n,x,y,w,width);
  if(n < 4) return 0;
  const int stride = 1;
  gsl_fit_wlinear(x,stride,w,stride,y,stride,n,&b1,&a1,&c00,&c01,&c11,&chi1);
  if(chi1/(n-2) > 5 && n > 4) {
    int nremove = 0;
    do {
      remove_worst_point(x,y,w,n,a1,b1);
      n--;
      nremove++;
      gsl_fit_wlinear(x,stride,w,stride,y,stride,n,&b1,&a1,&c00,&c01,&c11,&chi1);

    } while(chi1/(n-2)> 5 && n > 4 && nremove < 3);
  }

  CopyHits(trk,20,31,&n,x,y,w,width);
  if(n < 4) return 0;
  gsl_fit_wlinear(x,stride,w,stride,y,stride,n,&b2,&a2,&c00,&c01,&c11,&chi2);
  if(chi2/(n-2) > 5 && n > 4) {
    int nremove = 0;
    do {
      remove_worst_point(x,y,w,n,a2,b2);
      n--;
      nremove++;
      gsl_fit_wlinear(x,stride,w,stride,y,stride,n,&b2,&a2,&c00,&c01,&c11,&chi2);
    } while(chi2/(n-2)> 5 && n > 4 && nremove < 3);
  }

  int n2 = n - 1;

  n = 0;
  CopyUV(trk,12,19,a1,b1,&n,r,z,w);
  CopyUV(trk,32,39,a2,b2,&n,r,z,w);
  if(n < 4) return 0;
  gsl_fit_wlinear(r,stride,w,stride,r,stride,n,&b3,&a3,&c00,&c01,&c11,&chi3);
  if(chi3/(n-2) > 5 && n > 6) {
    int nremove = 0;
    do {
      remove_worst_point(r,z,w,n,a3,b3);
      n--;
      nremove++;
  gsl_fit_wlinear(r,stride,w,stride,r,stride,n,&b3,&a3,&c00,&c01,&c11,&chi3);
    } while(chi3/(n-2)> 5 && n > 6 && nremove < 3);
  }


  // output x0[] and u0[]...a track vector pointing to outwards
  // u0[2] is from a3. (Fit to UV planes)
  // u0[0],u0[1] are from a2 (fit in DC2XY planes)
  //
  // Note: a3 = cotT = cosT/sinT
  //       uz = cosT = cotT*sinT = a3*sinT
  //       sinT = 1.0/(1.0 + cotT**2)
  //       a2 = tanP = uy/ux
  //
  float sinT = 1.0/sqrt(1.0 + a3*a3);
  float cosP = 1.0/sqrt(1.0 + a2*a2);
  if(x[0]<0) cosP = -cosP;
  float sinP = a2*cosP;
  u0[2] = a3*sinT;
  u0[0] = sinT*cosP;
  u0[1] = sinT*sinP;
  
  // basepoint x0[] is defined as having the same x coordinate
  // as x[n2] point...this should be the outmost point in DC2XY
  x0[0] = x[n2];
  x0[1] = a2*x0[0] + b2;
  x0[2] = a3*sqrt(x0[0]*x0[0] + x0[1]*x0[1]) + b3;
  
  //  cout << " a3="<<a3<<" b3="<<b3<<" u0[2]="<<u0[2]<<endl;
  return 1;
}

void DcFitter::CopyHits(const DDCHTRACKS_ST *trk, int i1, int i2,
			    int *pn, double *x, double *y, double *w, double *pw)
{
  int n = 0;
  for(int i=i1;i<=i2;i++) {
    int ih = trk->hits[i];
    if(ih != -1) {
      DDCHHIT_ST *h = fHit + ih;
      x[n] = h->xyz[0];
      y[n] = h->xyz[1];
      w[n] = 10000.0; // sigma_y ~ 0.01 --> w = 1/sigma_y**2 ~ 10000
      pw[n] = h->width;
      n++;
    }
  }
  *pn = n;
}

void DcFitter::CopyUV(const DDCHTRACKS_ST *trk,int i1,int i2,double a, double b,
			   int *pn, double *r, double *z, double *w)
{
  int n = *pn;
  for(int i=i1;i<=i2;i++) {
    int ih = trk->hits[i];
    if(ih != -1) {
      DDCHHIT_ST *h = fHit + ih;
      float *x0 = h->xyz;
      float *v0 = h->vxyz;
      double t = (a*x0[0] + b - x0[1])/(v0[1] - a*v0[0]);
      double x = x0[0] + t*v0[0];
      double y = x0[1] + t*v0[1];
      double zz = x0[2] + t*v0[2];
      double rr = sqrt(x*x + y*y);

//      cout << h->plane <<" :("<< x <<","<< y <<","<< zz <<") "<< rr <<endl; 

      r[n] = rr;
      z[n] = zz;
      w[n] = 100.0; // sigma_z ~ 0.1 --> w = 1/sigma_y**2 ~ 100
      n++;
    }
  }
  *pn = n;
}



