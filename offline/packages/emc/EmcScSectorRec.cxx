// Name: EmcScSectorRec.cxx
// Author: A. Bazilevsky (RIKEN-BNL)
// Major modifications by M. Volkov (RRC KI) Jan 27 2000

// ///////////////////////////////////////////////////////////////////////////

/*
A.Bazilevsky (IHEP, Protvino) - Dec. 1998
(from March 99 - RIKEN-BNL Research Center)

shura@bnl.gov

Cluster-Shower reconstruction routines based on the A.Lednev's 
method developed for GAMS (IHEP-CERN).

The Classes and Basic Functions description is in cluster.h

The method is based on the parameterized shower profile (energy deposited 
in the tower as a function of the distance between tower center and shower 
Center of Gravity, shower energy and impact angle). No difference between 
the shower produced by electrons or gammas is assumed.

************ The shower reconstruction follows three steps: *********

1.  Cluster search. A cluster is one or several neighboring cells 
separated from the other clusters with zero cells. The cluster are analyzed 
independently. 
Implementation: Class "cluster". The list of clusters is obtained via 
"Find_Clusters" function that has the list of EmcModules as an input (look at 
"EmcModule" structure description in cluster.h). For the brief description of 
the "cluster" methods look in cluster.h.

2.  Each cluster may contain several peaks. A peak is located in a cell whose 
deposited energy is higher then that of any adjoined cell. Peak regions are 
calculated by sharing the energy deposited in each cell according the  
energies expected from the gammas located in each peak region. This is 
iterative procedure. 
Implementation: Class "EmcPeakarea"; most of the methods are inherited from 
"cluster". The array of peakareas for the given cluster is obtained via 
"cluster::GetPeaks( EmcPeakarea*, EmcModule* )" method. For the brief description of 
the "EmcPeakarea" methods look in cluster.h.

3.  Gamma reconstruction within the peak region. One or two gammas within a 
peak region. Minimizes Hi**2 function in two-dimensional space ( X,Y shower 
position ). If it is more then some limit, two-gamma hypothesis is taken. 
The two-gamma separation is done by minimizing Hi**2 function with the 
maximum slope method in three-dimensional space: a=(E1-E2)/E0, dX=X1-X2, 
dY=Y1-Y2. 
Implementation: Class "EmcEmshower". The array of EmcEmshower's for the given 
peakarea is obtained via "EmcPeakarea::GetGammas( EmcEmshower* )" method. 
For the brief description of the "EmcEmshower" methods look in cluster.h.

************* Main tips for using these library ***************

1. Parameters setting

SetThreshold(TowerThresh) - sets the threshold (GeV) in each tower; 
needed for the calculation of sigmas of the energy fluctuation in the 
towers; default value is 0.02 GeV;

SetChi2Limit(i) - sets Chi2 limit for the splitting of peakarea onto 
two showers; default value is 1;

2. Program flow

All sectors are processed independently. Everything starts with 
Find_Clusters call which returns the list of clusters. It has 
the list of EmcModules as an input. Towers are counted from 0. 
Before every Find_Cluster call user should set the sector geometry 
and vertex position via SetGeometry( SecGeom Geom, float* pVert) call. 

As soon as one gets the list of clusters, he can do whatever he wants: 
get cluster's characteristics, split each of them onto peakarea's, 
get peakarea's characteristics, split each of them onto EmcEmshower's, 
get EmcEmshower's characteristics. 

3. Shower Profile

The parameterized Shower Profile is used when splitting cluster onto 
peakareas and peakarea onto EmcEmshowers. The parameters for the Shower 
Profile calculation are set via SetProfileParameters(...) function for 
each peakarea object. The energy deposited in the particular tower is 
obtained via PredictEnergy(...) function (based on the Shower Profile)

*/

#include "EmcScSectorRec.h"
#include "EmcCluster.h"
#include "emcTowerContent.h"
#include "EmcIndexer.h"

#include <iostream>

// ///////////////////////////////////////////////////////////////////////////
// Define and initialize static members

// Parameters for sigma in Hi2 calculations (p.36-37 v.3)
float EmcScSectorRec::fgEpar00 = 0.005;
float EmcScSectorRec::fgEpar0 = 0.0014;
float EmcScSectorRec::fgEpar1 = 0.03;
float EmcScSectorRec::fgEpar2 = -0.03;
// This is for PPLO mode !!!
float EmcScSectorRec::fgEpar3 = 0.;
float EmcScSectorRec::fgEpar4 = 4.0;

// ///////////////////////////////////////////////////////////////////////////
// EmcScSectorRec member functions

void EmcScSectorRec::CorrectEnergy(float Energy, float x, float y, 
				   float* Ecorr)
{
  // Corrects the EM Shower Energy for attenuation in fibers and 
  // long energy leakage
  //
  // (x,y) - impact position (cm) in Sector frame

  float sinT;
  float att, leak, corr;
  const float leakPar = 0.0033; // parameter from fit
  const float attPar = 120; // Attenuation in module (cm)
  const float X0 = 2; // radiation length (cm)

  *Ecorr = Energy;
  if( Energy < 0.01 ) return;

  GetImpactAngle(x, y, &sinT); // sinT not used so far
  leak = 2-sqrt(1+leakPar*log(1+Energy)*log(1+Energy));
  att = exp(log(Energy)*X0/attPar);
  corr = leak*att;
  *Ecorr = Energy/corr;
}

/////////////////////////////////////////////////////////////////

void EmcScSectorRec::CorrectECore(float Ecore, float x, float y, float* Ecorr)
{
  // Corrects the EM Shower Core Energy for attenuation in fibers, 
  // long energy leakage and angle dependance
  //
  // (x,y) - impact position (cm) in Sector frame

  float ec, ec2, corr;
  float sinT;
  const float par1 = 0.918;
  const float par2 = 1.35;
  const float par3 = 0.003;

  *Ecorr = Ecore;
  if( Ecore < 0.01 ) return;

  GetImpactAngle(x, y, &sinT );
  corr = par1 * ( 1 - par2*sinT*sinT*sinT*sinT*(1 - par3*log(Ecore)) );
  ec = Ecore/corr;

  CorrectEnergy( ec, x, y, &ec2);
  *Ecorr = ec2;
}

/////////////////////////////////////////////////////////////////////

void EmcScSectorRec::CorrectPosition(float Energy, float x, float y,
				     float* pxc, float* pyc, bool callSetPar)
{
  // Corrects the Shower Center of Gravity for the systematic error due to 
  // the limited tower size and angle shift
  //
  // Everything here is in cm. 
  // (x,y) - CG position, (*pxc,*pyc) - corrected position

  //  std::cout << __FILE__ << "  " << __LINE__ << " CorrectPosition "<< std::endl;


  float xShift, yShift, xZero, yZero, bx, by;
  float t, x0, y0;
  int ix0, iy0;

  SetProfileParameters( 0, Energy, x/GetModSizex(), y/GetModSizey() );

  t = 1.93+0.383*log(Energy);
  xShift = t*fSinTx;
  yShift = t*fSinTy;

  if( fSinTx > 0 ) 
    {
      //      xZero=xShift -  0.417 *fSinTx - 1.500 * fSinTx * fSinTx ;
      xZero=xShift -  0.417 *fSinTx - 1.500 * sin2ax ;
    }
  else
    {
      //      xZero=xShift +  0.417 *fSinTx - 1.500 * fSinTx * fSinTx ;
      xZero=xShift -  0.417 *fSinTx + 1.500 * sin2ax ;
    }      

  if( fSinTy > 0 ) 
    {
      yZero = yShift - 0.417 * fSinTy - 1.500 * sin2ay;
    }
  else
    {
      yZero = yShift - 0.417 * fSinTy + 1.500 * sin2ay;
    }

  //  yZero=yShift-(0.417*EmcCluster::ABS(fSinTy)+1.500*fSinTy*fSinTy)*signy;

  t = 0.98 + 0.98*sqrt(Energy);
  bx = 0.16 + t*sin2ax;
  by = 0.16 + t*sin2ay;
  
  x0 = x/GetModSizex() - xShift + xZero;
  ix0 = EmcCluster::lowint(x0 + 0.5);
  if( EmcCluster::ABS(x0-ix0) <= 0.5 ) {
    x0 = (ix0-xZero)+bx*asinh( 2.*(x0-ix0)*sinh(0.5/bx) );
    *pxc = x0*GetModSizex();
  }
  else {
    *pxc =  x - xShift*GetModSizex();
    printf("????? Something wrong in CorrectPosition of EMCalClusterChi2: x=%f  dx=%f\n", x, x0-ix0);
  }

  y0 = y/GetModSizey() - yShift + yZero;
  iy0 = EmcCluster::lowint(y0 + 0.5);

  if( EmcCluster::ABS(y0-iy0) <= 0.5 ) {

    y0 = (iy0-yZero)+by*asinh( 2.*(y0-iy0)*sinh(0.5/by) );
    *pyc = y0*GetModSizey();

  }
  else {

    *pyc = y - yShift*GetModSizey();
    printf("????? Something wrong in CorrectPosition of EMCalClusterChi2: y=%f  dy=%f\n", y, y0-iy0);

  }
  
}

// ///////////////////////////////////////////////////////////////////////////

void EmcScSectorRec::CalculateErrors( float e, float x, float y, float* pde,
				   float* pdx, float* pdy, float* pdz)
{
  // Returns the errors for the reconstructed energy and position 
  // (in the hypothesis of EM shower)
  // Should be called only just after CorrectPosition !!!

  float de, dy, dz, dxg, dyg, dzg;
  static float ae = 0.076, be = 0.022;  	// de/E = a/sqrt(E)&b
  static float a = 0.57, b = 0.155, d = 1.6;	// dx = a/sqrt(E)+b (cm)
  static float dx = 0.1;  // (cm)
  
  de = sqrt( ae*ae*e + be*be*e*e );
  dz = a/sqrt(e) + b;
  dy = dz;
  dz = sqrt( dz*dz + d*d*sin2ax );
  dy = sqrt( dy*dy + d*d*sin2ax );
  
  SectorToGlobalErr( dx, dy, dz, &dxg, &dyg, &dzg );
  
  *pde = de;
  *pdx = dxg;
  *pdy = dyg;
  *pdz = dzg;

}

// ///////////////////////////////////////////////////////////////////////////

void EmcScSectorRec::SetProfileParameters(int sec, float Energy, float x,
					float y )
{

  //  std::cout << __FILE__ << "  " << __LINE__ << " setprofileparameters " <<  sec << "  " << Energy << "  " <<  x << "  " << y << std::endl;


  // Axis Z here means X in two dim Sector coord system !!! 
  // So one has to supply (x,y) coordinates in cell units (x instead of z)
  // If sec < 0 this routine changes only Energy dependent parameters - 
  // the angle dependent ones are set in the previous call

  float t;
  //  static float sin2ax, sin2ay, sin2a, lgE;
  float vx, vy, vz;
  float xVert, yVert, zVert;
  int sign;
  
  if( sec >= 0 ) {
    // Vertex coordinates in Sector coordinate system
    GlobalToSector( fVx, fVy, fVz, &xVert, &yVert, &zVert );
    vz = -zVert;
    // It is if we use the coordinates of the Sector Center (not zero-tower)
    vy = y*fModSizey - yVert;
    vx = x*fModSizex - xVert;
    // From this point X coord in sector frame is Z coord in Global Coord System !!!
    fSinTx = vx/sqrt(vx*vx+vz*vz);
    fSinTy = vy/sqrt(vy*vy+vz*vz);
    if( EmcCluster::ABS(fSinTx) > 0.7 || EmcCluster::ABS(fSinTy) > 0.7 ) 
      printf("!!!!! Something strange in SetProfileParmeters of mEmcClusterChi2: fSinTx=%f  fSinTy=%f\n",fSinTx, fSinTy);
    t = (vx*vx+vy*vy)/(vx*vx+vy*vy+vz*vz);
    sin2a=t;
    fSin4T=t*t;
    sin2ax=fSinTx*fSinTx;
    sin2ay=fSinTy*fSinTy;
  }
  
  if( Energy <= 1.e-10 ) lgE=0;
  else lgE=log(Energy);
  
  fPpar1=0.59-(1.45+0.13*lgE)*sin2a;
  fPpar2=0.265+(0.80+0.32*lgE)*sin2a;
  fPpar3=0.25+(0.45-0.036*lgE)*sin2a;
  fPpar4=0.42;
  
  if( fSinTx > 0 ) sign = 1;
  else sign = -1;
  fPshiftx = (1.05+0.12*lgE) * sin2ax * sign;
  
  if( fSinTy > 0 ) sign = 1;
  else sign = -1;
  fPshifty = (1.05+0.12*lgE) * sin2ay * sign;
}

// ///////////////////////////////////////////////////////////////////////////

float EmcScSectorRec::PredictEnergy(float xc, float yc, float en)
{
  // Calculates the energy deposited in the tower, the distance between 
  // its center and shower Center of Gravity being (xc,yc)
  // en - shower energy
  // If en<0 -> no Shower Profile parameters change is needed

  float dx, dy, r1, r2, r3, e;
  
  if( en > 0 ) SetProfileParameters(-1,en,xc,yc);
  dx=fabs(xc-fPshiftx);
  dy=EmcCluster::ABS(yc-fPshifty);
  r2=dx*dx+dy*dy;
  r1=sqrt(r2);
  r3=r2*r1;
  e=fPpar1*exp(-r3/fPpar2)+fPpar3*exp(-r1/fPpar4);
  
  return e;

}

// ///////////////////////////////////////////////////////////////////////////

void EmcScSectorRec::TwoGamma(int nh, emcTowerContent **phit, float* pchi, float* pe1,
			   float* px1, float* py1, float* pe2, float* px2,
			   float* py2)
{

  float e0, x0, y0, xx, yy, yx;
  float dxy, rsg2, rsq;
  float dxc, dyc, r, epsc;
  int ix, iy, in, iter, dof;
  int iS,iST;

  float step, chisq2, u;
  float e1c, x1c, y1c, e2c, x2c, y2c;
  float eps0 = 0.0;
  float eps1, eps2, chisqc, ex;
  float dx1, dy1, dx2, dy2, a0, d;
  float dchi, dchi0, dd, dchida, a1, a2;
  float gr = 0.0;
  float grec, grxc, gryc, grc, gx1, gx2, gy1, gy2;
  float gre = 0.0;
  float grx = 0.0;
  float gry = 0.0;
  float scal;
  float dx0 = 0.0;
  float dy0 = 0.0;
  
  const float epsmax=0.9999;
  const float stpmin=0.025;
  const float delch=2;
  
  Momenta(nh,phit,&e0,&x0,&y0,&xx,&yy,&yx);
  *pe2 = 0;
  *px2 = 0;
  *py2 = 0;
  if( nh <= 0 ) return;
  //  choosing of the starting point
  dxy = xx-yy;
  rsg2 = dxy*dxy + 4*yx*yx;
  if( rsg2 < 1e-20 ) rsg2 = 1e-20;
  rsq = sqrt(rsg2);
  dxc = -sqrt((rsq+dxy)*2);
  dyc =  sqrt((rsq-dxy)*2);
  if( yx >= 0 ) dyc = -dyc;
  r = sqrt(dxc*dxc + dyc*dyc);
  epsc = 0;
  for( in=0; in<nh; in++ ) 
    {
      EmcIndexer::iPXiSiST((phit[in])->TowerID(),iS,iST);
      EmcIndexer::iSTxyST(iS,iST,ix,iy);
      u = (ix-x0)*dxc/r + (iy-y0)*dyc/r;
      epsc -= phit[in]->Energy() * u * EmcCluster::ABS(u);
    }
  epsc /= (e0*rsq);
  if( epsc >  0.8 ) epsc = 0.8;
  if( epsc < -0.8 ) epsc =-0.8;
  dxc /= sqrt(1-epsc*epsc);
  dyc /= sqrt(1-epsc*epsc);
  //  Start of iterations
  step = 0.1;
  chisq2 = 1.e35;
  for( iter=0; iter<100; iter++)
    {
      c3to5(e0,x0,y0,epsc,dxc,dyc,&e1c,&x1c,&y1c,&e2c,&x2c,&y2c);
      eps1 = (1+epsc)/2;
      eps2 = (1-epsc)/2;
      chisqc = 0;
      for( in=0; in<nh; in++ ) {
	ex = (phit[in])->Energy();

	EmcIndexer::iPXiSiST((phit[in])->TowerID(),iS,iST);
	EmcIndexer::iSTxyST(iS,iST,ix,iy);

	dx1 = x1c - ix;
	dy1 = y1c - iy;
	dx2 = x2c - ix;
	dy2 = y2c - iy;
	a0 = e1c*PredictEnergy(dx1, dy1, e1c) + e2c*PredictEnergy(dx2, dy2, e2c);
	d = fgEpar00*fgEpar00 + e0*( fgEpar1*a0/e0 + fgEpar2*a0*a0/e0/e0 +fgEpar3*a0*a0*a0/e0/e0/e0 ) + e0*sqrt(e0)*fgEpar4*a0/e0*(1-a0/e0)*fSin4T + e0*e0*fgEpar0*fgEpar0;
	chisqc += (a0-ex)*(a0-ex)/d;
      }
      if( chisqc >= chisq2 ) {
	if( iter > 0 ) {
	  dchi = chisqc-chisq2;
	  dchi0 = gr*step;
	  step /= (2*sqrt(1+dchi/dchi0));
	}
	step /= 2;
      }
      else {
	// Calculation of gradient
	grec = 0;
	grxc = 0;
	gryc = 0;
	for( in=0; in<nh; in++ ) 
	  {
	    ex = (phit[in])->Energy();
	    
	    EmcIndexer::iPXiSiST((phit[in])->TowerID(),iS,iST);
	    EmcIndexer::iSTxyST(iS,iST,ix,iy);
	    dx1 = x1c - ix;
	    dy1 = y1c - iy;
	    dx2 = x2c - ix;
	    dy2 = y2c - iy;
	    a1 = e1c*PredictEnergy(dx1,dy1,e1c);
	    a2 = e2c*PredictEnergy(dx2,dy2,e2c);
	    a0 = a1 + a2;
	    d = fgEpar00*fgEpar00 + e0*( fgEpar1*a0/e0 + fgEpar2*a0*a0/e0/e0 +fgEpar3*a0*a0*a0/e0/e0/e0 ) + e0*sqrt(e0)*fgEpar4*a0/e0*(1-a0/e0)*fSin4T + e0*e0*fgEpar0*fgEpar0;
	    dd = (a0-ex)/d;
	    dchida = dd*( 2 - dd*(fgEpar1 + 2*fgEpar2*a0/e0 + 3*fgEpar3*a0*a0/e0/e0 + e0*sqrt(e0)*fgEpar4*fSin4T*(1-2*a0/e0) + 2*fgEpar0*fgEpar0*a0) );
	    gx1 = ( e1c*PredictEnergy(x1c+0.05-ix,dy1,e1c) - a1 )*20;
	    gx2 = ( e2c*PredictEnergy(x2c+0.05-ix,dy2,e2c) - a2 )*20;
	    gy1 = ( e1c*PredictEnergy(dx1, y1c+0.05-iy,e1c) - a1 )*20;
	    gy2 = ( e2c*PredictEnergy(dx2, y2c+0.05-iy,e2c) - a2 )*20;
	    grec += (dchida*((a1/e1c-a2/e2c)*e0 - (gx1+gx2)*dxc -(gy1+gy2)*dyc)/2);
	    grxc += (dchida*(gx1*eps2-gx2*eps1));
	    gryc += (dchida*(gy1*eps2-gy2*eps1));
	  }
	grc = sqrt(grec*grec + grxc*grxc + gryc*gryc);
	if( grc < 1e-10 ) grc = 1e-10;
	if( iter > 0 ) {
	  float cosi = (gre*grec + grx*grxc + gry*gryc ) / (gr*grc);
	  scal = EmcCluster::ABS(gr/grc - cosi);
	  if( scal < 0.1 ) scal = 0.1;
	  step /= scal;
	}
	chisq2 = chisqc;
	eps0 = epsc;
	dx0 = dxc;
	dy0 = dyc;
	gre = grec;
	grx = grxc;
	gry = gryc;
	gr = grc;
      }
      epsc = eps0 - step*gre/gr;
      while( EmcCluster::ABS(epsc) >= epsmax ) {
	step /= 2;
        if (EmcCluster::ABS(epsc) == epsmax && step == 0)
	  {
	    std::cout <<  __FILE__ << " " << __LINE__ << " EmcCluster::ABS(epsc) == epsmax && step == 0, breaking out" << std::endl;
	    break;
	  }
	epsc = eps0 - step*gre/gr;
      }
      dxc = dx0 - step*grx/gr;
      dyc = dy0 - step*gry/gr;
      if( step*gr < stpmin ) break;
    }
  if( (*pchi)*nh-chisq2 < delch ) return;
  dof = nh;
  if( dof < 1 ) dof = 1;
  *pchi = chisq2/dof;
  c3to5(e0,x0,y0,eps0,dx0,dy0,pe1,px1,py1,pe2,px2,py2);

}

// ///////////////////////////////////////////////////////////////////////////

void EmcScSectorRec::TwoGamma(int nh, EmcModule* phit, float* pchi, float* pe1,
			   float* px1, float* py1, float* pe2, float* px2,
			   float* py2)
{

  float e0, x0, y0, xx, yy, yx;
  float dxy, rsg2, rsq;
  float dxc, dyc, r, epsc;
  int ix, iy, ixy, in, iter, dof;
  float step, chisq2, u;
  float e1c, x1c, y1c, e2c, x2c, y2c;
  float eps0 = 0.0;
  float eps1, eps2, chisqc, ex;
  float dx1, dy1, dx2, dy2, a0, d;
  float dchi, dchi0, dd, dchida, a1, a2;
  float gr = 0.0;
  float grec, grxc, gryc, grc, gx1, gx2, gy1, gy2;
  float gre = 0.0;
  float grx = 0.0;
  float gry = 0.0;
  float scal;
  float dx0 = 0.0;
  float dy0 = 0.0;
  
  const float epsmax=0.9999;
  const float stpmin=0.025;
  const float delch=2;
  
  Momenta(nh,phit,&e0,&x0,&y0,&xx,&yy,&yx);
  *pe2 = 0;
  *px2 = 0;
  *py2 = 0;
  if( nh <= 0 ) return;
  //  choosing of the starting point
  dxy = xx-yy;
  rsg2 = dxy*dxy + 4*yx*yx;
  if( rsg2 < 1e-20 ) rsg2 = 1e-20;
  rsq = sqrt(rsg2);
  dxc = -sqrt((rsq+dxy)*2);
  dyc =  sqrt((rsq-dxy)*2);
  if( yx >= 0 ) dyc = -dyc;
  r = sqrt(dxc*dxc + dyc*dyc);
  epsc = 0;
  for( in=0; in<nh; in++ ) {
    ixy = phit[in].ich;
    iy = ixy/fNx;
    ix = ixy - iy*fNx;
    u = (ix-x0)*dxc/r + (iy-y0)*dyc/r;
    epsc -= phit[in].amp * u * EmcCluster::ABS(u);
  }
  epsc /= (e0*rsq);
  if( epsc >  0.8 ) epsc = 0.8;
  if( epsc < -0.8 ) epsc =-0.8;
  dxc /= sqrt(1-epsc*epsc);
  dyc /= sqrt(1-epsc*epsc);
  //  Start of iterations
  step = 0.1;
  chisq2 = 1.e35;
  for( iter=0; iter<100; iter++)
    {
      c3to5(e0,x0,y0,epsc,dxc,dyc,&e1c,&x1c,&y1c,&e2c,&x2c,&y2c);
      eps1 = (1+epsc)/2;
      eps2 = (1-epsc)/2;
      chisqc = 0;
      for( in=0; in<nh; in++ ) {
	ex = phit[in].amp;
	ixy = phit[in].ich;
	iy = ixy/fNx;
	ix = ixy - iy*fNx;
	dx1 = x1c - ix;
	dy1 = y1c - iy;
	dx2 = x2c - ix;
	dy2 = y2c - iy;
	a0 = e1c*PredictEnergy(dx1, dy1, e1c) + e2c*PredictEnergy(dx2, dy2, e2c);
	d = fgEpar00*fgEpar00 + e0*( fgEpar1*a0/e0 + fgEpar2*a0*a0/e0/e0 +fgEpar3*a0*a0*a0/e0/e0/e0 ) + e0*sqrt(e0)*fgEpar4*a0/e0*(1-a0/e0)*fSin4T + e0*e0*fgEpar0*fgEpar0;
	chisqc += (a0-ex)*(a0-ex)/d;
      }
      if( chisqc >= chisq2 ) {
	if( iter > 0 ) {
	  dchi = chisqc-chisq2;
	  dchi0 = gr*step;
	  step /= (2*sqrt(1+dchi/dchi0));
	}
	step /= 2;
      }
      else {
	// Calculation of gradient
	grec = 0;
	grxc = 0;
	gryc = 0;
	for( in=0; in<nh; in++ ) {
	  ex = phit[in].amp;
	  ixy = phit[in].ich;
	  iy = ixy/fNx;
	  ix = ixy - iy*fNx;
	  dx1 = x1c - ix;
	  dy1 = y1c - iy;
	  dx2 = x2c - ix;
	  dy2 = y2c - iy;
	  a1 = e1c*PredictEnergy(dx1,dy1,e1c);
	  a2 = e2c*PredictEnergy(dx2,dy2,e2c);
	  a0 = a1 + a2;
	  d = fgEpar00*fgEpar00 + e0*( fgEpar1*a0/e0 + fgEpar2*a0*a0/e0/e0 +fgEpar3*a0*a0*a0/e0/e0/e0 ) + e0*sqrt(e0)*fgEpar4*a0/e0*(1-a0/e0)*fSin4T + e0*e0*fgEpar0*fgEpar0;
	  dd = (a0-ex)/d;
	  dchida = dd*( 2 - dd*(fgEpar1 + 2*fgEpar2*a0/e0 + 3*fgEpar3*a0*a0/e0/e0 + e0*sqrt(e0)*fgEpar4*fSin4T*(1-2*a0/e0) + 2*fgEpar0*fgEpar0*a0) );
	  gx1 = ( e1c*PredictEnergy(x1c+0.05-ix,dy1,e1c) - a1 )*20;
	  gx2 = ( e2c*PredictEnergy(x2c+0.05-ix,dy2,e2c) - a2 )*20;
	  gy1 = ( e1c*PredictEnergy(dx1, y1c+0.05-iy,e1c) - a1 )*20;
	  gy2 = ( e2c*PredictEnergy(dx2, y2c+0.05-iy,e2c) - a2 )*20;
	  grec += (dchida*((a1/e1c-a2/e2c)*e0 - (gx1+gx2)*dxc -(gy1+gy2)*dyc)/2);
	  grxc += (dchida*(gx1*eps2-gx2*eps1));
	  gryc += (dchida*(gy1*eps2-gy2*eps1));
	}
	grc = sqrt(grec*grec + grxc*grxc + gryc*gryc);
	if( grc < 1e-10 ) grc = 1e-10;
	if( iter > 0 ) {
	  float cosi = (gre*grec + grx*grxc + gry*gryc ) / (gr*grc);
	  scal = EmcCluster::ABS(gr/grc - cosi);
	  if( scal < 0.1 ) scal = 0.1;
	  step /= scal;
	}
	chisq2 = chisqc;
	eps0 = epsc;
	dx0 = dxc;
	dy0 = dyc;
	gre = grec;
	grx = grxc;
	gry = gryc;
	gr = grc;
      }
      epsc = eps0 - step*gre/gr;
      while( EmcCluster::ABS(epsc) >= epsmax ) {
	step /= 2;
	epsc = eps0 - step*gre/gr;
      }
      dxc = dx0 - step*grx/gr;
      dyc = dy0 - step*gry/gr;
      if( step*gr < stpmin ) break;
    }
  if( (*pchi)*nh-chisq2 < delch ) return;
  dof = nh;
  if( dof < 1 ) dof = 1;
  *pchi = chisq2/dof;
  c3to5(e0,x0,y0,eps0,dx0,dy0,pe1,px1,py1,pe2,px2,py2);

}

// ///////////////////////////////////////////////////////////////////////////



float EmcScSectorRec::ClusterChisq(int nh, emcTowerContent** phit, float e, float x,
				float y, int &ndf)
{


  float chi=0;
  int ix, iy;
  int iS,iST;
  float et, a, d;
  
  for( int in=0; in<nh; in++ ) 
    {
      
      EmcIndexer::iPXiSiST((phit[in])->TowerID(),iS,iST);
      EmcIndexer::iSTxyST(iS,iST,ix,iy);
      
      et = (phit[in])->Energy();
      a = PredictEnergy(x-ix, y-iy, -1);
      d = fgEpar00*fgEpar00 + e*(fgEpar1*a + fgEpar2*a*a + fgEpar3*a*a*a) + 
	e*sqrt(e)*fgEpar4*a*(1-a)*fSin4T + e*e*fgEpar0*fgEpar0;
      a *= e;
      chi += (et-a)*(et-a)/d;
  }

  ndf=nh; // change needed for PbGl MV 28.01.00
  return chi;

}

// ///////////////////////////////////////////////////////////////////////////


float EmcScSectorRec::ClusterChisq(int nh, EmcModule* phit, float e, float x,
				float y, int &ndf)
{

  float chi=0;
  int ixy, ix, iy;
  float et, a, d;
  
  for( int in=0; in<nh; in++ ) {
    ixy = phit[in].ich;
    iy = ixy/fNx;
    ix = ixy - iy*fNx;
    et = phit[in].amp;
    a = PredictEnergy(x-ix, y-iy, -1);
    d = fgEpar00*fgEpar00 + e*(fgEpar1*a + fgEpar2*a*a + fgEpar3*a*a*a) + 
      e*sqrt(e)*fgEpar4*a*(1-a)*fSin4T + e*e*fgEpar0*fgEpar0;
    a *= e;
    chi += (et-a)*(et-a)/d;
  }

  ndf=nh; // change needed for PbGl MV 28.01.00
  return chi;

}

// ///////////////////////////////////////////////////////////////////////////


float EmcScSectorRec::Chi2Limit(int ND)
{
  //  Here the reverse Chi2Correct function is used
  
  float rn, a, b, chi2;
  
  if( ND < 1 ) return 9999.;  // Should we put 0. here?
  
  chi2 = fgChi2Level[EmcCluster::min(ND,50)-1];
  if( chi2 > 100 ) return 9999.; // Why should chi2 ever be >100?
  
  rn = ND;
  b = 0.072*sqrt(rn);
  a = 6.21/(sqrt(rn)+4.7);
  
  return chi2*a/(1.-chi2*b);

}

// ///////////////////////////////////////////////////////////////////////////

float EmcScSectorRec::Chi2Correct(float Chi2, int ND)
{
  // Chi2 - is reduced Chi2: Chi2/ND !!
  // MV 02.22.2000: Actually the above is not true. The supplied value of Chi2
  // has been already divided by ND. So Chi2 here is only corrected.

  float rn, a, b, c;
  
  if( ND < 1 ) return 9999.; // Should we put 0. here?
  
  rn = ND;
  b = 0.072*sqrt(rn);
  a = 6.21/(sqrt(rn)+4.7);
  c = a + b*Chi2;
  if( c < 1 ) c=1;
  
  return Chi2/c;

}

// ///////////////////////////////////////////////////////////////////////////

void EmcScSectorRec::SetTowerThreshold(float Thresh)
{
  fgTowerThresh = Thresh;
  fgEpar0 = Thresh*0.07;
  fgEpar00 = EmcCluster::max( (double)Thresh/3, 0.005 );
}

// **********************************************************************

void EmcScSectorRec::getTowerPos(int ix, int iy, float &x, float & y){
  x = 2.859+5.562*ix+int(ix/12)*0.256;
  y = 2.859+5.562*iy+int(iy/12)*0.156;
}

// **********************************************************************


/// Converts coordinates in units of towers into cm's (Local coord. system)
void   EmcScSectorRec::TowersToSector(float xT, float yT, float & xS, float & yS){
  int   x  = int(xT);
  float dx = xT - x;
  int   y  = int(yT);
  float dy = yT - y;
  xS = fModSizex*(x+0.5) + int(xT/12)*0.256 + fModSizex*dx;
  yS = fModSizey*(y+0.5) + int(yT/12)*0.156 + fModSizey*dy;
}

// **********************************************************************
/// Returns  coordinates of the tower centers in cm's (Local coord. system)
void   EmcScSectorRec::TowersToSector(int xT,   int yT,   float & xS, float & yS){
    xS = fModSizex*(xT+0.5) + int(xT/12)*0.256;
    yS = fModSizey*(yT+0.5) + int(yT/12)*0.156;
}

// **********************************************************************
/// Converts Local Sector coordinates in cm into integer tower numbers
void   EmcScSectorRec::SectorToTowers(float xS, float yS, int & xT,   int & yT){
  // PbSc
  xT = int(((xS-int(xS/67.0)*67.0)-0.078)/fModSizex) + 12*int(xS/67.0);
  yT = int(((yS-int(yS/66.9)*66.9)-0.078)/fModSizey) + 12*int(yS/66.9);

}
// ///////////////////////////////////////////////////////////////////////////
// EOF
