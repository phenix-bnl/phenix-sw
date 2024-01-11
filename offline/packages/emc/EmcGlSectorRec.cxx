// Name: EmcGlSectorRec.h
// Author: M. Volkov (RRC KI) Jan 27 2000
// PbGl sector reconstruction

//INCLUDECHECKER: Removed this line: #include <cmath>
//INCLUDECHECKER: Removed this line: #include "gsl/gsl_math.h"
#include "EmcGlSectorRec.h"
#include "EmcCluster.h"
#include "emcTowerContent.h"
#include "EmcIndexer.h"


// Define and initialize static members

// Parameters for sigma in Hi2 calculations (p.36-37 v.3)
float EmcGlSectorRec::fgEpar00 = 0.010;
float EmcGlSectorRec::fgEpar0 = 0.01;
float EmcGlSectorRec::fgEpar1 = 0.035;
float EmcGlSectorRec::fgEpar2 = -0.035;
// This is for PPLO mode !!!
float EmcGlSectorRec::fgEpar3 = 0.;
float EmcGlSectorRec::fgEpar4 = 1.0;

float EmcGlSectorRec::fSin4T = 0.;

float EmcGlSectorRec::fgConfLevel=0.01;

// Parameters for sigma calculation in Chi2 analysis
float const EmcGlSectorRec::fgSigEcorr0=2.25e-02;
float const EmcGlSectorRec::fgSigEcorr1=1.43e-02;
float const EmcGlSectorRec::fgSigAcorr0=7.20815e-02;
float const EmcGlSectorRec::fgSigAcorr1=-3.22703e-01;

// Parameters for threshold calculation in Chi2 analysis
float const EmcGlSectorRec::fgCutEcorr0=2.1e-02;
float const EmcGlSectorRec::fgCutEcorr1=0.009;

// Parameters for coordinate correction
float const EmcGlSectorRec::fgCoorPar00=0.1584384;
float const EmcGlSectorRec::fgCoorPar01=0.1125;
float const EmcGlSectorRec::fgCoorPar02=4.89856;
float const EmcGlSectorRec::fgCoorPar03=2.42169e-01;
float const EmcGlSectorRec::fgCoorPar10=0.4777;
float const EmcGlSectorRec::fgCoorPar11=4.87463;
float const EmcGlSectorRec::fgCoorPar12=1.23336e-01;
float const EmcGlSectorRec::fgCoorPar20=3.28647;
float const EmcGlSectorRec::fgCoorPar21=6.30741e-01;

// EmcGlSectorRec member functions

void 
EmcGlSectorRec::Gamma(int nh, EmcModule* phit, float* pchi, float* pchi0,
		      float* pe1, float* px1, float* py1, float* pe2,
		      float* px2, float* py2, int &ndf)
{
  // Tests for 1 or 2 photon hypothesis by minimizing the chi2.
  // If the energy of one of two showers is less then fgMinShowerEnergy 
  // they are merged
  //
  // Returns two shower parameters (pe1,px1,py1,pe2,px2,py2). 
  // If one shower hypothesis accepted -> *pe2=0
  // *pchi  - chi2 after splitting
  // *pchi0 - chi2 before splitting
  // ndf contains number of degrees of freedom (not equal to number of cluster
  // modules for PbGl).

  float e1, x1, y1, e2, x2, y2;
  float chi, chi0, chisave;
  int dof;
  float d2, xm2;
  const float zTG=100;
  const float xmcut=0.0015; // (GeV), for overlapping showers separation

  *pe1=0;
  *px1=0;
  *py1=0;
  *pe2=0;
  *px2=0;
  *py2=0;

  if(nh<=0) return;

  Mom1(nh, phit, &e1, &x1, &y1);
  *pe1=e1;     

  if( e1 <= 0 ) return;
  
  SetProfileParameters(0, e1, x1, y1);

  chisave = *pchi;
  chi = *pchi;

  // ClusterChisq parameter list changed MV 28.01.00
  chi0 = ClusterChisq(nh, phit, e1, x1, y1, ndf);
  dof = ndf; // nh->ndf MV 28.01.00

  // ndf=0 means the cluster's chi2 cannot be found; in this case chi0=0.
  if( dof < 1 ) dof=1;
  chi = chi0/dof;
  
  *pchi0 = chi;
  *pchi = chi;
  *px1 = x1;
  *py1 = y1;
  
  if( e1 <= fgMinShowerEnergy ) return;

  if( chi > chisave ) {
    TwoGamma(nh,phit,&chi,&e1,&x1,&y1,&e2,&x2,&y2);
    if( e2 > 0 ) {
      d2 = ((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2))/zTG/zTG;
      xm2 = e1*e2*d2;
      if( xm2 > 0 ) xm2 = sqrt(xm2);
      if( xm2 > xmcut && e1 > fgMinShowerEnergy && e2 > fgMinShowerEnergy) {
	*pe1 = e1;
	*px1 = x1;
	*py1 = y1;
	*pe2 = e2;
	*px2 = x2;
	*py2 = y2;
	*pchi = chi;
      }
    }	
  }
}

float 
EmcGlSectorRec::Chi2Limit(int ndf)
{
  // Returns reduced chi2 correcponding to the current CL.

  if(ndf<1) return 0.;
  float chi2 = fgChi2Level[EmcCluster::min(ndf,50)-1];
  return chi2;

}

float 
EmcGlSectorRec::Chi2Correct(float chi2, int ndf)
{

  if(ndf<1) return 0.;
  return chi2;

}

void 
EmcGlSectorRec::SetProfileParameters(int sectorID, float energy,
				     float x, float y)
{

  //  std::cout << __FILE__ << "  " << __LINE__ << " setprofileparameters " <<  sectorID << "  " << energy << "  " <<  x << "  " << y << std::endl;

  // z (horizontal) and y (vertical) coordinates wrt sector front face
  // reference frame. RF is tied to the module with index 0, i.e. lower left
  // corner of sector. z and y are in module units.
  // If sectorID<0 only energy-dependent parameters are calculated.

  float vx, vy, vz;
  float xVert, yVert, zVert;

  if(sectorID>=0){

    // Vertex coordinates in Sector coordinate system (in cm)
    GlobalToSector(fVx, fVy, fVz, &xVert, &yVert, &zVert);
    vz = -zVert;
    vy = y*fModSizey - yVert;
    vx = x*fModSizex - xVert;

    fSinTy=vy/sqrt(vy*vy+vz*vz);
    fSinTx=vx/sqrt(vx*vx+vz*vz);

    fSin4T = fabs((2*fSinTy-fSinTx)*(fSinTy-2*fSinTx)*(fSinTy-fSinTx)*(fSinTy-fSinTx));

    //angles in degrees
    fTheta=180.0/ M_PI * asin(sqrt(vx*vx+vy*vy)/sqrt(vx*vx+vy*vy+vz*vz));
    fPhi=180.0/ M_PI * (M_PI + atan2(vy, vx));

#if 0
    cerr<<"Before 2nd pass"<<endl;
    cerr<<"fTheta="<<fTheta<<" ";
    cerr<<"fPhi="<<fPhi<<endl;
    cerr<<"fVx="<<fVx<<" fVy="<<fVy<<" fVz="<<fVz<<endl;
    cerr<<"xVert="<<xVert<<" yVert="<<yVert<<" zVert="<<zVert<<endl;
    cerr<<"                      y="<<y*fModSizey<<" x="<<x*fModSizex<<endl;
    cerr<<endl;
#endif

    // ==========> Experimental 2nd pass <==========
    // As the angles of incidence are calculated based on COG, we have
    // a systematic shift in the corrected COG value. For 5 GeV/c electrons 
    // at 20 degrees the shift is ~1.5 mm (RMS of Xcorr-Xinc ~8 mm).
    // 2nd pass is aimed at eliminating this (small) problem.
    
    y*=fModSizey; // convert to cm
    x*=fModSizex;
    float xcorr, ycorr; // corrected values
    CorrectPosition(energy, x, y, &xcorr, &ycorr, false);

    // Recalculate angles
    vy = ycorr-yVert;
    vx = xcorr-xVert;

    fSinTy=vy/sqrt(vy*vy+vz*vz);
    fSinTx=vx/sqrt(vx*vx+vz*vz);

    //angles in degrees
    fTheta=180.0/ M_PI * asin(sqrt(vx*vx+vy*vy)/sqrt(vx*vx+vy*vy+vz*vz));
    fPhi=180.0/ M_PI * (M_PI + atan2(vy, vx));
    // ==========> End of experimental 2nd pass <==========
#if 0
    cerr<<"After 2nd pass"<<endl;
    cerr<<"fTheta="<<fTheta<<" ";
    cerr<<"fPhi="<<fPhi<<endl;
    cerr<<"fVx="<<fVx<<" fVy="<<fVy<<" fVz="<<fVz<<endl;
    cerr<<"xVert="<<xVert<<" yVert="<<yVert<<" zVert="<<zVert<<endl;
    cerr<<"                      y="<<y*fModSizey<<" x="<<x*fModSizex<<endl;
    cerr<<endl;
#endif
  }

  // Calculate coordinate transformation parameters
  fShift=ShiftFunc(energy, fTheta);
  fExc1=Sigma1Func(energy, fTheta);
  fExc2=Sigma2Func(energy, fTheta);
  
  // Parameters we need to pass to the shape function.
  fShapePars[0]=AFunc(energy, fTheta, fPhi);
  fShapePars[1]=CFunc(energy, fTheta, fPhi);
  fShapePars[2]=DFunc(energy, fTheta, fPhi);
  fShapePars[3]=SFunc(energy, fTheta, fPhi);

  // Special threshold for chi2 calculation
  fSpecThr=fgCutEcorr0+fgCutEcorr1*energy;
  fThrCorr=0;
}

float 
EmcGlSectorRec::PredictEnergy (float deltaz, float deltay, float energy)
{
  // Calculate relative predicted energy deposit in module whose center is at
  // deltaz deltay module units from cluster COG.
  // If energy<0 no profile parameters change needed.

  if(energy>0) SetProfileParameters(-1, energy, 0., 0.);

  // Rotate coordinates according to phi      
  Rotate(fPhi, deltaz, deltay);

  // Shift and squeeze
  deltaz-=fShift;
  if(deltaz<0.0) deltaz*=fExc1;
  else deltaz*=fExc2;

  // Effective distance between this module and center of gravity
  float rCG=sqrt(deltaz*deltaz+deltay*deltay);
  
  // Calculate predicted energy deposit in this module.
  float predicted=ShapeFunc(&rCG, fShapePars)+fThrCorr;

  return predicted;
}



float EmcGlSectorRec::ClusterChisq(int nh,  emcTowerContent** phit, float e, float x,
				float y, int &ndf)
// Scheme similar to PbSc (modified by AB)
{

  float chi=0;
  int nhout = 0;
  int ix, iy;
  int iS,iST;
  float et, a, d;
  
  for( int in=0; in<nh; in++ ) 
    {

      et = (phit[in])->Energy();
      if( et/e > 0.004 ) 
	{
	  EmcIndexer::iPXiSiST( (phit[in])->TowerID(),iS,iST);
	  EmcIndexer::iSTxyST(iS,iST,ix,iy);
      
	  a = PredictEnergy(x-ix, y-iy, -1);
	  d = fgEpar00*fgEpar00 + e*(fgEpar1*a + fgEpar2*a*a + fgEpar3*a*a*a) + 
	    e*e*fgEpar4*a*(1-a)*fSin4T + e*e*fgEpar0*fgEpar0;
	  d += 0.30*0.30*e*e*a*a*(1-a); // 5% error 
	  //      d += 0.25*0.25*e*e*a*a; // 5% error 
	  a *= e;
	  chi += (et-a)*(et-a)/d;
	  nhout++;
	}
    }

  ndf=nhout; // change needed for PbGl MV 28.01.00
  return chi;
  
}




float EmcGlSectorRec::ClusterChisq(int nh, EmcModule* phit, float e, float x,
				float y, int &ndf)
// Scheme similar to PbSc (modified by AB)
{

  float chi=0;
  int nhout = 0;
  int ixy, ix, iy;
  float et, a, d;
  
  for( int in=0; in<nh; in++ ) {
    et = phit[in].amp;
    if( et/e > 0.004 ) {
      ixy = phit[in].ich;
      iy = ixy/fNx;
      ix = ixy - iy*fNx;
      a = PredictEnergy(x-ix, y-iy, -1);
      d = fgEpar00*fgEpar00 + e*(fgEpar1*a + fgEpar2*a*a + fgEpar3*a*a*a) + 
	e*e*fgEpar4*a*(1-a)*fSin4T + e*e*fgEpar0*fgEpar0;
      d += 0.30*0.30*e*e*a*a*(1-a); // 5% error 
      //      d += 0.25*0.25*e*e*a*a; // 5% error 
      a *= e;
      chi += (et-a)*(et-a)/d;
      nhout++;
    }
  }

  ndf=nhout; // change needed for PbGl MV 28.01.00
  return chi;

}



void 
EmcGlSectorRec::CorrectEnergy (float Energy, float x, float y, 
			       float* Ecorr)
{
  // angular & nonlinearity correction for the cluster energy
  // MM 02.11.2000
  //
  // (x,y) = x & y center of gravity values

    // variables for angular correction of energy

    static float  par_1 = -0.012043;
    static float  par_2 = 0.077908;
    float  xm,ym;
    float  sin_theta;
    float  theta_deg;
    float  corr_factor_ang;

    // variables for energy nonlinearity correction

    static float  par_a     =  1.0386;     /* corr. funct. par. */
    static float  par_b     = -0.041423;   /* corr. funct. par. */
    static float  par_c     =  0.006064;   /* corr. funct. par. */
    float  log_e;
    float  corr_factor_nonl;

    /****************************************/
    /* angular correction of cluster energy */
    /****************************************/

    // get 1st momenta

    xm = x;
    ym = y;

    // get angle for hit

    GetImpactAngle(xm,ym,&sin_theta);
    theta_deg = 180.0/ M_PI * asin(sin_theta);

    // correct for perpendicular incident

    corr_factor_ang = par_1*exp(theta_deg*par_2)+1-par_1;

    // correct energy

    // Change energy scale on Klaus Reyger's recommendation
    // This has to happen before angular and nonlinearity effects

    //    Energy = 1.11 * Energy;

    // 17-Apr-2001: Additional 3.8% scaling on top of the 11% before (SB)
    // Removed to preserve consistency between simulation and real
    // data production - correction moved back to calibrator G.D. 02/14/02
    //    Energy = 1.15218 * Energy;
    
    Energy = Energy/corr_factor_ang;


    /*********************************************/
    /* nonlinearity correction of cluster energy */
    /*********************************************/

    // precalculate logarithm of cluster energy

    log_e = log(Energy);

    // calculate correction factor for cluster energy

    corr_factor_nonl = par_a + par_b*(log_e)+par_c*(log_e*log_e);

    // calculate corrected cluster energy

    *Ecorr = corr_factor_nonl*Energy;
}


void 
EmcGlSectorRec::CorrectECore (float Ecore, float x, float y, 
			      float* Ecorr)
{
  // Corrects the EM Shower Energy for attenuation in fibers and 
  // long energy leakage
  //
  // (x,y) - impact position (cm)

  *Ecorr = Ecore;
}


void 
EmcGlSectorRec::CorrectPosition (float energy, float zcg, float ycg,
				 float* zcgcorr, float* ycgcorr, 
				 bool callSetPar)
{
  // Corrects the Shower Center of Gravity for the systematic error due to 
  // the limited tower size and angle shift.
  // (zcg, ycg) -- CG position in cm,
  // (*pzcg, *pycg) -- corrected position in cm.
  // callSetPar is used to turn off SetProfileParameters() call when
  // CorrectPosition() is called from SetProfileParameters(). This allows
  // to avoid infinite recursion.

  //  std::cout << __FILE__ << "  " << __LINE__ << " CorrectPosition "<< std::endl;


  float spar[3]; // parameters for s-function

  if(callSetPar)
    SetProfileParameters( 0, energy, zcg/GetModSizex(), ycg/GetModSizey() );

  // Correcting z
  zcg/=GetModSizex(); // convert from cm to module units
  spar[0]=fgCoorPar00+fgCoorPar01*fabs(fSinTx)+
    (fgCoorPar02+fgCoorPar03*energy)*fSinTx*fSinTx;
  spar[0]*=copysign(1., fSinTx);
  spar[1]=fgCoorPar10*fabs(fSinTx)+
    (fgCoorPar11+fgCoorPar12*energy)*fSinTx*fSinTx;
  spar[1]*=copysign(1., fSinTx);
  spar[2]=(fgCoorPar20+fgCoorPar21*log(energy))*fSinTx;

#if 0
  // Experimental for 5 GeV/c 20 deg gammas.
  // If we don't distinguish between electrons and gammas, Zrec-Zinc
  // distribution becomes shifted approx. 7mm for 5 GeV/c 20 deg. RMS is ~12 mm
  // for gammas, ~8 mm for electrons.
  spar[0]=1.28090;
  spar[0]*=copysign(1., fSinTx);
  spar[1]=1.07773;
  spar[1]*=copysign(1., fSinTx);
  spar[2]=1.65389;
  spar[2]*=copysign(1., fSinTx);
#endif

  *zcgcorr=InvScurveFunc(zcg, spar)*GetModSizex();

  // Correcting y
  ycg/=GetModSizey(); // convert from cm to module units
  spar[0]=fgCoorPar00+fgCoorPar01*fabs(fSinTy)+
    (fgCoorPar02+fgCoorPar03*energy)*fSinTy*fSinTy;
  spar[0]*=copysign(1., fSinTy);
  spar[1]=fgCoorPar10*fabs(fSinTy)+
    (fgCoorPar11+fgCoorPar12*energy)*fSinTy*fSinTy;
  spar[1]*=copysign(1., fSinTy);
  spar[2]=(fgCoorPar20+fgCoorPar21*log(energy))*fSinTy;
  *ycgcorr=InvScurveFunc(ycg, spar)*GetModSizey();
}

void 
EmcGlSectorRec::CalculateErrors (float e, float x, float y, float* pde,
				 float* pdx, float* pdy, float* pdz)
{
  // Returns the errors for the reconstructed energy and position 
  // (in the hypothesis of EM shower)
  // Should be called only just after CorrectPosition !!!

  // NO CALCULATION YET FOR PBGL -- PARAMETERS ARE NOT FOUND!!!

  *pde = 999.999;
  *pdx = 999.999;
  *pdy = 999.999;
  *pdz = 999.999;
}



void 
EmcGlSectorRec::TwoGamma (int nmod, emcTowerContent **modules, float *pchi,
			  float *pe1, float *pz1, float *py1,
			  float *pe2, float *pz2, float *py2)
{
  // NO UNFOLDING YET FOR PBGL -- PARAMETERS ARE NOT FOUND!!!

  float e0, z0, y0, zz, yy, zy;

  *pe2=0.;
  *pz2=0.;
  *py2=0.;

  Momenta(nmod, modules, &e0, &z0, &y0, &zz, &yy, &zy);

  *pe1=e0;
  *pz1=z0;
  *py1=y0;
}




void 
EmcGlSectorRec::TwoGamma (int nmod, EmcModule *modules, float *pchi,
			  float *pe1, float *pz1, float *py1,
			  float *pe2, float *pz2, float *py2)
{
  // NO UNFOLDING YET FOR PBGL -- PARAMETERS ARE NOT FOUND!!!

  float e0, z0, y0, zz, yy, zy;

  *pe2=0.;
  *pz2=0.;
  *py2=0.;

  Momenta(nmod, modules, &e0, &z0, &y0, &zz, &yy, &zy);

  *pe1=e0;
  *pz1=z0;
  *py1=y0;
}

float 
EmcGlSectorRec::ShiftFunc (float energy, float angle)
{
  // Calculate x coord. shift according to energy and angle.
  // Angle in degrees.
  
  const float par0=-6.06226e+00;
  const float par1=-3.49026e-01;

  return (par0+par1*log(energy))*(1.0-cos(angle* M_PI /180.0));
}

float 
EmcGlSectorRec::Sigma1Func (float energy, float angle)
{
  // Calculate 1st transformation factor.
  // Angle in degrees.

  const float par0=-1.75549e-02;
  const float par1=-1.35630e-03;

  return 1.0+(par0+par1*log(energy))*angle;
}

float
EmcGlSectorRec::Sigma2Func (float energy, float angle)
{
  // Calculate 2nd transformation factor.
  // Angle in degrees.

  const float par0=-2.33139e-02;
  const float par1=-2.79326e-03;

  return 1.0+(par0+par1*log(energy))*angle;

}

float 
EmcGlSectorRec::ShapeFunc (float *x, float *par)
{
  // Function describes lateral shower shape for e/m showers.
  // x[0] -- (corrected) distance between the module center and the c.g. of the shower.

  float rad=fabs(x[0]);
  float value;
  
  float expr1=exp(-::pow(rad, 1.95)/par[1]);
  
    float d=par[2];
    float s=par[3];
    float expr2=d*exp(-rad/s);
    value=par[0]*EmcCluster::max(expr1,expr2);
  
  return value;
}

float 
EmcGlSectorRec::AFunc (float energy, float angle, float phi)
{
  // Calculate 1st parameter for ShapeFunc based on energy and polar
  // and azimuthal angles.
  // Angle and phi in degrees.

  // Ap0
  const float Ap0p0p0=-3.81425e+00;
  const float Ap0p0p1=-1.94749e+00;

  float Ap0p0=Ap0p0p0+Ap0p0p1*log(energy);

  const float Ap0p1p0=4.62982e+00;
  const float Ap0p1p1=1.94482e+00;

  float Ap0p1=Ap0p1p0+Ap0p1p1*log(energy);

  float Ap0=Ap0p0+Ap0p1*cos(angle* M_PI/180.0);

  // Ap1
  const float Ap1p0p0=2.65696e-05;
  const float Ap1p0p1=5.56524e-05;

  float Ap1p0=Ap1p0p0+Ap1p0p1*log(energy);

  float Ap1=exp(Ap1p0*::pow(angle, 2.5))-1.0;

  // Ap2
  const float Ap2p0p0=1.90663e+01;
  const float Ap2p0p1=-2.96820e-02;

  float Ap2p0=Ap2p0p0+Ap2p0p1*log(energy);

  const float Ap2p1p0=1.51973e-04;
  const float Ap2p1p1=1.20920e-04;

  float Ap2p1=Ap2p1p0+Ap2p1p1*log(energy);

  float Ap2=Ap2p0+exp(Ap2p1*angle*angle*angle);

  // Final result
  float A[3]={Ap0, Ap1, Ap2};

  return PeriodicFunc(&phi, A);

}

float 
EmcGlSectorRec::CFunc (float energy, float angle, float phi)
{
  // Calculate 2nd parameter for ShapeFunc.
  // Angle and phi in degrees.
  // Angle and phi are dummy parameters.

#if 0
  const float cp0=3.28956e-01;
  const float cp1=-1.08942e-02;

  float c=cp0+cp1*log(energy);
#endif

  const float cp0p0=-4.13339e-03;
  const float cp0p1=-6.24275e-03;

  float cp0=cp0p0+cp0p1*log(energy);
  
  float c=0.34+cp0*::pow(angle, 0.3);
  
  return c;

}

float 
EmcGlSectorRec::DFunc (float energy, float angle, float phi)
{
  // Calculate 3rd parameter for ShapeFunc.
  // Angle and phi in degrees.
  // Angle and phi are dummy parameters.

  const float dp0=1.35528e-01;
  const float dp1=-2.43148e-02;

  float d=dp0+dp1*log(energy);

  return d;

}

float 
EmcGlSectorRec::SFunc (float energy, float angle, float phi)
{
  // Calculate 4th parameter for ShapeFunc.
  // Angle and phi in degrees.
  // Angle and phi are dummy parameters.

  const float sp0=5.21967e-01;
  const float sp1=2.66467e-02;

  float s=sp0+sp1*log(energy);

  return s;

}

float 
EmcGlSectorRec::PeriodicFunc (float *x, float *par)
{
  // Auxiliary function for calculating of 1st parameter for ShapeFunc.

  float mean;
  float var=x[0];
  
  const float a=45.0;

  if(var!=0.0) mean=a*(fabs(var)/var)*((1+2*abs(int((var-0.00001)/(2*a)))));
  else mean=a;
  
  return par[0]+par[1]*exp(-0.5*(var-mean)*(var-mean)/(par[2]*par[2]));
  
}

float 
EmcGlSectorRec::InvScurveFunc (float cog, float *par)
{
  // Dependence between incidence coord. and COG

  float b=par[0];
  float d=par[1];
  float D=par[2];

  cog-=d;
  float translation=cog>0.? int(cog+0.5): int(cog-0.5);
  float xinc=translation+
    asinh(2.*(cog-translation)*sinh(0.5/b))*b-D+d;

  return xinc;

}

void 
EmcGlSectorRec::Rotate (float phi, float &deltaRow, float &deltaCol)
{
  // Rotate coordinates according to phi

  float alpha=phi * M_PI / 180.0;
  float sina=sin(alpha);
  float cosa=cos(alpha);
  
  float xprime=deltaRow;
  float yprime=deltaCol;
  
  deltaRow=xprime*cosa+yprime*sina;
  deltaCol=-xprime*sina+yprime*cosa;

}

float 
EmcGlSectorRec::CalcSigma (float predicted, float totSignal, float theta)
{
  // Calculate predicted sigma squared for this module based on predicted
  // relative energy deposit.

  float sigmaSq; // return value

  // Response in PISA is somewhat different from standard GEANT => forced to
  // introduce correction :(
  const float pisaCorr=2.;

  sigmaSq=totSignal*fabs(predicted*(0.9-predicted));

  // energy correction
  sigmaSq*=(pisaCorr*fgSigEcorr0+fgSigEcorr1*totSignal);

  // Angle-dependent correction
  sigmaSq*=1.0+(fgSigAcorr0*exp(fgSigAcorr1*totSignal))*theta;

  return sigmaSq;
  
}

void 
EmcGlSectorRec::getTowerPos(int ix, int iy, 
			    float &x, float & y)
{
  x = fModSizex * ix+int(ix/6)*0.15;
  y = fModSizey * iy+int(iy/6)*0.163;
}

/// Converts coordinates in units of towers into cm's (Local coord. system)
void   
EmcGlSectorRec::TowersToSector(float xT, float yT, 
			       float & xS, float & yS)
{
  int   x  = int(xT);
  float dx = xT - x;
  int   y  = int(yT);
  float dy = yT - y;
  xS = fModSizex*x + int(xT/6)*0.15  + fModSizex*dx;
  yS = fModSizey*y + int(yT/6)*0.163 + fModSizey*dy;
}

/// Returns  coordinates of the tower centers in cm's (Local coord. system)
void   
EmcGlSectorRec::TowersToSector(int xT, int yT,   
			       float & xS, float & yS)
{
  xS = fModSizex*xT + int(xT/6)*0.15;
  yS = fModSizey*yT + int(yT/6)*0.163;
}

/// Converts Local Sector coordinates in cm into integer tower numbers
void  
EmcGlSectorRec::SectorToTowers(float xS, float yS, 
			       int & xT, int & yT)
{
  // PbGl
  xT = int(((xS-int(xS/24.612)*24.612)+2.0385)/fModSizex) + 6*int(xS/24.612);
  yT = int(((yS-int(yS/16.471)*16.471)+2.0385)/fModSizey) + 4*int(yS/16.471);
}












