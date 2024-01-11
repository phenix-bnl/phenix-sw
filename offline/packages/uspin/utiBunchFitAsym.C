////////////////////////////////////////////////////////////
// Physics asymmetry extraction using the bunch fit method (Tanida method)
//
//  Author  : Hideyuki Kobayahsi (C to C++ translation) 
//          : Kiyoshi Tanida (original)
//  Created : 2003-03-10
//  Modified: 2003-04-22 by K. Tanida 
//            skip bunches with |polarization| > 1 or zero statistics
//            re-calculate chi-square and degree-of-freedom correctly 
////////////////////////////////////////////////////////////

#include "utiBunchFitAsym.hh"
#include "gsl/gsl_const.h"
using namespace std;

////////////////////////////////////////////////////////////
// Full fitting using the Newton-Raphson method
////////////////////////////////////////////////////////////
void utiBunchFitAsym::FullFit()
{
  FullFit(NumL,NumR,PolBlue,PolYell);
}
void utiBunchFitAsym::FullFit(double nl[],double nr[])
{
  FullFit(nl,nr,PolBlue,PolYell);
}
void utiBunchFitAsym::FullFit(double nl[],double nr[],double polb[],double poly[])
{
  int i,k,l;
  double a,e,f,g;
  double fullmat[9] = {0}; // Matrix for full fitting
  double vec[3] = {0};
  double inv_fullmat[9] = {0}; // Invert matrix for full fitting
  double d,ab,ay,chisq,dof,redchi;
  double dd,dab,day,deld;
  double ddlin;

  //
  // Do Linear approximation fit to get the initial guess
  //
  LinApproxFit(nl,nr,polb,poly);

  // Initialize asymmetry array
  for(int i=0;i<MaxNBUNCH;i++){
    alr[i]=0; // Asymmetry of left right
    dalr[i]=0; // Error of Asymmetry of left right
  }

  //
  // Use results from linear approximation fit
  //
  d=GetDetAsym();
  ab=GetBlueAsym();
  ay=GetYellAsym();

  //
  // Errors from linear approximation fit
  //
  ddlin=GetDetAsymErr();
  
  for(l=0;l<NITER;l++){// Iteration for Newton-Raphson method 
    //
    // Initialize the matrix
    //
    for(k=0;k<9;k++) fullmat[k]=0.0;
    for(k=0;k<9;k++) inv_fullmat[k]=0.0;
    for(k=0;k<3;k++) vec[k]=0.0;
    for(i=0;i<NBUNCH;i++){ // Loop over NBUNCH
      //
      // Calculate Hessian (acting as Jacobian in Newton-Raphson method.
      //
      if(polb[i]*polb[i] > 1.0 || poly[i]*poly[i] > 1.0
	 || nl[i]<=0.0 || nr[i] <=0.0) continue;
      // skip if |polarization| > 1.0 or zero statistics
      alr[i]=(nl[i]-nr[i])/(nl[i]+nr[i]);
      dalr[i]=2.0*sqrt(nl[i]*nr[i]/(nl[i]+nr[i]))/(nl[i]+nr[i]);
      e = ab*polb[i] + ay*poly[i];
      f = 1.0 + d*e;
      g = d+e-alr[i]*f;
      a =  1.0/pow(f,4.0)/(dalr[i]*dalr[i]);
      vec[0] += a*((d+e)/f - alr[i])*(1-e*e)*f*f;
      vec[1] += polb[i]*a*g*(1-d*d)*f;
      vec[2] += poly[i]*a*g*(1-d*d)*f;
      fullmat[0] += a*(1.0-e*e)*(1.0 - e*e -2.0*e*g);
      fullmat[1] += polb[i]*a*((1.0-d*d)*(1.0-e*e)-2.0*(d+e)*g);
      fullmat[2] += poly[i]*a*((1.0-d*d)*(1.0-e*e)-2.0*(d+e)*g);
      fullmat[3] += polb[i]*a*((1.0-d*d)*(1.0-e*e)-2.0*(d+e)*g);
      fullmat[4] += polb[i]*polb[i]*a*(1.0-d*d)*(1.0-d*d-2.0*d*g);
      fullmat[5] += polb[i]*poly[i]*a*(1.0-d*d)*(1.0-d*d-2.0*d*g);
      fullmat[6] += poly[i]*a*((1.0-d*d)*(1.0-e*e)-2.0*(d+e)*g);
      fullmat[7] += polb[i]*poly[i]*a*(1.0-d*d)*(1.0-d*d-2.0*d*g);
      fullmat[8] += poly[i]*poly[i]*a*(1.0-d*d)*(1.0-d*d-2.0*d*g);
    } // End of Loop over NBUNCH
      
    //   printf("%f %f %f\n",vec[0],vec[1],vec[2]);

    //
    // Apply first order correction using inverted Jacobian
    // Invertex Jacobian also serves as Error Matrix.
    //
    inverse(fullmat,inv_fullmat);
    d  -= inv_fullmat[0]*vec[0] + inv_fullmat[1]*vec[1] +
      inv_fullmat[2]*vec[2];
    ab -= inv_fullmat[3]*vec[0] + inv_fullmat[4]*vec[1] +
      inv_fullmat[5]*vec[2];
    ay -= inv_fullmat[6]*vec[0] + inv_fullmat[7]*vec[1] +
      inv_fullmat[8]*vec[2];
    deld = inv_fullmat[0]*vec[0] + inv_fullmat[1]*vec[1] +
      inv_fullmat[2]*vec[2];
    //printf("%2d %15.10f %15.10f %15.10f\n",l,d,ab,ay);
    //
    // loop ends when delta D is negligibly smaller than 
    // statistical error.
    //
    if(fabs(deld/ddlin) < EPS) break;
    if(l==NITER-1){
      if( verbosity> 0 )
	{
	  fprintf(stderr,"Fitting not converged.\n");
	}
    }
  }

  dd=sqrt(inv_fullmat[0]);
  dab=sqrt(inv_fullmat[4]);
  day=sqrt(inv_fullmat[8]);

  //
  // Calculate chisquare
  //
  chisq=0.0;
  dof = 0.0;
  for(i=0;i<NBUNCH;i++){
    if(polb[i]*polb[i] > 1.0 || poly[i]*poly[i] > 1.0
       || nl[i]<=0.0 || nr[i] <=0.0) continue;
    // skip if |polarization| > 1.0 or zero statistics
    e = ab*polb[i] + ay*poly[i];
    f = 1.0 + d*e;
    g = d+e-alr[i]*f;
    a =  1.0/pow(f,2.0)/(dalr[i]*dalr[i]);
    chisq += a*g*g;
    dof += 1.0;
  }
  dof -= 3.0;
  if(dof > 0.5){
    redchi=chisq/dof;
  }else{
    if( verbosity>0 )
      { 
	fprintf(stderr,"Not a valid fit, dof is less than one.\n");
      }
    redchi=0.0;
  }

  SetFitParams(d,dd,ab,dab,ay,day,chisq,dof,redchi,
	       "Newton-Rapson full");
}


////////////////////////////////////////////////////////////
// Linear approximation fitting
////////////////////////////////////////////////////////////
void utiBunchFitAsym::LinApproxFit()
{
  LinApproxFit(NumL,NumR,PolBlue,PolYell);
}
void utiBunchFitAsym::LinApproxFit(double nl[],double nr[])
{
  LinApproxFit(nl,nr,PolBlue,PolYell);
}
void utiBunchFitAsym::LinApproxFit(double nl[],double nr[],double polb[],double poly[])
{
  int i,k;
  double linmat[9] = {0}; // Matrix for linear approximation fitting
  double vec[3] = {0};
  // Inverted matrix for linear approximation fitting,
  // Also known as Error Matrix.
  double inv_linmat[9] = {0}; 
  double dlin,ablin,aylin;
  double ddlin,dablin,daylin;
  double chisq,dof,redchi;

  // Initialize asymmetry array
  for(int i=0;i<MaxNBUNCH;i++){
    alr[i]=0; // Asymmetry of left right
    dalr[i]=0; // Error of Asymmetry of left right
  }

  //
  // Calculate matrix
  //
  for(k=0;k<9;k++) linmat[k]=0;
  for(k=0;k<9;k++) inv_linmat[k]=0;
  for(k=0;k<3;k++) vec[k]=0;
  for(i=0;i<NBUNCH;i++){
    if(polb[i]*polb[i] > 1.0 || poly[i]*poly[i] > 1.0
       || nl[i]<=0.0 || nr[i] <=0.0) continue;    
    // skip if |polarization| > 1.0 or zero statistics

    alr[i]=(nl[i]-nr[i])/(nl[i]+nr[i]);
    dalr[i]=2.0*sqrt(nl[i]*nr[i]/(nl[i]+nr[i]))/(nl[i]+nr[i]);
    linmat[0] += 1.0/(dalr[i]*dalr[i]);
    linmat[1] += polb[i]/(dalr[i]*dalr[i]);
    linmat[2] += poly[i]/(dalr[i]*dalr[i]);
    linmat[3] += polb[i]/(dalr[i]*dalr[i]);
    linmat[4] += polb[i]*polb[i]/(dalr[i]*dalr[i]);
    linmat[5] += polb[i]*poly[i]/(dalr[i]*dalr[i]);
    linmat[6] += poly[i]/(dalr[i]*dalr[i]);
    linmat[7] += polb[i]*poly[i]/(dalr[i]*dalr[i]);
    linmat[8] += poly[i]*poly[i]/(dalr[i]*dalr[i]);
    vec[0] += alr[i]/(dalr[i]*dalr[i]);
    vec[1] += alr[i]*polb[i]/(dalr[i]*dalr[i]);
    vec[2] += alr[i]*poly[i]/(dalr[i]*dalr[i]);
  }

  //
  // Solve the matrix equation
  //
  inverse(linmat, inv_linmat);
  dlin=inv_linmat[0]*vec[0] + inv_linmat[1]*vec[1] + inv_linmat[2]*vec[2];
  ablin=inv_linmat[3]*vec[0] + inv_linmat[4]*vec[1] + inv_linmat[5]*vec[2];
  aylin=inv_linmat[6]*vec[0] + inv_linmat[7]*vec[1] + inv_linmat[8]*vec[2];
  ddlin=sqrt(inv_linmat[0]);
  dablin=sqrt(inv_linmat[4]);
  daylin=sqrt(inv_linmat[8]);

  //
  // Calculate the chisquare
  //
  chisq=0.0;
  dof=0.0;
  for(i=0;i<NBUNCH;i++){
    if(polb[i]*polb[i] > 1.0 || poly[i]*poly[i] > 1.0
       || nl[i]<=0.0 || nr[i] <=0.0) continue;    
    // skip if |polarization| > 1.0

    chisq += (alr[i]-dlin-ablin*polb[i]-aylin*poly[i])*
      (alr[i]-dlin-ablin*polb[i]-aylin*poly[i])/
      (dalr[i]*dalr[i]);
    dof += 1.0;
  }
  dof -= 3.0;
  if(dof > 0.5){
    redchi=chisq/dof;
  }else{
    if( verbosity>0 )      
      {
	fprintf(stderr,"Not a valid fit, dof is less than one.\n");
      }
    redchi=0.0;
  }
  //
  // Save the results
  //
  SetFitParams(dlin,ddlin,ablin,dablin,aylin,daylin,chisq,dof,redchi,
	       "Linear approximation");
}


////////////////////////////////////////////////////////////
// Bunch pair analysis -- Sqrt formula
////////////////////////////////////////////////////////////
void utiBunchFitAsym::BunchPairAna()
{
  BunchPairAna(NumL,NumR,PolBlue,PolYell);
}
void utiBunchFitAsym::BunchPairAna(double nl[],double nr[])
{
  BunchPairAna(nl,nr,PolBlue,PolYell);
}
void utiBunchFitAsym::BunchPairAna(double nl[],double nr[],double polb[],double poly[])
{
  int i;
  double nbul,nbur,nbdl,nbdr; // for sqrt formula 
  double nyul,nyur,nydl,nydr; // for sqrt formula 
  double pbu,pbd,pyu,pyd; // Polariztion 
  double pbuS,pbdS,pyuS,pydS; // Polariztion sum over bunchs
  double nbu,nbd,nyu,nyd; // Number of counts 
  double ab,ay,d; 
  double dab,day,dd;
  double pbav, pyav;

  nbul=nbur=nbdl=nbdr=0;
  nyul=nyur=nydl=nydr=0;
  pbuS=pbdS=pyuS=pydS=0;
  nbu=nbd=nyu=nyd=0;

  for(i=0;i<NBUNCH;i++){ // Loop over NBUNCH
    if(polb[i]>0.0){
      nbul += nl[i];
      nbur += nr[i];
      pbuS += polb[i]*(nl[i]+nr[i]);
      nbu  += nl[i]+nr[i];
    }
    if(polb[i]<0.0){
      nbdl += nl[i];
      nbdr += nr[i];
      pbdS += polb[i]*(nl[i]+nr[i]);
      nbd  += nl[i]+nr[i];
    }
    if(poly[i]>0.0){
      nyul += nl[i];
      nyur += nr[i];
      pyuS += poly[i]*(nl[i]+nr[i]);
      nyu  += nl[i]+nr[i];
    }
    if(poly[i]<0.0){
      nydl += nl[i];
      nydr += nr[i];
      pydS += poly[i]*(nl[i]+nr[i]);
      nyd  +=nl[i]+nr[i];
    }
  } // End of loop over NBUNCH
  pbu=pbuS/nbu;
  pbd=pbdS/nbd;
  pyu=pyuS/nyu;
  pyd=pydS/nyd;

  pbav=(pbu*nbu-pbd*nbd)/(nbu+nbd);
  pyav=(pyu*nyu-pyd*nyd)/(nyu+nyd);

  /*
  cout <<"Blue: UL UR DL DR  pU pD  nU nD  pAv\n";
  cout << " " << nbul;
  cout << " " << nbur;
  cout << " " << nbdl;
  cout << " " << nbdr;
  cout << " " ;
  cout << " " << pbu;
  cout << " " << pbd;
  cout << " " ;
  cout << " " << nbu;
  cout << " " << nbd;
  cout << " " ;
  cout << " " << pbav;
  cout << endl;
  cout <<"Yell: UL UR DL DR  pU pD  nU nD  pAv\n";
  cout << " " << nyul;
  cout << " " << nyur;
  cout << " " << nydl;
  cout << " " << nydr;
  cout << " " ;
  cout << " " << pyu;
  cout << " " << pyd;
  cout << " " ;
  cout << " " << nyu;
  cout << " " << nyd;
  cout << " " ;
  cout << " " << pyav;
  cout << endl;
  */

  double nume,deno;
  //
  // Physics asym blue
  //
  nume=sqrt(nbul*nbdr) - sqrt(nbdl*nbur);
  deno=sqrt(nbul*nbdr) + sqrt(nbdl*nbur);
  ab=nume/deno/pbav;
  //
  // Physics asym blue error
  //
  nume
    =sqrt(nbul*nbdl*nbur*nbdl)*sqrt(1/nbul+1/nbdl+1/nbur+1/nbdr);
  deno
    =(sqrt(nbul*nbdr)+sqrt(nbdl*nbur))*(sqrt(nbul*nbdr)+sqrt(nbdl*nbur));
  dab=nume/deno/pbav;

  //
  // Physics asym yellow
  //
  nume=sqrt(nyul*nydr) - sqrt(nydl*nyur);
  deno=sqrt(nyul*nydr) + sqrt(nydl*nyur);
  ay=nume/deno/pyav;

  //
  // Physics asym yellow error
  //
  nume
    =sqrt(nyul*nydl*nyur*nydl)*sqrt(1/nyul+1/nydl+1/nyur+1/nydr);
  deno
    =(sqrt(nyul*nydr)+sqrt(nydl*nyur))*(sqrt(nyul*nydr)+sqrt(nydl*nyur));
  day=nume/deno/pyav;

  //
  // Detector asym blue
  //
  nume=sqrt(nbul*nbdl) - sqrt(nbur*nbdr);
  deno=sqrt(nbul*nbdl) + sqrt(nbur*nbdr);
  d=nume/deno;
  dd=-1;

  //
  // Detector asym error
  //
  nume
    =sqrt(nyul*nydl*nyur*nydl)*sqrt(1/nyul+1/nydl+1/nyur+1/nydr);
  deno
    =(sqrt(nyul*nydl)+sqrt(nyur*nydr))*(sqrt(nyul*nydl)+sqrt(nyur*nydr));
  dd=nume/deno/pyav;

  SetFitParams(d,dd,ab,dab,ay,day,-1,-1,-1,"Bunch Pair Analysis");
}


////////////////////////////////////////////////////////////
// Simulation of polarized collision measurement
////////////////////////////////////////////////////////////
void utiBunchFitAsym::SimPolCollMea()
{
  SimPolCollMea(NumL,NumR,PolBlue,PolYell,BunchStat);
}
void utiBunchFitAsym::SimPolCollMea(double nl[],double nr[])
{
  SimPolCollMea(nl,nr,PolBlue,PolYell,BunchStat);
}
void utiBunchFitAsym::SimPolCollMea(double nl[],double nr[],
				    double polb[],double poly[],double bstat[])
{
  int i;
  double a;
  for(i=0;i<NBUNCH;i++){
    a = STAT*bstat[i]*(1.0+DT)*(1.0+ABT*polb[i]+AYT*poly[i]);
    nl[i] = a + sqrt(a)*grand();
    a = STAT*bstat[i]*(1.0-DT)*(1.0-ABT*polb[i]-AYT*poly[i]);
    nr[i] = a + sqrt(a)*grand();
  }
}

////////////////////////////////////////////////////////////
// Print fit options
////////////////////////////////////////////////////////////
void utiBunchFitAsym::PrintFitOptions()
{
  cout <<"EPS ="<<EPS<<" : Calculation termination error"<<endl;
  cout <<"NITER="<<NITER<<" : Number of iteration for Newton-Rapson method"<<endl;
}

////////////////////////////////////////////////////////////
// Print simulation options
////////////////////////////////////////////////////////////
void utiBunchFitAsym::PrintSimOptions()
{
  cout << "DT = " << DT <<" : Simulated detector asymmetry"<<endl;
  cout << "ABT = " << ABT <<" : Simulated blue beam physics asymmetry"<<endl;
  cout << "AYT = " << AYT <<" : Simulated yellow beam physics asymmetry"<<endl;
  cout << "STAT = " << STAT <<" : Total statistics"<<endl;
  cout << "SEED = " << SEED<<" : Seed of randomization"<<endl;
}

////////////////////////////////////////////////////////////
// Print fit parameters
////////////////////////////////////////////////////////////
void utiBunchFitAsym::PrintFitParams()
{
  cout << GetFitTypeName();
  cout << " results are:\n";
  printf("Asym:  Value       +-   StatErr      ((Value-True)/StatErr)\n");
  printf("D : %15.10f +- %15.10f (%f)\n",DetAsym,DetAsymErr,(DetAsym-DT)/DetAsymErr);
  printf("Ab: %15.10f +- %15.10f (%f)\n",BlueAsym,BlueAsymErr,(BlueAsym-ABT)/BlueAsymErr);
  printf("Ay: %15.10f +- %15.10f (%f)\n",YellAsym,YellAsymErr,(YellAsym-AYT)/YellAsymErr); 
  GetDOF();
  printf("chisq/dof: %f/%d\n",Chisq,(int) DOF);
}

////////////////////////////////////////////////////////////
// Set Fit Parameters
////////////////////////////////////////////////////////////
void utiBunchFitAsym::SetFitParams(
				   double dDetAsym, double dDetAsymErr,
				   double dBlueAsym, double dBlueAsymErr,
				   double dYellAsym, double dYellAsymErr,
				   double dChisq, double dDOF,double dRedChisq,
				   string sFitTypeName
				   )
{
  SetDetAsym(dDetAsym);SetDetAsymErr(dDetAsymErr);
  SetBlueAsym(dBlueAsym);SetBlueAsymErr(dBlueAsymErr);
  SetYellAsym(dYellAsym);SetYellAsymErr(dYellAsymErr);
  SetChisq(dChisq);SetDOF(dDOF);SetRedChisq(dRedChisq);
  SetFitTypeName(sFitTypeName);
}
    

////////////////////////////////////////////////////////////
// Generate a simulated bunch pattern
////////////////////////////////////////////////////////////
void utiBunchFitAsym::init()
{
  init(PolBlue,PolYell,BunchStat);
}
void utiBunchFitAsym::init(double polb[],double poly[],double bstat[])
{
  int i;
  int a;
  srand(SEED);
  for(i=0;i<NBUNCH;i++){
    a=i%4;
    //cout << " " << a << endl;
    switch(a){
    case 0:
      polb[i] =  0.20;
      poly[i] =  0.15;
      bstat[i]= 0.2;
      break;
    case 1:
      polb[i] =  0.20;
      poly[i] = -0.15;
      bstat[i]= 1.8;
      break;
    case 2:
      polb[i] = -0.20;
      poly[i] =  0.15;
      bstat[i]= 1.0;
      break;
    case 3:
      polb[i] = -0.20;
      poly[i] = -0.15;
      bstat[i]= 1.0;
      break;
    case 4:
      polb[i] = 0.0;
      poly[i] = 0.0;
      bstat[i]= 1.0;
      break;
    default:
      polb[i] = 0.0;
      poly[i] = 0.0;
      bstat[i]= 1.0;
      break;
    }
  }
}

////////////////////////////////////////////////////////////
// Get inverse matrix -- symmetric case only! 
////////////////////////////////////////////////////////////
int utiBunchFitAsym::inverse(double *input, double *inverted) 
{
  double det;
  det = input[0]*input[4]*input[8]
    - input[0]*input[5]*input[7]
    - input[1]*input[3]*input[8]
    + input[1]*input[5]*input[6]
    + input[2]*input[3]*input[7]
    - input[2]*input[4]*input[6];
  if(fabs(det) < EPS) return -1; /* determinant is zero */
  inverted[0]=(input[4]*input[8]-input[5]*input[7])/det;
  inverted[1]=(input[5]*input[6]-input[3]*input[8])/det;
  inverted[2]=(input[3]*input[7]-input[4]*input[6])/det;
  inverted[3]=(input[2]*input[7]-input[1]*input[8])/det;
  inverted[4]=(input[0]*input[8]-input[2]*input[6])/det;
  inverted[5]=(input[1]*input[6]-input[0]*input[7])/det;
  inverted[6]=(input[1]*input[5]-input[2]*input[4])/det;
  inverted[7]=(input[2]*input[3]-input[0]*input[5])/det;
  inverted[8]=(input[0]*input[4]-input[1]*input[3])/det;
  return 0;
}


////////////////////////////////////////////////////////////
// Get random number as double
////////////////////////////////////////////////////////////
double utiBunchFitAsym::frand()
{
  int i;
  double a;
  
  i=rand();
  a=((float )i+0.5)/((float )RAND_MAX+1.0);
  return a;
}

////////////////////////////////////////////////////////////
// Get random number
////////////////////////////////////////////////////////////
double utiBunchFitAsym::grand()
{
  double r1,r2,x1;
  
  r1 = frand();
  r2 = frand();
  x1 = sqrt(fabs(-2.0*log(r1)))*cos(2*M_PI*r2);
  return x1;
}

////////////////////////////////////////////////////////////
// Application of simulation and fit 
////////////////////////////////////////////////////////////
void utiBunchFitAsym::SimAndFit()
{

  for(int j=0;j<NTRY;j++){ // Loop over NTRY

    //
    // Simulation of polarized collision measurement
    //
    
    init(); // Creates model bunch pattern
    SimPolCollMea(); // Fill values in nl[] and nr[]

    //
    // Bunch pair analysis -- Sqrt formula
    //
    BunchPairAna();
    PrintFitParams();

    //
    // Linear approximation fitting
    //
    LinApproxFit();
    PrintFitParams(); // Show results

    //
    // Full fitting using the Newton Lapson method
    //
    FullFit();
    PrintFitParams(); // Show results

  } // End of Loop over NTRY
}

