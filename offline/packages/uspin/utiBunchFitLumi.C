////////////////////////////////////////////////////////////
// Utility for relative luminosity check and asymmetry 
// calculation using luminosity formula (or others)
//
//  Author  : Kiyoshi Tanida (RIKEN)
//  Created : 2003-04-22
////////////////////////////////////////////////////////////

#include "utiBunchFitLumi.hh"
#include "gsl/gsl_const.h"

using namespace std;

////////////////////////////////////////////////////////////
// Full fitting using the Newton-Raphson method
////////////////////////////////////////////////////////////
void utiBunchFitLumi::FullFit(){
  FullFit(Ratio,ErrRatio,PolBlue,PolYell);
}

void utiBunchFitLumi::FullFit(double ratio[],double dratio[]){
  SetRatio(NBUNCH,ratio,dratio);
  FullFit();
}

void utiBunchFitLumi::FullFit(double ratio[],double dratio[],
			      double polb[],double poly[]){
  SetRatio(NBUNCH,ratio,dratio);
  SetPol(NBUNCH,polb,poly);
  FullFit();
  return;
}

void utiBunchFitLumi::FullFit(vector<double> ratio,vector<double> dratio,
			      vector<double> polb,vector<double> poly){
  if((int)ratio.size()!=NBUNCH || (int)dratio.size()!=NBUNCH ||
     (int)polb.size()!=NBUNCH || (int)poly.size()!=NBUNCH){
    fprintf(stderr,"Input is inconsistent with NBUNCH.\n");
    return;
  }

  double matrix[4]; 
  double inv_mat[4]; // Inverse matrix or error matrix 
  double c,all,chisq,dof,redchi; //all is for A_LL, c is a fitting constant
  double dc,dall; // error of c and all
  double m1,mx,mxx,ma,mxa; //important matrix elements
  double a;
  //
  // Initialize the matrix
  //
  for(int k=0;k<4;k++) matrix[k]=0.0;
  m1=mx=mxx=ma=mxa=0.0;
  for(int i=0;i<NBUNCH;i++){ // Loop over NBUNCH
    //
    // Calculate matrix elements 
    //
    if(polb[i]*polb[i] > 1.0 || poly[i]*poly[i] > 1.0
       || dratio[i] <= 0.0) continue;
    // skip if |polarization| > 1.0 or error is negative
    m1 += 1.0/(dratio[i]*dratio[i]);
    mx += polb[i]*poly[i]/(dratio[i]*dratio[i]);
    mxx += polb[i]*polb[i]*poly[i]*poly[i]/(dratio[i]*dratio[i]);
    ma += ratio[i]/(dratio[i]*dratio[i]);
    mxa += ratio[i]*polb[i]*poly[i]/(dratio[i]*dratio[i]);
  }

  // this is for temporal use: debugging output
  //cout << "m1 =" << m1 << endl;
  //cout << "mx =" << mx << endl; 
  //cout << "mxx=" << mxx << endl;
  //cout << "ma =" << ma << endl;
  //cout << "mxa=" << mxa << endl;
  
  c=(ma*mxx - mx*mxa)/(m1*mxx - mx*mx);
  all=(mxa - c*mx)/(c*mxx);
  matrix[0]=c*c*mxx;
  matrix[1]=2.0*c*mx+2.0*all*c*mxx-mxa;
  matrix[2]=2.0*c*mx+2.0*all*c*mxx-mxa;
  matrix[3]=m1+2.0*all*mx+all*all*mxx;
  inverse(matrix,inv_mat);
  
  dall=sqrt(inv_mat[0]);
  dc=sqrt(inv_mat[3]);

  //
  // Calculate chisquare
  //
  chisq=0.0;
  dof = -2.0; // Why -2? --> number of parameters subtracted. 
  for(int i=0;i<NBUNCH;i++){
    if(polb[i]*polb[i] > 1.0 || poly[i]*poly[i] > 1.0 ||
       dratio[i] <= 0.0) continue;
    // skip if |polarization| > 1.0 or error is negative.
    a=(c+c*all*polb[i]*poly[i]-ratio[i])/dratio[i];
    chisq += a*a;
    dof += 1.0;
  }
  if(dof > 0.5){
    redchi=chisq/dof;
  }else{
    if( verbosity> 0 )
      {
	fprintf(stderr,"Not a valid fit, dof is less than one.\n");
      }
    redchi=0.0;
  }

  SetFitParams(c,dc,all,dall,chisq,dof,redchi);
}

////////////////////////////////////////////////////////////
// Simulation of polarized collision measurement
////////////////////////////////////////////////////////////
void utiBunchFitLumi::SimPolCollMea(){
  SimPolCollMea(Ratio,ErrRatio,PolBlue,PolYell,BunchStat);
}

void utiBunchFitLumi::SimPolCollMea(double ratio[],double dratio[]){
  SimPolCollMea();
  for(int i=0; i<NBUNCH; i++){
    ratio[i]=Ratio[i];
    dratio[i]=ErrRatio[i];
  }
}

void utiBunchFitLumi::SimPolCollMea(double ratio[],double dratio[],
				    double polb[],double poly[],
				    double bstat[]){
  SetPol(NBUNCH,polb,poly);
  SetBunchStat(NBUNCH,bstat);
  SimPolCollMea();

  for(int i=0; i<NBUNCH; i++){
    ratio[i]=Ratio[i];
    dratio[i]=ErrRatio[i];
  }
}

void utiBunchFitLumi::SimPolCollMea(vector<double> &ratio,
				    vector<double> &dratio,
				    vector<double> polb,vector<double> poly,
				    vector<double> bstat){
  double a,b;
  for(int i=0;i<NBUNCH;i++){
    a = STAT*bstat[i]*CT*((double )1.0+(double )ALLT*polb[i]*poly[i]);
    a += sqrt(a)*grand();
    b = STAT*bstat[i];
    b += sqrt(b)*grand();
    ratio[i]=a/b;
    dratio[i] = (a/b)*sqrt(1.0/a + 1.0/b);
  }
}

////////////////////////////////////////////////////////////
// Print simulation options
////////////////////////////////////////////////////////////
void utiBunchFitLumi::PrintSimOptions()
{
  cout << "CT = " << CT <<" : Simulated ratio"<<endl;
  cout << "ALLT = " << ALLT <<" : Simulated A_LL"<<endl;
  cout << "STAT = " << STAT <<" : Total statistics"<<endl;
  cout << "SEED = " << SEED<<" : Seed of randomization"<<endl;
}

////////////////////////////////////////////////////////////
// Print fit parameters
////////////////////////////////////////////////////////////
void utiBunchFitLumi::PrintFitParams()
{
  cout << " results are:\n";
  printf("Lumi:  Value       +-   StatErr      ((Value-True)/StatErr)\n");
  printf("C   : %15.10f +- %15.10f (%f)\n",CRatio,CRatioErr,
	 (CRatio-CT)/CRatioErr);
  printf("A_LL: %15.10f +- %15.10f (%f)\n",A_LL,A_LLErr,
	 (A_LL-ALLT)/A_LLErr);
  printf("chisq/dof: %f/%d\n",Chisq,(int )DOF);
}

void utiBunchFitLumi::PrintFitOptions()
{
  cout << "IMPLEMENT ME, PrintFitOptions() was never written" << endl;
}

////////////////////////////////////////////////////////////
// Set Fit Parameters
////////////////////////////////////////////////////////////
void utiBunchFitLumi::SetFitParams(
				   double dCRatio, double dCRatioErr,
				   double dA_LL, double dA_LLErr,
				   double dChisq, double dDOF,double dRedChisq
				   )
{
  SetCRatio(dCRatio);SetCRatioErr(dCRatioErr);
  SetA_LL(dA_LL);SetA_LLErr(dA_LLErr);
  SetChisq(dChisq);SetDOF(dDOF);SetRedChisq(dRedChisq);
}
    

////////////////////////////////////////////////////////////
// Generate a simulated bunch pattern
////////////////////////////////////////////////////////////
void utiBunchFitLumi::siminit(){
  siminit(PolBlue,PolYell,BunchStat);
}

void utiBunchFitLumi::siminit(double polb[],double poly[],double bstat[]){
  siminit();
  for(int i=0; i<NBUNCH; i++){
    polb[i]=PolBlue[i];
    poly[i]=PolYell[i];
    bstat[i]=BunchStat[i];
  }
}

void utiBunchFitLumi::siminit(vector<double> &polb,vector<double> &poly,
			      vector<double> &bstat){
  polb.resize(NBUNCH);
  poly.resize(NBUNCH);
  bstat.resize(NBUNCH);

  srand(SEED);
  for(int i=0;i<NBUNCH;i++){
    int a=i%4;
    //cout << " " << a << endl;
    switch(a){
    case 0:
      polb[i] =  0.80;
      poly[i] =  0.75;
      bstat[i]= 0.2;
      break;
    case 1:
      polb[i] =  0.80;
      poly[i] = -0.75;
      bstat[i]= 1.8;
      break;
    case 2:
      polb[i] = -0.80;
      poly[i] =  0.75;
      bstat[i]= 1.0;
      break;
    case 3:
      polb[i] = -0.80;
      poly[i] = -0.75;
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
// Get inverse matrix 
////////////////////////////////////////////////////////////
int utiBunchFitLumi::inverse(double *input, double *inverted) 
{
  double det;
  det = input[0]*input[3] - input[1]*input[2];
  if(fabs(det) < EPS) return -1; /* determinant is zero */
  inverted[0]= input[3]/det;
  inverted[1]=-input[1]/det;
  inverted[2]=-input[2]/det;
  inverted[3]= input[0]/det;
  return 0;
}


////////////////////////////////////////////////////////////
// Get random number as double
////////////////////////////////////////////////////////////
double utiBunchFitLumi::frand()
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
double utiBunchFitLumi::grand()
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
void utiBunchFitLumi::SimAndFit()
{

  for(int j=0;j<NTRY;j++){ // Loop over NTRY

    //
    // Simulation of polarized collision measurement
    //
    
    siminit(); // Creates model bunch pattern
    SimPolCollMea(); // Fill values in ratio and dratio

    //
    // Full fitting
    //
    FullFit();
    PrintFitParams(); // Show results

  } // End of Loop over NTRY
}
