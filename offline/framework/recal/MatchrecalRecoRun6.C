#include "MatchrecalRecoRun6.h"
#include <Fun4AllReturnCodes.h>

#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>
#include <getClass.h>
#include <RunHeader.h>

#include <PHCompositeNode.h>
#include <RunNumberRanges.h>
#include <recoConsts.h>
#include <iostream>

//Run6 PC3 matching recalibrator, adapted from A. Adare's MiniRecal
//A. Sickles

using namespace std;
using namespace findNode;


MatchrecalRecoRun6::MatchrecalRecoRun6(const string &name): 
  Recalibrator(name)
{
  baseclasses.insert("PHCentralTrack");
  initParameters();
}

int MatchrecalRecoRun6::isValidRun(const int runno) const
{
   if (runno < (int) BEGIN_OF_RUN6 || runno > (int) BEGIN_OF_RUN7)
   {
      return 0;
   }
   return 1;
}

int MatchrecalRecoRun6::process_event(PHCompositeNode *topNode)
{
   PHCentralTrack *d_cnt = getClass<PHCentralTrack>(topNode, inputnodename.c_str());
   if (!d_cnt)
   {
      return 0;
   }
   for(unsigned int i=0; i<d_cnt->get_npart(); i++){
      int arm = d_cnt->get_dcarm(i);
      float alpha = d_cnt->get_alpha(i);
      float zed = d_cnt->get_zed(i);
      float pt = d_cnt->get_pt(i);
      float pc3dphi = d_cnt->get_pc3dphi(i);
      float pc3dz = d_cnt->get_pc3dz(i);

      float pc3sdphi = GetPC3sdphi(arm,Alpha(alpha),Zed(zed),pt,pc3dphi);
      float pc3sdz = GetPC3sdz(arm,Alpha(alpha),Zed(zed),pt,pc3dz);

      d_cnt->set_pc3sdphi(i,pc3sdphi);
      d_cnt->set_pc3sdz(i,pc3sdz);
   }
      
      

   return EVENT_OK;
}
       
float MatchrecalRecoRun6::GetPC3sdphi(const int iarm, const int ialpha, const int ized, const float pt, const float pc3dphi)
{
  if (ized < 0 || ized > 3)
    {
      return -999;
    }
  if (iarm < 0 || iarm > 1)
    {
      return -999;
    }
  if (ialpha < 0 || ialpha > 1)
    {
      return -999;
    }
  float mean;
  if (ized == 0 || ized == 3)
    {
      if (pt == 0) 
	{
           mean = 0.0;
	}
      else
	{
	  mean = PC3_dphi_mean[iarm][ialpha][ized][0] + PC3_dphi_mean[iarm][ialpha][ized][1] / pt
	    + PC3_dphi_mean[iarm][ialpha][ized][2] / (pt * pt);
	}
    }
  else
    {
      mean = PC3_dphi_mean[iarm][ialpha][ized][0]
	* exp(-PC3_dphi_mean[iarm][ialpha][ized][1] * pt) + PC3_dphi_mean[iarm][ialpha][ized][2];
  }

  double par[6];
  for(int ipar=0;ipar<3;ipar++){
    par[ipar]   = PC3_dphi_sigma[iarm][ipar];
    par[ipar+3] = PC3_dphi_sigma_scale[iarm][ized][ipar];
  }

  if (verbosity) 
    cout <<  "GetPC3sdphi(" << iarm << ", " << ialpha << ", " << ized << ", "
	 << pt << ", " << pc3dphi << ") " <<
      "\nmean= " << mean << 
      //      "\nsigma= " << GetSigma(par, pt) << 
      endl;

  return GetDphiDz(pc3dphi, mean, GetSigma(par, pt) );
}


float MatchrecalRecoRun6::GetPC3sdz(const int iarm, const int ialpha, const int ized, const float pt, const float pc3dz)
{
  if(ized<0 || ized>3)
     return -999;
  if(iarm<0 || iarm>1)
     return -999;
  if(ialpha<0 || ialpha>1)
     return -999;

  double mpar[3];
  if( pt < 1.5 ){
    mpar[0] = PC3_dz_mean_lowpt[iarm][ialpha][ized][0];
    mpar[1] = PC3_dz_mean_lowpt[iarm][ialpha][ized][1];
    mpar[2] = PC3_dz_mean_offset_lowpt[iarm][ialpha][ized][0];
  }
  else{ // pt > 1.5 GeV/c                                                                                                                                   
    mpar[0] = PC3_dz_mean_highpt[iarm][ialpha][ized][0];
    mpar[1] = PC3_dz_mean_highpt[iarm][ialpha][ized][1];
    mpar[2] = PC3_dz_mean_offset_highpt[iarm][ialpha][ized][0];
  }
  float mean = GetExp(mpar, pt);

  double par[6];
  par[0] = PC3_dz_sigma_slope[iarm][ialpha];
  par[1] = GetSigmaParameter(PC3_dz_sigma_offset[iarm], (double)0);
  par[2] = 1.0;
  for(int ipar=0;ipar<3;ipar++){
    par[ipar+3] = PC3_dz_sigma_scale[iarm][ized][ipar];
  }

  return GetDphiDz(pc3dz, mean, GetSigma(par, pt) );
}

float MatchrecalRecoRun6::GetExp(const double* par, const float pt) const
{
  return par[0] * exp( - par[1] * pt ) + par[2] ;
}


float MatchrecalRecoRun6::GetDphiDz(const float dphidz, const float mean, const float sigma) const
{
  if( sigma == 0 ){
    if( verbosity > 0 ){
      cout << " Error : sigma parameter = 0. return uncorrected variable" << endl;
    }

    return dphidz;
  }

  return ( dphidz - mean ) / sigma ;
}

float MatchrecalRecoRun6::GetSigma(const double* par, const float pt) const
{
  if( pt == 0 ) {
    cout << " GetSigma():: Error : pt = 0." << endl;
    return 0.0;
  }
  
  // Parameterization of sigma vs pt
  double sigma = sqrt(par[0]*par[0]/(pt*pt) + par[1]*par[1])/par[2] ;
  
  // Additional pt dependent scale factor
  // double spar[3];
  // spar[0] = par[3];
  // spar[1] = par[4];
  // spar[2] = par[5];

  // This scale factor is supposed to correct for large widths at 
  // low pT. The correction appears completely negligible below 0.5 GeV
  // according to Hiroshi's slides linked from:
  // www.phenix.bnl.gov/WWW/p/draft/masui/2006/matchrecalreco/matchrecalreco.html
  // His code to make this 2nd correction has a bug (it uses width in radians, 
  // rather than # of sigma), and since I am
  // only interested in pT > 1.0 GeV, I will leave out this part.

  //  double scale = GetExp(spar, pt);
  double scale = 1.0;

  if (verbosity) {
    for (int i=0; i<6; i++) {
      cout << "par[" << i << "]: " << par[i] << endl;
    }
    cout << "GetSigma(): sigma = " << sigma << endl;
    cout << "GetSigma(): scale = " << scale << endl;
  }

  return  sigma * scale ;
}

float MatchrecalRecoRun6::GetSigmaParameter(const double* par, const double cent) const
{
  return par[0]
    + par[1] * cent
    + par[2] * cent * cent
    + par[3] * cent * cent * cent
    + par[4] * cent * cent * cent * cent;
}


// These fns. return indices/bins
int MatchrecalRecoRun6::dcArm(const float phi) const
{
  // West = 1, East = 0
  return (phi < 1.57) ? 1 : 0;
}

int MatchrecalRecoRun6::Alpha(const float alpha) const
{
  return (alpha<0) ? 0 : 1;
}

int MatchrecalRecoRun6::Zed(const float zed) const
{
  if (zed < -80 || zed > 80) 
    return -1;
  
  int ized = (int)((zed+80.0)/160.0 * 4.0);

  return ized;
}


// Straight from MatchingParameters.h...
void MatchrecalRecoRun6::initParameters()
{
  //
  // Initialization
  //
  //----------------------------------------------------------------------------
  // Delta phi mean
  //----------------------------------------------------------------------------
  // PC3 delta phi mean

   PC3_dphi_mean[0][0][0][0] =     0.00064036;   PC3_dphi_mean[0][0][0][1] =    0.000513385;   PC3_dphi_mean[0][0][0][2] =   -0.000670475; 
   PC3_dphi_mean[0][1][0][0] =    0.000634362;   PC3_dphi_mean[0][1][0][1] =   -0.000603336;   PC3_dphi_mean[0][1][0][2] =     0.00070004; 
   PC3_dphi_mean[0][0][1][0] =    0.000738389;   PC3_dphi_mean[0][0][1][1] =        1.41738;   PC3_dphi_mean[0][0][1][2] =    0.000998814; 
   PC3_dphi_mean[0][1][1][0] =   -0.000708542;   PC3_dphi_mean[0][1][1][1] =          1.103;   PC3_dphi_mean[0][1][1][2] =    0.000556522; 
   PC3_dphi_mean[0][0][2][0] =    0.000402727;   PC3_dphi_mean[0][0][2][1] =       0.606943;   PC3_dphi_mean[0][0][2][2] =    0.000740953; 
   PC3_dphi_mean[0][1][2][0] =   -0.000338489;   PC3_dphi_mean[0][1][2][1] =       0.298488;   PC3_dphi_mean[0][1][2][2] =    0.000607991; 
   PC3_dphi_mean[0][0][3][0] =    0.000891784;   PC3_dphi_mean[0][0][3][1] =    0.000174721;   PC3_dphi_mean[0][0][3][2] =   -0.000663276; 
   PC3_dphi_mean[0][1][3][0] =    0.000815292;   PC3_dphi_mean[0][1][3][1] =   -0.000259218;   PC3_dphi_mean[0][1][3][2] =    0.000684897; 
   PC3_dphi_mean[1][0][0][0] =   -0.000452445;   PC3_dphi_mean[1][0][0][1] =    0.000446367;   PC3_dphi_mean[1][0][0][2] =   -0.000748796; 
   PC3_dphi_mean[1][1][0][0] =   -0.000761211;   PC3_dphi_mean[1][1][0][1] =    8.25071e-06;   PC3_dphi_mean[1][1][0][2] =    0.000573515; 
   PC3_dphi_mean[1][0][1][0] =    0.000387935;   PC3_dphi_mean[1][0][1][1] =        1.85651;   PC3_dphi_mean[1][0][1][2] =   -0.000522509; 
   PC3_dphi_mean[1][1][1][0] =      -0.859299;   PC3_dphi_mean[1][1][1][1] =    5.04115e-05;   PC3_dphi_mean[1][1][1][2] =       0.858091; 
   PC3_dphi_mean[1][0][2][0] =     1.6994e-05;   PC3_dphi_mean[1][0][2][1] =       0.759062;   PC3_dphi_mean[1][0][2][2] =   -0.000462734; 
   PC3_dphi_mean[1][1][2][0] =    0.000245862;   PC3_dphi_mean[1][1][2][1] =        2.05354;   PC3_dphi_mean[1][1][2][2] =    -0.00109465; 
   PC3_dphi_mean[1][0][3][0] =   -0.000930327;   PC3_dphi_mean[1][0][3][1] =    0.000165693;   PC3_dphi_mean[1][0][3][2] =   -0.000627251; 
   PC3_dphi_mean[1][1][3][0] =    -0.00145745;   PC3_dphi_mean[1][1][3][1] =     0.00011494;   PC3_dphi_mean[1][1][3][2] =    0.000540668; 

   

  //----------------------------------------------------------------------------
  // Delta phi sigma
  //----------------------------------------------------------------------------
  // PC3 delta phi sigma
  PC3_dphi_sigma[0][0] =       0.691592;   PC3_dphi_sigma[0][1] =       0.577695;   PC3_dphi_sigma[0][2] =            492; 
  PC3_dphi_sigma[1][0] =       0.724974;   PC3_dphi_sigma[1][1] =       0.676884;   PC3_dphi_sigma[1][2] =            492; 

  //----------------------------------------------------------------------------
  // Delta z mean
  //----------------------------------------------------------------------------
  // PC3 delta z mean
  // EAST
  PC3_dz_mean_lowpt[0][0][0][0]  =      -0.101195;   PC3_dz_mean_lowpt[0][0][0][1]  =        1.87967; 
  PC3_dz_mean_lowpt[0][0][1][0]  =        -2.4813;   PC3_dz_mean_lowpt[0][0][1][1]  =        7.20652; 
  PC3_dz_mean_lowpt[0][0][2][0]  =       -1.50232;   PC3_dz_mean_lowpt[0][0][2][1]  =        4.41151; 
  PC3_dz_mean_lowpt[0][0][3][0]  =       -3.00745;   PC3_dz_mean_lowpt[0][0][3][1]  =        5.42085; 

  PC3_dz_mean_lowpt[0][1][0][0]  =        1.67437;   PC3_dz_mean_lowpt[0][1][0][1]  =        3.93779; 
  PC3_dz_mean_lowpt[0][1][1][0]  =       0.295351;   PC3_dz_mean_lowpt[0][1][1][1]  =       0.432712; 
  PC3_dz_mean_lowpt[0][1][2][0]  =       -1.67574;   PC3_dz_mean_lowpt[0][1][2][1]  =        4.15244; 
  PC3_dz_mean_lowpt[0][1][3][0]  =         -2.551;   PC3_dz_mean_lowpt[0][1][3][1]  =        3.61751; 

  PC3_dz_mean_offset_lowpt[0][0][0][0] =      0.0756703; 
  PC3_dz_mean_offset_lowpt[0][0][1][0] =      0.0661027; 
  PC3_dz_mean_offset_lowpt[0][0][2][0] =      0.0700065; 
  PC3_dz_mean_offset_lowpt[0][0][3][0] =       0.166489; 

  PC3_dz_mean_offset_lowpt[0][1][0][0] =       0.224481; 
  PC3_dz_mean_offset_lowpt[0][1][1][0] =     -0.0291823; 
  PC3_dz_mean_offset_lowpt[0][1][2][0] =      0.0653403; 
  PC3_dz_mean_offset_lowpt[0][1][3][0] =       0.133288; 

  PC3_dz_mean_highpt[0][0][0][0] =        3.03636;   PC3_dz_mean_highpt[0][0][0][1] =        1.26047; 
  PC3_dz_mean_highpt[0][0][1][0] =             20;   PC3_dz_mean_highpt[0][0][1][1] =        3.48045; 
  PC3_dz_mean_highpt[0][0][2][0] =       -2.55729;   PC3_dz_mean_highpt[0][0][2][1] =        1.75245; 
  PC3_dz_mean_highpt[0][0][3][0] =       -4.15904;   PC3_dz_mean_highpt[0][0][3][1] =        1.35183; 

  PC3_dz_mean_highpt[0][1][0][0] =        3.83203;   PC3_dz_mean_highpt[0][1][0][1] =        1.25249; 
  PC3_dz_mean_highpt[0][1][1][0] =       0.657016;   PC3_dz_mean_highpt[0][1][1][1] =       0.642785; 
  PC3_dz_mean_highpt[0][1][2][0] =       -2.56363;   PC3_dz_mean_highpt[0][1][2][1] =        1.77957; 
  PC3_dz_mean_highpt[0][1][3][0] =       -5.76053;   PC3_dz_mean_highpt[0][1][3][1] =        1.53089; 

  PC3_dz_mean_offset_highpt[0][0][0][0] =      -0.419586; 
  PC3_dz_mean_offset_highpt[0][0][1][0] =      -0.026837; 
  PC3_dz_mean_offset_highpt[0][0][2][0] =       0.247874; 
  PC3_dz_mean_offset_highpt[0][0][3][0] =       0.747081; 

  PC3_dz_mean_offset_highpt[0][1][0][0] =      -0.424568; 
  PC3_dz_mean_offset_highpt[0][1][1][0] =      -0.127333; 
  PC3_dz_mean_offset_highpt[0][1][2][0] =       0.269383; 
  PC3_dz_mean_offset_highpt[0][1][3][0] =       0.729245; 

  // WEST
  PC3_dz_mean_lowpt[1][0][0][0]  =        1.95616;   PC3_dz_mean_lowpt[1][0][0][1]  =        3.79051; 
  PC3_dz_mean_lowpt[1][0][1][0]  =       0.856572;   PC3_dz_mean_lowpt[1][0][1][1]  =        4.24843; 
  PC3_dz_mean_lowpt[1][0][2][0]  =       -0.79123;   PC3_dz_mean_lowpt[1][0][2][1]  =         3.3248; 
  PC3_dz_mean_lowpt[1][0][3][0]  =       -1.85318;   PC3_dz_mean_lowpt[1][0][3][1]  =        3.47201; 

  PC3_dz_mean_lowpt[1][1][0][0]  =        6.90203;   PC3_dz_mean_lowpt[1][1][0][1]  =             10; 
  PC3_dz_mean_lowpt[1][1][1][0]  =       0.940021;   PC3_dz_mean_lowpt[1][1][1][1]  =        7.58491; 
  PC3_dz_mean_lowpt[1][1][2][0]  =        3.09126;   PC3_dz_mean_lowpt[1][1][2][1]  =     0.00700842; 
  PC3_dz_mean_lowpt[1][1][3][0]  =        5.17631;   PC3_dz_mean_lowpt[1][1][3][1]  =      0.0058618; 

  PC3_dz_mean_offset_lowpt[1][0][0][0] =       0.184498; 
  PC3_dz_mean_offset_lowpt[1][0][1][0] =     -0.0437485; 
  PC3_dz_mean_offset_lowpt[1][0][2][0] =      -0.190234; 
  PC3_dz_mean_offset_lowpt[1][0][3][0] =      -0.354763; 

  PC3_dz_mean_offset_lowpt[1][1][0][0] =       0.145374; 
  PC3_dz_mean_offset_lowpt[1][1][1][0] =     -0.0076823; 
  PC3_dz_mean_offset_lowpt[1][1][2][0] =       -3.15905; 
  PC3_dz_mean_offset_lowpt[1][1][3][0] =       -5.33721; 

  PC3_dz_mean_highpt[1][0][0][0] =        3.22403;   PC3_dz_mean_highpt[1][0][0][1] =        1.17316; 
  PC3_dz_mean_highpt[1][0][1][0] =        8.10827;   PC3_dz_mean_highpt[1][0][1][1] =        2.52265; 
  PC3_dz_mean_highpt[1][0][2][0] =       -11.9038;   PC3_dz_mean_highpt[1][0][2][1] =        2.73712; 
  PC3_dz_mean_highpt[1][0][3][0] =       -7.82691;   PC3_dz_mean_highpt[1][0][3][1] =        1.81754; 

  PC3_dz_mean_highpt[1][1][0][0] =        19.9998;   PC3_dz_mean_highpt[1][1][0][1] =        2.70149; 
  PC3_dz_mean_highpt[1][1][1][0] =        6.35921;   PC3_dz_mean_highpt[1][1][1][1] =      0.0180746; 
  PC3_dz_mean_highpt[1][1][2][0] =       -1.11576;   PC3_dz_mean_highpt[1][1][2][1] =        1.48727; 
  PC3_dz_mean_highpt[1][1][3][0] =       -2.72615;   PC3_dz_mean_highpt[1][1][3][1] =        1.05591; 

  PC3_dz_mean_offset_highpt[1][0][0][0] =      -0.408306; 
  PC3_dz_mean_offset_highpt[1][0][1][0] =      -0.194744; 
  PC3_dz_mean_offset_highpt[1][0][2][0] =     -0.0089454; 
  PC3_dz_mean_offset_highpt[1][0][3][0] =       0.175841; 

  PC3_dz_mean_offset_highpt[1][1][0][0] =      -0.249803; 
  PC3_dz_mean_offset_highpt[1][1][1][0] =       -6.23633; 
  PC3_dz_mean_offset_highpt[1][1][2][0] =      0.0277607; 
  PC3_dz_mean_offset_highpt[1][1][3][0] =       0.370999; 

  //----------------------------------------------------------------------------
  // Delta z sigma
  //----------------------------------------------------------------------------
  // PC3 delta z sigma
  // EAST
  // Slope parameter
  PC3_dz_sigma_slope[0][0]    =       0.880134;   PC3_dz_sigma_slope[0][1]    =       0.850635; 

  // Centrality dependent offset
  PC3_dz_sigma_offset[0][0]    =        1.60425; 
  PC3_dz_sigma_offset[0][1]    =              0; 
  PC3_dz_sigma_offset[0][2]    =              0; 
  PC3_dz_sigma_offset[0][3]    =              0; 
  PC3_dz_sigma_offset[0][4]    =              0; 

  // WEST
  // Slope parameter
  PC3_dz_sigma_slope[1][0]    =       0.920674;   PC3_dz_sigma_slope[1][1]    =       0.873094; 

  // Centrality dependent offset
  PC3_dz_sigma_offset[1][0]    =        1.59538; 
  PC3_dz_sigma_offset[1][1]    =              0; 
  PC3_dz_sigma_offset[1][2]    =              0; 
  PC3_dz_sigma_offset[1][3]    =              0; 
  PC3_dz_sigma_offset[1][4]    =              0; 


  //----------------------------------------------------------------------------
  // pT dependent scale factor for Delta phi/z sigma
  //----------------------------------------------------------------------------
  // PC3 delta phi sigma
  // EAST
  PC3_dphi_sigma_scale[0][0][0] =      0.0560078;   PC3_dphi_sigma_scale[0][0][1] =        6.59443;   PC3_dphi_sigma_scale[0][0][2] =     0.00169544; 
  PC3_dphi_sigma_scale[0][1][0] =        0.02291;   PC3_dphi_sigma_scale[0][1][1] =        5.64268;   PC3_dphi_sigma_scale[0][1][2] =     0.00165871; 
  PC3_dphi_sigma_scale[0][2][0] =      0.0242151;   PC3_dphi_sigma_scale[0][2][1] =        5.69887;   PC3_dphi_sigma_scale[0][2][2] =     0.00165064; 
  PC3_dphi_sigma_scale[0][3][0] =      0.0643801;   PC3_dphi_sigma_scale[0][3][1] =        6.90639;   PC3_dphi_sigma_scale[0][3][2] =     0.00167835; 

  // WEST
  PC3_dphi_sigma_scale[1][0][0] =      0.0579221;   PC3_dphi_sigma_scale[1][0][1] =        6.73592;   PC3_dphi_sigma_scale[1][0][2] =     0.00195286; 
  PC3_dphi_sigma_scale[1][1][0] =      0.0213385;   PC3_dphi_sigma_scale[1][1][1] =        5.38085;   PC3_dphi_sigma_scale[1][1][2] =     0.00181995; 
  PC3_dphi_sigma_scale[1][2][0] =      0.0243124;   PC3_dphi_sigma_scale[1][2][1] =        5.74032;   PC3_dphi_sigma_scale[1][2][2] =     0.00185266; 
  PC3_dphi_sigma_scale[1][3][0] =      0.0693555;   PC3_dphi_sigma_scale[1][3][1] =        7.15302;   PC3_dphi_sigma_scale[1][3][2] =     0.00183627; 

  // PC3 delta z sigma
  // EAST
  PC3_dz_sigma_scale[0][0][0] =        7.08147;   PC3_dz_sigma_scale[0][0][1] =        4.65645;   PC3_dz_sigma_scale[0][0][2] =        1.74764; 
  PC3_dz_sigma_scale[0][1][0] =        6.70692;   PC3_dz_sigma_scale[0][1][1] =        4.80633;   PC3_dz_sigma_scale[0][1][2] =        1.74371; 
  PC3_dz_sigma_scale[0][2][0] =        7.14652;   PC3_dz_sigma_scale[0][2][1] =        5.00937;   PC3_dz_sigma_scale[0][2][2] =        1.75018; 
  PC3_dz_sigma_scale[0][3][0] =        7.36042;   PC3_dz_sigma_scale[0][3][1] =        4.78258;   PC3_dz_sigma_scale[0][3][2] =        1.77067; 

  // WEST
  PC3_dz_sigma_scale[1][0][0] =        7.08624;   PC3_dz_sigma_scale[1][0][1] =        4.54772;   PC3_dz_sigma_scale[1][0][2] =        1.74238; 
  PC3_dz_sigma_scale[1][1][0] =         7.2554;   PC3_dz_sigma_scale[1][1][1] =        4.85841;   PC3_dz_sigma_scale[1][1][2] =        1.74612; 
  PC3_dz_sigma_scale[1][2][0] =        7.01406;   PC3_dz_sigma_scale[1][2][1] =        4.75945;   PC3_dz_sigma_scale[1][2][2] =        1.73788; 
  PC3_dz_sigma_scale[1][3][0] =         6.8503;   PC3_dz_sigma_scale[1][3][1] =        4.46175;   PC3_dz_sigma_scale[1][3][2] =         1.7303; 

  return;
}
