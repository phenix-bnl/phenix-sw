#include "MatchrecalRecoRun15pp.h"
#include <Fun4AllReturnCodes.h>

#include <PHGlobal.h>
#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>
#include <getClass.h>
#include <RunHeader.h>

#include <PHCompositeNode.h>
#include <RunNumberRanges.h>
#include <recoConsts.h>
#include <iostream>

//Run15pp PC2/PC3 matching recalibrator, adapted from A. Adare's MiniRecal
//A. Sickles

using namespace std;
using namespace findNode;


MatchrecalRecoRun15pp::MatchrecalRecoRun15pp(const string &name): 
  Recalibrator(name)
{
  baseclasses.insert("PHCentralTrack");
  initParameters();
}

int MatchrecalRecoRun15pp::isValidRun(const int runno) const
{
   if (runno < 421707 || runno > 432008)
   {
      return 0;
   }
   return 1;
}

void MatchrecalRecoRun15pp::help() const
{
  cout << "===================================================================" << endl;
  cout << "MatchrecalRecoRun15pp::help method output"                    << endl;
  cout << "Qiao Xu(qiao.xu@vanderbilt.edu) " << endl;
  cout << "PC3 matching for Run-15 p+p " << endl;
  cout << "PC2 matching for Run-15 p+p " << endl;
  cout << "TOF matching will be added" << endl;
  cout << "Run15 pp 200 GeV with minimum bias triggers" << endl;
  cout << "Central arm tracks with trasverse momentum < 0.2 GeV/c are ignored" << endl;
  cout << "The matching is done in dphi and dz directions, fit with double-Gaussian distribution" << endl;
  cout << endl;
  cout << "===================================================================" << endl;
}

int MatchrecalRecoRun15pp::process_event(PHCompositeNode *topNode)
{
   PHCentralTrack *d_cnt = findNode::getClass<PHCentralTrack>(topNode, "PHCentralTrack");
   PHGlobal *d_global = findNode::getClass<PHGlobal>(topNode, "PHGlobal");
   if (!d_cnt)
   {
      return 0;
   }
   if (!d_global)
   {
      return 0;
   }

   for(unsigned int i=0; i<d_cnt->get_npart(); i++){
      int arm = d_cnt->get_dcarm(i);
      int charge = d_cnt->get_charge(i);
      float mom = d_cnt->get_track(i)->get_mom();
      
      float the0 = d_cnt->get_track(i)->get_the0();
      float pt = -9999;
      float dummypt = -9999;
      if (the0 > -999)
        {
            pt = mom*sin(the0);
        }
      else
        {
            pt = mom;
        }
      
      dummypt = pt;
      if(dummypt < 0.5)
  {
    dummypt = 0.5;//To fix problem with extremelely narrow pc3s distributions at low pT
  }
      if(dummypt > 5.5)
	{
	  dummypt = 5.5;//To fix problem with extremelely narrow pc3s distributions at high pT
	}

      float pc3dphi = d_cnt->get_pc3dphi(i);
      float pc3dz = d_cnt->get_pc3dz(i);

      float pc3sdphi = GetPC3sdphi(arm,charge,dummypt,pc3dphi);
      float pc3sdz = GetPC3sdz(arm,charge,dummypt,pc3dz);
      
      d_cnt->set_pc3sdphi(i,pc3sdphi);
      d_cnt->set_pc3sdz(i,pc3sdz);
      
      float pc2dphi = d_cnt->get_pc2dphi(i);
      float pc2dz = d_cnt->get_pc2dz(i);

      float pc2sdphi = GetPC2sdphi(arm,charge,dummypt,pc2dphi);
      float pc2sdz = GetPC2sdz(arm,charge,dummypt,pc2dz);

      d_cnt->set_pc2sdphi(i,pc2sdphi);
      d_cnt->set_pc2sdz(i,pc2sdz);
   }

   return EVENT_OK;
}
       
float MatchrecalRecoRun15pp::GetPC3sdphi(const int iarm, const int icharge, const float pt, const float pc3dphi)
{
  if (iarm < 0 || iarm > 1)
    {
      return -999;
    }
  if (fabs(icharge) < 1 || fabs(icharge) > 1)
    {
      return -999;
    }
  if(pt < 0.2)
    {
      return -999;
    }
  float mean = 0;
  float sigma = 0;
  int jcharge = 0;
  if(icharge==1) jcharge = 0;
  if(icharge==-1) jcharge = 1;
  mean = PC3_dphi_mean[iarm][jcharge][0]+PC3_dphi_mean[iarm][jcharge][1]*pt+PC3_dphi_mean[iarm][jcharge][2]/pt+PC3_dphi_mean[iarm][jcharge][3]/sqrt(pt)+PC3_dphi_mean[iarm][jcharge][4]/pt/pt+PC3_dphi_mean[iarm][jcharge][5]/pt/pt/pt+PC3_dphi_mean[iarm][jcharge][6]/pt/pt/pt/pt;
  sigma = PC3_dphi_sigma[iarm][jcharge][0]+PC3_dphi_sigma[iarm][jcharge][1]*pt+PC3_dphi_sigma[iarm][jcharge][2]*pt*pt+PC3_dphi_sigma[iarm][jcharge][3]*pt*pt*pt+PC3_dphi_sigma[iarm][jcharge][4]*pt*pt*pt*pt+PC3_dphi_sigma[iarm][jcharge][5]*pt*pt*pt*pt*pt+PC3_dphi_sigma[iarm][jcharge][6]/sqrt(pt)+PC3_dphi_sigma[iarm][jcharge][7]/pt/pt;
  if (Verbosity()){
    cout <<  "GetPC3sdphi(" << iarm << ", " << icharge << ", "  << pt << ", " << pc3dphi << ") " <<
      "\nmean= " << mean << 
      endl;
    cout <<  "GetPC3sdphi(" << iarm << ", " << icharge << ", "  << pt << ", " << pc3dphi << ") " <<
      "\nsigma= " << sigma << 
      endl;
  }
  if(sigma == 0)
      return -999;
  else
    return (pc3dphi - mean) / sigma;
}


float MatchrecalRecoRun15pp::GetPC3sdz(const int iarm, const int icharge, const float pt, const float pc3dz)
{
  if (iarm < 0 || iarm > 1)
    {
      return -999;
    }
  if (fabs(icharge) < 1 || fabs(icharge) > 1)
    {
      return -999;
    }
  if(pt < 0.2)
    {
      return -999;
    }
  float mean = 0;
  float sigma = 0;
  int jcharge = 0;
  if(icharge==1) jcharge = 0;
  if(icharge==-1) jcharge = 1;
  mean = PC3_dz_mean[iarm][jcharge][0]+PC3_dz_mean[iarm][jcharge][1]*pt+PC3_dz_mean[iarm][jcharge][2]/pt+PC3_dz_mean[iarm][jcharge][3]/sqrt(pt)+PC3_dz_mean[iarm][jcharge][4]/pt/pt+PC3_dz_mean[iarm][jcharge][5]/pt/pt/pt+PC3_dz_mean[iarm][jcharge][6]/pt/pt/pt/pt;
  sigma = PC3_dz_sigma[iarm][jcharge][0]+PC3_dz_sigma[iarm][jcharge][1]*pt+PC3_dz_sigma[iarm][jcharge][2]*pt*pt+PC3_dz_sigma[iarm][jcharge][3]*pt*pt*pt+PC3_dz_sigma[iarm][jcharge][4]*pt*pt*pt*pt+PC3_dz_sigma[iarm][jcharge][5]*pt*pt*pt*pt*pt+PC3_dz_sigma[iarm][jcharge][6]/sqrt(pt)+PC3_dz_sigma[iarm][jcharge][7]/pt/pt;
  if (Verbosity()){
    cout <<  "GetPC3sdz(" << iarm << ", " << icharge << ", " << pt << ", " << pc3dz << ") " <<
      "\nmean= " << mean << 
      endl;
    cout <<  "GetPC3sdz(" << iarm << ", " << icharge << ", " << pt << ", " << pc3dz << ") " <<
      "\nsigma= " << sigma << 
      endl;
  }
  if(sigma == 0)
      return -999;
  else
    return (pc3dz - mean) / sigma;
}

float MatchrecalRecoRun15pp::GetPC2sdphi(const int iarm, const int icharge, const float pt, const float pc2dphi)
{
  if (iarm < 0 || iarm > 1)
    {
      return -999;
    }
  if (fabs(icharge) < 1 || fabs(icharge) > 1)
    {
      return -999;
    }
  if(pt < 0.2)
    {
      return -999;
    }
  float mean = 0;
  float sigma = 0;
  int jcharge = 0;
  if(icharge==1) jcharge = 0;
  if(icharge==-1) jcharge = 1;
  mean = PC2_dphi_mean[iarm][jcharge][0]+PC2_dphi_mean[iarm][jcharge][1]*pt+PC2_dphi_mean[iarm][jcharge][2]/pt+PC2_dphi_mean[iarm][jcharge][3]/sqrt(pt)+PC2_dphi_mean[iarm][jcharge][4]/pt/pt+PC2_dphi_mean[iarm][jcharge][5]/pt/pt/pt+PC2_dphi_mean[iarm][jcharge][6]/pt/pt/pt/pt;
  sigma = PC2_dphi_sigma[iarm][jcharge][0]+PC2_dphi_sigma[iarm][jcharge][1]*pt+PC2_dphi_sigma[iarm][jcharge][2]*pt*pt+PC2_dphi_sigma[iarm][jcharge][3]*pt*pt*pt+PC2_dphi_sigma[iarm][jcharge][4]*pt*pt*pt*pt+PC2_dphi_sigma[iarm][jcharge][5]*pt*pt*pt*pt*pt+PC2_dphi_sigma[iarm][jcharge][6]/sqrt(pt)+PC2_dphi_sigma[iarm][jcharge][7]/pt/pt;
  if (Verbosity()){
    cout <<  "GetPC2sdphi(" << iarm << ", " << icharge << ", "  << pt << ", " << pc2dphi << ") " <<
      "\nmean= " << mean << 
      endl;
    cout <<  "GetPC2sdphi(" << iarm << ", " << icharge << ", "  << pt << ", " << pc2dphi << ") " <<
      "\nsigma= " << sigma << 
      endl;
  }
  if(sigma == 0)
      return -999;
  else
    return (pc2dphi - mean) / sigma;
}


float MatchrecalRecoRun15pp::GetPC2sdz(const int iarm, const int icharge, const float pt, const float pc2dz)
{
  if (iarm < 0 || iarm > 1)
    {
      return -999;
    }
  if (fabs(icharge) < 1 || fabs(icharge) > 1)
    {
      return -999;
    }
  if(pt < 0.2)
    {
      return -999;
    }
  float mean = 0;
  float sigma = 0;
  int jcharge = 0;
  if(icharge==1) jcharge = 0;
  if(icharge==-1) jcharge = 1;
  
  if(pt > 3){  float pts = 3.;
  mean = PC2_dz_mean[iarm][jcharge][0]+PC2_dz_mean[iarm][jcharge][1]*pts+PC2_dz_mean[iarm][jcharge][2]/pts+PC2_dz_mean[iarm][jcharge][3]/sqrt(pts)+PC2_dz_mean[iarm][jcharge][4]/pts/pts+PC2_dz_mean[iarm][jcharge][5]/pts/pts/pts+PC2_dz_mean[iarm][jcharge][6]/pts/pts/pts/pts;
  sigma = PC2_dz_sigma[iarm][jcharge][0]+PC2_dz_sigma[iarm][jcharge][1]*pts+PC2_dz_sigma[iarm][jcharge][2]*pts*pts+PC2_dz_sigma[iarm][jcharge][3]*pts*pts*pts+PC2_dz_sigma[iarm][jcharge][4]*pts*pts*pts*pts+PC2_dz_sigma[iarm][jcharge][5]*pts*pts*pts*pts*pts+PC2_dz_sigma[iarm][jcharge][6]/sqrt(pts)+PC2_dz_sigma[iarm][jcharge][7]/pts/pts;
  }
  if (Verbosity()){
    cout <<  "GetPC2sdz(" << iarm << ", " << icharge << ", " << pt << ", " << pc2dz << ") " <<
      "\nmean= " << mean << 
      endl;
    cout <<  "GetPC2sdz(" << iarm << ", " << icharge << ", " << pt << ", " << pc2dz << ") " <<
      "\nsigma= " << sigma << 
      endl;
  }
  if(sigma == 0)
      return -999;
  else
    return (pc2dz - mean) / sigma;
}

void MatchrecalRecoRun15pp::initParameters()
{
  //
  // Initialization
  //

PC3_dphi_mean[0][0][0] = 0.000430828;
PC3_dphi_mean[0][0][1] = 0.000184655;
PC3_dphi_mean[0][0][2] = 0.000255305;
PC3_dphi_mean[0][0][3] = 0.000424993;
PC3_dphi_mean[0][0][4] = -4.54449e-05;
PC3_dphi_mean[0][0][5] = -9.50015e-05;
PC3_dphi_mean[0][0][6] = -7.1242e-05;
PC3_dphi_mean[0][1][0] = 0.000225081;
PC3_dphi_mean[0][1][1] = 8.27579e-05;
PC3_dphi_mean[0][1][2] = 0.00022704;
PC3_dphi_mean[0][1][3] = 0.000264965;
PC3_dphi_mean[0][1][4] = 9.43863e-05;
PC3_dphi_mean[0][1][5] = 3.53653e-05;
PC3_dphi_mean[0][1][6] = 1.50746e-05;
PC3_dphi_mean[1][0][0] = -0.000141353;
PC3_dphi_mean[1][0][1] = 3.34851e-05;
PC3_dphi_mean[1][0][2] = -0.000349821;
PC3_dphi_mean[1][0][3] = -0.000300917;
PC3_dphi_mean[1][0][4] = -0.000153129;
PC3_dphi_mean[1][0][5] = -1.51126e-05;
PC3_dphi_mean[1][0][6] = 2.18853e-05;
PC3_dphi_mean[1][1][0] = -0.000458309;
PC3_dphi_mean[1][1][1] = -0.000164498;
PC3_dphi_mean[1][1][2] = -0.000225325;
PC3_dphi_mean[1][1][3] = -0.000443789;
PC3_dphi_mean[1][1][4] = 9.92432e-05;
PC3_dphi_mean[1][1][5] = 9.66958e-05;
PC3_dphi_mean[1][1][6] = 4.55827e-05;
PC3_dz_mean[0][0][0] = 0.0235703;
PC3_dz_mean[0][0][1] = 0.00629264;
PC3_dz_mean[0][0][2] = -0.0161879;
PC3_dz_mean[0][0][3] = 0.012174;
PC3_dz_mean[0][0][4] = -0.0310034;
PC3_dz_mean[0][0][5] = -0.00672132;
PC3_dz_mean[0][0][6] = 0.00732984;
PC3_dz_mean[0][1][0] = 0.00506679;
PC3_dz_mean[0][1][1] = 0.0231576;
PC3_dz_mean[0][1][2] = -0.00957907;
PC3_dz_mean[0][1][3] = -0.0100321;
PC3_dz_mean[0][1][4] = 0.00665784;
PC3_dz_mean[0][1][5] = 0.00305429;
PC3_dz_mean[0][1][6] = -0.00358973;
PC3_dz_mean[1][0][0] = 1.09544;
PC3_dz_mean[1][0][1] = 0.0473387;
PC3_dz_mean[1][0][2] = -0.119676;
PC3_dz_mean[1][0][3] = 0.750498;
PC3_dz_mean[1][0][4] = -0.263252;
PC3_dz_mean[1][0][5] = 0.11495;
PC3_dz_mean[1][0][6] = -0.0154389;
PC3_dz_mean[1][1][0] = 1.11283;
PC3_dz_mean[1][1][1] = 0.0376005;
PC3_dz_mean[1][1][2] = -0.101173;
PC3_dz_mean[1][1][3] = 0.779793;
PC3_dz_mean[1][1][4] = -0.272148;
PC3_dz_mean[1][1][5] = 0.111514;
PC3_dz_mean[1][1][6] = -0.011072;
PC3_dphi_sigma[0][0][0] = 0.000521002;
PC3_dphi_sigma[0][0][1] = 0.000109566;
PC3_dphi_sigma[0][0][2] = 1.78554e-05;
PC3_dphi_sigma[0][0][3] = 2.77799e-06;
PC3_dphi_sigma[0][0][4] = 4.18492e-07;
PC3_dphi_sigma[0][0][5] = 5.93107e-08;
PC3_dphi_sigma[0][0][6] = 0.000835199;
PC3_dphi_sigma[0][0][7] = 0.000723165;
PC3_dphi_sigma[0][1][0] = 0.000505937;
PC3_dphi_sigma[0][1][1] = 0.000103122;
PC3_dphi_sigma[0][1][2] = 1.57483e-05;
PC3_dphi_sigma[0][1][3] = 2.18814e-06;
PC3_dphi_sigma[0][1][4] = 2.66865e-07;
PC3_dphi_sigma[0][1][5] = 2.21593e-08;
PC3_dphi_sigma[0][1][6] = 0.000815376;
PC3_dphi_sigma[0][1][7] = 0.000701127;
PC3_dphi_sigma[1][0][0] = 0.000606298;
PC3_dphi_sigma[1][0][1] = 0.00012682;
PC3_dphi_sigma[1][0][2] = 1.9435e-05;
PC3_dphi_sigma[1][0][3] = 2.67133e-06;
PC3_dphi_sigma[1][0][4] = 3.1842e-07;
PC3_dphi_sigma[1][0][5] = 2.48182e-08;
PC3_dphi_sigma[1][0][6] = 0.000955767;
PC3_dphi_sigma[1][0][7] = 0.000757969;
PC3_dphi_sigma[1][1][0] = 0.000661511;
PC3_dphi_sigma[1][1][1] = 0.000146384;
PC3_dphi_sigma[1][1][2] = 2.15976e-05;
PC3_dphi_sigma[1][1][3] = 2.55679e-06;
PC3_dphi_sigma[1][1][4] = 1.87326e-07;
PC3_dphi_sigma[1][1][5] = -2.12939e-08;
PC3_dphi_sigma[1][1][6] = 0.000975135;
PC3_dphi_sigma[1][1][7] = 0.00055282;
PC3_dz_sigma[0][0][0] = 0.801104;
PC3_dz_sigma[0][0][1] = 0.200279;
PC3_dz_sigma[0][0][2] = -0.00278419;
PC3_dz_sigma[0][0][3] = -0.00722536;
PC3_dz_sigma[0][0][4] = -0.000753957;
PC3_dz_sigma[0][0][5] = 0.000513381;
PC3_dz_sigma[0][0][6] = 0.690237;
PC3_dz_sigma[0][0][7] = 0.10851;
PC3_dz_sigma[0][1][0] = 0.627926;
PC3_dz_sigma[0][1][1] = 0.117274;
PC3_dz_sigma[0][1][2] = 0.0169546;
PC3_dz_sigma[0][1][3] = 0.00198406;
PC3_dz_sigma[0][1][4] = -0.000214229;
PC3_dz_sigma[0][1][5] = -0.000294597;
PC3_dz_sigma[0][1][6] = 0.966703;
PC3_dz_sigma[0][1][7] = 0.0450926;
PC3_dz_sigma[1][0][0] = 1.60725;
PC3_dz_sigma[1][0][1] = 0.0941546;
PC3_dz_sigma[1][0][2] = -0.0942929;
PC3_dz_sigma[1][0][3] = 0.00776694;
PC3_dz_sigma[1][0][4] = 0.00816979;
PC3_dz_sigma[1][0][5] = -0.00147169;
PC3_dz_sigma[1][0][6] = 0.118278;
PC3_dz_sigma[1][0][7] = 0.173903;
PC3_dz_sigma[1][1][0] = 1.12492;
PC3_dz_sigma[1][1][1] = 0.192291;
PC3_dz_sigma[1][1][2] = -0.0683753;
PC3_dz_sigma[1][1][3] = 0.000693014;
PC3_dz_sigma[1][1][4] = 0.00674303;
PC3_dz_sigma[1][1][5] = -0.00113004;
PC3_dz_sigma[1][1][6] = 0.467044;
PC3_dz_sigma[1][1][7] = 0.134633;

PC2_dphi_sigma[0][0][0] = 0;
PC2_dphi_sigma[0][0][1] = 0;
PC2_dphi_sigma[0][0][2] = 0;
PC2_dphi_sigma[0][0][3] = 0;
PC2_dphi_sigma[0][0][4] = 0;
PC2_dphi_sigma[0][0][5] = 0;
PC2_dphi_sigma[0][0][6] = 0;
PC2_dphi_sigma[0][0][7] = 0;
PC2_dphi_mean[0][0][0] = -0.0779735;
PC2_dphi_mean[0][0][1] = -0.00136205;
PC2_dphi_mean[0][0][2] = 0.0216856;
PC2_dphi_mean[0][0][3] = -0.0426029;
PC2_dphi_mean[0][0][4] = 0.0072229;
PC2_dphi_mean[0][0][5] = -0.00768278;
PC2_dphi_mean[0][0][6] = 0.00179557;
PC2_dz_sigma[0][0][0] = 0;
PC2_dz_sigma[0][0][1] = 0;
PC2_dz_sigma[0][0][2] = 0;
PC2_dz_sigma[0][0][3] = 0;
PC2_dz_sigma[0][0][4] = 0;
PC2_dz_sigma[0][0][5] = 0;
PC2_dz_sigma[0][0][6] = 0;
PC2_dz_sigma[0][0][7] = 0;
PC2_dz_mean[0][0][0] = -9.0597;
PC2_dz_mean[0][0][1] = -0.0288012;
PC2_dz_mean[0][0][2] = 2.21001;
PC2_dz_mean[0][0][3] = -2.24447;
PC2_dz_mean[0][0][4] = -0.956304;
PC2_dz_mean[0][0][5] = 0.327325;
PC2_dz_mean[0][0][6] = -0.047667;
PC2_dphi_sigma[0][1][0] = 0;
PC2_dphi_sigma[0][1][1] = 0;
PC2_dphi_sigma[0][1][2] = 0;
PC2_dphi_sigma[0][1][3] = 0;
PC2_dphi_sigma[0][1][4] = 0;
PC2_dphi_sigma[0][1][5] = 0;
PC2_dphi_sigma[0][1][6] = 0;
PC2_dphi_sigma[0][1][7] = 0;
PC2_dphi_mean[0][1][0] = -0.0779735;
PC2_dphi_mean[0][1][1] = -0.00136205;
PC2_dphi_mean[0][1][2] = 0.0216856;
PC2_dphi_mean[0][1][3] = -0.0426029;
PC2_dphi_mean[0][1][4] = 0.0072229;
PC2_dphi_mean[0][1][5] = -0.00768278;
PC2_dphi_mean[0][1][6] = 0.00179557;
PC2_dz_sigma[0][1][0] = 0;
PC2_dz_sigma[0][1][1] = 0;
PC2_dz_sigma[0][1][2] = 0;
PC2_dz_sigma[0][1][3] = 0;
PC2_dz_sigma[0][1][4] = 0;
PC2_dz_sigma[0][1][5] = 0;
PC2_dz_sigma[0][1][6] = 0;
PC2_dz_sigma[0][1][7] = 0;
PC2_dz_mean[0][1][0] = -9.0597;
PC2_dz_mean[0][1][1] = -0.0288012;
PC2_dz_mean[0][1][2] = 2.21001;
PC2_dz_mean[0][1][3] = -2.24447;
PC2_dz_mean[0][1][4] = -0.956304;
PC2_dz_mean[0][1][5] = 0.327325;
PC2_dz_mean[0][1][6] = -0.047667;
PC2_dphi_sigma[1][0][0] = 0.00233963;
PC2_dphi_sigma[1][0][1] = 0.00190131;
PC2_dphi_sigma[1][0][2] = -0.0020095;
PC2_dphi_sigma[1][0][3] = 0.00083418;
PC2_dphi_sigma[1][0][4] = -0.000155397;
PC2_dphi_sigma[1][0][5] = 1.10954e-05;
PC2_dphi_sigma[1][0][6] = -0.0015145;
PC2_dphi_sigma[1][0][7] = 0.000682337;
PC2_dphi_mean[1][0][0] = -0.0135043;
PC2_dphi_mean[1][0][1] = 0.000852687;
PC2_dphi_mean[1][0][2] = -0.0230986;
PC2_dphi_mean[1][0][3] = 0.0298397;
PC2_dphi_mean[1][0][4] = 0.00567858;
PC2_dphi_mean[1][0][5] = -0.000926723;
PC2_dphi_mean[1][0][6] = -2.10279e-05;
PC2_dz_sigma[1][0][0] = -75.9303;
PC2_dz_sigma[1][0][1] = 55.4911;
PC2_dz_sigma[1][0][2] = -29.7009;
PC2_dz_sigma[1][0][3] = 9.30328;
PC2_dz_sigma[1][0][4] = -1.53741;
PC2_dz_sigma[1][0][5] = 0.103232;
PC2_dz_sigma[1][0][6] = 45.5757;
PC2_dz_sigma[1][0][7] = -1.89641;
PC2_dz_mean[1][0][0] = -10.1431;
PC2_dz_mean[1][0][1] = 0.570331;
PC2_dz_mean[1][0][2] = -27.2593;
PC2_dz_mean[1][0][3] = 30.6813;
PC2_dz_mean[1][0][4] = 9.99743;
PC2_dz_mean[1][0][5] = -3.00855;
PC2_dz_mean[1][0][6] = 0.390668;
PC2_dphi_sigma[1][1][0] = 0.0209246;
PC2_dphi_sigma[1][1][1] = -0.00855383;
PC2_dphi_sigma[1][1][2] = 0.00201921;
PC2_dphi_sigma[1][1][3] = 2.10502e-05;
PC2_dphi_sigma[1][1][4] = -8.83301e-05;
PC2_dphi_sigma[1][1][5] = 1.05801e-05;
PC2_dphi_sigma[1][1][6] = -0.0136319;
PC2_dphi_sigma[1][1][7] = 0.00135336;
PC2_dphi_mean[1][1][0] = -0.0397639;
PC2_dphi_mean[1][1][1] = 0.0017228;
PC2_dphi_mean[1][1][2] = -0.100112;
PC2_dphi_mean[1][1][3] = 0.107875;
PC2_dphi_mean[1][1][4] = 0.0398134;
PC2_dphi_mean[1][1][5] = -0.0128254;
PC2_dphi_mean[1][1][6] = 0.00183279;
PC2_dz_sigma[1][1][0] = -19.173;
PC2_dz_sigma[1][1][1] = 16.0815;
PC2_dz_sigma[1][1][2] = -9.50981;
PC2_dz_sigma[1][1][3] = 3.2909;
PC2_dz_sigma[1][1][4] = -0.600411;
PC2_dz_sigma[1][1][5] = 0.044393;
PC2_dz_sigma[1][1][6] = 11.4768;
PC2_dz_sigma[1][1][7] = -0.281618;
PC2_dz_mean[1][1][0] = -4.6881;
PC2_dz_mean[1][1][1] = 0.311004;
PC2_dz_mean[1][1][2] = -13.5247;
PC2_dz_mean[1][1][3] = 15.5815;
PC2_dz_mean[1][1][4] = 4.73882;
PC2_dz_mean[1][1][5] = -1.34752;
PC2_dz_mean[1][1][6] = 0.163402;

  return;
}
