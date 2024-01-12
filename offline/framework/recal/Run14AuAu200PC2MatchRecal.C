#include "Run14AuAu200PC2MatchRecal.h"
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

//Run14AuAu200GeV PC3 matching recalibrator, adapted from A. Adare's MiniRecal
//Qiao Xu

using namespace std;
using namespace findNode;


Run14AuAu200PC2MatchRecal::Run14AuAu200PC2MatchRecal(const string &name): 
  Recalibrator(name)
{
  baseclasses.insert("PHCentralTrack");
  initParameters();
}

int Run14AuAu200PC2MatchRecal::isValidRun(const int runno) const
{
   if (runno < 405639  || runno > 414988)
   {
      return 0;
   }
   return 1;
}

void Run14AuAu200PC2MatchRecal::help() const
{
  cout << "===================================================================" << endl;
  cout << "Run14AuAu200PC2MatchRecal::help method output"                    << endl;
  cout << "Qiao Xu(qiao.xu@vanderbilt.edu) " << endl;
  cout << "PC2 matching for Run-14 Au+Au 200GeV " << endl;
  cout << "TOF matching will be added" << endl;
  cout << "Run14 Au+Au 200 GeV with minimum bias triggers" << endl;
  cout << "Central arm tracks with trasverse momentum < 0.2 GeV/c are ignored" << endl;
  cout << "The matching is done in dphi and dz directions, fit with double-Gaussian distribution" << endl;
  cout << endl;
  cout << "===================================================================" << endl;
}

int Run14AuAu200PC2MatchRecal::process_event(PHCompositeNode *topNode)
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

      
      float pc2dphi = d_cnt->get_pc2dphi(i);
      float pc2dz = d_cnt->get_pc2dz(i);

      float pc2sdphi = GetPC2sdphi(arm,charge,dummypt,pc2dphi);
      float pc2sdz = GetPC2sdz(arm,charge,dummypt,pc2dz);
      
      d_cnt->set_pc2sdphi(i,pc2sdphi);
      d_cnt->set_pc2sdz(i,pc2sdz);
      
   }

   return EVENT_OK;
}
      
float Run14AuAu200PC2MatchRecal::GetPC2sdphi(const int iarm, const int icharge, const float pt, const float pc2dphi)
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
  if(pt >= 0.2 && pt < 0.3){
    mean = PC2_dphifirst_mean[iarm][jcharge][0];
    sigma = PC2_dphifirst_sigma[iarm][jcharge][0];
  }
  else{
  mean = PC2_dphi_mean[iarm][jcharge][0]+PC2_dphi_mean[iarm][jcharge][1]*pt+PC2_dphi_mean[iarm][jcharge][2]/pt+PC2_dphi_mean[iarm][jcharge][3]/sqrt(pt)+PC2_dphi_mean[iarm][jcharge][4]/pt/pt+PC2_dphi_mean[iarm][jcharge][5]/pt/pt/pt+PC2_dphi_mean[iarm][jcharge][6]/pt/pt/pt/pt;
  sigma = PC2_dphi_sigma[iarm][jcharge][0]+PC2_dphi_sigma[iarm][jcharge][1]*pt+PC2_dphi_sigma[iarm][jcharge][2]*pt*pt+PC2_dphi_sigma[iarm][jcharge][3]*pt*pt*pt+PC2_dphi_sigma[iarm][jcharge][4]*pt*pt*pt*pt+PC2_dphi_sigma[iarm][jcharge][5]*pt*pt*pt*pt*pt+PC2_dphi_sigma[iarm][jcharge][6]/sqrt(pt)+PC2_dphi_sigma[iarm][jcharge][7]/pt/pt;
  }
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


float Run14AuAu200PC2MatchRecal::GetPC2sdz(const int iarm, const int icharge, const float pt, const float pc2dz)
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
  if(pt >= 0.2 && pt < 0.3){
    mean = PC2_dzfirst_mean[iarm][jcharge][0];
    sigma = PC2_dzfirst_sigma[iarm][jcharge][0];
  }
  else{
    float pts = pt;
    if(pts > 3.){ pts = 3.;}
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

void Run14AuAu200PC2MatchRecal::initParameters()
{
  //
  // Initialization
  //
PC2_dphifirst_mean[0][0][0] = -0.099;
PC2_dphifirst_sigma[0][0][0] = 0;
PC2_dzfirst_mean[0][0][0] = -9.8;
PC2_dzfirst_sigma[0][0][0] = 0;
PC2_dphifirst_mean[0][1][0] = -0.099;
PC2_dphifirst_sigma[0][1][0] = 0;
PC2_dzfirst_mean[0][1][0] = -9.8;
PC2_dzfirst_sigma[0][1][0] = 0;
PC2_dphifirst_mean[1][0][0] = -0.00612074;
PC2_dphifirst_sigma[1][0][0] = 0.00957479;
PC2_dzfirst_mean[1][0][0] = 1.17795;
PC2_dzfirst_sigma[1][0][0] = 2.12715;
PC2_dphifirst_mean[1][1][0] = 0.00364744;
PC2_dphifirst_sigma[1][1][0] = 0.00911213;
PC2_dzfirst_mean[1][1][0] = 1.26493;
PC2_dzfirst_sigma[1][1][0] = 2.22572;
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
PC2_dphi_sigma[1][0][0] = -0.031636;
PC2_dphi_sigma[1][0][1] = 0.0297056;
PC2_dphi_sigma[1][0][2] = -0.0181815;
PC2_dphi_sigma[1][0][3] = 0.00614063;
PC2_dphi_sigma[1][0][4] = -0.00104525;
PC2_dphi_sigma[1][0][5] = 7.04419e-05;
PC2_dphi_sigma[1][0][6] = 0.0171332;
PC2_dphi_sigma[1][0][7] = 3.86631e-06;
PC2_dphi_mean[1][0][0] = -0.0162044;
PC2_dphi_mean[1][0][1] = 0.00110399;
PC2_dphi_mean[1][0][2] = -0.0272144;
PC2_dphi_mean[1][0][3] = 0.0354708;
PC2_dphi_mean[1][0][4] = 0.0070862;
PC2_dphi_mean[1][0][5] = -0.00144678;
PC2_dphi_mean[1][0][6] = 7.58815e-05;
PC2_dz_sigma[1][0][0] = -15.6193;
PC2_dz_sigma[1][0][1] = 13.2367;
PC2_dz_sigma[1][0][2] = -7.95046;
PC2_dz_sigma[1][0][3] = 2.74876;
PC2_dz_sigma[1][0][4] = -0.495878;
PC2_dz_sigma[1][0][5] = 0.0362272;
PC2_dz_sigma[1][0][6] = 9.25204;
PC2_dz_sigma[1][0][7] = -0.286317;
PC2_dz_mean[1][0][0] = 1.03092;
PC2_dz_mean[1][0][1] = 0.0247302;
PC2_dz_mean[1][0][2] = 0.333661;
PC2_dz_mean[1][0][3] = 0.0947676;
PC2_dz_mean[1][0][4] = -0.463188;
PC2_dz_mean[1][0][5] = 0.242775;
PC2_dz_mean[1][0][6] = -0.0439683;
PC2_dphi_sigma[1][1][0] = 0.0640414;
PC2_dphi_sigma[1][1][1] = -0.0509144;
PC2_dphi_sigma[1][1][2] = 0.0315222;
PC2_dphi_sigma[1][1][3] = -0.0113826;
PC2_dphi_sigma[1][1][4] = 0.00211418;
PC2_dphi_sigma[1][1][5] = -0.000153428;
PC2_dphi_sigma[1][1][6] = -0.0350583;
PC2_dphi_sigma[1][1][7] = 0.00194294;
PC2_dphi_mean[1][1][0] = -0.0466041;
PC2_dphi_mean[1][1][1] = 0.00213876;
PC2_dphi_mean[1][1][2] = -0.112684;
PC2_dphi_mean[1][1][3] = 0.124525;
PC2_dphi_mean[1][1][4] = 0.0423318;
PC2_dphi_mean[1][1][5] = -0.0128809;
PC2_dphi_mean[1][1][6] = 0.00174387;
PC2_dz_sigma[1][1][0] = -10.1462;
PC2_dz_sigma[1][1][1] = 8.89949;
PC2_dz_sigma[1][1][2] = -5.44932;
PC2_dz_sigma[1][1][3] = 1.93427;
PC2_dz_sigma[1][1][4] = -0.360511;
PC2_dz_sigma[1][1][5] = 0.0273494;
PC2_dz_sigma[1][1][6] = 6.1221;
PC2_dz_sigma[1][1][7] = -0.149744;
PC2_dz_mean[1][1][0] = 1.3787;
PC2_dz_mean[1][1][1] = -0.0112356;
PC2_dz_mean[1][1][2] = -0.112813;
PC2_dz_mean[1][1][3] = -0.133274;
PC2_dz_mean[1][1][4] = 0.238034;
PC2_dz_mean[1][1][5] = -0.123824;
PC2_dz_mean[1][1][6] = 0.0221286;
  
return;
}
