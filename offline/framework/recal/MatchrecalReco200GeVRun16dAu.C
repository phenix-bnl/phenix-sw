#include "MatchrecalReco200GeVRun16dAu.h"
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

//200GeVRun16dAu PC3/PC2 matching recalibrator, adapted from A. Adare's MiniRecal
//A. Sickles

using namespace std;
using namespace findNode;


MatchrecalReco200GeVRun16dAu::MatchrecalReco200GeVRun16dAu(const string &name): 
  Recalibrator(name)
{
  baseclasses.insert("PHCentralTrack");
  initParameters();
}

int MatchrecalReco200GeVRun16dAu::isValidRun(const int runno) const
{
   if (runno < 454774 || runno > 455639)
   {
      return 0;
   }
   return 1;
}

void MatchrecalReco200GeVRun16dAu::help() const
{
  cout << "===================================================================" << endl;
  cout << "MatchrecalReco200GeVRun16dAu::help method output"                    << endl;
  cout << "Qiao Xu(qiao.xu@vanderbilt.edu), Weizhuang Peng(weizhuang.peng@vanderbilt.edu)" << endl;
  cout << "PC3/PC2 matching for Run-16 200GeV d+Au " << endl;
  cout << "TOF matching will be added" << endl;
  cout << "Run16 dAu 200 GeV with all the triggers combination" << endl;
  cout << "Central arm tracks with trasverse momentum < 0.2 GeV/c are ignored" << endl;
  cout << "The matching is done in dphi and dz directions, fit with double-Gaussian distribution" << endl;
  cout << endl;
  cout << "===================================================================" << endl;
}

int MatchrecalReco200GeVRun16dAu::process_event(PHCompositeNode *topNode)
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
  float bbcv = d_global->getBbcZVertex();
  int ivz=-1;
  if(bbcv>-30 && bbcv<=-20) ivz=0;
  else if(bbcv>-20 && bbcv<=-10) ivz=1;
  else if(bbcv>-10 && bbcv<=-5) ivz=2;
  else if(bbcv>-5 && bbcv<=0) ivz=3;
  else if(bbcv>0 && bbcv<=5) ivz=4;
  else if(bbcv>5 && bbcv<=10) ivz=5;
  else if(bbcv>10 && bbcv<=20) ivz=6;
  else if(bbcv>20 && bbcv<=30) ivz=7;
  else;
  if(ivz<0) return 0;

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
      
      if(dummypt > 5.5)
	{
	  dummypt = 5.5;//To fix problem with extremelely narrow pc3s distributions at high pT
	}

      float pc3dphi = d_cnt->get_pc3dphi(i);
      float pc3dz = d_cnt->get_pc3dz(i);

      float pc3sdphi = GetPC3sdphi(arm,charge,ivz,dummypt,pc3dphi);
      float pc3sdz = GetPC3sdz(arm,charge,ivz,dummypt,pc3dz);

      d_cnt->set_pc3sdphi(i,pc3sdphi);
      d_cnt->set_pc3sdz(i,pc3sdz);
      
      float pc2dphi = d_cnt->get_pc2dphi(i);
      float pc2dz = d_cnt->get_pc2dz(i);

      float pc2sdphi = GetPC2sdphi(arm,charge,0,dummypt,pc2dphi);
      float pc2sdz = GetPC2sdz(arm,charge,0,dummypt,pc2dz);
      
      d_cnt->set_pc2sdphi(i,pc2sdphi);
      d_cnt->set_pc2sdz(i,pc2sdz);
      
   }

   return EVENT_OK;
}
       
float MatchrecalReco200GeVRun16dAu::GetPC3sdphi(const int iarm, const int icharge, const int ivz, const float pt, const float pc3dphi)
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
      mean = PC3_dphifirst_mean[iarm][jcharge][ivz];
      sigma = PC3_dphifirst_sigma[iarm][jcharge][ivz];
  }
  else{
      mean = PC3_dphi_mean[iarm][jcharge][ivz][0]+PC3_dphi_mean[iarm][jcharge][ivz][1]*pt+PC3_dphi_mean[iarm][jcharge][ivz][2]/pt+PC3_dphi_mean[iarm][jcharge][ivz][3]/sqrt(pt)+PC3_dphi_mean[iarm][jcharge][ivz][4]/pt/pt+PC3_dphi_mean[iarm][jcharge][ivz][5]/pt/pt/pt+PC3_dphi_mean[iarm][jcharge][ivz][6]/pt/pt/pt/pt;
      sigma = PC3_dphi_sigma[iarm][jcharge][ivz][0]+PC3_dphi_sigma[iarm][jcharge][ivz][1]*pt+PC3_dphi_sigma[iarm][jcharge][ivz][2]*pt*pt+PC3_dphi_sigma[iarm][jcharge][ivz][3]*pt*pt*pt+PC3_dphi_sigma[iarm][jcharge][ivz][4]*pt*pt*pt*pt+PC3_dphi_sigma[iarm][jcharge][ivz][5]*pt*pt*pt*pt*pt+PC3_dphi_sigma[iarm][jcharge][ivz][6]/sqrt(pt)+PC3_dphi_sigma[iarm][jcharge][ivz][7]/pt/pt;
  }
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


float MatchrecalReco200GeVRun16dAu::GetPC3sdz(const int iarm, const int icharge, const int ivz, const float pt, const float pc3dz)
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
      mean = PC3_dzfirst_mean[iarm][jcharge][ivz];
      sigma = PC3_dzfirst_sigma[iarm][jcharge][ivz];
  }
  else{
      mean = PC3_dz_mean[iarm][jcharge][ivz][0]+PC3_dz_mean[iarm][jcharge][ivz][1]*pt+PC3_dz_mean[iarm][jcharge][ivz][2]/pt+PC3_dz_mean[iarm][jcharge][ivz][3]/sqrt(pt)+PC3_dz_mean[iarm][jcharge][ivz][4]/pt/pt+PC3_dz_mean[iarm][jcharge][ivz][5]/pt/pt/pt+PC3_dz_mean[iarm][jcharge][ivz][6]/pt/pt/pt/pt;
      sigma = PC3_dz_sigma[iarm][jcharge][ivz][0]+PC3_dz_sigma[iarm][jcharge][ivz][1]*pt+PC3_dz_sigma[iarm][jcharge][ivz][2]*pt*pt+PC3_dz_sigma[iarm][jcharge][ivz][3]*pt*pt*pt+PC3_dz_sigma[iarm][jcharge][ivz][4]*pt*pt*pt*pt+PC3_dz_sigma[iarm][jcharge][ivz][5]*pt*pt*pt*pt*pt+PC3_dz_sigma[iarm][jcharge][ivz][6]/sqrt(pt)+PC3_dz_sigma[iarm][jcharge][ivz][7]/pt/pt;
  }
  if (Verbosity()){
    cout <<  "GetPC3sdz(" << iarm << ", " << icharge << ", " << ivz << ", " << pt << ", " << pc3dz << ") " <<
      "\nmean= " << mean << 
      endl;
    cout <<  "GetPC3sdz(" << iarm << ", " << icharge << ", " << ivz << ", " << pt << ", " << pc3dz << ") " <<
      "\nsigma= " << sigma << 
      endl;
  }
  if(sigma == 0)
      return -999;
  else
    return (pc3dz - mean) / sigma;
}

float MatchrecalReco200GeVRun16dAu::GetPC2sdphi(const int iarm, const int icharge, const int ivz, const float pt, const float pc2dphi)
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
      mean = PC2_dphifirst_mean[iarm][jcharge][ivz];
      sigma = PC2_dphifirst_sigma[iarm][jcharge][ivz];
  }
  else{
      mean = PC2_dphi_mean[iarm][jcharge][ivz][0]+PC2_dphi_mean[iarm][jcharge][ivz][1]*pt+PC2_dphi_mean[iarm][jcharge][ivz][2]/pt+PC2_dphi_mean[iarm][jcharge][ivz][3]/sqrt(pt)+PC2_dphi_mean[iarm][jcharge][ivz][4]/pt/pt+PC2_dphi_mean[iarm][jcharge][ivz][5]/pt/pt/pt+PC2_dphi_mean[iarm][jcharge][ivz][6]/pt/pt/pt/pt;
      sigma = PC2_dphi_sigma[iarm][jcharge][ivz][0]+PC2_dphi_sigma[iarm][jcharge][ivz][1]*pt+PC2_dphi_sigma[iarm][jcharge][ivz][2]*pt*pt+PC2_dphi_sigma[iarm][jcharge][ivz][3]*pt*pt*pt+PC2_dphi_sigma[iarm][jcharge][ivz][4]*pt*pt*pt*pt+PC2_dphi_sigma[iarm][jcharge][ivz][5]*pt*pt*pt*pt*pt+PC2_dphi_sigma[iarm][jcharge][ivz][6]/sqrt(pt)+PC2_dphi_sigma[iarm][jcharge][ivz][7]/pt/pt;
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


float MatchrecalReco200GeVRun16dAu::GetPC2sdz(const int iarm, const int icharge, const int ivz, const float pt, const float pc2dz)
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
      mean = PC2_dzfirst_mean[iarm][jcharge][ivz];
      sigma = PC2_dzfirst_sigma[iarm][jcharge][ivz];
  }
  else{
      mean = PC2_dz_mean[iarm][jcharge][ivz][0]+PC2_dz_mean[iarm][jcharge][ivz][1]*pt+PC2_dz_mean[iarm][jcharge][ivz][2]/pt+PC2_dz_mean[iarm][jcharge][ivz][3]/sqrt(pt)+PC2_dz_mean[iarm][jcharge][ivz][4]/pt/pt+PC2_dz_mean[iarm][jcharge][ivz][5]/pt/pt/pt+PC2_dz_mean[iarm][jcharge][ivz][6]/pt/pt/pt/pt;
      sigma = PC2_dz_sigma[iarm][jcharge][ivz][0]+PC2_dz_sigma[iarm][jcharge][ivz][1]*pt+PC2_dz_sigma[iarm][jcharge][ivz][2]*pt*pt+PC2_dz_sigma[iarm][jcharge][ivz][3]*pt*pt*pt+PC2_dz_sigma[iarm][jcharge][ivz][4]*pt*pt*pt*pt+PC2_dz_sigma[iarm][jcharge][ivz][5]*pt*pt*pt*pt*pt+PC2_dz_sigma[iarm][jcharge][ivz][6]/sqrt(pt)+PC2_dz_sigma[iarm][jcharge][ivz][7]/pt/pt;
  }
  if (Verbosity()){
    cout <<  "GetPC2sdz(" << iarm << ", " << icharge << ", " << ivz << ", " << pt << ", " << pc2dz << ") " <<
      "\nmean= " << mean << 
      endl;
    cout <<  "GetPC2sdz(" << iarm << ", " << icharge << ", " << ivz << ", " << pt << ", " << pc2dz << ") " <<
      "\nsigma= " << sigma << 
      endl;
  }
  if(sigma == 0)
      return -999;
  else
    return (pc2dz - mean) / sigma;
}

void MatchrecalReco200GeVRun16dAu::initParameters()
{
  //
  // Initialization
  //
    PC3_dphifirst_mean[0][0][0] = 0.00590261;
    PC3_dphifirst_sigma[0][0][0] = 0.00872134;
    PC3_dzfirst_mean[0][0][0] = -0.413702;
    PC3_dzfirst_sigma[0][0][0] = 3.74288;
    PC3_dphifirst_mean[0][0][1] = -0.000967716;
    PC3_dphifirst_sigma[0][0][1] = 0.00935705;
    PC3_dzfirst_mean[0][0][1] = 0.319792;
    PC3_dzfirst_sigma[0][0][1] = 3.32479;
    PC3_dphifirst_mean[0][0][2] = -0.00408875;
    PC3_dphifirst_sigma[0][0][2] = 0.0103263;
    PC3_dzfirst_mean[0][0][2] = 1.04172;
    PC3_dzfirst_sigma[0][0][2] = 3.27094;
    PC3_dphifirst_mean[0][0][3] = -0.00569017;
    PC3_dphifirst_sigma[0][0][3] = 0.0108304;
    PC3_dzfirst_mean[0][0][3] = 1.71227;
    PC3_dzfirst_sigma[0][0][3] = 3.64608;
    PC3_dphifirst_mean[0][0][4] = -0.00770688;
    PC3_dphifirst_sigma[0][0][4] = 0.0121824;
    PC3_dzfirst_mean[0][0][4] = 2.22154;
    PC3_dzfirst_sigma[0][0][4] = 3.87744;
    PC3_dphifirst_mean[0][0][5] = -0.00770286;
    PC3_dphifirst_sigma[0][0][5] = 0.0123534;
    PC3_dzfirst_mean[0][0][5] = 2.65953;
    PC3_dzfirst_sigma[0][0][5] = 4.05635;
    PC3_dphifirst_mean[0][0][6] = -0.00597726;
    PC3_dphifirst_sigma[0][0][6] = 0.0124661;
    PC3_dzfirst_mean[0][0][6] = 3.41397;
    PC3_dzfirst_sigma[0][0][6] = 4.23821;
    PC3_dphifirst_mean[0][0][7] = 0.00372256;
    PC3_dphifirst_sigma[0][0][7] = 0.00899776;
    PC3_dzfirst_mean[0][0][7] = 3.51631;
    PC3_dzfirst_sigma[0][0][7] = 4.78841;
    PC3_dphifirst_mean[0][1][0] = -0.00412661;
    PC3_dphifirst_sigma[0][1][0] = 0.00898556;
    PC3_dzfirst_mean[0][1][0] = -0.353448;
    PC3_dzfirst_sigma[0][1][0] = 3.46221;
    PC3_dphifirst_mean[0][1][1] = 0.00233772;
    PC3_dphifirst_sigma[0][1][1] = 0.0094767;
    PC3_dzfirst_mean[0][1][1] = 0.419564;
    PC3_dzfirst_sigma[0][1][1] = 3.0672;
    PC3_dphifirst_mean[0][1][2] = 0.00557479;
    PC3_dphifirst_sigma[0][1][2] = 0.0106104;
    PC3_dzfirst_mean[0][1][2] = 1.1873;
    PC3_dzfirst_sigma[0][1][2] = 3.12046;
    PC3_dphifirst_mean[0][1][3] = 0.00703344;
    PC3_dphifirst_sigma[0][1][3] = 0.0105717;
    PC3_dzfirst_mean[0][1][3] = 1.89935;
    PC3_dzfirst_sigma[0][1][3] = 3.7494;
    PC3_dphifirst_mean[0][1][4] = 0.0093744;
    PC3_dphifirst_sigma[0][1][4] = 0.0110456;
    PC3_dzfirst_mean[0][1][4] = 2.40425;
    PC3_dzfirst_sigma[0][1][4] = 3.84576;
    PC3_dphifirst_mean[0][1][5] = 0.0108198;
    PC3_dphifirst_sigma[0][1][5] = 0.0111437;
    PC3_dzfirst_mean[0][1][5] = 2.89401;
    PC3_dzfirst_sigma[0][1][5] = 3.97185;
    PC3_dphifirst_mean[0][1][6] = 0.0122332;
    PC3_dphifirst_sigma[0][1][6] = 0.0125094;
    PC3_dzfirst_mean[0][1][6] = 3.64684;
    PC3_dzfirst_sigma[0][1][6] = 4.13885;
    PC3_dphifirst_mean[0][1][7] = 0.00595199;
    PC3_dphifirst_sigma[0][1][7] = 0.0119698;
    PC3_dzfirst_mean[0][1][7] = 3.75414;
    PC3_dzfirst_sigma[0][1][7] = 4.72637;
    PC3_dphifirst_mean[1][0][0] = 0.00310525;
    PC3_dphifirst_sigma[1][0][0] = 0.00909194;
    PC3_dzfirst_mean[1][0][0] = 0.43222;
    PC3_dzfirst_sigma[1][0][0] = 3.78951;
    PC3_dphifirst_mean[1][0][1] = -0.00466713;
    PC3_dphifirst_sigma[1][0][1] = 0.010313;
    PC3_dzfirst_mean[1][0][1] = 1.11647;
    PC3_dzfirst_sigma[1][0][1] = 3.35939;
    PC3_dphifirst_mean[1][0][2] = -0.00720583;
    PC3_dphifirst_sigma[1][0][2] = 0.0106337;
    PC3_dzfirst_mean[1][0][2] = 1.96504;
    PC3_dzfirst_sigma[1][0][2] = 3.63272;
    PC3_dphifirst_mean[1][0][3] = -0.00802655;
    PC3_dphifirst_sigma[1][0][3] = 0.010785;
    PC3_dzfirst_mean[1][0][3] = 2.59045;
    PC3_dzfirst_sigma[1][0][3] = 3.95993;
    PC3_dphifirst_mean[1][0][4] = -0.0087299;
    PC3_dphifirst_sigma[1][0][4] = 0.0114974;
    PC3_dzfirst_mean[1][0][4] = 3.13695;
    PC3_dzfirst_sigma[1][0][4] = 3.97786;
    PC3_dphifirst_mean[1][0][5] = -0.00800563;
    PC3_dphifirst_sigma[1][0][5] = 0.0115991;
    PC3_dzfirst_mean[1][0][5] = 3.70681;
    PC3_dzfirst_sigma[1][0][5] = 4.04324;
    PC3_dphifirst_mean[1][0][6] = -0.00503771;
    PC3_dphifirst_sigma[1][0][6] = 0.0109307;
    PC3_dzfirst_mean[1][0][6] = 4.5778;
    PC3_dzfirst_sigma[1][0][6] = 4.16783;
    PC3_dphifirst_mean[1][0][7] = 0.00293884;
    PC3_dphifirst_sigma[1][0][7] = 0.00954166;
    PC3_dzfirst_mean[1][0][7] = 5.0897;
    PC3_dzfirst_sigma[1][0][7] = 4.51585;
    PC3_dphifirst_mean[1][1][0] = -0.00397215;
    PC3_dphifirst_sigma[1][1][0] = 0.00860945;
    PC3_dzfirst_mean[1][1][0] = 0.582825;
    PC3_dzfirst_sigma[1][1][0] = 4.08607;
    PC3_dphifirst_mean[1][1][1] = 0.00432936;
    PC3_dphifirst_sigma[1][1][1] = 0.0101785;
    PC3_dzfirst_mean[1][1][1] = 1.03405;
    PC3_dzfirst_sigma[1][1][1] = 3.47433;
    PC3_dphifirst_mean[1][1][2] = 0.00616205;
    PC3_dphifirst_sigma[1][1][2] = 0.0104057;
    PC3_dzfirst_mean[1][1][2] = 1.78554;
    PC3_dzfirst_sigma[1][1][2] = 3.63201;
    PC3_dphifirst_mean[1][1][3] = 0.00685897;
    PC3_dphifirst_sigma[1][1][3] = 0.0109046;
    PC3_dzfirst_mean[1][1][3] = 2.38385;
    PC3_dzfirst_sigma[1][1][3] = 4.03878;
    PC3_dphifirst_mean[1][1][4] = 0.00655331;
    PC3_dphifirst_sigma[1][1][4] = 0.0111616;
    PC3_dzfirst_mean[1][1][4] = 2.94725;
    PC3_dzfirst_sigma[1][1][4] = 4.09979;
    PC3_dphifirst_mean[1][1][5] = 0.00532157;
    PC3_dphifirst_sigma[1][1][5] = 0.0111646;
    PC3_dzfirst_mean[1][1][5] = 3.50024;
    PC3_dzfirst_sigma[1][1][5] = 4.16278;
    PC3_dphifirst_mean[1][1][6] = 0.00212653;
    PC3_dphifirst_sigma[1][1][6] = 0.0101977;
    PC3_dzfirst_mean[1][1][6] = 4.33106;
    PC3_dzfirst_sigma[1][1][6] = 4.27961;
    PC3_dphifirst_mean[1][1][7] = -0.00427906;
    PC3_dphifirst_sigma[1][1][7] = 0.00914491;
    PC3_dzfirst_mean[1][1][7] = 4.66438;
    PC3_dzfirst_sigma[1][1][7] = 4.58705;
    PC3_dphi_sigma[0][0][0][0] = -0.013707;
    PC3_dphi_sigma[0][0][0][1] = 0.00875368;
    PC3_dphi_sigma[0][0][0][2] = -0.00352448;
    PC3_dphi_sigma[0][0][0][3] = 0.000769895;
    PC3_dphi_sigma[0][0][0][4] = -7.95832e-05;
    PC3_dphi_sigma[0][0][0][5] = 2.74635e-06;
    PC3_dphi_sigma[0][0][0][6] = 0.0097302;
    PC3_dphi_sigma[0][0][0][7] = 0.000216376;
    PC3_dphi_mean[0][0][0][0] = 0.00226427;
    PC3_dphi_mean[0][0][0][1] = -7.24725e-05;
    PC3_dphi_mean[0][0][0][2] = -0.000361859;
    PC3_dphi_mean[0][0][0][3] = -0.00117265;
    PC3_dphi_mean[0][0][0][4] = 0.000921569;
    PC3_dphi_mean[0][0][0][5] = -0.000509809;
    PC3_dphi_mean[0][0][0][6] = 9.49835e-05;
    PC3_dz_sigma[0][0][0][0] = 16.0758;
    PC3_dz_sigma[0][0][0][1] = -12.0172;
    PC3_dz_sigma[0][0][0][2] = 6.90192;
    PC3_dz_sigma[0][0][0][3] = -2.21131;
    PC3_dz_sigma[0][0][0][4] = 0.361568;
    PC3_dz_sigma[0][0][0][5] = -0.023404;
    PC3_dz_sigma[0][0][0][6] = -7.73047;
    PC3_dz_sigma[0][0][0][7] = 0.408178;
    PC3_dz_mean[0][0][0][0] = 2.21793;
    PC3_dz_mean[0][0][0][1] = -0.0112648;
    PC3_dz_mean[0][0][0][2] = 0.268024;
    PC3_dz_mean[0][0][0][3] = -1.61661;
    PC3_dz_mean[0][0][0][4] = 0.916099;
    PC3_dz_mean[0][0][0][5] = -0.708298;
    PC3_dz_mean[0][0][0][6] = 0.144812;
    PC3_dphi_sigma[0][0][1][0] = -0.000440752;
    PC3_dphi_sigma[0][0][1][1] = 0.000280856;
    PC3_dphi_sigma[0][0][1][2] = 0.000200028;
    PC3_dphi_sigma[0][0][1][3] = -0.000131901;
    PC3_dphi_sigma[0][0][1][4] = 2.85193e-05;
    PC3_dphi_sigma[0][0][1][5] = -2.01478e-06;
    PC3_dphi_sigma[0][0][1][6] = 0.00168846;
    PC3_dphi_sigma[0][0][1][7] = 0.000516776;
    PC3_dphi_mean[0][0][1][0] = -0.00212501;
    PC3_dphi_mean[0][0][1][1] = 0.000194114;
    PC3_dphi_mean[0][0][1][2] = -0.00865384;
    PC3_dphi_mean[0][0][1][3] = 0.00938771;
    PC3_dphi_mean[0][0][1][4] = 0.00308795;
    PC3_dphi_mean[0][0][1][5] = -0.000863547;
    PC3_dphi_mean[0][0][1][6] = 6.86866e-05;
    PC3_dz_sigma[0][0][1][0] = 4.25177;
    PC3_dz_sigma[0][0][1][1] = -3.19937;
    PC3_dz_sigma[0][0][1][2] = 1.98183;
    PC3_dz_sigma[0][0][1][3] = -0.638409;
    PC3_dz_sigma[0][0][1][4] = 0.103184;
    PC3_dz_sigma[0][0][1][5] = -0.00659742;
    PC3_dz_sigma[0][0][1][6] = -1.06727;
    PC3_dz_sigma[0][0][1][7] = 0.110484;
    PC3_dz_mean[0][0][1][0] = 10.5413;
    PC3_dz_mean[0][0][1][1] = -0.478083;
    PC3_dz_mean[0][0][1][2] = 18.5253;
    PC3_dz_mean[0][0][1][3] = -23.0367;
    PC3_dz_mean[0][0][1][4] = -5.49433;
    PC3_dz_mean[0][0][1][5] = 1.281;
    PC3_dz_mean[0][0][1][6] = -0.131511;
    PC3_dphi_sigma[0][0][2][0] = 0.000197513;
    PC3_dphi_sigma[0][0][2][1] = -0.000811855;
    PC3_dphi_sigma[0][0][2][2] = 0.0010459;
    PC3_dphi_sigma[0][0][2][3] = -0.000458902;
    PC3_dphi_sigma[0][0][2][4] = 8.90599e-05;
    PC3_dphi_sigma[0][0][2][5] = -6.25609e-06;
    PC3_dphi_sigma[0][0][2][6] = 0.00162818;
    PC3_dphi_sigma[0][0][2][7] = 0.000449147;
    PC3_dphi_mean[0][0][2][0] = -0.00425828;
    PC3_dphi_mean[0][0][2][1] = 0.000325497;
    PC3_dphi_mean[0][0][2][2] = -0.0126569;
    PC3_dphi_mean[0][0][2][3] = 0.0144987;
    PC3_dphi_mean[0][0][2][4] = 0.00415712;
    PC3_dphi_mean[0][0][2][5] = -0.00107346;
    PC3_dphi_mean[0][0][2][6] = 7.58494e-05;
    PC3_dz_sigma[0][0][2][0] = 2.15912;
    PC3_dz_sigma[0][0][2][1] = -1.69027;
    PC3_dz_sigma[0][0][2][2] = 1.20264;
    PC3_dz_sigma[0][0][2][3] = -0.404694;
    PC3_dz_sigma[0][0][2][4] = 0.0658489;
    PC3_dz_sigma[0][0][2][5] = -0.00412308;
    PC3_dz_sigma[0][0][2][6] = 0.139526;
    PC3_dz_sigma[0][0][2][7] = 0.0619283;
    PC3_dz_mean[0][0][2][0] = 15.0184;
    PC3_dz_mean[0][0][2][1] = -0.704979;
    PC3_dz_mean[0][0][2][2] = 27.4217;
    PC3_dz_mean[0][0][2][3] = -33.7039;
    PC3_dz_mean[0][0][2][4] = -8.31054;
    PC3_dz_mean[0][0][2][5] = 2.04537;
    PC3_dz_mean[0][0][2][6] = -0.22521;
    PC3_dphi_sigma[0][0][3][0] = -0.00109734;
    PC3_dphi_sigma[0][0][3][1] = 0.000221547;
    PC3_dphi_sigma[0][0][3][2] = 0.000401672;
    PC3_dphi_sigma[0][0][3][3] = -0.000224265;
    PC3_dphi_sigma[0][0][3][4] = 4.5167e-05;
    PC3_dphi_sigma[0][0][3][5] = -3.02437e-06;
    PC3_dphi_sigma[0][0][3][6] = 0.00238411;
    PC3_dphi_sigma[0][0][3][7] = 0.000405697;
    PC3_dphi_mean[0][0][3][0] = -0.00418687;
    PC3_dphi_mean[0][0][3][1] = 0.000339768;
    PC3_dphi_mean[0][0][3][2] = -0.0115433;
    PC3_dphi_mean[0][0][3][3] = 0.0137521;
    PC3_dphi_mean[0][0][3][4] = 0.0034528;
    PC3_dphi_mean[0][0][3][5] = -0.000789763;
    PC3_dphi_mean[0][0][3][6] = 2.60396e-05;
    PC3_dz_sigma[0][0][3][0] = 0.0763659;
    PC3_dz_sigma[0][0][3][1] = 0.0510602;
    PC3_dz_sigma[0][0][3][2] = 0.193358;
    PC3_dz_sigma[0][0][3][3] = -0.0698363;
    PC3_dz_sigma[0][0][3][4] = 0.00866858;
    PC3_dz_sigma[0][0][3][5] = -0.000301985;
    PC3_dz_sigma[0][0][3][6] = 1.32254;
    PC3_dz_sigma[0][0][3][7] = 0.0187862;
    PC3_dz_mean[0][0][3][0] = 17.4486;
    PC3_dz_mean[0][0][3][1] = -0.838321;
    PC3_dz_mean[0][0][3][2] = 31.4722;
    PC3_dz_mean[0][0][3][3] = -38.9945;
    PC3_dz_mean[0][0][3][4] = -9.31937;
    PC3_dz_mean[0][0][3][5] = 2.24269;
    PC3_dz_mean[0][0][3][6] = -0.238173;
    PC3_dphi_sigma[0][0][4][0] = 0.00256832;
    PC3_dphi_sigma[0][0][4][1] = -0.0025909;
    PC3_dphi_sigma[0][0][4][2] = 0.00197845;
    PC3_dphi_sigma[0][0][4][3] = -0.000734332;
    PC3_dphi_sigma[0][0][4][4] = 0.000131021;
    PC3_dphi_sigma[0][0][4][5] = -8.81019e-06;
    PC3_dphi_sigma[0][0][4][6] = 0.000291607;
    PC3_dphi_sigma[0][0][4][7] = 0.000499056;
    PC3_dphi_mean[0][0][4][0] = -0.00664778;
    PC3_dphi_mean[0][0][4][1] = 0.000481185;
    PC3_dphi_mean[0][0][4][2] = -0.0165365;
    PC3_dphi_mean[0][0][4][3] = 0.0198745;
    PC3_dphi_mean[0][0][4][4] = 0.00496753;
    PC3_dphi_mean[0][0][4][5] = -0.00118478;
    PC3_dphi_mean[0][0][4][6] = 6.83122e-05;
    PC3_dz_sigma[0][0][4][0] = 0.614253;
    PC3_dz_sigma[0][0][4][1] = 0.0617505;
    PC3_dz_sigma[0][0][4][2] = 0.0388948;
    PC3_dz_sigma[0][0][4][3] = 0.00815548;
    PC3_dz_sigma[0][0][4][4] = -0.00682674;
    PC3_dz_sigma[0][0][4][5] = 0.000831456;
    PC3_dz_sigma[0][0][4][6] = 0.852917;
    PC3_dz_sigma[0][0][4][7] = 0.0743415;
    PC3_dz_mean[0][0][4][0] = 18.8603;
    PC3_dz_mean[0][0][4][1] = -0.881805;
    PC3_dz_mean[0][0][4][2] = 35.2669;
    PC3_dz_mean[0][0][4][3] = -42.9608;
    PC3_dz_mean[0][0][4][4] = -10.7979;
    PC3_dz_mean[0][0][4][5] = 2.71359;
    PC3_dz_mean[0][0][4][6] = -0.300028;
    PC3_dphi_sigma[0][0][5][0] = 0.00479324;
    PC3_dphi_sigma[0][0][5][1] = -0.0034704;
    PC3_dphi_sigma[0][0][5][2] = 0.0021204;
    PC3_dphi_sigma[0][0][5][3] = -0.00069032;
    PC3_dphi_sigma[0][0][5][4] = 0.000111444;
    PC3_dphi_sigma[0][0][5][5] = -6.79986e-06;
    PC3_dphi_sigma[0][0][5][6] = -0.00137055;
    PC3_dphi_sigma[0][0][5][7] = 0.000652625;
    PC3_dphi_mean[0][0][5][0] = -0.00504373;
    PC3_dphi_mean[0][0][5][1] = 0.000399855;
    PC3_dphi_mean[0][0][5][2] = -0.0127765;
    PC3_dphi_mean[0][0][5][3] = 0.0155616;
    PC3_dphi_mean[0][0][5][4] = 0.00365765;
    PC3_dphi_mean[0][0][5][5] = -0.000849368;
    PC3_dphi_mean[0][0][5][6] = 2.70249e-05;
    PC3_dz_sigma[0][0][5][0] = 4.03075;
    PC3_dz_sigma[0][0][5][1] = -2.63717;
    PC3_dz_sigma[0][0][5][2] = 1.62329;
    PC3_dz_sigma[0][0][5][3] = -0.523478;
    PC3_dz_sigma[0][0][5][4] = 0.0840184;
    PC3_dz_sigma[0][0][5][5] = -0.00525493;
    PC3_dz_sigma[0][0][5][6] = -1.08969;
    PC3_dz_sigma[0][0][5][7] = 0.174433;
    PC3_dz_mean[0][0][5][0] = 22.4774;
    PC3_dz_mean[0][0][5][1] = -1.07067;
    PC3_dz_mean[0][0][5][2] = 42.7547;
    PC3_dz_mean[0][0][5][3] = -51.9276;
    PC3_dz_mean[0][0][5][4] = -13.1609;
    PC3_dz_mean[0][0][5][5] = 3.33907;
    PC3_dz_mean[0][0][5][6] = -0.371788;
    PC3_dphi_sigma[0][0][6][0] = 0.016382;
    PC3_dphi_sigma[0][0][6][1] = -0.0107928;
    PC3_dphi_sigma[0][0][6][2] = 0.00567305;
    PC3_dphi_sigma[0][0][6][3] = -0.0017231;
    PC3_dphi_sigma[0][0][6][4] = 0.000272529;
    PC3_dphi_sigma[0][0][6][5] = -1.70792e-05;
    PC3_dphi_sigma[0][0][6][6] = -0.00879993;
    PC3_dphi_sigma[0][0][6][7] = 0.00116129;
    PC3_dphi_mean[0][0][6][0] = -0.0048234;
    PC3_dphi_mean[0][0][6][1] = 0.000394319;
    PC3_dphi_mean[0][0][6][2] = -0.0121069;
    PC3_dphi_mean[0][0][6][3] = 0.0148583;
    PC3_dphi_mean[0][0][6][4] = 0.0034158;
    PC3_dphi_mean[0][0][6][5] = -0.000837374;
    PC3_dphi_mean[0][0][6][6] = 2.8088e-05;
    PC3_dz_sigma[0][0][6][0] = -6.59356;
    PC3_dz_sigma[0][0][6][1] = 5.15739;
    PC3_dz_sigma[0][0][6][2] = -2.51502;
    PC3_dz_sigma[0][0][6][3] = 0.746003;
    PC3_dz_sigma[0][0][6][4] = -0.118741;
    PC3_dz_sigma[0][0][6][5] = 0.00766056;
    PC3_dz_sigma[0][0][6][6] = 5.26402;
    PC3_dz_sigma[0][0][6][7] = -0.102378;
    PC3_dz_mean[0][0][6][0] = 27.9952;
    PC3_dz_mean[0][0][6][1] = -1.4057;
    PC3_dz_mean[0][0][6][2] = 52.3927;
    PC3_dz_mean[0][0][6][3] = -64.1923;
    PC3_dz_mean[0][0][6][4] = -15.9558;
    PC3_dz_mean[0][0][6][5] = 4.05922;
    PC3_dz_mean[0][0][6][6] = -0.457582;
    PC3_dphi_sigma[0][0][7][0] = 0.0336913;
    PC3_dphi_sigma[0][0][7][1] = -0.0202318;
    PC3_dphi_sigma[0][0][7][2] = 0.00958933;
    PC3_dphi_sigma[0][0][7][3] = -0.00266539;
    PC3_dphi_sigma[0][0][7][4] = 0.000388956;
    PC3_dphi_sigma[0][0][7][5] = -2.26436e-05;
    PC3_dphi_sigma[0][0][7][6] = -0.0207483;
    PC3_dphi_sigma[0][0][7][7] = 0.00221277;
    PC3_dphi_mean[0][0][7][0] = 0.000398455;
    PC3_dphi_mean[0][0][7][1] = 0.000135739;
    PC3_dphi_mean[0][0][7][2] = -4.88541e-05;
    PC3_dphi_mean[0][0][7][3] = 0.000962074;
    PC3_dphi_mean[0][0][7][4] = -0.000672164;
    PC3_dphi_mean[0][0][7][5] = 0.000181198;
    PC3_dphi_mean[0][0][7][6] = -8.77371e-05;
    PC3_dz_sigma[0][0][7][0] = -3.19388;
    PC3_dz_sigma[0][0][7][1] = 0.969763;
    PC3_dz_sigma[0][0][7][2] = 0.60574;
    PC3_dz_sigma[0][0][7][3] = -0.446226;
    PC3_dz_sigma[0][0][7][4] = 0.0991369;
    PC3_dz_sigma[0][0][7][5] = -0.0074135;
    PC3_dz_sigma[0][0][7][6] = 4.13911;
    PC3_dz_sigma[0][0][7][7] = -0.0864765;
    PC3_dz_mean[0][0][7][0] = 5.71355;
    PC3_dz_mean[0][0][7][1] = -0.319103;
    PC3_dz_mean[0][0][7][2] = -0.235081;
    PC3_dz_mean[0][0][7][3] = -4.83082;
    PC3_dz_mean[0][0][7][4] = 3.01803;
    PC3_dz_mean[0][0][7][5] = -1.48141;
    PC3_dz_mean[0][0][7][6] = 0.24977;
    PC3_dphi_sigma[0][1][0][0] = -0.0251111;
    PC3_dphi_sigma[0][1][0][1] = 0.0187634;
    PC3_dphi_sigma[0][1][0][2] = -0.0100801;
    PC3_dphi_sigma[0][1][0][3] = 0.00314553;
    PC3_dphi_sigma[0][1][0][4] = -0.000506949;
    PC3_dphi_sigma[0][1][0][5] = 3.24355e-05;
    PC3_dphi_sigma[0][1][0][6] = 0.016175;
    PC3_dphi_sigma[0][1][0][7] = -0.000303979;
    PC3_dphi_mean[0][1][0][0] = -0.000787946;
    PC3_dphi_mean[0][1][0][1] = 0.000171909;
    PC3_dphi_mean[0][1][0][2] = -7.87546e-05;
    PC3_dphi_mean[0][1][0][3] = 0.00224342;
    PC3_dphi_mean[0][1][0][4] = -0.00123356;
    PC3_dphi_mean[0][1][0][5] = 0.000822891;
    PC3_dphi_mean[0][1][0][6] = -0.000200619;
    PC3_dz_sigma[0][1][0][0] = -5.51986;
    PC3_dz_sigma[0][1][0][1] = 4.81617;
    PC3_dz_sigma[0][1][0][2] = -2.56946;
    PC3_dz_sigma[0][1][0][3] = 0.822771;
    PC3_dz_sigma[0][1][0][4] = -0.139253;
    PC3_dz_sigma[0][1][0][5] = 0.00946518;
    PC3_dz_sigma[0][1][0][6] = 4.41971;
    PC3_dz_sigma[0][1][0][7] = -0.102595;
    PC3_dz_mean[0][1][0][0] = 1.89195;
    PC3_dz_mean[0][1][0][1] = -0.0193472;
    PC3_dz_mean[0][1][0][2] = -0.0771419;
    PC3_dz_mean[0][1][0][3] = -0.778944;
    PC3_dz_mean[0][1][0][4] = 0.47598;
    PC3_dz_mean[0][1][0][5] = -0.34592;
    PC3_dz_mean[0][1][0][6] = 0.0618735;
    PC3_dphi_sigma[0][1][1][0] = 0.0202492;
    PC3_dphi_sigma[0][1][1][1] = -0.0123567;
    PC3_dphi_sigma[0][1][1][2] = 0.00570726;
    PC3_dphi_sigma[0][1][1][3] = -0.00151995;
    PC3_dphi_sigma[0][1][1][4] = 0.000213574;
    PC3_dphi_sigma[0][1][1][5] = -1.21764e-05;
    PC3_dphi_sigma[0][1][1][6] = -0.0112594;
    PC3_dphi_sigma[0][1][1][7] = 0.00107981;
    PC3_dphi_mean[0][1][1][0] = 0.00506018;
    PC3_dphi_mean[0][1][1][1] = -0.000182294;
    PC3_dphi_mean[0][1][1][2] = 0.0120606;
    PC3_dphi_mean[0][1][1][3] = -0.0124391;
    PC3_dphi_mean[0][1][1][4] = -0.00496819;
    PC3_dphi_mean[0][1][1][5] = 0.00169507;
    PC3_dphi_mean[0][1][1][6] = -0.000231364;
    PC3_dz_sigma[0][1][1][0] = 0.151186;
    PC3_dz_sigma[0][1][1][1] = 0.145367;
    PC3_dz_sigma[0][1][1][2] = 0.129595;
    PC3_dz_sigma[0][1][1][3] = -0.0713345;
    PC3_dz_sigma[0][1][1][4] = 0.0134113;
    PC3_dz_sigma[0][1][1][5] = -0.000792609;
    PC3_dz_sigma[0][1][1][6] = 1.10388;
    PC3_dz_sigma[0][1][1][7] = 0.0417759;
    PC3_dz_mean[0][1][1][0] = 7.76892;
    PC3_dz_mean[0][1][1][1] = -0.313859;
    PC3_dz_mean[0][1][1][2] = 13.3714;
    PC3_dz_mean[0][1][1][3] = -16.4021;
    PC3_dz_mean[0][1][1][4] = -4.0827;
    PC3_dz_mean[0][1][1][5] = 0.957378;
    PC3_dz_mean[0][1][1][6] = -0.0992071;
    PC3_dphi_sigma[0][1][2][0] = 0.0258064;
    PC3_dphi_sigma[0][1][2][1] = -0.0163706;
    PC3_dphi_sigma[0][1][2][2] = 0.00787651;
    PC3_dphi_sigma[0][1][2][3] = -0.00221072;
    PC3_dphi_sigma[0][1][2][4] = 0.000329453;
    PC3_dphi_sigma[0][1][2][5] = -1.99583e-05;
    PC3_dphi_sigma[0][1][2][6] = -0.0145537;
    PC3_dphi_sigma[0][1][2][7] = 0.0012385;
    PC3_dphi_mean[0][1][2][0] = 0.00875274;
    PC3_dphi_mean[0][1][2][1] = -0.000399472;
    PC3_dphi_mean[0][1][2][2] = 0.0199372;
    PC3_dphi_mean[0][1][2][3] = -0.0218382;
    PC3_dphi_mean[0][1][2][4] = -0.00754597;
    PC3_dphi_mean[0][1][2][5] = 0.0023904;
    PC3_dphi_mean[0][1][2][6] = -0.000297973;
    PC3_dz_sigma[0][1][2][0] = -3.96824;
    PC3_dz_sigma[0][1][2][1] = 3.52724;
    PC3_dz_sigma[0][1][2][2] = -1.91769;
    PC3_dz_sigma[0][1][2][3] = 0.64345;
    PC3_dz_sigma[0][1][2][4] = -0.114539;
    PC3_dz_sigma[0][1][2][5] = 0.00818833;
    PC3_dz_sigma[0][1][2][6] = 3.38075;
    PC3_dz_sigma[0][1][2][7] = -0.0486477;
    PC3_dz_mean[0][1][2][0] = 13.3455;
    PC3_dz_mean[0][1][2][1] = -0.598668;
    PC3_dz_mean[0][1][2][2] = 24.6828;
    PC3_dz_mean[0][1][2][3] = -29.9072;
    PC3_dz_mean[0][1][2][4] = -7.73575;
    PC3_dz_mean[0][1][2][5] = 1.96517;
    PC3_dz_mean[0][1][2][6] = -0.223233;
    PC3_dphi_sigma[0][1][3][0] = 0.0263903;
    PC3_dphi_sigma[0][1][3][1] = -0.0164723;
    PC3_dphi_sigma[0][1][3][2] = 0.00777225;
    PC3_dphi_sigma[0][1][3][3] = -0.00213074;
    PC3_dphi_sigma[0][1][3][4] = 0.000308915;
    PC3_dphi_sigma[0][1][3][5] = -1.81518e-05;
    PC3_dphi_sigma[0][1][3][6] = -0.015049;
    PC3_dphi_sigma[0][1][3][7] = 0.00129382;
    PC3_dphi_mean[0][1][3][0] = 0.00841973;
    PC3_dphi_mean[0][1][3][1] = -0.00039323;
    PC3_dphi_mean[0][1][3][2] = 0.0188973;
    PC3_dphi_mean[0][1][3][3] = -0.0207942;
    PC3_dphi_mean[0][1][3][4] = -0.00715596;
    PC3_dphi_mean[0][1][3][5] = 0.00229042;
    PC3_dphi_mean[0][1][3][6] = -0.000276912;
    PC3_dz_sigma[0][1][3][0] = -1.49225;
    PC3_dz_sigma[0][1][3][1] = 1.73231;
    PC3_dz_sigma[0][1][3][2] = -0.919482;
    PC3_dz_sigma[0][1][3][3] = 0.313712;
    PC3_dz_sigma[0][1][3][4] = -0.0562112;
    PC3_dz_sigma[0][1][3][5] = 0.00394912;
    PC3_dz_sigma[0][1][3][6] = 1.99362;
    PC3_dz_sigma[0][1][3][7] = 0.012443;
    PC3_dz_mean[0][1][3][0] = 16.0719;
    PC3_dz_mean[0][1][3][1] = -0.739236;
    PC3_dz_mean[0][1][3][2] = 29.6599;
    PC3_dz_mean[0][1][3][3] = -36.1399;
    PC3_dz_mean[0][1][3][4] = -9.09881;
    PC3_dz_mean[0][1][3][5] = 2.26265;
    PC3_dz_mean[0][1][3][6] = -0.248403;
    PC3_dphi_sigma[0][1][4][0] = 0.0275128;
    PC3_dphi_sigma[0][1][4][1] = -0.0172595;
    PC3_dphi_sigma[0][1][4][2] = 0.00820014;
    PC3_dphi_sigma[0][1][4][3] = -0.00226509;
    PC3_dphi_sigma[0][1][4][4] = 0.00033009;
    PC3_dphi_sigma[0][1][4][5] = -1.94272e-05;
    PC3_dphi_sigma[0][1][4][6] = -0.0157558;
    PC3_dphi_sigma[0][1][4][7] = 0.00134466;
    PC3_dphi_mean[0][1][4][0] = 0.00487827;
    PC3_dphi_mean[0][1][4][1] = -0.000208792;
    PC3_dphi_mean[0][1][4][2] = 0.0109847;
    PC3_dphi_mean[0][1][4][3] = -0.0115338;
    PC3_dphi_mean[0][1][4][4] = -0.00455767;
    PC3_dphi_mean[0][1][4][5] = 0.00161925;
    PC3_dphi_mean[0][1][4][6] = -0.000193377;
    PC3_dz_sigma[0][1][4][0] = 0.933873;
    PC3_dz_sigma[0][1][4][1] = 0.154726;
    PC3_dz_sigma[0][1][4][2] = -0.0963833;
    PC3_dz_sigma[0][1][4][3] = 0.0499634;
    PC3_dz_sigma[0][1][4][4] = -0.0112084;
    PC3_dz_sigma[0][1][4][5] = 0.00089421;
    PC3_dz_sigma[0][1][4][6] = 0.495014;
    PC3_dz_sigma[0][1][4][7] = 0.107605;
    PC3_dz_mean[0][1][4][0] = 16.8156;
    PC3_dz_mean[0][1][4][1] = -0.769293;
    PC3_dz_mean[0][1][4][2] = 31.0683;
    PC3_dz_mean[0][1][4][3] = -37.8119;
    PC3_dz_mean[0][1][4][4] = -9.54701;
    PC3_dz_mean[0][1][4][5] = 2.40717;
    PC3_dz_mean[0][1][4][6] = -0.267716;
    PC3_dphi_sigma[0][1][5][0] = 0.0312659;
    PC3_dphi_sigma[0][1][5][1] = -0.0195118;
    PC3_dphi_sigma[0][1][5][2] = 0.00928004;
    PC3_dphi_sigma[0][1][5][3] = -0.00258413;
    PC3_dphi_sigma[0][1][5][4] = 0.000381481;
    PC3_dphi_sigma[0][1][5][5] = -2.28353e-05;
    PC3_dphi_sigma[0][1][5][6] = -0.0182788;
    PC3_dphi_sigma[0][1][5][7] = 0.00155465;
    PC3_dphi_mean[0][1][5][0] = 0.00745882;
    PC3_dphi_mean[0][1][5][1] = -0.000318654;
    PC3_dphi_mean[0][1][5][2] = 0.0176719;
    PC3_dphi_mean[0][1][5][3] = -0.0188723;
    PC3_dphi_mean[0][1][5][4] = -0.00698446;
    PC3_dphi_mean[0][1][5][5] = 0.00233514;
    PC3_dphi_mean[0][1][5][6] = -0.000270916;
    PC3_dz_sigma[0][1][5][0] = 4.10158;
    PC3_dz_sigma[0][1][5][1] = -2.04626;
    PC3_dz_sigma[0][1][5][2] = 1.01885;
    PC3_dz_sigma[0][1][5][3] = -0.268756;
    PC3_dz_sigma[0][1][5][4] = 0.0347573;
    PC3_dz_sigma[0][1][5][5] = -0.001675;
    PC3_dz_sigma[0][1][5][6] = -1.39798;
    PC3_dz_sigma[0][1][5][7] = 0.210011;
    PC3_dz_mean[0][1][5][0] = 23.0063;
    PC3_dz_mean[0][1][5][1] = -1.1103;
    PC3_dz_mean[0][1][5][2] = 44.08;
    PC3_dz_mean[0][1][5][3] = -53.3037;
    PC3_dz_mean[0][1][5][4] = -13.8442;
    PC3_dz_mean[0][1][5][5] = 3.62394;
    PC3_dz_mean[0][1][5][6] = -0.421559;
    PC3_dphi_sigma[0][1][6][0] = 0.0398795;
    PC3_dphi_sigma[0][1][6][1] = -0.024296;
    PC3_dphi_sigma[0][1][6][2] = 0.0112993;
    PC3_dphi_sigma[0][1][6][3] = -0.00308922;
    PC3_dphi_sigma[0][1][6][4] = 0.000448598;
    PC3_dphi_sigma[0][1][6][5] = -2.64551e-05;
    PC3_dphi_sigma[0][1][6][6] = -0.0241572;
    PC3_dphi_sigma[0][1][6][7] = 0.00203358;
    PC3_dphi_mean[0][1][6][0] = 0.00746367;
    PC3_dphi_mean[0][1][6][1] = -0.000294268;
    PC3_dphi_mean[0][1][6][2] = 0.0190426;
    PC3_dphi_mean[0][1][6][3] = -0.0196559;
    PC3_dphi_mean[0][1][6][4] = -0.00796245;
    PC3_dphi_mean[0][1][6][5] = 0.00278309;
    PC3_dphi_mean[0][1][6][6] = -0.000329302;
    PC3_dz_sigma[0][1][6][0] = 4.11147;
    PC3_dz_sigma[0][1][6][1] = -2.04905;
    PC3_dz_sigma[0][1][6][2] = 1.09481;
    PC3_dz_sigma[0][1][6][3] = -0.315561;
    PC3_dz_sigma[0][1][6][4] = 0.0452609;
    PC3_dz_sigma[0][1][6][5] = -0.00250536;
    PC3_dz_sigma[0][1][6][6] = -1.25578;
    PC3_dz_sigma[0][1][6][7] = 0.204203;
    PC3_dz_mean[0][1][6][0] = 28.8568;
    PC3_dz_mean[0][1][6][1] = -1.43411;
    PC3_dz_mean[0][1][6][2] = 55.0486;
    PC3_dz_mean[0][1][6][3] = -66.8794;
    PC3_dz_mean[0][1][6][4] = -17.0416;
    PC3_dz_mean[0][1][6][5] = 4.37035;
    PC3_dz_mean[0][1][6][6] = -0.49319;
    PC3_dphi_sigma[0][1][7][0] = 0.0558648;
    PC3_dphi_sigma[0][1][7][1] = -0.0328349;
    PC3_dphi_sigma[0][1][7][2] = 0.0148833;
    PC3_dphi_sigma[0][1][7][3] = -0.00400138;
    PC3_dphi_sigma[0][1][7][4] = 0.000574255;
    PC3_dphi_sigma[0][1][7][5] = -3.36805e-05;
    PC3_dphi_sigma[0][1][7][6] = -0.0354404;
    PC3_dphi_sigma[0][1][7][7] = 0.00310501;
    PC3_dphi_mean[0][1][7][0] = 0.0115665;
    PC3_dphi_mean[0][1][7][1] = -0.000437585;
    PC3_dphi_mean[0][1][7][2] = 0.0311086;
    PC3_dphi_mean[0][1][7][3] = -0.0320884;
    PC3_dphi_mean[0][1][7][4] = -0.0131343;
    PC3_dphi_mean[0][1][7][5] = 0.00464649;
    PC3_dphi_mean[0][1][7][6] = -0.000585021;
    PC3_dz_sigma[0][1][7][0] = 7.27006;
    PC3_dz_sigma[0][1][7][1] = -3.87009;
    PC3_dz_sigma[0][1][7][2] = 1.89256;
    PC3_dz_sigma[0][1][7][3] = -0.506956;
    PC3_dz_sigma[0][1][7][4] = 0.0661742;
    PC3_dz_sigma[0][1][7][5] = -0.00327292;
    PC3_dz_sigma[0][1][7][6] = -3.11798;
    PC3_dz_sigma[0][1][7][7] = 0.345739;
    PC3_dz_mean[0][1][7][0] = 27.5503;
    PC3_dz_mean[0][1][7][1] = -1.33633;
    PC3_dz_mean[0][1][7][2] = 54.2754;
    PC3_dz_mean[0][1][7][3] = -65.2898;
    PC3_dz_mean[0][1][7][4] = -16.9459;
    PC3_dz_mean[0][1][7][5] = 4.33624;
    PC3_dz_mean[0][1][7][6] = -0.475323;
    PC3_dphi_sigma[1][0][0][0] = 0.000526781;
    PC3_dphi_sigma[1][0][0][1] = 0.00516453;
    PC3_dphi_sigma[1][0][0][2] = -0.00449067;
    PC3_dphi_sigma[1][0][0][3] = 0.00180207;
    PC3_dphi_sigma[1][0][0][4] = -0.000338131;
    PC3_dphi_sigma[1][0][0][5] = 2.39958e-05;
    PC3_dphi_sigma[1][0][0][6] = -0.00124331;
    PC3_dphi_sigma[1][0][0][7] = 0.00121267;
    PC3_dphi_mean[1][0][0][0] = 0.0243622;
    PC3_dphi_mean[1][0][0][1] = -0.00101327;
    PC3_dphi_mean[1][0][0][2] = 0.0699155;
    PC3_dphi_mean[1][0][0][3] = -0.0732104;
    PC3_dphi_mean[1][0][0][4] = -0.0295797;
    PC3_dphi_mean[1][0][0][5] = 0.00998302;
    PC3_dphi_mean[1][0][0][6] = -0.001492;
    PC3_dz_sigma[1][0][0][0] = -14.9931;
    PC3_dz_sigma[1][0][0][1] = 12.742;
    PC3_dz_sigma[1][0][0][2] = -7.40028;
    PC3_dz_sigma[1][0][0][3] = 2.48059;
    PC3_dz_sigma[1][0][0][4] = -0.428916;
    PC3_dz_sigma[1][0][0][5] = 0.0294482;
    PC3_dz_sigma[1][0][0][6] = 9.70552;
    PC3_dz_sigma[1][0][0][7] = -0.287758;
    PC3_dz_mean[1][0][0][0] = -13.4734;
    PC3_dz_mean[1][0][0][1] = 0.79118;
    PC3_dz_mean[1][0][0][2] = -32.9782;
    PC3_dz_mean[1][0][0][3] = 39.8833;
    PC3_dz_mean[1][0][0][4] = 10.0531;
    PC3_dz_mean[1][0][0][5] = -2.58058;
    PC3_dz_mean[1][0][0][6] = 0.291023;
    PC3_dphi_sigma[1][0][1][0] = -0.0122928;
    PC3_dphi_sigma[1][0][1][1] = 0.0114857;
    PC3_dphi_sigma[1][0][1][2] = -0.00675841;
    PC3_dphi_sigma[1][0][1][3] = 0.00224377;
    PC3_dphi_sigma[1][0][1][4] = -0.000378062;
    PC3_dphi_sigma[1][0][1][5] = 2.52606e-05;
    PC3_dphi_sigma[1][0][1][6] = 0.00790127;
    PC3_dphi_sigma[1][0][1][7] = 0.000387741;
    PC3_dphi_mean[1][0][1][0] = 0.0102251;
    PC3_dphi_mean[1][0][1][1] = -0.000231898;
    PC3_dphi_mean[1][0][1][2] = 0.0386996;
    PC3_dphi_mean[1][0][1][3] = -0.0366085;
    PC3_dphi_mean[1][0][1][4] = -0.0190342;
    PC3_dphi_mean[1][0][1][5] = 0.0070952;
    PC3_dphi_mean[1][0][1][6] = -0.00114896;
    PC3_dz_sigma[1][0][1][0] = -5.26068;
    PC3_dz_sigma[1][0][1][1] = 4.0215;
    PC3_dz_sigma[1][0][1][2] = -1.93909;
    PC3_dz_sigma[1][0][1][3] = 0.536882;
    PC3_dz_sigma[1][0][1][4] = -0.075456;
    PC3_dz_sigma[1][0][1][5] = 0.00420882;
    PC3_dz_sigma[1][0][1][6] = 4.42645;
    PC3_dz_sigma[1][0][1][7] = -0.108519;
    PC3_dz_mean[1][0][1][0] = -10.5302;
    PC3_dz_mean[1][0][1][1] = 0.642606;
    PC3_dz_mean[1][0][1][2] = -27.2109;
    PC3_dz_mean[1][0][1][3] = 32.6363;
    PC3_dz_mean[1][0][1][4] = 8.57228;
    PC3_dz_mean[1][0][1][5] = -2.26793;
    PC3_dz_mean[1][0][1][6] = 0.264171;
    PC3_dphi_sigma[1][0][2][0] = -0.0206473;
    PC3_dphi_sigma[1][0][2][1] = 0.0167009;
    PC3_dphi_sigma[1][0][2][2] = -0.00921765;
    PC3_dphi_sigma[1][0][2][3] = 0.00292809;
    PC3_dphi_sigma[1][0][2][4] = -0.000479258;
    PC3_dphi_sigma[1][0][2][5] = 3.13836e-05;
    PC3_dphi_sigma[1][0][2][6] = 0.0132567;
    PC3_dphi_sigma[1][0][2][7] = 3.30825e-05;
    PC3_dphi_mean[1][0][2][0] = 0.0068264;
    PC3_dphi_mean[1][0][2][1] = -1.84259e-05;
    PC3_dphi_mean[1][0][2][2] = 0.0317621;
    PC3_dphi_mean[1][0][2][3] = -0.0281644;
    PC3_dphi_mean[1][0][2][4] = -0.0167752;
    PC3_dphi_mean[1][0][2][5] = 0.00647539;
    PC3_dphi_mean[1][0][2][6] = -0.00106915;
    PC3_dz_sigma[1][0][2][0] = -14.7907;
    PC3_dz_sigma[1][0][2][1] = 11.2331;
    PC3_dz_sigma[1][0][2][2] = -5.95367;
    PC3_dz_sigma[1][0][2][3] = 1.83296;
    PC3_dz_sigma[1][0][2][4] = -0.293206;
    PC3_dz_sigma[1][0][2][5] = 0.0187561;
    PC3_dz_sigma[1][0][2][6] = 9.91139;
    PC3_dz_sigma[1][0][2][7] = -0.326046;
    PC3_dz_mean[1][0][2][0] = -6.48826;
    PC3_dz_mean[1][0][2][1] = 0.453913;
    PC3_dz_mean[1][0][2][2] = -18.7422;
    PC3_dz_mean[1][0][2][3] = 22.8238;
    PC3_dz_mean[1][0][2][4] = 5.78282;
    PC3_dz_mean[1][0][2][5] = -1.49235;
    PC3_dz_mean[1][0][2][6] = 0.168542;
    PC3_dphi_sigma[1][0][3][0] = -0.0252221;
    PC3_dphi_sigma[1][0][3][1] = 0.0193911;
    PC3_dphi_sigma[1][0][3][2] = -0.0103354;
    PC3_dphi_sigma[1][0][3][3] = 0.00317801;
    PC3_dphi_sigma[1][0][3][4] = -0.000504711;
    PC3_dphi_sigma[1][0][3][5] = 3.21462e-05;
    PC3_dphi_sigma[1][0][3][6] = 0.0162222;
    PC3_dphi_sigma[1][0][3][7] = -0.000153689;
    PC3_dphi_mean[1][0][3][0] = 0.00662686;
    PC3_dphi_mean[1][0][3][1] = -1.60313e-07;
    PC3_dphi_mean[1][0][3][2] = 0.0313803;
    PC3_dphi_mean[1][0][3][3] = -0.0276787;
    PC3_dphi_mean[1][0][3][4] = -0.0165891;
    PC3_dphi_mean[1][0][3][5] = 0.00639275;
    PC3_dphi_mean[1][0][3][6] = -0.00104993;
    PC3_dz_sigma[1][0][3][0] = -18.1608;
    PC3_dz_sigma[1][0][3][1] = 13.3629;
    PC3_dz_sigma[1][0][3][2] = -6.80792;
    PC3_dz_sigma[1][0][3][3] = 1.99186;
    PC3_dz_sigma[1][0][3][4] = -0.300176;
    PC3_dz_sigma[1][0][3][5] = 0.0179751;
    PC3_dz_sigma[1][0][3][6] = 12.0461;
    PC3_dz_sigma[1][0][3][7] = -0.418764;
    PC3_dz_mean[1][0][3][0] = -2.45609;
    PC3_dz_mean[1][0][3][1] = 0.262708;
    PC3_dz_mean[1][0][3][2] = -9.50059;
    PC3_dz_mean[1][0][3][3] = 12.5387;
    PC3_dz_mean[1][0][3][4] = 2.29425;
    PC3_dz_mean[1][0][3][5] = -0.36719;
    PC3_dz_mean[1][0][3][6] = 0.00948093;
    PC3_dphi_sigma[1][0][4][0] = -0.0216309;
    PC3_dphi_sigma[1][0][4][1] = 0.0166619;
    PC3_dphi_sigma[1][0][4][2] = -0.00885034;
    PC3_dphi_sigma[1][0][4][3] = 0.00272261;
    PC3_dphi_sigma[1][0][4][4] = -0.000433854;
    PC3_dphi_sigma[1][0][4][5] = 2.7853e-05;
    PC3_dphi_sigma[1][0][4][6] = 0.0141821;
    PC3_dphi_sigma[1][0][4][7] = -6.80102e-05;
    PC3_dphi_mean[1][0][4][0] = 0.00736969;
    PC3_dphi_mean[1][0][4][1] = -3.21113e-05;
    PC3_dphi_mean[1][0][4][2] = 0.033127;
    PC3_dphi_mean[1][0][4][3] = -0.0296554;
    PC3_dphi_mean[1][0][4][4] = -0.0172064;
    PC3_dphi_mean[1][0][4][5] = 0.00657736;
    PC3_dphi_mean[1][0][4][6] = -0.00107257;
    PC3_dz_sigma[1][0][4][0] = -21.8249;
    PC3_dz_sigma[1][0][4][1] = 16.3515;
    PC3_dz_sigma[1][0][4][2] = -8.55982;
    PC3_dz_sigma[1][0][4][3] = 2.57921;
    PC3_dz_sigma[1][0][4][4] = -0.401563;
    PC3_dz_sigma[1][0][4][5] = 0.0249199;
    PC3_dz_sigma[1][0][4][6] = 14.1196;
    PC3_dz_sigma[1][0][4][7] = -0.496288;
    PC3_dz_mean[1][0][4][0] = -2.53712;
    PC3_dz_mean[1][0][4][1] = 0.279509;
    PC3_dz_mean[1][0][4][2] = -9.94673;
    PC3_dz_mean[1][0][4][3] = 13.0641;
    PC3_dz_mean[1][0][4][4] = 2.53693;
    PC3_dz_mean[1][0][4][5] = -0.466952;
    PC3_dz_mean[1][0][4][6] = 0.029264;
    PC3_dphi_sigma[1][0][5][0] = -0.0229382;
    PC3_dphi_sigma[1][0][5][1] = 0.0176987;
    PC3_dphi_sigma[1][0][5][2] = -0.00940416;
    PC3_dphi_sigma[1][0][5][3] = 0.00288624;
    PC3_dphi_sigma[1][0][5][4] = -0.000458035;
    PC3_dphi_sigma[1][0][5][5] = 2.92171e-05;
    PC3_dphi_sigma[1][0][5][6] = 0.0148793;
    PC3_dphi_sigma[1][0][5][7] = -7.39098e-05;
    PC3_dphi_mean[1][0][5][0] = 0.00720066;
    PC3_dphi_mean[1][0][5][1] = -1.77917e-05;
    PC3_dphi_mean[1][0][5][2] = 0.0325675;
    PC3_dphi_mean[1][0][5][3] = -0.0291466;
    PC3_dphi_mean[1][0][5][4] = -0.0167985;
    PC3_dphi_mean[1][0][5][5] = 0.00635372;
    PC3_dphi_mean[1][0][5][6] = -0.00102481;
    PC3_dz_sigma[1][0][5][0] = -26.9718;
    PC3_dz_sigma[1][0][5][1] = 19.5915;
    PC3_dz_sigma[1][0][5][2] = -10.0694;
    PC3_dz_sigma[1][0][5][3] = 3.00106;
    PC3_dz_sigma[1][0][5][4] = -0.466137;
    PC3_dz_sigma[1][0][5][5] = 0.0290674;
    PC3_dz_sigma[1][0][5][6] = 17.365;
    PC3_dz_sigma[1][0][5][7] = -0.662505;
    PC3_dz_mean[1][0][5][0] = 1.71569;
    PC3_dz_mean[1][0][5][1] = 0.0832021;
    PC3_dz_mean[1][0][5][2] = -0.209747;
    PC3_dz_mean[1][0][5][3] = 2.01389;
    PC3_dz_mean[1][0][5][4] = -0.896782;
    PC3_dz_mean[1][0][5][5] = 0.551846;
    PC3_dz_mean[1][0][5][6] = -0.10341;
    PC3_dphi_sigma[1][0][6][0] = -0.0165785;
    PC3_dphi_sigma[1][0][6][1] = 0.0134656;
    PC3_dphi_sigma[1][0][6][2] = -0.00728516;
    PC3_dphi_sigma[1][0][6][3] = 0.00226635;
    PC3_dphi_sigma[1][0][6][4] = -0.000362513;
    PC3_dphi_sigma[1][0][6][5] = 2.32468e-05;
    PC3_dphi_sigma[1][0][6][6] = 0.0109247;
    PC3_dphi_sigma[1][0][6][7] = 0.000181968;
    PC3_dphi_mean[1][0][6][0] = 0.0101461;
    PC3_dphi_mean[1][0][6][1] = -0.000189681;
    PC3_dphi_mean[1][0][6][2] = 0.038429;
    PC3_dphi_mean[1][0][6][3] = -0.0363885;
    PC3_dphi_mean[1][0][6][4] = -0.01854;
    PC3_dphi_mean[1][0][6][5] = 0.00676658;
    PC3_dphi_mean[1][0][6][6] = -0.0010625;
    PC3_dz_sigma[1][0][6][0] = -25.0669;
    PC3_dz_sigma[1][0][6][1] = 18.1746;
    PC3_dz_sigma[1][0][6][2] = -9.31753;
    PC3_dz_sigma[1][0][6][3] = 2.78466;
    PC3_dz_sigma[1][0][6][4] = -0.435058;
    PC3_dz_sigma[1][0][6][5] = 0.0272712;
    PC3_dz_sigma[1][0][6][6] = 16.4878;
    PC3_dz_sigma[1][0][6][7] = -0.649568;
    PC3_dz_mean[1][0][6][0] = 2.91662;
    PC3_dz_mean[1][0][6][1] = -0.0254806;
    PC3_dz_mean[1][0][6][2] = -0.413893;
    PC3_dz_mean[1][0][6][3] = 1.23361;
    PC3_dz_mean[1][0][6][4] = -0.391152;
    PC3_dz_mean[1][0][6][5] = 0.3694;
    PC3_dz_mean[1][0][6][6] = -0.0808414;
    PC3_dphi_sigma[1][0][7][0] = -0.018342;
    PC3_dphi_sigma[1][0][7][1] = 0.0143464;
    PC3_dphi_sigma[1][0][7][2] = -0.0073128;
    PC3_dphi_sigma[1][0][7][3] = 0.00212357;
    PC3_dphi_sigma[1][0][7][4] = -0.000313082;
    PC3_dphi_sigma[1][0][7][5] = 1.8117e-05;
    PC3_dphi_sigma[1][0][7][6] = 0.0118709;
    PC3_dphi_sigma[1][0][7][7] = 0.0002828;
    PC3_dphi_mean[1][0][7][0] = 0.030558;
    PC3_dphi_mean[1][0][7][1] = -0.00122806;
    PC3_dphi_mean[1][0][7][2] = 0.0859472;
    PC3_dphi_mean[1][0][7][3] = -0.0907528;
    PC3_dphi_mean[1][0][7][4] = -0.0350406;
    PC3_dphi_mean[1][0][7][5] = 0.0113536;
    PC3_dphi_mean[1][0][7][6] = -0.00158723;
    PC3_dz_sigma[1][0][7][0] = -25.7214;
    PC3_dz_sigma[1][0][7][1] = 18.4803;
    PC3_dz_sigma[1][0][7][2] = -9.238;
    PC3_dz_sigma[1][0][7][3] = 2.65308;
    PC3_dz_sigma[1][0][7][4] = -0.394811;
    PC3_dz_sigma[1][0][7][5] = 0.0234126;
    PC3_dz_sigma[1][0][7][6] = 17.1883;
    PC3_dz_sigma[1][0][7][7] = -0.690587;
    PC3_dz_mean[1][0][7][0] = 2.77578;
    PC3_dz_mean[1][0][7][1] = -0.0609228;
    PC3_dz_mean[1][0][7][2] = -0.551844;
    PC3_dz_mean[1][0][7][3] = 1.31857;
    PC3_dz_mean[1][0][7][4] = -0.462275;
    PC3_dz_mean[1][0][7][5] = 0.510748;
    PC3_dz_mean[1][0][7][6] = -0.115392;
    PC3_dphi_sigma[1][1][0][0] = 0.049692;
    PC3_dphi_sigma[1][1][0][1] = -0.0295225;
    PC3_dphi_sigma[1][1][0][2] = 0.0131058;
    PC3_dphi_sigma[1][1][0][3] = -0.00335696;
    PC3_dphi_sigma[1][1][0][4] = 0.000452944;
    PC3_dphi_sigma[1][1][0][5] = -2.49989e-05;
    PC3_dphi_sigma[1][1][0][6] = -0.0302428;
    PC3_dphi_sigma[1][1][0][7] = 0.00246772;
    PC3_dphi_mean[1][1][0][0] = -0.0415117;
    PC3_dphi_mean[1][1][0][1] = 0.00168202;
    PC3_dphi_mean[1][1][0][2] = -0.115741;
    PC3_dphi_mean[1][1][0][3] = 0.118942;
    PC3_dphi_mean[1][1][0][4] = 0.0497588;
    PC3_dphi_mean[1][1][0][5] = -0.0166812;
    PC3_dphi_mean[1][1][0][6] = 0.00240343;
    PC3_dz_sigma[1][1][0][0] = 2.94676;
    PC3_dz_sigma[1][1][0][1] = -0.528199;
    PC3_dz_sigma[1][1][0][2] = -0.18771;
    PC3_dz_sigma[1][1][0][3] = 0.201702;
    PC3_dz_sigma[1][1][0][4] = -0.0521754;
    PC3_dz_sigma[1][1][0][5] = 0.0044129;
    PC3_dz_sigma[1][1][0][6] = -0.732384;
    PC3_dz_sigma[1][1][0][7] = 0.182095;
    PC3_dz_mean[1][1][0][0] = -0.518907;
    PC3_dz_mean[1][1][0][1] = 0.220722;
    PC3_dz_mean[1][1][0][2] = -0.243704;
    PC3_dz_mean[1][1][0][3] = 3.63012;
    PC3_dz_mean[1][1][0][4] = -1.92097;
    PC3_dz_mean[1][1][0][5] = 0.984668;
    PC3_dz_mean[1][1][0][6] = -0.175172;
    PC3_dphi_sigma[1][1][1][0] = 0.0587141;
    PC3_dphi_sigma[1][1][1][1] = -0.0385298;
    PC3_dphi_sigma[1][1][1][2] = 0.0187919;
    PC3_dphi_sigma[1][1][1][3] = -0.00531176;
    PC3_dphi_sigma[1][1][1][4] = 0.000789565;
    PC3_dphi_sigma[1][1][1][5] = -4.75313e-05;
    PC3_dphi_sigma[1][1][1][6] = -0.0341016;
    PC3_dphi_sigma[1][1][1][7] = 0.00225262;
    PC3_dphi_mean[1][1][1][0] = -0.0439579;
    PC3_dphi_mean[1][1][1][1] = 0.00178831;
    PC3_dphi_mean[1][1][1][2] = -0.121216;
    PC3_dphi_mean[1][1][1][3] = 0.125453;
    PC3_dphi_mean[1][1][1][4] = 0.0516016;
    PC3_dphi_mean[1][1][1][5] = -0.017315;
    PC3_dphi_mean[1][1][1][6] = 0.0025094;
    PC3_dz_sigma[1][1][1][0] = 0.130158;
    PC3_dz_sigma[1][1][1][1] = 0.243888;
    PC3_dz_sigma[1][1][1][2] = 0.0637469;
    PC3_dz_sigma[1][1][1][3] = -0.0814449;
    PC3_dz_sigma[1][1][1][4] = 0.0244002;
    PC3_dz_sigma[1][1][1][5] = -0.00229212;
    PC3_dz_sigma[1][1][1][6] = 1.13129;
    PC3_dz_sigma[1][1][1][7] = 0.0612811;
    PC3_dz_mean[1][1][1][0] = -7.30322;
    PC3_dz_mean[1][1][1][1] = 0.486939;
    PC3_dz_mean[1][1][1][2] = -20.0626;
    PC3_dz_mean[1][1][1][3] = 24.1183;
    PC3_dz_mean[1][1][1][4] = 6.33652;
    PC3_dz_mean[1][1][1][5] = -1.70763;
    PC3_dz_mean[1][1][1][6] = 0.204356;
    PC3_dphi_sigma[1][1][2][0] = 0.0532147;
    PC3_dphi_sigma[1][1][2][1] = -0.0350281;
    PC3_dphi_sigma[1][1][2][2] = 0.0170217;
    PC3_dphi_sigma[1][1][2][3] = -0.00476616;
    PC3_dphi_sigma[1][1][2][4] = 0.00069889;
    PC3_dphi_sigma[1][1][2][5] = -4.13026e-05;
    PC3_dphi_sigma[1][1][2][6] = -0.0305257;
    PC3_dphi_sigma[1][1][2][7] = 0.00198469;
    PC3_dphi_mean[1][1][2][0] = -0.038757;
    PC3_dphi_mean[1][1][2][1] = 0.00146654;
    PC3_dphi_mean[1][1][2][2] = -0.110401;
    PC3_dphi_mean[1][1][2][3] = 0.11251;
    PC3_dphi_mean[1][1][2][4] = 0.0479936;
    PC3_dphi_mean[1][1][2][5] = -0.0163208;
    PC3_dphi_mean[1][1][2][6] = 0.00238416;
    PC3_dz_sigma[1][1][2][0] = -0.91086;
    PC3_dz_sigma[1][1][2][1] = 1.38993;
    PC3_dz_sigma[1][1][2][2] = -0.763815;
    PC3_dz_sigma[1][1][2][3] = 0.239345;
    PC3_dz_sigma[1][1][2][4] = -0.037244;
    PC3_dz_sigma[1][1][2][5] = 0.00223847;
    PC3_dz_sigma[1][1][2][6] = 1.61437;
    PC3_dz_sigma[1][1][2][7] = 0.0644442;
    PC3_dz_mean[1][1][2][0] = -9.56649;
    PC3_dz_mean[1][1][2][1] = 0.590385;
    PC3_dz_mean[1][1][2][2] = -27.3093;
    PC3_dz_mean[1][1][2][3] = 31.7914;
    PC3_dz_mean[1][1][2][4] = 9.23056;
    PC3_dz_mean[1][1][2][5] = -2.59916;
    PC3_dz_mean[1][1][2][6] = 0.322752;
    PC3_dphi_sigma[1][1][3][0] = 0.0538853;
    PC3_dphi_sigma[1][1][3][1] = -0.0360989;
    PC3_dphi_sigma[1][1][3][2] = 0.0178661;
    PC3_dphi_sigma[1][1][3][3] = -0.00509895;
    PC3_dphi_sigma[1][1][3][4] = 0.000761726;
    PC3_dphi_sigma[1][1][3][5] = -4.58214e-05;
    PC3_dphi_sigma[1][1][3][6] = -0.0306443;
    PC3_dphi_sigma[1][1][3][7] = 0.00193499;
    PC3_dphi_mean[1][1][3][0] = -0.0357065;
    PC3_dphi_mean[1][1][3][1] = 0.00132976;
    PC3_dphi_mean[1][1][3][2] = -0.102631;
    PC3_dphi_mean[1][1][3][3] = 0.104004;
    PC3_dphi_mean[1][1][3][4] = 0.0450578;
    PC3_dphi_mean[1][1][3][5] = -0.0154317;
    PC3_dphi_mean[1][1][3][6] = 0.00226166;
    PC3_dz_sigma[1][1][3][0] = 1.00564;
    PC3_dz_sigma[1][1][3][1] = 0.0950735;
    PC3_dz_sigma[1][1][3][2] = -0.1088;
    PC3_dz_sigma[1][1][3][3] = 0.0570087;
    PC3_dz_sigma[1][1][3][4] = -0.0129559;
    PC3_dz_sigma[1][1][3][5] = 0.00109663;
    PC3_dz_sigma[1][1][3][6] = 0.537911;
    PC3_dz_sigma[1][1][3][7] = 0.12367;
    PC3_dz_mean[1][1][3][0] = -1.99545;
    PC3_dz_mean[1][1][3][1] = 0.208878;
    PC3_dz_mean[1][1][3][2] = -10.4819;
    PC3_dz_mean[1][1][3][3] = 12.4097;
    PC3_dz_mean[1][1][3][4] = 3.40664;
    PC3_dz_mean[1][1][3][5] = -0.918484;
    PC3_dz_mean[1][1][3][6] = 0.109359;
    PC3_dphi_sigma[1][1][4][0] = 0.0481732;
    PC3_dphi_sigma[1][1][4][1] = -0.0317743;
    PC3_dphi_sigma[1][1][4][2] = 0.0154322;
    PC3_dphi_sigma[1][1][4][3] = -0.00430231;
    PC3_dphi_sigma[1][1][4][4] = 0.000626254;
    PC3_dphi_sigma[1][1][4][5] = -3.66301e-05;
    PC3_dphi_sigma[1][1][4][6] = -0.0273392;
    PC3_dphi_sigma[1][1][4][7] = 0.00178676;
    PC3_dphi_mean[1][1][4][0] = -0.0379166;
    PC3_dphi_mean[1][1][4][1] = 0.00142376;
    PC3_dphi_mean[1][1][4][2] = -0.108592;
    PC3_dphi_mean[1][1][4][3] = 0.110402;
    PC3_dphi_mean[1][1][4][4] = 0.0473877;
    PC3_dphi_mean[1][1][4][5] = -0.0161749;
    PC3_dphi_mean[1][1][4][6] = 0.00236123;
    PC3_dz_sigma[1][1][4][0] = 0.543536;
    PC3_dz_sigma[1][1][4][1] = 0.19625;
    PC3_dz_sigma[1][1][4][2] = -0.00252828;
    PC3_dz_sigma[1][1][4][3] = -0.0325699;
    PC3_dz_sigma[1][1][4][4] = 0.0113864;
    PC3_dz_sigma[1][1][4][5] = -0.00110412;
    PC3_dz_sigma[1][1][4][6] = 0.940493;
    PC3_dz_sigma[1][1][4][7] = 0.0978841;
    PC3_dz_mean[1][1][4][0] = -5.18126;
    PC3_dz_mean[1][1][4][1] = 0.379103;
    PC3_dz_mean[1][1][4][2] = -18.8024;
    PC3_dz_mean[1][1][4][3] = 21.5434;
    PC3_dz_mean[1][1][4][4] = 6.68819;
    PC3_dz_mean[1][1][4][5] = -1.97452;
    PC3_dz_mean[1][1][4][6] = 0.259641;
    PC3_dphi_sigma[1][1][5][0] = 0.0537581;
    PC3_dphi_sigma[1][1][5][1] = -0.0360734;
    PC3_dphi_sigma[1][1][5][2] = 0.0179584;
    PC3_dphi_sigma[1][1][5][3] = -0.00517776;
    PC3_dphi_sigma[1][1][5][4] = 0.000784925;
    PC3_dphi_sigma[1][1][5][5] = -4.80934e-05;
    PC3_dphi_sigma[1][1][5][6] = -0.0305832;
    PC3_dphi_sigma[1][1][5][7] = 0.00195475;
    PC3_dphi_mean[1][1][5][0] = -0.0398352;
    PC3_dphi_mean[1][1][5][1] = 0.00151749;
    PC3_dphi_mean[1][1][5][2] = -0.113304;
    PC3_dphi_mean[1][1][5][3] = 0.115682;
    PC3_dphi_mean[1][1][5][4] = 0.0491209;
    PC3_dphi_mean[1][1][5][5] = -0.016702;
    PC3_dphi_mean[1][1][5][6] = 0.00242791;
    PC3_dz_sigma[1][1][5][0] = -4.22595;
    PC3_dz_sigma[1][1][5][1] = 4.07536;
    PC3_dz_sigma[1][1][5][2] = -2.27541;
    PC3_dz_sigma[1][1][5][3] = 0.725102;
    PC3_dz_sigma[1][1][5][4] = -0.117857;
    PC3_dz_sigma[1][1][5][5] = 0.0075744;
    PC3_dz_sigma[1][1][5][6] = 3.58868;
    PC3_dz_sigma[1][1][5][7] = -0.000466533;
    PC3_dz_mean[1][1][5][0] = -1.76939;
    PC3_dz_mean[1][1][5][1] = 0.226572;
    PC3_dz_mean[1][1][5][2] = -10.469;
    PC3_dz_mean[1][1][5][3] = 12.47;
    PC3_dz_mean[1][1][5][4] = 3.4742;
    PC3_dz_mean[1][1][5][5] = -0.945455;
    PC3_dz_mean[1][1][5][6] = 0.117968;
    PC3_dphi_sigma[1][1][6][0] = 0.0472507;
    PC3_dphi_sigma[1][1][6][1] = -0.0306162;
    PC3_dphi_sigma[1][1][6][2] = 0.0147383;
    PC3_dphi_sigma[1][1][6][3] = -0.0041017;
    PC3_dphi_sigma[1][1][6][4] = 0.000600604;
    PC3_dphi_sigma[1][1][6][5] = -3.56181e-05;
    PC3_dphi_sigma[1][1][6][6] = -0.0271414;
    PC3_dphi_sigma[1][1][6][7] = 0.0018921;
    PC3_dphi_mean[1][1][6][0] = -0.0432943;
    PC3_dphi_mean[1][1][6][1] = 0.00171827;
    PC3_dphi_mean[1][1][6][2] = -0.120767;
    PC3_dphi_mean[1][1][6][3] = 0.124559;
    PC3_dphi_mean[1][1][6][4] = 0.05156;
    PC3_dphi_mean[1][1][6][5] = -0.0173469;
    PC3_dphi_mean[1][1][6][6] = 0.00249421;
    PC3_dz_sigma[1][1][6][0] = -2.75338;
    PC3_dz_sigma[1][1][6][1] = 2.89675;
    PC3_dz_sigma[1][1][6][2] = -1.50469;
    PC3_dz_sigma[1][1][6][3] = 0.436404;
    PC3_dz_sigma[1][1][6][4] = -0.0627274;
    PC3_dz_sigma[1][1][6][5] = 0.00342448;
    PC3_dz_sigma[1][1][6][6] = 2.93606;
    PC3_dz_sigma[1][1][6][7] = 0.00901412;
    PC3_dz_mean[1][1][6][0] = 4.48295;
    PC3_dz_mean[1][1][6][1] = -0.165481;
    PC3_dz_mean[1][1][6][2] = -0.377241;
    PC3_dz_mean[1][1][6][3] = -0.974934;
    PC3_dz_mean[1][1][6][4] = 0.967761;
    PC3_dz_mean[1][1][6][5] = -0.452587;
    PC3_dz_mean[1][1][6][6] = 0.0796333;
    PC3_dphi_sigma[1][1][7][0] = 0.0303642;
    PC3_dphi_sigma[1][1][7][1] = -0.0182489;
    PC3_dphi_sigma[1][1][7][2] = 0.00819609;
    PC3_dphi_sigma[1][1][7][3] = -0.00209992;
    PC3_dphi_sigma[1][1][7][4] = 0.000283606;
    PC3_dphi_sigma[1][1][7][5] = -1.58053e-05;
    PC3_dphi_sigma[1][1][7][6] = -0.0174495;
    PC3_dphi_sigma[1][1][7][7] = 0.00159909;
    PC3_dphi_mean[1][1][7][0] = -0.042947;
    PC3_dphi_mean[1][1][7][1] = 0.00182399;
    PC3_dphi_mean[1][1][7][2] = -0.115839;
    PC3_dphi_mean[1][1][7][3] = 0.121174;
    PC3_dphi_mean[1][1][7][4] = 0.0484518;
    PC3_dphi_mean[1][1][7][5] = -0.0160099;
    PC3_dphi_mean[1][1][7][6] = 0.00223769;
    PC3_dz_sigma[1][1][7][0] = 6.99904;
    PC3_dz_sigma[1][1][7][1] = -5.11593;
    PC3_dz_sigma[1][1][7][2] = 3.36788;
    PC3_dz_sigma[1][1][7][3] = -1.23336;
    PC3_dz_sigma[1][1][7][4] = 0.223029;
    PC3_dz_sigma[1][1][7][5] = -0.0154383;
    PC3_dz_sigma[1][1][7][6] = -2.17334;
    PC3_dz_sigma[1][1][7][7] = 0.197768;
    PC3_dz_mean[1][1][7][0] = 3.73318;
    PC3_dz_mean[1][1][7][1] = -0.124467;
    PC3_dz_mean[1][1][7][2] = -0.414415;
    PC3_dz_mean[1][1][7][3] = -0.193828;
    PC3_dz_mean[1][1][7][4] = 0.514255;
    PC3_dz_mean[1][1][7][5] = -0.162635;
    PC3_dz_mean[1][1][7][6] = 0.0255038;

PC2_dphifirst_mean[0][0][0] = -0.099;
PC2_dphifirst_sigma[0][0][0] = 0;
PC2_dzfirst_mean[0][0][0] = -9.8;
PC2_dzfirst_sigma[0][0][0] = 0;
PC2_dphifirst_mean[0][1][0] = -0.099;
PC2_dphifirst_sigma[0][1][0] = 0;
PC2_dzfirst_mean[0][1][0] = -9.8;
PC2_dzfirst_sigma[0][1][0] = 0;
PC2_dphifirst_mean[1][0][0] = -0.00475372;
PC2_dphifirst_sigma[1][0][0] = 0.00679902;
PC2_dzfirst_mean[1][0][0] = 0.642809;
PC2_dzfirst_sigma[1][0][0] = 3.09127;
PC2_dphifirst_mean[1][1][0] = 0.00206371;
PC2_dphifirst_sigma[1][1][0] = 0.00661069;
PC2_dzfirst_mean[1][1][0] = 0.478135;
PC2_dzfirst_sigma[1][1][0] = 3.21144;
PC2_dphi_sigma[0][0][0][0] = 0;
PC2_dphi_sigma[0][0][0][1] = 0;
PC2_dphi_sigma[0][0][0][2] = 0;
PC2_dphi_sigma[0][0][0][3] = 0;
PC2_dphi_sigma[0][0][0][4] = 0;
PC2_dphi_sigma[0][0][0][5] = 0;
PC2_dphi_sigma[0][0][0][6] = 0;
PC2_dphi_sigma[0][0][0][7] = 0;
PC2_dphi_mean[0][0][0][0] = -0.0779735;
PC2_dphi_mean[0][0][0][1] = -0.00136205;
PC2_dphi_mean[0][0][0][2] = 0.0216856;
PC2_dphi_mean[0][0][0][3] = -0.0426029;
PC2_dphi_mean[0][0][0][4] = 0.0072229;
PC2_dphi_mean[0][0][0][5] = -0.00768278;
PC2_dphi_mean[0][0][0][6] = 0.00179557;
PC2_dz_sigma[0][0][0][0] = 0;
PC2_dz_sigma[0][0][0][1] = 0;
PC2_dz_sigma[0][0][0][2] = 0;
PC2_dz_sigma[0][0][0][3] = 0;
PC2_dz_sigma[0][0][0][4] = 0;
PC2_dz_sigma[0][0][0][5] = 0;
PC2_dz_sigma[0][0][0][6] = 0;
PC2_dz_sigma[0][0][0][7] = 0;
PC2_dz_mean[0][0][0][0] = -9.0597;
PC2_dz_mean[0][0][0][1] = -0.0288012;
PC2_dz_mean[0][0][0][2] = 2.21001;
PC2_dz_mean[0][0][0][3] = -2.24447;
PC2_dz_mean[0][0][0][4] = -0.956304;
PC2_dz_mean[0][0][0][5] = 0.327325;
PC2_dz_mean[0][0][0][6] = -0.047667;
PC2_dphi_sigma[0][1][0][0] = 0;
PC2_dphi_sigma[0][1][0][1] = 0;
PC2_dphi_sigma[0][1][0][2] = 0;
PC2_dphi_sigma[0][1][0][3] = 0;
PC2_dphi_sigma[0][1][0][4] = 0;
PC2_dphi_sigma[0][1][0][5] = 0;
PC2_dphi_sigma[0][1][0][6] = 0;
PC2_dphi_sigma[0][1][0][7] = 0;
PC2_dphi_mean[0][1][0][0] = -0.0779735;
PC2_dphi_mean[0][1][0][1] = -0.00136205;
PC2_dphi_mean[0][1][0][2] = 0.0216856;
PC2_dphi_mean[0][1][0][3] = -0.0426029;
PC2_dphi_mean[0][1][0][4] = 0.0072229;
PC2_dphi_mean[0][1][0][5] = -0.00768278;
PC2_dphi_mean[0][1][0][6] = 0.00179557;
PC2_dz_sigma[0][1][0][0] = 0;
PC2_dz_sigma[0][1][0][1] = 0;
PC2_dz_sigma[0][1][0][2] = 0;
PC2_dz_sigma[0][1][0][3] = 0;
PC2_dz_sigma[0][1][0][4] = 0;
PC2_dz_sigma[0][1][0][5] = 0;
PC2_dz_sigma[0][1][0][6] = 0;
PC2_dz_sigma[0][1][0][7] = 0;
PC2_dz_mean[0][1][0][0] = -9.0597;
PC2_dz_mean[0][1][0][1] = -0.0288012;
PC2_dz_mean[0][1][0][2] = 2.21001;
PC2_dz_mean[0][1][0][3] = -2.24447;
PC2_dz_mean[0][1][0][4] = -0.956304;
PC2_dz_mean[0][1][0][5] = 0.327325;
PC2_dz_mean[0][1][0][6] = -0.047667;
PC2_dphi_sigma[1][0][0][0] = 0.00442039;
PC2_dphi_sigma[1][0][0][1] = 5.80048e-05;
PC2_dphi_sigma[1][0][0][2] = -0.000962551;
PC2_dphi_sigma[1][0][0][3] = 0.000527179;
PC2_dphi_sigma[1][0][0][4] = -0.000112759;
PC2_dphi_sigma[1][0][0][5] = 8.84649e-06;
PC2_dphi_sigma[1][0][0][6] = -0.0025592;
PC2_dphi_sigma[1][0][0][7] = 0.000748675;
PC2_dphi_mean[1][0][0][0] = -0.00452125;
PC2_dphi_mean[1][0][0][1] = 0.000415364;
PC2_dphi_mean[1][0][0][2] = -0.000165822;
PC2_dphi_mean[1][0][0][3] = 0.00469644;
PC2_dphi_mean[1][0][0][4] = -0.00315717;
PC2_dphi_mean[1][0][0][5] = 0.00187137;
PC2_dphi_mean[1][0][0][6] = -0.000405371;
PC2_dz_sigma[1][0][0][0] = -14.8591;
PC2_dz_sigma[1][0][0][1] = 13.7552;
PC2_dz_sigma[1][0][0][2] = -8.37532;
PC2_dz_sigma[1][0][0][3] = 2.86491;
PC2_dz_sigma[1][0][0][4] = -0.501026;
PC2_dz_sigma[1][0][0][5] = 0.0346183;
PC2_dz_sigma[1][0][0][6] = 8.41788;
PC2_dz_sigma[1][0][0][7] = -0.154697;
PC2_dz_mean[1][0][0][0] = -15.2052;
PC2_dz_mean[1][0][0][1] = 0.860754;
PC2_dz_mean[1][0][0][2] = -35.5492;
PC2_dz_mean[1][0][0][3] = 41.9627;
PC2_dz_mean[1][0][0][4] = 11.6404;
PC2_dz_mean[1][0][0][5] = -3.10039;
PC2_dz_mean[1][0][0][6] = 0.360379;
PC2_dphi_sigma[1][1][0][0] = 0.0455061;
PC2_dphi_sigma[1][1][0][1] = -0.0292228;
PC2_dphi_sigma[1][1][0][2] = 0.0142715;
PC2_dphi_sigma[1][1][0][3] = -0.00408311;
PC2_dphi_sigma[1][1][0][4] = 0.000618052;
PC2_dphi_sigma[1][1][0][5] = -3.79799e-05;
PC2_dphi_sigma[1][1][0][6] = -0.0268056;
PC2_dphi_sigma[1][1][0][7] = 0.0018308;
PC2_dphi_mean[1][1][0][0] = -0.0327724;
PC2_dphi_mean[1][1][0][1] = 0.0012608;
PC2_dphi_mean[1][1][0][2] = -0.0896841;
PC2_dphi_mean[1][1][0][3] = 0.092243;
PC2_dphi_mean[1][1][0][4] = 0.0386292;
PC2_dphi_mean[1][1][0][5] = -0.0131506;
PC2_dphi_mean[1][1][0][6] = 0.00194344;
PC2_dz_sigma[1][1][0][0] = 6.47554;
PC2_dz_sigma[1][1][0][1] = -3.50533;
PC2_dz_sigma[1][1][0][2] = 1.65705;
PC2_dz_sigma[1][1][0][3] = -0.449502;
PC2_dz_sigma[1][1][0][4] = 0.0619762;
PC2_dz_sigma[1][1][0][5] = -0.00332307;
PC2_dz_sigma[1][1][0][6] = -3.34724;
PC2_dz_sigma[1][1][0][7] = 0.285792;
PC2_dz_mean[1][1][0][0] = -15.4837;
PC2_dz_mean[1][1][0][1] = 0.808899;
PC2_dz_mean[1][1][0][2] = -38.1642;
PC2_dz_mean[1][1][0][3] = 43.9536;
PC2_dz_mean[1][1][0][4] = 12.999;
PC2_dz_mean[1][1][0][5] = -3.58677;
PC2_dz_mean[1][1][0][6] = 0.431277;
  return;
}
