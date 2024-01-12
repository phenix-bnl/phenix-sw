#include "MatchrecalRecoRun14AuAu15GeV.h"
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

//Run15pAl PC3 matching recalibrator, adapted from A. Adare's MiniRecal
//Qiao Xu

using namespace std;
using namespace findNode;


MatchrecalRecoRun14AuAu15GeV::MatchrecalRecoRun14AuAu15GeV(const string &name): 
  Recalibrator(name)
{
  baseclasses.insert("PHCentralTrack");
  initParameters();
}

int MatchrecalRecoRun14AuAu15GeV::isValidRun(const int runno) const
{
   if (runno >= 402024 && runno <= 405182)
   {
      return 1;
   }
   return 0;
}

void MatchrecalRecoRun14AuAu15GeV::help() const
{
  cout << "===================================================================" << endl;
  cout << "MatchrecalRecoRun14AuAu15GeV::help method output"                    << endl;
  cout << "Qiao Xu(qiao.xu@vanderbilt.edu) " << endl;
  cout << "PC3/PC2 matching for Run-14 Au+Au " << endl;
  cout << "TOF matching will be added" << endl;
  cout << "Run14 Au-Au 15 GeV with minimum bias triggers" << endl;
  cout << "Central arm tracks with trasverse momentum < 0.2 GeV/c are ignored" << endl;
  cout << "The matching is done in dphi and dz directions, fit with double-Gaussian distribution" << endl;
  cout << endl;
  cout << "===================================================================" << endl;
}

int MatchrecalRecoRun14AuAu15GeV::process_event(PHCompositeNode *topNode)
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
       
float MatchrecalRecoRun14AuAu15GeV::GetPC3sdphi(const int iarm, const int icharge, const float pt, const float pc3dphi)
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
    mean = PC3_dphifirst_mean[iarm][jcharge][0];
    sigma = PC3_dphifirst_sigma[iarm][jcharge][0];
  }
  else{
  mean = PC3_dphi_mean[iarm][jcharge][0]+PC3_dphi_mean[iarm][jcharge][1]*pt+PC3_dphi_mean[iarm][jcharge][2]/pt+PC3_dphi_mean[iarm][jcharge][3]/sqrt(pt)+PC3_dphi_mean[iarm][jcharge][4]/pt/pt+PC3_dphi_mean[iarm][jcharge][5]/pt/pt/pt+PC3_dphi_mean[iarm][jcharge][6]/pt/pt/pt/pt;
  sigma = PC3_dphi_sigma[iarm][jcharge][0]+PC3_dphi_sigma[iarm][jcharge][1]*pt+PC3_dphi_sigma[iarm][jcharge][2]*pt*pt+PC3_dphi_sigma[iarm][jcharge][3]*pt*pt*pt+PC3_dphi_sigma[iarm][jcharge][4]*pt*pt*pt*pt+PC3_dphi_sigma[iarm][jcharge][5]*pt*pt*pt*pt*pt+PC3_dphi_sigma[iarm][jcharge][6]/sqrt(pt)+PC3_dphi_sigma[iarm][jcharge][7]/pt/pt;
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


float MatchrecalRecoRun14AuAu15GeV::GetPC3sdz(const int iarm, const int icharge, const float pt, const float pc3dz)
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
    mean = PC3_dzfirst_mean[iarm][jcharge][0];
    sigma = PC3_dzfirst_sigma[iarm][jcharge][0];
  }
  else{
    if(pt > 3){  float pts = 3.;
  mean = PC3_dz_mean[iarm][jcharge][0]+PC3_dz_mean[iarm][jcharge][1]*pts+PC3_dz_mean[iarm][jcharge][2]/pts+PC3_dz_mean[iarm][jcharge][3]/sqrt(pts)+PC3_dz_mean[iarm][jcharge][4]/pts/pts+PC3_dz_mean[iarm][jcharge][5]/pts/pts/pts+PC3_dz_mean[iarm][jcharge][6]/pts/pts/pts/pts;
  sigma = PC3_dz_sigma[iarm][jcharge][0]+PC3_dz_sigma[iarm][jcharge][1]*pts+PC3_dz_sigma[iarm][jcharge][2]*pts*pts+PC3_dz_sigma[iarm][jcharge][3]*pts*pts*pts+PC3_dz_sigma[iarm][jcharge][4]*pts*pts*pts*pts+PC3_dz_sigma[iarm][jcharge][5]*pts*pts*pts*pts*pts+PC3_dz_sigma[iarm][jcharge][6]/sqrt(pts)+PC3_dz_sigma[iarm][jcharge][7]/pts/pts;
  }
  }
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

float MatchrecalRecoRun14AuAu15GeV::GetPC2sdphi(const int iarm, const int icharge, const float pt, const float pc2dphi)
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
    mean = PC3_dphifirst_mean[iarm][jcharge][0];
    sigma = PC3_dphifirst_sigma[iarm][jcharge][0];
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


float MatchrecalRecoRun14AuAu15GeV::GetPC2sdz(const int iarm, const int icharge, const float pt, const float pc2dz)
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
    if(pt > 3){  float pts = 3.;
  mean = PC2_dz_mean[iarm][jcharge][0]+PC2_dz_mean[iarm][jcharge][1]*pts+PC2_dz_mean[iarm][jcharge][2]/pts+PC2_dz_mean[iarm][jcharge][3]/sqrt(pts)+PC2_dz_mean[iarm][jcharge][4]/pts/pts+PC2_dz_mean[iarm][jcharge][5]/pts/pts/pts+PC2_dz_mean[iarm][jcharge][6]/pts/pts/pts/pts;
  sigma = PC2_dz_sigma[iarm][jcharge][0]+PC2_dz_sigma[iarm][jcharge][1]*pts+PC2_dz_sigma[iarm][jcharge][2]*pts*pts+PC2_dz_sigma[iarm][jcharge][3]*pts*pts*pts+PC2_dz_sigma[iarm][jcharge][4]*pts*pts*pts*pts+PC2_dz_sigma[iarm][jcharge][5]*pts*pts*pts*pts*pts+PC2_dz_sigma[iarm][jcharge][6]/sqrt(pts)+PC2_dz_sigma[iarm][jcharge][7]/pts/pts;
  }
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

void MatchrecalRecoRun14AuAu15GeV::initParameters()
{
  //
  // Initialization
  //
PC3_dphifirst_mean[0][0][0] = -0.00617281;
PC3_dphifirst_sigma[0][0][0] = 0.0123106;
PC3_dzfirst_mean[0][0][0] = -0.822238;
PC3_dzfirst_sigma[0][0][0] = 3.51803;
PC3_dphifirst_mean[0][1][0] = 0.00761261;
PC3_dphifirst_sigma[0][1][0] = 0.0119309;
PC3_dzfirst_mean[0][1][0] = -0.633777;
PC3_dzfirst_sigma[0][1][0] = 3.27981;
PC3_dphifirst_mean[1][0][0] = -0.0076941;
PC3_dphifirst_sigma[1][0][0] = 0.0121479;
PC3_dzfirst_mean[1][0][0] = 0.852554;
PC3_dzfirst_sigma[1][0][0] = 3.39277;
PC3_dphifirst_mean[1][1][0] = 0.00564104;
PC3_dphifirst_sigma[1][1][0] = 0.0116336;
PC3_dzfirst_mean[1][1][0] = 1.05242;
PC3_dzfirst_sigma[1][1][0] = 3.57906;
PC3_dphi_sigma[0][0][0] = 0.0334391;
PC3_dphi_sigma[0][0][1] = -0.0264302;
PC3_dphi_sigma[0][0][2] = 0.0155598;
PC3_dphi_sigma[0][0][3] = -0.00532768;
PC3_dphi_sigma[0][0][4] = 0.00095798;
PC3_dphi_sigma[0][0][5] = -6.82105e-05;
PC3_dphi_sigma[0][0][6] = -0.016913;
PC3_dphi_sigma[0][0][7] = 0.00120772;
PC3_dphi_mean[0][0][0] = -0.00426423;
PC3_dphi_mean[0][0][1] = 0.000825598;
PC3_dphi_mean[0][0][2] = 0.00133354;
PC3_dphi_mean[0][0][3] = 0.00549796;
PC3_dphi_mean[0][0][4] = -0.00391044;
PC3_dphi_mean[0][0][5] = 0.00185687;
PC3_dphi_mean[0][0][6] = -0.000359483;
PC3_dz_sigma[0][0][0] = -11.6455;
PC3_dz_sigma[0][0][1] = 11.3305;
PC3_dz_sigma[0][0][2] = -7.91836;
PC3_dz_sigma[0][0][3] = 3.30615;
PC3_dz_sigma[0][0][4] = -0.718468;
PC3_dz_sigma[0][0][5] = 0.0619849;
PC3_dz_sigma[0][0][6] = 7.43162;
PC3_dz_sigma[0][0][7] = -0.1967;
PC3_dz_mean[0][0][0] = -0.384752;
PC3_dz_mean[0][0][1] = 0.03576;
PC3_dz_mean[0][0][2] = 0.0814048;
PC3_dz_mean[0][0][3] = -0.0945063;
PC3_dz_mean[0][0][4] = 0.0170256;
PC3_dz_mean[0][0][5] = -0.0403102;
PC3_dz_mean[0][0][6] = 0.00853465;
PC3_dphi_sigma[0][1][0] = 0.0452353;
PC3_dphi_sigma[0][1][1] = -0.0312602;
PC3_dphi_sigma[0][1][2] = 0.0176221;
PC3_dphi_sigma[0][1][3] = -0.00626137;
PC3_dphi_sigma[0][1][4] = 0.00121826;
PC3_dphi_sigma[0][1][5] = -9.26776e-05;
PC3_dphi_sigma[0][1][6] = -0.0260539;
PC3_dphi_sigma[0][1][7] = 0.00186453;
PC3_dphi_mean[0][1][0] = -0.000328825;
PC3_dphi_mean[0][1][1] = 7.29073e-05;
PC3_dphi_mean[0][1][2] = 0.000294457;
PC3_dphi_mean[0][1][3] = 0.00122059;
PC3_dphi_mean[0][1][4] = -0.000559892;
PC3_dphi_mean[0][1][5] = 0.000234373;
PC3_dphi_mean[0][1][6] = 1.32275e-05;
PC3_dz_sigma[0][1][0] = -2103.59;
PC3_dz_sigma[0][1][1] = 2039.78;
PC3_dz_sigma[0][1][2] = -1478.52;
PC3_dz_sigma[0][1][3] = 627.212;
PC3_dz_sigma[0][1][4] = -138.994;
PC3_dz_sigma[0][1][5] = 12.2779;
PC3_dz_sigma[0][1][6] = 1078.6;
PC3_dz_sigma[0][1][7] = -34.9133;
PC3_dz_mean[0][1][0] = -1.71173;
PC3_dz_mean[0][1][1] = 0.150644;
PC3_dz_mean[0][1][2] = 0.00568036;
PC3_dz_mean[0][1][3] = 1.76073;
PC3_dz_mean[0][1][4] = -1.00832;
PC3_dz_mean[0][1][5] = 0.539174;
PC3_dz_mean[0][1][6] = -0.0964446;
PC3_dphi_sigma[1][0][0] = 0.0100678;
PC3_dphi_sigma[1][0][1] = -0.00834319;
PC3_dphi_sigma[1][0][2] = 0.00566222;
PC3_dphi_sigma[1][0][3] = -0.00220237;
PC3_dphi_sigma[1][0][4] = 0.000440996;
PC3_dphi_sigma[1][0][5] = -3.37479e-05;
PC3_dphi_sigma[1][0][6] = -0.00355832;
PC3_dphi_sigma[1][0][7] = 0.000668495;
PC3_dphi_mean[1][0][0] = -0.0558711;
PC3_dphi_mean[1][0][1] = 0.00355877;
PC3_dphi_mean[1][0][2] = -0.110897;
PC3_dphi_mean[1][0][3] = 0.135989;
PC3_dphi_mean[1][0][4] = 0.0344039;
PC3_dphi_mean[1][0][5] = -0.0089612;
PC3_dphi_mean[1][0][6] = 0.000995006;
PC3_dz_sigma[1][0][0] = 1.65388;
PC3_dz_sigma[1][0][1] = -0.418082;
PC3_dz_sigma[1][0][2] = -0.26105;
PC3_dz_sigma[1][0][3] = 0.444954;
PC3_dz_sigma[1][0][4] = -0.188311;
PC3_dz_sigma[1][0][5] = 0.0262115;
PC3_dz_sigma[1][0][6] = 0.368249;
PC3_dz_sigma[1][0][7] = 0.0632009;
PC3_dz_mean[1][0][0] = 1.60814;
PC3_dz_mean[1][0][1] = -0.103445;
PC3_dz_mean[1][0][2] = -0.460182;
PC3_dz_mean[1][0][3] = -0.0604653;
PC3_dz_mean[1][0][4] = 0.205758;
PC3_dz_mean[1][0][5] = -0.016216;
PC3_dz_mean[1][0][6] = -0.00803018;
PC3_dphi_sigma[1][1][0] = 0.00813638;
PC3_dphi_sigma[1][1][1] = 0.00390665;
PC3_dphi_sigma[1][1][2] = -0.00617161;
PC3_dphi_sigma[1][1][3] = 0.00280101;
PC3_dphi_sigma[1][1][4] = -0.000525381;
PC3_dphi_sigma[1][1][5] = 3.77826e-05;
PC3_dphi_sigma[1][1][6] = -0.00677025;
PC3_dphi_sigma[1][1][7] = 0.00123843;
PC3_dphi_mean[1][1][0] = -0.0733123;
PC3_dphi_mean[1][1][1] = 0.0039779;
PC3_dphi_mean[1][1][2] = -0.161978;
PC3_dphi_mean[1][1][3] = 0.187782;
PC3_dphi_mean[1][1][4] = 0.0572774;
PC3_dphi_mean[1][1][5] = -0.0169618;
PC3_dphi_mean[1][1][6] = 0.00226607;
PC3_dz_sigma[1][1][0] = 45.9417;
PC3_dz_sigma[1][1][1] = -42.838;
PC3_dz_sigma[1][1][2] = 30.1342;
PC3_dz_sigma[1][1][3] = -12.1217;
PC3_dz_sigma[1][1][4] = 2.46369;
PC3_dz_sigma[1][1][5] = -0.188543;
PC3_dz_sigma[1][1][6] = -22.6673;
PC3_dz_sigma[1][1][7] = 0.867538;
PC3_dz_mean[1][1][0] = 3.3532;
PC3_dz_mean[1][1][1] = -0.276473;
PC3_dz_mean[1][1][2] = -0.348055;
PC3_dz_mean[1][1][3] = -2.41408;
PC3_dz_mean[1][1][4] = 1.56804;
PC3_dz_mean[1][1][5] = -0.791895;
PC3_dz_mean[1][1][6] = 0.135958;

PC2_dphifirst_mean[0][0][0] = -0.099;
PC2_dphifirst_sigma[0][0][0] = 0;
PC2_dzfirst_mean[0][0][0] = -9.8;
PC2_dzfirst_sigma[0][0][0] = 0;
PC2_dphifirst_mean[0][1][0] = -0.099;
PC2_dphifirst_sigma[0][1][0] = 0;
PC2_dzfirst_mean[0][1][0] = -9.8;
PC2_dzfirst_sigma[0][1][0] = 0;
PC2_dphifirst_mean[1][0][0] = -0.00711219;
PC2_dphifirst_sigma[1][0][0] = 0.00962052;
PC2_dzfirst_mean[1][0][0] = 0.747454;
PC2_dzfirst_sigma[1][0][0] = 2.497;
PC2_dphifirst_mean[1][1][0] = 0.003654;
PC2_dphifirst_sigma[1][1][0] = 0.00851316;
PC2_dzfirst_mean[1][1][0] = 0.826049;
PC2_dzfirst_sigma[1][1][0] = 2.72371;
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
PC2_dphi_sigma[1][0][0] = 0.0116437;
PC2_dphi_sigma[1][0][1] = -0.00699474;
PC2_dphi_sigma[1][0][2] = 0.0039636;
PC2_dphi_sigma[1][0][3] = -0.00144406;
PC2_dphi_sigma[1][0][4] = 0.000289573;
PC2_dphi_sigma[1][0][5] = -2.21972e-05;
PC2_dphi_sigma[1][0][6] = -0.0061074;
PC2_dphi_sigma[1][0][7] = 0.000836774;
PC2_dphi_mean[1][0][0] = -0.0341686;
PC2_dphi_mean[1][0][1] = 0.00232849;
PC2_dphi_mean[1][0][2] = -0.062825;
PC2_dphi_mean[1][0][3] = 0.0790907;
PC2_dphi_mean[1][0][4] = 0.0185871;
PC2_dphi_mean[1][0][5] = -0.00463445;
PC2_dphi_mean[1][0][6] = 0.000467277;
PC2_dz_sigma[1][0][0] = 4.77257;
PC2_dz_sigma[1][0][1] = -3.57057;
PC2_dz_sigma[1][0][2] = 2.31172;
PC2_dz_sigma[1][0][3] = -0.808204;
PC2_dz_sigma[1][0][4] = 0.126597;
PC2_dz_sigma[1][0][5] = -0.00539867;
PC2_dz_sigma[1][0][6] = -1.75328;
PC2_dz_sigma[1][0][7] = 0.121698;
PC2_dz_mean[1][0][0] = 0.896354;
PC2_dz_mean[1][0][1] = -0.024249;
PC2_dz_mean[1][0][2] = -0.226219;
PC2_dz_mean[1][0][3] = 0.176458;
PC2_dz_mean[1][0][4] = -0.0401968;
PC2_dz_mean[1][0][5] = 0.0865138;
PC2_dz_mean[1][0][6] = -0.0223121;
PC2_dphi_sigma[1][1][0] = 0.0511945;
PC2_dphi_sigma[1][1][1] = -0.0342738;
PC2_dphi_sigma[1][1][2] = 0.0190707;
PC2_dphi_sigma[1][1][3] = -0.00689584;
PC2_dphi_sigma[1][1][4] = 0.00140687;
PC2_dphi_sigma[1][1][5] = -0.000113646;
PC2_dphi_sigma[1][1][6] = -0.0302602;
PC2_dphi_sigma[1][1][7] = 0.00202444;
PC2_dphi_mean[1][1][0] = -0.0714693;
PC2_dphi_mean[1][1][1] = 0.00433153;
PC2_dphi_mean[1][1][2] = -0.142803;
PC2_dphi_mean[1][1][3] = 0.173069;
PC2_dphi_mean[1][1][4] = 0.0468439;
PC2_dphi_mean[1][1][5] = -0.013109;
PC2_dphi_mean[1][1][6] = 0.00169354;
PC2_dz_sigma[1][1][0] = -82.9106;
PC2_dz_sigma[1][1][1] = 69.4083;
PC2_dz_sigma[1][1][2] = -40.7646;
PC2_dz_sigma[1][1][3] = 13.0867;
PC2_dz_sigma[1][1][4] = -2.02048;
PC2_dz_sigma[1][1][5] = 0.117253;
PC2_dz_sigma[1][1][6] = 45.8792;
PC2_dz_sigma[1][1][7] = -1.63448;
PC2_dz_mean[1][1][0] = 35.6408;
PC2_dz_mean[1][1][1] = -2.2334;
PC2_dz_mean[1][1][2] = 66.6671;
PC2_dz_mean[1][1][3] = -83.7903;
PC2_dz_mean[1][1][4] = -19.7856;
PC2_dz_mean[1][1][5] = 4.94925;
PC2_dz_mean[1][1][6] = -0.556732;

return;
}
