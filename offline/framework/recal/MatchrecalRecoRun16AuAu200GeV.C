#include "MatchrecalRecoRun16AuAu200GeV.h"
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

//Run16AuAu PC3/PC2 matching recalibrator, adapted from A. Adare's MiniRecal
//Qiao Xu

using namespace std;
using namespace findNode;


MatchrecalRecoRun16AuAu200GeV::MatchrecalRecoRun16AuAu200GeV(const string &name): 
  Recalibrator(name)
{
  baseclasses.insert("PHCentralTrack");
  initParameters();
}

int MatchrecalRecoRun16AuAu200GeV::isValidRun(const int runno) const
{
   if ((runno >= 443135 && runno <= 454252) || (runno >= 458390 && runno <= 459071))
   {
      return 1;
   }
   return 0;
}

void MatchrecalRecoRun16AuAu200GeV::help() const
{
  cout << "===================================================================" << endl;
  cout << "MatchrecalRecoRun16AuAu200GeV::help method output"                    << endl;
  cout << "Qiao Xu(qiao.xu@vanderbilt.edu) " << endl;
  cout << "PC3/PC2 matching for Run-16 Au+Au " << endl;
  cout << "TOF matching will be added" << endl;
  cout << "Run14 Au-Au 16 GeV with minimum bias triggers" << endl;
  cout << "Central arm tracks with trasverse momentum < 0.2 GeV/c are ignored" << endl;
  cout << "The matching is done in dphi and dz directions, fit with double-Gaussian distribution" << endl;
  cout << endl;
  cout << "===================================================================" << endl;
}

int MatchrecalRecoRun16AuAu200GeV::process_event(PHCompositeNode *topNode)
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
       
float MatchrecalRecoRun16AuAu200GeV::GetPC3sdphi(const int iarm, const int icharge, const float pt, const float pc3dphi)
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


float MatchrecalRecoRun16AuAu200GeV::GetPC3sdz(const int iarm, const int icharge, const float pt, const float pc3dz)
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
  float mean = 0.;
  float sigma = 0.;
  int jcharge = 0;
  if(icharge==1) jcharge = 0;
  if(icharge==-1) jcharge = 1;
  if(pt >= 0.2 && pt < 0.3){
    mean = PC3_dzfirst_mean[iarm][jcharge][0];
    sigma = PC3_dzfirst_sigma[iarm][jcharge][0];
  }
  else{
          float pts;
          if(pt > 3) pts = 3.;
          else pts = pt;

  mean = PC3_dz_mean[iarm][jcharge][0]+PC3_dz_mean[iarm][jcharge][1]*pts+PC3_dz_mean[iarm][jcharge][2]/pts+PC3_dz_mean[iarm][jcharge][3]/sqrt(pts)+PC3_dz_mean[iarm][jcharge][4]/pts/pts+PC3_dz_mean[iarm][jcharge][5]/pts/pts/pts+PC3_dz_mean[iarm][jcharge][6]/pts/pts/pts/pts;
  sigma = PC3_dz_sigma[iarm][jcharge][0]+PC3_dz_sigma[iarm][jcharge][1]*pts+PC3_dz_sigma[iarm][jcharge][2]*pts*pts+PC3_dz_sigma[iarm][jcharge][3]*pts*pts*pts+PC3_dz_sigma[iarm][jcharge][4]*pts*pts*pts*pts+PC3_dz_sigma[iarm][jcharge][5]*pts*pts*pts*pts*pts+PC3_dz_sigma[iarm][jcharge][6]/sqrt(pts)+PC3_dz_sigma[iarm][jcharge][7]/pts/pts;
  
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

float MatchrecalRecoRun16AuAu200GeV::GetPC2sdphi(const int iarm, const int icharge, const float pt, const float pc2dphi)
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


float MatchrecalRecoRun16AuAu200GeV::GetPC2sdz(const int iarm, const int icharge, const float pt, const float pc2dz)
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
      float pts;
      if(pt > 3) pts = 3.;
      else pts = pt;

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

void MatchrecalRecoRun16AuAu200GeV::initParameters()
{
  //
  // Initialization
  //
PC3_dphifirst_mean[0][0][0] = -0.00497316;
PC3_dphifirst_sigma[0][0][0] = 0.0127128;
PC3_dzfirst_mean[0][0][0] = 0.392812;
PC3_dzfirst_sigma[0][0][0] = 4.43345;
PC3_dphifirst_mean[0][1][0] = 0.00635915;
PC3_dphifirst_sigma[0][1][0] = 0.01251;
PC3_dzfirst_mean[0][1][0] = 0.771295;
PC3_dzfirst_sigma[0][1][0] = 3.59874;
PC3_dphifirst_mean[1][0][0] = -0.00652311;
PC3_dphifirst_sigma[1][0][0] = 0.0130133;
PC3_dzfirst_mean[1][0][0] = 1.43078;
PC3_dzfirst_sigma[1][0][0] = 3.77798;
PC3_dphifirst_mean[1][1][0] = 0.00490101;
PC3_dphifirst_sigma[1][1][0] = 0.0123297;
PC3_dzfirst_mean[1][1][0] = 1.4153;
PC3_dzfirst_sigma[1][1][0] = 4.50789;
PC3_dphi_sigma[0][0][0] = 0.00163798;
PC3_dphi_sigma[0][0][1] = 0.000683831;
PC3_dphi_sigma[0][0][2] = -0.00126408;
PC3_dphi_sigma[0][0][3] = 0.000745572;
PC3_dphi_sigma[0][0][4] = -0.000189561;
PC3_dphi_sigma[0][0][5] = 1.79331e-05;
PC3_dphi_sigma[0][0][6] = -2.9621e-05;
PC3_dphi_sigma[0][0][7] = 0.000674701;
PC3_dphi_mean[0][0][0] = -0.00510771;
PC3_dphi_mean[0][0][1] = 0.000438504;
PC3_dphi_mean[0][0][2] = -0.0135086;
PC3_dphi_mean[0][0][3] = 0.0158387;
PC3_dphi_mean[0][0][4] = 0.00445527;
PC3_dphi_mean[0][0][5] = -0.00127273;
PC3_dphi_mean[0][0][6] = 0.000111975;
PC3_dz_sigma[0][0][0] = 2.73963;
PC3_dz_sigma[0][0][1] = -1.27194;
PC3_dz_sigma[0][0][2] = -0.0766264;
PC3_dz_sigma[0][0][3] = 0.474641;
PC3_dz_sigma[0][0][4] = -0.190013;
PC3_dz_sigma[0][0][5] = 0.0234628;
PC3_dz_sigma[0][0][6] = -0.662153;
PC3_dz_sigma[0][0][7] = 0.13369;
PC3_dz_mean[0][0][0] = 37.9287;
PC3_dz_mean[0][0][1] = -1.99373;
PC3_dz_mean[0][0][2] = 78.7965;
PC3_dz_mean[0][0][3] = -94.3998;
PC3_dz_mean[0][0][4] = -25.3371;
PC3_dz_mean[0][0][5] = 6.74104;
PC3_dz_mean[0][0][6] = -0.797561;
PC3_dphi_sigma[0][1][0] = 0.0181236;
PC3_dphi_sigma[0][1][1] = -0.0083636;
PC3_dphi_sigma[0][1][2] = 0.00224125;
PC3_dphi_sigma[0][1][3] = -2.8676e-06;
PC3_dphi_sigma[0][1][4] = -0.000114373;
PC3_dphi_sigma[0][1][5] = 1.57619e-05;
PC3_dphi_sigma[0][1][6] = -0.0109466;
PC3_dphi_sigma[0][1][7] = 0.00126187;
PC3_dphi_mean[0][1][0] = 0.0168657;
PC3_dphi_mean[0][1][1] = -0.000817213;
PC3_dphi_mean[0][1][2] = 0.0396736;
PC3_dphi_mean[0][1][3] = -0.0438408;
PC3_dphi_mean[0][1][4] = -0.0149345;
PC3_dphi_mean[0][1][5] = 0.00471562;
PC3_dphi_mean[0][1][6] = -0.000603779;
PC3_dz_sigma[0][1][0] = -7.19568;
PC3_dz_sigma[0][1][1] = 7.31758;
PC3_dz_sigma[0][1][2] = -5.31429;
PC3_dz_sigma[0][1][3] = 2.26902;
PC3_dz_sigma[0][1][4] = -0.503002;
PC3_dz_sigma[0][1][5] = 0.0447828;
PC3_dz_sigma[0][1][6] = 4.56749;
PC3_dz_sigma[0][1][7] = -0.0379212;
PC3_dz_mean[0][1][0] = 31.4393;
PC3_dz_mean[0][1][1] = -1.62737;
PC3_dz_mean[0][1][2] = 65.0379;
PC3_dz_mean[0][1][3] = -77.8976;
PC3_dz_mean[0][1][4] = -20.9485;
PC3_dz_mean[0][1][5] = 5.58579;
PC3_dz_mean[0][1][6] = -0.664023;
PC3_dphi_sigma[1][0][0] = -0.0189221;
PC3_dphi_sigma[1][0][1] = 0.0170035;
PC3_dphi_sigma[1][0][2] = -0.0100394;
PC3_dphi_sigma[1][0][3] = 0.00335581;
PC3_dphi_sigma[1][0][4] = -0.000575506;
PC3_dphi_sigma[1][0][5] = 3.9872e-05;
PC3_dphi_sigma[1][0][6] = 0.0116691;
PC3_dphi_sigma[1][0][7] = 0.00023139;
PC3_dphi_mean[1][0][0] = -0.0283243;
PC3_dphi_mean[1][0][1] = 0.00181767;
PC3_dphi_mean[1][0][2] = -0.0523387;
PC3_dphi_mean[1][0][3] = 0.0669415;
PC3_dphi_mean[1][0][4] = 0.0131989;
PC3_dphi_mean[1][0][5] = -0.00221632;
PC3_dphi_mean[1][0][6] = 3.74883e-05;
PC3_dz_sigma[1][0][0] = -33.651;
PC3_dz_sigma[1][0][1] = 27.1666;
PC3_dz_sigma[1][0][2] = -16.1448;
PC3_dz_sigma[1][0][3] = 5.61596;
PC3_dz_sigma[1][0][4] = -1.03273;
PC3_dz_sigma[1][0][5] = 0.0776834;
PC3_dz_sigma[1][0][6] = 19.9777;
PC3_dz_sigma[1][0][7] = -0.723409;
PC3_dz_mean[1][0][0] = 6.02545;
PC3_dz_mean[1][0][1] = -0.236957;
PC3_dz_mean[1][0][2] = 8.2685;
PC3_dz_mean[1][0][3] = -10.0552;
PC3_dz_mean[1][0][4] = -2.58531;
PC3_dz_mean[1][0][5] = 0.656758;
PC3_dz_mean[1][0][6] = -0.0773499;
PC3_dphi_sigma[1][1][0] = 0.0349921;
PC3_dphi_sigma[1][1][1] = -0.0185142;
PC3_dphi_sigma[1][1][2] = 0.00645557;
PC3_dphi_sigma[1][1][3] = -0.000882065;
PC3_dphi_sigma[1][1][4] = -4.93781e-05;
PC3_dphi_sigma[1][1][5] = 1.74047e-05;
PC3_dphi_sigma[1][1][6] = -0.0210834;
PC3_dphi_sigma[1][1][7] = 0.0017365;
PC3_dphi_mean[1][1][0] = -0.0325147;
PC3_dphi_mean[1][1][1] = 0.00110033;
PC3_dphi_mean[1][1][2] = -0.0965827;
PC3_dphi_mean[1][1][3] = 0.0966557;
PC3_dphi_mean[1][1][4] = 0.0432379;
PC3_dphi_mean[1][1][5] = -0.0150042;
PC3_dphi_mean[1][1][6] = 0.00222413;
PC3_dz_sigma[1][1][0] = -12.5743;
PC3_dz_sigma[1][1][1] = 11.6044;
PC3_dz_sigma[1][1][2] = -7.69217;
PC3_dz_sigma[1][1][3] = 2.97592;
PC3_dz_sigma[1][1][4] = -0.608561;
PC3_dz_sigma[1][1][5] = 0.0510195;
PC3_dz_sigma[1][1][6] = 7.64284;
PC3_dz_sigma[1][1][7] = -0.151757;
PC3_dz_mean[1][1][0] = 9.50033;
PC3_dz_mean[1][1][1] = -0.459948;
PC3_dz_mean[1][1][2] = 14.4987;
PC3_dz_mean[1][1][3] = -18.169;
PC3_dz_mean[1][1][4] = -4.23992;
PC3_dz_mean[1][1][5] = 1.0123;
PC3_dz_mean[1][1][6] = -0.106883;

PC2_dphifirst_mean[0][0][0] = -0.099;
PC2_dphifirst_sigma[0][0][0] = 0;
PC2_dzfirst_mean[0][0][0] = -9.8;
PC2_dzfirst_sigma[0][0][0] = 0;
PC2_dphifirst_mean[0][1][0] = -0.099;
PC2_dphifirst_sigma[0][1][0] = 0;
PC2_dzfirst_mean[0][1][0] = -9.8;
PC2_dzfirst_sigma[0][1][0] = 0;
PC2_dphifirst_mean[1][0][0] = -0.00577927;
PC2_dphifirst_sigma[1][0][0] = 0.0214451;
PC2_dzfirst_mean[1][0][0] = 1.21792;
PC2_dzfirst_sigma[1][0][0] = 2.16737;
PC2_dphifirst_mean[1][1][0] = 0.00384826;
PC2_dphifirst_sigma[1][1][0] = 0.00942693;
PC2_dzfirst_mean[1][1][0] = 1.37331;
PC2_dzfirst_sigma[1][1][0] = 2.2563;
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
PC2_dphi_sigma[1][0][0] = 0.0107805;
PC2_dphi_sigma[1][0][1] = -0.00484281;
PC2_dphi_sigma[1][0][2] = 0.00192214;
PC2_dphi_sigma[1][0][3] = -0.00046439;
PC2_dphi_sigma[1][0][4] = 6.56234e-05;
PC2_dphi_sigma[1][0][5] = -3.57669e-06;
PC2_dphi_sigma[1][0][6] = -0.00617409;
PC2_dphi_sigma[1][0][7] = 0.000953943;
PC2_dphi_mean[1][0][0] = -0.0186895;
PC2_dphi_mean[1][0][1] = 0.0012873;
PC2_dphi_mean[1][0][2] = -0.0308157;
PC2_dphi_mean[1][0][3] = 0.0410442;
PC2_dphi_mean[1][0][4] = 0.0069459;
PC2_dphi_mean[1][0][5] = -0.000891007;
PC2_dphi_mean[1][0][6] = -6.79961e-05;
PC2_dz_sigma[1][0][0] = -20.8162;
PC2_dz_sigma[1][0][1] = 16.717;
PC2_dz_sigma[1][0][2] = -9.59165;
PC2_dz_sigma[1][0][3] = 3.15701;
PC2_dz_sigma[1][0][4] = -0.540957;
PC2_dz_sigma[1][0][5] = 0.03749;
PC2_dz_sigma[1][0][6] = 12.4092;
PC2_dz_sigma[1][0][7] = -0.442682;
PC2_dz_mean[1][0][0] = 1.03961;
PC2_dz_mean[1][0][1] = 0.00938603;
PC2_dz_mean[1][0][2] = -1.38477;
PC2_dz_mean[1][0][3] = 1.33435;
PC2_dz_mean[1][0][4] = 0.491714;
PC2_dz_mean[1][0][5] = -0.108041;
PC2_dz_mean[1][0][6] = 0.0070692;
PC2_dphi_sigma[1][1][0] = 0.0408214;
PC2_dphi_sigma[1][1][1] = -0.0238357;
PC2_dphi_sigma[1][1][2] = 0.0104404;
PC2_dphi_sigma[1][1][3] = -0.00260883;
PC2_dphi_sigma[1][1][4] = 0.000328637;
PC2_dphi_sigma[1][1][5] = -1.45359e-05;
PC2_dphi_sigma[1][1][6] = -0.0248102;
PC2_dphi_sigma[1][1][7] = 0.00183747;
PC2_dphi_mean[1][1][0] = -0.0470499;
PC2_dphi_mean[1][1][1] = 0.0020247;
PC2_dphi_mean[1][1][2] = -0.1219;
PC2_dphi_mean[1][1][3] = 0.129883;
PC2_dphi_mean[1][1][4] = 0.0495826;
PC2_dphi_mean[1][1][5] = -0.0162046;
PC2_dphi_mean[1][1][6] = 0.00231428;
PC2_dz_sigma[1][1][0] = -13.994;
PC2_dz_sigma[1][1][1] = 11.8567;
PC2_dz_sigma[1][1][2] = -7.146;
PC2_dz_sigma[1][1][3] = 2.48323;
PC2_dz_sigma[1][1][4] = -0.450822;
PC2_dz_sigma[1][1][5] = 0.0331871;
PC2_dz_sigma[1][1][6] = 8.36631;
PC2_dz_sigma[1][1][7] = -0.256453;
PC2_dz_mean[1][1][0] = 2.43665;
PC2_dz_mean[1][1][1] = -0.0765422;
PC2_dz_mean[1][1][2] = 0.830255;
PC2_dz_mean[1][1][3] = -1.81556;
PC2_dz_mean[1][1][4] = 0.241119;
PC2_dz_mean[1][1][5] = -0.214561;
PC2_dz_mean[1][1][6] = 0.0438203;


return;
}
