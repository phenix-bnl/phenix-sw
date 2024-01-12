#include "MatchrecalRecoRun15pAl.h"
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
//A. Sickles

using namespace std;
using namespace findNode;


MatchrecalRecoRun15pAl::MatchrecalRecoRun15pAl(const string &name): 
  Recalibrator(name)
{
  baseclasses.insert("PHCentralTrack");
  initParameters();
}

int MatchrecalRecoRun15pAl::isValidRun(const int runno) const
{
   if (runno < 436759 || runno > 438422)
   {
      return 0;
   }
   return 1;
}

void MatchrecalRecoRun15pAl::help() const
{
  cout << "===================================================================" << endl;
  cout << "MatchrecalRecoRun15pAl::help method output"                    << endl;
  cout << "Qiao Xu(qiao.xu@vanderbilt.edu) " << endl;
  cout << "PC3/PC2 matching for Run-15 p+Al " << endl;
  cout << "TOF matching will be added" << endl;
  cout << "Run15 pAl 200 GeV with minimum bias triggers" << endl;
  cout << "Central arm tracks with trasverse momentum < 0.2 GeV/c are ignored" << endl;
  cout << "The matching is done in dphi and dz directions, fit with double-Gaussian distribution" << endl;
  cout << endl;
  cout << "===================================================================" << endl;
}

int MatchrecalRecoRun15pAl::process_event(PHCompositeNode *topNode)
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
       
float MatchrecalRecoRun15pAl::GetPC3sdphi(const int iarm, const int icharge, const float pt, const float pc3dphi)
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


float MatchrecalRecoRun15pAl::GetPC3sdz(const int iarm, const int icharge, const float pt, const float pc3dz)
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

float MatchrecalRecoRun15pAl::GetPC2sdphi(const int iarm, const int icharge, const float pt, const float pc2dphi)
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


float MatchrecalRecoRun15pAl::GetPC2sdz(const int iarm, const int icharge, const float pt, const float pc2dz)
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
  mean = PC2_dz_mean[iarm][jcharge][0]+PC2_dz_mean[iarm][jcharge][1]*pt+PC2_dz_mean[iarm][jcharge][2]/pt+PC2_dz_mean[iarm][jcharge][3]/sqrt(pt)+PC2_dz_mean[iarm][jcharge][4]/pt/pt+PC2_dz_mean[iarm][jcharge][5]/pt/pt/pt+PC2_dz_mean[iarm][jcharge][6]/pt/pt/pt/pt;
  sigma = PC2_dz_sigma[iarm][jcharge][0]+PC2_dz_sigma[iarm][jcharge][1]*pt+PC2_dz_sigma[iarm][jcharge][2]*pt*pt+PC2_dz_sigma[iarm][jcharge][3]*pt*pt*pt+PC2_dz_sigma[iarm][jcharge][4]*pt*pt*pt*pt+PC2_dz_sigma[iarm][jcharge][5]*pt*pt*pt*pt*pt+PC2_dz_sigma[iarm][jcharge][6]/sqrt(pt)+PC2_dz_sigma[iarm][jcharge][7]/pt/pt;
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

void MatchrecalRecoRun15pAl::initParameters()
{
  //
  // Initialization
  //

PC3_dphi_mean[0][0][0] = 0.000415739;
PC3_dphi_mean[0][0][1] = 0.000175489;
PC3_dphi_mean[0][0][2] = 0.000257705;
PC3_dphi_mean[0][0][3] = 0.000416148;
PC3_dphi_mean[0][0][4] = -3.35078e-05;
PC3_dphi_mean[0][0][5] = -8.62315e-05;
PC3_dphi_mean[0][0][6] = -6.62015e-05;
PC3_dphi_mean[0][1][0] = 0.000224111;
PC3_dphi_mean[0][1][1] = 8.16642e-05;
PC3_dphi_mean[0][1][2] = 0.000221573;
PC3_dphi_mean[0][1][3] = 0.000262665;
PC3_dphi_mean[0][1][4] = 8.49496e-05;
PC3_dphi_mean[0][1][5] = 2.75146e-05;
PC3_dphi_mean[0][1][6] = 9.9538e-06;
PC3_dphi_mean[1][0][0] = -0.000109564;
PC3_dphi_mean[1][0][1] = 4.04811e-05;
PC3_dphi_mean[1][0][2] = -0.000330053;
PC3_dphi_mean[1][0][3] = -0.000265659;
PC3_dphi_mean[1][0][4] = -0.000162519;
PC3_dphi_mean[1][0][5] = -2.38562e-05;
PC3_dphi_mean[1][0][6] = 1.8906e-05;
PC3_dphi_mean[1][1][0] = -0.000495162;
PC3_dphi_mean[1][1][1] = -0.000211503;
PC3_dphi_mean[1][1][2] = -0.000184711;
PC3_dphi_mean[1][1][3] = -0.000436748;
PC3_dphi_mean[1][1][4] = 0.000121628;
PC3_dphi_mean[1][1][5] = 9.10386e-05;
PC3_dphi_mean[1][1][6] = 3.25177e-05;
PC3_dz_mean[0][0][0] = -0.221433;
PC3_dz_mean[0][0][1] = 0.0123528;
PC3_dz_mean[0][0][2] = 0.116723;
PC3_dz_mean[0][0][3] = -0.0682445;
PC3_dz_mean[0][0][4] = -0.0205728;
PC3_dz_mean[0][0][5] = -0.0789782;
PC3_dz_mean[0][0][6] = 0.0306057;
PC3_dz_mean[0][1][0] = 0.051158;
PC3_dz_mean[0][1][1] = -0.035985;
PC3_dz_mean[0][1][2] = -0.200849;
PC3_dz_mean[0][1][3] = -0.106348;
PC3_dz_mean[0][1][4] = 0.0427897;
PC3_dz_mean[0][1][5] = 0.0745309;
PC3_dz_mean[0][1][6] = -0.034668;
PC3_dz_mean[1][0][0] = 1.28171;
PC3_dz_mean[1][0][1] = 0.00434357;
PC3_dz_mean[1][0][2] = -0.336393;
PC3_dz_mean[1][0][3] = 0.70856;
PC3_dz_mean[1][0][4] = -0.245686;
PC3_dz_mean[1][0][5] = 0.175948;
PC3_dz_mean[1][0][6] = -0.0394322;
PC3_dz_mean[1][1][0] = 1.19895;
PC3_dz_mean[1][1][1] = 0.0135684;
PC3_dz_mean[1][1][2] = -0.157077;
PC3_dz_mean[1][1][3] = 0.766336;
PC3_dz_mean[1][1][4] = -0.237471;
PC3_dz_mean[1][1][5] = 0.113102;
PC3_dz_mean[1][1][6] = -0.0219464;
PC3_dphi_sigma[0][0][0] = 0.000512267;
PC3_dphi_sigma[0][0][1] = 0.000104286;
PC3_dphi_sigma[0][0][2] = 1.61669e-05;
PC3_dphi_sigma[0][0][3] = 2.32037e-06;
PC3_dphi_sigma[0][0][4] = 3.01031e-07;
PC3_dphi_sigma[0][0][5] = 2.98224e-08;
PC3_dphi_sigma[0][0][6] = 0.000830324;
PC3_dphi_sigma[0][0][7] = 0.000731142;
PC3_dphi_sigma[0][1][0] = 0.000494371;
PC3_dphi_sigma[0][1][1] = 9.71689e-05;
PC3_dphi_sigma[0][1][2] = 1.39248e-05;
PC3_dphi_sigma[0][1][3] = 1.70113e-06;
PC3_dphi_sigma[0][1][4] = 1.42494e-07;
PC3_dphi_sigma[0][1][5] = -9.01686e-09;
PC3_dphi_sigma[0][1][6] = 0.000806234;
PC3_dphi_sigma[0][1][7] = 0.00070623;
PC3_dphi_sigma[1][0][0] = 0.000619738;
PC3_dphi_sigma[1][0][1] = 0.000127325;
PC3_dphi_sigma[1][0][2] = 1.92761e-05;
PC3_dphi_sigma[1][0][3] = 2.6187e-06;
PC3_dphi_sigma[1][0][4] = 3.04335e-07;
PC3_dphi_sigma[1][0][5] = 2.0823e-08;
PC3_dphi_sigma[1][0][6] = 0.000987801;
PC3_dphi_sigma[1][0][7] = 0.000809001;
PC3_dphi_sigma[1][1][0] = 0.000745167;
PC3_dphi_sigma[1][1][1] = 0.000157031;
PC3_dphi_sigma[1][1][2] = 2.00949e-05;
PC3_dphi_sigma[1][1][3] = 1.41246e-06;
PC3_dphi_sigma[1][1][4] = -2.26715e-07;
PC3_dphi_sigma[1][1][5] = -1.45692e-07;
PC3_dphi_sigma[1][1][6] = 0.00110145;
PC3_dphi_sigma[1][1][7] = 0.000571209;
PC3_dz_sigma[0][0][0] = -0.402782;
PC3_dz_sigma[0][0][1] = 0.2184;
PC3_dz_sigma[0][0][2] = 0.102234;
PC3_dz_sigma[0][0][3] = -0.0158471;
PC3_dz_sigma[0][0][4] = -0.00844221;
PC3_dz_sigma[0][0][5] = 0.00161999;
PC3_dz_sigma[0][0][6] = 1.76864;
PC3_dz_sigma[0][0][7] = -0.0340316;
PC3_dz_sigma[0][1][0] = 1.15536;
PC3_dz_sigma[0][1][1] = 0.069951;
PC3_dz_sigma[0][1][2] = -0.039274;
PC3_dz_sigma[0][1][3] = 0.00658131;
PC3_dz_sigma[0][1][4] = 0.00333028;
PC3_dz_sigma[0][1][5] = -0.000738106;
PC3_dz_sigma[0][1][6] = 0.294654;
PC3_dz_sigma[0][1][7] = 0.131052;
PC3_dz_sigma[1][0][0] = 1.15723;
PC3_dz_sigma[1][0][1] = 0.0155606;
PC3_dz_sigma[1][0][2] = -0.0469413;
PC3_dz_sigma[1][0][3] = 0.00729794;
PC3_dz_sigma[1][0][4] = 0.00377079;
PC3_dz_sigma[1][0][5] = -0.000681568;
PC3_dz_sigma[1][0][6] = 0.499463;
PC3_dz_sigma[1][0][7] = 0.109521;
PC3_dz_sigma[1][1][0] = 1.10563;
PC3_dz_sigma[1][1][1] = 0.0126482;
PC3_dz_sigma[1][1][2] = -0.0383994;
PC3_dz_sigma[1][1][3] = 0.00974568;
PC3_dz_sigma[1][1][4] = 0.00383429;
PC3_dz_sigma[1][1][5] = -0.000904168;
PC3_dz_sigma[1][1][6] = 0.504186;
PC3_dz_sigma[1][1][7] = 0.0995106;


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
PC2_dphi_sigma[1][0][0] = 0.0205071;
PC2_dphi_sigma[1][0][1] = -0.0109408;
PC2_dphi_sigma[1][0][2] = 0.00457997;
PC2_dphi_sigma[1][0][3] = -0.0011267;
PC2_dphi_sigma[1][0][4] = 0.00015152;
PC2_dphi_sigma[1][0][5] = -8.3981e-06;
PC2_dphi_sigma[1][0][6] = -0.0121034;
PC2_dphi_sigma[1][0][7] = 0.00117846;
PC2_dphi_mean[1][0][0] = -0.0170786;
PC2_dphi_mean[1][0][1] = 0.00111006;
PC2_dphi_mean[1][0][2] = -0.0258466;
PC2_dphi_mean[1][0][3] = 0.0366893;
PC2_dphi_mean[1][0][4] = 0.00356916;
PC2_dphi_mean[1][0][5] = 0.000738755;
PC2_dphi_mean[1][0][6] = -0.000367044;
PC2_dz_sigma[1][0][0] = -35.06;
PC2_dz_sigma[1][0][1] = 27.6402;
PC2_dz_sigma[1][0][2] = -15.1395;
PC2_dz_sigma[1][0][3] = 4.7172;
PC2_dz_sigma[1][0][4] = -0.759358;
PC2_dz_sigma[1][0][5] = 0.0487932;
PC2_dz_sigma[1][0][6] = 20.3324;
PC2_dz_sigma[1][0][7] = -0.646996;
PC2_dz_mean[1][0][0] = -6.6604;
PC2_dz_mean[1][0][1] = 0.416345;
PC2_dz_mean[1][0][2] = -16.6049;
PC2_dz_mean[1][0][3] = 20.0677;
PC2_dz_mean[1][0][4] = 5.0478;
PC2_dz_mean[1][0][5] = -1.1755;
PC2_dz_mean[1][0][6] = 0.10863;
PC2_dphi_sigma[1][1][0] = 0.0671569;
PC2_dphi_sigma[1][1][1] = -0.0465362;
PC2_dphi_sigma[1][1][2] = 0.0241353;
PC2_dphi_sigma[1][1][3] = -0.00721966;
PC2_dphi_sigma[1][1][4] = 0.00112173;
PC2_dphi_sigma[1][1][5] = -6.93646e-05;
PC2_dphi_sigma[1][1][6] = -0.0387949;
PC2_dphi_sigma[1][1][7] = 0.00230065;
PC2_dphi_mean[1][1][0] = -0.0368508;
PC2_dphi_mean[1][1][1] = 0.00147024;
PC2_dphi_mean[1][1][2] = -0.0991889;
PC2_dphi_mean[1][1][3] = 0.102902;
PC2_dphi_mean[1][1][4] = 0.0427529;
PC2_dphi_mean[1][1][5] = -0.0147548;
PC2_dphi_mean[1][1][6] = 0.00220706;
PC2_dz_sigma[1][1][0] = 15.2779;
PC2_dz_sigma[1][1][1] = -10.4876;
PC2_dz_sigma[1][1][2] = 5.62971;
PC2_dz_sigma[1][1][3] = -1.7421;
PC2_dz_sigma[1][1][4] = 0.278874;
PC2_dz_sigma[1][1][5] = -0.0177742;
PC2_dz_sigma[1][1][6] = -8.27456;
PC2_dz_sigma[1][1][7] = 0.479692;
PC2_dz_mean[1][1][0] = 1.36851;
PC2_dz_mean[1][1][1] = -0.0367935;
PC2_dz_mean[1][1][2] = -0.20829;
PC2_dz_mean[1][1][3] = -0.0670255;
PC2_dz_mean[1][1][4] = 0.184438;
PC2_dz_mean[1][1][5] = -0.0641618;
PC2_dz_mean[1][1][6] = 0.00857133;
  
return;
}
