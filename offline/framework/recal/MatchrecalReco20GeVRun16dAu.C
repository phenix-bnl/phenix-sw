#include "MatchrecalReco20GeVRun16dAu.h"
#include <Fun4AllReturnCodes.h>

#include <PHGlobal.h>
#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>
#include <getClass.h>
#include <RunHeader.h>
#include <PHPoint.h>
#include <VtxOut.h>

#include <PHCompositeNode.h>
#include <RunNumberRanges.h>
#include <recoConsts.h>
#include <iostream>

//20GeVRun16dAu PC3 matching recalibrator, adapted from A. Adare's MiniRecal
//A. Sickles

using namespace std;
using namespace findNode;


MatchrecalReco20GeVRun16dAu::MatchrecalReco20GeVRun16dAu(const string &name): 
  Recalibrator(name)
{
  baseclasses.insert("PHCentralTrack");
  initParameters();
}

int MatchrecalReco20GeVRun16dAu::isValidRun(const int runno) const
{
   if (runno < 456652 || runno > 457298)
   {
      return 0;
   }
   return 1;
}

void MatchrecalReco20GeVRun16dAu::help() const
{
  cout << "===================================================================" << endl;
  cout << "MatchrecalReco20GeVRun16dAu::help method output"                    << endl;
  cout << "Qiao Xu(qiao.xu@vanderbilt.edu), Weizhuang Peng(weizhuang.peng@vanderbilt.edu)" << endl;
  cout << "PC3 matching for Run-16 20GeV d+Au " << endl;
  cout << "TOF matching will be added" << endl;
  cout << "Run16 dAu 20 GeV with all the triggers combination" << endl;
  cout << "Central arm tracks with trasverse momentum < 0.2 GeV/c are ignored" << endl;
  cout << "The matching is done in dphi and dz directions, fit with double-Gaussian distribution" << endl;
  cout << endl;
  cout << "===================================================================" << endl;
}

int MatchrecalReco20GeVRun16dAu::process_event(PHCompositeNode *topNode)
{
   PHCentralTrack *d_cnt = findNode::getClass<PHCentralTrack>(topNode, "PHCentralTrack");
   PHGlobal *d_global = findNode::getClass<PHGlobal>(topNode, "PHGlobal");
   VtxOut *vertexes = findNode::getClass<VtxOut>(topNode, "VtxOut");
   if (!d_cnt)
   {
      return 0;
   }
   if (!d_global)
   {
      return 0;
   }
  float bbcv = d_global->getBbcZVertex();
  PHPoint fvtx_vertex = vertexes->get_Vertex("FVTX");
  float  FVTXz = fvtx_vertex.getZ();
  if(FVTXz!=FVTXz) {
    FVTXz=-9999;
    // leave zvertex from the BBC
  } else {
    bbcv = FVTXz; 
  }
  int ivz=-1;
  //if(bbcv>-30 && bbcv<=-10) ivz=0;
  //else if(bbcv>-20 && bbcv<=-10) ivz=1;
  //else if(bbcv>-10 && bbcv<=-5) ivz=1;
  //else if(bbcv>-5 && bbcv<=0) ivz=2;
  //else if(bbcv>0 && bbcv<=5) ivz=3;
  //else if(bbcv>5 && bbcv<=10) ivz=4;
  //else if(bbcv>10 && bbcv<=20) ivz=6;
  //else if(bbcv>10 && bbcv<=30) ivz=5;
  if(bbcv>-10 && bbcv<=10) ivz=0;
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

      float PC2dphi = d_cnt->get_pc2dphi(i);
      float PC2dz = d_cnt->get_pc2dz(i);

      float PC2sdphi = GetPC2sdphi(arm,charge,0,dummypt,PC2dphi);
      float PC2sdz = GetPC2sdz(arm,charge,0,dummypt,PC2dz);
      
      d_cnt->set_pc2sdphi(i,PC2sdphi);
      d_cnt->set_pc2sdz(i,PC2sdz);
      
   }

   return EVENT_OK;
}
       
float MatchrecalReco20GeVRun16dAu::GetPC3sdphi(const int iarm, const int icharge, const int ivz, const float pt, const float pc3dphi)
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
  if(pt > 3){  float pts = 3.;
      mean = PC3_dphi_mean[iarm][jcharge][ivz][0]+PC3_dphi_mean[iarm][jcharge][ivz][1]*pts+PC3_dphi_mean[iarm][jcharge][ivz][2]/pts+PC3_dphi_mean[iarm][jcharge][ivz][3]/sqrt(pts)+PC3_dphi_mean[iarm][jcharge][ivz][4]/pts/pts+PC3_dphi_mean[iarm][jcharge][ivz][5]/pts/pts/pts+PC3_dphi_mean[iarm][jcharge][ivz][6]/pts/pts/pts/pts;
      sigma = PC3_dphi_sigma[iarm][jcharge][ivz][0]+PC3_dphi_sigma[iarm][jcharge][ivz][1]*pts+PC3_dphi_sigma[iarm][jcharge][ivz][2]*pts*pts+PC3_dphi_sigma[iarm][jcharge][ivz][3]*pts*pts*pts+PC3_dphi_sigma[iarm][jcharge][ivz][4]*pts*pts*pts*pts+PC3_dphi_sigma[iarm][jcharge][ivz][5]*pts*pts*pts*pts*pts+PC3_dphi_sigma[iarm][jcharge][ivz][6]/sqrt(pts)+PC3_dphi_sigma[iarm][jcharge][ivz][7]/pts/pts;
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


float MatchrecalReco20GeVRun16dAu::GetPC3sdz(const int iarm, const int icharge, const int ivz, const float pt, const float pc3dz)
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
  if(pt > 3){ float pts = 3.;
      mean = PC3_dz_mean[iarm][jcharge][ivz][0]+PC3_dz_mean[iarm][jcharge][ivz][1]*pts+PC3_dz_mean[iarm][jcharge][ivz][2]/pts+PC3_dz_mean[iarm][jcharge][ivz][3]/sqrt(pts)+PC3_dz_mean[iarm][jcharge][ivz][4]/pts/pts+PC3_dz_mean[iarm][jcharge][ivz][5]/pts/pts/pts+PC3_dz_mean[iarm][jcharge][ivz][6]/pts/pts/pts/pts;
      sigma = PC3_dz_sigma[iarm][jcharge][ivz][0]+PC3_dz_sigma[iarm][jcharge][ivz][1]*pts+PC3_dz_sigma[iarm][jcharge][ivz][2]*pts*pts+PC3_dz_sigma[iarm][jcharge][ivz][3]*pts*pts*pts+PC3_dz_sigma[iarm][jcharge][ivz][4]*pts*pts*pts*pts+PC3_dz_sigma[iarm][jcharge][ivz][5]*pts*pts*pts*pts*pts+PC3_dz_sigma[iarm][jcharge][ivz][6]/sqrt(pts)+PC3_dz_sigma[iarm][jcharge][ivz][7]/pts/pts;
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

float MatchrecalReco20GeVRun16dAu::GetPC2sdphi(const int iarm, const int icharge, const int ivz, const float pt, const float PC2dphi)
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
      if(pt > 3){ float pts = 3.;
      mean = PC2_dphi_mean[iarm][jcharge][ivz][0]+PC2_dphi_mean[iarm][jcharge][ivz][1]*pts+PC2_dphi_mean[iarm][jcharge][ivz][2]/pts+PC2_dphi_mean[iarm][jcharge][ivz][3]/sqrt(pts)+PC2_dphi_mean[iarm][jcharge][ivz][4]/pts/pts+PC2_dphi_mean[iarm][jcharge][ivz][5]/pts/pts/pts+PC2_dphi_mean[iarm][jcharge][ivz][6]/pts/pts/pts/pts;
      sigma = PC2_dphi_sigma[iarm][jcharge][ivz][0]+PC2_dphi_sigma[iarm][jcharge][ivz][1]*pts+PC2_dphi_sigma[iarm][jcharge][ivz][2]*pts*pts+PC2_dphi_sigma[iarm][jcharge][ivz][3]*pts*pts*pts+PC2_dphi_sigma[iarm][jcharge][ivz][4]*pts*pts*pts*pts+PC2_dphi_sigma[iarm][jcharge][ivz][5]*pts*pts*pts*pts*pts+PC2_dphi_sigma[iarm][jcharge][ivz][6]/sqrt(pts)+PC2_dphi_sigma[iarm][jcharge][ivz][7]/pts/pts;
    }
  }
  if (Verbosity()){
    cout <<  "GetPC2sdphi(" << iarm << ", " << icharge << ", "  << pt << ", " << PC2dphi << ") " <<
      "\nmean= " << mean << 
      endl;
    cout <<  "GetPC2sdphi(" << iarm << ", " << icharge << ", "  << pt << ", " << PC2dphi << ") " <<
      "\nsigma= " << sigma << 
      endl;
  }
  if(sigma == 0)
      return -999;
  else
    return (PC2dphi - mean) / sigma;
}


float MatchrecalReco20GeVRun16dAu::GetPC2sdz(const int iarm, const int icharge, const int ivz, const float pt, const float PC2dz)
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
      if(pt > 3){ float pts = 3.;
      mean = PC2_dz_mean[iarm][jcharge][ivz][0]+PC2_dz_mean[iarm][jcharge][ivz][1]*pts+PC2_dz_mean[iarm][jcharge][ivz][2]/pts+PC2_dz_mean[iarm][jcharge][ivz][3]/sqrt(pts)+PC2_dz_mean[iarm][jcharge][ivz][4]/pts/pts+PC2_dz_mean[iarm][jcharge][ivz][5]/pts/pts/pts+PC2_dz_mean[iarm][jcharge][ivz][6]/pts/pts/pts/pts;
      sigma = PC2_dz_sigma[iarm][jcharge][ivz][0]+PC2_dz_sigma[iarm][jcharge][ivz][1]*pts+PC2_dz_sigma[iarm][jcharge][ivz][2]*pts*pts+PC2_dz_sigma[iarm][jcharge][ivz][3]*pts*pts*pts+PC2_dz_sigma[iarm][jcharge][ivz][4]*pts*pts*pts*pts+PC2_dz_sigma[iarm][jcharge][ivz][5]*pts*pts*pts*pts*pts+PC2_dz_sigma[iarm][jcharge][ivz][6]/sqrt(pts)+PC2_dz_sigma[iarm][jcharge][ivz][7]/pts/pts;
    }
  }
  if (Verbosity()){
    cout <<  "GetPC2sdz(" << iarm << ", " << icharge << ", " << ivz << ", " << pt << ", " << PC2dz << ") " <<
      "\nmean= " << mean << 
      endl;
    cout <<  "GetPC2sdz(" << iarm << ", " << icharge << ", " << ivz << ", " << pt << ", " << PC2dz << ") " <<
      "\nsigma= " << sigma << 
      endl;
  }
  if(sigma == 0)
      return -999;
  else
    return (PC2dz - mean) / sigma;
}

void MatchrecalReco20GeVRun16dAu::initParameters()
{
  //
  // Initialization
  //

    PC3_dphifirst_mean[0][0][0] = -0.00345074;
    PC3_dphifirst_sigma[0][0][0] = 0.00917307;
    PC3_dzfirst_mean[0][0][0] = 0.151734;
    PC3_dzfirst_sigma[0][0][0] = 3.18506;
    PC3_dphifirst_mean[0][1][0] = 0.00454469;
    PC3_dphifirst_sigma[0][1][0] = 0.00899247;
    PC3_dzfirst_mean[0][1][0] = 0.369316;
    PC3_dzfirst_sigma[0][1][0] = 2.82791;
    PC3_dphifirst_mean[1][0][0] = -0.00726084;
    PC3_dphifirst_sigma[1][0][0] = 0.0101044;
    PC3_dzfirst_mean[1][0][0] = 0.991556;
    PC3_dzfirst_sigma[1][0][0] = 3.26781;
    PC3_dphifirst_mean[1][1][0] = 0.00632074;
    PC3_dphifirst_sigma[1][1][0] = 0.0101914;
    PC3_dzfirst_mean[1][1][0] = 0.656093;
    PC3_dzfirst_sigma[1][1][0] = 3.16383;
    PC3_dphi_sigma[0][0][0][0] = -0.172236;
    PC3_dphi_sigma[0][0][0][1] = 0.168549;
    PC3_dphi_sigma[0][0][0][2] = -0.122698;
    PC3_dphi_sigma[0][0][0][3] = 0.0528138;
    PC3_dphi_sigma[0][0][0][4] = -0.0121046;
    PC3_dphi_sigma[0][0][0][5] = 0.00113636;
    PC3_dphi_sigma[0][0][0][6] = 0.0890013;
    PC3_dphi_sigma[0][0][0][7] = -0.0021531;
    PC3_dphi_mean[0][0][0][0] = -0.0479214;
    PC3_dphi_mean[0][0][0][1] = 0.00329109;
    PC3_dphi_mean[0][0][0][2] = -0.0917845;
    PC3_dphi_mean[0][0][0][3] = 0.117102;
    PC3_dphi_mean[0][0][0][4] = 0.025921;
    PC3_dphi_mean[0][0][0][5] = -0.00607109;
    PC3_dphi_mean[0][0][0][6] = 0.000590218;
    PC3_dz_sigma[0][0][0][0] = 5.68447;
    PC3_dz_sigma[0][0][0][1] = -2.07425;
    PC3_dz_sigma[0][0][0][2] = -1.34599;
    PC3_dz_sigma[0][0][0][3] = 1.94617;
    PC3_dz_sigma[0][0][0][4] = -0.753794;
    PC3_dz_sigma[0][0][0][5] = 0.0981798;
    PC3_dz_sigma[0][0][0][6] = -2.5475;
    PC3_dz_sigma[0][0][0][7] = 0.204097;
    PC3_dz_mean[0][0][0][0] = -2.3413;
    PC3_dz_mean[0][0][0][1] = 0.563646;
    PC3_dz_mean[0][0][0][2] = 0.719355;
    PC3_dz_mean[0][0][0][3] = 2.8045;
    PC3_dz_mean[0][0][0][4] = -1.48111;
    PC3_dz_mean[0][0][0][5] = 0.51254;
    PC3_dz_mean[0][0][0][6] = -0.0632445;
    PC3_dphi_sigma[0][1][0][0] = -0.115336;
    PC3_dphi_sigma[0][1][0][1] = 0.130272;
    PC3_dphi_sigma[0][1][0][2] = -0.104058;
    PC3_dphi_sigma[0][1][0][3] = 0.0478927;
    PC3_dphi_sigma[0][1][0][4] = -0.0115541;
    PC3_dphi_sigma[0][1][0][5] = 0.00112921;
    PC3_dphi_sigma[0][1][0][6] = 0.054507;
    PC3_dphi_sigma[0][1][0][7] = -0.000626904;
    PC3_dphi_mean[0][1][0][0] = -0.00215621;
    PC3_dphi_mean[0][1][0][1] = 0.000399059;
    PC3_dphi_mean[0][1][0][2] = 0.000911218;
    PC3_dphi_mean[0][1][0][3] = 0.00265692;
    PC3_dphi_mean[0][1][0][4] = -0.00120397;
    PC3_dphi_mean[0][1][0][5] = 0.000376675;
    PC3_dphi_mean[0][1][0][6] = -4.64592e-06;
    PC3_dz_sigma[0][1][0][0] = 10.9634;
    PC3_dz_sigma[0][1][0][1] = -12.0964;
    PC3_dz_sigma[0][1][0][2] = 10.2022;
    PC3_dz_sigma[0][1][0][3] = -5.0757;
    PC3_dz_sigma[0][1][0][4] = 1.36511;
    PC3_dz_sigma[0][1][0][5] = -0.151567;
    PC3_dz_sigma[0][1][0][6] = -4.25672;
    PC3_dz_sigma[0][1][0][7] = 0.19537;
    PC3_dz_mean[0][1][0][0] = 1.69271;
    PC3_dz_mean[0][1][0][1] = -0.111682;
    PC3_dz_mean[0][1][0][2] = -0.152013;
    PC3_dz_mean[0][1][0][3] = -0.95791;
    PC3_dz_mean[0][1][0][4] = 0.431024;
    PC3_dz_mean[0][1][0][5] = -0.212828;
    PC3_dz_mean[0][1][0][6] = 0.0325871;
    PC3_dphi_sigma[1][0][0][0] = 0.00983525;
    PC3_dphi_sigma[1][0][0][1] = -0.0243198;
    PC3_dphi_sigma[1][0][0][2] = 0.0290023;
    PC3_dphi_sigma[1][0][0][3] = -0.017023;
    PC3_dphi_sigma[1][0][0][4] = 0.0048406;
    PC3_dphi_sigma[1][0][0][5] = -0.00052953;
    PC3_dphi_sigma[1][0][0][6] = 0.000846253;
    PC3_dphi_sigma[1][0][0][7] = 0.000234219;
    PC3_dphi_mean[1][0][0][0] = -0.108388;
    PC3_dphi_mean[1][0][0][1] = 0.00718682;
    PC3_dphi_mean[1][0][0][2] = -0.20381;
    PC3_dphi_mean[1][0][0][3] = 0.258122;
    PC3_dphi_mean[1][0][0][4] = 0.0583765;
    PC3_dphi_mean[1][0][0][5] = -0.0137275;
    PC3_dphi_mean[1][0][0][6] = 0.00136272;
    PC3_dz_sigma[1][0][0][0] = 13.3233;
    PC3_dz_sigma[1][0][0][1] = -14.104;
    PC3_dz_sigma[1][0][0][2] = 11.2786;
    PC3_dz_sigma[1][0][0][3] = -5.18379;
    PC3_dz_sigma[1][0][0][4] = 1.22355;
    PC3_dz_sigma[1][0][0][5] = -0.114083;
    PC3_dz_sigma[1][0][0][6] = -5.34935;
    PC3_dz_sigma[1][0][0][7] = 0.228527;
    PC3_dz_mean[1][0][0][0] = 3.15301;
    PC3_dz_mean[1][0][0][1] = -0.342068;
    PC3_dz_mean[1][0][0][2] = -0.841228;
    PC3_dz_mean[1][0][0][3] = -0.526715;
    PC3_dz_mean[1][0][0][4] = 0.579577;
    PC3_dz_mean[1][0][0][5] = -0.170114;
    PC3_dz_mean[1][0][0][6] = 0.0151558;
    PC3_dphi_sigma[1][1][0][0] = -0.172157;
    PC3_dphi_sigma[1][1][0][1] = 0.207664;
    PC3_dphi_sigma[1][1][0][2] = -0.176376;
    PC3_dphi_sigma[1][1][0][3] = 0.0856675;
    PC3_dphi_sigma[1][1][0][4] = -0.0216925;
    PC3_dphi_sigma[1][1][0][5] = 0.00221774;
    PC3_dphi_sigma[1][1][0][6] = 0.0784302;
    PC3_dphi_sigma[1][1][0][7] = -0.000952605;
    PC3_dphi_mean[1][1][0][0] = 0.162008;
    PC3_dphi_mean[1][1][0][1] = -0.0112514;
    PC3_dphi_mean[1][1][0][2] = 0.282354;
    PC3_dphi_mean[1][1][0][3] = -0.376409;
    PC3_dphi_mean[1][1][0][4] = -0.0710515;
    PC3_dphi_mean[1][1][0][5] = 0.01427;
    PC3_dphi_mean[1][1][0][6] = -0.00117908;
    PC3_dz_sigma[1][1][0][0] = -20.4527;
    PC3_dz_sigma[1][1][0][1] = 23.1713;
    PC3_dz_sigma[1][1][0][2] = -19.7267;
    PC3_dz_sigma[1][1][0][3] = 9.98706;
    PC3_dz_sigma[1][1][0][4] = -2.6741;
    PC3_dz_sigma[1][1][0][5] = 0.289545;
    PC3_dz_sigma[1][1][0][6] = 10.851;
    PC3_dz_sigma[1][1][0][7] = -0.210462;
    PC3_dz_mean[1][1][0][0] = 18.0579;
    PC3_dz_mean[1][1][0][1] = -1.20579;
    PC3_dz_mean[1][1][0][2] = 27.9113;
    PC3_dz_mean[1][1][0][3] = -36.9612;
    PC3_dz_mean[1][1][0][4] = -7.54278;
    PC3_dz_mean[1][1][0][5] = 1.70555;
    PC3_dz_mean[1][1][0][6] = -0.176472;

PC2_dphifirst_mean[0][0][0] = -0.099;
PC2_dphifirst_sigma[0][0][0] = 0;
PC2_dzfirst_mean[0][0][0] = -9.8;
PC2_dzfirst_sigma[0][0][0] = 0;
PC2_dphifirst_mean[0][1][0] = -0.099;
PC2_dphifirst_sigma[0][1][0] = 0;
PC2_dzfirst_mean[0][1][0] = -9.8;
PC2_dzfirst_sigma[0][1][0] = 0;
PC2_dphifirst_mean[1][0][0] = -0.0043036;
PC2_dphifirst_sigma[1][0][0] = 0.0067052;
PC2_dzfirst_mean[1][0][0] = 0.316685;
PC2_dzfirst_sigma[1][0][0] = 3.44028;
PC2_dphifirst_mean[1][1][0] = 0.00147032;
PC2_dphifirst_sigma[1][1][0] = 0.00639142;
PC2_dzfirst_mean[1][1][0] = 0.0329409;
PC2_dzfirst_sigma[1][1][0] = 3.46895;
PC2_dphi_sigma[0][0][0][0] = 0;
PC2_dphi_sigma[0][0][0][1] = 0;
PC2_dphi_sigma[0][0][0][2] = 0;
PC2_dphi_sigma[0][0][0][3] = 0;
PC2_dphi_sigma[0][0][0][4] = 0;
PC2_dphi_sigma[0][0][0][5] = 0;
PC2_dphi_sigma[0][0][0][6] = 0;
PC2_dphi_sigma[0][0][0][7] = 0;
PC2_dphi_mean[0][0][0][0] = -0.0480741;
PC2_dphi_mean[0][0][0][1] = -0.00795064;
PC2_dphi_mean[0][0][0][2] = -0.0154756;
PC2_dphi_mean[0][0][0][3] = -0.0426187;
PC2_dphi_mean[0][0][0][4] = 0.0125659;
PC2_dphi_mean[0][0][0][5] = 0.00399688;
PC2_dphi_mean[0][0][0][6] = -0.00229411;
PC2_dz_sigma[0][0][0][0] = 0;
PC2_dz_sigma[0][0][0][1] = 0;
PC2_dz_sigma[0][0][0][2] = 0;
PC2_dz_sigma[0][0][0][3] = 0;
PC2_dz_sigma[0][0][0][4] = 0;
PC2_dz_sigma[0][0][0][5] = 0;
PC2_dz_sigma[0][0][0][6] = 0;
PC2_dz_sigma[0][0][0][7] = 0;
PC2_dz_mean[0][0][0][0] = -7.57078;
PC2_dz_mean[0][0][0][1] = -0.202469;
PC2_dz_mean[0][0][0][2] = 1.29485;
PC2_dz_mean[0][0][0][3] = -3.70357;
PC2_dz_mean[0][0][0][4] = 0.879719;
PC2_dz_mean[0][0][0][5] = -0.619026;
PC2_dz_mean[0][0][0][6] = 0.121239;
PC2_dphi_sigma[0][1][0][0] = 0;
PC2_dphi_sigma[0][1][0][1] = 0;
PC2_dphi_sigma[0][1][0][2] = 0;
PC2_dphi_sigma[0][1][0][3] = 0;
PC2_dphi_sigma[0][1][0][4] = 0;
PC2_dphi_sigma[0][1][0][5] = 0;
PC2_dphi_sigma[0][1][0][6] = 0;
PC2_dphi_sigma[0][1][0][7] = 0;
PC2_dphi_mean[0][1][0][0] = -0.0480741;
PC2_dphi_mean[0][1][0][1] = -0.00795064;
PC2_dphi_mean[0][1][0][2] = -0.0154756;
PC2_dphi_mean[0][1][0][3] = -0.0426187;
PC2_dphi_mean[0][1][0][4] = 0.0125659;
PC2_dphi_mean[0][1][0][5] = 0.00399688;
PC2_dphi_mean[0][1][0][6] = -0.00229411;
PC2_dz_sigma[0][1][0][0] = 0;
PC2_dz_sigma[0][1][0][1] = 0;
PC2_dz_sigma[0][1][0][2] = 0;
PC2_dz_sigma[0][1][0][3] = 0;
PC2_dz_sigma[0][1][0][4] = 0;
PC2_dz_sigma[0][1][0][5] = 0;
PC2_dz_sigma[0][1][0][6] = 0;
PC2_dz_sigma[0][1][0][7] = 0;
PC2_dz_mean[0][1][0][0] = -7.57078;
PC2_dz_mean[0][1][0][1] = -0.202469;
PC2_dz_mean[0][1][0][2] = 1.29485;
PC2_dz_mean[0][1][0][3] = -3.70357;
PC2_dz_mean[0][1][0][4] = 0.879719;
PC2_dz_mean[0][1][0][5] = -0.619026;
PC2_dz_mean[0][1][0][6] = 0.121239;
PC2_dphi_sigma[1][0][0][0] = 0.097536;
PC2_dphi_sigma[1][0][0][1] = -0.0925514;
PC2_dphi_sigma[1][0][0][2] = 0.0732012;
PC2_dphi_sigma[1][0][0][3] = -0.0356843;
PC2_dphi_sigma[1][0][0][4] = 0.00931168;
PC2_dphi_sigma[1][0][0][5] = -0.000982028;
PC2_dphi_sigma[1][0][0][6] = -0.0508327;
PC2_dphi_sigma[1][0][0][7] = 0.00244463;
PC2_dphi_mean[1][0][0][0] = -0.00341067;
PC2_dphi_mean[1][0][0][1] = 0.000496647;
PC2_dphi_mean[1][0][0][2] = -0.00112326;
PC2_dphi_mean[1][0][0][3] = 0.00389741;
PC2_dphi_mean[1][0][0][4] = -0.00270379;
PC2_dphi_mean[1][0][0][5] = 0.00208964;
PC2_dphi_mean[1][0][0][6] = -0.000500613;
PC2_dz_sigma[1][0][0][0] = -120.063;
PC2_dz_sigma[1][0][0][1] = 85.7108;
PC2_dz_sigma[1][0][0][2] = -43.0564;
PC2_dz_sigma[1][0][0][3] = 11.4965;
PC2_dz_sigma[1][0][0][4] = -1.23667;
PC2_dz_sigma[1][0][0][5] = 71.5953;
PC2_dz_sigma[1][0][0][6] = -3.03419;
PC2_dz_sigma[1][0][0][7] = 0;
PC2_dz_mean[1][0][0][0] = -1.3089;
PC2_dz_mean[1][0][0][1] = 0.294059;
PC2_dz_mean[1][0][0][2] = 0.237824;
PC2_dz_mean[1][0][0][3] = 3.04557;
PC2_dz_mean[1][0][0][4] = -1.614;
PC2_dz_mean[1][0][0][5] = 0.685657;
PC2_dz_mean[1][0][0][6] = -0.100011;
PC2_dphi_sigma[1][1][0][0] = 0.0295169;
PC2_dphi_sigma[1][1][0][1] = 0.00776261;
PC2_dphi_sigma[1][1][0][2] = -0.0209663;
PC2_dphi_sigma[1][1][0][3] = 0.011936;
PC2_dphi_sigma[1][1][0][4] = -0.0027599;
PC2_dphi_sigma[1][1][0][5] = 0.000217669;
PC2_dphi_sigma[1][1][0][6] = -0.0256848;
PC2_dphi_sigma[1][1][0][7] = 0.00252352;
PC2_dphi_mean[1][1][0][0] = 0.0244044;
PC2_dphi_mean[1][1][0][1] = -0.00280358;
PC2_dphi_mean[1][1][0][2] = 0.00118929;
PC2_dphi_mean[1][1][0][3] = -0.0361657;
PC2_dphi_mean[1][1][0][4] = 0.0211533;
PC2_dphi_mean[1][1][0][5] = -0.0116343;
PC2_dphi_mean[1][1][0][6] = 0.00208388;
PC2_dz_sigma[1][1][0][0] = -0.744272;
PC2_dz_sigma[1][1][0][1] = -0.75519;
PC2_dz_sigma[1][1][0][2] = 1.73276;
PC2_dz_sigma[1][1][0][3] = -0.875431;
PC2_dz_sigma[1][1][0][4] = 0.135866;
PC2_dz_sigma[1][1][0][5] = 1.8133;
PC2_dz_sigma[1][1][0][6] = 0.0494015;
PC2_dz_sigma[1][1][0][7] = 0;
PC2_dz_mean[1][1][0][0] = -1.21822;
PC2_dz_mean[1][1][0][1] = 0.193225;
PC2_dz_mean[1][1][0][2] = -0.486396;
PC2_dz_mean[1][1][0][3] = 3.6087;
PC2_dz_mean[1][1][0][4] = -1.72698;
PC2_dz_mean[1][1][0][5] = 0.962938;
PC2_dz_mean[1][1][0][6] = -0.176525;

  return;
}
