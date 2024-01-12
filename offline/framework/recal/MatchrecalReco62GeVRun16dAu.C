#include "MatchrecalReco62GeVRun16dAu.h"
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

//62GeVRun16dAu PC3 matching recalibrator, adapted from A. Adare's MiniRecal
//A. Sickles

using namespace std;
using namespace findNode;


MatchrecalReco62GeVRun16dAu::MatchrecalReco62GeVRun16dAu(const string &name): 
  Recalibrator(name)
{
  baseclasses.insert("PHCentralTrack");
  initParameters();
}

int MatchrecalReco62GeVRun16dAu::isValidRun(const int runno) const
{
   if (runno < 455792 || runno > 456283)
   {
      return 0;
   }
   return 1;
}

void MatchrecalReco62GeVRun16dAu::help() const
{
  cout << "===================================================================" << endl;
  cout << "MatchrecalReco62GeVRun16dAu::help method output"                    << endl;
  cout << "Qiao Xu(qiao.xu@vanderbilt.edu), Weizhuang Peng(weizhuang.peng@vanderbilt.edu)" << endl;
  cout << "PC3 matching for Run-16 62GeV d+Au " << endl;
  cout << "TOF matching will be added" << endl;
  cout << "Run16 dAu 62 GeV with all the triggers combination" << endl;
  cout << "Central arm tracks with trasverse momentum < 0.2 GeV/c are ignored" << endl;
  cout << "The matching is done in dphi and dz directions, fit with double-Gaussian distribution" << endl;
  cout << endl;
  cout << "===================================================================" << endl;
}

int MatchrecalReco62GeVRun16dAu::process_event(PHCompositeNode *topNode)
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
       
float MatchrecalReco62GeVRun16dAu::GetPC3sdphi(const int iarm, const int icharge, const int ivz, const float pt, const float pc3dphi)
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


float MatchrecalReco62GeVRun16dAu::GetPC3sdz(const int iarm, const int icharge, const int ivz, const float pt, const float pc3dz)
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


float MatchrecalReco62GeVRun16dAu::GetPC2sdphi(const int iarm, const int icharge, const int ivz, const float pt, const float pc2dphi)
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


float MatchrecalReco62GeVRun16dAu::GetPC2sdz(const int iarm, const int icharge, const int ivz, const float pt, const float pc2dz)
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
  if(pt > 3){  float pts = 3.;
      mean = PC2_dz_mean[iarm][jcharge][ivz][0]+PC2_dz_mean[iarm][jcharge][ivz][1]*pts+PC2_dz_mean[iarm][jcharge][ivz][2]/pts+PC2_dz_mean[iarm][jcharge][ivz][3]/sqrt(pts)+PC2_dz_mean[iarm][jcharge][ivz][4]/pts/pts+PC2_dz_mean[iarm][jcharge][ivz][5]/pts/pts/pts+PC2_dz_mean[iarm][jcharge][ivz][6]/pts/pts/pts/pts;
      sigma = PC2_dz_sigma[iarm][jcharge][ivz][0]+PC2_dz_sigma[iarm][jcharge][ivz][1]*pts+PC2_dz_sigma[iarm][jcharge][ivz][2]*pts*pts+PC2_dz_sigma[iarm][jcharge][ivz][3]*pts*pts*pts+PC2_dz_sigma[iarm][jcharge][ivz][4]*pts*pts*pts*pts+PC2_dz_sigma[iarm][jcharge][ivz][5]*pts*pts*pts*pts*pts+PC2_dz_sigma[iarm][jcharge][ivz][6]/sqrt(pts)+PC2_dz_sigma[iarm][jcharge][ivz][7]/pts/pts;
  }
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

void MatchrecalReco62GeVRun16dAu::initParameters()
{
  //
  // Initialization
  //

    PC3_dphifirst_mean[0][0][0] = 0.00510323;
    PC3_dphifirst_sigma[0][0][0] = 0.00976985;
    PC3_dzfirst_mean[0][0][0] = -85.9086;
    PC3_dzfirst_sigma[0][0][0] = 46.704;
    PC3_dphifirst_mean[0][0][1] = -0.00201031;
    PC3_dphifirst_sigma[0][0][1] = 0.00950839;
    PC3_dzfirst_mean[0][0][1] = -4.02618;
    PC3_dzfirst_sigma[0][0][1] = 4.78739;
    PC3_dphifirst_mean[0][0][2] = -0.00274954;
    PC3_dphifirst_sigma[0][0][2] = 0.00891885;
    PC3_dzfirst_mean[0][0][2] = -0.0118144;
    PC3_dzfirst_sigma[0][0][2] = 3.31595;
    PC3_dphifirst_mean[0][0][3] = -0.00367783;
    PC3_dphifirst_sigma[0][0][3] = 0.00895019;
    PC3_dzfirst_mean[0][0][3] = 0.674914;
    PC3_dzfirst_sigma[0][0][3] = 3.37294;
    PC3_dphifirst_mean[0][0][4] = -0.00507157;
    PC3_dphifirst_sigma[0][0][4] = 0.00960833;
    PC3_dzfirst_mean[0][0][4] = 1.23481;
    PC3_dzfirst_sigma[0][0][4] = 3.41346;
    PC3_dphifirst_mean[0][0][5] = -0.00550685;
    PC3_dphifirst_sigma[0][0][5] = 0.0101787;
    PC3_dzfirst_mean[0][0][5] = 2.02413;
    PC3_dzfirst_sigma[0][0][5] = 4.15681;
    PC3_dphifirst_mean[0][0][6] = -0.00551295;
    PC3_dphifirst_sigma[0][0][6] = 0.0113171;
    PC3_dzfirst_mean[0][0][6] = 4.51707;
    PC3_dzfirst_sigma[0][0][6] = 4.61606;
    PC3_dphifirst_mean[0][0][7] = 0.00163041;
    PC3_dphifirst_sigma[0][0][7] = 0.0118729;
    PC3_dzfirst_mean[0][0][7] = 9.4165;
    PC3_dzfirst_sigma[0][0][7] = 19.2579;
    PC3_dphifirst_mean[0][1][0] = -0.00379304;
    PC3_dphifirst_sigma[0][1][0] = 0.00973819;
    PC3_dzfirst_mean[0][1][0] = -94.5648;
    PC3_dzfirst_sigma[0][1][0] = 47.5618;
    PC3_dphifirst_mean[0][1][1] = 0.00322318;
    PC3_dphifirst_sigma[0][1][1] = 0.0094605;
    PC3_dzfirst_mean[0][1][1] = -3.89751;
    PC3_dzfirst_sigma[0][1][1] = 4.63176;
    PC3_dphifirst_mean[0][1][2] = 0.0040337;
    PC3_dphifirst_sigma[0][1][2] = 0.00883683;
    PC3_dzfirst_mean[0][1][2] = 0.12632;
    PC3_dzfirst_sigma[0][1][2] = 3.21371;
    PC3_dphifirst_mean[0][1][3] = 0.00528674;
    PC3_dphifirst_sigma[0][1][3] = 0.00901953;
    PC3_dzfirst_mean[0][1][3] = 0.857531;
    PC3_dzfirst_sigma[0][1][3] = 3.21969;
    PC3_dphifirst_mean[0][1][4] = 0.00778369;
    PC3_dphifirst_sigma[0][1][4] = 0.00983455;
    PC3_dzfirst_mean[0][1][4] = 1.50673;
    PC3_dzfirst_sigma[0][1][4] = 3.47545;
    PC3_dphifirst_mean[0][1][5] = 0.0201039;
    PC3_dphifirst_sigma[0][1][5] = 0.0214213;
    PC3_dzfirst_mean[0][1][5] = 2.28426;
    PC3_dzfirst_sigma[0][1][5] = 4.0531;
    PC3_dphifirst_mean[0][1][6] = 0.0111993;
    PC3_dphifirst_sigma[0][1][6] = 0.0115119;
    PC3_dzfirst_mean[0][1][6] = 4.78578;
    PC3_dzfirst_sigma[0][1][6] = 4.39285;
    PC3_dphifirst_mean[0][1][7] = 0.017471;
    PC3_dphifirst_sigma[0][1][7] = 0.0254877;
    PC3_dzfirst_mean[0][1][7] = 8.81275;
    PC3_dzfirst_sigma[0][1][7] = 13.645;
    PC3_dphifirst_mean[1][0][0] = 0.00138514;
    PC3_dphifirst_sigma[1][0][0] = 0.0106854;
    PC3_dzfirst_mean[1][0][0] = -64.8397;
    PC3_dzfirst_sigma[1][0][0] = 32.3281;
    PC3_dphifirst_mean[1][0][1] = -0.00553466;
    PC3_dphifirst_sigma[1][0][1] = 0.0103405;
    PC3_dzfirst_mean[1][0][1] = -3.21976;
    PC3_dzfirst_sigma[1][0][1] = 4.93573;
    PC3_dphifirst_mean[1][0][2] = -0.00626524;
    PC3_dphifirst_sigma[1][0][2] = 0.00977875;
    PC3_dzfirst_mean[1][0][2] = 0.834823;
    PC3_dzfirst_sigma[1][0][2] = 3.39373;
    PC3_dphifirst_mean[1][0][3] = -0.00648903;
    PC3_dphifirst_sigma[1][0][3] = 0.00948241;
    PC3_dzfirst_mean[1][0][3] = 1.60964;
    PC3_dzfirst_sigma[1][0][3] = 3.504;
    PC3_dphifirst_mean[1][0][4] = -0.00646786;
    PC3_dphifirst_sigma[1][0][4] = 0.00942376;
    PC3_dzfirst_mean[1][0][4] = 2.22952;
    PC3_dzfirst_sigma[1][0][4] = 3.8592;
    PC3_dphifirst_mean[1][0][5] = -0.00629278;
    PC3_dphifirst_sigma[1][0][5] = 0.00989191;
    PC3_dzfirst_mean[1][0][5] = 3.05462;
    PC3_dzfirst_sigma[1][0][5] = 4.10901;
    PC3_dphifirst_mean[1][0][6] = -0.00573174;
    PC3_dphifirst_sigma[1][0][6] = 0.0106984;
    PC3_dzfirst_mean[1][0][6] = 5.64375;
    PC3_dzfirst_sigma[1][0][6] = 4.36771;
    PC3_dphifirst_mean[1][0][7] = 0.00284857;
    PC3_dphifirst_sigma[1][0][7] = 0.0098925;
    PC3_dzfirst_mean[1][0][7] = 18.93;
    PC3_dzfirst_sigma[1][0][7] = 27.8167;
    PC3_dphifirst_mean[1][1][0] = -0.00287123;
    PC3_dphifirst_sigma[1][1][0] = 0.00959898;
    PC3_dzfirst_mean[1][1][0] = -81.1334;
    PC3_dzfirst_sigma[1][1][0] = 21.8106;
    PC3_dphifirst_mean[1][1][1] = 0.00512356;
    PC3_dphifirst_sigma[1][1][1] = 0.010281;
    PC3_dzfirst_mean[1][1][1] = -3.3697;
    PC3_dzfirst_sigma[1][1][1] = 5.19013;
    PC3_dphifirst_mean[1][1][2] = 0.00563331;
    PC3_dphifirst_sigma[1][1][2] = 0.00986738;
    PC3_dzfirst_mean[1][1][2] = 0.633483;
    PC3_dzfirst_sigma[1][1][2] = 3.49986;
    PC3_dphifirst_mean[1][1][3] = 0.00595152;
    PC3_dphifirst_sigma[1][1][3] = 0.010053;
    PC3_dzfirst_mean[1][1][3] = 1.31557;
    PC3_dzfirst_sigma[1][1][3] = 3.55156;
    PC3_dphifirst_mean[1][1][4] = 0.00537618;
    PC3_dphifirst_sigma[1][1][4] = 0.0100525;
    PC3_dzfirst_mean[1][1][4] = 2.02911;
    PC3_dzfirst_sigma[1][1][4] = 3.94468;
    PC3_dphifirst_mean[1][1][5] = 0.00431289;
    PC3_dphifirst_sigma[1][1][5] = 0.0100669;
    PC3_dzfirst_mean[1][1][5] = 2.83778;
    PC3_dzfirst_sigma[1][1][5] = 4.23018;
    PC3_dphifirst_mean[1][1][6] = 0.0029838;
    PC3_dphifirst_sigma[1][1][6] = 0.0102131;
    PC3_dzfirst_mean[1][1][6] = 5.40571;
    PC3_dzfirst_sigma[1][1][6] = 4.43371;
    PC3_dphifirst_mean[1][1][7] = -0.00391794;
    PC3_dphifirst_sigma[1][1][7] = 0.00946602;
    PC3_dzfirst_mean[1][1][7] = 25.2009;
    PC3_dzfirst_sigma[1][1][7] = 19.1198;
    PC3_dphi_sigma[0][0][0][0] = 0.286067;
    PC3_dphi_sigma[0][0][0][1] = -0.68881;
    PC3_dphi_sigma[0][0][0][2] = 0.829882;
    PC3_dphi_sigma[0][0][0][3] = -0.496526;
    PC3_dphi_sigma[0][0][0][4] = 0.138618;
    PC3_dphi_sigma[0][0][0][5] = -0.0137485;
    PC3_dphi_sigma[0][0][0][6] = -0.0471436;
    PC3_dphi_sigma[0][0][0][7] = -0.00560345;
    PC3_dphi_mean[0][0][0][0] = -0.782978;
    PC3_dphi_mean[0][0][0][1] = 0.0421137;
    PC3_dphi_mean[0][0][0][2] = -1.71961;
    PC3_dphi_mean[0][0][0][3] = 2.03023;
    PC3_dphi_mean[0][0][0][4] = 0.568821;
    PC3_dphi_mean[0][0][0][5] = -0.1563;
    PC3_dphi_mean[0][0][0][6] = 0.0190835;
    PC3_dz_sigma[0][0][0][0] = 1.76997;
    PC3_dz_sigma[0][0][0][1] = 0.914962;
    PC3_dz_sigma[0][0][0][2] = -0.145345;
    PC3_dz_sigma[0][0][0][3] = -0.840383;
    PC3_dz_sigma[0][0][0][4] = 0.136274;
    PC3_dz_sigma[0][0][0][5] = 0.0151138;
    PC3_dz_sigma[0][0][0][6] = 2.09417;
    PC3_dz_sigma[0][0][0][7] = 2.76494;
    PC3_dz_mean[0][0][0][0] = -879.064;
    PC3_dz_mean[0][0][0][1] = 82.7154;
    PC3_dz_mean[0][0][0][2] = 580.207;
    PC3_dz_mean[0][0][0][3] = 1008.57;
    PC3_dz_mean[0][0][0][4] = -1700.81;
    PC3_dz_mean[0][0][0][5] = 1043.75;
    PC3_dz_mean[0][0][0][6] = -205.64;
    PC3_dphi_sigma[0][0][1][0] = -0.00955853;
    PC3_dphi_sigma[0][0][1][1] = 0.0110375;
    PC3_dphi_sigma[0][0][1][2] = -0.00763875;
    PC3_dphi_sigma[0][0][1][3] = 0.00287069;
    PC3_dphi_sigma[0][0][1][4] = -0.000531097;
    PC3_dphi_sigma[0][0][1][5] = 3.76536e-05;
    PC3_dphi_sigma[0][0][1][6] = 0.00531969;
    PC3_dphi_sigma[0][0][1][7] = 0.000526436;
    PC3_dphi_mean[0][0][1][0] = -0.000820734;
    PC3_dphi_mean[0][0][1][1] = 0.000339252;
    PC3_dphi_mean[0][0][1][2] = 0.00022735;
    PC3_dphi_mean[0][0][1][3] = 0.00213177;
    PC3_dphi_mean[0][0][1][4] = -0.00132977;
    PC3_dphi_mean[0][0][1][5] = 0.000670621;
    PC3_dphi_mean[0][0][1][6] = -0.000154171;
    PC3_dz_sigma[0][0][1][0] = -49.1366;
    PC3_dz_sigma[0][0][1][1] = 43.2282;
    PC3_dz_sigma[0][0][1][2] = -27.2624;
    PC3_dz_sigma[0][0][1][3] = 10.0015;
    PC3_dz_sigma[0][0][1][4] = -1.90126;
    PC3_dz_sigma[0][0][1][5] = 0.142777;
    PC3_dz_sigma[0][0][1][6] = 28.7185;
    PC3_dz_sigma[0][0][1][7] = -1.01493;
    PC3_dz_mean[0][0][1][0] = 260.684;
    PC3_dz_mean[0][0][1][1] = -15.5202;
    PC3_dz_mean[0][0][1][2] = 538.737;
    PC3_dz_mean[0][0][1][3] = -656.534;
    PC3_dz_mean[0][0][1][4] = -169.521;
    PC3_dz_mean[0][0][1][5] = 44.7973;
    PC3_dz_mean[0][0][1][6] = -5.3186;
    PC3_dphi_sigma[0][0][2][0] = 0.00865383;
    PC3_dphi_sigma[0][0][2][1] = -0.00633941;
    PC3_dphi_sigma[0][0][2][2] = 0.00389285;
    PC3_dphi_sigma[0][0][2][3] = -0.00131353;
    PC3_dphi_sigma[0][0][2][4] = 0.000211718;
    PC3_dphi_sigma[0][0][2][5] = -1.15052e-05;
    PC3_dphi_sigma[0][0][2][6] = -0.00380068;
    PC3_dphi_sigma[0][0][2][7] = 0.000748811;
    PC3_dphi_mean[0][0][2][0] = -0.00107872;
    PC3_dphi_mean[0][0][2][1] = 0.000329894;
    PC3_dphi_mean[0][0][2][2] = -4.30167e-05;
    PC3_dphi_mean[0][0][2][3] = 0.00280095;
    PC3_dphi_mean[0][0][2][4] = -0.00164927;
    PC3_dphi_mean[0][0][2][5] = 0.00091895;
    PC3_dphi_mean[0][0][2][6] = -0.00020934;
    PC3_dz_sigma[0][0][2][0] = -3.61684;
    PC3_dz_sigma[0][0][2][1] = 3.48142;
    PC3_dz_sigma[0][0][2][2] = -2.26643;
    PC3_dz_sigma[0][0][2][3] = 0.938674;
    PC3_dz_sigma[0][0][2][4] = -0.200853;
    PC3_dz_sigma[0][0][2][5] = 0.0167207;
    PC3_dz_sigma[0][0][2][6] = 3.2309;
    PC3_dz_sigma[0][0][2][7] = -0.057676;
    PC3_dz_mean[0][0][2][0] = 2.91071;
    PC3_dz_mean[0][0][2][1] = -0.175438;
    PC3_dz_mean[0][0][2][2] = -0.0946224;
    PC3_dz_mean[0][0][2][3] = -2.68387;
    PC3_dz_mean[0][0][2][4] = 1.48759;
    PC3_dz_mean[0][0][2][5] = -0.839981;
    PC3_dz_mean[0][0][2][6] = 0.150703;
    PC3_dphi_sigma[0][0][3][0] = -0.00303834;
    PC3_dphi_sigma[0][0][3][1] = 0.00330439;
    PC3_dphi_sigma[0][0][3][2] = -0.00191346;
    PC3_dphi_sigma[0][0][3][3] = 0.000691801;
    PC3_dphi_sigma[0][0][3][4] = -0.000145409;
    PC3_dphi_sigma[0][0][3][5] = 1.37349e-05;
    PC3_dphi_sigma[0][0][3][6] = 0.00263121;
    PC3_dphi_sigma[0][0][3][7] = 0.000496861;
    PC3_dphi_mean[0][0][3][0] = -0.0192807;
    PC3_dphi_mean[0][0][3][1] = 0.00131771;
    PC3_dphi_mean[0][0][3][2] = -0.0401916;
    PC3_dphi_mean[0][0][3][3] = 0.0499835;
    PC3_dphi_mean[0][0][3][4] = 0.0117829;
    PC3_dphi_mean[0][0][3][5] = -0.00280383;
    PC3_dphi_mean[0][0][3][6] = 0.000242757;
    PC3_dz_sigma[0][0][3][0] = 0.087572;
    PC3_dz_sigma[0][0][3][1] = -0.0410396;
    PC3_dz_sigma[0][0][3][2] = 0.286235;
    PC3_dz_sigma[0][0][3][3] = -0.118296;
    PC3_dz_sigma[0][0][3][4] = 0.0196118;
    PC3_dz_sigma[0][0][3][5] = -0.00100819;
    PC3_dz_sigma[0][0][3][6] = 1.3395;
    PC3_dz_sigma[0][0][3][7] = 0.00567294;
    PC3_dz_mean[0][0][3][0] = 28.3637;
    PC3_dz_mean[0][0][3][1] = -1.47101;
    PC3_dz_mean[0][0][3][2] = 56.8001;
    PC3_dz_mean[0][0][3][3] = -68.9146;
    PC3_dz_mean[0][0][3][4] = -17.706;
    PC3_dz_mean[0][0][3][5] = 4.50421;
    PC3_dz_mean[0][0][3][6] = -0.505617;
    PC3_dphi_sigma[0][0][4][0] = 0.00174919;
    PC3_dphi_sigma[0][0][4][1] = -2.4348e-06;
    PC3_dphi_sigma[0][0][4][2] = -0.000304737;
    PC3_dphi_sigma[0][0][4][3] = 0.000264;
    PC3_dphi_sigma[0][0][4][4] = -9.2017e-05;
    PC3_dphi_sigma[0][0][4][5] = 1.16661e-05;
    PC3_dphi_sigma[0][0][4][6] = -0.000211615;
    PC3_dphi_sigma[0][0][4][7] = 0.000635071;
    PC3_dphi_mean[0][0][4][0] = -0.0159901;
    PC3_dphi_mean[0][0][4][1] = 0.00116521;
    PC3_dphi_mean[0][0][4][2] = -0.0320925;
    PC3_dphi_mean[0][0][4][3] = 0.0408936;
    PC3_dphi_mean[0][0][4][4] = 0.00892423;
    PC3_dphi_mean[0][0][4][5] = -0.00202105;
    PC3_dphi_mean[0][0][4][6] = 0.000147858;
    PC3_dz_sigma[0][0][4][0] = 0.527179;
    PC3_dz_sigma[0][0][4][1] = -0.168087;
    PC3_dz_sigma[0][0][4][2] = 0.134626;
    PC3_dz_sigma[0][0][4][3] = 0.0279183;
    PC3_dz_sigma[0][0][4][4] = -0.0224748;
    PC3_dz_sigma[0][0][4][5] = 0.00298523;
    PC3_dz_sigma[0][0][4][6] = 1.066;
    PC3_dz_sigma[0][0][4][7] = 0.019977;
    PC3_dz_mean[0][0][4][0] = 16.6562;
    PC3_dz_mean[0][0][4][1] = -0.79801;
    PC3_dz_mean[0][0][4][2] = 32.3005;
    PC3_dz_mean[0][0][4][3] = -39.1733;
    PC3_dz_mean[0][0][4][4] = -10.0708;
    PC3_dz_mean[0][0][4][5] = 2.57075;
    PC3_dz_mean[0][0][4][6] = -0.28923;
    PC3_dphi_sigma[0][0][5][0] = -0.00426502;
    PC3_dphi_sigma[0][0][5][1] = 0.00542177;
    PC3_dphi_sigma[0][0][5][2] = -0.00369087;
    PC3_dphi_sigma[0][0][5][3] = 0.0014516;
    PC3_dphi_sigma[0][0][5][4] = -0.000304877;
    PC3_dphi_sigma[0][0][5][5] = 2.66922e-05;
    PC3_dphi_sigma[0][0][5][6] = 0.00282342;
    PC3_dphi_sigma[0][0][5][7] = 0.00058924;
    PC3_dphi_mean[0][0][5][0] = -0.0158074;
    PC3_dphi_mean[0][0][5][1] = 0.00113737;
    PC3_dphi_mean[0][0][5][2] = -0.0320517;
    PC3_dphi_mean[0][0][5][3] = 0.0406686;
    PC3_dphi_mean[0][0][5][4] = 0.00893756;
    PC3_dphi_mean[0][0][5][5] = -0.002047;
    PC3_dphi_mean[0][0][5][6] = 0.000148376;
    PC3_dz_sigma[0][0][5][0] = 9.32332;
    PC3_dz_sigma[0][0][5][1] = -6.3013;
    PC3_dz_sigma[0][0][5][2] = 3.3859;
    PC3_dz_sigma[0][0][5][3] = -0.963677;
    PC3_dz_sigma[0][0][5][4] = 0.129768;
    PC3_dz_sigma[0][0][5][5] = -0.00600536;
    PC3_dz_sigma[0][0][5][6] = -4.18802;
    PC3_dz_sigma[0][0][5][7] = 0.304262;
    PC3_dz_mean[0][0][5][0] = 24.7432;
    PC3_dz_mean[0][0][5][1] = -1.1694;
    PC3_dz_mean[0][0][5][2] = 51.0118;
    PC3_dz_mean[0][0][5][3] = -60.3907;
    PC3_dz_mean[0][0][5][4] = -16.64;
    PC3_dz_mean[0][0][5][5] = 4.50045;
    PC3_dz_mean[0][0][5][6] = -0.537955;
    PC3_dphi_sigma[0][0][6][0] = 0.0120897;
    PC3_dphi_sigma[0][0][6][1] = -0.00623212;
    PC3_dphi_sigma[0][0][6][2] = 0.00265447;
    PC3_dphi_sigma[0][0][6][3] = -0.000700402;
    PC3_dphi_sigma[0][0][6][4] = 0.00010857;
    PC3_dphi_sigma[0][0][6][5] = -7.21319e-06;
    PC3_dphi_sigma[0][0][6][6] = -0.00694164;
    PC3_dphi_sigma[0][0][6][7] = 0.00110911;
    PC3_dphi_mean[0][0][6][0] = -0.000711859;
    PC3_dphi_mean[0][0][6][1] = 0.000285358;
    PC3_dphi_mean[0][0][6][2] = 2.64914e-05;
    PC3_dphi_mean[0][0][6][3] = 0.00225902;
    PC3_dphi_mean[0][0][6][4] = -0.00138959;
    PC3_dphi_mean[0][0][6][5] = 0.000668003;
    PC3_dphi_mean[0][0][6][6] = -0.000171409;
    PC3_dz_sigma[0][0][6][0] = -40.2359;
    PC3_dz_sigma[0][0][6][1] = 33.0605;
    PC3_dz_sigma[0][0][6][2] = -19.7127;
    PC3_dz_sigma[0][0][6][3] = 6.82648;
    PC3_dz_sigma[0][0][6][4] = -1.21701;
    PC3_dz_sigma[0][0][6][5] = 0.0850599;
    PC3_dz_sigma[0][0][6][6] = 24.3022;
    PC3_dz_sigma[0][0][6][7] = -0.88946;
    PC3_dz_mean[0][0][6][0] = 359.107;
    PC3_dz_mean[0][0][6][1] = -21.151;
    PC3_dz_mean[0][0][6][2] = 732.916;
    PC3_dz_mean[0][0][6][3] = -889.3;
    PC3_dz_mean[0][0][6][4] = -232.906;
    PC3_dz_mean[0][0][6][5] = 62.2699;
    PC3_dz_mean[0][0][6][6] = -7.46458;
    PC3_dphi_sigma[0][0][7][0] = 0.314449;
    PC3_dphi_sigma[0][0][7][1] = -0.247609;
    PC3_dphi_sigma[0][0][7][2] = 0.144931;
    PC3_dphi_sigma[0][0][7][3] = -0.0496344;
    PC3_dphi_sigma[0][0][7][4] = 0.00887704;
    PC3_dphi_sigma[0][0][7][5] = -0.000629204;
    PC3_dphi_sigma[0][0][7][6] = -0.176126;
    PC3_dphi_sigma[0][0][7][7] = 0.00840972;
    PC3_dphi_mean[0][0][7][0] = -0.0165221;
    PC3_dphi_mean[0][0][7][1] = 0.00275667;
    PC3_dphi_mean[0][0][7][2] = 0.00654987;
    PC3_dphi_mean[0][0][7][3] = 0.0162621;
    PC3_dphi_mean[0][0][7][4] = -0.0123092;
    PC3_dphi_mean[0][0][7][5] = 0.00503183;
    PC3_dphi_mean[0][0][7][6] = -0.000822442;
    PC3_dz_sigma[0][0][7][0] = 297.333;
    PC3_dz_sigma[0][0][7][1] = 333.228;
    PC3_dz_sigma[0][0][7][2] = -598.129;
    PC3_dz_sigma[0][0][7][3] = 338.697;
    PC3_dz_sigma[0][0][7][4] = -79.2633;
    PC3_dz_sigma[0][0][7][5] = 6.5692;
    PC3_dz_sigma[0][0][7][6] = -322.946;
    PC3_dz_sigma[0][0][7][7] = 32.1385;
    PC3_dz_mean[0][0][7][0] = 984.665;
    PC3_dz_mean[0][0][7][1] = -82.6417;
    PC3_dz_mean[0][0][7][2] = 204.098;
    PC3_dz_mean[0][0][7][3] = -1478.57;
    PC3_dz_mean[0][0][7][4] = 717.274;
    PC3_dz_mean[0][0][7][5] = -419.803;
    PC3_dz_mean[0][0][7][6] = 77.6372;
    PC3_dphi_sigma[0][1][0][0] = -0.0966672;
    PC3_dphi_sigma[0][1][0][1] = 0.0104218;
    PC3_dphi_sigma[0][1][0][2] = 0.0488512;
    PC3_dphi_sigma[0][1][0][3] = -0.0422652;
    PC3_dphi_sigma[0][1][0][4] = 0.0133562;
    PC3_dphi_sigma[0][1][0][5] = -0.00143398;
    PC3_dphi_sigma[0][1][0][6] = 0.0740873;
    PC3_dphi_sigma[0][1][0][7] = -0.00419907;
    PC3_dphi_mean[0][1][0][0] = 0.197158;
    PC3_dphi_mean[0][1][0][1] = -0.0304507;
    PC3_dphi_mean[0][1][0][2] = -0.0460828;
    PC3_dphi_mean[0][1][0][3] = -0.193802;
    PC3_dphi_mean[0][1][0][4] = 0.118465;
    PC3_dphi_mean[0][1][0][5] = -0.0526417;
    PC3_dphi_mean[0][1][0][6] = 0.0082514;
    PC3_dz_sigma[0][1][0][0] = 1846.98;
    PC3_dz_sigma[0][1][0][1] = -1258.83;
    PC3_dz_sigma[0][1][0][2] = 608.721;
    PC3_dz_sigma[0][1][0][3] = -167.581;
    PC3_dz_sigma[0][1][0][4] = 23.9916;
    PC3_dz_sigma[0][1][0][5] = -1.38596;
    PC3_dz_sigma[0][1][0][6] = -1102.47;
    PC3_dz_sigma[0][1][0][7] = 53.3478;
    PC3_dz_mean[0][1][0][0] = -8337.7;
    PC3_dz_mean[0][1][0][1] = 298.265;
    PC3_dz_mean[0][1][0][2] = -26650.9;
    PC3_dz_mean[0][1][0][3] = 26253.6;
    PC3_dz_mean[0][1][0][4] = 11881.3;
    PC3_dz_mean[0][1][0][5] = -4126.86;
    PC3_dz_mean[0][1][0][6] = 609.792;
    PC3_dphi_sigma[0][1][1][0] = -5.71941e-05;
    PC3_dphi_sigma[0][1][1][1] = 0.00650106;
    PC3_dphi_sigma[0][1][1][2] = -0.00659767;
    PC3_dphi_sigma[0][1][1][3] = 0.00302441;
    PC3_dphi_sigma[0][1][1][4] = -0.000648372;
    PC3_dphi_sigma[0][1][1][5] = 5.22469e-05;
    PC3_dphi_sigma[0][1][1][6] = -0.00105816;
    PC3_dphi_sigma[0][1][1][7] = 0.000810318;
    PC3_dphi_mean[0][1][1][0] = -7.82946e-05;
    PC3_dphi_mean[0][1][1][1] = 9.52512e-05;
    PC3_dphi_mean[0][1][1][2] = 0.000143416;
    PC3_dphi_mean[0][1][1][3] = 0.00115433;
    PC3_dphi_mean[0][1][1][4] = -0.000676155;
    PC3_dphi_mean[0][1][1][5] = 0.000422889;
    PC3_dphi_mean[0][1][1][6] = -6.04577e-05;
    PC3_dz_sigma[0][1][1][0] = -11.3755;
    PC3_dz_sigma[0][1][1][1] = 11.0686;
    PC3_dz_sigma[0][1][1][2] = -6.99015;
    PC3_dz_sigma[0][1][1][3] = 2.65612;
    PC3_dz_sigma[0][1][1][4] = -0.529538;
    PC3_dz_sigma[0][1][1][5] = 0.0411886;
    PC3_dz_sigma[0][1][1][6] = 8.07506;
    PC3_dz_sigma[0][1][1][7] = -0.228722;
    PC3_dz_mean[0][1][1][0] = -11.181;
    PC3_dz_mean[0][1][1][1] = 1.22418;
    PC3_dz_mean[0][1][1][2] = -0.121999;
    PC3_dz_mean[0][1][1][3] = 9.99675;
    PC3_dz_mean[0][1][1][4] = -4.24202;
    PC3_dz_mean[0][1][1][5] = 1.94616;
    PC3_dz_mean[0][1][1][6] = -0.308219;
    PC3_dphi_sigma[0][1][2][0] = 0.0134097;
    PC3_dphi_sigma[0][1][2][1] = -0.00608837;
    PC3_dphi_sigma[0][1][2][2] = 0.00172967;
    PC3_dphi_sigma[0][1][2][3] = -4.84929e-05;
    PC3_dphi_sigma[0][1][2][4] = -8.18547e-05;
    PC3_dphi_sigma[0][1][2][5] = 1.29831e-05;
    PC3_dphi_sigma[0][1][2][6] = -0.00792821;
    PC3_dphi_sigma[0][1][2][7] = 0.00101143;
    PC3_dphi_mean[0][1][2][0] = 0.0229086;
    PC3_dphi_mean[0][1][2][1] = -0.00123469;
    PC3_dphi_mean[0][1][2][2] = 0.0496788;
    PC3_dphi_mean[0][1][2][3] = -0.0575714;
    PC3_dphi_mean[0][1][2][4] = -0.0171854;
    PC3_dphi_mean[0][1][2][5] = 0.00501127;
    PC3_dphi_mean[0][1][2][6] = -0.000616457;
    PC3_dz_sigma[0][1][2][0] = -12.2194;
    PC3_dz_sigma[0][1][2][1] = 10.4023;
    PC3_dz_sigma[0][1][2][2] = -6.14045;
    PC3_dz_sigma[0][1][2][3] = 2.13832;
    PC3_dz_sigma[0][1][2][4] = -0.385762;
    PC3_dz_sigma[0][1][2][5] = 0.0274618;
    PC3_dz_sigma[0][1][2][6] = 7.90058;
    PC3_dz_sigma[0][1][2][7] = -0.228302;
    PC3_dz_mean[0][1][2][0] = 1.93849;
    PC3_dz_mean[0][1][2][1] = -0.0633773;
    PC3_dz_mean[0][1][2][2] = 0.0626183;
    PC3_dz_mean[0][1][2][3] = -1.54764;
    PC3_dz_mean[0][1][2][4] = 0.776211;
    PC3_dz_mean[0][1][2][5] = -0.496932;
    PC3_dz_mean[0][1][2][6] = 0.0938928;
    PC3_dphi_sigma[0][1][3][0] = 0.00422282;
    PC3_dphi_sigma[0][1][3][1] = 0.00210572;
    PC3_dphi_sigma[0][1][3][2] = -0.00354556;
    PC3_dphi_sigma[0][1][3][3] = 0.00191309;
    PC3_dphi_sigma[0][1][3][4] = -0.000462779;
    PC3_dphi_sigma[0][1][3][5] = 4.26468e-05;
    PC3_dphi_sigma[0][1][3][6] = -0.00313272;
    PC3_dphi_sigma[0][1][3][7] = 0.000871505;
    PC3_dphi_mean[0][1][3][0] = 0.024225;
    PC3_dphi_mean[0][1][3][1] = -0.00130348;
    PC3_dphi_mean[0][1][3][2] = 0.0533719;
    PC3_dphi_mean[0][1][3][3] = -0.0614046;
    PC3_dphi_mean[0][1][3][4] = -0.0187772;
    PC3_dphi_mean[0][1][3][5] = 0.00555299;
    PC3_dphi_mean[0][1][3][6] = -0.000682692;
    PC3_dz_sigma[0][1][3][0] = -5.6159;
    PC3_dz_sigma[0][1][3][1] = 5.26695;
    PC3_dz_sigma[0][1][3][2] = -3.25038;
    PC3_dz_sigma[0][1][3][3] = 1.23662;
    PC3_dz_sigma[0][1][3][4] = -0.250596;
    PC3_dz_sigma[0][1][3][5] = 0.0204173;
    PC3_dz_sigma[0][1][3][6] = 4.21355;
    PC3_dz_sigma[0][1][3][7] = -0.0764278;
    PC3_dz_mean[0][1][3][0] = 2.19108;
    PC3_dz_mean[0][1][3][1] = -0.0320725;
    PC3_dz_mean[0][1][3][2] = 0.107941;
    PC3_dz_mean[0][1][3][3] = -1.62854;
    PC3_dz_mean[0][1][3][4] = 0.887031;
    PC3_dz_mean[0][1][3][5] = -0.556645;
    PC3_dz_mean[0][1][3][6] = 0.105822;
    PC3_dphi_sigma[0][1][4][0] = 0.0127441;
    PC3_dphi_sigma[0][1][4][1] = -0.00466434;
    PC3_dphi_sigma[0][1][4][2] = 0.000453665;
    PC3_dphi_sigma[0][1][4][3] = 0.000531137;
    PC3_dphi_sigma[0][1][4][4] = -0.000211776;
    PC3_dphi_sigma[0][1][4][5] = 2.41874e-05;
    PC3_dphi_sigma[0][1][4][6] = -0.00795572;
    PC3_dphi_sigma[0][1][4][7] = 0.00108804;
    PC3_dphi_mean[0][1][4][0] = 0.0123314;
    PC3_dphi_mean[0][1][4][1] = -0.000646126;
    PC3_dphi_mean[0][1][4][2] = 0.026967;
    PC3_dphi_mean[0][1][4][3] = -0.0305327;
    PC3_dphi_mean[0][1][4][4] = -0.00984628;
    PC3_dphi_mean[0][1][4][5] = 0.00308112;
    PC3_dphi_mean[0][1][4][6] = -0.000373895;
    PC3_dz_sigma[0][1][4][0] = 0.876397;
    PC3_dz_sigma[0][1][4][1] = -0.00540212;
    PC3_dz_sigma[0][1][4][2] = -0.0852543;
    PC3_dz_sigma[0][1][4][3] = 0.126496;
    PC3_dz_sigma[0][1][4][4] = -0.0440074;
    PC3_dz_sigma[0][1][4][5] = 0.0047731;
    PC3_dz_sigma[0][1][4][6] = 0.601859;
    PC3_dz_sigma[0][1][4][7] = 0.0741046;
    PC3_dz_mean[0][1][4][0] = 17.5396;
    PC3_dz_mean[0][1][4][1] = -0.815074;
    PC3_dz_mean[0][1][4][2] = 35.8966;
    PC3_dz_mean[0][1][4][3] = -42.3953;
    PC3_dz_mean[0][1][4][4] = -11.8474;
    PC3_dz_mean[0][1][4][5] = 3.21115;
    PC3_dz_mean[0][1][4][6] = -0.384826;
    PC3_dphi_sigma[0][1][5][0] = 0.0204436;
    PC3_dphi_sigma[0][1][5][1] = -0.0100785;
    PC3_dphi_sigma[0][1][5][2] = 0.00341329;
    PC3_dphi_sigma[0][1][5][3] = -0.000445817;
    PC3_dphi_sigma[0][1][5][4] = -3.8996e-05;
    PC3_dphi_sigma[0][1][5][5] = 1.18913e-05;
    PC3_dphi_sigma[0][1][5][6] = -0.0126787;
    PC3_dphi_sigma[0][1][5][7] = 0.0013818;
    PC3_dphi_mean[0][1][5][0] = 0.0197836;
    PC3_dphi_mean[0][1][5][1] = -0.00103999;
    PC3_dphi_mean[0][1][5][2] = 0.0442002;
    PC3_dphi_mean[0][1][5][3] = -0.0503159;
    PC3_dphi_mean[0][1][5][4] = -0.015847;
    PC3_dphi_mean[0][1][5][5] = 0.0048052;
    PC3_dphi_mean[0][1][5][6] = -0.000577357;
    PC3_dz_sigma[0][1][5][0] = -3.16622;
    PC3_dz_sigma[0][1][5][1] = 3.95217;
    PC3_dz_sigma[0][1][5][2] = -2.67749;
    PC3_dz_sigma[0][1][5][3] = 1.08884;
    PC3_dz_sigma[0][1][5][4] = -0.229868;
    PC3_dz_sigma[0][1][5][5] = 0.0190095;
    PC3_dz_sigma[0][1][5][6] = 2.64403;
    PC3_dz_sigma[0][1][5][7] = 0.0405473;
    PC3_dz_mean[0][1][5][0] = 17.4903;
    PC3_dz_mean[0][1][5][1] = -0.79499;
    PC3_dz_mean[0][1][5][2] = 34.3722;
    PC3_dz_mean[0][1][5][3] = -41.1968;
    PC3_dz_mean[0][1][5][4] = -10.877;
    PC3_dz_mean[0][1][5][5] = 2.83947;
    PC3_dz_mean[0][1][5][6] = -0.325929;
    PC3_dphi_sigma[0][1][6][0] = 0.0324161;
    PC3_dphi_sigma[0][1][6][1] = -0.0184285;
    PC3_dphi_sigma[0][1][6][2] = 0.00811809;
    PC3_dphi_sigma[0][1][6][3] = -0.00214795;
    PC3_dphi_sigma[0][1][6][4] = 0.000312963;
    PC3_dphi_sigma[0][1][6][5] = -1.92303e-05;
    PC3_dphi_sigma[0][1][6][6] = -0.0200847;
    PC3_dphi_sigma[0][1][6][7] = 0.00184014;
    PC3_dphi_mean[0][1][6][0] = 0.0294756;
    PC3_dphi_mean[0][1][6][1] = -0.00147419;
    PC3_dphi_mean[0][1][6][2] = 0.0694763;
    PC3_dphi_mean[0][1][6][3] = -0.0777591;
    PC3_dphi_mean[0][1][6][4] = -0.0255215;
    PC3_dphi_mean[0][1][6][5] = 0.00780084;
    PC3_dphi_mean[0][1][6][6] = -0.000965;
    PC3_dz_sigma[0][1][6][0] = -2.20165;
    PC3_dz_sigma[0][1][6][1] = 2.12686;
    PC3_dz_sigma[0][1][6][2] = -0.758437;
    PC3_dz_sigma[0][1][6][3] = 0.0457049;
    PC3_dz_sigma[0][1][6][4] = 0.041634;
    PC3_dz_sigma[0][1][6][5] = -0.00751225;
    PC3_dz_sigma[0][1][6][6] = 2.9518;
    PC3_dz_sigma[0][1][6][7] = -0.0163848;
    PC3_dz_mean[0][1][6][0] = -276.517;
    PC3_dz_mean[0][1][6][1] = 16.3655;
    PC3_dz_mean[0][1][6][2] = -585.586;
    PC3_dz_mean[0][1][6][3] = 705.114;
    PC3_dz_mean[0][1][6][4] = 189.109;
    PC3_dz_mean[0][1][6][5] = -51.2585;
    PC3_dz_mean[0][1][6][6] = 6.21999;
    PC3_dphi_sigma[0][1][7][0] = 0.143403;
    PC3_dphi_sigma[0][1][7][1] = -0.129162;
    PC3_dphi_sigma[0][1][7][2] = 0.0865106;
    PC3_dphi_sigma[0][1][7][3] = -0.032816;
    PC3_dphi_sigma[0][1][7][4] = 0.00624188;
    PC3_dphi_sigma[0][1][7][5] = -0.000456547;
    PC3_dphi_sigma[0][1][7][6] = -0.0754294;
    PC3_dphi_sigma[0][1][7][7] = 0.00391096;
    PC3_dphi_mean[0][1][7][0] = 0.516921;
    PC3_dphi_mean[0][1][7][1] = -0.0273395;
    PC3_dphi_mean[0][1][7][2] = 1.1168;
    PC3_dphi_mean[0][1][7][3] = -1.32926;
    PC3_dphi_mean[0][1][7][4] = -0.361875;
    PC3_dphi_mean[0][1][7][5] = 0.0970662;
    PC3_dphi_mean[0][1][7][6] = -0.0113997;
    PC3_dz_sigma[0][1][7][0] = 687.278;
    PC3_dz_sigma[0][1][7][1] = -887.024;
    PC3_dz_sigma[0][1][7][2] = 678.151;
    PC3_dz_sigma[0][1][7][3] = -269.082;
    PC3_dz_sigma[0][1][7][4] = 51.9524;
    PC3_dz_sigma[0][1][7][5] = -3.84132;
    PC3_dz_sigma[0][1][7][6] = -249.361;
    PC3_dz_sigma[0][1][7][7] = -3.55135;
    PC3_dz_mean[0][1][7][0] = 521.34;
    PC3_dz_mean[0][1][7][1] = -27.9929;
    PC3_dz_mean[0][1][7][2] = 945.488;
    PC3_dz_mean[0][1][7][3] = -1271.59;
    PC3_dz_mean[0][1][7][4] = -161.142;
    PC3_dz_mean[0][1][7][5] = -9.84747;
    PC3_dz_mean[0][1][7][6] = 8.03899;
    PC3_dphi_sigma[1][0][0][0] = 0.00436797;
    PC3_dphi_sigma[1][0][0][1] = 0.000411374;
    PC3_dphi_sigma[1][0][0][2] = -0.00010786;
    PC3_dphi_sigma[1][0][0][3] = -3.39679e-05;
    PC3_dphi_sigma[1][0][0][4] = -3.89704e-06;
    PC3_dphi_sigma[1][0][0][5] = 7.44421e-07;
    PC3_dphi_sigma[1][0][0][6] = -0.003148;
    PC3_dphi_sigma[1][0][0][7] = 0.00155997;
    PC3_dphi_mean[1][0][0][0] = 0.224251;
    PC3_dphi_mean[1][0][0][1] = -0.0104924;
    PC3_dphi_mean[1][0][0][2] = 0.538472;
    PC3_dphi_mean[1][0][0][3] = -0.610458;
    PC3_dphi_mean[1][0][0][4] = -0.189911;
    PC3_dphi_mean[1][0][0][5] = 0.054183;
    PC3_dphi_mean[1][0][0][6] = -0.00682271;
    PC3_dz_sigma[1][0][0][0] = -3177.82;
    PC3_dz_sigma[1][0][0][1] = 2133.36;
    PC3_dz_sigma[1][0][0][2] = -1023.59;
    PC3_dz_sigma[1][0][0][3] = 280.323;
    PC3_dz_sigma[1][0][0][4] = -40.1951;
    PC3_dz_sigma[1][0][0][5] = 2.3548;
    PC3_dz_sigma[1][0][0][6] = 1915.26;
    PC3_dz_sigma[1][0][0][7] = -89.3243;
    PC3_dz_mean[1][0][0][0] = -2095.62;
    PC3_dz_mean[1][0][0][1] = -46.5985;
    PC3_dz_mean[1][0][0][2] = -17856.4;
    PC3_dz_mean[1][0][0][3] = 12073.9;
    PC3_dz_mean[1][0][0][4] = 12982.4;
    PC3_dz_mean[1][0][0][5] = -6212.36;
    PC3_dz_mean[1][0][0][6] = 1144.65;
    PC3_dphi_sigma[1][0][1][0] = -0.0111378;
    PC3_dphi_sigma[1][0][1][1] = 0.0131341;
    PC3_dphi_sigma[1][0][1][2] = -0.00873563;
    PC3_dphi_sigma[1][0][1][3] = 0.00314422;
    PC3_dphi_sigma[1][0][1][4] = -0.000567607;
    PC3_dphi_sigma[1][0][1][5] = 4.05149e-05;
    PC3_dphi_sigma[1][0][1][6] = 0.00611208;
    PC3_dphi_sigma[1][0][1][7] = 0.000582452;
    PC3_dphi_mean[1][0][1][0] = -0.0069684;
    PC3_dphi_mean[1][0][1][1] = 0.000738164;
    PC3_dphi_mean[1][0][1][2] = -0.000271711;
    PC3_dphi_mean[1][0][1][3] = 0.00858109;
    PC3_dphi_mean[1][0][1][4] = -0.00584603;
    PC3_dphi_mean[1][0][1][5] = 0.00350056;
    PC3_dphi_mean[1][0][1][6] = -0.000727813;
    PC3_dz_sigma[1][0][1][0] = 362.41;
    PC3_dz_sigma[1][0][1][1] = -317.267;
    PC3_dz_sigma[1][0][1][2] = 202.537;
    PC3_dz_sigma[1][0][1][3] = -73.5137;
    PC3_dz_sigma[1][0][1][4] = 13.566;
    PC3_dz_sigma[1][0][1][5] = -0.974429;
    PC3_dz_sigma[1][0][1][6] = -191.019;
    PC3_dz_sigma[1][0][1][7] = 6.84793;
    PC3_dz_mean[1][0][1][0] = 102.711;
    PC3_dz_mean[1][0][1][1] = -6.34036;
    PC3_dz_mean[1][0][1][2] = 210.114;
    PC3_dz_mean[1][0][1][3] = -257.517;
    PC3_dz_mean[1][0][1][4] = -65.8943;
    PC3_dz_mean[1][0][1][5] = 17.3985;
    PC3_dz_mean[1][0][1][6] = -2.069;
    PC3_dphi_sigma[1][0][2][0] = -0.00723829;
    PC3_dphi_sigma[1][0][2][1] = 0.00923443;
    PC3_dphi_sigma[1][0][2][2] = -0.00627704;
    PC3_dphi_sigma[1][0][2][3] = 0.00230468;
    PC3_dphi_sigma[1][0][2][4] = -0.000423542;
    PC3_dphi_sigma[1][0][2][5] = 3.13264e-05;
    PC3_dphi_sigma[1][0][2][6] = 0.00439842;
    PC3_dphi_sigma[1][0][2][7] = 0.000537839;
    PC3_dphi_mean[1][0][2][0] = -0.00800287;
    PC3_dphi_mean[1][0][2][1] = 0.000930919;
    PC3_dphi_mean[1][0][2][2] = 0.000169286;
    PC3_dphi_mean[1][0][2][3] = 0.00926676;
    PC3_dphi_mean[1][0][2][4] = -0.0061248;
    PC3_dphi_mean[1][0][2][5] = 0.0034863;
    PC3_dphi_mean[1][0][2][6] = -0.000700062;
    PC3_dz_sigma[1][0][2][0] = -14.1948;
    PC3_dz_sigma[1][0][2][1] = 11.2673;
    PC3_dz_sigma[1][0][2][2] = -6.24631;
    PC3_dz_sigma[1][0][2][3] = 2.01345;
    PC3_dz_sigma[1][0][2][4] = -0.336632;
    PC3_dz_sigma[1][0][2][5] = 0.0225194;
    PC3_dz_sigma[1][0][2][6] = 9.37245;
    PC3_dz_sigma[1][0][2][7] = -0.31186;
    PC3_dz_mean[1][0][2][0] = -14.271;
    PC3_dz_mean[1][0][2][1] = 0.774646;
    PC3_dz_mean[1][0][2][2] = -36.3206;
    PC3_dz_mean[1][0][2][3] = 42.3909;
    PC3_dz_mean[1][0][2][4] = 12.1274;
    PC3_dz_mean[1][0][2][5] = -3.37532;
    PC3_dz_mean[1][0][2][6] = 0.413065;
    PC3_dphi_sigma[1][0][3][0] = -0.0106148;
    PC3_dphi_sigma[1][0][3][1] = 0.0113052;
    PC3_dphi_sigma[1][0][3][2] = -0.00726565;
    PC3_dphi_sigma[1][0][3][3] = 0.0025844;
    PC3_dphi_sigma[1][0][3][4] = -0.00046434;
    PC3_dphi_sigma[1][0][3][5] = 3.34982e-05;
    PC3_dphi_sigma[1][0][3][6] = 0.00661305;
    PC3_dphi_sigma[1][0][3][7] = 0.000385548;
    PC3_dphi_mean[1][0][3][0] = -0.0241338;
    PC3_dphi_mean[1][0][3][1] = 0.00179691;
    PC3_dphi_mean[1][0][3][2] = -0.0355137;
    PC3_dphi_mean[1][0][3][3] = 0.0512274;
    PC3_dphi_mean[1][0][3][4] = 0.00573956;
    PC3_dphi_mean[1][0][3][5] = 0.000250711;
    PC3_dphi_mean[1][0][3][6] = -0.000307669;
    PC3_dz_sigma[1][0][3][0] = -18.9564;
    PC3_dz_sigma[1][0][3][1] = 14.6111;
    PC3_dz_sigma[1][0][3][2] = -8.03026;
    PC3_dz_sigma[1][0][3][3] = 2.59375;
    PC3_dz_sigma[1][0][3][4] = -0.439273;
    PC3_dz_sigma[1][0][3][5] = 0.0300492;
    PC3_dz_sigma[1][0][3][6] = 12.2742;
    PC3_dz_sigma[1][0][3][7] = -0.437515;
    PC3_dz_mean[1][0][3][0] = -14.7403;
    PC3_dz_mean[1][0][3][1] = 0.865256;
    PC3_dz_mean[1][0][3][2] = -36.0197;
    PC3_dz_mean[1][0][3][3] = 43.311;
    PC3_dz_mean[1][0][3][4] = 11.1821;
    PC3_dz_mean[1][0][3][5] = -2.80294;
    PC3_dz_mean[1][0][3][6] = 0.302106;
    PC3_dphi_sigma[1][0][4][0] = -0.00926673;
    PC3_dphi_sigma[1][0][4][1] = 0.0101237;
    PC3_dphi_sigma[1][0][4][2] = -0.00657804;
    PC3_dphi_sigma[1][0][4][3] = 0.00237434;
    PC3_dphi_sigma[1][0][4][4] = -0.000434338;
    PC3_dphi_sigma[1][0][4][5] = 3.2065e-05;
    PC3_dphi_sigma[1][0][4][6] = 0.00592479;
    PC3_dphi_sigma[1][0][4][7] = 0.000405547;
    PC3_dphi_mean[1][0][4][0] = -0.00861186;
    PC3_dphi_mean[1][0][4][1] = 0.000997402;
    PC3_dphi_mean[1][0][4][2] = 3.93447e-05;
    PC3_dphi_mean[1][0][4][3] = 0.0101951;
    PC3_dphi_mean[1][0][4][4] = -0.00653865;
    PC3_dphi_mean[1][0][4][5] = 0.00375268;
    PC3_dphi_mean[1][0][4][6] = -0.000746824;
    PC3_dz_sigma[1][0][4][0] = -24.1893;
    PC3_dz_sigma[1][0][4][1] = 18.3129;
    PC3_dz_sigma[1][0][4][2] = -9.85485;
    PC3_dz_sigma[1][0][4][3] = 3.07858;
    PC3_dz_sigma[1][0][4][4] = -0.499853;
    PC3_dz_sigma[1][0][4][5] = 0.0325581;
    PC3_dz_sigma[1][0][4][6] = 15.4243;
    PC3_dz_sigma[1][0][4][7] = -0.554682;
    PC3_dz_mean[1][0][4][0] = 0.549549;
    PC3_dz_mean[1][0][4][1] = 0.115469;
    PC3_dz_mean[1][0][4][2] = -0.311918;
    PC3_dz_mean[1][0][4][3] = 2.595;
    PC3_dz_mean[1][0][4][4] = -1.22264;
    PC3_dz_mean[1][0][4][5] = 0.718196;
    PC3_dz_mean[1][0][4][6] = -0.134049;
    PC3_dphi_sigma[1][0][5][0] = -0.00876377;
    PC3_dphi_sigma[1][0][5][1] = 0.0099822;
    PC3_dphi_sigma[1][0][5][2] = -0.00664589;
    PC3_dphi_sigma[1][0][5][3] = 0.00244559;
    PC3_dphi_sigma[1][0][5][4] = -0.000454272;
    PC3_dphi_sigma[1][0][5][5] = 3.38457e-05;
    PC3_dphi_sigma[1][0][5][6] = 0.00554803;
    PC3_dphi_sigma[1][0][5][7] = 0.000447234;
    PC3_dphi_mean[1][0][5][0] = -0.00873678;
    PC3_dphi_mean[1][0][5][1] = 0.00100892;
    PC3_dphi_mean[1][0][5][2] = 7.80398e-05;
    PC3_dphi_mean[1][0][5][3] = 0.0103601;
    PC3_dphi_mean[1][0][5][4] = -0.00667564;
    PC3_dphi_mean[1][0][5][5] = 0.00383193;
    PC3_dphi_mean[1][0][5][6] = -0.00076052;
    PC3_dz_sigma[1][0][5][0] = -31.5567;
    PC3_dz_sigma[1][0][5][1] = 23.6344;
    PC3_dz_sigma[1][0][5][2] = -12.5877;
    PC3_dz_sigma[1][0][5][3] = 3.87816;
    PC3_dz_sigma[1][0][5][4] = -0.619903;
    PC3_dz_sigma[1][0][5][5] = 0.039513;
    PC3_dz_sigma[1][0][5][6] = 19.8468;
    PC3_dz_sigma[1][0][5][7] = -0.749478;
    PC3_dz_mean[1][0][5][0] = -11.7628;
    PC3_dz_mean[1][0][5][1] = 0.789699;
    PC3_dz_mean[1][0][5][2] = -29.0554;
    PC3_dz_mean[1][0][5][3] = 35.7921;
    PC3_dz_mean[1][0][5][4] = 8.85031;
    PC3_dz_mean[1][0][5][5] = -2.1586;
    PC3_dz_mean[1][0][5][6] = 0.22565;
    PC3_dphi_sigma[1][0][6][0] = 0.0119593;
    PC3_dphi_sigma[1][0][6][1] = -0.00903408;
    PC3_dphi_sigma[1][0][6][2] = 0.00586775;
    PC3_dphi_sigma[1][0][6][3] = -0.00222442;
    PC3_dphi_sigma[1][0][6][4] = 0.000436499;
    PC3_dphi_sigma[1][0][6][5] = -3.28655e-05;
    PC3_dphi_sigma[1][0][6][6] = -0.00518301;
    PC3_dphi_sigma[1][0][6][7] = 0.000818772;
    PC3_dphi_mean[1][0][6][0] = -0.010012;
    PC3_dphi_mean[1][0][6][1] = 0.00114735;
    PC3_dphi_mean[1][0][6][2] = 0.000491873;
    PC3_dphi_mean[1][0][6][3] = 0.011838;
    PC3_dphi_mean[1][0][6][4] = -0.0079736;
    PC3_dphi_mean[1][0][6][5] = 0.00449911;
    PC3_dphi_mean[1][0][6][6] = -0.000874751;
    PC3_dz_sigma[1][0][6][0] = -17.4816;
    PC3_dz_sigma[1][0][6][1] = 13.9148;
    PC3_dz_sigma[1][0][6][2] = -8.12904;
    PC3_dz_sigma[1][0][6][3] = 2.918;
    PC3_dz_sigma[1][0][6][4] = -0.570001;
    PC3_dz_sigma[1][0][6][5] = 0.0453473;
    PC3_dz_sigma[1][0][6][6] = 11.9874;
    PC3_dz_sigma[1][0][6][7] = -0.451281;
    PC3_dz_mean[1][0][6][0] = 137.275;
    PC3_dz_mean[1][0][6][1] = -8.19888;
    PC3_dz_mean[1][0][6][2] = 270.725;
    PC3_dz_mean[1][0][6][3] = -329.527;
    PC3_dz_mean[1][0][6][4] = -86.0448;
    PC3_dz_mean[1][0][6][5] = 23.1731;
    PC3_dz_mean[1][0][6][6] = -2.80445;
    PC3_dphi_sigma[1][0][7][0] = 0.198834;
    PC3_dphi_sigma[1][0][7][1] = -0.159428;
    PC3_dphi_sigma[1][0][7][2] = 0.0938326;
    PC3_dphi_sigma[1][0][7][3] = -0.0316978;
    PC3_dphi_sigma[1][0][7][4] = 0.005493;
    PC3_dphi_sigma[1][0][7][5] = -0.000375605;
    PC3_dphi_sigma[1][0][7][6] = -0.108676;
    PC3_dphi_sigma[1][0][7][7] = 0.00524436;
    PC3_dphi_mean[1][0][7][0] = 0.0251432;
    PC3_dphi_mean[1][0][7][1] = -0.00266552;
    PC3_dphi_mean[1][0][7][2] = -0.00178518;
    PC3_dphi_mean[1][0][7][3] = -0.03148;
    PC3_dphi_mean[1][0][7][4] = 0.0172938;
    PC3_dphi_mean[1][0][7][5] = -0.00856998;
    PC3_dphi_mean[1][0][7][6] = 0.00137562;
    PC3_dz_sigma[1][0][7][0] = 1885.08;
    PC3_dz_sigma[1][0][7][1] = -1429.04;
    PC3_dz_sigma[1][0][7][2] = 743.102;
    PC3_dz_sigma[1][0][7][3] = -211.984;
    PC3_dz_sigma[1][0][7][4] = 30.9237;
    PC3_dz_sigma[1][0][7][5] = -1.81912;
    PC3_dz_sigma[1][0][7][6] = -1051.53;
    PC3_dz_sigma[1][0][7][7] = 42.4669;
    PC3_dz_mean[1][0][7][0] = 5150.22;
    PC3_dz_mean[1][0][7][1] = -146.473;
    PC3_dz_mean[1][0][7][2] = 18308.6;
    PC3_dz_mean[1][0][7][3] = -17319.4;
    PC3_dz_mean[1][0][7][4] = -8539.26;
    PC3_dz_mean[1][0][7][5] = 2994.07;
    PC3_dz_mean[1][0][7][6] = -435.843;
    PC3_dphi_sigma[1][1][0][0] = 0.0370626;
    PC3_dphi_sigma[1][1][0][1] = -0.0184263;
    PC3_dphi_sigma[1][1][0][2] = 0.00400878;
    PC3_dphi_sigma[1][1][0][3] = 0.000678779;
    PC3_dphi_sigma[1][1][0][4] = -0.000455585;
    PC3_dphi_sigma[1][1][0][5] = 5.69271e-05;
    PC3_dphi_sigma[1][1][0][6] = -0.0223922;
    PC3_dphi_sigma[1][1][0][7] = 0.00215002;
    PC3_dphi_mean[1][1][0][0] = -0.0707683;
    PC3_dphi_mean[1][1][0][1] = 0.00961061;
    PC3_dphi_mean[1][1][0][2] = 0.0111632;
    PC3_dphi_mean[1][1][0][3] = 0.0748104;
    PC3_dphi_mean[1][1][0][4] = -0.042372;
    PC3_dphi_mean[1][1][0][5] = 0.0197169;
    PC3_dphi_mean[1][1][0][6] = -0.00312107;
    PC3_dz_sigma[1][1][0][0] = 0.731131;
    PC3_dz_sigma[1][1][0][1] = 1.12421;
    PC3_dz_sigma[1][1][0][2] = 1.72349;
    PC3_dz_sigma[1][1][0][3] = 2.59083;
    PC3_dz_sigma[1][1][0][4] = 3.3754;
    PC3_dz_sigma[1][1][0][5] = -0.961611;
    PC3_dz_sigma[1][1][0][6] = 0.589492;
    PC3_dz_sigma[1][1][0][7] = 0.308933;
    PC3_dz_mean[1][1][0][0] = -7.94853;
    PC3_dz_mean[1][1][0][1] = -12.2285;
    PC3_dz_mean[1][1][0][2] = -5.16654;
    PC3_dz_mean[1][1][0][3] = -6.40831;
    PC3_dz_mean[1][1][0][4] = -3.35825;
    PC3_dz_mean[1][1][0][5] = -2.18286;
    PC3_dz_mean[1][1][0][6] = -1.41886;
    PC3_dphi_sigma[1][1][1][0] = 0.0523738;
    PC3_dphi_sigma[1][1][1][1] = -0.0325245;
    PC3_dphi_sigma[1][1][1][2] = 0.015236;
    PC3_dphi_sigma[1][1][1][3] = -0.00423134;
    PC3_dphi_sigma[1][1][1][4] = 0.000639841;
    PC3_dphi_sigma[1][1][1][5] = -4.09231e-05;
    PC3_dphi_sigma[1][1][1][6] = -0.0311265;
    PC3_dphi_sigma[1][1][1][7] = 0.00218363;
    PC3_dphi_mean[1][1][1][0] = -0.0506782;
    PC3_dphi_mean[1][1][1][1] = 0.00206188;
    PC3_dphi_mean[1][1][1][2] = -0.139013;
    PC3_dphi_mean[1][1][1][3] = 0.144856;
    PC3_dphi_mean[1][1][1][4] = 0.0580907;
    PC3_dphi_mean[1][1][1][5] = -0.0191575;
    PC3_dphi_mean[1][1][1][6] = 0.00273291;
    PC3_dz_sigma[1][1][1][0] = 61.3359;
    PC3_dz_sigma[1][1][1][1] = -50.4937;
    PC3_dz_sigma[1][1][1][2] = 30.9954;
    PC3_dz_sigma[1][1][1][3] = -10.7983;
    PC3_dz_sigma[1][1][1][4] = 1.91011;
    PC3_dz_sigma[1][1][1][5] = -0.132404;
    PC3_dz_sigma[1][1][1][6] = -31.4024;
    PC3_dz_sigma[1][1][1][7] = 1.27388;
    PC3_dz_mean[1][1][1][0] = -7.29936;
    PC3_dz_mean[1][1][1][1] = 0.750971;
    PC3_dz_mean[1][1][1][2] = -0.109512;
    PC3_dz_mean[1][1][1][3] = 7.05685;
    PC3_dz_mean[1][1][1][4] = -3.74699;
    PC3_dz_mean[1][1][1][5] = 1.97105;
    PC3_dz_mean[1][1][1][6] = -0.351232;
    PC3_dphi_sigma[1][1][2][0] = 0.0594134;
    PC3_dphi_sigma[1][1][2][1] = -0.0380818;
    PC3_dphi_sigma[1][1][2][2] = 0.0181012;
    PC3_dphi_sigma[1][1][2][3] = -0.00497242;
    PC3_dphi_sigma[1][1][2][4] = 0.000711327;
    PC3_dphi_sigma[1][1][2][5] = -3.9897e-05;
    PC3_dphi_sigma[1][1][2][6] = -0.0348686;
    PC3_dphi_sigma[1][1][2][7] = 0.0022557;
    PC3_dphi_mean[1][1][2][0] = -0.0512542;
    PC3_dphi_mean[1][1][2][1] = 0.00206803;
    PC3_dphi_mean[1][1][2][2] = -0.141045;
    PC3_dphi_mean[1][1][2][3] = 0.146864;
    PC3_dphi_mean[1][1][2][4] = 0.0589856;
    PC3_dphi_mean[1][1][2][5] = -0.0195084;
    PC3_dphi_mean[1][1][2][6] = 0.00278772;
    PC3_dz_sigma[1][1][2][0] = -11.57;
    PC3_dz_sigma[1][1][2][1] = 10.3824;
    PC3_dz_sigma[1][1][2][2] = -6.31915;
    PC3_dz_sigma[1][1][2][3] = 2.21468;
    PC3_dz_sigma[1][1][2][4] = -0.40074;
    PC3_dz_sigma[1][1][2][5] = 0.0288585;
    PC3_dz_sigma[1][1][2][6] = 7.38117;
    PC3_dz_sigma[1][1][2][7] = -0.172808;
    PC3_dz_mean[1][1][2][0] = -16.1829;
    PC3_dz_mean[1][1][2][1] = 0.886619;
    PC3_dz_mean[1][1][2][2] = -40.8259;
    PC3_dz_mean[1][1][2][3] = 47.4266;
    PC3_dz_mean[1][1][2][4] = 13.8249;
    PC3_dz_mean[1][1][2][5] = -3.91551;
    PC3_dz_mean[1][1][2][6] = 0.49046;
    PC3_dphi_sigma[1][1][3][0] = 0.0488442;
    PC3_dphi_sigma[1][1][3][1] = -0.0296279;
    PC3_dphi_sigma[1][1][3][2] = 0.0130165;
    PC3_dphi_sigma[1][1][3][3] = -0.00316958;
    PC3_dphi_sigma[1][1][3][4] = 0.000372626;
    PC3_dphi_sigma[1][1][3][5] = -1.39918e-05;
    PC3_dphi_sigma[1][1][3][6] = -0.0288761;
    PC3_dphi_sigma[1][1][3][7] = 0.00197549;
    PC3_dphi_mean[1][1][3][0] = -0.0480101;
    PC3_dphi_mean[1][1][3][1] = 0.00190313;
    PC3_dphi_mean[1][1][3][2] = -0.133763;
    PC3_dphi_mean[1][1][3][3] = 0.138357;
    PC3_dphi_mean[1][1][3][4] = 0.0565821;
    PC3_dphi_mean[1][1][3][5] = -0.0188795;
    PC3_dphi_mean[1][1][3][6] = 0.00271114;
    PC3_dz_sigma[1][1][3][0] = 1.43207;
    PC3_dz_sigma[1][1][3][1] = 0.0633627;
    PC3_dz_sigma[1][1][3][2] = -0.325328;
    PC3_dz_sigma[1][1][3][3] = 0.221885;
    PC3_dz_sigma[1][1][3][4] = -0.0599022;
    PC3_dz_sigma[1][1][3][5] = 0.00581029;
    PC3_dz_sigma[1][1][3][6] = 0.144925;
    PC3_dz_sigma[1][1][3][7] = 0.121784;
    PC3_dz_mean[1][1][3][0] = -18.67;
    PC3_dz_mean[1][1][3][1] = 1.07818;
    PC3_dz_mean[1][1][3][2] = -46.0837;
    PC3_dz_mean[1][1][3][3] = 54.1102;
    PC3_dz_mean[1][1][3][4] = 15.3725;
    PC3_dz_mean[1][1][3][5] = -4.28274;
    PC3_dz_mean[1][1][3][6] = 0.530652;
    PC3_dphi_sigma[1][1][4][0] = 0.045934;
    PC3_dphi_sigma[1][1][4][1] = -0.0267381;
    PC3_dphi_sigma[1][1][4][2] = 0.0109209;
    PC3_dphi_sigma[1][1][4][3] = -0.0023133;
    PC3_dphi_sigma[1][1][4][4] = 0.000197542;
    PC3_dphi_sigma[1][1][4][5] = -2.03424e-07;
    PC3_dphi_sigma[1][1][4][6] = -0.0274007;
    PC3_dphi_sigma[1][1][4][7] = 0.00192786;
    PC3_dphi_mean[1][1][4][0] = -0.0546427;
    PC3_dphi_mean[1][1][4][1] = 0.00227537;
    PC3_dphi_mean[1][1][4][2] = -0.14757;
    PC3_dphi_mean[1][1][4][3] = 0.15515;
    PC3_dphi_mean[1][1][4][4] = 0.0607993;
    PC3_dphi_mean[1][1][4][5] = -0.0199256;
    PC3_dphi_mean[1][1][4][6] = 0.00282026;
    PC3_dz_sigma[1][1][4][0] = -6.47586;
    PC3_dz_sigma[1][1][4][1] = 6.81065;
    PC3_dz_sigma[1][1][4][2] = -4.62393;
    PC3_dz_sigma[1][1][4][3] = 1.81656;
    PC3_dz_sigma[1][1][4][4] = -0.365145;
    PC3_dz_sigma[1][1][4][5] = 0.0288441;
    PC3_dz_sigma[1][1][4][6] = 4.50933;
    PC3_dz_sigma[1][1][4][7] = -0.0204579;
    PC3_dz_mean[1][1][4][0] = -12.962;
    PC3_dz_mean[1][1][4][1] = 0.755659;
    PC3_dz_mean[1][1][4][2] = -34.3324;
    PC3_dz_mean[1][1][4][3] = 40.0469;
    PC3_dz_mean[1][1][4][4] = 11.5695;
    PC3_dz_mean[1][1][4][5] = -3.22408;
    PC3_dz_mean[1][1][4][6] = 0.399163;
    PC3_dphi_sigma[1][1][5][0] = 0.0511155;
    PC3_dphi_sigma[1][1][5][1] = -0.0312103;
    PC3_dphi_sigma[1][1][5][2] = 0.013768;
    PC3_dphi_sigma[1][1][5][3] = -0.00335584;
    PC3_dphi_sigma[1][1][5][4] = 0.000393935;
    PC3_dphi_sigma[1][1][5][5] = -1.46841e-05;
    PC3_dphi_sigma[1][1][5][6] = -0.0302076;
    PC3_dphi_sigma[1][1][5][7] = 0.00204472;
    PC3_dphi_mean[1][1][5][0] = -0.0492224;
    PC3_dphi_mean[1][1][5][1] = 0.00195037;
    PC3_dphi_mean[1][1][5][2] = -0.136563;
    PC3_dphi_mean[1][1][5][3] = 0.141707;
    PC3_dphi_mean[1][1][5][4] = 0.0574023;
    PC3_dphi_mean[1][1][5][5] = -0.0190761;
    PC3_dphi_mean[1][1][5][6] = 0.00272457;
    PC3_dz_sigma[1][1][5][0] = -9.80103;
    PC3_dz_sigma[1][1][5][1] = 9.14085;
    PC3_dz_sigma[1][1][5][2] = -5.58084;
    PC3_dz_sigma[1][1][5][3] = 1.96654;
    PC3_dz_sigma[1][1][5][4] = -0.360106;
    PC3_dz_sigma[1][1][5][5] = 0.0262971;
    PC3_dz_sigma[1][1][5][6] = 6.54725;
    PC3_dz_sigma[1][1][5][7] = -0.104707;
    PC3_dz_mean[1][1][5][0] = -4.9778;
    PC3_dz_mean[1][1][5][1] = 0.40542;
    PC3_dz_mean[1][1][5][2] = -15.83;
    PC3_dz_mean[1][1][5][3] = 19.0482;
    PC3_dz_mean[1][1][5][4] = 5.23657;
    PC3_dz_mean[1][1][5][5] = -1.4518;
    PC3_dz_mean[1][1][5][6] = 0.182758;
    PC3_dphi_sigma[1][1][6][0] = 0.0493278;
    PC3_dphi_sigma[1][1][6][1] = -0.0307283;
    PC3_dphi_sigma[1][1][6][2] = 0.0143141;
    PC3_dphi_sigma[1][1][6][3] = -0.00393211;
    PC3_dphi_sigma[1][1][6][4] = 0.000589834;
    PC3_dphi_sigma[1][1][6][5] = -3.75415e-05;
    PC3_dphi_sigma[1][1][6][6] = -0.0290266;
    PC3_dphi_sigma[1][1][6][7] = 0.00201932;
    PC3_dphi_mean[1][1][6][0] = -0.0516166;
    PC3_dphi_mean[1][1][6][1] = 0.00211732;
    PC3_dphi_mean[1][1][6][2] = -0.139462;
    PC3_dphi_mean[1][1][6][3] = 0.146684;
    PC3_dphi_mean[1][1][6][4] = 0.0572688;
    PC3_dphi_mean[1][1][6][5] = -0.0186772;
    PC3_dphi_mean[1][1][6][6] = 0.00262453;
    PC3_dz_sigma[1][1][6][0] = 28.3063;
    PC3_dz_sigma[1][1][6][1] = -24.5344;
    PC3_dz_sigma[1][1][6][2] = 16.4874;
    PC3_dz_sigma[1][1][6][3] = -6.40972;
    PC3_dz_sigma[1][1][6][4] = 1.2825;
    PC3_dz_sigma[1][1][6][5] = -0.100207;
    PC3_dz_sigma[1][1][6][6] = -13.4295;
    PC3_dz_sigma[1][1][6][7] = 0.573355;
    PC3_dz_mean[1][1][6][0] = 6.65409;
    PC3_dz_mean[1][1][6][1] = -0.388729;
    PC3_dz_mean[1][1][6][2] = -0.401835;
    PC3_dz_mean[1][1][6][3] = -2.22255;
    PC3_dz_mean[1][1][6][4] = 1.44899;
    PC3_dz_mean[1][1][6][5] = -0.674893;
    PC3_dz_mean[1][1][6][6] = 0.115505;
    PC3_dphi_sigma[1][1][7][0] = -0.110404;
    PC3_dphi_sigma[1][1][7][1] = 0.0907801;
    PC3_dphi_sigma[1][1][7][2] = -0.0537789;
    PC3_dphi_sigma[1][1][7][3] = 0.0178752;
    PC3_dphi_sigma[1][1][7][4] = -0.00304171;
    PC3_dphi_sigma[1][1][7][5] = 0.00020685;
    PC3_dphi_sigma[1][1][7][6] = 0.0627152;
    PC3_dphi_sigma[1][1][7][7] = -0.0016898;
    PC3_dphi_mean[1][1][7][0] = 3.34272;
    PC3_dphi_mean[1][1][7][1] = -0.162035;
    PC3_dphi_mean[1][1][7][2] = 7.93588;
    PC3_dphi_mean[1][1][7][3] = -9.02353;
    PC3_dphi_mean[1][1][7][4] = -2.80176;
    PC3_dphi_mean[1][1][7][5] = 0.811843;
    PC3_dphi_mean[1][1][7][6] = -0.103642;
    PC3_dz_sigma[1][1][7][0] = 2136.02;
    PC3_dz_sigma[1][1][7][1] = -1398.42;
    PC3_dz_sigma[1][1][7][2] = 640.926;
    PC3_dz_sigma[1][1][7][3] = -166.943;
    PC3_dz_sigma[1][1][7][4] = 22.6036;
    PC3_dz_sigma[1][1][7][5] = -1.23453;
    PC3_dz_sigma[1][1][7][6] = -1277.32;
    PC3_dz_sigma[1][1][7][7] = 56.6214;
    PC3_dz_mean[1][1][7][0] = -1312.56;
    PC3_dz_mean[1][1][7][1] = 51.3086;
    PC3_dz_mean[1][1][7][2] = -3612.39;
    PC3_dz_mean[1][1][7][3] = 3845.17;
    PC3_dz_mean[1][1][7][4] = 1409.23;
    PC3_dz_mean[1][1][7][5] = -436.859;
    PC3_dz_mean[1][1][7][6] = 58.5656;

PC2_dphifirst_mean[0][0][0] = -0.099;
PC2_dphifirst_sigma[0][0][0] = 0;
PC2_dzfirst_mean[0][0][0] = -9.8;
PC2_dzfirst_sigma[0][0][0] = 0;
PC2_dphifirst_mean[0][1][0] = -0.099;
PC2_dphifirst_sigma[0][1][0] = 0;
PC2_dzfirst_mean[0][1][0] = -9.8;
PC2_dzfirst_sigma[0][1][0] = 0;
PC2_dphifirst_mean[1][0][0] = -0.00463678;
PC2_dphifirst_sigma[1][0][0] = 0.00678517;
PC2_dzfirst_mean[1][0][0] = 0.727844;
PC2_dzfirst_sigma[1][0][0] = 3.05688;
PC2_dphifirst_mean[1][1][0] = 0.00183203;
PC2_dphifirst_sigma[1][1][0] = 0.00641507;
PC2_dzfirst_mean[1][1][0] = 0.538167;
PC2_dzfirst_sigma[1][1][0] = 3.19457;
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
PC2_dphi_sigma[1][0][0][0] = 0.0501725;
PC2_dphi_sigma[1][0][0][1] = -0.0344652;
PC2_dphi_sigma[1][0][0][2] = 0.018173;
PC2_dphi_sigma[1][0][0][3] = -0.00562123;
PC2_dphi_sigma[1][0][0][4] = 0.000915758;
PC2_dphi_sigma[1][0][0][5] = -5.95638e-05;
PC2_dphi_sigma[1][0][0][6] = -0.0288543;
PC2_dphi_sigma[1][0][0][7] = 0.00186785;
PC2_dphi_mean[1][0][0][0] = -0.0183853;
PC2_dphi_mean[1][0][0][1] = 0.001338;
PC2_dphi_mean[1][0][0][2] = -0.0272901;
PC2_dphi_mean[1][0][0][3] = 0.0382996;
PC2_dphi_mean[1][0][0][4] = 0.0051631;
PC2_dphi_mean[1][0][0][5] = -0.000212404;
PC2_dphi_mean[1][0][0][6] = -0.000185489;
PC2_dz_sigma[1][0][0][0] = -46.73;
PC2_dz_sigma[1][0][0][1] = 39.4808;
PC2_dz_sigma[1][0][0][2] = -23.958;
PC2_dz_sigma[1][0][0][3] = 8.42211;
PC2_dz_sigma[1][0][0][4] = -1.54484;
PC2_dz_sigma[1][0][0][5] = 0.113622;
PC2_dz_sigma[1][0][0][6] = 26.2223;
PC2_dz_sigma[1][0][0][7] = -0.866038;
PC2_dz_mean[1][0][0][0] = 1.03368;
PC2_dz_mean[1][0][0][1] = -0.035291;
PC2_dz_mean[1][0][0][2] = -0.300257;
PC2_dz_mean[1][0][0][3] = 0.547599;
PC2_dz_mean[1][0][0][4] = -0.223489;
PC2_dz_mean[1][0][0][5] = 0.22254;
PC2_dz_mean[1][0][0][6] = -0.0545695;
PC2_dphi_sigma[1][1][0][0] = 0.0370648;
PC2_dphi_sigma[1][1][0][1] = -0.0202265;
PC2_dphi_sigma[1][1][0][2] = 0.00787692;
PC2_dphi_sigma[1][1][0][3] = -0.00157472;
PC2_dphi_sigma[1][1][0][4] = 0.000115728;
PC2_dphi_sigma[1][1][0][5] = 2.46139e-06;
PC2_dphi_sigma[1][1][0][6] = -0.022976;
PC2_dphi_sigma[1][1][0][7] = 0.00176529;
PC2_dphi_mean[1][1][0][0] = -0.0556632;
PC2_dphi_mean[1][1][0][1] = 0.00244196;
PC2_dphi_mean[1][1][0][2] = -0.142775;
PC2_dphi_mean[1][1][0][3] = 0.153234;
PC2_dphi_mean[1][1][0][4] = 0.0569575;
PC2_dphi_mean[1][1][0][5] = -0.0182968;
PC2_dphi_mean[1][1][0][6] = 0.0025742;
PC2_dz_sigma[1][1][0][0] = -16.4924;
PC2_dz_sigma[1][1][0][1] = 15.2932;
PC2_dz_sigma[1][1][0][2] = -9.78422;
PC2_dz_sigma[1][1][0][3] = 3.62312;
PC2_dz_sigma[1][1][0][4] = -0.699501;
PC2_dz_sigma[1][1][0][5] = 0.0540565;
PC2_dz_sigma[1][1][0][6] = 9.31896;
PC2_dz_sigma[1][1][0][7] = -0.196919;
PC2_dz_mean[1][1][0][0] = -11.8102;
PC2_dz_mean[1][1][0][1] = 0.638121;
PC2_dz_mean[1][1][0][2] = -30.5198;
PC2_dz_mean[1][1][0][3] = 34.8517;
PC2_dz_mean[1][1][0][4] = 10.7086;
PC2_dz_mean[1][1][0][5] = -3.09224;
PC2_dz_mean[1][1][0][6] = 0.392172;
  return;
}
