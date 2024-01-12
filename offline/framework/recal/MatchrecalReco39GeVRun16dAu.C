#include "MatchrecalReco39GeVRun16dAu.h"
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

//39GeVRun16dAu PC3/PC2 matching recalibrator, adapted from A. Adare's MiniRecal
//A. Sickles

using namespace std;
using namespace findNode;


MatchrecalReco39GeVRun16dAu::MatchrecalReco39GeVRun16dAu(const string &name): 
  Recalibrator(name)
{
  baseclasses.insert("PHCentralTrack");
  initParameters();
}

int MatchrecalReco39GeVRun16dAu::isValidRun(const int runno) const
{
   if (runno < 457634 || runno > 458167)
   {
      return 0;
   }
   return 1;
}

void MatchrecalReco39GeVRun16dAu::help() const
{
  cout << "===================================================================" << endl;
  cout << "MatchrecalReco39GeVRun16dAu::help method output"                    << endl;
  cout << "Qiao Xu(qiao.xu@vanderbilt.edu), Sylvia Morrow(sylvia.i.morrow@vanderbilt.edu)" << endl;
  cout << "PC3/PC2 matching for Run-16 39GeV d+Au " << endl;
  cout << "TOF matching will be added" << endl;
  cout << "Run16 dAu 39 GeV with all the triggers combination" << endl;
  cout << "Central arm tracks with trasverse momentum < 0.2 GeV/c are ignored" << endl;
  cout << "The matching is done in dphi and dz directions, fit with double-Gaussian distribution" << endl;
  cout << endl;
  cout << "===================================================================" << endl;
}

int MatchrecalReco39GeVRun16dAu::process_event(PHCompositeNode *topNode)
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
  if(bbcv>-10 && bbcv<=-5) ivz=0;
  else if(bbcv>-5 && bbcv<=0) ivz=1;
  else if(bbcv>0 && bbcv<=5) ivz=2;
  else if(bbcv>5 && bbcv<=10) ivz=3;
  //else if(bbcv>10 && bbcv<=20) ivz=6;
  //else if(bbcv>10 && bbcv<=30) ivz=5;
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
       
float MatchrecalReco39GeVRun16dAu::GetPC3sdphi(const int iarm, const int icharge, const int ivz, const float pt, const float pc3dphi)
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

float MatchrecalReco39GeVRun16dAu::GetPC3sdz(const int iarm, const int icharge, const int ivz, const float pt, const float pc3dz)
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


float MatchrecalReco39GeVRun16dAu::GetPC2sdphi(const int iarm, const int icharge, const int ivz, const float pt, const float PC2dphi)
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


float MatchrecalReco39GeVRun16dAu::GetPC2sdz(const int iarm, const int icharge, const int ivz, const float pt, const float PC2dz)
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


void MatchrecalReco39GeVRun16dAu::initParameters()
{
  //
  // Initialization
  //

    PC3_dphifirst_mean[0][0][0] = -0.0024294;
    PC3_dphifirst_sigma[0][0][0] = 0.00890441;
    PC3_dzfirst_mean[0][0][0] = -0.12054;
    PC3_dzfirst_sigma[0][0][0] = 3.11735;
    PC3_dphifirst_mean[0][0][1] = -0.00328354;
    PC3_dphifirst_sigma[0][0][1] = 0.00876034;
    PC3_dzfirst_mean[0][0][1] = 0.207466;
    PC3_dzfirst_sigma[0][0][1] = 3.16868;
    PC3_dphifirst_mean[0][0][2] = -0.00478218;
    PC3_dphifirst_sigma[0][0][2] = 0.00951293;
    PC3_dzfirst_mean[0][0][2] = 0.530126;
    PC3_dzfirst_sigma[0][0][2] = 3.22419;
    PC3_dphifirst_mean[0][0][3] = -0.00513319;
    PC3_dphifirst_sigma[0][0][3] = 0.0100701;
    PC3_dzfirst_mean[0][0][3] = 0.792905;
    PC3_dzfirst_sigma[0][0][3] = 3.26272;
    PC3_dphifirst_mean[0][1][0] = 0.00385972;
    PC3_dphifirst_sigma[0][1][0] = 0.00890314;
    PC3_dzfirst_mean[0][1][0] = 0.0363011;
    PC3_dzfirst_sigma[0][1][0] = 3.01132;
    PC3_dphifirst_mean[0][1][1] = 0.00511752;
    PC3_dphifirst_sigma[0][1][1] = 0.00896733;
    PC3_dzfirst_mean[0][1][1] = 0.34319;
    PC3_dzfirst_sigma[0][1][1] = 3.00162;
    PC3_dphifirst_mean[0][1][2] = 0.00759254;
    PC3_dphifirst_sigma[0][1][2] = 0.00973954;
    PC3_dzfirst_mean[0][1][2] = 0.736985;
    PC3_dzfirst_sigma[0][1][2] = 3.07818;
    PC3_dphifirst_mean[0][1][3] = 0.00946688;
    PC3_dphifirst_sigma[0][1][3] = 0.0102082;
    PC3_dzfirst_mean[0][1][3] = 1.06039;
    PC3_dzfirst_sigma[0][1][3] = 3.0969;
    PC3_dphifirst_mean[1][0][0] = -0.00594689;
    PC3_dphifirst_sigma[1][0][0] = 0.00980316;
    PC3_dzfirst_mean[1][0][0] = 0.756619;
    PC3_dzfirst_sigma[1][0][0] = 3.18443;
    PC3_dphifirst_mean[1][0][1] = -0.00632417;
    PC3_dphifirst_sigma[1][0][1] = 0.00942973;
    PC3_dzfirst_mean[1][0][1] = 1.11938;
    PC3_dzfirst_sigma[1][0][1] = 3.18709;
    PC3_dphifirst_mean[1][0][2] = -0.00634772;
    PC3_dphifirst_sigma[1][0][2] = 0.00943408;
    PC3_dzfirst_mean[1][0][2] = 1.61976;
    PC3_dzfirst_sigma[1][0][2] = 3.29902;
    PC3_dphifirst_mean[1][0][3] = -0.00994733;
    PC3_dphifirst_sigma[1][0][3] = 0.014557;
    PC3_dzfirst_mean[1][0][3] = 2.05212;
    PC3_dzfirst_sigma[1][0][3] = 3.72217;
    PC3_dphifirst_mean[1][1][0] = 0.00538798;
    PC3_dphifirst_sigma[1][1][0] = 0.00984816;
    PC3_dzfirst_mean[1][1][0] = 0.602278;
    PC3_dzfirst_sigma[1][1][0] = 3.28793;
    PC3_dphifirst_mean[1][1][1] = 0.00588786;
    PC3_dphifirst_sigma[1][1][1] = 0.010085;
    PC3_dzfirst_mean[1][1][1] = 0.915837;
    PC3_dzfirst_sigma[1][1][1] = 3.30262;
    PC3_dphifirst_mean[1][1][2] = 0.00518076;
    PC3_dphifirst_sigma[1][1][2] = 0.00991736;
    PC3_dzfirst_mean[1][1][2] = 1.34399;
    PC3_dzfirst_sigma[1][1][2] = 3.31762;
    PC3_dphifirst_mean[1][1][3] = 0.00409963;
    PC3_dphifirst_sigma[1][1][3] = 0.00994848;
    PC3_dzfirst_mean[1][1][3] = 1.80649;
    PC3_dzfirst_sigma[1][1][3] = 3.61884;
    PC3_dphi_sigma[0][0][0][0] = 0.00343698;
    PC3_dphi_sigma[0][0][0][1] = 0.00032269;
    PC3_dphi_sigma[0][0][0][2] = -0.000942824;
    PC3_dphi_sigma[0][0][0][3] = 0.000360058;
    PC3_dphi_sigma[0][0][0][4] = -3.50427e-05;
    PC3_dphi_sigma[0][0][0][5] = -1.34514e-06;
    PC3_dphi_sigma[0][0][0][6] = -0.00172763;
    PC3_dphi_sigma[0][0][0][7] = 0.000745306;
    PC3_dphi_mean[0][0][0][0] = -0.00176447;
    PC3_dphi_mean[0][0][0][1] = 0.000404169;
    PC3_dphi_mean[0][0][0][2] = 6.45643e-05;
    PC3_dphi_mean[0][0][0][3] = 0.00369972;
    PC3_dphi_mean[0][0][0][4] = -0.00240636;
    PC3_dphi_mean[0][0][0][5] = 0.00136399;
    PC3_dphi_mean[0][0][0][6] = -0.000292935;
    PC3_dz_sigma[0][0][0][0] = 55.0246;
    PC3_dz_sigma[0][0][0][1] = -51.8306;
    PC3_dz_sigma[0][0][0][2] = 36.7886;
    PC3_dz_sigma[0][0][0][3] = -15.4328;
    PC3_dz_sigma[0][0][0][4] = 3.44892;
    PC3_dz_sigma[0][0][0][5] = -0.314032;
    PC3_dz_sigma[0][0][0][6] = -27.3758;
    PC3_dz_sigma[0][0][0][7] = 0.985032;
    PC3_dz_mean[0][0][0][0] = 3.01448;
    PC3_dz_mean[0][0][0][1] = -0.209231;
    PC3_dz_mean[0][0][0][2] = -0.319425;
    PC3_dz_mean[0][0][0][3] = -2.76535;
    PC3_dz_mean[0][0][0][4] = 1.6249;
    PC3_dz_mean[0][0][0][5] = -0.839884;
    PC3_dz_mean[0][0][0][6] = 0.140687;
    PC3_dphi_sigma[0][0][1][0] = 0.00590171;
    PC3_dphi_sigma[0][0][1][1] = -0.000143364;
    PC3_dphi_sigma[0][0][1][2] = -0.00179224;
    PC3_dphi_sigma[0][0][1][3] = 0.00110651;
    PC3_dphi_sigma[0][0][1][4] = -0.000258538;
    PC3_dphi_sigma[0][0][1][5] = 2.11129e-05;
    PC3_dphi_sigma[0][0][1][6] = -0.00353086;
    PC3_dphi_sigma[0][0][1][7] = 0.00085543;
    PC3_dphi_mean[0][0][1][0] = -0.00169402;
    PC3_dphi_mean[0][0][1][1] = 0.000371571;
    PC3_dphi_mean[0][0][1][2] = -9.58507e-05;
    PC3_dphi_mean[0][0][1][3] = 0.0038079;
    PC3_dphi_mean[0][0][1][4] = -0.00249136;
    PC3_dphi_mean[0][0][1][5] = 0.00147978;
    PC3_dphi_mean[0][0][1][6] = -0.000327602;
    PC3_dz_sigma[0][0][1][0] = 31.9447;
    PC3_dz_sigma[0][0][1][1] = -29.0156;
    PC3_dz_sigma[0][0][1][2] = 19.6098;
    PC3_dz_sigma[0][0][1][3] = -7.77437;
    PC3_dz_sigma[0][0][1][4] = 1.65625;
    PC3_dz_sigma[0][0][1][5] = -0.146437;
    PC3_dz_sigma[0][0][1][6] = -15.5459;
    PC3_dz_sigma[0][0][1][7] = 0.590526;
    PC3_dz_mean[0][0][1][0] = 2.11035;
    PC3_dz_mean[0][0][1][1] = -0.0559507;
    PC3_dz_mean[0][0][1][2] = 0.183686;
    PC3_dz_mean[0][0][1][3] = -2.12286;
    PC3_dz_mean[0][0][1][4] = 1.05336;
    PC3_dz_mean[0][0][1][5] = -0.644607;
    PC3_dz_mean[0][0][1][6] = 0.121258;
    PC3_dphi_sigma[0][0][2][0] = 0.00551575;
    PC3_dphi_sigma[0][0][2][1] = 0.000348692;
    PC3_dphi_sigma[0][0][2][2] = -0.00190199;
    PC3_dphi_sigma[0][0][2][3] = 0.000898972;
    PC3_dphi_sigma[0][0][2][4] = -0.000130149;
    PC3_dphi_sigma[0][0][2][5] = 1.14007e-06;
    PC3_dphi_sigma[0][0][2][6] = -0.00345762;
    PC3_dphi_sigma[0][0][2][7] = 0.000880142;
    PC3_dphi_mean[0][0][2][0] = 0.00123517;
    PC3_dphi_mean[0][0][2][1] = 5.45214e-05;
    PC3_dphi_mean[0][0][2][2] = -0.000399537;
    PC3_dphi_mean[0][0][2][3] = 0.000114303;
    PC3_dphi_mean[0][0][2][4] = -2.34866e-05;
    PC3_dphi_mean[0][0][2][5] = 0.000115967;
    PC3_dphi_mean[0][0][2][6] = -8.21827e-05;
    PC3_dz_sigma[0][0][2][0] = 6.46477;
    PC3_dz_sigma[0][0][2][1] = -1.87386;
    PC3_dz_sigma[0][0][2][2] = -1.59232;
    PC3_dz_sigma[0][0][2][3] = 1.84821;
    PC3_dz_sigma[0][0][2][4] = -0.627106;
    PC3_dz_sigma[0][0][2][5] = 0.0716634;
    PC3_dz_sigma[0][0][2][6] = -3.21116;
    PC3_dz_sigma[0][0][2][7] = 0.273577;
    PC3_dz_mean[0][0][2][0] = 2.66355;
    PC3_dz_mean[0][0][2][1] = -0.146716;
    PC3_dz_mean[0][0][2][2] = -0.179272;
    PC3_dz_mean[0][0][2][3] = -2.55926;
    PC3_dz_mean[0][0][2][4] = 1.62781;
    PC3_dz_mean[0][0][2][5] = -0.899794;
    PC3_dz_mean[0][0][2][6] = 0.163936;
    PC3_dphi_sigma[0][0][3][0] = 0.0108997;
    PC3_dphi_sigma[0][0][3][1] = -0.00182847;
    PC3_dphi_sigma[0][0][3][2] = -0.00251687;
    PC3_dphi_sigma[0][0][3][3] = 0.00214794;
    PC3_dphi_sigma[0][0][3][4] = -0.000639508;
    PC3_dphi_sigma[0][0][3][5] = 6.79214e-05;
    PC3_dphi_sigma[0][0][3][6] = -0.00712114;
    PC3_dphi_sigma[0][0][3][7] = 0.0011303;
    PC3_dphi_mean[0][0][3][0] = -0.00160634;
    PC3_dphi_mean[0][0][3][1] = 0.000404303;
    PC3_dphi_mean[0][0][3][2] = 0.000204262;
    PC3_dphi_mean[0][0][3][3] = 0.00331832;
    PC3_dphi_mean[0][0][3][4] = -0.00234545;
    PC3_dphi_mean[0][0][3][5] = 0.00128674;
    PC3_dphi_mean[0][0][3][6] = -0.000292529;
    PC3_dz_sigma[0][0][3][0] = 2.19262;
    PC3_dz_sigma[0][0][3][1] = -0.830585;
    PC3_dz_sigma[0][0][3][2] = -0.324696;
    PC3_dz_sigma[0][0][3][3] = 0.566101;
    PC3_dz_sigma[0][0][3][4] = -0.19635;
    PC3_dz_sigma[0][0][3][5] = 0.0207963;
    PC3_dz_sigma[0][0][3][6] = -0.130488;
    PC3_dz_sigma[0][0][3][7] = 0.0835342;
    PC3_dz_mean[0][0][3][0] = 0.0272242;
    PC3_dz_mean[0][0][3][1] = 0.226145;
    PC3_dz_mean[0][0][3][2] = 0.27622;
    PC3_dz_mean[0][0][3][3] = 0.286578;
    PC3_dz_mean[0][0][3][4] = -0.0586214;
    PC3_dz_mean[0][0][3][5] = -0.0859509;
    PC3_dz_mean[0][0][3][6] = 0.0291342;
    PC3_dphi_sigma[0][1][0][0] = 0.00618073;
    PC3_dphi_sigma[0][1][0][1] = 0.000824252;
    PC3_dphi_sigma[0][1][0][2] = -0.00248295;
    PC3_dphi_sigma[0][1][0][3] = 0.00123664;
    PC3_dphi_sigma[0][1][0][4] = -0.000236752;
    PC3_dphi_sigma[0][1][0][5] = 1.40702e-05;
    PC3_dphi_sigma[0][1][0][6] = -0.0044279;
    PC3_dphi_sigma[0][1][0][7] = 0.000942552;
    PC3_dphi_mean[0][1][0][0] = -0.00035593;
    PC3_dphi_mean[0][1][0][1] = 6.20831e-05;
    PC3_dphi_mean[0][1][0][2] = 8.19905e-05;
    PC3_dphi_mean[0][1][0][3] = 0.00174176;
    PC3_dphi_mean[0][1][0][4] = -0.00108938;
    PC3_dphi_mean[0][1][0][5] = 0.000634199;
    PC3_dphi_mean[0][1][0][6] = -8.12624e-05;
    PC3_dz_sigma[0][1][0][0] = 0.355166;
    PC3_dz_sigma[0][1][0][1] = -0.286646;
    PC3_dz_sigma[0][1][0][2] = 0.174714;
    PC3_dz_sigma[0][1][0][3] = 0.0539648;
    PC3_dz_sigma[0][1][0][4] = -0.0495204;
    PC3_dz_sigma[0][1][0][5] = 0.00848992;
    PC3_dz_sigma[0][1][0][6] = 0.943138;
    PC3_dz_sigma[0][1][0][7] = 0.0423994;
    PC3_dz_mean[0][1][0][0] = 2.23381;
    PC3_dz_mean[0][1][0][1] = -0.0514171;
    PC3_dz_mean[0][1][0][2] = 0.234081;
    PC3_dz_mean[0][1][0][3] = -2.37087;
    PC3_dz_mean[0][1][0][4] = 1.29251;
    PC3_dz_mean[0][1][0][5] = -0.837893;
    PC3_dz_mean[0][1][0][6] = 0.161714;
    PC3_dphi_sigma[0][1][1][0] = 0.00796564;
    PC3_dphi_sigma[0][1][1][1] = 0.000378921;
    PC3_dphi_sigma[0][1][1][2] = -0.00300786;
    PC3_dphi_sigma[0][1][1][3] = 0.00180226;
    PC3_dphi_sigma[0][1][1][4] = -0.000447059;
    PC3_dphi_sigma[0][1][1][5] = 4.21853e-05;
    PC3_dphi_sigma[0][1][1][6] = -0.00569418;
    PC3_dphi_sigma[0][1][1][7] = 0.00102657;
    PC3_dphi_mean[0][1][1][0] = -0.000534844;
    PC3_dphi_mean[0][1][1][1] = 5.25849e-05;
    PC3_dphi_mean[0][1][1][2] = -8.76499e-06;
    PC3_dphi_mean[0][1][1][3] = 0.00215259;
    PC3_dphi_mean[0][1][1][4] = -0.00145616;
    PC3_dphi_mean[0][1][1][5] = 0.000911715;
    PC3_dphi_mean[0][1][1][6] = -0.000132424;
    PC3_dz_sigma[0][1][1][0] = 0.617545;
    PC3_dz_sigma[0][1][1][1] = 0.131991;
    PC3_dz_sigma[0][1][1][2] = -0.158819;
    PC3_dz_sigma[0][1][1][3] = 0.0137637;
    PC3_dz_sigma[0][1][1][4] = 0.0395522;
    PC3_dz_sigma[0][1][1][5] = -0.00939925;
    PC3_dz_sigma[0][1][1][6] = 0.545593;
    PC3_dz_sigma[0][1][1][7] = 0.092546;
    PC3_dz_mean[0][1][1][0] = 4.2694;
    PC3_dz_mean[0][1][1][1] = -0.298671;
    PC3_dz_mean[0][1][1][2] = -0.249645;
    PC3_dz_mean[0][1][1][3] = -4.63561;
    PC3_dz_mean[0][1][1][4] = 2.81513;
    PC3_dz_mean[0][1][1][5] = -1.52228;
    PC3_dz_mean[0][1][1][6] = 0.268038;
    PC3_dphi_sigma[0][1][2][0] = 0.0122152;
    PC3_dphi_sigma[0][1][2][1] = -0.00048385;
    PC3_dphi_sigma[0][1][2][2] = -0.00419883;
    PC3_dphi_sigma[0][1][2][3] = 0.00282399;
    PC3_dphi_sigma[0][1][2][4] = -0.000731169;
    PC3_dphi_sigma[0][1][2][5] = 6.75072e-05;
    PC3_dphi_sigma[0][1][2][6] = -0.00887483;
    PC3_dphi_sigma[0][1][2][7] = 0.00124395;
    PC3_dphi_mean[0][1][2][0] = -0.000278494;
    PC3_dphi_mean[0][1][2][1] = 3.77329e-05;
    PC3_dphi_mean[0][1][2][2] = -0.00011134;
    PC3_dphi_mean[0][1][2][3] = 0.00176989;
    PC3_dphi_mean[0][1][2][4] = -0.00103349;
    PC3_dphi_mean[0][1][2][5] = 0.000679772;
    PC3_dphi_mean[0][1][2][6] = -7.9171e-05;
    PC3_dz_sigma[0][1][2][0] = -1.17735;
    PC3_dz_sigma[0][1][2][1] = 0.629805;
    PC3_dz_sigma[0][1][2][2] = 0.417209;
    PC3_dz_sigma[0][1][2][3] = -0.579732;
    PC3_dz_sigma[0][1][2][4] = 0.231733;
    PC3_dz_sigma[0][1][2][5] = -0.0306441;
    PC3_dz_sigma[0][1][2][6] = 1.76816;
    PC3_dz_sigma[0][1][2][7] = 0.0365536;
    PC3_dz_mean[0][1][2][0] = 37.1796;
    PC3_dz_mean[0][1][2][1] = -1.9633;
    PC3_dz_mean[0][1][2][2] = 78.1193;
    PC3_dz_mean[0][1][2][3] = -93.311;
    PC3_dz_mean[0][1][2][4] = -25.3175;
    PC3_dz_mean[0][1][2][5] = 6.79281;
    PC3_dz_mean[0][1][2][6] = -0.811178;
    PC3_dphi_sigma[0][1][3][0] = 0.0114451;
    PC3_dphi_sigma[0][1][3][1] = -0.000207603;
    PC3_dphi_sigma[0][1][3][2] = -0.00393067;
    PC3_dphi_sigma[0][1][3][3] = 0.00259188;
    PC3_dphi_sigma[0][1][3][4] = -0.000684875;
    PC3_dphi_sigma[0][1][3][5] = 6.76693e-05;
    PC3_dphi_sigma[0][1][3][6] = -0.00853395;
    PC3_dphi_sigma[0][1][3][7] = 0.0013058;
    PC3_dphi_mean[0][1][3][0] = -0.0010334;
    PC3_dphi_mean[0][1][3][1] = 0.000155323;
    PC3_dphi_mean[0][1][3][2] = 0.00014147;
    PC3_dphi_mean[0][1][3][3] = 0.00249875;
    PC3_dphi_mean[0][1][3][4] = -0.00159155;
    PC3_dphi_mean[0][1][3][5] = 0.000960927;
    PC3_dphi_mean[0][1][3][6] = -0.000113687;
    PC3_dz_sigma[0][1][3][0] = 5.48185;
    PC3_dz_sigma[0][1][3][1] = -1.86031;
    PC3_dz_sigma[0][1][3][2] = -1.19995;
    PC3_dz_sigma[0][1][3][3] = 1.73082;
    PC3_dz_sigma[0][1][3][4] = -0.667103;
    PC3_dz_sigma[0][1][3][5] = 0.0848986;
    PC3_dz_sigma[0][1][3][6] = -2.45318;
    PC3_dz_sigma[0][1][3][7] = 0.218741;
    PC3_dz_mean[0][1][3][0] = 47.3205;
    PC3_dz_mean[0][1][3][1] = -2.52294;
    PC3_dz_mean[0][1][3][2] = 99.1382;
    PC3_dz_mean[0][1][3][3] = -118.894;
    PC3_dz_mean[0][1][3][4] = -31.7421;
    PC3_dz_mean[0][1][3][5] = 8.39655;
    PC3_dz_mean[0][1][3][6] = -0.98359;
    PC3_dphi_sigma[1][0][0][0] = 0.016046;
    PC3_dphi_sigma[1][0][0][1] = -0.0121742;
    PC3_dphi_sigma[1][0][0][2] = 0.0085613;
    PC3_dphi_sigma[1][0][0][3] = -0.00379593;
    PC3_dphi_sigma[1][0][0][4] = 0.000902427;
    PC3_dphi_sigma[1][0][0][5] = -8.51348e-05;
    PC3_dphi_sigma[1][0][0][6] = -0.00786146;
    PC3_dphi_sigma[1][0][0][7] = 0.00097077;
    PC3_dphi_mean[1][0][0][0] = -0.00863623;
    PC3_dphi_mean[1][0][0][1] = 0.00103328;
    PC3_dphi_mean[1][0][0][2] = 0.00118707;
    PC3_dphi_mean[1][0][0][3] = 0.00915944;
    PC3_dphi_mean[1][0][0][4] = -0.00639321;
    PC3_dphi_mean[1][0][0][5] = 0.00334126;
    PC3_dphi_mean[1][0][0][6] = -0.000644774;
    PC3_dz_sigma[1][0][0][0] = -8.40922;
    PC3_dz_sigma[1][0][0][1] = 1.97312;
    PC3_dz_sigma[1][0][0][2] = 2.13815;
    PC3_dz_sigma[1][0][0][3] = -2.03561;
    PC3_dz_sigma[1][0][0][4] = 0.643724;
    PC3_dz_sigma[1][0][0][5] = -0.0711101;
    PC3_dz_sigma[1][0][0][6] = 7.55062;
    PC3_dz_sigma[1][0][0][7] = -0.391817;
    PC3_dz_mean[1][0][0][0] = -15.3179;
    PC3_dz_mean[1][0][0][1] = 0.87383;
    PC3_dz_mean[1][0][0][2] = -37.2904;
    PC3_dz_mean[1][0][0][3] = 44.2011;
    PC3_dz_mean[1][0][0][4] = 12.0846;
    PC3_dz_mean[1][0][0][5] = -3.23528;
    PC3_dz_mean[1][0][0][6] = 0.377292;
    PC3_dphi_sigma[1][0][1][0] = -0.0604241;
    PC3_dphi_sigma[1][0][1][1] = 0.0585088;
    PC3_dphi_sigma[1][0][1][2] = -0.0403609;
    PC3_dphi_sigma[1][0][1][3] = 0.0160338;
    PC3_dphi_sigma[1][0][1][4] = -0.00331509;
    PC3_dphi_sigma[1][0][1][5] = 0.000276534;
    PC3_dphi_sigma[1][0][1][6] = 0.0323479;
    PC3_dphi_sigma[1][0][1][7] = -0.000461992;
    PC3_dphi_mean[1][0][1][0] = -0.00635823;
    PC3_dphi_mean[1][0][1][1] = 0.000750327;
    PC3_dphi_mean[1][0][1][2] = 0.000345353;
    PC3_dphi_mean[1][0][1][3] = 0.00697099;
    PC3_dphi_mean[1][0][1][4] = -0.00477804;
    PC3_dphi_mean[1][0][1][5] = 0.00271135;
    PC3_dphi_mean[1][0][1][6] = -0.000557961;
    PC3_dz_sigma[1][0][1][0] = -4.40119;
    PC3_dz_sigma[1][0][1][1] = 1.2313;
    PC3_dz_sigma[1][0][1][2] = 1.09218;
    PC3_dz_sigma[1][0][1][3] = -1.11825;
    PC3_dz_sigma[1][0][1][4] = 0.360486;
    PC3_dz_sigma[1][0][1][5] = -0.0398447;
    PC3_dz_sigma[1][0][1][6] = 4.47638;
    PC3_dz_sigma[1][0][1][7] = -0.159996;
    PC3_dz_mean[1][0][1][0] = 1.53801;
    PC3_dz_mean[1][0][1][1] = -0.0801517;
    PC3_dz_mean[1][0][1][2] = -0.424151;
    PC3_dz_mean[1][0][1][3] = 0.921487;
    PC3_dz_mean[1][0][1][4] = -0.477416;
    PC3_dz_mean[1][0][1][5] = 0.364406;
    PC3_dz_mean[1][0][1][6] = -0.0795986;
    PC3_dphi_sigma[1][0][2][0] = -0.0249287;
    PC3_dphi_sigma[1][0][2][1] = 0.0243667;
    PC3_dphi_sigma[1][0][2][2] = -0.0161154;
    PC3_dphi_sigma[1][0][2][3] = 0.00606422;
    PC3_dphi_sigma[1][0][2][4] = -0.00118146;
    PC3_dphi_sigma[1][0][2][5] = 9.34145e-05;
    PC3_dphi_sigma[1][0][2][6] = 0.0141866;
    PC3_dphi_sigma[1][0][2][7] = 0.000119749;
    PC3_dphi_mean[1][0][2][0] = -0.00666782;
    PC3_dphi_mean[1][0][2][1] = 0.000802087;
    PC3_dphi_mean[1][0][2][2] = 0.000870413;
    PC3_dphi_mean[1][0][2][3] = 0.00696612;
    PC3_dphi_mean[1][0][2][4] = -0.00496959;
    PC3_dphi_mean[1][0][2][5] = 0.00263814;
    PC3_dphi_mean[1][0][2][6] = -0.000515992;
    PC3_dz_sigma[1][0][2][0] = -6.76225;
    PC3_dz_sigma[1][0][2][1] = 2.027;
    PC3_dz_sigma[1][0][2][2] = 1.55984;
    PC3_dz_sigma[1][0][2][3] = -1.6947;
    PC3_dz_sigma[1][0][2][4] = 0.553417;
    PC3_dz_sigma[1][0][2][5] = -0.0616243;
    PC3_dz_sigma[1][0][2][6] = 6.12159;
    PC3_dz_sigma[1][0][2][7] = -0.250396;
    PC3_dz_mean[1][0][2][0] = -0.285685;
    PC3_dz_mean[1][0][2][1] = 0.182556;
    PC3_dz_mean[1][0][2][2] = 0.0130681;
    PC3_dz_mean[1][0][2][3] = 2.91832;
    PC3_dz_mean[1][0][2][4] = -1.82837;
    PC3_dz_mean[1][0][2][5] = 1.0524;
    PC3_dz_mean[1][0][2][6] = -0.197782;
    PC3_dphi_sigma[1][0][3][0] = 0.00123717;
    PC3_dphi_sigma[1][0][3][1] = 0.000888078;
    PC3_dphi_sigma[1][0][3][2] = -0.000404167;
    PC3_dphi_sigma[1][0][3][3] = -9.71368e-05;
    PC3_dphi_sigma[1][0][3][4] = 9.26291e-05;
    PC3_dphi_sigma[1][0][3][5] = -1.38421e-05;
    PC3_dphi_sigma[1][0][3][6] = 0.000274515;
    PC3_dphi_sigma[1][0][3][7] = 0.000624803;
    PC3_dphi_mean[1][0][3][0] = -0.00721392;
    PC3_dphi_mean[1][0][3][1] = 0.000866031;
    PC3_dphi_mean[1][0][3][2] = 0.00055739;
    PC3_dphi_mean[1][0][3][3] = 0.00786727;
    PC3_dphi_mean[1][0][3][4] = -0.00521809;
    PC3_dphi_mean[1][0][3][5] = 0.00284577;
    PC3_dphi_mean[1][0][3][6] = -0.000560722;
    PC3_dz_sigma[1][0][3][0] = -8.62531;
    PC3_dz_sigma[1][0][3][1] = 2.61281;
    PC3_dz_sigma[1][0][3][2] = 1.85868;
    PC3_dz_sigma[1][0][3][3] = -2.05919;
    PC3_dz_sigma[1][0][3][4] = 0.676659;
    PC3_dz_sigma[1][0][3][5] = -0.0753775;
    PC3_dz_sigma[1][0][3][6] = 7.45611;
    PC3_dz_sigma[1][0][3][7] = -0.301716;
    PC3_dz_mean[1][0][3][0] = -0.634014;
    PC3_dz_mean[1][0][3][1] = 0.197057;
    PC3_dz_mean[1][0][3][2] = -0.0514932;
    PC3_dz_mean[1][0][3][3] = 3.65195;
    PC3_dz_mean[1][0][3][4] = -2.30898;
    PC3_dz_mean[1][0][3][5] = 1.36396;
    PC3_dz_mean[1][0][3][6] = -0.256381;
    PC3_dphi_sigma[1][1][0][0] = 0.0646938;
    PC3_dphi_sigma[1][1][0][1] = -0.0464303;
    PC3_dphi_sigma[1][1][0][2] = 0.0257423;
    PC3_dphi_sigma[1][1][0][3] = -0.00874242;
    PC3_dphi_sigma[1][1][0][4] = 0.00165601;
    PC3_dphi_sigma[1][1][0][5] = -0.000133837;
    PC3_dphi_sigma[1][1][0][6] = -0.0365578;
    PC3_dphi_sigma[1][1][0][7] = 0.00220936;
    PC3_dphi_mean[1][1][0][0] = 0.0080248;
    PC3_dphi_mean[1][1][0][1] = -0.00108675;
    PC3_dphi_mean[1][1][0][2] = -0.00100547;
    PC3_dphi_mean[1][1][0][3] = -0.0114753;
    PC3_dphi_mean[1][1][0][4] = 0.00749961;
    PC3_dphi_mean[1][1][0][5] = -0.00377583;
    PC3_dphi_mean[1][1][0][6] = 0.00068125;
    PC3_dz_sigma[1][1][0][0] = 45.3535;
    PC3_dz_sigma[1][1][0][1] = -42.7555;
    PC3_dz_sigma[1][1][0][2] = 30.3547;
    PC3_dz_sigma[1][1][0][3] = -12.6131;
    PC3_dz_sigma[1][1][0][4] = 2.75366;
    PC3_dz_sigma[1][1][0][5] = -0.243003;
    PC3_dz_sigma[1][1][0][6] = -22.3346;
    PC3_dz_sigma[1][1][0][7] = 0.828266;
    PC3_dz_mean[1][1][0][0] = 38.5731;
    PC3_dz_mean[1][1][0][1] = -2.15885;
    PC3_dz_mean[1][1][0][2] = 78.5416;
    PC3_dz_mean[1][1][0][3] = -93.6889;
    PC3_dz_mean[1][1][0][4] = -25.8049;
    PC3_dz_mean[1][1][0][5] = 7.04685;
    PC3_dz_mean[1][1][0][6] = -0.861203;
    PC3_dphi_sigma[1][1][1][0] = 0.0142619;
    PC3_dphi_sigma[1][1][1][1] = -0.00219144;
    PC3_dphi_sigma[1][1][1][2] = -0.00364306;
    PC3_dphi_sigma[1][1][1][3] = 0.00289396;
    PC3_dphi_sigma[1][1][1][4] = -0.000798642;
    PC3_dphi_sigma[1][1][1][5] = 7.6334e-05;
    PC3_dphi_sigma[1][1][1][6] = -0.00931321;
    PC3_dphi_sigma[1][1][1][7] = 0.00115287;
    PC3_dphi_mean[1][1][1][0] = 0.0136203;
    PC3_dphi_mean[1][1][1][1] = -0.00172316;
    PC3_dphi_mean[1][1][1][2] = -0.00159705;
    PC3_dphi_mean[1][1][1][3] = -0.0182635;
    PC3_dphi_mean[1][1][1][4] = 0.0117824;
    PC3_dphi_mean[1][1][1][5] = -0.0060426;
    PC3_dphi_mean[1][1][1][6] = 0.00107739;
    PC3_dz_sigma[1][1][1][0] = -3.99546;
    PC3_dz_sigma[1][1][1][1] = 1.48173;
    PC3_dz_sigma[1][1][1][2] = 0.779156;
    PC3_dz_sigma[1][1][1][3] = -0.979448;
    PC3_dz_sigma[1][1][1][4] = 0.33136;
    PC3_dz_sigma[1][1][1][5] = -0.0377006;
    PC3_dz_sigma[1][1][1][6] = 3.91251;
    PC3_dz_sigma[1][1][1][7] = -0.112806;
    PC3_dz_mean[1][1][1][0] = 2.51656;
    PC3_dz_mean[1][1][1][1] = -0.119902;
    PC3_dz_mean[1][1][1][2] = -0.328036;
    PC3_dz_mean[1][1][1][3] = -0.753631;
    PC3_dz_mean[1][1][1][4] = 0.694377;
    PC3_dz_mean[1][1][1][5] = -0.371301;
    PC3_dz_mean[1][1][1][6] = 0.0642998;
    PC3_dphi_sigma[1][1][2][0] = 0.0174178;
    PC3_dphi_sigma[1][1][2][1] = -0.00224271;
    PC3_dphi_sigma[1][1][2][2] = -0.00515685;
    PC3_dphi_sigma[1][1][2][3] = 0.0039512;
    PC3_dphi_sigma[1][1][2][4] = -0.00108954;
    PC3_dphi_sigma[1][1][2][5] = 0.000105566;
    PC3_dphi_sigma[1][1][2][6] = -0.0118577;
    PC3_dphi_sigma[1][1][2][7] = 0.00132922;
    PC3_dphi_mean[1][1][2][0] = 0.0105506;
    PC3_dphi_mean[1][1][2][1] = -0.00135111;
    PC3_dphi_mean[1][1][2][2] = -0.000847141;
    PC3_dphi_mean[1][1][2][3] = -0.0148385;
    PC3_dphi_mean[1][1][2][4] = 0.00928798;
    PC3_dphi_mean[1][1][2][5] = -0.00479436;
    PC3_dphi_mean[1][1][2][6] = 0.000855817;
    PC3_dz_sigma[1][1][2][0] = -3.55894;
    PC3_dz_sigma[1][1][2][1] = 1.83289;
    PC3_dz_sigma[1][1][2][2] = 0.584148;
    PC3_dz_sigma[1][1][2][3] = -1.10762;
    PC3_dz_sigma[1][1][2][4] = 0.430283;
    PC3_dz_sigma[1][1][2][5] = -0.053776;
    PC3_dz_sigma[1][1][2][6] = 3.32434;
    PC3_dz_sigma[1][1][2][7] = -0.0306715;
    PC3_dz_mean[1][1][2][0] = 2.07879;
    PC3_dz_mean[1][1][2][1] = -0.0388894;
    PC3_dz_mean[1][1][2][2] = -0.110096;
    PC3_dz_mean[1][1][2][3] = -0.348103;
    PC3_dz_mean[1][1][2][4] = 0.47408;
    PC3_dz_mean[1][1][2][5] = -0.31983;
    PC3_dz_mean[1][1][2][6] = 0.0662743;
    PC3_dphi_sigma[1][1][3][0] = 0.0204408;
    PC3_dphi_sigma[1][1][3][1] = -0.00361899;
    PC3_dphi_sigma[1][1][3][2] = -0.00537072;
    PC3_dphi_sigma[1][1][3][3] = 0.00457832;
    PC3_dphi_sigma[1][1][3][4] = -0.00135418;
    PC3_dphi_sigma[1][1][3][5] = 0.000141336;
    PC3_dphi_sigma[1][1][3][6] = -0.0137605;
    PC3_dphi_sigma[1][1][3][7] = 0.00142213;
    PC3_dphi_mean[1][1][3][0] = 0.0116472;
    PC3_dphi_mean[1][1][3][1] = -0.00142397;
    PC3_dphi_mean[1][1][3][2] = -0.00110136;
    PC3_dphi_mean[1][1][3][3] = -0.0163254;
    PC3_dphi_mean[1][1][3][4] = 0.0106028;
    PC3_dphi_mean[1][1][3][5] = -0.00550381;
    PC3_dphi_mean[1][1][3][6] = 0.000974182;
    PC3_dz_sigma[1][1][3][0] = -56.1933;
    PC3_dz_sigma[1][1][3][1] = 52.5445;
    PC3_dz_sigma[1][1][3][2] = -36.4096;
    PC3_dz_sigma[1][1][3][3] = 14.8932;
    PC3_dz_sigma[1][1][3][4] = -3.23742;
    PC3_dz_sigma[1][1][3][5] = 0.287291;
    PC3_dz_sigma[1][1][3][6] = 30.5121;
    PC3_dz_sigma[1][1][3][7] = -0.937191;
    PC3_dz_mean[1][1][3][0] = 2.89381;
    PC3_dz_mean[1][1][3][1] = -0.168083;
    PC3_dz_mean[1][1][3][2] = -0.402237;
    PC3_dz_mean[1][1][3][3] = -0.920308;
    PC3_dz_mean[1][1][3][4] = 0.837854;
    PC3_dz_mean[1][1][3][5] = -0.415534;
    PC3_dz_mean[1][1][3][6] = 0.0750072;

PC2_dphifirst_mean[0][0][0] = -0.099;
PC2_dphifirst_sigma[0][0][0] = 0;
PC2_dzfirst_mean[0][0][0] = -9.8;
PC2_dzfirst_sigma[0][0][0] = 0;
PC2_dphifirst_mean[0][1][0] = -0.099;
PC2_dphifirst_sigma[0][1][0] = 0;
PC2_dzfirst_mean[0][1][0] = -9.8;
PC2_dzfirst_sigma[0][1][0] = 0;
PC2_dphifirst_mean[1][0][0] = -0.00449517;
PC2_dphifirst_sigma[1][0][0] = 0.0068005;
PC2_dzfirst_mean[1][0][0] = 0.771919;
PC2_dzfirst_sigma[1][0][0] = 3.13075;
PC2_dphifirst_mean[1][1][0] = 0.00169776;
PC2_dphifirst_sigma[1][1][0] = 0.00643468;
PC2_dzfirst_mean[1][1][0] = 0.592235;
PC2_dzfirst_sigma[1][1][0] = 3.23693;
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
PC2_dphi_sigma[1][0][0][0] = 0.00535198;
PC2_dphi_sigma[1][0][0][1] = 0.00121763;
PC2_dphi_sigma[1][0][0][2] = -0.00225419;
PC2_dphi_sigma[1][0][0][3] = 0.00103945;
PC2_dphi_sigma[1][0][0][4] = -0.000202771;
PC2_dphi_sigma[1][0][0][5] = 1.5425e-05;
PC2_dphi_sigma[1][0][0][6] = -0.00393487;
PC2_dphi_sigma[1][0][0][7] = 0.000884382;
PC2_dphi_mean[1][0][0][0] = -0.0220565;
PC2_dphi_mean[1][0][0][1] = 0.00172072;
PC2_dphi_mean[1][0][0][2] = -0.0302136;
PC2_dphi_mean[1][0][0][3] = 0.0445651;
PC2_dphi_mean[1][0][0][4] = 0.00486954;
PC2_dphi_mean[1][0][0][5] = 0.000112672;
PC2_dphi_mean[1][0][0][6] = -0.000241694;
PC2_dz_sigma[1][0][0][0] = 0.946145;
PC2_dz_sigma[1][0][0][1] = -4.72191;
PC2_dz_sigma[1][0][0][2] = 5.4966;
PC2_dz_sigma[1][0][0][3] = -2.69319;
PC2_dz_sigma[1][0][0][4] = 0.587286;
PC2_dz_sigma[1][0][0][5] = -0.0452613;
PC2_dz_sigma[1][0][0][6] = 1.68208;
PC2_dz_sigma[1][0][0][7] = -0.0612782;
PC2_dz_mean[1][0][0][0] = 9.90721;
PC2_dz_mean[1][0][0][1] = -0.586544;
PC2_dz_mean[1][0][0][2] = 17.4751;
PC2_dz_mean[1][0][0][3] = -21.0157;
PC2_dz_mean[1][0][0][4] = -6.01797;
PC2_dz_mean[1][0][0][5] = 1.83756;
PC2_dz_mean[1][0][0][6] = -0.257753;
PC2_dphi_sigma[1][1][0][0] = 0.0560873;
PC2_dphi_sigma[1][1][0][1] = -0.0377321;
PC2_dphi_sigma[1][1][0][2] = 0.01983;
PC2_dphi_sigma[1][1][0][3] = -0.0062713;
PC2_dphi_sigma[1][1][0][4] = 0.00105785;
PC2_dphi_sigma[1][1][0][5] = -7.06643e-05;
PC2_dphi_sigma[1][1][0][6] = -0.0329519;
PC2_dphi_sigma[1][1][0][7] = 0.00210183;
PC2_dphi_mean[1][1][0][0] = -0.061032;
PC2_dphi_mean[1][1][0][1] = 0.00269688;
PC2_dphi_mean[1][1][0][2] = -0.156166;
PC2_dphi_mean[1][1][0][3] = 0.167925;
PC2_dphi_mean[1][1][0][4] = 0.0620376;
PC2_dphi_mean[1][1][0][5] = -0.019824;
PC2_dphi_mean[1][1][0][6] = 0.00276784;
PC2_dz_sigma[1][1][0][0] = 38.4128;
PC2_dz_sigma[1][1][0][1] = -33.7379;
PC2_dz_sigma[1][1][0][2] = 22.0953;
PC2_dz_sigma[1][1][0][3] = -8.1767;
PC2_dz_sigma[1][1][0][4] = 1.51882;
PC2_dz_sigma[1][1][0][5] = -0.107038;
PC2_dz_sigma[1][1][0][6] = -19.6686;
PC2_dz_sigma[1][1][0][7] = 0.823627;
PC2_dz_mean[1][1][0][0] = 42.1633;
PC2_dz_mean[1][1][0][1] = -2.53311;
PC2_dz_mean[1][1][0][2] = 82.2055;
PC2_dz_mean[1][1][0][3] = -100.731;
PC2_dz_mean[1][1][0][4] = -25.8256;
PC2_dz_mean[1][1][0][5] = 6.8564;
PC2_dz_mean[1][1][0][6] = -0.820653;

  return;
}
