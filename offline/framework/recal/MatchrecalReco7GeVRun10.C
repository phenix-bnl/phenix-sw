#include <MatchrecalReco7GeVRun10.h>

#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>
#include <RunHeader.h>

#include <getClass.h>
#include <PHCompositeNode.h>

#include <iostream>

using namespace std;

//_____________________________________________________________________________________________________________________________
MatchrecalReco7GeVRun10::MatchrecalReco7GeVRun10(const string &name) :
  Recalibrator(name)
{
  memset(p0_pc2_m_dp, 0, sizeof(p0_pc2_m_dp));
  memset(p1_pc2_m_dp, 0, sizeof(p1_pc2_m_dp));
  memset(p2_pc2_m_dp, 0, sizeof(p2_pc2_m_dp));
  memset(p0_pc2_s_dp, 0, sizeof(p0_pc2_s_dp));
  memset(p1_pc2_s_dp, 0, sizeof(p1_pc2_s_dp));
  memset(p0_pc2_m_dz, 0, sizeof(p0_pc2_m_dz));
  memset(p1_pc2_m_dz, 0, sizeof(p1_pc2_m_dz));
  memset(p2_pc2_m_dz, 0, sizeof(p2_pc2_m_dz));
  memset(p0_pc2_s_dz, 0, sizeof(p0_pc2_s_dz));
  memset(p1_pc2_s_dz, 0, sizeof(p1_pc2_s_dz));
  memset(p0_pc3_m_dp, 0, sizeof(p0_pc3_m_dp));
  memset(p1_pc3_m_dp, 0, sizeof(p1_pc3_m_dp));
  memset(p2_pc3_m_dp, 0, sizeof(p2_pc3_m_dp));
  memset(p0_pc3_s_dp, 0, sizeof(p0_pc3_s_dp));
  memset(p1_pc3_s_dp, 0, sizeof(p1_pc3_s_dp));
  memset(p0_pc3_m_dz, 0, sizeof(p0_pc3_m_dz));
  memset(p1_pc3_m_dz, 0, sizeof(p1_pc3_m_dz));
  memset(p2_pc3_m_dz, 0, sizeof(p2_pc3_m_dz));
  memset(p0_pc3_s_dz, 0, sizeof(p0_pc3_s_dz));
  memset(p1_pc3_s_dz, 0, sizeof(p1_pc3_s_dz));
  memset(p0_emc_m_dp, 0, sizeof(p0_emc_m_dp));
  memset(p1_emc_m_dp, 0, sizeof(p1_emc_m_dp));
  memset(p2_emc_m_dp, 0, sizeof(p2_emc_m_dp));
  memset(p0_emc_s_dp, 0, sizeof(p0_emc_s_dp));
  memset(p1_emc_s_dp, 0, sizeof(p1_emc_s_dp));
  memset(p0_emc_m_dz, 0, sizeof(p0_emc_m_dz));
  memset(p1_emc_m_dz, 0, sizeof(p1_emc_m_dz));
  memset(p2_emc_m_dz, 0, sizeof(p2_emc_m_dz));
  memset(p0_emc_s_dz, 0, sizeof(p0_emc_s_dz));
  memset(p1_emc_s_dz, 0, sizeof(p1_emc_s_dz));

  baseclasses.insert("PHCentralTrackv23");
  baseclasses.insert("PHCentralTrackv24");
  return;
}

int
MatchrecalReco7GeVRun10::isValidRun(const int RunNumber) const
{
  if(RunNumber >= 315450 && RunNumber <= 318944)
    {
      return 1;
    }
  return 0;
}

//_____________________________________________________________________________________________________________________________
int
MatchrecalReco7GeVRun10::Init(PHCompositeNode *topNode)
{
  //! Matching Calibration Parameters
  InitPars();
  return 0;
}

//_____________________________________________________________________________________________________________________________
int
MatchrecalReco7GeVRun10::InitRun(PHCompositeNode *topNode)
{
  RunHeader* d_runheader = findNode::getClass<RunHeader>(topNode, "RunHeader");
  if(!d_runheader)
    {
      cout << PHWHERE << " RunHeader not found"  << endl;
      return 0;
    }
  int RunNumber = d_runheader->get_RunNumber();
  if(!isValidRun(RunNumber))
    {
      cout << " Not Valid Run! " << endl;
      return -1;
    }

  cout << "---------------------------------------------------------------" << endl;
  cout << " MatchrecalReco7GeVRun10::InitRun() :  run number = " << RunNumber << endl;
  cout << "---------------------------------------------------------------" << endl;
  return 0;
}

//_____________________________________________________________________________________________________________________________
int
MatchrecalReco7GeVRun10::process_event(PHCompositeNode *topNode)
{

  PHCentralTrack *track = findNode::getClass<PHCentralTrack>(topNode, inputnodename.c_str());
  if (!track)
    {
      cout << PHWHERE << "Could not find PHCentralTrack !" << endl;
      return 0;
    }



  unsigned int trks = track->get_npart();
  for(unsigned int itrk = 0; itrk < trks; itrk++)
    {

      PHSnglCentralTrack* sngltrk = track->get_track(itrk);

      //! track variables
      float mom      = sngltrk->get_mom();
      float the0     = sngltrk->get_the0();
      float phi0     = sngltrk->get_phi0();
      float zed      = sngltrk->get_zed();
      float phi1     = sngltrk->get_phi();
      float pt       = mom * sin(the0);
      float charge   = sngltrk->get_charge();
      float alpha    = sngltrk->get_alpha();

      //! 0-10 bins from -75cm to +75cm
      int ized  = Zed(zed);
      if(ized < 0)continue;

      //! Charge not found
      if(charge == 0 || alpha == 0)continue;
      //! Charge for matching calibrations
      int ich = 0;
      if(charge > 0)ich = 1;

      //! Apply the0 and phi0 cut here
      if(the0 < -100 || phi0 < -100)continue;

      //! Arm and sector  (Central Arm)
      int   isec = sngltrk->get_sect();
      int   iarm = 0;             //! East
      if (cos(phi1) > 0) iarm = 1; //! West

      //! all the sectors of emc sectors from 0-7
      int   isd  = iarm * 4 + isec;
      if (isd < -1 || isd > 8) continue;
      //! isd = 0 & 1 (East PbGl)
      //! isd = 2 & 3 (East PbSc)
      //! isd = 4,5,6,7 (West PbSc)

      //! EMC calibrations
      int iemc = -1;
      if(isd == 0 || isd == 1)iemc = 0;
      else if(isd == 2 || isd == 3)iemc = 1;
      else if(isd > 3)iemc = 2;
      if(iemc < 0)continue;

      //! pc2
      float pc2dphi  = sngltrk->get_pc2dphi();
      float pc2dz    = sngltrk->get_pc2dz();

      //! pc3
      float pc3dphi  = sngltrk->get_pc3dphi();
      float pc3dz    = sngltrk->get_pc3dz();

      //     //! tofe
      //     float   tofdphi  = sngltrk->get_tofdphi();
      //     float   tofdz    = sngltrk->get_tofdz();

      //     //! tofw
      //     float   tofwdphi = sngltrk->get_tofwdphi();
      //     float   tofwdz   = sngltrk->get_tofwdz();

      //! emc
      float   emcdphi  = sngltrk->get_emcdphi();
      float   emcdz    = sngltrk->get_emcdz();

      float pc2sdphi = -9999.;
      float pc2sdz   = -9999.;
      float pc3sdphi = -9999.;
      float pc3sdz   = -9999.;
      float emcsdphi = -9999.;
      float emcsdz   = -9999.;
      //    float tofsdphi = -9999.;
      //    float tofsdz   = -9999.;
      //    float tofwsdphi= -9999.;
      //    float tofwsdz  = -9999.;


      //! *************************************************************************************//
      //!
      //! To get the sigmalized dphi and dz variables for each detector
      //! Function to return sdval (val=dphi||dz) GetSval(int itype,int idet,int isub,int ized,int ich,float fpt,float fdval)
      //! GetFineTSval(int iarm,int icent,float pt,float pc3sdz) : Remove the centrality dependence in the PC3 sdZ

      //! itype
      //! 0 : dphi
      //! 1 : dz

      //! idet
      //! 0 : pc2 (only ized as this is only one side no isub)
      //! 1 : pc3 (arm wise)
      //! isub (iarm)
      //! 0 : Arm  (East)
      //! 1 : Arm  (West)

      //! 2 : emc
      //! isub (iemc)
      //! 0 : (PbGlE)
      //! 1 : (PbScE)
      //! 2 : (PbScW)

      //! 3 : tof
      //! isub (iarm)
      //! 0 : Arm (East)
      //! 1 : Arm (West)

      //! *************************************************************************************//
      //!
      //! PC3
      if(pc3dphi > -9999 || pc3dz > -9999)
	{
	  pc3sdphi = GetSval(0, 1, iarm, ized, ich, pt, pc3dphi);
	  pc3sdz   = GetSval(1, 1, iarm, ized, ich, pt, pc3dz);

	  //! Centrality dependence
	  //      pc3sdz   = GetFineTSval(iarm,cent,pt,pc3sdz);
	}

      //! PC2
      if(pc2dphi > -9999 || pc2dz > -9999)
	{
	  pc2sdphi = GetSval(0, 0, iarm, ized, ich, pt, pc2dphi);
	  pc2sdz   = GetSval(1, 0, iarm, ized, ich, pt, pc2dz);
	}

      //! EMC
      if(emcdphi > -9999 || emcdz > -9999)
	{
	  emcsdphi = GetSval(0, 2, iemc, ized, ich, pt, emcdphi);
	  emcsdz   = GetSval(1, 2, iemc, ized, ich, pt, emcdz);
	}

      //     //! ToF east
      //     if(tofdphi > -9999 || tofdz > -9999){ //! east
      //       tofsdphi = GetSval(0,3,iarm,ized,ich,pt,tofdphi);
      //       tofsdz   = GetSval(1,3,iarm,ized,ich,pt,tofdz);
      //     }

      //     //! ToF west
      //     if(tofwdphi > -9999 || tofwdz > -9999){ //! west
      //       tofwsdphi = GetSval(0,3,iarm,ized,ich,pt,tofwdphi);
      //       tofwsdz   = GetSval(1,3,iarm,ized,ich,pt,tofwdz);
      //     }

      // Set all variables
      sngltrk->set_pc2sdphi(pc2sdphi);
      sngltrk->set_pc2sdz(pc2sdz);
      sngltrk->set_pc3sdphi(pc3sdphi);
      sngltrk->set_pc3sdz(pc3sdz);
      //    sngltrk->set_tofsdphi(tofsdphi);
      //    sngltrk->set_tofsdz(tofsdz);
      //    sngltrk->set_tofwsdphi(tofwsdphi);
      //    sngltrk->set_tofwsdz(tofwsdz);
      sngltrk->set_emcsdphi(emcsdphi);
      sngltrk->set_emcsdz(emcsdz);

    }//! itrk loop ends

  return 0;
}
//_____________________________________________________________________________________________________________________________
int
MatchrecalReco7GeVRun10::Zed(const float zed) const
{
  int ized = (int)(NZBN * (zed + 75.) / 150.);
  if(ized < 0 || ized >= NZBN)
    {
      return -9999;
    }
  return ized;
}
//_____________________________________________________________________________________________________________________________

float
MatchrecalReco7GeVRun10::GetSval(const int itype, const int idet, const int isub, const int ized, const int ich, const float fpt, const float fdval) const
{
  if(fdval < -9000 || fpt < 0)
    {
      return -9999;
    }

  float p0_m = 0;
  float p1_m = 0;
  float p2_m = 0;
  float p0_s = 1;
  float p1_s = 0;


  if(itype == 0)
    {
      //! dphi
      //cout<<"GetSval :: dPhi ::::::::: "<<endl;
      if(idet == 0)
	{
	  //! PC2
	  p0_m = p0_pc2_m_dp[ized][ich];
	  p1_m = p1_pc2_m_dp[ized][ich];
	  p2_m = p2_pc2_m_dp[ized][ich];
	  p0_s = p0_pc2_s_dp[ized][ich];
	  p1_s = p1_pc2_s_dp[ized][ich];
	}
      else if(idet == 1)
	{
	  //! PC3
	  p0_m = p0_pc3_m_dp[isub][ized][ich];
	  p1_m = p1_pc3_m_dp[isub][ized][ich];
	  p2_m = p2_pc3_m_dp[isub][ized][ich];
	  p0_s = p0_pc3_s_dp[isub][ized][ich];
	  p1_s = p1_pc3_s_dp[isub][ized][ich];
	}
      else if(idet == 2) //! EMC
	{
	  p0_m = p0_emc_m_dp[isub][ized][ich];
	  p1_m = p1_emc_m_dp[isub][ized][ich];
	  p2_m = p2_emc_m_dp[isub][ized][ich];
	  p0_s = p0_emc_s_dp[isub][ized][ich];
	  p1_s = p1_emc_s_dp[isub][ized][ich];
	}
    }
  else if(itype == 1)
    {
      //! dz
      //cout<<"GetSval :: dZ ******** "<<endl;
      if(idet == 0)
	{
	  //! PC2
	  p0_m = p0_pc2_m_dz[ized][ich];
	  p1_m = p1_pc2_m_dz[ized][ich];
	  p2_m = p2_pc2_m_dz[ized][ich];
	  p0_s = p0_pc2_s_dz[ized][ich];
	  p1_s = p1_pc2_s_dz[ized][ich];
	}

      else if(idet == 1)
	{
	  //! PC3
	  p0_m = p0_pc3_m_dz[isub][ized][ich];
	  p1_m = p1_pc3_m_dz[isub][ized][ich];
	  p2_m = p2_pc3_m_dz[isub][ized][ich];
	  p0_s = p0_pc3_s_dz[isub][ized][ich];
	  p1_s = p1_pc3_s_dz[isub][ized][ich];

	}
      else if(idet == 2)
	{
	  //! EMC
	  p0_m = p0_emc_m_dz[isub][ized][ich];
	  p1_m = p1_emc_m_dz[isub][ized][ich];
	  p2_m = p2_emc_m_dz[isub][ized][ich];
	  p0_s = p0_emc_s_dz[isub][ized][ich];
	  p1_s = p1_emc_s_dz[isub][ized][ich];
	}
    }

  //! First pass
  float mean  = p0_m + p1_m / fpt + p2_m / fpt / fpt;
  float sigma = sqrt(p0_s * p0_s / fpt / fpt + p1_s * p1_s);

  float sdval = (fdval - mean) / sigma;
  return sdval;
}

//_____________________________________________________________________________________________________________________________
int MatchrecalReco7GeVRun10::InitPars()
{

  //! PC2 dz neg
  p0_pc2_m_dz[0][0] = -0.645781;
  p1_pc2_m_dz[0][0] = 0.0860037;
  p2_pc2_m_dz[0][0] = 0.0191053;
  p0_pc2_s_dz[0][0] = 0.679913;
  p1_pc2_s_dz[0][0] = 1.60269;
  p0_pc2_m_dz[1][0] = -0.580579;
  p1_pc2_m_dz[1][0] = 0.0553218;
  p2_pc2_m_dz[1][0] = 0.0475528;
  p0_pc2_s_dz[1][0] = 0.642891;
  p1_pc2_s_dz[1][0] = 1.65883;
  p0_pc2_m_dz[2][0] = -0.6548;
  p1_pc2_m_dz[2][0] = 0.0758518;
  p2_pc2_m_dz[2][0] = 0.0268317;
  p0_pc2_s_dz[2][0] = 0.64329;
  p1_pc2_s_dz[2][0] = 1.63918;
  p0_pc2_m_dz[3][0] = -0.662656;
  p1_pc2_m_dz[3][0] = -0.0774599;
  p2_pc2_m_dz[3][0] = 0.0499771;
  p0_pc2_s_dz[3][0] = 0.62528;
  p1_pc2_s_dz[3][0] = 1.65703;
  p0_pc2_m_dz[4][0] = -0.554859;
  p1_pc2_m_dz[4][0] = -0.0628684;
  p2_pc2_m_dz[4][0] = 0.0299451;
  p0_pc2_s_dz[4][0] = 0.600541;
  p1_pc2_s_dz[4][0] = 1.59333;
  p0_pc2_m_dz[5][0] = -0.741266;
  p1_pc2_m_dz[5][0] = -0.122332;
  p2_pc2_m_dz[5][0] = 0.0216188;
  p0_pc2_s_dz[5][0] = 0.601052;
  p1_pc2_s_dz[5][0] = 1.60106;
  p0_pc2_m_dz[6][0] = -0.623609;
  p1_pc2_m_dz[6][0] = -0.192541;
  p2_pc2_m_dz[6][0] = 0.0243726;
  p0_pc2_s_dz[6][0] = 0.611281;
  p1_pc2_s_dz[6][0] = 1.6661;
  p0_pc2_m_dz[7][0] = -0.849229;
  p1_pc2_m_dz[7][0] = -0.00700826;
  p2_pc2_m_dz[7][0] = -0.0469905;
  p0_pc2_s_dz[7][0] = 0.622947;
  p1_pc2_s_dz[7][0] = 1.65194;
  p0_pc2_m_dz[8][0] = -0.778599;
  p1_pc2_m_dz[8][0] = -0.18626;
  p2_pc2_m_dz[8][0] = -0.00696875;
  p0_pc2_s_dz[8][0] = 0.617362;
  p1_pc2_s_dz[8][0] = 1.65738;
  p0_pc2_m_dz[9][0] = -0.57777;
  p1_pc2_m_dz[9][0] = -0.305737;
  p2_pc2_m_dz[9][0] = 0.034357;
  p0_pc2_s_dz[9][0] = 0.645734;
  p1_pc2_s_dz[9][0] = 1.6161;

  //! PC2 dz pos
  p0_pc2_m_dz[0][1] = -0.873904;
  p1_pc2_m_dz[0][1] = 0.234123;
  p2_pc2_m_dz[0][1] = -0.0704833;
  p0_pc2_s_dz[0][1] = 0.728531;
  p1_pc2_s_dz[0][1] = 1.59332;
  p0_pc2_m_dz[1][1] = -0.628804;
  p1_pc2_m_dz[1][1] = 0.0511253;
  p2_pc2_m_dz[1][1] = 0.013198;
  p0_pc2_s_dz[1][1] = 0.71428;
  p1_pc2_s_dz[1][1] = 1.63029;
  p0_pc2_m_dz[2][1] = -0.699992;
  p1_pc2_m_dz[2][1] = 0.0752769;
  p2_pc2_m_dz[2][1] = 0.00344542;
  p0_pc2_s_dz[2][1] = 0.703305;
  p1_pc2_s_dz[2][1] = 1.63356;
  p0_pc2_m_dz[3][1] = -0.74852;
  p1_pc2_m_dz[3][1] = -0.00661142;
  p2_pc2_m_dz[3][1] = 0.0263759;
  p0_pc2_s_dz[3][1] = 0.707283;
  p1_pc2_s_dz[3][1] = 1.62493;
  p0_pc2_m_dz[4][1] = -0.69647;
  p1_pc2_m_dz[4][1] = 0.0945029;
  p2_pc2_m_dz[4][1] = -0.000573567;
  p0_pc2_s_dz[4][1] = 0.674601;
  p1_pc2_s_dz[4][1] = 1.58266;
  p0_pc2_m_dz[5][1] = -0.844573;
  p1_pc2_m_dz[5][1] = 0.0423424;
  p2_pc2_m_dz[5][1] = -0.013942;
  p0_pc2_s_dz[5][1] = 0.672889;
  p1_pc2_s_dz[5][1] = 1.58172;
  p0_pc2_m_dz[6][1] = -0.753985;
  p1_pc2_m_dz[6][1] = 0.0705266;
  p2_pc2_m_dz[6][1] = -0.0214193;
  p0_pc2_s_dz[6][1] = 0.693801;
  p1_pc2_s_dz[6][1] = 1.63884;
  p0_pc2_m_dz[7][1] = -0.685058;
  p1_pc2_m_dz[7][1] = -0.0782864;
  p2_pc2_m_dz[7][1] = 0.0146364;
  p0_pc2_s_dz[7][1] = 0.682429;
  p1_pc2_s_dz[7][1] = 1.64248;
  p0_pc2_m_dz[8][1] = -0.819589;
  p1_pc2_m_dz[8][1] = 0.0608283;
  p2_pc2_m_dz[8][1] = -0.0249299;
  p0_pc2_s_dz[8][1] = 0.711327;
  p1_pc2_s_dz[8][1] = 1.62064;
  p0_pc2_m_dz[9][1] = -0.512063;
  p1_pc2_m_dz[9][1] = -0.168621;
  p2_pc2_m_dz[9][1] = 0.0660573;
  p0_pc2_s_dz[9][1] = 0.737229;
  p1_pc2_s_dz[9][1] = 1.57298;

  //! PC2 dphi neg
  p0_pc2_m_dp[0][0] = -0.000876663;
  p1_pc2_m_dp[0][0] = 9.18467e-05;
  p2_pc2_m_dp[0][0] = -0.000138423;
  p0_pc2_s_dp[0][0] = 0.00115192;
  p1_pc2_s_dp[0][0] = 0.00176996;
  p0_pc2_m_dp[1][0] = -0.000776167;
  p1_pc2_m_dp[1][0] = 1.73591e-05;
  p2_pc2_m_dp[1][0] = 1.62453e-05;
  p0_pc2_s_dp[1][0] = 0.00113719;
  p1_pc2_s_dp[1][0] = 0.00179436;
  p0_pc2_m_dp[2][0] = -0.000689441;
  p1_pc2_m_dp[2][0] = -1.88897e-05;
  p2_pc2_m_dp[2][0] = 9.46431e-05;
  p0_pc2_s_dp[2][0] = 0.00110248;
  p1_pc2_s_dp[2][0] = 0.0017624;
  p0_pc2_m_dp[3][0] = -0.000795284;
  p1_pc2_m_dp[3][0] = 0.000219984;
  p2_pc2_m_dp[3][0] = 7.1105e-05;
  p0_pc2_s_dp[3][0] = 0.00113036;
  p1_pc2_s_dp[3][0] = 0.00158015;
  p0_pc2_m_dp[4][0] = -0.000920397;
  p1_pc2_m_dp[4][0] = 0.000386048;
  p2_pc2_m_dp[4][0] = 4.27338e-05;
  p0_pc2_s_dp[4][0] = 0.00113638;
  p1_pc2_s_dp[4][0] = 0.00154128;
  p0_pc2_m_dp[5][0] = -0.000677175;
  p1_pc2_m_dp[5][0] = 0.000190659;
  p2_pc2_m_dp[5][0] = 8.64593e-05;
  p0_pc2_s_dp[5][0] = 0.00112622;
  p1_pc2_s_dp[5][0] = 0.00171656;
  p0_pc2_m_dp[6][0] = -0.000709168;
  p1_pc2_m_dp[6][0] = 0.000119737;
  p2_pc2_m_dp[6][0] = 9.21637e-05;
  p0_pc2_s_dp[6][0] = 0.00107812;
  p1_pc2_s_dp[6][0] = 0.00177674;
  p0_pc2_m_dp[7][0] = -0.000813491;
  p1_pc2_m_dp[7][0] = 0.000205353;
  p2_pc2_m_dp[7][0] = 1.84916e-05;
  p0_pc2_s_dp[7][0] = 0.00105899;
  p1_pc2_s_dp[7][0] = 0.00174827;
  p0_pc2_m_dp[8][0] = -0.000831048;
  p1_pc2_m_dp[8][0] = 0.000107709;
  p2_pc2_m_dp[8][0] = -1.90871e-05;
  p0_pc2_s_dp[8][0] = 0.00103512;
  p1_pc2_s_dp[8][0] = 0.00173334;
  p0_pc2_m_dp[9][0] = -0.000973537;
  p1_pc2_m_dp[9][0] = 0.00030437;
  p2_pc2_m_dp[9][0] = -0.00018569;
  p0_pc2_s_dp[9][0] = 0.00103507;
  p1_pc2_s_dp[9][0] = 0.00164368;

  //! PC2 dphi pos
  p0_pc2_m_dp[0][1] = -0.00135781;
  p1_pc2_m_dp[0][1] = -0.000216215;
  p2_pc2_m_dp[0][1] = 9.94931e-05;
  p0_pc2_s_dp[0][1] = 0.00138207;
  p1_pc2_s_dp[0][1] = 0.00181652;
  p0_pc2_m_dp[1][1] = -0.00151567;
  p1_pc2_m_dp[1][1] = -8.15385e-05;
  p2_pc2_m_dp[1][1] = -4.28523e-05;
  p0_pc2_s_dp[1][1] = 0.00137588;
  p1_pc2_s_dp[1][1] = 0.00180644;
  p0_pc2_m_dp[2][1] = -0.00140076;
  p1_pc2_m_dp[2][1] = -0.000340907;
  p2_pc2_m_dp[2][1] = -2.44557e-05;
  p0_pc2_s_dp[2][1] = 0.00138818;
  p1_pc2_s_dp[2][1] = 0.00171129;
  p0_pc2_m_dp[3][1] = -0.00140069;
  p1_pc2_m_dp[3][1] = -0.000297654;
  p2_pc2_m_dp[3][1] = -5.62782e-05;
  p0_pc2_s_dp[3][1] = 0.00142414;
  p1_pc2_s_dp[3][1] = 0.00145711;
  p0_pc2_m_dp[4][1] = -0.0015666;
  p1_pc2_m_dp[4][1] = -0.00015178;
  p2_pc2_m_dp[4][1] = -0.000119344;
  p0_pc2_s_dp[4][1] = 0.00140587;
  p1_pc2_s_dp[4][1] = 0.00156244;
  p0_pc2_m_dp[5][1] = -0.00147204;
  p1_pc2_m_dp[5][1] = -0.000240044;
  p2_pc2_m_dp[5][1] = -6.56923e-05;
  p0_pc2_s_dp[5][1] = 0.00140072;
  p1_pc2_s_dp[5][1] = 0.00157479;
  p0_pc2_m_dp[6][1] = -0.00134522;
  p1_pc2_m_dp[6][1] = -0.000319612;
  p2_pc2_m_dp[6][1] = -2.64099e-05;
  p0_pc2_s_dp[6][1] = 0.00135238;
  p1_pc2_s_dp[6][1] = 0.00159335;
  p0_pc2_m_dp[7][1] = -0.00125309;
  p1_pc2_m_dp[7][1] = -0.000250758;
  p2_pc2_m_dp[7][1] = -1.81574e-05;
  p0_pc2_s_dp[7][1] = 0.00131687;
  p1_pc2_s_dp[7][1] = 0.00157818;
  p0_pc2_m_dp[8][1] = -0.00120365;
  p1_pc2_m_dp[8][1] = -0.000189233;
  p2_pc2_m_dp[8][1] = 2.63679e-05;
  p0_pc2_s_dp[8][1] = 0.00130107;
  p1_pc2_s_dp[8][1] = 0.00150649;
  p0_pc2_m_dp[9][1] = -0.00103803;
  p1_pc2_m_dp[9][1] = -0.000355924;
  p2_pc2_m_dp[9][1] = 0.000181471;
  p0_pc2_s_dp[9][1] = 0.00126634;
  p1_pc2_s_dp[9][1] = 0.00154226;

  //! PC3 dz neg
  p0_pc3_m_dz[0][0][0] = -1.72701;
  p1_pc3_m_dz[0][0][0] = -0.0679524;
  p2_pc3_m_dz[0][0][0] = 0.00777703;
  p0_pc3_s_dz[0][0][0] = 0.979525;
  p1_pc3_s_dz[0][0][0] = 2.23967;
  p0_pc3_m_dz[0][1][0] = -1.5894;
  p1_pc3_m_dz[0][1][0] = 0.0290738;
  p2_pc3_m_dz[0][1][0] = 0.018127;
  p0_pc3_s_dz[0][1][0] = 0.965059;
  p1_pc3_s_dz[0][1][0] = 2.26222;
  p0_pc3_m_dz[0][2][0] = -1.34003;
  p1_pc3_m_dz[0][2][0] = -0.148188;
  p2_pc3_m_dz[0][2][0] = 0.0660535;
  p0_pc3_s_dz[0][2][0] = 0.94118;
  p1_pc3_s_dz[0][2][0] = 2.28074;
  p0_pc3_m_dz[0][3][0] = -1.37282;
  p1_pc3_m_dz[0][3][0] = -0.0651129;
  p2_pc3_m_dz[0][3][0] = 0.0314597;
  p0_pc3_s_dz[0][3][0] = 0.944633;
  p1_pc3_s_dz[0][3][0] = 2.26516;
  p0_pc3_m_dz[0][4][0] = -1.1532;
  p1_pc3_m_dz[0][4][0] = -0.133883;
  p2_pc3_m_dz[0][4][0] = 0.0613705;
  p0_pc3_s_dz[0][4][0] = 0.881929;
  p1_pc3_s_dz[0][4][0] = 2.26971;
  p0_pc3_m_dz[0][5][0] = -1.50015;
  p1_pc3_m_dz[0][5][0] = -0.0192779;
  p2_pc3_m_dz[0][5][0] = -0.00890369;
  p0_pc3_s_dz[0][5][0] = 0.886161;
  p1_pc3_s_dz[0][5][0] = 2.26374;
  p0_pc3_m_dz[0][6][0] = -1.52652;
  p1_pc3_m_dz[0][6][0] = 0.150247;
  p2_pc3_m_dz[0][6][0] = -0.0556912;
  p0_pc3_s_dz[0][6][0] = 0.93284;
  p1_pc3_s_dz[0][6][0] = 2.31173;
  p0_pc3_m_dz[0][7][0] = -1.27135;
  p1_pc3_m_dz[0][7][0] = 0.0140999;
  p2_pc3_m_dz[0][7][0] = -0.0415237;
  p0_pc3_s_dz[0][7][0] = 0.913087;
  p1_pc3_s_dz[0][7][0] = 2.29285;
  p0_pc3_m_dz[0][8][0] = -0.936278;
  p1_pc3_m_dz[0][8][0] = -0.192415;
  p2_pc3_m_dz[0][8][0] = 0.00640988;
  p0_pc3_s_dz[0][8][0] = 0.944233;
  p1_pc3_s_dz[0][8][0] = 2.27046;
  p0_pc3_m_dz[0][9][0] = -0.777339;
  p1_pc3_m_dz[0][9][0] = -0.00161368;
  p2_pc3_m_dz[0][9][0] = -0.0249607;
  p0_pc3_s_dz[0][9][0] = 0.889173;
  p1_pc3_s_dz[0][9][0] = 2.3298;
  p0_pc3_m_dz[1][0][0] = -1.19196;
  p1_pc3_m_dz[1][0][0] = 0.32489;
  p2_pc3_m_dz[1][0][0] = -0.0234928;
  p0_pc3_s_dz[1][0][0] = 1.04537;
  p1_pc3_s_dz[1][0][0] = 2.27619;
  p0_pc3_m_dz[1][1][0] = -0.961162;
  p1_pc3_m_dz[1][1][0] = 0.144543;
  p2_pc3_m_dz[1][1][0] = 0.0584691;
  p0_pc3_s_dz[1][1][0] = 1.0251;
  p1_pc3_s_dz[1][1][0] = 2.31714;
  p0_pc3_m_dz[1][2][0] = -1.08098;
  p1_pc3_m_dz[1][2][0] = 0.257685;
  p2_pc3_m_dz[1][2][0] = 0.000747576;
  p0_pc3_s_dz[1][2][0] = 1.01327;
  p1_pc3_s_dz[1][2][0] = 2.33166;
  p0_pc3_m_dz[1][3][0] = -0.912149;
  p1_pc3_m_dz[1][3][0] = -0.16169;
  p2_pc3_m_dz[1][3][0] = 0.0947559;
  p0_pc3_s_dz[1][3][0] = 1.00263;
  p1_pc3_s_dz[1][3][0] = 2.31885;
  p0_pc3_m_dz[1][4][0] = -0.935247;
  p1_pc3_m_dz[1][4][0] = 0.0593679;
  p2_pc3_m_dz[1][4][0] = 0.0186025;
  p0_pc3_s_dz[1][4][0] = 0.963417;
  p1_pc3_s_dz[1][4][0] = 2.23321;
  p0_pc3_m_dz[1][5][0] = -1.29389;
  p1_pc3_m_dz[1][5][0] = 0.0545925;
  p2_pc3_m_dz[1][5][0] = -0.0187062;
  p0_pc3_s_dz[1][5][0] = 0.975077;
  p1_pc3_s_dz[1][5][0] = 2.20865;
  p0_pc3_m_dz[1][6][0] = -0.996832;
  p1_pc3_m_dz[1][6][0] = -0.217283;
  p2_pc3_m_dz[1][6][0] = 0.0270798;
  p0_pc3_s_dz[1][6][0] = 1.01322;
  p1_pc3_s_dz[1][6][0] = 2.34282;
  p0_pc3_m_dz[1][7][0] = -1.24482;
  p1_pc3_m_dz[1][7][0] = -0.0175059;
  p2_pc3_m_dz[1][7][0] = -0.051912;
  p0_pc3_s_dz[1][7][0] = 1.03187;
  p1_pc3_s_dz[1][7][0] = 2.30283;
  p0_pc3_m_dz[1][8][0] = -1.25654;
  p1_pc3_m_dz[1][8][0] = -0.0610753;
  p2_pc3_m_dz[1][8][0] = -0.051326;
  p0_pc3_s_dz[1][8][0] = 1.03438;
  p1_pc3_s_dz[1][8][0] = 2.2889;
  p0_pc3_m_dz[1][9][0] = -0.812838;
  p1_pc3_m_dz[1][9][0] = -0.377978;
  p2_pc3_m_dz[1][9][0] = 0.0389693;
  p0_pc3_s_dz[1][9][0] = 1.04239;
  p1_pc3_s_dz[1][9][0] = 2.26793;

  //! PC3 dz pos
  p0_pc3_m_dz[0][0][1] = -1.86932;
  p1_pc3_m_dz[0][0][1] = 0.426213;
  p2_pc3_m_dz[0][0][1] = -0.0480817;
  p0_pc3_s_dz[0][0][1] = 1.06513;
  p1_pc3_s_dz[0][0][1] = 2.24116;
  p0_pc3_m_dz[0][1][1] = -1.60161;
  p1_pc3_m_dz[0][1][1] = 0.303767;
  p2_pc3_m_dz[0][1][1] = 0.00566057;
  p0_pc3_s_dz[0][1][1] = 1.05719;
  p1_pc3_s_dz[0][1][1] = 2.27063;
  p0_pc3_m_dz[0][2][1] = -1.50368;
  p1_pc3_m_dz[0][2][1] = 0.247249;
  p2_pc3_m_dz[0][2][1] = 0.00546078;
  p0_pc3_s_dz[0][2][1] = 1.0223;
  p1_pc3_s_dz[0][2][1] = 2.29947;
  p0_pc3_m_dz[0][3][1] = -1.50544;
  p1_pc3_m_dz[0][3][1] = 0.223356;
  p2_pc3_m_dz[0][3][1] = -0.0204552;
  p0_pc3_s_dz[0][3][1] = 1.03093;
  p1_pc3_s_dz[0][3][1] = 2.30435;
  p0_pc3_m_dz[0][4][1] = -1.27103;
  p1_pc3_m_dz[0][4][1] = 0.156408;
  p2_pc3_m_dz[0][4][1] = -0.0193825;
  p0_pc3_s_dz[0][4][1] = 0.966021;
  p1_pc3_s_dz[0][4][1] = 2.26916;
  p0_pc3_m_dz[0][5][1] = -1.42305;
  p1_pc3_m_dz[0][5][1] = -0.102961;
  p2_pc3_m_dz[0][5][1] = 0.00463179;
  p0_pc3_s_dz[0][5][1] = 0.980212;
  p1_pc3_s_dz[0][5][1] = 2.27814;
  p0_pc3_m_dz[0][6][1] = -1.41723;
  p1_pc3_m_dz[0][6][1] = -0.0974291;
  p2_pc3_m_dz[0][6][1] = -0.0104371;
  p0_pc3_s_dz[0][6][1] = 1.0393;
  p1_pc3_s_dz[0][6][1] = 2.26879;
  p0_pc3_m_dz[0][7][1] = -1.30234;
  p1_pc3_m_dz[0][7][1] = -0.16701;
  p2_pc3_m_dz[0][7][1] = -0.01669;
  p0_pc3_s_dz[0][7][1] = 1.04594;
  p1_pc3_s_dz[0][7][1] = 2.24131;
  p0_pc3_m_dz[0][8][1] = -1.09444;
  p1_pc3_m_dz[0][8][1] = -0.298882;
  p2_pc3_m_dz[0][8][1] = -0.0150752;
  p0_pc3_s_dz[0][8][1] = 1.0523;
  p1_pc3_s_dz[0][8][1] = 2.26121;
  p0_pc3_m_dz[0][9][1] = -0.766665;
  p1_pc3_m_dz[0][9][1] = -0.316191;
  p2_pc3_m_dz[0][9][1] = -0.00175624;
  p0_pc3_s_dz[0][9][1] = 1.06886;
  p1_pc3_s_dz[0][9][1] = 2.23403;
  p0_pc3_m_dz[1][0][1] = -1.25843;
  p1_pc3_m_dz[1][0][1] = 0.185367;
  p2_pc3_m_dz[1][0][1] = -0.0503981;
  p0_pc3_s_dz[1][0][1] = 1.11821;
  p1_pc3_s_dz[1][0][1] = 2.2214;
  p0_pc3_m_dz[1][1][1] = -0.902728;
  p1_pc3_m_dz[1][1][1] = 0.0205783;
  p2_pc3_m_dz[1][1][1] = 0.0302783;
  p0_pc3_s_dz[1][1][1] = 1.08725;
  p1_pc3_s_dz[1][1][1] = 2.29664;
  p0_pc3_m_dz[1][2][1] = -1.00542;
  p1_pc3_m_dz[1][2][1] = 0.0367962;
  p2_pc3_m_dz[1][2][1] = 0.0285422;
  p0_pc3_s_dz[1][2][1] = 1.07788;
  p1_pc3_s_dz[1][2][1] = 2.30919;
  p0_pc3_m_dz[1][3][1] = -0.998443;
  p1_pc3_m_dz[1][3][1] = -0.112808;
  p2_pc3_m_dz[1][3][1] = 0.0672821;
  p0_pc3_s_dz[1][3][1] = 1.06992;
  p1_pc3_s_dz[1][3][1] = 2.30181;
  p0_pc3_m_dz[1][4][1] = -0.97436;
  p1_pc3_m_dz[1][4][1] = 0.150232;
  p2_pc3_m_dz[1][4][1] = -0.00724303;
  p0_pc3_s_dz[1][4][1] = 1.0168;
  p1_pc3_s_dz[1][4][1] = 2.22334;
  p0_pc3_m_dz[1][5][1] = -1.23119;
  p1_pc3_m_dz[1][5][1] = 0.0553884;
  p2_pc3_m_dz[1][5][1] = -0.00733431;
  p0_pc3_s_dz[1][5][1] = 0.950596;
  p1_pc3_s_dz[1][5][1] = 2.25485;
  p0_pc3_m_dz[1][6][1] = -1.1027;
  p1_pc3_m_dz[1][6][1] = -0.00662955;
  p2_pc3_m_dz[1][6][1] = 0.0134581;
  p0_pc3_s_dz[1][6][1] = 1.06313;
  p1_pc3_s_dz[1][6][1] = 2.30628;
  p0_pc3_m_dz[1][7][1] = -1.08452;
  p1_pc3_m_dz[1][7][1] = -0.0683452;
  p2_pc3_m_dz[1][7][1] = 0.00983396;
  p0_pc3_s_dz[1][7][1] = 1.07363;
  p1_pc3_s_dz[1][7][1] = 2.30379;
  p0_pc3_m_dz[1][8][1] = -1.18469;
  p1_pc3_m_dz[1][8][1] = 0.0507696;
  p2_pc3_m_dz[1][8][1] = -0.0222632;
  p0_pc3_s_dz[1][8][1] = 1.12715;
  p1_pc3_s_dz[1][8][1] = 2.27397;
  p0_pc3_m_dz[1][9][1] = -0.746167;
  p1_pc3_m_dz[1][9][1] = -0.170373;
  p2_pc3_m_dz[1][9][1] = 0.0645889;
  p0_pc3_s_dz[1][9][1] = 1.13844;
  p1_pc3_s_dz[1][9][1] = 2.2216;

  //! PC3 dphi neg
  p0_pc3_m_dp[0][0][0] = 0.000577879;
  p1_pc3_m_dp[0][0][0] = 0.000630181;
  p2_pc3_m_dp[0][0][0] = -0.000226728;
  p0_pc3_s_dp[0][0][0] = 0.00150175;
  p1_pc3_s_dp[0][0][0] = 0.00162829;
  p0_pc3_m_dp[0][1][0] = 0.000803231;
  p1_pc3_m_dp[0][1][0] = 0.000471049;
  p2_pc3_m_dp[0][1][0] = -4.57159e-05;
  p0_pc3_s_dp[0][1][0] = 0.00153818;
  p1_pc3_s_dp[0][1][0] = 0.00156981;
  p0_pc3_m_dp[0][2][0] = 0.00100065;
  p1_pc3_m_dp[0][2][0] = 0.000345239;
  p2_pc3_m_dp[0][2][0] = 7.38329e-05;
  p0_pc3_s_dp[0][2][0] = 0.00152054;
  p1_pc3_s_dp[0][2][0] = 0.00151146;
  p0_pc3_m_dp[0][3][0] = 0.00112371;
  p1_pc3_m_dp[0][3][0] = 0.000378874;
  p2_pc3_m_dp[0][3][0] = 9.46477e-05;
  p0_pc3_s_dp[0][3][0] = 0.00154577;
  p1_pc3_s_dp[0][3][0] = 0.00147556;
  p0_pc3_m_dp[0][4][0] = 0.0010578;
  p1_pc3_m_dp[0][4][0] = 0.000499394;
  p2_pc3_m_dp[0][4][0] = 6.89501e-05;
  p0_pc3_s_dp[0][4][0] = 0.00156297;
  p1_pc3_s_dp[0][4][0] = 0.00140033;
  p0_pc3_m_dp[0][5][0] = 0.00120175;
  p1_pc3_m_dp[0][5][0] = 4.13701e-07;
  p2_pc3_m_dp[0][5][0] = 0.000201625;
  p0_pc3_s_dp[0][5][0] = 0.00154766;
  p1_pc3_s_dp[0][5][0] = 0.00159959;
  p0_pc3_m_dp[0][6][0] = 0.00118147;
  p1_pc3_m_dp[0][6][0] = -4.30691e-05;
  p2_pc3_m_dp[0][6][0] = 0.000174692;
  p0_pc3_s_dp[0][6][0] = 0.00147781;
  p1_pc3_s_dp[0][6][0] = 0.0017299;
  p0_pc3_m_dp[0][7][0] = 0.0011279;
  p1_pc3_m_dp[0][7][0] = -3.67219e-05;
  p2_pc3_m_dp[0][7][0] = 9.97979e-05;
  p0_pc3_s_dp[0][7][0] = 0.00151154;
  p1_pc3_s_dp[0][7][0] = 0.00167246;
  p0_pc3_m_dp[0][8][0] = 0.000898315;
  p1_pc3_m_dp[0][8][0] = 0.000110662;
  p2_pc3_m_dp[0][8][0] = -1.80185e-05;
  p0_pc3_s_dp[0][8][0] = 0.00144634;
  p1_pc3_s_dp[0][8][0] = 0.00178279;
  p0_pc3_m_dp[0][9][0] = 0.000937414;
  p1_pc3_m_dp[0][9][0] = 6.83204e-05;
  p2_pc3_m_dp[0][9][0] = -0.000150607;
  p0_pc3_s_dp[0][9][0] = 0.00144117;
  p1_pc3_s_dp[0][9][0] = 0.00173581;
  p0_pc3_m_dp[1][0][0] = -0.000144851;
  p1_pc3_m_dp[1][0][0] = 6.88778e-06;
  p2_pc3_m_dp[1][0][0] = -0.000141319;
  p0_pc3_s_dp[1][0][0] = 0.00166561;
  p1_pc3_s_dp[1][0][0] = 0.00216899;
  p0_pc3_m_dp[1][1][0] = -0.00025329;
  p1_pc3_m_dp[1][1][0] = 0.000132418;
  p2_pc3_m_dp[1][1][0] = -7.4043e-06;
  p0_pc3_s_dp[1][1][0] = 0.00170327;
  p1_pc3_s_dp[1][1][0] = 0.00213768;
  p0_pc3_m_dp[1][2][0] = -9.85801e-05;
  p1_pc3_m_dp[1][2][0] = 2.85551e-05;
  p2_pc3_m_dp[1][2][0] = 8.62519e-05;
  p0_pc3_s_dp[1][2][0] = 0.00169482;
  p1_pc3_s_dp[1][2][0] = 0.00206599;
  p0_pc3_m_dp[1][3][0] = -0.000251227;
  p1_pc3_m_dp[1][3][0] = 0.00032724;
  p2_pc3_m_dp[1][3][0] = 6.48377e-05;
  p0_pc3_s_dp[1][3][0] = 0.00167948;
  p1_pc3_s_dp[1][3][0] = 0.00188931;
  p0_pc3_m_dp[1][4][0] = -0.000142408;
  p1_pc3_m_dp[1][4][0] = 0.000161809;
  p2_pc3_m_dp[1][4][0] = 0.000137761;
  p0_pc3_s_dp[1][4][0] = 0.00167376;
  p1_pc3_s_dp[1][4][0] = 0.00190298;
  p0_pc3_m_dp[1][5][0] = -3.53955e-05;
  p1_pc3_m_dp[1][5][0] = 0.000255394;
  p2_pc3_m_dp[1][5][0] = 0.000112574;
  p0_pc3_s_dp[1][5][0] = 0.00170793;
  p1_pc3_s_dp[1][5][0] = 0.00207359;
  p0_pc3_m_dp[1][6][0] = 0.000164261;
  p1_pc3_m_dp[1][6][0] = -0.000159242;
  p2_pc3_m_dp[1][6][0] = 0.000213101;
  p0_pc3_s_dp[1][6][0] = 0.00171899;
  p1_pc3_s_dp[1][6][0] = 0.00204741;
  p0_pc3_m_dp[1][7][0] = 0.000104482;
  p1_pc3_m_dp[1][7][0] = -0.000305021;
  p2_pc3_m_dp[1][7][0] = 0.000189996;
  p0_pc3_s_dp[1][7][0] = 0.00164941;
  p1_pc3_s_dp[1][7][0] = 0.00206811;
  p0_pc3_m_dp[1][8][0] = -0.000245787;
  p1_pc3_m_dp[1][8][0] = -0.00017916;
  p2_pc3_m_dp[1][8][0] = 7.3206e-05;
  p0_pc3_s_dp[1][8][0] = 0.00164602;
  p1_pc3_s_dp[1][8][0] = 0.00195273;
  p0_pc3_m_dp[1][9][0] = -0.000446543;
  p1_pc3_m_dp[1][9][0] = -9.79218e-05;
  p2_pc3_m_dp[1][9][0] = -7.38441e-05;
  p0_pc3_s_dp[1][9][0] = 0.0016401;
  p1_pc3_s_dp[1][9][0] = 0.00189747;
  //! PC3 dphi pos
  p0_pc3_m_dp[0][0][1] = 0.00070801;
  p1_pc3_m_dp[0][0][1] = -0.000433526;
  p2_pc3_m_dp[0][0][1] = 0.000164086;
  p0_pc3_s_dp[0][0][1] = 0.0017076;
  p1_pc3_s_dp[0][0][1] = 0.00168993;
  p0_pc3_m_dp[0][1][1] = 0.00062908;
  p1_pc3_m_dp[0][1][1] = -0.000487766;
  p2_pc3_m_dp[0][1][1] = 6.33272e-05;
  p0_pc3_s_dp[0][1][1] = 0.00177268;
  p1_pc3_s_dp[0][1][1] = 0.00168337;
  p0_pc3_m_dp[0][2][1] = 0.000609219;
  p1_pc3_m_dp[0][2][1] = -0.000566509;
  p2_pc3_m_dp[0][2][1] = 1.5663e-05;
  p0_pc3_s_dp[0][2][1] = 0.00176251;
  p1_pc3_s_dp[0][2][1] = 0.00167821;
  p0_pc3_m_dp[0][3][1] = 0.000649714;
  p1_pc3_m_dp[0][3][1] = -0.000642991;
  p2_pc3_m_dp[0][3][1] = -1.77598e-06;
  p0_pc3_s_dp[0][3][1] = 0.00183577;
  p1_pc3_s_dp[0][3][1] = 0.00160519;
  p0_pc3_m_dp[0][4][1] = 0.000600138;
  p1_pc3_m_dp[0][4][1] = -0.000594887;
  p2_pc3_m_dp[0][4][1] = -3.43846e-05;
  p0_pc3_s_dp[0][4][1] = 0.001871;
  p1_pc3_s_dp[0][4][1] = 0.00172072;
  p0_pc3_m_dp[0][5][1] = 0.000486826;
  p1_pc3_m_dp[0][5][1] = -0.000684643;
  p2_pc3_m_dp[0][5][1] = 3.31041e-05;
  p0_pc3_s_dp[0][5][1] = 0.00184426;
  p1_pc3_s_dp[0][5][1] = 0.00172316;
  p0_pc3_m_dp[0][6][1] = 0.000423708;
  p1_pc3_m_dp[0][6][1] = -0.000448644;
  p2_pc3_m_dp[0][6][1] = -1.81224e-05;
  p0_pc3_s_dp[0][6][1] = 0.00179773;
  p1_pc3_s_dp[0][6][1] = 0.00172196;
  p0_pc3_m_dp[0][7][1] = 0.000471302;
  p1_pc3_m_dp[0][7][1] = -0.000225514;
  p2_pc3_m_dp[0][7][1] = -3.53558e-05;
  p0_pc3_s_dp[0][7][1] = 0.0017693;
  p1_pc3_s_dp[0][7][1] = 0.00170259;
  p0_pc3_m_dp[0][8][1] = 0.00071193;
  p1_pc3_m_dp[0][8][1] = -0.00027217;
  p2_pc3_m_dp[0][8][1] = 7.22811e-05;
  p0_pc3_s_dp[0][8][1] = 0.00176444;
  p1_pc3_s_dp[0][8][1] = 0.00171591;
  p0_pc3_m_dp[0][9][1] = 0.000993837;
  p1_pc3_m_dp[0][9][1] = -0.000354861;
  p2_pc3_m_dp[0][9][1] = 0.000212229;
  p0_pc3_s_dp[0][9][1] = 0.0016744;
  p1_pc3_s_dp[0][9][1] = 0.00168322;
  p0_pc3_m_dp[1][0][1] = -0.00074222;
  p1_pc3_m_dp[1][0][1] = -0.000228256;
  p2_pc3_m_dp[1][0][1] = 0.000134624;
  p0_pc3_s_dp[1][0][1] = 0.00182821;
  p1_pc3_s_dp[1][0][1] = 0.00228599;
  p0_pc3_m_dp[1][1][1] = -0.000991442;
  p1_pc3_m_dp[1][1][1] = -5.44879e-05;
  p2_pc3_m_dp[1][1][1] = -5.71606e-05;
  p0_pc3_s_dp[1][1][1] = 0.00184138;
  p1_pc3_s_dp[1][1][1] = 0.00219359;
  p0_pc3_m_dp[1][2][1] = -0.000987482;
  p1_pc3_m_dp[1][2][1] = -0.000352035;
  p2_pc3_m_dp[1][2][1] = -4.00003e-05;
  p0_pc3_s_dp[1][2][1] = 0.0018498;
  p1_pc3_s_dp[1][2][1] = 0.0021477;
  p0_pc3_m_dp[1][3][1] = -0.000918026;
  p1_pc3_m_dp[1][3][1] = -0.000449296;
  p2_pc3_m_dp[1][3][1] = -4.34335e-05;
  p0_pc3_s_dp[1][3][1] = 0.00190328;
  p1_pc3_s_dp[1][3][1] = 0.00188691;
  p0_pc3_m_dp[1][4][1] = -0.00117122;
  p1_pc3_m_dp[1][4][1] = -0.000282062;
  p2_pc3_m_dp[1][4][1] = -0.000116011;
  p0_pc3_s_dp[1][4][1] = 0.00191302;
  p1_pc3_s_dp[1][4][1] = 0.00198651;
  p0_pc3_m_dp[1][5][1] = -0.00069027;
  p1_pc3_m_dp[1][5][1] = -0.000529055;
  p2_pc3_m_dp[1][5][1] = -2.05989e-05;
  p0_pc3_s_dp[1][5][1] = 0.00191185;
  p1_pc3_s_dp[1][5][1] = 0.00193027;
  p0_pc3_m_dp[1][6][1] = -0.000682929;
  p1_pc3_m_dp[1][6][1] = -0.000277425;
  p2_pc3_m_dp[1][6][1] = -9.24763e-05;
  p0_pc3_s_dp[1][6][1] = 0.00187428;
  p1_pc3_s_dp[1][6][1] = 0.00187515;
  p0_pc3_m_dp[1][7][1] = -0.000772704;
  p1_pc3_m_dp[1][7][1] = -0.000122438;
  p2_pc3_m_dp[1][7][1] = -7.35094e-05;
  p0_pc3_s_dp[1][7][1] = 0.00185033;
  p1_pc3_s_dp[1][7][1] = 0.00185812;
  p0_pc3_m_dp[1][8][1] = -0.000846304;
  p1_pc3_m_dp[1][8][1] = -5.84164e-05;
  p2_pc3_m_dp[1][8][1] = -3.86517e-06;
  p0_pc3_s_dp[1][8][1] = 0.00180633;
  p1_pc3_s_dp[1][8][1] = 0.00182733;
  p0_pc3_m_dp[1][9][1] = -0.000746998;
  p1_pc3_m_dp[1][9][1] = -0.000335806;
  p2_pc3_m_dp[1][9][1] = 0.000219708;
  p0_pc3_s_dp[1][9][1] = 0.00176745;
  p1_pc3_s_dp[1][9][1] = 0.00186676;

  //! EMC dphi neg
  p0_emc_m_dp[0][0][0] = 0.000433522;
  p1_emc_m_dp[0][0][0] = 0.00447111;
  p2_emc_m_dp[0][0][0] = -0.000450912;
  p0_emc_s_dp[0][0][0] = 0.00182684;
  p1_emc_s_dp[0][0][0] = 0.00389582;
  p0_emc_m_dp[0][1][0] = 0.000956048;
  p1_emc_m_dp[0][1][0] = 0.00393021;
  p2_emc_m_dp[0][1][0] = -0.000163784;

  p0_emc_s_dp[0][1][0] = 0.0019306;
  p1_emc_s_dp[0][1][0] = 0.00387519;
  p0_emc_m_dp[0][2][0] = 0.000847876;
  p1_emc_m_dp[0][2][0] = 0.00420459;
  p2_emc_m_dp[0][2][0] = -0.000149085;
  p0_emc_s_dp[0][2][0] = 0.00192029;
  p1_emc_s_dp[0][2][0] = 0.00399715;
  p0_emc_m_dp[0][3][0] = 0.000459846;
  p1_emc_m_dp[0][3][0] = 0.00475629;
  p2_emc_m_dp[0][3][0] = -0.000283731;
  p0_emc_s_dp[0][3][0] = 0.00189435;
  p1_emc_s_dp[0][3][0] = 0.00410325;
  p0_emc_m_dp[0][4][0] = 0.00101513;
  p1_emc_m_dp[0][4][0] = 0.00428765;
  p2_emc_m_dp[0][4][0] = -0.000173608;
  p0_emc_s_dp[0][4][0] = 0.0019195;
  p1_emc_s_dp[0][4][0] = 0.00414167;
  p0_emc_m_dp[0][5][0] = 0.000925958;
  p1_emc_m_dp[0][5][0] = 0.00392767;
  p2_emc_m_dp[0][5][0] = -4.60935e-05;
  p0_emc_s_dp[0][5][0] = 0.00206519;
  p1_emc_s_dp[0][5][0] = 0.00398189;
  p0_emc_m_dp[0][6][0] = 0.000822041;
  p1_emc_m_dp[0][6][0] = 0.00378243;
  p2_emc_m_dp[0][6][0] = -1.68349e-05;
  p0_emc_s_dp[0][6][0] = 0.00188544;
  p1_emc_s_dp[0][6][0] = 0.00402043;
  p0_emc_m_dp[0][7][0] = 4.39375e-05;
  p1_emc_m_dp[0][7][0] = 0.00443455;
  p2_emc_m_dp[0][7][0] = -0.000222724;
  p0_emc_s_dp[0][7][0] = 0.00193168;
  p1_emc_s_dp[0][7][0] = 0.00392409;
  p0_emc_m_dp[0][8][0] = 0.00035562;
  p1_emc_m_dp[0][8][0] = 0.00386183;
  p2_emc_m_dp[0][8][0] = -0.000131168;
  p0_emc_s_dp[0][8][0] = 0.00178895;
  p1_emc_s_dp[0][8][0] = 0.00403559;
  p0_emc_m_dp[0][9][0] = -4.10135e-05;
  p1_emc_m_dp[0][9][0] = 0.00443124;
  p2_emc_m_dp[0][9][0] = -0.000444111;
  p0_emc_s_dp[0][9][0] = 0.00177148;
  p1_emc_s_dp[0][9][0] = 0.0038817;
  p0_emc_m_dp[1][0][0] = 0.00166237;
  p1_emc_m_dp[1][0][0] = 0.00283671;
  p2_emc_m_dp[1][0][0] = -0.00055966;
  p0_emc_s_dp[1][0][0] = 0.00181807;
  p1_emc_s_dp[1][0][0] = 0.00466329;
  p0_emc_m_dp[1][1][0] = 0.00149412;
  p1_emc_m_dp[1][1][0] = 0.00300559;
  p2_emc_m_dp[1][1][0] = -0.000432315;
  p0_emc_s_dp[1][1][0] = 0.00181004;
  p1_emc_s_dp[1][1][0] = 0.00473647;
  p0_emc_m_dp[1][2][0] = 0.00147933;
  p1_emc_m_dp[1][2][0] = 0.00331918;
  p2_emc_m_dp[1][2][0] = -0.000424725;
  p0_emc_s_dp[1][2][0] = 0.00176253;
  p1_emc_s_dp[1][2][0] = 0.00486926;
  p0_emc_m_dp[1][3][0] = 0.00174032;
  p1_emc_m_dp[1][3][0] = 0.00317236;
  p2_emc_m_dp[1][3][0] = -0.000321518;
  p0_emc_s_dp[1][3][0] = 0.0018503;
  p1_emc_s_dp[1][3][0] = 0.00480884;
  p0_emc_m_dp[1][4][0] = 0.00158693;
  p1_emc_m_dp[1][4][0] = 0.00359648;
  p2_emc_m_dp[1][4][0] = -0.000431116;
  p0_emc_s_dp[1][4][0] = 0.00185167;
  p1_emc_s_dp[1][4][0] = 0.00489419;
  p0_emc_m_dp[1][5][0] = 0.00138772;
  p1_emc_m_dp[1][5][0] = 0.00283309;
  p2_emc_m_dp[1][5][0] = -0.000220309;
  p0_emc_s_dp[1][5][0] = 0.00177509;
  p1_emc_s_dp[1][5][0] = 0.00475974;
  p0_emc_m_dp[1][6][0] = 0.000809448;
  p1_emc_m_dp[1][6][0] = 0.00345488;
  p2_emc_m_dp[1][6][0] = -0.000369827;
  p0_emc_s_dp[1][6][0] = 0.00183691;
  p1_emc_s_dp[1][6][0] = 0.00463888;
  p0_emc_m_dp[1][7][0] = 0.00133287;
  p1_emc_m_dp[1][7][0] = 0.00292954;
  p2_emc_m_dp[1][7][0] = -0.000317293;
  p0_emc_s_dp[1][7][0] = 0.00181832;
  p1_emc_s_dp[1][7][0] = 0.00456237;
  p0_emc_m_dp[1][8][0] = 0.00116854;
  p1_emc_m_dp[1][8][0] = 0.00301468;
  p2_emc_m_dp[1][8][0] = -0.000431211;
  p0_emc_s_dp[1][8][0] = 0.00178886;
  p1_emc_s_dp[1][8][0] = 0.00459242;
  p0_emc_m_dp[1][9][0] = 0.00162016;
  p1_emc_m_dp[1][9][0] = 0.00270864;
  p2_emc_m_dp[1][9][0] = -0.000523369;
  p0_emc_s_dp[1][9][0] = 0.00171949;
  p1_emc_s_dp[1][9][0] = 0.00452448;
  p0_emc_m_dp[2][0][0] = -0.00229344;
  p1_emc_m_dp[2][0][0] = 0.00300471;
  p2_emc_m_dp[2][0][0] = -0.000600412;
  p0_emc_s_dp[2][0][0] = 0.00174777;
  p1_emc_s_dp[2][0][0] = 0.00576329;
  p0_emc_m_dp[2][1][0] = -0.00161387;
  p1_emc_m_dp[2][1][0] = 0.00251524;
  p2_emc_m_dp[2][1][0] = -0.000342056;
  p0_emc_s_dp[2][1][0] = 0.00182103;
  p1_emc_s_dp[2][1][0] = 0.00563931;
  p0_emc_m_dp[2][2][0] = -0.00160225;
  p1_emc_m_dp[2][2][0] = 0.00287154;
  p2_emc_m_dp[2][2][0] = -0.000367583;
  p0_emc_s_dp[2][2][0] = 0.00183644;
  p1_emc_s_dp[2][2][0] = 0.00561861;
  p0_emc_m_dp[2][3][0] = -0.0015701;
  p1_emc_m_dp[2][3][0] = 0.00334976;
  p2_emc_m_dp[2][3][0] = -0.000473725;
  p0_emc_s_dp[2][3][0] = 0.00186959;
  p1_emc_s_dp[2][3][0] = 0.00554047;
  p0_emc_m_dp[2][4][0] = -0.00138792;
  p1_emc_m_dp[2][4][0] = 0.00328851;
  p2_emc_m_dp[2][4][0] = -0.000426216;
  p0_emc_s_dp[2][4][0] = 0.00188407;
  p1_emc_s_dp[2][4][0] = 0.00556843;
  p0_emc_m_dp[2][5][0] = -0.00159299;
  p1_emc_m_dp[2][5][0] = 0.00298673;
  p2_emc_m_dp[2][5][0] = -0.000290793;
  p0_emc_s_dp[2][5][0] = 0.00204891;
  p1_emc_s_dp[2][5][0] = 0.00554058;
  p0_emc_m_dp[2][6][0] = -0.00136184;
  p1_emc_m_dp[2][6][0] = 0.00258237;
  p2_emc_m_dp[2][6][0] = -0.000231062;
  p0_emc_s_dp[2][6][0] = 0.00206449;
  p1_emc_s_dp[2][6][0] = 0.00534212;
  p0_emc_m_dp[2][7][0] = -0.00120264;
  p1_emc_m_dp[2][7][0] = 0.00226367;
  p2_emc_m_dp[2][7][0] = -0.000218139;
  p0_emc_s_dp[2][7][0] = 0.00200397;
  p1_emc_s_dp[2][7][0] = 0.0052767;
  p0_emc_m_dp[2][8][0] = -0.00161512;
  p1_emc_m_dp[2][8][0] = 0.00247667;
  p2_emc_m_dp[2][8][0] = -0.000355789;
  p0_emc_s_dp[2][8][0] = 0.00193294;
  p1_emc_s_dp[2][8][0] = 0.00522673;
  p0_emc_m_dp[2][9][0] = -0.00238655;
  p1_emc_m_dp[2][9][0] = 0.00308933;
  p2_emc_m_dp[2][9][0] = -0.000639272;
  p0_emc_s_dp[2][9][0] = 0.00193377;
  p1_emc_s_dp[2][9][0] = 0.00512937;

  //! EMC dphi pos
  p0_emc_m_dp[0][0][1] = 0.000383531;
  p1_emc_m_dp[0][0][1] = -0.00364294;
  p2_emc_m_dp[0][0][1] = 0.000265206;
  p0_emc_s_dp[0][0][1] = 0.00221706;
  p1_emc_s_dp[0][0][1] = 0.00329528;
  p0_emc_m_dp[0][1][1] = 0.000837446;
  p1_emc_m_dp[0][1][1] = -0.00430282;
  p2_emc_m_dp[0][1][1] = 0.000307223;
  p0_emc_s_dp[0][1][1] = 0.00224427;
  p1_emc_s_dp[0][1][1] = 0.0032554;
  p0_emc_m_dp[0][2][1] = 0.000958543;
  p1_emc_m_dp[0][2][1] = -0.0047455;
  p2_emc_m_dp[0][2][1] = 0.000357246;
  p0_emc_s_dp[0][2][1] = 0.0023467;
  p1_emc_s_dp[0][2][1] = 0.00319021;
  p0_emc_m_dp[0][3][1] = 0.000666215;
  p1_emc_m_dp[0][3][1] = -0.00458747;
  p2_emc_m_dp[0][3][1] = 0.000288087;
  p0_emc_s_dp[0][3][1] = 0.00235828;
  p1_emc_s_dp[0][3][1] = 0.0032038;
  p0_emc_m_dp[0][4][1] = 0.000400077;
  p1_emc_m_dp[0][4][1] = -0.00426147;
  p2_emc_m_dp[0][4][1] = 0.000196007;
  p0_emc_s_dp[0][4][1] = 0.00235001;
  p1_emc_s_dp[0][4][1] = 0.00335706;
  p0_emc_m_dp[0][5][1] = 0.000221242;
  p1_emc_m_dp[0][5][1] = -0.00426385;
  p2_emc_m_dp[0][5][1] = 0.000263529;
  p0_emc_s_dp[0][5][1] = 0.00232944;
  p1_emc_s_dp[0][5][1] = 0.00331234;
  p0_emc_m_dp[0][6][1] = 7.87847e-05;
  p1_emc_m_dp[0][6][1] = -0.00404151;
  p2_emc_m_dp[0][6][1] = 0.000219638;
  p0_emc_s_dp[0][6][1] = 0.00232489;
  p1_emc_s_dp[0][6][1] = 0.00323626;
  p0_emc_m_dp[0][7][1] = 0.000590587;
  p1_emc_m_dp[0][7][1] = -0.00440265;
  p2_emc_m_dp[0][7][1] = 0.000365031;
  p0_emc_s_dp[0][7][1] = 0.00220537;
  p1_emc_s_dp[0][7][1] = 0.00335693;
  p0_emc_m_dp[0][8][1] = 0.00055569;
  p1_emc_m_dp[0][8][1] = -0.00416373;
  p2_emc_m_dp[0][8][1] = 0.000375657;
  p0_emc_s_dp[0][8][1] = 0.00225775;
  p1_emc_s_dp[0][8][1] = 0.00334495;
  p0_emc_m_dp[0][9][1] = 0.000681525;
  p1_emc_m_dp[0][9][1] = -0.00415039;
  p2_emc_m_dp[0][9][1] = 0.000493145;
  p0_emc_s_dp[0][9][1] = 0.00217568;
  p1_emc_s_dp[0][9][1] = 0.00327433;
  p0_emc_m_dp[1][0][1] = 0.00153749;
  p1_emc_m_dp[1][0][1] = -0.00123532;
  p2_emc_m_dp[1][0][1] = -3.60275e-05;
  p0_emc_s_dp[1][0][1] = 0.00211465;
  p1_emc_s_dp[1][0][1] = 0.00418092;
  p0_emc_m_dp[1][1][1] = 0.00131565;
  p1_emc_m_dp[1][1][1] = -0.00118959;
  p2_emc_m_dp[1][1][1] = -0.000145017;
  p0_emc_s_dp[1][1][1] = 0.00211731;
  p1_emc_s_dp[1][1][1] = 0.0042695;
  p0_emc_m_dp[1][2][1] = 0.00140766;
  p1_emc_m_dp[1][2][1] = -0.00143359;
  p2_emc_m_dp[1][2][1] = -0.000148674;
  p0_emc_s_dp[1][2][1] = 0.00219659;
  p1_emc_s_dp[1][2][1] = 0.00422341;
  p0_emc_m_dp[1][3][1] = 0.00152113;
  p1_emc_m_dp[1][3][1] = -0.00161557;
  p2_emc_m_dp[1][3][1] = -0.000118539;
  p0_emc_s_dp[1][3][1] = 0.00223853;
  p1_emc_s_dp[1][3][1] = 0.00427339;
  p0_emc_m_dp[1][4][1] = 0.00138602;
  p1_emc_m_dp[1][4][1] = -0.00133751;
  p2_emc_m_dp[1][4][1] = -0.000205544;
  p0_emc_s_dp[1][4][1] = 0.00229492;
  p1_emc_s_dp[1][4][1] = 0.00437961;
  p0_emc_m_dp[1][5][1] = 4.51388e-05;
  p1_emc_m_dp[1][5][1] = -0.000679257;
  p2_emc_m_dp[1][5][1] = -0.000258149;
  p0_emc_s_dp[1][5][1] = 0.00227325;
  p1_emc_s_dp[1][5][1] = 0.0042776;
  p0_emc_m_dp[1][6][1] = 0.000376538;
  p1_emc_m_dp[1][6][1] = -0.00079472;
  p2_emc_m_dp[1][6][1] = -0.000219399;
  p0_emc_s_dp[1][6][1] = 0.00226561;
  p1_emc_s_dp[1][6][1] = 0.00417341;
  p0_emc_m_dp[1][7][1] = 0.0010049;
  p1_emc_m_dp[1][7][1] = -0.00106058;
  p2_emc_m_dp[1][7][1] = -7.41204e-05;
  p0_emc_s_dp[1][7][1] = 0.0022624;
  p1_emc_s_dp[1][7][1] = 0.0041948;
  p0_emc_m_dp[1][8][1] = 0.00120504;
  p1_emc_m_dp[1][8][1] = -0.000776582;
  p2_emc_m_dp[1][8][1] = -6.75443e-05;
  p0_emc_s_dp[1][8][1] = 0.00221094;
  p1_emc_s_dp[1][8][1] = 0.00421169;
  p0_emc_m_dp[1][9][1] = 0.00163839;
  p1_emc_m_dp[1][9][1] = -0.000934161;
  p2_emc_m_dp[1][9][1] = 8.36898e-05;
  p0_emc_s_dp[1][9][1] = 0.00212562;
  p1_emc_s_dp[1][9][1] = 0.00428355;
  p0_emc_m_dp[2][0][1] = -0.0027551;
  p1_emc_m_dp[2][0][1] = -0.00117151;
  p2_emc_m_dp[2][0][1] = -2.89105e-05;
  p0_emc_s_dp[2][0][1] = 0.00205621;
  p1_emc_s_dp[2][0][1] = 0.00532719;
  p0_emc_m_dp[2][1][1] = -0.00286869;
  p1_emc_m_dp[2][1][1] = -0.000959404;
  p2_emc_m_dp[2][1][1] = -0.000234468;
  p0_emc_s_dp[2][1][1] = 0.00217352;
  p1_emc_s_dp[2][1][1] = 0.00507367;
  p0_emc_m_dp[2][2][1] = -0.0027912;
  p1_emc_m_dp[2][2][1] = -0.00127123;
  p2_emc_m_dp[2][2][1] = -0.00020692;
  p0_emc_s_dp[2][2][1] = 0.00221799;
  p1_emc_s_dp[2][2][1] = 0.0050227;
  p0_emc_m_dp[2][3][1] = -0.00252565;
  p1_emc_m_dp[2][3][1] = -0.00144887;
  p2_emc_m_dp[2][3][1] = -0.000183557;
  p0_emc_s_dp[2][3][1] = 0.00224207;
  p1_emc_s_dp[2][3][1] = 0.00479209;
  p0_emc_m_dp[2][4][1] = -0.00248448;
  p1_emc_m_dp[2][4][1] = -0.00162475;
  p2_emc_m_dp[2][4][1] = -0.000176411;
  p0_emc_s_dp[2][4][1] = 0.0022755;
  p1_emc_s_dp[2][4][1] = 0.00491614;
  p0_emc_m_dp[2][5][1] = -0.00256592;
  p1_emc_m_dp[2][5][1] = -0.0017112;
  p2_emc_m_dp[2][5][1] = -0.00010732;
  p0_emc_s_dp[2][5][1] = 0.00228881;
  p1_emc_s_dp[2][5][1] = 0.0047008;
  p0_emc_m_dp[2][6][1] = -0.00261992;
  p1_emc_m_dp[2][6][1] = -0.00143002;
  p2_emc_m_dp[2][6][1] = -0.000190454;
  p0_emc_s_dp[2][6][1] = 0.00233756;
  p1_emc_s_dp[2][6][1] = 0.00451586;
  p0_emc_m_dp[2][7][1] = -0.00272536;
  p1_emc_m_dp[2][7][1] = -0.00116227;
  p2_emc_m_dp[2][7][1] = -0.000219724;
  p0_emc_s_dp[2][7][1] = 0.00225884;
  p1_emc_s_dp[2][7][1] = 0.00443873;
  p0_emc_m_dp[2][8][1] = -0.00237219;
  p1_emc_m_dp[2][8][1] = -0.00150838;
  p2_emc_m_dp[2][8][1] = -5.70278e-05;
  p0_emc_s_dp[2][8][1] = 0.0022187;
  p1_emc_s_dp[2][8][1] = 0.0044134;
  p0_emc_m_dp[2][9][1] = -0.0026303;
  p1_emc_m_dp[2][9][1] = -0.00118143;
  p2_emc_m_dp[2][9][1] = -1.97758e-05;
  p0_emc_s_dp[2][9][1] = 0.0021652;
  p1_emc_s_dp[2][9][1] = 0.00436914;

  //! EMC dz neg
  p0_emc_m_dz[0][0][0] = -3.01763;
  p1_emc_m_dz[0][0][0] = -0.469824;
  p2_emc_m_dz[0][0][0] = 0.319962;
  p0_emc_s_dz[0][0][0] = 1.19061;
  p1_emc_s_dz[0][0][0] = 3.65277;
  p0_emc_m_dz[0][1][0] = -2.79838;
  p1_emc_m_dz[0][1][0] = -0.324732;
  p2_emc_m_dz[0][1][0] = 0.282516;
  p0_emc_s_dz[0][1][0] = 1.18485;
  p1_emc_s_dz[0][1][0] = 3.57967;
  p0_emc_m_dz[0][2][0] = -2.71861;
  p1_emc_m_dz[0][2][0] = -0.201191;
  p2_emc_m_dz[0][2][0] = 0.215621;
  p0_emc_s_dz[0][2][0] = 1.22035;
  p1_emc_s_dz[0][2][0] = 3.46544;
  p0_emc_m_dz[0][3][0] = -2.54771;
  p1_emc_m_dz[0][3][0] = -0.149168;
  p2_emc_m_dz[0][3][0] = 0.150847;
  p0_emc_s_dz[0][3][0] = 1.30535;
  p1_emc_s_dz[0][3][0] = 3.30656;
  p0_emc_m_dz[0][4][0] = -2.20839;
  p1_emc_m_dz[0][4][0] = -0.305646;
  p2_emc_m_dz[0][4][0] = 0.170372;
  p0_emc_s_dz[0][4][0] = 1.22551;
  p1_emc_s_dz[0][4][0] = 3.35411;
  p0_emc_m_dz[0][5][0] = -2.33589;
  p1_emc_m_dz[0][5][0] = 0.0597328;
  p2_emc_m_dz[0][5][0] = -0.0729887;
  p0_emc_s_dz[0][5][0] = 1.24663;
  p1_emc_s_dz[0][5][0] = 3.36053;
  p0_emc_m_dz[0][6][0] = -2.05405;
  p1_emc_m_dz[0][6][0] = 0.15538;
  p2_emc_m_dz[0][6][0] = -0.162637;
  p0_emc_s_dz[0][6][0] = 1.2948;
  p1_emc_s_dz[0][6][0] = 3.31156;
  p0_emc_m_dz[0][7][0] = -1.7139;
  p1_emc_m_dz[0][7][0] = 0.245756;
  p2_emc_m_dz[0][7][0] = -0.25834;
  p0_emc_s_dz[0][7][0] = 1.24598;
  p1_emc_s_dz[0][7][0] = 3.32453;
  p0_emc_m_dz[0][8][0] = -1.22382;
  p1_emc_m_dz[0][8][0] = 0.104101;
  p2_emc_m_dz[0][8][0] = -0.276624;
  p0_emc_s_dz[0][8][0] = 1.22883;
  p1_emc_s_dz[0][8][0] = 3.39708;
  p0_emc_m_dz[0][9][0] = -0.519783;
  p1_emc_m_dz[0][9][0] = -0.0954101;
  p2_emc_m_dz[0][9][0] = -0.257762;
  p0_emc_s_dz[0][9][0] = 1.2488;
  p1_emc_s_dz[0][9][0] = 3.32898;
  p0_emc_m_dz[1][0][0] = -4.66168;
  p1_emc_m_dz[1][0][0] = 0.0669362;
  p2_emc_m_dz[1][0][0] = 0.153556;
  p0_emc_s_dz[1][0][0] = 1.00246;
  p1_emc_s_dz[1][0][0] = 3.63976;
  p0_emc_m_dz[1][1][0] = -4.16646;
  p1_emc_m_dz[1][1][0] = 0.159441;
  p2_emc_m_dz[1][1][0] = 0.130367;
  p0_emc_s_dz[1][1][0] = 1.05831;
  p1_emc_s_dz[1][1][0] = 3.48953;
  p0_emc_m_dz[1][2][0] = -3.26259;
  p1_emc_m_dz[1][2][0] = -0.237267;
  p2_emc_m_dz[1][2][0] = 0.196097;
  p0_emc_s_dz[1][2][0] = 1.10696;
  p1_emc_s_dz[1][2][0] = 3.25132;
  p0_emc_m_dz[1][3][0] = -2.69744;
  p1_emc_m_dz[1][3][0] = -0.164287;
  p2_emc_m_dz[1][3][0] = 0.123219;
  p0_emc_s_dz[1][3][0] = 1.13237;
  p1_emc_s_dz[1][3][0] = 3.1614;
  p0_emc_m_dz[1][4][0] = -1.93417;
  p1_emc_m_dz[1][4][0] = -0.442851;
  p2_emc_m_dz[1][4][0] = 0.17035;
  p0_emc_s_dz[1][4][0] = 1.05217;
  p1_emc_s_dz[1][4][0] = 3.24914;
  p0_emc_m_dz[1][5][0] = -1.2786;
  p1_emc_m_dz[1][5][0] = -0.451679;
  p2_emc_m_dz[1][5][0] = 0.0793417;
  p0_emc_s_dz[1][5][0] = 1.09259;
  p1_emc_s_dz[1][5][0] = 3.14249;
  p0_emc_m_dz[1][6][0] = -0.902682;
  p1_emc_m_dz[1][6][0] = -0.223449;
  p2_emc_m_dz[1][6][0] = -0.0199616;
  p0_emc_s_dz[1][6][0] = 1.10421;
  p1_emc_s_dz[1][6][0] = 3.24202;
  p0_emc_m_dz[1][7][0] = -0.366716;
  p1_emc_m_dz[1][7][0] = -0.099521;
  p2_emc_m_dz[1][7][0] = -0.116403;
  p0_emc_s_dz[1][7][0] = 1.02539;
  p1_emc_s_dz[1][7][0] = 3.37469;
  p0_emc_m_dz[1][8][0] = 0.510293;
  p1_emc_m_dz[1][8][0] = -0.384785;
  p2_emc_m_dz[1][8][0] = -0.085672;
  p0_emc_s_dz[1][8][0] = 1.05828;
  p1_emc_s_dz[1][8][0] = 3.50444;
  p0_emc_m_dz[1][9][0] = 0.783046;
  p1_emc_m_dz[1][9][0] = -0.0852186;
  p2_emc_m_dz[1][9][0] = -0.172907;
  p0_emc_s_dz[1][9][0] = 0.930126;
  p1_emc_s_dz[1][9][0] = 3.75824;
  p0_emc_m_dz[2][0][0] = -3.5885;
  p1_emc_m_dz[2][0][0] = 0.603353;
  p2_emc_m_dz[2][0][0] = 0.129413;
  p0_emc_s_dz[2][0][0] = 1.15193;
  p1_emc_s_dz[2][0][0] = 3.76613;
  p0_emc_m_dz[2][1][0] = -2.87453;
  p1_emc_m_dz[2][1][0] = 0.328788;
  p2_emc_m_dz[2][1][0] = 0.187333;
  p0_emc_s_dz[2][1][0] = 1.21652;
  p1_emc_s_dz[2][1][0] = 3.60113;
  p0_emc_m_dz[2][2][0] = -2.36044;
  p1_emc_m_dz[2][2][0] = 0.295899;
  p2_emc_m_dz[2][2][0] = 0.132831;
  p0_emc_s_dz[2][2][0] = 1.21376;
  p1_emc_s_dz[2][2][0] = 3.40516;
  p0_emc_m_dz[2][3][0] = -1.44006;
  p1_emc_m_dz[2][3][0] = -0.141668;
  p2_emc_m_dz[2][3][0] = 0.179059;
  p0_emc_s_dz[2][3][0] = 1.2134;
  p1_emc_s_dz[2][3][0] = 3.29833;
  p0_emc_m_dz[2][4][0] = -1.1994;
  p1_emc_m_dz[2][4][0] = 0.207345;
  p2_emc_m_dz[2][4][0] = 0.0294105;
  p0_emc_s_dz[2][4][0] = 1.22539;
  p1_emc_s_dz[2][4][0] = 3.20535;
  p0_emc_m_dz[2][5][0] = -0.332346;
  p1_emc_m_dz[2][5][0] = -0.105496;
  p2_emc_m_dz[2][5][0] = -0.00426049;
  p0_emc_s_dz[2][5][0] = 1.25752;
  p1_emc_s_dz[2][5][0] = 3.11724;
  p0_emc_m_dz[2][6][0] = 0.150373;
  p1_emc_m_dz[2][6][0] = -0.168358;
  p2_emc_m_dz[2][6][0] = -0.0269751;
  p0_emc_s_dz[2][6][0] = 1.27842;
  p1_emc_s_dz[2][6][0] = 3.13669;
  p0_emc_m_dz[2][7][0] = 0.747776;
  p1_emc_m_dz[2][7][0] = -0.222767;
  p2_emc_m_dz[2][7][0] = -0.0832351;
  p0_emc_s_dz[2][7][0] = 1.29051;
  p1_emc_s_dz[2][7][0] = 3.13717;
  p0_emc_m_dz[2][8][0] = 1.23012;
  p1_emc_m_dz[2][8][0] = -0.295026;
  p2_emc_m_dz[2][8][0] = -0.105863;
  p0_emc_s_dz[2][8][0] = 1.19575;
  p1_emc_s_dz[2][8][0] = 3.36403;
  p0_emc_m_dz[2][9][0] = 1.8227;
  p1_emc_m_dz[2][9][0] = -0.424275;
  p2_emc_m_dz[2][9][0] = -0.109639;
  p0_emc_s_dz[2][9][0] = 1.22045;
  p1_emc_s_dz[2][9][0] = 3.44533;

  //! EMC dz pos
  p0_emc_m_dz[0][0][1] = -3.22747;
  p1_emc_m_dz[0][0][1] = 0.31655;
  p2_emc_m_dz[0][0][1] = 0.224489;
  p0_emc_s_dz[0][0][1] = 1.41207;
  p1_emc_s_dz[0][0][1] = 3.29709;
  p0_emc_m_dz[0][1][1] = -2.9124;
  p1_emc_m_dz[0][1][1] = 0.308117;
  p2_emc_m_dz[0][1][1] = 0.214494;
  p0_emc_s_dz[0][1][1] = 1.42378;
  p1_emc_s_dz[0][1][1] = 3.25237;
  p0_emc_m_dz[0][2][1] = -2.67582;
  p1_emc_m_dz[0][2][1] = 0.153953;
  p2_emc_m_dz[0][2][1] = 0.183704;
  p0_emc_s_dz[0][2][1] = 1.46192;
  p1_emc_s_dz[0][2][1] = 3.13026;
  p0_emc_m_dz[0][3][1] = -2.51767;
  p1_emc_m_dz[0][3][1] = 0.018466;
  p2_emc_m_dz[0][3][1] = 0.137152;
  p0_emc_s_dz[0][3][1] = 1.48837;
  p1_emc_s_dz[0][3][1] = 3.10682;
  p0_emc_m_dz[0][4][1] = -2.33603;
  p1_emc_m_dz[0][4][1] = -0.0359063;
  p2_emc_m_dz[0][4][1] = 0.108368;
  p0_emc_s_dz[0][4][1] = 1.41725;
  p1_emc_s_dz[0][4][1] = 3.08934;
  p0_emc_m_dz[0][5][1] = -2.59635;
  p1_emc_m_dz[0][5][1] = 0.154682;
  p2_emc_m_dz[0][5][1] = -0.100573;
  p0_emc_s_dz[0][5][1] = 1.50795;
  p1_emc_s_dz[0][5][1] = 3.02713;
  p0_emc_m_dz[0][6][1] = -2.5467;
  p1_emc_m_dz[0][6][1] = 0.279598;
  p2_emc_m_dz[0][6][1] = -0.206929;
  p0_emc_s_dz[0][6][1] = 1.58296;
  p1_emc_s_dz[0][6][1] = 2.95387;
  p0_emc_m_dz[0][7][1] = -2.33077;
  p1_emc_m_dz[0][7][1] = 0.350457;
  p2_emc_m_dz[0][7][1] = -0.300032;
  p0_emc_s_dz[0][7][1] = 1.55666;
  p1_emc_s_dz[0][7][1] = 2.99428;
  p0_emc_m_dz[0][8][1] = -1.77483;
  p1_emc_m_dz[0][8][1] = 0.0163714;
  p2_emc_m_dz[0][8][1] = -0.317617;
  p0_emc_s_dz[0][8][1] = 1.46025;
  p1_emc_s_dz[0][8][1] = 3.12175;
  p0_emc_m_dz[0][9][1] = -1.47883;
  p1_emc_m_dz[0][9][1] = 0.200961;
  p2_emc_m_dz[0][9][1] = -0.420972;
  p0_emc_s_dz[0][9][1] = 1.46312;
  p1_emc_s_dz[0][9][1] = 3.17529;
  p0_emc_m_dz[1][0][1] = -5.81258;
  p1_emc_m_dz[1][0][1] = 2.40185;
  p2_emc_m_dz[1][0][1] = -0.434252;
  p0_emc_s_dz[1][0][1] = 1.28184;
  p1_emc_s_dz[1][0][1] = 3.33907;
  p0_emc_m_dz[1][1][1] = -5.05458;
  p1_emc_m_dz[1][1][1] = 2.00028;
  p2_emc_m_dz[1][1][1] = -0.339928;
  p0_emc_s_dz[1][1][1] = 1.34471;
  p1_emc_s_dz[1][1][1] = 3.17498;
  p0_emc_m_dz[1][2][1] = -4.13561;
  p1_emc_m_dz[1][2][1] = 1.36292;
  p2_emc_m_dz[1][2][1] = -0.202596;
  p0_emc_s_dz[1][2][1] = 1.33881;
  p1_emc_s_dz[1][2][1] = 3.01756;
  p0_emc_m_dz[1][3][1] = -3.26035;
  p1_emc_m_dz[1][3][1] = 0.869547;
  p2_emc_m_dz[1][3][1] = -0.135749;
  p0_emc_s_dz[1][3][1] = 1.34714;
  p1_emc_s_dz[1][3][1] = 2.93652;
  p0_emc_m_dz[1][4][1] = -2.45203;
  p1_emc_m_dz[1][4][1] = 0.419907;
  p2_emc_m_dz[1][4][1] = -0.0424349;
  p0_emc_s_dz[1][4][1] = 1.30647;
  p1_emc_s_dz[1][4][1] = 2.96396;
  p0_emc_m_dz[1][5][1] = -1.32997;
  p1_emc_m_dz[1][5][1] = -0.603717;
  p2_emc_m_dz[1][5][1] = 0.151839;
  p0_emc_s_dz[1][5][1] = 1.29883;
  p1_emc_s_dz[1][5][1] = 3.0004;
  p0_emc_m_dz[1][6][1] = -0.463203;
  p1_emc_m_dz[1][6][1] = -1.04087;
  p2_emc_m_dz[1][6][1] = 0.215238;
  p0_emc_s_dz[1][6][1] = 1.32417;
  p1_emc_s_dz[1][6][1] = 3.03268;
  p0_emc_m_dz[1][7][1] = 0.467518;
  p1_emc_m_dz[1][7][1] = -1.58559;
  p2_emc_m_dz[1][7][1] = 0.305232;
  p0_emc_s_dz[1][7][1] = 1.35448;
  p1_emc_s_dz[1][7][1] = 3.15736;
  p0_emc_m_dz[1][8][1] = 1.47392;
  p1_emc_m_dz[1][8][1] = -2.22287;
  p2_emc_m_dz[1][8][1] = 0.410408;
  p0_emc_s_dz[1][8][1] = 1.37151;
  p1_emc_s_dz[1][8][1] = 3.23653;
  p0_emc_m_dz[1][9][1] = 2.07665;
  p1_emc_m_dz[1][9][1] = -2.36009;
  p2_emc_m_dz[1][9][1] = 0.443462;
  p0_emc_s_dz[1][9][1] = 1.37599;
  p1_emc_s_dz[1][9][1] = 3.39522;
  p0_emc_m_dz[2][0][1] = -4.94395;
  p1_emc_m_dz[2][0][1] = 2.68222;
  p2_emc_m_dz[2][0][1] = -0.538384;
  p0_emc_s_dz[2][0][1] = 1.37707;
  p1_emc_s_dz[2][0][1] = 3.45127;
  p0_emc_m_dz[2][1][1] = -4.18286;
  p1_emc_m_dz[2][1][1] = 2.36689;
  p2_emc_m_dz[2][1][1] = -0.446461;
  p0_emc_s_dz[2][1][1] = 1.39413;
  p1_emc_s_dz[2][1][1] = 3.36508;
  p0_emc_m_dz[2][2][1] = -3.37697;
  p1_emc_m_dz[2][2][1] = 1.90959;
  p2_emc_m_dz[2][2][1] = -0.359831;
  p0_emc_s_dz[2][2][1] = 1.41514;
  p1_emc_s_dz[2][2][1] = 3.15576;
  p0_emc_m_dz[2][3][1] = -2.07767;
  p1_emc_m_dz[2][3][1] = 0.917342;
  p2_emc_m_dz[2][3][1] = -0.150598;
  p0_emc_s_dz[2][3][1] = 1.40094;
  p1_emc_s_dz[2][3][1] = 3.05006;
  p0_emc_m_dz[2][4][1] = -1.33865;
  p1_emc_m_dz[2][4][1] = 0.546684;
  p2_emc_m_dz[2][4][1] = -0.0712068;
  p0_emc_s_dz[2][4][1] = 1.31596;
  p1_emc_s_dz[2][4][1] = 3.06029;
  p0_emc_m_dz[2][5][1] = -0.35184;
  p1_emc_m_dz[2][5][1] = -0.104707;
  p2_emc_m_dz[2][5][1] = 0.0207666;
  p0_emc_s_dz[2][5][1] = 1.29003;
  p1_emc_s_dz[2][5][1] = 3.02847;
  p0_emc_m_dz[2][6][1] = 0.543685;
  p1_emc_m_dz[2][6][1] = -0.643686;
  p2_emc_m_dz[2][6][1] = 0.144687;
  p0_emc_s_dz[2][6][1] = 1.41348;
  p1_emc_s_dz[2][6][1] = 2.90802;
  p0_emc_m_dz[2][7][1] = 1.4756;
  p1_emc_m_dz[2][7][1] = -1.22489;
  p2_emc_m_dz[2][7][1] = 0.248974;
  p0_emc_s_dz[2][7][1] = 1.41199;
  p1_emc_s_dz[2][7][1] = 2.97547;
  p0_emc_m_dz[2][8][1] = 2.26263;
  p1_emc_m_dz[2][8][1] = -1.69476;
  p2_emc_m_dz[2][8][1] = 0.343696;
  p0_emc_s_dz[2][8][1] = 1.43275;
  p1_emc_s_dz[2][8][1] = 3.07814;
  p0_emc_m_dz[2][9][1] = 3.12681;
  p1_emc_m_dz[2][9][1] = -2.22195;
  p2_emc_m_dz[2][9][1] = 0.487032;
  p0_emc_s_dz[2][9][1] = 1.38888;
  p1_emc_s_dz[2][9][1] = 3.23974;

  //cout<<"MatchrecalReco7GeVRun10::InitPars() ::::: Loaded array for Mean and Sigma for Matching Calibrations"<<endl;
  return 1;
}
//_____________________________________________________________________________________________________________________________

