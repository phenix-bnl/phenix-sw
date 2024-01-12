#include "MatchrecalReco.h"
#include <Fun4AllReturnCodes.h>

#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>
#include <getClass.h>
#include <RunHeader.h>
#include <utiCentrality.h>

#include <PHCompositeNode.h>
#include <RunNumberRanges.h>
#include <recoConsts.h>

#include <iostream>

using namespace std;
using namespace findNode;

MatchrecalReco::MatchrecalReco(const char* name): Recalibrator(name)
{
  baseclasses.insert("PHCentralTrack");

  // Initialization
  // Mean dphi
  for(int ialpha=0;ialpha<NCH;ialpha++){
    for(int iz=0;iz<NZED;iz++){
      for(int ipar=0;ipar<3;ipar++){
        // PC2, and TOF
        PC2_dphi_mean[ialpha][iz][ipar] = 0.0;
        TOF_dphi_mean[ialpha][iz][ipar] = 0.0;

        // PC3, and EMC
        for(int ia=0;ia<NARM;ia++){
          PC3_dphi_mean[ia][ialpha][iz][ipar] = 0.0;
          EMC_dphi_mean[ia][ialpha][iz][ipar] = 0.0;
        }
      }
    }
  }

  // Sigma dphi
  for(int ipar=0;ipar<3;ipar++){
    PC2_dphi_sigma[ipar] = 0.0;
    TOF_dphi_sigma[ipar] = 0.0;
    for(int ia=0;ia<NARM;ia++){
      PC3_dphi_sigma[ia][ipar] = 0.0;
      EMC_dphi_sigma[ia][ipar] = 0.0;
    }
  }

  // Mean dz
  for(int ialpha=0;ialpha<NCH;ialpha++){
    for(int iz=0;iz<NZED;iz++){
      for(int ipar=0;ipar<2;ipar++){
        // PC2, and TOF
        PC2_dz_mean_lowpt[ialpha][iz][ipar] = 0.0;
        PC2_dz_mean_highpt[ialpha][iz][ipar] = 0.0;

        TOF_dz_mean_lowpt[ialpha][iz][ipar] = 0.0;
        TOF_dz_mean_highpt[ialpha][iz][ipar] = 0.0;

        // PC3, and EMC
        for(int ia=0;ia<NARM;ia++){
          PC3_dz_mean_lowpt[ia][ialpha][iz][ipar] = 0.0;
          PC3_dz_mean_highpt[ia][ialpha][iz][ipar] = 0.0;

          EMC_dz_mean_lowpt[ia][ialpha][iz][ipar] = 0.0;
          EMC_dz_mean_highpt[ia][ialpha][iz][ipar] = 0.0;
        }
      }

      for(int ic=0;ic<NCENT;ic++){
        // PC2, and TOF
        PC2_dz_mean_offset_lowpt[ialpha][iz][ic] = 0.0;
        TOF_dz_mean_offset_lowpt[ialpha][iz][ic] = 0.0;

        PC2_dz_mean_offset_highpt[ialpha][iz][ic] = 0.0;
        TOF_dz_mean_offset_highpt[ialpha][iz][ic] = 0.0;

        // PC3, and EMC
        for(int ia=0;ia<NARM;ia++){
          PC3_dz_mean_offset_lowpt[ia][ialpha][iz][ic] = 0.0;
          EMC_dz_mean_offset_lowpt[ia][ialpha][iz][ic] = 0.0;

          PC3_dz_mean_offset_highpt[ia][ialpha][iz][ic] = 0.0;
          EMC_dz_mean_offset_highpt[ia][ialpha][iz][ic] = 0.0;
        }
      }

    }
  }

  // Sigma dz
  for(int ialpha=0;ialpha<NCH;ialpha++){
    // PC2, and TOF
    PC2_dz_sigma_slope[ialpha] = 0.0;
    TOF_dz_sigma_slope[ialpha] = 0.0;

    for(int ia=0;ia<NARM;ia++){
      // PC3
      PC3_dz_sigma_slope[ia][ialpha] = 0.0;

      // EMC
      for(int iz=0;iz<2;iz++){
        EMC_dz_sigma_slope[ia][ialpha][iz] = 0.0;
      }
    }
  }

  for(int ipar=0;ipar<5;ipar++){
    // PC2, and TOF
    PC2_dz_sigma_offset[ipar] = 0.0;
    TOF_dz_sigma_offset[ipar] = 0.0;

    for(int ia=0;ia<NARM;ia++){
      // PC3
      PC3_dz_sigma_offset[ia][ipar] = 0.0;

      // EMC
      for(int iz=0;iz<2;iz++){
        EMC_dz_sigma_offset[ia][iz][ipar] = 0.0;
      }
    }
  }

  // Scale factor for sigma
  for(int iz=0;iz<NZED;iz++){
    for(int ipar=0;ipar<3;ipar++){
      // PC2, and TOF
      PC2_dphi_sigma_scale[iz][ipar] = 0.0;
      TOF_dphi_sigma_scale[iz][ipar] = 0.0;
      PC2_dz_sigma_scale[iz][ipar] = 0.0;
      TOF_dz_sigma_scale[iz][ipar] = 0.0;

      // PC3, and EMC
      for(int ia=0;ia<NARM;ia++){
        PC3_dphi_sigma_scale[ia][iz][ipar] = 0.0;
        EMC_dphi_sigma_scale[ia][iz][ipar] = 0.0;
        PC3_dz_sigma_scale[ia][iz][ipar] = 0.0;
        EMC_dz_sigma_scale[ia][iz][ipar] = 0.0;
      }
    }
  }



}

int
MatchrecalReco::InitRun(PHCompositeNode *topNode)
{
  run_Burn_pc2_match = 2;
  run_Burn_pc3_match = 2;
  run_Burn_tof_match = 2;
  run_Burn_Run4_match = 2;

  RunHeader* d_run = getClass<RunHeader>(topNode, "RunHeader");
  if(!d_run){
    cout << PHWHERE << " RunHeader not found" << endl;
    return 0;
  }

  runNumber = d_run->get_RunNumber();


  // Run4 200 GeV
  if (runNumber >= 107445 && runNumber < 122470)
    {
      InitRun4();
    }


  return 0;
}

int
MatchrecalReco::isValidRun(const int runno) const
{
  if (runno < (int) BEGIN_OF_RUN4 || runno > (int) BEGIN_OF_RUN5)
    {
      return 0;
    }

  return 1;
}


int
MatchrecalReco::process_event(PHCompositeNode *topNode)
{
  d_cnt = getClass<PHCentralTrack>(topNode, inputnodename.c_str());
  if (!d_cnt)
    {
      return 0;
    }

  // Run4 62.4 GeV ...
  if ( runNumber >= 122470 && runNumber <= 123564 )
    {
      Burn_pc2_match();
      Burn_pc3_match();
      Burn_tof_match();
    }
  else
    {
      Burn_match_Run4(topNode);
    }

  return EVENT_OK;
}


int MatchrecalReco::Burn_match_Run4(PHCompositeNode* topNode)
{
  if (!run_Burn_Run4_match)
    {
      if (verbosity > 0)
        {
          cout << PHWHERE << " " << Name() << " Run4 burning disabled" << endl;
        }
      return 0;
    }

  int cent  = PhUtilities::getCentralityByClockRun4(topNode);
  if( Centrality(cent)==-1 ){
    return 1;
  }

  for (unsigned int itrk = 0; itrk < d_cnt->get_npart(); itrk++)
    {
      PHSnglCentralTrack *sngltrk = d_cnt->get_track(itrk);

      sngltrk->ShutUp();
      if (run_Burn_Run4_match == 2)
        {
          if (sngltrk->isImplemented(sngltrk->get_dcarm()) &&
              sngltrk->isImplemented(sngltrk->get_mom()) &&
              sngltrk->isImplemented(sngltrk->get_the0()) &&
              sngltrk->isImplemented(sngltrk->get_zed()) &&
              sngltrk->isImplemented(sngltrk->get_alpha()) &&
              sngltrk->isImplemented(sngltrk->get_pc2dphi()) &&
              sngltrk->isImplemented(sngltrk->get_pc2dz()) &&
              sngltrk->isImplemented(sngltrk->get_pc3dphi()) &&
              sngltrk->isImplemented(sngltrk->get_pc3dz()) &&
              sngltrk->isImplemented(sngltrk->get_tofdphi()) &&
              sngltrk->isImplemented(sngltrk->get_tofdz()) &&
              sngltrk->isImplemented(sngltrk->get_emcdphi()) &&
              sngltrk->isImplemented(sngltrk->get_emcdz()) )
            {
              if (verbosity > 0)
                {
                  cout << PHWHERE << " " << Name() << " Run4 burning workable" << endl;
                }
              run_Burn_Run4_match = 1;
            }
          else
            {
              run_Burn_Run4_match = 0;
              sngltrk->ShutUp(1);
              if (verbosity > 0)
                {
                  cout << PHWHERE << " " << Name() << " Run4 burning not possible" << endl;
                }
              return 0;
            }
        }
      sngltrk->ShutUp(1);

      float mom  = sngltrk->get_mom();
      float the0 = sngltrk->get_the0();
      float zed  = sngltrk->get_zed();
      float pt   = mom;
      if (the0 > -999 && zed > -999)
        {
          pt = mom * sin(the0);
        }

      short dcarm = sngltrk->get_dcarm();
      int ialpha  = Alpha(sngltrk->get_alpha());
      int ized    = Zed(sngltrk->get_zed());
      if(ized==-1){
        continue;
      }


      // PC3
      float pc3sdphi = GetPC3sdphi(dcarm, ialpha, ized, pt, sngltrk->get_pc3dphi());
      float pc3sdz   = GetPC3sdz(dcarm, ialpha, ized, cent, pt, sngltrk->get_pc3dz());

      // PC2
      float pc2sdphi  = GetPC2sdphi(dcarm, ialpha, ized, pt, sngltrk->get_pc2dphi());
      float pc2sdz    = GetPC2sdz(dcarm, ialpha, ized, cent, pt, sngltrk->get_pc2dz());

      // TOF
      float tofsdphi  = GetTOFsdphi(dcarm, ialpha, ized, pt, sngltrk->get_tofdphi());
      float tofsdz    = GetTOFsdz(dcarm, ialpha, ized, cent, pt, sngltrk->get_tofdz());

      // EMC
      float emcsdphi  = GetEMCsdphi(dcarm, ialpha, ized, pt, sngltrk->get_emcdphi());
      float emcsdz    = GetEMCsdz(dcarm, ialpha, ized, cent, pt, sngltrk->get_emcdz());

      // Set all variables
      // Kill virtual warnings
      sngltrk->ShutUp(1);
      sngltrk->set_pc3sdphi(pc3sdphi);
      sngltrk->set_pc3sdz(pc3sdz);
      sngltrk->set_pc2sdphi(pc2sdphi);
      sngltrk->set_pc2sdz(pc2sdz);
      sngltrk->set_tofsdphi(tofsdphi);
      sngltrk->set_tofsdz(tofsdz);
      sngltrk->set_emcsdphi(emcsdphi);
      sngltrk->set_emcsdz(emcsdz);
      sngltrk->ShutUp(0);

      /*
      sngltrk->set_pc3dphi(pc3sdphi);
      sngltrk->set_pc3dz(pc3sdz);
      sngltrk->set_pc2dphi(pc2sdphi);
      sngltrk->set_pc2dz(pc2sdz);
      sngltrk->set_tofdphi(tofsdphi);
      sngltrk->set_tofdz(tofsdz);
      sngltrk->set_emcdphi(emcsdphi);
      sngltrk->set_emcdz(emcsdz);
      */

    }
  return EVENT_OK;
}

//_____________________________________________________________________________________
// PC2 Box matching cuts 
bool MatchrecalReco::IsPC2BoxCutsOK(PHCompositeNode* topNode, const int itrk, const int cent, const double Nsigma)
{
  PHSnglCentralTrack* sngltrk = getClass<PHCentralTrack>(topNode, "PHCentralTrack")->get_track(itrk);
  if(!sngltrk){
    cout << PHWHERE << " PHSnglCentralTrack not found" << endl;
    return false;
  }

  short dcarm = sngltrk->get_dcarm();
  int ialpha  = Alpha(sngltrk->get_alpha());
  int ized    = Zed(sngltrk->get_zed());
  float pt    = sngltrk->get_mom() * sin(sngltrk->get_the0());

  float pc2sdphi  = GetPC2sdphi(dcarm, ialpha, ized, pt, sngltrk->get_pc2dphi());
  float pc2sdz    = GetPC2sdz(dcarm, ialpha, ized, cent, pt, sngltrk->get_pc2dz());

  return (fabs(pc2sdphi) < Nsigma && fabs(pc2sdz) < Nsigma );
}

//_____________________________________________________________________________________
// PC3 Box matching cuts 
bool MatchrecalReco::IsPC3BoxCutsOK(PHCompositeNode* topNode, const int itrk, const int cent, const double Nsigma)
{
  PHSnglCentralTrack* sngltrk = getClass<PHCentralTrack>(topNode, "PHCentralTrack")->get_track(itrk);
  if(!sngltrk){
    cout << PHWHERE << " PHSnglCentralTrack not found" << endl;
    return false;
  }

  short dcarm = sngltrk->get_dcarm();
  int ialpha  = Alpha(sngltrk->get_alpha());
  int ized    = Zed(sngltrk->get_zed());
  float pt    = sngltrk->get_mom() * sin(sngltrk->get_the0());

  float pc3sdphi  = GetPC3sdphi(dcarm, ialpha, ized, pt, sngltrk->get_pc3dphi());
  float pc3sdz    = GetPC3sdz(dcarm, ialpha, ized, cent, pt, sngltrk->get_pc3dz());

  return (fabs(pc3sdphi) < Nsigma && fabs(pc3sdz) < Nsigma );
}

//_____________________________________________________________________________________
// TOF Box matching cuts 
bool MatchrecalReco::IsTOFBoxCutsOK(PHCompositeNode* topNode, const int itrk, const int cent, const double Nsigma)
{
  PHSnglCentralTrack* sngltrk = getClass<PHCentralTrack>(topNode, "PHCentralTrack")->get_track(itrk);
  if(!sngltrk){
    cout << PHWHERE << " PHSnglCentralTrack not found" << endl;
    return false;
  }

  short dcarm = sngltrk->get_dcarm();
  int ialpha  = Alpha(sngltrk->get_alpha());
  int ized    = Zed(sngltrk->get_zed());
  float pt    = sngltrk->get_mom() * sin(sngltrk->get_the0());

  float tofsdphi  = GetTOFsdphi(dcarm, ialpha, ized, pt, sngltrk->get_tofdphi());
  float tofsdz    = GetTOFsdz(dcarm, ialpha, ized, cent, pt, sngltrk->get_tofdz());

  return (fabs(tofsdphi) < Nsigma && fabs(tofsdz) < Nsigma );
}

//_____________________________________________________________________________________
// EMC Box matching cuts 
bool MatchrecalReco::IsEMCBoxCutsOK(PHCompositeNode* topNode, const int itrk, const int cent, const double Nsigma)
{
  PHSnglCentralTrack* sngltrk = getClass<PHCentralTrack>(topNode, "PHCentralTrack")->get_track(itrk);
  if(!sngltrk){
    cout << PHWHERE << " PHSnglCentralTrack not found" << endl;
    return false;
  }

  short dcarm = sngltrk->get_dcarm();
  int ialpha  = Alpha(sngltrk->get_alpha());
  int ized    = Zed(sngltrk->get_zed());
  float pt    = sngltrk->get_mom() * sin(sngltrk->get_the0());

  float emcsdphi  = GetEMCsdphi(dcarm, ialpha, ized, pt, sngltrk->get_emcdphi());
  float emcsdz    = GetEMCsdz(dcarm, ialpha, ized, cent, pt, sngltrk->get_emcdz());

  return (fabs(emcsdphi) < Nsigma && fabs(emcsdz) < Nsigma );
}


//_____________________________________________________________________________________
// PC2 Radial matching cuts 
bool MatchrecalReco::IsPC2RadialCutsOK(PHCompositeNode* topNode, const int itrk, const int cent, const double Nsigma)
{
  PHSnglCentralTrack* sngltrk = getClass<PHCentralTrack>(topNode, "PHCentralTrack")->get_track(itrk);
  if(!sngltrk){
    cout << PHWHERE << " PHSnglCentralTrack not found" << endl;
    return false;
  }

  short dcarm = sngltrk->get_dcarm();
  int ialpha  = Alpha(sngltrk->get_alpha());
  int ized    = Zed(sngltrk->get_zed());
  float pt    = sngltrk->get_mom() * sin(sngltrk->get_the0());

  float pc2sdphi  = GetPC2sdphi(dcarm, ialpha, ized, pt, sngltrk->get_pc2dphi());
  float pc2sdz    = GetPC2sdz(dcarm, ialpha, ized, cent, pt, sngltrk->get_pc2dz());

  return sqrt(pc2sdphi*pc2sdphi + pc2sdz*pc2sdz) < Nsigma ;
}

//_____________________________________________________________________________________
// PC3 Radial matching cuts 
bool MatchrecalReco::IsPC3RadialCutsOK(PHCompositeNode* topNode, const int itrk, const int cent, const double Nsigma)
{
  PHSnglCentralTrack* sngltrk = getClass<PHCentralTrack>(topNode, "PHCentralTrack")->get_track(itrk);
  if(!sngltrk){
    cout << PHWHERE << " PHSnglCentralTrack not found" << endl;
    return false;
  }

  short dcarm = sngltrk->get_dcarm();
  int ialpha  = Alpha(sngltrk->get_alpha());
  int ized    = Zed(sngltrk->get_zed());
  float pt    = sngltrk->get_mom() * sin(sngltrk->get_the0());

  float pc3sdphi  = GetPC3sdphi(dcarm, ialpha, ized, pt, sngltrk->get_pc3dphi());
  float pc3sdz    = GetPC3sdz(dcarm, ialpha, ized, cent, pt, sngltrk->get_pc3dz());

  return sqrt(pc3sdphi*pc3sdphi + pc3sdz*pc3sdz) < Nsigma ;
}

//_____________________________________________________________________________________
// TOF Radial matching cuts 
bool MatchrecalReco::IsTOFRadialCutsOK(PHCompositeNode* topNode, const int itrk, const int cent, const double Nsigma)
{
  PHSnglCentralTrack* sngltrk = getClass<PHCentralTrack>(topNode, "PHCentralTrack")->get_track(itrk);
  if(!sngltrk){
    cout << PHWHERE << " PHSnglCentralTrack not found" << endl;
    return false;
  }

  short dcarm = sngltrk->get_dcarm();
  int ialpha  = Alpha(sngltrk->get_alpha());
  int ized    = Zed(sngltrk->get_zed());
  float pt    = sngltrk->get_mom() * sin(sngltrk->get_the0());

  float tofsdphi  = GetTOFsdphi(dcarm, ialpha, ized, pt, sngltrk->get_tofdphi());
  float tofsdz    = GetTOFsdz(dcarm, ialpha, ized, cent, pt, sngltrk->get_tofdz());

  return sqrt(tofsdphi*tofsdphi + tofsdz*tofsdz) < Nsigma ;
}

//_____________________________________________________________________________________
// EMC Radial matching cuts 
bool MatchrecalReco::IsEMCRadialCutsOK(PHCompositeNode* topNode, const int itrk, const int cent, const double Nsigma)
{
  PHSnglCentralTrack* sngltrk = getClass<PHCentralTrack>(topNode, "PHCentralTrack")->get_track(itrk);
  if(!sngltrk){
    cout << PHWHERE << " PHSnglCentralTrack not found" << endl;
    return false;
  }

  short dcarm = sngltrk->get_dcarm();
  int ialpha  = Alpha(sngltrk->get_alpha());
  int ized    = Zed(sngltrk->get_zed());
  float pt    = sngltrk->get_mom() * sin(sngltrk->get_the0());

  float emcsdphi  = GetEMCsdphi(dcarm, ialpha, ized, pt, sngltrk->get_emcdphi());
  float emcsdz    = GetEMCsdz(dcarm, ialpha, ized, cent, pt, sngltrk->get_emcdz());

  return sqrt(emcsdphi*emcsdphi + emcsdz*emcsdz) < Nsigma ;
}


//_____________________________________________________________________________________
// PC2 delta phi
float MatchrecalReco::GetPC2sdphi(const int iarm, const int ialpha, const int ized, const float pt, const float pc2dphi)
{
  float mean = PC2_dphi_mean[ialpha][ized][0];
  if(ized==0 || ized==3){
    if(pt==0) mean = 0.0;
    else{
      mean = PC2_dphi_mean[ialpha][ized][0] + PC2_dphi_mean[ialpha][ized][1]/pt + PC2_dphi_mean[ialpha][ized][2]/(pt*pt);
    }
  }

  double par[6];
  for(int ipar=0;ipar<3;ipar++){
    par[ipar]   = PC2_dphi_sigma[ipar];
    par[ipar+3] = PC2_dphi_sigma_scale[ized][ipar];
  }

  return GetDphiDz(pc2dphi, mean, GetSigma(par, pt) );
}

//_____________________________________________________________________________________
// PC2 delta z
float MatchrecalReco::GetPC2sdz(const int iarm, const int ialpha, const int ized, const int cent, const float pt, const float pc2dz)
{
  int icent = Centrality(cent);
  if (icent < 0)
    {
      return NAN;
    }

  double mpar[3];
  if( pt < 1.5 ){
    mpar[0] = PC2_dz_mean_lowpt[ialpha][ized][0];
    mpar[1] = PC2_dz_mean_lowpt[ialpha][ized][1];
    mpar[2] = PC2_dz_mean_offset_lowpt[ialpha][ized][icent];
  }
  else{ // pt > 1.5 GeV/c
    mpar[0] = PC2_dz_mean_highpt[ialpha][ized][0];
    mpar[1] = PC2_dz_mean_highpt[ialpha][ized][1];
    mpar[2] = PC2_dz_mean_offset_highpt[ialpha][ized][icent];
  }
  float mean = GetExp(mpar, pt);

  double par[6];
  par[0] = PC2_dz_sigma_slope[ialpha];
  par[1] = GetSigmaParameter(PC2_dz_sigma_offset, (double)cent);
  par[2] = 1.0;
  for(int ipar=0;ipar<3;ipar++){
    par[ipar+3] = PC2_dz_sigma_scale[ized][ipar];
  }

  return GetDphiDz(pc2dz, mean, GetSigma(par, pt) );
}

//_____________________________________________________________________________________

//_____________________________________________________________________________________
// PC3 delta phi
float MatchrecalReco::GetPC3sdphi(const int iarm, const int ialpha, const int ized, const float pt, const float pc3dphi)
{
  float mean = PC3_dphi_mean[iarm][ialpha][ized][0];
  if(ized==0 || ized==3){
    if(pt==0) mean = 0.0;
    else{
      mean = PC3_dphi_mean[iarm][ialpha][ized][0] + PC3_dphi_mean[iarm][ialpha][ized][1]/pt 
        + PC3_dphi_mean[iarm][ialpha][ized][2]/(pt*pt);
    }
  }

  double par[6];
  for(int ipar=0;ipar<3;ipar++){
    par[ipar]   = PC3_dphi_sigma[iarm][ipar];
    par[ipar+3] = PC3_dphi_sigma_scale[iarm][ized][ipar];
  }

  return GetDphiDz(pc3dphi, mean, GetSigma(par, pt) );
}

//_____________________________________________________________________________________
// PC3 delta z
float MatchrecalReco::GetPC3sdz(const int iarm, const int ialpha, const int ized, const int cent, const float pt, const float pc3dz)
{
  int icent = Centrality(cent);
  if (icent < 0)
    {
      return NAN;
    }
  double mpar[3];
  if( pt < 1.5 ){
    mpar[0] = PC3_dz_mean_lowpt[iarm][ialpha][ized][0];
    mpar[1] = PC3_dz_mean_lowpt[iarm][ialpha][ized][1];
    mpar[2] = PC3_dz_mean_offset_lowpt[iarm][ialpha][ized][icent];
  }
  else{ // pt > 1.5 GeV/c
    mpar[0] = PC3_dz_mean_highpt[iarm][ialpha][ized][0];
    mpar[1] = PC3_dz_mean_highpt[iarm][ialpha][ized][1];
    mpar[2] = PC3_dz_mean_offset_highpt[iarm][ialpha][ized][icent];
  }
  float mean = GetExp(mpar, pt);

  double par[6];
  par[0] = PC3_dz_sigma_slope[iarm][ialpha];
  par[1] = GetSigmaParameter(PC3_dz_sigma_offset[iarm], (double)cent);
  par[2] = 1.0;
  for(int ipar=0;ipar<3;ipar++){
    par[ipar+3] = PC3_dz_sigma_scale[iarm][ized][ipar];
  }

  return GetDphiDz(pc3dz, mean, GetSigma(par, pt) );
}

// TOF delta phi
float MatchrecalReco::GetTOFsdphi(const int iarm, const int ialpha, const int ized, const float pt, const float tofdphi)
{
  double mean = GetExp(TOF_dphi_mean[ialpha][ized], pt);

  double par[6];
  for(int ipar=0;ipar<3;ipar++){
    par[ipar]   = TOF_dphi_sigma[ipar];
    par[ipar+3] = TOF_dphi_sigma_scale[ized][ipar];
  }

  return GetDphiDz(tofdphi, mean, GetSigma(par, pt) );
}

//_____________________________________________________________________________________
// TOF delta z
float MatchrecalReco::GetTOFsdz(const int iarm, const int ialpha, const int ized, const int cent, const float pt, const float tofdz)
{
  int icent = Centrality(cent);
  if (icent < 0)
    {
      return NAN;
    }

  double mpar[3];
  if( pt < 1.5 ){
    mpar[0] = TOF_dz_mean_lowpt[ialpha][ized][0];
    mpar[1] = TOF_dz_mean_lowpt[ialpha][ized][1];
    mpar[2] = TOF_dz_mean_offset_lowpt[ialpha][ized][icent];
  }
  else{ // pt > 1.5 GeV/c
    mpar[0] = TOF_dz_mean_highpt[ialpha][ized][0];
    mpar[1] = TOF_dz_mean_highpt[ialpha][ized][1];
    mpar[2] = TOF_dz_mean_offset_highpt[ialpha][ized][icent];
  }
  float mean = GetExp(mpar, pt);

  double par[6];
  par[0] = TOF_dz_sigma_slope[ialpha];
  par[1] = GetSigmaParameter(TOF_dz_sigma_offset, (double)cent);
  par[2] = 1.0;
  for(int ipar=0;ipar<3;ipar++){
    par[ipar+3] = TOF_dz_sigma_scale[ized][ipar];
  }

  return GetDphiDz(tofdz, mean, GetSigma(par, pt) );
}

//_____________________________________________________________________________________
// EMC delta phi
float MatchrecalReco::GetEMCsdphi(const int iarm, const int ialpha, const int ized, const float pt, const float emcdphi)
{
  float mean = 0.0;
  if(ized==0 || ized==3){
    if(pt==0) mean = 0.0;
    else{
      mean = EMC_dphi_mean[iarm][ialpha][ized][0] + EMC_dphi_mean[iarm][ialpha][ized][1]/pt 
        + EMC_dphi_mean[iarm][ialpha][ized][2]/(pt*pt);
    }
  }
  else{ // ized==1 || ized==2
    mean = GetExp(EMC_dphi_mean[iarm][ialpha][ized], pt);
  }

  double par[6];
  for(int ipar=0;ipar<3;ipar++){
    par[ipar]   = EMC_dphi_sigma[iarm][ipar];
    par[ipar+3] = EMC_dphi_sigma_scale[iarm][ized][ipar];
  }

  return GetDphiDz(emcdphi, mean, GetSigma(par, pt) );
}

//_____________________________________________________________________________________
// EMC delta z
float MatchrecalReco::GetEMCsdz(const int iarm, const int ialpha, const int ized, const int cent, const float pt, const float emcdz)
{
  int icent = Centrality(cent);
  if (icent < 0)
    {
      return NAN;
    }
  double mpar[3];
  if( pt < 1.5 ){
    mpar[0] = EMC_dz_mean_lowpt[iarm][ialpha][ized][0];
    mpar[1] = EMC_dz_mean_lowpt[iarm][ialpha][ized][1];
    mpar[2] = EMC_dz_mean_offset_lowpt[iarm][ialpha][ized][icent];
  }
  else{ // pt > 1.5 GeV/c
    mpar[0] = EMC_dz_mean_highpt[iarm][ialpha][ized][0];
    mpar[1] = EMC_dz_mean_highpt[iarm][ialpha][ized][1];
    mpar[2] = EMC_dz_mean_offset_highpt[iarm][ialpha][ized][icent];
  }
  float mean = GetExp(mpar, pt);

  // 0: |zed|<40 cm,   1: |zed|>40 cm
  int jzed = (ized==1||ized==2) ? 0 : 1;

  double par[6];
  par[0] = EMC_dz_sigma_slope[iarm][ialpha][jzed];
  par[1] = GetSigmaParameter(EMC_dz_sigma_offset[iarm][jzed], (double)cent);
  par[2] = 1.0;
  for(int ipar=0;ipar<3;ipar++){
    par[ipar+3] = EMC_dz_sigma_scale[iarm][ized][ipar];
  }

  return GetDphiDz(emcdz, mean, GetSigma(par, pt) );
}


float MatchrecalReco::GetExp(const double* par, const float pt) const
{
  return par[0] * exp( - par[1] * pt ) + par[2] ;
}

float MatchrecalReco::GetSigmaParameter(const double* par, const double cent) const
{
  return par[0]
    + par[1] * cent
    + par[2] * cent * cent
    + par[3] * cent * cent * cent
    + par[4] * cent * cent * cent * cent;
}

float MatchrecalReco::GetSigma(const double* par, const float pt) const
{
  if( pt == 0 ){
    if( verbosity > 0 ){
      cout << PHWHERE << " GetSigma():: Error : pt = 0." << endl;
    }

    return 0.0;
  }

  // Parameterization of sigma vs pt
  double sigma = sqrt(par[0]*par[0]/(pt*pt) + par[1]*par[1])/par[2] ;


  // Additional pt dependent scale factor
  double spar[3];
  spar[0] = par[3];
  spar[1] = par[4];
  spar[2] = par[5];

  double scale = GetExp(spar, pt);

  return  sigma * scale ;
}


float MatchrecalReco::GetDphiDz(const float dphidz, const float mean, const float sigma) const
{
  if( sigma == 0 ){
    if( verbosity > 0 ){
      cout << PHWHERE << " Error : sigma parameter = 0. return uncorrected variable" << endl;
    }

    return dphidz;
  }

  return ( dphidz - mean ) / sigma ;
}

//____________________________________________________________________________________
// Ids
int MatchrecalReco::Alpha(const float alpha) const
{
  return (alpha<0) ? 0 : 1;
}

int MatchrecalReco::Zed(const float zed) const
{
  if (zed < -80 || zed > 80) 
    return -1;
  
  int ized = (int)((zed+80.0)/160.0 * 4.0);

  return ized;
}

int MatchrecalReco::Centrality(const int cent) const
{
  int cent_new = (int)(cent-0.001);

  if(cent_new<0||cent_new>100){
    if(verbosity>1){
      cout << PHWHERE << " Error : cent= " << cent_new << endl;
      return -1;
    }
  }

  if(cent_new<10)      return 0;
  else if(cent_new<20) return 1;
  else if(cent_new<30) return 2;
  else if(cent_new<40) return 3;
  else if(cent_new<50) return 4;
  else if(cent_new<60) return 5;
  else if(cent_new<70) return 6;
  else if(cent_new<80) return 7;
  else if(cent_new<93) return 8;
  else{
    return -1;
  }

  return -1;

}


// Initialization of Run4 matching parameters
int MatchrecalReco::InitRun4()
{
  cout << "MatchrecalReco:: Initialization of Run4 Au+Au 200 GeV matching parameters" << endl;

  //
  // Initialization
  //
  //----------------------------------------------------------------------------
  // Delta phi mean
  //----------------------------------------------------------------------------
  // PC2 delta phi mean
  PC2_dphi_mean[0][0][0]    =    0.000237436;   PC2_dphi_mean[0][0][1]    =    0.000121893;   PC2_dphi_mean[0][0][2]    =   -0.000597117; 
  PC2_dphi_mean[1][0][0]    =    -8.6732e-05;   PC2_dphi_mean[1][0][1]    =   -0.000255144;   PC2_dphi_mean[1][0][2]    =     0.00061531; 
  PC2_dphi_mean[0][1][0]    =    0.000146459; 
  PC2_dphi_mean[1][1][0]    =   -0.000308724; 
  PC2_dphi_mean[0][2][0]    =    0.000161404; 
  PC2_dphi_mean[1][2][0]    =   -0.000290955; 
  PC2_dphi_mean[0][3][0]    =    0.000132057;   PC2_dphi_mean[0][3][1]    =    0.000233562;   PC2_dphi_mean[0][3][2]    =   -0.000637648; 
  PC2_dphi_mean[1][3][0]    =   -0.000135407;   PC2_dphi_mean[1][3][1]    =    -0.00027831;   PC2_dphi_mean[1][3][2]    =    0.000641312; 

  // PC3 delta phi mean
  PC3_dphi_mean[0][0][0][0] =    7.92062e-06;   PC3_dphi_mean[0][0][0][1] =    0.000409708;   PC3_dphi_mean[0][0][0][2] =   -0.000641215; 
  PC3_dphi_mean[0][1][0][0] =   -2.81675e-05;   PC3_dphi_mean[0][1][0][1] =   -0.000565078;   PC3_dphi_mean[0][1][0][2] =    0.000745395; 
  PC3_dphi_mean[0][0][1][0] =    0.000397299; 
  PC3_dphi_mean[0][1][1][0] =   -0.000204097; 
  PC3_dphi_mean[0][0][2][0] =     0.00016035; 
  PC3_dphi_mean[0][1][2][0] =   -0.000228482; 
  PC3_dphi_mean[0][0][3][0] =    0.000212101;   PC3_dphi_mean[0][0][3][1] =    7.44109e-05;   PC3_dphi_mean[0][0][3][2] =   -0.000654329; 
  PC3_dphi_mean[0][1][3][0] =    0.000179821;   PC3_dphi_mean[0][1][3][1] =    -0.00024679;   PC3_dphi_mean[0][1][3][2] =    0.000702705; 
  PC3_dphi_mean[1][0][0][0] =     0.00027575;   PC3_dphi_mean[1][0][0][1] =   -1.49254e-06;   PC3_dphi_mean[1][0][0][2] =   -0.000639281; 
  PC3_dphi_mean[1][1][0][0] =   -0.000103282;   PC3_dphi_mean[1][1][0][1] =   -3.64231e-05;   PC3_dphi_mean[1][1][0][2] =    0.000631692; 
  PC3_dphi_mean[1][0][1][0] =    9.71047e-05; 
  PC3_dphi_mean[1][1][1][0] =   -0.000382726; 
  PC3_dphi_mean[1][0][2][0] =    0.000275887; 
  PC3_dphi_mean[1][1][2][0] =   -0.000163808; 
  PC3_dphi_mean[1][0][3][0] =    3.20262e-05;   PC3_dphi_mean[1][0][3][1] =    1.87706e-05;   PC3_dphi_mean[1][0][3][2] =   -0.000629097; 
  PC3_dphi_mean[1][1][3][0] =   -0.000303173;   PC3_dphi_mean[1][1][3][1] =   -0.000201794;   PC3_dphi_mean[1][1][3][2] =    0.000698886; 

  // TOF delta phi mean
  TOF_dphi_mean[0][0][0]    =     -0.0326328;   TOF_dphi_mean[0][0][1]    =         7.3734;   TOF_dphi_mean[0][0][2]    =     0.00100035; 
  TOF_dphi_mean[1][0][0]    =      0.0636145;   TOF_dphi_mean[1][0][1]    =        7.93525;   TOF_dphi_mean[1][0][2]    =   -0.000157875; 
  TOF_dphi_mean[0][1][0]    =     0.00386857;   TOF_dphi_mean[0][1][1]    =        1.85365;   TOF_dphi_mean[0][1][2]    =     0.00106368; 
  TOF_dphi_mean[1][1][0]    =    -0.00298648;   TOF_dphi_mean[1][1][1]    =        1.56878;   TOF_dphi_mean[1][1][2]    =   -3.57252e-05; 
  TOF_dphi_mean[0][2][0]    =     0.00470834;   TOF_dphi_mean[0][2][1]    =        2.29136;   TOF_dphi_mean[0][2][2]    =     0.00102251; 
  TOF_dphi_mean[1][2][0]    =    -0.00228267;   TOF_dphi_mean[1][2][1]    =        1.23658;   TOF_dphi_mean[1][2][2]    =    0.000122902; 
  TOF_dphi_mean[0][3][0]    =      -0.677775;   TOF_dphi_mean[0][3][1]    =        13.6155;   TOF_dphi_mean[0][3][2]    =    0.000609074; 
  TOF_dphi_mean[1][3][0]    =        0.11599;   TOF_dphi_mean[1][3][1]    =        8.58712;   TOF_dphi_mean[1][3][2]    =    3.07537e-05; 

  // EMC delta phi mean
  EMC_dphi_mean[0][0][0][0] =     0.00107915;   EMC_dphi_mean[0][0][0][1] =     0.00227667;   EMC_dphi_mean[0][0][0][2] =   -0.000735097; 
  EMC_dphi_mean[0][1][0][0] =     0.00101577;   EMC_dphi_mean[0][1][0][1] =    -0.00266386;   EMC_dphi_mean[0][1][0][2] =    0.000718267; 
  EMC_dphi_mean[0][0][1][0] =      0.0064635;   EMC_dphi_mean[0][0][1][1] =        1.41534;   EMC_dphi_mean[0][0][1][2] =     0.00257525; 
  EMC_dphi_mean[0][1][1][0] =     -0.0087097;   EMC_dphi_mean[0][1][1][1] =        1.58182;   EMC_dphi_mean[0][1][1][2] =    0.000227303; 
  EMC_dphi_mean[0][0][2][0] =     0.00642884;   EMC_dphi_mean[0][0][2][1] =         1.3467;   EMC_dphi_mean[0][0][2][2] =     0.00271648; 
  EMC_dphi_mean[0][1][2][0] =    -0.00728982;   EMC_dphi_mean[0][1][2][1] =        1.39612;   EMC_dphi_mean[0][1][2][2] =    0.000456605; 
  EMC_dphi_mean[0][0][3][0] =     0.00199357;   EMC_dphi_mean[0][0][3][1] =     0.00260444;   EMC_dphi_mean[0][0][3][2] =   -0.000930409; 
  EMC_dphi_mean[0][1][3][0] =     0.00226464;   EMC_dphi_mean[0][1][3][1] =    -0.00294236;   EMC_dphi_mean[0][1][3][2] =    0.000974586; 
  EMC_dphi_mean[1][0][0][0] =   -0.000578748;   EMC_dphi_mean[1][0][0][1] =     0.00300048;   EMC_dphi_mean[1][0][0][2] =   -0.000974659; 
  EMC_dphi_mean[1][1][0][0] =   -0.000725675;   EMC_dphi_mean[1][1][0][1] =    -0.00289081;   EMC_dphi_mean[1][1][0][2] =    0.000931181; 
  EMC_dphi_mean[1][0][1][0] =     0.00722348;   EMC_dphi_mean[1][0][1][1] =        1.33629;   EMC_dphi_mean[1][0][1][2] =    0.000771852; 
  EMC_dphi_mean[1][1][1][0] =    -0.00742968;   EMC_dphi_mean[1][1][1][1] =         1.4648;   EMC_dphi_mean[1][1][1][2] =    -0.00192537; 
  EMC_dphi_mean[1][0][2][0] =     0.00733508;   EMC_dphi_mean[1][0][2][1] =        1.37523;   EMC_dphi_mean[1][0][2][2] =    0.000600598; 
  EMC_dphi_mean[1][1][2][0] =      -0.007441;   EMC_dphi_mean[1][1][2][1] =        1.45153;   EMC_dphi_mean[1][1][2][2] =    -0.00209352; 
  EMC_dphi_mean[1][0][3][0] =    -0.00106299;   EMC_dphi_mean[1][0][3][1] =      0.0032645;   EMC_dphi_mean[1][0][3][2] =    -0.00109748; 
  EMC_dphi_mean[1][1][3][0] =    -0.00113193;   EMC_dphi_mean[1][1][3][1] =    -0.00254345;   EMC_dphi_mean[1][1][3][2] =    0.000808225; 

  //----------------------------------------------------------------------------
  // Delta phi sigma
  //----------------------------------------------------------------------------
  // PC2 delta phi sigma
  PC2_dphi_sigma[0]    =       0.456163;   PC2_dphi_sigma[1]    =       0.492848;   PC2_dphi_sigma[2]    =            419; 

  // PC3 delta phi sigma
  PC3_dphi_sigma[0][0] =       0.689732;   PC3_dphi_sigma[0][1] =       0.580264;   PC3_dphi_sigma[0][2] =            492; 
  PC3_dphi_sigma[1][0] =       0.726417;   PC3_dphi_sigma[1][1] =       0.621423;   PC3_dphi_sigma[1][2] =            492; 

  // TOF delta phi sigma
  TOF_dphi_sigma[0]    =       0.814785;   TOF_dphi_sigma[1]    =        1.92927;   TOF_dphi_sigma[2]    =            507; 

  // EMC delta phi sigma
  EMC_dphi_sigma[0][0] =       0.819293;   EMC_dphi_sigma[0][1] =         2.1512;   EMC_dphi_sigma[0][2] =            507; 
  EMC_dphi_sigma[1][0] =       0.841551;   EMC_dphi_sigma[1][1] =           2.22;   EMC_dphi_sigma[1][2] =            507; 


  //----------------------------------------------------------------------------
  // Delta z mean
  //----------------------------------------------------------------------------
  // PC2 delta z mean
  PC2_dz_mean_lowpt[0][0][0]     =        1.49451;   PC2_dz_mean_lowpt[0][0][1]     =        4.04796; 
  PC2_dz_mean_lowpt[0][1][0]     =       0.337169;   PC2_dz_mean_lowpt[0][1][1]     =          3.291; 
  PC2_dz_mean_lowpt[0][2][0]     =      -0.514687;   PC2_dz_mean_lowpt[0][2][1]     =        2.80814; 
  PC2_dz_mean_lowpt[0][3][0]     =       -1.09418;   PC2_dz_mean_lowpt[0][3][1]     =        3.26326; 

  PC2_dz_mean_lowpt[1][0][0]     =        1.46218;   PC2_dz_mean_lowpt[1][0][1]     =        5.96386; 
  PC2_dz_mean_lowpt[1][1][0]     =        0.11828;   PC2_dz_mean_lowpt[1][1][1]     =        1.84148; 
  PC2_dz_mean_lowpt[1][2][0]     =     -0.0921154;   PC2_dz_mean_lowpt[1][2][1]     =        1.87342; 
  PC2_dz_mean_lowpt[1][3][0]     =       -2.16223;   PC2_dz_mean_lowpt[1][3][1]     =        7.05066; 

  PC2_dz_mean_offset_lowpt[0][0][0]    =        0.10156;         PC2_dz_mean_offset_lowpt[1][0][0]    =      0.0567609; 
  PC2_dz_mean_offset_lowpt[0][0][1]    =       0.123126;         PC2_dz_mean_offset_lowpt[1][0][1]    =      0.0760758; 
  PC2_dz_mean_offset_lowpt[0][0][2]    =       0.145941;         PC2_dz_mean_offset_lowpt[1][0][2]    =       0.101339; 
  PC2_dz_mean_offset_lowpt[0][0][3]    =       0.174364;         PC2_dz_mean_offset_lowpt[1][0][3]    =       0.127718; 
  PC2_dz_mean_offset_lowpt[0][0][4]    =       0.207089;         PC2_dz_mean_offset_lowpt[1][0][4]    =       0.156927; 
  PC2_dz_mean_offset_lowpt[0][0][5]    =         0.2304;         PC2_dz_mean_offset_lowpt[1][0][5]    =       0.181335; 
  PC2_dz_mean_offset_lowpt[0][0][6]    =       0.247536;         PC2_dz_mean_offset_lowpt[1][0][6]    =       0.191854; 
  PC2_dz_mean_offset_lowpt[0][0][7]    =       0.245539;         PC2_dz_mean_offset_lowpt[1][0][7]    =       0.198409; 
  PC2_dz_mean_offset_lowpt[0][0][8]    =       0.245164;         PC2_dz_mean_offset_lowpt[1][0][8]    =       0.198712; 
  PC2_dz_mean_offset_lowpt[0][1][0]    =     -0.0970367;         PC2_dz_mean_offset_lowpt[1][1][0]    =       -0.10931; 
  PC2_dz_mean_offset_lowpt[0][1][1]    =      -0.076957;         PC2_dz_mean_offset_lowpt[1][1][1]    =     -0.0886938; 
  PC2_dz_mean_offset_lowpt[0][1][2]    =     -0.0528457;         PC2_dz_mean_offset_lowpt[1][1][2]    =     -0.0653352; 
  PC2_dz_mean_offset_lowpt[0][1][3]    =     -0.0241522;         PC2_dz_mean_offset_lowpt[1][1][3]    =     -0.0364878; 
  PC2_dz_mean_offset_lowpt[0][1][4]    =     0.00686981;         PC2_dz_mean_offset_lowpt[1][1][4]    =    -0.00599075; 
  PC2_dz_mean_offset_lowpt[0][1][5]    =      0.0300835;         PC2_dz_mean_offset_lowpt[1][1][5]    =      0.0188969; 
  PC2_dz_mean_offset_lowpt[0][1][6]    =      0.0467733;         PC2_dz_mean_offset_lowpt[1][1][6]    =       0.030537; 
  PC2_dz_mean_offset_lowpt[0][1][7]    =      0.0448156;         PC2_dz_mean_offset_lowpt[1][1][7]    =      0.0280462; 
  PC2_dz_mean_offset_lowpt[0][1][8]    =      0.0514111;         PC2_dz_mean_offset_lowpt[1][1][8]    =      0.0401815; 
  PC2_dz_mean_offset_lowpt[0][2][0]    =     -0.0277631;         PC2_dz_mean_offset_lowpt[1][2][0]    =      0.0405638; 
  PC2_dz_mean_offset_lowpt[0][2][1]    =    -0.00644712;         PC2_dz_mean_offset_lowpt[1][2][1]    =      0.0633248; 
  PC2_dz_mean_offset_lowpt[0][2][2]    =      0.0174009;         PC2_dz_mean_offset_lowpt[1][2][2]    =       0.086198; 
  PC2_dz_mean_offset_lowpt[0][2][3]    =      0.0442947;         PC2_dz_mean_offset_lowpt[1][2][3]    =       0.111845; 
  PC2_dz_mean_offset_lowpt[0][2][4]    =      0.0736452;         PC2_dz_mean_offset_lowpt[1][2][4]    =       0.141034; 
  PC2_dz_mean_offset_lowpt[0][2][5]    =      0.0988135;         PC2_dz_mean_offset_lowpt[1][2][5]    =       0.164935; 
  PC2_dz_mean_offset_lowpt[0][2][6]    =       0.104841;         PC2_dz_mean_offset_lowpt[1][2][6]    =       0.176412; 
  PC2_dz_mean_offset_lowpt[0][2][7]    =       0.104891;         PC2_dz_mean_offset_lowpt[1][2][7]    =       0.180417; 
  PC2_dz_mean_offset_lowpt[0][2][8]    =       0.106293;         PC2_dz_mean_offset_lowpt[1][2][8]    =       0.169574; 
  PC2_dz_mean_offset_lowpt[0][3][0]    =       -0.13963;         PC2_dz_mean_offset_lowpt[1][3][0]    =     -0.0366267; 
  PC2_dz_mean_offset_lowpt[0][3][1]    =      -0.116341;         PC2_dz_mean_offset_lowpt[1][3][1]    =     -0.0142185; 
  PC2_dz_mean_offset_lowpt[0][3][2]    =     -0.0937184;         PC2_dz_mean_offset_lowpt[1][3][2]    =      0.0089962; 
  PC2_dz_mean_offset_lowpt[0][3][3]    =     -0.0661715;         PC2_dz_mean_offset_lowpt[1][3][3]    =      0.0360692; 
  PC2_dz_mean_offset_lowpt[0][3][4]    =     -0.0360998;         PC2_dz_mean_offset_lowpt[1][3][4]    =      0.0667512; 
  PC2_dz_mean_offset_lowpt[0][3][5]    =     -0.0136426;         PC2_dz_mean_offset_lowpt[1][3][5]    =      0.0906405; 
  PC2_dz_mean_offset_lowpt[0][3][6]    =     0.00267248;         PC2_dz_mean_offset_lowpt[1][3][6]    =       0.103046; 
  PC2_dz_mean_offset_lowpt[0][3][7]    =     0.00473474;         PC2_dz_mean_offset_lowpt[1][3][7]    =       0.100668; 
  PC2_dz_mean_offset_lowpt[0][3][8]    =     -0.0140234;         PC2_dz_mean_offset_lowpt[1][3][8]    =      0.0888812; 
  

  PC2_dz_mean_highpt[0][0][0]    =        3.40211;   PC2_dz_mean_highpt[0][0][1]    =        1.45925; 
  PC2_dz_mean_highpt[0][1][0]    =        1.24413;   PC2_dz_mean_highpt[0][1][1]    =        1.62314; 
  PC2_dz_mean_highpt[0][2][0]    =       -2.00786;   PC2_dz_mean_highpt[0][2][1]    =        1.88085; 
  PC2_dz_mean_highpt[0][3][0]    =       -3.46346;   PC2_dz_mean_highpt[0][3][1]    =        1.46431; 

  PC2_dz_mean_highpt[1][0][0]    =        3.38239;   PC2_dz_mean_highpt[1][0][1]    =        1.54647; 
  PC2_dz_mean_highpt[1][1][0]    =        1.11438;   PC2_dz_mean_highpt[1][1][1]    =        1.62164; 
  PC2_dz_mean_highpt[1][2][0]    =       -1.10247;   PC2_dz_mean_highpt[1][2][1]    =        1.57363; 
  PC2_dz_mean_highpt[1][3][0]    =       -3.47059;   PC2_dz_mean_highpt[1][3][1]    =        1.56844; 

  PC2_dz_mean_offset_highpt[0][0][0]    =      -0.293299;      PC2_dz_mean_offset_highpt[1][0][0]    =      -0.292226; 
  PC2_dz_mean_offset_highpt[0][0][1]    =      -0.272745;      PC2_dz_mean_offset_highpt[1][0][1]    =      -0.272829; 
  PC2_dz_mean_offset_highpt[0][0][2]    =      -0.249089;      PC2_dz_mean_offset_highpt[1][0][2]    =      -0.251052; 
  PC2_dz_mean_offset_highpt[0][0][3]    =      -0.221577;      PC2_dz_mean_offset_highpt[1][0][3]    =      -0.224845; 
  PC2_dz_mean_offset_highpt[0][0][4]    =      -0.192283;      PC2_dz_mean_offset_highpt[1][0][4]    =      -0.198762; 
  PC2_dz_mean_offset_highpt[0][0][5]    =      -0.169138;      PC2_dz_mean_offset_highpt[1][0][5]    =      -0.175142; 
  PC2_dz_mean_offset_highpt[0][0][6]    =      -0.155733;      PC2_dz_mean_offset_highpt[1][0][6]    =      -0.156711; 
  PC2_dz_mean_offset_highpt[0][0][7]    =      -0.145453;      PC2_dz_mean_offset_highpt[1][0][7]    =      -0.158411; 
  PC2_dz_mean_offset_highpt[0][0][8]    =      -0.140046;      PC2_dz_mean_offset_highpt[1][0][8]    =      -0.128126; 
  PC2_dz_mean_offset_highpt[0][1][0]    =      -0.200641;      PC2_dz_mean_offset_highpt[1][1][0]    =       -0.19275; 
  PC2_dz_mean_offset_highpt[0][1][1]    =      -0.179553;      PC2_dz_mean_offset_highpt[1][1][1]    =      -0.171791; 
  PC2_dz_mean_offset_highpt[0][1][2]    =      -0.156968;      PC2_dz_mean_offset_highpt[1][1][2]    =      -0.151545; 
  PC2_dz_mean_offset_highpt[0][1][3]    =      -0.131582;      PC2_dz_mean_offset_highpt[1][1][3]    =      -0.124267; 
  PC2_dz_mean_offset_highpt[0][1][4]    =     -0.0978223;      PC2_dz_mean_offset_highpt[1][1][4]    =     -0.0952468; 
  PC2_dz_mean_offset_highpt[0][1][5]    =     -0.0756056;      PC2_dz_mean_offset_highpt[1][1][5]    =     -0.0688306; 
  PC2_dz_mean_offset_highpt[0][1][6]    =     -0.0615572;      PC2_dz_mean_offset_highpt[1][1][6]    =     -0.0566655; 
  PC2_dz_mean_offset_highpt[0][1][7]    =     -0.0519137;      PC2_dz_mean_offset_highpt[1][1][7]    =     -0.0591621; 
  PC2_dz_mean_offset_highpt[0][1][8]    =     -0.0511116;      PC2_dz_mean_offset_highpt[1][1][8]    =     -0.0134964; 
  PC2_dz_mean_offset_highpt[0][2][0]    =      0.0776101;      PC2_dz_mean_offset_highpt[1][2][0]    =       0.132885; 
  PC2_dz_mean_offset_highpt[0][2][1]    =      0.0998329;      PC2_dz_mean_offset_highpt[1][2][1]    =       0.153872; 
  PC2_dz_mean_offset_highpt[0][2][2]    =       0.123402;      PC2_dz_mean_offset_highpt[1][2][2]    =       0.177543; 
  PC2_dz_mean_offset_highpt[0][2][3]    =       0.148883;      PC2_dz_mean_offset_highpt[1][2][3]    =       0.202348; 
  PC2_dz_mean_offset_highpt[0][2][4]    =       0.179784;      PC2_dz_mean_offset_highpt[1][2][4]    =       0.231196; 
  PC2_dz_mean_offset_highpt[0][2][5]    =       0.204265;      PC2_dz_mean_offset_highpt[1][2][5]    =       0.250452; 
  PC2_dz_mean_offset_highpt[0][2][6]    =       0.209708;      PC2_dz_mean_offset_highpt[1][2][6]    =       0.257948; 
  PC2_dz_mean_offset_highpt[0][2][7]    =       0.225202;      PC2_dz_mean_offset_highpt[1][2][7]    =       0.263584; 
  PC2_dz_mean_offset_highpt[0][2][8]    =       0.211248;      PC2_dz_mean_offset_highpt[1][2][8]    =       0.285069; 
  PC2_dz_mean_offset_highpt[0][3][0]    =        0.25692;      PC2_dz_mean_offset_highpt[1][3][0]    =       0.310098; 
  PC2_dz_mean_offset_highpt[0][3][1]    =       0.277505;      PC2_dz_mean_offset_highpt[1][3][1]    =       0.332009; 
  PC2_dz_mean_offset_highpt[0][3][2]    =       0.302595;      PC2_dz_mean_offset_highpt[1][3][2]    =       0.353883; 
  PC2_dz_mean_offset_highpt[0][3][3]    =        0.33255;      PC2_dz_mean_offset_highpt[1][3][3]    =       0.380902; 
  PC2_dz_mean_offset_highpt[0][3][4]    =       0.359932;      PC2_dz_mean_offset_highpt[1][3][4]    =       0.411889; 
  PC2_dz_mean_offset_highpt[0][3][5]    =       0.385385;      PC2_dz_mean_offset_highpt[1][3][5]    =       0.439318; 
  PC2_dz_mean_offset_highpt[0][3][6]    =       0.400703;      PC2_dz_mean_offset_highpt[1][3][6]    =       0.453876; 
  PC2_dz_mean_offset_highpt[0][3][7]    =       0.397734;      PC2_dz_mean_offset_highpt[1][3][7]    =       0.453955; 
  PC2_dz_mean_offset_highpt[0][3][8]    =       0.413319;      PC2_dz_mean_offset_highpt[1][3][8]    =       0.447369; 



  // PC3 delta z mean
  // EAST
  PC3_dz_mean_lowpt[0][0][0][0]  =     -0.0157558;   PC3_dz_mean_lowpt[0][0][0][1]  =        1.80759; 
  PC3_dz_mean_lowpt[0][0][1][0]  =      -0.576858;   PC3_dz_mean_lowpt[0][0][1][1]  =        4.60654; 
  PC3_dz_mean_lowpt[0][0][2][0]  =       -1.38999;   PC3_dz_mean_lowpt[0][0][2][1]  =        3.95977; 
  PC3_dz_mean_lowpt[0][0][3][0]  =       -1.52469;   PC3_dz_mean_lowpt[0][0][3][1]  =        3.67692; 

  PC3_dz_mean_lowpt[0][1][0][0]  =        0.37958;   PC3_dz_mean_lowpt[0][1][0][1]  =        1.85725; 
  PC3_dz_mean_lowpt[0][1][1][0]  =      0.0634701;   PC3_dz_mean_lowpt[0][1][1][1]  =        1.83506; 
  PC3_dz_mean_lowpt[0][1][2][0]  =      -0.981141;   PC3_dz_mean_lowpt[0][1][2][1]  =        3.24184; 
  PC3_dz_mean_lowpt[0][1][3][0]  =       -1.47934;   PC3_dz_mean_lowpt[0][1][3][1]  =        3.11245; 

  PC3_dz_mean_offset_lowpt[0][0][0][0] =     -0.0451523;     PC3_dz_mean_offset_lowpt[0][1][0][0] =     0.00699509; 
  PC3_dz_mean_offset_lowpt[0][0][0][1] =     -0.0129364;     PC3_dz_mean_offset_lowpt[0][1][0][1] =      0.0358938; 
  PC3_dz_mean_offset_lowpt[0][0][0][2] =      0.0196935;     PC3_dz_mean_offset_lowpt[0][1][0][2] =      0.0688421; 
  PC3_dz_mean_offset_lowpt[0][0][0][3] =      0.0580194;     PC3_dz_mean_offset_lowpt[0][1][0][3] =       0.104709; 
  PC3_dz_mean_offset_lowpt[0][0][0][4] =       0.102971;     PC3_dz_mean_offset_lowpt[0][1][0][4] =        0.15063; 
  PC3_dz_mean_offset_lowpt[0][0][0][5] =        0.13867;     PC3_dz_mean_offset_lowpt[0][1][0][5] =       0.183511; 
  PC3_dz_mean_offset_lowpt[0][0][0][6] =       0.158348;     PC3_dz_mean_offset_lowpt[0][1][0][6] =       0.200861; 
  PC3_dz_mean_offset_lowpt[0][0][0][7] =       0.165466;     PC3_dz_mean_offset_lowpt[0][1][0][7] =       0.210713; 
  PC3_dz_mean_offset_lowpt[0][0][0][8] =       0.156458;     PC3_dz_mean_offset_lowpt[0][1][0][8] =       0.212617; 
  PC3_dz_mean_offset_lowpt[0][0][1][0] =     -0.0928666;     PC3_dz_mean_offset_lowpt[0][1][1][0] =      -0.044556; 
  PC3_dz_mean_offset_lowpt[0][0][1][1] =     -0.0620129;     PC3_dz_mean_offset_lowpt[0][1][1][1] =     -0.0135964; 
  PC3_dz_mean_offset_lowpt[0][0][1][2] =      -0.027894;     PC3_dz_mean_offset_lowpt[0][1][1][2] =      0.0212068; 
  PC3_dz_mean_offset_lowpt[0][0][1][3] =      0.0117534;     PC3_dz_mean_offset_lowpt[0][1][1][3] =      0.0613521; 
  PC3_dz_mean_offset_lowpt[0][0][1][4] =      0.0569034;     PC3_dz_mean_offset_lowpt[0][1][1][4] =       0.104421; 
  PC3_dz_mean_offset_lowpt[0][0][1][5] =      0.0945491;     PC3_dz_mean_offset_lowpt[0][1][1][5] =       0.140782; 
  PC3_dz_mean_offset_lowpt[0][0][1][6] =       0.112196;     PC3_dz_mean_offset_lowpt[0][1][1][6] =       0.161192; 
  PC3_dz_mean_offset_lowpt[0][0][1][7] =       0.113233;     PC3_dz_mean_offset_lowpt[0][1][1][7] =       0.161818; 
  PC3_dz_mean_offset_lowpt[0][0][1][8] =        0.11984;     PC3_dz_mean_offset_lowpt[0][1][1][8] =       0.150888; 
  PC3_dz_mean_offset_lowpt[0][0][2][0] =     -0.0766527;     PC3_dz_mean_offset_lowpt[0][1][2][0] =     -0.0720379; 
  PC3_dz_mean_offset_lowpt[0][0][2][1] =     -0.0444195;     PC3_dz_mean_offset_lowpt[0][1][2][1] =     -0.0410491; 
  PC3_dz_mean_offset_lowpt[0][0][2][2] =     -0.0116986;     PC3_dz_mean_offset_lowpt[0][1][2][2] =    -0.00833202; 
  PC3_dz_mean_offset_lowpt[0][0][2][3] =      0.0255425;     PC3_dz_mean_offset_lowpt[0][1][2][3] =      0.0297767; 
  PC3_dz_mean_offset_lowpt[0][0][2][4] =      0.0703213;     PC3_dz_mean_offset_lowpt[0][1][2][4] =      0.0726045; 
  PC3_dz_mean_offset_lowpt[0][0][2][5] =       0.102286;     PC3_dz_mean_offset_lowpt[0][1][2][5] =       0.104056; 
  PC3_dz_mean_offset_lowpt[0][0][2][6] =       0.118585;     PC3_dz_mean_offset_lowpt[0][1][2][6] =       0.130612; 
  PC3_dz_mean_offset_lowpt[0][0][2][7] =       0.126287;     PC3_dz_mean_offset_lowpt[0][1][2][7] =       0.120952; 
  PC3_dz_mean_offset_lowpt[0][0][2][8] =        0.11339;     PC3_dz_mean_offset_lowpt[0][1][2][8] =       0.121972; 
  PC3_dz_mean_offset_lowpt[0][0][3][0] =     0.00728742;     PC3_dz_mean_offset_lowpt[0][1][3][0] =       -0.01654; 
  PC3_dz_mean_offset_lowpt[0][0][3][1] =      0.0373084;     PC3_dz_mean_offset_lowpt[0][1][3][1] =      0.0162043; 
  PC3_dz_mean_offset_lowpt[0][0][3][2] =      0.0723178;     PC3_dz_mean_offset_lowpt[0][1][3][2] =      0.0501693; 
  PC3_dz_mean_offset_lowpt[0][0][3][3] =       0.110326;     PC3_dz_mean_offset_lowpt[0][1][3][3] =      0.0909067; 
  PC3_dz_mean_offset_lowpt[0][0][3][4] =       0.151517;     PC3_dz_mean_offset_lowpt[0][1][3][4] =       0.133747; 
  PC3_dz_mean_offset_lowpt[0][0][3][5] =       0.187278;     PC3_dz_mean_offset_lowpt[0][1][3][5] =       0.166885; 
  PC3_dz_mean_offset_lowpt[0][0][3][6] =       0.211856;     PC3_dz_mean_offset_lowpt[0][1][3][6] =       0.186374; 
  PC3_dz_mean_offset_lowpt[0][0][3][7] =       0.208697;     PC3_dz_mean_offset_lowpt[0][1][3][7] =       0.196929; 
  PC3_dz_mean_offset_lowpt[0][0][3][8] =        0.18099;     PC3_dz_mean_offset_lowpt[0][1][3][8] =       0.170474; 


  PC3_dz_mean_highpt[0][0][0][0] =        5.14501;   PC3_dz_mean_highpt[0][0][0][1] =        1.56499; 
  PC3_dz_mean_highpt[0][0][1][0] =        1.69854;   PC3_dz_mean_highpt[0][0][1][1] =        1.63629; 
  PC3_dz_mean_highpt[0][0][2][0] =       -2.76844;   PC3_dz_mean_highpt[0][0][2][1] =        1.87518; 
  PC3_dz_mean_highpt[0][0][3][0] =        -5.2312;   PC3_dz_mean_highpt[0][0][3][1] =        1.52074; 

  PC3_dz_mean_highpt[0][1][0][0] =        4.75984;   PC3_dz_mean_highpt[0][1][0][1] =        1.44739; 
  PC3_dz_mean_highpt[0][1][1][0] =        2.05652;   PC3_dz_mean_highpt[0][1][1][1] =        1.68931; 
  PC3_dz_mean_highpt[0][1][2][0] =       -1.55972;   PC3_dz_mean_highpt[0][1][2][1] =        1.41858; 
  PC3_dz_mean_highpt[0][1][3][0] =       -5.21379;   PC3_dz_mean_highpt[0][1][3][1] =         1.4972; 

  PC3_dz_mean_offset_highpt[0][0][0][0] =      -0.554909;     PC3_dz_mean_offset_highpt[0][1][0][0] =      -0.527933; 
  PC3_dz_mean_offset_highpt[0][0][0][1] =      -0.523075;     PC3_dz_mean_offset_highpt[0][1][0][1] =      -0.497031; 
  PC3_dz_mean_offset_highpt[0][0][0][2] =      -0.492317;     PC3_dz_mean_offset_highpt[0][1][0][2] =      -0.463971; 
  PC3_dz_mean_offset_highpt[0][0][0][3] =      -0.454837;     PC3_dz_mean_offset_highpt[0][1][0][3] =      -0.427446; 
  PC3_dz_mean_offset_highpt[0][0][0][4] =      -0.409706;     PC3_dz_mean_offset_highpt[0][1][0][4] =      -0.388716; 
  PC3_dz_mean_offset_highpt[0][0][0][5] =      -0.373963;     PC3_dz_mean_offset_highpt[0][1][0][5] =      -0.362418; 
  PC3_dz_mean_offset_highpt[0][0][0][6] =      -0.361322;     PC3_dz_mean_offset_highpt[0][1][0][6] =      -0.335143; 
  PC3_dz_mean_offset_highpt[0][0][0][7] =      -0.345144;     PC3_dz_mean_offset_highpt[0][1][0][7] =      -0.313638; 
  PC3_dz_mean_offset_highpt[0][0][0][8] =      -0.367775;     PC3_dz_mean_offset_highpt[0][1][0][8] =      -0.396893; 
  PC3_dz_mean_offset_highpt[0][0][1][0] =      -0.227808;     PC3_dz_mean_offset_highpt[0][1][1][0] =      -0.198843; 
  PC3_dz_mean_offset_highpt[0][0][1][1] =      -0.198138;     PC3_dz_mean_offset_highpt[0][1][1][1] =      -0.170889; 
  PC3_dz_mean_offset_highpt[0][0][1][2] =      -0.164493;     PC3_dz_mean_offset_highpt[0][1][1][2] =      -0.135487; 
  PC3_dz_mean_offset_highpt[0][0][1][3] =      -0.127191;     PC3_dz_mean_offset_highpt[0][1][1][3] =     -0.0980524; 
  PC3_dz_mean_offset_highpt[0][0][1][4] =     -0.0797096;     PC3_dz_mean_offset_highpt[0][1][1][4] =     -0.0584502; 
  PC3_dz_mean_offset_highpt[0][0][1][5] =     -0.0501193;     PC3_dz_mean_offset_highpt[0][1][1][5] =     -0.0187612; 
  PC3_dz_mean_offset_highpt[0][0][1][6] =     -0.0249427;     PC3_dz_mean_offset_highpt[0][1][1][6] =    -0.00141595; 
  PC3_dz_mean_offset_highpt[0][0][1][7] =     -0.0275427;     PC3_dz_mean_offset_highpt[0][1][1][7] =    -0.00657677; 
  PC3_dz_mean_offset_highpt[0][0][1][8] =     -0.0214352;     PC3_dz_mean_offset_highpt[0][1][1][8] =     -0.0141932; 
  PC3_dz_mean_offset_highpt[0][0][2][0] =      0.0847024;     PC3_dz_mean_offset_highpt[0][1][2][0] =       0.103051; 
  PC3_dz_mean_offset_highpt[0][0][2][1] =       0.114286;     PC3_dz_mean_offset_highpt[0][1][2][1] =        0.13235; 
  PC3_dz_mean_offset_highpt[0][0][2][2] =       0.149371;     PC3_dz_mean_offset_highpt[0][1][2][2] =       0.167663; 
  PC3_dz_mean_offset_highpt[0][0][2][3] =       0.187558;     PC3_dz_mean_offset_highpt[0][1][2][3] =       0.206943; 
  PC3_dz_mean_offset_highpt[0][0][2][4] =        0.22511;     PC3_dz_mean_offset_highpt[0][1][2][4] =       0.247065; 
  PC3_dz_mean_offset_highpt[0][0][2][5] =       0.267478;     PC3_dz_mean_offset_highpt[0][1][2][5] =       0.277672; 
  PC3_dz_mean_offset_highpt[0][0][2][6] =       0.286359;     PC3_dz_mean_offset_highpt[0][1][2][6] =       0.294513; 
  PC3_dz_mean_offset_highpt[0][0][2][7] =       0.276455;     PC3_dz_mean_offset_highpt[0][1][2][7] =       0.312913; 
  PC3_dz_mean_offset_highpt[0][0][2][8] =       0.295075;     PC3_dz_mean_offset_highpt[0][1][2][8] =       0.342816; 
  PC3_dz_mean_offset_highpt[0][0][3][0] =       0.559422;     PC3_dz_mean_offset_highpt[0][1][3][0] =       0.535743; 
  PC3_dz_mean_offset_highpt[0][0][3][1] =       0.587304;     PC3_dz_mean_offset_highpt[0][1][3][1] =       0.568334; 
  PC3_dz_mean_offset_highpt[0][0][3][2] =       0.618329;     PC3_dz_mean_offset_highpt[0][1][3][2] =       0.602364; 
  PC3_dz_mean_offset_highpt[0][0][3][3] =       0.660528;     PC3_dz_mean_offset_highpt[0][1][3][3] =       0.637724; 
  PC3_dz_mean_offset_highpt[0][0][3][4] =       0.704989;     PC3_dz_mean_offset_highpt[0][1][3][4] =       0.684043; 
  PC3_dz_mean_offset_highpt[0][0][3][5] =       0.724716;     PC3_dz_mean_offset_highpt[0][1][3][5] =       0.718067; 
  PC3_dz_mean_offset_highpt[0][0][3][6] =       0.758506;     PC3_dz_mean_offset_highpt[0][1][3][6] =       0.740657; 
  PC3_dz_mean_offset_highpt[0][0][3][7] =       0.771211;     PC3_dz_mean_offset_highpt[0][1][3][7] =       0.773197; 
  PC3_dz_mean_offset_highpt[0][0][3][8] =       0.768857;     PC3_dz_mean_offset_highpt[0][1][3][8] =       0.721375; 


  // WEST
  PC3_dz_mean_lowpt[1][0][0][0]  =        1.04926;   PC3_dz_mean_lowpt[1][0][0][1]  =        3.40314; 
  PC3_dz_mean_lowpt[1][0][1][0]  =       0.201643;   PC3_dz_mean_lowpt[1][0][1][1]  =        1.81502; 
  PC3_dz_mean_lowpt[1][0][2][0]  =      -0.717209;   PC3_dz_mean_lowpt[1][0][2][1]  =        3.37809; 
  PC3_dz_mean_lowpt[1][0][3][0]  =       -1.00091;   PC3_dz_mean_lowpt[1][0][3][1]  =        3.01922; 

  PC3_dz_mean_lowpt[1][1][0][0]  =       0.199097;   PC3_dz_mean_lowpt[1][1][0][1]  =         1.8484; 
  PC3_dz_mean_lowpt[1][1][1][0]  =       0.146906;   PC3_dz_mean_lowpt[1][1][1][1]  =        1.80997; 
  PC3_dz_mean_lowpt[1][1][2][0]  =      -0.094091;   PC3_dz_mean_lowpt[1][1][2][1]  =        1.82732; 
  PC3_dz_mean_lowpt[1][1][3][0]  =     -0.0839814;   PC3_dz_mean_lowpt[1][1][3][1]  =        1.78852; 

  PC3_dz_mean_offset_lowpt[1][0][0][0] =       0.116031;     PC3_dz_mean_offset_lowpt[1][1][0][0] =      0.0616055; 
  PC3_dz_mean_offset_lowpt[1][0][0][1] =        0.14696;     PC3_dz_mean_offset_lowpt[1][1][0][1] =      0.0911064; 
  PC3_dz_mean_offset_lowpt[1][0][0][2] =       0.180206;     PC3_dz_mean_offset_lowpt[1][1][0][2] =        0.12423; 
  PC3_dz_mean_offset_lowpt[1][0][0][3] =        0.21762;     PC3_dz_mean_offset_lowpt[1][1][0][3] =       0.161534; 
  PC3_dz_mean_offset_lowpt[1][0][0][4] =       0.261451;     PC3_dz_mean_offset_lowpt[1][1][0][4] =       0.203309; 
  PC3_dz_mean_offset_lowpt[1][0][0][5] =       0.297105;     PC3_dz_mean_offset_lowpt[1][1][0][5] =       0.237162; 
  PC3_dz_mean_offset_lowpt[1][0][0][6] =        0.32276;     PC3_dz_mean_offset_lowpt[1][1][0][6] =       0.252128; 
  PC3_dz_mean_offset_lowpt[1][0][0][7] =       0.316072;     PC3_dz_mean_offset_lowpt[1][1][0][7] =       0.249964; 
  PC3_dz_mean_offset_lowpt[1][0][0][8] =       0.327456;     PC3_dz_mean_offset_lowpt[1][1][0][8] =       0.255945; 
  PC3_dz_mean_offset_lowpt[1][0][1][0] =      -0.105233;     PC3_dz_mean_offset_lowpt[1][1][1][0] =     -0.0970452; 
  PC3_dz_mean_offset_lowpt[1][0][1][1] =     -0.0765686;     PC3_dz_mean_offset_lowpt[1][1][1][1] =     -0.0676679; 
  PC3_dz_mean_offset_lowpt[1][0][1][2] =     -0.0434101;     PC3_dz_mean_offset_lowpt[1][1][1][2] =     -0.0318559; 
  PC3_dz_mean_offset_lowpt[1][0][1][3] =    -0.00291021;     PC3_dz_mean_offset_lowpt[1][1][1][3] =      0.0065548; 
  PC3_dz_mean_offset_lowpt[1][0][1][4] =      0.0420906;     PC3_dz_mean_offset_lowpt[1][1][1][4] =      0.0487723; 
  PC3_dz_mean_offset_lowpt[1][0][1][5] =       0.075836;     PC3_dz_mean_offset_lowpt[1][1][1][5] =      0.0880721; 
  PC3_dz_mean_offset_lowpt[1][0][1][6] =       0.103653;     PC3_dz_mean_offset_lowpt[1][1][1][6] =       0.101251; 
  PC3_dz_mean_offset_lowpt[1][0][1][7] =       0.102298;     PC3_dz_mean_offset_lowpt[1][1][1][7] =       0.108867; 
  PC3_dz_mean_offset_lowpt[1][0][1][8] =       0.102853;     PC3_dz_mean_offset_lowpt[1][1][1][8] =       0.114153; 
  PC3_dz_mean_offset_lowpt[1][0][2][0] =     -0.0757313;     PC3_dz_mean_offset_lowpt[1][1][2][0] =    0.000615523; 
  PC3_dz_mean_offset_lowpt[1][0][2][1] =     -0.0457933;     PC3_dz_mean_offset_lowpt[1][1][2][1] =      0.0326097; 
  PC3_dz_mean_offset_lowpt[1][0][2][2] =     -0.0122803;     PC3_dz_mean_offset_lowpt[1][1][2][2] =       0.067477; 
  PC3_dz_mean_offset_lowpt[1][0][2][3] =      0.0268178;     PC3_dz_mean_offset_lowpt[1][1][2][3] =       0.103516; 
  PC3_dz_mean_offset_lowpt[1][0][2][4] =      0.0676277;     PC3_dz_mean_offset_lowpt[1][1][2][4] =       0.146092; 
  PC3_dz_mean_offset_lowpt[1][0][2][5] =        0.10302;     PC3_dz_mean_offset_lowpt[1][1][2][5] =        0.17974; 
  PC3_dz_mean_offset_lowpt[1][0][2][6] =       0.119813;     PC3_dz_mean_offset_lowpt[1][1][2][6] =       0.197382; 
  PC3_dz_mean_offset_lowpt[1][0][2][7] =       0.114101;     PC3_dz_mean_offset_lowpt[1][1][2][7] =       0.195369; 
  PC3_dz_mean_offset_lowpt[1][0][2][8] =       0.117108;     PC3_dz_mean_offset_lowpt[1][1][2][8] =       0.196506; 
  PC3_dz_mean_offset_lowpt[1][0][3][0] =      -0.196726;     PC3_dz_mean_offset_lowpt[1][1][3][0] =     -0.0728856; 
  PC3_dz_mean_offset_lowpt[1][0][3][1] =        -0.1632;     PC3_dz_mean_offset_lowpt[1][1][3][1] =     -0.0395225; 
  PC3_dz_mean_offset_lowpt[1][0][3][2] =      -0.131052;     PC3_dz_mean_offset_lowpt[1][1][3][2] =     -0.0054738; 
  PC3_dz_mean_offset_lowpt[1][0][3][3] =     -0.0887754;     PC3_dz_mean_offset_lowpt[1][1][3][3] =      0.0345562; 
  PC3_dz_mean_offset_lowpt[1][0][3][4] =     -0.0475715;     PC3_dz_mean_offset_lowpt[1][1][3][4] =      0.0795645; 
  PC3_dz_mean_offset_lowpt[1][0][3][5] =     -0.0105862;     PC3_dz_mean_offset_lowpt[1][1][3][5] =       0.112336; 
  PC3_dz_mean_offset_lowpt[1][0][3][6] =     0.00560808;     PC3_dz_mean_offset_lowpt[1][1][3][6] =       0.132657; 
  PC3_dz_mean_offset_lowpt[1][0][3][7] =      0.0165312;     PC3_dz_mean_offset_lowpt[1][1][3][7] =       0.120088; 
  PC3_dz_mean_offset_lowpt[1][0][3][8] =     0.00548378;     PC3_dz_mean_offset_lowpt[1][1][3][8] =        0.11922; 


  PC3_dz_mean_highpt[1][0][0][0] =        4.90529;   PC3_dz_mean_highpt[1][0][0][1] =        1.46727; 
  PC3_dz_mean_highpt[1][0][1][0] =        1.90615;   PC3_dz_mean_highpt[1][0][1][1] =        1.63714; 
  PC3_dz_mean_highpt[1][0][2][0] =       -2.00929;   PC3_dz_mean_highpt[1][0][2][1] =         1.6257; 
  PC3_dz_mean_highpt[1][0][3][0] =       -4.86707;   PC3_dz_mean_highpt[1][0][3][1] =         1.4532; 

  PC3_dz_mean_highpt[1][1][0][0] =        4.76059;   PC3_dz_mean_highpt[1][1][0][1] =        1.50043; 
  PC3_dz_mean_highpt[1][1][1][0] =        1.84373;   PC3_dz_mean_highpt[1][1][1][1] =        1.63985; 
  PC3_dz_mean_highpt[1][1][2][0] =       -1.65445;   PC3_dz_mean_highpt[1][1][2][1] =        1.58445; 
  PC3_dz_mean_highpt[1][1][3][0] =       -4.57329;   PC3_dz_mean_highpt[1][1][3][1] =        1.49662; 

  PC3_dz_mean_offset_highpt[1][0][0][0] =      -0.434817;     PC3_dz_mean_offset_highpt[1][1][0][0] =      -0.439596; 
  PC3_dz_mean_offset_highpt[1][0][0][1] =      -0.404428;     PC3_dz_mean_offset_highpt[1][1][0][1] =       -0.41276; 
  PC3_dz_mean_offset_highpt[1][0][0][2] =       -0.37222;     PC3_dz_mean_offset_highpt[1][1][0][2] =      -0.379588; 
  PC3_dz_mean_offset_highpt[1][0][0][3] =      -0.331802;     PC3_dz_mean_offset_highpt[1][1][0][3] =      -0.342523; 
  PC3_dz_mean_offset_highpt[1][0][0][4] =      -0.292539;     PC3_dz_mean_offset_highpt[1][1][0][4] =      -0.306191; 
  PC3_dz_mean_offset_highpt[1][0][0][5] =      -0.258268;     PC3_dz_mean_offset_highpt[1][1][0][5] =      -0.265564; 
  PC3_dz_mean_offset_highpt[1][0][0][6] =      -0.238294;     PC3_dz_mean_offset_highpt[1][1][0][6] =      -0.245073; 
  PC3_dz_mean_offset_highpt[1][0][0][7] =      -0.204702;     PC3_dz_mean_offset_highpt[1][1][0][7] =      -0.261727; 
  PC3_dz_mean_offset_highpt[1][0][0][8] =      -0.202992;     PC3_dz_mean_offset_highpt[1][1][0][8] =      -0.213004; 
  PC3_dz_mean_offset_highpt[1][0][1][0] =      -0.243388;     PC3_dz_mean_offset_highpt[1][1][1][0] =      -0.237279; 
  PC3_dz_mean_offset_highpt[1][0][1][1] =      -0.215534;     PC3_dz_mean_offset_highpt[1][1][1][1] =       -0.20491; 
  PC3_dz_mean_offset_highpt[1][0][1][2] =       -0.18295;     PC3_dz_mean_offset_highpt[1][1][1][2] =       -0.17671; 
  PC3_dz_mean_offset_highpt[1][0][1][3] =      -0.141587;     PC3_dz_mean_offset_highpt[1][1][1][3] =      -0.134939; 
  PC3_dz_mean_offset_highpt[1][0][1][4] =     -0.0955864;     PC3_dz_mean_offset_highpt[1][1][1][4] =     -0.0915169; 
  PC3_dz_mean_offset_highpt[1][0][1][5] =     -0.0645991;     PC3_dz_mean_offset_highpt[1][1][1][5] =     -0.0583052; 
  PC3_dz_mean_offset_highpt[1][0][1][6] =     -0.0551151;     PC3_dz_mean_offset_highpt[1][1][1][6] =     -0.0346843; 
  PC3_dz_mean_offset_highpt[1][0][1][7] =     -0.0427348;     PC3_dz_mean_offset_highpt[1][1][1][7] =     -0.0320057; 
  PC3_dz_mean_offset_highpt[1][0][1][8] =     -0.0512441;     PC3_dz_mean_offset_highpt[1][1][1][8] =     -0.0147996; 
  PC3_dz_mean_offset_highpt[1][0][2][0] =      0.0892798;     PC3_dz_mean_offset_highpt[1][1][2][0] =        0.13773; 
  PC3_dz_mean_offset_highpt[1][0][2][1] =       0.120959;     PC3_dz_mean_offset_highpt[1][1][2][1] =        0.17011; 
  PC3_dz_mean_offset_highpt[1][0][2][2] =       0.152394;     PC3_dz_mean_offset_highpt[1][1][2][2] =       0.203799; 
  PC3_dz_mean_offset_highpt[1][0][2][3] =       0.188791;     PC3_dz_mean_offset_highpt[1][1][2][3] =       0.241644; 
  PC3_dz_mean_offset_highpt[1][0][2][4] =       0.235213;     PC3_dz_mean_offset_highpt[1][1][2][4] =       0.277581; 
  PC3_dz_mean_offset_highpt[1][0][2][5] =       0.270819;     PC3_dz_mean_offset_highpt[1][1][2][5] =       0.313808; 
  PC3_dz_mean_offset_highpt[1][0][2][6] =       0.289448;     PC3_dz_mean_offset_highpt[1][1][2][6] =       0.319821; 
  PC3_dz_mean_offset_highpt[1][0][2][7] =       0.287532;     PC3_dz_mean_offset_highpt[1][1][2][7] =       0.325245; 
  PC3_dz_mean_offset_highpt[1][0][2][8] =       0.247885;     PC3_dz_mean_offset_highpt[1][1][2][8] =       0.339347; 
  PC3_dz_mean_offset_highpt[1][0][3][0] =        0.36519;     PC3_dz_mean_offset_highpt[1][1][3][0] =       0.420578; 
  PC3_dz_mean_offset_highpt[1][0][3][1] =       0.394429;     PC3_dz_mean_offset_highpt[1][1][3][1] =       0.452182; 
  PC3_dz_mean_offset_highpt[1][0][3][2] =       0.427773;     PC3_dz_mean_offset_highpt[1][1][3][2] =       0.488343; 
  PC3_dz_mean_offset_highpt[1][0][3][3] =       0.471806;     PC3_dz_mean_offset_highpt[1][1][3][3] =       0.527696; 
  PC3_dz_mean_offset_highpt[1][0][3][4] =       0.515966;     PC3_dz_mean_offset_highpt[1][1][3][4] =       0.564591; 
  PC3_dz_mean_offset_highpt[1][0][3][5] =       0.548404;     PC3_dz_mean_offset_highpt[1][1][3][5] =       0.607727; 
  PC3_dz_mean_offset_highpt[1][0][3][6] =       0.568056;     PC3_dz_mean_offset_highpt[1][1][3][6] =       0.629321; 
  PC3_dz_mean_offset_highpt[1][0][3][7] =       0.577218;     PC3_dz_mean_offset_highpt[1][1][3][7] =       0.627136; 
  PC3_dz_mean_offset_highpt[1][0][3][8] =       0.547012;     PC3_dz_mean_offset_highpt[1][1][3][8] =       0.588527; 


  // TOF delta z mean
  TOF_dz_mean_lowpt[0][0][0]     =      0.0780612;   TOF_dz_mean_lowpt[0][0][1]     =        2.07345; 
  TOF_dz_mean_lowpt[0][1][0]     =      -0.818484;   TOF_dz_mean_lowpt[0][1][1]     =        4.84855; 
  TOF_dz_mean_lowpt[0][2][0]     =      -0.977739;   TOF_dz_mean_lowpt[0][2][1]     =        3.77048; 
  TOF_dz_mean_lowpt[0][3][0]     =       -1.57238;   TOF_dz_mean_lowpt[0][3][1]     =        2.86559; 

  TOF_dz_mean_lowpt[1][0][0]     =       0.481964;   TOF_dz_mean_lowpt[1][0][1]     =         1.1726; 
  TOF_dz_mean_lowpt[1][1][0]     =      -0.016459;   TOF_dz_mean_lowpt[1][1][1]     =        1.90886; 
  TOF_dz_mean_lowpt[1][2][0]     =       -1.49833;   TOF_dz_mean_lowpt[1][2][1]     =        3.29169; 
  TOF_dz_mean_lowpt[1][3][0]     =       -1.56081;   TOF_dz_mean_lowpt[1][3][1]     =        2.48784; 

  TOF_dz_mean_offset_lowpt[0][0][0]    =       0.464201;     TOF_dz_mean_offset_lowpt[1][0][0]    =       0.472051; 
  TOF_dz_mean_offset_lowpt[0][0][1]    =        0.50022;     TOF_dz_mean_offset_lowpt[1][0][1]    =       0.507054; 
  TOF_dz_mean_offset_lowpt[0][0][2]    =       0.536651;     TOF_dz_mean_offset_lowpt[1][0][2]    =       0.543988; 
  TOF_dz_mean_offset_lowpt[0][0][3]    =       0.571695;     TOF_dz_mean_offset_lowpt[1][0][3]    =       0.586182; 
  TOF_dz_mean_offset_lowpt[0][0][4]    =       0.616706;     TOF_dz_mean_offset_lowpt[1][0][4]    =       0.635205; 
  TOF_dz_mean_offset_lowpt[0][0][5]    =       0.658011;     TOF_dz_mean_offset_lowpt[1][0][5]    =       0.663673; 
  TOF_dz_mean_offset_lowpt[0][0][6]    =       0.664407;     TOF_dz_mean_offset_lowpt[1][0][6]    =       0.703987; 
  TOF_dz_mean_offset_lowpt[0][0][7]    =       0.706654;     TOF_dz_mean_offset_lowpt[1][0][7]    =       0.704539; 
  TOF_dz_mean_offset_lowpt[0][0][8]    =       0.687589;     TOF_dz_mean_offset_lowpt[1][0][8]    =       0.728334; 
  TOF_dz_mean_offset_lowpt[0][1][0]    =      0.0700461;     TOF_dz_mean_offset_lowpt[1][1][0]    =      0.0977926; 
  TOF_dz_mean_offset_lowpt[0][1][1]    =       0.100396;     TOF_dz_mean_offset_lowpt[1][1][1]    =       0.130358; 
  TOF_dz_mean_offset_lowpt[0][1][2]    =        0.13251;     TOF_dz_mean_offset_lowpt[1][1][2]    =       0.161459; 
  TOF_dz_mean_offset_lowpt[0][1][3]    =       0.169509;     TOF_dz_mean_offset_lowpt[1][1][3]    =        0.20331; 
  TOF_dz_mean_offset_lowpt[0][1][4]    =       0.214216;     TOF_dz_mean_offset_lowpt[1][1][4]    =       0.246942; 
  TOF_dz_mean_offset_lowpt[0][1][5]    =       0.252381;     TOF_dz_mean_offset_lowpt[1][1][5]    =       0.274376; 
  TOF_dz_mean_offset_lowpt[0][1][6]    =        0.25593;     TOF_dz_mean_offset_lowpt[1][1][6]    =        0.29243; 
  TOF_dz_mean_offset_lowpt[0][1][7]    =        0.25975;     TOF_dz_mean_offset_lowpt[1][1][7]    =       0.298364; 
  TOF_dz_mean_offset_lowpt[0][1][8]    =       0.286846;     TOF_dz_mean_offset_lowpt[1][1][8]    =       0.251534; 
  TOF_dz_mean_offset_lowpt[0][2][0]    =      -0.203396;     TOF_dz_mean_offset_lowpt[1][2][0]    =      -0.283923; 
  TOF_dz_mean_offset_lowpt[0][2][1]    =      -0.172807;     TOF_dz_mean_offset_lowpt[1][2][1]    =      -0.255464; 
  TOF_dz_mean_offset_lowpt[0][2][2]    =      -0.140891;     TOF_dz_mean_offset_lowpt[1][2][2]    =      -0.221124; 
  TOF_dz_mean_offset_lowpt[0][2][3]    =     -0.0948605;     TOF_dz_mean_offset_lowpt[1][2][3]    =       -0.17647; 
  TOF_dz_mean_offset_lowpt[0][2][4]    =     -0.0493975;     TOF_dz_mean_offset_lowpt[1][2][4]    =      -0.138293; 
  TOF_dz_mean_offset_lowpt[0][2][5]    =     -0.0105725;     TOF_dz_mean_offset_lowpt[1][2][5]    =      -0.105339; 
  TOF_dz_mean_offset_lowpt[0][2][6]    =   -0.000863563;     TOF_dz_mean_offset_lowpt[1][2][6]    =     -0.0741118; 
  TOF_dz_mean_offset_lowpt[0][2][7]    =      0.0156144;     TOF_dz_mean_offset_lowpt[1][2][7]    =     -0.0818602; 
  TOF_dz_mean_offset_lowpt[0][2][8]    =     -0.0174081;     TOF_dz_mean_offset_lowpt[1][2][8]    =     -0.0980278; 
  TOF_dz_mean_offset_lowpt[0][3][0]    =      -0.426552;     TOF_dz_mean_offset_lowpt[1][3][0]    =      -0.489683; 
  TOF_dz_mean_offset_lowpt[0][3][1]    =      -0.401459;     TOF_dz_mean_offset_lowpt[1][3][1]    =      -0.465724; 
  TOF_dz_mean_offset_lowpt[0][3][2]    =      -0.370356;     TOF_dz_mean_offset_lowpt[1][3][2]    =        -0.4337; 
  TOF_dz_mean_offset_lowpt[0][3][3]    =      -0.331255;     TOF_dz_mean_offset_lowpt[1][3][3]    =      -0.396643; 
  TOF_dz_mean_offset_lowpt[0][3][4]    =      -0.298981;     TOF_dz_mean_offset_lowpt[1][3][4]    =      -0.339601; 
  TOF_dz_mean_offset_lowpt[0][3][5]    =      -0.252248;     TOF_dz_mean_offset_lowpt[1][3][5]    =      -0.317551; 
  TOF_dz_mean_offset_lowpt[0][3][6]    =      -0.221637;     TOF_dz_mean_offset_lowpt[1][3][6]    =      -0.298009; 
  TOF_dz_mean_offset_lowpt[0][3][7]    =      -0.202949;     TOF_dz_mean_offset_lowpt[1][3][7]    =      -0.278395; 
  TOF_dz_mean_offset_lowpt[0][3][8]    =      -0.249677;     TOF_dz_mean_offset_lowpt[1][3][8]    =      -0.332437; 



  TOF_dz_mean_highpt[0][0][0]    =        4.29256;   TOF_dz_mean_highpt[0][0][1]    =        1.41899; 
  TOF_dz_mean_highpt[0][1][0]    =        1.59363;   TOF_dz_mean_highpt[0][1][1]    =        1.61298; 
  TOF_dz_mean_highpt[0][2][0]    =        -1.4895;   TOF_dz_mean_highpt[0][2][1]    =        1.59904; 
  TOF_dz_mean_highpt[0][3][0]    =       -4.85839;   TOF_dz_mean_highpt[0][3][1]    =        1.47659; 

  TOF_dz_mean_highpt[1][0][0]    =        4.22598;   TOF_dz_mean_highpt[1][0][1]    =        1.33371; 
  TOF_dz_mean_highpt[1][1][0]    =        1.50647;   TOF_dz_mean_highpt[1][1][1]    =        1.56729; 
  TOF_dz_mean_highpt[1][2][0]    =       -2.19051;   TOF_dz_mean_highpt[1][2][1]    =        1.67926; 
  TOF_dz_mean_highpt[1][3][0]    =       -4.79861;   TOF_dz_mean_highpt[1][3][1]    =        1.39658; 

  TOF_dz_mean_offset_highpt[0][0][0]    =     -0.0660269;     TOF_dz_mean_offset_highpt[1][0][0]    =     -0.0220871; 
  TOF_dz_mean_offset_highpt[0][0][1]    =      -0.028064;     TOF_dz_mean_offset_highpt[1][0][1]    =      0.0130955; 
  TOF_dz_mean_offset_highpt[0][0][2]    =     -0.0033748;     TOF_dz_mean_offset_highpt[1][0][2]    =      0.0388059; 
  TOF_dz_mean_offset_highpt[0][0][3]    =      0.0333147;     TOF_dz_mean_offset_highpt[1][0][3]    =      0.0801842; 
  TOF_dz_mean_offset_highpt[0][0][4]    =      0.0822883;     TOF_dz_mean_offset_highpt[1][0][4]    =       0.122802; 
  TOF_dz_mean_offset_highpt[0][0][5]    =       0.137722;     TOF_dz_mean_offset_highpt[1][0][5]    =       0.141682; 
  TOF_dz_mean_offset_highpt[0][0][6]    =      0.0793893;     TOF_dz_mean_offset_highpt[1][0][6]    =       0.207697; 
  TOF_dz_mean_offset_highpt[0][0][7]    =       0.120063;     TOF_dz_mean_offset_highpt[1][0][7]    =       0.299603; 
  TOF_dz_mean_offset_highpt[0][0][8]    =       0.197926;     TOF_dz_mean_offset_highpt[1][0][8]    =       0.106255; 
  TOF_dz_mean_offset_highpt[0][1][0]    =      -0.065236;     TOF_dz_mean_offset_highpt[1][1][0]    =     -0.0443663; 
  TOF_dz_mean_offset_highpt[0][1][1]    =     -0.0276864;     TOF_dz_mean_offset_highpt[1][1][1]    =     -0.0139988; 
  TOF_dz_mean_offset_highpt[0][1][2]    =     0.00142303;     TOF_dz_mean_offset_highpt[1][1][2]    =      0.0195085; 
  TOF_dz_mean_offset_highpt[0][1][3]    =       0.038014;     TOF_dz_mean_offset_highpt[1][1][3]    =      0.0476108; 
  TOF_dz_mean_offset_highpt[0][1][4]    =      0.0721757;     TOF_dz_mean_offset_highpt[1][1][4]    =      0.0976269; 
  TOF_dz_mean_offset_highpt[0][1][5]    =       0.114372;     TOF_dz_mean_offset_highpt[1][1][5]    =       0.109804; 
  TOF_dz_mean_offset_highpt[0][1][6]    =       0.154159;     TOF_dz_mean_offset_highpt[1][1][6]    =      0.0965395; 
  TOF_dz_mean_offset_highpt[0][1][7]    =       0.115781;     TOF_dz_mean_offset_highpt[1][1][7]    =       0.178978; 
  TOF_dz_mean_offset_highpt[0][1][8]    =        0.23478;     TOF_dz_mean_offset_highpt[1][1][8]    =      0.0242845; 
  TOF_dz_mean_offset_highpt[0][2][0]    =     -0.0798797;     TOF_dz_mean_offset_highpt[1][2][0]    =      -0.122322; 
  TOF_dz_mean_offset_highpt[0][2][1]    =     -0.0415312;     TOF_dz_mean_offset_highpt[1][2][1]    =     -0.0925317; 
  TOF_dz_mean_offset_highpt[0][2][2]    =    -0.00424062;     TOF_dz_mean_offset_highpt[1][2][2]    =     -0.0583115; 
  TOF_dz_mean_offset_highpt[0][2][3]    =      0.0293138;     TOF_dz_mean_offset_highpt[1][2][3]    =     -0.0182354; 
  TOF_dz_mean_offset_highpt[0][2][4]    =      0.0688336;     TOF_dz_mean_offset_highpt[1][2][4]    =      0.0229394; 
  TOF_dz_mean_offset_highpt[0][2][5]    =       0.093079;     TOF_dz_mean_offset_highpt[1][2][5]    =      0.0660982; 
  TOF_dz_mean_offset_highpt[0][2][6]    =       0.155247;     TOF_dz_mean_offset_highpt[1][2][6]    =      0.0630096; 
  TOF_dz_mean_offset_highpt[0][2][7]    =       0.121819;     TOF_dz_mean_offset_highpt[1][2][7]    =       0.100842; 
  TOF_dz_mean_offset_highpt[0][2][8]    =       0.191354;     TOF_dz_mean_offset_highpt[1][2][8]    =      0.0180959; 
  TOF_dz_mean_offset_highpt[0][3][0]    =      0.0913207;     TOF_dz_mean_offset_highpt[1][3][0]    =      0.0745472; 
  TOF_dz_mean_offset_highpt[0][3][1]    =       0.130467;     TOF_dz_mean_offset_highpt[1][3][1]    =      0.0936505; 
  TOF_dz_mean_offset_highpt[0][3][2]    =       0.155154;     TOF_dz_mean_offset_highpt[1][3][2]    =       0.127294; 
  TOF_dz_mean_offset_highpt[0][3][3]    =       0.200568;     TOF_dz_mean_offset_highpt[1][3][3]    =       0.164808; 
  TOF_dz_mean_offset_highpt[0][3][4]    =        0.25578;     TOF_dz_mean_offset_highpt[1][3][4]    =       0.220095; 
  TOF_dz_mean_offset_highpt[0][3][5]    =       0.253824;     TOF_dz_mean_offset_highpt[1][3][5]    =       0.223518; 
  TOF_dz_mean_offset_highpt[0][3][6]    =       0.279347;     TOF_dz_mean_offset_highpt[1][3][6]    =       0.311125; 
  TOF_dz_mean_offset_highpt[0][3][7]    =       0.312365;     TOF_dz_mean_offset_highpt[1][3][7]    =       0.274498; 
  TOF_dz_mean_offset_highpt[0][3][8]    =       0.398569;     TOF_dz_mean_offset_highpt[1][3][8]    =       0.234786; 


  // EMC delta z mean
  // EAST
  EMC_dz_mean_lowpt[0][0][0][0]  =        5.15974;   EMC_dz_mean_lowpt[0][0][0][1]  =        3.29167; 
  EMC_dz_mean_lowpt[0][0][1][0]  =        1.66626;   EMC_dz_mean_lowpt[0][0][1][1]  =        2.47508; 
  EMC_dz_mean_lowpt[0][0][2][0]  =       -3.65934;   EMC_dz_mean_lowpt[0][0][2][1]  =        3.05677; 
  EMC_dz_mean_lowpt[0][0][3][0]  =       -6.39953;   EMC_dz_mean_lowpt[0][0][3][1]  =        3.00301; 

  EMC_dz_mean_lowpt[0][1][0][0]  =         5.3453;   EMC_dz_mean_lowpt[0][1][0][1]  =        2.75579; 
  EMC_dz_mean_lowpt[0][1][1][0]  =        2.32917;   EMC_dz_mean_lowpt[0][1][1][1]  =          2.492; 
  EMC_dz_mean_lowpt[0][1][2][0]  =       -3.15473;   EMC_dz_mean_lowpt[0][1][2][1]  =        3.41915; 
  EMC_dz_mean_lowpt[0][1][3][0]  =       -6.03205;   EMC_dz_mean_lowpt[0][1][3][1]  =         3.1547; 

  EMC_dz_mean_offset_lowpt[0][0][0][0] =       -3.03819;     EMC_dz_mean_offset_lowpt[0][1][0][0] =       -3.08371; 
  EMC_dz_mean_offset_lowpt[0][0][0][1] =        -3.0494;     EMC_dz_mean_offset_lowpt[0][1][0][1] =       -3.08792; 
  EMC_dz_mean_offset_lowpt[0][0][0][2] =       -3.05187;     EMC_dz_mean_offset_lowpt[0][1][0][2] =       -3.08813; 
  EMC_dz_mean_offset_lowpt[0][0][0][3] =       -3.04383;     EMC_dz_mean_offset_lowpt[0][1][0][3] =       -3.06856; 
  EMC_dz_mean_offset_lowpt[0][0][0][4] =       -2.99665;     EMC_dz_mean_offset_lowpt[0][1][0][4] =       -3.04146; 
  EMC_dz_mean_offset_lowpt[0][0][0][5] =       -2.98386;     EMC_dz_mean_offset_lowpt[0][1][0][5] =       -3.02969; 
  EMC_dz_mean_offset_lowpt[0][0][0][6] =       -2.99518;     EMC_dz_mean_offset_lowpt[0][1][0][6] =       -3.04394; 
  EMC_dz_mean_offset_lowpt[0][0][0][7] =       -2.97368;     EMC_dz_mean_offset_lowpt[0][1][0][7] =       -3.02951; 
  EMC_dz_mean_offset_lowpt[0][0][0][8] =       -3.00752;     EMC_dz_mean_offset_lowpt[0][1][0][8] =       -3.06435; 
  EMC_dz_mean_offset_lowpt[0][0][1][0] =       -1.50278;     EMC_dz_mean_offset_lowpt[0][1][1][0] =       -1.44389; 
  EMC_dz_mean_offset_lowpt[0][0][1][1] =        -1.4727;     EMC_dz_mean_offset_lowpt[0][1][1][1] =       -1.41821; 
  EMC_dz_mean_offset_lowpt[0][0][1][2] =       -1.44582;     EMC_dz_mean_offset_lowpt[0][1][1][2] =       -1.37685; 
  EMC_dz_mean_offset_lowpt[0][0][1][3] =       -1.40744;     EMC_dz_mean_offset_lowpt[0][1][1][3] =       -1.34702; 
  EMC_dz_mean_offset_lowpt[0][0][1][4] =       -1.37447;     EMC_dz_mean_offset_lowpt[0][1][1][4] =       -1.31765; 
  EMC_dz_mean_offset_lowpt[0][0][1][5] =       -1.35384;     EMC_dz_mean_offset_lowpt[0][1][1][5] =       -1.29417; 
  EMC_dz_mean_offset_lowpt[0][0][1][6] =       -1.34606;     EMC_dz_mean_offset_lowpt[0][1][1][6] =       -1.28671; 
  EMC_dz_mean_offset_lowpt[0][0][1][7] =       -1.32902;     EMC_dz_mean_offset_lowpt[0][1][1][7] =       -1.31376; 
  EMC_dz_mean_offset_lowpt[0][0][1][8] =        -1.3658;     EMC_dz_mean_offset_lowpt[0][1][1][8] =       -1.35561; 
  EMC_dz_mean_offset_lowpt[0][0][2][0] =       0.624292;     EMC_dz_mean_offset_lowpt[0][1][2][0] =       0.724957; 
  EMC_dz_mean_offset_lowpt[0][0][2][1] =       0.644299;     EMC_dz_mean_offset_lowpt[0][1][2][1] =       0.740957; 
  EMC_dz_mean_offset_lowpt[0][0][2][2] =       0.664871;     EMC_dz_mean_offset_lowpt[0][1][2][2] =       0.769881; 
  EMC_dz_mean_offset_lowpt[0][0][2][3] =       0.689888;     EMC_dz_mean_offset_lowpt[0][1][2][3] =       0.788314; 
  EMC_dz_mean_offset_lowpt[0][0][2][4] =       0.721703;     EMC_dz_mean_offset_lowpt[0][1][2][4] =       0.816013; 
  EMC_dz_mean_offset_lowpt[0][0][2][5] =       0.748504;     EMC_dz_mean_offset_lowpt[0][1][2][5] =       0.836895; 
  EMC_dz_mean_offset_lowpt[0][0][2][6] =       0.753057;     EMC_dz_mean_offset_lowpt[0][1][2][6] =        0.84982; 
  EMC_dz_mean_offset_lowpt[0][0][2][7] =        0.75842;     EMC_dz_mean_offset_lowpt[0][1][2][7] =       0.826066; 
  EMC_dz_mean_offset_lowpt[0][0][2][8] =       0.734101;     EMC_dz_mean_offset_lowpt[0][1][2][8] =       0.825815; 
  EMC_dz_mean_offset_lowpt[0][0][3][0] =        2.36136;     EMC_dz_mean_offset_lowpt[0][1][3][0] =        2.53129; 
  EMC_dz_mean_offset_lowpt[0][0][3][1] =        2.41548;     EMC_dz_mean_offset_lowpt[0][1][3][1] =        2.58295; 
  EMC_dz_mean_offset_lowpt[0][0][3][2] =         2.4609;     EMC_dz_mean_offset_lowpt[0][1][3][2] =          2.623; 
  EMC_dz_mean_offset_lowpt[0][0][3][3] =        2.50146;     EMC_dz_mean_offset_lowpt[0][1][3][3] =        2.67034; 
  EMC_dz_mean_offset_lowpt[0][0][3][4] =        2.54607;     EMC_dz_mean_offset_lowpt[0][1][3][4] =        2.71033; 
  EMC_dz_mean_offset_lowpt[0][0][3][5] =        2.56347;     EMC_dz_mean_offset_lowpt[0][1][3][5] =         2.7367; 
  EMC_dz_mean_offset_lowpt[0][0][3][6] =        2.58198;     EMC_dz_mean_offset_lowpt[0][1][3][6] =         2.7451; 
  EMC_dz_mean_offset_lowpt[0][0][3][7] =        2.58805;     EMC_dz_mean_offset_lowpt[0][1][3][7] =        2.72629; 
  EMC_dz_mean_offset_lowpt[0][0][3][8] =        2.57981;     EMC_dz_mean_offset_lowpt[0][1][3][8] =        2.72758; 


  EMC_dz_mean_highpt[0][0][0][0] =        6.22219;   EMC_dz_mean_highpt[0][0][0][1] =        1.37058; 
  EMC_dz_mean_highpt[0][0][1][0] =        2.16654;   EMC_dz_mean_highpt[0][0][1][1] =        1.32334; 
  EMC_dz_mean_highpt[0][0][2][0] =       -1.79583;   EMC_dz_mean_highpt[0][0][2][1] =       0.918915; 
  EMC_dz_mean_highpt[0][0][3][0] =       -3.47373;   EMC_dz_mean_highpt[0][0][3][1] =       0.938806; 

  EMC_dz_mean_highpt[0][1][0][0] =        8.23977;   EMC_dz_mean_highpt[0][1][0][1] =        1.48038; 
  EMC_dz_mean_highpt[0][1][1][0] =        1.39572;   EMC_dz_mean_highpt[0][1][1][1] =       0.759474; 
  EMC_dz_mean_highpt[0][1][2][0] =        -1.6594;   EMC_dz_mean_highpt[0][1][2][1] =        1.16143; 
  EMC_dz_mean_highpt[0][1][3][0] =       -9.48849;   EMC_dz_mean_highpt[0][1][3][1] =        1.63529; 

  EMC_dz_mean_offset_highpt[0][0][0][0] =       -3.84102;     EMC_dz_mean_offset_highpt[0][1][0][0] =       -3.87046; 
  EMC_dz_mean_offset_highpt[0][0][0][1] =       -3.85101;     EMC_dz_mean_offset_highpt[0][1][0][1] =       -3.88408; 
  EMC_dz_mean_offset_highpt[0][0][0][2] =       -3.88418;     EMC_dz_mean_offset_highpt[0][1][0][2] =        -3.9433; 
  EMC_dz_mean_offset_highpt[0][0][0][3] =       -3.89065;     EMC_dz_mean_offset_highpt[0][1][0][3] =       -3.91308; 
  EMC_dz_mean_offset_highpt[0][0][0][4] =       -3.90061;     EMC_dz_mean_offset_highpt[0][1][0][4] =        -3.9238; 
  EMC_dz_mean_offset_highpt[0][0][0][5] =       -3.89146;     EMC_dz_mean_offset_highpt[0][1][0][5] =        -3.9275; 
  EMC_dz_mean_offset_highpt[0][0][0][6] =       -3.91497;     EMC_dz_mean_offset_highpt[0][1][0][6] =       -4.01747; 
  EMC_dz_mean_offset_highpt[0][0][0][7] =       -3.61817;     EMC_dz_mean_offset_highpt[0][1][0][7] =       -4.02127; 
  EMC_dz_mean_offset_highpt[0][0][0][8] =       -3.43449;     EMC_dz_mean_offset_highpt[0][1][0][8] =       -4.16044; 
  EMC_dz_mean_offset_highpt[0][0][1][0] =       -1.74643;     EMC_dz_mean_offset_highpt[0][1][1][0] =       -1.85032; 
  EMC_dz_mean_offset_highpt[0][0][1][1] =       -1.70965;     EMC_dz_mean_offset_highpt[0][1][1][1] =       -1.81165; 
  EMC_dz_mean_offset_highpt[0][0][1][2] =       -1.70168;     EMC_dz_mean_offset_highpt[0][1][1][2] =       -1.80843; 
  EMC_dz_mean_offset_highpt[0][0][1][3] =       -1.67624;     EMC_dz_mean_offset_highpt[0][1][1][3] =       -1.77813; 
  EMC_dz_mean_offset_highpt[0][0][1][4] =        -1.6227;     EMC_dz_mean_offset_highpt[0][1][1][4] =       -1.74898; 
  EMC_dz_mean_offset_highpt[0][0][1][5] =       -1.65315;     EMC_dz_mean_offset_highpt[0][1][1][5] =       -1.72601; 
  EMC_dz_mean_offset_highpt[0][0][1][6] =       -1.60982;     EMC_dz_mean_offset_highpt[0][1][1][6] =       -1.79393; 
  EMC_dz_mean_offset_highpt[0][0][1][7] =       -1.73414;     EMC_dz_mean_offset_highpt[0][1][1][7] =       -1.78237; 
  EMC_dz_mean_offset_highpt[0][0][1][8] =        -1.7499;     EMC_dz_mean_offset_highpt[0][1][1][8] =       -1.81849; 
  EMC_dz_mean_offset_highpt[0][0][2][0] =        1.05817;     EMC_dz_mean_offset_highpt[0][1][2][0] =        1.01059; 
  EMC_dz_mean_offset_highpt[0][0][2][1] =        1.09235;     EMC_dz_mean_offset_highpt[0][1][2][1] =        1.03876; 
  EMC_dz_mean_offset_highpt[0][0][2][2] =          1.108;     EMC_dz_mean_offset_highpt[0][1][2][2] =        1.05988; 
  EMC_dz_mean_offset_highpt[0][0][2][3] =        1.15821;     EMC_dz_mean_offset_highpt[0][1][2][3] =        1.09306; 
  EMC_dz_mean_offset_highpt[0][0][2][4] =        1.19123;     EMC_dz_mean_offset_highpt[0][1][2][4] =        1.15262; 
  EMC_dz_mean_offset_highpt[0][0][2][5] =        1.22581;     EMC_dz_mean_offset_highpt[0][1][2][5] =         1.1212; 
  EMC_dz_mean_offset_highpt[0][0][2][6] =        1.20508;     EMC_dz_mean_offset_highpt[0][1][2][6] =        1.16505; 
  EMC_dz_mean_offset_highpt[0][0][2][7] =         1.2992;     EMC_dz_mean_offset_highpt[0][1][2][7] =         1.1177; 
  EMC_dz_mean_offset_highpt[0][0][2][8] =        1.34665;     EMC_dz_mean_offset_highpt[0][1][2][8] =         1.1751; 
  EMC_dz_mean_offset_highpt[0][0][3][0] =        3.21774;     EMC_dz_mean_offset_highpt[0][1][3][0] =        3.27841; 
  EMC_dz_mean_offset_highpt[0][0][3][1] =         3.3055;     EMC_dz_mean_offset_highpt[0][1][3][1] =        3.37132; 
  EMC_dz_mean_offset_highpt[0][0][3][2] =        3.39111;     EMC_dz_mean_offset_highpt[0][1][3][2] =        3.41935; 
  EMC_dz_mean_offset_highpt[0][0][3][3] =        3.45425;     EMC_dz_mean_offset_highpt[0][1][3][3] =        3.43816; 
  EMC_dz_mean_offset_highpt[0][0][3][4] =        3.50691;     EMC_dz_mean_offset_highpt[0][1][3][4] =        3.54028; 
  EMC_dz_mean_offset_highpt[0][0][3][5] =        3.63111;     EMC_dz_mean_offset_highpt[0][1][3][5] =        3.53519; 
  EMC_dz_mean_offset_highpt[0][0][3][6] =         3.6198;     EMC_dz_mean_offset_highpt[0][1][3][6] =        3.59676; 
  EMC_dz_mean_offset_highpt[0][0][3][7] =        3.72937;     EMC_dz_mean_offset_highpt[0][1][3][7] =         3.5655; 
  EMC_dz_mean_offset_highpt[0][0][3][8] =        3.54221;     EMC_dz_mean_offset_highpt[0][1][3][8] =         3.7847; 



  // WEST
  EMC_dz_mean_lowpt[1][0][0][0]  =        5.90321;   EMC_dz_mean_lowpt[1][0][0][1]  =        3.01232; 
  EMC_dz_mean_lowpt[1][0][1][0]  =        2.66317;   EMC_dz_mean_lowpt[1][0][1][1]  =        2.82739; 
  EMC_dz_mean_lowpt[1][0][2][0]  =       -2.75973;   EMC_dz_mean_lowpt[1][0][2][1]  =        2.82887; 
  EMC_dz_mean_lowpt[1][0][3][0]  =       -5.86426;   EMC_dz_mean_lowpt[1][0][3][1]  =        3.04195; 

  EMC_dz_mean_lowpt[1][1][0][0]  =        5.22252;   EMC_dz_mean_lowpt[1][1][0][1]  =        2.85218; 
  EMC_dz_mean_lowpt[1][1][1][0]  =        2.47755;   EMC_dz_mean_lowpt[1][1][1][1]  =        2.76474; 
  EMC_dz_mean_lowpt[1][1][2][0]  =       -2.62399;   EMC_dz_mean_lowpt[1][1][2][1]  =        2.87644; 
  EMC_dz_mean_lowpt[1][1][3][0]  =        -5.8344;   EMC_dz_mean_lowpt[1][1][3][1]  =        3.12015; 

  EMC_dz_mean_offset_lowpt[1][0][0][0] =       -2.38983;     EMC_dz_mean_offset_lowpt[1][1][0][0] =       -2.45978; 
  EMC_dz_mean_offset_lowpt[1][0][0][1] =       -2.39002;     EMC_dz_mean_offset_lowpt[1][1][0][1] =       -2.46966; 
  EMC_dz_mean_offset_lowpt[1][0][0][2] =       -2.38087;     EMC_dz_mean_offset_lowpt[1][1][0][2] =       -2.46845; 
  EMC_dz_mean_offset_lowpt[1][0][0][3] =       -2.35993;     EMC_dz_mean_offset_lowpt[1][1][0][3] =       -2.44699; 
  EMC_dz_mean_offset_lowpt[1][0][0][4] =        -2.3313;     EMC_dz_mean_offset_lowpt[1][1][0][4] =       -2.41882; 
  EMC_dz_mean_offset_lowpt[1][0][0][5] =       -2.30556;     EMC_dz_mean_offset_lowpt[1][1][0][5] =       -2.40801; 
  EMC_dz_mean_offset_lowpt[1][0][0][6] =       -2.29954;     EMC_dz_mean_offset_lowpt[1][1][0][6] =        -2.4016; 
  EMC_dz_mean_offset_lowpt[1][0][0][7] =       -2.32927;     EMC_dz_mean_offset_lowpt[1][1][0][7] =       -2.40519; 
  EMC_dz_mean_offset_lowpt[1][0][0][8] =       -2.33312;     EMC_dz_mean_offset_lowpt[1][1][0][8] =       -2.43278; 
  EMC_dz_mean_offset_lowpt[1][0][1][0] =      -0.855994;     EMC_dz_mean_offset_lowpt[1][1][1][0] =      -0.866692; 
  EMC_dz_mean_offset_lowpt[1][0][1][1] =      -0.823965;     EMC_dz_mean_offset_lowpt[1][1][1][1] =      -0.838799; 
  EMC_dz_mean_offset_lowpt[1][0][1][2] =       -0.79208;     EMC_dz_mean_offset_lowpt[1][1][1][2] =      -0.804336; 
  EMC_dz_mean_offset_lowpt[1][0][1][3] =      -0.754449;     EMC_dz_mean_offset_lowpt[1][1][1][3] =      -0.771835; 
  EMC_dz_mean_offset_lowpt[1][0][1][4] =       -0.71833;     EMC_dz_mean_offset_lowpt[1][1][1][4] =      -0.732212; 
  EMC_dz_mean_offset_lowpt[1][0][1][5] =      -0.695842;     EMC_dz_mean_offset_lowpt[1][1][1][5] =      -0.707015; 
  EMC_dz_mean_offset_lowpt[1][0][1][6] =      -0.685273;     EMC_dz_mean_offset_lowpt[1][1][1][6] =      -0.700025; 
  EMC_dz_mean_offset_lowpt[1][0][1][7] =       -0.70465;     EMC_dz_mean_offset_lowpt[1][1][1][7] =       -0.71944; 
  EMC_dz_mean_offset_lowpt[1][0][1][8] =      -0.717994;     EMC_dz_mean_offset_lowpt[1][1][1][8] =       -0.71234; 
  EMC_dz_mean_offset_lowpt[1][0][2][0] =        1.19305;     EMC_dz_mean_offset_lowpt[1][1][2][0] =        1.26161; 
  EMC_dz_mean_offset_lowpt[1][0][2][1] =        1.21336;     EMC_dz_mean_offset_lowpt[1][1][2][1] =        1.28161; 
  EMC_dz_mean_offset_lowpt[1][0][2][2] =        1.24188;     EMC_dz_mean_offset_lowpt[1][1][2][2] =        1.30437; 
  EMC_dz_mean_offset_lowpt[1][0][2][3] =        1.26552;     EMC_dz_mean_offset_lowpt[1][1][2][3] =        1.32689; 
  EMC_dz_mean_offset_lowpt[1][0][2][4] =        1.29563;     EMC_dz_mean_offset_lowpt[1][1][2][4] =         1.3556; 
  EMC_dz_mean_offset_lowpt[1][0][2][5] =         1.3184;     EMC_dz_mean_offset_lowpt[1][1][2][5] =        1.38011; 
  EMC_dz_mean_offset_lowpt[1][0][2][6] =        1.31755;     EMC_dz_mean_offset_lowpt[1][1][2][6] =        1.38362; 
  EMC_dz_mean_offset_lowpt[1][0][2][7] =        1.29562;     EMC_dz_mean_offset_lowpt[1][1][2][7] =        1.36356; 
  EMC_dz_mean_offset_lowpt[1][0][2][8] =        1.31868;     EMC_dz_mean_offset_lowpt[1][1][2][8] =        1.33475; 
  EMC_dz_mean_offset_lowpt[1][0][3][0] =         2.7462;     EMC_dz_mean_offset_lowpt[1][1][3][0] =        2.91476; 
  EMC_dz_mean_offset_lowpt[1][0][3][1] =        2.80789;     EMC_dz_mean_offset_lowpt[1][1][3][1] =        2.96942; 
  EMC_dz_mean_offset_lowpt[1][0][3][2] =        2.85041;     EMC_dz_mean_offset_lowpt[1][1][3][2] =        3.02137; 
  EMC_dz_mean_offset_lowpt[1][0][3][3] =        2.90159;     EMC_dz_mean_offset_lowpt[1][1][3][3] =         3.0604; 
  EMC_dz_mean_offset_lowpt[1][0][3][4] =        2.92831;     EMC_dz_mean_offset_lowpt[1][1][3][4] =        3.10193; 
  EMC_dz_mean_offset_lowpt[1][0][3][5] =        2.96888;     EMC_dz_mean_offset_lowpt[1][1][3][5] =        3.12479; 
  EMC_dz_mean_offset_lowpt[1][0][3][6] =        2.96528;     EMC_dz_mean_offset_lowpt[1][1][3][6] =        3.13113; 
  EMC_dz_mean_offset_lowpt[1][0][3][7] =        2.93098;     EMC_dz_mean_offset_lowpt[1][1][3][7] =        3.10606; 
  EMC_dz_mean_offset_lowpt[1][0][3][8] =        2.93666;     EMC_dz_mean_offset_lowpt[1][1][3][8] =        3.09533; 


  EMC_dz_mean_highpt[1][0][0][0] =        7.35243;   EMC_dz_mean_highpt[1][0][0][1] =        1.38806; 
  EMC_dz_mean_highpt[1][0][1][0] =         1.6707;   EMC_dz_mean_highpt[1][0][1][1] =       0.912256; 
  EMC_dz_mean_highpt[1][0][2][0] =       -3.13527;   EMC_dz_mean_highpt[1][0][2][1] =        1.53857; 
  EMC_dz_mean_highpt[1][0][3][0] =       -6.50038;   EMC_dz_mean_highpt[1][0][3][1] =        1.30459; 

  EMC_dz_mean_highpt[1][1][0][0] =         5.8416;   EMC_dz_mean_highpt[1][1][0][1] =        1.24816; 
  EMC_dz_mean_highpt[1][1][1][0] =         2.7551;   EMC_dz_mean_highpt[1][1][1][1] =        1.41795; 
  EMC_dz_mean_highpt[1][1][2][0] =       -2.57638;   EMC_dz_mean_highpt[1][1][2][1] =          1.406; 
  EMC_dz_mean_highpt[1][1][3][0] =       -5.21517;   EMC_dz_mean_highpt[1][1][3][1] =        1.21459; 

  EMC_dz_mean_offset_highpt[1][0][0][0] =       -3.22881;     EMC_dz_mean_offset_highpt[1][1][0][0] =       -3.30744; 
  EMC_dz_mean_offset_highpt[1][0][0][1] =       -3.25019;     EMC_dz_mean_offset_highpt[1][1][0][1] =       -3.32766; 
  EMC_dz_mean_offset_highpt[1][0][0][2] =       -3.26021;     EMC_dz_mean_offset_highpt[1][1][0][2] =       -3.32831; 
  EMC_dz_mean_offset_highpt[1][0][0][3] =       -3.26052;     EMC_dz_mean_offset_highpt[1][1][0][3] =       -3.31185; 
  EMC_dz_mean_offset_highpt[1][0][0][4] =       -3.27458;     EMC_dz_mean_offset_highpt[1][1][0][4] =       -3.36289; 
  EMC_dz_mean_offset_highpt[1][0][0][5] =       -3.26852;     EMC_dz_mean_offset_highpt[1][1][0][5] =       -3.37753; 
  EMC_dz_mean_offset_highpt[1][0][0][6] =       -3.33048;     EMC_dz_mean_offset_highpt[1][1][0][6] =       -3.47231; 
  EMC_dz_mean_offset_highpt[1][0][0][7] =       -3.34597;     EMC_dz_mean_offset_highpt[1][1][0][7] =       -3.44322; 
  EMC_dz_mean_offset_highpt[1][0][0][8] =       -3.44114;     EMC_dz_mean_offset_highpt[1][1][0][8] =       -3.74157; 
  EMC_dz_mean_offset_highpt[1][0][1][0] =       -1.24854;     EMC_dz_mean_offset_highpt[1][1][1][0] =       -1.15532; 
  EMC_dz_mean_offset_highpt[1][0][1][1] =       -1.22393;     EMC_dz_mean_offset_highpt[1][1][1][1] =       -1.12496; 
  EMC_dz_mean_offset_highpt[1][0][1][2] =       -1.19026;     EMC_dz_mean_offset_highpt[1][1][1][2] =       -1.10316; 
  EMC_dz_mean_offset_highpt[1][0][1][3] =       -1.16696;     EMC_dz_mean_offset_highpt[1][1][1][3] =       -1.06311; 
  EMC_dz_mean_offset_highpt[1][0][1][4] =       -1.14509;     EMC_dz_mean_offset_highpt[1][1][1][4] =       -1.04121; 
  EMC_dz_mean_offset_highpt[1][0][1][5] =       -1.10692;     EMC_dz_mean_offset_highpt[1][1][1][5] =       -1.04521; 
  EMC_dz_mean_offset_highpt[1][0][1][6] =       -1.14244;     EMC_dz_mean_offset_highpt[1][1][1][6] =       -1.04992; 
  EMC_dz_mean_offset_highpt[1][0][1][7] =       -1.17458;     EMC_dz_mean_offset_highpt[1][1][1][7] =       -1.12403; 
  EMC_dz_mean_offset_highpt[1][0][1][8] =       -1.26247;     EMC_dz_mean_offset_highpt[1][1][1][8] =        -1.2278; 
  EMC_dz_mean_offset_highpt[1][0][2][0] =        1.46605;     EMC_dz_mean_offset_highpt[1][1][2][0] =        1.53899; 
  EMC_dz_mean_offset_highpt[1][0][2][1] =         1.4944;     EMC_dz_mean_offset_highpt[1][1][2][1] =        1.57365; 
  EMC_dz_mean_offset_highpt[1][0][2][2] =        1.52626;     EMC_dz_mean_offset_highpt[1][1][2][2] =        1.60915; 
  EMC_dz_mean_offset_highpt[1][0][2][3] =         1.5718;     EMC_dz_mean_offset_highpt[1][1][2][3] =        1.62206; 
  EMC_dz_mean_offset_highpt[1][0][2][4] =        1.59639;     EMC_dz_mean_offset_highpt[1][1][2][4] =        1.67507; 
  EMC_dz_mean_offset_highpt[1][0][2][5] =        1.62396;     EMC_dz_mean_offset_highpt[1][1][2][5] =        1.68264; 
  EMC_dz_mean_offset_highpt[1][0][2][6] =        1.60312;     EMC_dz_mean_offset_highpt[1][1][2][6] =        1.63832; 
  EMC_dz_mean_offset_highpt[1][0][2][7] =        1.53344;     EMC_dz_mean_offset_highpt[1][1][2][7] =        1.67916; 
  EMC_dz_mean_offset_highpt[1][0][2][8] =        1.47672;     EMC_dz_mean_offset_highpt[1][1][2][8] =        1.64878; 
  EMC_dz_mean_offset_highpt[1][0][3][0] =        3.63447;     EMC_dz_mean_offset_highpt[1][1][3][0] =        3.74038; 
  EMC_dz_mean_offset_highpt[1][0][3][1] =        3.69203;     EMC_dz_mean_offset_highpt[1][1][3][1] =        3.80368; 
  EMC_dz_mean_offset_highpt[1][0][3][2] =        3.75337;     EMC_dz_mean_offset_highpt[1][1][3][2] =        3.85656; 
  EMC_dz_mean_offset_highpt[1][0][3][3] =        3.79869;     EMC_dz_mean_offset_highpt[1][1][3][3] =        3.90658; 
  EMC_dz_mean_offset_highpt[1][0][3][4] =        3.88214;     EMC_dz_mean_offset_highpt[1][1][3][4] =        4.00088; 
  EMC_dz_mean_offset_highpt[1][0][3][5] =         3.9229;     EMC_dz_mean_offset_highpt[1][1][3][5] =        4.04783; 
  EMC_dz_mean_offset_highpt[1][0][3][6] =        3.93659;     EMC_dz_mean_offset_highpt[1][1][3][6] =        4.01804; 
  EMC_dz_mean_offset_highpt[1][0][3][7] =        3.93341;     EMC_dz_mean_offset_highpt[1][1][3][7] =         4.0044; 
  EMC_dz_mean_offset_highpt[1][0][3][8] =        4.02492;     EMC_dz_mean_offset_highpt[1][1][3][8] =        4.08637; 


  //----------------------------------------------------------------------------
  // Delta z sigma
  //----------------------------------------------------------------------------
  // PC2 delta z sigma
  // Slope parameter
  PC2_dz_sigma_slope[0]       =       0.439997;   PC2_dz_sigma_slope[1]       =        0.44095; 

  // Centrality dependent offset
  PC2_dz_sigma_offset[0]       =       0.476281; 
  PC2_dz_sigma_offset[1]       =    -0.00413134; 
  PC2_dz_sigma_offset[2]       =    0.000119297; 
  PC2_dz_sigma_offset[3]       =   -1.04366e-06; 
  PC2_dz_sigma_offset[4]       =    1.27828e-08; 

  // PC3 delta z sigma
  // EAST
  // Slope parameter
  PC3_dz_sigma_slope[0][0]    =       0.699192;   PC3_dz_sigma_slope[0][1]    =       0.701651; 

  // Centrality dependent offset
  PC3_dz_sigma_offset[0][0]    =       0.651351; 
  PC3_dz_sigma_offset[0][1]    =    -0.00524575; 
  PC3_dz_sigma_offset[0][2]    =     9.4638e-05; 
  PC3_dz_sigma_offset[0][3]    =    3.08621e-07; 
  PC3_dz_sigma_offset[0][4]    =    6.79686e-09; 

  // WEST
  // Slope parameter
  PC3_dz_sigma_slope[1][0]    =       0.722686;   PC3_dz_sigma_slope[1][1]    =       0.730872; 

  // Centrality dependent offset
  PC3_dz_sigma_offset[1][0]    =       0.606524; 
  PC3_dz_sigma_offset[1][1]    =    -0.00390433; 
  PC3_dz_sigma_offset[1][2]    =    4.45059e-05; 
  PC3_dz_sigma_offset[1][3]    =    1.16482e-06; 
  PC3_dz_sigma_offset[1][4]    =    1.45763e-09; 

  // TOF delta z sigma
  // Slope parameter
  TOF_dz_sigma_slope[0]       =       0.739392;   TOF_dz_sigma_slope[1]       =       0.760105; 

  // Centrality dependent offset
  TOF_dz_sigma_offset[0]       =       0.737521; 
  TOF_dz_sigma_offset[1]       =    -0.00564167; 
  TOF_dz_sigma_offset[2]       =    0.000123582; 
  TOF_dz_sigma_offset[3]       =   -2.92036e-07; 
  TOF_dz_sigma_offset[4]       =    9.45833e-09; 

  // EMC delta z sigma
  // EAST
  // Slope parameter
  EMC_dz_sigma_slope[0][0][0] =        0.76041;   EMC_dz_sigma_slope[0][1][0] =       0.819147; 

  // Centrality dependent offset
  EMC_dz_sigma_offset[0][0][0] =        2.36847; 
  EMC_dz_sigma_offset[0][0][1] =    -0.00879769; 
  EMC_dz_sigma_offset[0][0][2] =    7.41765e-05; 
  EMC_dz_sigma_offset[0][0][3] =              0; 
  EMC_dz_sigma_offset[0][0][4] =              0; 

  // Slope parameter
  EMC_dz_sigma_slope[0][0][1] =       0.849894;   EMC_dz_sigma_slope[0][1][1] =       0.863739; 

  // Centrality dependent offset
  EMC_dz_sigma_offset[0][1][0] =        2.84962; 
  EMC_dz_sigma_offset[0][1][1] =    -0.00449869; 
  EMC_dz_sigma_offset[0][1][2] =    3.53469e-05; 
  EMC_dz_sigma_offset[0][1][3] =              0; 
  EMC_dz_sigma_offset[0][1][4] =              0; 

  // WEST
  // Slope parameter
  EMC_dz_sigma_slope[1][0][0] =       0.782518;   EMC_dz_sigma_slope[1][1][0] =       0.827325; 

  // Centrality dependent offset
  EMC_dz_sigma_offset[1][0][0] =        2.33887; 
  EMC_dz_sigma_offset[1][0][1] =    -0.00920025; 
  EMC_dz_sigma_offset[1][0][2] =    8.55876e-05; 
  EMC_dz_sigma_offset[1][0][3] =              0; 
  EMC_dz_sigma_offset[1][0][4] =              0; 

  // Slope parameter
  EMC_dz_sigma_slope[1][0][1] =       0.877584;   EMC_dz_sigma_slope[1][1][1] =       0.916298; 

  // Centrality dependent offset
  EMC_dz_sigma_offset[1][1][0] =        2.85235; 
  EMC_dz_sigma_offset[1][1][1] =    -0.00527069; 
  EMC_dz_sigma_offset[1][1][2] =      5.028e-05; 
  EMC_dz_sigma_offset[1][1][3] =              0; 
  EMC_dz_sigma_offset[1][1][4] =              0; 


  //----------------------------------------------------------------------------
  // pT dependent scale factor for Delta phi/z sigma
  //----------------------------------------------------------------------------
  // PC2 delta phi sigma
  PC2_dphi_sigma_scale[0][0]    =        22.5499;   PC2_dphi_sigma_scale[0][1]    =        8.89396;   PC2_dphi_sigma_scale[0][2]    =        1.02443; 
  PC2_dphi_sigma_scale[1][0]    =        12.1211;   PC2_dphi_sigma_scale[1][1]    =        11.2692;   PC2_dphi_sigma_scale[1][2]    =       0.978992; 
  PC2_dphi_sigma_scale[2][0]    =        14.9354;   PC2_dphi_sigma_scale[2][1]    =        11.9944;   PC2_dphi_sigma_scale[2][2]    =       0.976258; 
  PC2_dphi_sigma_scale[3][0]    =        25.6613;   PC2_dphi_sigma_scale[3][1]    =         9.2898;   PC2_dphi_sigma_scale[3][2]    =        1.00183; 

  // PC3 delta phi sigma
  // EAST
  PC3_dphi_sigma_scale[0][0][0] =        18.0848;   PC3_dphi_sigma_scale[0][0][1] =        8.83611;   PC3_dphi_sigma_scale[0][0][2] =        1.02581; 
  PC3_dphi_sigma_scale[0][1][0] =         13.159;   PC3_dphi_sigma_scale[0][1][1] =        11.5191;   PC3_dphi_sigma_scale[0][1][2] =        1.00169; 
  PC3_dphi_sigma_scale[0][2][0] =        11.6669;   PC3_dphi_sigma_scale[0][2][1] =        10.9358;   PC3_dphi_sigma_scale[0][2][2] =       0.998894; 
  PC3_dphi_sigma_scale[0][3][0] =        19.7987;   PC3_dphi_sigma_scale[0][3][1] =        9.01307;   PC3_dphi_sigma_scale[0][3][2] =        1.01534; 

  // WEST
  PC3_dphi_sigma_scale[1][0][0] =        16.2728;   PC3_dphi_sigma_scale[1][0][1] =        8.81418;   PC3_dphi_sigma_scale[1][0][2] =        1.02152; 
  PC3_dphi_sigma_scale[1][1][0] =        10.0984;   PC3_dphi_sigma_scale[1][1][1] =        11.0433;   PC3_dphi_sigma_scale[1][1][2] =       0.995821; 
  PC3_dphi_sigma_scale[1][2][0] =        12.2288;   PC3_dphi_sigma_scale[1][2][1] =        11.6306;   PC3_dphi_sigma_scale[1][2][2] =        1.00463; 
  PC3_dphi_sigma_scale[1][3][0] =        18.8768;   PC3_dphi_sigma_scale[1][3][1] =        9.30321;   PC3_dphi_sigma_scale[1][3][2] =        1.02447; 

  // TOF delta phi sigma
  TOF_dphi_sigma_scale[0][0]    =        15.3107;   TOF_dphi_sigma_scale[0][1]    =        10.0667;   TOF_dphi_sigma_scale[0][2]    =        1.00204; 
  TOF_dphi_sigma_scale[1][0]    =         14.011;   TOF_dphi_sigma_scale[1][1]    =        13.6968;   TOF_dphi_sigma_scale[1][2]    =        1.07296; 
  TOF_dphi_sigma_scale[2][0]    =        9.01396;   TOF_dphi_sigma_scale[2][1]    =        11.6926;   TOF_dphi_sigma_scale[2][2]    =       0.989334; 
  TOF_dphi_sigma_scale[3][0]    =        18.7065;   TOF_dphi_sigma_scale[3][1]    =        10.3418;   TOF_dphi_sigma_scale[3][2]    =       0.946212; 

  // EMC delta phi sigma
  // EAST
  EMC_dphi_sigma_scale[0][0][0] =        9.66036;   EMC_dphi_sigma_scale[0][0][1] =        9.51544;   EMC_dphi_sigma_scale[0][0][2] =        1.01865;
  EMC_dphi_sigma_scale[0][1][0] =        8.65402;   EMC_dphi_sigma_scale[0][1][1] =        13.0606;   EMC_dphi_sigma_scale[0][1][2] =        1.02532;
  EMC_dphi_sigma_scale[0][2][0] =        4.12475;   EMC_dphi_sigma_scale[0][2][1] =        9.44991;   EMC_dphi_sigma_scale[0][2][2] =       0.998977;
  EMC_dphi_sigma_scale[0][3][0] =        9.81571;   EMC_dphi_sigma_scale[0][3][1] =         9.6048;   EMC_dphi_sigma_scale[0][3][2] =        1.02692;

  // WEST
  EMC_dphi_sigma_scale[1][0][0] =        9.41391;   EMC_dphi_sigma_scale[1][0][1] =        9.90642;   EMC_dphi_sigma_scale[1][0][2] =        1.04209;
  EMC_dphi_sigma_scale[1][1][0] =        6.56411;   EMC_dphi_sigma_scale[1][1][1] =        12.4446;   EMC_dphi_sigma_scale[1][1][2] =        1.01465;
  EMC_dphi_sigma_scale[1][2][0] =        5.68853;   EMC_dphi_sigma_scale[1][2][1] =        11.4753;   EMC_dphi_sigma_scale[1][2][2] =        1.00339;
  EMC_dphi_sigma_scale[1][3][0] =        8.86251;   EMC_dphi_sigma_scale[1][3][1] =        9.35409;   EMC_dphi_sigma_scale[1][3][2] =       0.990825;
  

  // PC2 delta z sigma
  PC2_dz_sigma_scale[0][0]    =        10.4919;   PC2_dz_sigma_scale[0][1]    =        12.0695;   PC2_dz_sigma_scale[0][2]    =        1.00345; 
  PC2_dz_sigma_scale[1][0]    =         4.7433;   PC2_dz_sigma_scale[1][1]    =        10.9406;   PC2_dz_sigma_scale[1][2]    =        1.00186; 
  PC2_dz_sigma_scale[2][0]    =        5.17576;   PC2_dz_sigma_scale[2][1]    =        11.3236;   PC2_dz_sigma_scale[2][2]    =       0.995264; 
  PC2_dz_sigma_scale[3][0]    =        11.5708;   PC2_dz_sigma_scale[3][1]    =        12.5358;   PC2_dz_sigma_scale[3][2]    =       0.999271; 

  // PC3 delta z sigma
  // EAST
  PC3_dz_sigma_scale[0][0][0] =        11.9315;   PC3_dz_sigma_scale[0][0][1] =        11.7332;   PC3_dz_sigma_scale[0][0][2] =        1.01348; 
  PC3_dz_sigma_scale[0][1][0] =        5.26696;   PC3_dz_sigma_scale[0][1][1] =        10.5023;   PC3_dz_sigma_scale[0][1][2] =        1.00533; 
  PC3_dz_sigma_scale[0][2][0] =        5.67835;   PC3_dz_sigma_scale[0][2][1] =        10.8112;   PC3_dz_sigma_scale[0][2][2] =        1.00844; 
  PC3_dz_sigma_scale[0][3][0] =        17.2119;   PC3_dz_sigma_scale[0][3][1] =        13.3607;   PC3_dz_sigma_scale[0][3][2] =        1.02731; 

  // WEST
  PC3_dz_sigma_scale[1][0][0] =        9.12028;   PC3_dz_sigma_scale[1][0][1] =        11.1496;   PC3_dz_sigma_scale[1][0][2] =        1.00095; 
  PC3_dz_sigma_scale[1][1][0] =         3.7987;   PC3_dz_sigma_scale[1][1][1] =         9.3594;   PC3_dz_sigma_scale[1][1][2] =       0.998234; 
  PC3_dz_sigma_scale[1][2][0] =        4.76633;   PC3_dz_sigma_scale[1][2][1] =        10.1813;   PC3_dz_sigma_scale[1][2][2] =        1.02027; 
  PC3_dz_sigma_scale[1][3][0] =        11.8569;   PC3_dz_sigma_scale[1][3][1] =        12.0475;   PC3_dz_sigma_scale[1][3][2] =        1.02866; 

  // TOF delta z sigma
  TOF_dz_sigma_scale[0][0]    =        8.44457;   TOF_dz_sigma_scale[0][1]    =        10.6814;   TOF_dz_sigma_scale[0][2]    =       0.992709; 
  TOF_dz_sigma_scale[1][0]    =        4.97218;   TOF_dz_sigma_scale[1][1]    =        10.2378;   TOF_dz_sigma_scale[1][2]    =        1.03581; 
  TOF_dz_sigma_scale[2][0]    =        4.38788;   TOF_dz_sigma_scale[2][1]    =        9.89905;   TOF_dz_sigma_scale[2][2]    =        1.01739; 
  TOF_dz_sigma_scale[3][0]    =        6.96378;   TOF_dz_sigma_scale[3][1]    =        10.4366;   TOF_dz_sigma_scale[3][2]    =       0.985494; 

  // EMC delta z sigma
  // EAST
  EMC_dz_sigma_scale[0][0][0] =              0;   EMC_dz_sigma_scale[0][0][1] =              0;   EMC_dz_sigma_scale[0][0][2] =              1; 
  EMC_dz_sigma_scale[0][1][0] =              0;   EMC_dz_sigma_scale[0][1][1] =              0;   EMC_dz_sigma_scale[0][1][2] =              1; 
  EMC_dz_sigma_scale[0][2][0] =              0;   EMC_dz_sigma_scale[0][2][1] =              0;   EMC_dz_sigma_scale[0][2][2] =              1; 
  EMC_dz_sigma_scale[0][3][0] =              0;   EMC_dz_sigma_scale[0][3][1] =              0;   EMC_dz_sigma_scale[0][3][2] =              1; 

  // WEST
  EMC_dz_sigma_scale[1][0][0] =              0;   EMC_dz_sigma_scale[1][0][1] =              0;   EMC_dz_sigma_scale[1][0][2] =              1; 
  EMC_dz_sigma_scale[1][1][0] =              0;   EMC_dz_sigma_scale[1][1][1] =              0;   EMC_dz_sigma_scale[1][1][2] =              1; 
  EMC_dz_sigma_scale[1][2][0] =              0;   EMC_dz_sigma_scale[1][2][1] =              0;   EMC_dz_sigma_scale[1][2][2] =              1; 
  EMC_dz_sigma_scale[1][3][0] =              0;   EMC_dz_sigma_scale[1][3][1] =              0;   EMC_dz_sigma_scale[1][3][2] =              1; 

  return 0;
}


//_______________________________________________________________________________________
// Matching calibrator (old)
int MatchrecalReco::Burn_pc2_match()
{
  if (!run_Burn_pc2_match)
    {
      if (verbosity > 0)
        {
          cout << ThisName << " PC2 burning disabled" << endl;
        }
      return 0;
    }
  float meanpc2p, meanpc2z, sigmapc2z, sigmapc2p;

  for (unsigned int i = 0; i < d_cnt->get_npart(); i++)
    {
      PHSnglCentralTrack *sngltrk = d_cnt->get_track(i);
      sngltrk->ShutUp();
      if (run_Burn_pc2_match == 2)
        {
          if (sngltrk->isImplemented(sngltrk->get_mom()) &&
              sngltrk->isImplemented(sngltrk->get_the0()) &&
              sngltrk->isImplemented(sngltrk->get_zed()) &&
              sngltrk->isImplemented(sngltrk->get_alpha()) &&
              sngltrk->isImplemented(sngltrk->get_pc2dphi()) &&
              sngltrk->isImplemented(sngltrk->get_pc2dz()))
            {
              if (verbosity > 0)
                {
                  cout << ThisName << " PC2 burning workable" << endl;
                }
              run_Burn_pc2_match = 1;
            }
          else
            {
              run_Burn_pc2_match = 0;
              sngltrk->ShutUp(1);
              if (verbosity > 0)
                {
                  cout << ThisName << " PC2 burning not possible" << endl;
                }
              return 0;
            }
        }
      sngltrk->ShutUp(1);
      float mom = sngltrk->get_mom();
      float the0 = sngltrk->get_the0();
      float pt = mom;
      if (the0 > -999)
        {
          pt = mom * sin(the0);
        }
      float zed = sngltrk->get_zed();
      float alpha = sngltrk->get_alpha();

      float pc2dphi = sngltrk->get_pc2dphi();
      float pc2dz = sngltrk->get_pc2dz();

      //calculate the width
      sigmapc2z = sqrt(0.47 * 0.47 / pt / pt + 0.559 * 0.559);
      sigmapc2p = sqrt(0.481 * 0.481 / pt / pt + 0.494 * 0.494) / 419.;

      //calculate the mean values
      if (alpha < 0)
        {
          if (zed > 0)
            {
              meanpc2z = -0.313 * exp( -0.7 * pt) + 0.264;
              meanpc2p = (0.122 * exp( -0.7 * pt) + 0.08) / 419.;
            }
          else
            {
              meanpc2z = 0.341 * exp( -0.7 * pt) - 0.053;
              meanpc2p = (0.17 * exp( -0.7 * pt) + 0.06) / 419.;
            }
        }
      else
        {
          if (zed > 0)
            {
              meanpc2z = -0.201 * exp( -0.7 * pt) + 0.288;
              meanpc2p = ( -0.17 * exp( -0.7 * pt) - 0.12) / 419.;
            }
          else
            {
              meanpc2z = 0.291 * exp( -0.7 * pt) - 0.044;
              meanpc2p = ( -0.19 * exp( -0.7 * pt) - 0.12) / 419.;
            }
        }

      float pc2sdphi = (pc2dphi - meanpc2p) / sigmapc2p;
      float pc2sdz = (pc2dz - meanpc2z) / sigmapc2z;

      sngltrk->set_pc2sdphi(pc2sdphi);
      sngltrk->set_pc2sdz(pc2sdz);
    }
  return EVENT_OK;
}



int MatchrecalReco::Burn_pc3_match()
{
  if (!run_Burn_pc3_match)
    {
      if (verbosity > 0)
        {
          cout << ThisName << " PC3 burning disabled" << endl;
        }
      return 0;
    }
  float meanpc3p, meanpc3z, sigmapc3z, sigmapc3p;

  for (unsigned int i = 0; i < d_cnt->get_npart(); i++)
    {
      PHSnglCentralTrack *sngltrk = d_cnt->get_track(i);
      sngltrk->ShutUp();
      if (run_Burn_pc3_match == 2)
        {
          if (sngltrk->isImplemented(sngltrk->get_mom()) &&
              sngltrk->isImplemented(sngltrk->get_the0()) &&
              sngltrk->isImplemented(sngltrk->get_zed()) &&
              sngltrk->isImplemented(sngltrk->get_alpha()) &&
              sngltrk->isImplemented(sngltrk->get_phi()) &&
              sngltrk->isImplemented(sngltrk->get_pc3dphi()) &&
              sngltrk->isImplemented(sngltrk->get_pc3dz()))
            {
              if (verbosity > 0)
                {
                  cout << ThisName << " PC3 burning workable" << endl;
                }
              run_Burn_pc3_match = 1;
            }
          else
            {
              run_Burn_pc3_match = 0;
              sngltrk->ShutUp(1); // enable virtual warnings again
              if (verbosity > 0)
                {
                  cout << ThisName << " PC3 burning gave invalid" << endl;
                }
              return 0;
            }
        }
      float mom = sngltrk->get_mom();
      float the0 = sngltrk->get_the0();
      float pt = mom;
      if (the0 > -999)
        {
          pt = mom * sin(the0);
        }
      float zed = sngltrk->get_zed();
      float alpha = sngltrk->get_alpha();
      float phi = sngltrk->get_phi();

      float pc3dphi = sngltrk->get_pc3dphi();
      float pc3dz = sngltrk->get_pc3dz();

      //calculate the width

      if (phi < 1.57)
        { //west
          sigmapc3z = sqrt(0.739 * 0.739 / pt / pt + 0.723 * 0.723);
          sigmapc3p = sqrt(0.751 * 0.751 / pt / pt + 0.641 * 0.641) / 492.;
        }
      else
        { //east
          sigmapc3z = sqrt(0.65 * 0.65 / pt / pt + 0.716 * 0.716);
          sigmapc3p = sqrt(0.706 * 0.706 / pt / pt + 0.623 * 0.623) / 492.;
        }

      //calculate the mean values
      if (alpha < 0)
        {
          if (zed > 0)
            {
              if (phi < 1.57)
                { //west
                  meanpc3z = -0.41 * exp( -0.7 * pt) + 0.343;
                  meanpc3p = (0.166 * exp( -0.7 * pt) + 0.18) / 492.;
                }
              else
                {
                  meanpc3z = -0.44 * exp( -0.7 * pt) + 0.32;
                  meanpc3p = (0.32 * exp( -0.7 * pt) + 0.07) / 492.;
                }
            }
          else
            {
              if (phi < 1.57)
                { //west
                  meanpc3z = 0.469 * exp( -0.7 * pt) - 0.036;
                  meanpc3p = (0.25 * exp( -0.7 * pt) + 0.06) / 492.;
                }
              else
                {
                  meanpc3z = 0.33 * exp( -0.7 * pt) - 0.01;
                  meanpc3p = (0.45 * exp( -0.7 * pt) + 0.13) / 492.;
                }
            }
        }
      else
        {
          if (zed > 0)
            {
              if (phi < 1.57)
                { //west
                  meanpc3z = -0.314 * exp( -0.7 * pt) + 0.372;
                  meanpc3p = ( -0.23 * exp( -0.7 * pt) - 0.1) / 492.;
                }
              else
                {
                  meanpc3z = -0.41 * exp( -0.7 * pt) + 0.31;
                  meanpc3p = ( -0.32 * exp( -0.7 * pt) - 0.13) / 492.;
                }
            }
          else
            {
              if (phi < 1.57)
                { //west
                  meanpc3z = 0.465 * exp( -0.7 * pt) - 0.035;
                  meanpc3p = ( -0.297 * exp( -0.7 * pt) - 0.19) / 492.;
                }
              else
                {
                  meanpc3z = 0.51 * exp( -0.7 * pt) - 0.03;
                  meanpc3p = ( -0.41 * exp( -0.7 * pt) - 0.06) / 492.;
                }
            }
        }

      float pc3sdphi = (pc3dphi - meanpc3p) / sigmapc3p;
      float pc3sdz = (pc3dz - meanpc3z) / sigmapc3z;

      sngltrk->set_pc3sdphi(pc3sdphi);
      sngltrk->set_pc3sdz(pc3sdz);
      sngltrk->ShutUp(1);
    }

  return EVENT_OK;
}

int MatchrecalReco::Burn_tof_match()
{
  // Based on 62.4 GeV Au+Au fast track analysis output.
  // Magnetic field setting of 62.4 GeV run is (--), reversed field
  // (commited by T.Chujo, 2004.8.20)
  //
  if (!run_Burn_tof_match)
    {
      if (verbosity > 0)
        {
          cout << ThisName << " TOF burning disabled" << endl;
        }
      return 0;
    }

  float meantofp, meantofz, sigmatofz, sigmatofp;

  for (unsigned int i = 0; i < d_cnt->get_npart(); i++)
    {
      PHSnglCentralTrack *sngltrk = d_cnt->get_track(i);
      if (run_Burn_tof_match == 2)
        {
          if (sngltrk->isImplemented(sngltrk->get_mom()) &&
              sngltrk->isImplemented(sngltrk->get_the0()) &&
              sngltrk->isImplemented(sngltrk->get_zed()) &&
              sngltrk->isImplemented(sngltrk->get_alpha()) &&
              sngltrk->isImplemented(sngltrk->get_tofdphi()) &&
              sngltrk->isImplemented(sngltrk->get_tofdz()))
            {
              if (verbosity > 0)
                {
                  cout << ThisName << " TOF burning workable" << endl;
                }
              run_Burn_tof_match = 1;
            }
          else
            {
              run_Burn_tof_match = 0;
              sngltrk->ShutUp(1);
              if (verbosity > 0)
                {
                  cout << ThisName << " TOF burning gave invalid" << endl;
                }
              return 0;
            }
        }
      float mom = sngltrk->get_mom();
      float the0 = sngltrk->get_the0();
      float pt = mom;
      if (the0 > -999)
        {
          pt = mom * sin(the0);
        }
      float zed = sngltrk->get_zed();
      float alpha = sngltrk->get_alpha();
      float tofdphi = sngltrk->get_tofdphi();
      float tofdz = sngltrk->get_tofdz();

      //calculate the width and mean values
      if (alpha < 0)
        {
          if (zed > 0)
            {
              sigmatofz = ( 1.554 * exp( -1.896 * pt ) ) + 0.8161;
              sigmatofp = ( 0.01225 * exp( -5.152 * pt ) ) + 0.003853;
              meantofz = ( -0.7534 * exp( -0.7 * pt ) ) + 0.1224;
              meantofp = ( 0.005549 * exp( -2.335 * pt ) ) + 0.001061;
            }
          else
            {
              sigmatofz = ( 1.415 * exp( -1.679 * pt ) ) + 0.7823;
              sigmatofp = ( 0.007296 * exp( -3.904 * pt ) ) + 0.003961;
              meantofz = ( 0.2214 * exp( -0.007944 * pt * pt * pt * pt * pt * pt * pt ) ) + 0.2312;
              meantofp = ( 0.003348 * exp( -1.615 * pt ) ) + 0.001123;
            }
        }
      else
        {
          if (zed > 0)
            {
              sigmatofz = ( 1.610 * exp( -1.732 * pt ) ) + 0.80050;
              sigmatofp = ( 0.004362 * exp( -2.7580 * pt ) ) + 0.0037140;
              meantofz = ( -0.6625 * exp( -0.7 * pt ) ) + 0.06245;
              meantofp = ( -0.002175 * exp( -0.7707 * pt ) ) + 0.0001591;
            }
          else
            {
              sigmatofz = ( 1.381 * exp( -1.509 * pt ) ) + 0.7761;
              sigmatofp = ( 0.004356 * exp( -2.8920 * pt ) ) + 0.0039780;
              meantofz = ( 0.4939 * exp( -0.1922 * pt * pt ) ) + 0.1322;
              meantofp = ( -0.002152 * exp( -0.5342 * pt ) ) + 0.0002295;
            }
        }

      float tofsdphi = (tofdphi - meantofp) / sigmatofp;
      float tofsdz = (tofdz - meantofz) / sigmatofz;

      sngltrk->set_tofsdphi(tofsdphi);
      sngltrk->set_tofsdz(tofsdz);
      sngltrk->ShutUp(1);
    }
  return EVENT_OK;
}

