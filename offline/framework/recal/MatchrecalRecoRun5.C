#include "MatchrecalRecoRun5.h"
#include "Fun4AllReturnCodes.h"

#include "PHCentralTrack.h"
#include "PHSnglCentralTrack.h"
#include "getClass.h"
#include "RunHeader.h"

#include "PHGlobal.h"

#include "PHCompositeNode.h"
#include "recoConsts.h"

#include <iostream>

using namespace std;
using namespace findNode;

MatchrecalRecoRun5::MatchrecalRecoRun5(const char* name): Recalibrator(name)
{
  baseclasses.insert("PHCentralTrack");
}

int MatchrecalRecoRun5::InitRun(PHCompositeNode *topNode)
{

  RunHeader* d_run = findNode::getClass<RunHeader>(topNode, "RunHeader");
  if(!d_run){
    cout << PHWHERE << " RunHeader not found" << endl;
    return 0;
  }

  runNumber = d_run->get_RunNumber();

  b_field = 0;
  if(runNumber<155800) b_field = 1;
  else                 b_field = -1;

  return 0;
}

int MatchrecalRecoRun5::isValidRun(const int runno) const
{

  // Run5 CuCu 200 GeV
  if (149000 < runno && runno < 161000)
    {
      return 1;
    }
  // Run5 pp 200 GeV
  else if (166000 < runno && runno < 180000)
    {
      return 1;
    }

  return 0;
}

int MatchrecalRecoRun5::process_event(PHCompositeNode *topNode)
{
  d_cnt    = findNode::getClass<PHCentralTrack>(topNode, inputnodename.c_str());
  d_global = findNode::getClass<PHGlobal>(topNode, "PHGlobal");

  if (!d_cnt || !d_global) return 0;

  // Common correction (Run5)
  Common_Run5();

  // AfterBurner (CuCu / pp)
  if      (149000 < runNumber && runNumber < 164000) AfterBurner_Run5CuCu();
  else if (166000 < runNumber && runNumber < 180000) AfterBurner_Run5pp();

  return EVENT_OK;
}

int MatchrecalRecoRun5::Common_Run5()
{

  float bbcsum = d_global->getBbcChargeN() + d_global->getBbcChargeS();

  short i_bbcsum = -1;
  if(400<=bbcsum)                    i_bbcsum = 0;
  else if(200<=bbcsum && bbcsum<400) i_bbcsum = 1;
  else if(60<=bbcsum && bbcsum<200)  i_bbcsum = 2;
  else                               i_bbcsum = 3;

  if(i_bbcsum==-1) return 0;

  for (unsigned int itrk = 0; itrk < d_cnt->get_npart(); itrk++)
    {

      PHSnglCentralTrack *sngltrk = d_cnt->get_track(itrk);
      sngltrk->ShutUp();

      if (
	  sngltrk->isImplemented(sngltrk->get_the0()) &&
	  sngltrk->isImplemented(sngltrk->get_mom()) &&
	  sngltrk->isImplemented(sngltrk->get_charge()) &&
	  sngltrk->isImplemented(sngltrk->get_zed()) &&
	  sngltrk->isImplemented(sngltrk->get_phi()) &&
	  sngltrk->isImplemented(sngltrk->get_pc2dphi()) &&
	  sngltrk->isImplemented(sngltrk->get_pc2dz()) &&
	  sngltrk->isImplemented(sngltrk->get_pc3dphi()) &&
	  sngltrk->isImplemented(sngltrk->get_pc3dz()) &&
	  sngltrk->isImplemented(sngltrk->get_tofdphi()) &&
	  sngltrk->isImplemented(sngltrk->get_tofdz()) &&
	  sngltrk->isImplemented(sngltrk->get_emcdphi()) &&
	  sngltrk->isImplemented(sngltrk->get_emcdz()) 
	  )
	{
	  if (verbosity > 0) cout << PHWHERE << " " << Name() << "Workable" << endl;
	}
      else
	{
	  sngltrk->ShutUp(1);
	  if (verbosity > 0) cout << PHWHERE << " " << Name() << "Not workable" << endl;
	  return 0;
	}
        
      sngltrk->ShutUp(1); // enable virtual warnings again

      float the0 = sngltrk->get_the0();
      float mom  = sngltrk->get_mom();
      float pt   = mom;
      if (the0 > -999)
        {
          pt = mom * sin(the0);
        }

      short charge = sngltrk->get_charge();
      float zed    = sngltrk->get_zed();
      float phi    = sngltrk->get_phi();

      // --------------------

      short i_ch = -1;
      if(charge>0) i_ch = 0;
      else         i_ch = 1;

      short i_zed = -1;
      if(zed<-40)                i_zed=0; // far south
      else if(-40<=zed && zed<0) i_zed=1; // south
      else if(0<=zed && zed<40)  i_zed=2; // north
      else if(40<=zed)           i_zed=3; // far north

      short i_phi = -1;
      if(fabs(phi-0)<1.57) i_phi=0;
      else i_phi = 1;
 
      if(pt<0.05 || i_ch==-1 || i_zed==-1 || i_phi==-1) continue;
 
      // Matching variables

      float pc2dphi = sngltrk->get_pc2dphi();
      float pc2dz   = sngltrk->get_pc2dz();

      float pc3dphi = sngltrk->get_pc3dphi();
      float pc3dz   = sngltrk->get_pc3dz();

      float tofdphi = sngltrk->get_tofdphi();
      float tofdz   = sngltrk->get_tofdz();

      float emcdphi = sngltrk->get_emcdphi();
      float emcdz   = sngltrk->get_emcdz();

      // ------------------

      float pc2sdphi = Getpc2sdphi(i_ch, i_zed, i_phi, pt, pc2dphi);
      float pc3sdphi = Getpc3sdphi(i_ch, i_zed, i_phi, pt, pc3dphi);
      float tofsdphi = Gettofsdphi(i_ch, i_zed, i_phi, pt, tofdphi);
      float emcsdphi = Getemcsdphi(i_ch, i_zed, i_phi, pt, emcdphi);

      float pc2sdz = Getpc2sdz(i_bbcsum, i_ch, i_zed, i_phi, pt, pc2dz);
      float pc3sdz = Getpc3sdz(i_bbcsum, i_ch, i_zed, i_phi, pt, pc3dz);
      float tofsdz = Gettofsdz(i_bbcsum, i_ch, i_zed, i_phi, pt, tofdz);
      float emcsdz = Getemcsdz(i_bbcsum, i_ch, i_zed, i_phi, pt, emcdz);

      // ------------------

      sngltrk->set_pc2sdphi(pc2sdphi);
      sngltrk->set_pc3sdphi(pc3sdphi);
      sngltrk->set_tofsdphi(tofsdphi);
      sngltrk->set_emcsdphi(emcsdphi);
      sngltrk->set_pc2sdz(pc2sdz);
      sngltrk->set_pc3sdz(pc3sdz);
      sngltrk->set_tofsdz(tofsdz);
      sngltrk->set_emcsdz(emcsdz);

      /*
      sngltrk->ShutUp();
      sngltrk->set_pc2dphi(pc2sdphi);
      sngltrk->set_pc3dphi(pc3sdphi);
      sngltrk->set_tofdphi(tofsdphi);
      sngltrk->set_emcdphi(emcsdphi);
      sngltrk->set_pc2dz(pc2sdz);
      sngltrk->set_pc3dz(pc3sdz);
      sngltrk->set_tofdz(tofsdz);
      sngltrk->set_emcdz(emcsdz);
      sngltrk->ShutUp(1);
      */

    }

  return EVENT_OK;

}

// AfterBurner for Run5CuCu
int MatchrecalRecoRun5::AfterBurner_Run5CuCu()
{

  for (unsigned int itrk = 0; itrk < d_cnt->get_npart(); itrk++)
    {

      PHSnglCentralTrack *sngltrk = d_cnt->get_track(itrk);
      sngltrk->ShutUp();

      if (
	  sngltrk->isImplemented(sngltrk->get_the0()) &&
	  sngltrk->isImplemented(sngltrk->get_mom()) &&
	  sngltrk->isImplemented(sngltrk->get_charge()) &&
	  sngltrk->isImplemented(sngltrk->get_zed()) &&
	  sngltrk->isImplemented(sngltrk->get_phi()) &&
	  sngltrk->isImplemented(sngltrk->get_pc2sdphi()) &&
	  sngltrk->isImplemented(sngltrk->get_pc2sdz()) &&
	  sngltrk->isImplemented(sngltrk->get_pc3sdphi()) &&
	  sngltrk->isImplemented(sngltrk->get_pc3sdz()) &&
	  sngltrk->isImplemented(sngltrk->get_tofsdphi()) &&
	  sngltrk->isImplemented(sngltrk->get_tofsdz()) &&
	  sngltrk->isImplemented(sngltrk->get_emcsdphi()) &&
	  sngltrk->isImplemented(sngltrk->get_emcsdz()) 
	  )
	{
	  if (verbosity > 0) cout << PHWHERE << " " << Name() << "Workable" << endl;
	}
      else
	{
	  sngltrk->ShutUp(1);
	  if (verbosity > 0) cout << PHWHERE << " " << Name() << "Not workable" << endl;
	  return 0;
	}
        
      sngltrk->ShutUp(1); // enable virtual warnings again

      float the0 = sngltrk->get_the0();
      float mom  = sngltrk->get_mom();
      float pt   = mom;
      if (the0 > -999)
        {
          pt = mom * sin(the0);
        }

      short charge = sngltrk->get_charge();
      float zed    = sngltrk->get_zed();
      float phi    = sngltrk->get_phi();

      // --------------------

      short i_ch = -1;
      if(charge>0) i_ch = 0;
      else         i_ch = 1;

      short i_zed = -1;
      if(zed<-40)                i_zed=0; // far south
      else if(-40<=zed && zed<0) i_zed=1; // south
      else if(0<=zed && zed<40)  i_zed=2; // north
      else if(40<=zed)           i_zed=3; // far north

      short i_phi = -1;
      if(fabs(phi-0)<1.57) i_phi=0;
      else i_phi = 1;
 
      if(pt<0.05 || i_ch==-1 || i_zed==-1 || i_phi==-1) continue;
 
      // Matching variables

      float pc2dphi = sngltrk->get_pc2sdphi();
      float pc2dz   = sngltrk->get_pc2sdz();

      float pc3dphi = sngltrk->get_pc3sdphi();
      float pc3dz   = sngltrk->get_pc3sdz();

      float tofdphi = sngltrk->get_tofsdphi();
      float tofdz   = sngltrk->get_tofsdz();

      float emcdphi = sngltrk->get_emcsdphi();
      float emcdz   = sngltrk->get_emcsdz();

      // ------------------

      float pc2sdphi = Getpc2sdphi_AB_Run5CuCu(i_ch, i_zed, i_phi, pt, pc2dphi);
      float pc3sdphi = Getpc3sdphi_AB_Run5CuCu(i_ch, i_zed, i_phi, pt, pc3dphi);
      float tofsdphi = Gettofsdphi_AB_Run5CuCu(i_ch, i_zed, i_phi, pt, tofdphi);
      float emcsdphi = Getemcsdphi_AB_Run5CuCu(i_ch, i_zed, i_phi, pt, emcdphi);

      float pc2sdz = Getpc2sdz_AB_Run5CuCu(i_ch, i_zed, i_phi, pt, pc2dz);
      float pc3sdz = Getpc3sdz_AB_Run5CuCu(i_ch, i_zed, i_phi, pt, pc3dz);
      float tofsdz = Gettofsdz_AB_Run5CuCu(i_ch, i_zed, i_phi, pt, tofdz);
      float emcsdz = Getemcsdz_AB_Run5CuCu(i_ch, i_zed, i_phi, pt, emcdz);

      // ------------------

      sngltrk->set_pc2sdphi(pc2sdphi);
      sngltrk->set_pc3sdphi(pc3sdphi);
      sngltrk->set_tofsdphi(tofsdphi);
      sngltrk->set_emcsdphi(emcsdphi);
      sngltrk->set_pc2sdz(pc2sdz);
      sngltrk->set_pc3sdz(pc3sdz);
      sngltrk->set_tofsdz(tofsdz);
      sngltrk->set_emcsdz(emcsdz);

      /*
      sngltrk->ShutUp();
      sngltrk->set_pc2dphi(pc2sdphi);
      sngltrk->set_pc3dphi(pc3sdphi);
      sngltrk->set_tofdphi(tofsdphi);
      sngltrk->set_emcdphi(emcsdphi);
      sngltrk->set_pc2dz(pc2sdz);
      sngltrk->set_pc3dz(pc3sdz);
      sngltrk->set_tofdz(tofsdz);
      sngltrk->set_emcdz(emcsdz);
      sngltrk->ShutUp(1);
      */

    }

  return EVENT_OK;
}


// AfterBurner for Run5pp
int MatchrecalRecoRun5::AfterBurner_Run5pp()
{

  for (unsigned int itrk = 0; itrk < d_cnt->get_npart(); itrk++)
    {

      PHSnglCentralTrack *sngltrk = d_cnt->get_track(itrk);
      sngltrk->ShutUp();

      if (
	  sngltrk->isImplemented(sngltrk->get_the0()) &&
	  sngltrk->isImplemented(sngltrk->get_mom()) &&
	  sngltrk->isImplemented(sngltrk->get_charge()) &&
	  sngltrk->isImplemented(sngltrk->get_zed()) &&
	  sngltrk->isImplemented(sngltrk->get_phi()) &&
	  sngltrk->isImplemented(sngltrk->get_pc2sdphi()) &&
	  sngltrk->isImplemented(sngltrk->get_pc2sdz()) &&
	  sngltrk->isImplemented(sngltrk->get_pc3sdphi()) &&
	  sngltrk->isImplemented(sngltrk->get_pc3sdz()) &&
	  sngltrk->isImplemented(sngltrk->get_tofsdphi()) &&
	  sngltrk->isImplemented(sngltrk->get_tofsdz()) &&
	  sngltrk->isImplemented(sngltrk->get_emcsdphi()) &&
	  sngltrk->isImplemented(sngltrk->get_emcsdz()) 
	  )
	{
	  if (verbosity > 0) cout << PHWHERE << " " << Name() << "Workable" << endl;
	}
      else
	{
	  sngltrk->ShutUp(1);
	  if (verbosity > 0) cout << PHWHERE << " " << Name() << "Not workable" << endl;
	  return 0;
	}
        
      sngltrk->ShutUp(1); // enable virtual warnings again

      float the0 = sngltrk->get_the0();
      float mom  = sngltrk->get_mom();
      float pt   = mom;
      if (the0 > -999)
        {
          pt = mom * sin(the0);
        }

      short charge = sngltrk->get_charge();
      float zed    = sngltrk->get_zed();
      float phi    = sngltrk->get_phi();

      // --------------------

      short i_ch = -1;
      if(charge>0) i_ch = 0;
      else         i_ch = 1;

      short i_zed = -1;
      if(zed<-40)                i_zed=0; // far south
      else if(-40<=zed && zed<0) i_zed=1; // south
      else if(0<=zed && zed<40)  i_zed=2; // north
      else if(40<=zed)           i_zed=3; // far north

      short i_phi = -1;
      if(fabs(phi-0)<1.57) i_phi=0;
      else i_phi = 1;
 
      if(pt<0.05 || i_ch==-1 || i_zed==-1 || i_phi==-1) continue;
 
      // Matching variables

      float pc2dphi = sngltrk->get_pc2sdphi();
      float pc2dz   = sngltrk->get_pc2sdz();

      float pc3dphi = sngltrk->get_pc3sdphi();
      float pc3dz   = sngltrk->get_pc3sdz();

      float tofdphi = sngltrk->get_tofsdphi();
      float tofdz   = sngltrk->get_tofsdz();

      float emcdphi = sngltrk->get_emcsdphi();
      float emcdz   = sngltrk->get_emcsdz();

      // ------------------

      float pc2sdphi = Getpc2sdphi_AB_Run5pp(i_ch, i_zed, i_phi, pt, pc2dphi);
      float pc3sdphi = Getpc3sdphi_AB_Run5pp(i_ch, i_zed, i_phi, pt, pc3dphi);
      float tofsdphi = Gettofsdphi_AB_Run5pp(i_ch, i_zed, i_phi, pt, tofdphi);
      float emcsdphi = Getemcsdphi_AB_Run5pp(i_ch, i_zed, i_phi, pt, emcdphi);

      float pc2sdz = Getpc2sdz_AB_Run5pp(i_ch, i_zed, i_phi, pt, pc2dz);
      float pc3sdz = Getpc3sdz_AB_Run5pp(i_ch, i_zed, i_phi, pt, pc3dz);
      float tofsdz = Gettofsdz_AB_Run5pp(i_ch, i_zed, i_phi, pt, tofdz);
      float emcsdz = Getemcsdz_AB_Run5pp(i_ch, i_zed, i_phi, pt, emcdz);

      // ------------------

      sngltrk->set_pc2sdphi(pc2sdphi);
      sngltrk->set_pc3sdphi(pc3sdphi);
      sngltrk->set_tofsdphi(tofsdphi);
      sngltrk->set_emcsdphi(emcsdphi);
      sngltrk->set_pc2sdz(pc2sdz);
      sngltrk->set_pc3sdz(pc3sdz);
      sngltrk->set_tofsdz(tofsdz);
      sngltrk->set_emcsdz(emcsdz);

      /*
      sngltrk->ShutUp();
      sngltrk->set_pc2dphi(pc2sdphi);
      sngltrk->set_pc3dphi(pc3sdphi);
      sngltrk->set_tofdphi(tofsdphi);
      sngltrk->set_emcdphi(emcsdphi);
      sngltrk->set_pc2dz(pc2sdz);
      sngltrk->set_pc3dz(pc3sdz);
      sngltrk->set_tofdz(tofsdz);
      sngltrk->set_emcdz(emcsdz);
      sngltrk->ShutUp(1);
      */

    }

  return EVENT_OK;
}


// Getpc2sdphi
float MatchrecalRecoRun5::Getpc2sdphi(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dphi)
{

  if(dphi==-9999) return -9999;

  if(i_phi==1) return -9999; // <-- For PC2.

  // Radius (PC2)
  float R = 419.0;

  // Parameters
  float a_m[2][4][2],b_m[2][4][2],c_m[2][4][2];
  float a_s[2][4][2],b_s[2][4][2];

  if(b_field==1)
    {
      a_m[0][0][0]= -0.589954; b_m[0][0][0]= -1.100000; c_m[0][0][0]= -0.348405;
      a_m[0][1][0]= 0.112832;  b_m[0][1][0]= -1.100000; c_m[0][1][0]= -0.434569;
      a_m[0][2][0]= 0.122215;  b_m[0][2][0]= -1.100000; c_m[0][2][0]= -0.427454;
      a_m[0][3][0]= -0.561275; b_m[0][3][0]= -1.100000; c_m[0][3][0]= -0.386974;
      a_m[1][0][0]= 0.596621;  b_m[1][0][0]= -1.100000; c_m[1][0][0]= -0.602384;
      a_m[1][1][0]= -0.168633; b_m[1][1][0]= -1.100000; c_m[1][1][0]= -0.620614;
      a_m[1][2][0]= -0.175619; b_m[1][2][0]= -1.100000; c_m[1][2][0]= -0.622188;
      a_m[1][3][0]= 0.538306;  b_m[1][3][0]= -1.100000; c_m[1][3][0]= -0.642025;
      
      a_s[0][0][0]= 0.519189; b_s[0][0][0]= 0.537868;
      a_s[0][1][0]= 0.476121; b_s[0][1][0]= 0.515228;
      a_s[0][2][0]= 0.459703; b_s[0][2][0]= 0.553785;
      a_s[0][3][0]= 0.482083; b_s[0][3][0]= 0.527524;
      a_s[1][0][0]= 0.521431; b_s[1][0][0]= 0.554682;
      a_s[1][1][0]= 0.465813; b_s[1][1][0]= 0.531192;
      a_s[1][2][0]= 0.464670; b_s[1][2][0]= 0.564581;
      a_s[1][3][0]= 0.497039; b_s[1][3][0]= 0.516234;
    }
  else if(b_field==-1)
    {
      a_m[0][0][0]= 0.606284;  b_m[0][0][0]= -1.100000; c_m[0][0][0]= -0.602442;
      a_m[0][1][0]= -0.164438; b_m[0][1][0]= -1.100000; c_m[0][1][0]= -0.619171;
      a_m[0][2][0]= -0.191261; b_m[0][2][0]= -1.100000; c_m[0][2][0]= -0.617121;
      a_m[0][3][0]= 0.503845;  b_m[0][3][0]= -1.100000; c_m[0][3][0]= -0.635373;
      a_m[1][0][0]= -0.602078; b_m[1][0][0]= -1.100000; c_m[1][0][0]= -0.344063;
      a_m[1][1][0]= 0.104829;  b_m[1][1][0]= -1.100000; c_m[1][1][0]= -0.429357;
      a_m[1][2][0]= 0.115489;  b_m[1][2][0]= -1.100000; c_m[1][2][0]= -0.420829;
      a_m[1][3][0]= -0.550072; b_m[1][3][0]= -1.100000; c_m[1][3][0]= -0.382257;

      a_s[0][0][0]= 0.535890; b_s[0][0][0]= 0.553632;
      a_s[0][1][0]= 0.488856; b_s[0][1][0]= 0.508878;
      a_s[0][2][0]= 0.484650; b_s[0][2][0]= 0.553376;
      a_s[0][3][0]= 0.519040; b_s[0][3][0]= 0.505567;
      a_s[1][0][0]= 0.507132; b_s[1][0][0]= 0.536645;
      a_s[1][1][0]= 0.439908; b_s[1][1][0]= 0.535290;
      a_s[1][2][0]= 0.433660; b_s[1][2][0]= 0.569482;
      a_s[1][3][0]= 0.470339; b_s[1][3][0]= 0.529281;
    }
  else
    {
      return -9999;
    }

  float m_dphi = 0;
  float s_dphi = 1;
  
  m_dphi = ( a_m[i_ch][i_zed][i_phi] * exp(b_m[i_ch][i_zed][i_phi]*pt) + c_m[i_ch][i_zed][i_phi] ) / R;
  s_dphi = sqrt( a_s[i_ch][i_zed][i_phi]*a_s[i_ch][i_zed][i_phi]/pt/pt + b_s[i_ch][i_zed][i_phi]*b_s[i_ch][i_zed][i_phi] ) / R;

  float sdphi  = -9999;
  if(s_dphi>0) sdphi = (dphi - m_dphi)/s_dphi;      

  return sdphi;

}


// Getpc3sdphi
float MatchrecalRecoRun5::Getpc3sdphi(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dphi)
{

  if(dphi==-9999) return -9999;

  // Radius (PC3)
  float R = 492.0;

  // Parameters
  float a_m[2][4][2],b_m[2][4][2],c_m[2][4][2];
  float a_s[2][4][2],b_s[2][4][2];

  if(b_field==1)
    {
      a_m[0][0][0]= -1.132296; b_m[0][0][0]= -1.400000; c_m[0][0][0]= -0.394910;
      a_m[0][0][1]= -0.807569; b_m[0][0][1]= -1.400000; c_m[0][0][1]= 0.400061;
      a_m[0][1][0]= 0.085843;  b_m[0][1][0]= -1.400000; c_m[0][1][0]= -0.473269;
      a_m[0][1][1]= 0.306211;  b_m[0][1][1]= -1.400000; c_m[0][1][1]= 0.470336;
      a_m[0][2][0]= 0.123689;  b_m[0][2][0]= -1.400000; c_m[0][2][0]= -0.387835;
      a_m[0][2][1]= 0.143826;  b_m[0][2][1]= -1.400000; c_m[0][2][1]= 0.392951;
      a_m[0][3][0]= -0.926143; b_m[0][3][0]= -1.400000; c_m[0][3][0]= -0.547190;
      a_m[0][3][1]= -1.114498; b_m[0][3][1]= -1.400000; c_m[0][3][1]= 0.446288;
      a_m[1][0][0]= 1.078963;  b_m[1][0][0]= -1.400000; c_m[1][0][0]= -0.595782;
      a_m[1][0][1]= 0.714729;  b_m[1][0][1]= -1.400000; c_m[1][0][1]= 0.166853;
      a_m[1][1][0]= -0.200760; b_m[1][1][0]= -1.400000; c_m[1][1][0]= -0.723566;
      a_m[1][1][1]= -0.322325; b_m[1][1][1]= -1.400000; c_m[1][1][1]= 0.218021;
      a_m[1][2][0]= -0.175655; b_m[1][2][0]= -1.400000; c_m[1][2][0]= -0.641057;
      a_m[1][2][1]= -0.213545; b_m[1][2][1]= -1.400000; c_m[1][2][1]= 0.190877;
      a_m[1][3][0]= 0.985963;  b_m[1][3][0]= -1.400000; c_m[1][3][0]= -0.774557;
      a_m[1][3][1]= 0.968756;  b_m[1][3][1]= -1.400000; c_m[1][3][1]= 0.313450;

      a_s[0][0][0]= 0.831694; b_s[0][0][0]= 0.681992;
      a_s[0][0][1]= 0.813753; b_s[0][0][1]= 0.615978;
      a_s[0][1][0]= 0.786659; b_s[0][1][0]= 0.669544;
      a_s[0][1][1]= 0.759124; b_s[0][1][1]= 0.640163;
      a_s[0][2][0]= 0.767942; b_s[0][2][0]= 0.700482;
      a_s[0][2][1]= 0.763025; b_s[0][2][1]= 0.650093;
      a_s[0][3][0]= 0.790472; b_s[0][3][0]= 0.694188;
      a_s[0][3][1]= 0.794222; b_s[0][3][1]= 0.615539;
      a_s[1][0][0]= 0.838478; b_s[1][0][0]= 0.699993;
      a_s[1][0][1]= 0.795954; b_s[1][0][1]= 0.658575;
      a_s[1][1][0]= 0.795403; b_s[1][1][0]= 0.679872;
      a_s[1][1][1]= 0.729901; b_s[1][1][1]= 0.661110;
      a_s[1][2][0]= 0.787223; b_s[1][2][0]= 0.709380;
      a_s[1][2][1]= 0.719344; b_s[1][2][1]= 0.662066;
      a_s[1][3][0]= 0.795732; b_s[1][3][0]= 0.677296;
      a_s[1][3][1]= 0.766792; b_s[1][3][1]= 0.635873;
    }
  else if(b_field==-1)
    {
      a_m[0][0][0]= 1.075355;  b_m[0][0][0]= -1.400000; c_m[0][0][0]= -0.592248;
      a_m[0][0][1]= 0.715598;  b_m[0][0][1]= -1.400000; c_m[0][0][1]= 0.153389;
      a_m[0][1][0]= -0.210295; b_m[0][1][0]= -1.400000; c_m[0][1][0]= -0.721065;
      a_m[0][1][1]= -0.339011; b_m[0][1][1]= -1.400000; c_m[0][1][1]= 0.195463;
      a_m[0][2][0]= -0.205684; b_m[0][2][0]= -1.400000; c_m[0][2][0]= -0.637747;
      a_m[0][2][1]= -0.248660; b_m[0][2][1]= -1.400000; c_m[0][2][1]= 0.168332;
      a_m[0][3][0]= 0.949432;  b_m[0][3][0]= -1.400000; c_m[0][3][0]= -0.775335;
      a_m[0][3][1]= 0.925043;  b_m[0][3][1]= -1.400000; c_m[0][3][1]= 0.305483;
      a_m[1][0][0]= -1.147283; b_m[1][0][0]= -1.400000; c_m[1][0][0]= -0.384375;
      a_m[1][0][1]= -0.813470; b_m[1][0][1]= -1.400000; c_m[1][0][1]= 0.390154;
      a_m[1][1][0]= 0.068981;  b_m[1][1][0]= -1.400000; c_m[1][1][0]= -0.462578;
      a_m[1][1][1]= 0.332202;  b_m[1][1][1]= -1.400000; c_m[1][1][1]= 0.449466;
      a_m[1][2][0]= 0.115537;  b_m[1][2][0]= -1.400000; c_m[1][2][0]= -0.378445;
      a_m[1][2][1]= 0.186124;  b_m[1][2][1]= -1.400000; c_m[1][2][1]= 0.372964;
      a_m[1][3][0]= -0.899455; b_m[1][3][0]= -1.400000; c_m[1][3][0]= -0.545159;
      a_m[1][3][1]= -1.086101; b_m[1][3][1]= -1.400000; c_m[1][3][1]= 0.441488;

      a_s[0][0][0]= 0.853019; b_s[0][0][0]= 0.674976;
      a_s[0][0][1]= 0.815754; b_s[0][0][1]= 0.646761;
      a_s[0][1][0]= 0.799689; b_s[0][1][0]= 0.659489;
      a_s[0][1][1]= 0.749674; b_s[0][1][1]= 0.660276;
      a_s[0][2][0]= 0.792421; b_s[0][2][0]= 0.683064;
      a_s[0][2][1]= 0.751906; b_s[0][2][1]= 0.674456;
      a_s[0][3][0]= 0.818613; b_s[0][3][0]= 0.654682;
      a_s[0][3][1]= 0.785382; b_s[0][3][1]= 0.636597;
      a_s[1][0][0]= 0.814136; b_s[1][0][0]= 0.688218;
      a_s[1][0][1]= 0.797013; b_s[1][0][1]= 0.622191;
      a_s[1][1][0]= 0.771106; b_s[1][1][0]= 0.687499;
      a_s[1][1][1]= 0.737590; b_s[1][1][1]= 0.645733;
      a_s[1][2][0]= 0.752540; b_s[1][2][0]= 0.727556;
      a_s[1][2][1]= 0.742538; b_s[1][2][1]= 0.664807;
      a_s[1][3][0]= 0.759661; b_s[1][3][0]= 0.718388;
      a_s[1][3][1]= 0.777625; b_s[1][3][1]= 0.625489;      
    }
  else
    {
      return -9999;
    }

  float m_dphi = 0;
  float s_dphi = 1;

  m_dphi = ( a_m[i_ch][i_zed][i_phi] * exp(b_m[i_ch][i_zed][i_phi]*pt) + c_m[i_ch][i_zed][i_phi] ) / R;
  s_dphi = sqrt( a_s[i_ch][i_zed][i_phi]*a_s[i_ch][i_zed][i_phi]/pt/pt + b_s[i_ch][i_zed][i_phi]*b_s[i_ch][i_zed][i_phi] ) / R;

  float sdphi  = -9999;
  if(s_dphi>0) sdphi = (dphi - m_dphi)/s_dphi;      

  return sdphi;

}



// Gettofsdphi
float MatchrecalRecoRun5::Gettofsdphi(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dphi)
{

  if(dphi==-9999) return -9999;

  if(i_phi==0) return -9999; // <-- For TOF.

  // Radius (TOF)
  float R = 506.0;

  // Parameters
  float a_m[2][4][2],b_m[2][4][2],c_m[2][4][2];
  float a_s[2][4][2],b_s[2][4][2];

  if(b_field==1)
    {
      a_m[0][0][1]= -0.119332; b_m[0][0][1]= -1.800000; c_m[0][0][1]= 0.803470;
      a_m[0][1][1]= 1.855198;  b_m[0][1][1]= -1.800000; c_m[0][1][1]= 0.957092;
      a_m[0][2][1]= 1.721190;  b_m[0][2][1]= -1.800000; c_m[0][2][1]= 0.882279;
      a_m[0][3][1]= -0.269301; b_m[0][3][1]= -1.800000; c_m[0][3][1]= 0.772336;
      a_m[1][0][1]= 0.220819;  b_m[1][0][1]= -1.800000; c_m[1][0][1]= 0.215439;
      a_m[1][1][1]= -1.435393; b_m[1][1][1]= -1.800000; c_m[1][1][1]= 0.285535;
      a_m[1][2][1]= -1.579059; b_m[1][2][1]= -1.800000; c_m[1][2][1]= 0.327567;
      a_m[1][3][1]= -0.237002; b_m[1][3][1]= -1.800000; c_m[1][3][1]= 0.387810;

      a_s[0][0][1]= 0.714617; b_s[0][0][1]= 2.209567;
      a_s[0][1][1]= 0.830797; b_s[0][1][1]= 2.317026;
      a_s[0][2][1]= 0.797253; b_s[0][2][1]= 2.107996;
      a_s[0][3][1]= 0.485451; b_s[0][3][1]= 2.113753;
      a_s[1][0][1]= 0.882163; b_s[1][0][1]= 2.212983;
      a_s[1][1][1]= 0.839544; b_s[1][1][1]= 2.331417;
      a_s[1][2][1]= 0.909195; b_s[1][2][1]= 2.068029;
      a_s[1][3][1]= 0.892024; b_s[1][3][1]= 1.986232;
    }
  else if(b_field==-1)
    {
      a_m[0][0][1]= 0.223340;  b_m[0][0][1]= -1.800000; c_m[0][0][1]= 0.017304;
      a_m[0][1][1]= -1.499997; b_m[0][1][1]= -1.800000; c_m[0][1][1]= 0.186997;
      a_m[0][2][1]= -1.615606; b_m[0][2][1]= -1.800000; c_m[0][2][1]= 0.225896;
      a_m[0][3][1]= -0.341767; b_m[0][3][1]= -1.800000; c_m[0][3][1]= 0.186383;
      a_m[1][0][1]= 0.019200;  b_m[1][0][1]= -1.800000; c_m[1][0][1]= 0.611367;
      a_m[1][1][1]= 2.037543;  b_m[1][1][1]= -1.800000; c_m[1][1][1]= 0.860882;
      a_m[1][2][1]= 1.871031;  b_m[1][2][1]= -1.800000; c_m[1][2][1]= 0.779042;
      a_m[1][3][1]= 0.001004;  b_m[1][3][1]= -1.800000; c_m[1][3][1]= 0.565315;

      a_s[0][0][1]= 0.886694; b_s[0][0][1]= 2.195392;
      a_s[0][1][1]= 0.858236; b_s[0][1][1]= 2.329252;
      a_s[0][2][1]= 0.933725; b_s[0][2][1]= 2.055704;
      a_s[0][3][1]= 0.911001; b_s[0][3][1]= 1.964767;
      a_s[1][0][1]= 0.654328; b_s[1][0][1]= 2.221203;
      a_s[1][1][1]= 0.791308; b_s[1][1][1]= 2.341891;
      a_s[1][2][1]= 0.774621; b_s[1][2][1]= 2.116084;
      a_s[1][3][1]= 0.432425; b_s[1][3][1]= 2.122969;
    }
  else
    {
      return -9999;
    }

  float m_dphi = 0;
  float s_dphi = 1;

  m_dphi = ( a_m[i_ch][i_zed][i_phi] * exp(b_m[i_ch][i_zed][i_phi]*pt) + c_m[i_ch][i_zed][i_phi] ) / R;
  s_dphi = sqrt( a_s[i_ch][i_zed][i_phi]*a_s[i_ch][i_zed][i_phi]/pt/pt + b_s[i_ch][i_zed][i_phi]*b_s[i_ch][i_zed][i_phi] ) / R;

  float sdphi  = -9999;
  if(s_dphi>0) sdphi = (dphi - m_dphi)/s_dphi;      

  return sdphi;

}



// Getemcsdphi
float MatchrecalRecoRun5::Getemcsdphi(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dphi)
{

  if(dphi==-9999) return -9999;

  // Radius (EMC)
  float R = 503.0;

  // Parameters
  float a_m[2][4][2],b_m[2][4][2],c_m[2][4][2];
  float a_s[2][4][2],b_s[2][4][2];

  if(b_field==1)
    {
      a_m[0][0][0]= 1.771378;  b_m[0][0][0]= -1.200000; c_m[0][0][0]= -0.182140;
      a_m[0][0][1]= 1.552208;  b_m[0][0][1]= -1.200000; c_m[0][0][1]= 1.415308;
      a_m[0][1][0]= 2.942078;  b_m[0][1][0]= -1.200000; c_m[0][1][0]= -0.084370;
      a_m[0][1][1]= 2.856300;  b_m[0][1][1]= -1.200000; c_m[0][1][1]= 1.525009;
      a_m[0][2][0]= 3.152888;  b_m[0][2][0]= -1.200000; c_m[0][2][0]= -0.283066;
      a_m[0][2][1]= 2.872244;  b_m[0][2][1]= -1.200000; c_m[0][2][1]= 1.507205;
      a_m[0][3][0]= 2.144078;  b_m[0][3][0]= -1.200000; c_m[0][3][0]= -0.505840;
      a_m[0][3][1]= 1.476908;  b_m[0][3][1]= -1.200000; c_m[0][3][1]= 1.772877;
      a_m[1][0][0]= -1.764469; b_m[1][0][0]= -1.200000; c_m[1][0][0]= -1.446646;
      a_m[1][0][1]= -1.791852; b_m[1][0][1]= -1.200000; c_m[1][0][1]= 0.340717;
      a_m[1][1][0]= -2.909572; b_m[1][1][0]= -1.200000; c_m[1][1][0]= -1.461472;
      a_m[1][1][1]= -2.913629; b_m[1][1][1]= -1.200000; c_m[1][1][1]= 0.392095;
      a_m[1][2][0]= -3.009542; b_m[1][2][0]= -1.200000; c_m[1][2][0]= -1.569492;
      a_m[1][2][1]= -2.837486; b_m[1][2][1]= -1.200000; c_m[1][2][1]= 0.410580;
      a_m[1][3][0]= -1.770229; b_m[1][3][0]= -1.200000; c_m[1][3][0]= -1.686148;
      a_m[1][3][1]= -1.628034; b_m[1][3][1]= -1.200000; c_m[1][3][1]= 0.789688;
      
      a_s[0][0][0]= 1.064856; b_s[0][0][0]= 2.490684;
      a_s[0][0][1]= 1.029747; b_s[0][0][1]= 2.339439;
      a_s[0][1][0]= 1.149146; b_s[0][1][0]= 2.381991;
      a_s[0][1][1]= 1.050123; b_s[0][1][1]= 2.345184;
      a_s[0][2][0]= 1.105426; b_s[0][2][0]= 2.333966;
      a_s[0][2][1]= 1.034651; b_s[0][2][1]= 2.277821;
      a_s[0][3][0]= 1.019349; b_s[0][3][0]= 2.303583;
      a_s[0][3][1]= 1.026582; b_s[0][3][1]= 2.362498;
      a_s[1][0][0]= 1.090772; b_s[1][0][0]= 2.626319;
      a_s[1][0][1]= 1.009778; b_s[1][0][1]= 2.383843;
      a_s[1][1][0]= 1.135557; b_s[1][1][0]= 2.527253;
      a_s[1][1][1]= 1.009237; b_s[1][1][1]= 2.396518;
      a_s[1][2][0]= 1.127451; b_s[1][2][0]= 2.450350;
      a_s[1][2][1]= 0.820144; b_s[1][2][1]= 2.419662;
      a_s[1][3][0]= 1.086757; b_s[1][3][0]= 2.426369;
      a_s[1][3][1]= 0.972078; b_s[1][3][1]= 2.462521;      
    }
  else if(b_field==-1)
    {
      a_m[0][0][0]= -1.801356; b_m[0][0][0]= -1.200000; c_m[0][0][0]= -1.471755;
      a_m[0][0][1]= -1.800937; b_m[0][0][1]= -1.200000; c_m[0][0][1]= 0.321216;
      a_m[0][1][0]= -2.958690; b_m[0][1][0]= -1.200000; c_m[0][1][0]= -1.496468;
      a_m[0][1][1]= -2.898186; b_m[0][1][1]= -1.200000; c_m[0][1][1]= 0.341117;
      a_m[0][2][0]= -3.086718; b_m[0][2][0]= -1.200000; c_m[0][2][0]= -1.601782;
      a_m[0][2][1]= -2.861170; b_m[0][2][1]= -1.200000; c_m[0][2][1]= 0.379961;
      a_m[0][3][0]= -1.828288; b_m[0][3][0]= -1.200000; c_m[0][3][0]= -1.719275;
      a_m[0][3][1]= -1.676432; b_m[0][3][1]= -1.200000; c_m[0][3][1]= 0.775023;
      a_m[1][0][0]= 1.738110;  b_m[1][0][0]= -1.200000; c_m[1][0][0]= -0.184062;
      a_m[1][0][1]= 1.530551;  b_m[1][0][1]= -1.200000; c_m[1][0][1]= 1.382342;
      a_m[1][1][0]= 2.912160;  b_m[1][1][0]= -1.200000; c_m[1][1][0]= -0.101241;
      a_m[1][1][1]= 2.895195;  b_m[1][1][1]= -1.200000; c_m[1][1][1]= 1.458137;
      a_m[1][2][0]= 3.108878;  b_m[1][2][0]= -1.200000; c_m[1][2][0]= -0.287218;
      a_m[1][2][1]= 2.903737;  b_m[1][2][1]= -1.200000; c_m[1][2][1]= 1.464701;
      a_m[1][3][0]= 2.140336;  b_m[1][3][0]= -1.200000; c_m[1][3][0]= -0.507444;
      a_m[1][3][1]= 1.525320;  b_m[1][3][1]= -1.200000; c_m[1][3][1]= 1.728657;

      a_s[0][0][0]= 1.126546; b_s[0][0][0]= 2.507812;
      a_s[0][0][1]= 1.057163; b_s[0][0][1]= 2.269841;
      a_s[0][1][0]= 1.189244; b_s[0][1][0]= 2.399645;
      a_s[0][1][1]= 1.084259; b_s[0][1][1]= 2.288780;
      a_s[0][2][0]= 1.178578; b_s[0][2][0]= 2.320204;
      a_s[0][2][1]= 0.999189; b_s[0][2][1]= 2.271898;
      a_s[0][3][0]= 1.133175; b_s[0][3][0]= 2.300947;
      a_s[0][3][1]= 1.030447; b_s[0][3][1]= 2.338541;
      a_s[1][0][0]= 1.043834; b_s[1][0][0]= 2.603433;
      a_s[1][0][1]= 0.957063; b_s[1][0][1]= 2.465811;
      a_s[1][1][0]= 1.066592; b_s[1][1][0]= 2.518903;
      a_s[1][1][1]= 0.951799; b_s[1][1][1]= 2.474389;
      a_s[1][2][0]= 1.041026; b_s[1][2][0]= 2.469456;
      a_s[1][2][1]= 0.910924; b_s[1][2][1]= 2.398910;
      a_s[1][3][0]= 0.958045; b_s[1][3][0]= 2.434707;
      a_s[1][3][1]= 0.955861; b_s[1][3][1]= 2.489516;
    }
  else
    {
      return -9999;
    }

  float m_dphi = 0;
  float s_dphi = 1;

  m_dphi = ( a_m[i_ch][i_zed][i_phi] * exp(b_m[i_ch][i_zed][i_phi]*pt) + c_m[i_ch][i_zed][i_phi] ) / R;
  s_dphi = sqrt( a_s[i_ch][i_zed][i_phi]*a_s[i_ch][i_zed][i_phi]/pt/pt + b_s[i_ch][i_zed][i_phi]*b_s[i_ch][i_zed][i_phi] ) / R;

  float sdphi  = -9999;
  if(s_dphi>0) sdphi = (dphi - m_dphi)/s_dphi;      

  return sdphi;

}


// Getpc2sdz
float MatchrecalRecoRun5::Getpc2sdz(const short i_bbcsum, const short i_ch, const short i_zed, const short i_phi, const float pt, const float dz)
{

  if(dz==-9999) return -9999;

  if(i_phi==1) return -9999; // <-- For PC2.

  // Parameters
  float a_m[2][4][2],b_m[2][4][2],c_m[2][4][2], d_m[2][4][2],e_m[2][4][2];
  float a_s[2][4][2],b_s[2][4][2];

  if(b_field==1)
    {

      if(i_bbcsum==0)
	{
	  a_m[0][0][0]= 2.301243;  b_m[0][0][0]= -1.200000; c_m[0][0][0]= 0.056990; d_m[0][0][0]= 0.681475;  e_m[0][0][0]= -0.152062;
	  a_m[0][1][0]= 0.684411;  b_m[0][1][0]= -1.200000; c_m[0][1][0]= 0.080290; d_m[0][1][0]= 0.283106;  e_m[0][1][0]= -0.060679;
	  a_m[0][2][0]= -0.785570; b_m[0][2][0]= -1.200000; c_m[0][2][0]= 0.251083; d_m[0][2][0]= 0.015202;  e_m[0][2][0]= 0.073552;
	  a_m[0][3][0]= -2.398310; b_m[0][3][0]= -1.200000; c_m[0][3][0]= 0.354305; d_m[0][3][0]= -0.288589; e_m[0][3][0]= 0.155636;
	  a_m[1][0][0]= 2.103320;  b_m[1][0][0]= -1.200000; c_m[1][0][0]= 0.026295; d_m[1][0][0]= 0.423240;  e_m[1][0][0]= -0.025269;
	  a_m[1][1][0]= 0.621258;  b_m[1][1][0]= -1.200000; c_m[1][1][0]= 0.076337; d_m[1][1][0]= 0.211592;  e_m[1][1][0]= -0.024492;
	  a_m[1][2][0]= -0.601928; b_m[1][2][0]= -1.200000; c_m[1][2][0]= 0.289903; d_m[1][2][0]= 0.210760;  e_m[1][2][0]= -0.012635;
	  a_m[1][3][0]= -1.974482; b_m[1][3][0]= -1.200000; c_m[1][3][0]= 0.410884; d_m[1][3][0]= 0.095080;  e_m[1][3][0]= -0.019469;

	  a_s[0][0][0]= 0.498039; b_s[0][0][0]= 0.543627;
	  a_s[0][1][0]= 0.489372; b_s[0][1][0]= 0.543185;
	  a_s[0][2][0]= 0.488972; b_s[0][2][0]= 0.538302;
	  a_s[0][3][0]= 0.485315; b_s[0][3][0]= 0.537674;
	  a_s[1][0][0]= 0.451452; b_s[1][0][0]= 0.535324;
	  a_s[1][1][0]= 0.455977; b_s[1][1][0]= 0.522010;
	  a_s[1][2][0]= 0.444080; b_s[1][2][0]= 0.502054;
	  a_s[1][3][0]= 0.443190; b_s[1][3][0]= 0.522629;
	}
      else if(i_bbcsum==1)
	{
	  a_m[0][0][0]= 2.295104;  b_m[0][0][0]= -1.200000; c_m[0][0][0]= 0.081833; d_m[0][0][0]= 0.706507;  e_m[0][0][0]= -0.153557;
	  a_m[0][1][0]= 0.687189;  b_m[0][1][0]= -1.200000; c_m[0][1][0]= 0.103241; d_m[0][1][0]= 0.307941;  e_m[0][1][0]= -0.061432;
	  a_m[0][2][0]= -0.794910; b_m[0][2][0]= -1.200000; c_m[0][2][0]= 0.275263; d_m[0][2][0]= 0.035692;  e_m[0][2][0]= 0.074581;
	  a_m[0][3][0]= -2.380486; b_m[0][3][0]= -1.200000; c_m[0][3][0]= 0.374137; d_m[0][3][0]= -0.264766; e_m[0][3][0]= 0.154850;
	  a_m[1][0][0]= 2.091738;  b_m[1][0][0]= -1.200000; c_m[1][0][0]= 0.049971; d_m[1][0][0]= 0.446848;  e_m[1][0][0]= -0.026336;
	  a_m[1][1][0]= 0.577228;  b_m[1][1][0]= -1.200000; c_m[1][1][0]= 0.103953; d_m[1][1][0]= 0.234659;  e_m[1][1][0]= -0.024356;
	  a_m[1][2][0]= -0.558264; b_m[1][2][0]= -1.200000; c_m[1][2][0]= 0.308929; d_m[1][2][0]= 0.232255;  e_m[1][2][0]= -0.011898;
	  a_m[1][3][0]= -1.971650; b_m[1][3][0]= -1.200000; c_m[1][3][0]= 0.433411; d_m[1][3][0]= 0.117111;  e_m[1][3][0]= -0.019572;

	  a_s[0][0][0]= 0.496393; b_s[0][0][0]= 0.583630;
	  a_s[0][1][0]= 0.486620; b_s[0][1][0]= 0.581700;
	  a_s[0][2][0]= 0.491661; b_s[0][2][0]= 0.569012;
	  a_s[0][3][0]= 0.482908; b_s[0][3][0]= 0.579881;
	  a_s[1][0][0]= 0.448192; b_s[1][0][0]= 0.576755;
	  a_s[1][1][0]= 0.455097; b_s[1][1][0]= 0.556582;
	  a_s[1][2][0]= 0.441334; b_s[1][2][0]= 0.544518;
	  a_s[1][3][0]= 0.438454; b_s[1][3][0]= 0.567997;
	}
      else if(i_bbcsum==2)
	{
	  a_m[0][0][0]= 2.317345;  b_m[0][0][0]= -1.200000; c_m[0][0][0]= 0.109194; d_m[0][0][0]= 0.746117;  e_m[0][0][0]= -0.159796;
	  a_m[0][1][0]= 0.675826;  b_m[0][1][0]= -1.200000; c_m[0][1][0]= 0.138935; d_m[0][1][0]= 0.346238;  e_m[0][1][0]= -0.063904;
	  a_m[0][2][0]= -0.798980; b_m[0][2][0]= -1.200000; c_m[0][2][0]= 0.303576; d_m[0][2][0]= 0.063388;  e_m[0][2][0]= 0.074771;
	  a_m[0][3][0]= -2.347128; b_m[0][3][0]= -1.200000; c_m[0][3][0]= 0.403110; d_m[0][3][0]= -0.236272; e_m[0][3][0]= 0.157355;
	  a_m[1][0][0]= 2.093758;  b_m[1][0][0]= -1.200000; c_m[1][0][0]= 0.080384; d_m[1][0][0]= 0.479332;  e_m[1][0][0]= -0.026735;
	  a_m[1][1][0]= 0.583093;  b_m[1][1][0]= -1.200000; c_m[1][1][0]= 0.136719; d_m[1][1][0]= 0.273412;  e_m[1][1][0]= -0.027777;
	  a_m[1][2][0]= -0.537432; b_m[1][2][0]= -1.200000; c_m[1][2][0]= 0.332035; d_m[1][2][0]= 0.262268;  e_m[1][2][0]= -0.013628;
	  a_m[1][3][0]= -1.951390; b_m[1][3][0]= -1.200000; c_m[1][3][0]= 0.464629; d_m[1][3][0]= 0.150042;  e_m[1][3][0]= -0.020614;

	  a_s[0][0][0]= 0.510511; b_s[0][0][0]= 0.706544;
	  a_s[0][1][0]= 0.498886; b_s[0][1][0]= 0.701416;
	  a_s[0][2][0]= 0.497747; b_s[0][2][0]= 0.697065;
	  a_s[0][3][0]= 0.499272; b_s[0][3][0]= 0.698558;
	  a_s[1][0][0]= 0.451113; b_s[1][0][0]= 0.717888;
	  a_s[1][1][0]= 0.460813; b_s[1][1][0]= 0.684277;
	  a_s[1][2][0]= 0.447519; b_s[1][2][0]= 0.674041;
	  a_s[1][3][0]= 0.444298; b_s[1][3][0]= 0.704832;
	}
      else if(i_bbcsum==3)
	{
	  a_m[0][0][0]= 2.378716;  b_m[0][0][0]= -1.200000; c_m[0][0][0]= 0.126262; d_m[0][0][0]= 0.773359;  e_m[0][0][0]= -0.161434;
	  a_m[0][1][0]= 0.691772;  b_m[0][1][0]= -1.200000; c_m[0][1][0]= 0.167412; d_m[0][1][0]= 0.388664;  e_m[0][1][0]= -0.079305;
	  a_m[0][2][0]= -0.738995; b_m[0][2][0]= -1.200000; c_m[0][2][0]= 0.306567; d_m[0][2][0]= 0.069827;  e_m[0][2][0]= 0.080748;
	  a_m[0][3][0]= -2.460678; b_m[0][3][0]= -1.200000; c_m[0][3][0]= 0.428890; d_m[0][3][0]= -0.224013; e_m[0][3][0]= 0.161618;
	  a_m[1][0][0]= 2.055592;  b_m[1][0][0]= -1.200000; c_m[1][0][0]= 0.100735; d_m[1][0][0]= 0.494326;  e_m[1][0][0]= -0.023182;
	  a_m[1][1][0]= 0.607450;  b_m[1][1][0]= -1.200000; c_m[1][1][0]= 0.161091; d_m[1][1][0]= 0.301990;  e_m[1][1][0]= -0.029783;
	  a_m[1][2][0]= -0.721711; b_m[1][2][0]= -1.200000; c_m[1][2][0]= 0.364276; d_m[1][2][0]= 0.283000;  e_m[1][2][0]= -0.019023;
	  a_m[1][3][0]= -1.872335; b_m[1][3][0]= -1.200000; c_m[1][3][0]= 0.479842; d_m[1][3][0]= 0.179144;  e_m[1][3][0]= -0.030305;

	  a_s[0][0][0]= 0.574664; b_s[0][0][0]= 1.031497;
	  a_s[0][1][0]= 0.585988; b_s[0][1][0]= 0.995877;
	  a_s[0][2][0]= 0.572382; b_s[0][2][0]= 1.001145;
	  a_s[0][3][0]= 0.570020; b_s[0][3][0]= 1.019966;
	  a_s[1][0][0]= 0.486628; b_s[1][0][0]= 0.999571;
	  a_s[1][1][0]= 0.527436; b_s[1][1][0]= 0.936910;
	  a_s[1][2][0]= 0.505052; b_s[1][2][0]= 0.927275;
	  a_s[1][3][0]= 0.488162; b_s[1][3][0]= 0.985782;
	}
      else
	{
	  return -9999;
	}

    }
  else if(b_field==-1)
    {

      if(i_bbcsum==0)
	{
	  a_m[0][0][0]= 2.087683;  b_m[0][0][0]= -1.200000; c_m[0][0][0]= 0.033355; d_m[0][0][0]= 0.421490;  e_m[0][0][0]= -0.021067;
	  a_m[0][1][0]= 0.604436;  b_m[0][1][0]= -1.200000; c_m[0][1][0]= 0.082530; d_m[0][1][0]= 0.210874;  e_m[0][1][0]= -0.020999;
	  a_m[0][2][0]= -0.573706; b_m[0][2][0]= -1.200000; c_m[0][2][0]= 0.287390; d_m[0][2][0]= 0.206776;  e_m[0][2][0]= -0.009463;
	  a_m[0][3][0]= -1.966241; b_m[0][3][0]= -1.200000; c_m[0][3][0]= 0.409072; d_m[0][3][0]= 0.088890;  e_m[0][3][0]= -0.016121;
	  a_m[1][0][0]= 2.354559;  b_m[1][0][0]= -1.200000; c_m[1][0][0]= 0.055247; d_m[1][0][0]= 0.680776;  e_m[1][0][0]= -0.148949;
	  a_m[1][1][0]= 0.737469;  b_m[1][1][0]= -1.200000; c_m[1][1][0]= 0.080829; d_m[1][1][0]= 0.288255;  e_m[1][1][0]= -0.060768;
	  a_m[1][2][0]= -0.844226; b_m[1][2][0]= -1.200000; c_m[1][2][0]= 0.258153; d_m[1][2][0]= 0.014651;  e_m[1][2][0]= 0.074356;
	  a_m[1][3][0]= -2.317693; b_m[1][3][0]= -1.200000; c_m[1][3][0]= 0.348545; d_m[1][3][0]= -0.291527; e_m[1][3][0]= 0.159156;

	  a_s[0][0][0]= 0.487448; b_s[0][0][0]= 0.548573;
	  a_s[0][1][0]= 0.497392; b_s[0][1][0]= 0.533263;
	  a_s[0][2][0]= 0.493394; b_s[0][2][0]= 0.509742;
	  a_s[0][3][0]= 0.480367; b_s[0][3][0]= 0.537903;
	  a_s[1][0][0]= 0.460304; b_s[1][0][0]= 0.534494;
	  a_s[1][1][0]= 0.454748; b_s[1][1][0]= 0.517839;
	  a_s[1][2][0]= 0.447710; b_s[1][2][0]= 0.515554;
	  a_s[1][3][0]= 0.445554; b_s[1][3][0]= 0.526277;
	}
      else if(i_bbcsum==1)
	{
	  a_m[0][0][0]= 2.079141;  b_m[0][0][0]= -1.200000; c_m[0][0][0]= 0.055726; d_m[0][0][0]= 0.443747;  e_m[0][0][0]= -0.022461;
	  a_m[0][1][0]= 0.599726;  b_m[0][1][0]= -1.200000; c_m[0][1][0]= 0.104760; d_m[0][1][0]= 0.234546;  e_m[0][1][0]= -0.022603;
	  a_m[0][2][0]= -0.557948; b_m[0][2][0]= -1.200000; c_m[0][2][0]= 0.305257; d_m[0][2][0]= 0.225969;  e_m[0][2][0]= -0.009015;
	  a_m[0][3][0]= -1.989063; b_m[0][3][0]= -1.200000; c_m[0][3][0]= 0.431682; d_m[0][3][0]= 0.110053;  e_m[0][3][0]= -0.016774;
	  a_m[1][0][0]= 2.296083;  b_m[1][0][0]= -1.200000; c_m[1][0][0]= 0.081923; d_m[1][0][0]= 0.704886;  e_m[1][0][0]= -0.151956;
	  a_m[1][1][0]= 0.717548;  b_m[1][1][0]= -1.200000; c_m[1][1][0]= 0.105022; d_m[1][1][0]= 0.310540;  e_m[1][1][0]= -0.061330;
	  a_m[1][2][0]= -0.774333; b_m[1][2][0]= -1.200000; c_m[1][2][0]= 0.269243; d_m[1][2][0]= 0.032133;  e_m[1][2][0]= 0.077104;
	  a_m[1][3][0]= -2.300644; b_m[1][3][0]= -1.200000; c_m[1][3][0]= 0.366553; d_m[1][3][0]= -0.268503; e_m[1][3][0]= 0.156606;

	  a_s[0][0][0]= 0.485701; b_s[0][0][0]= 0.588248;
	  a_s[0][1][0]= 0.495350; b_s[0][1][0]= 0.571677;
	  a_s[0][2][0]= 0.490801; b_s[0][2][0]= 0.551031;
	  a_s[0][3][0]= 0.480749; b_s[0][3][0]= 0.576243;
	  a_s[1][0][0]= 0.449635; b_s[1][0][0]= 0.585712;
	  a_s[1][1][0]= 0.450724; b_s[1][1][0]= 0.559616;
	  a_s[1][2][0]= 0.448933; b_s[1][2][0]= 0.551616;
	  a_s[1][3][0]= 0.443077; b_s[1][3][0]= 0.567927;
	}
      else if(i_bbcsum==2)
	{
	  a_m[0][0][0]= 2.056704;  b_m[0][0][0]= -1.200000; c_m[0][0][0]= 0.086820; d_m[0][0][0]= 0.475878;  e_m[0][0][0]= -0.023743;
	  a_m[0][1][0]= 0.651234;  b_m[0][1][0]= -1.200000; c_m[0][1][0]= 0.132178; d_m[0][1][0]= 0.267346;  e_m[0][1][0]= -0.022415;
	  a_m[0][2][0]= -0.530084; b_m[0][2][0]= -1.200000; c_m[0][2][0]= 0.328390; d_m[0][2][0]= 0.250597;  e_m[0][2][0]= -0.008348;
	  a_m[0][3][0]= -1.966932; b_m[0][3][0]= -1.200000; c_m[0][3][0]= 0.458207; d_m[0][3][0]= 0.136317;  e_m[0][3][0]= -0.014745;
	  a_m[1][0][0]= 2.332330;  b_m[1][0][0]= -1.200000; c_m[1][0][0]= 0.106292; d_m[1][0][0]= 0.734674;  e_m[1][0][0]= -0.150520;
	  a_m[1][1][0]= 0.677230;  b_m[1][1][0]= -1.200000; c_m[1][1][0]= 0.140139; d_m[1][1][0]= 0.344569;  e_m[1][1][0]= -0.061355;
	  a_m[1][2][0]= -0.752951; b_m[1][2][0]= -1.200000; c_m[1][2][0]= 0.296430; d_m[1][2][0]= 0.060285;  e_m[1][2][0]= 0.074932;
	  a_m[1][3][0]= -2.309731; b_m[1][3][0]= -1.200000; c_m[1][3][0]= 0.396971; d_m[1][3][0]= -0.241286; e_m[1][3][0]= 0.159132;

	  a_s[0][0][0]= 0.499548; b_s[0][0][0]= 0.708862;
	  a_s[0][1][0]= 0.498171; b_s[0][1][0]= 0.703757;
	  a_s[0][2][0]= 0.489194; b_s[0][2][0]= 0.690576;
	  a_s[0][3][0]= 0.495665; b_s[0][3][0]= 0.696359;
	  a_s[1][0][0]= 0.462805; b_s[1][0][0]= 0.714128;
	  a_s[1][1][0]= 0.455143; b_s[1][1][0]= 0.689110;
	  a_s[1][2][0]= 0.452194; b_s[1][2][0]= 0.683749;
	  a_s[1][3][0]= 0.446896; b_s[1][3][0]= 0.706209;
	}
      else if(i_bbcsum==3)
	{
	  a_m[0][0][0]= 2.157622;  b_m[0][0][0]= -1.200000; c_m[0][0][0]= 0.100269; d_m[0][0][0]= 0.499859;  e_m[0][0][0]= -0.024220;
	  a_m[0][1][0]= 0.471619;  b_m[0][1][0]= -1.200000; c_m[0][1][0]= 0.167500; d_m[0][1][0]= 0.303928;  e_m[0][1][0]= -0.028038;
	  a_m[0][2][0]= -0.628458; b_m[0][2][0]= -1.200000; c_m[0][2][0]= 0.357365; d_m[0][2][0]= 0.267679;  e_m[0][2][0]= -0.009177;
	  a_m[0][3][0]= -2.022419; b_m[0][3][0]= -1.200000; c_m[0][3][0]= 0.485138; d_m[0][3][0]= 0.154309;  e_m[0][3][0]= -0.012546;
	  a_m[1][0][0]= 2.240420;  b_m[1][0][0]= -1.200000; c_m[1][0][0]= 0.133636; d_m[1][0][0]= 0.761535;  e_m[1][0][0]= -0.157154;
	  a_m[1][1][0]= 0.923207;  b_m[1][1][0]= -1.200000; c_m[1][1][0]= 0.141196; d_m[1][1][0]= 0.377301;  e_m[1][1][0]= -0.068932;
	  a_m[1][2][0]= -0.631151; b_m[1][2][0]= -1.200000; c_m[1][2][0]= 0.298838; d_m[1][2][0]= 0.070676;  e_m[1][2][0]= 0.080979;
	  a_m[1][3][0]= -2.232214; b_m[1][3][0]= -1.200000; c_m[1][3][0]= 0.408669; d_m[1][3][0]= -0.224847; e_m[1][3][0]= 0.163165;

	  a_s[0][0][0]= 0.562405; b_s[0][0][0]= 1.031229;
	  a_s[0][1][0]= 0.595697; b_s[0][1][0]= 0.984049;
	  a_s[0][2][0]= 0.610250; b_s[0][2][0]= 0.948426;
	  a_s[0][3][0]= 0.556901; b_s[0][3][0]= 1.025199;
	  a_s[1][0][0]= 0.475621; b_s[1][0][0]= 1.025708;
	  a_s[1][1][0]= 0.527152; b_s[1][1][0]= 0.932826;
	  a_s[1][2][0]= 0.500168; b_s[1][2][0]= 0.954520;
	  a_s[1][3][0]= 0.491779; b_s[1][3][0]= 0.992993;
	}
      else
	{
	  return -9999;
	}

    }
  else
    {
      return -9999;
    }

  float m_dz = 0;
  float s_dz = 1;
  
  if(pt>1.5)       m_dz =  a_m[i_ch][i_zed][i_phi] * exp(b_m[i_ch][i_zed][i_phi]*pt) + c_m[i_ch][i_zed][i_phi];
  else if(pt<=1.5) m_dz =  d_m[i_ch][i_zed][i_phi] + e_m[i_ch][i_zed][i_phi]*pt;

  s_dz = sqrt( a_s[i_ch][i_zed][i_phi]*a_s[i_ch][i_zed][i_phi]/pt/pt + b_s[i_ch][i_zed][i_phi]*b_s[i_ch][i_zed][i_phi] );

  float sdz  = -9999;
  if(s_dz>0) sdz = (dz - m_dz)/s_dz;      

  return sdz;

}


// Getpc3sdz
float MatchrecalRecoRun5::Getpc3sdz(const short i_bbcsum, const short i_ch, const short i_zed, const short i_phi, const float pt, const float dz)
{

  if(dz==-9999) return -9999;

  // Parameters
  float a_m[2][4][2],b_m[2][4][2],c_m[2][4][2], d_m[2][4][2],e_m[2][4][2];
  float a_s[2][4][2],b_s[2][4][2];

  if(b_field==1)
    {

      if(i_bbcsum==0)
	{
	  a_m[0][0][0]= 3.369143;  b_m[0][0][0]= -1.200000; c_m[0][0][0]= 0.016102;  d_m[0][0][0]= 0.863030;  e_m[0][0][0]= -0.180608;
	  a_m[0][0][1]= 2.989062;  b_m[0][0][1]= -1.200000; c_m[0][0][1]= 0.073802;  d_m[0][0][1]= 0.598403;  e_m[0][0][1]= -0.006840;
	  a_m[0][1][0]= 1.005143;  b_m[0][1][0]= -1.200000; c_m[0][1][0]= 0.136584;  d_m[0][1][0]= 0.408097;  e_m[0][1][0]= -0.072726;
	  a_m[0][1][1]= 0.888351;  b_m[0][1][1]= -1.200000; c_m[0][1][1]= 0.451646;  d_m[0][1][1]= 0.572620;  e_m[0][1][1]= 0.016094;
	  a_m[0][2][0]= -1.127080; b_m[0][2][0]= -1.200000; c_m[0][2][0]= 0.334021;  d_m[0][2][0]= 0.052701;  e_m[0][2][0]= 0.066634;
	  a_m[0][2][1]= -0.978176; b_m[0][2][1]= -1.200000; c_m[0][2][1]= 0.783555;  d_m[0][2][1]= 0.523981;  e_m[0][2][1]= 0.068966;
	  a_m[0][3][0]= -3.382529; b_m[0][3][0]= -1.200000; c_m[0][3][0]= 0.522782;  d_m[0][3][0]= -0.305376; e_m[0][3][0]= 0.165980;
	  a_m[0][3][1]= -3.117532; b_m[0][3][1]= -1.200000; c_m[0][3][1]= 1.290850;  d_m[0][3][1]= 0.619006;  e_m[0][3][1]= 0.093256;
	  a_m[1][0][0]= 3.094170;  b_m[1][0][0]= -1.200000; c_m[1][0][0]= -0.004896; d_m[1][0][0]= 0.587275;  e_m[1][0][0]= -0.043450;
	  a_m[1][0][1]= 3.388643;  b_m[1][0][1]= -1.200000; c_m[1][0][1]= 0.124355;  d_m[1][0][1]= 0.985158;  e_m[1][0][1]= -0.187518;
	  a_m[1][1][0]= 1.018896;  b_m[1][1][0]= -1.200000; c_m[1][1][0]= 0.134816;  d_m[1][1][0]= 0.367842;  e_m[1][1][0]= -0.046799;
	  a_m[1][1][1]= 1.026082;  b_m[1][1][1]= -1.200000; c_m[1][1][1]= 0.476398;  d_m[1][1][1]= 0.745991;  e_m[1][1][1]= -0.065720;
	  a_m[1][2][0]= -0.921979; b_m[1][2][0]= -1.200000; c_m[1][2][0]= 0.374377;  d_m[1][2][0]= 0.250506;  e_m[1][2][0]= -0.016356;
	  a_m[1][2][1]= -1.127585; b_m[1][2][1]= -1.200000; c_m[1][2][1]= 0.776825;  d_m[1][2][1]= 0.418656;  e_m[1][2][1]= 0.118641;
	  a_m[1][3][0]= -2.916258; b_m[1][3][0]= -1.200000; c_m[1][3][0]= 0.586723;  d_m[1][3][0]= 0.128251;  e_m[1][3][0]= -0.031226;
	  a_m[1][3][1]= -3.496411; b_m[1][3][1]= -1.200000; c_m[1][3][1]= 1.256593;  d_m[1][3][1]= 0.304985;  e_m[1][3][1]= 0.243756;

	  a_s[0][0][0]= 0.805497; b_s[0][0][0]= 0.691732;
	  a_s[0][0][1]= 0.781698; b_s[0][0][1]= 0.696356;
	  a_s[0][1][0]= 0.808613; b_s[0][1][0]= 0.676847;
	  a_s[0][1][1]= 0.784718; b_s[0][1][1]= 0.697686;
	  a_s[0][2][0]= 0.797709; b_s[0][2][0]= 0.689246;
	  a_s[0][2][1]= 0.779253; b_s[0][2][1]= 0.716388;
	  a_s[0][3][0]= 0.784582; b_s[0][3][0]= 0.693295;
	  a_s[0][3][1]= 0.770643; b_s[0][3][1]= 0.749069;
	  a_s[1][0][0]= 0.782665; b_s[1][0][0]= 0.690719;
	  a_s[1][0][1]= 0.768322; b_s[1][0][1]= 0.712272;
	  a_s[1][1][0]= 0.783740; b_s[1][1][0]= 0.672059;
	  a_s[1][1][1]= 0.746096; b_s[1][1][1]= 0.695293;
	  a_s[1][2][0]= 0.752705; b_s[1][2][0]= 0.672075;
	  a_s[1][2][1]= 0.755580; b_s[1][2][1]= 0.719401;
	  a_s[1][3][0]= 0.753130; b_s[1][3][0]= 0.705445;
	  a_s[1][3][1]= 0.744022; b_s[1][3][1]= 0.752204;
	}
      else if(i_bbcsum==1)
	{
	  a_m[0][0][0]= 3.351082;  b_m[0][0][0]= -1.200000; c_m[0][0][0]= 0.051712; d_m[0][0][0]= 0.899580;  e_m[0][0][0]= -0.183796;
	  a_m[0][0][1]= 3.022037;  b_m[0][0][1]= -1.200000; c_m[0][0][1]= 0.102127; d_m[0][0][1]= 0.635576;  e_m[0][0][1]= -0.009814;
	  a_m[0][1][0]= 1.005576;  b_m[0][1][0]= -1.200000; c_m[0][1][0]= 0.168621; d_m[0][1][0]= 0.444551;  e_m[0][1][0]= -0.074986;
	  a_m[0][1][1]= 0.906096;  b_m[0][1][1]= -1.200000; c_m[0][1][1]= 0.482791; d_m[0][1][1]= 0.607703;  e_m[0][1][1]= 0.015951;
	  a_m[0][2][0]= -1.092862; b_m[0][2][0]= -1.200000; c_m[0][2][0]= 0.363960; d_m[0][2][0]= 0.082678;  e_m[0][2][0]= 0.067666;
	  a_m[0][2][1]= -1.133190; b_m[0][2][1]= -1.200000; c_m[0][2][1]= 0.826662; d_m[0][2][1]= 0.557607;  e_m[0][2][1]= 0.065787;
	  a_m[0][3][0]= -3.376101; b_m[0][3][0]= -1.200000; c_m[0][3][0]= 0.555536; d_m[0][3][0]= -0.269726; e_m[0][3][0]= 0.163656;
	  a_m[0][3][1]= -3.107986; b_m[0][3][1]= -1.200000; c_m[0][3][1]= 1.321719; d_m[0][3][1]= 0.651213;  e_m[0][3][1]= 0.092822;
	  a_m[1][0][0]= 3.072845;  b_m[1][0][0]= -1.200000; c_m[1][0][0]= 0.027358; d_m[1][0][0]= 0.617708;  e_m[1][0][0]= -0.042230;
	  a_m[1][0][1]= 3.330568;  b_m[1][0][1]= -1.200000; c_m[1][0][1]= 0.164511; d_m[1][0][1]= 1.023759;  e_m[1][0][1]= -0.192864;
	  a_m[1][1][0]= 0.969720;  b_m[1][1][0]= -1.200000; c_m[1][1][0]= 0.174130; d_m[1][1][0]= 0.402418;  e_m[1][1][0]= -0.047964;
	  a_m[1][1][1]= 1.056797;  b_m[1][1][1]= -1.200000; c_m[1][1][1]= 0.506337; d_m[1][1][1]= 0.781388;  e_m[1][1][1]= -0.066851;
	  a_m[1][2][0]= -0.880186; b_m[1][2][0]= -1.200000; c_m[1][2][0]= 0.402631; d_m[1][2][0]= 0.284510;  e_m[1][2][0]= -0.018114;
	  a_m[1][2][1]= -1.155869; b_m[1][2][1]= -1.200000; c_m[1][2][1]= 0.810553; d_m[1][2][1]= 0.448424;  e_m[1][2][1]= 0.119876;
	  a_m[1][3][0]= -2.877860; b_m[1][3][0]= -1.200000; c_m[1][3][0]= 0.615393; d_m[1][3][0]= 0.163331;  e_m[1][3][0]= -0.033425;
	  a_m[1][3][1]= -3.450016; b_m[1][3][1]= -1.200000; c_m[1][3][1]= 1.283800; d_m[1][3][1]= 0.338041;  e_m[1][3][1]= 0.244102;

	  a_s[0][0][0]= 0.802922; b_s[0][0][0]= 0.756103;
	  a_s[0][0][1]= 0.780953; b_s[0][0][1]= 0.761898;
	  a_s[0][1][0]= 0.802527; b_s[0][1][0]= 0.744273;
	  a_s[0][1][1]= 0.780863; b_s[0][1][1]= 0.763237;
	  a_s[0][2][0]= 0.792881; b_s[0][2][0]= 0.752987;
	  a_s[0][2][1]= 0.772764; b_s[0][2][1]= 0.781684;
	  a_s[0][3][0]= 0.780668; b_s[0][3][0]= 0.758924;
	  a_s[0][3][1]= 0.765384; b_s[0][3][1]= 0.810653;
	  a_s[1][0][0]= 0.758295; b_s[1][0][0]= 0.757934;
	  a_s[1][0][1]= 0.761831; b_s[1][0][1]= 0.780522;
	  a_s[1][1][0]= 0.778496; b_s[1][1][0]= 0.719802;
	  a_s[1][1][1]= 0.721568; b_s[1][1][1]= 0.751334;
	  a_s[1][2][0]= 0.737898; b_s[1][2][0]= 0.734232;
	  a_s[1][2][1]= 0.752899; b_s[1][2][1]= 0.747976;
	  a_s[1][3][0]= 0.737922; b_s[1][3][0]= 0.770426;
	  a_s[1][3][1]= 0.737167; b_s[1][3][1]= 0.815137;
	}
      else if(i_bbcsum==2)
	{
	  a_m[0][0][0]= 3.321944;  b_m[0][0][0]= -1.200000; c_m[0][0][0]= 0.096416; d_m[0][0][0]= 0.954382;  e_m[0][0][0]= -0.192084;
	  a_m[0][0][1]= 2.962733;  b_m[0][0][1]= -1.200000; c_m[0][0][1]= 0.149396; d_m[0][0][1]= 0.683440;  e_m[0][0][1]= -0.012887;
	  a_m[0][1][0]= 0.939567;  b_m[0][1][0]= -1.200000; c_m[0][1][0]= 0.223213; d_m[0][1][0]= 0.499487;  e_m[0][1][0]= -0.078033;
	  a_m[0][1][1]= 0.882522;  b_m[0][1][1]= -1.200000; c_m[0][1][1]= 0.535637; d_m[0][1][1]= 0.662225;  e_m[0][1][1]= 0.012301;
	  a_m[0][2][0]= -1.129316; b_m[0][2][0]= -1.200000; c_m[0][2][0]= 0.407981; d_m[0][2][0]= 0.122586;  e_m[0][2][0]= 0.068338;
	  a_m[0][2][1]= -0.972436; b_m[0][2][1]= -1.200000; c_m[0][2][1]= 0.856363; d_m[0][2][1]= 0.597314;  e_m[0][2][1]= 0.066636;
	  a_m[0][3][0]= -3.338620; b_m[0][3][0]= -1.200000; c_m[0][3][0]= 0.600068; d_m[0][3][0]= -0.224916; e_m[0][3][0]= 0.165093;
	  a_m[0][3][1]= -3.087468; b_m[0][3][1]= -1.200000; c_m[0][3][1]= 1.367859; d_m[0][3][1]= 0.697546;  e_m[0][3][1]= 0.094010;
	  a_m[1][0][0]= 3.112084;  b_m[1][0][0]= -1.200000; c_m[1][0][0]= 0.066881; d_m[1][0][0]= 0.666497;  e_m[1][0][0]= -0.046548;
	  a_m[1][0][1]= 3.432842;  b_m[1][0][1]= -1.200000; c_m[1][0][1]= 0.197044; d_m[1][0][1]= 1.067012;  e_m[1][0][1]= -0.191514;
	  a_m[1][1][0]= 0.961908;  b_m[1][1][0]= -1.200000; c_m[1][1][0]= 0.221989; d_m[1][1][0]= 0.455908;  e_m[1][1][0]= -0.050709;
	  a_m[1][1][1]= 1.044008;  b_m[1][1][1]= -1.200000; c_m[1][1][1]= 0.559926; d_m[1][1][1]= 0.835357;  e_m[1][1][1]= -0.070493;
	  a_m[1][2][0]= -0.885534; b_m[1][2][0]= -1.200000; c_m[1][2][0]= 0.441958; d_m[1][2][0]= 0.325046;  e_m[1][2][0]= -0.018033;
	  a_m[1][2][1]= -1.221438; b_m[1][2][1]= -1.200000; c_m[1][2][1]= 0.859694; d_m[1][2][1]= 0.486023;  e_m[1][2][1]= 0.123908;
	  a_m[1][3][0]= -2.904060; b_m[1][3][0]= -1.200000; c_m[1][3][0]= 0.667810; d_m[1][3][0]= 0.210273;  e_m[1][3][0]= -0.033065;
	  a_m[1][3][1]= -3.470814; b_m[1][3][1]= -1.200000; c_m[1][3][1]= 1.336415; d_m[1][3][1]= 0.383271;  e_m[1][3][1]= 0.246059;

	  a_s[0][0][0]= 0.819354; b_s[0][0][0]= 0.945860;
	  a_s[0][0][1]= 0.802513; b_s[0][0][1]= 0.947425;
	  a_s[0][1][0]= 0.815606; b_s[0][1][0]= 0.932626;
	  a_s[0][1][1]= 0.794334; b_s[0][1][1]= 0.948612;
	  a_s[0][2][0]= 0.809573; b_s[0][2][0]= 0.936685;
	  a_s[0][2][1]= 0.789985; b_s[0][2][1]= 0.959679;
	  a_s[0][3][0]= 0.803163; b_s[0][3][0]= 0.939542;
	  a_s[0][3][1]= 0.784967; b_s[0][3][1]= 0.983379;
	  a_s[1][0][0]= 0.765726; b_s[1][0][0]= 0.951602;
	  a_s[1][0][1]= 0.767932; b_s[1][0][1]= 0.968336;
	  a_s[1][1][0]= 0.764567; b_s[1][1][0]= 0.918703;
	  a_s[1][1][1]= 0.724650; b_s[1][1][1]= 0.936344;
	  a_s[1][2][0]= 0.747083; b_s[1][2][0]= 0.919749;
	  a_s[1][2][1]= 0.764468; b_s[1][2][1]= 0.928437;
	  a_s[1][3][0]= 0.747772; b_s[1][3][0]= 0.959614;
	  a_s[1][3][1]= 0.736691; b_s[1][3][1]= 0.993728;
	}
      else if(i_bbcsum==3)
	{
	  a_m[0][0][0]= 3.435788;  b_m[0][0][0]= -1.200000; c_m[0][0][0]= 0.112593; d_m[0][0][0]= 0.995752;  e_m[0][0][0]= -0.199996;
	  a_m[0][0][1]= 2.916374;  b_m[0][0][1]= -1.200000; c_m[0][0][1]= 0.182043; d_m[0][0][1]= 0.710488;  e_m[0][0][1]= -0.006859;
	  a_m[0][1][0]= 1.094709;  b_m[0][1][0]= -1.200000; c_m[0][1][0]= 0.250303; d_m[0][1][0]= 0.557951;  e_m[0][1][0]= -0.098780;
	  a_m[0][1][1]= 0.816678;  b_m[0][1][1]= -1.200000; c_m[0][1][1]= 0.577515; d_m[0][1][1]= 0.702585;  e_m[0][1][1]= 0.008018;
	  a_m[0][2][0]= -1.053016; b_m[0][2][0]= -1.200000; c_m[0][2][0]= 0.419417; d_m[0][2][0]= 0.129695;  e_m[0][2][0]= 0.081669;
	  a_m[0][2][1]= -1.150406; b_m[0][2][1]= -1.200000; c_m[0][2][1]= 0.895611; d_m[0][2][1]= 0.601268;  e_m[0][2][1]= 0.083334;
	  a_m[0][3][0]= -3.316591; b_m[0][3][0]= -1.200000; c_m[0][3][0]= 0.624481; d_m[0][3][0]= -0.206705; e_m[0][3][0]= 0.172576;
	  a_m[0][3][1]= -2.967278; b_m[0][3][1]= -1.200000; c_m[0][3][1]= 1.374600; d_m[0][3][1]= 0.717748;  e_m[0][3][1]= 0.106088;
	  a_m[1][0][0]= 3.162979;  b_m[1][0][0]= -1.200000; c_m[1][0][0]= 0.087954; d_m[1][0][0]= 0.684907;  e_m[1][0][0]= -0.039744;
	  a_m[1][0][1]= 3.376302;  b_m[1][0][1]= -1.200000; c_m[1][0][1]= 0.230772; d_m[1][0][1]= 1.099208;  e_m[1][0][1]= -0.196942;
	  a_m[1][1][0]= 1.114992;  b_m[1][1][0]= -1.200000; c_m[1][1][0]= 0.240895; d_m[1][1][0]= 0.498673;  e_m[1][1][0]= -0.055875;
	  a_m[1][1][1]= 1.094153;  b_m[1][1][1]= -1.200000; c_m[1][1][1]= 0.598188; d_m[1][1][1]= 0.880443;  e_m[1][1][1]= -0.075786;
	  a_m[1][2][0]= -1.224447; b_m[1][2][0]= -1.200000; c_m[1][2][0]= 0.500389; d_m[1][2][0]= 0.348990;  e_m[1][2][0]= -0.016487;
	  a_m[1][2][1]= -1.423145; b_m[1][2][1]= -1.200000; c_m[1][2][1]= 0.894527; d_m[1][2][1]= 0.485290;  e_m[1][2][1]= 0.147777;
	  a_m[1][3][0]= -2.744024; b_m[1][3][0]= -1.200000; c_m[1][3][0]= 0.687212; d_m[1][3][0]= 0.264281;  e_m[1][3][0]= -0.054348;
	  a_m[1][3][1]= -3.573360; b_m[1][3][1]= -1.200000; c_m[1][3][1]= 1.375292; d_m[1][3][1]= 0.408659;  e_m[1][3][1]= 0.248734;

	  a_s[0][0][0]= 0.904659; b_s[0][0][0]= 1.425746;
	  a_s[0][0][1]= 0.889370; b_s[0][0][1]= 1.436848;
	  a_s[0][1][0]= 0.926846; b_s[0][1][0]= 1.380905;
	  a_s[0][1][1]= 0.922072; b_s[0][1][1]= 1.382631;
	  a_s[0][2][0]= 0.928616; b_s[0][2][0]= 1.366226;
	  a_s[0][2][1]= 0.879680; b_s[0][2][1]= 1.430544;
	  a_s[0][3][0]= 0.900252; b_s[0][3][0]= 1.411130;
	  a_s[0][3][1]= 0.877325; b_s[0][3][1]= 1.455813;
	  a_s[1][0][0]= 0.818724; b_s[1][0][0]= 1.377816;
	  a_s[1][0][1]= 0.772223; b_s[1][0][1]= 1.429560;
	  a_s[1][1][0]= 0.867973; b_s[1][1][0]= 1.280765;
	  a_s[1][1][1]= 0.833366; b_s[1][1][1]= 1.302593;
	  a_s[1][2][0]= 0.837835; b_s[1][2][0]= 1.285017;
	  a_s[1][2][1]= 0.820939; b_s[1][2][1]= 1.310229;
	  a_s[1][3][0]= 0.803327; b_s[1][3][0]= 1.376706;
	  a_s[1][3][1]= 0.738691; b_s[1][3][1]= 1.446805;
	}
      else
	{
	  return -9999;
	}

    }
  else if(b_field==-1)
    {

      if(i_bbcsum==0)
	{
	  a_m[0][0][0]= 3.157592;  b_m[0][0][0]= -1.200000; c_m[0][0][0]= -0.003781; d_m[0][0][0]= 0.583885;  e_m[0][0][0]= -0.036777;
	  a_m[0][0][1]= 3.354224;  b_m[0][0][1]= -1.200000; c_m[0][0][1]= 0.093286;  d_m[0][0][1]= 0.954147;  e_m[0][0][1]= -0.187598;
	  a_m[0][1][0]= 1.019290;  b_m[0][1][0]= -1.200000; c_m[0][1][0]= 0.142444;  d_m[0][1][0]= 0.363995;  e_m[0][1][0]= -0.038449;
	  a_m[0][1][1]= 1.103036;  b_m[0][1][1]= -1.200000; c_m[0][1][1]= 0.443633;  d_m[0][1][1]= 0.716391;  e_m[0][1][1]= -0.060288;
	  a_m[0][2][0]= -0.928187; b_m[0][2][0]= -1.200000; c_m[0][2][0]= 0.372272;  d_m[0][2][0]= 0.242764;  e_m[0][2][0]= -0.013279;
	  a_m[0][2][1]= -1.136168; b_m[0][2][1]= -1.200000; c_m[0][2][1]= 0.759242;  d_m[0][2][1]= 0.394065;  e_m[0][2][1]= 0.121597;
	  a_m[0][3][0]= -2.968799; b_m[0][3][0]= -1.200000; c_m[0][3][0]= 0.590625;  d_m[0][3][0]= 0.115866;  e_m[0][3][0]= -0.023453;
	  a_m[0][3][1]= -3.454301; b_m[0][3][1]= -1.200000; c_m[0][3][1]= 1.248973;  d_m[0][3][1]= 0.286737;  e_m[0][3][1]= 0.251489;
	  a_m[1][0][0]= 3.345997;  b_m[1][0][0]= -1.200000; c_m[1][0][0]= 0.021152;  d_m[1][0][0]= 0.863986;  e_m[1][0][0]= -0.176163;
	  a_m[1][0][1]= 3.040680;  b_m[1][0][1]= -1.200000; c_m[1][0][1]= 0.035025;  d_m[1][0][1]= 0.552722;  e_m[1][0][1]= 0.000069;
	  a_m[1][1][0]= 1.080173;  b_m[1][1][0]= -1.200000; c_m[1][1][0]= 0.133680;  d_m[1][1][0]= 0.414620;  e_m[1][1][0]= -0.072999;
	  a_m[1][1][1]= 0.899024;  b_m[1][1][1]= -1.200000; c_m[1][1][1]= 0.424494;  d_m[1][1][1]= 0.538975;  e_m[1][1][1]= 0.021563;
	  a_m[1][2][0]= -1.123255; b_m[1][2][0]= -1.200000; c_m[1][2][0]= 0.338453;  d_m[1][2][0]= 0.055940;  e_m[1][2][0]= 0.067309;
	  a_m[1][2][1]= -1.022368; b_m[1][2][1]= -1.200000; c_m[1][2][1]= 0.768675;  d_m[1][2][1]= 0.502094;  e_m[1][2][1]= 0.068473;
	  a_m[1][3][0]= -3.296735; b_m[1][3][0]= -1.200000; c_m[1][3][0]= 0.522339;  d_m[1][3][0]= -0.303999; e_m[1][3][0]= 0.168949;
	  a_m[1][3][1]= -3.119996; b_m[1][3][1]= -1.200000; c_m[1][3][1]= 1.279792;  d_m[1][3][1]= 0.608086;  e_m[1][3][1]= 0.095173;

	  a_s[0][0][0]= 0.806255; b_s[0][0][0]= 0.683221;
	  a_s[0][0][1]= 0.786242; b_s[0][0][1]= 0.706172;
	  a_s[0][1][0]= 0.808939; b_s[0][1][0]= 0.685911;
	  a_s[0][1][1]= 0.776522; b_s[0][1][1]= 0.705276;
	  a_s[0][2][0]= 0.797220; b_s[0][2][0]= 0.693092;
	  a_s[0][2][1]= 0.777196; b_s[0][2][1]= 0.723532;
	  a_s[0][3][0]= 0.793717; b_s[0][3][0]= 0.705428;
	  a_s[0][3][1]= 0.762930; b_s[0][3][1]= 0.751740;
	  a_s[1][0][0]= 0.785819; b_s[1][0][0]= 0.698085;
	  a_s[1][0][1]= 0.766332; b_s[1][0][1]= 0.700093;
	  a_s[1][1][0]= 0.778617; b_s[1][1][0]= 0.673354;
	  a_s[1][1][1]= 0.760088; b_s[1][1][1]= 0.676207;
	  a_s[1][2][0]= 0.751347; b_s[1][2][0]= 0.672761;
	  a_s[1][2][1]= 0.764099; b_s[1][2][1]= 0.732073;
	  a_s[1][3][0]= 0.742036; b_s[1][3][0]= 0.698385;
	  a_s[1][3][1]= 0.755655; b_s[1][3][1]= 0.757821;
	}
      else if(i_bbcsum==1)
	{
	  a_m[0][0][0]= 3.115485;  b_m[0][0][0]= -1.200000; c_m[0][0][0]= 0.029687; d_m[0][0][0]= 0.615440;  e_m[0][0][0]= -0.038452;
	  a_m[0][0][1]= 3.366701;  b_m[0][0][1]= -1.200000; c_m[0][0][1]= 0.123239; d_m[0][0][1]= 0.989036;  e_m[0][0][1]= -0.190786;
	  a_m[0][1][0]= 0.985221;  b_m[0][1][0]= -1.200000; c_m[0][1][0]= 0.177343; d_m[0][1][0]= 0.401091;  e_m[0][1][0]= -0.044550;
	  a_m[0][1][1]= 1.096555;  b_m[0][1][1]= -1.200000; c_m[0][1][1]= 0.472914; d_m[0][1][1]= 0.752317;  e_m[0][1][1]= -0.064236;
	  a_m[0][2][0]= -0.865198; b_m[0][2][0]= -1.200000; c_m[0][2][0]= 0.394530; d_m[0][2][0]= 0.267960;  e_m[0][2][0]= -0.008923;
	  a_m[0][2][1]= -1.187221; b_m[0][2][1]= -1.200000; c_m[0][2][1]= 0.791701; d_m[0][2][1]= 0.423365;  e_m[0][2][1]= 0.121169;
	  a_m[0][3][0]= -2.934873; b_m[0][3][0]= -1.200000; c_m[0][3][0]= 0.616993; d_m[0][3][0]= 0.143610;  e_m[0][3][0]= -0.021854;
	  a_m[0][3][1]= -3.453008; b_m[0][3][1]= -1.200000; c_m[0][3][1]= 1.276710; d_m[0][3][1]= 0.314785;  e_m[0][3][1]= 0.251891;
	  a_m[1][0][0]= 3.313653;  b_m[1][0][0]= -1.200000; c_m[1][0][0]= 0.054846; d_m[1][0][0]= 0.898929;  e_m[1][0][0]= -0.182622;
	  a_m[1][0][1]= 3.017569;  b_m[1][0][1]= -1.200000; c_m[1][0][1]= 0.066291; d_m[1][0][1]= 0.579426;  e_m[1][0][1]= 0.004046;
	  a_m[1][1][0]= 1.018079;  b_m[1][1][0]= -1.200000; c_m[1][1][0]= 0.172695; d_m[1][1][0]= 0.449067;  e_m[1][1][0]= -0.076286;
	  a_m[1][1][1]= 0.909839;  b_m[1][1][1]= -1.200000; c_m[1][1][1]= 0.453037; d_m[1][1][1]= 0.567342;  e_m[1][1][1]= 0.024927;
	  a_m[1][2][0]= -1.095484; b_m[1][2][0]= -1.200000; c_m[1][2][0]= 0.363087; d_m[1][2][0]= 0.084997;  e_m[1][2][0]= 0.067221;
	  a_m[1][2][1]= -1.041284; b_m[1][2][1]= -1.200000; c_m[1][2][1]= 0.796601; d_m[1][2][1]= 0.528547;  e_m[1][2][1]= 0.070886;
	  a_m[1][3][0]= -3.267867; b_m[1][3][0]= -1.200000; c_m[1][3][0]= 0.548796; d_m[1][3][0]= -0.268616; e_m[1][3][0]= 0.164552;
	  a_m[1][3][1]= -3.210191; b_m[1][3][1]= -1.200000; c_m[1][3][1]= 1.319062; d_m[1][3][1]= 0.640770;  e_m[1][3][1]= 0.091702;

	  a_s[0][0][0]= 0.802256; b_s[0][0][0]= 0.750629;
	  a_s[0][0][1]= 0.782657; b_s[0][0][1]= 0.772082;
	  a_s[0][1][0]= 0.804367; b_s[0][1][0]= 0.750840;
	  a_s[0][1][1]= 0.772241; b_s[0][1][1]= 0.770112;
	  a_s[0][2][0]= 0.791723; b_s[0][2][0]= 0.756625;
	  a_s[0][2][1]= 0.769553; b_s[0][2][1]= 0.788674;
	  a_s[0][3][0]= 0.793491; b_s[0][3][0]= 0.763683;
	  a_s[0][3][1]= 0.759259; b_s[0][3][1]= 0.812860;
	  a_s[1][0][0]= 0.766932; b_s[1][0][0]= 0.761887;
	  a_s[1][0][1]= 0.761396; b_s[1][0][1]= 0.768006;
	  a_s[1][1][0]= 0.779417; b_s[1][1][0]= 0.716702;
	  a_s[1][1][1]= 0.730085; b_s[1][1][1]= 0.743796;
	  a_s[1][2][0]= 0.738025; b_s[1][2][0]= 0.734598;
	  a_s[1][2][1]= 0.771082; b_s[1][2][1]= 0.762290;
	  a_s[1][3][0]= 0.726487; b_s[1][3][0]= 0.765419;
	  a_s[1][3][1]= 0.749828; b_s[1][3][1]= 0.820880;
	}
      else if(i_bbcsum==2)
	{
	  a_m[0][0][0]= 3.075970;  b_m[0][0][0]= -1.200000; c_m[0][0][0]= 0.075524; d_m[0][0][0]= 0.658464;  e_m[0][0][0]= -0.038412;
	  a_m[0][0][1]= 3.460356;  b_m[0][0][1]= -1.200000; c_m[0][0][1]= 0.155086; d_m[0][0][1]= 1.041275;  e_m[0][0][1]= -0.200500;
	  a_m[0][1][0]= 1.072527;  b_m[0][1][0]= -1.200000; c_m[0][1][0]= 0.214230; d_m[0][1][0]= 0.449605;  e_m[0][1][0]= -0.046111;
	  a_m[0][1][1]= 1.107493;  b_m[0][1][1]= -1.200000; c_m[0][1][1]= 0.520827; d_m[0][1][1]= 0.804528;  e_m[0][1][1]= -0.067352;
	  a_m[0][2][0]= -0.853720; b_m[0][2][0]= -1.200000; c_m[0][2][0]= 0.433857; d_m[0][2][0]= 0.304978;  e_m[0][2][0]= -0.008806;
	  a_m[0][2][1]= -1.155105; b_m[0][2][1]= -1.200000; c_m[0][2][1]= 0.828958; d_m[0][2][1]= 0.462033;  e_m[0][2][1]= 0.122386;
	  a_m[0][3][0]= -2.899430; b_m[0][3][0]= -1.200000; c_m[0][3][0]= 0.657439; d_m[0][3][0]= 0.189535;  e_m[0][3][0]= -0.025361;
	  a_m[0][3][1]= -3.447586; b_m[0][3][1]= -1.200000; c_m[0][3][1]= 1.319797; d_m[0][3][1]= 0.355069;  e_m[0][3][1]= 0.256894;
	  a_m[1][0][0]= 3.361093;  b_m[1][0][0]= -1.200000; c_m[1][0][0]= 0.091173; d_m[1][0][0]= 0.941659;  e_m[1][0][0]= -0.180729;
	  a_m[1][0][1]= 3.039546;  b_m[1][0][1]= -1.200000; c_m[1][0][1]= 0.104438; d_m[1][0][1]= 0.623906;  e_m[1][0][1]= 0.001197;
	  a_m[1][1][0]= 0.961016;  b_m[1][1][0]= -1.200000; c_m[1][1][0]= 0.221431; d_m[1][1][0]= 0.498472;  e_m[1][1][0]= -0.078980;
	  a_m[1][1][1]= 0.880852;  b_m[1][1][1]= -1.200000; c_m[1][1][1]= 0.504533; d_m[1][1][1]= 0.620818;  e_m[1][1][1]= 0.017611;
	  a_m[1][2][0]= -1.046925; b_m[1][2][0]= -1.200000; c_m[1][2][0]= 0.403341; d_m[1][2][0]= 0.121230;  e_m[1][2][0]= 0.070169;
	  a_m[1][2][1]= -1.002922; b_m[1][2][1]= -1.200000; c_m[1][2][1]= 0.834892; d_m[1][2][1]= 0.567381;  e_m[1][2][1]= 0.070693;
	  a_m[1][3][0]= -3.256132; b_m[1][3][0]= -1.200000; c_m[1][3][0]= 0.590623; d_m[1][3][0]= -0.231599; e_m[1][3][0]= 0.171788;
	  a_m[1][3][1]= -3.106711; b_m[1][3][1]= -1.200000; c_m[1][3][1]= 1.354856; d_m[1][3][1]= 0.684506;  e_m[1][3][1]= 0.092306;

	  a_s[0][0][0]= 0.818873; b_s[0][0][0]= 0.939195;
	  a_s[0][0][1]= 0.801382; b_s[0][0][1]= 0.956665;
	  a_s[0][1][0]= 0.818852; b_s[0][1][0]= 0.936836;
	  a_s[0][1][1]= 0.787311; b_s[0][1][1]= 0.953472;
	  a_s[0][2][0]= 0.805178; b_s[0][2][0]= 0.940871;
	  a_s[0][2][1]= 0.783525; b_s[0][2][1]= 0.968022;
	  a_s[0][3][0]= 0.808687; b_s[0][3][0]= 0.947572;
	  a_s[0][3][1]= 0.777658; b_s[0][3][1]= 0.985112;
	  a_s[1][0][0]= 0.783219; b_s[1][0][0]= 0.956218;
	  a_s[1][0][1]= 0.776399; b_s[1][0][1]= 0.957072;
	  a_s[1][1][0]= 0.764307; b_s[1][1][0]= 0.917402;
	  a_s[1][1][1]= 0.728615; b_s[1][1][1]= 0.936585;
	  a_s[1][2][0]= 0.744514; b_s[1][2][0]= 0.923591;
	  a_s[1][2][1]= 0.773658; b_s[1][2][1]= 0.942300;
	  a_s[1][3][0]= 0.740711; b_s[1][3][0]= 0.953608;
	  a_s[1][3][1]= 0.763518; b_s[1][3][1]= 0.996227;
	}
      else if(i_bbcsum==3)
	{
	  a_m[0][0][0]= 3.187303;  b_m[0][0][0]= -1.200000; c_m[0][0][0]= 0.097102; d_m[0][0][0]= 0.695343;  e_m[0][0][0]= -0.044855;
	  a_m[0][0][1]= 3.313230;  b_m[0][0][1]= -1.200000; c_m[0][0][1]= 0.196849; d_m[0][0][1]= 1.085700;  e_m[0][0][1]= -0.208290;
	  a_m[0][1][0]= 1.030679;  b_m[0][1][0]= -1.200000; c_m[0][1][0]= 0.252752; d_m[0][1][0]= 0.502159;  e_m[0][1][0]= -0.054142;
	  a_m[0][1][1]= 1.224589;  b_m[0][1][1]= -1.200000; c_m[0][1][1]= 0.556239; d_m[0][1][1]= 0.850314;  e_m[0][1][1]= -0.074006;
	  a_m[0][2][0]= -1.059242; b_m[0][2][0]= -1.200000; c_m[0][2][0]= 0.481588; d_m[0][2][0]= 0.323083;  e_m[0][2][0]= -0.006435;
	  a_m[0][2][1]= -1.050293; b_m[0][2][1]= -1.200000; c_m[0][2][1]= 0.837567; d_m[0][2][1]= 0.463541;  e_m[0][2][1]= 0.142440;
	  a_m[0][3][0]= -3.014333; b_m[0][3][0]= -1.200000; c_m[0][3][0]= 0.702976; d_m[0][3][0]= 0.220547;  e_m[0][3][0]= -0.025511;
	  a_m[0][3][1]= -3.662127; b_m[0][3][1]= -1.200000; c_m[0][3][1]= 1.376437; d_m[0][3][1]= 0.371323;  e_m[0][3][1]= 0.270202;
	  a_m[1][0][0]= 3.074507;  b_m[1][0][0]= -1.200000; c_m[1][0][0]= 0.142988; d_m[1][0][0]= 0.974052;  e_m[1][0][0]= -0.186753;
	  a_m[1][0][1]= 2.832332;  b_m[1][0][1]= -1.200000; c_m[1][0][1]= 0.165202; d_m[1][0][1]= 0.668911;  e_m[1][0][1]= -0.015143;
	  a_m[1][1][0]= 1.177605;  b_m[1][1][0]= -1.200000; c_m[1][1][0]= 0.240845; d_m[1][1][0]= 0.536618;  e_m[1][1][0]= -0.080282;
	  a_m[1][1][1]= 0.850881;  b_m[1][1][1]= -1.200000; c_m[1][1][1]= 0.538712; d_m[1][1][1]= 0.658466;  e_m[1][1][1]= 0.015556;
	  a_m[1][2][0]= -0.955518; b_m[1][2][0]= -1.200000; c_m[1][2][0]= 0.416276; d_m[1][2][0]= 0.144149;  e_m[1][2][0]= 0.069739;
	  a_m[1][2][1]= -1.355461; b_m[1][2][1]= -1.200000; c_m[1][2][1]= 0.892153; d_m[1][2][1]= 0.592693;  e_m[1][2][1]= 0.067042;
	  a_m[1][3][0]= -3.122100; b_m[1][3][0]= -1.200000; c_m[1][3][0]= 0.606224; d_m[1][3][0]= -0.196794; e_m[1][3][0]= 0.168824;
	  a_m[1][3][1]= -2.866605; b_m[1][3][1]= -1.200000; c_m[1][3][1]= 1.372665; d_m[1][3][1]= 0.705212;  e_m[1][3][1]= 0.106021;
	  
	  a_s[0][0][0]= 0.904715; b_s[0][0][0]= 1.418879;
	  a_s[0][0][1]= 0.884524; b_s[0][0][1]= 1.448646;
	  a_s[0][1][0]= 0.957090; b_s[0][1][0]= 1.351552;
	  a_s[0][1][1]= 0.879113; b_s[0][1][1]= 1.430215;
	  a_s[0][2][0]= 0.943362; b_s[0][2][0]= 1.354904;
	  a_s[0][2][1]= 0.864474; b_s[0][2][1]= 1.452585;
	  a_s[0][3][0]= 0.891853; b_s[0][3][0]= 1.427574;
	  a_s[0][3][1]= 0.867008; b_s[0][3][1]= 1.462137;
	  a_s[1][0][0]= 0.817277; b_s[1][0][0]= 1.392102;
	  a_s[1][0][1]= 0.771750; b_s[1][0][1]= 1.438643;
	  a_s[1][1][0]= 0.861225; b_s[1][1][0]= 1.285581;
	  a_s[1][1][1]= 0.835691; b_s[1][1][1]= 1.303847;
	  a_s[1][2][0]= 0.824865; b_s[1][2][0]= 1.314870;
	  a_s[1][2][1]= 0.875669; b_s[1][2][1]= 1.298920;
	  a_s[1][3][0]= 0.805357; b_s[1][3][0]= 1.366914;
	  a_s[1][3][1]= 0.734323; b_s[1][3][1]= 1.484789;
	}
      else
	{
	  return -9999;
	}

    }
  else
    {
      return -9999;
    }

  float m_dz = 0;
  float s_dz = 1;
  
  if(pt>1.5)       m_dz =  a_m[i_ch][i_zed][i_phi] * exp(b_m[i_ch][i_zed][i_phi]*pt) + c_m[i_ch][i_zed][i_phi];
  else if(pt<=1.5) m_dz =  d_m[i_ch][i_zed][i_phi] + e_m[i_ch][i_zed][i_phi]*pt;

  s_dz = sqrt( a_s[i_ch][i_zed][i_phi]*a_s[i_ch][i_zed][i_phi]/pt/pt + b_s[i_ch][i_zed][i_phi]*b_s[i_ch][i_zed][i_phi] );

  float sdz  = -9999;
  if(s_dz>0) sdz = (dz - m_dz)/s_dz;      

  return sdz;

}


// Gettofsdz
float MatchrecalRecoRun5::Gettofsdz(const short i_bbcsum, const short i_ch, const short i_zed, const short i_phi, const float pt, const float dz)
{

  if(dz==-9999) return -9999;

  if(i_phi==0) return -9999; // <-- For TOF.

  // Parameters
  float a_m[2][4][2],b_m[2][4][2],c_m[2][4][2], d_m[2][4][2],e_m[2][4][2];
  float a_s[2][4][2],b_s[2][4][2];

  if(b_field==1)
    {

      if(i_bbcsum==0)
	{
	  a_m[0][0][1]= 3.055062;  b_m[0][0][1]= -1.200000; c_m[0][0][1]= 0.578756; d_m[0][0][1]= 1.154777;  e_m[0][0][1]= -0.041479;
	  a_m[0][1][1]= 0.869201;  b_m[0][1][1]= -1.200000; c_m[0][1][1]= 0.577191; d_m[0][1][1]= 0.676921;  e_m[0][1][1]= 0.029090;
	  a_m[0][2][1]= -0.546990; b_m[0][2][1]= -1.200000; c_m[0][2][1]= 0.624272; d_m[0][2][1]= 0.458490;  e_m[0][2][1]= 0.050065;
	  a_m[0][3][1]= -3.047881; b_m[0][3][1]= -1.200000; c_m[0][3][1]= 0.819880; d_m[0][3][1]= 0.029360;  e_m[0][3][1]= 0.186705;
	  a_m[1][0][1]= 3.492861;  b_m[1][0][1]= -1.200000; c_m[1][0][1]= 0.661393; d_m[1][0][1]= 1.638122;  e_m[1][0][1]= -0.258215;
	  a_m[1][1][1]= 0.890965;  b_m[1][1][1]= -1.200000; c_m[1][1][1]= 0.616658; d_m[1][1][1]= 0.863404;  e_m[1][1][1]= -0.068389;
	  a_m[1][2][1]= -1.149145; b_m[1][2][1]= -1.200000; c_m[1][2][1]= 0.614772; d_m[1][2][1]= 0.196275;  e_m[1][2][1]= 0.160892;
	  a_m[1][3][1]= -3.418259; b_m[1][3][1]= -1.200000; c_m[1][3][1]= 0.760023; d_m[1][3][1]= -0.291923; e_m[1][3][1]= 0.317814;

	  a_s[0][0][1]= 0.803782; b_s[0][0][1]= 0.793297;
	  a_s[0][1][1]= 0.804939; b_s[0][1][1]= 0.844068;
	  a_s[0][2][1]= 0.792683; b_s[0][2][1]= 0.811383;
	  a_s[0][3][1]= 0.754262; b_s[0][3][1]= 0.798532;
	  a_s[1][0][1]= 0.802525; b_s[1][0][1]= 0.822443;
	  a_s[1][1][1]= 0.814084; b_s[1][1][1]= 0.853049;
	  a_s[1][2][1]= 0.794208; b_s[1][2][1]= 0.804700;
	  a_s[1][3][1]= 0.789532; b_s[1][3][1]= 0.795355;
	}
      else if(i_bbcsum==1)
	{
	  a_m[0][0][1]= 3.087322;  b_m[0][0][1]= -1.200000; c_m[0][0][1]= 0.605239; d_m[0][0][1]= 1.207493;  e_m[0][0][1]= -0.056174;
	  a_m[0][1][1]= 0.942235;  b_m[0][1][1]= -1.200000; c_m[0][1][1]= 0.606660; d_m[0][1][1]= 0.713827;  e_m[0][1][1]= 0.026657;
	  a_m[0][2][1]= -0.725669; b_m[0][2][1]= -1.200000; c_m[0][2][1]= 0.680273; d_m[0][2][1]= 0.495488;  e_m[0][2][1]= 0.050951;
	  a_m[0][3][1]= -3.052559; b_m[0][3][1]= -1.200000; c_m[0][3][1]= 0.852847; d_m[0][3][1]= 0.063021;  e_m[0][3][1]= 0.187789;
	  a_m[1][0][1]= 3.486789;  b_m[1][0][1]= -1.200000; c_m[1][0][1]= 0.697194; d_m[1][0][1]= 1.689982;  e_m[1][0][1]= -0.272950;
	  a_m[1][1][1]= 0.995807;  b_m[1][1][1]= -1.200000; c_m[1][1][1]= 0.637767; d_m[1][1][1]= 0.904498;  e_m[1][1][1]= -0.074125;
	  a_m[1][2][1]= -1.155337; b_m[1][2][1]= -1.200000; c_m[1][2][1]= 0.647161; d_m[1][2][1]= 0.231661;  e_m[1][2][1]= 0.159747;
	  a_m[1][3][1]= -3.488858; b_m[1][3][1]= -1.200000; c_m[1][3][1]= 0.800914; d_m[1][3][1]= -0.261650; e_m[1][3][1]= 0.321194;
	  
	  a_s[0][0][1]= 0.809598; b_s[0][0][1]= 0.854401;
	  a_s[0][1][1]= 0.814032; b_s[0][1][1]= 0.899509;
	  a_s[0][2][1]= 0.799224; b_s[0][2][1]= 0.871908;
	  a_s[0][3][1]= 0.759566; b_s[0][3][1]= 0.857395;
	  a_s[1][0][1]= 0.803092; b_s[1][0][1]= 0.884572;
	  a_s[1][1][1]= 0.818951; b_s[1][1][1]= 0.911655;
	  a_s[1][2][1]= 0.794714; b_s[1][2][1]= 0.868768;
	  a_s[1][3][1]= 0.790471; b_s[1][3][1]= 0.856687;
	}
      else if(i_bbcsum==2)
	{
	  a_m[0][0][1]= 3.085366;  b_m[0][0][1]= -1.200000; c_m[0][0][1]= 0.656556; d_m[0][0][1]= 1.253734;  e_m[0][0][1]= -0.051690;
	  a_m[0][1][1]= 0.937513;  b_m[0][1][1]= -1.200000; c_m[0][1][1]= 0.640557; d_m[0][1][1]= 0.757077;  e_m[0][1][1]= 0.020143;
	  a_m[0][2][1]= -0.537277; b_m[0][2][1]= -1.200000; c_m[0][2][1]= 0.723227; d_m[0][2][1]= 0.551201;  e_m[0][2][1]= 0.056175;
	  a_m[0][3][1]= -3.223975; b_m[0][3][1]= -1.200000; c_m[0][3][1]= 0.922307; d_m[0][3][1]= 0.109921;  e_m[0][3][1]= 0.192604;
	  a_m[1][0][1]= 3.692876;  b_m[1][0][1]= -1.200000; c_m[1][0][1]= 0.720496; d_m[1][0][1]= 1.744508;  e_m[1][0][1]= -0.277386;
	  a_m[1][1][1]= 1.014271;  b_m[1][1][1]= -1.200000; c_m[1][1][1]= 0.668016; d_m[1][1][1]= 0.946323;  e_m[1][1][1]= -0.073563;
	  a_m[1][2][1]= -1.045141; b_m[1][2][1]= -1.200000; c_m[1][2][1]= 0.694227; d_m[1][2][1]= 0.284211;  e_m[1][2][1]= 0.163248;
	  a_m[1][3][1]= -3.655296; b_m[1][3][1]= -1.200000; c_m[1][3][1]= 0.859932; d_m[1][3][1]= -0.214906; e_m[1][3][1]= 0.324627;

	  a_s[0][0][1]= 0.832610; b_s[0][0][1]= 1.039962;
	  a_s[0][1][1]= 0.834887; b_s[0][1][1]= 1.083311;
	  a_s[0][2][1]= 0.826012; b_s[0][2][1]= 1.052469;
	  a_s[0][3][1]= 0.790083; b_s[0][3][1]= 1.035181;
	  a_s[1][0][1]= 0.813713; b_s[1][0][1]= 1.072283;
	  a_s[1][1][1]= 0.834406; b_s[1][1][1]= 1.101457;
	  a_s[1][2][1]= 0.818420; b_s[1][2][1]= 1.050608;
	  a_s[1][3][1]= 0.814354; b_s[1][3][1]= 1.031638;
	}
      else if(i_bbcsum==3)
	{
	  a_m[0][0][1]= 2.638667;  b_m[0][0][1]= -1.200000; c_m[0][0][1]= 0.718831; d_m[0][0][1]= 1.316423;  e_m[0][0][1]= -0.079943;
	  a_m[0][1][1]= 0.786978;  b_m[0][1][1]= -1.200000; c_m[0][1][1]= 0.672227; d_m[0][1][1]= 0.790410;  e_m[0][1][1]= 0.009611;
	  a_m[0][2][1]= -0.940360; b_m[0][2][1]= -1.200000; c_m[0][2][1]= 0.797539; d_m[0][2][1]= 0.590025;  e_m[0][2][1]= 0.059369;
	  a_m[0][3][1]= -3.131869; b_m[0][3][1]= -1.200000; c_m[0][3][1]= 0.927222; d_m[0][3][1]= 0.117589;  e_m[0][3][1]= 0.220563;
	  a_m[1][0][1]= 3.333570;  b_m[1][0][1]= -1.200000; c_m[1][0][1]= 0.781554; d_m[1][0][1]= 1.810762;  e_m[1][0][1]= -0.312348;
	  a_m[1][1][1]= 1.136093;  b_m[1][1][1]= -1.200000; c_m[1][1][1]= 0.679984; d_m[1][1][1]= 0.977854;  e_m[1][1][1]= -0.076124;
	  a_m[1][2][1]= -1.003339; b_m[1][2][1]= -1.200000; c_m[1][2][1]= 0.711217; d_m[1][2][1]= 0.303926;  e_m[1][2][1]= 0.180782;
	  a_m[1][3][1]= -3.253369; b_m[1][3][1]= -1.200000; c_m[1][3][1]= 0.862694; d_m[1][3][1]= -0.198506; e_m[1][3][1]= 0.346523;

	  a_s[0][0][1]= 0.922516; b_s[0][0][1]= 1.521843;
	  a_s[0][1][1]= 0.915729; b_s[0][1][1]= 1.571813;
	  a_s[0][2][1]= 0.908110; b_s[0][2][1]= 1.551314;
	  a_s[0][3][1]= 0.878136; b_s[0][3][1]= 1.525409;
	  a_s[1][0][1]= 0.821887; b_s[1][0][1]= 1.585541;
	  a_s[1][1][1]= 0.891407; b_s[1][1][1]= 1.585885;
	  a_s[1][2][1]= 0.805011; b_s[1][2][1]= 1.553706;
	  a_s[1][3][1]= 0.831522; b_s[1][3][1]= 1.552321;
	}
      else
	{
	  return -9999;
	}

    }
  else if(b_field==-1)
    {

      if(i_bbcsum==0)
	{
	  a_m[0][0][1]= 3.503186;  b_m[0][0][1]= -1.200000; c_m[0][0][1]= 0.627598; d_m[0][0][1]= 1.612849;  e_m[0][0][1]= -0.262844;
	  a_m[0][1][1]= 0.923402;  b_m[0][1][1]= -1.200000; c_m[0][1][1]= 0.582846; d_m[0][1][1]= 0.827849;  e_m[0][1][1]= -0.059706;
	  a_m[0][2][1]= -1.063668; b_m[0][2][1]= -1.200000; c_m[0][2][1]= 0.587763; d_m[0][2][1]= 0.177236;  e_m[0][2][1]= 0.159223;
	  a_m[0][3][1]= -3.541998; b_m[0][3][1]= -1.200000; c_m[0][3][1]= 0.757797; d_m[0][3][1]= -0.320435; e_m[0][3][1]= 0.326545;
	  a_m[1][0][1]= 2.962795;  b_m[1][0][1]= -1.200000; c_m[1][0][1]= 0.538694; d_m[1][0][1]= 1.106116;  e_m[1][0][1]= -0.042678;
	  a_m[1][1][1]= 0.970962;  b_m[1][1][1]= -1.200000; c_m[1][1][1]= 0.531771; d_m[1][1][1]= 0.643354;  e_m[1][1][1]= 0.025057;
	  a_m[1][2][1]= -0.749828; b_m[1][2][1]= -1.200000; c_m[1][2][1]= 0.619592; d_m[1][2][1]= 0.431783;  e_m[1][2][1]= 0.049177;
	  a_m[1][3][1]= -3.178026; b_m[1][3][1]= -1.200000; c_m[1][3][1]= 0.805905; d_m[1][3][1]= 0.018477;  e_m[1][3][1]= 0.179809;

	  a_s[0][0][1]= 0.825399; b_s[0][0][1]= 0.808341;
	  a_s[0][1][1]= 0.838162; b_s[0][1][1]= 0.842038;
	  a_s[0][2][1]= 0.818845; b_s[0][2][1]= 0.799331;
	  a_s[0][3][1]= 0.811046; b_s[0][3][1]= 0.787664;
	  a_s[1][0][1]= 0.774608; b_s[1][0][1]= 0.806256;
	  a_s[1][1][1]= 0.783718; b_s[1][1][1]= 0.848808;
	  a_s[1][2][1]= 0.768015; b_s[1][2][1]= 0.823000;
	  a_s[1][3][1]= 0.736900; b_s[1][3][1]= 0.799921;
	}
      else if(i_bbcsum==1)
	{
	  a_m[0][0][1]= 3.625204;  b_m[0][0][1]= -1.200000; c_m[0][0][1]= 0.646222; d_m[0][0][1]= 1.657039;  e_m[0][0][1]= -0.272021;
	  a_m[0][1][1]= 0.998551;  b_m[0][1][1]= -1.200000; c_m[0][1][1]= 0.603853; d_m[0][1][1]= 0.867332;  e_m[0][1][1]= -0.066482;
	  a_m[0][2][1]= -1.148842; b_m[0][2][1]= -1.200000; c_m[0][2][1]= 0.627565; d_m[0][2][1]= 0.210439;  e_m[0][2][1]= 0.157609;
	  a_m[0][3][1]= -3.708013; b_m[0][3][1]= -1.200000; c_m[0][3][1]= 0.802791; d_m[0][3][1]= -0.294918; e_m[0][3][1]= 0.331481;
	  a_m[1][0][1]= 3.006800;  b_m[1][0][1]= -1.200000; c_m[1][0][1]= 0.564219; d_m[1][0][1]= 1.135605;  e_m[1][0][1]= -0.038038;
	  a_m[1][1][1]= 0.867631;  b_m[1][1][1]= -1.200000; c_m[1][1][1]= 0.572652; d_m[1][1][1]= 0.664846;  e_m[1][1][1]= 0.034332;
	  a_m[1][2][1]= -0.701035; b_m[1][2][1]= -1.200000; c_m[1][2][1]= 0.649106; d_m[1][2][1]= 0.469846;  e_m[1][2][1]= 0.047501;
	  a_m[1][3][1]= -3.089192; b_m[1][3][1]= -1.200000; c_m[1][3][1]= 0.830309; d_m[1][3][1]= 0.034651;  e_m[1][3][1]= 0.189414;

	  a_s[0][0][1]= 0.828828; b_s[0][0][1]= 0.868872;
	  a_s[0][1][1]= 0.839377; b_s[0][1][1]= 0.904543;
	  a_s[0][2][1]= 0.823697; b_s[0][2][1]= 0.859254;
	  a_s[0][3][1]= 0.814082; b_s[0][3][1]= 0.851011;
	  a_s[1][0][1]= 0.781919; b_s[1][0][1]= 0.864024;
	  a_s[1][1][1]= 0.788691; b_s[1][1][1]= 0.907857;
	  a_s[1][2][1]= 0.768218; b_s[1][2][1]= 0.883546;
	  a_s[1][3][1]= 0.738066; b_s[1][3][1]= 0.866152;
	}
      else if(i_bbcsum==2)
	{
	  a_m[0][0][1]= 3.555009;  b_m[0][0][1]= -1.200000; c_m[0][0][1]= 0.695679; d_m[0][0][1]= 1.718550;  e_m[0][0][1]= -0.286249;
	  a_m[0][1][1]= 1.024562;  b_m[0][1][1]= -1.200000; c_m[0][1][1]= 0.633878; d_m[0][1][1]= 0.919180;  e_m[0][1][1]= -0.078724;
	  a_m[0][2][1]= -1.075438; b_m[0][2][1]= -1.200000; c_m[0][2][1]= 0.670009; d_m[0][2][1]= 0.257903;  e_m[0][2][1]= 0.163182;
	  a_m[0][3][1]= -3.358631; b_m[0][3][1]= -1.200000; c_m[0][3][1]= 0.825168; d_m[0][3][1]= -0.244773; e_m[0][3][1]= 0.330246;
	  a_m[1][0][1]= 3.052516;  b_m[1][0][1]= -1.200000; c_m[1][0][1]= 0.608498; d_m[1][0][1]= 1.191945;  e_m[1][0][1]= -0.045507;
	  a_m[1][1][1]= 1.074413;  b_m[1][1][1]= -1.200000; c_m[1][1][1]= 0.584447; d_m[1][1][1]= 0.704179;  e_m[1][1][1]= 0.028983;
	  a_m[1][2][1]= -0.717922; b_m[1][2][1]= -1.200000; c_m[1][2][1]= 0.707645; d_m[1][2][1]= 0.526517;  e_m[1][2][1]= 0.046191;
	  a_m[1][3][1]= -2.896381; b_m[1][3][1]= -1.200000; c_m[1][3][1]= 0.865884; d_m[1][3][1]= 0.090819;  e_m[1][3][1]= 0.183367;

	  a_s[0][0][1]= 0.847988; b_s[0][0][1]= 1.053998;
	  a_s[0][1][1]= 0.863029; b_s[0][1][1]= 1.086874;
	  a_s[0][2][1]= 0.849061; b_s[0][2][1]= 1.043375;
	  a_s[0][3][1]= 0.842406; b_s[0][3][1]= 1.027650;
	  a_s[1][0][1]= 0.801447; b_s[1][0][1]= 1.046839;
	  a_s[1][1][1]= 0.801837; b_s[1][1][1]= 1.096535;
	  a_s[1][2][1]= 0.785008; b_s[1][2][1]= 1.065866;
	  a_s[1][3][1]= 0.761017; b_s[1][3][1]= 1.043074;
	}
      else if(i_bbcsum==3)
	{
	  a_m[0][0][1]= 3.365284;  b_m[0][0][1]= -1.200000; c_m[0][0][1]= 0.737204; d_m[0][0][1]= 1.772944;  e_m[0][0][1]= -0.300435;
	  a_m[0][1][1]= 0.870867;  b_m[0][1][1]= -1.200000; c_m[0][1][1]= 0.677025; d_m[0][1][1]= 0.945108;  e_m[0][1][1]= -0.070178;
	  a_m[0][2][1]= -0.976908; b_m[0][2][1]= -1.200000; c_m[0][2][1]= 0.692931; d_m[0][2][1]= 0.254376;  e_m[0][2][1]= 0.202385;
	  a_m[0][3][1]= -3.345966; b_m[0][3][1]= -1.200000; c_m[0][3][1]= 0.858518; d_m[0][3][1]= -0.208677; e_m[0][3][1]= 0.326505;
	  a_m[1][0][1]= 2.840220;  b_m[1][0][1]= -1.200000; c_m[1][0][1]= 0.690630; d_m[1][0][1]= 1.251047;  e_m[1][0][1]= -0.077804;
	  a_m[1][1][1]= 0.786938;  b_m[1][1][1]= -1.200000; c_m[1][1][1]= 0.627648; d_m[1][1][1]= 0.754150;  e_m[1][1][1]= 0.002433;
	  a_m[1][2][1]= -0.710296; b_m[1][2][1]= -1.200000; c_m[1][2][1]= 0.734044; d_m[1][2][1]= 0.595981;  e_m[1][2][1]= 0.014136;
	  a_m[1][3][1]= -2.872389; b_m[1][3][1]= -1.200000; c_m[1][3][1]= 0.899228; d_m[1][3][1]= 0.112118;  e_m[1][3][1]= 0.194068;

	  a_s[0][0][1]= 0.936000; b_s[0][0][1]= 1.547858;
	  a_s[0][1][1]= 0.959473; b_s[0][1][1]= 1.561977;
	  a_s[0][2][1]= 0.931484; b_s[0][2][1]= 1.532811;
	  a_s[0][3][1]= 0.938415; b_s[0][3][1]= 1.511075;
	  a_s[1][0][1]= 0.809571; b_s[1][0][1]= 1.553467;
	  a_s[1][1][1]= 0.795349; b_s[1][1][1]= 1.576128;
	  a_s[1][2][1]= 0.769858; b_s[1][2][1]= 1.557319;
	  a_s[1][3][1]= 0.688256; b_s[1][3][1]= 1.548026;
	}
      else
	{
	  return -9999;
	}

    }
  else
    {
      return -9999;
    }

  float m_dz = 0;
  float s_dz = 1;
  
  if(pt>1.5)       m_dz =  a_m[i_ch][i_zed][i_phi] * exp(b_m[i_ch][i_zed][i_phi]*pt) + c_m[i_ch][i_zed][i_phi];
  else if(pt<=1.5) m_dz =  d_m[i_ch][i_zed][i_phi] + e_m[i_ch][i_zed][i_phi]*pt;

  s_dz = sqrt( a_s[i_ch][i_zed][i_phi]*a_s[i_ch][i_zed][i_phi]/pt/pt + b_s[i_ch][i_zed][i_phi]*b_s[i_ch][i_zed][i_phi] );

  float sdz  = -9999;
  if(s_dz>0) sdz = (dz - m_dz)/s_dz;      

  return sdz;

}




// Getemcsdz
float MatchrecalRecoRun5::Getemcsdz(const short i_bbcsum, const short i_ch, const short i_zed, const short i_phi, const float pt, const float dz)
{

  if(dz==-9999) return -9999;

  // Parameters
  float a_m[2][4][2],b_m[2][4][2],c_m[2][4][2], d_m[2][4][2],e_m[2][4][2];
  float a_s[2][4][2],b_s[2][4][2];

  if(b_field==1)
    {

      if(i_bbcsum==0)
	{
	  a_m[0][0][0]= 5.091065;  b_m[0][0][0]= -1.200000; c_m[0][0][0]= -2.648682; d_m[0][0][0]= -0.590708; e_m[0][0][0]= -0.846767;
	  a_m[0][0][1]= 4.613458;  b_m[0][0][1]= -1.200000; c_m[0][0][1]= -3.003250; d_m[0][0][1]= -1.344955; e_m[0][0][1]= -0.634332;
	  a_m[0][1][0]= 1.7;       b_m[0][1][0]= -1.200000; c_m[0][1][0]= -0.7;      d_m[0][1][0]= 0.0;       e_m[0][1][0]= -0.26;
	  a_m[0][1][1]= 1.559957;  b_m[0][1][1]= -1.200000; c_m[0][1][1]= -0.838713; d_m[0][1][1]= -0.236581; e_m[0][1][1]= -0.247348;
	  a_m[0][2][0]= -1.772602; b_m[0][2][0]= -1.200000; c_m[0][2][0]= 1.676429;  d_m[0][2][0]= 0.833370;  e_m[0][2][0]= 0.383190;
	  a_m[0][2][1]= -2.107304; b_m[0][2][1]= -1.200000; c_m[0][2][1]= 1.760080;  d_m[0][2][1]= 0.792274;  e_m[0][2][1]= 0.453335;
	  a_m[0][3][0]= -5.151066; b_m[0][3][0]= -1.200000; c_m[0][3][0]= 3.717770;  d_m[0][3][0]= 1.606594;  e_m[0][3][0]= 0.888014;
	  a_m[0][3][1]= -4.946006; b_m[0][3][1]= -1.200000; c_m[0][3][1]= 3.953437;  d_m[0][3][1]= 1.962443;  e_m[0][3][1]= 0.827406;
	  a_m[1][0][0]= 4.588695;  b_m[1][0][0]= -1.200000; c_m[1][0][0]= -2.466874; d_m[1][0][0]= -0.922298; e_m[1][0][0]= -0.543665;
	  a_m[1][0][1]= 5.043491;  b_m[1][0][1]= -1.200000; c_m[1][0][1]= -2.827487; d_m[1][0][1]= -0.992563; e_m[1][0][1]= -0.689448;
	  a_m[1][1][0]= 1.877075;  b_m[1][1][0]= -1.200000; c_m[1][1][0]= -0.617374; d_m[1][1][0]= 0.085259;  e_m[1][1][0]= -0.289605;
	  a_m[1][1][1]= 1.969695;  b_m[1][1][1]= -1.200000; c_m[1][1][1]= -0.815628; d_m[1][1][1]= -0.032098; e_m[1][1][1]= -0.318661;
	  a_m[1][2][0]= -1.828265; b_m[1][2][0]= -1.200000; c_m[1][2][0]= 1.679424;  d_m[1][2][0]= 1.038450;  e_m[1][2][0]= 0.241285;
	  a_m[1][2][1]= -1.638137; b_m[1][2][1]= -1.200000; c_m[1][2][1]= 1.656819;  d_m[1][2][1]= 0.953298;  e_m[1][2][1]= 0.303971;
	  a_m[1][3][0]= -4.553748; b_m[1][3][0]= -1.200000; c_m[1][3][0]= 3.595982;  d_m[1][3][0]= 2.123547;  e_m[1][3][0]= 0.489293;
	  a_m[1][3][1]= -4.7;      b_m[1][3][1]= -1.200000; c_m[1][3][1]= 3.8;       d_m[1][3][1]= 2.022832;  e_m[1][3][1]= 0.662825;

	  a_s[0][0][0]= 1.091459; b_s[0][0][0]= 2.826512;
	  a_s[0][0][1]= 1.107861; b_s[0][0][1]= 2.881371;
	  a_s[0][1][0]= 1.05;     b_s[0][1][0]= 2.5;
	  a_s[0][1][1]= 1.063798; b_s[0][1][1]= 2.442467;
	  a_s[0][2][0]= 1.104628; b_s[0][2][0]= 2.377293;
	  a_s[0][2][1]= 1.023755; b_s[0][2][1]= 2.529858;
	  a_s[0][3][0]= 1.143662; b_s[0][3][0]= 2.794776;
	  a_s[0][3][1]= 1.104319; b_s[0][3][1]= 2.896870;
	  a_s[1][0][0]= 0.930208; b_s[1][0][0]= 3.028157;
	  a_s[1][0][1]= 0.795823; b_s[1][0][1]= 3.049664;
	  a_s[1][1][0]= 1.030943; b_s[1][1][0]= 2.549712;
	  a_s[1][1][1]= 0.928965; b_s[1][1][1]= 2.561313;
	  a_s[1][2][0]= 1.042947; b_s[1][2][0]= 2.523579;
	  a_s[1][2][1]= 0.930705; b_s[1][2][1]= 2.680403;
	  a_s[1][3][0]= 0.943652; b_s[1][3][0]= 3.008747;
	  a_s[1][3][1]= 0.895474; b_s[1][3][1]= 3.080201;
	}
      else if(i_bbcsum==1)
	{
	  a_m[0][0][0]= 5.311320;  b_m[0][0][0]= -1.200000; c_m[0][0][0]= -2.648878; d_m[0][0][0]= -0.550112; e_m[0][0][0]= -0.862490;
	  a_m[0][0][1]= 4.574721;  b_m[0][0][1]= -1.200000; c_m[0][0][1]= -2.977381; d_m[0][0][1]= -1.318984; e_m[0][0][1]= -0.641913;
	  a_m[0][1][0]= 1.863618;  b_m[0][1][0]= -1.200000; c_m[0][1][0]= -0.619330; d_m[0][1][0]= 0.241488;  e_m[0][1][0]= -0.389096;
	  a_m[0][1][1]= 1.351067;  b_m[0][1][1]= -1.200000; c_m[0][1][1]= -0.784789; d_m[0][1][1]= -0.198235; e_m[0][1][1]= -0.247936;
	  a_m[0][2][0]= -1.844467; b_m[0][2][0]= -1.200000; c_m[0][2][0]= 1.718391;  d_m[0][2][0]= 0.861332;  e_m[0][2][0]= 0.388757;
	  a_m[0][2][1]= -1.950102; b_m[0][2][1]= -1.200000; c_m[0][2][1]= 1.781446;  d_m[0][2][1]= 0.832634;  e_m[0][2][1]= 0.449814;
	  a_m[0][3][0]= -5.197091; b_m[0][3][0]= -1.200000; c_m[0][3][0]= 3.785562;  d_m[0][3][0]= 1.652819;  e_m[0][3][0]= 0.891497;
	  a_m[0][3][1]= -4.740579; b_m[0][3][1]= -1.200000; c_m[0][3][1]= 3.987845;  d_m[0][3][1]= 2.010570;  e_m[0][3][1]= 0.833434;
	  a_m[1][0][0]= 4.739319;  b_m[1][0][0]= -1.200000; c_m[1][0][0]= -2.464588; d_m[1][0][0]= -0.889132; e_m[1][0][0]= -0.559136;
	  a_m[1][0][1]= 4.935711;  b_m[1][0][1]= -1.200000; c_m[1][0][1]= -2.802272; d_m[1][0][1]= -0.943394; e_m[1][0][1]= -0.722229;
	  a_m[1][1][0]= 1.803700;  b_m[1][1][0]= -1.200000; c_m[1][1][0]= -0.579344; d_m[1][1][0]= 0.125732;  e_m[1][1][0]= -0.295182;
	  a_m[1][1][1]= 1.797528;  b_m[1][1][1]= -1.200000; c_m[1][1][1]= -0.759920; d_m[1][1][1]= 0.012121;  e_m[1][1][1]= -0.329988;
	  a_m[1][2][0]= -1.927442; b_m[1][2][0]= -1.200000; c_m[1][2][0]= 1.720400;  d_m[1][2][0]= 1.079975;  e_m[1][2][0]= 0.239526;
	  a_m[1][2][1]= -1.725563; b_m[1][2][1]= -1.200000; c_m[1][2][1]= 1.703744;  d_m[1][2][1]= 0.983681;  e_m[1][2][1]= 0.310671;
	  a_m[1][3][0]= -4.668484; b_m[1][3][0]= -1.200000; c_m[1][3][0]= 3.669283;  d_m[1][3][0]= 2.162113;  e_m[1][3][0]= 0.507944;
	  a_m[1][3][1]= -4.735300; b_m[1][3][1]= -1.200000; c_m[1][3][1]= 3.824000;  d_m[1][3][1]= 2.067865;  e_m[1][3][1]= 0.672410;

	  a_s[0][0][0]= 1.086877; b_s[0][0][0]= 2.820020;
	  a_s[0][0][1]= 1.109334; b_s[0][0][1]= 2.874083;
	  a_s[0][1][0]= 1.088982; b_s[0][1][0]= 2.401072;
	  a_s[0][1][1]= 1.059556; b_s[0][1][1]= 2.441206;
	  a_s[0][2][0]= 1.108168; b_s[0][2][0]= 2.371457;
	  a_s[0][2][1]= 1.030420; b_s[0][2][1]= 2.528109;
	  a_s[0][3][0]= 1.133941; b_s[0][3][0]= 2.795375;
	  a_s[0][3][1]= 1.105648; b_s[0][3][1]= 2.892259;
	  a_s[1][0][0]= 0.937116; b_s[1][0][0]= 3.014352;
	  a_s[1][0][1]= 0.833287; b_s[1][0][1]= 3.029022;
	  a_s[1][1][0]= 1.039246; b_s[1][1][0]= 2.539550;
	  a_s[1][1][1]= 0.935194; b_s[1][1][1]= 2.556655;
	  a_s[1][2][0]= 1.047721; b_s[1][2][0]= 2.519360;
	  a_s[1][2][1]= 0.939356; b_s[1][2][1]= 2.684191;
	  a_s[1][3][0]= 0.970839; b_s[1][3][0]= 2.997745;
	  a_s[1][3][1]= 0.905934; b_s[1][3][1]= 3.073354;
	}
      else if(i_bbcsum==2)
	{
	  a_m[0][0][0]= 4.920545;  b_m[0][0][0]= -1.200000; c_m[0][0][0]= -2.582660; d_m[0][0][0]= -0.484179; e_m[0][0][0]= -0.888566;
	  a_m[0][0][1]= 4.775831;  b_m[0][0][1]= -1.200000; c_m[0][0][1]= -2.970753; d_m[0][0][1]= -1.267450; e_m[0][0][1]= -0.656743;
	  a_m[0][1][0]= 1.933359;  b_m[0][1][0]= -1.200000; c_m[0][1][0]= -0.577675; d_m[0][1][0]= 0.304569;  e_m[0][1][0]= -0.398630;
	  a_m[0][1][1]= 1.496036;  b_m[0][1][1]= -1.200000; c_m[0][1][1]= -0.743760; d_m[0][1][1]= -0.130213; e_m[0][1][1]= -0.266304;
	  a_m[0][2][0]= -1.733848; b_m[0][2][0]= -1.200000; c_m[0][2][0]= 1.760682;  d_m[0][2][0]= 0.903292;  e_m[0][2][0]= 0.400594;
	  a_m[0][2][1]= -1.934201; b_m[0][2][1]= -1.200000; c_m[0][2][1]= 1.828717;  d_m[0][2][1]= 0.878932;  e_m[0][2][1]= 0.453995;
	  a_m[0][3][0]= -4.981087; b_m[0][3][0]= -1.200000; c_m[0][3][0]= 3.825445;  d_m[0][3][0]= 1.707187;  e_m[0][3][0]= 0.899942;
	  a_m[0][3][1]= -4.933285; b_m[0][3][1]= -1.200000; c_m[0][3][1]= 4.081428;  d_m[0][3][1]= 2.070201;  e_m[0][3][1]= 0.849303;
	  a_m[1][0][0]= 4.899369;  b_m[1][0][0]= -1.200000; c_m[1][0][0]= -2.462987; d_m[1][0][0]= -0.833932; e_m[1][0][0]= -0.579935;
	  a_m[1][0][1]= 5.105520;  b_m[1][0][1]= -1.200000; c_m[1][0][1]= -2.802985; d_m[1][0][1]= -0.892532; e_m[1][0][1]= -0.735736;
	  a_m[1][1][0]= 1.855989;  b_m[1][1][0]= -1.200000; c_m[1][1][0]= -0.539716; d_m[1][1][0]= 0.185337;  e_m[1][1][0]= -0.304400;
	  a_m[1][1][1]= 1.796571;  b_m[1][1][1]= -1.200000; c_m[1][1][1]= -0.714321; d_m[1][1][1]= 0.067511;  e_m[1][1][1]= -0.334426;
	  a_m[1][2][0]= -2.018450; b_m[1][2][0]= -1.200000; c_m[1][2][0]= 1.785077;  d_m[1][2][0]= 1.125622;  e_m[1][2][0]= 0.248491;
	  a_m[1][2][1]= -1.814088; b_m[1][2][1]= -1.200000; c_m[1][2][1]= 1.773339;  d_m[1][2][1]= 1.027332;  e_m[1][2][1]= 0.321631;
	  a_m[1][3][0]= -4.711494; b_m[1][3][0]= -1.200000; c_m[1][3][0]= 3.749677;  d_m[1][3][0]= 2.217679;  e_m[1][3][0]= 0.525359;
	  a_m[1][3][1]= -4.942504; b_m[1][3][1]= -1.200000; c_m[1][3][1]= 3.925406;  d_m[1][3][1]= 2.126664;  e_m[1][3][1]= 0.682045;

	  a_s[0][0][0]= 1.104421; b_s[0][0][0]= 2.839648;
	  a_s[0][0][1]= 1.105520; b_s[0][0][1]= 2.904110;
	  a_s[0][1][0]= 1.108990; b_s[0][1][0]= 2.434485;
	  a_s[0][1][1]= 1.079131; b_s[0][1][1]= 2.470090;
	  a_s[0][2][0]= 1.126352; b_s[0][2][0]= 2.404266;
	  a_s[0][2][1]= 1.047349; b_s[0][2][1]= 2.564386;
	  a_s[0][3][0]= 1.147538; b_s[0][3][0]= 2.824512;
	  a_s[0][3][1]= 1.123564; b_s[0][3][1]= 2.923090;
	  a_s[1][0][0]= 0.960018; b_s[1][0][0]= 3.036440;
	  a_s[1][0][1]= 0.851378; b_s[1][0][1]= 3.050155;
	  a_s[1][1][0]= 1.046791; b_s[1][1][0]= 2.579896;
	  a_s[1][1][1]= 0.954430; b_s[1][1][1]= 2.583615;
	  a_s[1][2][0]= 1.070591; b_s[1][2][0]= 2.553380;
	  a_s[1][2][1]= 0.960900; b_s[1][2][1]= 2.720107;
	  a_s[1][3][0]= 0.992814; b_s[1][3][0]= 3.024283;
	  a_s[1][3][1]= 0.953259; b_s[1][3][1]= 3.093445;
	}
      else if(i_bbcsum==3)
	{
	  a_m[0][0][0]= 4.980548;  b_m[0][0][0]= -1.200000; c_m[0][0][0]= -2.557891; d_m[0][0][0]= -0.430410; e_m[0][0][0]= -0.910060;
	  a_m[0][0][1]= 4.167154;  b_m[0][0][1]= -1.200000; c_m[0][0][1]= -2.895227; d_m[0][0][1]= -1.200302; e_m[0][0][1]= -0.706771;
	  a_m[0][1][0]= 2.065909;  b_m[0][1][0]= -1.200000; c_m[0][1][0]= -0.538794; d_m[0][1][0]= 0.356101;  e_m[0][1][0]= -0.416760;
	  a_m[0][1][1]= 1.269573;  b_m[0][1][1]= -1.200000; c_m[0][1][1]= -0.677754; d_m[0][1][1]= -0.091183; e_m[0][1][1]= -0.272602;
	  a_m[0][2][0]= -2.212168; b_m[0][2][0]= -1.200000; c_m[0][2][0]= 1.831810;  d_m[0][2][0]= 0.932751;  e_m[0][2][0]= 0.397743;
	  a_m[0][2][1]= -2.032600; b_m[0][2][1]= -1.200000; c_m[0][2][1]= 1.878265;  d_m[0][2][1]= 0.905075;  e_m[0][2][1]= 0.462725;
	  a_m[0][3][0]= -4.783207; b_m[0][3][0]= -1.200000; c_m[0][3][0]= 3.828480;  d_m[0][3][0]= 1.725630;  e_m[0][3][0]= 0.921245;
	  a_m[0][3][1]= -4.048088; b_m[0][3][1]= -1.200000; c_m[0][3][1]= 4.021489;  d_m[0][3][1]= 2.103158;  e_m[0][3][1]= 0.863402;
	  a_m[1][0][0]= 4.472456;  b_m[1][0][0]= -1.200000; c_m[1][0][0]= -2.390453; d_m[1][0][0]= -0.807595; e_m[1][0][0]= -0.587141;
	  a_m[1][0][1]= 4.540310;  b_m[1][0][1]= -1.200000; c_m[1][0][1]= -2.737240; d_m[1][0][1]= -0.843433; e_m[1][0][1]= -0.759477;
	  a_m[1][1][0]= 1.079018;  b_m[1][1][0]= -1.200000; c_m[1][1][0]= -0.424143; d_m[1][1][0]= 0.240840;  e_m[1][1][0]= -0.323406;
	  a_m[1][1][1]= 2.203111;  b_m[1][1][1]= -1.200000; c_m[1][1][1]= -0.713985; d_m[1][1][1]= 0.110130;  e_m[1][1][1]= -0.340602;
	  a_m[1][2][0]= -2.023987; b_m[1][2][0]= -1.200000; c_m[1][2][0]= 1.810563;  d_m[1][2][0]= 1.153867;  e_m[1][2][0]= 0.251011;
	  a_m[1][2][1]= -2.067919; b_m[1][2][1]= -1.200000; c_m[1][2][1]= 1.841623;  d_m[1][2][1]= 1.047987;  e_m[1][2][1]= 0.322814;
	  a_m[1][3][0]= -4.767163; b_m[1][3][0]= -1.200000; c_m[1][3][0]= 3.823681;  d_m[1][3][0]= 2.257801;  e_m[1][3][0]= 0.528502;
	  a_m[1][3][1]= -4.392721; b_m[1][3][1]= -1.200000; c_m[1][3][1]= 3.902109;  d_m[1][3][1]= 2.158037;  e_m[1][3][1]= 0.691278;

	  a_s[0][0][0]= 1.139413; b_s[0][0][0]= 3.097918;
	  a_s[0][0][1]= 1.141574; b_s[0][0][1]= 3.155654;
	  a_s[0][1][0]= 1.172837; b_s[0][1][0]= 2.714888;
	  a_s[0][1][1]= 1.139156; b_s[0][1][1]= 2.754073;
	  a_s[0][2][0]= 1.155685; b_s[0][2][0]= 2.711113;
	  a_s[0][2][1]= 1.093500; b_s[0][2][1]= 2.850422;
	  a_s[0][3][0]= 1.172758; b_s[0][3][0]= 3.092739;
	  a_s[0][3][1]= 1.144431; b_s[0][3][1]= 3.190668;
	  a_s[1][0][0]= 0.997750; b_s[1][0][0]= 3.280772;
	  a_s[1][0][1]= 0.906032; b_s[1][0][1]= 3.284282;
	  a_s[1][1][0]= 1.118434; b_s[1][1][0]= 2.841292;
	  a_s[1][1][1]= 1.009214; b_s[1][1][1]= 2.857495;
	  a_s[1][2][0]= 1.117819; b_s[1][2][0]= 2.830514;
	  a_s[1][2][1]= 1.042365; b_s[1][2][1]= 2.991574;
	  a_s[1][3][0]= 1.042146; b_s[1][3][0]= 3.271206;
	  a_s[1][3][1]= 1.004457; b_s[1][3][1]= 3.342293;
	}
      else
	{
	  return -9999;
	}

    }
  else if(b_field==-1)
    {

      if(i_bbcsum==0)
	{
	  a_m[0][0][0]= 4.725694;  b_m[0][0][0]= -1.200000; c_m[0][0][0]= -2.586857; d_m[0][0][0]= -0.774454; e_m[0][0][0]= -0.734678;
	  a_m[0][0][1]= 5.207522;  b_m[0][0][1]= -1.200000; c_m[0][0][1]= -2.997681; d_m[0][0][1]= -0.877091; e_m[0][0][1]= -0.881909;
	  a_m[0][1][0]= 1.810236;  b_m[0][1][0]= -1.200000; c_m[0][1][0]= -0.609562; d_m[0][1][0]= 0.156578;  e_m[0][1][0]= -0.337729;
	  a_m[0][1][1]= 2.070068;  b_m[0][1][1]= -1.200000; c_m[0][1][1]= -0.856512; d_m[0][1][1]= 0.008043;  e_m[0][1][1]= -0.368580;
	  a_m[0][2][0]= -1.827980; b_m[0][2][0]= -1.200000; c_m[0][2][0]= 1.744294;  d_m[0][2][0]= 1.003978;  e_m[0][2][0]= 0.313547;
	  a_m[0][2][1]= -1.656321; b_m[0][2][1]= -1.200000; c_m[0][2][1]= 1.742048;  d_m[0][2][1]= 0.904807;  e_m[0][2][1]= 0.397370;
	  a_m[0][3][0]= -4.754308; b_m[0][3][0]= -1.200000; c_m[0][3][0]= 3.814045;  d_m[0][3][0]= 2.031543;  e_m[0][3][0]= 0.697649;
	  a_m[0][3][1]= -4.237639; b_m[0][3][1]= -1.200000; c_m[0][3][1]= 3.876857;  d_m[0][3][1]= 1.921170;  e_m[0][3][1]= 0.850021;
	  a_m[1][0][0]= 5.201531;  b_m[1][0][0]= -1.200000; c_m[1][0][0]= -2.472205; d_m[1][0][0]= -0.640764; e_m[1][0][0]= -0.679829;
	  a_m[1][0][1]= 4.184110;  b_m[1][0][1]= -1.200000; c_m[1][0][1]= -2.818682; d_m[1][0][1]= -1.445724; e_m[1][0][1]= -0.464993;
	  a_m[1][1][0]= 1.815488;  b_m[1][1][0]= -1.200000; c_m[1][1][0]= -0.586441; d_m[1][1][0]= 0.161545;  e_m[1][1][0]= -0.308390;
	  a_m[1][1][1]= 1.342619;  b_m[1][1][1]= -1.200000; c_m[1][1][1]= -0.776307; d_m[1][1][1]= -0.296001; e_m[1][1][1]= -0.186453;
	  a_m[1][2][0]= -1.849837; b_m[1][2][0]= -1.200000; c_m[1][2][0]= 1.667047;  d_m[1][2][0]= 0.877378;  e_m[1][2][0]= 0.345764;
	  a_m[1][2][1]= -1.901968; b_m[1][2][1]= -1.200000; c_m[1][2][1]= 1.729114;  d_m[1][2][1]= 0.854712;  e_m[1][2][1]= 0.398198;
	  a_m[1][3][0]= -4.983516; b_m[1][3][0]= -1.200000; c_m[1][3][0]= 3.571471;  d_m[1][3][0]= 1.721689;  e_m[1][3][0]= 0.709197;
	  a_m[1][3][1]= -4.875386; b_m[1][3][1]= -1.200000; c_m[1][3][1]= 3.783208;  d_m[1][3][1]= 2.057302;  e_m[1][3][1]= 0.646434;

	  a_s[0][0][0]= 1.130807; b_s[0][0][0]= 2.821279;
	  a_s[0][0][1]= 1.054516; b_s[0][0][1]= 2.842358;
	  a_s[0][1][0]= 1.119835; b_s[0][1][0]= 2.398576;
	  a_s[0][1][1]= 1.027878; b_s[0][1][1]= 2.431196;
	  a_s[0][2][0]= 1.124225; b_s[0][2][0]= 2.393763;
	  a_s[0][2][1]= 1.031649; b_s[0][2][1]= 2.519285;
	  a_s[0][3][0]= 1.155137; b_s[0][3][0]= 2.847339;
	  a_s[0][3][1]= 1.115427; b_s[0][3][1]= 2.893481;
	  a_s[1][0][0]= 0.856020; b_s[1][0][0]= 3.009018;
	  a_s[1][0][1]= 0.888369; b_s[1][0][1]= 3.065594;
	  a_s[1][1][0]= 0.997358; b_s[1][1][0]= 2.528375;
	  a_s[1][1][1]= 0.969314; b_s[1][1][1]= 2.579222;
	  a_s[1][2][0]= 1.014185; b_s[1][2][0]= 2.510044;
	  a_s[1][2][1]= 0.940830; b_s[1][2][1]= 2.632785;
	  a_s[1][3][0]= 0.905895; b_s[1][3][0]= 3.031787;
	  a_s[1][3][1]= 0.869244; b_s[1][3][1]= 3.099831;
	}
      else if(i_bbcsum==1)
	{
	  a_m[0][0][0]= 4.792815;  b_m[0][0][0]= -1.200000; c_m[0][0][0]= -2.581375; d_m[0][0][0]= -0.748917; e_m[0][0][0]= -0.738489;
	  a_m[0][0][1]= 5.109424;  b_m[0][0][1]= -1.200000; c_m[0][0][1]= -2.969250; d_m[0][0][1]= -0.834345; e_m[0][0][1]= -0.905869;
	  a_m[0][1][0]= 1.717362;  b_m[0][1][0]= -1.200000; c_m[0][1][0]= -0.573808; d_m[0][1][0]= 0.194811;  e_m[0][1][0]= -0.340521;
	  a_m[0][1][1]= 2.112477;  b_m[0][1][1]= -1.200000; c_m[0][1][1]= -0.832687; d_m[0][1][1]= 0.056836;  e_m[0][1][1]= -0.385079;
	  a_m[0][2][0]= -1.989020; b_m[0][2][0]= -1.200000; c_m[0][2][0]= 1.791720;  d_m[0][2][0]= 1.037982;  e_m[0][2][0]= 0.313553;
	  a_m[0][2][1]= -1.630822; b_m[0][2][1]= -1.200000; c_m[0][2][1]= 1.775524;  d_m[0][2][1]= 0.941035;  e_m[0][2][1]= 0.392011;
	  a_m[0][3][0]= -4.711809; b_m[0][3][0]= -1.200000; c_m[0][3][0]= 3.851920;  d_m[0][3][0]= 2.078324;  e_m[0][3][0]= 0.699285;
	  a_m[0][3][1]= -4.561345; b_m[0][3][1]= -1.200000; c_m[0][3][1]= 3.962445;  d_m[0][3][1]= 1.952331;  e_m[0][3][1]= 0.869587;
	  a_m[1][0][0]= 5.026971;  b_m[1][0][0]= -1.200000; c_m[1][0][0]= -2.450716; d_m[1][0][0]= -0.602517; e_m[1][0][0]= -0.703034;
	  a_m[1][0][1]= 4.416495;  b_m[1][0][1]= -1.200000; c_m[1][0][1]= -2.843697; d_m[1][0][1]= -1.415934; e_m[1][0][1]= -0.477607;
	  a_m[1][1][0]= 1.999660;  b_m[1][1][0]= -1.200000; c_m[1][1][0]= -0.572428; d_m[1][1][0]= 0.208767;  e_m[1][1][0]= -0.323864;
	  a_m[1][1][1]= 1.417201;  b_m[1][1][1]= -1.200000; c_m[1][1][1]= -0.761391; d_m[1][1][1]= -0.261554; e_m[1][1][1]= -0.189262;
	  a_m[1][2][0]= -1.832340; b_m[1][2][0]= -1.200000; c_m[1][2][0]= 1.698427;  d_m[1][2][0]= 0.916858;  e_m[1][2][0]= 0.340363;
	  a_m[1][2][1]= -1.934296; b_m[1][2][1]= -1.200000; c_m[1][2][1]= 1.765450;  d_m[1][2][1]= 0.901318;  e_m[1][2][1]= 0.384643;
	  a_m[1][3][0]= -4.892997; b_m[1][3][0]= -1.200000; c_m[1][3][0]= 3.616653;  d_m[1][3][0]= 1.750405;  e_m[1][3][0]= 0.729719;
	  a_m[1][3][1]= -4.878599; b_m[1][3][1]= -1.200000; c_m[1][3][1]= 3.851419;  d_m[1][3][1]= 2.094065;  e_m[1][3][1]= 0.664667;

	  a_s[0][0][0]= 1.125425; b_s[0][0][0]= 2.816956;
	  a_s[0][0][1]= 1.049420; b_s[0][0][1]= 2.836347;
	  a_s[0][1][0]= 1.123288; b_s[0][1][0]= 2.392709;
	  a_s[0][1][1]= 1.030586; b_s[0][1][1]= 2.426137;
	  a_s[0][2][0]= 1.118681; b_s[0][2][0]= 2.391959;
	  a_s[0][2][1]= 1.028141; b_s[0][2][1]= 2.519357;
	  a_s[0][3][0]= 1.146060; b_s[0][3][0]= 2.852453;
	  a_s[0][3][1]= 1.099169; b_s[0][3][1]= 2.897940;
	  a_s[1][0][0]= 0.874638; b_s[1][0][0]= 2.992788;
	  a_s[1][0][1]= 0.907692; b_s[1][0][1]= 3.048134;
	  a_s[1][1][0]= 0.998541; b_s[1][1][0]= 2.524375;
	  a_s[1][1][1]= 0.984732; b_s[1][1][1]= 2.570806;
	  a_s[1][2][0]= 1.021465; b_s[1][2][0]= 2.502961;
	  a_s[1][2][1]= 0.987069; b_s[1][2][1]= 2.622299;
	  a_s[1][3][0]= 0.917368; b_s[1][3][0]= 3.026446;
	  a_s[1][3][1]= 0.908860; b_s[1][3][1]= 3.085161;
	}
      else if(i_bbcsum==2)
	{
	  a_m[0][0][0]= 4.798032;  b_m[0][0][0]= -1.200000; c_m[0][0][0]= -2.553445; d_m[0][0][0]= -0.710183; e_m[0][0][0]= -0.745899;
	  a_m[0][0][1]= 5.134794;  b_m[0][0][1]= -1.200000; c_m[0][0][1]= -2.954496; d_m[0][0][1]= -0.791665; e_m[0][0][1]= -0.911546;
	  a_m[0][1][0]= 2.008805;  b_m[0][1][0]= -1.200000; c_m[0][1][0]= -0.548492; d_m[0][1][0]= 0.260670;  e_m[0][1][0]= -0.357986;
	  a_m[0][1][1]= 1.975965;  b_m[0][1][1]= -1.200000; c_m[0][1][1]= -0.764010; d_m[0][1][1]= 0.120540;  e_m[0][1][1]= -0.397695;
	  a_m[0][2][0]= -1.762430; b_m[0][2][0]= -1.200000; c_m[0][2][0]= 1.823274;  d_m[0][2][0]= 1.073283;  e_m[0][2][0]= 0.326093;
	  a_m[0][2][1]= -1.622959; b_m[0][2][1]= -1.200000; c_m[0][2][1]= 1.821201;  d_m[0][2][1]= 0.974566;  e_m[0][2][1]= 0.408233;
	  a_m[0][3][0]= -4.740023; b_m[0][3][0]= -1.200000; c_m[0][3][0]= 3.931319;  d_m[0][3][0]= 2.139158;  e_m[0][3][0]= 0.701887;
	  a_m[0][3][1]= -4.895677; b_m[0][3][1]= -1.200000; c_m[0][3][1]= 4.062043;  d_m[0][3][1]= 2.016391;  e_m[0][3][1]= 0.865965;
	  a_m[1][0][0]= 5.147642;  b_m[1][0][0]= -1.200000; c_m[1][0][0]= -2.443647; d_m[1][0][0]= -0.552986; e_m[1][0][0]= -0.716676;
	  a_m[1][0][1]= 4.429460;  b_m[1][0][1]= -1.200000; c_m[1][0][1]= -2.825519; d_m[1][0][1]= -1.378630; e_m[1][0][1]= -0.488564;
	  a_m[1][1][0]= 1.791854;  b_m[1][1][0]= -1.200000; c_m[1][1][0]= -0.507845; d_m[1][1][0]= 0.271142;  e_m[1][1][0]= -0.339183;
	  a_m[1][1][1]= 1.500381;  b_m[1][1][1]= -1.200000; c_m[1][1][1]= -0.725073; d_m[1][1][1]= -0.195777; e_m[1][1][1]= -0.205897;
	  a_m[1][2][0]= -1.846488; b_m[1][2][0]= -1.200000; c_m[1][2][0]= 1.754715;  d_m[1][2][0]= 0.961862;  e_m[1][2][0]= 0.344092;
	  a_m[1][2][1]= -2.119123; b_m[1][2][1]= -1.200000; c_m[1][2][1]= 1.845893;  d_m[1][2][1]= 0.941934;  e_m[1][2][1]= 0.393510;
	  a_m[1][3][0]= -4.662725; b_m[1][3][0]= -1.200000; c_m[1][3][0]= 3.666045;  d_m[1][3][0]= 1.806916;  e_m[1][3][0]= 0.740214;
	  a_m[1][3][1]= -4.562960; b_m[1][3][1]= -1.200000; c_m[1][3][1]= 3.909356;  d_m[1][3][1]= 2.158823;  e_m[1][3][1]= 0.671349;

	  a_s[0][0][0]= 1.127826; b_s[0][0][0]= 2.847853;
	  a_s[0][0][1]= 1.055707; b_s[0][0][1]= 2.863749;
	  a_s[0][1][0]= 1.134912; b_s[0][1][0]= 2.428271;
	  a_s[0][1][1]= 1.040543; b_s[0][1][1]= 2.463138;
	  a_s[0][2][0]= 1.133373; b_s[0][2][0]= 2.431917;
	  a_s[0][2][1]= 1.059668; b_s[0][2][1]= 2.547694;
	  a_s[0][3][0]= 1.150942; b_s[0][3][0]= 2.886764;
	  a_s[0][3][1]= 1.102112; b_s[0][3][1]= 2.931211;
	  a_s[1][0][0]= 0.914945; b_s[1][0][0]= 3.010604;
	  a_s[1][0][1]= 0.907651; b_s[1][0][1]= 3.075559;
	  a_s[1][1][0]= 1.014677; b_s[1][1][0]= 2.560457;
	  a_s[1][1][1]= 1.010763; b_s[1][1][1]= 2.602469;
	  a_s[1][2][0]= 1.037584; b_s[1][2][0]= 2.543966;
	  a_s[1][2][1]= 0.990265; b_s[1][2][1]= 2.673849;
	  a_s[1][3][0]= 0.940226; b_s[1][3][0]= 3.054145;
	  a_s[1][3][1]= 0.937979; b_s[1][3][1]= 3.114060;
	}
      else if(i_bbcsum==3)
	{
	  a_m[0][0][0]= 4.453152;  b_m[0][0][0]= -1.200000; c_m[0][0][0]= -2.494896; d_m[0][0][0]= -0.657661; e_m[0][0][0]= -0.775554;
	  a_m[0][0][1]= 5.082455;  b_m[0][0][1]= -1.200000; c_m[0][0][1]= -2.925634; d_m[0][0][1]= -0.726803; e_m[0][0][1]= -0.945672;
	  a_m[0][1][0]= 2.183427;  b_m[0][1][0]= -1.200000; c_m[0][1][0]= -0.525633; d_m[0][1][0]= 0.304299;  e_m[0][1][0]= -0.358798;
	  a_m[0][1][1]= 2.032533;  b_m[0][1][1]= -1.200000; c_m[0][1][1]= -0.725163; d_m[0][1][1]= 0.158796;  e_m[0][1][1]= -0.400678;
	  a_m[0][2][0]= -1.750285; b_m[0][2][0]= -1.200000; c_m[0][2][0]= 1.856791;  d_m[0][2][0]= 1.114116;  e_m[0][2][0]= 0.316898;
	  a_m[0][2][1]= -1.551168; b_m[0][2][1]= -1.200000; c_m[0][2][1]= 1.835335;  d_m[0][2][1]= 1.005430;  e_m[0][2][1]= 0.400292;
	  a_m[0][3][0]= -4.422087; b_m[0][3][0]= -1.200000; c_m[0][3][0]= 3.928771;  d_m[0][3][0]= 2.177169;  e_m[0][3][0]= 0.715823;
	  a_m[0][3][1]= -5.282273; b_m[0][3][1]= -1.200000; c_m[0][3][1]= 4.140233;  d_m[0][3][1]= 2.069261;  e_m[0][3][1]= 0.855874;
	  a_m[1][0][0]= 4.649025;  b_m[1][0][0]= -1.200000; c_m[1][0][0]= -2.370115; d_m[1][0][0]= -0.496444; e_m[1][0][0]= -0.758205;
	  a_m[1][0][1]= 4.405061;  b_m[1][0][1]= -1.200000; c_m[1][0][1]= -2.791393; d_m[1][0][1]= -1.339417; e_m[1][0][1]= -0.508599;
	  a_m[1][1][0]= 1.954689;  b_m[1][1][0]= -1.200000; c_m[1][1][0]= -0.479342; d_m[1][1][0]= 0.314314;  e_m[1][1][0]= -0.343820;
	  a_m[1][1][1]= 1.182984;  b_m[1][1][1]= -1.200000; c_m[1][1][1]= -0.654162; d_m[1][1][1]= -0.172678; e_m[1][1][1]= -0.194822;
	  a_m[1][2][0]= -1.562748; b_m[1][2][0]= -1.200000; c_m[1][2][0]= 1.750568;  d_m[1][2][0]= 0.972116;  e_m[1][2][0]= 0.361152;
	  a_m[1][2][1]= -2.135499; b_m[1][2][1]= -1.200000; c_m[1][2][1]= 1.854031;  d_m[1][2][1]= 0.946851;  e_m[1][2][1]= 0.410607;
	  a_m[1][3][0]= -3.533942; b_m[1][3][0]= -1.200000; c_m[1][3][0]= 3.603065;  d_m[1][3][0]= 1.854007;  e_m[1][3][0]= 0.731317;
	  a_m[1][3][1]= -4.111356; b_m[1][3][1]= -1.200000; c_m[1][3][1]= 3.938005;  d_m[1][3][1]= 2.155657;  e_m[1][3][1]= 0.713842;

	  a_s[0][0][0]= 1.133837; b_s[0][0][0]= 3.114301;
	  a_s[0][0][1]= 1.085118; b_s[0][0][1]= 3.120749;
	  a_s[0][1][0]= 1.202048; b_s[0][1][0]= 2.712995;
	  a_s[0][1][1]= 1.077048; b_s[0][1][1]= 2.769732;
	  a_s[0][2][0]= 1.190101; b_s[0][2][0]= 2.729593;
	  a_s[0][2][1]= 1.133347; b_s[0][2][1]= 2.824716;
	  a_s[0][3][0]= 1.194045; b_s[0][3][0]= 3.152788;
	  a_s[0][3][1]= 1.121400; b_s[0][3][1]= 3.207351;
	  a_s[1][0][0]= 0.980458; b_s[1][0][0]= 3.241461;
	  a_s[1][0][1]= 0.939961; b_s[1][0][1]= 3.324188;
	  a_s[1][1][0]= 1.086014; b_s[1][1][0]= 2.832984;
	  a_s[1][1][1]= 1.076555; b_s[1][1][1]= 2.879870;
	  a_s[1][2][0]= 1.083638; b_s[1][2][0]= 2.829660;
	  a_s[1][2][1]= 1.059462; b_s[1][2][1]= 2.964804;
	  a_s[1][3][0]= 1.019070; b_s[1][3][0]= 3.291507;
	  a_s[1][3][1]= 0.980174; b_s[1][3][1]= 3.366194;
	}
      else
	{
	  return -9999;
	}

    }
  else
    {
      return -9999;
    }

  float m_dz = 0;
  float s_dz = 1;
  
  if(pt>1.5)       m_dz =  a_m[i_ch][i_zed][i_phi] * exp(b_m[i_ch][i_zed][i_phi]*pt) + c_m[i_ch][i_zed][i_phi];
  else if(pt<=1.5) m_dz =  d_m[i_ch][i_zed][i_phi] + e_m[i_ch][i_zed][i_phi]*pt;

  s_dz = sqrt( a_s[i_ch][i_zed][i_phi]*a_s[i_ch][i_zed][i_phi]/pt/pt + b_s[i_ch][i_zed][i_phi]*b_s[i_ch][i_zed][i_phi] );

  float sdz  = -9999;
  if(s_dz>0) sdz = (dz - m_dz)/s_dz;      

  return sdz;

}







// Getpc2sdphi_AB_Run5CuCu
float MatchrecalRecoRun5::Getpc2sdphi_AB_Run5CuCu(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dphi)
{

  if(dphi==-9999) return -9999;

  if(i_phi==1) return -9999; // <-- For PC2.

  // Parameters
  float a_m[2][4][2],b_m[2][4][2],c_m[2][4][2];
  float a_s[2][4][2],b_s[2][4][2],c_s[2][4][2];

  if(b_field==1)
    {
      a_m[0][0][0]= -2.621425; b_m[0][0][0]= -4.633405;  c_m[0][0][0]= -0.025894;
      a_m[0][1][0]= 0.578953;  b_m[0][1][0]= -10.547779; c_m[0][1][0]= -0.011232;
      a_m[0][2][0]= 0.924102;  b_m[0][2][0]= -14.466429; c_m[0][2][0]= -0.003042;
      a_m[0][3][0]= -2.689494; b_m[0][3][0]= -4.762798;  c_m[0][3][0]= -0.018011;
      a_m[1][0][0]= 2.678659;  b_m[1][0][0]= -4.925399;  c_m[1][0][0]= 0.025731;
      a_m[1][1][0]= -0.515281; b_m[1][1][0]= -11.357586; c_m[1][1][0]= 0.009220;
      a_m[1][2][0]= -0.524686; b_m[1][2][0]= -11.064453; c_m[1][2][0]= 0.009723;
      a_m[1][3][0]= 2.565810;  b_m[1][3][0]= -4.937619;  c_m[1][3][0]= 0.022966;

      a_s[0][0][0]= 5.329920; b_s[0][0][0]= -9.572003; c_s[0][0][0]= 0.929764;
      a_s[0][1][0]= 0.511334; b_s[0][1][0]= -6.491470; c_s[0][1][0]= 0.928343;
      a_s[0][2][0]= 0.550748; b_s[0][2][0]= -5.539123; c_s[0][2][0]= 0.910318;
      a_s[0][3][0]= 5.525588; b_s[0][3][0]= -9.520925; c_s[0][3][0]= 0.922462;
      a_s[1][0][0]= 5.135982; b_s[1][0][0]= -9.932173; c_s[1][0][0]= 0.921113;
      a_s[1][1][0]= 0.408055; b_s[1][1][0]= -4.933444; c_s[1][1][0]= 0.917927;
      a_s[1][2][0]= 0.423658; b_s[1][2][0]= -4.372385; c_s[1][2][0]= 0.902018;
      a_s[1][3][0]= 5.278187; b_s[1][3][0]= -10.055516; c_s[1][3][0]= 0.925208;
    }
  else if(b_field==-1)
    {
      a_m[0][0][0]= 2.543854;    b_m[0][0][0]= -5.008500;  c_m[0][0][0]= 0.027251;
      a_m[0][1][0]= -0.451838;   b_m[0][1][0]= -9.167613;  c_m[0][1][0]= 0.007181;
      a_m[0][2][0]= -266.735779; b_m[0][2][0]= -75.936218; c_m[0][2][0]= 0.009381;
      a_m[0][3][0]= 2.770411;    b_m[0][3][0]= -4.810205;  c_m[0][3][0]= 0.022376;
      a_m[1][0][0]= -2.293996;   b_m[1][0][0]= -4.785532;  c_m[1][0][0]= -0.022121;
      a_m[1][1][0]= 0.558686;    b_m[1][1][0]= -7.038810;  c_m[1][1][0]= -0.009034;
      a_m[1][2][0]= 0.814566;    b_m[1][2][0]= -12.731340; c_m[1][2][0]= -0.006423;
      a_m[1][3][0]= -2.596116;   b_m[1][3][0]= -4.624861;  c_m[1][3][0]= -0.014953;

      a_s[0][0][0]= 5.251951; b_s[0][0][0]= -10.077770; c_s[0][0][0]= 0.914263;
      a_s[0][1][0]= 0.446974; b_s[0][1][0]= -6.758409;  c_s[0][1][0]= 0.924978;
      a_s[0][2][0]= 0.458263; b_s[0][2][0]= -5.657904;  c_s[0][2][0]= 0.901187;
      a_s[0][3][0]= 5.622466; b_s[0][3][0]= -10.173546; c_s[0][3][0]= 0.929063;
      a_s[1][0][0]= 5.122475; b_s[1][0][0]= -9.991574;  c_s[1][0][0]= 0.938068;
      a_s[1][1][0]= 0.508055; b_s[1][1][0]= -4.715918;  c_s[1][1][0]= 0.928579;
      a_s[1][2][0]= 0.525047; b_s[1][2][0]= -4.218955;  c_s[1][2][0]= 0.910511;
      a_s[1][3][0]= 5.362937; b_s[1][3][0]= -9.464120;  c_s[1][3][0]= 0.929607;
    }
  else
    {
      return -9999;
    }

  float m_dphi = 0;
  float s_dphi = 1;

  m_dphi = a_m[i_ch][i_zed][i_phi] * exp(b_m[i_ch][i_zed][i_phi]*pt) + c_m[i_ch][i_zed][i_phi] ;
  s_dphi = a_s[i_ch][i_zed][i_phi] * exp(b_s[i_ch][i_zed][i_phi]*pt) + c_s[i_ch][i_zed][i_phi] ;

  float sdphi  = -9999;
  if(s_dphi>0) sdphi = (dphi - m_dphi)/s_dphi;      

  return sdphi;

}



// Getpc3sdphi_AB_Run5CuCu
float MatchrecalRecoRun5::Getpc3sdphi_AB_Run5CuCu(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dphi)
{

  if(dphi==-9999) return -9999;

  // Parameters
  float a_m[2][4][2],b_m[2][4][2],c_m[2][4][2];
  float a_s[2][4][2],b_s[2][4][2],c_s[2][4][2];

  if(b_field==1)
    {
      a_m[0][0][0]= -1.734279;   b_m[0][0][0]= -3.346509;  c_m[0][0][0]= -0.003181;
      a_m[0][0][1]= -1.713248;   b_m[0][0][1]= -3.435493;  c_m[0][0][1]= -0.000553;
      a_m[0][1][0]= 0.05;        b_m[0][1][0]= -2;         c_m[0][1][0]= -0.02;
      a_m[0][1][1]= 593.987000;  b_m[0][1][1]= -87.533607; c_m[0][1][1]= -0.009848;
      a_m[0][2][0]= 28.666771;   b_m[0][2][0]= -60.993317; c_m[0][2][0]= -0.007099;
      a_m[0][2][1]= 455.797852;  b_m[0][2][1]= -88.387634; c_m[0][2][1]= -0.008500;
      a_m[0][3][0]= -1.688646;   b_m[0][3][0]= -3.297406;  c_m[0][3][0]= 0.003178;
      a_m[0][3][1]= -1.866439;   b_m[0][3][1]= -3.466884;  c_m[0][3][1]= 0.000944;
      a_m[1][0][0]= 1.803105;    b_m[1][0][0]= -3.488111;  c_m[1][0][0]= 0.004593;
      a_m[1][0][1]= 1.572519;    b_m[1][0][1]= -3.425851;  c_m[1][0][1]= 0.027255;
      a_m[1][1][0]= 0.018061;    b_m[1][1][0]= -1.560993;  c_m[1][1][0]= 0.008548;
      a_m[1][1][1]= -319.181824; b_m[1][1][1]= -78.655945; c_m[1][1][1]= 0.008812;
      a_m[1][2][0]= 0.031192;    b_m[1][2][0]= -1.513650;  c_m[1][2][0]= 0.007837;
      a_m[1][2][1]= -389.454102; b_m[1][2][1]= -82.728546; c_m[1][2][1]= 0.006857;
      a_m[1][3][0]= 1.761509;    b_m[1][3][0]= -3.560390;  c_m[1][3][0]= 0.000416;
      a_m[1][3][1]= 1.759184;    b_m[1][3][1]= -3.341089;  c_m[1][3][1]= 0.000823;
      
      a_s[0][0][0]= 2.149984; b_s[0][0][0]= -5.894949; c_s[0][0][0]= 0.896626;
      a_s[0][0][1]= 2.269542; b_s[0][0][1]= -6.288278; c_s[0][0][1]= 0.888943;
      a_s[0][1][0]= 0.499072; b_s[0][1][0]= -5.465285; c_s[0][1][0]= 0.896249;
      a_s[0][1][1]= 0.536226; b_s[0][1][1]= -5.538710; c_s[0][1][1]= 0.887551;
      a_s[0][2][0]= 0.520963; b_s[0][2][0]= -5.459202; c_s[0][2][0]= 0.898653;
      a_s[0][2][1]= 0.532797; b_s[0][2][1]= -5.437765; c_s[0][2][1]= 0.883341;
      a_s[0][3][0]= 2.310378; b_s[0][3][0]= -6.352923; c_s[0][3][0]= 0.906707;
      a_s[0][3][1]= 2.250830; b_s[0][3][1]= -6.113011; c_s[0][3][1]= 0.894453;
      a_s[1][0][0]= 2.237139; b_s[1][0][0]= -6.551824; c_s[1][0][0]= 0.885321;
      a_s[1][0][1]= 2.300988; b_s[1][0][1]= -6.246789; c_s[1][0][1]= 0.862625;
      a_s[1][1][0]= 0.455496; b_s[1][1][0]= -4.856327; c_s[1][1][0]= 0.876470;
      a_s[1][1][1]= 0.534496; b_s[1][1][1]= -4.842080; c_s[1][1][1]= 0.883615;
      a_s[1][2][0]= 0.455721; b_s[1][2][0]= -4.873035; c_s[1][2][0]= 0.884157;
      a_s[1][2][1]= 0.559309; b_s[1][2][1]= -4.743444; c_s[1][2][1]= 0.884328;
      a_s[1][3][0]= 2.278328; b_s[1][3][0]= -6.632694; c_s[1][3][0]= 0.898817;
      a_s[1][3][1]= 2.376618; b_s[1][3][1]= -6.354586; c_s[1][3][1]= 0.889691;
    }
  else if(b_field==-1)
    {
      a_m[0][0][0]= 1.675586;    b_m[0][0][0]= -3.446800;  c_m[0][0][0]= 0.009690;
      a_m[0][0][1]= 1.441859;    b_m[0][0][1]= -3.353493;  c_m[0][0][1]= 0.028690;
      a_m[0][1][0]= -16.708542;  b_m[0][1][0]= -62.379520; c_m[0][1][0]= 0.013148;
      a_m[0][1][1]= -157.552612; b_m[0][1][1]= -70.415665; c_m[0][1][1]= 0.013258;
      a_m[0][2][0]= 0.056375;    b_m[0][2][0]= -1.609489;  c_m[0][2][0]= 0.005128;
      a_m[0][2][1]= 145.590073;  b_m[0][2][1]= 0.000020;   c_m[0][2][1]= -145.583130;
      a_m[0][3][0]= 1.779311;    b_m[0][3][0]= -3.322345;  c_m[0][3][0]= -0.004632;
      a_m[0][3][1]= 1.765543;    b_m[0][3][1]= -3.155026;  c_m[0][3][1]= -0.004294;
      a_m[1][0][0]= -1.630087;   b_m[1][0][0]= -3.469291;  c_m[1][0][0]= 0.002057;
      a_m[1][0][1]= -1.615133;   b_m[1][0][1]= -3.598298;  c_m[1][0][1]= -0.005203;
      a_m[1][1][0]= 0.300837;    b_m[1][1][0]= -10.331965; c_m[1][1][0]= -0.008923;
      a_m[1][1][1]= 0.346427;    b_m[1][1][1]= -9.907279;  c_m[1][1][1]= -0.005267;
      a_m[1][2][0]= -190.676071; b_m[1][2][0]= 0.000023;   c_m[1][2][0]= 190.677856;
      a_m[1][2][1]= -261.075531; b_m[1][2][1]= 0.000022;   c_m[1][2][1]= 261.080994;
      a_m[1][3][0]= -1.713442;   b_m[1][3][0]= -3.240776;  c_m[1][3][0]= 0.009786;
      a_m[1][3][1]= -1.870920;   b_m[1][3][1]= -3.391754;  c_m[1][3][1]= -0.000336;

      a_s[0][0][0]= 2.156320; b_s[0][0][0]= -6.407017; c_s[0][0][0]= 0.894821;
      a_s[0][0][1]= 2.189976; b_s[0][0][1]= -6.005532; c_s[0][0][1]= 0.862630;
      a_s[0][1][0]= 0.480592; b_s[0][1][0]= -5.334855; c_s[0][1][0]= 0.884603;
      a_s[0][1][1]= 0.527449; b_s[0][1][1]= -5.174195; c_s[0][1][1]= 0.886260;
      a_s[0][2][0]= 0.471575; b_s[0][2][0]= -5.349191; c_s[0][2][0]= 0.893937;
      a_s[0][2][1]= 0.535833; b_s[0][2][1]= -4.916038; c_s[0][2][1]= 0.878286;
      a_s[0][3][0]= 2.141740; b_s[0][3][0]= -6.015268; c_s[0][3][0]= 0.904350;
      a_s[0][3][1]= 2.277297; b_s[0][3][1]= -5.853403; c_s[0][3][1]= 0.890691;
      a_s[1][0][0]= 2.251019; b_s[1][0][0]= -6.440630; c_s[1][0][0]= 0.899530;
      a_s[1][0][1]= 2.390691; b_s[1][0][1]= -6.834228; c_s[1][0][1]= 0.887115;
      a_s[1][1][0]= 0.478730; b_s[1][1][0]= -4.938652; c_s[1][1][0]= 0.892178;
      a_s[1][1][1]= 0.545739; b_s[1][1][1]= -5.201107; c_s[1][1][1]= 0.884059;
      a_s[1][2][0]= 0.495263; b_s[1][2][0]= -4.727518; c_s[1][2][0]= 0.893859;
      a_s[1][2][1]= 0.517649; b_s[1][2][1]= -4.735703; c_s[1][2][1]= 0.874692;
      a_s[1][3][0]= 2.384572; b_s[1][3][0]= -6.322931; c_s[1][3][0]= 0.902485;
      a_s[1][3][1]= 2.291169; b_s[1][3][1]= -5.987862; c_s[1][3][1]= 0.884662;
    }
  else
    {
      return -9999;
    }

  float m_dphi = 0;
  float s_dphi = 1;

  m_dphi = a_m[i_ch][i_zed][i_phi] * exp(b_m[i_ch][i_zed][i_phi]*pt) + c_m[i_ch][i_zed][i_phi] ;
  s_dphi = a_s[i_ch][i_zed][i_phi] * exp(b_s[i_ch][i_zed][i_phi]*pt) + c_s[i_ch][i_zed][i_phi] ;

  float sdphi  = -9999;
  if(s_dphi>0) sdphi = (dphi - m_dphi)/s_dphi;      

  return sdphi;

}




// Gettofsdphi_AB_Run5CuCu
float MatchrecalRecoRun5::Gettofsdphi_AB_Run5CuCu(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dphi)
{

  if(dphi==-9999) return -9999;

  if(i_phi==0) return -9999; // <-- For TOF.

  // Parameters
  float a_m[2][4][2],b_m[2][4][2],c_m[2][4][2];
  float a_s[2][4][2],b_s[2][4][2],c_s[2][4][2];

  if(b_field==1)
    {
      a_m[0][0][1]= -1.110346;  b_m[0][0][1]= -3.952576;  c_m[0][0][1]= 0.000749;
      a_m[0][1][1]= -0.214911;  b_m[0][1][1]= -7.121745;  c_m[0][1][1]= -0.032498;
      a_m[0][2][1]= -0.165886;  b_m[0][2][1]= -9.095329;  c_m[0][2][1]= -0.007571;
      a_m[0][3][1]= -1.892362;  b_m[0][3][1]= -5.600119;  c_m[0][3][1]= 0.000481;
      a_m[1][0][1]= 1.264813;   b_m[1][0][1]= -4.027739;  c_m[1][0][1]= 0.006985;
      a_m[1][1][1]= -0.114446;  b_m[1][1][1]= -2.277546;  c_m[1][1][1]= 0.001286;
      a_m[1][2][1]= -63.576557; b_m[1][2][1]= -67.531364; c_m[1][2][1]= 0.001678;
      a_m[1][3][1]= 1.362593;   b_m[1][3][1]= -3.439764;  c_m[1][3][1]= -0.008533;

      a_s[0][0][1]= 1.590917; b_s[0][0][1]= -6.093370; c_s[0][0][1]= 0.872920;
      a_s[0][1][1]= 0.411215; b_s[0][1][1]= -4.371634; c_s[0][1][1]= 0.889365;
      a_s[0][2][1]= 0.420796; b_s[0][2][1]= -3.976402; c_s[0][2][1]= 0.891037;
      a_s[0][3][1]= 2.894221; b_s[0][3][1]= -7.094763; c_s[0][3][1]= 0.894794;
      a_s[1][0][1]= 1.904407; b_s[1][0][1]= -7.037816; c_s[1][0][1]= 0.881891;
      a_s[1][1][1]= 0.396500; b_s[1][1][1]= -4.130976; c_s[1][1][1]= 0.885410;
      a_s[1][2][1]= 0.336881; b_s[1][2][1]= -5.174034; c_s[1][2][1]= 0.893714;
      a_s[1][3][1]= 1.800578; b_s[1][3][1]= -7.072052; c_s[1][3][1]= 0.909070;
    }
  else if(b_field==-1)
    {
      a_m[0][0][1]= 1.125713;  b_m[0][0][1]= -3.812612; c_m[0][0][1]= 0.007164;
      a_m[0][1][1]= -0.199725; b_m[0][1][1]= -6.969610; c_m[0][1][1]= -0.010189;
      a_m[0][2][1]= 0.155737;  b_m[0][2][1]= 0.020320;  c_m[0][2][1]= -0.156833;
      a_m[0][3][1]= 1.431842;  b_m[0][3][1]= -3.321682; c_m[0][3][1]= -0.009259;
      a_m[1][0][1]= -0.928290; b_m[1][0][1]= -3.858549; c_m[1][0][1]= -0.001343;
      a_m[1][1][1]= -0.166617; b_m[1][1][1]= -5.362175; c_m[1][1][1]= -0.025277;
      a_m[1][2][1]= -0.185845; b_m[1][2][1]= -7.737848; c_m[1][2][1]= -0.006302;
      a_m[1][3][1]= -1.743189; b_m[1][3][1]= -5.242410; c_m[1][3][1]= 0.005418;

      a_s[0][0][1]= 1.683295; b_s[0][0][1]= -6.436670; c_s[0][0][1]= 0.883856;
      a_s[0][1][1]= 0.375355; b_s[0][1][1]= -3.762115; c_s[0][1][1]= 0.877337;
      a_s[0][2][1]= 0.328677; b_s[0][2][1]= -5.716558; c_s[0][2][1]= 0.903926;
      a_s[0][3][1]= 1.820011; b_s[0][3][1]= -6.678172; c_s[0][3][1]= 0.906243;
      a_s[1][0][1]= 1.256417; b_s[1][0][1]= -4.987801; c_s[1][0][1]= 0.857176;
      a_s[1][1][1]= 0.448054; b_s[1][1][1]= -4.430433; c_s[1][1][1]= 0.890688;
      a_s[1][2][1]= 0.431198; b_s[1][2][1]= -4.041662; c_s[1][2][1]= 0.891686;
      a_s[1][3][1]= 2.487827; b_s[1][3][1]= -6.647962; c_s[1][3][1]= 0.897505;
    }
  else
    {
      return -9999;
    }

  float m_dphi = 0;
  float s_dphi = 1;

  m_dphi = a_m[i_ch][i_zed][i_phi] * exp(b_m[i_ch][i_zed][i_phi]*pt) + c_m[i_ch][i_zed][i_phi] ;
  s_dphi = a_s[i_ch][i_zed][i_phi] * exp(b_s[i_ch][i_zed][i_phi]*pt) + c_s[i_ch][i_zed][i_phi] ;

  float sdphi  = -9999;
  if(s_dphi>0) sdphi = (dphi - m_dphi)/s_dphi;      

  return sdphi;

}




// Getemcsdphi_AB_Run5CuCu
float MatchrecalRecoRun5::Getemcsdphi_AB_Run5CuCu(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dphi)
{

  if(dphi==-9999) return -9999;

  // Parameters
  float a_m[2][4][2],b_m[2][4][2],c_m[2][4][2];
  float a_s[2][4][2],b_s[2][4][2],c_s[2][4][2];

  if(b_field==1)
    {
      a_m[0][0][0]= -1.279549;  b_m[0][0][0]= -4.634430; c_m[0][0][0]= -0.008691;
      a_m[0][0][1]= -1.440253;  b_m[0][0][1]= -4.300927; c_m[0][0][1]= 0.004240;
      a_m[0][1][0]= 0.178234;   b_m[0][1][0]= -0.984745; c_m[0][1][0]= -0.036364;
      a_m[0][1][1]= 0.075711;   b_m[0][1][1]= -0.602451; c_m[0][1][1]= -0.011467;
      a_m[0][2][0]= 0.160576;   b_m[0][2][0]= -1.221799; c_m[0][2][0]= -0.019344;
      a_m[0][2][1]= 267.807922; b_m[0][2][1]= -0.000040; c_m[0][2][1]= -267.768372;
      a_m[0][3][0]= -1.281249;  b_m[0][3][0]= -4.348098; c_m[0][3][0]= -0.000114;
      a_m[0][3][1]= -1.481054;  b_m[0][3][1]= -4.143635; c_m[0][3][1]= 0.012308;
      a_m[1][0][0]= 1.443636;   b_m[1][0][0]= -4.426483; c_m[1][0][0]= -0.012297;
      a_m[1][0][1]= 1.090685;   b_m[1][0][1]= -4.361079; c_m[1][0][1]= 0.007520;
      a_m[1][1][0]= -0.093621;  b_m[1][1][0]= -0.707925; c_m[1][1][0]= 0.013516;
      a_m[1][1][1]= -0.215213;  b_m[1][1][1]= -1.181399; c_m[1][1][1]= 0.029177;
      a_m[1][2][0]= -0.17;      b_m[1][2][0]= -2;        c_m[1][2][0]= 0;
      a_m[1][2][1]= -0.148121;  b_m[1][2][1]= -1.067148; c_m[1][2][1]= 0.020740;
      a_m[1][3][0]= 1.421066;   b_m[1][3][0]= -4.472879; c_m[1][3][0]= -0.014863;
      a_m[1][3][1]= 1.017509;   b_m[1][3][1]= -4.114962; c_m[1][3][1]= 0.005982;

      a_s[0][0][0]= 1.572581; b_s[0][0][0]= -6.680762; c_s[0][0][0]= 0.947003;
      a_s[0][0][1]= 1.475277; b_s[0][0][1]= -6.499318; c_s[0][0][1]= 0.949047;
      a_s[0][1][0]= 0.219123; b_s[0][1][0]= -6.947808; c_s[0][1][0]= 0.950736;
      a_s[0][1][1]= 0.248690; b_s[0][1][1]= -7.653214; c_s[0][1][1]= 0.946379;
      a_s[0][2][0]= 0.238417; b_s[0][2][0]= -6.363704; c_s[0][2][0]= 0.950974;
      a_s[0][2][1]= 0.367714; b_s[0][2][1]= -7.984720; c_s[0][2][1]= 0.932685;
      a_s[0][3][0]= 1.619649; b_s[0][3][0]= -6.666134; c_s[0][3][0]= 0.942543;
      a_s[0][3][1]= 1.501140; b_s[0][3][1]= -6.327397; c_s[0][3][1]= 0.932243;
      a_s[1][0][0]= 1.450312; b_s[1][0][0]= -7.441484; c_s[1][0][0]= 0.936637;
      a_s[1][0][1]= 1.519389; b_s[1][0][1]= -7.030383; c_s[1][0][1]= 0.938980;
      a_s[1][1][0]= -0.1;     b_s[1][1][0]= -4.5;      c_s[1][1][0]= 0.94;
      a_s[1][1][1]= 0.349207; b_s[1][1][1]= -7.590701; c_s[1][1][1]= 0.940759;
      a_s[1][2][0]= 0.308357; b_s[1][2][0]= -9.178736; c_s[1][2][0]= 0.927030;
      a_s[1][2][1]= 0.474355; b_s[1][2][1]= -4.769287; c_s[1][2][1]= 0.926707;
      a_s[1][3][0]= 1.418306; b_s[1][3][0]= -6.842786; c_s[1][3][0]= 0.924579;
      a_s[1][3][1]= 1.435813; b_s[1][3][1]= -6.680600; c_s[1][3][1]= 0.922419;
    }
  else if(b_field==-1)
    {
      a_m[0][0][0]= 1.382476;   b_m[0][0][0]= -4.418052; c_m[0][0][0]= -0.007736;
      a_m[0][0][1]= 1.008120;   b_m[0][0][1]= -4.203723; c_m[0][0][1]= 0.007527;
      a_m[0][1][0]= -0.120212;  b_m[0][1][0]= -0.848548; c_m[0][1][0]= 0.021336;
      a_m[0][1][1]= -0.202633;  b_m[0][1][1]= -1.256999; c_m[0][1][1]= 0.023115;
      a_m[0][2][0]= -0.17;      b_m[0][2][0]= -2;        c_m[0][2][0]= 0;
      a_m[0][2][1]= -0.107891;  b_m[0][2][1]= -0.380273; c_m[0][2][1]= 0.046439;
      a_m[0][3][0]= 1.437641;   b_m[0][3][0]= -4.319651; c_m[0][3][0]= -0.009662;
      a_m[0][3][1]= 1.078722;   b_m[0][3][1]= -3.892452; c_m[0][3][1]= -0.001106;
      a_m[1][0][0]= -1.208282;  b_m[1][0][0]= -4.829333; c_m[1][0][0]= -0.008338;
      a_m[1][0][1]= -1.370204;  b_m[1][0][1]= -4.455673; c_m[1][0][1]= 0.005601;
      a_m[1][1][0]= 0.204261;   b_m[1][1][0]= -1.086126; c_m[1][1][0]= -0.033931;
      a_m[1][1][1]= 0.102882;   b_m[1][1][1]= -0.700918; c_m[1][1][1]= -0.012388;
      a_m[1][2][0]= 0.141237;   b_m[1][2][0]= -1.122407; c_m[1][2][0]= -0.015096;
      a_m[1][2][1]= 283.810303; b_m[1][2][1]= -0.000029; c_m[1][2][1]= -283.776825;
      a_m[1][3][0]= -1.304921;  b_m[1][3][0]= -4.348482; c_m[1][3][0]= 0.005432;
      a_m[1][3][1]= -1.525904;  b_m[1][3][1]= -4.163012; c_m[1][3][1]= 0.013853;

      a_s[0][0][0]= 1.445586; b_s[0][0][0]= -6.941732; c_s[0][0][0]= 0.950492;
      a_s[0][0][1]= 1.447681; b_s[0][0][1]= -6.630951; c_s[0][0][1]= 0.952910;
      a_s[0][1][0]= 0.962670; b_s[0][1][0]= -0.001428; c_s[0][1][0]= -0.006060;
      a_s[0][1][1]= 0.291373; b_s[0][1][1]= -7.954238; c_s[0][1][1]= 0.954917;
      a_s[0][2][0]= 0.220061; b_s[0][2][0]= -8.133987; c_s[0][2][0]= 0.941566;
      a_s[0][2][1]= 0.378221; b_s[0][2][1]= -5.724005; c_s[0][2][1]= 0.940082;
      a_s[0][3][0]= 1.421947; b_s[0][3][0]= -6.399640; c_s[0][3][0]= 0.940174;
      a_s[0][3][1]= 1.477969; b_s[0][3][1]= -6.381603; c_s[0][3][1]= 0.939511;
      a_s[1][0][0]= 1.585185; b_s[1][0][0]= -7.669078; c_s[1][0][0]= 0.944008;
      a_s[1][0][1]= 1.481770; b_s[1][0][1]= -6.797526; c_s[1][0][1]= 0.936430;
      a_s[1][1][0]= 0.310398; b_s[1][1][0]= -7.330626; c_s[1][1][0]= 0.943334;
      a_s[1][1][1]= 0.350351; b_s[1][1][1]= -7.264140; c_s[1][1][1]= 0.931535;
      a_s[1][2][0]= 0.327066; b_s[1][2][0]= -6.741928; c_s[1][2][0]= 0.939582;
      a_s[1][2][1]= 0.497615; b_s[1][2][1]= -6.995723; c_s[1][2][1]= 0.917375;
      a_s[1][3][0]= 1.675661; b_s[1][3][0]= -7.008808; c_s[1][3][0]= 0.932265;
      a_s[1][3][1]= 1.563655; b_s[1][3][1]= -6.511658; c_s[1][3][1]= 0.915747;
    }
  else
    {
      return -9999;
    }

  float m_dphi = 0;
  float s_dphi = 1;

  m_dphi = a_m[i_ch][i_zed][i_phi] * exp(b_m[i_ch][i_zed][i_phi]*pt) + c_m[i_ch][i_zed][i_phi] ;
  s_dphi = a_s[i_ch][i_zed][i_phi] * exp(b_s[i_ch][i_zed][i_phi]*pt) + c_s[i_ch][i_zed][i_phi] ;

  float sdphi  = -9999;
  if(s_dphi>0) sdphi = (dphi - m_dphi)/s_dphi;      

  return sdphi;

}





// Getpc2sdz_AB_Run5CuCu
float MatchrecalRecoRun5::Getpc2sdz_AB_Run5CuCu(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dz)
{

  if(dz==-9999) return -9999;

  if(i_phi==1) return -9999; // <-- For PC2.

  // Parameters
  float a_m[2][4][2],b_m[2][4][2],c_m[2][4][2];
  float a_s[2][4][2],b_s[2][4][2],c_s[2][4][2];

  if(b_field==1)
    {
      a_m[0][0][0]= 0.131266;   b_m[0][0][0]= -3.310583; c_m[0][0][0]= 0.006777;
      a_m[0][1][0]= 0.119066;   b_m[0][1][0]= -3.156633; c_m[0][1][0]= -0.000450;
      a_m[0][2][0]= -0.019539;  b_m[0][2][0]= -1.574764; c_m[0][2][0]= -0.002755;
      a_m[0][3][0]= -64.104477; b_m[0][3][0]= -0.000010; c_m[0][3][0]= 64.092651;
      a_m[1][0][0]= -0.139980;  b_m[1][0][0]= -6.145175; c_m[1][0][0]= -0.002114;
      a_m[1][1][0]= 0.069872;   b_m[1][1][0]= -2.534139; c_m[1][1][0]= -0.006324;
      a_m[1][2][0]= 0.163393;   b_m[1][2][0]= -4.744146; c_m[1][2][0]= -0.000908;
      a_m[1][3][0]= 0.398610;   b_m[1][3][0]= -4.428200; c_m[1][3][0]= -0.003681;

      a_s[0][0][0]= 0.225744; b_s[0][0][0]= -4.331605; c_s[0][0][0]= 0.866560;
      a_s[0][1][0]= 0.277825; b_s[0][1][0]= -5.244361; c_s[0][1][0]= 0.864790;
      a_s[0][2][0]= 0.271390; b_s[0][2][0]= -6.146148; c_s[0][2][0]= 0.867051;
      a_s[0][3][0]= 0.234802; b_s[0][3][0]= -5.004974; c_s[0][3][0]= 0.864825;
      a_s[1][0][0]= 0.271926; b_s[1][0][0]= -3.616776; c_s[1][0][0]= 0.882322;
      a_s[1][1][0]= 0.286534; b_s[1][1][0]= -5.510642; c_s[1][1][0]= 0.903133;
      a_s[1][2][0]= 0.319101; b_s[1][2][0]= -5.334864; c_s[1][2][0]= 0.903979;
      a_s[1][3][0]= 0.307524; b_s[1][3][0]= -3.883111; c_s[1][3][0]= 0.884430;
    }
  else if(b_field==-1)
    {
      a_m[0][0][0]= -0.149176;   b_m[0][0][0]= -7.536214; c_m[0][0][0]= -0.001171;
      a_m[0][1][0]= 0.076406;    b_m[0][1][0]= -2.488517; c_m[0][1][0]= -0.008332;
      a_m[0][2][0]= 0.152817;    b_m[0][2][0]= -3.785154; c_m[0][2][0]= -0.007569;
      a_m[0][3][0]= 0.376531;    b_m[0][3][0]= -4.038488; c_m[0][3][0]= -0.008267;
      a_m[1][0][0]= 0.160176;    b_m[1][0][0]= -2.926766; c_m[1][0][0]= 0.005785;
      a_m[1][1][0]= 0.147838;    b_m[1][1][0]= -2.765279; c_m[1][1][0]= -0.006614;
      a_m[1][2][0]= -0.007804;   b_m[1][2][0]= -1.148793; c_m[1][2][0]= -0.006962;
      a_m[1][3][0]= -160.422333; b_m[1][3][0]= 0.000035;  c_m[1][3][0]= 160.412292;

      a_s[0][0][0]= 0.227472; b_s[0][0][0]= -4.790365; c_s[0][0][0]= 0.867484;
      a_s[0][1][0]= 0.273782; b_s[0][1][0]= -7.228034; c_s[0][1][0]= 0.874831;
      a_s[0][2][0]= 0.292514; b_s[0][2][0]= -7.690249; c_s[0][2][0]= 0.875021;
      a_s[0][3][0]= 0.263450; b_s[0][3][0]= -5.216400; c_s[0][3][0]= 0.865411;
      a_s[1][0][0]= 0.289306; b_s[1][0][0]= -3.297737; c_s[1][0][0]= 0.880404;
      a_s[1][1][0]= 0.307514; b_s[1][1][0]= -4.570702; c_s[1][1][0]= 0.897850;
      a_s[1][2][0]= 0.301598; b_s[1][2][0]= -4.686037; c_s[1][2][0]= 0.898442;
      a_s[1][3][0]= 0.288823; b_s[1][3][0]= -3.462354; c_s[1][3][0]= 0.878989;
    }
  else
    {
      return -9999;
    }

  float m_dz = 0;
  float s_dz = 1;
  
  m_dz =  a_m[i_ch][i_zed][i_phi] * exp(b_m[i_ch][i_zed][i_phi]*pt) + c_m[i_ch][i_zed][i_phi];
  s_dz =  a_s[i_ch][i_zed][i_phi] * exp(b_s[i_ch][i_zed][i_phi]*pt) + c_s[i_ch][i_zed][i_phi];

  float sdz  = -9999;
  if(s_dz>0) sdz = (dz - m_dz)/s_dz;      

  return sdz;

}


// Getpc3sdz_AB_Run5CuCu
float MatchrecalRecoRun5::Getpc3sdz_AB_Run5CuCu(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dz)
{

  if(dz==-9999) return -9999;

  // Parameters
  float a_m[2][4][2],b_m[2][4][2],c_m[2][4][2];
  float a_s[2][4][2],b_s[2][4][2],c_s[2][4][2];

  if(b_field==1)
    {
      a_m[0][0][0]= 411.105927; b_m[0][0][0]= 0.000016;   c_m[0][0][0]= -411.107269;
      a_m[0][0][1]= -0.459471;  b_m[0][0][1]= -4.749057;  c_m[0][0][1]= -0.006310;
      a_m[0][1][0]= 0.074198;   b_m[0][1][0]= -2.855553;  c_m[0][1][0]= 0.001567;
      a_m[0][1][1]= -0.261047;  b_m[0][1][1]= -5.359416;  c_m[0][1][1]= -0.004292;
      a_m[0][2][0]= -0.005941;  b_m[0][2][0]= 0.056348;   c_m[0][2][0]= 0.002044;
      a_m[0][2][1]= -0.188711;  b_m[0][2][1]= -3.807636;  c_m[0][2][1]= -0.003998;
      a_m[0][3][0]= 0.198867;   b_m[0][3][0]= -5.091263;  c_m[0][3][0]= -0.010301;
      a_m[0][3][1]= -95.129189; b_m[0][3][1]= 0.000009;   c_m[0][3][1]= 95.129585;
      a_m[1][0][0]= -0.270385;  b_m[1][0][0]= -4.820120;  c_m[1][0][0]= 0.000178;
      a_m[1][0][1]= -0.283112;  b_m[1][0][1]= -11.331577; c_m[1][0][1]= 0.002294;
      a_m[1][1][0]= 0.028886;   b_m[1][1][0]= -1.639293;  c_m[1][1][0]= -0.003216;
      a_m[1][1][1]= -0.231467;  b_m[1][1][1]= -11.108932; c_m[1][1][1]= -0.001956;
      a_m[1][2][0]= 0.164613;   b_m[1][2][0]= -4.188074;  c_m[1][2][0]= -0.001395;
      a_m[1][2][1]= -0.253060;  b_m[1][2][1]= -4.083627;  c_m[1][2][1]= -0.008004;
      a_m[1][3][0]= 0.490119;   b_m[1][3][0]= -4.210233;  c_m[1][3][0]= -0.001676;
      a_m[1][3][1]= -0.195058;  b_m[1][3][1]= -3.610750;  c_m[1][3][1]= -0.008149;

      a_s[0][0][0]= 0.249235; b_s[0][0][0]= -3.771473; c_s[0][0][0]= 0.866321;
      a_s[0][0][1]= 0.269311; b_s[0][0][1]= -4.579882; c_s[0][0][1]= 0.860852;
      a_s[0][1][0]= 0.274965; b_s[0][1][0]= -4.724604; c_s[0][1][0]= 0.864768;
      a_s[0][1][1]= 0.262153; b_s[0][1][1]= -5.233736; c_s[0][1][1]= 0.857708;
      a_s[0][2][0]= 0.276696; b_s[0][2][0]= -4.776758; c_s[0][2][0]= 0.866702;
      a_s[0][2][1]= 0.487;    b_s[0][2][1]= -6.9;      c_s[0][2][1]= 0.86;
      a_s[0][3][0]= 0.268005; b_s[0][3][0]= -4.607562; c_s[0][3][0]= 0.872887;
      a_s[0][3][1]= 0.255628; b_s[0][3][1]= -4.952415; c_s[0][3][1]= 0.862780;
      a_s[1][0][0]= 0.277434; b_s[1][0][0]= -3.160183; c_s[1][0][0]= 0.860625;
      a_s[1][0][1]= 0.260393; b_s[1][0][1]= -3.533362; c_s[1][0][1]= 0.854454;
      a_s[1][1][0]= 0.287590; b_s[1][1][0]= -4.570953; c_s[1][1][0]= 0.879887;
      a_s[1][1][1]= 0.289118; b_s[1][1][1]= -4.217619; c_s[1][1][1]= 0.879287;
      a_s[1][2][0]= 0.310851; b_s[1][2][0]= -4.207944; c_s[1][2][0]= 0.889311;
      a_s[1][2][1]= 0.295643; b_s[1][2][1]= -5.110414; c_s[1][2][1]= 0.872993;
      a_s[1][3][0]= 0.315620; b_s[1][3][0]= -3.379849; c_s[1][3][0]= 0.872917;
      a_s[1][3][1]= 0.301841; b_s[1][3][1]= -3.973994; c_s[1][3][1]= 0.858036;
    }
  else if(b_field==-1)
    {
      a_m[0][0][0]= -0.255035;   b_m[0][0][0]= -5.116137;  c_m[0][0][0]= -0.003476;
      a_m[0][0][1]= -0.316312;   b_m[0][0][1]= -12.398051; c_m[0][0][1]= 0.002141;
      a_m[0][1][0]= 0.041852;    b_m[0][1][0]= -1.774996;  c_m[0][1][0]= -0.008478;
      a_m[0][1][1]= -0.515246;   b_m[0][1][1]= -21.277954; c_m[0][1][1]= -0.007448;
      a_m[0][2][0]= 0.166650;    b_m[0][2][0]= -3.684431;  c_m[0][2][0]= -0.009810;
      a_m[0][2][1]= -0.235193;   b_m[0][2][1]= -4.495111;  c_m[0][2][1]= -0.016416;
      a_m[0][3][0]= 0.482754;    b_m[0][3][0]= -3.984449;  c_m[0][3][0]= -0.010132;
      a_m[0][3][1]= -0.158016;   b_m[0][3][1]= -4.677722;  c_m[0][3][1]= -0.025346;
      a_m[1][0][0]= 83.521324;   b_m[1][0][0]= 0.000010;   c_m[1][0][0]= -83.516068;
      a_m[1][0][1]= -0.468548;   b_m[1][0][1]= -5.271546;  c_m[1][0][1]= -0.005800;
      a_m[1][1][0]= 0.093636;    b_m[1][1][0]= -2.414590;  c_m[1][1][0]= -0.008604;
      a_m[1][1][1]= -0.276943;   b_m[1][1][1]= -5.955795;  c_m[1][1][1]= -0.006270;
      a_m[1][2][0]= -166.946732; b_m[1][2][0]= 0.000021;   c_m[1][2][0]= 166.944275;
      a_m[1][2][1]= -0.179728;   b_m[1][2][1]= -4.028400;  c_m[1][2][1]= -0.013826;
      a_m[1][3][0]= 0.190565;    b_m[1][3][0]= -3.934596;  c_m[1][3][0]= -0.020539;
      a_m[1][3][1]= -324.469574; b_m[1][3][1]= 0.000013;   c_m[1][3][1]= 324.458740;

      a_s[0][0][0]= 0.249384; b_s[0][0][0]= -4.359792; c_s[0][0][0]= 0.868754;
      a_s[0][0][1]= 0.244082; b_s[0][0][1]= -4.614120; c_s[0][0][1]= 0.864735;
      a_s[0][1][0]= 0.258783; b_s[0][1][0]= -4.616556; c_s[0][1][0]= 0.862949;
      a_s[0][1][1]= 0.263115; b_s[0][1][1]= -4.507815; c_s[0][1][1]= 0.857674;
      a_s[0][2][0]= 0.287359; b_s[0][2][0]= -4.701052; c_s[0][2][0]= 0.862021;
      a_s[0][2][1]= 0.266631; b_s[0][2][1]= -3.939627; c_s[0][2][1]= 0.847913;
      a_s[0][3][0]= 0.270558; b_s[0][3][0]= -4.147957; c_s[0][3][0]= 0.865809;
      a_s[0][3][1]= 0.270920; b_s[0][3][1]= -4.420605; c_s[0][3][1]= 0.857967;
      a_s[1][0][0]= 0.271598; b_s[1][0][0]= -3.066451; c_s[1][0][0]= 0.864216;
      a_s[1][0][1]= 0.285412; b_s[1][0][1]= -3.901517; c_s[1][0][1]= 0.855620;
      a_s[1][1][0]= 0.294974; b_s[1][1][0]= -4.679876; c_s[1][1][0]= 0.884267;
      a_s[1][1][1]= 0.292300; b_s[1][1][1]= -4.675159; c_s[1][1][1]= 0.879340;
      a_s[1][2][0]= 0.309442; b_s[1][2][0]= -4.109450; c_s[1][2][0]= 0.891916;
      a_s[1][2][1]= 0.273691; b_s[1][2][1]= -5.493692; c_s[1][2][1]= 0.865584;
      a_s[1][3][0]= 0.297177; b_s[1][3][0]= -3.235617; c_s[1][3][0]= 0.874070;
      a_s[1][3][1]= 0.268106; b_s[1][3][1]= -4.043327; c_s[1][3][1]= 0.855721;
    }
  else
    {
      return -9999;
    }

  float m_dz = 0;
  float s_dz = 1;
  
  m_dz =  a_m[i_ch][i_zed][i_phi] * exp(b_m[i_ch][i_zed][i_phi]*pt) + c_m[i_ch][i_zed][i_phi];
  s_dz =  a_s[i_ch][i_zed][i_phi] * exp(b_s[i_ch][i_zed][i_phi]*pt) + c_s[i_ch][i_zed][i_phi];

  float sdz  = -9999;
  if(s_dz>0) sdz = (dz - m_dz)/s_dz;      

  return sdz;

}



// Gettofsdz_AB_Run5CuCu
float MatchrecalRecoRun5::Gettofsdz_AB_Run5CuCu(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dz)
{

  if(dz==-9999) return -9999;

  if(i_phi==0) return -9999; // <-- For TOF.

  // Parameters
  float a_m[2][4][2],b_m[2][4][2],c_m[2][4][2];
  float a_s[2][4][2],b_s[2][4][2],c_s[2][4][2];

  if(b_field==1)
    {
      a_m[0][0][1]= -0.503773;  b_m[0][0][1]= -5.173459;  c_m[0][0][1]= -0.007388;
      a_m[0][1][1]= -0.316430;  b_m[0][1][1]= -4.563392;  c_m[0][1][1]= -0.004862;
      a_m[0][2][1]= -0.209307;  b_m[0][2][1]= -5.196280;  c_m[0][2][1]= -0.002780;
      a_m[0][3][1]= -0.091808;  b_m[0][3][1]= -2.667146;  c_m[0][3][1]= -0.007071;
      a_m[1][0][1]= 518.991516; b_m[1][0][1]= 0.000017;   c_m[1][0][1]= -519.004456;
      a_m[1][1][1]= -0.164328;  b_m[1][1][1]= -10.186732; c_m[1][1][1]= -0.002616;
      a_m[1][2][1]= -0.328921;  b_m[1][2][1]= -3.498825;  c_m[1][2][1]= -0.004642;
      a_m[1][3][1]= -0.304909;  b_m[1][3][1]= -3.734405;  c_m[1][3][1]= -0.012523;

      a_s[0][0][1]= 0.313695; b_s[0][0][1]= -6.620260; c_s[0][0][1]= 0.896040;
      a_s[0][1][1]= 0.260567; b_s[0][1][1]= -5.801398; c_s[0][1][1]= 0.892861;
      a_s[0][2][1]= 0.265791; b_s[0][2][1]= -5.650951; c_s[0][2][1]= 0.890890;
      a_s[0][3][1]= 0.277367; b_s[0][3][1]= -4.743789; c_s[0][3][1]= 0.891512;
      a_s[1][0][1]= 0.257571; b_s[1][0][1]= -3.964287; c_s[1][0][1]= 0.882688;
      a_s[1][1][1]= 0.243848; b_s[1][1][1]= -4.797996; c_s[1][1][1]= 0.891313;
      a_s[1][2][1]= 0.280227; b_s[1][2][1]= -5.500307; c_s[1][2][1]= 0.891115;
      a_s[1][3][1]= 0.266978; b_s[1][3][1]= -5.265349; c_s[1][3][1]= 0.891300;
    }
  else if(b_field==-1)
    {
      a_m[0][0][1]= 373.154633; b_m[0][0][1]= 0.000011;  c_m[0][0][1]= -373.158905;
      a_m[0][1][1]= -0.20;      b_m[0][1][1]= -13;       c_m[0][1][1]= 0;
      a_m[0][2][1]= -0.309326;  b_m[0][2][1]= -3.496877; c_m[0][2][1]= -0.003303;
      a_m[0][3][1]= -0.276284;  b_m[0][3][1]= -4.208422; c_m[0][3][1]= -0.016433;
      a_m[1][0][1]= -0.493751;  b_m[1][0][1]= -5.363070; c_m[1][0][1]= 0.005687;
      a_m[1][1][1]= -0.325547;  b_m[1][1][1]= -4.486079; c_m[1][1][1]= 0.006925;
      a_m[1][2][1]= -0.213956;  b_m[1][2][1]= -5.236904; c_m[1][2][1]= -0.000192;
      a_m[1][3][1]= -0.106343;  b_m[1][3][1]= -3.223982; c_m[1][3][1]= -0.012373;

      a_s[0][0][1]= 0.240864; b_s[0][0][1]= -5.438857; c_s[0][0][1]= 0.891998;
      a_s[0][1][1]= 0.205346; b_s[0][1][1]= -4.917623; c_s[0][1][1]= 0.890174;
      a_s[0][2][1]= 0.267998; b_s[0][2][1]= -7.222520; c_s[0][2][1]= 0.891670;
      a_s[0][3][1]= 0.256042; b_s[0][3][1]= -6.305027; c_s[0][3][1]= 0.892795;
      a_s[1][0][1]= 0.346272; b_s[1][0][1]= -5.477163; c_s[1][0][1]= 0.894497;
      a_s[1][1][1]= 0.293125; b_s[1][1][1]= -5.003844; c_s[1][1][1]= 0.893471;
      a_s[1][2][1]= 0.288952; b_s[1][2][1]= -4.571383; c_s[1][2][1]= 0.891683;
      a_s[1][3][1]= 0.305923; b_s[1][3][1]= -4.484206; c_s[1][3][1]= 0.891789;
    }
  else
    {
      return -9999;
    }

  float m_dz = 0;
  float s_dz = 1;
  
  m_dz =  a_m[i_ch][i_zed][i_phi] * exp(b_m[i_ch][i_zed][i_phi]*pt) + c_m[i_ch][i_zed][i_phi];
  s_dz =  a_s[i_ch][i_zed][i_phi] * exp(b_s[i_ch][i_zed][i_phi]*pt) + c_s[i_ch][i_zed][i_phi];

  float sdz  = -9999;
  if(s_dz>0) sdz = (dz - m_dz)/s_dz;      

  return sdz;

}


// Getemcsdz_AB_Run5CuCu
float MatchrecalRecoRun5::Getemcsdz_AB_Run5CuCu(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dz)
{

  if(dz==-9999) return -9999;

  // Parameters
  float a_m[2][4][2],b_m[2][4][2],c_m[2][4][2];
  float a_s[2][4][2],b_s[2][4][2],c_s[2][4][2];

  if(b_field==1)
    {
      a_m[0][0][0]= 0.439817;  b_m[0][0][0]= -2.608805; c_m[0][0][0]= -0.025979;
      a_m[0][0][1]= 0.193477;  b_m[0][0][1]= -1.625962; c_m[0][0][1]= -0.037914;
      a_m[0][1][0]= 0.271625;  b_m[0][1][0]= -2.818756; c_m[0][1][0]= 0.004550;
      a_m[0][1][1]= 0.053198;  b_m[0][1][1]= -1.202286; c_m[0][1][1]= -0.010321;
      a_m[0][2][0]= -0.192018; b_m[0][2][0]= -2.487540; c_m[0][2][0]= 0.009203;
      a_m[0][2][1]= -0.378084; b_m[0][2][1]= -3.007156; c_m[0][2][1]= 0.015548;
      a_m[0][3][0]= -0.373005; b_m[0][3][0]= -2.278377; c_m[0][3][0]= 0.041109;
      a_m[0][3][1]= -0.490082; b_m[0][3][1]= -2.584082; c_m[0][3][1]= 0.050832;
      a_m[1][0][0]= 0.351460;  b_m[1][0][0]= -2.330449; c_m[1][0][0]= -0.034486;
      a_m[1][0][1]= 0.445857;  b_m[1][0][1]= -2.493413; c_m[1][0][1]= -0.032001;
      a_m[1][1][0]= 0.221979;  b_m[1][1][0]= -2.574320; c_m[1][1][0]= -0.007990;
      a_m[1][1][1]= 0.172640;  b_m[1][1][1]= -2.092535; c_m[1][1][1]= -0.009697;
      a_m[1][2][0]= -0.164245; b_m[1][2][0]= -2.155461; c_m[1][2][0]= 0.018527;
      a_m[1][2][1]= -0.423024; b_m[1][2][1]= -3.262069; c_m[1][2][1]= 0.013078;
      a_m[1][3][0]= -0.283690; b_m[1][3][0]= -1.943692; c_m[1][3][0]= 0.053095;
      a_m[1][3][1]= -0.649562; b_m[1][3][1]= -2.974320; c_m[1][3][1]= 0.043682;

      a_s[0][0][0]= 0.952042;   b_s[0][0][0]= -0.001469;  c_s[0][0][0]= -0.007080;
      a_s[0][0][1]= 500.022980; b_s[0][0][1]= 0.000020;   c_s[0][0][1]= -499.098236;
      a_s[0][1][0]= 0.045484;   b_s[0][1][0]= -2.820647;  c_s[0][1][0]= 0.915964;
      a_s[0][1][1]= 241.321152; b_s[0][1][1]= 0.000008;   c_s[0][1][1]= -240.399048;
      a_s[0][2][0]= 164.278992; b_s[0][2][0]= -0.000020;  c_s[0][2][0]= -163.345901;
      a_s[0][2][1]= 0.02;       b_s[0][2][1]= -1;         c_s[0][2][1]= 0.91;
      a_s[0][3][0]= 0.941555;   b_s[0][3][0]= -0.003570;  c_s[0][3][0]= -0.007755;
      a_s[0][3][1]= 0.933075;   b_s[0][3][1]= -0.000936;  c_s[0][3][1]= -0.009744;
      a_s[1][0][0]= 0.195129;   b_s[1][0][0]= -12.816408; c_s[1][0][0]= 0.932450;
      a_s[1][0][1]= 0.955;      b_s[1][0][1]= -0.0048;    c_s[1][0][1]= -0.0050;
      a_s[1][1][0]= 0.145829;   b_s[1][1][0]= -10.585724; c_s[1][1][0]= 0.916482;
      a_s[1][1][1]= 0.05;       b_s[1][1][1]= -1;         c_s[1][1][1]= 0.90;
      a_s[1][2][0]= 0.172751;   b_s[1][2][0]= -13.230371; c_s[1][2][0]= 0.912630;
      a_s[1][2][1]= 0.164352;   b_s[1][2][1]= -7.415835;  c_s[1][2][1]= 0.900532;
      a_s[1][3][0]= 0.048801;   b_s[1][3][0]= -0.823365;  c_s[1][3][0]= 0.906503;
      a_s[1][3][1]= 0.05;       b_s[1][3][1]= -1;         c_s[1][3][1]= 0.90;
    }
  else if(b_field==-1)
    {
      a_m[0][0][0]= 0.329279;  b_m[0][0][0]= -2.079147; c_m[0][0][0]= -0.060335;
      a_m[0][0][1]= 0.394210;  b_m[0][0][1]= -2.244192; c_m[0][0][1]= -0.054509;
      a_m[0][1][0]= 0.237566;  b_m[0][1][0]= -2.396272; c_m[0][1][0]= -0.036995;
      a_m[0][1][1]= 0.183742;  b_m[0][1][1]= -2.079369; c_m[0][1][1]= -0.032218;
      a_m[0][2][0]= -0.107016; b_m[0][2][0]= -2.297375; c_m[0][2][0]= -0.013325;
      a_m[0][2][1]= -0.366999; b_m[0][2][1]= -3.603467; c_m[0][2][1]= -0.016845;
      a_m[0][3][0]= -0.213917; b_m[0][3][0]= -1.721527; c_m[0][3][0]= 0.026443;
      a_m[0][3][1]= -0.526057; b_m[0][3][1]= -2.923039; c_m[0][3][1]= 0.013057;
      a_m[1][0][0]= 0.530418;  b_m[1][0][0]= -2.779548; c_m[1][0][0]= -0.045097;
      a_m[1][0][1]= 0.261429;  b_m[1][0][1]= -1.841095; c_m[1][0][1]= -0.056841;
      a_m[1][1][0]= 0.317124;  b_m[1][1][0]= -2.761410; c_m[1][1][0]= -0.031620;
      a_m[1][1][1]= 0.100803;  b_m[1][1][1]= -1.565603; c_m[1][1][1]= -0.034184;
      a_m[1][2][0]= -0.186067; b_m[1][2][0]= -2.558677; c_m[1][2][0]= -0.011948;
      a_m[1][2][1]= -0.376963; b_m[1][2][1]= -3.067950; c_m[1][2][1]= -0.005830;
      a_m[1][3][0]= -0.411807; b_m[1][3][0]= -2.412852; c_m[1][3][0]= 0.022048;
      a_m[1][3][1]= -0.534311; b_m[1][3][1]= -2.841938; c_m[1][3][1]= 0.023790;

      a_s[0][0][0]= 294.013641; b_s[0][0][0]= 0.000012;   c_s[0][0][0]= -293.077759;
      a_s[0][0][1]= 0.951601;   b_s[0][0][1]= 0.001728;   c_s[0][0][1]= -0.008163;
      a_s[0][1][0]= 205.274994; b_s[0][1][0]= -0.000011;  c_s[0][1][0]= -204.344818;
      a_s[0][1][1]= 199.488190; b_s[0][1][1]= -0.000025;  c_s[0][1][1]= -198.551147;
      a_s[0][2][0]= 0.932312;   b_s[0][2][0]= -0.003229;  c_s[0][2][0]= -0.009460;
      a_s[0][2][1]= 0.931001;   b_s[0][2][1]= -0.001531;  c_s[0][2][1]= -0.009980;
      a_s[0][3][0]= 0.931069;   b_s[0][3][0]= -0.000609;  c_s[0][3][0]= -0.010134;
      a_s[0][3][1]= 0.933376;   b_s[0][3][1]= 0.001599;   c_s[0][3][1]= -0.010254;
      a_s[1][0][0]= 0.183120;   b_s[1][0][0]= -7.174686;  c_s[1][0][0]= 0.940898;
      a_s[1][0][1]= 0.957709;   b_s[1][0][1]= -0.005712;  c_s[1][0][1]= -0.005154;
      a_s[1][1][0]= 0.128067;   b_s[1][1][0]= -5.409693;  c_s[1][1][0]= 0.915591;
      a_s[1][1][1]= 0.04;       b_s[1][1][1]= -1;         c_s[1][1][1]= 0.9;
      a_s[1][2][0]= 0.123404;   b_s[1][2][0]= -5.930827;  c_s[1][2][0]= 0.910440;
      a_s[1][2][1]= 0.161890;   b_s[1][2][1]= -11.083554; c_s[1][2][1]= 0.901788;
      a_s[1][3][0]= 0.091763;   b_s[1][3][0]= -1.580857;  c_s[1][3][0]= 0.902247;
      a_s[1][3][1]= 0.041989;   b_s[1][3][1]= -1.539519;  c_s[1][3][1]= 0.910793;
    }
  else
    {
      return -9999;
    }

  float m_dz = 0;
  float s_dz = 1;
  
  m_dz =  a_m[i_ch][i_zed][i_phi] * exp(b_m[i_ch][i_zed][i_phi]*pt) + c_m[i_ch][i_zed][i_phi];
  s_dz =  a_s[i_ch][i_zed][i_phi] * exp(b_s[i_ch][i_zed][i_phi]*pt) + c_s[i_ch][i_zed][i_phi];

  float sdz  = -9999;
  if(s_dz>0) sdz = (dz - m_dz)/s_dz;      

  return sdz;

}





// Getpc2sdphi_AB_Run5pp
float MatchrecalRecoRun5::Getpc2sdphi_AB_Run5pp(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dphi)
{

  if(dphi==-9999) return -9999;

  if(i_phi==1) return -9999; // <-- For PC2.

  // Parameters
  float a_m[2][4][2],b_m[2][4][2],c_m[2][4][2];
  float a_s[2][4][2],b_s[2][4][2],c_s[2][4][2];

  if(b_field==-1)
    {

      a_m[0][0][0]= 2.015010;  b_m[0][0][0]= -3.607040; c_m[0][0][0]= 0.115375;
      a_m[0][1][0]= 0.330364;  b_m[0][1][0]= -0.443826; c_m[0][1][0]= -0.024574;
      a_m[0][2][0]= 0.284205;  b_m[0][2][0]= -0.670361; c_m[0][2][0]= 0.034598;
      a_m[0][3][0]= 2.363529;  b_m[0][3][0]= -3.601223; c_m[0][3][0]= 0.095368;
      a_m[1][0][0]= -1.938805; b_m[1][0][0]= -3.724604; c_m[1][0][0]= -0.170571;
      a_m[1][1][0]= -0.240343; b_m[1][1][0]= -0.604245; c_m[1][1][0]= -0.081551;
      a_m[1][2][0]= -0.339016; b_m[1][2][0]= -0.283357; c_m[1][2][0]= 0.031224;
      a_m[1][3][0]= -2.457787; b_m[1][3][0]= -3.894732; c_m[1][3][0]= -0.179033;

      a_s[0][0][0]= 3.068805; b_s[0][0][0]= -9.788651;  c_s[0][0][0]= 0.929038;
      a_s[0][1][0]= 0.364723; b_s[0][1][0]= -6.662183;  c_s[0][1][0]= 0.916284;
      a_s[0][2][0]= 0.393581; b_s[0][2][0]= -5.688225;  c_s[0][2][0]= 0.894924;
      a_s[0][3][0]= 3.706633; b_s[0][3][0]= -10.871546; c_s[0][3][0]= 0.933488;
      a_s[1][0][0]= 3.126586; b_s[1][0][0]= -9.739370;  c_s[1][0][0]= 0.943779;
      a_s[1][1][0]= 0.440620; b_s[1][1][0]= -3.900664;  c_s[1][1][0]= 0.921490;
      a_s[1][2][0]= 0.448719; b_s[1][2][0]= -3.864461;  c_s[1][2][0]= 0.912851;
      a_s[1][3][0]= 3.596593; b_s[1][3][0]= -9.553382;  c_s[1][3][0]= 0.937850;

    }
  else
    {
      return -9999;
    }

  float m_dphi = 0;
  float s_dphi = 1;

  m_dphi = a_m[i_ch][i_zed][i_phi] * exp(b_m[i_ch][i_zed][i_phi]*pt) + c_m[i_ch][i_zed][i_phi] ;
  s_dphi = a_s[i_ch][i_zed][i_phi] * exp(b_s[i_ch][i_zed][i_phi]*pt) + c_s[i_ch][i_zed][i_phi] ;

  float sdphi  = -9999;
  if(s_dphi>0) sdphi = (dphi - m_dphi)/s_dphi;      

  return sdphi;

}



// Getpc3sdphi_AB_Run5pp
float MatchrecalRecoRun5::Getpc3sdphi_AB_Run5pp(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dphi)
{

  if(dphi==-9999) return -9999;

  // Parameters
  float a_m[2][4][2],b_m[2][4][2],c_m[2][4][2];
  float a_s[2][4][2],b_s[2][4][2],c_s[2][4][2];

  if(b_field==-1)
    {

      a_m[0][0][0]= 1.330177;  b_m[0][0][0]= -1.970619; c_m[0][0][0]= 0.126783;
      a_m[0][1][0]= 0.410192;  b_m[0][1][0]= -0.478259; c_m[0][1][0]= 0.042422;
      a_m[0][2][0]= 0.463326;  b_m[0][2][0]= -0.740694; c_m[0][2][0]= 0.071187;
      a_m[0][3][0]= 1.648432;  b_m[0][3][0]= -2.436392; c_m[0][3][0]= 0.171471;
      a_m[1][0][0]= -1.176697; b_m[1][0][0]= -1.983708; c_m[1][0][0]= -0.213493;
      a_m[1][1][0]= -0.391935; b_m[1][1][0]= -0.339557; c_m[1][1][0]= -0.049706;
      a_m[1][2][0]= -0.409504; b_m[1][2][0]= -0.675151; c_m[1][2][0]= -0.120623;
      a_m[1][3][0]= -1.660839; b_m[1][3][0]= -2.255581; c_m[1][3][0]= -0.193412;

      a_m[0][0][1]= 2.031626;  b_m[0][0][1]= -3.469800; c_m[0][0][1]= 0.245895;
      a_m[0][1][1]= 0.353683;  b_m[0][1][1]= -0.687765; c_m[0][1][1]= 0.116202;
      a_m[0][2][1]= 0.381272;  b_m[0][2][1]= -0.723136; c_m[0][2][1]= 0.110846;
      a_m[0][3][1]= 1.712531;  b_m[0][3][1]= -2.405184; c_m[0][3][1]= 0.167015;
      a_m[1][0][1]= -0.987026; b_m[1][0][1]= -1.668200; c_m[1][0][1]= -0.148735;
      a_m[1][1][1]= -0.431728; b_m[1][1][1]= -0.368080; c_m[1][1][1]= -0.002390;
      a_m[1][2][1]= -0.372847; b_m[1][2][1]= -0.989258; c_m[1][2][1]= -0.125468;
      a_m[1][3][1]= -1.735774; b_m[1][3][1]= -2.611722; c_m[1][3][1]= -0.164505;

      a_s[0][0][0]= 0.891157; b_s[0][0][0]= -5.017300; c_s[0][0][0]= 0.925264;
      a_s[0][1][0]= 0.339350; b_s[0][1][0]= -4.593891; c_s[0][1][0]= 0.893605;
      a_s[0][2][0]= 0.324807; b_s[0][2][0]= -4.524579; c_s[0][2][0]= 0.902371;
      a_s[0][3][0]= 1.214348; b_s[0][3][0]= -5.403859; c_s[0][3][0]= 0.926645;
      a_s[1][0][0]= 0.792780; b_s[1][0][0]= -4.007743; c_s[1][0][0]= 0.914195;
      a_s[1][1][0]= 0.389116; b_s[1][1][0]= -4.264251; c_s[1][1][0]= 0.901740;
      a_s[1][2][0]= 0.388507; b_s[1][2][0]= -3.627810; c_s[1][2][0]= 0.894435;
      a_s[1][3][0]= 1.358815; b_s[1][3][0]= -4.989146; c_s[1][3][0]= 0.918771;

      a_s[0][0][1]= 2.624624; b_s[0][0][1]= -7.204114; c_s[0][0][1]= 0.882336;
      a_s[0][1][1]= 0.405867; b_s[0][1][1]= -4.450969; c_s[0][1][1]= 0.888272;
      a_s[0][2][1]= 0.441999; b_s[0][2][1]= -4.271328; c_s[0][2][1]= 0.868631;
      a_s[0][3][1]= 1.392257; b_s[0][3][1]= -4.977888; c_s[0][3][1]= 0.894936;
      a_s[1][0][1]= 0.568751; b_s[1][0][1]= -3.635519; c_s[1][0][1]= 0.903715;
      a_s[1][1][1]= 0.439076; b_s[1][1][1]= -4.833741; c_s[1][1][1]= 0.896116;
      a_s[1][2][1]= 0.437797; b_s[1][2][1]= -4.074483; c_s[1][2][1]= 0.886970;
      a_s[1][3][1]= 1.338334; b_s[1][3][1]= -5.205309; c_s[1][3][1]= 0.901292;

    }
  else
    {
      return -9999;
    }

  float m_dphi = 0;
  float s_dphi = 1;

  m_dphi = a_m[i_ch][i_zed][i_phi] * exp(b_m[i_ch][i_zed][i_phi]*pt) + c_m[i_ch][i_zed][i_phi] ;
  s_dphi = a_s[i_ch][i_zed][i_phi] * exp(b_s[i_ch][i_zed][i_phi]*pt) + c_s[i_ch][i_zed][i_phi] ;

  float sdphi  = -9999;
  if(s_dphi>0) sdphi = (dphi - m_dphi)/s_dphi;      

  return sdphi;

}




// Gettofsdphi_AB_Run5pp
float MatchrecalRecoRun5::Gettofsdphi_AB_Run5pp(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dphi)
{

  if(dphi==-9999) return -9999;

  if(i_phi==0) return -9999; // <-- For TOF.

  // Parameters
  float a_m[2][4][2],b_m[2][4][2],c_m[2][4][2];
  float a_s[2][4][2],b_s[2][4][2],c_s[2][4][2];

  if(b_field==-1)
    {

      a_m[0][0][1]= 0.541913;  b_m[0][0][1]= -2.538273; c_m[0][0][1]= 0.024183;
      a_m[0][1][1]= 0.056338;  b_m[0][1][1]= -2.689526; c_m[0][1][1]= 0.003827;
      a_m[0][2][1]= 0.144386;  b_m[0][2][1]= -1.656618; c_m[0][2][1]= 0.025968;
      a_m[0][3][1]= 0.933905;  b_m[0][3][1]= -2.341007; c_m[0][3][1]= 0.052972;
      a_m[1][0][1]= -2.556893; b_m[1][0][1]= -6.335881; c_m[1][0][1]= 0.011585;
      a_m[1][1][1]= -0.375420; b_m[1][1][1]= -4.302002; c_m[1][1][1]= -0.028733;
      a_m[1][2][1]= -0.430302; b_m[1][2][1]= -5.171904; c_m[1][2][1]= 0.011883;
      a_m[1][3][1]= -3.934052; b_m[1][3][1]= -7.154367; c_m[1][3][1]= 0.128033;

      a_s[0][0][1]= 0.295395; b_s[0][0][1]= -3.099122; c_s[0][0][1]= 0.894827;
      a_s[0][1][1]= 0.335967; b_s[0][1][1]= -5.070638; c_s[0][1][1]= 0.909524;
      a_s[0][2][1]= 0.942889; b_s[0][2][1]= -0.000199; c_s[0][2][1]= -0.007813;
      a_s[0][3][1]= 0.483349; b_s[0][3][1]= -4.474638; c_s[0][3][1]= 0.922890;
      a_s[1][0][1]= 3.835206; b_s[1][0][1]= -9.794409; c_s[1][0][1]= 0.904599;
      a_s[1][1][1]= 0.408210; b_s[1][1][1]= -4.592564; c_s[1][1][1]= 0.894217;
      a_s[1][2][1]= 0.395401; b_s[1][2][1]= -5.588413; c_s[1][2][1]= 0.934679;
      a_s[1][3][1]= 4.183227; b_s[1][3][1]= -8.403849; c_s[1][3][1]= 0.915767;

    }
  else
    {
      return -9999;
    }

  float m_dphi = 0;
  float s_dphi = 1;

  m_dphi = a_m[i_ch][i_zed][i_phi] * exp(b_m[i_ch][i_zed][i_phi]*pt) + c_m[i_ch][i_zed][i_phi] ;
  s_dphi = a_s[i_ch][i_zed][i_phi] * exp(b_s[i_ch][i_zed][i_phi]*pt) + c_s[i_ch][i_zed][i_phi] ;

  float sdphi  = -9999;
  if(s_dphi>0) sdphi = (dphi - m_dphi)/s_dphi;      

  return sdphi;

}




// Getemcsdphi_AB_Run5pp
float MatchrecalRecoRun5::Getemcsdphi_AB_Run5pp(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dphi)
{

  if(dphi==-9999) return -9999;

  // Parameters
  float a_m[2][4][2],b_m[2][4][2],c_m[2][4][2];
  float a_s[2][4][2],b_s[2][4][2],c_s[2][4][2];

  if(b_field==-1)
    {

      a_m[0][0][0]= 1.006725;  b_m[0][0][0]= -4.050394;  c_m[0][0][0]= 0.009623;
      a_m[0][1][0]= -0.175283; b_m[0][1][0]= -1.552222;  c_m[0][1][0]= 0.006900;
      a_m[0][2][0]= 34.354702; b_m[0][2][0]= -58.636757; c_m[0][2][0]= -0.024950;
      a_m[0][3][0]= 1.192001;  b_m[0][3][0]= -3.804001;  c_m[0][3][0]= -0.023867;
      a_m[1][0][0]= -0.909479; b_m[1][0][0]= -4.620378;  c_m[1][0][0]= -0.012580;
      a_m[1][1][0]= 0.141406;  b_m[1][1][0]= -0.684375;  c_m[1][1][0]= -0.045787;
      a_m[1][2][0]= 0.204753;  b_m[1][2][0]= -0.493291;  c_m[1][2][0]= -0.106114;
      a_m[1][3][0]= -1.111672; b_m[1][3][0]= -4.265694;  c_m[1][3][0]= -0.023185;

      a_m[0][0][1]= 0.770370;  b_m[0][0][1]= -3.905779;  c_m[0][0][1]= 0.004824;
      a_m[0][1][1]= -0.248647; b_m[0][1][1]= -0.932810;  c_m[0][1][1]= 0.062352;
      a_m[0][2][1]= -0.113163; b_m[0][2][1]= -1.485127;  c_m[0][2][1]= -0.001196;
      a_m[0][3][1]= 0.961270;  b_m[0][3][1]= -3.806662;  c_m[0][3][1]= 0.003852;
      a_m[1][0][1]= -0.985964; b_m[1][0][1]= -4.036561;  c_m[1][0][1]= 0.003435;
      a_m[1][1][1]= 0.122859;  b_m[1][1][1]= -1.599385;  c_m[1][1][1]= 0.014832;
      a_m[1][2][1]= -2.310034; b_m[1][2][1]= -29.924826; c_m[1][2][1]= 0.013745;
      a_m[1][3][1]= -1.184844; b_m[1][3][1]= -3.615728;  c_m[1][3][1]= 0.023030;

      a_s[0][0][0]= 0.446635; b_s[0][0][0]= -6.038372;  c_s[0][0][0]= 0.946504;
      a_s[0][1][0]= 0.258422; b_s[0][1][0]= -11.399488; c_s[0][1][0]= 0.935605;
      a_s[0][2][0]= 0.234880; b_s[0][2][0]= -8.546148;  c_s[0][2][0]= 0.934890;
      a_s[0][3][0]= 0.592870; b_s[0][3][0]= -5.424229;  c_s[0][3][0]= 0.932679;
      a_s[1][0][0]= 0.534473; b_s[1][0][0]= -5.629936;  c_s[1][0][0]= 0.918933;
      a_s[1][1][0]= 0.324218; b_s[1][1][0]= -6.651138;  c_s[1][1][0]= 0.924555;
      a_s[1][2][0]= 0.277988; b_s[1][2][0]= -3.994603;  c_s[1][2][0]= 0.896713;
      a_s[1][3][0]= 0.768623; b_s[1][3][0]= -5.327719;  c_s[1][3][0]= 0.903357;

      a_s[0][0][1]= 0.463142; b_s[0][0][1]= -5.052478; c_s[0][0][1]= 0.940884;
      a_s[0][1][1]= 0.304682; b_s[0][1][1]= -7.919525; c_s[0][1][1]= 0.935128;
      a_s[0][2][1]= 0.316511; b_s[0][2][1]= -5.305285; c_s[0][2][1]= 0.916539;
      a_s[0][3][1]= 0.522772; b_s[0][3][1]= -5.240161; c_s[0][3][1]= 0.934092;
      a_s[1][0][1]= 0.514028; b_s[1][0][1]= -4.980975; c_s[1][0][1]= 0.927437;
      a_s[1][1][1]= 0.318893; b_s[1][1][1]= -5.386336; c_s[1][1][1]= 0.903361;
      a_s[1][2][1]= 0.433672; b_s[1][2][1]= -5.357428; c_s[1][2][1]= 0.888820;
      a_s[1][3][1]= 0.617379; b_s[1][3][1]= -4.094435; c_s[1][3][1]= 0.882314;

    }
  else
    {
      return -9999;
    }

  float m_dphi = 0;
  float s_dphi = 1;

  m_dphi = a_m[i_ch][i_zed][i_phi] * exp(b_m[i_ch][i_zed][i_phi]*pt) + c_m[i_ch][i_zed][i_phi] ;
  s_dphi = a_s[i_ch][i_zed][i_phi] * exp(b_s[i_ch][i_zed][i_phi]*pt) + c_s[i_ch][i_zed][i_phi] ;

  float sdphi  = -9999;
  if(s_dphi>0) sdphi = (dphi - m_dphi)/s_dphi;      

  return sdphi;

}





// Getpc2sdz_AB_Run5pp
float MatchrecalRecoRun5::Getpc2sdz_AB_Run5pp(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dz)
{

  if(dz==-9999) return -9999;

  if(i_phi==1) return -9999; // <-- For PC2.

  // Parameters
  float a_m[2][4][2],b_m[2][4][2],c_m[2][4][2];
  float a_s[2][4][2],b_s[2][4][2],c_s[2][4][2];

  if(b_field==-1)
    {

      a_m[0][0][0]= 0.582733;  b_m[0][0][0]= -4.053913; c_m[0][0][0]= -0.069580;
      a_m[0][1][0]= 0.334872;  b_m[0][1][0]= -4.157117; c_m[0][1][0]= -0.052412;
      a_m[0][2][0]= -0.112192; b_m[0][2][0]= -8.882053; c_m[0][2][0]= -0.030591;
      a_m[0][3][0]= -0.320847; b_m[0][3][0]= -4.314891; c_m[0][3][0]= -0.006039;
      a_m[1][0][0]= 0.900675;  b_m[1][0][0]= -4.029882; c_m[1][0][0]= -0.061185;
      a_m[1][1][0]= 0.411637;  b_m[1][1][0]= -2.703951; c_m[1][1][0]= -0.088894;
      a_m[1][2][0]= -0.242962; b_m[1][2][0]= -4.237959; c_m[1][2][0]= -0.030734;
      a_m[1][3][0]= -0.651867; b_m[1][3][0]= -4.131330; c_m[1][3][0]= -0.019359;

      a_s[0][0][0]= 0.195637;  b_s[0][0][0]= -4.095285;  c_s[0][0][0]= 0.897857;
      a_s[0][1][0]= 0.132865;  b_s[0][1][0]= -4.362690;  c_s[0][1][0]= 0.890697;
      a_s[0][2][0]= 10.960847; b_s[0][2][0]= -56.249424; c_s[0][2][0]= 0.912010;
      a_s[0][3][0]= 0.219773;  b_s[0][3][0]= -1.228415;  c_s[0][3][0]= 0.821425;
      a_s[1][0][0]= 0.420098;  b_s[1][0][0]= -1.840952;  c_s[1][0][0]= 0.842711;
      a_s[1][1][0]= 0.216881;  b_s[1][1][0]= -2.747353;  c_s[1][1][0]= 0.915359;
      a_s[1][2][0]= 0.251092;  b_s[1][2][0]= -3.171220;  c_s[1][2][0]= 0.915318;
      a_s[1][3][0]= 0.332218;  b_s[1][3][0]= -2.317173;  c_s[1][3][0]= 0.884496;

    }
  else
    {
      return -9999;
    }

  float m_dz = 0;
  float s_dz = 1;
  
  m_dz =  a_m[i_ch][i_zed][i_phi] * exp(b_m[i_ch][i_zed][i_phi]*pt) + c_m[i_ch][i_zed][i_phi];
  s_dz =  a_s[i_ch][i_zed][i_phi] * exp(b_s[i_ch][i_zed][i_phi]*pt) + c_s[i_ch][i_zed][i_phi];

  float sdz  = -9999;
  if(s_dz>0) sdz = (dz - m_dz)/s_dz;      

  return sdz;

}


// Getpc3sdz_AB_Run5pp
float MatchrecalRecoRun5::Getpc3sdz_AB_Run5pp(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dz)
{

  if(dz==-9999) return -9999;

  // Parameters
  float a_m[2][4][2],b_m[2][4][2],c_m[2][4][2];
  float a_s[2][4][2],b_s[2][4][2],c_s[2][4][2];

  if(b_field==-1)
    {

      a_m[0][0][0]= 0.400166;  b_m[0][0][0]= -3.833910;  c_m[0][0][0]= -0.075115;
      a_m[0][1][0]= 0.259622;  b_m[0][1][0]= -4.160172;  c_m[0][1][0]= -0.052628;
      a_m[0][2][0]= -0.253058; b_m[0][2][0]= -55.104946; c_m[0][2][0]= -0.035753;
      a_m[0][3][0]= -0.176988; b_m[0][3][0]= -7.369661;  c_m[0][3][0]= -0.018003;
      a_m[1][0][0]= 0.614755;  b_m[1][0][0]= -3.550229;  c_m[1][0][0]= -0.081942;
      a_m[1][1][0]= 0.329541;  b_m[1][1][0]= -2.935650;  c_m[1][1][0]= -0.077548;
      a_m[1][2][0]= -0.179354; b_m[1][2][0]= -6.317812;  c_m[1][2][0]= -0.041580;
      a_m[1][3][0]= -0.446365; b_m[1][3][0]= -4.721392;  c_m[1][3][0]= -0.028388;

      a_m[0][0][1]= 0.511774;  b_m[0][0][1]= -3.683561; c_m[0][0][1]= -0.042659;
      a_m[0][1][1]= 0.200224;  b_m[0][1][1]= -2.650512; c_m[0][1][1]= -0.080499;
      a_m[0][2][1]= -0.353899; b_m[0][2][1]= -5.319614; c_m[0][2][1]= -0.080167;
      a_m[0][3][1]= -0.660180; b_m[0][3][1]= -4.945984; c_m[0][3][1]= -0.115142;
      a_m[1][0][1]= 0.267512;  b_m[1][0][1]= -3.258627; c_m[1][0][1]= -0.055405;
      a_m[1][1][1]= 0.075705;  b_m[1][1][1]= -2.237268; c_m[1][1][1]= -0.072457;
      a_m[1][2][1]= -0.323865; b_m[1][2][1]= -7.148006; c_m[1][2][1]= -0.100463;
      a_m[1][3][1]= -0.555961; b_m[1][3][1]= -4.860697; c_m[1][3][1]= -0.095678;

      a_s[0][0][0]= 0.183871; b_s[0][0][0]= -2.896709; c_s[0][0][0]= 0.890423;
      a_s[0][1][0]= 0.114629; b_s[0][1][0]= -1.198846; c_s[0][1][0]= 0.862366;
      a_s[0][2][0]= 0.090109; b_s[0][2][0]= -0.669501; c_s[0][2][0]= 0.854718;
      a_s[0][3][0]= 0.217530; b_s[0][3][0]= -1.707888; c_s[0][3][0]= 0.849145;
      a_s[1][0][0]= 0.318905; b_s[1][0][0]= -1.647901; c_s[1][0][0]= 0.855260;
      a_s[1][1][0]= 0.199011; b_s[1][1][0]= -2.955103; c_s[1][1][0]= 0.922545;
      a_s[1][2][0]= 0.231173; b_s[1][2][0]= -2.309620; c_s[1][2][0]= 0.908814;
      a_s[1][3][0]= 0.292317; b_s[1][3][0]= -2.670391; c_s[1][3][0]= 0.905479;
      
      a_s[0][0][1]= 0.174357; b_s[0][0][1]= -2.637530; c_s[0][0][1]= 0.892500;
      a_s[0][1][1]= 0.171587; b_s[0][1][1]= -2.614626; c_s[0][1][1]= 0.876740;
      a_s[0][2][1]= 0.203393; b_s[0][2][1]= -1.806667; c_s[0][2][1]= 0.848961;
      a_s[0][3][1]= 0.222570; b_s[0][3][1]= -2.174730; c_s[0][3][1]= 0.865624;
      a_s[1][0][1]= 0.311313; b_s[1][0][1]= -2.910436; c_s[1][0][1]= 0.897146;
      a_s[1][1][1]= 0.191187; b_s[1][1][1]= -6.460569; c_s[1][1][1]= 0.954940;
      a_s[1][2][1]= 0.177050; b_s[1][2][1]= -6.622426; c_s[1][2][1]= 0.929436;
      a_s[1][3][1]= 0.377349; b_s[1][3][1]= -1.581796; c_s[1][3][1]= 0.831666;

    }
  else
    {
      return -9999;
    }

  float m_dz = 0;
  float s_dz = 1;
  
  m_dz =  a_m[i_ch][i_zed][i_phi] * exp(b_m[i_ch][i_zed][i_phi]*pt) + c_m[i_ch][i_zed][i_phi];
  s_dz =  a_s[i_ch][i_zed][i_phi] * exp(b_s[i_ch][i_zed][i_phi]*pt) + c_s[i_ch][i_zed][i_phi];

  float sdz  = -9999;
  if(s_dz>0) sdz = (dz - m_dz)/s_dz;      

  return sdz;

}



// Gettofsdz_AB_Run5pp
float MatchrecalRecoRun5::Gettofsdz_AB_Run5pp(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dz)
{

  if(dz==-9999) return -9999;

  if(i_phi==0) return -9999; // <-- For TOF.

  // Parameters
  float a_m[2][4][2],b_m[2][4][2],c_m[2][4][2];
  float a_s[2][4][2],b_s[2][4][2],c_s[2][4][2];

  if(b_field==-1)
    {

      a_m[0][0][1]= 0.555719;  b_m[0][0][1]= -3.967567; c_m[0][0][1]= -0.009265;
      a_m[0][1][1]= 0.188769;  b_m[0][1][1]= -3.936033; c_m[0][1][1]= -0.034640;
      a_m[0][2][1]= -0.414191; b_m[0][2][1]= -5.398513; c_m[0][2][1]= -0.101009;
      a_m[0][3][1]= -0.789465; b_m[0][3][1]= -4.995487; c_m[0][3][1]= -0.137594;
      a_m[1][0][1]= 0.300450;  b_m[1][0][1]= -2.286959; c_m[1][0][1]= -0.057457;
      a_m[1][1][1]= -0.070359; b_m[1][1][1]= -5.727507; c_m[1][1][1]= -0.033585;
      a_m[1][2][1]= -0.313372; b_m[1][2][1]= -4.474472; c_m[1][2][1]= -0.087561;
      a_m[1][3][1]= -0.751836; b_m[1][3][1]= -3.767226; c_m[1][3][1]= -0.084258;

      a_s[0][0][1]= 0.960023; b_s[0][0][1]= -0.001996; c_s[0][0][1]= -0.005510;
      a_s[0][1][1]= 0.142639; b_s[0][1][1]= -2.526719; c_s[0][1][1]= 0.890242;
      a_s[0][2][1]= 0.203204; b_s[0][2][1]= -2.434195; c_s[0][2][1]= 0.869078;
      a_s[0][3][1]= 0.194790; b_s[0][3][1]= -1.913296; c_s[0][3][1]= 0.867479;
      a_s[1][0][1]= 0.312228; b_s[1][0][1]= -3.199359; c_s[1][0][1]= 0.904568;
      a_s[1][1][1]= 0.319347; b_s[1][1][1]= -4.666223; c_s[1][1][1]= 0.942264;
      a_s[1][2][1]= 0.361397; b_s[1][2][1]= -2.861397; c_s[1][2][1]= 0.883101;
      a_s[1][3][1]= 0.528620; b_s[1][3][1]= -1.792927; c_s[1][3][1]= 0.822991;

    }
  else
    {
      return -9999;
    }

  float m_dz = 0;
  float s_dz = 1;
  
  m_dz =  a_m[i_ch][i_zed][i_phi] * exp(b_m[i_ch][i_zed][i_phi]*pt) + c_m[i_ch][i_zed][i_phi];
  s_dz =  a_s[i_ch][i_zed][i_phi] * exp(b_s[i_ch][i_zed][i_phi]*pt) + c_s[i_ch][i_zed][i_phi];

  float sdz  = -9999;
  if(s_dz>0) sdz = (dz - m_dz)/s_dz;      

  return sdz;

}


// Getemcsdz_AB_Run5pp
float MatchrecalRecoRun5::Getemcsdz_AB_Run5pp(const short i_ch, const short i_zed, const short i_phi, const float pt, const float dz)
{

  if(dz==-9999) return -9999;

  // Parameters
  float a_m[2][4][2],b_m[2][4][2],c_m[2][4][2];
  float a_s[2][4][2],b_s[2][4][2],c_s[2][4][2];

  if(b_field==-1)
    {

      a_m[0][0][0]= 0.726464;  b_m[0][0][0]= -2.847642; c_m[0][0][0]= -0.102258;
      a_m[0][1][0]= 0.392451;  b_m[0][1][0]= -3.375648; c_m[0][1][0]= -0.052354;
      a_m[0][2][0]= -0.195833; b_m[0][2][0]= -3.817746; c_m[0][2][0]= -0.054840;
      a_m[0][3][0]= -0.494309; b_m[0][3][0]= -2.948926; c_m[0][3][0]= -0.030377;
      a_m[1][0][0]= 0.920964;  b_m[1][0][0]= -3.450575; c_m[1][0][0]= -0.064165;
      a_m[1][1][0]= 0.462776;  b_m[1][1][0]= -3.276515; c_m[1][1][0]= -0.049186;
      a_m[1][2][0]= -0.282538; b_m[1][2][0]= -3.680972; c_m[1][2][0]= -0.054087;
      a_m[1][3][0]= -0.711876; b_m[1][3][0]= -2.870914; c_m[1][3][0]= -0.012707;

      a_m[0][0][1]= 0.816256;  b_m[0][0][1]= -2.928933; c_m[0][0][1]= -0.081260;
      a_m[0][1][1]= 0.368709;  b_m[0][1][1]= -2.970480; c_m[0][1][1]= -0.061620;
      a_m[0][2][1]= -0.455623; b_m[0][2][1]= -3.863694; c_m[0][2][1]= -0.066475;
      a_m[0][3][1]= -0.869674; b_m[0][3][1]= -3.704003; c_m[0][3][1]= -0.073219;
      a_m[1][0][1]= 0.657599;  b_m[1][0][1]= -2.767524; c_m[1][0][1]= -0.086008;
      a_m[1][1][1]= 0.246912;  b_m[1][1][1]= -1.940528; c_m[1][1][1]= -0.085573;
      a_m[1][2][1]= -0.439494; b_m[1][2][1]= -3.893484; c_m[1][2][1]= -0.067806;
      a_m[1][3][1]= -0.839019; b_m[1][3][1]= -3.244616; c_m[1][3][1]= -0.029068;

      a_s[0][0][0]= -0.136775;  b_s[0][0][0]= -3.862427; c_s[0][0][0]= 1.005122;
      a_s[0][1][0]= -0.083801;  b_s[0][1][0]= -2.583298; c_s[0][1][0]= 0.994113;
      a_s[0][2][0]= -0.092684;  b_s[0][2][0]= -1.282715; c_s[0][2][0]= 1.005724;
      a_s[0][3][0]= -0.209988;  b_s[0][3][0]= -4.421926; c_s[0][3][0]= 0.986223;
      a_s[1][0][0]= -0.081770;  b_s[1][0][0]= -0.594089; c_s[1][0][0]= 1.044554;
      a_s[1][1][0]= 0.988112;   b_s[1][1][0]= 0.005549;  c_s[1][1][0]= -0.008226;
      a_s[1][2][0]= -0.054870;  b_s[1][2][0]= -0.523477; c_s[1][2][0]= 1.006570;
      a_s[1][3][0]= 256.941376; b_s[1][3][0]= -0.000014; c_s[1][3][0]= -255.969574;

      a_s[0][0][1]= -0.137687; b_s[0][0][1]= -4.443789; c_s[0][0][1]= 1.001359;
      a_s[0][1][1]= -0.124087; b_s[0][1][1]= -0.204417; c_s[0][1][1]= 1.081473;
      a_s[0][2][1]= -0.087107; b_s[0][2][1]= -0.633048; c_s[0][2][1]= 1.024316;
      a_s[0][3][1]= -0.176150; b_s[0][3][1]= -3.075050; c_s[0][3][1]= 1.009203;
      a_s[1][0][1]= -0.076298; b_s[1][0][1]= -2.933526; c_s[1][0][1]= 1.006586;
      a_s[1][1][1]= -0.085148; b_s[1][1][1]= -1.118380; c_s[1][1][1]= 1.009195;
      a_s[1][2][1]= -0.121889; b_s[1][2][1]= -0.520027; c_s[1][2][1]= 1.033638;
      a_s[1][3][1]= -0.077849; b_s[1][3][1]= -3.476497; c_s[1][3][1]= 0.975466;

    }
  else
    {
      return -9999;
    }

  float m_dz = 0;
  float s_dz = 1;
  
  m_dz =  a_m[i_ch][i_zed][i_phi] * exp(b_m[i_ch][i_zed][i_phi]*pt) + c_m[i_ch][i_zed][i_phi];
  s_dz =  a_s[i_ch][i_zed][i_phi] * exp(b_s[i_ch][i_zed][i_phi]*pt) + c_s[i_ch][i_zed][i_phi];

  float sdz  = -9999;
  if(s_dz>0) sdz = (dz - m_dz)/s_dz;      

  return sdz;

}

