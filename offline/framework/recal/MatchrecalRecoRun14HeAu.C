#include "MatchrecalRecoRun14HeAu.h"
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

MatchrecalRecoRun14HeAu::MatchrecalRecoRun14HeAu(const char* name): Recalibrator(name)
{
  memset(tofdphimean,0,sizeof(tofdphimean));
  memset(tofdzmean,0,sizeof(tofdzmean));
  memset(tofdphisigma,0,sizeof(tofdphisigma));
  memset(tofdzsigma,0,sizeof(tofdzsigma));
    
  memset(pc3dphimean,0,sizeof(pc3dphimean));
  memset(pc3dzmean,0,sizeof(pc3dzmean));
  memset(pc3dphisigma,0,sizeof(pc3dphisigma));
  memset(pc3dzsigma,0,sizeof(pc3dzsigma));

  memset(pc2_dphi_mean,0,sizeof(pc2_dphi_mean));
  memset(pc2_dz_mean,0,sizeof(pc2_dz_mean));
  memset(pc2_dphi_sigma,0,sizeof(pc2_dphi_sigma));
  memset(pc2_dz_sigma,0,sizeof(pc2_dz_sigma));
    
  baseclasses.insert("PHCentralTrack");
}

int MatchrecalRecoRun14HeAu::Init(PHCompositeNode *topNode)
{

  cout<<"Initialize the Run14 He3Au Matching for pc3 and tof"<<endl;
  cout<<"Initialize the Run14 He3Au Matching for pc2"<<endl;

  pc3dphimean[0][0][0]=0.002328860;pc3dphimean[0][0][1]=-0.000235182;pc3dphimean[0][0][2]=-0.001700290;pc3dphimean[0][0][3]=0.000513218;
  pc3dphisigma[0][0][0]=0.001060730;pc3dphisigma[0][0][1]=-0.000030010;pc3dphisigma[0][0][2]=0.000579318;pc3dphisigma[0][0][3]=0.000467513;
  pc3dzmean[0][0][0]=-0.167157993;pc3dzmean[0][0][1]=-0.024628600;pc3dzmean[0][0][2]=-0.135368004;pc3dzmean[0][0][3]=-0.000148443;
  pc3dzsigma[0][0][0]=0.658861995;pc3dzsigma[0][0][1]=0.094845697;pc3dzsigma[0][0][2]=0.655206978;pc3dzsigma[0][0][3]=0.093536600;
  pc3dphimean[0][1][0]=-0.000691247;pc3dphimean[0][1][1]=0.000288772;pc3dphimean[0][1][2]=0.001933470;pc3dphimean[0][1][3]=-0.000525569;
  pc3dphisigma[0][1][0]=-0.000016701;pc3dphisigma[0][1][1]=0.000148110;pc3dphisigma[0][1][2]=0.001638590;pc3dphisigma[0][1][3]=0.000402110;
  pc3dzmean[0][1][0]=-0.312395006;pc3dzmean[0][1][1]=0.011704100;pc3dzmean[0][1][2]=-0.022637799;pc3dzmean[0][1][3]=-0.014631400;
  pc3dzsigma[0][1][0]=0.350800991;pc3dzsigma[0][1][1]=0.146898001;pc3dzsigma[0][1][2]=0.962917984;pc3dzsigma[0][1][3]=0.057298001;
  pc3dphimean[1][0][0]=-0.000438260;pc3dphimean[1][0][1]=-0.000138282;pc3dphimean[1][0][2]=-0.000950051;pc3dphimean[1][0][3]=0.000408522;
  pc3dphisigma[1][0][0]=0.001248970;pc3dphisigma[1][0][1]=-0.000044434;pc3dphisigma[1][0][2]=0.000746445;pc3dphisigma[1][0][3]=0.000438509;
  pc3dzmean[1][0][0]=1.468999982;pc3dzmean[1][0][1]=-0.054445598;pc3dzmean[1][0][2]=-0.186836004;pc3dzmean[1][0][3]=0.017964499;
  pc3dzsigma[1][0][0]=0.724511981;pc3dzsigma[1][0][1]=0.046695899;pc3dzsigma[1][0][2]=0.670863986;pc3dzsigma[1][0][3]=0.113341004;
  pc3dphimean[1][1][0]=-0.001705810;pc3dphimean[1][1][1]=0.000186327;pc3dphimean[1][1][2]=0.000975175;pc3dphimean[1][1][3]=-0.000415513;
  pc3dphisigma[1][1][0]=0.000217781;pc3dphisigma[1][1][1]=0.000151847;pc3dphisigma[1][1][2]=0.001606270;pc3dphisigma[1][1][3]=0.000432108;
  pc3dzmean[1][1][0]=1.383820057;pc3dzmean[1][1][1]=-0.048694700;pc3dzmean[1][1][2]=-0.075243197;pc3dzmean[1][1][3]=0.013744300;
  pc3dzsigma[1][1][0]=0.137511000;pc3dzsigma[1][1][1]=0.123079002;pc3dzsigma[1][1][2]=1.336089969;pc3dzsigma[1][1][3]=0.028577700;
  
  //tof
  tofdphimean[0][0][0]=0.000873495;tofdphimean[0][0][1]=0.000028189;tofdphimean[0][0][2]=-0.000642758;tofdphimean[0][0][3]=0.000087570;
  tofdphisigma[0][0][0]=0.006369170;tofdphisigma[0][0][1]=-0.000113738;tofdphisigma[0][0][2]=-0.001069460;tofdphisigma[0][0][3]=0.000477118;
  tofdzmean[0][0][0]=0.274280995;tofdzmean[0][0][1]=-0.088327102;tofdzmean[0][0][2]=-0.405842006;tofdzmean[0][0][3]=0.023276500;
  tofdzsigma[0][0][0]=0.874916017;tofdzsigma[0][0][1]=0.045682300;tofdzsigma[0][0][2]=0.691609979;tofdzsigma[0][0][3]=0.178742006;
  tofdphimean[0][1][0]=0.003532650;tofdphimean[0][1][1]=-0.000435130;tofdphimean[0][1][2]=-0.000971710;tofdphimean[0][1][3]=0.000152476;
  tofdphisigma[0][1][0]=0.007464860;tofdphisigma[0][1][1]=-0.000247743;tofdphisigma[0][1][2]=-0.002121730;tofdphisigma[0][1][3]=0.000579410;
  tofdzmean[0][1][0]=0.318536013;tofdzmean[0][1][1]=-0.091900200;tofdzmean[0][1][2]=-0.455772012;tofdzmean[0][1][3]=0.011119800;
  tofdzsigma[0][1][0]=0.109380998;tofdzsigma[0][1][1]=0.181988999;tofdzsigma[0][1][2]=1.406100035;tofdzsigma[0][1][3]=0.130567998;
  tofdphimean[1][0][0]=0.002414960;tofdphimean[1][0][1]=-0.000496356;tofdphimean[1][0][2]=-0.003121630;tofdphimean[1][0][3]=0.000787865;
  tofdphisigma[1][0][0]=0.004772550;tofdphisigma[1][0][1]=-0.000349505;tofdphisigma[1][0][2]=-0.001870280;tofdphisigma[1][0][3]=0.000618176;
  tofdzmean[1][0][0]=2.868129969;tofdzmean[1][0][1]=-0.158044994;tofdzmean[1][0][2]=-1.176990032;tofdzmean[1][0][3]=0.076545902;
  tofdzsigma[1][0][0]=1.367249966;tofdzsigma[1][0][1]=0.039048702;tofdzsigma[1][0][2]=1.149129987;tofdzsigma[1][0][3]=-0.077593498;
  tofdphimean[1][1][0]=-0.003278430;tofdphimean[1][1][1]=0.000568171;tofdphimean[1][1][2]=0.002947460;tofdphimean[1][1][3]=-0.000633824;
  tofdphisigma[1][1][0]=0.001416470;tofdphisigma[1][1][1]=0.000131365;tofdphisigma[1][1][2]=0.001364210;tofdphisigma[1][1][3]=0.000279878;
  tofdzmean[1][1][0]=2.632270098;tofdzmean[1][1][1]=-0.101276003;tofdzmean[1][1][2]=-0.983641028;tofdzmean[1][1][3]=0.059218802;
  tofdzsigma[1][1][0]=2.116149902;tofdzsigma[1][1][1]=-0.029661300;tofdzsigma[1][1][2]=0.112187997;tofdzsigma[1][1][3]=0.157830998;
  
  //pc2
  pc2_dphifirst_mean[0][0][0] = -0.099;
  pc2_dphifirst_sigma[0][0][0] = 0;
  pc2_dzfirst_mean[0][0][0] = -9.8;
  pc2_dzfirst_sigma[0][0][0] = 0;
  pc2_dphifirst_mean[0][1][0] = -0.099;
  pc2_dphifirst_sigma[0][1][0] = 0;
  pc2_dzfirst_mean[0][1][0] = -9.8;
  pc2_dzfirst_sigma[0][1][0] = 0;
  pc2_dphifirst_mean[1][0][0] = -0.00510224;
  pc2_dphifirst_sigma[1][0][0] = 0.00699183;
  pc2_dzfirst_mean[1][0][0] = 0.763148;
  pc2_dzfirst_sigma[1][0][0] = 2.93855;
  pc2_dphifirst_mean[1][1][0] = 0.00205922;
  pc2_dphifirst_sigma[1][1][0] = 0.00665996;
  pc2_dzfirst_mean[1][1][0] = 0.648651;
  pc2_dzfirst_sigma[1][1][0] = 3.09573;

  pc2_dphi_sigma[0][0][0] = 0;
  pc2_dphi_sigma[0][0][1] = 0;
  pc2_dphi_sigma[0][0][2] = 0;
  pc2_dphi_sigma[0][0][3] = 0;
  pc2_dphi_sigma[0][0][4] = 0;
  pc2_dphi_sigma[0][0][5] = 0;
  pc2_dphi_sigma[0][0][6] = 0;
  pc2_dphi_sigma[0][0][7] = 0;
  pc2_dphi_mean[0][0][0] = -0.0779735;
  pc2_dphi_mean[0][0][1] = -0.00136205;
  pc2_dphi_mean[0][0][2] = 0.0216856;
  pc2_dphi_mean[0][0][3] = -0.0426029;
  pc2_dphi_mean[0][0][4] = 0.0072229;
  pc2_dphi_mean[0][0][5] = -0.00768278;
  pc2_dphi_mean[0][0][6] = 0.00179557;
  pc2_dz_sigma[0][0][0] = 0;
  pc2_dz_sigma[0][0][1] = 0;
  pc2_dz_sigma[0][0][2] = 0;
  pc2_dz_sigma[0][0][3] = 0;
  pc2_dz_sigma[0][0][4] = 0;
  pc2_dz_sigma[0][0][5] = 0;
  pc2_dz_sigma[0][0][6] = 0;
  pc2_dz_sigma[0][0][7] = 0;
  pc2_dz_mean[0][0][0] = -9.0597;
  pc2_dz_mean[0][0][1] = -0.0288012;
  pc2_dz_mean[0][0][2] = 2.21001;
  pc2_dz_mean[0][0][3] = -2.24447;
  pc2_dz_mean[0][0][4] = -0.956304;
  pc2_dz_mean[0][0][5] = 0.327325;
  pc2_dz_mean[0][0][6] = -0.047667;
  pc2_dphi_sigma[0][1][0] = 0;
  pc2_dphi_sigma[0][1][1] = 0;
  pc2_dphi_sigma[0][1][2] = 0;
  pc2_dphi_sigma[0][1][3] = 0;
  pc2_dphi_sigma[0][1][4] = 0;
  pc2_dphi_sigma[0][1][5] = 0;
  pc2_dphi_sigma[0][1][6] = 0;
  pc2_dphi_sigma[0][1][7] = 0;
  pc2_dphi_mean[0][1][0] = -0.0779735;
  pc2_dphi_mean[0][1][1] = -0.00136205;
  pc2_dphi_mean[0][1][2] = 0.0216856;
  pc2_dphi_mean[0][1][3] = -0.0426029;
  pc2_dphi_mean[0][1][4] = 0.0072229;
  pc2_dphi_mean[0][1][5] = -0.00768278;
  pc2_dphi_mean[0][1][6] = 0.00179557;
  pc2_dz_sigma[0][1][0] = 0;
  pc2_dz_sigma[0][1][1] = 0;
  pc2_dz_sigma[0][1][2] = 0;
  pc2_dz_sigma[0][1][3] = 0;
  pc2_dz_sigma[0][1][4] = 0;
  pc2_dz_sigma[0][1][5] = 0;
  pc2_dz_sigma[0][1][6] = 0;
  pc2_dz_sigma[0][1][7] = 0;
  pc2_dz_mean[0][1][0] = -9.0597;
  pc2_dz_mean[0][1][1] = -0.0288012;
  pc2_dz_mean[0][1][2] = 2.21001;
  pc2_dz_mean[0][1][3] = -2.24447;
  pc2_dz_mean[0][1][4] = -0.956304;
  pc2_dz_mean[0][1][5] = 0.327325;
  pc2_dz_mean[0][1][6] = -0.047667;
  pc2_dphi_sigma[1][0][0] = 0.00672718;
  pc2_dphi_sigma[1][0][1] = -0.00248607;
  pc2_dphi_sigma[1][0][2] = 0.000812001;
  pc2_dphi_sigma[1][0][3] = -0.000145074;
  pc2_dphi_sigma[1][0][4] = 1.40743e-05;
  pc2_dphi_sigma[1][0][5] = -4.12691e-07;
  pc2_dphi_sigma[1][0][6] = -0.00357624;
  pc2_dphi_sigma[1][0][7] = 0.000744732;
  pc2_dphi_mean[1][0][0] = -0.00437925;
  pc2_dphi_mean[1][0][1] = 0.000339777;
  pc2_dphi_mean[1][0][2] = -0.00272497;
  pc2_dphi_mean[1][0][3] = 0.00585416;
  pc2_dphi_mean[1][0][4] = -0.000992101;
  pc2_dphi_mean[1][0][5] = 0.000878826;
  pc2_dphi_mean[1][0][6] = -0.000240304;
  pc2_dz_sigma[1][0][0] = -2.92601;
  pc2_dz_sigma[1][0][1] = 3.32708;
  pc2_dz_sigma[1][0][2] = -1.99042;
  pc2_dz_sigma[1][0][3] = 0.647709;
  pc2_dz_sigma[1][0][4] = -0.105817;
  pc2_dz_sigma[1][0][5] = 0.00678451;
  pc2_dz_sigma[1][0][6] = 2.14756;
  pc2_dz_sigma[1][0][7] = 0.000832243;
  pc2_dz_mean[1][0][0] = -3.07756;
  pc2_dz_mean[1][0][1] = 0.217217;
  pc2_dz_mean[1][0][2] = -7.85528;
  pc2_dz_mean[1][0][3] = 9.89831;
  pc2_dz_mean[1][0][4] = 2.09621;
  pc2_dz_mean[1][0][5] = -0.386183;
  pc2_dz_mean[1][0][6] = 0.0208002;
  pc2_dphi_sigma[1][1][0] = 0.036986;
  pc2_dphi_sigma[1][1][1] = -0.0226766;
  pc2_dphi_sigma[1][1][2] = 0.0105554;
  pc2_dphi_sigma[1][1][3] = -0.0028497;
  pc2_dphi_sigma[1][1][4] = 0.000401921;
  pc2_dphi_sigma[1][1][5] = -2.24815e-05;
  pc2_dphi_sigma[1][1][6] = -0.0219712;
  pc2_dphi_sigma[1][1][7] = 0.00161907;
  pc2_dphi_mean[1][1][0] = -0.0384202;
  pc2_dphi_mean[1][1][1] = 0.00167986;
  pc2_dphi_mean[1][1][2] = -0.0945678;
  pc2_dphi_mean[1][1][3] = 0.103047;
  pc2_dphi_mean[1][1][4] = 0.0365639;
  pc2_dphi_mean[1][1][5] = -0.0114934;
  pc2_dphi_mean[1][1][6] = 0.00162328;
  pc2_dz_sigma[1][1][0] = -2.9303;
  pc2_dz_sigma[1][1][1] = 2.77088;
  pc2_dz_sigma[1][1][2] = -1.46498;
  pc2_dz_sigma[1][1][3] = 0.440912;
  pc2_dz_sigma[1][1][4] = -0.0683771;
  pc2_dz_sigma[1][1][5] = 0.00421116;
  pc2_dz_sigma[1][1][6] = 2.37825;
  pc2_dz_sigma[1][1][7] = -0.039446;
  pc2_dz_mean[1][1][0] = -4.27312;
  pc2_dz_mean[1][1][1] = 0.261847;
  pc2_dz_mean[1][1][2] = -11.8408;
  pc2_dz_mean[1][1][3] = 13.7302;
  pc2_dz_mean[1][1][4] = 4.03417;
  pc2_dz_mean[1][1][5] = -1.12943;
  pc2_dz_mean[1][1][6] = 0.139963;

  return 0;
}


int MatchrecalRecoRun14HeAu::InitRun(PHCompositeNode *topNode)
{
  return 0;
}

int MatchrecalRecoRun14HeAu::isValidRun(const int runno) const
{

  // Run14HeAu 200 GeV
  if (415751 <= runno && runno <= 416892)
    {
      return 1;
    }

  return 0;
}

int MatchrecalRecoRun14HeAu::process_event(PHCompositeNode *topNode)
{
  d_cnt    = findNode::getClass<PHCentralTrack>(topNode, inputnodename.c_str());
  //d_global = findNode::getClass<PHGlobal>(topNode, "PHGlobal");

  if (!d_cnt ) return 0;

  for (unsigned int itrk = 0; itrk < d_cnt->get_npart(); itrk++)
    {

      PHSnglCentralTrack *sngltrk = d_cnt->get_track(itrk);
      sngltrk->ShutUp();
      
      if (
	  sngltrk->isImplemented(sngltrk->get_dcarm()) &&
          sngltrk->isImplemented(sngltrk->get_the0()) &&
          sngltrk->isImplemented(sngltrk->get_mom()) &&
          sngltrk->isImplemented(sngltrk->get_charge()) &&
          //sngltrk->isImplemented(sngltrk->get_zed()) &&
          //sngltrk->isImplemented(sngltrk->get_phi()) &&
          //sngltrk->isImplemented(sngltrk->get_pc2dphi()) &&
          //sngltrk->isImplemented(sngltrk->get_pc2dz()) &&
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
      
      short dcarm = sngltrk->get_dcarm();
      float the0 = sngltrk->get_the0();
      float mom  = sngltrk->get_mom();
      float dummypt = -9999;
      float pt = -9999;

      if (the0 > -999)
        {
          pt = mom * sin(the0);
        }
      else pt = mom;
      dummypt = pt;
      
      if(dummypt > 5.5)
  {
    dummypt = 5.5;//To fix problem with extremelely narrow pc3s distributions at high pT
  }
      short charge = 0.5*(sngltrk->get_charge()+1);
      if(charge<0||charge>1) continue;

      float pc3dphi = sngltrk->get_pc3dphi();
      float pc3dz   = sngltrk->get_pc3dz();

      float pc2dphi = sngltrk->get_pc2dphi();
      float pc2dz   = sngltrk->get_pc2dz();
      
      //float tofdphi = sngltrk->get_tofdphi();
      //float tofdz   = sngltrk->get_tofdz();

      //float emcdphi = sngltrk->get_emcdphi();
      //float emcdz   = sngltrk->get_emcdz();
     
      float pc2sdphi = Getpc2sdphi(dcarm, charge, dummypt, pc2dphi);
      float pc3sdphi = Getpc3sdphi(dcarm, charge, pt, pc3dphi);
      //float tofsdphi = Gettofsdphi(dcarm, charge, pt, tofdphi);
      //float emcsdphi = Getemcsdphi(i_ch, i_zed, i_phi, pt, emcdphi);

      float pc2sdz = Getpc2sdz(dcarm, charge, dummypt, pc2dz);
      float pc3sdz = Getpc3sdz(dcarm, charge, pt, pc3dz);
      //float tofsdz = Gettofsdz(dcarm, charge, pt, tofdz);
      //float emcsdz = Getemcsdz(i_bbcsum, i_ch, i_zed, i_phi, pt, emcdz);

      sngltrk->set_pc2sdphi(pc2sdphi);
      sngltrk->set_pc3sdphi(pc3sdphi);
      //sngltrk->set_tofsdphi(tofsdphi);
      //sngltrk->set_emcsdphi(emcsdphi);
      sngltrk->set_pc2sdz(pc2sdz);
      sngltrk->set_pc3sdz(pc3sdz);
      //sngltrk->set_tofsdz(tofsdz);
      //sngltrk->set_emcsdz(emcsdz);

      float tofdphi = -9999;
      float tofdz   = -9999;      
      
      if(dcarm==0 &&  sngltrk->get_pltof()>400 &&  sngltrk->get_slat()>=0){
	tofdphi = sngltrk->get_tofdphi();
	tofdz   = sngltrk->get_tofdz();
      }

      if(dcarm==1 &&  sngltrk->get_pltofw()>400 &&  sngltrk->get_striptofw()>=0){
	tofdphi = sngltrk->get_tofwdphi();
	tofdz   = sngltrk->get_tofwdz();
      }

      if(tofdphi>-100 && tofdz>-100){
	float tofsdphi = Gettofsdphi(dcarm, charge, pt, tofdphi);
	float tofsdz = Gettofsdz(dcarm, charge, pt, tofdz);
	if(dcarm==0){
	  sngltrk->set_tofsdphi(tofsdphi);
	  sngltrk->set_tofsdz(tofsdz);
	}
	if(dcarm==1){
	  sngltrk->set_tofwsdphi(tofsdphi);
	  sngltrk->set_tofwsdz(tofsdz);
	}
      }

      // ------------------

     
    }

  return EVENT_OK;
}

// Getpc3sdphi
float MatchrecalRecoRun14HeAu::Getpc3sdphi(const short dcarm, const short charge, const float pt, const float dphi)
{

  if(dphi==-9999) return -9999;

  if(pt<0.2) return -9999;

  float ptcal = pt;
  if(pt<=0.4) ptcal=0.4;
  if(pt>=4.0) ptcal=4.0;

  float mean = pc3dphimean[dcarm][charge][0]+
    pc3dphimean[dcarm][charge][1]*ptcal+
    pc3dphimean[dcarm][charge][2]/sqrt(ptcal)+
    pc3dphimean[dcarm][charge][3]/ptcal/ptcal;

  float sigma = pc3dphisigma[dcarm][charge][0]+
    pc3dphisigma[dcarm][charge][1]*ptcal+
    pc3dphisigma[dcarm][charge][2]/sqrt(ptcal)+
    pc3dphisigma[dcarm][charge][3]/ptcal/ptcal;

  return (dphi-mean)/sigma;
}

float MatchrecalRecoRun14HeAu::Getpc3sdz(const short dcarm, const short charge, const float pt, const float dz)
{

  if(dz==-9999) return -9999;

  if(pt<0.2) return -9999;

  float ptcal = pt;
  if(pt<=0.4) ptcal=0.4;
  if(pt>=4.0) ptcal=4.0;

  float mean = pc3dzmean[dcarm][charge][0]+
    pc3dzmean[dcarm][charge][1]*ptcal+
    pc3dzmean[dcarm][charge][2]/sqrt(ptcal)+
    pc3dzmean[dcarm][charge][3]/ptcal/ptcal;

  float sigma = pc3dzsigma[dcarm][charge][0]+
    pc3dzsigma[dcarm][charge][1]*ptcal+
    pc3dzsigma[dcarm][charge][2]/sqrt(ptcal)+
    pc3dzsigma[dcarm][charge][3]/ptcal/ptcal;

  return (dz-mean)/sigma;
}


// Gettofsdphi
float MatchrecalRecoRun14HeAu::Gettofsdphi(const short dcarm, const short charge, const float pt, const float dphi)
{

  if(dphi==-9999) return -9999;

  if(pt<0.2) return -9999;

  float ptcal = pt;
  if(pt<=0.4) ptcal=0.4;
  if(pt>=4.0) ptcal=4.0;

  float mean = tofdphimean[dcarm][charge][0]+
    tofdphimean[dcarm][charge][1]*ptcal+
    tofdphimean[dcarm][charge][2]/sqrt(ptcal)+
    tofdphimean[dcarm][charge][3]/ptcal/ptcal;

  float sigma = tofdphisigma[dcarm][charge][0]+
    tofdphisigma[dcarm][charge][1]*ptcal+
    tofdphisigma[dcarm][charge][2]/sqrt(ptcal)+
    tofdphisigma[dcarm][charge][3]/ptcal/ptcal;

  return (dphi-mean)/sigma;
}

float MatchrecalRecoRun14HeAu::Gettofsdz(const short dcarm, const short charge, const float pt, const float dz)
{

  if(dz==-9999) return -9999;

  if(pt<0.2) return -9999;

  float ptcal = pt;
  if(pt<=0.4) ptcal=0.4;
  if(pt>=4.0) ptcal=4.0;

  float mean = tofdzmean[dcarm][charge][0]+
    tofdzmean[dcarm][charge][1]*ptcal+
    tofdzmean[dcarm][charge][2]/sqrt(ptcal)+
    tofdzmean[dcarm][charge][3]/ptcal/ptcal;

  float sigma = tofdzsigma[dcarm][charge][0]+
    tofdzsigma[dcarm][charge][1]*ptcal+
    tofdzsigma[dcarm][charge][2]/sqrt(ptcal)+
    tofdzsigma[dcarm][charge][3]/ptcal/ptcal;

  return (dz-mean)/sigma;
}

float MatchrecalRecoRun14HeAu::Getpc2sdphi(const int iarm, const int icharge, const float pt, const float pc2dphi)
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
    mean = pc2_dphifirst_mean[iarm][jcharge][0];
    sigma = pc2_dphifirst_sigma[iarm][jcharge][0];
  }
  else{
    mean = pc2_dphi_mean[iarm][jcharge][0]+pc2_dphi_mean[iarm][jcharge][1]*pt+pc2_dphi_mean[iarm][jcharge][2]/pt+pc2_dphi_mean[iarm][jcharge][3]/sqrt(pt)+pc2_dphi_mean[iarm][jcharge][4]/pt/pt+pc2_dphi_mean[iarm][jcharge][5]/pt/pt/pt+pc2_dphi_mean[iarm][jcharge][6]/pt/pt/pt/pt;
    sigma = pc2_dphi_sigma[iarm][jcharge][0]+pc2_dphi_sigma[iarm][jcharge][1]*pt+pc2_dphi_sigma[iarm][jcharge][2]*pt*pt+pc2_dphi_sigma[iarm][jcharge][3]*pt*pt*pt+pc2_dphi_sigma[iarm][jcharge][4]*pt*pt*pt*pt+pc2_dphi_sigma[iarm][jcharge][5]*pt*pt*pt*pt*pt+pc2_dphi_sigma[iarm][jcharge][6]/sqrt(pt)+pc2_dphi_sigma[iarm][jcharge][7]/pt/pt;
  }
  if (Verbosity()){
    cout <<  "Getpc2sdphi(" << iarm << ", " << icharge << ", "  << pt << ", " << pc2dphi << ") " <<
      "\nmean= " << mean << 
      endl;
    cout <<  "Getpc2sdphi(" << iarm << ", " << icharge << ", "  << pt << ", " << pc2dphi << ") " <<
      "\nsigma= " << sigma << 
      endl;
  }
  if(sigma == 0)
      return -999;
  else
    return (pc2dphi - mean) / sigma;
}

float MatchrecalRecoRun14HeAu::Getpc2sdz(const int iarm, const int icharge, const float pt, const float pc2dz)
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
      mean = pc2_dzfirst_mean[iarm][jcharge][0];
      sigma = pc2_dzfirst_sigma[iarm][jcharge][0];
  }
  else{
  if(pt > 3){  float pts = 3.;
  mean = pc2_dz_mean[iarm][jcharge][0]+pc2_dz_mean[iarm][jcharge][1]*pts+pc2_dz_mean[iarm][jcharge][2]/pts+pc2_dz_mean[iarm][jcharge][3]/sqrt(pts)+pc2_dz_mean[iarm][jcharge][4]/pts/pts+pc2_dz_mean[iarm][jcharge][5]/pts/pts/pts+pc2_dz_mean[iarm][jcharge][6]/pts/pts/pts/pts;
  sigma = pc2_dz_sigma[iarm][jcharge][0]+pc2_dz_sigma[iarm][jcharge][1]*pts+pc2_dz_sigma[iarm][jcharge][2]*pts*pts+pc2_dz_sigma[iarm][jcharge][3]*pts*pts*pts+pc2_dz_sigma[iarm][jcharge][4]*pts*pts*pts*pts+pc2_dz_sigma[iarm][jcharge][5]*pts*pts*pts*pts*pts+pc2_dz_sigma[iarm][jcharge][6]/sqrt(pts)+pc2_dz_sigma[iarm][jcharge][7]/pts/pts;
  }
  }
  if (Verbosity()){
    cout <<  "Getpc2sdz(" << iarm << ", " << icharge << ", " << pt << ", " << pc2dz << ") " <<
      "\nmean= " << mean << 
      endl;
    cout <<  "Getpc2sdz(" << iarm << ", " << icharge << ", " << pt << ", " << pc2dz << ") " <<
      "\nsigma= " << sigma << 
      endl;
  }
  if(sigma == 0)
      return -999;
  else
    return (pc2dz - mean) / sigma;
}
