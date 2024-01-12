#include <EmcPidrecalReco.h>

#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>

#include <RunNumberRanges.h>
#include <recoConsts.h>
#include <getClass.h>
#include <PHCompositeNode.h>
#include <Fun4AllServer.h>

#include <TH2.h>

#include <iostream>

using namespace std;

EmcPidrecalReco::EmcPidrecalReco(const string &name): Recalibrator(name)
{
  baseclasses.insert("PHCentralTrack");
}

int
EmcPidrecalReco::isValidRun(const int runno) const
{
  if (runno < (int) BEGIN_OF_RUN3 || runno > (int) BEGIN_OF_RUN5)
    {
      return 0;
    }
  return 1;
}

int
EmcPidrecalReco::Init(PHCompositeNode *topNode)
{
  if (fillhistos)
    {
      Fun4AllServer *se = Fun4AllServer::instance();
      string topnodename = topNode->getName().getString();
      string Histoname = Name();
      Histoname += "_momtof";
      Histoname += topnodename;
      momtof = new TH2F(Histoname.c_str(), "1./momentum vs tof", 100, 15, 40, 100, -10, 10);
      se->registerHisto(momtof);

      Histoname = Name();
      Histoname += "_momtofP";
      Histoname += topnodename;
      momtofP = new TH2F(Histoname.c_str(), "1./momentum vs tof Proton", 100, 15, 40, 100, -10, 10);
      se->registerHisto(momtofP);

      Histoname = Name();
      Histoname += "_momtofK";
      Histoname += topnodename;
      momtofK = new TH2F(Histoname.c_str(), "1./momentum vs tof Kaon", 100, 15, 40, 100, -10, 10);
      se->registerHisto(momtofK);

      Histoname = Name();
      Histoname += "_momtofPi";
      Histoname += topnodename;
      momtofPi = new TH2F(Histoname.c_str(), "1./momentum vs tof Pion", 100, 15, 40, 100, -10, 10);
      se->registerHisto(momtofPi);
    }
  return 0;
}

int
EmcPidrecalReco::process_event(PHCompositeNode *topNode)
{
  PHCentralTrack *d_cnt = findNode::getClass<PHCentralTrack>(topNode, inputnodename.c_str());

  if (d_cnt)
    {
      for (unsigned int i = 0; i < d_cnt->get_npart(); i++)
        {
          PHSnglCentralTrack *sngltrk = d_cnt->get_track(i);
          int sect = sngltrk->get_sect();
          if (sect < 0) // exclude tracks which did not hit the emc at all
            {
              continue;
            }
          int dcarm = sngltrk->get_dcarm();
          float mom = sngltrk->get_mom();
          float m2emc = sngltrk->get_m2emc();
          short charge = sngltrk->get_charge();
          if ((dcarm == 0 && (sect == 2 || sect == 3)) ||
              (dcarm == 1)) // do this only for pbsc
            {
              float nPi = IsPion(m2emc, mom, charge);
              float nK = IsKaon(m2emc, mom, charge);
              float nP = IsProton(m2emc, mom, charge);
              sngltrk->ShutUp();
              sngltrk->set_isPi(nPi);
              sngltrk->set_isK(nK);
              sngltrk->set_isP(nP);
              sngltrk->ShutUp(0);
              if (fillhistos)
                {
                  momtof->Fill(sngltrk->get_temc(), 1. / mom*charge);
                  if (fabs(nPi) < 1)
                    {
                      momtofPi->Fill(sngltrk->get_temc(), 1. / mom*charge);
                    }
                  if (fabs(nP) < 1)
                    {
                      momtofP->Fill(sngltrk->get_temc(), 1. / mom*charge);
                    }
                  if (fabs(nK) < 1)
                    {
                      momtofK->Fill(sngltrk->get_temc(), 1. / mom*charge);
                    }
                }
            }
        }
    }

  return 0;
}

float
EmcPidrecalReco::IsPion(const float m2emc, const float mom, const short charge)
{
  if (mom < 0.2)
    {
      return -9999.9;
    }
  float nPi = 9999.9;
  float m2pi_ana_mean = 9999.9;
  float m2pi_ana_sigma = 0.000001;

  float piplus_centroid_par1 = -9999;
  float piplus_centroid_par2 = -9999;
  float piplus_centroid_par3 = -9999;
  float piplus_centroid_par4 = -9999;

  float pimin_centroid_par1 = -9999;
  float pimin_centroid_par2 = -9999;
  float pimin_centroid_par3 = -9999;
  float pimin_centroid_par4 = -9999;

  float piplus_sig_par1 = -9999;
  float piplus_sig_par2 = -9999;
  float piplus_sig_par3 = -9999;
  float piplus_sig_par4 = -9999;

  float pimin_sig_par1 = -9999;
  float pimin_sig_par2 = -9999;
  float pimin_sig_par3 = -9999;
  float pimin_sig_par4 = -9999;
  if (mom < 1.2)
    {

      piplus_centroid_par1 = 2.29043e-02;
      piplus_centroid_par2 = -3.91075e-02;
      piplus_centroid_par3 = 9.79027e-02;
      piplus_centroid_par4 = -5.97900e-02;

      pimin_centroid_par1 = 1.81909e-02 ;
      pimin_centroid_par2 = -1.21184e-02;
      pimin_centroid_par3 = 5.77247e-02 ;
      pimin_centroid_par4 = -3.82399e-02;

      piplus_sig_par1 = 0.048499;
      piplus_sig_par2 = -0.0401503;
      piplus_sig_par3 = 0.00990828;
      piplus_sig_par4 = 0.030773;

      pimin_sig_par1 = 0.0541103;
      pimin_sig_par2 = -0.0445806;
      pimin_sig_par3 = 0.0108446;
      pimin_sig_par4 = 0.0282518;
    }
  if (mom >= 1.2)
    {

      piplus_centroid_par1 = 7.48437e-02;
      piplus_centroid_par2 = -8.81360e-02;
      piplus_centroid_par3 = 3.58415e-02;
      piplus_centroid_par4 = 0;

      pimin_centroid_par1 = 4.68922e-02 ;
      pimin_centroid_par2 = -3.88911e-02;
      pimin_centroid_par3 = 1.76744e-02 ;
      pimin_centroid_par4 = 0;

      piplus_sig_par1 = 0.048499;
      piplus_sig_par2 = -0.0401503;
      piplus_sig_par3 = 0.00990828;
      piplus_sig_par4 = 0.030773;

      pimin_sig_par1 = 0.0541103;
      pimin_sig_par2 = -0.0445806;
      pimin_sig_par3 = 0.0108446;
      pimin_sig_par4 = 0.0282518;
    }


  if (charge > 0) // pi+
    {
      m2pi_ana_mean = piplus_centroid_par1
	+ piplus_centroid_par2 * mom
	+ piplus_centroid_par3 * mom * mom
	+ piplus_centroid_par4 * mom * mom * mom;

      m2pi_ana_sigma = piplus_sig_par1
	+ piplus_sig_par2 / mom
	+ piplus_sig_par3 / mom / mom
	+ piplus_sig_par4 * mom * mom;
    }

  if (charge < 0) // pi-
    {
      m2pi_ana_mean = pimin_centroid_par1
	+ pimin_centroid_par2 * mom
	+ pimin_centroid_par3 * mom * mom
	+ pimin_centroid_par4 * mom * mom * mom;

      m2pi_ana_sigma = pimin_sig_par1
	+ pimin_sig_par2 / mom
	+ pimin_sig_par3 / mom / mom
	+ pimin_sig_par4 * mom * mom;
    }

  nPi = (m2emc - m2pi_ana_mean) / m2pi_ana_sigma;
  return nPi;
}

float
EmcPidrecalReco::IsKaon(const float m2emc, const float mom, const short charge)
{
  if (mom < 0.3 )
    {
      return -9999.9;
    }

  float nK = 9999.9;
  float m2k_ana_mean = 9999.9;
  float m2k_ana_sigma = 0.000001;

  float kplus_centroid_par1 = -9999;
  float kplus_centroid_par2 = -9999;
  float kplus_centroid_par3 = -9999;
  float kplus_centroid_par4 = -9999;

  float kmin_centroid_par1 = -9999;
  float kmin_centroid_par2 = -9999;
  float kmin_centroid_par3 = -9999;
  float kmin_centroid_par4 = -9999;

  float kplus_sig_par1 = -9999;
  float kplus_sig_par2 = -9999;
  float kplus_sig_par3 = -9999;
  float kplus_sig_par4 = -9999;

  float kmin_sig_par1 = -9999;
  float kmin_sig_par2 = -9999;
  float kmin_sig_par3 = -9999;
  float kmin_sig_par4 = -9999;
  if (mom < 1.2)
    {

      kplus_centroid_par1 = 1.45776e-01;
      kplus_centroid_par2 = 5.37155e-01;
      kplus_centroid_par3 = -9.50063e-01;
      kplus_centroid_par4 = 5.17235e-01;

      kmin_centroid_par1 = 1.76902e-01 ;
      kmin_centroid_par2 = 3.04517e-01;
      kmin_centroid_par3 = -5.37774e-01;
      kmin_centroid_par4 = 3.03167e-01;

      kplus_sig_par1 = 0.0525039;
      kplus_sig_par2 = -0.0278102;
      kplus_sig_par3 = 0.00499849;
      kplus_sig_par4 = 0.00406312;

      kmin_sig_par1 = 0.135903;
      kmin_sig_par2 = -0.0935004;
      kmin_sig_par3 = 0.0188272;
      kmin_sig_par4 = -0.0295423;
    }

  if (mom >= 1.2)
    {

      kplus_centroid_par1 = 0.237803;
      kplus_centroid_par2 = 0;
      kplus_centroid_par3 = 0;
      kplus_centroid_par4 = 0;

      kmin_centroid_par1 = 0.23210;
      kmin_centroid_par2 = 0;
      kmin_centroid_par3 = 0;
      kmin_centroid_par4 = 0;

      kplus_sig_par1 = 0.0525039;
      kplus_sig_par2 = -0.0278102;
      kplus_sig_par3 = 0.00499849;
      kplus_sig_par4 = 0.00406312;

      kmin_sig_par1 = 0.135903;
      kmin_sig_par2 = -0.0935004;
      kmin_sig_par3 = 0.0188272;
      kmin_sig_par4 = -0.0295423;
    }
  if (charge > 0) // K+
    {
      m2k_ana_mean = kplus_centroid_par1
	+ kplus_centroid_par2 * mom
	+ kplus_centroid_par3 * mom * mom
	+ kplus_centroid_par4 * mom * mom * mom;

      m2k_ana_sigma = kplus_sig_par1
	+ kplus_sig_par2 / mom
	+ kplus_sig_par3 / mom / mom
	+ kplus_sig_par4 * mom * mom;

    }

  if (charge < 0) // K-
    {
      m2k_ana_mean = kmin_centroid_par1
	+ kmin_centroid_par2 * mom
	+ kmin_centroid_par3 * mom * mom
	+ kmin_centroid_par4 * mom * mom * mom;

      m2k_ana_sigma = kmin_sig_par1
	+ kmin_sig_par2 / mom
	+ kmin_sig_par3 / mom / mom
	+ kmin_sig_par4 * mom * mom;
    }

  nK = (m2emc - m2k_ana_mean) / m2k_ana_sigma;
  return nK;
}

float
EmcPidrecalReco::IsProton(const float m2emc, const float mom, const short charge)
{
  if (mom < 0.5 )
    {
      return -9999.9;
    }
  float nP = 9999.9;
  float m2p_ana_mean = 9999.9;
  float m2p_ana_sigma = 0.000001;

  float p_centroid_par1 = -9999;
  float p_centroid_par2 = -9999;
  float p_centroid_par3 = -9999;
  float p_centroid_par4 = -9999;

  float pbar_centroid_par1 = -9999;
  float pbar_centroid_par2 = -9999;
  float pbar_centroid_par3 = -9999;
  float pbar_centroid_par4 = -9999;

  float p_sig_par1 = -9999;
  float p_sig_par2 = -9999;
  float p_sig_par3 = -9999;
  float p_sig_par4 = -9999;

  float pbar_sig_par1 = -9999;
  float pbar_sig_par2 = -9999;
  float pbar_sig_par3 = -9999;
  float pbar_sig_par4 = -9999;
  if (mom < 1.2)
    {

      p_centroid_par1 = 8.13481e-01;
      p_centroid_par2 = 1.02371e-01;
      p_centroid_par3 = -1.41322e-01;
      p_centroid_par4 = 5.66948e-02;

      pbar_centroid_par1 = 9.01126e-01;
      pbar_centroid_par2 = -1.63571e-01;
      pbar_centroid_par3 = 1.31146e-02;
      pbar_centroid_par4 = 2.67600e-02;

      p_sig_par1 = 0.206892;
      p_sig_par2 = -0.210955;
      p_sig_par3 = 0.0729115;
      p_sig_par4 = 0.0092632;

      pbar_sig_par1 = 0.18808;
      pbar_sig_par2 = -0.150232;
      pbar_sig_par3 = 0.0414251;
      pbar_sig_par4 = 0.00795054;
    }

  if (mom >= 1.2)
    {

      p_centroid_par1 = 8.68208e-01;
      p_centroid_par2 = -7.33613e-02;
      p_centroid_par3 = 3.65111e-02;
      p_centroid_par4 = 0;

      pbar_centroid_par1 = 9.22650e-01;
      pbar_centroid_par2 = -2.36893e-01;
      pbar_centroid_par3 = 9.19517e-02;
      pbar_centroid_par4 = 0;

      p_sig_par1 = 0.206892;
      p_sig_par2 = -0.210955;
      p_sig_par3 = 0.0729115;
      p_sig_par4 = 0.0092632;

      pbar_sig_par1 = 0.18808;
      pbar_sig_par2 = -0.150232;
      pbar_sig_par3 = 0.0414251;
      pbar_sig_par4 = 0.00795054;
    }
  if (charge > 0) // proton
    {
      m2p_ana_mean = p_centroid_par1
	+ p_centroid_par2 * mom
	+ p_centroid_par3 * mom * mom
	+ p_centroid_par4 * mom * mom * mom;

      m2p_ana_sigma = p_sig_par1
	+ p_sig_par2 / mom
	+ p_sig_par3 / mom / mom
	+ p_sig_par4 * mom * mom;


    }

  if (charge < 0) // pbar
    {
      m2p_ana_mean = pbar_centroid_par1
	+ pbar_centroid_par2 * mom
	+ pbar_centroid_par3 * mom * mom
	+ pbar_centroid_par4 * mom * mom * mom;

      m2p_ana_sigma = pbar_sig_par1
	+ pbar_sig_par2 / mom
	+ pbar_sig_par3 / mom / mom
	+ pbar_sig_par4 * mom * mom;
    }

  nP = (m2emc - m2p_ana_mean) / m2p_ana_sigma;
  return nP;
}
