#include <sstream>
#include <string>

#include "PhysicsqaReco.h"
#include "TH1.h"
#include "TH2.h"
#include "TLorentzVector.h"

#include "PHCompositeNode.h"
#include "PHTypedNodeIterator.h"
#include "PHIODataNode.h"
#include "TriggerHelper.h"
#include "Fun4AllServer.h"
#include "getClass.h"

#include "PHCentralTrack.h"
#include "PHGlobal.h"

#include "emcClusterContainer.h"
#include "emcClusterContent.h"
#include "emcTowerContainer.h"
#include "emcTowerContent.h"
#include "EmcIndexer.h"

#include "PdbBankManager.hh"
#include "PdbApplication.hh"
#include "PdbBankList.hh"
#include "PdbCalBank.hh"
#include "PdbParameter.hh"
#include "PdbParameterError.hh"

#include <frog/FROG.h>
#include <TFile.h>
#include <TIterator.h>

#include "QaDatabaseManager.h"
#include "QaEntry.h"

#include "recoConsts.h"

using namespace std;

typedef PHIODataNode<PHCentralTrack> PHParticleNode_t;
typedef PHIODataNode<PHGlobal> PHGlobalNode_t;

PhysicsqaReco::PhysicsqaReco(int c)
{
  commit = c;
  Tag = "Production";

  ThisName = "PhysQA";
  numphotons = 1;
  nevt = 0;
  runNumber = 0;
  QA_ETOFCUT = 0.0014;
  QA_TOFFACTOR = 29.9792458;

}

int PhysicsqaReco::Init(PHCompositeNode *topNode)
{

  Fun4AllServer *server = Fun4AllServer::instance();

  cout << "PhysicsqaReco is being initialized" << endl;

  server->registerHisto("PQA_mean_transverse_momentum" , meanpt = new TH1F("PQA_mean_transverse_momentum" , "PQA_mean_transverse_momentum" , 100, 0.0, 5.0 ));
  server->registerHisto("PQA_tof_mass2_distribution" , tofmass2 = new TH1F("PQA_tof_mass2_distribution" , "PQA_tof_mass2_distribution" , 100, 0.0, 4.0 ));
  server->registerHisto("PQA_tof_mass2_distribution_pos", tofmass2pos = new TH1F("PQA_tof_mass2_distribution_pos", "PQA_tof_mass2_distribution_pos", 180, 0.6, 1.2 ));
  server->registerHisto("PQA_tof_mass2_distribution_neg", tofmass2neg = new TH1F("PQA_tof_mass2_distribution_neg", "PQA_tof_mass2_distribution_neg", 180, 0.6, 1.2 ));
  server->registerHisto("PQA_emcGl_mass2_distribution" , emcGlmass2 = new TH1F("PQA_emcGl_mass2_distribution" , "PQA_emcGl_mass2_distribution" , 180, 0.0, 4.0 ));
  server->registerHisto("PQA_emcSc_mass2_distribution" , emcScmass2 = new TH1F("PQA_emcSc_mass2_distribution" , "PQA_emcSc_mass2_distribution" , 180, 0.0, 4.0 ));
  server->registerHisto("PQA_NumberOfTracks" , numtracks = new TH1F("PQA_NumberOfTracks" , "PQA_NumberOfTracks" , 600, -0.5, 599.5));
  server->registerHisto("PQA_E_over_P_For_ElectronsSc" , eoverpESc = new TH1F("PQA_E_over_P_For_ElectronsSc" , "PQA_E_over_P_For_Electrons" , 200, 0.0, 3.0 ));
  server->registerHisto("PQA_E_over_P_For_ElectronsGl" , eoverpEGl = new TH1F("PQA_E_over_P_For_ElectronsGl" , "PQA_E_over_P_For_Electrons" , 200, 0.0, 3.0 ));

  server->registerHisto("PQA_invmassPi0_0_0" , invmassPi0_0_0 = new TH1F("PQA_invmassPi0_0_0" , "PQA_invmassPi0_0_0" , 200, -1, 3 ));
  server->registerHisto("PQA_invmassPi0_0_1" , invmassPi0_0_1 = new TH1F("PQA_invmassPi0_0_1" , "PQA_invmassPi0_0_1" , 200, -1, 3 ));
  server->registerHisto("PQA_invmassPi0_0_2" , invmassPi0_0_2 = new TH1F("PQA_invmassPi0_0_2" , "PQA_invmassPi0_0_2" , 200, -1, 3 ));
  server->registerHisto("PQA_invmassPi0_0_3" , invmassPi0_0_3 = new TH1F("PQA_invmassPi0_0_3" , "PQA_invmassPi0_0_3" , 200, -1, 3 ));
  server->registerHisto("PQA_invmassPi0_1_0" , invmassPi0_1_0 = new TH1F("PQA_invmassPi0_1_0" , "PQA_invmassPi0_1_0" , 200, -1, 3 ));
  server->registerHisto("PQA_invmassPi0_1_1" , invmassPi0_1_1 = new TH1F("PQA_invmassPi0_1_1" , "PQA_invmassPi0_1_1" , 200, -1, 3 ));
  server->registerHisto("PQA_invmassPi0_1_2" , invmassPi0_1_2 = new TH1F("PQA_invmassPi0_1_2" , "PQA_invmassPi0_1_2" , 200, -1, 3 ));
  server->registerHisto("PQA_invmassPi0_1_3" , invmassPi0_1_3 = new TH1F("PQA_invmassPi0_1_3" , "PQA_invmassPi0_1_3" , 200, -1, 3 ));

  server->registerHisto("PQA_CENTRAL" , CENTRAL = new TH1F("PQA_CENTRAL" , "PQA_CENTRAL" , 2300, -10., 120.));
  server->registerHisto("PQA_emcsGldphiH" , emcsGldphiH = new TH1F("PQA_emcsGldphiH" , "PQA_emcsGldphiH" , 700, -8, 8 ));
  server->registerHisto("PQA_emcsGldzH" , emcsGldzH = new TH1F("PQA_emcsGldzH" , "PQA_emcsGldzH" , 700, -8, 8 ));
  server->registerHisto("PQA_emcsScdphiH" , emcsScdphiH = new TH1F("PQA_emcsScdphiH" , "PQA_emcsScdphiH" , 700, -8, 8 ));
  server->registerHisto("PQA_emcsScdzH" , emcsScdzH = new TH1F("PQA_emcsScdzH" , "PQA_emcsScdzH" , 700, -8, 8 ));
  server->registerHisto("PQA_tofsdphiH" , tofsdphiH = new TH1F("PQA_tofsdphiH" , "PQA_tofsdphiH" , 700, -8, 8 ));
  server->registerHisto("PQA_tofsdzH" , tofsdzH = new TH1F("PQA_tofsdzH" , "PQA_tofsdzH" , 700, -8, 8 ));
  server->registerHisto("PQA_pc2sdphi" , pc2sdphi = new TH1F("PQA_pc2sdphi" , "PQA_pc2sdphi" , 700, -8, 8 ));
  server->registerHisto("PQA_pc2sdz" , pc2sdz = new TH1F("PQA_pc2sdz" , "PQA_pc2sdz" , 700, -8, 8 ));
  server->registerHisto("PQA_east_pc3sdphi" , east_pc3sdphi = new TH1F("PQA_east_pc3sdphi" , "PQA_east_pc3sdphi" , 700, -8, 8 ));
  server->registerHisto("PQA_east_pc3sdz" , east_pc3sdz = new TH1F("PQA_east_pc3sdz" , "PQA_east_pc3sdz" , 700, -8, 8 ));
  server->registerHisto("PQA_west_pc3sdphi" , west_pc3sdphi = new TH1F("PQA_west_pc3sdphi" , "PQA_west_pc3sdphi" , 700, -8, 8 ));
  server->registerHisto("PQA_west_pc3sdz" , west_pc3sdz = new TH1F("PQA_west_pc3sdz" , "PQA_west_pc3sdz" , 700, -8, 8 ));
  server->registerHisto("PQA_HIGHPC3SDPHI" , HIGHPC3SDPHI = new TH1F("PQA_HIGHPC3SDPHI" , "PQA_HIGHPC3SDPHI" , 1500, -8, 8 ));
  server->registerHisto("PQA_SMD_N_X" , SMDNX = new TH1F("PQA_SMD_N_X" , "SMD North X BPM Histogram" , 2000, -5., 5. ));
  server->registerHisto("PQA_SMD_N_Y" , SMDNY = new TH1F("PQA_SMD_N_Y" , "SMD North Y BPM Histogram" , 2000, -5., 5. ));
  server->registerHisto("PQA_SMD_S_X" , SMDSX = new TH1F("PQA_SMD_S_X" , "SMD South X BPM Histogram" , 2000, -5., 5. ));
  server->registerHisto("PQA_SMD_S_Y" , SMDSY = new TH1F("PQA_SMD_S_Y" , "SMD South Y BPM Histogram" , 2000, -5., 5. ));

  BBCTimeZero = -999;

  return 0;
}

int PhysicsqaReco::process_event(PHCompositeNode *topNode)
{
  TriggerHelper myTH(topNode);
  bool answer = myTH.didLevel1TriggerGetScaled("BBCLL1");
  if (!answer)
    return 0;

  PHCentralTrack *particle = findNode::getClass<PHCentralTrack>(topNode, "PHCentralTrack");
  PHGlobal *global = findNode::getClass<PHGlobal>(topNode, "PHGlobal");

  PhotonQA(topNode);

  if (particle && global)
    {
      int Npart = particle->get_npart();
      particle->ShutUp();
      numtracks->Fill(Npart);

      CENTRAL ->Fill(global->getBbcPercentile());

      SMDNX ->Fill(global->get_SmdXN());
      SMDNY ->Fill(global->get_SmdYN());
      SMDSX ->Fill(global->get_SmdXS());
      SMDSY ->Fill(global->get_SmdYS());

      for (int i = 0; i < Npart; i++)
        {
          float m2tof = particle->get_m2tof(i);
          float m2emc = particle->get_m2emc(i);

          int charge = particle->get_charge(i);
          int quality = particle->get_quality(i);

          if ((quality != 31) && (quality != 63))
            continue;
          float pt = particle->get_pt(i);

          float ecore = particle->get_ecore(i);

          float tofsdphi = particle->get_tofsdphi(i);
          float tofsdz = particle->get_tofsdz(i);

          float emcsdphi = particle->get_emcsdphi(i);
          float emcsdz = particle->get_emcsdz(i);

          int warnmap = particle->get_warnmap(i);
          int deadmap = particle->get_deadmap(i);
          int sect = particle->get_sect(i);
          int dcarm = particle->get_dcarm(i);

          int n0 = particle->get_n0(i);

          float chi2 = particle->get_chi2(i);
          float npe = particle->get_npe0(i);

          float pc2sdphiv = particle->get_pc2sdphi(i);
          float pc2sdzv = particle->get_pc2sdz(i);

          float pc3sdphi = particle->get_pc3sdphi(i);
          float pc3sdz = particle->get_pc3sdz(i);

          meanpt ->Fill(pt);

          if (pt > 3)
            HIGHPC3SDPHI->Fill(pc3sdphi);

          if (dcarm == 1)
            {
              if ((deadmap == 0) && (warnmap == 0))
                {
                  emcScmass2 ->Fill(m2emc);
                  if ((n0 > 2) && ((chi2 / npe) < 10) && (ecore > 0.4) && fabs(emcsdphi) < 3. && fabs(emcsdz) < 3.)
                    eoverpESc ->Fill(ecore / pt);
                  emcsScdphiH ->Fill(emcsdphi);
                  emcsScdzH ->Fill(emcsdz);
                }
              if (pt > 0.6)
                {
                  pc2sdphi ->Fill(pc2sdphiv);
                  pc2sdz ->Fill(pc2sdzv);
                  west_pc3sdphi ->Fill(pc3sdphi);
                  west_pc3sdz ->Fill(pc3sdz);
                }
            }
          else
            {
              if (pt > 0.6)
                {
                  tofsdphiH ->Fill(tofsdphi);
                  tofsdzH ->Fill(tofsdz);
                }
              if (fabs(tofsdphi) < 3 && fabs(tofsdz) < 3 && pt > 0.6)
                {
                  tofmass2 ->Fill(m2tof);
                }
              if (charge > 0 && fabs(tofsdphi) < 3 && fabs(tofsdz) < 3 && pt > 0.6)
                {
                  tofmass2pos ->Fill(m2tof);
                }
              else
                {
                  tofmass2neg ->Fill(m2tof);
                }
              if (pt > 0.6)
                {
                  east_pc3sdphi ->Fill(pc3sdphi);
                  east_pc3sdz ->Fill(pc3sdz);
                }
              if ((deadmap == 0) && (warnmap == 0))
                {
                  if (dcarm == 0 && sect > 1)
                    {
                      emcScmass2 ->Fill(m2emc);
                      if ((n0 > 2) && ((chi2 / npe) < 10) && (ecore > 0.4) && fabs(emcsdphi) < 3. && fabs(emcsdz) < 3.0)
                        eoverpESc ->Fill(ecore / pt);
                      emcsScdphiH ->Fill(emcsdphi);
                      emcsScdzH ->Fill(emcsdz);

                    }
                  else
                    {
                      if ((n0 > 2) && (ecore > 0.4) && fabs(emcsdphi) < 3. && fabs(emcsdz) < 3.)
                        eoverpEGl ->Fill(ecore / pt);
                      emcGlmass2 ->Fill(m2emc);
                      emcsGldphiH ->Fill(emcsdphi);
                      emcsGldzH ->Fill(emcsdz);

                    }
                }
            }
        }
    }



  return 0;
}

int PhysicsqaReco::PhotonQA(PHCompositeNode *topNode)
{

  PHNodeIterator iter(topNode);

  emcClusterContainer* clusters = findNode::getClass<emcClusterContainer>(topNode, "emcClusterContainer");

  if (!clusters)
    {
      cout << PHWHERE
	   << ": cannot find emcClusterContainer object." << endl;
      return -1;
    }

  if ( !clusters->isValid() )
    return -1;


  PHGlobal *global = findNode::getClass<PHGlobal>(topNode, "PHGlobal");

  BBCTimeZero = global->getBbcTimeZero();
  float zvertex = global->getZVertex();


  int nphot = 0;
  int arm_a = -1, sector_a = -1;
  int arm_b = -1, sector_b = -1;
  float ecore_a = 0, ecore_b = 0;
  float x_a = 0, y_a = 0, z_a = 0, r_a = 0;
  float x_b = 0, y_b = 0, z_b = 0, r_b = 0;
  float px_a = 0, py_a = 0, pz_a = 0;
  float px_b = 0, py_b = 0, pz_b = 0;
  float invmass = 0, asym = -2, pt = 0;
  float cosine = -2;

  unsigned int MAXCLUSTERS = clusters->size();
  if (MAXCLUSTERS == 0)
    return 0;
  for (unsigned int i = 0; i < MAXCLUSTERS - 1; ++i )
    {
      emcClusterContent* clustcont_a = clusters->getCluster(i);
      if (!clustcont_a)
        {
          continue;
        }
      arm_a = clustcont_a->arm();
      sector_a = clustcont_a->sector();
      x_a = clustcont_a->x();
      y_a = clustcont_a->y();
      z_a = clustcont_a->z();
      ecore_a = clustcont_a->ecore();

      r_a = sqrt( x_a * x_a + y_a * y_a + (z_a - zvertex) * (z_a - zvertex) );
      if (r_a < 500)
        continue;
      if (clustcont_a->chi2() >= 3.0 || clustcont_a->tof() - BBCTimeZero >= 5.0 || ecore_a < 0.5 )
        continue;

      px_a = (ecore_a * x_a) / r_a;
      py_a = (ecore_a * y_a) / r_a;
      pz_a = (ecore_a * z_a) / r_a;

      TLorentzVector v1(px_a, py_a, pz_a, ecore_a);
      nphot++;

      for (unsigned int j = i; j < MAXCLUSTERS; ++j )
        {
          emcClusterContent* clustcont_b = clusters->getCluster(j);
          if (!clustcont_b)
            {
              continue;
            }
          arm_b = clustcont_b->arm();
          sector_b = clustcont_b->sector();
          x_b = clustcont_b->x();
          y_b = clustcont_b->y();
          z_b = clustcont_b->z();
          ecore_b = clustcont_b->ecore();

          r_b = sqrt( x_b * x_b + y_b * y_b + (z_b - zvertex) * (z_b - zvertex) );
          if (r_b < 500)
            continue;

          if (clustcont_b->chi2() >= 3.0 ||
              clustcont_b->tof() - BBCTimeZero >= 5.0 ||
              ecore_b < 0.5 )
            continue;

          px_b = (ecore_b * x_b) / r_b;
          py_b = (ecore_b * y_b) / r_b;
          pz_b = (ecore_b * z_b) / r_b;

          TLorentzVector v2(px_b, py_b, pz_b, ecore_b);
          invmass = sqrt(fabs((v1 + v2) * (v1 + v2)));
          cosine = float( (v1.Dot(v2)) / (v1.Mag() * v2.Mag() ) );

          asym = (ecore_a - ecore_b ) / ( ecore_a + ecore_b );
          if (asym >= 0.8 || invmass < 0.02 ||
              (arm_a != arm_b) ||
              (arm_a == 1 && ((sector_a / 2) != (sector_b / 2)) )
	      )
            continue;

          pt = ( (v1 + v2).Pt() );
          FillPi0(arm_a, sector_a, arm_b, sector_b, pt, invmass, cosine);
        }
    }

  return 0;
}

int PhysicsqaReco::FillPi0(int arm_a, int sector_a, int arm_b, int sector_b,
                           float pt, float invmass, float cosine)
{

  if (pt >= 2.0 && cosine <= 0.990888)
    {
      if ((arm_a == 0) && (sector_a == 0))
        invmassPi0_0_0->Fill(invmass);
      if ((arm_a == 0) && (sector_a == 1))
        invmassPi0_0_1->Fill(invmass);
      if ((arm_a == 0) && (sector_a == 2))
        invmassPi0_0_2->Fill(invmass);
      if ((arm_a == 0) && (sector_a == 3))
        invmassPi0_0_3->Fill(invmass);
      if ((arm_a == 1) && (sector_a == 0))
        invmassPi0_1_0->Fill(invmass);
      if ((arm_a == 1) && (sector_a == 1))
        invmassPi0_1_1->Fill(invmass);
      if ((arm_a == 1) && (sector_a == 2))
        invmassPi0_1_2->Fill(invmass);
      if ((arm_a == 1) && (sector_a == 3))
        invmassPi0_1_3->Fill(invmass);

    }

  return 0;
}


int PhysicsqaReco::ReadHistogramsFromFile(const char *filename)
{
  //
  //  This routine is used to fetch histograms from an existing file
  //  and register them with the Fun4AllServer.  The advantage of this is
  //  twofold:
  //    1) Users who need statistics integrated across many files can
  //       use this utility to integrate their histograms later and
  //       then launch the final analysis to determine calibrations or qa.
  //    2) Developers can run the code to fill the histograms once
  //       (sometimes a slow process) and then run the code that fits
  //       the histograms many many times.
  //
  //  This code is based upon an OnlMon code that does a similar task.
  //  The one difference is that if a histogram in the incoming file
  //  already exists the contents of the new histogram are added to the
  //  existing one.
  //                                     TKH 10-17-2003  OnlCal
  //                                     TKH  7-31-2004  Fun4All
  //

  Fun4AllServer *se = Fun4AllServer::instance();

  FROG fr;
  TFile *histofile = new TFile(fr.location(filename), "READ");
  if (!histofile->IsOpen())
    {
      cout << "PhysicsqaReco::Can't open histogram file: " << filename << endl;
      return -1;
    }

  TIterator *titer = histofile->GetListOfKeys()->MakeIterator();
  TObject *obj;
  while ((obj = titer->Next()))
    {
      if (verbosity > 0)
        {
          cout << "TObject at " << obj;
          cout << " " << obj->GetName();
          cout << " " << obj->ClassName();
          cout << endl;

          cout << " HistoName:  " << ((TH1*)histofile->Get(obj->GetName()))->GetName();
          cout << " HistoClass: " << ((TH1*)histofile->Get(obj->GetName()))->ClassName();
          cout << endl << endl;
        }

      TH1* oldHisto = dynamic_cast<TH1*>(se->getHisto(((TH1*)histofile->Get(obj->GetName()))->GetName()));
      if ( !oldHisto )
        {
          // Brand new histogram...store it.
          TH1* histo = (TH1*) histofile->Get(obj->GetName())->Clone();
          histo->Sumw2();                          // Do good error propagation...
          se->registerHisto(histo->GetName(), histo);  // Add to existing map...
        }
      else
        {
          // Already existing histogram found...add new to old and discard new.
          TH1* histo = (TH1*) histofile->Get(obj->GetName())->Clone("temporary");
          histo->Sumw2();  // Do good error propagation...
          oldHisto->Add(histo);
          delete histo;
        }

    }
  delete titer;
  //histofile->Close();
  //delete histofile;

  return 0;
}


int PhysicsqaReco::End(PHCompositeNode *topNode)
{
  //
  //  In this routine we will "re-fetch" the pointers to
  //  all our histograms.  When this is a production run of the code
  //  this will re-assign all pointers to where they already point
  //  (seems useless and is).  However, if this is a post-production
  //  run, then the histograms themselves have *only* been read into
  //  the Fun4AllServer and we will only be able to access them
  //  by fetching these very pointers.  So, it swings both ways :).
  //
  //                    TKH MPM 7-31-2004
  //

  Fun4AllServer *server = Fun4AllServer::instance();
  meanpt = dynamic_cast<TH1*>(server->getHisto("PQA_mean_transverse_momentum"));
  tofmass2 = dynamic_cast<TH1*>(server->getHisto("PQA_tof_mass2_distribution"));
  tofmass2pos = dynamic_cast<TH1*>(server->getHisto("PQA_tof_mass2_distribution_pos"));
  tofmass2neg = dynamic_cast<TH1*>(server->getHisto("PQA_tof_mass2_distribution_neg"));
  emcGlmass2 = dynamic_cast<TH1*>(server->getHisto("PQA_emcGl_mass2_distribution"));
  emcScmass2 = dynamic_cast<TH1*>(server->getHisto("PQA_emcSc_mass2_distribution"));
  numtracks = dynamic_cast<TH1*>(server->getHisto("PQA_NumberOfTracks"));
  eoverpESc = dynamic_cast<TH1*>(server->getHisto("PQA_E_over_P_For_ElectronsSc"));
  eoverpEGl = dynamic_cast<TH1*>(server->getHisto("PQA_E_over_P_For_ElectronsGl"));
  invmassPi0_0_0 = dynamic_cast<TH1*>(server->getHisto("PQA_invmassPi0_0_0"));
  invmassPi0_0_1 = dynamic_cast<TH1*>(server->getHisto("PQA_invmassPi0_0_1"));
  invmassPi0_0_2 = dynamic_cast<TH1*>(server->getHisto("PQA_invmassPi0_0_2"));
  invmassPi0_0_3 = dynamic_cast<TH1*>(server->getHisto("PQA_invmassPi0_0_3"));
  invmassPi0_1_0 = dynamic_cast<TH1*>(server->getHisto("PQA_invmassPi0_1_0"));
  invmassPi0_1_1 = dynamic_cast<TH1*>(server->getHisto("PQA_invmassPi0_1_1"));
  invmassPi0_1_2 = dynamic_cast<TH1*>(server->getHisto("PQA_invmassPi0_1_2"));
  invmassPi0_1_3 = dynamic_cast<TH1*>(server->getHisto("PQA_invmassPi0_1_3"));
  CENTRAL = dynamic_cast<TH1*>(server->getHisto("PQA_CENTRAL"));
  emcsGldphiH = dynamic_cast<TH1*>(server->getHisto("PQA_emcsGldphiH"));
  emcsGldzH = dynamic_cast<TH1*>(server->getHisto("PQA_emcsGldzH"));
  emcsScdphiH = dynamic_cast<TH1*>(server->getHisto("PQA_emcsScdphiH"));
  emcsScdzH = dynamic_cast<TH1*>(server->getHisto("PQA_emcsScdzH"));
  tofsdphiH = dynamic_cast<TH1*>(server->getHisto("PQA_tofsdphiH"));
  tofsdzH = dynamic_cast<TH1*>(server->getHisto("PQA_tofsdzH"));
  pc2sdphi = dynamic_cast<TH1*>(server->getHisto("PQA_pc2sdphi"));
  pc2sdz = dynamic_cast<TH1*>(server->getHisto("PQA_pc2sdz"));
  east_pc3sdphi = dynamic_cast<TH1*>(server->getHisto("PQA_east_pc3sdphi"));
  east_pc3sdz = dynamic_cast<TH1*>(server->getHisto("PQA_east_pc3sdz"));
  west_pc3sdphi = dynamic_cast<TH1*>(server->getHisto("PQA_west_pc3sdphi"));
  west_pc3sdz = dynamic_cast<TH1*>(server->getHisto("PQA_west_pc3sdz"));
  HIGHPC3SDPHI = dynamic_cast<TH1*>(server->getHisto("PQA_HIGHPC3SDPHI"));
  SMDNX = dynamic_cast<TH1*>(server->getHisto("PQA_SMD_N_X"));
  SMDNY = dynamic_cast<TH1*>(server->getHisto("PQA_SMD_N_Y"));
  SMDSX = dynamic_cast<TH1*>(server->getHisto("PQA_SMD_S_X"));
  SMDSY = dynamic_cast<TH1*>(server->getHisto("PQA_SMD_S_Y"));

  //  A few general counters...
  // NOTE!!! GetEntries() is not always right...always use the Integral()...

  // Becasue of the changes to FrameWorkVars, it is no longer a usefull
  // method for extracting the run number. Now the run number must be
  // passed into PhysicsqaReco from the macro (or it will be filled
  // automatically during production).
  //                               -MPM
  recoConsts *rc = recoConsts::instance();
  int runNumber = rc->get_IntFlag("RUNNUMBER");

  double nEvent = numtracks->Integral();
  double nTrack = numtracks->GetMean() * nEvent;

  if (verbosity)
    {
      cout << "runNumber:   " << runNumber << endl;
      cout << "nEvent:      " << nEvent << endl;
      cout << "nTrack:      " << nTrack << endl;
    }

  //  Hold the collection of Database entries in a vector...

  //  By cramming everything about a single run into this vector and
  //  submitting it at once, everything about that run will have the
  //  same insert time in the database.  InsertTime is used extensively
  //  in managing the database later on.
  //                               -MPM 8/25/2004
  vector<QaEntry> Cesar;

  //  Tracks per event...
  QaEntry TrkPerEvt;
  TrkPerEvt.value = numtracks->GetMean();
  TrkPerEvt.error = numtracks->GetRMS() / sqrt(nEvent);
  TrkPerEvt.name = "<TracksPerEvent>";
  Cesar.push_back(TrkPerEvt);

  int bin1 = 0, bin2 = 0;
  //  Fraction of tracks with 3 sigma match in PC3...
  QaEntry Pc3EastMatchFraction;
  bin1 = east_pc3sdphi->FindBin( -3.0);
  bin2 = east_pc3sdphi->FindBin( 3.0);
  Pc3EastMatchFraction.value = east_pc3sdphi->Integral(bin1, bin2) / east_pc3sdphi->GetEntries();
  Pc3EastMatchFraction.error = sqrt(east_pc3sdphi->Integral(bin1, bin2)) / east_pc3sdphi->GetEntries(); // roughly true
  Pc3EastMatchFraction.name = "PC3 East Match Fraction";
  Cesar.push_back(Pc3EastMatchFraction);

  //  Fraction of tracks with 3 sigma match in PC3...
  QaEntry Pc3WestMatchFraction;
  bin1 = west_pc3sdphi->FindBin( -3.0);
  bin2 = west_pc3sdphi->FindBin( 3.0);
  Pc3WestMatchFraction.value = west_pc3sdphi->Integral(bin1, bin2) / west_pc3sdphi->GetEntries();
  Pc3WestMatchFraction.error = sqrt(west_pc3sdphi->Integral(bin1, bin2)) / west_pc3sdphi->GetEntries(); // roughly true
  Pc3WestMatchFraction.name = "PC3 West Match Fraction";
  Cesar.push_back(Pc3WestMatchFraction);

  //  Mean Pt of a Track during a Run...
  QaEntry MeanPt;
  MeanPt.value = meanpt->GetMean();
  MeanPt.error = meanpt->GetRMS() / sqrt(nTrack);
  MeanPt.name = "Mean Pt";
  Cesar.push_back(MeanPt);

  //  Keep on pushing!!!
  //
  //
  //

  cout << "commit = " << commit << endl;
  if (commit == 1)
    {

      // Commits Qa quantites calculated above into the appropriate database

      int SegmentNumber = -1;
      //string Tag = "Production";
      QaDatabaseManager database;
      database.WriteToDatabase("calibrations", "phnxrc", "physicsqa", runNumber, SegmentNumber, Tag, Cesar);
    }

  return 0;
}

int PhysicsqaReco::SetProductionTag(char *tagname)
{
  Tag = tagname;
  return 0;
}
