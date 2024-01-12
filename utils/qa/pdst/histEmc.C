/*
 * histEmc.C
 * Book and fill EMCal histograms.
 */

#include "histEmc.h"
#include "QADefs.h"
#include "dEmcClusterLocalExtWrapper.h"
#include "emcClusterContainer.h"
#include "emcClusterContent.h"
#include "emcTowerContainer.h"
#include "emcTowerContent.h"
#include "TriggerHelper.h"
#include "EmcIndexer.h"
#include "BbcOut.h"
#include "dCglTrackWrapper.h"
#include "CglTrack.h"

#include "getClass.h"
//ROOT headers
#include "TH1.h"
#include "TH2.h"
#include "TString.h"

#include "Fun4AllHistoManager.h"
#include "Fun4AllServer.h"

#include <list>
#include <algorithm>
#include <iostream>
using namespace std;

// Declare EMC histograms
TH1F *emcWest01ETot;
TH1F *emcWest23ETot;
TH1F *emcEastETot;
TH1F *emcPbGlETot;
TH1F *emcWest01E;
TH1F *emcWest23E;
TH1F *emcEastE;
TH1F *emcPbGlE;
TH1F *emcWest01E1;
TH1F *emcWest23E1;
TH1F *emcEastE1;
TH1F *emcPbGlE1;
TH1F *emcW0E;
TH1F *emcW1E;
TH1F *emcW2E;
TH1F *emcW3E;
TH1F *emcE0E;
TH1F *emcE1E;
TH1F *emcE2E;
TH1F *emcE3E;

TH1F *emcWest0Time;
TH1F *emcWest1Time;
TH1F *emcWest2Time;
TH1F *emcWest3Time;
TH1F *emcEast2Time;
TH1F *emcEast3Time;
TH1F *emcPbGl0Time;
TH1F *emcPbGl1Time;
TH2F *emcYZ_sector[8][7];
char name[127], title[255];
float bounds[8] = {0, 0.5, 1.0, 1.5, 2., 4., 8., 100000};

int QAEmc::InitRun(PHCompositeNode *topNode)
{
  Fun4AllServer *se = Fun4AllServer::instance();

  BbcOut * bbcout = findNode::getClass<BbcOut>(topNode, "BbcOut");

  emcClusterContainer* clusters =
    findNode::getClass<emcClusterContainer>(topNode, "emcClusterContainer");

  emcTowerContainer* towers =
    findNode::getClass<emcTowerContainer>(topNode, "emcTowerContainer");

  CglTrack* cgltrack = findNode::getClass<CglTrack>(topNode, "CglTrack");

  if (cgltrack == 0)
    {
      return -1;
    }

  if (!bbcout || !clusters || !towers || !cgltrack)
    {
      cout << "At least one of the objects are missed for EmcQA" << endl;
      se->unregisterSubsystem(this);
      return 0;
    }

  Fun4AllHistoManager *hm = se->getHistoManager(HistoManagerName);
  if (!hm)
    {
      hm = new Fun4AllHistoManager(HistoManagerName);
      se->registerHistoManager(hm);
    }

  // Total energy per event for pairs of sectors
  emcWest01ETot = new TH1F("emcWest01ETot", "EMCAL total energy West, Sec.01", 100, 0.0, 200.0);
  hm->registerHisto(emcWest01ETot);
  emcWest23ETot = new TH1F("emcWest23ETot", "EMCAL total energy West, Sec.23", 100, 0.0, 200.0);
  hm->registerHisto(emcWest23ETot);
  emcEastETot = new TH1F("emcEastETot", "EMCAL total energy East", 100, 0.0, 200.0);
  hm->registerHisto(emcEastETot);
  emcPbGlETot = new TH1F("emcPbGlETot", "EMCAL total energy PbGl", 100, 0.0, 200.0);
  hm->registerHisto(emcPbGlETot);

  // Cluster energy
  emcWest01E = new TH1F("emcWest01E", "EMCAL cluster energy West Sec. 01", 100, 0.0, 10.0);
  hm->registerHisto(emcWest01E);
  emcWest23E = new TH1F("emcWest23E", "EMCAL cluster energy West Sec. 23", 100, 0.0, 10.0);
  hm->registerHisto(emcWest23E);
  emcEastE = new TH1F("emcEastE", "EMCAL cluster energy East", 100, 0.0, 10.0);
  hm->registerHisto(emcEastE);
  emcPbGlE = new TH1F("emcPbGlE", "EMCAL cluster energy PbGl", 100, 0.0, 10.0);
  hm->registerHisto(emcPbGlE);

  // Cluster energy per sector (looking at Run-4 seems to be necessary!)
  // Differences seen _within_ granules
  emcW0E = new TH1F("emcW0E", "EMCAL cluster energy West Sec. 0", 100, 0.0, 5.0);
  hm->registerHisto(emcW0E);
  emcW1E = new TH1F("emcW1E", "EMCAL cluster energy West Sec. 1", 100, 0.0, 5.0);
  hm->registerHisto(emcW1E);
  emcW2E = new TH1F("emcW2E", "EMCAL cluster energy West Sec. 2", 100, 0.0, 5.0);
  hm->registerHisto(emcW2E);
  emcW3E = new TH1F("emcW3E", "EMCAL cluster energy West Sec. 3", 100, 0.0, 5.0);
  hm->registerHisto(emcW3E);
  emcE3E = new TH1F("emcE3E", "EMCAL cluster energy East Sec. 3", 100, 0.0, 5.0);
  hm->registerHisto(emcE3E);
  emcE2E = new TH1F("emcE2E", "EMCAL cluster energy East Sec. 2", 100, 0.0, 5.0);
  hm->registerHisto(emcE2E);
  emcE1E = new TH1F("emcE1E", "EMCAL cluster energy East Sec. 1", 100, 0.0, 5.0);
  hm->registerHisto(emcE1E);
  emcE0E = new TH1F("emcE0E", "EMCAL cluster energy East Sec. 0", 100, 0.0, 5.0);
  hm->registerHisto(emcE0E);


  // MIP peaks
  emcWest01E1 = new TH1F("emcWest01E1", "EMCAL cluster energy West Sec.01 0.17<E<1 minion cut", 90, 0.1, 1.0);
  hm->registerHisto(emcWest01E1);
  emcWest23E1 = new TH1F("emcWest23E1", "EMCAL cluster energy West Sec.23 0.17<E<1 minion cut", 90, 0.1, 1.0);
  hm->registerHisto(emcWest23E1);
  emcEastE1 = new TH1F("emcEastE1", "EMCAL cluster energy East 0.17<E<1 minion cut", 90, 0.1, 1.0);
  hm->registerHisto(emcEastE1);
  emcPbGlE1 = new TH1F("emcPbGlE1", "EMCAL cluster energy PbGl 0.17<E<1 minion cut", 90, 0.1, 1.0);
  hm->registerHisto(emcPbGlE1);

  // Timing distribution
  emcWest0Time = new TH1F("emcWest0Time", "EMCAL t - BBC t0 West Sec.0", 600, -20.0, 40.0);
  hm->registerHisto(emcWest0Time);
  emcWest1Time = new TH1F("emcWest1Time", "EMCAL t - BBC t0 West Sec.1", 600, -20.0, 40.0);
  hm->registerHisto(emcWest1Time);
  emcWest2Time = new TH1F("emcWest2Time", "EMCAL t - BBC t0 West Sec.2", 600, -20.0, 40.0);
  hm->registerHisto(emcWest2Time);
  emcWest3Time = new TH1F("emcWest3Time", "EMCAL t - BBC t0 West Sec.3", 600, -20.0, 40.0);
  hm->registerHisto(emcWest3Time);
  emcEast2Time = new TH1F("emcEast2Time", "EMCAL t - BBC t0 East 2", 600, -20.0, 40.0);
  hm->registerHisto(emcEast2Time);
  emcEast3Time = new TH1F("emcEast3Time", "EMCAL t - BBC t0 East 3", 600, -20.0, 40.0);
  hm->registerHisto(emcEast3Time);
  emcPbGl0Time = new TH1F("emcPbGl0Time", "EMCAL t - BBC t0 PbGl 0", 600, -20.0, 40.0);
  hm->registerHisto(emcPbGl0Time);
  emcPbGl1Time = new TH1F("emcPbGl1Time", "EMCAL t - BBC t0 PbGl 1", 600, -20.0, 40.0);
  hm->registerHisto(emcPbGl1Time);

  // Cluster distribution in each sector
  const char *sectname[8] = { "W0", "W1", "W2", "W3", "E0", "E1", "E2", "E3" };
  for (int iecut = 0; iecut < 7; iecut++)
    {
      TString name = "emc";
      name += sectname[0];
      name += "YZcut";
      name += iecut;
      emcYZ_sector[0][iecut] = new TH2F(name,
                                        Form("EMCal hit freq. y vs z, sector %s, energy %f - %f",
					     sectname[0], bounds[iecut], bounds[iecut + 1]),
					72, -0.5, 71.5, 36, -0.5, 35.5);
      hm->registerHisto(emcYZ_sector[0][iecut]);

      name = "emc";
      name += sectname[1];
      name += "YZcut";
      name += iecut;
      emcYZ_sector[1][iecut] = new TH2F(name,
                                        Form("EMCal hit freq. y vs z, sector %s, energy %f - %f",
					     sectname[1], bounds[iecut], bounds[iecut + 1]),
                                        72, -0.5, 71.5, 36, -0.5, 35.5);
      hm->registerHisto(emcYZ_sector[1][iecut]);

      name = "emc";
      name += sectname[2];
      name += "YZcut";
      name += iecut;
      emcYZ_sector[2][iecut] = new TH2F(name,
                                        Form("EMCal hit freq. y vs z, sector %s, energy %f - %f",
					     sectname[2], bounds[iecut], bounds[iecut + 1]),
                                        72, -0.5, 71.5, 36, -0.5, 35.5);
      hm->registerHisto(emcYZ_sector[2][iecut]);

      name = "emc";
      name += sectname[3];
      name += "YZcut";
      name += iecut;
      emcYZ_sector[3][iecut] = new TH2F(name,
                                        Form("EMCal hit freq. y vs z, sector %s, energy %f - %f",
					     sectname[3], bounds[iecut], bounds[iecut + 1]),
					72, -0.5, 71.5, 36, -0.5, 35.5);
      hm->registerHisto(emcYZ_sector[3][iecut]);

      name = "emc";
      name += sectname[4];
      name += "YZcut";
      name += iecut;
      emcYZ_sector[4][iecut] = new TH2F(name,
                                        Form("EMCal hit freq. y vs z, sector %s, energy %f - %f",
					     sectname[4], bounds[iecut], bounds[iecut + 1]),
                                        96, -0.5, 95.5, 48, -0.5, 47.5);
      hm->registerHisto(emcYZ_sector[4][iecut]);

      name = "emc";
      name += sectname[5];
      name += "YZcut";
      name += iecut;
      emcYZ_sector[5][iecut] = new TH2F(name,
                                        Form("EMCal hit freq. y vs z, sector %s, energy %f - %f",
					     sectname[5], bounds[iecut], bounds[iecut + 1]),
                                        96, -0.5, 95.5, 48, -0.5, 47.5);
      hm->registerHisto(emcYZ_sector[5][iecut]);

      name = "emc";
      name += sectname[6];
      name += "YZcut";
      name += iecut;
      emcYZ_sector[6][iecut] = new TH2F(name, Form("EMCal hit freq. y vs z, sector %s, energy %f - %f",
						   sectname[6], bounds[iecut], bounds[iecut + 1]),
                                        72, -0.5, 71.5, 36, -0.5, 35.5);
      hm->registerHisto(emcYZ_sector[6][iecut]);

      name = "emc";
      name += sectname[7];
      name += "YZcut";
      name += iecut;
      emcYZ_sector[7][iecut] = new TH2F(name,
                                        Form("EMCal hit freq. y vs z, sector %s, energy %f - %f",
					     sectname[7], bounds[iecut], bounds[iecut + 1]),
                                        72, -0.5, 71.5, 36, -0.5, 35.5);
      hm->registerHisto(emcYZ_sector[7][iecut]);
    }

  return 0;
}

int QAEmc::emcHistFill_cluscont(PHCompositeNode *topNode)
{
  BbcOut* bbcout = findNode::getClass<BbcOut>(topNode, "BbcOut");

  if (!bbcout)
    {
      return 0;
    }
  float bbct0 = bbcout->get_TimeZero();
  float bbcvz = bbcout->get_VertexPoint();
  float etotw01 = 0.0;
  float etotw23 = 0.0;
  float etote = 0.0;
  float etotePbGl = 0.0;
  float e_cur = 0.0, ecore = 0.0, ecent = 0.0;
  int sector;
  int arm;
  short ntwr;
  float chi2;
  float prob;
  float tofcorr;

  list<int> emcTracks;
  int emcclusid;
  int matchFlag;
  int towerID;
  int iypos, izpos;

  emcClusterContainer* clusters =
    findNode::getClass<emcClusterContainer>(topNode, "emcClusterContainer");
  emcTowerContainer* towers =
    findNode::getClass<emcTowerContainer>(topNode, "emcTowerContainer");

  CglTrack* cgltrack = findNode::getClass<CglTrack>(topNode, "CglTrack");
  if (cgltrack == 0)
    {
      cerr << PHWHERE
	   << ": cannot find CglTrack object." << endl;
      return -1;
    }

  TriggerHelper triggerhelper(topNode);

  if (!clusters)
    {
      cerr << PHWHERE
	   << ": cannot find emcClusterContainer object." << endl;
      return -1;
    }

  if (!towers)
    {
      cerr << PHWHERE
	   << ": cannot find emcTowerContainer object." << endl;
      return -1;
    }

  if ( triggerhelper.didLevel1TriggerFire("PPG(Pedestal)") ||
       triggerhelper.didLevel1TriggerFire("PPG(Test Pulse)") ||
       triggerhelper.didLevel1TriggerFire("PPG(Laser)"))
    {
      return 2;
    }

  if( fabs(bbcvz) < 40. )
    {

      // make a list of cluster ID's associated with a track
      // (this is used for MIP peaks)
      if (cgltrack)
        {
          for (unsigned int icgl = 0; icgl < cgltrack->get_CglNTrack(); icgl++)
            {
              emcclusid = cgltrack->get_emcclusid(icgl);
              if (emcclusid > -1)
                {
                  emcTracks.push_back(emcclusid);
                }
            }
          // sort cluster ID's in ascending order
          emcTracks.sort();
        }

      // loop over clusters
      for (size_t iemc = 0; iemc < clusters->size(); ++iemc)
        {
          emcClusterContent* clus = clusters->getCluster(iemc);
          // if cluster ID is in list of those associated with a track
          // set the matchFlag to 1
          matchFlag = 1; // no matching now, to be fixed
          if (emcTracks.size() > 0)
            {
              int tempid = emcTracks.front();
              if (tempid == (int) iemc)
                {
                  matchFlag = 1;
                  emcTracks.pop_front();
                  while (emcTracks.size() > 0 && (tempid == emcTracks.front()))
                    emcTracks.pop_front();
                }
            }


          ntwr = clus->multiplicity();
          e_cur = clus->e();
          ecore = clus->ecore();
          //	  ecorr = clus->ecorr();
          ecent = clus->ecent();
          chi2 = clus->chi2();
          prob = clus->prob_photon();
          tofcorr = clus->tof() - bbct0;

          arm = clus->arm();
          sector = clus->sector();

          if (clus->x() > 0)
            {
              if (sector < 2)
                {
                  etotw01 += e_cur;
                  emcWest01E->Fill(e_cur);

                  if (sector == 0)
                    emcW0E->Fill(e_cur);
                  if (sector == 1)
                    emcW1E->Fill(e_cur);

                  if (fabs(bbct0) < 1000. && ecore >= 0.5 && chi2 < 3)
                    {
                      if (sector == 0)
                        emcWest0Time->Fill(tofcorr);
                      if (sector == 1)
                        emcWest1Time->Fill(tofcorr);
                    }
                  // Min. ion.
                  if ( (e_cur > 0.17) && (chi2 > 3 || ntwr < 2) &&
                       matchFlag == 1)
                    emcWest01E1->Fill(e_cur);

                }
              else
                {
                  etotw23 += e_cur;
                  emcWest23E->Fill(e_cur);

                  if (sector == 2)
                    emcW2E->Fill(e_cur);
                  if (sector == 3)
                    emcW3E->Fill(e_cur);

                  if (fabs(bbct0) < 1000. && ecore >= 0.5 && chi2 < 3)
                    {
                      if (sector == 2)
                        emcWest2Time->Fill(tofcorr);
                      if (sector == 3)
                        emcWest3Time->Fill(tofcorr);
                    }
                  // Min. ion.
                  if ( (e_cur > 0.17) && (chi2 > 3 || ntwr < 2) &&
                       matchFlag == 1)
                    emcWest23E1->Fill(e_cur);
                }

            }
          else
            {
              if (sector < 2)
                {
                  etotePbGl += e_cur;
                  emcPbGlE->Fill(e_cur);

                  if (sector == 0)
                    emcE0E->Fill(e_cur);
                  if (sector == 1)
                    emcE1E->Fill(e_cur);

                  if (fabs(bbct0) < 1000. && ecore >= 0.5 && chi2 < 3)
                    {
                      if (sector == 0)
                        emcPbGl0Time->Fill(tofcorr);
                      if (sector == 1)
                        emcPbGl1Time->Fill(tofcorr);
                    }
                  // Min. ion
                  if ( (e_cur > 0.17) && (chi2 > 3 || ntwr < 2) &&
                       matchFlag == 1)
                    emcPbGlE1->Fill(e_cur);

                }
              else
                {

                  etote += e_cur;
                  emcEastE->Fill(e_cur);

                  if (sector == 2)
                    emcE2E->Fill(e_cur);
                  if (sector == 3)
                    emcE3E->Fill(e_cur);

                  if (fabs(bbct0) < 1000. && ecore >= 0.5 && chi2 < 3)
                    {
                      if (sector == 2)
                        emcEast2Time->Fill(tofcorr);
                      if (sector == 3)
                        emcEast3Time->Fill(tofcorr);
                    }
                  // Min. ion
                  if ( (e_cur > 0.17) && (chi2 > 3 || ntwr < 2) &&
                       matchFlag == 1)
                    emcEastE1->Fill(e_cur);

                }

            } // if West or East

        } // for loop over clusters

      // clear list after each event
      emcTracks.clear();

      // total energy per event in each pair of clusters
      emcWest01ETot->Fill(etotw01);
      emcWest23ETot->Fill(etotw23);
      emcEastETot->Fill(etote);
      emcPbGlETot->Fill(etotePbGl);

      // loop over towers
      for (size_t iemc = 0; iemc < towers->size(); ++iemc)
        {
          emcTowerContent* tower = towers->getTower(iemc);

          e_cur = tower->Energy();
          towerID = tower->TowerID();
          EmcIndexer::TowerLocation(towerID, arm, sector, iypos, izpos);

          // Sector-by-sector hit pattern with energy cuts
          for (int iecut = 0; iecut < 7; iecut++)
            {
              if (e_cur > bounds[iecut] && e_cur <= bounds[iecut + 1])
                emcYZ_sector[4*arm + sector][iecut]->Fill(izpos, iypos);
            }

        } // for loop over towers

    } // vertex cut

  return 0;
}

int QAEmc::emcHistFill_staftable(PHCompositeNode *topNode)
{
  BbcOut* bbcout = findNode::getClass<BbcOut>(topNode, "BbcOut");
  CglTrack* cgltrack = findNode::getClass<CglTrack>(topNode, "CglTrack");
  dEmcClusterLocalExtWrapper* emcclusterlocalext =
    findNode::getClass<dEmcClusterLocalExtWrapper>(topNode, "dEmcClusterLocalExt");

  if (!bbcout || !cgltrack || !emcclusterlocalext)
    {
      return 0;
    }

  float bbct0 = bbcout->get_TimeZero();
  float bbcvz = bbcout->get_VertexPoint();
  float etotw01 = 0.0;
  float etotw23 = 0.0;
  float etote = 0.0;
  float etotePbGl = 0.0;
  float e_cur = 0.0, ecore = 0.0, ecorr = 0.0, ecent = 0.0;
  int sector;
  int arm;
  short ntwr;
  float chi2;
  float prob;
  float tofcorr;

  list<int> emcTracks;
  int emcclusid;
  int matchFlag;


  if ( fabs(bbcvz) < 40. )
    {

      // make a list of cluster ID's associated with a track
      // (this is used for MIP peaks)
      if (cgltrack)
        {
          for (unsigned int icgl = 0; icgl < cgltrack->get_CglNTrack(); icgl++)
            {
              emcclusid = cgltrack->get_emcclusid(icgl);
              if (emcclusid > -1)
                {
                  emcTracks.push_back(emcclusid);
                }
            }
          // sort cluster ID's in ascending order
          emcTracks.sort();
        }

      // loop over clusters
      for (size_t iemc = 0; iemc < emcclusterlocalext->RowCount(); iemc++)
        {

          // if cluster ID is in list of those associated with a track
          // set the matchFlag to 1
          matchFlag = 0;
          if (emcTracks.size() > 0)
            {
	      int tempid = emcTracks.front();
              if (tempid == (int) iemc)
                {
                  matchFlag = 1;
                  emcTracks.pop_front();
                  while (emcTracks.size() > 0 && (tempid == emcTracks.front()))
                    emcTracks.pop_front();
                }
            }


          ntwr = emcclusterlocalext->get_twrhit(iemc);
          e_cur = emcclusterlocalext->get_e(iemc);
          ecorr = emcclusterlocalext->get_ecorr(iemc);
          ecent = emcclusterlocalext->get_ecent(iemc);
          chi2 = emcclusterlocalext->get_chi2(iemc);
          prob = emcclusterlocalext->get_prob_photon(iemc);
          tofcorr = emcclusterlocalext->get_tof(iemc) - bbct0;

          arm = emcclusterlocalext->get_arm(iemc);
          sector = emcclusterlocalext->get_sector(iemc);

          if (emcclusterlocalext->get_xyz(0, iemc) > 0)
            {
              if (sector < 2)
                {
                  etotw01 += e_cur;
                  emcWest01E->Fill(e_cur);

                  if (sector == 0)
                    emcW0E->Fill(e_cur);
                  if (sector == 1)
                    emcW1E->Fill(e_cur);

                  if (fabs(bbct0) < 1000. && ecore >= 0.5 && chi2 < 3)
                    {
                      if (sector == 0)
                        emcWest0Time->Fill(tofcorr);
                      if (sector == 1)
                        emcWest1Time->Fill(tofcorr);
                    }
                  // Min. ion.
                  if ( (e_cur > 0.17) && (chi2 > 0.1 || ntwr < 2) &&
                       matchFlag == 1)
                    emcWest01E1->Fill(e_cur);

                }
              else
                {
                  etotw23 += e_cur;
                  emcWest23E->Fill(e_cur);

                  if (sector == 2)
                    emcW2E->Fill(e_cur);
                  if (sector == 3)
                    emcW3E->Fill(e_cur);

                  if (fabs(bbct0) < 1000. && ecore >= 0.5 && chi2 < 3)
                    {
                      if (sector == 2)
                        emcWest2Time->Fill(tofcorr);
                      if (sector == 3)
                        emcWest3Time->Fill(tofcorr);
                    }
                  // Min. ion.
                  if ( (e_cur > 0.17) && (chi2 > 0.1 || ntwr < 2) &&
                       matchFlag == 1)
                    emcWest23E1->Fill(e_cur);
                }

              // Sector-by-sector hit pattern with energy cuts
              for (int iecut = 0; iecut < 4; iecut++)
                {
                  if (e_cur > iecut*0.5)
                    emcYZ_sector[sector][iecut]->Fill(emcclusterlocalext->get_ind(0, iemc),
                                                      emcclusterlocalext->get_ind(1, iemc));
                }
              if (e_cur > 0.1)
                emcYZ_sector[sector][4]->Fill(emcclusterlocalext->get_ind(0, iemc), emcclusterlocalext->get_ind(1, iemc));
            }
          else
            {
              if (sector < 2)
                {
                  etotePbGl += e_cur;
                  emcPbGlE->Fill(e_cur);

                  if (sector == 0)
                    emcE0E->Fill(e_cur);
                  if (sector == 1)
                    emcE1E->Fill(e_cur);

                  if (fabs(bbct0) < 1000. && ecore >= 0.5 && chi2 < 3)
                    {
                      if (sector == 0)
                        emcPbGl0Time->Fill(tofcorr);
                      if (sector == 1)
                        emcPbGl1Time->Fill(tofcorr);
                    }
                  // Min. ion
                  if ( (e_cur > 0.17) && (chi2 > 0.1 || ntwr < 2) &&
                       matchFlag == 1)
                    emcPbGlE1->Fill(e_cur);

                }
              else
                {

                  etote += e_cur;
                  emcEastE->Fill(e_cur);

                  if (sector == 2)
                    emcE2E->Fill(e_cur);
                  if (sector == 3)
                    emcE3E->Fill(e_cur);

                  if (fabs(bbct0) < 1000. && ecore >= 0.5 && chi2 < 3)
                    {
                      if (sector == 2)
                        emcEast2Time->Fill(tofcorr);
                      if (sector == 3)
                        emcEast3Time->Fill(tofcorr);
                    }
                  // Min. ion
                  if ( (e_cur > 0.17) && (chi2 > 0.1 || ntwr < 2) &&
                       matchFlag == 1)
                    emcEastE1->Fill(e_cur);

                }


              // Sector-by-sector hit pattern with energy cuts
              for (int iecut = 0; iecut < 4; iecut++)
                {
                  if (e_cur > iecut*0.5)
                    emcYZ_sector[4 + sector][iecut]->Fill(emcclusterlocalext->get_ind(0, iemc),
                                                          emcclusterlocalext->get_ind(1, iemc));
                }
              if (e_cur > 0.1 )
                emcYZ_sector[4 + sector][4]->Fill(emcclusterlocalext->get_ind(0, iemc), emcclusterlocalext->get_ind(1, iemc));


            } // if West or East

        } // for loop over clusters

      // clear list after each event
      emcTracks.clear();

      // total energy per event in each pair of clusters
      emcWest01ETot->Fill(etotw01);
      emcWest23ETot->Fill(etotw23);
      emcEastETot->Fill(etote);
      emcPbGlETot->Fill(etotePbGl);

    } // vertex cut

  return 0;
}

int QAEmc::process_event(PHCompositeNode *topNode)
{
  emcClusterContainer* clusters = findNode::getClass<emcClusterContainer>(topNode, "emcClusterContainer");

  if ( !clusters )
    {
      emcHistFill_staftable(topNode);
    }
  else
    {
      emcHistFill_cluscont(topNode);
    }
  return 0;
}

