// Book and fill CRK histograms.

#include "histCrk.h"
#include "QADefs.h"

#include "dCrkHitWrapper.h"
#include "CrkHit.h"
#include "BbcOut.h"
#include "getClass.h"
#include "TH1.h"
#include "TH2.h"
#include <fstream>

#include "Fun4AllHistoManager.h"
#include "Fun4AllServer.h"

//
// From Here Crk funcs
//
TH1F *crkringz[4], *crkringphi[4], *crkringnpe[4];
TH1F *crkpe[32], *crkncrk, *crkhit;
TH2F *crkt0corr, *crkt0bbc, *crkt0raw, *crkpmtphiz[4];
float tac[5120], charge[5120], toff[5120], reject[5120];


int QACrk::InitRun(PHCompositeNode *topNode)
{
  Fun4AllServer *se = Fun4AllServer::instance();
  BbcOut * bbcout = findNode::getClass<BbcOut>(topNode, "BbcOut");
  CrkHit * dcrkhit = findNode::getClass<CrkHit>(topNode, "CrkHit");

  if (!bbcout || !dcrkhit)
    {
      if (!dcrkhit)
        std::cout << ThisName << " : CrkHit object not found." << std::endl;
      if (!bbcout)
        std::cout << ThisName << " : BbcOut object not found." << std::endl;
      se->unregisterSubsystem(this);
      return 0;
    }

  Fun4AllHistoManager *hm = se->getHistoManager(HistoManagerName);
  if (!hm)
    {
      hm = new Fun4AllHistoManager(HistoManagerName);
      se->registerHistoManager(hm);
    }

  int i, ch, count;
  float t0, error;

  for (i = 0;i < 5120;i++)
    toff[i] = -1000;
  for (i = 0;i < 5120;i++)
    reject[i] = 0;
  std::ifstream fin("crk_t0_offset.txt");
  while (fin >> ch >> count >> t0 >> error )
    toff[ch] = t0;
  fin.close();
  fin.open("crk_reject_map.txt");
  while (fin >> ch )
    {
      reject[ch] = 1;
    }
  fin.close();

  crkncrk = new TH1F("crkncrk", "Number of Crk hits distribution", 500, 0, 500);
  hm->registerHisto(crkncrk);
  crkt0corr = new TH2F("crkt0corr", "Crk T0 distribution (After rough correction)", 5120, 0, 5120, 600, -60, 60);
  hm->registerHisto(crkt0corr);
  crkt0bbc = new TH2F("crkt0bbc", "Crk T0 distribution (After bbc subtract)", 5120, 0, 5120, 600, -60, 60);
  hm->registerHisto(crkt0bbc);
  crkt0raw = new TH2F("crkt0raw", "Crk T0 distribution (NO correction)", 5120, 0, 5120, 600, -60, 60);
  hm->registerHisto(crkt0raw);
  crkhit = new TH1F("crkhit", "Crk Hit distribution", 5120, 0, 5120);
  hm->registerHisto(crkhit);

  for (i = 0;i < 32;i++)
    {
      char title[50];
      char explain[50];
      sprintf(title, "crkpe%d", i);
      sprintf(explain, "Crk Charge distribution %d", i);
      crkpe[i] = new TH1F(title, explain, 100, 0, 10);
      hm->registerHisto(crkpe[i]);
    }

  crkringz[0] = new TH1F("crkringz0", "Ring Z distribution in WS", 128, -264, -136);
  hm->registerHisto(crkringz[0]);
  crkringz[1] = new TH1F("crkringz1", "Ring Z distribution in WN", 128, 136, 264);
  hm->registerHisto(crkringz[1]);
  crkringz[2] = new TH1F("crkringz2", "Ring Z distribution in ES", 128, -264, -136);
  hm->registerHisto(crkringz[2]);
  crkringz[3] = new TH1F("crkringz3", "Ring Z distribution in EN", 128, 136, 264);
  hm->registerHisto(crkringz[3]);

  crkringphi[0] = new TH1F("crkringphi0", "Ring Phi distribution in WS", 110, -40, 70);
  hm->registerHisto(crkringphi[0]);
  crkringphi[1] = new TH1F("crkringphi1", "Ring Phi distribution in WN", 110, -40, 70);
  hm->registerHisto(crkringphi[1]);
  crkringphi[2] = new TH1F("crkringphi2", "Ring Phi distribution in ES", 110, -40, 70);
  hm->registerHisto(crkringphi[2]);
  crkringphi[3] = new TH1F("crkringphi3", "Ring Phi distribution in EN", 110, -40, 70);
  hm->registerHisto(crkringphi[3]);

  crkpmtphiz[0] = new TH2F("crkpmtphiz0", "Pmt ZxPhi distribution in WS", 80, -0.5, 79.5, 16, -0.5, 15.5);
  hm->registerHisto(crkpmtphiz[0]);
  crkpmtphiz[1] = new TH2F("crkpmtphiz1", "Pmt ZxPhi distribution in WN", 80, -0.5, 79.5, 16, -0.5, 15.5);
  hm->registerHisto(crkpmtphiz[1]);
  crkpmtphiz[2] = new TH2F("crkpmtphiz2", "Pmt ZxPhi distribution in ES", 80, -0.5, 79.5, 16, -0.5, 15.5);
  hm->registerHisto(crkpmtphiz[2]);
  crkpmtphiz[3] = new TH2F("crkpmtphiz3", "Pmt ZxPhi distribution in EN", 80, -0.5, 79.5, 16, -0.5, 15.5);
  hm->registerHisto(crkpmtphiz[3]);

  crkringnpe[0] = new TH1F("crkringnpe0", "NPE distribution of Ring in WS", 300, 0, 30);
  hm->registerHisto(crkringnpe[0]);
  crkringnpe[1] = new TH1F("crkringnpe1", "NPE distribution of Ring in WN", 300, 0, 30);
  hm->registerHisto(crkringnpe[1]);
  crkringnpe[2] = new TH1F("crkringnpe2", "NPE distribution of Ring in ES", 300, 0, 30);
  hm->registerHisto(crkringnpe[2]);
  crkringnpe[3] = new TH1F("crkringnpe3", "NPE distribution of Ring in EN", 300, 0, 30);
  hm->registerHisto(crkringnpe[3]);

  return 0;
}

int QACrk::process_event(PHCompositeNode *topNode)
{
  BbcOut * bbcout = findNode::getClass<BbcOut>(topNode, "BbcOut");
  CrkHit * dcrkhit = findNode::getClass<CrkHit>(topNode, "CrkHit");

  if (!bbcout || !dcrkhit)
    {
      std::cout << ThisName << " : CrkHit object not found." << std::endl;
      return 0;
    }

  int i;

  for (i = 0;i < 5120;i++)
    {
      charge[i] = -1.0;
      tac[i] = -2000.0;
    }

  float bbct0 = bbcout->get_TimeZero();

  //  int ncrk = dcrkhit->RowCount();
  int ncrk = dcrkhit->get_CrkNHit();
  crkncrk->Fill(ncrk);

  for (i = 0;i < ncrk;i++)
    {
      int pmtid = dcrkhit->get_pmt(i);
      if (!reject[pmtid])
        {
          charge[pmtid] = dcrkhit->get_npe(i);
          tac[pmtid] = dcrkhit->get_time(i);
          crkhit->Fill(pmtid);
          crkpe[pmtid / 160]->Fill(charge[pmtid]);
          if (charge[pmtid] > 1.5)
            {
              if (toff[pmtid] > -1000)
                crkt0corr->Fill(pmtid, tac[pmtid] - bbct0 / 1.12 - toff[pmtid]);
              else
                crkt0corr->Fill(pmtid, -1000);
              crkt0bbc->Fill(pmtid, tac[pmtid] - bbct0 / 1.12);
              crkt0raw->Fill(pmtid, tac[pmtid]);
              int row = pmtid / 16;
              int sect = row / 80;
              int iz = pmtid - row * 16;
              int iphi = row - sect * 80;
              crkpmtphiz[sect]->Fill(iphi, iz);
            }
        }
    }
  ring_finder(charge);
  return 0;
}


void QACrk::ring_finder(float hitmat[])
{

  int nrow = 320, npmt = 16, nsum = 4;
  int hitreg[5120];

  // Make a list of which rows of pmt's contain hits. There are 320 rows.

  int smhit[320];
  for (int ism = 0;ism < nrow;ism++)
    {
      smhit[ism] = 0;
      for (int ipmt = 0;ipmt < npmt;ipmt++)
        {
          int iaddr = ism * npmt + ipmt;
          if (hitmat[iaddr] > 0)
            {

              // This pmt has a hit
              hitreg[iaddr] = 1;

              // This row has a hit
              smhit[ism] = 1;
            }
          else
            hitreg[iaddr] = 0;
        }
    }

  // Now find which of these hit rows are contiguous

  int smhitkeep = -5, working = 0, hcount = -1;
  int candidate[100][100], numcand[100];

  for (int ism = 0;ism < nrow;ism++)
    {
      if (smhit[ism] == 1)
        {
          if (ism == smhitkeep + 1)
            {
              working++;
              smhitkeep++;
            }
          else
            {
              smhitkeep = ism;
              working = 1;
            }
        }
      else
        {
          if (working > 1)
            {
              hcount++;
              numcand[hcount] = working;

              for (int ic = 0;ic < working;ic++)
                {
                  candidate[hcount][ic] = ism + ic - working;
                }
            }
          working = 0;
        }
    }
  hcount++;

  // So we have clusters of hit rows, now we want to look for clusters of hits,
  // ie. rings, in those clusters of hit rows

  // Make things simple by only looking at clusters of 3,4 or 5 supermodules

  int pmtnum;
  for (int hc = 0;hc < hcount;hc++)
    {
      if (numcand[hc] == 3 || numcand[hc] == 4 || numcand[hc] == 5 )
        {

          float sum, sumkeep = 0;
          int jsm, sumaddr = 0, sumhit = 0, sumpmtnum = 0, sumhitkeep = 0;

          // Slide a 4 pmt wide sum window through this cluster of hit rows, and
          // look for the maximum summed yield

          for (int ipmt = 0;ipmt < npmt - 4;ipmt++)
            {

              sum = 0;
              sumhit = 0;
              for (int ic = 0;ic < numcand[hc];ic++)
                {

                  jsm = candidate[hc][ic];
                  pmtnum = jsm * npmt + ipmt;

                  sum = sum + hitmat[pmtnum] + hitmat[pmtnum + 1] + hitmat[pmtnum + 2]
		    + hitmat[pmtnum + 3];

                  sumhit = sumhit + hitreg[pmtnum] + hitreg[pmtnum + 1] + hitreg[pmtnum + 2]
		    + hitreg[pmtnum + 3];
                }
              if (sum > sumkeep)
                {
                  sumaddr = ipmt;
                  sumkeep = sum;
                  sumhitkeep = sumhit;
                  sumpmtnum = pmtnum;
                }
            }

          // Make cuts to:
          //    Keep only "rings" that have sumkeep values between 2 and 30
          //    Keep only "rings" that have at least 2 PMT's hit, and have less
          //       than all of the PMT's in the sum turned on
          //    Reject rings from sum ranges that begin at ipmt = 0 or 1, or
          //       end at ipmt = 14 or 15. This is to avoid edge effects due to
          //       partial rings and also noisy tubes (which are more common at
          //       the ends of the rows)


          if ( (sumkeep > 2 && sumkeep < 30) &&
               (sumhitkeep > 2 && sumhitkeep < 4*numcand[hc]) &&
               (sumaddr > 1 && sumaddr < npmt - 4 - 1) )
            {

              // Record the number of PE's in a histogram

              int sector = candidate[hc][1] / 80;

              crkringnpe[sector]->Fill(sumkeep);

              // Now get the centroid of the ring

              float zcontr = 0, norm = 0;
              float phicontr = 0;

              for (int ic = 0;ic < numcand[hc];ic++)
                {

                  jsm = candidate[hc][ic];

                  for (int isum = 0;isum < nsum;isum++)
                    {

                      pmtnum = jsm * npmt + sumaddr + isum;

                      float z, phi;
                      int sect;
                      get_pmt_coords(pmtnum, &sect, &z, &phi);

                      zcontr = zcontr + hitmat[pmtnum] * z;
                      phicontr = phicontr + hitmat[pmtnum] * phi;
                      norm = norm + hitmat[pmtnum];
                    }
                }
              zcontr = zcontr / norm;
              phicontr = phicontr / norm;

              crkringphi[sector]->Fill(phicontr);
              crkringz[sector]->Fill(zcontr);
            }
        }
    }
}


int QACrk::get_pmt_coords(int pmtnum, int *sect, float *z, float *phi)
{

  // Determine the PMT coordinates from the pmt number

  // The first 1280 PMT's are in the south end of the west RICH, sector 0
  // The second 1280 are in the north end of the west RICH, sector 1
  // The third 1280 are in the south end of the east RICH, sector 2
  // The fourth 1280 are in the north end of the east RICH, sector 3

  float pmt_phi_min = -33.0365;
  float pmt_dphi = 1.11216;

  float z_pmt[32] = {141.227, 150.056, 158.885, 167.714, 176.543, 185.372, 194.201,
                     203.03, 211.859, 220.688, 229.532, 237.948, 246.055, 253.983,
                     260.134, 264.587, 145.53, 154.359, 163.188, 172.017, 180.846,
                     189.675, 198.504, 207.333, 216.162, 225.217, 233.848, 242.047,
                     250.061, 257.907, 262.362, 266.815};

  int row = pmtnum / 16;
  int zloc = pmtnum - row * 16;
  int sector = row / 80;
  int iphi = row - sector * 80;

  *phi = (pmt_phi_min + (iphi + 0.5) * (pmt_dphi));

  if (((sector == 1 || sector == 2) && (iphi % 2 == 1)) ||
      ((sector == 0 || sector == 3) && (iphi % 2 == 0)))
    {
      *z = z_pmt[zloc];
    }
  else
    {
      *z = z_pmt[zloc + 16];
    }
  if ( sector % 2 == 0)
    *z = -*z;   // Z < 0

  *sect = sector;

  return 0;
}
