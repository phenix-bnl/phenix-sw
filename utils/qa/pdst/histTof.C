#include <histTof.h>

// ROOT header files
#include <TH1.h>
#include <TH2.h>
#include <QADefs.h>                                                                                                                  

#include <Fun4AllHistoManager.h>
#include <Fun4AllServer.h>
#include <Fun4AllReturnCodes.h>

#include <CglTrack.h>
#include <dCglTrackWrapper.h>
#include <BbcOut.h>
#include <DchTrack.h>
#include <dTofReconstructedWrapper.h>
#include <PHTrackOut.h>
#include <getClass.h>

//Declare time-of-flight histograms
TH1F *tofDist;
TH1F *tofELoss;
TH2F *tofYZ;
TH2F *tofYZ_north;
TH2F *tofYX;
TH1F *tofProY;
TH1F *tofProZ;
TH2F *tofBetaPt;
TH2F *tofMass2p;

int QATof::InitRun(PHCompositeNode *topNode)
{
  Fun4AllServer *se = Fun4AllServer::instance();

  BbcOut* bbcout = findNode::getClass<BbcOut>(topNode, "BbcOut");
  PHTrackOut* phtrack = findNode::getClass<PHTrackOut>(topNode, "PHTrackOut");
  CglTrack* cgltrack = findNode::getClass<CglTrack>(topNode, "CglTrack");
  DchTrack* dchtrack = findNode::getClass<DchTrack>(topNode, "DchTrack");
  dTofReconstructedWrapper* tofreconstructed =
    findNode::getClass<dTofReconstructedWrapper>(topNode, "dTofReconstructed");

  if (!bbcout || !phtrack || !cgltrack || !dchtrack || !tofreconstructed )
    {
      se->unregisterSubsystem(this);
      return 0;
    }

  // check for HistoManager - if it does not exist, create it                                                                           // HistoManagerName is defined in QADefs.h

  Fun4AllHistoManager *hm = se->getHistoManager(HistoManagerName);
  if (!hm)
    {
      hm = new Fun4AllHistoManager(HistoManagerName);
      se->registerHistoManager(hm);
    }

  tofDist = new TH1F ("tofDist", "TOF - BBCt0 dist.", 400, 0.0, 100.0);
  hm->registerHisto(tofDist);
  tofELoss = new TH1F ("tofELoss", "Eloss dist.", 100, 0.0, 0.02);
  hm->registerHisto(tofELoss);
  tofYZ = new TH2F ("tofYZ", "TOF hit (y-z)",
                    1000, -300, 300, 1000, -300, 150);
  hm->registerHisto(tofYZ);
  tofYZ_north = new TH2F ("tofYZ_north", "TOF hit FEM north (y-z)",
                          1000, -300, 300, 1000, -300, 150);
  hm->registerHisto(tofYZ_north);
  tofYX = new TH2F ("tofYX", "TOF hit (y-x)",
                    1000, -600, -400, 1000, -300, 150);
  hm->registerHisto(tofYX);
  tofProY = new TH1F ("tofProY", "TOF - Cgl Pro(Y)", 500, -100, 100);
  hm->registerHisto(tofProY);
  tofProZ = new TH1F ("tofProZ", "TOF - Cgl Pro(Z)", 500, -100, 100);
  hm->registerHisto(tofProZ);
  tofBetaPt = new TH2F ("tofBetaPt", "1/BETA vs pt*charge",
                        2000, -4.0, 4.0, 2000, 0.8, 3);
  hm->registerHisto(tofBetaPt);
  tofMass2p = new TH2F ("tofMass2p", "p*charge vs Squared Mass",
                        2000, -0.3, 1.7, 2000, -5, 5);
  hm->registerHisto(tofMass2p);
  return 0;
}

int QATof::process_event(PHCompositeNode *topNode)
{
  BbcOut* bbcout = findNode::getClass<BbcOut>(topNode, "BbcOut");
  PHTrackOut* phtrack = findNode::getClass<PHTrackOut>(topNode, "PHTrackOut");
  CglTrack* cgltrack = findNode::getClass<CglTrack>(topNode, "CglTrack");
  DchTrack* dchtrack = findNode::getClass<DchTrack>(topNode, "DchTrack");
  dTofReconstructedWrapper* tofreconstructed =
    findNode::getClass<dTofReconstructedWrapper>(topNode, "dTofReconstructed");

  if (!bbcout || !phtrack || !cgltrack || !dchtrack || !tofreconstructed )
    {
      return EVENT_OK;
    }

  float bbct0 = bbcout->get_TimeZero();
  int ntrk = phtrack->get_PHNTrack();

  if ( (bbct0 > -200) && (bbct0 < 100) )
    {
      //init. ID
      int dchid = -1;
      int tofrecid = -1;

      for (int itrk = 0; itrk < ntrk; itrk++)
        {
          dchid = cgltrack->get_dctracksid(itrk);
          tofrecid = cgltrack->get_tofrecid(itrk);

          float toft, eloss, diff_y, diff_z;
          float mass2, beta;
          float xtof[3] = {0, 0, 0};
          float tofpro[3] = {0, 0, 0};
          int slatid = -1;

          //PHTrack content
          tofpro[0] = phtrack->get_projectionTof(itrk, 0);
          tofpro[1] = phtrack->get_projectionTof(itrk, 1);
          tofpro[2] = phtrack->get_projectionTof(itrk, 2);

          //DchTrack content
          float alpha = dchtrack->get_alpha(dchid);
          float the0 = dchtrack->get_theta0(dchid);
          float mom = dchtrack->get_momentum(dchid);
          float pt = fabs(mom * sin(the0));
          float quality = dchtrack->get_quality(dchid);
          float path = phtrack->get_tofPathLength(itrk);
          int charge = 0;
          if (alpha < 0)
            charge = 1;
          if (alpha > 0)
            charge = -1;

          if (tofrecid >= 0)
            {
              //TofReconstructed content
              slatid = tofreconstructed->get_slatid(tofrecid);
              toft = tofreconstructed->get_tof(tofrecid);
              eloss = tofreconstructed->get_eloss(tofrecid);
              xtof[0] = tofreconstructed->get_xtof(0, tofrecid);
              xtof[1] = tofreconstructed->get_xtof(1, tofrecid);
              xtof[2] = tofreconstructed->get_xtof(2, tofrecid);

              diff_y = xtof[1] - tofpro[1];
              diff_z = xtof[2] - tofpro[2];
              // Have to hack the square for lack of ** or ^ operator
              float masstmp = ((toft - bbct0) * 29.9792458 / path);
              masstmp *= masstmp;
              mass2 = mom * mom * ( masstmp - 1);
              beta = (toft - bbct0) * 29.9792458 / path;

              if ( (slatid >= 0) && (eloss > 0.002) )
                {
                  tofDist->Fill(toft - bbct0);
                  tofYZ->Fill(xtof[2], xtof[1]);

                  if (xtof[2] < 0)
                    {
                      tofYZ_north->Fill(xtof[2], xtof[1]);
                    }

                  tofYX->Fill(xtof[0], xtof[1]);
                }

              if (slatid >= 0)
                tofELoss->Fill(eloss);

              if ( (slatid >= 0) && (eloss > 0.002) && (quality > 20) )
                {
                  tofProY->Fill(diff_y);
                  tofProZ->Fill(diff_z);
                }

              if ( (slatid >= 0) && (eloss > 0.002) && (quality > 20) &&
                   (mom != 0) && (mom < 5) && (path > 400) && (path < 600) &&
                   (sqrt(diff_y*diff_y + diff_z*diff_z) < 5.0) )
                {
                  tofBetaPt->Fill(charge * pt, beta);
                  tofMass2p->Fill(mass2, charge * mom);
                }
            } //end if (tofrecid >= 0)
        } // end of itrk loop
    } // end if ( (bbct0 > -200) && (bbct0 < 100) )
  return 0;
}
