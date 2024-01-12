#include "histBbc.h"
#include "QADefs.h"

#include "BbcOut.h"
#include "getClass.h"

#include "Fun4AllHistoManager.h"
#include "Fun4AllServer.h"

#include "TH1.h"
#include "TH2.h"


TH1 *bbct0;
TH1 *bbcvtx;
TH1 *bbcnpmt;
TH1 *bbcQ;
TH2 *bbcQnQs;
TH2 *bbcvtxt0;

//--------------------------------------------------------------------

int QABbc::InitRun(PHCompositeNode *topNode)
{
  Fun4AllServer *se = Fun4AllServer::instance();
  BbcOut * bbcout = findNode::getClass<BbcOut>(topNode, "BbcOut");

  // check the required input, if it is missing, unregister myself
  // so Fun4All doesn't call me anymore
  if (!bbcout)
    {
      se->unregisterSubsystem(this);
      return 0;
    }
  // check for HistoManager - if it does not exist, create it
  // HistoManagerName is defined in QADefs.h
  Fun4AllHistoManager *hm = se->getHistoManager(HistoManagerName);
  if (!hm)
    {
      hm = new Fun4AllHistoManager(HistoManagerName);
      se->registerHistoManager(hm);
    }

  bbct0 = new TH1F("bbct0", "BBC TimeZero", 200, -20.0, 20.0);
  hm->registerHisto(bbct0);
  bbcvtx = new TH1F ("bbcvtx", "BBC z vertex", 200, -200.0, 200.0);
  hm->registerHisto(bbcvtx);
  bbcnpmt = new TH1F("bbcnpmt", "BBC n PMT", 100, 0.0, 150.0);
  hm->registerHisto(bbcnpmt);
  bbcQ = new TH1F("bbcQ", "BBC ChargeSum[mip]", 200, 0.0, 2000.0);
  hm->registerHisto(bbcQ);
  bbcQnQs = new TH2F("bbcQnQs", "BBC Qn vs Qs",
                     50, 0.0, 300.0, 50, 0.0, 300.0);
  hm->registerHisto(bbcQnQs);
  bbcvtxt0 = new TH2F("bbcvtxt0", "BBC TimeZero vs z vertex",
                      50, -200.0, 200.0, 50, -20.0, 20.0);
  hm->registerHisto(bbcvtxt0);

  return 0;
}

//--------------------------------------------------------------------

int QABbc::process_event(PHCompositeNode *topNode)
{
  BbcOut * bbcout = findNode::getClass<BbcOut>(topNode, "BbcOut");

  if (!bbcout)
    {
      return 0;
    }

  float nn = bbcout->get_nPmt(1);
  float ns = bbcout->get_nPmt(0);
  float qn = bbcout->get_ChargeSum(1);
  float qs = bbcout->get_ChargeSum(0);
  float t0 = bbcout->get_TimeZero();
  float vtx = bbcout->get_VertexPoint();

  bbct0->Fill(t0);
  bbcvtx->Fill(vtx);
  bbcnpmt->Fill(nn + ns);
  bbcQ->Fill(qn + qs);
  bbcQnQs->Fill(qn, qs);
  bbcvtxt0->Fill(vtx, t0);

  return 0;

}





