#include "histZdc.h"
#include "QADefs.h"

#include "TriggerHelper.h"

#include "ZdcOut.h"
#include "BbcOut.h"
#include "getClass.h"

#include "Fun4AllHistoManager.h"
#include "Fun4AllServer.h"

#include "TH1.h"
#include "TH2.h"

TriggerHelper *trig = 0;

// Declare ZDC histograms
TH1 *zdcenorth;      // North detector energy
TH1 *zdcesouth;      // South detector energy
TH1 *zdcesum;	      // Sum of energies
TH2 *zdcEnEs;	      // North vs. South energy, BBC&ZDC Trig Only
TH1 *zdcvtx;         // Collision Vertex Position
TH1 *zdct0;          // Collision Start Time
TH1 *zdcbbcvtx;      // Collision vertex position (ZDC - BBC)
TH1 *zdcbbct0;       // Collision Start Time (ZDC - BBC)

QAZdc::~QAZdc()
{
  if (trig)
    {
      delete trig;
    }
}

//-------------------------------------------------------------------------
/*
 *  Create the ZDC QA histograms
 */
int QAZdc::InitRun(PHCompositeNode *topNode)
{
  BbcOut * bbcout = findNode::getClass<BbcOut>(topNode, "BbcOut");
  ZdcOut * zdcout = findNode::getClass<ZdcOut>(topNode, "ZdcOut");
  Fun4AllServer *se = Fun4AllServer::instance();
  // check the required input, if it is missing, unregister myself
  // so Fun4All doesn't call me anymore
  if (!bbcout || !zdcout)
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

  const float MAX_ENERGY = 8000.;
  zdcenorth = new TH1F("zdcenorth", "ZDC North Energy", 2000, 0.0, MAX_ENERGY);
  zdcenorth->SetXTitle("Energy (GeV)");
  zdcenorth->SetLineColor(2);
  hm->registerHisto(zdcenorth);

  zdcesouth = new TH1F("zdcesouth", "ZDC South Energy", 2000, 0.0, MAX_ENERGY);
  zdcesouth->SetXTitle("Energy (GeV)");
  zdcesouth->SetLineColor(4);
  hm->registerHisto(zdcesouth);


  zdcesum = new TH1F("zdcesum", "ZDC Energy Sum", 200, 0.0, MAX_ENERGY);
  zdcesum->SetXTitle("Energy (Gev)");
  zdcesum->SetLineColor(2);
  hm->registerHisto(zdcesum);

  zdcEnEs = new TH2F("zdcEnEs", "ZDC North Energy vs South Energy",
                     200, 0.0, MAX_ENERGY, 200, 0.0, MAX_ENERGY);
  zdcEnEs->SetMarkerColor(2);
  zdcEnEs->SetYTitle("ZDC South Energy (GeV)");
  zdcEnEs->SetXTitle("ZDC North Energy (GeV)");
  hm->registerHisto(zdcEnEs);

  zdcvtx = new TH1F("zdcvtx", "ZDC Vertex", 160, -80., 80.);
  zdcvtx->SetLineColor(2);
  zdcvtx->SetXTitle("Vtx (cm)");
  hm->registerHisto(zdcvtx);

  zdcbbcvtx = new TH1F("zdcbbcvtx", "ZDC vtx - BBC vtx", 480, -40.0, 40.0);
  zdcbbcvtx->SetLineColor(2);
  zdcbbcvtx->SetXTitle("delta vtx (cm)");
  hm->registerHisto(zdcbbcvtx);

  zdct0 = new TH1F("zdct0", "ZDC Start Time", 200, -10., 10.);
  zdct0->SetLineColor(2);
  zdct0->SetXTitle("TimeZero (ns)");
  hm->registerHisto(zdct0);

  zdcbbct0 = new TH1F("zdcbbct0", "ZDC t0 - BBC t0", 480, -4.0, 4.0);
  zdcbbct0->SetLineColor(2);
  zdcbbct0->SetXTitle("delta t0 (cm)");
  hm->registerHisto(zdcbbct0);
  trig = new TriggerHelper(topNode);
  return 0;

}

//-------------------------------------------------------------------------

int QAZdc::process_event(PHCompositeNode *topNode)
{
  if (!trig->IsEventMinBias())
    {
      return 0;
    }
  BbcOut * bbcout = findNode::getClass<BbcOut>(topNode, "BbcOut");
  ZdcOut * zdcout = findNode::getClass<ZdcOut>(topNode, "ZdcOut");

  if (!bbcout || !zdcout)
    {
      return 0;
    }

  // get_Energy(0) returns south; get_Energy(1) returns north
  float zes = zdcout->get_Energy(0);
  float zen = zdcout->get_Energy(1);

  zdcenorth->Fill(zen);
  zdcesouth->Fill(zes);
  zdcesum->Fill(zen + zes);
  zdcEnEs->Fill(zen, zes);

  float zvtx = zdcout->get_Zvertex();
  zdcvtx->Fill( zvtx );
  zdcbbcvtx->Fill( zvtx - bbcout->get_VertexPoint());

  float zt0 = zdcout->get_TimeZero();
  zdct0->Fill( zt0 );
  zdcbbct0->Fill( zt0 - bbcout->get_TimeZero());

  return 0;
}

//EOF





