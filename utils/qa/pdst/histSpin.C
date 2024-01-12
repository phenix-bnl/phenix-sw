#include "histSpin.h"
#include "TrigLvl1.h"
#include "SpinDataEventOut.h"
#include "RunHeader.h"
#include "getClass.h"
#include "EventTypes.h"
#include "TH1.h"
#include "QADefs.h"

#include "Fun4AllHistoManager.h"
#include "Fun4AllServer.h"

#include "Event.h"

#include <cstdlib>
#include <cmath>
#include <iostream>
#include <sstream>

using namespace std;

///////////////////////////////////////////////////////////////////
#define PACKETID_GL1P_SPECIAL 850
#define PACKETID_CDEV_RUN5 819
#define NCROSS 120
#define NSCALER 4
#define N_GL1P_BOARD 2

TH1 *spin_cross;
TH1 *spin_cross_corr;
TH1 *spin_cross_gl1p[N_GL1P_BOARD];
TH1 *spin_fillpat_V124_b[2]; // [empty,filled]
TH1 *spin_fillpat_V124_y[2]; // [empty,filled]
TH1 *spin_spinpat_V124_b[3]; // [+,-,0]
TH1 *spin_spinpat_V124_y[3]; // [+,-,0]
TH1 *spin_fillpat_CDEV_b[2]; // [empty,filled]
TH1 *spin_fillpat_CDEV_y[2]; // [empty,filled]
TH1 *spin_spinpat_CDEV_b[3]; // [+,-,0]
TH1 *spin_spinpat_CDEV_y[3]; // [+,-,0]
TH1D *spin_scaler_event_min;
TH1D *spin_scaler_event_max;
TH1D *spin_scaler[NSCALER]; // [BBC_vtxcut,BBC_novtxcut,ZDC_wide,ZDC_narrow]
TH1D *spin_scaler_1st[NSCALER];
TH1D *spin_scaler_sum[NSCALER];
TH1D *spin_scaler_sum_1st[NSCALER];
TH1D *spin_scaler_2mom[NSCALER];
TH1D *spin_scaler_beg[NSCALER];
TH1D *spin_scaler_end[NSCALER];


int
QASpin::InitRun(PHCompositeNode *topNode)
{
  Fun4AllServer *se = Fun4AllServer::instance();
  SpinDataEventOut *spin_out;
  TrigLvl1 *trig_lvl1;

  spin_out = findNode::getClass<SpinDataEventOut>(topNode, "SpinDataEventOut");
  trig_lvl1 = findNode::getClass<TrigLvl1>(topNode, "TrigLvl1");

  if (!spin_out || !trig_lvl1)
    {
      printf("no spin_out\n");
      se->unregisterSubsystem(this);
      return (0);
    }

  DefineHistogram();
  return (0);
}

int
QASpin::End(PHCompositeNode *topNode)
{
#ifdef DEBUG
  spin_cross->Write();
  spin_cross_corr->Write();
  for (int i = 0; i < 2; i++)
    {
      spin_cross_gl1p[i]->Write();
    }

  for (int i = 0; i < 2; i++)
    {
      spin_fillpat_V124_b[i]->Write();
    }
  for (int i = 0; i < 2; i++)
    {
      spin_fillpat_V124_y[i]->Write();
    }
  for (int i = 0; i < 3; i++)
    {
      spin_spinpat_V124_b[i]->Write();
    }
  for (int i = 0; i < 3; i++)
    {
      spin_spinpat_V124_y[i]->Write();
    }

  for (int i = 0; i < 2; i++)
    {
      spin_fillpat_CDEV_b[i]->Write();
    }
  for (int i = 0; i < 2; i++)
    {
      spin_fillpat_CDEV_y[i]->Write();
    }
  for (int i = 0; i < 3; i++)
    {
      spin_spinpat_CDEV_b[i]->Write();
    }
  for (int i = 0; i < 3; i++)
    {
      spin_spinpat_CDEV_y[i]->Write();
    }

  spin_scaler_event_min->Write();
  spin_scaler_event_max->Write();
  for (int i = 0; i < NSCALER; i++)
    {
      spin_scaler[i]->Write();
    }
  for (int i = 0; i < NSCALER; i++)
    {
      spin_scaler_1st[i]->Write();
    }
  for (int i = 0; i < NSCALER; i++)
    {
      spin_scaler_sum[i]->Write();
    }
  for (int i = 0; i < NSCALER; i++)
    {
      spin_scaler_sum_1st[i]->Write();
    }
  for (int i = 0; i < NSCALER; i++)
    {
      spin_scaler_2mom[i]->Write();
    }
  for (int i = 0; i < NSCALER; i++)
    {
      spin_scaler_beg[i]->Write();
    }
  for (int i = 0; i < NSCALER; i++)
    {
      spin_scaler_end[i]->Write();
    }
#endif
  return (0);
}

int
QASpin::process_event(PHCompositeNode *topNode)
{
  process_event_spin(topNode);
  process_event_CDEV(topNode);
  process_event_special(topNode);
  return (0);
}

int QASpin::process_event_spin(PHCompositeNode *topNode)
{
  SpinDataEventOut *spin_out;
  TrigLvl1 *trig_lvl1;

  spin_out = findNode::getClass<SpinDataEventOut>(topNode, "SpinDataEventOut");
  trig_lvl1 = findNode::getClass<TrigLvl1>(topNode, "TrigLvl1");

  if (!spin_out || !trig_lvl1)
    {
      printf("no spin_out\n");
      return (0);
    }

  if (trig_lvl1->get_lvl1_trigscaled() == 0 ||
      spin_out->GetEventSequence() < 0)
    {
      return (0);
    }

  int evtnum = spin_out->GetEventSequence();
  int cross = spin_out->GetGL1CrossingID();
  spin_cross->Fill(cross);
  spin_cross_corr->Fill(spin_out->GetSpinGL1CrossingID());
  for (int ib = 0; ib < N_GL1P_BOARD; ib++)
    {
      spin_cross_gl1p[ib]->Fill(spin_out->GetGL1PCrossingID(ib));
    }

  int spinpat_v124_b = (int)spin_out->GetSpinDirectionBlueFromV124();
  int spinpat_v124_y = (int)spin_out->GetSpinDirectionYellowFromV124();
  if (abs(spinpat_v124_b) <= 1)
    {
      spin_fillpat_V124_b[1]->Fill(cross);
    }
  else
    {
      spin_fillpat_V124_b[0]->Fill(cross);
    }
  if (abs(spinpat_v124_y) <= 1)
    {
      spin_fillpat_V124_y[1]->Fill(cross);
    }
  else
    {
      spin_fillpat_V124_y[0]->Fill(cross);
    }
  if (spinpat_v124_b == 1)
    {
      spin_spinpat_V124_b[0]->Fill(cross);
    }
  else if (spinpat_v124_b == -1)
    {
      spin_spinpat_V124_b[1]->Fill(cross);
    }
  else
    {
      spin_spinpat_V124_b[2]->Fill(cross);
    }
  if (spinpat_v124_y == 1)
    {
      spin_spinpat_V124_y[0]->Fill(cross);
    }
  else if (spinpat_v124_y == -1)
    {
      spin_spinpat_V124_y[1]->Fill(cross);
    }
  else
    {
      spin_spinpat_V124_y[2]->Fill(cross);
    }

  int evtmin = (int)spin_scaler_event_min->GetBinContent(cross + 1);
  int evtmax = (int)spin_scaler_event_max->GetBinContent(cross + 1);

  for (int iscaler = 0; iscaler < NSCALER; iscaler++)
    {
      if (evtnum < 0)
        {
          continue;
        }

      int sc_count = spin_out->GetGL1PScalerCount(0, iscaler);
      long sc_count_sum = spin_out->GetGL1PSumScalerCount(0, iscaler);

      spin_scaler[iscaler]->Fill(cross, sc_count);
      spin_scaler_2mom[iscaler]->Fill(cross, pow((double)sc_count, 2.0));

      if (evtnum > evtmax)
        {
          spin_scaler_sum[iscaler]->SetBinContent(cross + 1, sc_count_sum);
        }

      if (evtnum < evtmin)
        {
          spin_scaler_1st[iscaler]->SetBinContent(cross + 1, sc_count);
          spin_scaler_sum_1st[iscaler]->SetBinContent(cross + 1, sc_count_sum);
        }
    }

  if (evtnum < evtmin)
    {
      spin_scaler_event_min->SetBinContent(cross + 1, evtnum);
    }
  if (evtnum > evtmax)
    {
      spin_scaler_event_max->SetBinContent(cross + 1, evtnum);
    }

  return (0);
};
///////////////////////////////////////////////////////////////////
int
QASpin::process_event_CDEV(PHCompositeNode *topNode)
{
  int fillpat_cdev_b[NCROSS], fillpat_cdev_y[NCROSS];
  int spinpat_cdev_b[NCROSS], spinpat_cdev_y[NCROSS];

  RunHeader *rh = findNode::getClass<RunHeader>(topNode, "RunHeader");
  if (!rh)
    {
      cout << PHWHERE << "RunHeader is missing" << endl;
      exit(1);
    }
  int runnum = rh->get_RunNumber();

  if (runnum < 130554)
    {
      cout << PHWHERE << "You need to get the old cdev stuff back" << endl;
      exit(1);
    }
  else
    { // run-5
      Event* evt = findNode::getClass<Event>(topNode, "PRDF");
      if (!evt)
        {
          return (0);
        }

      Packet *p = evt->getPacket(PACKETID_CDEV_RUN5);
      if (!p)
        {
          return (0);
        }

      for (int icross = 0; icross < NCROSS; icross++)
        {
          int icross1 = icross * 3;
          fillpat_cdev_b[icross] = (int)p->dValue(icross1, "bkt_intendedFillPatternB");
          fillpat_cdev_y[icross] = (int)p->dValue(icross1, "bkt_intendedFillPatternY");
          spinpat_cdev_b[icross] = (int)p->dValue(icross1, "bkt_polarizationFillPatternB");
          spinpat_cdev_y[icross] = (int)p->dValue(icross1, "bkt_polarizationFillPatternY");
        }
      delete p;
    }

  for (int icross = 0; icross < NCROSS; icross++)
    {
      if (fillpat_cdev_b[icross] == 0)
        {
          spin_fillpat_CDEV_b[0]->Fill(icross);
        }
      else if (fillpat_cdev_b[icross] == 1)
        {
          spin_fillpat_CDEV_b[1]->Fill(icross);
        }
      else
        {
          printf("Error : Unexpected fill pattern (blue).\n");
        }

      if (fillpat_cdev_y[icross] == 0)
        {
          spin_fillpat_CDEV_y[0]->Fill(icross);
        }
      else if (fillpat_cdev_y[icross] == 1)
        {
          spin_fillpat_CDEV_y[1]->Fill(icross);
        }
      else
        {
          printf("Error : Unexpected fill pattern (yellow).\n");
        }

      if (spinpat_cdev_b[icross] == 1)
        {
          spin_spinpat_CDEV_b[0]->Fill(icross);
        }
      else if (spinpat_cdev_b[icross] == -1)
        {
          spin_spinpat_CDEV_b[1]->Fill(icross);
        }
      else if (spinpat_cdev_b[icross] == 0)
        {
          spin_spinpat_CDEV_b[2]->Fill(icross);
        }
      else
        {
          printf("Error : Unexpected spin pattern (blue).\n");
        }

      if (spinpat_cdev_y[icross] == 1)
        {
          spin_spinpat_CDEV_y[0]->Fill(icross);
        }
      else if (spinpat_cdev_y[icross] == -1)
        {
          spin_spinpat_CDEV_y[1]->Fill(icross);
        }
      else if (spinpat_cdev_y[icross] == 0)
        {
          spin_spinpat_CDEV_y[2]->Fill(icross);
        }
      else
        {
          printf("Error : Unexpected spin pattern (yellow).\n");
        }
    }

  return (0);
}

int
QASpin::process_event_special(PHCompositeNode *topNode)
{
  Event* evt = findNode::getClass<Event>(topNode, "PRDF");
  if (!evt)
    {
      return (0);
    }

  TH1D **spin_scaler_sp;
  if (evt->getEvtType() == BEGRUNEVENT)
    {
      spin_scaler_sp = spin_scaler_beg;
    }
  else if (evt->getEvtType() == ENDRUNEVENT)
    {
      spin_scaler_sp = spin_scaler_end;
    }
  else
    {
      return (0);
    }

  for (int iscaler = 0; iscaler < NSCALER; iscaler++)
    {
      for (int icross = 0; icross < NCROSS; icross++)
        {
          int binc = (int)spin_scaler_sp[iscaler]->GetBinContent(icross + 1);
          if (binc != 0)
            {
              printf("Error : BEGRUN or ENDRUN heppens twice (QASpin).\n");
              return (0);
            }
        }
    }

  Packet *p = evt->getPacket(PACKETID_GL1P_SPECIAL);
  if (!p)
    {
      return (0);
    }

  for (int icross = 0; icross < NCROSS; icross++)
    {
      for (int iscaler = 0; iscaler < NSCALER; iscaler++)
        {
          int count = p->iValue(iscaler * NCROSS + icross);
          spin_scaler_sp[iscaler]->SetBinContent(icross + 1, count);
        }
    }
  delete p;
  return (0);
}

void QASpin::DefineHistogram(void)
{
  ostringstream hname;
  ostringstream htitle;
  // check for HistoManager - if it does not exist, create it                                                                           // HistoManagerName is defined in QADefs.h
  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllHistoManager *hm = se->getHistoManager(HistoManagerName);
  if (!hm)
    {
      hm = new Fun4AllHistoManager(HistoManagerName);
      se->registerHistoManager(hm);
    }

  spin_cross = new TH1F("spin_cross", "Crossing ID from GL1", NCROSS, 0, NCROSS);
  hm->registerHisto(spin_cross);
  spin_cross_corr = new TH1F("spin_cross_corr", "Corrected Crossing ID",
                             NCROSS, 0, NCROSS);
  hm->registerHisto(spin_cross_corr);
  for (int ib = 0; ib < N_GL1P_BOARD; ib++)
    {
      hname.str("");
      htitle.str("");
      hname << "spin_cross_gl1p_b" << ib;
      htitle << "Crossing ID from GL1P-" << ib;
      spin_cross_gl1p[ib] = new TH1F(hname.str().c_str(), htitle.str().c_str(), NCROSS, 0, NCROSS);
      ;
      hm->registerHisto(spin_cross_gl1p[ib]);
    }

  for (int i = 0; i < 2; i++)
    {
      hname.str("");
      htitle.str("");
      hname << "spin_fillpat_V124_b" << i;
      htitle << "V124 Fill Pattern " << i << " (blue)";
      spin_fillpat_V124_b[i] = new TH1F(hname.str().c_str(), htitle.str().c_str(), NCROSS, 0, NCROSS);
      hm->registerHisto(spin_fillpat_V124_b[i]);

      hname.str("");
      htitle.str("");
      hname << "spin_fillpat_V124_y" << i;
      htitle << "V124 Fill Pattern " << i << " (yellow)";
      spin_fillpat_V124_y[i] = new TH1F(hname.str().c_str(), htitle.str().c_str(), NCROSS, 0, NCROSS);
      hm->registerHisto(spin_fillpat_V124_y[i]);

      hname.str("");
      htitle.str("");
      hname << "spin_fillpat_CDEV_b" << i;
      htitle << "CDEV Fill Pattern " << i << " (blue)";
      spin_fillpat_CDEV_b[i] = new TH1F(hname.str().c_str(), htitle.str().c_str(), NCROSS, 0, NCROSS);
      hm->registerHisto(spin_fillpat_CDEV_b[i]);

      hname.str("");
      htitle.str("");
      hname << "spin_fillpat_CDEV_y" << i;
      htitle << "CDEV Fill Pattern " << i << " (yellow)";
      spin_fillpat_CDEV_y[i] = new TH1F(hname.str().c_str(), htitle.str().c_str(), NCROSS, 0, NCROSS);
      hm->registerHisto(spin_fillpat_CDEV_y[i]);
    }

  string name1[3] = {"p", "m", "0"};
  for (int ipat = 0; ipat < 3; ipat++)
    {
      hname.str("");
      htitle.str("");
      hname << "spin_spinpat_V124_b" << name1[ipat];
      htitle << "V124 Spin Pattern (blue, " << name1[ipat] << ")";
      spin_spinpat_V124_b[ipat] = new TH1F(hname.str().c_str(), htitle.str().c_str(), NCROSS, 0, NCROSS);
      hm->registerHisto(spin_spinpat_V124_b[ipat]);

      hname.str("");
      htitle.str("");
      hname << "spin_spinpat_V124_y" << name1[ipat];
      htitle << "V124 Spin Pattern (yellow, " << name1[ipat] << ")";
      spin_spinpat_V124_y[ipat] = new TH1F(hname.str().c_str(), htitle.str().c_str(), NCROSS, 0, NCROSS);
      hm->registerHisto(spin_spinpat_V124_y[ipat]);

      hname.str("");
      htitle.str("");
      hname << "spin_spinpat_CDEV_b" << name1[ipat];
      htitle << "CDEV Spin Pattern (blue, " << name1[ipat] << ")";
      spin_spinpat_CDEV_b[ipat] = new TH1F(hname.str().c_str(), htitle.str().c_str(), NCROSS, 0, NCROSS);
      hm->registerHisto(spin_spinpat_CDEV_b[ipat]);

      hname.str("");
      htitle.str("");
      hname << "spin_spinpat_CDEV_y" << name1[ipat];
      htitle << "CDEV Spin Pattern (yellow, " << name1[ipat] << ")";
      spin_spinpat_CDEV_y[ipat] = new TH1F(hname.str().c_str(), htitle.str().c_str(), NCROSS, 0, NCROSS);
      hm->registerHisto(spin_spinpat_CDEV_y[ipat]);
    }

  spin_scaler_event_min = new TH1D("spin_scaler_event_min", "Minimum Event Number",
                                   NCROSS, 0, NCROSS);
  hm->registerHisto(spin_scaler_event_min);

  spin_scaler_event_max = new TH1D("spin_scaler_event_max", "Maximum Event Number",
                                   NCROSS, 0, NCROSS);
  hm->registerHisto(spin_scaler_event_max);

  string scaler_name[NSCALER] = {
    "bbc_vtxcut", "bbc_novtxcut", "zdc_wide", "zdc_narrow"};
  string scaler_title[NSCALER] = {
    "BBC vertex cut", "BBC no vertex cut", "ZDC wide", "ZDC narrow"};

  for (int iscaler = 0; iscaler < NSCALER; iscaler++)
    {

      hname.str("");
      htitle.str("");
      hname << "spin_scaler_" << scaler_name[iscaler];
      htitle << "GL1P Scaler (" << scaler_title[iscaler] << ")";
      spin_scaler[iscaler] = new TH1D(hname.str().c_str(), htitle.str().c_str(), NCROSS, 0, NCROSS);
      hm->registerHisto(spin_scaler[iscaler]);

      hname.str("");
      htitle.str("");
      hname << "spin_scaler_1st_" << scaler_name[iscaler];
      htitle << "GL1P Scaler, 1st event (" << scaler_title[iscaler] << ")";
      spin_scaler_1st[iscaler] = new TH1D(hname.str().c_str(), htitle.str().c_str(), NCROSS, 0, NCROSS);
      hm->registerHisto(spin_scaler_1st[iscaler]);

      hname.str("");
      htitle.str("");
      hname << "spin_scaler_sum_" << scaler_name[iscaler];
      htitle << "GL1P Scaler Sum (" << scaler_title[iscaler] << ")";
      spin_scaler_sum[iscaler] = new TH1D(hname.str().c_str(), htitle.str().c_str(), NCROSS, 0, NCROSS);
      hm->registerHisto(spin_scaler_sum[iscaler]);

      hname.str("");
      htitle.str("");
      hname << "spin_scaler_sum_1st_" << scaler_name[iscaler];
      htitle << "GL1P Scaler Sum, 1st event (" << scaler_title[iscaler] << ")";
      spin_scaler_sum_1st[iscaler] = new TH1D(hname.str().c_str(), htitle.str().c_str(), NCROSS, 0, NCROSS);
      hm->registerHisto(spin_scaler_sum_1st[iscaler]);

      hname.str("");
      htitle.str("");
      hname << "spin_scaler_2mom_" << scaler_name[iscaler];
      htitle << "GL1P Scaler 2nd moment, #Sigma K^{2}N_{K} ("
      << scaler_title[iscaler] << ")";
      spin_scaler_2mom[iscaler] = new TH1D(hname.str().c_str(), htitle.str().c_str(), NCROSS, 0, NCROSS);
      hm->registerHisto(spin_scaler_2mom[iscaler]);

      hname.str("");
      htitle.str("");
      hname << "spin_scaler_beg_" << scaler_name[iscaler];
      htitle << "GL1P Scaler, BEGRUNEVENT (" << scaler_title[iscaler] << ")";
      spin_scaler_beg[iscaler] = new TH1D(hname.str().c_str(), htitle.str().c_str(), NCROSS, 0, NCROSS);
      hm->registerHisto(spin_scaler_beg[iscaler]);

      hname.str("");
      htitle.str("");
      hname << "spin_scaler_end_" << scaler_name[iscaler];
      htitle << "GL1P Scaler, ENDRUNEVENT (" << scaler_title[iscaler] << ")";
      spin_scaler_end[iscaler] = new TH1D(hname.str().c_str(), htitle.str().c_str(), NCROSS, 0, NCROSS);
      hm->registerHisto(spin_scaler_end[iscaler]);
    }

  for (int icross = 0; icross < NCROSS; icross++)
    {
      spin_scaler_event_min->SetBinContent(icross + 1, 1.0e+9);
    }

  return ;
};
///////////////////////////////////////////////////////////////////
