#include "MisalignSearchHist.h"
#include "SvxQADefs.h"

#include <phool.h>
#include <PHCompositeNode.h>
#include <Fun4AllServer.h>
#include <Fun4AllHistoManager.h>
#include <Fun4AllReturnCodes.h>
#include <getClass.h>
#include <TrigLvl1.h>
#include <TriggerHelper.h>

#include <EventHeader.h>
#include <RunHeader.h>
#include <PreviousEvent.h>
#include <SvxRawhitList.h>
#include <SvxRawhit.h>
#include <SvxClusterList.h>
#include <SvxCluster.h>
#include <svxAddress.hh>
#include <VtxOut.h>
#include <PHPoint.h>
#include <SvxStripThreshold.h>
#include <BbcOut.h>
#include <Bbc.hh>
#include <SvxQAEventSelection.h>

#include <TFile.h>
#include <TH1.h>
#include <TH2.h>
#include <TProfile.h>
#include <TTree.h>
#include <TBenchmark.h>
#include <TSQLServer.h>
#include <TSQLStatement.h>

#include <iostream>
#include <cmath>
#include <gsl/gsl_rng.h>

using namespace std;
using namespace findNode;

MisalignSearchHist::MisalignSearchHist():
  SubsysReco("MISALIGNSEARCHHIST"),
  d_runheader(NULL),
  d_eventhead(NULL),
  d_svxraw(NULL),
	d_bbc(NULL),
	trigname("BBCLL1(>0 tubes) narrowvtx"),
	clockname("CLOCK"),
  m_EventNumber(0),
  m_EventSeqNumber(0),
  m_runnumber(0),
  is_fixrange(false),
  MAXEVENTS(0),
  binwidth(0),
	is_bbcq10percent(false)
{
  memset(B0_chip_profile_all,0,sizeof(B0_chip_profile_all));
  memset(B0_chip_profile_clk,0,sizeof(B0_chip_profile_clk));
  memset(B0_chip_profile_otr,0,sizeof(B0_chip_profile_otr));
  memset(B1_chip_profile_all,0,sizeof(B1_chip_profile_all));
  memset(B1_chip_profile_clk,0,sizeof(B1_chip_profile_clk));
  memset(B1_chip_profile_otr,0,sizeof(B1_chip_profile_otr));
}


int MisalignSearchHist::Init(PHCompositeNode *topNode)
{

  return EVENT_OK;
}

//==============================================================
int MisalignSearchHist::InitRun(PHCompositeNode *topNode)
{

  d_runheader = findNode::getClass<RunHeader>(topNode, "RunHeader");
  if (d_runheader == NULL)
  {
    if (verbosity > 0)
      cout << PHWHERE << "Can't find RunHeader. " << endl;
    return ABORTRUN;
  }

  //set the bin limits for the TProfiles
  m_runnumber = d_runheader->get_RunNumber();
  Set_maxevents(m_runnumber);


	//Initialize nevent histos using the bin limits set above.
  h_nevents_all = new TH1I("h_nevents_all","Number of events all triggers",
			nbinevtseq, 0, nbinevtseq * binwidth);
  h_nevents_clk = new TH1I("h_nevents_clk","Number of events clock trigger",
			nbinevtseq, 0, nbinevtseq * binwidth);
  h_nevents_otr = new TH1I("h_nevents_otr","Number of events other trigger",
			nbinevtseq, 0, nbinevtseq * binwidth);

  //Initialize the chip level TProfiles using the bin limits set above.
  //must come after Set_maxevents()
  Init_TProfiles();


  return EVENT_OK;
}
//==============================================================
int MisalignSearchHist::process_event(PHCompositeNode *topNode)
{

  GetNodes(topNode);

  if (m_EventNumber % 1000 == 0 && verbosity > 0)
    cout << "MisalignSearchHist::process_event() event no: " << m_EventNumber << endl;
  m_EventNumber++;

  //trigger selection
  TriggerHelper trghelp(topNode);
  bool isclock = trghelp.didLevel1TriggerGetScaled(clockname.c_str()); 
  bool isother = trghelp.didLevel1TriggerGetScaled(trigname.c_str());


	// Multiplicity cut 
	// --Select events in the top 10% bbc multiplicity
	bool is_central = true;
	if (is_bbcq10percent)
	{
		float bbc_q = d_bbc->get_ChargeSum(Bbc::North) + d_bbc->get_ChargeSum(Bbc::South);
		if (bbc_q < 1200)
      is_central = false;
	}

	// Fill nevent histos
  m_EventSeqNumber = d_eventhead->get_EvtSequence();
	h_nevents_all->Fill(m_EventSeqNumber);
	if(isclock)
		h_nevents_clk->Fill(m_EventSeqNumber);
	if(isother && !is_bbcq10percent)
		h_nevents_otr->Fill(m_EventSeqNumber);
	else if(is_bbcq10percent && is_central)
		h_nevents_otr->Fill(m_EventSeqNumber);


  //before we start getting the raw hit information, need to make some arrays 
	//to hold the number of pixels which fire in each chip
  int Npixelhit_chipB0_all[NCHIP_B0] = {0};
  int Npixelhit_chipB0_clk[NCHIP_B0] = {0};
  int Npixelhit_chipB0_otr[NCHIP_B0] = {0};
  int Npixelhit_chipB1_all[NCHIP_B1] = {0};
  int Npixelhit_chipB1_clk[NCHIP_B1] = {0};
  int Npixelhit_chipB1_otr[NCHIP_B1] = {0};

  //raw hit info
  int nrawhit = (d_svxraw != NULL) ? d_svxraw->get_nRawhits() : 0;

  for (int ihit = 0; ihit < nrawhit; ihit++)
  {
    SvxRawhit *rhit = d_svxraw->get_Rawhit(ihit);
    int r_section = rhit->get_svxSection();
    int r_layer   = rhit->get_layer();
    int r_ladder  = rhit->get_ladder();
    int r_sensor  = rhit->get_sensor();

    if(r_layer > 1) continue;

    int chip_no; //chip number

    if (r_section == 0) // pixel or stripixel
    {
      int pixelRoc = rhit->get_pixelROC();
      int sensor_chip = pixelRoc;
      if (r_sensor == 1 || r_sensor == 3)
          sensor_chip -= 4;

      if (verbosity > 1)
      {
        cout << "eventnum" << m_EventNumber << endl;
        cout << "layer" << r_layer << endl;
        cout << "ladder" << r_ladder << endl;
        cout << "sensor" << r_sensor << endl;
        cout << "ROC   " << pixelRoc << endl;
        //cout << "status" << status << endl;
      }

      chip_no  = r_ladder * 4 * 4 + r_sensor * 4 + sensor_chip;
      
			if (r_layer == 0)
        Npixelhit_chipB0_all[chip_no]++;
			else if (r_layer == 1)
        Npixelhit_chipB1_all[chip_no]++;

			if(isclock)
			{
				if (r_layer == 0)
        	Npixelhit_chipB0_clk[chip_no]++;
				else if (r_layer == 1)
        	Npixelhit_chipB1_clk[chip_no]++;
			}
			if(isother && !is_bbcq10percent)
			{
				if (r_layer == 0)
        	Npixelhit_chipB0_otr[chip_no]++;
				else if (r_layer == 1)
        	Npixelhit_chipB1_otr[chip_no]++;
			}
			else if(is_bbcq10percent && is_central)
			{
				if (r_layer == 0)
        	Npixelhit_chipB0_otr[chip_no]++;
				else if (r_layer == 1)
        	Npixelhit_chipB1_otr[chip_no]++;
			}
    }
  }

  //now fill the chip level tprofiles for this event
  for (int ichip = 0; ichip < NCHIP_B0; ichip++)
	{
		B0_chip_profile_all[ichip]->Fill(m_EventSeqNumber, Npixelhit_chipB0_all[ichip]);
		if(isclock)
			B0_chip_profile_clk[ichip]->Fill(m_EventSeqNumber, Npixelhit_chipB0_clk[ichip]);
		if(isother && !is_bbcq10percent)
			B0_chip_profile_otr[ichip]->Fill(m_EventSeqNumber, Npixelhit_chipB0_otr[ichip]);
		else if(is_bbcq10percent && is_central)
			B0_chip_profile_otr[ichip]->Fill(m_EventSeqNumber, Npixelhit_chipB0_otr[ichip]);
	}

  for (int ichip = 0; ichip < NCHIP_B1; ichip++)
	{
		B1_chip_profile_all[ichip]->Fill(m_EventSeqNumber, Npixelhit_chipB1_all[ichip]);
		if(isclock)
			B1_chip_profile_clk[ichip]->Fill(m_EventSeqNumber, Npixelhit_chipB1_clk[ichip]);
		if(isother && !is_bbcq10percent)
			B1_chip_profile_otr[ichip]->Fill(m_EventSeqNumber, Npixelhit_chipB1_otr[ichip]);
		else if(is_bbcq10percent && is_central)
			B1_chip_profile_otr[ichip]->Fill(m_EventSeqNumber, Npixelhit_chipB1_otr[ichip]);
	}


  return EVENT_OK;
}
//==============================================================

//----Classes needed----------------------------------------

int MisalignSearchHist::GetNodes(PHCompositeNode *topNode)
{

    //-------------------EventHeader----------------------------------
    d_eventhead = findNode::getClass<EventHeader>(topNode, "EventHeader");
    if (d_eventhead == NULL)
    {
        cerr << PHWHERE << " EventHeader node not found." << endl;
        return DISCARDEVENT;
    }
    //------------------------------------------------------------------

    //----------------SvxRawhitList-------------------------
    d_svxraw = findNode::getClass<SvxRawhitList>(topNode, "SvxRawhitList");
    if (d_svxraw == NULL)
    {
        cerr << "SvxRawhit node not found." << endl;
        return DISCARDEVENT;
    }
    //------------------------------------------------------

    //-------------------BbcOut----------------------------------------
    d_bbc = NULL;
    d_bbc = findNode::getClass<BbcOut>(topNode, "BbcOut");
    if (d_bbc == NULL && is_bbcq10percent)
    {
        cerr << "BbcOut node not found." << std::endl;
        return DISCARDEVENT;
    }
    //------------------------------------------------------------------


    return 0;
}

//==============================================================

int MisalignSearchHist::End(PHCompositeNode *topNode)
{

  TFile *file = new TFile(outname.c_str(), "RECREATE");
  if (verbosity > 0)
      cout << "MisalignSearchHist::End() - writing TProfiles to file" << endl;

  //--------------------------------------//
  //----- write chip TProfiles -----------//
  //--------------------------------------//

  for (int ichip = 0; ichip < NCHIP_B0; ichip++)
	{
		B0_chip_profile_all[ichip]->Write();
		B0_chip_profile_clk[ichip]->Write();
		B0_chip_profile_otr[ichip]->Write();
	}

  for (int ichip = 0; ichip < NCHIP_B1; ichip++)
	{
		B1_chip_profile_all[ichip]->Write();
		B1_chip_profile_clk[ichip]->Write();
		B1_chip_profile_otr[ichip]->Write();
	}

	h_nevents_all->Write();
	h_nevents_clk->Write();
	h_nevents_otr->Write();

  file->Close();
  return 0;
}


//==============================================================

void MisalignSearchHist::Set_maxevents(const int runnumber)
{
  //This method sets the range of the TProfiles

  int events_in_run = -1;
  //If the is_fixrange flag is set, the range of the TProfiles is set to a fixed value,
  //regardless of the number of events in the range.
  if (is_fixrange)
  {
    MAXEVENTS = 12500000; //default of 12.5M set for Run 14 AuAu 15GeV
  }
  //otherwise, set the range of the TProfiles based on the number of events in the run
  else
  {

    //first get the number of events in this run from the daq database
    char cmd[200];
    sprintf(cmd, "select eventsinrun from run where runnumber=%d", runnumber);
    TSQLServer *db = TSQLServer::Connect("odbcn://daq", "phnxrc", "");
    if (verbosity > 0)
			cout << "MisalignSearchHist::Set_maxevents() - DB: " << db->ServerInfo() << endl;
    TSQLStatement *stmt = db->Statement(cmd, 1);
    if (stmt->Process())
    {
			stmt->StoreResult();
			while (stmt->NextResultRow())
			{
				events_in_run = stmt->GetInt(0);
			}
		}
		delete db;


    if (events_in_run < 0)
			events_in_run = 50000000; //set default to 50M if we can't get the number from the database

		// Make the upper bound equal to n_events rounded up to the nearest 100,000
		MAXEVENTS  = 100000 * ( int(events_in_run/100000) + 1 );
	}
	// Binning is 2 bins per 100,000 i.e. binwidth is 50000
	nbinevtseq = 2*(MAXEVENTS/100000);
	binwidth = MAXEVENTS / nbinevtseq;

	if (verbosity > 0)
	{
  	cout << "MisalignSearchHist::Set_maxevents() - runnumber=" << runnumber << " events in this run=" << events_in_run << endl;
  	cout << "MisalignSearchHist::Set_maxevents() - set MAXEVENTS=" << MAXEVENTS << " and binwidth=" << binwidth << endl;
  }

  return;

}

//==============================================================
void MisalignSearchHist::Init_TProfiles()
{

    //Initialize Chip level TProfiles for B0
    for (int ichip = 0; ichip < NCHIP_B0; ichip++)
    {
        int ladder = ichip / 16;
        int sensor = (ichip / 4) % 4;
        int roc = ichip % 4;

        B0_chip_profile_all[ichip] = new TProfile(
						Form("prof_all_chiprawhit_B0_ladder%d_sensor%d_chip%d", ladder, sensor, roc),
						Form("B0 ladder%d sensor%d chip%d", ladder, sensor, roc),
            nbinevtseq, 0, nbinevtseq * binwidth);
        B0_chip_profile_clk[ichip] = new TProfile(
						Form("prof_clk_chiprawhit_B0_ladder%d_sensor%d_chip%d", ladder, sensor, roc),
						Form("B0 ladder%d sensor%d chip%d", ladder, sensor, roc),
            nbinevtseq, 0, nbinevtseq * binwidth);
        B0_chip_profile_otr[ichip] = new TProfile(
						Form("prof_otr_chiprawhit_B0_ladder%d_sensor%d_chip%d", ladder, sensor, roc),
						Form("B0 ladder%d sensor%d chip%d", ladder, sensor, roc),
            nbinevtseq, 0, nbinevtseq * binwidth);
    }


    //Initialize Chip level TProfiles for B1
    for (int ichip = 0; ichip < NCHIP_B1; ichip++)
    {
        int ladder = ichip / 16;
        int sensor = (ichip / 4) % 4;
        int roc = ichip % 4;

        B1_chip_profile_all[ichip] = new TProfile(
						Form("prof_all_chiprawhit_B1_ladder%d_sensor%d_chip%d", ladder, sensor, roc),
						Form("B1 ladder%d sensor%d chip%d", ladder, sensor, roc),
						nbinevtseq, 0, nbinevtseq * binwidth);
        B1_chip_profile_clk[ichip] = new TProfile(
						Form("prof_clk_chiprawhit_B1_ladder%d_sensor%d_chip%d", ladder, sensor, roc),
						Form("B1 ladder%d sensor%d chip%d", ladder, sensor, roc),
						nbinevtseq, 0, nbinevtseq * binwidth);
        B1_chip_profile_otr[ichip] = new TProfile(
						Form("prof_otr_chiprawhit_B1_ladder%d_sensor%d_chip%d", ladder, sensor, roc),
						Form("B1 ladder%d sensor%d chip%d", ladder, sensor, roc),
						nbinevtseq, 0, nbinevtseq * binwidth);
    }


    if (verbosity > 0)
    {
        cout << "MisalignSearchHist::Init_chipTProfiles() - Successfully initialized chip level TProfiles." << endl;
    }

}
