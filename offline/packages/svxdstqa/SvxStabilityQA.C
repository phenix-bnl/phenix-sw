#include "SvxStabilityQA.h"
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
#include <vector>

using namespace std;
using namespace findNode;

SvxStabilityQA::SvxStabilityQA():
  SubsysReco("SVXSTABILITYQA"),
  d_runheader(NULL),
  d_svxstripthreshold(NULL),
  d_eventhead(NULL),
  d_svxadr(NULL),
  d_svxraw(NULL),
  d_svxcls(NULL),
  d_eventselection(NULL),
  m_EventNumber(0),
  m_EventSeqNumber(0),
  is_tickcut(true),
  m_bbczcut(10), //bbcz cut
  m_runnumber(0),
  is_fixrange(false),
  is_bbcq10percent(false),
  run_pixels(true),
  MAXEVENTS(0),
  binwidth(0)
{
  memset(nevents,0,sizeof(nevents));
  memset(B0_chips_hit,0,sizeof(B0_chips_hit));
  memset(B1_chips_hit,0,sizeof(B1_chips_hit));
  memset(B2_chips_hit,0,sizeof(B2_chips_hit));
  memset(B3_chips_hit,0,sizeof(B3_chips_hit));
  memset(B0_chip_profile,0,sizeof(B0_chip_profile));
  memset(B1_chip_profile,0,sizeof(B1_chip_profile));
  memset(B2_chip_profile,0,sizeof(B2_chip_profile));
  memset(B3_chip_profile,0,sizeof(B3_chip_profile));
  memset(B2_strip_adc_profile,0,sizeof(B2_strip_adc_profile));
  memset(B3_strip_adc_profile,0,sizeof(B3_strip_adc_profile));
}


int SvxStabilityQA::Init(PHCompositeNode *topNode)
{
    Fun4AllServer *se = Fun4AllServer::instance();
    Fun4AllHistoManager *hm = se->getHistoManager(HistoManagerName);
    if (!hm)
    {
        hm = new Fun4AllHistoManager(HistoManagerName);
        se->registerHistoManager(hm);
    }

    // initialize event selection handler
    d_eventselection = new SvxQAEventSelection();
    d_eventselection->Set_BBCZCut(m_bbczcut);
    d_eventselection->Set_TickCut(is_tickcut);
    d_eventselection->Set_BbcQ10Percent(is_bbcq10percent);
    d_eventselection->Set_Verbosity(verbosity);


    memset(nevents, 0, sizeof(nevents));
    memset(B0_chips_hit, 0, sizeof(B0_chips_hit));
    memset(B1_chips_hit, 0, sizeof(B1_chips_hit));
    memset(B2_chips_hit, 0, sizeof(B2_chips_hit));
    memset(B3_chips_hit, 0, sizeof(B3_chips_hit));


    return EVENT_OK;
}

//==============================================================
int SvxStabilityQA::InitRun(PHCompositeNode *topNode)
{

    // Strip Threshold
    d_svxstripthreshold = findNode::getClass<SvxStripThreshold>(topNode, "SvxStripThreshold");
    if ( d_svxstripthreshold == NULL)
    {
        if (verbosity > 0)
        {
            cout << PHWHERE << "Can't find SvxStripThreshold. " << endl;
        }
        return ABORTRUN;
    }

    d_runheader = findNode::getClass<RunHeader>(topNode, "RunHeader");
    if (d_runheader == NULL)
    {
        if (verbosity > 0)
        {
            cout << PHWHERE << "Can't find RunHeader. " << endl;
        }
        return ABORTRUN;
    }


    //set the bin limits for the TProfiles
    m_runnumber = d_runheader->get_RunNumber();
    Set_maxevents(m_runnumber);

    //Initialize the chip level TProfiles using the bin limits set above.
    //must come after Set_maxevents()
    Init_TProfiles();


    return EVENT_OK;
}
//==============================================================
int SvxStabilityQA::process_event(PHCompositeNode *topNode)
{

    GetNodes(topNode);

    if (m_EventNumber % 1000 == 0 && verbosity > 0)
    {
        cout << "SvxStabilityQA::process_event() event no: " << m_EventNumber << endl;
    }
    m_EventNumber++;

    //this handles the trigger & event selection
    if (!d_eventselection->EventSelection(topNode)) return EVENT_OK;


    m_EventSeqNumber = d_eventhead->get_EvtSequence();
    int ieventbin = floor(m_EventSeqNumber / binwidth);
    nevents[ieventbin]++;

    //before we start getting the raw hit information, need to make some arrays to hold
    //the number of pixels which fire in each chip
    int Npixelhit_chipB0[NCHIP_B0] = {0};
    int Npixelhit_chipB1[NCHIP_B1] = {0};
    int Npixelhit_chipB2[NCHIP_B2] = {0};
    int Npixelhit_chipB3[NCHIP_B3] = {0};

    //raw hit info
    int nrawhit = (d_svxraw != NULL) ? d_svxraw->get_nRawhits() : 0;

    for (int ihit = 0; ihit < nrawhit; ihit++)
    {
        SvxRawhit *rhit = d_svxraw->get_Rawhit(ihit);
        int r_section = rhit->get_svxSection();
        int r_layer   = rhit->get_layer();
        int r_ladder  = rhit->get_ladder();
        int r_sensor  = rhit->get_sensor();

        if(!run_pixels && r_layer < 2) continue;

        int chip_no; //chip number

        if (r_section == 0) // pixel or stripixel
        {
            if (r_layer < 2) // pixel
            {
                int pixelRoc = rhit->get_pixelROC();
                int channel = rhit->get_channel();
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
                {
                    B0_chips_hit[chip_no][ieventbin][channel]++;
                    Npixelhit_chipB0[chip_no]++;
                }
                if (r_layer == 1)
                {
                    B1_chips_hit[chip_no][ieventbin][channel]++;
                    Npixelhit_chipB1[chip_no]++;
                }

            }
            else //stripixel
            {
                int r_SS = rhit->get_sensorSection();
                int r_readout = rhit->get_sensorReadout();
                int r_channel = rhit->get_channel();
                int stripRoc = d_svxadr->getStripRoc(r_SS, r_readout, r_channel); //get the chip number in this sensor (0-11)
                int stripRocChannel = d_svxadr->getStripRocChannel(r_SS, r_readout, r_channel); //get the channel number in this chip (0-127)

                float adc = rhit->get_adc();

                //if (adc<10) continue;

                if (adc < 10 || adc > 180) continue;

                //int th = _StripThresholds->getThreshold(layer-2,ladder,sensor,roc,rocchnl) - 24;
                int th = d_svxstripthreshold->getThreshold(r_layer - 2, r_ladder, r_sensor, stripRoc, stripRocChannel) - 24;
                if (adc < th) continue;



                if (verbosity > 1)
                {
                    cout << "layer" << r_layer << endl;
                    cout << "ladder" << r_ladder << endl;
                    cout << "sensor" << r_ladder << endl;
                    cout << "section " << r_SS << endl;
                    cout << "readout" << r_readout << endl;
                    cout << "channel" << r_channel << endl;
                    cout << "stripRoc" << stripRoc << endl;
                    cout << "stripRocChannel" << stripRocChannel << endl;
                    cout << "adc" << adc << endl;
                }

                if (r_layer == 2)
                {
                    int chip_no = r_ladder * 5 * 12 + r_sensor * 12 + stripRoc;
                    B2_strip_adc_profile[chip_no][stripRocChannel]->Fill(m_EventSeqNumber, adc - th);
                    //B2_strip_adc_profile[chip_no][stripRocChannel]->Fill(m_EventSeqNumber, adc);
                    B2_chips_hit[chip_no][ieventbin][stripRocChannel]++;
                    Npixelhit_chipB2[chip_no]++;
                }
                if (r_layer == 3)
                {
                    int chip_no = r_ladder * 6 * 12 + r_sensor * 12 + stripRoc;
                    B3_strip_adc_profile[chip_no][stripRocChannel]->Fill(m_EventSeqNumber, adc - th);
                    //B3_strip_adc_profile[chip_no][stripRocChannel]->Fill(m_EventSeqNumber, adc);
                    B3_chips_hit[chip_no][ieventbin][stripRocChannel]++;
                    Npixelhit_chipB3[chip_no]++;
                }


            }
        }
    }

if(run_pixels)
{
    //now fill the chip level tprofiles for this event
    for (int ichip = 0; ichip < NCHIP_B0; ichip++)
        B0_chip_profile[ichip]->Fill(m_EventSeqNumber, Npixelhit_chipB0[ichip]);

    for (int ichip = 0; ichip < NCHIP_B1; ichip++)
        B1_chip_profile[ichip]->Fill(m_EventSeqNumber, Npixelhit_chipB1[ichip]);
}
    for (int ichip = 0; ichip < NCHIP_B2; ichip++)
        B2_chip_profile[ichip]->Fill(m_EventSeqNumber, Npixelhit_chipB2[ichip]);

    for (int ichip = 0; ichip < NCHIP_B3; ichip++)
        B3_chip_profile[ichip]->Fill(m_EventSeqNumber, Npixelhit_chipB3[ichip]);



    return EVENT_OK;
}
//==============================================================

//----Classes needed----------------------------------------

int SvxStabilityQA::GetNodes(PHCompositeNode *topNode)
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

    //----------------SvxClusterList-------------------------
    d_svxcls = findNode::getClass<SvxClusterList>(topNode, "SvxClusterList");
    if (d_svxcls == NULL)
    {
        cerr << "SvxClusterList node not found.Register SvxReco module" << endl;
        return DISCARDEVENT;
    }
    //------------------------------------------------------

    //----------------svxAddress------------------------------
    d_svxadr = findNode::getClass<svxAddress>(topNode, "svxAddress");
    if (d_svxadr == NULL)
    {
        cerr << "svxAddress node not found." << endl;
        return DISCARDEVENT;
    }



    return 0;
}

//==============================================================

int SvxStabilityQA::End(PHCompositeNode *topNode)
{

  TFile *file = new TFile(outname.c_str(), "RECREATE");
    if (verbosity > 0)
        cout << "SvxStabilityQa::End() - writing TProfiles to file" << endl;

    TTree *profiletree = new TTree("profiletree", "Tree of profiles for each pixel or channel in the VTX");
    TProfile *tmpprofile = 0;
    profiletree->Branch("pixel", "TProfile", &tmpprofile);
    profiletree->SetAutoFlush(1000);

    //first fill all the pixel/channel level hit vs evt sequence TProfiles
    //then all the chip level hit vs evt sequence TProfiles
    //last all the channel level <adc> vs evt sequence TProfiles


    //--------------------------------------//
    //--- Pixel / channel based profiles ---//
    //--------------------------------------//
    //for testing purposes
    gBenchmark->Start("filltimer");


    //PIXEL LAYER 1 (B0)
    for (int ichip = 0; ichip < NCHIP_B0; ichip++)
    {
        if (ichip % 10 == 0 && verbosity > 0)
        {
            cout << "SvxStabilityQA::End() - filling TProfiles for chip " << ichip << " in B0" << endl;
        }

        int ladder = ichip / 16;
        int sensor = (ichip / 4) % 4;
        int roc = ichip % 4;

        for (int ipix = 0; ipix < NPIXEL; ipix++)
        {

            tmpprofile = new TProfile(Form("prof_pixelrawhit_B0_ladder%d_sensor%d_roc%d_pixel%d", ladder, sensor, roc, ipix),
                                      Form("B0 ladder%d sensor%d chip%d pixel%d", ladder, sensor, roc, ipix),
                                      nbinevtseq, 0, nbinevtseq * binwidth);
            if(run_pixels)
            {
                for (int ibin = 0; ibin < nbinevtseq; ibin++)
                {
                    int total_pixel_hits = B0_chips_hit[ichip][ibin][ipix];

                    //fill all the hits
                    if (total_pixel_hits > 0)
                        tmpprofile->Fill(tmpprofile->GetBinCenter(ibin + 1), 1, total_pixel_hits); //fill the pixel level TProfile

                    //now fill for all the events with no hits
                    tmpprofile->Fill(tmpprofile->GetBinCenter(ibin + 1), 0, nevents[ibin] - total_pixel_hits); //fill the pixel level TProfile

                }
            }
            profiletree->Fill();
            delete tmpprofile;
        }
    }

    if (verbosity > 0)
    {
        cout << "SvxStabilityQA::End() - Done filling pixel TProfiles for B0" << endl;
    }


    //PIXEL LAYER 2 (B1)
    for (int ichip = 0; ichip < NCHIP_B1; ichip++)
    {
        if (ichip % 10 == 0 && verbosity > 0)
        {
            cout << "SvxStabilityQA::End() - filling TProfiles for chip " << ichip << " in B1" << endl;
        }

        int ladder = ichip / 16;
        int sensor = (ichip / 4) % 4;
        int roc = ichip % 4;

        for (int ipix = 0; ipix < NPIXEL; ipix++)
        {

            tmpprofile = new TProfile(Form("prof_pixelrawhit_B1_ladder%d_sensor%d_roc%d_pixel%d", ladder, sensor, roc, ipix),
                                      Form("B1 ladder%d sensor%d chip%d pixel%d", ladder, sensor, roc, ipix),
                                      nbinevtseq, 0, nbinevtseq * binwidth);

            if(run_pixels)
            {
                for (int ibin = 0; ibin < nbinevtseq; ibin++)
                {
                    int total_pixel_hits = B1_chips_hit[ichip][ibin][ipix];

                    //fill all the hits
                    if (total_pixel_hits > 0)
                        tmpprofile->Fill(tmpprofile->GetBinCenter(ibin + 1), 1, total_pixel_hits); //fill the pixel level TProfile

                    //now fill for all the events with no hits
                    tmpprofile->Fill(tmpprofile->GetBinCenter(ibin + 1), 0, nevents[ibin] - total_pixel_hits); //fill the pixel level TProfile
                }
            }
            profiletree->Fill();
            delete tmpprofile;
        }
    }

    if (verbosity > 0)
    {
        cout << "SvxStabilityQA::End() - Done filling pixel TProfiles for B1" << endl;
    }

    //STRIPPIXEL LAYER 1 (B2)
    for (int ichip = 0; ichip < NCHIP_B2; ichip++)
    {
        if (ichip % 100 == 0 && verbosity > 0)
        {
            cout << "SvxStabilityQA::End() - filling channel TProfiles for chip " << ichip << " in B2" << endl;
        }

        int ladder = ichip / (5 * 12);
        int sensor = (ichip / 12) % 5;
        int roc = ichip % 12;

        for (int ipix = 0; ipix < NSTRIPCHANNEL; ipix++)
        {

            tmpprofile = new TProfile(Form("prof_channelrawhit_B2_ladder%d_sensor%d_roc%d_pixel%d", ladder, sensor, roc, ipix),
                                      Form("B2 ladder%d sensor%d chip%d pixel%d", ladder, sensor, roc, ipix),
                                      nbinevtseq, 0, nbinevtseq * binwidth);

            for (int ibin = 0; ibin < nbinevtseq; ibin++)
            {
                int total_pixel_hits = B2_chips_hit[ichip][ibin][ipix];

                //fill all the hits
                if (total_pixel_hits > 0)
                    tmpprofile->Fill(tmpprofile->GetBinCenter(ibin + 1), 1, total_pixel_hits); //fill the pixel level TProfile

                //now fill for all the events with no hits
                tmpprofile->Fill(tmpprofile->GetBinCenter(ibin + 1), 0, nevents[ibin] - total_pixel_hits); //fill the pixel level TProfile
            }
            profiletree->Fill();
            delete tmpprofile;
        }
    }

    if (verbosity > 0)
    {
        cout << "SvxStabilityQA::End() - Done filling channel TProfiles for B2" << endl;
    }



    //STRIPPIXEL LAYER 2 (B3)
    for (int ichip = 0; ichip < NCHIP_B3; ichip++)
    {
        if (ichip % 100 == 0 && verbosity > 0)
        {
            cout << "SvxStabilityQA::End() - filling TProfiles for chip " << ichip << " in B3" << endl;
        }

        int ladder = ichip / (6 * 12);
        int sensor = (ichip / 12) % 6;
        int roc = ichip % 12;

        for (int ipix = 0; ipix < NSTRIPCHANNEL; ipix++)
        {

            tmpprofile = new TProfile(Form("prof_channelrawhit_B3_ladder%d_sensor%d_roc%d_pixel%d", ladder, sensor, roc, ipix),
                                      Form("B3 ladder%d sensor%d chip%d pixel%d", ladder, sensor, roc, ipix),
                                      nbinevtseq, 0, nbinevtseq * binwidth);

            for (int ibin = 0; ibin < nbinevtseq; ibin++)
            {
                int total_pixel_hits = B3_chips_hit[ichip][ibin][ipix];

                //fill all the hits
                if (total_pixel_hits > 0)
                    tmpprofile->Fill(tmpprofile->GetBinCenter(ibin + 1), 1, total_pixel_hits); //fill the pixel level TProfile

                //now fill for all the events with no hits
                tmpprofile->Fill(tmpprofile->GetBinCenter(ibin + 1), 0, nevents[ibin] - total_pixel_hits); //fill the pixel level TProfile
            }
            profiletree->Fill();
            delete tmpprofile;
        }
    }

    if (verbosity > 0)
    {
        cout << "SvxStabilityQA::End() - Done filling channel TProfiles for B3" << endl;
    }


    //--------------------------------------//
    //----- write chip TProfiles -----------//
    //--------------------------------------//

    for (int ichip = 0; ichip < NCHIP_B0; ichip++)
    {
        tmpprofile = B0_chip_profile[ichip];
        profiletree->Fill();
    }

    for (int ichip = 0; ichip < NCHIP_B1; ichip++)
    {
        tmpprofile = B1_chip_profile[ichip];
        profiletree->Fill();
    }

    for (int ichip = 0; ichip < NCHIP_B2; ichip++)
    {
        tmpprofile = B2_chip_profile[ichip];
        profiletree->Fill();
    }

    for (int ichip = 0; ichip < NCHIP_B3; ichip++)
    {
        tmpprofile = B3_chip_profile[ichip];
        profiletree->Fill();
    }

    if (verbosity > 0)
    {
        cout << "SvxStabilityQA::End() - Done filling chip TProfiles" << endl;
    }


    //--------------------------------------//
    //---- <adc> vs evt. seq. TProfiles ----//
    //--------------------------------------//

    for (int ichip = 0; ichip < NCHIP_B2; ichip++)
        for (int ichannel = 0; ichannel < NSTRIPCHANNEL; ichannel++)
        {
            tmpprofile = B2_strip_adc_profile[ichip][ichannel];
            profiletree->Fill();
        }

    for (int ichip = 0; ichip < NCHIP_B3; ichip++)
        for (int ichannel = 0; ichannel < NSTRIPCHANNEL; ichannel++)
        {
            tmpprofile = B3_strip_adc_profile[ichip][ichannel];
            profiletree->Fill();
        }

    if (verbosity > 0)
    {
        cout << "SvxStabilityQA::End() - Done filling adc TProfiles" << endl;
    }



    //--------------------------------------//
    //----- write the tree to file ---------//
    //--------------------------------------//
    if (verbosity > 0)
    {
        cout << "SvxStabilityQA::End() - Writing TTree of TProfiles to file" << endl;
    }
    profiletree->Write();

    gBenchmark->Show("filltimer");

    file->Close();
    return 0;
}


//==============================================================

void SvxStabilityQA::Set_maxevents(const int runnumber)
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
	cout << "SvxStabilityQA::Set_maxevents() - DB: " << db->ServerInfo() << endl;
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

      //We want to bin the TProfiles so that rebin can be used to get the same binning for different number of events
      //(i.e. we want the bin widths to be integer multiples of eachother)
      if (events_in_run <= 3125000 )
        {
	  MAXEVENTS = 3125000;
        }
      if (events_in_run > 3125000 && events_in_run <= 6250000)
        {
	  MAXEVENTS = 6250000;
        }
      if (events_in_run > 6250000 && events_in_run <= 12500000)
        {
	  MAXEVENTS = 12500000;
        }
      if (events_in_run > 12500000 && events_in_run <= 25000000)
        {
	  MAXEVENTS = 25000000;
        }
      if (events_in_run > 25000000 && events_in_run <= 50000000)
        {
	  MAXEVENTS = 50000000;
        }
      if (events_in_run > 50000000)
        {
	  MAXEVENTS = 100000000;
        }
    }

  //set the bin width, which depends on the total number of events in the run, and the number of available bins
  binwidth = MAXEVENTS / nbinevtseq;

  if (verbosity > 0)
    {
      cout << "SvxStabilityQA::Set_maxevents() - runnumber=" << runnumber << " events in this run=" << events_in_run << endl;
      cout << "SvxStabilityQA::Set_maxevents() - set MAXEVENTS=" << MAXEVENTS << " and binwidth=" << binwidth << endl;
    }


  return;

}

//==============================================================
void SvxStabilityQA::Init_TProfiles()
{

    //Initialize Chip level TProfiles for B0
    for (int ichip = 0; ichip < NCHIP_B0; ichip++)
    {
        int ladder = ichip / 16;
        int sensor = (ichip / 4) % 4;
        int roc = ichip % 4;

        //make a temporary TProfile to hold the chip level raw hits
        B0_chip_profile[ichip] = new TProfile(Form("prof_chiprawhit_B0_ladder%d_sensor%d_chip%d", ladder, sensor, roc),
                                              Form("B0 ladder%d sensor%d chip%d", ladder, sensor, roc),
                                              nbinevtseq, 0, nbinevtseq * binwidth);
    }


    //Initialize Chip level TProfiles for B1
    for (int ichip = 0; ichip < NCHIP_B1; ichip++)
    {
        int ladder = ichip / 16;
        int sensor = (ichip / 4) % 4;
        int roc = ichip % 4;

        //make a temporary TProfile to hold the chip level raw hits
        B1_chip_profile[ichip] = new TProfile(Form("prof_chiprawhit_B1_ladder%d_sensor%d_chip%d", ladder, sensor, roc),
                                              Form("B1 ladder%d sensor%d chip%d", ladder, sensor, roc),
                                              nbinevtseq, 0, nbinevtseq * binwidth);
    }


    //Initialize Chip level TProfiles for B2
    for (int ichip = 0; ichip < NCHIP_B2; ichip++)
    {
        int ladder = ichip / (5 * 12);
        int sensor = (ichip / 12) % 5;
        int roc = ichip % 12;

        //make a temporary TProfile to hold the chip level raw hits
        B2_chip_profile[ichip] = new TProfile(Form("prof_chiprawhit_B2_ladder%d_sensor%d_chip%d", ladder, sensor, roc),
                                              Form("B2 ladder%d sensor%d chip%d", ladder, sensor, roc),
                                              nbinevtseq, 0, nbinevtseq * binwidth);
    }


    //Initialize Chip level TProfiles for B3
    for (int ichip = 0; ichip < NCHIP_B3; ichip++)
    {
        int ladder = ichip / (6 * 12);
        int sensor = (ichip / 12) % 6;
        int roc = ichip % 12;

        //make a temporary TProfile to hold the chip level raw hits
        B3_chip_profile[ichip] = new TProfile(Form("prof_chiprawhit_B3_ladder%d_sensor%d_chip%d", ladder, sensor, roc),
                                              Form("B3 ladder%d sensor%d chip%d", ladder, sensor, roc),
                                              nbinevtseq, 0, nbinevtseq * binwidth);
    }



    //Initialize <ADC> TProfiles for each channel in B2
    for (int ichip = 0; ichip < NCHIP_B2; ichip++)
    {
        int ladder = ichip / (5 * 12);
        int sensor = (ichip / 12) % 5;
        int roc = ichip % 12;

        for (int ichannel = 0; ichannel < NSTRIPCHANNEL; ichannel++)
        {
            B2_strip_adc_profile[ichip][ichannel] = new TProfile(Form("prof_stripadc_B2_ladder%d_sensor%d_chip%d_channel%d", ladder, sensor, roc, ichannel),
                    Form("B2 ladder%d sensor%d chip%d channel%d", ladder, sensor, roc, ichannel),
                    nbinevtseq, 0, nbinevtseq * binwidth);
            B2_strip_adc_profile[ichip][ichannel]->SetXTitle("Evt. Sequence");
            B2_strip_adc_profile[ichip][ichannel]->SetYTitle("<adc> above threshold");
        }
    }

    //Initialize <ADC> TProfiles for each channel in B3
    for (int ichip = 0; ichip < NCHIP_B3; ichip++)
    {
        int ladder = ichip / (6 * 12);
        int sensor = (ichip / 12) % 6;
        int roc = ichip % 12;

        for (int ichannel = 0; ichannel < NSTRIPCHANNEL; ichannel++)
        {
            B3_strip_adc_profile[ichip][ichannel] = new TProfile(Form("prof_stripadc_B3_ladder%d_sensor%d_chip%d_channel%d", ladder, sensor, roc, ichannel),
                    Form("B3 ladder%d sensor%d chip%d channel%d", ladder, sensor, roc, ichannel),
                    nbinevtseq, 0, nbinevtseq * binwidth);
            B3_strip_adc_profile[ichip][ichannel]->SetXTitle("Evt. Sequence");
            B3_strip_adc_profile[ichip][ichannel]->SetYTitle("<adc> above threshold");
        }
    }



    if (verbosity > 0)
    {
        cout << "SvxStabilityQA::Init_chipTProfiles() - Successfully initialized chip level TProfiles." << endl;
    }


}
