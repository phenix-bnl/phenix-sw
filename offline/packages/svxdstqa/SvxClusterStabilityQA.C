#include "SvxClusterStabilityQA.h"
#include "SvxQADefs.h"

#include <phool.h>
#include <PHCompositeNode.h>
#include <Fun4AllServer.h>
#include <Fun4AllReturnCodes.h>
#include <getClass.h>

#include <TrigLvl1.h>
#include <TriggerHelper.h>
#include <RunHeader.h>
#include <EventHeader.h>
#include <VtxOut.h>
#include <SvxRawhitList.h>
#include <SvxRawhit.h>
#include <SvxClusterList.h>
#include <SvxCluster.h>
#include <SvxRawhitClusterList.h>
#include <SvxRawhitCluster.h>
#include <SvxQAEventSelection.h>

#include <svxAddress.hh>

#include <TSQLServer.h>
#include <TSQLStatement.h>

#include <TFile.h>
#include <TH1.h>
#include <TH2.h>
#include <TH3.h>
#include <TProfile.h>
#include <TTree.h>
#include <TBenchmark.h>

#include <iostream>
#include <iomanip>
#include <cmath>

using namespace std;
using namespace findNode;


bool SvxClusterStabilityQA::getRawhitIndex(
    const int nrawcls,
    SvxCluster *cls,
    int &pos, int &size,
    bool isfirst)
{
    static int rc_first = 0;
    if (isfirst)
    {
        rc_first = 0;
    }

    int i_size = (cls->get_layer() < 2) ? cls->get_size()
                 : (cls->get_xz_size(0) + cls->get_xz_size(1));


    int rc_max = rc_first + i_size;
    if (rc_max > nrawcls)
    {
        cout << "exceed rc max " << rc_max << " " << nrawcls << endl;
        return false;
    }

    // return value
    size = i_size;
    pos  = rc_first;


    // move to next first position
    rc_first += size;

    return true;
}



SvxClusterStabilityQA::SvxClusterStabilityQA():
    SubsysReco("SVXCLUSTERSTABILITYQA"),
    d_eventhead(NULL),
    d_vtxout(NULL),
    d_svxraw(NULL),
    d_svxcls(NULL),
    d_svxrawcls(NULL),
    d_svxadr(NULL),
    d_eventselection(NULL),
    is_tickcut(true),
    m_bbczcut(10), //bbcz cut
    m_EventNumber(0),
    m_EventSeqNumber(0),
    m_runnumber(0)
{
    for (int i = 0; i < 3; i++) m_pticks[i] = 0;
}


int SvxClusterStabilityQA::Init(PHCompositeNode *topNode)
{
    m_EventNumber = 0;
    m_EventSeqNumber = 0;

    // initialize event selection handler
    d_eventselection = new SvxQAEventSelection();
    d_eventselection->Set_BBCZCut(m_bbczcut);
    d_eventselection->Set_TickCut(is_tickcut);
    d_eventselection->Set_Verbosity(verbosity);

    memset(nevents, 0, sizeof(nevents));
    memset(B0_chips_hit, 0, sizeof(B0_chips_hit));
    memset(B1_chips_hit, 0, sizeof(B1_chips_hit));
    memset(B2_chips_hit, 0, sizeof(B2_chips_hit));
    memset(B3_chips_hit, 0, sizeof(B3_chips_hit));
    memset(B0_chips_hitw, 0, sizeof(B0_chips_hitw));
    memset(B1_chips_hitw, 0, sizeof(B1_chips_hitw));
    memset(B2_chips_hitw, 0, sizeof(B2_chips_hitw));
    memset(B3_chips_hitw, 0, sizeof(B3_chips_hitw));


    return EVENT_OK;
}

//==============================================================
int SvxClusterStabilityQA::InitRun(PHCompositeNode *topNode)
{
    if (m_outname.length() == 0)
    {
        cerr << "SvxClusterStabilityQA::InitRun Outputname is not set. Abort" << endl;
        return ABORTRUN;
    }

    RunHeader *runheader = findNode::getClass<RunHeader>(topNode, "RunHeader");
    if (runheader == NULL)
    {
        if (verbosity > 0)
        {
            cout << PHWHERE << "Can't find RunHeader. " << endl;
        }
        return ABORTRUN;
    }


    //set the bin limits for the TProfiles
    m_runnumber = runheader->get_RunNumber();
    Set_maxevents(m_runnumber);

    //initialize histograms;
    Init_TProfiles();

    memset(nevents, 0, sizeof(nevents));
    memset(B0_chips_hit, 0, sizeof(B0_chips_hit));
    memset(B1_chips_hit, 0, sizeof(B1_chips_hit));
    memset(B2_chips_hit, 0, sizeof(B2_chips_hit));
    memset(B3_chips_hit, 0, sizeof(B3_chips_hit));
    memset(B0_chips_hitw, 0, sizeof(B0_chips_hitw));
    memset(B1_chips_hitw, 0, sizeof(B1_chips_hitw));
    memset(B2_chips_hitw, 0, sizeof(B2_chips_hitw));
    memset(B3_chips_hitw, 0, sizeof(B3_chips_hitw));

    return EVENT_OK;
}
//==============================================================
int SvxClusterStabilityQA::process_event(PHCompositeNode *topNode)
{

    GetNodes(topNode);

    m_EventSeqNumber = d_eventhead->get_EvtSequence();
    int ieventbin    = floor(m_EventSeqNumber / binwidth);

    if (verbosity > 0)
    {
        if (m_EventNumber % 1000 == 0 && m_EventNumber > 1000 - 1)
        {
            cout << "SvxClusterStabilityQA::process_event() event no: " << m_EventNumber << " " << m_EventSeqNumber << endl;
        }
    }

    //this handles the trigger & event selection
    if (!d_eventselection->EventSelection(topNode)) return EVENT_OK;

    float zbbc = d_vtxout->get_ZVertex("BBC");
    m_hzbbc->Fill(zbbc);
    m_hzbbc_vs_evtseq->Fill(m_EventSeqNumber, zbbc);


    // counting number of analyzed events
    nevents[ieventbin]++;


    m_hzbbc_vs_evtseq_selection->Fill(m_EventSeqNumber, zbbc);

    //////////////////////
    int nclus    = (d_svxcls != NULL)    ? d_svxcls->get_nClusters() : 0;
    int nraw     = (d_svxraw != NULL)    ? d_svxraw->get_nRawhits()  : 0;
    int nrawclus = (d_svxrawcls != NULL) ? d_svxrawcls->get_nRawhitClusters() : 0;

    for (int ihit = 0; ihit < nclus; ihit++)
    {
        //cout<<"icls : "<<ihit<<endl;

        int size, pos;
        SvxCluster *cls = d_svxcls->get_Cluster(ihit);

        if (cls->get_svxSection() != 0) continue; // require pixel and strip

        if (!SvxClusterStabilityQA::getRawhitIndex(nrawclus, cls, pos, size, ihit == 0))
        {
            cerr << " getRawhitIndex failed" << endl;
            continue;
        }

        int   c_layer = cls->get_layer();

        float weight   = 1. / cls->get_size();   // for pixel
        float x_weight = 1. / cls->get_xz_size(0); // for strip-x
        float z_weight = 1. / cls->get_xz_size(1); // for strip-z

        // rawhit-cluster loop to get rawhit
        for (int irc = pos; irc < pos + size; irc++)
        {
            SvxRawhitCluster *svxrc = (d_svxrawcls!=NULL) ? d_svxrawcls->get_RawhitCluster(irc) : NULL;
            if(svxrc==NULL) continue;

            int iraw = svxrc->get_rawhitID();
            int icls = svxrc->get_clusterID();
            if (ihit != icls)
            {
                cout << "clusterID is different : " << icls << " " << ihit << endl;
                continue;
            }


            if (iraw < nraw)
            {
                SvxRawhit *rhit = d_svxraw->get_Rawhit(iraw);
                if (iraw != rhit->get_hitID())
                {
                    cout << "rawhit id is different with idx " << iraw << " " << rhit->get_hitID() << endl;
                    continue;
                }

                int r_layer   = rhit->get_layer();
                int r_ladder  = rhit->get_ladder();
                int r_sensor  = rhit->get_sensor();

                int chip_no = 0;
                if (r_layer < 2) // pixel
                {
                    int channel     = rhit->get_channel();
                    int sensor_chip = rhit->get_pixelROC() % 4; // ROC=0-7

                    if (verbosity > 1)
                    {
                        cout << "eventnum" << m_EventNumber << endl;
                        cout << "layer " << r_layer << endl;
                        cout << "ladder" << r_ladder << endl;
                        cout << "sensor" << r_sensor << endl;
                        cout << "chip  " << sensor_chip << endl;
                    }

                    chip_no  = r_ladder * 4 * 4 + r_sensor * 4 + sensor_chip;
                    if (r_layer == 0)
                    {
                        B0_chips_hit [chip_no][ieventbin][channel]++;
                        B0_chips_hitw[chip_no][ieventbin][channel] += weight;
                    }
                    else if (r_layer == 1)
                    {
                        B1_chips_hit [chip_no][ieventbin][channel]++;
                        B1_chips_hitw[chip_no][ieventbin][channel] += weight;
                    }
                }
                else  // strip
                {
                    int r_SS      = rhit->get_sensorSection();
                    int r_readout = rhit->get_sensorReadout();
                    int r_channel = rhit->get_channel();
                    int stripRoc        = d_svxadr->getStripRoc(r_SS, r_readout, r_channel);
                    int stripRocChannel = d_svxadr->getStripRocChannel(r_SS, r_readout, r_channel);

                    weight = (r_readout == 0) ? x_weight : z_weight;

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
                        //cout << "adc" << adc << endl;
                    }

                    if (r_layer == 2)
                    {
                        chip_no = r_ladder * 5 * 12 + r_sensor * 12 + stripRoc;
                        B2_chips_hit[chip_no][ieventbin][stripRocChannel]++;
                        B2_chips_hitw[chip_no][ieventbin][stripRocChannel] += weight;
                    }
                    else if (r_layer == 3)
                    {
                        chip_no = r_ladder * 6 * 12 + r_sensor * 12 + stripRoc;
                        B3_chips_hit[chip_no][ieventbin][stripRocChannel]++;
                        B3_chips_hitw[chip_no][ieventbin][stripRocChannel] += weight;
                    }


                }
            }
        }

        ////////////////////
        // fill cluster info for debug
        int c_ladder = cls->get_ladder();
        int c_sensor = cls->get_sensor();
        double lx = cls->get_xyz_local(0);
        double lz = cls->get_xyz_local(2);

        int sensid = -1;
        if (c_layer == 0 || c_layer == 1) sensid = 4 * c_ladder + c_sensor;
        else if (c_layer == 2)        sensid = 5 * c_ladder + c_sensor;
        else if (c_layer == 3)        sensid = 6 * c_ladder + c_sensor;
        else
        {
            cerr << "out of range layer : " << c_layer << endl;
            continue;
        }

        //cout<<ihit<<" "<<sensid<<" : "<<c_layer<<" "<<c_ladder<<" "<<c_sensor<<endl;
        m_h3_clsmapd[c_layer]->Fill(sensid, lz, lx);
    }

    m_EventNumber++;

    return EVENT_OK;
}
//==============================================================

//----Classes needed----------------------------------------

int SvxClusterStabilityQA::GetNodes(PHCompositeNode *topNode)
{

    //-------------------EventHeader----------------------------------
    d_eventhead = findNode::getClass<EventHeader>(topNode, "EventHeader");
    if (d_eventhead == NULL)
    {
        cerr << PHWHERE << " EventHeader node not found." << endl;
        return DISCARDEVENT;
    }
    //------------------------------------------------------------------

    //------------------Vertex------------------------------
    d_vtxout = findNode::getClass<VtxOut>(topNode, "VtxOut");
    if (d_vtxout == NULL)
    {
        cerr << "VtxOut node not found." << endl;
        return DISCARDEVENT;
    }
    //-----------------------------------------------------

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

    //----------------SvxClusterList-------------------------
    d_svxrawcls = findNode::getClass<SvxRawhitClusterList>(topNode, "SvxRawhitClusterList");
    if (d_svxrawcls == NULL)
    {
        cerr << "SvxRawhitClusterList node not found.Register SvxReco module" << endl;
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

int SvxClusterStabilityQA::End(PHCompositeNode *topNode)
{

    TFile *file = new TFile(m_outname.c_str(), "RECREATE");
    if (verbosity > 0)
        cout << "SvxClusterStabilityQa::End() - writing TProfiles to file" << endl;

    TTree *profiletree = new TTree("profiletree", "Tree of profiles for each pixel or channel based on Clusters in the VTX");
    TProfile *tmpprofile = 0;
    profiletree->Branch("pixel", "TProfile", &tmpprofile);
    profiletree->SetAutoFlush(1000);

    //first fill all the pixel/channel level hit vs evt sequence TProfiles


    //B0 loop

    //--------------------------------------//
    //--- Pixel / channel based profiles ---//
    //--------------------------------------//
    //for testing purposes
    TBenchmark bm;
    bm.Start("filltimer");


    //PIXEL LAYER 1 (B0) or 2 (B1)
    for (int ilayer = 0; ilayer < 2; ilayer++)
    {
        int nchip;
        unsigned int (*chips_hit)[nbinevtseq][NPIXEL];
        float        (*chips_hitw)[nbinevtseq][NPIXEL];
        if (ilayer == 0)
        {
            nchip      = NCHIP_B0;
            chips_hit  = B0_chips_hit;
            chips_hitw = B0_chips_hitw;
        }
        else   // B1
        {
            nchip      = NCHIP_B1;
            chips_hit  = B1_chips_hit;
            chips_hitw = B1_chips_hitw;
        }

        for (int ichip = 0; ichip < nchip; ichip++)
        {
            if (verbosity > 1)
                cout << "B" << ilayer << ", Chip " << ichip << " writing" << endl;

            int ladder = ichip / 16;
            int sensor = (ichip / 4) % 4;
            int roc    = ichip % 4;
            for (int ipix = 0; ipix < NPIXEL; ipix++)
            {
                int row = ipix / 32;
                int col = ipix % 32;

                tmpprofile = new TProfile(
                    Form("prof_pixelrawhit_B%d_ladder%d_sensor%d_roc%d_pixel%d_with_mask",
                         ilayer, ladder, sensor, roc, ipix),
                    Form("B%d ladder%d sensor%d chip%d pixel%d with mask",
                         ilayer, ladder, sensor, roc, ipix),
                    nbinevtseq, 0, nbinevtseq * binwidth);

                int   total_evt = 0;
                int   total_hit_sum = 0;
                float total_hit_sumw = 0;
                for (int ibin = 0; ibin < nbinevtseq; ibin++)
                {
                    int   total_pixel_hits   = chips_hit[ichip][ibin][ipix];
                    float total_pixel_hits_w = chips_hitw[ichip][ibin][ipix];
                    total_evt      += nevents[ibin];
                    total_hit_sum  += total_pixel_hits;
                    total_hit_sumw += total_pixel_hits_w;

                    // fill hit
                    if (total_pixel_hits > 0)
                    {
                        tmpprofile->Fill(tmpprofile->GetBinCenter(ibin + 1), 1, total_pixel_hits_w);
                    }

                    // fill nohit
                    tmpprofile->Fill(tmpprofile->GetBinCenter(ibin + 1), 0, nevents[ibin] - total_pixel_hits);

                    tmpprofile->SetBinEntries(ibin + 1, nevents[ibin]);
                }

                tmpprofile->SetEntries(total_evt);

                profiletree->Fill();
                delete tmpprofile;

                m_h3_clsmap[ilayer]->Fill(ichip + 0.5, col + 0.5, row + 0.5, total_hit_sum);
                m_h3_clsmapw[ilayer]->Fill(ichip + 0.5, col + 0.5, row + 0.5, total_hit_sumw);
            } // pixel loop
        } // chip loop

    } // layer loop


    /////////////////////////////////////////////
    //STRIPPIXEL LAYER 1 and 2 (B2 and B3)
    for (int layer = 2; layer < 4; layer++)
    {
        int nchip = 0;
        int nsens_lad = 0;
        unsigned int (*chips_hit)[nbinevtseq][NSTRIPCHANNEL];
        float        (*chips_hitw)[nbinevtseq][NSTRIPCHANNEL];
        if (layer == 2)
        {
            nchip      = NCHIP_B2;
            nsens_lad  = 5;
            chips_hit  = B2_chips_hit;
            chips_hitw = B2_chips_hitw;
        }
        else if (layer == 3)
        {
            nchip      = NCHIP_B3;
            nsens_lad  = 6;
            chips_hit  = B3_chips_hit;
            chips_hitw = B3_chips_hitw;
        }
        else
        {
            cerr << "out of layer : " << layer << endl;
            break;
        }

        for (int ichip = 0; ichip < nchip; ichip++)
        {
            if (verbosity > 1)
                cout << "B" << layer << ", Chip " << ichip << " writing" << endl;

            int ladder = ichip / (nsens_lad * 12);
            int sensor = (ichip / 12) % nsens_lad;
            int roc    = ichip % 12;

            for (int ipix = 0; ipix < NSTRIPCHANNEL; ipix++)
            {
                tmpprofile = new TProfile(
                    Form("prof_channelrawhit_B%d_ladder%d_sensor%d_roc%d_pixel%d",
                         layer, ladder, sensor, roc, ipix),
                    Form("B%d ladder%d sensor%d chip%d pixel%d",
                         layer, ladder, sensor, roc, ipix),
                    nbinevtseq, 0, nbinevtseq * binwidth);

                int   total_evt = 0;
                int   total_pixel_hits_sum   = 0;
                float total_pixel_hits_sum_w = 0;
                for (int ibin = 0; ibin < nbinevtseq; ibin++)
                {
                    int   total_pixel_hits   = chips_hit[ichip][ibin][ipix];
                    float total_pixel_hits_w = chips_hitw[ichip][ibin][ipix];
                    total_pixel_hits_sum   += total_pixel_hits;
                    total_pixel_hits_sum_w += total_pixel_hits_w;
                    total_evt              += nevents[ibin];

                    //fill all the hits
                    if (total_pixel_hits > 0)
                    {
                        tmpprofile->Fill(tmpprofile->GetBinCenter(ibin + 1), 1, total_pixel_hits_w);
                    }

                    //now fill for all the events with no hits
                    tmpprofile->Fill(tmpprofile->GetBinCenter(ibin + 1), 0, nevents[ibin] - total_pixel_hits);
                    tmpprofile->SetBinEntries(ibin + 1, nevents[ibin]);
                }

                tmpprofile->SetEntries(total_evt);

                profiletree->Fill();
                delete tmpprofile;

                // fill hit hist
                int lad_sens  = ladder * nsens_lad + sensor;
                int sensor_ch = d_svxadr->getStripSensorChannel(roc, ipix); // 0-383
                int sensec    = d_svxadr->getStripSensorSection(roc); // 0:L, 1:R
                int readout   = d_svxadr->getStripSensorReadout(roc); // 0:X, 1:U

                for (int i = 0; i < 30; i++)
                {
                    int ichx = 0, ichz = 0;
                    if (sensec == 0)
                    {
                        ichz = i;
                        if (readout == 0)
                        {
                            ichx = sensor_ch;
                        }
                        else
                        {
                            ichx = sensor_ch + i;
                            if (ichx >= 384) ichx -= 384;
                        }
                    }
                    else // sensec==1
                    {
                        ichz = i + 30;
                        if (readout == 0)
                        {
                            ichx = sensor_ch;
                        }
                        else
                        {
                            ichx = sensor_ch + i - 30;
                            if (ichx < 0) ichx += 384;
                        }
                    }

                    m_h3_clsmap[layer]->Fill(lad_sens + 0.5, ichz + 0.5, ichx + 0.5, total_pixel_hits_sum);
                    m_h3_clsmapw[layer]->Fill(lad_sens + 0.5, ichz + 0.5, ichx + 0.5, total_pixel_hits_sum_w);
                }
            }
        } // chip loop
    } // layer loop

    if (verbosity > 0)
    {
        cout << "SvxClusterStabilityQA::End() - Done filling channel TProfiles" << endl;
    }


    //--------------------------------------//
    //----- write the tree to file ---------//
    //--------------------------------------//
    if (verbosity > 0)
    {
        cout << "SvxClusterStabilityQA::End() - Writing TTree of TProfiles to file" << endl;
    }

    profiletree->Write();

    bm.Stop("filltimer");
    bm.Show("filltimer");

    ////////
    m_hzbbc->Write();
    m_hzbbc_vs_evtseq->Write();
    m_hzbbc_vs_evtseq_selection->Write();
    m_htrigL1->Write();
    m_htrignarrow->Write();
    m_htrignarrow_copyA->Write();
    m_htrignarrow_copyB->Write();
    m_htrignovtx->Write();
    for (int i = 0; i < 4; i++) m_h3_clsmap[i]->Write();
    for (int i = 0; i < 4; i++) m_h3_clsmapw[i]->Write();
    for (int i = 0; i < 4; i++) m_h3_clsmapd[i]->Write();

    file->Close();

    return 0;
}


//  copy from SvxStabilityQA
void SvxClusterStabilityQA::Set_maxevents(int runnumber)
{
    //This method sets the range of the TProfiles based on the number of events in the run

    //first get the number of events in this run from the daq database
    char cmd[200];
    sprintf(cmd, "select eventsinrun from run where runnumber=%d", runnumber);
    TSQLServer *db = TSQLServer::Connect("odbcn://daq", "phnxrc", "");
    if (verbosity > 0)
        cout << "SvxClusterStabilityQA::Set_maxevents() - DB: " << db->ServerInfo() << endl;

    TSQLStatement *stmt = db->Statement(cmd, 1);
    Int_t nevents = -1;
    if (stmt->Process())
    {
        stmt->StoreResult();
        while (stmt->NextResultRow())
        {
            nevents = stmt->GetInt(0);
        }
    }
    delete db;

    if (nevents < 0)
        nevents = 50000000; //set default to 50M if we can't get the number from the database

    //We want to bin the TProfiles so that rebin can be used to get the same binning for different number of events
    //(i.e. we want the bin widths to be integer multiples of eachother)
    if      (                      nevents <=  3125000)
    {
        MAXEVENTS =   3125000;
    }
    else if ( 3125000 < nevents && nevents <=  6250000)
    {
        MAXEVENTS =   6250000;
    }
    else if ( 6250000 < nevents && nevents <= 12500000)
    {
        MAXEVENTS =  12500000;
    }
    else if (12500000 < nevents && nevents <= 25000000)
    {
        MAXEVENTS =  25000000;
    }
    else if (25000000 < nevents && nevents <= 50000000)
    {
        MAXEVENTS =  50000000;
    }
    else if (50000000 < nevents)
    {
        MAXEVENTS = 100000000;
    }
    else
    {
        MAXEVENTS = 50000000;
    }


    //set the bin width, which depends on the total number of events in the run, and the number of available bins
    binwidth = MAXEVENTS / nbinevtseq;

    if (verbosity > 0)
    {
        cout << "SvxClusterStabilityQA::Set_maxevents() - runnumber=" << runnumber << " events in this run=" << nevents << endl;
        cout << "SvxClusterStabilityQA::Set_maxevents() - set MAXEVENTS=" << MAXEVENTS << " and binwidth=" << binwidth << endl;
    }
}

void SvxClusterStabilityQA::Init_TProfiles()  //initialize the TProfiles that need to be in memory during processevent
{
    //------------------------------------------------------------------------------------------------------
    // histograms
    //------------------------------------------------------------------------------------------------------
    m_hzbbc  =  new TH1D("hzbbc_svxstablityQA", "hzbbc", 800, -40, 40); //zbbc vertex;

    m_hzbbc_vs_evtseq = new TH2F("hzbbc_vs_evtseq_svxstabilityQA",
                                 "bbcz vs evtseq", nbinevtseq, 0, nbinevtseq * binwidth, 100, -50, 50);
    m_hzbbc_vs_evtseq->SetXTitle("event sequential number");
    m_hzbbc_vs_evtseq->SetYTitle("bbcz");

    m_hzbbc_vs_evtseq_selection = new TH2F("hzbbc_vs_evtseq_selection_svxstabilityQA",
                                           "bbcz vs evtseq", nbinevtseq, 0, nbinevtseq * binwidth, 100, -50, 50);
    m_hzbbc_vs_evtseq_selection->SetXTitle("event sequential number");
    m_hzbbc_vs_evtseq_selection->SetYTitle("bbcz");

    //counts trigger
    m_htrigL1 = new TH1I("htrigl1", "htrigL1", nbinevtseq, 0, nbinevtseq * binwidth);
    m_htrigL1->SetXTitle("event sequential number");

    m_htrignarrow = new TH1I("htrignarrow", "htrignarrow", nbinevtseq, 0, nbinevtseq * binwidth);
    m_htrignarrow->SetXTitle("event sequential number");

    m_htrignarrow_copyA = new TH1I("htrignarrow_copyA", "htrignarrow_copyA", nbinevtseq, 0, nbinevtseq * binwidth);
    m_htrignarrow_copyA->SetXTitle("event sequential number");

    m_htrignarrow_copyB = new TH1I("htrignarrow_copyB", "htrignarrow_copyB", nbinevtseq, 0, nbinevtseq * binwidth);
    m_htrignarrow_copyB->SetXTitle("event sequential number");

    m_htrignovtx = new TH1I("htrignovtx", "htrignovtx", nbinevtseq, 0, nbinevtseq * binwidth);
    m_htrignovtx->SetXTitle("event sequential number");

    ////////////
    // cluster hit map
    m_h3_clsmap[0] = new TH3D("h3_clsmap_0", "B0 chipid col row", 160, 0, 160, 32, 0, 32, 256, 0, 256);
    m_h3_clsmap[1] = new TH3D("h3_clsmap_1", "B1 chipid col row", 320, 0, 320, 32, 0, 32, 256, 0, 256);
    m_h3_clsmap[2] = new TH3D("h3_clsmap_2", "B2 chipid col row",  80, 0,  80, 60, 0, 60, 384, 0, 384); // 5sensor*16ladder
    m_h3_clsmap[3] = new TH3D("h3_clsmap_3", "B3 chipid col row", 144, 0, 144, 60, 0, 60, 384, 0, 384); // 6sensor*24ladder

    m_h3_clsmapw[0] = new TH3D("h3_clsmapw_0", "B0 w chipid col row", 160, 0, 160, 32, 0, 32, 256, 0, 256);
    m_h3_clsmapw[1] = new TH3D("h3_clsmapw_1", "B1 w chipid col row", 320, 0, 320, 32, 0, 32, 256, 0, 256);
    m_h3_clsmapw[2] = new TH3D("h3_clsmapw_2", "B2 w chipid col row",  80, 0,  80, 60, 0, 60, 384, 0, 384); // 5sensor*16ladder
    m_h3_clsmapw[3] = new TH3D("h3_clsmapw_3", "B3 w chipid col row", 144, 0, 144, 60, 0, 60, 384, 0, 384); // 6sensor*24ladder


    // cluster hit map
    static const double colsize[2] = {0.0425, 0.0625};
    static const int    sizeid[7]  = {0,  1,  0, 1,  0, 1,  0};
    static const int    ncol[7]    = {31, 2, 30, 2, 30, 2, 31};

    static const double xmin = -0.5 * 0.0050 * 256;
    //static const float xmax =  0.5*0.0050*256;

    double zsize = -0.5 * (colsize[0] * 2 * (31 + 30) + colsize[1] * 2 * 3);
    //cout<<zsize<<endl;

    double zary[129];
    zary[0] = zsize;
    int idx = 0;
    for (int id = 0; id < 7; id++)
    {
        for (int icol = 0; icol < ncol[id]; icol++)
        {
            zsize += colsize[sizeid[id]];

            zary[idx + 1] = zsize;
            //cout<<idx<<" "<<setw(10)<<zsize<<" "<<setw(10)<<zary[idx]<<" "<<setw(10)<<zsize - zary[idx]<<endl;
            idx++;
        }
    }

    double xary[41], xary2[81];
    for (int i = 0; i < 41; i++)
    {
        xary[i] = i;
    }
    for (int i = 0; i < 81; i++)
    {
        xary2[i] = i;
    }

    double yary[257];
    for (int i = 0; i < 257; i++)
    {
        yary[i] = 0.005 * i + xmin;
    }

    m_h3_clsmapd[0] = new TH3D("h3_clsmapd_0", "B0 sensor id, col, row", 10 * 4, xary,  128, zary, 256, yary);
    m_h3_clsmapd[1] = new TH3D("h3_clsmapd_1", "B1 sensor id, col, row", 20 * 4, xary2, 128, zary, 256, yary);
    m_h3_clsmapd[2] = new TH3D("h3_clsmapd_2", "B2 sensor id, z, x", 16 * 5, 0, 16 * 5,  60, -3, 3, 384, -1.536, 1.536);
    m_h3_clsmapd[3] = new TH3D("h3_clsmapd_3", "B3 sensor id, z, x", 24 * 6, 0, 24 * 6,  60, -3, 3, 384, -1.536, 1.536);

}


bool SvxClusterStabilityQA::calcPixelChipChannel(double lx, double lz, int *chip, int *row, int *col, bool isdebug)
{
    static const double chippos[5] = { -2.780, -1.400, 0.000, 1.400, 2.780};
    static const double halfx      = 0.640; //cm = 50um*128;

    ///////////////
    if (lx <= -halfx || halfx < lx)
    {
        cerr << "lx out of range : " << lx << endl;
        return false;
    }


    // calc chip
    int rchip = -1;
    for (int ichip = 0; ichip < 4; ichip++)
    {
        if ( chippos[ichip] <= lz && lz < chippos[ichip + 1] )
        {
            rchip = ichip;
            break;
        }
    }
    if (rchip == -1)
    {
        cerr << "chip out of range : " << rchip << endl;
        return false;
    }

    if (isdebug) cout << "rchip " << rchip << endl;


    // calc row // 0-255
    int lx2 = 10000 * (lx + halfx) + 0.5;
    if (isdebug) cout << "lx2 : " << lx2 << " " << lx << endl;
    int i_row = lx2 / 50; // divide by 50um to get bin#
    if (i_row < 0 || 255 < i_row)
    {
        cerr << "row out of range : " << i_row << " " << lx << endl;
        return false;
    }

    // calc col -- 0-31
    int lz2 = 10000 * (lz - chippos[rchip]) + 0.5; // round up at 0.5um
    if (isdebug) cout << "lz2 : " << lz2 << " " << lz << endl;
    if (lz2 < 0)
    {
        cout << "out of range lz2<0 : " << lz2 << endl;
        return false;
    }

    int i_col = 0;
    if (rchip == 0)
    {
        i_col = lz2 / 425;
    }
    else
    {
        if (0 <= lz2 && lz2 < 625)
        {
            i_col = 0;
        }
        else
        {
            lz2 -= 625;
            i_col = (lz2 / 425) + 1;
        }
    }
    if (i_col < 0 || 31 < i_col)
    {
        if (i_col == 32) i_col = 31;
        else
        {
            cerr << "col out of range : " << i_col << " " << lz << " " << lz - chippos[rchip] << endl;
            return false;
        }
    }


    int i_chip = 3 - rchip; // reverse order


    *chip = i_chip;
    *row  = i_row;
    *col  = i_col;

    return true;
}


bool SvxClusterStabilityQA::calcStripChipChannel(double lx, double lz,
        int *senSec,
        int *sensChipX, int *sensChipU,
        int *sensChipChanX, int *sensChipChanU,
        int *iLx, int *iLz,
        bool isdebug
                                                )
{
    static const double half_x = 1.536; // cm =   80*384*0.5um
    static const double half_z = 3.0;   // cm = 1000* 60*0.5um


    // sensor section
    if (lz < -half_z || half_z < lz)
    {
        cout << "out of range lz: " << lz << endl;
    }

    int sensec = (lz < 0) ? 0 : 1;

    // lx
    if ( lx < -half_x || half_x <= lx )
    {
        cout << "out of range lx " << lx << endl;
        return false;
    }

    double lx2 = floor(10000 * (lx + half_x) + 0.5) / 10000.;
    int ilx = int(lx2 / 0.008);
    if (ilx < 0 || 384 <= ilx)
    {
        cout << "out of range ilx " << ilx << endl;
        return false;
    }

    // lz
    double lz2 = floor(10000.*(lz + (sensec == 0 ? half_z : 0.0)) + 0.5) / 10000.;
    int ilz = lz2 / 0.1; // ilz == 0-29. range is check by if (<half_z)
    int iu  = ilx - ilz + (sensec == 0 ? 0 : +30); // iu = 0-383
    if (384 <= iu)   iu -= 384;
    else if (iu < 0) iu += 384;

    if (isdebug)
        cout << "  d z :" << lz2 << " " << ilz << " " << iu << endl;


    // convert chip channel
    int xchip    = int(ilx / 128);
    int xchip_ch =     ilx % 128;

    int uchip    = int(iu / 128);
    int uchip_ch =     iu % 128;


    //////////
    // convert to sensor chip & sensor channel
    int a_xchip = 2 - xchip;
    int a_uchip = 2 - uchip;

    int a_xchip_ch = -1;
    int a_uchip_ch = -1;
    if (sensec == 0)
    {
        // channel selection
        if (a_xchip == 0 || a_xchip == 1) a_xchip_ch = 127 - xchip_ch;
        else                       a_xchip_ch = xchip_ch;

        if (a_uchip == 0) a_uchip_ch = 127 - uchip_ch;
        else           a_uchip_ch = uchip_ch;

    }
    else   // sensec==1
    {
        // channel selection
        if (a_xchip == 0) a_xchip_ch = 127 - xchip_ch;
        else           a_xchip_ch = xchip_ch;

        if (a_uchip == 0 || a_uchip == 1) a_uchip_ch = 127 - uchip_ch;
        else                       a_uchip_ch = uchip_ch;
    }

    int senschip_x = -1, senschip_u = -1;
    if (sensec == 0)
    {
        if     (a_xchip == 0) senschip_x = 1;
        else if (a_xchip == 1) senschip_x = 2;
        else if (a_xchip == 2) senschip_x = 9;

        if     (a_uchip == 0) senschip_u =  0;
        else if (a_uchip == 1) senschip_u = 11;
        else if (a_uchip == 2) senschip_u = 10;
    }
    else
    {
        if     (a_xchip == 0) senschip_x = 3;
        else if (a_xchip == 1) senschip_x = 8;
        else if (a_xchip == 2) senschip_x = 7;

        if     (a_uchip == 0) senschip_u = 4;
        else if (a_uchip == 1) senschip_u = 5;
        else if (a_uchip == 2) senschip_u = 6;
    }

    if (senschip_x < 0 || 12 <= senschip_x)
    {
        cout << "out of range senschip_x : " << senschip_x << endl;
        return false;
    }
    if (senschip_u < 0 || 12 <= senschip_u)
    {
        cout << "out of range senschip_u : " << senschip_u << endl;
        return false;
    }

    *senSec    = sensec;
    *sensChipX = senschip_x;
    *sensChipU = senschip_u;
    *sensChipChanX = a_xchip_ch;
    *sensChipChanU = a_uchip_ch;
    *iLx = ilx;
    *iLz = ilz;

    return true;
}


