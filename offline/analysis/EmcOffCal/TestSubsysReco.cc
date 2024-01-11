#include "TestSubsysReco.h"
#include "Warnmap.h"

#include <TFile.h>
#include <TH1.h>
#include <TH2.h>
#include <TTree.h>
#include <TVector3.h>

#include <PHNodeIterator.h>
#include <PHTypedNodeIterator.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>

#include <Fun4AllReturnCodes.h>
#include <Fun4AllServer.h>
#include <recoConsts.h>
#include <RunHeader.h>
#include <PHGlobal.h>
#include <SpinDataEventOut.h>
#include <TrigLvl1.h>
#include <ErtOut.h>
#include <PHCentralTrack.h>
#include <emcClusterContainer.h>
#include <emcClusterContent.h>

#include <getClass.h>
#include <TriggerUtilities.h>
#include <TriggerHelper.h>


#include <cstdlib>
#include <fstream>
#include <iostream>

using namespace std;

TestSubsysReco::TestSubsysReco(
   const int run, const char* ofilename, const char *name)
{
   m_run_target = run;
   ThisName = name;
   m_ofilename = ofilename;

   return ;
}

int TestSubsysReco::Init(PHCompositeNode *topNode)
{
     if (verbosity > 0) cout << "Calling Init" << endl;
     // Create Histograms here - later you will have to do file magic
     // to make sure they are not deleted when the input file is closed
     m_file = new TFile(m_ofilename, "RECREATE");
     m_h1_ert_check = new TH1D("h1_ert_check", "", 10, 0, 10);
     m_h2_gl1_check = new TH2D("h2_gl1_check", "", 16, 0, 16, 10, 0, 10);
//     m_tree = new TTree("test_tree", "");
//     m_tree->Branch("run", &d_run, "run/I");
////     m_tree->Branch("evt", &d_evt, "evt/I");
////     m_tree->Branch("bbcz", &d_bbcz, "bbcz/F");
////     m_tree->Branch("bbct0", &d_bbct0, "bbct0/F");
//     m_tree->Branch("trig_4x4c", &d_trig_4x4c, "trig_4x4c/I");
//     m_tree->Branch("trig_4x4a", &d_trig_4x4a, "trig_4x4a/I");
//     m_tree->Branch("trig_bbc",  &d_trig_bbc,  "trig_bbc/I");

     return EVENT_OK;
}

int TestSubsysReco::InitRun(PHCompositeNode *topNode)
{
     if (verbosity > 0) {
	  recoConsts *rc = recoConsts::instance();
	  // this rc flag is set by the framework
	  cout << "Calling InitRun for Run"
	       << rc->get_IntFlag("RUNNUMBER") << endl;
     }
     return EVENT_OK;
}

int TestSubsysReco::process_event(PHCompositeNode *topNode)
{
     if (verbosity > 2) cout << "Calling process_event" << endl;

     ////
     //// TriggerHelper
     ////
     TriggerHelper trig_help(topNode);
     d_trig_bbc  = trig_help.trigScaled("BBCLL1(>0 tubes)");

     d_trig_4x4a = trig_help.trigLive("ERTLL1_4x4a&BBCLL1");
     d_trig_4x4b = trig_help.trigLive("ERTLL1_4x4b&BBCLL1");
     d_trig_4x4c = trig_help.trigLive("ERTLL1_4x4c&BBCLL1");
     d_trig_2x2  = trig_help.trigLive("ERTLL1_2x2&BBCLL1");

     if (! d_trig_bbc) return EVENT_OK;

     ////
     //// get nodes
     ////
     RunHeader* runheader = findNode::getClass<RunHeader>(topNode, "RunHeader");
     if (runheader == 0) {
        cout << "No RunHeader node.  Abort." << endl;
        exit(1);
     }
//     PHGlobal* phglobal = findNode::getClass<PHGlobal>(topNode, "PHGlobal");
//     if (phglobal == 0) {
//        cout << "No PHGlobal node.  Abort." << endl;
//        exit(1);
//     }
//     TrigLvl1* triglvl1 = findNode::getClass<TrigLvl1>(topNode, "TrigLvl1");
//     if (triglvl1 == 0) {
//        cout << "No TrigLvl1 node.  Abort." << endl;
//        exit(1);
//     }
     ErtOut* ertout = findNode::getClass<ErtOut>(topNode, "ErtOut");
     if (ertout == 0) {
        cout << "No ErtOut node.  Abort." << endl;
        exit(1);
     }
//     emcClusterContainer* emcclustercontainer = 
//       findNode::getClass<emcClusterContainer>(topNode, "emcClusterContainer");
//     if (emcclustercontainer == 0) {
//        cout << "No emcClusterContainer node.  Abort." << endl;
//        exit(1);
//     }

     ////
     //// data in PHGlobal
     ////
     d_run = runheader->get_RunNumber();
//     d_evt = phglobal->getEventNumber();
//     d_bbcz = phglobal->getBbcZVertex();
//     d_bbct0 = phglobal->getBbcTimeZero();

     ////
     //// event selection
     ////
//     if (fabs(d_bbcz) > 30) return EVENT_OK;
//     if (! d_trig_4x4c && ! d_trig_4x4a && ! d_trig_bbc) return EVENT_OK;

     ////
     //// ErtOut
     ////
     vector<int> as_ns_hit_4x4a;
     vector<int> as_ns_hit_4x4b;
     vector<int> as_ns_hit_4x4c;
     vector<int> as_ns_hit_2x2;
//     vector<int> as_ns_hit_rich;

     int nert = ertout->get_ERThit_N();
     for (int ihit = 0; ihit < nert; ihit++) {
        int arm  = ertout->get_ERTarm(ihit); // 0 = west, 1 = east
        int sect = ertout->get_ERTsector(ihit);
        int sm   = ertout->get_ERTsm(ihit);
        int armsect = 4 * arm + sect;
        int ns;
        if (armsect != 4 && armsect != 5) { // PbSc
           ns = (sm % 6) / 3;
        } else { // PbGl
           ns = (sm % 8) / 4;
        }
        int as_ns = 2 * armsect + ns;

        unsigned int trig_ert = ertout->get_ERTtrigmode(ihit);
        // trigmode 0 = 4x4a, 1 = 4x4b, 2 = 4x4c, 3 = 2x2, 4 = rich
        if (trig_ert == 0) {
           as_ns_hit_4x4a.push_back(as_ns);
        } else if (trig_ert == 1) {
           as_ns_hit_4x4b.push_back(as_ns);
        } else if (trig_ert == 2) {
           as_ns_hit_4x4c.push_back(as_ns);
        } else if (trig_ert == 3) {
           as_ns_hit_2x2.push_back(as_ns);
//        } else if (trig_ert == 4) {
//           as_ns_hit_rich.push_back(as_ns);
        }
     }

     ////
     //// GL1 check
     ////
     if (as_ns_hit_4x4a.size() == 1) {
        if (d_trig_4x4a) m_h2_gl1_check->Fill(as_ns_hit_4x4a[0], 0);
        else             m_h2_gl1_check->Fill(as_ns_hit_4x4a[0], 1);
     }
     if (as_ns_hit_4x4b.size() == 1) {
        if (d_trig_4x4b) m_h2_gl1_check->Fill(as_ns_hit_4x4b[0], 2);
        else             m_h2_gl1_check->Fill(as_ns_hit_4x4b[0], 3);
     }
     if (as_ns_hit_4x4c.size() == 1) {
        if (d_trig_4x4c) m_h2_gl1_check->Fill(as_ns_hit_4x4c[0], 4);
        else             m_h2_gl1_check->Fill(as_ns_hit_4x4c[0], 5);
     }
     if (as_ns_hit_2x2.size() == 1) {
        if (d_trig_2x2) m_h2_gl1_check->Fill(as_ns_hit_2x2[0], 6);
        else            m_h2_gl1_check->Fill(as_ns_hit_2x2[0], 7);
     }
//     if (as_ns_hit_rich.size() == 1) {
//        if (d_trig_rich) m_h2_gl1_check->Fill(as_ns_hit_rich[0], 8);
//        else             m_h2_gl1_check->Fill(as_ns_hit_rich[0], 9);
//     }

     ////
     //// ErtOut (emulation) check
     ////
     if (d_trig_4x4a) {
        if (as_ns_hit_4x4a.size() > 0) m_h1_ert_check->Fill(0);
        else                           m_h1_ert_check->Fill(1);
     }
     if (d_trig_4x4b) {
        if (as_ns_hit_4x4b.size() > 0) m_h1_ert_check->Fill(2);
        else                           m_h1_ert_check->Fill(3);
     }
     if (d_trig_4x4c) {
        if (as_ns_hit_4x4c.size() > 0) m_h1_ert_check->Fill(4);
        else                           m_h1_ert_check->Fill(5);
     }
     if (d_trig_2x2) {
        if (as_ns_hit_2x2.size() > 0) m_h1_ert_check->Fill(6);
        else                          m_h1_ert_check->Fill(7);
     }
//     if (d_trig_rich) {
//        if (as_ns_hit_rich.size() > 0) m_h1_ert_check->Fill(8);
//        else                           m_h1_ert_check->Fill(9);
//     }

     return EVENT_OK;
}

int TestSubsysReco::End(PHCompositeNode *topNode)
{
     if (verbosity > 0) cout << "Calling End" << endl;
     ////
     //// write out
     ////
     m_file->cd();
     m_file->Write();
     m_file->Close();

     return EVENT_OK;
}

int TestSubsysReco::Reset(PHCompositeNode *topNode)
{
     if (verbosity > 1) cout << "Calling Reset" << endl;
     return EVENT_OK;
}

int TestSubsysReco::ResetEvent(PHCompositeNode *topNode)
{
     if (verbosity > 2) cout << "Calling ResetEvent" << endl;
     return EVENT_OK;
}
