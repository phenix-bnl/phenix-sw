#include "NhitTree.h"

#include <TFile.h>
#include <TH3D.h>
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
#include <PHCentralTrack.h>
#include <emcClusterContainer.h>
#include <emcClusterContent.h>

#include <getClass.h>
#include <TriggerUtilities.h>
#include <TriggerHelper.h>

#include <iostream>
#include <fstream>


using namespace std;
using namespace EmcAnaCommon;

NhitTree::NhitTree(
   const int run, const char* ofilename, const char *name
   )
{
     ThisName = name;
     m_run = run;
     //     m_ofilename = GetEnvSure("NHIT_DATA_FILE_NAME");
     m_ofilename = ofilename;
}

NhitTree::~NhitTree()
{
  ;
}

int NhitTree::Init(PHCompositeNode *topNode)
{
  
  
  
  double pos_array[100];
  for (int i = 0; i < 100; i++)
    pos_array[i] = i;
  
  if (verbosity > 0) cout << "Calling Init" << endl;
  // Create Histograms here - later you will have to do file magic
  // to make sure they are not deleted when the input file is closed
  cout<<"N_ARMSECT: "<<N_ARMSECT<<endl;
  for (Int_t ias = 0; ias < N_ARMSECT; ias++){
    char hname[256];
    sprintf(hname, "nhit_run%i_as%i", m_run, ias);
    
    int ny_max, nz_max;
    if (IsPbGl(ias)) { ny_max = N_YPOS_PBGL;  nz_max = N_ZPOS_PBGL; }
    else             { ny_max = N_YPOS_PBSC;  nz_max = N_ZPOS_PBSC; }
    
    m_h3_nhit[ias] = new TH3D(hname, "", nz_max, pos_array, ny_max, pos_array,
			      N_ECORE_RANGE_FOR_NHIT_HIST, ECORE_RANGE_FOR_NHIT_HIST);
  }
  
  return EVENT_OK;
}

int NhitTree::InitRun(PHCompositeNode *topNode)
{
     if (verbosity > 0) {
	  recoConsts *rc = recoConsts::instance();
	  // this rc flag is set by the framework
	  cout << "Calling InitRun for Run"
	       << rc->get_IntFlag("RUNNUMBER") << endl;
     }
     return EVENT_OK;
}

int NhitTree::process_event(PHCompositeNode *topNode)
{
     if (verbosity > 2) cout << "Calling process_event" << endl;

     ////
     //// get nodes
     ////
     RunHeader* runheader = findNode::getClass<RunHeader>(topNode, "RunHeader");
     PHGlobal*  phglobal  = findNode::getClass<PHGlobal> (topNode, "PHGlobal");
     TrigLvl1*  triglvl1  = findNode::getClass<TrigLvl1> (topNode, "TrigLvl1");
     emcClusterContainer *emcclustercontainer = 
       findNode::getClass<emcClusterContainer>(topNode, "emcClusterContainer");
     if (runheader == 0 || phglobal == 0 || emcclustercontainer == 0) {
	  cout << PHWHERE
	       << "RunHeader/PHGlobal/emcClusterContainer Node missing.  Abort."
	       << endl;
     }

     if (triglvl1 == 0) {
	  cout << PHWHERE
	       << "TrigLvl1 Node missing.  Abort."
	       << endl;
     }

     ////
     //// TriggerHelper
     ////
     TriggerHelper trig_help(topNode);
     Int_t trig_4x4c_bbc_narrow = trig_help.didLevel1TriggerFire("ERTLL1_4x4c&BBCLL1(narrow)");

     ////
     //// data in PHGlobal
     ////
     int d_run = runheader->get_RunNumber();
//     int d_evt = phglobal->getEventNumber();
     float d_bbcz = phglobal->getBbcZVertex();
//     int d_bbct0 = phglobal->getBbcTimeZero();

     if (d_run != m_run) {
	  cout << "Event with different run number is included!!\n";
          cout << "should be " << d_run << " got " << m_run << endl;
	  return EVENT_OK;
     }

     if (fabs(d_bbcz) > 30 || (! trig_4x4c_bbc_narrow)) return EVENT_OK;

     ////
     //// data in emcClusterContainer
     ////
     Int_t n_clus = emcclustercontainer->size();
     for (Int_t iclus = 0; iclus < n_clus; iclus++) {
	  emcClusterContent* clus = emcclustercontainer->getCluster(iclus);
	  if (1/*clus->prob_photon() > 0.01*/) {//commented by me

               int d_armsect = clus->arm() * 4 + clus->sector();
               int d_ypos    = clus->iypos();
               int d_zpos    = clus->izpos();
	       float d_ecore = clus->ecore();
	       //if(d_armsect == 2 &&
	       //d_ypos >= 24 && d_ypos <36 &&
	       // d_zpos >= 24 && d_zpos <36 ){
		 //cout<<"there is a hit in sector2, sm14, and it has ecore: "<<d_ecore<<endl;
	       //}
               m_h3_nhit[d_armsect]->Fill(d_zpos, d_ypos, d_ecore);
	  }
     }

     return EVENT_OK;
}

int NhitTree::End(PHCompositeNode *topNode)
{
  m_file = new TFile(m_ofilename, "RECREATE");//moved from Init by me
  if (verbosity > 0) cout << "Calling End" << endl;
     ////
     //// write out
     ////
     m_file->cd();
     for(int ii = 0; ii<8; ii++){//added by me
       m_h3_nhit[ii]->Write();
     }
     m_file->Write(); 
     m_file->Close();
     cout<<"NhitTree ended!"<<endl;
     return EVENT_OK;
}

int NhitTree::Reset(PHCompositeNode *topNode)
{
     if (verbosity > 1) cout << "Calling Reset" << endl;
     return EVENT_OK;
}

int NhitTree::ResetEvent(PHCompositeNode *topNode)
{
     if (verbosity > 2) cout << "Calling ResetEvent" << endl;
     return EVENT_OK;
}
