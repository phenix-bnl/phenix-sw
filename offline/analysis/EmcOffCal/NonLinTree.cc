#include "NonLinTree.h"
#include "Warnmap.h"

#include <TFile.h>
#include <TH2D.h>
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


#include <cstdlib>
#include <iostream>
#include <sstream>

using namespace std;

NonLinTree::NonLinTree(
   const int run, const char* ofilename, const char* fn_warnmap, const char *name )
{
   m_run       = run;
   m_ofilename = ofilename;
   ThisName    = name;

   m_warnmap = new Warnmap();
   m_warnmap->ReadMap(fn_warnmap);

   ostringstream oss;

   m_file = new TFile(m_ofilename.c_str(), "RECREATE");
   for (Int_t ias = 0; ias < EmcAnaCommon::N_ARMSECT; ias++){
      oss.str("");
      oss << "h2_pi0mass_as" << ias;
      m_h2_pi0mass[ias] = new TH2D(oss.str().c_str(), "",
                                   N_BIN_MOM, MOM_MIN, MOM_MAX,
                                   N_BIN_MASS, MASS_MIN, MASS_MAX );
   }
}

NonLinTree::~NonLinTree()
{
   delete m_warnmap;
}

int NonLinTree::process_event(PHCompositeNode *topNode)
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
     if (runheader == 0 || phglobal == 0 || 
         emcclustercontainer == 0 || triglvl1 == 0) {
	  cout << PHWHERE
	       << "RunHeader/PHGlobal/emcClusterContainer/TrigLvl1 Node missing.  Abort."
	       << endl;
          exit(1);
     }

     ////
     //// TriggerHelper
     ////
     TriggerHelper trig_help(topNode);
     Int_t trig_4x4c = trig_help.didLevel1TriggerFire("ERTLL1_4x4c&BBCLL1");

     ////
     //// data in PHGlobal
     ////
     int run = runheader->get_RunNumber();
     float bbcz = phglobal->getBbcZVertex();
//     int d_bbct0 = phglobal->getBbcTimeZero();

     if (run != m_run) {
        cout << "Event with different run number (" << run << ") is included!!" << endl
             << "Abort." << endl;
          exit(1);
     }
     if (fabs(bbcz) > 30 || (! trig_4x4c)) return EVENT_OK;

     ////
     //// data in emcClusterContainer
     ////
     Int_t n_clus = emcclustercontainer->size();
     for (Int_t ic = 0; ic < n_clus; ic++) {
        emcClusterContent* iclus = emcclustercontainer->getCluster(ic);
        double prob_i  = iclus->prob_photon();
        double ene_i   = iclus->ecore();
        int as_i       = iclus->arm() * 4 + iclus->sector();
        int y_i        = iclus->iypos();
        int z_i        = iclus->izpos();
        if (prob_i > 0.01 && ene_i > ECORE_MIN && ! m_warnmap->IsBad(as_i, y_i, z_i) ) {

           for (Int_t jc = ic + 1; jc < n_clus; jc++) {
              emcClusterContent* jclus = emcclustercontainer->getCluster(jc);
              double prob_j  = jclus->prob_photon();
              double ene_j   = jclus->ecore();
              int as_j       = jclus->arm() * 4 + jclus->sector();
              int y_j        = jclus->iypos();
              int z_j        = jclus->izpos();

              double ene_asym = fabs(ene_i - ene_j) / (ene_i + ene_j);
              if (prob_j > 0.01 && ene_j > ECORE_MIN && ! m_warnmap->IsBad(as_j, y_j, z_j)
                  && as_i == as_j && ene_asym < ENE_ASYM_MAX ) {

                 TVector3 mom_i, mom_j;
                 mom_i.SetXYZ(iclus->x(), iclus->y(), iclus->z() - bbcz);
                 mom_j.SetXYZ(jclus->x(), jclus->y(), jclus->z() - bbcz);
                 mom_i.SetMag(ene_i);
                 mom_j.SetMag(ene_j);

                 double mom  = (mom_i + mom_j).Mag();
                 double mass = sqrt( pow(ene_i + ene_j, 2) - mom * mom );
                 m_h2_pi0mass[as_i]->Fill(mom, mass);
              }
           }
        }
     }
     
     return EVENT_OK;
}

int NonLinTree::End(PHCompositeNode *topNode)
{
     if (verbosity > 0) cout << "Calling End" << endl;

     m_file->cd();
     m_file->Write();
     m_file->Close();

     return EVENT_OK;
}
