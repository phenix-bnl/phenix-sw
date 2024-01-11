#include "MakeClusterTree.h"
#include "Warnmap.h"

#include <RunHeader.h>
#include <EventHeader.h>
#include <PHGlobal.h>
#include <SpinDataEventOut.h>
#include <TrigLvl1.h>
#include <emcClusterContainer.h>
#include <emcClusterContent.h>
#include <TriggerUtilities.h>
#include <TriggerHelper.h>

#include <PHNodeIterator.h>
#include <PHTypedNodeIterator.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>

#include <Fun4AllReturnCodes.h>
#include <Fun4AllServer.h>
#include <recoConsts.h>
#include <getClass.h>

#include <TFile.h>
#include <TTree.h>
#include <TVector3.h>


#include <cassert>
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <vector>

using namespace std;

//static vector<int> vERTHit; //commented by me


MakeClusterTree::MakeClusterTree(
   const int run, const char* ofilename, const char* fname_warnmap, 
   const float nevt_max, const double mom_cut, const double trk_cut,
   const int bl_apply_warnmap, const int bl_with_partesum_etc, 
   const char *name)
{
   m_run_target = run;
   m_nevt_max = nevt_max;
   m_mom_cut = mom_cut;
   MIN_CUT = trk_cut;
   m_bl_apply_warnmap = bl_apply_warnmap;
   m_bl_with_partesum_etc = bl_with_partesum_etc;
   ThisName = name;

   m_ofilename = ofilename;

   if (m_bl_apply_warnmap) {
      m_warnmap = new Warnmap();
      m_warnmap->ReadMap(fname_warnmap);
   } else {
      m_warnmap = NULL;
   }
}

int MakeClusterTree::Init(PHCompositeNode *topNode)
{
     if (verbosity > 0) cout << "Calling Init" << endl;
     // Create Histograms here - later you will have to do file magic
     // to make sure they are not deleted when the input file is closed
     m_file = new TFile(m_ofilename, "RECREATE");
     m_tree = new TTree("cluster_tree", "");
     //// PHGlobal
     m_tree->Branch("run", &d_run, "run/I");
     m_tree->Branch("evt", &d_evt, "evt/I");
     m_tree->Branch("cross", &d_cross, "cross/I");
     m_tree->Branch("bbcz", &d_bbcz, "bbcz/F");
     m_tree->Branch("bbct0", &d_bbct0, "bbct0/F");

     m_tree->Branch("trigscaled", &d_trigscaled, "trigscaled/I");
     m_tree->Branch("trig_MPC_2x2", &d_trig_MPC_2x2, "trig_MPC_2x2/I");
     m_tree->Branch("trig_4x4c_bbc_narrow", &d_trig_4x4c_bbc_narrow, "trig_4x4c_bbc_narrow/I");
     m_tree->Branch("trig_4x4a", &d_trig_4x4a, "trig_4x4a/I");
     m_tree->Branch("trig_4x4b", &d_trig_4x4b, "trig_4x4b/I");
     m_tree->Branch("trig_bbc",  &d_trig_bbc,  "trig_bbc/I");
     m_tree->Branch("trig_bbc_narrow",  &d_trig_bbc_narrow,  "trig_bbc_narrow/I");
     m_tree->Branch("trig_novertex",  &d_trig_novertex,  "trig_novertex/I");

     //// emcClusterContainer
     m_tree->Branch("n_photon", &d_n_photon, "n_photon/I");
     m_tree->Branch("armsect", &d_armsect, "armsect[n_photon]/I");
     m_tree->Branch("ypos", &d_ypos, "ypos[n_photon]/I");
     m_tree->Branch("zpos", &d_zpos, "zpos[n_photon]/I");

     if (m_bl_with_partesum_etc) {
        m_tree->Branch("multiplicity", &d_multiplicity, "multiplicity[n_photon]/I");
        m_tree->Branch("multiplicity_total", &d_multiplicity_total, "multiplicity_total/I");
        m_tree->Branch("towerid", &d_towerid, "towerid[multiplicity_total]/I");
        m_tree->Branch("partesum", &d_partesum, "partesum[multiplicity_total]/F");
        m_tree->Branch("e", &d_e, "e[n_photon]/F");
     }
     m_tree->Branch("ecore", &d_ecore, "ecore[n_photon]/F");
     m_tree->Branch("ecent", &d_ecent, "ecent[n_photon]/F");
     m_tree->Branch("prob", &d_prob, "prob[n_photon]/F");
     m_tree->Branch("chi2", &d_chi2, "chi2[n_photon]/F");
     m_tree->Branch("x", &d_x, "x[n_photon]/F");
     m_tree->Branch("y", &d_y, "y[n_photon]/F");
     m_tree->Branch("z", &d_z, "z[n_photon]/F");
     m_tree->Branch("pc3dphi", d_pc3dphi, "pc3dphi[n_photon]/F");
     m_tree->Branch("pc3dz", d_pc3dz, "pc3dz[n_photon]/F");
     m_tree->Branch("tof", d_tof, "tof[n_photon]/F");
     m_tree->Branch("rawtdc", d_rawtdc, "rawtdc[n_photon]/F");

     //track
    //  m_tree->Branch("n_trkpart", &t_n_trkpart, "n_trkpart/I");

//      m_tree->Branch("trk_dcarm", t_dcarm, "trk_dcarm[n_trkpart]/I");
//      m_tree->Branch("trk_sect", t_sect, "trk_sect[n_trkpart]/I");
//      m_tree->Branch("trk_ysect", t_ysect, "trk_ysect[n_trkpart]/I");
//      m_tree->Branch("trk_zsect", t_zsect, "trk_zsect[n_trkpart]/I");
     
//      m_tree->Branch("trk_erthit", t_erthit, "trk_erthit[n_trkpart]/I");

//      m_tree->Branch("trk_pemcx", t_pemcx, "trk_pemcx[n_trkpart]/F");
//      m_tree->Branch("trk_pemcy", t_pemcy, "trk_pemcy[n_trkpart]/F");
//      m_tree->Branch("trk_pemcz", t_pemcz, "trk_pemcz[n_trkpart]/F");

//      m_tree->Branch("trk_dcphi", t_dcphi, "trk_dcphi[n_trkpart]/F");
//      m_tree->Branch("trk_zed", t_zed, "trk_zed[n_trkpart]/F");

//      m_tree->Branch("trk_ppc1x", t_ppc1x, "trk_ppc1x[n_trkpart]/F");
//      m_tree->Branch("trk_ppc1y", t_ppc1y, "trk_ppc1y[n_trkpart]/F");
//      m_tree->Branch("trk_ppc1z", t_ppc1z, "trk_ppc1z[n_trkpart]/F");

//      m_tree->Branch("trk_ppc3x", t_ppc3x, "trk_ppc3x[n_trkpart]/F");
//      m_tree->Branch("trk_ppc3y", t_ppc3y, "trk_ppc3y[n_trkpart]/F");
//      m_tree->Branch("trk_ppc3z", t_ppc3z, "trk_ppc3z[n_trkpart]/F");

//      m_tree->Branch("trk_mom", t_mom, "trk_mom[n_trkpart]/F");
//      m_tree->Branch("trk_ecore", t_ecore, "trk_ecore[n_trkpart]/F");
//      m_tree->Branch("trk_pT", t_pT, "trk_pT[n_trkpart]/F");
//      m_tree->Branch("trk_alpha", t_alpha, "trk_alpha[n_trkpart]/F");
//      m_tree->Branch("trk_the0", t_the0, "trk_the0[n_trkpart]/F");
//      m_tree->Branch("trk_phi0", t_phi0, "trk_phi0[n_trkpart]/F");
//      m_tree->Branch("trk_beta", t_beta, "trk_beta[n_trkpart]/F");

//      m_tree->Branch("trk_charge", t_charge, "trk_charge[n_trkpart]/I");
//      m_tree->Branch("trk_quality", t_quality, "trk_quality[n_trkpart]/I");
//      m_tree->Branch("trk_n0", t_n0, "trk_n0[n_trkpart]/F");
//      m_tree->Branch("trk_npe0", t_npe0, "trk_npe0[n_trkpart]/F");
//      m_tree->Branch("trk_n1", t_n1, "trk_n1[n_trkpart]/F");
//      m_tree->Branch("trk_chi2", t_chi2, "trk_chi2[n_trkpart]/F");
//      m_tree->Branch("trk_dep", t_dep, "trk_dep[n_trkpart]/F");
//      m_tree->Branch("trk_disp", t_disp, "trk_disp[n_trkpart]/F");
//      m_tree->Branch("trk_prob", t_prob, "trk_prob[n_trkpart]/F");

//      m_tree->Branch("trk_emcdphi", t_emcdphi, "trk_emcdphi[n_trkpart]/F");
//      m_tree->Branch("trk_emcdz", t_emcdz, "trk_emcdz[n_trkpart]/F");

//      m_tree->Branch("trk_pc3dphi", t_pc3dphi, "trk_pc3dphi[n_trkpart]/F");
//      m_tree->Branch("trk_pc3dz", t_pc3dz, "trk_pc3dz[n_trkpart]/F");
     //commented up to here by me

     //m_tree->Branch("trk_emcsdphi", t_emcsdphi, "trk_emcsdphi[n_trkpart]/F");
     //m_tree->Branch("trk_emcsdz", t_emcsdz, "trk_emcsdz[n_trkpart]/F");
     //m_tree->Branch("trk_emcsdphi_e", t_emcsdphi_e, "trk_emcsdphi_e[n_trkpart]/F");
     //m_tree->Branch("trk_emcsdz_e", t_emcsdz_e, "trk_emcsdz_e[n_trkpart]/F");

     //m_tree->Branch("trk_flag", t_flag, "trk_flag[n_trkpart]/I");

     m_nevt_processed = 0.0;

     return EVENT_OK;
}

int MakeClusterTree::InitRun(PHCompositeNode *topNode)
{
     if (verbosity > 0) {
	  recoConsts *rc = recoConsts::instance();
	  // this rc flag is set by the framework
	  cout << "Calling InitRun for Run"
	       << rc->get_IntFlag("RUNNUMBER") << endl;
     }
     return EVENT_OK;
}

int MakeClusterTree::process_event(PHCompositeNode *topNode)
{
     if (verbosity > 2) cout << "Calling process_event" << endl;

     if (m_nevt_max > 0 && m_nevt_processed >= m_nevt_max) return EVENT_OK;

     ////
     //// get nodes
     ////
     RunHeader* runheader = findNode::getClass<RunHeader>(topNode, "RunHeader");
     assert(runheader);
     PHGlobal* phglobal = findNode::getClass<PHGlobal>(topNode, "PHGlobal");
     assert(phglobal);
     EventHeader *evthead = findNode::getClass<EventHeader>(topNode, "EventHeader");
     assert(evthead);
     TrigLvl1* triglvl1 = findNode::getClass<TrigLvl1>(topNode, "TrigLvl1");
     assert(triglvl1);
     //ErtOut* ertout = findNode::getClass<ErtOut>(topNode, "ErtOut"); //commented by me
     emcClusterContainer *emcclustercontainer = 
       findNode::getClass<emcClusterContainer>(topNode, "emcClusterContainer");
     assert(emcclustercontainer);
     //PHCentralTrack *cnttrack //commented by me
     // = findNode::getClass<PHCentralTrack>(topNode, "PHCentralTrack");
     
     // if (cnttrack == 0) { //commented by me
//        cout << PHWHERE
// 	    << "PHCentralTrack Node missing.  Abort."
// 	    << endl;
//      }


     ////
     //// TriggerHelper
     ////
     d_trigscaled = triglvl1->get_lvl1_trigscaled();
     TriggerHelper trig_help(topNode);

     //new run 11 stuff:
     d_trig_MPC_2x2 = trig_help.trigScaled("MPC4x4c&ERTLL1_2x2");
     d_trig_4x4c_bbc_narrow = trig_help.trigScaled("ERTLL1_4x4c&BBCLL1(narrow)");
     d_trig_4x4a = trig_help.trigScaled("ERTLL1_4x4a");
     d_trig_4x4b = trig_help.trigScaled("ERTLL1_4x4b");
     d_trig_bbc  = trig_help.trigScaled("BBCLL1(>0 tubes)");
     d_trig_novertex  = trig_help.trigScaled("BBCLL1(>0 tubes) novertex");
     d_trig_bbc_narrow  = trig_help.trigScaled("BBCLL1(>0 tubes) narrowvtx");

     //old run 9 stuff:
    //  d_trig_2x2 = trig_help.trigScaled("ERTLL1_2x2&BBCLL1");
//      d_trig_4x4c = trig_help.trigScaled("ERTLL1_4x4c&BBCLL1");
//      d_trig_4x4a = trig_help.trigScaled("ERTLL1_4x4a");
//      d_trig_4x4b = trig_help.trigScaled("ERTLL1_4x4b");
//      d_trig_bbc  = trig_help.trigScaled("BBCLL1(>0 tubes)");
//      d_trig_novertex  = trig_help.trigScaled("BBCLL1(noVertexCut)");
     //if( d_trig_4x4a && d_trig_bbc ) { d_trig_4x4a = 1; }
     //else { d_trig_4x4a = 0; }
     /*
     d_trig_4x4c = trig_help.trigScaled("ERTLL1_4x4c&BBCLL1");
     d_trig_4x4a = trig_help.trigScaled("ERTLL1_4x4a&BBCLL1");
     d_trig_bbc  = trig_help.trigScaled("BBCLL1(>0 tubes)");
     */
     ////
     //// data in PHGlobal
     ////
     d_run = runheader->get_RunNumber();
     //d_evt = phglobal->getEventNumber();
     d_evt = evthead->get_EvtSequence();
     d_cross = triglvl1->get_lvl1_clock_cross();
     d_bbcz = phglobal->getBbcZVertex();
     d_bbct0 = phglobal->getBbcTimeZero();

     ////
     //// event selection
     ////
     //if (fabs(d_bbcz) > 30) return EVENT_OK;
     if ( !(d_trig_MPC_2x2) && !(d_trig_4x4c_bbc_narrow) && !(d_trig_4x4a) && !(d_trig_4x4b)
	  && !(d_trig_bbc) && !(d_trig_novertex) && !(d_trig_bbc_narrow)) return EVENT_OK;

     //ERT trigger //commented by me
    //  vERTHit.clear();
//      int nERT = ertout->get_ERThit_N();
//      for(int i=0; i<nERT; i++)
//        {
// 	 int ert_arm = ertout->get_ERTarm(i);
// 	 int ert_sect = ertout->get_ERTsector(i);
// 	 int ert_sm = ertout->get_ERTsm(i);
// 	 int ERThit = 128*(1-ert_arm) + 32*ert_sect + ert_sm;
// 	 int ert_trigmode = ertout->get_ERTtrigmode(i);
// 	 if( ert_trigmode>=0 && ert_trigmode<=2 )
// 	   {
// 	     vERTHit.push_back(ERThit);
// 	   }
//        }

     ////
     //// data in emcClusterContainer
     ////
     Int_t n_clus = emcclustercontainer->size();
     Int_t iph = 0;
     Int_t multi_total = 0;

     bool exceeded_some_limit = false;
     for (Int_t iclus = 0; iclus < n_clus; iclus++) {
	  emcClusterContent* clus = emcclustercontainer->getCluster(iclus);

          int armsect = clus->arm() * 4 + clus->sector();
          int ypos = clus->iypos();
          int zpos = clus->izpos();

	  if (1/*clus->prob_photon() > 0.02*/ && //commented by me
	      clus->ecore() > m_mom_cut &&
	      (! m_bl_apply_warnmap || 
	       ! m_warnmap->IsBad(armsect, ypos, zpos) ) )
	    {
	      
	      d_armsect[iph] = armsect;
	      d_ypos[iph]    = ypos;
	      d_zpos[iph]    = zpos;
	      
	      d_ecore[iph] = clus->ecore();
	      d_ecent[iph] = clus->ecent();
	      d_prob[iph] = clus->prob_photon();
	      d_chi2[iph] = clus->chi2();
	      d_x[iph] = clus->x();
	      d_y[iph] = clus->y();
	      //d_z[iph] = clus->z() - d_bbcz;
	      d_z[iph] = clus->z();

	      d_pc3dphi[iph] = clus->emcpc3dphi();
	      d_pc3dz[iph] = clus->emcpc3dz();

	      //d_tof[iph] = clus->tof();
	      d_tof[iph] = clus->tofcorr();
	      d_rawtdc[iph] = clus->rawtdc();
	      
	      if (m_bl_with_partesum_etc)
		{
		  d_multiplicity[iph] = clus->multiplicity();


		  
		  for (Int_t imul = 0; imul < d_multiplicity[iph]; imul++)
		    {
		      d_towerid[multi_total] = clus->towerid(imul);
		      d_partesum[multi_total] = clus->partesum(imul);
		      multi_total++;
		      if( multi_total>=max_multiplicity_total ){
			cout<<"next message about max photon is a lie, it was actually max_multiplicity that was exceeded"<<endl;
			exceeded_some_limit = true;
			break;
		      }
		    }
		  
		  d_e[iph] = clus->e();
		}
	      
	      iph++;
	      if( (iph>=max_n_photon) || (multi_total>=max_multiplicity_total) ){
		cout<<"exceeded max photon"<<endl;
		exceeded_some_limit = true;
		break;
	      }
	    }//end if

     }//end emc cluster loop
     //photon
     if(exceeded_some_limit){
       d_n_photon = 0;
     } else{ 
       d_n_photon = iph;
     }

     
     if (m_bl_with_partesum_etc){
       if(exceeded_some_limit){
	 d_multiplicity_total = 0;
       } else{
	 d_multiplicity_total = multi_total;
       }
     }

     //central track
     //start comment here by me
     // Int_t npart = cnttrack->get_npart();
//      Int_t itrack_part = 0;
		 
//      for(Int_t ipart=0; ipart<npart; ipart++)
//        {
// 	 //momentum cut
// 	 if( cnttrack->get_mom(ipart)>MIN_CUT &&
// 	     cnttrack->get_quality(ipart)>7 )
// 	   {
// 	     t_dcarm[itrack_part] = cnttrack->get_dcarm(ipart);
// 	     t_sect[itrack_part] = cnttrack->get_sect(ipart);
// 	     t_ysect[itrack_part] = cnttrack->get_ysect(ipart);
// 	     t_zsect[itrack_part] = cnttrack->get_zsect(ipart);

// 	     int trk_sm = SectToSM( t_dcarm[itrack_part], t_sect[itrack_part],
// 				    t_ysect[itrack_part], t_zsect[itrack_part] );
// 	     int trk_ERThit
// 	       = 128*t_dcarm[itrack_part] + 32*t_sect[itrack_part] + trk_sm;
// 	     for(unsigned int i=0; i<vERTHit.size(); i++)
// 	       {
// 		 int diff_ERTm = vERTHit[i] - trk_ERThit;

// 		 if( abs(diff_ERTm)<2 ) { t_erthit[itrack_part] = 1; }
// 		 else { t_erthit[itrack_part] = -1; }
// 	       }

// 	     t_pemcx[itrack_part] = cnttrack->get_pemcx(ipart);
// 	     t_pemcy[itrack_part] = cnttrack->get_pemcy(ipart);
// 	     t_pemcz[itrack_part] = cnttrack->get_pemcz(ipart);

// 	     t_dcphi[itrack_part] = cnttrack->get_phi(ipart);
// 	     t_zed[itrack_part] = cnttrack->get_zed(ipart);

// 	     t_ppc1x[itrack_part] = cnttrack->get_ppc1x(ipart);
// 	     t_ppc1y[itrack_part] = cnttrack->get_ppc1y(ipart);
// 	     t_ppc1z[itrack_part] = cnttrack->get_ppc1z(ipart);

// 	     t_ppc3x[itrack_part] = cnttrack->get_ppc3x(ipart);
// 	     t_ppc3y[itrack_part] = cnttrack->get_ppc3y(ipart);
// 	     t_ppc3z[itrack_part] = cnttrack->get_ppc3z(ipart);

// 	     t_mom[itrack_part] = cnttrack->get_mom(ipart);
// 	     t_ecore[itrack_part] = cnttrack->get_ecore(ipart);
// 	     t_alpha[itrack_part] = cnttrack->get_alpha(ipart);
// 	     t_the0[itrack_part] = cnttrack->get_the0(ipart);
// 	     t_phi0[itrack_part] = cnttrack->get_phi0(ipart);
// 	     t_beta[itrack_part] = cnttrack->get_beta(ipart);
	     
// 	     t_pT[itrack_part] = cnttrack->get_mom(ipart) * sin( cnttrack->get_the0(ipart) );

// 	     t_charge[itrack_part] = cnttrack->get_charge(ipart);
// 	     t_quality[itrack_part] = cnttrack->get_quality(ipart);
// 	     t_n0[itrack_part] = cnttrack->get_n0(ipart);
// 	     t_npe0[itrack_part] = cnttrack->get_npe0(ipart);
// 	     t_n1[itrack_part] = cnttrack->get_n1(ipart);
// 	     t_chi2[itrack_part] = cnttrack->get_chi2(ipart);
// 	     t_dep[itrack_part] = cnttrack->get_dep(ipart);
// 	     t_disp[itrack_part] = cnttrack->get_disp(ipart);
// 	     t_prob[itrack_part] = cnttrack->get_prob(ipart);
	     
// 	     t_emcdphi[itrack_part] = cnttrack->get_emcdphi(ipart);
// 	     t_emcdz[itrack_part] = cnttrack->get_emcdz(ipart);

// 	     t_pc3dphi[itrack_part] = cnttrack->get_pc3dphi(ipart);
// 	     t_pc3dz[itrack_part] = cnttrack->get_pc3dz(ipart);

// 	     //t_emcsdphi[itrack_part] = cnttrack->get_emcsdphi(ipart);
// 	     //t_emcsdz[itrack_part] = cnttrack->get_emcsdz(ipart);
// 	     //t_emcsdphi_e[itrack_part] = cnttrack->get_emcsdphi_e(ipart);
// 	     //t_emcsdz_e[itrack_part] = cnttrack->get_emcsdz_e(ipart);

// 	     //flag
// 	     /*
// 	     if( fabs(cnttrack->get_emcsdphi(ipart))<3.
// 		 && fabs(cnttrack->get_emcsdz(ipart))<3.
// 		 && fabs(cnttrack->get_pc3sdphi(ipart))<3.
// 		 && fabs(cnttrack->get_pc3sdz(ipart))<3. )
// 	       {
// 		 t_flag[itrack_part] = -1; // emc and pc3 match

// 	       }else if( fabs(cnttrack->get_emcsdphi(ipart))<3.
// 			 && fabs(cnttrack->get_emcsdz(ipart))<3. ) {

// 		 t_flag[itrack_part] = -2; // emc match

// 	       }else if( fabs(cnttrack->get_pc3sdphi(ipart))<3.
// 			 && fabs(cnttrack->get_pc3sdz(ipart))<3. ) {
		
// 		 t_flag[itrack_part] = -3; // pc3 match

// 	       }else{
// 		 t_flag[itrack_part] = -4; // else
// 	       }
// 	     */
// 	     itrack_part++;
// 	   }//end if for central track
//        }//end central track loop
     
//      t_n_trkpart = itrack_part;
//end comment here by me
     
     m_tree->Fill();
     if((Long_t)m_nevt_processed%1000==0){
       m_file = m_tree->GetCurrentFile();
       m_file->Write();
     }
     //trk_tree->Fill();
     m_nevt_processed += 1.0;
     return EVENT_OK;
}

int MakeClusterTree::End(PHCompositeNode *topNode)
{
     if (verbosity > 0) cout << "Calling End" << endl;
     ////
     //// write out
     ////
     m_file = m_tree->GetCurrentFile();
     m_file->Write();
     m_file->Close();

     return EVENT_OK;
}

int MakeClusterTree::Reset(PHCompositeNode *topNode)
{
     if (verbosity > 1) cout << "Calling Reset" << endl;
     return EVENT_OK;
}

int MakeClusterTree::ResetEvent(PHCompositeNode *topNode)
{
     if (verbosity > 2) cout << "Calling ResetEvent" << endl;
     return EVENT_OK;
}

int MakeClusterTree::SectToSM(int dcarm, int sect, int ysect, int zsect)
{
  int sm = 0;
  if( 4*dcarm+sect>=2 )
    {
      //PbSc
      sm = (ysect/12)*6 + zsect/12;

    }else{
      //PbGl
      sm = (ysect/12)*8 + zsect/12;
    }

  return sm;
}


