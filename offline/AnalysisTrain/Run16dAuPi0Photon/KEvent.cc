//#include <stdio.h>
#include <iostream>

#include "TObject.h"
#include "PHCentralTrack.h"
#include "emcClusterContainer.h"
#include "emcClusterContent.h"
#include "mEmcGeometryModule.h"
#include "ErtOut.h"
#include "KCluster.h"
#include "KEvent.h"
#include "TH2F.h"

//#include "HotTwrList.h" Run7
//#include "TowerHelper.h"
//#include "Warnmap.h" // Run8

using namespace std;

KEvent::KEvent() :
  g_multiplicity(0), centralityBin(-1), theta(-1.0), reacBin(-1), trigger_photon(-1)
{
  //TwrVeto = NULL;
//  TwrVeto = new Warnmap();    // Run8
//  TwrVeto->ReadMap("/direct/phenix+hp/data10/ondrejch/ThermalPhotonRun8/src/map/map.kensuke.Run8.txt"); // dAu Run 8 - preliminary one from Kensuke
//  TwrVeto->ReadMap("/direct/phenix+scratch/wholz/EmCal/daucalib/map/warnmap.txt");   // Run8 (used for calibration)
  //Run7 TwrVeto = new  HotTwrList("/direct/phenix+scratch/wholz/EmCal/daucalib/map/warnmap.txt");
  //TwrHlp = new TowerHelper();
  ntrig = 0;
}

KEvent::~KEvent() {
  clear();
//  if(TwrVeto) delete TwrVeto;
//  if(TwrHlp) delete TwrHlp; Run7
}

const KCluster* KEvent::cluster_element(unsigned int n) const
{
  if ( n >= clusterVector.size() ) return 0;
  return clusterVector[n].get();
}

// Remove the trigger photon from event
void KEvent::removeTrigger()
{
  int n = trigger_photon;
  if ( n > -1 )
    {
      if ( (unsigned int)n < clusterVector.size() )
	{
	  clusterVector.erase(clusterVector.begin()+n);
	  g_multiplicity--;
	}
    }
}

// Clear contents of the event.
void KEvent::clear()
{
  clusterVector.clear();
  g_multiplicity = 0;
}

int KEvent::getEvent( emcClusterContainer *emccont, PHCentralTrack* phCentralTrack_ptr,
		  float bbcT0, float bbcVz, mEmcGeometryModule *EmcGeo,
		  THmulf *gamma1, THmulf *gammarap, THmulf *tower, THmulf *towertof,THmulf* towertof_3ns, ErtOut *ertout, int inrunn,
		      TNtuple *gnt, int is_bookNt, float inPtcut,int ERTb,
                  //int _status_array[2][4][48][96],
                  //float _low_limit_array[2][4][48][96],
                  //float _high_limit_array[2][4][48][96],
                  //float bbcqn, float bbcqs, int nevents,TH1F *cluster_counter )
                  float bbcqn, float bbcqs, int nevents,TH1F *cluster_counter,
				  TH2F *bbcqs_ecoreMax, TH2F *bbcqs_ecoreTotal, TH2F *bbcqs_ecoreAverage,
				  TH2F *bbcqn_ecoreMax, TH2F *bbcqn_ecoreTotal, TH2F *bbcqn_ecoreAverage, THmulf *cent_fw_corr)
//		  TNtuple *gnt, int is_bookNt)
//		  int inrunn, TNtuple *gnt, int is_bookNt)
{

  int nclusters = emccont->size();

  const static unsigned int sccut3x3Map = 0x1ce70;
  const static unsigned int glcut3x3Map = 0x1ce70;
  const static unsigned int tightsccut3x3Map = 0x7de1ce70;
  const static unsigned int tightglcut3x3Map = 0x7de1ce70;

  unsigned int cut3x3Map;
  unsigned int tightcut3x3Map;

  trigger_photon = -1;  // initialize index of trigger photon to -1

  clear();

  float ET_02_ecore=0.0;
  float ET_00_ecore=0.0;
  float ET_02_e=0.0;
  float ET_00_e=0.0;
		//cout << "ncluster = " << nclusters << endl;
  for(int j=0; j<nclusters; j++)
    {
      emcClusterContent *emc = emccont->getCluster(j);

      if ( !emc ) {std::cout << "oh god, emc == 0!" << std::endl; return -1;}

      ET_00_ecore += emc->ecore();
      ET_00_e += emc->e();
//	  cout << "sector = " << emc->sector() << endl;
        if (emc->e() > 0.4) //&& emc->deadmap()==0 && emc->warnmap()==0 // &&emc->sector()<6.0)
	  //if (emc->ecore() > 0.3) //&& emc->deadmap()==0 && emc->warnmap()==0 // &&emc->sector()<6.0)
        {
        ET_02_ecore += emc->ecore();
        ET_02_e += emc->e();

	}
    }


  int flag_deadwarn_event=0;

  for(int j=0; j<nclusters; j++)
    {
      emcClusterContent *emc = emccont->getCluster(j);

      if ( !emc ) { std::cout << "oh god, emc == 0!" << std::endl; return -1;}

      if (emc->e() > 0.4)// && emc->deadmap()==0 && emc->warnmap()==0)
      //  if (emc->ecore() > 0.3)// && emc->deadmap()==0 && emc->warnmap()==0)
	{

//	  float tofEshift;
	  float corrtof, ecent;
	  ecent = emc->ecent();
	  corrtof = emc->tofcorr() - bbcT0;
	  // if(corrtof>-40&&corrtof<80)
	  //std::cout<<"\n In Event.cc "<<corrtof<<"\t"<<emc->tofcorr();

//	  cluster_counter->Fill(corrtof);
//cout << ecent << endl << corrtof<< endl << "======================="  << endl;
	  KCluster* c = new KCluster();

          // Some global things
          c->set_nevents(nevents);
          c->set_bbcqn(bbcqn);
          c->set_bbcqs(bbcqs);

          c->set_et_00_ecore(ET_00_ecore);
          c->set_et_02_ecore(ET_02_ecore);
          c->set_et_00_e(ET_00_e);
          c->set_et_02_e(ET_02_e);

	  c->set_xyz(emc->x(),emc->y(),emc->z());
	  c->set_e(emc->e());
	  c->set_arm(emc->arm());
	  c->set_sector(emc->sector());
	  c->set_ipos(emc->iypos(),emc->izpos());

          //
          // Here, Dennis's TOF cut code
          //
          /*int arms = emc->arm();
          int sec = emc->sector();
          int iypos = emc->iypos();
          int izpos = emc->izpos();

          int tof_status = _status_array[arms][sec][iypos][izpos];
          float tof_low_limit = _low_limit_array[arms][sec][iypos][izpos];
          float tof_high_limit = _high_limit_array[arms][sec][iypos][izpos];
          float tofs = 0;//emc->tof() - bbcT0;

          float x = emc->x();
          float y = emc->y();
          float z = emc->z() - bbcVz;
          float theta = acos(z / sqrt(x*x+y*y+z*z));
          float pt = emc->ecore() * sin(theta) ;

          int tofstat = 1;
          if (tof_status == 1 || tof_status == 4) tofstat=0;
          if (tof_status == 2 && pt < 1) tofstat=0;
          if ( (tofs < tof_low_limit) || (tofs > tof_high_limit) ) tofstat=0;*/
          //
          // Till here, Dennis's TOF cut code
          //

       /// Setting ERT bits
          int nsm, ert4x4abit, ert4x4bbit, ert4x4cbit;
          if(emc->arm()==0||emc->sector()>=2){ // PbSc
             nsm = (emc->iypos()/12)*6+(emc->izpos()/12);
           } else { // PbGl
             nsm = (emc->iypos()/12)*8+(emc->izpos()/12);
           }
           ert4x4abit = ertout->get_ERTbit(0,emc->arm(),emc->sector(),nsm);
           ert4x4bbit = ertout->get_ERTbit(1,emc->arm(),emc->sector(),nsm);
           ert4x4cbit = ertout->get_ERTbit(2,emc->arm(),emc->sector(),nsm);
           c->setErtBit(ert4x4abit,ert4x4bbit,ert4x4cbit);

		//   flag of ERT events triggered by dead warn towers
	   if((ert4x4abit==1||ert4x4bbit==1||ert4x4cbit==1)&&emc->deadmap()!=0 && emc->warnmap()!=0)
	     flag_deadwarn_event=1;
  	// printf("dead tower ERT %dth cluster ecore = %lf\tert4x4 a/b/c bit = %d\t%d\t%d\n",j,emc->ecore(),ert4x4abit,ert4x4bbit,ert4x4cbit);
          // They are for PbGl dispersion cut
          c->set_theta(emc->theta()); // WGH: not in CWG
          c->set_corrdisp(emc->corrdispy(),emc->corrdispz());

	  c->set_multiplicity(emc->multiplicity());
	  c->set_ecore(emc->ecore());
	  // They are new ecore definition
          if(emc->multiplicity()<=4) c->set_ecore_newe(emc->e());
          else c->set_ecore_newe(emc->ecore());

	  c->set_ecent(emc->ecent());
	  c->set_tofcorr(corrtof);
	  c->set_prob_photon(emc->prob_photon());
	  c->set_chi2(emc->chi2());
	  c->set_disp(emc->dispy(),emc->dispz());
	  c->set_padisp(emc->padispy(),emc->padispz());
	  c->set_yz_cg(emc->ycg(),emc->zcg());
	  c->set_emcpc3dz(emc->emcpc3dz());
	  c->set_emcpc3dphi(emc->emcpc3dphi());
	  c->setArmSecIyIz();
	  c->setLocalPos(EmcGeo);
	  c->setVtxZ(bbcVz);
	  c->setTrackVector(theta);
          c->set_maps(emc->deadmap(),emc->warnmap()); /// maps


	  // There are certain variables that are not stored in the pDST.
	  if ( emc->has_E9() ) c->set_e9( emc->e9() );

	  int sector = c->getSec();
	  if (sector < 6)
	    {
	      cut3x3Map = sccut3x3Map;
	      tightcut3x3Map = tightsccut3x3Map;
	    }
	  else
	    {
	      cut3x3Map = glcut3x3Map;
	      tightcut3x3Map = tightglcut3x3Map;
	    }

	  int arm = emc->arm();
	  int tmpsector = emc->sector();

	  int thesector;				// Run7 warnmap
	  if(arm==1) thesector = 7 - tmpsector;
	  else thesector = tmpsector;


	  // add PID cuts and fiducial cuts here
          //if (tofstat==1) {
          if (((c->warnmap() & cut3x3Map) ==0)
            && ((c->deadmap() & cut3x3Map) ==0) )  {
              if (c->passFiducialCuts())
	                {}  // do nothing for now
          //if (1) {
	      // first cluster with pT > 3 GeV/c gets "marked" with
	      // index trigger_photon
	      if (c->getPT() > 3.0 && trigger_photon==-1)
		{
		  trigger_photon = g_multiplicity;
		}

	      c->passTOFCut(c->getSec());
	      //c->passTOFCut();
	      c->passCHI2Cut();
	      c->passStochasticCuts();
	      c->passPc3RejCut();
	      c->passTrackRejCut(phCentralTrack_ptr);

              if((c->warnmap() & tightcut3x3Map) ==0
               && (c->deadmap() & tightcut3x3Map)==0 ) c->setTightMapCut(1);
                 else c->setTightMapCut(0);

	      c->fillHist(gamma1, centralityBin, inrunn, gnt, is_bookNt, inPtcut, nclusters, reacBin, ERTb);
	      c->fillHistRap(gammarap, centralityBin);
	      tower->Fill(1.0,c->ecore(),sector,c->getIy(),c->getIz(), c->getTightMapCut());
              //tower->Fill(1.0,ecent,sector,c->getIy(),c->getIz(), c->getTightMapCut());
	      towertof->Fill(1.0,corrtof,ecent,sector,c->getIy(),c->getIz());
	      cluster_counter->Fill(nclusters);
	      towertof_3ns->Fill(1.0,corrtof,ecent,sector,c->getIy(),c->getIz());

//  if(sector>5)  tof_PbGl->Fill(1.0,corrtof,sector);

	      // Note that we don't need to delete this... the
	      // shared_ptr handles its own deletion.
	      boost::shared_ptr<KCluster> c_ptr( c );
	      clusterVector.push_back(c_ptr);
	      g_multiplicity++;

	  }
	  else
	    {
	      // If we reach here, we haven't stored c as a shared_ptr, so
	      // we need to get rid of it.
	      delete c;
	      c = 0;
	    }

	}
    }

  float ecore_02_Max=0.0;
  float ecore_02_Total=0.0;
  float ecore_02_Average=0.0;

  int counter=0;
  int nhclus=0;
  int nclus=0;

  for(int j=0; j<nclusters; j++)
    {
       emcClusterContent *emc = emccont->getCluster(j);

       if ( !emc ) { std::cout << "oh god, emc == 0!" << std::endl; return -1;}


        if (emc->e() > 0.05 && emc->deadmap()==0 && emc->warnmap()==0) nclus++;
        if (emc->e() > 0.5 && emc->deadmap()==0 && emc->warnmap()==0) nhclus++;

         if (emc->e() > 0.4)// && emc->deadmap()==0 && emc->warnmap()==0)
       // if (emc->ecore() > 0.3)// && emc->deadmap()==0 && emc->warnmap()==0)
		{

		if(emc->deadmap()==0 && emc->warnmap()==0)// && emc->sector()<6.0)
		  {
//		if(emc->arm()==1&&emc->sector()<2)	  cout << "Arm = PbGl" << emc->arm() << "\tSector = " << emc->sector() << endl;
//		else	  cout << "Arm = PbSc" << emc->arm() << "\tSector = " << emc->sector() << endl;
//			  cout << "Energy of "<<j<<"th cluster = " << emc->ecore() << endl;
			ecore_02_Total += emc->ecore();
			if(ecore_02_Max<emc->ecore()) ecore_02_Max=emc->ecore();
			counter++;
		}
	  }
    }

	if(counter!=0) ecore_02_Average = ecore_02_Total/counter;
    //cout << "E_Total = "  << ecore_02_Total << "\t" << "E_Average= " << counter << endl;
//	if((ert4x4abit==1||ert4x4bbit==1||ert4x4cbit==1)&&)
	if(flag_deadwarn_event==0) {
	  bbcqs_ecoreMax->Fill(bbcqs,ecore_02_Max);
	  bbcqs_ecoreTotal->Fill(bbcqs,ecore_02_Total);
	  bbcqs_ecoreAverage->Fill(bbcqs,ecore_02_Average);
	  bbcqn_ecoreMax->Fill(bbcqn,ecore_02_Max);
	  bbcqn_ecoreTotal->Fill(bbcqn,ecore_02_Total);
	  bbcqn_ecoreAverage->Fill(bbcqn,ecore_02_Average);

	   cent_fw_corr->Fill(1.0, bbcqn, nhclus, nclus, ecore_02_Max, ecore_02_Total, 0);
       cent_fw_corr->Fill(1.0, bbcqs, nhclus, nclus, ecore_02_Max, ecore_02_Total, 1);
	}

/*
	FILE *fp;
//    fp=NULL;
//	fp=fopen("nsm_arm_sec_Etotal_2.0GeV_goodert.txt","a+");

	int nsm_temp=-1;
	int nfill=0;

	if(ecore_02_Total<2.0&&flag_deadwarn_event==0) {

	fp=fopen("nsm_arm_sec_Etotal_2.0GeV_goodert.txt","a+");
	  for(int j=0; j<nclusters; j++) {
		emcClusterContent *emc = emccont->getCluster(j);
		if ( !emc ) { std::cout << "oh god, emc == 0!" << std::endl; return -1;}

		int nsm, ert4x4abit, ert4x4bbit, ert4x4cbit;
		if(emc->arm()==0||emc->sector()>=2){ // PbSc
		     nsm = (emc->iypos()/12)*6+(emc->izpos()/12);
		} else { // PbGl
		     nsm = (emc->iypos()/12)*8+(emc->izpos()/12);
		}
		ert4x4abit = ertout->get_ERTbit(0,emc->arm(),emc->sector(),nsm);
		ert4x4bbit = ertout->get_ERTbit(1,emc->arm(),emc->sector(),nsm);
		ert4x4cbit = ertout->get_ERTbit(2,emc->arm(),emc->sector(),nsm);

		if(ert4x4abit==1||ert4x4bbit==1||ert4x4cbit==1) {
//			printf("sick nsm = %d\tin sector %d\n",nsm,c->getSec());
	//		printf("sick nsm = %d\tin arm%d \tsector %d\n",nsm,emc->arm(),emc->sector());
		//	if(nsm==nsm_temp) printf("nluster = %d\nnsm=previous nsm = %d\n",nclusters,nsm);
			if(nsm!=nsm_temp) {
				fprintf(fp,"%d\t%d\t%d\n",nsm,emc->arm(),emc->sector());
				nsm_temp=nsm;
				nfill++;
				//printf("nfill = %d\n",nfill);
			}
		}
	  }
	fclose(fp);
	}

*/

  return  g_multiplicity;
}

float KEvent::getEtByMease() const
{
  return 0;
}
