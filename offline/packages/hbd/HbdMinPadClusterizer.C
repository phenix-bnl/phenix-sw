//******************************************************************************
//
// HbdMinPadClusterizer class
//
// Authors:
//
// Yosuke Watanabe(Tokyo University) - yosuke@nucl.phys.s.u-tokyo.ac.jp
//
// 22/07/2011 
//
// Please contact the authors before committing any changes
// to this file to the PHENIX repository.
//******************************************************************************

#include "HbdMinPadClusterizer.h"
#include "HbdPreClusterMaker.h"
#include "YWCutter.h"


//
// Fun4All and PHENIX headers
//
#include <phool.h>
#include <PHCompositeNode.h>
#include <getClass.h>
#include <Fun4AllReturnCodes.h>
#include <Fun4AllServer.h>
#include <PHGlobal.h>
#include <PHParticle.h>
#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>
#include <PHAngle.h>
#include <recoConsts.h>

#include <HbdCellList.h>
#include <HbdBlobList.h>
#include <HbdBlobListv1.h>
#include <HbdCell.h>
#include <HbdFinalSimSupport.h>
#include "HbdNeighboursV2.h"

#include <iostream>
#include <fstream>
#include <string>
#include <cstdlib>
#include <set>
#include <vector>
#include <algorithm>

typedef PHIODataNode <PHCentralTrack> PHParticleNode_t;
typedef PHIODataNode <HbdBlobList> HbdBlobListNode_t;
typedef PHIODataNode <PHGlobal> PHGlobalNode_t;

using namespace std;
using namespace findNode;

HbdMinPadClusterizer::HbdMinPadClusterizer(const char *NodeName, const char *CNTName):
  SubsysReco("HbdMinPadClusterizer"),d_NodeName(NodeName),d_CNTName(CNTName)
{
  d_NodeName = NodeName;
  verbose=false;
}

HbdMinPadClusterizer::~HbdMinPadClusterizer()
{
  //delete premaker;
}

int HbdMinPadClusterizer::Init(PHCompositeNode *topNode)
{ // Init

  cout<<"HbdMinPadClusterizer::Init() begin"<<endl;
  
  recoConsts *rc = recoConsts::instance();
  mc_flag = rc->get_IntFlag("HBD_MC",0); //0-data, 1-MC, 2-emnedding, 3-HIJING

  clst_flag = rc->get_IntFlag("HBD_CLUSTERIZER",3); //1-WisCl, 3-MinPadCl, 4-WisCl+MinPadCl
  
  sd_flag = rc->get_IntFlag("HBD_DOUBLE_REJECTION",0); //0-no double rej, 1-MM double rejection
  
//  int runnum,runno;
  int runnum;
  if (mc_flag == 0){ //Data
    runnum = rc->get_IntFlag("RUNNUMBER");
//    runno = runnum;
  }
  else if(mc_flag==2){ //Embedding
    runnum = 215034; //Run for geometry
//    runno  = rc->get_IntFlag("HBD_EMBRUN",303448); //Run for thresholds
  }
  else{
    runnum = 215034;
//    runno  = 999999; //Run for thresholds
  }

  hbdgeo.fetch(runnum);
  hbdgeo.fetchPad(runnum);
  cout<<"HbdMinPadClusterizer::Init() runnumber "<<runnum<<endl;
  
  premaker = new HbdPreClusterMaker(runnum);

  cutter = new YWCutter();
  cutter->SetMCFlag(mc_flag);
  cutter->SetRunnumber(runnum);
  cutter->Init();

  //For S/D rejection
  if (sd_flag > 0){
    cutter2 = new YWCutter();
    cutter2->SetMCFlag(mc_flag);
    cutter2->SetRunnumber(runnum);
    cutter2->Init();
    cutter2->SetRejection(25);
  }

  return 0;
}

int HbdMinPadClusterizer::InitRun(PHCompositeNode *topNode)
{
  return 0;
}

int HbdMinPadClusterizer::process_event(PHCompositeNode *topNode)
{

  if (verbose){
    static int ncalls=0;
    if(ncalls%1000==0 )cout << "HbdMinPadClusterizer::process_event Ncalls = " << ncalls << endl;
    ncalls ++;
  }

  CellList = findNode::getClass<HbdCellList>(topNode, "HbdCellList");
  if (!CellList) cout << PHWHERE << "HbdMinPadClusterizer:: No HbdCellList!" << endl;

  //Get the centrality
  float centrality = -9999.;

  //Get the bbcq
  if (mc_flag == 1){ //1-PURE MC
    bbcq = 1.0;
  }
  else{
    PHGlobal *global = findNode::getClass<PHGlobal>(topNode,"PHGlobal");
    if (!global) cout << PHWHERE << "HbdMinPadClusterizer:: No PHGlobal!" << endl;
    bbcq = global->getBbcChargeN()+global->getBbcChargeS();
    centrality = global->getCentrality();
  }
  if(verbose) cout<<"HbdMinPadClusterizer::Process() bbcq "<<bbcq<<endl;
  

  //Skip if the centrality is not defined
  if (centrality < 0 && mc_flag!=1) return 0;

  //
  // Find input data nodes
  //
  PHCentralTrack *trk = getClass<PHCentralTrack>(topNode,"PHCentralTrack");
  if(!trk) trk = getClass<PHCentralTrack>(topNode,"EWGCentralTrack");
  if(!trk) trk = getClass<PHCentralTrack>(topNode,d_CNTName.c_str());
  if(!trk) cout << PHWHERE << "HbdWisClusterizer:: No PHCentralTrack!" << endl;
  
  int ntrk = trk->get_npart();
  
  //
  // Here is the departure point for the clustering
  //
  if (ntrk>0){ 
    for (int jtrk=0; jtrk<ntrk; jtrk++){ 
      // loop over central arm electrons
      
          float PADX_p        = -9999;
 	  float PADY_p        = -9999;
 	  int  HBDSECTOR_p    = -9999;
 	  int HBDSIDE_p       = -9999;
	  float  PADCHARGE0_p = -9999;
	  float  PADCHARGE1_p = -9999;
	  float  PADCHARGE2_p = -9999;
	  float  PADCHARGE3_p = -9999;
	  float  PADCHARGE4_p = -9999;
	  float  PADCHARGE5_p = -9999;
	  float  PADCHARGE6_p = -9999;
	  float  PADCHARGE7_p = -9999;
	  float  PADCHARGE8_p = -9999;
	  float  PADCHARGE9_p = -9999;
	  float  PADKEY0_p    = -9999;
	  float  PADKEY1_p    = -9999;
	  float  PADKEY2_p    = -9999;
	  float  PADKEY3_p    = -9999;
	  float  PADKEY4_p    = -9999;
	  float  PADKEY5_p    = -9999;
	  float  PADKEY6_p    = -9999;
	  float  PADKEY7_p    = -9999;
	  float  PADKEY8_p    = -9999;
	  float  PADKEY9_p    = -9999;
	  int HBDPADNUM_p       = -9999;
	  int HBDPADTYPE_p    = -9999;
	  float LOCX_p        = -9999;
 	  float LOCY_p        = -9999;
	  
	  float hbd_px = -9999;
	  float hbd_py = -9999;
	  float hbd_pz = -9999;
	  hbd_px = trk->get_track(jtrk)->get_phbdx();
	  hbd_py = trk->get_track(jtrk)->get_phbdy();
	  hbd_pz = trk->get_track(jtrk)->get_phbdz();
	  
//	  int hbd_presize = -9999;
	  int hbd_sector = -9999;
	  double loc_y,loc_z;
	  int pretmp_hbd_sector = 0;
	  int tmp_hbd_sector = 0;
	  int closest_pad = 0;

	  //Clusterizer variables 
	  float YWELECTRON   = -9999;
	  float YWSELECTRON  = -9999;
	  float ELECTRONTYPE = -9999;
	  

	  //skip the tracks that have bad projections to HBD
	  if(!((fabs(hbd_px))<99999)) continue;
	  if(!((fabs(hbd_py))<99999)) continue;
	  if(!((fabs(hbd_pz))<99999)) continue;
	  
	  hbdgeo.GlobToLoc(hbd_px,hbd_py,hbd_pz,loc_y,loc_z,pretmp_hbd_sector);
	  if(fabs(loc_z)>99999||fabs(loc_y)>99999){
	    cerr<<"HbdMinPadClusterizer:process_event(): GlobToLoc gives bad result"<<endl;
	  }
	  
	  tmp_hbd_sector = pretmp_hbd_sector;
	  hbd_sector = tmp_hbd_sector;
	  
	  LOCX_p = loc_z;
	  LOCY_p = loc_y;

	  //
	  // Swapped variables
	  //
	  float hbd_px_s = -hbd_px;//swapped track projection to hbd
	  float hbd_py_s =  hbd_py;//swapped track projection to hbd
	  float hbd_pz_s =  hbd_pz;//swapped track projection to hbd
//	  int hbd_presize_s = -9999;
	  int hbd_sector_s = -9999;
	 
	  float PADX_s        = -9999;
 	  float PADY_s        = -9999;
 	  int  HBDSECTOR_s    = -9999;
 	  int HBDSIDE_s       = -9999;
	  float  PADCHARGE0_s = -9999;
	  float  PADCHARGE1_s = -9999;
	  float  PADCHARGE2_s = -9999;
	  float  PADCHARGE3_s = -9999;
	  float  PADCHARGE4_s = -9999;
	  float  PADCHARGE5_s = -9999;
	  float  PADCHARGE6_s = -9999;
	  float  PADCHARGE7_s = -9999;
	  float  PADCHARGE8_s = -9999;
	  float  PADCHARGE9_s = -9999;
/*
	  float  PADKEY0_s    = -9999;
	  float  PADKEY1_s    = -9999;
	  float  PADKEY2_s    = -9999;
	  float  PADKEY3_s    = -9999;
	  float  PADKEY4_s    = -9999;
	  float  PADKEY5_s    = -9999;
	  float  PADKEY6_s    = -9999;
	  float  PADKEY7_s    = -9999;
	  float  PADKEY8_s    = -9999;
	  float  PADKEY9_s    = -9999;
*/
	  int HBDPADNUM_s      = -9999;
	  int HBDPADTYPE_s    = -9999;
 	  float LOCX_s        = -9999;
 	  float LOCY_s        = -9999;

	  vector<PadsInCluster> v_hitkey;
	  
		//Real track(non-swapped)
		hbdgeo.FindNearestPad(loc_y,loc_z,closest_pad);
		if(closest_pad == 189 || closest_pad == 190 || closest_pad == 191){
			double low_array[8]={-12,-8.1125,-4.7,-2.7,0,2.7,4.7,8.1125};
			double high_array[8]={-8.1125,-4.7,-2.7,0,2.7,4.7,8.1125,12};
			for(int l=0;l<8;l++){
				if(loc_y>= low_array[l]&&loc_y<high_array[l]){
					closest_pad = 181 + l;
				}
			}
		}
		if(closest_pad == 0 || closest_pad == 1){
			double low_array2[9]={-12,-9.459,-6.757,-4.054,-1.3514,1.3514,4.054,6.757,9.459};
			double high_array2[9]={-9.459,-6.757,-4.054,-1.3514,1.3514,4.054,6.757,9.459,12};
			for(int l=0;l<9;l++){
				if(loc_y>= low_array2[l]&&loc_y<high_array2[l]){
					closest_pad = 2+l;
				}
			}
		}

		premaker->get_padkeys(tmp_hbd_sector,closest_pad,loc_z,loc_y,v_hitkey);
		PADX_p    = premaker->get_padx();
		PADY_p    = premaker->get_pady();
		HBDPADTYPE_p = premaker->get_padtype();

//		hbd_presize = v_hitkey.size();

		if(v_hitkey.size()>0){
			vector<PadsInCluster>::iterator it = v_hitkey.begin();
			while(it!=v_hitkey.end()){
				(*it).charge = CellList->get_cell((*it).padkey)->get_charge();
				it++;
			}
			for(unsigned int pc =0;pc < v_hitkey.size();pc++){
				switch(pc){
					case 0: 
						PADKEY0_p = v_hitkey[pc].padkey;
						PADCHARGE0_p = v_hitkey[pc].charge;
						break;
					case 1: 
						PADKEY1_p = v_hitkey[pc].padkey;
						PADCHARGE1_p = v_hitkey[pc].charge;
						break;
					case 2: 
						PADKEY2_p = v_hitkey[pc].padkey;
						PADCHARGE2_p = v_hitkey[pc].charge;
						break;
					case 3: 
					        PADKEY3_p = v_hitkey[pc].padkey;
						PADCHARGE3_p = v_hitkey[pc].charge;
						break;
					case 4: 
						PADKEY4_p = v_hitkey[pc].padkey;
						PADCHARGE4_p = v_hitkey[pc].charge;
						break;
					case 5: 
					        PADKEY5_p = v_hitkey[pc].padkey; 
						PADCHARGE5_p = v_hitkey[pc].charge;
						break;
					case 6: 
						PADKEY6_p = v_hitkey[pc].padkey;
						PADCHARGE6_p = v_hitkey[pc].charge;
						break;
					case 7: 
						PADKEY7_p = v_hitkey[pc].padkey;
						PADCHARGE7_p = v_hitkey[pc].charge;
						break;
					case 8: 
					        PADKEY8_p = v_hitkey[pc].padkey;
						PADCHARGE8_p = v_hitkey[pc].charge;
						break;
					case 9: 
						PADKEY9_p = v_hitkey[pc].padkey;
						PADCHARGE9_p = v_hitkey[pc].charge;
						break;
				}
			}
		}

		HBDSECTOR_p = hbd_sector;

		if(closest_pad<96){
		  HBDSIDE_p = 0;
		}else if(closest_pad>95){
		  HBDSIDE_p = 1;
		}
		HBDPADNUM_p = closest_pad;

		//Swapped track
		hbdgeo.GlobToLoc(hbd_px_s,hbd_py_s,hbd_pz_s,loc_y,loc_z,pretmp_hbd_sector);
		if(fabs(loc_z)>99999||fabs(loc_y)>99999){
			cerr<<"HbdMinPadClusterizer:process_event(): GlobToLoc gives bad result2"<<endl;
		}
		hbd_sector_s = pretmp_hbd_sector;
		hbdgeo.FindNearestPad(loc_y,loc_z,closest_pad);
		if(closest_pad == 189 || closest_pad == 190 || closest_pad == 191){
			double low_array[8]={-12,-8.1125,-4.7,-2.7,0,2.7,4.7,8.1125};
			double high_array[8]={-8.1125,-4.7,-2.7,0,2.7,4.7,8.1125,12};
			for(int l=0;l<8;l++){
				if(loc_y>= low_array[l]&&loc_y<high_array[l]){
					closest_pad = 181 + l;
				}
			}
		}
		if(closest_pad == 0 || closest_pad == 1){
			double low_array2[9]={-12,-9.459,-6.757,-4.054,-1.3514,1.3514,4.054,6.757,9.459};
			double high_array2[9]={-9.459,-6.757,-4.054,-1.3514,1.3514,4.054,6.757,9.459,12};
			for(int l=0;l<9;l++){
				if(loc_y>= low_array2[l]&&loc_y<high_array2[l]){
					closest_pad = 2+l;
				}
			}
		}
		LOCX_s = loc_z;
		LOCY_s = loc_y;

		v_hitkey.clear();
		premaker->get_padkeys(hbd_sector_s,closest_pad,loc_z,loc_y,v_hitkey);
		PADX_s       = premaker->get_padx();
		PADY_s       = premaker->get_pady();
		HBDPADTYPE_s = premaker->get_padtype();

//		hbd_presize_s = v_hitkey.size();
		if(v_hitkey.size()>0){
			vector<PadsInCluster>::iterator it = v_hitkey.begin();
			while(it!=v_hitkey.end()){
				(*it).charge = CellList->get_cell((*it).padkey)->get_charge();
				it++;
			}
			for(unsigned int pc =0;pc < v_hitkey.size();pc++){
				switch(pc){
					case 0: 
					     // PADKEY0_s = v_hitkey[pc].padkey;
						PADCHARGE0_s = v_hitkey[pc].charge;
						break;
					case 1: 
					     // PADKEY1_s = v_hitkey[pc].padkey;
						PADCHARGE1_s = v_hitkey[pc].charge;
						break;
					case 2: 
					     // PADKEY2_s = v_hitkey[pc].padkey;
						PADCHARGE2_s = v_hitkey[pc].charge;
						break;
					case 3: 
					     //	PADKEY3_s = v_hitkey[pc].padkey;
						PADCHARGE3_s = v_hitkey[pc].charge;
						break;
					case 4: 
					     // PADKEY4_s = v_hitkey[pc].padkey;
						PADCHARGE4_s = v_hitkey[pc].charge;
						break;
					case 5: 
					     // PADKEY5_s = v_hitkey[pc].padkey;
				                PADCHARGE5_s = v_hitkey[pc].charge;
						break;
					case 6: 
					     //	PADKEY6_s = v_hitkey[pc].padkey;
						PADCHARGE6_s = v_hitkey[pc].charge;
						break;
					case 7: 
					     //	PADKEY7_s = v_hitkey[pc].padkey;
						PADCHARGE7_s = v_hitkey[pc].charge;
						break;
					case 8: 
					     // PADKEY8_s = v_hitkey[pc].padkey;
						PADCHARGE8_s = v_hitkey[pc].charge;
						break;
					case 9: 
					     //	PADKEY9_s = v_hitkey[pc].padkey;
						PADCHARGE9_s = v_hitkey[pc].charge;
						break;
				}
			}

		}
	  
		HBDSECTOR_s = hbd_sector_s;

		if(closest_pad<96){
		  HBDSIDE_s = 0;
		}else if(closest_pad>95){
		  HBDSIDE_s = 1;
		}
		HBDPADNUM_s = closest_pad;

		
      electron.Reset();
      if ( !(HBDPADNUM_p<0 || HBDPADNUM_p==0 || HBDPADNUM_p==1 || HBDPADNUM_p==189 || HBDPADNUM_p==190 || HBDPADNUM_p==191) ){
	electron.set_bbcq(bbcq);
	electron.set_padx(PADX_p);
	electron.set_pady(PADY_p);
	electron.set_hbdsector(int(HBDSECTOR_p));      
	electron.set_hbdside(int(HBDSIDE_p));      
	electron.set_padcharge0(PADCHARGE0_p);      
	electron.set_padcharge1(PADCHARGE1_p);      
	electron.set_padcharge2(PADCHARGE2_p);      
	electron.set_padcharge3(PADCHARGE3_p);
	electron.set_padcharge4(PADCHARGE4_p);
	electron.set_padcharge5(PADCHARGE5_p);
	electron.set_padcharge6(PADCHARGE6_p);
	electron.set_padcharge7(PADCHARGE7_p);
	electron.set_padcharge8(PADCHARGE8_p);
	electron.set_padcharge9(PADCHARGE9_p);

	electron.set_padkey0(PADKEY0_p);      
	electron.set_padkey1(PADKEY1_p);      
	electron.set_padkey2(PADKEY2_p);      
	electron.set_padkey3(PADKEY3_p);
	electron.set_padkey4(PADKEY4_p);
	electron.set_padkey5(PADKEY5_p);
	electron.set_padkey6(PADKEY6_p);
	electron.set_padkey7(PADKEY7_p);
	electron.set_padkey8(PADKEY8_p);
	electron.set_padkey9(PADKEY9_p);

	electron.set_hbdpadnum((int)HBDPADNUM_p);
	electron.set_padtype(HBDPADTYPE_p);
	electron.set_locx(LOCX_p);
	electron.set_locy(LOCY_p);
	electron.set_hbdpz(hbd_pz);
      }

      //loop over different rejections
      int hbdrej = 0;
      for ( int i=1;i<=25;i++){
	cutter->SetRejection(i);
	if (cutter->isHBDElectron(electron)) hbdrej = i;
	else break;
      }
      if (clst_flag == 3 || clst_flag == 4) 
	trk->get_track(jtrk)->set_hbdid(hbdrej);


      //Continue with the fixed rejection
      if (mc_flag==1) cutter->SetRejection(rejection);
      else cutter->SetEventRejection(centrality); //Sets the rejection according to centrality

      cutter->isHBDElectron(electron);
      if (cutter->isHBDElectron(electron)) YWELECTRON = 1;
      else                                 YWELECTRON = 0;

      if (clst_flag == 3){ //MinPad Clusterizer alone
	trk->get_track(jtrk)->set_hbdsize(electron.get_hbdsize());
	trk->get_track(jtrk)->set_hbdcharge(electron.get_hbdcharge());
	trk->get_track(jtrk)->set_hbdsector(int(HBDSECTOR_p));
      }
      else if (clst_flag == 4){//WisCl+MinPad Clusterizer
	trk->get_track(jtrk)->set_hbdx(YWELECTRON);
	trk->get_track(jtrk)->set_hbdhubcharge(electron.get_hbdcharge());
	trk->get_track(jtrk)->set_hbdspokecharge1(electron.get_hbdsize());
      }


      //Swapped track
      selectron.Reset();
      if ( !(HBDPADNUM_s<0 || HBDPADNUM_s==0 || HBDPADNUM_s==1 || HBDPADNUM_s==189 || HBDPADNUM_s==190 || HBDPADNUM_s==191) ){
	selectron.set_bbcq(bbcq);
	selectron.set_padx(PADX_s);
	selectron.set_pady(PADY_s);
	selectron.set_hbdsector(int(HBDSECTOR_s));      
	selectron.set_hbdside(int(HBDSIDE_s));      
	selectron.set_padcharge0(PADCHARGE0_s);      
	selectron.set_padcharge1(PADCHARGE1_s);      
	selectron.set_padcharge2(PADCHARGE2_s);      
	selectron.set_padcharge3(PADCHARGE3_s);
	selectron.set_padcharge4(PADCHARGE4_s);
	selectron.set_padcharge5(PADCHARGE5_s);
	selectron.set_padcharge6(PADCHARGE6_s);
	selectron.set_padcharge7(PADCHARGE7_s);
	selectron.set_padcharge8(PADCHARGE8_s);
	selectron.set_padcharge9(PADCHARGE9_s);
	selectron.set_hbdpadnum((int)HBDPADNUM_s);
	selectron.set_padtype(HBDPADTYPE_s);
	selectron.set_locx(LOCX_s);
	selectron.set_locy(LOCY_s);
	selectron.set_hbdpz(hbd_pz_s);
      }
      
      //loop over different rejections
      int shbdrej = 0;
      for ( int i=1;i<=25;i++){
      cutter->SetRejection(i);
      if (cutter->isHBDElectron(selectron)) shbdrej = i;
      else break;
      }
      if (clst_flag == 3)
	trk->get_track(jtrk)->set_hbdhubcharge(shbdrej);

      //Continue with the fixed rejection
      if (mc_flag==1) cutter->SetRejection(rejection);
      else cutter->SetEventRejection(centrality); //Sets the rejection according to centrality

      cutter->isHBDElectron(selectron);
      if (cutter->isHBDElectron(selectron)) YWSELECTRON = 1;
      else                                  YWSELECTRON = 0;

      if (clst_flag == 3){ //MinPad Clusterizer alone
	trk->get_track(jtrk)->set_hbdspokecharge1(selectron.get_hbdcharge());
	trk->get_track(jtrk)->set_hbdspokecharge2(selectron.get_hbdsize());
	trk->get_track(jtrk)->set_hbdx(int(HBDSECTOR_s));
      }
      else if (clst_flag == 4){//WisCl+MinPad Clusterizer
	trk->get_track(jtrk)->set_hbdy(YWSELECTRON);
      }


      //-------------------------------------------------------------------
      //S/D rejection - experimental 
      
      //Continue if the double flag is not set
      if (sd_flag != 1) continue;
      
      //Get the coordinates of the main pad
      double mainxx,mainyy,mainzz;
      mainxx=mainyy=mainzz=-9999;
      int mainpn = (int)PADKEY0_p % 192;
      int mainsc = ((int)PADKEY0_p-mainpn)/192;
      hbdgeo.getPadGlobal(mainpn, mainsc, mainxx, mainyy, mainzz);
      
      
      //Get the list of the first neighbors
      vector<Pad> FirstNeighbor;
      for (int kk=0;kk<8;kk++){
	short key = pad_neighbours[(int)PADKEY0_p][kk];
        
	if (key <=0) continue; 	//skip if the key is not ok

	bool usedpad = false;
	for (int k=0;k<10;k++){
	  if ( key == electron.get_usedpad(k) ){ usedpad = true; break; }
	}
	if (usedpad) continue; 	//skip if the pad is used
	
	//Push a new pad
	Pad newpad;
	double xx,yy,zz;
	xx=yy=zz=-9999;

	int pn = (int)key % 192;
	int sc = ((int)key-pn)/192;
	hbdgeo.getPadGlobal(pn, sc, xx, yy, zz);
	
	newpad.padkey = key;
	newpad.charge = CellList->get_cell(key)->get_charge();
	newpad.distance = sqrt( pow((xx-hbd_px),2) + pow((yy-hbd_py),2) + pow((zz-hbd_pz),2) );
	
	FirstNeighbor.push_back(newpad); //Push it into the list
      }
      
      //Sort the pads according to distance from the projection point
      std::sort(FirstNeighbor.begin(), FirstNeighbor.end(), sort_by_distance);
      
      
      //Get the list of the second neighbors
      vector<Pad> SecondNeighbor;
      for (unsigned int jj=0;jj<FirstNeighbor.size();jj++){  //loop over the first neighbors
	
	short key1 = FirstNeighbor[jj].padkey;
	
	for (int kk=0;kk<8;kk++){
	  short key = pad_neighbours[key1][kk];
	  
	  if (key <=0) continue; 	 //skip if the key is not ok
	  
	  if (key == PADKEY0_p) continue;//skip if this is the main pad
	  
	  bool usedpad = false;
	  for (int k=0;k<10;k++){
	    if ( key == electron.get_usedpad(k) ){ usedpad = true; break; }
	  }
	  if (usedpad) continue; 	//skip if the pad is used
	  
	  bool firstpad = false;
	  for (unsigned int k=0;k<FirstNeighbor.size();k++){
	    if ( key ==  FirstNeighbor[k].padkey ){ firstpad = true; break; }
	  }
	  if (firstpad) continue; 	//skip if it is a 1st neighbor
	  
	  bool havepad = false;
	  for (unsigned int k=0;k<SecondNeighbor.size();k++){
	    if ( key == SecondNeighbor[k].padkey ){ havepad = true; break; }
	  }
	  if (havepad) continue; 	//skip if the is already in the list
	  
	  //Push a new pad
	  Pad newpad;
	  double xx,yy,zz;
	  xx=yy=zz=-9999;
	  
	  int pn = (int)key % 192;
	  int sc = ((int)key-pn)/192;
	  hbdgeo.getPadGlobal(pn, sc, xx, yy, zz);
	  
	  newpad.padkey = key;
	  newpad.charge = CellList->get_cell(key)->get_charge();
	  newpad.distance = sqrt( pow((xx-hbd_px),2) + pow((yy-hbd_py),2) + pow((zz-hbd_pz),2) );
	  
	  SecondNeighbor.push_back(newpad); //Push it into the list
	}
      }
      
      //Sort the pads according to distance from the projection point
      std::sort(SecondNeighbor.begin(), SecondNeighbor.end(), sort_by_distance);
      
      
      //Create the list of compact triplets seeded on the second neighbors
      vector<Triplet> SecondTriplet;
      for (unsigned int jj=0;jj<SecondNeighbor.size();jj++){  //loop over the second neighbors
	
	short key0 = SecondNeighbor[jj].padkey;
	
	for (int kk=0;kk<8;kk++){

	  short key1,key2;
//	  key1=key2=-9999;

	  //How to treat the end of the loop
	  if ( pad_neighbours[key0][kk] == 0 ) break; //if this one reaches zero, end the loop

	  if (kk==7){ //if the first one is at the end, the second must be at beginning
	    key1 = pad_neighbours[key0][kk];
	    key2 = pad_neighbours[key0][0];
	  }
	  else if (kk<7 && pad_neighbours[key0][kk+1]==0){ //if the second one at the end, move it forward 
	    key1 = pad_neighbours[key0][kk];
	    key2 = pad_neighbours[key0][0];
	  }
	  else{
	    key1 = pad_neighbours[key0][kk];
	    key2 = pad_neighbours[key0][kk+1];
	  }
	  
	  //check that the pads are not contained in the primary cluster
	  bool usedpad1 = false;
	  bool usedpad2 = false;
	  for (int k=0;k<10;k++){
	    if ( key1 == electron.get_usedpad(k) ){ usedpad1 = true; break; }
	  }
	  for (int k=0;k<10;k++){
	    if ( key2 == electron.get_usedpad(k) ){ usedpad2 = true; break; }
	  }

	  //Create a triplet
	  float charge0 = CellList->get_cell(key0)->get_charge();
	  float charge1 = CellList->get_cell(key1)->get_charge();
	  float charge2 = CellList->get_cell(key2)->get_charge();
	  
	  //Calculate the center of gravity
	  float tripletcharge = 0;
	  float tripletdistance = -9999;
	  float maindistance = -9999;
	  float triplet_cog[3];
	  for (int n=0;n<3;n++) triplet_cog[n]=0;
	  short triplet_key[3];
	  for (int n=0;n<3;n++) triplet_key[n]=-9999;
	  float triplet_pad_charge[3];
	  for (int n=0;n<3;n++) triplet_pad_charge[n]=-9999;
	  double tmpx,tmpy,tmpz;
	  tmpx=tmpy=tmpz=-9999;
	  int tmpsize = 0;

	  //Skip if the charge is not ok
	  if (charge0 <= -999) continue;

	  //Get pad 0
	  int pn0 = (int)key0 % 192;
	  int sc0 = ((int)key0-pn0)/192;
	  hbdgeo.getPadGlobal(pn0, sc0, tmpx, tmpy, tmpz);
	  triplet_cog[0] += tmpx * charge0; //x
	  triplet_cog[1] += tmpy * charge0; //y 
	  triplet_cog[2] += tmpz * charge0; //z 
	  tripletcharge += charge0;
	  triplet_key[0] = key0; 
	  triplet_pad_charge[0] = charge0; 
	  tmpsize++;
	  
	  //Get pad 1
	  if (charge1>-999 && usedpad1==false){
	    int pn1 = (int)key1 % 192;
	    int sc1 = ((int)key1-pn1)/192;
	    hbdgeo.getPadGlobal(pn1, sc1, tmpx, tmpy, tmpz);
	    triplet_cog[0] += tmpx * charge1; //x
	    triplet_cog[1] += tmpy * charge1; //y 
	    triplet_cog[2] += tmpz * charge1; //z 
	    tripletcharge += charge1;
	    triplet_key[1] = key1; 
	    triplet_pad_charge[1] = charge1; 
	    tmpsize++;
	  }

	  //Get pad 2
	  if (charge2>-999 && usedpad2==false){
	    int pn2 = (int)key2 % 192;
	    int sc2 = ((int)key2-pn2)/192;
	    hbdgeo.getPadGlobal(pn2, sc2, tmpx, tmpy, tmpz);
	    triplet_cog[0] += tmpx * charge2; //x
	    triplet_cog[1] += tmpy * charge2; //y 
	    triplet_cog[2] += tmpz * charge2; //z 
	    tripletcharge += charge2;
	    triplet_key[2] = key2; 
	    triplet_pad_charge[2] = charge2; 
	    tmpsize++;
	  }
	  
	  triplet_cog[0] /= tripletcharge; 
	  triplet_cog[1] /= tripletcharge; 
	  triplet_cog[2] /= tripletcharge; 
	  
	  //Calculate the distance to the projection point
	  tripletdistance = sqrt( pow((triplet_cog[0]-hbd_px),2)
				  + pow((triplet_cog[1]-hbd_py),2)
				  + pow((triplet_cog[2]-hbd_pz),2) );
	  
	  
	  //Calculate the distance to the main pad
	  maindistance = sqrt( pow((triplet_cog[0]-mainxx),2)
				  + pow((triplet_cog[1]-mainyy),2)
				  + pow((triplet_cog[2]-mainzz),2) );

	  //Perform some basic cuts to reduce the number of triplets
	  if (tripletcharge<3 || tripletcharge>50) continue;

	  //Reject it if the CoG is closer than 2*a
	  //(to avoid picking the primary pads later in the clusterizer)
	  //Should also check later
	  if (maindistance < 2*1.55) continue;

	  //Skip if the is already in the list
	  bool havetriplet = false;
	  for (unsigned int k=0;k<SecondTriplet.size();k++){
	    if ( tripletcharge == SecondTriplet[k].charge ){ havetriplet = true; break; }
	  }
	  if (havetriplet) continue;
	  
	  
	  //Push a new triplet
	  Triplet triplet;
	  triplet.padkey[0] = triplet_key[0];
	  triplet.padkey[1] = triplet_key[1];
	  triplet.padkey[2] = triplet_key[2];
	  triplet.charge = tripletcharge;
	  triplet.distance = tripletdistance;
	  triplet.position[0] = triplet_cog[0];
	  triplet.position[1] = triplet_cog[1];
	  triplet.position[2] = triplet_cog[2];
	  triplet.padcharge[0] = triplet_pad_charge[0];
	  triplet.padcharge[1] = triplet_pad_charge[1];
	  triplet.padcharge[2] = triplet_pad_charge[2];
	  
	  SecondTriplet.push_back(triplet); //Push it into the list
	}
      }
      //--- Finished creating the triples ---

      //Sort the triplets according to distance from the projection point
      std::sort(SecondTriplet.begin(), SecondTriplet.end(), sort_Triplet_by_distance);

  
      //Perform S/D charge cut 
      if (cutter->isHBDElectron(electron)){
	
	float primcharge = electron.get_hbdcharge(); //charge of the primary cluster
	int primsize = electron.get_hbdsize(); //size of the primary cluster
	bool missing_found = false;
	
	
	//Loop for the max ef sig cuts
	if (cutter->isSingleElectron(centrality,primsize,primcharge)){
	  
	  
	  //This is a single electron that we accept
	  ELECTRONTYPE = 0;
	  
	  //Start looking for a nearby hit within the first neighbors
	  for (unsigned int ii=0;ii<FirstNeighbor.size();ii++){
	    
	    float mischarge   = FirstNeighbor[ii].charge; 
	    float misdistance = FirstNeighbor[ii].distance; 
	    
	    //If it doesn't satisfy charge cut for the missing pad
	    if (misdistance < 2.6){ //closer
	      if (!cutter->isMissingElectron(centrality,primsize,mischarge,0)) continue;
	    }
	    else{ //farther
	      if (!cutter->isMissingElectron(centrality,primsize,mischarge,1)) continue;
	    }
	    
	    //If it doesn't satisfy charge cut for the single hit
	    if (!cutter->isSingleElectron(centrality,1,mischarge)) continue;
	    
	    //Continue looking if the combined charge is still a single
	    if (cutter->isSingleElectron(centrality,primsize+1,primcharge+mischarge)) continue;
	    
	    //If a nearby hit is found
	    ELECTRONTYPE = 1;
	    
	    missing_found = true;
	    break;
	  } 
	  
	  if (!missing_found){
	    //Start looking for a nearby hit within the second neighbors
	    for (unsigned int ii=0;ii<SecondNeighbor.size();ii++){
	      
	      float mischarge   = SecondNeighbor[ii].charge; 
	      //float misdistance = SecondNeighbor[ii].distance; 
	      
	      //If it doesn't satisfy charge cut for the missing pad
	      if (!cutter->isMissingElectron(centrality,primsize,mischarge,2)) continue;
	      //If it doesn't satisfy charge cut for the single hit
	      if (!cutter->isSingleElectron(centrality,1,mischarge)) continue;

	      //Continue looking if the combined charge is still a single
	      if (cutter->isSingleElectron(centrality,primsize+1,primcharge+mischarge)) continue;
	      
	      //If a nearby hit is found
	      ELECTRONTYPE = 2;
	      missing_found = true;
	      break;
	    } 
          }
	  
          if (!missing_found){
            //
	    //Start looking for a nearby hit using triplets
	    //
	    if (SecondTriplet.size() > 0){
	      
	      for (unsigned int tcnt=0; tcnt<SecondTriplet.size(); tcnt++){ // loop over triplets
		
		float PADX_trpl        = -9999;
		float PADY_trpl        = -9999;
		int  HBDSECTOR_trpl    = -9999;
		int HBDSIDE_trpl       = -9999;
		float  PADCHARGE0_trpl = -9999;
		float  PADCHARGE1_trpl = -9999;
		float  PADCHARGE2_trpl = -9999;
		float  PADCHARGE3_trpl = -9999;
		float  PADCHARGE4_trpl = -9999;
		float  PADCHARGE5_trpl = -9999;
		float  PADCHARGE6_trpl = -9999;
		float  PADCHARGE7_trpl = -9999;
		float  PADCHARGE8_trpl = -9999;
		float  PADCHARGE9_trpl = -9999;
		int  PADKEY0_trpl    = -9999;
		int  PADKEY1_trpl    = -9999;
		int  PADKEY2_trpl    = -9999;
		int  PADKEY3_trpl    = -9999;
		int  PADKEY4_trpl    = -9999;
		int  PADKEY5_trpl    = -9999;
		int  PADKEY6_trpl    = -9999;
		int  PADKEY7_trpl    = -9999;
		int  PADKEY8_trpl    = -9999;
		int  PADKEY9_trpl    = -9999;
		int HBDPADNUM_trpl       = -9999;
		int HBDPADTYPE_trpl    = -9999;
		float LOCX_trpl        = -9999;
		float LOCY_trpl        = -9999;
		
		float triplet_x = -9999;
		float triplet_y = -9999;
		float triplet_z = -9999;
		triplet_x = SecondTriplet[tcnt].position[0];
		triplet_y = SecondTriplet[tcnt].position[1];
		triplet_z = SecondTriplet[tcnt].position[2];
		
//		int triplet_presize = -9999;
		int triplet_sector = -9999;
		double triplet_loc_y,triplet_loc_z;
		int pretmp_triplet_sector = 0;
		int tmp_triplet_sector = 0;
		int triplet_closest_pad = 0;
		
		//skip the tracks that have bad projections to HBD
		if(!((fabs(triplet_x))<99999)) continue;
		if(!((fabs(triplet_y))<99999)) continue;
		if(!((fabs(triplet_z))<99999)) continue;
		
		hbdgeo.GlobToLoc(triplet_x,triplet_y,triplet_z,triplet_loc_y,triplet_loc_z,pretmp_triplet_sector);
		if(fabs(triplet_loc_z)>99999||fabs(triplet_loc_y)>99999){
		  cerr<<"HbdMinPadClusterizer:process_event(): GlobToLoc gives bad result"<<endl;
		}
		
		tmp_triplet_sector = pretmp_triplet_sector;
		triplet_sector = tmp_triplet_sector;
		
		LOCX_trpl = triplet_loc_z;
		LOCY_trpl = triplet_loc_y;
	      
	      
		vector<PadsInCluster> triplet_hitkey;
	      
		//Real track(non-swapped)
		hbdgeo.FindNearestPad(triplet_loc_y,triplet_loc_z,triplet_closest_pad);
		if(triplet_closest_pad == 189 || triplet_closest_pad == 190 || triplet_closest_pad == 191){
		  double low_array[8]={-12,-8.1125,-4.7,-2.7,0,2.7,4.7,8.1125};
		  double high_array[8]={-8.1125,-4.7,-2.7,0,2.7,4.7,8.1125,12};
		  for(int ll=0;ll<8;ll++){
		    if(triplet_loc_y>= low_array[ll]&&triplet_loc_y<high_array[ll]){
		      triplet_closest_pad = 181 + ll;
		    }
		  }
		}
		if(triplet_closest_pad == 0 || triplet_closest_pad == 1){
		  double low_array2[9]={-12,-9.459,-6.757,-4.054,-1.3514,1.3514,4.054,6.757,9.459};
		  double high_array2[9]={-9.459,-6.757,-4.054,-1.3514,1.3514,4.054,6.757,9.459,12};
		  for(int ll=0;ll<9;ll++){
		    if(triplet_loc_y>= low_array2[ll]&&triplet_loc_y<high_array2[ll]){
		      triplet_closest_pad = 2+ll;
		    }
		  }
		}
		
		premaker->get_padkeys(tmp_triplet_sector,triplet_closest_pad,triplet_loc_z,triplet_loc_y,triplet_hitkey);
		PADX_trpl    = premaker->get_padx();
		PADY_trpl    = premaker->get_pady();
		HBDPADTYPE_trpl = premaker->get_padtype();
		
//		triplet_presize = triplet_hitkey.size();
		
		if(triplet_hitkey.size()>0){
		  vector<PadsInCluster>::iterator it = triplet_hitkey.begin();
		  while(it!=triplet_hitkey.end()){
		    (*it).charge = CellList->get_cell((*it).padkey)->get_charge();
		    it++;
		  }
		  for(unsigned int tpc =0;tpc < triplet_hitkey.size();tpc++){
		    switch(tpc){
		    case 0: 
		      PADKEY0_trpl = triplet_hitkey[tpc].padkey;
		      PADCHARGE0_trpl = triplet_hitkey[tpc].charge;
		      break;
		    case 1: 
		      PADKEY1_trpl = triplet_hitkey[tpc].padkey;
		      PADCHARGE1_trpl = triplet_hitkey[tpc].charge;
		      break;
		    case 2: 
		      PADKEY2_trpl = triplet_hitkey[tpc].padkey;
		      PADCHARGE2_trpl = triplet_hitkey[tpc].charge;
		      break;
		    case 3: 
		      PADKEY3_trpl = triplet_hitkey[tpc].padkey;
		      PADCHARGE3_trpl = triplet_hitkey[tpc].charge;
		      break;
		    case 4: 
		      PADKEY4_trpl = triplet_hitkey[tpc].padkey;
		      PADCHARGE4_trpl = triplet_hitkey[tpc].charge;
		      break;
		    case 5: 
		      PADKEY5_trpl = triplet_hitkey[tpc].padkey; 
		      PADCHARGE5_trpl = triplet_hitkey[tpc].charge;
		      break;
		    case 6: 
		      PADKEY6_trpl = triplet_hitkey[tpc].padkey;
		      PADCHARGE6_trpl = triplet_hitkey[tpc].charge;
		      break;
		    case 7: 
		      PADKEY7_trpl = triplet_hitkey[tpc].padkey;
		      PADCHARGE7_trpl = triplet_hitkey[tpc].charge;
		      break;
		    case 8: 
		      PADKEY8_trpl = triplet_hitkey[tpc].padkey;
		      PADCHARGE8_trpl = triplet_hitkey[tpc].charge;
		      break;
		    case 9: 
		      PADKEY9_trpl = triplet_hitkey[tpc].padkey;
		      PADCHARGE9_trpl = triplet_hitkey[tpc].charge;
		      break;
		    }
		  }
		}
		
		HBDSECTOR_trpl = triplet_sector;
		
		if(triplet_closest_pad<96){
		  HBDSIDE_trpl = 0;
		}
		else if(triplet_closest_pad>95){
		  HBDSIDE_trpl = 1;
		}
		HBDPADNUM_trpl = triplet_closest_pad;
		
		
		electron2.Reset();
		if ( !(HBDPADNUM_trpl<0 || HBDPADNUM_trpl==0 || HBDPADNUM_trpl==1 || HBDPADNUM_trpl==189 || HBDPADNUM_trpl==190 || HBDPADNUM_trpl==191) ){
		  electron2.set_bbcq(bbcq);
		  electron2.set_padx(PADX_trpl);
		  electron2.set_pady(PADY_trpl);
		  electron2.set_hbdsector(int(HBDSECTOR_trpl));      
		  electron2.set_hbdside(int(HBDSIDE_trpl));      
		  electron2.set_padcharge0(PADCHARGE0_trpl);      
		  electron2.set_padcharge1(PADCHARGE1_trpl);      
		  electron2.set_padcharge2(PADCHARGE2_trpl);      
		  electron2.set_padcharge3(PADCHARGE3_trpl);
		  electron2.set_padcharge4(PADCHARGE4_trpl);
		  electron2.set_padcharge5(PADCHARGE5_trpl);
		  electron2.set_padcharge6(PADCHARGE6_trpl);
		  electron2.set_padcharge7(PADCHARGE7_trpl);
		  electron2.set_padcharge8(PADCHARGE8_trpl);
		  electron2.set_padcharge9(PADCHARGE9_trpl);
		  electron2.set_hbdpadnum((int)HBDPADNUM_trpl);
		  electron2.set_padtype(HBDPADTYPE_trpl);
		  
		  electron2.set_padkey0(PADKEY0_trpl);      
		  electron2.set_padkey1(PADKEY1_trpl);      
		  electron2.set_padkey2(PADKEY2_trpl);      
		  electron2.set_padkey3(PADKEY3_trpl);
		  electron2.set_padkey4(PADKEY4_trpl);
		  electron2.set_padkey5(PADKEY5_trpl);
		  electron2.set_padkey6(PADKEY6_trpl);
		  electron2.set_padkey7(PADKEY7_trpl);
		  electron2.set_padkey8(PADKEY8_trpl);
		  electron2.set_padkey9(PADKEY9_trpl);
		  
		  electron2.set_locx(LOCX_trpl);
		  electron2.set_locy(LOCY_trpl);
		  electron2.set_hbdpz(triplet_z);
		}
		
		//Skip if it doesn't pass the cutter
		if (!cutter2->isHBDElectron(electron2)) continue;
		
		//Check for double counting
		bool check = false;
		for (int a=0;a<10;a++){
		  if (check) break;
		  if ( electron.get_usedpad(a) <=-999 ) continue;
		  for (int b=0;b<10;b++){
		    if ( electron2.get_usedpad(b) <=-999 ) continue;
		    if (electron.get_usedpad(a) == electron2.get_usedpad(b)){
		      //cout << "Same pad used twice !!!" << endl; 
		      check = true;
		      break;
		    }
		  }
		}
		
		
		//Skip the triplet if it contains some primary pads
		if (check){
		  //cout << "Skipping triplet - double counting..." << endl; 
		  continue;
		}

		//Skip the triplet if it contains double charge
		if (!cutter->isSingleElectron(centrality,electron2.get_hbdsize(),electron2.get_hbdcharge())){
		  //cout << "Skipping triplet - double charge..." << endl; 
		  continue;
		}
		
		//Continue looking if the combined charge is still a single
		if (cutter->isSingleElectron(centrality,
					     electron.get_hbdsize()+electron2.get_hbdsize(),
					     electron.get_hbdcharge()+electron2.get_hbdcharge()))
		  continue;
		
		//If a nearby hit is found
		ELECTRONTYPE = 4;
		
		break;
	      
	      }
	    }
	  }
	}
	else{
	  //This is an overlapping double hit that we reject
	  ELECTRONTYPE = 9;
	}
	// --- End of S/D rejection ----
	
      } 
      
      //Write the ELECTRONTYPE
      if (clst_flag == 3 || clst_flag == 4) 
	trk->get_track(jtrk)->set_hbdspokecharge2(ELECTRONTYPE);
    
    } // --- End of track loop ---
    
  } // --- End of (if ntrk>0) ---

  return 0;
}

int HbdMinPadClusterizer::End(PHCompositeNode *topNode)
{ // End

  delete premaker;
  delete cutter;
  if (sd_flag > 0) delete cutter2;

  return 0;
}

bool sort_by_distance(Pad const& first, Pad const& second){
  return first.distance < second.distance;
}

bool sort_Triplet_by_distance(Triplet const& first, Triplet const& second){
  return first.distance < second.distance;
}
