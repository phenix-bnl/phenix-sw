// $Id: mMutStripOccup.cxx,v 1.13 2017/07/11 16:13:14 phnxbld Exp $
/////////////////////////////////////////////////////////////////////////
//
// Utility class : mMutStripOccup:
// Author: chun zhang
// Date: 6/17/02
// Description : fill histograms for occupancy studies, out put a text file
//               to evalute the statues of the strips.
//
/////////////////////////////////////////////////////////////////////////

// MUTOO headers
//
#include <mMutStripOccup.h>
#include <mMutStripOccupPar.h>
#include <TMutHitMap.h>
#include <TMutNode.h>
#include <TMutKeyGen.h>
#include <PHException.h>
#include <MUTOO.h>
#include <MUTGEOM.h>
#include <PHGeometry.h>
#include <TMutGeo.h>
#include <MutGeom.h>
#include <MutStrip.h>

#include<iostream>
#include<fstream>
#include<TFile.h>
#include<string>

using namespace std;

/*!
Constructor
*/
mMutStripOccup::mMutStripOccup()
{
  MUTOO::TRACE("initializing module mMutStripOccup",MUTOO::ALOT);
  _occup_map();
  nevent = 0;
}

/*!
Event method
*/

mMutStripOccup::Occup_map&
mMutStripOccup::init_map() 
{
  TH1F* charge_hist;
  static Occup_map mymap;
  
  cout << " we are initializing the occupancy map in mMutStripOccup" << endl; 
  for( int iarm = 0; iarm < 2; iarm++){
    const MUTGEOM::ArmNumber arm = MUTGEOM::ArmNumber(iarm);
    MutArm* thisarm = new MutArm(arm);
    for( int ista = 0; ista < 3; ista++){
      const MUTGEOM::StationNumber sta = MUTGEOM::StationNumber(ista);
      MutStation* thisstation = new MutStation((const MutArm*)thisarm, sta);
      for( int ioct = 0; ioct < 8; ioct++){
	const MUTGEOM::OctantNumber oct = MUTGEOM::OctantNumber(ioct);
	MutOctant* thisoctant = new MutOctant((const MutArm*)thisarm,
					      (const MutStation*)thisstation,
					      oct);
	for( int ihoc = 0; ihoc < 2; ihoc++){
	  const MUTGEOM::HalfOctantNumber hoc = MUTGEOM::HalfOctantNumber(ihoc);
	  MutHalfOctant* thishalf = new MutHalfOctant((const MutArm*)thisarm,
						      (const MutStation*)thisstation,
						      (const MutOctant*)thisoctant,
						      hoc);
	  for( int igap = 0; igap < 3; igap++){
	    if(ista==2 && igap ==2) continue;
	    const MUTGEOM::GapNumber gap = MUTGEOM::GapNumber(igap);
	    MutGap* thisgap = new MutGap((const MutArm*)thisarm,
					 (const MutStation*)thisstation,
					 (const MutOctant*)thisoctant,
					 (const MutHalfOctant*)thishalf,
					 gap);
	    for( int icat = 0; icat < 2; icat++){
	      MUTGEOM::PlaneNumber cat;
	      if(icat == 0 ) {
		cat = MUTGEOM::PlaneNumber(0);
	      }
	      else {
		// skip anode plane.
		//
		cat = MUTGEOM::PlaneNumber(2);
	      }
	      MutPlane* thisplane = new MutPlane((const MutArm*)thisarm,
						 (const MutStation*)thisstation,
						 (const MutOctant*)thisoctant,
						 (const MutHalfOctant*)thishalf,
						 (const MutGap*)thisgap,
						 (const MUTGEOM::PlaneNumber&)cat);  
	      int total_strips = thisplane->getNumElements();
	      for( int istrip = 0; istrip < total_strips; istrip++){
		TMutKeyGen::key_type key = TMutKeyGen::get_key(iarm,
							       ista,
							       ioct,
							       ihoc,
							       igap,
							       icat,
							       istrip);
		char buff[100];
		TString title;
		sprintf(buff, "strips_%d_cath%d_gap%d_half%d_octant%d_sta%d_arm%d", istrip, icat, igap, ihoc, ioct, ista,iarm);
		title = buff;
		charge_hist= new TH1F(title, title, total_strips, 0, total_strips);
		//MutStripOccup mystrip = *(new MutStripOccup(0,0,0,0,0,charge_hist)); 
		MutStripOccup mystrip(0,0,0,0,0,charge_hist);     
		mymap.insert(std::make_pair(key,mystrip));
	      } // end of istrip
            } // end of icat
	  } // end of igap
	} // end of ihoc
      }// end of ioct 
    }// end of ista
  } // end of iarm
  
  return mymap;
}

mMutStripOccup::Occup_map &
mMutStripOccup::_occup_map() 
{
  static Occup_map& occup_map = init_map();
  return occup_map;
}
PHBoolean mMutStripOccup::event(PHCompositeNode* top_node)
{
  try {
    // Reset IOC pointers
    //
    set_interface_ptrs(top_node);
    if((_mod_par->get_switch_on())==0) return True;
    // fill occupancy histograms 

    nevent=nevent+1;    
    if(nevent % 10000 == 0 ) cout<< " we are processing event " << nevent << "for occupancy QA" << endl; 
    fill_map();

  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
    return False;
  }

  return True;
}

void mMutStripOccup::set_interface_ptrs(PHCompositeNode* top_node) {
  // module runtime parameters
  //
  _mod_par = TMutNode<mMutStripOccupPar>::find_node(top_node, "mMutStripOccupPar");
  // TMutHit IOC
  //
  _hit_map = TMutNode<TMutHitMap>::find_node(top_node, "TMutHitMap");
}


void mMutStripOccup::fill_map() {

  TMutHitMap::iterator hit_iter = _hit_map->range();
  const MutStrip* TheStrip;
  PHPoint strip_begin, strip_end;

  while(TMutHitMap::pointer hit_ptr = hit_iter.next()){
    // get the location of current hit 
    //
    int iarm     = hit_ptr->get()->get_arm();
    int istation = hit_ptr->get()->get_station();
    int ioctant  = hit_ptr->get()->get_octant();
    int ihoct    = hit_ptr->get()->get_half_octant();
    int igap     = hit_ptr->get()->get_gap();
    int icath    = hit_ptr->get()->get_cathode();
    int istrip   = hit_ptr->get()->get_strip();
    float qvalue = hit_ptr->get()->get_q();
    if((_mod_par->get_charge_cut())>qvalue) continue;
    TheStrip = TMutGeo::get_strip_geom(iarm,istation,ioctant,ihoct,igap,icath, istrip);
    strip_begin = TheStrip->getGlobalPositionBegin();
    strip_end   = TheStrip->getGlobalPositionEnd();
    float strip_length=sqrt((strip_begin.getX()-strip_end.getX())*(strip_begin.getX()-strip_end.getX())+
			   (strip_begin.getY()-strip_end.getY())*(strip_begin.getY()-strip_end.getY())+
			   (strip_begin.getZ()-strip_end.getZ())*(strip_begin.getZ()-strip_end.getZ()));
    
    TMutKeyGen::key_type key = TMutKeyGen::get_key(iarm,
						   istation,
						   ioctant,
						   ihoct,
						   igap,
						   icath,
						   istrip);
    Occup_map::iterator iter = _occup_map().find(key);

    if(iter == _occup_map().end()) {
      cout << " we hit the end of the map. " << endl;
      continue;
    }
    
    iter->second.set_occup_sum();
    iter->second.set_charge(Axis_t(qvalue));
    iter->second.set_q_ava();
    iter->second.set_q_rms();
    if(strip_length==0 ) continue;
    float occup_ava = (iter->second.get_occup_sum())/strip_length/nevent;
    iter->second.set_occup_ava(occup_ava);
  }
}
    
void mMutStripOccup::fill_hist(TString rootfile) {

  if((_mod_par->get_switch_on())==0) return ;
  TFile* outroot = new TFile(rootfile, "RECREATE");
  outroot->cd();
  TH1F* occuhist;
  TH1F* qavahist;
  TH1F* qrmshist;

  for( int iarm = 0; iarm < 2; iarm++){
    const MUTGEOM::ArmNumber arm = MUTGEOM::ArmNumber(iarm);
    MutArm* thisarm = new MutArm(arm);
    for( int ista = 0; ista < 3; ista++){
      const MUTGEOM::StationNumber sta = MUTGEOM::StationNumber(ista);
      MutStation* thisstation = new MutStation((const MutArm*)thisarm, sta);
      for( int ioct = 0; ioct < 8; ioct++){
	const MUTGEOM::OctantNumber oct = MUTGEOM::OctantNumber(ioct);
	MutOctant* thisoctant = new MutOctant((const MutArm*)thisarm,
					      (const MutStation*)thisstation,
					      oct);
	for( int ihoc = 0; ihoc < 2; ihoc++){
	  const MUTGEOM::HalfOctantNumber hoc = MUTGEOM::HalfOctantNumber(ihoc);
	  MutHalfOctant* thishalf = new MutHalfOctant((const MutArm*)thisarm,
						      (const MutStation*)thisstation,
						      (const MutOctant*)thisoctant,
						      hoc);
	  for( int igap = 0; igap < 3; igap++){
	    if(ista==2 && igap ==2) continue;
	    const MUTGEOM::GapNumber gap = MUTGEOM::GapNumber(igap);
	    MutGap* thisgap = new MutGap((const MutArm*)thisarm,
					 (const MutStation*)thisstation,
					 (const MutOctant*)thisoctant,
					 (const MutHalfOctant*)thishalf,
					 gap);
	    if(ista==2 && igap ==2) continue;
	    for( int icat = 0; icat < 2; icat++){
	      MUTGEOM::PlaneNumber cat;
	      if(icat == 0 ) {
		cat = MUTGEOM::PlaneNumber(0);
	      }
	      else {
		// skip anode plane.
		//
		cat = MUTGEOM::PlaneNumber(2);
	      }
	      MutPlane* thisplane = new MutPlane((const MutArm*)thisarm,
						 (const MutStation*)thisstation,
						 (const MutOctant*)thisoctant,
						 (const MutHalfOctant*)thishalf,
						 (const MutGap*)thisgap,
						 (const MUTGEOM::PlaneNumber&)cat);  
	      int total_strips = thisplane->getNumElements();
	      char buff[100];
	      TString title;
	      sprintf(buff, "occup_cath%d_gap%d_half%d_octant%d_sta%d_arm%d", icat, igap, ihoc, ioct, ista, iarm);
	      title = buff;
	      occuhist=new TH1F(title, title, total_strips, 0, float(total_strips));
	      sprintf(buff, "qava_cath%d_gap%d_half%d_octant%d_sta%d_arm%d", icat, igap, ihoc, ioct, ista, iarm);
	      title = buff;
	      qavahist=new TH1F(title, title, total_strips, 0, float(total_strips));
	      sprintf(buff, "qrms_cath%d_gap%d_half%d_octant%d_sta%d_arm%d", icat, igap, ihoc, ioct, ista,iarm);
	      title = buff;
	      qrmshist=new TH1F(title, title, total_strips, 0, float(total_strips));
	      for( int istrip = 0; istrip < total_strips; istrip++){
		TMutKeyGen::key_type key = TMutKeyGen::get_key(iarm,
							       ista,
							       ioct,
							       ihoc,
							       igap,
							       icat,
							       istrip);
		Occup_map::iterator iter = _occup_map().find(key);
		if(iter == _occup_map().end()) continue;
		occuhist->SetBinContent((istrip+1),(iter->second.get_occup_ava()));
		qavahist->SetBinContent((istrip+1),(iter->second.get_q_ava()));
		qrmshist->SetBinContent((istrip+1),(iter->second.get_q_rms()));
	      } // end of istrip
	    } // end of icat
	  } // end of igap
	}// end of ihoc
      } // end of ioct
    } // end of ista
  }// end of iarm
  outroot->Write();
}

void mMutStripOccup::fill_text(TString filename) {
  
  if((_mod_par->get_switch_on())==0) return;
  if(!filename) filename="txt_out.txt";
  int istate = 0;
  std::cout << " we are writing the output txt file" << filename <<std::endl;
  ofstream fout(filename);
  fout << "# Q/A ascii file" <<endl;
  fout << "# run 31503 beta version only staton0, octant0"<<endl;
  fout << "# format-- iarm::istation::igap::iplane::ioctant::ihalf::istrip::istate::occup::qmean::qrms" << endl;

  for( int iarm = 0; iarm < 2; iarm++){
    const MUTGEOM::ArmNumber arm = MUTGEOM::ArmNumber(iarm);
    MutArm* thisarm = new MutArm(arm);
    for( int ista = 0; ista < 3; ista++){
      const MUTGEOM::StationNumber sta = MUTGEOM::StationNumber(ista);
      MutStation* thisstation = new MutStation((const MutArm*)thisarm, sta);
      for( int ioct = 0; ioct < 8; ioct++){
	const MUTGEOM::OctantNumber oct = MUTGEOM::OctantNumber(ioct);
	MutOctant* thisoctant = new MutOctant((const MutArm*)thisarm,
					      (const MutStation*)thisstation,
					      oct);
	for( int ihoc = 0; ihoc < 2; ihoc++){
	  const MUTGEOM::HalfOctantNumber hoc = MUTGEOM::HalfOctantNumber(ihoc);
	  MutHalfOctant* thishalf = new MutHalfOctant((const MutArm*)thisarm,
						      (const MutStation*)thisstation,
						      (const MutOctant*)thisoctant,
						      hoc);
	  for( int igap = 0; igap < 3; igap++){
	   if(ista==2 && igap ==2) continue;
	   const MUTGEOM::GapNumber gap = MUTGEOM::GapNumber(igap);
	    MutGap* thisgap = new MutGap((const MutArm*)thisarm,
					 (const MutStation*)thisstation,
					 (const MutOctant*)thisoctant,
					 (const MutHalfOctant*)thishalf,
					 gap);
	    if(ista==2 && igap ==2) continue;
	    for( int icat = 0; icat < 2; icat++){
	      MUTGEOM::PlaneNumber cat;
	      if(icat == 0 ) {
		cat = MUTGEOM::PlaneNumber(0);
	      }
	      else {
		// skip anode plane.
		//
		cat = MUTGEOM::PlaneNumber(2);
	      }
	      MutPlane* thisplane = new MutPlane((const MutArm*)thisarm,
						 (const MutStation*)thisstation,
						 (const MutOctant*)thisoctant,
						 (const MutHalfOctant*)thishalf,
						 (const MutGap*)thisgap,
						 (const MUTGEOM::PlaneNumber&)cat);  
	      int total_strips = thisplane->getNumElements();
	      for( int istrip = 0; istrip < total_strips; istrip++){
		TMutKeyGen::key_type key = TMutKeyGen::get_key(iarm,
							       ista,
							       ioct,
							       ihoc,
							       igap,
							       icat,
							       istrip);
		Occup_map::iterator iter = _occup_map().find(key);
		if(iter == _occup_map().end()) continue;
		if((iter->second.get_occup_ava()) < 0.00000001) istate = 1;
		if((iter->second.get_occup_ava()) > 0.000025 &&
		   (iter->second.get_q_ava()) > 75 ) istate = 2;
		if((iter->second.get_occup_ava()) > 0.000025 &&
		   (iter->second.get_q_ava()) < 75 ) istate = 3;
		fout << iarm <<" " ;
		fout << ista <<" " ;
		fout << ioct <<" " ;
		fout << ihoc <<" " ;
		fout << igap <<" " ;
		fout << icat <<" " ;
		fout << istrip <<" " ;
		fout << istate << " " ;
		fout << (iter->second.get_occup_ava())<<" ";
		fout << (iter->second.get_q_ava())<<" ";
		fout << (iter->second.get_q_rms())<<endl;
		
	      } // end of istrip
	    } // end of icat
	  } // end of igap
	}// end of ihoc
      } // end of ioct
    } // end of ista
  }// end of iarm
  cout << " we are closing the output text file." << endl;
  fout.close(); 
}

void mMutStripOccup::clean_up(){
  _occup_map().clear();
}










