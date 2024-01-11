// $Id: MuonNdstAna.C,v 1.1 2009/07/04 18:33:52 hpereira Exp $
/*!
  \file    MuonNdstAna.cxx
  \ingroup supermodules 
  \brief   nanoDST analysis loop to read nanoDSTs, fill in ntuples/trees
  \author  Sean Kelly
  \version $Revision: 1.1 $
  \date    $Date: 2009/07/04 18:33:52 $
*/

#include "MuonNdstAna.h"
#include "TriggerHelper.h"
#include "PHTrackIntegratorKF.h"

// Mutoo 
#include "PHTFileServer.h"
#include <getClass.h>

#include<sstream>
#include<fstream>

using namespace std;

//______________________________________________
MuonNdstAna::MuonNdstAna( const char* name, const char* file ) : 
	SubsysReco( name ),
	_mutoo(0),
	_global(0),
	_filename( file ? file:"mwg_ntuple.root"),
	_single_track(0),
	_trk_chi_square_cut(20),
	_vtx_chi_square_cut(20),
	_vtx_z_cut(30),
	_hadron_depth_cut(3),
	_hadron_ptot_vertex_cut(2.8),
	_hadron_ptot_sta1_cut(1.8),
	_ievt(0),
	_npassed(0),
	_trig_selection(0),
	_timer( PHTimeServer::get()->insert_new( name ) )
{ }

//______________________________________________
int MuonNdstAna::InitRun(PHCompositeNode *top_node)
{
  static bool init_done __attribute__ ((unused)) = initialize();
  return 0;
}

//______________________________________________
void MuonNdstAna::set_node_ptrs(PHCompositeNode *topNode)
{
  
  // Look for ndst nodes from the node tree...
  //

  // New framework tracks
  //
  _mutoo = findNode::getClass<PHMuoTracksOut>(topNode, "PHMuoTracksOO");
  if (!_mutoo) cout << PHWHERE << "MuonNdstAna:: PHMuoTracks not in Node Tree" << endl;
  
  // PHGlobal information
  //
  _global = findNode::getClass<PHGlobal>(topNode, "PHGlobal");
  if (!_global) cout << PHWHERE << "MuonNdstAna:: PHGlobal not in Node Tree" << endl;

  return;
}

//______________________________________________
int MuonNdstAna::process_event(PHCompositeNode *top_node)
{
  _timer.get()->restart();
  try {    
    set_node_ptrs(top_node);
    if(_mutoo&&_global) {
      // incrementing event counter.
      //
      increment(MuonNdstAna::ALL);
      // Apply zvtx cut here.
      //
      if(!pass_zvtx_cut()) return 0;
      
      // Fill single mutoo track.
      //
      fill_single_track(top_node);

    }
  } catch (std::exception& e) {
    MUTOO::TRACE(e.what());
  }  
  _timer.get()->stop();
  
  return 0;
}

//______________________________________________
int MuonNdstAna::End(PHCompositeNode* top_node) 
{
  PHTFileServer::get().write( _filename );
	MUTOO::PRINT( cout, "MuonNdstAna::End" );
  cout << " NDST has been successfully produced. " << endl;
  cout << " Number of event processed : " << _ievt << ". "<< endl;
  cout << " Number of event passed trigger and vtx [-30cm, 30cm] cut " << _npassed << ". " <<endl;
  _timer.get()->print_stat();
	MUTOO::PRINT( cout, "**" );
  return 0;
}

//______________________________________________
bool MuonNdstAna::initialize()
{
  //
  // Output file
  //
 PHTFileServer::get().open( _filename,"recreate");  

  // Output TTree
  //
  _single_track = new TTree("_single_track", "mutoo single track tree");
  _single_track->Branch("T_p", &_p[0], "T_p[5]/D", 32000);
  _single_track->Branch("T_eta", &_eta, "T_eta/D", 32000);
  _single_track->Branch("T_phi", &_phi, "T_phi/D", 32000);
  _single_track->Branch("T_theta", &_theta, "T_theta/D", 32000);
  _single_track->Branch("T_chi_square", &_chi_square, "T_chi_square/D", 32000);
  _single_track->Branch("T_depth", &_depth, "T_depth/I", 32000);
  _single_track->Branch("T_theta_xp", &_theta_xp, "T_theta_xp/D", 32000);
  _single_track->Branch("T_pid", &_pid, "T_pid/I", 32000);
  _single_track->Branch("T_charge", &_charge, "T_charge/I", 32000);
  _single_track->Branch("T_z_vtx", &_z_vtx, "T_z_vtx/D", 32000);
  _single_track->Branch("T_cent", &_cent, "T_cent/D", 32000);

  // Histograms.
  //
  _hntracks       = new TH1F("_hntracks","number of tracks", 21, -0.5, 20.5); 
  _hnhadrons      = new TH1F("_hnhadrons","number of hadrons", 21, -0.5, 20.5); 

  _hcent          = new TH1F("_hcent","centrality for all events", 200, 0.0, 100.0);						      
  _hzvtx          = new TH1F("_hzvtx","zvtx for all events", 160, -40.0, 40.0);						      
  _hphi           = new TH1F("_hphi","phi distribution for all tracks", 361, -0.5, 360.5);					      
  _hgap0xy        = new TH2F("_hgap0xy","XY distribution at muid gap0 for all tracks", 1000, -500.0, 500.0, 1000, -500.0, 500.0);  
  _hst1xy         = new TH2F("_hst1xy","XY distribution at mutr sta1 for all tracks", 1000, -500.0, 500.0, 1000, -500.0, 500.0);   

  _hcent_hadron   = new TH1F("_hcent_hadron","centrality for all hadron events", 200, 0.0, 100.0);
  _hzvtx_hadron   = new TH1F("_hzvtx_hadron","zvtx for all hadron events", 160, -40.0, 40.0);
  _hphi_hadron    = new TH1F("_hphi_hadron","phi distribution for all hadrons", 361, -0.5, 360.5);
  _hgap0xy_hadron = new TH2F("_hgap0xy_hadron","XY distribution at muid gap0 for all hadrons", 1000, -500.0, 500.0, 1000, -500.0, 500.0);
  _hst1xy_hadron  = new TH2F("_hst1xy_hadron","XY distribution at mutr sta1 for all hadrons", 1000, -500.0, 500.0, 1000, -500.0, 500.0);

  return true;
}

//______________________________________________
void MuonNdstAna::fill_single_track(PHCompositeNode* topNode)
{

  // Look at the trigger first.
  //
  TriggerHelper* myTH = new TriggerHelper(topNode);
  //  TrigLvl1* lvl1_trig = myTH->get_trigLvl1();
  
  // Check trigger
  // 
  //
  static int first = 1;
  if(_trig_selection==1) {
    if(first) {
      int sScale  = myTH->getLevel1TriggerProperty("MUIDS_1D&BBCLL1",TriggerHelper::l1Scaledown);
      int nScale  = myTH->getLevel1TriggerProperty("MUIDN_1D&BBCLL1",TriggerHelper::l1Scaledown);
      cout << "  MUIDS_1D&BBCLL1 downscale is " << sScale;
      cout << "  MUIDN_1D&BBCLL1 downscale is " << nScale;
      cout << endl;
      first = 0;
    }
    if(!(myTH->didLevel1TriggerGetScaled("MUIDN_1D&BBCLL1"))&&
       !(myTH->didLevel1TriggerGetScaled("MUIDS_1D&BBCLL1"))){
      delete myTH;
      return;
    }
  } else {
    if(first) {
      int mbScale = myTH->getLevel1TriggerProperty("BBCLL1>=1",TriggerHelper::l1Scaledown);
      cout << "  BBCLL1>=1 downscale is " << mbScale << endl;
      first = 0;
    }
    if(!(myTH->IsEventMinBias())) {
      delete myTH;
      return;
    }
  }      
  
  increment(MuonNdstAna::PASSED);

  // Cache format so we can restore
  //

  int ntrk = _mutoo->get_npart();
  _hntracks->Fill(float(ntrk));

  int n_had = 0;

  init_single_tree();

  for (int itrk=0; itrk<ntrk; itrk++){
    
    // No ghost tracks, but in mutoo tracks by defination are not ghosts.
    //
    if (_mutoo->get_ghostflag(itrk)) continue;

    if (!pass_trk_chi_square_cut(itrk)) continue;

    // Do the momentum corrections.
    // 
    // TMutTrkPar hadron_trk_par( propagate_hadron(itrk) );

    // First index in get_px gives the location of 
    // where we measured the px, "0" means at vtx.
    //
    
    _p[0]  = _mutoo->get_px(0,itrk);
    _p[1]  = _mutoo->get_py(0,itrk);
    _p[2]  = _mutoo->get_pz(0, itrk);
    _p[3]  = sqrt(_p[0]*_p[0]+_p[1]*_p[1]);
    _p[4]  = sqrt(_p[0]*_p[0]+_p[1]*_p[1]+_p[2]*_p[2]);
    _phi   = get_phi(_mutoo->get_px(0,itrk),
		    _mutoo->get_py(0,itrk));
    _theta = get_theta(_mutoo->get_px(0,itrk),
		       _mutoo->get_py(0,itrk),
		       _mutoo->get_pz(0,itrk));
    _eta   = get_eta(_mutoo->get_px(0,itrk),
		     _mutoo->get_py(0,itrk),
		     _mutoo->get_pz(0,itrk));
    _chi_square = get_chi_square(itrk);
    _charge = _mutoo->get_charge(itrk);
    _theta_xp = hadron_theta_xp(itrk);
    _z_vtx = _global->getBbcZVertex();
    _cent  = _global->get_dAuBbcCentrality();
    _depth = get_max_depth(itrk);

    // tag on hadrons.
    //
    if(is_hadron(itrk)) {
      n_had++;
      _pid = 1;
      _hphi_hadron->Fill(_phi);
      _hgap0xy_hadron->Fill(_mutoo->get_muIDOO_gap0(0,_depth-2,itrk),
			    _mutoo->get_muIDOO_gap0(1,_depth-2,itrk));
      _hst1xy_hadron->Fill(_mutoo->get_xpos(1,itrk),
			   _mutoo->get_ypos(1,itrk));
    } else {
      _pid = 0;
    }
    _hphi->Fill(_phi);
    _hgap0xy->Fill(_mutoo->get_muIDOO_gap0(0,_depth-2,itrk),
		   _mutoo->get_muIDOO_gap0(1,_depth-2,itrk));
    _hst1xy->Fill(_mutoo->get_xpos(1,itrk),
		  _mutoo->get_ypos(1,itrk));

    _single_track->Fill();
  }
 
  _hnhadrons->Fill(float(n_had));
  
  if(n_had>0) {
    _hcent_hadron->Fill(_global->get_dAuBbcCentrality());
    _hzvtx_hadron->Fill(_global->getBbcZVertex());
  }
  _hcent->Fill(_global->get_dAuBbcCentrality());
  _hzvtx->Fill(_global->getBbcZVertex());

  if(myTH) delete myTH;
}

//______________________________________________
Double_t MuonNdstAna::get_phi(float px, float py) const 
{
  if(py > 0 ) return 180.0*atan2(py,px)/M_PI;
  else if(py < 0 ) return 180.0*(2.0+atan2(py,px)/M_PI);
  else return 0;
}

//______________________________________________
Double_t MuonNdstAna::get_theta(float px, float py, float pz) const 
{
  Double_t pt = sqrt(px*px+py*py);
  Double_t theta = atan(pt/pz);
  if(pz>0) return theta*180.0/M_PI;
  else if(pz<0) return (M_PI+theta)*180/M_PI;
  else return 0;
}

//______________________________________________
Double_t MuonNdstAna::get_eta(float px, float py, float pz) const 
{
  float ptot = sqrt(px*px+py*py+pz*pz);
  Double_t eta = 0.5*log((ptot+pz)/(ptot-pz));
  return eta;
}

//______________________________________________
bool  MuonNdstAna::pass_zvtx_cut() {
  
  if(fabs(_global->getBbcZVertex()) > get_vtx_z_cut()) return false;
  return true;

}

//______________________________________________
bool  MuonNdstAna::pass_trk_chi_square_cut(const int trk_index) 
{
  
  if( get_chi_square(trk_index) > get_trk_chi_square_cut()) return false;
  return true;

}

//______________________________________________
bool  MuonNdstAna::pass_vtx_chi_square_cut(const float vtx_chi)
{
  
  if(vtx_chi > get_vtx_chi_square_cut()) return false;
  return true;
}

//______________________________________________
Int_t MuonNdstAna::get_max_depth(int trk_index) const 
{
  // Each new framework track could associates with three muid roads,
  // one road for each gap from gap 2 to gap4 if it is possible, here,
  // we select the deepest road. 
  
  UShort_t last_gap = 0;
  
  for(int road_index = 2; road_index >= 0; road_index--) {
    if(_mutoo->get_muIDOOhits(road_index, trk_index)) {
      // When we filled the muIDOOhit array, we filled in the following way,
      // the arry index of the gap bit of the associated road is the depth of the road minus 2.
      //
      last_gap = road_index+2;
      break;
    }
  }

  return  last_gap;
}

//______________________________________________
Double_t MuonNdstAna::get_chi_square(int trk_index) const 
{
      
  int nhits = 0;
  // get the mutr hit pattern. we have 16 cathode planes, so
  // maximium number of hits is 16.
  //
  int mutr_hit_bit = _mutoo->get_muTRhits(trk_index);
  for(int ihit = 0; ihit < 16; ihit++) {
    if(mutr_hit_bit&(0x0001<<ihit)) nhits++;
  }

  // number of freedom is number of mesurements ( nhits ) minus
  // number of fit parameters from Kalman filter, which is 5 ( px, py, pz, x, y)
  //
  int nfreedom = nhits - 5;

  // chi_square per freedom.
  //
  return _mutoo->get_chisquare(trk_index)/(1.0*nfreedom);
}

//______________________________________________
bool MuonNdstAna::is_hadron(int trk_index) const 
{

  bool is_hadron = false;
  // First check on chi_squre.
  // 
  //  float chi = get_chi_square(trk_index);
  // if(chi > get_trk_chi_square_cut()) return is_hadron;

  float ptot = sqrt(_mutoo->get_px(1,trk_index)*_mutoo->get_px(1,trk_index)+_mutoo->get_py(1,trk_index)*_mutoo->get_py(1,trk_index)+_mutoo->get_pz(1,trk_index)*_mutoo->get_pz(1,trk_index));

  // If momentum-depth matching for hadrons.
  //
  if(ptot>=get_hadron_ptot_sta1_cut()&&get_max_depth(trk_index)<=get_hadron_depth_cut()&&get_max_depth(trk_index)>0) is_hadron = true;

  return is_hadron;
}

//______________________________________________
TMutTrkPar MuonNdstAna::propagate_hadron(int trk_index) const 
{

  // Make a TMutTrkPar from track momentum at station 1, and position of the most
  // upstream cathode plane of station 1.
  TMutTrkPar local_par(_mutoo->get_xpos(1,trk_index),
		       _mutoo->get_ypos(1,trk_index),
		       _mutoo->get_zpos(1,trk_index),
		       _mutoo->get_px(1,trk_index),
		       _mutoo->get_py(1,trk_index),
		       _mutoo->get_pz(1,trk_index),
		       _mutoo->get_charge(trk_index),
		       _mutoo->get_chisquare(trk_index));
  
  float bbc_z = _global->getBbcZVertex();
  
  // create track integrator, set particle as a pion
  PHTrackIntegratorKF integrator;
  integrator.set_particle_type( PHTrackIntegratorKF::PION );
  integrator.initialize( local_par );
  integrator.extrapolate( bbc_z );
  
  // if extrapolation is OK, update the local_trk_par
  if( integrator.get_error() ) 
    cerr << "MuonNdstAna::propagate_hadron - extrapolation failed.\n";
  integrator.finish( local_par );
  
  return local_par;
}

//______________________________________________
Double_t MuonNdstAna::hadron_theta_xp(int index) const 
{

  // Polar angle of primary vector.
  //
  float theta_pri = atan2(sqrt(_mutoo->get_xpos(1,index)*_mutoo->get_xpos(1,index)
			       +_mutoo->get_ypos(1,index)*_mutoo->get_ypos(1,index)),
			  (_mutoo->get_zpos(1,index)-_global->getBbcZVertex()));
  // polar angle of sta1 vector.
  float theta_sta1= atan2(sqrt(_mutoo->get_px(1,index)*_mutoo->get_px(1,index)
			       +_mutoo->get_py(1,index)*_mutoo->get_py(1,index)),
			  _mutoo->get_pz(1,index));
  return fabs(theta_pri-theta_sta1);
}


