// $Id: mMutFindVtx.cxx,v 1.21 2011/12/24 04:48:30 slash Exp $

//////////////////////////////////////////////////////////////////
/*
  \file mMutFindVtx.cxx
  \brief associate tracks to make vertex objects
  \author S.Kelly 
  \version $Revision: 1.21 $
  \date    $Date: 2011/12/24 04:48:30 $
*/
//////////////////////////////////////////////////////////////////

// MUTOO headers
//
#include <mMutFindVtx.h>
#include <mMutFindVtxPar.h>
#include <TMutNode.h>
#include <PHException.h>
#include <MUTOO.h>
#include <TMutGeo.h>

// PHENIX headers
//
#include<TMutCoordMap.h>
#include<TMutStubMap.h>
#include<PHGeometry.h>

// STL/BOOST
//
#include <iostream>
#include <string>
#include <boost/array.hpp>
#include <list>

using namespace std;

//___________________________________________________________
mMutFindVtx::mMutFindVtx() : 
  _timer(PHTimeServer::get()->insert_new("mMutFindVtx"))
{
  MUTOO::TRACE("initializing module mMutFindVtx",MUTOO::ALOT);
}

//___________________________________________________________
PHBoolean mMutFindVtx::event(PHCompositeNode* top_node)
{

  _timer.get()->restart(); 
  
  try { 
    
    // Reset IOC pointers
    set_interface_ptrs(top_node);
    find_pairs();

    if(_mod_par->get_use_nagle_fit()) {
      nagle_fit();
    } else {
      update_pairs();
    }
    
  } catch(std::exception& e) {    

    MUTOO::TRACE(e.what());
    return False;

  }  

  _timer.get()->stop();
  if(_mod_par->get_verbosity() >= MUTOO::SOME) _timer.get()->print();  
  if(_mod_par->get_verbosity() >= MUTOO::ALOT) _vtx_map->print();  
  
  return True;
}

//___________________________________________________________
void mMutFindVtx::set_interface_ptrs(PHCompositeNode* top_node)
{  
  
  // module runtime parameters
  //
  _mod_par = TMutNode<mMutFindVtxPar>::find_node(top_node,"mMutFindVtxPar");
  
  // TMutTrk IOC
  //
  _trk_map = TMutNode<TMutTrkMap>::find_node(top_node,"TMutTrkMap");
  
  // TMutVtx IOC
  //
  _vtx_map = TMutNode<TMutVtxMap>::find_node(top_node,"TMutVtxMap");
  
}

//___________________________________________________________
void mMutFindVtx::find_pairs()
{
  
  for(int arm=0; arm<MUTOO::NumberOfArms; ++arm)
  {
  
    // Associate all unique pairs of TMutTrk with a vertex object
    TMutTrkMap::iterator trk_iter1 = _trk_map->get(arm);
    while(TMutTrkMap::pointer trk_ptr1 = trk_iter1.next()){
      
      if( !accept_trk( trk_ptr1 ) ) continue;
      
      TMutTrkMap::iterator trk_iter2 = trk_iter1;
      if(trk_iter2.at_end()) break;
      while(TMutTrkMap::pointer trk_ptr2 = trk_iter2.next())
	    if( accept_trk( trk_ptr2 ) ) {
    	  TMutVtxMap::iterator vtx_iter = _vtx_map->insert_new(trk_ptr2->get()->get_arm());
	      PHKey::associate(vtx_iter.current(), trk_ptr1);
	      PHKey::associate(vtx_iter.current(), trk_ptr2);
	    } // loop over/check on track 2
    }   // loop over tracks 1
    

  }     // loop over arm
  
}

//___________________________________________________________
void mMutFindVtx::update_pairs()
{
  // Write the vertex interface from the TMutTrkPar at vertex (This mode
  // assumes that the tracks have already been extrapolated to the
  // vertex and corrected for energy loss)
  //
  float xvertex(0), yvertex(0), zvertex(0);
  TMutVtxMap::iterator vtx_iter = _vtx_map->range();
  while(TMutVtxMap::pointer vtx_ptr = vtx_iter.next()) {
    
    double px1=0, py1=0, pz1=0, charge1=0, px2=0, py2=0, pz2=0, charge2=0;    
    
    TMutTrkMap::const_key_iterator trk_iter = vtx_ptr->get()->get_associated<TMutTrk>();
    unsigned short which=0;
    while(TMutTrkMap::const_pointer trk_ptr = trk_iter.next()){
      which = (which == 0) ? 1 : 0;
      TMutStubMap::const_key_iterator stub_iter = trk_ptr->get()->get_associated<TMutStub>();
      while(TMutStubMap::const_pointer stub_ptr = stub_iter.next()){
	
	if(stub_ptr->get()->get_station() != MUTOO::Station2) continue;
	
	if(which==1) {
	  px1 = (trk_ptr->get()->get_trk_par_vtx())->get_px();
	  py1 = (trk_ptr->get()->get_trk_par_vtx())->get_py();
	  pz1 = (trk_ptr->get()->get_trk_par_vtx())->get_pz();
	  xvertex = (trk_ptr->get()->get_trk_par_vtx())->get_x();
	  yvertex = (trk_ptr->get()->get_trk_par_vtx())->get_y();
	  zvertex = (trk_ptr->get()->get_trk_par_vtx())->get_z();
	  charge1 = trk_ptr->get()->get_charge();
	} else {
	  px2 = (trk_ptr->get()->get_trk_par_vtx())->get_px();
	  py2 = (trk_ptr->get()->get_trk_par_vtx())->get_py();
	  pz2 = (trk_ptr->get()->get_trk_par_vtx())->get_pz();
	  charge2 = trk_ptr->get()->get_charge();
	}
      } // close TMutStub
    } // close TMutTrk loop

    // fill vertex
    vtx_ptr->get()->set_x(xvertex);
    vtx_ptr->get()->set_y(yvertex);
    vtx_ptr->get()->set_z(zvertex);
   
	  vtx_ptr->get()->set_px1(px1);
	  vtx_ptr->get()->set_py1(py1);
	  vtx_ptr->get()->set_pz1(pz1);
	  vtx_ptr->get()->set_charge1(charge1);
    
	  vtx_ptr->get()->set_px2(px2);
	  vtx_ptr->get()->set_py2(py2);
	  vtx_ptr->get()->set_pz2(pz2);
	  vtx_ptr->get()->set_charge2(charge2);

  }
}

//___________________________________________________________
void mMutFindVtx::nagle_fit()
{
  // Correct TMutTrkPar for eloss and write to vertex object at nominal PHENIX vertex
  //
  static boost::array<double,4> theta_17 = {{1.135760, 1.026590, -0.000003, -1.034864}};
  static boost::array<double,4> theta_23 = {{1.180041, 1.027412, -0.000018, -1.146959}};
  static boost::array<double,4> theta_28 = {{1.218594, 1.031854, -0.000023, -1.173510}};
  static boost::array<double,4> theta_33 = {{1.264924, 1.035123, -0.000038, -1.269907}};
  
  float xvertex(0), yvertex(0), zvertex(0);
  TMutVtxMap::iterator vtx_iter = _vtx_map->range();
  while(TMutVtxMap::pointer vtx_ptr = vtx_iter.next()) {

    double px1=0, py1=0, pz1=0, charge1=0, px2=0, py2=0, pz2=0, charge2=0;    

    TMutTrkMap::const_key_iterator trk_iter = vtx_ptr->get()->get_associated<TMutTrk>();
    unsigned short which=0;
    while(TMutTrkMap::const_pointer trk_ptr = trk_iter.next()){
      xvertex = trk_ptr->get()->get_trk_par_vtx()->get_x();
      yvertex = trk_ptr->get()->get_trk_par_vtx()->get_y();
      zvertex = trk_ptr->get()->get_trk_par_vtx()->get_z();
      which = (which == 0) ? 1 : 0;
      TMutStubMap::const_key_iterator stub_iter = trk_ptr->get()->get_associated<TMutStub>();
      while(TMutStubMap::const_pointer stub_ptr = stub_iter.next()){
	
	if(stub_ptr->get()->get_station() != MUTOO::Station2) continue;
	
	double r = std::sqrt(MUTOO::SQUARE(stub_ptr->get()->get_fit_par()->get_x()) +
			     MUTOO::SQUARE(stub_ptr->get()->get_fit_par()->get_y()));	  
	
	// theta from station 2 stub
	double theta_deg = MUTOO::RAD_TO_DEG*std::atan2(r, std::fabs(stub_ptr->get()->get_fit_par()->get_z()-zvertex));	
	
	// total momentum at the vertex using parameterization by JN and reconstructed momentum
	double p_vertex=0;	
	if(theta_deg < 17) {
	  p_vertex = calc_p_vertex(theta_17,trk_ptr->get()->get_trk_par()->get_ptot());
	} else if( theta_deg > 17 && theta_deg <= 23) {
	  double w = (theta_deg-17)/5.0;	  
	  double p_1 = calc_p_vertex(theta_17,trk_ptr->get()->get_trk_par()->get_ptot());
	  double p_2 = calc_p_vertex(theta_23,trk_ptr->get()->get_trk_par()->get_ptot());
	  p_vertex = (1-w)*p_1 + w*p_2;
	} else if( theta_deg > 23 && theta_deg <= 28) {
	  double w = (theta_deg-23)/5.0;	  
	  double p_1 = calc_p_vertex(theta_23,trk_ptr->get()->get_trk_par()->get_ptot());
	  double p_2 = calc_p_vertex(theta_28,trk_ptr->get()->get_trk_par()->get_ptot());
	  p_vertex = (1-w)*p_1 + w*p_2;
	} else {
	  p_vertex = calc_p_vertex(theta_33,trk_ptr->get()->get_trk_par()->get_ptot());
	}
	
	double phi = std::atan2(stub_ptr->get()->get_fit_par()->get_y(),
				stub_ptr->get()->get_fit_par()->get_x());
	
	double theta = std::atan2(r,stub_ptr->get()->get_fit_par()->get_z());	
	
	if(which==1){
	  px1 = p_vertex*std::sin(theta)*std::cos(phi);
	  py1 = p_vertex*std::sin(theta)*std::sin(phi);
	  pz1 = p_vertex*std::cos(theta);
	  charge1 = trk_ptr->get()->get_charge();
	} else {
	  px2 = p_vertex*std::sin(theta)*std::cos(phi);
	  py2 = p_vertex*std::sin(theta)*std::sin(phi);
	  pz2 = p_vertex*std::cos(theta);
	  charge2 = trk_ptr->get()->get_charge();
	}
      } // close TMutStub
    } // close TMutTrk loop

    // fill vertex
    vtx_ptr->get()->set_x(xvertex);
    vtx_ptr->get()->set_y(yvertex);
    vtx_ptr->get()->set_z(zvertex);
	  vtx_ptr->get()->set_px1(px1);
	  vtx_ptr->get()->set_py1(py1);
	  vtx_ptr->get()->set_pz1(pz1);
	  vtx_ptr->get()->set_charge1(charge1);
    
	  vtx_ptr->get()->set_px2(px2);
	  vtx_ptr->get()->set_py2(py2);
	  vtx_ptr->get()->set_pz2(pz2);
	  vtx_ptr->get()->set_charge2(charge2);

  }
}

//___________________________________________________________
double mMutFindVtx::calc_p_vertex(const boost::array<double,4>& pars, double p_trk)
{ return pars[0] + pars[1]*p_trk; }

//___________________________________________________________
bool mMutFindVtx::accept_trk( TMutTrkMap::const_pointer trk_ptr ) const
{
  
  // require track is reconstructed and non ghost
  if( !trk_ptr->get()->get_reco_success()|| trk_ptr->get()->get_ghost() ) 
  { return false; }

  // check stubs if required
  if( _mod_par->get_check_stubs() && !( 
    trk_ptr->get()->has_stub(MUTOO::Station1) && 
    trk_ptr->get()->has_stub(MUTOO::Station2) && 
    trk_ptr->get()->has_stub(MUTOO::Station3) ) ) 
  { return false; }
  
  // all checks passed
  return true;
}
