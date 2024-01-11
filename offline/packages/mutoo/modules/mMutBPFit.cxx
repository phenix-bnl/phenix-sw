// $Id: mMutBPFit.cxx,v 1.41 2011/12/24 04:48:28 slash Exp $

/*!
  \file    mMutBPFit.cxx
  \brief   Bend Plane track fit module
  \author  S. Kelly
  \version $Revision: 1.41 $
  \date    $Date: 2011/12/24 04:48:28 $
*/

// MUTOO headers
//
#include "mMutBPFit.h"
#include "mMutBPFitPar.h"

#include <MutGeom.h>
#include <MUTOO.h>
#include <PHException.h>
#include <PHGslMatrix.h>
#include <TMutBPUtil.h>
#include <TMutExtVtx.h>
#include <TMutCoordMap.h>
#include <TMutGeo.h>
#include <TMutNode.h>
#include <TMutStubMap.h>
#include <TMutTrackUtil.h>

#include<TMuiRoadMapO.h>
#include<TMuiClusterMapO.h>
#include<MuiGeomClasses.hh>

// #include <gsl/gsl_matrix.h>
// #include <gsl/gsl_permutation.h>
// #include <gsl/gsl_linalg.h>

#include <iostream>
#include <string>

using namespace std;

//______________________________________________________  
mMutBPFit::mMutBPFit()  :
	_timer( PHTimeServer::get()->insert_new("mMutBPFit") ),
  _ext_vtx_flag( true ),
  _ext_vtx( PHPoint(0, 0, 0 ) )
{
  MUTOO::TRACE("initializing module mMutBPFit",MUTOO::ALOT);
}

//______________________________________________________  
PHBoolean mMutBPFit::event(PHCompositeNode* top_node)
{

  _timer.get()->restart(); 
  
  try { 

    // Reset IOC pointers
    set_interface_ptrs(top_node);
    
    track_loop();

  } catch(exception& e) {

    MUTOO::TRACE(e.what());
    return False;
    
  }  

  // Timer
  _timer.get()->stop();
  if( _mod_par->get_verbosity() >= MUTOO::SOME ) _timer.get()->print();  

  return True;
}

//______________________________________________________  
void mMutBPFit::track_loop()
{
  TMutTrkMap::iterator trk_iter = _trk_map->range();
  while(TMutTrkMap::pointer trk_ptr = trk_iter.next())
  {
		
    switch( trk_ptr->get()->get_associated<TMutStub>().count() )
    {
      case 3:
      fit_track123(trk_ptr);
      break;
			
      case 2:
      if(_mod_par->get_use_vertex23() ) fit_track23_vertex( trk_ptr );
      else fit_track23( trk_ptr );
      break;
			
      default:
      break;
    }
  }
}

//______________________________________________________  
void mMutBPFit::fit_track123(TMutTrkMap::pointer trk_ptr)
{
    
  // If we have a stub in all three stations do 
  // the three station single bend plane fit
  
  // If only two stations just return
  if( chk_trk_fit_mode(trk_ptr, STA_123 ) == false ) return; 
  
  // retrieve arm
  unsigned short arm = trk_ptr->get()->get_arm(); 
  
  // Get the stations 1,2, and 3 stub pointers
  TMutStubMap::key_iterator stub_iter = trk_ptr->get()->get_associated<TMutStub>();  
  TMutStubMap::pointer stub_sta1 = 0;
  TMutStubMap::pointer stub_sta2 = 0;
  TMutStubMap::pointer stub_sta3 = 0;
  while(TMutStubMap::pointer stub_ptr = stub_iter.next()){
    if(stub_ptr->get()->get_station() == MUTOO::Station1) stub_sta1 = stub_ptr;
    else if (stub_ptr->get()->get_station() == MUTOO::Station2) stub_sta2 = stub_ptr;
    else if (stub_ptr->get()->get_station() == MUTOO::Station3) stub_sta3 = stub_ptr;
  }

  // cout << "mMutBPFit::fit_track123 - got stubs" << endl;
  
  // Get the z of the bend plane 
  double z_bp_23 = TMutBPUtil::get_zbp_sta23(arm, stub_sta2->get()->get_theta());
  double z_bp_12 = TMutBPUtil::get_zbp_sta12(arm, stub_sta2->get()->get_theta());
  double pt_kick_23 = TMutBPUtil::get_pt_kick_sta23(arm, stub_sta2->get()->get_theta());
  double pt_kick_12 = TMutBPUtil::get_pt_kick_sta12(arm, stub_sta2->get()->get_theta());
  double kick_ratio = pt_kick_23/pt_kick_12;

  double bp_term=0;
  double bp_term_12( 0 );
  double bp_term_23( 0 );

  // some dumps
  if( _mod_par->get_verbosity() >= MUTOO::ALOT ) 
  cout 
    << "mMutBPFit::fit_track123: z_bp_23 z_bp_12 pt_kick_23 pt_kick_12=" 
    << z_bp_23 << " "
    << z_bp_12 << " "
    << pt_kick_23 << " "
    << pt_kick_12 << endl;

  // Reference z taken from station 1 stub
  double z_ref = stub_sta1->get()->get_fit_par()->get_z();

  // Bend plane phi values
  double phi12 = 0.;
  double phi23 = 0.;
  
  typedef list<fit_data> hit_data_list;
  hit_data_list hit_list;
  
  // Build fit data list for every coordinate
  TMutCoordMap::const_key_iterator coord_iter = trk_ptr->get()->get_associated<TMutCoord>();
  while(TMutCoordMap::const_pointer coord_ptr = coord_iter.next())
  {
    // cout << "mMutBPFit::fit_track123 - coord " << coord_ptr->get()->get_key().get_obj_key() << endl;
    
    double cathode_angle = TMutGeo::get_cathode_angle(coord_ptr->get()->get_arm(),
						      coord_ptr->get()->get_station(),
						      coord_ptr->get()->get_octant(),
						      coord_ptr->get()->get_half_octant(),
						      coord_ptr->get()->get_gap(),
						      coord_ptr->get()->get_cathode(),
						      coord_ptr->get()->get_peak_strip());

    double sin_cathode = sin(cathode_angle);
    double cos_cathode = cos(cathode_angle);
    double z_meas = coord_ptr->get()->get_mean_z();

    // Here we calculate the correction due to the 2 bend planes.  In
    // station 1 there is no correction (just the linear extrapolation
    // from the reference plane at station 1) -- station 2 and 3
    // pick up the appropriately normalized bend plane correction.
    bp_term=0;
    
    if(coord_ptr->get()->get_station() == MUTOO::Station2){

      // Extrapolate station 2 stub to z bend plane between station 1 and 2
      PHPoint bp_point = TMutTrackUtil::linear_track_model(stub_sta2->get()->get_fit_par(),z_bp_12);
      phi12 = atan2(bp_point.getY(),bp_point.getX());
      phi12 = (atan2(stub_sta2->get()->get_fit_par()->get_y(),
                     stub_sta2->get()->get_fit_par()->get_x()) +
               atan2(stub_sta1->get()->get_fit_par()->get_y(),
                     stub_sta1->get()->get_fit_par()->get_x()))/2.0;
      bp_term = (z_meas-z_bp_12)*(sin_cathode*sin(phi12) + cos_cathode*cos(phi12));
      
    } else if(coord_ptr->get()->get_station() == MUTOO::Station3) { 

      PHPoint bp_point_12 = TMutTrackUtil::linear_track_model(stub_sta2->get()->get_fit_par(),z_bp_12);

      phi12 = (atan2(stub_sta2->get()->get_fit_par()->get_y(),
                     stub_sta2->get()->get_fit_par()->get_x()) +
               atan2(stub_sta1->get()->get_fit_par()->get_y(),
                     stub_sta1->get()->get_fit_par()->get_x()))/2.0;
      bp_term_12 = (z_meas-z_bp_12)*(sin_cathode*sin(phi12) + cos_cathode*cos(phi12));
      
      // Extrapolate station 2 stub to z bend plane between station 2 and 3 //1 and 2
      PHPoint bp_point_23 = TMutTrackUtil::linear_track_model(stub_sta2->get()->get_fit_par(),z_bp_23);
      phi23 = (atan2(stub_sta2->get()->get_fit_par()->get_y(),
                     stub_sta2->get()->get_fit_par()->get_x()) +
               atan2(stub_sta3->get()->get_fit_par()->get_y(),
                     stub_sta3->get()->get_fit_par()->get_x()))/2.0;
     
      bp_term_23 = (z_meas-z_bp_23)*(sin_cathode*sin(phi23) + cos_cathode*cos(phi23));

      // Bend plane correction is the sum of the two terms 
      // normalize by the ratio of the kicks
      bp_term = bp_term_12 + bp_term_23*kick_ratio;
    }
    
    // Calculate the bend plane term - only for coords upstream of the bend 
    // plane
    //    
    fit_data fit_goods = {
      {{ 
        -sin_cathode,                   // cf[0]
	      -(z_meas-z_ref)*sin_cathode,    // cf[1]
	      cos_cathode,                    // cf[2]
	      (z_meas-z_ref)*cos_cathode,     // cf[3]
	      bp_term 
      }},                      // cf[4]
      z_meas,
      coord_ptr->get()->get_w_absolute(),
      coord_ptr->get()->get_error(),
    };
    
    hit_list.push_back(fit_goods);
  } 

  // Option to add a vertex point:
  if(_mod_par->get_use_vertex123() ) {

    // cout << "mMutBPFit::fit_track123 - adding vertex" << endl;
    
    double sin_cathode = 0.0;
    double cos_cathode = 1.0;
    PHPoint vtx_point( (_ext_vtx_flag) ? _ext_vtx:PHPoint( 0, 0, 0 ) );
    double z_meas = vtx_point.getZ();
    double bp_term = 0.0;

    // Retrieve previous momentum fit value and set weight of the vertex point based
    // on the momentum of the track (multiple scattering = f(1/p)
    double sigma = 0.0;
       
    if(_mod_par->get_use_p_dep_sigma() ) {

      double px = trk_ptr->get()->get_bp_par()->get_px_st2();
      double py = trk_ptr->get()->get_bp_par()->get_py_st2();
      double pz = trk_ptr->get()->get_bp_par()->get_pz_st2();
      double pfit = sqrt(MUTOO::SQUARE(px) + MUTOO::SQUARE(py) + MUTOO::SQUARE(pz));
      sigma = 2.0 + 9.0/pfit;
      
    }
    else{
      sigma = 5.0;
    }

    fit_data fit_goodsx = {
      {{ 
        -sin_cathode,                   // cf[0]
        -(z_meas-z_ref)*sin_cathode,    // cf[1]
        cos_cathode,                    // cf[2]
  	    (z_meas-z_ref)*cos_cathode,     // cf[3]
  	    bp_term 
      }},                      // cf[4]
      z_meas,
      0.0,  // x vertex point
      sigma   // x vertex error ?? retrieve from somewhere???
    };
    
    hit_list.push_back(fit_goodsx);

    sin_cathode = 1.0;
    cos_cathode = 0.0;
    vtx_point =  (_ext_vtx_flag) ? _ext_vtx:PHPoint( 0, 0, 0 ) ;
    z_meas = vtx_point.getZ();
    bp_term = 0.0;
    fit_data fit_goodsy = 
    {
      {{
        -sin_cathode,                   // cf[0]
        -(z_meas-z_ref)*sin_cathode,    // cf[1]
        cos_cathode,                    // cf[2]
        (z_meas-z_ref)*cos_cathode,     // cf[3]
        bp_term 
      }},                      // cf[4]
      z_meas,
      0.0,  // y vertex point
      sigma   // y vertex error ?? retrieve from somewhere???
    };
   
    hit_list.push_back(fit_goodsy);
    // cout << "mMutBPFit::fit_track123 - adding vertex - done." << endl;

  }  // if use_vertex

  // If using the MuID, retrieve MuID hits:
  if(_mod_par->get_use_muid_123fit() ) 
  {

    // cout << "mMutBPFit::fit_track123 - adding muid" << endl;
    bp_term = bp_term_12 + bp_term_23*kick_ratio;
    TMuiRoadMapO::key_iterator road_iter( trk_ptr->get()->get_associated<TMuiRoadO>() );

    if (road_iter.count() > 0) 
    {
 
      // cout << "mMutBPFit::fit_track123 - road: " << road_iter->get()->get_key().get_obj_key() << endl;
      TMuiClusterMapO::key_iterator clus_iter = road_iter->get()->get_associated<TMuiClusterO>();
      while( TMuiClusterMapO::pointer clus_ptr = clus_iter.next() ) 
      {
        
//         cout 
//           << "mMutBPFit::fit_track123 -"
//           << " cluster " << clus_ptr->get()->get_key().get_obj_key() 
//           << " (" << clus_ptr->get()->get_arm() << "," << clus_ptr->get()->get_plane() << "," << clus_ptr->get()->get_panel() << "," << clus_ptr->get()->get_orientation() << ")"
//           << endl;
        unsigned short orientation( clus_ptr->get()->get_orientation() );
        double cos_cathode =  ( orientation == kHORIZ ) ? 1.0 : 0.0;
        double sin_cathode =  ( orientation == kHORIZ ) ? 0.0 : -1.0;
        double z_meas = 0.5*( clus_ptr->get()->get_coord_begin().getZ() +
          clus_ptr->get()->get_coord_end().getZ() );

        double sigmaN[5] = { 5.0, 6.0, 7.0, 8.0, 9.0 };

        double sigma;
        int i = clus_ptr->get()->get_plane();
        sigma = sigmaN[i];

//        double sigma[5] = { 15.0, 15.0, 15.0, 15.0, 15.0 };
/*        sigma =  ( orientation == kHORIZ ) ?
            clus_ptr->get()->get_centroidsigma().getY():
            clus_ptr->get()->get_centroidsigma().getX(); */


        double xy_meas =  ( orientation == kHORIZ ) ?
            clus_ptr->get()->get_centroidpos().getY():
            clus_ptr->get()->get_centroidpos().getX();

        fit_data fit_goodsm = {
          {{ 
            -sin_cathode,                   // cf[0]
            -(z_meas-z_ref)*sin_cathode,    // cf[1]
            cos_cathode,                    // cf[2]
            (z_meas-z_ref)*cos_cathode,     // cf[3]
            bp_term 
          }},                      // cf[4]
          z_meas,
          xy_meas,  // x vertex point
          sigma     // x vertex error ?? retrieve from somewhere???
        };
    
        hit_list.push_back(fit_goodsm);

      }

    }
    
    // cout << "mMutBPFit::fit_track123 - adding muid - done" << endl;
    
  }
  
  // check hit list
  if( hit_list.empty() ) 
  {
    if( _mod_par->get_verbosity() >= MUTOO::SOME )
    cout << "mMutBPFit::fit_track123 - no hits." << endl;
    return;
  }
  
  // Iterate over the hit_list above and calculate the inverse 
  // parameter matrix. 
  PHGslMatrix m( 5,5 ); 
  m.zero(); 
  
  vector<double> vsi( 5, 0. );
  vector<double> evsi( 5, 0. );
  vector<double> sgsin( hit_list.size(), 0. );

  // Loop over hit_list and fill the m_array
  int l = 0; // hit iterator for sgsin array reference  
  hit_data_list::iterator list_iter = hit_list.begin();
  for(;list_iter!=hit_list.end();++list_iter) { 
    
    sgsin[l] = 1/(list_iter->w_error * list_iter->w_error);
    for (int i=0;i<5;i++) {
      for(int j=0;j<5;j++)
      m(i,j) += list_iter->cf[i] * list_iter->cf[j] * sgsin[l];
      
      vsi[i] += list_iter->cf[i] * list_iter->w_meas * sgsin[l];
      evsi[i] += list_iter->cf[i] * list_iter->cf[i] * sgsin[l]; 
    }
    
    ++l;
  }

  // Quick check
  if( _mod_par->get_verbosity() >= MUTOO::MAX ) {
    MUTOO::PRINT(cout,"Matrix View Values");
    cout << m;
  }
  
  // LU decompose and invert the m_view matrix to the m matrix
  m = m.invert();  
  
  // Calculate the fitted parameters
  vector<double> pf( 5, 0. );
  vector<double> epf( 5, 0. );

  for (int i=0;i<5;i++) 
  for(int j=0;j<5;j++) {
    pf[i] += m(j,i) * vsi[j];
    epf[i] += m(j,i) * m(j,i) * evsi[j];
  }
  if( _mod_par->get_verbosity() >= MUTOO::ALOT ) {
    cout 
      << "mMutBPFit::fit_track123: pf = " << pf[0] << " " << pf[1] << " "
      << pf[2] << " " << pf[3] << " " << pf[4] << endl;
  }

  // cout << "mMutBPFit::fit_track123 - momentum done" << endl;
    
  // Calculate the predicted positions and residuals
  vector<double> wp( hit_list.size(), 0. );
  vector<double> res( hit_list.size(), 0. );
  vector<double> dchisq( hit_list.size(), 0. );
  double chisq = 0.;
  
  int k=0;  // wp and res array iterator
  list_iter = hit_list.begin();
  for(;list_iter!=hit_list.end();++list_iter) {
    for(int i=0;i<5;i++) {
      wp[k] += list_iter->cf[i] * pf[i];
    }
    res[k] = list_iter->w_meas - wp[k];
    dchisq[k] = (res[k] * res[k]) *sgsin[k];
    chisq += dchisq[k];
    ++k;
  }
  
  // Put chi-square contribution into coord object:
  coord_iter = trk_ptr->get()->get_associated<TMutCoord>();
  k = 0;
  while(TMutCoordMap::const_pointer coord_ptr = coord_iter.next()){
     coord_ptr->get()->push_chi_sqr_inc(
       trk_ptr->get()->get_key().get_obj_key(),
       dchisq[k++] );
  }
  // cout << "mMutBPFit::fit_track123 - chisquare done" << endl;

  // Calculating the momentum, positions, and charges of the track 
  //
  double pz = 0.;
  double dpz = 0.;
  double chg = 0.;
  
  // Stations 1,2, and 3
  double dx_p1 = pf[1];
  double eaxp1sq = epf[1]*epf[1];
  double dy_p1 = pf[3];
  double eayp1sq = epf[3]*epf[3];
    
  // there is a cut in the original code here to require that the
  // field is on
  //
  // 21-Nov: Add error calculations as well (MLB)
    
  double axp2 = pf[1] - pf[4]*(float)sin((double)phi12);
  double eaxp2sq = epf[1]*epf[1] + epf[4]*epf[4]*sin(phi12)*sin(phi12);
  double ayp2 = pf[3] + pf[4]*(float)cos((double)phi12);
  double eayp2sq = epf[3]*epf[3] + epf[4]*epf[4]*cos(phi12)*cos(phi12);

  double x_12 = pf[0] + pf[1]*(z_bp_12 - z_ref);
  double exb12sq = epf[0]*epf[0] + epf[1]*epf[1]*(z_bp_12 - z_ref)*(z_bp_12 - z_ref);
  double y_12 = pf[2] + pf[3]*(z_bp_12 - z_ref);
  double eyb12sq = epf[2]*epf[2] + epf[3]*epf[3]*(z_bp_12 - z_ref)*(z_bp_12 - z_ref);
  double rhob = (float)sqrt((double)(x_12*x_12+y_12*y_12));
  double erhobsq = (x_12*x_12*exb12sq + y_12*y_12*eyb12sq) / (rhob*rhob);
  double phi12p1 = (-(float)sin((double)phi12)*dx_p1
    + (float)cos((double)phi12)*dy_p1 )/rhob;
  double ephi12p1sq = ((sin(phi12)*sin(phi12)*eaxp1sq + cos(phi12)*cos(phi12)*
    eayp1sq) / (rhob*rhob)) + (pow((-sin(phi12)*dx_p1 + 
    cos(phi12)*dy_p1),2)*(erhobsq/pow(rhob,4)));
  double phi12p2 = (-(float)sin((double)phi12)*axp2
    + (float)cos((double)phi12)*ayp2 )/rhob;
  double ephi12p2sq = ((sin(phi12)*sin(phi12)*eaxp2sq + cos(phi12)*
    cos(phi12)*eayp2sq) / (rhob*rhob)) + (pow((-sin(phi12)*axp2 + 
    cos(phi12)*ayp2),2)*(erhobsq/pow(rhob,4)));
  double edenomsq = rhob*rhob*(cos(phi12p2)*cos(phi12p2)*ephi12p2sq + 
    cos(phi12p1)*cos(phi12p1)*ephi12p1sq) + erhobsq*(sin(phi12p2)-
    sin(phi12p1))*(sin(phi12p2)-sin(phi12p1));

  //    double phi12 = atan2(y_12,x_12); // i think that this should work
  double dx_p2 = pf[1] - pf[4]*sin(phi12);
  double dy_p2 = pf[3] + pf[4]*cos(phi12);
    
  double rho = sqrt(x_12*x_12 + y_12*y_12);
  double phi12_p1 = (-sin(phi12)*dx_p1 + cos(phi12)*dy_p1)/rho;
  double phi12_p2 = (-sin(phi12)*dx_p2 + cos(phi12)*dy_p2)/rho;
  double denom = (sin(phi12_p2) - sin(phi12_p1))*rho;
    
  if (denom != 0){
    pz = fabs(pt_kick_12/denom);
    dpz = sqrt(edenomsq/(pt_kick_12*pt_kick_12)); 
  } else {
    pz = 10000;
    dpz = 0;
  }
  
  // correct pz sign to match the arm
  if( arm == MUTOO::South ) pz*=-1;

  // find the charge
  if (phi12_p1 > phi12_p2) chg = (arm == MUTOO::South) ? 1:-1;
  else chg =  (arm == MUTOO::South) ? -1:1;

  // check the field configuration
  if( TMutBPUtil::get_mode() == TMutBPUtil::REVERSE ) chg *= -1;
  
  // dump
  if( _mod_par->get_verbosity() >= MUTOO::ALOT )
  cout << "mMutBPFit::fit_track123: chg = " << chg << endl;

  // Update the Tracks
  // Update Station 1
  double px_1 = pf[1] * pz;
  double py_1 = pf[3] * pz;
  
  // Calculating the Momentum and Position of the Vertex for 
  // mass measurements using a multiple scattering bend plane

  // store external/default vertex depending on _ext_vtx_flag
  PHPoint vtx_point = ( (_ext_vtx_flag) ? _ext_vtx:PHPoint( 0, 0, 0 ) );
  double xvtx = vtx_point.getX();
  double yvtx = vtx_point.getY();
  double zvtx = vtx_point.getZ();
  
  boost::array<double,3> vtx_mom;
  vtx_mom = TMutBPUtil::get_mom_vtx(vtx_point, trk_ptr);

  // Update the vertex track parameters in the TMutBPPar object
  TMutBPPar local_bp_par;
  local_bp_par.set_px_vtx(vtx_mom.at(0));
  local_bp_par.set_py_vtx(vtx_mom.at(1));
  local_bp_par.set_pz_vtx(vtx_mom.at(2));
  local_bp_par.set_x_vtx(xvtx);
  local_bp_par.set_y_vtx(yvtx);
  local_bp_par.set_z_vtx(zvtx);
  local_bp_par.set_chi_sq(chisq);


  // Update Station 1
  local_bp_par.set_px_st1(px_1);
  local_bp_par.set_py_st1(py_1);
  local_bp_par.set_pz_st1(pz);
  local_bp_par.set_x_st1(pf[0]);
  local_bp_par.set_y_st1(pf[2]);
  local_bp_par.set_z_st1(z_ref);
  
  // Update Station 2
  double z_ref_2 = stub_sta2->get()->get_fit_par()->get_z();  
  local_bp_par.set_px_st2( (pf[1]-pf[4]*sin(phi12))*pz );
  local_bp_par.set_py_st2( (pf[3]+pf[4]*cos(phi12))*pz);
  local_bp_par.set_pz_st2(pz);
  local_bp_par.set_x_st2(pf[0]+pf[1]*(z_ref_2-z_ref)-pf[4]*sin(phi12)*(z_ref_2-z_bp_12));
  local_bp_par.set_y_st2(pf[2]+pf[3]*(z_ref_2-z_ref)+pf[4]*cos(phi12)*(z_ref_2-z_bp_12));
  local_bp_par.set_z_st2(z_ref_2);
  
  // Update Station 3
  double z_ref_3 = stub_sta3->get()->get_fit_par()->get_z();  
  local_bp_par.set_px_st3( (pf[1]-pf[4]*sin(phi12)-kick_ratio*pf[4]*sin(phi23))*pz );
  local_bp_par.set_py_st3( (pf[3]+pf[4]*cos(phi12)+kick_ratio*pf[4]*cos(phi23))*pz );
  local_bp_par.set_pz_st3(pz);
  local_bp_par.set_x_st3(pf[0]+pf[1]*(z_ref_3-z_ref)-pf[4]*sin(phi12)
			 *(z_ref_3-z_bp_12)-kick_ratio*pf[4]*sin(phi23)*(z_ref_3-z_bp_23));
  local_bp_par.set_y_st3(pf[2]+pf[3]*(z_ref_3-z_ref)+pf[4]*cos(phi12)
			 *(z_ref_3-z_bp_12)+kick_ratio*pf[4]*cos(phi23)*(z_ref_3-z_bp_23));
  local_bp_par.set_z_st3(z_ref_3);
  
  // cout << "mMutBPFit::fit_track123 - local_bp_par done" << endl;

  // Update the TMutTrk object with local TMutBPPar
  //
  trk_ptr->get()->set_bp_par(local_bp_par);
  
  // Update the w chi square so mMutRejectTrack can sort based upon BP chi-square
  //
  trk_ptr->get()->set_w_chi_square(chisq);
  trk_ptr->get()->set_ndf( coord_iter.count() - 5 );
  
  // TMutTrkPar at reference plane (Kalman filter seed)
  //
  if(_mod_par->get_ref_plane() == mMutBPFitPar::DOWNSTREAM) {
    TMutTrkPar local_trk_par;
    local_trk_par.set_x(local_bp_par.get_x_st3());
    local_trk_par.set_y(local_bp_par.get_y_st3());
    local_trk_par.set_z(local_bp_par.get_z_st3());
    local_trk_par.set_px(local_bp_par.get_px_st3());
    local_trk_par.set_py(local_bp_par.get_py_st3());
    local_trk_par.set_pz(local_bp_par.get_pz_st3());
    local_trk_par.set_charge( static_cast<int>(chg) );
    local_trk_par.set_chi_square(chisq);
    trk_ptr->get()->set_trk_par(local_trk_par);
    trk_ptr->get()->set_bp_fit3();

  } else {
    TMutTrkPar local_trk_par;
    local_trk_par.set_x(local_bp_par.get_x_st1());
    local_trk_par.set_y(local_bp_par.get_y_st1());
    local_trk_par.set_z(local_bp_par.get_z_st1());
    local_trk_par.set_px(local_bp_par.get_px_st1());
    local_trk_par.set_py(local_bp_par.get_py_st1());
    local_trk_par.set_pz(local_bp_par.get_pz_st1());
    local_trk_par.set_charge( static_cast<int>(chg) );
    local_trk_par.set_chi_square(chisq);
    trk_ptr->get()->set_trk_par(local_trk_par);
    trk_ptr->get()->set_bp_fit3();
  }

  // Only have errors in station 1 for now
  //
  trk_ptr->get()->get_bp_par()->set_err_px_st1(dpz);
  trk_ptr->get()->get_bp_par()->set_err_py_st1(dpz*pf[1]);
  trk_ptr->get()->get_bp_par()->set_err_pz_st1(dpz*pf[3]);
    
  // Update the vertex track parameters 
  TMutTrkPar local_vtx_par;
  local_vtx_par.set_px(vtx_mom.at(0));
  local_vtx_par.set_py(vtx_mom.at(1));
  local_vtx_par.set_pz(vtx_mom.at(2));    
  trk_ptr->get()->set_trk_par_vtx(local_vtx_par);    

  // Store position at station 3 so that mMutMuiRoad can match roads to muon tracks
  // after bend-plane fit.  Fit params need to be pushed on to trk_par_list

  if(_mod_par->get_use_muid_123fit() ) {

    TMutTrkPar local_trk_par;
    local_trk_par.set_px( local_bp_par.get_px_st3() );
    local_trk_par.set_py( local_bp_par.get_py_st3() );
    local_trk_par.set_pz( local_bp_par.get_pz_st3() );

    local_trk_par.set_x( local_bp_par.get_x_st3() );
    local_trk_par.set_y( local_bp_par.get_y_st3() );
    local_trk_par.set_z( local_bp_par.get_z_st3() );

    trk_ptr->get()->push_trk_par( local_trk_par );

  }

  if( _mod_par->get_verbosity() >= MUTOO::SOME ) {
    MUTOO::PRINT(cout,"mMutBPFit::fit_track123 - after eloss calculation");
    trk_ptr->get()->print();
  }
  
}
  

//______________________________________________________  
void mMutBPFit::fit_track23_vertex(TMutTrkMap::pointer trk_ptr)
{

  // If we have a stub in stations 2 and 3 call 
  // the single bend plane
  // Check for three station stubs and set the track fit mode
  
  if (chk_trk_fit_mode(trk_ptr, STA_23) == false) return;  
  
  unsigned short arm = trk_ptr->get()->get_arm(); 

  // Get the stations 2, and 3 stub pointers
  TMutStubMap::key_iterator stub_iter = trk_ptr->get()->get_associated<TMutStub>();  
  TMutStubMap::pointer stub_sta2 = 0;
  TMutStubMap::pointer stub_sta3 = 0;
  while(TMutStubMap::pointer stub_ptr = stub_iter.next()){
    if(stub_ptr->get()->get_station() == MUTOO::Station2){
      stub_sta2 = stub_ptr;
    } else if (stub_ptr->get()->get_station() == MUTOO::Station3) {
      stub_sta3 = stub_ptr;
    }
  }
  
  // Get the z of the bend plane 
  double z_bp_23 = TMutBPUtil::get_zbp_sta23(arm, stub_sta2->get()->get_theta());
  double z_bp_12 = TMutBPUtil::get_zbp_sta12(arm, stub_sta2->get()->get_theta());
  double pt_kick_23 = TMutBPUtil::get_pt_kick_sta23(arm, stub_sta2->get()->get_theta());
  double pt_kick_12 = TMutBPUtil::get_pt_kick_sta12(arm, stub_sta2->get()->get_theta());
  double kick_ratio = pt_kick_12/pt_kick_23;
  
  // some dumps
  if( _mod_par->get_verbosity() >= MUTOO::ALOT ) 
  cout 
    << "mMutBPFit::fit_track23_vertex: z_bp_23 pt_kick_23 pt_kick_12=" 
    << z_bp_23 << " "
    << pt_kick_23 << " "
    << pt_kick_12 << endl;
    
  // Reference z taken from station 1 stub
  double z_ref = stub_sta2->get()->get_fit_par()->get_z();

  // Bend plane phi values
  double phi23 = 0.;
  
  typedef list<fit_data> hit_data_list;
  hit_data_list hit_list;
  
  // Build fit data list for every coordinate
  TMutCoordMap::const_key_iterator coord_iter = trk_ptr->get()->get_associated<TMutCoord>();
  while(TMutCoordMap::const_pointer coord_ptr = coord_iter.next()){

    // stations 2 and 3 only
    if(coord_ptr->get()->get_station() == MUTOO::Station1) continue;

    double cathode_angle = TMutGeo::get_cathode_angle(coord_ptr->get()->get_arm(),
						      coord_ptr->get()->get_station(),
						      coord_ptr->get()->get_octant(),
						      coord_ptr->get()->get_half_octant(),
						      coord_ptr->get()->get_gap(),
						      coord_ptr->get()->get_cathode(),
						      coord_ptr->get()->get_peak_strip());

    double sin_cathode = sin(cathode_angle);
    double cos_cathode = cos(cathode_angle);
    double z_meas = coord_ptr->get()->get_mean_z();

    // Here we calculate the correction due to the 2 bend planes.  In
    // station 1 there is no correction (just the linear extrapolation
    // from the reference plane at station 1) -- station 2 and 3
    // pick up the appropriately normalized bend plane correction.
    // 
    double bp_term=0;
    if(coord_ptr->get()->get_station() == MUTOO::Station3){

      // Extrapolate station 2 stub to z bend plane between station 2 and 3
      PHPoint bp_point_23 = TMutTrackUtil::linear_track_model(stub_sta2->get()->get_fit_par(),z_bp_23);
      phi23 = (atan2(stub_sta2->get()->get_fit_par()->get_y(),
                     stub_sta2->get()->get_fit_par()->get_x()) +
               atan2(stub_sta3->get()->get_fit_par()->get_y(),
                     stub_sta3->get()->get_fit_par()->get_x()))/2.0;
      double bp_term_23 = (z_meas-z_bp_23)*(sin_cathode*sin(phi23) + cos_cathode*cos(phi23));

      // Bend plane correction is the sum of the two terms 
      // normalize by the ratio of the kicks
      bp_term = bp_term_23;
    }
    
    // Calculate the bend plane term - only for coords upstream of the bend plane
    fit_data fit_goods = {
      {{ 
        -sin_cathode,                   // cf[0]
        -(z_meas-z_ref)*sin_cathode,    // cf[1]
        cos_cathode,                    // cf[2]
        (z_meas-z_ref)*cos_cathode,     // cf[3]
        bp_term 
      }},                                // cf[4]
      z_meas,
      coord_ptr->get()->get_w_absolute(),
      coord_ptr->get()->get_error(),
    };
    
    hit_list.push_back(fit_goods);
  } 

  // Option to add a vertex point:
  if(_mod_par->get_use_vertex23() ) {

    double bp_term = 0.0;
    double sin_cathode = 0.0;
    double cos_cathode = 1.0;
    PHPoint vtx_point( (_ext_vtx_flag) ? _ext_vtx:PHPoint( 0, 0, 0 ) );
    double z_meas = vtx_point.getZ();
    double phi12 = atan2(stub_sta2->get()->get_fit_par()->get_y(),
                    stub_sta2->get()->get_fit_par()->get_x()) -
               ( atan2(stub_sta3->get()->get_fit_par()->get_y(),
                       stub_sta3->get()->get_fit_par()->get_x()) - 
                 atan2(stub_sta2->get()->get_fit_par()->get_y(),
                       stub_sta2->get()->get_fit_par()->get_x()))/ 2.0; 


    bp_term = kick_ratio*(z_meas-z_bp_12)*(sin_cathode*sin(phi12) + cos_cathode*cos(phi12));

    fit_data fit_goodsx = {
      {{ 
        -sin_cathode,                   // cf[0]
        -(z_meas-z_ref)*sin_cathode,    // cf[1]
        cos_cathode,                    // cf[2]
        (z_meas-z_ref)*cos_cathode,     // cf[3]
        bp_term 
      }},                      // cf[4]
      z_meas,
      0.0,  // x vertex point
      5.0   // x vertex error ?? retrieve from somewhere???
    };
    
    hit_list.push_back(fit_goodsx);

    sin_cathode = 1.0;
    cos_cathode = 0.0;
    vtx_point =  (_ext_vtx_flag) ? _ext_vtx:PHPoint( 0, 0, 0 ) ;
    z_meas = vtx_point.getZ();
    bp_term = kick_ratio*(z_meas-z_bp_12)*(sin_cathode*sin(phi12) + cos_cathode*cos(phi12));
    fit_data fit_goodsy = {
      {{ 
        -sin_cathode,                   // cf[0]
        -(z_meas-z_ref)*sin_cathode,    // cf[1]
        cos_cathode,                    // cf[2]
        (z_meas-z_ref)*cos_cathode,     // cf[3]
        bp_term 
      }},                      // cf[4]
      z_meas,
      0.0,  // y vertex point
      5.0   // y vertex error ?? retrieve from somewhere???
    };
   
    hit_list.push_back(fit_goodsy);

  }  // if use_vertex23

  if( hit_list.empty() ) 
  {
    if( _mod_par->get_verbosity() >= MUTOO::SOME )
    cout << "mMutBPFit::fit_track23_vertex - no hits." << endl;
    return;
  }

  // Iterate over the hit_list above and calculate the inverse 
  // parameter matrix. 
  PHGslMatrix m(5,5); 
  m.zero();
  
  vector<double> vsi( 5, 0. );
  vector<double> evsi( 5, 0. );
  vector<double> sgsin( hit_list.size(), 0. );

  // Loop over hit_list and fill the m_array
  int l = 0; // hit iterator for sgsin array reference  
  hit_data_list::iterator list_iter = hit_list.begin();
  for(;list_iter!=hit_list.end();++list_iter) { 
    
    sgsin[l] = 1/(list_iter->w_error * list_iter->w_error);
    
    for (int i=0;i<5;i++) {
      for(int j=0;j<5;j++)
	    m(i,j) += list_iter->cf[i] * list_iter->cf[j] * sgsin[l];

      vsi[i] += list_iter->cf[i] * list_iter->w_meas * sgsin[l];
      evsi[i] += list_iter->cf[i] * list_iter->cf[i] * sgsin[l]; 
    }
    
    ++l;
  }

  // Quick check
  if( _mod_par->get_verbosity() >= MUTOO::MAX ) {
    MUTOO::PRINT(cout,"Matrix View Values");
    cout << m;
  }
    
  // invert m
  m = m.invert();
  
  // Calculate the fitted parameters
  vector<double> pf( 5, 0. );
  vector<double> epf( 5, 0. );

  for (int i=0;i<5;i++)
  for(int j=0;j<5;j++) {
    pf[i] += m(j,i) * vsi[j];
    epf[i] += m(j,i) * m(j,i) * evsi[j];
  }
  
  if( _mod_par->get_verbosity() >= MUTOO::ALOT ) {
    cout 
      << "mMutBPFit::fit_track23_vertex: pf = " << pf[0] << " " << pf[1] << " "
      << pf[2] << " " << pf[3] << " " << pf[4] << endl;
  }
  
  // Calculate the predicted positions and residuals
  vector<double> wp( hit_list.size(), 0. );
  vector<double> res( hit_list.size(), 0. );
  vector<double> dchisq( hit_list.size(), 0. );
  double chisq = 0.;
  
  int k=0;  // wp and res array iterator
  list_iter = hit_list.begin();
  for(;list_iter!=hit_list.end();++list_iter) {
    for(int i=0;i<5;i++) {
      wp[k] += list_iter->cf[i] * pf[i];
    }
    res[k] = list_iter->w_meas - wp[k];
    dchisq[k] = (res[k] * res[k]) *sgsin[k];
    chisq += dchisq[k];

    ++k;
  }

  // Calculating the momentum, positions, and charges of the track 
  double pz = 0.;
  double dpz = 0.;
  double pzalt = 0.;
  double chg = 0.;

  // Stations 2 & 3 only now
  double dx_p2 = pf[1];
  double dy_p2 = pf[3];
  
  //double eaxp2sq = epf[1]*epf[1];
  //double eayp2sq = epf[3]*epf[3];
    
  // there is a cut in the original code here to require that the
  // field is on
  //
  // Add error calculations on fit params from old framework:
  // approximate error formulae for axp3 and ayp3 includes constributions from
  // stub fit errors in x and y as well as MS contribution from magnet + nosecone
  // using station 2,3 fit errors would give unrealistic (large) momentum errors
  //
    
  double x_23 = pf[0] + pf[1]*(z_bp_23 - z_ref);
  double y_23 = pf[2] + pf[3]*(z_bp_23 - z_ref);

  double z3 = stub_sta3->get()->get_fit_par()->get_z();
  double dx_p3 = pf[1] - pf[4]*sin(phi23);
  double eaxp3sq = 4. * (stub_sta2->get()->get_fit_par()->get_covar(0,0)*pow(1/
    (z3 - z_ref),2) + stub_sta3->get()->get_fit_par()->get_covar(0,0)*pow(1/
    (z3 - z_ref),2)) + 0.085*0.085;
  double dy_p3 = pf[3] + pf[4]*cos(phi23);
  double eayp3sq = 4. * (stub_sta2->get()->get_fit_par()->get_covar(2,2)*pow(1/
    (z3 - z_ref),2) + stub_sta3->get()->get_fit_par()->get_covar(2,2)*pow(1/
    (z3 - z_ref),2)) + 0.085*0.085;
  double phi2 = stub_sta2->get()->get_phi();
  double phi3 = stub_sta3->get()->get_phi();
  
  // method using the bend angle
  double rho = sqrt(x_23*x_23 + y_23*y_23);
  double phi23_p1 = (-sin(phi23)*dx_p2 + cos(phi23)*dy_p2)/rho;
  double phi23_p2 = (-sin(phi23)*dx_p3 + cos(phi23)*dy_p3)/rho;
  double denom = (sin(phi23_p2) - sin(phi23_p1))*rho;

  if (denom != 0){
    pzalt = fabs(pt_kick_23/denom);
    dpz = (pzalt*pzalt) * (sin(phi23)*sin(phi23)*eaxp3sq + cos(phi23)*cos(phi23)*eayp3sq);
    dpz = sqrt(dpz);
  }

  // correct pz sign to match the arm
  if( arm == MUTOO::South ) pzalt*=-1;

  // method using the phi difference between stations 3 and 2
  pz = fabs( -pt_kick_23/((phi3-phi2)*rho*1.2) *((z3-z_ref)/kick_ratio+(z3-z_bp_23)) );
  
  // correct pz sign to match the arm
  if( arm == MUTOO::South ) pz*=-1;

  if( _mod_par->get_verbosity() >= MUTOO::ALOT ){
    cout << "mMutBPFit::fit_track23_vertex: phi23_p1,p2 = " << phi23_p1 << " " << phi23_p2 << endl;  
    cout << "mMutBPFit::fit_track23_vertex: pz,pzalt = " << pz << " " << pzalt << endl;
  }
 
  // find the charge 
  if (phi23_p2 < 0) chg = ( arm == MUTOO::South ) ? 1:-1;
  else  chg = ( arm == MUTOO::South ) ? -1:1;

  // check the field configuration
  if( TMutBPUtil::get_mode() == TMutBPUtil::REVERSE ) chg *= -1;

  if( _mod_par->get_verbosity() >= MUTOO::ALOT )
  cout << "mMutBPFit::fit_track23_vertex: chg = " << chg << endl;
    
  // Update the Tracks
  // Update Station 2
  //double pold = sqrt(px*px + py*py + pz*pz);
  double px_2 = pf[1] * pz;
  double py_2 = pf[3] * pz;

  TMutTrkPar local_trk_par;

  local_trk_par.set_x(pf[0]);
  local_trk_par.set_y(pf[2]);
  local_trk_par.set_z(z_ref);
  local_trk_par.set_px(px_2);
  local_trk_par.set_py(py_2);
  local_trk_par.set_pz(pz);
  local_trk_par.set_charge(static_cast<int>(chg));
  TMutBPPar local_bp_par;
  local_bp_par.set_px_st2( px_2 );
  local_bp_par.set_py_st2( py_2 );
  local_bp_par.set_pz_st2(pz);
  trk_ptr->get()->set_bp_par(local_bp_par);
  trk_ptr->get()->get_bp_par()->set_err_pz_st2(dpz);


  // Update so mMutRejectTracks can sort based on BP chi square
  trk_ptr->get()->set_w_chi_square(chisq);
  trk_ptr->get()->set_ndf( coord_iter.count() - 5 );

  trk_ptr->get()->set_trk_par(local_trk_par);
  trk_ptr->get()->set_bp_fit2();

  // Store position at station 3 so that mMutMuiRoad can match roads to muon tracks
  // after bend-plane fit.  Fit params need to be pushed on to trk_par_list

  if(_mod_par->get_use_muid_23fit() ) {
    local_trk_par.set_px( (pf[1]-pf[4]*sin(phi23))*pz );
    local_trk_par.set_py( (pf[3]+pf[4]*cos(phi23))*pz);
    local_trk_par.set_pz(pz);

    local_trk_par.set_x(pf[0]+pf[1]*(z3-z_ref)-pf[4]*sin(phi23)*(z3-z_bp_23));
    local_trk_par.set_y(pf[2]+pf[3]*(z3-z_ref)+pf[4]*cos(phi23)*(z3-z_bp_23));
    local_trk_par.set_z(z3);

    trk_ptr->get()->push_trk_par( local_trk_par );
  }

  if( _mod_par->get_verbosity() >= MUTOO::SOME ) {
    MUTOO::PRINT(cout,"mMutBPFit::fit_track23_vertex - after eloss calculation");
    trk_ptr->get()->print();
  }

}  // end fit_track23_vertex

//______________________________________________________  
void mMutBPFit::fit_track23(TMutTrkMap::pointer trk_ptr)
{
    
  // If we have a stub in stations 2 and 3 call 
  // the single bend plane
  // Check for three station stubs and set the track fit mode
  
  if (chk_trk_fit_mode(trk_ptr, STA_23) == false) return;  
  
  unsigned short arm = trk_ptr->get()->get_arm(); 

  // Get the stations 2, and 3 stub pointers
  TMutStubMap::key_iterator stub_iter = trk_ptr->get()->get_associated<TMutStub>();  
  TMutStubMap::pointer stub_sta2 = 0;
  TMutStubMap::pointer stub_sta3 = 0;
  while(TMutStubMap::pointer stub_ptr = stub_iter.next()){
    if(stub_ptr->get()->get_station() == MUTOO::Station2){
      stub_sta2 = stub_ptr;
    } else if (stub_ptr->get()->get_station() == MUTOO::Station3) {
      stub_sta3 = stub_ptr;
    }
  }
  
  // Get the z of the bend plane 
  double z_bp_23 = TMutBPUtil::get_zbp_sta23(arm, stub_sta2->get()->get_theta());
  double pt_kick_23 = TMutBPUtil::get_pt_kick_sta23(arm, stub_sta2->get()->get_theta());
  double pt_kick_12 = TMutBPUtil::get_pt_kick_sta12(arm, stub_sta2->get()->get_theta());
  double kick_ratio = pt_kick_23/pt_kick_12;
  
  // some dumps
  if( _mod_par->get_verbosity() >= MUTOO::ALOT ) 
  cout 
    << "mMutBPFit::fit_track23: z_bp_23 pt_kick_23 pt_kick_12=" 
    << z_bp_23 << " "
    << pt_kick_23 << " "
    << pt_kick_12 << endl;
    
  // Reference z taken from station 1 stub
  double z_ref = stub_sta2->get()->get_fit_par()->get_z();

  // Bend plane phi values
  double phi23 = 0.;
  
  typedef list<fit_data> hit_data_list;
  hit_data_list hit_list;
  
  // Build fit data list for every coordinate
  TMutCoordMap::const_key_iterator coord_iter = trk_ptr->get()->get_associated<TMutCoord>();
  while(TMutCoordMap::const_pointer coord_ptr = coord_iter.next()){

    // stations 2 and 3 only
    if(coord_ptr->get()->get_station() == MUTOO::Station1) continue;

    double cathode_angle = TMutGeo::get_cathode_angle(coord_ptr->get()->get_arm(),
						      coord_ptr->get()->get_station(),
						      coord_ptr->get()->get_octant(),
						      coord_ptr->get()->get_half_octant(),
						      coord_ptr->get()->get_gap(),
						      coord_ptr->get()->get_cathode(),
						      coord_ptr->get()->get_peak_strip());

    double sin_cathode = sin(cathode_angle);
    double cos_cathode = cos(cathode_angle);
    double z_meas = coord_ptr->get()->get_mean_z();

    // Here we calculate the correction due to the 2 bend planes.  In
    // station 1 there is no correction (just the linear extrapolation
    // from the reference plane at station 1) -- station 2 and 3
    // pick up the appropriately normalized bend plane correction.
    // 
    double bp_term=0;
    if(coord_ptr->get()->get_station() == MUTOO::Station3){

      // Extrapolate station 2 stub to z bend plane between station 2 and 3
      PHPoint bp_point_23 = TMutTrackUtil::linear_track_model(stub_sta2->get()->get_fit_par(),z_bp_23);
      phi23 = (atan2(stub_sta2->get()->get_fit_par()->get_y(),
                     stub_sta2->get()->get_fit_par()->get_x()) +
               atan2(stub_sta3->get()->get_fit_par()->get_y(),
                     stub_sta3->get()->get_fit_par()->get_x()))/2.0;
      double bp_term_23 = (z_meas-z_bp_23)*(sin_cathode*sin(phi23) + cos_cathode*cos(phi23));

      // Bend plane correction is the sum of the two terms 
      // normalize by the ratio of the kicks
      bp_term = bp_term_23;
    }
    
    // Calculate the bend plane term - only for coords upstream of the bend plane
    fit_data fit_goods = {
      {{ 
        -sin_cathode,                   // cf[0]
	      -(z_meas-z_ref)*sin_cathode,    // cf[1]
	      cos_cathode,                    // cf[2]
	      (z_meas-z_ref)*cos_cathode,     // cf[3]
	      bp_term 
      }},                                // cf[4]
      z_meas,
      coord_ptr->get()->get_w_absolute(),
      coord_ptr->get()->get_error(),
    };
    
    hit_list.push_back(fit_goods);
  } 

  if( hit_list.empty() ) 
  {
    if( _mod_par->get_verbosity() >= MUTOO::SOME )
    cout << "mMutBPFit::fit_track23_vertex - no hits." << endl;
    return;
  }
  
  // Iterate over the hit_list above and calculate the inverse 
  // parameter matrix. 
  PHGslMatrix m(5,5); 
  m.zero();
  
  vector<double> vsi( 5, 0. );
  vector<double> evsi( 5, 0. );
  vector<double> sgsin( hit_list.size(), 0. );
  // double sgsin[30] = {0.};  // error array 1/sigma^2

  // Loop over hit_list and fill the m_array
  int l = 0; // hit iterator for sgsin array reference  
  hit_data_list::iterator list_iter = hit_list.begin();
  for(;list_iter!=hit_list.end();++list_iter) { 
    
    sgsin[l] = 1/(list_iter->w_error * list_iter->w_error);
    
    for (int i=0;i<5;i++) {
      for(int j=0;j<5;j++)
	    m(i,j) += list_iter->cf[i] * list_iter->cf[j] * sgsin[l];

      vsi[i] += list_iter->cf[i] * list_iter->w_meas * sgsin[l];
      evsi[i] += list_iter->cf[i] * list_iter->cf[i] * sgsin[l]; 
    }
    
    ++l;
  }

  // Quick check
  if( _mod_par->get_verbosity() >= MUTOO::MAX ) {
    MUTOO::PRINT(cout,"Matrix View Values");
    cout << m;
  }
    
  // invert m
  m = m.invert();
  
  // Calculate the fitted parameters
  vector<double> pf( 5, 0. );
  vector<double> epf( 5, 0. );

  for (int i=0;i<5;i++)
  for(int j=0;j<5;j++) {
    pf[i] += m(j,i) * vsi[j];
    epf[i] += m(j,i) * m(j,i) * evsi[j];
  }
  
  if( _mod_par->get_verbosity() >= MUTOO::ALOT ) {
    cout 
      << "mMutBPFit::fit_track23: pf = " << pf[0] << " " << pf[1] << " "
      << pf[2] << " " << pf[3] << " " << pf[4] << endl;
  }
  
  // Calculate the predicted positions and residuals
  vector<double> wp( hit_list.size(), 0. );
  vector<double> res( hit_list.size(), 0. );
  vector<double> dchisq( hit_list.size(), 0. );
  double chisq = 0.;
  
  int k=0;  // wp and res array iterator
  list_iter = hit_list.begin();
  for(;list_iter!=hit_list.end();++list_iter) {
    for(int i=0;i<5;i++) {
      wp[k] += list_iter->cf[i] * pf[i];
    }
    res[k] = list_iter->w_meas - wp[k];
    dchisq[k] = (res[k] * res[k]) *sgsin[k];
    chisq += dchisq[k];

    ++k;
  }
  
  // Calculating the momentum, positions, and charges of the track 
  double pz = 0.;
  double dpz = 0.;
  double pzalt = 0.;
  double chg = 0.;

  // Stations 2 & 3 only now
  double dx_p2 = pf[1];
  double dy_p2 = pf[3];
  
  //double eaxp2sq = epf[1]*epf[1];
  //double eayp2sq = epf[3]*epf[3];
    
  // there is a cut in the original code here to require that the
  // field is on
  //
  // Add error calculations on fit params from old framework:
  // approximate error formulae for axp3 and ayp3 includes constributions from
  // stub fit errors in x and y as well as MS contribution from magnet + nosecone
  // using station 2,3 fit errors would give unrealistic (large) momentum errors
  //
    
  double x_23 = pf[0] + pf[1]*(z_bp_23 - z_ref);
  double y_23 = pf[2] + pf[3]*(z_bp_23 - z_ref);

  double z3 = stub_sta3->get()->get_fit_par()->get_z();
  double dx_p3 = pf[1] - pf[4]*sin(phi23);
  double eaxp3sq = 4. * (stub_sta2->get()->get_fit_par()->get_covar(0,0)*pow(1/
    (z3 - z_ref),2) + stub_sta3->get()->get_fit_par()->get_covar(0,0)*pow(1/
    (z3 - z_ref),2)) + 0.085*0.085;
  double dy_p3 = pf[3] + pf[4]*cos(phi23);
  double eayp3sq = 4. * (stub_sta2->get()->get_fit_par()->get_covar(2,2)*pow(1/
    (z3 - z_ref),2) + stub_sta3->get()->get_fit_par()->get_covar(2,2)*pow(1/
    (z3 - z_ref),2)) + 0.085*0.085;
  double phi2 = stub_sta2->get()->get_phi();
  double phi3 = stub_sta3->get()->get_phi();
  
  // method using the bend angle
  double rho = sqrt(x_23*x_23 + y_23*y_23);
  double phi23_p1 = (-sin(phi23)*dx_p2 + cos(phi23)*dy_p2)/rho;
  double phi23_p2 = (-sin(phi23)*dx_p3 + cos(phi23)*dy_p3)/rho;
  double denom = (sin(phi23_p2) - sin(phi23_p1))*rho;

  if (denom != 0){
    pzalt = fabs(pt_kick_23/denom);
    dpz = (pzalt*pzalt) * (sin(phi23)*sin(phi23)*eaxp3sq + cos(phi23)*cos(phi23)*eayp3sq);
    dpz = sqrt(dpz);
  }

  // correct pz sign to match the arm
  if( arm == MUTOO::South ) pzalt*=-1;

  // method using the phi difference between stations 3 and 2
  pz = fabs( -pt_kick_23/((phi3-phi2)*rho*1.2) *((z3-z_ref)/kick_ratio+(z3-z_bp_23)) );
  
  // correct pz sign to match the arm
  if( arm == MUTOO::South ) pz*=-1;

  if( _mod_par->get_verbosity() >= MUTOO::ALOT ){
    cout << "mMutBPFit::fit_track23: phi23_p1,p2 = " << phi23_p1 << " " << phi23_p2 << endl;  
    cout << "mMutBPFit::fit_track23: pz,pzalt = " << pz << " " << pzalt << endl;
  }
 
  // find the charge 
  if (phi23_p2 < 0) chg = ( arm == MUTOO::South ) ? 1:-1;
  else  chg = ( arm == MUTOO::South ) ? -1:1;

  // check the field configuration
  if( TMutBPUtil::get_mode() == TMutBPUtil::REVERSE ) chg *= -1;

  if( _mod_par->get_verbosity() >= MUTOO::ALOT )
  cout << "mMutBPFit::fit_track23: chg = " << chg << endl;
    
  // Update the Tracks
  // Update Station 2
  //double pold = sqrt(px*px + py*py + pz*pz);
  double px_2 = pf[1] * pz;
  double py_2 = pf[3] * pz;

  TMutTrkPar local_trk_par;

  local_trk_par.set_x(pf[0]);
  local_trk_par.set_y(pf[2]);
  local_trk_par.set_z(z_ref);
  local_trk_par.set_px(px_2);
  local_trk_par.set_py(py_2);
  local_trk_par.set_pz(pz);
  local_trk_par.set_charge(static_cast<int>(chg));
  TMutBPPar local_bp_par;
  local_bp_par.set_px_st2( px_2 );
  local_bp_par.set_py_st2( py_2 );
  local_bp_par.set_pz_st2(pz);
  trk_ptr->get()->set_bp_par(local_bp_par);
  trk_ptr->get()->get_bp_par()->set_err_pz_st2(dpz);

  // Update so mMutRejectTracks can sort based on BP chi square
  trk_ptr->get()->set_w_chi_square(chisq);
  trk_ptr->get()->set_ndf( coord_iter.count() - 5 );

  trk_ptr->get()->set_trk_par(local_trk_par);
  trk_ptr->get()->set_bp_fit2();

  if( _mod_par->get_verbosity() >= MUTOO::SOME ) {
    MUTOO::PRINT(cout,"mMutBPFit::fit_track23 - after eloss calculation");
    trk_ptr->get()->print();
  }

}  // end fit_track23

//______________________________________________________  
bool mMutBPFit::chk_trk_fit_mode(TMutTrkMap::pointer trk_ptr, const trk_fit_mode& mode)
{
  
  if (
    mode == STA_123 &&
    trk_ptr->get()->has_stub(MUTOO::Station1) && 
    trk_ptr->get()->has_stub(MUTOO::Station2) &&
    trk_ptr->get()->has_stub(MUTOO::Station3) )
  {
    return true;
  } else if (
    mode == STA_23 &&
    trk_ptr->get()->has_stub(MUTOO::Station2) &&
    trk_ptr->get()->has_stub(MUTOO::Station3) ) 
  {
    return true;
 	} else { return false; }

}

//______________________________________________________  
/*! Reset IOC and external interface pointers */
void mMutBPFit::set_interface_ptrs(PHCompositeNode* top_node)
{  

  // module runtime parameters
  _mod_par = TMutNode<mMutBPFitPar>::find_node(top_node,"mMutBPFitPar");
  
  _trk_map = TMutNode<TMutTrkMap>::find_node(top_node,"TMutTrkMap");
  
  // initialize external vertex
  _ext_vtx = PHPoint( 0, 0, 0 );
  
  // try load external vertex otherwise
  bool error( false );
  PHPoint vtx( TMutExtVtx::get().get_vtx( error ) );
  if( !error ) _ext_vtx = vtx;
  else if( _mod_par->get_verbosity() >= MUTOO::SOME )
  {  cerr << "mMutBPFit::set_interface_ptrs - wrong vertex.\n"; }
  
}
