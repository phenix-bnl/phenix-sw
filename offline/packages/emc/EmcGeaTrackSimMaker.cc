//////////////////////////////////////////////////////////////////////////////////
//
// this module fills the emcGeaTrackContainer. you wan't to fill these 
// information as soon as possible, as response code might need info 
// about tracks (ie particle type).
//
//////////////////////////////////////////////////////////////////////////////////

#include <emctypes.h>
#include <dio_trk.hh>
#include <emcNodeHelper.h>
#include <Fun4AllReturnCodes.h>

#include <emcGeaEdep.h>
#include <emcGeaEdepContainer.h>
#include <emcGeaDeposit.h>
#include <emcGeaDepositContainer.h>
#include <emcGeaTrackContent.h>
#include <emcGeaTrackContainer.h>

#include <EmcGeaTrackSimMaker.h>

#include <cassert>
#include <cstdlib>
#include <map>


ClassImp(EmcGeaTrackSimMaker);



int EmcGeaTrackSimMaker::InitRun(PHCompositeNode * root){
  PHCompositeNode * emcnode = emcNodeHelper::findCompositeNode(root, "EMC"); EMCNODEASSERT( emcnode );
  PHCompositeNode * dstnode = emcNodeHelper::findCompositeNode(root, "DST"); EMCNODEASSERT( dstnode );


  // check input
  emcGeaEdepContainer * edeps = getGeaObject(emcGeaEdepContainer, emcGeaEdepContainer, "emcGeaEdepContainer", emcnode);
  EMCNODEASSERT( edeps );

  emcGeaDepositContainer * deposits = getGeaObject(emcGeaDepositContainer, emcGeaDepositContainer, "emcGeaDepositContainer", dstnode);
  EMCNODEASSERT( deposits );


  // check output
  emcGeaTrackContainer * tracks = getGeaObject(emcGeaTrackContainer, emcGeaTrackContainer, "emcGeaTrackContainer", dstnode);
  EMCNODEASSERT( tracks );


  return 0;
}



int EmcGeaTrackSimMaker::Reset(PHCompositeNode * root){

  PHCompositeNode * dstnode = emcNodeHelper::findCompositeNode(root, "DST"); EMCNODEASSERT( dstnode );

  emcGeaTrackContainer * tracks = getGeaObject(emcGeaTrackContainer, emcGeaTrackContainer, "emcGeaTrackContainer", dstnode);
  EMCNODEASSERT( tracks );
  tracks->Reset();

  return 0;
}





int EmcGeaTrackSimMaker::process_event(PHCompositeNode * root){

  PHCompositeNode * emcnode = emcNodeHelper::findCompositeNode(root, "EMC"); EMCNODEASSERT( emcnode );
  PHCompositeNode * dstnode = emcNodeHelper::findCompositeNode(root, "DST"); EMCNODEASSERT( dstnode );

  emcGeaEdepContainer * edeps = getGeaObject(emcGeaEdepContainer, emcGeaEdepContainer, "emcGeaEdepContainer", emcnode);
  EMCNODEASSERT( edeps );
  
  emcGeaDepositContainer * deposits = getGeaObject(emcGeaDepositContainer, emcGeaDepositContainer, "emcGeaDepositContainer", dstnode);
  EMCNODEASSERT( deposits );
  
  emcGeaTrackContainer * tracks = getGeaObject(emcGeaTrackContainer, emcGeaTrackContainer, "emcGeaTrackContainer", dstnode);
  EMCNODEASSERT( tracks );

  

  //
  // iteration 1: go through geant deposit table, and create a track
  // for every deposit
  //
  for(size_t i = 0; i < edeps->size(); i++){
    emcGeaEdep * xdep = edeps->get(i);

    emcGeaTrackContent * track = add_track_recursive(tracks, xdep->trkno);
    if( track == NULL ) return ABORTRUN;
    if( track->get_impx() == 0  &&  track->get_impy() == 0  &&  track->get_impz() == 0 ){
      // this imp[xyz] is wrong if
      // - track A deposits energy in emcal AND
      // - decays into track B1 nad B2.. AND
      // - B? deposits energy in emcal
      // then imp for B? is set to imp of A
      
      track->set_impx( xdep->x );
      track->set_impy( xdep->y );
      track->set_impz( xdep->z );
    }
  }



  //
  // iteration 2: loop over all tracks and fill daughterlists
  //
  for(size_t i = 0; i < tracks->size(); i++){
    emcGeaTrackContent * track = tracks->get(i);
    if(track->get_parent_trkno() == 0) continue; // primary particle, no parent
    emcGeaTrackContent * parenttrack = tracks->find( track->get_parent_trkno() );
    if(parenttrack == NULL) return ABORTRUN; // track table is broken
    parenttrack->append_to_daughter_list( track->get_trkno() );
  }


  //
  // iteration 3: add deposits to tracks
  //
  for(size_t i = 0; i < deposits->size(); i++){
    emcGeaDeposit * deposit = deposits->get(i);
    emc_trkno_t trkno = deposit->get_trkno();
    emc_towerid_t towerid = deposit->get_towerid();

    emcGeaTrackContent * track = tracks->find( trkno );
    if( track == NULL ){
      std::cerr << __PRETTY_FUNCTION__ << ": found no track with trkno = " << trkno << std::endl
		<< __PRETTY_FUNCTION__ << ": dropping deposit" << std::endl;
      continue;
    }

    emcGeaDeposit::datatype_t type = emcGeaDeposit::GEA;
    track->set_edep_bytower(towerid, deposit->get_edep(type), type);
    track->set_tof_bytower(towerid, deposit->get_tof(type), type);

    type = emcGeaDeposit::ORIG;
    track->set_edep_bytower(towerid, deposit->get_edep(type), type);
    track->set_tof_bytower(towerid, deposit->get_tof(type), type);

    type = emcGeaDeposit::CALIB;
    track->set_edep_bytower(towerid, deposit->get_edep(type), type);
    track->set_tof_bytower(towerid, deposit->get_tof(type), type);
  }


  return 0;
}





emcGeaTrackContent * EmcGeaTrackSimMaker::add_track_recursive(emcGeaTrackContainer * tracks, emc_trkno_t trkno){
  emcGeaTrackContent * track = tracks->find( trkno );
  if( track != NULL ) return track;

  track = emcGeaTrackContent::createdef(trkno);
  if( tracks->add(track) == -1 ) return NULL;
  track = tracks->find(trkno);


  //
  // for information on this secret call, see
  //
  // "Documentation for PISA Track Ancestry Accessor Functions"
  // http://www.phenix.bnl.gov/phenix/WWW/simulation/pisaAncestry.html
  //
  int status, true_track, nfile, error, itparent, idparent, idpart;
  float ptot, ptheta, pphi, r_vertex, z_vertex, theta_vertex, phi_vertex;
  
  true_track = trkno;
  nfile = error = itparent = idparent = idpart = 0;
  ptot = ptheta = pphi = r_vertex = z_vertex = theta_vertex = phi_vertex = 0.0;
  
  status = dio_ptrkstack(&true_track, &nfile, &error, 
			 &ptot, &ptheta, &pphi,
			 &r_vertex, &z_vertex, 
			 &theta_vertex, &phi_vertex,
			 &itparent, &idparent, &idpart);
  
  assert(status == 0); // todo: should do better error handling
  
  // input file field
  track->set_input( nfile );

  // pid field
  track->set_pid( idpart );

  // parent trkno field
  if( idparent == 0 ) track->set_parent_trkno( 0 );
  else track->set_parent_trkno( abs(itparent) );

  // ancestry level field
  if( track->get_parent_trkno() != 0 ){
    emcGeaTrackContent * parent = add_track_recursive( tracks, track->get_parent_trkno() );
    if(parent == NULL) return NULL;
    track->set_anclvl( parent->get_anclvl() + 1 );
  } else {
    track->set_anclvl( 0 );
  }


  
  // total momentum (_NOT_ kinetic energy) field
  static const float rmass[26] = 
    { -1.0, // pid indexing starts with 1
      0.0,    0.0005, 0.0005, 0.0,    0.1057,
      0.1057, 0.1349, 0.1395, 0.1395, 0.4977,
      0.4936, 0.4936, 0.9396, 0.9383, 0.9383,
      0.4977, 0.5475, 1.1156, 1.1894, 1.1926,
      1.1975, 1.3149, 1.3213, 1.6724, 0.9396
    };
  
  track->set_ekin( ptot ); // for photons
  if ( (idpart > 0) && (idpart < 26) ){
    float work = sqrt(rmass[idpart] * rmass[idpart] + ptot * ptot) - rmass[idpart];
    if (work > 0.0) track->set_ekin( work );
  }
  
      
  const double TORAD = M_PI / 180.0;
  
  // position where the particle was born
  {
    double sinphi = sin(TORAD * (double)phi_vertex);
    double cosphi = cos(TORAD * (double)phi_vertex);
    double sintheta = sin(TORAD * (double)theta_vertex);
    
    if(z_vertex != 0.0){
      track->set_x( cosphi * (double)r_vertex );
      track->set_y( sinphi * (double)r_vertex );
      track->set_z( z_vertex );
    } else {
      track->set_x( sintheta * cosphi * (double)r_vertex );
      track->set_y( sintheta * sinphi * (double)r_vertex );
      track->set_z( z_vertex );
    }
  }
  
  
  // momemntum the particle was born with
  {  
    double sinphi = sin(TORAD * (double)pphi);
    double cosphi = cos(TORAD * (double)pphi);
    double sintheta = sin(TORAD * (double)ptheta);
    double costheta = cos(TORAD * (double)ptheta);
    
    track->set_px( sintheta * cosphi * (double)ptot );
    track->set_py( sintheta * sinphi * (double)ptot );
    track->set_pz( costheta * (double)ptot );
  }


  return track;
}
