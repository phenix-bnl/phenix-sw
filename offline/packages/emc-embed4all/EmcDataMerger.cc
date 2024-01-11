/*
 *
 * for description see header
 *
 */


#include <Fun4AllReturnCodes.h>
#include <Fun4AllServer.h>
#include <emcNodeHelper.h>

#include <emcGeaDeposit.h>
#include <emcGeaTrackContent.h>
#include <emcGeaTrackContainer.h>
#include <emcGeaTowerContent.h>
#include <emcGeaTowerContainer.h>
#include <emcGeaClusterContent.h>
#include <emcGeaClusterContainer.h>


#include <EmcDataMerger.h>


using namespace std;




ClassImp(EmcDataMerger);





EmcDataMerger::EmcDataMerger(): SubsysReco("EmcDataMerger"){
  seentracks = seentowers = false;
}





EmcDataMerger::~EmcDataMerger(){
}





int EmcDataMerger::InitRun(PHCompositeNode * root){
  // check input
  for(list<string>::iterator i = srcnodes.begin(); i != srcnodes.end(); i++){
    PHCompositeNode * src = Fun4AllServer::instance()->topNode( i->c_str() );

    if( src == NULL ){
      std::cerr << __PRETTY_FUNCTION__ << ": source topnode " << *i << " not found!" << std::endl;
      return ABORTRUN;
    }

    PHCompositeNode* srcdstnode = emcNodeHelper::findCompositeNode(src, "DST"); assert( srcdstnode != NULL );

  }



  // create output
  {
    PHCompositeNode* dstdstnode = emcNodeHelper::findCompositeNode(root, "DST");
    if( dstdstnode == NULL ){
      dstdstnode = new PHCompositeNode("DST");
      root->addNode(dstdstnode);
    }
    
    
    emcGeaTrackContainer * tracks = emcGeaTrackContainer::createdef();
    emcNodeHelper::insertObject< emcGeaTrackContainer >(dstdstnode, tracks, "emcGeaTrackContainer");
    
    emcGeaTowerContainer * towers = emcGeaTowerContainer::createdef();
    emcNodeHelper::insertObject< emcTowerContainer >(dstdstnode, towers, "emcTowerContainer");
    
    emcGeaClusterContainer * clusters = emcGeaClusterContainer::createdef();
    emcNodeHelper::insertObject< emcClusterContainer >(dstdstnode, clusters, "emcClusterContainer");
    
    tracks->SetTowers( towers ); tracks->SetClusters( clusters );
    towers->SetTracks( tracks ); towers->SetClusters( clusters );
    clusters->SetTracks( tracks ); clusters->SetTowers( towers );
    
  }


  return EVENT_OK;
}





int EmcDataMerger::ResetEvent(PHCompositeNode * root){
  PHCompositeNode* dstdstnode = emcNodeHelper::findCompositeNode(root, "DST"); 
  assert( dstdstnode != NULL );

  emcGeaTrackContainer * tracks = getGeaObject(emcGeaTrackContainer, emcGeaTrackContainer, "emcGeaTrackContainer", dstdstnode);
  assert( tracks != NULL );
  tracks->Reset();

  emcGeaTowerContainer * towers = getGeaObject(emcGeaTowerContainer, emcTowerContainer, "emcTowerContainer", dstdstnode);
  assert( towers != NULL );
  towers->Reset();

  emcGeaClusterContainer * clusters = getGeaObject(emcGeaClusterContainer, emcClusterContainer, "emcClusterContainer", dstdstnode);
  assert( clusters != NULL );
  clusters->Reset();


  trknooffs.clear();


  return EVENT_OK;
}






int EmcDataMerger::process_event(PHCompositeNode * root){
  PHCompositeNode* dstdstnode = emcNodeHelper::findCompositeNode(root, "DST"); 
  assert( dstdstnode != NULL );

  emcGeaTrackContainer * dsttracks = getGeaObject(emcGeaTrackContainer, emcGeaTrackContainer, "emcGeaTrackContainer", dstdstnode);
  assert( dsttracks != NULL );

  emcGeaTowerContainer * dsttowers = getGeaObject(emcGeaTowerContainer, emcTowerContainer, "emcTowerContainer", dstdstnode);
  assert( dsttowers != NULL );


  emc_trkno_t trackoffs = 0;

  for(list<string>::iterator i = srcnodes.begin(); i != srcnodes.end(); i++){

    for(size_t j = 0; j < dsttracks->size(); j++)
      if( dsttracks->get( j )->get_trkno() > trackoffs ) trackoffs = dsttracks->get( j )->get_trkno();
    trknooffs[i->c_str()] = trackoffs;



    PHCompositeNode * src = Fun4AllServer::instance()->topNode( i->c_str() );
    assert( src != NULL );

    PHCompositeNode* srcdstnode = emcNodeHelper::findCompositeNode(src, "DST"); assert( srcdstnode != NULL );

    emcGeaTrackContainer * srctracks = getGeaObject(emcGeaTrackContainer, emcGeaTrackContainer, "emcGeaTrackContainer", srcdstnode);
    assert( srctracks != NULL ); // todo: better error handling
    merge(*i, srctracks, dsttracks);

    emcGeaTowerContainer * srctowers = getGeaObject(emcGeaTowerContainer, emcTowerContainer, "emcTowerContainer", srcdstnode);
    assert( srctowers != NULL ); // todo: better error handling
    merge(*i, srctowers, dsttowers);

  }


  cout << __PRETTY_FUNCTION__ << ": "
       << "tracks.size()=" << dsttracks->size() << "  "
       << "towers.size()=" << dsttowers->size() << "  "
       << "filenomap.size()=" << filenomap.size() << " " 
       << endl;
  
  return 0;
}















///////////////////////////// real work ////////////////////////////////////////////


int EmcDataMerger::merge(std::string srcname, emcGeaTrackContainer * src, emcGeaTrackContainer * dest){


  for(size_t i = 0; i < src->size(); i++){
    emcGeaTrackContent * oldtrk = src->get(i);
    emcGeaTrackContent * newtrk = dest->find( oldtrk->get_trkno() );

    // create new track if necessary
    if( isSpecialTrkno(oldtrk->get_trkno()) ){
      if(newtrk == NULL){
	newtrk = emcGeaTrackContent::createdef( oldtrk->get_trkno() );
	newtrk->copy( oldtrk );
	newtrk->clear_deposits();
	if( dest->add( newtrk ) == -1 ) return -1;
	newtrk = dest->find( oldtrk->get_trkno() );
      }
    } else {
      assert( dest->find(oldtrk->get_trkno() + trknooffs[srcname]) == NULL );

      // copy trk & update trkno
      newtrk = emcGeaTrackContent::createdef();
      newtrk->copy( oldtrk );
      newtrk->clear_deposits();
      newtrk->set_trkno( newtrk->get_trkno() + trknooffs[srcname] );

      if( dest->add( newtrk ) == -1 ) return -1;
      newtrk = dest->find( oldtrk->get_trkno() + trknooffs[srcname] );

      // update input
      my_input_t key(srcname, oldtrk->get_input());
      if( filenomap.find(key) == filenomap.end() ) filenomap[key] = filenomap.size() + 1;
      newtrk->set_input( filenomap[key] );

      // update parent trkno
      if( !isSpecialTrkno(newtrk->get_parent_trkno()) )
	newtrk->set_parent_trkno( newtrk->get_parent_trkno() + trknooffs[srcname] );
      
      // update daughter list
      newtrk->clear_daughter_list();
      for(emc_tracklist_t::iterator j = oldtrk->get_daughter_list().begin(); j != oldtrk->get_daughter_list().end(); j++)
	if( !isSpecialTrkno(*j) ) newtrk->append_to_daughter_list( *j + trknooffs[srcname] );
	else newtrk->append_to_daughter_list( *j );
    }



    // copy deposit list
    emc_towerlist_t twrlist = oldtrk->get_tower_list();
    for(emc_towerlist_t::iterator i = twrlist.begin(); i != twrlist.end(); i++){
      emc_towerid_t towerid = *i;
      emcGeaDeposit::datatype_t type = emcGeaDeposit::GEA;
      float origedep, oldedep, origtof, oldtof;


      type = emcGeaDeposit::GEA;

      origedep = newtrk->get_edep_bytower(towerid, type); if ( isnan(origedep) ) origedep = 0.0;
      oldedep = oldtrk->get_edep_bytower(towerid, type); if ( isnan(oldedep) ) oldedep = 0.0;
      newtrk->set_edep_bytower(towerid, origedep + oldedep, type);

      origtof = newtrk->get_tof_bytower(towerid, type);
      oldtof = oldtrk->get_tof_bytower(towerid, type);
      if( !isnan(oldtof) && (isnan(origtof) || oldtof < origtof) ) 
	newtrk->set_tof_bytower(towerid, oldtof, type);
      

      type = emcGeaDeposit::ORIG;

      origedep = newtrk->get_edep_bytower(towerid, type); if ( isnan(origedep) ) origedep = 0.0;
      oldedep = oldtrk->get_edep_bytower(towerid, type); if ( isnan(oldedep) ) oldedep = 0.0;
      newtrk->set_edep_bytower(towerid, origedep + oldedep, type);

      origtof = newtrk->get_tof_bytower(towerid, type);
      oldtof = oldtrk->get_tof_bytower(towerid, type);
      if( !isnan(oldtof) && (isnan(origtof) || oldtof < origtof) ) 
	newtrk->set_tof_bytower(towerid, oldtof, type);
      

      type = emcGeaDeposit::CALIB;

      origedep = newtrk->get_edep_bytower(towerid, type); if ( isnan(origedep) ) origedep = 0.0;
      oldedep = oldtrk->get_edep_bytower(towerid, type); if ( isnan(oldedep) ) oldedep = 0.0;
      newtrk->set_edep_bytower(towerid, origedep + oldedep, type);

      origtof = newtrk->get_tof_bytower(towerid, type);
      oldtof = oldtrk->get_tof_bytower(towerid, type);
      if( !isnan(oldtof) && (isnan(origtof) || oldtof < origtof) ) 
	newtrk->set_tof_bytower(towerid, oldtof, type);
    }
    

  }


  return 0;
}





#define DIFF_WARNING(x) \
  if( !firstsrc  &&  srctower->x != dsttower->x ) \
    cerr << "EmcDataMerger::merge: towerid = " << srctower->towerid() << ": " \
         << "overwriting value for " << #x << ": "\
         << "was = " << srctower->x << "   new = " << dsttower->x \
         << std::endl;


int EmcDataMerger::merge(std::string srcname, emcGeaTowerContainer * src, emcGeaTowerContainer * dest){

  for(size_t i = 0; i < src->size(); i++){
    bool firstsrc = (i == 0);
    emcGeaTowerContent * srctower = src->get( i );
    emcGeaTowerContent * dsttower = dest->find( srctower->get_towerid() );

    if( dsttower == NULL ){
      dsttower = srctower->clone();

    /*
     * no need for this: tracklist is calculated on the fly
     *      dsttower->clear_tracks();
      emc_tracklist_t trklst = srctower->get_track_list();
      for(emc_tracklist_t::iterator j = trklst.begin(); j != trklst.end(); j++){
      	if( !isSpecialTrkno(*j) ) dsttower->add_track( *j + trknooffs[srcname] );
      	else dsttower->add_track( *j );
      }
    */

      if( dest->add( dsttower ) == -1 ) return -1;
      //continue;
      dsttower = dest->find( srctower->get_towerid() );
    }

    /*
     * no need for this: tracklist is calculated on the fly
     *
    // append tracks
    {    
      emc_tracklist_t trklst = srctower->get_track_list();
      for(emc_tracklist_t::iterator j = trklst.begin(); j != trklst.end(); j++){
	if( !isSpecialTrkno(*j) ) dsttower->add_track( *j + trknooffs[srcname] );
	else dsttower->add_track( *j );
      }
      }
    */

    /*
     * no need for this: calibrated values are calculated on the fly
     *
    if( srctower->hasCalib() && dsttower->canHaveCalib() ){
      float tof = srctower->ToF() < dsttower->ToF() ? srctower->ToF() : dsttower->ToF();
      float energy = dsttower->Energy() + srctower->Energy();
      dsttower->SetCalibrated( energy, tof );
      
      // todo: how can you set uncorrected tof?
      //virtual float UncorrectedToF() const { warning("UncorrectedToF"); return 0; }
    }
    */


    /*    
    if( srctower->hasGain() && dsttower->canHaveGain() ){
      if( dsttower->hasGain() ){
	DIFF_WARNING( HG() );
	DIFF_WARNING( LG() );
	DIFF_WARNING( ADC() );
	DIFF_WARNING( TDC() );
      }	else {
	dsttower->SetADCTDC( srctower->ADC(), srctower->TDC(), srctower->HG(), srctower->LG() );
      }
    }
    */

    /*
     * no need for this: simfrac is calculated on the fly..
     *
     if( srctower->canHaveMerSimStatus()  &&  dsttower->canHaveMerSimStatus() ){
     float simpart = 
	srctower->SimFrac() * srctower->Energy() +
	dsttower->SimFrac() * (dsttower->Energy() - srctower->Energy());

      dsttower->SetMerSimStatus( true, simpart > 0.0 );
      dsttower->SetSimFrac( simpart / dsttower->Energy() );
    }
    */
    /*
    // output will have invalid raw values when multiple sources have deposit
    // in the same tower!!!
    */


    // this will ruin gains
    if( dsttower->canHaveRaw() ){
      const static float lowgain_convfac = 0.001;
      const static float highgain_convfac = 0.008;
      const static float tdc_convfac = 0.05;

      int lopre = 4000;
      int hipre = 4000;
      int lopost = 4000 - ( int ) (dsttower->Energy() / lowgain_convfac);
      int hipost = 4000 - ( int ) (dsttower->Energy() / highgain_convfac);

      int tac = ( int )( dsttower->ToF() / tdc_convfac );

      dsttower->SetRaw( hipost, hipre, lopost, lopre, tac, 0, 0, 0, 0);
    }




    if( srctower->hasReference() ){
      // /// Whether this tower is a reference one.
      // virtual bool isReference(void) const;
      
      // /// Pointer to reference (if available, see hasReference()).
      // virtual emcTowerContent* Reference(void) const;
    }
    

    DIFF_WARNING( ErrorNeighbours() );
    DIFF_WARNING( WarnNeighbours() );
    dsttower->SetNeighbours( srctower->ErrorNeighbours() | dsttower->ErrorNeighbours(), 
			     srctower->WarnNeighbours() | dsttower->WarnNeighbours() 
			     );

    
    DIFF_WARNING( DataError() );
    dsttower->SetDataError( srctower->DataError() | dsttower->DataError() );
  }
    


  return 0;
}

#undef DIFF_WARNING


