

#include <typeinfo>
#include <string>

#include <Fun4AllReturnCodes.h>
#include <emcNodeHelper.h>

#include <emcGeaDepositContainer.h>
#include <emcGeaTrackContainer.h>
#include <emcGeaTrackContainervA.h>
#include <emcGeaTowerContainer.h>
#include <emcGeaTowerContainervA.h>
#include <emcGeaClusterContainer.h>
#include <emcGeaClusterContainervA.h>

#include <EmcRealContainerImporter.h>


ClassImp(EmcRealContainerImporter);





static std::ostream & operator << (std::ostream & stream, const EmcRealContainerImporter::srctype_t &type){

  switch(type){
  case EmcRealContainerImporter::UNSET: stream << "UNSET"; break;
  case EmcRealContainerImporter::TOWER: stream << "TOWER"; break;
  case EmcRealContainerImporter::CLUSTER: stream << "CLUSTER"; break;
  default: stream << "**unknown**"; break;
  }

  return stream;
}





int EmcRealContainerImporter::InitRun(PHCompositeNode * root){
  PHCompositeNode * thisroot = getroot( root );



  // check for input nodes
  PHCompositeNode* dst = emcNodeHelper::findCompositeNode(thisroot, "DST"); EMCNODEASSERT( dst );

  emcGeaTrackContainer * dsttracks = emcNodeHelper::getObject< emcGeaTrackContainer >("emcGeaTrackContainer", dst);
  emcTowerContainer * dsttowers = emcNodeHelper::getObject< emcTowerContainer >("emcTowerContainer", dst);
  emcClusterContainer * dstclusters = emcNodeHelper::getObject< emcClusterContainer >("emcClusterContainer", dst);

  if( srctype == UNSET ){
    if( dsttowers != NULL ) srctype = TOWER;
    else if( dstclusters != NULL ) srctype = CLUSTER;
  }

  std::cerr << __PRETTY_FUNCTION__ << ": srctype = " << srctype << std::endl;

  if( !dsttracks ){
    dsttracks = emcGeaTrackContainer::createdef();
    emcNodeHelper::insertObject< emcGeaTrackContainer >( dst, dsttracks, "emcGeaTrackContainer");
  }
  if( !dsttowers ){
    dsttowers = emcGeaTowerContainer::createdef();
    emcNodeHelper::insertObject< emcTowerContainer >( dst, dsttowers, "emcTowerContainer");
  }
  if( !dstclusters ){
    dstclusters = emcGeaClusterContainer::createdef();
    emcNodeHelper::insertObject< emcClusterContainer >( dst, dstclusters, "emcClusterContainer");
  }

  

  // create output nodes
  if ( !emcNodeHelper::makeCompositeNode(thisroot, "EMCDSTIMP", "-p") ) return ABORTRUN;
  PHCompositeNode* imp = emcNodeHelper::findCompositeNode(thisroot, "EMCDSTIMP"); EMCNODEASSERT( imp );

  emcGeaTrackContainer * tracks = emcGeaTrackContainer::createdef();
  emcGeaTowerContainer * towers = emcGeaTowerContainer::createdef();
  emcGeaClusterContainer * clusters = emcGeaClusterContainer::createdef();

  tracks->SetTowers( towers ); tracks->SetClusters( clusters );
  towers->SetTracks( tracks ); towers->SetClusters( clusters );
  clusters->SetTracks( tracks ); clusters->SetTowers( towers );

  emcNodeHelper::insertObject< emcGeaTrackContainer >(imp, tracks, "emcGeaTrackContainer");
  emcNodeHelper::insertObject< emcTowerContainer >(imp, towers, "emcTowerContainer");
  emcNodeHelper::insertObject< emcClusterContainer >(imp, clusters, "emcClusterContainer");



  return SubsysRecoStack::InitRun( root );
}





template<class T> static inline PHIODataNode<T> * X(PHNodeIterator * iter, const char * name){
  return static_cast< PHIODataNode< T >* >( iter->findFirst("PHIODataNode", name) );
}

static void switchnodes(PHCompositeNode * r1, PHCompositeNode * r2){ 

  PHNodeIterator iter1( r1 );
  PHIODataNode<emcGeaTrackContainer>* trk1 = X<emcGeaTrackContainer>( &iter1, "emcGeaTrackContainer" );
  PHIODataNode<emcTowerContainer>* twr1 = X<emcTowerContainer>( &iter1, "emcTowerContainer" );
  PHIODataNode<emcClusterContainer>* clr1 = X<emcClusterContainer>( &iter1, "emcClusterContainer" );
  assert( trk1 != NULL  &&  twr1 != NULL  &&  clr1 != NULL );

  PHNodeIterator iter2( r2 );
  PHIODataNode<emcGeaTrackContainer>* trk2 = X<emcGeaTrackContainer>( &iter2, "emcGeaTrackContainer" );
  PHIODataNode<emcTowerContainer>* twr2 = X<emcTowerContainer>( &iter2, "emcTowerContainer" );
  PHIODataNode<emcClusterContainer>* clr2 = X<emcClusterContainer>( &iter2, "emcClusterContainer" );
  assert( trk2 != NULL  &&  twr2 != NULL  &&  clr2 != NULL );

  emcGeaTrackContainer * trk = trk1->getData(); trk1->setData( trk2->getData() ); trk2->setData( trk );
  emcTowerContainer * twr = twr1->getData(); twr1->setData( twr2->getData() ); twr2->setData( twr );
  emcClusterContainer * clr = clr1->getData(); clr1->setData( clr2->getData() ); clr2->setData( clr );

}





int EmcRealContainerImporter::ResetEvent(PHCompositeNode * root){
  int rc = SubsysRecoStack::ResetEvent( root );

  // switch DST <-> IMPDST nodes back
  PHCompositeNode * thisroot = getroot( root );
  PHCompositeNode* dst = emcNodeHelper::findCompositeNode(thisroot, "DST"); EMCNODEASSERT( dst );
  PHCompositeNode* imp = emcNodeHelper::findCompositeNode(thisroot, "EMCDSTIMP"); EMCNODEASSERT( imp );
  if( switched ){ switchnodes(dst, imp); switched = !switched; }

  emcGeaTrackContainer * imptracks = emcNodeHelper::getObject< emcGeaTrackContainer >("emcGeaTrackContainer", imp);
  emcTowerContainer * imptowers = emcNodeHelper::getObject< emcTowerContainer >("emcTowerContainer", imp);
  emcClusterContainer * impclusters = emcNodeHelper::getObject< emcClusterContainer >("emcClusterContainer", imp);
  EMCNODEASSERT( imptracks ); EMCNODEASSERT( imptowers ); EMCNODEASSERT( impclusters );

  imptracks->Reset(); imptowers->Reset(); impclusters->Reset();

  return rc;
}





int EmcRealContainerImporter::process_event_cluster(PHCompositeNode * srcdst, PHCompositeNode * destdst){
  // get input and output nodes
  emcClusterContainer * srcclusters = emcNodeHelper::getObject< emcClusterContainer >("emcClusterContainer", srcdst);
  emcGeaTrackContainer * dsttracks = getGeaObject(emcGeaTrackContainer, emcGeaTrackContainer, "emcGeaTrackContainer", destdst);
  emcGeaTowerContainer * dsttowers = getGeaObject(emcGeaTowerContainer, emcTowerContainer, "emcTowerContainer", destdst);
  assert( srcclusters != NULL  &&  dsttracks != NULL  &&  dsttowers != NULL );



  // create special track for real data
  emcGeaTrackContent * realtrack = dsttracks->find( EMC_REALDATA_TRKNO );
  if( realtrack == NULL ){
    realtrack = emcGeaTrackContent::createdef( EMC_REALDATA_TRKNO );
    if( dsttracks->add(realtrack) == -1 ) return ABORTRUN;
    realtrack = dsttracks->find( EMC_REALDATA_TRKNO );
  }



  // fill deposit information based on cluster content
  for(size_t i = 0; i < srcclusters->size(); i++){
    emcClusterContent * cluster = srcclusters->getCluster( i );
    //if( cluster->has_id() == false ){
    //  std::cerr << __PRETTY_FUNCTION__ << ": cluster has no id! dropping.\n" << std::endl;
    //  continue;
    //}
    
    // for each tower contributing to this cluster..
    for(int j = 0; j < cluster->multiplicity(); j++){
      int towerid = cluster->towerid(j);
      float edep = cluster->partesum(j) - (j ? cluster->partesum(j-1) : 0);

      // ..add an entry in the tower container and
      emcGeaTowerContent * tower = dsttowers->find(towerid);
      if( tower == NULL ){
	tower = emcGeaTowerContent::createdef( towerid );
	if( dsttowers->add(tower) == -1 ) return ABORTRUN;

	tower = dsttowers->find( towerid );
	assert( tower->get_towerid() == towerid );
      }

      // ..increase the deposit in the special geatrack
      edep += realtrack->get_edep_bytower(towerid);
      realtrack->set_edep_bytower(towerid, edep);
    }

  }



  return EVENT_OK;
}





int EmcRealContainerImporter::process_event_tower(PHCompositeNode * srcdst, PHCompositeNode * destdst){
  // get input and output nodes
  emcTowerContainer * srctowers = emcNodeHelper::getObject< emcTowerContainer >("emcTowerContainer", srcdst);
  emcGeaTrackContainer * dsttracks = getGeaObject(emcGeaTrackContainer, emcGeaTrackContainer, "emcGeaTrackContainer", destdst);
  emcGeaTowerContainer * dsttowers = getGeaObject(emcGeaTowerContainer, emcTowerContainer, "emcTowerContainer", destdst);
  assert( srctowers != NULL  &&  dsttracks != NULL  &&  dsttowers != NULL );



  // create special track for real data
  emcGeaTrackContent * realtrack = dsttracks->find( EMC_REALDATA_TRKNO );
  if( realtrack == NULL ){
    realtrack = emcGeaTrackContent::createdef( EMC_REALDATA_TRKNO );
    if( dsttracks->add(realtrack) == -1 ) return ABORTRUN;
    realtrack = dsttracks->find( EMC_REALDATA_TRKNO );
  }



  // fill new style towertable from old towercontainer
  for(size_t i = 0; i < srctowers->size(); i++){
    emcTowerContent * srctower = srctowers->getTower( i );



    emcGeaTowerContent * dsttower = dsttowers->find( srctower->TowerID() );
    if( dsttower != NULL ){
      std::cerr << __PRETTY_FUNCTION__ << ": tower " << srctower->TowerID() << " alerady exists!" << std::endl;
      return ABORTEVENT;
    }
    dsttower = emcGeaTowerContent::createdef( srctower->TowerID() );
    if( dsttowers->add( dsttower ) == -1 ) return ABORTRUN;
    dsttower = dsttowers->find( srctower->TowerID() );



    // should hasRaw()
    dsttower->SetRaw(srctower->HGPost(), srctower->HGPre(),
		     srctower->LGPost(), srctower->LGPre(),
		     srctower->TAC(),
		     srctower->AMUPre(), srctower->AMUPost(), srctower->AMUTAC(),
		     srctower->BeamClock()
		     );
    dsttower->SetDataError( srctower->DataError() );

    // should hasDC()
    dsttower->SetADCTDC( srctower->ADC(), srctower->TDC(), dsttower->HG(), dsttower->LG() );

    // should hasCalib()
    realtrack->set_edep_bytower( dsttower->TowerID(), srctower->Energy() );
    // dsttower->SetToF( srctower->UncorrectedToF() );

    if( dsttower->canHaveGain() )
      dsttower->SetGain( srctower->Gain() );

    
    dsttower->SetNeighbours( srctower->ErrorNeighbours(), srctower->WarnNeighbours() );
  }



  return EVENT_OK;
}





int EmcRealContainerImporter::process_event(PHCompositeNode * root){

  // switch DST <-> IMPDST nodes
  PHCompositeNode * thisroot = getroot( root );
  PHCompositeNode* dst = emcNodeHelper::findCompositeNode(thisroot, "DST"); EMCNODEASSERT( dst );
  PHCompositeNode* imp = emcNodeHelper::findCompositeNode(thisroot, "EMCDSTIMP"); EMCNODEASSERT( imp );
  switchnodes(dst, imp); switched = !switched;



  int rc;

  switch(srctype){
  case TOWER: 
    rc = process_event_tower(imp, dst);
    break;

  case CLUSTER: 
    rc = process_event_cluster(imp, dst);
    break;

  default: 
    std::cerr << __PRETTY_FUNCTION__ << ": incorrect srctype: " << srctype << std::endl; 
    rc = ABORTRUN;
    break;
  }

  if( rc != EVENT_OK ) return rc;



  return SubsysRecoStack::process_event( root );
}




