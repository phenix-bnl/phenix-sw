//////////////////////////////////////////////////////////////////////////////////
//
// SubsysReco that resotres the links between emc containers 
// (emcGeaTrackContainer, emcGeaTowerContainer and emcGeaClusterContainer) 
// read in from disk.
//
//////////////////////////////////////////////////////////////////////////////////


#include <typeinfo>

#include <Fun4AllReturnCodes.h>
#include <emcNodeHelper.h>

#include <emcGeaTrackContainer.h>
#include <emcGeaTrackContainervA.h>
#include <emcGeaTowerContainer.h>
#include <emcGeaTowerContainervA.h>
#include <emcGeaClusterContainer.h>
#include <emcGeaClusterContainervA.h>

#include <EmcGeaContainerImporter.h>


ClassImp(EmcGeaContainerImporter);






int EmcGeaContainerImporter::process_event(PHCompositeNode * root){
  PHCompositeNode * thisroot = getroot( root );

  PHCompositeNode* dstnode = emcNodeHelper::findCompositeNode(thisroot, "DST"); 
  EMCNODEASSERT( dstnode );

  emcGeaTrackContainer * tracks = getGeaObject( emcGeaTrackContainer, emcGeaTrackContainer, "emcGeaTrackContainer", dstnode);
  emcGeaTowerContainer * towers = getGeaObject( emcGeaTowerContainer, emcTowerContainer, "emcTowerContainer", dstnode);
  emcGeaClusterContainer * clusters = getGeaObject( emcGeaClusterContainer, emcClusterContainer, "emcClusterContainer", dstnode);




  if( tracks ){
    tracks->invcache();

    // nice crosscast crafted by hand ( emcGeaTrackContainer -> emcContainerT<emcGeaTrackContent> )
    emcGeaTrackContainervA * ptracks = dynamic_cast< emcGeaTrackContainervA * >( tracks );
    if( ptracks == NULL ){
      std::cerr << __PRETTY_FUNCTION__ << ": could not cast emcGeaTrackContainer to emcGeaTrackContainervA." << std::endl;
      return ABORTRUN;
    }


    ptracks->makeindex();

    for(size_t i = 0; i < tracks->size(); i++)
      tracks->get(i)->set_container( ptracks );

    if( !towers ){
      std::cerr << __PRETTY_FUNCTION__ << ": found tracks, but no towers!" << std::endl;
      return ABORTRUN;
    }

    tracks->SetTowers( towers );

    if( clusters ) tracks->SetClusters( clusters );
  }



  if( towers ){
    towers->invcache();

    // nice crosscast crafted by hand
    emcGeaTowerContainervA * ptowers = dynamic_cast< emcGeaTowerContainervA * >( towers );
    if( ptowers == NULL ){
      std::cerr << __PRETTY_FUNCTION__ << ": could not cast emcTowerContainer to emcGeaTowerContainervA." << std::endl;
      return ABORTRUN;
    }

    ptowers->makeindex();

    for(size_t i = 0; i < towers->size(); i++)
      towers->get(i)->set_container( ptowers );


    if( !tracks ){
      std::cerr << __PRETTY_FUNCTION__ << ": found towers, but no tracks!" << std::endl;
      return ABORTRUN;
    }
    towers->SetTracks( tracks );


    if( clusters ) towers->SetClusters( clusters );
  }



  if( clusters ){
    clusters->invcache();

    // nice crosscast crafted by hand
    emcGeaClusterContainervA * pclusters = dynamic_cast< emcGeaClusterContainervA * >( clusters );
    if( pclusters == NULL ){
      std::cerr << __PRETTY_FUNCTION__ << ": could not cast emcClusterContainer to emcGeaClusterContainervA." << std::endl;
      return ABORTRUN;
    }

    pclusters->makeindex();

    for(size_t i = 0; i < clusters->size(); i++)
      clusters->get(i)->set_container( pclusters );


    if( !tracks ){
      std::cerr << __PRETTY_FUNCTION__ << ": found clusters, but no tracks!" << std::endl;
      return ABORTRUN;
    }
    clusters->SetTracks( tracks );


    if( !towers ){
      std::cerr << __PRETTY_FUNCTION__ << ": found clusters, but no towers!" << std::endl;
      return ABORTRUN;
    }
    clusters->SetTowers( towers );
  }




#if 0
  if(tracks) printf("tracks   = %30s @ %p  %4d entries\n", typeid(*tracks).name(), tracks, tracks->size());
  if(towers) printf("towers   = %30s @ %p  %4d entries\n", typeid(*towers).name(), towers, towers->size());
  if(clusters) printf("clusters = %30s @ %p  %4d entries\n", typeid(*clusters).name(), clusters, clusters->size());
#endif



    
  return SubsysRecoStack::process_event( root );
}
