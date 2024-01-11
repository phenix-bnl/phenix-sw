
#include "SvxTrackList_v1.h"
#include "SvxClusterList.h"

#include <iostream>
#include <map>

ClassImp(SvxTrackList_v1)

using namespace std;

//_____________________________________________________________________
SvxTrackList_v1::SvxTrackList_v1() :
  _next_index(0),
  _track_array(0)
{      
  //_track_array = new TClonesArray( ValueImp().ClassName(), 10 );
  _track_array = new TObjArray( 10 );
}

//_____________________________________________________________________
SvxTrackList_v1::~SvxTrackList_v1()
{
  _track_array->Delete();
  delete _track_array;
}

//_____________________________________________________________________
void SvxTrackList_v1::Reset()
{
  //_track_array->Clear(); // dangerous since Clear() doesn't call destructors
  _track_array->Delete();
  _next_index = 0;
  return;
}

//_____________________________________________________________________
void SvxTrackList_v1::identify(ostream& os) const
{
  os << "identify yourself: SvxTrackList_v1 Object\n"
     << "# of tracks: " << size() << std::endl;
  return;
}

//_____________________________________________________________________
void SvxTrackList_v1::insert_track(const SvxTrack* trk_ptr)
{
  // create instance directly in array, we can thank ROOT for this
  // horrible syntax
  const ValueImp* versioned_ptr = dynamic_cast<const ValueImp*>(trk_ptr);
  if(versioned_ptr)
    new((*_track_array)[_next_index++]) ValueImp(*versioned_ptr);
  return;
}
 
//_____________________________________________________________________
SvxTrack* SvxTrackList_v1::get_new_track()
{
  // create instance directly in array, we can thank ROOT for this
  // horrible syntax
  //return new((*_track_array)[_next_index++]) ValueImp(); 
  SvxTrack* tptr = new ValueImp();
  _track_array->Add(tptr);
  return tptr;
}
 
//_____________________________________________________________________
void SvxTrackList_v1::remove_track(unsigned int itrk)
{
  _track_array->RemoveAt(itrk);
  return;
}

//_____________________________________________________________________
void SvxTrackList_v1::remove_track(SvxTrack* trk)
{
  _track_array->Remove(trk);
  return;
}

//_____________________________________________________________________
SvxTrack* SvxTrackList_v1::get_track(unsigned int itrk) const
{
  SvxTrack *track = static_cast<SvxTrack*>(_track_array->At(itrk));
  return track;
}

//_____________________________________________________________________
void SvxTrackList_v1::load_associations(SvxClusterList* clus_list)
{
  // allow multiple associated tracks per cluster (probably not the case)
  multimap<int, SvxTrack*> clusid_trk_map;
  typedef multimap<int, SvxTrack*>::iterator mapiter;
  typedef pair<mapiter,mapiter> maprange;

  // first make a map of cluster ID -> SvxTrack
  for(unsigned int itrk=0; itrk<size(); itrk++) {
    SvxTrack* trk_ptr = get_track(itrk);
    vector<int> clusids = trk_ptr->get_cluster_id_list();
    for(unsigned int iclus=0; iclus<clusids.size(); iclus++)
      clusid_trk_map.insert( make_pair(clusids[iclus], trk_ptr) );
  }

  // loop over all clusters {
  //   get corresponding tracks using the cluster ID
  //   loop over corresponding tracks
  //     add cluster pointer to the track's list of associations
  for(int iclus=0; iclus<clus_list->get_nClusters(); iclus++) {
    SvxCluster* clus_ptr = clus_list->get_Cluster(iclus);
    maprange therange = clusid_trk_map.equal_range( clus_ptr->get_hitID() );
    for(mapiter theiter=therange.first; theiter!=therange.second; theiter++) {
      SvxTrack* trk_ptr = theiter->second;
      trk_ptr->pushback_cluster_pointer(clus_ptr);
    }
  }
  return;
}

//_____________________________________________________________________
// void SvxTrackList_v1::save_associations(SvxClusterList* clus_list)
// {
  // This function isn't needed if we keep the cluster IDs in the
  // Track objects, and keep it in sync with the list of cluster
  // pointers at all times.

//   map<SvxCluster*, unsigned int> clus_ptr_map;
//   for(int iclus=0; iclus<clus_list->get_nClusters(); iclus++) {
//     SvxCluster* clus_ptr = clus_list->get_Cluster(iclus);
//     clus_ptr_map[clus_ptr] = iclus;
//   }

//   unsigned int iassoc = 0;
//   for(unsigned int itrk=0; itrk<size(); itrk++) {
//     SvxTrack* trk_ptr = get_track(itrk);
//     vector<SvxCluster*> clus_ptrs = trk_ptr->get_clusters();

//     for(unsigned int j=0; j<clus_ptrs.size(); j++) {
//       SvxCluster* clus_ptr = clus_ptrs[j];
//       unsigned int iclus = clus_ptr_map[clus_ptr];
//       new((*_association_array)[iassoc++]) Association(iclus,itrk);       
//     }
//   }
//   return;
// }
