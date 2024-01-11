
#include "SvxTrack_v1.h"
//#include "SvxTrackProjection.h"

ClassImp(SvxTrack_v1)

using namespace std;

//_____________________________________________________________________
SvxTrack_v1::SvxTrack_v1()
{}

//_____________________________________________________________________
bool SvxTrack_v1::has_fit(SvxTrackFit::FitType type) const
{
  fit_list_type::const_iterator fititer = _fit_list.find(type);
  return (fititer!=_fit_list.end());
}

//_____________________________________________________________________
const SvxTrackFit* SvxTrack_v1::get_fit(SvxTrackFit::FitType type) const
{
  // loop over the stored fits and look for the desired code
  // if the fit isn't found return zero
  fit_list_type::const_iterator fititer = _fit_list.find(type);
  if(fititer!=_fit_list.end())
    return &(fititer->second);
  else
    return 0;
}

//_____________________________________________________________________
SvxTrackFit* SvxTrack_v1::get_fit(SvxTrackFit::FitType type)
{
  // loop over the stored fits and look for the desired code
  // if the fit isn't found return zero
  fit_list_type::iterator fititer = _fit_list.find(type);
  if(fititer!=_fit_list.end())
    return &(fititer->second);
  else
    return 0;
}

//_____________________________________________________________________
void SvxTrack_v1::add_fit(const SvxTrackFit* basefit)
{
  const SvxTrackFit_v1* fitptr = dynamic_cast<const SvxTrackFit_v1*>(basefit);
  if( !fitptr ){
    cerr << "SvxTrack_v1::add_fit() only accepts SvxTrackFit_v1!" << endl;
    return;
  }

  // loop over the stored fits and replace the type if already existing
  // otherwise add to the list of fits as a new entry
  SvxTrackFit::FitType type = basefit->get_fit_type();
  // fit_list_type::iterator fititer = _fit_list.find(type);
  // if(fititer!=_fit_list.end())
  //   fititer->second = *fitptr;
  // else
  _fit_list[type] = *fitptr;
  return;
}

//_____________________________________________________________________
vector<SvxCluster*> SvxTrack_v1::get_clusters_in_layer(UShort_t ilayer) const
{
  vector<SvxCluster*> clusters;
  for(unsigned int i=0; i<_cluster_list.size(); i++) {
    if(_cluster_list[i]->get_layer() == ilayer) {
      clusters.push_back(_cluster_list[i]);
    }
  }

  return clusters;
}

//_____________________________________________________________________
vector<SvxCluster*> SvxTrack_v1::get_clusters_ordered() const
{
  vector<SvxCluster*> clusters = _cluster_list;
  sort(clusters.begin(), clusters.end(), cluster_radius_less_than());
  return clusters;
}

//_____________________________________________________________________
vector<SvxCluster*> SvxTrack_v1::get_clusters() const
{
  vector<SvxCluster*> clusters = _cluster_list;
  return clusters;
}

//_____________________________________________________________________
bool SvxTrack_v1::has_cluster_in_layer(UShort_t ilayer) const
{
  return (get_clusters_in_layer(ilayer).size() > 0);
}

//_____________________________________________________________________
bool SvxTrack_v1::has_cluster(int cluster_id) const
{
  return (find(_cluster_id_list.begin(),
               _cluster_id_list.end(),
               cluster_id) != _cluster_id_list.end());
}

//_____________________________________________________________________
bool SvxTrack_v1::has_cluster(const SvxCluster* clus_ptr) const
{
  return (find(_cluster_list.begin(),
               _cluster_list.end(), clus_ptr) != _cluster_list.end());
}

//_____________________________________________________________________
void SvxTrack_v1::add_cluster(SvxCluster *cluster)
{
  // push back a new entry
  _cluster_list.push_back(cluster);
  _cluster_id_list.push_back(cluster->get_hitID());
  return;
}

//_____________________________________________________________________
void SvxTrack_v1::remove_cluster(SvxCluster *clus_ptr)
{
  int clusid = clus_ptr->get_hitID();
  remove(_cluster_list.begin(), _cluster_list.end(), clus_ptr);
  remove(_cluster_id_list.begin(), _cluster_id_list.end(), clusid);
  return;
}

//_____________________________________________________________________
void SvxTrack_v1::pushback_cluster_pointer(SvxCluster *cluster)
{
  // push back just the cluster pointer
  _cluster_list.push_back(cluster);
  return;
}

//_____________________________________________________________________
void SvxTrack_v1::print(std::ostream& os, bool max) const
{
  static unsigned int nprint = 0;
  if(max && nprint>=10) return;

  os << "This SvxTrack_v1 has " << _cluster_list.size()
     << " associated clusters and " << _fit_list.size() << " fits." << endl;

  if(_cluster_list.size()) os << "Clusters:" << endl;
  for(unsigned int iclus=0; iclus<_cluster_list.size(); iclus++) {
    cout << " " << iclus << ": " << _cluster_list[iclus]
         << "  hitID: " << _cluster_list[iclus]->get_hitID() << endl;
  }

  if(_fit_list.size()) os << "Fits:" << endl;

  for(fit_list_type::const_iterator iter = _fit_list.begin(); iter!=_fit_list.end(); ++iter)
    iter->second.print();
 
  return;
}

