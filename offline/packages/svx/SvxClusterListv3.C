// ========================
// FILE: SvxClusterListv3.C
// ========================
// ****************************************************************************
// Implementation of the container SvxClusterListv3 for the SvxClusterv3
// ---
// Created by Sasha Lebedev <lebedev@iastate.edu> in July 2010
//
// ****************************************************************************

#include <SvxClusterListv3.h>

#include <algorithm>
#include <cstdlib>
#include <vector>

using namespace std;

ClassImp(SvxClusterListv3)

// Constructor(s) & destructor
// """""""""""""""""""""""""""
SvxClusterListv3::SvxClusterListv3(const unsigned int length)
{
  m_hit_list   = new TClonesArray("SvxClusterv3", length);
  m_hit_id_unused = 0;
  unSort();
  listPointersSet = false;
  //std::cout << "SvxClusterListv3 object created" << std::endl;
}

SvxClusterListv3::~SvxClusterListv3()
{
  delete m_hit_list;
  //std::cout << "SvxClusterListv3 object destroyed" << std::endl;
}

// The "standard PHObject response" functions...
// """""""""""""""""""""""""""""""""""""""""""""
void SvxClusterListv3::Reset()
{
  m_hit_id_unused = 0;
  m_hit_list->Clear();
  if ( m_hit_list->GetSize() > SVXNCLUSTER ) m_hit_list->Expand(SVXNCLUSTER);
  unSort();
  listPointersSet = false;
}

int SvxClusterListv3::isValid() const
{
  return ( ( get_nClusters() > 0 ) ? 1 : 0 ) ;
}

void SvxClusterListv3::identify(std::ostream &os) const
{
  os << "SvxClusterListv3 object: nClusters = "
     << get_nClusters() << std::endl;
}

// Add/remove/set/get methods...
// """""""""""""""""""""""""""""
SvxCluster* SvxClusterListv3::addCluster(const int ihit)
{
  int         index = ( ihit < 0 ) ? get_nClusters() : ihit ;
  TClonesArray   &P = *m_hit_list;
  SvxCluster* cluster = new(P[index]) SvxClusterv3(this);
  cluster->set_hitID(m_hit_id_unused++);
  if ( get_nClusters() == 0 ) listPointersSet = true;
  unSort();
  return cluster;
}

void SvxClusterListv3::removeCluster (const unsigned int ihit)
{
  m_hit_list->RemoveAt(ihit) ;
  unSort();
}

// Routines to manipulate the cluster array...
// """""""""""""""""""""""""""""""""""""""""""
int SvxClusterListv3::Compress()
{
  m_hit_list->Compress();
  return get_nClusters();
}

int SvxClusterListv3::set_TClonesArraySize (const unsigned int nhit)
{
  if ( nhit > (unsigned int)get_nClusters() ) m_hit_list->Expand(nhit);
  return m_hit_list->GetSize();
}

// Sorting and index search
// """"""""""""""""""""""""
void SvxClusterListv3::restoreListPointers ()
{
  SvxClusterv3* cluster = NULL;
  for (int i = 0; i < get_nClusters(); i++ ) {
    if ( ( cluster = (SvxClusterv3*) m_hit_list->UncheckedAt(i) ) )
      cluster->set_ListPointer(this);
  }
  listPointersSet = true;
}

void SvxClusterListv3::sort_hitID () {
  if ( hitIDsorted      ) return;
  if ( !listPointersSet ) restoreListPointers();
  SvxClusterv3 hit;
  hit.set_sortingSwitch(-1);
  sensorIDsorted = false;
//  m_hit_list->UnSort();
  SortSTL();
//  m_hit_list->Sort(get_nClusters());
  hitIDsorted    = true;
}

void SvxClusterListv3::sort_sensorID () {
  if ( sensorIDsorted   ) return;
  if ( !listPointersSet ) restoreListPointers();
  SvxClusterv3 hit;
  hit.set_sortingSwitch(1);
  hitIDsorted    = false;
//  m_hit_list->UnSort();
  SortSTL();
//  m_hit_list->Sort(get_nClusters());
  sensorIDsorted = true;
}

void SvxClusterListv3::unSort() {
  hitIDsorted    = false;
  sensorIDsorted = false;
  m_hit_list->UnSort();
}

void SvxClusterListv3::SortSTL()
{
   int n_ent = get_nClusters();

   // make and sort an temporal array
   vector<SvxHit*> hlist(n_ent);
   for (int i = 0; i < n_ent; i++) {
      hlist[i] = get_Cluster(i)->Clone();
   }

   sort(hlist.begin(), hlist.end(), SvxHit::CompPointer);
   // copy parameters and delete the item of the temporal array
   for (int i = 0; i < n_ent; i++) {
      get_Cluster(i)->Copy(hlist[i]);
      delete hlist[i];
   }
}

// Find first occurrence of cluster
int SvxClusterListv3::indexOfCluster (
   const int hitid, int ifrom, int iupto) const
{
  if (get_nClusters() == 0) return -1;

  if (iupto < 0) iupto = get_nClusters() - 1;
  if (ifrom < 0 || iupto >= get_nClusters() || ifrom > iupto) {
     std::cerr << "SvxClusterListv3::indexOfCluster(): Invalid search range, from "
               << ifrom << " upto " << iupto << ", where the hit list has " 
               << get_nClusters() << " entries." << std::endl;
     exit(1);
  }

  if ( iupto-ifrom > 8 && hitIDsorted ) {
     int i_low = ifrom - 1;
     int i_up  = iupto + 1;
     while (i_up - i_low > 1) {
        int i_cent     = (i_low + i_up) / 2;
        int hitid_curr = get_Cluster(i_cent)->get_hitID();
        if      (hitid_curr == hitid) { return i_cent; }
        else if (hitid_curr <  hitid) { i_low = i_cent; }
        else                          { i_up = i_cent; }
     }
     return -1;

  } else {

    SvxCluster* phit = NULL;
    for ( int i = ifrom; i <= iupto; i++ ) {
      if ( ( phit = get_Cluster(i) ) ) {
	if ( phit->get_hitID() == hitid ) return i;
      }
    }
    return -1;

  } // if (indto-ifrom > 8 && hitIDsorted)
}

//int SvxClusterListv3::indexOfCluster (const SvxSensor* sensor,
//				      const int         ifrom,
//				      const int         iupto) const {
//
//  int indto = ( iupto < 0 ) ? get_nClusters() : iupto;
//
//  if ( indto-ifrom > 8 && sensorIDsorted ) {
//
//    SvxClusterv3 hit;
//    hit.set_svxSection(sensor->get_svxSection());
//    hit.set_layer(sensor->get_layer());
//    hit.set_ladder(sensor->get_ladder());
//    hit.set_sensor(sensor->get_sensor());
//    hit.set_sortingSwitch(1);
//    int ind = m_hit_list->BinarySearch(&hit, indto);
//    if ( (ind == ifrom ) || ind < 0 ) return ind;
//    SvxCluster* phit = NULL;
//    if ( ind < ifrom ) {
//      for ( int i = ifrom; i < indto; i++ ) {
//	if ( ( phit = get_Cluster(i) ) ) {
//	  return ( sensor->checkID(phit) ) ? i : -1;
//	}
//      }
//      return -1;
//    } else {
//      int ilast = ind;
//      for ( int i = ind-1; i >= ifrom; i-- ) {
//	if ( ( phit = get_Cluster(i) ) ) {
//	  if ( sensor->checkID(phit) ) {
//	    ilast = i;
//	  } else {
//	    return ilast;
//	  }
//	}
//      }
//      return ilast;
//    }
//
//  } else {
//
//    SvxCluster* phit = NULL;
//    for ( int i = ifrom; i < indto; i++ ) {
//      if ( ( phit = get_Cluster(i) ) ) {
//	if ( sensor->checkID(phit) ) return i;
//      }
//    }
//    return -1;
//
//  } // if (indto-ifrom > 8 && hitIDsorted)
//}

int SvxClusterListv3::indexOfCluster (const SvxCluster* hit  ,
				      const int         ifrom,
				      const int         iupto) const {
  int indto = ( iupto < 0 ) ? get_nClusters() : iupto ;
  for ( int i = ifrom; i < indto; i++ ) {
    if ( get_Cluster(i) == hit ) return i;
  }
  return -1;
}

bool SvxClusterListv3::indexOfCluster(const SvxSensor* sensor, 
                                      int& idx_lb, int& idx_ub, 
                                      int ifrom, int iupto) const
{
   if (! sensorIDsorted) {
      std::cerr << "SvxClusterListv3::indexOfCluster(): The hit list hasn't been sorted by sensorID." << std::endl;
      exit(1);
   }
   idx_lb = -1;
   idx_ub = -2;
   if (get_nClusters() == 0) return false;
   if (iupto < 0) iupto = get_nClusters() - 1;
   if (ifrom < 0 || iupto >= get_nClusters() || ifrom > iupto) {
      std::cerr << "SvxClusterListv3::indexOfCluster(): Invalid search range, from "
           << ifrom << " upto " << iupto << ", where the hit list has " 
           << get_nClusters() << " entries." << std::endl;
      exit(1);
   }

   SvxClusterv3* hit_cmp = new SvxClusterv3();
   hit_cmp->set_svxSection(sensor->get_svxSection());
   hit_cmp->set_layer     (sensor->get_layer());
   hit_cmp->set_ladder    (sensor->get_ladder());
   hit_cmp->set_sensor    (sensor->get_sensor());
   hit_cmp->set_sortingSwitch(1); // 1 = sort by sensor ID

   int i_low = ifrom - 1;
   int i_up  = iupto + 1;
   int i_mid = -1; // index of a first matched entry
   while (i_up - i_low > 1) {
      int i_cent     = (i_low + i_up) / 2;
      int cmp = hit_cmp->Compare(get_Cluster(i_cent));
      if      (cmp == 0) { i_mid = i_cent;  break; }
      else if (cmp >  0) { i_low = i_cent; }
      else               { i_up  = i_cent; }
   }
   if (i_mid < 0) {
      delete hit_cmp;
      return false;
   }

   idx_lb = idx_ub = i_mid;
   for (; idx_lb > i_low + 1 && hit_cmp->Compare(get_Cluster(idx_lb - 1)) == 0; idx_lb--);
   for (; idx_ub < i_up  - 1 && hit_cmp->Compare(get_Cluster(idx_ub + 1)) == 0; idx_ub++);

   delete hit_cmp;
   return true;
}

// Methods
// """""""
void SvxClusterListv3::print() const
{
  std::cout << "SvxClusterListv3: nClusters = " << get_nClusters()
       << " hitIDsorted = "    << hitIDsorted
       << " sensorIDsorted = " << sensorIDsorted << std::endl;
  SvxCluster* cluster = NULL;
  for (int i=0; i < get_nClusters(); i++) {
    std::cout << "SvxCluster number " << i << ":";
    if( (cluster = get_Cluster(i)) ) {
      std::cout << std::endl;
      cluster->print();
    } else {
      std::cout << " missing ..." << std::endl;
    }
  }
}



