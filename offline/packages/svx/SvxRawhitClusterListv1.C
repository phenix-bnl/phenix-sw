// ==============================
// FILE: SvxRawhitClusterListv1.C
// ==============================
// ****************************************************************************
// Implementation of the container SvxRawhitClusterListv1
// ---
// Created  by V. L. Rykov on 15-Feb-2003
//
// Modified by V. L. Rykov on 11-May-2004:
//     Sorting and index search methods added.
// ****************************************************************************

#include <SvxRawhitClusterListv1.h>

#include <algorithm>
#include <cstdlib>
#include <vector>

ClassImp(SvxRawhitClusterListv1)

using namespace std;

// Constructor(s) & destructor
// """""""""""""""""""""""""""
SvxRawhitClusterListv1::SvxRawhitClusterListv1(const unsigned int length)
{
  RawhitCluster   = new TClonesArray("SvxRawhitClusterv1", length);
  nRawhitClusters = RawhitCluster->GetLast() + 1;
  unSort();
  listPointersSet = false;
  //std::cout << "SvxRawhitClusterListv1 object created" << std::endl;
}

SvxRawhitClusterListv1::~SvxRawhitClusterListv1()
{
  delete RawhitCluster;
  //std::cout << "SvxRawhitClusterListv1 object destroyed" << std::endl;
}

// The "standard PHObject response" functions...
// """""""""""""""""""""""""""""""""""""""""""""
void SvxRawhitClusterListv1::Reset()
{
  RawhitCluster->Clear();
  if ( RawhitCluster->GetSize() > SVXNRAWHITCLUSTER )
    RawhitCluster->Expand(SVXNRAWHITCLUSTER);
  nRawhitClusters = RawhitCluster->GetLast() + 1 ;
  unSort();
  listPointersSet = false;
}

int SvxRawhitClusterListv1::isValid() const
{
  return ( ( nRawhitClusters > 0 ) ? 1 : 0 ) ;
}

void SvxRawhitClusterListv1::identify(std::ostream &os) const
{
  os << "Identify yourself: SvxRawhitClusterListv1 object: nRawhitClusters = "
     << nRawhitClusters << std::endl;
}

// Add/remove/set/get methods...
// """""""""""""""""""""""""""""
SvxRawhitCluster* SvxRawhitClusterListv1::addRawhitCluster(const int ihit)
{
  int       index = ( ihit < 0 ) ? nRawhitClusters : ihit ;
  TClonesArray &P = *RawhitCluster;
  SvxRawhitCluster* rawhitcluster = new(P[index]) SvxRawhitClusterv1(this);
  if ( nRawhitClusters == 0 ) listPointersSet = true;
  nRawhitClusters = RawhitCluster->GetLast() + 1;
  unSort();
  return rawhitcluster;
}

void SvxRawhitClusterListv1::removeRawhitCluster (const unsigned int ihit)
{
  RawhitCluster->RemoveAt(ihit) ;
  unSort();
}

// Routines to manipulate the cluster array...
// """""""""""""""""""""""""""""""""""""""""""
int SvxRawhitClusterListv1::Compress()
{
  RawhitCluster->Compress();
  return (int) (nRawhitClusters = RawhitCluster->GetLast() + 1);
}

int SvxRawhitClusterListv1::set_TClonesArraySize (const unsigned int nhit)
{
  if ( nhit > nRawhitClusters ) RawhitCluster->Expand(nhit);
  return RawhitCluster->GetSize();
}

// Sorting and index search
// """"""""""""""""""""""""
void SvxRawhitClusterListv1::restoreListPointers()
{
  SvxRawhitClusterv1* r2c = NULL;
  for (unsigned int i = 0; i < nRawhitClusters; i++ ) {
    if ( ( r2c = (SvxRawhitClusterv1*) RawhitCluster->UncheckedAt(i) ) )
      r2c->set_ListPointer(this);
  }
  listPointersSet = true;
}

void SvxRawhitClusterListv1::sortRawhits() {
  if ( rawhitSorted ) return;
  if ( !listPointersSet ) restoreListPointers();
  SvxRawhitClusterv1 r2c;
  r2c.set_sortingSwitch(-1);
  clusterSorted = false;
//  RawhitCluster->UnSort();
  SortSTL();
//  RawhitCluster->Sort(nRawhitClusters);
  rawhitSorted  = true;
}

void SvxRawhitClusterListv1::sortClusters() {
  if ( clusterSorted ) return;
  if ( !listPointersSet ) restoreListPointers();
  SvxRawhitClusterv1 r2c;
  r2c.set_sortingSwitch(1);
  rawhitSorted  = false;
//  RawhitCluster->UnSort();
  SortSTL();
//  RawhitCluster->Sort(nRawhitClusters);
  clusterSorted = true;
}

void SvxRawhitClusterListv1::unSort() {
  rawhitSorted  = false;
  clusterSorted = false;
  RawhitCluster->UnSort();
}

void SvxRawhitClusterListv1::SortSTL()
{
   int n_ent = get_nRawhitClusters();

   // make and sort an temporal array
   vector<SvxRawhitCluster*> hlist(n_ent);
   for (int i = 0; i < n_ent; i++) {
      hlist[i] = get_RawhitCluster(i)->Clone();
   }

   sort(hlist.begin(), hlist.end(), SvxRawhitCluster::CompPointer);
   // copy parameters and delete the item of the temporal array
   for (int i = 0; i < n_ent; i++) {
      get_RawhitCluster(i)->Copy(hlist[i]);
      delete hlist[i];
   }
}

//// Find first occurrence of rawhit
//int SvxRawhitClusterListv1::indexOfRawhit (const int id   ,
//					   const int ifrom,
//					   const int iupto) const {
//
//  int indt = ( iupto < 0 ) ? nRawhitClusters : iupto;
//
//  if ( indt-ifrom > 8 && rawhitSorted ) {
//
//    SvxRawhitClusterv1 r2c;
//    r2c.set_rawhitID(id);
//    r2c.set_sortingSwitch(-1);
//    int ind = RawhitCluster->BinarySearch(&r2c, indt);
//    if ( (ind == ifrom) || ind < 0 ) return ind;
//    SvxRawhitCluster* pr2c = NULL;
//    if ( ind < ifrom ) {
//      for ( int i = ifrom; i < indt; i++ ) {
//	if ( ( pr2c = get_RawhitCluster(i) ) ) {
//	  return ( pr2c->get_rawhitID() == id ) ? i : -1;
//	}
//      }
//      return -1;
//    } else {
//      int ilast = ind;
//      for ( int i = ind-1; i >= ifrom; i-- ) {
//	if ( ( pr2c = get_RawhitCluster(i) ) ) {
//	  if ( pr2c->get_rawhitID() == id ) {
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
//    SvxRawhitCluster* pr2c = NULL;
//    for ( int i = ifrom; i < indt; i++ ) {
//      if ( ( pr2c = get_RawhitCluster(i) ) ) {
//	if ( pr2c->get_rawhitID() == id ) return i;
//      }
//    }
//    return -1;
//
//  } // if (indt-ifrom > 8 && rawhitSorted)
//}
//
//// Find first occurrence of cluster
//int SvxRawhitClusterListv1::indexOfCluster (const int id   ,
//					    const int ifrom,
//					    const int iupto) const {
//
//  int indt = ( iupto < 0 ) ? nRawhitClusters : iupto;
//
//  if ( indt-ifrom > 8 && clusterSorted ) {
//
//    SvxRawhitClusterv1 r2c;
//    r2c.set_clusterID(id);
//    r2c.set_sortingSwitch(1);
//    int ind = RawhitCluster->BinarySearch(&r2c, indt);
//    if ( (ind == ifrom) || ind < 0 ) return ind;
//    SvxRawhitCluster* pr2c = NULL;
//    if ( ind < ifrom ) {
//      for ( int i = ifrom; i < indt; i++ ) {
//	if ( ( pr2c = get_RawhitCluster(i) ) ) {
//	  return ( pr2c->get_clusterID() == id ) ? i : -1;
//	}
//      }
//      return -1;
//    } else {
//      int ilast = ind;
//      for ( int i = ind-1; i >= ifrom; i-- ) {
//	if ( ( pr2c = get_RawhitCluster(i) ) ) {
//	  if ( pr2c->get_clusterID() == id ) {
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
//    SvxRawhitCluster* pr2c = NULL;
//    for ( int i = ifrom; i < indt; i++ ) {
//      if ( ( pr2c = get_RawhitCluster(i) ) ) {
//	if ( pr2c->get_clusterID() == id ) return i;
//      }
//    }
//    return -1;
//
//  } // if (indt-ifrom > 8 && clusterSorted)
//}

bool SvxRawhitClusterListv1::indexOfRawhit(
   const int id, int& idx_lb, int& idx_ub, int ifrom, int iupto) const
{
   if (! rawhitSorted) {
      std::cerr << "SvxRawhitClusterListv1::indexOfRawhit(): The hit list hasn't been sorted by RawhitID." << std::endl;
      exit(1);
   }
   idx_lb = -1;
   idx_ub = -2;
   int n_ent = get_nRawhitClusters();
   if (n_ent == 0) return false;
   if (iupto < 0) iupto = n_ent - 1;
   if (ifrom < 0 || iupto >= n_ent || ifrom > iupto) {
      std::cerr << "SvxRawhitClusterListv1::indexOfRawhit(): Invalid search range, from "
           << ifrom << " upto " << iupto << ", where the hit list has " 
           << n_ent << " entries." << std::endl;
      exit(1);
   }

   int i_low = ifrom - 1;
   int i_up  = iupto + 1;
   int i_mid = -1; // index of a first matched entry
   while (i_up - i_low > 1) {
      int i_cent  = (i_low + i_up) / 2;
      int id_cent = get_RawhitCluster(i_cent)->get_rawhitID();
      if      (id == id_cent) { i_mid = i_cent;  break; }
      else if (id >  id_cent) { i_low = i_cent; }
      else                    { i_up  = i_cent; }
   }
   if (i_mid < 0) return false;

   idx_lb = idx_ub = i_mid;
   for (; idx_lb > i_low + 1 && get_RawhitCluster(idx_lb - 1)->get_rawhitID() == id; idx_lb--);
   for (; idx_ub < i_up  - 1 && get_RawhitCluster(idx_ub + 1)->get_rawhitID() == id; idx_ub++);
   return true;
}

bool SvxRawhitClusterListv1::indexOfCluster(
   const int id, int& idx_lb, int& idx_ub, int ifrom, int iupto) const
{
   if (! clusterSorted) {
      std::cerr << "SvxRawhitClusterListv1::indexOfCluster(): The hit list hasn't been sorted by ClusterID." << std::endl;
      exit(1);
   }
   idx_lb = -1;
   idx_ub = -2;
   int n_ent = get_nRawhitClusters();
   if (n_ent == 0) return false;
   if (iupto < 0) iupto = n_ent - 1;
   if (ifrom < 0 || iupto >= n_ent || ifrom > iupto) {
      std::cerr << "SvxRawhitClusterListv1::indexOfCluster(): Invalid search range, from "
           << ifrom << " upto " << iupto << ", where the hit list has " 
           << n_ent << " entries." << std::endl;
      exit(1);
   }

   int i_low = ifrom - 1;
   int i_up  = iupto + 1;
   int i_mid = -1; // index of a first matched entry
   while (i_up - i_low > 1) {
      int i_cent  = (i_low + i_up) / 2;
      int id_cent = get_RawhitCluster(i_cent)->get_clusterID();
      if      (id == id_cent) { i_mid = i_cent;  break; }
      else if (id >  id_cent) { i_low = i_cent; }
      else                    { i_up  = i_cent; }
   }
   if (i_mid < 0) return false;

   idx_lb = idx_ub = i_mid;
   for (; idx_lb > i_low + 1 && get_RawhitCluster(idx_lb - 1)->get_clusterID() == id; idx_lb--);
   for (; idx_ub < i_up  - 1 && get_RawhitCluster(idx_ub + 1)->get_clusterID() == id; idx_ub++);
   return true;
}


// Methods
// """""""
void SvxRawhitClusterListv1::print() const
{
  std::cout << "SvxRawhitClusterListv1: nRawhitClusters = " << nRawhitClusters
       << " rawhitSorted = "  << rawhitSorted
       << " clusterSorted = " << clusterSorted << std::endl;
  SvxRawhitCluster* rawhitcluster = NULL;
  for (unsigned int i=0; i < nRawhitClusters; i++) {
    std::cout << " SvxRawhitCluster number " << i << ": ";
    if( (rawhitcluster = get_RawhitCluster(i)) ) {
      rawhitcluster->print();
    } else {
      std::cout << "missing ..." << std::endl;
    }
  }
}
