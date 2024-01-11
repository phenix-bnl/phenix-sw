// ============================
// FILE: SvxGhitClusterListv1.C
// ============================
// ****************************************************************************
// Implementation of the container SvxGhitClusterListv1
// ---
// Created  by V. L. Rykov on 15-Feb-2003
//
// Modified by V. L. Rykov on 11-May-2004:
//     Sorting and index search methods added.
// ****************************************************************************

#include <SvxGhitClusterListv1.h>

#include <algorithm>
#include <cstdlib>
#include <vector>

ClassImp(SvxGhitClusterListv1)

using namespace std;

// Constructor(s) & destructor
// """""""""""""""""""""""""""
SvxGhitClusterListv1::SvxGhitClusterListv1(const unsigned int length)
{
  GhitCluster   = new TClonesArray("SvxGhitClusterv1", length);
  nGhitClusters = GhitCluster->GetLast() + 1;
  unSort();
  listPointersSet = false;
  //std::cout << "SvxGhitClusterListv1 object created" << std::endl;
}

SvxGhitClusterListv1::~SvxGhitClusterListv1()
{
  delete GhitCluster;
}

// The "standard PHObject response" functions...
// """""""""""""""""""""""""""""""""""""""""""""
void SvxGhitClusterListv1::Reset()
{
  GhitCluster->Clear();
  if ( GhitCluster->GetSize() > SVXNGHITCLUSTER )
    GhitCluster->Expand(SVXNGHITCLUSTER);
  nGhitClusters = GhitCluster->GetLast() + 1 ;
  unSort();
  listPointersSet = false;
}

int SvxGhitClusterListv1::isValid() const
{
  return ( ( nGhitClusters > 0 ) ? 1 : 0 ) ;
}

void SvxGhitClusterListv1::identify(std::ostream &os) const
{
  os << "Identify yourself: SvxGhitClusterListv1 object: nGhitClusters = "
     << nGhitClusters << std::endl;
}

// Add/remove/set/get methods...
// """""""""""""""""""""""""""""
SvxGhitCluster* SvxGhitClusterListv1::addGhitCluster(const int ihit)
{
  int       index = ( ihit < 0 ) ? nGhitClusters : ihit ;
  TClonesArray &P = *GhitCluster;
  SvxGhitCluster* ghitcluster = new(P[index]) SvxGhitClusterv1(this);
  if ( nGhitClusters == 0 ) listPointersSet = true;
  nGhitClusters = GhitCluster->GetLast() + 1;
  unSort();
  return ghitcluster;
}

void SvxGhitClusterListv1::removeGhitCluster (const unsigned int ihit)
{
  GhitCluster->RemoveAt(ihit) ;
  unSort();
}

// Routines to manipulate the cluster array...
// """""""""""""""""""""""""""""""""""""""""""
int SvxGhitClusterListv1::Compress()
{
  GhitCluster->Compress();
  return (int) (nGhitClusters = GhitCluster->GetLast() + 1);
}

int SvxGhitClusterListv1::set_TClonesArraySize (const unsigned int nhit)
{
  if ( nhit > nGhitClusters ) GhitCluster->Expand(nhit);
  return GhitCluster->GetSize();
}

// Sorting and index search
// """"""""""""""""""""""""
void SvxGhitClusterListv1::restoreListPointers()
{
  SvxGhitClusterv1* g2c = NULL;
  for (unsigned int i = 0; i < nGhitClusters; i++ ) {
    if ( ( g2c = (SvxGhitClusterv1*) GhitCluster->UncheckedAt(i) ) )
      g2c->set_ListPointer(this);
  }
  listPointersSet = true;
}

void SvxGhitClusterListv1::sortGhits() {
  if ( ghitSorted ) return;
  if ( !listPointersSet ) restoreListPointers();
  SvxGhitClusterv1 g2c;
  g2c.set_sortingSwitch(-1);
  clusterSorted = false;
//  GhitCluster->UnSort();
  SortSTL();
//  GhitCluster->Sort(nGhitClusters);
  ghitSorted = true;
}

void SvxGhitClusterListv1::sortClusters() {
  if ( clusterSorted ) return;
  if ( !listPointersSet ) restoreListPointers();
  SvxGhitClusterv1 g2c;
  g2c.set_sortingSwitch(1);
  ghitSorted = false;
//  GhitCluster->UnSort();
  SortSTL();
//  GhitCluster->Sort(nGhitClusters);
  clusterSorted = true;
}

void SvxGhitClusterListv1::unSort() {
  ghitSorted    = false;
  clusterSorted = false;
  GhitCluster->UnSort();
}

void SvxGhitClusterListv1::SortSTL()
{
   int n_ent = get_nGhitClusters();

   // make and sort an temporal array
   vector<SvxGhitCluster*> hlist(n_ent);
   for (int i = 0; i < n_ent; i++) {
      hlist[i] = get_GhitCluster(i)->Clone();
   }

   sort(hlist.begin(), hlist.end(), SvxGhitCluster::CompPointer);
   // copy parameters and delete the item of the temporal array
   for (int i = 0; i < n_ent; i++) {
      get_GhitCluster(i)->Copy(hlist[i]);
      delete hlist[i];
   }
}

//// Find first occurrence of ghit
//int SvxGhitClusterListv1::indexOfGhit (const int id   ,
//				       const int ifrom,
//				       const int iupto) const {
//
//  int indt = ( iupto < 0 ) ? nGhitClusters : iupto;
//
//  if ( indt-ifrom > 8 && ghitSorted ) {
//
//    SvxGhitClusterv1 g2c;
//    g2c.set_ghitID(id);
//    g2c.set_sortingSwitch(-1);
//    int ind = GhitCluster->BinarySearch(&g2c, indt);
//    if ( (ind == ifrom) || ind < 0 ) return ind;
//    SvxGhitCluster* pg2c = NULL;
//    if ( ind < ifrom ) {
//      for ( int i = ifrom; i < indt; i++ ) {
//	if ( ( pg2c = get_GhitCluster(i) ) ) {
//	  return ( pg2c->get_ghitID() == id ) ? i : -1;
//	}
//      }
//      return -1;
//    } else {
//      int ilast = ind;
//      for ( int i = ind-1; i >= ifrom; i-- ) {
//	if ( ( pg2c = get_GhitCluster(i) ) ) {
//	  if ( pg2c->get_ghitID() == id ) {
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
//    SvxGhitCluster* pg2c = NULL;
//    for ( int i = ifrom; i < indt; i++ ) {
//      if ( ( pg2c = get_GhitCluster(i) ) ) {
//	if ( pg2c->get_ghitID() == id ) return i;
//      }
//    }
//    return -1;
//
//  } // if (indt-ifrom > 8 && ghitSorted)
//}
//
//// Find first occurrence of cluster
//int SvxGhitClusterListv1::indexOfCluster (const int id   ,
//					  const int ifrom,
//					  const int iupto) const {
//
//  int indt = ( iupto < 0 ) ? nGhitClusters : iupto;
//
//  if ( indt-ifrom > 8 && clusterSorted ) {
//
//    SvxGhitClusterv1 g2c;
//    g2c.set_clusterID(id);
//    g2c.set_sortingSwitch(1);
//    int ind = GhitCluster->BinarySearch(&g2c, indt);
//    if ( (ind == ifrom) || ind < 0 ) return ind;
//    SvxGhitCluster* pg2c = NULL;
//    if ( ind < ifrom ) {
//      for ( int i = ifrom; i < indt; i++ ) {
//	if ( ( pg2c = get_GhitCluster(i) ) ) {
//	  return ( pg2c->get_clusterID() == id ) ? i : -1;
//	}
//      }
//      return -1;
//    } else {
//      int ilast = ind;
//      for ( int i = ind-1; i >= ifrom; i-- ) {
//	if ( ( pg2c = get_GhitCluster(i) ) ) {
//	  if ( pg2c->get_clusterID() == id ) {
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
//    SvxGhitCluster* pg2c = NULL;
//    for ( int i = ifrom; i < indt; i++ ) {
//      if ( ( pg2c = get_GhitCluster(i) ) ) {
//	if ( pg2c->get_clusterID() == id ) return i;
//      }
//    }
//    return -1;
//
//  } // if (indt-ifrom > 8 && clusterSorted)
//}

bool SvxGhitClusterListv1::indexOfGhit(
   const int id, int& idx_lb, int& idx_ub, int ifrom, int iupto) const
{
   if (! ghitSorted) {
      std::cerr << "SvxGhitClusterListv1::indexOfGhit(): The hit list hasn't been sorted by GhitID." << std::endl;
      exit(1);
   }
   idx_lb = -1;
   idx_ub = -2;
   int n_ent = get_nGhitClusters();
   if (n_ent == 0) return false;
   if (iupto < 0) iupto = n_ent - 1;
   if (ifrom < 0 || iupto >= n_ent || ifrom > iupto) {
      std::cerr << "SvxGhitClusterListv1::indexOfGhit(): Invalid search range, from "
           << ifrom << " upto " << iupto << ", where the hit list has " 
           << n_ent << " entries." << std::endl;
      exit(1);
   }

   int i_low = ifrom - 1;
   int i_up  = iupto + 1;
   int i_mid = -1; // index of a first matched entry
   while (i_up - i_low > 1) {
      int i_cent  = (i_low + i_up) / 2;
      int id_cent = get_GhitCluster(i_cent)->get_ghitID();
      if      (id == id_cent) { i_mid = i_cent;  break; }
      else if (id >  id_cent) { i_low = i_cent; }
      else                    { i_up  = i_cent; }
   }
   if (i_mid < 0) return false;

   idx_lb = idx_ub = i_mid;
   for (; idx_lb > i_low + 1 && get_GhitCluster(idx_lb - 1)->get_ghitID() == id; idx_lb--);
   for (; idx_ub < i_up  - 1 && get_GhitCluster(idx_ub + 1)->get_ghitID() == id; idx_ub++);
   return true;
}

bool SvxGhitClusterListv1::indexOfCluster(
   const int id, int& idx_lb, int& idx_ub, int ifrom, int iupto) const
{
   if (! clusterSorted) {
      std::cerr << "SvxGhitClusterListv1::indexOfCluster(): The hit list hasn't been sorted by ClusterID." << std::endl;
      exit(1);
   }
   idx_lb = -1;
   idx_ub = -2;
   int n_ent = get_nGhitClusters();
   if (n_ent == 0) return false;
   if (iupto < 0) iupto = n_ent - 1;
   if (ifrom < 0 || iupto >= n_ent || ifrom > iupto) {
      std::cerr << "SvxGhitClusterListv1::indexOfCluster(): Invalid search range, from "
           << ifrom << " upto " << iupto << ", where the hit list has " 
           << n_ent << " entries." << std::endl;
      exit(1);
   }

   int i_low = ifrom - 1;
   int i_up  = iupto + 1;
   int i_mid = -1; // index of a first matched entry
   while (i_up - i_low > 1) {
      int i_cent  = (i_low + i_up) / 2;
      int id_cent = get_GhitCluster(i_cent)->get_clusterID();
      if      (id == id_cent) { i_mid = i_cent;  break; }
      else if (id >  id_cent) { i_low = i_cent; }
      else                    { i_up  = i_cent; }
   }
   if (i_mid < 0) return false;

   idx_lb = idx_ub = i_mid;
   for (; idx_lb > i_low + 1 && get_GhitCluster(idx_lb - 1)->get_clusterID() == id; idx_lb--);
   for (; idx_ub < i_up  - 1 && get_GhitCluster(idx_ub + 1)->get_clusterID() == id; idx_ub++);
   return true;
}

// Methods
// """""""
void SvxGhitClusterListv1::print() const
{
  std::cout << "SvxGhitClusterListv1: nGhitClusters = " << nGhitClusters
       << " ghitSorted = "    << ghitSorted
       << " clusterSorted = " << clusterSorted << std::endl;
  SvxGhitCluster* ghitcluster = NULL;
  for (unsigned int i=0; i < nGhitClusters; i++) {
    std::cout << " SvxGhitCluster number " << i << ": ";
    if( (ghitcluster = get_GhitCluster(i)) ) {
      ghitcluster->print();
    } else {
      std::cout << "missing ..." << std::endl;
    }
  }
}
