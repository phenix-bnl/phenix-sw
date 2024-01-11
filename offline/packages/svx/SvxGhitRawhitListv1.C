// ===========================
// FILE: SvxGhitRawhitListv1.C
// ===========================
// ****************************************************************************
// Implementation of the container SvxGhitRawhitListv1 for the SvxGhitRawhitv1
// ---
// Created  by V. L. Rykov on 14-Feb-2003
//
// Modified by V. L. Rykov on 11-May-2004:
//     Sorting and index search methods added.
// ****************************************************************************

#include <SvxGhitRawhitListv1.h>

#include <algorithm>
#include <cstdlib>
#include <vector>

ClassImp(SvxGhitRawhitListv1)

using namespace std;

// Constructor(s) & destructor
// """""""""""""""""""""""""""
SvxGhitRawhitListv1::SvxGhitRawhitListv1(const unsigned int length)
{
  GhitRawhit   = new TClonesArray("SvxGhitRawhitv1", length);
  nGhitRawhits = GhitRawhit->GetLast() + 1;
  unSort();
  listPointersSet = false;
  //std::cout << "SvxGhitRawhitListv1 object created" << std::endl;
}

SvxGhitRawhitListv1::~SvxGhitRawhitListv1()
{
  delete GhitRawhit;
}

// The "standard PHObject response" functions...
// """""""""""""""""""""""""""""""""""""""""""""
void SvxGhitRawhitListv1::Reset()
{
  GhitRawhit->Clear();
  if ( GhitRawhit->GetSize() > SVXNGHITRAWHIT )
    GhitRawhit->Expand(SVXNGHITRAWHIT);
  nGhitRawhits = GhitRawhit->GetLast() + 1 ;
  unSort();
  listPointersSet = false;
}

int SvxGhitRawhitListv1::isValid() const
{
  return ( ( nGhitRawhits > 0 ) ? 1 : 0 ) ;
}

void SvxGhitRawhitListv1::identify(std::ostream &os) const
{
  os << "Identify yourself: SvxGhitRawhitListv1 object: nGhitRawhits = "
     << nGhitRawhits << std::endl;
}

// Add/remove/set/get methods...
// """""""""""""""""""""""""""""
SvxGhitRawhit* SvxGhitRawhitListv1::addGhitRawhit(const int ihit)
{
  int       index = ( ihit < 0 ) ? nGhitRawhits : ihit ;
  TClonesArray &P = *GhitRawhit;
  SvxGhitRawhit* ghitrawhit = new(P[index]) SvxGhitRawhitv1(this);
  if ( nGhitRawhits == 0 ) listPointersSet = true;
  nGhitRawhits = GhitRawhit->GetLast() + 1;
  unSort();
  return ghitrawhit;
}

void SvxGhitRawhitListv1::removeGhitRawhit (const unsigned int ihit)
{
  GhitRawhit->RemoveAt(ihit);
  unSort();
}

// Routines to manipulate the rawhit array...
// """""""""""""""""""""""""""""""""""""""""""
int SvxGhitRawhitListv1::Compress()
{
  GhitRawhit->Compress();
  return (int) (nGhitRawhits = GhitRawhit->GetLast() + 1);
}

int SvxGhitRawhitListv1::set_TClonesArraySize (const unsigned int nhit)
{
  if ( nhit > nGhitRawhits ) GhitRawhit->Expand(nhit);
  return GhitRawhit->GetSize();
}

// Sorting and index search
// """"""""""""""""""""""""
void SvxGhitRawhitListv1::restoreListPointers()
{
  SvxGhitRawhitv1* g2r = NULL;
  for (unsigned int i = 0; i < nGhitRawhits; i++ ) {
    if ( ( g2r = (SvxGhitRawhitv1*) GhitRawhit->UncheckedAt(i) ) )
      g2r->set_ListPointer(this);
  }
  listPointersSet = true;
}

void SvxGhitRawhitListv1::sortGhits() {
  if ( ghitSorted ) return;
  if ( !listPointersSet ) restoreListPointers();
  SvxGhitRawhitv1 g2r;
  g2r.set_sortingSwitch(-1);
  rawhitSorted = false;
//  GhitRawhit->UnSort();
  SortSTL();
//  GhitRawhit->Sort(nGhitRawhits);
  ghitSorted   = true;
}

void SvxGhitRawhitListv1::sortRawhits() {
  if ( rawhitSorted ) return;
  if ( !listPointersSet ) restoreListPointers();
  SvxGhitRawhitv1 g2r;
  g2r.set_sortingSwitch(1);
  ghitSorted   = false;
//  GhitRawhit->UnSort();
  SortSTL();
//  GhitRawhit->Sort(nGhitRawhits);
  rawhitSorted = true;
}

void SvxGhitRawhitListv1::unSort() {
  ghitSorted   = false;
  rawhitSorted = false;
  GhitRawhit->UnSort();
}

void SvxGhitRawhitListv1::SortSTL()
{
   int n_ent = get_nGhitRawhits();

   // make and sort an temporal array
   vector<SvxGhitRawhit*> hlist(n_ent);
   for (int i = 0; i < n_ent; i++) {
      hlist[i] = get_GhitRawhit(i)->Clone();
   }

   sort(hlist.begin(), hlist.end(), SvxGhitRawhit::CompPointer);
   // copy parameters and delete the item of the temporal array
   for (int i = 0; i < n_ent; i++) {
      get_GhitRawhit(i)->Copy(hlist[i]);
      delete hlist[i];
   }
}

// Find first occurrence of ghit
//int SvxGhitRawhitListv1::indexOfGhit (const int id   ,
//				      const int ifrom,
//				      const int iupto) const {
//
//  int indt = ( iupto < 0 ) ? nGhitRawhits : iupto;
//
//  if ( indt-ifrom > 8 && ghitSorted ) {
//
//    SvxGhitRawhitv1 g2r;
//    g2r.set_ghitID(id);
//    g2r.set_sortingSwitch(-1);
//    int ind = GhitRawhit->BinarySearch(&g2r, indt);
//    if ( (ind == ifrom) || ind < 0 ) return ind;
//    SvxGhitRawhit* pg2r = NULL;
//    if ( ind < ifrom ) {
//      for ( int i = ifrom; i < indt; i++ ) {
//	if ( ( pg2r = get_GhitRawhit(i) ) ) {
//	  return ( pg2r->get_ghitID() == id ) ? i : -1;
//	}
//      }
//      return -1;
//    } else {
//      int ilast = ind;
//      for ( int i = ind-1; i >= ifrom; i-- ) {
//	if ( ( pg2r = get_GhitRawhit(i) ) ) {
//	  if ( pg2r->get_ghitID() == id ) {
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
//    SvxGhitRawhit* pg2r = NULL;
//    for ( int i = ifrom; i < indt; i++ ) {
//      if ( ( pg2r = get_GhitRawhit(i) ) ) {
//	if ( pg2r->get_ghitID() == id ) return i;
//      }
//    }
//    return -1;
//
//  } // if (indt-ifrom > 8 && ghitSorted)
//}

// Find first occurrence of rawhit
//int SvxGhitRawhitListv1::indexOfRawhit (const int id   ,
//					const int ifrom,
//					const int iupto) const {
//
//  int indt = ( iupto < 0 ) ? nGhitRawhits : iupto;
//
//  if ( indt-ifrom > 8 && rawhitSorted ) {
//
//    SvxGhitRawhitv1 g2r;
//    g2r.set_rawhitID(id);
//    g2r.set_sortingSwitch(1);
//    int ind = GhitRawhit->BinarySearch(&g2r, indt);
//    if ( (ind == ifrom) || ind < 0 ) return ind;
//    SvxGhitRawhit* pg2r = NULL;
//    if ( ind < ifrom ) {
//      for ( int i = ifrom; i < indt; i++ ) {
//	if ( ( pg2r = get_GhitRawhit(i) ) ) {
//	  return ( pg2r->get_rawhitID() == id ) ? i : -1;
//	}
//      }
//      return -1;
//    } else {
//      int ilast = ind;
//      for ( int i = ind-1; i >= ifrom; i-- ) {
//	if ( ( pg2r = get_GhitRawhit(i) ) ) {
//	  if ( pg2r->get_rawhitID() == id ) {
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
//    SvxGhitRawhit* pg2r = NULL;
//    for ( int i = ifrom; i < indt; i++ ) {
//      if ( ( pg2r = get_GhitRawhit(i) ) ) {
//	if ( pg2r->get_rawhitID() == id ) return i;
//      }
//    }
//    return -1;
//
//  } // if (indt-ifrom > 8 && rawhitSorted)
//}

bool SvxGhitRawhitListv1::indexOfGhit(
   const int id, int& idx_lb, int& idx_ub, int ifrom, int iupto) const
{
   if (! ghitSorted) {
      std::cerr << "SvxGhitRawhitListv1::indexOfGhit(): The hit list hasn't been sorted by GhitID." << std::endl;
      exit(1);
   }
   idx_lb = -1;
   idx_ub = -2;
   int n_ent = get_nGhitRawhits();
   if (n_ent == 0) return false;
   if (iupto < 0) iupto = n_ent - 1;
   if (ifrom < 0 || iupto >= n_ent || ifrom > iupto) {
      std::cerr << "SvxGhitRawhitListv1::indexOfGhit(): Invalid search range, from "
           << ifrom << " upto " << iupto << ", where the hit list has " 
           << n_ent << " entries." << std::endl;
      exit(1);
   }

   int i_low = ifrom - 1;
   int i_up  = iupto + 1;
   int i_mid = -1; // index of a first matched entry
   while (i_up - i_low > 1) {
      int i_cent  = (i_low + i_up) / 2;
      int id_cent = get_GhitRawhit(i_cent)->get_ghitID();
      if      (id == id_cent) { i_mid = i_cent;  break; }
      else if (id >  id_cent) { i_low = i_cent; }
      else                    { i_up  = i_cent; }
   }
   if (i_mid < 0) return false;

   idx_lb = idx_ub = i_mid;
   for (; idx_lb > i_low + 1 && get_GhitRawhit(idx_lb - 1)->get_ghitID() == id; idx_lb--);
   for (; idx_ub < i_up  - 1 && get_GhitRawhit(idx_ub + 1)->get_ghitID() == id; idx_ub++);
   return true;
}

bool SvxGhitRawhitListv1::indexOfRawhit(
   const int id, int& idx_lb, int& idx_ub, int ifrom, int iupto) const
{
   if (! rawhitSorted) {
      std::cerr << "SvxGhitRawhitListv1::indexOfRawhit(): The hit list hasn't been sorted by rawhitID." << std::endl;
      exit(1);
   }
   idx_lb = -1;
   idx_ub = -2;
   int n_ent = get_nGhitRawhits();
   if (n_ent == 0) return false;
   if (iupto < 0) iupto = n_ent - 1;
   if (ifrom < 0 || iupto >= n_ent || ifrom > iupto) {
      std::cerr << "SvxGhitRawhitListv1::indexOfRawhit(): Invalid search range, from "
           << ifrom << " upto " << iupto << ", where the hit list has " 
           << n_ent << " entries." << std::endl;
      exit(1);
   }

   int i_low = ifrom - 1;
   int i_up  = iupto + 1;
   int i_mid = -1; // index of a first matched entry
   while (i_up - i_low > 1) {
      int i_cent  = (i_low + i_up) / 2;
      int id_cent = get_GhitRawhit(i_cent)->get_rawhitID();
      if      (id == id_cent) { i_mid = i_cent;  break; }
      else if (id >  id_cent) { i_low = i_cent; }
      else                    { i_up  = i_cent; }
   }
   if (i_mid < 0) return false;

   idx_lb = idx_ub = i_mid;
   for (; idx_lb > i_low + 1 && get_GhitRawhit(idx_lb - 1)->get_rawhitID() == id; idx_lb--);
   for (; idx_ub < i_up  - 1 && get_GhitRawhit(idx_ub + 1)->get_rawhitID() == id; idx_ub++);
   return true;
}

// Methods
// """""""
void SvxGhitRawhitListv1::print() const
{
  std::cout << "SvxGhitRawhitListv1: nGhitRawhits = " << nGhitRawhits
       << " ghitSorted = "   << ghitSorted
       << " rawhitSorted = " << rawhitSorted << std::endl;
  SvxGhitRawhit* ghitrawhit = NULL;
  for (unsigned int i=0; i < nGhitRawhits; i++) {
    std::cout << " SvxGhitRawhit number " << i << ": ";
    if( (ghitrawhit = get_GhitRawhit(i)) ) {
      ghitrawhit->print();
    } else {
      std::cout << "missing ..." << std::endl;
    }
  }
}
