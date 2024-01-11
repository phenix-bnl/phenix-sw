// =====================
// FILE: SvxGhitListv1.C
// =====================
// ****************************************************************************
// Implementation of the container SvxGhitListv1 for the SvxGhitv1
// Template used: SvxGhitListv1.C by Jeffery Mitchell as of 11/20/2003
// ---
// Created by V. L. Rykov on 09-Mar-2003
//
// Modified by V. L. Rykov on 12-May-2004:
//     Sorting and index search methods added.
// ****************************************************************************

#include <SvxGhitListv1.h>

#include <algorithm>
#include <cstdlib>
#include <vector>

ClassImp(SvxGhitListv1)

using namespace std;

// Constructor(s) & destructor
// """""""""""""""""""""""""""
SvxGhitListv1::SvxGhitListv1(const unsigned int length)
{
  m_hit_list   = new TClonesArray("SvxGhitv1", length);
  m_hit_id_unused = 0;
  unSort();
  listPointersSet = false;
  //std::cout << "SvxGhitListv1 object created" << std::endl;
}

SvxGhitListv1::~SvxGhitListv1()
{
  delete m_hit_list;
}

// The "standard PHObject response" functions...
// """""""""""""""""""""""""""""""""""""""""""""
void SvxGhitListv1::Reset()
{
  m_hit_id_unused = 0;
  m_hit_list->Clear();
  if ( m_hit_list->GetSize() > SVXNGHIT ) m_hit_list->Expand(SVXNGHIT);
  unSort();
  listPointersSet = false;
}

int SvxGhitListv1::isValid() const
{
  return ( ( get_nGhits() > 0 ) ? 1 : 0 ) ;
}

void SvxGhitListv1::identify(std::ostream &os) const
{
  os << "Identify yourself: SvxGhitListv1 object: nGhits = "
     << get_nGhits() << std::endl;
}

// Add/remove/set/get methods...
// """""""""""""""""""""""""""""
SvxGhit* SvxGhitListv1::addGhit(const int ihit)
{
  int       index = ( ihit < 0 ) ? get_nGhits() : ihit ;
  TClonesArray &P = *m_hit_list;
  SvxGhit*   ghit = new(P[index]) SvxGhitv1(this);
  ghit->set_hitID(m_hit_id_unused++);
  if ( get_nGhits() == 0 ) listPointersSet = true;
  unSort();
  return ghit;
}

void SvxGhitListv1::removeGhit (const unsigned int ihit)
{
  m_hit_list->RemoveAt(ihit);
  unSort();
}

// Routines to manipulate the cluster array...
// """""""""""""""""""""""""""""""""""""""""""
int SvxGhitListv1::Compress()
{
  m_hit_list->Compress();
  return get_nGhits();
}

int SvxGhitListv1::set_TClonesArraySize (const unsigned int nhit)
{
  if ( nhit > (unsigned int)get_nGhits() ) m_hit_list->Expand(nhit);
  return m_hit_list->GetSize();
}

// Sorting and index search
// """"""""""""""""""""""""
void SvxGhitListv1::restoreListPointers ()
{
  SvxGhitv1* ghit = NULL;
  for (int i = 0; i < get_nGhits(); i++ ) {
    if ( ( ghit = (SvxGhitv1*) m_hit_list->UncheckedAt(i) ) )
      ghit->set_ListPointer(this);
  }
  listPointersSet = true;
}

void SvxGhitListv1::sort_hitID () {
  if ( hitIDsorted      ) return;
  if ( !listPointersSet ) restoreListPointers();
  SvxGhitv1 hit;
  hit.set_sortingSwitch(-1);
  sensorIDsorted = false;
//  m_hit_list->UnSort();
  SortSTL();
//  m_hit_list->Sort(get_nGhits());
  hitIDsorted   = true;
}

void SvxGhitListv1::sort_sensorID () {
  if ( sensorIDsorted   ) return;
  if ( !listPointersSet ) restoreListPointers();
  SvxGhitv1 hit;
  hit.set_sortingSwitch(1);
  hitIDsorted = false;
//  m_hit_list->UnSort();
  SortSTL();
//  m_hit_list->Sort(get_nGhits());
  sensorIDsorted   = true;
}

void SvxGhitListv1::unSort() {
  hitIDsorted    = false;
  sensorIDsorted = false;
  m_hit_list->UnSort();
}

void SvxGhitListv1::SortSTL()
{
   int n_ent = get_nGhits();

   // make and sort an temporal array
   vector<SvxHit*> hlist(n_ent);
   for (int i = 0; i < n_ent; i++) {
      hlist[i] = get_Ghit(i)->Clone();
   }

   sort(hlist.begin(), hlist.end(), SvxHit::CompPointer);
   // copy parameters and delete the item of the temporal array
   for (int i = 0; i < n_ent; i++) {
      get_Ghit(i)->Copy(hlist[i]);
      delete hlist[i];
   }
}

// Find first occurrence of ghit
int SvxGhitListv1::indexOfGhit (
   const int hitid, int ifrom, int iupto) const
{
  if (get_nGhits() == 0) return -1;

  if (iupto < 0) iupto = get_nGhits() - 1;
  if (ifrom < 0 || iupto >= get_nGhits() || ifrom > iupto) {
     std::cerr << "SvxGhitListv1::indexOfGhit(): Invalid search range, from "
               << ifrom << " upto " << iupto << ", where the hit list has " 
               << get_nGhits() << " entries." << std::endl;
     exit(1);
  }

  if ( iupto-ifrom > 8 && hitIDsorted ) {
     int i_low = ifrom - 1;
     int i_up  = iupto + 1;
     while (i_up - i_low > 1) {
        int i_cent     = (i_low + i_up) / 2;
        int hitid_curr = get_Ghit(i_cent)->get_hitID();
        if      (hitid_curr == hitid) { return i_cent; }
        else if (hitid_curr <  hitid) { i_low = i_cent; }
        else                          { i_up = i_cent; }
     }
     return -1;

  } else {

    SvxGhit* phit = NULL;
    for ( int i = ifrom; i <= iupto; i++ ) {
      if ( ( phit = get_Ghit(i) ) ) {
	if ( phit->get_hitID() == hitid ) return i;
      }
    }
    return -1;

  } // if (indto-ifrom > 8 && hitIDsorted)
}

//int SvxGhitListv1::indexOfGhit (const SvxSensor* sensor,
//				const int         ifrom,
//				const int         iupto) const {
//
//  int indto = ( iupto < 0 ) ? get_nGhits() : iupto;
//
//  if ( indto-ifrom > 8 && sensorIDsorted ) {
//
//    SvxGhitv1 hit;
//    hit.set_svxSection(sensor->get_svxSection());
//    hit.set_layer(sensor->get_layer());
//    hit.set_ladder(sensor->get_ladder());
//    hit.set_sensor(sensor->get_sensor());
//    hit.set_sortingSwitch(1);
//    int ind = m_hit_list->BinarySearch(&hit, indto);
//    if ( (ind == ifrom ) || ind < 0 ) return ind;
//    SvxGhit* phit = NULL;
//    if ( ind < ifrom ) {
//      for ( int i = ifrom; i < indto; i++ ) {
//	if ( ( phit = get_Ghit(i) ) ) {
//	  return ( sensor->checkID(phit) ) ? i : -1;
//	}
//      }
//      return -1;
//    } else {
//      int ilast = ind;
//      for ( int i = ind-1; i >= ifrom; i-- ) {
//	if ( ( phit = get_Ghit(i) ) ) {
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
//    SvxGhit* phit = NULL;
//    for ( int i = ifrom; i < indto; i++ ) {
//      if ( ( phit = get_Ghit(i) ) ) {
//	if ( sensor->checkID(phit) ) return i;
//      }
//    }
//    return -1;
//
//  } // if (indto-ifrom > 8 && hitIDsorted)
//}

int SvxGhitListv1::indexOfGhit (const SvxGhit* hit  ,
				const int      ifrom,
				const int      iupto) const {
  int indto = ( iupto < 0 ) ? get_nGhits() : iupto ;
  for ( int i = ifrom; i < indto; i++ ) {
    if ( get_Ghit(i) == hit ) return i;
  }
  return -1;
}

bool SvxGhitListv1::indexOfGhit(const SvxSensor* sensor, 
                                int& idx_lb, int& idx_ub, 
                                int ifrom, int iupto) const
{
   if (! sensorIDsorted) {
      std::cerr << "SvxGhitListv1::indexOfGhit(): The hit list hasn't been sorted by sensorID." << std::endl;
      exit(1);
   }
   idx_lb = -1;
   idx_ub = -2;
   if (get_nGhits() == 0) return false;
   if (iupto < 0) iupto = get_nGhits() - 1;
   if (ifrom < 0 || iupto >= get_nGhits() || ifrom > iupto) {
      std::cerr << "SvxGhitListv1::indexOfGhit(): Invalid search range, from "
           << ifrom << " upto " << iupto << ", where the hit list has " 
           << get_nGhits() << " entries." << std::endl;
      exit(1);
   }

   SvxGhitv1* hit_cmp = new SvxGhitv1();
   hit_cmp->set_svxSection(sensor->get_svxSection());
   hit_cmp->set_layer     (sensor->get_layer());
   hit_cmp->set_ladder    (sensor->get_ladder());
   hit_cmp->set_sensor    (sensor->get_sensor());
   hit_cmp->set_sortingSwitch(1); // sort by sensor ID

   int i_low = ifrom - 1;
   int i_up  = iupto + 1;
   int i_mid = -1; // index of a first matched entry
   while (i_up - i_low > 1) {
      int i_cent     = (i_low + i_up) / 2;
      int cmp = hit_cmp->Compare(get_Ghit(i_cent));
      if      (cmp == 0) { i_mid = i_cent;  break; }
      else if (cmp >  0) { i_low = i_cent; }
      else               { i_up  = i_cent; }
   }
   if (i_mid < 0) {
      delete hit_cmp;
      return false;
   }

   idx_lb = idx_ub = i_mid;
   for (; idx_lb > i_low + 1 && hit_cmp->Compare(get_Ghit(idx_lb - 1)) == 0; idx_lb--);
   for (; idx_ub < i_up  - 1 && hit_cmp->Compare(get_Ghit(idx_ub + 1)) == 0; idx_ub++);

   delete hit_cmp;
   return true;
}

// Methods
// """""""
void SvxGhitListv1::print() const
{
   std::cout << "SvxGhitListv1: nGhits = " << get_nGhits()
       << " hitIDsorted = "    << hitIDsorted
       << " sensorIDsorted = " << sensorIDsorted << std::endl;
  SvxGhit* ghit = NULL;
  for (int i=0; i < get_nGhits(); i++) {
    std::cout << "SvxGhit number " << i << ":";
    if( (ghit = get_Ghit(i)) ) {
      std::cout << std::endl;
      ghit->print();
    } else {
      std::cout << " missing ..." << std::endl;
    }
  }
}
