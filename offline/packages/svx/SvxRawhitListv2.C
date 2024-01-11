// =======================
// FILE: SvxRawhitListv2.C
// =======================
// ****************************************************************************
// Implementation of the container SvxRawhitListv2 for the SvxRawhitv2
// ****************************************************************************



#include <SvxRawhitListv2.h>

#include <algorithm>
#include <cstdlib>
#include <vector>

//#include <iostream>

ClassImp(SvxRawhitListv2)

// Constructor(s) & destructor
// """""""""""""""""""""""""""
SvxRawhitListv2::SvxRawhitListv2(const unsigned int length)
{
  m_hit_list   = new TClonesArray("SvxRawhitv2", length);
  m_hit_id_unused = 0;
  unSort();
  listPointersSet = false;
  //std::cout << "SvxRawhitListv2 object created" << std::endl;
}

SvxRawhitListv2::~SvxRawhitListv2()
{
  delete m_hit_list;
  //std::cout << "SvxRawhitListv2 object destroyed" << std::endl;
}

// The "standard PHObject response" functions...
// """""""""""""""""""""""""""""""""""""""""""""
void SvxRawhitListv2::Reset()
{
//  std::cout<<PHWHERE<<" Reset is called"<<std::endl;
  m_hit_id_unused = 0;
  m_hit_list->Clear();
  if ( m_hit_list->GetSize() > SVXNRAWHIT ) m_hit_list->Expand(SVXNRAWHIT);
  unSort();
  listPointersSet = false;
}

int SvxRawhitListv2::isValid() const
{
  return ( ( get_nRawhits() > 0 ) ? 1 : 0 ) ;
}

void SvxRawhitListv2::identify(std::ostream &os) const
{
  os << "Identify yourself: SvxRawhitListv2 object: nRawhits = "
     << get_nRawhits() << std::endl;
}

// Add/remove/set/get methods...
// """""""""""""""""""""""""""""
SvxRawhit* SvxRawhitListv2::addRawhit(const int ihit)
{
  int         index = ( ihit < 0 ) ? get_nRawhits() : ihit ;
  TClonesArray   &P = *m_hit_list;
  SvxRawhit* rawhit = new(P[index]) SvxRawhitv2(this);
  rawhit->set_hitID(m_hit_id_unused++);
  if ( get_nRawhits() == 0 ) listPointersSet = true;
  unSort();
  return rawhit;
}

void SvxRawhitListv2::removeRawhit (const unsigned int ihit)
{
  m_hit_list->RemoveAt(ihit) ;
  unSort();
}

// Routines to manipulate the cluster array...
// """""""""""""""""""""""""""""""""""""""""""
int SvxRawhitListv2::Compress()
{
  m_hit_list->Compress();
  return (int) get_nRawhits();
}

int SvxRawhitListv2::set_TClonesArraySize (const unsigned int nhit)
{
  if ( nhit > (unsigned int)get_nRawhits() ) m_hit_list->Expand(nhit);
  return m_hit_list->GetSize();
}

// Sorting and index search
// """"""""""""""""""""""""
void SvxRawhitListv2::restoreListPointers ()
{
  SvxRawhitv2* rawhit = NULL;
  for (int i = 0; i < get_nRawhits(); i++ ) {
    if ( ( rawhit = (SvxRawhitv2*) m_hit_list->UncheckedAt(i) ) )
      rawhit->set_ListPointer(this);
  }
  listPointersSet = true;
}

void SvxRawhitListv2::sort_hitID () {
  if ( hitIDsorted      ) return;
  if ( !listPointersSet ) restoreListPointers();
  SvxRawhitv2 hit;
  hit.set_sortingSwitch(-1);
  sensorIDsorted = false;
//  m_hit_list->UnSort();
  SortSTL();
//  m_hit_list->Sort(get_nRawhits());
  hitIDsorted    = true;
}

void SvxRawhitListv2::sort_sensorID () {
  if ( sensorIDsorted   ) return;
  if ( !listPointersSet ) restoreListPointers();
  SvxRawhitv2 hit;
  hit.set_sortingSwitch(1);
  hit.set_compareChan(3);
  hitIDsorted    = false  ;
//  m_hit_list->UnSort()        ;
  SortSTL();
//  m_hit_list->Sort(get_nRawhits())  ;
  sensorIDsorted = true   ;
}

void SvxRawhitListv2::unSort() {
  hitIDsorted    = false;
  sensorIDsorted = false;
//  m_hit_list->UnSort();
}

void SvxRawhitListv2::SortSTL()
{
   int n_ent = get_nRawhits();

   // make and sort an temporal array
//   std::vector<SvxHit*> hlist(n_ent);
//   for (int i = 0; i < n_ent; i++) {
   std::vector<SvxHit*> hlist(n_ent);
   for (int i = 0; i < n_ent; i++) {
      hlist[i] = get_Rawhit(i)->Clone();
//      std::cout << " " << hlist[i] << std::endl;
//      hlist[i]->print();
//      std::cout << get_Rawhit(i) << std::endl;
//      std::cout << hlist[i] << std::endl;
   }

//   int fuga = 0;
//   for (std::vector<SvxHit*>::iterator iter = hlist.begin();
//        iter != hlist.end(); iter++){
//      std::cout << "XX " << fuga++ << std::endl;
//      (*iter)->print();
//   }
//   std::cout << "YY " << n_ent << std::endl;
//   std::cout << "Cloned aa " << n_ent << std::endl;
   std::sort(hlist.begin(), hlist.end(), SvxHit::CompPointer);
//   std::cout << "Cloned aa " << std::endl;
   // copy parameters and delete the item of the temporal array
   for (int i = 0; i < n_ent; i++) {
      get_Rawhit(i)->Copy(hlist[i]);
      delete hlist[i];
   }
//   std::cout << "Cloned aa " << std::endl;
}

// Find first occurrence of rawhit
int SvxRawhitListv2::indexOfRawhit (
   const int hitid, int ifrom, int iupto) const
{
  if (get_nRawhits() == 0) return -1;

  if (iupto < 0) iupto = get_nRawhits() - 1;
  if (ifrom < 0 || iupto >= get_nRawhits() || ifrom > iupto) {
     std::cerr << "SvxHitListv2::FindHitIndex(): Invalid search range, from "
               << ifrom << " upto " << iupto << ", where the hit list has " 
               << get_nRawhits() << " entries." << std::endl;
     exit(1);
  }

  if ( iupto-ifrom > 8 && hitIDsorted ) {
     int i_low = ifrom - 1;
     int i_up  = iupto + 1;
     while (i_up - i_low > 1) {
        int i_cent     = (i_low + i_up) / 2;
        int hitid_curr = get_Rawhit(i_cent)->get_hitID();

        if      (hitid_curr == hitid) { return i_cent; }
        else if (hitid_curr <  hitid) { i_low = i_cent; }
        else                          { i_up = i_cent; }
     }
     return -1;

  } else {

    SvxRawhit* phit = NULL;
    for ( int i = ifrom; i <= iupto; i++ ) {
      if ( ( phit = get_Rawhit(i) ) ) {
	if ( phit->get_hitID() == hitid ) return i;
      }
    }
    return -1;

  } // if (iupto-ifrom > 8 && hitIDsorted)
}

//int SvxRawhitListv2::indexOfRawhit (const SvxSensor* sensor,
//				    const int         ifrom,
//				    const int         iupto) const {
//
//  int indto = ( iupto < 0 ) ? get_nRawhits() : iupto;
//
//  if ( indto-ifrom > 8 && sensorIDsorted ) {
//
//    SvxRawhitv2 hit;
//    hit.set_svxSection(sensor->get_svxSection());
//    hit.set_layer(sensor->get_layer());
//    hit.set_ladder(sensor->get_ladder());
//    hit.set_sensor(sensor->get_sensor());
//    hit.set_sortingSwitch(1);
//    hit.set_compareChan(0);
//    int ind = m_hit_list->BinarySearch(&hit, indto);
//    if ( (ind == ifrom ) || ind < 0 ) return ind;
//    SvxRawhit* phit = NULL;
//    if ( ind < ifrom ) {
//      for ( int i = ifrom; i < indto; i++ ) {
//	if ( ( phit = get_Rawhit(i) ) ) {
//	  return ( sensor->checkID(phit) ) ? i : -1;
//	}
//      }
//      return -1;
//    } else {
//      int ilast = ind;
//      for ( int i = ind-1; i >= ifrom; i-- ) {
//	if ( ( phit = get_Rawhit(i) ) ) {
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
//    SvxRawhit* phit = NULL;
//    for ( int i = ifrom; i < indto; i++ ) {
//      if ( ( phit = get_Rawhit(i) ) ) {
//	if ( sensor->checkID(phit) ) return i;
//      }
//    }
//    return -1;
//
//  } // if (indto-ifrom > 8 && hitIDsorted)
//}

// SvxRawhit of the same sensor (npar=0) && sensorSection (npar=1)
//                                       && sensorReadout (npar=2)
//                                       && channel       (npar=3) 
int SvxRawhitListv2::indexOfRawhit (      SvxRawhit& rawhit,
				    const int        npar  ,
				    const int        ifrom ,
				    const int        iupto ) const {

  int indto = ( iupto < 0 ) ? get_nRawhits() : iupto;

  SvxRawhitv2* hit = (SvxRawhitv2*) &rawhit;
  hit->set_sortingSwitch(1);
  hit->set_compareChan(npar);
  if ( indto-ifrom > 8 && sensorIDsorted ) {
    int ind = m_hit_list->BinarySearch(hit, indto);
    if ( (ind == ifrom ) || ind < 0 ) return ind;
    SvxRawhit* phit = NULL;
    if ( ind < ifrom ) {
      for ( int i = ifrom; i < indto; i++ ) {
	if ( ( phit = get_Rawhit(i) ) ) {
	  if ( hit->Compare(phit) ) {
	    return -1;
	  } else {
	    if ( npar > 2 )
               std::cout << "SvxRawhitListv2::indexOfRawhit--WARNING: "
                         << "More than one SvxRawhits for the same channel"
                         << " found in the list" << std::endl;
	    return i;
	  }
	}
      }
      return -1;
    } else {
      int ilast = ind;
      for ( int i = ind-1; i >= ifrom; i-- ) {
	if ( ( phit = get_Rawhit(i) ) ) {
	  if ( hit->Compare(phit) ) {
	    return ilast;
	  } else {
	    ilast = i;
	    if ( npar > 2 )
               std::cout << "SvxRawhitListv2::indexOfRawhit--WARNING: "
                         << "More than one SvxRawhits for the same channel"
                         << " found in the list" << std::endl;
	  }
	}
      }
      return ilast;
    }

  } else {

    SvxRawhit* phit = NULL;
    for ( int i = ifrom; i < indto; i++ ) {
      if ( ( phit = get_Rawhit(i) ) ) {
	if ( !hit->Compare(phit) ) return i;
      }
    }
    return -1;

  } // if (indto-ifrom > 8 && hitIDsorted)
}

// SvxRawhit having the pointer equal to hit
int SvxRawhitListv2::indexOfRawhit (const SvxRawhit* hit  ,
				    const int        ifrom,
				    const int        iupto) const {
  int indto = ( iupto < 0 ) ? get_nRawhits() : iupto ;
  for ( int i = ifrom; i < indto; i++ ) {
    if ( get_Rawhit(i) == hit ) return i;
  }
  return -1;
}

bool SvxRawhitListv2::indexOfRawhit(const SvxSensor* sensor, 
                                    int& idx_lb, int& idx_ub, 
                                    int ifrom, int iupto) const
{

   if (! sensorIDsorted) {
      std::cerr << "SvxRawhitListv2::indexOfRawhit(): The hit list hasn't been sorted by sensorID." << std::endl;
      exit(1);
   }
   idx_lb = -1;
   idx_ub = -2;
   if (get_nRawhits() == 0) return false;
   if (iupto < 0) iupto = get_nRawhits() - 1;
   if (ifrom < 0 || iupto >= get_nRawhits() || ifrom > iupto) {
      std::cerr << "SvxRawhitListv2::indexOfRawhit(): Invalid search range, from "
           << ifrom << " upto " << iupto << ", where the hit list has " 
           << get_nRawhits() << " entries." << std::endl;
      exit(1);
   }

   SvxRawhitv2* hit_cmp = new SvxRawhitv2();
   hit_cmp->set_svxSection(sensor->get_svxSection());
   hit_cmp->set_layer     (sensor->get_layer());
   hit_cmp->set_ladder    (sensor->get_ladder());
   hit_cmp->set_sensor    (sensor->get_sensor());
   hit_cmp->set_sortingSwitch(1); // sort by sensor ID
   hit_cmp->set_compareChan(0);

   int i_low = ifrom - 1;
   int i_up  = iupto + 1;
   int i_mid = -1; // index of a first matched entry
   while (i_up - i_low > 1) {
      int i_cent     = (i_low + i_up) / 2;
      int cmp = hit_cmp->Compare(get_Rawhit(i_cent));
      if      (cmp == 0) { i_mid = i_cent;  break; }
      else if (cmp >  0) { i_low = i_cent; }
      else               { i_up  = i_cent; }
   }
   if (i_mid < 0) {
      delete hit_cmp;
      return false;
   }

   idx_lb = idx_ub = i_mid;
   for (; idx_lb > i_low + 1 && hit_cmp->Compare(get_Rawhit(idx_lb - 1)) == 0; idx_lb--);
   for (; idx_ub < i_up  - 1 && hit_cmp->Compare(get_Rawhit(idx_ub + 1)) == 0; idx_ub++);

   delete hit_cmp;
   return true;
}

// Methods
// """""""
void SvxRawhitListv2::print() const
{
   std::cout << "SvxRawhitListv2: nRawhits = " << get_nRawhits()
       << " hitIDsorted = "    << hitIDsorted
       << " sensorIDsorted = " << sensorIDsorted << std::endl;
  SvxRawhit* rawhit = NULL;
  for (int i=0; i < get_nRawhits(); i++) {
    std::cout << "SvxRawhit number " << i << ":";
    if( (rawhit = get_Rawhit(i)) ) {
      std::cout << std::endl;
      rawhit->print();
    } else {
      std::cout << " missing ..." << std::endl;
    }
  }
}
