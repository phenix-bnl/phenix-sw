// ================================
// FILE: SvxCentralTrackRecalList.C
// ================================

#include "SvxCentralTrackRecalList.h"

ClassImp(SvxCentralTrackRecalList)

using namespace std;


// Constructor & Destructor
// """""""""""""""""""""""""""
SvxCentralTrackRecalList::SvxCentralTrackRecalList(const unsigned int length)
{
  m_list = new TClonesArray("SvxCentralTrackRecal", length);
  m_id_unused = 0;
  //std::cout << "SvxCentralTrackRecalList object created" << std::endl;
}

SvxCentralTrackRecalList::~SvxCentralTrackRecalList()
{
  delete m_list;
}


// The "standard PHObject response" functions...
// """""""""""""""""""""""""""""""""""""""""""""
void SvxCentralTrackRecalList::Reset()
{
  m_id_unused = 0;
  m_list->Clear();
  if ( m_list->GetSize()>SVXNCNTRECAL ) m_list->Expand(SVXNCNTRECAL);
}

int SvxCentralTrackRecalList::isValid() const
{
  return ( (GetNentries()>0) ? 1 : 0 );
}

void SvxCentralTrackRecalList::identify(std::ostream &os) const
{
  os << "Identify yourself: SvxCentralTrackRecalList object: nCentralTrackRecals = "
     << GetNentries() << std::endl;
}


// Add/remove/set/get methods...
// """""""""""""""""""""""""""""
SvxCentralTrackRecal* SvxCentralTrackRecalList::AddEntry(const int ihit)
{
  int index = ( ihit<0 ) ? GetNentries() : ihit;
  TClonesArray &P = *m_list;
  SvxCentralTrackRecal *svxcntrecal = new(P[index]) SvxCentralTrackRecal();
  svxcntrecal->set_ID(m_id_unused++);
  return svxcntrecal;
}

void SvxCentralTrackRecalList::RemoveEntry(const unsigned int ihit)
{
  m_list->RemoveAt(ihit);
}


// Routines to manipulate the cluster array...
// """""""""""""""""""""""""""""""""""""""""""
int SvxCentralTrackRecalList::Compress()
{
  m_list->Compress();
  return GetNentries();
}

int SvxCentralTrackRecalList::set_TClonesArraySize (const unsigned int nhit)
{
  if ( nhit>(unsigned int)GetNentries() ) m_list->Expand(nhit);
  return m_list->GetSize();
}


// Methods
// """""""
void SvxCentralTrackRecalList::print() const
{
  std::cout << "SvxCentralTrackRecalList: nSvxCentralTrackRecals = "
	    << GetNentries() << std::endl;
  SvxCentralTrackRecal* svxcntrecal = NULL;
  for ( int i=0; i<GetNentries(); i++ ) {
    std::cout << "SvxCentralTrackRecal number " << i << ":";
    if( (svxcntrecal=GetSvxCNTRecal(i)) ) {
      std::cout << std::endl;
      svxcntrecal->print();
    } else {
      std::cout << " missing ..." << std::endl;
    }
  }
}
