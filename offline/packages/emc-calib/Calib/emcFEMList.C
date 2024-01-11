#include "emcFEMList.h"

#include <algorithm>
#include <cctype>
#include <iostream>

#include "EmcIndexer.h"

namespace 
{
  void pbsc(std::set<std::string>& sectors)
  {
    sectors.insert("W0");
    sectors.insert("W1");
    sectors.insert("W2");
    sectors.insert("W3");
    sectors.insert("E2");
    sectors.insert("E3");
  }
  
  void pbgl(std::set<std::string>& sectors)
  {
    sectors.insert("E0");
    sectors.insert("E1");
  }
}

//_____________________________________________________________________________
emcFEMList::emcFEMList(const char* sectors)
{
  std::string sparts = sectors;
  std::transform(sparts.begin(),sparts.end(), sparts.begin(),::toupper);
  
  if ( sparts == "EMCAL" || sparts == "PBSCPBGL" || sparts == "PBGLPBSC" )
    {
      pbsc(fSectors);
      pbgl(fSectors);
    }
  else if ( sparts == "PBSC" )
    {
      pbsc(fSectors);
    }
  else if ( sparts == "PBGL" )
    {
      pbgl(fSectors);
    }
  else
    {
      for ( int i = 0; i < 8; ++i )
	{
	  std::string search = EmcIndexer::EmcSectorId(i);
	  size_t f = sparts.find(search);
	  if ( f < sparts.size() )
	    {
	      sparts.erase(f,f+search.size());
	      fSectors.insert(search);
	    }
	}
      
      if (!sparts.empty())
	{
	  std::cerr << "<ERROR> " << __FILE__ << ":" << __LINE__
		    << " parts not empty. Remaining stuff="
		    << sparts
		    << "\n"
		    << "It's likely a syntax error that you must correct."
		    << "I will consider you want no EMCAL sector at all," 
		    << " so it's likely to crash later on..."
		    << std::endl;
	  fSectors.clear();
	}
    }
  
  // We got our list of sectors. Now get the list of fems from there.
  std::set<std::string>::const_iterator it;
  for ( it = fSectors.begin(); it != fSectors.end(); ++it ) 
    {
      int is = EmcIndexer::EmcSectorNumber((*it).c_str());
      int nfems = 18;
      if ( is >= 6 )
	{
	  nfems = 32;
	}
      for ( int i = 0; i < nfems; ++i ) 
	{
	  int fem = EmcIndexer::iSiSM144_PXSM144(is,i);
	  fFems.insert(fem);
	}
    }
}

//_____________________________________________________________________________
std::set<int>
emcFEMList::fems(const char* sname) const
{
  std::set<int> rv;

  int is = EmcIndexer::EmcSectorNumber(sname);
  if ( is < 0 )
    {
      return rv;
    }

  int nfems = 18;
  if ( is >= 6 )
    {
      nfems = 32;
    }
  for ( int i = 0; i < nfems; ++i ) 
    {
      int fem = EmcIndexer::iSiSM144_PXSM144(is,i);
      rv.insert(fem);
    }
  return rv;
}

//_____________________________________________________________________________
bool
emcFEMList::hasFEM(int ifem) const
{
  std::set<int>::const_iterator it = fFems.find(ifem);

  return ( it != fFems.end() );
}

//_____________________________________________________________________________
bool
emcFEMList::hasSector(const char* name) const
{
  std::string sname = name;
  std::transform(sname.begin(),sname.end(),sname.begin(),::toupper);
  std::set<std::string>::const_iterator it = fSectors.find(sname);

  return ( it != fSectors.end() );
}

//_____________________________________________________________________________
void
emcFEMList::print(std::ostream& os) const
{
  os << "emcFEMList sectors:";
  std::set<std::string>::const_iterator it;
  for ( it = fSectors.begin(); it != fSectors.end(); ++it ) 
    {
      os << (*it) << " ";
    }
}
