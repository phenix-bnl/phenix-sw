#include "EventSorting.h"
#include <cstdio>
#include <iostream>
#include <iomanip>

ClassImp(EventSorting)

//_____________________________________________________________________________
EventSorting::EventSorting()
{
}

//_____________________________________________________________________________
EventSorting::EventSorting(const EventSorting& es)
{
  es.copyTo(*this);
}

//_____________________________________________________________________________
EventSorting&
EventSorting::operator=(const EventSorting& es)
{
  if ( &es != this ) 
    {
      es.copyTo(*this);
    }
  return *this;
}

//_____________________________________________________________________________
void 
EventSorting::copyTo(EventSorting& es) const
{
  es.set(fCentralities,fVertices);
  for (size_t ic = 0 ; ic < fCentralities.size() ; ++ic )
    {	  
      for (size_t iv = 0 ; iv < fVertices.size() ; ++iv )
	{
	  es.fNEvents(ic,iv) = fNEvents(ic,iv);
	}
    }  
}

//_____________________________________________________________________________
EventSorting&
EventSorting::operator+=(const EventSorting& es)
{
  // First check that we can add

  if ( es.getCentralities() != fCentralities ||
       es.getVertices() != fVertices ) 
    {
      std::cerr << "EventSorting::operator+= : objects are not compatbile!"
		<< std::endl;
    }
  else 
    {
      for (size_t ic = 0 ; ic < fCentralities.size() ; ++ic )
	{	  
	  for (size_t iv = 0 ; iv < fVertices.size() ; ++iv )
	    {
	      fNEvents(ic,iv) += es.fNEvents(ic,iv);
	    }
	}
    }
  return *this;
}

//_____________________________________________________________________________
void 
EventSorting::init(void)
{
  fPathNames.clear();

  char path[1024];

  fPathNames.resize(fCentralities.size());

  fNEvents.ResizeTo(fCentralities.size(),fVertices.size());
  fNEvents.Zero();

  for ( size_t ic = 0 ; ic < fCentralities.size() ; ic++ ) 
    {
      sprintf(path,"C%d",ic);
      fPathNames[ic] = path;
    }
}

//_____________________________________________________________________________
EventSorting::~EventSorting()
{
}

//_____________________________________________________________________________
std::vector<int>
EventSorting::getCentralities(void) const
{
  return fCentralities;
}

//_____________________________________________________________________________
void 
EventSorting::getCentralityClassLimits(size_t index, int& centMin, 
				       int& centMax) const
{
  if ( index==0 )
    {
      centMin=0;
    }
  else if ( (index==fCentralities.size()-1) ) // the last one is minbias
    {
      centMin=10000;
    }
  else
    {
      centMin=fCentralities[index-1];
    }
  centMax=fCentralities[index];
}

//_____________________________________________________________________________
void 
EventSorting::getCentralityDescription(size_t index, std::string& title) const
{
  int centMin, centMax;
  char tmp[200];

  if ( index == fCentralities.size()-1 )
    {
      title = "Min.bias";
    }
  else
    {
      getCentralityClassLimits(index, centMin, centMax);
      sprintf(tmp,"Centrality [%d-%d %%[ ",centMin,centMax);
      title = tmp;
    }
}

//_____________________________________________________________________________
size_t
EventSorting::getCentralityIndex(const int centrality) const
{
  int ic = -1;

  for ( size_t i = 0 ; i < fCentralities.size() && ic==-1; i++) 
    {
      if ( centrality < fCentralities[i] ) 
	{ 
	  ic = i; 
	}
    }
  return ic;
}

//_____________________________________________________________________________
bool
EventSorting::getIndices(int centrality, double zvertex,
			 int &ic, int& iv) const
{

  ic = iv = -1;
  bool inside = false;

  ic = getCentralityIndex(centrality);

  if ( ic >= 0 ) 
    {

      iv = getVertexIndex(zvertex);

      if ( iv >= 0 ) 
	{
	  inside = true;      
	}
    }

  return inside;
}

//_____________________________________________________________________________
size_t
EventSorting::getNCentralities(void) const
{
  return fCentralities.size();
}

//_____________________________________________________________________________
float
EventSorting::getNEvents(void) const
{
  float n = 0;

  for ( size_t ic = 0 ; ic < fCentralities.size(); ++ic )
    {
      for ( size_t iv = 0 ; iv < fVertices.size(); ++iv )
	{
	  n += fNEvents(ic,iv);
	}
    }
  return n;
}

//_____________________________________________________________________________
float
EventSorting::getNEvents(const int centrality) const
{
  int ic = getCentralityIndex(centrality);
  float n = 0;

  if ( ic >= 0 ) 
    {
      for ( size_t iv = 0 ; iv < fVertices.size() ; ++iv ) 
	{
	  n += fNEvents(ic,iv);
	}
    }
  return n;
}

//_____________________________________________________________________________
float
EventSorting::getNEvents(int centrality, double zvertex) const
{
  int ic,iv;
  float n = 0;

  bool inside = getIndices(centrality,zvertex,ic,iv);

  if ( inside ) 
    {
      n = fNEvents(ic,iv);
    }
  return n;
}


//_____________________________________________________________________________
bool
EventSorting::getPath(int centrality, std::string& spath) const
{
  static std::string kUnknown = "unknown";

  int icent;
  int idummy;

  bool inside = getIndices(centrality,0,icent,idummy);

  if ( !inside ) 
    {
      spath = kUnknown;
    }
  else
    {
      spath = fPathNames[icent];
    }
  return inside;
}

//_____________________________________________________________________________
int
EventSorting::getVertexIndex(const double zvertex) const
{
  int iv = -1;

  for ( size_t i = 0 ; i < fVertices.size()-1 && iv==-1; i++) 
    {
      if ( zvertex>= fVertices[i] && zvertex < fVertices[i+1] ) 
	{ 
	  iv = i; 
	}
    }
  return iv;
}

//_____________________________________________________________________________
std::vector<double>
EventSorting::getVertices(void) const
{
  return fVertices;
}

//_____________________________________________________________________________
float
EventSorting::incrNevents(const int centrality, const double zvertex, float n)
{
  int ic, iv;
  bool inside = getIndices(centrality,zvertex,ic,iv);
  if ( inside ) 
    {
      fNEvents(ic,iv) += n;
      return n;
    }
  return 0;
}

//_____________________________________________________________________________
std::ostream&
EventSorting::print(std::ostream& out) const
{
  std::ostream::fmtflags oldflags = out.flags();

  static std::string sp1 = "  ";
  static std::string sp2 = "    ";

  std::vector<int> cent = fCentralities;
  std::vector<double> vert = fVertices;

  cent.insert(cent.begin(),0);
    
  for ( size_t ic = 0 ; ic < cent.size()-1 ; ++ic ) 
    {
      out << sp1 << "[" << setw(2) << cent[ic] << "-" 
	  << setw(3) << cent[ic+1] 
	  << " [ % : nevents = " << getNEvents(cent[ic])
	  << endl;
      for ( size_t iv = 0 ; iv < vert.size()-1 ; ++iv )
	{
	  out << sp1 << sp2 
	      << "[" << setw(2) << vert[iv] << "-"
	      << setw(4) << vert[iv+1] << " [ cm : nevents = "
	      << fNEvents(ic,iv)
	      << endl;
	}
    }

  out << "Total number of events : " << getNEvents() << endl;

  out.setf(oldflags);
  return out;
}

//_____________________________________________________________________________
void
EventSorting::reset(void)
{
  fNEvents.Zero();
}

//_____________________________________________________________________________
void
EventSorting::set(const std::vector<int>& cent,
		  const std::vector<double>& vert)
{
  fCentralities=cent;
  fVertices=vert;
  init();
}
