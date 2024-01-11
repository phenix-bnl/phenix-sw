#include "HistogramCollection.h"

#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TDirectory.h"
#include <string>
#include <map>
#include <iostream>

using namespace std;

//_____________________________________________________________________________
HistogramCollection::HistogramCollection(TDirectory* dir)
{
  fDir=dir;
  fNevents1=0;
  fNevents2=0;
}

//_____________________________________________________________________________
TH1* 
HistogramCollection::book(const std::string& name, const std::string& title,
			  int nbins, double xmin, double xmax)
{
  if ( exist(name) ) 
    {
      std::cerr << "HistogramCollection::book : " << name << " already exists"
		<< std::endl;
      return 0;
    }
  else 
    {
      fMap[name] = new TH1F(name.c_str(),title.c_str(),nbins,xmin,xmax);
      fMap[name]->Sumw2();
      return fMap[name];
    }
} 

//_____________________________________________________________________________
TH2* 
HistogramCollection::book(const std::string& name, const std::string& title,
			  int nbinx, double xmin, double xmax,
			  int nbiny, double ymin, double ymax)
{
  if ( exist(name) ) 
    {
      std::cerr << "HistogramCollection::book : " << name << " already exists"
		<< std::endl;
      return 0;
    }
  else 
    {
      fMap[name] = new TH2F(name.c_str(),title.c_str(),
			    nbinx,xmin,xmax,
			    nbiny,ymin,ymax);  
      fMap[name]->Sumw2();
      return static_cast<TH2*>(fMap[name]);
    }
} 

//_____________________________________________________________________________
TH3* 
HistogramCollection::book(const std::string& name, const std::string& title,
			  int nbinx, double xmin, double xmax,
			  int nbiny, double ymin, double ymax,
			  int nbinz, double zmin, double zmax)
{
  if ( exist(name) ) 
    {
      std::cerr << "HistogramCollection::book : " << name << " already exists"
		<< std::endl;
      return 0;
    }
  else 
    {
      fMap[name] = new TH3F(name.c_str(),title.c_str(),
			    nbinx,xmin,xmax,
			    nbiny,ymin,ymax,
			    nbinz,zmin,zmax);  
      fMap[name]->Sumw2();
      return static_cast<TH3*>(fMap[name]);
    }
} 

//_____________________________________________________________________________
bool
HistogramCollection::exist(const std::string& name) const
{
  if ( fMap.find(name) != fMap.end() ) 
    { 
      return true; 
    }
  else 
    {
      return false;
    }
}

//_____________________________________________________________________________
bool
HistogramCollection::fillW(const std::string& name, 
			   double x, double weight)
{
  fIterator = fMap.find(name);

  if ( fIterator != fMap.end() ) 
    {

      TH1* h = dynamic_cast<TH1*>(fIterator->second);
      
      if ( !h ) 
	{
	  cerr << "HistogramCollection::fillW : Eurk. Got a bad thing here with histo " 
	       <<  name << endl;
	  return false;
	}
      
//     if ( h->InheritsFrom("TH2") ) {
//       std::cerr << "HistogramCollection::fillW(x) " << name << " is a 2D histo!"
// 		<< std::endl;
//       return false;
//     }

      h->Fill(x,weight);
      return true;
    }
  else 
    {
      std::cerr << "HistogramCollection::fillW(x) " << name << " is not in the collection "
		<< std::endl;
      return false;
    }
}

//_____________________________________________________________________________
bool
HistogramCollection::fillW(const std::string& name, 
			   double x, double y, double weight)
{
  fIterator = fMap.find(name);
  
  if ( fIterator != fMap.end() ) 
    {

      TH2* h = dynamic_cast<TH2*>(fIterator->second);
      
      if ( !h ) 
	{
	  std::cerr << "HistogramCollection::fillW(x,y) " << name << " is not a 2D histo!"
		    << std::endl;
	  return false;
	}

      h->Fill(x,y,weight);
      return true;
    }
  else 
    {
      std::cerr << "HistogramCollection::fillW(x,y) " << name << " is not in the collection !"
	        << std::endl;
      return false;
    }
}

//_____________________________________________________________________________
bool
HistogramCollection::fillW(const std::string& name, 
			   double x, double y, double z,
			   double weight)
{
  fIterator = fMap.find(name);
  
  if ( fIterator != fMap.end() ) 
    {

      TH3* h = dynamic_cast<TH3*>(fIterator->second);
      
      if ( !h ) 
	{
	  std::cerr << "HistogramCollection::fillW(x,y,z) " << name << " is not a 3D histo!"
		    << std::endl;
	  return false;
	}

      h->Fill(x,y,z,weight);
      return true;
    }
  else 
    {
      std::cerr << "HistogramCollection::fillW(x,y,z) " << name << " is not in the collection !"
	        << std::endl;
      return false;
    }
}

//_____________________________________________________________________________
TH1*
HistogramCollection::get(const std::string& name) const
{
  if ( exist(name) ) 
    {
      return fMap.find(name)->second;
    }
  else 
    {
      return 0;
    }
}

//_____________________________________________________________________________
void
HistogramCollection::print(std::ostream& out) const
{
  out << "**HistogramCollection (dir=" << fDir->GetPath() << std::endl;
  out << "  --histograms : " << std::endl;

  std::map<std::string, TH1*>::const_iterator it1;

  for ( it1 = fMap.begin() ; it1 != fMap.end() ; it1++ ) 
    {
      it1->second->ls();
    }
  
  out << "  --nevents (1,2) : " << getNevents1() 
      << " " << getNevents2() << std::endl;
}

//_____________________________________________________________________________
void
HistogramCollection::write(void) const 
{
  std::map<std::string, TH1*>::const_iterator it1;

  for ( it1 = fMap.begin() ; it1 != fMap.end() ; it1++ ) 
    {
      std::cout << "Writing " << it1->first << std::endl;
      it1->second->Write();
    }
}
