#include "emcTofT0FEM.h"
#include "emcTracedValue.h"
#include "emcCalFEMFactory.h"

#include <cassert>
#include <cstring>
#include <string>

using namespace std;

namespace
{ 
  static string name = "emcTofT0FEM";
  static string title = "Tof T0 drift";
  string Title(int version)
  {
    string rv = "ToF T0 Drift ";
    if ( version == 1 ) 
      {
	rv += " (with BBC global T0)";
      }
    return rv;
  }
  static string classname = "emcTofT0FEM";

  // Below are the necessary pieces to let the emcCalFEMFactory
  // build emcTofT0FEM objects.
  emcCalFEM* creator(int absPosition,
		     const PHTimeStamp& start,
		     const PHTimeStamp& end,
		     bool isDefault)
  {
    if ( isDefault )
      {
	return emcTofT0FEM::Default(absPosition,start,end,0);
      }
    else
      {
	return new emcTofT0FEM(absPosition,start,end,0);
      }
  }
 
  emcCalFEM* creatorB(int absPosition,
		      const PHTimeStamp& start,
		      const PHTimeStamp& end,
		      bool isDefault)
  {
    if ( isDefault )
      {
	return emcTofT0FEM::Default(absPosition,start,end,1);
      }
    else
      {
	return new emcTofT0FEM(absPosition,start,end,1);
      }
  }

  static bool r = emcCalFEMFactory::registerCreator("TofT0s",
						    creator);
  static bool rB = emcCalFEMFactory::registerCreator("TofT0Bs",
						     creatorB);
}

//_____________________________________________________________________________
emcTofT0FEM::emcTofT0FEM(int absPosition, int version)
  : emcGainFEM(absPosition), fBBCT0(0)
{
  NameIt(name,Title(version),classname);
  SetVersion(version);
}

//_____________________________________________________________________________
emcTofT0FEM::emcTofT0FEM(int absPosition,
			 const PHTimeStamp& tStart, const PHTimeStamp& tEnd,
			 int version)
  : emcGainFEM(absPosition,tStart,tEnd), fBBCT0(0) 
{
  NameIt(name,Title(version),classname);
  SetVersion(version);
}

//_____________________________________________________________________________
emcTofT0FEM*
emcTofT0FEM::Default(int absPosition, 
		     const PHTimeStamp& tStart, const PHTimeStamp& tEnd,
		     int version)
{
  emcTofT0FEM* fem = new emcTofT0FEM(absPosition,tStart,tEnd,version);

  if ( version == 0 )
    {
      fem->SetNumberOfChannels(144);
      for ( size_t i = 0; i < fem->size(); i++ ) 
	{
	  fem->AddNewItem(i,new emcTracedValue(0,0,0));
	}
    }
  else
    {
      fem->SetNumberOfChannels(145);
      for ( size_t i = 0; i < fem->size()-1; i++ ) 
	{
	  fem->AddNewItem(i,new emcTracedValue(0,0,0));
	}
      fem->AddNewItem(144,new emcTracedValue(0,0,0));
    }
  

  return fem;

}

//_____________________________________________________________________________
const char*
emcTofT0FEM::GetCategory(void) const
{
  if ( Version() == 0 )
    { 
      return "TofT0s"; 
    }
  else
    {
      return "TofT0Bs";
    }
}

//_____________________________________________________________________________
void
emcTofT0FEM::setBBCT0(float b)
{
  if ( Version() == 0 )
    {
      std::cerr << "<E> emcTofT0FEM::setBBCT0 not implemented for this version"
		<< " Using 0."
		<< std::endl;
    }
  else
    {
      fBBCT0=b;
    }
}

//_____________________________________________________________________________
void 
emcTofT0FEM::writeDataToFile(FILE* fp) const
{
  time_t tics = GetStartValTime().getTics(); 
  char timeString[25]; 
  timeString[24] = '\0'; 
  strncpy(timeString, ctime(&tics), 24);   
  fprintf(fp,"%s\n",timeString) ;

  size_t i,j ;
  int item = 0 ;

  for (i=0;i<fItems.size();i++) {
    assert(fItems[i]!=0) ;
    emcItemVector& itemVector = *(fItems[i]) ;
    for (j=0;j<itemVector.size();j++) {    
      fprintf(fp," %5d", item) ;
      itemVector[j]->writeData(fp) ;
      fprintf(fp,"\n") ;
    }
    item++ ;
  }
  fprintf(fp,"144 %ld 0 0\n",GetXmax());
  fprintf(fp,"145 %ld 0 0\n",GetXmin());
  if ( Version() > 0 ) 
    {
      fprintf(fp,"146 0 %f 0\n",getBBCT0());
    }
}
