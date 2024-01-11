#include "emcObjectFillerManager.h"
#include "emcObjectFillerRegistry.h"
#include "emcObjectFiller.h"
#include <iostream>
#include "TClass.h"

using namespace std;

int emcObjectFillerManager::fVerbose=0;

//_____________________________________________________________________________
bool
emcObjectFillerManager::fill(PHCompositeNode* topNode, 
			    PHObject& dest)
  
{
  emcObjectFiller* converter = 
    emcObjectFillerRegistry::findFiller(topNode,dest,fVerbose);

  if ( !converter ) 
    {
      if ( fVerbose ) 
	{
	  cerr << "emcObjectFillerManager::fill : did not find "
	       << " a suitable filler for object of class "
	       << dest.ClassName()
	       << endl;
	}
      return false;
    }
  else
    {
      if ( fVerbose ) 
	{
	  cout << "emcObjectFillerManager::fill : "
	       << "I'm asking this objectfiller to do the job :";
	  converter->identify(cout);
	  cout << endl;
	}
      return converter->fill(topNode,dest,fVerbose);
    }
}

//_____________________________________________________________________________
bool
emcObjectFillerManager::isFillerAvailable(PHObject& dest)
{
  emcObjectFiller* converter = 
    emcObjectFillerRegistry::findFiller(dest,fVerbose);
  if ( converter ) 
    {
      return true;
    }
  else
    {
      return false;
    }
}
