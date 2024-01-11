#include "emcObjectFillerRegistry.h"
#include "emcObjectFiller.h"
#include <iostream>

using namespace std;

//_____________________________________________________________________________
void
emcObjectFillerRegistry::add(emcObjectFiller* oc)
{
  fillers()->push_back(oc);
}

//_____________________________________________________________________________
void
emcObjectFillerRegistry::remove(emcObjectFiller* oc)
{
  fillers()->remove(oc);
}

//_____________________________________________________________________________
emcObjectFiller*
emcObjectFillerRegistry::findFiller(PHCompositeNode* topNode, PHObject& dest,
				   int verbose)
{
  if ( fillers()->empty() ) 
    {
      cerr << "emcObjectFillerRegistry::findFiller : not a single filler "
	   << "available at this time! Sorry."
	   << endl;
      return 0;
    }

  list<emcObjectFiller*>::iterator it;

  list<emcObjectFiller*>* thelist = fillers();

  emcObjectFiller* filler = 0;

  for ( it = thelist->begin(); it != thelist->end(); it++ ) 
    {
      if ( (*it)->canFill(topNode,dest,verbose) )
	{
	  if ( filler==0 ) 
	    {
	      filler = *it;
	    }
	  else
	    {
	      cerr << "emcObjectFillerRegistery::findFiller : "
		   << " More than one suitable Filler found !"
		   << endl
		   << "Do you really expect ME to decide which one to "
		   << " use ? No way. " 
		   << endl;
	      filler = 0;
	      break;
	    }
	}
    }

  return filler;

}

//_____________________________________________________________________________
emcObjectFiller*
emcObjectFillerRegistry::findFiller(PHObject& dest,
				   int verbose)
{
  list<emcObjectFiller*>::iterator it;
  list<emcObjectFiller*>* thelist = fillers();

  emcObjectFiller* filler = 0;

  for ( it = thelist->begin(); it != thelist->end(); it++ ) 
    {
      if ( (*it)->canFill(dest,verbose) )
	{
	  if ( filler==0 ) 
	    {
	      filler = *it;
	    }
	  else
	    {
	      cerr << "emcObjectFillerRegistery::findFiller : "
		   << " More than one suitable Filler found !"
		   << endl
		   << "Do you really expect ME to decide which one to "
		   << " use ? No way. " 
		   << endl;
	      filler = 0;
	      break;
	    }
	}
    }
  return filler;
}


//_____________________________________________________________________________
list<emcObjectFiller*>*
emcObjectFillerRegistry::fillers(void)
{
  static list<emcObjectFiller*> thelist;
  return &thelist;
}

//_____________________________________________________________________________
void
emcObjectFillerRegistry::print(ostream& out)
{
  out << "emcObjectFillerRegistry : here are my ObjectFillers :"
      << endl;

  list<emcObjectFiller*>::iterator it;

  list<emcObjectFiller*>* thelist = fillers();

  for ( it = thelist->begin(); it != thelist->end(); it++ ) 
    {
      (*it)->identify(out);
    }
}
