#include "emcTracedFEMMerger.h"

#include <cassert>
#include <iostream>

#include "phool.h"
#include "emcTracedFEM.h"
#include "emcTracedValue.h"

//_____________________________________________________________________________
emcTracedFEM*
emcTracedFEMMerger::merge(const emcTracedFEM& t1,
			  const emcTracedFEM& t2)
{
  if ( t1.AbsolutePosition() != t2.AbsolutePosition() ||
       t1.GetNumberOfChannels() != t2.GetNumberOfChannels() )
    {
      std::cerr << PHWHERE << " t1 and t2 absolute positions not compatible !"
		<< std::endl;
      return 0;
    }

  if ( t1.GetXmin() > t2.GetXmin() )
    {
      std::cerr << PHWHERE << " t1 and t2 not time ordered !"
		<< std::endl;
      return 0;
    }

  emcTracedFEM* rv = t1.clone();
  assert(rv!=0);

  rv->SetXmax(t2.GetXmax());

  for ( size_t i = 0; i < t2.GetNumberOfChannels(); ++i )
    {
      emcTracedValue* last = rv->LastItem(i);
      assert(last!=0);

      int xrelmax = static_cast<int>(t1.GetXmax()-t1.GetXmin());

      // Add 1 constant line to connect end of t1 to start of t2
      rv->AddNewItem(i,new emcTracedValue(xrelmax,
					  last->getValue(xrelmax),
					  0,
					  true));
		     
      time_t toffset = t2.GetXmin()-t1.GetXmin();
      t2.FirstItem(i);
      emcTracedValue* tv = 0;
      bool first = true;

      while ( ( tv = t2.NextItem() ) )
	{
	  if (first)
	    {
	      // Add 1 constant line to connect "virtual" start of t2 
	      // to actual start of t2
	      rv->AddNewItem(i,new emcTracedValue(toffset,
						  tv->GetConstant(),
						  0,
						  true));
	      first=false;
	    }

	  rv->AddNewItem(i,new emcTracedValue(tv->GetX()+toffset,
					      tv->GetConstant(),
					      tv->GetSlope(),
					      tv->isConstant()));
	}
    }

  return rv;
}
