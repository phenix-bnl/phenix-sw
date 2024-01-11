#include "emcEmbedUtils.h"

#include "fkinWrapper.h"
#include <iostream>

//_____________________________________________________________________________
bool
check_fkin(fkinWrapper* fkin, float& simul_vtx, float& ptot_input)
{
  if (!fkin) return false;

  int entries_fkin = (int)fkin->RowCount();
      
  int icount;

  for ( icount = 0 ; icount < entries_fkin ; ++icount )
    { 
      // 1) let's check that there is a true primary track
      if ( fkin->get_idparent(icount)== 0  && 
	   fkin->get_itparent(icount)==-1 )
	{
	  simul_vtx  = fkin->get_z_vertex(icount);
	  ptot_input = fkin->get_ptot(icount);
	  break;
	}
    }
  
  if (icount == entries_fkin)
    {
      return false;
    }
  else
    {
      return true;
    }
}
