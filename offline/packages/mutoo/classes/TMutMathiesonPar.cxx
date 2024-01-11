// $Id: TMutMathiesonPar.cxx,v 1.6 2010/09/10 00:24:58 hpereira Exp $
//////////////////////////////////////////////////////////////////
/*!
   \file    TMutMathiesonPar.cxx
   \brief   dictionarized class to handle mathieson parameters
   \author  H. Pereira
   \version $Revision: 1.6 $
   \date    $Date: 2010/09/10 00:24:58 $
*/
//////////////////////////////////////////////////////////////////

#include "TMutMathiesonPar.h"

ClassImp(TMutMathiesonPar)
using namespace std;

//_______________________________________________
unsigned int TMutMathiesonPar::_grid_size = 500;
boost::array< TMutMathiesonPar::mathieson_parameters, TMutMathiesonPar::n_parameters > TMutMathiesonPar::_cap_coupling;

/*
  This is new set of parameters derived by Hugo Pereira from Run4/Run5 p+p data.
  Tuned to have the q_peak/q_total distributions (charge on the peak strip in the cluster over total charge in the cluster)
  match between Monte-Carlo and real data.
*/
bool TMutMathiesonPar::initialize_mathieson_parameters( void )
{

  _cap_coupling.assign( make_pair(0,0) );

  // warning: need to call the "unchecked" version, that does not call 'check_initialized'
  // otherwise one enters an infinite (recursive) loop
  for( int octant = 0; octant < MUTOO::NumberOfOctants; octant++ )
  for( int gap = 0; gap < MUTOO::NumberOfGaps; gap++ )
  {
    set_mathieson_parameters_unchecked( 0, 0, octant, gap, 0.43,0.31 );
    set_mathieson_parameters_unchecked( 0, 1, octant, gap, 0.27,0.31 );
    set_mathieson_parameters_unchecked( 0, 2, octant, gap, 0.45,0.37 );
    set_mathieson_parameters_unchecked( 1, 0, octant, gap, 0.39,0.29 );
    set_mathieson_parameters_unchecked( 1, 1, octant, gap, 0.31,0.29 );
    set_mathieson_parameters_unchecked( 1, 2, octant, gap, 0.41,0.41 );
  }

  return true;
}


//__________________________________________________________
void TMutMathiesonPar::print( std::ostream& out )
{
  MUTOO::PRINT( out, "TMutMathiesonPar::print" );
  std::cout << "_grid_size: " << _grid_size << std::endl;
  for( int arm=0; arm<MUTOO::NumberOfArms; arm++ )
    for( int station = 0; station < MUTOO::NumberOfStations; station++ )
      for( int gap = 0; gap < MUTOO::NumberOfGaps; gap++ )
        for( int octant = 0; octant<MUTOO::NumberOfOctants; octant++)
	  {
	    if( station==2 && gap==2 ) continue;

        mathieson_parameters pars( get_mathieson_parameters( arm, station, octant, gap ) );
        out
          << "[" << arm << "," << station << "," << octant << "," << gap  << "] "
	      << " cc=" << pars.first
	      << " ac=" << pars.second
	      << std::endl;
	  }

  MUTOO::PRINT( out, "**" );
}
