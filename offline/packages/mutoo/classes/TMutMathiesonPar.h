#ifndef __TMUTMATHIESONPAR_H__
#define __TMUTMATHIESONPAR_H__

// $Id: TMutMathiesonPar.h,v 1.17 2014/11/27 01:24:09 slash Exp $
//////////////////////////////////////////////////////////////////
/*!
   \file    TMutMathiesonPar.h
   \brief   dictionarized class to handle mathieson parameters
   \author  H. Pereira
   \version $Revision: 1.17 $
   \date    $Date: 2014/11/27 01:24:09 $
*/
//////////////////////////////////////////////////////////////////

#ifndef __CINT__
#include "MUTOO.h"
#include <boost/array.hpp>
#endif

#include <iostream>
#include <map>

//! dictionarized class to handle mathieson parameters
class TMutMathiesonPar
{
	public:

  //! mathieson grid size
  static const unsigned int& get_grid_size( void )
  { return _grid_size; }

  //! grid size
  static void set_grid_size( const unsigned int& value )
  { _grid_size = value; }

  /*! Set the value of the mathieson parameters in given arm station */
  inline static void set_mathieson_parameters(
    unsigned short arm,
    unsigned short station,
    unsigned short octant,
    unsigned short gap,
    const double& first, const double& second);

  /*! Set the value of the mathieson parameters in given arm station */
  inline static void set_mathieson_parameters(
    unsigned short arm,
    unsigned short station,
    const double& first, const double& second);

  //! print capacitive coupling parameters
  static void print( std::ostream& out = std::cout );

  //! get index of mathieson parameter for a given arm, station, octant, gap
  inline static int get_mathieson_index(int arm, int station, int octant, int gap);

  //! make sure mathieson parameters are initialized
  inline static void check_initialized( void );

#ifndef __CINT__

  //! mathieson parameters (neighbor strip coupling, cathode to anode coupling)
  typedef std::pair< double, double > mathieson_parameters;

  //______________________________________________________________________
  /*! Set the value of the mathieson parameters in given arm station */
  inline static void set_mathieson_parameters(
    unsigned short arm,
    unsigned short station,
    unsigned short octant,
    unsigned short gap,
    const mathieson_parameters& value)
    { set_mathieson_parameters( arm, station, octant, gap, value.first, value.second ); }

  //______________________________________________________________________
  /*! Get the value of the capacative coupling in given arm station */
  inline static mathieson_parameters get_mathieson_parameters(
    unsigned short arm,
    unsigned short station,
    unsigned short octant,
    unsigned short gap
    );

  //______________________________________________________________________
	//! number of capacitive coupling parameters
  enum { n_parameters=MUTOO::NumberOfArms*MUTOO::NumberOfStations*MUTOO::NumberOfOctants*MUTOO::NumberOfGaps};

#endif

	private:

  //! is called (once) in check_initialized
  static bool initialize_mathieson_parameters( void );

  /*! Set the value of the mathieson parameters in given arm station */
  inline static void set_mathieson_parameters_unchecked(
    unsigned short arm,
    unsigned short station,
    unsigned short octant,
    unsigned short gap,
    const double& first, const double& second);

#ifndef __CINT__
  /*!
		capacitive coupling.
		first parameter is cathode coupling (neighbor strips)
  	second parameter is anode coupling (wire to strip)
	*/
  typedef boost::array< mathieson_parameters, n_parameters > MathiesonParametersArray;
  static MathiesonParametersArray _cap_coupling;

#endif

  //! mathieson grid size
  static unsigned int _grid_size;

};

#ifndef __CINT__

//__________________________________________________________
void TMutMathiesonPar::check_initialized()
{ static bool initialized __attribute__ ((unused)) = initialize_mathieson_parameters(); }

//__________________________________________________________
void TMutMathiesonPar::set_mathieson_parameters(
    unsigned short arm,
    unsigned short station,
    const double& first, const double& second)
{
  for( unsigned int octant = 0; int(octant) < MUTOO::NumberOfOctants; octant++ )
    for( unsigned int gap = 0; int(gap) < MUTOO::NumberOfGaps; gap++ )
  { set_mathieson_parameters( arm, station, octant, gap, first, second ); }
}

//__________________________________________________________
void TMutMathiesonPar::set_mathieson_parameters(
    unsigned short arm,
    unsigned short station,
    unsigned short octant,
    unsigned short gap,
    const double& first, const double& second)
{
    check_initialized();
    set_mathieson_parameters_unchecked( arm, station, octant, gap, first, second );
}

//__________________________________________________________
void TMutMathiesonPar::set_mathieson_parameters_unchecked(
    unsigned short arm,
    unsigned short station,
    unsigned short octant,
    unsigned short gap,
    const double& first, const double& second)
{ _cap_coupling[ get_mathieson_index( arm, station, octant, gap ) ] = std::make_pair( first, second ); }

//__________________________________________________________
TMutMathiesonPar::mathieson_parameters TMutMathiesonPar::get_mathieson_parameters(
    unsigned short arm,
    unsigned short station,
    unsigned short octant,
    unsigned short gap
    )
{
    check_initialized();
    return _cap_coupling[ get_mathieson_index( arm, station, octant, gap ) ];
}

//__________________________________________________________
int TMutMathiesonPar::get_mathieson_index(
    int arm, int station, int octant, int gap)
{ return gap + MUTOO::NumberOfGaps*( octant + MUTOO::NumberOfOctants*(station + MUTOO::NumberOfStations*arm ) ); }

#endif

#endif
