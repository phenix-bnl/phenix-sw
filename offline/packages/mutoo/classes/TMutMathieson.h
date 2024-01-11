#ifndef __TMUTMATHIESON_H__
#define __TMUTMATHIESON_H__

// $Id: TMutMathieson.h,v 1.20 2011/12/24 04:48:21 slash Exp $
//////////////////////////////////////////////////////////////////
/*!
   \file    TMutMathieson.h
   \brief   Utility class for Mathison x-q mapping and inverse
   \author  S. Kelly
   \version $Revision: 1.20 $
   \date    $Date: 2011/12/24 04:48:21 $
*/
//////////////////////////////////////////////////////////////////

// BOOST/SL includes
#include<boost/array.hpp>
#include<cmath>
#include<climits>
#include<iostream>
#include<map>
#include<vector>

// MUTOO includes
#include "MUTOO.h"
#include "MUTOO_HASH_MAP.h"
#include "PHException.h"

// MUTGEOM includes
#include<MutStrip.h>

// Forward declerations
class ChargeToXFtor;
class XToChargeFtor;

/*! \ingroup classes */
//! Utility class that encapsulates q-xlocal mapping via the Mathieson distribution.
/*!
  This class provide the interface to a mapping from the charge
  distribution on two or three strip wide clusters to a local x and
  the inverse of this mapping.  The implementation makes use of
  lookups table that are initialized upon first.  There is a lookup
  table for each unique value of the stereo angle. By definition the
  stereo angle is the angle between the cathode strip and the anode
  wire.  *To Be Done* Correct treatment of strips on boundary of
  cathode plane -- these should exclude the capacitivly coupled
  charge from the non-existent neighbour.
*/
class TMutMathieson {

 public:

	//! shortcut for strip_number strip_charge pair
  typedef std::pair<unsigned short, double> strip_charge_pair;

  //! shortcut for strip_number strip_charge pair array
  typedef std::vector<strip_charge_pair> strip_charge_array;

  //! shortcut for strip_number strip_charge pair 3 wide array
  typedef boost::array<double,3> charge_array;

  /*!
    Distribute charge q_total via Mathieson centered at x_local relative to specified strip <br>
    x_local - cm <br>
    The return value is a std::vector of (strip number, charge) std::pairs.
  */
  static strip_charge_array get_charge_array(double q_total, double x_local, MutStrip* strip);

  /*!
    Return centroid of Mathieson distribute charge distribution.<br>
    Input is a boost array of (MutStrip*,charge) std::pairs.
  */
  static std::pair<double,double> get_x_local(const charge_array&, MutStrip* peak_strip_ptr=0);

  /*! Disable the use of multiple lookup tables for stereo strips */
  static void disable_stereo()
  {_no_stereo = true;}

  /*! Enable the use of multiple lookup tables for stereo strips */
  static void enable_stereo()
  {_no_stereo = false;}

  /*! Return stereo status */
  static bool is_stereo_disabled()
  { return _no_stereo; }

  /*! \brief
    initialize mathieson lookup table.
    Is done only once unless forced is set to true.
    Returns true if initialization is done.
  */
  static bool initialize( bool forced = false);

  //! returns true if initialisation was performed
  static bool initialized( void )
  { return _initialized; }

  //! Mathieson table entry
  struct mathieson_data {
    double x;
    double q1;
    double q2;
    double q3;
  };

  /*! \brief
    Table for Mathieson lookup. We use standard construct upon
    first use semantics for the Mathieson lookup table.  The table
    is actually n vectors of values, where n is the number of stereo
    angles in MUTR geometry. The vectors are stored in a std::hash_map
    where the key is a hash from the stereo angle to an integer.
  */
  typedef std::vector<TMutMathieson::mathieson_data> mathieson_data_vector;

  /*! \brief
    The key is a hash of the stereo angle
    The value is a vector of mathieson data structures
  */
  typedef MUTOO::hash_map<unsigned short, mathieson_data_vector>::type mathieson_data_map;

  private:

  //! returns index in hash table
  static unsigned short get_hash(unsigned short arm, unsigned short station, unsigned short gap, unsigned short cathode, unsigned short octant) {
    //since the cathode counting scheme in the geom is 0, 2 instead of 0, 1
    if (cathode==2) cathode=1;
    if(arm>MUTOO::NumberOfArms || station>MUTOO::NumberOfStations || gap>MUTOO::NumberOfGaps || cathode>MUTOO::NumberOfCathodePlanes || octant>MUTOO::NumberOfOctants )
    throw std::runtime_error( DESCRIPTION( "invalid argument TMutMathieson::get_hash" ) );

              return octant + MUTOO::NumberOfOctants*( cathode + MUTOO::NumberOfCathodePlanes *(
	                        gap + MUTOO::NumberOfGaps*( station + MUTOO::NumberOfStations*arm ) ) );

  }

  //! returns angle index matching angle in hash table
  static int get_angle_hash(double angle);

  /*! \brief
    Returns the vector of data values associated with the stereo angle
    of input strip.
  */
  static mathieson_data_vector& mathieson_lookup(MutStrip* strip_ptr);

  /*! \brief
    Initializes mathieson lookup tables.  One table is initialized for
    each value of the stereo angle (angle between anode and cathode) as
    per determined by the input geometry (rounded to the nearest degree).
  */
  static mathieson_data_map initialize_mathieson();

  //! verbosity mode
  static bool _verbose;

  //! true when initialization was done
  static bool _initialized;

  //! if true turn off multiple tables for stereo angles
  static bool _no_stereo;

  //! mathieson lookup table
  static mathieson_data_map _lookup;

};

//! get best position from charges using lookup
class ChargeToXFtor
{
 public:

  ChargeToXFtor(const TMutMathieson::charge_array& input_charges) :
    _input_charges(input_charges),
    _min_chi_sqr(1e37){;}

  void operator() (const TMutMathieson::mathieson_data& data)
  {

    // Sum the 3 input charges
    double q_total = _input_charges.at(0) +
      _input_charges.at(1) +
      _input_charges.at(2);

    // Calculate the Chi Square
    float chi_sqr =
      MUTOO::SQUARE(_input_charges.at(0)/q_total - data.q1) +
      MUTOO::SQUARE(_input_charges.at(1)/q_total - data.q2) +
      MUTOO::SQUARE(_input_charges.at(2)/q_total - data.q3);

    // Save the data associated with smallest Chi Square value
    if (chi_sqr < _min_chi_sqr) {
      _min_chi_sqr = chi_sqr;
      _min_data = data;
    }

  }

  double get_x()
  { return _min_data.x; }

  double get_chi_sqr()
  { return _min_chi_sqr; }

 private:

	//! charges
  const TMutMathieson::charge_array& _input_charges;

	//! best chisquare from lookup
	double _min_chi_sqr;

	//! best mathieson data from lookup
	TMutMathieson::mathieson_data _min_data;

};

//! get charges from position using lookup
class XToChargeFtor
{
 public:

  XToChargeFtor(double q_total, double x_local, MutStrip* strip) :
    _q_total(q_total),
    _x_local(x_local),
    _min_chi_sqr(1e37),
    _strip_ptr(strip)
    {;}

  void operator() (const TMutMathieson::mathieson_data& data)
  {
    // Calculate the Chi Square
    float chi_sqr = MUTOO::SQUARE(_x_local - data.x);

    // If this x is nearest to x_local, calculate charges.  Note
    // we could just save the index given that only one x is the
    // true min -- do calc_charges once -- worry about this later.
    if (chi_sqr < _min_chi_sqr) {
      _min_chi_sqr = chi_sqr;
      calc_charges(data);
    }
  }

  void calc_charges(const TMutMathieson::mathieson_data& data)
  {

    int strip_num = _strip_ptr->getStrip();

    // clear the array before calculating new charges
    _strip_charge_array.clear();

    // peak strip by definition exists
    _strip_charge_array.push_back(std::make_pair(strip_num, _q_total*data.q2));

    // For strips on the edge -- do not quite the right thing.  We should use
    // a different mathieson lookup tables for boundary strips, there is no
    // capacitive coupling between the peak and non-existent neighbour.
    if(strip_num == 0) {
      _strip_charge_array.push_back(std::make_pair(strip_num+1, _q_total*data.q3));
    } else if (strip_num == _strip_ptr->getNumberOfStrips()-1) {
      _strip_charge_array.push_back(std::make_pair(strip_num-1, _q_total*data.q1));
    } else {
      _strip_charge_array.push_back(std::make_pair(strip_num-1, _q_total*data.q1));
      _strip_charge_array.push_back(std::make_pair(strip_num+1, _q_total*data.q3));
    }
  }

  TMutMathieson::strip_charge_array get_strip_charge_array()
  { return _strip_charge_array; }

  private:

  //! total cluster charge
	double _q_total;

	//! cluster x position
  double _x_local;

	//! min chisquare
  double _min_chi_sqr;

	//! running pointer to strip object
  MutStrip* _strip_ptr;

	//! current mathieson lookup table
  TMutMathieson::strip_charge_array _strip_charge_array;

};

#endif



