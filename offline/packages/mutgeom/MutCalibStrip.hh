#ifndef __MUTCALIBSTRIP_HH__
#define __MUTCALIBSTRIP_HH__

// $Id: MutCalibStrip.hh,v 1.13 2011/09/08 18:26:23 slash Exp $

/*!
  \file MutCalibStrip.hh
  \brief declare calibration data class that will store the
  calibration data and be able to retrieve it from the database.
*/

#include <PHTimeStamp.h>
#include <PHString.h>

#include <iostream>
#include <vector>
#include <set>
#include <PdbMutCalibStrip.hh>
#include <PdbMutCalibStrip_v2.hh>

#include "MUTGEOM.h"

#ifndef __CINT__
#include <boost/array.hpp>
#endif

//! strip calibration object ordering, based on unique id.
class OrderFunc
{
  public:

  //! predicate
  bool operator() (const PdbMutCalibStrip p1, const PdbMutCalibStrip p2) const
  { return ( p1.getUniqueId() < p2.getUniqueId() ); }
};

//______________________________________________________________________
// strip container which can handle PdbMutCalibStrip and PdbCalibStrip_v2 objects

class StripSet
{
public:

  StripSet() 
  {
    clear();
  }

  bool empty()
  {
    return (stripset_v1.empty() && stripset_v2.empty());
  }

  void clear()
  {
    stripset_v1.clear();
    stripset_v2.clear();
  }

  bool find(PdbMutCalibStrip strip)
  {
    return (stripset_v1.find( strip ) != stripset_v1.end());
  }
  bool find(PdbMutCalibStrip_v2 strip)
  {
    return (stripset_v2.find( strip ) != stripset_v2.end());
  }
  // add the read strip to the set
  // if some other strip, with the same indices are already
  // in the set, a warning will be issued
  void insert(PdbMutCalibStrip strip)
  {
    if (!find(strip))
      stripset_v1.insert( strip );
    else
      {
	printf("MutCalibStrip::dbGetAll - a strip with the same indices has already been inserted\n");
	strip.print();
      }
  }
  void insert(PdbMutCalibStrip_v2 strip)
  {
    if (!find(strip))
      stripset_v2.insert( strip );
    else
      {
	printf("MutCalibStrip::dbGetAll - a strip with the same indices has already been inserted\n");
	strip.print();
      }    
  }
  void erase(PdbMutCalibStrip strip)
  {
    stripset_v1.erase( strip );
  }
  void erase(PdbMutCalibStrip_v2 strip)
  {
    stripset_v2.erase( strip );
  }

  const PdbMutCalibStrip* get_v1(size_t i) const
  {
    StripSet_v1::iterator iter = stripset_v1.begin();
    for (size_t ni=0; ni<i; ni++) iter++; 
    return &(*iter);
  } 

  const PdbMutCalibStrip_v2* get_v2(size_t i) const
  {
    StripSet_v2::iterator iter = stripset_v2.begin();
    for (size_t ni=0; ni<i; ni++) iter++; 
    return &(*iter);
  } 

  const PdbMutCalibStrip* get(PdbMutCalibStrip strip) const
  {
    return &(*stripset_v1.find( strip ));
  } 
  const PdbMutCalibStrip_v2* get(PdbMutCalibStrip_v2 strip) const
  {
    return &(*stripset_v2.find( strip ));
  } 
  
  int size() const
  {
    return int(stripset_v1.size())+int(stripset_v2.size());
  }

protected:
    //! strip set
    typedef std::set< PdbMutCalibStrip, OrderFunc > StripSet_v1;
    typedef std::set< PdbMutCalibStrip_v2, OrderFunc > StripSet_v2;

  StripSet_v1 stripset_v1;
  StripSet_v2 stripset_v2;
};

//! strip calibration interface
class MutCalibStrip
{

 public:

  //! constructor
  MutCalibStrip();

  //! destructor
  ~MutCalibStrip();

  //! decide if gain corrections must be applied when reading from DB
  void setUseGainCorrections( bool value )
  { _use_gain_corrections = value; }

  //! decide if we should use new calibration method (developed 2011) or old method
  void setUseNewCalibrationMethod( bool value )
  { _use_new_calibration_method = value;  }

  //! decide if new or old calibration constants should be used
  bool getUseNewCalibrationMethod( void ) const
  { return _use_new_calibration_method; }

  //! decide if gain corrections must be applied when reading from DB
  bool getUseGainCorrections( void ) const
  { return _use_gain_corrections; }

  //! read calibration objects from database
  int dbGetAll( int runnumber);

  //! read calibration objects from database
  int dbGetAll(PHTimeStamp tsearch);

  //! write calibration objects to database
  int dbPutAll( int beginrun, int endrun, PHString descriptor) const;

  //! write calibration objects to database
  int dbPutAll(PHTimeStamp start, PHTimeStamp stop, PHString descriptor) const;

  //! read calibration from ASCII text
  int txtGetAll( const char* infile);

  //! write calibration to ASCII text
  int txtPutAll( const char* outfile) const;

  //! number of Calibration objects to store
  int getNumberOfStrips()
  { return _strips.size(); }

  //! swaps calibration for two strips
  void swap( PdbMutCalibStrip first, PdbMutCalibStrip second );

  //! reset
  void reset( void )
  { _strips.clear(); }

  //! access to the individual calib objects
  /*! changing these afterwards does not change the contents in the set */
  const PdbMutCalibStrip* getPdbMutCalibStrip(
    const int& arm,
    const int& station,
    const int& octant,
    const int& halfoctant,
    const int& gap,
    const int& plane,
    const int& strip
    ) const;

  /*!
  put in other strip values,
  overwriting if strip is already existing in set
  */
  void putPdbMutCalibStrip(const PdbMutCalibStrip* );

  void putPdbMutCalibStrip(const PdbMutCalibStrip_v2* );

  //! initialize gain corrections for Run8 and before
  /*!
  note:
  - this is called automatically from dbGetAll
  - it must be called explicitely if planning to use txtGetAll.
  */
  void initialize_gain_corrections_run8( void );

  //! initialize gain corrections for Run9 and later
  /*!
  note:
  - this is called automatically from dbGetAll
  - it must be called explicitely if planning to use txtGetAll.
  */
  void initialize_gain_corrections_run9( void );

  //! initialize gain corrections for Run11 and later
  /*!
  note:
  - this is called automatically from dbGetAll
  - it must be called explicitely if planning to use txtGetAll.
  */
  void initialize_gain_corrections_run11( void );

  //! print
  void print( std::ostream& out = std::cout ) const;    
    
  protected:

  //! set of associated calibration objects
  StripSet _strips;

  #ifndef __CINT__
  //!@name gain corrections
  //@{

  //! get gain correction index for given arm, station, gap and octant
  int get_index( int arm, int station, int octant, int gap  )
  { return gap + MUTGEOM::NumberOfGaps*( octant + MUTGEOM::NumberOfOctants*( station + MUTGEOM::NumberOfStations*arm ) ); }

  //! get gain correction for given arm, station, gap and octant
  float get_gain_correction( int arm, int station, int octant, int gap  )
  { return _gain_corrections[get_index( arm, station, octant, gap)]; }

  //! number of arm, station, gap, octants
  /* (ignoring the fact that station 3 has only 2 gaps) */
  enum { NUM_OCTANTS = 144 };

  //! use gain corrections when loading the calibrations from database
  bool _use_gain_corrections;

  //! use gain corrections when loading the calibrations from database
  bool _use_new_calibration_method;

  //! octant by octant calibration gain scale correction
  boost::array<float, NUM_OCTANTS> _gain_corrections;

  //@}
  void apply_gain_corrections(PdbMutCalibStrip& strip);

  #endif

};
#endif /* __MUTCALIBSTRIP_HH__ */
