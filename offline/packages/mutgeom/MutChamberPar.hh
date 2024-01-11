#ifndef __MUTCHAMBERPAR_HH__
#define __MUTCHAMBERPAR_HH__

// $Id: MutChamberPar.hh,v 1.4 2013/08/05 18:01:02 slash Exp $

/*!
  \file MutChamberPar.hh
  \brief declare calibration data class that will store the
  calibration data and be able to retrieve it from the database.
*/

#include <PHTimeStamp.h>
#include <PHString.h>

#include <iostream>
#include <map>
#include <PdbMutChamberPar.hh>


//! Chamber parameters for simulation
class MutChamberPar
{

 public:

  //! return singleton
  static MutChamberPar& get( void );

  //! return singleton
  static MutChamberPar& instance( void )
  {
    return get();
  }

  //! destructor
  virtual ~MutChamberPar( void );

  //! initialize
  void initialize();

  //! true if initialized
  bool initialized( void ) const
  { return _initialized; }

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
  int getNumberOfChambers()
  { return _chambers.size(); }

  //! reset
  void reset( void )
  { 
    _chambers.clear(); 
    _initialized = false;
  }

  //! access to the individual calib objects
  /*! changing these afterwards does not change the contents in the set */
  const PdbMutChamberPar* getPdbMutChamberPar(const int& arm,
						const int& station,
						const int& octant,
						const int& halfoctant,
						const int& gap,
						const int& plane);

  /*!
  put in other strip values,
  overwriting if strip is already existing in set
  */
  void putPdbMutChamberPar(const PdbMutChamberPar* );

  //! load text file with time offset and time offset rms
  //! text format: arm station gap cathode time_offset time_offset_rms
  void load_time_offset( std::string filename );
  
  //! load text with additional pedestal
  //! text format: arm station gap cathode pedestal
  void load_pedestal( std::string filename );

  //! load text with Landau parameters
  //! text format: arm station octant halfoctant gap cathode Landau_offset Landau_scale
  void load_Landau_parameters( std::string filename );

  //! load text with Mathieson parameters
  //! text format: arm station octant gap cathode_coupling anode_coupling
  void load_Mathieson_parameters( std::string filename );

  //! load text with cathode error
  //! text format: arm station gap cathode cathode_error
  void load_cathode_error( std::string filename );

  //! print
  void print( std::ostream& out = std::cout ) const;    
  
protected:

  //! constructor
  MutChamberPar();

  //! initialization flag
  void set_initialized( bool value )
  { _initialized = value; }

private:

  //! chamber container
  std::map<int, PdbMutChamberPar> _chambers;

  bool _initialized;
};
#endif /* __MUTCHAMBERPAR_HH__ */
