#ifndef __TMUTDATABASECNTRL_H__
#define __TMUTDATABASECNTRL_H__

// $Id: TMutDatabaseCntrl.h,v 1.24 2013/06/17 18:34:00 slash Exp $

#include <TDataType.h>
#include <iostream>
#include <sstream>
#include <string>
#include <map>
#include <set>
#include <cstdlib>

#include <TMutDatabaseInit.h>


/*! \ingroup classes */
//! Statically scoped class for managing which files are retreived from the database
/*!
  Statically scoped class for managing which files are retreived from the database
*/

class TMutDatabaseCntrl
{
 public:

  //! verbosity enumerations
  enum Verbosity {NONE=0,SOME=1,ALOT=2,MAX=3};

  //! calibration method enumerations
  enum CalibrationMethod {PIECEWISE=0,CURVE=1};

  //! DB type enumeration
  enum DBType
  {
    GLOBAL_ALIGN,
    INTERNAL_ALIGN,
    OCTANT_SURVEY,
    STRIP_GEOMETRY,
    STRIP_SPACING,
    WIRE_GEOMETRY,
    ATTENUATED_CHANNELS,
    DISABLED_WIRES,
    DISCONNECTED_WIRES,
    DCM_CHANNELS,
    DEAD_CHANNELS,
    CALIBRATIONS,
    LANDAU_PARAMETERS
  };

  /*! Returns true if database access for this file or procedure is set */
  static bool get_database_access(const std::string& tag);

  /*! decides if given file is to be accessed or not at fetching stage */
  static void set_database_access(const std::string& tag, bool flag)
  {
    _get().check_flag( "TMutDatabaseCntrl::set_database_access", tag );
    _get()._flag_map[tag]=flag;
  }

  /*! decides if given file is to be accessed or not at fetching stage */
  static void set_database_access(const char *tag, bool flag)
  { set_database_access( std::string( tag ), flag ); }

  /*! dump file access flags */
  static void print(std::ostream& os = std::cout);

  //! sets calibration method: curve or piecewise
  static void set_CalibrationMethod( CalibrationMethod value )
  { _get()._calibrationmethod = value; }

  //! retrieves calibration method: curve or piecewise
  static CalibrationMethod get_CalibrationMethod( void )
    {
      // before Run11, there was no PIECEWISE calibration
      PHTimeStamp timeStamp = TMutDatabaseInit::get_time_stamp();
      static PHTimeStamp time_run11(2011,1,1,0,0,0);
      if (timeStamp < time_run11)
	return CURVE;
      
      return _get()._calibrationmethod;
    }

  //! sets verbosity for db fetching
  static void set_verbosity( Verbosity value )
  { _get()._verbosity = value; }

  //! retrieves verbosity for db fetching
  static Verbosity get_verbosity( void )
  { return _get()._verbosity; }

  //! sets verbosity for db fetching
  static void set_verbosity( DBType type, Verbosity value )
  { _get()._verbosity_map[type] = value; }

  //! retrieves verbosity for db fetching
  static Verbosity get_verbosity( DBType type )
  {
    Private::VerbosityMap::const_iterator iter( _get()._verbosity_map.find( type ) );
    return iter == _get()._verbosity_map.end() ? _get()._verbosity: iter->second;
  }

  //! set "local" filename to be used in place of database access
  static void set_filename( const char* tag, const char* filename )
  {
    if( tag )
    { set_filename( std::string( tag ), std::string( filename ) ); }
  }

  //! set "local" filename to be used in place of database access
  static void set_filename( const std::string& tag, const std::string& filename )
  {
    _get().check_flag( "TMutDatabaseCntrl::set_filename", tag );
    _get()._file_map[tag] = filename;
  }

  //! get "local" filename to be used in place of database access
  static std::string get_filename( const std::string& tag );

  /*!
    Set mut.internalAligConsts.dat file name
    To be used after setting the data base access of "use_local_internal_align_file"
  */
  static void set_internalAlig_file_name(const char* filename )
  { set_filename( "use_local_internal_align_file", filename ); }

  /*! Get mut.internalAligConsts.dat file name */
  static std::string get_internalAlig_file_name( void )
  { return get_filename( "use_local_internal_align_file" ); }

  /*!
    Set mut.disabledAnodes.dat file name
      	To be used after setting the data base access of "use_local_dead_HV_file"
  */
  static void set_disabledAnodes_file_name(const char* filename)
  { set_filename( "use_local_dead_HV_file", filename ); }

  /*! Get mut.internalAligConsts.dat file name */
  static std::string get_disabledAnodes_file_name( void )
  { return get_filename( "use_local_dead_HV_file" ); }

  protected:

  //! stores all members in a unique 'Private' class
  class Private
  {

    public:

    //! constructor
    Private( void ):
      _verbosity( TMutDatabaseCntrl::NONE )
    {
      initialize_valid_flags();
      initialize_map();
    }

    //! set of valid tag strings, and conveniance "<<" operator
    class FlagSet: public std::set<std::string>
    {

      public:

      FlagSet& operator << (const std::string& value )
      {
        insert( value );
        return *this;
      }

    };

    //! check flag against list
    void check_flag( const std::string& function_name, const std::string tag )
    {

      if( _valid_flags.find( tag ) == _valid_flags.end() )
      {

        std::cout
          << function_name
          << " - Flag: " << tag << " is invalid. "
          << std::endl;

        std::cout
          << function_name
          << " - Check spelling or offline/packages/mutgeom/TMutDatabaseCntrl code."
          << std::endl;
        exit(0);

      }

    }

    //! shortcut for tag/flags maps
    typedef std::map<std::string,bool> FlagMap;

    //! shortcut for tag/filename maps
    typedef std::map<std::string, std::string > FileMap;

    //! verbosity
    Verbosity _verbosity;

    CalibrationMethod _calibrationmethod;

    //! verbosity
    typedef std::map<DBType, Verbosity> VerbosityMap;
    VerbosityMap _verbosity_map;

    //! map of tags and access flags
    FlagMap _flag_map;

    //! map of tags and filenames
    FileMap _file_map;

    protected:

    //! initialize list of valid flags
    void initialize_valid_flags( void );

    //! Call out which files/geometry states can be modified using the database
    void initialize_map();

    private:

    //! list of valid tags
    /*!
    an error is printed out when a flag is used at runtime that is not in this list.
    This prevents users to make typos and not getting the result they expect
    */
    FlagSet _valid_flags;

  };

  //! return Private singleton
  static Private& _get( void );

};

#endif	//__TMUTDATABASECNTRL_H__
