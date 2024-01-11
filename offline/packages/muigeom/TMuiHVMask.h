// $Id: TMuiHVMask.h,v 1.2 2009/08/26 14:34:32 phnxbld Exp $

#ifndef TMuiHVMask_hh
#define TMuiHVMask_hh

/*!
  \file    TMuiHVMask.hh
  \brief   muid tube efficiencies
  \author  S.Kelly, H. Pereira
  \version $Revision: 1.2 $
  \date    $Date: 2009/08/26 14:34:32 $
*/

#include <PHTimeStamp.h>
#include <TDataType.h>
#include <string>
#include "MUIGEOM.h"

#ifndef __CINT__
#include "TMuiChannelId.hh"
#include "hash_vector.hh"
#endif

//! muid tube efficiencies
class TMuiHVMask
{
  
  public:

  //! virtual dtor to make compiler happy
  virtual ~TMuiHVMask() {}

  //! controls from where the efficiency are loaded
  enum Mode
  {
    //! load the efficiencies from file
    FROM_FILE,
      
    //! load the efficiencies from database
    FROM_DATABASE,
    
    //! use a unique efficiency value for all tubes 
    FIXED_VALUE
  };
  
  //! retrieves efficiency for a given two_pack
  static double get_effic_twopack(
    const UShort_t& arm,
    const UShort_t& plane,
    const UShort_t& panel,
    const UShort_t& orient,
    const UShort_t& twopack);
  
  //! retrieves efficiency for a given HV group
  /*! 
    this method returns the efficiency of the first two pack found in the group.
    It assumes that all twopacks in the group have the same efficiency
  */
  static double get_effic_hvgroup(
    const UShort_t& arm,
    const UShort_t& plane,
    const UShort_t& panel,
    const UShort_t& orient,
    const UShort_t& hvgroup);
  
  //! set efficiency for all two packs, used if _mode is FIXED_VALUE
  static void set_effic_twopack( const double& eff );
  
  //! mode (source of the efficiency values)
  static void set_mode( const Mode& mode )
  { _mode = mode; }
  
  //! search time stamp
  static void set_search_timestamp( const PHTimeStamp& stamp )
  { _time_stamp = stamp; }
  
  //! changes north efficiency filename
  static void set_filename_north(const char* filename) 
  { _filename_north = std::string(filename); }
  
  //! changes south efficiency filename
  static void set_filename_south(const char* filename) 
  { _filename_south = std::string(filename); }
  
  //! changes module verbosity
  void set_verbosity( const MUIGEOM::Verbosity& value )
  { _verbosity = value; }
  
  //! load efficiency 
  static bool initialize( void );

  //! update the database with loaded efficiencies
  static bool update_database( PHTimeStamp start, PHTimeStamp stop, const char* comments );

  //! update the specified filenames with loaded efficiencies
  static bool update_files( void );
  
  private:
  
  #ifndef __CINT__
    
  //! efficiency pair. 
  /*! 
    First is the efficiency
    second is the number of entries (tracks) used for the measurement. It is used
    to compute an error on the efficiency.
  */
  typedef std::pair< double, unsigned int > EfficiencyPair;
  
  //! map channel Id and efficiency
  typedef hashVector<TMuiChannelId,EfficiencyPair> private_map;
  
  //! channel Id and efficiency
  static private_map _map;  
    
  //! two tuve efficiency by hand
  static EfficiencyPair _effic_twopack;
  
  //! retrieves efficiency for a given two_pack
  static EfficiencyPair get_pair_twopack(
    const UShort_t& arm,
    const UShort_t& plane,
    const UShort_t& panel,
    const UShort_t& orient,
    const UShort_t& twopack);
  
  #endif
  
  //! load from file
  static void initialize_from_file( void );
  
  //! load from database
  static void initialize_from_database( void );

  //! dump plane averaged efficiencies
  /*! 
    the plane averaged efficiencies are calculated 
    from containing twopack efficiencies, weighted by 
    the number of entries in the measurement
  */
  static void dump_average_efficiencies( void );
  
  //! module verbosity
  static MUIGEOM::Verbosity _verbosity;
  
  //! controls mode
  static Mode _mode;
  
  //! time stamp to access the database
  static PHTimeStamp _time_stamp;
  
  //! north efficiency filename
  static std::string _filename_north;
  
  //! south efficiency filename
  static std::string _filename_south;
  
  //! true when initialization was done at least once.
  static bool _init_done;
  
  ClassDef(TMuiHVMask, 1)
};

#endif
