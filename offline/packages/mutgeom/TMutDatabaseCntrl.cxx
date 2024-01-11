// $Id: TMutDatabaseCntrl.cxx,v 1.27 2016/03/21 18:26:18 shlim Exp $
#include "TMutDatabaseCntrl.h"
#include "MUTGEOM.h"

using namespace std;

//____________________________________________________
TMutDatabaseCntrl::Private& TMutDatabaseCntrl::_get( void )
{
  static Private singleton;
  return singleton;
}

//____________________________________________________
bool TMutDatabaseCntrl::get_database_access(const std::string& tag)
{
  _get().check_flag( "TMutDatabaseCntrk::get_database_access",  tag );
  Private::FlagMap::const_iterator iter = _get()._flag_map.find(tag);
  return (iter == _get()._flag_map.end()) ? false: iter->second;
}

//____________________________________________________
std::string TMutDatabaseCntrl::get_filename(const std::string& tag)
{
  _get().check_flag( "TMutDatabaseCntrk::get_filename", tag );
  Private::FileMap::const_iterator iter = _get()._file_map.find(tag);
  if( iter == _get()._file_map.end() )
  {
    cout << "TMutDatabaseCntrl::get_filename - tag " << tag << " not found" << endl;
    return "";
  } else return iter->second;
}

//____________________________________________________
void TMutDatabaseCntrl::print(std::ostream& os)
{

  MUTGEOM::PRINT( os, "TMutDatabaseCntrl::print" );
  os << "flags:" << endl;
  for( Private::FlagMap::const_iterator iter = _get()._flag_map.begin(); iter != _get()._flag_map.end(); ++iter )
  os << iter->first << ": " << ((iter->second) ? "true":"false") << std::endl;
  os << endl;

  os << "filenames:" << endl;
  for( Private::FileMap::const_iterator iter = _get()._file_map.begin(); iter != _get()._file_map.end(); ++iter)
  os << iter->first << ": " << iter->second << std::endl;
  os << endl;

  MUTGEOM::PRINT( os, "**" );

}

//____________________________________________________
void TMutDatabaseCntrl::Private::initialize_valid_flags( void )
{

  // initialize valid flags
  _valid_flags
    << "arms_from_database"
    << "attenuated_channels"
    << "dead_channels"
    << "disable_HV"
    << "internal_alignment_corrections"
    << "global_alignment_corrections"
    << "Run02"
    << "use_local_attenuated_chnl_file"
    << "use_local_calibration_file"
    << "use_local_dcm_map_file"
    << "use_local_dead_channel_file"
    << "use_local_dead_HV_file"
    << "use_local_dead_wire_file"
    << "use_local_global_align_file"
    << "use_local_internal_align_file"
    << "use_local_landau_parameters_file"
    << "use_local_octant_survey_file"
		<< "use_local_st1_autocad_file"
    << "use_st3_geometry_fix";

}

//____________________________________________________
void TMutDatabaseCntrl::Private::initialize_map( void )
{

  // geometry
  _flag_map.insert( std::make_pair( "use_st3_geometry_fix", true ) );
  _flag_map.insert( std::make_pair( "use_local_octant_survey_file", false ) );
  _file_map.insert( std::make_pair( "use_local_octant_survey_file", "MutOctantPositions.dat" ) );

  //alignment
  _flag_map.insert( std::make_pair( "global_alignment_corrections", true ) );
  _flag_map.insert( std::make_pair( "internal_alignment_corrections", true ) );

  _flag_map.insert( std::make_pair( "use_local_global_align_file", false ) );
  _flag_map.insert( std::make_pair( "use_local_internal_align_file", false ) );

  _file_map.insert( std::make_pair( "use_local_global_align_file", "mut.globalAligConsts.dat" ) );
  _file_map.insert( std::make_pair( "use_local_internal_align_file", "mut.internalAligConsts.dat" ) );


  // landau parameters
  _flag_map.insert( std::make_pair( "use_local_landau_parameters_file", false ) );
  _file_map.insert( std::make_pair( "use_local_landau_parameters_file", "landau_parameters.txt" ) );

  // calibrations
  _flag_map.insert( std::make_pair( "use_local_calibration_file", false ) );
  _file_map.insert( std::make_pair( "use_local_calibration_file", "mutcalibstrips.txt" ) );

  // DCM channels
  _flag_map.insert( std::make_pair( "dead_channels", true ) );
  _flag_map.insert( std::make_pair( "use_local_dcm_map_file", false ) );
  _flag_map.insert( std::make_pair( "use_local_dead_channel_file", false ) );
  _file_map.insert( std::make_pair( "use_local_dead_channel_file","DeadChannels.dat" ) );

  //strips with attenuated charge
  _flag_map.insert( std::make_pair( "attenuated_channels", true ) );
  _flag_map.insert( std::make_pair( "use_local_attenuated_chnl_file", false ) );
  _file_map.insert( std::make_pair( "use_local_attenuated_chnl_file", "AttenuatedChannels.txt" ) );

  //disable anode cards and individual wires for simulation
  _flag_map.insert( std::make_pair( "disable_HV", true ) );
  _flag_map.insert( std::make_pair( "use_local_dead_HV_file", false ) );
  _flag_map.insert( std::make_pair( "use_local_dead_wire_file", false ) );

  _file_map.insert( std::make_pair( "use_local_dead_HV_file", "mut.disabledAnodes.dat") );
  _file_map.insert( std::make_pair( "use_local_dead_wire_file", "mut.disabledWires.dat") );

	//autocad file for st1
	_flag_map.insert( std::make_pair( "use_local_st1_autocad_file", false ) );

  //geometry database access
  //Only for debugging since only the survey data in the database is
  //maintained and guaranteed.
  _flag_map.insert( std::make_pair("arms_from_database",true));

  // Define control parameters for run-year. xhe GSU
  _flag_map.insert( std::make_pair("Run01",false));
  _flag_map.insert( std::make_pair("Run02",false));
  _flag_map.insert( std::make_pair("Run03",false));
  _flag_map.insert( std::make_pair("Run04",false));

}

