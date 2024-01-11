// $Id: TFvtxDatabaseCntrl.cxx,v 1.7 2014/03/07 16:19:30 jinhuang Exp $                                                                                             

/*!
 * \file TFvtxDatabaseCntrl.cxx
 * \brief 
 * \author Jin Huang <jhuang@bnl.gov>
 * \version $Revision: 1.7 $
 * \date $Date: 2014/03/07 16:19:30 $
 */

#include "FVTXGEOM.h"

#include <recoConsts.h>
#include <RunToTime.hh>
#include <PdbCalBank.hh>
#include <PdbBankManager.hh>
#include <PdbApplication.hh>

#include <stdexcept>
#include <unistd.h> // for sleep

#include "TFvtxDatabaseCntrl.h"

using namespace std;

void
TFvtxDatabaseCntrl::obsolete_warning(const std::string & suggestion)
{
  std::cout<<std::endl;
  std::cout << "TFvtxDatabaseCntrl - WARNING - TFvtxDatabaseCntrl is obsolete."
      << " Please use TFvtxGlobalParCntrl::" << suggestion << " instead"
      << std::endl;
  std::cout<<std::endl;
  sleep(3);
}

//
////____________________________________________________
//TFvtxDatabaseCntrl::Private&
//TFvtxDatabaseCntrl::_get(void)
//{
//  static Private singleton;
//  return singleton;
//}
//
////____________________________________________________
//bool
//TFvtxDatabaseCntrl::get_flag(const std::string& tag)
//{
//  _get().check_flag("TMutDatabaseCntrk::get_database_access", tag);
//  Private::FlagMap::const_iterator iter = _get()._flag_map.find(tag);
//  return (iter == _get()._flag_map.end()) ? false : iter->second;
//}
//
////____________________________________________________
//std::string
//TFvtxDatabaseCntrl::get_filename(const std::string& tag)
//{
//  _get().check_filename("TMutDatabaseCntrk::get_filename", tag);
//  Private::FileMap::const_iterator iter = _get()._file_map.find(tag);
//  if (iter == _get()._file_map.end())
//    {
//      cout << "TFvtxDatabaseCntrl::get_filename - tag " << tag << " not found"
//          << endl;
//      return "";
//    }
//  else
//    return iter->second;
//}
//
////____________________________________________________
//void
//TFvtxDatabaseCntrl::print(std::ostream& os)
//{
//
//  FVTXGEOM::PRINT(os, "TFvtxDatabaseCntrl::print");
//
//  os << "run number:" << get_pdb_run_number() <<" @ "<< get_pdb_time()  <<endl <<endl;
//
//  os << "flags:" << endl;
//  for (Private::FlagMap::const_iterator iter = _get()._flag_map.begin();
//      iter != _get()._flag_map.end(); ++iter)
//    os << iter->first << ": " << ((iter->second) ? "true" : "false")
//        << std::endl;
//  os << endl;
//
//  os << "filenames:" << endl;
//  for (Private::FileMap::const_iterator iter = _get()._file_map.begin();
//      iter != _get()._file_map.end(); ++iter)
//    os << iter->first << ": " << iter->second << std::endl;
//  os << endl;
//
//  os << "Flags are defined in TFvtxGlobalParStorage::initialize_parameters()  " << endl;
//  os << "      and documented at https://www.phenix.bnl.gov/WWW/offline/wikioff/index.php/FVTX/Global_Parameters" << endl;
//
//  FVTXGEOM::PRINT(os, "**");
//
//}
//
////____________________________________________________
////! set_run_number for database reading
//void
//TFvtxDatabaseCntrl::set_pdb_run_number(int runnumber)
//{
//  _get()._set_run_num = runnumber;
//  _get()._run_num = runnumber;
//  sync_pdb_time();
//}
//
////____________________________________________________
////! set time stamp for database reading
//void
//TFvtxDatabaseCntrl::sync_pdb_time()
//{
//
//  RunToTime* runTime = RunToTime::instance();
//  if (!runTime)
//    {
//      cout << "TFvtxDatabaseCntrl::set_pdb_run_number - Error - Can not get an instance for RunToTime"<< endl;
//      return;
//    }
//
//  PHTimeStamp *ts(runTime->getBeginTime(_get()._run_num));
//  if (!ts)
//    {
//      cout << "TFvtxDatabaseCntrl::set_pdb_run_number - Error - Can not get start time stamp for run "
//          <<_get()._run_num<<". The current time stamp will be used!!"<< endl;
//      return;
//    }
//
//  PHTimeStamp Tstart = *ts;
//  delete ts;
//
//  _get()._pdb_time = (Tstart);
//
//  cout << "TFvtxDatabaseCntrl::set_pdb_run_number - will use run "
//      << get_pdb_run_number() << " for FVTX databaser ops. Time stamp = "
//      << Tstart << endl;
//}
//
////____________________________________________________
////! get time stamp for database reading
//const PHTimeStamp &
//TFvtxDatabaseCntrl::get_pdb_time()
//{
//  // update runnumber/time stamp if necessary
//  get_pdb_run_number();
//
//  return _get()._pdb_time;
//}
//
//int
//TFvtxDatabaseCntrl::get_pdb_run_number()
//{
//
//  if (_get()._set_run_num > 0)
//    {
//      // use custom set run number
//      return _get()._set_run_num;
//    }
//  else
//    {
//      // use automatic run number, sync time stamp is a new run number is set in recoConsts
//
//      recoConsts *rc = recoConsts::instance();
//
//      int run_num = rc->get_IntFlag("RUNNUMBER");
//
//      if (run_num <= 0)
//        {
//          throw runtime_error(
//              "TFvtxDatabaseCntrl::get_pdb_run_number - Can not find the run number in recoConsts");
//        }
//
//      if (_get()._run_num != run_num)
//        {
//          _get()._run_num = run_num;
//
//          sync_pdb_time();
//        }
//      return _get()._run_num;
//    }
//}
//
////____________________________________________________
////! \brief init flags map an file map, any flag must be first defined here
////!
////! Please refer details to and update
////! https://www.phenix.bnl.gov/WWW/offline/wikioff/index.php/FVTX/calibration_database#Flags
//void
//TFvtxDatabaseCntrl::Private::initialize_map(void)
//{
//  // valid flags must be declared here first
//
//  //generic
//  _flag_map.insert(std::make_pair("is_sim", false));
//
//  // dead maps
//  _flag_map.insert(std::make_pair("deadmap_auto_load", true));
//  _flag_map.insert(std::make_pair("deadmap_use_calibration_database",true));
//  _flag_map.insert(std::make_pair("deadmap_use_production_map_for_sim", false));
//  _file_map.insert(std::make_pair("deadmap_fvtxdb_use_custom_deadmap", ""));
//
//  // geometry/alignment
//  _flag_map.insert(std::make_pair("geom_auto_load", true));
//  _flag_map.insert(std::make_pair("geom_use_calibration_database", true));
//  _flag_map.insert(std::make_pair("geom_calibration_database_TGeo_format", false));
//  _file_map.insert(std::make_pair("geom_root_file_name", "fvtxgeom.root"));
//  _file_map.insert(
//      std::make_pair("geom_root_file_path",
//          "/afs/rhic.bnl.gov/phenix/PHENIX_LIB/simulation/run12/"));
//
//
//}
