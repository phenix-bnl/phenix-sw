// $Id: TFvtxDatabaseCntrl.h,v 1.3 2014/03/07 16:19:30 jinhuang Exp $                                                                                             

/*!
 * \file TFvtxDatabaseCntrl.h
 * \brief 
 * \author Jin Huang <jhuang@bnl.gov>
 * \version $Revision: 1.3 $
 * \date $Date: 2014/03/07 16:19:30 $
 */

#ifndef TFVTXDATABASECNTRL_H_
#define TFVTXDATABASECNTRL_H_

#include <TDataType.h>
#include <iostream>
#include <sstream>
#include <string>
#include <map>
#include <set>
#include <cstdlib>

#include <PHTimeStamp.h>

#include "TFvtxGlobalParCntrl.h"

/*!
 * \brief This class is obsolete. Please use TFvtxGlobalParCntrl instead
 */
class TFvtxDatabaseCntrl
{

public:
  static void obsolete_warning(const std::string & suggestion);

  /*! get flag for database access. Valid flags must be defined in Private::initialize_map() */
  static bool
  get_flag(const std::string& tag) {
    obsolete_warning("get_bool_par()");
    return TFvtxGlobalParCntrl::get_bool_par(tag);
  }

  /*! set flag for database access. Valid flags must be defined in Private::initialize_map() */
  //! Refer details to https://www.phenix.bnl.gov/WWW/offline/wikioff/index.php/FVTX/calibration_database#Flags
  static void
  set_flag(const std::string& tag, bool flag){
    obsolete_warning("set_bool_par()");
     TFvtxGlobalParCntrl::set_bool_par(tag,flag);
  }

  /*! get flag for database access. Valid flags must be defined in Private::initialize_map() */
  //! Refer details to https://www.phenix.bnl.gov/WWW/offline/wikioff/index.php/FVTX/calibration_database#Flags
  static void
  set_flag(const char *tag, bool flag)
  {
    set_flag(std::string(tag), flag);
  }

  /*! dump file access flags */
  static void
  print(std::ostream& os = std::cout)
  {
    obsolete_warning("print()");
    TFvtxGlobalParCntrl::print(os);
  }

  //! set "local" filename to be used in place of database access
  //! Refer details to https://www.phenix.bnl.gov/WWW/offline/wikioff/index.php/FVTX/calibration_database#Flags
  static void
  set_filename(const char* tag, const char* filename){
    obsolete_warning("set_string_par()");
     TFvtxGlobalParCntrl::set_string_par(tag,filename);
  }

  //! set "local" filename to be used in place of database access
  //! Refer details to https://www.phenix.bnl.gov/WWW/offline/wikioff/index.php/FVTX/calibration_database#Flags
  static void
  set_filename(const std::string& tag, const std::string& filename){
    obsolete_warning("set_string_par()");
     TFvtxGlobalParCntrl::set_string_par(tag,filename);
  }

  //! get "local" filename to be used in place of database access
  static std::string
  get_filename(const std::string& tag){
    obsolete_warning("get_string_par()");
    return TFvtxGlobalParCntrl::get_string_par(tag);
  }

  //! set_run_number for database reading. Otherwise, will get from recoConsts
  static void
  set_pdb_run_number(int runnumber){
    obsolete_warning("set_pdb_run_number()");
     TFvtxGlobalParCntrl::set_pdb_run_number(runnumber);
  }

  //! get run number for database reading
  static int
  get_pdb_run_number(){
    obsolete_warning("get_pdb_run_number()");
     return TFvtxGlobalParCntrl::get_pdb_run_number();
  }

  //! get time stamp for database reading
  static const PHTimeStamp &
  get_pdb_time()
  {

    obsolete_warning("get_pdb_time()");
     return TFvtxGlobalParCntrl::get_pdb_time();

  }

protected:
//
//  //! set time stamp for database reading according to run number
//  static void
//  sync_pdb_time();
//
//  //! stores all members in a unique 'Private' class
//  class Private
//  {
//
//  public:
//
//    //! constructor
//    Private(void) :
//        _set_run_num(0), _run_num(0), _pdb_time()
//    {
//      initialize_map();
//    }
//
//    //! set of valid tag strings, and conveniance "<<" operator
//    class FlagSet : public std::set<std::string>
//    {
//
//    public:
//
//      FlagSet&
//      operator <<(const std::string& value)
//      {
//        insert(value);
//        return *this;
//      }
//
//    };
//
//    //! check flag against list
//    void
//    check_flag(const std::string& function_name, const std::string tag)
//    {
//
//      if (_flag_map.find(tag) == _flag_map.end())
//        {
//
//          std::cout << function_name << " - Flag: " << tag << " is invalid. "
//              << std::endl;
//
//          std::cout << function_name
//              << " - Check spelling at offline/packages/fvtxgeom/TFvtxDatabaseCntrl::Private::initialize_map()"
//              << std::endl;
//          exit(0);
//
//        }
//
//    }
//
//    //! check flag against list
//    void
//    check_filename(const std::string& function_name, const std::string tag)
//    {
//
//      if (_file_map.find(tag) == _file_map.end())
//        {
//
//          std::cout << function_name << " - Flag: " << tag << " is invalid. "
//              << std::endl;
//
//          std::cout << function_name
//              << " - Check spelling at offline/packages/fvtxgeom/TFvtxDatabaseCntrl::Private::initialize_map()"
//              << std::endl;
//          exit(0);
//
//        }
//
//    }
//
//    //! shortcut for tag/flags maps
//    typedef std::map<std::string, bool> FlagMap;
//
//    //! shortcut for tag/filename maps
//    typedef std::map<std::string, std::string> FileMap;
//
//    //! map of tags and access flags
//    FlagMap _flag_map;
//
//    //! map of tags and filenames
//    FileMap _file_map;
//
//    //! user-set run number for database loading. _set_run_num = 0 means use recoConsts
//    int _set_run_num;
//
//    //! number for database loading
//    int _run_num;
//
//    //! time stamp for database loading
//    PHTimeStamp _pdb_time;
//
//  protected:
//
//
//    //! \brief init flags map an file map, any flag must be first defined here
//    //!
//    //! Please refer details to and update
//    //! https://www.phenix.bnl.gov/WWW/offline/wikioff/index.php/FVTX/calibration_database#Flags
//    void
//    initialize_map();
//
//  };
//
//  //! return Private singleton
//  static Private&
//  _get(void);
};

#endif /* TFVTXDATABASECNTRL_H_ */
