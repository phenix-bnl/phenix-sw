// $Id: TFvtxGlobalParCntrl.h,v 1.3 2015/02/06 16:40:20 jinhuang Exp $

/*!
 * \file TFvtxGlobalParCntrl.h
 * \brief 
 * \author Jin Huang <jhuang@bnl.gov>
 * \version $Revision: 1.3 $
 * \date $Date: 2015/02/06 16:40:20 $
 */

#ifndef TFvtxGlobalParCntrl_H_
#define TFvtxGlobalParCntrl_H_

#include <TObject.h>
#include <iostream>
#include <sstream>
#include <string>
#include <map>
#include <set>
#include <cstdlib>
#include <stdexcept>
#include <sstream>
#include <PdbCalParameters.hh>

#include <PHTimeStamp.h>
#include <PHString.h>

/*!
 * \brief Manages database IO
 *
 * duplicated from TFvtxGlobalParCntrl
 */
class TFvtxGlobalParCntrl
{

public:

  //!@name access the parameters
  //@{

  /*! Print all parameters */
  static void
  print(std::ostream& os = std::cout)
  {
    get_parameters().print(os);
  }

  /*! get boolean parameters.
   * Valid flags must be defined in Parameters::initialize_parameters()
   *  Refer details to https://www.phenix.bnl.gov/WWW/offline/wikioff/index.php/FVTX/Global_Parameters */
  static bool
  get_bool_par(const std::string& name)
  {
    get_parameters().check_bool_par(name);
    return get_parameters()._bool_par_map[name];
  }

  /*! set boolean parameters.
   * Valid flags must be defined in Parameters::initialize_parameters()
   *  Refer details to https://www.phenix.bnl.gov/WWW/offline/wikioff/index.php/FVTX/Global_Parameters */
  static void
  set_bool_par(const std::string& name, bool val)
  {
    get_parameters().check_bool_par(name);
    get_parameters()._bool_par_map[name] = val;
    get_parameters()._param_status_map[name] = Parameters::kUserSet;
  }

  /*! get string parameters.
   * Valid flags must be defined in Parameters::initialize_parameters()
   *  Refer details to https://www.phenix.bnl.gov/WWW/offline/wikioff/index.php/FVTX/Global_Parameters */
  static std::string
  get_string_par(const std::string& name)
  {
    get_parameters().check_string_par(name);
    return get_parameters()._string_par_map[name];
  }

  /*! set string parameters.
   * Valid flags must be defined in Parameters::initialize_parameters()
   *  Refer details to https://www.phenix.bnl.gov/WWW/offline/wikioff/index.php/FVTX/Global_Parameters */
  static void
  set_string_par(const std::string& name, std::string val)
  {
    get_parameters().check_string_par(name);
    get_parameters()._string_par_map[name] = val;
    get_parameters()._param_status_map[name] = Parameters::kUserSet;
  }

  /*! get float parameters.
   * Valid flags must be defined in Parameters::initialize_parameters()
   *  Refer details to https://www.phenix.bnl.gov/WWW/offline/wikioff/index.php/FVTX/Global_Parameters */
  static float
  get_float_par(const std::string& name)
  {
    get_parameters().check_float_par(name);
    return get_parameters()._float_par_map[name];
  }

  /*! set float parameters.
   * Valid flags must be defined in Parameters::initialize_parameters()
   *  Refer details to https://www.phenix.bnl.gov/WWW/offline/wikioff/index.php/FVTX/Global_Parameters */
  static void
  set_float_par(const std::string& name, float val)
  {
    get_parameters().check_float_par(name);
    get_parameters()._float_par_map[name] = val;
    get_parameters()._param_status_map[name] = Parameters::kUserSet;
  }

  //!@}

  //!@name timestamp for database use
  //@{
public:

  //! set_run_number for database reading. Otherwise, will get from recoConsts
  static void
  set_pdb_run_number(int runnumber);

  //! get run number for database reading
  static int
  get_pdb_run_number();

  //! get time stamp for database reading
  static const PHTimeStamp &
  get_pdb_time();

protected:

  //! set time stamp for database reading according to run number
  static void
  sync_pdb_time();

  //!@}

  //!@name parameter storage
  //@{

public:
  //! stores all members in a unique 'Parameters' class
  class Parameters : public TObject
  {

  public:

    enum param_status
    {

      //! Place holder
      kInvalid = 0,

      //! Lowest priority: Default value from Parameters::initialize_parameters
      kDefault = 1,

      //! Mid priority: Loaded from database as default parameter for given run period
      kDatabase = 2,

      //! Highest priority: Set by users through macros, it will override all other options
      kUserSet = 3,

      //! Place holder
      kDummy = -1
    };

    //! constructor
    Parameters(void) :
        _set_run_num(0), _run_num(0), _pdb_time()
    {
      initialize_parameters();
    }

    void
    print(std::ostream& os = std::cout) const;

    //! Parameters -> PdbCalParameters
    void
    export_parameters(PdbCalParameters & pdb) const;

    //! PdbCalParameters -> Parameters
    void
    load_parameters(const PdbCalParameters & pdb);

    //! shortcut for tag/filename maps
    typedef std::map<std::string, param_status> ParamStatusMap;

    //! map of tags and filenames
    ParamStatusMap _param_status_map;

    param_status
    get_param_status(std::string parameter_name) const;
    std::string
    get_param_status_desc(std::string parameter_name) const;

    //! shortcut for tag/flags maps
    typedef std::map<std::string, bool> BoolParMap;

    //! map of tags and access flags
    BoolParMap _bool_par_map;

    //! check whether a parameter name is valid
    void
    check_bool_par(const std::string & name) const
    {
      check_par<BoolParMap>(_bool_par_map, name);
    }

    //! shortcut for tag/filename maps
    typedef std::map<std::string, std::string> StringParMap;

    //! map of tags and filenames
    StringParMap _string_par_map;

    //! check whether a string parameter name is valid
    void
    check_string_par(const std::string & name) const
    {
      check_par<StringParMap>(_string_par_map, name);
    }

    //! shortcut for tag/filename maps
    typedef std::map<std::string, float> FloatParMap;

    //! map of tags and filenames
    FloatParMap _float_par_map;

    //! check whether a float parameter name is valid
    void
    check_float_par(const std::string & name) const
    {
      check_par<FloatParMap>(_float_par_map, name);
    }

    //! user-set run number for database loading. _set_run_num = 0 means use recoConsts
    int _set_run_num;

    //! number for database loading
    int _run_num;

    //! time stamp for database loading
    PHTimeStamp _pdb_time;

  protected:

    //! \brief init flags map an file map, any flag must be first defined here
    //!
    //! Please refer details to and update:
    //! https://www.phenix.bnl.gov/WWW/offline/wikioff/index.php/FVTX/Global_Parameters
    void
    initialize_parameters();

    //! mark all parameters as kDefault
    void
    initialize_parameter_status(const param_status s = kDefault);

    //____________________________________________________
    //! check flag against list
    template<class FlagMap>
      void
      check_par(const FlagMap & map, const std::string & name) const
      {
        if (map.find(name) == map.end())
          {
            std::stringstream s;
            s
                << "TFvtxGlobalParCntrl::Parameters::check_par - ERROR - parameter name "
                << name << " is invalid.";

            std::cout << s.str() << " Available parameters are" << std::endl;
            print();

            throw std::runtime_error(s.str());
          }
      }

    // please update version numer if new variables are added
  ClassDef(TFvtxGlobalParCntrl::Parameters, 1) //storage for FVTX global parameters
  };

protected:
  //! return Parameters
  static Parameters&
  get_parameters(void);

  static Parameters _parameters;

  //!@}

  //!@name database IO
  //@{
public:

  //! call this at init_run to update parameters with the database settings
  //! It is OK to call multiple times. It only perform a database reading when a new run is used.
  static
  void
  init_run()
  {
    if (TFvtxGlobalParCntrl::get_pdb_time() == _timestamp_last_init_run) return;

    load_pdb_parameter(TFvtxGlobalParCntrl::get_pdb_time());

    _timestamp_last_init_run = TFvtxGlobalParCntrl::get_pdb_time();
  }

  //! database table name
  static const PHString calibname;

  //! database class name
  static const PHString classname;

  //! read from database
  static int
  load_pdb_parameter(PHTimeStamp tsearch);

  //! write to database
  static int
  save_pdb_parameter(PHTimeStamp start, PHTimeStamp stop, PHString descriptor);
  static int
  save_pdb_parameter(int beginrun, int endrun, PHString descriptor);

  static
  void
  test();

private:

  static PHTimeStamp _timestamp_last_init_run;

  //!@}

};

#endif /* TFvtxGlobalParCntrl_H_ */
