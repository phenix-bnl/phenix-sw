// $Id: TFvtxGlobalParCntrl.cxx,v 1.6 2016/02/04 14:42:15 jinhuang Exp $

/*!
 * \file TFvtxGlobalParCntrl.cxx
 * \brief 
 * \author Jin Huang <jhuang@bnl.gov>
 * \version $Revision: 1.6 $
 * \date $Date: 2016/02/04 14:42:15 $
 */

#include "FVTXGEOM.h"

#include <recoConsts.h>
#include <RunToTime.hh>
#include <PdbCalBank.hh>
#include <PdbBankManager.hh>
#include <PdbApplication.hh>

#include "TFvtxGlobalParCntrl.h"

using namespace std;

ClassImp(TFvtxGlobalParCntrl::Parameters);

PHTimeStamp TFvtxGlobalParCntrl::_timestamp_last_init_run;
const PHString TFvtxGlobalParCntrl::calibname = "calib.fvtx.parameter";
const PHString TFvtxGlobalParCntrl::classname = "PdbCalParametersBank";

TFvtxGlobalParCntrl::Parameters TFvtxGlobalParCntrl::_parameters;

//____________________________________________________
//! \brief init flags map an file map, any flag must be first defined here
//!
//! Please refer details to and update
//! https://www.phenix.bnl.gov/WWW/offline/wikioff/index.php/FVTX/Global_Parameters
void
TFvtxGlobalParCntrl::Parameters::initialize_parameters(void)
{
  // valid flags must be declared here first

  //generic
  _bool_par_map.insert(std::make_pair("is_pp", true));
  _bool_par_map.insert(std::make_pair("use_svx", true));
  _bool_par_map.insert(std::make_pair("is_sim", false));
  _bool_par_map.insert(std::make_pair("quick_muon_reco",false));
  

  // dead maps database
  _bool_par_map.insert(std::make_pair("deadmap_auto_load", true));
  _bool_par_map.insert(
      std::make_pair("deadmap_use_calibration_database", true));
  _bool_par_map.insert(
      std::make_pair("deadmap_use_production_map_for_sim", false));
  _string_par_map.insert(
      std::make_pair("deadmap_fvtxdb_use_custom_deadmap", ""));

  // geometry/alignment database
  _bool_par_map.insert(std::make_pair("geom_auto_load", true));
  _bool_par_map.insert(std::make_pair("geom_use_calibration_database", true));
  _bool_par_map.insert(
      std::make_pair("geom_calibration_database_TGeo_format", false));
  _string_par_map.insert(
      std::make_pair("geom_root_file_name", "fvtxgeom.root"));
  _string_par_map.insert(
      std::make_pair("geom_root_file_path",
          "/afs/rhic.bnl.gov/phenix/PHENIX_LIB/simulation/run12/"));

  // expected beam center, default use is for the seed beam location in the pattern recognition.
  _float_par_map.insert(std::make_pair("beam_x_seed", 0));
  _float_par_map.insert(std::make_pair("beam_y_seed", 0));
  _float_par_map.insert(std::make_pair("beam_dxdz", 0));
  _float_par_map.insert(std::make_pair("beam_dydz", 0));
  _bool_par_map.insert(std::make_pair("beam_use_average_xy", false));

  // mark everything as initialized status
  initialize_parameter_status();
}

void
TFvtxGlobalParCntrl::Parameters::initialize_parameter_status(
    const TFvtxGlobalParCntrl::Parameters::param_status s)
{

  for (BoolParMap::const_iterator iter = _bool_par_map.begin();
      iter != _bool_par_map.end(); ++iter)
    _param_status_map.insert(std::make_pair(iter->first, s));

  for (StringParMap::const_iterator iter = _string_par_map.begin();
      iter != _string_par_map.end(); ++iter)
    _param_status_map.insert(std::make_pair(iter->first, s));

  for (FloatParMap::const_iterator iter = _float_par_map.begin();
      iter != _float_par_map.end(); ++iter)
    _param_status_map.insert(std::make_pair(iter->first, s));

}

std::string
TFvtxGlobalParCntrl::Parameters::get_param_status_desc(
    std::string parameter_name) const
{
  ParamStatusMap::const_iterator it = _param_status_map.find(parameter_name);

  param_status s = kInvalid;
  if (it != _param_status_map.end())
    s = it->second;

  switch (s)
    {
  case
  //! Place holder
  kInvalid:
    return "Invalid";
    break;
  case
  //! Lowest priority: Default value from Parameters::initialize_parameters
  kDefault:
    return "Default Value";

    break;
  case
  //! Mid priority: Loaded from database as default parameter for given run period
  kDatabase:
    return "From Database";

    break;
  case
  //! Highest priority: Set by users through macros, it will override all other options
  kUserSet:
    return "User Set";

    break;
  default:
    return "Invalid";
    }
}

TFvtxGlobalParCntrl::Parameters::param_status
TFvtxGlobalParCntrl::Parameters::get_param_status(
    std::string parameter_name) const
{
  ParamStatusMap::const_iterator it = _param_status_map.find(parameter_name);

  param_status s = kInvalid;
  if (it != _param_status_map.end())
    s = it->second;

  return s;
}

//____________________________________________________
//! Parameters -> PdbCalParameters
void
TFvtxGlobalParCntrl::Parameters::export_parameters(PdbCalParameters & pdb) const
{

  for (BoolParMap::const_iterator iter = _bool_par_map.begin();
      iter != _bool_par_map.end(); ++iter)
    pdb.set_parameter(iter->first, iter->second);

  for (StringParMap::const_iterator iter = _string_par_map.begin();
      iter != _string_par_map.end(); ++iter)
    pdb.set_parameter(iter->first, iter->second);

  for (FloatParMap::const_iterator iter = _float_par_map.begin();
      iter != _float_par_map.end(); ++iter)
    pdb.set_parameter(iter->first, iter->second);
}

//____________________________________________________
//! PdbCalParameters -> Parameters
void
TFvtxGlobalParCntrl::Parameters::load_parameters(const PdbCalParameters & pdb)
{

  for (BoolParMap::iterator iter = _bool_par_map.begin();
      iter != _bool_par_map.end(); ++iter)
    {
      if (get_param_status(iter->first) <= kDatabase
          and pdb.is_parameter_exist(iter->first))
        {
          iter->second = pdb.get_parameter<bool>(iter->first);
          _param_status_map[iter->first] = kDatabase;
        }
    }
  for (StringParMap::iterator iter = _string_par_map.begin();
      iter != _string_par_map.end(); ++iter)
    {
      if (get_param_status(iter->first) <= kDatabase
          and pdb.is_parameter_exist(iter->first))
        {
          iter->second = pdb.get_parameter<string>(iter->first);
          _param_status_map[iter->first] = kDatabase;
        }
    }

  for (FloatParMap::iterator iter = _float_par_map.begin();
      iter != _float_par_map.end(); ++iter)
    {
      if (get_param_status(iter->first) <= kDatabase
          and pdb.is_parameter_exist(iter->first))
        {
          iter->second = pdb.get_parameter<float>(iter->first);
          _param_status_map[iter->first] = kDatabase;
        }
    }
}

//____________________________________________________
TFvtxGlobalParCntrl::Parameters&
TFvtxGlobalParCntrl::get_parameters(void)
{
  return _parameters;
}

//____________________________________________________
void
TFvtxGlobalParCntrl::Parameters::print(std::ostream& os) const
{

  FVTXGEOM::PRINT(os, "TFvtxGlobalParCntrl::Parameters::print");

  map<string, string> printmap;

//  s << _bool_par_map.size() << " boolean parameters:" << endl;
  for (BoolParMap::const_iterator iter = _bool_par_map.begin();
      iter != _bool_par_map.end(); ++iter)
    {
      stringstream s;

      s << iter->first << " (Boolean)\t: "
          << ((iter->second) ? "true" : "false") << " ("
          << get_param_status_desc(iter->first) << ")" << std::endl;

      printmap[iter->first] = s.str();
    }

//  os << _string_par_map.size() << " string parameters:" << endl;
  for (StringParMap::const_iterator iter = _string_par_map.begin();
      iter != _string_par_map.end(); ++iter)
    {
      stringstream s;
      s << iter->first << " (String)\t: "
          << ((iter->second.length() == 0 ? string("[empty]") : iter->second))
          << " (" << get_param_status_desc(iter->first) << ")" << std::endl;
      printmap[iter->first] = s.str();
    }
//  os << endl;

//  os << _float_par_map.size() << " float parameters:" << endl;
  for (FloatParMap::const_iterator iter = _float_par_map.begin();
      iter != _float_par_map.end(); ++iter)
    {
      stringstream s;
      os << iter->first << "\t: " << ((iter->second)) << " ("
          << get_param_status_desc(iter->first) << ")" << std::endl;
      printmap[iter->first] = s.str();
    }
//  os << endl;

  for (map<string, string>::const_iterator iter = printmap.begin();
      iter != printmap.end(); ++iter)
    {
      os << iter->second;
    }
  os << endl;

  os << "run number:";
  if (_run_num)
    os << _run_num;
  else
    os << " not yet set";
  os << endl;

  os << "time stamp: " << _pdb_time << endl;
  os << endl;

  os
      << "Parameters are defined in TFvtxGlobalParCntrl::Parameters::initialize_parameters()  "
      << endl;
  os
      << "      and documented at https://www.phenix.bnl.gov/WWW/offline/wikioff/index.php/FVTX/Global_Parameters"
      << endl;

  FVTXGEOM::PRINT(os, "**");

}

//____________________________________________________
//! set_run_number for database reading
void
TFvtxGlobalParCntrl::set_pdb_run_number(int runnumber)
{
  get_parameters()._set_run_num = runnumber;
  get_parameters()._run_num = runnumber;
  sync_pdb_time();
}

//____________________________________________________
//! set time stamp for database reading
void
TFvtxGlobalParCntrl::sync_pdb_time()
{

  RunToTime* runTime = RunToTime::instance();
  if (!runTime)
    {
      cout
          << "TFvtxGlobalParCntrl::set_pdb_run_number - Error - Can not get an instance for RunToTime"
          << endl;
      return;
    }

  PHTimeStamp *ts(runTime->getBeginTime(get_parameters()._run_num));
  if (!ts)
    {
      cout
          << "TFvtxGlobalParCntrl::set_pdb_run_number - Error - Can not get start time stamp for run "
          << get_parameters()._run_num
          << ". The current time stamp will be used!!" << endl;
      return;
    }

  PHTimeStamp Tstart = *ts;
  delete ts;

  get_parameters()._pdb_time = (Tstart);

  cout << "TFvtxGlobalParCntrl::set_pdb_run_number - will use run "
      << get_pdb_run_number() << " for FVTX databaser ops. Time stamp = "
      << Tstart << endl;
}

//____________________________________________________
//! get time stamp for database reading
const PHTimeStamp &
TFvtxGlobalParCntrl::get_pdb_time()
{
  // update runnumber/time stamp if necessary
  get_pdb_run_number();

  return get_parameters()._pdb_time;
}

int
TFvtxGlobalParCntrl::get_pdb_run_number()
{

  if (get_parameters()._set_run_num > 0)
    {
      // use custom set run number
      return get_parameters()._set_run_num;
    }
  else
    {
      // use automatic run number, sync time stamp is a new run number is set in recoConsts

      recoConsts *rc = recoConsts::instance();

      int run_num = rc->get_IntFlag("RUNNUMBER");

      if (run_num <= 0)
        {
          throw runtime_error(
              "TFvtxGlobalParCntrl::get_pdb_run_number - Can not find the run number in recoConsts");
        }

      if (get_parameters()._run_num != run_num)
        {
          get_parameters()._run_num = run_num;

          sync_pdb_time();
        }
      return get_parameters()._run_num;
    }
}

void
TFvtxGlobalParCntrl::test()
{
  PdbCalParameters p;

  p.set_parameter("test", 10);

  p.print();

  cout << "p.set_parameter<int>(test) = " << p.get_parameter<int>("test")
      << endl;
  cout << "p.set_parameter<int>(test2) = " << p.get_parameter<int>("test2")
      << endl;

  PdbCalParameters p2;

  set_float_par("beam_x_seed", 0.3);
  get_parameters().print();
  get_parameters().export_parameters(p2);
  p2.set_parameter("is_pp", false);
  set_float_par("beam_y_seed", -0.3);
  get_parameters().load_parameters(p2);
  get_parameters().print();
}

//! read from database
int
TFvtxGlobalParCntrl::load_pdb_parameter(PHTimeStamp tsearch)
{

  cout << "TFvtxGlobalParCntrl::load_pdb_parameter for time stamp " << tsearch
      << endl;

  // start out fresh
  // Access the database, pull data into the internal structure
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  PdbBankID bankid((int) 0);
  const PHString class_n = classname;

  int ret = 0;

  // Open the Objectivity database for reading and pull all the values for
  // a given bank (i.e. a strip) and put it into our set
  if (application->startRead())
    {

      PdbCalBank *fvtxBank = bankManager->fetchBank(class_n.getString(), bankid,
          calibname.getString(), tsearch);

      if (fvtxBank)
        {

          //          printf("TFvtxGlobalParCntrl::load_pdb_parameter - start validity : %s (%u)\n", fvtxBank->getStartValTime().formatTimeString(), (unsigned int)fvtxBank->getStartValTime().getTics());
          //          printf("TFvtxGlobalParCntrl::load_pdb_parameter - end validity   : %s (%u)\n", fvtxBank->getEndValTime().formatTimeString(), (unsigned int)fvtxBank->getEndValTime().getTics());
          //          printf("TFvtxGlobalParCntrl::load_pdb_parameter - insertion      : %s (%u)\n", fvtxBank->getInsertTime().formatTimeString(), (unsigned int) fvtxBank->getInsertTime().getTics());
          //          printf("TFvtxGlobalParCntrl::load_pdb_parameter - description    : %s\n", fvtxBank->getDescription().getString());

          //fvtxBank->print();
          int length = fvtxBank->getLength();

          if (length == 1)
            {

              const PdbCalParameters & dba =
                  (PdbCalParameters &) fvtxBank->getEntry(0);

              cout
                  << "TFvtxGlobalParCntrl::load_pdb_parameter load parameters from database with "
                  << dba.size() << " entries. Description is "
                  << fvtxBank->getDescription().getString() << endl;

              get_parameters().load_parameters(dba);
              ret++;

            }
          else
            {

              cout
                  << "TFvtxGlobalParCntrl::load_pdb_parameter invalid FVTX parameter band"
                  << ", # of object = " << length << ", bank_id = "
                  << bankid.getInternalValue() << ", description = "
                  << fvtxBank->getDescription() << endl;
              throw runtime_error(
                  "TFvtxGlobalParCntrl::load_pdb_parameter invalid FVTX geometry band");

            }

          delete fvtxBank;

        } //      if (fvtxBank)

      else
        {
          cout
              << "TFvtxGlobalParCntrl::load_pdb_parameter - Error - failed to load parameters on bank_id = "
              << bankid.getInternalValue()
              << ". This usually means the initial parameters for the run you are looking at is not yet configured in the database yet. "
              << endl
              << "In this case, the default value will be used. Please contact FVTX software expert for further inputs."
              << endl;
//          throw runtime_error(
//              "TFvtxGlobalParCntrl::load_pdb_parameter - Error - failed to load parameters");

          sleep(5);
        }

    }
  else
    {

      application->abort();
      cout
          << ("TFvtxGlobalParCntrl::load_pdb_parameter - Error - Transaction aborted.\n");
      throw runtime_error(
          "TFvtxGlobalParCntrl::load_pdb_parameter - Error - Transaction aborted.");
    }

  return ret;
}

//! read from database
int
TFvtxGlobalParCntrl::save_pdb_parameter(PHTimeStamp start, PHTimeStamp stop,
    PHString descriptor)
{

  PdbBankID bankid(0);
  const PHString class_n = classname;

  cout << "TFvtxGlobalParCntrl::save_pdb_parameter - startvaltime: " << start
      << endl;
  cout << "TFvtxGlobalParCntrl::save_pdb_parameter - endvaltime: " << stop
      << endl;
  cout << "TFvtxGlobalParCntrl::save_pdb_parameter - description: "
      << descriptor << endl;
  cout << "TFvtxGlobalParCntrl::save_pdb_parameter - band id: "
      << bankid.getInternalValue() << endl;

  //timestamp values: sanity check
  if (start > stop)
    {
      cout
          << "TFvtxGlobalParCntrl::save_pdb_parameter - invalid start and stop:"
          << start << stop << endl;
      cout << "ignored" << endl;
      return -1;
    }

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  if (application->startUpdate())
    {
      PdbCalBank *fvtxBank = bankManager->createBank(class_n.getString(),
          bankid, descriptor.getString(), start, stop, calibname.getString());

      if (fvtxBank)
        {

          fvtxBank->setLength(1);

          cout
              << "TFvtxGlobalParCntrl::save_pdb_parameter - new bank created, uploading the current parameters"
              << endl;

          PdbCalParameters & pdb =
              dynamic_cast<PdbCalParameters &>(fvtxBank->getEntry(0));
          get_parameters().export_parameters(pdb);

          fvtxBank->print();
          pdb.print();

          // commit new bank to the db and delete
          application->commit(fvtxBank);
          delete fvtxBank;

        }
      else
        {
          cout
              << ("TFvtxGlobalParCntrl::save_pdb_parameter - bankManager returned zero-pointer\n");
        }

    }
  else
    cout
        << "TFvtxGlobalParCntrl::save_pdb_parameter - failed to start application for update"
        << endl;

  cout << "TFvtxGlobalParCntrl::save_pdb_parameter - done." << endl;
  cout << endl;

  return 0;
}

int TFvtxGlobalParCntrl::
  save_pdb_parameter(int beginrun, int endrun, PHString descriptor)
{

  RunToTime* runTime = RunToTime::instance();
  PHTimeStamp *ts(runTime->getBeginTime(beginrun));
  PHTimeStamp Tstart = *ts;
  delete ts;
  PHTimeStamp Tstop;

  // The runnumber is encoded into PHTimeStamp.
  if (endrun > 0)
    {
      ts = runTime->getEndTime(endrun);
      Tstop = *ts;
      delete ts;
    }
  else
    {
      Tstop.setToFarFuture();
    }

  return save_pdb_parameter(Tstart, Tstop, descriptor);
}
