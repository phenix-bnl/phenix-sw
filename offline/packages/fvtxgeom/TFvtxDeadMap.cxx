// $Id: TFvtxDeadMap.cxx,v 1.7 2014/12/31 18:59:01 jinhuang Exp $                                                                                             

/*!
 * \file TFvtxDeadMap.cxx
 * \brief 
 * \author Jin Huang <jhuang@bnl.gov>
 * \version $Revision: 1.7 $
 * \date $Date: 2014/12/31 18:59:01 $
 */

#include <cmath>
#include <iostream>
#include <fstream>
#include <algorithm>
#include <ctype.h>
#include <string>
#include <climits>
#include <cstdlib>
#include <string>
#include <sstream>
#include <cstring>
#include <cassert>

#include <odbc++/connection.h>
#include <odbc++/setup.h>
#include <odbc++/types.h>
#include <odbc++/errorhandler.h>
#include <sql.h>
#include <odbc++/drivermanager.h>
#include <odbc++/resultset.h>
#include <odbc++/resultsetmetadata.h>
#include <odbc++/preparedstatement.h>
#include <odbc++/databasemetadata.h>

#include <FVTXGEOM.h>
#include <FvtxGeom.h>
#include <recoConsts.h>
#include <RunToTime.hh>
#include <PdbCalBank.hh>
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include "TFvtxGlobalParCntrl.h"

#include "TFvtxDeadMap.h"

using namespace std;

using namespace odbc;

ClassImp(TFvtxDeadMap)

const PHString TFvtxDeadMap::calibname = "calib.fvtx.deadmap";

const PHString TFvtxDeadMap::classname = "PdbFvtxDeadMapBank";

TFvtxDeadMap::TFvtxDeadMap() :
    _con(NULL)
{
}

TFvtxDeadMap::~TFvtxDeadMap()
{
  close_con_fvtxdb();
}

void
TFvtxDeadMap::Print(Option_t *) const
{

  cout << "TFvtxDeadMap::Print - Info - print " << dead_maps.size()
      << " dead maps:" << endl;

  for (dead_map_vec::const_iterator it = dead_maps.begin();
      it != dead_maps.end(); ++it)
    {
      const dead_map_ptr ptr = (*it);

      size_t cnt = ptr->get_n_record();

      cout << "Map " << ptr->get_key_word() << " (" << ptr->get_comment()
          << ") with " << cnt << " records:" << endl;

      for (size_t i = 0; i < cnt; i++)
        {
          dead_map_coder c(ptr->get_record(i));

          c.print();
        }
    }
}

void
TFvtxDeadMap::init_run()
{
  FVTXGEOM::PRINT(cout, "TFvtxDeadMap::init_run");

  TFvtxGlobalParCntrl::init_run();

  cout
      << "TFvtxDeadMap::init_run - Loading dead map according to configuration in TFvtxGlobalParCntrl."
      << endl;

  TFvtxDeadMap map;

  if (!TFvtxGlobalParCntrl::get_bool_par("deadmap_auto_load"))
    {
      cout
          << "TFvtxDeadMap::init_run - I am configured not to use dead map. All channels enabled."
          << endl;
      return;
    }

  if (TFvtxGlobalParCntrl::get_bool_par("deadmap_use_calibration_database"))
    {
      cout
          << "TFvtxDeadMap::init_run - load dead map from calibration database."
          << endl;

      const bool is_sim = TFvtxGlobalParCntrl::get_bool_par("is_sim")
          && (!TFvtxGlobalParCntrl::get_bool_par(
              "deadmap_use_production_map_for_sim"));
      if (is_sim)
        {
          cout << "TFvtxDeadMap::init_run - load dead map for simulation."
              << endl;
        }
      else
        {
          cout << "TFvtxDeadMap::init_run - load dead map for production."
              << endl;
        }
      map.dbGetAll(TFvtxGlobalParCntrl::get_pdb_time(), is_sim);

    }
  else
    {
      cout
          << "TFvtxDeadMap::init_run - load dead map from fvtx database (obsolete, used in 1st produciton of run12)."
          << endl;

      string use_custom_deadmap = TFvtxGlobalParCntrl::get_string_par(
          "deadmap_fvtxdb_use_custom_deadmap");
      if (use_custom_deadmap.length())
        {
          cout << "TFvtxDeadMap::init_run - use custom database "
              << use_custom_deadmap << endl;
          map.load_dead_chan_map_fvtxdb(use_custom_deadmap,
              TFvtxGlobalParCntrl::get_pdb_run_number());
        }
      else
        map.automatic_load_dead_chan_map_fvtxdb(
            TFvtxGlobalParCntrl::get_pdb_run_number());
    }

  map.apply_dead_maps();

  FVTXGEOM::PRINT(cout, "**");
}

void
TFvtxDeadMap::apply_dead_maps() const
{

  // set all channel active before deactivate dead channels
  for (int arm = 0; arm < FVTXGEOM::NumberOfArms; arm++)
    {
      FvtxGeom::get_arm(arm)->set_active(true);
    }

  if (dead_maps.size() == 0)
    {
      cout
          << "TFvtxDeadMap::apply_dead_maps - Info - No deadmap is loaded. Enable all FVTX channels."
          << endl;

      return;
    }

  cout << "TFvtxDeadMap::apply_dead_maps - Info - apply " << dead_maps.size()
      << " dead maps:" << endl;

  for (dead_map_vec::const_iterator it = dead_maps.begin();
      it != dead_maps.end(); ++it)
    {
      const dead_map_ptr ptr = (*it);

      size_t cnt = ptr->get_n_record();

      cout << "Applying Map " << ptr->get_key_word() << " ("
          << ptr->get_comment() << ") with " << cnt << " records." << endl;

      for (size_t i = 0; i < cnt; i++)
        {
          const dead_map_coder c(ptr->get_record(i));

          TFvtxDeadMap::apply_dead_chan(c, i);
        }
    }

  // Print out summary
  for (int arm = 0; arm < FVTXGEOM::NumberOfArms; arm++)
    {
      const double alive_ratio = FvtxGeom::get_arm(arm)->get_active_ratio();

      if (alive_ratio == 0)
        {
          cout << "TFvtxDeadMap::load_dead_chan_map - WARNING - " << "FVTX Arm "
              << arm << " is disabled in dead channel maps!" << endl;
        }
      else
        {
          cout << "TFvtxDeadMap::load_dead_chan_map - INFO - " << "FVTX Arm "
              << arm << ": " << (int) ((1 - alive_ratio) * 100)
              << "% is disabled in the dead channel map." << endl;
        }
    }
}

void
TFvtxDeadMap::apply_dead_chan(const dead_map_coder &map, const int nrows)
{

  const int arm = map.arm;
  const int cage = map.cage;
  const int station = map.station;
  const int sector = map.sector;
  const int side = map.side;
  const int strip = map.strip;

  try
    {
      if (arm < 0)
        {
          for (int i = 0; i < FVTXGEOM::NumberOfArms; i++)
            {
              FvtxArm * fvtx_arm = FvtxGeom::get_arm(i);
              fvtx_arm->set_active(false);
            }

          cout
              << "TFvtxDeadMap::load_dead_chan - WARNING: disable all FVTX due to dead map record # "
              << nrows << ": arm " << arm << " cage " << cage << " station "
              << station << " sector " << sector << " side " << side
              << " strip " << strip << endl;
        }
      else
        {

          FvtxArm * fvtx_arm = FvtxGeom::get_arm(arm);

          if (cage < 0)
            {
              fvtx_arm->set_active(false);

              cout
                  << "TFvtxDeadMap::load_dead_chan - WARNING: disable whole FVTX arm due to dead map record # "
                  << nrows << ": arm " << arm << " cage " << cage << " station "
                  << station << " sector " << sector << " side " << side
                  << " strip " << strip << endl;
            }
          else
            {

              FvtxCage * fvtx_cage = fvtx_arm->get_cage(cage);

              if (station < 0)
                {
                  fvtx_cage->set_active(false);

                  cout
                      << "TFvtxDeadMap::load_dead_chan - WARNING: disable whole FVTX cage due to dead map record # "
                      << nrows << ": arm " << arm << " cage " << cage
                      << " station " << station << " sector " << sector
                      << " side " << side << " strip " << strip << endl;
                }
              else
                {

                  FvtxStation * fvtx_station = fvtx_cage->get_station(station);

                  if (sector < 0)
                    {
                      fvtx_station->set_active(false);

                      cout
                          << "TFvtxDeadMap::load_dead_chan - WARNING: disable whole FVTX half disk due to dead map record # "
                          << nrows << ": arm " << arm << " cage " << cage
                          << " station " << station << " sector " << sector
                          << " side " << side << " strip " << strip << endl;

                    }
                  else
                    {

                      FvtxSector * fvtx_sector = fvtx_station->get_sector(
                          sector);

                      if (side < 0)
                        {
                          fvtx_sector->set_active(false);
                        }
                      else
                        {

                          FvtxColumn * fvtx_column = fvtx_sector->get_column(
                              side);

                          if (strip < 0)
                            {
                              fvtx_column->set_active(false);
                            }
                          else
                            {
                              FvtxStrip * fvtx_strip = fvtx_column->get_strip(
                                  strip);
                              fvtx_strip->set_active(false);
                            }
                        }
                    }
                }
            }
        }
    }
  catch (std::exception& e)
    {
      cout << "TFvtxDeadMap::load_dead_chan - load map error : " << e.what()
          << ".  Offending record # " << nrows << ": arm " << arm << " cage "
          << cage << " station " << station << " sector " << sector << " side "
          << side << " strip " << strip << endl;
    }

}

TFvtxDeadMap::dead_map_coder::dead_map_coder(
    PdbFvtxDeadMap::dead_map_record record) :
    orig_record(record)
{
  // record is an integer, packed in the form abcddeffff,
//  where a=arm, b=cage, c=station, dd=wedge, e=side, ffff=strip.
  // Values that are 'too high', for example strip=9999 means 'all strips'.
  // Thus the last number in the example is 1132189999,
  // which is arm=1, cage=1, station=3, wedge=21, side=8 (both sides), and strip=9999 (all strips).

  strip = record % 10000;
  record /= 10000;
  side = record % 10;
  record /= 10;
  sector = record % 100;
  record /= 100;
  station = record % 10;
  record /= 10;
  cage = record % 10;
  record /= 10;
  arm = record % 10;
  record /= 10;

  const int max_strip =
      station == 0 ?
          (int) (FVTXGEOM::NumberOfStripsSt1) :
          (int) (FVTXGEOM::NumberOfStripsSt2);

  if (strip >= max_strip)
    strip = -1;
  if (side >= FVTXGEOM::NumberOfColumns)
    side = -1;
  if (sector >= FVTXGEOM::NumberOfSectors)
    sector = -1;
  if (station >= FVTXGEOM::NumberOfStations)
    station = -1;
  if (cage >= FVTXGEOM::NumberOfCages)
    cage = -1;
  if (arm >= FVTXGEOM::NumberOfArms)
    arm = -1;
}

void
TFvtxDeadMap::dead_map_coder::print() const
{
  cout << "PdbFvtxDeadMap::dead_map_coder record " << orig_record << " :"
      << endl;
  cout << "\t" << "arm = " << "\t" << arm << endl;
  cout << "\t" << "cage = " << "\t" << cage << endl;
  cout << "\t" << "station = " << "\t" << station << endl;
  cout << "\t" << "sector = " << "\t" << sector << endl;
  cout << "\t" << "side = " << "\t" << side << endl;
  cout << "\t" << "strip = " << "\t" << strip << endl;
}

TFvtxDeadMap::dead_map_record
TFvtxDeadMap::dead_map_coder::encoder( //
    int arm, //
    int cage, //
    int station, //
    int sector, //
    int side, //
    int strip //
    )
{
  dead_map_record record = 0;

  record += arm < 0 ? FVTXGEOM::NumberOfArms : arm;

  record *= 10;
  record += cage < 0 ? FVTXGEOM::NumberOfCages : cage;

  record *= 10;
  record += station < 0 ? FVTXGEOM::NumberOfStations : station;

  record *= 100;
  record += sector < 0 ? FVTXGEOM::NumberOfSectors : sector;

  record *= 10;
  record += side < 0 ? FVTXGEOM::NumberOfColumns : side;

  record *= 10000;
  record += strip < 0 ? FVTXGEOM::NumberOfStripsSt2 : strip;

  return record;
}

void
TFvtxDeadMap::automatic_load_dead_chan_map_fvtxdb(int run_num)
{

  if (run_num <= 0)
    {
      recoConsts *rc = recoConsts::instance();
      run_num = rc->get_IntFlag("RUNNUMBER");
    }

  string dead_map_name;

  cout
      << "TFvtxDeadMap::automatic_load_dead_chan_map_fvtxdb - automatic looking for the dead channel map for run "
      << run_num << endl;

  Statement* stmt = NULL;
  ResultSet* rs = NULL;
  ostringstream cmd;

  try
    {
      if (!_con)
        _con = DriverManager::getConnection("fvtx", "phnxrc", "");

      cmd.str("");
      cmd
          << "select run_number_begin,run_number_end,database_name,comment from fvtx_dead_map_index "
          << "where run_number_begin <= " << run_num
          << " and run_number_end >= " << run_num;
      stmt = _con->createStatement();
      rs = stmt->executeQuery(cmd.str().c_str());
    }
  catch (std::exception& e)
    {
      cout
          << string(
              "TFvtxDeadMap::automatic_load_dead_chan_map_fvtxdb - database connection error : ")
              + e.what() + string(". Skipping load map!") << endl;

      if (rs)
        delete rs; // close the data base connection
      if (stmt)
        delete stmt;

      return;
    }

  int cnt = 0;
  while (rs->next())
    {

      try
        {
          dead_map_name = rs->getString("database_name");
          const string comment = rs->getString("comment");
          cout << "TFvtxDeadMap::automatic_load_dead_chan_map_fvtxdb - run "
              << run_num << " is linked to dead channel map " << dead_map_name
              << "(" << comment << ")"
              << " in database fvtx/fvtx_dead_map_index" << endl;

        }
      catch (std::exception& e)
        {
          cout
              << "TFvtxDeadMap::automatic_load_dead_chan_map_fvtxdb - database reading error : "
              << e.what() << (". Skipping load map!") << endl;

          if (rs)
            delete rs; // close the data base connection
          if (stmt)
            delete stmt;

          return;
        }

      load_dead_chan_map_fvtxdb(dead_map_name, run_num);
      cnt++;
    }

  if (cnt > 0)
    {

      cout
          << "TFvtxDeadMap::automatic_load_dead_chan_map_fvtxdb - finished automatically loading "
          << cnt << " dead channel tables for run " << run_num << endl;

    }
  else
    {
      ostringstream o;

      o
          << "TFvtxDeadMap::automatic_load_dead_chan_map_fvtxdb - Warning - can not find dead channel map for run ";
      o << run_num;
      o << " in database fvtx/fvtx_dead_map_index";
      o << ". Stop loading dead wedge map.";

      cout << o.str() << endl;
    }

  if (rs)
    delete rs; // close the data base connection
  if (stmt)
    delete stmt;
}

void
TFvtxDeadMap::load_dead_chan_map_fvtxdb(const std::string & dead_map_name,
    int run_num)
{

  Statement* stmt = NULL;
  ResultSet* rs = NULL;
  ostringstream cmd;
//  odbc::Connection * con = NULL;

  if (run_num <= 0)
    {
      recoConsts *rc = recoConsts::instance();
      run_num = rc->get_IntFlag("RUNNUMBER");
    }

  try
    {
      if (!_con)
        _con = DriverManager::getConnection("fvtx", "phnxrc", "");

      cmd.str("");
      cmd << "select * from " << dead_map_name
          << " where runnumber = 0 or runnumber = " << run_num;
      stmt = _con->createStatement();
      rs = stmt->executeQuery(cmd.str().c_str());

    }
  catch (std::exception& e)
    {
      cout
          << string(
              "TFvtxDeadMap::load_dead_chan_map_fvtxdb - database connection error : ")
              + e.what() + string(". Skipping load map!") << endl;

      if (rs)
        delete rs; // close the data base connection
      if (stmt)
        delete stmt;

      return;
    }

  int nrows = 0;
  while (rs->next())
    {
      string map;

      try
        {
//          int run_number = rs->getInt("runnumber");
//      string comment = rs->getString("comment");

          map = rs->getString("locator");
//          cout << "get " << run_number << ", map = " << map << endl;
        }
      catch (std::exception& e)
        {
          ostringstream o;
          o
              << "TFvtxDeadMap::load_dead_chan_map_fvtxdb - database connection read at record #";
          o << nrows;
          o << e.what();
          o << ". Skipping rest of dead wedge map!";

          cout << o.str() << endl;

          break;
        }
      catch (...)
        {

          ostringstream o;
          o
              << "TFvtxDeadMap::load_dead_chan_map_fvtxdb - database connection read at record #";
          o << nrows;
          o << ". Skipping rest of dead wedge map!";

          break;
        }

      string keyword;
      try
        {
          keyword = rs->getString("keyword");
        }
      catch (...)
        { // Ignore errors
        }
      keyword.erase(remove(keyword.begin(), keyword.end(), ' '), keyword.end());
      if (keyword.length() == 0)
        keyword = "No_Keyword";

      string comment;
      try
        {
          comment = rs->getString("comment");
        }
      catch (...)
        { // Ignore errors
        }
      if (comment.length() == 0)
        comment = "No_Comment";

      dead_map_ptr dead_map = add_dead_map();
      dead_map->set_key_word(keyword.c_str());
      dead_map->set_comment(comment.c_str());

      istringstream a(map.c_str());
      char c; // temp char to parse array
      a >> c;
      //      cout << " c = " << c << endl;
      if (c != '{')
        {
          ostringstream o;
          o
              << "TFvtxDeadMap::load_dead_chan_map_fvtxdb - Error - can not understand following dead channel locator ";
          o << map;
          o << " with char " << c;
          o << ". Continue anyway";

          cout << o.str() << endl;

        }

      int cnt = 0;
      dead_map_record record = 0;
      while (a >> record)
        {
//          cout << "get record " << record << endl;
          a >> c;
          if (c != '}' && c != ',')
            {
              ostringstream o;
              o
                  << "TFvtxDeadMap::load_dead_chan_map_fvtxdb - Error - can not understand following dead channel locator ";
              o << map;
              o << " with char " << c;
              o << ". Stop loading this record.";

              cout << o.str() << endl;
            }

          cnt++;

          dead_map->add_record(record);
        } // loop of records

      nrows++;

      cout
          << "TFvtxDeadMap::load_dead_chan_map_fvtxdb - Info - Finished loading "
          << cnt << " dead channel locators from map #" << nrows << ", "
          << keyword << " (" << comment << "), " << " in database fvtx/"
          << dead_map_name << endl;
    }

  cout
      << "TFvtxDeadMap::load_dead_chan_map_fvtxdb - Info - Finished loading dead channel map "
      << dead_map_name << " with " << nrows << " dead maps" << endl;

  if (rs)
    delete rs; // close the data base connection
  if (stmt)
    delete stmt;
//  delete con; // close the data base connection
}

//! close db connection
void
TFvtxDeadMap::close_con_fvtxdb()
{
  if (_con)
    {
      delete _con;
      _con = 0;
    }
}

int
TFvtxDeadMap::dbGetAll(int runnumber, bool is_sim)
{
  RunToTime* runTime = RunToTime::instance();
  PHTimeStamp *ts(runTime->getBeginTime(runnumber));
  PHTimeStamp Tstart = *ts;
  delete ts;

  return dbGetAll(Tstart, is_sim);
}

int
TFvtxDeadMap::dbGet(PHTimeStamp tsearch, TFvtxDeadMap::enu_band_id band_id)
{

  // start out fresh
  // Access the database, pull data into the internal structure
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  PdbBankID bankid((int) band_id);

  int ret = 0;

  // Open the Objectivity database for reading and pull all the values for
  // a given bank (i.e. a strip) and put it into our set
  if (application->startRead())
    {

      PdbCalBank *fvtxBank = bankManager->fetchBank(classname.getString(),
          bankid, calibname.getString(), tsearch);

      if (fvtxBank)
        {

          //          printf("TFvtxDeadMap::dbGetAll - start validity : %s (%u)\n", fvtxBank->getStartValTime().formatTimeString(), (unsigned int)fvtxBank->getStartValTime().getTics());
          //          printf("TFvtxDeadMap::dbGetAll - end validity   : %s (%u)\n", fvtxBank->getEndValTime().formatTimeString(), (unsigned int)fvtxBank->getEndValTime().getTics());
          //          printf("TFvtxDeadMap::dbGetAll - insertion      : %s (%u)\n", fvtxBank->getInsertTime().formatTimeString(), (unsigned int) fvtxBank->getInsertTime().getTics());
          //          printf("TFvtxDeadMap::dbGetAll - description    : %s\n", fvtxBank->getDescription().getString());

          //fvtxBank->print();
          int length = fvtxBank->getLength();

          for (int i = 0; i < length; i++)
            {
              add_dead_map((const PdbFvtxDeadMap&) (fvtxBank->getEntry(i)));
              ret++;
            }

          cout << "TFvtxDeadMap::dbGet sucessfully loaded " << ret
              << " dead maps from band_id = " << band_id << " with description "
              << fvtxBank->getDescription() << endl;

          delete fvtxBank;

        }
      else
        {
          cout
              << "TFvtxDeadMap::dbGet - Error - failed to load dead map on band_id = "
              << band_id << endl;
        }

    }
  else
    {

      application->abort();
      cout << ("TFvtxDeadMap::dbGet - Error - Transaction aborted.\n");

    }

  return ret;
}

int
TFvtxDeadMap::dbPutAll(int beginrun, int endrun, PHString descriptor,
    TFvtxDeadMap::enu_band_id band_id) const
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

  return dbPutAll(Tstart, Tstop, descriptor, band_id);
}

int
TFvtxDeadMap::dbPutAll(PHTimeStamp start, PHTimeStamp stop, PHString descriptor,
    TFvtxDeadMap::enu_band_id band_id) const
{

  cout << "TFvtxDeadMap::dbPutAll - startvaltime: " << start << endl;
  cout << "TFvtxDeadMap::dbPutAll - endvaltime: " << stop << endl;
  cout << "TFvtxDeadMap::dbPutAll - description: " << descriptor << endl;
  cout << "TFvtxDeadMap::dbPutAll - band id: " << (int) band_id << endl;

  // do we have something to put into the database?
  int length = get_n_dead_maps();
  if (length < 1)
    {
      cout << "TFvtxDeadMap::dbPutAll - Nothing to put into DB " << endl;
      cout << "length = " << length << endl;
      return -1;
    }

  //timestamp values: sanity check
  if (start > stop)
    {
      cout << "TFvtxDeadMap::dbPutAll - invalid start and stop:" << start
          << stop << endl;
      cout << "ignored" << endl;
      return -1;
    }

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  PdbBankID bankid((int) band_id);

  if (application->startUpdate())
    {

      PdbCalBank *fvtxBank = bankManager->createBank(classname.getString(),
          bankid, descriptor.getString(), start, stop, calibname.getString());

      if (fvtxBank)
        {

          cout << "TFvtxDeadMap::dbPutAll - new bank created, uploading "
              << length << " dead maps" << endl;
          fvtxBank->setLength(length);
          fvtxBank->print();

          int i = 0;
          for (dead_map_vec::const_iterator it = dead_maps.begin();
              it != dead_maps.end(); ++it)
            {
              const dead_map_ptr ptr = (*it);

              PdbFvtxDeadMap *strip_pointer =
                  (PdbFvtxDeadMap*) &(fvtxBank->getEntry(i));
              *strip_pointer = (*(ptr.get()));

              i++;
            }

          // commit new bank to the db and delete
          application->commit(fvtxBank);
          delete fvtxBank;

        }
      else
        {
          cout
              << ("TFvtxDeadMap::dbPutAll - bankManager returned zero-pointer\n");
        }

    }
  else
    cout << "TFvtxDeadMap::dbPutAll - failed to start application for update"
        << endl;

  cout << "TFvtxDeadMap::dbPutAll - done." << endl;
  cout << endl;

  return 0;
}

int
TFvtxDeadMap::txtGetAll(const char* infile, const char * comment)
{
  cout << "TFvtxDeadMap::txtGetAll - reading " << infile
      << " in records of (arm cage station sector side strip)" << endl;

  fstream f(infile, ios_base::in);

  if (!f.is_open())
    {

      cout << "TFvtxDeadMap::txtGetAll - Error - can not open file " << infile
          << endl;
      return 0;

    }

  TFvtxDeadMap::dead_map_ptr map_ptr = add_dead_map();
  map_ptr->set_key_word("text_import");

  if (comment)
    {
      map_ptr->set_comment(comment);
    }
  else
    {
      stringstream buf;
      buf << "imported from file " << infile;
      map_ptr->set_comment(buf.str().c_str());
    }

  int ret = 0;

  string line;

  while (!f.eof())
    {
      getline(f, line);

      if (line.length() == 0)
        continue;

      istringstream s(line);

      int arm = -1;
      int cage = -1;
      int station = -1;
      int sector = -1;
      int side = -1;
      int strip = -1;

      try
        {
          s >> arm;
          if (s.fail())
            throw runtime_error("No arm number");
          s >> cage;
          if (s.fail())
            throw runtime_error("No cage number");
          s >> station;
          if (s.fail())
            throw runtime_error("No station number");
          s >> sector;
          if (s.fail())
            throw runtime_error("No sector number");
          s >> side;
          if (s.fail())
            throw runtime_error("No side number");
          s >> strip;
          if (s.fail())
            throw runtime_error("No strip number");
        }
      catch (std::exception & e)
        {
          cout << "TFvtxDeadMap::txtGetAll - " << e.what() << ", ignore record "
              << line << endl;
          continue;
        }

      TFvtxDeadMap::add_dead_chan(map_ptr, //
          arm, //
          cage, //
          station, //
          sector, //
          side, //
          strip //
          );

      ret++;
    }
  cout << "TFvtxDeadMap::txtGetAll - imported " << ret << " records from "
      << infile << endl;

  return ret;
}

int
TFvtxDeadMap::txtPutAll(const char* outfile) const
{

  cout << "TFvtxDeadMap::txtPutAll - Info - print " << dead_maps.size()
      << " dead maps to " << outfile << endl;

  fstream f(outfile, ios_base::out);

  if (!f.is_open())
    {

      cout << "TFvtxDeadMap::txtPutAll - Error - can not open file " << outfile
          << endl;
      return 0;

    }

  int ret = 0;

  for (dead_map_vec::const_iterator it = dead_maps.begin();
      it != dead_maps.end(); ++it)
    {
      const dead_map_ptr ptr = (*it);

      size_t cnt = ptr->get_n_record();

      f << "# Map " << ptr->get_key_word() << " (" << ptr->get_comment()
          << ") with " << cnt << " records:" << endl;
      f <<"# in orders of arm cage station sector side strip"<<endl;

      for (size_t i = 0; i < cnt; i++)
        {
          dead_map_coder c(ptr->get_record(i));

          f <<  c.arm;
          f << "\t" << c.cage;
          f << "\t" << c.station;
          f << "\t" << c.sector;
          f << "\t" << c.side;
          f << "\t" << c.strip << endl;

          ret++;
        }
    }
  f.close();

  cout << "TFvtxDeadMap::txtPutAll - Info - saved " << ret << " records to "
      << outfile << endl;

  return ret;
}

//! read fvtx database and put into calibration database
void
TFvtxDeadMap::example_fvtxdb2calib_db(int run)
{

  cout
      << "TFvtxDeadMap::example_fvtxdb2calib_db - Example to read dead maps for run "
      << run << " from fvtx database and put into calibration database" << endl;

  // make an object
  TFvtxDeadMap dead_maps;

  // load the dead map from fvtx database
  dead_maps.automatic_load_dead_chan_map_fvtxdb(run);

  for (dead_map_vec::iterator it = dead_maps.get_dead_maps().begin();
      it != dead_maps.get_dead_maps().end(); ++it)
    {
      dead_map_ptr ptr = (*it);

      if (string(ptr->get_key_word()) == string("dead_strips"))
        {
          dead_maps.get_dead_maps().erase(it);

          break;
        }
    }

  // test apply it to analysis
  dead_maps.apply_dead_maps();

  // upload to the calibration database
  stringstream buf;
  buf << "imported from FVTX database for run " << run;
  dead_maps.dbPutAll(run, run, buf.str().c_str());

}

//! write a single dead channel into calibration database
void
TFvtxDeadMap::example_deadchan2calib_db(int run)
{
  cout
      << "TFvtxDeadMap::example_deadchan2calib_db - Example to put one dead map with single dead channel for run "
      << run << ", and put into calibration database" << endl;

  stringstream buf;

  // make an object
  TFvtxDeadMap dead_maps;

  // make a new dead map
  TFvtxDeadMap::dead_map_ptr map_ptr = dead_maps.add_dead_map();
  map_ptr->set_key_word("oncal_test");
  buf.str("");
  buf << "test for oncal/insetering dead map for run " << run;
  map_ptr->set_comment(buf.str().c_str());

  // add single dead wedge of arm0 cage1 station2 sector3
  TFvtxDeadMap::add_dead_chan(map_ptr, //
      /*int arm = */0, //
      /*int cage =*/1, //
      /*int station =*/2, //
      /*int sector =*/3, //
      /*int side = */-1, //
      /*int strip =*/-1 //
      );

  // See what's in there
  dead_maps.Print();

  // upload to the calibration database
  buf.str("");
  buf << "a set of dead maps for run" << run
      << ", although in this test there is only one map in this set";
  dead_maps.dbPutAll(run, run, buf.str().c_str());
}

//! Print what's in calibration database for a single run
void
TFvtxDeadMap::example_print_calib_db(int run)
{

  cout
      << "TFvtxDeadMap::example_print_calib_db - Example to read dead maps for run "
      << run
      << " from calibration database, print the content and apply for analysis"
      << endl;
  // make an object
  TFvtxDeadMap dead_maps;

  // load the dead map from fvtx database
  dead_maps.dbGetAll(run);

  // See what's in there
  dead_maps.Print();

  // Save to a text file
  dead_maps.txtPutAll("TFvtxDeadMap_example_print_calib_db.txt");

  // test apply it to analysis
  dead_maps.apply_dead_maps();
}

//! Save an empty dead map for a long run period in band_id BAND_ID_RUN_PERIOD,
//! so dbGetAll() do not report error if long period map is not filled.
void
TFvtxDeadMap::example_make_dummy_deadmap()
{
  cout << "TFvtxDeadMap::example_make_dummy_deadmap - WARNING -"
      << " this function will overwrite all dead maps for all running period. "
      << "Therefore, it is usually disabled for safety." << endl;

//  // make an object
//  TFvtxDeadMap dead_maps;
//
//  // make a new & empty dead map
//  TFvtxDeadMap::dead_map_ptr map_ptr = dead_maps.add_dead_map();
//  map_ptr->set_key_word("dummy");
//  map_ptr->set_comment("Dummy dead map with all channel enabled");
//
//  // load the dead map from fvtx database
//  PHTimeStamp begin( //
//      2011,//int year,//
//      12, //int month, //
//      1, //int day, //
//      0, //int hour, //
//      0, //int minute, //
//      0, //int second, //
//      0 //int fraction//
//      );
//  PHTimeStamp end( //
//      2021,//int year,//
//      12, //int month, //
//      1, //int day, //
//      0, //int hour, //
//      0, //int minute, //
//      0, //int second, //
//      0 //int fraction//
//      );
//
//  dead_maps.dbPutAll(begin, end, "Dummy dead maps with all channel enabled",
//      BAND_ID_RUN_SINGLE);
//  dead_maps.dbPutAll(begin, end, "Dummy dead maps with all channel enabled",
//      BAND_ID_RUN_PERIOD);
//  dead_maps.dbPutAll(begin, end, "Dummy dead maps with all channel enabled",
//      BAND_ID_SIM);

}

