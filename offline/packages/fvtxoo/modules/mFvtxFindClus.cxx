#include <mFvtxFindClus.h>
#include <mFvtxFindClusPar.h>
#include <TMutNode.h>  
#include <TFvtxHitMap.h>
#include <TFvtxClusMap.h>
#include <PHException.h>
#include <FVTXOO.h>
#include <FVTXOO_FEM.h>
#include <FVTXGEOM.h>
#include <FvtxGeom.h>
#include <recoConsts.h>

#include <cmath>
#include <iostream>
#include <algorithm>
#include <ctype.h>
#include <string>
#include <climits>
#include <cstdlib>
#include <string>
#include <sstream>
#include <cstring>

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

using namespace std;

using namespace odbc;

mFvtxFindClus::mFvtxFindClus() :
    _mod_par(NULL), _hit_map(NULL), _clus_map(NULL), _timer(
        PHTimeServer::get()->insert_new("mFvtxFindClus")), _con(NULL)
{
  FVTXOO::TRACE("initializing module mFvtxFindClus");
}

//destructor
mFvtxFindClus::~mFvtxFindClus()
{
  if (_con)
    delete _con;
}

void
mFvtxFindClus::init_run(PHCompositeNode* top_node)
{
  FVTXOO::TRACE("initializing run for mFvtxFindClus");
  try
    {

      //Reset IOC pointers
      set_interface_ptrs(top_node);

    }
  catch (std::exception& e)
    {
      FVTXOO::TRACE(
          string("mFvtxFindClus::init_run - Ignoring Error -") + e.what());
    }

  // Obsolete, use TFvtxDeadMap instead
//  load_dead_chan_map();
}

mFvtxFindClus::dead_map_decoder::dead_map_decoder(int record) :
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
  if (side >= FVTXOO::MAX_COLUMN)
    side = -1;
  if (sector >= FVTXOO::MAX_SECTOR)
    sector = -1;
  if (station >= FVTXOO::MAX_STATION)
    station = -1;
  if (cage >= FVTXOO::MAX_CAGE)
    cage = -1;
  if (arm >= FVTXOO::MAX_ARM)
    arm = -1;
}

void
mFvtxFindClus::dead_map_decoder::print() const
{
  cout << "mFvtxFindClus::dead_map_decoder received record " << orig_record
      << endl;
  cout << "\t" << "arm = " << "\t" << arm << endl;
  cout << "\t" << "cage = " << "\t" << cage << endl;
  cout << "\t" << "station = " << "\t" << station << endl;
  cout << "\t" << "sector = " << "\t" << sector << endl;
  cout << "\t" << "side = " << "\t" << side << endl;
  cout << "\t" << "strip = " << "\t" << strip << endl;
}

void
mFvtxFindClus::load_dead_chan_map(void)
{
  cout
      << "mFvtxFindClus::load_dead_chan_map - WRNING - Obsolete function, use TFvtxDeadMap instead "
      << endl;

  // set all channel active before deactivate dead channels
  for (int arm = 0; arm < FVTXGEOM::NumberOfArms; arm++)
    {
      FvtxGeom::get_arm(arm)->set_active(true);
    }

  if (!_mod_par)
    {
      cout
          << "mFvtxFindClus::load_dead_chan_map - Error - cannot find mFvtxFindClusPar, quit ..."
          << endl;

      return;
    }

  string dead_map_name(_mod_par->get_dead_map_name().Data());

  if (dead_map_name.length() == 0 && _mod_par->get_auto_load_dead_map())
    {
      automatic_load_dead_chan_map();
    }
  else if (dead_map_name.length() > 0)
    {
      // load dead map as specified in _mod_par->get_dead_map_name()

      if (_mod_par->get_auto_load_dead_map())
        cout << "mFvtxFindClus::load_dead_chan_map - Warning - "
            << "ignore automatically select dead channel map, since map "
            << dead_map_name << " is specified." << endl;

      cout
          << "mFvtxFindClus::load_dead_chan_map - Info - load dead channel map "
          << dead_map_name << endl;
      load_dead_chan_map(dead_map_name);
    }
  else
    {
      cout
          << "mFvtxFindClus::load_dead_chan_map - Info - I am configured NOT to load dead channel map. All strips enabled."
          << endl;
    }

  if (_con)
    {
      delete _con;
      _con = NULL;
    }

  // Print out summary
  for (int arm = 0; arm < FVTXGEOM::NumberOfArms; arm++)
    {
      const double alive_ratio = FvtxGeom::get_arm(arm)->get_active_ratio();

      if (alive_ratio == 0)
        {
          cout << "mFvtxFindClus::load_dead_chan_map - WARNING - "
              << "FVTX Arm " << arm << " is disabled in dead channel maps!"
              << endl;
        }
      else
        {
          cout << "mFvtxFindClus::load_dead_chan_map - INFO - " << "FVTX Arm "
              << arm << ": " << (int) ((1 - alive_ratio) * 100)
              << "% is disabled in the dead channel map." << endl;
        }
    }
  return;
}

void
mFvtxFindClus::automatic_load_dead_chan_map(void)
{
  cout
      << "mFvtxFindClus::automatic_load_dead_chan_map - WRNING - Obsolete function, use TFvtxDeadMap instead "
      << endl;

  recoConsts *rc = recoConsts::instance();
  unsigned int run_num = rc->get_IntFlag("RUNNUMBER");

  string dead_map_name;

  cout
      << "mFvtxFindClus::automatic_load_dead_chan_map - automatic looking for the dead channel map for run "
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
      FVTXOO::TRACE(
          string(
              "mFvtxFindClus::automatic_load_dead_chan_map - database connection error : ")
              + e.what() + string(". Skipping load map!"));

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
          cout << "mFvtxFindClus::automatic_load_dead_chan_map - run "
              << run_num << " is linked to dead channel map " << dead_map_name
              << "(" << comment << ")"
              << " in database fvtx/fvtx_dead_map_index" << endl;

        }
      catch (std::exception& e)
        {
          FVTXOO::TRACE(
              string(
                  "mFvtxFindClus::automatic_load_dead_chan_map - database reading error : ")
                  + e.what() + string(". Skipping load map!"));

          if (rs)
            delete rs; // close the data base connection
          if (stmt)
            delete stmt;

          return;
        }

      load_dead_chan_map(dead_map_name);
      cnt++;
    }

  if (cnt > 0)
    {

      cout
          << "mFvtxFindClus::automatic_load_dead_chan_map - finished automatically loading "
          << cnt << " dead channel tables for run " << run_num << endl;

    }
  else
    {
      ostringstream o;

      o
          << "mFvtxFindClus::automatic_load_dead_chan_map - Warning - can not find dead channel map for run ";
      o << run_num;
      o << " in database fvtx/fvtx_dead_map_index";
      o << ". Stop loading dead wedge map.";

      FVTXOO::TRACE(o.str());
    }

  if (rs)
    delete rs; // close the data base connection
  if (stmt)
    delete stmt;
}

void
mFvtxFindClus::load_dead_chan_map(const std::string & dead_map_name)
{

  cout
      << "mFvtxFindClus::load_dead_chan_map - WRNING - Obsolete function, use TFvtxDeadMap instead "
      << endl;

  Statement* stmt = NULL;
  ResultSet* rs = NULL;
  ostringstream cmd;
//  odbc::Connection * con = NULL;

  recoConsts *rc = recoConsts::instance();
  unsigned int run_num = rc->get_IntFlag("RUNNUMBER");

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
      FVTXOO::TRACE(
          string(
              "mFvtxFindClus::load_dead_chan_map - database connection error : ")
              + e.what() + string(". Skipping load map!"));

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
              << "mFvtxFindClus::load_dead_chan_map - database connection read at record #";
          o << nrows;
          o << e.what();
          o << ". Skipping rest of dead wedge map!";

          FVTXOO::TRACE(o.str());

          break;
        }
      catch (...)
        {

          ostringstream o;
          o
              << "mFvtxFindClus::load_dead_chan_map - database connection read at record #";
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

      istringstream a(map.c_str());
      char c; // temp char to parse array
      a >> c;
      //      cout << " c = " << c << endl;
      if (c != '{')
        {
          ostringstream o;
          o
              << "mFvtxFindClus::load_dead_chan_map - Error - can not understand following dead channel locator ";
          o << map;
          o << " with char " << c;
          o << ". Continue anyway";

          FVTXOO::TRACE(o.str());

        }

      int cnt = 0;
      int32_t record = 0;
      while (a >> record)
        {
//          cout << "get record " << record << endl;
          a >> c;
          if (c != '}' && c != ',')
            {
              ostringstream o;
              o
                  << "mFvtxFindClus::load_dead_chan_map - Error - can not understand following dead channel locator ";
              o << map;
              o << " with char " << c;
              o << ". Stop loading this record.";

              FVTXOO::TRACE(o.str());
            }

          cnt++;
          dead_map_decoder map(record);

          if (_mod_par->get_verbosity() >= FVTXOO::MAX)
            map.print();

          load_dead_chan(map, nrows);
        }

      nrows++;

      cout << "mFvtxFindClus::load_dead_chan_map - Info - Finished loading "
          << cnt << " dead channel locators from map #" << nrows << ", "
          << keyword << " (" << comment << "), " << " in database fvtx/"
          << dead_map_name << endl;
    }

  cout
      << "mFvtxFindClus::load_dead_chan_map - Info - Finished loading dead channel map "
      << dead_map_name << " with " << nrows << " dead maps" << endl;

  if (rs)
    delete rs; // close the data base connection
  if (stmt)
    delete stmt;
//  delete con; // close the data base connection
}

void
mFvtxFindClus::load_dead_chan(const mFvtxFindClus::dead_map_decoder &map,
    const int nrows)
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
          for (int i = 0; i < FVTXOO::MAX_ARM; i++)
            {
              FvtxArm * fvtx_arm = FvtxGeom::get_arm(i);
              fvtx_arm->set_active(false);
            }

          cout
              << "mFvtxFindClus::load_dead_chan - WARNING: disable all FVTX due to dead map record # "
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
                  << "mFvtxFindClus::load_dead_chan - WARNING: disable whole FVTX arm due to dead map record # "
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
                      << "mFvtxFindClus::load_dead_chan - WARNING: disable whole FVTX cage due to dead map record # "
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
                          << "mFvtxFindClus::load_dead_chan - WARNING: disable whole FVTX half disk due to dead map record # "
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
      FVTXOO::TRACE(
          string("mFvtxFindClus::load_dead_chan - load map error : ")
              + e.what());

      cout
          << "mFvtxFindClus::load_dead_chan - load map error : Offending record # "
          << nrows << ": arm " << arm << " cage " << cage << " station "
          << station << " sector " << sector << " side " << side << " strip "
          << strip << endl;
    }

}

PHBoolean
mFvtxFindClus::event(PHCompositeNode* top_node)
{
  _timer.get()->restart();

  try
    {

      //Reset IOC pointers
      set_interface_ptrs(top_node);

      //clear cluster map
      _clus_map->clear();

      //associate groups of contiguous hits with TFvtxClus objects
      find_clusters();

      //apply run time specified cluster cuts
      apply_cluster_cuts();

    }
  catch (std::exception& e)
    {
      FVTXOO::TRACE(e.what());
      return False;
    }

  //if verbose dump the contents of the cluster map
  _timer.get()->stop();
  if (_mod_par->get_verbosity() >= FVTXOO::ALOT)
    _clus_map->print();
  if (_mod_par->get_verbosity() >= FVTXOO::SOME)
    _timer.get()->print();

  return True;
}

void
mFvtxFindClus::set_interface_ptrs(PHCompositeNode* top_node)
{
  _mod_par = TMutNode<mFvtxFindClusPar>::find_node(top_node,
      "mFvtxFindClusPar");
  _hit_map = TMutNode<TFvtxHitMap>::find_node(top_node, "TFvtxHitMap");
  _clus_map = TMutNode<TFvtxClusMap>::find_node(top_node, "TFvtxClusMap");
}

void
mFvtxFindClus::find_clusters()
{

  if (!_mod_par)
    {
      cout
          << "mFvtxFindClus::find_clusters - Error - cannot find mFvtxFindClusPar, quit ..."
          << endl;

      return;
    }
  const bool verb = _mod_par->get_verbosity() >= FVTXOO::ALOT;

  try
    {

      //leave space for additional cluster cuts later
      for (int arm = 0; arm < FVTXOO::MAX_ARM; arm++)
        {
          FvtxArm * fvtx_arm = FvtxGeom::get_arm(arm);

          if (!fvtx_arm->is_active())
            {
              if (verb)
                cout << "mFvtxFindClus::find_clusters - Info - Ignore arm "
                    << arm << endl;

              TFvtxHitMap::iterator hit_iter = _hit_map->get(arm //
                  );
              while (TFvtxHitMap::pointer hit_ptr = hit_iter.next())
                {
                  unsigned long st = hit_ptr->get()->get_status();
                  hit_ptr->get()->set_status(st | TFvtxHit::masked);
                }
            }
          else
            for (int cage = 0; cage < FVTXOO::MAX_CAGE; cage++)
              {
                FvtxCage * fvtx_cage = fvtx_arm->get_cage(cage);

                if (!fvtx_cage->is_active())
                  {
                    if (verb)
                      cout << "mFvtxFindClus::find_clusters - Info - Ignore "
                          << "arm " << arm << " cage " << cage << endl;

                    TFvtxHitMap::iterator hit_iter = _hit_map->get(arm //
                        , cage //
                        );
                    while (TFvtxHitMap::pointer hit_ptr = hit_iter.next())
                      {
                        unsigned long st = hit_ptr->get()->get_status();
                        hit_ptr->get()->set_status(st | TFvtxHit::masked);
                      }
                  }
                else
                  for (int station = 0; station < FVTXOO::MAX_STATION;
                      station++)
                    {
                      FvtxStation * fvtx_station = fvtx_cage->get_station(
                          station);

                      if (!fvtx_station->is_active())
                        {
                          if (verb)
                            cout
                                << "mFvtxFindClus::find_clusters - Info - Ignore "
                                << "arm " << arm << " cage " << cage
                                << " station " << station << endl;

                          TFvtxHitMap::iterator hit_iter = _hit_map->get(arm //
                              , cage //
                              , station //
                              );
                          while (TFvtxHitMap::pointer hit_ptr = hit_iter.next())
                            {
                              unsigned long st = hit_ptr->get()->get_status();
                              hit_ptr->get()->set_status(st | TFvtxHit::masked);
                            }
                        }
                      else
                        for (int sector = 0; sector < FVTXOO::MAX_SECTOR;
                            sector++)
                          {
                            FvtxSector * fvtx_sector = fvtx_station->get_sector(
                                sector);

                            if (!fvtx_sector->is_active())
                              {
                                if (verb)
                                  cout
                                      << "mFvtxFindClus::find_clusters - Info - Ignore "
                                      << "arm " << arm << " cage " << cage
                                      << " station " << station << " sector "
                                      << sector << endl;

                                TFvtxHitMap::iterator hit_iter = _hit_map->get(
                                    arm //
                                    , cage //
                                    , station //
                                    , sector //
                                    );
                                while (TFvtxHitMap::pointer hit_ptr =
                                    hit_iter.next())
                                  {
                                    unsigned long st =
                                        hit_ptr->get()->get_status();
                                    hit_ptr->get()->set_status(
                                        st | TFvtxHit::masked);
                                  }
                              }
                            else
                              for (int column = 0; column < FVTXOO::MAX_COLUMN;
                                  column++)
                                {

                                  FvtxColumn * fvtx_column =
                                      fvtx_sector->get_column(column);

                                  if (!fvtx_column->is_active())
                                    {
                                      if (verb)
                                        cout
                                            << "mFvtxFindClus::find_clusters - Info - Ignore "
                                            << "arm " << arm << " cage " << cage
                                            << " station " << station
                                            << " sector " << sector
                                            << " column " << column << endl;

                                      TFvtxHitMap::iterator hit_iter =
                                          _hit_map->get(arm //
                                              , cage //
                                              , station //
                                              , sector //
                                              , column //
                                              );
                                      while (TFvtxHitMap::pointer hit_ptr =
                                          hit_iter.next())
                                        {
                                          unsigned long st =
                                              hit_ptr->get()->get_status();
                                          hit_ptr->get()->set_status(
                                              st | TFvtxHit::masked);
                                        }
                                    }
                                  else
                                    find_clusters(arm, cage, station, sector,
                                        column, fvtx_column);
                                } //   for (int column = 0; column < FVTXOO::MAX_COLUMN;
                          } // for (int sector = 0; sector < FVTXOO::MAX_SECTOR;
                    } // for (int station = 0; station < FVTXOO::MAX_STATION;
              } // for (int cage = 0; cage < FVTXOO::MAX_STATION; cage++)
        } //       for (int arm = 0; arm < FVTXOO::MAX_ARM; arm++)

    } // try
  catch (std::exception& e)
    {
      FVTXOO::TRACE(
          string("mFvtxFindClus::find_clusters - Error - ") + e.what());
//      continue;
    }

  return;
}

bool
mFvtxFindClus::find_clusters(const int& arm, const int& cage,
    const int& station, const int& sector, const int& column,
    const FvtxColumn * fvtx_column)
{

  int previous_strip = -1;
  TFvtxClusMap::pointer cluster_ptr = 0; //pointer to previous cluster
  TFvtxHitMap::pointer prev_hit_ptr = 0; //pointer to previous hit

  //iterator over hits in hit map
  TFvtxHitMap::iterator hit_iter = _hit_map->get(arm, cage, station, sector,
      column);
  while (TFvtxHitMap::pointer hit_ptr = hit_iter.next())
    {
      //get number of current strip
      int current_strip = hit_ptr->get()->get_strip();

      FvtxStrip * fvtx_strip = NULL;
      try
        {
          fvtx_strip = fvtx_column->get_strip(current_strip);
        }
      catch (std::exception& e)
        {
          FVTXOO::TRACE(
              string("mFvtxFindClus::find_clusters - Error locating strip - ")
                  + e.what());
          continue;
        }

      // this is a dead strip as defined in dead channel map
      if (!fvtx_strip->is_active())
        {
          if (_mod_par->get_verbosity() >= FVTXOO::ALOT)
            cout << "mFvtxFindClus::find_clusters - Info - Ignore hit "
                << "arm " << arm << " cage " << cage << " station " << station
                << " sector " << sector << " column " << column << " strip "
                << current_strip << endl;

          unsigned long st = hit_ptr->get()->get_status();
          hit_ptr->get()->set_status(st | TFvtxHit::masked);

          continue;
        }

      //make a new cluster if 1) this hit is the first one so there's no current cluster yet
      // or 2) the column locations of current and previous hits are different or
      // 3) the strip number has been incremented by more than 2 (more than one missing
      // strip between previous cluster and current cluster)

      bool new_cluster = (cluster_ptr == 0)
          || (current_strip > (previous_strip + 2));
      if (new_cluster && _mod_par->get_verbosity() >= FVTXOO::MAX)
        cout << "mFvtxFindClus::find_clusters - creating new cluster" << endl;

      if (!new_cluster)
        {

          if (current_strip == (previous_strip + 2))
            {

              //there's a strip missing
              // ask to make a new cluster if merging is allowed

              //if merging isn't allowed
              if (!(_mod_par->get_merge_adjacent_clusters()))
                new_cluster = true;
              else
                {
                  //can merge the clusters
                  if ((_mod_par->get_verbosity() >= FVTXOO::MAX))
                    cout
                        << "mFvtxFindClus::find_clusters - one strip missing between adjacent clusters"
                        << std::endl;

                  //make a new cluster if the missing strip isn't excused
                  if (!excuse_strip(fvtx_column, current_strip - 1))
                    new_cluster = true;
                  else
                    {
                      if (_mod_par->get_verbosity() >= FVTXOO::MAX)
                        cout
                            << "mFvtxFindClus::find_clusters - missing strip excused"
                            << std::endl;

                      TFvtxHitMap::iterator old_hit_iter = _hit_map->get(arm,
                          cage, station, sector, column, current_strip - 1);

                      //the missing strip is excused, but need to put it in the hit map
                      TFvtxHitMap::pointer missing_hit_ptr = NULL;

                      if (old_hit_iter.count() > 0)
                        {
                          if (_mod_par->get_verbosity() >= FVTXOO::MAX)
                            cout
                                << "mFvtxFindClus::find_clusters - already have an old hit, modify it ..."
                                << endl;

                          if (_mod_par->get_verbosity() >= FVTXOO::MAX)
                            {
                              assert(old_hit_iter.count()==1);
                            }

                          missing_hit_ptr = old_hit_iter.current(); // already have an old hit, modify it ...

                          if (_mod_par->get_verbosity() >= FVTXOO::MAX)
                            {
                              assert(missing_hit_ptr);
                              assert(missing_hit_ptr->get());
                            }
                        }
                      else
                        {
                          if (_mod_par->get_verbosity() >= FVTXOO::MAX)
                            cout
                                << "mFvtxFindClus::find_clusters - make a new missing hit ..."
                                << endl;

                          TFvtxHitMap::iterator newhit_iter =
                              _hit_map->insert_new(
                                  hit_ptr->get()->get_arm(), // make a new missing hit
                                  hit_ptr->get()->get_cage(),
                                  hit_ptr->get()->get_station(),
                                  hit_ptr->get()->get_sector(),
                                  hit_ptr->get()->get_column(),
                                  current_strip - 1);

                          if (_mod_par->get_verbosity() >= FVTXOO::MAX)
                            {
                              assert(newhit_iter.count()==1);
                            }

                          missing_hit_ptr = newhit_iter.current();

                          if (_mod_par->get_verbosity() >= FVTXOO::MAX)
                            {
                              assert(missing_hit_ptr);
                              assert(missing_hit_ptr->get());
                            }
                        }

                      //set charge as average of charges of adjacent strips
                      float missingStripCharge = 0.5
                          * (hit_ptr->get()->get_q()
                              + prev_hit_ptr->get()->get_q());
                      missing_hit_ptr->get()->set_q(missingStripCharge);

                      //set a missing status
                      unsigned long st = missing_hit_ptr->get()->get_status();
                      missing_hit_ptr->get()->set_status(
                          st | TFvtxHit::missing_hit);

                      //set a large error on the charge
                      missing_hit_ptr->get()->set_error_q(
                          10.0 * missingStripCharge);
                      if (_mod_par->get_verbosity() >= FVTXOO::MAX)
                        {
                          cout << "current cluster:  " << endl;
                          cluster_ptr->get()->print();
                        }

                      //associate the missing strip to current cluster
                      PHKey::associate(missing_hit_ptr, cluster_ptr);

                      if (_mod_par->get_verbosity() >= FVTXOO::MAX)
                        {
                          cout << "missing hit" << endl;
                          missing_hit_ptr->get()->print();
                          cout << "updated current cluster" << endl;
                          cluster_ptr->get()->print();
                          cout << "next adjacent hit" << endl;
                          hit_ptr->get()->print();
                        }
                    }

                }
            }
        }

      //if a new cluster is needed, make it
      if (new_cluster)
        cluster_ptr =
            _clus_map->insert_new(hit_ptr->get()->get_arm(),
                hit_ptr->get()->get_cage(), hit_ptr->get()->get_station(),
                hit_ptr->get()->get_sector(), hit_ptr->get()->get_column()).current();

      //associate the current hit to the current cluster
      PHKey::associate(hit_ptr, cluster_ptr);

      //increment strip number and previous hit pointer
      previous_strip = current_strip;
      prev_hit_ptr = hit_ptr;

    }

  //return true if new cluster was created
  return (cluster_ptr);
}

bool
mFvtxFindClus::excuse_strip(const FvtxColumn * fvtx_column, int strip)
{
  if (strip < 0)
    return false;

  FvtxStrip * fvtx_strip = NULL;
  try
    {
      fvtx_strip = fvtx_column->get_strip(strip);
    }
  catch (std::exception& e)
    {
      stringstream s;

      s << "mFvtxFindClus::excuse_strip - Error locating strip " << strip
          << " - " << e.what();

      FVTXOO::TRACE(s.str());
      return false;
    }

  return !(fvtx_strip->is_active());
}

void
mFvtxFindClus::apply_cluster_cuts()
{

  TFvtxClusMap::iterator clus_iter = _clus_map->range();

  if (_mod_par->get_verbosity() > FVTXOO::MAX)
    cout << "mFvtxFindClus::apply_cluster_cuts - n_clusters: "
        << clus_iter.count() << endl;

  while (TFvtxClusMap::pointer clus_ptr = clus_iter.next())
    {

      // remove cluster if it contains less than the required number of strips
      if (clus_ptr->get()->get_n_strip() < _mod_par->get_min_cluster_width())
        {
          _clus_map->erase(clus_ptr->get()->get_key());
          continue;
        }

      if (clus_ptr->get()->get_n_strip() > _mod_par->get_max_cluster_width())
        {
          _clus_map->erase(clus_ptr->get()->get_key());
          continue;
        }

    }
}
