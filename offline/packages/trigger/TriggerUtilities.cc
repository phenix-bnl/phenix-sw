#include <TriggerUtilities.h>

#include <Lvl1Struct.h>
#include <Lvl2Struct.h>

#include <TrigLvl1v2.h>
#include <TrigRunLvl1v3.h>
#include <TrigRunLvl2v3.h>

#include <msg_profile.h>
#include <msg_control.h>
#include <phool.h>

#include <PHTimeStamp.h>

// odbc++ includes

#include <odbc++/connection.h>
#include <odbc++/setup.h>
#include <odbc++/errorhandler.h>
#include <odbc++/drivermanager.h>
#include <odbc++/resultset.h>
#include <odbc++/resultsetmetadata.h>
#include <odbc++/preparedstatement.h>
#include <odbc++/databasemetadata.h>
#include <odbc++/types.h>

#include <cstdlib>
#include <ctime>
#include <iostream>
#include <memory>
#include <sstream>
#include <string>

#include <map>

using namespace odbc;
using namespace std;

TrigRunLvl1 *
TriggerUtilities::getNewTrigRunLvl1()
{
  return new TrigRunLvl1v3();
}

TrigRunLvl2 *
TriggerUtilities::getNewTrigRunLvl2()
{
  return new TrigRunLvl2v3();
}

TriggerUtilities::TriggerUtilities()
{
  verbosity = 0;
  _trigHelp = 0;
}

TriggerUtilities::~TriggerUtilities()
{
  cleanupTriggerHelper();
}

int
TriggerUtilities::dbFillTrigRunObjects(int runNumber,
                                       TrigRunLvl1* trigrunlvl1,
                                       TrigRunLvl2* trigrunlvl2)
{
  ostringstream msg;
  if (verbosity > 0)
    {
      msg << PHWHERE
	  << "doing database lookups for trigger helper objects-- "
	  << "this should only be done once per run";
      send_message(MSG_SEV_INFORMATIONAL, msg.str());
    }
  // TrigRunLvl1 --> set object pointer to null for initialization
  string lvl1_trigger_description;
  int lvl1_trigger_version = 0;
  string bbcll1_description;
  int bbcll1_version = 0;
  string lvl1_partition_name ;
  string lvl1_trigger_name[32];
  string lvl2_trigger_name[32];

  int lvl1_trigger_enable[32];
  memset(lvl1_trigger_enable, 0, sizeof(lvl1_trigger_enable));

  int lvl1_trigger_bit[32];
  memset(lvl1_trigger_bit, 0, sizeof(lvl1_trigger_bit));

  int lvl1_trigger_scaledown[32];
  memset(lvl1_trigger_scaledown, 0, sizeof(lvl1_trigger_scaledown));

  int lvl1_lvl2_reject_enable[32];
  memset(lvl1_lvl2_reject_enable, -1, sizeof(lvl1_lvl2_reject_enable));

  float lvl1_trigger_rate_begin[32];
  memset(lvl1_trigger_rate_begin, 0, sizeof(lvl1_trigger_rate_begin));

  int lvl1_lvl2_force_accept[32];
  memset(lvl1_lvl2_force_accept, 0, sizeof(lvl1_lvl2_force_accept));
  //     { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  //       0, 0, 0, 0, 0, 0, 0, 0, 0 };

  // TrigRunLvl2 --> set object pointer to null for initialization

  string lvl2_description;
  int lvl2_version = 0;
  int lvl2_run_enable = 0;
  int lvl2_reject_enable = 0;


  int lvl2_trigger_bit[32];
  memset(lvl2_trigger_bit, 0, sizeof(lvl2_trigger_bit));

  int lvl2_lvl1_assoc[64][32];
  memset(lvl2_lvl1_assoc, 0, sizeof(lvl2_lvl1_assoc));

  int lvl2_lvl1_prescale[64][32];
  memset(lvl2_lvl1_prescale, 0, sizeof(lvl2_lvl1_prescale));

  // this will be filled in later
  // int lvl2_trigger_bit[32] =
  //     { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  //       0, 0, 0, 0, 0, 0, 0, 0, 0 };

  map<string, int> lvl2namemap;
  map<string, int> lvl2namebitmap;


  //  Establish connection to Postgres...

  Connection *con = 0;
  try
    {
      con = DriverManager::getConnection("Phenix", "phnxrc", "");
    }
  catch (SQLException& e)
    {
      msg.str("");
      msg << PHWHERE
	  << " Exception caught during DriverManager::getConnection" ;
      send_message(MSG_SEV_SEVEREERROR, msg.str());
      msg.str("");
      msg << "Message: " << e.getMessage() ;
      send_message(MSG_SEV_SEVEREERROR, msg.str());
      if (con)
        {
          delete con;
        }
      return -1;
    }

  Statement *stmt = con->createStatement();

  msg.str("");
  msg << "select nlvl1trig, nlvl2trig, nlvl2algo, timestamp, trigconfigname, configversion from l2config where runnumber=" << runNumber << " limit 1" ;

  if (verbosity > 0)
    {
      send_message(MSG_SEV_INFORMATIONAL, msg.str());
    }
  //  Get results of search...
  ResultSet *rs = 0;
  try
    {
      rs = stmt->executeQuery(msg.str().c_str());
    }
  catch (SQLException& e)
    {
      msg.str("");
      msg << PHWHERE << "Exception caught"
	  << " Message: " << e.getMessage() ;
      send_message(MSG_SEV_ERROR, msg.str());
      delete stmt;
      delete con;
      return -1;
    }

  if (!rs->next())
    {
      msg.str("");

      msg << PHWHERE << "Error retrieving configuration for run."
	  << runNumber ;
      send_message(MSG_SEV_ERROR, msg.str());

      delete rs;
      delete stmt;
      delete con;
      return -1;
    }

  //
  // convert postgres time to PHTime, there is slight difference between RUN-3 and RUN-4:
  // fraction is missing in RUN-3.
  //
  Timestamp pgts;
  try
    {
      pgts = rs->getTimestamp("timestamp");
    }
  catch (SQLException& e)
    {
      msg.str("");
      msg << PHWHERE << "Exception caught"
	  << " Message: " << e.getMessage()
	  << ": triglvl1.start_time will be set to null, continuing...";
      send_message(MSG_SEV_ERROR, msg.str());

    }
  time_t startTicks = pgts.getTime();
  lvl1_trigger_description = rs->getString("trigconfigname");
  lvl1_trigger_version = rs->getInt("configversion");

  vector<Lvl1Trig> Lvl1TrigArray;
  vector<TrigAlgo> Lvl2TrigArray;
  vector<Lvl2Algo> Lvl2AlgoArray;
  if (verbosity > 0)
    {
      msg.str("");
      msg << "Reading sizes of lvl1 and lvl2 structure arrays" ;
      send_message(MSG_SEV_INFORMATIONAL, msg.str());
    }
  int lvl1trigsize = rs->getInt("nlvl1trig");
  int trigalgosize = rs->getInt("nlvl2trig");
  int lvl2algosize = rs->getInt("nlvl2algo");

  delete rs;
  rs = 0;
  delete stmt;
  stmt = 0;

  if (lvl1trigsize > 0)
    {
      for (int i = 0; i < lvl1trigsize; i++)
        {
          Lvl1Trig lvl1trigH;
          char* _result[6];
          for (int j = 0; j < 6; j++)
            {
              char _command[256];
              sprintf(_command, "select lvl1trigstruct[%d][%d] from l2config where runnumber=%d", i + 1, j + 1, runNumber);
              stmt = con->createStatement();
              rs = stmt->executeQuery(_command);

              if (!rs->next())
                {
                  msg.str("");
                  msg << PHWHERE << "Empty result for query: "
		      << _command ;
                  send_message(MSG_SEV_FATAL, msg.str());
                  delete rs;
                  delete stmt;
                  exit(1);
                }
              else
                {
                  string mys = rs->getString(1);
                  const char* tt = mys.c_str();
                  _result[j] = new char[strlen(tt) + 1];
                  strcpy(_result[j], tt);
                  delete rs;
		  rs = 0;
                }
            }

	  // Lvl1Trig::Lvl1BitName is a string
	  lvl1trigH.lvl1BitName.assign(_result[0]);
	  
          if (sscanf(_result[1], "%lu", &(lvl1trigH.lvl1Scaledown)) != 1)
            {
              msg.str("");
              msg << "Error reading lvl1 scaledown for bit: " << i ;
              send_message(MSG_SEV_FATAL, msg.str());
              exit(1);
            }
          if (sscanf(_result[2], "%lu", &(lvl1trigH.lvl1BitMask)) != 1)
            {
              msg.str("");
              msg << "Error reading lvl1 bit mask for bit: " << i ;
              send_message(MSG_SEV_FATAL, msg.str());
              exit(1);
            }
          if (sscanf(_result[3], "%lu", &(lvl1trigH.bitNumber)) != 1)
            {
              msg.str("");
              msg << "Error reading lvl1 bit number for bit: " << i ;
              send_message(MSG_SEV_FATAL, msg.str());
              exit(1);
            }
          if (sscanf(_result[4], "%lu", &(lvl1trigH.partition)) != 1)
            {
              msg.str("");
              msg << "Error reading lvl1 partition for bit: " << i ;
              send_message(MSG_SEV_FATAL, msg.str());
              exit(1);
            }
          if (sscanf(_result[5], "%f", &(lvl1trigH.rawScalerRate)) != 1)
            {
              msg.str("");
              msg << "Error reading lvl1 raw scaler rate for bit: " << i ;
              send_message(MSG_SEV_FATAL, msg.str());
              exit(1);
            }
          Lvl1TrigArray.push_back(lvl1trigH);
          for (int j = 0; j < 6; j++)
            {
              delete [] _result[j];
            }
        } // for (int i = 0; i < lvl1trigsize; i++)
    } // if (lvl1trigsize > 0)

  if (trigalgosize > 0)
    {
      for (int i = 0; i < trigalgosize; i++)
        {
          TrigAlgo trigalgoH;
          char* _result[8];
          for (int j = 0; j < 8; j++)
            {
              ostringstream _command;
              _command << "select lvl2trigalgo[" << (i + 1)
		       << "][" << (j + 1)
		       << "] from l2config where runnumber=" << runNumber ;
              stmt = con->createStatement();
              rs = stmt->executeQuery(_command.str().c_str());
              if (!rs->next())
                {
                  msg.str("");
                  msg << PHWHERE << "Empty result for query: " << _command.str() ;
                  send_message(MSG_SEV_FATAL, msg.str());
                  delete rs;
                  delete stmt;
                  exit(1);
                }
              string mys = rs->getString(1);
              const char * tt = mys.c_str();
              _result[j] = new char[strlen(tt) + 1];
              strcpy(_result[j], tt);
              delete rs;
	      rs = 0;
              delete stmt;
	      stmt = 0;
            }

	  trigalgoH.name.assign(_result[0]);

          if (sscanf(_result[1], "%lu", &(trigalgoH.scaledown)) != 1)
            {
              msg.str("");
              msg << "Error reading trigger algorithm scaledown: " << i ;
              send_message(MSG_SEV_FATAL, msg.str());
              exit(1);
            }
          if (sscanf(_result[2], "%d", reinterpret_cast<int*>(&trigalgoH.algoInitialMode)) != 1)
            {
              msg.str("");
              msg << "Error reading initial lvl2 algorithm mode: " << i ;
              send_message(MSG_SEV_FATAL, msg.str());
              exit(1);
            }

	  trigalgoH.lvl1BitName.assign(_result[3]);

          if (sscanf(_result[4], "%lu", &(trigalgoH.bitNumber)) != 1)
            {
              msg.str("");
              msg << "Error reading bit number for trigger: " << i ;
              send_message(MSG_SEV_FATAL, msg.str());
              exit(1);
            }
          if (sscanf(_result[5], "%lu", &(trigalgoH.partition)) != 1)
            {
              msg.str("");
              msg << "Error reading partition for trigger: " << i ;
              send_message(MSG_SEV_FATAL, msg.str());
              exit(1);
            }
          if (sscanf(_result[6], "%lu", &(trigalgoH.forcedAcceptN)) != 1)
            {
              msg.str("");
              msg << "Error reading forced accept number for trigger: " << i ;
              send_message(MSG_SEV_FATAL, msg.str());
              exit(1);
            }
          if (sscanf(_result[7], "%d", reinterpret_cast<int*>(&trigalgoH.bitLvl2Mode)) != 1)
            {
              msg.str("");
              msg << "Error reading lvl2 bit mode for trigger: " << i ;
              send_message(MSG_SEV_FATAL, msg.str());
              exit(1);
            }
          Lvl2TrigArray.push_back(trigalgoH);
          for (int j = 0; j < 8; j++)
	    {
              delete [] _result[j];
	    }
        } // for (int i = 0; i < trigalgosize; i++)
    } // if (trigalgosize > 0)

  if (lvl2algosize > 0)
    {
      for (int i = 0; i < lvl2algosize; i++)
        {
          Lvl2Algo algoH;
          char* _result[2];
          for (int j = 0; j < 2; j++)
            {
              ostringstream _command;
              _command << "select lvl2algo[" << (i + 1) << "][" << (j + 1)
		       << "] from l2config where runnumber=" << runNumber ;
              stmt = con->createStatement();
              rs = stmt->executeQuery(_command.str().c_str());

              if (!rs->next())
                {
                  msg.str("");
                  msg << PHWHERE << "Empty result for query: " << _command.str() ;
                  send_message(MSG_SEV_FATAL, msg.str());
                  delete rs;
                  delete stmt;
                  exit(1);
                }
              string mys = rs->getString(1);
              const char * tt = mys.c_str();
              _result[j] = new char[strlen(tt) + 1];
              strcpy(_result[j], tt);
              delete rs;
	      rs = 0;
              delete stmt;
	      stmt = 0;
            }

	  algoH.algoName.assign(_result[0]);

          if (sscanf(_result[1], "%lu", &(algoH.algoIndex)) != 1)
            {
              msg.str("");
              msg << "Error reading algorithm index: " << i ;
              send_message(MSG_SEV_FATAL, msg.str());
              exit(1);
            }
          Lvl2AlgoArray.push_back(algoH);
          for (int j = 0; j < 2; j++)
            {
              delete [] _result[j];
            }
        } // for (int i = 0; i < lvl2alogsize; i++)
    } // if (lvl2algosize > 0)

//   delete rs;
//   delete stmt;
  delete con;

  /////////////////
  //We extracted every thing already, now fill the array
  ////////////////

  for (int i = 0; i < lvl1trigsize; i++)
    {
      int part = 0;
      if (Lvl1TrigArray[i].partition == 9999)
        {
          part = -1;
        }
      else
        {
          part = Lvl1TrigArray[i].partition;
        }
      // Gobinda doesn't store the name, only the number
      // presumably we could do a lookup from
      // Sergey's db of the configuration itself
      // but does any one use this?
      //
      ostringstream partName;
      partName << part ;
      lvl1_partition_name = partName.str();

      lvl1_trigger_name[i] = Lvl1TrigArray[i].lvl1BitName;
      lvl1_trigger_scaledown[i] = Lvl1TrigArray[i].lvl1Scaledown;
      lvl1_trigger_enable[i] = Lvl1TrigArray[i].lvl1BitMask;
      lvl1_trigger_bit[i] = Lvl1TrigArray[i].bitNumber;
      lvl1_trigger_rate_begin[i] = Lvl1TrigArray[i].rawScalerRate;
    }

  /// Lvl1 done, start lvl2/////

  if (trigalgosize > 0)
    {
      lvl2_reject_enable = 1;
      lvl2_run_enable = 1;
    }
  else
    {
      lvl2_reject_enable = 0;
      lvl2_run_enable = 0;
    }
  lvl2_description = "Level-2 Foo Foo";          // not yet into database
  lvl2_version = 1;                          // not yet into database
  int lvl2ArrayLength = trigalgosize;

  int indexPlus1 = 1;
  map <int, int> opmodemap;
  opmodemap[EvBLvl2disabled] = 3;
  opmodemap[EvBLvl2enabledNoReject] = 1;
  opmodemap[EvBLvl2enabledReject] = 2;
  // this is allow backward compatibiity from run2
  // while allowing run3 "disabled" mode to be contained
  // in lvl1_lvl2_reject_enabled  (in run2, reject_enabled = 0 ==
  // run3 EvBlvl2enabledNoReject, so I set this key's opmap value
  // to 1 + 0.  The 1 + is just because for stl maps, 0 means undefined
  // which is also why indexPlus1 is not just index
  for (int i = 0; i < lvl2ArrayLength; i++)
    {
      TrigAlgo lvl2trig = Lvl2TrigArray[i];
      int lvl1bit = lvl2trig.bitNumber;
      if (lvl1_lvl2_reject_enable[lvl1bit] < 0)
        {
          lvl1_lvl2_reject_enable[lvl1bit] = opmodemap[lvl2trig.bitLvl2Mode] - 1;
        }
      if (lvl1_lvl2_reject_enable[lvl1bit] != opmodemap[lvl2trig.bitLvl2Mode] - 1)
        {
          msg.str("");
          msg << PHWHERE
	      << "inconsistency in Lvl2 reject enabled status for lvl1 bit: "
	      << lvl1bit;
          send_message(MSG_SEV_ERROR, msg.str());
        }
      // inconsitency because this value should be the same for all lvl2's associated
      // with the same lvl1

      if (lvl2namemap[lvl2trig.name] < 1)
        {
          lvl2namemap[lvl2trig.name] = indexPlus1++;
        }
      // lvl2 version? probably could/should be filled in with lvl2bit (ie. later)
      lvl2_lvl1_prescale[ lvl2namemap[lvl2trig.name] - 1 ][lvl1bit] = lvl2trig.scaledown;
      lvl2_lvl1_assoc[ lvl2namemap[lvl2trig.name] - 1 ][lvl1bit] = 1;
      lvl2_trigger_name[lvl2namemap[lvl2trig.name] - 1] = lvl2trig.name;

      int alglen = lvl2algosize;
      string strlvl2trigname = lvl2trig.name;
      for (int kalg = 0; kalg < alglen; kalg++)
        {
          Lvl2Algo lvl2alg = Lvl2AlgoArray[kalg];
          string strlvl2algname = lvl2alg.algoName;
          if (strlvl2trigname == strlvl2algname)
            {
              lvl2_trigger_bit[lvl2namemap[lvl2trig.name] - 1]
		= lvl2alg.algoIndex;
              break;
            }
        } // loop over algo's

    } // loop over trigAlgos which are lvl2/lvl1 associations

  // fill in TrigRunLvl1 Object
  if (trigrunlvl1)
    {
      trigrunlvl1->set_run_number(runNumber);
      trigrunlvl1->set_start_time(startTicks);
      if (lvl1_trigger_description != "")
        {
          trigrunlvl1->set_lvl1_trigger_description(lvl1_trigger_description.c_str());
          trigrunlvl1->set_lvl1_trigger_version(lvl1_trigger_version);
        }
      if (bbcll1_description != "")
        {
          trigrunlvl1->set_lvl1_bbcll1_description(bbcll1_description.c_str());
          trigrunlvl1->set_lvl1_bbcll1_version(bbcll1_version);
        }
      if (lvl1_partition_name != "")
        {
          trigrunlvl1->set_lvl1_partition_name(lvl1_partition_name.c_str());
        }
      // initialize all triggers as disabled
      for (int i = 0; i < 32; i++)
        {
          trigrunlvl1->set_lvl1_trigger_enable(0, i);
        }

      for (int i = 0; i < 32; i++)
        {
          trigrunlvl1->set_lvl1_trig_name(lvl1_trigger_name[i].c_str(), i);
          trigrunlvl1->set_lvl1_trig_bitmask(lvl1_trigger_enable[i], i);
          int enable = 0;
          // very confusing bitmask field to enable field mapping
          if (lvl1_trigger_enable[i] == 0)
            {
              enable = 1;
            }
          trigrunlvl1->set_lvl1_trigger_enable(enable, i);
          trigrunlvl1->set_lvl1_trig_bit(lvl1_trigger_bit[i], i);
          trigrunlvl1->set_lvl1_trig_scale_down(lvl1_trigger_scaledown[i], i);
          trigrunlvl1->set_lvl1_lvl2_reject_enable(lvl1_lvl2_reject_enable[i], i);
          trigrunlvl1->set_lvl1_trig_rate_begin(lvl1_trigger_rate_begin[i], i);
        }
      trigrunlvl1->dump_info(trigrunlvl1);
    }

  // fill in TrigRunLvl2 Object
  if (trigrunlvl2)
    {
      trigrunlvl2->set_lvl2_description(lvl2_description.c_str());
      trigrunlvl2->set_lvl2_version(lvl2_version);
      trigrunlvl2->set_lvl2_run_enable(lvl2_run_enable);
      trigrunlvl2->set_lvl2_reject_enable(lvl2_reject_enable);

      for (int lvl1bit = 0; lvl1bit < 32; lvl1bit++)
        {
          trigrunlvl2->set_lvl1_lvl2_force_accept(lvl1_lvl2_force_accept[lvl1bit], lvl1bit);
          trigrunlvl2->set_lvl2_trig_name(lvl2_trigger_name[lvl1bit].c_str(), lvl1bit);
          trigrunlvl2->set_lvl2_trig_bit(lvl2_trigger_bit[lvl1bit], lvl1bit);
        }
      for (int lvl2bit = 0; lvl2bit < 64; lvl2bit++)
        {
          for (int lvl1bit = 0; lvl1bit < 32; lvl1bit++)
            {
              trigrunlvl2->set_lvl2_lvl1_prescale(lvl2_lvl1_prescale[lvl2bit][lvl1bit], lvl2bit, lvl1bit);
              trigrunlvl2->set_lvl2_lvl1_assoc(lvl2_lvl1_prescale[lvl2bit][lvl1bit], lvl2bit, lvl1bit);
            }
        }
      trigrunlvl2->dump_info(trigrunlvl2);
    }

  return 1;
}

int
TriggerUtilities::send_message(const int severity, const string &err_message)
{
  msg_control *Message = new msg_control(MSG_TYPE_OFFLINE,
                                         MSG_SOURCE_LVL1,
                                         severity, "TriggerUtilities");
  cout << *Message << err_message << endl;
  delete Message;
  return 0;
}
