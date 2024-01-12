#include "TriggerAccounting.h"

#include <Fun4AllServer.h>
#include <Fun4AllReturnCodes.h>
#include <TrigLvl1.h>
#include <TrigRunLvl1.h>
#include <Event.h>
#include <EventTypes.h>

#include <recoConsts.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>

#include <phool.h>
#include <getClass.h>

#include <boost/foreach.hpp>

#include <odbc++/connection.h>
#include <odbc++/errorhandler.h>
#include <odbc++/drivermanager.h>
#include <odbc++/preparedstatement.h>
#include <odbc++/resultset.h>
#include <odbc++/resultsetmetadata.h>

#include <algorithm>
#include <iostream>
#include <sstream>

using namespace std;

static odbc::Connection *con = 0;

TriggerAccounting::TriggerAccounting(const string &name):
  SubsysReco(name),
  indextable("trgaccindex")
{
  // copied from EventTypes.h
  eventTypeNames[1] = "DATAEVENT";
  eventTypeNames[2] = "DATA2EVENT";
  eventTypeNames[3] = "DATA3EVENT";
  eventTypeNames[7] = "REJECTEDEVENT";
  eventTypeNames[8] = "SPILLONEVENT";
  eventTypeNames[9] = "BEGRUNEVENT";
  eventTypeNames[11] = "RUNINFOEVENT";
  eventTypeNames[12] = "ENDRUNEVENT";
  eventTypeNames[14] = "SCALEREVENT";
  eventTypeNames[16] = "SPILLOFFEVENT";

  return ;
}

TriggerAccounting::~TriggerAccounting()
{
  delete con;
  return;
}

int
TriggerAccounting::InitRun(PHCompositeNode *topNode)
{
  memset(itrig, 0, sizeof(itrig));
  TrigRunLvl1* trigrunlvl1 = findNode::getClass<TrigRunLvl1>(topNode, "TrigRunLvl1");
  for (int i = 0; i < 32; i++)
    {
      if (trigrunlvl1-> get_lvl1_trig_name_bybit(i))
        {
          trigname[i] = trigrunlvl1-> get_lvl1_trig_name_bybit(i);
          if (verbosity > 1)
            {
              cout << "Adding trigger to list: " << trigname[i] << endl;
            }
        }
    }
  return 0;
}


int
TriggerAccounting::process_event(PHCompositeNode *topNode)
{
  Event *evt = findNode::getClass<Event>(topNode, "PRDF");
  eventtypes[evt->getEvtType()]++;
  if (evt->getEvtType() != DATAEVENT)
    {
      return EVENT_OK;
    }
  TrigLvl1* triglvl1 = findNode::getClass<TrigLvl1>(topNode, "TrigLvl1");
  unsigned int scaledtrig = triglvl1->get_lvl1_trigscaled();
  for (int i = 0; i < 32; i++)
    {
      if (scaledtrig&0x1)
        {
          itrig[i]++;
        }
      scaledtrig >>= 1;
      if (!scaledtrig)
        {
          break;
        }
    }

  return EVENT_OK;
}

int
TriggerAccounting::EndRun(const int runno)
{
  runs.insert(runno);
  map<string, pair<string, int> >::iterator trgiter;
  for (int i = 0; i < 32; i++)
    {
      if (!trigname[i].empty())
        {
          trgiter = trigfired.find(trigname[i]);
          if (trgiter == trigfired.end())
            {
              trigfired[trigname[i]] = make_pair("", itrig[i]);
            }
          else
            {
              (trgiter->second).second += itrig[i];
            }
        }
    }
  return 0;
}

int
TriggerAccounting::End(PHCompositeNode *topNode)
{
  Fun4AllServer *se = Fun4AllServer::instance();
  vector<string> fnames;
  se->GetInputFullFileList(fnames);
  vector<string>::iterator fiter;
  ostringstream filelist;
  int ifirst = 1;
  for (fiter = fnames.begin(); fiter != fnames.end(); fiter++)
    {
      string::size_type pos1 = (*fiter).find_last_of("/");
      if (pos1 != string::npos)
        {
          *fiter = (*fiter).substr(pos1 + 1);
        }
      if (!ifirst)
        {
          filelist << " ";
        }
      else
        {
          ifirst = 0;
        }
      filelist << *fiter;
    }
  map<string, pair<string, int> >::const_iterator trgiter;
  if (verbosity > 0)
    {
      for (trgiter = trigfired.begin(); trgiter != trigfired.end(); trgiter++)
        {
          cout << trgiter->first << " fired " << (trgiter->second).second << " times" << endl;
        }
    }
  recoConsts *rc = recoConsts::instance();
  if (! rc->FlagExist("CVSTAG"))
    {
      return 0;
    }
  string tablename = "triggeracct_";
  tablename += rc->get_CharFlag("CVSTAG");
  indextable += "_";
  indextable += rc->get_CharFlag("CVSTAG");
  // The bizarre cast here to convert the tablename to lower chars is needed for newer gccs
  transform(tablename.begin(), tablename.end(), tablename.begin(), (int(*)(int))tolower);
  transform(indextable.begin(), indextable.end(), indextable.begin(), (int(*)(int))tolower);
  if (CheckAndCreateIndexTable(indextable))
    {
      return -1;
    }
  if (CheckAndCreateTable(tablename))
    {
      return -1;
    }
  int runnumber = -1;
  if (runs.size() == 1) // exactly one entry means all files were from same run
    {
      runnumber = *(runs.begin());
    }
  if (InsertNewRow(tablename, filelist.str(), runnumber))
    {
      return -1;
    }
  if (AddTriggers(tablename, filelist.str(), runnumber))
    {
      return -1;
    }
  if (AddEventTypes(tablename, filelist.str(), runnumber))
    {
      return -1;
    }

  return 0;
}

int
TriggerAccounting::GetConnection()
{
  if (con)
    {
      return 0;
    }
  try
    {
      con = odbc::DriverManager::getConnection("phnxreco_odbc", "phnxrecouser", "");
    }
  catch (odbc::SQLException& e)
    {
      cout << PHWHERE
           << " Exception caught during DriverManager::getConnection" << endl;
      cout << "Message: " << e.getMessage() << endl;
      delete con;
      con = 0;
      return -1;
    }
  return 0;
}

// this code is really ugly with cut and paste but it is only executed
// once to create the table.
int
TriggerAccounting::CheckAndCreateTable(const std::string &tablename)
{
  if (GetConnection())
    {
      return -1;
    }
  ostringstream cmd;
  cmd << "select * from pg_tables where tablename = '" << tablename << "'";
  odbc::Statement* stmt = con->createStatement();
  if (verbosity > 0)
    {
      cout << "cmd: " << cmd.str() << endl;
    }

  odbc::ResultSet *rs = 0;
  try
    {
      rs = stmt->executeQuery(cmd.str());
    }
  catch (odbc::SQLException& e)
    {
      cout << tablename << " does not exist, creating it" << endl;
    }
  cmd.str("");
  if (! rs->next())
    {
      delete rs;
      rs = 0;
      cmd << "CREATE TABLE " << tablename << "(filename text NOT NULL, runnumber int NOT NULL, inserttime bigint NOT NULL";
      cmd << ", primary key(filename,runnumber))";
      if (verbosity > 0)
        {
          cout << "Executing " << cmd.str() << endl;
        }
      try
        {
          stmt->executeUpdate(cmd.str());
        }
      catch (odbc::SQLException& e)
        {
          cout << "Exception caught, Message: " << e.getMessage() << endl;
        }
      cmd.str("");
      cmd << "GRANT ALL ON " << tablename << " TO phnxrc";
      if (verbosity > 0)
        {
          cout << "Executing " << cmd.str() << endl;
        }
      try
        {
          stmt->executeUpdate(cmd.str());
        }
      catch (odbc::SQLException& e)
        {
          cout << "Exception caught, Message: " << e.getMessage() << endl;
        }
      cmd.str("");
    }
  delete stmt;

  return 0;
}


// this code is really ugly with cut and paste but it is only executed
// once to create the index table.
int
TriggerAccounting::CheckAndCreateIndexTable(const std::string &tablename)
{
  if (GetConnection())
    {
      return -1;
    }
  ostringstream cmd;
  cmd << "select * from pg_tables where tablename = '" << tablename << "'";
  odbc::Statement* stmt = con->createStatement();
  if (verbosity > 0)
    {
      cout << "cmd: " << cmd.str() << endl;
    }

  odbc::ResultSet *rs = 0;
  try
    {
      rs = stmt->executeQuery(cmd.str());
    }
  catch (odbc::SQLException& e)
    {
      cout << tablename << " does not exist, creating it" << endl;
    }
  cmd.str("");
  if (! rs->next())
    {
      delete rs;
      rs = 0;
      cmd << "CREATE TABLE " << tablename << "(name text NOT NULL, id text, index serial";
      cmd << ", primary key(name))";
      if (verbosity > 0)
        {
          cout << "Executing " << cmd.str() << endl;
        }
      try
        {
          stmt->executeUpdate(cmd.str());
        }
      catch (odbc::SQLException& e)
        {
          cout << "Exception caught, Message: " << e.getMessage() << endl;
        }
      cmd.str("");
      cmd << "GRANT ALL ON " << tablename << " TO phnxrc";
      if (verbosity > 0)
        {
          cout << "Executing " << cmd.str() << endl;
        }
      try
        {
          stmt->executeUpdate(cmd.str());
        }
      catch (odbc::SQLException& e)
        {
          cout << "Exception caught, Message: " << e.getMessage() << endl;
        }
      string tmptab = tablename;
      tmptab += "_index_seq";
      cmd.str("");
      cmd << "GRANT ALL ON SEQUENCE " << tmptab << " TO phnxrc";
      if (verbosity > 0)
        {
          cout << "Executing " << cmd.str() << endl;
        }
      try
        {
          stmt->executeUpdate(cmd.str());
        }
      catch (odbc::SQLException& e)
        {
          cout << "Exception caught, Message: " << e.getMessage() << endl;
        }
    }
  delete stmt;

  return 0;
}

int
TriggerAccounting::InsertNewRow(const std::string &tablename, const std::string &filename, const int runnumber)
{
  if (GetConnection())
    {
      return -1;
    }
  ostringstream cmd;
  cmd << "select * from " << tablename << " where filename = '" << filename
      << "' and runnumber = " << runnumber;

  odbc::Statement* stmt = con->createStatement();
  odbc::ResultSet *rs = 0;
  try
    {
      rs = stmt->executeQuery(cmd.str());
    }
  catch (odbc::SQLException& e)
    {
      cout << PHWHERE << " Exception caught: " << e.getMessage() << endl;
    }
  if (rs->next())
    {
      cmd.str("");
      cmd << "delete from " << tablename << " where filename = '" << filename
	  << "' and runnumber = " << runnumber;

      odbc::Statement* stmt2 = con->createStatement();
      try
        {
          stmt2->executeUpdate(cmd.str());
        }
      catch (odbc::SQLException& e)
        {
          cout << PHWHERE << " exception caught: " << e.getMessage() << endl;
        }
      delete stmt2;
    }
  delete rs;
  cmd.str("");
  cmd << "insert into " << tablename << " (filename, runnumber, inserttime) values ('" << filename
      << "', " << runnumber << ", " << time(0) << ")";
  try
    {
      stmt->executeUpdate(cmd.str());
    }
  catch (odbc::SQLException& e)
    {
      cout << PHWHERE << " exception caught: " << e.getMessage() << endl;
    }
  if (verbosity > 0)
    {
      cout << PHWHERE << " executed " << cmd.str() << endl;
    }
  delete stmt;
  return 0;
}

int
TriggerAccounting::AddTriggers(const std::string &tablename, const std::string &filename, const int runnumber)
{
  if (GetConnection())
    {
      return -1;
    }
  FillIndexTable(trigfired, "trg");
  AddMissingColumns(trigfired, tablename);
  odbc::Statement* stmt = con->createStatement();
  ostringstream cmd;
  map<string, pair<string, int> >::const_iterator trgiter;
  for (trgiter = trigfired.begin(); trgiter != trigfired.end(); trgiter++)
    {
      cmd.str("");
      cmd << "update " << tablename << " set " << (trgiter->second).first
	  << " = " << (trgiter->second).second
	  << " where runnumber = " << runnumber
	  << " and filename = '" << filename << "'";
      stmt->executeUpdate(cmd.str());
    }
  return 0;
}

int
TriggerAccounting::AddEventTypes(const std::string &tablename, const std::string &filename, const int runnumber)
{
  if (GetConnection())
    {
      return -1;
    }
  map<int, int>::const_iterator titer;
  map<int, string>::const_iterator evtypeiter;
  for (titer = eventtypes.begin(); titer != eventtypes.end(); titer++)
    {
      evtypeiter = eventTypeNames.find(titer->first);
      if (evtypeiter == eventTypeNames.end())
        {
          cout << PHWHERE << " Unknown Event Type found " << titer->first
               << ", please add it, recompile and try again" << endl;
          delete con;
          exit(1);
        }
      pair<string, int> pr = make_pair( "", titer->second);
      evttypesfired[evtypeiter->second] = pr;
      if (verbosity > 1)
        {
          cout << "event type " << evtypeiter->second << " added, counts "
               << titer->second << endl;
        }
    }

  FillIndexTable(evttypesfired, "evttype");
  AddMissingColumns(evttypesfired, tablename);
  odbc::Statement* stmt = con->createStatement();
  ostringstream cmd;
  map<string, pair<string, int> >::const_iterator evtiter;
  for (evtiter = evttypesfired.begin(); evtiter != evttypesfired.end(); evtiter++)
    {
      cmd.str("");
      cmd << "update " << tablename << " set " << (evtiter->second).first
	  << " = " << (evtiter->second).second
	  << " where runnumber = " << runnumber
	  << " and filename = '" << filename << "'";
      stmt->executeUpdate(cmd.str());
    }
  return 0;
}

int
TriggerAccounting::FillIndexTable(std::map<std::string, std::pair<std::string, int> > &map, const string &prefix)
{
  ostringstream cmd;
  cmd << "select index from " << indextable << " where name = ?" << endl;
  odbc::PreparedStatement *prep = con->prepareStatement(cmd.str());
  cmd.str("");
  cmd << "insert into " << indextable << " (name) values (?)";
  odbc::PreparedStatement *prepinsert = con->prepareStatement(cmd.str());
  cmd.str("");
  cmd << "update " << indextable << " set id = ? where name = ?";
  odbc::PreparedStatement *prepupdate = con->prepareStatement(cmd.str());
  odbc::ResultSet *rs  = NULL;
  std::map<std::string, std::pair<std::string, int> >::iterator iter;
  for (iter = map.begin(); iter != map.end(); iter++)
    {
      int index;
      if (verbosity > 2)
        {
          cout << "trigger: " << iter->first << " events: "
               << (iter->second).second << endl;
        }
      prep->setString(1, iter->first);
      rs = prep->executeQuery();
      if (!rs->next()) // no entry for this trigger
        {
          if (verbosity > 1)
            {
              cout << "inserting " << iter->first << " into index table" << endl;
            }
          prepinsert->setString(1, iter->first);
          try
            {
              prepinsert->executeUpdate();
            }
          catch (odbc::SQLException& e)
            {
              cout << PHWHERE << " Exception caught, probably benign race condition " << endl;
              cout << e.getMessage() << endl;
            }
          delete rs;
          rs = prep->executeQuery();
          if (rs->next())
            {
              index = rs->getInt("index");
            }
          else
            {
              cout << PHWHERE << " could not read back index of inserted trigger "
                   << iter->first << " exiting" << endl;
              delete con;
              exit(1);
            }
        }
      else
        {
          index = rs->getInt("index");
        }
      if (verbosity > 1)
        {
          cout << " found index " << index
               << " for trigger " << (iter->second).second << endl;
        }
      ostringstream idstring;
      idstring << prefix << index;
      (iter->second).first = idstring.str();
      delete rs;
      prepupdate->setString(1, idstring.str());
      prepupdate->setString(2, iter->first);
      prepupdate->executeUpdate();
    }
  return 0;
}

int
TriggerAccounting::AddMissingColumns(const std::map<std::string, std::pair<std::string, int> > &map, const std::string &tablename)
{
  set<string> columns_exist;
  ostringstream cmd;
  cmd << "select column_name from information_schema.columns where table_name= \'" << tablename << "\'";
  odbc::Statement* stmt = con->createStatement();
  odbc::ResultSet *rs = 0;
  try
    {
      rs = stmt->executeQuery(cmd.str());
    }
  catch (odbc::SQLException& e)
    {
      cout << "Exception caught: " << e.getMessage() << endl;
    }
  while (rs->next())
    {
      string id = rs->getString("column_name");
      if (verbosity > 2)
        {
          cout << "found column " << id << endl;
        }
      columns_exist.insert(id);
    }
  delete rs;
  std::map<std::string, std::pair<std::string, int> >:: const_iterator iter;
  set<string> columns_to_be_added;
  for (iter = map.begin();  iter != map.end(); iter++)
    {
      string column_name = (iter->second).first;
      if (columns_exist.find(column_name) == columns_exist.end())
        {
          if (verbosity > 1)
            {
              cout << "need to create " << column_name << endl;
            }
          columns_to_be_added.insert(column_name);
        }
    }
  // if there are no columns to be added - just leave
  if (columns_to_be_added.size() == 0)
    {
      return 0;
    }
  odbc::Statement *stmtaddcolumn = con->createStatement();
  BOOST_FOREACH(string column_name, columns_to_be_added)
    {
      if (column_name == "-1")
	{
	  cout << PHWHERE << " trigger index not set" << endl;
	  delete con;
	  exit(1);
	}
      cmd.str("");
      cmd << "alter table " << tablename << " add column "
	  << column_name << " integer default -1";
      int icnt = stmtaddcolumn->executeUpdate(cmd.str());
      if (verbosity > 1)
	{
        cout << "column " << column_name << " added, update cnt: " << icnt << endl;
      }
  }
  return 0;
}
