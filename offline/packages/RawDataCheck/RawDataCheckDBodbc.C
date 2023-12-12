#include <RawDataCheckDBodbc.h>
#include <BadEvent.h>
#include <phool.h>

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

#include <algorithm>
#include <cctype>
#include <ctime>
#include <iostream>
#include <sstream>

using namespace odbc;
using namespace std;

RawDataCheckDBodbc::RawDataCheckDBodbc():
  verbosity(0),
  dbname("BadEvents"),
  dbowner("phnxrc"),
  dbpasswd(""),
  tableexists(0)
{}


void
RawDataCheckDBodbc::identify(ostream& out) const
{
  cout << "DB Name: " << dbname << endl;
  cout << "DB Owner: " << dbowner << endl;
  cout << "DB Pwd: " << dbpasswd << endl;
  return ;
}

int
RawDataCheckDBodbc::CheckAndCreateTable(const string &name)
{
  TableName(name);
  return CheckAndCreateTable();
}

int
RawDataCheckDBodbc::CheckAndCreateTable()
{
  if (tableexists)
    {
      return 0;
    }
  Connection *con = NULL;
  try
    {
      con = DriverManager::getConnection(dbname.c_str(), dbowner.c_str(), dbpasswd.c_str());
    }
  catch (SQLException& e)
    {
      cout << PHWHERE
           << " Exception caught during DriverManager::getConnection" << endl;
      cout << "Message: " << e.getMessage() << endl;
      if (con)
        {
          delete con;
        }
      return -1;
    }

  Statement* stmt = con->createStatement();
  ostringstream cmd;
  // pg_tables is an pg internal table which contains amongst other things
  // a list of all available tables in DB
  cmd << "SELECT tablename FROM pg_tables where tablename = '" << table << "' and schemaname='public'";
  ResultSet *rs = NULL;
  try
    {
      rs = stmt->executeQuery(cmd.str());
    }
  catch (SQLException& e)
    {
      cout << PHWHERE << "Exception caught: " << e.getMessage() << endl;
    }
  int iret = 0;
  int createnewtable = 1;
  if (rs->next())
    {
      string tabname = rs->getString("tablename");
      if (tabname != table)
        {
          cout << PHWHERE << "BadEvent DB Tablenames do not match, read " << tabname
               << " looked for " << table << endl;
        }
      else
        {
          createnewtable = 0;
        }
    }
  delete rs;
  if (createnewtable == 1)
    {
      cmd.str("");
      cmd << "CREATE TABLE " << table << "(run int NOT NULL, segment int, event int NOT NULL, trigraw bigint, triglive bigint, trigscaled bigint, crossing int, reason text, cvstag text default 'unknown', ticks int default 0, primary key(run,event,cvstag))";

      if (verbosity > 0)
        {
          cout << "Executing " << cmd.str() << endl;
        }

      try
        {
          iret = stmt->executeUpdate(cmd.str());
        }
      catch (SQLException& e)
        {
          cout << PHWHERE << "Exception caught: " << e.getMessage() << endl;
        }
    }
  delete con;
  tableexists = 1;
  return iret;
}

int
RawDataCheckDBodbc::AddBadEvent(BadEvent *newbad)
{
  CheckAndCreateTable();
  Connection *con = NULL;
  try
    {
      con = DriverManager::getConnection(dbname.c_str(), dbowner.c_str(), dbpasswd.c_str());
    }
  catch (SQLException& e)
    {
      cout << PHWHERE
           << " Exception caught during DriverManager::getConnection" << endl;
      cout << "Message: " << e.getMessage() << endl;
      if (con)
        {
          delete con;
        }
      return -1;
    }

  ostringstream cmd, cmd1;

  Statement* stmt = con->createStatement();
  // first check if this event is already marked corrupt (e.g. if running twice)
  cmd << "SELECT COUNT(*) FROM " << table
      << " WHERE  run = " << newbad->Run()
      << "and event = " << newbad->Event()
      << "and cvstag = '" << newbad->CvsTag() << "'";
  ResultSet *rs = stmt->executeQuery(cmd.str());
  int haveentry = 0;
  while (rs->next())
    {
      haveentry = rs->getInt(1);
    }
  delete rs;

  // if there is no entry for this event create one
  if (!haveentry)
    {
      cmd.str("");
      cmd << "INSERT INTO " << table << "(run, segment, event, trigraw, triglive, trigscaled, crossing, reason, cvstag, ticks)";
      cmd1 << "VALUES(" << newbad->Run()
	   << "," << newbad->SegmentId()
	   << "," << newbad->Event()
	   << "," << newbad->TrigRaw()
	   << "," << newbad->TrigLive()
	   << "," << newbad->TrigScaled()
	   << "," << newbad->Crossing()
	   << ",'" << newbad->Reason()
	   << ",'" << newbad->CvsTag()
	   << ",'" << newbad->TicksToLastEvent()
	   << "')";
      cmd << cmd1.str();
      stmt->executeUpdate(cmd.str());
    }
  delete con;

  return 0;
}

int
RawDataCheckDBodbc::AddBadEvents(list<BadEvent> *badlist)
{
  // table name not set (if someone Adds bad events from the outside)
  if (table.size() == 0)
    {
      table = badlist->begin()->CvsTag();
    }
  CheckAndCreateTable();
  Connection *con = NULL;
  try
    {
      con = DriverManager::getConnection(dbname.c_str(), dbowner.c_str(), dbpasswd.c_str());
    }
  catch (SQLException& e)
    {
      cout << PHWHERE
           << " Exception caught during DriverManager::getConnection" << endl;
      cout << "Message: " << e.getMessage() << endl;
      if (con)
        {
          delete con;
        }
      return -1;
    }

  ostringstream cmd;

  cmd << "INSERT INTO " << table
      << " (run, segment, event, trigraw, triglive, trigscaled, crossing, reason, cvstag, ticks)"
      << " VALUES( ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)";
  PreparedStatement *stmt = 0;
  try
    {
      stmt = con->prepareStatement(cmd.str());
    }
  catch (SQLException& e)
    {
      cout << PHWHERE
           << " Exception caught during DriverManager::prepareStatement(string &)" << endl;
      cout << "Message: " << e.getMessage() << endl;
    }
  cmd.str("");
  cmd << "select count(*) from " << table
      << " where run = ? and event = ? and cvstag = ?";
  PreparedStatement *stmt2 = 0;
  try
    {
      stmt2 = con->prepareStatement(cmd.str());
    }
  catch (SQLException& e)
    {
      cout << PHWHERE
           << " Exception caught during DriverManager::prepareStatement(string &)" << endl;
      cout << "Message: " << e.getMessage() << endl;
    }
  // first check if this event is already marked corrupt (e.g. if running twice)
  list<BadEvent>::const_iterator biter;
  for (biter = badlist->begin(); biter != badlist->end(); biter++)
    {
      stmt2->setInt(1, biter->Run());
      stmt2->setInt(2, biter->Event());
      stmt2->setString(3, biter->CvsTag());
      ResultSet *rs2 = stmt2->executeQuery();
      int haveentry = 0;
      while (rs2->next())
        {
          haveentry = rs2->getInt(1);
        }
      delete rs2;
      if (haveentry == 1)
        {
          continue;
        }
      stmt->setInt(1, biter->Run());
      stmt->setInt(2, biter->SegmentId());
      stmt->setInt(3, biter->Event());
      stmt->setLong(4, biter->TrigRaw());
      stmt->setLong(5, biter->TrigLive());
      stmt->setLong(6, biter->TrigScaled());
      stmt->setInt(7, biter->Crossing());
      stmt->setString(8, biter->Reason());
      stmt->setString(9, biter->CvsTag());
      stmt->setInt(10, biter->TicksToLastEvent());
      if (verbosity > 0)
        {
          biter->identify();
        }
      try
        {
          stmt->executeUpdate();
        }
      catch (SQLException& e)
        {
          string errmsg = e.getMessage();
          if (errmsg.find("Cannot insert a duplicate key into unique index") != string::npos)
            {
              if (verbosity > 0)
                {
                  cout << "BadEvent already in DB" << endl;
                }
            }
          else
            {
              cout << PHWHERE
                   << " Exception caught during preparedStatement::executUpdate()" << endl;
              cout << errmsg << endl;
            }
        }
    }
  delete con;

  return 0;
}

void
RawDataCheckDBodbc::TableName(const std::string &name)
{
  table = name;
  // transform table name to all lower case
  transform(table.begin(), table.end(), table.begin(), (int(*)(int))tolower);
  return;
}
