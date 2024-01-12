#include <OutputAccounting.h>
#include <TrigSelect.h>
#include <Fun4AllServer.h>
#include <Fun4AllOutputManager.h>

#include <recoConsts.h>

#include <PHCompositeNode.h>

#include <odbc++/connection.h>
#include <odbc++/errorhandler.h>
#include <odbc++/drivermanager.h>
#include <odbc++/resultset.h>
#include <odbc++/resultsetmetadata.h>

#include <boost/filesystem/path.hpp>

#include <algorithm>
#include <iostream>
#include <sstream>

using namespace std;

static odbc::Connection *con = 0;

OutputAccounting::OutputAccounting(const string &name): SubsysReco(name)
{
  return ;
}

OutputAccounting::~OutputAccounting()
{
  delete con;
  return ;
}

int
OutputAccounting::End(PHCompositeNode *topNode)
{
  Fun4AllServer *se = Fun4AllServer::instance();
  recoConsts *rc = recoConsts::instance();
  if (! rc->FlagExist("CVSTAG"))
    {
      return 0;
    }
  string tablename = "outfiles_";
  tablename += rc->get_CharFlag("CVSTAG");
  // The bizarre cast here to convert the tablename to lower chars is needed for newer gccs
  transform(tablename.begin(), tablename.end(), tablename.begin(), (int(*)(int))tolower);

  if (CheckAndCreateTable(tablename))
    {
      return -1;
    }
  vector<string> outmanagernames;
  se->GetOutputManagerList(outmanagernames);
  vector<string>::iterator fiter;
  ostringstream filelist;
  //  int ifirst = 1;
  int segment = se->SegmentNumber();
  int runnumber = rc->get_IntFlag("RUNNUMBER");
  for (fiter = outmanagernames.begin(); fiter != outmanagernames.end(); fiter++)
    {
      Fun4AllOutputManager *out = se->getOutputManager(*fiter);
      if (out->OutFileName() == "")
	{
	  continue;
	}
      boost::filesystem::path full_file(out->OutFileName());
      string fname = out->OutFileName();
      if (verbosity > 0)
        {
          cout << "output filename: " << full_file.filename() << endl;
          cout << ", Events: " << out->EventsWritten() << endl;
        }
      if (InsertNewRow(tablename, runnumber, segment, full_file.filename().string(), out->EventsWritten()))
        {
          return -1;
        }
    }
  vector<string> modulelist;
  se->GetModuleList(modulelist);
  TrigSelect *tr;
  for (fiter = modulelist.begin(); fiter != modulelist.end(); fiter++)
    {
      SubsysReco *sr = se->getSubsysReco((*fiter).c_str());
      tr = dynamic_cast<TrigSelect *> (sr);
      if (tr)
	{
	  if (verbosity > 0)
	    {
	      cout << *fiter << ": num fired = " << tr->GetNTrigSelected() << endl;
	    }
	  if (InsertNewRow(tablename, runnumber , segment, *fiter, tr->GetNTrigSelected()))
	    {
	      return -1;
	    }
	}
    }
  return 0;
}

int
OutputAccounting::GetConnection()
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

int
OutputAccounting::CheckAndCreateTable(const std::string &tablename)
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
      cmd << "CREATE TABLE " << tablename << "(filename text NOT NULL, runnumber int default -1, segment int default -1, events int default -1, inserttime bigint NOT NULL";
      cmd << ", primary key(filename,runnumber,segment))";
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
    }
  delete stmt;

  return 0;
}

int
OutputAccounting::InsertNewRow(const std::string &tablename, const int runnumber, const int segment, const std::string &filename, const int nevents)
{
  if (GetConnection())
    {
      return -1;
    }
  ostringstream cmd;
  cmd << "select * from " << tablename << " where filename = '" << filename
      << "' and runnumber = " << runnumber << " and segment = " << segment;

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
  if (rs->next())
    {
      cmd.str("");
      cmd << "delete from " << tablename << " where filename = '" << filename
	  << "' and runnumber = " << runnumber << " and segment = " << segment;

      odbc::Statement* stmt2 = con->createStatement();
      try
        {
          stmt2->executeUpdate(cmd.str());
        }
      catch (odbc::SQLException& e)
        {
          cout << "1 exception caught: " << e.getMessage() << endl;
        }
      delete stmt2;
    }
  delete rs;
  cmd.str("");
  cmd << "insert into " << tablename << " (filename, runnumber, segment, events, inserttime) values ('" << filename
      << "', " << runnumber << ", " << segment << ", " << nevents << ", " << time(0) << ")";
  try
    {
      stmt->executeUpdate(cmd.str());
    }
  catch (odbc::SQLException& e)
    {
      cout << "2 exception caught: " << e.getMessage() << endl;
    }
  delete stmt;
  return 0;
}

