#include "QaDatabaseManager.h"
#include "QaEntry.h"

#include <sstream>
#include <iostream>
#include <string>
#include <vector>
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
#include "phool.h"

#include "PHTimeStamp.h"

using namespace odbc;
using namespace std;

QaDatabaseManager::QaDatabaseManager()
{

}

void QaDatabaseManager::WriteToDatabase(std::string DatabaseName, std::string UserName, std::string TableName, int RunNumber, int SegmentNumber, std::string Tag, std::vector<QaEntry> Cesar)
{
  // Declarations
  Connection* con;
  Statement* stmt;
  ostringstream cmd;
  PHTimeStamp time;
  int insertTime = 0;
  unsigned int numins = 0;

  //Connect to database calibrations
  try
    {
      con = DriverManager::getConnection(DatabaseName,UserName, "");
    }
  catch (SQLException& e)
    {
      cout << PHWHERE
	   << " Exception caught during DriverManager::getConnection" << endl;
      cout << e.getMessage() << endl;
      return;
    }

  //Fill the table with some values
  vector<QaEntry>::const_iterator i;
  for(i = Cesar.begin(); i < Cesar.end(); i++)
    {
      cmd.str("");
      time.setToSystemTime();
      insertTime = time.getTics();
      cmd <<  "insert into " << TableName << " values(" <<  RunNumber <<  "," << SegmentNumber << ",'" << Tag << "'," << insertTime << "," << i->value << "," << i->error << ",'" << i->name << "')";
      cout << cmd.str() << endl;
      stmt = con->createStatement();
      try{
	numins = stmt->executeUpdate(cmd.str().c_str());
      }
      catch (SQLException& e)
	{
	  cout << e.getMessage() << endl; 
	  return;
	}
      if(numins != 1)
	{
	  cout << "Error executing insert" << endl;
	}
    }
  
  delete con;

}
