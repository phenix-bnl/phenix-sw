#include <stdio.h>
#include <iostream>
#include <stdlib.h>
#include <sstream>
#include <string>
//#include <odbc++/connection.h>
//#include <odbc++/setup.h>
//#include <odbc++/types.h>
//#include <odbc++/errorhandler.h>
//#include <sql.h>
//#include <odbc++/drivermanager.h>
//#include <odbc++/resultset.h>
//#include <odbc++/resultsetmetadata.h>
//#include <odbc++/preparedstatement.h>
//#include <odbc++/databasemetadata.h>
//#include "phool.h"

//using namespace odbc;
using namespace std;

void
Test()
{
  gSystem->Load("libpdbcalBase");
//  gSystem->Load("libRDBCodbc");
  gSystem->Load("libRDBC");

  gSystem->Load("libodbc++");

  odbc::Connection con;
  ---------------------------------------------------------------------
  con = DriverManager::getConnection("fvtx", "phnxrc", "");

  cmd.str("");
  cmd << "select * from wedges_run12_pp_510";
  stmt = con->createStatement();
  rs = stmt->executeQuery(cmd.str().c_str());

  int nrows = 0; // see if there are any rows, and how many
  int cage, roc, station, wedge;
  while (rs->next())
    {
      nrows++;
      cage = rs->getInt(1); // read the cage number
      roc = rs->getInt(2); //
      station = rs->getInt(3); //
      wedge = rs->getInt(4); //
      printf("%d: %d %d %d %d \n", nrows, cage, roc, station, wedge);
    }

}
