//
// Sasha Lebedev <lebedev@iastate.edu>
// Cesar Luiz da Silva <slash@bnl.gov>
//

#include <summaryQA.h>

#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <time.h>

#include <odbc++/connection.h>
#include <odbc++/drivermanager.h>
#include <odbc++/statement.h>
#include <odbc++/types.h>

#include <TFile.h>

#include <phool.h>

using namespace std;
using namespace odbc;

ofstream TextFile;
ofstream StatusFile;
Connection* con = 0;
Statement* stmt = 0;

QASummary::QASummary()
{
  qafile = NULL;
  inputName = "histo.root";
  outputName1 = "text.txt";
  outputName2 = "status.txt";
  CommitToDatabase = false;
  tagEntry = "test";
  runNumber = 0;
  segmentNumber = -1;
}

int QASummary::Init()
{
  cout << "Reading from " << inputName << endl;
  cout << "Writing text to " << outputName1 << endl;
  cout << "Writing status bits to " << outputName2 << endl;
  qafile = new TFile(inputName, "READ");
  TextFile.open(outputName1);
  TextFile << " ----------------------------------------------------" << endl;
  TextFile << " ************ QA Summary for run  " << runNumber << "  ************" << endl;
  StatusFile.open(outputName2);
  if (CommitToDatabase)
    {
      try
        {
          con = DriverManager::getConnection("calibrations", "phnxrc", "");
        }
      catch (SQLException& e)
        {
          cout << PHWHERE
	       << " Exception caught during DriverManager::getConnection" << endl;
          cout << e.getMessage() << endl;
          CommitToDatabase = 0;
        }
    }
  return 0;
}

int QASummary::End()
{
  if (qafile)
    {
      qafile->Close();
      // delete inputFile;
      qafile = NULL;
    }
  TextFile.close();
  StatusFile.close();
  return 0;
}

int QASummary::CommitToQADatabase(const char* subsystem, const char* parname, float parvalue, float parerror)
{
  if (!CommitToDatabase)
    return -1;

  ostringstream cmd;
  unsigned int numins;

  cmd.str("");
  cmd << "insert into " << subsystem << "qa values(" <<
    runNumber << "," <<
    segmentNumber << "," <<
    "'" << tagEntry << "' ," <<
    time(0) << ", " <<
    parvalue << "," <<
    parerror << ", '" <<
    parname << "')";

  cout << cmd.str() << endl;
  stmt = con->createStatement();
  try
    {
      numins = stmt->executeUpdate(cmd.str().c_str());
    }
  catch (SQLException& e)
    {
      cout << e.getMessage() << endl;
      return 1;
    }
  if (numins != 1)
    {
      cout << "Error executing insert" << endl;
      return -1;
    }
  return 0;
}




