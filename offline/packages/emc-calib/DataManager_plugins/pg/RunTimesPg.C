#include "RunTimesFactory.h"
#include <cstdlib>

class RunTimesPg : public RunTimes
{
public:
  RunTimesPg() {}
  virtual ~RunTimesPg() {};

protected:
  virtual bool Init();
};

namespace
{
  RunTimes* creator()
  {
    return new RunTimesPg;
  }

  const std::string name = "Pg";
  const bool registered =
  RunTimesFactory::instance().registerCreator(name,
					      creator,
					      "RunTimes");
}

#include <sstream>

#include "odbc++/connection.h"
#include <odbc++/drivermanager.h>
#include <odbc++/resultset.h>

//_____________________________________________________________________________
bool
RunTimesPg::Init()
{
  std::cout << "This method needs fixing - it uses the wrong DB" << std::endl;
  std::cout << "Tell L.Aphecetche to fix it" << std::endl;
  exit(1);

  odbc::Connection* con = odbc::DriverManager::getConnection
    ("Phenix","phnxrc","phnxdb1.phenix.bnl.gov");

  if (!con)
    {
      std::cerr << "RunTimesPg::Init : could not get connection to postgre Phenix database" << std::endl;
      return false;
    }

  odbc::Statement* stmt = con->createStatement();

  std::ostringstream cmd;

  cmd << "select runnumber,brunixtime,erunixtime from run where runnumber>="
      << fMinRunNumber;

  odbc::ResultSet* rs = stmt->executeQuery(cmd.str().c_str());

  while ( ( rs->next() ) )
    {
      PHTimeStamp begin(rs->getInt("brunixtime"));
      PHTimeStamp end(rs->getInt("erunixtime"));
      int runnumber = rs->getInt("runnumber");
      fTimes[runnumber] = new RunLite(runnumber,begin,end);
    }

  delete rs;
  delete con;
  odbc::DriverManager::shutdown();

  return true;
}
