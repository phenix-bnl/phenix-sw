#include "PgPostApplication.hh"
#include "PgPostBankWrapperManager.hh"
#include "PgPostBankWrapper.hh"
#include "PgPostBankWrapper2.hh"

#include <PdbApplicationFactory.hh>

#include <RDBC/TSQL.h>
#include <RDBC/TSQLDriverManager.h>

#include <sstream>

using namespace std;

namespace
{
  PdbApplication* singletonCreator()
  {

    // rememeber that this will not neccessarily return a
    // pointer to the singleton PgPostApplication. If 
    // an Objy application is instantiated, it will return 0.

    cout << "*** deprecated interface -- use PdbApplication::instance()" << endl;
    return PgPostApplication::instance();
  }

  const std::string name = "Pg";
  const bool registered =
    PdbApplicationFactory::instance().registerCreator(name, singletonCreator, "PdbApplication");
}

PgPostApplication *PgPostApplication::instance() 
{
  return mySpecificCopy;
}


int PgPostApplication::Register(const string &dbname)
{
  if ( __instance.get() ) return -1;
  mySpecificCopy  = new PgPostApplication (dbname);
  __instance = std::auto_ptr<PdbApplication>(mySpecificCopy);
  return 0;
}

int PgPostApplication::releaseConnection()
{
  if ( !__instance.get() ) return -1;
  if(con){
    con->Close();
    con = 0;
  }
  return 0;
}

PgPostApplication * PgPostApplication::mySpecificCopy = 0;
TSQLConnection * PgPostApplication::con = 0;

PgPostApplication::PgPostApplication(const string &dbname)
{
  dsn = dbname;
  readOnly = 1;
}

PgPostApplication::~PgPostApplication()
{
  mySpecificCopy = 0;
  if(con){
    con->Close();
    con = 0;
  }
}


TSQLConnection * PgPostApplication::getConnection()
{
  if (!__instance.get())
    {
      PgPostApplication::Register();
    }
  if (!con)
    {
      ostringstream constr;
      constr << "dsn=" << dsn << "; uid=phnxrc; pwd= ";
      con = gSQLDriverManager->GetConnection(constr.str().c_str());
    }
  return con;
}

int
PgPostApplication::setDBName(const char *name)
{
  string newname = name;
  if (dsn != newname)
    {
      dsn = newname;
      if (con)
        {
          con->Close();
        }
      ostringstream constr;
      constr << "dsn=" << dsn << "; uid=phnxrc; pwd= ";
      con = gSQLDriverManager->GetConnection(constr.str().c_str());
      if (!con)
        {
          cout << PHWHERE << " Could not open data base " << dsn << endl;
        }
    }
  return 0;
}

PdbStatus PgPostApplication::startUpdate()
{
  readOnly = 0;
  return 1;
}


PdbStatus
PgPostApplication::startRead()
{
  readOnly = 1;
  return 1;
}

PdbStatus
PgPostApplication::abort()
{
  PgPostBankWrapperManager::instance().clear();
  // will roll back a Xact in progres
  if (con)    con->Rollback();
  return 1;
}

PdbStatus
PgPostApplication::commit()
{
  if (readOnly)
    {
      // Forget about the registered wrappers.
      PgPostBankWrapperManager::instance().clear();
      return 1;
    }
  else
    {
      // Commit the registered wrappers (this will also forget them).
      bool ok = PgPostBankWrapperManager::instance().commit();
     if (con) con->Commit();
     return (ok==true);
    }
}

PdbStatus
PgPostApplication::commit(PdbCalBank *b){
  PgPostBankWrapper *tb = dynamic_cast<PgPostBankWrapper*>(b);
  if (tb) {
    return tb->commit(); 
  }
  else {

    PgPostBankWrapper2 *tb2 = dynamic_cast<PgPostBankWrapper2*>(b);
    if (tb2) {
      return tb2->commit(); 
    }
    else {
      cerr << PHWHERE << "Can only commit PgPostBankWrapper2" << endl;
      return 0;
    }
  }
}

PdbStatus
PgPostApplication::commit(PdbCalBank *b, int rid, long it,long st,long et){
  PgPostBankWrapper *tb = dynamic_cast<PgPostBankWrapper*>(b);
  if (tb) {
    return tb->commit(); 
  }
  else {

    PgPostBankWrapper2 *tb2 = dynamic_cast<PgPostBankWrapper2*>(b);
    if (tb2) {
      return tb2->commit_rid(rid,it,st,et); 
    }
    else {
      cerr << PHWHERE << "Can only commit PgPostBankWrapper2" << endl;
      return 0;
    }
  }
}

int
PgPostApplication::DisconnectDB()
{
  if (con)
    {
      con->Close();
    }
  con = 0;
  return 0;
}
