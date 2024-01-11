#include "emcDB.h"
#include "PHPointerList.h"
#include "PHString.h"
#include "TROOT.h"
#include "emcManageable.h"
#include "emcDataManager.h"
#include "emcRawDataAccessor.h"
#include "PHTimeStamp.h"
#include "emcGains.h"

#include "convertNormalization.h"

#include <cstdlib>
#include <cassert>
#include <vector>
#include <string>

using namespace std;

//_____________________________________________________________________________
int usage(void) 
{
  cout << "emcDB " << endl;
  cout << "      --dump [db full name] " << endl
       << "        Dump DB on screen. " << endl;
  cout << "      --dbms [Objy|Pg(default)]" << endl
       << "      Database Management System used (Objy or Postgres)"
       << endl;
  cout << "      --bankID [int] (use with --dump only) Specify a bankID to look for " << endl;
  cout << "      --insertedAfter (with --dump only) \"year,month,....\" " << endl 
       << "      --insertedBefore (with --dump only) \"year,month....\" " << endl 
       << "        Used to select date interval of bank insertion date." << endl
       << endl
       << "     --startAfter (with --dump only)" << endl
       << "     --startBefore (with --dump only)" << endl
       << "     --endAfter (with --dump only)" << endl
       << "     --endBefore (with --dump only)" << endl
       << "       Like --insertedAfter/Before, but select on Start and End"
       << "       Validity dates." << endl;
  cout << endl; 
  cout << "      --update [directory name] " << endl
       << "        Read all calibration data files in the directory " << endl
       << "        and write them into DB." << endl;
  cout << "      --updatePbGlGains [top] " << endl
       << "        Special case of above for PbGlGains, " << endl 
       << "        where top is the top directory name," << endl
       << "        where to find RunXXXX sub-directories, containing " << endl
       << "        normal calibration Gains directory" << endl;

  cout << "      --forceDate \"year,month,day,hour,minute,second\" " << endl
       << "        used with --update : specify the start-of-validity " << endl
       << "        of all the data to be read from file and written " << endl
       << "        into DB." << endl;

  cout << "      --forceEndDate \"year,month,day,hour,minute,second\" " << endl
       << "        used with --update : specify the end-of-validity " << endl
       << "        of all the data to be read from file and written " << endl
       << "        into DB. If not specified, end=infinity" << endl;

  cout << "      --debug" << endl;
  cout << "      --batch (non interactive version)" << endl;
  cout << endl;
  cout << "      --runtimes [minrunnumber] Make a file with relations Run Number<->TimeStamps, starting at minrunnumber" << endl;
  cout << endl;
  cout << "      --compare [directory] " << endl
       << "        (with --dump AND --config) " << endl 
       << "        Allow to compare banks from database with banks from " << endl
       << "       file(s) to be found under directory." << endl
       << "       Several --compare options can be given to compare " << endl
       << "       one db bank against several ASCII versions" << endl;
  cout << "      --config [gdb.dat] " << endl
       << "      Give the name of a gdb.dat configuration file " << endl
       << "      To be used to provide FEM Absposition to PinNumber " << endl
       << "      relashionship (only needed for --dump with --compare " << endl
       << "      for pedestals)" << endl;
  cout << "      --convertNormalization data_source " << endl
       << "      Convert inical into a format useable by LVL2" << endl;
  cout << "      --dbname [name]" << endl
       << "      for expert mode : use a different database, e.g. oncal"
       << endl;
  cout << "      --help "<< endl
       << "        This message." << endl;
  cout << "      --version " << endl;
  cout << "        Version number of this program." << endl;
  cout << endl;
  cout << "Examples: " << endl;
  cout << "  emcDB --dump calib.emc.Gains" << endl;
  cout << "  emcDB --update /afs/rhic/phenix/phnxemc/EMC/m_080701/ToF" << endl
       << "        --forceDate \"2001,08,01,12,45,00\" " << endl;
  cout << endl << endl;
  return -1;
}

//_____________________________________________________________________________
PHTimeStamp* getDate(const string& arg)
{
  // Convert a string "year,month,day,hour,min,sec" into a timestamp.
  // Brute force method. No checks.

  PHString str(arg.c_str());
  PHPointerList<PHString> tok;
  
  str.split(tok,",");
  
  int y[6];
  size_t j;
  
  assert( 6 <= tok.length() );

  for (j=0;j<tok.length() && j<6;j++) {
    y[j] = atoi(tok[j]->getString());
  }
  
  tok.clearAndDestroy();

  return new PHTimeStamp(y[0],y[1],y[2],y[3],y[4],y[5]);
}
 
//_____________________________________________________________________________
int test(string config)
{
  emcManageable::EStorage data_source = emcManageable::kDB_Objy;

  int status;

  emcRawDataAccessor::GetInstance(status,config.c_str());

  if (!status) {

    emcDataManager* dm = emcDataManager::GetInstance();

    emcGains gains;

    gains.SetSource(data_source);

    emcGains* gaino = 0;

    PHTimeStamp when;
    when.setToSystemTime();

    gaino = (emcGains*)(dm->Collect(gains,when));

    if (gaino) {
      cout << gaino->GetSize() << " FEMs read" << endl;
    }
    else {
      cerr << "Failed to fetch gain at ts=" << when << endl;
    }
  }
  else {
    cerr << "<E> Cannot create RDA" << endl;
    return -1;
  }
  return 0;
}


//_____________________________________________________________________________
int main(int argc, char** argv)
{

  //  TROOT root("emcdb","Small EMCAL DB utility program");

  try
  {  
    string dbms = "Pg";
    string dbname = "calibrations";
    bool kConfig = false; // --config option given ?
    string what_to_write = "none";
    bool kForceDate = false; // --forceDate option given ?
    bool kDump = false; // --dump option given ?
    string what_to_dump;
    // bool kSimulDBs = false; // --simulDBs
    // bool kFake = false; // --fake
    bool kUpdate = false; // --update
    string UpdateDir;
    bool kUpdatePbGlGains = false; // --updatePbGlGains
    string UpdatePbGlGainsDir;
    string what_to_convert; 
    bool kBatch = false; // --batch
    bool kBankID = false; // --bankID
    bool kDebug = false; // --debug
    bool kRunTimes = false; // --runtimes
    int MinRunNumber = 0;
    bool kVersion = false; // --version
    vector<string> directories; // --compare 
    bool kConvertNormalization = false; // --convertNormalization
    int ConvertNormalizationSource=0;
    bool kSourceDir=false;// --sourceDir
    string SourceDir;

    int BankID = -1;

    size_t i;
    vector<string> args;

    PHTimeStamp* TforceDate = 0;
    PHTimeStamp* TforceEndDate = 0;
    PHTimeStamp* TinsertAfter = 0;
    PHTimeStamp* TinsertBefore = 0;
    PHTimeStamp* TstartAfter = 0;
    PHTimeStamp* TstartBefore = 0;
    PHTimeStamp* TendAfter = 0;
    PHTimeStamp* TendBefore = 0;

    for ( i = 1; i < static_cast<size_t>(argc); i++ ) {
      args.push_back(argv[i]);
    }

    string config = "FEM.conf";

    i = 0; 
    bool error = false;

    if (args.empty()) { return usage(); }

    while ( !error && i < args.size() ) {

      string opt = args[i];
      string arg = ".";

      if ( i+1 < args.size() ) { arg = args[i+1]; }

      if ( opt=="--dbname") {
	if (arg!=".") {
	  dbname=arg;
	  i+=2;
	}
      }
      else if (opt=="--write") {
	if (arg!=".") {
	  what_to_write=arg;
	  i+=2;
	}
      }
      else if (opt=="--bankID") {
	if (arg!=".") {
	  kBankID = true;
	  BankID = atoi(arg.c_str());
	  i+=2;
	}
      }
      else if ( opt=="--dbms") {
	if (arg!=".") {
	  dbms=arg;
	  i+=2;
	}
      }
      else if (opt=="--forceDate") {
	if (arg!=".") {
	  kForceDate=true;
	  TforceDate = getDate(arg);
	  i+=2; 
	}
      }      
      else if (opt=="--forceEndDate") {
	if (arg!=".") {
	  TforceEndDate = getDate(arg);
	  i+=2; 
	}
      }
      else if (opt=="--compare") {
	if (arg!=".") {
	  directories.push_back(arg);
	  i+=2;
	}
      }
      else if (opt=="--insertedAfter") {
	if (arg!=".") {
	  TinsertAfter = getDate(arg);
	  i+=2;
	}
      }
      else if (opt=="--insertedBefore") {
	if (arg!=".") {
	  TinsertBefore = getDate(arg);
	  i+=2;
	}
      }
      else if (opt=="--startBefore") {
	if (arg!=".") {
	  TstartBefore = getDate(arg);
	  i+=2;
	}
      }
      else if (opt=="--startAfter") {
	if (arg!=".") {
	  TstartAfter = getDate(arg);
	  i+=2;
	}
      }
       else if (opt=="--endBefore") {
	if (arg!=".") {
	  TendBefore = getDate(arg);
	  i+=2;
	}
      }
      else if (opt=="--endAfter") {
	if (arg!=".") {
	  TendAfter = getDate(arg);
	  i+=2;
	}
      }
      else if (opt=="--dump") {
	if (arg!=".") {
	  kDump=true;
	  what_to_dump=arg;
	  i+=2;
	}
      }
      else if (opt=="--config") {
	if (arg!=".") {
	  kConfig=true;
	  config=arg;
	  i+=2;
	}
      }
      else if (opt=="--convertNormalization")
	{
	  if (arg!=".") 
	    {
	      kConvertNormalization=true;
	      ConvertNormalizationSource=atoi(arg.c_str());
	      i+=2;
	    }
	}
      else if ( opt=="--sourceDir")
	{
	  if (arg!=".") 
	    {
	      kSourceDir=true;
	      SourceDir=arg;
	      i+=2;
	    }
	}

      else if (opt=="--help" || opt=="-h" ) {
	return usage();
      }
      else if (opt=="--version") {
	kVersion = true;
	i++;
      }
      else if (opt=="--debug") {
	kDebug = true;
	i++;
      }
      else if (opt=="--batch") {
	kBatch=true;
	i++;
      }
      else if (opt=="--convert") {
	what_to_convert = arg;
	i += 2;
      }
      else if (opt=="--update") {
	kUpdate = true;
	UpdateDir = arg; 
	i += 2; 
      }
      else if (opt=="--updatePbGlGains") {
	kUpdatePbGlGains = true;
	UpdatePbGlGainsDir = arg;
	i += 2;
      }
      else if (opt=="--simulDBs") {
	// kSimulDBs=true;
	i++;
      }
      else if (opt=="--fake") {
	// kFake = true;
	i++;
      }
      else if (opt=="--runtimes") {
	kRunTimes = true;
	++i;
	if (arg.substr(0,2)!="--") 
	  {
	    MinRunNumber=atoi(arg.c_str());
	    ++i;
	  }
      }
      else {
	cerr << "<E> Unknown option : " << opt << endl;
	//error = true; 
	i++;
      }
    } // end loop on arguments

    // Some checks here.
    if ( kBankID ) {
      if ( !kDump ) {
	cerr << "<E> Option --bankID can only be used with --dump" << endl;
	return usage();
      }
      if ( BankID < 0 ) {
	cerr << "<E> BankID = " << BankID << " is not valid" << endl;
	return usage();
      }
    }

    if (TforceDate) {
      cout << "<I> I will use " << (*TforceDate) 
	   << " as starting validity period for everything I will write " 
	   << endl;
    } 
    if (TforceEndDate) {
      cout << "<I> I will use " << (*TforceEndDate) 
	   << " as end of validity period for everything I will write " 
	   << endl;
    } 

    //
    // --------------------------------------------------------------------
    //

    if (kVersion) 
      {
	cout << emcDB::Version() << endl;
      }

    emcDB edb(dbms.c_str(),dbname.c_str(),!kBatch,TforceDate,TforceEndDate);
    
    if (kConvertNormalization)
      {
	if ( (ConvertNormalizationSource==emcManageable::kDB_Objy ||
	      ConvertNormalizationSource==emcManageable::kDB_Pg) && 
	    !TforceDate) 
	  {
	    cerr << "<E> --convertNormalization from DB requires the "
		 << "--forceDate option !" << endl;
	    return -2;
	  }
	else if (ConvertNormalizationSource==emcManageable::kFile_ASCII &&
		 !kSourceDir) {
	   cerr << "<E> --convertNormalization from ASCII files requires the "
		 << "--sourceDir option !" << endl;
	    return -3;
	}
	emcManageable::EStorage source;
	emcDataManager* dm = emcDataManager::GetInstance();

	switch (ConvertNormalizationSource)
	  {
	  case emcManageable::kDB_Objy:
	    source=emcManageable::kDB_Objy;
	    break;
	  case emcManageable::kDB_Pg:
	    source=emcManageable::kDB_Pg;
	    break;
	  case emcManageable::kFile_ASCII:
	    source=emcManageable::kFile_ASCII;
	    dm->SetSourceDir(SourceDir.c_str());
	    break;
	  case emcManageable::kDB_Construction:
	    source=emcManageable::kDB_Construction;
	    break;
	  default:
	    cerr << "<E> Invalid data source : " << ConvertNormalizationSource
		 << " Possible values are : "
		 << "\n" << emcManageable::kDB_Pg << "=" 
		 << emcManageable::GetStorageName(emcManageable::kDB_Pg)
		 << "\n" << emcManageable::kDB_Objy << "=" 
		 << emcManageable::GetStorageName(emcManageable::kDB_Objy)
		 << "\n" << emcManageable::kFile_ASCII << "=" 
		 << emcManageable::GetStorageName(emcManageable::kFile_ASCII)
		 << "\n" << emcManageable::kDB_Construction << "=" 
		 << emcManageable::GetStorageName(emcManageable::kDB_Construction)
		 << endl;
	    return -20;		 		 
	  }

	if (kForceDate) 
	  {
	    convertNormalization(source,TforceDate);
	  }
	else
	  {
	    convertNormalization(source);
	  }
      }

    if (kConfig) {
      edb.SetConfigurationFile(config);
    }

    edb.SetDirectories(directories);

    if (kDebug) edb.Debug(true);

    if (!TinsertAfter) {
      TinsertAfter = new PHTimeStamp(0);
    }

    if (!TinsertBefore) {
      TinsertBefore = new PHTimeStamp();
      TinsertBefore->setToFarFuture();
    }

    if (!TstartAfter) {
      TstartAfter = new PHTimeStamp(0);
    }

    if (!TstartBefore) {
      TstartBefore = new PHTimeStamp();
      TstartBefore->setToFarFuture();
    }

    if (!TendAfter) {
      TendAfter = new PHTimeStamp(0);
    }

    if (!TendBefore) {
      TendBefore = new PHTimeStamp();
      TendBefore->setToFarFuture();
    }

    edb.SetInsertInterval(*TinsertAfter,*TinsertBefore);
    edb.SetStartInterval(*TstartAfter,*TstartBefore);
    edb.SetEndInterval(*TendAfter,*TendBefore);

    edb.SetBankID(BankID);

    if (kRunTimes) 
      {
	edb.MakeRunTimes(MinRunNumber);
	exit(1);
      }

    if (kDump) {
      bool ok = edb.Dump(what_to_dump.c_str());
      if (!ok) {
	cerr << "<E> --dump failed" << endl;
      }
    }

    if (kUpdate) {
      bool ok = edb.Update(UpdateDir);
      if (!ok) {
	cerr << "<E> --update failed" << endl;
      }
    }

    if (kUpdatePbGlGains) {
      edb.UpdatePbGlGains(UpdatePbGlGainsDir);
    }

    delete TinsertAfter;
    delete TinsertBefore;
    delete TstartAfter;
    delete TstartBefore;
    delete TendAfter;
    delete TendBefore;
    delete TforceDate;
    delete TforceEndDate;

  } // end of try...

  catch (const exception &e)
  {
    cerr << "Got an exception : " << e.what() << endl;
  }

  return 0;
}

