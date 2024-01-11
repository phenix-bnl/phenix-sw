#include <iostream>
#include <cassert>
#include <cstdlib>
#include <unistd.h>
#include <sys/types.h>
#include <dirent.h>
#include <libgen.h>
#include <TString.h>
#include <TROOT.h>

#include <MpcMap.h>
#include <MpcCalib.h>
#include <PHTimeStamp.h>
#include <RunToTime.hh>

// to instantiate the PostGres factories
#include <PgPostApplication.hh>
#include <PgPostBankManager.hh>
#include <RunToTimePg.hh>

using namespace std;

//TROOT groot("tdcoverflow","overflow calibration");

void usage()
{
  cout << "to download from the database:" << endl;
  cout << "usage: mpcdbase <calibtype>" << endl;
  cout << "  where <calibtype> can be " << endl;
  cout << "         all gain ped gaincorr leakage pedestal lopost lopre hipost hipre " << endl;
  cout << "         hiloratio hilolimit leastcount overflow map led pshape np1shape" << endl;
  cout << endl;
  cout << "to save to the database:" << endl;
  cout << "usage: mpcdbase -w <calibfile>" << endl;
  cout << "  where <calibfile> is the calibration file in the form of" << endl;
  cout << endl;
  cout << endl;
  cout << "Options are" << endl;
  cout << "  -h         #print this help message." << endl;
  cout << "  -r <run>   #read/write to specific run. Start and End times are auto computed." << endl;
  cout << "  -w         #write mode (store calibs into database)." << endl;
  cout << "  -u <user>  #name of user committing into database." << endl;
  cout << "  -m <user>  #message to be stored in database (should be description of calibrations)." << endl;
  cout << "  -y         #answer yes to all questions (non-interactive mode)." << endl;
}

void checkrange(bool rangetest, const char *message)
{
  if ( !rangetest )
    {
      cout << message << endl;
      exit(-3);
    }
}

int main(int argc, char **argv)
{

  bool storeflag = false;	// true = store in database, false = read from database

  // which calibration type to process
  bool allflag = false;
  bool pedflag = false;
  bool gainflag = false;
  bool gaincorrflag = false;
  bool lopostpedflag = false;
  bool loprepedflag = false;
  bool hipostpedflag = false;
  bool hiprepedflag = false;
  bool hiloratioflag = false;
  bool hilolimitflag = false;
  bool leastcountflag = false;
  bool overflowflag = false;
  bool mapflag = false;
  bool ledflag = false;
  bool pshapeflag = false;
  bool np1shapeflag = false;
  bool leakageflag = false;
  bool interactiveflag = true;
  int  runnumber = 0;		// if runnumber is specified, we use that to get time limits
  string name;			// user who submitted led calib
  string description;		// message to store in database

  // process command line options
  int c;
  extern int optind;
  extern char *optarg;
  while ((c = getopt(argc, argv, "ywhr:u:m:")) != -1)
    {
      switch(c)
	{
	case 'w':		// write to database
	  storeflag = true;
	  break;
	case 'y':		// non-interactive mode (answer "yes" automatically)
	  interactiveflag = false;
	  break;
	case 'r':		// use runnumber to get start/stop times
	  runnumber = atoi(optarg);
	  break;
	case 'u':		// username
	  name = optarg;
	  break;
	case 'm':		// message to store in database
	  description = optarg;
	  break;
	case 'h':		// help
	  usage();
	  exit(0);
	  break;
	default:
	  usage();
	  exit(-1);
	  break;
	}
    }

  // process command line arguments
  if ( optind==argc )
    {
      // an argument must be specified
      usage();
      exit(-1);
    }

  /*
  cout << "run " << runnumber << endl
       << "user " << name << endl
       << "msg  " << description << endl;
  */

  PgPostApplication::Register();
  PgPostBankManager::Register();
  RunToTimePg::Register();

  // set up the MPC calibration objects
  MpcMap *mpcmap = MpcMap::instance();
  MpcCalib *mpccalib = MpcCalib::instance();
  mpccalib->Reset();

  if ( storeflag==true )
    {
      // parse filenames to find out which calibration type to store
      while ( optind < argc )
        {
	  TString fname;

	  // if fname is a directory, we process all files in the directory
	  if ( optind==(argc-1) && opendir(argv[optind])!=NULL )
	    {
	      fname = argv[optind];
	      //fname += "/MpcCal";
	      int status = mpccalib->Download_All( fname.Data() );
              if ( status != 0 )
	        {
		  cout << "unable to open one or more of the expected calibration files," << endl;
		  cout << "please check " << argv[optind] << endl;
		  exit(3);
		}
	      status = mpcmap->Download_Maps( fname.Data() );
              if ( status != 0 )
	        {
		  cout << "unable to open one or more of the expected calibration files," << endl;
		  cout << "please check " << argv[optind] << endl;
		  exit(3);
		}

	      allflag = true;

	      break;
	    }

	  fname = basename( argv[optind] );
	  if ( fname.EndsWith(".gains") ) 
	    {
	      mpccalib->Download_Gains(argv[optind]);
	      if (gainflag==true)
		{
		  cout << "duplicate adc calib files, aborting..." << endl;
		  exit(1);
		}
	      gainflag = true;
	    }
	  else if ( fname.EndsWith(".ped") )
	    {
	      mpccalib->Download_Pedestals(argv[optind],"ped");
	      if (pedflag==true)
		{
		  cout << "duplicate pedestal calib files, aborting..." << endl;
		  exit(1);
		}
	      pedflag = true;
	    }
	  else if ( fname.EndsWith(".gaincorr") ) 
	    {
	      mpccalib->Download_GainCorr(argv[optind]);
	      if (gaincorrflag==true)
		{
		  cout << "duplicate adc calib files, aborting..." << endl;
		  exit(1);
		}
	      gaincorrflag = true;
	    }
/*
          else if ( fname.BeginsWith("MpcCal.leakage") )
            {
              mpccalib->Download_LeakageCorrection("/phenix/u/chiu/subsys_mpc/database/");
              if (leakageflag==true)
                {
                  cout << "duplicate leakage files, aborting..." << endl;
                  exit(1);
                }
              leakageflag = true;
            }
*/
	  else if ( fname.EndsWith(".led") )
	    {
	      mpccalib->Download_Led(argv[optind]);
	      if (ledflag==true)
		{
		  cout << "duplicate led calib files, aborting..." << endl;
		  exit(1);
		}
	      ledflag = true;
	    }
	  else if ( fname.EndsWith(".pshape") )
	    {
	      mpccalib->Download_Shapes(argv[optind],"p");
	      if (pshapeflag==true)
		{
		  cout << "duplicate pedestal calib files, aborting..." << endl;
		  exit(1);
		}
	      pshapeflag = true;
	    }
	  else if ( fname.EndsWith(".np1shape") )
	    {
	      mpccalib->Download_Shapes(argv[optind],"np1");
	      if (np1shapeflag==true)
		{
		  cout << "duplicate pedestal calib files, aborting..." << endl;
		  exit(1);
		}
	      np1shapeflag = true;
	    }
	  else if ( fname.EndsWith(".lopostped") )
	    {
	      mpccalib->Download_Pedestals(argv[optind],"lopost");
	      if (lopostpedflag==true)
		{
		  cout << "duplicate lo post pedestal calib files, aborting..." << endl;
		  exit(1);
		}
	      lopostpedflag = true;
	    }
	  else if ( fname.EndsWith(".lopreped") )
	    {
	      mpccalib->Download_Pedestals(argv[optind],"lopre");
	      if (loprepedflag==true)
		{
		  cout << "duplicate lo pre pedestal calib files, aborting..." << endl;
		  exit(1);
		}
	      loprepedflag = true;
	    }
	  else if ( fname.EndsWith(".hipostped") )
	    {
	      mpccalib->Download_Pedestals(argv[optind],"hipost");
	      if (hipostpedflag==true)
		{
		  cout << "duplicate hi post pedestal calib files, aborting..." << endl;
		  exit(1);
		}
	      hipostpedflag = true;
	    }
	  else if ( fname.EndsWith(".hipreped") )
	    {
	      mpccalib->Download_Pedestals(argv[optind],"hipre");
	      if (hiprepedflag==true)
		{
		  cout << "duplicate hi pre pedestal calib files, aborting..." << endl;
		  exit(1);
		}
	      hiprepedflag = true;
	    }
	  else if ( fname.EndsWith(".hiloratio") )
	    {
	      mpccalib->Download_HiLoRatio(argv[optind]);
	      if (hiloratioflag==true)
		{
		  cout << "duplicate hi lo ratio calib files, aborting..." << endl;
		  exit(1);
		}
	      hiloratioflag = true;
	    }
	  else if ( fname.EndsWith(".hilolimit") )
	    {
	      mpccalib->Download_HiLoLimit(argv[optind]);
	      if (hilolimitflag==true)
		{
		  cout << "duplicate hi lo limit calib files, aborting..." << endl;
		  exit(1);
		}
	      hilolimitflag = true;
	    }
	  else if ( fname.EndsWith(".leastcount") )
	    {
	      mpccalib->Download_TDC_LeastCount(argv[optind]);
	      if (leastcountflag==true)
		{
		  cout << "duplicate least count calib files, aborting..." << endl;
		  exit(1);
		}
	      leastcountflag = true;
	    }
	  else if ( fname.EndsWith(".overflow") )
	    {
	      mpccalib->Download_TDC_Overflow(argv[optind]);
	      if (overflowflag==true)
		{
		  cout << "duplicate overflow calib files, aborting..." << endl;
		  exit(1);
		}
	      overflowflag = true;
	    }
          else if ( fname.EndsWith(".map") )
            {
              mpcmap->Download_Maps(argv[optind]);
              if (mapflag==true)
                {
                  cout << "duplicate map calib files, aborting..." << endl;
                  exit(1);
                }
              mapflag = true;
            }

	  optind++;
	}
    }
  else
    {
      // get types of calibrations to download

      while ( optind < argc )
        {
	  if ( strcmp(argv[optind],"all") == 0 ) allflag = true;
	  else if ( strcmp(argv[optind],"gain") == 0 ) gainflag = true;
	  else if ( strcmp(argv[optind],"gains") == 0 ) gainflag = true;
	  else if ( strcmp(argv[optind],"ped") == 0 ) pedflag = true;
	  else if ( strcmp(argv[optind],"gaincorr") == 0 ) gaincorrflag = true;
	  else if ( strcmp(argv[optind],"leakage") == 0 ) leakageflag = true;
	  else if ( strcmp(argv[optind],"lopost") == 0 ) lopostpedflag = true;
	  else if ( strcmp(argv[optind],"lopre") == 0 ) loprepedflag = true;
	  else if ( strcmp(argv[optind],"hipost") == 0 ) hipostpedflag = true;
	  else if ( strcmp(argv[optind],"hipre") == 0 ) hiprepedflag = true;
	  else if ( strcmp(argv[optind],"hiloratio") == 0 ) hiloratioflag = true;
	  else if ( strcmp(argv[optind],"hilolimit") == 0 ) hilolimitflag  = true;
	  else if ( strcmp(argv[optind],"leastcount") == 0 ) leastcountflag = true;
	  else if ( strcmp(argv[optind],"overflow") == 0 ) overflowflag = true;
	  else if ( strcmp(argv[optind],"map") == 0 ) mapflag = true;
	  else if ( strcmp(argv[optind],"led") == 0 ) ledflag = true;
	  else if ( strcmp(argv[optind],"pshape") == 0 ) pshapeflag = true;
	  else if ( strcmp(argv[optind],"np1shape") == 0 ) np1shapeflag = true;
	  else 
	    {
	      cout << "error: " << argv[optind]
		   << " is an unknown mpc calibration type" << endl;
	      exit(-1);
            }

	  optind++;
        }
    }

  //-*** ask for timestamp or run number

  int year, month, day, hour, minute, second;
  PHTimeStamp tstamp;
  if ( runnumber==0 )
    {
      cout << "Please Enter the Start Time for the Calibration" << endl;
      cout << "Year(YYYY): ";
      cin >> year;
      cout << "     Month: ";
      cin >> month;
      checkrange( month>=1 && month<=12, "require month>=1 && month<=12" );
      cout << "       Day: ";
      cin >> day;
      checkrange( day>=1 && day<=31, "require day>=1 && day<=31" );
      cout << "      Hour: ";
      cin >> hour;
      checkrange( hour>=0 && hour<24, "require hour>=0 && hour<24" );
      cout << "    Minute: ";
      cin >> minute;
      checkrange( minute>=0 && minute<60, "require minute>=0 && minute<60" );
      cout << "    Second: ";
      cin >> second;
      checkrange( second>=0 && second<60, "require second>=0 && second<60" );

      tstamp.set( year, month, day, hour, minute, second, 0 );
    }
  else
    {
      RunToTime *runtotime = RunToTime::instance();
      PHTimeStamp *begintime = runtotime->getBeginTime( runnumber );
      if ( begintime != 0 )
        {
          tstamp.setTics( runtotime->getBeginTime( runnumber )->getTics() );
        }
      else
        {
          cout << "ERROR: Couldn't find run, aborting... " 
            << runnumber << endl;
          exit(1);
        }
    }


  PHTimeStamp tstop;

  if ( storeflag )
    {
      int end_year, end_month, end_day, end_hour, end_minute, end_second;
      if ( runnumber==0 )
        {
          cout << "Please Enter the End Time for the Calibration (or > 2038 for max)" << endl;
          cout << "Year(YYYY): ";
          cin >> end_year;
          if ( end_year > 2038 )
            {
              tstop.setToFarFuture();
            }
          else
            {
              cout << "     Month: ";
              cin >> end_month;
              checkrange( end_month>=1 && end_month<=12, "require month>=1 && month<=12" );
              cout << "       Day: ";
              cin >> end_day;
              checkrange( end_day>=1 && end_day<=31, "require day>=1 && day<=31" );
              cout << "      Hour: ";
              cin >> end_hour;
              checkrange( end_hour>=0 && end_hour<24, "require hour>=0 && hour<24" );
              cout << "    Minute: ";
              cin >> end_minute;
              checkrange( end_minute>=0 && end_minute<60, "require minute>=0 && minute<60" );
              cout << "    Second: ";
              cin >> end_second;
              checkrange( end_second>=0 && end_second<60, "require second>=0 && second<60" );
     
              tstop.set( end_year, end_month, end_day, end_hour, end_minute, end_second, 0 );
            }
        }
      else
        {
          RunToTime *runtotime = RunToTime::instance();
          tstop.setTics( runtotime->getEndTime( runnumber )->getTics() + 10 );
        }
    }

  cout << "Begin Timestamp is:  " << tstamp << endl;
  if ( storeflag ) cout << "End Timestamp is:    " << tstop << endl;

  if ( storeflag )
    {
      if ( interactiveflag )
        {
          //char temp_answer[10];
          string temp_answer;
          cout << "Are you sure you want to write to the database with the above date? (y/n)" << endl; 
          //cin.getline( temp_answer, 10 );	// this is a kludge
          cin >> temp_answer;	// this is a kludge
          //if ( strcmp(temp_answer,"y")==0 || strcmp(temp_answer,"yes")==0 )
          if ( temp_answer[0] == 'y' )
            {
              cout << "Okay, going ahead.....cross your fingers...." << endl;
            }
          else
            {
cout << "temp " << temp_answer << endl;
              cout << "Aborting..." << endl;
              exit(0);
    	    }
        }

      // get description and name for the database
      if ( description.size() == 0 )
        {
          cout << "Please input description for this calibration parameter:" << endl;
          //char temp_description[256];
          //cin.getline(temp_description,256);
          //description = temp_description;
          getline(cin,description);
        }
      if ( name.size() == 0 )
        {
          cout << "Please enter your name:" << endl;
          //char temp_name[256];
          //cin.getline(temp_name,256);
          //name = temp_name;
          getline(cin,name);
        }
    }

  if ( storeflag )
    {
      cout << "name2 " << name << endl;
      cout << "desc2 " << description << endl;
      //mpccalib->Dump_to_file("LED");
      cout << "Begin Timestamp is:  " << tstamp << endl;
      cout << "End Timestamp is:    " << tstop << endl;
    }

  //-* save calibrations to database from file(s)
  if (storeflag)
    {
      if (gainflag||allflag) mpccalib->StoreInDatabase( tstamp, "GAINS", name, description, tstop );
      if (pedflag||allflag) mpccalib->StoreInDatabase( tstamp, "PED", name, description, tstop );
      if (gaincorrflag||allflag) mpccalib->StoreInDatabase( tstamp, "GAINCORR", name, description, tstop );
      if (leakageflag||allflag) mpccalib->StoreInDatabase( tstamp, "LEAKAGE", name, description, tstop );
      if (hiloratioflag||allflag) mpccalib->StoreInDatabase( tstamp, "HILORATIO", name, description, tstop );
      if (hilolimitflag||allflag) mpccalib->StoreInDatabase( tstamp, "HILOLIMIT", name, description, tstop );
      if (leastcountflag||allflag) mpccalib->StoreInDatabase( tstamp, "LEASTCOUNT", name, description, tstop );
      if (overflowflag||allflag) mpccalib->StoreInDatabase( tstamp, "OVERFLOW", name, description, tstop );
      if (lopostpedflag||allflag) mpccalib->StoreInDatabase( tstamp, "LOPOSTPED", name, description, tstop );
      if (loprepedflag||allflag) mpccalib->StoreInDatabase( tstamp, "LOPREPED", name, description, tstop );
      if (hipostpedflag||allflag) mpccalib->StoreInDatabase( tstamp, "HIPOSTPED", name, description, tstop );
      if (hiprepedflag||allflag) mpccalib->StoreInDatabase( tstamp, "HIPREPED", name, description, tstop );
      if (mapflag||allflag) mpcmap->StoreInDatabase( tstamp, name.c_str(), description.c_str(), tstop );
      if (ledflag) mpccalib->StoreInDatabase( tstamp, "LED", name, description, tstop );
      if (pshapeflag)
        {
          mpccalib->StoreInDatabase( tstamp, "PSHAPE", name, description, tstop );
          mpccalib->StoreInDatabase( tstamp, "PSHERR", name, description, tstop );
        }
      if (np1shapeflag)
        {
          mpccalib->StoreInDatabase( tstamp, "NP1SHAPE", name, description, tstop );
          mpccalib->StoreInDatabase( tstamp, "NP1SHERR", name, description, tstop );
        }
    }
  
  //-* read calibrations from database and dump to file(s)
  if (!storeflag)
    {
      if (gainflag||allflag)
        {
          mpccalib->Download( tstamp, "GAINS" );
          mpccalib->Dump_to_file( "GAINS" );
        }
      if (pedflag||allflag)
        {
          mpccalib->Download( tstamp, "PED" );
          mpccalib->Dump_to_file( "PED" );
        }
      if (gaincorrflag||allflag)
        {
          mpccalib->Download( tstamp, "GAINCORR" );
          mpccalib->Dump_to_file( "GAINCORR" );
        }
      if (leakageflag||allflag)
        {
          mpccalib->Download( tstamp, "LEAKAGE" );
          mpccalib->Dump_to_file( "LEAKAGE" );
        }
      if (hiloratioflag||allflag)
        {
          mpccalib->Download( tstamp, "HILORATIO" );
          mpccalib->Dump_to_file( "HILORATIO" );
        }
      if (hilolimitflag||allflag)
        {
          mpccalib->Download( tstamp, "HILOLIMIT" );
          mpccalib->Dump_to_file( "HILOLIMIT" );
        }
      if (leastcountflag||allflag)
        {
          mpccalib->Download( tstamp, "LEASTCOUNT" );
          mpccalib->Dump_to_file( "LEASTCOUNT" );
        }
      if (overflowflag||allflag)
        {
          mpccalib->Download( tstamp, "OVERFLOW" );
          mpccalib->Dump_to_file( "OVERFLOW" );
        }
      if (lopostpedflag||allflag)
        {
          mpccalib->Download( tstamp, "LOPOSTPED" );
          mpccalib->Dump_to_file( "LOPOSTPED" );
        }
      if (loprepedflag||allflag)
        {
          mpccalib->Download( tstamp, "LOPREPED" );
          mpccalib->Dump_to_file( "LOPREPED" );
        }
      if (hipostpedflag||allflag)
        {
          mpccalib->Download( tstamp, "HIPOSTPED" );
          mpccalib->Dump_to_file( "HIPOSTPED" );
        }
      if (hiprepedflag||allflag)
        {
          mpccalib->Download( tstamp, "HIPREPED" );
          mpccalib->Dump_to_file( "HIPREPED" );
        }
      if (mapflag||allflag)
        {
          mpcmap->Download_Maps( tstamp );
          mpcmap->Dump_to_file();
        }
      if (ledflag)
        {
          mpccalib->Download( tstamp, "LED" );
          mpccalib->Dump_to_file( "LED" );
        }
      if (pshapeflag)
        {
          mpccalib->Download( tstamp, "PSHAPE" );
          mpccalib->Download( tstamp, "PSHERR" );
          mpccalib->Dump_to_file( "PSHAPE" );
        }
      if (np1shapeflag)
        {
          mpccalib->Download( tstamp, "NP1SHAPE" );
          mpccalib->Download( tstamp, "NP1SHERR" );
          mpccalib->Dump_to_file( "NP1SHAPE" );
        }
    }
  
  return 1;
}

