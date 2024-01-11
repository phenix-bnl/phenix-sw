#include <iostream>
#include <cassert>
#include <cstdlib>
#include <unistd.h>
#include <sys/types.h>
#include <dirent.h>
#include <libgen.h>
#include "ZdcCalib.hh"
#include "PHTimeStamp.h"
#include <TString.h>
#include <TROOT.h>

// to instantiate the PostGres factories
#include "PgPostApplication.hh"
#include "PgPostBankManager.hh"
#include "RunToTimePg.hh"

using namespace std;

TROOT groot("tdcoverflow","overflow calibration");

void usage()
{
  cout << "to download from the database:" << endl;
  cout << "usage: zdcdbase <calibtype>" << endl;
  cout << "  where <calibtype> can be " << endl;
  cout << "         all adc pedestal pmtgain tdc overflow" << endl;
  cout << "         slew tdclut tzero zvtx smdoffset" << endl;
  cout << endl;
  cout << "to save to the database:" << endl;
  cout << "usage: zdcdbase -w <calibfile>" << endl;
  cout << "  where <calibfile> is the calibration file in the form of" << endl;
  cout << "  ZdcCalib.xxxxxx.*" << endl;
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


  PgPostApplication::Register();
  PgPostBankManager::Register();
  RunToTimePg::Register();

  bool storeflag = false;	// true = store in database, false = read from database

  // which calibration type to process
  bool allflag = false;
  bool adcflag = false;
  bool pedflag = false;
  bool pmtflag = false;
  bool tdcflag = false, tdc0flag = false, tdc1flag = false;
  bool oflowflag = false, oflow0flag = false, oflow1flag = false;
  bool slewflag = false, slew0flag = false, slew1flag = false;
  bool tdclutflag = false, tdc0lutflag = false, tdc1lutflag = false;
  bool tzeroflag = false;
  bool zvtxflag = false;
  bool smdoffsetflag = false;

  // set up the zdc calibration objects
  ZdcCalib zcalib;

  ZdcCalibPar<PdbPmtFitPar>* adcpar = zcalib.getAdcGain();
  ZdcCalibPar<PdbPmtPeak>*   pedpar = zcalib.getPedestal();
  ZdcCalibPar<PdbPmtPeak>*   pmtpar = zcalib.getPmtGain();
  ZdcCalibPar<PdbPmtFitPar>* tdc0par = zcalib.getTdcGain0();
  ZdcCalibPar<PdbPmtFitPar>* tdc1par = zcalib.getTdcGain1();
  ZdcCalibPar<PdbPmtPeak>* oflow0par = zcalib.getOverflow0();
  ZdcCalibPar<PdbPmtPeak>* oflow1par = zcalib.getOverflow1();
  ZdcCalibPar<PdbPmtFitPar>* slew0par = zcalib.getSlewing0();
  ZdcCalibPar<PdbPmtFitPar>* slew1par = zcalib.getSlewing1();
  ZdcCalibPar<PdbPmtPeak>* tdc0lutpar = zcalib.getTdc0LUT();
  ZdcCalibPar<PdbPmtPeak>* tdc1lutpar = zcalib.getTdc1LUT();
  ZdcCalibPar<PdbPmtPeak>* tzeropar = zcalib.getTzero();
  ZdcCalibPar<PdbPmtPeak>* zvtxpar = zcalib.getZvtx();
  ZdcCalibPar<PdbPmtPeak>* smdoffsetpar = zcalib.getSmdOffset();

  // process command line options
  int c;
  extern int optind;
  extern char *optarg;
  while ((c = getopt(argc, argv, "wh")) != -1)
    {
      switch(c)
	{
	case 'w':		// write to database
	  storeflag = true;
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
	      fname += "/ZdcCalib";
	      int status = zcalib.restore( (char*)fname.Data() );
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
	  if ( fname.BeginsWith("ZdcCalib.adc") ) 
	    {
	      adcpar->restore(argv[optind],"adc");
	      if (adcflag==true)
		{
		  cout << "duplicate adc calib files, aborting..." << endl;
		  exit(1);
		}
	      adcflag = true;
	    }
	  else if ( fname.BeginsWith("ZdcCalib.pedestal") )
	    {
	      pedpar->restore(argv[optind],"pedestal");
	      if (pedflag==true)
		{
		  cout << "duplicate pedestal calib files, aborting..." << endl;
		  exit(1);
		}
	      pedflag = true;
	    }
	  else if ( fname.BeginsWith("ZdcCalib.pmtgain") )
	    {
	      pmtpar->restore(argv[optind],"pmtgain");
	      if (pmtflag==true)
		{
		  cout << "duplicate pmtgain calib files, aborting..." << endl;
		  exit(1);
		}
	      pmtflag = true;
	    }
	  else if ( fname.BeginsWith("ZdcCalib.tdc0lut") )
	    {
	      tdc0lutpar->restorelut(argv[optind],"tdc0lut");
	      if (tdc0lutflag==true)
		{
		  cout << "duplicate tdc0lut calib files, aborting..." << endl;
		  exit(1);
		}
	      tdc0lutflag = true;
	    }
	  else if ( fname.BeginsWith("ZdcCalib.tdc1lut") )
	    {
	      tdc1lutpar->restorelut(argv[optind],"tdc1lut");
	      if (tdc1lutflag==true)
		{
		  cout << "duplicate tdc1lut calib files, aborting..." << endl;
		  exit(1);
		}
	      tdc1lutflag = true;
	    }
	  else if ( fname.BeginsWith("ZdcCalib.tdc0") )
	    {
	      tdc0par->restore(argv[optind],"tdc0");
	      if (tdc0flag==true)
		{
		  cout << "duplicate tdc0 calib files, aborting..." << endl;
		  exit(1);
		}
	      tdc0flag = true;
	    }
	  else if ( fname.BeginsWith("ZdcCalib.tdc1") )
	    {
	      tdc1par->restore(argv[optind],"tdc1");
	      if (tdc1flag==true)
		{
		  cout << "duplicate tdc1 calib files, aborting..." << endl;
		  exit(1);
		}
	      tdc1flag = true;
	    }
	  else if ( fname.BeginsWith("ZdcCalib.overflow0") )
	    {
	      oflow0par->restore(argv[optind],"overflow0");
	      if (oflow0flag==true)
		{
		  cout << "duplicate overflow0 calib files, aborting..." << endl;
		  exit(1);
		}
	      oflow0flag = true;
	    }
	  else if ( fname.BeginsWith("ZdcCalib.overflow1") )
	    {
	      oflow1par->restore(argv[optind],"overflow1");
	      if (oflow1flag==true)
		{
		  cout << "duplicate overflow1 calib files, aborting..." << endl;
		  exit(1);
		}
	      oflow1flag = true;
	    }
	  else if ( fname.BeginsWith("ZdcCalib.slewpar0") )
	    {
	      slew0par->restore(argv[optind],"slewpar0");
	      if (slew0flag==true)
		{
		  cout << "duplicate slewpar0 calib files, aborting..." << endl;
		  exit(1);
		}
	      slew0flag = true;
	    }
	  else if ( fname.BeginsWith("ZdcCalib.slewpar1") )
	    {
	      slew1par->restore(argv[optind],"slewpar1");
	      if (slew1flag==true)
		{
		  cout << "duplicate slewpar1 calib files, aborting..." << endl;
		  exit(1);
		}
	      slew1flag = true;
	    }
	  else if ( fname.BeginsWith("ZdcCalib.tzero") )
	    {
	      tzeropar->restore(argv[optind],"tzero");
	      if (tzeroflag==true)
		{
		  cout << "duplicate tzero calib files, aborting..." << endl;
		  exit(1);
		}
	      tzeroflag = true;
	    }
	  else if ( fname.BeginsWith("ZdcCalib.zvtx") )
	    {
	      zvtxpar->restore(argv[optind],"zvtx");
	      if (zvtxflag==true)
		{
		  cout << "duplicate zvtx calib files, aborting..." << endl;
		  exit(1);
		}
	      zvtxflag = true;
	    }
          else if ( fname.BeginsWith("ZdcCalib.smdoffset") )
            {
              smdoffsetpar->restore(argv[optind],"smdoffset");
              if (smdoffsetflag==true)
                {
                  cout << "duplicate smdoffset calib files, aborting..." << endl;
                  exit(1);
                }
              smdoffsetflag = true;
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
	  else if ( strcmp(argv[optind],"adc") == 0 ) adcflag = true;
	  else if ( strcmp(argv[optind],"pedestal") == 0 ) pedflag = true;
	  else if ( strcmp(argv[optind],"pmtgain") == 0 ) pmtflag = true;
	  else if ( strcmp(argv[optind],"tdc") == 0 ) tdcflag = true;
	  else if ( strcmp(argv[optind],"overflow") == 0 ) oflowflag = true;
	  else if ( strcmp(argv[optind],"slew") == 0 ) slewflag = true;
	  else if ( strcmp(argv[optind],"tdclut") == 0 ) tdclutflag = true;
	  else if ( strcmp(argv[optind],"tzero") == 0 ) tzeroflag = true;
	  else if ( strcmp(argv[optind],"zvtx") == 0 ) zvtxflag = true;
	  else if ( strcmp(argv[optind],"smdoffset") == 0 ) smdoffsetflag = true;
	  else 
	    {
	      cout << "error: " << argv[optind]
		   << " is an unknown zdc calibration type" << endl;
	      exit(-1);
            }

	  optind++;
        }
    }

  //-*** ask for timestamp or run number

  int year, month, day, hour, minute, second;
  cout << "Please Enter the Time for the Calibration" << endl;
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

  PHTimeStamp tstamp( year, month, day, hour, minute, second, 0 );
  cout << "Timestamp is " << tstamp << endl;

  if ( !storeflag ) zcalib.restore(tstamp);

  char description[256];
  char name[20];
  if ( storeflag )
    {
      char temp_answer[10];
      cout << "Are you sure you want to write to the database with the above date? (y/n)" << endl; 
      cin.getline( temp_answer, 10 );	// this is a kludge
      cin.getline( temp_answer, 10 );
      if ( strcmp(temp_answer,"y")==0 || strcmp(temp_answer,"yes")==0 )
	{
	  cout << "Okay, going ahead.....cross your fingers...." << endl;
	}
      else
	{
	  cout << "Aborting..." << endl;
	  exit(0);
	}

      // get description and name for the database
      cout << "Please input description for this calibration parameter:" << endl;
      cin.getline(description,256);
      cout << "Please enter your name:" << endl;
      cin.getline(name,20);
    }

  //-* save calibrations to database from file(s)
  if (storeflag)
    {
      if (adcflag||allflag) adcpar->store( tstamp, "adc", description, name );
      if (pedflag||allflag) pedpar->store( tstamp, "pedestal", description, name );
      if (pmtflag||allflag) pmtpar->store( tstamp, "pmtgain", description, name );
      if (tdc0flag||allflag) tdc0par->store( tstamp, "tdc0", description, name );
      if (tdc1flag||allflag) tdc1par->store( tstamp, "tdc1", description, name );
      if (oflow0flag||allflag) oflow0par->store( tstamp, "overflow0", description, name );
      if (oflow1flag||allflag) oflow1par->store( tstamp, "overflow1", description, name );
      if (slew0flag||allflag) slew0par->store( tstamp, "slewpar0", description, name );
      if (slew1flag||allflag) slew1par->store( tstamp, "slewpar1", description, name );
      if (tdc0lutflag||allflag) tdc0lutpar->storelut( tstamp, "tdc0lut", description, name );
      if (tdc1lutflag||allflag) tdc1lutpar->storelut( tstamp, "tdc1lut", description, name );
      if (tzeroflag||allflag) tzeropar->store( tstamp, "tzero", description, name );
      if (zvtxflag||allflag) zvtxpar->store( tstamp, "zvtx", description, name );
      if (smdoffsetflag||allflag) smdoffsetpar->store( tstamp, "smdoffset", description, name );
    }
  
  //-* read calibrations from database and dump to file(s)
  if (!storeflag)
    {
      if (adcflag||allflag) adcpar->dump("adc");
      if (pedflag||allflag) pedpar->dump("pedestal");
      if (pmtflag||allflag) pmtpar->dump("pmtgain");
      if (tdcflag||allflag)
        {
          tdc0par->dump("tdc0");
          tdc1par->dump("tdc1");
        }
      if (oflowflag||allflag)
        {
 	  oflow0par->dump("overflow0");
          oflow1par->dump("overflow1");
	}
      if (slewflag||allflag)
        {
          slew0par->dump("slewpar0");
          slew1par->dump("slewpar1");
	}
      if (tdclutflag||allflag)
        {
	  tdc0lutpar->dump("tdc0lut");
	  tdc1lutpar->dump("tdc1lut");
	}
      if (tzeroflag||allflag) tzeropar->dump("tzero");
      if (zvtxflag||allflag) zvtxpar->dump("zvtx");
      if (smdoffsetflag||allflag) smdoffsetpar->dump("smdoffset");
    }
  
  return 1;
}

