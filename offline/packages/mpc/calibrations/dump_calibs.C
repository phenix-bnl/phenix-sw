void mpcmap_dbase(const int storeflag = 0)
{
  gSystem->Load("libmpc.so");

/*
  MpcCalib *mpccalib = MpcCalib::instance();
  mpccalib->Print("ALL");
*/

  MpcMap *mpcmap = MpcMap::instance();
  //mpcmap->Print("ALL");
  mpcmap->Print();

  if ( storeflag==1 )
    {
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

  char description[256];
  char name[20];
  if ( storeflag )
    {
      char temp_answer[10];
      cout << "Are you sure you want to write to the database with the above date? (y/n)" << endl;
      cin.getline( temp_answer, 10 );   // this is a kludge
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

      mpcmap->StoreInDatabase(tstamp);
    }
}

void checkrange(bool rangetest, const char *message)
{
  if ( !rangetest )
    {
      cout << message << endl;
      exit(-3);
    }
}


void mpccalib_dbase(const int storeflag = 0)
{
  gSystem->Load("libmpc.so");

  MpcCalib *mpccalib = MpcCalib::instance();
  //mpccalib->Print("ALL");

  if ( storeflag==0 )
    {
      mpccalib->Download_Gains("MpcCal.gains");
      mpccalib->Dump("GAINS");
    }

  if ( storeflag==1 )
    {
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

  char description[256];
  char name[20];
  if ( storeflag )
    {
      char temp_answer[10];
      cout << "Are you sure you want to write to the database with the above date? (y/n)" << endl;
      cin.getline( temp_answer, 10 );   // this is a kludge
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

      mpccalib->Download_Gains("MpcCal.gains");
      mpccalib->StoreInDatabase(tstamp,"GAINS",name,description);
    }
}

