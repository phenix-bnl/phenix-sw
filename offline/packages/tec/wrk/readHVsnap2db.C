void readHVsnap2db(Int_t year=2003,
		   Int_t month=1,
		   Int_t day=1,
		   Int_t hour=0,
		   Int_t minute=0,
		   Int_t second=0) {

  const char* Month[12] = 
  {
    "Jan","Feb","Mar",
    "Apr","May","Jun",
    "Jul","Aug","Sep",
    "Oct","Nov","Dec"
  };

  gROOT->Reset();

// Load shared libraries
  gROOT->Macro("loadlibraries.C");

// Create hv object
  TecHVObject* TCO = new TecHVObject();
  cout << TCO->getName() << " created." << endl;
  TCO->setDebugLevel(2);

// Set time
  PHTimeStamp Tstart(year,month,day,hour,minute,second);
  PHTimeStamp Tend(year,month,day,hour,minute,second);
  Tstart -= 30;
  Tend += 30;
  cout << Tstart << endl;
  cout << Tend << endl;

// Read values from snapshot file
  char gfile[80];
  char strtmp[80];
  float flotmp;
  const int ntyp=2;   // has to be <= 2
  const int nplan=48; // has to be <= 48
  const int nsides=2;
  const int nplanes=6;
  const int nsectors=4;
  const int TECMAXSECT = 4; // has to be the same as TecBasicObject.hh
  const int TECMAXSIDE = 2; // has to be the same as TecBasicObject.hh
  const int TECMAXPLANE = 6; // has to be the same as TecBasicObject.hh
  int iside,iplane,isector,index;
  int i,j;
  float hv[nplan][ntyp];

  sprintf(gfile,"snapshots/%s.%d.%04d.%02d%02d%02d.txt",
	  Month[month-1],day,year,hour,minute,second);
  ifstream in(gfile);
  if (!in){
    cout << "Cannot open file " << gfile << endl;
    exit(1);
  }
  cout << "Reading file " << gfile << endl;
  in.getline(strtmp,80);
  in.getline(strtmp,80);
  for (iside=nsides-1; iside>-1; iside--){
    for (i=0; i<nplan/nsides; i++){
      for (j=0; j<ntyp; j++){
	in >> strtmp >> isector >> iplane 
	   >> strtmp >> strtmp >> strtmp >> strtmp >> flotmp;
	index = iside + iplane*TECMAXSIDE + isector*TECMAXSIDE*TECMAXPLANE;
	in.getline(strtmp,80);
	TCO.setHVVal(index, j, flotmp);
      }
    }
  }
  in.close();

// Write info to the database
  PHBoolean status2 = TCO->UpdateHVVal(&Tstart, &Tend);

//
// Now read back from the database
//
// Set search time
  TCO->setTimeStamp(PHTimeStamp(year,month,day,hour,minute,second));

// Set location (calibration bank name)
  char* location = "hv.tec.run00";

// Read info from the database
  PHBoolean status = TCO->FetchHVVal();

// Now write values to a file you want to look at.
  for (j=0; j<ntyp; j++){
    for (i=0; i<nplan; i++){
      hv[i][j] = TCO->getHVVal(i,j);
    }
  }

  char gfile1[80];
  sprintf(gfile1,
	  "ListHV_%s.%02d.%04d.%02d%02d%02d.txt",
	  Month[month-1],day,year,hour,minute,second);
  FILE *fpw1 = fopen(gfile1,"w");

  for (j=0; j<ntyp; j++){
    for (iside=0; iside<nsides; iside++){
      for (isector=0; isector<nsectors; isector++){
	for (iplane=0; iplane<nplanes; iplane++){
	  index = iside + iplane*TECMAXSIDE + isector*TECMAXSIDE*TECMAXPLANE;
	  fprintf(fpw1,
		  "hvtype %d sector %d plane %d side %d hvval %.1f\n",
		  j, isector, iplane, iside, hv[index][j]);
	  //printf(
	  // "hvtype %d sector %d plane %d side %d hvval %.1f\n",
	  // j, isector, iplane, iside, hv[index][j]);
	}
      }
    }
  }

}
