void readHV(const int myrunnumber) {

// Load shared libraries
  gROOT->Macro("loadlibraries.C");

  char gfile1[80];
  sprintf(gfile1,"ListHV_%d.txt",myrunnumber);
  FILE *fpw1 = fopen(gfile1,"w");

// Create hv object
   TecHVObject* TCO = new TecHVObject();
   cout << TCO->getName() << " created." << endl;
   TCO->setDebugLevel(2);

// Set run number

   TCO->setRunNumber(myrunnumber);         // find run start time

// Read info from the database
   
   PHBoolean status = TCO->Fetch();


// Now write values to a file you want to look at.

   const int ntyp=2;   // has to be <= 2
   const int nplan=48; // has to be <= 48
   const int nsides=2;
   const int nplanes=6;
   const int nsectors=4;
   const int TECMAXSECT = 4; // has to be the same as TecBasicObject.hh
   const int TECMAXSIDE = 2; // has to be the same as TecBasicObject.hh
   const int TECMAXPLANE = 6; // has to be the same as TecBasicObject.hh
   int i, j, iside, iplane, isector, index;
   float hv[nplan][ntyp];

   for (j=0; j<ntyp; j++){
     for (i=0; i<nplan; i++){
       hv[i][j] = TCO->getHVVal(i,j);
     }
   }
   for (j=0; j<ntyp; j++){
     for (iside=0; iside<nsides; iside++){
       for (isector=0; isector<nsectors; isector++){
	 for (iplane=0; iplane<nplanes; iplane++){
	   index = iside + iplane*TECMAXSIDE + isector*TECMAXSIDE*TECMAXPLANE;
	   fprintf(fpw1,
		   "hvtype %d sector %d plane %d side %d hvval %.1f\n",
		   j, isector, iplane, iside, hv[index][j]);
	   //printf(
	   //   "hvtype %d sector %d plane %d side %d hvval %.1f\n",
	   //   j, isector, iplane, iside, hv[index][j]);
	 }
       }
     }
   }

}
