void BeamLoop()
{
  gSystem->Load("libfun4all.so");
  
  int runs[37]= {69502,
		 69650,
		 69654,
		 70087,
		 70451,
		 70456,
		 71005,
		 72119,
		 72361,
		 72669,
		 74413,
		 74428,
		 74443,
		 74691,
		 74857,
		 74865,
		 74868,
		 75622,
		 75631,
		 75796,
		 75800,
		 76053,
		 76073,
		 76693,
		 76797,
		 76864,
		 76995,
		 77380,
		 77391,
		 77392,
		 78033,
		 78182,
		 78210,
		 78435,
		 83664,
		 88409,
		 91278};

  float xOffsets[37]= {0 ,			    
		       0.00719911 ,		    
		       0.0358691 ,		    
		       0.0613083 ,		    
		       -0.0167215 ,		    
		       0.0145572 ,		    
		       0.0145572 + 0.043788,	    
		       0.00734437 ,		    
		       -0.00768989 ,		    
		       -0.0314354 ,		    
		       -0.0608845 ,		    
		       -0.0438216 ,		    
		       -0.0633091 ,		    
		       0.0255278 ,		    
		       0.00643027 ,		    
		       -0.0662487 ,		    
		       -0.044189 ,		    
		       -0.0558256 ,		    
		       -0.0412128 ,		    
		       -0.0306118 ,		    
		       -0.089702 ,		    
		       -0.14673 ,		    
		       -0.14673 - 0.184347,	    
		       -0.0443969 ,		    
		       0.032352 ,		    
		       0.0809593 ,		    
		       0.00811476 ,		    
		       0.0401714 ,		    
		       0.0664962 ,		    
		       0.0369268 ,		    
		       0.00634503 ,		    
		       -0.0202398 ,		    
		       0.0104392 ,		    
		       0.00147718 ,		    
		       0.00147718 + 0.0957,	    
		       0.00147718 + 0.082682,	    
		       0.00147718 + 0.007307};

  float yOffsets[37] = {0 ,				
			-0.0765645 ,			
			0.00955725 ,			
			-0.0217168 ,			
			0.00611613 ,			
			0.0331331 ,			
			-0.0217168 - 0.045120,		
			0.00729831 ,			
			-0.0206502 ,			
			-0.148531 ,			
			-0.207407 ,			
			-0.183647 ,			
			-0.196092 ,			
			-0.201648 ,			
			-0.0404967 ,			
			-0.0424322 ,			
			-0.0556249 ,			
			-0.0273209 ,			
			-0.0522962 ,			
			-0.0437686 ,			
			-0.085174 ,			
			-0.0926768 ,			
			-0.0926768 +0.133623 ,		
			-0.105845 ,			
			-0.0556219 ,			
			-0.0868364 ,			
			-0.0638488 ,			
			-0.12156 ,				
			-0.0583103 ,			
			-0.0919542 ,			
			-0.0654435 ,			
			-0.0380082 ,			
			-0.0284569 ,			
			-0.00868927 ,			
			-0.00868927 + 0.172756,		
			-0.00868927 - 0.086620,		
			-0.00868927 + 0.067716};

  // Make an address object for dependency purposes...
  PHDchAddressObject *add = new PHDchAddressObject();
  add->setFlagMC(0);
  add->initialize();
  
  //  Set up bank names and validity ranges...
  PdbBankID bankID;
  bankID.set("*.DCH.FIRST.AUTO");
  bankID.setInternalValue(1); 
  char * geometryNameDB = "calibdch_v3_geo";
  char* descrip="Insertion by Hand.";
  PHTimeStamp tstop, tstart;

  for (int i=0; i<37; i++) {
    int begin = runs[i];
    float x = xOffsets[i];
    float y = yOffsets[i];

    RunToTime *rtt = RunToTime::instance();
    tstart = *(rtt->getBeginTime(begin));
    tstop  = *(rtt->getEndTime  (begin));

    cout << begin << " " << x << " " << y << endl;

    //  Make the Geometry Object with debug on...
    PHDchGeometryObject *geo = new PHDchGeometryObject(add,1);
    geo->xBeamOffset = x;
    geo->yBeamOffset = y;
    cout << geo->xBeamOffset << " " << geo->yBeamOffset << endl;
    geo->setCommittingFlag(6);
    geo->screenDump();

    geo->update(tstart,tstop,geometryNameDB,bankID,descrip);
    geo->commit();
    delete geo;
  }    

}
