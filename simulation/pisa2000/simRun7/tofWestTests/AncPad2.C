void AncPad2(const int iOption = 1,                   // Pair analysis for Lambda decay
             const int iPlot = 6,                     // Plot pair rapidity and transverse momentum summed for all momentum cuts
	     const float DecayRMAX = 30.,             // Cut on maximum R value at which Lambda decay can occur
             const bool pc1SameTag = true,            // Requirement that decay particle in PC3 must also be in PC1
             const bool pc1SisterTag = true,          // Requirement that partner ("sister") decay particle must also be in PC1
             const double validMassCut = 0.1,         // Cut on invariant mass with respect to PDG invariant mass
             const bool posField = true,              // Indicates that this is a postive field simulation
             const Long64_t MAXENTRIES = 0,           // Number of entries to be processed from input NTUPLE, 0 means all entries
             const float minMomentum = 0.2,           // minimum particle momentum allowed amont all the cuts
             const float maxMomentum = 9.0,           // maximum particle momentum allowed among all the cuts
             const bool ignoreTFW = false,            // ignore the presence of TOF-West, treat West Arm as having only EMCal
	     const int selectedEvent = 0,             // for debugging purposes, printing of information from selected event
	     const char *fname="pisaPAD2Hits.root") { // Default name for input NTUPLE file containing PC3 correlation information

  //
  // ROOT macro to analyze pisaRootRead NTUPLE for pair decay into PC3
  //
  // Author: C.F. Maguire
  // Creation date: February 19, 2007
  // Revised: February 20, 2007 including histograms
  // Obsolete after the introduction of the simulatePairs.C library 
  // This CINT macro takes too long to execute
  //

  const int posFieldEvents = 4657252;  // number of events in PISA hits files for ++ field simulation
  const float rScale = 0.1;

  /*
      Mutually exclusive plots (not including East-West, and AER-only treated as EMCal-only)
      iPlot = 1   East Arm TOF-TOF
      iPlot = 2   East Arm TOF-EMCal
      iPlot = 3   East Arm EMCal-EMCal (not in TOF-East)
      iPlot = 4   West Arm TOF/AER-TOF/AER
      iPlot = 5   West Arm TOF/AER-TOF
      iPlot = 6   West Arm TOF/AER-EMCal
      iPlot = 7   West Arm TOF-EMCal (not in AER)
      iPlot = 8   West Arm EMCal-EMCal (not in TOF-West)
      iPlot = 0   Summed after momentum cuts for both East and West Arms
      iPlot = 100 Summed after momentum cuts for East only
      iPlot = 200 Summed after momentum cuts for West only
  */

  //
  // Momentum cuts in GeV/c
  //
  float pionTFWandAERMin = 0.2;
  float protonTFWandAERMin = 0.2;
  float pionTFEMin = 0.2;
  float protonTFEMin = 0.2;
  float pionTFWonlyMin = 0.2;
  float protonTFWonlyMin = 0.2;
  float pionAERonlyMin = 0.2;
  float protonAERonlyMin = 0.2;
  float pionEMConlyMin = 0.2;
  float protonEMConlyMin = 0.2;

  float pionTFWandAERMax = 9.0;
  float protonTFWandAERMax = 9.0;
  float pionTFEMax = 9.0;
  float protonTFEMax = 3.0;
  float pionTFWonlyMax = 3.0;
  float protonTFWonlyMax = 5.0;
  float pionAERonlyMax = 1.5;
  float protonAERonlyMax = 2.5;
  float pionEMConlyMax = 1.5;
  float protonEMConlyMax = 2.5;

 //
  // Set up the ROOT canvas for drawing figures
  //
  TCanvas *c1 = 0;
  c1 = new TCanvas("c1", "TOF-West Correlation Plots", 200, 10, 700, 500);
  c1->SetFillColor(kWhite);
 
  //
  // Use ROOT's gStyle function to set up plot options
  //
  gStyle->SetOptStat(1110);
  gStyle->SetStatTextColor(kBlue);
  gStyle->SetFuncColor(kBlue);
  gStyle->SetFuncWidth(1.5);
  gStyle->SetOptFit(1111);
  gStyle->SetFitFormat("6.5g");

  //
  // Pick up input file called pisaTFWHits.root
  //

  TFile *f1 =  new TFile(fname);
  if (!f1) {
    cerr << "\n Unable to TFW NTUPLE file " << fname << endl;
    return;
  }

  TNtuple *AncPad2 = (TNtuple*)f1->Get("AncPad2");
  if(!AncPad2) {
    cerr << "\n Unable to find AncPad2 in PAD hits NTUPLE file " << fname << endl;
    return;
  }

  //
  // The following variable specifications are automatically produced by the MakeCode() NTUPLE function
  // The descriptive comments are added later
  //

//Declaration of leaves types
   Float_t         TRACK;
   Float_t         NFILE;
   Float_t         PTOT;
   Float_t         PTHETA;
   Float_t         PPHI;
   Float_t         R_VERTEX;
   Float_t         Z_VERTEX;
   Float_t         THET_VER;
   Float_t         PHI_VER;
   Float_t         ITPARENT;
   Float_t         IDPARENT;
   Float_t         IDPART;
   Float_t         IHIT;
   Float_t         NHIT;
   Float_t         IPC;
   Float_t         THETA;
   Float_t         PHI;
   Float_t         ZGLOBAL;
   Float_t         PTOT_PRI;
   Float_t         PTHE_PRI;
   Float_t         PPHI_PRI;
   Float_t         Z0ORIGIN;
   Float_t         ETRACK;
   Float_t         ZLOCAL1;
   Float_t         ZLOCAL2;
   Float_t         XGLOBAL;
   Float_t         XLOCAL1;
   Float_t         XLOCAL2;
   Float_t         YGLOBAL;
   Float_t         YLOCAL1;
   Float_t         YLOCAL2;
   Float_t         IPC123;
   Float_t         ITORIGIN;
   Float_t         IDORIGIN;
   Float_t         SECTOR;
   Float_t         DELTH13;
   Float_t         DELPH13;
   Float_t         REALMIN;
   Float_t         DELMIN;
   Float_t         IDMIN;
   Float_t         TORIGMIN;
   Float_t         TRACKMIN;
   Float_t         Z0PROJ;
   Float_t         DELTHETA;
   Float_t         TOFPC;
   Float_t         PATHPC;
   Float_t         BETAPC;
   Float_t         TOFSAME;    // Azimuthal angle in TOF-East or TOF-West for first particle
   Float_t         PC3SIST;
   Float_t         PC3STHET;
   Float_t         PC3SPHI;
   Float_t         PC3STOF;
   Float_t         PC3SPART;
   Float_t         PC3SPTOT;
   Float_t         PC3SPTHET;
   Float_t         PC3SPPHI;
   Float_t         TOFSTOF;
   Float_t         TOFSTHET;
   Float_t         TOFSPHI;
   Float_t         TOFSPART;
   Float_t         TOFSPTOT;
   Float_t         TOFSPTHET;
   Float_t         TOFSPPHI;
   Float_t         PC1SIST;
   Float_t         AERSAME;
   Float_t         AERSIST;
   Float_t         Z0_EVENT;
   Float_t         B_IMPACT;
   Float_t         EVENT;

   // Set branch addresses.
   AncPad2->SetBranchAddress("TRACK",&TRACK);
   AncPad2->SetBranchAddress("NFILE",&NFILE);
   AncPad2->SetBranchAddress("PTOT",&PTOT);
   AncPad2->SetBranchAddress("PTHETA",&PTHETA);
   AncPad2->SetBranchAddress("PPHI",&PPHI);
   AncPad2->SetBranchAddress("R_VERTEX",&R_VERTEX);
   AncPad2->SetBranchAddress("Z_VERTEX",&Z_VERTEX);
   AncPad2->SetBranchAddress("THET_VER",&THET_VER);
   AncPad2->SetBranchAddress("PHI_VER",&PHI_VER);
   AncPad2->SetBranchAddress("ITPARENT",&ITPARENT);
   AncPad2->SetBranchAddress("IDPARENT",&IDPARENT);
   AncPad2->SetBranchAddress("IDPART",&IDPART);
   AncPad2->SetBranchAddress("IHIT",&IHIT);
   AncPad2->SetBranchAddress("NHIT",&NHIT);
   AncPad2->SetBranchAddress("IPC",&IPC);
   AncPad2->SetBranchAddress("THETA",&THETA);
   AncPad2->SetBranchAddress("PHI",&PHI);
   AncPad2->SetBranchAddress("ZGLOBAL",&ZGLOBAL);
   AncPad2->SetBranchAddress("PTOT_PRI",&PTOT_PRI);
   AncPad2->SetBranchAddress("PTHE_PRI",&PTHE_PRI);
   AncPad2->SetBranchAddress("PPHI_PRI",&PPHI_PRI);
   AncPad2->SetBranchAddress("Z0ORIGIN",&Z0ORIGIN);
   AncPad2->SetBranchAddress("ETRACK",&ETRACK);
   AncPad2->SetBranchAddress("ZLOCAL1",&ZLOCAL1);
   AncPad2->SetBranchAddress("ZLOCAL2",&ZLOCAL2);
   AncPad2->SetBranchAddress("XGLOBAL",&XGLOBAL);
   AncPad2->SetBranchAddress("XLOCAL1",&XLOCAL1);
   AncPad2->SetBranchAddress("XLOCAL2",&XLOCAL2);
   AncPad2->SetBranchAddress("YGLOBAL",&YGLOBAL);
   AncPad2->SetBranchAddress("YLOCAL1",&YLOCAL1);
   AncPad2->SetBranchAddress("YLOCAL2",&YLOCAL2);
   AncPad2->SetBranchAddress("IPC123",&IPC123);
   AncPad2->SetBranchAddress("ITORIGIN",&ITORIGIN);
   AncPad2->SetBranchAddress("IDORIGIN",&IDORIGIN);
   AncPad2->SetBranchAddress("SECTOR",&SECTOR);
   AncPad2->SetBranchAddress("DELTH13",&DELTH13);
   AncPad2->SetBranchAddress("DELPH13",&DELPH13);
   AncPad2->SetBranchAddress("REALMIN",&REALMIN);
   AncPad2->SetBranchAddress("DELMIN",&DELMIN);
   AncPad2->SetBranchAddress("IDMIN",&IDMIN);
   AncPad2->SetBranchAddress("TORIGMIN",&TORIGMIN);
   AncPad2->SetBranchAddress("TRACKMIN",&TRACKMIN);
   AncPad2->SetBranchAddress("Z0PROJ",&Z0PROJ);
   AncPad2->SetBranchAddress("DELTHETA",&DELTHETA);
   AncPad2->SetBranchAddress("TOFPC",&TOFPC);
   AncPad2->SetBranchAddress("PATHPC",&PATHPC);
   AncPad2->SetBranchAddress("BETAPC",&BETAPC);
   AncPad2->SetBranchAddress("TOFSAME",&TOFSAME);
   AncPad2->SetBranchAddress("PC3SIST",&PC3SIST);
   AncPad2->SetBranchAddress("PC3STHET",&PC3STHET);
   AncPad2->SetBranchAddress("PC3SPHI",&PC3SPHI);
   AncPad2->SetBranchAddress("PC3STOF",&PC3STOF);
   AncPad2->SetBranchAddress("PC3SPART",&PC3SPART);
   AncPad2->SetBranchAddress("PC3SPTOT",&PC3SPTOT);
   AncPad2->SetBranchAddress("PC3SPTHET",&PC3SPTHET);
   AncPad2->SetBranchAddress("PC3SPPHI",&PC3SPPHI);
   AncPad2->SetBranchAddress("TOFSTOF",&TOFSTOF);
   AncPad2->SetBranchAddress("TOFSTHET",&TOFSTHET);
   AncPad2->SetBranchAddress("TOFSPHI",&TOFSPHI);
   AncPad2->SetBranchAddress("TOFSPART",&TOFSPART);
   AncPad2->SetBranchAddress("TOFSPTOT",&TOFSPTOT);
   AncPad2->SetBranchAddress("TOFSPTHET",&TOFSPTHET);
   AncPad2->SetBranchAddress("TOFSPPHI",&TOFSPPHI);
   AncPad2->SetBranchAddress("PC1SIST",&PC1SIST);
   AncPad2->SetBranchAddress("AERSAME",&AERSAME);
   AncPad2->SetBranchAddress("AERSIST",&AERSIST);
   AncPad2->SetBranchAddress("Z0_EVENT",&Z0_EVENT);
   AncPad2->SetBranchAddress("B_IMPACT",&B_IMPACT);
   AncPad2->SetBranchAddress("EVENT",&EVENT);

   //
   // Useful constants
   //
   const double DEGRAD = 57.295779513;                  // radians to degrees conversion
   const double LAMBDAMASS = 1.11560;                   // Lambda mass
   const double PROTONMASS = 0.93828;                   // proton mass
   const double PROTONMASSSQ = PROTONMASS*PROTONMASS;
   const double PIONMASS = 0.139567;                    // pion mass
   const double PIONMASSSQ = PIONMASS*PIONMASS;

   Long64_t nEntries = AncPad2->GetEntries();   // Long64_t is what ROOT uses internally
   cout << "\n Number of entries in file = " << nEntries << "; will use Decay RMAX = " << DecayRMAX << " cm" << endl;
   Long64_t readEntries = nEntries;
   if(MAXENTRIES>0 && MAXENTRIES<readEntries) {
     readEntries = MAXENTRIES;
     cout << "\n Will read first " << readEntries << " entries" << endl;
   }

     if(iOption == 1) {   // Lambda pair analysis

     //
     // Pair rapidity and transverse momentum histograms
     //

     TH1F *h1PairRapidityAccepted = new TH1F("h1PairRapidityAccepted", "Lambda Accepted Rapidity ", 60, -0.6, +0.6); 
     TH1F *h1PairPTranAccepted= new TH1F("h1PairPTranRAccepted", "Lambda Accepted Transverse Momentum", 50, 0.0, +20.0); 

     h1PairRapidityAccepted->SetFillColor(2);
     h1PairPTranAccepted->SetFillColor(2);

     //
     // Counting East and West arm hits before momentum cuts are checked
     //
     int kEastArm = 0;
     int kWestArm = 0;

     int kEastWestPair = 0;
     int kEastEastPair = 0;
     int kWestWestPair = 0;

     //
     // Counts of event types before momentum cuts
     //
     int kNoSister = 0;    // No sister particle
     int kEventType1 = 0;  // East Arm TOF-TOF
     int kEventType2 = 0;  // East Arm TOF-Emcal
     int kEventType3 = 0;  // East Arm EMCal-EMCal
     int kEventType4 = 0;  // West Arm TOF/AER-TOF/AER
     int kEventType5 = 0;  // West Arm TOF/AER-TOF
     int kEventType6 = 0;  // West Arm TOF/AER-EMCal
     int kEventType7 = 0;  // West Arm TOF-EMCal
     int kEventType8 = 0;  // West Arm EMCal-EMCal

     int countValidMass = 0;

     bool eastPlot = false;     // plot any East-East combination
     if(iPlot==0 || iPlot==100)
       eastPlot = true;

     bool westPlot = false;     // plot any West-West combination
     if(iPlot==0 || iPlot==200)
       westPlot = true;

     //
     // Sum over all entries (or fewer if requested by MAXENTRIES) in the NTUPLE file
     //
     Int_t nBytes = 0;
     for (Long64_t i=0; i<readEntries;i++) {
       nBytes += AncPad2->GetEntry(i);

       int iEvent = EVENT;
       if(iEvent == selectedEvent) {
	 cout << "\n Selected event " << selectedEvent << " data: PTOT = " << PTOT << ", IDPARENT " << IDPARENT;
	 cout << "\n IDPART " << IDPART << ",  PC3SPART " << PC3SPART;
	 cout << "\n IPC123 " << IPC123 << ",  PC1SIST " << PC1SIST;
	 cout << " R_VERTEX " << R_VERTEX;
	 cout << endl;
       }

       int idParent = IDPARENT;
       if(idParent != 18)
	 continue; // check on Lambda parent

       int idPart = IDPART;
       if(idPart!=9 && idPart!=14)
	 continue;

       int iPC123 = IPC123;   // bit pattern for PC hits, eg. 110 means PC3 and PC2 but not PC1
       if(pc1SameTag && iPC123 < 101)
         continue;  // skip if PC1 required and bit pattern shows PC1 was not hit by this track

       if(pc1SisterTag && PC1SIST < 0.0)
         continue;  // skip if PC1 sister required and PC1SIST (sector number) is negative default

       if(R_VERTEX > DecayRMAX)
         continue;

       if(PTOT<minMomentum || PTOT>maxMomentum)
	 continue;

       bool eastArm1 = false;
       bool westArm1 = false;

       if(PHI > -35.0 && PHI < +65.0){
         westArm1 = true;
         kWestArm++;
       }

      if(PHI > +115.0 && PHI < +215.0) {
         eastArm1 = true;
         kEastArm++;
      }

      if(PC3SIST<0){
	kNoSister++;
	continue;
      }

      if(PC3SPTOT<minMomentum || PC3SPTOT>maxMomentum)
	continue;

      int idSist = PC3SPART;
      if(idSist + idPart != 23)
	continue;

      bool eastArm2 = false;
      bool westArm2 = false;

      if(PC3SPHI>-90.0 && PC3SPHI<+90.0)
	westArm2 = true;

      if(PC3SPHI>+90.0 && PC3SPHI<+270.0)
	eastArm2 = true;
 
      //
      // Invariant mass calculation for the Lambda
      //

      bool validMass = false;
      double vertexMass = 0.0;
      double vertexPairRapidity = -999.0;
      double vertexPairPTran = -999.0;
      float protonMomentum = 0.0;
      float pionMomentum = 0.0;

      double px1 = PTOT*sin(PTHETA/DEGRAD)*cos(PPHI/DEGRAD);
      double py1 = PTOT*sin(PTHETA/DEGRAD)*sin(PPHI/DEGRAD);
      double pz1 = PTOT*cos(PTHETA/DEGRAD);
      double et1 = 0.0;
      if(idPart == 14) {
	et1 = sqrt(PTOT*PTOT + PROTONMASSSQ);
	protonMomentum = PTOT;
      }
      if(idPart == 9) {
	et1 = sqrt(PTOT*PTOT + PIONMASSSQ);
	pionMomentum = PTOT;
      }

      double px2 = PC3SPTOT*sin(PC3SPTHET/DEGRAD)*cos(PC3SPPHI/DEGRAD);
      double py2 = PC3SPTOT*sin(PC3SPTHET/DEGRAD)*sin(PC3SPPHI/DEGRAD);
      double pz2 = PC3SPTOT*cos(PC3SPTHET/DEGRAD);
      double et2 = 0.0;
      if(idSist == 14) {
	et2 = sqrt(PC3SPTOT*PC3SPTOT + PROTONMASSSQ);
	protonMomentum = PC3SPTOT;
      }
      if(idSist == 9) {
	et2 = sqrt(PC3SPTOT*PC3SPTOT + PIONMASSSQ);
	pionMomentum = PC3SPTOT;
      }
      if(et1 > 0.0 && et2 > 0.0) {
	double testMass = (et1+et2)*(et1+et2) - (px1+px2)*(px1+px2) -
	  (py1+py2)*(py1+py2) - (pz1+pz2)*(pz1+pz2);
	if(testMass > 0.0) {
	  testMass = sqrt(testMass);
	  if(fabs(testMass - LAMBDAMASS)<validMassCut) {
	    validMass = true;
	    vertexMass = testMass;
	    vertexPairRapidity = 0.5*log((et1+et2 + pz1+pz2)/(et1+et2 - (pz1+pz2)));
	    vertexPairPTran = sqrt((px1+px2)*(px1+px2) + (py1+py2)*(py1+py2));
	    countValidMass++;
	  } // check on mass cut
	} // check on test mass > 0
      } // check on et1 and et2 being greater than 0

       if(iEvent == selectedEvent) {
	 cout << "\n Selected event " << selectedEvent << " data: vertexMass = " << vertexMass << endl; 
	 return;
       }

      if(validMass) {

	if((eastArm1 && westArm2) || (eastArm2 && westArm1)) {
	  kEastWestPair++;
	  continue;
	} // skip East-West coincidences for now

	bool eventType1 = false;
	bool eventType2 = false;
	bool eventType3 = false;
	bool eventType4 = false;
	bool eventType5 = false;
	bool eventType6 = false;
	bool eventType7 = false;
	bool eventType8 = false;

	if((iPlot<4 || eastPlot) && eastArm1 && eastArm2) { // East Arm pair
	  kEastEastPair++;

	  if(TOFSAME>90.0 && TOFSPTOT>0.0) { // TOFSAME (azimuthal angle) > 167.25 for TOF-East

	    if(!eastPlot && iPlot != 1)
	      continue; // skip remaining checks

	    eventType1 = true;
	    kEventType1++; // this will double count, should be an even number
	    	  
	    if(idPart==9) { // avoid double counting in TOF-TOF
	      if(pionMomentum>pionTFEMin && pionMomentum<pionTFEMax &&
		 protonMomentum>protonTFEMin && protonMomentum<protonTFEMax) {
		h1PairRapidityAccepted->Fill(vertexPairRapidity);
		h1PairPTranAccepted->Fill(vertexPairPTran);
	      }
	    } // avoid double counting in plot

	    continue ; // skip remaining checks

	  } // TOF-TOF, East Arm

	  if(!eventType1 && (TOFSAME>90.0 || TOFSPTOT>0.0)) { // TOFSPTOT, sister particle momentum in TOF

	    if(!eastPlot && iPlot != 2)
	      continue; // skip remaining checks

	    eventType2 = true;
	    kEventType2++;
	    bool momentaOK = false;
	    if(TOFSAME>0){ // first particle is in TOF, second particle is in EMCal

	      if(idPart==9) { // pion in TOF, proton in EMC
		if(pionMomentum>pionTFEMin && pionMomentum<pionTFEMax &&
		   protonMomentum>protonEMConlyMin && protonMomentum<protonEMConlyMax) {
		  momentaOK = true;
		} // check momenta
	      }
	      else { // proton in TOF, pion in EMC
		if(pionMomentum>pionEMConlyMin && pionMomentum<pionEMConlyMax &&
		   protonMomentum>protonTFEMin && protonMomentum<protonTFEMax) {
		  momentaOK = true;
		} // check momenta
	      } // check on particle ID in TOF
	    }
	    else { // second particle is in TOF, first particle is in EMCal
	      if(idSist==9) { // pion in TOF, proton in EMC
		if(pionMomentum>pionTFEMin && pionMomentum<pionTFEMax &&
		   protonMomentum>protonEMConlyMin && protonMomentum<protonEMConlyMax) {
		  momentaOK = true;
		} // check momenta
	      }
	      else { // proton in TOF, pion in EMC
		if(pionMomentum>pionEMConlyMin && pionMomentum<pionEMConlyMax &&
		   protonMomentum>protonTFEMin && protonMomentum<protonTFEMax) {
		  momentaOK = true;
		} // check momenta
	      } // check on particle ID in TOF
	    } // check on TOFSAME in East Arm

	    if(momentaOK) {
	      h1PairRapidityAccepted->Fill(vertexPairRapidity);
	      h1PairPTranAccepted->Fill(vertexPairPTran);
	    } // check that momenta are within range

	    continue; // skip remaining checks

	  } // TOF-EMCal

	  if(!eventType1 && !eventType2) {

	    if(!eastPlot && iPlot != 3)
	      continue; // skip remaining checks

	    eventType3 = true;
	    kEventType3++; // this will double count, should be an even number
	    	  
	    if(idPart==9) { // avoid double counting in EMCal-EMCal
	      if(pionMomentum>pionEMConlyMin && pionMomentum<pionEMConlyMax &&
		 protonMomentum>protonEMConlyMin && protonMomentum<protonEMConlyMax) {
		h1PairRapidityAccepted->Fill(vertexPairRapidity);
		h1PairPTranAccepted->Fill(vertexPairPTran);
	      }
	    } // avoid double counting in plot

	    continue; // skip remaining checks

	  } // EMCal-EMCal
	  
	} // East Arm pair

	if((iPlot>3 || westPlot) && westArm1 && westArm2) { // West Arm pair
	  kWestWestPair++;

	  if(TOFSAME>-90.0 && AERSAME>0.0 && TOFSPHI>-90.0 && AERSIST>-90.0) {

	    if(!westPlot && iPlot != 4)
	      continue; // skip remaining checks

	    eventType4 = true;
	    kEventType4++; // this will double count, should be an even number

	    if(idPart==9) { // avoid double counting in TOF/AER-TOF/AER
	      if(pionMomentum>pionTFWandAERMin && pionMomentum<pionTFWandAERMax &&
		 protonMomentum>protonTFWandAERMin && protonMomentum<protonTFWandAERMax) {
		h1PairRapidityAccepted->Fill(vertexPairRapidity);
		h1PairPTranAccepted->Fill(vertexPairPTran);
	      }
	    } // avoid double counting in plot

	    continue;

	  } // TOF/AER-TOF/AER

	  //
	  // Event type 5, TOF/AER-TOF (both particles in TOF-West, only one in AER)
	  //
	  if((TOFSAME>-90.0 && AERSAME>0.0 && TOFSPHI>-90.0 && AERSIST<-90.0) ||
	     (TOFSAME>-90.0 && AERSAME<0.0 && TOFSPHI>-90.0 && AERSIST>-90.0)) {

	    if(!westPlot && iPlot != 5)
	      continue; // skip remaining checks

	    eventType5 = true;
	    kEventType5++;
	    bool momentaOK = false;

	    if(AERSAME>0.0) { // first particle has TFW/AER, sister particle has only TFW
	      if(idPart==9) { // pion in TFW/AER, proton in TFW-Only
		if(pionMomentum>pionTFWandAERMin && pionMomentum<pionTFWandAERMax && 
		   protonMomentum>protonTFWonlyMin && protonMomentum<protonTFWonlyMax) {
		  momentaOK = true;
		} // check momenta
	      }
	      else { // proton in TFW/AER, pion in TFW-only
		if(pionMomentum>pionTFWonlyMin && pionMomentum<pionTFWonlyMax &&
		   protonMomentum>protonTFWandAERMin && protonMomentum<protonTFWandAERMax) {
		  momentaOK = true;
		} // check momenta
	      } // check on particle ID in TFW/AER
	    }
	    else { // sister particle has TOF/AER, first particle has only TOF
	      if(idSist==9) { // pion in TFW/AER, proton in TFW-only
		if(pionMomentum>pionTFWandAERMin && pionMomentum<pionTFWandAERMax &&
		   protonMomentum>protonTFWonlyMin && protonMomentum<protonTFWonlyMax) {
		  momentaOK = true;
		} // check momenta
	      }
	      else { // proton in TFW/AER, pion in TFW-only
		if(pionMomentum>pionTFWonlyMin && pionMomentum<pionTFWonlyMax &&
		   protonMomentum>protonTFWandAERMin && protonMomentum<protonTFWandAERMax) {
		  momentaOK = true;
		} // check momenta
	      } // check on particle ID in TOF
	    } // check if same (first) or sister particle is in Aerogel

	    if(momentaOK) {
	      h1PairRapidityAccepted->Fill(vertexPairRapidity);
	      h1PairPTranAccepted->Fill(vertexPairPTran);
	    } // check that momenta are within range

	    continue; // skip remaining checks

	  } // TOF/AER-TOF/NoAER

	  //
	  // Event type 6, TOF/AER-EMCal (one particle in TOF/AER, other particle in EMCal only
	  //
	  if((TOFSAME>-90.0 && AERSAME>0.0 && TOFSPHI<-90.0) ||
	     (TOFSAME<-90.0 && TOFSPHI>-90.0 && AERSIST>-90.0)) {

	    if(!westPlot && iPlot != 6)
	      continue; // skip remaining checks

	    eventType6 = true;
	    kEventType6++;
	    bool momentaOK = false;

	    if(TOFSAME>-90.0) { // first particle has TFW/AER, sister particle has only EMCal only
	      if(idPart==9) { // pion in TFW/AER, proton in TFW-Only
		if(pionMomentum>pionTFWandAERMin && pionMomentum<pionTFWandAERMax && 
		   protonMomentum>protonEMConlyMin && protonMomentum<protonEMConlyMax) {
		  momentaOK = true;
		} // check momenta
	      }
	      else { // proton in TFW/AER, pion in EMCal-only
		if(pionMomentum>pionEMConlyMin && pionMomentum<pionEMConlyMax &&
		   protonMomentum>protonTFWandAERMin && protonMomentum<protonTFWandAERMax) {
		  momentaOK = true;
		} // check momenta
	      } // check on particle ID in TFW/AER
	    }
	    else { // sister particle has TOF/AER, first particle has only EMCal
	      if(idSist==9) { // pion in TFW/AER, proton in EMCal-only
		if(pionMomentum>pionTFWandAERMin && pionMomentum<pionTFWandAERMax &&
		   protonMomentum>protonEMConlyMin && protonMomentum<protonEMConlyMax) {
		  momentaOK = true;
		} // check momenta
	      }
	      else { // proton in TFW/AER, pion in EMCal-only
		if(pionMomentum>pionEMConlyMin && pionMomentum<pionEMConlyMax &&
		   protonMomentum>protonTFWandAERMin && protonMomentum<protonTFWandAERMax) {
		  momentaOK = true;
		} // check momenta
	      } // check on particle ID in TOF-West
	    } // check if same (first) or sister particle is in TOF-West

	    if(momentaOK) {
	      h1PairRapidityAccepted->Fill(vertexPairRapidity);
	      h1PairPTranAccepted->Fill(vertexPairPTran);
	    } // check that momenta are within range

	    continue; // skip remaining checks

	  } // event type 6 TOF/AER-EMCal

	  //
	  // Event type 7, TOF-EMCal (one particle in TOF, other particle in EMCal, no AER)
	  //
	  if((TOFSAME>-90.0 && AERSAME<0.0 && TOFSPHI<-90.0) ||
	     (TOFSAME<-90.0 && TOFSPHI>-90.0 && AERSIST<-90.0)) {

	    if(!westPlot && iPlot != 7)
	      continue; // skip remaining checks

	    eventType7 = true;
	    kEventType7++;
	    bool momentaOK = false;

	    if(TOFSAME>-90.0) { // first particle has TFW, sister particle has only EMCal only
	      if(idPart==9) { // pion in TFW, proton in EMCal-Only
		if(pionMomentum>pionTFWonlyMin && pionMomentum<pionTFWonlyMax && 
		   protonMomentum>protonEMConlyMin && protonMomentum<protonEMConlyMax) {
		  momentaOK = true;
		} // check momenta
	      }
	      else { // proton in TFW, pion in EMCal-only
		if(pionMomentum>pionEMConlyMin && pionMomentum<pionEMConlyMax &&
		   protonMomentum>protonTFWonlyMin && protonMomentum<protonTFWonlyMax) {
		  momentaOK = true;
		} // check momenta
	      } // check on particle ID in TFW
	    }
	    else { // sister particle has TOF, first particle has only EMCal
	      if(idSist==9) { // pion in TFW, proton in EMCal-only
		if(pionMomentum>pionTFWonlyMin && pionMomentum<pionTFWonlyMax &&
		   protonMomentum>protonEMConlyMin && protonMomentum<protonEMConlyMax) {
		  momentaOK = true;
		} // check momenta
	      }
	      else { // proton in TFW, pion in EMCal-only
		if(pionMomentum>pionEMConlyMin && pionMomentum<pionEMConlyMax &&
		   protonMomentum>protonTFWonlyMin && protonMomentum<protonTFWonlyMax) {
		  momentaOK = true;
		} // check momenta
	      } // check on particle ID in TOF-West
	    } // check if same (first) or sister particle is in TOF-West

	    if(momentaOK) {
	      h1PairRapidityAccepted->Fill(vertexPairRapidity);
	      h1PairPTranAccepted->Fill(vertexPairPTran);
	    } // check that momenta are within range

	    continue; // skip remaining checks

	  } // event type 7 TOF-EMCal (no TFW/AER)

	  //
	  // Event type 8, TOF-EMCal (both particles in EMCal, neither in TOF-West)
	  //
	  //
	  // Because of the continue statements, the check on eventType should not be necessary?
	  //
	  if(!eventType4 && !eventType5 && !eventType6 && !eventType7) {

	    if(!westPlot && iPlot != 8)
	      continue; // skip remaining checks

	    eventType8 = true;
	    kEventType8++; // this will double count, should be an even number
	    	  
	    if(idPart==9) { // avoid double counting in EMCal-EMCal
	      if(pionMomentum>pionEMConlyMin && pionMomentum<pionEMConlyMax &&
		 protonMomentum>protonEMConlyMin && protonMomentum<protonEMConlyMax) {
		h1PairRapidityAccepted->Fill(vertexPairRapidity);
		h1PairPTranAccepted->Fill(vertexPairPTran);
	      }
	    } // avoid double counting in plot

	    continue; // skip remaining checks

	  } // West Arm EMCal-EMCal

	} // West Arm pair

      } // check on valid mass

     } // loop over all entries

     cout << "\n Single particle yields in PC3: kEastArm = " << kEastArm << ",  kWestArm = " << kWestArm;
     cout << "\n kEastEastPair = " << kEastEastPair;
     cout << ",  kWestWestPair = " << kWestWestPair;
     cout << ",  kEastWestPair = " << kEastWestPair;
     cout <<  endl;

     cout << "\n Number of valid mass pairs = " << countValidMass << ", with following event types (before momentum cuts):";
     cout << "\n 1) East Arm TOF-TOF = " << kEventType1;
     cout << "\n 2) East Arm TOF-EMCal = " << kEventType2;
     cout << "\n 3) East Arm EMCal-EMCal = " << kEventType3;
     cout << "\n 4) West Arm TOF/AER-TOF/AER = " << kEventType4;
     cout << "\n 5) West Arm TOF/AER-TOF = " << kEventType5;
     cout << "\n 6) West Arm TOF/AER-EMCal = " << kEventType6;
     cout << "\n 7) West Arm TOF-EMCal = " << kEventType7;
     cout << "\n 8) West Arm EMCal-EMCal = " << kEventType8;
     cout << "\n\n   After momentum cuts summed yield = " << h1PairRapidityAccepted->GetSum();
     cout << endl;

     c1->Divide(1,2);
     c1->cd(1);
     if(posField)
       h1PairRapidityAccepted->SetMaximum(rScale*600);
     else
       h1PairRapidityAccepted->SetMaximum(rScale*600);
     h1PairRapidityAccepted->Draw();
     TLatex *tex0 = new TLatex(-0.55, rScale*550.0, "Simulation of #Lambda #rightarrow p + #pi^{-} Accepted in PHENIX");
     tex0->SetTextColor(4);
     tex0->SetTextSize(0.06);
     tex0->Draw();

     TLatex *tex1;
     if(iPlot==0)
       tex1 = new TLatex(-0.55, rScale*500.0, "All PHENIX detectors");       // change according to iPlot choice

     if(iPlot==1)
       tex1 = new TLatex(-0.55, rScale*500.0, "East Arm TOF-TOF only");      // change according to iPlot choice

     if(iPlot==2)
       tex1 = new TLatex(-0.55, rScale*500.0, "East Arm TOF-EMCal only");      // change according to iPlot choice

     if(iPlot==3)
       tex1 = new TLatex(-0.55, rScale*500.0, "East Arm EMCal-EMCal only");      // change according to iPlot choice

     if(iPlot==4)
       tex1 = new TLatex(-0.55, rScale*500.0, "West Arm TOF/Aer-TOF/Aer only");    // change according to iPlot choice

     if(iPlot==5)
       tex1 = new TLatex(-0.55, rScale*500.0, "West Arm TOF/Aer-TOF/NoAER only");   // change according to iPlot choice

     if(iPlot==6)
       tex1 = new TLatex(-0.55, rScale*500.0, "West Arm TOF/Aer-EMCal only");   // change according to iPlot choice

     if(iPlot==7)
       tex1 = new TLatex(-0.55, rScale*500.0, "West Arm TOF-EMCal only (no particle in AER)");   // change according to iPlot choice

     if(iPlot==8)
       tex1 = new TLatex(-0.55, rScale*500.0, "West Arm EMCal-EMCal only (no particle in TFW)");   // change according to iPlot choice

     if(iPlot==100)
       tex1 = new TLatex(-0.55, rScale*500.0, "East Arm PHENIX detectors");  // change according to iPlot choice

     if(iPlot==200)
       tex1 = new TLatex(-0.55, rScale*500.0, "West Arm PHENIX detectors");  // change according to iPlot choice

     tex1->SetTextColor(4);
     tex1->SetTextSize(0.05);
     tex1->Draw();

     TLatex *tex2;
     if(posField)
       tex2 = new TLatex(-0.55, rScale*450.0, "4.7M #Lambda with uniform distributions y = #pm 0.6, 0<p_{T}<20 GeV/c, into ++ magnetic field");
     else
       tex2 = new TLatex(-0.55, rScale*450.0, "5.0M #Lambda with uniform distributions y = #pm 0.6, 0<p_{T}<20 GeV/c, into -- magnetic field");
     tex2->SetTextColor(4);
     tex2->SetTextSize(0.05);
     tex2->Draw();

     TLatex *tex3 = new TLatex(-0.55, rScale*400.0, "February 20, 2007");
     tex3->SetTextColor(4);
     tex3->SetTextSize(0.05);
     tex3->Draw();

     c1->cd(2);
     if(posField)
       h1PairPTranAccepted->SetMaximum(rScale*800);
     else
       h1PairPTranAccepted->SetMaximum(rScale*800);
     h1PairPTranAccepted->Draw();


   } // iOption = 1, Lambda decay

}
