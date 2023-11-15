void AncTfw2(const int iOption = 1,                   // Pair analysis for Lambda decay
             const int iPlot = 0,                     // Plot pair rapidity and transverse momentum summed for all momentum cuts
	     const float DecayRMAX = 30.,             // Cut on maximum R value at which Lambda decay can occur
             const bool pc1SameTag = true,            // Requirement that decay particle in TOF-West must also be in PC1
             const bool pc1SisterTag = true,          // Requirement that partner ("sister") decay particle must also be in PC1
             const double validMassCut = 0.1,         // Cut on invariant mass with respect to PDG invariant mass
             const bool posField = true,              // Indicates that this is a postive field simulation
             const Long64_t MAXENTRIES = 0,           // Number of entries to be processed from input NTUPLE, 0 means all entries
             const float minMomentum = 0.2,           // minimum particle momentum allowed amont all the cuts
             const float maxMomentum = 9.0,           // maximum particle momentum allowed among all the cuts
	     const char *fname="pisaTFW2Hits.root") { // Default name for input NTUPLE file containing TOF-West correlation information
  //
  // ROOT macro to analyze pisaRootRead NTUPLE for pair decay into TOF-West
  //
  // Author: C.F. Maguire
  // Creation date: February 19, 2007
  //

  //
  // Mutually exclusive plot types (sister particle is always at least in EMCal/PC3)
  //
  /*
 iPlot = 1  Both decay particles in TOF-West and in Aerogel, both particles in EMC/PC3
 iPlot = 2  Both decay particles in TOF-West, neither particle in Aerogel, both particles in EMC/PC3
 iPlot = 3  Both decay particles in TOF-West, only one particle in Aerogel and other particle in EMC/PC3
 iPlot = 4  Only one particle in TOF-West, both particles in Aerogel and EMC/PC3
 iPlot = 5  Only one particle in TOF-West, neither particle in Aerogel, sister particle only in EMC/PC3
 iPlot = 6  Only one particle in TOF-West and in Aerogel, sister particle only in EMC/PC3
 iPlot = 7  Only one particle in TOF-West and is not in Aerogel, sister particle only in Aerogel and EMC/PC3
 iPlot = 8  Only one particle in TOF-West and Aerogel with sister particle is in TOF-East
 iPlot = 9  Only one particle in TOF-West but not Aerogel with sister particle is in TOF-East
 iPlot = 0  Plots summed yield after all momentum cuts
  */

  const int posFieldEvents = 4657252;  // number of events in PISA hits files for ++ field simulation
  const float rScale = 1.0;
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
  float protonTFEMax = 9.0;
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

  TNtuple *AncTfw2 = (TNtuple*)f1->Get("AncTfw2");
  if(!AncTfw2) {
    cerr << "\n Unable to find AncTfw2 in TFW NTUPLE file " << fname << endl;
    return;
  }

  //
  // The following variable specifications are automatically produced by the MakeCode() NTUPLE function
  // The descriptive comments are added later
  //

  //Declaration of leaves types
  Float_t         TRACK;               // track number 
  Float_t         NFILE;               // file number (typically 0)  
  Float_t         PTOT;                // vertex momentum magnitude of the particle detected in TOF-West (GeV/c)
  Float_t         PTHETA;              // vertex polar angle direction of the particle detected in TOF-West (degrees)
  Float_t         PPHI;                // vertex azimuthal angle direction of the particle detected in TOF-West
  Float_t         R_VERTEX;            // radial distance from collision point for the vertex of the particle detected in TOF-West
  Float_t         Z_VERTEX;            // Z coordinate for the vertex of the particle detected in TOF-West
  Float_t         THET_VER;            // theta coordinate for the vertex of the particle detected in TOF-West
  Float_t         PHI_VER;             // azimuthal coordinate for the vertex of the particle detected in TOF-West
  Float_t         ITPARENT;            // parent track number of the particle detected in TOF-West
  Float_t         IDPARENT;            // GEANT particle ID number for the parent of the particle detected in TOF-West
  Float_t         IDPART;              // GEANT particle ID number of the particle detected in TOF-West
  Float_t         IHIT;                // first hit number of the particle detected in TOF-West
  Float_t         NHIT;                // total number of hits in TOF-West
  Float_t         THETA;               // theta coordinate for the first hit of the particle detected in TOF-West 
  Float_t         PHI;                 // azimuthal coordinate for the first hit of the particle detected in TOF-West
  Float_t         DELE;                // summed energy loss for all the hits of the particle detected in TOF-West 
  Float_t         PTOT_PRI;            // momentum magnitude of the primary particle ancestor of the particle detected in TOF-West 
  Float_t         PTHE_PRI;            // theta momentum direction of the primary particle ancestor of the particle detected in TOF-West
  Float_t         PPHI_PRI;            // azimuthal momentum direction of the primary particle ancestor of the particle detected in TOF-West
  Float_t         Z0ORIGIN;            // Z origin of the primary particle ancestor of the particle detected in TOF-West
  Float_t         ETRACK;              // event input track number of the primary particle ancestor of the particle detected in TOF-West (usually 1)
  Float_t         XGLOBAL;             // entrance x coordinate in the master frame of the particle detected in TOF-West
  Float_t         XLOCAL1;             // entrance x coordinate in the local frame of the particle detected in TOF-West
  Float_t         XLOCAL2;             // exit x coordinate in the local frame of the particle detected in TOF-West
  Float_t         YGLOBAL;             // entrance y coordinate in the master frame of the particle detected in TOF-West
  Float_t         YLOCAL1;             // entrance y coordinate in the local frame of the particle detected in TOF-West
  Float_t         YLOCAL2;             // exit y coordinate in the local frame of the particle detected in TOF-West
  Float_t         ZGLOBAL;             // entrance z coordinate in the master frame of the particle detected in TOF-West
  Float_t         XZLOCAL1;            // entrance z coordinate in the local frame of the particle detected in TOF-West [TYPO ERROR IN NAME1]
  Float_t         ZLOCAL2;             // exit z coordinate in the local frame of the particle detected in TOF-West
  Float_t         ITORIGIN;            // track number of the primary particle ancestor of the particle detected in TOF-West
  Float_t         IDORIGIN;            // GEANT particle ID number of the primary particle ancestor of the particle detected in TOF-West
  Float_t         PANEL;               // panel number of the first hit of the particle detected in TOF-West
  Float_t         TOFTFW;              // average TOF for all the hits of the particle detected in TOF-West
  Float_t         PATHTFW;             // path length of the first hit of the particle detected in TOF-West
  Float_t         BETATFW;             // path length of the first hit of the particle detected in TOF-West
  Float_t         NTFWHIT;             // number of hits in TOF-West for this particle detected in TOF-West
  Float_t         PC2SAME;             // sector number in PC2 if this TOF-West particle is detected in PC2, -999 if not detected
  Float_t         AERSAME;             // momentum magnitude in Aerogel if this TOF-West particle is detected in Aerogel, -999 if not detected
  Float_t         PC3SAME;             // sector number in PC3 if this TOF-West particle is detected in PC3, -999 if not detected
  Float_t         PC3SIST;             // sector number in PC3 of the sister particle of this TOF-West particle if detected in PC3, -999 if not detected
  Float_t         PC3STHET;            // theta angle of the sister particle of this TOF-West particle if detected in PC3, -999 if not detected
  Float_t         PC3SPHI;             // azimuthal angle of the sister particle of this TOF-West particle if detected in PC3, -999 if not detected
  Float_t         PC3STOF;             // tof to PC3 of the sister particle of this TOF-West particle if detected in PC3, -999 if not detected
  Float_t         PC3SPART;            // GEANT particle ID of the sister particle of this TOF-West particle if detected in PC3, -999 if not detected
  Float_t         PC3SPTOT;            // momentum magnitude of the sister particle of this TOF-West particle if detected in PC3, -999 if not detected
  Float_t         PC3SPTHET;           // momentum theta direction of the sister particle of this TOF-West particle if detected in PC3, -999 if not detected
  Float_t         PC3SPPHI;            // momentum azimuthal direction of the sister particle of this TOF-West particle if detected in PC3, -999 if not detected
  Float_t         TOFESTOF;            // same set of parameters if sister particle is detected in TOF-East
  Float_t         TOFESTHET;
  Float_t         TOFESPHI;
  Float_t         TOFSPART;
  Float_t         TOFSPTOT;
  Float_t         TOFSPTHET;
  Float_t         TOFSPPHI;
  Float_t         PC1SAME;             // sector number in PC1 of this TOF-West particle if detected in PC1, -999 if not detected
  Float_t         PC1SIST;             // sector number in PC1 of the sister of this TOF-West particle if sister detected in PC1, -999 if not detected
  Float_t         TOFWSPHI;            // azimuthal angle in TOF-West of the sister particle of this TOF-West particle if sister detected in TOF-West, -999 if not detected
  Float_t         AERSPHI;             // azimuthal angle in Aerogel of the sister particle of this TOF-West particle if sister detected in Aerogel, -999 if not detected
  Float_t         Z0_EVENT;            // Z vertex of the whole event (same as Z0ORIGIN for single particle simulations)
  Float_t         B_IMPACT;            // impact parametr of the event (not relevant for single particle simulations)
  Float_t         EVENT;               // event number in this PISA run

  //
  // The following lines of code are automatically generated by the MakeCode() NTUPLE function
  //

   // Set branch addresses.
   AncTfw2->SetBranchAddress("TRACK",&TRACK);
   AncTfw2->SetBranchAddress("NFILE",&NFILE);
   AncTfw2->SetBranchAddress("PTOT",&PTOT);
   AncTfw2->SetBranchAddress("PTHETA",&PTHETA);
   AncTfw2->SetBranchAddress("PPHI",&PPHI);
   AncTfw2->SetBranchAddress("R_VERTEX",&R_VERTEX);
   AncTfw2->SetBranchAddress("Z_VERTEX",&Z_VERTEX);
   AncTfw2->SetBranchAddress("THET_VER",&THET_VER);
   AncTfw2->SetBranchAddress("PHI_VER",&PHI_VER);
   AncTfw2->SetBranchAddress("ITPARENT",&ITPARENT);
   AncTfw2->SetBranchAddress("IDPARENT",&IDPARENT);
   AncTfw2->SetBranchAddress("IDPART",&IDPART);
   AncTfw2->SetBranchAddress("IHIT",&IHIT);
   AncTfw2->SetBranchAddress("NHIT",&NHIT);
   AncTfw2->SetBranchAddress("THETA",&THETA);
   AncTfw2->SetBranchAddress("PHI",&PHI);
   AncTfw2->SetBranchAddress("DELE",&DELE);
   AncTfw2->SetBranchAddress("PTOT_PRI",&PTOT_PRI);
   AncTfw2->SetBranchAddress("PTHE_PRI",&PTHE_PRI);
   AncTfw2->SetBranchAddress("PPHI_PRI",&PPHI_PRI);
   AncTfw2->SetBranchAddress("Z0ORIGIN",&Z0ORIGIN);
   AncTfw2->SetBranchAddress("ETRACK",&ETRACK);
   AncTfw2->SetBranchAddress("XGLOBAL",&XGLOBAL);
   AncTfw2->SetBranchAddress("XLOCAL1",&XLOCAL1);
   AncTfw2->SetBranchAddress("XLOCAL2",&XLOCAL2);
   AncTfw2->SetBranchAddress("YGLOBAL",&YGLOBAL);
   AncTfw2->SetBranchAddress("YLOCAL1",&YLOCAL1);
   AncTfw2->SetBranchAddress("YLOCAL2",&YLOCAL2);
   AncTfw2->SetBranchAddress("ZGLOBAL",&ZGLOBAL);
   AncTfw2->SetBranchAddress("XZLOCAL1",&XZLOCAL1);
   AncTfw2->SetBranchAddress("ZLOCAL2",&ZLOCAL2);
   AncTfw2->SetBranchAddress("ITORIGIN",&ITORIGIN);
   AncTfw2->SetBranchAddress("IDORIGIN",&IDORIGIN);
   AncTfw2->SetBranchAddress("PANEL",&PANEL);
   AncTfw2->SetBranchAddress("TOFTFW",&TOFTFW);
   AncTfw2->SetBranchAddress("PATHTFW",&PATHTFW);
   AncTfw2->SetBranchAddress("BETATFW",&BETATFW);
   AncTfw2->SetBranchAddress("NTFWHIT",&NTFWHIT);
   AncTfw2->SetBranchAddress("PC2SAME",&PC2SAME);
   AncTfw2->SetBranchAddress("AERSAME",&AERSAME);
   AncTfw2->SetBranchAddress("PC3SAME",&PC3SAME);
   AncTfw2->SetBranchAddress("PC3SIST",&PC3SIST);
   AncTfw2->SetBranchAddress("PC3STHET",&PC3STHET);
   AncTfw2->SetBranchAddress("PC3SPHI",&PC3SPHI);
   AncTfw2->SetBranchAddress("PC3STOF",&PC3STOF);
   AncTfw2->SetBranchAddress("PC3SPART",&PC3SPART);
   AncTfw2->SetBranchAddress("PC3SPTOT",&PC3SPTOT);
   AncTfw2->SetBranchAddress("PC3SPTHET",&PC3SPTHET);
   AncTfw2->SetBranchAddress("PC3SPPHI",&PC3SPPHI);
   AncTfw2->SetBranchAddress("TOFESTOF",&TOFESTOF);
   AncTfw2->SetBranchAddress("TOFESTHET",&TOFESTHET);
   AncTfw2->SetBranchAddress("TOFESPHI",&TOFESPHI);
   AncTfw2->SetBranchAddress("TOFSPART",&TOFSPART);
   AncTfw2->SetBranchAddress("TOFSPTOT",&TOFSPTOT);
   AncTfw2->SetBranchAddress("TOFSPTHET",&TOFSPTHET);
   AncTfw2->SetBranchAddress("TOFSPPHI",&TOFSPPHI);
   AncTfw2->SetBranchAddress("PC1SAME",&PC1SAME);
   AncTfw2->SetBranchAddress("PC1SIST",&PC1SIST);
   AncTfw2->SetBranchAddress("TOFWSPHI",&TOFWSPHI);
   AncTfw2->SetBranchAddress("AERSPHI",&AERSPHI);
   AncTfw2->SetBranchAddress("Z0_EVENT",&Z0_EVENT);
   AncTfw2->SetBranchAddress("B_IMPACT",&B_IMPACT);
   AncTfw2->SetBranchAddress("EVENT",&EVENT);

   //
   // Useful constants
   //
   const double DEGRAD = 57.295779513;                  // radians to degrees conversion
   const double LAMBDAMASS = 1.11560;                   // Lambda mass
   const double PROTONMASS = 0.93828;                   // proton mass
   const double PROTONMASSSQ = PROTONMASS*PROTONMASS;
   const double PIONMASS = 0.139567;                    // pion mass
   const double PIONMASSSQ = PIONMASS*PIONMASS;

   if(iOption == 1) {   // Lambda pair analysis

     Long64_t nEntries = AncTfw2->GetEntries();   // Long64_t is what ROOT uses internally
     cout << "\n Number of entries in file = " << nEntries << "; will use Decay RMAX = " << DecayRMAX << " cm" << endl;
     Long64_t readEntries = nEntries;
     if(MAXENTRIES>0 && MAXENTRIES<readEntries) {
       readEntries = MAXENTRIES;
       cout << "\n Will read first " << readEntries << " entries" << endl;
     }

     //
     // Counting statistics before momentum cuts are checked
     //
     Int_t nBytes = 0;
     int nLambdaTofWest = 0;
     int nLambdaTofWestSisterTofEast = 0;
     int nLambdaTofWestBoth = 0;
     int nLambdaTofWestSameAER = 0;
     int nLambdaTofWestSameAERSisterAER = 0;
     int nLambdaTofWestSameAERNoSisterAERSisterPC3 = 0;
     int nLambdaTofWestNoSameAERNoSisterAERSisterPC3 = 0;
     int nLambdaTofWestBothSameAER = 0;
     int nLambdaTofWestBothSisterAER = 0;
     int nLambdaTofWestBothSameAERSisterAER = 0;
     int nLambdaTofWestBothNoSameAERSisterAER = 0;
     int nLambdaTofWestBothNoSameAERNoSisterAERSisterPC3 = 0;

     int nTofWestMissingPC3 = 0;
     int nAerMissingPC3 = 0;
     int nTofEastMissingPC3;

     int nTofWestSisterMissingPC3 = 0;
     int nAerSisterMissingPC3 = 0;
     int nTofEastSisterMissingPC3 = 0;

     int countPC3Sister = 0;
     int countValidMass = 0;

     //
     // Pair rapidity and transverse momentum histograms
     //

     TH1F *h1PairRapidityCut = new TH1F("h1PairRapidityCut", "Cut: Summed Rapidity", 60, -0.6, +0.6); 
     TH1F *h1PairPTranCut= new TH1F("h1PairPTranRCut", "Cut: Summed PT", 50, 0.0, +20.0); 
     h1PairRapidityCut->SetFillColor(2);
     h1PairPTranCut->SetFillColor(2);
     h1PairRapidityCut->SetXTitle("Lambda Rapidity Acceptance");
     h1PairRapidityCut->SetYTitle("Counts per 0.02 rapidity units");
     h1PairPTranCut->SetXTitle("Lambda Transverse Momentum (GeV/c) Acceptance");
     h1PairPTranCut->SetYTitle("Counts per 0.4 GeV/c bin");

     TH1F *h1PairRapidityBothTFWBothAER = new TH1F("h1PairRapidityBothTFWBothAER", "Rapidity when both decays are in TFW and AER", 60, -0.6, +0.6); 
     TH1F *h1PairPTranBothTFWBothAER = new TH1F("h1PairPTranBothTFWBothAER", "PT when both decays are in TFW and AER", 50, 0.0, +20.0); 
     TH1F *h1PairRapidityBothTFWBothAERCut = new TH1F("h1PairRapidityBothTFWAERCut", "Cut: Rapidity when both decays are in TFW and AER", 60, -0.6, +0.6); 
     TH1F *h1PairPTranBothTFWBothAERCut= new TH1F("h1PairPTranBothTFWBothAERCut", "Cut: PT when both decays are in TFW and AER", 50, 0.0, +20.0); 
     h1PairRapidityBothTFWBothAERCut->SetFillColor(2);
     h1PairPTranBothTFWBothAERCut->SetFillColor(2);

     TH1F *h1PairRapidityOneTFWandAERTFE = new TH1F("h1PairRapidityOneTFWandAERTFE", "Rapidity when one in TFW and AER, sister in TOF-East", 60, -0.6, +0.6); 
     TH1F *h1PairPTranOneTFWandAERTFE = new TH1F("h1PairPTranOneTFWandAERTFE", "PT when one in TFW and in AER, sister in TOF-East ", 50, 0.0, +20.0); 
     TH1F *h1PairRapidityOneTFWandAERTFECut = new TH1F("h1PairRapidityOneTFWandAERTFECut", "Cut: Rapidity when one in TFW and AER, sister in TOF-East, sister in PC3", 60, -0.6, +0.6); 
     TH1F *h1PairPTranOneTFWandAERTFECut = new TH1F("h1PairPTranOneTFWandAERTFECut", "Cut: PT when one in TFW and AER, sister in TOF-East ", 50, 0.0, +20.0); 
     h1PairRapidityOneTFWandAERTFECut->SetFillColor(2);
     h1PairPTranOneTFWandAERTFECut->SetFillColor(2);

     TH1F *h1PairRapidityOneTFWnotAERTFE = new TH1F("h1PairRapidityOneTFWnotAERTFE", "Rapidity when one in TFW but not AER, sister in TOF-East", 60, -0.6, +0.6); 
     TH1F *h1PairPTranOneTFWnotAERTFE = new TH1F("h1PairPTranOneTFWnotAERTFE", "PT when one in TFW but not AER, sister in TOF-East ", 50, 0.0, +20.0); 
     TH1F *h1PairRapidityOneTFWnotAERTFECut = new TH1F("h1PairRapidityOneTFWnotAERTFECut", "Cut: Rapidity when one in TFW but not AER, sister in TOF-East, sister in PC3", 60, -0.6, +0.6); 
     TH1F *h1PairPTranOneTFWnotAERTFECut = new TH1F("h1PairPTranOneTFWnotAERTFECut", "Cut: PT when one in TFW but not AER, sister in TOF-East ", 50, 0.0, +20.0); 
     h1PairRapidityOneTFWnotAERTFECut->SetFillColor(2);
     h1PairPTranOneTFWnotAERTFECut->SetFillColor(2);

     TH1F *h1PairRapidityOneTFWNeitherAER = new TH1F("h1PairRapidityOneTFWNeitherAER", "Rapidity when only one in TFW, neither in AER, sister in PC3", 60, -0.6, +0.6); 
     TH1F *h1PairPTranOneTFWNeitherAER = new TH1F("h1PairPTranOneTFWNeitherAER", "PT when only one in TFW, neither in AER, sister in PC3 ", 50, 0.0, +20.0); 
     TH1F *h1PairRapidityOneTFWNeitherAERCut = new TH1F("h1PairRapidityOneTFWNeitherAERCut", "Cut: Rapidity when only one in TFW, neither in AER, sister in PC3", 60, -0.6, +0.6); 
     TH1F *h1PairPTranOneTFWNeitherAERCut = new TH1F("h1PairPTranOneTFWNeitherAERCut", "Cut: PT when only one in TFW, neither in AER, sister in PC3 ", 50, 0.0, +20.0); 
     h1PairRapidityOneTFWNeitherAERCut->SetFillColor(2);
     h1PairPTranOneTFWNeitherAERCut->SetFillColor(2);

     TH1F *h1PairRapidityBothTFWNeitherAER = new TH1F("h1PairRapidityBothTFWNeitherAER", "Rapidity when both in TFW, neither in AER", 60, -0.6, +0.6); 
     TH1F *h1PairPTranBothTFWNeitherAER = new TH1F("h1PairPTranBothTFWNeitherAER", "PT when both in TFW, neither in AER", 50, 0.0, +20.0); 
     TH1F *h1PairRapidityBothTFWNeitherAERCut = new TH1F("h1PairRapidityBothTFWNeitherAERCut", "Cut: Rapidity when both in TFW, neither in AER", 60, -0.6, +0.6); 
     TH1F *h1PairPTranBothTFWNeitherAERCut = new TH1F("h1PairPTranBothTFWNeitherAERCut", "Cut: PT when both in TFW, neither in AER", 50, 0.0, +20.0); 
     h1PairRapidityBothTFWNeitherAERCut->SetFillColor(2);
     h1PairPTranBothTFWNeitherAERCut->SetFillColor(2);

     TH1F *h1PairRapidityOneTFWSameAER = new TH1F("h1PairRapidityOneTFWSameAER", "Rapidity when only one in TFW and in AER, sister not in AER but in PC3", 60, -0.6, +0.6); 
     TH1F *h1PairPTranOneTFWSameAER = new TH1F("h1PairPTranOneTFWSameAER", "PT when only one in TFW and in AER, sister not in AER but in PC3", 50, 0.0, +20.0); 
     TH1F *h1PairRapidityOneTFWSameAERCut = new TH1F("h1PairRapidityOneTFWSameAERCut", "Cut: Rapidity when only one in TFW and in AER, sister not in AER but in PC3", 60, -0.6, +0.6); 
     TH1F *h1PairPTranOneTFWSameAERCut = new TH1F("h1PairPTranOneTFWSameAERCut", "Cut: PT when only one in TFW and in AER, sister not in AER but in PC3", 50, 0.0, +20.0); 
     h1PairRapidityOneTFWSameAERCut->SetFillColor(2);
     h1PairPTranOneTFWSameAERCut->SetFillColor(2);

     TH1F *h1PairRapidityOneTFWSisterAER = new TH1F("h1PairRapidityOneTFWSisterAER", "Rapidity when only one in TFW and not in Aer, sister in AER", 60, -0.6, +0.6); 
     TH1F *h1PairPTranOneTFWSisterAER = new TH1F("h1PairPTranOneTFWSisterAER", "PT when only one in TFW and not in Aer, sister in AER", 50, 0.0, +20.0); 
     TH1F *h1PairRapidityOneTFWSisterAERCut = new TH1F("h1PairRapidityOneTFWSisterAERCut", "Cut: Rapidity when only one in TFW and not in Aer, sister in AER", 60, -0.6, +0.6); 
     TH1F *h1PairPTranOneTFWSisterAERCut = new TH1F("h1PairPTranOneTFWSisterAERCut", "Cut: PT when only one in TFW and not in Aer, sister in AER", 50, 0.0, +20.0); 
     h1PairRapidityOneTFWSisterAERCut->SetFillColor(2);
     h1PairPTranOneTFWSisterAERCut->SetFillColor(2);

     TH1F *h1PairRapidityOneTFWBothAER = new TH1F("h1PairRapidityOneTFWBothAER", "Rapidity when only one in TFW, both in AER", 60, -0.6, +0.6); 
     TH1F *h1PairPTranOneTFWBothAER = new TH1F("h1PairPTranOneTFWBothAER", "PT when only one in TFW, both in AER", 50, 0.0, +20.0); 
     TH1F *h1PairRapidityOneTFWBothAERCut = new TH1F("h1PairRapidityOneTFWBothAERCut", "Cut: Rapidity when only one in TFW, both in AER", 60, -0.6, +0.6); 
     TH1F *h1PairPTranOneTFWBothAERCut = new TH1F("h1PairPTranOneTFWBothAERCut", "Cut: PT when only one in TFW, both in AER", 50, 0.0, +20.0); 
     h1PairRapidityOneTFWBothAERCut->SetFillColor(2);
     h1PairPTranOneTFWBothAERCut->SetFillColor(2);

     TH1F *h1PairRapidityBothTFWOneAER = new TH1F("h1PairRapidityBothTFWOneAER", "Rapidity when both in TFW, only one in AER", 60, -0.6, +0.6); 
     TH1F *h1PairPTranBothTFWOneAER = new TH1F("h1PairPTranBothTFWOneAER", "PT when both in TFW, only one AER", 50, 0.0, +20.0); 
     TH1F *h1PairRapidityBothTFWOneAERCut = new TH1F("h1PairRapidityBothTFWOneAERCut", "Cut: Rapidity when both in TFW, only one in AER", 60, -0.6, +0.6); 
     TH1F *h1PairPTranBothTFWOneAERCut = new TH1F("h1PairPTranBothTFWOneAERCut", "Cut: PT when both in TFW, only one AER", 50, 0.0, +20.0); 
     h1PairRapidityBothTFWOneAERCut->SetFillColor(2);
     h1PairPTranBothTFWOneAERCut->SetFillColor(2);

     //
     // Sum over all entries (or fewer if requested by MAXENTRIES) in the NTUPLE file
     //

     for (Long64_t i=0; i<readEntries;i++) {
       nBytes += AncTfw2->GetEntry(i);

       int idParent = IDPARENT;
       int idPart = IDPART;
       if(idParent == 18 && DELE > 0 && R_VERTEX < DecayRMAX && (idPart==9 || idPart == 14)) {

         if(pc1SameTag && PC1SAME<0)
           continue;

         if(pc1SisterTag && PC1SIST<0)
           continue;

	 if(PTOT<minMomentum || PTOT>maxMomentum)
	   continue;

	 nLambdaTofWest++;

	 //
	 // Invariant mass calculation for the Lambda
	 //
	 // It is assumed that any sister particle in TOF-West or Aerogel is also detected in PC3,
	 // so the sister momentum vector is taken from the PC3 information
	 //
	 // The missing PC3 variables count when this assumption determined to be false
	 //

	 if(TOFWSPHI>-90.0 && PC3SIST<0)
	   nTofWestSisterMissingPC3++;  // sister in TOF-West but not in PC3

	 if(AERSPHI>-90.0 && PC3SIST<0)
	   nAerSisterMissingPC3++;  // sister in AER but not in PC3 

	 if(TOFESPHI>+90.0 && PC3SIST<0)
	   nTofEastSisterMissingPC3++;  // sister in TOF-East but not in PC3 

         bool validMass = false;
         double vertexMass = 0.0;
         double vertexPairRapidity = -999.0;
         double vertexPairPTran = -999.0;
         float protonMomentum = 0.0;
         float pionMomentum = 0.0;
         if(PC3SIST>0) {        // if there is no sister particle in PC3, then pair mass calculate is skipped
           int idSist = PC3SPART;
           if(idSist + idPart == 23) {
             countPC3Sister++;
	     if(PC3SPTOT>minMomentum && PC3SPTOT<maxMomentum) {

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
	     } // check on mass values adding to 23
	   } // check on sister momentum range
         } // check on sister particle in PC3

	 if(TOFESPHI>90.0) {
	   nLambdaTofWestSisterTofEast++;
	 }

	 if(TOFWSPHI>-90.0) {
	   nLambdaTofWestBoth++;
	   if(AERSAME > 0.0) {
	      nLambdaTofWestBothSameAER++;
	      if(AERSPHI > -90.0) {
		nLambdaTofWestBothSameAERSisterAER++;
	      }
	   } // check for first particle also in AER
	   else {
	      if(AERSPHI > -90.0) {
		nLambdaTofWestBothNoSameAERSisterAER++;
	      }
	   } // check for sister in AER while first particle is not in AER
	   if(PC3SPHI>-90.0 && AERSPHI < -90.0 && AERSAME < 0.0) {
	     nLambdaTofWestBothNoSameAERNoSisterAERSisterPC3++;
	   }
	 } // check for both particles in TOF-West

	 if(AERSAME > 0.0) {
	   nLambdaTofWestSameAER++;
	   if(AERSPHI>-90.0) {
	     nLambdaTofWestSameAERSisterAER++;
	   } // sister is in AER
	   else {
             // sister is not in AER
	     if(PC3SPHI>-90.0) {
	       nLambdaTofWestSameAERNoSisterAERSisterPC3++;
	     } // sister is in PC3 but not in AER
	   }
	 }  // check for same particle in AER

	 if(AERSAME < 0.0) {
	   if(AERSPHI<-90.0) {
	     if(PC3SPHI>-90.0) {
	       nLambdaTofWestNoSameAERNoSisterAERSisterPC3++;
	     } // sister is in PC3 but not in AER, particle is not in AER
	   } // check that sister is not in AER
         } // check that this particle is not in AER


	 //
	 // Fill pair rapidity and transverse momentum histograms if there is a valid mass
	 //
         if(!validMass) // this condition requires a sister particle in PC3
	   continue;

         if(TOFWSPHI>-90.0 && AERSAME>0.0 && AERSPHI>-90.0) {
           if(idPart == 9) { // plot only when first particle is a pion, avoids double counting
             h1PairRapidityBothTFWBothAER->Fill(vertexPairRapidity);
             h1PairPTranBothTFWBothAER->Fill(vertexPairPTran);
             if(pionMomentum>pionTFWandAERMin && pionMomentum<pionTFWandAERMax &&
                protonMomentum>protonTFWandAERMin && protonMomentum<protonTFWandAERMax) {
                h1PairRapidityBothTFWBothAERCut->Fill(vertexPairRapidity);
                h1PairPTranBothTFWBothAERCut->Fill(vertexPairPTran);
                h1PairRapidityCut->Fill(vertexPairRapidity);
                h1PairPTranCut->Fill(vertexPairPTran);
             } 
           } // double counting check
           continue;
         } // both particles in TOF-West and in Aerogel

         if(TOFWSPHI<-90.0 && AERSAME<0.0 && AERSPHI<-90.0) {
           h1PairRapidityOneTFWNeitherAER->Fill(vertexPairRapidity);
           h1PairPTranOneTFWNeitherAER->Fill(vertexPairPTran);
           if((idPart==9 && pionMomentum>pionTFWonlyMin && pionMomentum<pionTFWonlyMax &&
               idSist==14 && protonMomentum>protonEMConlyMin && protonMomentum<protonEMConlyMax) ||
              (idPart==14 && protonMomentum>protonTFWonlyMin && protonMomentum<protonTFWonlyMax &&
               idSist==9 && pionMomentum>pionEMConlyMin && pionMomentum<pionEMConlyMax)) {
             h1PairRapidityOneTFWNeitherAERCut->Fill(vertexPairRapidity);
             h1PairPTranOneTFWNeitherAERCut->Fill(vertexPairPTran);
             h1PairRapidityCut->Fill(vertexPairRapidity);
             h1PairPTranCut->Fill(vertexPairPTran);
           }
           continue;
         } // only one in TOF-West, neither in Aerogel

	 /*	
	 if(TOFESPHI>90.0 && AERSAME>0.0) {
           h1PairRapidityOneTFWandAERTFE->Fill(vertexPairRapidity);
           h1PairPTranOneTFWandAERTFE->Fill(vertexPairPTran); 
           if((idPart==9 && pionMomentum>pionTFWandAERMin && pionMomentum<pionTFWandAERMax &&
               idSist==14 && protonMomentum>protonTFEMin && protonMomentum<protonTFEMax) ||
              (idPart==14 && protonMomentum>protonTFWandAERMin && protonMomentum<protonTFWandAERMax &&
               idSist==9 && pionMomentum>pionTFEMin && pionMomentum<pionTFEMax)) {
             h1PairRapidityOneTFWandAERTFECut->Fill(vertexPairRapidity);
             h1PairPTranOneTFWandAERTFECut->Fill(vertexPairPTran);
             h1PairRapidityCut->Fill(vertexPairRapidity);
             h1PairPTranCut->Fill(vertexPairPTran);
           }
           continue;
         } // only one in TOF-West and Aerogel, sister is in TOF-East
	
	 if(TOFESPHI>90.0 && AERSAME<0.0) {
           h1PairRapidityOneTFWnotAERTFE->Fill(vertexPairRapidity);
           h1PairPTranOneTFWnotAERTFE->Fill(vertexPairPTran);
           if((idPart==9 && pionMomentum>pionTFWonlyMin && pionMomentum<pionTFWonlyMax &&
               idSist==14 && protonMomentum>protonTFEMin && protonMomentum<protonTFEMax) ||
              (idPart==14 && protonMomentum>protonTFWonlyMin && protonMomentum<protonTFWonlyMax &&
               idSist==9 && pionMomentum>pionTFEMin && pionMomentum<pionTFEMax)) {
             h1PairRapidityOneTFWnotAERTFECut->Fill(vertexPairRapidity);
             h1PairPTranOneTFWnotAERTFECut->Fill(vertexPairPTran);
             h1PairRapidityCut->Fill(vertexPairRapidity);
             h1PairPTranCut->Fill(vertexPairPTran);
           }
           continue;
         } // only one in TOF-West but not in Aerogel, sister is in TOF-East
	 */

         if(TOFWSPHI>-90.0 && AERSAME<0.0 && AERSPHI<-90.0) {
           if(idPart == 9) { // plot only when first particle is a pion, avoids double countint
             h1PairRapidityBothTFWNeitherAER->Fill(vertexPairRapidity);
             h1PairPTranBothTFWNeitherAER->Fill(vertexPairPTran);
             if(pionMomentum>pionTFWonlyMin && pionMomentum<pionTFWonlyMax &&
                protonMomentum>protonTFWonlyMin && protonMomentum<protonTFWonlyMax) {
                h1PairRapidityBothTFWNeitherAERCut->Fill(vertexPairRapidity);
                h1PairPTranBothTFWNeitherAERCut->Fill(vertexPairPTran);
                h1PairRapidityCut->Fill(vertexPairRapidity);
                h1PairPTranCut->Fill(vertexPairPTran);
             }
           }
           continue;
         } // both in TOF-West, neither in Aerogel

         if(TOFWSPHI<-90.0 && AERSAME>0.0 && AERSPHI<-90.0) {
           h1PairRapidityOneTFWSameAER->Fill(vertexPairRapidity);
           h1PairPTranOneTFWSameAER->Fill(vertexPairPTran);
           if((idPart==9 && pionMomentum>pionTFWandAERMin && pionMomentum<pionTFWandAERMax &&
               idSist==14 && protonMomentum>protonEMConlyMin && protonMomentum<protonEMConlyMax) ||
              (idPart==14 && protonMomentum>protonTFWandAERMin && protonMomentum<protonTFWandAERMax &&
               idSist==9 && pionMomentum>pionEMConlyMin && pionMomentum<pionEMConlyMax)) {
             h1PairRapidityOneTFWSameAERCut->Fill(vertexPairRapidity);
             h1PairPTranOneTFWSameAERCut->Fill(vertexPairPTran);
             h1PairRapidityCut->Fill(vertexPairRapidity);
             h1PairPTranCut->Fill(vertexPairPTran);
           }
           continue;
         } // one in TOF-West, same in Aerogel, sister not in Aerogel

         if(TOFWSPHI<-90.0 && AERSAME<0.0 && AERSPHI>-90.0) {
           h1PairRapidityOneTFWSisterAER->Fill(vertexPairRapidity);
           h1PairPTranOneTFWSisterAER->Fill(vertexPairPTran);
           if((idPart==9 && pionMomentum>pionTFWonlyMin && pionMomentum<pionTFWonlyMax &&
               idSist==14 && protonMomentum>protonAERonlyMin && protonMomentum<protonAERonlyMax) ||
              (idPart==14 && protonMomentum>protonTFWonlyMin && protonMomentum<protonTFWonlyMax &&
               idSist==9 && pionMomentum>pionAERonlyMin && pionMomentum<pionAERonlyMax)) {
             h1PairRapidityOneTFWSisterAERCut->Fill(vertexPairRapidity);
             h1PairPTranOneTFWSisterAERCut->Fill(vertexPairPTran);
             h1PairRapidityCut->Fill(vertexPairRapidity);
             h1PairPTranCut->Fill(vertexPairPTran);
           }
           continue;
         } // one in TOF-West, same not in Aerogel, sister in Aerogel

         if(TOFWSPHI<-90.0 && AERSAME>0.0 && AERSPHI>-90.0) {
           h1PairRapidityOneTFWBothAER->Fill(vertexPairRapidity);
           h1PairPTranOneTFWBothAER->Fill(vertexPairPTran);
           if((idPart==9 && pionMomentum>pionTFWandAERMin && pionMomentum<pionTFWandAERMax &&
               idSist==14 && protonMomentum>protonAERonlyMin && protonMomentum<protonAERonlyMax) ||
              (idPart==14 && protonMomentum>protonTFWandAERMin && protonMomentum<protonTFWandAERMax &&
               idSist==9 && pionMomentum>pionAERonlyMin && pionMomentum<pionAERonlyMax)) {
             h1PairRapidityOneTFWBothAERCut->Fill(vertexPairRapidity);
             h1PairPTranOneTFWBothAERCut->Fill(vertexPairPTran);
             h1PairRapidityCut->Fill(vertexPairRapidity);
             h1PairPTranCut->Fill(vertexPairPTran);
           }
           continue;
         } // only one in TOF-West, both in Aerogel

         if(TOFWSPHI>-90.0 && ((AERSAME>0.0 && AERSPHI<-90.0) || (AERSAME<0.0 && AERSPHI>-90.0))) {
           if(idPart == 9) { // plot only when first particle is a pion, avoids double counting
             h1PairRapidityBothTFWOneAER->Fill(vertexPairRapidity);
             h1PairPTranBothTFWOneAER->Fill(vertexPairPTran);
             if((AERSAME>0.0 && pionMomentum>pionTFWandAERMin && pionMomentum<pionTFWandAERMax &&
                 protonMomentum>protonTFWonlyMin && protonMomentum<protonTFWonlyMax) ||
                (AERSAME<0.0 && pionMomentum>pionTFWonlyMin && pionMomentum<pionTFWonlyMax &&
                 protonMomentum>protonTFWandAERMin && protonMomentum<protonTFWandAERMax)) {
               h1PairRapidityBothTFWOneAERCut->Fill(vertexPairRapidity);
               h1PairPTranBothTFWOneAERCut->Fill(vertexPairPTran);
               h1PairRapidityCut->Fill(vertexPairRapidity);
               h1PairPTranCut->Fill(vertexPairPTran);
             }
           }
           continue;
         } // both in TOF-West, only one in Aerogel

       } // check for Lambda parent, positive energy loss, vertex below DecayRMAX, and either a pi- or a proton

     } // loop over entries

     cout << "\n Number of entries in Tof-West with a parent Lambda decaying into p + pi- = " << nLambdaTofWest;
     cout << "\n Number of entries in Tof-West with one Lambda decay particle in TOF-West and sister particle in detected in TOF-East  = " << nLambdaTofWestSisterTofEast;
     cout << "\n Number of entries in Tof-West with both Lambda decay particles in TOF-West = " << nLambdaTofWestBoth;
     cout <<  endl;

     cout << "\n Number of entries in Tof-West with at least one decay particle in TOF-West and same particle in AER = " << nLambdaTofWestSameAER;
     cout << "\n Number of entries in Tof-West with at least one decay particle in TOF-West, same particle in AER, and sister in AER = " << nLambdaTofWestSameAERSisterAER;
     cout << "\n Number of entries in Tof-West with at least one decay particle in TOF-West, same particle in AER, sister is not AER but sister is in PC3 = ";
     cout << nLambdaTofWestSameAERNoSisterAERSisterPC3;
     cout << "\n Number of entries in Tof-West with at least one decay particle in TOF-West, neither particle in AER, but sister is in PC3 = ";
     cout << nLambdaTofWestNoSameAERNoSisterAERSisterPC3;
     cout << endl;

     cout << "\n Number of entries in Tof-West with both Lambda decay particles in TOF-West and first decay particle in AER = " << nLambdaTofWestBothSameAER;
     cout << "\n Number of entries in Tof-West with both Lambda decay particles in TOF-West and both decay particles in AER = " << nLambdaTofWestBothSameAERSisterAER;
     cout << "\n Number of entries in Tof-West with both Lambda decay particles in TOF-West, and only sister particle is in AER = " << nLambdaTofWestBothNoSameAERSisterAER;
     cout << "\n Number of entries in Tof-West with both Lambda decay particles in TOF-West, neither decay particle is in AER, but sister is in PC3 = ";
     cout << nLambdaTofWestBothNoSameAERNoSisterAERSisterPC3;

     cout << endl;

     cout << "\n Number of PC3 sister pairs " << countPC3Sister;
     cout << ";  number of valid pair mass values " << countValidMass;
     cout << endl;

     cout << "\n Number of missing PC3 when sister is in TOF-West = " << nTofWestMissingPC3;
     cout << "\n Number of missing PC3 when sister is in Aerogel = " << nAerMissingPC3;
     cout << "\n Number of missing PC3 when sister is in TOF-East = " << nTofEastMissingPC3;
     cout << endl;

     cout << "\n iPlot = 1  Both decay particles in TOF-West and in Aerogel, both particles in EMC/PC3: yield = " << h1PairRapidityBothTFWBothAER->GetSum();
     cout << "\n            After momentum cuts yield = " << h1PairRapidityBothTFWBothAERCut->GetSum();
     if(iPlot == 1) {
       c1->Divide(1,2);
       c1->cd(1);
       h1PairRapidityBothTFWBothAER->Draw();
       c1->cd(2);
       h1PairPTranBothTFWBothAER->Draw();
     } // iPLot = 1, plot when both particles are in TOF-West and in Aerogel

     cout << "\n iPlot = 2  Both decay particles in TOF-West, neither particle in Aerogel, both particles in EMC/PC3: yield = " << h1PairRapidityBothTFWNeitherAER->GetSum();
     cout << "\n            After momentum cuts yield = " << h1PairRapidityBothTFWNeitherAERCut->GetSum();
     if(iPlot == 2) {
       c1->Divide(1,2);
       c1->cd(1);
       h1PairRapidityBothTFWNeitherAER->Draw();
       c1->cd(2);
       h1PairPTranBothTFWNeitherAER->Draw();
     } // iPLot = 2, plot both in TOF-West, neither in aerogel

     cout << "\n iPlot = 3  Both decay particles in TOF-West, only one particle in Aerogel and other particle in EMC/PC3: yield = " << h1PairRapidityBothTFWOneAER->GetSum();
     cout << "\n            After momentum cuts yield = " << h1PairRapidityBothTFWOneAERCut->GetSum();
     if(iPlot == 3) {
       c1->Divide(1,2);
       c1->cd(1);
       h1PairRapidityBothTFWOneAER->Draw();
       c1->cd(2);
       h1PairPTranBothTFWOneAER->Draw();
     } // iPLot = 3, plot both in TOF-West, only one in aerogel

     cout << "\n iPlot = 4  Only one particle in TOF-West, both particles in Aerogel and EMC/PC3: yield = " << h1PairRapidityOneTFWBothAER->GetSum();
     cout << "\n            After momentum cuts yield = " << h1PairRapidityOneTFWBothAERCut->GetSum();
     if(iPlot == 4) {
       c1->Divide(1,2);
       c1->cd(1);
       h1PairRapidityOneTFWBothAER->Draw();
       c1->cd(2);
       h1PairPTranOneTFWBothAER->Draw();
     } // iPLot = 4, plot only one in TOF-West, both in aerogel

     cout << "\n iPlot = 5  Only one particle in TOF-West, neither particle in Aerogel, sister particle only in EMC/PC3: yield = " << h1PairRapidityOneTFWNeitherAER->GetSum();
     cout << "\n            After momentum cuts yield = " << h1PairRapidityOneTFWNeitherAERCut->GetSum();
     if(iPlot == 5) {
       c1->Divide(1,2);
       c1->cd(1);
       h1PairRapidityOneTFWNeitherAER->Draw();
       c1->cd(2);
       h1PairPTranOneTFWNeitherAER->Draw();
     } // iPLot = 5, plot only one in TOF-West, neither in aerogel 

     cout << "\n iPlot = 6  Only one particle in TOF-West and in Aerogel, sister particle only in EMC/PC3: yield = " << h1PairRapidityOneTFWSameAER->GetSum();
     cout << "\n            After momentum cuts yield = " << h1PairRapidityOneTFWSameAERCut->GetSum();
     if(iPlot == 6) {
       c1->Divide(1,2);
       c1->cd(1);
       h1PairRapidityOneTFWSameAER->Draw();
       c1->cd(2);
       h1PairPTranOneTFWSameAER->Draw();
     } // iPLot = 6, plot only one in TOF-West, same in aerogel, sister not in aerogel

     cout << "\n iPlot = 7  Only one particle in TOF-West and is not in Aerogel, sister particle only in Aerogel and EMC/PC3: yield = " << h1PairRapidityOneTFWSisterAER->GetSum();
     cout << "\n            After momentum cuts yield = " << h1PairRapidityOneTFWSisterAERCut->GetSum();
     if(iPlot == 7) {
       c1->Divide(1,2);
       c1->cd(1);
       h1PairRapidityOneTFWSisterAER->Draw();
       c1->cd(2);
       h1PairPTranOneTFWSisterAER->Draw();
     } // iPLot = 7, plot only one in TOF-West, same not aerogel, sister in aerogel

     cout << "\n iPlot = 8  Only one particle in TOF-West and in Aerogel, sister particle TOF-East: yield = " << h1PairRapidityOneTFWandAERTFE->GetSum();
     cout << "\n            After momentum cuts yield = " << h1PairRapidityOneTFWandAERTFECut->GetSum();
     if(iPlot == 8) {
       c1->Divide(1,2);
       c1->cd(1);
       h1PairRapidityOneTFWandAERTFE->Draw();
       c1->cd(2);
       h1PairPTranOneTFWandAERTFE->Draw();
     } //

     cout << "\n iPlot = 9  Only one particle in TOF-West and is not in Aerogel, sister particle TOF-East: yield = " << h1PairRapidityOneTFWnotAERTFE->GetSum();
     cout << "\n            After momentum cuts yield = " << h1PairRapidityOneTFWnotAERTFECut->GetSum();
     if(iPlot == 9) {
       c1->Divide(1,2);
       c1->cd(1);
       h1PairRapidityOneTFWnotAERTFE->Draw();
       c1->cd(2);
       h1PairPTranOneTFWnotAERTFE->Draw();
     } //

     cout << "\n\n   After momentum cuts summed yield = " << h1PairRapidityCut->GetSum();
     if(iPlot == 0) {
       c1->Divide(1,2);
       c1->cd(1);
       if(posField)
	 h1PairRapidityCut->SetMaximum(600);
       else
	  h1PairRapidityCut->SetMaximum(600);
       h1PairRapidityCut->Draw();
       TLatex *tex0 = new TLatex(-0.55, rScale*550.0, "Simulation of #Lambda #rightarrow p + #pi^{-} Accepted in PHENIX");
       tex0->SetTextColor(4);
       tex0->SetTextSize(0.06);
       tex0->Draw();

       TLatex *tex1 = new TLatex(-0.55, rScale*500.0, "Using TOF-West with RICH, Aerogel and EMCal for particle identification");
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

       TLatex *tex3 = new TLatex(-0.55, rScale*400.0, "February 19, 2007");
       tex3->SetTextColor(4);
       tex3->SetTextSize(0.05);
       tex3->Draw();

       c1->cd(2);
       if(posField)
	 h1PairPTranCut->SetMaximum(800);
       else
	  h1PairPTranCut->SetMaximum(800);
       h1PairPTranCut->Draw();
     } // iPLot = 0, plot summed yield after all momentum cuts

     cout << endl;

   } // iOption = 1, Lambda decay analysis

}
