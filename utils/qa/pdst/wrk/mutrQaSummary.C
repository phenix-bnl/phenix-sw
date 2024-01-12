{
  textFile << " ----------------------------------------------------" << endl;
  textFile << " -- MUTR QA Summary --" << endl;
  textFile << " ----------------------------------------------------" << endl;

  // define a bunch of constants
  const int NUMARM      = 2;
  const int NUMSTATION  = 3;
  const int NUMOCTANT   = 8;
  const int NUMHALFOCT  = 2;
  //const int NUMGAPST[NUMSTATION] = { 3, 3, 2};
  const int NUMPLANEST[NUMSTATION] = { 6, 6, 4};
  //onst int TOTPLANEST[NUMSTATION] = { 48, 48, 32};
  const int NUMSAMPLES  = 4;
  const int MAXTOTPLANE = 48;

  const int NUMLANDAU = 3; // number of Landau parameters
  const int NUMADDR  = 2; // used for keeping track of octant and plane info for hot/dead regions

  const float CLUSWIDMIN = 1.75;
  const float CLUSWIDMAX = 3.25;
  const int CLUSMINST[NUMSTATION] = { 0, 0, 0 };
  const int CLUSMAXST[NUMSTATION] = { 192, 192, 128 };
  const int NUMTRKMIN =    0;
  const int NUMTRKMAX =  100;
  const float PTOTMIN =    1.;
  const float PTOTMAX =   50.;
  const int NUMSTATBITS = 15;
  // packet
  const int MAXNUMPACKET = 192;
  const int TOTPACKETARM[NUMARM] = { 168, 192};
  
  //#define NNOHITSTX TOTPLANESSTX/8
  const int NNOHITST[NUMSTATION] = { 6, 6, 4 }; 
  //#define NHOTSTX TOTPLANESSTX/4
  const int NHOTST[NUMSTATION] = { 12, 12, 8 }; 
  
  // datatypes: 0 = Run2/Run4 AuAu, 1 = Run2(&3) pp, 2 = Run3 dAu 
  // dA values are set at about the mean between AA and pp for now..
  const int NDATATYPES = 3;
  const int CLUSQFITMIN[NDATATYPES] = { 50, 30, 40 };
  const int CLUSQFITMAX[NDATATYPES] = { 100, 100, 100 };
  
  
  // cluster peak charge in ADC value, so all limit times 10(gain value)
  const int CLUSQPEAKMIN[NDATATYPES] = { 300, 200, 250 };
  const int CLUSQPEAKMAX[NDATATYPES] = { 900, 900, 900 };
  
  const int PULSEHEIGHTMIN[NDATATYPES] = { 15, 2, 8 };

  const int HOTPACKST[NDATATYPES][NUMSTATION] = { 50, 40, 30,
                                                  50, 40, 30,       
                                                  50, 40, 30};
  
  int QPEAKRANGEMINST[NDATATYPES][NUMSTATION][NUMOCTANT][NUMHALFOCT];			
  int QPEAKRANGEMAXST[NDATATYPES][NUMSTATION][NUMOCTANT][NUMHALFOCT];
  
  double HOTPLANEST[NDATATYPES][NUMSTATION];
  
  // note that ordering of an array starts with last index, i.e
  // int test[2][2] = { 0, 1, 2, 3};
  // cout << test[0][1] << endl; - gives 1
  // cout << test[1][0] << endl; - gives 2
  // Thus [NDATATYPES][NUMSTATION] means that the first 3 values
  // are for the first datatype (one for each station), and so on
  
  // current comment the QFIT limitation valure.
  //const int QFITRANGEMINST[NDATATYPES][NUMSTATION] = { 1, 1, 1,  // datatype 0 
  //  					                1, 1, 1,  // datatype 1
  //					                1, 1, 1 };// datatype 2 
  //const int QFITRANGEMAXST[NDATATYPES][NUMSTATION] = { 125, 125, 125,  // datatype 0 
  //					                130, 130, 120,  // datatype 1
  //                                                     125, 125, 125};  // datatype 1
  
  
  for (int datatpcons = 0; datatpcons<NDATATYPES; datatpcons++){
    for (int stacons = 0; stacons<NUMSTATION; stacons++){
      if (datatpcons == 0){
	if (stacons !=  2) HOTPLANEST[datatpcons][stacons] = 5;
	else  HOTPLANEST[datatpcons][stacons] = 3;                     //datatype 0;      
      } 
      else if (datatpcons == 1) HOTPLANEST[datatpcons][stacons] = 0.3; //datatype 1; 
      else HOTPLANEST[datatpcons][stacons] = 1.0;                      //datatype 2; 
      for (int octcons = 0; octcons<NUMOCTANT; octcons++){
	for (int hfoctcons = 0; hfoctcons<NUMHALFOCT; hfoctcons++){
	  QPEAKRANGEMINST[datatpcons][stacons][octcons][hfoctcons] = 1;
	  if (datatpcons == 0) {
	    if (stacons == 0) QPEAKRANGEMAXST[datatpcons][stacons][octcons][hfoctcons] = 1100;         
	    else if (stacons == 1) QPEAKRANGEMAXST[datatpcons][stacons][octcons][hfoctcons] = 1050;      
	    else  QPEAKRANGEMAXST[datatpcons][stacons][octcons][hfoctcons] = 850;}                    //datatype 0;
	  if (datatpcons == 1) {
	    if (stacons == 0) QPEAKRANGEMAXST[datatpcons][stacons][octcons][hfoctcons] = 1100;
	    else if (stacons == 1) QPEAKRANGEMAXST[datatpcons][stacons][octcons][hfoctcons] = 1100;
	    else QPEAKRANGEMAXST[datatpcons][stacons][octcons][hfoctcons] = 900;}                     //datatype 1;
	  if (datatpcons == 2) {
	    if (stacons == 0) QPEAKRANGEMAXST[datatpcons][stacons][octcons][hfoctcons] = 1100;
	    else if (stacons == 1) QPEAKRANGEMAXST[datatpcons][stacons][octcons][hfoctcons] = 1100;
	    else QPEAKRANGEMAXST[datatpcons][stacons][octcons][hfoctcons] = 900;}                     //datatype 2;
	  
	}
      }
    }
  }

// determine datatype from the runnumber
  Int_t datatype = 0; // assume we're dealing with gold
  if(runNumber < 35000)
     { // Au-Au Run2
       datatype = 0;
     }
  else if ( (runNumber > 35000) && (runNumber < 45000) )
    { // pp Run-2, with some margin around start and stop
      datatype = 1;
    }
  else if ( (runNumber > 60000) && (runNumber < 80500) )
    { // dAu Run-3, with some margin around start and stop
      datatype = 2;
    }
  else if ( (runNumber > 80500) && (runNumber < 92500) )
    { // pp Run-3, with some margin around start and stop
      // same values as for Run-2, for now
      datatype = 1;
    }
  else if ( (runNumber > 100000) && (runNumber < 126000) )
    { // Au-Au Run-4, with some margin around start and stop
      // same values as for Run-2, for now
      datatype = 0;
    }
  else if ( (runNumber > 126000) && (runNumber < 140000) )
    { // pp Run-4, with some margin around start and stop
      // same values as for Run-2, for now
      datatype = 1;
    }
  else
    {
      cerr << " histMut. - unknown run period; will use"
	   << " default Run-2 AuAu limit values " << endl; 
      datatype = 0;
    }
  
  // histogram objects
  TH1F *MutNumCathClustersSt[NUMARM][NUMSTATION];
  //TH1F *MutCathClustQFitSt[NUMARM][NUMSTATION];
  TH1F *MutCathClustQPeakSt[NUMARM][NUMSTATION][NUMOCTANT][NUMHALFOCT];
  TProfile *MutHitsPerPlaneSt[NUMARM][NUMSTATION];
  
  TH1F *MutCathClustWidth[NUMARM];
  //TH1F *MutCathClustQFit[NUMARM];
  TH1F *MutCathClustQPeak[NUMARM];
  TH1F *MutNumTracks[NUMARM];
  TH1F *MutTrackMom[NUMARM];
  TProfile *MutPulseSamples[NUMARM];

  TProfile *MutHitsPerPacketArm[NUMARM];
  TProfile *MutAmuErrorPerPacketArm[NUMARM];

  
  // check if Mutr objects are into the qa file
  if (!qafile->Get("MutNumCathClustersSt[0][0]"))
    {
      textFile << "Mutr histograms don't exist." 
	       << "Status set to 2" << endl;
      statusFile << 2 << " " << 2 << " ";
      return;
    }
  
  // loop over our two arms 
  for (int arm = 0; arm < NUMARM; arm++)
    { 
      char id[128];
      int sta, oct, halfoct, totalp, addr;
      // arm and sta histograms
      for (sta = 0; sta<NUMSTATION; sta++)
	{
	  sprintf(id,"MutNumCathClustersSt[%d][%d]", arm, sta);
	  MutNumCathClustersSt[arm][sta] = (TH1F *)qafile->Get(id);
	  //  sprintf(id,"MutCathClustQFitSt[%d][%d]", arm, sta);
	  // MutCathClustQFitSt[arm][sta] = (TH1F *)qafile->Get(id);
	  for (oct = 0; oct<NUMOCTANT; oct++){
	    for (halfoct = 0; halfoct<NUMHALFOCT; halfoct++){
	      sprintf(id,"MutCathClustQPeakSt[%d][%d][%d][%d]", arm, sta, oct, halfoct);
	      MutCathClustQPeakSt[arm][sta][oct][halfoct] = (TH1F *)qafile->Get(id);
	    }	  
	  }
	  sprintf(id,"MutHitsPerPlaneSt[%d][%d]", arm, sta);
	  MutHitsPerPlaneSt[arm][sta] = (TProfile *)qafile->Get(id);
	}
      // arm histograms
      sprintf(id,"MutTrackMom[%d]", arm);
      MutTrackMom[arm] = (TH1F *)qafile->Get(id);
      sprintf(id,"MutCathClustWidth[%d]", arm);
      MutCathClustWidth[arm] = (TH1F *)qafile->Get(id);
      // sprintf(id,"MutCathClustQFit[%d]", arm);
      //MutCathClustQFit[arm] = (TH1F *)qafile->Get(id);
      sprintf(id,"MutCathClustQPeak[%d]", arm);
      MutCathClustQPeak[arm] = (TH1F *)qafile->Get(id);
      sprintf(id,"MutNumTracks[%d]", arm);
      MutNumTracks[arm] = (TH1F *)qafile->Get(id);
      sprintf(id,"MutPulseSamples[%d]", arm);
      MutPulseSamples[arm] = (TProfile *)qafile->Get(id);
      
      // TF1 *qfitfunc[NUMSTATION];
      TF1 *qpeakfunc[NUMSTATION][NUMOCTANT][NUMHALFOCT];
      for (sta = 0; sta<NUMSTATION; sta++)
	{
	  // sprintf(id,"qfitfunc[%d]", sta);
	  // qfitfunc[sta] = new TF1(id,"landau",QFITRANGEMINST[datatype][sta],
	  //		      QFITRANGEMAXST[datatype][sta]);
	  for (oct = 0; oct<NUMOCTANT; oct++){
	    for (halfoct = 0; halfoct<NUMHALFOCT; halfoct++){
	      sprintf(id,"qpeakfunc[%d][%d][%d]", sta, oct, halfoct);
	      qpeakfunc[sta][oct][halfoct] = new TF1(id,"landau",QPEAKRANGEMINST[datatype][sta][oct][halfoct], QPEAKRANGEMAXST[datatype][sta][oct][halfoct]);
	    }
	  }
	}

      sprintf(id,"MutHitsPerPacketArm[%d]", arm);
      MutHitsPerPacketArm[arm] = (TProfile *)qafile->Get(id);
      sprintf(id,"MutAmuErrorPerPacketArm[%d]", arm);
      MutAmuErrorPerPacketArm[arm] = (TProfile *)qafile->Get(id);
      
      Int_t mutrstatus = 0;
      Float_t meanp, meancathclustST[NUMSTATION];
      Float_t meancluswid, meanqfit,meanqpeak,meannumtracks;
      Float_t notrack,nevt,percnotrack,percwithtrack;
      Float_t samp[NUMSAMPLES];
      Double_t params[NUMLANDAU] = {0.0};
      Double_t returned_params[NUMLANDAU] = {0.0};
      // Float_t qfitMPVST[NUMSTATION] = {0};
      Float_t qpeakMPVST[NUMSTATION][NUMOCTANT][NUMHALFOCT]; 
      Int_t numhotST[NUMSTATION] = {0};
      Int_t ndeadST[NUMSTATION] = {0};
      Int_t howmanyplanes;
      Float_t hitavg;
      Int_t hotST[NUMSTATION][MAXTOTPLANE][NUMADDR];
      Int_t nohitST[NUMSTATION][MAXTOTPLANE][NUMADDR];
      Int_t plane;
      Int_t i;
      
      for (sta = 0; sta<NUMSTATION; sta++){
	for (oct = 0; oct<NUMOCTANT; oct++){
	  for (halfoct = 0; halfoct<NUMHALFOCT; halfoct++){ 
	    qpeakMPVST[sta][oct][halfoct] = 0.0;
	  }
	}
	for (totalp = 0; totalp<MAXTOTPLANE; totalp++){
	  for (addr = 0; addr<NUMADDR; addr++){ 
	    hotST[sta][totalp][addr] = 0;
	    nohitST[sta][totalp][addr] = 0;
	  }
	}
      }
      
      for (sta = 0; sta<NUMSTATION; sta++)
	{
	  //  qfitfunc[sta]->SetParameters(params);
	  // MutCathClustQFitSt[arm][sta]->Fit(qfitfunc[sta],"RQN");
	  //qfitfunc[sta]->GetParameters(returned_params);
	  //qfitMPVST[sta] = returned_params[1];
	  for (oct = 0; oct<NUMOCTANT; oct++){
	    for (halfoct = 0; halfoct<NUMHALFOCT; halfoct++){ 
	      qpeakfunc[sta][oct][halfoct]->SetParameters(params);
	      MutCathClustQPeakSt[arm][sta][oct][halfoct]->Fit(qpeakfunc[sta][oct][halfoct],"RQN");
	      qpeakfunc[sta][oct][halfoct]->GetParameters(returned_params);
	      qpeakMPVST[sta][oct][halfoct] = returned_params[1];
	    }
	  }
	  howmanyplanes = MutHitsPerPlaneSt[arm][sta]->GetNbinsX();
	  for (i=1; i<=howmanyplanes; i++)
	    {
	      hitavg = MutHitsPerPlaneSt[arm][sta]->GetBinContent(i);
	      
	      /* cout << " arm " << arm 
		 << " sta " << sta
		 << " i " << i
		 << " hitavg " << hitavg
		 << endl;
	      */
	      if (hitavg > HOTPLANEST[datatype][sta])  
		{
		  oct = (int)((i-1)/NUMPLANEST[sta]);
		  plane = (i-1) - oct*NUMPLANEST[sta];
		  hotST[sta][numhotST[sta]][0] = oct;
		  hotST[sta][numhotST[sta]][1] = plane;
		  numhotST[sta]++;
		}
	      if(hitavg == 0)
		{
		  oct = (int)((i-1)/NUMPLANEST[sta]);
		  plane = (i-1) - oct*NUMPLANEST[sta];
		  nohitST[sta][ndeadST[sta]][0] = oct;
		  nohitST[sta][ndeadST[sta]][1] = plane;
		  ndeadST[sta]++;
		}
	    }
	  meancathclustST[sta] = MutNumCathClustersSt[arm][sta]->GetMean();
	}   
      // end of loop over stations 
      
      meanp = MutTrackMom[arm]->GetMean();
      meancluswid = MutCathClustWidth[arm]->GetMean();
      // In Run4 Au-Au, we don't check QFIT information, set defallut 70 and do not change the original set up of status words. /quhai 
      meanqfit = 70;
      meanqpeak = MutCathClustQPeak[arm]->GetMean();
      meannumtracks = MutNumTracks[arm]->GetMean(); 
      notrack = MutNumTracks[arm]->GetBinContent(1);
      nevt = MutNumTracks[arm]->GetEntries();
      samp[0] = MutPulseSamples[arm]->GetBinContent(1);
      samp[1] = MutPulseSamples[arm]->GetBinContent(6);
      samp[2] = MutPulseSamples[arm]->GetBinContent(7);
      samp[3] = MutPulseSamples[arm]->GetBinContent(8);
      
      if(nevt!=0)
	{
	  percnotrack = notrack/nevt;
	}
      else 
	{
	  percnotrack = 1.0;
	}
      percwithtrack = 100*(1 - percnotrack); 


      //check paket related
      Int_t howmanypacks, namuerr,  numhotp, numdeadp;
      Float_t amuerrpack, hitavgp;
      Int_t wrnpack[MAXNUMPACKET]={0};
      Int_t hotp[MAXNUMPACKET] = {0};
      Int_t deadp[MAXNUMPACKET] = {0};
      namuerr = 0;
      numhotp = 0;
      numdeadp = 0;
     
      howmanypacks = MutHitsPerPacketArm[arm]->GetNbinsX();
	  for (i=1; i<=howmanypacks; i++)
	    {
	      amuerrpack = MutAmuErrorPerPacketArm[arm]->GetBinContent(i);
		      
	      if (amuerrpack >= 0.1){
		namuerr++;
		wrnpack[i-1] = 1;
	      }
	      hitavgp = MutHitsPerPacketArm[arm]->GetBinContent(i);
	      
	      // cout << " arm " << arm 
	      //	<< " i " << i
	      //	   << " hitavg per packet " << hitavgp
	      //	   << endl;
	     
	       if (arm == 0){
		 if (i <= 40){ 
		   if (hitavgp > HOTPACKST[datatype][0]){
		     numhotp++;
		     hotp[i-1] = 1;
		   }
		 }
		 else if (i <= 104) {
		   if (hitavgp > HOTPACKST[datatype][1]){
		     numhotp++;
		     hotp[i-1] = 1;
		   }
		 }
		 else {
		   if (hitavgp > HOTPACKST[datatype][2]){
		     numhotp++;
		     hotp[i-1] = 1;
		   }
		 }
	       }
	       else {
		 if (i <= 40){ 
		   if (hitavgp > HOTPACKST[datatype][0]){
		     numhotp++;
		     hotp[i-1] = 1;
		   }
		 }
		 else if (i <= 112) {
		   if (hitavgp > HOTPACKST[datatype][1]){
		     numhotp++;
		     hotp[i-1] = 1;
		   }
		 }
		 else {
		   if (hitavgp > HOTPACKST[datatype][2]){
		     numhotp++;
		     hotp[i-1] = 1;
		   }
		 }
	       }
	       if(hitavgp == 0)
		 {
		   numdeadp++;
		   deadp[i-1] = 1; 
		 }
	       
	    }
      
      // ok, now we have all the info, start filling in the error code
      // first a check on the things above station level (i.e. for the whole arm)
      if ( samp[2] < PULSEHEIGHTMIN[datatype] ) 
	mutrstatus = mutrstatus|4;
      if ( (samp[2]-samp[1])<0 || (samp[2]-samp[3])<0 ) 
	mutrstatus = mutrstatus|8;
      
      if ( meancluswid < CLUSWIDMIN || meancluswid > CLUSWIDMAX )
	mutrstatus = mutrstatus|16;
      if ( meanqfit < CLUSQFITMIN[datatype] || meanqfit > CLUSQFITMAX[datatype])  
	mutrstatus = mutrstatus|32;
      if ( meanqpeak < CLUSQPEAKMIN[datatype] || meanqpeak > CLUSQPEAKMAX[datatype] )  
	mutrstatus = mutrstatus|64;
      
      // now we move on to the station level
      Int_t status_bit = 128;
      for (sta = 0; sta<NUMSTATION; sta++)
	{
	  if ( meancathclustST[sta] <= CLUSMINST[sta] || 
	       (meancathclustST[sta] >= CLUSMAXST[sta]) )  
	    {
	      mutrstatus = mutrstatus|status_bit;
	    }
	  status_bit *= 2;
	}
      for (sta = 0; sta<NUMSTATION; sta++)
	{
	  if ( ndeadST[sta] >= NNOHITST[sta] || numhotST[sta]>=NHOTST[sta]) 
	    {
	      mutrstatus = mutrstatus|status_bit;
	    }
	  status_bit *= 2;
	}
      // back to above station level checks
      if (percwithtrack <= NUMTRKMIN || percwithtrack >= NUMTRKMAX)  
	mutrstatus = mutrstatus|8192;
      if (meanp < PTOTMIN || meanp > PTOTMAX)  
	mutrstatus = mutrstatus|16384;


      //get to packet level check
      if (namuerr > 10) mutrstatus = mutrstatus|32768;
      if (arm == 0){		
	if (numhotp>42 || numdeadp>21) mutrstatus = mutrstatus|65536;
      }       
      else {
	if (numhotp>48 || numdeadp>24) mutrstatus = mutrstatus|65536;
      }
      
     
      if(mutrstatus != 0) mutrstatus = mutrstatus|1; // overall status bit
      
      textFile << "MUTR - arm " << arm << endl;
      textFile << "MUTR mean pulse sample 3 amplitude =  " << samp[2] << endl;
      textFile << "MUTR pulse timing: samp3-samp2 = " << samp[2]-samp[1]
	       << "  samp3-samp4 = " << samp[2]-samp[3] << endl;
      textFile << "MUTR mean cathode cluster width =  " << meancluswid<<endl;
      // textFile << "MUTR mean cathode cluster fitted charge =  " << meanqfit<<endl;
      textFile << "MUTR mean cathode cluster peak charge =  " << meanqpeak<<endl;
      textFile << "MUTR mean number fitted cathode clusters (station 1) = " 
	       << meancathclustST[0] << endl;
      textFile << "MUTR mean number fitted cathode clusters (station 2) = " 
	       << meancathclustST[1] << endl;
      textFile << "MUTR mean number fitted cathode clusters (station 3) = " 
	       << meancathclustST[2] << endl;
      textFile << "MUTR Sta. 1: # of hot planes = " << numhotST[0] 
	       << " # dead planes = " << ndeadST[0]  << endl;
      textFile << "MUTR Sta. 2: # of hot planes = " << numhotST[1] 
	       << " # dead planes = " << ndeadST[1] << endl;
      textFile << "MUTR Sta. 3: # of hot planes = " << numhotST[2] 
	       << " # dead planes = " << ndeadST[2]  << endl;
      textFile << "MUTR percentage of events with fitted tracks = " << percwithtrack << " %" <<endl;
      textFile << "MUTR mean total momentum of fitted tracks =  " << meanp << " GeV/c" << endl;
      textFile <<  "MUTR number packets with cell error  rate > 10% = " << namuerr << endl; 
      textFile <<  "MUTR number of hot packets = " << numhotp << endl; 

      textFile <<  "MUTR number of dead packets = " << numdeadp << endl; 
 
      //  textFile <<  "MUTR station 1 qfit landau MPV = " << qfitMPVST[0] << endl;
      // textFile <<  "MUTR station 2 qfit landau MPV = " << qfitMPVST[1] << endl;
      // textFile <<  "MUTR station 3 qfit landau MPV = " << qfitMPVST[2] << endl;
      for (sta = 0; sta<NUMSTATION; sta++){
	for (oct = 0; oct<NUMOCTANT; oct++){
	  for (halfoct = 0; halfoct<NUMHALFOCT; halfoct++){  
	    textFile <<  "MUTR arm " << arm << "  station  "<< sta << "  octant   " << oct << "  halfoctant  " << halfoct << " qpeak landau MPV = " << qpeakMPVST[sta][oct][halfoct] << endl;
	  }
	}
      }
      textFile << endl;
      // Station by station reporting
      // hot
      for (sta = 0; sta<NUMSTATION; sta++)
	{
	  if (numhotST[sta]>0)
	    {
	      textFile <<  "List of hot planes on station " << sta+1 << ":" << endl;
	      textFile << "station   octant   plane" << endl;
	      textFile << "-------   ------   -----" << endl;
	      for(i=0;i<numhotST[sta];i++)
		{
		  textFile<<"   " << sta
			  <<"         " << hotST[sta][i][0]
			  <<"       " << hotST[sta][i][1]<<endl;
		}
	    }
	  else
	    { 
	      textFile << "No hot planes on station " << sta+1 << ":" << endl;
	    }
	}
      
      // dead
      for (sta = 0; sta<NUMSTATION; sta++)
	{
	  if (ndeadST[sta]>0)
	    {
	      textFile <<  "List of dead planes on station " << sta+1 << ":" << endl;
	      textFile << "station   octant   plane" << endl;
	      textFile << "-------   ------   -----" << endl;
	      for(i=0;i<ndeadST[sta];i++)
		{
		  textFile<<"   " << sta
			  <<"         " << nohitST[sta][i][0]
			  <<"       " << nohitST[sta][i][1]<<endl;
		}
	    }
	  else
	    { 
	      textFile << "No dead planes on station " << sta+1 << ":" << endl;
	    }
	}

      textFile << endl;
      // packet by packet reporting
      // amu cell error
      if (namuerr > 0){
	textFile <<  "List of wrong packets with error > 10% "<< endl;
	for (i = 0; i<=howmanypacks; i++)
	  {
	    if (wrnpack[i]>0)
	      {
		if (arm == 0)textFile << " packet " << 11001+i << endl;
		else textFile << " packet " << 11171+i << endl;
	      }
	 
	  }
	}
      else
	{ 
	  textFile << "No wrong packets for amu cell difference check." << endl;
	}


      // hot and dead packets reporting

      if ( numhotp > 0){
	textFile <<  "List of hot packets "<< endl;
	for (i = 0; i<=howmanypacks; i++)
	  {
	    if (hotp[i]>0)
	      {
		if (arm == 0)textFile << " packet " << 11001+i << endl;
		else textFile << " packet " << 11171+i << endl;
	      }
	  }
      }
      else textFile << "No hot packets for this arm." << endl;
     
      //dead
      if ( numdeadp > 0){
	textFile <<  "List of dead packets "<< endl;
	for (i = 0; i<=howmanypacks; i++)
	  {
	    if (deadp[i]>0)
	      {
		if (arm == 0)textFile << " packet " << 11001+i << endl;
		else textFile << " packet " << 11171+i << endl;
	      }
	  }
	}
      else  textFile << "No dead packets for this arm." << endl;

    
      // output summary info
      textFile << "MUTR arm " << arm << " status = " << mutrstatus;
      statusFile << mutrstatus << " ";
      
      textFile << " = (Bitwise) ";
      for(Int_t bit=NUMSTATBITS-1; bit>=0; bit--){
	status_bit = mutrstatus & (0x1<<bit);
	textFile << (status_bit>>bit);
      }
      textFile << endl;
             
    } // arm


}//EOF
