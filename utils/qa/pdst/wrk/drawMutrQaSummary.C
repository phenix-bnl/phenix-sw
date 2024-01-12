#include <fstream>
#include <iostream>

ofstream textFile;
ofstream statusFile;

void drawMutrQaSummary()
{
  TFile *qafile = new TFile("qa.root","READ");
  textFile.open("qaMutr.txt");
  statusFile.open("qaMutrStatus.txt");
  
  
  textFile << " ----------------------------------------------------" << endl;
  textFile << " -- MUTR QA Summary --" << endl;
  textFile << " ----------------------------------------------------" << endl;
  
  // define a bunch of constants
  const int NUMARM      = 2;
  const int NUMSTATION  = 3;
  const int NUMOCTANT   = 8;
  const int NUMHALFOCT  = 2;
  const int NUMGAPST[NUMSTATION] = { 3, 3, 2};
  const int NUMPLANEST[NUMSTATION] = { 6, 6, 4};
  const int TOTPLANEST[NUMSTATION] = { 48, 48, 32};
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
  
  //#define NNOHITSTX TOTPLANESSTX/8
  const int NNOHITST[NUMSTATION] = { 6, 6, 4 }; 
  //#define NHOTSTX TOTPLANESSTX/4
  const int NHOTST[NUMSTATION] = { 12, 12, 8 }; 
  
  // datatypes: 0 = Run2 AuAu, 1 = Run2(&3) pp, 2 = Run3 dAu 
  // dA values are set at about the mean between AA and pp for now..
  const int NDATATYPES = 3;
  const int CLUSQFITMIN[NDATATYPES] = { 50, 30, 40 };
  const int CLUSQFITMAX[NDATATYPES] = { 100, 100, 100 };
  
  const int CLUSQPEAKMIN[NDATATYPES] = { 30, 20, 25 };
  const int CLUSQPEAKMAX[NDATATYPES] = { 90, 90, 90 };
  
  const int PULSEHEIGHTMIN[NDATATYPES] = { 15, 2, 8 };
  // packet
  const int MAXNUMPACKET = 192;
  const int TOTPACKETARM[NUMARM] = { 168, 192};

  
  
  // note that ordering of an array starts with last index, i.e
  // int test[2][2] = { 0, 1, 2, 3};
  // cout << test[0][1] << endl; - gives 1
  // cout << test[1][0] << endl; - gives 2
  // Thus [NDATATYPES][NUMSTATION] means that the first 3 values
  // are for the first datatype (one for each station), and so on
  const int QFITRANGEMINST[NDATATYPES][NUMSTATION] = { 1, 1, 1,  // datatype 0 
						       1, 1, 1,  // datatype 1
						       1, 1, 1 };// datatype 2 
  const int QFITRANGEMAXST[NDATATYPES][NUMSTATION] = { 125, 125, 125,  // datatype 0 
						       130, 130, 120,  // datatype 1
						       125, 125, 125 };// datatype 2 
  
  const int QPEAKRANGEMINST[NDATATYPES][NUMSTATION] = { 1, 1, 1,  // datatype 0 
							1, 1, 1,  // datatype 1
							1, 1, 1 };// datatype 2 
  const int QPEAKRANGEMAXST[NDATATYPES][NUMSTATION] = { 110, 105, 85,  // datatype 0 
							110, 110, 90,  // datatype 1
							110, 110, 90 };// datatype 2 
  
  const int HOTPLANEST[NDATATYPES][NUMSTATION] = { 5, 5, 3,  // datatype 0 
						   0.1, 0.1, 0.1,  // datatype 1
						   1.0, 1.0, 1.0 };// datatype 2 
  // determine datatype from the runnumber
  Int_t datatype = 0; // assume we're dealing with gold
  
  Int_t runNumber = 61000;


  
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
  else
    {
      /*
	cout << " histMut. - unknown run period; will use"
	<< " default Run-2 AuAu limit values " << endl; 
      */
      printf( " histMut. - unknown run period; will use \n",
	      " default Run-2 AuAu limit values\n ") ; 
      datatype = 0;
    }
  
  // histogram objects
  TH1F *MutNumCathClustersSt[NUMARM][NUMSTATION];
  TH1F *MutCathClustQFitSt[NUMARM][NUMSTATION];
  TH1F *MutCathClustQPeakSt[NUMARM][NUMSTATION][NUMOCTANT][NUMHALFOCT];
  TProfile *MutHitsPerPlaneSt[NUMARM][NUMSTATION];
  
  TH1F *MutCathClustWidth[NUMARM];
  TH1F *MutCathClustQFit[NUMARM];
  TH1F *MutCathClustQPeak[NUMARM];
  TH1F *MutNumTracks[NUMARM];
  TH1F *MutTrackMom[NUMARM];
  TProfile *MutPulseSamples[NUMARM];

  // add hits per FEM paket
  TProfile *MutHitsPerPacketArm[NUMARM];
  TProfile *MutAmuErrorPerPacketArm[NUMARM];
  
// Coordinate for radiograph
  TH2F *MutHitCoord[2][3][3];


  // Create CANVAS for two arms
  TCanvas *mutNumClusterCS = new TCanvas("mutNumClusterCS", "South Mutr Number of Fitted Cathode Strip Clusters", 120,10,990,820);
  mutNumClusterCS->Divide(1,3);
  
  TCanvas *mutNumClusterCN = new TCanvas("mutNumClusterCN", "North Mutr Number of Fitted Cathode Strip Clusters", 120,10,990,820);
  mutNumClusterCN->Divide(1,3);

  
  TCanvas *mutCathClustQPeakCNS1 = new TCanvas("mutCathClustQPeakCNS1", "North Mutr Station 1 Cathode Cluster Q Peak", 120,10,990,820);
  mutCathClustQPeakCNS1->Divide(4,4);
  
  TCanvas *mutCathClustQPeakCNS2 = new TCanvas("mutCathClustQPeakCNS2", "North Mutr Station 2 Cathode Cluster Q Peak", 120,10,990,820);
  mutCathClustQPeakCNS2->Divide(4,4);
  
  TCanvas *mutCathClustQPeakCNS3 = new TCanvas("mutCathClustQPeakCNS3", "North Mutr Station 3 Cathode Cluster Q Peak", 120,10,990,820);
  mutCathClustQPeakCNS3->Divide(4,4);
  
 
   
  TCanvas *mutCathClustQPeakCSS1 = new TCanvas("mutCathClustQPeakCSS1", "South Mutr Station 1 Cathode Cluster Q Peak", 120,10,990,820);
  mutCathClustQPeakCSS1->Divide(4,4);
  
  TCanvas *mutCathClustQPeakCSS2 = new TCanvas("mutCathClustQPeakCSS2", "South Mutr Station 2 Cathode Cluster Q Peak", 120,10,990,820);
  mutCathClustQPeakCSS2->Divide(4,4);
  
  TCanvas *mutCathClustQPeakCSS3 = new TCanvas("mutCathClustQPeakCSS3", "South Mutr Station 3 Cathode Cluster Q Peak", 120,10,990,820);
  mutCathClustQPeakCSS3->Divide(4,4);
  

  TCanvas *mutRadioN = new TCanvas("mutRadioN", "North Mutr Radiograph", 120,10,990,820);
  mutRadioN->Divide(3,3);
  
  TCanvas *mutRadioS = new TCanvas("mutRadioS", "South Mutr Radiograph", 120,10,990,820);
  mutRadioS->Divide(3,3);


  TCanvas *mutCathClustQPeakArmCS = new TCanvas("mutCathClustQPeakArmCS", "South Mutr QPeak");
  
  TCanvas *mutCathClustQPeakArmCN = new TCanvas("mutCathClustQPeakArmCN", "North Mutr QPeak");

  TCanvas *mutCathClustWidthCS = new TCanvas("mutCathClustWidthCS", "South Mutr ClustersWidth");
  
  TCanvas *mutCathClustWidthCN = new TCanvas("mutCathClustWidthCN", "North Mutr ClustersWidth");
  
  TCanvas *mutNumTrackCS = new TCanvas("mutNumTrackCS", "South Mutr Number of Tracks");
  
  TCanvas *mutNumTrackCN = new TCanvas("mutNumTrackCN", "North Mutr Number of Tracks");
  
  TCanvas *mutTrackMomCS = new TCanvas("mutTrackMomCS", "South Mutr Momentum of Tracks");
  
  TCanvas *mutTrackMomCN = new TCanvas("mutTrackMomCN", "North Mutr Momentum of Tracks");

 
  TCanvas *mutNumHitsPakCS = new TCanvas("mutNumHitsPakCS", "South Mutr Number of Hits Per Packet");
  
  TCanvas *mutNumHitsPakCN = new TCanvas("mutNumHitsPakCN", "North Mutr Number of Hits Per Packet");
  
  TCanvas *mutAmuErrorCS = new TCanvas("mutAmuErrorCS", "South Mutr Amu Error Per Packet");
  
  TCanvas *mutAmuErrorCN = new TCanvas("mutAmuErrorCN", "North Mutr Amu Error Per Packet");
  

  TCanvas *mutHitsCS = new TCanvas("mutHitsCS", "South Mutr Hits", 120,10,990,820);
  mutHitsCS->Divide(1,3);
  
  TCanvas *mutHitsCN = new TCanvas("mutHitsCN", "North Mutr Hits", 120,10,990,820);
  mutHitsCN->Divide(1,3);
  
  TCanvas *mut4SamCS = new TCanvas("mut4SamCS", "South Mutr 4 sample pulse");
  
  TCanvas *mut4SamCN = new TCanvas("mut4SamCN", "North Mutr 4 sample pulse");

  
  // loop over our two arms 
  for (int arm = 0; arm < NUMARM; arm++)
    { 
      char id[128];
      int sta;
      int numr = 0;
      // arm and sta histograms
      for (sta = 0; sta<NUMSTATION; sta++)
	{
	  sprintf(id,"MutNumCathClustersSt[%d][%d]", arm, sta);
	  MutNumCathClustersSt[arm][sta] = (TH1F *)qafile->Get(id);
	  if (arm ==0) {
	    mutNumClusterCS->cd(sta + 1); 
	    MutNumCathClustersSt[arm][sta]->Draw();
	  } else {
	    mutNumClusterCN->cd(sta + 1); 
	    MutNumCathClustersSt[arm][sta]->Draw();
	  }
	  
	  // sprintf(id,"MutCathClustQFitSt[%d][%d]", arm, sta);
	  // MutCathClustQFitSt[arm][sta] = (TH1F *)qafile->Get(id);
	  int num = 0;
	
	  if (sta == 0){
	  for (int oct = 0; oct < NUMOCTANT; oct++)
	    { 
	      for (int halfoct = 0; halfoct < NUMHALFOCT; halfoct++)
		{ 
		  num++;
		  sprintf(id,"MutCathClustQPeakSt[%d][%d][%d][%d]", arm, sta, oct, halfoct);
		  MutCathClustQPeakSt[arm][sta][oct][halfoct] = (TH1F *)qafile->Get(id);
		  if (arm ==0) {
		    mutCathClustQPeakCSS1->cd(num);
		    MutCathClustQPeakSt[arm][sta][oct][halfoct]->Draw();
		  } else {
		    mutCathClustQPeakCNS1->cd(num);
		    MutCathClustQPeakSt[arm][sta][oct][halfoct]->Draw();
		  }
		}
	    }
	  }

	  if (sta == 1){
	    for (int oct = 0; oct < NUMOCTANT; oct++)
	      { 
	      for (int halfoct = 0; halfoct < NUMHALFOCT; halfoct++)
		{ 
		  num++;
		  sprintf(id,"MutCathClustQPeakSt[%d][%d][%d][%d]", arm, sta, oct, halfoct);
		  MutCathClustQPeakSt[arm][sta][oct][halfoct] = (TH1F *)qafile->Get(id);
		  if (arm ==0) {
		    mutCathClustQPeakCSS2->cd(num);
		    MutCathClustQPeakSt[arm][sta][oct][halfoct]->Draw();
		  } else {
		    mutCathClustQPeakCNS2->cd(num);
		    MutCathClustQPeakSt[arm][sta][oct][halfoct]->Draw();
		  }
		}
	    }
	  }

	  if (sta == 2){
	    for (int oct = 0; oct < NUMOCTANT; oct++)
	      { 
		for (int halfoct = 0; halfoct < NUMHALFOCT; halfoct++)
		  { 
		    num++;
		    sprintf(id,"MutCathClustQPeakSt[%d][%d][%d][%d]", arm, sta, oct, halfoct);
		    MutCathClustQPeakSt[arm][sta][oct][halfoct] = (TH1F *)qafile->Get(id);
		    if (arm ==0) {
		      mutCathClustQPeakCSS3->cd(num);
		      MutCathClustQPeakSt[arm][sta][oct][halfoct]->Draw();
		    } else {
		      mutCathClustQPeakCNS3->cd(num);
		      MutCathClustQPeakSt[arm][sta][oct][halfoct]->Draw();
		    }
		  }
	      }
	  }
	  
	  sprintf(id,"MutHitsPerPlaneSt[%d][%d]", arm, sta);
	  MutHitsPerPlaneSt[arm][sta] = (TProfile *)qafile->Get(id);
	  if (arm ==0) {
	    mutHitsCS->cd(sta + 1);
	    MutHitsPerPlaneSt[arm][sta]->Draw();
	  } else {
	    mutHitsCN->cd(sta + 1);
	    MutHitsPerPlaneSt[arm][sta]->Draw();
	  }

	  	  for (int gap=0; gap<3; gap++ ) {
            
	    numr++;
	    sprintf(id,"MutHitCoord[%d][%d][%d]",arm,sta,gap);
            cout << "id is : " << id << endl;
	    MutHitCoord[arm][sta][gap] = (TH2F *)qafile->Get(id);
                 if ( sta==2 && gap==2 ) continue;
                   if ( arm ==0 ) {
		     mutRadioS->cd(numr);
		     MutHitCoord[arm][sta][gap]->Draw();
		    }
		   else {
		     mutRadioN->cd(numr);
		     MutHitCoord[arm][sta][gap]->Draw();
		   }
		   
		   }
	}

   
      // arm histograms

      sprintf(id,"MutHitsPerPacketArm[%d]", arm);
      MutHitsPerPacketArm[arm] = (TProfile *)qafile->Get(id);
      if (arm ==0) {
	mutNumHitsPakCS->cd();
	MutHitsPerPacketArm[arm]->Draw();
      } else {
	mutNumHitsPakCN->cd();
	MutHitsPerPacketArm[arm]->Draw();
      }

      sprintf(id,"MutAmuErrorPerPacketArm[%d]", arm);
      MutAmuErrorPerPacketArm[arm] = (TProfile *)qafile->Get(id);
      if (arm ==0) {
	mutAmuErrorCS->cd();
	MutAmuErrorPerPacketArm[arm]->Draw();
      } else {
	mutAmuErrorCN->cd();
	MutAmuErrorPerPacketArm[arm]->Draw();
      }
  

      sprintf(id,"MutCathClustQPeak[%d]", arm);
      MutCathClustQPeak[arm] = (TH1F *)qafile->Get(id);
      if (arm ==0) {
	mutCathClustQPeakArmCS->cd();
	MutCathClustQPeak[arm]->Draw();
      } else {
	mutCathClustQPeakArmCN->cd();
	MutCathClustQPeak[arm]->Draw();
      }
      

      
      sprintf(id,"MutTrackMom[%d]", arm);
      MutTrackMom[arm] = (TH1F *)qafile->Get(id);
      if (arm ==0) {
	mutTrackMomCS->cd();
	MutTrackMom[arm]->Draw();
      } else {
	mutTrackMomCN->cd();
	MutTrackMom[arm]->Draw();
      }
      
      sprintf(id,"MutCathClustWidth[%d]", arm);
      MutCathClustWidth[arm] = (TH1F *)qafile->Get(id);
      if (arm ==0) {
	mutCathClustWidthCS->cd();
	MutCathClustWidth[arm]->Draw();
      } else {
	mutCathClustWidthCN->cd();
	MutCathClustWidth[arm]->Draw();
      }
      
      
      // sprintf(id,"MutCathClustQFit[%d]", arm);
      // MutCathClustQFit[arm] = (TH1F *)qafile->Get(id);
      
          
      sprintf(id,"MutNumTracks[%d]", arm);
      MutNumTracks[arm] = (TH1F *)qafile->Get(id);
      if (arm ==0) {
	mutNumTrackCS->cd();
	MutNumTracks[arm]->Draw();
      } else {
	mutNumTrackCN->cd();
	MutNumTracks[arm]->Draw();
      }
      
      
      sprintf(id,"MutPulseSamples[%d]", arm);
      MutPulseSamples[arm] = (TProfile *)qafile->Get(id);
      if (arm ==0) {
	mut4SamCS->cd();
	MutPulseSamples[arm]->Draw();
      } else {
	mut4SamCN->cd();
	MutPulseSamples[arm]->Draw();
      }
      
      /*      
      
      TF1 *qfitfunc[NUMSTATION];
      TF1 *qpeakfunc[NUMSTATION];
      for (sta = 0; sta<NUMSTATION; sta++)
	{
	  sprintf(id,"qfitfunc[%d]", sta);
	  qfitfunc[sta] = new TF1(id,"landau",QFITRANGEMINST[datatype][sta],
				  QFITRANGEMAXST[datatype][sta]);
	  sprintf(id,"qpeakfunc[%d]", sta);
	  qpeakfunc[sta] = new TF1(id,"landau",QPEAKRANGEMINST[datatype][sta],
				   QPEAKRANGEMAXST[datatype][sta]);
	}
      
      Int_t mutrstatus = 0;
      Float_t meanp, meancathclustST[NUMSTATION];
      Float_t meancluswid, meanqfit,meanqpeak,meannumtracks;
      Float_t notrack,nevt,percnotrack,percwithtrack;
      Float_t samp[NUMSAMPLES];
      Double_t params[NUMLANDAU] = {0};
      Double_t returned_params[NUMLANDAU];
      Float_t qfitMPVST[NUMSTATION] = {0};
      Float_t qpeakMPVST[NUMSTATION] = {0};
      Int_t numhotST[NUMSTATION] = {0};
      Int_t ndeadST[NUMSTATION] = {0};
      Int_t howmanyplanes;
      Float_t hitavg;
      Int_t hotST[NUMSTATION][MAXTOTPLANE][NUMADDR] = {0};
      Int_t nohitST[NUMSTATION][MAXTOTPLANE][NUMADDR] = {0};
      Int_t oct, plane;
      Int_t i;
      
      for (sta = 0; sta<NUMSTATION; sta++)
	{
	  qfitfunc[sta]->SetParameters(params);
	  MutCathClustQFitSt[arm][sta]->Fit(qfitfunc[sta],"RQN");
	  qfitfunc[sta]->GetParameters(returned_params);
	  qfitMPVST[sta] = returned_params[1];
	  
	  qpeakfunc[sta]->SetParameters(params);
	  MutCathClustQPeakSt[arm][sta]->Fit(qpeakfunc[sta],"RQN");
	  qpeakfunc[sta]->GetParameters(returned_params);
	  qpeakMPVST[sta] = returned_params[1];
	  
	  howmanyplanes = MutHitsPerPlaneSt[arm][sta]->GetNbinsX();
	  for (i=1; i<=howmanyplanes; i++)
	    {
	      hitavg = MutHitsPerPlaneSt[arm][sta]->GetBinContent(i);
	     /
		cout << " arm " << arm 
		<< " sta " << sta
		<< " i " << i
		<< " hitavg " << hitavg
		<< endl;
	      //
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
      meanqfit = MutCathClustQFit[arm]->GetMean();
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
      
      if(mutrstatus != 0) mutrstatus = mutrstatus|1; // overall status bit
      
      textFile << "MUTR - arm " << arm << endl;
      textFile << "MUTR mean pulse sample 3 amplitude =  " << samp[2] << endl;
      textFile << "MUTR pulse timing: samp3-samp2 = " << samp[2]-samp[1]
	       << "  samp3-samp4 = " << samp[2]-samp[3] << endl;
      textFile << "MUTR mean cathode cluster width =  " << meancluswid<<endl;
      textFile << "MUTR mean cathode cluster fitted charge =  " << meanqfit<<endl;
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
      textFile <<  "MUTR station 1 qfit landau MPV = " << qfitMPVST[0] << endl;
      textFile <<  "MUTR station 2 qfit landau MPV = " << qfitMPVST[1] << endl;
      textFile <<  "MUTR station 3 qfit landau MPV = " << qfitMPVST[2] << endl;
      textFile <<  "MUTR station 1 qpeak landau MPV = " << qpeakMPVST[0] << endl;
      textFile <<  "MUTR station 2 qpeak landau MPV = " << qpeakMPVST[1] << endl;
      textFile <<  "MUTR station 3 qpeak landau MPV = " << qpeakMPVST[2] << endl;
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
      
      // output summary info
      textFile << "MUTR status = " << mutrstatus;
      statusFile << mutrstatus << " ";
      
      textFile << " = (Bitwise) ";
      for(Int_t bit=NUMSTATBITS-1; bit>=0; bit--){
	status_bit = mutrstatus & (0x1<<bit);
	textFile << (status_bit>>bit);
      }
      textFile << endl;
      */      
    
    } // arm




  mutCathClustQPeakCNS1->SaveAs("qaMutrCathClustQPeakCNS1.gif");
  gSystem->Exec("mv qaMutrCathClustQPeakCNS1.gif qagifs");
  
  mutCathClustQPeakCNS2->SaveAs("qaMutrCathClustQPeakCNS2.gif");
  gSystem->Exec("mv qaMutrCathClustQPeakCNS2.gif qagifs");
  
  mutCathClustQPeakCNS3->SaveAs("qaMutrCathClustQPeakCNS3.gif");
  gSystem->Exec("mv qaMutrCathClustQPeakCNS3.gif qagifs");


  mutCathClustQPeakCSS1->SaveAs("qaMutrCathClustQPeakCSS1.gif");
  gSystem->Exec("mv qaMutrCathClustQPeakCSS1.gif qagifs");
  
  mutCathClustQPeakCSS2->SaveAs("qaMutrCathClustQPeakCSS2.gif");
  gSystem->Exec("mv qaMutrCathClustQPeakCSS2.gif qagifs");
  
  mutCathClustQPeakCSS3->SaveAs("qaMutrCathClustQPeakCSS3.gif");
  gSystem->Exec("mv qaMutrCathClustQPeakCSS3.gif qagifs");

 
  mutRadioN->SaveAs("qaMutrRadioN.gif");
  gSystem->Exec("mv qaMutrRadioN.gif qagifs");

  mutRadioS->SaveAs("qaMutRadioS.gif");
  gSystem->Exec("mv qaMutRadioS.gif qagifs");

  mutCathClustQPeakArmCN->SaveAs("qaMutCathClustQPeakArmN.gif");
  gSystem->Exec("mv qaMutCathClustQPeakArmN.gif qagifs");

  mutCathClustQPeakArmCS->SaveAs("qaMutCathClustQPeakArmS.gif");
  gSystem->Exec("mv qaMutCathClustQPeakArmS.gif qagifs");

  mutNumHitsPakCS->SaveAs("qaMutrNumHitsPakS.gif");
  gSystem->Exec("mv qaMutrNumHitsPakS.gif qagifs");
  
  mutNumHitsPakCN->SaveAs("qaMutrNumHitsPakN.gif");
  gSystem->Exec("mv qaMutrNumHitsPakN.gif qagifs");

  mutAmuErrorCS->SaveAs("qaMutrAmuErrorS.gif");
  gSystem->Exec("mv qaMutrAmuErrorS.gif qagifs");

  mutAmuErrorCN->SaveAs("qaMutrAmuErrorN.gif");
  gSystem->Exec("mv qaMutrAmuErrorN.gif qagifs");


  mutNumClusterCS->SaveAs("qaMutrNumClusterS.gif");
  gSystem->Exec("mv qaMutrNumClusterS.gif qagifs");
  
  mutNumClusterCN->SaveAs("qaMutrNumClusterN.gif");
  gSystem->Exec("mv qaMutrNumClusterN.gif qagifs");
  
 
  
  mutCathClustWidthCS->SaveAs("qaMutCathClustWidthCS.gif");
  gSystem->Exec("mv qaMutCathClustWidthCS.gif qagifs");
  
  mutNumTrackCS->SaveAs("qaMutNumTrackCS.gif");
  gSystem->Exec("mv qaMutNumTrackCS.gif qagifs");
  
  mutTrackMomCS->SaveAs("qaMutTrackMomCS.gif");
  gSystem->Exec("mv qaMutTrackMomCS.gif qagifs");
  
  mutHitsCS->SaveAs("qaMutHitsCS.gif");
  gSystem->Exec("mv qaMutHitsCS.gif qagifs");
  
  mut4SamCS->SaveAs("qaMut4SamCS.gif");
  gSystem->Exec("mv qaMut4SamCS.gif qagifs");
  

  mutCathClustWidthCN->SaveAs("qaMutCathClustWidthCN.gif");
  gSystem->Exec("mv qaMutCathClustWidthCN.gif qagifs");
  
  mutNumTrackCN->SaveAs("qaMutNumTrackCN.gif");
  gSystem->Exec("mv qaMutNumTrackCN.gif qagifs");
  
  mutTrackMomCN->SaveAs("qaMutTrackMomCN.gif");
  gSystem->Exec("mv qaMutTrackMomCN.gif qagifs");
  
  mutHitsCN->SaveAs("qaMutHitsCN.gif");
  gSystem->Exec("mv qaMutHitsCN.gif qagifs");
  
  mut4SamCN->SaveAs("qaMut4SamCN.gif");
  gSystem->Exec("mv qaMut4SamCN.gif qagifs");  
  
  
}//EOF
