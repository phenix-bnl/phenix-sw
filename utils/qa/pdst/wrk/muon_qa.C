/*!
  QA for Muon Arms
  Abhisek Sen, GSU  !*/

#include <iostream>
#include <fstream>
#include <stdio.h>

ofstream summaryFile;

void muon_qa(char* file="/data11/abhisek/run10_62GeV_QAFILES/qa_MWG_run10auau062muon_pro85_312222.root" ,
             char* outpath="/data10/abhisek/run10_62GeV_qaplots/")
 {
   gROOT->SetStyle("Plain");
  // gStyle->SetPalette(1);
  // gStyle->SetOptStat(1);
 //  gStyle->SetOptFit(0);
   //gROOT->ProcessLine(".L draw_wires.C");
   
   TFile *qafile=new TFile(file,"READ");
   if( !qafile ) {
    cout << "QA file missing" << file << endl;
    exit(1);
   }
   
   string FileName = file;
   int length = FileName.size();
   istringstream RUNNUMBER(FileName.substr(length-11,6));
   int runnumber;
   RUNNUMBER >> runnumber;
   cout << "RunNumber " << runnumber << endl ;

   
  // textFile.open("Run10_qa.txt");
   char summaryFileName[128];
   sprintf(summaryFileName,"summary%d",runnumber);
   summaryFile.open(summaryFileName);
   summaryFile << runnumber << endl;  

   cout << "****************************************************"<< endl;
   cout << "                 MUON ARM QA                           "<< endl;
   cout << "****************************************************"<< endl;


  const int NUMARM=2;
  const int NUMSTATION=3;
  const int NUMOCTANT=8;
  const int NUMHALFOCT =2 ;
  const int NUMGAP =3;  //except station 3
  const int NUMPLANEST[NUMSTATION] = { 6, 6, 4 };

  // Double_t HOTPLANE[NUMSTATION]={ 5, 5, 6 };
   Int_t oct;
   Int_t plane;

 //histogram objects
  TH1F *MutNumCathClustersSt[NUMARM][NUMSTATION];
 // TH1F *MutCathClustQFitSt[NUMARM][NUMSTATION];
  TH1F *MutCathClustQPeakSt[NUMARM][NUMSTATION][NUMOCTANT][NUMGAP];
  TProfile *MutHitsPerPlaneSt[NUMARM][NUMSTATION];

  TH1F *MutCathClustWidth[NUMARM][NUMSTATION];
//  TH1F *MutCathClustQFit[NUMARM];
  TH1F *MutCathClustQPeak[NUMARM];
  TH1F *MutNumTracks[NUMARM];
  TH1F *MutTrackMom[NUMARM];
  TH1F *MutAzimuth[NUMARM][NUMSTATION][NUMGAP];
  TH1F *MutRadial[NUMARM][NUMSTATION][NUMGAP];
  //TProfile *MutPulseSamples[NUMARM];
  TH1F* MutTrkChisqNdf[NUMARM];
  TH1F* MutClusChisq[NUMARM];
  TH1F* VtxChisqNdf[NUMARM];
  TH1F* MutAdcPulses[NUMARM][NUMSTATION][NUMOCTANT][NUMGAP];

  // add hits per FEM paket
  TProfile *MutHitsPerPacketArm[NUMARM];
  TProfile *MutAmuErrorPerPacketArm[NUMARM];

  // Coordinate for radiograph
  TH2F *MutHitCoord[2][3][3];

 // For QA analysis
  TF1 *qpeakfunc[NUMARM][NUMSTATION][NUMOCTANT][NUMGAP];
  TF1 *qpeakfunction[NUMARM];

  char name[128];
  char id[128];
  for( int i=0; i<NUMARM; i++)
  for( int j=0; j<NUMSTATION; j++)
  for( int k=0; k<NUMGAP; k++)
  {
   if(j==2 && k==2 ) continue;
   if(j==0) int size_st=150;
   if(j==1) int size_st=300;
   if(j==2) int size_st=450;
   sprintf(name, "MutRadial_arm%d_station%d_gap%d", i, j, k);
   MutRadial[i][j][k]=new TH1F(name,name,size_st,0,size_st);
   sprintf(name, "MutAzimuth_arm%d_station%d_gap%d", i, j, k);
   MutAzimuth[i][j][k]=new TH1F(name,name,500,-3.14.,3.14);
 }


 // Creating CANVAS

  TCanvas *mutNumClusterCS = new TCanvas("mutNumClusterCS", "South Mutr Number of Fitted Cathode Strip Clusters", 120,10,990,820);
  mutNumClusterCS->Divide(1,3);

  TCanvas *mutNumClusterCN = new TCanvas("mutNumClusterCN", "North Mutr Number of Fitted Cathode Strip Clusters", 120,10,990,820);
  mutNumClusterCN->Divide(1,3);

  TCanvas *mutCathClustQPeakCNS1 = new TCanvas("mutCathClustQPeakCNS1", "North Mutr Station 1 Cathode Cluster Q Peak", 120,10,990,820);
  mutCathClustQPeakCNS1->Divide(8,3);

  TCanvas *mutCathClustQPeakCNS2 = new TCanvas("mutCathClustQPeakCNS2", "North Mutr Station 2 Cathode Cluster Q Peak", 120,10,990,820);
  mutCathClustQPeakCNS2->Divide(8,3);

  TCanvas *mutCathClustQPeakCNS3 = new TCanvas("mutCathClustQPeakCNS3", "North Mutr Station 3 Cathode Cluster Q Peak", 120,10,990,820);
  mutCathClustQPeakCNS3->Divide(8,2);



  TCanvas *mutCathClustQPeakCSS1 = new TCanvas("mutCathClustQPeakCSS1", "South Mutr Station 1 Cathode Cluster Q Peak", 120,10,990,820);
  mutCathClustQPeakCSS1->Divide(8,3);

  TCanvas *mutCathClustQPeakCSS2 = new TCanvas("mutCathClustQPeakCSS2", "South Mutr Station 2 Cathode Cluster Q Peak", 120,10,990,820);
  mutCathClustQPeakCSS2->Divide(8,3);

  TCanvas *mutCathClustQPeakCSS3 = new TCanvas("mutCathClustQPeakCSS3", "South Mutr Station 3 Cathode Cluster Q Peak", 120,10,990,820);
  mutCathClustQPeakCSS3->Divide(8,2);

  TCanvas *mutHitsCS = new TCanvas("mutHitsCS", "South Mutr Hits", 120,10,990,820);
  mutHitsCS->Divide(1,3);

  TCanvas *mutHitsCN = new TCanvas("mutHitsCN", "North Mutr Hits", 120,10,990,820);
  mutHitsCN->Divide(1,3);



  TCanvas *mutRadioN = new TCanvas("mutRadioN", "North Mutr Radiograph", 120,10,990,820);
  mutRadioN->Divide(3,3);

  TCanvas *mutRadioS = new TCanvas("mutRadioS", "South Mutr Radiograph", 120,10,990,820);
  mutRadioS->Divide(3,3);


  TCanvas *mutCathClustQPeakArmCS = new TCanvas("mutCathClustQPeakArmCS", "South Mutr QPeak", 120,10,990,820);

  TCanvas *mutCathClustQPeakArmCN = new TCanvas("mutCathClustQPeakArmCN", "North Mutr QPeak", 120,10,990,820);

  TCanvas *mutCathClustWidthCS = new TCanvas("mutCathClustWidthCS", "South Mutr ClustersWidth", 120,10,990,820);
  mutCathClustWidthCS->Divide(1,3);

  TCanvas *mutCathClustWidthCN = new TCanvas("mutCathClustWidthCN", "North Mutr ClustersWidth", 120,10,990,820);
  mutCathClustWidthCN->Divide(1,3);

  TCanvas *mutNumTrackCS = new TCanvas("mutNumTrackCS", "South Mutr Number of Tracks", 120,10,990,820);

  TCanvas *mutNumTrackCN = new TCanvas("mutNumTrackCN", "North Mutr Number of Tracks", 120,10,990,820);

  TCanvas *mutTrackMomCS = new TCanvas("mutTrackMomCS", "South Mutr Momentum of Tracks", 120,10,990,820);

  TCanvas *mutTrackMomCN = new TCanvas("mutTrackMomCN", "North Mutr Momentum of Tracks", 120,10,990,820);


  TCanvas *mutNumHitsPakCS = new TCanvas("mutNumHitsPakCS", "South Mutr Number of Hits Per Packet", 120,10,990,820);

  TCanvas *mutNumHitsPakCN = new TCanvas("mutNumHitsPakCN", "North Mutr Number of Hits Per Packet", 120,10,990,820);

  TCanvas *mutAmuErrorCS = new TCanvas("mutAmuErrorCS", "South Mutr Amu Error Per Packet", 120,10,990,820);

  TCanvas *mutAmuErrorCN = new TCanvas("mutAmuErrorCN", "North Mutr Amu Error Per Packet", 120,10,990,820);


  //TCanvas *mut4SamCS = new TCanvas("mut4SamCS", "South Mutr 4 sample pulse");

  //TCanvas *mut4SamCN = new TCanvas("mut4SamCN", "North Mutr 4 sample pulse");

  TCanvas *mut_azimuth_North=new TCanvas("mut_azimuth_North","Azimuthal distribution North arm", 120,10,990,820);
  mut_azimuth_North->Divide(3,3);

  TCanvas* mut_azimuth_South=new TCanvas("mut_azimuth_South", "Azimuthal Distribution South arm", 120,10,990,820);
  mut_azimuth_South->Divide(3,3);  

  TCanvas* mut_radial_North=new TCanvas("mut_radial_North","Radial Distribution North arm", 120,10,990,820);
  mut_radial_North->Divide(3,3);  

  TCanvas* mut_radial_South= new TCanvas("mut_radial_South", "Radial Distribution South arm", 120,10,990,820);
  mut_radial_South->Divide(3,3);

  TCanvas* mut_trkchisqndf = new TCanvas("mut_trkchisqndf", "Track chisq/ndf ", 120,10,990,820);
  mut_trkchisqndf->Divide(1,2);

  TCanvas* mut_cluschisq = new TCanvas("mut_cluschisq", "Mutr cluster chisq ", 120,10,990,820);
  mut_cluschisq->Divide(1,2);

  TCanvas* mut_vtxchisqndf = new TCanvas("mut_vtxchisqndf", "vtx chisq/ndf", 120,10,990,820);
  mut_vtxchisqndf->Divide(1,2);

  TCanvas *mutCathClustAdcS1 = new TCanvas("mutCathClustAdcS1", "South Mutr Station 1 Cathode Cluster Adc", 120,10,990,820);
  mutCathClustAdcS1->Divide(8,3);
  
  TCanvas *mutCathClustAdcS2 = new TCanvas("mutCathClustAdcS2", "South Mutr Station 2 Cathode Cluster Adc", 120,10,990,820);
  mutCathClustAdcS2->Divide(8,3);
  
  TCanvas *mutCathClustAdcS3 = new TCanvas("mutCathClustAdcS3", "South Mutr Station 3 Cathode Cluster Adc", 120,10,990,820);
  mutCathClustAdcS3->Divide(8,2);

  
  TCanvas *mutCathClustAdcN1 = new TCanvas("mutCathClustAdcN1", "North Mutr Station 1 Cathode Cluster Adc", 120,10,990,820);
  mutCathClustAdcN1->Divide(8,3);

  TCanvas *mutCathClustAdcN2 = new TCanvas("mutCathClustAdcN2", "North Mutr Station 2 Cathode Cluster Adc", 120,10,990,820);
  mutCathClustAdcN2->Divide(8,3);

  TCanvas *mutCathClustAdcN3 = new TCanvas("mutCathClustAdcN3", "North Mutr Station 3 Cathode Cluster Adc", 120,10,990,820);
  mutCathClustAdcN3->Divide(8,2);



 
 

  cout << " QA file name " << qafile->GetName()  << endl;
 // muiRunNumber= (TH1F*) qafile->Get("muiRunNumber");
 // cout << "Run Number: " << muiRunNumber->GetMaximum() << endl;
 // summaryFile << "Run Number " << runnumber << endl;

  for( int arm=0;arm<NUMARM;arm++)
   {
    char id[128];
    int sta;
    int numr=0;

   Int_t howmanyplanes=0;
   Double_t hitavg = 0 ;
   Double_t HitPlaSt1 = 0.0 ;
   Double_t HitPlaSt2 = 0.0 ;
   Double_t HitPlaSt3 = 0.0 ;


    for(sta=0;sta<NUMSTATION;sta++)
     {
   
      int num = 0;
      int NumHotPLANE= 0;
      Int_t NumDeadPLANE= 0;


      for (int gap = 0; gap < NUMGAP; gap++)
      {
       if( sta==2 && gap==2 ) continue;
       for (int oct = 0; oct < NUMOCTANT; oct++)
        {
         num++;
  
       Double_t params[3]={0.1,0.1,0.1};
       Double_t returned_params[3]={0.0};           


        sprintf(id,"MutCathClustQPeakSt[%d][%d][%d][%d]", arm, sta, oct, gap);
        cout << "Loading "<< id << endl;
        MutCathClustQPeakSt[arm][sta][oct][gap] = (TH1F *)qafile->Get(id);
       
       sprintf(id,"qpeakfun[%d][%d][%d][%d]",arm,sta,oct,gap);
       qpeakfunc[arm][sta][oct][gap]=new TF1(id,"landau",0,100);
       qpeakfunc[arm][sta][oct][gap]->SetParameters(params);
       qpeakfunc[arm][sta][oct][gap]->SetLineColor(2);
       MutCathClustQPeakSt[arm][sta][oct][gap]->Fit(qpeakfunc[arm][sta][oct][gap],"RNQ");
       qpeakfunc[arm][sta][oct][gap]->GetParameters(returned_params);
       //string string1;
       summaryFile << "Arm "<< arm << " Station "<< sta  << " oct " << oct << " gap " << gap << " landau_mpv " << returned_params[1] << endl;
       summaryFile << "Arm "<< arm << " Station "<< sta  << " oct " << oct << " gap " << gap << " landau_sigma " << returned_params[2] << endl;
       //summaryFile << "Arm"+string1<<arm+"Station"+string1<<sta+"oct"+string1<<oct+"halfoct"+string1<<halfoct << " landau_norm " << returned_params[3] << endl;

       if (arm ==0) {
       if(sta==0) mutCathClustQPeakCSS1->cd(num);
       if(sta==1) mutCathClustQPeakCSS2->cd(num);
       if(sta==2) mutCathClustQPeakCSS3->cd(num);
       MutCathClustQPeakSt[arm][sta][oct][gap]->Draw();
       qpeakfunc[arm][sta][oct][gap]->Draw("same");
       } else if(arm==1){
       if(sta==0) mutCathClustQPeakCNS1->cd(num);
       if(sta==1) mutCathClustQPeakCNS2->cd(num);
       if(sta==2) mutCathClustQPeakCNS3->cd(num);
       MutCathClustQPeakSt[arm][sta][oct][gap]->Draw();
       qpeakfunc[arm][sta][oct][gap]->Draw("same");
       }
  /*     }
      }

     num=0;
      for (int gap = 0; gap < NUMGAP; gap++)
      {
       if( sta==2 && gap==2 ) continue;
       for (int oct = 0; oct < NUMOCTANT; oct++)
        {
       num++; */

       sprintf(id,"MutPulseSamples[%d][%d][%d][%d]", arm, sta, oct, gap);
       cout << "Loading "<< id << endl;
       MutAdcPulses[arm][sta][oct][gap] = (TH1F*)qafile->Get(id);

      if(arm ==0) {
       if(sta==0) mutCathClustAdcS1->cd(num);
       if(sta==1) mutCathClustAdcS2->cd(num);
       if(sta==2) mutCathClustAdcS3->cd(num);
       MutAdcPulses[arm][sta][oct][gap]->Draw();
       } else if(arm==1){
       if(sta==0) mutCathClustAdcN1->cd(num);
       if(sta==1) mutCathClustAdcN2->cd(num);
       if(sta==2) mutCathClustAdcN3->cd(num);
       MutAdcPulses[arm][sta][oct][gap]->Draw();
       } 
       
        } // for octant
       } // for gap
       


     sprintf(id,"MutNumCathClustersSt[%d][%d]", arm, sta);
     cout << "Loading "<< id << endl;
     MutNumCathClustersSt[arm][sta] = (TH1F *)qafile->Get(id);
     double meanClus= MutNumCathClustersSt[arm][sta]->GetMean();
     double sigmaClus= MutNumCathClustersSt[arm][sta]->GetRMS(); 
     summaryFile << "Arm " << arm << " Station " << sta << " oct " << 9999 << " gap " << 9999 << " MeanClus " << meanClus  << endl;
     if (arm ==0) {
     mutNumClusterCS->cd(sta + 1);
     MutNumCathClustersSt[arm][sta]->Draw();
     } else {
     mutNumClusterCN->cd(sta + 1);
     MutNumCathClustersSt[arm][sta]->Draw();
     }


       sprintf(id,"MutHitsPerPlaneSt[%d][%d]", arm, sta);
       cout << "Loading " << id << endl;
       MutHitsPerPlaneSt[arm][sta] = (TProfile *)qafile->Get(id);
       howmanyplanes = MutHitsPerPlaneSt[arm][sta]->GetNbinsX();
       double meanHit= MutHitsPerPlaneSt[arm][sta]->GetMean(2);
       double sigmaHit = MutHitsPerPlaneSt[arm][sta]->GetRMS(2);
       summaryFile << "Arm " << arm << " Station " << sta << " oct "<< 9999 << " gap " << 9999 << " MeanHit " << meanHit  << endl;   


    
  // Definition of Hot Plane: Hits are more than 1 sigma away from mean 
       for(int i=1; i<=howmanyplanes; i++ )
         {
             hitavg = MutHitsPerPlaneSt[arm][sta]->GetBinContent(i);

           if( hitavg > meanHit+sigmaHit )
              {
                NumHotPLANE++;
                oct = (int)((i-1)/NUMPLANEST[sta]);
                plane=(i-1) -oct*NUMPLANEST[sta];
                cout << "HOTPLANE station: " << sta << " octant: " << oct << "plane: " << plane << endl;
              }
              if(hitavg == 0)
               {
                 oct = (int)((i-1)/NUMPLANEST[sta]);
                 plane=(i-1) -oct*NUMPLANEST[sta];
                 cout << "DEADPLANE station: " << sta << " octant: " << oct << "plane: " << plane << endl;
                 NumDeadPLANE++;
                } 
              }
      summaryFile << "Arm " << arm << " Station " << sta << " oct " << 9999 << " gap " << 9999 << " Hot_plane " <<  NumHotPLANE << endl;
      //cout << "NO OF HOT PLANE ARM " <<arm << " STA "<< sta << "  : " <<  NumHotPLANE << endl;
      summaryFile << "Arm " << arm << " Station " << sta << " oct " << 9999 << " gap " << 9999 << " Dead_plane " <<  NumDeadPLANE << endl;
     // cout  << "NO OF DEAD PLANE ARM" << arm << " STA " << sta << " : " <<  NumDeadPLANE << endl;
    


       if (arm ==0) {
         mutHitsCS->cd(sta + 1);
         MutHitsPerPlaneSt[arm][sta]->Draw();
       } else {
         mutHitsCN->cd(sta + 1);
         MutHitsPerPlaneSt[arm][sta]->Draw();
        }

   for (int gap=0; gap<3; gap++ ) {
     
     if ( sta==2 && gap==2 ) continue;
     numr++;
     sprintf(id,"MutHitCoord[%d][%d][%d]",arm,sta,gap);
     cout << "Loading " << id << endl;
     MutHitCoord[arm][sta][gap] = (TH2F *)qafile->Get(id);
  //   cout << "arm " << arm << " sta " << sta << " gap " << gap << endl;
  //   cout << "x bins" << MutHitCoord[arm][sta][gap]->GetNbinsX() << " y bins " << MutHitCoord[arm][sta][gap]->GetNbinsY() << endl;
     for( int i_x=1; i_x < MutHitCoord[arm][sta][gap]->GetNbinsX(); i_x++)
     for( int i_y=1; i_y < MutHitCoord[arm][sta][gap]->GetNbinsY(); i_y++)
     {
       int temp= MutHitCoord[arm][sta][gap]->GetBinContent(i_x,i_y);
       if(temp==0) continue;
       double x= MutHitCoord[arm][sta][gap]->GetXaxis()->GetBinCenter(i_x);
       double y= MutHitCoord[arm][sta][gap]->GetYaxis()->GetBinCenter(i_y);
       //cout << "x " << x <<  "  y  " << y << endl;
       double r= sqrt( pow(x,2) + pow(y,2) );
       if( x!=0 ){ double phi = atan2( y , x ); }
       else{ double phi =  -9999; }
       //cout << "r " << r << "  phi " << phi << " count  " << temp <<  endl;
       if( MutAzimuth[arm][sta][gap]->FindBin(phi)>0) MutAzimuth[arm][sta][gap]->AddBinContent( MutAzimuth[arm][sta][gap]->FindBin(phi), temp );
       if( MutRadial[arm][sta][gap]->FindBin(r)>0) MutRadial[arm][sta][gap]->AddBinContent( MutRadial[arm][sta][gap]->FindBin(r), temp );
     }

  
     if ( arm ==0 ) {
        mutRadioS->cd(numr);
        //draw_wires(arm,sta,gap,false);
        MutHitCoord[arm][sta][gap]->Draw("COLZSAME");
        mut_azimuth_South->cd(numr);
        MutAzimuth[arm][sta][gap]->Draw();
        mut_radial_South->cd(numr);
        MutRadial[arm][sta][gap]->Draw();
       }
       else {
         mutRadioN->cd(numr);
         //draw_wires(arm,sta,gap,false);
         MutHitCoord[arm][sta][gap]->Draw("COLZSAME");
         mut_azimuth_North->cd(numr);
         MutAzimuth[arm][sta][gap]->Draw();
         mut_radial_North->cd(numr);
         MutRadial[arm][sta][gap]->Draw();
        }
        }


     sprintf(id,"MutCathClustWidth[%d][%d]", arm, sta);
     cout << "Loading "<< id << endl;
      MutCathClustWidth[arm][sta] = (TH1F *)qafile->Get(id);

     summaryFile << "Arm " << arm << " Station " << sta << " oct " << 9999 << " gap " << 9999 << " Clus_Width " << MutCathClustWidth[arm][sta]->GetMean() << endl;
      if (arm ==0) {
        mutCathClustWidthCS->cd(sta+1);
        MutCathClustWidth[arm][sta]->Draw();
      } else {
        mutCathClustWidthCN->cd(sta+1);
        MutCathClustWidth[arm][sta]->Draw();
      }





      }  // for station

     sprintf(id,"MutHitsPerPacketArm[%d]", arm);
     cout <<"Loading " << id << endl;
      MutHitsPerPacketArm[arm] = (TProfile *)qafile->Get(id);
      int howmanypackets= MutHitsPerPacketArm[arm]->GetNbinsX();
      double meanhit_pak= MutHitsPerPacketArm[arm]->GetMean(2);
      double sigmahit_pak= MutHitsPerPacketArm[arm]->GetRMS(2);
     summaryFile << "Arm " << arm << " station " << 9999 << " oct " << 9999 << " gap " << 9999 << " HitperPak " <<  meanhit_pak << endl;

      if (arm ==0) {
        mutNumHitsPakCS->cd();
        MutHitsPerPacketArm[arm]->Draw();
      } else {
        mutNumHitsPakCN->cd();
        MutHitsPerPacketArm[arm]->Draw();
      }

    // Hot Packet: Packet hits more than two sigma away from mean
     int hot_pak=0;
     int dead_pak=0;
     for( int i_pak=1; i_pak< howmanypackets; i_pak++){
          double hits_pak = MutHitsPerPacketArm[arm]->GetBinContent(i_pak);
          if( hits_pak > meanhit_pak+ 2*sigmahit_pak) hot_pak++;
          if( hits_pak ==0 ) dead_pak++;
         }

   summaryFile << "Arm " << arm << " station " << 9999 << " oct " << 9999 << " gap " << 9999 << " Hot_packet " <<  hot_pak << endl;
   summaryFile << "Arm " << arm << " station " << 9999 << " oct " << 9999 << " gap " << 9999 << " Dead_packet " << dead_pak << endl;



      sprintf(id,"MutAmuErrorPerPacketArm[%d]", arm);
      cout <<"Loading "<< id << endl;
      MutAmuErrorPerPacketArm[arm] = (TProfile *)qafile->Get(id);
      if (arm ==0) {
        mutAmuErrorCS->cd();
        MutAmuErrorPerPacketArm[arm]->Draw();
      } else {
        mutAmuErrorCN->cd();
        MutAmuErrorPerPacketArm[arm]->Draw();
      }


      Double_t params[3]={0.1,0.1,0.1};
      Double_t returned_params[3]={0.0};
      sprintf(id,"MutCathClustQPeak[%d]", arm);
      cout << "Loading "<< id << endl;
      MutCathClustQPeak[arm] = (TH1F *)qafile->Get(id);
      sprintf(id,"qpeakfunction[%d]",arm);
      qpeakfunction[arm]=new TF1(id,"landau",0,100);
      qpeakfunction[arm]->SetParameters(params);
      qpeakfunction[arm]->SetLineColor(2);
      MutCathClustQPeak[arm]->Fit(qpeakfunction[arm],"RNQ");
      qpeakfunction[arm]->GetParameters(returned_params);

      if(arm ==0) {
        mutCathClustQPeakArmCS->cd();
        MutCathClustQPeak[arm]->Draw();
        qpeakfunction[arm]->Draw("same");
      } else {
        mutCathClustQPeakArmCN->cd();
        MutCathClustQPeak[arm]->Draw();
        qpeakfunction[arm]->Draw("same");
      }
    summaryFile << "Arm " << arm << " station " << 9999 << " oct " << 9999 << " gap " << 9999 << " ClusQMPV " << returned_params[1] << endl;
    summaryFile << "Arm " << arm << " station " << 9999 << " oct " << 9999 << " gap " << 9999 << " ClusQSigma " << returned_params[2] << endl;


      sprintf(id,"MutTrackMom[%d]", arm);
      cout <<"Loading "<< id << endl;
      MutTrackMom[arm] = (TH1F *)qafile->Get(id);
      if (arm ==0) {
        mutTrackMomCS->cd();
        MutTrackMom[arm]->Draw();
      } else {
        mutTrackMomCN->cd();
        MutTrackMom[arm]->Draw();
      }


      // sprintf(id,"MutCathClustQFit[%d]", arm);
      // MutCathClustQFit[arm] = (TH1F *)qafile->Get(id);


      sprintf(id,"MutNumTracks[%d]", arm);
      cout << "Loading "<< id << endl;
      MutNumTracks[arm] = (TH1F *)qafile->Get(id);
      if (arm ==0) {
        mutNumTrackCS->cd();
        MutNumTracks[arm]->Draw();
      } else {
        mutNumTrackCN->cd();
        MutNumTracks[arm]->Draw();
      }
      summaryFile << "Arm " << arm << " station " << 9999 << " oct " << 9999 << " gap " << 9999 << " Trks/event " << MutNumTracks[arm]->Integral("width") << endl;

      sprintf(id,"MutTrackChisqNdf[%d]", arm);
      cout << "Loading "<< id << endl;
      MutTrkChisqNdf[arm] = (TH1F *)qafile->Get(id);
      if (arm ==0) {
        mut_trkchisqndf->cd(1);
        MutTrkChisqNdf[arm]->Draw();
      } else {
        mut_trkchisqndf->cd(2);
        MutTrkChisqNdf[arm]->Draw();
      }  
      summaryFile << "Arm " << arm << " station " << 9999 << " oct " << 9999 << " gap " << 9999 << " Mean_trkchisq/ndf " << MutTrkChisqNdf[arm]->GetMean() << endl;
 
     sprintf(id,"MutClusChisquare[%d]", arm);
      cout << "Loading "<< id << endl;
      MutClusChisq[arm] = (TH1F *)qafile->Get(id);
      if (arm ==0) {
        mut_cluschisq->cd(1);
        MutClusChisq[arm]->Draw();
      } else {
        mut_cluschisq->cd(2);
        MutClusChisq[arm]->Draw();
      }  


      sprintf(id,"VtxChisqNdf[%d]", arm);
      cout << "Loading "<< id << endl;
      VtxChisqNdf[arm] = (TH1F *)qafile->Get(id);
      if (arm ==0) {
        mut_vtxchisqndf->cd(1);
        VtxChisqNdf[arm]->Draw();
      } else {
        mut_vtxchisqndf->cd(2);
        VtxChisqNdf[arm]->Draw();
      }  
   summaryFile << "Arm " << arm << " station " << 9999 << " oct " << 9999 << " gap " << 9999 << " Mean_vtxchisq/ndf " << VtxChisqNdf[arm]->GetMean() << endl;


   }   // for arm


  char folder[128];
  sprintf(folder,"rm -rf %d",runnumber);
  gSystem->Exec(folder);

  sprintf(folder, ".mkdir %d",runnumber);
  gROOT->ProcessLine(folder);
 
  char name[128];

  mutNumClusterCS->Update();
  sprintf(name,"%d/qaMutrNumClusterS.gif",runnumber);
  mutNumClusterCS->SaveAs(name);

  mutNumClusterCN->Update();
  sprintf(name,"%d/qaMutrNumClusterN.gif",runnumber);
  mutNumClusterCN->SaveAs(name);

  mutCathClustQPeakCNS1->Update();
  sprintf(name,"%d/qaMutrCathClustQPeakCNS1.gif",runnumber);
  mutCathClustQPeakCNS1->SaveAs(name);

  mutCathClustQPeakCNS2->Update();
  sprintf(name,"%d/qaMutrCathClustQPeakCNS2.gif",runnumber);
  mutCathClustQPeakCNS2->SaveAs(name);

  mutCathClustQPeakCNS3->Update();
  sprintf(name,"%d/qaMutrCathClustQPeakCNS3.gif",runnumber);
  mutCathClustQPeakCNS3->SaveAs(name);

  mutCathClustAdcN1->Update();
  sprintf(name,"%d/qaMutrCathClustAdcN1.gif",runnumber);
  mutCathClustAdcN1->SaveAs(name);

  mutCathClustAdcN2->Update();
  sprintf(name,"%d/qaMutrCathClustAdcN2.gif",runnumber);
  mutCathClustAdcN2->SaveAs(name);

  mutCathClustAdcN3->Update();
  sprintf(name,"%d/qaMutrCathClustAdcN3.gif",runnumber);
  mutCathClustAdcN3->SaveAs(name);  


  mutCathClustQPeakCSS1->Update();
  sprintf(name,"%d/qaMutrCathClustQPeakCSS1.gif",runnumber);
  mutCathClustQPeakCSS1->SaveAs(name);

  mutCathClustQPeakCSS2->Update();
  sprintf(name,"%d/qaMutrCathClustQPeakCSS2.gif",runnumber);
  mutCathClustQPeakCSS2->SaveAs(name);

  mutCathClustQPeakCSS3->Update();
  sprintf(name,"%d/qaMutrCathClustQPeakCSS3.gif",runnumber);
  mutCathClustQPeakCSS3->SaveAs(name);

  mutCathClustAdcS1->Update();
  sprintf(name,"%d/qaMutrCathClustAdcS1.gif",runnumber);
  mutCathClustAdcS1->SaveAs(name);

  mutCathClustAdcS2->Update();
  sprintf(name,"%d/qaMutrCathClustAdcS2.gif",runnumber);
  mutCathClustAdcS2->SaveAs(name);

  mutCathClustAdcS3->Update();
  sprintf(name,"%d/qaMutrCathClustAdcS3.gif",runnumber);
  mutCathClustAdcS3->SaveAs(name);


  mutRadioN->Update();
  sprintf(name,"%d/qaMutrRadioN.gif",runnumber);
  mutRadioN->SaveAs(name);

  mutRadioS->Update();
  sprintf(name,"%d/qaMutRadioS.gif",runnumber);
  mutRadioS->SaveAs(name);

  mutCathClustQPeakArmCN->Update();
  sprintf(name,"%d/qaMutCathClustQPeakArmN.gif",runnumber);
  mutCathClustQPeakArmCN->SaveAs(name);

  mutCathClustQPeakArmCS->Update();
  sprintf(name,"%d/qaMutCathClustQPeakArmS.gif",runnumber);
  mutCathClustQPeakArmCS->SaveAs(name);

  mutNumHitsPakCS->Update();
  sprintf(name,"%d/qaMutrNumHitsPakS.gif",runnumber);  
  mutNumHitsPakCS->SaveAs(name);

  mutNumHitsPakCN->Update();
  sprintf(name,"%d/qaMutrNumHitsPakN.gif",runnumber);
  mutNumHitsPakCN->SaveAs(name);
  
  mutAmuErrorCS->Update();
  sprintf(name,"%d/qaMutrAmuErrorS.gif",runnumber);
  mutAmuErrorCS->SaveAs(name);

  mutAmuErrorCN->Update();
  sprintf(name,"%d/qaMutrAmuErrorN.gif",runnumber);
  mutAmuErrorCN->SaveAs(name);

  mutCathClustWidthCS->Update();
  sprintf(name,"%d/qaMutCathClustWidthCS.gif",runnumber);
  mutCathClustWidthCS->SaveAs(name);

  mutNumTrackCS->Update();
  sprintf(name,"%d/qaMutNumTrackCS.gif",runnumber);
  mutNumTrackCS->SaveAs(name);

  mutTrackMomCS->Update();
  sprintf(name,"%d/qaMutTrackMomCS.gif",runnumber);
  mutTrackMomCS->SaveAs(name);

  mutHitsCS->Update();
  sprintf(name,"%d/qaMutHitsCS.gif",runnumber);
  mutHitsCS->SaveAs(name);

 // mut4SamCS->Update();
 // sprintf(name,"%d/qaMut4SamCS.gif",runnumber);
 // mut4SamCS->SaveAs(name);

  mutCathClustWidthCN->Update();
  sprintf(name,"%d/qaMutCathClustWidthCN.gif",runnumber);
  mutCathClustWidthCN->SaveAs(name);

  mutNumTrackCN->Update();
  sprintf(name,"%d/qaMutNumTrackCN.gif",runnumber);
  mutNumTrackCN->SaveAs(name);

  mutTrackMomCN->Update();
  sprintf(name,"%d/qaMutTrackMomCN.gif",runnumber);
  mutTrackMomCN->SaveAs(name);

  mutHitsCN->Update();
  sprintf(name,"%d/qaMutHitsCN.gif",runnumber);
  mutHitsCN->SaveAs(name);

  //mut4SamCN->Update();
  //sprintf(name,"%d/qaMut4SamCN.gif",runnumber);
  //mut4SamCN->SaveAs(name);
  
  mut_radial_North->Update();
  sprintf(name,"%d/qaMutRadialN.gif",runnumber);
  mut_radial_North->SaveAs(name);

  mut_radial_South->Update();
  sprintf(name,"%d/qaMutRadialS.gif",runnumber);
  mut_radial_South->SaveAs(name);

  mut_azimuth_North->Update();
  sprintf(name,"%d/qaMutAzimuthN.gif",runnumber);
  mut_azimuth_North->SaveAs(name);

  mut_azimuth_South->Update();
  sprintf(name,"%d/qaMutAzimuthS.gif",runnumber);
  mut_azimuth_South->SaveAs(name);
 
  mut_trkchisqndf->Update();
  sprintf(name,"%d/qaMutTrkChisqNdf.gif",runnumber);
  mut_trkchisqndf->SaveAs(name);

  mut_cluschisq->Update();
  sprintf(name,"%d/qaMutClusChisq.gif",runnumber);
  mut_cluschisq->SaveAs(name);
 
  mut_vtxchisqndf->Update();
  sprintf(name,"%d/qaVtxChisqNdf.gif",runnumber);
  mut_vtxchisqndf->SaveAs(name);
 
 // sprintf(folder,"mv %d /data10/abhisek/run10_qaplots/ ",runnumber);
 // gSystem->Exec(folder);
  sprintf(folder,"mv %d %s ",runnumber,outpath);
  gSystem->Exec(folder);

  sprintf(folder,"mv %s %s%d ",summaryFileName,outpath,runnumber);
  gSystem->Exec(folder);

  gSystem->Exec("ps -o sid,ppid,pid,user,comm,vsize,rssize,time");

 }
