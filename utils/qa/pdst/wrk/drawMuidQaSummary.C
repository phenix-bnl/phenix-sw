//#include "TFile.h"
//#include "TH1.h"
//#include "TH2.h"
//#include "summaryMuid.h"
#include <fstream>
#include <iostream>

ofstream textFile;
ofstream statusFile;

// using namespace std;
//void drawMuidQaSummary()
// void drawMuidQaSummary(char* qafileIn, char* outputFile1, char* outputFile2, int runNumber) 

void drawMuidQaSummary() 
{

  TFile *qafile = new TFile("qa.root","READ");
  textFile.open("qaMuid.txt");
  statusFile.open("qaMuidStatus.txt");
  
  
  /*
  TFile *qafile = new TFile(qafileIn, "READ");
  
  // cout << "MuId..." << endl;
  fstream textFile(outputFile1, ios::in); 
  if (textFile) { 
    textFile.close(); 
    textFile.open(outputFile1,ios::app|ios::out); 
  }
  
  fstream statusFile(outputFile2, ios::in); 
  if (statusFile) {
    statusFile.close();  
    statusFile.open(outputFile2,ios::app|ios::out); 
    printf("Creating status output file \n");
  }
  
  */
  
  textFile << " ----------------------------------------------------" << endl;
  textFile << " -- MUI QA Summary --" << endl;
  textFile << " ----------------------------------------------------" << endl;
  
  // the maximum number of triggers (for any datatype)
  const int MAXTRIGGERS = 4;
  // South and North MUID arms
  const int NUMARM      = 2;
  
  // datatypes: 0 = Run2 AuAu, 1 = Run2(&3) pp, 2 = Run3 dAu 
  // dA values are set at about the mean between AA and pp for now..
  const int NDATATYPES = 4;
  
  // here we define our cuts, for now they are set to the same values for all datatypes
  // nhits cuts are relaxed, otherwise, they are identical to Run-2 pp
  const float NROADSMIN[NDATATYPES] = {0., 0., 0., 0.};
  // const float PROP1DTRIG_MIN[NDATATYPES] = {0.001, 0.001, 0.001, 0.001};
  const float MEANNHITS_MIN_MB[NDATATYPES] = {1., 1., 1., 1.};
  const float MEANNHITS_MAX_MB[NDATATYPES] = {25., 25., 25., 25.};
  //  const float MEANNHITS_MIN_1D[NDATATYPES] = {5., 5., 5., 5.};
  //  const float MEANNHITS_MAX_1D[NDATATYPES] = {50., 50., 50., 50.};
  const float EVTWROADS_MIN_MB[NDATATYPES] = {0.0005, 0.0005, 0.0005, 0.0005};
  //  const float EVTWROADS_MIN_1D[NDATATYPES] = {0.05, 0.05, 0.05, 0.05};
  const float MEANNROADS_MIN[NDATATYPES] = {1., 1., 1., 1.};
  const float MEANNROADS_MAX_MB[NDATATYPES] = {5., 5., 5., 5.};
  //  const float MEANNROADS_MAX_1D[NDATATYPES] = {5., 5., 5., 5.};
  const float NROADSSUP10_MAX_MB[NDATATYPES] = {0.15, 0.15, 0.15, 0.15};
  //  const float NROADSSUP10_MAX_1D[NDATATYPES] = {0.15, 0.15, 0.15, 0.15};
  const float GOLDPERROAD[NDATATYPES] = {1., 1., 1., 1.};
  //  const float NOGHOPERROAD_MIN_1D[NDATATYPES] = {0.07, 0.07, 0.07, 0.07};
  //  const float MEANLASTPLANE_MIN_1D[NDATATYPES] = {3.2, 3.2, 3.2, 0.07};
  const float MEANMATCHPOS_MAX_MB[NDATATYPES] = {40., 40., 40., 40.};
  //  const float MEANMATCHPOS_MAX_1D[NDATATYPES] = {40., 40., 40., 40.};
  
  // end of cut definitions - now let's get all the histograms
  
  TH1F *muiRunNumber;
  TH1F *muiDataType;
  TH1F *muiMinBias;
  TH1F *muiTriggerBit;
  TH1F *muiTriggerBit_scaled;
  TH1F *muiTriggerBit_live;
  
  
  // histograms: for each trigger 
  // 1-dim
  TH1F *muiNHits[MAXTRIGGERS];
  TH1F *muiNRoads[MAXTRIGGERS];
  TH1F *muiNRoads_NoGhost[MAXTRIGGERS];
  TH1F *muiNRoads_Golden[MAXTRIGGERS];
  TH1F *muiNGolden_per_Road[MAXTRIGGERS];
  TH1F *muiRoadTrack_MatchPos[MAXTRIGGERS];
  TH1F *muiLastPlane[MAXTRIGGERS];
  TH1F *muiMaxHits[MAXTRIGGERS];
  TH1F *muiFitQuality[MAXTRIGGERS];
  TH1F *muiBBCZVertex[MAXTRIGGERS];
  TH1F *muiChainHits[MAXTRIGGERS];
  // 2-dim
  TH2F *muiRoadGap0[MAXTRIGGERS];
  TH2F *muiRoadRefPos[MAXTRIGGERS];
  TH2F *muiP_Depth[MAXTRIGGERS];
  TH2F *muiNHits_BBCch[MAXTRIGGERS];
  
  muiRunNumber  = (TH1F *)qafile->Get("muiRunNumber");

  int runNumber = (int) muiRunNumber->GetBinContent(1);
  cout << "run number " << runNumber << endl;

  muiDataType = (TH1F *)qafile->Get("muiDataType");
  // check if Mui objects are into the file
  if (!muiDataType)
    {
      textFile << " MuId histograms don't exist. "
	       << "Status set to 2" << endl;
      statusFile << 2 << " " << 2 << " ";
      return;
    }
  muiMinBias = (TH1F *)qafile->Get("muiMinBias");
  muiTriggerBit = (TH1F *)qafile->Get("muiTriggerBit");
  muiTriggerBit_scaled = (TH1F *)qafile->Get("muiTriggerBit_scaled");
  muiTriggerBit_live = (TH1F *)qafile->Get("muiTriggerBit_live");

  TCanvas *muiTriggerC = new TCanvas("muiTriggerC", "Trigger Selection", 800, 600);
  muiTriggerC->Divide(3,2);
  muiTriggerC->cd(1);
  muiDataType->Draw();
  muiTriggerC->cd(2);
  muiMinBias->Draw();
  muiTriggerC->cd(3);
  muiTriggerBit->Draw();
  muiTriggerC->cd(4);
  muiTriggerBit_scaled->Draw();
  muiTriggerC->cd(5);
  muiTriggerBit_live->Draw();

  muiTriggerC->SaveAs("qaMuidTrig.eps");
  gSystem->Exec("pstopnm -ppm -xborder 0 -yborder 0 -portrait qaMuidTrig.eps");
  gSystem->Exec("ppmtogif qaMuidTrig.eps001.ppm > qagifs/qaMuidTrig.gif");


  int datatype = (int) muiDataType->GetBinContent(1);
  int nminbiasevt = (int) muiMinBias->GetBinContent(1);
  textFile << " datatype " << datatype << endl;
  textFile << " nminbiasevt " << nminbiasevt << endl;
  
  // set how many triggers we've studied and which they were
  // use strings for the trigger names
  int ntriggers = 0;
  if ( datatype==0 )
    { // For AuAu data
      ntriggers = 4;
    }
  if ( datatype == 1 )
    { // For pp data
      ntriggers = 4;
    }
  else if (datatype == 2)
    {
      ntriggers = 4;
    }
  else if (datatype == 4)
    {
      ntriggers = 2;
    }
  else
    {
      cout << " unknow data type. " << endl;
    }
  
  char id[128];
  TCanvas *muidHitC = new TCanvas("muidHitC", "Number of Muid Hits", 120,10,640,710);
  muidHitC->Divide(1,2);

  TCanvas *muidRoadC = new TCanvas("muidRoadC", "Number of Muid Roads", 120,10,640,710);
  muidRoadC->Divide(1,2);

  TCanvas *muidRoad_NoGhostC = new TCanvas("muidRoad_NoGhostC", "Number of Muid NoGhost Roads", 120,10,640,710);
  muidRoad_NoGhostC->Divide(1,2);

  TCanvas *muidRoad_GoldenC = new TCanvas("muidRoad_GoldenC", "Number of Muid Golden Roads", 120,10,640,710);
  muidRoad_GoldenC->Divide(1,2);

  TCanvas *muiNGolden_per_RoadC = new TCanvas("muiNGolden_per_RoadC", "Fraction of Muid Golden Roads", 120,10,640,710);
  muiNGolden_per_RoadC->Divide(1,2);

  TCanvas *muiRoadTrack_MatchC = new TCanvas("muiRoadTrack_MatchC", "Muid Road-Track Match Position", 120,10,640,710);
  muiRoadTrack_MatchC->Divide(1,2);

  TCanvas *muiLastPlaneC = new TCanvas("muiLastPlaneC", "Muid Last Plane", 120,10,640,710);
  muiLastPlaneC->Divide(1,2);

  TCanvas *muiMaxHitsC = new TCanvas("muiMaxHitsC", "Muid Maximum Hits", 120,10,640,710);
  muiMaxHitsC->Divide(1,2);

  TCanvas *muiFitQualityC = new TCanvas("muiFitQualityC", "Muid Fit Quality", 120,10,640,710);
  muiFitQualityC->Divide(1,2);

  TCanvas *muiBBCZVertexC = new TCanvas("muiBBCZVertexC", "Muid BBC Z-Vertex", 120,10,640,710);
  muiBBCZVertexC->Divide(1,2);

  TCanvas *muiChainHitsC = new TCanvas("muiChainHitsC", "Muid Chain Hits", 120,10,640,710);
  muiChainHitsC->Divide(1,2);

  TCanvas *muiRoadGap0C = new TCanvas("muiRoadGap0C", "Muid Road Gap0 Position", 120,10,640,710);
  muiRoadGap0C->Divide(1,2);

  TCanvas *muiRoadRefPosC = new TCanvas("muiRoadRefPosC", "Muid Road Ref Position", 120,10,640,710);
  muiRoadRefPosC->Divide(1,2);

  TCanvas *muiP_DepthC = new TCanvas("muiP_DepthC", "Muid Momentum vs Depth", 120,10,640,710);
  muiP_DepthC->Divide(1,2);

  TCanvas *muiNHits_BBCchC = new TCanvas("muiNHits_BBCchC", "Muid Number of Hits vs BBC Charge", 120,10,640,710);
  muiNHits_BBCchC->Divide(1,2);


  for (int trig = 0; trig<ntriggers; trig++)
    {
      //  cout << " trigger " << trig << endl;
      sprintf(id,"muiNHits[%d]", trig);
      muiNHits[trig] = (TH1F *)qafile->Get(id);
      muidHitC->cd(trig+1);
      muiNHits[trig]->Draw();
      
      sprintf(id,"muiNRoads[%d]", trig);
      muiNRoads[trig] = (TH1F *)qafile->Get(id);
      muidRoadC->cd(trig+1);
      muiNRoads[trig]->Draw();  
    
      sprintf(id,"muiNRoads_NoGhost[%d]", trig);
      muiNRoads_NoGhost[trig] = (TH1F *)qafile->Get(id);
      muidRoad_NoGhostC->cd(trig+1);
      muiNRoads_NoGhost[trig]->Draw();
      
      sprintf(id,"muiNRoads_Golden[%d]", trig);
      muiNRoads_Golden[trig] = (TH1F *)qafile->Get(id);
      muidRoad_GoldenC->cd(trig+1);
      muiNRoads_Golden[trig]->Draw();

      sprintf(id,"muiNGolden_per_Road[%d]", trig);
      muiNGolden_per_Road[trig] = (TH1F *)qafile->Get(id);
      muiNGolden_per_RoadC->cd(trig+1);
      muiNGolden_per_Road[trig]->Draw();
      
      sprintf(id,"muiRoadTrack_MatchPos[%d]", trig);
      muiRoadTrack_MatchPos[trig] = (TH1F *)qafile->Get(id);
      muiRoadTrack_MatchC->cd(trig+1);
      muiRoadTrack_MatchPos[trig]->Draw();

      sprintf(id,"muiLastPlane[%d]", trig);
      muiLastPlane[trig] = (TH1F *)qafile->Get(id);
      muiLastPlaneC->cd(trig+1);
      muiLastPlane[trig]->Draw();
     
      sprintf(id,"muiMaxHits[%d]", trig);
      muiMaxHits[trig] = (TH1F *)qafile->Get(id);
      muiMaxHitsC->cd(trig+1);
      muiMaxHits[trig]->Draw();
      
      sprintf(id,"muiFitQuality[%d]", trig);
      muiFitQuality[trig] = (TH1F *)qafile->Get(id);
      muiFitQualityC->cd(trig+1);
      muiFitQuality[trig]->Draw();

      
      sprintf(id,"muiBBCZVertex[%d]", trig);
      muiBBCZVertex[trig] = (TH1F *)qafile->Get(id);
      muiBBCZVertexC->cd(trig+1);
      muiBBCZVertex[trig]->Draw();
      
      sprintf(id,"muiChainHits[%d]", trig);
      muiChainHits[trig] = (TH1F *)qafile->Get(id);
      muiChainHitsC->cd(trig+1);
      muiChainHits[trig]->Draw(); 
      
      sprintf(id,"muiRoadGap0[%d]", trig);
      muiRoadGap0[trig] = (TH2F *)qafile->Get(id);
      muiRoadGap0C->cd(trig+1);
      muiRoadGap0[trig]->Draw();
      
      sprintf(id,"muiRoadRefPos[%d]", trig);
      muiRoadRefPos[trig] = (TH2F *)qafile->Get(id);
      muiRoadRefPosC->cd(trig+1);
      muiRoadRefPos[trig]->Draw();
      
      sprintf(id,"muiP_Depth[%d]", trig);
      muiP_Depth[trig] = (TH2F *)qafile->Get(id);
      muiP_DepthC->cd(trig+1);
      muiP_Depth[trig]->Draw();
      
      sprintf(id,"muiNHits_BBCch[%d]", trig);
      muiNHits_BBCch[trig] = (TH2F *)qafile->Get(id); 
      muiNHits_BBCchC->cd(trig+1);
      muiNHits_BBCch[trig]->Draw();

    } // trig loop
  

  muidHitC->SaveAs("qaMuidHits.eps");
  gSystem->Exec("pstopnm -ppm -xborder 0 -yborder 0 -portrait qaMuidHits.eps");
  gSystem->Exec("ppmtogif qaMuidHits.eps001.ppm > qagifs/qaMuidHits.gif");
  gSystem->Exec("rm qaMuidHits.eps qaMuidHits.eps001.ppm");


  muidRoadC->SaveAs("qaMuidRoads.eps");
  gSystem->Exec("pstopnm -ppm -xborder 0 -yborder 0 -portrait qaMuidRoads.eps");
  gSystem->Exec("ppmtogif qaMuidRoads.eps001.ppm > qagifs/qaMuidRoads.gif");
  gSystem->Exec("rm qaMuidRoads.eps qaMuidRoads.eps001.ppm");

  muidRoad_NoGhostC->SaveAs("qaMuidRoads_NoGhost.eps");
  gSystem->Exec("pstopnm -ppm -xborder 0 -yborder 0 -portrait qaMuidRoads_NoGhost.eps");
  gSystem->Exec("ppmtogif qaMuidRoads_NoGhost.eps001.ppm > qagifs/qaMuidRoads_NoGhost.gif");
  gSystem->Exec("rm qaMuidRoads_NoGhost.eps001.ppm qaMuidRoads_NoGhost.eps");

  muidRoad_GoldenC->SaveAs("qaMuidRoads_Golden.eps");
  gSystem->Exec("pstopnm -ppm -xborder 0 -yborder 0 -portrait qaMuidRoads_Golden.eps");
  gSystem->Exec("ppmtogif qaMuidRoads_Golden.eps001.ppm > qagifs/qaMuidRoads_Golden.gif");
  gSystem->Exec("rm qaMuidRoads_Golden.eps001.ppm qaMuidRoads_Golden.eps");

  muiNGolden_per_RoadC->SaveAs("qaMuidRoads_Golden_per_road.eps");
  gSystem->Exec("pstopnm -ppm -xborder 0 -yborder 0 -portrait qaMuidRoads_Golden_per_road.eps");
  gSystem->Exec("ppmtogif qaMuidRoads_Golden_per_road.eps001.ppm > qagifs/qaMuidRoads_Golden_per_road.gif");
  gSystem->Exec("rm qaMuidRoads_Golden_per_road.eps001.ppm qaMuidRoads_Golden_per_road.eps");

  muiRoadTrack_MatchC->SaveAs("qaMuidRoadTrack_Match.eps");
  gSystem->Exec("pstopnm -ppm -xborder 0 -yborder 0 -portrait qaMuidRoadTrack_Match.eps");
  gSystem->Exec("ppmtogif qaMuidRoadTrack_Match.eps001.ppm > qagifs/qaMuidRoadTrack_Match.gif");
  gSystem->Exec("rm qaMuidRoadTrack_Match.eps001.ppm qaMuidRoadTrack_Match.eps");

  muiLastPlaneC->SaveAs("qaMuidLastPlane.eps");
  gSystem->Exec("pstopnm -ppm -xborder 0 -yborder 0 -portrait qaMuidLastPlane.eps");
  gSystem->Exec("ppmtogif qaMuidLastPlane.eps001.ppm > qagifs/qaMuidLastPlane.gif");
  gSystem->Exec("rm qaMuidLastPlane.eps001.ppm qaMuidLastPlane.eps");

  muiMaxHitsC->SaveAs("qaMuidMaxHits.eps");
  gSystem->Exec("pstopnm -ppm -xborder 0 -yborder 0 -portrait qaMuidMaxHits.eps");
  gSystem->Exec("ppmtogif qaMuidMaxHits.eps001.ppm > qagifs/qaMuidMaxHits.gif");
  gSystem->Exec("rm qaMuidMaxHits.eps001.ppm qaMuidMaxHits.eps");

  muiFitQualityC->SaveAs("qaMuidFitQuality.eps");
  gSystem->Exec("pstopnm -ppm -xborder 0 -yborder 0 -portrait qaMuidFitQuality.eps");
  gSystem->Exec("ppmtogif qaMuidFitQuality.eps001.ppm > qagifs/qaMuidFitQuality.gif");
  gSystem->Exec("rm qaMuidFitQuality.eps001.ppm qaMuidFitQuality.eps");

  muiBBCZVertexC->SaveAs("qaMuidBBCZVertex.eps");
  gSystem->Exec("pstopnm -ppm -xborder 0 -yborder 0 -portrait qaMuidBBCZVertex.eps");
  gSystem->Exec("ppmtogif qaMuidBBCZVertex.eps001.ppm > qagifs/qaMuidBBCZVertex.gif");
  gSystem->Exec("rm qaMuidBBCZVertex.eps001.ppm qaMuidBBCZVertex.eps");

  muiChainHitsC->SaveAs("qaMuidChainHits.eps");
  gSystem->Exec("pstopnm -ppm -xborder 0 -yborder 0 -portrait qaMuidChainHits.eps");
  gSystem->Exec("ppmtogif qaMuidChainHits.eps001.ppm > qagifs/qaMuidChainHits.gif");
  gSystem->Exec("rm qaMuidChainHits.eps001.ppm qaMuidChainHits.eps");

  muiRoadGap0C->SaveAs("qaMuidRoadGap0.eps");
  gSystem->Exec("pstopnm -ppm -xborder 0 -yborder 0 -portrait qaMuidRoadGap0.eps");
  gSystem->Exec("ppmtogif qaMuidRoadGap0.eps001.ppm > qagifs/qaMuidRoadGap0.gif");
  gSystem->Exec("rm qaMuidRoadGap0.eps001.ppm qaMuidRoadGap0.eps");

  muiRoadRefPosC->SaveAs("qaMuidRoadRefPos.eps");
  gSystem->Exec("pstopnm -ppm -xborder 0 -yborder 0 -portrait qaMuidRoadRefPos.eps");
  gSystem->Exec("ppmtogif qaMuidRoadRefPos.eps001.ppm > qagifs/qaMuidRoadRefPos.gif");
  gSystem->Exec("rm qaMuidRoadRefPos.eps001.ppm qaMuidRoadRefPos.eps");

  muiP_DepthC->SaveAs("qaMuidP_Depth.eps");
  gSystem->Exec("pstopnm -ppm -xborder 0 -yborder 0 -portrait qaMuidP_Depth.eps");
  gSystem->Exec("ppmtogif qaMuidP_Depth.eps001.ppm > qagifs/qaMuidP_Depth.gif");
  gSystem->Exec("rm qaMuidP_Depth.eps001.ppm qaMuidP_Depth.eps");

  muiNHits_BBCchC->SaveAs("qaMuidNHits_BBCch.eps");
  gSystem->Exec("pstopnm -ppm -xborder 0 -yborder 0 -portrait qaMuidNHits_BBCch.eps");
  gSystem->Exec("ppmtogif qaMuidNHits_BBCch.eps001.ppm > qagifs/qaMuidNHits_BBCch.gif");
  gSystem->Exec("rm qaMuidNHits_BBCch.eps001.ppm qaMuidNHits_BBCch.eps");


  // we have all histograms
  // now go ahead and have a look at them and compare with the cut values
  //
  // The information is encoded as follows (same logic as for Run-2 pp) 
  // Syntax: Bit value(ibit) - meaning if the bit is set. Set bit = bad.. 
  // Bit value = 0x1 << ibit (=2^ibit) ;
  // 'Critical' bits
  // ---------------
  // 0x1(0) - overall status: Bad 
  // 0x2(1) - overall status: Questionable (not necessarily that bad)
  //
  // 0x4(2) - Ratio 1D triggers/Min bias triggers < cut (too low rate)
  // 0x8(3) - Hits per event for Min bias triggers not in acceptable range
  // 0x10(4) - Hits per event for 1D triggers not in acceptable range
  //
  // 0x20(5) - Fraction of MB trigger events with roads found too low
  // 0x40(6) - Fraction of 1D trigger events with roads found too low
  // 0x80(7) - For MB triggers: Numbers of roads per event not within limits
  // 0x100(8) - For 1D triggers: Numbers of roads per event not within limits
  // 
  // Room for additional 'critical' bits: 
  // 0x200(9) - 
  // ..
  // 0x8000(15) - 
  //
  // 'Informational' bits
  // --------------------
  // 0x10000(16) - For MB/1D triggers: all events with roads do not have golden roads
  // 0x20000(17) - For 1D triggers: too few ghost roads roads
  // 0x40000(18) - For 1D triggers: last plane not deep enough
  // 0x80000(19) - For MB & 1D triggers: mean road-track match is not good enough
  //
  // Room for additional 'critical' bits: 
  // 0x100000(20) - 
  // ..
  // 0x80000000(31) - (if we use unsigned ints..)
  //
  
  // We'll for now assume that the number of triggers are 
  // ntriggers = NUMARM*ntypes
  // where ntype should be typically 4, and in order: MB, 1D, 1D1S and 2D
  // You perhaps noticed that only MB and 1D trigger results are presently looked at,
  // even though histograms for the others are also recorded. 
  // This follows what was used for Run-2 pp but could be updated for e.g. Run4
  // (if trigger names used when recording data can be made more or less stable,
  // or TriggerHelper can handle the changes, this will make life easier).
  //
  int ntriggersperarm = ntriggers/NUMARM;
  
  // Loop over the two arms 
  for (int arm = 0; arm < NUMARM; arm++)
    { 
      
      unsigned int muidstatus ; 
      muidstatus = 0; 
      int trigbitmb = arm*ntriggersperarm;
      //      int trigbit1d = trigbitmb + 1;
      int nmbtrig = (int) muiTriggerBit->GetBinContent(trigbitmb + 1);
      //      int n1dtrig = (int) muiTriggerBit->GetBinContent(trigbit1d + 1);
      textFile << " MUID Arm: " << arm << endl
	       << " Number of MB triggers " << nmbtrig << endl;
      //	       << " Number of 1D triggers " << n1dtrig << endl;
      float trigratio = 0.;
      if ( nmbtrig == 0 )
	{ // no min bias triggers, we can't say anything about this run
	  statusFile << 2 << " " << 2 << " "; // questionable status 
	  return;  
	}
/*
      else if ( n1dtrig == 0 )
	{ // no 1d triggers for this arm - looks bad but we can't really judge
	  statusFile << 2 << " " ; // questionable status
	  textFile << " - can't judge the status of arm " << arm << endl;  
	  textFile << " Status bits MUID arm " << arm 
		   << " (hex): 2" << endl;
	  continue; // try next arm
	} 
      else
	{
	  trigratio = n1dtrig/(float)nmbtrig;
	  textFile << " Ratio 1D/MB " << trigratio << endl;
	}
      // if we made it this far, we have something to work with
      // bit 2 check: 1D/MB ratio
      if (trigratio < PROP1DTRIG_MIN[datatype]) 
	{
	  textFile << "Trigger Ratio bit (0x4) set" << endl;
	  muidstatus += 0x4;
	}
*/
      // bit 3 check: hits per event (MB) 
      float meanhitsmb =  muiNHits[trigbitmb]->GetMean();
      textFile << " Mean number of hits (MB): " << meanhitsmb << endl;
      if (meanhitsmb < MEANNHITS_MIN_MB[datatype] 
	  || meanhitsmb > MEANNHITS_MAX_MB[datatype]) 
	{
	  textFile << "Hits per event (MB) bit (0x8) set" << endl;
	  muidstatus += 0x8; 
	}
/*      
      // bit 4 check: hits per event (1d) 
      float meanhits1d =  muiNHits[trigbit1d]->GetMean();
      textFile << " Mean number of hits (1D): " << meanhits1d << endl;
      if (meanhits1d < MEANNHITS_MIN_1D[datatype] 
	  || meanhits1d > MEANNHITS_MAX_1D[datatype]) 
	{
	  textFile << "Hits per event (1D) bit (0x10) set" << endl;
	  muidstatus += 0x10; 
	}
*/      
      // bit 5 check: fraction of events with roads (MB) 
      int evtwroadsmb =  -1;
      float fracevtwroadsmb = -1.0;
      if (muiNRoads[trigbitmb]->GetEntries() > 0) 
	{ // subtract bin 1 which has zero road entries
	  evtwroadsmb =  muiNRoads[trigbitmb]->GetEntries() - muiNRoads[trigbitmb]->GetBinContent(1);
	  fracevtwroadsmb = evtwroadsmb/(float)muiNRoads[trigbitmb]->GetEntries();
	  if (fracevtwroadsmb < EVTWROADS_MIN_MB[datatype]) 
	    {
	      textFile << "Fraction of events w. roads (MB) bit (0x20) set" << endl;
	      muidstatus += 0x20; 
	    }
	}
      textFile << " Number of events with roads (MB): " << evtwroadsmb << endl;
      textFile << " Fraction of events with roads (MB): " << fracevtwroadsmb << endl;
/*      
      // bit 6 check: fraction of events with roads (1D) 
      int evtwroads1d =  -1;
      float fracevtwroads1d = -1.0;
      if (muiNRoads[trigbit1d]->GetEntries() > 0) 
	{ // subtract bin 1 which has zero road entries
	  evtwroads1d =  muiNRoads[trigbit1d]->GetEntries() - muiNRoads[trigbit1d]->GetBinContent(1);
	  fracevtwroads1d = evtwroads1d/(float)muiNRoads[trigbit1d]->GetEntries();
	  if (fracevtwroads1d < EVTWROADS_MIN_1D[datatype]) 
	    {
	      textFile << "Fraction of events w. roads (1D) bit (0x40) set" << endl;
	      muidstatus += 0x40; 
	    }
	}
      textFile << " Number of events with roads (1D): " << evtwroads1d << endl;
      textFile << " Fraction of events with roads (1D): " << fracevtwroads1d << endl;
*/      
      int suppressbin = 11; // means: suppressing events w. #roads<=10
      int startbin = 2; // means: exclude event w. #roads<1 
      // bit 7 check:  count events with too many roads (MB)
      // (count ok bins upto suppressbin limit and subtract from total 
      int supmb = 0;
      for (int ibin = startbin; ibin <= suppressbin; ibin++)
	{
	  supmb += muiNRoads[trigbitmb]->GetBinContent(ibin);
	} 
      int nonsuppressedmb = evtwroadsmb - supmb;
      float fracnonsuppressedmb = -1.0;
      if (evtwroadsmb > 0) fracnonsuppressedmb = nonsuppressedmb / evtwroadsmb;
      float meannroadsmb = (float) muiNRoads[trigbitmb]->GetMean();
      
      if ( meannroadsmb > MEANNROADS_MAX_MB[datatype] 
	   && fracnonsuppressedmb > NROADSSUP10_MAX_MB[datatype]) 
	{ // nonsuppressed/high part is too frequent - not good
	  textFile << "Fraction of events w. large number of roads (MB) bit (0x80) set" << endl;
	  muidstatus += 0x80; 
	}
      textFile << " Number of events with roads (MB) above " << suppressbin << " : " 
	       << nonsuppressedmb << endl;
      textFile << " Fraction of non-suppressed events with roads (MB) above " << suppressbin << " : " 
	       << fracnonsuppressedmb << endl;
/*      
      // bit 8 check:  count events with too many roads (1D)
      // (count ok bins upto suppressbin limit and subtract from total 
      int sup1d = 0;
      for (int ibin = startbin; ibin <= suppressbin; ibin++)
	{
	  sup1d += muiNRoads[trigbit1d]->GetBinContent(ibin);
	} 
      int nonsuppressed1d = evtwroads1d - sup1d;
      float fracnonsuppressed1d = -1.0;
      if (evtwroads1d > 0) fracnonsuppressed1d = nonsuppressed1d / evtwroads1d;
      float meannroads1d = muiNRoads[trigbit1d]->GetMean();
      
      if ( meannroads1d > MEANNROADS_MAX_1D[datatype] &&
	   fracnonsuppressed1d > NROADSSUP10_MAX_1D[datatype]) 
	{ // nonsuppressed/high part is too frequent - not good
	  textFile << "Fraction of events w. large number of roads (1D) bit (0x100) set" << endl;
	  muidstatus += 0x100; 
	}
      textFile << " Number of events with roads (1D) above " << suppressbin << " : " 
	       << nonsuppressed1d << endl;
      textFile << " Fraction of non-suppressed events with roads (1D) above " << suppressbin << " : " 
	       << fracnonsuppressed1d << endl;
*/      
      // That was the 'critical' part - what's left is the informational part
      // (that info is kept in part of the upper 16 bits)
      
      // bit 16 check: fraction of events without golden roads 
      // MB 
      int evtwgoldenroadsmb =  -1;
      float fracevtwgoldenroadsmb = -1.0;
      if (muiNRoads_Golden[trigbitmb]->GetEntries() > 0) 
	{ // subtract bin 1 which has zero road entries
	  evtwgoldenroadsmb =  muiNRoads_Golden[trigbitmb]->GetEntries() - 
	    muiNRoads_Golden[trigbitmb]->GetBinContent(1);
	  if (evtwroadsmb > 0) fracevtwgoldenroadsmb = evtwgoldenroadsmb / (float)evtwroadsmb;
	  if (fracevtwgoldenroadsmb < GOLDPERROAD[datatype]) 
	    {
	      textFile << "Fraction of events w. golden roads (MB) bit (0x10000) set" << endl;
	      muidstatus = muidstatus | 0x10000; 
	    }
	}
      textFile << " Number of events with golden roads (MB): " << evtwgoldenroadsmb << endl;
      textFile << " Fraction of events with golden roads (MB): " << fracevtwgoldenroadsmb << endl;
/*
      // 1D
      int evtwgoldenroads1d =  -1;
      float fracevtwgoldenroads1d = -1.0;
      if (muiNRoads_Golden[trigbit1d]->GetEntries() > 0) 
	{ // subtract bin 1 which has zero road entries
	  evtwgoldenroads1d =  muiNRoads_Golden[trigbit1d]->GetEntries() - 
	    muiNRoads_Golden[trigbit1d]->GetBinContent(1);
	  if (evtwroads1d > 0) fracevtwgoldenroads1d = evtwgoldenroads1d / (float)evtwroads1d;
	  if (fracevtwgoldenroads1d < GOLDPERROAD[datatype]) 
	    {
	      textFile << "Fraction of events w. golden roads (1D) bit (0x10000) set" << endl;
	      muidstatus = muidstatus | 0x10000; 
	    }
	}
      textFile << " Number of events with golden roads (1D): " << evtwgoldenroads1d << endl;
      textFile << " Fraction of events with golden roads (1D): " << fracevtwgoldenroads1d << endl;
      
      // bit 17 check: fraction of events without ghost roads 
      // 1D
      int evtwnoghostroads1d =  -1;
      float fracevtwnoghostroads1d = -1.0;
      if (muiNRoads_NoGhost[trigbit1d]->GetEntries() > 0) 
	{ // subtract bin 1 which has zero road entries
	  evtwnoghostroads1d =  muiNRoads_NoGhost[trigbit1d]->GetEntries() - 
	    muiNRoads_NoGhost[trigbit1d]->GetBinContent(1);
	  if (evtwroads1d > 0) fracevtwnoghostroads1d = evtwnoghostroads1d / (float)evtwroads1d;
	  if (fracevtwroads1d < NOGHOPERROAD_MIN_1D[datatype]) 
	    {
	      textFile << "Fraction of events w. no-ghost roads (1D) bit (0x20000) set" << endl;
	      muidstatus = muidstatus | 0x20000; 
	    }
	}
      textFile << " Number of events with no-ghost roads (1D): " << evtwnoghostroads1d << endl;
      textFile << " Fraction of events with no-ghost roads (1D): " << fracevtwnoghostroads1d << endl;
      
      // bit 18 check: average last plane deep enough?
      // 1D
      float meanlastplane =  muiLastPlane[trigbit1d]->GetMean();
      textFile << " Mean last plane (1D): " << meanlastplane << endl;
      if (meanlastplane < MEANLASTPLANE_MIN_1D[datatype]) 
	{
	  textFile << "Mean last plane (1D) bit (0x40000) set" << endl;
	  muidstatus += 0x40000; 
	}
*/      
      // bit 19 check: road-track match enough?
      // MB
      float meanroadtrackmatchmb =  muiRoadTrack_MatchPos[trigbitmb]->GetMean();
      textFile << " Mean road-track match (MB): " << meanroadtrackmatchmb << endl;
      if (meanroadtrackmatchmb > MEANMATCHPOS_MAX_MB[datatype]) 
	{
	  textFile << "Mean road-track match (MB) bit (0x80000) set" << endl;
	  muidstatus = muidstatus | 0x80000; 
	}
/*
      // 1D
      float meanroadtrackmatch1d =  muiRoadTrack_MatchPos[trigbit1d]->GetMean();
      textFile << " Mean road-track match (1D): " << meanroadtrackmatch1d << endl;
      if (meanroadtrackmatch1d > MEANMATCHPOS_MAX_1D[datatype]) 
	{
	  textFile << "Mean road-track match (1D) bit (0x80000) set" << endl;
	  muidstatus = muidstatus | 0x80000; 
	}
*/      
      // Almost done..
      // Check the critical bits. If any of them are on, set the lowest 
      // bit also
      int check = muidstatus & 0xffff;
      if (check != 0) muidstatus = muidstatus | 1;
      textFile << " Status bits MUID arm " << arm 
	       << " (hex): " << hex << muidstatus << dec << endl; 
      // status report - for this arm
      statusFile << muidstatus << " "; 
    }// end of arm loop
  
  cout << "    ...done." << endl;
  return;
} // EOF
