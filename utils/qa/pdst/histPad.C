/*
 * histPad.C
 * $Id: histPad.C,v 1.12 2007/06/28 22:48:44 phnxbld Exp $
 *
 * Book and fill pad chamber QA histograms.
 */

#include "histPad.h"
#include "QADefs.h"

#include "dPadClusterWrapper.h"
#include "PadCluster.h"
#include "PadRaw.h"
#include "getClass.h"

#include "Fun4AllHistoManager.h"
#include "Fun4AllServer.h"

// ROOT header files
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"

#include <iostream>
#include <cmath>

using namespace std;

// PC1 HV is organized in the following way: 9+20+20+9 i.e 0-8,9-28,29-48,49-57
// HV is shared for both North and South
// index 0-31 is equal to sect*4+hvsectindex (sect=0..7, hvsectindex=0..3)
#define PC1HVSEC1 8 
#define PC1HVSEC2 28
#define PC1HVSEC3 48

// PC2 HV is organized in the following way: 24+34+34+24 i.e 0-23,24-57,58-91,92-115
// HV is not shared for North and South
// index 0-31 is equal to side*16+sect*4+hvsectindex (side=0,1, sect=0..4, hvsectindex=0..3)
#define PC2HVSEC1 23 
#define PC2HVSEC2 57
#define PC2HVSEC3 91

// PC3 HV is organized in the following way: 29+29+29+29 i.e 0-28,29-57,58-86,87-115
// HV is not shared for North and South
// index 0-31 is equal to side*16+sect*4+hvsectindex (side=0,1, sect=0..4, hvsectindex=0..3)
#define PC3HVSEC1 28 
#define PC3HVSEC2 57
#define PC3HVSEC3 86

// degrees per radian (for conversions)
const float DEGRAD = 57.295779513;

// Declare pad chamber histograms
TH2F *pc1EastThetaPhi;
TH2F *pc1WestThetaPhi;
TProfile *pc1EastActivity;
TProfile *pc1WestActivity;
TH1F *pc1EastRatio;
TH1F *pc1WestRatio;

TH2F *pc2WestThetaPhi;
TProfile *pc2WestActivity;
TH1F *pc2WestRatio;

TH2F *pc3EastThetaPhi;
TH2F *pc3WestThetaPhi;
TProfile *pc3EastActivity;
TProfile *pc3WestActivity;
TH1F *pc3EastRatio;
TH1F *pc3WestRatio;
TH2F *pc1rawEastXZ;
TH2F *pc1rawWestXZ;
TH2F *pc2rawWestXZ;
TH2F *pc3rawEastXZ;
TH2F *pc3rawWestXZ;

// global counters: init to 0
int Pc1EastHv[32] = {};
int Pc1WestHv[32] = {};

int Pc2WestHv[32] = {};

int Pc3EastHv[32] = {};
int Pc3WestHv[32] = {};

int Pc1EastOneCell[32] = {};
int Pc1EastTwoCell[32] = {};
int Pc1WestOneCell[32] = {};
int Pc1WestTwoCell[32] = {};

int Pc2WestOneCell[32] = {};
int Pc2WestTwoCell[32] = {};

int Pc3EastOneCell[32] = {};
int Pc3EastTwoCell[32] = {};
int Pc3WestOneCell[32] = {};
int Pc3WestTwoCell[32] = {};


//-----------------------------------------------------------------------
int QAPad::InitRun(PHCompositeNode *topNode)
{
  PadCluster * pc1cluster = NULL;
  pc1cluster = findNode::getClass<PadCluster>(topNode, "Pc1Cluster");
  
  PadCluster * pc2cluster = NULL;
  pc2cluster = findNode::getClass<PadCluster>(topNode, "Pc2Cluster");

  PadCluster * pc3cluster = NULL;
  pc3cluster = findNode::getClass<PadCluster>(topNode, "Pc3Cluster");
  
  // check the required input, if it is missing, unregister myself
  // so Fun4All doesn't call me anymore
  Fun4AllServer *se = Fun4AllServer::instance();
  if (!pc1cluster || !pc2cluster || !pc3cluster) 
    {
      se->unregisterSubsystem(this);
      return 0;
    }

  // check for HistoManager - if it does not exist, create it
  // HistoManagerName is defined in QADefs.h
  Fun4AllHistoManager *hm = se->getHistoManager(HistoManagerName);
  if (!hm)
    {
      hm = new Fun4AllHistoManager(HistoManagerName);
      se->registerHistoManager(hm);
    }

  // PC1 East
  pc1EastThetaPhi = new TH2F("pc1EastThetaPhi", "PC1 East Theta vs. Phi",
      	      	      	     100, 65.0, 115.0, 240, 120.0, 240.0);
  hm->registerHisto(pc1EastThetaPhi);
  pc1EastThetaPhi->SetMarkerStyle(4);
  pc1EastThetaPhi->SetMarkerSize(0.2);
  
  pc1EastActivity = new TProfile("pc1EastActivity",
				 "PC1 EAST sectprofile clusters per event wire",
				 32, -0.5, 31.5);
  hm->registerHisto(pc1EastActivity);
  pc1EastRatio = new TH1F("pc1EastRatio",
			  "PC1 EAST ratios (2-cells to 1-cell)",
			  32, -0.5, 31.5);
  hm->registerHisto(pc1EastRatio);
  pc1EastRatio->SetStats(0);

  // PC1 West
  pc1WestThetaPhi = new TH2F("pc1WestThetaPhi", "PC1 West Theta vs. Phi",
      	      	      	     100, 65.0, 115.0, 240, -40.0, 80.0);
  hm->registerHisto(pc1WestThetaPhi);
  pc1WestThetaPhi->SetMarkerStyle(4);
  pc1WestThetaPhi->SetMarkerSize(0.2);
  
  
  pc1WestActivity = new TProfile("pc1WestActivity",
				 "PC1 West sectprofile clusters per event wire",
				 32, -0.5, 31.5);
  hm->registerHisto(pc1WestActivity);
  pc1WestRatio = new TH1F("pc1WestRatio",
			  "PC1 WEST ratios (2-cells to 1-cell)",
			  32, -0.5, 31.5); 
  hm->registerHisto(pc1WestRatio);

  // PC2 West
  pc2WestThetaPhi = new TH2F("pc2WestThetaPhi", "PC2 WEST Theta vs. Phi", 
			     100, 65.0, 115.0, 240, -40.0, 80.0);
  hm->registerHisto(pc2WestThetaPhi);
  pc2WestThetaPhi->SetMarkerStyle(4);
  pc2WestThetaPhi->SetMarkerSize(0.2);
  
  pc2WestActivity = new TProfile("pc2WestActivity",
				 "PC2 West sectprofile clusters per event wire",
				 32, -0.5, 31.5);
  hm->registerHisto(pc2WestActivity);
  pc2WestRatio = new TH1F("pc2WestRatio",
			  "PC2 WEST ratios (2-cells to 1-cell)",
			  32, -0.5, 31.5); 
  hm->registerHisto(pc2WestRatio);
  pc2WestRatio->SetStats(0);
  // PC3 East
  pc3EastThetaPhi = new TH2F("pc3EastThetaPhi", "PC3 EAST Theta vs. Phi",
      	      	      	     100, 65.0, 115.0, 240, 120.0, 240.0);
  hm->registerHisto(pc3EastThetaPhi);
  pc3EastThetaPhi->SetMarkerStyle(4);
  pc3EastThetaPhi->SetMarkerSize(0.2);
  
  pc3EastActivity = new TProfile("pc3EastActivity",
				 "PC3 EAST sectprofile clusters per event wire",
				 32, -0.5, 31.5);
  hm->registerHisto(pc3EastActivity);
  pc3EastRatio = new TH1F("pc3EastRatio",
			  "PC3 EAST ratios (2-cells to 1-cell)",
			  32, -0.5, 31.5); 
  hm->registerHisto(pc3EastRatio);
  pc3EastRatio->SetStats(0);
  // PC3 West
  pc3WestThetaPhi = new TH2F("pc3WestThetaPhi", "PC3 WEST Theta vs. Phi",
      	      	      	     100, 65.0, 115.0, 240, -40.0, 80.0);
  hm->registerHisto(pc3WestThetaPhi);
  pc3WestThetaPhi->SetMarkerStyle(4);
  pc3WestThetaPhi->SetMarkerSize(0.2);
  
  pc3WestActivity = new TProfile("pc3WestActivity",
				 "PC3 WEST sectprofile clusters per event wire",
				 32, -0.5, 31.5);
  hm->registerHisto(pc3WestActivity);
  pc3WestRatio = new TH1F("pc3WestRatio",
			  "PC3 WEST ratios (2-cells to 1-cell)",
			  32, -0.5, 31.5); 
  hm->registerHisto(pc3WestRatio);
  pc3WestRatio->SetStats(0);

  pc1rawEastXZ = new TH2F("pc1rawEastXZ","Pc1 Raw East XZ",216,-0.5,215.5,160,-0.5,159.5);
  pc1rawEastXZ->SetXTitle("side*108 + padz");
  pc1rawEastXZ->SetYTitle("sector*20 + padx");
  hm->registerHisto(pc1rawEastXZ);
  pc1rawWestXZ = new TH2F("pc1rawWestXZ","Pc1 Raw West XZ",216,-0.5,215.5,160,-0.5,159.5);
  pc1rawWestXZ->SetXTitle("side*108 + padz");
  pc1rawWestXZ->SetYTitle("sector*20 + padx");
  hm->registerHisto(pc1rawWestXZ);
  pc2rawWestXZ = new TH2F("pc2rawWestXZ","Pc2 Raw West XZ",216,-0.5,215.5,160,-0.5,159.5);
  pc2rawWestXZ->SetXTitle("side*108 + padz");
  pc2rawWestXZ->SetYTitle("sector*40 + padx");
  hm->registerHisto(pc2rawWestXZ);
  pc3rawEastXZ = new TH2F("pc3rawEastXZ","Pc3 Raw East XZ",216,-0.5,215.5,160,-0.5,159.5);
  pc3rawEastXZ->SetXTitle("side*108 + padz");
  pc3rawEastXZ->SetYTitle("sector*40 + padx");
  hm->registerHisto(pc3rawEastXZ);
  pc3rawWestXZ = new TH2F("pc3rawWestXZ","Pc3 Raw West XZ",216,-0.5,215.5,160,-0.5,159.5);
  pc3rawWestXZ->SetXTitle("side*108 + padz");
  pc3rawWestXZ->SetYTitle("sector*40 + padx");
  hm->registerHisto(pc3rawWestXZ);

  return 0;
}

//-----------------------------------------------------------------------

int QAPad::process_event(PHCompositeNode *topNode)
{
  PadCluster * pc1cluster = NULL;
  pc1cluster = findNode::getClass<PadCluster>(topNode, "Pc1Cluster");
    
  PadCluster * pc2cluster = NULL;
  pc2cluster = findNode::getClass<PadCluster>(topNode, "Pc2Cluster");

  PadCluster * pc3cluster = NULL;
  pc3cluster = findNode::getClass<PadCluster>(topNode, "Pc3Cluster");

  if (!pc1cluster || !pc2cluster || !pc3cluster) 
    {
      return 0;
    }

  PadRaw * pc1raw = NULL;
  pc1raw = findNode::getClass<PadRaw>(topNode, "Pc1Raw");

  PadRaw * pc2raw = NULL;
  pc2raw = findNode::getClass<PadRaw>(topNode, "Pc2Raw");

  PadRaw * pc3raw = NULL;
  pc3raw = findNode::getClass<PadRaw>(topNode, "Pc3Raw");

  //  int nPc1 = pc1cluster->RowCount();   // total # of PC1 clusters
  int nPc1 = pc1cluster->get_PadNCluster();
  //  cout << "padHistFill start nPc1 = " << nPc1 << endl;
  
  int iarm,iwire,isec,index;
  int Pc1EastHvThisEvent[32] = {};
  int Pc1WestHvThisEvent[32] = {};
  
  float x,y,z,phi,theta;
  float xval, yval, weight, ratio, ratioerr;
  
  // Loop over PC1 clusters
  for (int i=0; i < nPc1; i++)
    {
      //cout << "i = " << i << endl;
      x = pc1cluster->get_xyz(i,0);
      y = pc1cluster->get_xyz(i,1);
      z = pc1cluster->get_xyz(i,2);
      
      phi = DEGRAD * atan2(y,x);
      if (phi < -90.0)
	{
	  phi += 360.0;
	}
      theta = DEGRAD * acos(z/sqrt(x*x + y*y+ z*z));
      
      if (x < 0)	// East
	{
	  pc1EastThetaPhi->Fill(theta, phi);
	}
      else // West (i.e. x>0)
	{
	  pc1WestThetaPhi->Fill(theta, phi);
	} // End of east-west check
      
      iarm = pc1cluster->get_arm(i);
      isec = pc1cluster->get_sector(i);
      iwire = pc1cluster->get_wire(i);
      index = 4 * isec;
      
      if (iwire > PC1HVSEC1) 
	{ 
	  if (iwire <= PC1HVSEC2) index++;
	  else if (iwire <= PC1HVSEC3) index += 2;
	  else index += 3;
	}

      if (iarm == 0)
	{
	  Pc1EastHvThisEvent[index]++;
	  if (pc1cluster->get_type(i) == 1)
	    {
	      Pc1EastOneCell[index]++;
	    }
	  else if (pc1cluster->get_type(i) == 2)
	    {
	      Pc1EastTwoCell[index]++;
	    }
	}
      else
	{
	  Pc1WestHvThisEvent[index]++;
	  if (pc1cluster->get_type(i) == 1)
	    {
	      Pc1WestOneCell[index]++;
	    }
	  else if (pc1cluster->get_type(i) == 2)
	    {
	      Pc1WestTwoCell[index]++;
	    }
	}

    } // End of loop over PC1 clusters
 
  // Now fill east and west profile histograms
  for (int i = 0; i < 32; i++)
    {
      // Since the HV sectors have a non-equal size/# of wires,
      // we normalize by the number of wires in the sector
      if ( ((i%4)==0) || ((i%4)==3) )   
	{ // outer HV sectors of a chamber are smaller (9 wires)
	  weight = 1/9.0;
	}
      else
	{ // inner HV sectors of a chamber are larger (20 wires)
	  weight = 1/20.0;
	}
      xval = i;
      yval = Pc1EastHvThisEvent[i] * weight;
      pc1EastActivity->Fill(xval, yval);
      
      yval = Pc1WestHvThisEvent[i] * weight;
      pc1WestActivity->Fill(xval, yval);
      
      if ( (Pc1EastOneCell[i] > 0) && (Pc1EastTwoCell[i] > 0) )
	{
	  ratio = (float) Pc1EastTwoCell[i]/Pc1EastOneCell[i];
	  // the relative error in the ratio is the sqrt of the other relative errors squared
	  ratioerr = (float) ratio*sqrt(1.0/Pc1EastOneCell[i] + 1.0/Pc1EastTwoCell[i]);
	  
	}
      else 
	{ // something failed.. 
	  ratio = -1.0;
	  ratioerr = 1.0;
	}
      
      pc1EastRatio->SetBinContent(i+1,ratio);
      pc1EastRatio->SetBinError(i+1,ratioerr);
      
      if ( (Pc1WestOneCell[i] > 0) && (Pc1WestTwoCell[i] > 0) )
	{
	  ratio = (float) Pc1WestTwoCell[i]/Pc1WestOneCell[i];
	  // the relative error in the ratio is the sqrt of the other relative errors squared
	  ratioerr = (float) ratio*sqrt(1.0/Pc1WestOneCell[i] + 1.0/Pc1WestTwoCell[i]);
	  
	}
      else 
	{ // something failed.. 
	  ratio = -1.0;
	  ratioerr = 1.0;
	}
      
      pc1WestRatio->SetBinContent(i+1,ratio);
      pc1WestRatio->SetBinError(i+1,ratioerr);
      
      Pc1EastHv[i] += Pc1EastHvThisEvent[i];
      Pc1WestHv[i] += Pc1WestHvThisEvent[i];
    } // end loop to fill east/west profiles  
  // Finished with PC1
  
  // Now do PC2  
  //  int nPc2 = pc2cluster->RowCount();   // total # of PC2 clusters
  int nPc2 = pc2cluster->get_PadNCluster();

  //int iarm,iwire,isec,index;
  int Pc2WestHvThisEvent[32] = {};
  
  // Loop over PC2 clusters
  for (int i=0; i < nPc2; i++)
    {
      x = pc2cluster->get_xyz(i,0);
      y = pc2cluster->get_xyz(i,1);
      z = pc2cluster->get_xyz(i,2);
      
      phi = DEGRAD * atan2(y,x);
      if (phi < -90.0)
	{
	  phi += 360.0;
	}
      theta = DEGRAD * acos(z/sqrt(x*x + y*y+ z*z));
      
      
      if (x > 0)   // West
	{
	  pc2WestThetaPhi->Fill(theta, phi);
	}
      else  // Data from 2001 run shouldn't have pc2 east hits!
	{
	  // Should probably add event number to error msg
	  cerr << "Error PC2 X=" << x << endl;
	} // End of east-west check
      
      iarm = pc2cluster->get_arm(i);
      isec = pc2cluster->get_sector(i);
      iwire = pc2cluster->get_wire(i);
      index = 4 * isec;
      
      if (z > 0)	  //North
	index += 16;
      
      if (iwire > PC2HVSEC1) 
	{
	  if (iwire <= PC2HVSEC2) index ++;
	  else if (iwire <= PC2HVSEC3) index += 2;
	  else index += 3;
	}
      
      if (iarm == 1)
	{
	  Pc2WestHvThisEvent[index]++;
	  if (pc2cluster->get_type(i) == 1)
	    {
	      Pc2WestOneCell[index]++;
	    }
	  else if (pc2cluster->get_type(i) == 2)
	    {
	      Pc2WestTwoCell[index]++;
	    }
	}
    }  // End of loop over PC2 clusters
    
  // Fill the PC2 west profile histograms
  for (int i = 0; i < 32; i++)
    {
      // Since the HV sectors have a non-equal size/# of wires,
      // we normalize by the number of wires in the sector
      // The factor 2 is due to geometry
      if ( ((i%4)==0) || ((i%4)==3) )   
	{ // outer HV sectors of a chamber are smaller (24 wires)
	  weight = 2/24.0;
	}
      else
	{ // inner HV sectors of a chamber are larger (34 wires)
	  weight = 2/34.0;
	}
      
      xval = i;
      yval = Pc2WestHvThisEvent[i] * weight;
      pc2WestActivity->Fill(xval, yval);
      

      if ( (Pc2WestOneCell[i] > 0) && (Pc2WestTwoCell[i] > 0) )
	{
	  ratio = (float) Pc2WestTwoCell[i]/Pc2WestOneCell[i];
	  // the relative error in the ratio is the sqrt of the other relative errors squared
	  ratioerr = (float) ratio*sqrt(1.0/Pc2WestOneCell[i] + 1.0/Pc2WestTwoCell[i]);
	  
	}
      else 
	{ // something failed.. 
	  ratio = -1.0;
	  ratioerr = 1.0;
	}
      
      pc2WestRatio->SetBinContent(i+1,ratio);
      pc2WestRatio->SetBinError(i+1,ratioerr);

      Pc2WestHv[i] += Pc2WestHvThisEvent[i];
    } // end loop to fill west profile  
  // PC2 done
  
  // Now do PC3
  //  int nPc3 = pc3cluster->RowCount(); // total # of PC3 clusters
  int nPc3 = pc3cluster->get_PadNCluster();
  
  // int iarm,iwire,isec,index;
  int Pc3EastHvThisEvent[32] = {};
  int Pc3WestHvThisEvent[32] = {};
  
  // Loop over PC3 clusters
  for (int i=0; i < nPc3; i++)
    {
      x = pc3cluster->get_xyz(i,0);
      y = pc3cluster->get_xyz(i,1);
      z = pc3cluster->get_xyz(i,2);
      
      phi = DEGRAD * atan2(y,x);
    	if (phi < -90.0)
	  {	
	    phi += 360.0;
	  }
	theta = DEGRAD * acos(z/sqrt(x*x + y*y + z*z));
      
	if (x < 0)  // East
	  {
	    pc3EastThetaPhi->Fill(theta, phi);
	  }
	else  // West
	  {
	    pc3WestThetaPhi->Fill(theta, phi);
	  } // End of east-west check
      
	iarm = pc3cluster->get_arm(i);
	isec = pc3cluster->get_sector(i);
	iwire = pc3cluster->get_wire(i);
	index = 4 * isec;
	
	if (z > 0)	  //North
	  index += 16;
	
	if (iwire > PC3HVSEC1)
	  {
	    if (iwire <= PC3HVSEC2) index ++;
	    else if (iwire <= PC3HVSEC3) index += 2;
	    else index += 3;
	  }
	
	if (iarm == 0)
	  {
	    Pc3EastHvThisEvent[index]++;
	    if (pc3cluster->get_type(i) == 1)
	      {
		Pc3EastOneCell[index]++;
	      }
	    else if (pc3cluster->get_type(i) == 2)
	      {
		Pc3EastTwoCell[index]++;
	      }
	  }
	else
	  {
	    Pc3WestHvThisEvent[index]++;
	    if (pc3cluster->get_type(i) == 1)
	      {
		Pc3WestOneCell[index]++;
	      }
	    else if (pc3cluster->get_type(i) == 2)
	      {
		Pc3WestTwoCell[index]++;
	      }
	  }
	
    } // End of loop over PC3 clusters
  
  // Fill the PC3 east and west profile histogram
  for (int i=0; i<32; i++)
    {
      // Since the HV sectors for PC3 have an equal size/# of wires,
      // we normalize by the number of wires in the sector
      // The factor 2 is due to geometry
      
      weight = 2/29.0;
      
      xval = i;
      yval = Pc3EastHvThisEvent[i]*weight;
      pc3EastActivity->Fill(xval,yval);
      
      yval = Pc3WestHvThisEvent[i]*weight;
      pc3WestActivity->Fill(xval,yval);
      
      if ( (Pc3EastOneCell[i] > 0) && (Pc3EastTwoCell[i] > 0) )
	{
	  ratio = (float) Pc3EastTwoCell[i]/Pc3EastOneCell[i];
	  // the relative error in the ratio is the sqrt of the other relative errors squared
	  ratioerr = (float) ratio*sqrt(1.0/Pc3EastOneCell[i] + 1.0/Pc3EastTwoCell[i]);
	  
	}
      else 
	{ // something failed.. 
	  ratio = -1.0;
	  ratioerr = 1.0;
	}
      
      pc3EastRatio->SetBinContent(i+1,ratio);
      pc3EastRatio->SetBinError(i+1,ratioerr);
      
      if ( (Pc3WestOneCell[i] > 0) && (Pc3WestTwoCell[i] > 0) )
	{
	  ratio = (float) Pc3WestTwoCell[i]/Pc3WestOneCell[i];
	  // the relative error in the ratio is the sqrt of the other relative errors squared
	  ratioerr = (float) ratio*sqrt(1.0/Pc3WestOneCell[i] + 1.0/Pc3WestTwoCell[i]);
	  
	}
      else 
	{ // something failed.. 
	  ratio = -1.0;
	  ratioerr = 1.0;
	}
      
      pc3WestRatio->SetBinContent(i+1,ratio);
      pc3WestRatio->SetBinError(i+1,ratioerr);
      
      Pc3EastHv[i] += Pc3EastHvThisEvent[i];
      Pc3WestHv[i] += Pc3WestHvThisEvent[i];
    }

  //cout << "padHistFill end" << endl;
  int idx, idz;
  for (unsigned int ipad=0; ipad<pc1raw->get_PadNRaw(); ipad++)
    {
      if (!pc1raw->get_arm(ipad))
	{
	  idx = pc1raw->get_sector(ipad)*20 + pc1raw->get_padx(ipad);
	  idz = pc1raw->get_side(ipad)*108 + pc1raw->get_padz(ipad);
	  pc1rawEastXZ->Fill(idz,idx);
	}
      else
	{
	  idx = pc1raw->get_sector(ipad)*20 + pc1raw->get_padx(ipad);
	  idz = pc1raw->get_side(ipad)*108 + pc1raw->get_padz(ipad);
	  pc1rawWestXZ->Fill(idz,idx);
	}
    }
  
  for (unsigned int ipad=0; ipad<pc2raw->get_PadNRaw(); ipad++)
    {
      if (pc2raw->get_arm(ipad))
	{
	  idx = pc2raw->get_sector(ipad)*40 + pc2raw->get_padx(ipad);
	  idz = pc2raw->get_side(ipad)*108 + pc2raw->get_padz(ipad);
	  pc2rawWestXZ->Fill(idz,idx);
	}
    }

  for (unsigned int ipad=0; ipad<pc3raw->get_PadNRaw(); ipad++)
    {
      if (!pc3raw->get_arm(ipad))
	{
	  idx = pc3raw->get_sector(ipad)*40 + pc3raw->get_padx(ipad);
	  idz = pc3raw->get_side(ipad)*108 + pc3raw->get_padz(ipad);
	  pc3rawEastXZ->Fill(idz,idx);
	}
      else
	{
	  idx = pc3raw->get_sector(ipad)*40 + pc3raw->get_padx(ipad);
	  idz = pc3raw->get_side(ipad)*108 + pc3raw->get_padz(ipad);
	  pc3rawWestXZ->Fill(idz,idx);
	}
    }

  return 0;
}

// EOF
