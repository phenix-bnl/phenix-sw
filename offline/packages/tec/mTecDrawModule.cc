#include <gsl/gsl_math.h>
#include <gsl/gsl_rng.h>
#include <PHNode.h>
#include <PHIODataNode.h>
#include <PHTypedNodeIterator.h>
#include <getClass.h>
#include <mTecDrawModule.h>
#include <TecOutV1.hh>
#include <PadCluster.h>
#include <mTecUtilities.h>

#include <TROOT.h>
#include <TFile.h>
#include <TH1.h>
#include <TH2.h>
#include <TCanvas.h>
#include <TGraph.h>
#include <TLine.h>
#include <TText.h>
#include <TSystem.h>
#include <TBenchmark.h>
#include <TApplication.h>

#include <Getline.h>

#include <TDragZoomTH2F.hh>
#include <TSignalingTButton.hh>

#include <cmath>
#include <cstdlib>
#include <cstdio>
#include <iostream>
#include <fstream>
#include <vector>

using namespace std;

typedef PHIODataNode<PadCluster> PadClusterNode_t;
typedef PHIODataNode<TecOutV1> TecOutNode_t;
typedef PHIODataNode < TObject > ObjectNode_t;

//=========================================================================

mTecDrawModule::mTecDrawModule ()
{
  BinThreshold=8.;
  DrawMarkerSize = 0.5;
  DrawSize[0][0] = 640;
  DrawSize[0][1] = 720;
  DrawSize[1][0] = 600;
  DrawSize[1][1] = 800;
  DrawSize[2][0] = 640;
  DrawSize[2][1] = 720;
  DrawSize[3][0] = 800;
  DrawSize[3][1] = 800;
  DrawSize[4][0] = 500;
  DrawSize[4][1] = 975;
  DrawXY[0][0] = -520.;
  DrawXY[0][1] = -275.;
  DrawXY[0][2] = -360.;
  DrawXY[0][3] = -75.;
  DrawXY[1][0] = -550.;
  DrawXY[1][1] = -100.;
  DrawXY[1][2] = -400.;
  DrawXY[1][3] = 100.;
  DrawXY[2][0] = -520.;
  DrawXY[2][1] = 90.;
  DrawXY[2][2] = -360.;
  DrawXY[2][3] = 270.;
  DrawXY[3][0] = -440.;
  DrawXY[3][1] = 240.;
  DrawXY[3][2] = -240.;
  DrawXY[3][3] = 440.;
  DrawXY[4][0] = -550.;
  DrawXY[4][1] = -110.;
  DrawXY[4][2] = -350.;
  DrawXY[4][3] = 280.;
  fNumberOfCalls = 0; 
  Verbose = 0;
}

//=====================================================================

PHBoolean
mTecDrawModule::event (PHCompositeNode * topNode) {
  
  cout << "mTecDrawModule::event() is a dummy method." << endl;
  cout << "  Use Draw() methods to draw TEc hits and tracks." << endl;
  return True;
  
}

//=====================================================================

PHBoolean
mTecDrawModule::Draw(PHCompositeNode * root, EPHENIXSector Sector, EPHENIXSide Side,
		     TecGeometryObject* TGO, TecCalibrationObject* TCO)
{
  const char *fname = " ";
  int eventNumber = -1;
  int delay = -1;
  return mTecDrawModule::Draw(root, Sector, Side, eventNumber, fname, delay, TGO, TCO);
}

// SLSL0
PHBoolean
mTecDrawModule::Draw(PHCompositeNode * root, EPHENIXSector Sector, EPHENIXSide Side)
{
  const char *fname = " ";
  int eventNumber = -1;
  int delay = -1;
  return mTecDrawModule::Draw(root, Sector, Side, eventNumber, fname, delay);
}

PHBoolean
mTecDrawModule::Draw (PHCompositeNode * root,
                            EPHENIXSector Sector,
                            EPHENIXSide Side,
                            int delay)
{
  const char *fname = " ";
  int eventNumber = -1;
  return mTecDrawModule::Draw(root, Sector, Side, eventNumber, fname, delay);
}


PHBoolean
mTecDrawModule::Draw (PHCompositeNode * root,
                            EPHENIXSector Sector,
                            EPHENIXSide Side,
                            int eventNumber,
                            const char *fname)
{
  int delay = -1;
  return mTecDrawModule::Draw(root, Sector, Side, eventNumber, fname, delay);
}




//============================================================================
// SLSL1
PHBoolean
mTecDrawModule::Draw (PHCompositeNode * root,
                            EPHENIXSector Sector,
                            EPHENIXSide Side,
                            int eventNumber,
                            const char *fname,
                            int delay)
{
  int width = DrawSize[Sector][0];

  if (Side != kBoth) {
    DrawEventInNewWindow(root, Sector, Side, eventNumber, fname, 0, 0);
  }
  else {
    BinThreshold=0;
    DrawEventInNewWindow(root, Sector, kSouth, eventNumber, fname, 0, 0);
    //DrawEventInNewWindow(root, Sector, kNorth, eventNumber, fname, 0, 0);
    BinThreshold=4;
    DrawEventInNewWindow(root, Sector, kSouth, eventNumber, fname, width + 5, 0);
    //DrawEventInNewWindow(root, Sector, kNorth, eventNumber, fname, width + 5, 0);
  }

  if (delay < 0) //Wait for user input before returning.
    {

      //It takes a little work to get user input at the console without
      //blocking updates to the graphical interface.
      //The following code sets up a timer, which interrupts the
      //code waiting for user keyboard input and updates the
      //the graphical interface. If there has been no user input,
      //the timer is restarted and the process starts again.

      //Getline takes care of the the char* it returns;
      //we don't need to worry about cleaning up this memory.
      //See Getline.C in the ROOT sources for more info.

      const char    *input; // Holds user input;

      TTimer  *timer = new TTimer("gSystem->ProcessEvents();", 250, kFALSE);
      do {
        timer->TurnOn();
        timer->Reset();
        input =
          Getline("Press <return> to continue, q then <return> to quit.");
        timer->TurnOff();
      } while (!input);

      delete timer;

      //Quit if the user entered "q" or ".q".
      if (input[0] == 'q' ||
          strstr(input, ".q") == input) {
        gApplication->Terminate(0);
      }

    }
  else
    {
      gSystem->Sleep (delay * 1000);
    }

  //Graphics objects are now cleaned up automatically
  //when the TCanvas is closed, at least in theory.

  return True;
}


//============================================================================

PHBoolean
mTecDrawModule::Draw (PHCompositeNode * root,
                            EPHENIXSector Sector,
                            EPHENIXSide Side,
                            int eventNumber,
                            const char *fname,
                            int delay, 
			    TecGeometryObject* TGO,
			    TecCalibrationObject* TCO)
{
  int width = DrawSize[Sector][0];

  if (Side != kBoth) {
    DrawEventInNewWindow(root, Sector, Side, eventNumber, fname, 0, 0, TGO, TCO);
  }
  else {
    DrawEventInNewWindow(root, Sector, kSouth, eventNumber, fname, 0, 0, TGO, TCO);
    DrawEventInNewWindow(root, Sector, kNorth, eventNumber, fname, width + 5, 0, TGO, TCO);
  }

  if (delay < 0) //Wait for user input before returning.
    {

      //It takes a little work to get user input at the console without
      //blocking updates to the graphical interface.
      //The following code sets up a timer, which interrupts the
      //code waiting for user keyboard input and updates the
      //the graphical interface. If there has been no user input,
      //the timer is restarted and the process starts again.

      //Getline takes care of the the char* it returns;
      //we don't need to worry about cleaning up this memory.
      //See Getline.C in the ROOT sources for more info.

      const char    *input; // Holds user input;

      TTimer  *timer = new TTimer("gSystem->ProcessEvents();", 250, kFALSE);
      do {
        timer->TurnOn();
        timer->Reset();
        input =
          Getline("Press <return> to continue, q then <return> to quit.");
        timer->TurnOff();
      } while (!input);

      delete timer;

      //Quit if the user entered "q" or ".q".
      if (input[0] == 'q' ||
          strstr(input, ".q") == input) {
        gApplication->Terminate(0);
      }

    }
  else
    {
      gSystem->Sleep (delay * 1000);
    }

  //Graphics objects are now cleaned up automatically
  //when the TCanvas is closed, at least in theory.

  return True;
}





const char*
mTecDrawModule::GetTecNameStringFor(Int_t sector,
                                          Int_t side)
{
  //Return the name for the given side and sector of
  //the TEC.
  //This method will return an empty string if it doesn't.
  //find a match for the given sector and side.

  //This method was created to allow both the DrawEventInNewWindow
  //and DrawEventInCurrentPad methods to efficiently access
  //the name of the sector/side.

  //Maybe we should use a table to hold the possible names
  //to allow us to do something like "return name[side*sector];".

  const char* nameString = " ";
  if (sector == kSector0)
    {
      if (side == kSouth)
        nameString = "  TEC East Arm, Sector 0, South Side  ";
      if (side == kNorth)
        nameString = "  TEC East Arm, Sector 0, North Side  ";
    }
  else if (sector == kSector1)
    {
      if (side == kSouth)
        nameString = "  TEC East Arm, Sector 1, South Side  ";
      if (side == kNorth)
        nameString = "  TEC East Arm, Sector 1, North Side  ";
    }
  else if (sector == kSector2)
    {
      if (side == kSouth)
        nameString = "  TEC East Arm, Sector 2, South Side  ";
      if (side == kNorth)
        nameString = "  TEC East Arm, Sector 2, North Side  ";
    }
  else if (sector == kSector3)
    {
          if (side == kSouth)
            nameString = "  TEC East Arm, Sector 3, South Side  ";
          if (side == kNorth)
            nameString = "  TEC East Arm, Sector 3, North Side  ";
    }
  return nameString;
}

// SLSL2
void
mTecDrawModule::DrawEventInNewWindow (PHCompositeNode * root,
                                            EPHENIXSector sector,
                                            EPHENIXSide side,
                                            int eventNumber,
                                            const char *fname,
                                            Int_t xCanvasPosition,
                                            Int_t yCanvasPosition)
{
  //Draw a display of the specified event in the specified sector
  //and side of the TEC in a new TCanvas. All of
  //the objects created to make the display have their kCanDelete
  //bit set.
  //For the Side argument, this method will accept only kNorth or
  //kSouth, not kBoth.
  //xCanvasPosition and yCanvasPosition specify the coordinates of the
  //new TCanvas's upper left hand corner.

  if (side == kBoth) {
    if (Verbose > 0) {
      cerr << "kBoth passed to mTecDrawModule::DrawEventInNewWindow()"
           << endl;
    }
    return;
  }

  Int_t myNumOfCalls = fNumberOfCalls++;

  if (Verbose > 0)
    {
      cout << "mTecDrawModule::DrawEventInNewWindow(): Started..." << endl;
    }

  const char *myTitle = GetTecNameStringFor(sector, side);
  int width = DrawSize[sector][0];
  int height = DrawSize[sector][1];

  Char_t *cName = GetUniqueName("canvas", myNumOfCalls);

  TCanvas *canvas = new TCanvas(cName, myTitle, xCanvasPosition, yCanvasPosition, width, height);
  delete [] cName;

  canvas->cd();

  Char_t *padName = GetUniqueName("displayPad", myNumOfCalls);
  TPad *displayPad = new TPad(padName, "", 0.0107239,0.141053,0.983914,0.985263);
  //displayPad is used to hold the display of the event.
  displayPad->SetBit(kCanDelete);
  delete [] padName;
  displayPad->Draw(); //Associate displayPad with the current gPad.

  //Draw event number and file name.
  char eventNumberName[13];

  if (eventNumber >= 0)
    {

      if (eventNumber < 10000) {
        sprintf(eventNumberName, "Event # %d", eventNumber);
      }
      else {
        sprintf(eventNumberName, "Event # %d", 0);
      }

      TText *textevt = new TText(0.0107239,0.107, eventNumberName);
      //In coordinates of pad rather than plot.

      textevt->SetBit(kCanDelete);
      textevt->SetTextSize (0.035);
      textevt->Draw();

      TText *textfn = new TText(0.230563,0.107, fname);
      textfn->SetBit(kCanDelete);
      textfn->SetTextSize (0.0215);
      textfn->Draw();
    }

  TSignalingTButton *resetButton = new TSignalingTButton("Reset view", 0.442211, 0.0117647, 0.748744, 0.0870588);
  resetButton->SetBit(kCanDelete);
  resetButton->Draw();

  TSignalingTButton *closeButton = new TSignalingTButton("Close", 0.773869, 0.0117647, 0.983914, 0.0870588);
  closeButton->SetBit(kCanDelete);
  closeButton->Draw();
  closeButton->Connect("Released()","TCanvas",gPad->GetCanvas(),"Close()");

  displayPad->cd(); //Set the current pad (gPad) to displayPad.
  TDragZoomTH2F* histo = DrawEventInCurrentPad(root, sector, side);
  resetButton->Connect("Released()","TDragZoomTH2F",histo,"ResetView()");

  canvas->Modified();
  canvas->Update();
}



void
mTecDrawModule::DrawEventInNewWindow (PHCompositeNode * root,
                                            EPHENIXSector sector,
                                            EPHENIXSide side,
                                            int eventNumber,
                                            const char *fname,
                                            Int_t xCanvasPosition,
                                            Int_t yCanvasPosition,
					    TecGeometryObject* TGO,
					    TecCalibrationObject* TCO)
{
  //Draw a display of the specified event in the specified sector
  //and side of the TEC in a new TCanvas. All of
  //the objects created to make the display have their kCanDelete
  //bit set.
  //For the Side argument, this method will accept only kNorth or
  //kSouth, not kBoth.
  //xCanvasPosition and yCanvasPosition specify the coordinates of the
  //new TCanvas's upper left hand corner.

  if (side == kBoth) {
    if (Verbose > 0) {
      cerr << "kBoth passed to mTecDrawModule::DrawEventInNewWindow()"
           << endl;
    }
    return;
  }

  Int_t myNumOfCalls = fNumberOfCalls++;

  if (Verbose > 0)
    {
      cout << "mTecDrawModule::DrawEventInNewWindow(): Started..." << endl;
    }

  const char *myTitle = GetTecNameStringFor(sector, side);
  int width = DrawSize[sector][0];
  int height = DrawSize[sector][1];

  Char_t *cName = GetUniqueName("canvas", myNumOfCalls);

  TCanvas *canvas = new TCanvas(cName, myTitle, xCanvasPosition, yCanvasPosition, width, height);
  delete [] cName;

  canvas->cd();

  Char_t *padName = GetUniqueName("displayPad", myNumOfCalls);
  TPad *displayPad = new TPad(padName, "", 0.0107239,0.141053,0.983914,0.985263);
  //displayPad is used to hold the display of the event.
  displayPad->SetBit(kCanDelete);
  delete [] padName;
  displayPad->Draw(); //Associate displayPad with the current gPad.

  //Draw event number and file name.
  char eventNumberName[13];

  if (eventNumber >= 0)
    {

      if (eventNumber < 10000) {
        sprintf(eventNumberName, "Event # %d", eventNumber);
      }
      else {
        sprintf(eventNumberName, "Event # %d", 0);
      }

      TText *textevt = new TText(0.0107239,0.107, eventNumberName);
      //In coordinates of pad rather than plot.

      textevt->SetBit(kCanDelete);
      textevt->SetTextSize (0.035);
      textevt->Draw();

      TText *textfn = new TText(0.230563,0.107, fname);
      textfn->SetBit(kCanDelete);
      textfn->SetTextSize (0.0215);
      textfn->Draw();
    }

  TSignalingTButton *resetButton = new TSignalingTButton("Reset view", 0.442211, 0.0117647, 0.748744, 0.0870588);
  resetButton->SetBit(kCanDelete);
  resetButton->Draw();

  TSignalingTButton *closeButton = new TSignalingTButton("Close", 0.773869, 0.0117647, 0.983914, 0.0870588);
  closeButton->SetBit(kCanDelete);
  closeButton->Draw();
  closeButton->Connect("Released()","TCanvas",gPad->GetCanvas(),"Close()");

  displayPad->cd(); //Set the current pad (gPad) to displayPad.
  TDragZoomTH2F* histo = DrawEventInCurrentPad(root, sector, side, TGO, TCO);
  resetButton->Connect("Released()","TDragZoomTH2F",histo,"ResetView()");

  canvas->Modified();
  canvas->Update();
}






// SLSL3  MAIN
TDragZoomTH2F*
mTecDrawModule::DrawEventInCurrentPad(PHCompositeNode * root,
                                            Int_t sector,
                                            Int_t side)
{
  //Draw the specified sector and side of the specified event
  //into the current pad.
  //Return a pointer to the TDragZoomTH2F used to draw the axes
  //of the event display. This allows caller's access to the
  //TDragZoomTH2F's ResetView() method.
  //All objects created in the drawing of the event, including the
  //returned TDragZoomTH2F, will have had their kCanDelete bits set.

  using namespace TecUtilities;

  Int_t myNumOfCalls = fNumberOfCalls;

  if (side == kBoth) {
    if (Verbose > 0) {
      cerr << "kBoth passed to mTecDrawModule::DrawEventInCurrentPad()"
           << endl;
    }
    return (TDragZoomTH2F*)0;
  }

  PHNodeIterator iii (root);

  PadCluster* pc3=0;
  PHTypedNodeIterator<PadCluster> paditer(root);
  PadClusterNode_t *Pc3ClusterNode = paditer.find("Pc3Cluster");
  if(Pc3ClusterNode) { pc3 = (PadCluster*)Pc3ClusterNode->getData(); }
    else { cerr << "mTecDrawModule WARNING: Can not find Pc3Cluster." << endl; }

  TecOutV1* tecout=0;
  PHTypedNodeIterator<TecOutV1> teciter(root);
  TecOutNode_t *TecOutNode = teciter.find("TecOutV1");
  if(TecOutNode) { tecout = (TecOutV1*)TecOutNode->getData(); }
  else {
    cerr << "mTecDrawModule ERROR: Can not find TecOut." << endl;
    return (TDragZoomTH2F*)0;
  }

  PHNodeIterator nodeIter (root);
  PHCompositeNode *parNode;
  parNode = static_cast < PHCompositeNode * >(nodeIter.findFirst ("PHCompositeNode", "PAR"));
  if (!parNode) {
      PHMessage ("mTecDrawModule::Draw() ", PHError, "PAR node does not exist.");
      return (TDragZoomTH2F*)0;
  }

  PHNodeIterator *parNodeIter = new PHNodeIterator (parNode);

  PHBoolean status1 = False;
  TecCalibrationObject *TCO=findNode::getClass<TecCalibrationObject>(parNode,"TecCalibration");
  if (!TCO) {
      PHMessage ("mTecDrawModule::Draw() ", PHError, "TecCalibrationObject not found.\n");
      TCO = new TecCalibrationObject ();
      status1 = TCO->FetchFromFile ();
  }
  
  PHBoolean status2 = False;
  TecGeometryObject *TGO=0;
  ObjectNode_t *TGONode  =
    static_cast<ObjectNode_t*>(parNodeIter->findFirst ("PHIODataNode", "TecGeometry"));
  if (!TGONode) {
      PHMessage ("mTecDrawModule::Draw() ", PHError, "TecGeometryObject not found.\n");
      TGO = new TecGeometryObject ();
      status2 = TGO->FetchFromFile ();
  }
  else { TGO = static_cast < TecGeometryObject * >(TGONode->getData ()); }


  delete parNodeIter;

//  if(Verbose > 0) {
    cout << "-------------------------------------------------------------" << endl;
//    cout << "mTecDrawModule: Number of Tracks = " << tecout->getNTracks() << endl;
//    cout << "mTecDrawModule: Number of Hits = " << tecout->getNHits() << endl;
    cout << "mTecDrawModule: Drawing Threshold = " << BinThreshold << endl;
//  }

  int i, iarm, isect, iside, iplane, iwire;
  double x[30000], y[30000], x1, x2, y1, y2, z1;
  float X1, Y1, X2, Y2;

  Y1 = DrawXY[sector][1];
  Y2 = DrawXY[sector][3];
  X1 = DrawXY[sector][0];
  X2 = DrawXY[sector][2];

  const char* histTitle = GetTecNameStringFor(sector, side);
  Char_t *hName = GetUniqueName("histo", myNumOfCalls);
  TDragZoomTH2F* histo = new TDragZoomTH2F(hName, histTitle, 200,X1,X2,200,Y1,Y2);
  delete [] hName;
  histo->SetXTitle("PHENIX Global Coordinates (cm)");
  histo->SetBit(kCanDelete); //Delete with parent TCanvas
  histo->SetStats(0);
  histo->Draw();
  histo->SetDirectory(0); //Remove histo from the global directory.

  //Note: TGraphs are being used in place of TLines.
  //TGraphs are clipped by the frame of the histogram
  //they're drawn into; TLines are not.
  TGraph* line[400];

  // Draw Hits

  int MinBin[TECMAXINDEX], MaxBin[TECMAXINDEX], ij, myindex;
  float midphi[8], Xwire, Ywire;
  float relativebin, difference1, difference2;
  float MidPhi[TECMAXINDEX];
  float SinAlpha[TECMAXINDEX];

  if(Verbose>0) cout << "drawing hits..." << endl;

  GetMinMax(MinBin, MaxBin, TCO, Verbose);

  GetMidPhi(MidPhi, midphi, SinAlpha, TGO, Verbose);

  ij = 0;
  int nh[6]; for(int inh=0; inh<6; inh++) {nh[inh]=0;}
  vector<int> wirelist;

  for (i = 0; i < tecout->getNHits(); i++) {

      iarm = 0;
      isect = tecout->getHitSector(i);
      iplane = tecout->getHitPlane(i);
      iside = tecout->getHitSide(i);

      if ((isect == sector && iside == side) || (sector < 0 && iside == side)) {

          float ampl = tecout->getHitADC(i);
          if (ampl > BinThreshold) {

            iwire = tecout->getHitWire(i);
            myindex = iarm * 48 + isect * 12 + iplane * 2 + iside;
	    int glwire = iwire + (isect*12 + iplane*2 + iside)*1000;
	    int found=0;
	    for(unsigned int iw=0; iw<wirelist.size(); iw++) {
              if(wirelist[iw]==glwire) {found=1; break;}
	    }
              if(!found) {wirelist.push_back(glwire);}
                nh[iplane]++;

              Xwire = TGO->getGlobalX(isect*TECMAXSIDE*TECMAXPLANE+iplane*TECMAXSIDE+iside, iwire);
              Ywire = TGO->getGlobalY(isect*TECMAXSIDE*TECMAXPLANE+iplane*TECMAXSIDE+iside, iwire);

                  int mybin = tecout->getHitTimeBin(i);
                  difference1 = (float) (MaxBin[myindex] - MinBin[myindex]);
                  difference2 = (float) (mybin - MinBin[myindex]);
                  relativebin = difference2 / difference1;
                  float Xpos, Ypos;
                  CalcXYfromBin (relativebin, Xwire, Ywire,
                                 SinAlpha[myindex],
                                 Xpos, Ypos, Verbose);
                  x[ij] = Xpos;
                  y[ij] = Ypos;
                  ij++;
                  if (ij >= 30000) {
                      cerr << "mTecDraw ERROR: Too many Hits to draw." << endl;
                      break;
                  }
          }
      }
  }

  int hok=1;
  int wok=1;
  cout << "Total hits:  " << ij << endl;
  cout << "Total wires: " << wirelist.size() << endl;
  cout << "HITS: ";
  for(int ih=0; ih<6; ih++) {cout << nh[ih] << " "; if(nh[ih]>1000) {hok=0;}}
  cout << endl;
  int nw[6]; for(int iw=0; iw<6; iw++) {nw[iw]=0;}
  for(unsigned int iw=0; iw<wirelist.size(); iw++) {
    int index = wirelist[iw]/1000;
    int plane = (index%12)/2;
    nw[plane]++;
  }
  cout << "WIRES: ";
  for(int iw=0; iw<6; iw++) {cout << nw[iw] << " "; if(nw[iw]>100) {wok=0;}}
  cout << endl;
  if(hok && wok) {cout << "+++++++++++++ GOOD +++++++++++++" << endl;}
    else {cout << "-------------- BAD ---------------" << endl;}
	  

  wirelist.clear();

  int ijij = ij;
  TGraph *graph;
  if (ijij > 0)
    {
      graph = new TGraph (ijij, x, y);
      graph->SetBit(kCanDelete); //Delete with parent TCanvas
      graph->SetBit(kCannotPick); //Make graph unselectable.
      graph->SetMarkerStyle (20);
      graph->SetMarkerSize (DrawMarkerSize);
      graph->SetMarkerColor (2);
      graph->Draw("P");
    }

  //Draw Tracks
  if(Verbose>0) cout << "drawing tracks..." << endl;

  Double_t xCoords[2]; //Used in replacing TLines with TGraphs
  Double_t yCoords[2]; //Used in replacing TLines with TGraphs

  ij = 0;
  for (i = 0; i < tecout->getNTracks(); i++)
    {
      x1 = tecout->getTrackXin(i);
      y1 = tecout->getTrackYin(i);
      x2 = tecout->getTrackXout(i);
      y2 = tecout->getTrackYout(i);
      z1 = 1.0;
      if(tecout->getTrackSide(i)==0) z1 = -1.0;
      if ((side == 0 && z1 < 0) || (side == 1 && z1 > 0) || (side < 0))
        {
          xCoords[0] = x1;
          yCoords[0] = y1;
          xCoords[1] = x2;
          yCoords[1] = y2;

          line[ij] = new TGraph(2, xCoords, yCoords);
          line[ij]->SetLineWidth(1);
          line[ij]->SetLineColor(kBlack);
          line[ij]->SetBit(kCanDelete);
          line[ij]->SetBit(kCannotPick); //Make line[ij] unselectable.
          line[ij]->Draw("L");

          ij++;
        }
    }

// Draw Pc3 clusters if they exist
  float xpc[999],ypc[999];
  int npc=0;
  if(pc3) {
    for(unsigned int i=0; i<pc3->get_PadNCluster(); i++) {
       float x = pc3->get_xyz(i,0);
       float y = pc3->get_xyz(i,1);
       float z = pc3->get_xyz(i,2);
       if(x<0. && ((side==0 && z<0.) || (side==1 && z>0.))) {
         xpc[npc]=x;
         ypc[npc]=y;
         npc++;
         if(npc>=999) break;
       }
    }
      TGraph *graphpc=0;
      if (npc > 0) {
        graphpc = new TGraph (npc, xpc, ypc);
        graphpc->SetBit(kCanDelete);  //Delete with parent TCanvas
        graphpc->SetBit(kCannotPick); //Make graph unselectable.
        graphpc->SetMarkerStyle(20);
        graphpc->SetMarkerSize(DrawMarkerSize*2.);
        graphpc->SetMarkerColor(4);
        graphpc->Draw("P");
      }

  }
  
  if (status1)
    delete TCO;
  if (status2)
    delete TGO;

  if(Verbose>0) cout << "returning..." << endl;
  return histo;
}


TDragZoomTH2F*
mTecDrawModule::DrawEventInCurrentPad(PHCompositeNode * root,
                                            Int_t sector,
                                            Int_t side, 
					    TecGeometryObject* TGO,
					    TecCalibrationObject* TCO)
{
  //Draw the specified sector and side of the specified event
  //into the current pad.
  //Return a pointer to the TDragZoomTH2F used to draw the axes
  //of the event display. This allows caller's access to the
  //TDragZoomTH2F's ResetView() method.
  //All objects created in the drawing of the event, including the
  //returned TDragZoomTH2F, will have had their kCanDelete bits set.

  using namespace TecUtilities;

  Int_t myNumOfCalls = fNumberOfCalls;

  if (side == kBoth) {
    if (Verbose > 0) {
      cerr << "kBoth passed to mTecDrawModule::DrawEventInCurrentPad()"
           << endl;
    }
    return (TDragZoomTH2F*)0;
  }

  PHNodeIterator iii (root);

  TecOutV1* tecout;
  PHTypedNodeIterator<TecOutV1> teciter(root);
  TecOutNode_t *TecOutNode = teciter.find("TecOutV1");
  if(TecOutNode) {
    tecout = (TecOutV1*)TecOutNode->getData();
  }
  else {
    cerr << "mTecDrawModule ERROR: Can not find TecOut." << endl;
    return (TDragZoomTH2F*)0;
  }

  if (Verbose > 0)
    {
      cout <<
        "mTecDrawModule::DrawEventInCurrentPad(): Number of Tracks = "
           << tecout->getNTracks() << endl;
      cout <<
        "mTecDrawModule::DrawEventInCurrentPad(): Number of Hits = "
           << tecout->getNHits() << endl;
      cout << "mTecDrawModule::DrawEventInCurrentPad(): Drawing Threshold = " << 
               BinThreshold << endl;
    }

  int i, iarm, isect, iside, iplane, iwire;
  double x[30000], y[30000], x1, x2, y1, y2, z1;
  float X1, Y1, X2, Y2;

  Y1 = DrawXY[sector][1];
  Y2 = DrawXY[sector][3];
  X1 = DrawXY[sector][0];
  X2 = DrawXY[sector][2];

  const char* histTitle = GetTecNameStringFor(sector, side);
  Char_t *hName = GetUniqueName("histo", myNumOfCalls);
  TDragZoomTH2F* histo = new TDragZoomTH2F(hName, histTitle, 200,X1,X2,200,Y1,Y2);
  delete [] hName;
  histo->SetXTitle("PHENIX Global Coordinates (cm)");
  histo->SetBit(kCanDelete); //Delete with parent TCanvas
  histo->SetStats(0);
  histo->Draw();
  histo->SetDirectory(0); //Remove histo from the global directory.

  //Note: TGraphs are being used in place of TLines.
  //TGraphs are clipped by the frame of the histogram
  //they're drawn into; TLines are not.
  TGraph* line[400];

  // Draw Hits

  int MinBin[TECMAXINDEX], MaxBin[TECMAXINDEX], ij, myindex;
  float midphi[8], Xwire, Ywire;
  float relativebin, difference1, difference2;
  float MidPhi[TECMAXINDEX];
  float SinAlpha[TECMAXINDEX];

  if(Verbose>0) cout << "drawing hits..." << endl;

  GetMinMax(MinBin, MaxBin, TCO, Verbose);

  GetMidPhi(MidPhi, midphi, SinAlpha, TGO, Verbose);

  ij = 0;
  for (i = 0; i < tecout->getNHits(); i++)
    {
      iarm = 0;
      isect = tecout->getHitSector(i);
      iplane = tecout->getHitPlane(i);
      iside = tecout->getHitSide(i);

      if ((isect == sector && iside == side) || (sector < 0 && iside == side))
        {

          iwire = tecout->getHitWire(i);
          myindex = iarm * 48 + isect * 12 + iplane * 2 + iside;

          Xwire = TGO->getGlobalX(isect*TECMAXSIDE*TECMAXPLANE+iplane*TECMAXSIDE+iside, iwire);
          Ywire = TGO->getGlobalY(isect*TECMAXSIDE*TECMAXPLANE+iplane*TECMAXSIDE+iside, iwire);

              float ampl = tecout->getHitADC(i);
              if (ampl > 0)
                {
                  int mybin = tecout->getHitTimeBin(i);
                  difference1 = (float) (MaxBin[myindex] - MinBin[myindex]);
                  difference2 = (float) (mybin - MinBin[myindex]);
                  relativebin = difference2 / difference1;
                  float Xpos, Ypos;
                  CalcXYfromBin (relativebin, Xwire, Ywire,
                                 SinAlpha[myindex],
                                 Xpos, Ypos, Verbose);
                  x[ij] = Xpos;
                  y[ij] = Ypos;
                  ij++;
                  if (ij > 30000) {
                      cerr << "TEC Draw ERROR: Too many Hits to draw." <<
                        endl;
                      break;
                  }
                }
        }
    }

  int ijij = ij;
  TGraph *graph;
  if (ijij > 0)
    {
      graph = new TGraph (ijij, x, y);
      graph->SetBit(kCanDelete); //Delete with parent TCanvas
      graph->SetBit(kCannotPick); //Make graph unselectable.
      graph->SetMarkerStyle (20);
      graph->SetMarkerSize (DrawMarkerSize);
      graph->SetMarkerColor (2);
      graph->Draw("P");
    }

  //Draw Tracks
  if(Verbose>0) cout << "drawing tracks..." << endl;

  Double_t xCoords[2]; //Used in replacing TLines with TGraphs
  Double_t yCoords[2]; //Used in replacing TLines with TGraphs

  ij = 0;
  for (i = 0; i < tecout->getNTracks(); i++)
    {
      x1 = tecout->getTrackXin(i);
      y1 = tecout->getTrackYin(i);
      x2 = tecout->getTrackXout(i);
      y2 = tecout->getTrackYout(i);
      z1 = 1.0;
      if(tecout->getTrackSide(i)==0) z1 = -1.0;
      if ((side == 0 && z1 < 0) || (side == 1 && z1 > 0) || (side < 0))
        {
          xCoords[0] = x1;
          yCoords[0] = y1;
          xCoords[1] = x2;
          yCoords[1] = y2;

          line[ij] = new TGraph(2, xCoords, yCoords);
          line[ij]->SetLineWidth(1);
          line[ij]->SetLineColor(kBlack);
          line[ij]->SetBit(kCanDelete);
          line[ij]->SetBit(kCannotPick); //Make line[ij] unselectable.
          line[ij]->Draw("L");

          ij++;
        }
    }
  
  if(Verbose>0) cout << "returning..." << endl;
  return histo;
}


















