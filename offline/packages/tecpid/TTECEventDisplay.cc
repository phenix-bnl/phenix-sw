#include "TTECEventDisplay.hh"

#include "GetUniqueName.h"

#include <TGraph.h>
#include <TStyle.h>
#include <TMarker.h>
//INCLUDECHECKER: Removed this line: #include <TFrame.h>

//INCLUDECHECKER: Removed this line: #include <iostream>
#include <string>

ClassImp(TTECEventDisplay)
  
/**
 * The event will be drawn onto the given TVirtualPad.
 * <p>
 * The default title and dimensions of the display
 * will be taken from the given model.
 * <p>
 * model and pad are assumed to remain valid throughout
 * the life time of the TTECEventDisplay instance.
 * <p>
 * More models may be added to the display through the
 * Add method.
 * 
 * @param pad Where to draw.
 * @param model What to draw.
 */
TTECEventDisplay::TTECEventDisplay(TVirtualPad* pad,
				   TTECSectorSideEvent* model) :
  TQObject()
{
  fPad = pad;
  Init();
  Add(model);

  const char* fileName = model->GetFileName();
  fFileName = new char[strlen(fileName) + 1];
  strcpy(fFileName, fileName);

  fXMin = model->GetXMin();
  fYMin = model->GetYMin();
  fXMax = model->GetXMax();
  fYMax = model->GetYMax();

  SetTitle(model->GetRegionName());

}

/**
 * The event will be drawn onto the given TVirtualPad.
 * <p>
 * The default title and dimensions of the display
 * will be taken from the given model.
 * <p>
 * pad is assumed to remain valid throughout
 * the life time of the TTECEventDisplay instance.
 * <p>
 * Models may be added to this display through its Add method.
 * 
 * @param pad Where to draw.
 * @param title The title of the display.
 * @param xMin TEC x coordinate of the lower left hand corner.
 * @param yMin TEC y coordinate of the lower left hand corner.
 * @param xMax TEC x coordinate of the upper right hand corner.
 * @param yMax TEC y coordinate of the upper right hand corner. 
 */
TTECEventDisplay::TTECEventDisplay(TVirtualPad* pad, const char* title,
				   Double_t xMin, Double_t yMin,
				   Double_t xMax, Double_t yMax) :
  TQObject()
{
  fPad = pad;
  Init();

  fXMin = xMin;
  fYMin = yMin;
  fXMax = xMax;
  fYMax = yMax;

  SetTitle(title);
}

/**
 * Clean up.
 * <p>
 * Clear this display's TVirtualPad.
 */
TTECEventDisplay::~TTECEventDisplay()
{
  if (fPad)
    fPad->Clear();

  delete [] fHitRangeAttributes;
  delete [] fHitRangeBoundaries;

  delete [] fTitle;

  delete [] fFileName;

  delete fFileNameText;
  delete fEventNumberText;

  //If fHitGraphs exists, clear it out.
  if (fHitGraphs) {
    TIterator* iter = fHitGraphs->MakeIterator();
    TList* list = static_cast<TList*>(iter->Next());
    while (list) {
      list->Delete(); //Delete the graphs.
      list = static_cast<TList*>(iter->Next());
    }
    fHitGraphs->Delete(); //Delete the lists.

    delete iter;

    delete fHitGraphs;
    fHitGraphs = 0;
  }

}

///Initialization code shared between the constructors.
void TTECEventDisplay::Init()
{  
  fHasBeenDrawn = kFALSE;

  fLegendPad = 0;
  fEventNumberText = 0;
  fFileNameText = 0;

  fBlackAndWhite = kFALSE;
  fShowLegend = kTRUE;
  fShowEventNumber = kTRUE;
  fShowFileName = kTRUE;

  fNumberOfHitRanges = 4;

  fHitRangeBoundaries = new Double_t[fNumberOfHitRanges + 1];
  fHitRangeBoundaries[0] = 0.0;
  fHitRangeBoundaries[1] = 3.0;
  fHitRangeBoundaries[2] = 6.0;
  fHitRangeBoundaries[3] = 10.0;
  fHitRangeBoundaries[4] = 90.0;

  fHitRangeAttributes = new TAttMarker[fNumberOfHitRanges];
  fHitRangeAttributes[0] = TAttMarker(kMagenta, kStar, 0.5);
  fHitRangeAttributes[1] = TAttMarker(kGreen, kStar, 0.5);
  fHitRangeAttributes[2] = TAttMarker(kBlue, kStar, 0.5);
  fHitRangeAttributes[3] = TAttMarker(kRed, kStar, 0.5);

  fNumberOfModels = 0;
  fEventNumber = 0;
  fFileName = 0;
  fHitGraphs = 0;
  fTitle = 0;

  for (int i = 0; i < 8; i++) {
    fModels[i] = 0;
    fTracksDrawn[i] = kFALSE;
  }

}
  
/**
 * Set the title for this display.
 * 
 * @param title The new title.
 */
void TTECEventDisplay::SetTitle(const char* title)
{
  delete [] fTitle;
  fTitle = new char[strlen(title) + 1];
  strcpy(fTitle, title);
  if (fHasBeenDrawn) {
    if (fZoomer)
      fZoomer->SetTitle(fTitle);
    if (fPad) {
      fPad->Modified();
      fPad->Update();
    }
  }
}

/**
 * Set the ranges the hits will be divided into
 * and the attributes used to draw the hits in each range.
 * <p>
 * Double_t array boundaries should have numberOfRanges + 1 
 * elements:
 * a lower boundary, boundaries to separate each range,
 * and an upper boundary. For example, if you wanted to
 * display hits with amplitudes in [0, 15) and [15, 90],
 * you would would pass in 2 for numberOfRanges and
 * {0, 15, 90} for boundaries.
 * <p>
 * TAttMarker array attributes should have numberOfRanges
 * elements.
 * 
 * @param numberOfRanges The number of amplitude ranges.
 * @param boundaries Boundaries enclosing the ranges.
 * @param attributes Attributes for drawing each range of hits.
 */
void TTECEventDisplay::SetHitAmplitudeRanges(Int_t numberOfRanges,
					     const Double_t* boundaries,
					     const TAttMarker* attributes)
{
  
  fNumberOfHitRanges = numberOfRanges;
  
  if (boundaries != fHitRangeBoundaries) {
    delete [] fHitRangeBoundaries;
    fHitRangeBoundaries = new Double_t[fNumberOfHitRanges + 1];
  
    //Could use memcpy here.
    for (int i = 0; i < fNumberOfHitRanges + 1; i++)
      fHitRangeBoundaries[i] = boundaries[i];    
  }

  if (attributes != fHitRangeAttributes) {
    delete [] fHitRangeAttributes;

    fHitRangeAttributes = new TAttMarker[fNumberOfHitRanges];
    for (int j = 0; j < fNumberOfHitRanges; j++)
      fHitRangeAttributes[j] = attributes[j];
  }

  if (fHasBeenDrawn) {
    fPad->SetCursor(kWatch);
    DrawModels();
    UpdateAmplitudeLegend(kTRUE);
    fPad->Modified();
    fPad->Update();
    fPad->SetCursor(kPointer);
  }
}

/**
 * Set whether a legend of the hit marker styles 
 * and corresponding amplitude ranges should be drawn.
 *
 * @param showLegend kTRUE to show legend, kFALSE to hide it.
 */
void TTECEventDisplay::SetShowLegend(Bool_t showLegend)
{

  //The follow static variable is part of a skanky hack
  //to ensure that the legend does not show up with a
  //color scheme out of sync with the rest of the display.
  //Function static variable's are initialized only
  //once, the first time the function is called.
  static Bool_t oldBlackAndWhite = fBlackAndWhite;

  Bool_t oldShowLegend = fShowLegend;
  fShowLegend = showLegend;

  if (fHasBeenDrawn && oldShowLegend != fShowLegend) {
    fPad->SetCursor(kWatch);

    //Force the amplitude legend to be redrawn only if
    //the b&w state has changed since the last time
    //this method was called.
    UpdateAmplitudeLegend(oldBlackAndWhite != fBlackAndWhite);
    fPad->Modified();
    fPad->Update();
    fPad->SetCursor(kPointer);
  }

  oldBlackAndWhite = fBlackAndWhite;
}

/**
 * Should the event number be drawn?
 * 
 * @param showEventNumber kTRUE to show the event number,
 * kFALSE to hide it.
 */
void TTECEventDisplay::SetShowEventNumber(Bool_t showEventNumber)
{
  Bool_t oldShowEventNumber = fShowEventNumber;
  fShowEventNumber = showEventNumber;

  if (fHasBeenDrawn && oldShowEventNumber != fShowEventNumber) {
    UpdateEventNumberText();
    fPad->Modified();
    fPad->Update();
  }
}

/**
 * Should the file name be drawn?
 * 
 * @param showFileName kTRUE to show the file name,
 * kFALSE to hide it.
 */
void TTECEventDisplay::SetShowFileName(Bool_t showFileName)
{
  Bool_t oldShowFileName = fShowFileName;
  fShowFileName = showFileName;

  if (fHasBeenDrawn && oldShowFileName != fShowFileName) {
    UpdateFileNameText();
    if (fPad) {
      fPad->Modified();
      fPad->Update();
    }
  }
}

/**
 * Set whether the display should draw itself
 * in black and white only.
 * 
 * @param bw kTRUE for black and white; kFALSE for color.
 */
void TTECEventDisplay::SetBlackAndWhite(Bool_t bw)
{
  Bool_t oldBlackAndWhite = fBlackAndWhite;
  fBlackAndWhite = bw;

  fPad->SetCursor(kWatch);

  if (fHasBeenDrawn && oldBlackAndWhite != fBlackAndWhite) {
    if (fHitGraphs) {
      for (int range = 0; range < fNumberOfHitRanges; range++) {
	TList* list = (TList*)(fHitGraphs->At(range));
	if (list) {
	  TIterator* listIter = list->MakeIterator();
	  TGraph* hitGraph = static_cast<TGraph*>(listIter->Next());
	  while (hitGraph) {
	    if (fBlackAndWhite)
	      hitGraph->SetMarkerColor(kBlack);
	    else
	      hitGraph->SetMarkerColor(fHitRangeAttributes[range].GetMarkerColor());
	    hitGraph = static_cast<TGraph*>(listIter->Next());
	  }
	  delete listIter;
	}
      }
    }
    
    fMainPad->Modified();

    UpdateAmplitudeLegend(kTRUE);

    fPad->Modified();
    fPad->Update();
    fPad->SetCursor(kPointer);
  }
}

/**
 * Add the given TTECSectorSideEvent to the list
 * of models to draw. A TTECEventDisplay will only
 * accept one TTECSectorSideEvent per sector-side.
 * A TTECEventDisplay will only accept TTECSectorSideEvents
 * having the same event number and file of origin.
 * Add returns kFALSE if the given event is not accepted.
 * <p>
 * A TTECEventDisplay expects that the objects Added to
 * it will remain valid throughout its life time.
 *
 * @param model TTECSectorSideEvent to draw.
 * @return kFALSE if the model is not accepted.
 */
Bool_t TTECEventDisplay::Add(TTECSectorSideEvent* model)
{  
  if (!model || model->IsErrored())
    return kFALSE;

  if (fNumberOfModels == 0)
    fEventNumber = model->GetEventNumber();

  int sector = model->GetSector();
  int side = model->GetSide();
  int index = 4 * side + sector;
  
  if (fModels[index] || fEventNumber != model->GetEventNumber())
    return kFALSE;

  if (!fFileName) {
    const char* fileName = model->GetFileName();
    fFileName = new char[strlen(fileName) + 1];
    strcpy(fFileName, fileName);
  }

  fModels[index]  = model;
  fNumberOfModels++;

  if (fHasBeenDrawn) {
    fPad->SetCursor(kWatch);
    DrawModel(model);
    fPad->Modified();
    fPad->Update();
    fPad->SetCursor(kPointer);
  }

  return kTRUE;
}

/**
 * Change the bounds of the display so that every
 * TTECSectorSideEvent Added is visible.
 */
void TTECEventDisplay::ChangeBoundsToFitAll()
{
  Double_t oldXMin = fXMin;
  Double_t oldYMin = fYMin;
  Double_t oldXMax = fXMax;
  Double_t oldYMax = fYMax;

  TTECSectorSideEvent* model;
  for (int i = 0; i < 8; i++) {
    model = fModels[i];
    
    if (model) {
      if (model->GetXMin() < fXMin)
	fXMin = model->GetXMin();
      if (model->GetYMin() < fYMin)
	fYMin = model->GetYMin();
      if (model->GetXMax() > fXMax)
	fXMax = model->GetXMax();
      if (model->GetYMax() > fYMax)
	fYMax = model->GetYMax();
    }
  }

  if (fHasBeenDrawn && (fXMin != oldXMin || fYMin != oldYMin ||
			fXMax != oldXMax || fYMax != oldYMax)) {
    fPad->SetCursor(kWatch);
    fZoomer->SetBins(kNumberOfBins, fXMin, fXMax, kNumberOfBins, fYMin, fYMax);
    fPad->Modified();
    fPad->Update();
    fPad->SetCursor(kPointer);
  }

}


/**
 * Draw the event display onto the TVirtualPad specified
 * in the event display's constructor.
 * <p>
 * The TTECEventDisplay does not append itself to the pad's
 * list of objects to draw (as most ROOT classes do
 * when their Draw method is called).
 */
void TTECEventDisplay::Draw()
{

  gStyle->SetTitleColor(kWhite);

  fPad->Clear();
  
  fPad->SetCursor(kWatch);

  fPad->SetBorderMode(0);
  fPad->SetFrameFillColor(kWhite);
  fPad->SetFillColor(kWhite);

  fPad->cd();

  char* mainPadName = GetUniqueName("mainPad", fEventNumber);
  fMainPad = new TPad(mainPadName, "", 0.0, 0.1, 0.85, 1.0);
  delete [] mainPadName;

  fMainPad->SetBit(kCanDelete);
  fMainPad->SetBorderMode(0);
  fMainPad->SetFillColor(kWhite);
  fMainPad->SetFrameFillColor(kWhite);
  fMainPad->SetFrameBorderMode(0);
  fMainPad->Draw();

  fMainPad->cd();
  
  char* zoomerName = GetUniqueName("fZoomer", fEventNumber);
  fZoomer = new TDragZoomTH2F(zoomerName,
			      fTitle,
			      kNumberOfBins, fXMin, fXMax, 
			      kNumberOfBins, fYMin, fYMax);
  delete [] zoomerName;

  fZoomer->SetXTitle("PHENIX Global Coordinates (cm)");
  fZoomer->SetBit(kCanDelete); //Delete with parent TObject.
  fZoomer->SetStats(0);
  fZoomer->SetDirectory(0);
  fZoomer->Draw("AXIS");

  DrawModels();

  UpdateAmplitudeLegend(kTRUE);

  UpdateEventNumberText();

  UpdateFileNameText();

  fPad->Modified();
  fPad->Update();

  fPad->SetCursor(kPointer);

  fHasBeenDrawn = kTRUE;
}

///Reset the bounds to their pre-zoomed values.
void TTECEventDisplay::ResetBounds()
{
  if (fZoomer)
    fZoomer->ResetView();
}

/**
 * Draw all of the models Added to this TTECEventDisplay.
 * <p>
 * Clear any hits that have already been drawn.
 * <p>
 * This method does not take care of updating
 * the interface via Modified() and Update().
 */
void TTECEventDisplay::DrawModels()
{

  //If fHitGraphs exists, clear it out.
  if (fHitGraphs) {
    TIterator* iter = fHitGraphs->MakeIterator();
    TList* list = static_cast<TList*>(iter->Next());
    while (list) {
      list->Delete(); //Delete the graphs.
      list = static_cast<TList*>(iter->Next());
    }
    fHitGraphs->Delete(); //Delete the lists.

    delete iter;

    delete fHitGraphs;
    fHitGraphs = 0;
  }

  for (int i = 0; i < 8; i++)
    DrawModel(fModels[i]);
}

/**
 * Draw the given model.
 * <p>
 * It's OK to pass null to this method.
 * <p>
 * This method does not take care of updating
 * the interface via Modified() and Update().
 * 
 * @param model TTECSectorSideEvent to draw.
 */
void TTECEventDisplay::DrawModel(TTECSectorSideEvent* model)
{

  if (!model)
    return;

  //Should a list be created for each hit range?
  Bool_t createList = kFALSE;

  if (model->GetNumberOfHits() > 0) {

    if (!fHitGraphs) {
      fHitGraphs = new TObjArray(5);
      createList = kTRUE;
    }

    for (int i = 0; i < fNumberOfHitRanges; i++) {
      Double_t min = fHitRangeBoundaries[i];
      Double_t max = fHitRangeBoundaries[i + 1];

      TList* list;
      if (createList) {
	list = new TList;
      
	fHitGraphs->Add(list);
      }
      else {
	list = (TList*)(fHitGraphs->At(i));
      }

      Int_t numberOfHitsInRange;
      const TECHit_t* hits;
      model->GetHitsInAmplitudeRange(min, max, hits, numberOfHitsInRange);

      TGraph* hitGraph = 0;
      if (numberOfHitsInRange > 0) {
	hitGraph = new TGraph(numberOfHitsInRange);

	//Do not have this TGraph automatically deleted
	//with its graphical parent.
	hitGraph->ResetBit(kCanDelete);

	//Set the marker attributes.
	hitGraph->SetMarkerSize(fHitRangeAttributes[i].GetMarkerSize());
	hitGraph->SetMarkerStyle(fHitRangeAttributes[i].GetMarkerStyle());
	if (fBlackAndWhite) {
	  hitGraph->SetMarkerColor(kBlack);
	}
	else {
	  hitGraph->SetMarkerColor(fHitRangeAttributes[i].GetMarkerColor());
	}
	hitGraph->SetBit(kCannotPick);

	//Fill in the points.
	for (int j = 0; j < numberOfHitsInRange; j++) {
	  hitGraph->SetPoint(j, hits[j].x, hits[j].y);
	}

	list->Add(hitGraph);
	hitGraph->Draw("p");

      }
    }

  }

  //Draw the tracks.
  Int_t numberOfTracks = model->GetNumberOfTracks();

  //Used in drawing the tracks.
  Double_t x[2];
  Double_t y[2];

  int index = 4 * model->GetSide() + model->GetSector();

  if (!fTracksDrawn[index] && numberOfTracks > 0) {

    const TECTrack_t* tracks = model->GetTracks();
    for (int t = 0; t < numberOfTracks; t++) {
      x[0] = tracks[t].x1;
      x[1] = tracks[t].x2;
      y[0] = tracks[t].y1;
      y[1] = tracks[t].y2;

      TGraph* trackGraph = new TGraph(2, x, y);
      trackGraph->SetBit(kCanDelete);
      trackGraph->SetBit(kCannotPick);
      trackGraph->SetLineColor(kBlack);
      trackGraph->Draw("l");
    }

    fTracksDrawn[index] = kTRUE;
  }

}

/**
 * If a legend is to be drawn, draw it,
 * making sure that it matches the hits.
 * <p>
 * If the legend is to be hidden, take
 * care of hiding it.
 * <p>
 * This method does not take care of updating
 * the interface via Modified() and Update().
 * 
 * @param redraw Redraw the legend.
 */
void TTECEventDisplay::UpdateAmplitudeLegend(Bool_t redraw)
{

  if (fShowLegend && fNumberOfHitRanges > 0) {
    if (fLegendPad) {
      fPad->RecursiveRemove(fLegendPad);
    }
    else {
      char* legendPadName = GetUniqueName("legendPad", fEventNumber);
      fLegendPad = new TPad(legendPadName, "", 0.80, 0.33, 0.98, 0.86);
      delete [] legendPadName;

      fLegendPad->SetFillColor(kWhite);
      fLegendPad->SetBorderMode(0);
      fLegendPad->ResetBit(kCanDelete);

      redraw = kTRUE;
    }

    //Make sure the proper pad is the current pad.
    //Otherwise, who knows where your drawing commands
    //will be directed except perhaps for Rene Brun?
    fPad->cd();
    fLegendPad->Draw();

    if (redraw) {
      fLegendPad->Clear();
      fLegendPad->cd();

      TLegend* legend = new TLegend(0.001, 0.01, 0.96, 0.96, "Hit FADC");
      legend->SetBit(kCanDelete);
      legend->SetBorderSize(1);
      legend->SetFillColor(kWhite);

      //Note: The TLegend is not drawn immediately.
      //We need to create some TMarkers. To make sure
      //that they are deleted only when the TLegend is
      //no longer needs to use them, we will draw them
      //behind the TLegend, hiding them. The TMarkers
      //will have their kCanDelete bit set so that they'll
      //be deleted with the TLegend when the parent TPad
      //is Cleared or deleted.

      char rangeName[21];

      for (int i = 0; i < fNumberOfHitRanges; i++) {
	Double_t min = fHitRangeBoundaries[i];
	Double_t max = fHitRangeBoundaries[i + 1];

	if (i != fNumberOfHitRanges - 1) {
	  sprintf(rangeName, "[%5.1f, %5.1f)", min, max);
	}
	else {
	  sprintf(rangeName, "[%5.1f, %5.1f]", min, max);
	}

	TMarker* marker = new TMarker(0.5, 0.5, kStar);
	marker->SetBit(kCanDelete);
	
	if (!fBlackAndWhite) {
	  marker->SetMarkerColor(fHitRangeAttributes[i].GetMarkerColor());
	}
	else {
	  marker->SetMarkerColor(kBlack);
	}

	marker->SetMarkerSize(fHitRangeAttributes[i].GetMarkerSize());
	marker->SetMarkerStyle(fHitRangeAttributes[i].GetMarkerStyle());
	marker->Draw();

	legend->AddEntry(marker, rangeName, "p");
      }
      legend->Draw();
      fLegendPad->Modified();
    }
  }
  else {
    if (fLegendPad) {
      fPad->RecursiveRemove(fLegendPad);
    }
  }
}

/**
 * If the event number is to be displayed,
 * display it.
 * <p>
 * If it is to be hidden, hide it.
 * <p>
 * This method does not take care of updating
 * the interface via Modified() and Update().
 */
void TTECEventDisplay::UpdateEventNumberText()
{
  if (fShowEventNumber) {
    if (!fEventNumberText) {
      TString eventNumberString("Event # ");
      eventNumberString += fEventNumber;
      
      fEventNumberText =
	new TText(0.01, 0.05, (const char *) eventNumberString);
      
      //Do not have the TText deleted with the TObject its
      //been drawn into. Instead, TTECEventDisplay will
      //take care of deleting the memory itself.
      fEventNumberText->ResetBit(kCanDelete);
      fEventNumberText->SetTextSize(0.035);
    }
    else { 
      //Make sure that fEventNumberText doesn't end
      //getting drawn twice on fPad.
      fPad->RecursiveRemove(fEventNumberText);
    }
    fPad->cd();
    fEventNumberText->Draw();
  }
  else {
    if (fEventNumberText)
      fPad->RecursiveRemove(fEventNumberText);
  }
}

/**
 * If the file name is to be displayed,
 * display it.
 * <p>
 * If it is to be hidden, hide it.
 * <p>
 * This method does not take care of updating
 * the interface via Modified() and Update().
 */
void TTECEventDisplay::UpdateFileNameText()
{
  if (fShowFileName) {
    if (!fFileNameText) {
      fFileNameText = new TText(0.01, 0.01, fFileName);

      //Do not have the TText deleted with the TObject its
      //been drawn into. Instead, TTECEventDisplay will
      //take care of deleting the memory itself.
      fFileNameText->ResetBit(kCanDelete);
      fFileNameText->SetTextSize(0.03);
    }
    else {
      //Make sure fFileNameText doesn't get drawn twice.
      fPad->RecursiveRemove(fFileNameText);
    }
    fPad->cd();
    fFileNameText->Draw();
  }
  else {
    if (fFileNameText)
      fPad->RecursiveRemove(fFileNameText);
  }
}











