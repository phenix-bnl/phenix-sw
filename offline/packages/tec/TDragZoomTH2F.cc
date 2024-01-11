#include "TDragZoomTH2F.hh"

//INCLUDECHECKER: Removed this line: #include <TCanvas.h>

ClassImp(TDragZoomTH2F)

//
//The TDragZoomTH2F extends the TH2F class to allow
//users to zoom in by clicking and dragging within
//the area of the histogram, and to simultaneously
//unzoom both axes with one method call.
//
//The TDragZoomTH2F class has not been created
//to be used as a histogram, but rather to be
//used a background for a TGraph.
//
//Based on code from Brett Viren.
//David Faden, dfaden@iastate.edu, June 20, 2001.
//

void TDragZoomTH2F::Draw(Option_t* option) 
{
  //Override the superclass's Draw method so
  //as to capture a pointer to this histogram's
  //parent TVirtualPad. This pointer is needed
  //for times when this histogram needs to be
  //redrawn, but its pad is not the current 
  //pad (gPad).

  fPad = gPad;
  TH2F::Draw(option);
}


void TDragZoomTH2F::ExecuteEvent(Int_t event, Int_t px, Int_t py)
{
  //Respond to user interaction. If the user clicks and drags 
  //with mouse button 1, draw a box enclosing the area the user
  //has dragged through.
  //
  //When the user releases the mouse button,
  //change the coordinate system to match the drag.
  
  //px and py appear to be in absolute screen coordinates.

  if (!gPad)
    return; //Exit if gPad isn't available (no graphics).

  fPad = gPad; //Update fPad -- it looks like it's OK
  //to assign fPad in the Draw method, but this is
  //safer still.

  //Check to see if it is possible for the user to
  //zoom in any more. If not, set the cursor to 
  //a pointer and return. (It is a good idea to compute
  //this each time because the histogram may be zoomed
  //behind our back.)
  if (fYaxis.GetFirst() == fYaxis.GetLast() &&
      fXaxis.GetFirst() == fXaxis.GetLast()) {
    fPad->SetCursor(kPointer);
    return;
  }

  Bool_t inside = InZoomArea(px, py); //Is the mouse
  //in the zoom area? (How expensive is this check?)


  switch (event) {
  case kButton1Down:
    if (inside) {
      fPad->SetCursor(kCross);
      fButton1Down = kTRUE;

      fSelectedRange[0] = px;
      fSelectedRange[1] = py;
      fSelectedRange[2] = px;
      fSelectedRange[3] = py;
    }

    break;
    
  case kButton1Motion:
    if (fButton1Down && inside) {
      fDrawingSelection = true;
    }

    if (fDrawingSelection) {
      //Erase the old selection box.
      gVirtualX->DrawBox(fSelectedRange[0], fSelectedRange[1],
			 fSelectedRange[2], fSelectedRange[3],
			 TVirtualX::kHollow);

      if (inside) {
	//Update the coordinates.
	fSelectedRange[2] = px;
	fSelectedRange[3] = py;
      }
      
      //Draw the selection box.
      gVirtualX->DrawBox(fSelectedRange[0], fSelectedRange[1],
			 fSelectedRange[2], fSelectedRange[3],
			 TVirtualX::kHollow);
    }
    
    break;

  case kButton1Up:
    fButton1Down = kFALSE;

    if (fDrawingSelection) {
      fPad->SetCursor(kWatch);

      /*
      //Erase the selection box.
      gVirtualX->DrawBox(fSelectedRange[0], fSelectedRange[1],
			 fSelectedRange[2], fSelectedRange[3],
			 TVirtualX::kHollow);      
      */

      //Convert the absolute pixel coordinates recorded in
      //fSelectedRange into the coordinates of the histogram's
      //axes.
      Double_t x1 = fPad->AbsPixeltoX(fSelectedRange[0]);
      x1 = fPad->PadtoX(x1);
      Double_t y1 = fPad->AbsPixeltoY(fSelectedRange[1]);
      y1 = fPad->PadtoY(y1);
      Double_t x2 = fPad->AbsPixeltoX(fSelectedRange[2]);
      x2 = fPad->PadtoX(x2);
      Double_t y2 = fPad->AbsPixeltoY(fSelectedRange[3]);
      y2 = fPad->PadtoY(y2);

      //Get the bins corresponding to the axes coordinates.
      Int_t xBin1 = fXaxis.FindFixBin(x1);
      Int_t xBin2 = fXaxis.FindFixBin(x2);
      Int_t yBin1 = fYaxis.FindFixBin(y1);
      Int_t yBin2 = fYaxis.FindFixBin(y2);

      //Make sure the low values are in xBin1 and yBin1.
      Int_t elSwappo;
      if (xBin1 > xBin2) {
	elSwappo = xBin2;
	xBin2 = xBin1;
	xBin1 = elSwappo;
      }
      if (yBin1 > yBin2) {
	elSwappo = yBin2;
	yBin2 = yBin1;
	yBin1 = elSwappo;
      }

      //Zoom the histogram (and any TGraphs drawn on
      //top of it).
      fXaxis.SetRange(xBin1, xBin2);
      fYaxis.SetRange(yBin1, yBin2);
      fPad->Modified();
      fPad->Update();
      //fPad->SetCursor(kCross);
    }
    fDrawingSelection = false;

    break;
  }

  if (inside) {
    fPad->SetCursor(kCross);
  }

}


void TDragZoomTH2F::ResetView()
{
  //Reset the view of this TDragZoomTH2F to its 
  //original range by UnZooming all of its axes.
 
  //This method assumes that fPad is valid.

  if (fPad)
    fPad->SetCursor(kWatch);

  fXaxis.UnZoom();
  fYaxis.UnZoom();

  if (fPad) {
    fPad->Modified();
    fPad->Update();
    fPad->SetCursor(kCross);
  }
  
  ViewReset(); //Emit "ViewReset()"
}

Bool_t TDragZoomTH2F::InZoomArea(Int_t px, Int_t py)
{
  //Return kTRUE if the point (px, py) is within the
  //visible range of the axes.

  if (!fPad)
    return kFALSE;

  //Convert px, py into the coordinates of
  //the axes.
  Double_t x = fPad->AbsPixeltoX(px);
  x = fPad->PadtoX(x);
  Double_t y = fPad->AbsPixeltoY(py);
  y = fPad->PadtoY(y);

  //Find which bins correspond to x and y
  //and return kFALSE if they are out of the visible
  //range.
  Int_t xBin = fXaxis.FindFixBin(x);
  if (xBin < fXaxis.GetFirst() || xBin > fXaxis.GetLast())
    return kFALSE;

  Int_t yBin = fYaxis.FindFixBin(y);
  if (yBin < fYaxis.GetFirst() || yBin > fYaxis.GetLast())
    return kFALSE;

  return kTRUE;
}





















