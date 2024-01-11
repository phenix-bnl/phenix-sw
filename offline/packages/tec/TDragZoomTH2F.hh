#ifndef __TDRAGZOOMTH2F_HH__
#define __TDRAGZOOMTH2F_HH__

#include <TH2.h>
#include <TQObject.h>
#include <TVirtualPad.h>

/**
 * The TDragZoomTH2F extends the TH2F class to allow
 * users to zoom in by clicking and dragging within
 * the area of the histogram, and to simultaneously
 * unzoom both axes with one method call.
 * <p>
 * The TDragZoomTH2F class has not been created
 * to be used as a histogram, but rather to be
 * used a background for a TGraph.
 * <p>
 * This class isn't designed to be subclassed.
 * <p>
 * Based on code from Brett Viren.
 * 
 * @author David Faden, dfaden@iastate.edu
 * @version July 9, 2001
 */

class TDragZoomTH2F: public TH2F, public TQObject {

public:

  /**
   * No argument constructor to be used in reading
   * this object from a stream.
   */
  TDragZoomTH2F() : TH2F() {}

  TDragZoomTH2F(const char* name, //ROOT object name.
		const char* title, //Title of the histogram.
		Int_t nbinsx, //Number of bins on the X axis.
		Axis_t xlow, //Minimum non-error X value.
		Axis_t xup, //Maximum non-error X value.
		Int_t nbinsy, //Number of bins on the Y axis.
		Axis_t ylow, //Minimum non-error Y value.
		Axis_t yup) : //Maximum non-error Y value.
    TH2F(name, title, nbinsx, xlow, xup, nbinsy, ylow, yup),
    fPad(0),
    fButton1Down(kFALSE),
    fDrawingSelection(kFALSE) 
  {
    SetFillColor(kWhite);
  }

  void Draw(Option_t* option = "");
  void ExecuteEvent(Int_t event, Int_t x, Int_t y);

  void ResetView(); //*MENU*
  void ViewReset() { Emit("ViewReset()"); } //*SIGNAL*

private:
  TVirtualPad* fPad; //This histogram's pad -- assigned in Draw().
  Bool_t fButton1Down; //Indicates whether mouse button 1 was pressed in fHistogram.
  Bool_t fDrawingSelection; //Indicates whether selection box should be drawn.
  Int_t fSelectedRange[4]; //Holds the coordinates of the user's selection.


  Bool_t InZoomArea(Int_t x, Int_t y);

  ClassDef(TDragZoomTH2F,1)

};

#endif








