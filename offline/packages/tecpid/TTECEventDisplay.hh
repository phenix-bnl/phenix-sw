#ifndef __TTECEVENTDISPLAY_HH__
#define __TTECEVENTDISPLAY_HH__

#include "TTECSectorSideEvent.hh"
#include "TDragZoomTH2F.hh"

#include <TQObject.h>
#include <TPad.h>
#include <TText.h>
#include <TLegend.h>
#include <TList.h>
#include <TObjArray.h>

/**
 * A class to display an event from the TEC using ROOT.
 *
 * @author David Faden, dfaden@iastate.edu
 */
class TTECEventDisplay : public TQObject {

public:

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
  TTECEventDisplay(TVirtualPad* pad, TTECSectorSideEvent* model);

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
  TTECEventDisplay(TVirtualPad* pad, const char* title,
		   Double_t xMin, Double_t yMin,
		   Double_t xMax, Double_t yMax);
  
  /**
   * Clean up.
   * <p>
   * Clear this display's TVirtualPad.
   */
  ~TTECEventDisplay();

  /**
   * Set the title for this display.
   * 
   * @param title The new title.
   */
  void SetTitle(const char* title);

  ///Get this display's title.
  const char* GetTitle() const 
  {
    return fTitle;
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
  void SetHitAmplitudeRanges(Int_t numberOfRanges,
			     const Double_t* boundaries,
			     const TAttMarker* attributes);

  /**
   * Get the ranges the hits are divided into
   * and the attributes used to draw the hits in each range.
   * <p>
   * Double_t array boundaries will have numberOfRanges + 1 
   * elements:
   * a lower boundary, boundaries to separate each range,
   * and an upper boundary.
   * <p>
   * TAttMarker array attributes will have numberOfRanges
   * elements.
   * 
   * @param numberOfRanges The number of amplitude ranges.
   * @param boundaries Boundaries enclosing the ranges.
   * @param attributes Attributes for drawing each range of hits.
   */
  void GetHitAmplitudeRanges(Int_t& numberOfRanges,
			     const Double_t* boundaries,
			     const TAttMarker* attributes) const
  {
    numberOfRanges = fNumberOfHitRanges;
    boundaries = fHitRangeBoundaries;
    attributes = fHitRangeAttributes;
  }

  /**
   * Set whether a legend of the hit marker styles 
   * and corresponding amplitude ranges should be drawn.
   *
   * @param showLegend kTRUE to show legend, kFALSE to hide it.
   */
  void SetShowLegend(Bool_t showLegend);
  
  /**
   * Is a legend of the hit marker styles being drawn?
   * 
   * @return Is a legend of the hit marker styles being drawn?
   */
  Bool_t GetShowLegend() const
  {
    return fShowLegend;
  }
  
  /**
   * Should the event number be drawn?
   * 
   * @param showEventNumber kTRUE to show the event number,
   * kFALSE to hide it.
   */
  void SetShowEventNumber(Bool_t showEventNumber);

  /**
   * Is event number being drawn?
   * 
   * @return kTRUE if the event number if being drawn.
   */
  Bool_t GetShowEventNumber() const
  {
    return fShowEventNumber;
  }

  /**
   * Should the file name be drawn?
   * 
   * @param showFileName kTRUE to show the file name,
   * kFALSE to hide it.
   */
  void SetShowFileName(Bool_t showFileName);

  /**
   * Is the file name being drawn?
   */
  Bool_t GetShowFileName() const
  {
    return fShowFileName;
  }

  /**
   * Set whether the display should draw itself
   * in black and white only.
   * 
   * @param bw kTRUE for black and white; kFALSE for color.
   */
  void SetBlackAndWhite(Bool_t bw);

  /**
   * Is the display currently black and white?
   * 
   * @return Is the display currently black and white?
   */
  Bool_t GetBlackAndWhite() const
  {
    return fBlackAndWhite;
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
  Bool_t Add(TTECSectorSideEvent* model);

  /**
   * Change the bounds of the display so that every
   * TTECSectorSideEvent Added is visible.
   */
  void ChangeBoundsToFitAll();

  ///Has this display's Draw method been called?
  Bool_t HasBeenDrawn() const
  {
    return fHasBeenDrawn;
  }

  /**
   * Draw the event display onto the TVirtualPad specified
   * in the event display's constructor.
   * <p>
   * The TTECEventDisplay does not append itself to the pad's
   * list of objects to draw (as most ROOT classes do
   * when their Draw method is called).
   */
  void Draw();

  ///Reset the bounds to their pre-zoomed values.
  void ResetBounds();

private:
  /**
   * The number of bins for each axis of the view.
   * The more bins, the further in the user will be able to zoom.
   */
  static const Int_t kNumberOfBins = 400;

  ///Has the display been drawn?
  Bool_t fHasBeenDrawn;

  ///Where to draw.
  TVirtualPad* fPad;

  ///TPad to hold the main part of the display.
  TPad* fMainPad;

  ///Object that handles zooming the display.
  TDragZoomTH2F* fZoomer;

  ///TPad to hold the amplitude legend.
  TPad* fLegendPad;

  ///TText to display the event number.
  TText* fEventNumberText;
  
  ///TText to display the file name
  TText* fFileNameText;

  /**
   * A TObjArray of TLists of TGraphs.
   * There is one TList of TGraphs for each
   * hit amplitude range. The idea is to provide
   * an efficient way of changing the properties of
   * the drawn hits. For example, fHitGraphs will make it 
   * easier to change the hits from black and white to color 
   * or back again.
   */
  TObjArray* fHitGraphs;

  /**
   * Indicates whether tracks have been drawn
   * for the corresponding model.
   */
  Bool_t fTracksDrawn[8];

  ///Title of the event display.
  char* fTitle;

  ///Should the display draw itself in black and white?
  Bool_t fBlackAndWhite;

  ///Should a legend be drawn?
  Bool_t fShowLegend;

  ///Should the event number be drawn?
  Bool_t fShowEventNumber;

  ///Should the file name be drawn?
  Bool_t fShowFileName;

  ///Number of hit ranges.
  Int_t fNumberOfHitRanges;

  ///Boundaries of the hit ranges: lower, inter-range, upper
  Double_t* fHitRangeBoundaries;
  
  ///Attributes of each range's hits.
  TAttMarker* fHitRangeAttributes;


  ///Minimum TEC x coordinate to draw.
  Double_t fXMin;
  
  ///Minimum TEC y coordinate to draw.
  Double_t fYMin;

  ///Maximum TEC x coordinate to draw.
  Double_t fXMax;

  ///Maximum TEC y coordinate to draw.
  Double_t fYMax;

  ///Name of the file in which the events were stored.
  char* fFileName;

  ///The number of the event being drawn.
  Int_t fEventNumber;

  ///How many models are in the fModels array?
  Int_t fNumberOfModels;

  ///The data to display.
  TTECSectorSideEvent* fModels[8];



  ///Initialization code shared between the constructors.
  void Init();

  /**
   * Draw all of the models Added to this TTECEventDisplay.
   * <p>
   * Clear any hits and tracks that have already
   * been drawn.
   * <p>
   * This method does not take care of updating
   * the interface via Modified() and Update().
   */
  void DrawModels();

  /**
   * Draw the given model.
   * <p>
   * This method does not take care of updating
   * the interface via Modified() and Update().
   * 
   * @param model TTECSectorSideEvent to draw.
   */
  void DrawModel(TTECSectorSideEvent* model);

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
  void UpdateAmplitudeLegend(Bool_t redraw = kFALSE);


  /**
   * If the file name is to be displayed,
   * display it.
   * <p>
   * If it is to be hidden, hide it.
   * <p>
   * This method does not take care of updating
   * the interface via Modified() and Update().
   */
  void UpdateFileNameText();

  /**
   * If the event number is to be displayed,
   * display it.
   * <p>
   * If it is to be hidden, hide it.
   * <p>
   * This method does not take care of updating
   * the interface via Modified() and Update().
   */
  void UpdateEventNumberText();

  ClassDef(TTECEventDisplay,0)
};

#endif













