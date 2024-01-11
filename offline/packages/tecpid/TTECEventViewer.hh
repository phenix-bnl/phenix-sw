#ifndef __TTECEVENTVIEWER_HH__
#define __TTECEVENTVIEWER_HH__

#include <TQObject.h>
#include <TObject.h>
#include <TCanvas.h>

#include <TGFrame.h>
#include <TGMenu.h>

#include "TTECEventIterator.hh"
#include "TTECEventDisplay.hh"

class TList;
class TTECSectorSideEvent;
class TGTab;

/**
 * A class to view the TEC events contained in a PRDF file.
 * 
 * @author David Faden, dfaden@iastate.edu
 * @version July 10, 2001
 */
class TTECEventViewer : public TQObject, public TObject {

public:
  /*
   * Create a new TTECEventViewer object for the specified PRDF file.
   * firstEvent has a default value of 1 and openImmediately a default
   * value of kTRUE. This means that if you call this constructor
   * with only the name of the file, the graphical interface of
   * the TTECEventViewer will appear immediately.
   * If you'd like to delay the appearance of the TTECEventViewer 
   * interface, you may set openImmediately to kFALSE and call
   * the TTECEventViewer's Show method when you want it to appear.
   * 
   * @param prdfFileName Name of the file from which to load event data.
   * @param runnumber The run number of the given PRDF file.
   * @param isSimulation Indicates whether file contains simulated data.
   * @param firstEventNumber First event to display.
   * @param openImmediately Indicates whether the GUI should be shown right away.
   */
  TTECEventViewer(const char* prdfFileName, Int_t runnumber, Bool_t isSimulation = kFALSE, Int_t firstEventNumber = 1, Bool_t openImmediately = kTRUE);

  virtual ~TTECEventViewer();

  /**
   * Set this object's verbosity level.
   * The higher the verbosity level, the more
   * information this object will print out to
   * the standard C streams about its internal
   * workings.
   *
   * @param verbosity The new verbosity level.
   */
  void SetVerbosity(Int_t verbosity) 
  {
    fVerbosity = verbosity;

    if (fIterator)
      fIterator->SetVerbosity(fVerbosity);
  }

  /**
   * Get this object's verbosity level.
   */
  Int_t GetVerbosity() const
  {
    return fVerbosity;
  }

  /**
   * Should info be fetched from the database
   * or from local configuration files?
   *
   * @param useObjy Set to kTRUE for database.
   */
  void SetUseObjy(Bool_t useObjy)
  {
    fUseObjy = useObjy;
  }

  /**
   * Is info being fetched from the database
   * or from local configuration files?
   */
  Bool_t GetUseObjy() const
  {
    return fUseObjy;
  }

  /**
   * Does the PRDF file associated with this
   * object hold real or simulated data?
   */
  Bool_t IsSimulation() const
  {
    return fIsSimulation;
  }

  /**
   * Bring up the graphical interface if it has not already
   * been shown.
   */
  void Show();

  /**
   * Reset the range of the current view, the currently
   * visible tab. This method is used internally as a slot
   * for signals from the GUI.
   */
  void ResetCurrentView();

  ///Slot for handling a close of the main window.
  void HandleClose();

  ///Slot for handling menu events.
  void HandleMenu(Int_t selectionID);

  ///Advance the display to the next event.
  void NextEvent();

  ///Slot for handling tab selections.
  void HandleTabSelection(Int_t tabNumber);

  /**
   * Display the currently visible side-sector in a new
   * window. This method is used as a slot.
   */
  void DisplayInNewWindow();

private:
  //Constants:
  //Menu item identifiers.
  enum {
    //File menu
    kOpen,
    kSaveAsPS,
    kSaveAsEPS,
    kSaveAsGIF,
    kSaveAsCMacro,
    kSaveAsROOTFile,
    kClose,
    kQuitROOT,

    //Edit menu
    kUndo,
    kRedo,
    kNextEvent,
    kGotoEvent,
    kSetVerbosity,

    //View menu
    kResetView,
    kBlackAndWhite,
    kShowLegend,
    kShowEventNumber,
    kShowFileName,
    kNewWindowWithView
  };

  static const Int_t kNumberOfTabs = 9;


  //Instance variables:
  Int_t fVerbosity; //Verbosity level.
  Bool_t fIsSimulation; //Is the data simulated or real?
  Bool_t fUseObjy; //Should info be fetched from the database?
  Bool_t fHasBeenShown; //Has the graphical interface been shown?

  ///Iterates over the events in a PRDF file.
  TTECEventIterator* fIterator;

  ///Hold the data for each sector-side of an event.
  TTECSectorSideEvent* fModels[8];

  ///Name of the file being viewed.
  char* fPrdfFileName;

  Int_t fFirstEventNumber; //The first event to display.

  Int_t fEventNumber; //Current event number.
  Int_t fRunNumber; //Current run number.
  
  //GUI elements.
  Int_t fSelectedTabNumber; //Number of the selected TGTab/TCanvas.
  TCanvas* fCanvases[kNumberOfTabs]; //The TCanvases used to display the events.
  TGTab* fTabbedFrame; //The tabbed frame containing the canvases.

  TList* fGUIList; //List of GUI objects to delete.
  TGMainFrame *fMainWindow; //The viewer's main window.
  Window_t fMainWindowID; //The window ID of fMainWindow.

  ///Display the data for an event.
  TTECEventDisplay* fDisplays[kNumberOfTabs];
  
  ///Menu for changing properties of current view.
  TGPopupMenu* fViewMenu;



  //Private methods:  

  static void IndexToSectorSide(Int_t index, Int_t& sector, Int_t& side);

  /**
   * Set whether the specified menu item is checked
   * or unchecked. state == kTRUE for a check,
   * state == kFALSE for no check.
   *
   * @param menu The menu to change.
   * @param itemID ID of the menu item to change.
   * @param state kTRUE for check; kFALSE for no check.
   */
  static void SetMenuItemState(TGPopupMenu* menu, 
			       Int_t itemID, Bool_t state);


  /**
   * Set the bit indicated by flag on
   * every TObject in the list. If a TObject inherits
   * from TVirtualPad (and so implements GetListOfPrimitives),
   * set the indicated bit on every primitive of that pad.
   * If one of the primitives inherits from TVirtualPad,
   * do the same...
   *
   * @param list List of objects with bits to set.
   * @param flag The bit to set.
   */
  static void RecursiveSetBit(TList* list, UInt_t flag);

  ///Update the menus to reflect the current view.
  void UpdateMenus();

  /**
   * Allow the user to go to and display
   * an arbitrary event. Open an input
   * dialog to get the event number
   * and then change the viewer's views to display it.
   */
  void DoGotoEvent();

  /** 
   * Bring up a dialog to set the verbosity.
   * Set the verbosity.
   */
  void DoSetVerbosity();

  ///Toggle whether the current view is black and white.
  void ToggleBlackAndWhite();

  ///Toggle whether the current view has a legend.
  void ToggleShowLegend();

  ///Toggle whether the current view shows its event number.
  void ToggleShowEventNumber();

  ///Toggle whether the current view shows its file name.
  void ToggleShowFileName();

  ///Display event number targetNumber.
  Bool_t MoveToEvent(Int_t targetNumber);

  /**
   * Save the current view to a file.
   * The fileType array is assumed to be in a
   * format acceptable to TGFileInfo for TGFileDialog.
   * That is {"[File type name]", "*.[extension]", 0, 0},
   * filling in the bracketed portions with the specifics.
   *
   * @param fileType Description of the file type to save as.
   */ 
  void SaveAs(const char** fileType);

  /**
   * Open a new TTECEventViewer with the file
   * the user selects. Bring up a file dialog
   * to get the file name and a dialog to determine
   * whether the file contains simulated data.
   */
  void OpenPRDFFile();

  ClassDef(TTECEventViewer,0)

};

#endif





