#include <cstring>
#include <cstdlib>

#include "TTECEventViewer.hh"

#include "TTECSectorSideEvent.hh"
#include "TTextInputDialog.hh"
#include "GetUniqueName.h"

#include "TROOT.h"
#include "TFile.h"
#include "TList.h"
#include "TText.h"
#include "TSystem.h"
#include "TStyle.h"
	 
#include "TGTab.h"
#include "TGButton.h"
#include "TGLabel.h"
#include "TGLayout.h"
#include "TGMenu.h"
#include "TRootEmbeddedCanvas.h"
#include "TApplication.h"
#include "TGMsgBox.h"
#include "TGFileDialog.h"
#include "TGListBox.h"
#include "TGComboBox.h"

using namespace std;

///Used in saving an event display as PostScript.
const static char *gTTECEventViewerPSType[] = 
  {"PostScript", "*.ps",
   0, 0};

///Used in saving an event display as EPS.
const static char *gTTECEventViewerEPSType[] = 
   {"Encapsulated PostScript", "*.eps",
    0, 0};

///Used in saving an event display as a GIF.
const static char *gTTECEventViewerGIFType[] = 
   {"GIF", "*.gif",
    0, 0};

///Used in saving an event display as a C++ macro.
const static char *gTTECEventViewerCMacroType[] = 
   {"C++ Macro", "*.C",
    0, 0};

///Used in saving an event display in a ROOT file.
const static char *gTTECEventViewerROOTFileType[] = 
   {"ROOT", "*.root",
   0, 0};

ClassImp(TTECEventViewer)

  TTECEventViewer::TTECEventViewer(const char* prdfFileName, Int_t runnumber, Bool_t isSimulation, Int_t firstEventNumber, Bool_t openImmediately) : TQObject(), TObject()
{
  //Create a new TTECEventViewer object for the specified PRDF file.
  //Set the isSimulation parameter to kTRUE if the PRDF file contains
  //simulated data.
  //firstEvent has a default value of 1 and openImmediately a default
  //value of kTRUE. This means that if you call this constructor
  //with only the name of the file, the graphical interface of
  //the TTECEventViewer will appear immediately.
  //If you'd like to delay the appearance of the TTECEventViewer 
  //interface, you may set openImmediately to kFALSE and call
  //the TTECEventViewer's Show method when you want it to appear.

  fIsSimulation = isSimulation;
  fPrdfFileName = new char[strlen(prdfFileName) + 1];
  strcpy(fPrdfFileName, prdfFileName);

  fIterator = 0;

  for (int i = 0; i < 8; i++)
    fModels[i] = 0;

  for (int i = 0; i < TTECEventViewer::kNumberOfTabs; i++)
    fDisplays[i] = 0;

  fViewMenu = 0;

  fVerbosity = 12;
  fUseObjy = kTRUE;
  fHasBeenShown = kFALSE;
  fEventNumber = 0;
  fRunNumber = runnumber;
  fFirstEventNumber = firstEventNumber;
  fMainWindowID = 0;

  if (openImmediately) {
    Show();
  }
}

TTECEventViewer::~TTECEventViewer()
{
  //Clean up.

  delete [] fPrdfFileName;
  delete fIterator;

  for (int i = 0; i < 8; i++) {
    delete fModels[i];
  }

  for (int j = 0; j < TTECEventViewer::kNumberOfTabs; j++)
    delete fDisplays[j];

  fGUIList->Delete(); //Clean up the graphics objects.
  delete fGUIList;
}

void TTECEventViewer::Show()
{
  //Bring up the graphical interface if it has not already
  //been shown.

  if (fHasBeenShown)
    return;
  
  fHasBeenShown = kTRUE;

  fGUIList = new TList; //List of GUI elements to be deleted.

  //Note: each element that is added to fGUIList is shoved
  //to the front of the list using AddFirst (rather than 
  //simply Adding it to the end). This is because the order
  //in which the GUI elements are deleted is significant:
  //it causes a bad X11 crash if the parent frame of a
  //GUI element is deleted before it is. To make sure that
  //we don't forget to add a GUI element to the delete list,
  //we always add it to the list immediately after creatin it.
  //But this means that the element's position in the list
  //must be in the reverse order of its addition to the list.

  UInt_t windowWidth;
  UInt_t windowHeight;
  Int_t x, y;

  //Get dimensions of the screen.
  gVirtualX->GetWindowSize(gClient->GetRoot()->GetId(),
			   x, y, windowWidth, windowHeight);

  //Leave room for the taskbar (blindly assuming we're
  //on Windows and the taskbar's at the bottom).
  if (windowHeight > 29)
    windowHeight -= 28;

  //Set the dimensions to have a ratio similar to the 
  //TEC's.
  windowWidth = (550 * windowHeight) / 770;

  fMainWindow = new TGMainFrame(gClient->GetRoot(), windowWidth,windowHeight);
  fGUIList->AddFirst(fMainWindow);
  fMainWindow->Connect("CloseWindow()","TTECEventViewer",this,"HandleClose()");
  
  TGMenuBar* menuBar = new TGMenuBar(fMainWindow, 1, 1, kHorizontalFrame);
  fGUIList->AddFirst(menuBar);

  //Layout hints for placing the menu bar into the main window.
  TGLayoutHints* menuBarLayoutHints = 
    new TGLayoutHints(kLHintsTop | kLHintsLeft | kLHintsExpandX, 
		      0, 0, 1, 1);
  fGUIList->AddFirst(menuBarLayoutHints);

  //Layout hints for placing the menus into the menu bar.
  TGLayoutHints* menuLayoutHints = new TGLayoutHints(kLHintsTop | kLHintsLeft, 0, 6, 0, 0);
  fGUIList->AddFirst(menuLayoutHints);

  TGPopupMenu* fileMenu = new TGPopupMenu(gClient->GetRoot());
  fGUIList->AddFirst(fileMenu);
  fileMenu->AddEntry("&Open...", TTECEventViewer::kOpen);
  fileMenu->AddEntry("Save as PostScript...", TTECEventViewer::kSaveAsPS);
  fileMenu->AddEntry("Save as EPS...", TTECEventViewer::kSaveAsEPS);
  fileMenu->AddEntry("Save as GIF...", TTECEventViewer::kSaveAsGIF);
  fileMenu->AddEntry("Save as C++ Macro...", TTECEventViewer::kSaveAsCMacro);
  fileMenu->AddEntry("Save as ROOT File...", TTECEventViewer::kSaveAsROOTFile);
  fileMenu->AddSeparator();
  fileMenu->AddEntry("&Close Viewer", TTECEventViewer::kClose);
  fileMenu->AddSeparator();
  fileMenu->AddEntry("&Quit ROOT", TTECEventViewer::kQuitROOT);

  fileMenu->DisableEntry(TTECEventViewer::kOpen);

  fileMenu->Connect("Activated(Int_t)","TTECEventViewer",
		    this,"HandleMenu(Int_t)");

  TGPopupMenu* editMenu = new TGPopupMenu(gClient->GetRoot()); 
  fGUIList->AddFirst(editMenu);
  editMenu->AddEntry("Undo",TTECEventViewer::kUndo);
  editMenu->AddEntry("Redo",TTECEventViewer::kRedo);
  editMenu->AddSeparator();
  editMenu->AddEntry("Next Event", TTECEventViewer::kNextEvent);
  editMenu->AddEntry("Go to Event...", TTECEventViewer::kGotoEvent);
  editMenu->AddSeparator();
  editMenu->AddEntry("Set Verbosity...", TTECEventViewer::kSetVerbosity);

  editMenu->DisableEntry(TTECEventViewer::kUndo);
  editMenu->DisableEntry(TTECEventViewer::kRedo);

  editMenu->Connect("Activated(Int_t)","TTECEventViewer",
		    this,"HandleMenu(Int_t)");

  fViewMenu = new TGPopupMenu(gClient->GetRoot());
  fGUIList->AddFirst(fViewMenu);
  fViewMenu->AddEntry("Reset view",TTECEventViewer::kResetView);
  fViewMenu->AddSeparator();
  fViewMenu->AddEntry("Black and White", TTECEventViewer::kBlackAndWhite);
  fViewMenu->AddEntry("Show Legend", TTECEventViewer::kShowLegend);
  fViewMenu->AddEntry("Show Event Number", TTECEventViewer::kShowEventNumber);
  fViewMenu->AddEntry("Show File Name", TTECEventViewer::kShowFileName);
  fViewMenu->AddSeparator();
  fViewMenu->AddEntry("New window with view",
		     TTECEventViewer::kNewWindowWithView);
 
  fViewMenu->Connect("Activated(Int_t)","TTECEventViewer",
		    this,"HandleMenu(Int_t)");

  menuBar->AddPopup("File", fileMenu, menuLayoutHints);
  menuBar->AddPopup("Edit", editMenu, menuLayoutHints);
  menuBar->AddPopup("View", fViewMenu, menuLayoutHints);

  fMainWindow->AddFrame(menuBar, menuBarLayoutHints);

  //Create the tabbed frame.
  fTabbedFrame = new TGTab(fMainWindow, 700, 400);
  fGUIList->AddFirst(fTabbedFrame);

  fTabbedFrame->Connect("Selected(Int_t)", "TTECEventViewer",
			this, "HandleTabSelection(Int_t)");

  TGLayoutHints* tabbedFrameLayoutHints = 
    new TGLayoutHints(kLHintsTop | kLHintsLeft | 
		      kLHintsExpandX | kLHintsExpandY, 2, 2, 2, 2);
  fGUIList->AddFirst(tabbedFrameLayoutHints);

  //Used in laying out a canvas within a tab frame.
  TGLayoutHints* canvasLayoutHints =
    new TGLayoutHints(kLHintsTop | kLHintsLeft | 
		      kLHintsExpandX | kLHintsExpandY, 5, 5, 5, 5);
  fGUIList->AddFirst(canvasLayoutHints);

  TGLayoutHints* tabButtonFrameLayoutHints =
    new TGLayoutHints(kLHintsBottom | kLHintsLeft, 5, 5, 5, 5);  
  fGUIList->AddFirst(tabButtonFrameLayoutHints);

  TGLayoutHints* tabButtonLayoutHints =
    new TGLayoutHints(kLHintsBottom | kLHintsLeft, 5, 5, 5, 5);
  fGUIList->AddFirst(tabButtonLayoutHints);

  const char* tabTitles[] = {"All",
		     "0 South",
		     "1 South",
		     "2 South",
		     "3 South",
		     "0 North",
		     "1 North",
		     "2 North",
		     "3 North"};

  for (Int_t index = 0; index < TTECEventViewer::kNumberOfTabs; index++) {
    //Create a new tab and get a pointer to the main frame
    //of the new tab.
    TGCompositeFrame* frame = fTabbedFrame->AddTab(tabTitles[index]);

    //Add an embedded canvas. This is where the hits for
    //this particular sector-side will be plotted.

    //Get a unique name for the canvas to avoid overwriting
    //other ROOT objects with the same name.
    const char* name = GetUniqueName("tecCanvas", index);
    TRootEmbeddedCanvas* canvas =
      new TRootEmbeddedCanvas(name, frame,
			      (7 * windowWidth) / 10, 
			      (7 * windowHeight) / 10);
    fGUIList->AddFirst(canvas);
    delete name;

    //Store a pointer to the canvas for drawing into later.
    fCanvases[index] = canvas->GetCanvas();
    fCanvases[index]->SetFrameFillColor(kWhite);
    fCanvases[index]->SetFillColor(kWhite);

    //Get rid of the red rings around the pads.
    //A "better" solution would be to set each
    //pad to be uneditable. Unfortunately, doing so
    //causes the TDragZoomTH2F to crash ROOT.
    //I don't know why.
    fCanvases[index]->SetHighLightColor(kBlack);
    fCanvases[index]->SetBorderMode(0);

    frame->AddFrame(canvas, canvasLayoutHints);
      
    //Container for the buttons.
    TGCompositeFrame* buttonFrame =
      new TGCompositeFrame(frame, 60, 20, kHorizontalFrame);
    fGUIList->AddFirst(buttonFrame);

    TGTextButton* newWindowButton =
      new TGTextButton(buttonFrame, "New window with view", -1);
    fGUIList->AddFirst(newWindowButton);

      //Connect newWindowButton's "Clicked()" signal to this
      //TTECEventViewer's "DisplayInNewWindow()" slot so
      //that a click on newWindowButton will open the currently
      //displayed side and sector in a new window.
    newWindowButton->Connect("Clicked()", "TTECEventViewer",
			     this, "DisplayInNewWindow()");

    newWindowButton->SetToolTipText("Click to copy visible view into new window");

    buttonFrame->AddFrame(newWindowButton, tabButtonLayoutHints);

    TGTextButton* resetButton = 
      new TGTextButton(buttonFrame, "Reset view", -1);
    fGUIList->AddFirst(resetButton);

    resetButton->Connect("Clicked()", "TTECEventViewer", 
			 this, "ResetCurrentView()");

    resetButton->SetToolTipText("Click to reset visible view");

    buttonFrame->AddFrame(resetButton, tabButtonLayoutHints);
      
    frame->AddFrame(buttonFrame, tabButtonFrameLayoutHints);
  }

  fMainWindow->AddFrame(fTabbedFrame, tabbedFrameLayoutHints);

  //Add the "Next event" button to the window's
  //lower right hand corner.
  TGTextButton* nextButton =
    new TGTextButton(fMainWindow, "Next event", -1);
  fGUIList->AddFirst(nextButton);

  TGLayoutHints* nextButtonLayoutHints =
    new TGLayoutHints(kLHintsBottom | kLHintsRight, 5, 5, 5, 5);
  fGUIList->AddFirst(nextButtonLayoutHints);

  nextButton->Connect("Clicked()", "TTECEventViewer",
		      this, "NextEvent()");

  nextButton->SetToolTipText("Click to move viewer onto next event");

  fMainWindow->AddFrame(nextButton, nextButtonLayoutHints);

  //Show the window. According to the guitest.C
  //example in $ROOTSYS/tutorials, we need to call
  //Resize to initialize the layout algorithm.
  fMainWindow->MapSubwindows();
  fMainWindow->Resize(fMainWindow->GetDefaultSize());

  fMainWindow->SetWindowName("TEC Event Viewer");
  fMainWindow->SetIconName("TEC Event Viewer");
  fMainWindow->MapWindow();

  fMainWindow->Move(0, 0);

  //fMainWindowID is used in setting the cursor.
  fMainWindowID = (Window_t)(fMainWindow->GetId());

  //Fill in the display.
  //(It would be better to be able to do this before the 
  //window was shown, but I think doing so would crash ROOT.)
  MoveToEvent(fFirstEventNumber);

  fSelectedTabNumber = fTabbedFrame->GetCurrent();
}

void TTECEventViewer::ResetCurrentView()
{
  //Reset the range of the current view, the currently
  //visible tab. This method is used internally as a slot
  //for signals from the GUI.

  if (!fHasBeenShown)
    return;

  if (fDisplays[fSelectedTabNumber]) {
    gVirtualX->SetCursor(fMainWindowID, gVirtualX->CreateCursor(kWatch));
    fDisplays[fSelectedTabNumber]->ResetBounds();
    gVirtualX->SetCursor(fMainWindowID, gVirtualX->CreateCursor(kPointer));
  }

}

void TTECEventViewer::HandleClose()
{
  //Slot for handling a close of the main window.
  //In response to having this method called,
  //this object will delete itself.

  delete this;
}

void TTECEventViewer::HandleMenu(Int_t selectionID)
{
  //Slot for handling menu events.

  switch (selectionID) {
  case TTECEventViewer::kOpen:
    OpenPRDFFile();
    break;
  case TTECEventViewer::kSaveAsPS:
    SaveAs(gTTECEventViewerPSType);
    break;
  case TTECEventViewer::kSaveAsEPS:
    SaveAs(gTTECEventViewerEPSType);
    break;
  case TTECEventViewer::kSaveAsGIF:
    SaveAs(gTTECEventViewerGIFType);
    break;
  case TTECEventViewer::kSaveAsCMacro:
    SaveAs(gTTECEventViewerCMacroType);
    break;
  case TTECEventViewer::kSaveAsROOTFile:
    SaveAs(gTTECEventViewerROOTFileType);
    break;
  case TTECEventViewer::kClose:
    HandleClose();
    break;
  case TTECEventViewer::kQuitROOT:
    gApplication->Terminate(0);
    break;
  case TTECEventViewer::kUndo:
    break;
  case TTECEventViewer::kRedo:
    break;
  case TTECEventViewer::kNextEvent:
    NextEvent();
    break;
  case TTECEventViewer::kGotoEvent:
    DoGotoEvent();
    break;
  case TTECEventViewer::kSetVerbosity:
    DoSetVerbosity();
    break;
  case TTECEventViewer::kResetView:
    ResetCurrentView();
    break;
  case TTECEventViewer::kBlackAndWhite:
    ToggleBlackAndWhite();
    break;
  case TTECEventViewer::kShowLegend:
    ToggleShowLegend();
    break;
  case TTECEventViewer::kShowEventNumber:
    ToggleShowEventNumber();
    break;
  case TTECEventViewer::kShowFileName:
    ToggleShowFileName();
    break;
  case TTECEventViewer::kNewWindowWithView:
    DisplayInNewWindow();
    break;
  }
}

void TTECEventViewer::NextEvent()
{
  //Advance the display to the next event.
  
  MoveToEvent(fEventNumber + 1);
}

///Update the menus to reflect the current view.
void TTECEventViewer::UpdateMenus()
{
  TTECEventDisplay* display = fDisplays[fSelectedTabNumber];

  if (!display)
    return;

  SetMenuItemState(fViewMenu,
		   TTECEventViewer::kBlackAndWhite,
		   display->GetBlackAndWhite());
  SetMenuItemState(fViewMenu,
		   TTECEventViewer::kShowLegend,
		   display->GetShowLegend());
  SetMenuItemState(fViewMenu,
		   TTECEventViewer::kShowEventNumber,
		   display->GetShowEventNumber());
  SetMenuItemState(fViewMenu,
		   TTECEventViewer::kShowFileName,
		   display->GetShowFileName());
}

/**
 * Allow the user to go to and display
 * an arbitrary event. Open an input
 * dialog to get the event number, verify
 * that the given event number is valid (or not),
 * and then change the viewer's views to display it.
 */
void TTECEventViewer::DoGotoEvent()
{
  if (!fHasBeenShown)
    return;

  char* reply = 0;
  TString numberString(6);
  numberString += fEventNumber + 1;

  //TTextInputDialog takes care of deleting its own memory.
  new TTextInputDialog("Go to Event",
		       "Enter the number of the event to display.",
		       &reply,
		       (const char*) numberString,
		       "Go");

  if (reply) { //User didn't cancel.
    int targetNumber = atoi(reply);
    if (targetNumber > 0)
      MoveToEvent(targetNumber);
    else
      gVirtualX->Bell(100); //BEEP!
    delete [] reply;
  }
}


/** 
 * Bring up a dialog to set the verbosity.
 * Set the verbosity.
 */
void TTECEventViewer::DoSetVerbosity()
{

  //BUG: This is basically a duplicate of
  //code for DoGotoEvent().

  if (!fHasBeenShown)
    return;

  char* reply = 0;
  TString numberString(6);
  numberString += fVerbosity;

  //TTextInputDialog takes care of deleting its own memory.
  new TTextInputDialog("Set Verbosity",
		       "Enter the new verbosity number.",
		       &reply,
		       (const char*) numberString,
		       "Set Verbosity");

  if (reply) { //User didn't cancel
    int verbosity = atoi(reply);
    SetVerbosity(verbosity);
    delete [] reply;
  }
}


///Toggle whether the current view is black and white.
void TTECEventViewer::ToggleBlackAndWhite()
{
  TTECEventDisplay* display = fDisplays[fSelectedTabNumber];

  if (!display)
    return;

  Bool_t blackAndWhite = !(display->GetBlackAndWhite());

  gVirtualX->SetCursor(fMainWindowID, gVirtualX->CreateCursor(kWatch));  
  display->SetBlackAndWhite(blackAndWhite);
  gVirtualX->SetCursor(fMainWindowID, gVirtualX->CreateCursor(kPointer));

  SetMenuItemState(fViewMenu,
		   TTECEventViewer::kBlackAndWhite,
		   blackAndWhite);
}

///Toggle whether the current view has a legend.
void TTECEventViewer::ToggleShowLegend()
{
  TTECEventDisplay* display = fDisplays[fSelectedTabNumber];

  if (!display)
    return;

  Bool_t showLegend = !(display->GetShowLegend());

  gVirtualX->SetCursor(fMainWindowID, gVirtualX->CreateCursor(kWatch));
  display->SetShowLegend(showLegend);
  gVirtualX->SetCursor(fMainWindowID, gVirtualX->CreateCursor(kPointer));

  SetMenuItemState(fViewMenu,
		   TTECEventViewer::kShowLegend,
		   showLegend);
}

///Toggle whether the current view shows its event number.
void TTECEventViewer::ToggleShowEventNumber()
{
  TTECEventDisplay* display = fDisplays[fSelectedTabNumber];

  if (!display)
    return;

  Bool_t showEventNumber = !(display->GetShowEventNumber());
  display->SetShowEventNumber(showEventNumber);

  SetMenuItemState(fViewMenu,
		   TTECEventViewer::kShowEventNumber,
		   showEventNumber);
}

///Toggle whether the current view shows its file name.
void TTECEventViewer::ToggleShowFileName()
{
  TTECEventDisplay* display = fDisplays[fSelectedTabNumber];

  if (!display)
    return;

  Bool_t showFileName = !(display->GetShowFileName());
  display->SetShowFileName(showFileName);

  SetMenuItemState(fViewMenu,
		   TTECEventViewer::kShowFileName,
		   showFileName);
}

void TTECEventViewer::HandleTabSelection(Int_t tabNumber)
{
  //Slot for handling tab selections. This method keeps
  //the internals of the viewer in sync with which
  //tab (side-sector) that is currently visible.

  fSelectedTabNumber = tabNumber;

  if (fHasBeenShown) {
    if (fDisplays[fSelectedTabNumber] &&
	!fDisplays[fSelectedTabNumber]->HasBeenDrawn()) {

      gVirtualX->SetCursor(fMainWindowID, gVirtualX->CreateCursor(kWatch));

      fDisplays[fSelectedTabNumber]->Draw();

      gVirtualX->SetCursor(fMainWindowID, gVirtualX->CreateCursor(kPointer));
    }

    UpdateMenus();

  }
}

void TTECEventViewer::DisplayInNewWindow()
{
  //Display the currently visible side-sector in a new
  //window. This method is used as a slot.

  if (!fHasBeenShown)
    return;

  //Get a pointer to the canvas of the currently
  //visible event display.
  TCanvas* currentCanvas = fCanvases[fSelectedTabNumber];

  //Note:
  //currentCanvas->GetWindowWidth() and 
  //currentCanvas->GetWindowHeight() always return zero.
  //It is a bad idea to pass zero to the TCanvas
  //constructor for either its width or height. Doing
  //so leads to a nasty X11 error.
  //This is why we don't set the new TCanvas's dimensions
  //to the dimensions of the present view.


  //Alternatives to ROOT's TString are C++'s string or 
  //ostringstream.
  TString title("TEC Event ");
  title += fEventNumber;
  if (fDisplays[fSelectedTabNumber]) {
    title += ", ";
    title += fDisplays[fSelectedTabNumber]->GetTitle();
  }

  char* canvasName = GetUniqueName("displayCanvas", 
      TTECEventViewer::kNumberOfTabs * fEventNumber + fSelectedTabNumber);
  TCanvas* displayCanvas = new TCanvas(canvasName, (const char*)title,
				       0, 0, 400, 400);
  delete [] canvasName;

  gROOT->SetSelectedPad(displayCanvas);

  //Draw a copy of the contents of currentCanvas
  //into the current pad (displayCanvas).
  currentCanvas->DrawClonePad();

  //Ensure that all of the objects displayed in the new
  //canvas have their kCanDelete bit set. Otherwise,
  //they may leak memory.
  RecursiveSetBit(displayCanvas->GetListOfPrimitives(), kCanDelete);

  displayCanvas->Modified();
  displayCanvas->Update();
}

/**
 * Save the current view to a file.
 * The fileType array is assumed to be in a
 * format acceptable to TGFileInfo for TGFileDialog.
 * That is {"[File type name]", "*.[extension]", 0, 0},
 * filling in the bracketed portions with the specifics.
 *
 * @param fileType Description of the file type to save as.
 */
void TTECEventViewer::SaveAs(const char** fileTypeArray)
{
  if (!fHasBeenShown)
    return;

  char* fileName = 0;

  Bool_t keepOnAsking = kTRUE;

  //Extract the given file suffix and file type.
  char* suffix = (char*)(fileTypeArray[1]) + 1; //Advance past the '*'.

  TGFileInfo fileInfo;
  fileInfo.fIniDir = 0;

#ifdef CONST_TGFILEINFO
  fileInfo.fFileTypes =  fileTypeArray;
#else
  fileInfo.fFileTypes =  (const char **) fileTypeArray;
#endif

  while (keepOnAsking) {
    new TGFileDialog(gClient->GetRoot(),
		     gClient->GetRoot(),
		     kFDSave,
		     &fileInfo);
    
    if (!fileInfo.fFilename) { //User canceled.
      if (fileName)
	delete [] fileName;
      return;
    }

    if (strstr(fileInfo.fFilename, suffix)) {
      fileName =
	new char[strlen(fileInfo.fFilename) + 1];
      strcpy(fileName, fileInfo.fFilename);
    }
    else {
      fileName = 
	new char[strlen(fileInfo.fFilename) +
		strlen(suffix) + 1];
      strcpy(fileName, fileInfo.fFilename);
      strcat(fileName, suffix);
    }

    //Before ROOT 3.01/06, it was the calling code's
    //responsibility to clean up the TGFileInfo object's
    //fFilename memory. With 3.01/06, TGFileInfo now takes
    //care of deleting its fFilename itself. To be compatible
    //with both pre and post 3.01/06 versions, we set the
    //fFilename field to NULL after deleting it. It is OK
    //to delete a NULL pointer.
    delete [] fileInfo.fFilename;
    fileInfo.fFilename = 0;

    keepOnAsking = kFALSE;

    Long_t ignore;
    Bool_t fileExists = !(gSystem->GetPathInfo(fileName, &ignore, 
				      &ignore, &ignore, &ignore));
    if (fileExists) {
      keepOnAsking = kTRUE;

      Int_t reply;
      const char* existsMask = "%s already exists. Overwrite?";
      char* existsMessage = new char[strlen(fileName) + 
				    strlen(existsMask) - 1];
      sprintf(existsMessage, existsMask, fileName);

      new TGMsgBox(gClient->GetRoot(),
		   gClient->GetRoot(),
		   "File exists!",
		   existsMessage,
		   kMBIconExclamation,
		   kMBYes | kMBCancel,
		   &reply);

      delete [] existsMessage;

      if (reply == kMBYes)
	keepOnAsking = kFALSE;
    }

  }

  fCanvases[fSelectedTabNumber]->Print(fileName);

  delete [] fileName;
}

/**
 * Open a new TTECEventViewer with the file
 * the user selects. Bring up a file dialog
 * to get the file name and a dialog to determine
 * whether the file contains simulated data.
 */
void TTECEventViewer::OpenPRDFFile()
{
  if (!fHasBeenShown)
    return;
}



Bool_t TTECEventViewer::MoveToEvent(Int_t targetNumber)
{
  //Display event number targetNumber.
  //If this is the first time this method has been
  //called, take care of some extra initialization.
  //If there was a problem in getting the next event,
  //return kFALSE; otherwise, if everything went
  //A-okay, return kTRUE.
  //If the TTECEventViewer has not yet been shown,
  //this method will return kFALSE immediately.

  if (!fHasBeenShown) {
    cerr << "ERROR! TTECEventViewer::MoveToEvent was called before the " <<
      "viewer was Show()ed" << endl;
    return kFALSE;
  }

  if (targetNumber <= 0) {
    cerr << "ERROR! TTECEventViewer::MoveToEvent called with targetNumber <= 0"
	 << endl;
    return kFALSE;
  }

  if (fEventNumber == targetNumber)
    return kTRUE; //Already in the right spot.

  //Let the user know that we're doing stuff.
  gVirtualX->SetCursor(fMainWindowID, gVirtualX->CreateCursor(kWatch));

  //Change the event viewer window title.
  TString loadingTitle(30);
  loadingTitle += "Loading TEC Event ";
  loadingTitle += targetNumber;
  loadingTitle += "...";
  fMainWindow->SetWindowName((const char*) loadingTitle);
  fMainWindow->SetIconName((const char*) loadingTitle);

  if (!fIterator) {
    fIterator = new TTECEventIterator(fPrdfFileName,
				      fRunNumber,
				      fIsSimulation,
				      fUseObjy);
    fIterator->SetVerbosity(fVerbosity);
  }

  fTabbedFrame->SetTab(0);
  fSelectedTabNumber = 0;

  for (Int_t i = 0; i < TTECEventViewer::kNumberOfTabs; i++) {
    delete fDisplays[i];
    fDisplays[i] = 0;
  }

  fCanvases[0]->SetCursor(kWatch);
  fCanvases[0]->Update();

  if (!fIterator->MoveTo(targetNumber)) {

    if (fIterator->IsPastLast()) {

      //If fIterator has been moved past the last event,
      //the iterator's event number is equal to one
      //more than the last valid event number.
      fEventNumber = fIterator->GetEventNumber() - 1;

      if (fVerbosity > 0) {
	cerr << "TTECEventViewer error:" << endl;
	cerr << "  Expected at least " << targetNumber <<
	  " events, but found only " << fEventNumber << endl;
      }

      TString message("There are no more events to view after ");
      message += fEventNumber;
      message += ".";
  
      //TGMsgBox takes care of its own memory.
      new TGMsgBox(fMainWindow, gClient->GetRoot(),
		   "No more events!", 
		   (const char*) message,
		   kMBIconExclamation,
		   kMBClose);
    }

    return kFALSE;
  }

  fEventNumber = fIterator->GetEventNumber();

  for (int i = 0; i < 8; i++) {
    //Get the sector and side corresponding to this
    //index.
    Int_t sector;
    Int_t side;
    IndexToSectorSide(i, sector, side);
    
    fModels[i] = fIterator->GetEvent(sector, side);

    if (fModels[i])
      fDisplays[i + 1] = new TTECEventDisplay(fCanvases[i + 1], fModels[i]);

  }

  //The last for numbers are xMin, yMin, xMax, and yMax in TEC coordinates
  //for the display.
  fDisplays[0] = new TTECEventDisplay(fCanvases[0], "TEC East Arm", -550, -300, 0, 400);
  
  for (int i = 0; i < 8; i++) {
    fDisplays[0]->Add(fModels[i]);
  }

  fDisplays[0]->Draw();

  //Change the event viewer window title.
  TString newTitle(30);
  newTitle += "TEC Event ";
  newTitle += fEventNumber;
  newTitle += " - ";
  newTitle += fPrdfFileName;
  fMainWindow->SetWindowName((const char*) newTitle);
  fMainWindow->SetIconName((const char*) newTitle);

  UpdateMenus();

  gVirtualX->SetCursor(fMainWindowID, gVirtualX->CreateCursor(kPointer));

  return kTRUE;
}

/*
Int_t TTECEventViewer::SectorSideToIndex(Int_t sector, Int_t side)
{
  //Take the given sector and side and return a number
  //to be used as an index into fCanvases or fDraggers.

  //The first tab is reserved for a display of all of the events.

  return 4 * side + sector + 1;
}
*/

void TTECEventViewer::IndexToSectorSide(Int_t index,
					Int_t& sector,
					Int_t& side)
{
  //Set sector and side to the sector and side corresponding
  //to the given tab index.

  side = index / 4; //Depend on int math truncation.
  sector = index - 4 * side;

}

/**
 * Set whether the specified menu item is checked
 * or unchecked. state == kTRUE for a check,
 * state == kFALSE for no check.
 *
 * @param menu The menu to change.
 * @param itemID ID of the menu item to change.
 * @param state kTRUE for check; kFALSE for no check.
 */
void TTECEventViewer::SetMenuItemState(TGPopupMenu* menu, 
				       Int_t itemID,
				       Bool_t state)
{
  Bool_t currentState = menu->IsEntryChecked(itemID);
  if (state != currentState) {
    if (state)
      menu->CheckEntry(itemID);
    else
      menu->UnCheckEntry(itemID);
  }
}


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
void TTECEventViewer::RecursiveSetBit(TList* list, UInt_t flag)
{
  TIterator* iter = list->MakeIterator();
  TObject* obj = iter->Next();
  while (obj) {
    obj->SetBit(flag);
    if (obj->InheritsFrom("TVirtualPad")) {
      TVirtualPad* pad = static_cast<TVirtualPad*>(obj);
      TList* padList = pad->GetListOfPrimitives();
      RecursiveSetBit(padList, flag);
    }
    obj = iter->Next();
  }

  delete iter;
}


















































