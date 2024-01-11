#include "TTextInputDialog.hh"

#include <TGTextBuffer.h>
#include <TGFrame.h>
#include <TGClient.h>
#include <TVirtualX.h>
#include <TGButton.h>
#include <TGLayout.h>
#include <TGLabel.h>

#include <string.h>

ClassImp(TTextInputDialog)

/**
 * Create a new TTextInputDialog.
 * Pop up a dialog with the specified title.
 * Inside the dialog, the text field is accompanied
 * by the given message, indicating the purpose of
 * the dialog.
 * <p>
 * If the user clicks the OK button or
 * hits return within the text field, returnValue will
 * be set to contain a copy of the text the user entered.
 * The caller is responsible for deleting returnValue's
 * memory.
 * <p>
 * If the user clicks the cancel button or closes
 * the dialog, returnValue will be set to NULL.
 * In either case, when the dialog closes, it takes
 * care of deleting its own memory.
 */
TTextInputDialog::TTextInputDialog(const char* title,
				   const char* message,
				   char** returnValue,
				   const char* defaultValue,
				   const char* okButtonText,
				   const char* cancelButtonText)
  : TQObject()
{
  fReturnValue = returnValue;
  if (fReturnValue)
    *fReturnValue = 0;

  fGUIList = new TList;

  //GUI elements must be deleted child first, parent
  //later so we push GUI elements to the front
  //of fGUIList.

  fDialog = new TGTransientFrame(gClient->GetRoot(), gClient->GetRoot(), 400, 300);
  fGUIList->AddFirst(fDialog);

  fDialog->Connect("CloseWindow()", "TTextInputDialog",
		  this, "CloseWindow()");

  TGVerticalFrame* frame = new TGVerticalFrame(fDialog, 400, 300);
  fGUIList->AddFirst(frame);

  //Hints on how to layout frame within its parent.
  TGLayoutHints* frameLayoutHints =
    new TGLayoutHints(kLHintsExpandX | kLHintsExpandY,
		      1, 1, 1, 1);
  fGUIList->AddFirst(frameLayoutHints);

   TGLabel* label = new TGLabel(frame, message);
  fGUIList->AddFirst(label);

  TGLayoutHints* labelLayoutHints =
    new TGLayoutHints(kLHintsTop | kLHintsLeft, 5, 5, 5, 5);
  fGUIList->AddFirst(labelLayoutHints);
  frame->AddFrame(label, labelLayoutHints);

  TGTextBuffer* textBuffer = new TGTextBuffer(256);
  //textBuffer will be deleted with its parent TGTextEntry,
  //fTextEntry. textBuffer can grow as large as necessary.

  //Set the default value.
  textBuffer->AddText(0, defaultValue);

  fTextEntry = new TGTextEntry(frame, textBuffer);
  fGUIList->AddFirst(fTextEntry);

  fTextEntry->Resize(200, fTextEntry->GetDefaultHeight());

  //Connect a press of the return key in the text field
  //to accepting the text within the text field.
  fTextEntry->Connect("ReturnPressed()", "TTextInputDialog",
		      this, "AcceptInput()");

  TGLayoutHints* textEntryLayoutHints = 
    new TGLayoutHints(kLHintsExpandX, 5, 5, 5, 5);
  fGUIList->AddFirst(textEntryLayoutHints);

  frame->AddFrame(fTextEntry, textEntryLayoutHints);

  TGLayoutHints* buttonFrameLayoutHints = 
    new TGLayoutHints(kLHintsBottom | kLHintsRight, 5, 5, 5, 5);
  fGUIList->AddFirst(buttonFrameLayoutHints);

  TGCompositeFrame* buttonFrame = 
    new TGCompositeFrame(frame, 200, 70, kHorizontalFrame);
  fGUIList->AddFirst(buttonFrame);

  //Hints for laying out the buttons in buttonFrame.
  TGLayoutHints* buttonLayoutHints =
    new TGLayoutHints(kLHintsBottom | kLHintsLeft, 5, 5, 5, 5);
  fGUIList->AddFirst(buttonLayoutHints);
  
  TGTextButton* okButton = new TGTextButton(buttonFrame, okButtonText);
  fGUIList->AddFirst(okButton);

  okButton->Connect("Clicked()", "TTextInputDialog",
		    this, "AcceptInput()");

  buttonFrame->AddFrame(okButton, buttonLayoutHints);

  TGTextButton* cancelButton =
    new TGTextButton(buttonFrame, cancelButtonText);
  fGUIList->AddFirst(cancelButton);
  
  cancelButton->Connect("Clicked()", "TTextInputDialog",
			this, "Cancel()");

  buttonFrame->AddFrame(cancelButton, buttonLayoutHints);

  frame->AddFrame(buttonFrame, buttonFrameLayoutHints);

  fDialog->AddFrame(frame, frameLayoutHints);

  //The following code is almost a direct copy of dialogs.C
  Window_t wdum;
  int ax;
  int ay;

  int width = fDialog->GetDefaultWidth();
  int height = fDialog->GetDefaultHeight();
  
  const TGWindow* main = gClient->GetRoot();

  fDialog->Resize(width, height);

  fDialog->MapSubwindows();

  gVirtualX->TranslateCoordinates(main->GetId(),
				  main->GetId(),
           (((TGFrame *) main)->GetWidth() - width) / 2,
	   (((TGFrame *) main)->GetHeight() - height) / 2,
                ax, ay, wdum);
  fDialog->Move(ax, ay);
  fDialog->SetWMPosition(ax, ay);

   //Make this fDialog unresizable
   fDialog->SetWMSize(width, height);
   fDialog->SetWMSizeHints(width, height, width, height, 0, 0);

   fDialog->SetMWMHints(kMWMDecorAll | kMWMDecorResizeH  | kMWMDecorMaximize |
	       kMWMDecorMinimize | kMWMDecorMenu,
	       kMWMFuncAll  | kMWMFuncResize    | kMWMFuncMaximize |
	          kMWMFuncMinimize,
	       kMWMInputModeless);


  fDialog->SetWindowName(title);
  fDialog->SetIconName(title);

  //Pop up this dialog and wait for the user's reply.
  fDialog->MapWindow();

  fTextEntry->SelectAll();
  fTextEntry->SetFocus();

  gClient->WaitForUnmap(fDialog);
}
		     
TTextInputDialog::~TTextInputDialog()
{
  fGUIList->Delete();
  delete fGUIList;
}

//Note: Before, in response to a click of the
//cancel button or the ok (accept) button,
//this object would delete itself directly.
//This would lead to a segmentation fault
//because the signal/slot machinery still had
//internal references to the buttons, which
//had just been deleted. Fons Rademaker pointed
//out the problem and the solution. To solve the
//problem, rather than having a button press
//lead this object to delete itself directly,
//Rademaker suggested the code be changed
//to signal a window close on a button press.


///Handle the closing of the main window.
void TTextInputDialog::CloseWindow()
{
  delete this;
}

///Handle a press of the cancel button.
void TTextInputDialog::Cancel()
{
  fDialog->SendCloseMessage();
}

/**
 * Accept the current input as what the user
 * wants to input. Close the dialog, and
 * set returnValue to point to a copy of
 * the entered text.
 */
void TTextInputDialog::AcceptInput()
{
  if (fReturnValue) {
    const char* value = fTextEntry->GetBuffer()->GetString();
    *fReturnValue = new char[strlen(value) + 1];
    strcpy(*fReturnValue, value);
  }

  fDialog->SendCloseMessage();
}










