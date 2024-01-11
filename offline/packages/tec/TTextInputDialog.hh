#ifndef __TSTRINGINPUTDIALOG_HH__
#define __TSTRINGINPUTDIALOG_HH__

#include <TGFrame.h> //Holds all the frame definitions.
#include <TList.h>
#include <TGTextEntry.h>

/**
 * A class to get a line of text input from the user.
 * A dialog with a text field, OK and cancel buttons
 * is popped up.
 * This class requires ROOT 3 to compile and run.
 * It is based off of the dialogs.C ROOT tutorial.
 *
 * @author David Faden, dfaden@iastate.edu
 */
class TTextInputDialog : public TQObject {
  
public:
  /**
   * Create a new TStringInputDialog.
   * Pop up a dialog with the specified title.
   * Inside the dialog, the text field is accompanied
   * by the given message, indicating the purpose of
   * the dialog.
   *
   * If the user clicks the OK button or
   * hits return within the text field, returnValue will
   * point to a copy of the text the user entered.
   * The caller is responsible for deleting the C string
   * pointed to by returnValue.
   * If the user clicks the cancel button or closes
   * the dialog, returnValue will point to NULL.
   * In either case, when the dialog closes, it takes
   * care of deleting its own memory.
   * 
   * @param title The title of the dialog.
   * @param message Message accompanying text field.
   * @param returnValue Holds the entered text or NULL.
   * @param defaultValue The default text for the text field.
   * Defaults to the empty string.
   * @param okButtonText Text for the OK button. Defaults
   * to "OK".
   * @param cancelButtonText Text for the cancel button.
   * Defaults to "Cancel".
   */
  TTextInputDialog(const char* title,
		     const char* message,
		     char** returnValue,
		     const char* defaultValue = "",
		     const char* okButtonText = "OK",
		     const char* cancelButtonText = "Cancel");
		     
  ~TTextInputDialog();

  ///Handle the closing of the main window.
  void CloseWindow();

  ///Handle a press of the cancel button.
  void Cancel();

  /**
   * Accept the current input as what the user
   * wants to input. Close the dialog, and
   * set returnValue to point to a copy of
   * the entered text.
   */
  void AcceptInput();

private:
  TGTransientFrame* fDialog;
  TList* fGUIList; //List of GUI elements.
  TGTextEntry* fTextEntry; //Text field (taken care of with fGUIList).
  char** fReturnValue; //Points to the text the user entered.


  ClassDef(TTextInputDialog,0)

};

#endif











