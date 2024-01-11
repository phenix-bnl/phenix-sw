#include "TSignalingTButton.hh"

ClassImp(TSignalingTButton)

TSignalingTButton::TSignalingTButton(const char* title,
		    Double_t x1, 
		    Double_t y1, 
		    Double_t x2, 
		    Double_t y2) : TButton(title, "", x1, y1, x2, y2) 
{
  //A TSignalingTButton is simply a TButton
  //that emits signals so that other classes
  //may respond to actions on the TButton
  //through ROOT's signal/slot mechanism.
  //This mechanism is described at
  //http://root.cern.ch/root/HowtoSignalSlot.html.
  //The advantage of having a TButton emit
  //signals is that other classes may respond
  //to actions on the TButton without shoving
  //a bunch of obscure code into the TButton's
  //command string or subclassing TButton to accomplish
  //very narrow goals.
  //
  //For example, to have a TSignalingTButton closeButton 
  //close a TCanvas c1 in response to a click, one could use
  //the following code:
  //
  //closeButton->Connect("Clicked()","TCanvas",c1,"Close()");
  //

}


void TSignalingTButton::ExecuteEvent(Int_t event, Int_t px, Int_t py)
{
  //Respond to an event, such as from a mouse movement over 
  //this button.
  //Don't emit any signals if this TSignalingTButton has
  //been marked as editable.
  //If the event corresponds to the pressing of mouse button 1,
  //emit a "Pressed()" signal.
  //If the event corresponds to the release of mouse button 1,
  //emit a "Released()" signal.

  if (!IsEditable()) {
    if (event == kButton1Down) {
      Pressed();
    }
    else if (event == kButton1Up) {
      Released();
    }
  }

  //Call TButton's implementation of ExecuteEvent().
  TButton::ExecuteEvent(event,px,py);
}

