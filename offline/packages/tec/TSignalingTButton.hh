#ifndef __TSIGNALINGTBUTTON_HH__
#define __TSIGNALINGTBUTTON_HH__

#include <TButton.h>

//
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
//June 15, 2001, dfaden@iastate.edu
//
//This code was created in connection with the procedure to
//draw an event display in mTecHoughTrackModule.
//

class TSignalingTButton: public TButton {
 public:
  TSignalingTButton(const char* title, 
		    Double_t x1, 
		    Double_t y1, 
		    Double_t x2, 
		    Double_t y2);
  virtual ~TSignalingTButton() {}
  virtual void ExecuteEvent(Int_t event, Int_t px, Int_t py);

  //TSignalingTButton's signals are modeled after
  //ROOT's TGButton; it seems likely that someday
  //the default implementation of TButton will
  //also produce these signals. If this is the case,
  //it will take very little work to convert code
  //using TSignalingTButton to use TButton.

  void Pressed() { Emit("Pressed()"); } //*SIGNAL*
  void Released() { Emit("Released()"); } //*SIGNAL*

  ClassDef(TSignalingTButton,0)
  
};

#endif







