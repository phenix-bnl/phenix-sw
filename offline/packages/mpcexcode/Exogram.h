#ifndef _Exogram_h_
#define _Exogram_h_

#include<vector>
#include<map>

#include<TDataType.h>
#include<TH3D.h>

class TH2D;
//
//  Hello MPC-EX fans:
//
//    The class found in this file is called Exogram.  This
//  name is only because it is hard to pronounce MPCEX-ogram.
//  The class initially was envisioned simply as an event display
//  which takes advantage of the fact that a histogram is not only 
//  an excellent format for storing rectangular information,
//  (NOTE:  it is IDEAL for square and adapted to rectangles here),
//  but because of the advanced stage of development of this object
//  in root it can be manipulated at a very high level with the
//  simplest of commands.
//
//    It was recognized early in the development of the class that it
//  can also become a TRULY IDEAL basis for sophistocated pattern
//  recognition.  It is anticipated by the authors that this class
//  will eventually be ubiquitous throughout the MPC-EX code.
//
//  The MPC-EX yields hits (Raw or calibrated) that are:
//    -- Rectangular in shape (some long in x, some long in y)
//    -- Distributed in layers along z
//    -- Record some amount of charge deposit.
//
//  This level of information is cleanly represented as a 3D histogram:
//    -- X vs Y spans one layer
//    -- Z indexes layer-to-layer
//    -- Cell content represents:
//       * Charge deposit OR
//       * Hit/no-hit  OR
//       * Number of MIPS
//       * Anything the user dreams up at run time!
//
//  The only inconvenince of a 3D histogram is histogram cells are
//  logically square instead of rectangular.  This single fact is the 
//  reason why we develop the new Exogram class.
//
//  Exograms inherit the full public interface from TH3D and then add to
//  the TH3D one extra fill method:
//
//  FillEx(key, weight);
//
//  *The unique MPC-EX keys specify which channel of the detector 
//   provided a hit.
//  *The weight (defaulting to 1.00) allows the user to choose the 
//   meaning of the  cell contant at run time.
//
//  The FillEx routine fills the set of square TH3D cells that 
//  match the rectangle formed by the true MPC-EX Hit, each with 
//  the very same weight.
//
//                                  TKH, AH, NA  1-28-2014
//

class Exogram : public TH3D
{
 public:
  
  //! Default constructor
  Exogram();
  
  //! Full constructor(s)
  Exogram(const char* name, const char* title, 
	  Int_t nbinsx, const Float_t* xbins, 
	  Int_t nbinsy, const Float_t* ybins, 
	  Int_t nbinsz, const Float_t* zbins, bool P=false);
  Exogram(const char* name, const char* title, 
	  Int_t nbinsx, const Double_t* xbins, 
	  Int_t nbinsy, const Double_t* ybins, 
	  Int_t nbinsz, const Double_t* zbins, bool P=false);
  Exogram(const char* name, const char* title, 
	  Int_t nbinsx, Double_t xlow, Double_t xup, 
	  Int_t nbinsy, Double_t ylow, Double_t yup, 
	  Int_t nbinsz, Double_t zlow, Double_t zup, bool P=false);

  //! Virtual destructor
  ~Exogram( void ) {}

  //! Fill the histogram at a coordinate based on the key and with the given weight.
  void FillEx(unsigned int key, Double_t weight);

  // Custom Draw Methods
  void DrawLayer(int layer, Option_t* option = "");
  TH2D* GetLayer(int layer);
  TH2D* MultiplyLayers(int nlayers = 0);

  //  For testing purposes only...
  void Discretize();  

protected:
  // Determines the mapping between each MPC-EX channel
  // and a vector of bin indices from the histogram.
  void _MapTheBins(bool Projective);
  void _SetMultiplyBinContent(TH2D* histo);

  std::map<unsigned int, std::vector<Int_t> > _FillingMap;

  ClassDef(Exogram,1)
};
#endif
