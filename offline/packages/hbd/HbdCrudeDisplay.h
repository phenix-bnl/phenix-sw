#ifndef __HBDCRUDEDISPLAY_HH_
#define __HBDCRUDEDISPLAY_HH_

#include "map"
#include "TH2F.h"
#include "TPolyLine.h"

#define HBD_PADS 68
#define HBD_ROWS 9
#define HBD_COLS 15

//
//  This crude event display simply inherits from
//  TH2F histograms and adds a method that knows the 
//  locations of pads on the true HBD by pad number.
//
//  The histogram is filled in sets of 4 offset cells
//  to mimic the hexagonal pattern of the true HBD.
//
//                          TKH 4-14-2006
//
// hexagons! Anne 4/27/2006
class TGraph;
class TText;

class HbdCrudeDisplay : public TH2F
{

public:
  HbdCrudeDisplay();
  virtual ~HbdCrudeDisplay() {}

public:
  void SetCharge(unsigned int PadNumber, float Charge);
  void draw_hexagons();
  int get_padnumber(int row, int col);
  int get_row(int padnumber);
  int get_col(int padnumber);

  void SetScaleMax(int val) {ScaleMax = val;}
  void SetScaleMin(int val) {ScaleMin = val;}
  void SetLogY    (int val) {LogY     = val;}

  void SetAutoScale(int val) {AutoScale = val;}

private:
  int X[HBD_PADS];
  int Y[HBD_PADS];
  TPolyLine *pads[HBD_ROWS*HBD_COLS];
  int pad_colors[HBD_PADS];
  void set_colors();
  TGraph* g1;
  TText* tt;
  int dispd;

  int AutoScale,LogY;
  float ScaleMax,ScaleMin;
};


#endif /* __HBDCRUDEDISPLAY_HH_ */
