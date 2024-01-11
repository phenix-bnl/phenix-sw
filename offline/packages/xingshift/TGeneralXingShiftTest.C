#include "TGeneralXingShiftTest.h"
#include <iostream>

using namespace std;

TGeneralXingShiftTest::TGeneralXingShiftTest()
{
}

void TGeneralXingShiftTest::make_intended_fill_pattern()
{
  TBunch::pattern pat_seed_blue[8]   ={ TBunch::up  , TBunch::unfill, TBunch::down, TBunch::unfill,
				       TBunch::up  , TBunch::unfill, TBunch::down, TBunch::unfill};
  TBunch::pattern pat_seed_yellow[8] ={ TBunch::up  , TBunch::unfill, TBunch::up  , TBunch::unfill,
				       TBunch::down, TBunch::unfill, TBunch::down, TBunch::unfill};
 
  TRingPattern intended_blue,intended_yellow;

  for( int i = 0; i < TRing::nBunch - 8; i ++){
    intended_blue.at(i) = pat_seed_blue[i%8];
    intended_yellow.at(i) = pat_seed_yellow[i%8];
  }
  cout << "TEST " << endl;
  mxingshift.set_intended_ring_pattern(TGeneralXingShift::blue  ,intended_blue);
  mxingshift.set_intended_ring_pattern(TGeneralXingShift::yellow,intended_yellow);
}

void TGeneralXingShiftTest::run()
{
  make_intended_fill_pattern();

  mxingshift.get_intended_ring_pattern(TGeneralXingShift::blue).show();
  mxingshift.get_intended_ring_pattern(TGeneralXingShift::yellow).show();
  
  cout << " BLUE#0 meets YELLOW#80" << endl;

  mxingshift.make_testxing_or(80);
  mxingshift.get_testxing(TGeneralXingShift::blue).show();
  mxingshift.get_testxing(TGeneralXingShift::yellow).show();

}
