#ifndef __TGeneralXingShift_h_
#define __TGeneralXingShift_h_

#include <TGeneralXingShiftInput.h>
#include <xingshift_chi2.h>
#include <vector>

///////////////////////////////////////////////////////////////////////////////
//
// class TGeneralXingShift
//
//     calculate absolute xing shift, and blue/yelllow relative shift.
//
///////////////////////////////////////////////////////////////////////////////
//
// 1. set physics TRing using "set_physics_ring(beam_color,TRing);"
// 2. set intended TRingPattern using"set_intended_ring_pattern(beam_color,TRingPattern);"
// 3. use find_shift_using_match()
// 4. use get_result to access the result ( vector<xingshift_chi2> result;)
//      result.size() > 1  : multiple possibility
//      result.size() == 1 : uniquely determined
//      result.size() == 0 : NOTHING MATCHED
//      the elements of the vector:
//        xingshift_chi2.chi2          = 0;
//        xingshift_chi2.ishift        = (0-119) :: absolute xing shift;
//                                                  corrected xing = ( GL1 xing + ishift ) % 120;
//        xingshift_chi2.relativeshift = (0-119) :: BLUE#0 meets YELLOW#relativeshift
//                        
///////////////////////////////////////////////////////////////////////////////

class TGeneralXingShift: public TGeneralXingShiftInput
{
public:
  TGeneralXingShift() {}
  virtual ~TGeneralXingShift() {}

  void clear();
  void find_shift_using_match();
  void find_shift_using_chi2_neglect_pattern();
  void find_shift_using_chi2();

  const std::vector<xingshift_chi2>& get_result() const { return xingshift_result; }

  friend class TGeneralXingShiftTest; // test module

protected: // methods for internal use
  const TRingPattern& get_testxing(beam_color color) const;

  void make_testxing_or(int relativeshift); // blue #0 meets yellow #relativeshift
  void make_testxing_and(int relativeshift); // blue #0 meets yellow #relativeshift

  void normalize_physics_ring();
  float chi2_neglect_pattern(int ishift);

private:
  TRingPattern intended_ring_pattern_testxing[2];
  std::vector<xingshift_chi2> xingshift_result;

};

#endif
