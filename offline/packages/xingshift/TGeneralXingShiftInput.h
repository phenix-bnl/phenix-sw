#ifndef __TGeneralXingShiftInput_h_
#define __TGeneralXingShiftInput_h_

// other headers
#include<vector>
#include"TRingPattern.h"
#include"TRing.h"

///////////////////////////////////////////////////////////////////////////////
// TGeneralXingShiftInput
//
//  base class to do some pattern matching between
//  observed / intended fill and/or polarization pattern
//
//              Author: Kazuya Aoki
//
//
//
/////////////////////////////////////////////////////////////////////////////////

class TGeneralXingShiftInput
{
public:
  TGeneralXingShiftInput();
  virtual ~TGeneralXingShiftInput();

  enum beam_color {
    blue = 0,yellow = 1
  };

  // setters
  // if beam_color is out of range, they do nothing.
  void set_physics_ring( TGeneralXingShiftInput::beam_color beam ,const TRing& input);
  void set_intended_ring_pattern( TGeneralXingShiftInput::beam_color beam,const TRingPattern& input);
  void set_measured_ring_pattern( TGeneralXingShiftInput::beam_color beam,const TRingPattern& input);

  // getters
  // if beam_color is out of range, take it as 0(blue)
  const TRing& get_physics_ring ( TGeneralXingShiftInput::beam_color beam ) const;
  const TRingPattern& get_physics_ring_pattern ( TGeneralXingShiftInput::beam_color beam) const;
  const TRingPattern& get_intended_ring_pattern( TGeneralXingShiftInput::beam_color beam) const;
  const TRingPattern& get_measured_ring_pattern( TGeneralXingShiftInput::beam_color beam) const;

  virtual void clear();

  void set_verbosity(int _verbosity) { verbosity = _verbosity; }
  int get_verbosity() const { return verbosity; }

protected:
  bool check_bound_beam( TGeneralXingShiftInput::beam_color beam ) const;

  TRing physics_ring[2];
  TRingPattern physics_ring_pattern[2];
  TRingPattern intended_ring_pattern[2];
  TRingPattern measured_ring_pattern[2];

  int verbosity;
};

#endif
