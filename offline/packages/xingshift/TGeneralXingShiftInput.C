#include "TGeneralXingShiftInput.h"
#include<iostream>
#include<cstdio>

using namespace std;

TGeneralXingShiftInput::TGeneralXingShiftInput():
  verbosity(10)
{
}

TGeneralXingShiftInput::~TGeneralXingShiftInput()
{
}

void TGeneralXingShiftInput::set_physics_ring(beam_color beam,const TRing& input)
{
  if ( ! check_bound_beam(beam) ) return;
  physics_ring[beam] = input;
  physics_ring_pattern[beam] = physics_ring[beam].get_ring_pattern();
}

void TGeneralXingShiftInput::set_intended_ring_pattern(beam_color beam,const TRingPattern& input)
{
  if ( ! check_bound_beam(beam) ) return;
  intended_ring_pattern[beam] = input;
}

void TGeneralXingShiftInput::set_measured_ring_pattern(beam_color beam,const TRingPattern& input)
{
  if ( ! check_bound_beam(beam) ) return;
  measured_ring_pattern[beam] = input;
}

const TRing& TGeneralXingShiftInput::get_physics_ring( TGeneralXingShiftInput::beam_color beam ) const
{
  if ( ! check_bound_beam(beam) ) return physics_ring[0];
  return physics_ring[beam];
}

const TRingPattern& TGeneralXingShiftInput::get_physics_ring_pattern( TGeneralXingShiftInput::beam_color beam ) const
{
  if ( ! check_bound_beam(beam) ) return physics_ring_pattern[0];
  return physics_ring_pattern[beam];
}

const TRingPattern& TGeneralXingShiftInput::get_intended_ring_pattern( TGeneralXingShiftInput::beam_color beam) const
{
  if ( ! check_bound_beam(beam) ) return intended_ring_pattern[0];
  return intended_ring_pattern[beam];
}

const TRingPattern& TGeneralXingShiftInput::get_measured_ring_pattern( TGeneralXingShiftInput::beam_color beam) const
{
  if ( ! check_bound_beam(beam) ) return measured_ring_pattern[0];
  return measured_ring_pattern[beam];
}

bool TGeneralXingShiftInput::check_bound_beam( TGeneralXingShiftInput::beam_color beam ) const
{
  if ( beam == 0 || beam == 1 ) return true;
  if ( verbosity > 1 ) cout << " TGeneralXingShiftInput::beam_color out of range" << endl;
  return false;
}

void TGeneralXingShiftInput::clear()
{
  for( int i = 0;i < 2; i++){
    physics_ring[i].clear();
    physics_ring_pattern[i].clear();
    intended_ring_pattern[i].clear();
    measured_ring_pattern[i].clear();
  }
}
