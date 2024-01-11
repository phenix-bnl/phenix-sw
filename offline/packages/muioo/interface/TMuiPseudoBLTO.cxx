#include<TMuiPseudoBLTO.h>
#include<TDataType.h>

ClassImp(TMuiPseudoBLTO)
ClassImp(TMuiPseudoBLTO_v1)

TMuiPseudoBLTO_v1::TMuiPseudoBLTO_v1() :
  _arm(0),
  _index(0),
  _is_2D(false),
  _is_1D1S(false),
  _is_1D_quad0(false),
  _is_1D_quad1(false),
  _is_1D_quad2(false),
  _is_1D_quad3(false),
  _is_1S_quad0(false),
  _is_1S_quad1(false),
  _is_1S_quad2(false),
  _is_1S_quad3(false),
  _is_reco_2D(false),
  _is_reco_1D1S(false),
  _is_reco_1D_quad0(false),
  _is_reco_1D_quad1(false),
  _is_reco_1D_quad2(false),
  _is_reco_1D_quad3(false),
  _is_reco_1S_quad0(false),
  _is_reco_1S_quad1(false),
  _is_reco_1S_quad2(false),
  _is_reco_1S_quad3(false)
{}

TMuiPseudoBLTO_v1::TMuiPseudoBLTO_v1(const Key& key,
  UShort_t arm,
  UShort_t index) :
  TMuiPseudoBLTO(key),
  _arm(arm),
  _index(index),
  _is_2D(false),
  _is_1D1S(false),
  _is_1D_quad0(false),
  _is_1D_quad1(false),
  _is_1D_quad2(false),
  _is_1D_quad3(false),
  _is_1S_quad0(false),
  _is_1S_quad1(false),
  _is_1S_quad2(false),
  _is_1S_quad3(false),
  _is_reco_2D(false),
  _is_reco_1D1S(false),
  _is_reco_1D_quad0(false),
  _is_reco_1D_quad1(false),
  _is_reco_1D_quad2(false),
  _is_reco_1D_quad3(false),
  _is_reco_1S_quad0(false),
  _is_reco_1S_quad1(false),
  _is_reco_1S_quad2(false),
  _is_reco_1S_quad3(false)
{}


TMuiPseudoBLTO_v1::TMuiPseudoBLTO_v1(const TMuiPseudoBLTO* base_ptr) :
  TMuiPseudoBLTO(*base_ptr),
  _arm(base_ptr->get_arm()),
  _index(base_ptr->get_index()),
  _is_2D(base_ptr->is_2D_fired()),
  _is_1D1S(base_ptr->is_1D1S_fired()),
  _is_1D_quad0(base_ptr->is_1D_fired(0)),
  _is_1D_quad1(base_ptr->is_1D_fired(1)),
  _is_1D_quad2(base_ptr->is_1D_fired(2)),
  _is_1D_quad3(base_ptr->is_1D_fired(3)),
  _is_1S_quad0(base_ptr->is_1S_fired(0)),
  _is_1S_quad1(base_ptr->is_1S_fired(1)),
  _is_1S_quad2(base_ptr->is_1S_fired(2)),
  _is_1S_quad3(base_ptr->is_1S_fired(3)),
  _is_reco_2D(base_ptr->is_reco_2D_fired()),
  _is_reco_1D1S(base_ptr->is_reco_1D1S_fired()),
  _is_reco_1D_quad0(base_ptr->is_reco_1D_fired(0)),
  _is_reco_1D_quad1(base_ptr->is_reco_1D_fired(1)),
  _is_reco_1D_quad2(base_ptr->is_reco_1D_fired(2)),
  _is_reco_1D_quad3(base_ptr->is_reco_1D_fired(3)),
  _is_reco_1S_quad0(base_ptr->is_reco_1S_fired(0)),
  _is_reco_1S_quad1(base_ptr->is_reco_1S_fired(1)),
  _is_reco_1S_quad2(base_ptr->is_reco_1S_fired(2)),
  _is_reco_1S_quad3(base_ptr->is_reco_1S_fired(3))
{}

TMuiPseudoBLTO_v1::TMuiPseudoBLTO_v1(const TMuiPseudoBLTO& base_ref) :
  TMuiPseudoBLTO(base_ref),
  _arm(base_ref.get_arm()),
  _index(base_ref.get_index()),
  _is_2D(base_ref.is_2D_fired()),
  _is_1D1S(base_ref.is_1D1S_fired()),
  _is_1D_quad0(base_ref.is_1D_fired(0)),
  _is_1D_quad1(base_ref.is_1D_fired(1)),
  _is_1D_quad2(base_ref.is_1D_fired(2)),
  _is_1D_quad3(base_ref.is_1D_fired(3)),
  _is_1S_quad0(base_ref.is_1S_fired(0)),
  _is_1S_quad1(base_ref.is_1S_fired(1)),
  _is_1S_quad2(base_ref.is_1S_fired(2)),
  _is_1S_quad3(base_ref.is_1S_fired(3)),
  _is_reco_2D(base_ref.is_reco_2D_fired()),
  _is_reco_1D1S(base_ref.is_reco_1D1S_fired()),
  _is_reco_1D_quad0(base_ref.is_reco_1D_fired(0)),
  _is_reco_1D_quad1(base_ref.is_reco_1D_fired(1)),
  _is_reco_1D_quad2(base_ref.is_reco_1D_fired(2)),
  _is_reco_1D_quad3(base_ref.is_reco_1D_fired(3)),
  _is_reco_1S_quad0(base_ref.is_reco_1S_fired(0)),
  _is_reco_1S_quad1(base_ref.is_reco_1S_fired(1)),
  _is_reco_1S_quad2(base_ref.is_reco_1S_fired(2)),
  _is_reco_1S_quad3(base_ref.is_reco_1S_fired(3))
{}

bool TMuiPseudoBLTO_v1::check_2D_trigger() const
{

  // This is a self-consistency check between 1D trigger decision
  // we get from emulator and 2D trigger decision we get from emulator.
  // The ideal is that if one can find two different quadrant both have
  // 1D trigger fired, one should also find 2D trigger fired for this event.
  // Otherwise, SOMEONE SETS US A BOMB !!!
  //
  bool consistency = false;

  for( UShort_t iquad = 0; iquad < 3; iquad++) {
    if(is_1D_fired(iquad)) {
      for(UShort_t jquad = iquad+1; jquad < 4; jquad++) {
	if(is_1D_fired(jquad)) {
	  consistency = true;
	  return consistency;
	}
      }
    }
  }

  return consistency;
}

bool TMuiPseudoBLTO_v1::check_1D1S_trigger() const
{

  // This is a self-consistency check between 1D/1S trigger decision
  // we get from emulator and 1D1S trigger decision we get from emulator.
  // The ideal is that if one can find one quadrant has
  // 1D trigger fired and another quadrant had 1S/1D trigger fired,
  // one should also find 1D1S trigger fired for this event.
  // Otherwise, SOMEONE SETS US AN ATOMIC BOMB !!!
  //
  bool consistency = false;

  for( UShort_t iquad = 0; iquad < 4; iquad++) {
    if(is_1D_fired(iquad)) {
      for(UShort_t jquad = 0; jquad < 4; jquad++) {
	if((is_1D_fired(jquad)||is_1S_fired(jquad))&&(jquad!=iquad)) {
	  consistency = true;
	  return consistency;
	}
      }
    }
  }
  return consistency;
}
