// $Id: TRxnpScint_v1.cxx,v 1.4 2006/12/08 20:21:36 zhangc Exp $

/*!
        \file TRxnpScint_v1.cxx
        \brief interface object for each Rxnp scintilator
        \author C. Zhang
  \version $Revision: 1.4 $
  \date    $Date: 2006/12/08 20:21:36 $
*/

#include <cmath>

#include "TRxnpScint_v1.h"
ClassImp(TRxnpScint_v1)

//___________________________________________________________________________________________

using namespace std;

// Default constructor
//

TRxnpScint_v1::TRxnpScint_v1() :
  _arm(0),
  _ring(0),
  _scint(0),
  _tof(0),
  _tof_error(0),
  _high_e(0),
  _high_e_error(0),
  _low_e(0),
  _low_e_error(0),
  _high_nhits(0),
  _high_nhits_error(0),
  _low_nhits(0),
  _low_nhits_error(0),
  _status(0),
  _phi(0),
  _theta(0) {;}

// contructor with locations
//

TRxnpScint_v1::TRxnpScint_v1(const Key& key,
			     UShort_t arm,
			     UShort_t ring,
			     UShort_t scint) :
  TRxnpScint(key),
  _arm(arm),
  _ring(ring),
  _scint(scint),
  _tof(0),
  _tof_error(0),
  _high_e(0),
  _high_e_error(0),
  _low_e(0),
  _low_e_error(0),
  _high_nhits(0),
  _high_nhits_error(0),
  _low_nhits(0),
  _low_nhits_error(0),
  _status(0),
  _phi(0),
  _theta(0) {;}

// construct from pointer of base class
//

TRxnpScint_v1::TRxnpScint_v1( const TRxnpScint* base_ptr) :
  TRxnpScint(*base_ptr),
  _arm(base_ptr->get_arm()),
  _ring(base_ptr->get_ring()),
  _scint(base_ptr->get_scint()),
  _tof(base_ptr->get_tof()),
  _tof_error(base_ptr->get_tof_error()),
  _high_e(base_ptr->get_high_e()),
  _high_e_error(base_ptr->get_high_e_error()),
  _low_e(base_ptr->get_low_e()),
  _low_e_error(base_ptr->get_low_e_error()),
  _high_nhits(base_ptr->get_high_nhits()),
  _high_nhits_error(base_ptr->get_high_nhits_error()),
  _low_nhits(base_ptr->get_low_nhits()),
  _low_nhits_error(base_ptr->get_low_nhits_error()),
  _status(base_ptr->get_status()),
  _phi(base_ptr->get_phi()),
  _theta(base_ptr->get_theta()){;}

// construct from base reference
//

TRxnpScint_v1::TRxnpScint_v1( const TRxnpScint& base_ref) :
  TRxnpScint(base_ref),
  _arm(base_ref.get_arm()),
  _ring(base_ref.get_ring()),
  _scint(base_ref.get_scint()),
  _tof(base_ref.get_tof()),
  _tof_error(base_ref.get_tof_error()),
  _high_e(base_ref.get_high_e()),
  _high_e_error(base_ref.get_high_e_error()),
  _low_e(base_ref.get_low_e()),
  _low_e_error(base_ref.get_low_e_error()),
  _high_nhits(base_ref.get_high_nhits()),
  _high_nhits_error(base_ref.get_high_nhits_error()),
  _low_nhits(base_ref.get_low_nhits()),
  _low_nhits_error(base_ref.get_low_nhits_error()),
  _status(base_ref.get_status()),
  _phi(base_ref.get_phi()),
  _theta(base_ref.get_theta()){;}

Float_t TRxnpScint_v1::get_eta() const 
{

  return -1.0*log(tan(_theta/2.0));

}

void TRxnpScint_v1::print(std::ostream& os) const
{
  os<< " ***************** Dump informations about this scintilator ************************* " << endl;
  os<< " Location of the scintilator : ";
  os<< " arm = " << _arm << ";  ring = " << _ring << "; scint = " << _scint << endl;
  os<< " Energy : " << " high e = " << _high_e << ";  low e = " << _low_e << endl; 
  os<< " Nhits: " << " high nhits = " << _high_nhits << ";  low nhits = " << _low_nhits << endl; 
  os<< " Tof = " << _tof << endl;
  os<< " *********************************************************************************** " << endl;
}
  
