// $Id: TRxnpRawScint_v1.cxx,v 1.5 2007/03/08 23:11:20 zhangc Exp $

/*!
        \file TRxnpRawScint_v1.cxx
        \brief interface object for each Rxnp scintilator
        \author C. Zhang
  \version $Revision: 1.5 $
  \date    $Date: 2007/03/08 23:11:20 $
*/

#include <cmath>

#include "TRxnpRawScint_v1.h"
ClassImp(TRxnpRawScint_v1)

//___________________________________________________________________________________________

using namespace std;

// Default constructor
//

TRxnpRawScint_v1::TRxnpRawScint_v1() :
  _arm(0),
  _ring(0),
  _scint(0),
  _chanid(0),
  _high_pre(0),
  _high_pre_error(0),
  _high_post(0),
  _high_post_error(0),
  _low_pre(0),
  _low_pre_error(0),
  _low_post(0),
  _low_post_error(0),
  _tdc(0),
  _tdc_error(0),
  _amu_pre(0),
  _amu_post(0),
  _amu_tdc(0),
  _status(0),
  _phi(0),
  _theta(0) {;}

// contructor with locations
//

TRxnpRawScint_v1::TRxnpRawScint_v1(const Key& key,
				   UShort_t arm,
				   UShort_t ring,
				   UShort_t scint) :
  TRxnpRawScint(key),
  _arm(arm),
  _ring(ring),
  _scint(scint),
  _chanid(0),
  _high_pre(0),
  _high_pre_error(0),
  _high_post(0),
  _high_post_error(0),
  _low_pre(0),
  _low_pre_error(0),
  _low_post(0),
  _low_post_error(0),
  _tdc(0),
  _tdc_error(0),
  _amu_pre(0),
  _amu_post(0),
  _amu_tdc(0),
  _status(0),
  _phi(0),
  _theta(0) {;}

// construct from pointer of base class
//

TRxnpRawScint_v1::TRxnpRawScint_v1( const TRxnpRawScint* base_ptr) :
  TRxnpRawScint(*base_ptr),
  _arm(base_ptr->get_arm()),
  _ring(base_ptr->get_ring()),
  _scint(base_ptr->get_scint()),
  _chanid(base_ptr->get_chanid()),
  _high_pre(base_ptr->get_high_pre()),
  _high_pre_error(base_ptr->get_high_pre_error()),
  _high_post(base_ptr->get_high_post()),
  _high_post_error(base_ptr->get_high_post_error()),
  _low_pre(base_ptr->get_low_pre()),
  _low_pre_error(base_ptr->get_low_pre_error()),
  _low_post(base_ptr->get_low_post()),
  _low_post_error(base_ptr->get_low_post_error()),
  _tdc(base_ptr->get_tdc()),
  _tdc_error(base_ptr->get_tdc_error()),
  _amu_pre(base_ptr->get_amu_pre()),
  _amu_post(base_ptr->get_amu_post()),
  _amu_tdc(base_ptr->get_amu_tdc()),
  _status(base_ptr->get_status()),
  _phi(base_ptr->get_phi()),
  _theta(base_ptr->get_theta()){;}

// construct from base reference
//

TRxnpRawScint_v1::TRxnpRawScint_v1( const TRxnpRawScint& base_ref) :
  TRxnpRawScint(base_ref),
  _arm(base_ref.get_arm()),
  _ring(base_ref.get_ring()),
  _scint(base_ref.get_scint()),
  _chanid(base_ref.get_chanid()),
  _high_pre(base_ref.get_high_pre()),
  _high_pre_error(base_ref.get_high_pre_error()),
  _high_post(base_ref.get_high_post()),
  _high_post_error(base_ref.get_high_post_error()),
  _low_pre(base_ref.get_low_pre()),
  _low_pre_error(base_ref.get_low_pre_error()),
  _low_post(base_ref.get_low_post()),
  _low_post_error(base_ref.get_low_post_error()),
  _tdc(base_ref.get_tdc()),
  _tdc_error(base_ref.get_tdc_error()),
  _amu_pre(base_ref.get_amu_pre()),
  _amu_post(base_ref.get_amu_post()),
  _amu_tdc(base_ref.get_amu_tdc()),
  _status(base_ref.get_status()),
  _phi(base_ref.get_phi()),
  _theta(base_ref.get_theta()){;}

Float_t TRxnpRawScint_v1::get_eta() const 
{

  return -1.0*log(tan(_theta/2.0));

}

void TRxnpRawScint_v1::print(std::ostream& os) const
{
  os<< " ***************** Dump informations about this scintilator ************************* " << endl;
  get_key().print(os);
  os<< " Location of the scintilator : ";
  os<< " arm = " << _arm << ";  ring = " << _ring << "; scint = " << _scint << endl;
  os<< " chanid = " << _chanid << endl;
  os<< " High gain values : pre = " << _high_pre << ";  post = " << _high_post << endl; 
  os<< " Low gain values : pre = " << _high_pre << ";  post = " << _high_post << endl; 
  os<< " Tdc = " << _tdc << endl;
  os<< " Amu cell number: amu(tdc) = " << _amu_tdc << "; amu(pre) = "<< _amu_pre << "; amu(post) = " << _amu_post << endl; 
  os<< " *********************************************************************************** " << endl;
}
  
