// $Id: TRxnpRawXang_v1.cxx,v 1.2 2006/12/08 20:21:36 zhangc Exp $

/*!
  \file TRxnpRawXang_v1.cxx
  \ breif high level interface object of TRxnp detector, i.e. reaction plane angles.
  \ author C. Zhang
  \version $Revision: 1.2 $
  \date    $Date: 2006/12/08 20:21:36 $
*/

#include <cmath>
#include "TRxnpRawXang_v1.h"
ClassImp(TRxnpRawXang_v1)

//_________________________________________________________________________

using namespace std;

// Default constructor
//

TRxnpRawXang_v1::TRxnpRawXang_v1() :
  _arm(0),
  _ring(0),
  _X1_high(0),
  _Y1_high(0),
  _Xangv1_high(0),
  _X1_low(0),
  _Y1_low(0),
  _Xangv1_low(0),
  _X2_high(0),
  _Y2_high(0),
  _Xangv2_high(0),
  _X2_low(0),
  _Y2_low(0),
  _Xangv2_low(0),
  _eta(0){;}

//! Constructor with key and location
//
TRxnpRawXang_v1::TRxnpRawXang_v1(const Key& key, 
				 UShort_t  arm, 
				 UShort_t ring):
  TRxnpRawXang(key),
  _arm(arm),
  _ring(ring),
   _X1_high(0),
  _Y1_high(0),
  _Xangv1_high(0),
  _X1_low(0),
  _Y1_low(0),
  _Xangv1_low(0),
  _X2_high(0),
  _Y2_high(0),
  _Xangv2_high(0),
  _X2_low(0),
  _Y2_low(0),
  _Xangv2_low(0),
  _eta(0){;}

//! Construct from pointer of base class
TRxnpRawXang_v1::TRxnpRawXang_v1(const TRxnpRawXang* base_ptr):
  TRxnpRawXang(*base_ptr),
  _arm(base_ptr->get_arm()),
  _ring(base_ptr->get_ring()),
  _X1_high(base_ptr->get_X1_high()),
  _Y1_high(base_ptr->get_Y1_high()),
  _Xangv1_high(base_ptr->get_Xangv1_high()),
  _X1_low(base_ptr->get_X1_low()),
  _Y1_low(base_ptr->get_Y1_low()),
  _Xangv1_low(base_ptr->get_Xangv1_low()),
  _X2_high(base_ptr->get_X2_high()),
  _Y2_high(base_ptr->get_Y2_high()),
  _Xangv2_high(base_ptr->get_Xangv2_high()),
  _X2_low(base_ptr->get_X2_low()),
  _Y2_low(base_ptr->get_Y2_low()),
  _Xangv2_low(base_ptr->get_Xangv2_low()),
  _eta(base_ptr->get_eta()){;}

//! Construct from reference of base class
TRxnpRawXang_v1::TRxnpRawXang_v1(const TRxnpRawXang& base_ref):
  TRxnpRawXang(base_ref),
  _arm(base_ref.get_arm()),
  _ring(base_ref.get_ring()),
  _X1_high(base_ref.get_X1_high()),
  _Y1_high(base_ref.get_Y1_high()),
  _Xangv1_high(base_ref.get_Xangv1_high()),
  _X1_low(base_ref.get_X1_low()),
  _Y1_low(base_ref.get_Y1_low()),
  _Xangv1_low(base_ref.get_Xangv1_low()),
  _X2_high(base_ref.get_X2_high()),
  _Y2_high(base_ref.get_Y2_high()),
  _Xangv2_high(base_ref.get_Xangv2_high()),
  _X2_low(base_ref.get_X2_low()),
  _Y2_low(base_ref.get_Y2_low()),
  _Xangv2_low(base_ref.get_Xangv2_low()),
  _eta(base_ref.get_eta()){;}

void TRxnpRawXang_v1::print(std::ostream& os) const
{
  os<< " ***************** Dump xang about this ring ************************* " << std::endl;
  os<< " Location of the ring : ";
  os<< " arm = " << _arm << ";  ring = " << _ring << std::endl;
  os<< " high gain diploe angle  = : " << _Xangv1_high << ";  low gain dipole angle = " << _Xangv1_low << endl; 
  os<< " high gain quadroploe angle  = : " << _Xangv2_high << ";  low gain quadropole angle = " << _Xangv2_low << endl; 
  os<< " eta = " << _eta << endl;
  os<< " *********************************************************************************** " << endl;

}

 
  
