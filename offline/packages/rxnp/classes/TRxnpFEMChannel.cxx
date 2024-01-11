// $Id: TRxnpFEMChannel.cxx,v 1.4 2007/03/14 20:33:30 esumi Exp $
//////////////////////////////////////////////////////////////////
/*
        \file TRxnpFEMChannel.cxx
        \brief Implementation of utility class to store calibration consts info for each FEM channel from database
        \author Chun Zhang
        \version $Revision: 1.4 $
        \date    $Date: 2007/03/14 20:33:30 $
*/
//////////////////////////////////////////////////////////////////

#include <algorithm>

#include "TRxnpFEMChannel.h"

// copy constructor
//

TRxnpFEMChannel::TRxnpFEMChannel(const TRxnpFEMChannel& tc)
{
  _arm = tc._arm;
  _ring = tc._ring;
  _scint = tc._scint;

  std::copy(tc._high_slop.begin(), (tc._high_slop).end(), _high_slop.begin());
  std::copy(tc._low_slop.begin(), (tc._low_slop).end(), _low_slop.begin());
  std::copy(tc._tdc_slop.begin(), (tc._tdc_slop).end(), _tdc_slop.begin());

  std::copy(tc._high_int, (tc._high_int)+RXNP::NAMU, _high_int);
  std::copy(tc._low_int, (tc._low_int)+RXNP::NAMU, _low_int);
  std::copy(tc._tdc_int, (tc._tdc_int)+RXNP::NAMU, _tdc_int);

  std::copy(tc._high_wid, (tc._high_wid)+RXNP::NAMU, _high_wid);
  std::copy(tc._low_wid, (tc._low_wid)+RXNP::NAMU, _low_wid);
  std::copy(tc._tdc_wid, (tc._tdc_wid)+RXNP::NAMU, _tdc_wid);

}

TRxnpFEMChannel& TRxnpFEMChannel::operator=(const TRxnpFEMChannel& tc)
{
  if(this!=&tc) 
    {
      _arm = tc._arm;
      _ring = tc._ring;
      _scint = tc._scint;
      
      std::copy(tc._high_slop.begin(), (tc._high_slop).end(), _high_slop.begin());
      std::copy(tc._low_slop.begin(), (tc._low_slop).end(), _low_slop.begin());
      std::copy(tc._tdc_slop.begin(), (tc._tdc_slop).end(), _tdc_slop.begin());
      
      std::copy(tc._high_int, (tc._high_int)+RXNP::NAMU, _high_int);
      std::copy(tc._low_int, (tc._low_int)+RXNP::NAMU, _low_int);
      std::copy(tc._tdc_int, (tc._tdc_int)+RXNP::NAMU, _tdc_int);
      
      std::copy(tc._high_wid, (tc._high_wid)+RXNP::NAMU, _high_wid);
      std::copy(tc._low_wid, (tc._low_wid)+RXNP::NAMU, _low_wid);
      std::copy(tc._tdc_wid, (tc._tdc_wid)+RXNP::NAMU, _tdc_wid);
    }

  return *this;
}
