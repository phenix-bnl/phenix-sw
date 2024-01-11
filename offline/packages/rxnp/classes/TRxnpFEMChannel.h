#ifndef __TRXNPFEMCHANNEL_H__
#define __TRXNPFEMCHANNEL_H__

// $Id: TRxnpFEMChannel.h,v 1.6 2007/04/19 00:55:57 phnxmuid Exp $
//////////////////////////////////////////////////////////////////
/*
        \file TRxnpFEMChannel.h
        \brief Utility class to store calibration consts info for each FEM channel from database
        \author Chun Zhang
        \version $Revision: 1.6 $
        \date    $Date: 2007/04/19 00:55:57 $
*/
//////////////////////////////////////////////////////////////////

#include <vector>

#include <RXNP.h>

class TRxnpFEMChannel
{
 public:
  enum TXNPBBC {N_ADC= 100, BBC_MAX = 4000};

  //! constructor
  //
  TRxnpFEMChannel(int arm,
	       int ring,
	       int scint):
    _arm(arm),
    _ring(ring),
    _scint(scint)
    {
      for(int iamu = 0; iamu < RXNP::NAMU; iamu++)
        {
          _high_int[iamu]=0;
          _low_int[iamu]=0;
          _tdc_int[iamu]=0;
          _high_wid[iamu]=0;
          _low_wid[iamu]=0;
          _tdc_wid[iamu]=0;
        }
    }

  // copy constructor, needed since object contained by STL container
  //
  TRxnpFEMChannel(const TRxnpFEMChannel& tc);

  // destructor
  //
  virtual ~TRxnpFEMChannel() {;}

  // assaignment operator needed since object contained by STL container.
  //
  TRxnpFEMChannel & operator = ( const TRxnpFEMChannel& tc);

   // public accessor
  //
  // getter
  //
  inline int get_arm() const { return _arm;}
  inline int get_ring() const { return _ring;}
  inline int get_scint() const { return _scint;}

  inline float get_high_slop(int cent) const {return _high_slop[cent];}
  inline float get_high_int( int iamu) const {return _high_int[iamu];}
  inline float get_high_wid( int iamu) const {return _high_wid[iamu];}
  inline float get_low_slop( int cent) const {return _low_slop[cent];}
  inline float get_low_int(  int iamu) const {return _low_int[iamu];}
  inline float get_low_wid(  int iamu) const {return _low_wid[iamu];}
  inline float get_tdc_slop( int cent) const {return _tdc_slop[cent];}
  inline float get_tdc_int(  int iamu) const {return _tdc_int[iamu];}
  inline float get_tdc_wid(  int iamu) const {return _tdc_wid[iamu];}
  inline float get_tdc_lg_slewcoeff(int iadc) {return _tdc_lg_slew_coeff[iadc];}

  // setter
  //
  inline void set_high_slop(float slop) {
    if(slop!=0)
      _high_slop.push_back(1.0/slop);
    else
      _high_slop.push_back(1.0);
  }
  inline void set_low_slop( float slop) {
    if(slop!=0)
      _low_slop.push_back(1.0/slop);
    else
      _low_slop.push_back(1.0);
      
  }
  inline void set_tdc_slop( float slop) {_tdc_slop.push_back(slop);}
  inline void set_high_int( int iamu, float intercept) {_high_int[iamu]  = intercept;}
  inline void set_high_wid( int iamu, float width    ) {_high_wid[iamu]  = width;}
  inline void set_low_int(  int iamu, float intercept) {_low_int[iamu]   = intercept;}
  inline void set_low_wid(  int iamu, float width    ) {_low_wid[iamu]   = width;}
  inline void set_tdc_int(  int iamu, float intercept) {_tdc_int[iamu]   = intercept;}
  inline void set_tdc_wid(  int iamu, float width    ) {_tdc_wid[iamu]   = width;}
  inline void set_tdc_lg_slewcoeff(int iadc, float val) {_tdc_lg_slew_coeff[iadc] = val;}

 private:
  // detector location.
  //
  int _arm;
  int _ring;
  int _scint;

  // Calibration const. Assuming linear relation between adc and energy
  // high/low are high/low gain adc, int represents interception.
  //
  std::vector<float> _high_slop;
  std::vector<float> _low_slop;
  std::vector<float> _tdc_slop;
  
  // pedestal
  float _high_int[RXNP::NAMU];
  float _low_int[RXNP::NAMU];
  float _tdc_int[RXNP::NAMU];
  float _high_wid[RXNP::NAMU];
  float _low_wid[RXNP::NAMU];
  float _tdc_wid[RXNP::NAMU];
  float _tdc_lg_slew_coeff[N_ADC];

};

#endif
