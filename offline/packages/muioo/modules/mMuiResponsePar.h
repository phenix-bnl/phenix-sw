#ifndef __MMUIRESPONSEPAR_HH__
#define __MMUIRESPONSEPAR_HH__

#include<packetConstants.h>
#include<TMuiParBase.h>
#include<TDataType.h>
#include<PHException.h>
#include<MUIOO.h>

//! Runtime parameter object for mMuiResponse analysis module.
/*! 
Runtime parameter object for mMuiResponse analysis module
*/
class mMuiResponsePar : public TMuiParBase
{  
 public: 

  /*! default constructor */
  mMuiResponsePar() :
    _twopack_eff(1.0),
    _use_hv_mask(true)
{;}
  
  /*! destructor */
  ~mMuiResponsePar(){;}
  
  /*! set an efficiency for twopack   */
  void set_twopack_eff( double twopack_eff){ _twopack_eff = twopack_eff; }

  /*! get twopack efficiency */
  double get_twopack_eff() const { return _twopack_eff; }
  
  /*! enable the use of the hv mask in the response */
  void set_use_hv_mask( bool use_hv_mask){ _use_hv_mask = use_hv_mask; }
  
  /*! enable the use of the hv mask in the response */
  bool get_use_hv_mask() const { return _use_hv_mask;}
  
 private:  
  double _twopack_eff; 
  bool _use_hv_mask;
};

#endif /* __MMUIRESPONSEPAR_HH__ */







