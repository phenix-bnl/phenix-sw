// Class : TFvtxResidual_v1
// Author: D. Winter
// Date: 4/27/08
//
// Description: Implementation of v1 of TFvtxResidual
//

#ifndef __TFVTXRESIDUAL_V1_H__
#define __TFVTXRESIDUAL_V1_H__

#include <TFvtxResidual.h>

class TFvtxResidual_v1 : public TFvtxResidual
{
public:

  //! Default constructor
  TFvtxResidual_v1() {} 

  /*! Construct with key */
  TFvtxResidual_v1(const Key& key, const unsigned short arm, const unsigned short index) : 
    TFvtxResidual(key),
    _index(index),
    _arm(arm),
    _z(0.0),
    _dr(0.0),
    _dphi(0.0)
  {} 
  
  //! constructor
  TFvtxResidual_v1(const TFvtxResidual* ptr) :
    TFvtxResidual(*ptr)
  {
  }

  //! constructor
  TFvtxResidual_v1(const TFvtxResidual& ref) :
    TFvtxResidual(ref)
  {}

  virtual ~TFvtxResidual_v1() {}

  /*! Index [0,1024] */
  virtual unsigned short get_index() const { return _index; }

  virtual unsigned short get_arm() const { return _arm; }
  
  //! Get Z of residual
  virtual double get_z() const { return _z; } 

  //! Get R residual
  virtual double get_dr() const { return _dr; }

  //! Get residual in phi direction
  virtual double get_dphi() const { return _dphi; }

  virtual void set_arm(const unsigned short arm) {_arm = arm; }

  //! Set Z of residual
  virtual void set_z(const double z) { _z = z; } 

  //! Set R residual
  virtual void set_dr(const double dr) { _dr = dr; }

  //! Set residual in phi direction
  virtual void set_dphi(const double dphi) { _dphi = dphi; }

  //! dump object content to stream
  virtual void print( std::ostream &out = std::cout ) const {}

protected:
  unsigned short _index;
  unsigned short _arm;
  double _z;
  double _dr;
  double _dphi;

  ClassDef(TFvtxResidual_v1,1)
};

#endif
