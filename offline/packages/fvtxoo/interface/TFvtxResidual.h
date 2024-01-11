// Class : TFvtxResidual
// Author: D. Winter
// Date: 4/27/08
//
// Description: Holds a residual for FVTX tracks.  In order to avoid
// adding members and bookkeeping to the TFvtx*TrkPar objects, we make this
// class a PHKey.  That way we can transparently associate the result
// with the track.

#ifndef __TFVTXRESIDUAL_H__
#define __TFVTXRESIDUAL_H__

#include <PHKey.hh>
#include <FVTXOO.h>

class TFvtxResidual : public PHKey
{
public:

  //! Default constructor
  TFvtxResidual() {} 

  /*! Construct with key */
  TFvtxResidual(const Key& key) {} 
  
  virtual ~TFvtxResidual() {}

  /*! Index [0,1024] */
  virtual unsigned short get_index() const { return 0; }
  
  virtual unsigned short get_arm() const { return 0; }

  //! Get Z of residual
  virtual double get_z() const { return 0; } 

  //! Get R residual
  virtual double get_dr() const { return 0; }

  //! Get residual in phi direction
  virtual double get_dphi() const { return 0; }

  virtual void set_arm(const unsigned short arm) {}

  //! Set Z of residual
  virtual void set_z(const double z) {} 

  //! Set R residual
  virtual void set_dr(const double dr) {}

  //! Set residual in phi direction
  virtual void set_dphi(const double dphi) {}

  //! dump object content to stream
  virtual void print( std::ostream &out = std::cout ) const {}

protected:
  ClassDef(TFvtxResidual,1)
};

#endif
