// $Id: TRxnpRawXang.h,v 1.3 2014/01/10 11:47:23 bbannier Exp $

#ifndef _TRxnpRawXang_h_
#define _TRxnpRawXang_h_

/*!
  \file TRxnpRawXang.h
  \ breif high level interface object of TRxnp detector, i.e. reaction plane angles.
  \ author C. Zhang
  \version $Revision: 1.3 $
  \date    $Date: 2014/01/10 11:47:23 $
*/

#include <PHKey.hh>

/*! @ingroup interface */
//! RxnpRawXang object

class TRxnpRawXang : public PHKey
{
 public:
  //! @name Constructors/Destructors
  //@{		
  
  //! Default constructor 
  TRxnpRawXang()
    {}
  //! Constructor with key
  //
  TRxnpRawXang(const Key& key) : PHKey(key)
    {;}
  //! virtual destructor
  //
  virtual ~TRxnpRawXang()
    {;}
  //@}

  //! @name locator
  //@{		
  
  //! Arm [0,1] 
  virtual UShort_t get_arm() const 
    {return 0;}
  
  //! Ring [0,1] 
  virtual UShort_t get_ring() const 
    {return 0;}	
  //! Arm [0,1] 
  virtual void set_arm(UShort_t arm)
    {}

  //! Ring [0,1] 
  virtual void set_ring(UShort_t ring)
    {}							 	  
  //@}

  //! @name functional interface
  //@{	
  
  // getter for angle for dipole moment
  //
  // high gain
  //
  // reaction plane coordinates
  virtual double get_X1_high() const { return 0;}
  virtual double get_Y1_high() const { return 0;}
  // angle atan2(Y1, X1)
  virtual double get_Xangv1_high() const {return 0;}
  // low gain
  // reaction plane coordinates
  virtual double get_X1_low() const { return 0;}
  virtual double get_Y1_low() const { return 0;}
  // angle atan2(Y1, X1)
  virtual double get_Xangv1_low() const {return 0;}
  // getter for angle for quadropole moment 
  //
  // high gain
  // reaction plane coordinates
  virtual double get_X2_high() const { return 0;}
  virtual double get_Y2_high() const { return 0;}
  // angle atan2(Y2, X2)  
  virtual double get_Xangv2_high() const {return 0;}
  // low gain
  // reaction plane coordinates
  virtual double get_X2_low() const { return 0;}
  virtual double get_Y2_low() const { return 0;}
  // angle atan2(Y2, X2)

  virtual double get_Xangv2_low() const {return 0;}  
  // setter for angle for dipole moment
  //
  // high gain
  //
  // reaction plane coordinates
  virtual void set_X1_high(double value) {}
  virtual void set_Y1_high(double value) {}
  // angle atan2(Y1, X1)  
  virtual void set_Xangv1_high(double value) {;}
  // low gain
  //
  // reaction plane coordinates
  virtual void set_X1_low(double value) {}
  virtual void set_Y1_low(double value) {}
  // angle atan2(Y1, X1)  
  virtual void set_Xangv1_low(double value) {;}
  // setter for angle for quadropole moment 
  //
  // high gain
  //
  // reaction plane coordinates
  virtual void set_X2_high(double value) {}
  virtual void set_Y2_high(double value) {}
  // angle atan2(Y2, X2)  
  virtual void set_Xangv2_high(double value) {;}
  // low gain
  //
  // reaction plane coordinates
  virtual void set_X2_low(double value) {}
  virtual void set_Y2_low(double value) {}
  // angle atan2(Y2, X2)  
  virtual void set_Xangv2_low(double value) {;}

  // get eta for this ring
  //
  virtual double get_eta() const { return 0;}
  // setter for eta
  //
  virtual void set_eta(double value) {;}
  //@}
};

#endif // end ifndef
