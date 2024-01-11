// $Id: TRxnpRawXang_v1.h,v 1.2 2006/12/08 20:21:36 zhangc Exp $

#ifndef _TRxnpRawXang_v1_h_
#define _TRxnpRawXang_v1_h_

/*!
  \file TRxnpRawXang_v1.h
  \ breif high level interface object of TRxnp detector, i.e. reaction plane angles.
  \ author C. Zhang
  \version $Revision: 1.2 $
  \date    $Date: 2006/12/08 20:21:36 $
*/

#include <PHKey.hh>

#include "TRxnpRawXang.h"

/*! @ingroup interface */
//! Rxnp object

class TRxnpRawXang_v1 : public TRxnpRawXang
{
 public:
  //! @name Constructors/Destructors
  //@{		
  
  //! Default constructor 
  TRxnpRawXang_v1();

  //! Constructor with key and location
  //
  TRxnpRawXang_v1(const Key& key, UShort_t  arm, UShort_t ring);
  //! construct from pointer of base class
  //
  TRxnpRawXang_v1(const TRxnpRawXang* base_ptr);
  //! construct from pointer of base class
  //
  TRxnpRawXang_v1(const TRxnpRawXang& base_ref);

  //! virtual destructor
  //
  virtual ~TRxnpRawXang_v1()
    {;}
  //@}
  
  //! @name locator
  //@{		
  
  //! Arm [0,1] 
  virtual UShort_t get_arm() const 
    {return _arm;}
  
  //! Ring [0,1] 
  virtual UShort_t get_ring() const 
    {return _ring;}	
  //! Arm [0,1] 
  virtual void set_arm(UShort_t arm)
    {_arm = arm;}

  //! Ring [0,1] 
  virtual void set_ring(UShort_t ring)
    {_ring = ring;}							 	  
  //@}
  
  //! @name functional interface
  //@{	
  
  // getter for angle for dipole moment
  //
  // high gain
  //
  // reaction plane coordinates
  virtual double get_X1_high() const { return _X1_high;}
  virtual double get_Y1_high() const { return _Y1_high;}
  // angle atan2(Y1, X1)
  virtual double get_Xangv1_high() const {return _Xangv1_high;}
  // high gain
  //
  // reaction plane coordinates
  virtual double get_X1_low() const { return _X1_low;}
  virtual double get_Y1_low() const { return _Y1_low;}
  // angle atan2(Y1, X1)
  virtual double get_Xangv1_low() const {return _Xangv1_low;}

  // getter for angle for quadropole moment 
  //
  // high gain
  //
  // reaction plane coordinates
  virtual double get_X2_high() const { return _X2_high;}
  virtual double get_Y2_high() const { return _Y2_high;}
  // angle atan2(Y2, X2)
  virtual double get_Xangv2_high() const {return _Xangv2_high;}
  // low gain
  //
  // reaction plane coordinates
  virtual double get_X2_low() const { return _X2_low;}
  virtual double get_Y2_low() const { return _Y2_low;}
  // angle atan2(Y2, X2)
  virtual double get_Xangv2_low() const {return _Xangv2_low;}

  // setter for angle for dipole moment 
  //
  // high gain
  //
  // reaction plane coordinates
  virtual void set_X1_high(double value)  {  _X1_high = value;}
  virtual void set_Y1_high(double value)  {  _Y1_high = value;}
  // angle atan2(Y1, X1)
  virtual void set_Xangv1_high(double value)  { _Xangv1_high = value ;}
  // high gain
  //
  // reaction plane coordinates
  virtual void set_X1_low(double value)  {  _X1_low = value ;}
  virtual void set_Y1_low(double value)  {  _Y1_low = value;}
  // angle atan2(Y1, X1)
  virtual void set_Xangv1_low(double value)  { _Xangv1_low = value;}

  // setter for angle for quadropole moment 
  //
  // high gain
  //
  // reaction plane coordinates
  virtual void set_X2_high(double value)  {  _X2_high = value;}
  virtual void set_Y2_high(double value)  {  _Y2_high = value;}
  // angle atan2(Y2, X2)
  virtual void set_Xangv2_high(double value)  { _Xangv2_high = value ;}
  // high gain
  //
  // reaction plane coordinates
  virtual void set_X2_low(double value)  {  _X2_low = value ;}
  virtual void set_Y2_low(double value)  {  _Y2_low = value;}
  // angle atan2(Y2, X2)
  virtual void set_Xangv2_low(double value)  { _Xangv2_low = value;}

  // get eta for this ring
  //
  virtual double get_eta() const { return  _eta;}
  // setter for eta
  //
  virtual void set_eta(double value) { _eta = value;}
  //@}
  // dump out object contents
  //
  virtual void print(std::ostream& os = std::cout) const;

 private:

  UShort_t _arm;
  UShort_t _ring;

  double _X1_high;
  double _Y1_high;
  double _Xangv1_high;
  double _X1_low;
  double _Y1_low;
  double _Xangv1_low;
  double _X2_high;
  double _Y2_high;
  double _Xangv2_high;
  double _X2_low;
  double _Y2_low;
  double _Xangv2_low;
  double _eta;

  //! ROOT dictionary
  //
  ClassDef(TRxnpRawXang_v1, 1);
};

#endif // end ifndef
