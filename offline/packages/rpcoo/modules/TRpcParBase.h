// $Id: TRpcParBase.h,v 1.2 2008/08/28 00:53:57 kempel Exp $

#ifndef __TRpcParBase_h__
#define __TRpcParBase_h__

/*!
  \file    TRpcParBase.h
  \brief   Base class for runtime parameter objects. 
  \author  H. Pereira Da Costa
  \version $Revision: 1.2 $
  \date    $Date: 2008/08/28 00:53:57 $
*/

#include <iostream>
#include <PHObject.h>
#include <RPCOO.h>

//! Base class for runtime parameter objects.  
/*! 
  Satisfies PHOOL interface requirements and provides interface
  to run-time parameters that are common to all modules.
*/

class TRpcParBase : public PHObject
{

 public: 

  /*! default constructor */
  TRpcParBase() : 
    _verbosity(RPCOO::NONE){;}
  
  /*! destructor */
  virtual ~TRpcParBase(){;}

  /*! PHOOL inteface requirement */
  virtual void identify(std::ostream& os = std::cout) const {
    os << "TRpcParBase";
  }

  /*! PHOOL interface requirement -- null op */
  virtual void Reset(){;}

  /*! PHOOL interface requirement -- always returns 1 */
  virtual int isValid() const 
	{return 1;}

  /*! Verbosity level */
  RPCOO::Verbosity get_verbosity() const 
	{return _verbosity;}   

  /*! Verbosity level */
  void set_verbosity(RPCOO::Verbosity verbosity) 
	{_verbosity = verbosity;}
	
	//! printing (insert values of all parameters here
	virtual void print( std::ostream &out = std::cout ) const
	{}

 protected:  

	//! module verbosity level     
  RPCOO::Verbosity _verbosity;

};

#endif
