// $Id: TFvtxParBase.h,v 1.3 2014/01/26 16:47:16 bbannier Exp $

#ifndef __TFvtxParBase_h__
#define __TFvtxParBase_h__

/*!
	\file TFvtxParBase.h
	\brief Base class for runtime parameter objects. 
	\author H. Pereira Da Costa
	\version $Revision: 1.3 $
	\date $Date: 2014/01/26 16:47:16 $
*/

#include <iostream>
#include <PHObject.h>
#include <FVTXOO.h>
#include <TMutNode.h> //used for root cint dictionary generation

//! Base class for runtime parameter objects.	
/*! 
	Satisfies PHOOL interface requirements and provides interface
	to run-time parameters that are common to all modules.
*/

class TFvtxParBase : public PHObject
{

 public: 

	/*! default constructor */
	TFvtxParBase() : 
		_verbosity(FVTXOO::NONE){;}
	
	/*! destructor */
	virtual ~TFvtxParBase(){;}

	/*! PHOOL inteface requirement */
	virtual void identify(std::ostream& os = std::cout) const 
	{
		os << "TFvtxParBase";
	}

	/*! PHOOL interface requirement -- null op */
	virtual void Reset(){;}

	/*! PHOOL interface requirement -- always returns 1 */
	virtual int isValid() const 
	{return 1;}

	/*! Verbosity level */
	FVTXOO::Verbosity get_verbosity() const 
	{return _verbosity;}	 

	/*! Verbosity level */
	void set_verbosity(FVTXOO::Verbosity verbosity) 
	{_verbosity = verbosity;}

        //! printing (insert values of all parameters here
        virtual void print(std::ostream& out = std::cout) const {}

 protected:	

	//! module verbosity level		 
	FVTXOO::Verbosity _verbosity;

	ClassDef(TFvtxParBase, 1);
};

#endif
