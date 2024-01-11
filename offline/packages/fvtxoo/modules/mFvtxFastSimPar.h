// $Id: mFvtxFastSimPar.h,v 1.8 2014/01/26 16:47:16 bbannier Exp $
#ifndef __mFvtxFastSimPar_h__
#define __mFvtxFastSimPar_h__

/*!
	\file mFvtxFastSimPar.h
	\brief Runtime parameter object for mFvtxFastSim analysis module
	\author H. Pereira Da Costa
	\version $Revision: 1.8 $
	\date $Date: 2014/01/26 16:47:16 $
*/

#include <PHObject.h>
#include <PHException.h>
//#include <boost/array.hpp>
#include <iostream>

#include "TFvtxParBase.h"
#include "FVTXOO.h"

/*! @ingroup modules */
//! Runtime parameter object for mFvtxFastSim analysis module
class mFvtxFastSimPar : public TFvtxParBase
{
	
	public: 

	/*! default constructor */
	mFvtxFastSimPar():
		_do_clusters( true )
	{}
	
	/*! destructor */
	virtual ~mFvtxFastSimPar()
	{}
	
	//! PHOOL inteface requirement
	void identify(std::ostream& os = std::cout) const 
	{ os << "mFvtxFastSimPar";}
	
	//! number of parameters
	enum { n_acceptance_parameters = FVTXOO::MAX_ARM*FVTXOO::MAX_STATION };

	//! \brief return unique index for arm/station
	static int get_acceptance_index( int arm, int station )
	{ return station + FVTXOO::MAX_STATION*arm; }

	//! clustering 
	void set_do_clusters( const bool& value )
	{ _do_clusters = value; }
	
	//! clustering
	const bool& get_do_clusters( void ) const
	{ return _do_clusters; }

        //! printing (insert values of all parameters here
        void print(std::ostream& out = std::cout) const {
          FVTXOO::PRINT(out, "mFvtxFastSimPar");
          out << "_verbosity: " << get_verbosity() << std::endl;
          out << "_do_clusters: " << _do_clusters << std::endl;
          FVTXOO::PRINT(out, "**");
        }

        private:
			
	//! create clusters
	bool _do_clusters;	
	
	ClassDef(mFvtxFastSimPar, 1);
};

#endif
