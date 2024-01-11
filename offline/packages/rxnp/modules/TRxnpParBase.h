#ifndef __TRXNPPARBASE_HH__
#define __TRXNPPARBASE_HH__

#include <iostream>
#include <PHObject.h>
#include <MUTOO.h>
#include <TMutParameterDB.h>

//! Base class for runtime parameter objects.	
/*! 
	Satisfies PHOOL interface requirements and provides interface
	to run-time parameters that are common to all modules.
*/

class TRxnpParBase : public PHObject
{

	public: 

	/*! default constructor */
	TRxnpParBase() : _verbosity(MUTOO::NONE)
	{	
		TMutParameterDB::get().get<UShort_t>("TMutParBase_verbosity", _verbosity);
	}
	
	/*! destructor */
	virtual ~TRxnpParBase(){;}

	/*! PHOOL inteface requirement */
  virtual void identify(std::ostream& os = std::cout) const {os << "TRxnpParBasePar";}

	/*! PHOOL interface requirement -- null op */
	virtual void Reset(){;}

	/*! PHOOL interface requirement -- always returns 1 */
	virtual int isValid() const {return 1;}

	/*! Verbosity level */
	MUTOO::Verbosity get_verbosity() const {return (MUTOO::Verbosity) _verbosity;}	 

	/*! Verbosity level */
	void set_verbosity(MUTOO::Verbosity verbosity) {_verbosity = (UShort_t) verbosity;}
	
	protected:
	UShort_t _verbosity;



};

#endif /* __TRXNPPARBASE_HH__ */

























































































