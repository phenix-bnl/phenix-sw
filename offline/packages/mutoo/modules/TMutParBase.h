#ifndef __TMUTPARBASE_HH__
#define __TMUTPARBASE_HH__

#include <iostream>
#include <PHObject.h>
#include <MUTOO.h>
#include <TMutParameterDB.h>

//! Base class for runtime parameter objects.	
/*! 
	Satisfies PHOOL interface requirements and provides interface
	to run-time parameters that are common to all modules.
*/

class TMutParBase : public PHObject
{

	public: 

	/*! default constructor */
	TMutParBase() : _verbosity(MUTOO::NONE)
	{	
		TMutParameterDB::get().get<unsigned short>("TMutParBase_verbosity", _verbosity);
	}
	
	/*! destructor */
	virtual ~TMutParBase(){;}

	/*! PHOOL inteface requirement */
  virtual void identify(std::ostream& os = std::cout) const {os << "TMutParBasePar";}

	/*! PHOOL interface requirement -- null op */
	virtual void Reset(){;}

	/*! PHOOL interface requirement -- always returns 1 */
	virtual int isValid() const {return 1;}

	/*! Verbosity level */
	MUTOO::Verbosity get_verbosity() const {return (MUTOO::Verbosity) _verbosity;}	 

	/*! Verbosity level */
	void set_verbosity(MUTOO::Verbosity verbosity) {_verbosity = (unsigned short) verbosity;}
	
	protected:
	unsigned short _verbosity;



};

#endif /* __TMUTPARBASE_HH__ */

























































































