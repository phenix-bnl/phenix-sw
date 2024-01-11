#ifndef __MFVTXRESPONSEPAR_HH__
#define __MFVTXRESPONSEPAR_HH__

#include<packetConstants.h>
#include<TFvtxParBase.h>
#include<PHException.h>
//#include <boost/array.hpp>

#include<FVTXOO.h>

//! Runtime parameter object for mFvtxResponse analysis module.
/*! 
Runtime parameter object for mFvtxResponse analysis module
*/
class mFvtxResponsePar : public TFvtxParBase
{	
 public: 

	/*! default constructor */
	mFvtxResponsePar() :
		_do_evaluation( false ),
		_evaluation_file( "mFvtxResponse.root" ),
		_chamber_efficiency( 0.98 ),
                _add_noise_to_charge( true ),
                _noise_rms( 400.0 ),           // electrons
                _add_noise_hits( true ),
                _do_zero_suppress( true)
	{}
	
	/*! destructor */
	~mFvtxResponsePar(){;}
	
	/*! Chamber efficiency (0-1.0) */
	double get_chamber_efficiency() const 
	{ return _chamber_efficiency; }
	
	/*! Chamber efficiency (0-1.0) */
	void set_chamber_efficiency(double value)
	{ _chamber_efficiency = value; }

	/*! Add noise to generated charge on strips or not */
	bool get_add_noise_to_charge() const 
	{ return _add_noise_to_charge; }
	
	/*! Add noise to generated charge on strips or not */
	void set_add_noise_to_charge(bool value)
	{ _add_noise_to_charge = value; }

	/*! RMS of noise to add to strip charge (electrons) */
	double get_noise_rms() const 
	{ return _noise_rms; }
	
	/*! RMS of noise to add to strip charge (electrons) */
	void set_noise_rms(double value)
	{ _noise_rms = value; }

	/*! Add random noise hits to event or not */
	bool get_add_noise_hits() const 
	{ return _add_noise_hits; }
	
	/*! Add random noise hits to event or not  */
	void set_add_noise_hits(bool value)
	{ _add_noise_hits = value; }

	/*! Apply on-chip zero suppression to hits or not */
	bool get_do_zero_suppress() const 
	{ return _do_zero_suppress; }
	
	/*! Apply on-chip zero suppression to hits or not */
	void set_do_zero_suppress(bool value)
	{ _do_zero_suppress = value; }

	//! evaluation flag
	void set_do_evaluation( const bool& value )
		{ _do_evaluation = value;}

	//! evaluation flag
	const bool& get_do_evaluation( void ) const
		{ return _do_evaluation; }

	//! evaluation file
	void set_evaluation_file( const std::string& value )
		{ _evaluation_file = value; }

	//! evaluation file
	const std::string& get_evaluation_file( void ) const
		{ return _evaluation_file; }

        //! dump all parameters
        void print(std::ostream& out = std::cout) const {
          FVTXOO::PRINT(out, "mFvtxResponsePar");
          out << "_do_evaluation: " << _do_evaluation << std::endl;
          out << "_evaluation_file: " << _evaluation_file << std::endl;
          out << "_verbosity = " << _verbosity << ".\n";
          out << "_chamber_efficiency = " << _chamber_efficiency << ".\n";
          FVTXOO::PRINT(out, "**");
        }

        //! evaluation tag
	bool _do_evaluation;

	//! evaluation output file
	std::string _evaluation_file;
		
	//! chamber efficiency 
	double _chamber_efficiency;

  //! add noise to the real data hits or not
  bool _add_noise_to_charge;

  //! level of noise to add (electrons)
  double _noise_rms;

  //! add random noise hits or not
  bool _add_noise_hits;

  //! apply on-chip zero suppression to hits or not
  bool _do_zero_suppress;

  ClassDef(mFvtxResponsePar, 1);
};

#endif /* __MFVTXRESPONSEPAR_HH__ */







