#ifndef __MMUTRESPONSEPAR_HH__
#define __MMUTRESPONSEPAR_HH__

#include<packetConstants.h>
#include<TMutParBase.h>
#include<TMutParameterDB.h>
#include<TDataType.h>
#include<PHException.h>
#include<MUTOO.h>
#include <boost/array.hpp>

//! Runtime parameter object for mMutResponse analysis module.
/*!
Runtime parameter object for mMutResponse analysis module
*/
class mMutResponsePar : public TMutParBase
{
 public:

	/*! default constructor */
	mMutResponsePar() :
		_add_noise(false),
		_gain_error(0.00),
		_pedestal_error(0.0),
		_rise_time(4500.0),
		_fall_time(1000.0),
		_offset( -300 ),
		_adcmin(10),
		_adcmax(2047),
		_smear_q(true),
		_use_rms_scale( true ),
		_atten_strips(true),
		_add_drift_time(true)
	{
		//init the time samples
		_time_samples.push_back(0);
		_time_samples.push_back(500);
		_time_samples.push_back(600);
		_time_samples.push_back(700);

		// default value for rms_scale common noise is 1
		_rms_scale.assign(1);

		/*
			initialize additional nose for each gap
			needed to reproduce real data resolution
		*/

		// SOUTH
		set_rms_scale( 0, 0, 0, 3.27693 );
		set_rms_scale( 0, 0, 1, 3.59092 );
		set_rms_scale( 0, 0, 2, 3.28948 );
		set_rms_scale( 0, 1, 0, 2.24034 );
		set_rms_scale( 0, 1, 1, 2.25319 );
		set_rms_scale( 0, 1, 2, 2.99149 );
		set_rms_scale( 0, 2, 0, 2.82337 );
		set_rms_scale( 0, 2, 1, 1.8942	);

		// NORTH
		set_rms_scale( 1, 0, 0, 4.26734 );
		set_rms_scale( 1, 0, 1, 3.69984 );
		set_rms_scale( 1, 0, 2, 4.0609	);
		set_rms_scale( 1, 1, 0, 2.15482 );
		set_rms_scale( 1, 1, 1, 1.92973 );
		set_rms_scale( 1, 1, 2, 2.61322 );
		set_rms_scale( 1, 2, 0, 2.69575 );
		set_rms_scale( 1, 2, 1, 2.64299 );

		// default value for chamber efficiency is 0.98
		_chamber_efficiency.assign(0.98);

		//! read verbosity from parameter file, if any
		TMutParameterDB::get().get<unsigned short>( "mMutResponse_verbosity",_verbosity );
	}

	/*! destructor */
	~mMutResponsePar()
	{}

	/*! option to add extra noise hits */
	const bool& add_noise_hits() const
	{ return _add_noise; }

	/*! option to add extra noise hits */
	void set_noise_flag( const bool& add_noise )
	{ _add_noise = add_noise; }

	/*! option to smear q by rms from calibrations */
	const bool& get_smear_q() const
	{ return _smear_q; }

	/*! option to smear q by rms from calibrations */
	void set_smear_q( const bool& smear_q )
	{ _smear_q = smear_q; }

	/*! option to scale up RMS read from calibration when calculating charge noise */
	const bool& get_use_rms_scale() const
	{ return _use_rms_scale; }

	/*! option to scale up RMS read from calibration when calculating charge noise */
	void set_use_rms_scale( const bool& value )
	{ _use_rms_scale = value; }

	/*! option to attenuate charge on scratched cathodes*/
	const bool& get_atten_strips() const
	{ return _atten_strips; }

	/*! option to attenuate charge on scratched cathodes */
	void set_atten_strips( const bool& atten_strips)
	{ _atten_strips = atten_strips; }

	/*! fractional error. */
	const double& get_gain_error() const
	{ return _gain_error; }

	/*! fractional error. */
	void set_gain_error( const double& gain_error)
	{ _gain_error = gain_error; }

	/*! error in units of ADC counts. */
	const double& get_pedestal_error() const
	{ return _pedestal_error; }

	/*! error in units of ADC counts. */
	void set_pedestal_error( const double& pedestal_error)
	{ _pedestal_error = pedestal_error; }

	/*! cathode pulse rise time in nanoseconds. */
	const double& get_rise_time() const
	{ return _rise_time; }

	/*! cathode pulse rise time in nanoseconds. */
	void set_rise_time( const double& rise_time)
	{ _rise_time = rise_time; }

	/*! cathode pulse fall time in nanoseconds */
	const double& get_fall_time() const
	{ return _fall_time; }

	/*! cathode pulse fall time in nanoseconds */
	void set_fall_time( const double& fall_time )
	{ _fall_time = fall_time; }

	/*! cathode pulse sample offset wrt first sample in nanoseconds	 */
	const double& get_offset() const
	{ return _offset; }

	/*! cathode pulse sample offset wrt first sample in nanoseconds	 */
	void set_offset( const double& value )
	{ _offset = value; }

	/*! number of ADC samples from each cathode pulse */
	int get_nsamples() const
	{ return _time_samples.size(); }

	/*! number of ADC samples from each cathode pulse */
	void set_nsamples( const int& nsamples)
	{ _time_samples.resize(nsamples, 0.0); }

	/*! cathode pulse sample times in nanoseconds */
	const std::vector<double>& get_time_samples() const
	{ return _time_samples; }

	/*! cathode pulse sample times in nanoseconds */
	void set_time_samples(const unsigned int& index, const double& time)
	{
		if(index < _time_samples.size()) _time_samples[index] = time;
	}

	/*! common noise rms_scale factor */
	void set_rms_scale( const int& arm, const int& station, const int& gap, const double& value )
	{ _rms_scale[gap + MUTOO::NumberOfGaps*( station + MUTOO::NumberOfStations* arm) ] = value; }

	/*! common noise rms_scale factor */
	const double& get_rms_scale( const int& arm, const int& station, const int& gap ) const
	{	return _rms_scale[gap + MUTOO::NumberOfGaps*( station + MUTOO::NumberOfStations* arm )]; }

	/*! min number of ADC counts for a sample */
	const int& get_adcmin() const
	{ return _adcmin; }

	/*! min number of ADC counts for a sample */
	void set_adcmin( const int& adcmin )
	{ _adcmin = adcmin; }

	/*! max number of ADC counts for a sample */
	int get_adcmax() const
	{ return _adcmax; }

	/*! max number of ADC counts for a sample */
	void set_adcmax(int adcmax)
	{ _adcmin = adcmax; }

	/*! option to add drift time */
	const bool& add_drift_time() const
	{ return _add_drift_time; }

	/*! option to add drift time */
	void set_drift_time_flag( const bool& add_drift_time)
	{ _add_drift_time = add_drift_time; }

	/*! Chamber efficiency (0-1.0) */
	const double& get_chamber_efficiency() const
	{
	  MUTOO::PRINT( std::cout, "**" );
	  std::cout << "mMutResponsePar::get_chamber_efficiency() no longer implemented" << std::cout;
	  std::cout << " use mMutResponsePar::get_chamber_efficiency( const int& arm, const int& station, const int& gap ) instead" << std::cout;
	  MUTOO::PRINT( std::cout, "**" );
	  exit(1);
	}

        /*! Chamber efficiency (0-1.0) */
        const double& get_chamber_efficiency( const int& arm, const int& station, const int& gap, const int& plane, const int& octant, const int& half_oct) const
	  {    
	    int index = plane + MUTOO::NumberOfPlanes*(half_oct + MUTOO::MAX_HALF_OCTANT *(octant + MUTOO::MAX_OCTANT *(gap + MUTOO::NumberOfGaps*( station + MUTOO::NumberOfStations* arm ))));
	    return _chamber_efficiency[index]; 
	  }

	/*! Chamber efficiency (0-1.0) */
	void set_chamber_efficiency( const double& value)
	{
	  for(int iarm=0;iarm<MUTOO::NumberOfArms;iarm++)
	  for(int ista=0;ista<MUTOO::NumberOfStations;ista++)
	  for(int igap=0;igap<MUTOO::NumberOfGaps;igap++)
	  for(int ipla=0;ipla<MUTOO::NumberOfPlanes;ipla++)
	    for (int ioct=0; ioct<MUTOO::MAX_OCTANT; ioct++)
	      for (int ihoct=0; ihoct<MUTOO::MAX_HALF_OCTANT; ihoct++)
		set_chamber_efficiency(iarm,ista,igap,ipla,ioct,ihoct,value);
	}

        void set_chamber_efficiency( const int& arm, const int& station, const int& gap, const int& pla, const int& octant, const int& half_oct, const double& value )
	  {
	    int index =  pla + MUTOO::NumberOfPlanes*(half_oct + MUTOO::MAX_HALF_OCTANT *(octant + MUTOO::MAX_OCTANT *(gap + MUTOO::NumberOfGaps*( station + MUTOO::NumberOfStations* arm ))));
	    _chamber_efficiency[index] = value; 
	  }

	//! dump all parameters
	void print( void ) {
		MUTOO::PRINT( std::cout, "mMutResponsePar" );
		std::cout << "_verbosity = " << _verbosity << ".\n";

		std::cout << "_add_noise = " << _add_noise << ".\n";
		std::cout << "_gain_error = " << _gain_error << ".\n";
		std::cout << "_pedestal_error = " << _pedestal_error << ".\n";
		std::cout << "_rise_time = " << _rise_time << ".\n";
		std::cout << "_fall_time = " << _fall_time << ".\n";
		std::cout << "_offset = " << _offset << ".\n";

		std::cout << "_time_samples = ";
		for( unsigned int i=0; i<_time_samples.size(); i++ )
		std::cout << ((i) ? ", " : " ") << _time_samples[i];
		std::cout << std::endl;

		std::cout << "_adcmin = " << _adcmin << ".\n";
		std::cout << "_adcmax = " << _adcmax << ".\n";
		std::cout << "_smear_q = " << _smear_q << ".\n";
		std::cout << "_use_rms_scale = " << _use_rms_scale << ".\n";
		std::cout << "_atten_strips = " << _atten_strips << ".\n";
		std::cout << "_add_drift_time = "<< _add_drift_time << ".\n";
		std::cout << std::endl;

		// dump scale factors
		for( int arm=0; arm < MUTOO::NumberOfArms; arm++ )
		for( int station = 0; station < MUTOO::NumberOfStations; station++ )
		for( int gap = 0; gap < MUTOO::NumberOfGaps; gap++ )
		if( gap != 2 || station != 2 )
		{
			std::cout
				<< "_rms_scale["
				<< arm << "," << station << "," << gap << "]=" << get_rms_scale( arm, station, gap)
				<< std::endl;
		}

                // dump efficiencies factors
                for( int arm=0; arm < MUTOO::NumberOfArms; arm++)//MUTOO::NumberOfArms; arm++ )
		for( int station = 0; station < MUTOO::NumberOfStations; station++ )
		for( int gap = 0; gap < MUTOO::NumberOfGaps; gap++ )
		for( int pla = 0; pla < MUTOO::NumberOfPlanes; pla++ )
		for( int oct = 0; oct < MUTOO::MAX_OCTANT; oct++ )
		for( int hoct = 0; hoct < MUTOO::MAX_HALF_OCTANT; hoct++ )
		if( (gap != 2 || station != 2) && pla!=2 )
		{
		  int index =  pla + MUTOO::NumberOfPlanes*(hoct + MUTOO::MAX_HALF_OCTANT *(oct + MUTOO::MAX_OCTANT *(gap + MUTOO::NumberOfGaps*( station + MUTOO::NumberOfStations* arm ))));
		  std::cout
		    << "_chamber_efficiency["
		    << index << " " << arm << "," << station << "," << gap << "," << pla << "," << oct << "," << hoct << "]=" << get_chamber_efficiency( arm, station, gap, pla , oct, hoct)
		    << std::endl;
		}

		MUTOO::PRINT( std::cout, "**" );
	}

	private:

	//! true is adding random noise hits
	bool _add_noise;

	//! fractional
	double _gain_error;

	//!ADC count
	double _pedestal_error;

	//! //pulse rise time
	double _rise_time;

	//! pulse fall time
	double _fall_time;

	//! cathode pulse offset wrt first sample
	double _offset;

	//! time at which analog signal is sampled to ADC counts
	std::vector<double> _time_samples;

	//! minimum adc counts to store for a sample
	int _adcmin;

	//! maximum adc counts to store for a sample
	int _adcmax;

	//! boolean to control charge smearing
	bool _smear_q;

	//! use RMS scale
 bool _use_rms_scale;

	//! boolean to control scratched cathodes
	bool _atten_strips;

	//! true adds drift time before digitization
	bool _add_drift_time;

	//! chamber efficiency
	boost::array<double,1024> _chamber_efficiency;

	//! cathode additional single strip smearing
	boost::array<double,18> _rms_scale;


};

#endif /* __MMUTRESPONSEPAR_HH__ */
