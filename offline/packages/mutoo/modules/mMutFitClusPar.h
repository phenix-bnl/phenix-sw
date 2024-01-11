#ifndef __MMUTFITCLUSPAR_HH__
#define __MMUTFITCLUSPAR_HH__

#include<PHObject.h>
#include<MUTOO.h>
#include<TMutParBase.h>
#include<TMutParameterDB.h>
#include<TMutClusterFit.h>

//!Runtime parameter object for mMutFitClus analysis module
class mMutFitClusPar : public TMutParBase
{
 public: 
 	
 	/*! default constructor */
 	mMutFitClusPar() : 
 	 	_peak_ratio_scale(0.84),
		_single_track_fit_type( TMutClusterFit::MATHIESON_LOOKUP ),
		_multi_track_fit_type( TMutClusterFit::GSL_MATHIESON ),
 	 	_multi_track_fit(true),
 	 	_chi_max_good_fit(5.0),
 	 	_max_fit(15),
		_mc_smear_perp(0.00),
 	 	_mc_smear_ster(0.00)

	{
 			TMutParameterDB::get().get<unsigned short>("mMutFitClus_verbosity", _verbosity );
	}
 	
 	/*! destructor */
 	~mMutFitClusPar()
 	{}
 	 	
 	/*! Peak over total charge scale factor */
 	void set_peak_ratio_scale(Float_t scale) 
 	{ _peak_ratio_scale = scale; }
 	
 	/*! Peak over total charge scale factor */
 	Float_t get_peak_ratio_scale() const 
 	{ return _peak_ratio_scale; }

	//! single track cluster fit type
	void set_single_track_fit_type( const TMutClusterFit::FitType& type )
	{ _single_track_fit_type = type; }

	//! single track cluster fit type
	const TMutClusterFit::FitType& get_single_track_fit_type( void ) const
	{ return _single_track_fit_type; }
	
	//! multi track cluster fit type
	void set_multi_track_fit_type( const TMutClusterFit::FitType& type )
	{ _multi_track_fit_type = type; }

	//! multi track cluster fit type
	const TMutClusterFit::FitType& get_multi_track_fit_type( void ) const
	{ return _multi_track_fit_type; }
	
 	/*! fitting of multiple tracks to cluster or not */
 	void set_multi_track_fit(bool multi_track_fit)
 	{_multi_track_fit = multi_track_fit;}

 	/*! fitting of multiple tracks to cluster or not */
 	bool get_multi_track_fit() const 
 	{return _multi_track_fit;}
		
 	/*! If chi-square of fit greater than this, try different number tracks */
 	void set_chi_max_good_fit(Float_t chi_max_good_fit)
 	{_chi_max_good_fit = chi_max_good_fit;}

 	/*! If chi-square of fit greater than this, try different number tracks */
 	Float_t get_chi_max_good_fit() const 
 	{return _chi_max_good_fit;}

 	/*! Maximum number of tracks to fit a cluster to */
 	void set_max_fit(Int_t maxfit)
 	{_max_fit = maxfit;}

 	/*! Maximum number of tracks to fit a cluster to */
 	Int_t get_max_fit() const 
 	{return _max_fit;}
 	
	/*! smearing of MC hits in direction perp to the strips */
 	void set_mc_smear_perp(double smear)
 	{ _mc_smear_perp = smear; }
	
	/*! smearing of MC hits in direction perp to the strips */
 	double get_mc_smear_perp() const
 	{ return _mc_smear_perp; }

	/*! MC smearing for stereo planes */
 	void set_mc_smear_ster(double smear)
 	{ _mc_smear_ster = smear; }

	/*! MC smearing for stereo planes */
 	double get_mc_smear_ster() const
 	{ return _mc_smear_ster; }
	
 	//! dump all parameters	
 	void print( std::ostream& out = std::cout ) 
 	{
 	 	MUTOO::PRINT( out, "mMutFitClusPar" );
 	 	out << "_verbosity = " << _verbosity << ".\n";
 	 	out << "_peak_ratio_scale = " << _peak_ratio_scale << std::endl;
		out << "_single_track_fit_type = " << TMutClusterFit::get_fit_type_name( _single_track_fit_type ) << std::endl;
		out << "_multi_track_fit_type = " << TMutClusterFit::get_fit_type_name( _multi_track_fit_type ) << std::endl;
 	 	out << "_multi_track_fit = " << _multi_track_fit << std::endl;
 	 	out << "_chi_max_good_fit = " << _chi_max_good_fit << std::endl;
 	 	out << "_max_fit = " << _max_fit << std::endl;
 	 	out << "_mc_smear_perp = " << _mc_smear_perp << "cm\n";
 	 	out << "_mc_smear_ster = " << _mc_smear_ster << "cm\n";
 	 	MUTOO::PRINT( out, "**" );
 	}
	 	
 private: 	
 	
	//! peak over total charge scale factor
 	Float_t _peak_ratio_scale; 	
 
 	//! single track cluster fit type
	TMutClusterFit::FitType _single_track_fit_type;
 
 	//! single track cluster fit type
	TMutClusterFit::FitType _multi_track_fit_type;
 
	//! set to true if multi track fit is allowed
 	bool _multi_track_fit; 	 	
		
	//! max chi2 to tell first fit is enough 	
 	Float_t _chi_max_good_fit; 	
	
	//! max number of allowed fit
 	Int_t _max_fit; 	 	 	 	 	 	 	
 		
	//! smear perpendicular hit positions by this amount
 	Float_t _mc_smear_perp; 
	
	//! smear stereo hit positions by this amount
 	Float_t _mc_smear_ster; 
};

#endif /* __MMUTFITCLUSPAR_HH__ */
