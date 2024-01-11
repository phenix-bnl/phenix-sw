// $Id: mFvtxResponse.cxx,v 1.17 2011/12/01 04:16:22 slash Exp $

/*!
	\file mFvtxResponse.cxx
	\brief creates Hits from MC hits
	\author X.R Wang (follow Hugo's SlowSim module)
	\evaluation root file mFvtxResponse.root
	\version $Revision: 1.17 $
	\date $Date: 2011/12/01 04:16:22 $
*/

// FVTXOO headers
#include <mFvtxResponse.h>
#include <mFvtxResponsePar.h>
#include <TFvtxMCHitMap.h>
#include <TFvtxHitMap.h>
#include <TMutNode.h>
#include <PHException.h>
#include <FVTXOO.h>
#include <FVTXGEOM.h>
#include <MUTOO_FEM.h>
#include <PHTimer.h>
#include <FvtxGeom.h>
#include <TFvtxMCHit.h>
#include <TFvtxMCHit_v1.h>
#include <TFvtxHit.h>
#include <TFvtxHit_v1.h>


// STL/BOOST/GSL
//
#include <iostream>
#include <string>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_cdf.h>
#include <PHTFileServer.h>

//PH Geometry tools 
#include <PHLine.h>
#include <PHVector.h>
#include <PHPoint.h>
#include <PHGeometry.h>

/*! \ingroup modules */

using namespace std;

//_______________________________________________
mFvtxResponse::mFvtxResponse() :	 
	_timer( PHTimeServer::get()->insert_new("mFvtxResponse")),
				_FvtxHit_tree(0),
				_evt(0)
{
	FVTXOO::TRACE("initializing module mFvtxResponse");
}

//_____________________________________________																											 
void mFvtxResponse::print_summary( ostream& out )
{
// 	FVTXOO::PRINT( out, "mFvtxResponse::print_summary" );
// 	for( int arm=0; arm < FVTXOO::MAX_ARM; arm++ )
// 		for( int station=0; station < FVTXOO::MAX_STATION; station++ )
// 			out << "arm=" << arm << " station=" << station
// 		<< " total_mc_hits=" << endl;
// 	FVTXOO::PRINT( out, "**" );
}

//_______________________________________________
// Event method.
//
PHBoolean mFvtxResponse::event(PHCompositeNode* top_node)
{
	_timer.get()->restart();	 
	try { 
	
		
		// Reset IOC pointers
		set_interface_ptrs(top_node);	 
		if( _mod_par->get_verbosity() >= FVTXOO::SOME ) FVTXOO::TRACE( "mFvtxResponse::event" );
						 
		// Loop over TFvtxMCHits and generate TFvtxHit.
		// Add noise and apply masking where appropriate.
		response_loop();
		
                // Loop over TFvtxHits and add electronics noise to the charge on the strip:
		if ( _mod_par->get_add_noise_to_charge() ) add_noise_to_charge();

                // Add random noise hits to the event:
		if ( _mod_par->get_add_noise_hits() ) add_noise_hits();

                // Loop over TFvtxHits and determine whether the hits should be zero-suppressed or not
                // (this is on-chip zero-suppression)
		if ( _mod_par->get_do_zero_suppress() ) zero_suppress_loop();

                // Convert charge to ADC value:
                do_adc_conversion();

		// fill evaluation tree					
		if( _mod_par->get_do_evaluation() ) fill_evaluationRes_tree();
				
		// dump map size
		if( _mod_par->get_verbosity() >= FVTXOO::SOME ) 
		  cout << "mFvtxResponse::event	- _hit_map->size: " << _hit_map->size() << endl;

	} catch(std::exception& e) {
		FVTXOO::TRACE(e.what());
		return False;
	}		
	// If verbose dump the contents of the hit map
	_timer.get()->stop();
	
	if(_mod_par->get_verbosity() >= FVTXOO::ALOT) _hit_map->print();
	if(_mod_par->get_verbosity() >= FVTXOO::SOME) _timer.get()->print();
	return True;
}

//_______________________________________________
/*! Reset IOC and external interface pointers */
void mFvtxResponse::set_interface_ptrs(PHCompositeNode* top_node)
{	
	
	// Module runtime parameters
	_mod_par = TMutNode<mFvtxResponsePar>::find_node(top_node,"mFvtxResponsePar");
	if( _mod_par->get_verbosity() >= FVTXOO::SOME ) FVTXOO::TRACE( "mFvtxResponse::set_interface_ptrs" );

	// IOC 
	_mc_hit_map = TMutNode<TFvtxMCHitMap>::find_node(top_node,"TFvtxMCHitMap");
	_hit_map = TMutNode<TFvtxHitMap>::find_node(top_node,"TFvtxHitMap");
} 

//_______________________________________________
void mFvtxResponse::response_loop()
{
	if( _mod_par->get_verbosity() >= FVTXOO::SOME ) FVTXOO::TRACE( "mFvtxResponse::response_loop" );
	
	// Get an iterator to all the TFvtxMCHits
	TFvtxMCHitMap::const_iterator mc_hit_iter = _mc_hit_map->range();
	if( _mod_par->get_verbosity() >= FVTXOO::SOME ) 
	cout << "mFvtxResponse::response_loop - mc_hit_iter.counts: " << mc_hit_iter.count() << endl;

	// Loop over TFvtxMCHits
	while(TFvtxMCHitMap::const_pointer mc_hit_ptr = mc_hit_iter.next()){

		// Put chamber efficiency into hits:
		double eff = gsl_rng_uniform(_rng.get());
		if (eff >= _mod_par->get_chamber_efficiency()) continue;

		create_hit(mc_hit_ptr);
	}
}

//_______________________________________________
void mFvtxResponse::add_noise_to_charge()
{
	if( _mod_par->get_verbosity() >= FVTXOO::SOME ) FVTXOO::TRACE( "mFvtxResponse::add_noise_to_charge" );
	
	// Get an iterator to all the TFvtxMCHits
	TFvtxHitMap::const_iterator hit_iter = _hit_map->range();
	if( _mod_par->get_verbosity() >= FVTXOO::SOME ) 
	cout << "mFvtxResponse::add_noise_to_charge - hit_iter.counts: " << hit_iter.count() << endl;

	// Loop over TFvtxMCHits
	while(TFvtxHitMap::const_pointer hit_ptr = hit_iter.next()){

                // Add noise to charge:

                double noise = gsl_ran_gaussian( _rng.get(), _mod_par->get_noise_rms() );
                double chargenew = hit_ptr->get()->get_q() + noise;
        
		hit_ptr->get()->set_q( chargenew > 0 ? chargenew : 0.0 );
	}
}

//_______________________________________________
void mFvtxResponse::add_noise_hits()
{
	if( _mod_par->get_verbosity() >= FVTXOO::SOME ) FVTXOO::TRACE( "mFvtxResponse::add_noise_hits" );
	
	// Determine fraction of strips that should have noise hits.  gsl_cdf_ugaussian_Q
        // function will determine probability for an event to be in the upper tail above
        // the input value you specify (in a distribution with sigma=1.0).

        double xsigma = FVTXOO::FPHX_THRESH[0]/_mod_par->get_noise_rms();

        double qprob;
        qprob = gsl_cdf_ugaussian_Q( xsigma );

        // Calculate total number of strips in system so you know how many should be ON from noise:
        int NumberOfStrips = 0;
        int nstrips;

        // Calculate number of strips in each station, radius:
        for (int ist = 0; ist < FVTXGEOM::NumberOfStations; ist++){
            FvtxColumn* column_ptr = FvtxGeom::get_arm( 0 )->
                  get_cage( 0 )->
                  get_station( ist )->
                  get_sector( 0 )->
                  get_column(0) ;

            nstrips = (int)column_ptr->get_n_strips();
            NumberOfStrips += FVTXGEOM::NumberOfArms*FVTXGEOM::NumberOfCages*FVTXGEOM::NumberOfSectors*FVTXGEOM::NumberOfColumns*nstrips;
        }

        // Randomly generate strip number for noise hit
        int NumberOfNoiseHits = (int)(NumberOfStrips * qprob);
        int iarm, icage, istation, isector, icolumn, istrip;
        for (int ihit = 0; ihit < NumberOfNoiseHits; ihit++){
          iarm = (int)(gsl_rng_uniform(_rng.get()) * FVTXGEOM::NumberOfArms);
          icage = (int)(gsl_rng_uniform(_rng.get()) * FVTXGEOM::NumberOfCages);
          istation = (int)(gsl_rng_uniform(_rng.get()) * FVTXGEOM::NumberOfStations);
          isector = (int)(gsl_rng_uniform(_rng.get()) * FVTXGEOM::NumberOfSectors);
          icolumn = (int)(gsl_rng_uniform(_rng.get()) * FVTXGEOM::NumberOfColumns);
          FvtxColumn* column_ptr = FvtxGeom::get_arm( iarm )->
                  get_cage( icage )->
                  get_station( istation )->
                  get_sector( isector )->
                  get_column(icolumn) ;

          nstrips = (int)column_ptr->get_n_strips();
          istrip = (int)( gsl_rng_uniform(_rng.get()) * nstrips );

          // Check first to see if there is already a hit on this strip.  If there is, skip adding
          // a noise hit again.  If not, create a new hit.

          TFvtxHitMap::iterator oldhit_iter = _hit_map->get(
				iarm,
                                icage,
				istation,
				isector,
				icolumn,
				istrip );

          if (oldhit_iter.count() == 0){
              TFvtxHitMap::iterator hit_iter = _hit_map->insert_new(
				iarm, icage, istation, isector, icolumn, istrip );
  
            // Randomly select charge from tail of noise distribution:
            double noise_charge;
            noise_charge = gsl_ran_ugaussian_tail( _rng.get(), xsigma) * _mod_par->get_noise_rms();
            hit_iter->get()->set_q( noise_charge );
          }
        }

}

//_______________________________________________
void mFvtxResponse::zero_suppress_loop()
{
	if( _mod_par->get_verbosity() >= FVTXOO::SOME ) FVTXOO::TRACE( "mFvtxResponse::zero_suppress_loop" );
	
	// Get an iterator to all the TFvtxHits
	TFvtxHitMap::const_iterator hit_iter = _hit_map->range();
	if( _mod_par->get_verbosity() >= FVTXOO::SOME ) 
          cout << "mFvtxResponse::zero_suppress_loop - hit_iter.counts: " << hit_iter.count() << endl;

	// Loop over TFvtxHits
	while(TFvtxHitMap::const_pointer hit_ptr = hit_iter.next()){

          // Apply threshold to hits.  If below threshold, remove hit.
          if (hit_ptr->get()->get_q() <  FVTXOO::FPHX_THRESH[0])
                                        _hit_map->erase(hit_ptr->get()->get_key());          
	}

}

//_______________________________________________
void mFvtxResponse::do_adc_conversion()
{
	if( _mod_par->get_verbosity() >= FVTXOO::SOME ) FVTXOO::TRACE( "mFvtxResponse::do_adc_conversion" );
	
	// Get an iterator to all the TFvtxHits
	TFvtxHitMap::const_iterator hit_iter = _hit_map->range();
	if( _mod_par->get_verbosity() >= FVTXOO::SOME ) 
          cout << "mFvtxResponse::do_adc_conversion - hit_iter.counts: " << hit_iter.count() << endl;

	// Loop over TFvtxHits
        unsigned short ADC;
        float charge;

	while(TFvtxHitMap::const_pointer hit_ptr = hit_iter.next()){

          charge = hit_ptr->get()->get_q();

          // Apply threshold to hits.  If below threshold, remove hit.
          if (FVTXOO::FPHX_THRESH[0] <= charge && charge < FVTXOO::FPHX_THRESH[1] ) ADC = 0;
          else if (FVTXOO::FPHX_THRESH[1] <= charge && charge < FVTXOO::FPHX_THRESH[2] ) ADC = 1;
          else if (FVTXOO::FPHX_THRESH[2] <= charge && charge < FVTXOO::FPHX_THRESH[3] ) ADC = 2;
          else if (FVTXOO::FPHX_THRESH[3] <= charge && charge < FVTXOO::FPHX_THRESH[4] ) ADC = 3;
          else if (FVTXOO::FPHX_THRESH[4] <= charge && charge < FVTXOO::FPHX_THRESH[5] ) ADC = 4;
          else if (FVTXOO::FPHX_THRESH[5] <= charge && charge < FVTXOO::FPHX_THRESH[6] ) ADC = 5;
          else if (FVTXOO::FPHX_THRESH[6] <= charge && charge < FVTXOO::FPHX_THRESH[7] ) ADC = 6;
          else if (FVTXOO::FPHX_THRESH[7] <= charge ) ADC = 7;

          hit_ptr->get()->set_adc(ADC);

	}

}

//_______________________________________________
void mFvtxResponse::create_hit(TFvtxMCHitMap::const_pointer mc_hit_ptr)
{
	
	unsigned short arm = mc_hit_ptr->get()->get_arm();
        unsigned short cage = mc_hit_ptr->get()->get_cage();
	unsigned short station = mc_hit_ptr->get()->get_station();
	unsigned short sector = mc_hit_ptr->get()->get_sector();
	unsigned short column = mc_hit_ptr->get()->get_column();

	// check if MC hits has strips
	if( _mod_par->get_verbosity() >= FVTXOO::NONE && !mc_hit_ptr->get()->get_n_strip() ) 
	cout << "mFvtxResponse::create_hit - found TFvtxMCHit with no strips hit" << endl;
	
	// loop over fired strips
	for( unsigned int istrip=0; istrip< mc_hit_ptr->get()->get_n_strip(); istrip++ )
	{
		
		const TFvtxMCStrip* strip = mc_hit_ptr->get()->get_strip(istrip);			 
		
                // kluge to check on invalid strip until real fix is put in
                FvtxColumn* column_ptr = FvtxGeom::get_arm( mc_hit_ptr->get()->get_arm())->
                  get_cage(mc_hit_ptr->get()->get_cage())->
                  get_station(mc_hit_ptr->get()->get_station())->
                  get_sector(mc_hit_ptr->get()->get_sector())->
                  get_column(mc_hit_ptr->get()->get_column()) ;

                int strip_num = (int) strip->get_strip();
                int max_strips = (int)column_ptr->get_n_strips();
                if (strip_num >= max_strips || strip_num < 0){
                  cout << "Invalid strip number encountered in mFvtxResponse " << 
                  mc_hit_ptr->get()->get_arm() << ", " <<
                  mc_hit_ptr->get()->get_cage() << ", " <<
                  mc_hit_ptr->get()->get_station() << ", " <<
                  mc_hit_ptr->get()->get_sector() << ", " <<
                  mc_hit_ptr->get()->get_column() << ", " <<
                  strip->get_strip() << endl;
                  continue;
                }
                
		// insert new hit
		TFvtxHitMap::iterator hit_iter = _hit_map->insert_new(
				arm,
                                cage,
				station,
				sector,
				column,
				strip->get_strip() );

		
		// fill charge
		/*
			Get the charge from the TFvtxHit
			Add the charge from the TFvtxMCStrip
			and update
		*/
		double charge = hit_iter->get()->get_q();
		charge += strip->get_q();

		hit_iter->get()->set_q( charge );
		
		// associate MC hit with this hit for evaluation purpose
		PHKey::associate(TFvtxMCHitMap::pointer(mc_hit_ptr), hit_iter.current());

	}
	
}

//____________________________________________________________																				
void mFvtxResponse::book_evaluationRes_tree( void )
{

	FVTXOO::PRINT( cout, "FvtxUnpackPisa::book_evaluationRes_tree" );
	_evt = 0;

	// create TFile																																							 
	PHTFileServer::get().open( _mod_par->get_evaluation_file(), "RECREATE" );
	cout << "writing to file \"" << _mod_par->get_evaluation_file() << "\"" << endl;

	enum { BUFFER_SIZE=32000 };
	enum { AUTO_SAVE=16000 };

	_FvtxHit_tree = new TTree( "FvtxHit_eval", "FvtxHit_eval" );
	_FvtxHit_tree->Branch( "evt", &_evt, "evt/I", BUFFER_SIZE );
	_FvtxHit_tree->Branch( "arm", &_arm, "arm/I", BUFFER_SIZE );
	_FvtxHit_tree->Branch( "cage",   &_cage, "cage/I", BUFFER_SIZE );
        _FvtxHit_tree->Branch( "station", &_station, "station/I", BUFFER_SIZE );
	_FvtxHit_tree->Branch( "sector;", &_sector, "sector/I", BUFFER_SIZE );
	_FvtxHit_tree->Branch( "column;", &_column, "column/I", BUFFER_SIZE );

	_FvtxHit_tree->Branch( "q", &_q, "q/D", BUFFER_SIZE );
	FVTXOO::PRINT( cout, "**" );
}

//-------------------------------------------------------------------------
void mFvtxResponse::fill_evaluationRes_tree( void )
{

	if( !_FvtxHit_tree ) book_evaluationRes_tree();

	TFvtxHitMap::const_iterator hit_iter = _hit_map->range();

	// Loop over TFvtxHits
	while(TFvtxHitMap::const_pointer hit_ptr = hit_iter.next()){

  	  // get hit indexes	
	  _arm= hit_ptr->get()->get_arm();
          _cage= hit_ptr->get()->get_cage();
	  _station= hit_ptr->get()->get_station();
	  _sector= hit_ptr->get()->get_sector();
	  _column= hit_ptr->get()->get_column();
	  _q = hit_ptr->get()->get_q();

	  _FvtxHit_tree->Fill();

	  _evt++;

        }
}
