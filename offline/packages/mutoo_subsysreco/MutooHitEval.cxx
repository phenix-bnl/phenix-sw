// $Id: MutooHitEval.cxx,v 1.7 2011/07/14 22:27:10 pinkenbu Exp $

//////////////////////////////////////////////////////////////
/*!
\file MutooHitEval.cxx
\brief single hit evaluation ntuple
\author  Hugo Pereira
\version $Revision: 1.7 $
\date $Date: 2011/07/14 22:27:10 $
*/
//////////////////////////////////////////////////////////////

#include "MutooHitEval.h"

#include <iostream>

#include <MUTOO.h>
#include <PHTFileServer.h>
#include <TTree.h>

#include <TMutClusMap.h>
#include <TMutCoordMap.h>
#include <TMutTrkMap.h>
#include <TMutMCHitMap.h>
#include <MutCalib.h>
#include <TMutGeo.h>
#include <MutStrip.h>

using namespace std;

//_____________________________________
MutooHitEval::MutooHitEval( const char* name, const char* file_name ) :
    _file_name( file_name ),
    _timer(PHTimeServer::get()->insert_new(name) ),
    _extended_tree( false )
{
    ThisName = name;
    return ;
}

//_____________________________________
int MutooHitEval::Init(PHCompositeNode *top_node)
{
    MUTOO::PRINT( cout, "MutooHitEval::Init" );

    // create TFile
    PHTFileServer::get().open( _file_name, "RECREATE" );
    cout << "writing to file \"" << _file_name << "\"" << endl;

    enum { BUFFER_SIZE=32000 };
    enum { AUTO_SAVE=16000 };

    _eval_hits = new TTree( "eval_charge", "charge" );
    _eval_hits->Branch( "arm", &_arm, "arm/I", BUFFER_SIZE );
    _eval_hits->Branch( "station", &_station, "station/I", BUFFER_SIZE );
    _eval_hits->Branch( "octant", &_octant, "octant/I", BUFFER_SIZE );
    _eval_hits->Branch( "half", &_half, "half/I", BUFFER_SIZE );
    _eval_hits->Branch( "gap", &_gap, "gap/I", BUFFER_SIZE );
    _eval_hits->Branch( "cathode", &_cathode, "cathode/I", BUFFER_SIZE );
    _eval_hits->Branch( "strip", &_strip, "strip/I", BUFFER_SIZE );
    _eval_hits->Branch( "hit_mult", &_hit_mult, "hit_mult/I", BUFFER_SIZE );

    _eval_hits->Branch( "q", &_q, "q/F", BUFFER_SIZE );
    _eval_hits->Branch( "error", &_error, "error/F", BUFFER_SIZE );
    _eval_hits->Branch( "attenuated", &_attenuated, "attenuated/O", BUFFER_SIZE );
    _eval_hits->Branch( "saturated", &_saturated, "saturated/O", BUFFER_SIZE );
    _eval_hits->Branch( "has_track", &_has_track, "has_track/O", BUFFER_SIZE );
    _eval_hits->Branch( "is_peak", &_is_peak, "is_peak/O", BUFFER_SIZE );
    _eval_hits->Branch( "clus_size", &_clus_size, "clus_size/I", BUFFER_SIZE );
    _eval_hits->Branch( "clus_chi2", &_clus_chi2, "clus_chi2/F", BUFFER_SIZE );
    _eval_hits->Branch( "clus_q", &_clus_q, "clus_q/F", BUFFER_SIZE );

    if( _extended_tree )
    {
        _eval_hits->Branch( "calib_gain", &_calib_gain, "calib_gain/F", BUFFER_SIZE );
        _eval_hits->Branch( "calib_rms", &_calib_rms, "calib_rms/F", BUFFER_SIZE );
        _eval_hits->Branch( "samples", &_samples[0], "samples[4]/I", BUFFER_SIZE );
        _eval_hits->Branch( "calib_ped", &_calib_ped, "calib_ped/F", BUFFER_SIZE );
        _eval_hits->Branch( "amu", &_amu[0], "amu[4]/F", BUFFER_SIZE );
        _eval_hits->Branch( "qsamples", &_qsamples[0], "qsamples[4]/F", BUFFER_SIZE );
        _eval_hits->Branch( "qsamples_rms", &_qsamples_rms, "qsamples_rms/F", BUFFER_SIZE );

        _eval_hits->Branch( "q_tot_mc", &_q_tot_mc, "q_tot_mc/F", BUFFER_SIZE );
        _eval_hits->Branch( "q_mc", &_q_mc, "q_mc/F", BUFFER_SIZE );
        _eval_hits->Branch( "packet_id", &_packet_id, "packet_id/I", BUFFER_SIZE );
    }

    _eval_hits->SetAutoSave( AUTO_SAVE );
    cout << "hits evaluation tree booked booked" << endl;

    MUTOO::PRINT( cout, "**" );
    return 0;
}
//_____________________________________
int MutooHitEval::InitRun(PHCompositeNode *top_node)
{ return 0; }

//_____________________________________
int MutooHitEval::process_event(PHCompositeNode *top_node)
{
    _timer.get()->restart();
    try {

        set_interface_ptrs( top_node );
        fill_tree();

    } catch( exception& e )  { cout << e.what() << endl; }
    _timer.get()->stop();

    return 0;

};

//________________________________________________________
void MutooHitEval::fill_tree( void )
{

    if( !_hit_map ) return;

    // hit multiplicity
    double hits_south = _hit_map->get( MUTOO::South ).count();
    double hits_north = _hit_map->get( MUTOO::North ).count();

    TMutHitMap::iterator hit_iter = _hit_map->range();
    while( TMutHitMap::pointer hit_ptr = hit_iter.next() )
    {

        // store hit location
        _arm = hit_ptr->get()->get_arm();
        _station = hit_ptr->get()->get_station();
        _octant = hit_ptr->get()->get_octant();
        _half = hit_ptr->get()->get_half_octant();
        _gap = hit_ptr->get()->get_gap();
        _cathode = hit_ptr->get()->get_cathode();
        _strip = hit_ptr->get()->get_strip();

        _hit_mult = (_arm == MUTOO::South) ? hits_south:hits_north;

        // store hit charge and error
        _q = hit_ptr->get()->get_q();
        _error = hit_ptr->get()->get_error_q();

        // retrieve geom object
        MutStrip *strip_ptr = TMutGeo::get_strip_geom(
            hit_ptr->get()->get_arm(), hit_ptr->get()->get_station(), hit_ptr->get()->get_octant(), hit_ptr->get()->get_half_octant(), hit_ptr->get()->get_gap(), hit_ptr->get()->get_cathode(), hit_ptr->get()->get_strip() );

        // atenuation
        _attenuated = int( (!strip_ptr) || strip_ptr->UseAttenuation() );

        // saturation
        _saturated = hit_ptr->get()->get_is_saturated();

        // retrieve associated cluster
        _clus_size = 0;
        _clus_chi2 = -1;
        _has_track = false;
        _is_peak = false;
        TMutClusMap::const_key_iterator clus_iter = hit_ptr->get()->get_associated<TMutClus>();
        while( TMutClusMap::const_pointer clus_ptr = clus_iter.next() )
        {
            _clus_size = clus_ptr->get()->get_n_strip();
            _clus_chi2 = clus_ptr->get()->get_chi_square();
            _clus_q = 0;

            // check if peak strip
            TMutHitMap::const_key_iterator local_hit_iter( clus_ptr->get()->get_associated<TMutHit>() );
            double q_max(-1);

            while( TMutHitMap::const_pointer local_hit_ptr = local_hit_iter.next() )
            {
                if( local_hit_ptr->get()->get_q() > q_max )
                {
                    q_max = local_hit_ptr->get()->get_q();
                    _is_peak = ( local_hit_ptr->get()->get_strip() == _strip );
                }

                _clus_q += local_hit_ptr->get()->get_q();
            }

            // get associated coordinates
            TMutCoordMap::const_key_iterator coord_iter( clus_ptr->get()->get_associated<TMutCoord>() );
            while( TMutCoordMap::const_pointer coord_ptr = coord_iter.next() )
            {
                if( coord_ptr->get()->get_associated<TMutTrk>().count() )
                {
                    _has_track = true;
                    break;
                }
            }

        }

        if( _extended_tree )
        {

            // packet id
            _packet_id = strip_ptr ? strip_ptr->getPacket_ID() : -1;

            // retrieve associated calibration
            MutCalibStrip *calib = MutCalib();
            const PdbMutCalibStrip *dbstrip = calib->getPdbMutCalibStrip(
                hit_ptr->get()->get_arm(), hit_ptr->get()->get_station(), hit_ptr->get()->get_octant(), hit_ptr->get()->get_half_octant(), hit_ptr->get()->get_gap(), hit_ptr->get()->get_cathode(), hit_ptr->get()->get_strip() );

            // calibration constants
            _calib_gain = (dbstrip) ? dbstrip->get_gain():-1;
            _calib_rms = (dbstrip) ? dbstrip->get_rms():-1;
            _calib_ped = (dbstrip) ? dbstrip->get_pedestal():-1;

            // store sample and associate charge
            _amu.assign( 0 );
            _samples.assign( 0 );
            _qsamples.assign( 0 );
            for( unsigned int i=0; i<4; i++ )
            {
                _amu[i] = hit_ptr->get()->get_amu( i );
                _samples[i] = hit_ptr->get()->get_adc( i );
                if( dbstrip ) _qsamples[i] = dbstrip->getCharge( _samples[i] );
            }

            // retrieve peak charge
            double qmean = (_qsamples.at(1) + _qsamples.at(2) + _qsamples.at(3))/3.0;

            // retrieve RMS
            _qsamples_rms = sqrt((
                pow((_qsamples.at(1) - qmean),2) +
                pow((_qsamples.at(2) - qmean),2) +
                pow((_qsamples.at(3) - qmean),2) ) / 3.0);

            // retrieve associated MC hit
            _q_mc = -1;
            _q_tot_mc = -1;

            bool found = false;
            TMutMCHitMap::key_iterator mc_hit_iter = hit_ptr->get()->get_associated<TMutMCHit>();
            while( TMutMCHitMap::pointer mc_hit_ptr = mc_hit_iter.next() )
            {

                // get associated strip list
                if( !mc_hit_ptr->get()->get_strip_list() ) continue;

                _q_tot_mc = mc_hit_ptr->get()->get_eloss();

                TMutMCHit::strip_list strips( *mc_hit_ptr->get()->get_strip_list() );
                for( unsigned int i=0; i<strips.size() && !found; i++ )
                {

                    // check strip and hit matches
                    if( !(
                        strips[i].get_cathode() == hit_ptr->get()->get_cathode() &&
                        strips[i].get_strip() == hit_ptr->get()->get_strip() ) ) continue;

                    _q_mc = strips[i].get_q();
                    found = true;
                    break;

                }

                if( found ) break;

            }

        }

        // fill tree
        if( _eval_hits ) _eval_hits->Fill();

    } // loop over hits

    return;
}

//__________________________________________
void MutooHitEval::set_interface_ptrs( PHCompositeNode *top_node )
{

    // try get maps from top node
    _hit_map = 0;
    try {
        _hit_map = TMutNode<TMutHitMap>::find_node(top_node,"TMutHitMap");
    } catch( exception &e ) {
        cout << e.what() << endl;
    }

}

//________________________________________________
int MutooHitEval::End( PHCompositeNode *top_node )
{
//     _timer.get()->print_stat();
    PHTFileServer::get().write( _file_name );
    return 0;
}
