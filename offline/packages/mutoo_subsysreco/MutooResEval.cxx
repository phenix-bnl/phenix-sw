// $Id: MutooResEval.cxx,v 1.10 2012/02/27 18:13:28 slash Exp $

//////////////////////////////////////////////////////////////
/*!
\file MutooResEval.cxx
\brief fun4all mutoo resolution evaluator
\author  Hugo Pereira
\version $Revision: 1.10 $
\date $Date: 2012/02/27 18:13:28 $
*/
//////////////////////////////////////////////////////////////

#include "MutooResEval.h"

#include <iostream>

#include <MUTOO.h>
#include <mMutBPFitPar.h>
#include <mMutKalFitPar.h>
#include <PHTFileServer.h>

#include <TMuiHitMapO.h>
#include <TMuiClusterMapO.h>
#include <TMuiRoadMapO.h>
#include <TMuiGeo.h>

#include <TMutClusMap.h>
#include <TMutCoordMap.h>
#include <TMutGeo.h>
#include <TMutMeasureModel.h>
#include <TMutTrkMap.h>
#include <TMutTrackUtil.h>

using namespace std;

//_____________________________________
MutooResEval::MutooResEval( const char* name, const char* file_name ) :
    SubsysReco( name ),
    _timer(PHTimeServer::get()->insert_new(name) ),
    _file_name( file_name ),
    _flags( REFIT|MAGNETS_ON )
{ return; }

//_____________________________________
int MutooResEval::Init(PHCompositeNode *top_node)
{
    MUTOO::PRINT( cout, "MutooResEval::Init" );

    // print flags
    cout << "flags: " << endl;
    cout << "REFIT: " << (get_flag( REFIT ) ? "true":"false" ) << endl;
    cout << "MAGNETS_ON: " << (get_flag( MAGNETS_ON ) ? "true":"false" ) << endl;
    cout << "USE_MUID: " << (get_flag( USE_MUID ) ? "true":"false" ) << endl;

    // create TFile
    PHTFileServer::get().open( _file_name, "RECREATE" );
    cout << "writing to file \"" << _file_name << "\"" << endl;

    enum { BUFFER_SIZE=32000 };
    enum { AUTO_SAVE=16000 };
    _resolution = new TTree( "resolution", "resolution" );
    _resolution->Branch( "arm", &_arm, "arm/I", BUFFER_SIZE );
    _resolution->Branch( "station", &_station, "station/I", BUFFER_SIZE );
    _resolution->Branch( "octant", &_octant, "octant/I", BUFFER_SIZE );
    _resolution->Branch( "half", &_half, "half/I", BUFFER_SIZE );
    _resolution->Branch( "gap", &_gap, "gap/I", BUFFER_SIZE );
    _resolution->Branch( "cathode", &_cathode, "cathode/I", BUFFER_SIZE );

    if( get_flag( USE_MUID ) )
    {
        _resolution->Branch( "plane", &_plane, "plane/I", BUFFER_SIZE );
        _resolution->Branch( "panel", &_panel, "panel/I", BUFFER_SIZE );
        _resolution->Branch( "orientation", &_orientation, "orientation/I", BUFFER_SIZE );
        _resolution->Branch( "is_muid", &_is_muid, "is_muid/O", BUFFER_SIZE );
    }

    _resolution->Branch( "hits_station", &_hits_station[0], "hits_station[3]/I", BUFFER_SIZE );

    _resolution->Branch( "trk_chisquare", &_trk_chisquare, "trk_chisquare/F", BUFFER_SIZE );
    _resolution->Branch( "trk_ndf", &_trk_ndf, "trk_ndf/I", BUFFER_SIZE );
    _resolution->Branch( "trk_depth", &_trk_depth, "trk_depth/I", BUFFER_SIZE );
    _resolution->Branch( "trk_dg0", &_trk_dg0, "trk_dg0/F", BUFFER_SIZE );

    _resolution->Branch( "reco_x", &_reco_x, "reco_x/F", BUFFER_SIZE );
    _resolution->Branch( "reco_y", &_reco_y, "reco_y/F", BUFFER_SIZE );
    _resolution->Branch( "reco_z", &_reco_z, "reco_z/F", BUFFER_SIZE );
    _resolution->Branch( "reco_v", &_reco_v, "reco_v/F", BUFFER_SIZE );
    _resolution->Branch( "reco_w", &_reco_w, "reco_w/F", BUFFER_SIZE );
    _resolution->Branch( "reco_px", &_reco_px, "reco_px/F", BUFFER_SIZE );
    _resolution->Branch( "reco_py", &_reco_py, "reco_py/F", BUFFER_SIZE );
    _resolution->Branch( "reco_pz", &_reco_pz, "reco_pz/F", BUFFER_SIZE );

    // coordinate info
    _resolution->Branch( "coord_w", &_coord_w, "coord_w/F", BUFFER_SIZE );
    _resolution->Branch( "coord_err", &_coord_err, "coord_err/F", BUFFER_SIZE );
    _resolution->Branch( "coord_z", &_coord_z, "coord_z/F", BUFFER_SIZE );
    _resolution->Branch( "coord_status", &_coord_status, "coord_status/I", BUFFER_SIZE );

    // cluster information
    _resolution->Branch( "clus_status", &_clus_status, "clus_status/I", BUFFER_SIZE );
    _resolution->Branch( "clus_size", &_clus_size, "clus_size/I", BUFFER_SIZE );
    _resolution->Branch( "is_multi", &_is_multi, "is_multi/O", BUFFER_SIZE );
    _resolution->Branch( "is_stereo", &_is_stereo, "is_stereo/O", BUFFER_SIZE );

    _resolution->SetAutoSave( AUTO_SAVE );
    cout << "_resolution booked" << endl;

    MUTOO::PRINT( cout, "**" );
    return 0;
}
//_____________________________________
int MutooResEval::InitRun(PHCompositeNode *top_node)
{
    MUTOO::PRINT( cout, "MutooResEval::InitRun" );

    // Create Node Tree
    create_node_tree(top_node);

    MUTOO::PRINT( cout, "**" );
    return 0;
}

//_____________________________________
void MutooResEval::create_node_tree( PHCompositeNode* top_node )
{

    // Instantiate nodes for mutoo containers
    {
        PHNodeIterator nodeItr(top_node);
        _mutoo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "MUTOO"));
        if(!_mutoo_node)
        {
            _mutoo_node = new PHCompositeNode("MUTOO");
            top_node->addNode(_mutoo_node);
        }
    }

    // par module for Bend Plane fit
    mMutBPFitPar* mMutBPFit_par;
    try {
        mMutBPFit_par = TMutNode<mMutBPFitPar>::find_node( _mutoo_node, "mMutBPFitPar" );
    } catch( exception& e )
    {
        cout << "MutooResEval::create_node_tree - creating mMutBPFitPar" << endl;
        mMutBPFit_par = TMutNode<mMutBPFitPar>::new_node(_mutoo_node,"mMutBPFitPar");
        mMutBPFit_par->set_verbosity( MUTOO::NONE );
    }

    // par module for kalman filter
    try {
        _mMutKalFit_par = TMutNode<mMutKalFitPar>::find_node( _mutoo_node, "mMutKalFitPar" );
    } catch( exception& e )
    {
        cout << "MutooResEval::create_node_tree - creating mMutKalFitPar" << endl;
        _mMutKalFit_par = TMutNode<mMutKalFitPar>::new_node(_mutoo_node,"mMutKalFitPar");
        _mMutKalFit_par->set_verbosity( MUTOO::NONE );
        _mMutKalFit_par->set_use_muid( get_flag( USE_MUID ) );
    }

    // par module for straight track fit
    try {
        _mMutStraightFit_par = TMutNode<mMutStraightFitPar>::find_node( _mutoo_node, "mMutStraightFitPar" );
    } catch( exception& e )
    {
        cout << "MutooResEval::create_node_tree - creating mMutKalFitPar" << endl;
        _mMutStraightFit_par = TMutNode<mMutStraightFitPar>::new_node(_mutoo_node,"mMutStraightFitPar");
        _mMutStraightFit_par->set_verbosity( MUTOO::NONE );
        _mMutStraightFit_par->set_use_muid( get_flag( USE_MUID ) );
    }

}

//_____________________________________
int MutooResEval::process_event(PHCompositeNode *top_node)
{
    _timer.get()->restart();
    try {

        // get local pointers to needed nodes
        set_interface_ptrs( top_node );

        // loop over station/gap/cathode
        if( get_flag( REFIT ) )
        {

            // in "USE_MUID" mode, one first need to refit the full track to properly add the muid clusters
            if( get_flag( USE_MUID ) )
            {
                if( get_flag( MAGNETS_ON ) ) _mMutKalFitMod.event( top_node );
                else _mMutStraightFitMod.event( top_node );

            }

            // loop over cathodes in the track
            for( int station=0; station < MUTOO::NumberOfStations; station++ )
                for( int gap=0; gap < MUTOO::NumberOfGaps; gap++ )
                for( int cathode=0; cathode < MUTOO::NumberOfCathodePlanes; cathode++ )
            {

                /*
                refit tracks
                Bend plane refit must be done since it seeds the kalman filter and gets erased by it
                */
                _mMutBPFitMod.event( top_node );

                if( get_flag( MAGNETS_ON ) )
                {

                    // desactivate cathode on both arms
                    _mMutKalFit_par->set_mutr_desactivated( MUTOO::North, station, gap, cathode );
                    _mMutKalFit_par->set_mutr_desactivated( MUTOO::South, station, gap, cathode );
                    _mMutKalFitMod.event( top_node );

                    fill_tree();

                    // reactivate
                    _mMutKalFit_par->set_mutr_activated( MUTOO::North, station, gap, cathode );
                    _mMutKalFit_par->set_mutr_activated( MUTOO::South, station, gap, cathode );

                } else {

                    // desactivate cathode on both arms
                    _mMutStraightFit_par->set_mutr_desactivated(  MUTOO::North, station, gap, cathode );
                    _mMutStraightFit_par->set_mutr_desactivated(  MUTOO::South, station, gap, cathode );
                    _mMutStraightFitMod.event( top_node );

                    fill_tree();

                    // reactivate
                    _mMutStraightFit_par->set_mutr_activated(  MUTOO::North, station, gap, cathode );
                    _mMutStraightFit_par->set_mutr_activated(  MUTOO::South, station, gap, cathode );

                }

            }

            if( get_flag( USE_MUID ) )
            {
                for( int plane = 0; plane < MUIOO::MAX_PLANE; plane++ )
                    for( int panel = 0; panel < MUIOO::MAX_PANEL; panel++ )
                {

                    if( get_flag( MAGNETS_ON ) )
                    {

                        // desactivate cathode on both arms
                        _mMutKalFit_par->set_muid_desactivated( MUTOO::North, plane, panel );
                        _mMutKalFit_par->set_muid_desactivated( MUTOO::South, plane, panel );
                        _mMutKalFitMod.event( top_node );

                        fill_tree();

                        // reactivate
                        _mMutKalFit_par->set_muid_activated( MUTOO::North, plane, panel );
                        _mMutKalFit_par->set_muid_activated( MUTOO::South, plane, panel );

                    } else {

                        // desactivate cathode on both arms
                        _mMutStraightFit_par->set_muid_desactivated(  MUTOO::North, plane, panel );
                        _mMutStraightFit_par->set_muid_desactivated(  MUTOO::South, plane, panel );
                        _mMutStraightFitMod.event( top_node );

                        fill_tree();

                        // reactivate
                        _mMutStraightFit_par->set_muid_activated(  MUTOO::North, plane, panel );
                        _mMutStraightFit_par->set_muid_activated(  MUTOO::South, plane, panel );

                    }

                }
            }

        } else fill_tree();

    } catch( exception& e )  { cout << e.what() << endl; }
    _timer.get()->stop();

    return 0;

};

//________________________________________________________
void MutooResEval::fill_tree( void )
{

    // loop over tracks
    TMutTrkMap::iterator iter( _trk_map->range() );
    while( TMutTrkMap::const_pointer trk_ptr = iter.next() )
    {

        // basic checks on track
        if( !accept_trk( trk_ptr ) ) continue;

        // get track parameters
        const std::vector<TMutTrkPar> &trk_par_vect( *trk_ptr->get()->get_trk_par_list() );
        if( !trk_par_vect.size() ) continue;

	//        cout
	//            << "MutooResEval::fill_tree -"
	//            << " coordinates:" <<  trk_ptr->get()->get_associated<TMutCoord>().count()
	//            << " track parameters: " << trk_par_vect.size() << endl;

        // fill track quality
        _trk_chisquare = trk_ptr->get()->get_w_chi_square();
        _trk_ndf = trk_ptr->get()->get_ndf();

        // fill track hits per station
        _hits_station.assign(0);
        {
            // get associated coordinates
            TMutCoordMap::key_iterator coord_iter( trk_ptr->get()->get_associated<TMutCoord>() );
            while( TMutCoordMap::pointer coord_ptr = coord_iter.next() )
            { _hits_station[coord_ptr->get()->get_station()]++; }

        }

        // retrieve associated roads
        _trk_depth = 0;
        _trk_dg0 = -1;
        TMuiRoadMapO::const_key_iterator road_iter = trk_ptr->get()->get_associated<TMuiRoadO>();
        while( TMuiRoadMapO::const_pointer road_ptr = road_iter.next() )
        {
            double dg0( get_dg0( *trk_ptr, *road_ptr ) );
            if( _trk_dg0 < 0 || dg0 < _trk_dg0 ) {
                _trk_dg0 = dg0;
                _trk_depth = road_ptr->get()->get_depth();
            }
        }

        // get associated coordinates
        TMutCoordMap::const_key_iterator coord_iter( trk_ptr->get()->get_associated<TMutCoord>() );
        while( TMutCoordMap::const_pointer coord_ptr = coord_iter.next() )
        {

            // check if coordinate is desactivated
            bool accepted = true;
            if( get_flag( REFIT ) )
            {

                if( get_flag( MAGNETS_ON ) )
                {

                    accepted = _mMutKalFit_par->get_mutr_desactivated(
                        coord_ptr->get()->get_arm(),
                        coord_ptr->get()->get_station(),
                        coord_ptr->get()->get_gap(),
                        coord_ptr->get()->get_cathode() );

                } else {

                    accepted = _mMutStraightFit_par->get_mutr_desactivated(
                        coord_ptr->get()->get_arm(),
                        coord_ptr->get()->get_station(),
                        coord_ptr->get()->get_gap(),
                        coord_ptr->get()->get_cathode() );

                }

            }

            if( !accepted ) continue;

            // fill locator
            _is_muid = false;
            _arm = coord_ptr->get()->get_arm();
            _station =  coord_ptr->get()->get_station();
            _octant = coord_ptr->get()->get_octant();
            _half = coord_ptr->get()->get_half_octant();
            _gap =  coord_ptr->get()->get_gap();
            _cathode =  coord_ptr->get()->get_cathode();

            _plane = 0;
            _panel = 0;
            _orientation = 0;

            // fill coord quality
            _coord_w = coord_ptr->get()->get_w_absolute();
            _coord_err = coord_ptr->get()->get_error();
            _coord_z = coord_ptr->get()->get_mean_z();
            _coord_status = coord_ptr->get()->get_status();

            _is_multi = is_multi_cluster( coord_ptr );
            _is_stereo = coord_ptr->get()->get_stereo();

            // retrieve associated cluster
            TMutClusMap::key_iterator clus_iter = coord_ptr->get()->get_associated<TMutClus>();
            _clus_size = ( clus_iter.count() ) ? clus_iter->get()->get_n_strip():0;
            _clus_status = ( clus_iter.count() ) ? clus_iter->get()->get_status():0;

            // get closest track parameters
            const TMutTrkPar trk_par(
                *min_element(
                trk_par_vect.begin(), trk_par_vect.end(), closest_z_ftor( coord_ptr->get()->get_mean_z() ) ) );

            _reco_x = trk_par.get_x();
            _reco_y = trk_par.get_y();
            _reco_z = trk_par.get_z();
            _reco_px = trk_par.get_px();
            _reco_py = trk_par.get_py();
            _reco_pz = trk_par.get_pz();

            // store projection matrix from state vector to measurement
            const double angle( TMutGeo::get_cathode_angle( coord_ptr->get()->get_arm(), coord_ptr->get()->get_station(), coord_ptr->get()->get_octant(), coord_ptr->get()->get_half_octant(), coord_ptr->get()->get_gap(), coord_ptr->get()->get_cathode(), coord_ptr->get()->get_peak_strip()) );
            const double cos_phi( cos( angle ) );
            const double sin_phi( sin( angle ) );

            _reco_w = -sin_phi*_reco_x + cos_phi*_reco_y;
            _reco_v = cos_phi*_reco_x - sin_phi*_reco_y;

            // fill tree
            _resolution->Fill();
        }

        // add muid hits if requested
        if( get_flag( USE_MUID ) )
        {

            // get associated coordinates
            TMuiClusterMapO::key_iterator clus_iter( trk_ptr->get()->get_associated<TMuiClusterO>() );
            while( TMuiClusterMapO::pointer clus_ptr = clus_iter.next() )
            {

                // check if coordinate is desactivated
                bool accepted = true;
                if( get_flag( REFIT ) )
                {

                    if( get_flag( MAGNETS_ON ) )
                    {

                        accepted = _mMutKalFit_par->get_muid_desactivated(
                            clus_ptr->get()->get_arm(),
                            clus_ptr->get()->get_plane(),
                            clus_ptr->get()->get_panel() );

                    } else {

                        accepted = _mMutStraightFit_par->get_muid_desactivated(
                            clus_ptr->get()->get_arm(),
                            clus_ptr->get()->get_plane(),
                            clus_ptr->get()->get_panel() );

                }

            }

            if( !accepted ) continue;

            // fill locator
            _is_muid = true;
            _arm = clus_ptr->get()->get_arm();
            _station =  0;
            _octant = 0;
            _half = 0;
            _gap =  0;
            _cathode = 0;

            _plane = clus_ptr->get()->get_plane();
            _panel = clus_ptr->get()->get_panel();
            _orientation = clus_ptr->get()->get_orientation();

            // fill coord quality
            _coord_w = clus_ptr->get()->get_w_absolute();
            _coord_err = clus_ptr->get()->get_error();
            _coord_z = clus_ptr->get()->get_mean_z();
            _coord_status = 0;

            _is_multi = false;
            _is_stereo = false;

            // retrieve associated hits
            TMuiHitMapO::key_iterator hit_iter = clus_ptr->get()->get_associated<TMuiHitO>();
            _clus_size = hit_iter.count();
            _clus_status = 0;

            // get closest track parameters
            const TMutTrkPar trk_par(
                *min_element(
                trk_par_vect.begin(), trk_par_vect.end(), closest_z_ftor( clus_ptr->get()->get_mean_z() ) ) );

            _reco_x = trk_par.get_x();
            _reco_y = trk_par.get_y();
            _reco_z = trk_par.get_z();
            _reco_px = trk_par.get_px();
            _reco_py = trk_par.get_py();
            _reco_pz = trk_par.get_pz();

            // get the angle
            const double angle( TMuiGeo::get_panel_angle( clus_ptr->get()->get_location() ) );
            const double cos_phi( cos( angle ) );
            const double sin_phi( sin( angle ) );
            _reco_w = -sin_phi*_reco_x + cos_phi*_reco_y;
            _reco_v = cos_phi*_reco_x - sin_phi*_reco_y;

            // fill tree
            _resolution->Fill();
        }


        }

    }

}

//___________________________________________________________
bool MutooResEval::accept_trk( TMutTrkMap::const_pointer trk_ptr ) const
{

    // require track is reconstructed and non ghost
    if( !trk_ptr->get()->get_reco_success()|| trk_ptr->get()->get_ghost() )
    { return false; }

    // check stubs if required
    if( !(
        trk_ptr->get()->has_stub(MUTOO::Station1) &&
        trk_ptr->get()->has_stub(MUTOO::Station2) &&
        trk_ptr->get()->has_stub(MUTOO::Station3) ) )
    { return false; }

    // all checks passed
    return true;

}

//__________________________________________
void MutooResEval::set_interface_ptrs( PHCompositeNode *top_node )
{
    try {
        _trk_map = TMutNode<TMutTrkMap>::find_node(top_node,"TMutTrkMap");
    } catch( exception& e )
    { cout << e.what() << endl; }
}

//________________________________________________
int MutooResEval::End( PHCompositeNode *top_node )
{
  //    _timer.get()->print_stat();
    PHTFileServer::get().write( _file_name );
    return 0;
}

//__________________________________________________________________________
double MutooResEval::get_dg0( const TMutTrkMap::value_type& track, const TMuiRoadMapO::value_type& road ) const
{

    // retrieve last track parameters
    const TMutTrkPar* trk_par =
        ( track.get()->get_trk_par_list() && track.get()->get_trk_par_list()->size() ) ?
        &track.get()->get_trk_par_list()->back():
        0;
    if( !trk_par ) return -1;

    // retrieve track point at gap 0
    PHPoint gap0_point( road.get()->get_gap0_point() );

    // get track point extrapolated at this z
    TMutFitPar fit_par(
        trk_par->get_x(),
        trk_par->get_y(),
        trk_par->get_z(),
        trk_par->get_px()/trk_par->get_pz(),
        trk_par->get_py()/trk_par->get_pz() );

    PHPoint track_point( TMutTrackUtil::linear_track_model( &fit_par, gap0_point.getZ() ) );

    return gap0_point.distanceToPoint( track_point );

}

//_________________________________________________________________
bool MutooResEval::is_multi_cluster( TMutCoordMap::const_pointer coord_ptr ) const
{

    int out(0);

    // retrieve associated clusters
    TMutClusMap::const_key_iterator clus_iter = coord_ptr->get()->get_associated<TMutClus>();
    while( TMutClusMap::const_pointer clus_ptr = clus_iter.next() )
    { out += clus_ptr->get()->get_associated<TMutCoord>().count(); }

    return (out > 1 );

}
