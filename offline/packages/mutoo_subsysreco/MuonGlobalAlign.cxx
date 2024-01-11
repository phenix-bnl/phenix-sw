// $Id: MuonGlobalAlign.cxx,v 1.33 2013/10/08 21:59:41 phnxbld Exp $

/*!
\file  MuonGlobalAlign.cxx
\brief   muon global alignment module
\author  Hugo Pereira/Catherine Silvestre
\version $Revision: 1.33 $
\date  $Date: 2013/10/08 21:59:41 $
*/

#include "MuonGlobalAlign.h"
#include <recoConsts.h>
#include <Fun4AllReturnCodes.h>
#include <PHTFileServer.h>

#include <TMuiAlign.h>
#include <TMuiClusterMapO.h>

#include <TMutAlign.h>

#include <TMutCoordMap.h>
#include <TMutGapCoordMap.h>
#include <TMutGeo.h>
#include <TMutHitMap.h>

#include <TMutNode.h>
#include <TMutTrkMap.h>

#include <TMutVtxMap.h>

#include <TMuiGeo.h>

#include <TMutAlignmentCorrection.h>

#include <TMath.h>
#include <fstream>
#include <cassert>

using namespace std;
using namespace MILLEPEDE;

//______________________________________________________
MuonGlobalAlign::MuonGlobalAlign( const char* name ):
    MuonSubsysReco( name ),

    // maps
    _trk_map( 0 ), _cluster_map( 0 ), _coord_map( 0 ),
    // indexes
    _n_tracks_total( 0 ),

    // filenames
    _scratch_filename( "./scratch" ),
    _n_std_dev( 1 ),
    _par_init( false ),

    // default Alignment flags
    _flags( ALIGN_W|ALIGN_MUTR|ALIGN_MUID|ITERATE|DO_ALIGNMENT|DO_EVALUATION ),

    _output_misalignment( "alignment_corrections_out.txt" ),
    _dumpfile_name( "millepede.log" ),

    // evaluation
    _evaluation_filename( "MuonGlobalAlign.root" ),
    _evaluation_tree( 0 ),
    _timer( PHTimeServer::get()->insert_new( name )),
    _alignment_filename( "MuonGlobalAlign.root" ),
    _alignment_tree( 0 )
{

    MUTOO::TRACE( "MuonGlobalAlign::MuonGlobalAlign" );

    set_flag( MAGNETS_ON, false );
    set_flag( USE_CUTS, false );

    // initialize number of tracks/detector
    _n_tracks.assign( 0 );

}

//______________________________________________________
int MuonGlobalAlign::Init(PHCompositeNode *top_node)
{

    // call base class initialization
    // this is needed to get the module row (in list of registered modules) set properly
    MuonSubsysReco::Init( top_node );

    MUTOO::PRINT( cout, "MuonGlobalAlign::Init" );

    for (int i=0; i<MILLEPEDE::NGLB; i++) _par[i] = 0.0;

    // initialize reconstruction modules
    createNodeTree( top_node );

    // initialize evaluation ntuple
    if( get_flag( DO_EVALUATION ) ) initialize_evaluation_tree();

    // initialize parameters for Millepede
    if( get_flag( DO_ALIGNMENT ) )
    {


        initialize_alignment_tree();
        init_parameters( 1 , true );
        init_minimize();

        // Fix parameters for MuTr and alignment
        if( get_flag( ALIGN_MUTR ) )
        {

            if( _fixed_mutr_detectors.empty() ) fix_mutr_2gaps();

        } else {

            fix_mutr_all();

        }

        // fix station2 gap2 in both arms since the detector does not exists
        fix_mutr_gap( 0, 2, 2, ALL );
        fix_mutr_gap( 1, 2, 2, ALL );

        register_fixed_detectors();
        print_fixed_parameters( cout );

    }

    MUTOO::PRINT( cout, "**" );
    return 0;
}

//______________________________________________________
int MuonGlobalAlign::process_event(PHCompositeNode *top_node)
{
    if( verbosity >= 1 ) MUTOO::TRACE( "MuonGlobalAlign::process_event" );

    _timer.get()->restart();
    try {

        // get pointers to needed map
        set_interface_pointers( top_node );

        // clear alignment par map
        if( !get_flag( READ_MEM ) )
        { _align_par_map->clear(); }

        // fill evaluation tree
        if( !get_flag( READ_MEM ) && get_flag( DO_EVALUATION ) )
        { fill_evaluation_tree(); }

        // minimization
        if( get_flag( DO_ALIGNMENT ) )
        { minimize(); }

    } catch (exception &e ) {

        cout << e.what() << endl;

    }

    // needed to make sure maps are always written whatever SubsysReco are in the macro
    MuonSubsysReco::write_maps_if_needed();

    _timer.get()->stop();

    return EVENT_OK;
}

//______________________________________________________
int MuonGlobalAlign::End(PHCompositeNode *top_node)
{
    MUTOO::PRINT( cout, "MuonGlobalAlign::End" );

    // print timing statistics
//     _timer.get()->print_stat();

    // end minimize
    if( get_flag( DO_ALIGNMENT ) )
    {
        end_minimize();
        print_to_file( _dumpfile_name );
        TMutAlignmentCorrection alignmentCorrections;

        // set flags
        alignmentCorrections.set_z_alignment_enabled( get_flag(ALIGN_Z) );
        alignmentCorrections.set_mutr_alignment_enabled( get_flag(ALIGN_MUTR) );
        alignmentCorrections.set_muid_alignment_enabled( get_flag(ALIGN_MUID) );

        // read from tree, write output to text file
        alignmentCorrections.initialize( _alignment_tree, _output_misalignment );
    }

    // close evaluation tfile
    if( get_flag( DO_EVALUATION ) ) PHTFileServer::get().write( _evaluation_filename );
    if( get_flag( DO_ALIGNMENT ) ) PHTFileServer::get().write( _alignment_filename );
    MUTOO::PRINT( cout, "**" );

    return 0;

}

//______________________________________________________
int MuonGlobalAlign::createNodeTree( PHCompositeNode* top_node )
{
    MUTOO::PRINT( cout, "MuonGlobalAlign::createNodeTree" );

    // Instantiate nodes for muioo containers
    PHCompositeNode* muioo_node(0);
    {
        PHNodeIterator nodeItr(top_node);
        muioo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "MUIOO"));
        if(!muioo_node)
        {
            cout << "MuonGlobalAlign::createNodeTree - adding MUIOO node" << endl;
            muioo_node = new PHCompositeNode("MUIOO");
            top_node->addNode(muioo_node);
        }
    }

    // Instantiate nodes for mutoo containers
    PHCompositeNode* mutoo_node(0);
    {
        PHNodeIterator nodeItr(top_node);
        mutoo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "MUTOO"));
        if(!mutoo_node)
        {
            cout << "MuonGlobalAlign::createNodeTree - adding MUTOO node" << endl;
            mutoo_node = new PHCompositeNode("MUTOO");
            top_node->addNode(mutoo_node);
        }
    }

    // instanciate DST node
    PHCompositeNode* dst_node( 0 );
    {
        PHNodeIterator nodeItr(top_node);
        dst_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "DST"));
        if (!dst_node)
        {
            cout << "MuonGlobalAlign::createNodeTree - adding DST node" << endl;
            dst_node = new PHCompositeNode("DST");
            top_node->addNode(dst_node);
        }
    }

    // alignment parameter node
    if( !get_flag( READ_MEM ) )
    {

        // create new AlignPar node and make persistent
        TMutNode<TMutAlignParMap>::new_node( mutoo_node, "TMutAlignParMap")->make_persistant(dst_node,"TMutAlignPar");

    }


    MUTOO::PRINT( cout, "MuonGlobalAlign::createNodeTree - done." );
    return 0;

}

//______________________________________________________
void MuonGlobalAlign::initialize_evaluation_tree( void )
{
    MUTOO::TRACE( "MuonGlobalAlign::initialize_evaluation_tree" );

    // open TFile
    PHTFileServer::get().open( _evaluation_filename, "RECREATE" );

    // create tree (for now the tree is empty)
    _evaluation_tree = new TTree( "alignment", "global alignment tree" );

    // track related variables

    _evaluation_tree->Branch("arm", &_arm, "arm/I", BUFFER_SIZE);
    _evaluation_tree->Branch("chi_square", &_chi_square, "chi_square/D", BUFFER_SIZE);
    _evaluation_tree->Branch("prob", &_prob, "prob/D", BUFFER_SIZE);
    _evaluation_tree->Branch("ndf", &_ndf, "ndf/I", BUFFER_SIZE);
    _evaluation_tree->Branch("chi_square_vtx", &_chi_square_vtx, "chi_square_vtx/D", BUFFER_SIZE);
    _evaluation_tree->Branch("prob_vtx", &_prob_vtx, "prob_vtx/D", BUFFER_SIZE);
    _evaluation_tree->Branch("ndf_vtx", &_ndf_vtx, "ndf_vtx/I", BUFFER_SIZE);
    _evaluation_tree->Branch("accept_trk", &_accept_trk, "accept_trk/I", BUFFER_SIZE);
    _evaluation_tree->Branch("theta", &_theta[0], "theta[18]/F", BUFFER_SIZE);

    // muon tracker variables
    _evaluation_tree->Branch("octant", &_octant, "octant/I", BUFFER_SIZE);
    _evaluation_tree->Branch("half_octant", &_half_octant[0], "half_octant[18]/I", BUFFER_SIZE);

    _evaluation_tree->Branch("mutr_coords", &_mutr_coords, "mutr_coords/I", BUFFER_SIZE);
    _evaluation_tree->Branch("flag", &_flag[0], "flag[18]/I", BUFFER_SIZE);
    _evaluation_tree->Branch("x_fit", &_x_fit[0], "x_fit[18]/D", BUFFER_SIZE);
    _evaluation_tree->Branch("y_fit", &_y_fit[0], "y_fit[18]/D", BUFFER_SIZE);
    _evaluation_tree->Branch("x_not_extrapo", &_x_not_extrapo[0], "x_not_extrapo[18]/D", BUFFER_SIZE);
    _evaluation_tree->Branch("y_not_extrapo", &_y_not_extrapo[0], "y_not_extrapo[18]/D", BUFFER_SIZE);
    _evaluation_tree->Branch("v_fit", &_v_fit[0], "v_fit[18]/D", BUFFER_SIZE);
    _evaluation_tree->Branch("w_fit", &_w_fit[0], "w_fit[18]/D", BUFFER_SIZE);
    _evaluation_tree->Branch("z_fit", &_z_fit[0], "z_fit[18]/D", BUFFER_SIZE);
    _evaluation_tree->Branch("w_det", &_w_det[0], "w_det[18]/D", BUFFER_SIZE);
    _evaluation_tree->Branch("z_det", &_z_det[0], "z_det[18]/D", BUFFER_SIZE);
    _evaluation_tree->Branch("residu", &_residu[0], "residu[18]/D", BUFFER_SIZE);
    _evaluation_tree->Branch("hit", &_hit, "hit/I", BUFFER_SIZE);

    // muon identifier variables
    _evaluation_tree->Branch("plane_eval", &_plane_eval[0], "plane_eval[60]/I", BUFFER_SIZE);
    _evaluation_tree->Branch("panel_eval", &_panel_eval[0], "panel_eval[60]/I", BUFFER_SIZE);
    _evaluation_tree->Branch("orientation_eval", &_orientation_eval[0], "orientation_eval[60]/I", BUFFER_SIZE);
    _evaluation_tree->Branch("detector_index", &_detector_index, "detector_index/I", BUFFER_SIZE);
    _evaluation_tree->Branch("muid_clusters", &_muid_clusters, "muid_clusters/I", BUFFER_SIZE);
    _evaluation_tree->Branch("muid_flag", &_muid_flag[0], "muid_flag[60]/I", BUFFER_SIZE);
    _evaluation_tree->Branch("muid_w_det", &_muid_w_det[0], "muid_w_det[60]/D", BUFFER_SIZE);
    _evaluation_tree->Branch("muid_z_det", &_muid_z_det[0], "muid_z_det[60]/D", BUFFER_SIZE);
    _evaluation_tree->Branch("muid_x_fit", &_muid_x_fit[0], "muid_x_fit[60]/D", BUFFER_SIZE);
    _evaluation_tree->Branch("muid_y_fit", &_muid_y_fit[0], "muid_y_fit[60]/D", BUFFER_SIZE);
    _evaluation_tree->Branch("muid_v_fit", &_muid_v_fit[0], "muid_v_fit[60]/D", BUFFER_SIZE);
    _evaluation_tree->Branch("muid_w_fit", &_muid_w_fit[0], "muid_w_fit[60]/D", BUFFER_SIZE);
    _evaluation_tree->Branch("muid_z_fit", &_muid_z_fit[0], "muid_z_fit[60]/D", BUFFER_SIZE);
    _evaluation_tree->Branch("muid_residu", &_muid_residu[0], "muid_residu[60]/D", BUFFER_SIZE);
    _evaluation_tree->SetAutoSave( AUTO_SAVE );


}

//______________________________________________________
void MuonGlobalAlign::initialize_alignment_tree( void )
{
    MUTOO::TRACE( "MuonGlobalAlign::initialize_alignment_tree" );

    // open TFile
    PHTFileServer::get().open( _alignment_filename, "RECREATE" );

    // create tree (for now the tree is empty)

    _alignment_tree = new TTree( "misalignment", " misalignments parameters" );
    _alignment_tree->Branch("delta_x", &_delta_x, "delta_x/D", BUFFER_SIZE);
    _alignment_tree->Branch("delta_y", &_delta_y, "delta_y/D", BUFFER_SIZE);
    _alignment_tree->Branch("delta_z", &_delta_z, "delta_z/D", BUFFER_SIZE);
    _alignment_tree->Branch("delta_w", &_delta_w, "delta_w/D", BUFFER_SIZE);
    _alignment_tree->Branch("delta_phi", &_delta_phi, "delta_phi/D", BUFFER_SIZE);
    _alignment_tree->Branch("delta_w_millepede", &_delta_w_millepede, "delta_w_millepede/D", BUFFER_SIZE);
    _alignment_tree->Branch("delta_z_millepede", &_delta_z_millepede, "delta_z_millepede/D", BUFFER_SIZE);
    _alignment_tree->Branch("delta_phi_millepede", &_delta_phi_millepede,"delta_phi_millepede/D", BUFFER_SIZE);
    _alignment_tree->Branch("delta_x_millepede", &_delta_x_millepede, "delta_x_millepede/D", BUFFER_SIZE);
    _alignment_tree->Branch("delta_y_millepede", &_delta_y_millepede, "delta_y_millepede/D", BUFFER_SIZE);

    _alignment_tree->Branch("arm", &_arm, "arm/I", BUFFER_SIZE);
    _alignment_tree->Branch("station", &_station, "station/I", BUFFER_SIZE);
    _alignment_tree->Branch("gap", &_gap, "gap/I", BUFFER_SIZE);
    _alignment_tree->Branch("octant", &_octant, "octant/I", BUFFER_SIZE);
    _alignment_tree->Branch("half", &_half, "half/I", BUFFER_SIZE);
    _alignment_tree->Branch("cathode", &_cathode, "cathode/I", BUFFER_SIZE);

    _alignment_tree->Branch("error_w", &_error_w, "error_w/D", BUFFER_SIZE);
    _alignment_tree->Branch("error_z", &_error_z, "error_z/D", BUFFER_SIZE);
    _alignment_tree->Branch("error_phi", &_error_phi, "error_phi/D", BUFFER_SIZE);
    _alignment_tree->Branch("error_x", &_error_x, "error_x/D", BUFFER_SIZE);
    _alignment_tree->Branch("error_y", &_error_y, "error_y/D", BUFFER_SIZE);
    _alignment_tree->Branch("nb_tracks", &_nb_tracks, "nb_tracks/I", BUFFER_SIZE);
    _alignment_tree->Branch("angle", &_angle,"angle/D", BUFFER_SIZE);

    _alignment_tree->Branch("is_muid", &_is_muid, "is_muid/I", BUFFER_SIZE);
    _alignment_tree->Branch("plane", &_plane, "plane/I", BUFFER_SIZE);
    _alignment_tree->Branch("panel", &_panel, "panel/I", BUFFER_SIZE);
    _alignment_tree->Branch("orientation", &_orientation, "orientation/I", BUFFER_SIZE);
    _alignment_tree->SetAutoSave( AUTO_SAVE );

}

//______________________________________________________
void MuonGlobalAlign::set_interface_pointers( PHCompositeNode* top_node )
{

    if( verbosity >= 1 ) MUTOO::TRACE( "MuonGlobalAlign::set_interface_pointers" );

    _align_par_map = TMutNode<TMutAlignParMap>::find_node( top_node, "TMutAlignParMap" );

    if( !get_flag( READ_MEM ) )
    {

        // retrieve track map
        _trk_map = TMutNode<TMutTrkMap>::find_node( top_node, "TMutTrkMap" );

        // retrieve cluster map
        _cluster_map = TMutNode<TMuiClusterMapO>::find_node( top_node, "TMuiClusterMapO" );

        // retrieve coordinate map
        _coord_map = TMutNode<TMutCoordMap>::find_node( top_node, "TMutCoordMap" );

    }
}

//______________________________________________________
bool MuonGlobalAlign::init_parameters( int n_std_dev, bool dump )
{
    cout << "MuonGlobalAlign::init_parameters" << endl;
    if( _par_init ) {
        cout << "MuonGlobalAlign::init_parameters - already initialized" << endl;
        return true;
    }

    // define total number of detectors
    _nb_det = _nb_mutr_det;
    if( get_flag( ALIGN_MUID ) ) _nb_det += _nb_muid_det;
    cout << "MuonGlobalAlign::init_parameters - _nb_det=" << _nb_det << endl;
    assert( _nb_det <= NPLAN );

    /*
    initialize millepede
    NPARPLAN is defined in millepede.h
    */
    _n_std_dev = n_std_dev;
    C_INITGL( _nb_det*NPARPLAN, NPARTRK, _n_std_dev, int(dump)-1);

    _par_init = true;
    return true;
}

//______________________________________________________
void MuonGlobalAlign::fix_mutr_cathode( int arm, int station, int gap, int cathode, unsigned int flags )
{

    cout << "MuonGlobalAlign::fix_mutr_cathode - [" << arm << "," << station << "," << gap << "," << cathode << "] flags:" << flags << endl;

    Alignment::MutrDetId id( arm, station, gap, cathode );
    Alignment::MutrDetId::Map::iterator iter( _fixed_mutr_detectors.find( id ) );
    if( iter == _fixed_mutr_detectors.end() ) { _fixed_mutr_detectors.insert( make_pair( id, flags ) ); }
    else { iter->second |= flags; }

}


//______________________________________________________
void MuonGlobalAlign::fix_muid_plane( int arm, int plane, int orientation, unsigned int flags )
{
    Alignment::MuidDetId id( arm, plane, orientation );
    Alignment::MuidDetId::Map::iterator iter( _fixed_muid_detectors.find( id ) );
    if( iter == _fixed_muid_detectors.end() ) { _fixed_muid_detectors.insert( make_pair( id, flags ) ); }
    else { iter->second |= flags; }
}

//______________________________________________________
void MuonGlobalAlign::register_fixed_detectors( void )
{

    MUTOO::TRACE( "MuonGlobalAlign::register_fixed_detectors" );

    cout << "MuonGlobalAlign::register_fixed_detectors - mutr: " << _fixed_mutr_detectors.size() << endl;
    cout << "MuonGlobalAlign::register_fixed_detectors - muid: " << _fixed_muid_detectors.size() << endl;

    // fix mutr detectors
    for( Alignment::MutrDetId::Map::iterator iter = _fixed_mutr_detectors.begin(); iter != _fixed_mutr_detectors.end(); iter++ )
    {

        fix_parameter_mutr( iter->first._arm, iter->first._station, iter->first._gap, iter->first._cathode, iter->second );

        // possibly constrain both half-octants
        if( get_flag( USE_CONSTRAINTS ) )
        {
            for( int octant = 0; octant < MUTOO::NumberOfOctants; octant++)
            { constraint_halfs( iter->first._arm, iter->first._station, iter->first._gap, iter->first._cathode, octant); }
        }

    }

    // fixed muid detectors
    for( Alignment::MuidDetId::Map::iterator iter = _fixed_muid_detectors.begin(); iter != _fixed_muid_detectors.end(); iter++ )
    { fix_parameter_muid( iter->first._arm, iter->first._plane, iter->first._orientation, iter->second ); }

}

//______________________________________________________
void MuonGlobalAlign::fix_parameter_mutr( int arm, int station, int gap, int cathode, int parameter_bit )
{
    if( !_par_init ){
        cout << "MuonGlobalAlign::fix_w - mutr parameters not initialized.\n";
        return;
    }

    for( int octant = 0; octant<MUTOO::NumberOfOctants; octant ++)
        for( int half_octant = 0; half_octant<MUTOO::NumberOfHalfOctants; half_octant ++)
    {
        int half_octant_index = get_index_half_octant( arm, station, octant, half_octant, gap, cathode ) ;
        if( parameter_bit & PAR_W ) C_PARSIG( half_octant_index*NPARPLAN+1, 0.0 );
        if( parameter_bit & PAR_Z ) C_PARSIG( half_octant_index*NPARPLAN+2, 0.0 );
        if( parameter_bit & PAR_PHI ) C_PARSIG( half_octant_index*NPARPLAN+3, 0.0 );
    }

    return;

}

//______________________________________________________
void MuonGlobalAlign::fix_parameter_muid( int arm, int plane, int orientation , int parameter_bit )
{
    if( !_par_init ){
        cout << "MuonGlobalAlign::fix_w - muid parameters not initialized.\n";
        return;
    }

    for(  int panel = 0; panel < MUIOO::MAX_PANEL ; panel ++)
    {
        int panel_index = get_index_panel ( arm, plane, panel, orientation);
        if( parameter_bit & PAR_W ) C_PARSIG( panel_index*NPARPLAN+1, 0.0 );
        if( parameter_bit & PAR_Z ) C_PARSIG( panel_index*NPARPLAN+2, 0.0 );
        if( parameter_bit & PAR_PHI ) C_PARSIG( panel_index*NPARPLAN+3, 0.0 );
    }

    return;

}

//______________________________________________________
bool MuonGlobalAlign::constraint_halfs(int arm, int station, int gap, int cath, int octant)
{

    cout << "MuonGlobalAlign::constraint_halfs " << endl;
    for(int i_par = 1;i_par<4;i_par++)
    {
        vector<float> t(_nb_det*NPARPLAN, 0);
        for( int half_octant = 0; half_octant<MUTOO::NumberOfHalfOctants; half_octant ++)
        {

            int half_octant_index = get_index_half_octant( arm, station, octant, half_octant, gap, cath ) ;

            if(half_octant == 0) t[half_octant_index*NPARPLAN+i_par]= 1;
            else t[half_octant_index*NPARPLAN+i_par]= -1;
        }
        C_CONSTF(&t[0] , 0.0 );
    }
    return true;

}

//______________________________________________________
bool MuonGlobalAlign::init_minimize( void )
{
    MUTOO::TRACE( "MuonGlobalAlign::init_minimize" );

    // Check which parameters to align
    if( !( get_flag( ALIGN_W ) || get_flag( ALIGN_Z ) || get_flag( ALIGN_PHI ) ) ) {
        cout << "MuonGlobalAlign::init_minimize - ERROR: nothing to minimize.\n";
        return false;
    } else {
        cout << "MuonGlobalAlign::init_minimize - _align_w: " << ((get_flag( ALIGN_W ))?"true":"false") << endl;
        cout << "MuonGlobalAlign::init_minimize - _align_z: " << ((get_flag( ALIGN_Z ))?"true":"false") << endl;
        cout << "MuonGlobalAlign::init_minimize - _align_phi: " << ((get_flag( ALIGN_PHI ))?"true":"false") << endl;
    }

    // fix all parameters if alignment not required
    for( int i=0; i<_nb_det; i++)
    {
        if (!get_flag( ALIGN_W )) C_PARSIG(i*NPARPLAN+1,0.0);
        if (!get_flag( ALIGN_Z )) C_PARSIG(i*NPARPLAN+2,0.0);
        if (!get_flag( ALIGN_PHI )) C_PARSIG(i*NPARPLAN+3,0.0);
    }

    // initialize global/local matres to 0
    zerloc_( &_dergb[0], &_derlc[0] );

    // tell minimization to iterate
    if( get_flag( ITERATE ) ) C_INITUN(11,10000., const_cast<char*>( _scratch_filename ) );

    // force two half octant to move together
    // C_CONSTF(2,0.0);

    return true;
}

//______________________________________________________
void MuonGlobalAlign::minimize( void )
{

    if( verbosity >= 1 ) MUTOO::TRACE( "MuonGlobalAlign::minimize" );

    if( !get_flag( READ_MEM ) )
    {

        TMutTrkMap::iterator trk_iter( _trk_map->range() );
        _n_tracks_total += trk_iter.count();
        while( TMutTrkMap::pointer trk_ptr = trk_iter.next() )
        {

            // never save ghost or reco failed tracks
            if( trk_ptr->get()->get_ghost() || !trk_ptr->get()->get_reco_success() ) continue;

            /*
            always call accept_track because some of the
            output parameters are calculated in there.
            */
            const bool accepted( accept_trk( trk_ptr ) );
            if( accepted || !get_flag( USE_CUTS ) )
            {

                // calculate derivatives
                if( get_flag( MAGNETS_ON ) ) minimize_magnets_on( trk_ptr );
                else minimize_magnets_off( trk_ptr );

                // perform local minimization
                fitloc_();

            }
        }

    } else {

        TMutAlignParMap::iterator align_par_iter = _align_par_map->range();
        _n_tracks_total += align_par_iter.count();

        while( TMutAlignParMap::pointer align_par_ptr = align_par_iter.next() )
        {

            cout
                << "MuonGlobalAlign::minimize -"
                << " type: " << (align_par_ptr->get()->get_detector_type() == TMutAlignPar::MuTr ? "MuTr":"MuId" )
                << " octant: " << align_par_ptr->get()->get_octant()
                << " chisquare: " << align_par_ptr->get()->get_trk_chi_square_ndf()
                << " vtx chisquare: " << align_par_ptr->get()->get_trk_chi_square_vtx_ndf()
                << endl;


            // check detector type
            if( align_par_ptr->get()->get_detector_type() == TMutAlignPar::MuTr && !get_flag( ALIGN_MUTR ) ) continue;
            if( align_par_ptr->get()->get_detector_type() == TMutAlignPar::MuId && !get_flag( ALIGN_MUID ) ) continue;

            // if both MuTR and MuID alignment are selected, the first parameter must be MuTr
            // Any other case means that the parameter list is corrupted
            if( get_flag( ALIGN_MUTR ) && get_flag( ALIGN_MUID ) )
            {
                assert( align_par_ptr->get()->get_detector_type() == TMutAlignPar::MuTr );
                assert( align_par_iter.current() && align_par_iter.current()->get()->get_detector_type() == TMutAlignPar::MuId );

                // check track quality
                const bool accepted( accept_trk( align_par_ptr ) );
                if( accepted || !get_flag( USE_CUTS ) )
                {
                    minimize_magnets_off( align_par_ptr );
                    minimize_magnets_off( align_par_iter.current() );
                    if( !get_flag( WRITE_MEM ) ) fitloc_();
                }

                // advance one, to skip the MuID alignment parameters that has just been processed.
                align_par_iter.next();

            }  else {

                // calculate derivatives
                const bool accepted( accept_trk( align_par_ptr ) );
                if( accepted || !get_flag( USE_CUTS ) )
                {
                    minimize_magnets_off( align_par_ptr );
                    if( !get_flag( WRITE_MEM ) ) fitloc_();
                }

            }

        }

    }

    return;

}

//______________________________________________________
void MuonGlobalAlign::minimize_magnets_off( TMutTrkMap::pointer trk_ptr )
{
    // first case - No magnetic field, straight tracks
    // Track coordinates at the detector
    if( verbosity >= 1 ) MUTOO::TRACE( "MuonGlobalAlign::minimize_magnets_off" );

    // get the fit parameters
    const std::vector<TMutTrkPar> &trk_par_vect( *trk_ptr->get()->get_trk_par_list() );
    if( !trk_par_vect.size() ) return;

    // pick up a set of track parameter, for instance we choose arbitrally le param of dect0
    double z_fit = trk_par_vect.front().get_z();
    double x_not_extrapo = trk_par_vect.front().get_x();
    double y_not_extrapo = trk_par_vect.front().get_y();

    double tx =(trk_par_vect.front().get_px())/(trk_par_vect.front().get_pz());
    double ty = (trk_par_vect.front().get_py())/(trk_par_vect.front().get_pz());

    // MuTR
    /*! in principle muid alignment parameters shoud also be stored into TMutAlignPar */
    if( get_flag( ALIGN_MUTR ) )
    {

        // create new alignment parameter
        TMutAlignParMap::iterator align_par_iter = _align_par_map->insert_new( trk_ptr->get()->get_arm() );

        // store octant number
        align_par_iter->get()->set_octant( trk_ptr->get()->get_octant() );

        // associate to track
        PHKey::associate(align_par_iter.current(), trk_ptr);
        align_par_iter->get()->set_detector_type( TMutAlignPar::MuTr );

        // retrieve coordinates
        int n_coord(0);
        TMutCoordMap::const_key_iterator coord_iter = trk_ptr->get()->get_associated<TMutCoord>();
        while( TMutCoordMap::const_pointer coord_ptr = coord_iter.next() )
        {

            // store detector index
            align_par_iter->get()->set_detector_index( n_coord, get_index_half_octant( coord_ptr->get()->get_location() ) );

            // store measurement
            align_par_iter->get()->set_w_det( n_coord, coord_ptr->get()->get_w_absolute() );
            align_par_iter->get()->set_sigma( n_coord, coord_ptr->get()->get_error() );

            // calculate local derivatives
            double angle( TMutGeo::get_cathode_angle(
                coord_ptr->get()->get_arm(),
                coord_ptr->get()->get_station(),
                coord_ptr->get()->get_octant(),
                coord_ptr->get()->get_half_octant(),
                coord_ptr->get()->get_gap(),
                coord_ptr->get()->get_cathode(),
                coord_ptr->get()->get_peak_strip()) );
            double cos_phi  = -sin( angle );
            double sin_phi  = cos( angle );
            double z_det = coord_ptr->get()->get_mean_z();

            align_par_iter->get()->set_dwdx( n_coord, cos_phi );
            align_par_iter->get()->set_dwdy( n_coord, sin_phi );
            align_par_iter->get()->set_dwdtx( n_coord, cos_phi*(z_det-z_fit) );
            align_par_iter->get()->set_dwdty( n_coord, sin_phi*(z_det-z_fit) );

            float x_fit = x_not_extrapo + tx*(z_det-z_fit);
            float y_fit = y_not_extrapo + ty*(z_det-z_fit);
            align_par_iter->get()->set_wrt_z( n_coord, cos_phi*tx+sin_phi*ty );
            align_par_iter->get()->set_wrt_phi( n_coord, -sin_phi*x_fit + cos_phi*y_fit );

            n_coord++;

        }

        align_par_iter->get()->set_n_coord( n_coord );
        align_par_iter->get()->set_trk_theta0( _mem_trk_theta0 );
        align_par_iter->get()->set_trk_chi_square_ndf( _mem_trk_chi_square_ndf );
        align_par_iter->get()->set_trk_chi_square_vtx_ndf( _mem_trk_chi_square_vtx_ndf );
        align_par_iter->get()->set_trk_st2_hit( _mem_trk_st2_hit );

        // perform alignment
        minimize_magnets_off( align_par_iter.current() );

    }

    // MuID
    /*! in principle muid alignment parameters shoud also be stored into TMutAlignPar */
    if( get_flag( ALIGN_MUID ) )
    {

        // create new alignment parameter
        TMutAlignParMap::iterator align_par_iter = _align_par_map->insert_new( trk_ptr->get()->get_arm() );

        // store octant number
        align_par_iter->get()->set_octant( trk_ptr->get()->get_octant() );

        // associate to track
        PHKey::associate(align_par_iter.current(), trk_ptr);
        align_par_iter->get()->set_detector_type( TMutAlignPar::MuId );

        // retrieve associated muid clusters
        int n_cluster(0);
        TMuiClusterMapO::key_iterator clus_iter( trk_ptr->get()->get_associated<TMuiClusterO>() );
        while( TMuiClusterMapO::pointer clus_ptr = clus_iter.next() )
        {

            // store detector index
            align_par_iter->get()->set_detector_index( n_cluster, get_index_panel( clus_ptr->get()->get_location() ) );

            // store measurement
            align_par_iter->get()->set_w_det( n_cluster, clus_ptr->get()->get_w_absolute() );
            align_par_iter->get()->set_sigma( n_cluster, clus_ptr->get()->get_error() );

            // calculate local derivatives
            double angle( TMuiGeo::get_panel_angle( clus_ptr->get()->get_location() ) );
            double cos_phi  = -sin( angle );
            double sin_phi  = cos( angle );
            double z_det = clus_ptr->get()->get_mean_z();

            // calculate local derivatives
            align_par_iter->get()->set_dwdx( n_cluster, cos_phi );
            align_par_iter->get()->set_dwdy( n_cluster, sin_phi );
            align_par_iter->get()->set_dwdtx( n_cluster, cos_phi*(z_det-z_fit) );
            align_par_iter->get()->set_dwdty( n_cluster, sin_phi*(z_det-z_fit) );


            double x_fit = x_not_extrapo + tx*(z_det-z_fit);
            double y_fit = y_not_extrapo + ty*(z_det-z_fit);
            align_par_iter->get()->set_wrt_z( n_cluster, cos_phi*tx+sin_phi*ty );
            align_par_iter->get()->set_wrt_phi( n_cluster, -sin_phi*x_fit + cos_phi*y_fit );

            n_cluster++;
        }

        // perform alignment
        align_par_iter->get()->set_n_coord( n_cluster );
        align_par_iter->get()->set_trk_theta0( _mem_trk_theta0 );
        align_par_iter->get()->set_trk_chi_square_ndf( _mem_trk_chi_square_ndf );
        align_par_iter->get()->set_trk_chi_square_vtx_ndf( _mem_trk_chi_square_vtx_ndf );
        align_par_iter->get()->set_trk_st2_hit( _mem_trk_st2_hit );


        // perform alignment
        minimize_magnets_off( align_par_iter.current() );

    }

}

//___________________________________________________________________________
void MuonGlobalAlign::minimize_magnets_off( TMutAlignParMap::pointer align_par_ptr )
{
    for (int i_coord = 0; i_coord < align_par_ptr->get()->get_n_coord(); i_coord++)
    {

        float w_det = align_par_ptr->get()->get_w_det(i_coord);
        float sigma = align_par_ptr->get()->get_sigma(i_coord);

        // local derivatives
        _derlc[0]= align_par_ptr->get()->get_dwdx(i_coord);//dwdx;
        _derlc[1]= align_par_ptr->get()->get_dwdtx(i_coord);//dwdtx;
        _derlc[2]= align_par_ptr->get()->get_dwdy(i_coord);//dwdy;
        _derlc[3]= align_par_ptr->get()->get_dwdty(i_coord);//dwdty;

        // retrieve detector index
        int index_det = align_par_ptr->get()->get_detector_index(i_coord);

        // derivative wrt w alignment parameter
        _dergb[NPARPLAN * index_det ]=-1.;

        // derivative wrt z alignment parameter
        _dergb[NPARPLAN*index_det+1]= align_par_ptr->get()->get_wrt_z(i_coord);//cos_phi*tx+sin_phi*ty;

        // derivative wrt phi alignment parameter
        //-sin_phi*x_fit + cos_phi*y_fit;
        _dergb[NPARPLAN*index_det+2]= align_par_ptr->get()->get_wrt_phi(i_coord);

        if( Verbosity() > 0 )
        {
            cout << "MuonGlobalAlign::minimize_magnets_off - derlc: " << _derlc[0] << "," << _derlc[1] << "," << _derlc[2] << "," << _derlc[3] << endl;
            cout << "MuonGlobalAlign::minimize_magnets_off - index_det: " << index_det << " _dergb: " << _dergb[NPARPLAN * index_det ] << "," << _dergb[NPARPLAN*index_det+1] << "," << _dergb[NPARPLAN*index_det+2] << endl;
            cout << "MuonGlobalAlign::minimize_magnets_off - w_det: " << w_det << " error: " << sigma << endl;
        }

        // book local/global derivatives, measurement, error
        if( !get_flag( WRITE_MEM ) ) equloc_( &_dergb[0], &_derlc[0], &w_det, &sigma);

        // increment number of 'tracks' for this detector
        _n_tracks[index_det]++;

    }

}

//______________________________________________________
void MuonGlobalAlign::minimize_magnets_on( TMutTrkMap::pointer trk_ptr )
{

    // todo: use TMutAlignPar in case of magnets_on also

    // right now the READ_MEM mode is not supported
    if( get_flag( READ_MEM ) )
    {
        cout << "MuonGlobalAlign::minimize_magnets_on - READ_MEM mode not supported" << endl;
        return;
    }

    // second case - magnetic field, bended tracks (not fitted. use staight correction to track instead)
    // Track coordinates at the detector
    if( verbosity >= 1 ) MUTOO::TRACE( "MuonGlobalAlign::minimize_magnets_on" );

    // get the fit parameters
    const std::vector<TMutTrkPar> &trk_par_vect( *trk_ptr->get()->get_trk_par_list() );
    if( !trk_par_vect.size() ) return;

    // magnetic field, bended tracks (not fitted. use staight correction to track instead)
    // reference z
    double z_ref = trk_par_vect.front().get_z();

    // retrieve coordinates
    TMutCoordMap::const_key_iterator coord_iter = trk_ptr->get()->get_associated<TMutCoord>();
    while( TMutCoordMap::const_pointer coord_ptr = coord_iter.next() )
    {
        // retrieve detector index
        int index_det( get_index_half_octant( coord_ptr->get()->get_location() ) );

        // store projection matrix from state vector to measurement
        double angle(
            TMutGeo::get_cathode_angle(
            coord_ptr->get()->get_arm(), coord_ptr->get()->get_station(), coord_ptr->get()->get_octant(), coord_ptr->get()->get_half_octant(), coord_ptr->get()->get_gap(), coord_ptr->get()->get_cathode(), coord_ptr->get()->get_peak_strip()) );

        // get the local parameters
        float z_det = coord_ptr->get()->get_mean_z();
        float w_det = coord_ptr->get()->get_w_absolute();
        float sigma = coord_ptr->get()->get_error();

        // get closest track parameters to z_det
        const TMutTrkPar trk_par( *min_element( trk_par_vect.begin(), trk_par_vect.end(), closest_z_ftor( z_det ) ) );

        // retrieve coordinate positions
        double z_fit = trk_par.get_z();
        double x_not_extrapo = trk_par.get_x();
        double y_not_extrapo = trk_par.get_y();
        double tx = trk_par.get_px()/trk_par.get_pz();
        double ty = trk_par.get_py()/trk_par.get_pz();

        // extrapolate fit coord to detector
        double x_fit = x_not_extrapo + tx*(z_det-z_fit);
        double y_fit = y_not_extrapo + ty*(z_det-z_fit);

        float cos_phi  = -sin( angle );
        float sin_phi  = cos( angle );

        float w_fit = cos_phi*x_fit + sin_phi*y_fit;
        float residu = w_det - w_fit;

        // calculate local derivatives
        _derlc[0]  = cos_phi;
        _derlc[1]  = cos_phi*(z_det-z_ref);
        _derlc[2]  = sin_phi;
        _derlc[3]  = sin_phi*(z_det-z_ref);

        // calculate global derivatives
        // derivative wrt w alignment parameter
        _dergb[NPARPLAN * index_det ]=-1.;

        // derivative wrt z alignment parameter
        _dergb[NPARPLAN*index_det+1]=  cos_phi*tx+sin_phi*ty;

        // derivative wrt phi alignment parameter
        _dergb[NPARPLAN*index_det+2]= -sin_phi*x_fit + cos_phi*y_fit;

        // increment number of 'tracks' for this detector
        _n_tracks[index_det]++;

        // book local/global derivatives, measurement, error
        equloc_( &_dergb[0], &_derlc[0], &residu, &sigma);
    }

    // MuID
    // Track coordinates at the detector

    if( get_flag( ALIGN_MUID ) )
    {

        // retrieve associated muid clusters
        TMuiClusterMapO::key_iterator clus_iter( trk_ptr->get()->get_associated< TMuiClusterO >() );

        // add muid clusters, if required
        while( TMuiClusterMapO::pointer clus_ptr = clus_iter.next() )
        {

            // get the local parameters
            // retrive cluster z
            float z_det = clus_ptr->get()->get_mean_z();

            // retrieve cluster measurement
            float w_det = clus_ptr->get()->get_w_absolute();
            float sigma = clus_ptr->get()->get_error();
            double angle( TMuiGeo::get_panel_angle( clus_ptr->get()->get_location() ) );

            // get closest track parameters to z_det
            const TMutTrkPar trk_par( *min_element( trk_par_vect.begin(), trk_par_vect.end(), closest_z_ftor( z_det ) ) );

            // retrieve coordinate positions
            double z_fit = trk_par.get_z();
            double x_not_extrapo = trk_par.get_x();
            double y_not_extrapo = trk_par.get_y();
            double tx = trk_par.get_px()/trk_par.get_pz();
            double ty = trk_par.get_py()/trk_par.get_pz();

            // pick up a set of track parameter, for instance we choose arbitrally le param of dect0
            float x_fit = x_not_extrapo + tx*(z_det-z_fit);
            float y_fit = y_not_extrapo + ty*(z_det-z_fit);
            float cos_phi  = -sin( angle );
            float sin_phi  = cos( angle );

            float w_fit = cos_phi*x_fit + sin_phi*y_fit;
            float residu = w_det - w_fit;

            // calculate local derivatives
            float dwdx  = cos_phi;
            float dwdy  = sin_phi;
            float dwdtx = cos_phi*(z_det-z_fit);
            float dwdty = sin_phi*(z_det-z_fit);

            // local derivatives
            _derlc[0]= dwdx;
            _derlc[1]= dwdtx;
            _derlc[2]= dwdy;
            _derlc[3]= dwdty;

            // retrieve detector index
            int index_det( get_index_panel( clus_ptr->get()->get_location() ) );

            // derivative wrt w alignment parameter
            _dergb[NPARPLAN * index_det ]=-1.;

            // derivative wrt z alignment parameter
            _dergb[NPARPLAN*index_det+1]=  cos_phi*tx+sin_phi*ty;

            // derivative wrt phi alignment parameter
            _dergb[NPARPLAN*index_det+2]= -sin_phi*x_fit + cos_phi*y_fit;

            // book local/global derivatives, measurement, error
            equloc_( &_dergb[0], &_derlc[0], &residu, &sigma);

            // increment number of 'tracks' for this detector
            _n_tracks[index_det]++;
        }
    }

}

//______________________________________________________
void MuonGlobalAlign::end_minimize( )
{

    if( !get_flag( WRITE_MEM ) )
    {
        MUTOO::TRACE( "MuonGlobalAlign::end_minimize, fitglo" );
        fitglo_( &_par[0] );
        C_PRTGLO(20);
        MUTOO::TRACE( "MuonGlobalAlign::end_minimize, done" );
    }

}

//________________________________________
bool MuonGlobalAlign::print_to_file( const char* filename_align )
{
    MUTOO::TRACE( string("MuonGlobalAlign::print_to_file - ") + filename_align );

    // make_backup( filename_align );
    ofstream out( filename_align, ios::out );
    if( !out ) {
        cout << "Align::DumpToFile - ERROR: cannot write to file \"" << filename_align << "\".\n";
        return false;
    }

    if( get_flag( ALIGN_MUTR ) ) print_mutr_parameters_to_stream( out );
    if( get_flag( ALIGN_MUID ) ) print_muid_parameters_to_stream( out );

    print_fixed_parameters( out );

    MUTOO::PRINT( out, "CONFIGURATION" );
    out << "_align_w      :" << ((get_flag( ALIGN_W ))?"true":"false")  << endl;
    out << "_align_z      :" << ((get_flag( ALIGN_Z ))?"true":"false")  << endl;
    out << "_align_phi    :" << ((get_flag( ALIGN_PHI ))?"true":"false")  << endl;
    out << "_align_muid     :" << ((get_flag( ALIGN_MUID ))?"true":"false")  << endl;
    out << "_align_mutr     :" << ((get_flag( ALIGN_MUTR ))?"true":"false")  << endl;
    out << "_iterate      :" << ((get_flag( ITERATE ))?"true":"false") << endl;
    out << "_scratch_filename :" << _scratch_filename << endl;
    out << "_n_std_dev    :" << _n_std_dev << endl;
    MUTOO::PRINT( out, "**" );

    out.close();

    return true;

}

//________________________________________
void MuonGlobalAlign::print_mutr_parameters_to_stream( ostream &out )
{
    MUTOO::TRACE( "MuonGlobalAlign::print_mutr_parameters_to_stream" );

    boost::array< double, 2 > angle;
    boost::array< double, 2 > delta_w;
    boost::array< double, 2 > error_w2;

    // dump mutr alignment
    MUTOO::PRINT( out, "MUON TRACKER" );

    for (int arm=0; arm<MUTOO::NumberOfArms;arm++)
        for ( int station =0; station <MUTOO::NumberOfStations; station++)
        for( int gap = 0; gap<MUTOO::NumberOfGaps; gap++ )
        for(int octant = 0; octant <MUTOO::NumberOfOctants;octant ++)
        for( int cath = 0; cath<MUTOO::NumberOfCathodePlanes; cath++ )
    {

        for(int half_octant = 0; half_octant <MUTOO::NumberOfHalfOctants;half_octant ++)
        {

            // needed to remove station3 gap3
            if( station == 2 && gap == 2 ) continue;

            int index = get_index_half_octant(arm, station, octant, half_octant, gap,cath);

            // get misalignment parameters
            TMutAlign::CathodeParameters _cathode_para = TMutAlign::get_cathode_parameters(arm,station, octant, gap, cath);
            TMutAlign::AnodeParameters _anode_para = TMutAlign::get_anode_parameters( arm, station, octant, gap);

            _delta_x  = _cathode_para._delta_x;
            _delta_y  = _cathode_para._delta_y;
            _delta_phi = _cathode_para._delta_phi;
            _delta_z  = _anode_para._delta_z;

            _angle = angle[half_octant] = TMutGeo::get_cathode_angle(arm,station, octant,half_octant,gap,cath,0 );
            _delta_w = -sin( angle[half_octant] )*_delta_x+cos( angle[half_octant] )*_delta_y;
            out << " arm = "<< arm<<  " ; station = "<< station
                <<" ; octant = "<< octant
                <<" ; half octant = "<< half_octant
                <<" ; gap = "<< gap
                <<" ; cathode = "<< cath << " - ";

            // location parameters
            _arm = arm;
            _station = station;
            _gap = gap;
            _octant = octant;
            _half = half_octant;
            _cathode = cath;
            _is_muid = 0;
            _plane = 0;
            _panel = 0;
            _orientation = 0;

            // Dump W
            int j=NPARPLAN*index+1;
            double err=errpar_(&j);
            if( err==0 && get_flag( ALIGN_W ) ) err=-999.9;

            out << "W: " << format("%10.4f %10.4f ", _par[NPARPLAN*index],err);
            _delta_w_millepede = delta_w[half_octant] = _par[NPARPLAN*index];
            _error_w = err;
            error_w2[half_octant] = _error_w;

            // Dump Z
            j=NPARPLAN*index+2;
            err=errpar_(&j);
            if( err==0 && get_flag( ALIGN_Z ) ) err=-999.9;
            out << " Z :" << format("%10.4f %10.4f ", _par[NPARPLAN*index+1],err);
            _delta_z_millepede = _par[NPARPLAN*index+1];
            _error_z = err;

            // Dump Phi
            j=NPARPLAN*index+3;
            err=errpar_(&j);
            if( err==0 && get_flag( ALIGN_PHI ) ) err=-999.9;
            out << " Phi: " << format("%10.4f %10.4f  ", _par[NPARPLAN*index+2],err);
            _delta_phi_millepede = _par[NPARPLAN*index+2];
            _error_phi = err;

            // strip angle
            out << " angle: " << _angle;

            // Dump number of tracks
            out << " n_tracks: " << _n_tracks[index];

            out << endl;

            _nb_tracks = _n_tracks[index];
            if(half_octant ==1)
            {
                out << " angle1="<< angle[1] <<  " angle0="<< angle[0] << endl;
                if( abs(angle[0] - angle[1])<0.00001)
                {

                    // use average delta w between the two half octant instead !
                    _delta_x_millepede = - ( delta_w[0]*sin ( angle[0] )+ delta_w[1]*sin ( angle[1] ))/2;
                    _delta_y_millepede = ( delta_w[0]*cos ( angle[0] ) + delta_w[1]*cos ( angle[1] ))/2;
                    _error_x = sqrt(pow(sin( angle[0] )/2,2)*pow(error_w2[0],2) + pow(sin( angle[1])/2,2)*pow(error_w2[1],2));
                    _error_y = sqrt(pow(cos( angle[0] )/2,2)*pow(error_w2[0],2)  + pow(cos( angle[1])/2,2)*pow(error_w2[1],2));

                } else {

                    out << "sin ( )"<< sin( angle[1] - angle[0] )<<endl;
                    _delta_x_millepede = ( cos( angle[1] )*delta_w[0] - cos( angle[0] )*delta_w[1] )/sin( angle[1] - angle[0] );
                    _delta_y_millepede = ( sin( angle[1] )*delta_w[0] - sin( angle[0] )*delta_w[1] )/sin( angle[1] - angle[0] );
                    _error_x = sqrt(pow(cos( angle[1] )/ sin(angle[1]-angle[0]),2)*pow(error_w2[0],2) + pow(cos( angle[0] )/ sin(angle[1]-angle[0]),2)*pow(error_w2[1],2));
                    _error_y = sqrt(pow(sin( angle[1] )/ sin(angle[1]-angle[0]),2)*pow(error_w2[0],2) + pow(sin( angle[0] )/ sin(angle[1]-angle[0]),2)*pow(error_w2[1],2));

                }

            }


            // Fill misalignment tree
            _alignment_tree->Fill();
        }
    }

    MUTOO::PRINT( out, "**" );

    return;

}

//________________________________________
void MuonGlobalAlign::print_muid_parameters_to_stream( ostream &out )
{
    MUTOO::TRACE( "MuonGlobalAlign::print_muid_parameters_to_stream" );

    // dump muid alignment
    MUTOO::PRINT( out, "MUON IDENTIFIER" );
    for (int arm=0; arm<MUIOO::MAX_ARM;arm++)
        for ( int plane =0; plane <MUIOO::MAX_PLANE; plane++)
        for ( int panel = 0; panel< MUIOO::MAX_PANEL; panel++ )
        for (int orientation = 0; orientation < MUIOO::MAX_ORIENTATION; orientation ++ )
    {
        int index = get_index_panel(arm, plane, panel, orientation);

        // get misalignment parameters
        TMuiAlign::PanelParameters _panel_para = TMuiAlign::get_panel_parameters(arm,plane, panel);

        _delta_x   = _panel_para._delta_x;
        _delta_y   = _panel_para._delta_y;
        _delta_phi = _panel_para._delta_phi;
        _delta_z   = _panel_para._delta_z;

        _angle = TMuiGeo::get_panel_angle( arm, plane, panel, orientation );
        _delta_w = -sin( _angle )*_delta_x+cos( _angle )*_delta_y;

        cout
            << " arm = "<< arm<<  " ; plane = "<< plane
            <<" ; panel = "<< panel
            <<" ; orientation = "<< orientation << " - " << _panel_para << endl;

        out
            << " arm = "<< arm<<  " ; plane = "<< plane
            <<" ; panel = "<< panel
            <<" ; orientation = "<< orientation << " - ";

        // Tree parameters
        _arm = arm;
        _station = 0;
        _gap = 0;
        _octant = 0;
        _half = 0;
        _cathode = 0;

        _is_muid = 1;
        _plane = plane;
        _panel = panel;
        _orientation = orientation;

        // Dump W
        int j=NPARPLAN*index+1;
        float err=errpar_(&j);
        if( err==0 && get_flag( ALIGN_W ) ) err=-999.9;
        out << "W: " << format("%10.4f %10.4f ", _par[NPARPLAN*index],err);
        _delta_w_millepede = _par[NPARPLAN*index];
        _error_w = err;

        // Dump Z
        j=NPARPLAN*index+2;
        err=errpar_(&j);
        if( err==0 && get_flag( ALIGN_Z ) ) err=-999.9;
        out << " Z :" << format("%10.4f %10.4f ", _par[NPARPLAN*index+1],err);
        _delta_z_millepede = _par[NPARPLAN*index+1];
        _error_z = err;

        // Dump Phi
        j=NPARPLAN*index+3;
        err=errpar_(&j);
        if( err==0 && get_flag( ALIGN_PHI ) ) err=-999.9;
        out << " Phi: " << format("%10.4f %10.4f  ", _par[NPARPLAN*index+2],err);
        _delta_phi_millepede = _par[NPARPLAN*index+2];
        _error_phi = err;

        // strip angle
        out << " angle: " << _angle;

        // Dump number of tracks
        out << " n_tracks: " << _n_tracks[index];

        out << endl;
        _delta_x_millepede = -sin(_angle)*_delta_w_millepede ;
        _delta_y_millepede = cos( _angle )*_delta_w_millepede;
        _error_x = sqrt(pow(sin( _angle ),2)*pow(_error_w,2));
        _error_y = sqrt(pow(cos( _angle ),2)*pow(_error_w,2));

        out
            << "_delta_x " <<_delta_x
            <<" _delta_y "<<_delta_y
            << " _angle "<< _angle
            <<" _delta_w "<<_delta_w
            <<" _delta_w_millepede " << _delta_w_millepede
            << " _error_w "<<_error_w<< endl;

        _nb_tracks = _n_tracks[index];

        // Fill misalignment tree
        _alignment_tree->Fill();

    }

    MUTOO::PRINT( out, "**" );

    return;

}

//________________________________________
void MuonGlobalAlign::print_fixed_parameters( ostream &out ) const
{

    MUTOO::TRACE( "MuonGlobalAlign::print_fixed_parameters" );
    MUTOO::PRINT( out, "FIXED PARAMETERS" );

    // mutr detectors
    for( Alignment::MutrDetId::Map::const_iterator iter = _fixed_mutr_detectors.begin(); iter != _fixed_mutr_detectors.end(); iter++ )
    {
        if( iter->first._station == 2 && iter->first._gap == 2 ) continue;
        out
            << " arm = " << iter->first._arm
            << " ; station = " << iter->first._station
            << " ; gap = " << iter->first._gap
            << " ; cathode = " << iter->first._cathode << " - ";

        if( iter->second & PAR_W ) out << " FIX_W";
        if( iter->second & PAR_Z ) out << " FIX_Z";
        if( iter->second & PAR_PHI ) out << " FIX_PHI";
        out << endl;

    }

    for( Alignment::MuidDetId::Map::const_iterator iter = _fixed_muid_detectors.begin(); iter != _fixed_muid_detectors.end(); iter++ )
    {
        out
            << " arm = "<< iter->first._arm
            <<  " ; plane = " << iter->first._plane
            <<" ; orientation = " << iter->first._orientation
            << " - ";

        if( iter->second & PAR_W ) out << " FIX_W";
        if( iter->second & PAR_Z ) out << " FIX_Z";
        if( iter->second & PAR_PHI ) out << " FIX_PHI";
        out << endl;
    }

    MUTOO::PRINT( out, "**" );

}

//______________________________________________________
void MuonGlobalAlign::check_detector_index( void ) const
{

    MUTOO::PRINT( cout, "MuonGlobalAlign::check_detector_index" );

    // muon tracker half octant indices
    cout << "mutr_half_octant_index" << endl;
    set<int> index_set;
    for (int arm=0; arm<MUTOO::NumberOfArms;arm++)
        for ( int station =0; station < MUTOO::NumberOfStations; station++)
        for(int octant = 0; octant < MUTOO::NumberOfOctants;octant ++)
        for(int half_octant = 0; half_octant < MUTOO::NumberOfHalfOctants;half_octant ++)
        for( int gap = 0; gap < MUTOO::NumberOfGaps; gap++ )
        for( int cathode = 0; cathode < MUTOO::NumberOfCathodePlanes; cathode++ )
    {
        int index( get_index_half_octant( arm, station, octant, half_octant, gap, cathode ) );
        if( index_set.find( index ) != index_set.end() )
            cout
            << "mutr [" << arm << "," << station << "," << octant << "," << half_octant << "," << gap << "," << cathode << "] "
            << "duplicated (" << index << ")" << endl;
        index_set.insert( index );
    }

    // muid panel indices
    if( get_flag( ALIGN_MUID ) )
    {
        cout << "muid_panel_index" << endl;
        for (int arm=0; arm<MUIOO::MAX_ARM;arm++)
            for ( int plane =0; plane < MUIOO::MAX_PLANE; plane++)
            for( int panel = 0; panel < MUIOO::MAX_PANEL; panel++ )
            for(int orientation = 0; orientation < MUIOO::MAX_ORIENTATION; orientation ++)
        {
            int index( get_index_panel( arm, plane, panel, orientation ) );
            if( index_set.find( index ) != index_set.end() )
                cout
                << "muid [" << arm << "," << plane << "," << panel << "," << orientation << "] "
                << "duplicated (" << index << ")" << endl;
            index_set.insert( index );
        }
    }

    // muon tracker cathode indices
    index_set.clear();
    cout << "mutr_cathode_index" << endl;
    for (int arm=0; arm<MUTOO::NumberOfArms;arm++)
        for ( int station =0; station < MUTOO::NumberOfStations; station++)
        for( int gap = 0; gap < MUTOO::NumberOfGaps; gap++ )
        for( int cathode = 0; cathode < MUTOO::NumberOfCathodePlanes; cathode++ )
    {
        int index( get_index_cathode( arm, station, gap, cathode ) );
        if( index_set.find( index ) != index_set.end() )
            cout
            << "mutr [" << arm << "," << station << "," << gap << "," << cathode << "] "
            << "duplicated (" << index << ")" << endl;
        index_set.insert( index );
    }

    // muid orientation indices
    if( get_flag( ALIGN_MUID ) )
    {
        cout << "muid_orientation_index" << endl;
        for (int arm=0; arm<MUIOO::MAX_ARM;arm++)
            for ( int plane =0; plane < MUIOO::MAX_PLANE; plane++)
            for(int orientation = 0; orientation < MUIOO::MAX_ORIENTATION; orientation ++)
        {
            int index( get_index_orientation( arm, plane, orientation ) );
            if( index_set.find( index ) != index_set.end() )
                cout
                << "muid [" << arm << "," << plane << "," << orientation << "] "
                << "duplicated (" << index << ")" << endl;
            index_set.insert( index );
        }
    }

    MUTOO::PRINT( cout, "**" );

}

//______________________________________________________
int MuonGlobalAlign::get_index_half_octant( int arm, int station, int octant, int half_octant, int gap, int cathode )
{
    int local_index(
        cathode + MUTOO::NumberOfCathodePlanes*(
        gap+MUTOO::NumberOfGaps*(
        half_octant + MUTOO::NumberOfHalfOctants*(
        octant + MUTOO::NumberOfOctants*(
        station + MUTOO::NumberOfStations*arm ))))
        );

    return local_index;

}

//______________________________________________________
int MuonGlobalAlign::get_index_cathode( int arm, int station, int gap, int cathode )
{
    // some checks
    if(
        arm > MUTOO::NumberOfArms ||
        station> MUTOO::NumberOfStations ||
        gap > MUTOO::NumberOfGaps ||
        cathode > MUTOO::NumberOfCathodePlanes )
        throw runtime_error( "MuonGlobalALign::get_index_octant - invalid location" );

    int local_index(
        cathode + MUTOO::NumberOfCathodePlanes*(
        gap+MUTOO::NumberOfGaps*(
        station + MUTOO::NumberOfStations*arm )));


    return local_index;
}

//______________________________________________________
int MuonGlobalAlign::get_index_panel( int arm, int plane, int panel, int orientation ) const
{

    // some checks
    if(
        arm > MUIOO::MAX_ARM ||
        plane> MUIOO::MAX_PLANE ||
        panel > MUIOO::MAX_PANEL ||
        orientation > MUIOO::MAX_ORIENTATION )
        throw runtime_error( "MuonGlobalALign::get_index_panel - invalid location" );


    if( !get_flag( ALIGN_MUID ) ) {
        throw DESCRIPTION( "ALIGN_MUID is false" );
        return 0;
    }

    int local_index(
        orientation + MUIOO::MAX_ORIENTATION*(
        panel+MUIOO::MAX_PANEL*(
        plane + MUIOO::MAX_PLANE*arm))
        );

    return _muid_offset+local_index;

}

//______________________________________________________
int MuonGlobalAlign::get_index_orientation( int arm, int plane, int orientation ) const
{

    // some checks
    if(
        arm > MUIOO::MAX_ARM ||
        plane> MUIOO::MAX_PLANE ||
        orientation > MUIOO::MAX_ORIENTATION )
        throw runtime_error( "MuonGlobalALign::get_index_panel - invalid location" );


    if( !get_flag( ALIGN_MUID ) ) {
        throw DESCRIPTION( "ALIGN_MUID is false" );
        return 0;
    }

    int local_index(
        orientation + MUIOO::MAX_ORIENTATION*(
        plane + MUIOO::MAX_PLANE*arm)
        );

    return _muid_offset+local_index;

}

//______________________________________________________
void MuonGlobalAlign::fill_evaluation_tree( void )
{
    if( verbosity >= 1 ) MUTOO::TRACE( "MuonGlobalAlign::fill_evaluation_tree" );

    // loop over tracks
    TMutTrkMap::iterator trk_iter = _trk_map->range();
    while( TMutTrkMap::pointer trk_ptr = trk_iter.next() )
    {

        // check if track is a ghost, or failed reconstruction
        if( trk_ptr->get()->get_ghost() || !trk_ptr->get()->get_reco_success() )
        { continue; }

        // picking values to fill the tree
        _arm = trk_ptr->get()->get_arm();
        _octant = trk_ptr->get()->get_octant();
        _chi_square = trk_ptr->get()->get_chi_square();
        _ndf = trk_ptr->get()->get_ndf();
        _prob = TMath::Prob( _chi_square, _ndf );

        _chi_square_vtx = trk_ptr->get()->get_trk_par_vtx()->get_chi_square();
        _ndf_vtx = 2;
        _prob_vtx = TMath::Prob( _chi_square_vtx, _ndf_vtx );

        int station;
        _hit=0;
        _accept_trk = accept_trk( trk_ptr );

        // Fill trees
        if( !( _accept_trk && get_flag( USE_CUTS ) ) ) continue;

        // checking if the last station received enough hit
        TMutCoordMap::const_key_iterator coord_iter = trk_ptr->get()->get_associated<TMutCoord>();
        while( TMutCoordMap::const_pointer coord_ptr = coord_iter.next() )
        {
            // if hit in all detectors of last station to be able to have 2D traking
            station = coord_ptr->get()->get_station() ;
            if( station==2 ) _hit++;
        }


        fill_mutr_evaluation_tree( trk_ptr );
        fill_muid_evaluation_tree( trk_ptr );

        _evaluation_tree->Fill();
    }
}

//_____________________________________________________________________________
void MuonGlobalAlign::fill_mutr_evaluation_tree( TMutTrkMap::pointer trk_ptr )
{
    if( verbosity >= 1 ) MUTOO::TRACE( "MuonGlobalAlign::fill_mutr_evaluation_tree" );

    // check fit parameters size
    const std::vector<TMutTrkPar> &trk_par_vect( *trk_ptr->get()->get_trk_par_list() );
    if( !trk_par_vect.size() ) return;
    // reset flags
    _flag.assign( 0 );

    // retrieve associated coordinates
    TMutCoordMap::const_key_iterator coord_iter = trk_ptr->get()->get_associated<TMutCoord>();
    _mutr_coords = coord_iter.count();
    while( TMutCoordMap::const_pointer coord_ptr = coord_iter.next() )
    {
        // index of the coordinate
        int detector_index = get_index_ntp(
            coord_ptr->get()->get_station(), coord_ptr->get()->get_gap(), coord_ptr->get()->get_cathode() );

        // retreive coordinate z
        _z_det[detector_index] = coord_ptr->get()->get_mean_z();

        // retreive coordinate measurement
        _w_det[detector_index] = coord_ptr->get()->get_w_absolute();

        // retreive which half octant
        _half_octant[detector_index] = coord_ptr->get()->get_half_octant();

        // store projection matrix from state vector to measurement
        double angle( TMutGeo::get_cathode_angle(
            coord_ptr->get()->get_arm(), coord_ptr->get()->get_station(), coord_ptr->get()->get_octant(), coord_ptr->get()->get_half_octant(), coord_ptr->get()->get_gap(), coord_ptr->get()->get_cathode(), coord_ptr->get()->get_peak_strip()) );

        // find best matching track parameters
        const TMutTrkPar trk_par( *min_element( trk_par_vect.begin(), trk_par_vect.end(), closest_z_ftor( _z_det[detector_index] ) ) );

        // fill fit parameters
        _x_not_extrapo[detector_index] = trk_par.get_x();
        _y_not_extrapo[detector_index] = trk_par.get_y();
        _z_fit[detector_index] = trk_par.get_z();

        _x_fit[detector_index] = trk_par.get_x() + (trk_par.get_px())/(trk_par.get_pz())*(_z_det[detector_index]-_z_fit[detector_index]);
        _y_fit[detector_index] = trk_par.get_y() + (trk_par.get_py())/(trk_par.get_pz())*(_z_det[detector_index]-_z_fit[detector_index]);
        _v_fit[detector_index] =  cos( angle )*_x_fit[detector_index] + sin( angle )*_y_fit[detector_index];
        _w_fit[detector_index] = -sin( angle )*_x_fit[detector_index] + cos( angle )*_y_fit[detector_index];
        _residu[detector_index] = _w_det[detector_index] - _w_fit[detector_index];
        _flag[detector_index] = 1;

        double x_fit_square = pow(_x_fit[detector_index],2);
        double y_fit_square = pow(_y_fit[detector_index],2);
        double z_fit = fabs(_z_fit[detector_index]);
        _theta[detector_index] = atan2( sqrt (x_fit_square + y_fit_square), z_fit);

    }
}

//_____________________________________________________________________________
void MuonGlobalAlign::fill_muid_evaluation_tree( TMutTrkMap::pointer trk_ptr )
{
    if( verbosity >= 1 ) MUTOO::TRACE( "MuonGlobalAlign::fill_muid_evaluation_tree" );

    // reset flags
    _muid_flag.assign( 0 );

    // check fit parameters size
    // check fit parameters size
    const std::vector<TMutTrkPar> &trk_par_vect( *trk_ptr->get()->get_trk_par_list() );
    if( !trk_par_vect.size() ) return;

    // retrieve associated muid clusters
    TMuiClusterMapO::key_iterator clus_iter( trk_ptr->get()->get_associated<TMuiClusterO>() );
    _muid_clusters = clus_iter.count();

    while( TMuiClusterMapO::pointer clus_ptr = clus_iter.next() )
    {

        // detector index
        int detector_index( get_muid_index_ntp(
            clus_ptr->get()->get_plane(), clus_ptr->get()->get_panel(), clus_ptr->get()->get_orientation() ) );

        // retreive which panel, plane, orientation
        _plane_eval[detector_index] = clus_ptr->get()->get_plane();
        _panel_eval[detector_index]  = clus_ptr->get()->get_panel();
        _orientation_eval[detector_index]  = clus_ptr->get()->get_orientation();
        _detector_index = detector_index;

        // retrive cluster z
        _muid_z_det[detector_index] = clus_ptr->get()->get_mean_z();

        // retrieve cluster measurement
        _muid_w_det[detector_index] = clus_ptr->get()->get_w_absolute();

        // find best matching track parameters
        const TMutTrkPar trk_par( *min_element( trk_par_vect.begin(), trk_par_vect.end(), closest_z_ftor( _muid_z_det[detector_index] ) ) );

        // fill fit parameters
        _muid_z_fit[detector_index] = trk_par.get_z();
        _muid_x_fit[detector_index] = trk_par.get_x() + (trk_par.get_px())/(trk_par.get_pz())*(_muid_z_det[detector_index]-_muid_z_fit[detector_index]);
        _muid_y_fit[detector_index] = trk_par.get_y() + (trk_par.get_py())/(trk_par.get_pz())*(_muid_z_det[detector_index]-_muid_z_fit[detector_index]);

        double angle( TMuiGeo::get_panel_angle( clus_ptr->get()->get_location() ) );
        _muid_v_fit[detector_index] =  cos( angle )*_muid_x_fit[detector_index] + sin( angle )*_muid_y_fit[detector_index];
        _muid_w_fit[detector_index] = -sin( angle )*_muid_x_fit[detector_index] + cos( angle )*_muid_y_fit[detector_index];

        _muid_residu[detector_index] = _muid_w_det[detector_index] - _muid_w_fit[detector_index];
        _muid_flag[detector_index] = 1;

    }

}

//______________________________________________________
void MuonGlobalAlign::fix_mutr_all( void )
{
    MUTOO::TRACE( "MuonGlobalAlign::fix_mutr_all" );
    for(int arm=0;arm< MUTOO::NumberOfArms;arm++)
        for( int station=0; station< MUTOO::NumberOfStations; station++ )
        for( int gap = 0; gap < MUTOO::NumberOfGaps; gap++ )
    { fix_mutr_gap( arm, station, gap, PAR_Z|PAR_W|PAR_PHI ); }

}

//______________________________________________________
void MuonGlobalAlign::fix_mutr_2stations( void )
{

    MUTOO::TRACE( "MuonGlobalAlign::fix_mutr_2stations" );
    for(int arm=0;arm< MUTOO::NumberOfArms;arm++)
        for( int station=0; station< MUTOO::NumberOfStations; station++ )
        for( int gap = 0; gap < MUTOO::NumberOfGaps; gap++ )
    {
        if( station == 2 && gap != 2 ) continue;
        fix_mutr_gap( arm, station, gap, PAR_Z|PAR_W|PAR_PHI );
    }

}

//______________________________________________________
void MuonGlobalAlign::fix_mutr_2gaps( void )
{

    MUTOO::TRACE( "MuonGlobalAlign::fix_mutr_2gaps" );
    for( int arm=0; arm < MUTOO::NumberOfArms; arm++ )
    {

        // fix station 2 cathode 2 (since the detector does not exist
        fix_mutr_gap( arm, 2, 2, PAR_Z|PAR_W|PAR_PHI );

        // north and south arm
        // fix station0 gap1 and station1 gap0
        fix_mutr_gap( arm, 0, 1, PAR_Z|PAR_W|PAR_PHI );
        fix_mutr_gap( arm, 1, 0, PAR_Z|PAR_W|PAR_PHI );

    }

}


//______________________________________________________
void MuonGlobalAlign::fix_muid_2planes( void )
{

    MUTOO::TRACE( "MuonGlobalAlign::fix_muid_2planes" );
    for( int arm=0; arm < MUIOO::MAX_ARM; arm++ )
    {
        fix_muid_plane( arm, 0, PAR_Z|PAR_W|PAR_PHI );
        fix_muid_plane( arm, 4, PAR_Z|PAR_W|PAR_PHI );
    }

}

//_________________________________
string MuonGlobalAlign::make_backup( const string& filename_align )
{
    if( access( filename_align.c_str(), R_OK ) ) return filename_align;

    string backup;
    unsigned int ver=0;

    do{
        ostringstream what;
        what << filename_align << "." << ver;
        backup = what.str();
        ver++;
    } while  ( !access( backup.c_str(), R_OK ) );

    ostringstream what;
    what << "cp " << filename_align << " " << backup;
    system( what.str().c_str() );

    cout << "MuonGlobalAlign::make_backup - file \"" << backup << "\" created.\n";
    return backup;
}

//_________________________________
string MuonGlobalAlign::format( const char* format, ... )
{

    char buffer[2048];
    va_list p;
    va_start(p,format);
    vsprintf(buffer, format, p);
    va_end(p);

    return string( buffer );
}

//_________________________________
bool MuonGlobalAlign::accept_trk( TMutTrkMap::pointer trk_ptr)
{

    // getting chi_square, number of hit in the arm
    double chi_square = trk_ptr->get()->get_chi_square();
    double chi_square_vtx = trk_ptr->get()->get_trk_par_vtx()->get_chi_square();
    double ndf_vtx = 2;
    double ndf = trk_ptr->get()->get_ndf();
    int station;

    // stating if the last station received enough hit
    TMutCoordMap::const_key_iterator coord_iter = trk_ptr->get()->get_associated<TMutCoord>();
    int hit =0;

    // check fit parameters size
    const std::vector<TMutTrkPar> &trk_par_vect( *trk_ptr->get()->get_trk_par_list() );
    if( !trk_par_vect.size() ) return false;

    float theta[18];
    memset(theta, 0, 18);

    while( TMutCoordMap::const_pointer coord_ptr = coord_iter.next() )
    {

        station = coord_ptr->get()->get_station();
        float z_det = coord_ptr->get()->get_mean_z();

        // index of the coordinate
        int detector_index = get_index_ntp(
            coord_ptr->get()->get_station(), coord_ptr->get()->get_gap(), coord_ptr->get()->get_cathode() );

        // get closest track parameters to z_det
        const TMutTrkPar trk_par( *min_element( trk_par_vect.begin(), trk_par_vect.end(), closest_z_ftor( z_det ) ) );

        // retreive coordinate z
        _z_det[detector_index] = coord_ptr->get()->get_mean_z();

        // retrieve coordinate positions
        _z_fit[detector_index] = trk_par.get_z();

        // extrapolate fit coord around detect
        _x_fit[detector_index] = trk_par.get_x() + (trk_par.get_px())/(trk_par.get_pz())*(_z_det[detector_index]-_z_fit[detector_index]);
        _y_fit[detector_index] = trk_par.get_y() + (trk_par.get_py())/(trk_par.get_pz())*(_z_det[detector_index]-_z_fit[detector_index]);

        double x_fit_square = pow(_x_fit[detector_index],2);
        double y_fit_square = pow(_y_fit[detector_index],2);
        double z_fit = fabs(_z_fit[detector_index]);
        theta[detector_index] = atan2( sqrt (x_fit_square + y_fit_square), z_fit);

        // if hit in all detectors of last station to be able to have 2D traking
        if( station==2 ) hit++;

    }

    _mem_trk_theta0 = theta[0];
    _mem_trk_chi_square_ndf = chi_square/ndf;
    _mem_trk_chi_square_vtx_ndf = chi_square_vtx/ndf_vtx;
    _mem_trk_st2_hit = hit;

    if( verbosity >= 1 )
    {
        cout << "MuonGlobalAlign::accept_trk - trk_chi_square_ndf " << _mem_trk_chi_square_ndf << endl;
        cout << "MuonGlobalAlign::accept_trk - trk_chi_square_vtx_ndf " << _mem_trk_chi_square_vtx_ndf << endl;
        cout << "MuonGlobalAlign::accept_trk - trk_st2_hit " << _mem_trk_st2_hit << endl;
    }

    // cut on theta if requiered
    bool theta_small = false;
    bool theta_big = false;

    // cut the events close to the beam axis - leave tracks away from the beam
    if( theta[0] < 0.4 ) theta_small = true;
    else theta_big = true;

    // cut on the chi_square
    if( chi_square/ndf>20 )
    {

        if( verbosity >= 1 ) cout << "MuonGlobalAlign::accept_trk - chi_square cut " << endl;
        return false;

    }

    // cut on the vertex chi_square
    if( chi_square_vtx/ndf_vtx>15 )
    {

        if( verbosity >= 1 ) cout << "MuonGlobalAlign::accept_trk - chi_square_vtx cut " << endl;
        return false;

    };

    // make sure the number of hit in the last detector is sufficient to make a 2D track reconstruction
    if(trk_ptr->get()->get_arm() == MUTOO::South && hit != 4 )
    {

        if( verbosity >= 1 ) cout << "MuonGlobalAlign::accept_trk - south arm number of hit cut " << endl;
        return false;

    }

    if(trk_ptr->get()->get_arm() == MUTOO::North && hit != 4 )
    {

        if( verbosity >= 1 ) cout << "MuonGlobalAlign::accept_trk - north arm number of hit cut " << endl;
        return false;
    }

    // cut on theta
    if(get_flag( USE_THETA_CUT_CLOSE_TRACK ) && theta_big == true )
    {

        if( verbosity >= 1 ) cout << "MuonGlobalAlign::accept_trk - theta too big cut " << endl;
        return false;

    }

    if(get_flag( USE_THETA_CUT ) && theta_small == true )
    {

        if( verbosity >= 1 ) cout << "MuonGlobalAlign::accept_trk - theta too small cut " << endl;
        return false;
    }

    return true;

}

//_________________________________
bool MuonGlobalAlign::accept_trk( TMutAlignParMap::pointer align_par_ptr )
{
    // cut on theta if requiered
    bool theta_small = false;
    bool theta_big = false;

    // cut the events close to the beam axis - leave tracks away from the beam
    if( align_par_ptr->get()->get_trk_theta0() < 0.4 ) theta_small = true;
    else theta_big = true;

    if( verbosity >= 1 )
    {
        cout << "MuonGlobalAlign::accept_trk - trk_chi_square_ndf " << align_par_ptr->get()->get_trk_chi_square_ndf() << endl;
        cout << "MuonGlobalAlign::accept_trk - trk_chi_square_vtx_ndf " << align_par_ptr->get()->get_trk_chi_square_vtx_ndf() << endl;
        cout << "MuonGlobalAlign::accept_trk - trk_st2_hit " << align_par_ptr->get()->get_trk_st2_hit() << endl;
    }

    // cut on the chi_square
    if( align_par_ptr->get()->get_trk_chi_square_ndf() > 20 )
    {

        if( verbosity >= 1 ) cout << "MuonGlobalAlign::accept_trk - chi_square cut " << endl;
        return false;

    }

    // cut on the vertex chi_square
    if( align_par_ptr->get()->get_trk_chi_square_vtx_ndf() > 15 )
    {

        if( verbosity >= 1 ) cout << "MuonGlobalAlign::accept_trk - chi_square_vtx cut " << endl;
        return false;

    };

    // make sure the number of hit in the last detector is sufficient to make a 2D track reconstruction
    if( align_par_ptr->get()->get_trk_st2_hit() != 4 )
    {

        if( verbosity >= 1 ) cout << "MuonGlobalAlign::accept_trk - number of hit cut " << endl;
        return false;

    }

    // cut on theta
    if(get_flag( USE_THETA_CUT_CLOSE_TRACK ) && theta_big == true )
    {

        if( verbosity >= 1 ) cout << "MuonGlobalAlign::accept_trk - theta too big cut " << endl;
        return false;

    }

    if(get_flag( USE_THETA_CUT ) && theta_small == true )
    {

        if( verbosity >= 1 ) cout << "MuonGlobalAlign::accept_trk - theta too small cut " << endl;
        return false;
    }

    return true;

}

//________________________________________
void MuonGlobalAlign::get_octant_angle( ostream &out ) const
{

    MUTOO::TRACE( "MuonGlobalAlign::get_octant_angle" );

    out << "// arm   station   octant   angle"<< endl;

    for (int arm=0; arm<MUTOO::NumberOfArms;arm++)
        for ( int station =0; station < MUTOO::NumberOfStations; station++)
        for(int octant = 0; octant < MUTOO::NumberOfOctants;octant ++)
    {
        MutArm* geometry = (arm == MUTOO::South) ? SouthArm() : NorthArm();

        double angle = (
            geometry->f_pMutStations[station]->
            f_pMutOctants[octant]->getBeginPhi()
            +   geometry->f_pMutStations[station]->
            f_pMutOctants[octant]->getEndPhi()) /2;

        out <<  arm<< "    "<< station<< "    " << octant<< "    " << angle << endl;

    }

    MUTOO::PRINT( out, "**" );

}

//________________________________________
bool MuonGlobalAlign::print_angle_to_file( const char* filename_align )
{

    MUTOO::TRACE( "MuonGlobalAlign::print_angle_to_file" );
    make_backup( filename_align );
    ofstream out( filename_align, ios::out );
    if( !out )
    {

        cout << "Align::DumpToFile - ERROR: cannot write to file \"" << filename_align << "\".\n";
        return false;

    }

    get_octant_angle( out );
    out.close();

    return true;

}
