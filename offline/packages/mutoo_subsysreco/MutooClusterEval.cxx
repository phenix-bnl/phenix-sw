// $Id: MutooClusterEval.cxx,v 1.8 2011/07/14 22:27:10 pinkenbu Exp $

//////////////////////////////////////////////////////////////
/*!
\file MutooClusterEval.cxx
\brief cluster evaluation ntuple
\author  Hugo Pereira
\version $Revision: 1.8 $
\date $Date: 2011/07/14 22:27:10 $
*/
//////////////////////////////////////////////////////////////

#include "MutooClusterEval.h"

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
MutooClusterEval::MutooClusterEval( const char* name, const char* file_name ) :
    _file_name( file_name ),
    _timer(PHTimeServer::get()->insert_new(name) )
{
    ThisName = name;
    return ;
}

//_____________________________________
int MutooClusterEval::Init(PHCompositeNode *top_node)
{
    MUTOO::PRINT( cout, "MutooClusterEval::Init" );

    // create TFile
    PHTFileServer::get().open( _file_name, "RECREATE" );
    cout << "writing to file \"" << _file_name << "\"" << endl;

    enum { BUFFER_SIZE=32000 };
    enum { AUTO_SAVE=16000 };

    _eval_clusters = new TTree( "eval_clusters", "clusters" );
    _eval_clusters->Branch( "arm", &_arm, "arm/I", BUFFER_SIZE );
    _eval_clusters->Branch( "station", &_station, "station/I", BUFFER_SIZE );
    _eval_clusters->Branch( "octant", &_octant, "octant/I", BUFFER_SIZE );
    _eval_clusters->Branch( "half", &_half, "half/I", BUFFER_SIZE );
    _eval_clusters->Branch( "gap", &_gap, "gap/I", BUFFER_SIZE );
    _eval_clusters->Branch( "cathode", &_cathode, "cathode/I", BUFFER_SIZE );
    _eval_clusters->Branch( "hit_mult", &_hit_mult, "hit_mult/I", BUFFER_SIZE );

    _eval_clusters->Branch( "n_coords", &_n_coords, "n_coords/I", BUFFER_SIZE );

    _eval_clusters->Branch( "low_charge", &_low_charge, "low_charge/O", BUFFER_SIZE );
    _eval_clusters->Branch( "high_charge", &_high_charge, "high_charge/O", BUFFER_SIZE );
    _eval_clusters->Branch( "has_attenuated_strip", &_has_attenuated_strip, "has_attenuated_strip/O", BUFFER_SIZE );
    _eval_clusters->Branch( "has_saturated_strip", &_has_saturated_strip, "has_saturated_strip/O", BUFFER_SIZE );
    _eval_clusters->Branch( "has_bad_strip", &_has_bad_strip, "has_bad_strip/O", BUFFER_SIZE );
    _eval_clusters->Branch( "peak_bound", &_peak_bound, "peak_bound/O", BUFFER_SIZE );
    _eval_clusters->Branch( "has_track", &_has_track, "has_track/O", BUFFER_SIZE );

    _eval_clusters->Branch( "clus_size", &_clus_size, "clus_size/I", BUFFER_SIZE );
    _eval_clusters->Branch( "clus_chi2", &_clus_chi2, "clus_chi2/F", BUFFER_SIZE );
    _eval_clusters->Branch( "clus_q", &_clus_q, "clus_q/F", BUFFER_SIZE );
    _eval_clusters->Branch( "clus_q_rms", &_clus_q_rms, "clus_q_rms/F", BUFFER_SIZE );
  
   if( _extended_tree )
    {
    _eval_clusters->Branch( "Q1", &_Q[0], "Q1/F", BUFFER_SIZE);
    _eval_clusters->Branch( "Q2", &_Q[1], "Q2/F", BUFFER_SIZE);
    _eval_clusters->Branch( "Q3", &_Q[2], "Q3/F", BUFFER_SIZE);
    _eval_clusters->Branch( "Q4", &_Q[3], "Q4/F", BUFFER_SIZE); 
    _eval_clusters->Branch( "Q5", &_Q[4], "Q5/F", BUFFER_SIZE);
    _eval_clusters->Branch( "Q6", &_Q[5], "Q6/F", BUFFER_SIZE);
    _eval_clusters->Branch( "Q7", &_Q[6], "Q7/F", BUFFER_SIZE);
    _eval_clusters->Branch( "Q8", &_Q[7], "Q8/F", BUFFER_SIZE);
    

    _eval_clusters->Branch( "Qe1", &_Qe[0], "Qe1/F", BUFFER_SIZE);
    _eval_clusters->Branch( "Qe2", &_Qe[1], "Qe2/F", BUFFER_SIZE);
    _eval_clusters->Branch( "Qe3", &_Qe[2], "Qe3/F", BUFFER_SIZE);
    _eval_clusters->Branch( "Qe4", &_Qe[3], "Qe4/F", BUFFER_SIZE);
    _eval_clusters->Branch( "Qe5", &_Qe[4], "Qe5/F", BUFFER_SIZE);
    _eval_clusters->Branch( "Qe6", &_Qe[5], "Qe6/F", BUFFER_SIZE);
    _eval_clusters->Branch( "Qe7", &_Qe[6], "Qe7/F", BUFFER_SIZE);
    _eval_clusters->Branch( "Qe8", &_Qe[7], "Qe8/F", BUFFER_SIZE);


    _eval_clusters->Branch( "w1", &_w[0], "w1/F", BUFFER_SIZE);
    _eval_clusters->Branch( "w2", &_w[1], "w2/F", BUFFER_SIZE);
    _eval_clusters->Branch( "w3", &_w[2], "w3/F", BUFFER_SIZE);
    _eval_clusters->Branch( "w4", &_w[3], "w1/F", BUFFER_SIZE);

    

    _eval_clusters->Branch( "we1", &_w_err[0], "we1/F", BUFFER_SIZE);
    _eval_clusters->Branch( "we2", &_w_err[1], "we2/F", BUFFER_SIZE);
    _eval_clusters->Branch( "we3", &_w_err[2], "we3/F", BUFFER_SIZE);
    _eval_clusters->Branch( "we4", &_w_err[3], "we4/F", BUFFER_SIZE);

    _eval_clusters->Branch( "qpeak1", &_q_peak[0], "qpeak1/F", BUFFER_SIZE);
    _eval_clusters->Branch( "qpeak2", &_q_peak[1], "qpeak2/F", BUFFER_SIZE);
    _eval_clusters->Branch( "qpeak3", &_q_peak[2], "qpeak3/F", BUFFER_SIZE);
    _eval_clusters->Branch( "qpeak4", &_q_peak[3], "qpeak4/F", BUFFER_SIZE);

    }

    _eval_clusters->SetAutoSave( AUTO_SAVE );
    cout << "hits evaluation tree booked booked" << endl;

    MUTOO::PRINT( cout, "**" );
    return 0;
}
//_____________________________________
int MutooClusterEval::InitRun(PHCompositeNode *top_node)
{ return 0; }

//_____________________________________
int MutooClusterEval::process_event(PHCompositeNode *top_node)
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
void MutooClusterEval::fill_tree( void )
{

    // cluster map
    if( !_clus_map ) return;

    // hit multiplicity
    double hits_south(0);
    double hits_north(0);
    if( _hit_map )
    {
        hits_south = _hit_map->get( MUTOO::South ).count();
        hits_north = _hit_map->get( MUTOO::North ).count();
    }

    // loop over clusters
    TMutClusMap::iterator clus_iter = _clus_map->range();
    while( TMutClusMap::pointer clus_ptr = clus_iter.next() )
    {

        // store cluster location
        _arm = clus_ptr->get()->get_arm();
        _station = clus_ptr->get()->get_station();
        _octant = clus_ptr->get()->get_octant();
        _half = clus_ptr->get()->get_half_octant();
        _gap = clus_ptr->get()->get_gap();
        _cathode = clus_ptr->get()->get_cathode();

        _hit_mult = (_arm == MUTOO::South) ? hits_south:hits_north;

        _clus_size = clus_ptr->get()->get_n_strip();
        _clus_chi2 = clus_ptr->get()->get_chi_square();

        _peak_bound = clus_ptr->get()->get_peak_bound();
        _low_charge = clus_ptr->get()->get_low_charge();
        _high_charge = clus_ptr->get()->get_high_charge();
        _has_attenuated_strip = false;
        _has_saturated_strip = false;
        _has_bad_strip = clus_ptr->get()->get_bad_strip();
  
       // initialize some values as defined.. these are needed only for extended tree

         if( _extended_tree ){
            for(int i_tmp=0; i_tmp<8; i_tmp++)
             {
               _Q[i_tmp] = 0.;
               _Qe[i_tmp] = 0.;
              if( i_tmp <4){
                 _w[i_tmp] = 0. ;
                 _w_err[i_tmp] = 0. ;
                 _q_peak[i_tmp] = 0. ;
              }
             }
         }


        // find associated coordinate
        Int_t index=0;
        TMutCoordMap::const_key_iterator coord_iter( clus_ptr->get()->get_associated<TMutCoord>() );
        _has_track = 0;
        _n_coords = coord_iter.count();
        while( TMutCoordMap::const_pointer coord_ptr = coord_iter.next() )
        {
           if( _extended_tree && index<4)
             {
               _w[index]= coord_ptr->get()->get_w();
               _w_err[index] = coord_ptr->get()->get_error();
               _q_peak[index] = coord_ptr->get()->get_q_peak();
             }
             index++;
             
            if( coord_ptr->get()->get_associated<TMutTrk>().count() >= 1 )
            {
                _has_track = 1;
                //break;
            }
        }

 // loop over associated hits to store total charge
        _clus_q = 0;
        _clus_q_rms = 0;
        index=0;

        TMutHitMap::const_key_iterator hit_iter( clus_ptr->get()->get_associated<TMutHit>() );
        while( TMutHitMap::const_pointer hit_ptr = hit_iter.next() )
        {
           if( _extended_tree && index<8){
              _Q[index]= hit_ptr->get()->get_q();
              _Qe[index]= hit_ptr->get()->get_error_q();
           }
           index++;
          
            _clus_q += hit_ptr->get()->get_q();
            _clus_q_rms += MUTOO::SQUARE( hit_ptr->get()->get_q() );

            MutStrip *strip_ptr = TMutGeo::get_strip_geom(
                hit_ptr->get()->get_arm(),
                hit_ptr->get()->get_station(),
                hit_ptr->get()->get_octant(),
                hit_ptr->get()->get_half_octant(),
                hit_ptr->get()->get_gap(),
                hit_ptr->get()->get_cathode(),
                hit_ptr->get()->get_strip() );

            // atenuation
            _has_attenuated_strip |= (!strip_ptr) || strip_ptr->UseAttenuation();

            // saturation
            _has_saturated_strip |= hit_ptr->get()->get_is_saturated();

        }

        // get charge rms
        _clus_q_rms = sqrt( _clus_q_rms/_clus_size - MUTOO::SQUARE( _clus_q/_clus_size ) );


        assert( _has_saturated_strip == clus_ptr->get()->get_saturated_strip() );
        assert( _has_attenuated_strip == clus_ptr->get()->get_attenuated_strip() );

        // fill tree
        if( _eval_clusters ) _eval_clusters->Fill();

    }

    return;
}

//__________________________________________
void MutooClusterEval::set_interface_ptrs( PHCompositeNode *top_node )
{

    // try get maps from top node
    _hit_map = 0;
    try {
        _hit_map = TMutNode<TMutHitMap>::find_node(top_node,"TMutHitMap");
    } catch( exception &e ) {
        cout << e.what() << endl;
    }

    // try get maps from top node
    _clus_map = 0;
    try {
        _clus_map = TMutNode<TMutClusMap>::find_node(top_node,"TMutClusMap");
    } catch( exception &e ) {
        cout << e.what() << endl;
    }
}

//________________________________________________
int MutooClusterEval::End( PHCompositeNode *top_node )
{
  //    _timer.get()->print_stat();
    PHTFileServer::get().write( _file_name );
    return 0;
}
