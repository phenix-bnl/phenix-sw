// $Id: MuonUnpackDST.cxx,v 1.41 2011/07/14 22:27:10 pinkenbu Exp $

#include<PHMapManager.h>
#include<PHTimer.h>
#include<PHTimeServer.h>
#include<TMuiHitMapO.h>
#include<TMui1DRoadMapO.h>
#include<TMuiRoadMapO.h>
#include<mMutZeroSupPar.h>
#include<recoConsts.h>

#include<TMutHitMap.h>
#include<TMutClusMap.h>
#include<TMutCoordMap.h>
#include<TMutGapCoordMap.h>
#include<TMutStubMap.h>
#include<TMutTrkMap.h>
#include<TMutVtxMap.h>

#include "MuonUnpackDST.h"
#include "MuonUtil.h"

using namespace std;

//______________________________________________________
MuonUnpackDST::MuonUnpackDST() :
    MuonSubsysReco("MUONUNPACKDST"),
    _timer( PHTimeServer::get()->insert_new("MUONUNPACKDST") ),
    _flags( MuonUnpackDST::NONE )
{ return ; }

//______________________________________________________
MuonUnpackDST::~MuonUnpackDST()
{}

//______________________________________________________
int MuonUnpackDST::InitRun(PHCompositeNode *top_node)
{

    MUTOO::PRINT( cout, "MuonUnpackDST::InitRun" );

    // Create Node Tree
    CreateNodeTree(top_node);

    // initialize database:
    MuonUtil::initialize_database( top_node );

    if( get_flag( NO_ZERO_SUP ) ) cout << "MuonUnpackDST::InitRun - NO_ZERO_SUP: true" << endl;

    MUTOO::PRINT( cout, "**" );

    return 0;
}


//______________________________________________________
int MuonUnpackDST::CreateNodeTree(PHCompositeNode *top_node)
{
    try {

        // Instantiate nodes for mutoo containers
        PHCompositeNode* mutoo_node(0);
        {
            PHNodeIterator nodeItr(top_node);
            mutoo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "MUTOO"));
            if(!mutoo_node){
                mutoo_node = new PHCompositeNode("MUTOO");
                top_node->addNode(mutoo_node);
            }
        }

        // Instantiate nodes for muioo containers
        PHCompositeNode* muioo_node(0);
        {
            PHNodeIterator nodeItr(top_node);
            muioo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "MUIOO"));
            if(!muioo_node){
                muioo_node = new PHCompositeNode("MUIOO");
                top_node->addNode(muioo_node);
            }
        }

        PHCompositeNode* dst_node( 0 );
        {
            PHNodeIterator nodeItr(top_node);
            dst_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "DST"));
            if( !dst_node )
            {
                dst_node = new PHCompositeNode("DST");
                top_node->addNode(dst_node);
            }
        }

        // read mut and mui hit maps
        TMutNode<TMutHitMap>::new_dst_input_node(mutoo_node, "TMutHitMap", dst_node, "TMutHit");
        TMutNode<TMuiHitMapO>::new_dst_input_node(muioo_node, "TMuiHitMapO", dst_node, "TMuiHitO");

        // calibrations
        mMutCalibratePar* mMutCalibrate_par = TMutNode<mMutCalibratePar>::new_node(mutoo_node,"mMutCalibratePar");

        // Zero suppression parameters
        if( !get_flag( NO_ZERO_SUP ) )
        {
            mMutZeroSupPar* mMutZeroSup_par( TMutNode<mMutZeroSupPar>::new_node( mutoo_node, "mMutZeroSupPar" ) );

            if( recoConsts::instance()->get_IntFlag("PRINT_MUTOO_PARAMETERS", 1) )
            { mMutZeroSup_par->print(); }
        }

        if( recoConsts::instance()->get_IntFlag("PRINT_MUTOO_PARAMETERS", 1) )
        { mMutCalibrate_par->print(); }

    } catch (exception& e) { MUTOO::TRACE(e.what()); }
    return 0;
}

//______________________________________________________
int MuonUnpackDST::process_event(PHCompositeNode *top_node)
{

    _timer.get()->restart();

    try {

        PHMapManager::read(top_node);

        // load vertex
        load_vertex_if_needed( top_node );

        // run zero suppression if any
        if( !get_flag( NO_ZERO_SUP ) ) _mMutZeroSup_mod.event( top_node );

        // calibrations
        _mMutCalibrate_mod.event(top_node);

    } catch (std::exception& e) {

        MUTOO::TRACE(e.what());

    }

    write_maps_if_needed();
    _timer.get()->stop();
    return 0;
}

//______________________________________________________
int MuonUnpackDST::End(PHCompositeNode* top_node)
{

    // print this module timer statistics
//     _timer.get()->print_stat();

    // zero suppression summary
    if( !get_flag( NO_ZERO_SUP ) ) _mMutZeroSup_mod.print_summary();

    return 0;
}
