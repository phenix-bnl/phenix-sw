//////////////////////////////////////////////////////////////////
//
// Utility class: mMutBPVertex:
// Author: M.Leitch
// Date: 3/20/02
// Description: Template for MUTOO analysis module
//              
//////////////////////////////////////////////////////////////////

// MUTOO headers
//
#include <mMutBPVertex.h>
#include <mMutBPVertexPar.h>
#include <TMutNode.h>
#include <TMutTrkMap.h>
#include <TMutVtxMap.h>
#include <TMutBPUtil.h>
#include <PHException.h>
#include <MUTOO.h>

// PHENIX headers
//

/*! \ingroup modules */
/*! \file mMutBPVertex.cxx */

// STL/BOOST
//
#include <iostream>
#include <string>

using namespace std;

//____________________________________________________________
mMutBPVertex::mMutBPVertex() : 
	_timer(PHTimeServer::get()->insert_new("mMutBPVertex"))
{
  MUTOO::TRACE("initializing module mMutBPVertex",MUTOO::ALOT);
}

//____________________________________________________________
PHBoolean mMutBPVertex::event(PHCompositeNode* top_node)
{

  _timer.get()->restart(); 
  
  try { 

    // Reset IOC pointers
    //
    set_interface_ptrs(top_node);

    vtx_loop();

  } catch(exception& e) {
    MUTOO::TRACE(e.what());
    return False;
  }  

  // Timer
  //
  _timer.get()->stop();
  if( _mod_par->get_verbosity() >= MUTOO::SOME )
  _timer.get()->print(); 
  return True;
}

//____________________________________________________________
/*! Reset IOC and external interface pointers */
void mMutBPVertex::set_interface_ptrs(PHCompositeNode* top_node)
{
  
  // module runtime parameters
  //
  _mod_par = TMutNode<mMutBPVertexPar>::find_node(top_node,"mMutBPVertexPar");
  _vtx_map = TMutNode<TMutVtxMap>::find_node(top_node,"TMutVtxMap");
} 

//____________________________________________________________
void mMutBPVertex::vtx_loop()
{
  
  TMutVtxMap::iterator vtx_iter = _vtx_map->range();
  while(TMutVtxMap::pointer vtx_ptr = vtx_iter.next())
  {
    
    //    vtx_ptr->get()->print();
    
    TMutTrkMap::key_iterator trk_iter = vtx_ptr->get()->get_associated<TMutTrk>();

    // sanity check
    //
    if(trk_iter.count() != 2) throw logic_error(DESCRIPTION("mal formed vertex"));

    // Capture pointers to TMutTrk objects
    //
    TMutTrkMap::pointer trk_ptr1 = trk_iter.current();
    trk_iter++;
    TMutTrkMap::pointer trk_ptr2 = trk_iter.current();    

    const TMutTrkPar* trk_par1 = trk_ptr1->get()->get_trk_par();
    const TMutTrkPar* trk_par2 = trk_ptr2->get()->get_trk_par();

    // get track parameters at station-1
    // if BP fit only has been done then these will be for BP fit
    PHPoint x1;
    x1.setX(trk_par1->get_x());
    x1.setY(trk_par1->get_y());
    x1.setZ(trk_par1->get_z());
    PHPoint x2;
    x2.setX(trk_par2->get_x());
    x2.setY(trk_par2->get_y());
    x2.setZ(trk_par2->get_z());
    double x1p[2] = {trk_par1->get_px()/trk_par1->get_pz(), trk_par1->get_py()/trk_par1->get_pz()};
    double x2p[2] = {trk_par2->get_px()/trk_par2->get_pz(), trk_par2->get_py()/trk_par2->get_pz()};
    PHPoint x1c;
    PHPoint x2c;

    // get vertex from dca of this pair of bend plane tracks
    TMutBPUtil::get_distcls(x1,x2,x1p,x2p,x1c,x2c);
    double dist = sqrt(MUTOO::SQUARE(x1c.getX() - x2c.getX()) + MUTOO::SQUARE(x1c.getY() - x2c.getY())
      + MUTOO::SQUARE(x1c.getZ() - x2c.getZ()));
    double xv[3] = {
      (x1c.getX()+x2c.getX())/2, 
      (x1c.getY()+x2c.getY())/2, 
      (x1c.getZ()+x2c.getZ())/2
    };

    // reproject these bend plane tracks to vertex found above for the pair
    PHPoint vtx_point;
    
    // since we cannot store x!=0, y!=0 in vtx, set them to zero
    vtx_point.setX(xv[0]);
    vtx_point.setY(xv[1]);
    vtx_point.setZ(xv[2]);

    // project tracks to new vertex
    boost::array<double,3> vtx_mom1;
    boost::array<double,3> vtx_mom2;
    vtx_mom1 = TMutBPUtil::get_mom_vtx(vtx_point, trk_ptr1);
    vtx_mom2 = TMutBPUtil::get_mom_vtx(vtx_point, trk_ptr2);
    
    /* 
      update vertex pointer
      it is stored twice: 
      - at the vertex position, which gets overwritten if
        either the L2FastVtx or KalVtx fit is done afterwards
      - at the bp vertex position, in order not to be overwritten
    */
    vtx_ptr->get()->set_x( xv[0] );
    vtx_ptr->get()->set_y( xv[1] );
    vtx_ptr->get()->set_z( xv[2] );
    
    // update vertex pointer
    vtx_ptr->get()->set_x_bp( xv[0] );
    vtx_ptr->get()->set_y_bp( xv[1] );
    vtx_ptr->get()->set_z_bp( xv[2] );
    vtx_ptr->get()->set_dca_bp( dist );
    
    vtx_ptr->get()->set_px1( vtx_mom1.at(0) );
    vtx_ptr->get()->set_py1( vtx_mom1.at(1) );
    vtx_ptr->get()->set_pz1( vtx_mom1.at(2) );
    
    vtx_ptr->get()->set_px2( vtx_mom2.at(0) );
    vtx_ptr->get()->set_py2( vtx_mom2.at(1) );
    vtx_ptr->get()->set_pz2( vtx_mom2.at(2) );
      
    /* 
      number of degrees of freedom
      since there is no external vertex you have 
      2*5 measurement; 3 + 2*3 parameters =>1
    */
    vtx_ptr->get()->set_ndf( 1 );
    
  }

  return;

}

