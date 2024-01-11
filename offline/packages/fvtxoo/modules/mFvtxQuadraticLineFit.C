#include <iostream>
//#include <map>
//#include <vector>
//#include <numeric>
//#include <boost/bind.hpp>

#include <PHKey.hh>
#include <PHTFileServer.h>
#include <PHCylPoint.h>
#include <mFvtxQuadraticLineFit.h>
#include <TTree.h>
#include <TFvtxTrkMap.h>
#include <TFvtxCoordMap.h>
#include <TFvtxPisaHitMap.h>
#include <TFvtxQuadraticTrkParMap.h>
#include <TFvtxQuadraticTrkPar_v1.h>

#include <PHGslMatrix.h>
#include <FVTXGEOM.h>

// Shorthand for squaring a number
template<typename T> T SQ(const T& x) { return x*x; }

mFvtxQuadraticLineFit::mFvtxQuadraticLineFit() :
  _do_evaluation(true),
  _eval_filename("mFvtxQuadraticLineFit_eval.root"),
  _eval_tree(0),
  _ievent(0),
  _vertex_z(0.0),
  _trk_map(0),
  _coord_map(0),
  _hit_map(0),
  _vtxhit_map(0), 
  _timer("mFvtxQuadraticLineFit")
{}

mFvtxQuadraticLineFit::~mFvtxQuadraticLineFit() {}

void
mFvtxQuadraticLineFit::init(PHCompositeNode*) 
{
  _ievent = 0;
  std::cout << "mFvtxQuadraticLineFit::init: Writing to file " << _eval_filename << std::endl;
  PHTFileServer::get().open( _eval_filename, "RECREATE" );
  book_trees();
}

void
mFvtxQuadraticLineFit::end(PHCompositeNode*)
{
  _eval_tree->AutoSave();
  _timer.print_stat();
}

void
mFvtxQuadraticLineFit::book_trees()
{
  std::cout << "Booking straight line fit tree" << std::endl;
  _eval_tree = new TTree("quad_fit_eval","sl_fit_eval");
  _eval_tree->Branch("event",&_ievent,"event/I",BUFFER_SIZE);
  _eval_tree->Branch("trackid",&_trackid,"trackid/I",BUFFER_SIZE);
  _eval_tree->Branch("arm",&_arm,"arm/I",BUFFER_SIZE);
  _eval_tree->Branch("x0reco",&_x0reco,"x0reco/D",BUFFER_SIZE);
  _eval_tree->Branch("y0reco",&_y0reco,"y0reco/D",BUFFER_SIZE);
  _eval_tree->Branch("z0reco",&_z0reco,"z0reco/D",BUFFER_SIZE);
  _eval_tree->Branch("mxreco",&_mxreco,"mxreco/D",BUFFER_SIZE);
  _eval_tree->Branch("myreco",&_myreco,"myreco/D",BUFFER_SIZE);
  _eval_tree->Branch("cxreco",&_cxreco,"cxreco/D",BUFFER_SIZE);
  _eval_tree->Branch("cyreco",&_cyreco,"cyreco/D",BUFFER_SIZE);
  _eval_tree->Branch("chi2",&_chi2,"chi2/D",BUFFER_SIZE);
  _eval_tree->Branch("ndf",&_ndf,"ndf/I",BUFFER_SIZE);
  _eval_tree->Branch("nhits",&_size,"nhits/I",BUFFER_SIZE);
  _eval_tree->Branch("x",&_xhit[0],"x[nhits]/D",BUFFER_SIZE);
  _eval_tree->Branch("y",&_yhit[0],"y[nhits]/D",BUFFER_SIZE);
  _eval_tree->Branch("z",&_zhit[0],"z[nhits]/D",BUFFER_SIZE);

  _eval_tree->SetAutoSave(AUTO_SAVE);
}

void
mFvtxQuadraticLineFit::fill_trees(TFvtxTrkMap::const_pointer trk_ptr, 
				  const TFvtxQuadraticTrackFit& fit)
{
  _trackid = trk_ptr->get()->get_index();
  _arm = trk_ptr->get()->get_arm();

  PHGslMatrix state = fit.get_state();
  _x0reco = state(0,0);
  _y0reco = state(1,0);
  _z0reco = fit.get_z();
  _mxreco = state(2,0);
  _myreco = state(3,0);
  _cxreco = state(4,0);
  _cyreco = state(5,0);
  _chi2 = fit.get_chisquare();
  _ndf = fit.get_ndf();

  _size = 0;
  _xhit.assign(0);
  _yhit.assign(0);
  _zhit.assign(0);
  TFvtxCoordMap::key_iterator coord_iter = trk_ptr->get()->get_associated<TFvtxCoord>();
  while ( TFvtxCoordMap::const_pointer coord_ptr = coord_iter.next() )
    {
      if ( _size >= 20 ) break;
      PHPoint pnt = coord_ptr->get()->get_coord_midpoint();
      _xhit[_size] = pnt.getX();
      _yhit[_size] = pnt.getY();
      _zhit[_size] = pnt.getZ();
      _size++;
    }

  _eval_tree->Fill();
}

void
mFvtxQuadraticLineFit::set_interface_ptrs(PHCompositeNode* top_node)
{
  _hit_map = TMutNode<TFvtxHitMap>::find_node(top_node,"TFvtxHitMap");
  _vtxhit_map = TMutNode<TFvtxPisaHitMap>::find_node(top_node,"TFvtxPisaHitMap");
  _coord_map = TMutNode<TFvtxCoordMap>::find_node(top_node,"TFvtxCoordMap");
  _trk_map = TMutNode<TFvtxTrkMap>::find_node(top_node,"TFvtxTrkMap");
  _par_map = TMutNode<TFvtxQuadraticTrkParMap>::find_node(top_node,"TFvtxQuadraticTrkParMap");
}

PHBoolean
mFvtxQuadraticLineFit::event(PHCompositeNode* topNode)
{
  _ievent++;
  //std::cout << "ievent = " << _ievent << std::endl;

  _timer.restart();

  set_interface_ptrs(topNode);

  fit();

  _timer.stop();

  return 0;
}

void
mFvtxQuadraticLineFit::set_par(TFvtxTrkMap::pointer trk_ptr,
			      const TFvtxQuadraticTrackFit& fit)
{
  TFvtxQuadraticTrkParMap::iterator par_iter = _par_map->insert_new( trk_ptr->get()->get_arm() );
  TFvtxQuadraticTrkParMap::pointer p = par_iter.current();

  PHGslMatrix state = fit.get_state();
  p->get()->set_ax(state(0,0));
  p->get()->set_ay(state(1,0));
  p->get()->set_bx(state(2,0));
  p->get()->set_by(state(3,0));
  p->get()->set_cx(state(4,0));
  p->get()->set_cy(state(5,0));
  p->get()->set_chi_square(fit.get_chisquare());
  p->get()->set_ndf(fit.get_ndf());

  PHGslMatrix cov = fit.get_gain().invert();
  for( unsigned int i=0; i<TFvtxQuadraticTrkPar::COVAR_ROW; i++ ) 
    {
      for( unsigned int j=0; j<TFvtxQuadraticTrkPar::COVAR_ROW; j++ )
	{ 
	  p->get()->set_covar(i,j,cov(i,j));
	}
    }
  
  PHKey::associate(trk_ptr,p);
}

void
mFvtxQuadraticLineFit::fit()
{
  if ( ! _trk_map ) return;

  _par_map->clear();
  
  TFvtxTrkMap::iterator iter( _trk_map->range() );
  while( TFvtxTrkMap::pointer trk_ptr = iter.next() )
    {
      TFvtxQuadraticTrackFit fit;
      fit.set_z(_vertex_z);

      // TODO: Add VTX hits
      //
      // TFvtxPisaHitMap::key_iterator vtxhit_iter = trk_ptr->get()->get_associated<TFvtxPisaHit>();
      // while(TFvtxPisaHitMap::const_pointer vtxhit_ptr = vtxhit_iter.next() )
      // {}

      // Add FVTX hits
      //
      TFvtxCoordMap::key_iterator coord_iter = trk_ptr->get()->get_associated<TFvtxCoord>();
      while(TFvtxCoordMap::const_pointer coord_ptr = coord_iter.next() )
	{
	  FvtxRNode r_node(coord_ptr);
	  fit.add_node(r_node);

	  FvtxPhiNode phi_node(coord_ptr);
	  fit.add_node(phi_node);	  
	}

      if ( ! fit.fit() ) std::cout << "WARNING: straightline fit failed" << std::endl;

      //fit.get_state().print();
      
      set_par(trk_ptr,fit);
      
      if ( _do_evaluation ) fill_trees(trk_ptr,fit);
    }
}

mFvtxQuadraticLineFit::FvtxRNode::FvtxRNode(TFvtxCoordMap::const_pointer coord_ptr)
{
  PHGslMatrix m( 1, 1 ); // measurement matrix
  PHGslMatrix cov( 1, 1 ); // covariance matrix
  PHGslMatrix h( 1, 6 ); // transfer matrix (row vector)
  
  // Get points at ends of strip:
  PHPoint begin = coord_ptr->get()->get_coord_begin();
  PHPoint end = coord_ptr->get()->get_coord_end();

  PHCylPoint pnt = coord_ptr->get()->get_coord_midpoint();
  //double deltaX = end.getX() - begin.getX();
  //double deltaY = end.getY() - begin.getY();

  //m( 0, 0 ) = coord_ptr->get()->get_w_absolute();
  m( 0, 0 ) = pnt.getR();
  cov( 0, 0 ) = SQ( coord_ptr->get()->get_error() );

  // retrieve strip angle
  //double angle = atan2(deltaY,deltaX) - M_PI/2.0;
  double angle = pnt.getPhi();

  FVTXOO::angle_normalize(angle); // make sure we're on (-pi,pi]
  // Now map the angle to 0,2pi
  if ( angle < 0.0 ) angle += 2.0*M_PI;
  //angle += M_PI; // send it to (0,2pi]

  // transfer matrix
  double z_hit = coord_ptr->get()->get_coord_midpoint().getZ();
  h( 0, 0 ) =  cos(angle);
  h( 0, 1 ) =  sin(angle);
  h( 0, 2 ) =  cos(angle)*z_hit;
  h( 0, 3 ) =  sin(angle)*z_hit;
  h( 0, 4 ) =  cos(angle)*z_hit*z_hit;
  h( 0, 5 ) =  sin(angle)*z_hit*z_hit;

//   h( 0, 0 ) =  cos(angle);
//   h( 0, 1 ) =  cos(angle)*(coord_ptr->get()->get_coord_midpoint().getZ());
//   h( 0, 2 ) =  sin(angle);
//   h( 0, 3 ) =  sin(angle)*(coord_ptr->get()->get_coord_midpoint().getZ());

  set_measurement( m, cov );
  set_h( h );
}

mFvtxQuadraticLineFit::FvtxPhiNode::FvtxPhiNode(TFvtxCoordMap::const_pointer coord_ptr)
{
  PHGslMatrix m( 1, 1 ); // measurement matrix
  PHGslMatrix cov( 1, 1 ); // covariance matrix
  PHGslMatrix h( 1, 6 ); // transfer matrix (rotation into measurement frame)
  
  // Get points at ends of strip:
  PHPoint begin = coord_ptr->get()->get_coord_begin();
  PHPoint end = coord_ptr->get()->get_coord_end();

  double deltaX = end.getX() - begin.getX();
  double deltaY = end.getY() - begin.getY();
  double strip_length = sqrt( SQ(deltaX) + SQ(deltaY) );

  m( 0, 0 ) = 0.0; // measurement is at the 'center' of the rotated coordinates
  cov( 0, 0 ) = SQ(strip_length)/12.0; // square of RMS of strip's length

  // retrieve strip angle
  //double angle = atan2(deltaY,deltaX) - M_PI/2.0;
  PHCylPoint pnt = coord_ptr->get()->get_coord_midpoint();
  double angle = pnt.getPhi();

  FVTXOO::angle_normalize(angle); // ensure (-pi,pi]
  // Now map the angle to 0,2pi
  if ( angle < 0.0 ) angle += 2.0*M_PI;
  //angle += M_PI; // send it to (0,2pi]

  // transfer matrix
  double z_hit = coord_ptr->get()->get_coord_midpoint().getZ();
  h( 0, 0 ) = -sin(angle);
  h( 0, 1 ) =  cos(angle);
  h( 0, 2 ) = -sin(angle)*z_hit;
  h( 0, 3 ) =  cos(angle)*z_hit;
  h( 0, 4 ) = -sin(angle)*z_hit*z_hit;
  h( 0, 5 ) =  cos(angle)*z_hit*z_hit;
//   h( 0, 0 ) = -sin(angle);
//   h( 0, 1 ) = -sin(angle)*(coord_ptr->get()->get_coord_midpoint().getZ());
//   h( 0, 2 ) =  cos(angle);
//   h( 0, 3 ) =  cos(angle)*(coord_ptr->get()->get_coord_midpoint().getZ());

  set_measurement( m, cov );
  set_h( h );
}

