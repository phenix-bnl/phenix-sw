#include <iostream>

#include <PHKey.hh>
#include <PHTFileServer.h>
#include <PHCylPoint.h>
#include <mFvtxStraightLineFit.h>
#include <TTree.h>
#include <TFvtxTrkMap.h>
#include <TFvtxCoordMap.h>
#include <TFvtxPisaHitMap.h>
#include <TFvtxStraightTrkParMap.h>
#include <TFvtxStraightTrkPar_v1.h>

#include <TFvtxResidualMap.h>

#include <PHGslMatrix.h>
#include <FVTXGEOM.h>
#include <FVTXOO.h>

const double PIXEL_ZLENGTH = 0.0450; // cm
const double N_STRIPS = 1.0;
const double PHI_STRIP_RESOL = N_STRIPS*0.0050;
const double R_RESOL = 0.0200/sqrt(12);  // throwing out a number here for radial pitch of VTX pixels
const double R_THICK = 0.0200; // cm

// Shorthand for squaring a number
template<typename T> T SQ(const T& x) { return x*x; }

mFvtxStraightLineFit::mFvtxStraightLineFit() :
  _do_evaluation(false),
  _eval_filename("mFvtxStraightLineFit_eval.root"),
  _eval_tree(0),
  _ievent(0),
  _vertex_z(0.0),
  _use_svx_cluster(false),
  _trk_map(0),
  _coord_map(0),
  _hit_map(0),
  _timer(PHTimeServer::get()->insert_new("mFvtxStraightLineFit"))
{
  FVTXOO::TRACE("initializing module mFvtxStraightLineFit");

  PHTimeServer::get()->print();
}

mFvtxStraightLineFit::~mFvtxStraightLineFit() {}

void
mFvtxStraightLineFit::init(PHCompositeNode*) 
{
  _ievent = 0;
  if ( _do_evaluation ) 
    {
      std::cout << "mFvtxStraightLineFit::init: Writing to file " << _eval_filename << std::endl;
      PHTFileServer::get().open( _eval_filename, "RECREATE" );
      book_trees();
    }
}

void
mFvtxStraightLineFit::end(PHCompositeNode*)
{
  if ( _do_evaluation ) _eval_tree->AutoSave();
  _timer.get()->print_stat();
}

void
mFvtxStraightLineFit::book_trees()
{
  std::cout << "Booking straight line fit tree" << std::endl;
  _eval_tree = new TTree("sl_fit_eval","sl_fit_eval");
  _eval_tree->Branch("event",&_ievent,"event/I",BUFFER_SIZE);
  _eval_tree->Branch("trackid",&_trackid,"trackid/I",BUFFER_SIZE);
  _eval_tree->Branch("arm",&_arm,"arm/S",BUFFER_SIZE);
  _eval_tree->Branch("x0reco",&_x0reco,"x0reco/D",BUFFER_SIZE);
  _eval_tree->Branch("y0reco",&_y0reco,"y0reco/D",BUFFER_SIZE);
  _eval_tree->Branch("z0reco",&_z0reco,"z0reco/D",BUFFER_SIZE);
  _eval_tree->Branch("mxreco",&_mxreco,"mxreco/D",BUFFER_SIZE);
  _eval_tree->Branch("myreco",&_myreco,"myreco/D",BUFFER_SIZE);
  _eval_tree->Branch("chi2",&_chi2,"chi2/D",BUFFER_SIZE);
  _eval_tree->Branch("ndf",&_ndf,"ndf/S",BUFFER_SIZE);
  _eval_tree->Branch("covar",&_covar[0][0],"covar[4][4]/D",BUFFER_SIZE);
  _eval_tree->Branch("covard",&_covard[0],"covard[4]/D",BUFFER_SIZE);
  _eval_tree->Branch("nvtx",&_nVtx,"nVtx/S",BUFFER_SIZE);
  _eval_tree->Branch("nfvtx",&_nFvtx,"nFvtx/S",BUFFER_SIZE);
  _eval_tree->Branch("nhits",&_size,"nhits/I",BUFFER_SIZE);
  _eval_tree->Branch("x",&_xhit[0],"x[nhits]/D",BUFFER_SIZE);
  _eval_tree->Branch("y",&_yhit[0],"y[nhits]/D",BUFFER_SIZE);
  _eval_tree->Branch("z",&_zhit[0],"z[nhits]/D",BUFFER_SIZE);
  _eval_tree->Branch("nresid",&_nresid,"nresid/I",BUFFER_SIZE);
  _eval_tree->Branch("dR",&_dR[0],"dR[nresid]/D",BUFFER_SIZE);
  _eval_tree->Branch("dPhi",&_dPhi[0],"dPhi[nresid]/D",BUFFER_SIZE);

  _eval_tree->SetAutoSave(AUTO_SAVE);
}

void
mFvtxStraightLineFit::fill_trees(TFvtxTrkMap::const_pointer trk_ptr, 
                                 const TMutStraightTrackFit& fit)
{
  _trackid = trk_ptr->get()->get_index();
  _arm = trk_ptr->get()->get_arm();

  PHGslMatrix state = fit.get_state();
  _x0reco = state(0,0);
  _y0reco = state(1,0);
  _z0reco = fit.get_z();
  _mxreco = state(2,0);
  _myreco = state(3,0);
  _chi2 = fit.get_chisquare();
  _ndf = fit.get_ndf();

  _size = 0;
  _xhit.assign(0);
  _yhit.assign(0);
  _zhit.assign(0);

  // Store the VTX hits
  if ( !_use_svx_cluster ) 
    {
      TFvtxPisaHitMap::key_iterator hit_iter = trk_ptr->get()->get_associated<TFvtxPisaHit>();
      _nVtx = hit_iter.count();
      while ( TFvtxPisaHitMap::const_pointer hit_ptr = hit_iter.next() )
        {
          if ( _size >= 20 ) break;
          const SvxSnglPisaHit* hit = hit_ptr->get()->get_pisa_hit();
          _xhit[_size] = hit->GetXGlobal();
          _yhit[_size] = hit->GetYGlobal();
          _zhit[_size] = hit->GetZGlobal();
          _size++;
        }
    }
  else
    {
      TFvtxSvxClusterMap::key_iterator key_iter( trk_ptr->get()->get_associated<TFvtxSvxCluster>() );
      _nVtx = key_iter.count();
      while( TFvtxSvxClusterMap::const_pointer ptr = key_iter.next() )
        { 
          if ( _size >= 20 ) break; 
          const SvxCluster* hit = ptr->get()->get_cluster();
          _xhit[_size] = hit->get_xyz_global(0);
          _yhit[_size] = hit->get_xyz_global(1);
          _zhit[_size] = hit->get_xyz_global(2);
          _size++;    
        }  
    }


  // Store the FVTX hits
  TFvtxCoordMap::key_iterator coord_iter = trk_ptr->get()->get_associated<TFvtxCoord>();
  _nFvtx = coord_iter.count();
  while ( TFvtxCoordMap::const_pointer coord_ptr = coord_iter.next() )
    {
      if ( _size >= 20 ) break;
      PHPoint pnt = coord_ptr->get()->get_coord_midpoint();
      _xhit[_size] = pnt.getX();
      _yhit[_size] = pnt.getY();
      _zhit[_size] = pnt.getZ();
      _size++;
    }

  TFvtxStraightTrkParMap::key_iterator par_iter = trk_ptr->get()->get_associated<TFvtxStraightTrkPar>();

  // Store the residuals
  _nresid = 0;
  _dR.assign(0);
  _dPhi.assign(0);

  TFvtxResidualMap::key_iterator res_iter = par_iter.current()->get()->get_associated<TFvtxResidual>();
  //_nresid = res_iter.count();
  //if ( _nresid != nFvtx + nVtx ) std::cout << "nresid = " << _nresid << std::endl;
  while ( TFvtxResidualMap::const_pointer res_ptr = res_iter.next() )
    {
      if ( _nresid >= 20 ) break;
      _dR[_nresid] = res_ptr->get()->get_dr();
      _dPhi[_nresid] = res_ptr->get()->get_dphi();
      _nresid++;
    }

  // Store the covariance matrix and diagonalized covariance matrix
  //
  PHGslMatrix covar(TFvtxStraightTrkPar::COVAR_ROW,TFvtxStraightTrkPar::COVAR_ROW);
  PHGslMatrix temp(TFvtxStraightTrkPar::COVAR_ROW,TFvtxStraightTrkPar::COVAR_ROW);
  par_iter.reset();
  //TFvtxStraightTrkParMap::key_iterator par_iter = trk_ptr->get()->get_associated<TFvtxStraightTrkPar>();
  if ( TFvtxStraightTrkParMap::const_pointer par_ptr = par_iter.next() )
    {
      for(int i=0; i<TFvtxStraightTrkPar::COVAR_ROW; i++)
        {
          for(int j=0; j<TFvtxStraightTrkPar::COVAR_ROW; j++)
            {
              covar(i,j) = _covar[i][j] = par_ptr->get()->get_covar(i,j);
            }
        }

      PHGslMatrix upperBlock(2,2);
      upperBlock(0,0) = covar(0,0);
      upperBlock(0,1) = covar(0,1);
      upperBlock(1,0) = covar(1,0);
      upperBlock(1,1) = covar(1,1);

      PHGslMatrix lowerBlock(2,2);
      lowerBlock(0,0) = covar(2,2);
      lowerBlock(0,1) = covar(2,3);
      lowerBlock(1,0) = covar(3,2);
      lowerBlock(1,1) = covar(3,3);
      
      PHGslMatrix upperDiag = upperBlock.diagonalize();
      PHGslMatrix lowerDiag = lowerBlock.diagonalize();

      for(int i=0; i<4; i++) 
        {
          if ( i<2 ) temp(i,i) = _covard[i] = upperDiag(i,i);
          else temp(i,i) = _covard[i] = lowerDiag(i-2,i-2);
        }

      //covar.print();
      //temp.print();

    }
  _eval_tree->Fill();
}

void
mFvtxStraightLineFit::set_interface_ptrs(PHCompositeNode* top_node)
{
  _hit_map = TMutNode<TFvtxHitMap>::find_node(top_node,"TFvtxHitMap");
  //_vtxhit_map = TMutNode<TFvtxPisaHitMap>::find_node(top_node,"TFvtxPisaHitMap");
  _coord_map = TMutNode<TFvtxCoordMap>::find_node(top_node,"TFvtxCoordMap");
  _trk_map = TMutNode<TFvtxTrkMap>::find_node(top_node,"TFvtxTrkMap");
  _par_map = TMutNode<TFvtxStraightTrkParMap>::find_node(top_node,"TFvtxStraightTrkParMap");
  _resid_map = TMutNode<TFvtxResidualMap>::find_node(top_node,"TFvtxResidualMap");
}

PHBoolean
mFvtxStraightLineFit::event(PHCompositeNode* topNode)
{
  _ievent++;

  _timer.get()->restart();

  set_interface_ptrs(topNode);

  fit();

  _timer.get()->stop();

  return 0;
}

void
mFvtxStraightLineFit::set_par(TFvtxTrkMap::pointer trk_ptr,
                              const TMutStraightTrackFit& fit)
{
  TFvtxStraightTrkParMap::iterator par_iter = _par_map->insert_new( trk_ptr->get()->get_arm() );
  TFvtxStraightTrkParMap::pointer p = par_iter.current();

  PHGslMatrix state = fit.get_state();
  p->get()->set_x(state(0,0));
  p->get()->set_y(state(1,0));
  p->get()->set_mx(state(2,0));
  p->get()->set_my(state(3,0));
  p->get()->set_chi_square(fit.get_chisquare());
  p->get()->set_ndf(fit.get_ndf());

  try
    {
      PHGslMatrix cov = fit.get_gain().invert();
      for (unsigned int i = 0; i < TFvtxStraightTrkPar::COVAR_ROW; i++)
        {
          for (unsigned int j = 0; j < TFvtxStraightTrkPar::COVAR_ROW; j++)
            {
              p->get()->set_covar(i, j, cov(i, j));
            }
        }
    }
  catch (const std::exception& e)
    {
      static int count = 0;
      count ++;

      if (FVTXOO::special_event_num(count))
        {

          std::cout<<"mFvtxStraightLineFit::set_par - "<<count<<" errors - "
              <<"event = "<<_ievent<<", track = "<<_trackid
              <<e.what()<<". Matrix = "<<std::endl;
          fit.get_gain().print(std::cout);
        }
    }
  

  // Create and associate residuals for this track fit par object
  //
  for (unsigned int i=0; i<_node_z.size(); i++)
    {
      // Residual in the R direction at this z measurement
      PHGslMatrix resR = _rnodes[i].get_m();
      resR -= _rnodes[i].get_h()*state;
      
      // Residual in the Phi direction at this z measurement
      PHGslMatrix resPhi = _phinodes[i].get_m();
      resPhi -= _phinodes[i].get_h()*state;

      TFvtxResidualMap::iterator res_iter = _resid_map->insert_new( trk_ptr->get()->get_arm() );
      TFvtxResidualMap::pointer res_p = res_iter.current();
      
      res_p->get()->set_z(_node_z[i]);
      res_p->get()->set_dr(resR(0,0));
      res_p->get()->set_dphi(resPhi(0,0));
      
      PHKey::associate(p,res_p);
    }
  
  PHKey::associate(trk_ptr,p);
}

void
mFvtxStraightLineFit::fit()
{
  if ( ! _trk_map ) return;

  _par_map->clear();

  TFvtxTrkMap::iterator iter( _trk_map->range() );
  while( TFvtxTrkMap::pointer trk_ptr = iter.next() )
    {
      TMutStraightTrackFit fit;
      fit.set_z(_vertex_z);

      _node_z.clear();
      _rnodes.clear();
      _phinodes.clear();
    
     
      //  Add VTX hits
      if ( !_use_svx_cluster )
        {
          TFvtxPisaHitMap::key_iterator vtxhit_iter = trk_ptr->get()->get_associated<TFvtxPisaHit>();
          while(TFvtxPisaHitMap::const_pointer vtxhit_ptr = vtxhit_iter.next() )
            {
              // smear phi and z coordinates.
              //
              const SvxSnglPisaHit* hit = vtxhit_ptr->get()->get_pisa_hit();
              double x = hit->GetXGlobal();
              double y = hit->GetYGlobal();
              double z = hit->GetZGlobal();
              PHCylPoint pnt = PHPoint(x,y,z);
        
              double r = pnt.getR();
              double z_width = PIXEL_ZLENGTH/sqrt(12.0);
              double phi_width = PHI_STRIP_RESOL/r/sqrt(12.0);
              double z_smear = (2.0* drand48() - 1.0)*z_width;
              double phi_smear = (2.0*drand48() - 1.0)*phi_width;
        
              VtxRNode r_node(vtxhit_ptr,z_smear,phi_smear);
              VtxPhiNode phi_node(vtxhit_ptr,z_smear,phi_smear);
        
              _node_z.push_back(z);
              _rnodes.push_back(r_node);
              _phinodes.push_back(phi_node);
        
              fit.add_node(r_node);
              fit.add_node(phi_node);
            }      
        }
      else 
        {
          TFvtxSvxClusterMap::key_iterator vtxhit_iter = trk_ptr->get()->get_associated<TFvtxSvxCluster>();
          while(TFvtxSvxClusterMap::const_pointer vtxhit_ptr = vtxhit_iter.next() )
            {
              // smear phi and z coordinates.
              //
              const SvxCluster* hit = vtxhit_ptr->get()->get_cluster();
              double x = hit->get_xyz_global(0);
              double y = hit->get_xyz_global(1);
              double z = hit->get_xyz_global(2);
              PHCylPoint pnt = PHPoint(x,y,z);
        
              double r = pnt.getR();
              double z_width = PIXEL_ZLENGTH/sqrt(12.0);
              double phi_width = PHI_STRIP_RESOL/r/sqrt(12.0);
              double z_smear = (2.0* drand48() - 1.0)*z_width;
              double phi_smear = (2.0*drand48() - 1.0)*phi_width;
        
              VtxRNode r_node(vtxhit_ptr,z_smear,phi_smear);
              VtxPhiNode phi_node(vtxhit_ptr,z_smear,phi_smear);
        
              _node_z.push_back(z);
              _rnodes.push_back(r_node);
              _phinodes.push_back(phi_node);
        
              fit.add_node(r_node);
              fit.add_node(phi_node);
            }      
        }
    
      // Add FVTX hits
      //
      TFvtxCoordMap::key_iterator coord_iter = trk_ptr->get()->get_associated<TFvtxCoord>();
      while ( TFvtxCoordMap::const_pointer coord_ptr = coord_iter.next() )
        {
          PHPoint pnt(coord_ptr->get()->get_coord_midpoint());
      
          FvtxRNode r_node(coord_ptr);
          FvtxPhiNode phi_node(coord_ptr);
      
          fit.add_node(r_node);
          fit.add_node(phi_node);   
      
          _node_z.push_back(pnt.getZ());
          _rnodes.push_back(r_node);
          _phinodes.push_back(phi_node);
        }
    
      try
        {
          if ( ! fit.fit() ) std::cout << "WARNING: straightline fit failed" << std::endl;
        }
      catch(const std::exception& e)
        {
          static int countFitErr = 0;
          countFitErr++;
      
          if (FVTXOO::special_event_num(countFitErr))
            {
              std::cout << "mFvtxStraightLineFit::fit - " << countFitErr << " errors: " << e.what() << std::endl;
            }
        }
    
      set_par(trk_ptr,fit);

      if ( _do_evaluation ) fill_trees(trk_ptr,fit);
    }
}

mFvtxStraightLineFit::FvtxRNode::FvtxRNode(TFvtxCoordMap::const_pointer coord_ptr)
{
  PHGslMatrix m( 1, 1 ); // measurement matrix
  PHGslMatrix cov( 1, 1 ); // covariance matrix
  PHGslMatrix h( 1, 4 ); // transfer matrix (row vector)
  
  // Get points at ends of strip:
  //PHPoint begin = coord_ptr->get()->get_coord_begin();
  //PHPoint end = coord_ptr->get()->get_coord_end();

  PHCylPoint pnt = coord_ptr->get()->get_coord_midpoint();

  // Ooops. w is a signed quantity. Do not use.
  //m( 0, 0 ) = coord_ptr->get()->get_w_absolute();
  m( 0, 0 ) = pnt.getR();
  cov( 0, 0 ) = SQ( coord_ptr->get()->get_error() );

  // Calculate strip angle
  double angle = pnt.getPhi();

  angle = FVTXOO::angle_normalize(angle); // make sure we're on (-pi,pi]
  // Now map the angle to 0,2pi
  if ( angle < 0.0 ) angle += 2.0*M_PI;

  // transfer matrix
  h( 0, 0 ) =  cos(angle);
  h( 0, 1 ) =  sin(angle);
  h( 0, 2 ) =  cos(angle)*(coord_ptr->get()->get_coord_midpoint().getZ());
  h( 0, 3 ) =  sin(angle)*(coord_ptr->get()->get_coord_midpoint().getZ());

  set_measurement( m, cov );
  set_h( h );
}

mFvtxStraightLineFit::FvtxPhiNode::FvtxPhiNode(TFvtxCoordMap::const_pointer coord_ptr)
{
  PHGslMatrix m( 1, 1 ); // measurement matrix
  PHGslMatrix cov( 1, 1 ); // covariance matrix
  PHGslMatrix h( 1, 4 ); // transfer matrix (rotation into measurement frame)
  
  // Get points at ends of strip:
  PHPoint begin = coord_ptr->get()->get_coord_begin();
  PHPoint end = coord_ptr->get()->get_coord_end();

  double deltaX = end.getX() - begin.getX();
  double deltaY = end.getY() - begin.getY();
  double strip_length = sqrt( SQ(deltaX) + SQ(deltaY) );

  m( 0, 0 ) = 0.0; // measurement is at the 'center' of the rotated coordinates
  cov( 0, 0 ) = SQ(strip_length)/12.0; // square of RMS of strip's length

  // Calculate hit phi angle
  PHCylPoint pnt = coord_ptr->get()->get_coord_midpoint();
  double angle = pnt.getPhi();

  angle = FVTXOO::angle_normalize(angle); // ensure (-pi,pi]
  // Now map the angle to 0,2pi
  if ( angle < 0.0 ) angle += 2.0*M_PI;

  // transfer matrix
  h( 0, 0 ) = -sin(angle);
  h( 0, 1 ) =  cos(angle);
  h( 0, 2 ) = -sin(angle)*(coord_ptr->get()->get_coord_midpoint().getZ());
  h( 0, 3 ) =  cos(angle)*(coord_ptr->get()->get_coord_midpoint().getZ());

  set_measurement( m, cov );
  set_h( h );
}

mFvtxStraightLineFit::VtxRNode::VtxRNode(TFvtxPisaHitMap::const_pointer hit_ptr, 
                                         const double zsmear, const double phismear)
{
  PHGslMatrix m( 1, 1 ); // measurement matrix
  PHGslMatrix cov( 1, 1 ); // covariance matrix
  PHGslMatrix h( 1, 4 ); // transfer matrix (row vector)
 
  const SvxSnglPisaHit* hit = hit_ptr->get()->get_pisa_hit();

  double x = hit->GetXGlobal();
  double y = hit->GetYGlobal();
  double z = hit->GetZGlobal();

  PHPoint cartPnt(x,y,z);
  PHCylPoint pnt = cartPnt;

  // Smear the angle and normalize it
  double angle = pnt.getPhi();
  angle += phismear;
  angle = FVTXOO::angle_normalize(angle);
  if ( angle < 0.0 ) angle += 2.0*M_PI;

  // Measurement and covariance
  m( 0, 0 ) = pnt.getR();
  cov( 0, 0 ) = SQ( R_THICK )/12.0;

  // transfer matrix
  h( 0, 0 ) =  cos(angle);
  h( 0, 1 ) =  sin(angle);
  h( 0, 2 ) =  cos(angle)* (z + zsmear);
  h( 0, 3 ) =  sin(angle)* (z + zsmear);

  set_measurement( m, cov );
  set_h( h );
}

mFvtxStraightLineFit::VtxPhiNode::VtxPhiNode(TFvtxPisaHitMap::const_pointer hit_ptr,
                                             const double zsmear, const double phismear)
{
  PHGslMatrix m( 1, 1 ); // measurement matrix
  PHGslMatrix cov( 1, 1 ); // covariance matrix
  PHGslMatrix h( 1, 4 ); // transfer matrix (row vector)
 
  const SvxSnglPisaHit* hit = hit_ptr->get()->get_pisa_hit();

  double x = hit->GetXGlobal();
  double y = hit->GetYGlobal();
  double z = hit->GetZGlobal();

  PHPoint cartPnt(x,y,z);
  PHCylPoint pnt = cartPnt;

  // Smear the angle and normalize it
  double angle = pnt.getPhi();
  angle += phismear;
  angle = FVTXOO::angle_normalize(angle);
  if ( angle < 0.0 ) angle += 2.0*M_PI;

  // Measurement and covariance
  m( 0, 0 ) = 0.0;
  cov( 0, 0 ) = SQ( PHI_STRIP_RESOL )/12.0;

  // transfer matrix
  h( 0, 0 ) = -sin(angle);
  h( 0, 1 ) =  cos(angle);
  h( 0, 2 ) = -sin(angle) * (z + zsmear);
  h( 0, 3 ) =  cos(angle) * (z + zsmear);

  set_measurement( m, cov );
  set_h( h );
}

mFvtxStraightLineFit::VtxRNode::VtxRNode(TFvtxSvxClusterMap::const_pointer clus_ptr, 
                                         const double zsmear, const double phismear)
{
  PHGslMatrix m( 1, 1 ); // measurement matrix
  PHGslMatrix cov( 1, 1 ); // covariance matrix
  PHGslMatrix h( 1, 4 ); // transfer matrix (row vector)
 
  const SvxCluster* cluster = clus_ptr->get()->get_cluster();

  double x = cluster->get_xyz_global(0);
  double y = cluster->get_xyz_global(0);
  double z = cluster->get_xyz_global(0);

  PHPoint cartPnt(x,y,z);
  PHCylPoint pnt = cartPnt;

  // Smear the angle and normalize it
  double angle = pnt.getPhi();
  angle += phismear;
  angle = FVTXOO::angle_normalize(angle);
  if ( angle < 0.0 ) angle += 2.0*M_PI;

  // Measurement and covariance
  m( 0, 0 ) = pnt.getR();
  cov( 0, 0 ) = SQ( R_THICK )/12.0;

  // transfer matrix
  h( 0, 0 ) =  cos(angle);
  h( 0, 1 ) =  sin(angle);
  h( 0, 2 ) =  cos(angle)* (z + zsmear);
  h( 0, 3 ) =  sin(angle)* (z + zsmear);

  set_measurement( m, cov );
  set_h( h );
}

mFvtxStraightLineFit::VtxPhiNode::VtxPhiNode(TFvtxSvxClusterMap::const_pointer clus_ptr,
                                             const double zsmear, const double phismear)
{
  PHGslMatrix m( 1, 1 ); // measurement matrix
  PHGslMatrix cov( 1, 1 ); // covariance matrix
  PHGslMatrix h( 1, 4 ); // transfer matrix (row vector)
 
  const SvxCluster* cluster = clus_ptr->get()->get_cluster();

  double x = cluster->get_xyz_global(0);
  double y = cluster->get_xyz_global(0);
  double z = cluster->get_xyz_global(0);

  PHPoint cartPnt(x,y,z);
  PHCylPoint pnt = cartPnt;

  // Smear the angle and normalize it
  double angle = pnt.getPhi();
  angle += phismear;
  angle = FVTXOO::angle_normalize(angle);
  if ( angle < 0.0 ) angle += 2.0*M_PI;

  // Measurement and covariance
  m( 0, 0 ) = 0.0;
  cov( 0, 0 ) = SQ( PHI_STRIP_RESOL )/12.0;

  // transfer matrix
  h( 0, 0 ) = -sin(angle);
  h( 0, 1 ) =  cos(angle);
  h( 0, 2 ) = -sin(angle) * (z + zsmear);
  h( 0, 3 ) =  cos(angle) * (z + zsmear);

  set_measurement( m, cov );
  set_h( h );
}
