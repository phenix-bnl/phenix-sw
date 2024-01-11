// This module uses hits from the VTX and tries to constrain
// tracks even furter than the mFvtxFindTracks module.
#include <mFvtxAddVtxHits.h>
#include <PHCompositeNode.h>
#include <Fun4AllServer.h>
#include <recoConsts.h>

#include <PHTFileServer.h>
#include <PHCylPoint.h>
#include <mFvtxModuleBase.h>
#include <TMutMCTrkMap.h>
#include <TFvtxTrkMap.h>
#include <TFvtxMCHitMap.h>
#include <TFvtxPisaHitMap.h>
#include <TFvtxCoordMap.h>
#include <SvxSnglPisaHit.h>
#include <FVTXOO.h>
#include <FVTXGEOM.h>
#include <SvxClusterList.h>
#include <TFvtxSvxClusterMap.h>

#include <TTree.h>
#include <TRandom3.h>

#include <PHGslMatrix.h>
#include <iomanip>
#include <iostream>
#include <map>
#include <vector>
#include <numeric>
#include <boost/bind.hpp>

using boost::bind;

// Shorthand for squaring a number
template<typename T> T SQ(const T& x) { return x*x; }

// Utility to all STL access to delete
template<typename T> struct Deleter : public std::unary_function<void,T>
{
  void operator()(const T* v) const { delete v; }
};

// Analytically perform a linear fit, using constant weights
//
template<typename T> std::pair<T,T>
linearFit(const std::vector<double>& x, const std::vector<double>& y)
{
  int n = x.size();
  std::vector<T> x2(x.size());
  std::vector<T> xy(x.size());
  std::transform(x.begin(),x.end(),x.begin(),x2.begin(),std::multiplies<T>());
  std::transform(x.begin(),x.end(),y.begin(),xy.begin(),std::multiplies<T>());

  T sumX  = std::accumulate(x.begin(),x.end(),0.0);
  T sumXY = std::accumulate(xy.begin(),xy.end(),0.0);
  T sumY  = std::accumulate(y.begin(),y.end(),0.0);
  T sumX2 = std::accumulate(x2.begin(),x2.end(),0.0);
  T delta = n*sumX2 - sumX*sumX;

  T slope  = 1.0 * (n*sumXY - sumX*sumY)/delta;
  T offset = 1.0 * (sumX2*sumY - sumX*sumXY)/delta;

  return std::pair<T,T>(slope,offset);
}

template<typename T> std::pair<T,T>
weighted_linearFit(const std::vector<T>& x, const std::vector<T>& y, const std::vector<T>& w)
{
  
  int n = x.size();
  std::vector<T> wx2(x.size());
  std::vector<T> wxy(x.size());
  std::vector<T> wx(x.size());
  std::vector<T> wy(x.size());

  for (int i=0; i< n; i++)
    {
      wx[i] =  w[i] * x[i];
      wxy[i] = w[i] * x[i] * y[i];
      wx2[i] = w[i] * x[i] * x[i];
      wy[i] = w[i] * y[i];

    }

  T sumWX  = std::accumulate(wx.begin(),wx.end(),0.0);
  T sumWXY = std::accumulate(wxy.begin(),wxy.end(),0.0);
  T sumWY  = std::accumulate(wy.begin(),wy.end(),0.0);
  T sumWX2 = std::accumulate(wx2.begin(),wx2.end(),0.0);
  T sumW = std::accumulate(w.begin(),w.end(),0.0);

  T delta = sumW*sumWX2 - sumWX*sumWX;

  T slope  = 1.0 * (sumW*sumWXY - sumWX*sumWY)/delta;
  T offset = 1.0 * (sumWX2*sumWY - sumWX*sumWXY)/delta;
  
  return std::pair<T,T>(slope,offset);
}

template<typename T> T
drin_dalpha(const int j, const T phi, const T z)
{
  T drin = 0.0;
  if ( j==0 ) drin = cos(phi);
  if ( j==1 ) drin = z * cos(phi);
  if ( j==2 ) drin = sin(phi);
  if ( j==3 ) drin = z * sin(phi);

  return drin;
}

template<typename T> T
drout_dalpha(const int j, const T phi, const T z)
{
  T drout = 0.0;
  if ( j==0 ) drout = -sin(phi);
  if ( j==1 ) drout = -z * sin(phi);
  if ( j==2 ) drout = cos(phi);
  if ( j==3 ) drout = z * cos(phi);

  return drout;
}

template<typename T> PHGslMatrix
minimize_chiSquare(
		   const std::vector<T>& x, // x measurements
		   const std::vector<T>& y, // y measurements
		   const std::vector<T>& z, // z location of measurements
                   const std::vector<T>& phi, // phi's of measurements
		   const std::vector<T>& w_in, // weights for in-plane coordinate
		   const std::vector<T>& w_out // weights for out-of-plane coordinate
		   )
{

  unsigned int n = x.size();

  PHGslMatrix M(4,4);
  PHGslMatrix D(4,1);

  T tmp[4] = {0.0};

  //  std::cout << "calculating matrix elements..." << std::endl;
  // calculate matrix elements                                                                       
  for (int j=0; j<4; j++) // row                                                                     
    {
      //      std::cout << "\t" << "row = " << j << std::endl;

      T val = 0.0;
      tmp[0] = 0.0;
      tmp[1] = 0.0;
      tmp[2] = 0.0;
      tmp[3] = 0.0;

      for (unsigned int i=0; i<n; i++)
        {
          T drin = drin_dalpha<T>(j,phi[i],z[i]);
          T drout = drout_dalpha<T>(j,phi[i],z[i]);

	  //        std::cout << "drin = " << drin << std::endl;                                               
	  //        std::cout << "drout = " << drout << std::endl;                                             
	  
	  // precalculate the sin/cos
	  //
	  T sin_phi = sin(phi[i]);
	  T cos_phi = cos(phi[i]);

          tmp[0] += cos_phi * drin * w_in[i] - sin_phi * drout * w_out[i];
          tmp[1] += z[i] * (cos_phi * drin * w_in[i] - sin_phi * drout * w_out[i]);
          tmp[2] += sin_phi * drin * w_in[i] + cos_phi * drout * w_out[i];
          tmp[3] += z[i] * (sin_phi * drin * w_in[i] + cos_phi * drout * w_out[i]);

          T xin = x[i]*cos_phi + y[i]*sin_phi;
          T xout = -x[i]*sin_phi + y[i]*cos_phi;
          val += xin * drin * w_in[i] + xout * drout * w_out[i];

        }

      M.set(j,0,tmp[0]);
      M.set(j,1,tmp[1]);
      M.set(j,2,tmp[2]);
      M.set(j,3,tmp[3]);
      D.set(j,0,val);
    }

//   std::cout << "\n" << "Matrix = " << std::endl;
//   for (int k=0; k<4; k++)
//     {
//       std::cout << M.get(k,0) << "\t" << M.get(k,1) << "\t"
//                 << M.get(k,2) << "\t" << M.get(k,3) << std::endl;
//     }

//   std::cout << "\ninverting Matrix" << std::endl;
  PHGslMatrix Minv = M.invert();
  PHGslMatrix param = Minv*D;

  return param;
}

template<typename T> std::pair<std::vector<T>,std::vector<T> >
rotate_into_wedge(const std::vector<T>& x, const std::vector<T>& y, 
		  const std::vector<T>& z, const T& phi)
{
  std::vector<T> in, out;
  unsigned int n = x.size();
//   T phi_rot = 0.0;

//   // find the wedge angle in first station.
//   for (unsigned int i=0; i < n; i++)
//     {
//       if  ( std::abs(z[i]) < 20.0 && std::abs(z[i]) > 17.0 )
// 	{
// 	  phi_rot = phi[i];
// 	  break;
// 	}
//     }

  T sin_phi = sin(phi);
  T cos_phi = cos(phi);

  // rotate coordinates
  for (unsigned int i=0; i < n; i++)
    {
      in.push_back( x[i] * cos_phi + y[i] * sin_phi );
      out.push_back( -x[i] * sin_phi + y[i] * cos_phi );
    } 


  return std::pair<std::vector<T>,std::vector<T> >(in,out);
}





mFvtxAddVtxHits::mFvtxAddVtxHits() :
  _use_svx_cluster(false),
  _do_evaluation(false),
  _evalFilename("mFvtxAddVtxHits_eval.root"),
  _dPhi(0.087),
  _dZcut(0.1),
  _ievent(0),
  _trk_ab_tree(0),
  _track(0),
  _size(mFvtxAddVtxHits::max_size),
  _timer("mFvtxAddVtxHits")
{
  FVTXOO::TRACE("Initializing module mFvtxAddHits compiled " __DATE__ " " __TIME__ );
  clear_track();
}

mFvtxAddVtxHits::~mFvtxAddVtxHits() {}

void
mFvtxAddVtxHits::init(PHCompositeNode*)
{
  if ( _do_evaluation )
    {
      PHTFileServer::get().open( _evalFilename, "RECREATE" );
      std::cout << "mFvtxAddVtxHits::init: Writing to file " << _evalFilename << std::endl;
      book_trees();
    }
  _rnd.SetSeed(0);
}

PHBoolean
mFvtxAddVtxHits::event(PHCompositeNode* topNode)
{
  _timer.restart();

  _ievent++;

  if ( _ievent%1000==0 )
    std::cout << "mFvtxAddVtxHits ievent = " << _ievent << std::endl;

  // find reconstructed tracks and find hits in the barrel associated with
  // this track.
  set_interface_ptrs(topNode);

  find_tracks();
  
  _timer.stop();
  
  return 0;
}

void
mFvtxAddVtxHits::end(PHCompositeNode*)
{
  if ( _do_evaluation ) _trk_ab_tree->AutoSave();
  _timer.print_stat();
  
}

void
mFvtxAddVtxHits::set_interface_ptrs(PHCompositeNode* top_node)
{
  if ( _use_svx_cluster) 
  {
    _clus_map = TMutNode<TFvtxSvxClusterMap>::find_node(top_node, "TFvtxSvxClusterMap");
    //std::cout << "SvxClusterMap size " << _clus_map->count() << std::endl;

    TFvtxSvxClusterMap::iterator test_clus_iter( _clus_map->range() );
    while( TFvtxSvxClusterMap::pointer ptr = test_clus_iter.next() )
    {
      std::cout << "mFvtxAddVtxHits::set_interface_ptrs layer " << ptr->get()->get_cluster()->get_layer() << std::endl;
    }
  }
  else 
  {
    PHCompositeNode* signal_top_node = 0;
    recoConsts *rc = recoConsts::instance();

    if ( rc->FlagExist("EMBED_MC_TOPNODE") )
    {
      //cout << "mFvtxAddVtxHits::set_interface_ptrs - reading _signalNodeName from recoConst EMBED_MC_TOPNODE" << endl;
      signal_top_node = Fun4AllServer::instance()->topNode(  rc->get_CharFlag("EMBED_MC_TOPNODE") );
    }

    // Input Node(s)
    _mut_mc_trk_map = TMutNode<TMutMCTrkMap>::find_node(signal_top_node,"TMutMCTrkMap");  
    _mc_hit_map = TMutNode<TFvtxMCHitMap>::find_node(signal_top_node,"TFvtxMCHitMap");
    _pisa_hit_map = TMutNode<TFvtxPisaHitMap>::find_node(signal_top_node,"TFvtxPisaHitMap");

    //std::cout << "_mc_hit_map size " << _mc_hit_map->count() << std::endl;
  }
  _coord_map = TMutNode<TFvtxCoordMap>::find_node(top_node,"TFvtxCoordMap");

  // Output Node
  _trk_map = TMutNode<TFvtxTrkMap>::find_node(top_node,"TFvtxTrkMap");
  
}

void
mFvtxAddVtxHits::book_trees()
{
  if (_trk_ab_tree) delete _trk_ab_tree;

  _trk_ab_tree = new TTree("trk_ab_tree","trk_ab_tree");
  _trk_ab_tree->Branch("event",&_ievent,"event/I",BUFFER_SIZE);
  _trk_ab_tree->Branch("track",&_track,"track/I",BUFFER_SIZE);
  _trk_ab_tree->Branch("size",&_size,"size/I",BUFFER_SIZE);
  _trk_ab_tree->Branch("rchi2",&_rchi2,"rchi2/F",BUFFER_SIZE);
  _trk_ab_tree->Branch("rmc_slopeFit",&_rmc_slopeFit,"rmc_slopeFit/F",BUFFER_SIZE);
  _trk_ab_tree->Branch("rmc_offsetFit",&_rmc_offsetFit,"rmc_offsetFit/F",BUFFER_SIZE);
  _trk_ab_tree->Branch("phimc_slopeFit",&_phimc_slopeFit,"phimc_slopeFit/F",BUFFER_SIZE);
  _trk_ab_tree->Branch("phimc_offsetFit",&_phimc_offsetFit,"phimc_offsetFit/F",BUFFER_SIZE);
  _trk_ab_tree->Branch("xmc_int",&_xmc_int,"xmc_int/F",BUFFER_SIZE);
  _trk_ab_tree->Branch("ymc_int",&_ymc_int,"ymc_int/F",BUFFER_SIZE);
  _trk_ab_tree->Branch("xmc_slope",&_xmc_slope,"xmc_slope/F",BUFFER_SIZE);
  _trk_ab_tree->Branch("ymc_slope",&_ymc_slope,"ymc_slope/F",BUFFER_SIZE);
  _trk_ab_tree->Branch("r_slopeFit",&_r_slopeFit,"r_slopeFit/F",BUFFER_SIZE);
  _trk_ab_tree->Branch("r_offsetFit",&_r_offsetFit,"r_offsetFit/F",BUFFER_SIZE);
  _trk_ab_tree->Branch("phi_slopeFit",&_phi_slopeFit,"phi_slopeFit/F",BUFFER_SIZE);
  _trk_ab_tree->Branch("phi_offsetFit",&_phi_offsetFit,"phi_offsetFit/F",BUFFER_SIZE);
  _trk_ab_tree->Branch("x_int",&_x_int,"x_int/F",BUFFER_SIZE);
  _trk_ab_tree->Branch("y_int",&_y_int,"y_int/F",BUFFER_SIZE);
  _trk_ab_tree->Branch("x_slope",&_x_slope,"x_slope/F",BUFFER_SIZE);
  _trk_ab_tree->Branch("y_slope",&_y_slope,"y_slope/F",BUFFER_SIZE);
  _trk_ab_tree->Branch("phi_rot",&_phi_rot,"phi_rot/F",BUFFER_SIZE);
  _trk_ab_tree->Branch("inPlane_int",&_inPlane_int,"inPlane_int/F",BUFFER_SIZE);
  _trk_ab_tree->Branch("outPlane_int",&_outPlane_int,"outPlane_int/F",BUFFER_SIZE);
  _trk_ab_tree->Branch("inPlane_slope",&_inPlane_slope,"inPlane_slope/F",BUFFER_SIZE);
  _trk_ab_tree->Branch("outPlane_slope",&_outPlane_slope,"outPlane_slope/F",BUFFER_SIZE);  
  _trk_ab_tree->Branch("foundVtxHit",&_foundVtxHit,"foundVtxHit/O",BUFFER_SIZE);
  _trk_ab_tree->Branch("hasCharmParent",_hasCharmParent,"hasCharmParent[size]/O",BUFFER_SIZE);
  _trk_ab_tree->Branch("hasBottomParent",_hasBottomParent,"hasBottomParent[size]/O",BUFFER_SIZE);
  _trk_ab_tree->Branch("mc_tracknum",_mc_tracknum,"mc_tracknum[size]/I",BUFFER_SIZE);
  _trk_ab_tree->Branch("mc_hitnum",_mc_hitnum,"mc_hitnum[size]/I",BUFFER_SIZE);
  _trk_ab_tree->Branch("pid",_pid,"pid[size]/I",BUFFER_SIZE);
  _trk_ab_tree->Branch("parent_pid",_parent_pid,"parent_pid[size]/I",BUFFER_SIZE);
  _trk_ab_tree->Branch("halfWedgeId",_halfWedgeId,"halfWedgeId[size]/I",BUFFER_SIZE);
  _trk_ab_tree->Branch("nstrips",_nstrips,"nstrips[size]/I",BUFFER_SIZE);
  _trk_ab_tree->Branch("xmc",_xmc,"xmc[size]/F",BUFFER_SIZE);
  _trk_ab_tree->Branch("ymc",_ymc,"ymc[size]/F",BUFFER_SIZE);
  _trk_ab_tree->Branch("zmc",_zmc,"zmc[size]/F",BUFFER_SIZE);
  _trk_ab_tree->Branch("rmc",_rmc,"rmc[size]/F",BUFFER_SIZE);
  _trk_ab_tree->Branch("phimc",_phimc,"phimc[size]/F",BUFFER_SIZE);
  _trk_ab_tree->Branch("x",_x,"x[size]/F",BUFFER_SIZE);
  _trk_ab_tree->Branch("y",_y,"y[size]/F",BUFFER_SIZE);
  _trk_ab_tree->Branch("z",_z,"z[size]/F",BUFFER_SIZE);
  _trk_ab_tree->Branch("r",_r,"r[size]/F",BUFFER_SIZE);
  _trk_ab_tree->Branch("phi",_phi,"phi[size]/F",BUFFER_SIZE);
  _trk_ab_tree->Branch("inPlane",_inPlane,"inPlane[size]/F",BUFFER_SIZE);
  _trk_ab_tree->Branch("outPlane",_outPlane,"outPlane[size]/F",BUFFER_SIZE);
  _trk_ab_tree->Branch("x_cluster_width",_x_cluster_width,"x_cluster_width[size]/F",BUFFER_SIZE);
  _trk_ab_tree->Branch("y_cluster_width",_y_cluster_width,"y_cluster_width[size]/F",BUFFER_SIZE);
  _trk_ab_tree->Branch("x_cluster_begin",_x_cluster_begin,"x_cluster_begin[size]/F",BUFFER_SIZE);
  _trk_ab_tree->Branch("y_cluster_begin",_y_cluster_begin,"y_cluster_begin[size]/F",BUFFER_SIZE);
  _trk_ab_tree->Branch("x_cluster_end",_x_cluster_end,"x_cluster_end[size]/F",BUFFER_SIZE);
  _trk_ab_tree->Branch("y_cluster_end",_y_cluster_end,"y_cluster_end[size]/F",BUFFER_SIZE);
  _trk_ab_tree->SetAutoSave(AUTO_SAVE);

  return;
}


void 
mFvtxAddVtxHits::find_tracks()
{

  // loop thru all the reco tracks and add VTX hits
  if (_trk_map)
    {
      
      _track = 0;
      TFvtxTrkMap::iterator iter( _trk_map->range() );
      while( TFvtxTrkMap::pointer trk_ptr = iter.next() )
	{	  

          //ghost and bad track rejections
          if (trk_ptr->get()->get_ghost()) continue;
          if (not trk_ptr->get()->get_reco_success()) continue;

	  ++_track;
	  clear_track();
	  // 	  std::cout << "track = " << _track << std::endl;

	  TFvtxCoordMap::key_iterator coord_iter = trk_ptr->get()->get_associated<TFvtxCoord>();
	  while(TFvtxCoordMap::const_pointer coord_ptr = coord_iter.next() )
	    {

	      PHPoint cartPnt = coord_ptr->get()->get_coord_midpoint();
	      PHPoint cartPnt_begin = coord_ptr->get()->get_coord_begin();
	      PHPoint cartPnt_end = coord_ptr->get()->get_coord_end();
	      PHCylPoint pnt = cartPnt;

	      double xerr_begin = cartPnt_begin.getX();
	      double xerr_end = cartPnt_end.getX();
	      double yerr_begin = cartPnt_begin.getY();
	      double yerr_end = cartPnt_end.getY();
	      double deltaX = (xerr_begin - xerr_end)/sqrt(12);
	      double deltaY = (yerr_begin - yerr_end)/sqrt(12);
	      double strip_length = sqrt( SQ(deltaX) + SQ(deltaY) );
// 	      _x_begin.push_back(xerr_begin);
// 	      _y_begin.push_back(yerr_begin);
// 	      _x_end.push_back(xerr_end);
// 	      _y_end.push_back(yerr_end);

	      _rv.push_back(pnt.getR());
	      _xv.push_back(cartPnt.getX());
	      _xwv.push_back( 1.0/SQ(deltaX) );
	      _yv.push_back(cartPnt.getY());
	      _ywv.push_back( 1.0/SQ(deltaY) );
	      _zv.push_back(pnt.getZ());
	      
	      _r[_size] = pnt.getR();
	      _x[_size] = cartPnt.getX();
	      _y[_size] = cartPnt.getY();
	      _z[_size] = pnt.getZ();
	      _x_cluster_width[_size] = deltaX;
	      _y_cluster_width[_size] = deltaY;
	      _x_cluster_begin[_size] = xerr_begin;
	      _y_cluster_begin[_size] = yerr_begin;
	      _x_cluster_end[_size] = xerr_end;
	      _y_cluster_end[_size] = yerr_end;

	      double phi = pnt.getPhi();
 	      _phiv.push_back(FVTXOO::angle_normalize(phi));
	      _phi[_size] = FVTXOO::angle_normalize(phi);

	      if ( std::abs(_z[_size]) < 20.0 && std::abs(_z[_size]) > 17.0 ) _phi_rot = _phi[_size];

              if ( !_use_svx_cluster)
              { 
                // add any MC information needed.
	        TFvtxMCHitMap::key_iterator it = coord_ptr->get()->get_associated<TFvtxMCHit>();
	        if ( TFvtxMCHitMap::pointer p = it.current() )
	  	{
		 
	  	  int arm = p->get()->get_arm();
                  int cage = p->get()->get_cage();
                  int station = p->get()->get_station();
		  int sector = p->get()->get_sector();
		  int column = p->get()->get_column();
		  _halfWedgeId[_size] = FVTXGEOM::get_halfwedge_id(arm,cage,station,sector,column);
		  
		  _nstrips[_size] = p->get()->get_n_strip();
		  _xmc[_size] = p->get()->get_x();
		  _ymc[_size] = p->get()->get_y();
		  _zmc[_size] = p->get()->get_z();
		  _rmc[_size] = sqrt(_xmc[_size]*_xmc[_size] + _ymc[_size]*_ymc[_size]);
		  _phimc[_size] = atan2(_ymc[_size],_xmc[_size]);
		  
		  _xmcv.push_back(_xmc[_size]);
		  _ymcv.push_back(_ymc[_size]);
		  _zmcv.push_back(_zmc[_size]);
		  _rmcv.push_back(_rmc[_size]);
		  _phimcv.push_back(_phimc[_size]);

		  _mc_tracknum[_size] = p->get()->get_track_id();
		  _mc_hitnum[_size] = p->get()->get_index();

		  // Loop through TMutMCTrk to access pid's of hits.
		  TMutMCTrkMap::key_iterator mc_trk_iter = p->get()->get_associated<TMutMCTrk>();
		  if ( mc_trk_iter.count() )
		  {
		    _pid[_size] = mc_trk_iter.current()->get()->get_pid();
		    _parent_pid[_size] = mc_trk_iter.current()->get()->get_parent_id();
	          }
	 	}
              }
	      _coordnum[_size] = coord_ptr->get()->get_index();

	      float r_weight = _nstrips[_size]*0.0075/sqrt(12);
	      _rwv.push_back(1.0/SQ(r_weight));
	      _outw.push_back(1.0/SQ(strip_length/sqrt(12)));
	      _inw.push_back(1.0/SQ(r_weight));

	      ++_size;
	    }

	  // check to see if phi values are in the branch cut.  If they aren't, 
	  // then put all values in the same branch and find the average phi value.
	  //
	  std::vector<double>::iterator min_phi = std::min_element(_phiv.begin(),_phiv.end());
	  std::vector<double>::iterator max_phi = std::max_element(_phiv.begin(),_phiv.end());
	  if ( std::abs(*max_phi - *min_phi) > M_PI )
	    {
	      for (unsigned int k = 0; k < _phiv.size(); k++)
		{
		  if ( _phiv[k] < 0 ) _phiv[k] += 2 * M_PI;
		}
	    }
	  double avg_phi = std::accumulate(_phiv.begin(),_phiv.end(),0.0)/_phiv.size();
	  double phi_upper = avg_phi + _dPhi; // upper limit to phi range acceptance
	  double phi_lower = avg_phi - _dPhi; // lower limit to phi range acceptance
	  phi_upper = FVTXOO::angle_normalize(phi_upper);
	  phi_lower = FVTXOO::angle_normalize(phi_lower);
	  
	  // reconstruct track
	  //
	  boost::tie(_r_slopeFit,_r_offsetFit) = linearFit<double>(_zv,_rv);

	  // searching for VTX hits in vicinity of reco projection.  We use all hits withing vicinity of 
	  // this projection.	However, at some point a filtering algorithm has to be implemented where 
	  // only the best hit is used from a certain layer unless there are overlapping volumes. This 
	  // is most likely ok for pythia simulations, but will cause serious issues for hijing.
	  //

	  double z_smear[2];
	  double phi_smear[2];
	  double dR[2] = {9999.0,9999.0};  // value used to compare how close a hit is to its projection.
	  const double n_strips = 1.0;
	  const double phi_strip_resol = n_strips*0.0050/sqrt(12.0);
	  const double r_resol = 0.0200/sqrt(12);  // throwing out a number here for radial pitch of VTX pixels

          if ( !_use_svx_cluster)
          {
            std::vector<const SvxSnglPisaHit*> vtxHits(2);

       	    TFvtxPisaHitMap::iterator testhit_iter( _pisa_hit_map->range() );
	    while( TFvtxPisaHitMap::pointer ptr = testhit_iter.next() )
	    {
	      int layer = ptr->get()->get_pisa_hit()->GetLayer() - 1;

	      // For now we process only the pixel layers
	      if ( layer < 2 )
		{
		  const SvxSnglPisaHit *hit = ptr->get()->get_pisa_hit();

		  double x = hit->GetXGlobal();
		  double y = hit->GetYGlobal();
		  double z = hit->GetZGlobal();
		  //		  double zmc = z;
 		  double r = sqrt(x*x + y*y);
		  double phi = atan2(y,x);
 		  phi = FVTXOO::angle_normalize(phi);
		  //		  double phimc = phi;		  

		  // smear phi and z coordinates.  n_strips represents the number of strips fired
		  // by a single hit.
		  double z_width = 0.0450/sqrt(12.0);
		  double phi_width = phi_strip_resol/r;
		  z += (2* _rnd.Rndm() - 1)*z_width;
		  phi += (2*_rnd.Rndm() - 1)*phi_width;
 		  phi = FVTXOO::angle_normalize(phi);
		  FVTXOO::AngleOverlap<double> test;

		  double z_th = (r - _r_offsetFit)/_r_slopeFit;
		  double dZ = std::abs(z_th - z); 

		  if ( test(phi,phi,phi_lower,phi_upper) && (dZ < _dZcut) )
		    {
		      if ( dZ < dR[layer] )
			{

			  dR[layer] = dZ;
			  vtxHits[layer] = hit;
			  z_smear[layer] = z;
			  phi_smear[layer] = phi;

// 			  std::cout << "Acceting new hit in layer " << layer << std::endl;
// 			  std::cout << "Layer " << layer << " dZ = " << dZ << ";  dR = " 
// 				    << dR[layer] << std::endl;
			}
		    }
		}
	    }

	  //	  std::cout << "Filling node and tree with VTX hits" << std::endl;
	  
            TFvtxPisaHitMap::iterator acceptedHit_iter( _pisa_hit_map->range() );
	    while( TFvtxPisaHitMap::pointer ptr = acceptedHit_iter.next() )
	    {
	      if ( (ptr->get()->get_pisa_hit() != vtxHits[0]) && (ptr->get()->get_pisa_hit() != vtxHits[1]) )
		{
		  continue;
		}
	      
	      const SvxSnglPisaHit* hit = ptr->get()->get_pisa_hit();
	      int layer = ptr->get()->get_pisa_hit()->GetLayer() - 1;
	      
	      double x = hit->GetXGlobal();
	      double y = hit->GetYGlobal();
	      double zmc = hit->GetZGlobal();
	      double r = sqrt(x*x + y*y);
	      double phimc = atan2(y,x);
	      phimc = FVTXOO::angle_normalize(phimc);

	      // fill mc hits 
	      _rmcv.push_back(r);
	      _xmcv.push_back(x);
	      _ymcv.push_back(y);
	      _zmcv.push_back(zmc);
	      _phimcv.push_back(phimc);
	      _rmc[_size] = r;
	      _xmc[_size] = x;
	      _ymc[_size] = y;
	      _zmc[_size] = zmc;
	      _phimc[_size] = phimc;
	      
	      _rv.push_back(r);
	      _xv.push_back(r*cos(phi_smear[layer]));
	      //	      _xwv.push_back( 1.0/SQ(phi_strip_resol) );
	      _yv.push_back(r*sin(phi_smear[layer]));
	      //	      _ywv.push_back( 1.0/SQ(phi_strip_resol) );
	      _zv.push_back(z_smear[layer]);
	      _phiv.push_back(phi_smear[layer]);

	      const double weight_x = (phi_strip_resol*std::abs(sin(phi_smear[layer])) + r_resol*std::abs(cos(phi_smear[layer])))/sqrt(12);
	      const double weight_y = (phi_strip_resol*std::abs(cos(phi_smear[layer])) + r_resol*std::abs(sin(phi_smear[layer])))/sqrt(12);

	      _rwv.push_back(1.0/SQ(0.0200/sqrt(12)));
	      _xwv.push_back(1.0/SQ(weight_x));
	      _ywv.push_back(1.0/SQ(weight_y));
	      _inw.push_back(1.0/SQ(0.0200/sqrt(12)));
	      _outw.push_back(1.0/SQ(0.0050/sqrt(12)));

	      _foundVtxHit = true;
	      _r[_size] = r;
	      _x[_size] = r*cos(phi_smear[layer]);
	      _y[_size] = r*sin(phi_smear[layer]);
	      _z[_size] = z_smear[layer];
	      _x_cluster_width[_size] = phi_strip_resol;
	      _y_cluster_width[_size] = phi_strip_resol;
	      
	      _phi[_size] = phi_smear[layer];
	      _mc_tracknum[_size] = hit->GetMctrack();
	      _pid[_size] = hit->GetIdPart();
	      _halfWedgeId[_size] = -1;
	      _nstrips[_size] = int(n_strips);
	      //		      _parent_pid[_size] = mc_trk_iter.current()->get()->get_parent_id();
	      PHKey::associate( trk_ptr, ptr);
	      
	      ++_size;
	    }
          }
          else 
          {
            std::vector<const SvxCluster*> vtxClus(2);

            TFvtxSvxClusterMap::iterator test_clus_iter( _clus_map->range() );
            while( TFvtxSvxClusterMap::pointer ptr = test_clus_iter.next() )
            {
              int layer = ptr->get()->get_cluster()->get_layer();
              std::cout << "mFvtxAddSvxHit::layer " << layer << std::endl;

              // For now we process only the pixel layers
              if ( layer < 2 )
                {
                  const SvxCluster *clus = ptr->get()->get_cluster();

                  double x = clus->get_xyz_global(0);
                  double y = clus->get_xyz_global(1);
                  double z = clus->get_xyz_global(2);
                  //              double zmc = z;
                  double r = sqrt(x*x + y*y);
                  double phi = atan2(y,x);
                  phi = FVTXOO::angle_normalize(phi);
                  //              double phimc = phi;

                  // smear phi and z coordinates.  n_strips represents the number of strips fired
                  // by a single hit.
                  double z_width = 0.0450/sqrt(12.0);
                  double phi_width = phi_strip_resol/r;
                  z += (2* _rnd.Rndm() - 1)*z_width;
                  phi += (2*_rnd.Rndm() - 1)*phi_width;
                  phi = FVTXOO::angle_normalize(phi);
                  FVTXOO::AngleOverlap<double> test;

                  double z_th = (r - _r_offsetFit)/_r_slopeFit;
                  double dZ = std::abs(z_th - z);

                  if ( test(phi,phi,phi_lower,phi_upper) && (dZ < _dZcut) )
                    {
                      if ( dZ < dR[layer] )
                        {

                          dR[layer] = dZ;
                          vtxClus[layer] = clus;
                          z_smear[layer] = z;
                          phi_smear[layer] = phi;

//                        std::cout << "Acceting new hit in layer " << layer << std::endl;
//                        std::cout << "Layer " << layer << " dZ = " << dZ << ";  dR = "
//                                  << dR[layer] << std::endl;
                        }
                    }
                }
            }

          //      std::cout << "Filling node and tree with VTX hits" << std::endl;

            TFvtxSvxClusterMap::iterator acceptedClus_iter( _clus_map->range() );
            while( TFvtxSvxClusterMap::pointer ptr = acceptedClus_iter.next() )
            {
              if ( (ptr->get()->get_cluster() != vtxClus[0]) && (ptr->get()->get_cluster() != vtxClus[1]) )
                {
                  continue;
                }

              const SvxCluster *clus = ptr->get()->get_cluster();
              int layer = ptr->get()->get_cluster()->get_layer();

              double x = clus->get_xyz_global(0);
              double y = clus->get_xyz_global(1);
              double zmc = clus->get_xyz_global(2);
              double r = sqrt(x*x + y*y);
              double phimc = atan2(y,x);
              phimc = FVTXOO::angle_normalize(phimc);

              // fill mc hits
              _rmcv.push_back(r);
              _xmcv.push_back(x);
              _ymcv.push_back(y);
              _zmcv.push_back(zmc);
              _phimcv.push_back(phimc);
              _rmc[_size] = r;
              _xmc[_size] = x;
              _ymc[_size] = y;
              _zmc[_size] = zmc;
              _phimc[_size] = phimc;

              _rv.push_back(r);
              _xv.push_back(r*cos(phi_smear[layer]));
              //              _xwv.push_back( 1.0/SQ(phi_strip_resol) );
              _yv.push_back(r*sin(phi_smear[layer]));
              //              _ywv.push_back( 1.0/SQ(phi_strip_resol) );
              _zv.push_back(z_smear[layer]);
              _phiv.push_back(phi_smear[layer]);

              const double weight_x = (phi_strip_resol*std::abs(sin(phi_smear[layer])) + r_resol*std::abs(cos(phi_smear[layer])))/sqrt(12);
              const double weight_y = (phi_strip_resol*std::abs(cos(phi_smear[layer])) + r_resol*std::abs(sin(phi_smear[layer])))/sqrt(12);

              _rwv.push_back(1.0/SQ(0.0200/sqrt(12)));
              _xwv.push_back(1.0/SQ(weight_x));
              _ywv.push_back(1.0/SQ(weight_y));
              _inw.push_back(1.0/SQ(0.0200/sqrt(12)));
              _outw.push_back(1.0/SQ(0.0050/sqrt(12)));

              _foundVtxHit = true;
              _r[_size] = r;
              _x[_size] = r*cos(phi_smear[layer]);
              _y[_size] = r*sin(phi_smear[layer]);
              _z[_size] = z_smear[layer];
              _x_cluster_width[_size] = phi_strip_resol;
              _y_cluster_width[_size] = phi_strip_resol;

              _phi[_size] = phi_smear[layer];
              _mc_tracknum[_size] = -9999; //hit->GetMctrack();
              _pid[_size] = -9999; //hit->GetIdPart();
              _halfWedgeId[_size] = -1;
              _nstrips[_size] = int(n_strips);
              //                      _parent_pid[_size] = mc_trk_iter.current()->get()->get_parent_id();
              PHKey::associate( trk_ptr, ptr);

              ++_size;
            }
          }
	  get_rotated_fits();

 	  // re-fitting measured hits
	  boost::tie(_r_slopeFit,_r_offsetFit) = weighted_linearFit<double>(_zv,_rv,_rwv);
// 	  boost::tie(_x_slope, _x_int) = weighted_linearFit<double>(_zv,_xv,_xwv);
// 	  boost::tie(_y_slope, _y_int) = weighted_linearFit<double>(_zv,_yv,_ywv);
	  PHGslMatrix paramVec = minimize_chiSquare<double>(_xv,_yv,_zv,_phiv,_inw,_outw);
	  _x_int = paramVec.get(0,0);
	  _x_slope = paramVec.get(1,0);
	  _y_int = paramVec.get(2,0);
	  _y_slope = paramVec.get(3,0);
 	  _phi_offsetFit = atan2(_y_int,_x_int);

	  // fitting mc hits
	  boost::tie(_rmc_slopeFit,_rmc_offsetFit) = linearFit<double>(_zmcv,_rmcv);
	  boost::tie(_xmc_slope, _xmc_int) = linearFit<double>(_zmcv,_xmcv);
	  boost::tie(_ymc_slope, _ymc_int) = linearFit<double>(_zmcv,_ymcv);
	  _phimc_offsetFit = atan2(_ymc_int,_xmc_int);

	  //	  _phi_offsetFit = FVTXOO::angle_normalize(_phi_offsetFit);
	  _rchi2 = getResiduals();
	  if ( _do_evaluation ) _trk_ab_tree->Fill();

	}
    }
}

void
mFvtxAddVtxHits::get_rotated_fits()
{
  //rotate coordinates to be in the natural coordinate of the FVTX.
  boost::tie(_in,_out) = rotate_into_wedge<double>(_xv,_yv,_zv,_phi_rot);

  boost::tie(_inPlane_slope, _inPlane_int) = weighted_linearFit<double>(_zv,_in,_inw);
  boost::tie(_outPlane_slope, _outPlane_int) = weighted_linearFit<double>(_zv,_out,_outw);
  
  for (unsigned int i=0; i<_in.size(); i++)
    {
      _inPlane[i] = _in[i];
      _outPlane[i] = _out[i];
    }

  return;
}

void
mFvtxAddVtxHits::clear_track()
{

  _rmcv.clear();
  _xmcv.clear();
  _ymcv.clear();
  _zmcv.clear();
  _phimcv.clear();
  _rv.clear();
  _xv.clear();
  _yv.clear();
  _zv.clear();
  _phiv.clear();

  _in.clear();
  _out.clear();
  _x_begin.clear();
  _y_begin.clear();
  _x_end.clear();
  _y_end.clear();
  _inw.clear();
  _outw.clear();


  _rwv.clear();
  _xwv.clear();
  _ywv.clear();

  _rchi2 = -9999.0; 
  _rmc_slopeFit = -9999.0;
  _rmc_offsetFit = -9999.0;
  _phimc_slopeFit = -9999.0;
  _phimc_offsetFit = -9999.0;
  _xmc_int = -9999.0;
  _ymc_int = -9999.0;
  _xmc_slope = -9999.0;
  _ymc_slope = -9999.0;
  _r_slopeFit = -9999.0;
  _r_offsetFit = -9999.0;
  _phi_slopeFit = -9999.0;
  _phi_offsetFit = -9999.0;
  _x_int = -9999.0;
  _y_int = -9999.0;
  _x_slope = -9999.0;
  _y_slope = -9999.0;
  _phi_rot = -9999.0;
  _inPlane_int = -9999.0;
  _outPlane_int = -9999.0;
  _inPlane_slope = -9999.0;
  _outPlane_slope = -9999.0;
  _foundVtxHit = false;

  for (int i = 0; i < _size; i++)
    {
      _hasCharmParent[i] = false;
      _hasBottomParent[i] = false;
      _mc_tracknum[i] = -9999;
      _mc_hitnum[i] = -9999;
      _coordnum[i] = -9999;
      _pid[i] = -9999;
      _parent_pid[i] = -9999;
      _halfWedgeId[i] = -9999;
      _nstrips[i] = -9999;
      _xmc[i] = -9999.0;
      _ymc[i] = -9999.0;
      _zmc[i] = -9999.0;
      _rmc[i] = -9999.0;
      _phimc[i] = -9999.0;
      _x[i] = -9999.0;
      _y[i] = -9999.0;
      _z[i] = -9999.0;
      _r[i] = -9999.0;
      _phi[i] = -9999.0;
      _inPlane[i] = -9999.0;
      _outPlane[i] = -9999.0;
      _x_cluster_width[i] = -9999.0;
      _y_cluster_width[i] = -9999.0;
      _x_cluster_begin[i] = -9999.0;
      _y_cluster_begin[i] = -9999.0;
      _x_cluster_end[i] = -9999.0;
      _y_cluster_end[i] = -9999.0;
    }
  _size = 0;

  return;
}

int
mFvtxAddVtxHits::get_VtxLayerNumber(const float r)
{
  if ( r < 3.5 ) return 0;
  else return 1;

  return -9999;
}

double
mFvtxAddVtxHits::getResiduals()
{
  _r_resid.clear();
  
  for (int i = 0; i < _size; i++)
    {
      double r = _r[i];
      double z = _z[i];
      double dr = _r_slopeFit*z + _r_offsetFit - r;
      _r_resid.push_back(dr);
    }

  std::vector<double> resid2(_r_resid.size());
  std::transform(_r_resid.begin(),_r_resid.end(),
		 _r_resid.begin(),resid2.begin(),std::multiplies<double>());

  double rchi2 = std::accumulate(resid2.begin(),resid2.end(),0.0);

  return rchi2;
}






