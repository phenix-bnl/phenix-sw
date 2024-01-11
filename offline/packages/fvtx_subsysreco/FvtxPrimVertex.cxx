/*!
  \file		FvtxPrimVertex.cxx	
  \ingroup supermodules
  \brief	 Fvtx primary vertex module. 
  Reads TFvtxCompactTrk from DST, fill VtxOut with primary vertex from FVTX
  \author	Cesar Luiz da Silva
  \version $Revision: 1.38 $
  \date		$Date: 2016/01/13 17:31:17 $
*/

#include <FvtxPrimVertex.h>

// standard
#include <math.h>
#include <iostream>
#include <iomanip>

// root
#include <Minuit2/Minuit2Minimizer.h>
#include <TError.h>
#include <Math/Functor.h>

// phenix
#include <Fun4AllReturnCodes.h>
#include <PHIODataNode.h>
#include <getClass.h>
#include <PHTimer.h>
#include <VtxOut.h>
#include <BbcMultipleVtx.hh>
#include <BbcMultipleVtxList.hh>
#include <SvxSegmentList.h>
#include <SvxSegment.h>
#include <TMutExtVtx.h>
#include <TFvtxGlobalParCntrl.h>

// fvtx
#include <TFvtxCompactTrkMap.h> 
#include <FVTXOO.h>

using namespace std;

// global parameters required for ROOT vertex fitting algorithm
Vertex::TrackletIter fvtx_prim_vertex_fitter_data_begin;
Vertex::TrackletIter fvtx_prim_vertex_fitter_data_end;
float fvtx_prim_vertex_fitter_sigma_para;
float fvtx_prim_vertex_fitter_sigma_perp;
float fvtx_prim_vertex_fitter_sigma_xy;
float fvtx_prim_vertex_fitter_sigma_z;
float fvtx_prim_vertex_fitter_fvtx2vtx_weight;

// quick fit function for producing
// a robust vertex guess
// uncertainty scales should not be taken seriously
double square_dca_sum(const double *xvec) {
  
  PHPoint vertex(xvec[0],xvec[1],xvec[2]);
  double sum = 0.0;
  for (Vertex::TrackletIter iter = fvtx_prim_vertex_fitter_data_begin;
       iter != fvtx_prim_vertex_fitter_data_end;
       ++iter) {
    Tracklet* tracklet = (Tracklet*)(*iter);  
    float tw = tracklet->get_fit_weight();  
    double dca_3d = tracklet->get_dca(vertex);
    sum += tw*dca_3d*dca_3d;
  }

  return sum;
}

// fit function for vertex minimization
double gaussian_dca_sum(const double *xvec) {

  PHPoint vertex(xvec[0],xvec[1],xvec[2]);
  PHVector vertex_normal(0,0,1);
  PHPlane vertex_z_plane(vertex,vertex_normal);
  PHLine beam(vertex,vertex_normal);

  // scale for RMS ~ 1.0
  double s = 50.0; 
  // relative weight FVTX to VTX (slightly improves things)
  double w = fvtx_prim_vertex_fitter_fvtx2vtx_weight;

  double sum = 0.0;
  for (Vertex::TrackletIter iter = fvtx_prim_vertex_fitter_data_begin;
       iter != fvtx_prim_vertex_fitter_data_end;
       ++iter) {
    Tracklet* tracklet = (Tracklet*)(*iter);
    float tw = tracklet->get_fit_weight();

    if (tracklet->get_kind() == Tracklet::FVTX_TRACK) {
      PHPoint trkpnt = tracklet->get_intersection(vertex_z_plane);
      PHVector dca_r(trkpnt.getX()-vertex.getX(),
    		     trkpnt.getY()-vertex.getY(),
    		     0.0);
      PHVector mom = tracklet->get_momentum(trkpnt);	
      PHVector pt(mom.getX(),mom.getY(),0.0);
      double dot = PHGeometry::dot(dca_r,pt);
      double dca_para = dot/pt.length();
      double dca_perp = sqrt(dca_r.length()*dca_r.length()-dca_para*dca_para);
      if (dot < 0.0) dca_perp *= -1.0;
      
      sum += 
    	-0.5*s*w*tw*TMath::Gaus(dca_para,0.0,fvtx_prim_vertex_fitter_sigma_para) +
    	-0.5*s*w*tw*TMath::Gaus(dca_perp,0.0,fvtx_prim_vertex_fitter_sigma_perp);
      
    } else if ((tracklet->get_kind() == Tracklet::VTX_TRACK) || 
	       (tracklet->get_kind() == Tracklet::VTXP_STUB)) {
      // minimize the sum of ~dca_3d projections in xy and z separately
      // better than minimizing dca_3d alone
      PHPoint pca_3d = tracklet->get_pca(vertex);
      double dca_xy = sqrt(((pca_3d.getX() - vertex.getX()) * 
			    (pca_3d.getX() - vertex.getX())) +
			   ((pca_3d.getY() - vertex.getY()) * 
			    (pca_3d.getY() - vertex.getY())));
      double dca_z  = pca_3d.getZ() - vertex.getZ();
      
      sum += 
	-0.5*s*(1.0-w)*tw*TMath::Gaus(dca_xy,0.0,fvtx_prim_vertex_fitter_sigma_xy) +
	-0.5*s*(1.0-w)*tw*TMath::Gaus(dca_z ,0.0,fvtx_prim_vertex_fitter_sigma_z);
    }
  }
  
  return sum;
}


double ave(const std::vector<double> &x) {
  if (x.size() == 0) return NAN;

  double average = 0.0;
  for (unsigned int i=0; i<x.size(); ++i) {
    average += x[i];
  }
  average /= x.size();

  return average;
}

double rms(const std::vector<double> &x, double x0) {
  if (x.size() == 0) return NAN;

  double rms = 0.0;
  for (unsigned int i=0; i<x.size(); ++i) {
    rms += (x[i]-x0)*(x[i]-x0);
  }
  rms /= x.size();
  rms = sqrt(rms);

  return rms;
}

FvtxPrimVertex::FvtxPrimVertex() : 
  SubsysReco("FvtxPrimVertex"),
  _event_count(0),
  _vertex_counts(),			
  _fit_exits(),			
  _timer( boost::make_shared<PHTimer>("FvtxPrimVertex") ),
  _sources(),
  _clustering(Graph),
  _fix_xy(false),
  _make_fit(true),
  _make_recentering_fit(true),
  _mutr_use_fvtx(true),
  _use_north_arm(true),
  _use_south_arm(true),
  _use_east_arm(true),
  _use_west_arm(true),
  _max_vertexes(5),
  _max_intersections(50000),
  _vtx_x(NAN),
  _vtx_y(NAN),
  _vtx_ex(NAN),
  _vtx_ey(NAN),
  _beam_sum_counts(0),
  _beam_sum_x(0.0),
  _beam_sum_y(0.0),
  _fvtx_Rres(1.0),
  _chi2_cut(4.0),
  _max_Z(100),
  _max_R(1.0),
  _max_R2(0.75),
  _bbcz_window(1000),
  _fit_sigma_para(0.15),
  _fit_sigma_perp(0.65),
  _fit_sigma_xy(0.024),
  _fit_sigma_z(0.05625),
  _fit_fvtx2vtx_weight(0.40),
  _fit_fvtx2stub_weight(0.60),
  _vertex_names(),
  _vertex_priorities(),
  _correction_vector(),
  _bbc(),
  _min(),
  _exclude_fvtx(),
  _exclude_vtx(),
  _tracklets(),
  _intersections(),
  _vertexes() {

  reset_vertex_defaults();

  //! this need to be updated again in FvtxPrimVertex::InitRun() as TFvtxGlobalParCntrl is refined for a run from database.
  set_vtx_usage (TFvtxGlobalParCntrl::get_bool_par("use_svx"));
  set_pp_flag (TFvtxGlobalParCntrl::get_bool_par("is_pp"));

  _fit_exits.resize(4);
  _fit_exits.clear();

  _sources.clear();
  _sources.insert(Tracks);

  // mark all parameters as not user input
  _sources_user_set = false;
  _clustering_user_set = false;
  _max_vertexes_user_set = false;

  // correction vector
  _correction_vector[EAST][0] = 0.0;
  _correction_vector[EAST][1] = 0.0;
  _correction_vector[EAST][2] = 0.0;

  _correction_vector[WEST][0] = 0.0;
  _correction_vector[WEST][1] = 0.0;
  _correction_vector[WEST][2] = 0.0;

  _correction_vector[NORTH][0] = 0.0;
  _correction_vector[NORTH][1] = 0.0;
  _correction_vector[NORTH][2] = 0.0;

  _correction_vector[SOUTH][0] = 0.0;
  _correction_vector[SOUTH][1] = 0.0;
  _correction_vector[SOUTH][2] = 0.0;
}

FvtxPrimVertex::~FvtxPrimVertex() {
  if (_make_fit) delete _min;
}

int FvtxPrimVertex::Init(PHCompositeNode* top_node) {

  // move this to the init_run part. Since after init_run,
  // the run number is known and we will further configure
  // the code for e.g. HeavyIon or pp settings.

//  if (_make_fit) {
//    _min = new ROOT::Minuit2::Minuit2Minimizer(ROOT::Minuit2::kMigrad);
//    _min->SetPrintLevel(0);
//    _min->SetMaxFunctionCalls(1e5);
//    _min->SetMaxIterations(1e5);
//  }
//
//  FVTXOO::PRINT(cout, "FvtxPrimVertex::Init");
//  cout << "Fvtx Vertex Reconstruction Settings: " << endl;
//  cout << "   Max Vertexes Per Event = " << _max_vertexes << endl;
//  cout << "   Vertex Names in VtxOut: " << endl;
//  for (unsigned int i = 0; i < _vertex_names.size(); ++i) {
//    cout << "      vertex #" << i+1 << " = \"" << _vertex_names[i] << "\""<< endl;
//  }
//  cout << "   Search Limits (r,z) = (" << _max_R << "," << _max_Z << ") cm" << endl;
//  cout << "   Narrow Search Limits (r2,z) = (" << _max_R2 << "," << _max_Z << ") cm" << endl;
//  cout << "   Tracklet Sources = ";;
//  if (_sources.find(Tracks) != _sources.end()) cout << "FVTX Compact Tracks";
//  if (_sources.size() > 1) cout << " & ";
//  if (_sources.find(Segments) != _sources.end()) cout << "VTX Segments ";
//  cout << endl;
//  cout << "   Clustering Algorithm = ";
//  if (_clustering == AllInOne) cout << "AllInOne";
//  else if (_clustering == Cesars) cout << "Cesars";
//  else if (_clustering == Graph) cout << "Graph";
//  else if (_clustering == AntiKt) cout << "AntiKt";
//  cout << endl;
//  cout << "   Fixed Beam Center = " << boolalpha << _fix_xy << noboolalpha << endl;
//  if (_fix_xy) cout << "      beam center (x,y) = (" << _vtx_x << "," << _vtx_y << ")" << endl;
//  cout << "   Fitter Active = " << boolalpha << _make_fit << noboolalpha << endl;
//  if (_make_fit) {
//    cout << "   Fitter Weights:" << endl;
//    cout << "      sigma_para = " << _fit_sigma_para << endl;
//    cout << "      sigma_perp = " << _fit_sigma_perp << endl;
//    cout << "      sigma_xy   = " << _fit_sigma_xy << endl;
//    cout << "      sigma_z    = " << _fit_sigma_z << endl;
//    cout << "      fvtx2vtx   = " << _fit_fvtx2vtx_weight << endl;
//    cout << "      fvtx2stub   = " << _fit_fvtx2stub_weight << endl;
//  }
//  FVTXOO::PRINT(cout, "**");

  return EVENT_OK;
}

int FvtxPrimVertex::InitRun(PHCompositeNode* top_node) {

  TFvtxGlobalParCntrl::init_run();
  if (TFvtxGlobalParCntrl::get_bool_par("use_svx") and (not _sources_user_set))
    {
      set_source(FvtxPrimVertex::Segments, FvtxPrimVertex::Tracks);
    }

  if (not TFvtxGlobalParCntrl::get_bool_par("is_pp"))
    {
      if (not _max_vertexes_user_set)
        set_max_vertexes(1); // write out only one vertex
      if (not _clustering_user_set)
        set_clustering(FvtxPrimVertex::AllInOne); // single vertexing
    }

  _event_count = 0;
  _beam_sum_counts = 0;
  _beam_sum_x = 0.0;
  _beam_sum_y = 0.0;
  _vertex_counts.clear();
  _fit_exits.clear();

  if (_make_fit) {
    _min = new ROOT::Minuit2::Minuit2Minimizer(ROOT::Minuit2::kMigrad);
    _min->SetPrintLevel(0);
    _min->SetMaxFunctionCalls(1e5);
    _min->SetMaxIterations(1e5);
  }

  FVTXOO::PRINT(cout, "FvtxPrimVertex::InitRun");
  cout << "Fvtx Vertex Reconstruction Settings: " << endl;
  cout << "   Max Vertexes Per Event = " << _max_vertexes << endl;
  cout << "   Vertex Names in VtxOut: " << endl;
  for (unsigned int i = 0; i < _vertex_names.size(); ++i) {
    cout << "      vertex #" << i+1 << " = \"" << _vertex_names[i] << "\""<< endl;
  }
  cout << "   Search Limits (r,z) = (" << _max_R << "," << _max_Z << ") cm" << endl;
  cout << "   Narrow Search Limits (r2,z) = (" << _max_R2 << "," << _max_Z << ") cm" << endl;
  cout << "   Tracklet Sources = ";;
  if (_sources.find(Tracks) != _sources.end()) cout << "FVTX Compact Tracks";
  if (_sources.size() > 1) cout << " & ";
  if (_sources.find(Segments) != _sources.end()) cout << "VTX Segments ";
  cout << endl;
  cout << "   Clustering Algorithm = ";
  if (_clustering == AllInOne) cout << "AllInOne";
  else if (_clustering == Cesars) cout << "Cesars";
  else if (_clustering == Graph) cout << "Graph";
  else if (_clustering == AntiKt) cout << "AntiKt";
  cout << endl;
  cout << "   Fixed Beam Center = " << boolalpha << _fix_xy << noboolalpha << endl;
  if (_fix_xy) cout << "      beam center (x,y) = (" << _vtx_x << "," << _vtx_y << ")" << endl;
  cout << "   Fitter Active = " << boolalpha << _make_fit << noboolalpha << endl;
  if (_make_fit) {
    cout << "   Fitter Weights:" << endl;
    cout << "      sigma_para = " << _fit_sigma_para << endl;
    cout << "      sigma_perp = " << _fit_sigma_perp << endl;
    cout << "      sigma_xy   = " << _fit_sigma_xy << endl;
    cout << "      sigma_z    = " << _fit_sigma_z << endl;
    cout << "      fvtx2vtx   = " << _fit_fvtx2vtx_weight << endl;
    cout << "      fvtx2stub   = " << _fit_fvtx2stub_weight << endl;
  }
  cout << "   Correction Vectors:" << endl;
  cout << "      EAST = ("
       << _correction_vector[EAST][0] << ","
       << _correction_vector[EAST][1] << ","
       << _correction_vector[EAST][2] << ") cm" << endl;
  cout << "      WEST = ("
       << _correction_vector[WEST][0] << ","
       << _correction_vector[WEST][1] << ","
       << _correction_vector[WEST][2] << ") cm" << endl;
  cout << "      NORTH = ("
       << _correction_vector[NORTH][0] << ","
       << _correction_vector[NORTH][1] << ","
       << _correction_vector[NORTH][2] << ") cm" << endl;
  cout << "      SOUTH = ("
       << _correction_vector[SOUTH][0] << ","
       << _correction_vector[SOUTH][1] << ","
       << _correction_vector[SOUTH][2] << ") cm" << endl;
  
  FVTXOO::PRINT(cout, "**");
  return 0;
}

int FvtxPrimVertex::process_event(PHCompositeNode* top_node) {

  _timer->restart();
  ++_event_count;

  int return_code;
  return_code = make_tracklets(top_node);
  if (return_code != 0) {
    cout << PHWHERE << ":: make_tracklets reported error condition." << endl;
    _timer->stop();
    return return_code;
  }

  // create intersections (tracklet crossings)
  return_code = make_intersections();
  if (return_code != 0) {
    cout << PHWHERE << ":: make_intersections reported error condition." << endl;
    _timer->stop();
    return return_code;
  }

  // create vertexes (intersection clusters)
  return_code = make_vertexes();
  if (return_code != 0) {
    cout << PHWHERE << ":: make_vertexes reported error condition." << endl;
    _timer->stop();
    return return_code;
  }

  // fit vertexes (improves cluster positions)
  if (_make_fit) {
    return_code = fit_vertexes();
    if (return_code != 0) {
      cout << PHWHERE << ":: fit_vertexes reported error condition." << endl;
      _timer->stop();
      return return_code;
    }
  }

  // copy out vertexes
  return_code = export_vertexes(top_node);
  if (return_code != 0) {
    cout << PHWHERE << ":: export_vertexes reported error condition." << endl;
    _timer->stop();
    return return_code;
  }

  // beam averaging as the code runs (improves windowing)
  return_code = beam_averaging();
  if (return_code != 0) {
    cout << PHWHERE << ":: beam_averaging reported error condition." << endl;
    _timer->stop();
    return return_code;
  }

  // DONE!
  _timer->stop();
  return EVENT_OK;
}

std::vector<const Vertex*> FvtxPrimVertex::generate_vertexes(PHCompositeNode* top_node,
							     std::vector<const TFvtxCompactTrk*> exclude_fvtx,
							     std::vector<const SvxSegment*> exclude_vtx) {

  ++_event_count;
  std::vector<const Vertex*> vertexes;
  
  // set excluded tracks (to be provided by user of function)
  _exclude_fvtx = exclude_fvtx;
  _exclude_vtx = exclude_vtx;

  // construct tracklet interface
  int return_code = make_tracklets(top_node);
  if (return_code != 0) {
    cout << PHWHERE << ":: make_tracklets reported error condition." << endl;
    _timer->stop();
    return vertexes;
  }

  // create intersections (tracklet crossings)
  return_code = make_intersections();
  if (return_code != 0) {
    cout << PHWHERE << ":: make_intersections reported error condition." << endl;
    _timer->stop();
    return vertexes;
  }

  // create vertexes (intersection clusters)
  return_code = make_vertexes();
  if (return_code != 0) {
    cout << PHWHERE << ":: make_vertexes reported error condition." << endl;
    _timer->stop();
    return vertexes;
  }

  // fit vertexes (improves cluster positions)
  if (_make_fit) {
    return_code = fit_vertexes();
    if (return_code != 0) {
      cout << PHWHERE << ":: fit_vertexes reported error condition." << endl;
      _timer->stop();
      return vertexes;
    }
  }

  // return list of vertexes to user and exit
  for (unsigned int i = 0; i < _vertexes.size(); ++i) {
    vertexes.push_back(&_vertexes[i]);
  }

  // beam averaging as the code runs (improves windowing)
  return_code = beam_averaging();
  if (return_code != 0) {
    cout << PHWHERE << ":: beam_averaging reported error condition." << endl;
    _timer->stop();
    return vertexes;
  }

  return vertexes;
}

int FvtxPrimVertex::ResetEvent(PHCompositeNode* top_node) {
  _tracklets.clear();
  _intersections.clear();
  _vertexes.clear();
  return 0;
}

int FvtxPrimVertex::End(PHCompositeNode* top_node) {

  FVTXOO::PRINT( cout, "FvtxPrimVertex::End");

  if (!isnan(get_beam_x()) && !isnan(get_beam_y())) {
    cout << "Beam spot used in vertex finding (x,y) = (" << get_beam_x();
    cout << "," << get_beam_y() << ") cm" << endl;
  }

  unsigned int sum = 0;
  for (unsigned int i = 0; i < _max_vertexes; ++i) {
    if (_event_count > 0) {
      cout << _vertex_names[i] << " vertex fill rate = " 
	   << _vertex_counts[i]*100.0/_event_count << "%" << endl;
    } else {
      cout << _vertex_names[i] << " vertex fill rate = " 
	   << 0 << "%" << endl;
    }
    sum += _vertex_counts[i];
  }
  
  if (_make_fit) {
    cout << "Fitter exit points: " << endl;
    cout << "   Average     = " << _fit_exits[0] << endl;
    cout << "   DCA Squared = " << _fit_exits[1] << endl;
    cout << "   Rough Gauss = " << _fit_exits[2] << endl;
    cout << "   Fine Gauss  = " << _fit_exits[3] << endl;
  }

  FVTXOO::PRINT( cout, "**");

  _timer->print_stat();

  return 0;
}

int FvtxPrimVertex::make_tracklets(PHCompositeNode* top_node) {

  _tracklets.clear();

  // BbcMultipleVtx
  _bbc = NULL;
  if ( _bbcz_window < 100 ) {
    _bbc = findNode::getClass<BbcMultipleVtx>(top_node, "BbcMultipleVtx" );
    if (!_bbc) {
      cout << PHWHERE << ":: BbcMultipleVtx not in Node Tree. "
  	   << "\n Add the module BbcMultipleVtxReco before this." << endl;
      _timer->stop();
      return ABORTRUN;
    }
  }

  if ( (_sources.find(Segments) != _sources.end()) && 
       (_sources.find(PixelStubs) != _sources.end()) ) {
    cout << PHWHERE << ":: Do not use pixel stubs when VTX segments are also available." << endl;
    _timer->stop();
    return ABORTRUN;
  }

  if (_sources.find(Tracks) != _sources.end()) {

    TFvtxCompactTrkMap* fvtx_trk_map = 
      findNode::getClass<TFvtxCompactTrkMap>(top_node,"TFvtxCompactTrkMap");
    if (!fvtx_trk_map) {
      cout << PHWHERE << ":: TFvtxCompactTrkMap not available. "
	   << "\n Add the module FvtxReadbackDST before this." << endl;
      return ABORTRUN;
    }

    TFvtxCompactTrkMap::iterator iter( fvtx_trk_map->range() );
    while( TFvtxCompactTrkMap::const_pointer fvtx_ptr = iter.next() ) {
      if (!is_good(fvtx_ptr->get())) continue;
      if (fvtx_ptr->get()->get_chi2_ndf() > _chi2_cut) continue;
      Tracklet tracklet(fvtx_ptr->get(),_max_R,_max_Z,_fvtx_Rres,false);
      _tracklets.push_back(tracklet);
    }
  }

  if (_sources.find(Segments) != _sources.end()) {

    SvxSegmentList* segmentList =
      findNode::getClass<SvxSegmentList>(top_node,"SvxSegmentList");
    if(!segmentList) {
      cout << PHWHERE << ":: SvxSegmentList not available." << endl;
      return ABORTRUN;
    }

    for(int i = 0; i < segmentList->get_nSegments(); ++i) {
      SvxSegment *segment = segmentList->get_segment(i);

      if (!is_good(segment)) continue;

      Tracklet tracklet(segment,_max_R,_max_Z,_fvtx_Rres);
      _tracklets.push_back(tracklet);
    }
  }

  if (_sources.find(PixelStubs) != _sources.end()) {

    TFvtxCompactTrkMap* fvtx_trk_map = 
      findNode::getClass<TFvtxCompactTrkMap>(top_node,"TFvtxCompactTrkMap");
    if (!fvtx_trk_map) {
      cout << PHWHERE << ":: TFvtxCompactTrkMap not available. "
	   << "\n Add the module FvtxReadbackDST before this." << endl;
      return ABORTRUN;
    }

    TFvtxCompactTrkMap::iterator iter( fvtx_trk_map->range() );
    while( TFvtxCompactTrkMap::const_pointer fvtx_ptr = iter.next() ) {
      if (!is_good_stub(fvtx_ptr->get())) continue;
      if (fvtx_ptr->get()->get_chi2_ndf() > _chi2_cut) continue;
      Tracklet tracklet(fvtx_ptr->get(),_max_R,_max_Z,_fvtx_Rres,true);
      _tracklets.push_back(tracklet);
    }
  }

  // set correction vector to Tracklets
  if (!_tracklets.empty()) {
    _tracklets[0].set_correction_vector(Tracklet::EAST,
					_correction_vector[EAST][0],
					_correction_vector[EAST][1],
					_correction_vector[EAST][2]);
    _tracklets[0].set_correction_vector(Tracklet::WEST,
					_correction_vector[WEST][0],
					_correction_vector[WEST][1],
					_correction_vector[WEST][2]);
    _tracklets[0].set_correction_vector(Tracklet::NORTH,
					_correction_vector[NORTH][0],
					_correction_vector[NORTH][1],
					_correction_vector[NORTH][2]);
    _tracklets[0].set_correction_vector(Tracklet::SOUTH,
					_correction_vector[SOUTH][0],
					_correction_vector[SOUTH][1],
					_correction_vector[SOUTH][2]);
  }

  if (verbosity > 4) {
    cout << "---List of Tracklets----------------------" << endl;
    for (unsigned int i=0; i<_tracklets.size(); ++i) {
      _tracklets[i].print();
    }
    cout << "------------------------------------------" << endl;
  }

  return 0;
}

int FvtxPrimVertex::make_intersections() {
  
  unsigned int index = 0;  
  _intersections.clear();

  // loop over all tracklets and examine close approaches
  for (unsigned int i = 0; i < _tracklets.size(); ++i) {
    for (unsigned int j = i + 1; j < _tracklets.size(); ++j) {
      
      if (_tracklets[i].intersects(_tracklets[j])) {
	
	Intersection intersection(index, &_tracklets[i], &_tracklets[j],
				  _fvtx_Rres);
	
	if (isnan(intersection.get_x())) continue;
	if (isnan(intersection.get_y())) continue;
	if (isnan(intersection.get_z())) continue;

	// if a beam spot is defined tighten the search around it
	if ( !isnan(get_beam_x()) && !isnan(get_beam_y()) ) {
	  if (sqrt( MUTOO::SQUARE(intersection.get_x()-get_beam_x()) +
		    MUTOO::SQUARE(intersection.get_y()-get_beam_y())) > _max_R2) {
	    continue;
	  }
	}

	_intersections.push_back(intersection);
	++index;
      }
    }
  }

  if (verbosity > 3) {
    cout << "---List of Intersections------------------" << endl;
    for (unsigned int i=0; i<_intersections.size(); ++i) {
      _intersections[i].print();
    }
    cout << "------------------------------------------" << endl;
  }

  return 0;
}

int FvtxPrimVertex::make_vertexes() {

  _vertexes.clear();

  if (_intersections.size() < 1) return 0;

  if ((_clustering == AllInOne) ||
      (_intersections.size() > _max_intersections)) { // added to control beam-splash events with multi-vertexing on

    //----------------------------------------------
    // ALL IN ONE VERTEX
    // single vertex absorbs full list of intersections
    Vertex vertex;
    for (unsigned int i=0; i<_intersections.size(); ++i) {
      vertex.add_intersection(&_intersections[i]);
    }
    PHPoint ave = vertex.get_ave_point();
    PHPoint err = vertex.get_ave_error();
    vertex.set_point(ave);
    vertex.set_error(err);
    _vertexes.push_back(vertex);

  } else if (_clustering == Cesars) {

    std::set<Tracklet*> used_tracklets;
    cesars_vertex_selection(used_tracklets);

  } else if (_clustering == Graph) {

    //----------------------------------------------
    // GRAPH
    // all intersections that can reach each other through _fvtx_Res length
    // links are put into a single vertex
    // instersections can only participate in one vertex
    // but tracklets can appear in multiple vertexes

    // run clustering
    std::multimap<int,Intersection> clustered_intersections;
    make_graph_clusters(_intersections,clustered_intersections);
    
    // run selection
    std::set<Tracklet*> used_tracklets;
    vertex_selection(clustered_intersections,used_tracklets);
      
  } else if (_clustering == AntiKt) {

    //----------------------------------------------
    // ANTIKT
    // this algorithm isn't vetted yet (i think it has some logic bug)
    // but I want to keep it up to date since it should do a better job
    // of separating nearby collision vertexes
    
    // run clustering
    std::multimap<int,Intersection> clustered_intersections;
    make_antikt_clusters(_intersections,clustered_intersections);

    // run selection
    std::set<Tracklet*> used_tracklets;
    vertex_selection(clustered_intersections,used_tracklets);

  } else {
    cout << PHWHERE << " Must choose AllInOne, Cesars, or Graph" << endl;
    return ABORTRUN;
  }

  // compute average positions
  for (unsigned int i=0; i<_vertexes.size(); ++i) {
    PHPoint average = _vertexes[i].get_ave_point();
    PHPoint error = _vertexes[i].get_ave_error();

    if (_fix_xy) {
      average.setX(_vtx_x);
      average.setY(_vtx_y);
      error.setX(_vtx_ex);
      error.setY(_vtx_ey);
    }

    _vertexes[i].set_point(_vertexes[i].get_ave_point());
    _vertexes[i].set_error(_vertexes[i].get_ave_error());
  }

  // sort list by number of intersections (largest at entry 0)
  std::sort(_vertexes.begin(),_vertexes.end());
 
  if (verbosity > 2) {
    cout << "---List of Vertexes-----------------------" << endl;
    for (unsigned int i=0; i<_vertexes.size(); ++i) {
      _vertexes[i].print();
    }
    cout << "------------------------------------------" << endl;
  }

  return 0;
}

int FvtxPrimVertex::fit_vertexes() {

  for (unsigned int i=0; i<_vertexes.size(); ++i) {

    // fit only the vertexes to be written out if available
    if (i>=_max_vertexes) break;

    // load vertex fitting data
    fvtx_prim_vertex_fitter_data_begin = _vertexes[i].get_tracklets_begin();
    fvtx_prim_vertex_fitter_data_end   = _vertexes[i].get_tracklets_end();

    // run fitter
    fit_vertex(_vertexes[i]);
  }
  
  return 0;
}

int FvtxPrimVertex::beam_averaging() {

  // use only valid vertex reconstructions
  if (_vertexes.size() < 1) return EVENT_OK;
  if (isnan(_vertexes[0].get_x())) return EVENT_OK;
  if (isnan(_vertexes[0].get_y())) return EVENT_OK;

  ++_beam_sum_counts;
  _beam_sum_x += _vertexes[0].get_x();
  _beam_sum_y += _vertexes[0].get_y();

  return EVENT_OK;
}

int FvtxPrimVertex::export_vertexes(PHCompositeNode *top_node) {

  // save vertex in VtxOut
  VtxOut* vtxout = TMutNode<VtxOut>::find_io_node( top_node, "VtxOut");
  if (!vtxout) {
    cout << PHWHERE << " VtxOut not in Node Tree" << endl;
    return ABORTRUN;
  }

  if (verbosity > 0) {
    cout << "FvtxPrimVertex::--------------------------------------------" << endl;
    cout << "FvtxPrimVertex:: event " << _event_count << ":"
	 << " ntracklets = " << _tracklets.size()
	 << ", nintersections = " << _intersections.size() << endl;
  }

  // reset any existing vertexes that are being generated here
  for (unsigned int i = 0; i < _vertex_names.size(); ++i) {
    
    PHPoint oldvtx = vtxout->get_Vertex(_vertex_names[i].c_str());      
    if (!isnan(oldvtx.getX()) || !isnan(oldvtx.getY()) || !isnan(oldvtx.getZ())) {
      
      // null out existing vertex of this name
      float vertex[3] = {NAN,NAN,NAN};
      float error[3] = {NAN,NAN,NAN};
      vtxout->AddVtx(_vertex_names[i].c_str(), vertex, error, _vertex_priorities[i]);

      static set<unsigned int> warned;
      if (warned.find(i) == warned.end()) {
	// no warning has been issued yet
	cout << "FvtxPrimVertex:: WARNING, overwriting existing " << _vertex_names[i]
	     << " vertex: ( "<< oldvtx.getX() << ", " << oldvtx.getY() << ", " << oldvtx.getZ() << " )" << endl;
	warned.insert(i);
      }
    }    
  }
    
  unsigned int ivertex = 0;
  for (unsigned int i=0; i<_vertexes.size(); ++i) {

    // stop when the max number of vertexes have been recorded
    if (ivertex >= _max_vertexes) break;

    float vertex[3] = {NAN,NAN,NAN};
    float error[3] = {NAN,NAN,NAN};

    vertex[0] = _vertexes[i].get_x();
    vertex[1] = _vertexes[i].get_y();
    vertex[2] = _vertexes[i].get_z();

    error[0] = _vertexes[i].get_ex();
    error[1] = _vertexes[i].get_ey();
    error[2] = _vertexes[i].get_ez();
    
    // if using the bbc for matching,
    // skip this vertex if there is not a bbc match
    if ((_bbc)&&(find_bbcvtx(vertex[2], error[2]) < 0)) continue;
    
    // if this is a bad vertex don't write it out, unless there is a
    // stored vertex that should be overwritten
    if (isnan(vertex[0]) || isnan(vertex[1]) || isnan(vertex[2])) continue;
    
    // record the vertex
    vtxout->AddVtx(_vertex_names[ivertex].c_str(), vertex, error, _vertex_priorities[ivertex]);

    // report the vertex to a watching user
    if (verbosity > 0) {
      cout << "FvtxPrimVertex:: event " << _event_count << ":"
	   << " \"" << _vertex_names[ivertex] << "\" level " << _vertex_priorities[ivertex] << " = (" ;
      cout.setf(ios::fixed);
      size_t p = cout.precision();
      cout << setprecision(5) 
	   << vertex[0] << "," 
	   << vertex[1] << "," 
	   << vertex[2] << ") +/- ("
	   << error[0] << "," 
	   << error[1] << "," 
	   << error[2] << ") cm" << setprecision(p) << endl;
      cout.unsetf(ios::fixed);
    }      
      
    // set MuTr default: this needs to move to a new place outside of the vertexing
    if (ivertex == 0) {
      if ((_mutr_use_fvtx) && 
	  (!isnan(vertex[0]) && 
	   !isnan(vertex[1]) && 
	   !isnan(vertex[2]))) {
	TMutExtVtx::get().load_vtx( top_node );
      }
    }
    
    ++_vertex_counts[ivertex];
    ++ivertex;
  }

  if (verbosity > 0) {
    cout << "FvtxPrimVertex::--------------------------------------------" << endl;
  }

  return 0;
}

void FvtxPrimVertex::cesars_vertex_selection(std::set<Tracklet*>& used_tracklets) {

  //----------------------------------------------
  // CESAR's ALGORITHM
  // This algorithm is essentially a max-seed selection.
  // It will record the vertex created by an intersection
  // that collects the most intersections within a sphere of _fvtx_Rres
  // Some changes: I will take the center of the intersection
  // and do an unstable sort to find the highest multiplicity events
  // Additional changes: I've now done a full pass prior to 
  // vertex creation to limit the possible memory usage
  // More changes: find the two interesction seeds that 
  // produce the largest vertexes within are radius of _fvtx_Rres
  // but require the two vertexes not share intersections or tracklets -MPM
  // Changes: I've rewritten this to write out an arbitrary number of vertexes
  // via a recursive call. -MPM

  // stop the recursion when have gathered enough vertexes
  if (_vertexes.size() >= _max_vertexes) return;

  // look for another vertex
  int max_seed = -1;
  int max_seed_count = -1;

  // tag the seed intersections that will produce the two largest vertexes
  for (unsigned int i = 0; i < _intersections.size(); ++i) {

    // skip intersection if one of the tracklets in it has already been used
    if (used_tracklets.find(_intersections[i].get_first_tracklet()) !=
	used_tracklets.end()) continue;
    if (used_tracklets.find(_intersections[i].get_second_tracklet()) !=
	used_tracklets.end()) continue;
   
    int count = 1;
    vector<double> zpos;
    if (_bbc) zpos.push_back(_intersections[i].get_z());
    for (unsigned int j = i + 1; j < _intersections.size(); ++j) {

      // skip any other intersections already in use
      if (used_tracklets.find(_intersections[j].get_first_tracklet()) !=
	  used_tracklets.end()) continue;
      if (used_tracklets.find(_intersections[j].get_second_tracklet()) !=
	  used_tracklets.end()) continue;

      double xreso = 0.5*(_intersections[i].get_reso_x()+_intersections[j].get_reso_x());
      double yreso = 0.5*(_intersections[i].get_reso_y()+_intersections[j].get_reso_y());
      double zreso = 0.5*(_intersections[i].get_reso_z()+_intersections[j].get_reso_z());
      float rho = sqrt( MUTOO::SQUARE((_intersections[i].get_x()-_intersections[j].get_x())/xreso) +
			MUTOO::SQUARE((_intersections[i].get_y()-_intersections[j].get_y())/yreso) +
			MUTOO::SQUARE((_intersections[i].get_z()-_intersections[j].get_z())/zreso));
      if (rho < 1.0) {
	++count;
	if (_bbc) zpos.push_back(_intersections[j].get_z());
      }
    }
      
    // if using the bbc for matching,
    // skip this vertex if there is not a bbc match
    if(_bbc) {
      if (zpos.size() == 0) continue;
      double average = ave(zpos);
      double error = rms(zpos,average) / sqrt(zpos.size());
      if (find_bbcvtx(average, error) < 0) continue;
    }
      
    if (count > max_seed_count) {
      max_seed = i;
      max_seed_count = count;
    }
  }

  // recreate the best reconstructed vertex
  if ((max_seed > -1)&&(max_seed_count > 0)) {    

    Vertex vertex;
    vertex.add_intersection(&_intersections[max_seed]);
    vertex.set_point(_intersections[max_seed].get_point());

    for (unsigned int j = max_seed + 1; j < _intersections.size(); ++j) {
 
      // skip any other intersections already in use
      if (used_tracklets.find(_intersections[j].get_first_tracklet()) !=
	  used_tracklets.end()) continue;
      if (used_tracklets.find(_intersections[j].get_second_tracklet()) !=
	  used_tracklets.end()) continue;

      double xreso = 0.5*(_intersections[max_seed].get_reso_x() + _intersections[j].get_reso_x());
      double yreso = 0.5*(_intersections[max_seed].get_reso_y() + _intersections[j].get_reso_y());
      double zreso = 0.5*(_intersections[max_seed].get_reso_z() + _intersections[j].get_reso_z());
      float rho = sqrt( MUTOO::SQUARE((_intersections[max_seed].get_x()-_intersections[j].get_x())/xreso) +
			MUTOO::SQUARE((_intersections[max_seed].get_y()-_intersections[j].get_y())/yreso) +
			MUTOO::SQUARE((_intersections[max_seed].get_z()-_intersections[j].get_z())/zreso));
      if (rho < 1.0) {
	vertex.add_intersection(&_intersections[j]);
      }
    }

    // add vertex to the list
    _vertexes.push_back(vertex);

    // add this vertex's tracklets to the used list
    for (Vertex::TrackletIter iter = vertex.get_tracklets_begin();
	 iter != vertex.get_tracklets_end();
	 ++iter) {
      used_tracklets.insert(*iter);
    }      

    // have another go at it
    cesars_vertex_selection(used_tracklets);
  }

  // if no more vertexes are left to find
  return;
}


void FvtxPrimVertex::vertex_selection(std::multimap<int,Intersection>& clustered_intersections,
				      std::set<Tracklet*>& used_tracklets) {


  // stop the recursion when have gathered enough vertexes
  if (_vertexes.size() >= _max_vertexes) return;

  // look for the next leading vertex
  multimap<int,Intersection>::iterator max_cluster;
  int max_cluster_count = -1;

  // tag the seed intersections that will produce the largest vertexes
  // loop over all cluster ids in multimap of <clusterids,intersections>
  for (std::multimap<int,Intersection>::iterator outer 
	 = clustered_intersections.begin();
       outer != clustered_intersections.end();
       outer = clustered_intersections.upper_bound(outer->first)) {

    int count = 0;
    vector<double> zpos;

    // loop over all of this cluter's associated intersections
    for (std::multimap<int,Intersection>::iterator inner 
	   = clustered_intersections.lower_bound(outer->first);
	 inner != clustered_intersections.upper_bound(outer->first);
	 ++inner) {

      // skip intersections with already used tracklets
      if (used_tracklets.find(inner->second.get_first_tracklet()) !=
	  used_tracklets.end()) continue;
      if (used_tracklets.find(inner->second.get_second_tracklet()) !=
	  used_tracklets.end()) continue;

      ++count;
      if (_bbc) zpos.push_back(inner->second.get_z());
    }
      
    // if using the bbc for matching,
    // skip this vertex if there is not a bbc match
    if(_bbc) {
      if (zpos.size() == 0) continue;
      double average = ave(zpos);
      double error = rms(zpos,average) / sqrt(zpos.size());
      if (find_bbcvtx(average, error) < 0) continue;
    }
      
    // keep copies of max seed
    if (count > max_cluster_count) {
      max_cluster = outer;
      max_cluster_count = count;
    }
  } // end cluster loop

  // create the vertex 
  if (max_cluster_count > 0) {

    Vertex vertex;
    vertex.set_point(max_cluster->second.get_point());      
    // loop over all of this cluter's associated intersections
    for (std::multimap<int,Intersection>::iterator inner 
	   = clustered_intersections.lower_bound(max_cluster->first);
	 inner != clustered_intersections.upper_bound(max_cluster->first);
	 ++inner) {

      // skip the already used tracklets
      if (used_tracklets.find(inner->second.get_first_tracklet()) !=
	  used_tracklets.end()) continue;
      if (used_tracklets.find(inner->second.get_second_tracklet()) !=
	  used_tracklets.end()) continue;
	
      unsigned int index = inner->second.get_index();
      vertex.add_intersection(&_intersections[index]);
    }
      
    // add vertex to the list
    _vertexes.push_back(vertex);

    // add this vertex's tracklets to the used list
    for (Vertex::TrackletIter iter = vertex.get_tracklets_begin();
	 iter != vertex.get_tracklets_end();
	 ++iter) {
      used_tracklets.insert(*iter);
    }      

    // have another go at it
    vertex_selection(clustered_intersections,used_tracklets);
  }

  // if no more vertexes are left to find
  return;
}

int FvtxPrimVertex::find_bbcvtx(float z, float z_error) {

  if (!_bbc) return -999;

  BbcMultipleVtxList* bbclist = _bbc->get_vtxlist(0);
  int ibbcvtx = -999;
  float best_match = 999.9;

  for (int ibbc=0; ibbc<_bbc->get_size(); ibbc++) {
    float bbcz = bbclist->get_vertex_z(ibbc);
    float dz = fabs(z-bbcz);
    if (dz < best_match) {
      best_match = dz;
      ibbcvtx = ibbc;
    }
  }

  if ( best_match < sqrt(MUTOO::SQUARE(_bbcz_window) + MUTOO::SQUARE(2*z_error) )) {
    return ibbcvtx;
  }

  return -999;
}

bool FvtxPrimVertex::fit_vertex(Vertex &vertex) {

  if (vertex.get_num_tracklets() < 2) { 
    vertex.set_x(NAN);
    vertex.set_y(NAN);
    vertex.set_z(NAN);

    vertex.set_x(NAN); 
    vertex.set_y(NAN);
    vertex.set_z(NAN);
    
    return false;
  }

  // silent error infos prints from ROOT::MINUIT2
  const Int_t old_err_level = gErrorIgnoreLevel;
  gErrorIgnoreLevel = kWarning;

  float ave[3] = {vertex.get_x(),vertex.get_y(),vertex.get_z()};
  float ave_err[3] = {vertex.get_ex(),vertex.get_ey(),vertex.get_ez()};

  if (verbosity > 1) {
    cout << PHWHERE << ":: average vertex = ("
	 << ave[0] << ","
	 << ave[1] << ","
	 << ave[2] << ")" << endl;
  }

  //-----------------//
  // recentering fit //
  //-----------------//

  float guess[3] = {ave[0],ave[1],ave[2]}; 
  float step[3] = {0.001,0.001,0.001}; // 10 micron steps

  bool dcasq_fit_status = false;
  double dcasq[3]     = {NAN,NAN,NAN};
  double dcasq_err[3] = {NAN,NAN,NAN};

  _min->SetVariable(0,"x",guess[0],step[0]);
  _min->SetVariable(1,"y",guess[1],step[1]);
  _min->SetVariable(2,"z",guess[2],step[2]);

  if (_fix_xy) {
    _min->SetFixedVariable(0,"x",_vtx_x);
    _min->SetFixedVariable(1,"y",_vtx_y);
  }

  ROOT::Math::Functor fsquare(&square_dca_sum,3);
  _min->SetFunction(fsquare);
  
  if (!_min->Minimize()) {

    dcasq_fit_status = false;

    // revert to average and try next fit
    guess[0] = ave[0];
    guess[1] = ave[1];
    guess[2] = ave[2];

  } else {

    dcasq_fit_status = true;

    const double *fit     = _min->X();
    const double *fit_err = _min->Errors();

    // store dcasq vertex
    dcasq[0] = fit[0];
    dcasq[1] = fit[1];
    dcasq[2] = fit[2];

    dcasq_err[0] = fit_err[0];
    dcasq_err[1] = fit_err[1];
    dcasq_err[2] = fit_err[2];

    if (_fix_xy) {
      dcasq_err[0] = _vtx_ex;
      dcasq_err[1] = _vtx_ey;
    }

    // use the dcasq vertex for the next fit
    guess[0] = dcasq[0];
    guess[1] = dcasq[1];
    guess[2] = dcasq[2];

  }

  if (verbosity > 1) {
    cout << PHWHERE << ":: minimizer status = " << _min->Status() << endl;
    cout << PHWHERE << ":: dcasq vertex = ("
	 << dcasq[0] << ","
	 << dcasq[1] << ","
	 << dcasq[2] << ")" << endl;
  }

  //----------------------//
  // rough regulation fit //
  //----------------------//

  bool rough_fit_status = false;
  double rough[3]     = {NAN,NAN,NAN};
  double rough_err[3] = {NAN,NAN,NAN};

  _min->SetVariable(0,"x",guess[0],step[0]);
  _min->SetVariable(1,"y",guess[1],step[1]);
  _min->SetVariable(2,"z",guess[2],step[2]);

  if (_fix_xy) {
    _min->SetFixedVariable(0,"x",_vtx_x);
    _min->SetFixedVariable(1,"y",_vtx_y);
  }

  fvtx_prim_vertex_fitter_sigma_para      = _fit_sigma_para*2.0;
  fvtx_prim_vertex_fitter_sigma_perp      = _fit_sigma_perp*2.0;
  fvtx_prim_vertex_fitter_sigma_xy        = _fit_sigma_xy*2.0;
  fvtx_prim_vertex_fitter_sigma_z         = _fit_sigma_z*2.0;
  fvtx_prim_vertex_fitter_fvtx2vtx_weight = _fit_fvtx2vtx_weight;  
  if (_sources.find(PixelStubs) != _sources.end()) {
    fvtx_prim_vertex_fitter_fvtx2vtx_weight = _fit_fvtx2stub_weight;  
  }

  ROOT::Math::Functor fgaussian(&gaussian_dca_sum,3);
  _min->SetFunction(fgaussian);

  if (!_min->Minimize()) {
    
    rough_fit_status = false;
    
    // revert and try next fit
    if (dcasq_fit_status) {
      guess[0] = dcasq[0];
      guess[1] = dcasq[1];
      guess[2] = dcasq[2];
    } else {
      guess[0] = ave[0];
      guess[1] = ave[1];
      guess[2] = ave[2];
    }
    
  } else {

    rough_fit_status = true;

    const double *fit     = _min->X();
    const double *fit_err = _min->Errors();
    
    // store rough vertex
    rough[0] = fit[0];
    rough[1] = fit[1];
    rough[2] = fit[2];

    rough_err[0] = fit_err[0];
    rough_err[1] = fit_err[1];
    rough_err[2] = fit_err[2];

    if (_fix_xy) {
      rough_err[0] = _vtx_ex;
      rough_err[1] = _vtx_ey;
    }

    // use the dcasq vertex for the next fit
    guess[0] = rough[0];
    guess[1] = rough[1];
    guess[2] = rough[2];
  }

  if (verbosity > 1) {
    cout << PHWHERE << ":: minimizer status = " << _min->Status() << endl;
    cout << PHWHERE << ":: rough gauss vertex = ("
	 << rough[0] << ","
	 << rough[1] << ","
	 << rough[2] << ")" << endl;
  }

  //---------------------//
  // fine regulation fit //
  //---------------------//
  
  bool fine_fit_status = false;
  double fine[3]     = {NAN,NAN,NAN};
  double fine_err[3] = {NAN,NAN,NAN};

  _min->SetVariable(0,"x",guess[0],step[0]);
  _min->SetVariable(1,"y",guess[1],step[1]);
  _min->SetVariable(2,"z",guess[2],step[2]);

  if (_fix_xy) {
    _min->SetFixedVariable(0,"x",_vtx_x);
    _min->SetFixedVariable(1,"y",_vtx_y);
  }

  fvtx_prim_vertex_fitter_sigma_para = _fit_sigma_para;
  fvtx_prim_vertex_fitter_sigma_perp = _fit_sigma_perp;
  fvtx_prim_vertex_fitter_sigma_xy   = _fit_sigma_xy;
  fvtx_prim_vertex_fitter_sigma_z    = _fit_sigma_z;
  fvtx_prim_vertex_fitter_fvtx2vtx_weight = _fit_fvtx2vtx_weight;
  if (_sources.find(PixelStubs) != _sources.end()) {
    fvtx_prim_vertex_fitter_fvtx2vtx_weight = _fit_fvtx2stub_weight;  
  }

  //ROOT::Math::Functor fgaussian(&gaussian_dca_sum,3);
  //_min->SetFunction(fgaussian);

  if (!_min->Minimize()) {
    
    fine_fit_status = false;
 
  } else {

    fine_fit_status = true;

    const double *fit     = _min->X();
    const double *fit_err = _min->Errors();
    
    // store rough vertex
    fine[0] = fit[0];
    fine[1] = fit[1];
    fine[2] = fit[2];

    fine_err[0] = fit_err[0];
    fine_err[1] = fit_err[1];
    fine_err[2] = fit_err[2];

    if (_fix_xy) {
      fine_err[0] = _vtx_ex;
      fine_err[1] = _vtx_ey;
    }
  }

  if (verbosity > 1) {
    cout << PHWHERE << ":: minimizer status = " << _min->Status() << endl;
    cout << PHWHERE << ":: fine gauss vertex = ("
	 << fine[0] << ","
	 << fine[1] << ","
	 << fine[2] << ")" << endl;
  }

  //-----------------------------------//
  // send back the best successful fit //
  //-----------------------------------//

  if (fine_fit_status) {

    ++_fit_exits[3];

    vertex.set_x(fine[0]);
    vertex.set_y(fine[1]);
    vertex.set_z(fine[2]);

    vertex.set_ex(fine_err[0]);
    vertex.set_ey(fine_err[1]);
    vertex.set_ez(fine_err[2]);

  } else if (rough_fit_status) {

    ++_fit_exits[2];

    vertex.set_x(rough[0]);
    vertex.set_y(rough[1]);
    vertex.set_z(rough[2]);

    vertex.set_ex(rough_err[0]);
    vertex.set_ey(rough_err[1]);
    vertex.set_ez(rough_err[2]);

  } else if (dcasq_fit_status) {

    ++_fit_exits[1];

    vertex.set_x(dcasq[0]);
    vertex.set_y(dcasq[1]);
    vertex.set_z(dcasq[2]);

    vertex.set_ex(dcasq_err[0]);
    vertex.set_ey(dcasq_err[1]);
    vertex.set_ez(dcasq_err[2]);

  } else {

    ++_fit_exits[0];

    vertex.set_x(ave[0]);
    vertex.set_y(ave[1]);
    vertex.set_z(ave[2]);

    vertex.set_ex(ave_err[0]);
    vertex.set_ey(ave_err[1]);
    vertex.set_ez(ave_err[2]);
  }
  
  if (sqrt(pow(vertex.get_x(),2)+pow(vertex.get_y(),2)) > _max_R) {
    vertex.set_x(NAN);
    vertex.set_y(NAN);
    vertex.set_z(NAN);
    
    vertex.set_ex(NAN);
    vertex.set_ey(NAN);
    vertex.set_ez(NAN);
    
    return false;
  }

  if (!isnan(get_beam_x()) && !isnan(get_beam_y())) {
    if (sqrt(pow(vertex.get_x()-get_beam_x(),2)+pow(vertex.get_y()-get_beam_y(),2)) > _max_R2) {
      vertex.set_x(NAN);
      vertex.set_y(NAN);
      vertex.set_z(NAN);
      
      vertex.set_ex(NAN);
      vertex.set_ey(NAN);
      vertex.set_ez(NAN);

      return false;
    }
  }

  gErrorIgnoreLevel = old_err_level;
  return true;
}

bool FvtxPrimVertex::is_good(SvxSegment* segment) {

  if (isnan(segment->get3Momentum(0))) return false;
  if (isnan(segment->get3Momentum(1))) return false;
  if (isnan(segment->get3Momentum(2))) return false;
  if (isnan(segment->getMomentum())) return false;

  if (isnan(segment->getInnerMostProjectedPosition(0))) return false;
  if (isnan(segment->getInnerMostProjectedPosition(1))) return false;
  if (isnan(segment->getInnerMostProjectedPosition(2))) return false;

  // using px at first hit to determine arm
  if ((!_use_east_arm)&&(segment->get3Momentum(0) < 0.0)) return false;
  if ((!_use_west_arm)&&(segment->get3Momentum(0) > 0.0)) return false;

	//  chi2/pdf cut based on the svx primary vertex finding module
	if ((segment->getChiSq()/segment->getNDF()) <= 0.0) return false;
	if ((segment->getChiSq()/segment->getNDF()) > 6.0) return false;

  // look though the exclude list
  if (_exclude_vtx.size() != 0) {
    if (std::find(_exclude_vtx.begin(), 
		  _exclude_vtx.end(), 
		  segment)
	!= _exclude_vtx.end()) {
      return false;
    }
  }

  return true;
}

bool FvtxPrimVertex::is_good(TFvtxCompactTrk* track) {

  if (isnan(track->get_chi2_ndf())) return false;
  // 2 hit tracks are produced using the vertex
  // I exclude them here to ensure reproducibility if the 
  // vertex is reproduced after production
  if (track->get_nhits() == 2) return false;

  if ((!_use_north_arm)&&(track->get_arm() == 1)) return false;
  if ((!_use_south_arm)&&(track->get_arm() == 0)) return false;

  // look though the exclude list
  if (_exclude_fvtx.size() != 0) {
    if (std::find(_exclude_fvtx.begin(), 
		  _exclude_fvtx.end(), 
		  track)
	!= _exclude_fvtx.end()) {
      return false;
    }
  }

  return true;
}

bool FvtxPrimVertex::is_good_stub(TFvtxCompactTrk* track) {

  // use only 2hit tracks
  if (track->get_nhits() != 2) return false;

  // use only vtxp hits
  if ((track->get_svxhit_pattern() & 0x3) != 0x3) return false;

  // using px to determine arm
  if ((!_use_east_arm)&&(fabs(track->get_fvtx_phi()) > 1.57)) return false;
  if ((!_use_west_arm)&&(fabs(track->get_fvtx_phi()) < 1.57)) return false;

  // look though the exclude list
  if (_exclude_fvtx.size() != 0) {
    if (std::find(_exclude_fvtx.begin(), 
		  _exclude_fvtx.end(), 
		  track)
	!= _exclude_fvtx.end()) {
      return false;
    }
  }

  return true;
}

void FvtxPrimVertex::reset_vertex_defaults() {

  _vertex_counts.resize(_max_vertexes);

  _vertex_names.resize(_max_vertexes);
  for (unsigned int i = 0; i < _vertex_names.size(); ++i) {
    if (i == 0) {
      _vertex_names[i] = "FVTX";
    } else if (i == 1) {
      _vertex_names[i] = "FVTX_SECOND";
    } else {
      _vertex_names[i] = Form("FVTX_%i",i+1);
    }
  }

  _vertex_priorities.resize(_max_vertexes);
  for (unsigned int i = 0; i < _vertex_priorities.size(); ++i) {
    _vertex_priorities[i] = 8+i;
  }
}

//------------------------
// Tracklet implementation
//------------------------

float Tracklet::_max_R = 2.0;
float Tracklet::_max_Z = 12.0;
float Tracklet::_max_DCA = 1.0;

float Tracklet::_correction_vector[4][3];

Tracklet::Tracklet(TFvtxCompactTrk* fvtx_track,
		   float max_R = 2.0,
		   float max_Z = 12.0,
		   float max_DCA = 1.0,
		   bool stub = false) {
  if (!stub) {
    _kind = FVTX_TRACK;
    _fvtx_track = fvtx_track;
    _segment = NULL;
    _max_R = max_R;
    _max_Z = max_Z;
    _max_DCA = max_DCA;
  } else {
    _kind = VTXP_STUB;
    _fvtx_track = fvtx_track;
    _segment = NULL;
    _max_R = max_R;
    _max_Z = max_Z;
    _max_DCA = max_DCA;
  }
}

Tracklet::Tracklet(SvxSegment* segment,
		   float max_R = 2.0,
		   float max_Z = 12.0,
		   float max_DCA = 1.0) {
  _kind = VTX_TRACK;
  _fvtx_track = NULL;
  _segment = segment;
  _max_R = max_R;
  _max_Z = max_Z;
  _max_DCA = max_DCA;
}

void Tracklet::print() const {
  cout << "Tracklet: " << hex << this << dec << endl;
  cout << "  kind = " << _kind << endl;
  if (_kind == FVTX_TRACK) {
    _fvtx_track->print();
  } else if (_kind == VTX_TRACK) {
    _segment->identify(std::cout);
  } else if (_kind == VTXP_STUB) {
    _fvtx_track->print();
  } else {
    cout << PHWHERE << ":: undefined method for this Tracklet" << endl;
  }

  return;
}

float Tracklet::get_phi() const {
  if ( (_kind == FVTX_TRACK) || (_kind == VTXP_STUB) ) {
    return _fvtx_track->get_fvtx_phi();
  } else if (_kind == VTX_TRACK) {
    float px = _segment->get3MomentumAtPrimaryVertex(0);
    float py = _segment->get3MomentumAtPrimaryVertex(1);
    if (isnan(px) || isnan(py)) {
      px = _segment->get3Momentum(0);
      py = _segment->get3Momentum(1);
    }
    float phi = atan2(py,px);
    return phi;
  } else {
    cout << PHWHERE << ":: undefined method for this Tracklet" << endl;
  }
  return NAN;
}

float Tracklet::get_theta() const {
  if ((_kind == FVTX_TRACK)||(_kind == VTXP_STUB)) {
    return _fvtx_track->get_fvtx_theta();
  } else if (_kind == VTX_TRACK) {
    float pz = _segment->get3MomentumAtPrimaryVertex(2);
    if (isnan(pz)) pz = _segment->get3Momentum(2);
    float mom = _segment->getMomentum();
    float theta = acos(pz/mom);
    return theta;
  } else {
    cout << PHWHERE << ":: undefined method for this Tracklet" << endl;
  }
  return NAN;
}

// return a point on the object near the vertex
PHPoint Tracklet::get_point() const {

  if (_kind == FVTX_TRACK) {

    PHPoint point = _fvtx_track->get_fvtx_vtx();

    // corrective shift
    if (_fvtx_track->get_arm() == 1) {
      point.setX(point.getX() + _correction_vector[NORTH][0]);
      point.setY(point.getY() + _correction_vector[NORTH][1]);
      point.setZ(point.getZ() + _correction_vector[NORTH][2]);
    } else {
      point.setX(point.getX() + _correction_vector[SOUTH][0]);
      point.setY(point.getY() + _correction_vector[SOUTH][1]);
      point.setZ(point.getZ() + _correction_vector[SOUTH][2]);
    }
    return point;

  } else if (_kind == VTXP_STUB) {

    PHPoint point = _fvtx_track->get_fvtx_vtx();

    // corrective shift
    if (fabs(_fvtx_track->get_fvtx_phi()) > 1.57) {
      point.setX(point.getX() + _correction_vector[EAST][0]);
      point.setY(point.getY() + _correction_vector[EAST][1]);
      point.setZ(point.getZ() + _correction_vector[EAST][2]);
    } else {
      point.setX(point.getX() + _correction_vector[WEST][0]);
      point.setY(point.getY() + _correction_vector[WEST][1]);
      point.setZ(point.getZ() + _correction_vector[WEST][2]);
    }
    return point;
    
  } else if (_kind == VTX_TRACK) {

    PHPoint point(NAN,NAN,NAN);

    if(!isnan(_segment->getClosestApproach(0)) &&
       !isnan(_segment->getClosestApproach(1)) &&
       !isnan(_segment->getClosestApproach(2))) {
      point.setX(_segment->getClosestApproach(0));
      point.setY(_segment->getClosestApproach(1));
      point.setZ(_segment->getClosestApproach(2));
    } else if(!isnan(_segment->getInnerMostProjectedPosition(0)) &&
	      !isnan(_segment->getInnerMostProjectedPosition(1)) &&
	      !isnan(_segment->getInnerMostProjectedPosition(2))) {
      point.setX(_segment->getInnerMostProjectedPosition(0));
      point.setY(_segment->getInnerMostProjectedPosition(1));
      point.setZ(_segment->getInnerMostProjectedPosition(2));
    }

    // corrective shift
    if (_segment->get3Momentum(0) < 0.0) {
      point.setX(point.getX() + _correction_vector[EAST][0]);
      point.setY(point.getY() + _correction_vector[EAST][1]);
      point.setZ(point.getZ() + _correction_vector[EAST][2]);
    } else {
      point.setX(point.getX() + _correction_vector[WEST][0]);
      point.setY(point.getY() + _correction_vector[WEST][1]);
      point.setZ(point.getZ() + _correction_vector[WEST][2]);
    }
    
    return point;
  } else {
    cout << PHWHERE << ":: undefined method for this Tracklet" << endl;
  }
  PHPoint pnt(NAN,NAN,NAN);
  return pnt;
}

bool Tracklet::intersects(Tracklet tracklet) const {

  PHPoint pca1 = get_pca(tracklet);
  if (fabs(pca1.getZ()) > _max_Z || isnan(pca1.getZ())) return false;
  if (fabs(pca1.getX()) > _max_R || isnan(pca1.getX())) return false;
  if (fabs(pca1.getY()) > _max_R || isnan(pca1.getY())) return false;

  PHPoint pca2 = tracklet.get_pca(*this);
  if (pca2.getZ() > _max_Z || isnan(pca2.getZ())) return false;
  if (pca2.getX() > _max_R || isnan(pca2.getX())) return false;
  if (pca2.getY() > _max_R || isnan(pca2.getY())) return false;

  //----DEBUG PRINT--------------------------------------------
  // interested in the two track dispersion
  //cout << "__DEBUG_two_tracklet_dca__: " << get_dca(tracklet) << endl;
  //-----------------------------------------------------------

  if (get_dca(tracklet) > _max_DCA) return false;

  return true;
}

float Tracklet::get_dca(PHPoint point) const {
  float dca = NAN;

  if ((_kind == FVTX_TRACK)||(_kind == VTXP_STUB)) {
    float phi1 = get_phi();
    float theta1 = get_theta();
    PHVector vector1(cos(phi1)*sin(theta1),
		     sin(phi1)*sin(theta1),
		     cos(theta1));
    PHPoint point1 = get_point();
    PHLine line1 = PHLine(point1, vector1);
    
    dca = PHGeometry::distanceLinePoint(line1,point);
  } else if (_kind == VTX_TRACK) {
    PHVector vector1(_segment->get3MomentumAtPrimaryVertex(0),
		     _segment->get3MomentumAtPrimaryVertex(1),
		     _segment->get3MomentumAtPrimaryVertex(2));
    PHPoint point1 = get_point();
    PHLine line1(point1,vector1);
    dca = PHGeometry::distanceLinePoint(line1,point);
  } else {
    cout << PHWHERE << ":: undefined method for this Tracklet" << endl;
  }

  return dca;
}

float Tracklet::get_dca(Tracklet tracklet) const {

  float dca = NAN;

  if ( (_kind == FVTX_TRACK) || 
       (_kind == VTXP_STUB) || 
       (_kind == VTX_TRACK)) {
    float phi1 = get_phi();
    float theta1 = get_theta();
    PHVector vector1(cos(phi1)*sin(theta1),
		     sin(phi1)*sin(theta1),
		     cos(theta1));
    PHPoint point1 = get_point();
    PHLine line1 = PHLine(point1, vector1);
    
    if ( (tracklet.get_kind() == FVTX_TRACK) ||
	 (tracklet.get_kind() == VTXP_STUB) ||
	 (tracklet.get_kind() == VTX_TRACK)) {
      
      float phi2 = tracklet.get_phi();
      float theta2 = tracklet.get_theta();
      PHVector vector2(cos(phi2)*sin(theta2),
		       sin(phi2)*sin(theta2),
		       cos(theta2));
      PHPoint point2 = tracklet.get_point();
      PHLine line2 = PHLine(point2, vector2);
      
      dca = PHGeometry::distanceLineLine(line1,line2);
    }
  }

  if (isnan(dca)) {
    cout << PHWHERE << ":: undefined method for these Tracklets" << endl;
  }
  
  return dca;
}

PHPoint Tracklet::get_intersection(PHPlane plane) const {
  PHPoint point(NAN,NAN,NAN);

  if ( (_kind == FVTX_TRACK) || 
       (_kind == VTXP_STUB) || 
       (_kind == VTX_TRACK)) {
    float phi = get_phi();
    float theta = get_theta();
    PHVector vector(cos(phi)*sin(theta),
		    sin(phi)*sin(theta),
		    cos(theta));
    PHLine line = PHLine(get_point(), vector);
    PHGeometry::intersectionLinePlane(line,plane,point);
  } else {
    cout << PHWHERE << ":: undefined method for this Tracklet" << endl;
  }

  return point;
}

PHVector Tracklet::get_momentum(PHPoint point) const {
  
  PHVector momentum(NAN,NAN,NAN);
  
  if ( (_kind == FVTX_TRACK) || 
       (_kind == VTXP_STUB) || 
       (_kind == VTX_TRACK)) {
    float phi = get_phi();
    float theta = get_theta();
    momentum.setX(cos(phi)*sin(theta));
    momentum.setY(sin(phi)*sin(theta));
    momentum.setZ(cos(theta));
  } else {
    cout << PHWHERE << ":: undefined method for this Tracklet" << endl;
  }
  
  return momentum;
}

PHPoint Tracklet::get_pca(PHPoint point) const {

  PHPoint pca(NAN,NAN,NAN);
  
  if ( (_kind == FVTX_TRACK) || 
       (_kind == VTXP_STUB) || 
       (_kind == VTX_TRACK)) {
    float phi1 = get_phi();
    float theta1 = get_theta();
    PHVector vector1(cos(phi1)*sin(theta1),
		     sin(phi1)*sin(theta1),
		     cos(theta1));
    PHPoint point1 = get_point();
    PHLine line1 = PHLine(point1, vector1);
    
    pca = PHGeometry::closestApproachLinePoint(line1,point);
    
  } else {
    cout << PHWHERE << ":: undefined method for these Tracklets" << endl;
  }

  return pca;
}

PHPoint Tracklet::get_pca(Tracklet tracklet) const {

  PHPoint pca(NAN,NAN,NAN);

  if ( (_kind == FVTX_TRACK) || 
       (_kind == VTXP_STUB) || 
       (_kind == VTX_TRACK)) {
    float phi1 = get_phi();
    float theta1 = get_theta();
    PHVector vector1(cos(phi1)*sin(theta1),
		     sin(phi1)*sin(theta1),
		     cos(theta1));
    PHPoint point1 = get_point();
    PHLine line1 = PHLine(point1, vector1);
    
    if ( (tracklet.get_kind() == FVTX_TRACK) ||
	 (tracklet.get_kind() == VTXP_STUB) ||
	 (tracklet.get_kind() == VTX_TRACK)) {
      
      float phi2 = tracklet.get_phi();
      float theta2 = tracklet.get_theta();
      PHVector vector2(cos(phi2)*sin(theta2),
		       sin(phi2)*sin(theta2),
		       cos(theta2));
      PHPoint point2 = tracklet.get_point();
      PHLine line2 = PHLine(point2, vector2); 
      
      if (sin(line1.getDirection().angle(line2.getDirection())) != 0) {
	pca = PHGeometry::closestApproachLineLine(line1,line2);
      }
    }

  } else {
    cout << PHWHERE << ":: undefined method for these Tracklets" << endl;
  }
  
  return pca;
}

float Tracklet::get_fit_weight() const {

  return 1.0; // can't find improvement with this even when only trimming the high side

  float weight = NAN;

  if ((get_kind() == FVTX_TRACK) || (get_kind() == VTXP_STUB)) {
    float chisq_per_ndf = _fvtx_track->get_chi2_ndf();
    if ((chisq_per_ndf > 0.0)&&(chisq_per_ndf < 3.0)) chisq_per_ndf = 3.0;
    weight = TMath::Gaus(chisq_per_ndf,3.0,5.0);
  } else if (get_kind() == VTX_TRACK) {
    float chisq_per_ndf = _segment->getChiSq()/_segment->getNDF();
    if ((chisq_per_ndf > 0.0)&&(chisq_per_ndf < 3.0)) chisq_per_ndf = 3.0;
    weight = TMath::Gaus(chisq_per_ndf,3.0,5.0);
  }

  return weight;
}

//----------------------------
// Intersection implementation
//----------------------------

float Intersection::_max_sep = 1.0;

Intersection::Intersection() {
  // dummy constructor for antikt merging
  _index = 0;
  _tracklets.first = NULL;
  _tracklets.second = NULL;
  _size = 1.0;
}

Intersection::Intersection(unsigned int index, 
			   Tracklet* first, 
			   Tracklet* second, 
			   float max_sep) {
  _index = index;
  _tracklets.first = first;
  _tracklets.second = second;
  _max_sep = max_sep;

  PHPoint pca1 = first->get_pca(*second);
  PHPoint pca2 = second->get_pca(*first); 

  _point.setX(0.5*(pca1.getX()+pca2.getX()));
  _point.setY(0.5*(pca1.getY()+pca2.getY()));
  _point.setZ(0.5*(pca1.getZ()+pca2.getZ()));

  _size = 1.0;
}

void Intersection::print() const {
  std::cout << " Intersection (x,y,z) = (" 
	    << get_x() << ","
	    << get_y() << ","
	    << get_z() << ")" << std::endl;
  return;
}

bool Intersection::is_adjacent(Intersection& intersection) const {

  double xreso = 0.5*(get_reso_x() + intersection.get_reso_x());
  double yreso = 0.5*(get_reso_y() + intersection.get_reso_y());
  double zreso = 0.5*(get_reso_z() + intersection.get_reso_z());
  float rho = sqrt( MUTOO::SQUARE((get_x()-intersection.get_x())/xreso) +
		    MUTOO::SQUARE((get_y()-intersection.get_y())/yreso) +
		    MUTOO::SQUARE((get_z()-intersection.get_z())/zreso));
  if (rho < 1.0) return true;

  return false;
}

double Intersection::distance(Intersection& intersection) const {

  PHPoint point = intersection.get_point();
  double r = PHGeometry::distancePointToPoint(_point,point);
  return r;
}

Intersection Intersection::merge(Intersection& intersection) const {

  Intersection merged;

  // weighted average position
  double size = _size + intersection.size();
  double invsize = 1.0/_size;
  double x = 
    (_size*_point.getX() + intersection.size()*intersection.get_x())*invsize;
  double y = 
    (_size*_point.getY() + intersection.size()*intersection.get_y())*invsize;
  double z = 
    (_size*_point.getZ() + intersection.size()*intersection.get_z())*invsize;

  merged.set_x(x);
  merged.set_x(y);
  merged.set_x(z);
  merged.set_size(size);

  return merged;
}

// a piecewise description of the track z resolution
float Intersection::get_reso_z() const {

  // uniform setting for now
  return _max_sep;

  // \todo test if a variable z resolution will help remove split vertexes
  float reso = _max_sep;

  // the user input of fvtx_Rres is used as a scale adjustment
  // to a piece-wise formulation of the track z resolution
  // estimated from PISA

  float fabsz = fabs(_point.getZ());
  if (fabsz > 60.0) {
    reso *= 2.0+0.15*(fabsz-60.0); // x2 at z=60cm & x5 at z=80cm
  } else if (fabsz > 40.0) {
    reso *= 4.0-0.10*(fabsz-40.0); // x4 at z=40cm & x2 at z=60cm
  } else if (fabsz > 20.0) {
    reso *= 2.0+0.1*(fabsz-20.0); // x2 at z=20cm & x4 at z=40cm
  } else if (fabsz > 10.0) {
    reso *= 1.0+0.1*(fabsz-10.0); // x1 at z=10cm & x2 at z=20.0cm
  } 
  // else just use _fvtx_rRes directly

  return reso;
}

//----------------------
// Vertex implementation
//----------------------

Vertex::Vertex() {
  _point = PHPoint(NAN,NAN,NAN);
  _error = PHPoint(NAN,NAN,NAN);
  _intersections.clear();
  _tracklets.clear();
}

void Vertex::print() const {
  std::cout << "Vertex: " << std::hex << this << std::dec << std::endl;
  std::cout << " nintersections: " << get_num_intersections() << std::endl;
  for (IntersectionConstIter iter = get_intersections_begin();
       iter != get_intersections_end(); ++iter) {
    const Intersection* intersection = (const Intersection*)(*iter);
    intersection->print();
  } 
  std::cout << " ntracklets:     " << get_num_tracklets() << std::endl; 
  for (TrackletConstIter iter = get_tracklets_begin();
       iter != get_tracklets_end(); ++iter) {
    const Tracklet* tracklet = (const Tracklet*)(*iter);
    tracklet->print();
  } 
  std::cout << " vertex (x,y,z) = (" 
	    << get_x() << ","
	    << get_y() << ","
	    << get_z() << ")" << std::endl;
  std::cout << " vertex (ex,ey,ez) = (" 
	    << get_ex() << ","
	    << get_ey() << ","
	    << get_ez() << ")" << std::endl;

  return;
}

void Vertex::add_intersection(Intersection* intersection) {
  _intersections.insert(intersection);
  _tracklets.insert(intersection->get_first_tracklet());
  _tracklets.insert(intersection->get_second_tracklet());
}

PHPoint Vertex::get_ave_point() const {

  if (get_num_intersections() < 1) return PHPoint(NAN,NAN,NAN);

  float x = 0.0;
  float y = 0.0;
  float z = 0.0;

  for (IntersectionConstIter iter = get_intersections_begin();
       iter != get_intersections_end(); ++iter) {
    const Intersection* intersection = (const Intersection*)(*iter);
    
    x += intersection->get_x();
    y += intersection->get_y();
    z += intersection->get_z();
  }

  x /= get_num_intersections();
  y /= get_num_intersections();
  z /= get_num_intersections();

  return PHPoint(x,y,z);
}

PHPoint Vertex::get_ave_error() const {

  if (get_num_intersections() < 2) return PHPoint(NAN,NAN,NAN);

  PHPoint ave = get_ave_point();

  float avex = ave.getX();
  float avey = ave.getY();
  float avez = ave.getZ();

  float dx = 0.0;
  float dy = 0.0;
  float dz = 0.0;

  for (IntersectionConstIter iter = get_intersections_begin();
       iter != get_intersections_end(); ++iter) {
    const Intersection* intersection = (const Intersection*)(*iter);
    
    dx += pow(intersection->get_x()-avex,2);
    dy += pow(intersection->get_y()-avey,2);
    dz += pow(intersection->get_z()-avez,2);
  }

  // "rms"/n
  dx = sqrt(dx)/get_num_intersections();
  dy = sqrt(dy)/get_num_intersections();
  dz = sqrt(dz)/get_num_intersections();

  return PHPoint(dx,dy,dz);
}
