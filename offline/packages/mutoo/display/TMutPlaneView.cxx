// $Id: TMutPlaneView.cxx,v 1.28 2017/07/12 02:04:22 phnxbld Exp $

#include<PHGeometry.h>
#include<TMutPlaneView.h>
#include<MutGeom.h>
#include<MUTOO.h>
#include<PHException.h>
#include<PHGeometry.h>
#include<TLine.h>
#include<TPolyLine.h>
#include<TPave.h>
#include<TMutNode.h>
#include<TMutGeo.h>
#include<TMutTrackUtil.h>
#include<PHTrackIntegratorKF.h>
#include<PHTimer.h>
#include <MutStrip.h>

#include<boost/array.hpp>
#include<cmath>
#include<algorithm>

using namespace std;

//__________________________________________________________
TMutPlaneView::TMutPlaneView(
  unsigned short arm, 
  unsigned short octant,
  int event) :
  _geometry(0),
  _hit_map(0),
  _clus_map(0),
  _coord_map(0),
  _gap_coord_map(0),
  _stub_map(0),
  _trk_map(0), 
  _mc_hit_map(0),
  _mc_trk_map(0),
  _arm(arm),
  _octant(octant),
  _event(event),
  _canvas(0),
  _pad0(0),
  _pad1(0),
  _pad2(0),
  _pad3(0),
  _draw_stubs(true),
  _draw_stub_ext(false),
  _draw_tracks(true),
  _draw_mc_tracks(true),
  _is_monte_carlo(true)
{
  _geometry = (_arm == MUTOO::South) ? SouthArm() : NorthArm();
  setup_canvas();
}

//__________________________________________________________
TMutPlaneView::~TMutPlaneView()
{ _canvas->Close(); }

//__________________________________________________________
Bool_t TMutPlaneView::event(PHCompositeNode* top_node)
{
  _cathode_coord.clear();
  _hit_coord.clear();
  _peak_coord.clear();
  _offset_coord.clear();
  _coord.clear();
  _gap_coord.clear();
  _mc_hit.clear();
  _mc_trk.clear();
  _track_windows.at(0).clear();
  _track_windows.at(1).clear();
  _track_windows.at(2).clear();
  _stubs.at(0).clear();
  _stubs.at(1).clear();
  _stubs.at(2).clear();
  _tracks.clear();
  
  try {
    
    // TMutHit IOC
    _hit_map = TMutNode<TMutHitMap>::find_node(top_node,"TMutHitMap");
    
    // TMutClus IOC
    _clus_map = TMutNode<TMutClusMap>::find_node(top_node,"TMutClusMap");
    
    // TMutCoord IOC
    _coord_map = TMutNode<TMutCoordMap>::find_node(top_node,"TMutCoordMap");
    
    // TMutGapCoord IOC
    _gap_coord_map = TMutNode<TMutGapCoordMap>::find_node(top_node,"TMutGapCoordMap");
    
    // TMutStub IOC
    _stub_map = TMutNode<TMutStubMap>::find_node(top_node,"TMutStubMap");
    
    // TMutTrk IOC
    _trk_map = TMutNode<TMutTrkMap>::find_node(top_node,"TMutTrkMap");
    
  } catch(std::exception& e){
    MUTOO::TRACE(DESCRIPTION(e.what()));
  }
  
  // If not running on simulated data then set flag
  try {
    _mc_hit_map = TMutNode<TMutMCHitMap>::find_node(top_node,"TMutMCHitMap");
    _mc_trk_map = TMutNode<TMutMCTrkMap>::find_node(top_node,"TMutMCTrkMap");
  } catch(std::exception) {
    _is_monte_carlo = false;
  }
  return true;
}

//__________________________________________________________
void TMutPlaneView::setup_canvas()
{
  // setup canvas and some pads
  //
  std::ostringstream tmp;
  tmp << "mutoo_" << _arm << "_" << _octant << std::endl;
  std::string name = tmp.str();
  
  std::ostringstream tmp2;
  tmp2 << "MUTR Plane View, Arm " << _arm << ", Octant " 
    << _octant << ", Event " << _event << std::endl;
  std::string name2 = tmp2.str();
  _canvas = new TCanvas(name.c_str(),name2.c_str(),900,700);
  _canvas->SetFillColor(0);
  
  // fill coordinate vectors with frame data from this arm/octant/half_octant
  //
  get_frame_coord();
  
  _pad0 = new TPad("pad0","pad0",0.00, 0.90,1.0, 1.0,0);
  
  _pad0->SetBorderMode(0);
  _pad0->HighLight(0,0);
  _pad0->Range(0,0,1,1);  
  _label = new TPaveLabel(0.0,0.0,1,1,name2.c_str());
  _label->SetFillColor(0);
  _label->SetBorderSize(0);
  _label->SetTextFont(42);
  
  
  double xmin=0, ymin=0, xmax=0, ymax=0;
  
  if(_arm == MUTOO::North) 
  {
    
    _pad1 = new TPad("pad1","pad1",0.00, 0.00, 1.00, 0.3,0);
    _pad1->SetBorderMode(0);
    
    xmin = std::min(_range.at(0)[0].second,_range.at(0)[1].second);
    xmax = std::max(_range.at(0)[0].second,_range.at(0)[1].second);
    ymin = std::min(_range.at(0)[0].first,_range.at(0)[1].first);
    ymax = std::max(_range.at(0)[0].first,_range.at(0)[1].first);
    _pad1->Range(xmin,ymin,xmax,ymax);
    
    _pad2 = new TPad("pad2","pad2",0.00, 0.3, 1.00, 0.6,0);
    _pad2->SetBorderMode(0);
    
    xmin = std::min(_range.at(1)[0].second,_range.at(1)[1].second);
    xmax = std::max(_range.at(1)[0].second,_range.at(1)[1].second);
    ymin = std::min(_range.at(1)[0].first,_range.at(1)[1].first);
    ymax = std::max(_range.at(1)[0].first,_range.at(1)[1].first);
    _pad2->Range(xmin,ymin,xmax,ymax);
    
    _pad3 = new TPad("pad3","pad3",0.00, 0.6, 1.00, 0.9,0);  
    _pad3->SetBorderMode(0);
    
    xmin = std::min(_range.at(2)[0].second,_range.at(2)[1].second);
    xmax = std::max(_range.at(2)[0].second,_range.at(2)[1].second);
    ymin = std::min(_range.at(2)[0].first,_range.at(2)[1].first);
    ymax = std::max(_range.at(2)[0].first,_range.at(2)[1].first);
    _pad3->Range(xmin,ymin,xmax,ymax);
    
  } else {
    
    _pad3 = new TPad("pad3","pad3",0.00, 0.00, 1.00, 0.3,0);
    _pad3->SetBorderMode(0);
    
    xmin = std::min(_range.at(0)[0].second,_range.at(0)[1].second);
    xmax = std::max(_range.at(0)[0].second,_range.at(0)[1].second);
    ymin = std::min(_range.at(0)[0].first,_range.at(0)[1].first);
    ymax = std::max(_range.at(0)[0].first,_range.at(0)[1].first);
    _pad3->Range(xmin,ymin,xmax,ymax);
    
    _pad2 = new TPad("pad2","pad2",0.00, 0.3, 1.00, 0.6,0);
    _pad2->SetBorderMode(0);
    
    xmin = std::min(_range.at(1)[0].second,_range.at(1)[1].second);
    xmax = std::max(_range.at(1)[0].second,_range.at(1)[1].second);
    ymin = std::min(_range.at(1)[0].first,_range.at(1)[1].first);
    ymax = std::max(_range.at(1)[0].first,_range.at(1)[1].first);
    _pad2->Range(xmin,ymin,xmax,ymax);
    
    _pad1 = new TPad("pad1","pad1",0.00, 0.6, 1.00, 0.9,0);  
    _pad1->SetBorderMode(0);
    
    xmin = std::min(_range.at(2)[0].second,_range.at(2)[1].second);
    xmax = std::max(_range.at(2)[0].second,_range.at(2)[1].second);
    ymin = std::min(_range.at(2)[0].first,_range.at(2)[1].first);
    ymax = std::max(_range.at(2)[0].first,_range.at(2)[1].first);
    _pad1->Range(xmin,ymin,xmax,ymax);
    
  }
  
  _pad0->Draw();
  _pad1->Draw();
  _pad2->Draw();
  _pad3->Draw();
  
  return;
}


//__________________________________________________________
void TMutPlaneView::paint()
{
      
  // clear the existing primitives from last paint
  //
  clear_temp_storage();
  
  _pad0->cd();
  _label->Draw();  
  _pad1->cd();
  
  paint_frames_wz();
  paint_planes(VIEW_WZ);
  paint_hits(VIEW_WZ);
  paint_clusters(VIEW_WZ);
  paint_coord(VIEW_WZ);
  paint_gap_coord(VIEW_WZ);
  if(_is_monte_carlo ) {
    paint_mc_hit(VIEW_WZ);
    if(_draw_mc_tracks) paint_mc_trk(VIEW_WZ);
  }
  if(_arm == MUTOO::North) {
    if(_draw_stubs) paint_stubs(0);
    if(_draw_tracks) paint_trks(0);
  } else {
    if(_draw_stubs) paint_stubs(2);
    if(_draw_tracks) paint_trks(2);
  }
  
  _pad2->cd();
  paint_frames_wz();
  paint_planes(VIEW_WZ);
  paint_hits(VIEW_WZ);
  paint_clusters(VIEW_WZ);
  paint_coord(VIEW_WZ);
  paint_gap_coord(VIEW_WZ);
  if(_is_monte_carlo) {
    paint_mc_hit(VIEW_WZ);
    if(_draw_mc_tracks) paint_mc_trk(VIEW_WZ);
  }
  
  if(_draw_stubs) paint_stubs(1);
  if(_draw_tracks) paint_trks(1);
  
  _pad3->cd();
  paint_frames_wz();
  paint_planes(VIEW_WZ);
  paint_hits(VIEW_WZ);
  paint_clusters(VIEW_WZ);
  paint_coord(VIEW_WZ);
  paint_gap_coord(VIEW_WZ);
  
  if(_is_monte_carlo ) {
    paint_mc_hit(VIEW_WZ);
    if(_draw_mc_tracks) paint_mc_trk(VIEW_WZ);
  }
  
  if(_arm == MUTOO::North) {
    if(_draw_stubs) paint_stubs(2);
    if(_draw_tracks) paint_trks(2);
  } else {
    if(_draw_stubs) paint_stubs(0);
    if(_draw_tracks) paint_trks(0);
  }
}

//__________________________________________________________
void TMutPlaneView::get_frame(unsigned short station)
{
  
  // figure out the index of the last gap in current station
  //
  unsigned short last_gap = _geometry->
    f_pMutStations[station]->
    f_pMutOctants[_octant]->
    f_pMutHalfOctants[0]->getNumberOfGaps() - 1;
  
  // coordinate first strip, first plane
  //
  double phi_00 = _geometry->
    f_pMutStations[station]->
    f_pMutOctants[_octant]->getBeginPhi();
  
  double phi_01 = _geometry->
    f_pMutStations[station]->
    f_pMutOctants[_octant]->getEndPhi();
  
  double r_out = _geometry->
    f_pMutStations[station]->
    f_pMutOctants[_octant]->getOuterRadius();
  
  double x_1 = r_out*std::cos(phi_00);
  double y_1 = r_out*std::sin(phi_00);
  double x_2 = r_out*std::cos(phi_01);
  double y_2 = r_out*std::sin(phi_01);
  
  double x_mid = 0.5*(x_1 + x_2);
  double y_mid = 0.5*(y_1 + y_2);
  double phi_axis = atan2(y_mid,x_mid);
  
  _cos_x_xp = std::cos(phi_axis);
  _sin_x_xp = std::sin(phi_axis);
  
  double z_front = _geometry->
    f_pMutStations[station]->
    f_pMutOctants[_octant]->
    f_pMutHalfOctants[0]->
    f_pMutGaps[0]->
    f_pMutPlanes[0]->f_pMutStrips[0]->getGlobalPositionEnd().getZ();
  
  // coordinate last strip,  last plane
  //
  double z_back = _geometry->
    f_pMutStations[station]->
    f_pMutOctants[_octant]->
    f_pMutHalfOctants[0]->
    f_pMutGaps[last_gap]->
    f_pMutPlanes[MUTOO::Cathode2]->f_pMutStrips[0]->getGlobalPositionEnd().getZ();
  
  PHPoint c_00(x_1,y_1,z_front);	       	       
  PHPoint c_01(x_2,y_2,z_front);
  PHPoint c_10(x_1,y_1,z_back);
  PHPoint c_11(x_2,y_2,z_back);
  
  _origin.at(station) = c_00;
  _last_strip.at(station) = c_01;
  
  // Okay now we have all the coordinates transform to zw frame and store
  // in frame array associated with current station
  //
  coord_pair zw_00 = transform_zw(station,c_00);
  coord_pair zw_01 = transform_zw(station,c_01);
  coord_pair zw_10 = transform_zw(station,c_10);
  coord_pair zw_11 = transform_zw(station,c_11);
  
  // Now make the frame slightly larger than enclosed strips 
  //
  double len_1 = std::fabs(zw_00.second - zw_01.second);
  double len_2 = std::fabs(zw_00.second - zw_10.second);  
  double len = std::max(len_1,len_2);
  
  double wid_1 = std::fabs(zw_00.first - zw_11.first);
  double wid_2 = std::fabs(zw_01.first - zw_10.first);  
  double wid = std::max(wid_1,wid_2);
  
  const double bord_z_box=2;
  const double bord_w_box = 8;
  const double bord_z_fr=4;
  const double bord_w_fr = 12.0;
  
  double sgn = (_arm == MUTOO::South) ? 1.0 : -1.0;
  
  coord_pair box_00 = std::make_pair(zw_00.first + sgn*bord_z_box,
    zw_00.second-bord_w_box);
  
  coord_pair box_11 = std::make_pair(zw_00.first - sgn*(wid+bord_z_box), 
    zw_00.second+len+bord_w_box);
  
  coord_pair range_00 = std::make_pair(zw_00.first + sgn*bord_z_fr,
    zw_00.second-bord_w_fr);
  
  coord_pair range_11 = std::make_pair(zw_00.first - sgn*(wid+bord_z_fr), 
    zw_00.second+len+bord_w_fr);
  
  _frames.at(station).push_back(box_00);
  _frames.at(station).push_back(box_11);
  _range.at(station).push_back(range_00);
  _range.at(station).push_back(range_11);  
  
}

//__________________________________________________________
std::pair<double,double> TMutPlaneView::transform_zw(unsigned short station, const PHPoint& point)
{
  double w = -1.0*point.getX()*_sin_x_xp + point.getY()*_cos_x_xp;
  return std::make_pair(point.getZ(),w);
}
  
//__________________________________________________________
void TMutPlaneView::paint_planes(View view)
{
  // if coordinate vector is empty the fill it
  //
  if(_cathode_coord.size()==0){
    get_cathode_coord();
  }  
  // iterate over coordinate vector and draw mark 
  // for each strip center
  //
  coord_vector::iterator iter = _cathode_coord.begin();
  for(;iter!=_cathode_coord.end();++iter){
    if(view == VIEW_ZW){
      TMarker* marker = new TMarker(iter->first,iter->second,1);
      marker->Draw();
      _marker_vector.push_back(marker);
    } else {
      TMarker* marker = new TMarker(iter->second,iter->first,1);
      marker->Draw();
      _marker_vector.push_back(marker);
    }
  }
}

//__________________________________________________________
void TMutPlaneView::paint_hits(View view)
{
  // if coordinate vector is empty the fill it
  //
  if(_hit_coord.size()==0){
    get_hits();
  }  
  // iterate over coordinate vector and draw mark 
  // for each strip center
  //
  coord_vector::iterator iter = _hit_coord.begin();
  for(;iter!=_hit_coord.end();++iter){
    if(view == VIEW_ZW) {
      TMarker* marker = new TMarker(iter->first,iter->second,6);
      marker->SetMarkerColor(2);
      marker->SetMarkerSize(4);
      marker->Draw();
      _marker_vector.push_back(marker);
    } else {
      TMarker* marker = new TMarker(iter->second,iter->first,6);
      marker->SetMarkerColor(2);
      marker->SetMarkerSize(4);
      marker->Draw();
      _marker_vector.push_back(marker);
    }
  }
}

//__________________________________________________________
void TMutPlaneView::paint_clusters(View view)
{
  // if coordinate vector is empty the fill it
  //
  if(_peak_coord.size()==0){
    get_clusters();
  }  
  // iterate over coordinate vector and draw mark 
  // for each strip center
  //
  {
    coord_vector::iterator iter = _peak_coord.begin();
    for(;iter!=_peak_coord.end();++iter){
      if(view == VIEW_ZW){
        TMarker* marker = new TMarker(iter->first,iter->second,6);
        marker->SetMarkerColor(4);
        marker->SetMarkerSize(4);
        marker->Draw();
        _marker_vector.push_back(marker);
      } else {
        TMarker* marker = new TMarker(iter->second,iter->first,6);
        marker->SetMarkerColor(4);
        marker->SetMarkerSize(4);
        marker->Draw();
        _marker_vector.push_back(marker);
      }
    }
  }
  {
    coord_vector::iterator iter = _offset_coord.begin();
    for(;iter!=_offset_coord.end();++iter){
      if(view == VIEW_ZW){
        TMarker* marker = new TMarker(iter->first,iter->second,5);
        marker->SetMarkerColor(4);
        marker->SetMarkerSize(0.6);
        marker->Draw();
        _marker_vector.push_back(marker);
      } else {
        TMarker* marker = new TMarker(iter->second,iter->first,5);
        marker->SetMarkerColor(4);
        marker->SetMarkerSize(0.6);
        marker->Draw();
        _marker_vector.push_back(marker);
      }
    }
  }
}

//__________________________________________________________
void TMutPlaneView::paint_frames_wz()
{
  
  // if frame coord vector is empty then fill it
  //
  if(_frames.at(0).size()==0){
    get_frame_coord();
  }  
  
  // Draw half octant frame
  //  
  for(int station = 0; station<MUTOO::NumberOfStations; ++station){
    // if coordinate vector is empty the fill it
    //
    enum {FRAME_SIZE=2};
    if(_frames.at(station).size() < FRAME_SIZE){
      throw std::out_of_range(DESCRIPTION("wrong size in frame coord vector"));
    }
    _box.at(station) = new TPave(_frames.at(station)[0].second,
      _frames.at(station)[0].first,
      _frames.at(station)[1].second,
      _frames.at(station)[1].first);	     
    
    _box.at(station)->SetLineColor(station_color(station));
    _box.at(station)->SetFillColor(0);
    _box.at(station)->SetLineWidth(1);
    _box.at(station)->SetBorderSize(1);
    _box.at(station)->Draw();
  }
}

//__________________________________________________________
void TMutPlaneView::paint_gap_coord(View view)
{
  // if coordinate vector is empty the fill it
  //
  if(_gap_coord.size()==0){
    get_gap_coord();
  }  
  // iterate over coordinate vector and draw mark 
  // for each strip center
  //
  {
    coord_vector::iterator iter = _gap_coord.begin();
    for(;iter!=_gap_coord.end();++iter){
      if(view == VIEW_ZW){
        TMarker* marker = new TMarker(iter->first,iter->second,28);
        marker->SetMarkerColor(50);
        marker->SetMarkerSize(0.5);
        marker->Draw();
        _marker_vector.push_back(marker);
      } else {
        TMarker* marker = new TMarker(iter->second,iter->first,28);
        marker->SetMarkerColor(50);
        marker->SetMarkerSize(0.5);
        marker->Draw();
        _marker_vector.push_back(marker);
      }
    }
  }
}

//__________________________________________________________
void TMutPlaneView::paint_mc_trk(View view)
{
  
  cout << "TMutPlaneView::paint_mc_trk" << endl;
  
  //return;
  // if coordinate vector is empty the fill it
  //
  if(_mc_trk.size()==0){
    get_mc_trk();
  }  
  // iterate over coordinate vector and draw mark 
  // for each strip center
  //
  {
    TPolyLine* poly = new TPolyLine(_mc_trk.size());
    coord_vector::iterator iter = _mc_trk.begin();
    int i = 0;
    for(;iter!= _mc_trk.end();++iter){      
      if(view == VIEW_ZW){
        poly->SetPoint(i++,iter->first,iter->second);
      } else {
        poly->SetPoint(i++,iter->second,iter->first);
      }
    }
    poly->SetLineColor(2);
    poly->SetLineStyle(2);
    poly->Draw();
  }
}

//__________________________________________________________
void TMutPlaneView::paint_mc_hit(View view)
{
  // if coordinate vector is empty the fill it
  //
  if(_mc_hit.size()==0){
    get_mc_hit();
  }  
  // iterate over coordinate vector and draw mark 
  // for each strip center
  //
  {
    coord_vector::iterator iter = _mc_hit.begin();
    for(;iter!=_mc_hit.end();++iter){
      if(view == VIEW_ZW){
        TMarker* marker = new TMarker(iter->first,iter->second,5);
        marker->SetMarkerColor(1);
        marker->SetMarkerSize(0.7);
        marker->Draw();
        _marker_vector.push_back(marker);
      } else {
        TMarker* marker = new TMarker(iter->second,iter->first,5);
        marker->SetMarkerColor(1);
        marker->SetMarkerSize(0.7);
        marker->Draw();
        _marker_vector.push_back(marker);
      }
    }
  }
}

//__________________________________________________________
void TMutPlaneView::paint_coord(View view)
{
  // if coordinate vector is empty the fill it
  //
  if(_coord.size()==0){
    get_coord();
  }  
  // iterate over coordinate vector and draw mark 
  // for each strip center
  //
  {
    coord_vector::iterator iter = _coord.begin();
    for(;iter!=_coord.end();++iter){
      if(view == VIEW_ZW){
        TMarker*  marker = new TMarker(iter->first,iter->second,5);
        marker->SetMarkerColor(4);
        marker->SetMarkerSize(0.7);
        marker->Draw();
        _marker_vector.push_back(marker);
      } else {
        TMarker* marker = new TMarker(iter->second,iter->first,5);
        marker->SetMarkerColor(4);
        marker->SetMarkerSize(0.7);
        marker->Draw();
        _marker_vector.push_back(marker);
      }
    }
  }
}

//__________________________________________________________
void TMutPlaneView::get_frame_coord()
{
  for(int station=0;station<MUTOO::NumberOfStations;++station){
    get_frame(station);
  }
}

//__________________________________________________________
void TMutPlaneView::get_cathode_coord()
{  
  
  // sanity check
  if(_arm >= MUTOO::NumberOfArms || _octant >= MUTOO::NumberOfOctants) 
  {
    throw std::invalid_argument(DESCRIPTION("bad specification"));
  }
  
  // the looping looks a bit weird but recall that _arm, and _octant 
  // are state variable of this object.  So we loop over everything else.
  //
  for(int station=0;station<MUTOO::NumberOfStations;++station){
    for(int half_octant=0;half_octant<MUTOO::NumberOfHalfOctants;++half_octant){    
      //unsigned short half_octant = 0;
      // determine the number of gaps in current station
      //
      unsigned short n_gap = _geometry->
        f_pMutStations[station]->
        f_pMutOctants[_octant]->
        f_pMutHalfOctants[half_octant]->getNumberOfGaps();
      
      for(int gap=0;gap<n_gap;++gap){
        get_cathode(station,
          half_octant,
          gap,
          MUTOO::Cathode1);
        get_cathode(station,
          half_octant,
          gap,
          MUTOO::Cathode2);
      }
    }
  }
}

//__________________________________________________________
void TMutPlaneView::get_cathode(unsigned short station, 
  unsigned short half_octant,
  unsigned short gap,
  unsigned short plane)
  
{
  enum {MARKER_TYPE=1};
  
  MutArm* geometry = (_arm == MUTOO::South) ? SouthArm() : NorthArm();
  
  // determine the number of strips in current cathode
  // 
  unsigned short n_strip = geometry->
    f_pMutStations[station]->
    f_pMutOctants[_octant]->
    f_pMutHalfOctants[half_octant]->
    f_pMutGaps[gap]->
    f_pMutPlanes[plane]->getNumElements();
  
  
  // Determine local w space offset for polymarkers.  We are drawing
  // things in a coordinate system that is defined by the tangent to
  // the half-octant at the outer radius.  We plot the distance from
  // the endpoint of the first strip in the half octant plus an offset
  // chosed to seperate half octants in our funny psuedo-radial view.
  //
  
  PHPoint first = geometry->
    f_pMutStations[station]->
    f_pMutOctants[_octant]->
    f_pMutHalfOctants[half_octant]->
    f_pMutGaps[gap]->
    f_pMutPlanes[plane]->
    f_pMutStrips[0]->getGlobalPositionEnd();
  
  // We map 2*pi*r onto linear w by taking w =r*phi + distance to first 
  // strip where r and phi are the radial coordinates of the first strip 
  // in the half octant
  
  for(int i_strip=0;i_strip<n_strip;++i_strip){
    PHPoint strip = geometry->
      f_pMutStations[station]->
      f_pMutOctants[_octant]->
      f_pMutHalfOctants[half_octant]->
      f_pMutGaps[gap]->
      f_pMutPlanes[plane]->
      f_pMutStrips[i_strip]->getGlobalPositionEnd();
    
    coord_pair strip_zw = transform_zw(station,strip);
    
    // append to coordinates vector
    //
    _cathode_coord.push_back(strip_zw);
  }  
}

//__________________________________________________________
void TMutPlaneView::get_clusters()
{
  if(!_clus_map){
    MUTOO::TRACE("TMutPlaneView::get_clusters: No cluster map");
    return;
  }  
  // iterate through all clusters
  //
  TMutClusMap::const_iterator clus_iter = _clus_map->range();
  while(TMutClusMap::const_pointer clus_ptr = clus_iter.next()){    
    // if hit is not in this view then continue;
    //
    if(clus_ptr->get()->get_arm() != _arm ||
      clus_ptr->get()->get_octant() != _octant) continue;
    // get gap strip and offset coordinate
    //
    get_cluster_coord(clus_ptr);
  }
}

//__________________________________________________________
void TMutPlaneView::get_hits()
{
  if(!_hit_map){
    MUTOO::TRACE("TMutPlaneView::get_hits: No hit map");
    return;
  }  
  // iterate through all clusters
  //
  TMutHitMap::const_iterator hit_iter = _hit_map->range();
  while(TMutHitMap::const_pointer hit_ptr = hit_iter.next())
  {    
    
    // if hit is not in this view then continue;
    //
    if(hit_ptr->get()->get_arm() != _arm ||
      hit_ptr->get()->get_octant() != _octant) continue;
    // push this hit onto  _hit_coord vector 
    //
    get_strip_coord(hit_ptr);
  }
}

//__________________________________________________________
void TMutPlaneView::get_stubs()
{
  if(!_stub_map){
    MUTOO::TRACE("TMutPlaneView::get_hits: No stub map");
    return;
  }  
  TMutStubMap::const_iterator stub_iter = _stub_map->range();
  while(TMutStubMap::const_pointer stub_ptr = stub_iter.next()){
    
    // if track is not in this view then continue;
    //
    if(stub_ptr->get()->get_arm() != _arm ||
      stub_ptr->get()->get_octant() != _octant) continue;    
    
    for(unsigned short station=0; station<MUTOO::NumberOfStations;++station) {
      
      // If we're not drawing stub extrapolations then continue if not
      // in the same station as the current stub
      if(!_draw_stub_ext && stub_ptr->get()->get_station() != station) continue;
      
      const TMutFitPar* fit_par = stub_ptr->get()->get_fit_par();
      
      float delta = (_arm == MUTOO::South) ? -50 : 50;
      
      double z = TMutGeo::get_anode_plane_position(stub_ptr->get()->get_arm(),
        station,
        stub_ptr->get()->get_octant(),
        stub_ptr->get()->get_half_octant(),
        MUTOO::Gap1).getZ();
      
      // Get the endpoints 
      PHPoint trk_end = TMutTrackUtil::linear_track_model(fit_par,z - delta);  
      PHPoint trk_beg = TMutTrackUtil::linear_track_model(fit_par,z + delta);    
      
      // transform the track endpoints to zw space for this station
      coord_pair begin_zw = transform_zw(station,trk_beg);
      coord_pair end_zw = transform_zw(station,trk_end);
      int line_flag = (stub_ptr->get()->get_station() == station) ? 
        -1 : station_color(stub_ptr->get()->get_station());      
      _stubs.at(station).push_back(std::make_pair(std::make_pair(begin_zw,end_zw),line_flag));
      
    }
  }
}

//__________________________________________________________
void TMutPlaneView::get_mc_trk()
{
  if(!_mc_trk_map){
    MUTOO::TRACE("TMutPlaneView::get_mc_trks: No mc_trk map");
    return;
  }  
  TMutMCTrkMap::const_iterator trk_iter = _mc_trk_map->range();
  while(TMutMCTrkMap::const_pointer trk_ptr = trk_iter.next()){    
    // if track is not in this view then continue;
    //
    if(trk_ptr->get()->get_arm() != _arm) continue;    
    
    // Initialize track integrator at upstream coordinate
    TMutTrkPar trk_par_orig(
      trk_ptr->get()->get_x_us_gap(),
      trk_ptr->get()->get_y_us_gap(),
      trk_ptr->get()->get_z_us_gap(),
      trk_ptr->get()->get_px_us_gap(),
      trk_ptr->get()->get_py_us_gap(),
      trk_ptr->get()->get_pz_us_gap(),
      (int)trk_ptr->get()->get_charge()); 
    
    PHPoint us_point(trk_ptr->get()->get_x_us_gap(),
      trk_ptr->get()->get_y_us_gap(),
      trk_ptr->get()->get_z_us_gap());
    
    // If MC track not in this octants phi range then punt
    if(!TMutGeo::in_phi_fiducial(_arm,_octant,us_point)) continue;
    
    // Determine the endpoints for drawing the MC track
    double delta = (_arm == MUTOO::South) ? -30 : 30;
    double z_us = trk_ptr->get()->get_z_us_gap() - delta;
    double z_ds = trk_ptr->get()->get_z_ds_gap() + delta;
    
    double sgn = (z_ds-z_us)/std::fabs(z_ds-z_us);
    double z = z_us;
    PHTimer local;
    while(std::fabs(z) < std::fabs(z_ds))
    {
      
      // Take 0.5 cm steps in the dowstream direction
      z += sgn*0.5;
      PHTrackIntegratorKF integrator;
      integrator.initialize( trk_par_orig );
      integrator.extrapolate(z);
      if( integrator.get_error() )
      {
        cout << "TMutPlaneView::get_mc_trk - extrapolation to z=" << z << " failed" << endl;
        continue;
      }
      
      TMutTrkPar trk_par_out;
      integrator.finish( trk_par_out );
      
      // Figure out which station is closest to extrapolate point
      // and push the w,z pair onto the _mc_trk list after doing
      // the transform to wz space
      PHPoint point(
        trk_par_out.get_x(),
        trk_par_out.get_y(),
        trk_par_out.get_z());
      coord_pair mc_coord = transform_zw(nearest_station(z),point);
      _mc_trk.push_back(mc_coord);
      
    }
  }
}

//__________________________________________________________
void TMutPlaneView::get_field_on_trk()
{
  if(!_trk_map){
    MUTOO::TRACE("TMutPlaneView::get_field_on_trk: No track map");
    return;
  }  
  
  TMutTrkMap::const_iterator trk_iter = _trk_map->range();
  while(TMutTrkMap::const_pointer trk_ptr = trk_iter.next())
  {    
    
    // Require reco success and that track is in given octant
    if(!trk_ptr->get()->get_reco_success() || 
      trk_ptr->get()->get_arm() != _arm || 
      trk_ptr->get()->get_octant() != _octant) continue;    
    
    // get track parameters
    TMutTrkPar trk_par_orig( *trk_ptr->get()->get_trk_par() );
    
    // Determine the endpoints for drawing the  track
    double delta = 30.0;
    double z_us = (_arm == MUTOO::South) ? -188 + delta : 100 - delta;
    double z_ds = (_arm == MUTOO::South) ? -469 - delta : 630 + delta;
    
    double sgn = (z_ds-z_us)/std::fabs(z_ds-z_us);
    double z = z_us;
    PHTimer local;
    
    // Local storage for this tracks points
    coord_vector single_track;
    
    while(std::fabs(z) < std::fabs(z_ds))
    {
      // Take 2 cm steps in the dowstream direction
      z += sgn*2;
      
      PHTrackIntegratorKF integrator;
      integrator.initialize( trk_par_orig );
      integrator.extrapolate(z);
      
      if( integrator.get_error() )
      {
        cout << "TMutPlaneView::get_field_on_trk - extrapolation to z=" << z << " failed" << endl;
        continue;
      }

      // Figure out which station is closest to extrapolate point
      // and push the w,z pair onto the _mc_trk list after doing
      // the transform to wz space

      TMutTrkPar trk_par_out;
      integrator.finish( trk_par_out );
      PHPoint point(
        trk_par_out.get_x(),
        trk_par_out.get_y(),
        trk_par_out.get_z());
      
      coord_pair coord = transform_zw(nearest_station(z),point);
      single_track.push_back(coord);
      
    }
    
    // Push track points onto vector of tracks
    _tracks.push_back(single_track);
    
  }
}

//__________________________________________________________
void TMutPlaneView::get_trk_windows()
{
  if(!_trk_map){
    MUTOO::TRACE("TMutPlaneView::get_trk: No track map");
    return;
  }  
  TMutTrkMap::const_iterator trk_iter = _trk_map->range();
  while(TMutTrkMap::const_pointer trk_ptr = trk_iter.next()){    
    for(int station = 0; station<MUTOO::NumberOfStations; ++station){
      
      // Punt if not in this view or null window
      //
      if(trk_ptr->get()->get_octant() != _octant) continue;
      if(trk_ptr->get()->get_phi_min(station) == trk_ptr->get()->get_phi_max(station)) continue;
      
      double z1 = TMutGeo::get_anode_plane_position(trk_ptr->get()->get_arm(),
        station,
        trk_ptr->get()->get_octant(),
        MUTOO::HalfOctant1,
        MUTOO::Gap1).getZ();						    
      unsigned short gap = (station == MUTOO::Station3) ? MUTOO::Gap2 : MUTOO::Gap3;
      double z2 = TMutGeo::get_anode_plane_position(trk_ptr->get()->get_arm(),
        station,
        trk_ptr->get()->get_octant(),
        MUTOO::HalfOctant1,
        gap).getZ();
      // Line at phi min
      //
      {
        // x = z*tan(theta)*cos(phi)
        //
        double r=0;
        if(trk_ptr->get()->has_stub(station)) {
          // Draw phi line at r of track
          //
          TMutStubMap::const_key_iterator stub_iter = trk_ptr->get()->get_associated<TMutStub>();
          while(TMutStubMap::const_pointer stub_ptr = stub_iter.next()){
            if(stub_ptr->get()->get_station() == station){
              r = std::sqrt(MUTOO::SQUARE(stub_ptr->get()->get_fit_par()->get_x()) +
                MUTOO::SQUARE(stub_ptr->get()->get_fit_par()->get_y()));
            }
          }
        } else {
          // Draw phi line at r max of station
          //
          r = std::fabs(z1*std::tan(0.5*(trk_ptr->get()->get_theta_min(station) 
            + trk_ptr->get()->get_theta_max(station))));
        }
        
        double x = r*std::cos(trk_ptr->get()->get_phi_min(station));
        
        // y = z*tan(theta)*sin(phi)
        //
        double y = r*std::sin(trk_ptr->get()->get_phi_min(station));
        
        // Transform and push onto vector
        //
        PHPoint point1(x,y,z1);
        PHPoint point2(x,y,z2);
        coord_pair begin = transform_zw(station,point1);
        coord_pair end = transform_zw(station,point2);
        _track_windows.at(station).push_back(std::make_pair(begin,end));
      }      
      
      // Line at phi max
      //
      {
        // x = z*tan(theta)*cos(phi)
        //
        double r = std::fabs(z1*std::tan(trk_ptr->get()->get_theta_min(station)));
        double x = r*std::cos(trk_ptr->get()->get_phi_max(station));
        
        // y = z*tan(theta)*sin(phi)
        //
        double y = r*std::sin(trk_ptr->get()->get_phi_max(station));
        
        // Transform and push onto vector
        //
        PHPoint point1(x,y,z1);
        PHPoint point2(x,y,z2);
        coord_pair begin = transform_zw(station,point1);
        coord_pair end = transform_zw(station,point2);
        _track_windows.at(station).push_back(std::make_pair(begin,end));
      }      
    }
  }
}

//__________________________________________________________
void TMutPlaneView::paint_trks(unsigned short station)
{
  // if coordinate vector is empty the fill it
  //
  if(!_tracks.size()) get_field_on_trk();
  
  // iterate over coordinate vector and draw mark 
  // for each strip center
  //
  {
    std::vector<coord_vector>::const_iterator track_iter = _tracks.begin();
    // Loop over tracks
    //
    for(;track_iter != _tracks.end();++track_iter) {
      coord_vector::const_iterator point_iter = track_iter->begin();
      // Instantiate a poly line for current track
      //
      TPolyLine* poly = new TPolyLine(track_iter->size());   
      // Loop over points in track
      //
      int i = 0;
      for(;point_iter!=track_iter->end();++point_iter){
        poly->SetPoint(i++,point_iter->second,point_iter->first);
      }
      poly->SetLineColor(1);
      poly->SetLineWidth(2);
      poly->Draw();
    }
  }
}

//__________________________________________________________
void TMutPlaneView::paint_stubs(unsigned short station)
{
  // if coordinate vector is empty the fill it
  //
  if(_stubs.at(0).size()==0){
    get_stubs();
  }  
  
  // iterate over coordinate vector and draw mark 
  // for each strip center
  //
  {
    for(int i=0; i<MUTOO::NumberOfStations; ++i) {
      line_flag_vector::iterator iter = _stubs.at(i).begin();
      for(;iter!=_stubs.at(i).end();++iter){
        
        TLine* line = new TLine(iter->first.first.second,iter->first.first.first,
          iter->first.second.second,iter->first.second.first);
        if(iter->second == -1) {
          line->SetLineWidth(2);	
          line->SetLineColor(station_color(i));	
        } else {
          line->SetLineStyle(2);	
          line->SetLineColor(iter->second);	
        }
        line->Draw();      
        _line_vector.push_back(line);
      }
    }
  }
}

//__________________________________________________________
void TMutPlaneView::clear_temp_storage()
{
  //    std::for_each(_marker_vector.begin(),_marker_vector.end(),PVDeleteMarker());
  //    std::for_each(_line_vector.begin(),_line_vector.end(),PVDeleteLine());
}

//__________________________________________________________
void TMutPlaneView::get_strip_coord(TMutHitMap::const_pointer hit_ptr)
{
  
  unsigned short arm = hit_ptr->get()->get_arm();
  unsigned short station = hit_ptr->get()->get_station();
  unsigned short octant = hit_ptr->get()->get_octant();
  unsigned short half_octant = hit_ptr->get()->get_half_octant();
  unsigned short gap = hit_ptr->get()->get_gap();
  unsigned short cathode = hit_ptr->get()->get_cathode();
  unsigned short strip = hit_ptr->get()->get_strip();
  
  MutArm* geometry = (arm == MUTOO::South) ? SouthArm() : NorthArm();
  
  // Determine local w space offset for polymarkers.  We are drawing
  // things in a coordinate system that is defined by the tangent to
  // the half-octant at the outer radius.  We plot the distance from
  // the endpoint of the first strip in the half octant plus an offset
  // chosed to seperate half octants in our funny psuedo-radial view.
  //  
  unsigned short plane = (cathode == 0) ? MUTOO::Cathode1 : MUTOO::Cathode2;
  
  PHPoint strip_point = geometry->
    f_pMutStations[station]->
    f_pMutOctants[octant]->
    f_pMutHalfOctants[half_octant]->
    f_pMutGaps[gap]->
    f_pMutPlanes[plane]->
    f_pMutStrips[strip]->getGlobalPositionEnd();
  
  coord_pair strip_coord = transform_zw(station,strip_point);
  // append to coordinates vector
  //
  _hit_coord.push_back(strip_coord);
}

//__________________________________________________________
void TMutPlaneView::get_cluster_coord(TMutClusMap::const_pointer clus_ptr)
{
  
  unsigned short arm = clus_ptr->get()->get_arm();
  unsigned short station = clus_ptr->get()->get_station();
  unsigned short octant = clus_ptr->get()->get_octant();
  unsigned short half_octant = clus_ptr->get()->get_half_octant();
  unsigned short gap = clus_ptr->get()->get_gap();
  unsigned short cathode = clus_ptr->get()->get_cathode();
  unsigned short strip = 0;
  
  for( size_t index = 0; index < clus_ptr->get()->get_n_centroid(); index++ )
  {
    const TMutClusCentroid *centroid( clus_ptr->get()->get_centroid( index ) );
    if( !centroid ) continue;
    
    strip = centroid->get_peak_strip();
    const MutStrip* strip_geo = TMutGeo::get_strip_geom(arm,
      station,
      octant,
      half_octant,
      gap,
      cathode,
      strip);
    
    PHPoint strip_point = strip_geo->getGlobalPositionEnd();
    // peak strip positions
    //
    coord_pair strip_coord = transform_zw(station,strip_point);
    _peak_coord.push_back(strip_coord);
    // we now draw TMutCoord instead of offset from centroid
    //
    //      // peak strip position + offset from cluster fit
    //      //
    //      strip_coord.second += offset;
    //      _offset_coord.push_back(strip_coord);
  }
}

//__________________________________________________________
void TMutPlaneView::get_gap_coord()
{
  TMutGapCoordMap::const_iterator coord_iter = _gap_coord_map->range();
  while(TMutGapCoordMap::const_pointer coord_ptr = coord_iter.next()){
    unsigned short station = coord_ptr->get()->get_station();
    // if hit is not in this view then continue;
    //
    if(coord_ptr->get()->get_arm() != _arm ||
      coord_ptr->get()->get_octant() != _octant) continue;
    
    // push the gap coordinate onto list to be drawn
    //
    PHPoint point = coord_ptr->get()->get_coord();
    coord_pair gap_coord = transform_zw(station,point);
    _gap_coord.push_back(gap_coord);
  }
}

//__________________________________________________________
void TMutPlaneView::get_mc_hit()
{
  TMutMCHitMap::const_iterator hit_iter = _mc_hit_map->range();
  while(TMutMCHitMap::const_pointer hit_ptr = hit_iter.next()){
    unsigned short station = hit_ptr->get()->get_station();
    // if hit is not in this view then continue;
    //
    if(hit_ptr->get()->get_arm() != _arm ||
      hit_ptr->get()->get_octant() != _octant) continue;
    
    // push the monte-carlo hit onto list to be drawn
    //
    PHPoint point(hit_ptr->get()->get_x(),
      hit_ptr->get()->get_y(),
      hit_ptr->get()->get_z());		  ;
    
    coord_pair mc_hit = transform_zw(station,point);
    _mc_hit.push_back(mc_hit);
  }
}

//__________________________________________________________
void TMutPlaneView::get_coord()
{
  TMutCoordMap::const_iterator coord_iter = _coord_map->range();
  while(TMutCoordMap::const_pointer coord_ptr = coord_iter.next()){
    unsigned short station = coord_ptr->get()->get_station();
    // if hit is not in this view then continue;
    //
    if(coord_ptr->get()->get_arm() != _arm ||
      coord_ptr->get()->get_octant() != _octant) continue;    
    
    // push the gap coordinate onto list to be drawn
    //
    PHPoint point = coord_ptr->get()->get_coord_end();
    coord_pair coord = transform_zw(station,point);
    _coord.push_back(coord);
  }
}

//__________________________________________________________
unsigned short TMutPlaneView::station_color(unsigned short i_station) const
{
  static const boost::array<unsigned short,3> colors = {{2,6,4}};
  // at is bounds checked
  //
  return colors.at(i_station);
}

//__________________________________________________________
unsigned short TMutPlaneView::nearest_station(double z) const
{
  double min_delta = 1e37;
  unsigned short station=0;
  for(size_t i=0; i<_frames.size(); ++i){
    double delta = std::fabs(_frames.at(i)[0].first - z);
    if(delta < min_delta){
      station = i;
      min_delta = delta;
    }
  }
  return station;
}
