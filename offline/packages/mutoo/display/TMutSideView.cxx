// $Id: TMutSideView.cxx,v 1.10 2014/01/26 18:05:32 bbannier Exp $


/*!
   \file TMutSideView.cxx
   \brief mutoo side view display
   \author Sean Kelly
   \version $Revision: 1.10 $
   \date $Date: 2014/01/26 18:05:32 $
*/

// MUTOO/PHENIX
#include<TMutSideView.h>
#include<TMutGeo.h>
#include<MUTOO.h>
#include<PHException.h>
#include<TMutNode.h>
#include<TMutCoordMap.h>
#include<TMutGapCoordMap.h>
#include<TMutMCHitMap.h>
#include<TMutStubMap.h>
#include<TMutTrkMap.h>
#include<TMutTrackUtil.h>
#include<PHTrackIntegratorKF.h>

// STL/BOOST/ROOT
//
#include<boost/array.hpp>
#include<algorithm>
#include<TPolyLine.h>

using namespace std;

//_________________________________________________________________________________________________
TMutSideView::TMutSideView(unsigned short arm, unsigned short octant, int station, int event) : 
  _canvas(0),
  _arm(arm) ,
  _octant(octant),
  _station(station),
  _event(event),
  _gap_coord_map(0),
  _stub_map(0),
  _trk_map(0),
  _mc_hit_map(0)
  
{  
  setup_canvas();
}

//_________________________________________________________________________________________________
Bool_t TMutSideView::event(PHCompositeNode* top_node)
{
  clear_temp_storage();
  
  try {
    
    // TMutGapCoord IOC
    _gap_coord_map = TMutNode<TMutGapCoordMap>::find_node(top_node,"TMutGapCoordMap");

    // TMutStub IOC
    _stub_map = TMutNode<TMutStubMap>::find_node(top_node,"TMutStubMap");
    
    // TMutTrk IOC
    _trk_map = TMutNode<TMutTrkMap>::find_node(top_node,"TMutTrkMap");
    
  } catch(exception& e){
    MUTOO::TRACE(DESCRIPTION(e.what()));
  }

  // Check for MC
  try {
    _mc_hit_map = TMutNode<TMutMCHitMap>::find_node(top_node,"TMutMCHitMap");
  } catch (exception& e) {
    MUTOO::TRACE(DESCRIPTION(e.what()));
  }
  
  return true;
}


//_________________________________________________________________________________________________
/*! Paint the front view of the octants in a given station */
void TMutSideView::paint_stations()
{

  MutArm* _geometry = (_arm == MUTOO::South) ? SouthArm() : NorthArm();
  
  for(int i=0;i<MUTOO::NumberOfStations;++i)
  {    

    // radial coordinates of octant boundaries from geometry object
    //
    unsigned short last_gap = TMutGeo::get_n_gaps(_arm,i) - 1;
    double r_1 = _geometry->f_pMutStations[i]->f_pMutOctants[_octant]->getInnerRadius();
    double r_2 = _geometry->f_pMutStations[i]->f_pMutOctants[_octant]->getOuterRadius();
    double z_1 = TMutGeo::get_anode_plane_position(_arm,i,_octant,0,0).getZ();
    double z_2 = TMutGeo::get_anode_plane_position(_arm,i,_octant,0,last_gap).getZ();

    double zmin = min(z_1,z_2) - 2;
    double zmax = max(z_1,z_2) + 2;

    // Lines for a station in the rz view
    //
    _frames.push_back(new TLine(zmin,r_1,zmax,r_1));  
    _frames.back()->SetLineColor(station_color(i));
    _frames.push_back(new TLine(zmin,r_1,zmin,r_2));  
    _frames.back()->SetLineColor(station_color(i));
    _frames.push_back(new TLine(zmin,r_2,zmax,r_2)); 
    _frames.back()->SetLineColor(station_color(i));
    _frames.push_back(new TLine(zmax,r_2,zmax,r_1));  
    _frames.back()->SetLineColor(station_color(i));
  }

  line_vector::iterator iter = _frames.begin();
  for(;iter!=_frames.end();++iter){
    (*iter)->Draw();
  }
  
}

//___________________________________________________________________________
/*! Paint the front view of the octants in a given station */
void TMutSideView::paint_stubs()
{
  
  // Iterate over all tracks
  TMutStubMap::const_iterator stub_iter = _stub_map->get(_arm);
  while(TMutStubMap::const_pointer stub_ptr = stub_iter.next()){

    if(stub_ptr->get()->get_arm() != _arm || stub_ptr->get()->get_octant() != _octant) continue;
    
    const TMutFitPar* fit_par = stub_ptr->get()->get_fit_par();

    // Indicative of successfull stub fit
    if(stub_ptr->get()->get_fit_par()->get_z_begin() == 
       stub_ptr->get()->get_fit_par()->get_z_end() ) continue;

    // Stub endpoints
    double delta = (_arm==MUTOO::South) ? 5 : -5;
    double z_begin = stub_ptr->get()->get_fit_par()->get_z_begin() - delta;
    double z_end = stub_ptr->get()->get_fit_par()->get_z_end() + delta;

    PHPoint stub_begin = TMutTrackUtil::linear_track_model(fit_par, z_begin);    
    PHPoint stub_end = TMutTrackUtil::linear_track_model(fit_par, z_end);        
    
    double r_begin = sqrt(MUTOO::SQUARE(stub_begin.getX()) + MUTOO::SQUARE(stub_begin.getY()));
    double r_end = sqrt(MUTOO::SQUARE(stub_end.getX()) + MUTOO::SQUARE(stub_end.getY()));

    TLine* line = new TLine(z_begin,r_begin,z_end,r_end);
    line->SetLineWidth(2);
    line->SetLineColor(station_color(stub_ptr->get()->get_station()));
    line->Draw();
    _trks.push_back(line);
  }
  
}

//_________________________________________________________________________________________________
void TMutSideView::paint()
{
  _pad0->cd();
  _label->Draw();  
  _pad1->cd();

  try {
    
    for(int i=0;i<MUTOO::NumberOfStations;++i) paint_stations();

    paint_gap_coord();
    paint_stubs();
    paint_trks();
  } catch(exception& e) {
    MUTOO::TRACE(e.what());
  }
}

//________________________________________________________________________
/*! Hash station specification into a reasonable color */
unsigned short TMutSideView::station_color(unsigned short i_station) const
{
  static const boost::array<unsigned short,3> colors = {{2,6,4}};

  // at is bounds checked
  return colors.at(i_station);
  
}

//________________________________________________________________________
void TMutSideView::get_gap_coord()
{
  TMutGapCoordMap::const_iterator coord_iter = _gap_coord_map->range();
  while(TMutGapCoordMap::const_pointer coord_ptr = coord_iter.next()){

    if(coord_ptr->get()->get_arm() != _arm) continue;
    if(coord_ptr->get()->get_octant() != _octant) continue;
    
    double x = coord_ptr->get()->get_coord().getX();
    double y = coord_ptr->get()->get_coord().getY();
    double z = coord_ptr->get()->get_coord().getZ();
    double r = sqrt(MUTOO::SQUARE(x) + MUTOO::SQUARE(y));
    
    TMarker* marker = new TMarker(z,r,4);
    marker->SetMarkerColor(50);
    marker->SetMarkerSize(0.4);
    _gap_coord.push_back(marker);
  }
}
 
//_________________________________________________________________________________________________
void TMutSideView::paint_gap_coord() 
{
  if(_gap_coord.size()==0) get_gap_coord();
  
  marker_vector::iterator iter = _gap_coord.begin();
  for(;iter!=_gap_coord.end();++iter) (*iter)->Draw();
  
}

//_________________________________________________________________________________________________
void TMutSideView::clear_temp_storage()
{
  
  // delete root objects
  for_each(_trks.begin(),_trks.end(),OVDeleteLine());
  for_each(_gap_coord.begin(),_gap_coord.end(),OVDeleteMarker());
  for_each(_mc_hit.begin(),_mc_hit.end(),OVDeleteMarker());
  
  // clear lists
  _trks.clear();
  _gap_coord.clear();
  _mc_hit.clear();
  
}

//_________________________________________________________________________________________________
void TMutSideView::setup_canvas()
{

  string arm_str;
  if(_arm == MUTOO::South) arm_str = string("South");
  else arm_str = string("North");
  
  // setup canvas 
  ostringstream tmp;
  tmp << "mutoo_" << _arm << endl;
  string name = tmp.str();

  ostringstream tmp2;
  tmp2 << "MUTR Side View, Arm " << arm_str.c_str() 
       << "Octant " << _octant << "  Event " << _event;

  string name2 = tmp2.str();

  _canvas = new TCanvas(name.c_str(),name2.c_str(),600,600);
  _canvas->SetFillColor(0);
  _canvas->Draw();

  _pad0 = new TPad("pad0","pad0",0.00, 0.9, 1.0, 1.0, 0);
  _pad0->SetBorderMode(0);
  _pad0->HighLight(0,0);
  _pad0->Range(0,0,1,1);  

  _label = new TPaveLabel(0.0,0.0,1,1,name2.c_str());
  _label->SetFillColor(0);
  _label->SetBorderSize(0);
  _label->SetTextFont(42);

  _pad1 = new TPad("pad2","pad2",0.1, 0.1, 0.90, 0.90,0);
  _pad1->SetBorderMode(0);
  _pad1->HighLight(0,0);

  vector<double> zvec;
  vector<double> rvec;

  // -1 indicates we are drawing all 3 stations
  //
  int first=0;
  int last=0;
  if(_station == -1 ) {
     first=0;
     last=MUTOO::NumberOfStations;
  } else {
    first=_station;
    last=_station+1;
  }

  MutArm* _geometry = (_arm == MUTOO::South) ? SouthArm() : NorthArm();
  
  // Figure out canvas range  
  for(int i=first;i<last;++i)
  {              
    
    unsigned short last_gap = TMutGeo::get_n_gaps(_arm,i) - 1;
    
    double r_1 = _geometry->f_pMutStations[i]->f_pMutOctants[_octant]->getInnerRadius();
    double r_2 = _geometry->f_pMutStations[i]->f_pMutOctants[_octant]->getOuterRadius();

    double z_1 = TMutGeo::get_anode_plane_position(_arm,i,_octant,0,0).getZ();
    double z_2 = TMutGeo::get_anode_plane_position(_arm,i,_octant,0,last_gap).getZ();    
    
    zvec.push_back(z_1);    
    zvec.push_back(z_2);
    rvec.push_back(r_1);
    rvec.push_back(r_2);      
    
  }

  double zmin = (*min_element(zvec.begin(),zvec.end())) - 5;
  double zmax = (*max_element(zvec.begin(),zvec.end())) + 5;
  double rmin = (*min_element(rvec.begin(),rvec.end())) - 5;
  double rmax = (*max_element(rvec.begin(),rvec.end())) + 5;
  
  _pad1->Range(zmin,rmin,zmax,rmax);  
  _pad0->Draw();
  _pad1->Draw();

}


void 
TMutSideView::paint_mc_hit()
{
  // if coordinate vector is empty the fill it
  //
  if(_mc_hit.size()==0){
    get_mc_hit();
  }  
  marker_vector::iterator iter = _mc_hit.begin();
  for(;iter!=_mc_hit.end();++iter){
    (*iter)->Draw();
  }
}

void 
TMutSideView::get_mc_hit()
{
  if(!_mc_hit_map) return;
  TMutMCHitMap::const_iterator hit_iter = _mc_hit_map->range();
  while(TMutMCHitMap::const_pointer hit_ptr = hit_iter.next()){

    // if hit is not in this view then continue;
    if(hit_ptr->get()->get_arm() != _arm ||
       hit_ptr->get()->get_octant() != _octant) continue;


    double x = hit_ptr->get()->get_x();
    double y = hit_ptr->get()->get_y();
    double z = hit_ptr->get()->get_z();
    double r = sqrt(MUTOO::SQUARE(x) + MUTOO::SQUARE(y));

    TMarker* marker = new TMarker(z,r,5);
    marker->SetMarkerColor(1);
    marker->SetMarkerSize(0.4);
    _mc_hit.push_back(marker);
  }
}

//____________________________________________________________________________
void TMutSideView::paint_trks()
{
  
  // if coordinate vector is empty the fill it
  if(!_tracks.size()) get_trks();
  
  // iterate over coordinate vector and draw mark 
  // for each strip center
  vector<coord_vector>::const_iterator track_iter = _tracks.begin();
  // Loop over tracks
  for(;track_iter != _tracks.end();++track_iter) 
  {
    coord_vector::const_iterator point_iter = track_iter->begin();
    
    // Instantiate a poly line for current track
    TPolyLine* poly = new TPolyLine(track_iter->size());   
    
    // Loop over points in track
    int i = 0;
    for(;point_iter!=track_iter->end();++point_iter)
    { poly->SetPoint(i++,point_iter->second,point_iter->first); }
    
    poly->SetLineColor(1);
    poly->SetLineWidth(2);
    poly->Draw();
  }
}

//____________________________________________________________________________
void TMutSideView::get_trks()
{
  if(!_trk_map){
    MUTOO::TRACE("TMutSideView::get_trk: No track map");
    return;
  }  

  TMutTrkMap::const_iterator trk_iter = _trk_map->range();
  while(TMutTrkMap::const_pointer trk_ptr = trk_iter.next()){    
    
    // Require reco success and that track is in given octant
    //
    if(!trk_ptr->get()->get_reco_success() || 
       trk_ptr->get()->get_arm() != _arm || 
       trk_ptr->get()->get_octant() != _octant) continue;    
    
    // get track parameters
    TMutTrkPar trk_par_orig( *trk_ptr->get()->get_trk_par() );
    
    // Determine the endpoints for drawing the  track
    double delta = 30.0;
    double z_us = (_arm == MUTOO::South) ? -188 + delta : 100 - delta;
    double z_ds = (_arm == MUTOO::South) ? -469 - delta : 700 + delta;
    
    double sgn = (z_ds-z_us)/fabs(z_ds-z_us);
    double z = z_us;

    // Local storage for this tracks points
    //
    coord_vector single_track;
    while(fabs(z) < fabs(z_ds))
    {
      
      // Take 2 cm steps in the dowstream direction
      z += sgn*2;

      PHTrackIntegratorKF integrator;
      integrator.initialize( trk_par_orig );
      integrator.extrapolate(z);
      
      if( integrator.get_error() )
      {
        cout << "TMutSideView::get_trk - extrapolation to z=" << z << " failed" << endl;
        continue;
      }

      // Figure out which station is closest to extrapolate point
      // and push the w,z pair onto the _mc_trk list after doing
      // the transform to wz space
      TMutTrkPar trk_par_out;
      integrator.finish( trk_par_out );
      
      double x = trk_par_out.get_x();
      double y = trk_par_out.get_y();
      double z = trk_par_out.get_z();
      double r = sqrt(MUTOO::SQUARE(x) + MUTOO::SQUARE(y));      
      single_track.push_back(make_pair(r,z));
    }
    
    // Push track points onto vector of tracks
    _tracks.push_back(single_track);
    
  }
}
