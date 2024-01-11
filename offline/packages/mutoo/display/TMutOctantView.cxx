// $Id: TMutOctantView.cxx,v 1.13 2011/12/24 04:48:22 slash Exp $

#include<TMutOctantView.h>
#include<MUTOO.h>
#include<MutGeom.h>
#include<PHException.h>
#include<boost/array.hpp>
#include<TMutNode.h>
#include<TMutCoordMap.h>
#include<TMutGapCoordMap.h>
#include<TMutStubMap.h>
#include<TMutTrkMap.h>
#include<TMutTrackUtil.h>
#include <MutStrip.h>

using namespace std;

TMutOctantView::TMutOctantView(unsigned short arm, int event, int octant, int station) : 
  _arm(arm) , 
  _event(event), 
  _octant(octant),
  _station(station),
  _canvas(0), 
  _draw_stubs(false)
{
  setup_canvas();
}

Bool_t 
TMutOctantView::event(PHCompositeNode* top_node)
{
  clear_temp_storage();
  try {
    _coord_map = TMutNode<TMutCoordMap>::find_node(top_node,"TMutCoordMap");
    // TMutGapCoord IOC
    //
    _gap_coord_map = TMutNode<TMutGapCoordMap>::find_node(top_node,"TMutGapCoordMap");
    // TMutStub IOC
    //
    _stub_map = TMutNode<TMutStubMap>::find_node(top_node,"TMutStubMap");
    // TMutTrk IOC
    //
    _trk_map = TMutNode<TMutTrkMap>::find_node(top_node,"TMutTrkMap");
  } catch(exception& e){
    MUTOO::TRACE(DESCRIPTION(e.what()));
  }
  return true;
}


/*! Paint the front view of the octants in a given station */
void TMutOctantView::paint_octants(unsigned short i_station)
{
  if(i_station > MUTOO::NumberOfStations){
    throw invalid_argument(DESCRIPTION("paint_front_octant_view: bad station specification"));
  }  
  MutArm* _geometry = (_arm == MUTOO::South) ? SouthArm() : NorthArm();
  line_vector lines;
  
  for(int i=0;i<MUTOO::NumberOfOctants;++i){    
    // radial coordinates of octant boundaries from geometry object
    //
    double r_1 = _geometry->f_pMutStations[i_station]->f_pMutOctants[i]->getInnerRadius();
    double r_2 = _geometry->f_pMutStations[i_station]->f_pMutOctants[i]->getOuterRadius();
    double phi_1 = _geometry->f_pMutStations[i_station]->f_pMutOctants[i]->getBeginPhi();
    double phi_2 = _geometry->f_pMutStations[i_station]->f_pMutOctants[i]->getEndPhi();
    
    // convert to rectangular coordinates
    //
    double x_1 = r_1*cos(phi_1);
    double y_1 = r_1*sin(phi_1);
    double x_2 = r_2*cos(phi_1);
    double y_2 = r_2*sin(phi_1);
    double x_3 = r_2*cos(phi_2);
    double y_3 = r_2*sin(phi_2);
    double x_4 = r_1*cos(phi_2);
    double y_4 = r_1*sin(phi_2);
    lines.push_back(new TLine(x_1,y_1,x_2,y_2));  
    lines.push_back(new TLine(x_2,y_2,x_3,y_3));  
    lines.push_back(new TLine(x_3,y_3,x_4,y_4));  
    lines.push_back(new TLine(x_4,y_4,x_1,y_1));  
  }
  line_vector::iterator iter = lines.begin();
  for(;iter!=lines.end();++iter){
    (*iter)->SetLineColor(station_color(i_station));
    (*iter)->Draw();
  }
}

/*! Paint the front view of the octants in a given station */
void 
TMutOctantView::paint_trks()
{
  // Iterate over all tracks
  //
  //    TMutTrkMap::const_iterator trk_iter = _trk_map->range();
  //    while(TMutTrkMap::const_pointer trk_ptr = trk_iter.next()){
  
  //      const TMutFitPar* fit_par = trk_ptr->get()->get_fit_par();
  //      // get the endpoint by extrapolating to z = +- 500
  //      //
  //      double z_end = -500;
  //      PHPoint trk_end = TMutTrackUtil::linear_track_model(fit_par,z_end);  
  
  //      // linearly extrapolate to z=0 
  //      //
  //      PHPoint trk_begin = TMutTrackUtil::linear_track_model(fit_par,0);    
  
  //      // Projection onto XY plane
  //      //
  //      TLine* line = new TLine(trk_begin.getX(),trk_begin.getY(),trk_end.getX(),trk_end.getY());
  //      line->SetLineWidth(2);
  //      line->SetLineColor(1);
  //      line->Draw();
  //    _trks.push_back(line);
  //  }
}

/*! Paint the front view of the octants in a given station */
void 
TMutOctantView::paint_stubs()
{
  // Iterate over all tracks
  //
  TMutStubMap::const_iterator stub_iter = _stub_map->get(_arm);
  while(TMutStubMap::const_pointer stub_ptr = stub_iter.next()){

    if(stub_ptr->get()->get_arm() != _arm) continue;
    
    const TMutFitPar* fit_par = stub_ptr->get()->get_fit_par();

    // Indicative of successfull stub fit
    //
    if(stub_ptr->get()->get_fit_par()->get_z_begin() == 
       stub_ptr->get()->get_fit_par()->get_z_end() ) continue;

    // Stub endpoints
    //
    PHPoint stub_begin = TMutTrackUtil::linear_track_model(fit_par,stub_ptr->get()->get_fit_par()->get_z_begin());    
    PHPoint stub_end = TMutTrackUtil::linear_track_model(fit_par,stub_ptr->get()->get_fit_par()->get_z_end());        
    
    // Projection onto XY plane
    //
    TLine* line = new TLine(stub_begin.getX(),stub_begin.getY(),stub_end.getX(),stub_end.getY());
    line->SetLineWidth(2);
    line->SetLineColor(4);
    line->Draw();
    _trks.push_back(line);
  }
}

void
TMutOctantView::paint()
{
  _pad0->cd();
  _label->Draw();  
  _pad1->cd();

  try {
    for(int i=0;i<MUTOO::NumberOfStations;++i){
      if(_station!=-1 && i!=_station) continue;
      paint_octants(i);
    }  
    paint_coord();
    paint_gap_coord();
    if(_draw_stubs) paint_stubs();
    paint_trks();
  } catch(exception& e) {
    MUTOO::TRACE(e.what());
  }
}
/*! Hash station specification into a reasonable color */
unsigned short 
TMutOctantView::station_color(unsigned short i_station) const
{
  static const boost::array<unsigned short,3> colors = {{2,6,4}};
  // at is bounds checked
  //
  return colors.at(i_station);
}

void 
TMutOctantView::get_coord()
{
  TMutCoordMap::const_iterator coord_iter = _coord_map->range();
  while(TMutCoordMap::const_pointer coord_ptr = coord_iter.next()){
    if(coord_ptr->get()->get_arm() != _arm) continue;
    if(_station != -1 && coord_ptr->get()->get_station() != _station) continue;
    double x1 = coord_ptr->get()->get_coord_begin().getX();
    double y1 = coord_ptr->get()->get_coord_begin().getY();
    double x2 = coord_ptr->get()->get_coord_end().getX();
    double y2 = coord_ptr->get()->get_coord_end().getY();
    TLine* line = new TLine(x1,y1,x2,y2);
    line->SetLineColor(station_color(coord_ptr->get()->get_station()));
    _coord.push_back(line);
  }
}

void 
TMutOctantView::get_gap_coord()
{
  TMutGapCoordMap::const_iterator coord_iter = _gap_coord_map->range();
  while(TMutGapCoordMap::const_pointer coord_ptr = coord_iter.next()){
    if(coord_ptr->get()->get_arm() != _arm) continue;
    if(_station != -1 && coord_ptr->get()->get_station() != _station) continue;
    double x1 = coord_ptr->get()->get_coord().getX();
    double y1 = coord_ptr->get()->get_coord().getY();
    TMarker* marker = new TMarker(x1,y1,5);
    marker->SetMarkerColor(1);
    marker->SetMarkerSize(1);
    _gap_coord.push_back(marker);
  }
}
 
void 
TMutOctantView::paint_coord() 
{
  if(_coord.size()==0){
    get_coord();
  }
  line_vector::iterator iter = _coord.begin();
  for(;iter!=_coord.end();++iter){
    (*iter)->Draw();
  }
}

void 
TMutOctantView::paint_gap_coord() 
{
  if(_gap_coord.size()==0){
    get_gap_coord();
  }
  marker_vector::iterator iter = _gap_coord.begin();
  for(;iter!=_gap_coord.end();++iter){
    (*iter)->Draw();
  }
}

void 
TMutOctantView::clear_temp_storage()
{
  for_each(_coord.begin(),_coord.end(),OVDeleteLine());
  for_each(_trks.begin(),_trks.end(),OVDeleteLine());
  for_each(_gap_coord.begin(),_gap_coord.end(),OVDeleteMarker());
  _coord.clear();
  _trks.clear();
  _gap_coord.clear();
}

void
TMutOctantView::setup_canvas()
{
  string arm_str;
  if(_arm == MUTOO::South) {
    arm_str = string("South");
  } else {
    arm_str = string("North");
  }
  
  // setup canvas 
  //
  ostringstream tmp;
  tmp << "mutoo_" << _arm;
  string name = tmp.str();

  ostringstream tmp2;
  tmp2 << "MUTR Octant View, Arm " << arm_str.c_str() << ", Event " << _event;
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

  if(_arm == MUTOO::South && _octant == -1) {
    _pad1->Range(-310,-310,310,310);
  } else if(_arm == MUTOO::North && _octant == -1){
    _pad1->Range(-420,-420,420,420);
  } else if(_station == -1){

    MutArm* geometry = (_arm == MUTOO::South) ? SouthArm() : NorthArm();
    double r_1 = geometry->f_pMutStations[0]->f_pMutOctants[_octant]->getInnerRadius();
    double r_2 = geometry->f_pMutStations[2]->f_pMutOctants[_octant]->getOuterRadius();
    double phi_1 = geometry->f_pMutStations[2]->f_pMutOctants[_octant]->getBeginPhi();
    double phi_2 = geometry->f_pMutStations[2]->f_pMutOctants[_octant]->getEndPhi();
    
    // convert to rectangular coordinates
    //
    vector<double> xvec;
    vector<double> yvec;

    xvec.push_back(r_1*cos(phi_1));
    yvec.push_back(r_1*sin(phi_1));

    xvec.push_back(r_2*cos(phi_1));    
    yvec.push_back(r_2*sin(phi_1));

    xvec.push_back(r_2*cos(phi_2));
    yvec.push_back(r_2*sin(phi_2));

    xvec.push_back(r_1*cos(phi_2));
    yvec.push_back(r_1*sin(phi_2));

    double xmin = (*min_element(xvec.begin(),xvec.end())) - 10;
    double xmax = (*max_element(xvec.begin(),xvec.end())) + 10;
    double ymin = (*min_element(yvec.begin(),yvec.end())) - 10;
    double ymax = (*max_element(yvec.begin(),yvec.end())) + 10;

    _pad1->Range(xmin,ymin,xmax,ymax);

  } else {

    MutArm* geometry = (_arm == MUTOO::South) ? SouthArm() : NorthArm();
    double r_1 = geometry->f_pMutStations[_station]->f_pMutOctants[_octant]->getInnerRadius();
    double r_2 = geometry->f_pMutStations[_station]->f_pMutOctants[_octant]->getOuterRadius();
    double phi_1 = geometry->f_pMutStations[_station]->f_pMutOctants[_octant]->getBeginPhi();
    double phi_2 = geometry->f_pMutStations[_station]->f_pMutOctants[_octant]->getEndPhi();
    
    // convert to rectangular coordinates
    //
    vector<double> xvec;
    vector<double> yvec;

    xvec.push_back(r_1*cos(phi_1));
    yvec.push_back(r_1*sin(phi_1));

    xvec.push_back(r_2*cos(phi_1));    
    yvec.push_back(r_2*sin(phi_1));

    xvec.push_back(r_2*cos(phi_2));
    yvec.push_back(r_2*sin(phi_2));

    xvec.push_back(r_1*cos(phi_2));
    yvec.push_back(r_1*sin(phi_2));

    double xmin = (*min_element(xvec.begin(),xvec.end())) - 10;
    double xmax = (*max_element(xvec.begin(),xvec.end())) + 10;
    double ymin = (*min_element(yvec.begin(),yvec.end())) - 10;
    double ymax = (*max_element(yvec.begin(),yvec.end())) + 10;

    _pad1->Range(xmin,ymin,xmax,ymax);

  }
  _pad0->Draw();
  _pad1->Draw();
}

