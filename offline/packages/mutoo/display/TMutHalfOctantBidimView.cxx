// $Id: TMutHalfOctantBidimView.cxx,v 1.4 2011/12/24 04:48:22 slash Exp $

#include<TMutHalfOctantBidimView.h>
#include<MutGeom.h>
#include<MutStrip.h>
#include<MUTOO.h>

#include<TLine.h>
#include<TPolyLine.h>
#include<TPave.h>
#include<TH2S.h>
#include<TMarker.h>
#include<TMutNode.h>
#include<TMutGeo.h>
#include<PHTimer.h>
#include<TStyle.h>

#include<boost/array.hpp>
#include<cmath>
#include<algorithm>

//__________________________________________________________________________
TMutHalfOctantBidimView::TMutHalfOctantBidimView(unsigned short arm, 
  unsigned short octant,
  unsigned short halfoctant,
  unsigned short station,
  int event) :
  _geometry(0),
  _arm(arm),
  _octant(octant),
  _halfoctant(halfoctant),
  _station(station),
  _event(event),
  _canvas(0),
  _pad0(0),
  _pad1(0),
  _pad2(0),
  _pad3(0),
  _bidim0(0),
  _bidim1(0),
  _bidim2(0),
  _is_monte_carlo(true)
{
  _geometry = (_arm == MUTOO::South) ? SouthArm() : NorthArm();
  setup_canvas();
}

//__________________________________________________________________________
TMutHalfOctantBidimView::~TMutHalfOctantBidimView()
{
  _canvas->Close();
  if (_bidim0) delete(_bidim0);
  if (_bidim1) delete(_bidim1);
  if (_bidim2) delete(_bidim2);
}

//__________________________________________________________________________
Bool_t TMutHalfOctantBidimView::event(PHCompositeNode* top_node)
{

  try {
    // TMutHit IOC
    //
    _hit_map = TMutNode<TMutHitMap>::find_node(top_node,"TMutHitMap");
  } catch(std::exception& e){
    MUTOO::TRACE(DESCRIPTION(e.what()));
  }
  
  // If not running on simulated data then set flag
  //
  try {
    _mc_hit_map = TMutNode<TMutMCHitMap>::find_node(top_node,"TMutMCHitMap");
    _mc_trk_map = TMutNode<TMutMCTrkMap>::find_node(top_node,"TMutMCTrkMap");
  } catch(std::exception) {
    _is_monte_carlo = false;
  }
  return true;
}

//__________________________________________________________________________
void TMutHalfOctantBidimView::setup_canvas()
{
  // setup canvas and some pads
  //
  std::ostringstream tmpName;
  tmpName << "mutoo_HalfOctantBidim" << std::endl;
  std::string cName = tmpName.str();
  
  std::ostringstream tmpTitle;
  tmpTitle << "MUTR  HalfOctantBidimView: Arm:" << _arm
     << " Octant:" << _octant
     << " HalfOctant:" << _halfoctant
     << " Station:" << _station
     << " Event:" << _event << std::endl;
  std::string cTitle = tmpTitle.str();

  _canvas = new TCanvas(cName.c_str(), cTitle.c_str(), 1200, 700);
  _canvas->SetFillColor(0);

  _pad0 = new TPad("pad0", "pad0", 0.00, 0.90, 1.0, 1.0, 0);
  
  _pad0->SetBorderMode(0);
  _pad0->HighLight(0, 0);
  _pad0->Range(0, 0, 1, 1);  
  _label = new TPaveLabel(0.0, 0.0, 1, 1, cTitle.c_str());
  _label->SetFillColor(0);
  _label->SetBorderSize(0);
  _label->SetTextFont(42);

  _pad1 = new TPad("pad1", "pad1", 0.00, 0.00, 0.3, 0.9, 0);
  _pad1->SetBorderMode(0);

  _pad2 = new TPad("pad2", "pad2", 0.35, 0.00, 0.65, 0.9, 0);
  _pad2->SetBorderMode(0);

  _pad3 = new TPad("pad2", "pad3", 0.70, 0.00, 1.0, 0.9, 0);
  _pad2->SetBorderMode(0);

  _pad0->Draw();
  _pad1->Draw();
  _pad2->Draw();
  _pad3->Draw();
 
  return;
}

//__________________________________________________________________________
void TMutHalfOctantBidimView::paint()
{

  // clear the existing primitives from last paint
  //
  clear_temp_storage();
  
  _pad0->cd();
  _label->Draw();  

  // pointer to octant
  MutOctant* octant_ptr = _geometry->f_pMutStations[_station]->f_pMutOctants[_octant];
//   std::cout << "innerRadius: " << octant_ptr->getInnerRadius()
// 	    << " outerRadius: " << octant_ptr->getOuterRadius()
// 	    << " beginPhi: " << octant_ptr->getBeginPhi()
// 	    << " endPhi: " << octant_ptr->getEndPhi()
// 	    << std::endl;

  double innerRadius = octant_ptr->getInnerRadius();
  double outerRadius = octant_ptr->getOuterRadius();
  // pointer to halfoctant
  MutHalfOctant* halfoctant_ptr = octant_ptr->f_pMutHalfOctants[_halfoctant];
//   std::cout << "NumberOfGaps: " << halfoctant_ptr->getNumberOfGaps()
// 	    << std::endl;
//   std::cout << "BodyCenteredFrame: " << std::endl;
//   halfoctant_ptr->getBodyCenteredFrame().print();

  PHFrame bodyCenteredFrame = halfoctant_ptr->getBodyCenteredFrame();

  // bidim definition (rphi,r)
  double rphiExtension = 2.0 * outerRadius * tan(Pi/16.);
  _bidimFrame = bodyCenteredFrame;
  PHVector origin = (_bidimFrame.getV() * innerRadius) -
    (_bidimFrame.getU() * (0.5 * rphiExtension)) ;
  _bidimFrame.setOrigin(origin);
  if (_bidim0) delete(_bidim0);
  if (_bidim1) delete(_bidim1);
  if (_bidim2) delete(_bidim2);
  double rphiBin = 1.0; // to be taken from StripSpacing(plane)????
  int nRphiBins = int(rphiExtension / rphiBin);
  double rphiMax = nRphiBins * rphiBin;
  double rExtension = outerRadius - innerRadius;
  double rBin = rphiBin / tan(Pi/16.);
  int nRBins = int(rExtension / rBin) ;
  double rMax = nRBins * rBin;
  _bidim0 = new TH2S("bidim0", "number of TMutHit's",
        nRphiBins, 0., rphiMax, nRBins, 0., rMax);
  _bidim1 = new TH2S("bidim1", "number of orientations",
        nRphiBins, 0., rphiMax, nRBins, 0., rMax);
  _bidim2 = new TH2F("bidim2", "charge",
        nRphiBins, 0., rphiMax, nRBins, 0., rMax);

  // grid definition with index like bidim
  Grid grid = Grid( 
    _bidim0->GetBin(_bidim0->GetNbinsX() + 1,
    _bidim0->GetNbinsY() + 1) -
    _bidim0->GetBin(0, 0) + 1 );

  // grid filling
  fill_grid( grid );

  // bidim filling
  fill_bidim( grid );

  // bidim drawing
  _pad1->cd();
  _bidim0->SetMaximum(6);
  gStyle->SetPalette(1,0);
  gStyle->SetOptStat(false);
  _bidim0->Draw("COLZ");
  _pad2->cd();
  _bidim1->SetMaximum(4);
  _bidim1->Draw("COLZ");
  _pad3->cd();
  _bidim2->Draw("COLZ");

  // MCHit's ????
  draw_mchits();

}

//__________________________________________________________________________
void TMutHalfOctantBidimView::fill_grid(Grid &grid)
{
  // retrieve/check iterator over TMutHit's in this arm/station/octant
  // [no iterator directly for arm/station/octant/halfoctant]
  TMutHitMap::const_iterator hit_iter = _hit_map->get( _arm, _station, _octant );    
  if( !hit_iter.count() ) return;
  // Loop over all TMutHit's from this arm/station/octant
  hit_iter.reset();
  while( TMutHitMap::const_pointer hit_ptr = hit_iter.next() ) {
    // nothing if not in the right halfoctant
    if (hit_ptr->get()->get_half_octant() != _halfoctant) continue;
    // fill grid with current TMutHit
    fill_grid( grid, hit_ptr );
  }
  return;
}

//__________________________________________________________________________
void TMutHalfOctantBidimView::fill_grid(Grid &grid, TMutHitMap::const_pointer hit_ptr)
{
  // Strip
  MUTOO::cathode_locator location = hit_ptr->get()->get_location();
  MutStrip* strip = TMutGeo::get_strip_geom (location, hit_ptr->get()->get_strip());
  // Begin and end positions in global PHENIX frame
  PHPoint begin_global = strip->getGlobalPositionBegin();
  PHPoint end_global = strip->getGlobalPositionEnd();
  // now with respect to origin of bidim frame
  begin_global = begin_global - _bidimFrame.getOrigin();
  end_global = end_global - _bidimFrame.getOrigin();
  // and with z = 0 (????)
  begin_global.setZ(0.0);
  end_global.setZ(0.0);
  // Begin and end positions in bidim frame
  PHPoint begin_bidim;
  PHPoint end_bidim;
  begin_bidim.setX(begin_global.getX() * _bidimFrame.getU().getX() +
       begin_global.getY() * _bidimFrame.getU().getY());
  begin_bidim.setY(begin_global.getX() * _bidimFrame.getV().getX() +
       begin_global.getY() * _bidimFrame.getV().getY());
  end_bidim.setX(end_global.getX() * _bidimFrame.getU().getX() +
     end_global.getY() * _bidimFrame.getU().getY());
  end_bidim.setY(end_global.getX() * _bidimFrame.getV().getX() +
     end_global.getY() * _bidimFrame.getV().getY());
  // Direction in bidim frame
  PHVector direction = PHVector(end_bidim - begin_bidim);

  // Fill all cells of the grid along the strip
  // Points on strip vector: begin + alpha * (begin - end)
  // with alpha (between 0 and 1)
  // at successive ordinate values equal to bidim ordinate center values
  double ordinate = _bidim0->GetYaxis()->GetBinCenter(1);
  double dordinate = _bidim0->GetYaxis()->GetBinWidth(1);
  double alpha = ( ordinate - begin_bidim.getY() ) / direction.getY();
  double dalpha = dordinate / direction.getY();
  double abscissa = begin_bidim.getX() + alpha * direction.getX();
  double dabscissa = dalpha * direction.getX();
  for (int rBin = 1; rBin <= _bidim0->GetNbinsY(); rBin++) {
    // only between begin and end
    if (alpha >=0.0 && alpha <=1.0) {
      int rphiBin = _bidim0->GetXaxis()->FindBin(abscissa);
      int bidimBin = _bidim0->GetBin(rphiBin, rBin);
      // +1 for number of TMutHit's in the grid cell
      grid[bidimBin].first++;
      // TMutHit pointer inserted into HitSet of grid cell
      // with value 0.0
//       grid[bidimBin].second.make_pair(hit_ptr, 0.0); // would be simpler
      grid[bidimBin].second.insert
  (std::pair<TMutHitMap::const_pointer, double>(hit_ptr, 0.0));
    }
    abscissa = abscissa + dabscissa;
    ordinate = ordinate + dordinate;
    alpha = alpha + dalpha;
  }
  return;
}

//__________________________________________________________________________
void TMutHalfOctantBidimView::fill_bidim(Grid &grid)
{
  // fill bidim from grid
  for (int grid_index = 0; grid_index < int(grid.size()); grid_index++){
    if (!grid[grid_index].first) continue;
    // First bidim: number of TMutHit's
    _bidim0->SetBinContent(grid_index,
         grid[grid_index].first);
    // Second option: number of different orientations of TMutHit's
    _bidim1->SetBinContent(grid_index,
         number_of_orientations(grid, grid_index));
    // Third option: number of different orientations of TMutHit's
    _bidim2->SetBinContent(grid_index,
         charge(grid, grid_index));
  }
  return;
}

//__________________________________________________________________________
int TMutHalfOctantBidimView::number_of_orientations(Grid &grid, int grid_index)
{
  // assume grid_index is valid!!!!
  int nOrientations = 0;
  int maxNOrientations = 4;
  PHVector cathode_direction[4];
  // Loop over TMutHit's of grid cell at grid_index
  HitSet hs = grid[grid_index].second;
  HitSet::iterator hs_iter = hs.begin();
  for ( ; hs_iter != hs.end(); ++hs_iter ) {
    TMutHit* hit_ptr = hs_iter->first->get();
    // cathode location
    MUTOO::cathode_locator
      cathode_location(_arm, _station, _octant, _halfoctant,
           hit_ptr->get_gap(), hit_ptr->get_cathode());
    MutStrip* strip = TMutGeo::get_strip_geom (cathode_location,
                 hit_ptr->get_strip());
    PHPoint strip_begin = strip->getGlobalPositionBegin();
    PHPoint strip_end = strip->getGlobalPositionEnd();
    PHVector strip_direction = strip_end - strip_begin;
    strip_direction.normalize();
    // check new orientation
    bool same_orientation = false;
    for (int orientation = 0; orientation < nOrientations; orientation++) {
      if (strip_direction.dot(cathode_direction[orientation]) > 0.9995) {
  // directions closer than 1.8 degree
  same_orientation = true;
  break;
      }
    }
    if (!same_orientation) {
      // new orientation
      if (nOrientations < maxNOrientations) {
  cathode_direction[nOrientations] = strip_direction;
  nOrientations++;
      }
      else std::cout << "ERROR in TMutHalfOctantBidimView::number_of_orientations - too many different orientations" << std::endl;
    }
  }
  return(nOrientations);
}

//__________________________________________________________________________
float TMutHalfOctantBidimView::charge(Grid &grid, int grid_index)
{
  // assume grid_index is valid!!!!
  float charge = 0.0;
  // Loop over TMutHit's of grid cell at grid_index
  HitSet hs = grid[grid_index].second;
  HitSet::iterator hs_iter = hs.begin();
  for ( ; hs_iter != hs.end(); ++hs_iter ) {
    TMutHit* hit_ptr = hs_iter->first->get();
    charge = charge + hit_ptr->get_q();
  }
  return(charge);
}

//__________________________________________________________________________
void TMutHalfOctantBidimView::draw_mchits(void)
{
  TMutMCHitMap::const_iterator hit_iter = _mc_hit_map->range();
  while(TMutMCHitMap::const_pointer hit_ptr = hit_iter.next()){
    // if hit is not in this view then continue;
    if( ( hit_ptr->get()->get_station() != _station ) ||
  ( hit_ptr->get()->get_arm() != _arm ) ||
  ( hit_ptr->get()->get_octant() != _octant ) ||
  ( hit_ptr->get()->get_half_octant() != _halfoctant ) ) continue;       
    // monte-carlo hit
    PHPoint point(hit_ptr->get()->get_x(),
      hit_ptr->get()->get_y(),
      hit_ptr->get()->get_z());
    // with respect to origin of bidim frame
    point = point - _bidimFrame.getOrigin();
    // rotation in bidim frame
    double bidim_point_x = point.getX() * _bidimFrame.getU().getX() +
      point.getY() * _bidimFrame.getU().getY();
    double bidim_point_y = point.getX() * _bidimFrame.getV().getX() +
      point.getY() * _bidimFrame.getV().getY();
//     std::cout << "bidim_point_x = " << bidim_point_x << std::endl;
//     std::cout << "bidim_point_y = " << bidim_point_y << std::endl;
    // draw on same plot
    // memory leak!!!!
    // could be solved à la TMutPlaneView
    TMarker* m1 = new TMarker(bidim_point_x, bidim_point_y, 20);
    _pad2->cd();
    m1->Draw();
  }
  return;
}

//__________________________________________________________________________
void TMutHalfOctantBidimView::clear_temp_storage()
{
//    std::for_each(_marker_vector.begin(),_marker_vector.end(),PVDeleteMarker());
//    std::for_each(_line_vector.begin(),_line_vector.end(),PVDeleteLine());
}






