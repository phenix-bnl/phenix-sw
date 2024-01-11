// $Id: mFvtxFindCoord.cxx,v 1.17 2014/01/29 23:08:56 brooks Exp $ 

/*!
  \file mFvtxFindCoord.h
  \brief create coordinates from clusters
  \author M. Brooks
  \version $Revision: 1.17 $
  \date $Date: 2014/01/29 23:08:56 $
*/

#include "FVTXOO.h"
#include "mFvtxFindCoord.h"

#include <PHGeometry.h>
#include <iostream>

using namespace std;

//____________________________________________________________________ 
mFvtxFindCoord::mFvtxFindCoord() :
 _timer(PHTimeServer::get()->insert_new("mFvtxFindCoord") )
{
  FVTXOO::TRACE("initializing module mFvtxFindCoord compiled " __DATE__ " " __TIME__);
}



//____________________________________________________________________ 
void mFvtxFindCoord::set_interface_ptrs(PHCompositeNode* top_node)
{

  // module runtime parameters
  _mod_par = TMutNode<mFvtxFindCoordPar>::find_node(top_node, "mFvtxFindCoordPar");

  //clus map
  _clus_map = TMutNode<TFvtxClusMap>::find_node(top_node, "TFvtxClusMap");

  //coord map
  _coord_map = TMutNode<TFvtxCoordMap>::find_node(top_node, "TFvtxCoordMap");

}

//____________________________________________________________________
PHBoolean mFvtxFindCoord::event(PHCompositeNode* top_node)
{
  _timer.get()->restart();
  try{
    set_interface_ptrs(top_node);
    find_coords();
  } catch(std::exception& e) {
    FVTXOO::TRACE(e.what());
    return False;
  }
  
  if( _mod_par->get_verbosity() >= FVTXOO::ALOT )
  { _coord_map->print(); }
  
  _timer.get()->stop();
  return True;


}

//____________________________________________________________________ 
void mFvtxFindCoord::find_coords()
{

  // Loop over clusters, fit and store resulting coordinate
  TFvtxClusMap::iterator clus_iter = _clus_map->range();
  while(TFvtxClusMap::pointer clus_ptr = clus_iter.next()) 
  {

    // Create an associated Coord for each cluster:
    TFvtxCoordMap::iterator coord_iter = _coord_map->insert_new(
      clus_ptr->get()->get_arm(), clus_ptr->get()->get_cage(),
      clus_ptr->get()->get_station(), clus_ptr->get()->get_sector(), 
      clus_ptr->get()->get_column());
    
    PHKey::associate(coord_iter.current(), clus_ptr);
    
    TFvtxHitMap::key_iterator hit_iter = clus_ptr->get()->get_associated<TFvtxHit>();
    
    FvtxStrip* strip_ptr = FvtxGeom::get_arm( hit_iter.current()->get()->get_arm() )->
      get_cage(hit_iter.current()->get()->get_cage())->
      get_station(hit_iter.current()->get()->get_station())->
      get_sector(hit_iter.current()->get()->get_sector())->
      get_column(hit_iter.current()->get()->get_column())->
      get_strip(hit_iter.current()->get()->get_strip());
   
    unsigned short stripbegin = hit_iter.current()->get()->get_strip();

    // Calculate average strip position, relative to first, without weighting:
    int istrip = 0;
    float strip_avg = 0.0;
    float strip_charge_sum = 0.0;
    float charge_weight = 1.0;
    unsigned short adc;

//    strip_avg = ((double)hit_iter.count() - 1)*0.5;
    double peak_charge = 0;
    while(TFvtxHitMap::pointer hit_ptr = hit_iter.next()) 
    {
      // Put ADC to charge conversion here for now - needs to be moved to a Calibrate module:
      if( _mod_par->get_use_charge_weighted_fit()) {
        adc = hit_ptr->get()->get_adc(); 
        charge_weight = adc < 7 ? 
           (FVTXOO::FPHX_THRESH[adc] + FVTXOO::FPHX_THRESH[adc+1])/2.0 :
           (FVTXOO::FPHX_THRESH[adc] + FVTXOO::FPHX_MAXCHARGE)/2.0 ;
      }
      if (charge_weight > peak_charge) peak_charge = charge_weight;
      strip_avg += istrip * charge_weight;
      strip_charge_sum += charge_weight;
      istrip++;
    }

    strip_avg = strip_avg/strip_charge_sum; 

    strip_avg *= strip_ptr->get_width();

    double error = strip_ptr->get_width()/sqrt(12.0);
    //double error = strip_ptr->get_width()/(2.0*sqrt(6.0));
    
    coord_iter->get()->set_q_tot(strip_charge_sum);

    coord_iter->get()->set_q_peak(peak_charge);

    coord_iter->get()->set_w(strip_avg);

    coord_iter->get()->set_peak_strip((unsigned short)(stripbegin + strip_avg));
    
    PHPoint coord_begin = strip_ptr->get_position_begin();
    PHPoint coord_end = strip_ptr->get_position_end();
    double strip_length = PHGeometry::distancePointToPoint(coord_begin, coord_end);
    
    PHLine zero_line(coord_begin, coord_end);
    
    double dx = strip_avg*(coord_end.getY() - coord_begin.getY()) / strip_length;
    double dy = -strip_avg*(coord_end.getX() - coord_begin.getX()) / strip_length;

    // South and North arms appear to have geometry defined differently resulting in the
    // coordinate position versus q/qtotal being backwards for South arm if you use the
    // same equations as north.  Attempt to fix that here:
    // 20-Jul-12: this fix seems to be no longer needed with other geometry changes
    // Should make global geometry calculations more robust against geom changes...

    /*
    if (clus_ptr->get()->get_arm() == FVTXOO::South){
      dx = -dx;
      dy = -dy;
    }
    */
    
    coord_begin.setX(coord_begin.getX() + dx);
    coord_end.setX(coord_end.getX() + dx);
    
    coord_begin.setY(coord_begin.getY() + dy);
    coord_end.setY(coord_end.getY() + dy);
    
    //line representing coordinate
    PHLine coord_line(coord_begin, coord_end);
    
    //setting coordinate, this will also give you the mean z value
    coord_iter->get()->set_coord(coord_line);
    coord_iter->get()->set_error(error);
    
  }
}
void
mFvtxFindCoord::end(PHCompositeNode*)
{
   _timer.get()->print_stat();

  return;
}
