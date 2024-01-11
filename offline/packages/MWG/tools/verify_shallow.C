#include "Tools.h"
#include <PHMuoTracksOut.h>

//================================================================================
// Verifies that the road is shallow and did not just miss the next gap by traveling
// outside its active region.  This is done by projecting the roads (x,y) position 
// to the next gap and checking that it is within the physical space of the next 
// gap layer.  Returns true if this is the case.
//
// MuID coordinates were obtained from the database and then rounded here.
//
// This check is not perfect, but it should eliminate the most egregious of these roads.
//

Bool_t Tools::verify_shallow(PHMuoTracksOut* &muo, Int_t itrk)
{
  // Checking last road hit is from a shallow gap and getting its gap number.
  if( !Tools::is_road_shallow(muo, itrk) ) return false;
  Int_t depth = Tools::max_road_depth(muo, itrk);  // 0, 1, 2 = Gap2, Gap3, Gap4 respectively
  
  // retrieve road information from Gap0
  Float_t x_pos  = muo->get_muIDOO_gap0(0, depth, itrk); // x position
  Float_t y_pos  = muo->get_muIDOO_gap0(1, depth, itrk); // y position
  Float_t z_pos  = muo->get_muIDOO_gap0(2, depth, itrk); // z position
  Float_t slopex = muo->get_muIDOO_gap0(3, depth, itrk); // dx/dz
  Float_t slopey = muo->get_muIDOO_gap0(4, depth, itrk); // dy/dz

  // negate the MuID slope if necessary
  if( z_pos < 0 ) {
    slopex *= -1;
    slopey *= -1;
 }
  
  // Approximate delta_z between the panels of Gap0 and Gap3 or Gap4.
  // Except for panel 4 south, all the delta_z values for the different
  // panels are within 2 cm of each other, so for simplification an average 
  // is taken between them.  Panel 4 south is handled differently later.
  Float_t delta_z = 0.0;
  if( z_pos < 0 ) delta_z = (depth==0) ? 124.3 : 167.7;
  else            delta_z = (depth==0) ? 124.0 : 167.5;

  // projected change in x,y position to next gap
  Float_t delta_x = slopex * delta_z;
  Float_t delta_y = slopey * delta_z;
  
  // projected x,y at next gap
  Float_t x_pos_new = x_pos + delta_x;
  Float_t y_pos_new = y_pos + delta_y;
  
  // The x,y spacial coordinates of all MuID layers for north and south arms
  // are approximately the same (within 1 cm).  This allows for simplification
  // by being able to compare all roads to the same set of integer rounded MuID 
  // panel coordinates.

       // xmin              x_max             y_min             y_max
       if(x_pos_new> 105 && x_pos_new< 641 && y_pos_new>-483 && y_pos_new< 500) return true; // within panel 0+5
  else if(x_pos_new>-642 && x_pos_new<-107 && y_pos_new>-483 && y_pos_new< 500) return true; // within panel 2+3
  else if(x_pos_new>-125 && x_pos_new< 124 && y_pos_new> 101 && y_pos_new< 483) return true; // within panel 1

  // change parameters for panel 4 south if necessary
  if( z_pos < 0 ) {
    delta_z = (depth==0) ? 128.1 : 173.5;
    delta_x = slopex * delta_z;
    delta_y = slopey * delta_z;
    x_pos_new = x_pos + delta_x;
    y_pos_new = y_pos + delta_y;
  }
  
  if(x_pos_new>-125 && x_pos_new< 124 && y_pos_new>-483 && y_pos_new<-102) return true; // within panel 4
  
  return false;

}