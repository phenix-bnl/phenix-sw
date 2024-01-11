// $Id: TMuiChannelId.cc,v 1.8 2007/04/04 22:48:10 hpereira Exp $   
/*!
   \file TMuiChannelId.cc
   \brief "software" representation of a MuID channel address.
   \version $Revision: 1.8 $
   \date $Date: 2007/04/04 22:48:10 $
*/

#include <iostream>
#include "TMuiChannelId.hh"

// start: some helpful numbers for mapping to HV chains
static const short kMui_n_tubes_large_vert_west = 63;

// large panels
// horizontal: 0-18 -> chain_id0, 19-38 -> chain_id1, >38 -> chain_id2 
// vertical: 0-21 -> chain_id0, 19-38 -> chain_id1, >38 -> chain_id2 
// The values stored in the limit array is the upper inclusive bound for 
// the chains.
static const short kMui_large_chain_limits[TMuiChannelId::kOrientations][2] = 
{
  {18, 38}, 
  {21, 43}
};

// small panels
// horizontal:  0-22 -> chain_id0, >22 -> chain_id1
static const short kMui_small_chain_limit_horiz = 22;
// vertical - there is only chain with 25 tubes
// end : some helpful numbers for mapping to HV chains

// Functions for hash lookup based on TMuiChannelId objects.
// Implement these functions outside of the TMuiChannelId class for
// flexibility.
// (See Taligent guide, "Portable Hash")

//: Returns the position of the given TMuiChannelId object in the
//: PanelGeo hash table.
size_t PanelHash(const TMuiChannelId& ident)
{
  return 
    ( (ident.Arm()*TMuiChannelId::kPlanesPerArm   + ident.Plane())
    *TMuiChannelId::kPanelsPerPlane + ident.Panel() )
    % TMuiChannelId::kPanelsTotal;
}

//: Returns the position of the given TMuiChannelId object in the
//: TwoPackGeo hash table.
size_t TwoPackHash(const TMuiChannelId& ident)
{
  return ( ( ( (ident.Arm()
    *TMuiChannelId::kPlanesPerArm   + ident.Plane() )
    *TMuiChannelId::kPanelsPerPlane + ident.Panel() ) 
    *TMuiChannelId::kOrientations   + ident.Orient() )
    *TMuiChannelId::kTwoPacksPerPanelMax + ident.TwoPack() )
    % TMuiChannelId::kTwoPacksMaxTotal;
}

// Output to a stream.
std::ostream& operator << (std::ostream& s, const TMuiChannelId& ident)
{
  if (ident.TwoPack() >= 0) s << "Arm " << ident.Arm() << " Plane " << ident.Plane() << " Panel " << ident.Panel() << " Two-pack " << ident.TwoPack() << ((ident.Orient() == kHORIZ) ? "H":"V" );
  else s << "Arm " << ident.Arm() << " Plane " << ident.Plane() << " Panel " << ident.Panel();
  return s;
}

short TMuiChannelId::get_HV_chain_group() const
{
  // A two-pack has HV to its tubes (in two layers) from different HV chains.
  // These are labelled: 1-6 in hardware land for large panels. The return code
  // from this routine is a group of HV chains encoded as follows..
  // 0 : for chains 1,2
  // 1 : for chains 3,4
  // 2 : for chains 5,6

  short hardware_twopack = fTwoPack;
  // hardware_twopack is the same as the software twopack index, except for
  // West large panels, i.e. panels 0 and 5, and only for the vertical tubes,
  // for which the scales are reversed
  // These panels have 63 tubes in two layers(twopacks)
  if (fOrient == kVERT && (fPanel == 0 || fPanel == 5) ) 
  { hardware_twopack = kMui_n_tubes_large_vert_west - hardware_twopack; }

  // 3, convenient value to have for nav.
  short p_div_2( kPanelsPerPlane/2 ); 
  bool top(fPanel < p_div_2);
  bool large((fPanel % p_div_2) != 1);
  bool horiz(fOrient == kHORIZ);

  // bottom has an offset of 1 for the large horizontal limits, otherwise 
  // bottom and top panel limits are the same.
  short offset = (!top && large && horiz) ? 1:0;

  // Now: let's figure out to which chain this twopack belongs
  short hv_chain_id = -1; 

  if (large) 
  {
    short id = 0; 
    while (id < 2) { 
      // 2 limits for large panels 
      if (hardware_twopack <= (kMui_large_chain_limits[fOrient][id] + offset) ) 
      {
  hv_chain_id = id;
  id = 2; // no need to look further
      } 
      id++;
    }
    
    // not assigned, it must be the last chain..
    if (hv_chain_id == -1)  hv_chain_id = 2;

  } else {
    
    // small
    if (horiz) 
    {
      // horizontal
      if (hardware_twopack <= kMui_small_chain_limit_horiz) hv_chain_id = 0;
      else hv_chain_id = 1;
    } else {
      // vertical
      hv_chain_id = 0;
    }
  }
  
  return hv_chain_id;
}
