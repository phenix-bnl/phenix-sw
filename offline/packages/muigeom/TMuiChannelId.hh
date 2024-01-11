#ifndef MUI_CHANNEL_ID_H
#define MUI_CHANNEL_ID_H
// $Id: TMuiChannelId.hh,v 1.5 2006/12/21 13:41:44 hpereira Exp $   
/*!
   \file TMuiChannelId.hh
   \brief "software" representation of a MuID channel address.
   \version $Revision: 1.5 $
   \date $Date: 2006/12/21 13:41:44 $
*/

#include <cstddef>
#include <iostream>
#include "MuiCommon.hh"

//! "software" representation of a MuID channel address.
/*!
 "software" representation of a MuID channel address.
 
 Indexing a MuID readout channel (in the "software" scheme) requires
 five numbers:
<ul>
<li> the arm number, fArm (0 - North, 1 - South)
<li> the gap or plane number in the arm, fPlane (0-4)
<li> the panel number within the plane, fPanel (0-5)
<li> the tube orientation, fOrientation ("kHORIZ" - horizontal, "kVERT" - vertical)
<li> the tube number within the panel for the given orientation, fTwoPack (0 - some panel-dependent value)
</ul> 

@author <a href="mailto:pope@phy.ornl.gov">Kyle Pope </a>
 
*/
class TMuiChannelId
{
  
  public:
  
  enum 
  {
    //! number of planes for each arm
    kPlanesPerArm = 5,
  
    //! number of panels per plane 
    kPanelsPerPlane = 6, 
    
    //! Maximum number of two-packs in a given orientation/gap. 
    kTwoPacksPerPlaneMax = 326, 
 
    //! Number of two-packs of a single orientation in a panel. 
    /*! (This number differs from H to V and from panel to panel, so the upper limit is used here.) */ 
    kTwoPacksPerPanelMax = 64, 

    //! Number of arms in the system. 
    kArmsTotal = 2, 
    
    //! Number of two-pack orientations = 2 (HORIZ, VERT). 
    kOrientations = 2, 
    
    //! Number of gaps total in the system. 
    kPlanesTotal = kArmsTotal*kPlanesPerArm, 
    
    //! Number of panels total in the system. 
    kPanelsTotal = kPlanesTotal*kPanelsPerPlane, 
    
    //! = kPanelsTotal * kOrientations 
    kPanelsXYTotal = kPanelsTotal*kOrientations, 
    
    //! Upper limit on the number of two-packs in the system. 
    /*!  (assuming each panel has kTwoPacksPerPanelMax two-packs) */ 
    kTwoPacksMaxTotal = kPanelsXYTotal*kTwoPacksPerPanelMax, 
    
    //! Maximum allowed raw hits per panel. 
    kHitsPerPanelMax = kTwoPacksPerPanelMax, 
    
    //! Maximum allowed raw hits in entire MuID. 
    kHitsMaxTotal = kTwoPacksMaxTotal 
  }; 
  
  //! Constructor.
  TMuiChannelId(
    const short& arm=-1,
    const short& plane=-1,
    const short& panel=-1,
    const EOrient_t& orient=kHORIZ,
    const short& twopack=-1): 
    fArm(arm), 
    fPlane(plane), 
    fPanel(panel), 
    fOrient(orient),
    fTwoPack(twopack) 
    {};

  //! Destructor.
  ~TMuiChannelId()
  {}

  //! Arm identifier (0 - South, 1 - North)
  short Arm() const 
  {return fArm;};

  //! Gap identifier (0-4)
  short Plane() const 
  {return fPlane;};
  
  //! Panel identifier (0-5)
  short Panel() const 
  {return fPanel;};
  
  //! Orientation of two-pack (kHORIZ or kVERT).
  EOrient_t Orient() const 
  {return fOrient;};
  
  //! Two-pack ident (starts at 0)
  short TwoPack() const 
  {return fTwoPack;};

  //! tube to HV chain translator method
  short get_HV_chain_group() const;

  //! Set the sub-identifiers.
  void Set(
    const short& arm, 
    const short& plane, 
    const short& panel, 
    const EOrient_t& orient = kHORIZ, 
    const short& twopack = -1)
  {
    fArm = arm;
    fPlane = plane;
    fPanel = panel;
    fOrient  = orient;
    fTwoPack = twopack;
  }
  
  private:
  
  //! arm index
  short fArm;
  
  //! plane index
  short fPlane; 
  
  //! panel index
  short fPanel; 
  
  //! orientation
  EOrient_t fOrient; 
  
  //! two pack index
  short fTwoPack;

  //! Print TMuiChannelId information to a stream.
  friend std::ostream& operator << (std::ostream& s, const TMuiChannelId& n);
  
};

//! Return the position of a TMuiChannelId object in the PanelGeo hash table.
size_t PanelHash(const TMuiChannelId& ident);

//! Return the position of a TMuiChannelId object in the TwoPackGeo hash table.
size_t TwoPackHash(const TMuiChannelId& ident);

#endif  /* MUI_CHANNEL_ID_H */
