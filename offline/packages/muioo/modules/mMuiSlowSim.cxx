// $Id: mMuiSlowSim.cxx,v 1.4 2009/09/18 04:43:19 hpereira Exp $

/*!
   \file mMuiSlowSim.cxx
   \brief Convert PISA to detector hits in MUID
   \author J. Nagle C. Zhang
   \version $Revision: 1.4 $
   \date $Date: 2009/09/18 04:43:19 $
*/

/*!	A more detailed description:
    mMuiSlowSim fills the MuID interface object, TMuiMCHitO, with MuID PISA information.
    The MuID "Hit" object is filled with the following:	position, momentum, particle id,
    and which two-pack the particle passed through.	Though PISA does not contain the two-pack
    information, the position information is used to find which two-pack was "hit" in the
    simulation.
*/

// MUIOO headers
#include <mMuiSlowSim.h>
#include <mMuiSlowSimPar.h>

// PISA hit table,
#include <rootAnc.h>

// Geometry modules
#include <MuiGeomClasses.hh>

// PHENIX headers
#include <PHGeometryObject.h>

/*! \ingroup modules */
// STL/BOOST
//
#include <iostream>
#include <string>

using namespace std;

//_________________________________________________________________
mMuiSlowSim::mMuiSlowSim() :
  _timer( PHTimeServer::get()->insert_new("mMuiSlowSim") )
{
  MUIOO::TRACE("initializing module mMuiSlowSim");
}

//_________________________________________________________________
PHBoolean mMuiSlowSim::event(PHCompositeNode* top_node)
{

  _timer.get()->restart();

  try {
    // Reset IOC pointers
    set_interface_ptrs(top_node);
    _mc_hit_map->clear();

    // Do the conversion of PISA hits to TMuiMCHit objects
    digitize();

    // Do the association between MCMuiHit and MCTrack,
    // MuTr slowsim has to been run first.
    associate_mctrk();

  } catch(std::exception& e) {
    // MODULE CODE GOES HERE
    //
    MUIOO::TRACE(e.what());
    return False;
  }

  // If verbose dump the contents of the cluster map
  // Verbosity methods declared in TMuiParBase

  _timer.get()->stop();
  if(_mod_par->get_verbosity() >= MUIOO::ALOT) _mc_hit_map->print();
  if(_mod_par->get_verbosity() >= MUIOO::SOME) _timer.get()->print();
  return True;
}

//_________________________________________________________________
void mMuiSlowSim::set_interface_ptrs(PHCompositeNode* top_node)
{
  // module runtime parameters
  _mod_par = TMutNode<mMuiSlowSimPar>::find_node(top_node,"mMuiSlowSimPar");

  // Interface Object Container pointers for muid MC hits.
  _mc_hit_map = TMutNode<TMuiMCHitMapO>::find_node(top_node,"TMuiMCHitMapO");

  // Interface Object Containter pointers for MC tracks.
  _mc_trk_map = TMutNode<TMutMCTrkMap>::find_node(top_node,"TMutMCTrkMap");

  // PISA input in wrapped STAF table form here.	Use of wrapped STAF tables
  // has been depreciated -- this interface will eventually go away.
  munhitsWrapper* munhits_ptr= TMutNode<munhitsWrapper>::find_io_node(top_node,"munhits");
  _munhits_h = munhits_ptr->TableHeader();
  _munhits = munhits_ptr->TableData();
}


//_________________________________________________________________
void mMuiSlowSim::digitize()
{

  for (int hit=0; hit<_munhits_h.nok; hit++)
  {

    // Determine the arm and gap number from the PISA plane number.
    // PISA convention:	2001-2006 are South Arm, 1001-1006 are North.
    //staf table
    short PisaPlane = _munhits[hit].plane_num;

    /*plane_num is data memeber of munhits struct*/
    short iArm = (PisaPlane > 2000 ) ? kSOUTH:kNORTH;

    //divide 1001 through 1006 each by 1000 taken remainder subtract by 1
    short	iPlane = (PisaPlane%1000) - 1;

    if ( (iArm<0) || (iArm>=TMuiChannelId::kArmsTotal) || (iPlane<0) || (iPlane>=TMuiChannelId::kPlanesPerArm) )
    { continue; }

    TMuiMCHitMapO::iterator mc_hit_iter = _mc_hit_map->insert_new(iArm,iPlane);
    //mc_hit_iter is the pointer to the interface object (TMuiHitO the MC Hit Object)
    //insert_new methid is declared in file: TMuiMCHitMapO.h
    /* ONLY insert arm and plane information since there is NOT any panel or orientation
    information in PISA for the MuID */
    /* Set the location of the MCHit object */
    mc_hit_iter->get()->set_arm(iArm);
    mc_hit_iter->get()->set_plane(iPlane);

    /*the following seven set_* methods are defined in TMuiMCHitO */
    mc_hit_iter->get()->set_track_id(_munhits[hit].track_num);
    mc_hit_iter->get()->set_x(_munhits[hit].rhit[0]);
    mc_hit_iter->get()->set_y(_munhits[hit].rhit[1]);
    mc_hit_iter->get()->set_z(_munhits[hit].rhit[2]);
    mc_hit_iter->get()->set_px(_munhits[hit].phit[0]);
    mc_hit_iter->get()->set_py(_munhits[hit].phit[1]);
    mc_hit_iter->get()->set_pz(_munhits[hit].phit[2]);
    mc_hit_iter->get()->set_pid(_munhits[hit].trk_id);

    /* the get()->set_* takes pisa information and puts it into the interface object (here TMuiMCHitO) which has already had its pointer inserted into the container (here the the MCHitMap) */

    // rhit and phit originate in the MuID PISA file:
    // simulation/pisa2000/src/phnxcore/encodeRootEvntMui.cc
    PHVector X(
      _munhits[hit].rhit[0],
      _munhits[hit].rhit[1],
      _munhits[hit].rhit[2]);

    // make a unite vector from phit.
    float r = sqrt(_munhits[hit].phit[0]*_munhits[hit].phit[0]+_munhits[hit].phit[1]*_munhits[hit].phit[1]+_munhits[hit].phit[2]*_munhits[hit].phit[2]);
    float v1=_munhits[hit].phit[0]/r;
    float v2=_munhits[hit].phit[1]/r;
    float v3=_munhits[hit].phit[2]/r;

    /* need clarification
    Hep3Vector V(
      _munhits[hit].phit[0],		 //phit is the MuID PISA momentum
      _munhits[hit].phit[1],		 //...unit vector DirVect
      _munhits[hit].phit[2]);
    */
    PHVector V(v1,v2,v3);
    std::vector<TMuiChannelId> twoPackList;

    //TMuiChannelId located at offline/packages/muigeom

    twoPackList = TMuiGeometry::Geom()->findTwoPacks(iArm,iPlane,X,V);

    //TMuiGeometry located at offline/packages/muigeom
    //for above "findTwoPacks" accessor method declared in TMuiGeometry.hh
    // Find two-packs in the specified plane that lie along a trajectory.
    // The trajectory is given by the position GVect and the unit vector
    // DirVect in global coordinates.
    /* findTwoPacks(const short& Arm, const short& Plane, const Hep3Vector& GVect,
    const Hep3Vector &DirVect); */
    if (twoPackList.size() > 0)
    {
      for (unsigned int i=0; i<twoPackList.size(); i++)
      {

        short iPanel			= twoPackList[i].Panel();
        short iTwoPack		= twoPackList[i].TwoPack();
        EOrient_t iOrient = twoPackList[i].Orient();
        mc_hit_iter->get()->add_twopack(iOrient, iPanel, iTwoPack);

        /*	Panel(), Orient(), TwoPack() are TMuiChannelId accessor methods	*/
        /*	add_two_pack is a TMuiMCHitO method */
        /* now the interface object contains the two_pack information */
      }
    }
  }
  //	cout << "mMuiReadout-I8	finished looping through PISA data" << endl;
}

//_________________________________________________________________
void mMuiSlowSim::associate_mctrk()
{
  // Loop over TMuiMCHitMap [
  //	 Loop over TMutMCTrkMap [
  //		 if(the trkid from mctrk map matches trkid of muimchit map) [
  //			 associate muimchit and mctrk.
  //			 set association flag true
  //		 ]
  //	 ]
  //	 if(association flag is not true) [
  //			 make a new mctrk and insert it in map.
  //			 fill the fields of new mctrk object.
  //	 ]
  // ]

  TMuiMCHitMapO::iterator muimchit_iter = _mc_hit_map->range();
  while(TMuiMCHitMapO::pointer muimchit_ptr = muimchit_iter.next())
  {

    TMutMCTrkMap::iterator mctrk_iter = _mc_trk_map->range();
    bool is_associated = false;
    while(TMutMCTrkMap::pointer mctrk_ptr = mctrk_iter.next())
    {

      if(mctrk_ptr->get()->get_track_id()!=muimchit_ptr->get()->get_track_id())
      { continue; }
      is_associated = true;
      PHKey::associate(muimchit_ptr,mctrk_ptr);
    }
    if(is_associated==false) fill_new_mctrk(muimchit_ptr);

  }
}

//_________________________________________________________________
void mMuiSlowSim::fill_new_mctrk(TMuiMCHitMapO::pointer mc_hit_ptr, int trackID)
{

  int track_id = trackID;
  if (trackID == 0) track_id = mc_hit_ptr->get()->get_track_id();

  // Insert an new TMutMCTrk into map
  TMutMCTrkMap::iterator mctrk_iter = _mc_trk_map->insert_new(mc_hit_ptr->get()->get_arm());
  mctrk_iter->get()->set_arm(mc_hit_ptr->get()->get_arm());
  mctrk_iter->get()->from_pisa( track_id );

  // Do the association
  if (trackID == 0) PHKey::associate(mc_hit_ptr, mctrk_iter.current());

  //Add a TMutMCTrk for the parent
  int itparent( mctrk_iter->get()->get_parent_track_id() );
  int idparent( mctrk_iter->get()->get_parent_id() );
  if (itparent > 0 && idparent != 0)
  {

    //Check to see if parent already has a track
    bool parentTrkFilled = false;
    TMutMCTrkMap::iterator mctrk_iter2 = _mc_trk_map->range();
    while(TMutMCTrkMap::pointer mctrk_ptr = mctrk_iter2.next()){
      if(mctrk_ptr->get()->get_track_id() == abs(itparent)) {
        parentTrkFilled = true;
        break;
      }
    }

    if (!parentTrkFilled) fill_new_mctrk(mc_hit_ptr, abs(itparent));
  }

}
