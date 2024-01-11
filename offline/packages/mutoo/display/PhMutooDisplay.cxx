// $Id: PhMutooDisplay.cxx,v 1.13 2014/01/26 18:05:17 bbannier Exp $

/*!
   \file PhMutooDisplay.cxx
   \brief mutoo 2D display
   \author Sean Kelly
   \version $Revision: 1.13 $
   \date $Date: 2014/01/26 18:05:17 $
*/

// MUTOO headers
#include <PhMutooDisplay.h>
#include <TMutNode.h>
#include <PHException.h>
#include <MUTOO.h>
#include <PHTimer.h>
#include <TCanvas.h>
#include <EventHeader.h>

// ROOT headers
#include <TCanvas.h>

/*! \ingroup display */
/*! 

This classes the display of the MUTR detector and is the interface
to the PHENIX standard event display.  Views of the detector are
represented by objects which are held has data members of the
manager class.  Which perspective gets drawn by the Draw() method is
determined at runtime from the associated state variable in this
object.  External control of manager state with the PHENIX event
display GUI should be implemented at some point.  Here is a screen
shot of a particular view.
<a href="http://www.nevis.columbia.edu/~kelly/phenix/MUTOO/mutoo_plane_view.gif"> Plane View </a>
*/

// STL/BOOST
#include <iostream>
#include <string>

using namespace std;

//____________________________________________________________________________
PhMutooDisplay::PhMutooDisplay() :
  TNamed("MUTOO","MUTOO"),
  _event(-1),
  _timer("PhMutooDisplay"),
  _draw_stubs(true),
  _draw_tracks(true),
  _draw_mc_tracks(true),
  _octant_view(0),
  _plane_view(0),
  _side_view(0),
  _half_octant_bidim_view(0){}


// Destructor
//
PhMutooDisplay::~PhMutooDisplay(){
  if(_plane_view) delete _plane_view;
  if(_octant_view) delete _octant_view;
  if(_side_view) delete _side_view;
  if(_half_octant_bidim_view) delete _half_octant_bidim_view;
}

// ROOT interface
//
void
PhMutooDisplay::Draw(Option_t *option)
{
  AppendPad(option);
}

// ROOT interface
//
void
PhMutooDisplay::set_draw_tracks(bool value){
  _draw_tracks = value;
}
void
PhMutooDisplay::set_draw_mc_tracks(bool value){
  _draw_mc_tracks = value;
}

void
PhMutooDisplay::set_draw_stubs(bool value){
  _draw_stubs = value;
}

void 
PhMutooDisplay::paint_plane_view(unsigned short arm, unsigned short octant)
{
  // Sanity check input args
  //
  if(arm >= MUTOO::NumberOfArms || octant >= MUTOO::NumberOfOctants){
    MUTOO::TRACE("Bad arm/octant specifier to PhMutooDisplay::paint_plane_view");
    return;
  }

  // If the requested view exist then update it.  If the requested
  // view doesn't exist then clean up and make a new view.  Currently
  // we accomodate only one open TMutPlaneView at a time -- this
  // could obviously be changed if needed.
  //
  if(_plane_view) {
    // View exist and different view is requested 
    //
    if(arm != _plane_view->get_arm() || octant != _plane_view->get_octant()){
      delete _plane_view;
      _plane_view = new TMutPlaneView(arm,octant,_event);
    }         
  } else {
    // View doesn't exist 
    //
    _plane_view = new TMutPlaneView(arm,octant,_event);
  }
  //  _plane_view->set_draw_stubs(_draw_stubs);
  _plane_view->set_draw_tracks(_draw_tracks);
  _plane_view->set_draw_mc_tracks(_draw_mc_tracks);
  _plane_view->event(_top_node);
  _plane_view->paint();
}

void 
PhMutooDisplay::paint_half_octant_bidim_view(unsigned short arm, unsigned short  octant, unsigned short halfoctant, unsigned short station)
{
  // Dummy function, to be inspired from paint_plane_view

  // Sanity check input args
  //
  if(arm >= MUTOO::NumberOfArms ||
     octant >= MUTOO::NumberOfOctants ||
     halfoctant >= MUTOO::NumberOfHalfOctants ||
     station >= MUTOO::NumberOfStations){
    MUTOO::TRACE("Bad arm/octant specifier to PhMutooDisplay::paint_half_octant_bidim_view");
    return;
  }

  // If the requested view exist then update it.  If the requested
  // view doesn't exist then clean up and make a new view.  Currently
  // we accomodate only one open TMutHalfOctantBidimView at a time -- this
  // could obviously be changed if needed.
  //
  if(_half_octant_bidim_view) {
    // View exist and different view is requested 
    //
    if( arm != _half_octant_bidim_view->get_arm() ||
  octant != _half_octant_bidim_view->get_octant() ||
  halfoctant != _half_octant_bidim_view->get_halfoctant() ||
  station != _half_octant_bidim_view->get_station() ){
      delete _half_octant_bidim_view;
      _half_octant_bidim_view = new TMutHalfOctantBidimView(arm,octant,halfoctant,station,_event);
    }         
  } else {
    // View doesn't exist 
    //
    _half_octant_bidim_view = new TMutHalfOctantBidimView(arm,octant,halfoctant,station,_event);
  }
  _half_octant_bidim_view->event(_top_node);
  _half_octant_bidim_view->paint();
}
 
//___________________________________________________________________________
void PhMutooDisplay::paint_octant_view(unsigned short arm, int octant, int station)
{

  // Sanity check input args
  if(arm >= MUTOO::NumberOfArms){
    MUTOO::TRACE("Bad arm specifier to PhMutooDisplay::paint_octant_view");
    return;
  }

  // Sanity check input args
  if( octant >= MUTOO::NumberOfOctants){
    MUTOO::TRACE("Bad octant specifier to PhMutooDisplay::paint_octant_view");
    return;
  }

  // Sanity check input args
  if( station >= MUTOO::NumberOfStations){
    MUTOO::TRACE("Bad station specifier to PhMutooDisplay::paint_octant_view");
    return;
  }
  
  // If the requested view exist then update it.  If the requested
  // view doesn't exist then clean up and make a new view.  Currently
  // we accomodate only one open TMutPlaneView at a time -- this
  // could obviously be changed if needed.
  //
  if(_octant_view) {
    // View exist and different view is requested 
    //
    if(arm != _octant_view->get_arm() || 
       octant != _octant_view->get_octant() || 
       station != _octant_view->get_station() ){
      //      delete _octant_view;
      _octant_view = new TMutOctantView(arm,_event,octant,station);
    }         
  } else {
    // View doesn't exist 
    //
    _octant_view = new TMutOctantView(arm,_event,octant,station);
  }
  _octant_view->event(_top_node);
  //  _octant_view->set_draw_stubs(_draw_stubs);
  _octant_view->paint();
}

//_________________________________________________________________________________________________
void PhMutooDisplay::paint_side_view(unsigned short arm, unsigned short octant, int station)
{
  
  //cout << "PhMutooDisplay::paint_side_view - arm: " << arm << " octant: " << octant << " station: " << station << endl; 
  
  // Sanity check input args
  if(arm >= MUTOO::NumberOfArms){
    MUTOO::TRACE("Bad arm specifier to PhMutooDisplay::paint_octant_view");
    return;
  }
  
  // If the requested view exist then update it.  If the requested
  // view doesn't exist then clean up and make a new view.  Currently
  // we accomodate only one open TMutSideView at a time -- this
  // could obviously be changed if needed.
  //
  if(_side_view) 
  {
    // View exist and different view is requested 
    if(
      arm != _side_view->get_arm() || 
      octant != _side_view->get_octant() || 
      station != _side_view->get_station() )
    {
      //cout << "PhMutooDisplay::paint_side_view - deleting old side view" << endl;
      delete _side_view;
    }        
    
  }
    
  //cout << "PhMutooDisplay::paint_side_view - creating new side view" << endl;
  _side_view = new TMutSideView(arm,octant,station,_event);
  
  //cout << "PhMutooDisplay::paint_side_view - processing event" << endl;
  _side_view->event(_top_node);
  
  //  _side_view->set_draw_stubs(_draw_stubs);
  //cout << "PhMutooDisplay::paint_side_view - painting" << endl;
  _side_view->paint();
  
}

Bool_t 
PhMutooDisplay::event(PHCompositeNode* top_node)
{  
  // Capture the top_node pointer so we can draw plane
  // view if requested
  //
  _top_node = top_node;
  
  // Attempt to get the event number from the node tree
  //
  try {
    EventHeader* evt = TMutNode<EventHeader>::find_io_node(top_node,"EventHeader");
    _event = evt->get_EvtSequence();
  } catch (std::exception& e) {
    _event = -1;
  }

  return True;
}












