#ifndef __MMUTPHMUTOODISPLAY_HH__
#define __MMUTPHMUTOODISPLAY_HH__

#include <PHTimer.h>
#include <TNamed.h>

#include <TMutOctantView.h>
#include <TMutPlaneView.h>
#include <TMutSideView.h>
#include <TMutHalfOctantBidimView.h>

class PhMutooDisplayPar;
class TCanvas;

/*! @defgroup display MUTOO Display

The Display library contains classes to interface with the
standard PHENIX event display package.

*/


/*! \ingroup display */
/*!

INSERT MODULE DESCRIPTION HERE 

<br>
<h2>Analysis Module Interface Specification</h2>
<table>
<tr>
<td> Object </td>
<td> Description </td>
<td> Privilege </td>
</tr>
<tr>
<td> const PhMutooDisplayPar*</td>
<td> Parameter Table </td>
<td> immutable </td>
</tr>

INSERT ADDITIONAL IOC SPECIFICATIONS HERE

</table>
*/

class PhMutooDisplay 
: public TNamed
{
 public: 

  // ROOT/PHOOL interface 

  //! constructor
  PhMutooDisplay(); 
  
  //! destructor
  virtual ~PhMutooDisplay(); 
  
  virtual Bool_t event(PHCompositeNode*);
  virtual void Draw(Option_t *option="");
  virtual void Paint(Option_t* = "") {}
  virtual void Print(Option_t* = "") const {}

  // MUTOO interface
  
  //! draw a muon tracker octant
  void paint_plane_view(unsigned short arm, unsigned short octant);
  
  //! draw a muon tracker half octant
  void paint_half_octant_bidim_view(unsigned short arm, unsigned short octant, unsigned short halfoctant, unsigned short station);
  
  //! draw a muon tracker octant
  void paint_octant_view(unsigned short arm=0, int octant=-1, int station=-1);
  
  //! draw muon tracker side view
  void paint_side_view(unsigned short arm=0, unsigned short octant=0, int station=-1);
  
  //! decide if stubs are to be drawn
  void set_draw_stubs(bool);
  
  //! toggle tracks
  void set_draw_tracks(bool);
  
  //! toggle MC tracks
  void set_draw_mc_tracks(bool);

 private:

  //! event index
  int _event;
  
  //! module timer
  PHTimer _timer; 
  
  //! top node
  PHCompositeNode* _top_node;
  
  //! toggle stubs
  bool _draw_stubs;
  
  //! toggle tracks
  bool _draw_tracks;

  //! toggle mc tracks
  bool _draw_mc_tracks;
  
  TMutOctantView* _octant_view;
  TMutPlaneView* _plane_view;
  TMutSideView* _side_view;
  TMutHalfOctantBidimView* _half_octant_bidim_view;

};

#endif /* __PHMUTOODISPLAY_HH__ */
