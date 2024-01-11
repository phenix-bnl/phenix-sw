#ifndef __MMUTREJECTTRACKMODULE_HH__
#define __MMUTREJECTTRACKMODULE_HH__

#include<PHTimeServer.h>
#include<mMutRejectTrackPar.h>
#include<TMutTrkMap.h>
#include<TMutStubMap.h>

/*! \ingroup modules */
/*!

<br>
<h2>Analysis Module Interface Specification</h2>
<table>
<tr>
<td> Object </td>
<td> Description </td>
<td> Privilege </td>
</tr>
<tr>
<td> const mMutRejectTrackPar*</td>
<td> Parameter Table </td>
<td> immutable </td>
</tr>
<tr>
<td> TMutTrkMap*</td>
<td> Parameter Table </td>
<td> mutable </td>
</tr>
<tr>
<td> TMutStubMap*</td>
<td> Parameter Table </td>
<td> mutable </td>
</tr>

INSERT ADDITIONAL IOC SPECIFICATIONS HERE

</table>
*/

class mMutRejectTrack
{
 public: 
  
  //! constructor
  mMutRejectTrack();

  //! destructor   
  virtual ~mMutRejectTrack(){}
  
  //! event method 
  virtual PHBoolean event(PHCompositeNode* top_node);
  
  //! event method (single octant)
  virtual PHBoolean event(PHCompositeNode* top_node, unsigned short octant);
  
  //! print rejection module summary
  virtual void print_summary( std::ostream& out = std::cout );

 private:  
  
  //! sort tracks according to their chisquare value
  struct less_chisq_ftor {
    bool operator()(const TMutTrkMap::pointer trk1_ptr, TMutTrkMap::pointer trk2_ptr)
    { return (trk1_ptr->get()->get_w_chi_square_pdf() < trk2_ptr->get()->get_w_chi_square_pdf()); }
  };   
  
  //! get pointer to local nodes
  void set_interface_ptrs(PHCompositeNode* top_node);  
  
  //! decide which tracks are ghosts
  void tag_ghosts();
  
  //! remove rejected tracks from map
  /*! by default this method is not called since it creates some unnecessary overhead */
  void remove_rejected_tracks( void );

  //! remove bad coordinates from tracks
  void reject_outliers();
  
  //! return true if two tracks momentum pass relative difference cut
  bool check_momentum( TMutTrkMap::const_pointer, TMutTrkMap::const_pointer ) const;

  //! parameter table
  const mMutRejectTrackPar* _mod_par;    
  
  //! track map 
  TMutTrkMap* _trk_map;                  
  
  //! stub map
  TMutStubMap* _stub_map;    

  //! true if rejection is to be run in a single octant
  bool _single_octant;
  
  //! octant in which rejection is to be run
  unsigned short _octant;

  //! counter on total number of tracks
  int _total_tracks;
  
  //! counter on number of accepted tracks
  int _accepted_tracks;

  //! Timer
  PHTimeServer::timer _timer;
};

#endif /* __MMUTREJECTTRACKMODULE_HH__ */






