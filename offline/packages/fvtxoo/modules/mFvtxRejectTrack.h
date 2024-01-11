#ifndef __MFVTXREJECTTRACKMODULE_HH__
#define __MFVTXREJECTTRACKMODULE_HH__

#include<PHTimeServer.h>
#include<mFvtxRejectTrackPar.h>
#include<TFvtxTrkMap.h>
#include<TFvtxCoordMap.h>

#include<mFvtxModuleBase.h>

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
<td> const mFvtxRejectTrackPar*</td>
<td> Parameter Table </td>
<td> immutable </td>
</tr>
<tr>
<td> TFvtxTrkMap*</td>
<td> Parameter Table </td>
<td> mutable </td>
</tr>
<tr>
<td> TFvtxCoordMap*</td>
<td> Parameter Table </td>
<td> mutable </td>
</tr>

INSERT ADDITIONAL IOC SPECIFICATIONS HERE

</table>
*/

class mFvtxRejectTrack : public mFvtxModuleBase
{
 public: 
  
  //! constructor
  mFvtxRejectTrack();

  //! destructor   
  virtual ~mFvtxRejectTrack(){}
  
  //! event method 
  virtual PHBoolean event(PHCompositeNode* top_node);
  
  //! event method (single octant)
  virtual PHBoolean event(PHCompositeNode* top_node, unsigned short octant);

  void init(PHCompositeNode* top_node){}; 
  void init_run(PHCompositeNode* top_node){}; 
  void end(PHCompositeNode* top_node){}; 
  
  //! print rejection module summary
  virtual void print_summary( std::ostream& out = std::cout );

 private:  
  
  //! sort tracks according to their chisquare value
  struct less_chisq_ftor {
    bool operator()(const TFvtxTrkMap::pointer trk1_ptr, TFvtxTrkMap::pointer trk2_ptr)
    { return (trk1_ptr->get()->get_w_chi_square_pdf() < trk2_ptr->get()->get_w_chi_square_pdf()); }
  };   
  
  //! get pointer to local nodes
  void set_interface_ptrs(PHCompositeNode* top_node);  
  
  //! decide which tracks are ghosts
  void tag_ghosts();
  
  //! remove bad coordinates from tracks
  void reject_outliers();
  
  //! return true if two tracks momentum pass relative difference cut
  bool check_momentum( TFvtxTrkMap::const_pointer, TFvtxTrkMap::const_pointer ) const;

  //! parameter table
  const mFvtxRejectTrackPar* _mod_par;    
  
  //! track map 
  TFvtxTrkMap* _trk_map;                  
  
  //! coord map
  TFvtxCoordMap* _coord_map;    

  //! counter on total number of tracks
  int _total_tracks;
  
  //! counter on number of accepted tracks
  int _accepted_tracks;

  //! Timer
  PHTimeServer::timer _timer;
};

#endif /* __MFVTXREJECTTRACKMODULE_HH__ */






