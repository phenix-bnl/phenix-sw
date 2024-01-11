// $Id: MuonDisplay.h,v 1.1.1.1 2008/07/30 17:43:59 hpereira Exp $
#ifndef __MUONDISPLAY_H__
#define __MUONDISPLAY_H__

#include<string>
#include<SubsysReco.h>
#include<TDataType.h> 

/*!
	\file    MuonDisplay.h
  \ingroup supermodules 
	\brief   mutoo 2D event display supermodule. 
    Displays Mutr detectors, mc tracks, 
    tracks, stubs, mutr clusters, on user request. 
    Only front and side (octant) views are supported.
	  Also dumps all event maps on request.
  \author  Hugo Pereira
	\version $Revision: 1.1.1.1 $
	\date    $Date: 2008/07/30 17:43:59 $
*/

// Forward declerations
class PHCompositeNode;

#ifndef __CINT__
#include <PHTimeServer.h>
#endif

// MUON
class PhMutooDisplay;

/*!
	\class   MuonDisplay
  \ingroup supermodules 
	\brief   mutoo 2D event display supermodule. 
    Displays Mutr detectors, mc tracks, 
    tracks, stubs, mutr clusters, on user request. 
    Only front and side (octant) views are supported.
	  Also dumps all event maps on request.
*/ 
class MuonDisplay: public SubsysReco
{
 public:

  //! constructor
  MuonDisplay( const char* name = "MUONDISPLAY" );

  //! destructor
  virtual ~MuonDisplay() {}

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  
  int End(PHCompositeNode *topNode);
 
  void dump_mc_primary();
  void dump_hit();
  void dump_clus();
  void dump_coord();
  void dump_coord(ULong_t);
  void dump_gap_coord();
  void dump_stub();
  void dump_trk();
  void dump_vtx();
  void dump_mc_hit();
  void dump_mc_trk();
  void dump_eval();
  void dump_mui_road();
  void dump_mui_eval();
  void dump_mui_mchit();

  bool setup_display();
  void draw_plane(UShort_t arm, UShort_t octant);
  void draw_half_octant_bidim(UShort_t arm, UShort_t octant, UShort_t halfoctant, UShort_t station);
  void draw_octant(UShort_t arm, int octant = -1, int station=-1);
  void draw_side(UShort_t arm, UShort_t octant = 0, int station = -1);
  
  //! display commands
  void help( const char* module_name = "muon_display" );

 protected:
  
  void SetNodePtrs(PHCompositeNode* top_node);
  int CreateNodeTree(PHCompositeNode *topNode);
  bool sanity_check();
  PHCompositeNode* _top_node;
  PhMutooDisplay* _display;
  
  #ifndef __CINT__
  PHTimeServer::timer _timer;
  #endif 
  
  bool _init;
};

#endif /* __MUONDISPAY_H__ */







