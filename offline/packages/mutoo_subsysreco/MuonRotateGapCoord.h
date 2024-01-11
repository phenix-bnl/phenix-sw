// $Id: MuonRotateGapCoord.h,v 1.4 2007/11/30 20:05:33 hpereira Exp $

#ifndef __MUONROTATEGAPCOORD_H__
#define __MUONROTATEGAPCOORD_H__

/*!
  \file    MuonRotateGapCoord.h
  \ingroup supermodules 
  \brief   calculate reaction angle using muon arms run4 AuAu DST  
  \author  Sean Kelly
  \version $Revision: 1.4 $
  \date    $Date: 2007/11/30 20:05:33 $
*/

#include <string>
#include "MuonSubsysReco.h"

// Forward declerations
class PHCompositeNode;
class TMutCoordMap;
class TTree; 

// MUON
#ifndef __CINT__
#include <mMutBPFit.h>
#include <mMutBPVertex.h>
#include <mMutFindClus.h>
#include <mMutFindTrack.h>
#include <mMutFindVtx.h>
#include <mMutFitClus.h>
#include <mMutKalFit.h>
#include <mMutFitVtx.h>
#include <mMutMatchCoord.h>
#include <mMutMuiRoad.h>
#include <mMutRejectTrack.h>
#include <mMutRejectTrackPar.h>
#include <PHTFileServer.h>
#endif


//ROOT

class MuonRotateGapCoord: public MuonSubsysReco
{
 public:
  
  //! constructor
  MuonRotateGapCoord( const char* name = "MUONROTATEGAPCOORD" );
  
  //! destructor
  ~MuonRotateGapCoord();
  
  //! full initialization
  int Init(PHCompositeNode *);
  
  //! full initialization
  int InitRun(PHCompositeNode *);
  
  //! event processing method
  int process_event(PHCompositeNode *);
  
  //! end of run method
  int End(PHCompositeNode *);

  //! Set output filename
  //
  void set_outfile(const char* outfile) { std::string filename(outfile); _out_file = filename;}

 protected:
  
  //! create all mutoo nodes
  int CreateNodeTree(PHCompositeNode *);
  //! rotate TMutCoord by half octant in one station.  
  int rotation_coords_by_half_octant(PHCompositeNode* top_node,unsigned short arm, unsigned station);
  //! swap TMutCoord between two half octant one cathod plane
  void swap_coord_in_half_octant(TMutCoordMap* coord_map,
				 unsigned short arm,
				 unsigned short station,
				 unsigned short gap,
				 unsigned short cathod,
				 unsigned short octant1,
				 unsigned short half_octant1,
				 unsigned short octant2,
				 unsigned short half_octant2); 
  //! setpu output file and TTree.
  bool setup_output(); 

  //! initialize tree leaves
  bool init_tree();

  //! calculate reaction plane and fill TTree
  //
  int do_rotation(PHCompositeNode *);

  // Node tree data members
  //! mutoo working node
  PHCompositeNode *mutoo_node;
  
  //! dst io node
  PHCompositeNode *dst_node;
  
  
  #ifndef __CINT__ 
  
  //! cluster finding module
  mMutFindClus _mMutFindClus_mod;
  
  //! cluster fit module
  mMutFitClus _mMutFitClus_mod;
  
  //! coordinate matching to build gap_coordinates module
  mMutMatchCoord _mMutMatchCoord_mod;

  //! supermodule timer
  PHTimeServer::timer _timer;
  
  #endif
  
  // declear output file, tree and leaves.
  std::string _out_file;

  TTree* _anaTree;
  
  // event/run info
  unsigned int Tevt;
  unsigned int Trun;
  float Tcent;
  float Tzvtx; 
  
  // locations
  unsigned short Tarm;
  unsigned short Tsta;
  unsigned short Tgap;
  
  // hits stats
  unsigned int Tnhits;
  
  // reaction plane angles
  float Tphi1;   // plane with the directed moment.
  float Tphi2;   // plane with the elliptic moment.
};

#endif /* __MUONREACTIONPLANE_H__ */
