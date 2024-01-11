// $Id: MuonUnpackSlowSimDst.h,v 1.3 2007/12/10 12:59:51 hpereira Exp $

#ifndef __MUONUNPACKSLOWSIMDST_H__
#define __MUONUNPACKSLOWSIMDST_H__

#include <string>
#include "MuonSubsysReco.h"

#include <TDataType.h>
/*!
  \file    MuonUnpackSlowSimDst.h  
  \ingroup supermodules
  \brief   reads muid/mutr mc hit/track maps from a simulated DST 
  runs the response to build muid/mutr hit maps for later reconstruction.
  \author  Sean Kelly
  \version $Revision: 1.3 $
  \date    $Date: 2007/12/10 12:59:51 $
*/

// Forward declerations
class PHCompositeNode;
class TTree;

#ifndef __CINT__
#include <PHTimeServer.h>
#endif

/*!
  \class   MuonUnpackSlowSimDst 
  \ingroup supermodules
  \brief   reads muid/mutr mc hit/track maps from a simulated DST 
  runs the response to build muid/mutr hit maps for later reconstruction.
*/
class MuonUnpackSlowSimDst: public MuonSubsysReco
{
 public:

  enum { MAX_HIT = 100000, MAX_TRACK = 50000};

  //! constructor
  MuonUnpackSlowSimDst( const char* name = "MUONUNPACKSLOWSIMDST" );

  //! run initialization
  int InitRun(PHCompositeNode *topNode);
  
  //! event processing
  int process_event(PHCompositeNode *topNode);
   
  //! end of process
  int End(PHCompositeNode *topNode);
  // set up output file
  //
  void set_outfile(std::string file) 
  {
    _outfile = file;
  }

 protected:
 
  //! setup out file tree structure
  //
  void setup_output();

  // ! fill mutr pisa hit tree
  void fill_mutr_pisahit_tree();

  // ! fill primary particle tree
  void fill_primary_tree();

  // ! reset pisa hit tree branch;
  //
  void reset_pisahit_tree();

  // ! reset pisa hit tree branch;
  //
  void reset_primary_tree();

  //! create all new nodes
  int CreateNodeTree(PHCompositeNode *topNode);

  // Node for merged hits
  //! mutoo merged
  PHCompositeNode* _mutoo_node; 
  
  //! muioo merged
  PHCompositeNode* _muioo_node; 

  #ifndef __CINT__
  //! module timer
  PHTimeServer::timer _timer;
  #endif
  
  //! outfile
  std::string _outfile;


  //! MuTr pisa hit tree
  TTree* _mutr_pisahit_tree;
  // event information.
  Float_t         zcoll;  // collision vertex.
  Float_t         b;      // impact parameter.
  Int_t           ncoll;   // binary collisions
  Int_t           nhit;   // number of hits per arm, per station, per gap.
  Int_t           arm[MAX_HIT];    // arm.
  Int_t           station[MAX_HIT]; // station.
  Int_t           gap[MAX_HIT];     // gap.
  Float_t         AptG[MAX_HIT]; // pT of the mc track associated with a pisa hit.
  Float_t         ApzG[MAX_HIT]; // pZ of the mc track associated with a pisa hit.
  Float_t         Ar[MAX_HIT];   // tranverse position of a pisa hit.  
  Float_t         Az[MAX_HIT];   // z coordinate of a pisa hit.
  Float_t         Aphi[MAX_HIT]; // azimuthal angle of a pisa hit.
  Float_t         Arvtx[MAX_HIT]; // associated mc track transverse vertex
  Float_t         Azvtx[MAX_HIT]; // associated mc track vertex
  Float_t         Aphivtx[MAX_HIT]; // associated mc track azimuthal  vertex
  Int_t           Aidpart[MAX_HIT]; // associated mc track particle id
  Int_t           Agen[MAX_HIT];    // generation : 0 means primary, 1 means secondary.

  Int_t           Apridpart[MAX_HIT]; // associated primary track particle id.
  Float_t         Aprphi[MAX_HIT];    // associated primary track azimuthal angle.
  Float_t         Aprthe[MAX_HIT];    // associated primary track polar angle.
  Float_t         Aprpt[MAX_HIT];     // associated primary track pT.
  Float_t         Aprpz[MAX_HIT];     // associated primary track pZ

  //! primary particle tree.
  TTree* _primary_tree;

  Int_t           ntrk;                 // number of primary tracks
  Int_t           Ppridpart[MAX_TRACK]; // associated primary track particle id.
  Float_t         Pprphi[MAX_TRACK];    // associated primary track azimuthal angle.
  Float_t         Pprthe[MAX_TRACK];    // associated primary track polar angle.
  Float_t         Pprpt[MAX_TRACK];     // associated primary track pT.
  Float_t         Pprpz[MAX_TRACK];     // associated primary track pZ

};

#endif /* __MUONUNPACKSLOWSIMDST_H__ */







