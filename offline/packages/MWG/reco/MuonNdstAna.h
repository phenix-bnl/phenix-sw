// $Id: MuonNdstAna.h,v 1.1 2009/07/04 18:33:52 hpereira Exp $
#ifndef __MUONNDSTANA_H__
#define __MUONNDSTANA_H__

/*!
  \file    MuonNdstAna.h
  \ingroup supermodules 
  \brief   nanoDST analysis loop to read nanoDSTs, fill in ntuples/trees
  \author  Sean Kelly
  \version $Revision: 1.1 $
  \date    $Date: 2009/07/04 18:33:52 $
*/

#include <string>
#include "SubsysReco.h"

// ROOT header.
//
#include <TTree.h>
#include <TH1.h>
#include <TH2.h>

// declerations for phenix classes.
//
#include "PHCompositeNode.h"
#include "PHGlobal.h"
#include "PHMuoTracksOut.h"
#include "TMutTrkPar.hh" 

#ifndef __CINT__
#include <PHTimeServer.h>
#endif

/*!
  \class   MuonNdstAna
  \ingroup supermodules 
  \brief   nanoDST analysis loop to read nanoDSTs, fill in ntuples/trees
*/
class MuonNdstAna: public SubsysReco
{
 public:

  enum COUNTER {ALL, PASSED};
  MuonNdstAna( const char *name = "MUONNDSTANA", const char* file = "mwg_ntuple.root" );

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

  // cut setter and getter
  //
  void set_trk_chi_square_cut(Double_t cut) { _trk_chi_square_cut = cut;}
  void set_vtx_chi_square_cut(Double_t cut) { _vtx_chi_square_cut = cut;}
  void set_vtx_z_cut(Double_t cut) { _vtx_z_cut = cut;}
  void set_hadron_depth_cut(Int_t cut) { _hadron_depth_cut = cut;}
  void set_hadron_ptot_vertex_cut(Double_t cut) { _hadron_ptot_vertex_cut = cut;}
  void set_hadron_ptot_sta1_cut(Double_t cut) { _hadron_ptot_sta1_cut = cut;}

  Double_t get_trk_chi_square_cut() const { return _trk_chi_square_cut;}
  Double_t get_vtx_chi_square_cut() const { return _vtx_chi_square_cut;}
  Double_t get_vtx_z_cut() const { return _vtx_z_cut;}
  Int_t get_hadron_depth_cut() const { return _hadron_depth_cut;}
  Double_t get_hadron_ptot_vertex_cut() const { return _hadron_ptot_vertex_cut;}
  Double_t get_hadron_ptot_sta1_cut() const { return _hadron_ptot_sta1_cut;}
  
  //! Output ntuple file.
  void set_out_file(const char* file)
	{ if( file ) _filename = file;}
	
	//! output ntuple file
	void set_filename( const char* file )
	{ if( file ) _filename = file;}
	
  //! trig selection, 0 : minibais; 1 : Muid1D 
  void set_trigger(int trig) { _trig_selection = trig;}

 protected:
  
  void set_node_ptrs(PHCompositeNode* top_node);
  void fill_single_track(PHCompositeNode* top_node);

  Int_t get_max_depth(int trk_index) const;
  Double_t get_chi_square(int trk_index) const;
  Double_t get_phi (float px, float py) const;
  Double_t get_theta(float px, float py, float pz) const;
  Double_t get_eta(float px, float py, float pz) const;
  Double_t hadron_theta_xp(int index) const ;

  void init_single_tree() {
    _p[0] = 0;
    _p[1] = 0;
    _p[2] = 0;
    _p[3] = 0;
    _p[4] = 0;
    _phi  = 0;
    _theta= 0;
    _eta  = 0;
    _chi_square = 0;
    _theta_xp = 0;
    _z_vtx = 0;
    _cent = 0;
    _depth = 0;
    _pid = 0;
  }

#ifndef __CINT__
  bool initialize();
  bool pass_zvtx_cut();
  bool pass_trk_chi_square_cut(int trk_index);
  bool pass_vtx_chi_square_cut(const float vtx_chi);
  TMutTrkPar propagate_hadron(int trk_index) const; 
  bool is_hadron(int trk_index) const ;
#endif

  void increment( COUNTER input ) 
	{ 
    if(input==ALL) _ievt++;
    if(input==PASSED) _npassed++;
  }
	
  // data members
  PHMuoTracksOut* _mutoo;
  PHGlobal* _global;
	
	//! root output file
  std::string _filename;

  // Single track tree.
  TTree* _single_track;
	
  /*! Single track momentum, 0 : px; 1 : py; 2 : pz; 3 : pt; 4 : ptot. */
  Double_t _p[5];
	
  /*! Single track eta */
  Double_t _eta;
	
  /*! Sinlge track phi */
  Double_t _phi;
	
  /*! Single track theta */
  Double_t _theta;
	
  /*! Single track chi_square */
  Double_t _chi_square;
	
  /*! depth of the track */
  Int_t _depth;
	
  /*! Single track primary theta */
  Double_t _theta_xp;
	
  /*! Single track id, 0 : muon; 1 : hadron. */
  Int_t _pid;
	
  /*! Single track charge. */
  Int_t _charge;
	
  /*! Event vertex */
  Double_t _z_vtx;
  /*! Centrality */
  Double_t _cent;

  Double_t _trk_chi_square_cut;
  Double_t _vtx_chi_square_cut;
  Double_t _vtx_z_cut;
  Int_t _hadron_depth_cut;
  Double_t _hadron_ptot_vertex_cut;
  Double_t _hadron_ptot_sta1_cut;
  
  //! number of event has been analized.
  long _ievt;
  Int_t _npassed;

  //! trigger selection
  int _trig_selection;

  // Some Histograms for QA.
  TH1F* _hntracks;
  TH1F* _hnhadrons;

  TH1F* _hcent;
  TH1F* _hzvtx;
  TH1F* _hphi;
  TH2F* _hgap0xy;
  TH2F* _hst1xy;

  TH1F* _hcent_hadron;
  TH1F* _hzvtx_hadron;
  TH1F* _hphi_hadron;
  TH2F* _hgap0xy_hadron;
  TH2F* _hst1xy_hadron;

  #ifndef __CINT__
  PHTimeServer::timer _timer;
  #endif
  
};

#endif /* __MUONNDSTANA_H__ */






