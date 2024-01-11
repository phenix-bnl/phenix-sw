// $Id: FvtxRecoWithMut.h,v 1.10 2015/06/11 21:02:58 snowball Exp $
#ifndef __FvtxRecoWithMut_h__
#define __FvtxRecoWithMut_h__

/*!
	\file		FvtxRecoWithMut.h	
	\ingroup supermodules
	\brief	 Reconstructs muon + silicon tracks
	\author	Melynda Brooks
	\version $Revision: 1.10 $
	\date		$Date: 2015/06/11 21:02:58 $
*/

#include <string>
#include <map>
#include <MuonSubsysReco.h>
#ifndef __CINT__
#include <boost/shared_ptr.hpp>
#endif // __CINT__

// Forward declerations
class PHCompositeNode;
class PHTimer;
class mMutKalFitWithSili;
class mMutKalFitWithSiliReal;
class mFvtxFindHoughTracks;
class mFvtxFindTrackPar;

//! Reconstruction of Mut tracks with silicon
/*!
	\ingroup supermodules
	Forward vertex reconstruction module. 
	Reads TFvtxHits from DST, create clusters coordinates and tracks
*/
class FvtxRecoWithMut: public MuonSubsysReco
{
public:

  //! constructor
  FvtxRecoWithMut();

  //! destructor
  ~FvtxRecoWithMut();

  //! initialization
  int Init(PHCompositeNode *topNode);
	
  //! run initialization
  int InitRun(PHCompositeNode *topNode);

  //! event processing
  int process_event(PHCompositeNode *topNode);
	
  //! end of process
  int End(PHCompositeNode *topNode);
 
//  //! proximity cut value for matching MuTr track to Fvtx track
//  const Double_t& get_fvtx_mutr_proximity_cut( void ) const
//  { return _fvtx_mutr_proximity_cut; }


  //! proximity cut value for matching MuTr track to Fvtx track
  //! track will be accepted if proximity smaller than this value;
  //! otherwise, it will be compared with a second cut as defined
  //! in mMutKalFitWithSiliReal::get_fvtx_mutr_match_cut();
  //!
  //! This type of function is obsolete. One can use in future
  //!  TMutNode<mMutKalFitWithSiliRealPar>::find_node(se->topNode(),"mMutKalFitWithSiliRealPar")->set_fvtx_mutr_proximity_cut(...);
  void set_fvtx_mutr_proximity_cut( const Double_t& value );

  //! make mMutKalFitWithSili evaluation ntuple
  //!
  //! This type of function is obsolete. One can use in future
  //!  TMutNode<mMutKalFitWithSiliRealPar>::find_node(se->topNode(),"mMutKalFitWithSiliRealPar")->set_do_evaluation(true);
  void set_do_mutkalfiteval(bool a) ;


protected:

  static void obsolete_warning(const std::string & function, const std::string & suggestion);

  bool _do_mutkalfiteval;
	
  //! proximity cut value for matching MuTr track to Fvtx track
  //! track will be accepted if proximity smaller than this value;
  //! otherwise, it will be compared with a second cut as defined
  //! in mMutKalFitWithSiliReal::get_fvtx_mutr_match_cut();
//  Double_t _fvtx_mutr_proximity_cut;

  //! create all new nodes
  int create_node_tree(PHCompositeNode *topNode);

  //! retrive parameter table and print to cout
  //! one can also reset run specific parameters here
  int check_parameters(PHCompositeNode *topNode);

  //! rpc working node
  PHCompositeNode* _fvtx_node;
 
  // RPC module data members
  
  //! Fit Mutr+FVTX hits (MC)
  mMutKalFitWithSili* _mMutKalFitWithSili_mod;
  
  //! Fit Mutr+FVTX hits (real tracks)
  mMutKalFitWithSiliReal* _mMutKalFitWithSiliReal_mod;

  //! Find two hit tracks with precise vertex
  //#ifdef __CINT__
  mFvtxFindHoughTracks* _mFvtxFindHoughTracks_mod;
  //#else
  //  boost::shared_ptr<mFvtxFindHoughTracks> _mFvtxFindHoughTracks_mod;
  //#endif

  //! Parameters for the Hough track finding
  mFvtxFindTrackPar * _track_mod_par;
	
  //! module timer
  PHTimer* _timer;
	
};

#endif 
