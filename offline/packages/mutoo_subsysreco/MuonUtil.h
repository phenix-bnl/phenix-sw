// $Id: MuonUtil.h,v 1.30 2009/10/03 18:50:30 kempel Exp $

/*!
  \file   MuonUtil.h
  \brief  utility methods for mutoo supermodules
  \author  Hugo Pereira
  \version $Revision: 1.30 $
  \date    $Date: 2009/10/03 18:50:30 $
*/

#ifndef MuonUtil_h
#define MuonUtil_h

#include <Fun4AllServer.h>
#include <PHCompositeNode.h>
#include <string>
#include <mMuiFastRoadFinderPar.h>
#include <mMuiBLTEmulator.h>

//#ifndef __CINT__
#include <TMutNode.h>
//#endif

/*!
  \class MuonUtil
  \brief utility methods for mutoo supermodules
*/
class MuonUtil
{
  public:

  //! tell if mapfile_scale must be sign changed to match magnet currents
  static void set_check_mapfile_scale( const bool& value )
  { _check_mapfile_scale = value; }

  //! check that recoConsts timeStamp and run-number are set properly
  static void check_timestamp( void );

  enum InitializationFlags
  {
    NONE = 0,
    GEANT = 1<<0,
    MAGNETIC_FIELD = 1<<1,
    MUID = 1<<2,
    MUTR = 1<<3,
    ALL = GEANT|MAGNETIC_FIELD|MUID|MUTR
  };

  //! initialize mutoo/muioo database
  static void initialize_database(
    PHCompositeNode* top_node,
    bool do_mut,
    bool do_mui )
  {
    unsigned int flag = GEANT|MAGNETIC_FIELD;
    if( do_mut ) flag |= MUTR;
    if( do_mui ) flag |= MUID;
    initialize_database( top_node, flag );
  }

  //! initialize mutoo/muioo database
  static void initialize_database( PHCompositeNode* , const unsigned int& flag = ALL );

  //! magnetic field initialization
  static void initialize_magnetic_field( PHCompositeNode* );

  //! geant initialization
  static void initialize_geant( PHCompositeNode* );

  //! dump processor time for this process to given stream
  static void dump_process_time( void );

  #ifndef __CINT__
  //! dump processor time for this process to given stream
  static void dump_process_time( std::ostream& out );
  #endif

  //! retrieve level2 decision for a given trigger
  static bool get_l2_trigger_decision( PHCompositeNode*, const std::string& algo_name );

  //!@name Muid LL1 utilities
  //@{

  //! retrieve 1Shallow LL1 emulator decision for the given arm
  static bool get_1S_LL1trigger_decision( PHCompositeNode* top_node, const unsigned int& arm = 0 )
  { return get_LL1trigger_decision( top_node, arm, mMuiFastRoadFinderPar::Shallow ); }

  //! retrieve 1Deep LL1 emulator decision for the given arm
  static bool get_1D_LL1trigger_decision( PHCompositeNode* top_node, const unsigned int& arm = 0 )
  { return get_LL1trigger_decision( top_node, arm, mMuiFastRoadFinderPar::Deep ); }

  //! retrieve Deep-Deep LL1 emulator decision for the given arm
  static bool get_2D_LL1trigger_decision( PHCompositeNode* top_node, const unsigned int& arm = 0 )
  { return get_LL1trigger_decision( top_node, arm, mMuiFastRoadFinderPar::DeepDeep ); }

  //! retrieve Deep-Shallow LL1 emulator decision for the given arm // AR : added
  static bool get_1D1S_LL1trigger_decision( PHCompositeNode* top_node, const unsigned int& arm = 0)
  { return get_LL1trigger_decision( top_node, arm, mMuiFastRoadFinderPar::DeepShallow ); }

  //! retrieve LL1 emulator decision for a given trigger mode
  static bool get_LL1trigger_decision( PHCompositeNode* , const unsigned int& arm, const mMuiFastRoadFinderPar::Mode& mode );

  //@}

  //!@name Muid BLT utilities
  //@{

  //! retrieve 1Shallow BLT emulator decision for the given arm
  static bool get_1S_BLTtrigger_decision( PHCompositeNode* top_node, const unsigned int& arm = 0, const bool& use_reco = false )
  { return get_BLTtrigger_decision( top_node, arm, mMuiBLTEmulator::SINGLE_SHALLOW, use_reco ); }

  //! retrieve 1Deep BLT emulator decision for the given arm
  static bool get_1D_BLTtrigger_decision( PHCompositeNode* top_node, const unsigned int& arm = 0, const bool& use_reco = false )
  { return get_BLTtrigger_decision( top_node, arm, mMuiBLTEmulator::SINGLE_DEEP, use_reco ); }

  //! retrieve Deep-Deep BLT emulator decision for the given arm
  static bool get_2D_BLTtrigger_decision( PHCompositeNode* top_node, const unsigned int& arm = 0, const bool& use_reco = false )
  { return get_BLTtrigger_decision( top_node, arm, mMuiBLTEmulator::DEEP_DEEP, use_reco ); }

  //! retrieve Deep-Shallow BLT emulator decision for the given arm // AR : added
  static bool get_1D1S_BLTtrigger_decision( PHCompositeNode* top_node, const unsigned int& arm = 0, const bool& use_reco = false )
  { return get_BLTtrigger_decision( top_node, arm, mMuiBLTEmulator::DEEP_SHALLOW, use_reco ); }

  //! retrieve BLT emulator decision
  static bool get_BLTtrigger_decision( PHCompositeNode* , const unsigned int& arm, const mMuiBLTEmulator::Mode& mode, const bool& use_reco );

  //@}

  //! returns South+North bbc charge as taken from BBCOut node from top_node
  static double get_bbc_charge( PHCompositeNode* );

  //! returns bbc charge as taken from BBCOut node from top_node
  static double get_bbc_z( PHCompositeNode* );

  //! get (generic) centrality
  /*! insert code here to handle runs for which centrality has different definitions */
  static double get_centrality( PHCompositeNode* );

  //! check vertex matching
  static void check_vertex_matching( PHCompositeNode* top_node )
  { check_vertex_matching( top_node, top_node, top_node ); }

  //! check vertex matching
  static void check_vertex_matching( PHCompositeNode* signal_node, PHCompositeNode* background_node, PHCompositeNode* top_node );

  //! dump event to screen (local event number, bbc Z vertex, run5 centrality)
  static void dump_evtN_bbcZ_run5centrality( PHCompositeNode* , int fraction = 1 );

  #ifndef __CINT__

  //! convenience function that loads a node from any of the topnode registered by Fun4All
  template < typename T > inline static T* find_node( const std::string& name );

  //! convenience function that loads a node from any of the topnode registered by Fun4All
  template < typename T > inline static T* find_io_node( const std::string& name );

  #endif

  private:

  //! get run4 centrality
  static double _get_run3_centrality( PHCompositeNode* );

  //! get run4 centrality
  static double _get_run4_centrality( PHCompositeNode* );

  //! get generic centrality
  static double _get_generic_centrality( PHCompositeNode* );

  //! static initialization.
  /*! allows to display mesages at library loading stage */
  static bool _init( void );

  //! perform mut database initialization
  static void _initialize_mut_database( PHCompositeNode* );

  //! perform mui database initialization
  static void _initialize_mui_database( PHCompositeNode* );

  //! check mut geometry
  static void _check_mut_geometry( void );

  //! print station wires
  static void _print_mut_st3_wires( void );

  //! print dead strips
  static void _print_dead_strips( void );

  //! decide if map file scale needs to be sign changed to match magnet current
  static bool _check_mapfile_scale;

  //! allow to display message at library loaded
  static bool _loaded;


};


#ifndef __CINT__

//_________________________________________________________________________________________
template < typename T > T* MuonUtil::find_node( const std::string& name )
{
  std::vector<std::string> topNode_names;
  Fun4AllServer::instance()->GetTopNodes( topNode_names );
  T* out(0);
  for( std::vector<std::string>::iterator iter = topNode_names.begin(); iter != topNode_names.end(); iter++ )
  {

    try {

      out = TMutNode<T>::find_node( Fun4AllServer::instance()->topNode( *iter ), name );

    } catch( std::exception& e ) { out = 0; }

    if( out )
    {
      //std::cout << "MuonUtil::find_node - loaded map named " << name << " from node " << *iter << std::endl;
      return out;
    }

  }

  std::string failure("find_node failed: " + name);
  throw std::runtime_error(DESCRIPTION(failure.c_str()));
  return 0;

}

//_________________________________________________________________________________________
template < typename T > T* MuonUtil::find_io_node( const std::string& name )
{
  std::vector<std::string> topNode_names;
  Fun4AllServer::instance()->GetTopNodes( topNode_names );
  T* out(0);
  for( std::vector<std::string>::iterator iter = topNode_names.begin(); iter != topNode_names.end(); iter++ )
  {

    try {

      out = TMutNode<T>::find_io_node( Fun4AllServer::instance()->topNode( *iter ), name );

    } catch( std::exception& e ) { out = 0; }

    if( out )
    {
      //std::cout << "MuonUtil::find_node - loaded map named " << name << " from node " << *iter << std::endl;
      return out;
    }

  }

  std::string failure("find_node failed: " + name);
  throw std::runtime_error(DESCRIPTION(failure.c_str()));
  return 0;

}
#endif

#endif
