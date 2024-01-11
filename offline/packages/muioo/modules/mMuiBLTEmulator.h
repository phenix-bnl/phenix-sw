// $Id: mMuiBLTEmulator.h,v 1.6 2009/06/02 23:14:26 hpereira Exp $
#ifndef __MMUIBLTEMULATOR_HH__
#define __MMUIBLTEMULATOR_HH__

/*!
  \file    mMuiBLTEmulator.h
  \brief   muid pseudo BLT emulator
  \author  C. Zhang
  \version $Revision: 1.6 $
  \date    $Date: 2009/06/02 23:14:26 $
*/

#ifndef __CINT__
#include <PHTimeServer.h>
#include <PHIODataNode.h>
#include <PHNodeIterator.h>
#include <PHTypedNodeIterator.h>
#include <Event.h>

#include "TMuiPseudoBLTMapO.h"
#include "TMuiHitMapO.h"
#include <boost/array.hpp>
#include "MUIOO.h"
#include "mMuiBLTEmulatorPar.h"
#endif



/*! \ingroup modules */
/*! muid pseudo BLT (blue logic trigger) emulator
<h2>Analysis Module Interface Specification</h2>
<table>
<tr>
<td> Object </td>
<td> Description
 :: this is a translation from Hiroki's emulator ( offline/packages/mui/mMuiPseudoTrigger),
 I (chun zhang) am trying to keep it as untouched as possible, ie. do not blame me for anything.
</td>
<td> Privilege </td>
</tr>
<tr>
<td> const mMuiBLTEmulatorPar*</td>
<td> Parameter Table </td>
<td> immutable </td>
</tr>
<tr>
<td> TMuiHitMapO*</td>
<td> Parameter Table </td>
<td> immutable </td>
</tr>
<tr>
<td> TMuiPseudoBLTMapO*</td>
<td> Parameter Table </td>
<td> immutable </td>
</tr>

</table>
*/

//! MUID/MUI Blue-Logic Trigger emulator
class mMuiBLTEmulator
{
 public:

  enum
  {
    max_arm =2,
    max_quad = 4,
    max_plane = 4,
    max_orientation = 2,

    mlu_address_max = 4096,
    max_fem = 2,
    word_per_fem = 120
  };

  //! road depth enumeration
  enum RoadDepth
  {
    SHALLOW_ROAD = 1,
    DEEP_ROAD = 2
  };

  //! trigger mode
  enum Mode {

    NONE = 0,
    SINGLE_SHALLOW = 1<<0,
    SHALLOW_SHALLOW = 1<<1,
    SINGLE_DEEP = 1<<2,
    DEEP_SHALLOW = 1<<3,
    DEEP_DEEP = 1<<4

  };

  //! constructor
  mMuiBLTEmulator();

  //! destructor
  virtual ~mMuiBLTEmulator();

  //! called only at the first event
  void initialize(int runno);

  //! event method
  virtual PHBoolean event(PHCompositeNode*);
  
  //! print summary
  void print_summary( std::ostream &out = std::cout );

  private:

  //! fill "word" array from MuID packet
  void get_raw_data();

  //! fill "word' array from hit map.
  void get_hits();

  //! convert "word" array into pseudo-trigger output of each ROC
  void raw_to_trigpattern();

  //! return BLT trigger bits.
  int decision_event( const unsigned int& arm);

  //! "Decision MLU" emulator
  unsigned int decision_mlu( const unsigned int& arm) const;
  
  #ifndef __CINT__
  
  //! read MLU data from a text file
  typedef boost::array<unsigned int, mlu_address_max> mlu_array;
  bool read_mlu_data( mlu_array mlu_data_d, std::string mlu_data_file);

  //! make Deep or Shallow decision on one quadrant.
  bool quad_fire( unsigned int* quad_trig_pattern, mlu_array mlu_data) const;

  #endif
  
  //! set up interface pointer
  void set_interface_ptrs(PHCompositeNode* top_node);

  //! fill pseudo-BLT map.
  void fill_map();

  //! get not used palne
  int get_non_used_plane()
  { return _non_used_plane;}

  //! set not used palne
  void set_non_used_plane( const unsigned int& plane )
  { _non_used_plane = plane;}

  //! Cross check from hit objects.
  void check_blt();

  //! dump reco blt from TMuiHitMapO
  void dump_reco_blt( const unsigned int& arm)  const;

  //! decide 2D trigger from TMuiHitO.
  bool is_reco_2D( const unsigned int& arm) const;

  //! decide 1D1S trigger from TMuiHitO.
  bool is_reco_1D1S( const unsigned int& arm) const;

  //! decide 1D trigger from TMuiHitO.
  bool is_reco_1D( const unsigned int& arm, const unsigned int& quad) const
  { return get_fired_quad( arm, quad ) >= 7; }

  //! decide 1S trigger from TMuiHitO.
  bool is_reco_1S( const unsigned int& arm, const unsigned int& quad) const
  { return get_fired_quad( arm, quad ) >= 3; }

  //! decide which quadrant the TMuiHitO is in.
  unsigned int get_quad( const float& x, const float& y ) const;

  //! return number of fired planes in given quadrant
  unsigned int get_fired_quad( const unsigned int& arm, const unsigned int& quad ) const;
  
  #ifndef __CINT__
  //! module parameters
  const mMuiBLTEmulatorPar* _mod_par;
  
  //! hit map
  TMuiHitMapO* _hit_map;
  
  //! pseudo blt decision map
  TMuiPseudoBLTMapO* _blt_map;
  
  //! event structure
  Event* _event;

  #ifndef __CINT__
  //! MLU data for shallow roads
  mlu_array mlu_data_s;
  
  //! MLU data for deep roads
  mlu_array mlu_data_d;
  #endif

  #endif
  
  //! MLU data file for shallow roads
  long word[max_arm][max_fem][word_per_fem];

  //! This plane is excluded from the trigger decision
  unsigned int _non_used_plane;

  //! bit pattern of pseudo-trigger output for each plane of each quadrant
  unsigned int trig_pattern[max_arm][max_quad][max_plane];

  //! trigger state for each quadrant (Deep or shallow)
  unsigned int trig_accept[max_arm][max_quad];

  //! store arm statistics
  /*! uses a class instead of a typedef to have a default constructor */
  #ifndef __CINT__
  class arm_pair: public boost::array<int, max_arm >
  {
    public:

    // constructor
    arm_pair( void )
    { assign(0); }

  };

  //! store quadrant statistics
  typedef boost::array< arm_pair, max_quad > quad_pair;

  //! number of 2D triggers (from sim)
  arm_pair _n_2D_sim;

  //! number of 1D1S triggers (from sim)
  arm_pair _n_1D1S_sim;

  //! number of 1D triggers/panel (from sim)
  quad_pair _n_1D_sim;

  //! number of 1S triggers/panel (from sim)
  quad_pair _n_1S_sim;

  //! number of 2D triggers (from reco)
  arm_pair _n_2D_reco;

  //! number of 1D1S triggers (from reco)
  arm_pair _n_1D1S_reco;

  //! number of 1D triggers/panel (from reco)
  quad_pair _n_1D_reco;

  //! number of 1S triggers/panel (from reco)
  quad_pair _n_1S_reco;
  
  //! Timer
  PHTimeServer::timer _timer;
  #endif


};

#endif /* __MMUIBLTEMULATOR_HH__ */
