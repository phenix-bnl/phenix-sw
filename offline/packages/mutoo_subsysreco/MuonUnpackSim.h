#ifndef __MUONUNPACKSIM_H__
#define __MUONUNPACKSIM_H__

#include <string>
#include "MuonSubsysReco.h"

/*!
  \file    MuonUnpackSim.h
  \ingroup supermodules
  \brief   reads muid/mutr mc hit/track maps from a simulated DST
  runs the response to build muid/mutr hit maps for later reconstruction.
  \author  Sean Kelly
  \version $Revision: 1.35 $
  \date    $Date: 2019/04/03 21:13:12 $
*/

// Forward declerations
//

class PHCompositeNode;

// MUON

#ifndef __CINT__
#include <mMutResponse.h>
#include <mMutCalibrate.h>
#include <mMuiResponse.h>
#include <mMuiEmbed.h>
#include <mMutEmbed.h>
#include <mMutZeroSup.h>
#include <PHTimeServer.h>
#endif

/*!
  \class   MuonUnpackSim
  \ingroup supermodules
  \brief   reads muid/mutr mc hit/track maps from a simulated DST
  runs the response to build muid/mutr hit maps for later reconstruction.
*/
class MuonUnpackSim: public MuonSubsysReco
{
 public:

  //! embedding mode
  enum Mode {

    //! embed MC into real data background
    MC_SIGNAL_REAL_BG,

    //! embed MC into MC background
    MC_SIGNAL_MC_BG,

    //! no background
    MC_SIGNAL_NO_BG,

    //! embed real signal into real data background
    REAL_SIGNAL_REAL_BG

  };

  //! vertex to use in reconstruction
  enum vtx_to_use {

    //! vertex from signal file
    SIGNAL,

    //! vertex from background file
    BACKGROUND,

    //! reconstructed vertex
    RECO,

  };

  //! configuration flags
  enum Flag
  {

    //! no flags
    NONE = 0,

    //! skip Muioo simulations
    SKIP_MUIOO = (1<<0),

    //! skip mutoo simulations
    SKIP_MUTOO = (1<<1),

    //! skipp offline zero suppression
    NO_ZERO_SUP = (1<<2),

    //! skip calibration
    NO_CALIBRATE = (1<<3),

    //! skip charge smearing
    NO_CHARGE_SMEAR = (1<<4),

    //! skip additional RMS scale
    NO_RMS_SCALE = (1<<5),

    //! skip writing of the MC primaries
    NO_MC_PRIMARY = (1<<6)

  };

  //! constructor
  MuonUnpackSim( const char* name = "MUONUNPACKSIM", unsigned int mode=MC_SIGNAL_NO_BG, unsigned int vtx_to_use=SIGNAL);

  //! destructor
  ~MuonUnpackSim( void );

  //! run initialization
  int Init(PHCompositeNode*);

  //! run initialization
  int InitRun(PHCompositeNode*);

  //! event processing
  int process_event(PHCompositeNode*);

  //! end of process
  int End(PHCompositeNode*);

  //! changes embedding mode
  void SetMode(unsigned int);

  //! embedding mode
  unsigned int GetMode() const
  { return _mode; }

  //! changes vertex used in reconstruction
  void SetVtxToUse(unsigned int);

  //! vertex used in reconstruction
  unsigned int GetVtxToUse() const
  { return _vtx_to_use; }

  //! "signal" top node name
  void SetSignalNodeName( std::string value )
  { _signalNodeName = value; }

  //! "background" top node name
  void SetBackgroundNodeName( std::string value )
  { _backgroundNodeName = value; }

  //! flags
  void set_flags( const unsigned int& value )
  { _flags = value; }

  //! flags
  void set_flag( const Flag& flag, const bool& value )
  {
    if( value ) _flags |= flag;
    else _flags &= (~flag);
  }

  //! flags
  bool get_flag( const Flag& flag ) const
  { return _flags & flag; }

  /*! Chamber efficiency (0-1.0) */
  void set_mutr_base_efficiency( const double& value)
  {
    for(int iarm=0;iarm<MUTOO::NumberOfArms;iarm++)
      for(int ista=0;ista<MUTOO::NumberOfStations;ista++)
	for(int igap=0;igap<MUTOO::NumberOfGaps;igap++)
	  for(int pla=0;pla<MUTOO::NumberOfPlanes;pla++)
	    for (int oct=0; oct<MUTOO::MAX_OCTANT; oct++)
	      for (int hoct=0; hoct<MUTOO::MAX_HALF_OCTANT; hoct++)
		{
		  set_mutr_base_efficiency(iarm,ista,igap,pla,oct,hoct,value);
		}
  }

  /*! Chamber efficiency (0-1.0) */
  void set_mutr_base_efficiency( const int& arm, const double& value)
  {
    for(int ista=0;ista<MUTOO::NumberOfStations;ista++)
      for(int igap=0;igap<MUTOO::NumberOfGaps;igap++)
	for(int pla=0;pla<MUTOO::NumberOfPlanes;pla++)
	    for (int oct=0; oct<MUTOO::MAX_OCTANT; oct++)
	      for (int hoct=0; hoct<MUTOO::MAX_HALF_OCTANT; hoct++)
	  {
	    set_mutr_base_efficiency(arm,ista,igap,pla,oct,hoct,value);
	  }
  }
  
  /*! Chamber efficiency (0-1.0) */
  void set_mutr_base_efficiency( const int& arm, const int& station, const int& gap, const int& pla, const int& oct, const int& hoct, const double& value )
  { 
    int index = pla + MUTOO::NumberOfPlanes*(hoct + MUTOO::MAX_HALF_OCTANT *(oct + MUTOO::MAX_OCTANT *(gap + MUTOO::NumberOfGaps*( station + MUTOO::NumberOfStations* arm ))));
_mutr_base_efficiency[index] = value; }

  void set_bbcrate_dependent_efficiency( const std::string file );

  protected:

  //! stores node in internal pointers for each event
  void SetNodePtrs(PHCompositeNode*);

  //! create all new nodes
  int CreateNodeTree(PHCompositeNode*);

  //! obtain bbcrate for this run from DB
  float get_bbcrate(PHCompositeNode* top_node) const;

  //*! @name Nodes for merged hits
  //@{

  //! signal top node name
  std::string _signalNodeName;

  //! background top node name
  std::string _backgroundNodeName;

  //! mutoo merged
  PHCompositeNode* _mutoo_node;

  //! muioo merged
  PHCompositeNode* _muioo_node;

  // Nodes for input signal
  //! signal node for MC DST
  PHCompositeNode* _signal_node;

  //! background node for MC/RD DST
  PHCompositeNode* _ioc_signal_node;

  //! internal signal node (for response)
  PHCompositeNode* _background_node;

  //! internal background node
  PHCompositeNode* _ioc_background_node;

  //@}

  #ifndef __CINT__

  //!@name reconstruction modules
  //@{

  //! mutr response module (MCHit->Hit)
  mMutResponse _mMutResponse_mod;

  //! mutr hit calibration
  mMutCalibrate _mMutCalibrate_mod;

  //! mutr zero suppression
  mMutZeroSup _mMutZeroSup_mod;

  //! muid response
  mMuiResponse _mMuiResponse_mod;

  //! embeding module (merge hits from different input files)
  mMuiEmbed _mMuiEmbed_mod;

  //! embeding module (merge hits from different input files)
  mMutEmbed _mMutEmbed_mod;

  //@}

  //! Timer
  PHTimeServer::timer _timer;

  #endif

  //! embedding mode
  unsigned int _mode;

  //! what vertex to use in reconstruction
  unsigned int _vtx_to_use;

  //! patttern recognition configuration flags
  /*! it is a bitwise or of the Flags enumaration */
  unsigned int _flags;

  //! mutr base efficiency (propagated to mMutResponsePar)
  double _mutr_base_efficiency[1024];

  //! use BBC rate dependent chamber efficiency
  bool _check_bbc_rate;

  //! linear dependence of chamber efficiencies with BBC rate
  float _chamber_eff0[1024];
  float _chamber_eff1[1024];
};

#endif /* __MUONUNPACKSIM_H__ */
