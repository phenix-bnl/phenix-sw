// $Id: MuonUnpackPisa.h,v 1.25 2019/09/29 22:04:27 slash Exp $
#ifndef __MUONUNPACKPISA_H__
#define __MUONUNPACKPISA_H__

/*!
  \file		MuonUnpackPisa.h
  \ingroup supermodules
  \brief	 muon pisa to dst supermodule
  \author	Sean Kelly
  \version $Revision: 1.25 $
  \date		$Date: 2019/09/29 22:04:27 $
*/

// Forward declerations
class PHCompositeNode;

#ifndef __CINT__
#include <PHTimeServer.h>
#include <mMuiSlowSim.h>
#include <mMuiResponse.h>

#include <mMutSlowSim.h>
#include <mMutResponse.h>
#include <mMutCalibrate.h>
#include <mMutZeroSup.h>

#endif

#include "MuonSubsysReco.h"
#include "TDataType.h"


//! muon pisa to dst supermodule
/*! muon pisa to dst supermodule
    reads pisa root files; fills mutr/muid mc hit/track maps consequently;
    possibly runs the response, to build mutr/muid hit maps, writes to a simulated DST
    and possibly to a simulated PRDF.
*/
class MuonUnpackPisa: public MuonSubsysReco
{
 public:

  //! configuration flags
  enum Flag
  {

    //! no flags
    NONE = 0,

    //! run response code
    DO_RESPONSE = (1<<0),

    //! skipp offline zero suppression
    NO_ZERO_SUP = (1<<1),

    //! skip calibration
    NO_CALIBRATE = (1<<2),

    //! skip charge smearing
    NO_CHARGE_SMEAR = (1<<3),

    //! add noise
    ADD_NOISE = (1<<4)

  };

  //! constructor
  MuonUnpackPisa( const char* name = "MUONUNPACKPISA" );

  //! destructor
  virtual ~MuonUnpackPisa();


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

  //! run initialization
  int Init(PHCompositeNode *topNode);

  //! run initialization
  int InitRun(PHCompositeNode *topNode);

  //! event method
  int process_event(PHCompositeNode *topNode);

  //! end method
  int End(PHCompositeNode *topNode);

  //! returns true if response is to be run and written to DST
  bool do_response() const
  { return get_flag( DO_RESPONSE );}

  //! sets the do_response flag to tell if response is to be run and written to DST
  void set_do_response( bool value = true )
  { set_flag( DO_RESPONSE, value ); }

  //! returns true if noise has been added
  bool add_noise() const
  { return get_flag( ADD_NOISE ); }

  //! sets the add noise
  void set_add_noise( bool value = true )
  { set_flag( ADD_NOISE, value); }

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

  void set_mutr_base_efficiency(const int arm, const int station, const int gap, const int plane , double eff_value)
  {
    for (int oct=0; oct<MUTOO::MAX_OCTANT; oct++)
      for (int hoct=0; hoct<MUTOO::MAX_HALF_OCTANT; hoct++)
	  set_mutr_base_efficiency(arm, station, gap, plane, oct, hoct, eff_value);
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
    int index =  pla + MUTOO::NumberOfPlanes*(hoct + MUTOO::MAX_HALF_OCTANT *(oct + MUTOO::MAX_OCTANT *(gap + MUTOO::NumberOfGaps*( station + MUTOO::NumberOfStations* arm ))));
    _mutr_base_efficiency[index] = value;
  }

  /*! Set all chamber efficiencies from file */
  void set_mutr_base_efficiency( const std::string file );

  void set_bbcrate_dependent_efficiency( const std::string file );
  
  /*! Chamber correlation width */
  void set_mutr_correlation_width( const double& value)
    {
      for(int iarm=0;iarm<MUTOO::NumberOfArms;iarm++)
      for(int ista=0;ista<MUTOO::NumberOfStations;ista++)
      for(int igap=0;igap<MUTOO::NumberOfGaps;igap++)
      {
        set_mutr_correlation_width(iarm,ista,igap,value);
      }
    }

  /*! Chamber correlation width */
  void set_mutr_correlation_width( const int& arm, const double& value)
    {
      for(int ista=0;ista<MUTOO::NumberOfStations;ista++)
      for(int igap=0;igap<MUTOO::NumberOfGaps;igap++)
      {
        set_mutr_correlation_width(arm,ista,igap,value);
      }
    }

  /*! Chamber correlation width */
  void set_mutr_correlation_width( const int& arm, const int& station, const int& gap, const double& value )
  { _mutr_correlation_width[gap + MUTOO::NumberOfGaps*( station + MUTOO::NumberOfStations* arm) ] = value; }

  protected:

  int CreateNodeTree(PHCompositeNode *topNode);

   //! obtain bbcrate for this run from DB
  float get_bbcrate(PHCompositeNode* top_node) const;

 	//! mutoo working node
  PHCompositeNode * mutoo_node;

  //! muioo working node
  PHCompositeNode * muioo_node;

  //! dst io node
  PHCompositeNode * dst_node;

  #ifndef __CINT__

  //!@name reconstruction modules
  //@{

  // MUTOO module data members
  //! muid slowsimulator (pisa->MCHit)
  mMuiSlowSim _mMuiSlowSim_mod;

  //! muid response (MCHit->Hit)
  mMuiResponse _mMuiResponse_mod;

  //! mutr slowsimulator (pisa->MCHit)
  mMutSlowSim _mMutSlowSim_mod;

  //! mutr response (MCHit->Hit)
  mMutResponse _mMutResponse_mod;

  //! zero suppression
  mMutZeroSup _mMutZeroSup_mod;

  //! mutr hit calibration
  mMutCalibrate _mMutCalibrate_mod;

  //@}

  //! module timer
  PHTimeServer::timer _timer;

  #endif

  //! patttern recognition configuration flags
  /*! it is a bitwise or of the Flags enumaration */
  unsigned int _flags;

  // mutr gap efficiency
  double _mutr_base_efficiency[1024];
  double _mutr_correlation_width[18];

  //! use BBC rate dependent chamber efficiency
  bool _check_bbc_rate;

  //! linear dependence of chamber efficiencies with BBC rate
  float _chamber_eff0[1024];
  float _chamber_eff1[1024];

};

#endif /* __MUONUNPACKPISA_H__ */







