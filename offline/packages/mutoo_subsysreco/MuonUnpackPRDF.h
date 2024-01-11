#ifndef __MUONUNPACKPRDF_H__
#define __MUONUNPACKPRDF_H__

#include "MuonSubsysReco.h"
#include <MUTOO.h>
/*!
  \file    MuonUnpackPRDF.h
  \ingroup supermodules 
  \brief   creates mutr/muid hits from PRDF data, possibly write them under DST node
  \author  Hugo Pereira
  \version $Revision: 1.15 $
  \date    $Date: 2013/04/25 15:35:04 $
*/

// Forward declerations
class PHCompositeNode;

#ifndef __CINT__

#include <mMutUnpack.h>
#include <mMutCalibrate.h>
#include <mMuiRawUnpack.h>
#include <mMuiRawUnpack.h>
#include <mMutZeroSup.h>
#include <mMutZeroSupPar.h>
#include <PHTimeServer.h>

#endif

/*!
  \class   MuonUnpackPRDF
  \ingroup supermodules 
  \brief   creates mutr/muid hits from PRDF data, possibly write them under DST node
*/
class MuonUnpackPRDF: public MuonSubsysReco
{
 public:

  //! constructor
  MuonUnpackPRDF( const char* name = "MUONUNPACKPRDF" );
 
  //! destructor
  ~MuonUnpackPRDF();

  // Hack for now - copy the mMutZeroSupPar enumeration to this module
  // so we can access it from the macro. Will only need this functionality
  // for a short while, so probably o.k. for now.
  /*! Zero suppression mode */
  enum Mode {
    MC_IDMUTC_DCM1 = (1<<0),      //!< first monte carlo zero suppression mode
    MC_IDMUTC_FPGA0SUP = (1<<1),  //!< second monte carlo zero suppression mode
    DCM = (1<<2),                 //!< first RD suppression mode
    SHAPE = (1<<3),                //!< second RD suppression mode
    BADCHAN = (1<<4),             //!< suppress bad channels?
    TEST = (1<<5)                 //!< zero suppression tests
  };


  //! run initialization
  int Init(PHCompositeNode *);
 
  //! run initialization
  int InitRun(PHCompositeNode *);
  
  //! event method
  int process_event(PHCompositeNode *);
  
  //! finish processing
  int End(PHCompositeNode *);
  
  //! configuration flags
  enum Flag
  {
    NONE = 0,
      
    //! skip mutr offline zero suppression
    SKIP_ZERO_SUPPRESSION = 1<<0,
    
    //! skip mutr unpacking
    SKIP_MUTR = 1<<1,
    
    //! skip muid unpacking
    SKIP_MUID = 1<<2
  };
  
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

  void set_zero_mode( ULong_t mode )
  {_zero_mode = mode;}
  
  ULong_t get_zero_mode( ) const
  { return _zero_mode;}
  
  //! setter for verbosity 
  void set_verbosity(MUTOO::Verbosity v) { _verbosity = v; }
  
  //! getter for verbosity 
  MUTOO::Verbosity get_verbosity() const { return _verbosity; }
  
  protected:  
  
  //! creates checks all needed nodes
  int CreateNodeTree(PHCompositeNode *);

  //! patttern recognition configuration flags
  /*! it is a bitwise or of the Flags enumaration */
  unsigned int _flags;
  
  //! mutoo node
  PHCompositeNode * mutoo_node;   
  
  //! muioo node
  PHCompositeNode * muioo_node;   
  
  //! dst node (for output)
  PHCompositeNode *dst_node;      
  
  #ifndef __CINT__
  
  //!@name reconstruction modules
  //@{
  
  //! mutr PRDF unpacker
  mMutUnpack _mMutUnpack_mod;
  
  //! mutr offline zero suppression
  mMutZeroSup _mMutZeroSup_mod;
  
  //! mutr hit calibration
  mMutCalibrate _mMutCalibrate_mod;
  
  //! muid PRDF unpacker
  mMuiRawUnpack _mMuiRawUnpack_mod;
  
  //@}
  
  //! supermodule timer
  PHTimeServer::timer _timer;

  #endif

  //! Verbosity Flag for messaging
  MUTOO::Verbosity _verbosity;

  //! Zero Suppression mode to use
  ULong_t _zero_mode;

};

#endif /* __MUONUNPACKPRDF_H__ */
