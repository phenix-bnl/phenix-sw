#ifndef __FVTXUNPACKPRDF_H__
#define __FVTXUNPACKPRDF_H__

#include "MuonSubsysReco.h"

/*!
  \file    FvtxUnpackPRDF.h
  \ingroup supermodules 
  \brief   creates fvtx hits from PRDF data, possibly write them under DST node
  \author  Zhengyun You
  \version $Revision: 1.2 $
  \date    $Date: 2013/03/18 00:09:40 $
*/

// Forward declerations
class PHCompositeNode;

#ifndef __CINT__

#include <mFvtxUnpack.h>
//#include <mMutCalibrate.h>
//#include <mMuiRawUnpack.h>
//#include <mMutZeroSup.h>
#include <PHTimeServer.h>

#endif

/*!
  \class   FvtxUnpackPRDF
  \ingroup supermodules 
  \brief   creates mutr/muid hits from PRDF data, possibly write them under DST node
*/
class FvtxUnpackPRDF: public MuonSubsysReco
{
 public:

  //! constructor
  FvtxUnpackPRDF( const char* name = "FVTXUNPACKPRDF" );
 
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
      
    //! skip fvtx offline zero suppression
    SKIP_ZERO_SUPPRESSION = 1<<0,
    
    //! skip fvtx unpacking
    SKIP_FVTX = 1<<1,
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
  
  protected:  
  
  //! creates checks all needed nodes
  int CreateNodeTree(PHCompositeNode *);

  //! retrive parameter table and print to cout
  //! one can also reset run specific parameters here
  int check_parameters(PHCompositeNode *topNode);

  //! patttern recognition configuration flags
  /*! it is a bitwise or of the Flags enumaration */
  unsigned int _flags;
  
  //! fvtxoo node
  PHCompositeNode * fvtxoo_node;   
  
  //! dst node (for output)
  PHCompositeNode *dst_node;      
  
  #ifndef __CINT__
  
  //!@name reconstruction modules
  //@{
  
  //! fvtx PRDF unpacker
  mFvtxUnpack _mFvtxUnpack_mod;
  
  //! mutr offline zero suppression
//  mMutZeroSup _mMutZeroSup_mod;
  
  //! mutr hit calibration
//  mMutCalibrate _mMutCalibrate_mod;
  
  //@}
  
  //! supermodule timer
  PHTimeServer::timer _timer;

  #endif
  
};

#endif /* __FVTXUNPACKPRDF_H__ */
