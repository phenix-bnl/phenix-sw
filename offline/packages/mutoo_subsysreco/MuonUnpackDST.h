#ifndef __MUONUNPACKDST_H__
#define __MUONUNPACKDST_H__

#include "MuonSubsysReco.h"

/*!
\file MuonUnpackDST.h
\ingroup supermodules
\brief reads muid/mutr hit maps from a real data DST for later reconstruction.
\author Sean Kelly
\version $Revision: 1.18 $
\date $Date: 2010/08/25 22:17:33 $
*/

class PHCompositeNode;

#ifndef __CINT__
#include <PHTimeServer.h>
#include <mMutCalibrate.h>
#include <mMutZeroSup.h>
#endif

/*!
\class MuonUnpackDST
\ingroup supermodules
*/

class MuonUnpackDST: public MuonSubsysReco
{
    public:


    //! configuration flags
    enum Flag
    {

        //! no flags
        NONE = 0,

        //! skipp offline zero suppression
        NO_ZERO_SUP = (1<<0),

    };

    //! constructor
    MuonUnpackDST();

    //! destructor
    virtual ~MuonUnpackDST();

    //! run initialization
    int InitRun( PHCompositeNode* );

    //! event method
    int process_event( PHCompositeNode* );

    //! end of run
    int End( PHCompositeNode* );

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

    //! create neaded node
    int CreateNodeTree( PHCompositeNode* );

    #ifndef __CINT__

    //! mutr hit calibration
    mMutCalibrate _mMutCalibrate_mod;

    //! zero supression module
    mMutZeroSup _mMutZeroSup_mod;

    //! module timer
    PHTimeServer::timer _timer;

    #endif

    //! patttern recognition configuration flags
    /*! it is a bitwise or of the Flags enumaration */
    unsigned int _flags;


};

#endif /* __MUONUNPACKDST_H__ */
