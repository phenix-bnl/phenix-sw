#ifndef TFvtxAlignParMap_h
#define TFvtxAlignParMap_h

// $Id: TFvtxAlignParMap.h,v 1.2 2011/12/01 04:16:18 slash Exp $

/*!
\file TFvtxAlignParMap.h
\brief Fvtx global alignment parameters container
\author Zhengyun You
\version $Revision: 1.2 $
\date $Date: 2011/12/01 04:16:18 $
*/

// BOOST headers
#include <boost/smart_ptr.hpp>

// PHENIX headers
#include "TFvtxAlignPar_v1.hh"

#include <TMutKeyGen.h>
#include <TMutMapIO.h>
#include <PHMap.h>
#include <PHMapIterator.h>
#include <PHConstMapIterator.h>
#include <PHKey.hh>

/*! \ingroup container */
//! Fvtx global alignment parameters container
class TFvtxAlignParMap : public PHMap< PHKey::key_type, TFvtxAlignPar, TFvtxAlignPar_v1 >
{

    public:

    //! @name Constructors/Destructors
    //@{

    /*! Default contructor */
    TFvtxAlignParMap();

    /*! Default contructor */
    TFvtxAlignParMap(PHKey::map_key_type map_key);

    /*! Virtual destructor */
    virtual ~TFvtxAlignParMap()
    {}

    //@}

    //! @name Insertors
    //@{
    /*!
    Insert an new TFvtxAlignPar into map and return an iterator to the newly created
    object.
    */
    iterator insert_new(unsigned short arm);
    //@}

    //! @name Extractors
    //@{
    /*! Get an iterator to all TFvtxAlignPar in given arm */
    iterator get(unsigned short arm);

    /*! Get an iterator to all TFvtxAlignPar in given arm */
    const_iterator get(unsigned short arm) const;
    //@}

    //! @name Clear
    //@{
    void clear()
    {
        _count=0;
        PHMap<PHKey::key_type, TFvtxAlignPar, TFvtxAlignPar_v1>::clear();
    }

    //@}

    private:

    unsigned short get_roll_count()
    { return _count++%TMutKeyGen::get_max_index();}

    unsigned short _count;

};

#endif
