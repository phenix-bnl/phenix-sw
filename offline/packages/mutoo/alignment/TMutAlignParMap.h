#ifndef TMutAlignParMap_h
#define TMutAlignParMap_h

// $Id: TMutAlignParMap.h,v 1.2 2011/12/24 04:48:18 slash Exp $

/*!
\file TMutAlignParMap.h
\brief Muon global alignment parameters container
\author H. Pereira
\version $Revision: 1.2 $
\date $Date: 2011/12/24 04:48:18 $
*/

// BOOST headers
#include <boost/smart_ptr.hpp>

// PHENIX headers
#include "TMutAlignPar_v1.hh"

#include <TMutKeyGen.h>
#include <TMutMapIO.h>
#include <PHMap.h>
#include <PHMapIterator.h>
#include <PHConstMapIterator.h>
#include <PHKey.hh>

/*! \ingroup container */
//! Muon global alignment parameters container
class TMutAlignParMap : public PHMap< PHKey::key_type, TMutAlignPar, TMutAlignPar_v1 >
{

    public:

    //! @name Constructors/Destructors
    //@{

    /*! Default contructor */
    TMutAlignParMap();

    /*! Default contructor */
    TMutAlignParMap(PHKey::map_key_type map_key);

    /*! Virtual destructor */
    virtual ~TMutAlignParMap()
    {}

    //@}

    //! @name Insertors
    //@{
    /*!
    Insert an new TMutAlignPar into map and return an iterator to the newly created
    object.
    */
    iterator insert_new(unsigned short arm);
    //@}

    //! @name Extractors
    //@{
    /*! Get an iterator to all TMutAlignPar in given arm */
    iterator get(unsigned short arm);

    /*! Get an iterator to all TMutAlignPar in given arm */
    const_iterator get(unsigned short arm) const;
    //@}

    //! @name Clear
    //@{
    void clear()
    {
        _count=0;
        PHMap<PHKey::key_type, TMutAlignPar, TMutAlignPar_v1>::clear();
    }

    //@}

    private:

    unsigned short get_roll_count()
    { return _count++%TMutKeyGen::get_max_index();}

    unsigned short _count;

};

#endif
