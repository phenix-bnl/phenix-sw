#ifndef __MMUIADDDBENTRY_HH__
#define __MMUIADDDBENTRY_HH__

// $Id: mMuiAddDBEntry.hh,v 1.4 2009/08/22 13:58:52 hpereira Exp $  
/*!
   \file mMuiAddDBEntry.hh
   \brief add a new entry in the muid database. 
   \version $Revision: 1.4 $
   \date $Date: 2009/08/22 13:58:52 $
*/

#include <string>

#include "phool.h"
#include "PHTimeStamp.h"

class PHCompositeNode;

//! adds a new entry in the MuID database
/*!
 Adds a new entry in the MuID database.
 The data for the initializations are obtained from text files:
 
<ul>
 <li> mui-panel-geom.dat
 <li> mui-panel-size.dat
 <li> mui-tube-geom.dat
 <li> mui-tube-size.dat
 <li> mui-fem-config.dat
</ul> 
  @see TMuiGeometry
  @see TMuiAddressTable
*/
class mMuiAddDBEntry
{
  public:
  
  //! Constructor.
  mMuiAddDBEntry();
  
  //! Destructor.
  virtual ~mMuiAddDBEntry()
  {}
  
  //! Set the validity range (begin and end) for the database entry.
  void SetTimeStamps(const PHTimeStamp& t_begin, const PHTimeStamp& t_end);

  //! set description
  void SetDescription( char* description )
  { if( description ) _description = description; }
  
  //! Add the new database entry.
  PHBoolean event(PHCompositeNode *);

  private:
  
  //! DB entry start of validity
  PHTimeStamp _begin_time;
  
  //! DB entry end of validity
  PHTimeStamp _end_time;
  
  //! database description
  const char* _description;
  
  //! true when begin/end timestamp are set
  bool _time_set;
  
};

#endif /*__MMUIADDDBENTRY_HH__*/
