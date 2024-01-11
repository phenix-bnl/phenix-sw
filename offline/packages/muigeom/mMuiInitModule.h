#ifndef __MMUIINITMODULE_H__
#define __MMUIINITMODULE_H__

// $Id: mMuiInitModule.h,v 1.8 2009/06/15 20:36:31 hpereira Exp $   
/*!
   \file mMuiInitModule.h
   \brief beginning-of-job initializations for MuID reconstruction.
   \version $Revision: 1.8 $
   \date $Date: 2009/06/15 20:36:31 $
*/

#include <PHTimeStamp.h>

class PHCompositeNode;
 
//! beginning-of-job initializations for MuID reconstruction. 

/*!
 beginning-of-job initializations for MuID reconstruction.
<ul>
<li> initializes the geometry classes
<li> initializes the address class
</ul>
 
 Usually the data for the initializations will be obtained by a
 query to the PHENIX offline Objectivity database.  It is also
 possible to obtain these data from text files:
 
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
class mMuiInitModule
{
  public:
  
  //! Constructor.
  mMuiInitModule();
  
  //! Destructor.
  virtual ~mMuiInitModule()
  {}
  
  //! Set the time stamp for the database query.
  /* returns true if time stamp has changed */
  void SetSearchTimeStamp(const PHTimeStamp&);
  
  //! Enable mode where calibrations are read from the database. 
  void EnableDatabaseMode()
  {
    fIsUsingDatabase = true;
    std::cout << "mMuiInit::EnableDatabaseMode - will read calibrations from database" << std::endl;
  }
  
  //! Enable mode where calibrations are read from text files (not database).
  void DisableDatabaseMode()
  {
    fIsUsingDatabase = false;
    std::cout << "mMuiInit::DisableDatabaseMode - will read calibrations from text files" << std::endl;
  } 
  
  //! reset initialization
  void reset( void ) const;
  
  //! initialization
  void initialize( void );
  
  //! Do beginning-of-job initializations.
  /*! kept for backward compatibility */ 
  bool event(PHCompositeNode *node = 0);
  
  //! For outputting maps in local directory
  bool dumpmaps();

  private:
  
  //! time stamp to look after in database
  PHTimeStamp fSearchTime;
  
  //! true when database is to be used
  bool fIsUsingDatabase;
  
};

#endif /*__MMUIINITMODULE_H__*/
