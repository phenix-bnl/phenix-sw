#ifndef _PRIPISAHITHELPER_
#define _PRIPISAHITHELPER_

// $Id: PriPISAHitHelper.h,v 1.2 2015/03/04 16:26:13 snowball Exp $

/*!
\file  PriPISAHitHelper.h
\brief Helper class used to associate primary hit index to KinHit objects
\author  H. Pereira
\version $Revision: 1.2 $
\date    $Date: 2015/03/04 16:26:13 $
*/

#include <cassert>
#include <set> 
#include <PISAEvent.h>

class PriPISAHitHelper
{
  
  public: 
  
  //! constructor
 PriPISAHitHelper( void ):
  warningCounter(0)
  {}
  
  //! perform primary hit index associations to KinHit object
  void associate( void );

 private:
  int warningCounter;
      
};

#endif

