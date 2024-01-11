#ifndef __EMCOMHELPER_H__
#define __EMCOMHELPER_H__

#include "emcObjectManagerRegister.h"
#include "emcObjectManager.h"
#include "emcDataManager.h"
#include "emcDefines.h"

/** Helper class for DM plugins implementation(s). 
@ingroup dmplugins
*/

class emcOMHelper
{
public:

  static emcObjectManager* findOM(emcManageable& object, int to_or_from)
  {
    emcObjectManager* value = 0 ;

    emcObjectManagerMap cmap =
      emcObjectManagerRegister::GetInstance()->GetObjectManagerMap() ;
    emcObjectManagerMap::const_iterator it ;

    emcDataManager* dm = emcDataManager::GetInstance();

    if ( to_or_from == 0 )
      {
	for ( it = cmap.begin() ; it != cmap.end() && value == 0 ; it++)
	  {
	    emcObjectManagerMap::value_type entry = *it ;
	    if (dm->GetVerboseLevel() >= 10)
	      {
		std::cout << "FindPlugin :"
			  << " Trying plugin "
			  << entry.second->GetName()
			  << std::endl;
	      }

	    if ( entry.second->CanRead(object) )
	      {
		value = entry.second ;
		if ( dm->GetVerboseLevel() >= 10)
		  {
		    std::cout << "YES!!" << std::endl;
		  }
	      }
	    else
	      {
		if ( dm->GetVerboseLevel() >= 10)
		  {
		    std::cout << "NOPE" << std::endl;
		  }
	      }
	  }
      }
    else if ( to_or_from == 1 )
      {
	for ( it = cmap.begin() ; it != cmap.end() && value == 0 ; it++)
	  {
	    emcObjectManagerMap::value_type entry = *it ;
	    if ( entry.second->CanWrite(object) )
	      {
		value = entry.second ;
	      }
	  }
      }
    else
      {
	std::cerr << EMC_ERROR_MSG << "FindPlugin : invalid "
		  << " second parameter = " << to_or_from << ", valid=0=>read "
          << " or 1=>write." << std::endl ;
      }
    
    if ( !value)
      {
	std::cerr << EMC_ERROR_MSG << "FindPlugin : could not "
		  << " find a suitable plugin for object name="
		  << object.GetName() << " title=" << object.GetTitle()
		  << " classname=" << object.GetClassName() << std::endl ;
      }
    
    return value;
  }
};

#endif


