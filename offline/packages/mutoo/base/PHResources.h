// $Id: PHResources.h,v 1.5 2008/04/03 15:21:43 hpereira Exp $
#ifndef PHResources_h
#define PHResources_h

//////////////////////////////////////////////////////////////
/*! 
  \file PHResources.h
  \brief memory resources evaluation
  \author  Hugo Pereira
  \version $Revision: 1.5 $
  \date $Date: 2008/04/03 15:21:43 $
*/
//////////////////////////////////////////////////////////////

#include "MUTOO.h"

#include <iostream>

//! memory resources evaluation
/*! memory resources are evaluated parsing the /proc/[pid]/status file. This works on most linux versions that I know. */
class PHResources
{
  
  public:
  
  //! constructor
  PHResources( const int& pid ):
    _verbosity( MUTOO::NONE ),
    _resources_current( pid ),
    _resources_previous( pid ),
    _resources_diff( pid )
  {};
      
  //! verbosity
  void set_verbosity( const MUTOO::Verbosity& verbosity )
  { _verbosity = verbosity; }
    
  //! update resources
  /*! \param tag: if not zero, it is written has a header */
  bool update( const char* tag = 0 );

  //! class to handle resources from /proc/pid/status
  class Resources
  {  
  
    public:
    
    //! constructor
    Resources( const int& pid ):
      _valid( false ),
      _pid( pid ),
      _vmSize(0),
      _vmLock(0),
      _vmResident(0),
      _vmData(0),
      _vmStack(0),
      _vmExec(0),
      _vmLib(0)
    { update(); }
    
    //! update from proc file
    bool update( void );    
    
    //! true if update was successful
    bool _valid;
    
    int _pid;
    
    //! total memory size
    int _vmSize;
    
    //! locked memory size
    int _vmLock;
    
    //! resident memory size
    int _vmResident;
    
    //! data memory size
    int _vmData;
    
    //! stack memory size
    int _vmStack;
    
    //! exec memory size
    int _vmExec;
    
    //! library memory size
    int _vmLib;
    
    #ifndef __CINT__
    //! streamer
    friend std::ostream& operator << (std::ostream& out, const PHResources::Resources& resources );
    #endif
    
    // diff operator
    Resources operator - ( const Resources& resources ) const
    {
      Resources out( *this );
      out._vmSize -= resources._vmSize;
      out._vmLock -= resources._vmLock;
      out._vmResident -= resources._vmResident;
      out._vmData -= resources._vmData;
      out._vmStack -= resources._vmStack;
      out._vmExec -= resources._vmExec;
      out._vmLib -= resources._vmLib;
      return out;
    }
    
  };
  
  //! current resources
  Resources& get_current_resources( void )
  { return _resources_current; }
  
  //! diff resources with respect to previous call
  Resources& get_diff_resources( void )
  { return _resources_diff; }

  protected:  
       
  //! verbosity
  MUTOO::Verbosity _verbosity;
  
  //!@ name job resources
  //@{
  
  // current measurement
  Resources _resources_current;
  
  // last measurement
  Resources _resources_previous;

  // difference
  Resources _resources_diff;
  
  //@}
      
};


#endif
