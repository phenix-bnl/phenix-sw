
#ifndef __MuonEventNumberFilter_H__
#define __MuonEventNumberFilter_H__

// $Id: MuonEventNumberFilter.h,v 1.1 2008/12/19 23:30:52 hpereira Exp $

/*!
  \file		MuonEventNumberFilter.h	
  \ingroup supermodules
  \brief	 muid based offline trigger used to filter data on Deep Deep road pairs
  \author	Sean Kelly/Hugo Pereira
  \version $Revision: 1.1 $
  \date		$Date: 2008/12/19 23:30:52 $
*/

#include <SubsysReco.h>


// Forward declerations
class PHCompositeNode;

#ifndef __CINT__
#include <PHTimeServer.h>
#include <set>
#endif

#include <string>
#include <SubsysReco.h>

class PHCompositeNode;

/*!
  \class	 MuonEventNumberFilter
  \ingroup supermodules
  \brief	 muid based offline trigger used to filter data on Deep Deep road pairs
*/
class MuonEventNumberFilter: public SubsysReco
{
 public:


  //! Mode to select what to do with rejected events
  enum ACTION {
    DISCARD_EVENT,	//!< events are processed normaly but not written to the DST
    ABORT_EVENT,	//!< event processing is aborted immediately (later modules are not processed)
    DO_NOTHING	//!< do nothing, stricly
  };		

  //! constructor
  MuonEventNumberFilter( const char* name = "MUON_EVENTNUMBER_FILTER", const ACTION& action = DISCARD_EVENT );

  //! run initialization
  virtual int InitRun( PHCompositeNode* );
  
  //! destructor
  virtual ~MuonEventNumberFilter() {}
  
  //! event method
  int process_event(PHCompositeNode* );
  
  //! end method
  int End(PHCompositeNode* );
  
  //! set filename prototype
  /*! %i is used to replace by the run number */
  void set_event_filename( const char* file )
  { if( file ) _filename = file; }   
  
  private:
  
  //! load file 
  void load_events( int run_number );

  //! true if event is accepted
  bool accept_event( PHCompositeNode* );
    
  //! action
  ACTION _action;
  
  //! filename
  /*! %i is used to replace by the run number */
  std::string _filename;
  
  #ifndef __CINT__
  //! module timer
  PHTimeServer::timer _timer;

  //! set of event numbers
  std::set<int> _events;
  #endif
  
  //! total events
  unsigned long _total_events;
  
  //! accepted events
  unsigned long _accepted_events;
  
};

#endif /* __MuonEventNumberFilter_H__ */
