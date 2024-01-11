// $Id: MuonCounter.h,v 1.4 2015/01/15 21:40:49 snowball Exp $
#ifndef MuonCounter_h
#define MuonCounter_h

#include <SubsysReco.h>

/*!
\file MuonCounter.h
\ingroup supermodules 
\brief closes PHTFileManager in End() method
\author H. Pereira
\version $Revision: 1.4 $
\date $Date: 2015/01/15 21:40:49 $
*/

// Forward declerations
class PHCompositeNode;

/*!
\class MuonCounter
\ingroup supermodules 
\brief reads MC and reconstructed muid maps, fills efficiency ntuples
*/
class MuonCounter: public SubsysReco
{
  public:
  
  //! constructor
  MuonCounter( const char* name= "MuonCounter" );
  
  //! destructor
  virtual ~MuonCounter() 
  {}
  
  //! event method
  int process_event(PHCompositeNode *topNode);
  
  //! init method (begin of process)
  int Init(PHCompositeNode *topNode)
  { return 0; }
  
  //! end of process method
  int End(PHCompositeNode *topNode);

  //! event count
  /* number of events between two printouts */
  void set_event_dump( const int& value )
  { _event_dump = value; }

  bool
  is_log_scale_dump() const
  {
    return _log_scale_dump;
  }

  void
  set_log_scale_dump(bool logScaleDump = true)
  {
    _log_scale_dump = logScaleDump;
  }
  
  int get_count(){ return _local_counter;}

  
  private:
  
  //! local event counter
  unsigned int _local_counter;
  
  //! event count
  /*! 
  number of events between two printouts. 
  Non positive numbers mean: no printout
  */  
  int _event_dump;
  
  //! dump @ count of 1,2...,10,20...,100,200,...
  bool _log_scale_dump;

};

#endif /* __MuonCounter_H__ */
