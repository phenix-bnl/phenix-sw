// $Id: FvtxEventFilter.h,v 1.1 2013/06/15 23:16:38 keyaaron Exp $

/*!
  \file		FvtxEventFilter.h
  \ingroup supermodules
  \brief	 fvtx based offline event filter to select special events for ouput
  \author	Aaron Key
  \version $Revision: 1.1 $
  \date		$Date: 2013/06/15 23:16:38 $
*/

#ifndef __FVTXEVENTFILTER_H__
#define __FVTXEVENTFILTER_H__

#include <SubsysReco.h>
#include <PHTimer.h>

// Forward declerations
class PHCompositeNode;
class TFvtxHitMap;

#include<FVTXOO.h>

/*!
  \class	 FvtxEventFilter
  \ingroup supermodules
  \brief	 muid based offline trigger used to filter data on Deep Deep road pairs
*/
class FvtxEventFilter: public SubsysReco
{
 public:

  //! Mode to decide which trigger data we want to select.
  enum MODE{

    //! select events with a high FVTX hit multiplicity, default > 450 hits in an arm
    HIGH_HIT_MULT,

    //! nothing done. is default. prints a message at every event
    NONE
  };

  //! Mode to select what to do with rejected events
  enum ACTION {

    //! events are processed normaly but not written to the DST
    DISCARD_EVENT,

    //! event processing is aborted immediately (later modules are not processed)
    ABORT_EVENT,

    //! do nothing, stricly
    DO_NOTHING
  };

  //! constructor
  FvtxEventFilter( const char* name = "FVTXEVENTFILTER", const MODE& mode = NONE, const ACTION& action = ABORT_EVENT );

  //! run initialization
  virtual int InitRun( PHCompositeNode *topNode );

  //! destructor
  virtual ~FvtxEventFilter() {}

  //! event method
  int process_event(PHCompositeNode *topNode);

  //! end method
  int End(PHCompositeNode *topNode);

  //! current event accepted flag
  const bool& event_accepted( void ) const
  { return _event_accepted; }

  //! trigger mode
  void set_mode(MODE mode)
  { _mode = mode; }

  //! trigger mode
  MODE get_mode() const
  { return _mode; }

  //! action to be taken when event is not accepted
  void set_action(ACTION action)
  { _action = action; }

  //! action to be taken when event is not accepted
  ACTION get_action() const
  { return _action; }

  //! set minimum number of fvtx hits to be high multiplicity
  void set_high_mult_hit_cut(const int& val)
  { _min_fvtx_hits = val; }

  //! get minimum number of fvtx hits to be high multiplicity
  int get_high_mult_hit_cut() const
  { return _min_fvtx_hits; }

  protected:

  //! returns true when either FVTX arm has > _min_fvtx_coords coordinates
  bool is_high_multiplicity(PHCompositeNode* top_node);

  //! lower limit on number of fvtx coordinates in an arm to be considered high multiplicity
  unsigned int _min_fvtx_hits;

  //! module timer
  PHTimer _timer;

  //! filter mode
  MODE _mode;

  //! action to be taken for rejected events [local scope]
  ACTION _action;

  //! current event accepted flag
  bool _event_accepted;

  //! total number of processed events
  ULong_t _nevent;

  //! number of rejected events
  ULong_t _nrejected;

  //! Fvtx coordinate map
  TFvtxHitMap* _hit_map;

};

#endif /* __FVTXEVENTFILTER_H__ */
