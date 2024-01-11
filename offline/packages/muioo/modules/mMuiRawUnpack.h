#ifndef __MMUIRAWUNPACK_H__
#define __MMUIRAWUNPACK_H__

#include "PHTimeServer.h"

#include <mMuiRawUnpackPar.h>

class Event;   
class PHCompositeNode;
class TMuiHitMapO;

/**
 * Parses the MuID DCM packet data, and fills the TMuiRawMapO table for those
 * channels that have signals.
 *
 * @author Jason Newby \URL{mailto:rjnewby@utk.edu}
 * @see TMuiRawMapO.h
 */

/*! \@ingroup modules*/
class mMuiRawUnpack
{
public:
  //! Constructor.
  mMuiRawUnpack();
  
  //! Destructor.
  virtual ~mMuiRawUnpack(){}

  //! standard event method
  PHBoolean event(PHCompositeNode *basenode);
  
  //! event method
  PHBoolean event(Event* evt, PHCompositeNode *basenode);

 private:

  //! retrieves parameter node and IOC
  void set_interface_ptrs(PHCompositeNode* top_node);

  //! returns data bit pattern from data word
  unsigned int getbits(unsigned int word, int start, int length)
  { return (word >> (start+1-length)) & ~(~0 << length); }

  const mMuiRawUnpackPar* _mod_par; //!< parameter table
  Event* _event;                    //!< event class
  TMuiHitMapO* _hit_map;            //!< muid hit map

  
  PHTimeServer::timer _timer;
  
};

#endif /*__MMUIRAWUNPACK_H__*/
