/*
 * TFvtxMPTracks.h
 *
 *  Created on: Nov 11, 2012
 *      Author: jinhuang
 */

#ifndef TFVTXMPTRACKS_H_
#define TFVTXMPTRACKS_H_

#include <PHObject.h>
class TClonesArray;

class TFvtxMPTracks : public PHObject
{
public:
  TFvtxMPTracks();
  virtual
  ~TFvtxMPTracks();

  TFvtxMPTrack*  AddTrack();

  enum
  {
    MAX_N_NODES = 8
  };

  unsigned int nTracks;
  TClonesArray *Tracks;
};

#endif /* TFVTXMPTRACKS_H_ */
