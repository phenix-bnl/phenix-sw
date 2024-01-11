#ifndef __MNEWDCHUNPACKER_H__
#define __MNEWDCHUNPACKER_H__

#include "DchDgoPar.h"

class Event;
class PHCompositeNode;
class PHDchAddressObject;

class mNewDchUnpacker
{
public:
  mNewDchUnpacker();
  virtual ~mNewDchUnpacker() {}
  virtual int event(PHCompositeNode *, Event *, const short startpacket=1, const short endpacket=160);   

protected: 
  
  int fillWrapperTables(PHCompositeNode *topNode);

  // instead of a multi dimensional array (which gave problem with linux6.0):
  // finder[arm][side][key][pair][channel][nibbles]
  // we are using a one dimensional array.
  // The corresponding position in the one-dim array is calculated every time !
  int finder[numberOfArms*numberOfSides*numberOfKeyStones*numberOfPairs*numberOfChannels*numberOfNibbles];  
};
#endif /*__MNEWDCHUNPACKER_H__*/
