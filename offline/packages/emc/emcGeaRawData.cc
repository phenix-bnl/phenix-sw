//////////////////////////////////////////////////////////////////////////////////
//
// emcGeaRawData: this struct holds the raw adc values, it is the 
// reincarnation of dEmcGeaRawData.
//
// this data is not meant to be written to disc or to be used by anyone besides 
// the emcal library, so it is not scheme evolved.
//
//////////////////////////////////////////////////////////////////////////////////

#include <emcGeaRawData.h>

ClassImp(emcGeaRawData);
template class emcContentT< emcGeaRawData >;
const bool emcGeaRawData::__buildhash;



void emcGeaRawData::copy(emcGeaRawData const * from){
  hwkey = from->hwkey;
  swkey = from->swkey;
  type = from->type;
  adclopre = from->adclopre;
  adclopost = from->adclopost;
  adchipre = from->adchipre;
  adchipost = from->adchipost;
  tdc = from->tdc;
}
