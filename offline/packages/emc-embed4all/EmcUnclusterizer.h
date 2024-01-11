/*
 * class that undoes EmcClusterizerv0. due to the structure of 
 * the emcal data this class is pretty simple, but it could be 
 * (and with some implementation it was indeed) more complicated,
 * so this functionality got factorized out into a separate class.
 *
 */


#ifndef __EMC_UNCLUSTERIZER_H__
#define __EMC_UNCLUSTERIZER_H__

#include <string>

#include <Rtypes.h>

#include <SubsysReco.h>


class EmcUnclusterizer: public SubsysReco {
public:
  EmcUnclusterizer(): SubsysReco("EmcUnclusterizer"){};
  virtual ~EmcUnclusterizer(){};
  

public:
  int InitRun(PHCompositeNode * root);
  int process_event(PHCompositeNode * root);


  ClassDef(EmcUnclusterizer, 0)
};


#endif /* !__EMC_UNCLUSTERIZER_H__*/
