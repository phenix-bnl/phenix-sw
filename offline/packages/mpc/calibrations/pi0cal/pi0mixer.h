//author: Beau Meredith


#ifndef __PI0MIXER_H__
#define __PI0MIXER_H__

#include <iostream>
#include "pi0base.h"
#include "MpcPi0Mass.h"
#include "MpcPi0MassContainer.h"

class TH1;
class THmulf;

#define skmaxptcls 200
#define skmaxpooldepth 100



class pi0mixer
{
 public:  
  pi0mixer(int maxpooldepth=3, int type=0, int verbosity=0);
  //default copy constructor will be made by compiler
  virtual ~pi0mixer() {}
  //  virtual void Reset();

  int addevent();
  int pushevent();
  int addptcl(pi0base *ptcl);
  void clear();
  int isfull();
  int isempty();
  int setpooldepth(int depth);
  int getpooldepth();
  int getnumptcls(int ev);
  const pi0base* getelement(int iev, int iptcl);
  int mixmass( TH1* h1 );
  int mixmassevents( int eva, TH1* h );
  int mixmass(  MpcPi0MassContainer* h );
  int mixmassevents( int eva, MpcPi0MassContainer* h );
  void mixtester();
  
  void printtest();
  void setverbosity(int verbosity);
  
  //protected:
  
  int koldest;
  int ktype;         
  //with mass mixing 0 => MPC
  
  
  int kcurrentptcls;              
  int kcurrentpooldepth;
  int kmaxpooldepth;                    // max number of pools for mixer
  int knumptcls[skmaxpooldepth];        // number of ptcls in each event
  
  int kverbosity;
  
  pi0base event_pool[skmaxpooldepth][skmaxptcls];
  pi0base event[skmaxptcls];

  
};

#endif /* __PI0MIXER_H__ */
