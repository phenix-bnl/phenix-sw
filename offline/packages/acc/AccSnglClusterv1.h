#ifndef __ACCSNGLCLUSTERV1_H_
#define __ACCSNGLCLUSTERV1_H_

#include "PHObject.h"
#include "AccSnglCluster.h"

#define ACCBOX 4

class AccSnglClusterv1 : public AccSnglCluster
{
 public:
  AccSnglClusterv1();
  AccSnglClusterv1(AccSnglClusterv1*track);  
  virtual ~AccSnglClusterv1() {}

  // Here are the very explicit set routines...
  virtual void set_aerph1       (const unsigned int ibox, const float val)   {if (ibox<ACCBOX) aerph1[ibox]=val;}
  virtual void set_aerph2       (const unsigned int ibox, const float val)   {if (ibox<ACCBOX) aerph2[ibox]=val;}
  virtual void set_aert1        (const unsigned int ibox, const float val)   {if (ibox<ACCBOX) aert1 [ibox]=val;}
  virtual void set_aert2        (const unsigned int ibox, const float val)   {if (ibox<ACCBOX) aert2 [ibox]=val;}
  virtual void set_aerhitid     (const int   val)                            {aerhitid    =val;}
  virtual void set_aerhitconfig (const int   val)                            {aerhitconfig=val;}

  // Here are the very explicit "get" routines...
  float get_aerph1       (const unsigned int ibox) const {if (ibox<ACCBOX) return aerph1[ibox]; else return -9999;}
  float get_aerph2       (const unsigned int ibox) const {if (ibox<ACCBOX) return aerph2[ibox]; else return -9999;}
  float get_aert1        (const unsigned int ibox) const {if (ibox<ACCBOX) return aert1 [ibox]; else return -9999;}
  float get_aert2        (const unsigned int ibox) const {if (ibox<ACCBOX) return aert2 [ibox]; else return -9999;}
  int   get_aerhitid     ()                        const {return aerhitid     ;}
  int   get_aerhitconfig ()                        const {return aerhitconfig ;}

 protected:
  float aerph1[ACCBOX];       
  float aerph2[ACCBOX];
  float aert1 [ACCBOX];      
  float aert2 [ACCBOX];      
  int   aerhitid      ;
  int   aerhitconfig  ;

  ClassDef(AccSnglClusterv1,1)
};

#endif

