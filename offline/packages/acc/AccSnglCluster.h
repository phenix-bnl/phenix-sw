#ifndef __ACCSNGLCLUSTER_HH_
#define __ACCSNGLCLUSTER_HH_

#include "PHObject.h"
#include <iostream>

//
//  The AccSnglCluster is the base class of individual cluster  measurements.
//  It is foreseen that the contents of a SnglCluster might vary over time.
//  One could have additional or fewer indices kept in a future version
//  of a cluster hit.
//
//  This virtual base class implements the "superset" of all fields ever
//  encountered in any versioned class of AccSnglClusters.  The implementation 
//  is to print a warning that the field does not exist.  For any field
//  that is actually overridden in the versioned Sngl class, the real 
//  implementation overrides the warning.
//
//  This schema evolution allows the AccSnglCluster class to evolve over time
//  and yet all the code that uses these classes runs on all versions.
//
//                                    TKH 8-11-2003
//

class AccSnglCluster : public PHObject
{


 public:
  virtual ~AccSnglCluster() {}

  // Set the values in the SnglCluster...
  // These virtual functions should ALL be overridden!
  // If the local version is called by mistake, the user sees a
  // warning on their screen.
  virtual void set_aerph1       (const unsigned int ibox, const float val)   {warning("aerph1       ");}
  virtual void set_aerph2       (const unsigned int ibox, const float val)   {warning("aerph2       ");}
  virtual void set_aert1        (const unsigned int ibox, const float val)   {warning("aert1        ");}
  virtual void set_aert2        (const unsigned int ibox, const float val)   {warning("aert2        ");}
  virtual void set_aerhitid     (const int   val)                            {warning("aerhitid     ");}
  virtual void set_aerhitconfig (const int   val)                            {warning("aerhitconfig ");}

  // Get the values from the SnglCluster...
  // The virtual base class prints warning then returns crap...
  virtual float get_aerph1       (const unsigned int ibox) const {warning("aerph1       "); return -9999;}
  virtual float get_aerph2       (const unsigned int ibox) const {warning("aerph2       "); return -9999;}
  virtual float get_aert1        (const unsigned int ibox) const {warning("aert1        "); return -9999;}
  virtual float get_aert2        (const unsigned int ibox) const {warning("aert2        "); return -9999;}
  virtual int   get_aerhitid     ()                        const {warning("aerhitid     "); return -9999;}
  virtual int   get_aerhitconfig ()                        const {warning("aerhitconfig "); return -9999;}

 private:
  void warning(const char* field) const;

  ClassDef(AccSnglCluster,1)
};
#endif
