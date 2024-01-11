#ifndef __PADCLUSTER_H
#define __PADCLUSTER_H

#include <iostream>
#include "phool.h"
#include "PHObject.h"

class dPadClusterWrapper;

class PadCluster : public PHObject
{
 public:
  virtual ~PadCluster() {}
  virtual void identify(std::ostream& os = std::cout) const
    {
      os << "virtual PadCluster object";
      return;
    }

  virtual void Reset()
    {
      std::cout << PHWHERE << "ERROR Reset() not implemented by daughter class" << std::endl;
      return;
    }

  virtual int isValid() const
    {
      std::cout << PHWHERE << "isValid() not implemented by daughter class" << std::endl;
      return 0;
    }

  virtual void FillFromWrapper(dPadClusterWrapper *wrap) {return;}
  virtual unsigned int get_PadNCluster() const {return 0;}
  virtual void set_PadNCluster(const unsigned int nclus) {return;}
  virtual int set_TClonesArraySize(const unsigned int nclus) {return 0;}
  virtual void AddPadCluster(const unsigned int iclus) {return;}

  virtual short get_id(const unsigned int ihit) const {return -9999;}
  virtual short get_arm(const unsigned int ihit) const {return -9999;}
  virtual short get_sector(const unsigned int ihit) const {return -9999;}
  virtual short get_wire(const unsigned int ihit) const {return -9999;}
  virtual short get_cell(const unsigned int ihit) const {return -9999;}
  virtual short get_type(const unsigned int ihit) const {return -9999;}
  virtual float get_xyz(const unsigned int ihit, const short i) const {return -9999.9;}
  virtual float get_dxyz(const unsigned int ihit, const short i) const {return -9999.9;}

  virtual void set_id(const unsigned int ihit, const short ival) {return;}
  virtual void set_arm(const unsigned int ihit, const short ival) {return;}
  virtual void set_sector(const unsigned int ihit, const short ival) {return;}
  virtual void set_wire(const unsigned int ihit, const short ival) {return;}
  virtual void set_cell(const unsigned int ihit, const short ival) {return;}
  virtual void set_type(const unsigned int ihit, const short ival) {return;}
  virtual void set_xyz(const unsigned int ihit, const short i, const float rval) {return;}
  virtual void set_dxyz(const unsigned int ihit, const short i, const float rval) {return;}

  ClassDef (PadCluster,1)

};

#endif /* __PADCLUSTER_H */
