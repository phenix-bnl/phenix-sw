#ifndef __PADCLUSTERV2_H
#define __PADCLUSTERV2_H

#include "PadCluster.h"
#include <iostream>

class dPadClusterWrapper;
class TClonesArray;

class PadClusterv2 : public PadCluster
{
 public:
  PadClusterv2();
  virtual ~PadClusterv2();
  void identify(std::ostream& os = std::cout) const;
  void Reset();
  int isValid() const;

  void FillFromWrapper(dPadClusterWrapper *wrap);
  unsigned int get_PadNCluster() const {return PadNCluster;}
  void set_PadNCluster(const unsigned int nclus) {PadNCluster = nclus;return;}
  int set_TClonesArraySize(const unsigned int nclus);
  void AddPadCluster(const unsigned int iclus);

  short get_arm(const unsigned int ihit) const;
  short get_cell(const unsigned int ihit) const;
  short get_id(const unsigned int ihit) const;
  short get_sector(const unsigned int ihit) const;
  short get_type(const unsigned int ihit) const;
  short get_wire(const unsigned int ihit) const;

  float get_xyz(const unsigned int ihit, const short i) const;
  float get_dxyz(const unsigned int ihit, const short i) const;

  void set_arm(const unsigned int ihit, const short ival);
  void set_cell(const unsigned int ihit, const short ival);
  void set_id(const unsigned int ihit, const short ival);
  void set_sector(const unsigned int ihit, const short ival);
  void set_type(const unsigned int ihit, const short ival);
  void set_wire(const unsigned int ihit, const short ival);

  void set_xyz(const unsigned int ihit, const short i, const float rval);
  void set_dxyz(const unsigned int ihit, const short i, const float rval);

 protected:
  TClonesArray *GetPcClus() const {return PcClus;}
  unsigned int PadNCluster;
  TClonesArray *PcClus;

  ClassDef (PadClusterv2,1)

};

#endif /* __PADCLUSTERV2_H */

