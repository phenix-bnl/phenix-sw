#ifndef __MCHEPMCPARTICLE_H__
#define __MCHEPMCPARTICLE_H__

#include <iostream>
#include "TObject.h"
#include "phool.h"

class MCHepMCParticle : public TObject
{

 public:

  MCHepMCParticle(){ Reset();}
  ~MCHepMCParticle() {}
  void identify(std::ostream &os = std::cout) const;
  void Reset();

  int get_mc_n_part() const {return mc_n_part;}
  int get_mc_barcode() const {return mc_barcode;}
  int get_mc_vertex_begin() const {return mc_vertex_begin;}
  int get_mc_vertex_end() const {return mc_vertex_end;}
  float get_mc_px() const {return mc_px;}
  float get_mc_py() const {return mc_py;}
  float get_mc_pz() const {return mc_pz;}
  float get_mc_e() const {return mc_e;}
  float get_mc_ptot() const {return mc_ptot;}
  float get_mc_x_begin() const {return mc_x_begin;}
  float get_mc_y_begin() const {return mc_y_begin;}
  float get_mc_z_begin() const {return mc_z_begin;}
  float get_mc_x_end() const {return mc_x_end;}
  float get_mc_y_end() const {return mc_y_end;}
  float get_mc_z_end() const {return mc_z_end;}
  int get_mc_pid() const {return mc_pid;}
  int get_mc_status() const {return mc_status;}

 public:

  float mc_px, mc_py, mc_pz, mc_e, mc_ptot, mc_x_begin, mc_y_begin, mc_z_begin, mc_x_end, mc_y_end, mc_z_end;
  int mc_n_part, mc_pid, mc_status, mc_barcode, mc_vertex_begin, mc_vertex_end;
  ClassDef(MCHepMCParticle,1)

};

#endif /* __MCHEPMCPARTICLE_H__ */
