#include "PdbMpcExMinipadMIPCorrection.hh"
#include <iostream>

void PdbMpcExMinipadMIPCorrection::print() const {
  std::cout<<_key<<" "<<_mip_correction<<" "<<_fit_mpv<<" "<<_fit_sigma<<" "<<_cutoff_eff<<" "<<_cutoff_pos<<std::endl;
}
