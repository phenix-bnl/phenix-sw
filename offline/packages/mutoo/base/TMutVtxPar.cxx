
/*!
   \file    TMutVtxPar.cxx
   \brief   
   parameter class for muon two track vertex. 
   this class is obselete. It is only used internaly for the TMutVtx_v1 interface and is kept for 
   backward compatibility.
   \author  Sean KELLY
   \version $Revision: 1.2 $
   \date    $Date: 2007/11/18 13:25:02 $
*/

#include "TMutVtxPar.hh"
ClassImp(TMutVtxPar)

//_________________________________________________________________
void TMutVtxPar::print(std::ostream& os) const
{
  os << " vertex pars = {" << get_z() << "," 
    << get_px1() << "," 
    << get_py1() << "," 
    << get_pz1() << "," 
    << get_px2() << "," 
    << get_py2() << "," 
    << get_pz2() << "}" << std::endl;
  os << " charge1:" << get_charge1() << "  " << "charge2:" << get_charge2() << std::endl;
}
