
/*!
   \file    TMutVtxPar_v2.cxx
   \brief   
   parameter class for muon two track vertex. 
   this class is obselete. It is only used internaly for the TMutVtx_v1 interface and is kept for 
   backward compatibility.
   \author  Sean KELLY
   \version $Revision: 1.2 $
   \date    $Date: 2007/11/18 13:25:03 $
*/

#include "PHGslMatrix.h"
#include "TMutVtxPar_v2.hh"

ClassImp(TMutVtxPar_v2)

using namespace std;

//_____________________________________________________________
void TMutVtxPar_v2::print(ostream& os ) const
{
  os << " vertex pars = {" 
    << get_x() << ","  
    << get_y() << "," 
    << get_z() << "}" << endl;
  
  os << " bend plane vertex pars = {" 
    << get_x_bp() << ","  
    << get_y_bp() << "," 
    << get_z_bp() << "," 
    << get_dca_bp() << "}" << endl;
  
  os << " tracks momentum = {" 
    << get_px1() << "," 
    << get_py1() << "," 
    << get_pz1() << "} {" 
    << get_px2() << "," 
    << get_py2() << "," 
    << get_pz2() << "}" << endl;
  os << " charge1:" << get_charge1() << "  " << "charge2:" << get_charge2() << endl;
  
  os << " chisquare:" << get_chi_square() << "/" << get_ndf() << endl;
  
  // covariance matrix
  PHGslMatrix cov(COVAR_ROW,COVAR_ROW);
  for( unsigned int i=0; i<COVAR_ROW; i++ )
  for( unsigned int j=0; j<COVAR_ROW; j++ )
  { cov.set(i, j, get_covar( i, j ) ); }
  
  os << " covariance matrix: " << endl;
  os << cov << endl;
  
}
