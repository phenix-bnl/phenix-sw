#include <iostream>
#include "TMutTrkPar.hh"
#include "MUTOO.h"
#include "PHGslMatrix.h"

ClassImp(TMutTrkPar);

using namespace std;

//___________________________________________________
void TMutTrkPar::print( std::ostream &out ) const
{
  MUTOO::PRINT(out, "TMutTrkPar");
  out << "Position: (" << _x << "," << _y << "," << _z << ")" << endl;
  out << "Momentum: (" << _px << "," << _py << "," << _pz << ")" << endl;
  out << "Charge=" << _charge << " Chisquare=" << _chi_square << endl;
  out << "Covariance: " << endl;
  
  PHGslMatrix cov(5, 5 );
  for( unsigned int i=0; i<COVAR_ROW; i++ ) {
    for( unsigned int j=0; j<COVAR_ROW; j++ )
    { cov(i,j) = _covar[i*COVAR_ROW+j]; }
  }
  
  cov.print();
  
  MUTOO::PRINT(out, "**");
  return;
}
  
