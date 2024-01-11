#include <PHGslMatrix.h>
#include <TFvtxStraightTrkPar_v1.h>

ClassImp(TFvtxStraightTrkPar_v1)

void
TFvtxStraightTrkPar_v1::print( std::ostream& out ) const
{
  FVTXOO::PRINT(out, "TFvtxStraightTrkPar");
  out << "Intercepts: (" << _x << "," << _y << ")" << std::endl;
  out << "Slopes: (" << _mx << "," << _my << ")" << std::endl;
  out << "Chisquare=" << _chi_square << std::endl;
  out << "Covariance: " << std::endl;
  
  PHGslMatrix cov(5, 5 );
  for( unsigned int i=0; i<COVAR_ROW; i++ ) 
    {
      for( unsigned int j=0; j<COVAR_ROW; j++ )
	{ 
	  cov(i,j) = _covar[i*COVAR_ROW+j]; 
	}
    }
  
  cov.print();
  
  FVTXOO::PRINT(out, "**");
  return;
}
