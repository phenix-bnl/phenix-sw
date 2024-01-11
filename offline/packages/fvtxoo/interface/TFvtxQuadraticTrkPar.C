#include <iostream>
#include <TFvtxQuadraticTrkPar.h>
#include <FVTXOO.h>
#include <PHGslMatrix.h>

ClassImp(TFvtxQuadraticTrkPar);

TFvtxQuadraticTrkPar::TFvtxQuadraticTrkPar(const Key key,
					 const unsigned short arm,
					 const unsigned short index) :
  PHKey(key),
  _index(index),
  _arm(arm)
{
}


void
TFvtxQuadraticTrkPar::print( std::ostream& out ) const
{
  FVTXOO::PRINT(out, "TFvtxQuadraticTrkPar");
  out << "Constant param: (" << _ax << "," << _ay << ")" << std::endl;
  out << "linear param: (" << _bx << "," << _by << ")" << std::endl;
  out << "quadratic param: (" << _cx << "," << _cy << ")" << std::endl;  
  out << "Chisquare=" << _chi_square << std::endl;
  out << "Covariance: " << std::endl;
  
  PHGslMatrix cov(6, 6 );
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
  
