//-----------------------------------------------------------------------------
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999
//
//  Declaration of class PdbMvdRp
//
//  Purpose: User defined storage class
//
//  Description:
//
//  Author: bnorman
//-----------------------------------------------------------------------------
#ifndef __PDBMVDRP_HH__
#define __PDBMVDRP_HH__

#include "PdbCalChan.hh"

#include <iostream>
#include <cmath>

class PdbMvdRp : public PdbCalChan {
public:
  PdbMvdRp();
  virtual ~PdbMvdRp();

  PdbMvdRp& operator = (PdbMvdRp const& source);

	void setSinMeanN(int n, float val){ sinMeanN[n - 1] = val; }
	void setSinMeanS(int n, float val){ sinMeanS[n - 1] = val; }
	void setSinMeanB(int n, float val){ sinMeanB[n - 1] = val; }
	void setSinRmsN( int n, float val){ sinRmsN[n - 1]  = val; }
	void setSinRmsS( int n, float val){ sinRmsS[n - 1]  = val; }
	void setSinRmsB( int n, float val){ sinRmsB[n - 1]  = val; }
	void setCosMeanN(int n, float val){ cosMeanN[n - 1] = val; }
	void setCosMeanS(int n, float val){ cosMeanS[n - 1] = val; }
	void setCosMeanB(int n, float val){ cosMeanB[n - 1] = val; }
	void setCosRmsN( int n, float val){ cosRmsN[n - 1]  = val; }
	void setCosRmsS( int n, float val){ cosRmsS[n - 1]  = val; }
	void setCosRmsB( int n, float val){ cosRmsB[n - 1]  = val; }

	void setAlphaN(int n, float val){ alphaN[n - 1] = val; }
	void setAlphaS(int n, float val){ alphaS[n - 1] = val; }
	void setAlphaB(int n, float val){ alphaB[n - 1] = val; }

	// 1<=n<=2, 1<=n<=32

	void setAN(int n, int i, float val){ aN[n - 1][i - 1] = val; }
	void setAS(int n, int i, float val){ aS[n - 1][i - 1] = val; }
	void setAB(int n, int i, float val){ aB[n - 1][i - 1] = val; }
	void setBN(int n, int i, float val){ bN[n - 1][i - 1] = val; }
	void setBS(int n, int i, float val){ bS[n - 1][i - 1] = val; }
	void setBB(int n, int i, float val){ bB[n - 1][i - 1] = val; }

	float getSinMeanN(int n) const { return sinMeanN[n - 1]; } 
	float getSinMeanS(int n) const { return sinMeanS[n - 1]; }
	float getSinMeanB(int n) const { return sinMeanB[n - 1]; }
	float getSinRmsN( int n) const { return sinRmsN[n - 1];  }
	float getSinRmsS( int n) const { return sinRmsS[n - 1];  }
	float getSinRmsB( int n) const { return sinRmsB[n - 1];  }
	float getCosMeanN(int n) const { return cosMeanN[n - 1]; }
	float getCosMeanS(int n) const { return cosMeanS[n - 1]; }
	float getCosMeanB(int n) const { return cosMeanB[n - 1]; }
	float getCosRmsN( int n) const { return cosRmsN[n - 1];  }
	float getCosRmsS( int n) const { return cosRmsS[n - 1];  }
	float getCosRmsB( int n) const { return cosRmsB[n - 1];  }
	
	// convenience methods for getting X,Y attributes
	float getYMeanN(int n, int nHits) const { return nHits*sinMeanN[n - 1]; }
	float getYMeanS(int n, int nHits) const { return nHits*sinMeanS[n - 1]; }
	float getYMeanB(int n, int nHits) const { return nHits*sinMeanB[n - 1]; }
	float getYRmsN (int n, int nHits) const { 
		return sqrt((double)nHits)*sinRmsN[n - 1];  }
	float getYRmsS (int n, int nHits) const { 
		return sqrt((double)nHits)*sinRmsS[n - 1];  }
	float getYRmsB (int n, int nHits) const { 
		return sqrt((double)nHits)*sinRmsB[n - 1];  }
	float getXMeanN(int n, int nHits) const { return nHits*cosMeanN[n - 1]; }
	float getXMeanS(int n, int nHits) const { return nHits*cosMeanS[n - 1]; }
	float getXMeanB(int n, int nHits) const { return nHits*cosMeanB[n - 1]; }
	float getXRmsN (int n, int nHits) const { 
		return sqrt((double)nHits)*cosRmsN[n - 1];  }
	float getXRmsS (int n, int nHits) const { 
		return sqrt((double)nHits)*cosRmsS[n - 1];  }
	float getXRmsB (int n, int nHits) const { 
		return sqrt((double)nHits)*cosRmsB[n - 1];  }

	float getPsiN(int n, double x, double y, int nHits);
	float getPsiS(int n, double x, double y, int nHits);
	float getPsiB(int n, double x, double y, int nHits);

	float getAlphaN(int n ) const { return alphaN[n - 1]; }
	float getAlphaS(int n ) const { return alphaS[n - 1]; }
	float getAlphaB(int n ) const { return alphaB[n - 1]; }

	float getAN(int n, int i) const { return aN[n - 1][i - 1]; }
	float getAS(int n, int i) const { return aS[n - 1][i - 1]; }
	float getAB(int n, int i) const { return aB[n - 1][i - 1]; }
	float getBN(int n, int i) const { return bN[n - 1][i - 1]; }
	float getBS(int n, int i) const { return bS[n - 1][i - 1]; }
	float getBB(int n, int i) const { return bB[n - 1][i - 1]; }

  virtual void print() const;

	void flattenN(int harmonic, double &dRp);
	void flattenS(int harmonic, double &dRp);
	void flattenB(int harmonic, double &dRp);

private:
	float sinMeanN[2];
	float sinMeanS[2];
	float sinMeanB[2];
	float sinRmsN[2];
	float sinRmsS[2];
	float sinRmsB[2];
	float cosMeanN[2];
	float cosMeanS[2];
	float cosMeanB[2];
	float cosRmsN[2];
	float cosRmsS[2];
	float cosRmsB[2];

	float alphaN[2];
	float alphaS[2];
	float alphaB[2];

	float aN[2][4];
	float aS[2][4];
	float aB[2][4];
	float bN[2][4];
	float bS[2][4];
	float bB[2][4];

  ClassDef(PdbMvdRp,2)
};

#endif /* __PDMVDRP_HH__ */
