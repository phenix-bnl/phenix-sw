//-----------------------------------------------------------------------------
//  $header$
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999
//
//  Implementation of class PdbMvdRp
//
//  Author: bnorman
//-----------------------------------------------------------------------------
#include "PdbMvdRp.hh"

#include <iostream>
#include <string.h>

using namespace std;

PdbMvdRp::PdbMvdRp()
{
	for(int i = 0; i<2; i++){
		sinMeanN[i] = 0;
		sinMeanS[i] = 0;
		sinMeanB[i] = 0;
		cosMeanN[i] = 0;
		cosMeanS[i] = 0;
		cosMeanB[i] = 0;
		sinRmsN[i] = 1;
		sinRmsS[i] = 1;
		sinRmsB[i] = 1;
		cosRmsN[i] = 1;
		cosRmsS[i] = 1;
		cosRmsB[i] = 1;

		alphaN[i] = 0;
		alphaS[i] = 0;
		alphaB[i] = 0;

		for(int j = 0; j<4; j++){
			aN[i][j] = 0;
			aS[i][j] = 0;
			aB[i][j] = 0;
			bN[i][j] = 0;
			bS[i][j] = 0;
			bB[i][j] = 0;
		}
	}
}

PdbMvdRp::~PdbMvdRp()
{
}

PdbMvdRp &
PdbMvdRp::operator=(PdbMvdRp const &p){
	int size = 2*sizeof(float);

	memcpy(sinMeanN, p.sinMeanN, size);
	memcpy(sinMeanS, p.sinMeanS, size);
	memcpy(sinMeanB, p.sinMeanB, size);
	memcpy(sinRmsN,  p.sinRmsN,  size);
	memcpy(sinRmsS,  p.sinRmsS,  size);
	memcpy(sinRmsB,  p.sinRmsB,  size);
	memcpy(cosMeanN, p.cosMeanN, size);
	memcpy(cosMeanS, p.cosMeanS, size);
	memcpy(cosMeanB, p.cosMeanB, size);
	memcpy(cosRmsN,  p.cosRmsN,  size);
	memcpy(cosRmsS,  p.cosRmsS,  size);
	memcpy(cosRmsB,  p.cosRmsB,  size);

	memcpy(alphaN, p.alphaN, size);
	memcpy(alphaS, p.alphaS, size);
	memcpy(alphaB, p.alphaB, size);

	size *= 4;
	
	memcpy(aN, p.aN, size);
	memcpy(aS, p.aS, size);
	memcpy(aB, p.aB, size);
	memcpy(bN, p.bN, size);
	memcpy(bS, p.bS, size);
	memcpy(bB, p.bB, size);

	return *this;
}

float 
PdbMvdRp::getPsiN(int n, double x, double y, int nHits){
	// shift by means
	x -= getXMeanN(n, nHits);
	y -= getYMeanN(n, nHits);

	// rotate to "prime" space, where the axes are those of the X-Y ellipse.
	double alpha = getAlphaN(n);
	double X =  x*cos(-alpha) + y*sin(-alpha);
	double Y = -x*sin(-alpha) + y*cos(-alpha);

	// normalize by means
	X /= getXRmsN(n, nHits);
	Y /= getYRmsN(n, nHits);

	// rotate back to "unprime" space
	x =  X*cos(alpha) + Y*sin(alpha);
	y = -X*sin(alpha) + Y*cos(alpha);
	
	// fourier shifting
	double dPsi = atan2(y, x)/n;
	flattenN(n, dPsi);

	return dPsi;	
}

float 
PdbMvdRp::getPsiS(int n, double x, double y, int nHits){
	// shift by means
	x -= getXMeanS(n, nHits);
	y -= getYMeanS(n, nHits);

	// rotate to "prime" space, where the axes are those of the X-Y ellipse.
	double alpha = getAlphaS(n);
	double X =  x*cos(-alpha) + y*sin(-alpha);
	double Y = -x*sin(-alpha) + y*cos(-alpha);

	// normalize by means
	X /= getXRmsS(n, nHits);
	Y /= getYRmsS(n, nHits);

	// rotate back to "unprime" space
	x =  X*cos(alpha) + Y*sin(alpha);
	y = -X*sin(alpha) + Y*cos(alpha);
	
	// fourier shifting
	double dPsi = atan2(y, x)/n;
	flattenS(n, dPsi);

	return dPsi;	
}

float 
PdbMvdRp::getPsiB(int n, double x, double y, int nHits){
	// shift by means
	x -= getXMeanB(n, nHits);
	y -= getYMeanB(n, nHits);

	// rotate to "prime" space, where the axes are those of the X-Y ellipse.
	double alpha = getAlphaB(n);
	double X =  x*cos(-alpha) + y*sin(-alpha);
	double Y = -x*sin(-alpha) + y*cos(-alpha);

	// normalize by means
	X /= getXRmsB(n, nHits);
	Y /= getYRmsB(n, nHits);

	// rotate back to "unprime" space
	x =  X*cos(alpha) + Y*sin(alpha);
	y = -X*sin(alpha) + Y*cos(alpha);
	
	// fourier shifting
	double dPsi = atan2(y, x)/n;
	flattenB(n, dPsi);

	return dPsi;	
}

void
PdbMvdRp::print() const
{
	cout << "<sin 1N/1S/1B>    = " << sinMeanN[0] << "\t"
			 << sinMeanS[0] << "\t" << sinMeanB[0] << endl;
	cout << "RMS(sin 1N/1S/1B) = " << sinRmsN[0] << "\t"
			 << sinRmsS[0] << "\t" << sinRmsB[0] << endl;
	cout << "<Y 1N/1S/1B>    = " << getYMeanN(1, 90) << "\t"
			 << getYMeanS(1, 70) << "\t" << getYMeanB(1, 160) << endl;
	cout << "RMS(Y 1N/1S/1B) = " << getYRmsN(1, 90) << "\t"
			 << getYRmsS(1, 70) << "\t" << getYRmsB(1, 160) << endl;
	cout << "<sin 2N/2S/2B>    = " << sinMeanN[1] << "\t"
			 << sinMeanS[1] << "\t" << sinMeanB[1] << endl;
	cout << "RMS(sin 2N/2S/2B) = " << sinRmsN[1] << "\t"
			 << sinRmsS[1] << "\t" << sinRmsB[1] << endl;
	cout << "<Y 2N/2S/2B>    = " << getYMeanN(2, 90) << "\t"
			 << getYMeanS(2, 70) << "\t" << getYMeanB(2, 160) << endl;
	cout << "RMS(Y 2N/2S/2B) = " << getYRmsN(2, 90) << "\t"
			 << getYRmsS(2, 70) << "\t" << getYRmsB(2, 160) << endl;
	cout << "<cos 1N/1S/1B>    = " << cosMeanN[0] << "\t"
			 << cosMeanS[0] << "\t" << cosMeanB[0] << endl;
	cout << "RMS(cos 1N/1S/1B) = " << cosRmsN[0] << "\t"
			 << cosRmsS[0] << "\t" << cosRmsB[0] << endl;
	cout << "<X 1N/1S/1B>    = " << getXMeanN(1, 90) << "\t"
			 << getXMeanS(1, 70) << "\t" << getXMeanB(1, 160) << endl;
	cout << "RMS(X 1N/1S/1B) = " << getXRmsN(1, 90) << "\t"
			 << getXRmsS(1, 70) << "\t" << getXRmsB(1, 160) << endl;
	cout << "<cos 2N/2S/2B>    = " << cosMeanN[1] << "\t"
			 << cosMeanS[1] << "\t" << cosMeanB[1] << endl;
	cout << "RMS(cos 2N/2S/2B) = " << cosRmsN[1] << "\t"
			 << cosRmsS[1] << "\t" << cosRmsB[1] << endl;
	cout << "<X 2N/2S/2B>    = " << getXMeanN(2, 90) << "\t"
			 << getXMeanS(2, 70) << "\t" << getXMeanB(2, 160) << endl;
	cout << "RMS(X 2N/2S/2B) = " << getXRmsN(2, 90) << "\t"
			 << getXRmsS(2, 70) << "\t" << getXRmsB(2, 160) << endl;

	cout << "alpha(1N/1S/1B) = " << getAlphaN(1) << "\t"
			 << getAlphaS(1) << "\t" << getAlphaB(1) << "\t" << endl;
	cout << "alpha(2N/2S/2B) = " << getAlphaN(2) << "\t"
			 << getAlphaS(2) << "\t" << getAlphaB(2) << "\t" << endl;

	/*
	for(int i=1; i<=4; i++){
		cout << "A1N_" << i << " = " << getAN(1, i) 
				 << ", A1S_" << i << " = " << getAS(1, i)
				 << ", A1B_" << i << " = " << getAB(1, i) << endl;
		cout << "B1N_" << i << " = " << getBN(1, i) 
				 << ", B1S_" << i << " = " << getBS(1, i)
				 << ", B1B_" << i << " = " << getBB(1, i) << endl;
		cout << "A2N_" << i << " = " << getAN(2, i) 
				 << ", A2S_" << i << " = " << getAS(2, i)
				 << ", A2B_" << i << " = " << getAB(2, i) << endl;
		cout << "B2N_" << i << " = " << getBN(2, i) 
				 << ", B2S_" << i << " = " << getBS(2, i)
				 << ", B2B_" << i << " = " << getBB(2, i) << endl;
	}
	*/
}

void
PdbMvdRp::flattenN(int harmonic, double &dRp)
{		
	// shift is applied to n*psi.
	dRp *= harmonic;
	
	// do 4 orders of shifting
	for(int i = 1; i<=4; i++){
		dRp += getAN(harmonic, i)*cos(i*dRp) + getBN(harmonic, i)*sin(i*dRp);
	}
		
	// make sure the shifted value is in the proper range
	while(dRp>M_PI)  dRp -= 2*M_PI;
	while(-M_PI>dRp) dRp += 2*M_PI;
		
	dRp /= harmonic;
}

void
PdbMvdRp::flattenS(int harmonic, double &dRp)
{		
	// shift is applied to n*psi.
	dRp *= harmonic;
	
	// do 4 orders of shifting
	for(int i = 1; i<=4; i++){
		dRp += getAS(harmonic, i)*cos(i*dRp) + getBS(harmonic, i)*sin(i*dRp);
	}
		
	// make sure the shifted value is in the proper range
	while(dRp>M_PI)  dRp -= 2*M_PI;
	while(-M_PI>dRp) dRp += 2*M_PI;
		
	dRp /= harmonic;
}

void
PdbMvdRp::flattenB(int harmonic, double &dRp)
{		
	// shift is applied to n*psi.
	dRp *= harmonic;
	
	// do 4 orders of shifting
	for(int i = 1; i<=4; i++){
		dRp += getAB(harmonic, i)*cos(i*dRp) + getBB(harmonic, i)*sin(i*dRp);
	}
		
	// make sure the shifted value is in the proper range
	while(dRp>M_PI)  dRp -= 2*M_PI;
	while(-M_PI>dRp) dRp += 2*M_PI;
		
	dRp /= harmonic;
}
