#ifndef __UTIPROTOTYPE_HH__
#define __UTIPROTOTYPE_HH__

/*
 * Prototype declarations for Utility Functions
 *
 * Original Author: Charles F. Maguire (Vanderbilt University)
 * Creation Date: July 6, 1998
 */

#ifdef __cplusplus
extern "C" {
#endif
  
  int utiLineCircle(float a, float b, float r,
		    float *x1, float *y1, 
		    float *x2, float *y2);
  
  void utiGaussian(float *mean, float *sigma, long *randseed, 
		   float *xgauss);
  
  void utiLocaltoGlobal(float XYZloc[], float XYZtranslate[], 
			float Theta, 
			float *Xglo, float *Yglo, float *Zglo);
  
  void utiPolya(long *randseed, float *xpolya);
  
  void utiRPhitoXY(float *r, float *phi, float *x, float *y);
  
  float utiRandom(long *ldum);
  
  void utiSwap(float *num1, float *num2);
  
  void utiXYtoRPhi(float *x, float *y, float *r, float *phi);
  
#ifdef __cplusplus
}
#endif

void utiPolyint (double xa[], double ya[], int n, 
		 double x, double *y, double *dy);

#endif /* __UTIPROTOTYPE_HH__ */
