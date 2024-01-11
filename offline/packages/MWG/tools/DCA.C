// 06/27/2007 MXL
// calculate DCA between a muon track (using ST1 xyz and Pxyz) and the beam line
// copied from dca.C used for two muon tracks

#include "Tools.h"
#include <cmath>
#include <PHMuoTracksOut.h>

//___________________________________________________________________
void Tools::DCA(PHMuoTracksOut* &muo, Int_t idx1, Float_t &r_dca, Float_t &x_dca, Float_t &y_dca, Float_t &z_dca)
{
  Float_t a=0,b=0,c=0,d=0; /* equation du plan*/
  Float_t wx=0,wy=0,wz=0;
  Float_t X1=0,X2=0,Y1=0,Y2=0,Z1=0,Z2=0;
 
  //  Float_t r_dca = 0, x_dca=0, y_dca=0, z_dca =0;
  Float_t P1[2],xp[2],yp[2];
  Float_t p[2][3];
  Float_t x1[2]={0},y1[2]={0},z1[2]={0};
 
  x1[0] = muo->get_xpos(1,idx1);
  y1[0] = muo->get_ypos(1,idx1);
  z1[0] = muo->get_zpos(1,idx1);

  x1[1] = 0;
  y1[1] = 0;
  z1[1] = 0;

  p[0][0] = muo->get_px(1,idx1);
  p[0][1] = muo->get_py(1,idx1);
  p[0][2] = muo->get_pz(1,idx1);
 
  p[1][0] = 0;
  p[1][1] = 0;
  p[1][2] = 1;

 
  /* calcul de z dca */
  for(Int_t k =0;k<2;k++)
    {
      P1[k]=std::pow(p[k][0],2)+std::pow(p[k][1],2)+std::pow(p[k][2],2);
      P1[k]=std::sqrt(P1[k]);
      xp[k]=p[k][0]/p[k][2];
      yp[k]=p[k][1]/p[k][2];
      
      //Rdca[k] = xp[k]*x1[k]+yp[k]*y1[k]-z1[k]*pow(xp[k],2)-z1[k]*pow(yp[k],2);
      //Rdca[k] = -1*Rdca[k]/(pow(xp[k],2)+pow(yp[k],2));
      //ztemp[k]=Rdca[k];
      //Rdca[k] = pow(x1[k]+xp[k]*(Rdca[k]-z1[k]),2)+pow(y1[k]+yp[k]*(Rdca[k]-z1[k]),2);
      //Rdca[k]=sqrt(Rdca[k]);

    }
	
	/* calcul de w (produit vectoriel) */
	wx = yp[0]-yp[1];
	wy = xp[1]-xp[0];
	wz = xp[0]*yp[1]-xp[1]*yp[0];
	
	/*calcul des coeff pour eqn plan */
	
	a = wy-wz*yp[0];
	b = xp[0]*wz-wx;
	c = wx*yp[0]-xp[0]*wy;
	d = - x1[0]*a -y1[0]*b - z1[0]*c;
	
	/* sur la droite 2*/
	Z2 = a*(x1[1]-xp[1]*z1[1]) + b*(y1[1]-yp[1]*z1[1]) + d ;
	Z2 = -  Z2/(a*xp[1]+b*yp[1]+c);
	
	X2 = x1[1] + xp[1]*(Z2 - z1[1]);
	Y2 = y1[1] + yp[1]*(Z2 - z1[1]);
	
	
	Z1 = Z2 - xp[0]*(x1[0] - xp[0]*z1[0]- X2) - yp[0]*(y1[0] - yp[0]*z1[0] - Y2);
	Z1 = Z1/(1 +xp[0]*xp[0] + yp[0]*yp[0] );
	
	X1 = x1[0] + xp[0]*(Z1 - z1[0]);
	Y1 = y1[0] + yp[0]*(Z1 - z1[0]);
	
	z_dca =(Z1+Z2)/2; // z DCA vertex !
	y_dca =(Y1+Y2)/2;
	x_dca =(X1+X2)/2; 
	r_dca = std::pow((X2-X1),2)+std::pow((Y2-Y1),2)+std::pow((Z2-Z1),2);
	r_dca = std::sqrt(r_dca);
	return;
}
  
  
  

  

