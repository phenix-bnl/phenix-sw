#ifndef __DEMCCLUSTERLOCALEXT_H__
#define __DEMCCLUSTERLOCALEXT_H__


typedef struct {
   short id;
   int runno;
   int evno;
   short clusno;
   short method;
   short type;
   short arm;
   short sector;
   float xyz[3];
   float dxyz[3];
   float e;
   float ecore;
   float ecorr;
   float de;
   float tof;
   float ecent;
   float tofcorr;
   float dtof;
   float qual;
  int deadmap; // MV 2001/12/04
  int warnmap; // MV 2001/12/04
   float pid;
   float prob_photon;
   float prob_neuhad;
   float chi2;
   short nsh;
   float chi2_sh;
   float prob_photon_sh;
   float e_sh[2];
   float ecorr_sh[2];
   float de_sh[2];
   float xyz_sh[2][3];
   float dxyz_sh[2][3];
   float theta;
   float phi;
   float unitv[3];
   short ind[2];
   short twrhit;
   float tofmin;
   float etofmin;
   float tofmincorr;
   float tofmax;
   float etofmax;
   float tofmaxcorr;
   float tofmean;
   float disp[2];
   float padisp[2];
   float partesum[16];
   int twrlist[16];
   float e9;
   float re9;
   float yz_cg[2];
} DEMCCLUSTERLOCALEXT_ST;

typedef DEMCCLUSTERLOCALEXT_ST dEmcClusterLocalExt;
#endif /*__DEMCCLUSTERLOCALEXT_H__*/
