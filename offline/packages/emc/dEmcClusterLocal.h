#ifndef __DEMCCLUSTERLOCAL_H__
#define __DEMCCLUSTERLOCAL_H__


typedef
struct dEmcClusterLocal {
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
} DEMCCLUSTERLOCAL_ST;
#endif /*__DEMCCLUSTERLOCAL_H__*/
