//==================================================================
#ifndef _clust_tofread_hh_
#define _clust_tofread_hh_
//==================================================================

#ifdef COMPILE
#include <TH1.h>
#include <TH2.h>
#include <TF1.h>
#endif

void clust_tofread();

#ifdef GLOBAL
TH1F* h_tofbbc_slipar[8][4];
TH1F* h_tofbbc_slipar_err[8][4];
TH1F* h_tofbbcpar[8][3];
TH1F* h_tofbbcpar_err[8][3];
TH2F* h2_tofbbcpar[8][3];
TH2F* h2_tofbbcpar_stat[8];
TH1F* h_tofbbcpar_t0[8];
TH1F* h_tofbbcpar_lc[8];
TH2F* h2_tofbbcpar_t0[8];
TH2F* h2_tofbbcpar_lc[8];
float tofbbcpar_lc[24768];
float tofbbcpar_lc_err[24768];
float tofbbcpar_lc_stat[24768];
//
TH1F* h_tofe05par[8][4];
TH1F* h_tofe05par_err[8][4];
TH2F* h2_tofe05par[8][4];
TH2F* h2_tofe05par_stat[8];
float tofe05par_t0[24768];
float tofe05par_t0_err[24768];
float tofe05par_t0_stat[24768];
//
TH1F* h_tof_pbscglrunall[2];
float tof_pbscglrunall[2][20000];
float tof_pbscglrunall_err[2][20000];
float tof_pbscglrunall_stat[2][20000];

#else
extern TH1F* h_tofbbc_slipar[8][4];
extern TH1F* h_tofbbc_slipar_err[8][4];
extern TH1F* h_tofbbcpar[8][3];
extern TH1F* h_tofbbcpar_err[8][3];
extern TH2F* h2_tofbbcpar[8][3];
extern TH2F* h2_tofbbcpar_stat[8];
extern TH1F* h_tofbbcpar_t0[8];
extern TH1F* h_tofbbcpar_lc[8];
extern TH2F* h2_tofbbcpar_t0[8];
extern TH2F* h2_tofbbcpar_lc[8];
extern float tofbbcpar_lc[24768];
extern float tofbbcpar_lc_err[24768];
extern float tofbbcpar_lc_stat[24768];
//
extern TH1F* h_tofe05par[8][4];
extern TH1F* h_tofe05par_err[8][4];
extern TH2F* h2_tofe05par[8][4];
extern TH2F* h2_tofe05par_stat[8];
extern float tofe05par_t0[24768];
extern float tofe05par_t0_err[24768];
extern float tofe05par_t0_stat[24768];
//
extern TH1F* h_tof_pbscglrunall[2];
extern float tof_pbscglrunall[2][20000];
extern float tof_pbscglrunall_err[2][20000];
extern float tof_pbscglrunall_stat[2][20000];

#endif


#endif
//==================================================================

