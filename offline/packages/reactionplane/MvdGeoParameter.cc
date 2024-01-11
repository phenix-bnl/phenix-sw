#include "MvdGeoParameter.hh"
#include <iostream>

using namespace std;

void MvdGeoParameter :: Init()
{

  ang_ver_plena = 20.0;
  dr_ver_enc1 = 0.001;
  dr_ver_enc2 = 0.64;
  dr_ver_strut = 0.05;
  dvisl[0] = 0.015;
  dvisl[1] = 2.5;
  dvisl[2] = 31.8;
  dvoel[0] = 2.0;
  dvoel[1] = 0.055;
  dvoel[2] = 2.4;
  dvosl[0] = 0.015;
  dvosl[1] = 4.0;
  dvosl[2] = 31.8;
  dvroh[0] = 2.89541;
  dvroh[1] = 4.32147;
  dvroh[2] = 31.8;
  dvroh[3] = 1.235;
  dvwr1[0] = 0.015;
  dvwr1[1] = 2.6;
  dvwr1[2] = 2.65;
  dvwr2[0] = 0.015;
  dvwr2[1] = 3.725;
  dvwr2[2] = 2.65;
  dz_ver_enc = 37.2;
  dz_ver_endpl = 0.109;
  dz_ver_mopl = 0.048;
  dz_ver_pmb = 0.16;
  nph_segs = 6.0;
  pitch = 0.02;
  r1_ver_enc = 4.83;
  r1_ver_pad = 5.0;
  r2_ver_enc = 23.165;
  r2_ver_pad = 12.0;
  r_ver_mopl = 8.0;
  r_ver_plena = 11.5;
  r_ver_pmb = 19.0;
  r_ver_strut = 1.22;
  sv1ph = -120.0;
  ver_el_space = 0.8;
  ver_plnm_thwall = 1.0;
  ver_th_bus_cable = 0.043;
  ver_th_cabl = 0.005;
  ver_w_bus_cable = 3.0;
  ver_z_center = 0.0;
  vislr = 4.985;
  voslr = 7.485;
  wdvroh = 0.6;
  z_ver_mopl = 32.1;
  z_ver_pad = 34.975;
  z_ver_pmb = 35.15;
  med_ver_bus_cable = 108;
  med_ver_cabl = 108;
  med_ver_enc1 = 106;
  med_ver_enc2 = 102;
  med_ver_endpl = 106;
  med_ver_he = 19;
  med_ver_ins = 101;
  med_ver_mcm = 107;
  med_ver_mopl = 106;
  med_ver_pmb = 107;
  med_ver_roh = 102;
  med_ver_sen = 105;
  med_ver_strut = 206;
  nhvroh = 12;
  nstrip = 256;
  nwhole = 8;
  nwperp = 12;
}


void MvdGeoParameter :: Show()
{

  cout << endl << "MvdGeoParameter";
  cout << endl << "ang_ver_plena=" << ang_ver_plena;
  cout << endl << "dr_ver_enc1=" << dr_ver_enc1;
  cout << endl << "dr_ver_enc2=" << dr_ver_enc2;
  cout << endl << "dr_ver_strut=" << dr_ver_strut;
  cout << endl << "dvisl[0]=" << dvisl[0];
  cout << endl << "dvisl[1]=" << dvisl[1];
  cout << endl << "dvisl[2]=" << dvisl[2];
  cout << endl << "dvoel[0]=" << dvoel[0];
  cout << endl << "dvoel[1]=" << dvoel[1];
  cout << endl << "dvoel[2]=" << dvoel[2];
  cout << endl << "dvosl[0]=" << dvosl[0];
  cout << endl << "dvosl[1]=" << dvosl[1];
  cout << endl << "dvosl[2]=" << dvosl[2];
  cout << endl << "dvroh[0]=" << dvroh[0];
  cout << endl << "dvroh[1]=" << dvroh[1];
  cout << endl << "dvroh[2]=" << dvroh[2];
  cout << endl << "dvroh[3]=" << dvroh[3];
  cout << endl << "dvwr1[0]=" << dvwr1[0];
  cout << endl << "dvwr1[1]=" << dvwr1[1];
  cout << endl << "dvwr1[2]=" << dvwr1[2];
  cout << endl << "dvwr2[0]=" << dvwr2[0];
  cout << endl << "dvwr2[1]=" << dvwr2[1];
  cout << endl << "dvwr2[2]=" << dvwr2[2];
  cout << endl << "dz_ver_enc=" << dz_ver_enc;
  cout << endl << "dz_ver_endpl=" << dz_ver_endpl;
  cout << endl << "dz_ver_mopl=" << dz_ver_mopl;
  cout << endl << "dz_ver_pmb=" << dz_ver_pmb;
  cout << endl << "nph_segs=" << nph_segs;
  cout << endl << "pitch=" << pitch;
  cout << endl << "r1_ver_enc=" << r1_ver_enc;
  cout << endl << "r1_ver_pad=" << r1_ver_pad;
  cout << endl << "r2_ver_enc=" << r2_ver_enc;
  cout << endl << "r2_ver_pad=" << r2_ver_pad;
  cout << endl << "r_ver_mopl=" << r_ver_mopl;
  cout << endl << "r_ver_plena=" << r_ver_plena;
  cout << endl << "r_ver_pmb=" << r_ver_pmb;
  cout << endl << "r_ver_strut=" << r_ver_strut;
  cout << endl << "sv1ph=" << sv1ph;
  cout << endl << "ver_el_space=" << ver_el_space;
  cout << endl << "ver_plnm_thwall=" << ver_plnm_thwall;
  cout << endl << "ver_th_bus_cable=" << ver_th_bus_cable;
  cout << endl << "ver_th_cabl=" << ver_th_cabl;
  cout << endl << "ver_w_bus_cable=" << ver_w_bus_cable;
  cout << endl << "ver_z_center=" << ver_z_center;
  cout << endl << "vislr=" << vislr;
  cout << endl << "voslr=" << voslr;
  cout << endl << "wdvroh=" << wdvroh;
  cout << endl << "z_ver_mopl=" << z_ver_mopl;
  cout << endl << "z_ver_pad=" << z_ver_pad;
  cout << endl << "z_ver_pmb=" << z_ver_pmb;
  cout << endl << "med_ver_bus_cable=" << med_ver_bus_cable;
  cout << endl << "med_ver_cabl=" << med_ver_cabl;
  cout << endl << "med_ver_enc1=" << med_ver_enc1;
  cout << endl << "med_ver_enc2=" << med_ver_enc2;
  cout << endl << "med_ver_endpl=" << med_ver_endpl;
  cout << endl << "med_ver_he=" << med_ver_he;
  cout << endl << "med_ver_ins=" << med_ver_ins;
  cout << endl << "med_ver_mcm=" << med_ver_mcm;
  cout << endl << "med_ver_mopl=" << med_ver_mopl;
  cout << endl << "med_ver_pmb=" << med_ver_pmb;
  cout << endl << "med_ver_roh=" << med_ver_roh;
  cout << endl << "med_ver_sen=" << med_ver_sen;
  cout << endl << "med_ver_strut=" << med_ver_strut;
  cout << endl << "nhvroh=" << nhvroh;
  cout << endl << "nstrip=" << nstrip;
  cout << endl << "nwhole=" << nwhole;
  cout << endl << "nwperp=" << nwperp;
}


