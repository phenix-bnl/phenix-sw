
#include "Clust.hh"

//============================================================================
ClassImp(Clust)

//============================================================================
Clust::Clust(){

};
//============================================================================
void Clust::Reset(){
  int i;
  // EMCal Clustering information
  arm = 0;
  sector = 0;
  twrhit = 0;
  clustno = 0;
  nsh = 0;
  tof = 0;
  e = 0;
  ecent = 0;
  ecore = 0;
  ecorr = 0;
  tofcorr = 0;
  prob = 0;
  qa = 0;
  i = 3;
  while( i-- ){
    pos[i] = 0;
  }
  i = 2;
  while( i-- ){
    disp[i] = 0;
    padisp[i] = 0;
    ind[i] = 0;
  }
  // For 2x2/4x4 trigger study(From DST only)
  epart = 0;
  e22 = 0;
  e44 = 0;

};
//
//============================================================================
Clust* Clust::operator=(nt_emc& r_nt_emc){

   // EMCal Clustering information
   if( r_nt_emc.swkey>0 ){
     arm = (int)(r_nt_emc.swkey / 100000);
     sector = (int)((r_nt_emc.swkey-100000*arm)/10000);
     ind[1] = (int)((r_nt_emc.swkey-100000*arm-10000*sector)/100);
     ind[0] = (int)( r_nt_emc.swkey-100000*arm-10000*sector-100*ind[1]);
   } else {
     arm = -1;
     sector = 0;
     ind[1] = 0;
     ind[0] = 0;
   }

   tof = r_nt_emc.emct;
   e = r_nt_emc.e;
   ecent = r_nt_emc.ecent;
   ecore = r_nt_emc.ecore;
   ecorr = r_nt_emc.ecorr;
   tofcorr = r_nt_emc.emctc;
   prob = r_nt_emc.prob;
   pos[0] = r_nt_emc.emcx;
   pos[1] = r_nt_emc.emcy;
   pos[2] = r_nt_emc.emcz;
   //disp[],padisp[]..
   //--------------------------------------------------------
  

  return this;
};
//============================================================================
Clust* Clust::Set(mdst_run2tree& mdst,int nemc){
  if( nemc > MAX_EMC ) return NULL;

   // EMCal Clustering information
  arm = (int)mdst.emarm[nemc];
  sector = (int)mdst.emsect[nemc];
  ind[1] = (int)mdst.emysect[nemc];
  ind[0] = (int)mdst.emzsect[nemc];
  twrhit = (int)mdst.emntwr[nemc];
  e = mdst.eme[nemc];
  ecent = mdst.emecent[nemc];
  ecore = mdst.emecore[nemc];
  ecorr = mdst.emecorr[nemc];
  tof = mdst.emtmax[nemc];      //FIX.ME
  tofcorr = mdst.emtc[nemc] - mdst.bbct0 ;
  prob = mdst.emprob[nemc];
  pos[0] = mdst.emcx[nemc];
  pos[1] = mdst.emcy[nemc];
  pos[2] = mdst.emcz[nemc];
  //disp[],padisp[]..

  return this;
};
//============================================================================
Clust* Clust::Set(Dst* dst,int nemc){
  if( nemc > dst->dEmcClusterLocalExt->RowCount() ) return NULL;
   // EMCal Clustering information
  arm = (int)dst->dEmcClusterLocalExt->get_arm(nemc);
  sector = (int)dst->dEmcClusterLocalExt->get_sector(nemc);
  ind[1] = (int)dst->dEmcClusterLocalExt->get_ind(1,nemc);
  ind[0] = (int)dst->dEmcClusterLocalExt->get_ind(0,nemc);
  twrhit = (int)dst->dEmcClusterLocalExt->get_twrhit(nemc);
  e = dst->dEmcClusterLocalExt->get_e(nemc);
  ecent = dst->dEmcClusterLocalExt->get_ecent(nemc);
  ecore = dst->dEmcClusterLocalExt->get_ecore(nemc);
  ecorr = dst->dEmcClusterLocalExt->get_ecorr(nemc);
  tof = dst->dEmcClusterLocalExt->get_tof(nemc);
  tofcorr = dst->dEmcClusterLocalExt->get_tofcorr(nemc) - dst->dEventHeader->get_bbcTimeZero(0);
  prob = dst->dEmcClusterLocalExt->get_prob_photon(nemc);
  pos[0] = dst->dEmcClusterLocalExt->get_xyz(0,nemc);
  pos[1] = dst->dEmcClusterLocalExt->get_xyz(1,nemc);
  pos[2] = dst->dEmcClusterLocalExt->get_xyz(2,nemc);
  disp[0] = dst->dEmcClusterLocalExt->get_disp(0,nemc);
  disp[1] = dst->dEmcClusterLocalExt->get_disp(1,nemc);
  padisp[0] = dst->dEmcClusterLocalExt->get_padisp(0,nemc);
  padisp[1] = dst->dEmcClusterLocalExt->get_padisp(1,nemc);
  clustno = dst->dEmcClusterLocalExt->get_clusno(nemc);
  //------------------------------------------------------ Calculation e22,e44
  int iz22 = (int)(ind[0] / 2) * 2;
  int iy22 = (int)(ind[1] / 2) * 2;
  e22 = 0;
  e44 = 0;
  epart = 0;
  float e44_candidate[4];
  e44_candidate[0] = 0; e44_candidate[1] = 0;
  e44_candidate[2] = 0; e44_candidate[3] = 0;
  float parte;
  int iarm,isect,iy,iz;
  int iswkey;
  int n = dst->dEmcClusterLocalExt->get_twrhit(nemc);
  while( n-- ){
    iswkey = dst->dEmcClusterLocalExt->get_twrlist(n,nemc);
    parte = dst->dEmcClusterLocalExt->get_partesum(n,nemc);
    if( n>0 && parte > 0 )
      parte -= dst->dEmcClusterLocalExt->get_partesum(n-1,nemc);
    if( iswkey > 0 ){
      iarm = (int)(iswkey / 100000);
      isect = (int)((iswkey-iarm*100000)/10000);
      iy = (int)((iswkey-100000*iarm-10000*isect)/100);
      iz = (int)( iswkey-100000*iarm-10000*isect-100*iy);
      if( iarm == arm && isect == sector ){
	epart += parte;
	if( iz >= iz22 && iz < iz22 + 2 && iy >= iy22 && iy < iy22 + 2)
	  e22 += parte;
	if( iz >= iz22 && iz < iz22 + 4 && iy >= iy22 && iy < iy22 + 4)
	  e44_candidate[0] += parte;
	if( iz >= iz22 -2 && iz < iz22 + 2 && iy >= iy22 && iy < iy22 + 4)
	  e44_candidate[1] += parte;
	if( iz >= iz22 && iz < iz22 + 4 && iy >= iy22 -2 && iy < iy22 + 2)
	  e44_candidate[2] += parte;
	if( iz >= iz22 -2 && iz < iz22 + 2 && iy >= iy22 -2 && iy < iy22 + 2)
	  e44_candidate[3] += parte;
      }
    } else
      n = 0;
  }
  e44_candidate[0] = e44_candidate[0] > e44_candidate[1] ? e44_candidate[0] : e44_candidate[1];
  e44_candidate[2] = e44_candidate[2] > e44_candidate[3] ? e44_candidate[2] : e44_candidate[3];
  e44 = e44_candidate[0] > e44_candidate[2] ? e44_candidate[0] : e44_candidate[2];

  return this;
};


