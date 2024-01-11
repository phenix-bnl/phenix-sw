
#include "Track.hh"

//============================================================================
ClassImp(Track)

//============================================================================
Track::Track(){

};
//============================================================================
void Track::Reset(){
  int i;
  // Tracking information
  inters = 0;
  ptot = 0;
  cglarm = 0;
  quality = 0;
  pathl = 0;
  alpha = 0;
  beta = 0;
  i = 3;
  while( i-- ){
    proj[i] = 0;
    dir[i] = 0;
  }

  // RICH information
  crk_acc = 0 ;
  crk_npmt0 = 0;
  crk_npmt1 = 0;
  crk_npe0 = 0;
  crk_npe1 = 0;
  crk_chi2 = 0;
  crk_disp = 0;

  // RICH information after Z-flip
  crk_acc_s = 0 ;
  crk_npmt0_s = 0;
  crk_npmt1_s = 0;
  crk_npe0_s = 0;
  crk_npe1_s = 0;
  crk_chi2_s = 0;
  crk_disp_s = 0;

};
//
//============================================================================
Track* Track::operator=(nt_trk& r_nt_trk){
  // Tracking Information
  if (r_nt_trk.intsct/256 >= 1 )
    inters = 1;
  else
    inters = 0;
  ptot = r_nt_trk.mom;
  cglarm = (int)(r_nt_trk.armsid / 2);
  quality = r_nt_trk.quality;
  proj[0] = r_nt_trk.pemcx;
  proj[1] = r_nt_trk.pemcy;
  proj[2] = r_nt_trk.pemcz;
  dir[0] = sin(r_nt_trk.beta)*cos(r_nt_trk.phi-r_nt_trk.alpha); // FIX.ME!!!!!
  dir[1] = sin(r_nt_trk.beta)*sin(r_nt_trk.phi-r_nt_trk.alpha);
  dir[2] = cos(r_nt_trk.beta);
  pathl = r_nt_trk.plemc;
  alpha = r_nt_trk.alpha;
  beta = r_nt_trk.beta;
  
  // RICH information
  crk_acc = (int)r_nt_trk.acc;
  crk_npmt0 = (int)r_nt_trk.n0;
  crk_npmt1 = (int)r_nt_trk.n1;
  crk_npe0 = r_nt_trk.npe0;
  crk_npe1 = r_nt_trk.npe1;
  crk_chi2 = r_nt_trk.ch2;
  crk_disp = r_nt_trk.disp;

  // RICH information after Z-flip
  crk_acc_s = (int)r_nt_trk.sacc;
  crk_npmt0_s = (int)r_nt_trk.sn0;
  crk_npmt1_s = (int)r_nt_trk.sn1;
  crk_npe0_s = r_nt_trk.snpe0;
  crk_npe1_s = r_nt_trk.snpe1;
  crk_chi2_s = r_nt_trk.sch2;
  crk_disp_s = r_nt_trk.sdisp;


  return this;
};
//
//============================================================================
Track* Track::Set(mdst_run2tree& mdst,int ntrk){
  if( ntrk > MAX_TRK ) return NULL;
  // Tracking Information
  if (mdst.emcid[ntrk] >= 0 )
    inters = 1;
  else
    inters = 0;
  ptot = mdst.mom[ntrk];
  cglarm =  mdst.arm[ntrk];
  quality = mdst.quality[ntrk];
  proj[0] = mdst.pemcx[ntrk];
  proj[1] = mdst.pemcy[ntrk];
  proj[2] = mdst.pemcz[ntrk];
  dir[0] = sin(mdst.beta[ntrk])*cos(mdst.phi[ntrk]-mdst.alpha[ntrk]); // FIX.ME!!!!!
  dir[1] = sin(mdst.beta[ntrk])*sin(mdst.phi[ntrk]-mdst.alpha[ntrk]);
  dir[2] = cos(mdst.beta[ntrk]);
  pathl = mdst.plemc[ntrk];
  alpha = mdst.alpha[ntrk];
  beta = mdst.beta[ntrk];
  
  // RICH information
  crk_acc = (int)mdst.acc[ntrk];
  crk_npmt0 = (int)mdst.n0[ntrk];
  crk_npmt1 = 0; //(int)mdst.n1[ntrk];  // FIX.ME !!!!
  crk_npe0 = mdst.npe0[ntrk];
  crk_npe1 = 0; 
  crk_chi2 = mdst.chi2[ntrk];
  crk_disp = mdst.disp[ntrk];

  // RICH information after Z-flip
  crk_acc_s = (int)mdst.sacc[ntrk];
  crk_npmt0_s = (int)mdst.sn0[ntrk];
  crk_npmt1_s = 0; 
  crk_npe0_s = mdst.snpe0[ntrk];
  crk_npe1_s = 0; 
  crk_chi2_s = mdst.schi2[ntrk];
  crk_disp_s = mdst.sdisp[ntrk];


  return this;
};
//
//============================================================================
#ifdef DST_READING
Track* Track::Set(Dst* dst,int ntrk){
  if( ntrk > MAX_TRK ) return NULL;
  // Tracking Information
   ptot = dst->dDchTracks->get_momentum(ntrk);
  cglarm =  dst->dDchTracks->get_arm(ntrk);
  quality = dst->dDchTracks->get_quality(ntrk);
  if( cglarm == 1 && dst->dPHTrack->get_ifIntersectPbsc(ntrk) ){ //West
    proj[0] = dst->dPHTrack->get_projectionPbSc(0,ntrk);
    proj[1] = dst->dPHTrack->get_projectionPbSc(1,ntrk);
    proj[2] = dst->dPHTrack->get_projectionPbSc(2,ntrk);
    inters = 1;
  } else if( cglarm == 0 && dst->dPHTrack->get_ifIntersectPbgl(ntrk) ){ //East
    proj[0] = dst->dPHTrack->get_projectionPbGl(0,ntrk);
    proj[1] = dst->dPHTrack->get_projectionPbGl(1,ntrk);
    proj[2] = dst->dPHTrack->get_projectionPbGl(2,ntrk);
    inters = 1;
  } else {
    proj[0] = 0;
    proj[1] = 0;
    proj[2] = 0;
    inters = 0;
  }
  dir[0] = sin(dst->dDchTracks->get_beta(ntrk))*cos(dst->dDchTracks->get_phi(ntrk)-dst->dDchTracks->get_alpha(ntrk));
  dir[1] = sin(dst->dDchTracks->get_beta(ntrk))*sin(dst->dDchTracks->get_phi(ntrk)-dst->dDchTracks->get_alpha(ntrk));
  dir[2] = cos(dst->dDchTracks->get_beta(ntrk));
  pathl = dst->dPHTrack->get_emcPathLength(ntrk);
  alpha = dst->dDchTracks->get_alpha(ntrk);
  beta = dst->dDchTracks->get_beta(ntrk);
  
  // RICH information
  crk_acc = 0;
  crk_npmt0 = 0;
  crk_npmt1 = 0;
  crk_npe0 = 0;
  crk_npe1 = 0;
  crk_chi2 = 0;
  crk_disp = 0;

  // RICH information after Z-flip
  crk_acc_s = 0;
  crk_npmt0_s = 0;
  crk_npmt1_s = 0;
  crk_npe0_s = 0;
  crk_npe1_s = 0;
  crk_chi2_s = 0;
  crk_disp_s = 0;


  return this;
};
#endif
//
//============================================================================
