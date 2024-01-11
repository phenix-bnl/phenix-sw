////////////////////////////////////////////////////////////////
//  DchMcRecoTrack.C
//  Author:Federica Messer 
//  $Log: DchMcRecoTrack.C,v $
//  Revision 1.11  2002/05/12 03:04:28  jjia
//  evalator was modified
//
//  Revision 1.10  2002/05/02 23:48:13  jjia
//  add new variables
//
//  Revision 1.9  2002/04/03 14:35:54  dave
//  Clean ups.  Extra blank lines, commented-out code, removed.
//
//  Revision 1.8  2002/02/23 03:59:29  momchil
//  merged the latest changes of prod_2_4 into the HEAD branch
//
//  Revision 1.5.2.2  2002/02/20 22:13:05  pinkenbu
//  Here comes the head branch...
//
//  Revision 1.7  2002/02/13 04:53:32  jjia
//  add phivtx to evaluation ntuple
//
//  Revision 1.5  2001/12/09 13:40:44  jjia
//  extend the evaluation such that it contains Pc2 and also matching in term of number of sigmas for various subsystem
//
////////////////////////////////////////////////////////////////
#include "DchMcRecoTrack.hh"

DchMcRecoTrack::DchMcRecoTrack()
{
  eventID = -9999;
  eventxvtx = -9999;
  eventyvtx = -9999;
  eventzvtx= -9999;
  perfID= -9999;
  perfQual= -9999;
  momentumG= -9999;
  theta= -9999;
  phi= -9999;
  theta0G= -9999;
  phi0G= -9999;
  alphaG= -9999;
  betaG= -9999;
  zedG= -9999;
  generation= -9999;
  particleID= -9999;
  parentID= -9999;
  primaryID= -9999;
  rvtx= -9999;
  zvtx= -9999;
  phivtx = -9999;

  recoID= -9999;
  recoQual= -9999;
  momentumR= -9999;
  theta0R= -9999;
  phi0R= -9999;
  xhits= -9999;
  uvhits= -9999;
  mulcontrib= -9999;
  xmulcontrib= -9999;
  uvmulcontrib= -9999;

  mainID= -9999;
  xmainID= -9999;
  uvmainID= -9999;
  ambig= -9999;
  xambig= -9999;
  uvambig= -9999;
  purity= -9999;
  xpurity= -9999;
  uvpurity= -9999;
  dalpha= -9999;

  dbeta= -9999;
  dphi= -9999;
  dzed= -9999;
  ddist= -9999;
  sumfound= -9999;
  solution= -9999;
  perfDvertex= -9999;
  recoDvertex= -9999;
  chi2= -9999;
  numHitsFit= -9999;
  dalphaMin= -9999;
  dphiMin= -9999;
  avDist= -9999;
 
  pc1RecoId= -9999;
  pc2RecoId= -9999;
  pc3RecoId= -9999;
  emcRecoId= -9999;
  tecRecoId= -9999;
  tofRecoId= -9999;
   
  // if correct association then  McId == RecoId
  pc1McId= -9999; // the (Reconstructed) id that correspond to the MC hit
  pc2McId= -9999;
  pc3McId= -9999;
  emcMcId= -9999;
  tecMcId= -9999;
  tofMcId= -9999;
 
  xPc1Reco= -9999;
  yPc1Reco= -9999;
  zPc1Reco= -9999;

  xPc2Reco= -9999;
  yPc2Reco= -9999;
  zPc2Reco= -9999;

  xPc3Reco= -9999;
  yPc3Reco= -9999;
  zPc3Reco= -9999;

  xTecReco= -9999;
  yTecReco= -9999;
  zTecReco= -9999;

  xEmcReco= -9999;
  yEmcReco= -9999;
  zEmcReco= -9999;

  xTofReco= -9999;
  yTofReco= -9999;
  zTofReco= -9999;

  //---------------------
  xPc1Mc= -9999;
  yPc1Mc= -9999;
  zPc1Mc= -9999;

  xPc2Mc= -9999;
  yPc2Mc= -9999;
  zPc2Mc= -9999;

  xPc3Mc= -9999;
  yPc3Mc= -9999;
  zPc3Mc= -9999;

  xTecMc= -9999;
  yTecMc= -9999;
  zTecMc= -9999;

  xEmcMc= -9999;
  yEmcMc= -9999;
  zEmcMc= -9999;

  xTofMc= -9999;
  yTofMc= -9999;
  zTofMc= -9999;

  idGeantTrack = -9999;
 
  xPc1Proj = -9999;
  yPc1Proj = -9999;
  zPc1Proj = -9999;

  xPc2Proj = -9999;
  yPc2Proj = -9999;
  zPc2Proj = -9999;

  xPc3Proj = -9999;
  yPc3Proj = -9999;
  zPc3Proj = -9999;

  xTofProj = -9999;
  yTofProj = -9999;
  zTofProj = -9999;

  xTecProj = -9999;
  yTecProj = -9999;
  zTecProj = -9999;

  xEmcProj = -9999;
  yEmcProj = -9999;
  zEmcProj = -9999;

  pc13vtxr = -9999;
  pc13vtxm = -9999;
  bbcvtx = -9999;
  bbct0  = -9999;

  pathlEmc = -9999;
  emcmease = -9999;
  emcecore = -9999;
  emcecorr = -9999;
  emcecent = -9999;
  emctof = -9999;
  emctofmin = -9999;
  emcprobphot = -9999;
  emcchi2 = -9999;
  emcpartesum0 = -9999;
  emcpartesum1 = -9999;
  emcpartesum2 = -9999;
  emcpartesum3 = -9999;
  emcAncedep0 = -9999;
  emcAncedep1 = -9999;
  emcAncedep2 = -9999;
  emcAncptot0 = -9999;
  emcAncptot1 = -9999;
  emcAncptot2 = -9999;
  emcMcefrac = -9999;
  emcMcecore = -9999;
  emcMcmease = -9999;
  emcMctof = -9999;
  emcswkey = -9999;
  twrhit = -9999;
  emcAnctrkno0 = -9999;
  emcAnctrkno1 = -9999;
  emcAnctrkno2 = -9999;
  emcAnctwrhit0 = -9999;
  emcAnctwrhit1 = -9999;
  emcAnctwrhit2 = -9999;
  emcAncpid0 = -9999;
  emcAncpid1 = -9999;
  emcAncpid2 = -9999;

  crkacc = -9999;
  crknpmt0 = -9999;
  crknpmt1 = -9999;
  crknpmt3 = -9999;
  crknpe0 = -9999;
  crknpe1 = -9999;
  crknpe3 = -9999;
  crkchi2  = -9999;
  crkdisp = -9999;
  crkpath = -9999;



  for(int i=0;i<40;i++){
    hitid[i] = -1;//hitx[i] = hity[i] = hitz[i] = -9999;
  }

  
  _idG0 = -1; _pid0 = -1;_nx10 = -1;_idpare0 = -1;_gen0 = -1;
  _p0 = -100; _pthe0 = -100; _pphi0 = -100;
  _idG1 = -1; _idG2 = -1; 
  _p1 = -100; _p2 = -100; _pthe1= -100;_pthe2= -100;_pphi1= -100;_pphi2 = -100;
  _Q1 = -1;_Q2 =-1;
  _pid1= -1;_pid2=-1; 
  _phi1= -100;_phi2=-100; 
  _zed1= -100;_zed2=-100;
  _nx11= -1;_nx21=-1;_nx12=-1 ;_nx22=-1;
  _nx1G1=-1;_nx2G1=-1;_nx1G2= -1;_nx2G2 = -1;
  _alp1G1= -100;_alp2G1= -100;_alp1G2= -100;_alp2G2 = -100;
  _xPc1Reco1= -9999;_yPc1Reco1= -9999;_zPc1Reco1= -9999;_xPc1Reco2= -9999;_yPc1Reco2= -9999;_zPc1Reco2 = -9999;
  _xPc2Reco1= -9999;_yPc2Reco1= -9999;_zPc2Reco1= -9999;_xPc2Reco2= -9999;_yPc2Reco2= -9999;_zPc2Reco2 = -9999;
  _xPc3Reco1= -9999;_yPc3Reco1= -9999;_zPc3Reco1= -9999;_xPc3Reco2= -9999;_yPc3Reco2= -9999;_zPc3Reco2 = -9999;
  _xPc1Mc1= -9999;_yPc1Mc1= -9999;_zPc1Mc1= -9999;_xPc1Mc2= -9999;_yPc1Mc2= -9999;_zPc1Mc2 = -9999;
  _xPc2Mc1= -9999;_yPc2Mc1= -9999;_zPc2Mc1= -9999;_xPc2Mc2= -9999;_yPc2Mc2= -9999;_zPc2Mc2 = -9999;
  _xPc3Mc1= -9999;_yPc3Mc1= -9999;_zPc3Mc1= -9999;_xPc3Mc2= -9999;_yPc3Mc2= -9999;_zPc3Mc2 = -9999;



  ntrkG   = -9999;  ntrkR   = -9999;
  sigpc1  = -9999;  sigpc1p = -9999;  sigpc1z = -9999;  delpc1p = -9999;  delpc1z = -9999;
  sigpc2  = -9999;  sigpc2p = -9999;  sigpc2z = -9999;  delpc2p = -9999;  delpc2z = -9999;
  sigpc3  = -9999;  sigpc3p = -9999;  sigpc3z = -9999;  delpc3p = -9999;  delpc3z = -9999;
  sigtof  = -9999;  sigtofp = -9999;  sigtofz = -9999;  deltofp = -9999;  deltofz = -9999;
  sigemc  = -9999;  sigemcp = -9999;  sigemcz = -9999;  delemcp = -9999;  delemcz = -9999;
  alpha1  = -100;  alpha2  = -100; alpha1G = -100; alpha2G= -100;
  nx1 = 0; nx2 = 0;nx1G = 0; nx2G = 0;
  mdist1 = -100;  mdist2 = -100; mdist1G = -100; mdist2G = -100;
  chi21 = -100;  chi22 = -100; chi21G = -100; chi22G = -100;
}

DchMcRecoTrack::~DchMcRecoTrack()
{}

void DchMcRecoTrack::fill(float array[55]) 
{
  eventID = (int)array[0];
  eventxvtx = array[1];
  eventyvtx= array[2];
  eventzvtx= array[3];
  perfID= (int)array[4];
  perfQual= (int)array[5];
  momentumG= array[6];
  theta= array[7];
  phi= array[8];
  theta0G= array[9];
  
  phi0G= array[10];
  alphaG= array[11];
  betaG= array[12];
  zedG= array[13];
  generation= (int)array[14];
  particleID= (int)array[15];
  parentID= (int)array[16];
  primaryID= (int)array[17];
  rvtx= array[18];
  zvtx= array[19];
  phivtx= array[20];
  
  recoID= (int)array[21];
  recoQual= (int)array[22];
  momentumR= array[23];
  theta0R= array[24];
  phi0R= array[25];
  xhits= (int)array[26];
  uvhits= (int)array[27];
  mulcontrib= (int)array[28];
  xmulcontrib= (int)array[29];
  uvmulcontrib= (int)array[30];
  
  mainID= (int)array[31];
  xmainID= (int)array[32];
  uvmainID= (int)array[33];
  ambig= (int)array[34];
  xambig= (int)array[35];
  uvambig= (int)array[36];
  purity= array[37];
  xpurity= array[38];
  uvpurity= array[39];
  dalpha= array[40];
  
  dbeta= array[41];
  dphi= array[42];
  dzed= array[43];
  ddist= array[44];
  sumfound= (int)array[45];
  solution= (int)array[46];
  perfDvertex= array[47];
  recoDvertex= array[48];
  chi2= array[49];
  numHitsFit= (int)array[50];
  dalphaMin= array[51];
  dphiMin= array[52];
  idGeantTrack = (int)array[53];
  avDist= array[54];
      
}













