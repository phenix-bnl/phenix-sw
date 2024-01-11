// -----------------------------------------
// Created by:  Jiangyong Jia
//------------------------------------------
#include "PHEmbedMcRecoTrack.hh"

PHEmbedMcRecoTrack::PHEmbedMcRecoTrack(){
  resetALL();
}

PHEmbedMcRecoTrack::~PHEmbedMcRecoTrack(){
}

void PHEmbedMcRecoTrack::resetALL(){
  resetParticle();
  resetDC();
  resetPC();
  resetTOF();
  resetTEC();
  resetCRK();
  resetEMC();
  resetACC();
  resetModel();
  resetDCEva();
  resetMatch();
  resetTOFW(); // Added by Ron for TOFW
}

void PHEmbedMcRecoTrack::resetParticle(){
  type    = -1;
  evtID   = -1;
  idGeant = -1;
  evtxG   = -9999;
  evtyG   = -9999;
  evtzG   = -9999;
  pptG    = -9999;
  ppzG    = -9999;
  pthe0G  = -9999;
  pphi0G   = -9999;
  prapiG  = -9999;

  genG    = -9999;
  partidG = -1;
  pareidG = -1;
  primidG = -1;
  xvtxG   = -9999;
  yvtxG   = -9999;
  zvtxG   = -9999;

  bbcqn    = -9999;
  zdcen    = -9999;
  bbcqs    = -9999;
  zdces    = -9999;
  bbcvtx     = -9999;
  bbct0      = -9999;
  isminbias  = -1;
  bbccent    = -1;
  centclock  = -1;
}

void PHEmbedMcRecoTrack::resetDC(){

  ntrkG      = 0;
  dctrkidG   = -1;
  dctrkQualG = 0;
  the0G      = -9999;
  phi0G      = -9999;
  alphaG     = -9999;
  thetaG     = -9999;
  phiG       = -9999;
  betaG      = -9999;
  zedG       = -9999;
  momG       = -9999;
  ptG       = -9999;
  x1mG       = -9999;
  x2mG       = -9999;
  x1hG       = -1;
  x2hG       = -1;
  uv1hG      = -1;
  uv2hG      = -1;

  ntrkS      = -1;
  dctrkidS   = -1;
  dctrkQualS = 0;
  the0S      = -9999;
  phi0S      = -9999;
  alphaS     = -9999;
  thetaS     = -9999;
  phiS       = -9999;
  betaS      = -9999;
  zedS       = -9999;
  dcchi2S    = -9999;
  momS       = -9999;
  ptS       = -9999;
  x1mS       = -9999;
  x2mS       = -9999;
  x1hS       = -1;
  x2hS       = -1;
  uv1hS      = -1;
  uv2hS      = -1;

  x1hE       = -1;
  x2hE       = -1;
  uv1hE      = -1;
  uv2hE      = -1;

  ntrkb    = -1;
  ntrka    = -1;
  dctrkidR   = -1;
  dctrkQualR = 0;
  the0R      = -9999;
  phi0R      = -9999;
  alphaR     = -9999;
  thetaR     = -9999;
  phiR       = -9999;
  betaR      = -9999;
  zedR       = -9999;
  momR       = -9999;
  ptR       = -9999;
  dcchi2R    = -9999;
  x1mR       = -9999;
  x2mR       = -9999;
  x1hR       = -1;
  x2hR       = -1;
  uv1hR      = -1;
  uv2hR      = -1;

  x1hSG      = -1;
  x2hSG      = -1;
  x1hRG      = -1;
  x2hRG      = -1;
  x1hRS      = -1;
  x2hRS      = -1;
}
void PHEmbedMcRecoTrack::resetPC(){
  pc1idMc     = -1;
  pc2idMc     = -1;
  pc3idMc     = -1;
  pc1xMc      = -9999;
  pc1yMc      = -9999;
  pc1zMc      = -9999;
  pc2xMc      = -9999;
  pc2yMc      = -9999;
  pc2zMc      = -9999;
  pc3xMc      = -9999;
  pc3yMc      = -9999;
  pc3zMc      = -9999;
  pc1idS     = -1;
  pc2idS     = -1;
  pc3idS     = -1;
  pc1xS      = -9999;
  pc1yS      = -9999;
  pc1zS      = -9999;
  pc2xS      = -9999;
  pc2yS      = -9999;
  pc2zS      = -9999;
  pc3xS      = -9999;
  pc3yS      = -9999;
  pc3zS      = -9999;
  ppc1xS     = -9999;
  ppc1yS     = -9999;
  ppc1zS     = -9999;
  ppc2xS     = -9999;
  ppc2yS     = -9999;
  ppc2zS     = -9999;
  ppc3xS     = -9999;
  ppc3yS     = -9999;
  ppc3zS     = -9999;
  pc1idE     = -1;
  pc2idE     = -1;
  pc3idE     = -1;
  pc1idR     = -1;
  pc2idR     = -1;
  pc3idR     = -1;
  pc1xR      = -9999;
  pc1yR      = -9999;
  pc1zR      = -9999;
  pc2xR      = -9999;
  pc2yR      = -9999;
  pc2zR      = -9999;
  pc3xR      = -9999;
  pc3yR      = -9999;
  pc3zR      = -9999;
  ppc1xR     = -9999;
  ppc1yR     = -9999;
  ppc1zR     = -9999;
  ppc2xR     = -9999;
  ppc2yR     = -9999;
  ppc2zR     = -9999;
  ppc3xR     = -9999;
  ppc3yR     = -9999;
  ppc3zR     = -9999;
}	  

void PHEmbedMcRecoTrack::resetTOF(){
  tofidMc     = -1;
  tofxMc      = -9999;
  tofyMc      = -9999;
  tofzMc      = -9999;
  toftMc      = -9999;
  tofeMc      = -9999;
  tofpidMc    = -9999;
  pltofMc     = -9999;
  tofidS     = -1;
  tofxS      = -9999;
  tofyS      = -9999;
  tofzS      = -9999;
  toftS      = -9999;
  tofeS      = -9999;
  tofpidS    = -1;
  ptofxS     = -9999;
  ptofyS     = -9999;
  ptofzS     = -9999;
  pltofS     = -9999;
  tofidE     = -1;
  tofidR     = -1;
  tofxR      = -9999;
  tofyR      = -9999;
  tofzR      = -9999;
  toftR      = -9999;
  tofeR      = -9999;
  tofpidR    = -1;
  ptofxR     = -9999;
  ptofyR     = -9999;
  ptofzR     = -9999;
  pltofR     = -9999;  
}

void PHEmbedMcRecoTrack::resetTOFW() // Added by Ron for TOFW embedding
{

  tofwidMc     = -1;
  tofwxMc      = -9999;
  tofwyMc      = -9999;
  tofwzMc      = -9999;
  tofwtMc      = -9999;
  tofweMc      = -9999;
  tofwtdcupMc  = -9999;
  tofwadcupMc  = -9999;
  tofwtdcdnMc  = -9999;
  tofwadcdnMc  = -9999;
  tofwpidMc    = -9999;
  pltofwMc     = -9999;

  tofwembed   = -9999;
  tofwidS     = -1;
  tofwstripS  = -9999;
  tofwxS      = -9999;
  tofwyS      = -9999;
  tofwzS      = -9999;
  tofwtS      = -9999; 
  tofweS      = -9999; 
  tofwtdcupS  = -9999;
  tofwadcupS  = -9999;
  tofwtdcdnS  = -9999;
  tofwadcdnS  = -9999;
  tofwpidS    = -1;
  ptofwxS     = -9999;
  ptofwyS     = -9999;
  ptofwzS     = -9999;
  pltofwS     = -9999;

  tofwidE     = -1;    
  tofwidR     = -1;
  tofwstripR  = -9999;
  tofwxR      = -9999;
  tofwyR      = -9999;
  tofwzR      = -9999;
  tofwtR      = -9999; //tdc?
  tofweR      = -9999; //adc?
  tofwtdcupR  = -9999;
  tofwadcupR  = -9999;
  tofwtdcdnR  = -9999;
  tofwadcdnR  = -9999;
  tofwpidR    = -1;
  ptofwxR     = -9999;
  ptofwyR     = -9999;
  ptofwzR     = -9999;
  pltofwR     = -9999;  

}



void PHEmbedMcRecoTrack::resetTEC(){
  tecidMc     = -1;
  tecxMc      = -9999;
  tecyMc      = -9999;
  teczMc      = -9999;
  tecidS     = -1;
  tecxinS      = -9999;
  tecyinS      = -9999;
  teczinS      = -9999;
  tecxoutS      = -9999;
  tecyoutS      = -9999;
  teczoutS      = -9999;
  ptecxS     = -9999;
  ptecyS     = -9999;
  pteczS     = -9999;
  tecidE     = -1;
  tecidR     = -1;
  tecxinR      = -9999;
  tecyinR      = -9999;
  teczinR      = -9999;
  tecxoutR      = -9999;
  tecyoutR      = -9999;
  teczoutR      = -9999;
  ptecxR     = -9999;
  ptecyR     = -9999;
  pteczR     = -9999;
  tecdin = -9999.;
  tecdout = -9999.;
}
void PHEmbedMcRecoTrack::resetCRK(){
  plcrkMc     = -9999;
  plcrkS     = -9999;
  plcrkR     = -9999;
  crkaccS    = -1;
  crknpmt0S  = -1;
  crknpmt1S  = -1;
  crknpmt3S  = -1;
  crknpe0S   = -1;
  crknpe1S   = -1;
  crknpe3S   = -1;
  crkchi2S   = -1;
  crkdispS   = -1;
  crkpathS   = -1;
  crkaccR    = -1;
  crknpmt0R  = -1;
  crknpmt1R  = -1;
  crknpmt3R  = -1;
  crknpe0R   = -1;
  crknpe1R   = -1;
  crknpe3R   = -1;
  crkchi2R   = -1;
  crkdispR   = -1;
  crkpathR   = -1;
}	 
void PHEmbedMcRecoTrack::resetEMC(){
  emcidMc       = -1;
  emcxMc        = -9999;
  emcyMc        = -9999;
  emczMc        = -9999;
  plemcMc       = -9999;
  emctrkno0Mc   = -9999;
  emctrkno1Mc   = -9999;
  emctrkno2Mc   = -9999;
  emctwrhit0Mc  = -9999;
  emctwrhit1Mc  = -9999;
  emctwrhit2Mc  = -9999;
  emcpid0Mc     = -9999;
  emcpid1Mc     = -9999;
  emcpid2Mc     = -9999;
  emcedep0Mc    = -9999;
  emcedep1Mc    = -9999;
  emcedep2Mc    = -9999;
  emcptot0Mc    = -9999;
  emcptot1Mc    = -9999;
  emcptot2Mc    = -9999;
  emcidS        = -1;
  emcxS         = -9999;
  emcyS         = -9999;
  emczS         = -9999;
  emcswkeyS     = -9999;
  emcmeaseS     = -9999;
  emcecoreS     = -9999;
  emcecentS     = -9999;
  emcecorrS     = -9999;
  emctofS       = -9999;
  emctofcorrS   = -9999;
  emctofminS    = -9999;
  emcprobphotS  = -9999;
  emctwrhitS    = -9999;
  emcchi2S      = -9999;
  emcpartesum0S = -9999;
  emcpartesum1S = -9999;
  emcpartesum2S = -9999;
  emcpartesum3S = -9999;
  pemcxS        = -9999;
  pemcyS        = -9999;
  pemczS        = -9999;
  plemcS        = -9999;
  emcidE        = -1;
  emcidR        = -1;
  emcxR         = -9999;
  emcyR         = -9999;
  emczR         = -9999;
  emcswkeyR     = -9999;
  emcmeaseR     = -9999;
  emcecoreR     = -9999;
  emcecentR     = -9999;
  emcecorrR     = -9999;
  emctofR       = -9999;
  emctofcorrR   = -9999;
  emctofminR    = -9999;
  emcprobphotR  = -9999;
  emctwrhitR    = -9999;
  emcchi2R      = -9999;
  emcpartesum0R = -9999;
  emcpartesum1R = -9999;
  emcpartesum2R = -9999;
  emcpartesum3R = -9999;
  pemcxR        = -9999;
  pemcyR        = -9999;
  pemczR        = -9999;
  plemcR        = -9999;
}
void PHEmbedMcRecoTrack::resetACC(){
  aerboxidS        = -1;
  aerboxidR        = -1;
  aerph1R = -9999;
  aerph2R = -9999;
  aerph1S = -9999;
  aerph2S = -9999;

}
void PHEmbedMcRecoTrack::resetModel(){
  nx1x2fitS  = 0;
  mchi2S     = -9999;
  errS       = -9999;
  alphafS    = -9999;

  nx1x2fitR  = 0;
  mchi2R     = -9999;
  errR       = -9999;
  alphafR    = -9999;
}
void PHEmbedMcRecoTrack::resetDCEva(){
  sumfound   = 0;
  solution   = -1;
  mulmainS   = 0;
  xmulmainS  = 0;
  uvmulmainS = 0;
  mainIDS    = -1;
  xmainIDS   = -1;
  uvmainIDS  = -1;
  purityS    = 0;
  xpurityS   = 0;
  uvpurityS  = 0;
  sumfoundS  = 0;
  solutionS  = -1;

  mulmainR   = 0;
  xmulmainR  = 0;
  uvmulmainR = 0;
  mainIDR    = -1;
  xmainIDR   = -1;
  uvmainIDR  = -1;
  purityR    = 0;
  xpurityR   = 0;
  uvpurityR  = 0;
  sumfoundR  = 0;
  solutionR  = -1;
}
void PHEmbedMcRecoTrack::resetMatch(){
  pc2sS = -9999;pc2sdphiS = -9999;pc2sdzS = -9999;pc2dphiS = -9999;pc2dzS = -9999;
  pc3sS = -9999;pc3sdphiS = -9999;pc3sdzS = -9999;pc3dphiS = -9999;pc3dzS = -9999;
  tofsS = -9999;tofsdphiS = -9999;tofsdzS = -9999;tofdphiS = -9999;tofdzS = -9999;
  tofwsS = -9999;tofwsdphiS = -9999;tofwsdzS = -9999;tofwdphiS = -9999;tofwdzS = -9999; // Added by Ron
  emcsS = -9999;emcsdphiS = -9999;emcsdzS = -9999;emcdphiS = -9999;emcdzS = -9999;

  pc2sR = -9999;pc2sdphiR = -9999;pc2sdzR = -9999;pc2dphiR = -9999;pc2dzR = -9999;
  pc3sR = -9999;pc3sdphiR = -9999;pc3sdzR = -9999;pc3dphiR = -9999;pc3dzR = -9999;
  tofsR = -9999;tofsdphiR = -9999;tofsdzR = -9999;tofdphiR = -9999;tofdzR = -9999;
  tofwsR = -9999;tofwsdphiR = -9999;tofwsdzR = -9999;tofwdphiR = -9999;tofwdzR = -9999; // Added by Ron
  emcsR = -9999;emcsdphiR = -9999;emcsdzR = -9999;emcdphiR = -9999;emcdzR = -9999;
}



void PHEmbedMcRecoTrack::fillEvaluation(float*array){
  int m = 0;
  array[m++] = type;
  array[m++] = evtID;
  array[m++] = idGeant;
  array[m++] = evtxG;
  array[m++] = evtyG;
  array[m++] = evtzG;
  array[m++] = pptG;
  array[m++] = ppzG;
  array[m++] = pthe0G;
  array[m++] = pphi0G;
  array[m++] = prapiG;
  
  array[m++] = genG;
  array[m++] = partidG;
  array[m++] = pareidG;
  array[m++] = primidG;
  array[m++] = xvtxG;
  array[m++] = yvtxG;
  array[m++] = zvtxG;
  
  array[m++] = bbcqn;
  array[m++] = zdcen;
  array[m++] = bbcqs;
  array[m++] = zdces;
  array[m++] = bbcvtx;
  array[m++] = bbct0;
  array[m++] = isminbias;
  array[m++] = bbccent;
  array[m++] = centclock;

  array[m++] = ntrkG;
  array[m++] = dctrkidG;
  array[m++] = dctrkQualG;
  array[m++] = the0G;
  array[m++] = phi0G;
  array[m++] = alphaG;
  array[m++] = thetaG;
  array[m++] = phiG;
  array[m++] = betaG;
  array[m++] = zedG;
  array[m++] = momG;
  array[m++] = ptG;
  array[m++] = x1mG;
  array[m++] = x2mG;
  array[m++] = x1hG;
  array[m++] = x2hG;
  array[m++] = uv1hG;
  array[m++] = uv2hG;
  
  array[m++] = pc1idMc;
  array[m++] = pc2idMc;
  array[m++] = pc3idMc;
  array[m++] = tofidMc;
  array[m++] = tofwidMc; // TOFW
  array[m++] = tecidMc;
  array[m++] = emcidMc;

  array[m++] = pc1xMc;
  array[m++] = pc1yMc;
  array[m++] = pc1zMc;
  array[m++] = pc2xMc;
  array[m++] = pc2yMc;
  array[m++] = pc2zMc;
  array[m++] = pc3xMc;
  array[m++] = pc3yMc;
  array[m++] = pc3zMc;

  //TOFE
  array[m++] = tofxMc;
  array[m++] = tofyMc;
  array[m++] = tofzMc;
  array[m++] = toftMc;
  array[m++] = tofeMc;
  array[m++] = tofpidMc;

  //TOFW
  array[m++] = tofwxMc;
  array[m++] = tofwyMc;
  array[m++] = tofwzMc;
  array[m++] = tofwtMc;
  array[m++] = tofweMc;
  array[m++] = tofwtdcupMc;
  array[m++] = tofwadcupMc;
  array[m++] = tofwtdcdnMc;
  array[m++] = tofwadcdnMc;
  array[m++] = tofwpidMc;

  array[m++] = tecxMc;
  array[m++] = tecyMc;
  array[m++] = teczMc;
  array[m++] = emcxMc;
  array[m++] = emcyMc;
  array[m++] = emczMc;
  array[m++] = pltofMc;
  array[m++] = pltofwMc; // TOFW
  array[m++] = plcrkMc;
  array[m++] = plemcMc;

  array[m++] = emctrkno0Mc;
  array[m++] = emctrkno1Mc;
  array[m++] = emctrkno2Mc;
  array[m++] = emctwrhit0Mc;
  array[m++] = emctwrhit1Mc;
  array[m++] = emctwrhit2Mc;
  array[m++] = emcpid0Mc;
  array[m++] = emcpid1Mc;
  array[m++] = emcpid2Mc;
  array[m++] = emcedep0Mc;
  array[m++] = emcedep1Mc;
  array[m++] = emcedep2Mc;
  array[m++] = emcptot0Mc;
  array[m++] = emcptot1Mc;
  array[m++] = emcptot2Mc;

  array[m++] = ntrkS;
  array[m++] = dctrkidS;
  array[m++] = dctrkQualS;
  array[m++] = the0S;
  array[m++] = phi0S;
  array[m++] = alphaS;
  array[m++] = thetaS;
  array[m++] = phiS;
  array[m++] = betaS;
  array[m++] = zedS;
  array[m++] = dcchi2S;
  array[m++] = momS;
  array[m++] = ptS;
  array[m++] = x1mS;
  array[m++] = x2mS;
  array[m++] = x1hS;
  array[m++] = x2hS;
  array[m++] = uv1hS;
  array[m++] = uv2hS;
  
  array[m++] = pc1idS;
  array[m++] = pc2idS;
  array[m++] = pc3idS;
  array[m++] = tofidS;
  array[m++] = tecidS;
  array[m++] = emcidS;
  array[m++] = pc1xS;
  array[m++] = pc1yS;
  array[m++] = pc1zS;
  array[m++] = pc2xS;
  array[m++] = pc2yS;
  array[m++] = pc2zS;
  array[m++] = pc3xS;
  array[m++] = pc3yS;
  array[m++] = pc3zS;

  //TOFE
  array[m++] = tofxS;
  array[m++] = tofyS;
  array[m++] = tofzS;
  array[m++] = toftS;
  array[m++] = tofeS;
  array[m++] = tofpidS;

  //TOFW
  array[m++] = tofwxS;
  array[m++] = tofwyS;
  array[m++] = tofwzS;
  array[m++] = tofwtS;
  array[m++] = tofweS;
  array[m++] = tofwtdcupS;
  array[m++] = tofwadcupS;
  array[m++] = tofwtdcdnS;
  array[m++] = tofwadcdnS;
  array[m++] = tofwpidS;

  array[m++] = tecxinS;
  array[m++] = tecyinS;
  array[m++] = teczinS;
  array[m++] = tecxoutS;
  array[m++] = tecyoutS;
  array[m++] = teczoutS;
  array[m++] = emcxS;
  array[m++] = emcyS;
  array[m++] = emczS;

  array[m++] = ppc1xS;
  array[m++] = ppc1yS;
  array[m++] = ppc1zS;
  array[m++] = ppc2xS;
  array[m++] = ppc2yS;
  array[m++] = ppc2zS;
  array[m++] = ppc3xS;
  array[m++] = ppc3yS;
  array[m++] = ppc3zS;

  //TOFE
  array[m++] = ptofxS;
  array[m++] = ptofyS;
  array[m++] = ptofzS;

  //TOFW
  array[m++] = ptofwxS;
  array[m++] = ptofwyS;
  array[m++] = ptofwzS;

  array[m++] = ptecxS;
  array[m++] = ptecyS;
  array[m++] = pteczS;
  array[m++] = pemcxS;
  array[m++] = pemcyS;
  array[m++] = pemczS;
  array[m++] = pltofS;
  array[m++] = plcrkS;
  array[m++] = plemcS;

  array[m++] = aerboxidS;
  array[m++] = aerph1S;
  array[m++] = aerph2S;

  array[m++] = nx1x2fitS;
  array[m++] = mchi2S;
  array[m++] = errS;
  array[m++] = alphafS;
  array[m++] = crkaccS;
  array[m++] = crknpmt0S;
  array[m++] = crknpmt1S;
  array[m++] = crknpmt3S;
  array[m++] = crknpe0S;
  array[m++] = crknpe1S;
  array[m++] = crknpe3S;
  array[m++] = crkchi2S;
  array[m++] = crkdispS;
  array[m++] = crkpathS;
  array[m++] = emcswkeyS;
  array[m++] = emcmeaseS;
  array[m++] = emcecoreS;
  array[m++] = emcecentS;
  array[m++] = emcecorrS;
  array[m++] = emctofS;
  array[m++] = emctofcorrS;
  array[m++] = emctofminS;
  array[m++] = emcprobphotS;
  array[m++] = emctwrhitS;
  array[m++] = emcchi2S;
  array[m++] = emcpartesum0S;
  array[m++] = emcpartesum1S;
  array[m++] = emcpartesum2S;
  array[m++] = emcpartesum3S;

  array[m++] = x1hE;
  array[m++] = x2hE;
  array[m++] = uv1hE;
  array[m++] = uv2hE;
  array[m++] = pc1idE;
  array[m++] = pc2idE;
  array[m++] = pc3idE;
  array[m++] = tofidE;
  array[m++] = tofwidE; // TOFW
  array[m++] = tecidE;
  array[m++] = emcidE;

  array[m++] = ntrkb;
  array[m++] = ntrka;
  array[m++] = dctrkidR;
  array[m++] = dctrkQualR;
  array[m++] = the0R;
  array[m++] = phi0R;
  array[m++] = alphaR;
  array[m++] = thetaR;
  array[m++] = phiR;
  array[m++] = betaR;
  array[m++] = zedR;
  array[m++] = momR;
  array[m++] = ptR;
  array[m++] = dcchi2R;
  array[m++] = x1mR;
  array[m++] = x2mR;
  array[m++] = x1hR;
  array[m++] = x2hR;
  array[m++] = uv1hR;
  array[m++] = uv2hR;
  
  array[m++] = pc1idR;
  array[m++] = pc2idR;
  array[m++] = pc3idR;
  array[m++] = tofidR;
  array[m++] = tofwidR; // TOFW
  array[m++] = tecidR;
  array[m++] = emcidR;
  array[m++] = pc1xR;
  array[m++] = pc1yR;
  array[m++] = pc1zR;
  array[m++] = pc2xR;
  array[m++] = pc2yR;
  array[m++] = pc2zR;
  array[m++] = pc3xR;
  array[m++] = pc3yR;
  array[m++] = pc3zR;

  //TOFE
  array[m++] = tofxR;
  array[m++] = tofyR;
  array[m++] = tofzR;
  array[m++] = toftR;
  array[m++] = tofeR;
  array[m++] = tofpidR;

  //TOFW
  array[m++] = tofwxR;
  array[m++] = tofwyR;
  array[m++] = tofwzR;
  array[m++] = tofwtR;
  array[m++] = tofweR;
  array[m++] = tofwtdcupR;
  array[m++] = tofwadcupR;
  array[m++] = tofwtdcdnR;
  array[m++] = tofwadcdnR;
  array[m++] = tofwpidR;

  array[m++] = tecxinR;
  array[m++] = tecyinR;
  array[m++] = teczinR;
  array[m++] = tecxoutR;
  array[m++] = tecyoutR;
  array[m++] = teczoutR;
  array[m++] = emcxR;
  array[m++] = emcyR;
  array[m++] = emczR;


  array[m++] = ppc1xR;
  array[m++] = ppc1yR;
  array[m++] = ppc1zR;
  array[m++] = ppc2xR;
  array[m++] = ppc2yR;
  array[m++] = ppc2zR;
  array[m++] = ppc3xR;
  array[m++] = ppc3yR;
  array[m++] = ppc3zR;

  //TOFE
  array[m++] = ptofxR;
  array[m++] = ptofyR;
  array[m++] = ptofzR;

  //TOFW
  array[m++] = ptofwxR;
  array[m++] = ptofwyR;
  array[m++] = ptofwzR;

  array[m++] = ptecxR;
  array[m++] = ptecyR;
  array[m++] = pteczR;
  array[m++] = pemcxR;
  array[m++] = pemcyR;
  array[m++] = pemczR;
  array[m++] = tecdin;
  array[m++] = tecdout;
  array[m++] = pltofR;
  array[m++] = pltofwR; //TOFW
  array[m++] = plcrkR;
  array[m++] = plemcR;

  array[m++] = aerboxidR;
  array[m++] = aerph1R;
  array[m++] = aerph2R;

  array[m++] = nx1x2fitR;
  array[m++] = mchi2R;
  array[m++] = errR;
  array[m++] = alphafR;
  array[m++] = crkaccR;
  array[m++] = crknpmt0R;
  array[m++] = crknpmt1R;
  array[m++] = crknpmt3R;
  array[m++] = crknpe0R;
  array[m++] = crknpe1R;
  array[m++] = crknpe3R;
  array[m++] = crkchi2R;
  array[m++] = crkdispR;
  array[m++] = crkpathR;
  array[m++] = emcswkeyR;
  array[m++] = emcmeaseR;
  array[m++] = emcecoreR;
  array[m++] = emcecentR;
  array[m++] = emcecorrR;
  array[m++] = emctofR;
  array[m++] = emctofcorrR;
  array[m++] = emctofminR;
  array[m++] = emcprobphotR;
  array[m++] = emctwrhitR;
  array[m++] = emcchi2R;
  array[m++] = emcpartesum0R;
  array[m++] = emcpartesum1R;
  array[m++] = emcpartesum2R;
  array[m++] = emcpartesum3R;
  
  array[m++] = sumfound;
  array[m++] = solution;
  array[m++] = mulmainS;
  array[m++] = xmulmainS;
  array[m++] = uvmulmainS;
  array[m++] = mainIDS;
  array[m++] = xmainIDS;
  array[m++] = uvmainIDS;
  array[m++] = purityS;
  array[m++] = xpurityS;
  array[m++] = uvpurityS;
  array[m++] = sumfoundS;
  array[m++] = solutionS;
  array[m++] = mulmainR;
  array[m++] = xmulmainR;
  array[m++] = uvmulmainR;
  array[m++] = mainIDR;
  array[m++] = xmainIDR;
  array[m++] = uvmainIDR;
  array[m++] = purityR;
  array[m++] = xpurityR;
  array[m++] = uvpurityR;
  array[m++] = sumfoundR;
  array[m++] = solutionR;

  array[m++] = pc2sS;
  array[m++] = pc2sdphiS;
  array[m++] = pc2sdzS;
  array[m++] = pc2dphiS;
  array[m++] = pc2dzS;
  array[m++] = pc3sS;
  array[m++] = pc3sdphiS;
  array[m++] = pc3sdzS;
  array[m++] = pc3dphiS;
  array[m++] = pc3dzS;

  //TOFE
  array[m++] = tofsS;
  array[m++] = tofsdphiS;
  array[m++] = tofsdzS;
  array[m++] = tofdphiS;
  array[m++] = tofdzS;

  //TOFW
  array[m++] = tofwsS;
  array[m++] = tofwsdphiS;
  array[m++] = tofwsdzS;
  array[m++] = tofwdphiS;
  array[m++] = tofwdzS;

  array[m++] = emcsS;
  array[m++] = emcsdphiS;
  array[m++] = emcsdzS;
  array[m++] = emcdphiS;
  array[m++] = emcdzS;
  array[m++] = pc2sR;
  array[m++] = pc2sdphiR;
  array[m++] = pc2sdzR;
  array[m++] = pc2dphiR;
  array[m++] = pc2dzR;
  array[m++] = pc3sR;
  array[m++] = pc3sdphiR;
  array[m++] = pc3sdzR;
  array[m++] = pc3dphiR;
  array[m++] = pc3dzR;

  //TOFE
  array[m++] = tofsR;
  array[m++] = tofsdphiR;
  array[m++] = tofsdzR;
  array[m++] = tofdphiR;
  array[m++] = tofdzR;

  //TOFW
  array[m++] = tofwsR;
  array[m++] = tofwsdphiR;
  array[m++] = tofwsdzR;
  array[m++] = tofwdphiR;
  array[m++] = tofwdzR;

  array[m++] = emcsR;
  array[m++] = emcsdphiR;
  array[m++] = emcsdzR;
  array[m++] = emcdphiR;
  array[m++] = emcdzR;

  array[m++] = x1hSG;
  array[m++] = x2hSG;
  array[m++] = uvhSG;
  array[m++] = x1hRG;
  array[m++] = x2hRG;
  array[m++] = uvhRG;
  array[m++] = x1hRS;
  array[m++] = x2hRS;
  array[m++] = uvhRS;
}
