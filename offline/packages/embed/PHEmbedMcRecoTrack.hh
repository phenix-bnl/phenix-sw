// -----------------------------------------
// Created by:  Jiangyong Jia
//------------------------------------------
#ifndef _PHEMBEDMCRECOTRACK_H
#define _PHEMBEDMCRECOTRACK_H

//do I need to add matching variables??
class PHEmbedMcRecoTrack {
  friend class PHEmbedHistogrammer;
private:
  //total 374 variables
  //Geant information
  // 11 variables
  short          type; //type of the record
  int            evtID;
  int            idGeant;
  float          evtxG;
  float          evtyG;
  float          evtzG;
  float          pptG;
  float          ppzG;
  float          pthe0G;
  float          pphi0G;
  float          prapiG;

  // 7 variables
  short          genG;
  int            partidG;//particleid
  int            pareidG;//parentid
  int            primidG;//primaryid
  float          xvtxG;
  float          yvtxG;
  float          zvtxG;

  //global info
  // 9 variables
  float          bbcqn;
  float          zdcen;
  float          bbcqs;
  float          zdces;
  float          bbcvtx;
  float          bbct0;
  short          isminbias;
  float          bbccent,centclock;

  // 18 variables
  short          ntrkG;//number of perfect tracks
  short          dctrkidG;
  short          dctrkQualG;
  float          the0G;
  float          phi0G;
  float          alphaG;
  float          thetaG;
  float          phiG;
  float          betaG;
  float          zedG;
  float          momG;
  float          ptG;
  float          x1mG;
  float          x2mG;
  short          x1hG;
  short          x2hG;
  short          uv1hG;
  short          uv2hG;  
  
  /*
   *Mc id is the responsed id( in test particle node) correaponding to the original geant hit,
   *for DC I use dctrackidG (id of perfect tracks)
   */
  // 6 variables
  short          pc1idMc;
  short          pc2idMc;
  short          pc3idMc;
  short          tofidMc;
  short          tecidMc;
  short          emcidMc;  
  // 24 variables
  float          pc1xMc;
  float          pc1yMc;
  float          pc1zMc;
  float          pc2xMc;
  float          pc2yMc;
  float          pc2zMc;
  float          pc3xMc;
  float          pc3yMc;
  float          pc3zMc;
  float          tofxMc;
  float          tofyMc;
  float          tofzMc;
  float          toftMc;
  float          tofeMc;
  int            tofpidMc;
  float          tecxMc;
  float          tecyMc;
  float          teczMc;
  float          emcxMc;
  float          emcyMc;
  float          emczMc;
  float          pltofMc;
  float          plcrkMc;
  float          plemcMc;

  // 15 variables
  short          emctrkno0Mc;
  short          emctrkno1Mc;
  short          emctrkno2Mc;
  short          emctwrhit0Mc;
  short          emctwrhit1Mc;
  short          emctwrhit2Mc;
  short          emcpid0Mc;
  short          emcpid1Mc;
  short          emcpid2Mc;
  float          emcedep0Mc;
  float          emcedep1Mc;
  float          emcedep2Mc;
  float          emcptot0Mc;
  float          emcptot1Mc;
  float          emcptot2Mc;

  //response information
  // 19 variables 
  short          ntrkS;   //when do multiple tracks embedding or one Geant track can have several tracks reconstructed one need to have this
  short          dctrkidS;
  short          dctrkQualS;
  float          the0S;
  float          phi0S;
  float          alphaS;
  float          thetaS;
  float          phiS;
  float          betaS;
  float          zedS;
  float          dcchi2S;
  float          momS;
  float          ptS;
  float          x1mS;
  float          x2mS;
  short          x1hS;
  short          x2hS;
  short          uv1hS;
  short          uv2hS;

  //30variables
  short          pc1idS;
  short          pc2idS;
  short          pc3idS;
  short          tofidS;
  short          tecidS;
  short          emcidS;  
  float          pc1xS;
  float          pc1yS;
  float          pc1zS;
  float          pc2xS;
  float          pc2yS;
  float          pc2zS;
  float          pc3xS;
  float          pc3yS;
  float          pc3zS;
  float          tofxS;
  float          tofyS;
  float          tofzS;
  float          toftS;
  float          tofeS;
  int            tofpidS;
  float          tecxinS;
  float          tecyinS;
  float          teczinS;
  float          tecxoutS;
  float          tecyoutS;
  float          teczoutS;
  float          emcxS;
  float          emcyS;
  float          emczS;
  //25 variables
  float          ppc1xS;
  float          ppc1yS;
  float          ppc1zS;
  float          ppc2xS;
  float          ppc2yS;
  float          ppc2zS;
  float          ppc3xS;
  float          ppc3yS;
  float          ppc3zS;
  float          ptofxS;
  float          ptofyS;
  float          ptofzS;
  float          ptecxS;
  float          ptecyS;
  float          pteczS;
  float          pemcxS;
  float          pemcyS;
  float          pemczS;
  float          pltofS;
  float          plcrkS;
  float          plemcS;

  short          aerboxidS;
  float          aerph1S;
  float          aerph2S;

  // 29 variables
  short          nx1x2fitS;
  float          mchi2S;
  short          errS;
  float          alphafS;
  short          crkaccS;
  short          crknpmt0S;
  short          crknpmt1S;
  short          crknpmt3S;
  float          crknpe0S;
  float          crknpe1S;
  float          crknpe3S;
  float          crkchi2S;
  float          crkdispS;
  float          crkpathS;
  short          emcswkeyS;
  float          emcmeaseS;
  float          emcecoreS;
  float          emcecentS;
  float          emcecorrS;
  float          emctofS;
  float          emctofcorrS;
  float          emctofminS;
  float          emcprobphotS;
  short          emctwrhitS;
  float          emcchi2S;
  float          emcpartesum0S;
  float          emcpartesum1S;
  float          emcpartesum2S;
  float          emcpartesum3S;

  //information to keep after embedding 
  // if correct association then  xidE == xidR
  //10 variables
  short          x1hE;
  short          x2hE;
  short          uv1hE;
  short          uv2hE;
  short          pc1idE;
  short          pc2idE;
  short          pc3idE;
  short          tofidE;
  short          tecidE;
  short          emcidE;  

  //reconstruction information
  //20 variables
  short          ntrkb;
  short          ntrka;
  short          dctrkidR;
  short          dctrkQualR;
  float          the0R;
  float          phi0R;
  float          alphaR;
  float          thetaR;
  float          phiR;
  float          betaR;
  float          zedR;
  float          momR;
  float          ptR;
  float          dcchi2R;
  float          x1mR;
  float          x2mR;
  short          x1hR;
  short          x2hR;
  short          uv1hR;
  short          uv2hR;
  //30 variables
  short          pc1idR;
  short          pc2idR;
  short          pc3idR;
  short          tofidR;
  short          tecidR;
  short          emcidR;
  float          pc1xR;
  float          pc1yR;
  float          pc1zR;
  float          pc2xR;
  float          pc2yR;
  float          pc2zR;
  float          pc3xR;
  float          pc3yR;
  float          pc3zR;
  float          tofxR;
  float          tofyR;
  float          tofzR;
  float          toftR;
  float          tofeR;
  int            tofpidR;
  float          tecxinR;
  float          tecyinR;
  float          teczinR;
  float          tecxoutR;
  float          tecyoutR;
  float          teczoutR;
  float          emcxR;
  float          emcyR;
  float          emczR;

  //27 variables
  float          ppc1xR;
  float          ppc1yR;
  float          ppc1zR;
  float          ppc2xR;
  float          ppc2yR;
  float          ppc2zR;
  float          ppc3xR;
  float          ppc3yR;
  float          ppc3zR;
  float          ptofxR;
  float          ptofyR;
  float          ptofzR;
  float          ptecxR;
  float          ptecyR;
  float          pteczR;
  float          pemcxR;
  float          pemcyR;
  float          pemczR;
  float 	 tecdin;
  float 	 tecdout;
  float          pltofR;
  float          plcrkR;
  float          plemcR;

  short          aerboxidR;
  float          aerph1R;
  float          aerph2R;

  //29 variables
  short          nx1x2fitR;
  float          mchi2R;
  short          errR;
  float          alphafR;
  short          crkaccR;
  short          crknpmt0R;
  short          crknpmt1R;
  short          crknpmt3R;
  float          crknpe0R;
  float          crknpe1R;
  float          crknpe3R;
  float          crkchi2R;
  float          crkdispR;
  float          crkpathR;
  short          emcswkeyR;
  float          emcmeaseR;
  float          emcecoreR;
  float          emcecentR;
  float          emcecorrR;
  float          emctofR;
  float          emctofcorrR;
  float          emctofminR;
  float          emcprobphotR;
  short          emctwrhitR;
  float          emcchi2R;
  float          emcpartesum0R;
  float          emcpartesum1R;
  float          emcpartesum2R;
  float          emcpartesum3R;

  // fields for main contributor analysis or other efficiency study
  //24 variables
  short          sumfound;
  short          solution;  
  short          mulmainS;
  short          xmulmainS;
  short          uvmulmainS;
  short          mainIDS;
  short          xmainIDS;
  short          uvmainIDS;
  float          purityS;
  float          xpurityS;
  float          uvpurityS;
  short          sumfoundS;
  short          solutionS;  
  short          mulmainR;
  short          xmulmainR;
  short          uvmulmainR;
  short          mainIDR;
  short          xmainIDR ;
  short          uvmainIDR;
  float          purityR;
  float          xpurityR;
  float          uvpurityR;
  short          sumfoundR;
  short          solutionR;  


  //matching variables
  //40 variables
  float          pc2sS,pc2sdphiS,pc2sdzS,pc2dphiS,pc2dzS;
  float          pc3sS,pc3sdphiS,pc3sdzS,pc3dphiS,pc3dzS;
  float          tofsS,tofsdphiS,tofsdzS,tofdphiS,tofdzS;
  float          tofwsS,tofwsdphiS,tofwsdzS,tofwdphiS,tofwdzS; // Added by Ron
  float          emcsS,emcsdphiS,emcsdzS,emcdphiS,emcdzS;

  float          pc2sR,pc2sdphiR,pc2sdzR,pc2dphiR,pc2dzR;
  float          pc3sR,pc3sdphiR,pc3sdzR,pc3dphiR,pc3dzR;
  float          tofsR,tofsdphiR,tofsdzR,tofdphiR,tofdzR;
  float          tofwsR,tofwsdphiR,tofwsdzR,tofwdphiR,tofwdzR; // Added by Ron
  float          emcsR,emcsdphiR,emcsdzR,emcdphiR,emcdzR;



  //NEW TOFW VARIABLES
  float  tofwidMc;
  float  tofwxMc;
  float  tofwyMc;
  float  tofwzMc;
  float  tofwtMc;
  float  tofweMc;
  float  tofwtdcupMc;
  float  tofwadcupMc;
  float  tofwtdcdnMc;
  float  tofwadcdnMc;
  float  tofwpidMc;
  float  pltofwMc;

  int    tofwembed;//0: no contanimation, 1: has but no effect, 2: T contaminated, 3: A contaminated 
  float  tofwidS;
  int    tofwstripS;
  float  tofwxS;
  float  tofwyS;
  float  tofwzS;
  float  tofwtS; 
  float  tofweS; 
  float  tofwtdcupS;
  float  tofwadcupS;
  float  tofwtdcdnS;
  float  tofwadcdnS;
  float  tofwpidS;
  float  ptofwxS;
  float  ptofwyS;
  float  ptofwzS;
  float  pltofwS;
  
  float  tofwidE;    //what the hell is this?

  float  tofwidR;
  int    tofwstripR;
  float  tofwxR;
  float  tofwyR;
  float  tofwzR;
  float  tofwtR;
  float  tofweR;
  float  tofwtdcupR;
  float  tofwadcupR;
  float  tofwtdcdnR;
  float  tofwadcdnR;
  float  tofwpidR;
  float  ptofwxR;
  float  ptofwyR;
  float  ptofwzR;
  float  pltofwR;



public:
  //relate number of hits
  //9 variables
  short          x1hSG,x2hSG,uvhSG,x1hRG,x2hRG,uvhRG,x1hRS,x2hRS,uvhRS;
  
  /*
   * REORGNIZE ALL VARIABLES IN SUBSYSTEM LIST
   */
public:  
  PHEmbedMcRecoTrack(); 
  ~PHEmbedMcRecoTrack();

  void resetALL();
  void resetParticle();
  void resetDC();
  void resetPC();
  void resetTOF();
  void resetTOFW(); // Added for TOFW
  void resetTEC();
  void resetCRK();
  void resetEMC();
  void resetACC();
  void resetModel();
  void resetDCEva();
  void resetMatch();
  void fillEvaluation(float*array);

  //type:evtID:idGeant:evtxG:evtyG:evtzG:ppt:ppz:prapi:pphi:genG:partidG:pareidG:primidG:rvtxG:zvtxG:projzG: (17)
  //bbcQsum:zdcEsum:bbcvtx:bbct0:ntrkG:dctrkidG:dctrkQualG:the0G:phi0G:alphaG:thetaG:phiG:betaG:zedG:momG:x1mG:x2mG:x1hG:x2hG:uv1hG:uv2hG:(22)
  //ntrkS:dctrkidS:dctrkQualS:the0S:phi0S:alphaS:thetaS:phiS:betaS:zedS:dcchi2S:momS:x1mS:x2mS:x1hS:x2hS:uv1hS:uv2hS:projzS:(19)
  //x1hE:x2hE:uv1hE:uv2hE:ntrkwBR:ntrkeBR:ntrkwAR:ntrkeAR:(8)
  //dctrkidR:dctrkQualR:armsidR:the0R:phi0R:alphaR:thetaR:phiR:betaR:zedR:momR:dcchi2R:x1mR:x2mR:x1hR:x2hR:uv1hR:uv2hR:projzR:(19)
  //pc1idMc:pc2idMc:pc3idMc:pc1xMc:pc1yMc:pc1zMc:pc2xMc:pc2yMc:pc2zMc:pc3xMc:pc3yMc:pc3zMc:(12)
  //pc1idS:pc2idS:pc3idS:pc1xS:pc1yS:pc1zS:pc2xS:pc2yS:pc2zS:pc3xS:pc3yS:pc3zS:ppc1xS:ppc1yS:ppc1zS:ppc2xS:ppc2yS:ppc2zS:ppc3xS:ppc3yS:ppc3zS:(21)
  //pc1idE:pc2idE:pc3idE:(3)
  //pc1idR:pc2idR:pc3idR:pc1xR:pc1yR:pc1zR:pc2xR:pc2yR:pc2zR:pc3xR:pc3yR:pc3zR:ppc1xR:ppc1yR:ppc1zR:ppc2xR:ppc2yR:ppc2zR:ppc3xR:ppc3yR:ppc3zR:(21)
  //tofidMc:tofxMc:tofyMc:tofzMc:toftMc:tofeMc:pltofMc:tofidS:tofxS:tofyS:tofzS:toftS:tofeS:tofpidS:ptofxS:ptofyS:ptofzS:pltofS:(18)
  //tofidE:tofidR:tofxR:tofyR:tofzR:toftR:tofeR:tofpidR:ptofxR:ptofyR:ptofzR:pltofR:(12)
  //tecidMc:tecxMc:tecyMc:teczMc:tecidS:tecxinS:tecyinS:tecxoutS:tecyoutS:teczS:ptecxS:ptecyS:pteczS:tecidE:tecidR:tecxinR:tecyinR:tecxoutR:texyoutR:teczR:ptecxR:ptecyR:pteczR:tecdin:tecdout(25)
  //plcrkMc:plcrkS:plcrkR:(3)
  //emcidMc:emcxMc:emcyMc:emczMc:plemcMc:emcidS:emcxS:emcyS:emczS:pemcxS:pemcyS:pemczS:plemcS:emcidE:emcidR:emcxR:emcyR:emczR:pemcxR:pemcyR:pemczR:plemcR:(22)
  //nx1x2fitS:mchi2S:errS:alphafS:nx1x2fitR:mchi2R:errR:alphafR:(8)
  //sumfound:solution:(2)
  //mulmainS:xmulmainS:uvmulmainS:mainIDS:xmainIDS:uvmainIDS:purityS:xpurityS:uvpurityS:(9)
  //mulmainR:xmulmainR:uvmulmainR:mainIDR:xmainIDR:uvmainIDR:purityR:xpurityR:uvpurityR:(9)

  
  //-----------------------total 262 variables about 1048 bytes when saved to ntuple-------------------------------

  // --------------------- GET methods
  int            get_type(){return type;}
  int            get_evtID(){return evtID;}
  int            get_idGeant(){return idGeant;}
  float          get_evtxG(){return evtxG;}
  float          get_evtyG(){return evtyG;}
  float          get_evtzG(){return evtzG;}
  float          get_pptG(){return pptG;}
  float          get_ppzG(){return ppzG;}
  float          get_pthe0G(){return pthe0G;}
  float          get_pphi0G(){return pphi0G;}
  float          get_prapiG(){return prapiG;}

  short          get_genG(){return genG;}
  int            get_partidG(){return partidG;}
  int            get_pareidG(){return pareidG;}
  int            get_primidG(){return primidG;}
  float          get_xvtxG(){return xvtxG;}
  float          get_yvtxG(){return yvtxG;}
  float          get_zvtxG(){return zvtxG;}

  short          get_ntrkG(){ return ntrkG;}
  short          get_dctrkidG(){return dctrkidG;}
  short          get_dctrkQualG(){return dctrkQualG;}
  float          get_the0G(){return the0G;}
  float          get_phi0G(){return phi0G;}
  float          get_alphaG(){return alphaG;}
  float          get_thetaG(){return thetaG;}
  float          get_phiG(){return phiG;}
  float          get_betaG(){return betaG;}
  float          get_zedG(){return zedG;} 
  float          get_momG(){return momG;}
  float          get_ptG() {return ptG;}
  float          get_x1mG(){return x1mG;}
  float          get_x2mG(){return x2mG;}
  short          get_x1hG(){return x1hG;}
  short          get_x2hG(){return x2hG;}
  short          get_uv1hG(){return uv1hG;}
  short          get_uv2hG(){return uv2hG;}  

  float          get_bbcqn(){return bbcqn;}
  float          get_zdcen(){return zdcen;}
  float          get_bbcqs(){return bbcqs;}
  float          get_zdces(){return zdces;}
  float          get_bbcvtx(){return bbcvtx;}
  float          get_bbct0(){return bbct0;}
  short          get_isminbias(){return isminbias;}
  float          get_bbccent(){return bbccent;}
  float          get_centclock(){return centclock;}

  short          get_pc1idMc(){return pc1idMc;}
  short          get_pc2idMc(){return pc2idMc;}
  short          get_pc3idMc(){return pc3idMc;}
  short          get_tofidMc(){return tofidMc;}
  short          get_tecidMc(){return tecidMc;}
  short          get_emcidMc(){return emcidMc;}

  float          get_pc1xMc(){return pc1xMc;}
  float          get_pc1yMc(){return pc1yMc;}
  float          get_pc1zMc(){return pc1zMc;}
  float          get_pc2xMc(){return pc2xMc;}
  float          get_pc2yMc(){return pc2yMc;}
  float          get_pc2zMc(){return pc2zMc;}
  float          get_pc3xMc(){return pc3xMc;}
  float          get_pc3yMc(){return pc3yMc;}
  float          get_pc3zMc(){return pc3zMc;}
  float          get_tofxMc(){return tofxMc;}
  float          get_tofyMc(){return tofyMc;}
  float          get_tofzMc(){return tofzMc;}
  float          get_toftMc(){return toftMc;}
  float          get_tofeMc(){return tofeMc;}
  int            get_tofpidMc(){return tofpidMc;}
  float          get_tecxMc(){return tecxMc;}
  float          get_tecyMc(){return tecyMc;}
  float          get_teczMc(){return teczMc;}
  float          get_emcxMc(){return emcxMc;}
  float          get_emcyMc(){return emcyMc;}
  float          get_emczMc(){return emczMc;}
  float          get_pltofMc(){return pltofMc;}
  float          get_plcrkMc(){return plcrkMc;}
  float          get_plemcMc(){return plemcMc;}

  short          get_emctrkno0Mc(){return emctrkno0Mc;}
  short          get_emctrkno1Mc(){return emctrkno1Mc;}
  short          get_emctrkno2Mc(){return emctrkno2Mc;}
  short          get_emctwrhit0Mc(){return emctwrhit0Mc;}
  short          get_emctwrhit1Mc(){return emctwrhit1Mc;}
  short          get_emctwrhit2Mc(){return emctwrhit2Mc;}
  short          get_emcpid0Mc(){return emcpid0Mc;}
  short          get_emcpid1Mc(){return emcpid1Mc;}
  short          get_emcpid2Mc(){return emcpid2Mc;}
  float          get_emcedep0Mc(){return emcedep0Mc;}
  float          get_emcedep1Mc(){return emcedep1Mc;}
  float          get_emcedep2Mc(){return emcedep2Mc;}
  float          get_emcptot0Mc(){return emcptot0Mc;}
  float          get_emcptot1Mc(){return emcptot1Mc;}
  float          get_emcptot2Mc(){return emcptot2Mc;}

  //response information
  short          get_ntrkS(){return ntrkS;}
  short          get_dctrkidS(){return dctrkidS;}
  short          get_dctrkQualS(){return dctrkQualS;}
  float          get_the0S(){return the0S;}
  float          get_phi0S(){return phi0S;}
  float          get_alphaS(){return alphaS;}
  float          get_thetaS(){return thetaS;}
  float          get_phiS(){return phiS;}
  float          get_betaS(){return betaS;}
  float          get_zedS(){return zedS;}
  float          get_dcchi2S(){return dcchi2S;}
  float          get_momS(){return momS;}
  float          get_ptS(){return ptS;}
  float          get_x1mS(){return x1mS;}
  float          get_x2mS(){return x2mS;}
  short          get_x1hS(){return x1hS;}
  short          get_x2hS(){return x2hS;}
  short          get_uv1hS(){return uv1hS;}
  short          get_uv2hS(){return uv2hS;}
  short          get_pc1idS(){return pc1idS;}
  short          get_pc2idS(){return pc2idS;}
  short          get_pc3idS(){return pc3idS;}
  short          get_tofidS(){return tofidS;}
  short          get_tecidS(){return tecidS;}
  short          get_emcidS(){return emcidS;}  
  float          get_pc1xS(){return pc1xS;}
  float          get_pc1yS(){return pc1yS;}
  float          get_pc1zS(){return pc1zS;}
  float          get_pc2xS(){return pc2xS;}
  float          get_pc2yS(){return pc2yS;}
  float          get_pc2zS(){return pc2zS;}
  float          get_pc3xS(){return pc3xS;}
  float          get_pc3yS(){return pc3yS;}
  float          get_pc3zS(){return pc3zS;}
  float          get_tofxS(){return tofxS;}
  float          get_tofyS(){return tofyS;}
  float          get_tofzS(){return tofzS;}
  float          get_toftS(){return toftS;}
  float          get_tofeS(){return tofeS;}
  int            get_tofpidS(){return tofpidS;}
  float          get_tecxinS(){return tecxinS;}
  float          get_tecyinS(){return tecyinS;}
  float          get_teczinS(){return teczinS;}
  float          get_tecxoutS(){return tecxoutS;}
  float          get_tecyoutS(){return tecyoutS;}
  float          get_teczoutS(){return teczoutS;}

  float          get_emcxS(){return emcxS;}
  float          get_emcyS(){return emcyS;}
  float          get_emczS(){return emczS;}
  short          get_emcswkeyS(){return emcswkeyS;}
  float          get_emcmeaseS(){return emcmeaseS;}
  float          get_emcecoreS(){return emcecoreS;}
  float          get_emcecentS(){return emcecentS;}
  float          get_emcecorrS(){return emcecorrS;}
  float          get_emctofS(){return emctofS;}
  float          get_emctofcorrS(){return emctofcorrS;}
  float          get_emctofminS(){return emctofminS;}
  float          get_emcprobphotS(){return emcprobphotS;}
  short          get_emctwrhitS(){return emctwrhitS;}
  float          get_emcchi2S(){return emcchi2S;}
  float          get_emcpartesum0S(){return emcpartesum0S;}
  float          get_emcpartesum1S(){return emcpartesum1S;}
  float          get_emcpartesum2S(){return emcpartesum2S;}
  float          get_emcpartesum3S(){return emcpartesum3S;}

  float          get_ppc1xS(){return ppc1xS;}
  float          get_ppc1yS(){return ppc1yS;}
  float          get_ppc1zS(){return ppc1zS;}
  float          get_ppc2xS(){return ppc2xS;}
  float          get_ppc2yS(){return ppc2yS;}
  float          get_ppc2zS(){return ppc2zS;}
  float          get_ppc3xS(){return ppc3xS;}
  float          get_ppc3yS(){return ppc3yS;}
  float          get_ppc3zS(){return ppc3zS;}
  float          get_ptofxS(){return ptofxS;}
  float          get_ptofyS(){return ptofyS;}
  float          get_ptofzS(){return ptofzS;}
  float          get_ptecxS(){return ptecxS;}
  float          get_ptecyS(){return ptecyS;}
  float          get_pteczS(){return pteczS;}
  float          get_pemcxS(){return pemcxS;}
  float          get_pemcyS(){return pemcyS;}
  float          get_pemczS(){return pemczS;}
  float          get_pltofS(){return pltofS;}
  float          get_plcrkS(){return plcrkS;}
  float          get_plemcS(){return plemcS;}
  
  short          get_aerboxidS(){return aerboxidS;}
  float          get_aerph1S(){return aerph1S;}
  float          get_aerph2S(){return aerph2S;}
  
  short          get_nx1x2fitS(){return nx1x2fitS;}
  float          get_mchi2S(){return mchi2S;}
  short          get_errS(){return errS;}
  float          get_alphafS(){return alphafS;}

  short          get_crkaccS(){return crkaccS;}
  short          get_crknpmt0S(){return crknpmt0S;}
  short          get_crknpmt1S(){return crknpmt1S;}
  short          get_crknpmt3S(){return crknpmt3S;}
  float          get_crknpe0S(){return crknpe0S;}
  float          get_crknpe1S(){return crknpe1S;}
  float          get_crknpe3S(){return crknpe3S;}
  float          get_crkchi2S(){return crkchi2S;}
  float          get_crkdispS(){return crkdispS;}
  float          get_crkpathS(){return crkpathS;}

  
  //information to keep after embedding 
  // if correct association then  xidE == xidR
  short          get_x1hE(){return x1hE;}
  short          get_x2hE(){return x2hE;}
  short          get_uv1hE(){return uv1hE;}
  short          get_uv2hE(){return uv2hE;}
  short          get_pc1idE(){return pc1idE;}
  short          get_pc2idE(){return pc2idE;}
  short          get_pc3idE(){return pc3idE;}
  short          get_tofidE(){return tofidE;}
  short          get_tecidE(){return tecidE;}
  short          get_emcidE(){return emcidE;}  

  //reconstruction information
  short          get_ntrkb(){return ntrkb;}
  short          get_ntrka(){return ntrka;}
  short          get_dctrkidR(){return dctrkidR;}
  short          get_dctrkQualR(){return dctrkQualR;}
  float          get_the0R(){return the0R;}
  float          get_phi0R(){return phi0R;}
  float          get_alphaR(){return alphaR;}
  float          get_thetaR(){return thetaR;}
  float          get_phiR(){return phiR;}
  float          get_betaR(){return betaR;}
  float          get_zedR(){return zedR;}
  float          get_momR(){return momR;}
  float          get_ptR(){return ptR;}
  float          get_dcchi2R(){return dcchi2R;}
  float          get_x1mR(){return x1mR;}
  float          get_x2mR(){return x2mR;}
  short          get_x1hR(){return x1hR;}
  short          get_x2hR(){return x2hR;}
  short          get_uv1hR(){return uv1hR;}
  short          get_uv2hR(){return uv2hR;}
  
  short          get_pc1idR(){return pc1idR;}
  short          get_pc2idR(){return pc2idR;}
  short          get_pc3idR(){return pc3idR;}
  short          get_tofidR(){return tofidR;}
  short          get_tecidR(){return tecidR;}
  short          get_emcidR(){return emcidR;}

  float          get_pc1xR(){return pc1xR;}
  float          get_pc1yR(){return pc1yR;}
  float          get_pc1zR(){return pc1zR;}
  float          get_pc2xR(){return pc2xR;}
  float          get_pc2yR(){return pc2yR;}
  float          get_pc2zR(){return pc2zR;}
  float          get_pc3xR(){return pc3xR;}
  float          get_pc3yR(){return pc3yR;}
  float          get_pc3zR(){return pc3zR;}
  float          get_tofxR(){return tofxR;}
  float          get_tofyR(){return tofyR;}
  float          get_tofzR(){return tofzR;}
  float          get_toftR(){return toftR;}
  float          get_tofeR(){return tofeR;}
  int            get_tofpidR(){return tofpidR;}
  float          get_tecxinR(){return tecxinR;}
  float          get_tecyinR(){return tecyinR;}
  float          get_teczinR(){return teczinR;}
  float          get_tecxoutR(){return tecxoutR;}
  float          get_tecyoutR(){return tecyoutR;}
  float          get_teczoutR(){return teczoutR;}

  float          get_emcxR(){return emcxR;}
  float          get_emcyR(){return emcyR;}
  float          get_emczR(){return emczR;}
  short          get_emcswkeyR(){return emcswkeyR;}
  float          get_emcmeaseR(){return emcmeaseR;}
  float          get_emcecoreR(){return emcecoreR;}
  float          get_emcecentR(){return emcecentR;}
  float          get_emcecorrR(){return emcecorrR;}
  float          get_emctofR(){return emctofR;}
  float          get_emctofcorrR(){return emctofcorrR;}
  float          get_emctofminR(){return emctofminR;}
  float          get_emcprobphotR(){return emcprobphotR;}
  short          get_emctwrhitR(){return emctwrhitR;}
  float          get_emcchi2R(){return emcchi2R;}
  float          get_emcpartesum0R(){return emcpartesum0R;}
  float          get_emcpartesum1R(){return emcpartesum1R;}
  float          get_emcpartesum2R(){return emcpartesum2R;}
  float          get_emcpartesum3R(){return emcpartesum3R;}
  
  float          get_ppc1xR(){return ppc1xR;}
  float          get_ppc1yR(){return ppc1yR;}
  float          get_ppc1zR(){return ppc1zR;}
  float          get_ppc2xR(){return ppc2xR;}
  float          get_ppc2yR(){return ppc2yR;}
  float          get_ppc2zR(){return ppc2zR;}
  float          get_ppc3xR(){return ppc3xR;}
  float          get_ppc3yR(){return ppc3yR;}
  float          get_ppc3zR(){return ppc3zR;}
  float          get_ptofxR(){return ptofxR;}
  float          get_ptofyR(){return ptofyR;}
  float          get_ptofzR(){return ptofzR;}
  float          get_ptecxR(){return ptecxR;}
  float          get_ptecyR(){return ptecyR;}
  float          get_pteczR(){return pteczR;}
  float          get_pemcxR(){return pemcxR;}
  float          get_pemcyR(){return pemcyR;}
  float          get_pemczR(){return pemczR;}
  float          get_tecdin(){return tecdin;}
  float          get_tecdout(){return tecdout;}

  float          get_pltofR(){return pltofR;}
  float          get_plcrkR(){return plcrkR;}
  float          get_plemcR(){return plemcR;}

  short          get_aerboxidR(){return aerboxidR;}
  float          get_aerph1R(){return aerph1R;}
  float          get_aerph2R(){return aerph2R;}
  
  short          get_nx1x2fitR(){return nx1x2fitR;}
  float          get_mchi2R(){return mchi2R;}
  short          get_errR(){return errR;}
  float          get_alphafR(){return alphafR;}

  short          get_crkaccR(){return crkaccR;}
  short          get_crknpmt0R(){return crknpmt0R;}
  short          get_crknpmt1R(){return crknpmt1R;}
  short          get_crknpmt3R(){return crknpmt3R;}
  float          get_crknpe0R(){return crknpe0R;}
  float          get_crknpe1R(){return crknpe1R;}
  float          get_crknpe3R(){return crknpe3R;}
  float          get_crkchi2R(){return crkchi2R;}
  float          get_crkdispR(){return crkdispR;}
  float          get_crkpathR(){return crkpathR;}

  // fields for main contributor analysis or other efficiency study . 
  short          get_sumfound(){return sumfound;}
  short          get_solution(){return solution;}

  short          get_mulmainS(){return mulmainS;}
  short          get_xmulmainS(){return xmulmainS;}
  short          get_uvmulmainS(){return uvmulmainS;}
  short          get_mainIDS(){return mainIDS;}
  short          get_xmainIDS(){return xmainIDS;}
  short          get_uvmainIDS(){return uvmainIDS;}
  float          get_purityS(){return purityS;}
  float          get_xpurityS(){return xpurityS;}
  float          get_uvpurityS(){return uvpurityS;}
  short          get_sumfoundS(){return sumfoundS;}
  short          get_solutionS(){return solutionS;}

  short          get_mulmainR(){return mulmainR;}
  short          get_xmulmainR(){return xmulmainR;}
  short          get_uvmulmainR(){return uvmulmainR;}
  short          get_mainIDR(){return mainIDR;}
  short          get_xmainIDR(){return xmainIDR;}
  short          get_uvmainIDR(){return uvmainIDR;}
  float          get_purityR(){return purityR;}
  float          get_xpurityR(){return xpurityR;}
  float          get_uvpurityR(){return uvpurityR;}
  short          get_sumfoundR(){return sumfoundR;}
  short          get_solutionR(){return solutionR;}

  float          get_pc2sS(){return pc2sS;}
  float          get_pc2sdphiS(){return pc2sdphiS;}
  float          get_pc2sdzS(){return pc2sdzS;}
  float          get_pc2dphiS(){return pc2dphiS;}
  float          get_pc2dzS(){return pc2dzS;}
  float          get_pc3sS(){return pc3sS;}
  float          get_pc3sdphiS(){return pc3sdphiS;}
  float          get_pc3sdzS(){return pc3sdzS;}
  float          get_pc3dphiS(){return pc3dphiS;}
  float          get_pc3dzS(){return pc3dzS;}
  float          get_tofsS(){return tofsS;}
  float          get_tofsdphiS(){return tofsdphiS;}
  float          get_tofsdzS(){return tofsdzS;}
  float          get_tofdphiS(){return tofdphiS;}
  float          get_tofdzS(){return tofdzS;}
  float          get_emcsS(){return emcsS;}
  float          get_emcsdphiS(){return emcsdphiS;}
  float          get_emcsdzS(){return emcsdzS;}
  float          get_emcdphiS(){return emcdphiS;}
  float          get_emcdzS(){return emcdzS;}

  float          get_pc2sR(){return pc2sR;}
  float          get_pc2sdphiR(){return pc2sdphiR;}
  float          get_pc2sdzR(){return pc2sdzR;}
  float          get_pc2dphiR(){return pc2dphiR;}
  float          get_pc2dzR(){return pc2dzR;}
  float          get_pc3sR(){return pc3sR;}
  float          get_pc3sdphiR(){return pc3sdphiR;}
  float          get_pc3sdzR(){return pc3sdzR;}
  float          get_pc3dphiR(){return pc3dphiR;}
  float          get_pc3dzR(){return pc3dzR;}
  float          get_tofsR(){return tofsR;}
  float          get_tofsdphiR(){return tofsdphiR;}
  float          get_tofsdzR(){return tofsdzR;}
  float          get_tofdphiR(){return tofdphiR;}
  float          get_tofdzR(){return tofdzR;}
  float          get_emcsR(){return emcsR;}
  float          get_emcsdphiR(){return emcsdphiR;}
  float          get_emcsdzR(){return emcsdzR;}
  float          get_emcdphiR(){return emcdphiR;}
  float          get_emcdzR(){return emcdzR;}

  short          get_x1hSG(){return x1hSG;}
  short          get_x2hSG(){return x2hSG;}
  short          get_uvhSG(){return uvhSG;}
  short          get_x1hRG(){return x1hRG;}
  short          get_x2hRG(){return x2hRG;}
  short          get_uvhRG(){return uvhRG;}
  short          get_x1hRS(){return x1hRS;}
  short          get_x2hRS(){return x2hRS;}
  short          get_uvhRS(){return uvhRS;}


  //NEW TOFW GET FUNCTIONS
  float  get_tofwidMc(){return tofwidMc;}
  float  get_tofwxMc(){return tofwxMc;}
  float  get_tofwyMc(){return tofwyMc;}
  float  get_tofwzMc(){return tofwzMc;}
  float  get_tofwtMc(){return tofwtMc;}
  float  get_tofweMc(){return tofweMc;}
  float  get_tofwtdcupMc(){return tofwtdcupMc;}
  float  get_tofwadcupMc(){return tofwadcupMc;}
  float  get_tofwtdcdnMc(){return tofwtdcdnMc;}
  float  get_tofwadcdnMc(){return tofwadcdnMc;}
  float  get_tofwpidMc(){return tofwpidMc;}
  float  get_pltofwMc(){return pltofwMc;}
  
  int    get_tofwembed(){return tofwembed;}
  float  get_tofwidS(){return tofwidS;}
  int    get_tofwstripS(){return tofwstripS;}
  float  get_tofwxS(){return tofwxS;}
  float  get_tofwyS(){return tofwyS;}
  float  get_tofwzS(){return tofwzS;}
  float  get_tofwtS(){return tofwtS;}
  float  get_tofweS(){return tofweS;}
  float  get_tofwtdcupS(){return tofwtdcupS;}
  float  get_tofwadcupS(){return tofwadcupS;}
  float  get_tofwtdcdnS(){return tofwtdcdnS;}
  float  get_tofwadcdnS(){return tofwadcdnS;}
  float  get_tofwpidS(){return tofwpidS;}
  float  get_ptofwxS(){return ptofwxS;}
  float  get_ptofwyS(){return ptofwyS;}
  float  get_ptofwzS(){return ptofwzS;}
  float  get_pltofwS(){return pltofwS;}
  
  float  get_tofwidE(){return tofwidE;}
  float  get_tofwidR(){return tofwidR;}
  int    get_tofwstripR(){return tofwstripR;}
  float  get_tofwxR(){return tofwxR;}
  float  get_tofwyR(){return tofwyR;}
  float  get_tofwzR(){return tofwzR;}
  float  get_tofwtR(){return tofwtR;}
  float  get_tofweR(){return tofweR;}
  float  get_tofwtdcupR(){return tofwtdcupR;}
  float  get_tofwadcupR(){return tofwadcupR;}
  float  get_tofwtdcdnR(){return tofwtdcdnR;}
  float  get_tofwadcdnR(){return tofwadcdnR;}
  float  get_tofwpidR(){return tofwpidR;}
  float  get_ptofwxR(){return ptofwxR;}
  float  get_ptofwyR(){return ptofwyR;}
  float  get_ptofwzR(){return ptofwzR;}
  float  get_pltofwR(){return pltofwR;}


  //Somehow I missed all of these the first time around... - Ron
  float get_tofwsS(){return tofwsS;}
  float get_tofwsdzS(){return tofwsdzS;}
  float get_tofwsdphiS(){return tofwsdphiS;}

  float get_tofwsR(){return tofwsR;}
  float get_tofwsdzR(){return tofwsdzR;}
  float get_tofwsdphiR(){return tofwsdphiR;}

  float get_tofwdzS(){return tofwdzS;}
  float get_tofwdphiS(){return tofwdphiS;}

  float get_tofwdzR(){return tofwdzR;}
  float get_tofwdphiR(){return tofwdphiR;}





  //-------------------------------SET methods
  void           set_type(int val){type = val;}
  void           set_evtID(int val){ evtID = val;}
  void           set_idGeant(int val){ idGeant = val;}
  void           set_evtxG(float val){ evtxG = val;}
  void           set_evtyG(float val){ evtyG = val;}
  void           set_evtzG(float val){ evtzG = val;}
  void           set_pptG(float val){ pptG = val;}
  void           set_ppzG(float val){ ppzG = val;}
  void           set_pthe0G(float val){ pthe0G = val;}
  void           set_pphi0G(float val){ pphi0G = val;}
  void           set_prapiG(float val){ prapiG = val;}

  void           set_genG(short val){ genG = val;}
  void           set_partidG(int val){ partidG = val;}
  void           set_pareidG(int val){ pareidG = val;}
  void           set_primidG(int val){ primidG = val;}
  void           set_xvtxG(float val){ xvtxG = val;}
  void           set_yvtxG(float val){ zvtxG = val;}
  void           set_zvtxG(float val){ zvtxG = val;}

  void           set_ntrkG(short val){ ntrkG = val;}
  void           set_dctrkidG(short val){ dctrkidG = val;}
  void           set_dctrkQualG(short val){ dctrkQualG = val;}
  void           set_the0G(float val){ the0G = val;}
  void           set_phi0G(float val){ phi0G = val;}
  void           set_alphaG(float val){ alphaG = val;}
  void           set_thetaG(float val){ thetaG = val;}
  void           set_phiG(float val){ phiG = val;}
  void           set_betaG(float val){ betaG = val;}
  void           set_zedG(float val){ zedG = val;}
  void           set_momG(float val){ momG = val;}
  void           set_ptG(float val){ ptG = val;}
  void           set_x1mG(float val){ x1mG = val;}
  void           set_x2mG(float val){ x2mG = val;}
  void           set_x1hG(short val){ x1hG = val;}
  void           set_x2hG(short val){ x2hG = val;}
  void           set_uv1hG(short val){ uv1hG  = val;}
  void           set_uv2hG(short val){ uv2hG = val;}
  
  void           set_bbcqn(float val){ bbcqn  = val;}
  void           set_zdcen(float val){ zdcen  = val;}
  void           set_bbcqs(float val){ bbcqs  = val;}
  void           set_zdces(float val){ zdces  = val;}
  void           set_bbcvtx(float val){ bbcvtx = val;}
  void           set_bbct0(float val){ bbct0 = val;}
  void           set_isminbias(short val){isminbias = val;}
  void           set_bbccent(float val){ bbccent = val;}
  void           set_centclock(float val){ centclock = val;}

  void           set_pc1idMc(short val){pc1idMc = val;}
  void           set_pc2idMc(short val){pc2idMc = val;}
  void           set_pc3idMc(short val){pc3idMc = val;}
  void           set_tofidMc(short val){tofidMc = val;}
  void           set_tecidMc(short val){tecidMc = val;}
  void           set_emcidMc(short val){emcidMc = val;}

  void           set_pc1xMc(float val){ pc1xMc = val;}
  void           set_pc1yMc(float val){ pc1yMc = val;}
  void           set_pc1zMc(float val){ pc1zMc = val;}
  void           set_pc2xMc(float val){ pc2xMc = val;}
  void           set_pc2yMc(float val){ pc2yMc = val;}
  void           set_pc2zMc(float val){ pc2zMc = val;}
  void           set_pc3xMc(float val){ pc3xMc = val;}
  void           set_pc3yMc(float val){ pc3yMc = val;}
  void           set_pc3zMc(float val){ pc3zMc = val;}
  void           set_tofxMc(float val){ tofxMc = val;}
  void           set_tofyMc(float val){ tofyMc = val;}
  void           set_tofzMc(float val){ tofzMc = val;}
  void           set_toftMc(float val){ toftMc = val;}
  void           set_tofeMc(float val){ tofeMc = val;}
  void           set_tofpidMc(int val){ tofpidMc = val;}
  void           set_tecxMc(float val){ tecxMc = val;}
  void           set_tecyMc(float val){ tecyMc = val;}
  void           set_teczMc(float val){ teczMc = val;}
  void           set_emcxMc(float val){ emcxMc = val;}
  void           set_emcyMc(float val){ emcyMc = val;}
  void           set_emczMc(float val){ emczMc = val;}
  void           set_pltofMc(float val){ pltofMc = val;}
  void           set_plcrkMc(float val){ plcrkMc = val;}
  void           set_plemcMc(float val){ plemcMc = val;}

  void           set_emctrkno0Mc(short val){emctrkno0Mc = val;}
  void           set_emctrkno1Mc(short val){emctrkno1Mc = val;}
  void           set_emctrkno2Mc(short val){emctrkno2Mc = val;}
  void           set_emctwrhit0Mc(short val){emctwrhit0Mc = val;}
  void           set_emctwrhit1Mc(short val){emctwrhit1Mc = val;}
  void           set_emctwrhit2Mc(short val){emctwrhit2Mc = val;}
  void           set_emcpid0Mc(short val){emcpid0Mc = val;}
  void           set_emcpid1Mc(short val){emcpid1Mc = val;}
  void           set_emcpid2Mc(short val){emcpid2Mc = val;}
  void           set_emcedep0Mc(float val){emcedep0Mc = val;}
  void           set_emcedep1Mc(float val){emcedep1Mc = val;}
  void           set_emcedep2Mc(float val){emcedep2Mc = val;}
  void           set_emcptot0Mc(float val){emcptot0Mc = val;}
  void           set_emcptot1Mc(float val){emcptot1Mc = val;}
  void           set_emcptot2Mc(float val){emcptot2Mc = val;}

  //response information
  void           set_ntrkS(short val){ ntrkS = val;}
  void           set_dctrkidS(short val){ dctrkidS = val;}
  void           set_dctrkQualS(short val){ dctrkQualS = val;}
  void           set_the0S(float val){ the0S = val;}
  void           set_phi0S(float val){ phi0S = val;}
  void           set_alphaS(float val){ alphaS = val;}
  void           set_thetaS(float val){thetaS = val;}
  void           set_phiS(float val){phiS = val;}
  void           set_betaS(float val){ betaS = val;}
  void           set_zedS(float val){ zedS = val;}
  void           set_dcchi2S(float val){ dcchi2S = val;}
  void           set_momS(float val){ momS = val;}
  void           set_ptS(float val){ ptS = val;}
  void           set_x1mS(float val){ x1mS = val;}
  void           set_x2mS(float val){ x2mS = val;}
  void           set_x1hS(short val){ x1hS = val;}
  void           set_x2hS(short val){ x2hS = val;}
  void           set_uv1hS(short val){ uv1hS = val;}
  void           set_uv2hS(short val){ uv2hS = val;}
  void           set_pc1idS(short val){ pc1idS  = val;}
  void           set_pc2idS(short val){ pc2idS = val;}
  void           set_pc3idS(short val){ pc3idS = val;}
  void           set_tofidS(short val){ tofidS = val;}
  void           set_tecidS(short val){ tecidS = val;}
  void           set_emcidS(short val){ emcidS = val;}  
  void           set_pc1xS(float val){ pc1xS = val;}
  void           set_pc1yS(float val){ pc1yS = val;}
  void           set_pc1zS(float val){ pc1zS = val;}
  void           set_pc2xS(float val){ pc2xS = val;}
  void           set_pc2yS(float val){ pc2yS = val;}
  void           set_pc2zS(float val){ pc2zS = val;}
  void           set_pc3xS(float val){ pc3xS = val;}
  void           set_pc3yS(float val){ pc3yS = val;}
  void           set_pc3zS(float val){ pc3zS = val;}
  void           set_tofxS(float val){ tofxS = val;}
  void           set_tofyS(float val){ tofyS = val;}
  void           set_tofzS(float val){ tofzS = val;}
  void           set_toftS(float val){ toftS = val;}
  void           set_tofeS(float val){ tofeS = val;}
  void           set_tofpidS(short val){ tofpidS = val;}
  void           set_tecxinS(float val){ tecxinS = val;}
  void           set_tecyinS(float val){ tecyinS = val;}
  void           set_teczinS(float val){ teczinS = val;}
  void           set_tecxoutS(float val){ tecxoutS = val;}
  void           set_tecyoutS(float val){ tecyoutS = val;}
  void           set_teczoutS(float val){ teczoutS = val;}

  void           set_emcxS(float val){ emcxS = val;}
  void           set_emcyS(float val){ emcyS = val;}
  void           set_emczS(float val){ emczS = val;}
  void           set_emcswkeyS(short val){emcswkeyS = val;}
  void           set_emcmeaseS(float val){emcmeaseS = val;}
  void           set_emcecoreS(float val){emcecoreS = val;}
  void           set_emcecentS(float val){emcecentS = val;}
  void           set_emcecorrS(float val){emcecorrS = val;}
  void           set_emctofS(float val){emctofS = val;}
  void           set_emctofcorrS(float val){emctofcorrS = val;}
  void           set_emctofminS(float val){emctofminS = val;}
  void           set_emcprobphotS(float val){emcprobphotS = val;}
  void           set_emctwrhitS(short val){emctwrhitS = val;}
  void           set_emcchi2S(float val){emcchi2S = val;}
  void           set_emcpartesum0S(float val){emcpartesum0S = val;}
  void           set_emcpartesum1S(float val){emcpartesum1S = val;}
  void           set_emcpartesum2S(float val){emcpartesum2S = val;}
  void           set_emcpartesum3S(float val){emcpartesum3S = val;}

  void           set_ppc1xS(float val){ ppc1xS = val;}
  void           set_ppc1yS(float val){ ppc1yS = val;}
  void           set_ppc1zS(float val){ ppc1zS = val;}
  void           set_ppc2xS(float val){ ppc2xS = val;}
  void           set_ppc2yS(float val){ ppc2yS = val;}
  void           set_ppc2zS(float val){ ppc2zS = val;}
  void           set_ppc3xS(float val){ ppc3xS = val;}
  void           set_ppc3yS(float val){ ppc3yS = val;}
  void           set_ppc3zS(float val){ ppc3zS = val;}
  void           set_ptofxS(float val){ ptofxS = val;}
  void           set_ptofyS(float val){ ptofyS = val;}
  void           set_ptofzS(float val){ ptofzS = val;}
  void           set_ptecxS(float val){ ptecxS = val;}
  void           set_ptecyS(float val){ ptecyS = val;}
  void           set_pteczS(float val){ pteczS = val;}
  void           set_pemcxS(float val){ pemcxS = val;}
  void           set_pemcyS(float val){ pemcyS = val;}
  void           set_pemczS(float val){ pemczS = val;}
  
  void           set_pltofS(float val){ pltofS = val;}
  void           set_plcrkS(float val){ plcrkS = val;}
  void           set_plemcS(float val){ plemcS = val;}
  
  void           set_aerboxidS(short val){ aerboxidS = val;}
  void           set_aerph1S(float val){ aerph1S = val;}
  void           set_aerph2S(float val){ aerph2S = val;}

  void           set_nx1x2fitS(short val){ nx1x2fitS = val;}
  void           set_mchi2S(float val){ mchi2S = val;}
  void           set_errS(short val){ errS = val;}
  void           set_alphafS(float val){ alphafS = val;}

  void           set_crkaccS(short val){crkaccS = val;}
  void           set_crknpmt0S(short val){crknpmt0S = val;}
  void           set_crknpmt1S(short val){crknpmt1S = val;}
  void           set_crknpmt3S(short val){crknpmt3S = val;}
  void           set_crknpe0S(float val){crknpe0S = val;}
  void           set_crknpe1S(float val){crknpe1S = val;}
  void           set_crknpe3S(float val){crknpe3S = val;}
  void           set_crkchi2S(float val){crkchi2S = val;}
  void           set_crkdispS(float val){crkdispS = val;}
  void           set_crkpathS(float val){crkpathS = val;}

  
  //information to keep after embedding 
  void           set_x1hE(short val){ x1hE = val;}
  void           set_x2hE(short val){ x2hE = val;}
  void           set_uv1hE(short val){ uv1hE = val;}
  void           set_uv2hE(short val){ uv2hE = val;}
  void           set_pc1idE(short val){ pc1idE = val;}
  void           set_pc2idE(short val){ pc2idE = val;}
  void           set_pc3idE(short val){ pc3idE = val;}
  void           set_tofidE(short  val){ tofidE = val;}
  void           set_tecidE(short val){ tecidE = val;}
  void           set_emcidE(short  val){ emcidE = val;}  
  //reconstruction information
  void           set_ntrkb(short val){ ntrkb = val;}
  void           set_ntrka(short val){ ntrka = val;}
  void           set_dctrkidR(short val){ dctrkidR = val;}
  void           set_dctrkQualR(short val){ dctrkQualR = val;}
  void           set_the0R(float val){ the0R = val;}
  void           set_phi0R(float val){ phi0R = val;}
  void           set_alphaR(float val){ alphaR = val;}
  void           set_thetaR(float val){thetaR = val;}
  void           set_phiR(float val){phiR = val;}
  void           set_betaR(float val){ betaR = val;}
  void           set_zedR(float val){ zedR = val;}
  void           set_momR(float val){ momR = val;}
  void           set_ptR(float val){ ptR = val;}
  void           set_dcchi2R(float val){ dcchi2R = val;}
  void           set_x1mR(float val){ x1mR = val;}
  void           set_x2mR(float val){ x2mR = val;}
  void           set_x1hR(short val){ x1hR = val;}
  void           set_x2hR(short val){ x2hR = val;}
  void           set_uv1hR(short val){ uv1hR = val;}
  void           set_uv2hR(short val){ uv2hR = val;}
  
  void           set_pc1idR(short val){ pc1idR = val;}
  void           set_pc2idR(short val){ pc2idR = val;}
  void           set_pc3idR(short val){ pc3idR = val;}
  void           set_tofidR(short val){ tofidR = val;}
  void           set_tecidR(short val){ tecidR = val;}
  void           set_emcidR(short val){ emcidR = val;}

  void           set_pc1xR(float val){ pc1xR = val;}
  void           set_pc1yR(float val){ pc1yR = val;}
  void           set_pc1zR(float val){ pc1zR = val;}
  void           set_pc2xR(float val){ pc2xR = val;}
  void           set_pc2yR(float val){ pc2yR = val;}
  void           set_pc2zR(float val){ pc2zR = val;}
  void           set_pc3xR(float val){ pc3xR = val;}
  void           set_pc3yR(float val){ pc3yR = val;}
  void           set_pc3zR(float val){ pc3zR = val;}
  void           set_tofxR(float val){ tofxR = val;}
  void           set_tofyR(float val){ tofyR = val;}
  void           set_tofzR(float val){ tofzR = val;}
  void           set_toftR(float val){ toftR = val;}
  void           set_tofeR(float val){ tofeR = val;}
  void           set_tofpidR(short val){ tofpidR = val;}
  void           set_tecxinR(float val){ tecxinR = val;}
  void           set_tecyinR(float val){ tecyinR = val;}
  void           set_teczinR(float val){ teczinR = val;}
  void           set_tecxoutR(float val){ tecxoutR = val;}
  void           set_tecyoutR(float val){ tecyoutR = val;}
  void           set_teczoutR(float val){ teczoutR = val;}

  void           set_emcxR(float val){ emcxR = val;}
  void           set_emcyR(float val){ emcyR = val;}
  void           set_emczR(float val){ emczR = val;}
  void           set_emcswkeyR(short val){emcswkeyR = val;}
  void           set_emcmeaseR(float val){emcmeaseR = val;}
  void           set_emcecoreR(float val){emcecoreR = val;}
  void           set_emcecentR(float val){emcecentR = val;}
  void           set_emcecorrR(float val){emcecorrR = val;}
  void           set_emctofR(float val){emctofR = val;}
  void           set_emctofcorrR(float val){emctofcorrR = val;}
  void           set_emctofminR(float val){emctofminR = val;}
  void           set_emcprobphotR(float val){emcprobphotR = val;}
  void           set_emctwrhitR(short val){emctwrhitR = val;}
  void           set_emcchi2R(float val){emcchi2R = val;}
  void           set_emcpartesum0R(float val){emcpartesum0R = val;}
  void           set_emcpartesum1R(float val){emcpartesum1R = val;}
  void           set_emcpartesum2R(float val){emcpartesum2R = val;}
  void           set_emcpartesum3R(float val){emcpartesum3R = val;}
  
  void           set_ppc1xR(float val){ ppc1xR = val;}
  void           set_ppc1yR(float val){ ppc1yR = val;}
  void           set_ppc1zR(float val){ ppc1zR = val;}
  void           set_ppc2xR(float val){ ppc2xR = val;}
  void           set_ppc2yR(float val){ ppc2yR = val;}
  void           set_ppc2zR(float val){ ppc2zR = val;}
  void           set_ppc3xR(float val){ ppc3xR = val;}
  void           set_ppc3yR(float val){ ppc3yR = val;}
  void           set_ppc3zR(float val){ ppc3zR = val;}
  void           set_ptofxR(float val){ ptofxR = val;}
  void           set_ptofyR(float val){ ptofyR = val;}
  void           set_ptofzR(float val){ ptofzR = val;}
  void           set_ptecxR(float val){ ptecxR = val;}
  void           set_ptecyR(float val){ ptecyR = val;}
  void           set_pteczR(float val){ pteczR = val;}
  void           set_pemcxR(float val){ pemcxR = val;}
  void           set_pemcyR(float val){ pemcyR = val;}
  void           set_pemczR(float val){ pemczR = val;}
  void           set_tecdin(float val){ tecdin = val;}
  void           set_tecdout(float val){ tecdout = val;}

  void           set_pltofR(float val){ pltofR = val;}
  void           set_plcrkR(float val){ plcrkR = val;}
  void           set_plemcR(float val){ plemcR = val;}

  void           set_aerboxidR(short val){ aerboxidR = val;}
  void           set_aerph1R(float val){ aerph1R = val;}
  void           set_aerph2R(float val){ aerph2R = val;}

  void           set_nx1x2fitR(short val){ nx1x2fitR = val;}
  void           set_mchi2R(float val){ mchi2R = val;}
  void           set_errR(short val){ errR = val;}
  void           set_alphafR(float val){ alphafR = val;}
  
  void           set_crkaccR(short val){crkaccR = val;}
  void           set_crknpmt0R(short val){crknpmt0R = val;}
  void           set_crknpmt1R(short val){crknpmt1R = val;}
  void           set_crknpmt3R(short val){crknpmt3R = val;}
  void           set_crknpe0R(float val){crknpe0R = val;}
  void           set_crknpe1R(float val){crknpe1R = val;}
  void           set_crknpe3R(float val){crknpe3R = val;}
  void           set_crkchi2R(float val){crkchi2R = val;}
  void           set_crkdispR(float val){crkdispR = val;}
  void           set_crkpathR(float val){crkpathR = val;}

  // fields for main contributor analysis or other efficiency study . 
  void           set_sumfound(short val){ sumfound = val;}
  void           set_solution(short val){ solution = val;}
  void           set_mulmainS(short val){ mulmainS= val;}
  void           set_xmulmainS(short val){ xmulmainS = val;}
  void           set_uvmulmainS(short val){ uvmulmainS = val;}
  void           set_mainIDS(short val){ mainIDS = val;}
  void           set_xmainIDS(short val){ xmainIDS = val;}
  void           set_uvmainIDS(short val){ uvmainIDS = val;}
  void           set_purityS(float val){ purityS = val;}
  void           set_xpurityS(float val){ xpurityS = val;}
  void           set_uvpurityS(float val){uvpurityS  = val;}
  void           set_sumfoundS(short val){ sumfoundS = val;}
  void           set_solutionS(short val){ solutionS = val;}  
  
  void           set_mulmainR(short val){ mulmainR= val;}
  void           set_xmulmainR(short val){ xmulmainR = val;}
  void           set_uvmulmainR(short val){ uvmulmainR = val;}
  void           set_mainIDR(short val){ mainIDR = val;}
  void           set_xmainIDR(short val){ xmainIDR = val;}
  void           set_uvmainIDR(short val){ uvmainIDR = val;}
  void           set_purityR(float val){ purityR = val;}
  void           set_xpurityR(float val){ xpurityR = val;}
  void           set_uvpurityR(float val){uvpurityR  = val;}
  void           set_sumfoundR(short val){ sumfoundR = val;}
  void           set_solutionR(short val){ solutionR = val;}

  void           set_pc2sS(float val){pc2sS = val;}
  void           set_pc2sdphiS(float val){ pc2sdphiS = val;}
  void           set_pc2sdzS(float val){ pc2sdzS = val;}
  void           set_pc2dphiS(float val){ pc2dphiS = val;}
  void           set_pc2dzS(float val){ pc2dzS = val;}
  void           set_pc3sS(float val){pc3sS = val;}
  void           set_pc3sdphiS(float val){ pc3sdphiS = val;}
  void           set_pc3sdzS(float val){ pc3sdzS = val;}
  void           set_pc3dphiS(float val){ pc3dphiS = val;}
  void           set_pc3dzS(float val){ pc3dzS = val;}
  void           set_tofsS(float val){tofsS = val;}
  void           set_tofsdphiS(float val){ tofsdphiS = val;}
  void           set_tofsdzS(float val){ tofsdzS = val;}
  void           set_tofdphiS(float val){ tofdphiS = val;}
  void           set_tofdzS(float val){ tofdzS = val;}
  void           set_emcsS(float val){emcsS = val;}
  void           set_emcsdphiS(float val){ emcsdphiS = val;}
  void           set_emcsdzS(float val){ emcsdzS = val;}
  void           set_emcdphiS(float val){ emcdphiS = val;}
  void           set_emcdzS(float val){ emcdzS = val;}

  void           set_pc2sR(float val){pc2sR = val;}
  void           set_pc2sdphiR(float val){ pc2sdphiR = val;}
  void           set_pc2sdzR(float val){ pc2sdzR = val;}
  void           set_pc2dphiR(float val){ pc2dphiR = val;}
  void           set_pc2dzR(float val){ pc2dzR = val;}
  void           set_pc3sR(float val){pc3sR = val;}
  void           set_pc3sdphiR(float val){ pc3sdphiR = val;}
  void           set_pc3sdzR(float val){ pc3sdzR = val;}
  void           set_pc3dphiR(float val){ pc3dphiR = val;}
  void           set_pc3dzR(float val){ pc3dzR = val;}
  void           set_tofsR(float val){tofsR = val;}
  void           set_tofsdphiR(float val){ tofsdphiR = val;}
  void           set_tofsdzR(float val){ tofsdzR = val;}
  void           set_tofdphiR(float val){ tofdphiR = val;}
  void           set_tofdzR(float val){ tofdzR = val;}
  void           set_emcsR(float val){emcsR = val;}
  void           set_emcsdphiR(float val){ emcsdphiR = val;}
  void           set_emcsdzR(float val){ emcsdzR = val;}
  void           set_emcdphiR(float val){ emcdphiR = val;}
  void           set_emcdzR(float val){ emcdzR = val;}

  void           set_x1hSG(short val){ x1hSG = val;}
  void           set_x2hSG(short val){ x2hSG = val;}
  void           set_uvhSG(short val){ uvhSG = val;}
  void           set_x1hRG(short val){ x1hRG = val;}
  void           set_x2hRG(short val){ x2hRG = val;}
  void           set_uvhRG(short val){ uvhRG = val;}
  void           set_x1hRS(short val){ x1hRS = val;}
  void           set_x2hRS(short val){ x2hRS = val;}
  void           set_uvhRS(short val){ uvhRS = val;}



  //NEW TOFW SET FUNCTIONS
  void  set_tofwidMc(float val){tofwidMc = val;}
  void  set_tofwxMc(float val){tofwxMc = val;}
  void  set_tofwyMc(float val){tofwyMc = val;}
  void  set_tofwzMc(float val){tofwzMc = val;}
  void  set_tofwtMc(float val){tofwtMc = val;}
  void  set_tofweMc(float val){tofweMc = val;}
  void  set_tofwtdcupMc(float val){tofwtdcdnMc = val;}
  void  set_tofwadcupMc(float val){tofwadcupMc = val;}
  void  set_tofwtdcdnMc(float val){tofwtdcdnMc = val;}
  void  set_tofwadcdnMc(float val){tofwadcdnMc = val;}
  void  set_tofwpidMc(float val){tofwpidMc = val;}
  void  set_pltofwMc(float val){pltofwMc = val;}

  void  set_tofwembed(int val){tofwembed = val;}
  void  set_tofwidS(float val){tofwidS = val;}
  void  set_tofwstripS(int val){tofwstripS=val;}
  void  set_tofwxS(float val){tofwxS = val;}
  void  set_tofwyS(float val){tofwyS = val;}
  void  set_tofwzS(float val){tofwzS = val;}
  void  set_tofwtS(float val){tofwtS = val;} 
  void  set_tofweS(float val){tofweS = val;} 
  void  set_tofwtdcupS(float val){tofwtdcdnS = val;}
  void  set_tofwadcupS(float val){tofwadcupS = val;}
  void  set_tofwtdcdnS(float val){tofwtdcdnS = val;}
  void  set_tofwadcdnS(float val){tofwadcdnS = val;}
  void  set_tofwpidS(float val){tofwpidS = val;}
  void  set_ptofwxS(float val){ptofwxS = val;}
  void  set_ptofwyS(float val){ptofwyS = val;}
  void  set_ptofwzS(float val){ptofwzS = val;}
  void  set_pltofwS(float val){pltofwS = val;}
  
  void  set_tofwidE(float val){tofwidE = val;}
  void  set_tofwidR(float val){tofwidR = val;}
  void  set_tofwstripR(int val){tofwstripR=val;}
  void  set_tofwxR(float val){tofwxR = val;}
  void  set_tofwyR(float val){tofwyR = val;}
  void  set_tofwzR(float val){tofwzR = val;}
  void  set_tofwtR(float val){tofwtR = val;} 
  void  set_tofweR(float val){tofweR = val;} 
  void  set_tofwtdcupR(float val){tofwtdcdnR = val;}
  void  set_tofwadcupR(float val){tofwadcupR = val;}
  void  set_tofwtdcdnR(float val){tofwtdcdnR = val;}
  void  set_tofwadcdnR(float val){tofwadcdnR = val;}
  void  set_tofwpidR(float val){tofwpidR = val;}
  void  set_ptofwxR(float val){ptofwxR = val;}
  void  set_ptofwyR(float val){ptofwyR = val;}
  void  set_ptofwzR(float val){ptofwzR = val;}
  void  set_pltofwR(float val){pltofwR = val;}

  //Somehow I missed all of these the first time around... - Ron
  void set_tofwsS(float val){tofwsS = val;}
  void set_tofwsdzS(float val){tofwsdzS = val;}
  void set_tofwsdphiS(float val){tofwsdphiS = val;}

  void set_tofwsR(float val){tofwsR = val;}
  void set_tofwsdzR(float val){tofwsdzR = val;}
  void set_tofwsdphiR(float val){tofwsdphiR = val;}

  void set_tofwdzS(float val){tofwdzS = val;}
  void set_tofwdphiS(float val){tofwdphiS = val;}

  void set_tofwdzR(float val){tofwdzR = val;}
  void set_tofwdphiR(float val){tofwdphiR = val;}





};
#endif /* _DCHMCRECOTRACK_H */ // What the hell???

//#endif _PHEMBEDMCRECOTRACK_H // Why not this one???
