
{
  int nrc = 1;
  dDchRecoPar->SetRowCount(nrc);
  dDchRecoPar->set_mirrorHitAnalysis(0,0);
  dDchRecoPar->set_houghThresholdOnXCell(0,10) ;  // not used
  dDchRecoPar->set_houghThresholdOnXMask(0,15) ;  // not used
  dDchRecoPar->set_houghThresholdOnUVCell(0,3);  // not used
  dDchRecoPar->set_houghThresholdOnUVMask(0,6) ; // not used
  dDchRecoPar->set_purgeCandidateThreshold(0,15) ; // not used
  dDchRecoPar->set_firstXHoughThreshold(0,10) ;  // mc is 15
  dDchRecoPar->set_secondXHoughThreshold(0,10) ; // mc is 15
  dDchRecoPar->set_minimumNumberOfXHits(0,8);
  dDchRecoPar->set_minimumNumberOfUVHits(0,0);

  dDchRecoPar->set_XHitsThreshold(0,10);
  dDchRecoPar->set_cellDifferenceCut(0,8);
  
  dDchRecoPar->set_delBetaCut(0,0.2);
  dDchRecoPar->set_deltaBetaCut(0,0.2);
  dDchRecoPar->set_wireResolution(0,0.015); //
  dDchRecoPar->set_initUVChi2(0,10); // not used
  dDchRecoPar->set_initXChi2(0,5);  // not used 
  dDchRecoPar->set_deltaBetaVertexCut(0, 0.5); // for cosmic ray data!!
 
  dDchRecoPar->set_numberOfAlphaBins(0,300);
  dDchRecoPar->set_numberOfPhiBins(0,6000);
  dDchRecoPar->set_maxAlpha(0,0.8);
  dDchRecoPar->set_minAlpha(0,-0.8);
  dDchRecoPar->set_maxPhi(0,1.);
  dDchRecoPar->set_minPhi(0,-0.65);
  
  dDchRecoPar->set_numberOfBetaBins(0,60);
  dDchRecoPar->set_numberOfZedBins(0,200);
  dDchRecoPar->set_maxBeta(0,2.5);
  dDchRecoPar->set_minBeta(0,0.6);
  dDchRecoPar->set_maxZed(0,100.);
  dDchRecoPar->set_minZed(0,-100.);




}






