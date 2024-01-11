//************************************************************
// Parameter set-up macro written by the Pyrite GUI for PHOOL 
//************************************************************

{


  // Setting dPadRecPar Parameters
  size_t nrc=1;
  dPadRecPar->SetRowCount(nrc);
  dPadRecPar->set_verbose(0,0);
  dPadRecPar->set_method(0,0,0);
  dPadRecPar->set_method(1,0,0);
  dPadRecPar->set_method(2,0,0);

  // Setting mPadDetGeo Parameters
  double pc1Radius = 248.891;
  double pc2Radius = 419.173;
  double pc3Radius = 492.012;
  double ThetaArm[2] = { -0.589049, 2.15984};
  mPadDetGeo->set_pc1Radius(pc1Radius);
  mPadDetGeo->set_pc2Radius(pc2Radius);
  mPadDetGeo->set_pc3Radius(pc3Radius);
  mPadDetGeo->set_Theta0(ThetaArm);

  // Setting padInclBad parameters
  padInclBad->doNotInclBadChs();

}

