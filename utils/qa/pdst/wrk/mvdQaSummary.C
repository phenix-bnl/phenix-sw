{

  textFile << " ----------------------------------------------------" << endl;
  textFile << " -- MVD QA Summary --" << endl;
  textFile << " ----------------------------------------------------" << endl;
  
  Int_t mvdstatus = 0;
  float zoffset = 999;
  float veffec = 0;

  // histogram binning 
  // should be obtained from the histogram ...
  const float vbin=240;
  const float vmin=-30;
  const float vmax=30;

  float dbin, xmax, maxbin, binl, binr;
  double par[3];
  
  TH1F *mvd_bbc = (TH1F *) qafile->Get ("mvd_bbc");
  
  // add here analysis and printout results to textFile
  
  maxbin=mvd_bbc->GetMaximumBin();

  TF1 f12("f12","gaus",-2,2);
  f12.SetParameter(1, 0);
  f12.SetParameter(2, 1.0);
  f12.SetLineColor(4);
  
  mvd_bbc->Fit("f12", "RQN");
  f12.GetParameters(par);
  zoffset=par[1];
  //cout << endl << "BBC MVD vertex offset: " <<par[1] << endl;
  
  dbin=(vmax-vmin)/vbin;
  binl=int((par[1]-1.2-vmin)/dbin+0.5);
  binr=int((par[1]+1.2-vmin)/dbin+0.5);
  
  //cout << "Left and right peak cut: " << par[1]-1.2 << " " 
  //     << par[1]+1.2 << endl;
  //cout << "Number of counts in peak: " << mvd_bbc->Integral(binl,binr) << endl;
  

	if(mvd_bbc->Integral(0,vbin)>0)
  {
	  veffec=mvd_bbc->Integral(binl,binr)/mvd_bbc->Integral(0,vbin);
  }
  else
  {
	  veffec=0.;
  }
  textFile << " MVD z vertex offset compared to BBC =" << zoffset << endl;
  textFile << " MVD vertex reconstruction efficiency =" << veffec << endl;
  textFile << " MVD dn/deta values are NOT reliable =" << zoffset << endl;

  if (veffec>0.90 ) { 
    mvdstatus=0;
  }
  else {
    mvdstatus=int(veffec*100)+100;
  }

  textFile << " MVD Status: " << mvdstatus << endl;
  statusFile << mvdstatus << " ";
}



