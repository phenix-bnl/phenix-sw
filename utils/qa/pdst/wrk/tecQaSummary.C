
{
  const Int_t nSector  = 4;
  const Int_t nPlane   = 6; 
  const Int_t nSide    = 2; // North = 1 South = 0
  const Int_t nTotal   = nSector * nPlane * nSide;
	//RUN 3 sector 0 is mostly off considered inactive - skipping it for qa
	//RUN 3 most plane 5 are  highly inefficient (fem off bacause of noise) - skipping them
  //RUN 3 sector 2 plane 2N and sector 3 plane 1S are dead - skipping them
  const Int_t active[nSide][nSector][nPlane] =
  { 
     0, 0, 0, 0, 0, 0,  // E 0 South  0 - 5
     1, 1, 1, 1, 1, 0,  // E 1 South  0 - 5
     1, 1, 1, 1, 1, 0,  // E 2 South  0 - 5
     1, 0, 1, 1, 1, 0,  // E 3 South  0 - 5 
     0, 0, 0, 0, 0, 0,  // E 0 North  0 - 5 
     1, 1, 1, 1, 1, 0,  // E 1 North  0 - 5
     1, 1, 0, 1, 1, 0,  // E 2 North  0 - 5
     1, 1, 1, 1, 1, 0,  // E 3 North  0 - 5
  }

  textFile << " ----------------------------------------------------" << endl;
  textFile << " -- TEC QA Summary --" << endl;
  textFile << " ----------------------------------------------------" << endl;
  TH1F **techist = new TH1F[4];

  techist[0] = (TH1F *) qafile->Get ("tecTrkMult");
  techist[1] = (TH1F *) qafile->Get ("tecTrkNhits");
  techist[2] = (TH1F *) qafile->Get ("tecNhitProf");
  techist[3] = (TH1F *) qafile->Get ("tecNtrkProf");
  cout << "Opened histograms " << endl;
  Float_t xentries[10], xmean[10], xrms[10];

  for (Int_t ih = 0; ih < 1; ih++)
  {
    xentries[ih] = techist[ih]->GetEntries ();
    xmean[ih] = techist[ih]->GetMean ();
    xrms[ih] = techist[ih]->GetRMS ();
  }
  cout << "Unpacked histograms " << endl;

  Float_t xn[48];

  for (Int_t ic = 0; ic < 48; ic++)
  {
    xn[ic] = techist[2]->GetBinContent (ic + 1);
  }

  Float_t xratio[4], xr;
  Int_t tecstatus, ineff;
  tecstatus = 0;
  ineff = 0;

  for (Int_t isec = 0; isec<nSector; isec++)
		{
			for (Int_t iside = 0; iside<nSide; iside++)
				{
					for (Int_t ipl = 0; ipl<nPlane; ipl++)
						{
              int ind=(isec*2+iside)*nPlane+ipl;
							if(active[iside][isec][ipl]==1)
								{
									//cout<<isec<<" "<<" "<<ipl<<" "<<iside<<" ind "<<ind<<" "<<xn[ind]<<endl;
									if(xn[ind] < 15.)ineff = ineff + 1;
								}
						}
				}
		}


  if (ineff > 0)
  tecstatus = 2;

  if (ineff > 2)
  tecstatus = 1;


  textFile << " TEC number of inefficient sector side " << ineff << endl;
  textFile << " Average TEC track multiplicity " << xmean[0] << endl;
  textFile << " Average number of time bins/track " << xmean[1] << endl;
  textFile << " TEC Status = " << tecstatus << endl;

  statusFile << tecstatus << " ";

}
