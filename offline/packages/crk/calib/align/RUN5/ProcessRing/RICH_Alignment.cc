#include "RICH_Alignment.h"

RICH_Alignment::RICH_Alignment()
{
  TROOT("RICH_Alignment", "RICH_Alignment");

  i_Eval = 0;

  SetMaxent(-1);
  SetEventOffset(0);
  SetVerbosity(0);
  VerifByPhi(1);

  SetAlignmentFile("alignment.dat");

  memset(f_dz, 0, sizeof(f_dz)); 
  memset(f_dphi, 0, sizeof(f_dphi));
  memset(f_dzfinal, 0, sizeof(f_dzfinal)); 
  memset(f_dphifinal, 0, sizeof(f_dphifinal));
}

RICH_Alignment::RICH_Alignment(char *dstin)
{
  TROOT("RICH_Alignment", "RICH_Alignment");

  i_Eval = 0;
  
  SetMaxent(-1);
  SetEventOffset(0);
  SetVerbosity(0);
  VerifByPhi(1);
  
  SetAlignmentFile("alignment.dat");

  memset(f_dz, 0, sizeof(f_dz)); 
  memset(f_dphi, 0, sizeof(f_dphi));
  memset(f_dzfinal, 0, sizeof(f_dzfinal)); 
  memset(f_dphifinal, 0, sizeof(f_dphifinal));

  memset(i_hotPMT, 0, sizeof(i_hotPMT)); 
  i_nhotPMT = 0;
}

void RICH_Alignment::ImportHotPMTList(char *hotlistf)
{
  ifstream if_hot(hotlistf);
  while(!if_hot.eof())
    {
      if_hot >> i_hotPMT[i_nhotPMT];
      i_nhotPMT++;
    }
  if_hot.close();
}

void RICH_Alignment::Init(char *infile)
{
  fin = new TFile(infile,"read");
  trk = (TTree*)fin->Get("trk");

  v_ref = new float[3];
  b_ref = new float[3];
  posx = new float[1000]; 
  posy = new float[1000];
  posz = new float[1000];
  posr = new float[1000];
  posphi = new float[1000];
  this_npe = new float[1000];

  if(!trk)
    {
      cout<<" track tree not found !"<<endl;
      exit(1);
    }

  trk->SetBranchAddress("arm", &arm);
  trk->SetBranchAddress("side", &side);
  trk->SetBranchAddress("panel", &panel);
  trk->SetBranchAddress("v_ref", v_ref);
  trk->SetBranchAddress("b_ref", b_ref);
  trk->SetBranchAddress("npmt", &this_npmt);
  trk->SetBranchAddress("posx", posx);
  trk->SetBranchAddress("posy", posy);
  trk->SetBranchAddress("posz", posz);
  trk->SetBranchAddress("posr", posr);
  trk->SetBranchAddress("posphi", posphi);
  trk->SetBranchAddress("npe", this_npe);
  trk->SetBranchAddress("n0", &n0);  
}

void RICH_Alignment::ProcessRings(int iloop)
{  
  ntrk = (int) trk->GetEntries();

  TCanvas *tc_printed[4];

  TH2F *h_ring[2][2][24];
  TH2F *h_ringtot = NULL, *h_ringinit = NULL;
  TH1F *h_R = NULL;
  TH1F *h_R_init = NULL;
  
  Text_t drawn[100], tsect[10];

  if(iloop == 20)
    {
      h_ringtot = new TH2F("h_ringtot", "Accumulated for all panels for QA",
			   60, -15, 15, 60, -15, 15);
      h_R = new TH1F("h_R", "Distance from ring center", 150, 0, 30);
    }
  if(iloop == -1)
    {
      h_ringinit = new TH2F("h_ringinit",
			    "Accumulated for all panels with non-aligned",
			    60, -15, 15, 60, -15, 15);
      h_R_init = new TH1F("h_R_init", "Distance from ring center", 150, 0, 30);
    }

  for(int iarm=0; iarm<2; iarm++)
    {
      for(int iside=0; iside<2; iside++)
	{
	  switch(iarm *2 + iside)
	    {
	    case 0:    sprintf(tsect, "WS"); break;
	    case 1:    sprintf(tsect, "WN"); break;
	    case 2:    sprintf(tsect, "ES"); break;
	    case 3:    sprintf(tsect, "EN"); break;
	    }
	  if(iloop == 20)
	    {
	      tc_printed[iarm*2+iside] = new TCanvas(tsect, tsect, 
						     10, 10, 600, 400);
	    }
	  for(int ip=0; ip<24; ip++)
	    {
	      sprintf(drawn, "%s_%d_%d", tsect, ip, iloop);
	      h_ring[iarm][iside][ip] = new TH2F(drawn, drawn, 
						 60, -15, 15, 60, -15, 15);
	    } 
	}
    }
  
  if(i_Verb>0 && iloop ==-1) cout << "Total entry picked up is " << ntrk 
				  << endl;
  if(i_Verb>0) cout << iloop << "th loop" << endl;

  for(int itrk=0; itrk<ntrk; itrk++)
    {
      if(i_Verb>2) cout << itrk << "th track " << arm << " " << this_npmt 
			<< endl;
      trk->GetEntry(itrk);
      //      if(n0 <= 2) continue;
      // Make frame of Reflected line.
      // Z is the reflected line. And X is directed to beam axis.

      PHVector newZ(v_ref[0], v_ref[1], v_ref[2]);
      newZ.normalize();
      PHVector newY = newZ.cross(PHVector(0, 0, 1));
      newY.normalize();
      PHVector newX = newY.cross(newZ);
      newX.normalize();

      //   Align Z and Phi axis to positive
      if(newX.getZ()<0) newX = -newX;
      if(i_VerifByPhi==0 && iloop!=20)
	{
	  if(newY.getY()<0) newY = -newY;
	}
      newZ = newX.cross(newY);
      
      PHPoint base(b_ref[0], b_ref[1], b_ref[2]);    
      PHFrame fr_ref(base, newX, newY, newZ);

      for(int ipmt=0; ipmt<this_npmt; ipmt++)
	{
	  if(i_Verb>2) cout << ipmt << "th pmt" << endl;
	  //    Set mirror alignment
	  PHPoint pmt_pos(posx[ipmt], posy[ipmt], posz[ipmt]);
	  
	  if(iloop > -1)
	    {
	      if(i_VerifByPhi)
		{
		  pmt_pos.setX(posr[ipmt] 
			       * cos(posphi[ipmt] - 
				     f_dphi[arm][side][panel][iloop]));
		  pmt_pos.setY(posr[ipmt] 
			       * sin(posphi[ipmt] - 
				     f_dphi[arm][side][panel][iloop]));
		  pmt_pos.setZ(posz[ipmt] - f_dz[arm][side][panel][iloop]);
		}
	      else
		{
		  PHPoint dzdphi(0, f_dphi[arm][side][panel][iloop], 
				 f_dz[arm][side][panel][iloop]);
		  pmt_pos = pmt_pos - dzdphi;
		}
	    }
      
	  if(iloop == 20)
	    {
	      pmt_pos.setX(posr[ipmt] * cos(posphi[ipmt] - 
					    f_dphifinal[arm][side][panel]));
	      pmt_pos.setY(posr[ipmt] * sin(posphi[ipmt] - 
					    f_dphifinal[arm][side][panel]));
	      pmt_pos.setZ(posz[ipmt] - f_dzfinal[arm][side][panel]);
	    }
      
	  PHPoint pmt_pos_trans = transformPoint(PHFrame(), pmt_pos, fr_ref);
	  h_ring[arm][side][panel]->Fill(pmt_pos_trans.getX(),
					 pmt_pos_trans.getY());

	  if(iloop == 20)
	    {
	      h_ringtot->Fill(pmt_pos_trans.getX(),
			      pmt_pos_trans.getY());
	      h_R->Fill(sqrt(pmt_pos_trans.getX()*pmt_pos_trans.getX() +
			     pmt_pos_trans.getY()*pmt_pos_trans.getY()));
	    }
	  
	  if(iloop == -1)
	    {
	      h_ringinit->Fill(pmt_pos_trans.getX(),
			       pmt_pos_trans.getY());
	      h_R_init->Fill(sqrt(pmt_pos_trans.getX()*pmt_pos_trans.getX() +
				  pmt_pos_trans.getY()*pmt_pos_trans.getY()));
	    }
	}
    }

  if(i_Verb>1) cout << iloop << "th loop Fill end" << endl;

  Double_t z, phi;
  TGraph *tg, *guide_in=NULL, *guide_out=NULL;
  float gxin[31], gyin[31], gxout[31], gyout[31];

  if(iloop==20)
    {
      for(int i=0; i<31; i++)
	{
	  gxin[i] = 3.4 * cos ((float)i * M_PI / 15.);
	  gyin[i] = 3.4 * sin ((float)i * M_PI / 15.);
	  gxout[i] = 8.4 * cos ((float)i * M_PI / 15.);
	  gyout[i] = 8.4 * sin ((float)i * M_PI / 15.);
	}
      guide_in = new TGraph(31, gxin, gyin);
      guide_out = new TGraph(31, gxout, gyout);

      tf_eval->cd();
      h_ringtot->Write();
      h_R->Write();
    }
  if(iloop==-1 && i_Eval==1)
    {
      tf_eval->cd();
      h_ringinit->Write();
      h_R_init->Write();
    }
  
  for(int ic=0; ic<4; ic++)
    {
      if(iloop==20) tc_printed[ic]->Divide(6, 4);
      for(int ip=0; ip<24; ip++)
	{
	  if(iloop==20)
	    {
	      tc_printed[ic]->cd(ip+1);
	      h_ring[ic/2][ic%2][ip]->Draw("col");
	      guide_in->Draw("CP");
	      guide_out->Draw("CP");
	      if(i_Eval==1)
		{
		  CalcError(h_ring[ic/2][ic%2][ip], ic/2, ic%2, ip);
		  tf_eval->cd();
		  //	  h_ring[ic / 2][ic % 2][ip]->Write();
		}
	    }
	  else
	    {
	      tg = FindCent(h_ring[ic/2][ic%2][ip]);
	      tg->GetPoint(8, z, phi);
	      if(iloop == -1)
		{
		  f_dz[ic/2][ic%2][ip][iloop+1] = z;
		  if(i_VerifByPhi) 
		    f_dphi[ic / 2][ic % 2][ip][iloop+1] = phi / 263.5; 
		  else f_dphi[ic / 2][ic % 2][ip][iloop+1] = phi; 
		}
	      else if(iloop+1 > -1 && iloop + 1 < 20)
		{
		  f_dz[ic/2][ic%2][ip][iloop+1] = 
		    f_dz[ic / 2][ic % 2][ip][iloop] + z;
		  if(i_VerifByPhi) 
		    f_dphi[ic / 2][ic % 2][ip][iloop+1] = 
		      f_dphi[ic / 2][ic % 2][ip][iloop] + (phi / 263.5); 
		  else f_dphi[ic / 2][ic % 2][ip][iloop+1] = 
			 f_dphi[ic / 2][ic % 2][ip][iloop] + phi;
		}
	      
	      if(i_Eval==1)
		{
		  tf_eval->cd();
		  sprintf(drawn,"g_%s",h_ring[ic / 2][ic % 2][ip]->GetName());
		  //	  tg->Write(drawn);
		  //	  h_ring[ic / 2][ic % 2][ip]->Write();
		} 
	    }	  
	} // panel loop
      
      if(iloop==20 && i_Eval==1) tc_printed[ic]->Print();    
      
    } // Arm + Sector loop
  
  TTree *result=NULL;
  float dz, dphi, errdz, errdphi;
  int ia, is, ip;
  if(iloop == 20 && i_Eval==1)
    {
      result = new TTree("result", "Alignment result");
      result->Branch("arm", &ia, "arm/I");
      result->Branch("side", &is, "side/I");
      result->Branch("panel", &ip, "panel/I");
      result->Branch("dz", &dz, "dz/F");
      result->Branch("dphi", &dphi, "dphi/F");
      result->Branch("errdz", &errdz, "errdz/F");
      result->Branch("errdphi", &errdphi, "errdphi/F");
      
      for(ia=0; ia<2; ia++)
	{
	  for(is=0; is<2; is++)
	    {
	      for(ip=0; ip<24; ip++)
		{
		  dz = f_dzfinal[ia][is][ip]; dphi = f_dphifinal[ia][is][ip];
		  errdz = f_errdz[ia][is][ip]; errdphi = f_errdphi[ia][is][ip];
		  if(i_Eval==1) result->Fill();
		}
	    }
	}
      tf_eval->cd();
      result->Write();
    }
  
  for(int ic=0; ic<4; ic++)
    {
      for(int ip=0; ip<24; ip++)
	{
	  delete h_ring[ic / 2][ic % 2][ip];
	}
    }

  /*
  delete [] v_ref;
  delete [] b_ref;
  delete [] posx;
  delete [] posy;
  delete [] posz;
  delete [] posr;
  delete [] posphi;
  delete [] this_npe;
  */
}

TGraph* RICH_Alignment::FindCent(TH2* hist)
{
  float NBINS = hist->GetNbinsX(); 
  float XMAX = hist->GetXaxis()->GetXmax();
  float XMIN = hist->GetXaxis()->GetXmin();
  float SCAN_W = 2.;
  float binw = (XMAX - XMIN) / (float)NBINS;
  float z[15], phi[15];
  int xmin,xmax;
  Text_t gname[150];

  xmin = (int) ((-SCAN_W -XMIN )/ binw);
  xmax = (int) ((SCAN_W -XMIN )/ binw);

  sprintf(gname, "x%d_%s", 0, hist->GetName());
  TH1D *x = hist->ProjectionX(gname, xmin, xmax);
  x->SetAxisRange(-10., 0);
  z[0] = x->GetMean();
  x->SetAxisRange(0., 10.);
  z[0] += x->GetMean();
  z[0] /=2.;

  xmin = (int)((-XMIN -z[0] -SCAN_W)/ binw);
  xmax = (int)((-XMIN -z[0] +SCAN_W)/ binw);

  sprintf(gname, "phi%d_%s", 0, hist->GetName());
  TH1D *y = hist->ProjectionY(gname, xmin, xmax);
  y->SetAxisRange(-10., 0);
  phi[0] = y->GetMean();
  y->SetAxisRange(0., 10.);
  phi[0] += y->GetMean();
  phi[0] /=2.;
  delete x, y;

  for(int ig=1; ig<10; ig++)
    {
      xmin = (int) ((-SCAN_W -XMIN -phi[ig-1])/ binw);
      xmax = (int) ((SCAN_W -XMIN -phi[ig-1])/ binw);
      
      sprintf(gname,"x%d_%s", ig, hist->GetName());
      x = hist->ProjectionX(gname, xmin, xmax);
      x->SetAxisRange(z[ig-1]-10., z[ig-1]);
      z[ig] = x->GetMean();
      x->SetAxisRange(z[ig-1], z[ig-1]+10.);
      z[ig] += x->GetMean();
      z[ig] /=2.;

      xmin = (int)((-XMIN -z[ig-1] -SCAN_W)/ binw);
      xmax = (int)((-XMIN -z[ig-1] +SCAN_W)/ binw);
      
      sprintf(gname, "phi%d_%s", ig,hist->GetName());
      y = hist->ProjectionY(gname, xmin, xmax);
      y->SetAxisRange(phi[ig-1]-10., phi[ig-1]);
      phi[ig] = y->GetMean();
      y->SetAxisRange(phi[ig-1], phi[ig-1]+10.);
      phi[ig] += y->GetMean();
      phi[ig] /=2.;
      delete x,y;
    }

  TGraph *out = new TGraph(10, z, phi);
  return out;
}

void RICH_Alignment::CalcError(TH2* hist, int arm, int side, int panel)
{
  float NBINS = hist->GetNbinsX(); 
  float XMAX = hist->GetXaxis()->GetXmax();
  float XMIN = hist->GetXaxis()->GetXmin();
  float SCAN_W = 2.;
  float binw = (XMAX - XMIN) / (float)NBINS;
  int xmin, xmax;
  float err;
  float rms,N;
  Text_t gname[150];

  xmin = (int) ((-SCAN_W -XMIN )/ binw);
  xmax = (int) ((SCAN_W -XMIN )/ binw);
  
  sprintf(gname, "zerr_%s", hist->GetName());
  TH1D *x = hist->ProjectionX(gname, xmin, xmax);

  x->SetAxisRange(-10., 0);
  rms = x->GetRMS();
  N = x->Integral();
  if(N>0) err = rms*rms/N;
  else err = 0;

  x->SetAxisRange(0., 10.);
  rms = x->GetRMS();
  N = x->Integral();
  if(N>0) err += rms*rms/N;
  else err += 0;

  f_errdz[arm][side][panel] = sqrt(err)/2.;

  sprintf(gname, "phierr_%s", hist->GetName());
  TH1D *y = hist->ProjectionY(gname, xmin, xmax);

  y->SetAxisRange(-10., 0);
  rms = y->GetRMS();
  N = y->Integral();
  if(N>0) err = rms*rms/N;
  else err = 0;

  y->SetAxisRange(0., 10.);
  rms = y->GetRMS();
  N = y->Integral();
  if(N>0) err += rms*rms/N;
  else err += 0;

  f_errdphi[arm][side][panel] = (sqrt(err)/2.) / 263.5;
}

void RICH_Alignment::Calc(int fixed, int mean)
{
  int ia, is, ip;
  
  memset(f_dzfinal, 0, sizeof(f_dzfinal)); 
  memset(f_dphifinal, 0, sizeof(f_dphifinal));

  for(ia=0; ia<2; ia++)
    {
      for(is=0; is<2; is++)
	{
	  for(ip=0; ip<24; ip++)
	    {
	      for(int i=0; i<mean; i++)
		{
		  f_dzfinal[ia][is][ip] += f_dz[ia][is][ip][fixed + i + 1];
		  f_dphifinal[ia][is][ip] += f_dphi[ia][is][ip][fixed + i + 1];
		}
	      f_dzfinal[ia][is][ip] /= (float) mean;
	      f_dphifinal[ia][is][ip] /= (float) mean;
	      
	      if(!i_VerifByPhi)
		{
		  f_dphifinal[ia][is][ip] /= 263.5;
		}

	      if(i_Verb>1)
		{
		  cout << ia << " " << is << " " << ip << " " 
		       << f_dzfinal[ia][is][ip] << " " 
		       << f_dphifinal[ia][is][ip] << endl;
		}	  
	    }
	}
    }
}

void RICH_Alignment::Write()
{
  cout << "Writing alignment result on " << c_alignmentf << endl;
  ofstream fout(c_alignmentf);
  for(int ia=0; ia<2; ia++)
    {
      for(int is=0; is<2; is++)
	{
	  for(int ip=0; ip<24; ip++)
	    {
	      fout.width(3);
	      fout << ia;
	      fout.width(3);
	      fout << is;
	      fout.width(3);
	      fout << ip;
	      fout.width(20);
	      fout.precision(6);
	      fout << f_dzfinal[ia][is][ip];
	      fout.width(20);
	      fout.precision(6);
	      fout << f_dphifinal[ia][is][ip] << endl;
	    }
	}
    }
  fout.close();
}

void RICH_Alignment::Write(char *align)
{
  cout << "Writing alignment result on " << align << endl;
  ofstream fout(align);
  for(int ia=0; ia<2; ia++)
    {
      for(int is=0; is<2; is++)
	{
	  for(int ip=0; ip<24; ip++)
	    {
	      fout.width(3);
	      fout << ia;
	      fout.width(3);
	      fout << is;
	      fout.width(3);
	      fout << ip;
	      fout.width(20);
	      fout.precision(6);
	      fout << f_dzfinal[ia][is][ip];
	      fout.width(20);
	      fout.precision(6);
	      fout << f_dphifinal[ia][is][ip] << endl;
	    }
	}
    }
  fout.close();
}

void RICH_Alignment::Verify()
{
  ProcessRings(20);
}

void RICH_Alignment::Process(int fixed, int mean)
{
  if(i_Verb>0)
    cout << "Process DST Entries" << endl;
  
  for(int i=-1; i<fixed+mean; i++)
    {
      if(i_Verb>0 )
	cout << "Find ring center. " << i << "th try" << endl;
      ProcessRings(i);
    }
  cout << "Calculate final ring center positions." << endl;
  Calc(fixed, mean);
  Write();

  if(i_Eval>0) Verify();
}

void RICH_Alignment::ImportOutput(char *evalf,char *alignf)
{
  int arm, side, panel;
  float dz, dphi;
  int cnt = 0;

  TFile *evalimp = new TFile(evalf);
  trk = (TTree*)evalimp->Get("trk");
  ifstream fdzdphi(alignf);
  while(!fdzdphi.eof() && cnt<96)
    {
      fdzdphi >> arm >> side >> panel >> dz >> dphi;
      f_dzfinal[arm][side][panel] = dz;
      f_dphifinal[arm][side][panel] = dphi;
      cnt++;
    }
  fdzdphi.close();
}

void RICH_Alignment::Init_alignment_parameters(char *infile)
{
  cout << "initialize the alignment parameters" << endl;

  FILE *fa ;
  if((fa=fopen(infile,"r"))==NULL)
    {
      cout << "alignment parameter file doesn't exist" << endl;
      exit(0);
    }
  
  int Arm, Side, Panel;
  float Dz, Dphi;
  
  for(int il=0; il<96; il++)
    {
      fscanf(fa, "%d %d %d %f %f", &Arm, &Side, &Panel, &Dz, &Dphi);
      f_dzfinal[Arm][Side][Panel] = Dz;
      f_dphifinal[Arm][Side][Panel] = Dphi;
      
      //    f_dzstart[Arm][Side][Panel]=Dz;
      //    f_dphistart[Arm][Side][Panel]=Dphi;
      
      //    f_dzstart[Arm][Side][Panel]=0;
      //    f_dphistart[Arm][Side][Panel]=0;
      
      if(il==0)
	{
	  cout << "Parameter valus" << endl;
	  cout << " Arm  :  Side  : Panel : f_dz :  f_dph: " << endl;
	}
      cout << Arm << "  " << Side << "  " << Panel << "  " 
	   << f_dzfinal[Arm][Side][Panel] << "  " 
	   << f_dphifinal[Arm][Side][Panel] << endl;
    }
}
