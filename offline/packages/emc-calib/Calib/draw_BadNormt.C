//_____________________________________________________________________________
int count(TH2* h)
{
  int n = 0;

  for ( int ix = 1; ix <= h->GetXaxis()->GetNbins(); ++ix ) 
    {
      for ( int iy = 1; iy <= h->GetYaxis()->GetNbins(); ++iy ) 
	{
	  if ( h->GetCellContent(ix,iy) )
	    {
	      ++n;
	    }
	}
    }

  return n;
}

//_____________________________________________________________________________
void drawlines(int nx, int ny)
{
  const int solid=1;
  const int dotdot=3;

  for ( int ix=1; ix < nx; ++ix ) 
    {
      TLine* line = new TLine(ix-0.5,0,ix-0.5,ny-1);
      if ( ix%12== 0 )
        {
          line->SetLineStyle(solid);
        }
      else
        {
          line->SetLineStyle(dotdot);
        }
      line->Draw();
      for ( int iy=1; iy < ny; ++iy ) 
        {
          TLine* line = new TLine(0,iy-0.5,nx-1,iy-0.5);
          if ( iy%12== 0 )
            {
              line->SetLineStyle(solid);
            }
          else
            {
              line->SetLineStyle(dotdot);
            }
          line->Draw();
        }
    }
}

void draw_BadNormt(const char* file="/afs/rhic.bnl.gov/phenix/users/aphecetc/CALIB/Run4/BADNORMT/histos.root")
{
  TGaxis::SetMaxDigits(7);

  TFile* f = new TFile(file);
  
  gStyle->SetPalette(1);
  gStyle->SetOptStat(0);

  const char* sectors[] = { "W0","W1","W2","W3","E2","E3" };
  TCanvas* canvas[7];

  for ( size_t i = 0; i < 6; ++i ) 
    {
      canvas[i] = new TCanvas(sectors[i],sectors[i]);
      TH2* h = (TH2*)(f->Get(sectors[i]));
      int n = count(h);
      cout << sectors[i] << " " << n << endl;
      ostringstream str;
      str << sectors[i] << " : " << n << " bad normt towers";
      h->SetTitle(str.str().c_str());
      h->Draw("COLZ");
      drawlines(72,36);
    }

  canvas[6] = new TCanvas("BadNormtPerRun","BadNormtPerRun");
  canvas[6]->Draw();
  TH2* h = new TH2F("h","Number of bad normt towers",
		    10,122213,123564,
		    10,220,245);

  h->Draw();

  TGraph* g = (TGraph*)(f->Get("NBADS"));

  g->SetMarkerStyle(20);
  g->SetMarkerSize(1.0);
  g->Draw("LP");

  for ( size_t i = 0; i < 7; ++i ) 
    {
      string name = canvas[i]->GetName();
      name += ".eps";
      canvas[i]->SaveAs(name.c_str());
    }
}
