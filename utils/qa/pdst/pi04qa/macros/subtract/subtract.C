{

gROOT->Reset();

Text_t  comment[]        = "All data";
Text_t  directory[]      = "mass";

//Text_t  psfilesuffix[]   = "17_pbsc_60_91";
Text_t  psfilesuffix[]   = "18_pbsc_10_30";
const Text_t  filename_nomix[] = "/phenix/data24/saskia/run4/18/hists/gg_pbsc_midc.root";
const Text_t  filename_mix[]   = "/phenix/data24/saskia/run4/18/hists/ggmix_pbsc_midc.root";

Text_t  look_hist[20];
//int index = 2;

Float_t pt =1.25;

Int_t   nrebin = 1;          // 1st favorite nrebin=6, 2nd nrebin=4
const Float_t norm_region_1_lower = 0.085;  // Nominal 0.085
const Float_t norm_region_1_upper = 0.085; // Nominal 0.095
const Float_t norm_region_2_lower = 0.35;  // Nominal 0.21
const Float_t norm_region_2_upper = 0.5;  // Nominal 0.25
Float_t fit_lower = 0.08;            // Nominal 0.08, 2nd 0.06
Float_t fit_upper = 0.23;            // Nominal 0.23, 2nd 0.23 
Double_t init_params[7] = { 100., 0.138, 0.01, 0., 0.0, 0, 0};
const Float_t ax_range_lower = 0.025;
const Float_t ax_range_upper = 0.600;
const Bool_t  fit_and_subtract = kTRUE;

FILE *pkposwidth, *yields;

Text_t y_title[64];
sprintf( y_title, "yields_%s.txt", psfilesuffix );
yields = fopen(y_title,"w");

Text_t p_title[64];
sprintf( p_title, "peaks_%s.txt", psfilesuffix );
pkposwidth = fopen(p_title,"w");

gStyle->SetCanvasBorderMode(0);
gStyle->SetPadBorderMode(0);
gStyle->SetPadColor(0);
gStyle->SetCanvasColor(0);
gStyle->SetTitleColor(0);
gStyle->SetStatW(0.25);
gStyle->SetStatH(0.18);
gStyle->SetStatX(0.99);
gStyle->SetStatY(0.99);
gStyle->SetStatColor(0);//38
gStyle->SetStatFont(80);
gStyle->SetTitleFont(82);
gStyle->SetStatBorderSize(1);
gStyle->SetTitleBorderSize(0);
gStyle->SetTitleX(0.20);
gStyle->SetTitleY(0.98);
gStyle->SetTitleW(0.6);
gStyle->SetTitleH(0.06);
gStyle->SetLabelFont(80,"x");
gStyle->SetLabelFont(80,"y");
gStyle->SetLabelFont(80,"z");
gStyle->SetLabelSize( 0.05, "X" );
gStyle->SetLabelSize( 0.05, "Y" );
gStyle->SetTitleXSize( 0.045 );
gStyle->SetTitleYSize( 0.045 );
gStyle->SetStatStyle(1001);
gStyle->SetOptFit(1111);
gStyle->SetOptStat(0);

for (index=2; index<14; index++)
{
  sprintf(look_hist,"mass8_%i",index);
Double_t init_params[7] = { 100., 0.14, 0.014, 0., 0.0, 0, 0};

  if (index>2)
    {
      //      fit_lower = 0.04;
      //      fit_upper = 0.25;
      //      init_params[0] = init_params[0]/3.;
      if (index>3) init_params[2] = 0.007;
      if (index>7) 
	{
	  init_params[2] = 0.006;
	  fit_upper = 0.6;
	}
      if (index>=9) init_params[0] = 7.;
      //      if (index==5)
      //	{
      //	  fit_lower = 0.1;
      //	  fit_upper = 0.2;
      //	}
      if (index>5)
	{
	  init_params[1] = 0.137;
	  fit_lower = 0.06;
	  fit_upper = 0.3;
	  if (index>7) nrebin = 2;
	}
    }
 
TFile *f_nomix = new TFile( filename_nomix );
//f_nomix->cd(directory);
TH1D  *h_nomix = (TH1D *)gROOT->FindObject( look_hist );


TFile *f_mix   = new TFile( filename_mix   );
//f_mix->cd(directory);
TH1D  *h_mix   = (TH1D *)gROOT->FindObject( look_hist );

Text_t c_title[64];
sprintf( c_title, "subtract_%s_%s", look_hist, psfilesuffix );
TCanvas *c = new TCanvas(c_title, c_title, 20, 10, 700, 900);
TPad *pad[ 5 ];
  
pad[0] = new TPad("pad0","pad0", 0.20, 0.80, 0.90, 1.00, 0);
pad[1] = new TPad("pad1","pad1", 0.20, 0.60, 0.90, 0.80, 0);
pad[2] = new TPad("pad2","pad2", 0.20, 0.40, 0.90, 0.60, 0);
pad[3] = new TPad("pad3","pad3", 0.20, 0.00, 0.90, 0.40, 0);

pad[0]->Draw();
pad[1]->Draw();
pad[2]->Draw();
pad[3]->Draw();

Int_t norm_region_1_bin_lower =  h_nomix->FindBin( norm_region_1_lower );
Int_t norm_region_1_bin_upper =  h_nomix->FindBin( norm_region_1_upper );

Int_t norm_region_2_bin_lower =  h_nomix->FindBin( norm_region_2_lower );
Int_t norm_region_2_bin_upper =  h_nomix->FindBin( norm_region_2_upper );

printf("Normalize region 1 : Bin %d to %d\n", norm_region_1_bin_lower, norm_region_1_bin_upper);
printf("Normalize region 2 : Bin %d to %d\n", norm_region_2_bin_lower, norm_region_2_bin_upper);

Float_t nent_nomix_by_region = (Float_t)h_nomix->Integral( norm_region_1_bin_lower, norm_region_1_bin_upper )
                             + (Float_t)h_nomix->Integral( norm_region_2_bin_lower, norm_region_2_bin_upper );
Float_t nent_mix_by_region   = (Float_t)h_mix->Integral( norm_region_1_bin_lower, norm_region_1_bin_upper )
                             + (Float_t)h_mix->Integral( norm_region_2_bin_lower, norm_region_2_bin_upper );

printf("Nentries in normalize region for nomix = %f\n", nent_nomix_by_region );
printf("Nentries in normalize region for   mix = %f\n", nent_mix_by_region );

if (nent_mix_by_region > 0 ) 
{
  h_mix->Scale( nent_nomix_by_region / nent_mix_by_region );
}
TH1D *h_sub = new TH1D("sub_pt10", "Subtracted", 120, 0, 0.6);
h_sub->Add( h_nomix, h_mix, 1, -1 );
h_sub->SetTitle( "Subtracted" );

//Treate Error
const Int_t nbins = h_nomix->GetNbinsX();
printf("Number of bin: %d\n", nbins );
Stat_t err_vector[nbins];
for( Int_t i = 0 ; i< nbins ; i++ ) {
  err_vector[i] = h_nomix->GetBinError( i+1 );
  //printf("Bin: %d  Count: %f Err: %f\n", i+1, h_nomix->GetBinContent( i+1 ), err_vector[i] );
}
h_sub->SetError( err_vector );
 init_params[0] = h_sub->GetBinContent((int)(h_sub->GetMaximumBin()));
 printf("max = %f\n",init_params[0]);

//--------------------------

pad[0]->cd();

h_nomix->Rebin( nrebin );
h_nomix->SetAxisRange( ax_range_lower, ax_range_upper );
h_nomix->SetXTitle( "Invariant Mass [GeV/cc]" );
h_nomix->SetLabelFont(80,"y");
h_nomix->SetLabelSize( 0.05, "Y" );
h_nomix->SetLineColor( kRed );
h_nomix->Draw();

h_mix->Rebin( nrebin );
h_mix->SetLineColor( kBlue );
h_mix->Draw("SAME");

//--------------------------

pad[1]->cd();
h_sub->Rebin( nrebin );
h_sub->SetAxisRange( ax_range_lower, ax_range_upper );
h_sub->SetXTitle( "Invariant Mass [GeV/cc]" );
h_sub->Draw("HIST");

//--------------------------

pad[2]->cd();

TH1D *h_sub2 = new TH1D( "sub2", "Subtracted (Spare)", h_sub->GetNbinsX(), 0, 0.6);
if( fit_and_subtract ) {
  h_sub2->Add( h_sub, 1.0);
} else {
  h_sub2->Add( h_nomix, 1.0);
}



h_sub2->SetTitle( "Subtracted and Fit" );
h_sub2->SetXTitle( "Invariant Mass [GeV/cc]" );


// if (index>4)
//  {
   TF1 *fitfunc = new TF1("fitfunc","gaus(0)", fit_lower, fit_upper);
   //   init_params[3] = 0.;
   //  }
   // else
   //  {
   //   TF1 *fitfunc = new TF1("fitfunc","gaus(0)+pol0(3)", fit_lower, fit_upper);
   //  }

fitfunc->SetParameters( init_params ); 
fitfunc->SetParLimits(1,0.12,0.17); 
fitfunc->SetParLimits(2,0.002,0.024); 
 if (index>9) fitfunc->SetParLimits(2,0.002,0.016); 

h_sub2->Fit( "fitfunc", "RNO");
//h_sub2->Fit( "fitfunc", "LMRNO");
fitfunc->GetParameters( init_params ); 

h_sub2->SetAxisRange( ax_range_lower, ax_range_upper );
h_sub2->Draw("AXIS");

fitfunc->SetLineColor( 50 );
fitfunc->Draw("SAME");

TF1 *polyfunc = new TF1("polyfunc", "pol0", fit_lower, fit_upper);
polyfunc->SetParameters( init_params+3 );
polyfunc->SetLineColor( 8 );
polyfunc->Draw("SAME");

h_sub2->Draw("ESAME");

//--------------------------

pad[3]->cd();

Text_t s0[256];
pave = new TPaveText( 0.05, 0.05, 0.95, 0.95);
pave->SetFillColor(0);
pave->SetTextFont(82);
pave->SetTextSize( 0.025 );
pave->SetTextAlign( 10 );
pave->SetBorderSize( 1 );

pave->AddText( comment );

sprintf( s0, "Histogram : %s   \"%s\"", look_hist, "");   pave->AddText(s0);
sprintf( s0, "No-Mixing : %s", filename_nomix); pave->AddText(s0);
sprintf( s0, "Mixing    : %s", filename_mix);   pave->AddText(s0);


sprintf( s0, "Normalization Region 1 : (%1.3f , %1.3f) (%1.3f , %1.3f)",
	 norm_region_1_lower, norm_region_1_upper,
	 norm_region_2_lower, norm_region_2_upper );
pave->AddText(s0);

sprintf( s0, "Fit Region :  (%1.3f , %1.3f)", fit_lower, fit_upper);   pave->AddText(s0);

pave->AddText("Fit Result ...");
sprintf(s0, "         CHI2 / ndf : %f / %f", fitfunc->GetChisquare(), fitfunc->GetNDF() ); pave->AddText(s0);
sprintf(s0, "Parameter 0 (Const) : %f +- %f", init_params[0], fitfunc->GetParError(0) ); pave->AddText(s0);
sprintf(s0, "          1 (Mean)  : %f +- %f", init_params[1], fitfunc->GetParError(1) ); pave->AddText(s0);
sprintf(s0, "          2 (Sigma) : %f +- %f", init_params[2], fitfunc->GetParError(2) ); pave->AddText(s0);
sprintf(s0, "          3 (*1)    : %f +- %f", init_params[3], fitfunc->GetParError(3) ); pave->AddText(s0);
sprintf(s0, "          4 (*x)    : %f +- %f", init_params[4], fitfunc->GetParError(4) ); pave->AddText(s0);
sprintf(s0, "          5 (*xx)   : %f +- %f", init_params[5], fitfunc->GetParError(5) ); pave->AddText(s0);
sprintf(s0, "          6 (*xxx)  : %f +- %f", init_params[6], fitfunc->GetParError(6) ); pave->AddText(s0);

Float_t cons =  init_params[0];
Float_t mean =  init_params[1];
Float_t sigma = init_params[2];
Float_t m_2sigma_l = mean - 2 * sigma;
Float_t m_2sigma_u = mean + 2 * sigma;
Int_t   bin_m_2sigma_l = h_sub->FindBin( m_2sigma_l );
Int_t   bin_m_2sigma_u = h_sub->FindBin( m_2sigma_u );

sprintf(s0, "Two-sigma : (%f, %f)   Bin : (%d, %d)",
	m_2sigma_l, m_2sigma_u,
	bin_m_2sigma_l, bin_m_2sigma_u); pave->AddText(s0);

Float_t npi_by_bincount = 0;
Float_t npi_by_bincount_mpol = 0;
Float_t err_by_bincount = 0;
for( Int_t i = bin_m_2sigma_l ; i<= bin_m_2sigma_u ; i++ ){
  Float_t pos = (Float_t)(h_sub->GetBinCenter(i));
  Float_t cont = (Float_t)(h_sub->GetBinContent(i));
  Float_t err  = ((Float_t)(h_sub->GetBinError(i)));
  npi_by_bincount += cont;
  err_by_bincount += err * err;
  Float_t pol = (    (Float_t)(init_params[3])
		     //		     + (Float_t)(init_params[4])*pos
		     //		     + (Float_t)(init_params[5])*pos*pos
		     //		     + (Float_t)(init_params[6])*pos*pos*pos
		     );
  npi_by_bincount_mpol += cont;// - pol;

}
err_by_bincount = sqrt(err_by_bincount);
sprintf(s0, "Estimated n-pi in Two-sigma bins : %f +- %f", npi_by_bincount, err_by_bincount); pave->AddText(s0);
sprintf(s0, "    with subtraction polynomial  : %f", npi_by_bincount_mpol); pave->AddText(s0);

Float_t bin_density = (Float_t)(h_sub->GetNbinsX());
Float_t npi_by_func = sqrt(2*3.141592)*sigma*cons*bin_density;
Float_t err_by_func = sqrt(2*3.141592)*( cons * (fitfunc->GetParError(2)) + sigma * (fitfunc->GetParError(0)) )*bin_density;

sprintf(s0, "   by only integration gaussian  : %f +- %f", npi_by_func, err_by_func); pave->AddText(s0);

pave->Draw();

Float_t bgd = npi_by_bincount - npi_by_bincount_mpol;

fprintf(yields,"%f %f %f %f\n",pt,npi_by_bincount_mpol,bgd,err_by_bincount);
fprintf(pkposwidth,"%f %f %f %f %f\n",pt,mean,fitfunc->GetParError(1),sigma,fitfunc->GetParError(2));
 pt = pt + 0.5; 

 // c->SaveAs(".gif");

 //  f_nomix.Close();
 //  f_mix.Close();

}
fclose(yields);
fclose(pkposwidth);

}

