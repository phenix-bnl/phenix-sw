//
// root [] .L HbdEventDisplay.C
// root [] HbdEventDisplay( event_number)
//
#include <TCanvas.h>
#include <TMath.h>
#include <TColor.h>
#include <TPolyLine.h>
#include <TLine.h>
#include <TEllipse.h>
#include <TPaveText.h>
#include <iostream.h>
#include <stdlib.h>
#include <TFile.h>
#include <TTree.h>

int  pad_type[192];
float pad_center[192][2];
float pad_charge[12][192];
float alpha;
float xvertex[7],yvertex[7];
int nvertex;
static float r2d = 57.295779513;
static float d2r = 0.017453293;

const Float_t thresh_charge = 1.6; // hit threshold of charge [fC]
const Float_t limit_charge = 64.; // maximum limit of charge [fC]
const Float_t thresh_time = 135.; // threshold of time [ns]

const Short_t color_step = 21; // number of color steps
const Short_t color_max = 100; // maximum number in color palette for color change
const Short_t color_min = color_max - color_step + 1; // minimum number in color palette for color change
const Double_t ofxc = 0.81;  // offset for color display
const Double_t ofyca = 0.051-0.001*(Double_t)color_step; // offset for color display
const Double_t ofyc0 = 0.6; // offset for color display
const Double_t ofyc1 = 0.94; // offset for color display
const Double_t ofyc2 = ofyc0-2.2*(ofyc1-ofyc0)/(Double_t)(color_step-1)-0.05; // offset for color display
const Double_t size_hexagon = 0.012;
const Double_t bar_offset = 0.008; // offset for color bars
const Double_t bar_width = size_hexagon*6.; // offset for color bars
const Double_t bar_height = (ofyc1-ofyc0)/(Double_t)(color_step-1); // offset for color bars
const Double_t cog_size = 0.5; // size of cog circle
const Short_t cog_width = 1; // linewidth of cog circle


TPolyLine *bar_color[color_step]; // bars for color display
TPolyLine *tick_color[color_step]; // ticks for color display
TEllipse *ell[8]; // circle for color and etc. display
TLine *del[9]; // line for suppressed particle of ghit and nanoDST
TPaveText *pt[7]; // TPaveText for information and color, etc. display
TCanvas * c1;
TFile * f;
TTree * t;

void init_color_charge();
void draw_color_charge();
void PadInfo();
void PadVertex(int pad);
void DrawCell(int side,int sect,int pad,float charge);
void change_color();
void create_canvas();

void create_canvas()
{
   c1 = new TCanvas("c1","Event display",200,10,800,800);
   c1->SetFrameFillColor(0);
   c1->SetFillColor(10);
   c1->SetBorderSize(2);
   c1->SetFrameFillColor(0);
   change_color();
   TLatex *   tex = new TLatex(0.155,0.95,"EAST");
   tex->SetLineWidth(2);
   tex->Draw();
   tex = new TLatex(0.55,0.95,"WEST");
   tex->SetLineWidth(2);
   tex->Draw();
   tex = new TLatex(0.38,0.085,"0");
   tex->SetLineWidth(2);
   tex->Draw();
   tex = new TLatex(0.78,0.085,"0");
   tex->SetLineWidth(2);
   tex->Draw();
   tex = new TLatex(0.38,0.225,"1");
   tex->SetLineWidth(2);
   tex->Draw();
   tex = new TLatex(0.78,0.225,"1");
   tex->SetLineWidth(2);
   tex->Draw();
   tex = new TLatex(0.38,0.385,"2");
   tex->SetLineWidth(2);
   tex->Draw();
   tex = new TLatex(0.78,0.385,"2");
   tex->SetLineWidth(2);
   tex->Draw();
   tex = new TLatex(0.38,0.53,"3");
   tex->SetLineWidth(2);
   tex->Draw();
   tex = new TLatex(0.78,0.53,"3");
   tex->SetLineWidth(2);
   tex->Draw();
   tex = new TLatex(0.38,0.685,"4");
   tex->SetLineWidth(2);
   tex->Draw();
   tex = new TLatex(0.78,0.685,"4");
   tex->SetLineWidth(2);
   tex->Draw();
   tex = new TLatex(0.38,0.825,"5");
   tex->SetLineWidth(2);
   tex->Draw();
   tex = new TLatex(0.78,0.825,"5");
   tex->SetLineWidth(2);
   tex->Draw();
   init_color_charge(); 
   draw_color_charge();
}

void HbdEventDisplay(int event=1)
{
   gSystem->Load("libhbd.so");
   gSystem->Load("libemc.so");
 //  gSystem->Load("libCNT.so");
   f = new TFile("simDST.root");
   t = (TTree*) f->Get("T");
   create_canvas();
   int ith_event=event;
   cout<<"Hello world!!"<<endl;


   // prepare HBD Classes and set addresses
   //
   //HbdBlobList *hbl = new HbdBlobListv1();
   //HbdBlob *hb = new HbdBlobv1();
   HbdCellList *hcl = new HbdCellListv1();
   HbdCell *hc = new HbdCellv1();
//   PHCentralTrack *cnt = new PHCentralTrackv20();
   //   HbdGhitList *hgl = new HbdGhitListv1();
   //   HbdGhit *hg = new HbdGhitv1();
   //
   //t->SetBranchAddress("DST/HbdBlobList", &hbl);
   t->SetBranchAddress("DST/HbdCellList", &hcl); // for pad display //
   //   t->SetBranchAddress("DST/HbdGhitList", &hgl); // for ghit display //
//   t->SetBranchAddress("DST/PHCentralTrack", &cnt); //central track info

   PadInfo();

   for(int side=0;side<2;side++)
	 {
		for(int sect=0;sect<6;sect++)
		  {
			 for(int p=0;p<192;p++)
			   {
				  PadVertex(p);
				  for(int l=0;l<nvertex+1;l++)
					{
					   xvertex[l]=(0.25+0.5*side-xvertex[l]*(1./120.))*0.8;
					   yvertex[l]=(0.1+0.15*sect+yvertex[l]*(1./150.))*1.0;
					}

				  TPolyLine * line =new TPolyLine(nvertex+1, xvertex, yvertex,"");
				  line->Draw();
			   }
		  }
	 }

   t->GetEntry(ith_event);
   for(Short_t j=0; j<2304; j++)
	 {
		hc = hcl->get_cell(j);
		Short_t row = hc->get_sector();
		Short_t col = hc->get_padnum();
		Float_t charge = hc->get_charge();
//		Float_t time = hc->get_time();
//		if(time > thresh_time) charge = 0.;
		pad_charge[row][col]=charge;

		int ARM,SECT,PAD;
		if(row<6)
		  {
			 ARM=0;
		  }
		else
		  {
			 ARM=1;
		  }
		SECT=row-6*ARM;

		PAD=col;
		if(charge > thresh_charge)
		  {
			 cout <<" ARM: "<<ARM<<" SECT: "<<SECT<<" PAD: "<<PAD<<" charge: "<<charge<<endl;
		  }

		DrawCell(ARM,SECT,PAD,charge);
	 }
   /*
   int nblobs = hbl->get_nBlobs();
   cout << "number of blobs: " << nblobs << endl;
   for(short j=0; j<nblobs; j++){
      hb = hbl->get_blob(j);
      cout << "charge: " << hb->get_charge() << " ";
      cout << "cells: " << hb->get_size() << " x: " << hb->get_xyzin(0) << " y: ";
      cout << hb->get_xyzin(1) << " z: " << hb->get_xyzin(2);
      cout << " y(local): " << hb->get_xyzPosition(1);
      cout << " z(local): " << hb->get_xyzPosition(2) << endl;
      if(hb->get_sector()<6){
	 ARM=0;
	 SECT=hb->get_sector();
      }else{
	 ARM=1;
	 SECT=hb->get_sector()-6;
      }
      DrawBlob(hb->get_xyzPosition(1), hb->get_xyzPosition(2), ARM, SECT);
      cout << "ARM: " << ARM << " SECT: " << SECT << " #local max " << hb->get_nlocalmax() << endl;
   }
*/
   //hbl->Delete();
   //hb->Delete();
   hcl->Delete();
   hc->Delete();
   //hgl->Delete();
   //hg->Delete();
}

void DrawCell(int side, int sect,int pad,float charge)
{
   PadVertex(pad);
//   cout << pad_center[pad][0] << " " << pad_center[pad][1] << endl;
   /*
   cout << xvertex[0] << " " << yvertex[0] << endl;
   cout << xvertex[1] << " " << yvertex[1] << endl;
   cout << xvertex[2] << " " << yvertex[2] << endl;
   cout << xvertex[3] << " " << yvertex[3] << endl;
   cout << xvertex[4] << " " << yvertex[4] << endl;
   cout << xvertex[5] << " " << yvertex[5] << endl;*/
   for(int l=0;l<nvertex+1;l++)
	 {
		xvertex[l]=(0.25+0.5*side-xvertex[l]*(1./120.))*0.8;
		yvertex[l]=(0.1+0.15*sect+yvertex[l]*(1./150.))*1.0;
	 }

   /*
   cout << xvertex[0] << " " << yvertex[0] << endl;
   cout << xvertex[1] << " " << yvertex[1] << endl;
   cout << xvertex[2] << " " << yvertex[2] << endl;
   cout << xvertex[3] << " " << yvertex[3] << endl;
   cout << xvertex[4] << " " << yvertex[4] << endl;
   cout << xvertex[5] << " " << yvertex[5] << endl;*/
   Short_t color = (Short_t) ((charge-thresh_charge)/(limit_charge-thresh_charge) * (Float_t)(color_step-2)+1.);
   if(color>color_step-1) color = color_step-1;

   TPolyLine * polyLine =new TPolyLine(nvertex+1, xvertex, yvertex,"F");
   //TPolyLine *polyLine = new TPolyLine(n, x, y, "F");
   polyLine->SetLineColor(4);
   //cout<<color_min+color<<endl;
   polyLine->SetFillColor(color_min+color);
   polyLine->SetFillStyle(1001);
   polyLine->Draw("f");
   TPolyLine * polyLine1 =new TPolyLine(nvertex+1, xvertex, yvertex,"");
   polyLine1->SetLineColor(kBlack);
   polyLine1->Draw();
}

void DrawBlob(float y, float z, int side, int sect)
{
   y=(0.1 + 0.15*sect + y*(1.0/150.0))*1.0;
   z=(0.25 + 0.5*side - z*(1.0/120.0))*0.8;
   float yblob[5], zblob[5];
   yblob[0]=y-0.015;
   yblob[1]=y+0.015;
   yblob[2]=y+0.015;
   yblob[3]=y-0.015;
   yblob[4]=y-0.015;
   zblob[0]=z-0.015;
   zblob[1]=z-0.015;
   zblob[2]=z+0.015;
   zblob[3]=z+0.015;
   zblob[4]=z-0.015;
   TPolyLine *pL = new TPolyLine(5,zblob,yblob);
   pL->SetFillColor(1);
   pL->SetLineWidth(3.6);
   pL->SetLineColor(2);
   pL->SetFillStyle(4001);
   pL->Draw();
}
void change_color()
{
   for(Short_t i=color_min; i<=color_max; i++)
	 {
		Float_t x = (Float_t)(color_max-i)/(Float_t)(color_max-color_min)*4.7;
		Float_t r, g, b;
		Float_t pow = 1.5;
		if(x>=0. && x<1.)
		  {
			 r = 1.;                         g = 1.-TMath::Power(1.-x, pow); b = 0.;
		  }

		else if(x>=1. && x<2.)
		  {
			 r = 1.-TMath::Power(x-1., pow); g = 1.;                         b = 0.;
		  }

		else if(x>=2. && x<3.)
		  {
			 r = 0.;                         g = 1.;                         b = 1.-TMath::Power(3.-x, pow);
		  }

		else if(x>=3. && x<4.)
		  {
			 r = 0.;                         g = 1.-TMath::Power(x-3., pow); b = 1.;
		  }

		else if(x>=4. && x<=5.)
		  {
			 r = 1.-TMath::Power(5.-x, pow); g = 0.;                         b = 1.;
		  }

		cl = (TColor*) (gROOT->GetListOfColors()->At(i));
		cl->SetRGB(r, g, b);
		if(i==color_min) cl->SetRGB(0.9, 0.9, 0.9); // non hit pad set to be gray
	 }
}

void PadVertex(int pad)
{
   float x0, y0;
   x0 = pad_center[pad][0];
   y0 = pad_center[pad][1];
   //cout<<"x0 -> "<<x0<<" y0 -> "<<y0<<endl;

   if(pad_type[pad]==0)
	 {
		nvertex=6;
		for(int p=0;p<6;p++)
		  {
			 alpha=p*60.*d2r;
			 xvertex[p]=x0+1.55*TMath::Cos(alpha);
			 yvertex[p]=y0+1.55*TMath::Sin(alpha);
		  }
		xvertex[nvertex]=xvertex[0];
		yvertex[nvertex]=yvertex[0];
	 }

   if(pad_type[pad]==11)
	 {
		nvertex=4;
		alpha=0.0*60.*d2r;
		xvertex[0]=x0+1.55*TMath::Cos(alpha);
		yvertex[0]=y0+0.59+1.55*TMath::Sin(alpha);
		alpha=3.0*60.*d2r;
		xvertex[1]=x0+1.55*TMath::Cos(alpha);
		yvertex[1]=y0+0.59+1.55*TMath::Sin(alpha);
		alpha=4.0*60.*d2r;
		xvertex[2]=x0+1.55*TMath::Cos(alpha);
		yvertex[2]=y0+0.59+1.55*TMath::Sin(alpha);
		alpha=5.0*60.*d2r;
		xvertex[3]=x0+1.55*TMath::Cos(alpha);
		yvertex[3]=y0+0.59+1.55*TMath::Sin(alpha);
		xvertex[nvertex]=xvertex[0];
		yvertex[nvertex]=yvertex[0];
	 }

   if(pad_type[pad]==12)
	 {
		nvertex=4;
		alpha=0.0*60.*d2r;
		xvertex[0]=x0+1.55*TMath::Cos(alpha);
		yvertex[0]=y0-0.59+1.55*TMath::Sin(alpha);
		alpha=1.0*60.*d2r;
		xvertex[1]=x0+1.55*TMath::Cos(alpha);
		yvertex[1]=y0-0.59+1.55*TMath::Sin(alpha);
		alpha=2.0*60.*d2r;
		xvertex[2]=x0+1.55*TMath::Cos(alpha);
		yvertex[2]=y0-0.59+1.55*TMath::Sin(alpha);
		alpha=3.0*60.*d2r;
		xvertex[3]=x0+1.55*TMath::Cos(alpha);
		yvertex[3]=y0-0.59+1.55*TMath::Sin(alpha);
		xvertex[nvertex]=xvertex[0];
		yvertex[nvertex]=yvertex[0];
	 }

   if(pad_type[pad]==31)
	 {
		nvertex=5;
		xvertex[0]=x0+1.1869;
		yvertex[0]=y0+1.34125;
		xvertex[1]=x0-0.778;
		yvertex[1]=y0+1.34125;
		xvertex[2]=x0-1.5528;
		yvertex[2]=y0;
		xvertex[3]=x0-0.778;
		yvertex[3]=y0-1.34125;
		xvertex[4]=x0+1.1869;
		yvertex[4]=y0-1.34125;
		xvertex[nvertex]=xvertex[0];
		yvertex[nvertex]=yvertex[0];
	 }

   if(pad_type[pad]==32)
	 {
		nvertex=5;
		xvertex[0]=x0+1.125;
		yvertex[0]=y0+1.34125;
		xvertex[1]=x0-0.715;
		yvertex[1]=y0+1.34125;
		xvertex[2]=x0-1.489;
		yvertex[2]=y0;
		xvertex[3]=x0-0.715;
		yvertex[3]=y0-1.34125;
		xvertex[4]=x0+1.125;
		yvertex[4]=y0-1.34125;
		xvertex[nvertex]=xvertex[0];
		yvertex[nvertex]=yvertex[0];
	 }

   if(pad_type[pad]==33)
	 {
		nvertex=5;
		xvertex[0]=x0+1.489;
		yvertex[0]=y0;
		xvertex[1]=x0+0.715;
		yvertex[1]=y0+1.34125;
		xvertex[2]=x0-1.125;
		yvertex[2]=y0+1.34125;
		xvertex[3]=x0-1.125;
		yvertex[3]=y0-1.34125;
		xvertex[4]=x0+0.715;
		yvertex[4]=y0-1.34125;
		xvertex[nvertex]=xvertex[0];
		yvertex[nvertex]=yvertex[0];
	 }

   if(pad_type[pad]==34)
	 {
		nvertex=5;
		xvertex[0]=x0+1.55278;
		yvertex[0]=y0;
		xvertex[1]=x0+0.778;
		yvertex[1]=y0+1.34125;
		xvertex[2]=x0-1.1869;
		yvertex[2]=y0+1.34125;
		xvertex[3]=x0-1.1869;
		yvertex[3]=y0-1.34125;
		xvertex[4]=x0+0.778;
		yvertex[4]=y0-1.34125;
		xvertex[nvertex]=xvertex[0];
		yvertex[nvertex]=yvertex[0];
	 }

   if(pad_type[pad]==61)
	 {
		nvertex=4;
		xvertex[0]=x0+1.47837;
		yvertex[0]=y0+0.6656;
		xvertex[1]=x0-1.125;
		yvertex[1]=y0+0.6656;
		xvertex[2]=x0-1.125;
		yvertex[2]=y0-0.6656;
		xvertex[3]=x0+0.7151;
		yvertex[3]=y0-0.6656;
		xvertex[nvertex]=xvertex[0];
		yvertex[nvertex]=yvertex[0];
	 }

   if(pad_type[pad]==62)
	 {
		nvertex=4;
		xvertex[0]=x0+1.47837;
		yvertex[0]=y0-0.6656;
		xvertex[1]=x0-1.125;
		yvertex[1]=y0-0.6656;
		xvertex[2]=x0-1.125;
		yvertex[2]=y0+0.6656;
		xvertex[3]=x0+0.7151;
		yvertex[3]=y0+0.6656;
		xvertex[nvertex]=xvertex[0];
		yvertex[nvertex]=yvertex[0];
	 }

   if(pad_type[pad]==63)
	 {
		nvertex=4;
		xvertex[0]=x0+1.125;
		yvertex[0]=y0+0.6656;
		xvertex[1]=x0-1.47837;
		yvertex[1]=y0+0.6656;
		xvertex[2]=x0-0.7151;
		yvertex[2]=y0-0.6656;
		xvertex[3]=x0+1.125;
		yvertex[3]=y0-0.6656;
		xvertex[nvertex]=xvertex[0];
		yvertex[nvertex]=yvertex[0];
	 }

   if(pad_type[pad]==64)
	 {
		nvertex=4;
		xvertex[0]=x0+1.125;
		yvertex[0]=y0-0.6656;
		xvertex[1]=x0-1.47837;
		yvertex[1]=y0-0.6656;
		xvertex[2]=x0-0.7151;
		yvertex[2]=y0+0.6656;
		xvertex[3]=x0+1.125;
		yvertex[3]=y0+0.6656;
		xvertex[nvertex]=xvertex[0];
		yvertex[nvertex]=yvertex[0];
	 }

   if(pad_type[pad]==51)
	 {
		nvertex=0;
	 }

   if(pad_type[pad]==52)
	 {
		nvertex=0;
	 }
}

void PadInfo()
{
   for(int i=0;i<192;i++)
	 {
		pad_type[i]=0;
	 }

   pad_type[19]=12;  pad_type[36]=12;  pad_type[53]=12;
   pad_type[70]=12;  pad_type[104]=12; pad_type[121]=12;
   pad_type[138]=12; pad_type[155]=12; pad_type[172]=12;

   pad_type[27]=11;  pad_type[44]=11;  pad_type[61]=11;
   pad_type[78]=11;  pad_type[112]=11; pad_type[129]=11;
   pad_type[146]=11; pad_type[163]=11; pad_type[180]=11;

   pad_type[181]=31; pad_type[182]=31; pad_type[183]=31;
   pad_type[184]=31; pad_type[185]=31; pad_type[186]=31;
   pad_type[187]=31; pad_type[188]=31; pad_type[88]=32;
   pad_type[89]=32;  pad_type[90]=32;  pad_type[91]=32;
   pad_type[92]=32;  pad_type[93]=32;  pad_type[94]=32;

   pad_type[96]=33;  pad_type[97]=33;  pad_type[98]=33;
   pad_type[99]=33;  pad_type[100]=33; pad_type[101]=33;
   pad_type[102]=33; pad_type[103]=33;

   pad_type[3]=34;
   pad_type[4]=34;   pad_type[5]=34;   pad_type[6]=34;
   pad_type[7]=34;   pad_type[8]=34;   pad_type[9]=34;

   pad_type[2]=62;   pad_type[10]=61;  pad_type[87]=64;
   pad_type[95]=63;  pad_type[0]=52;   pad_type[1]=52;
   pad_type[189]=51; pad_type[190]=51; pad_type[191]=51;

   pad_center[0][1]=-5.445;    pad_center[0][0]=-26.532;
   pad_center[1][1]=5.445;     pad_center[1][0]=-26.532;
   pad_center[2][1]=-10.171;   pad_center[2][0]=-25.000;
   pad_center[3][1]=-8.108;    pad_center[3][0]=-25.000;
   pad_center[4][1]=-5.405;    pad_center[4][0]=-25.000;
   pad_center[5][1]=-2.703;    pad_center[5][0]=-25.000;
   pad_center[6][1]=0;         pad_center[6][0]=-25.000;
   pad_center[7][1]=2.703;     pad_center[7][0]=-25.000;
   pad_center[8][1]=5.405;     pad_center[8][0]=-25.000;
   pad_center[9][1]=8.108;     pad_center[9][0]=-25.000;
   pad_center[10][1]=10.171;   pad_center[10][0]=-25.000;
   pad_center[11][1]=-9.459;   pad_center[11][0]=-22.655;
   pad_center[12][1]=-6.756;   pad_center[12][0]=-22.655;
   pad_center[13][1]=-4.054;   pad_center[13][0]=-22.655;
   pad_center[14][1]=-1.351;   pad_center[14][0]=-22.655;
   pad_center[15][1]=1.351;    pad_center[15][0]=-22.655;
   pad_center[16][1]=4.054;    pad_center[16][0]=-22.655;
   pad_center[17][1]=6.756;    pad_center[17][0]=-22.655;
   pad_center[18][1]=9.459;    pad_center[18][0]=-22.655;
   pad_center[19][1]=-10.208;  pad_center[19][0]=-20.314;
   pad_center[20][1]=-8.108;   pad_center[20][0]=-20.314;
   pad_center[21][1]=-5.405;   pad_center[21][0]=-20.314;
   pad_center[22][1]=-2.703;   pad_center[22][0]=-20.314;
   pad_center[23][1]=0;        pad_center[23][0]=-20.314;
   pad_center[24][1]=2.703;    pad_center[24][0]=-20.314;
   pad_center[25][1]=5.405;    pad_center[25][0]=-20.314;
   pad_center[26][1]=8.108;    pad_center[26][0]=-20.314;
   pad_center[27][1]=10.208;   pad_center[27][0]=-20.314;
   pad_center[28][1]=-9.459;   pad_center[28][0]=-17.974;
   pad_center[29][1]=-6.756;   pad_center[29][0]=-17.974;
   pad_center[30][1]=-4.054;   pad_center[30][0]=-17.974;
   pad_center[31][1]=-1.351;   pad_center[31][0]=-17.974;
   pad_center[32][1]=1.351;    pad_center[32][0]=-17.974;
   pad_center[33][1]=4.054;    pad_center[33][0]=-17.974;
   pad_center[34][1]=6.756;    pad_center[34][0]=-17.974;
   pad_center[35][1]=9.459;    pad_center[35][0]=-17.974;
   pad_center[36][1]=-10.208;  pad_center[36][0]=-15.633;
   pad_center[37][1]=-8.108;   pad_center[37][0]=-15.633;
   pad_center[38][1]=-5.405;   pad_center[38][0]=-15.633;
   pad_center[39][1]=-2.703;   pad_center[39][0]=-15.633;
   pad_center[40][1]=0;        pad_center[40][0]=-15.633;
   pad_center[41][1]=2.703;    pad_center[41][0]=-15.633;
   pad_center[42][1]=5.405;    pad_center[42][0]=-15.633;
   pad_center[43][1]=8.108;    pad_center[43][0]=-15.633;
   pad_center[44][1]=10.208;   pad_center[44][0]=-15.633;
   pad_center[45][1]=-9.459;   pad_center[45][0]=-13.293;
   pad_center[46][1]=-6.756;   pad_center[46][0]=-13.293;
   pad_center[47][1]=-4.054;   pad_center[47][0]=-13.293;
   pad_center[48][1]=-1.351;   pad_center[48][0]=-13.293;
   pad_center[49][1]=1.351;    pad_center[49][0]=-13.293;
   pad_center[50][1]=4.054;    pad_center[50][0]=-13.293;
   pad_center[51][1]=6.756;    pad_center[51][0]=-13.293;
   pad_center[52][1]=9.459;    pad_center[52][0]=-13.293;
   pad_center[53][1]=-10.208;  pad_center[53][0]=-10.952;
   pad_center[54][1]=-8.108;   pad_center[54][0]=-10.952;
   pad_center[55][1]=-5.405;   pad_center[55][0]=-10.952;
   pad_center[56][1]=-2.703;   pad_center[56][0]=-10.952;
   pad_center[57][1]=0;        pad_center[57][0]=-10.952;
   pad_center[58][1]=2.703;    pad_center[58][0]=-10.952;
   pad_center[59][1]=5.405;    pad_center[59][0]=-10.952;
   pad_center[60][1]=8.108;    pad_center[60][0]=-10.952;
   pad_center[61][1]=10.208;   pad_center[61][0]=-10.952;
   pad_center[62][1]=-9.459;   pad_center[62][0]=-8.612;
   pad_center[63][1]=-6.756;   pad_center[63][0]=-8.612;
   pad_center[64][1]=-4.054;   pad_center[64][0]=-8.612;
   pad_center[65][1]=-1.351;   pad_center[65][0]=-8.612;
   pad_center[66][1]=1.351;    pad_center[66][0]=-8.612;
   pad_center[67][1]=4.054;    pad_center[67][0]=-8.612;
   pad_center[68][1]=6.756;    pad_center[68][0]=-8.612;
   pad_center[69][1]=9.459;    pad_center[69][0]=-8.612;
   pad_center[70][1]=-10.208;  pad_center[70][0]=-6.272;
   pad_center[71][1]=-8.108;   pad_center[71][0]=-6.272;
   pad_center[72][1]=-5.405;   pad_center[72][0]=-6.272;
   pad_center[73][1]=-2.703;   pad_center[73][0]=-6.272;
   pad_center[74][1]=0;        pad_center[74][0]=-6.272;
   pad_center[75][1]=2.703;    pad_center[75][0]=-6.272;
   pad_center[76][1]=5.405;    pad_center[76][0]=-6.272;
   pad_center[77][1]=8.108;    pad_center[77][0]=-6.272;
   pad_center[78][1]=10.208;   pad_center[78][0]=-6.272;
   pad_center[79][1]=-9.459;   pad_center[79][0]=-3.931;
   pad_center[80][1]=-6.756;   pad_center[80][0]=-3.931;
   pad_center[81][1]=-4.054;   pad_center[81][0]=-3.931;
   pad_center[82][1]=-1.351;   pad_center[82][0]=-3.931;
   pad_center[83][1]=1.351;    pad_center[83][0]=-3.931;
   pad_center[84][1]=4.054;    pad_center[84][0]=-3.931;
   pad_center[85][1]=6.756;    pad_center[85][0]=-3.931;
   pad_center[86][1]=9.459;    pad_center[86][0]=-3.931;
   pad_center[87][1]=-10.208;  pad_center[87][0]=-1.65;
   pad_center[88][1]=-8.108;   pad_center[88][0]=-1.65;
   pad_center[89][1]=-5.405;   pad_center[89][0]=-1.65;
   pad_center[90][1]=-2.703;   pad_center[90][0]=-1.65;
   pad_center[91][1]=0;        pad_center[91][0]=-1.65;
   pad_center[92][1]=2.703;    pad_center[92][0]=-1.65;
   pad_center[93][1]=5.405;    pad_center[93][0]=-1.65;
   pad_center[94][1]=8.108;    pad_center[94][0]=-1.65;
   pad_center[95][1]=10.208;   pad_center[95][0]=-1.65;
   pad_center[96][1]=-9.45;    pad_center[96][0]=1.65;
   pad_center[97][1]=-6.756;   pad_center[97][0]=1.65;
   pad_center[98][1]=-4.054;   pad_center[98][0]=1.65;
   pad_center[99][1]=-1.351;   pad_center[99][0]=1.65;
   pad_center[100][1]=1.351;   pad_center[100][0]=1.65;
   pad_center[101][1]=4.054;   pad_center[101][0]=1.65;
   pad_center[102][1]=6.756;   pad_center[102][0]=1.65;
   pad_center[103][1]=9.45;    pad_center[103][0]=1.65;
   pad_center[104][1]=-10.208; pad_center[104][0]=3.931;
   pad_center[105][1]=-8.108;  pad_center[105][0]=3.931;
   pad_center[106][1]=-5.405;  pad_center[106][0]=3.931;
   pad_center[107][1]=-2.703;  pad_center[107][0]=3.931;
   pad_center[108][1]=0;       pad_center[108][0]=3.931;
   pad_center[109][1]=2.703;   pad_center[109][0]=3.931;
   pad_center[110][1]=5.405;   pad_center[110][0]=3.931;
   pad_center[111][1]=8.108;   pad_center[111][0]=3.931;
   pad_center[112][1]=10.208;  pad_center[112][0]=3.931;
   pad_center[113][1]=-9.459;  pad_center[113][0]=6.271;
   pad_center[114][1]=-6.756;  pad_center[114][0]=6.271;
   pad_center[115][1]=-4.054;  pad_center[115][0]=6.271;
   pad_center[116][1]=-1.351;  pad_center[116][0]=6.271;
   pad_center[117][1]=1.351;   pad_center[117][0]=6.271;
   pad_center[118][1]=4.054;   pad_center[118][0]=6.271;
   pad_center[119][1]=6.756;   pad_center[119][0]=6.271;
   pad_center[120][1]=9.459;   pad_center[120][0]=6.271;
   pad_center[121][1]=-10.208; pad_center[121][0]=8.612;
   pad_center[122][1]=-8.108;  pad_center[122][0]=8.612;
   pad_center[123][1]=-5.405;  pad_center[123][0]=8.612;
   pad_center[124][1]=-2.703;  pad_center[124][0]=8.612;
   pad_center[125][1]=0;       pad_center[125][0]=8.612;
   pad_center[126][1]=2.703;   pad_center[126][0]=8.612;
   pad_center[127][1]=5.405;   pad_center[127][0]=8.612;
   pad_center[128][1]=8.108;   pad_center[128][0]=8.612;
   pad_center[129][1]=10.208;  pad_center[129][0]=8.612;
   pad_center[130][1]=-9.459;  pad_center[130][0]=10.952;
   pad_center[131][1]=-6.756;  pad_center[131][0]=10.952;
   pad_center[132][1]=-4.054;  pad_center[132][0]=10.952;
   pad_center[133][1]=-1.351;  pad_center[133][0]=10.952;
   pad_center[134][1]=1.351;   pad_center[134][0]=10.952;
   pad_center[135][1]=4.054;   pad_center[135][0]=10.952;
   pad_center[136][1]=6.756;   pad_center[136][0]=10.952;
   pad_center[137][1]=9.459;   pad_center[137][0]=10.952;
   pad_center[138][1]=-10.208; pad_center[138][0]=13.293;
   pad_center[139][1]=-8.108;  pad_center[139][0]=13.293;
   pad_center[140][1]=-5.405;  pad_center[140][0]=13.293;
   pad_center[141][1]=-2.703;  pad_center[141][0]=13.293;
   pad_center[142][1]=0;       pad_center[142][0]=13.293;
   pad_center[143][1]=2.703;   pad_center[143][0]=13.293;
   pad_center[144][1]=5.405;   pad_center[144][0]=13.293;
   pad_center[145][1]=8.108;   pad_center[145][0]=13.293;
   pad_center[146][1]=10.208;  pad_center[146][0]=13.293;
   pad_center[147][1]=-9.459;  pad_center[147][0]=15.633;
   pad_center[148][1]=-6.756;  pad_center[148][0]=15.633;
   pad_center[149][1]=-4.054;  pad_center[149][0]=15.633;
   pad_center[150][1]=-1.351;  pad_center[150][0]=15.633;
   pad_center[151][1]=1.351;   pad_center[151][0]=15.633;
   pad_center[152][1]=4.054;   pad_center[152][0]=15.633;
   pad_center[153][1]=6.756;   pad_center[153][0]=15.633;
   pad_center[154][1]=9.459;   pad_center[154][0]=15.633;
   pad_center[155][1]=-10.208; pad_center[155][0]=17.974;
   pad_center[156][1]=-8.108;  pad_center[156][0]=17.974;
   pad_center[157][1]=-5.405;  pad_center[157][0]=17.974;
   pad_center[158][1]=-2.703;  pad_center[158][0]=17.974;
   pad_center[159][1]=0;       pad_center[159][0]=17.974;
   pad_center[160][1]=2.703;   pad_center[160][0]=17.974;
   pad_center[161][1]=5.405;   pad_center[161][0]=17.974;
   pad_center[162][1]=8.108;   pad_center[162][0]=17.974;
   pad_center[163][1]=10.208;  pad_center[163][0]=17.974;
   pad_center[164][1]=-9.459;  pad_center[164][0]=20.314;
   pad_center[165][1]=-6.756;  pad_center[165][0]=20.314;
   pad_center[166][1]=-4.054;  pad_center[166][0]=20.314;
   pad_center[167][1]=-1.351;  pad_center[167][0]=20.314;
   pad_center[168][1]=1.351;   pad_center[168][0]=20.314;
   pad_center[169][1]=4.054;   pad_center[169][0]=20.314;
   pad_center[170][1]=6.756;   pad_center[170][0]=20.314;
   pad_center[171][1]=9.459;   pad_center[171][0]=20.314;
   pad_center[172][1]=-10.208; pad_center[172][0]=22.655;
   pad_center[173][1]=-8.108;  pad_center[173][0]=22.655;
   pad_center[174][1]=-5.405;  pad_center[174][0]=22.655;
   pad_center[175][1]=-2.703;  pad_center[175][0]=22.655;
   pad_center[176][1]=0;       pad_center[176][0]=22.655;
   pad_center[177][1]=2.703;   pad_center[177][0]=22.655;
   pad_center[178][1]=5.405;   pad_center[178][0]=22.655;
   pad_center[179][1]=8.108;   pad_center[179][0]=22.655;
   pad_center[180][1]=10.208;  pad_center[180][0]=22.655;
   pad_center[181][1]=-9.459;  pad_center[181][0]=24.999;
   pad_center[182][1]=-6.756;  pad_center[182][0]=24.999;
   pad_center[183][1]=-4.054;  pad_center[183][0]=24.999;
   pad_center[184][1]=-1.351;  pad_center[184][0]=24.999;
   pad_center[185][1]=1.351;   pad_center[185][0]=24.999;
   pad_center[186][1]=4.054;   pad_center[186][0]=24.999;
   pad_center[187][1]=6.756;   pad_center[187][0]=24.999;
   pad_center[188][1]=9.459;   pad_center[188][0]=24.999;
   pad_center[189][1]=-7.176;  pad_center[189][0]=26.534;
   pad_center[190][1]=0;       pad_center[190][0]=26.534;
   pad_center[191][1]=7.176;   pad_center[191][0]=26.534;
}

void init_color_charge()
{
   Double_t cy;
   cy = ofyc0 - ofyca;

   Double_t tmpx[5];
   Double_t tmpy[5];
   Double_t tickx[2];
   Double_t ticky[2];

   tmpx[0] = tmpx[3] =tmpx[4] = ofxc + bar_offset;
   tmpx[1] = tmpx[2] = tickx[0] = ofxc + bar_width + bar_offset;
   tickx[1] = tickx[0]+bar_width*0.2;

   tmpy[0] = tmpy[1] =tmpy[4] = cy;
   tmpy[2] = tmpy[3] = cy + bar_height*(Double_t)color_step;

   tick_color[0] = new TPolyLine(5, tmpx, tmpy);
   tick_color[0]->SetLineColor(1);

   for(Int_t i=0; i<color_step; i++)
	 {

		tmpy[0] = tmpy[1] =tmpy[4] = ticky[0] = ticky[1] = cy;
		tmpy[2] = tmpy[3] = cy + bar_height;

		bar_color[i] = new TPolyLine(5, tmpx, tmpy, "F");
		bar_color[i]->SetFillStyle(1001);
		bar_color[i]->SetFillColor(i+color_min);

		if(i!=0)
		  {

			 tick_color[i] = new TPolyLine(2, tickx, ticky);
			 tick_color[i]->SetLineColor(1);
		  }

		cy += bar_height;
	 }

   ell[0] = new TEllipse(ofxc, ofyc2+0.035, size_hexagon*cog_size);
   ell[0]->SetLineWidth(cog_width);
   ell[0]->SetLineColor(1);
   ell[0]->SetFillColor(10);
   ell[0]->SetFillStyle(1001);
}

void draw_color_charge()
{
   //pt[3]->Draw();

   TText *txt = new TText();
   Char_t text[30];

   Double_t cx;
   Double_t cy;

   cx = ofxc + bar_offset + bar_width;
   cy = ofyc0 - ofyca;

   for(Int_t i=0; i<color_step; i++)
	 {

		bar_color[i]->Draw();
		if(i!=0)
		  {

			 txt->SetTextSize(0.025*TMath::Sqrt(16./(Float_t)(color_step+5)));
			 sprintf(text, " %4.1f", (limit_charge-thresh_charge)*(Float_t)(i-1)/(Float_t)(color_step-2)+thresh_charge);
			 txt->DrawText(cx+bar_width*0.2+0.005, cy-0.008, text);
			 tick_color[i]->Draw();
		  }

		cy += (ofyc1-ofyc0)/(Double_t)(color_step-1);
	 }

   tick_color[0]->Draw();

   cx = ofxc;
   txt->SetTextSize(0.025);
   sprintf(text, "Charge [fC]");
   txt->DrawText(cx+0.01, ofyc1-ofyca+(ofyc1-ofyc0)/(Double_t)(color_step-1)+0.01, text);
   /*
   txt->SetTextSize(0.02);
   sprintf(text, "Center of Gravity");
   txt->DrawText(cx+0.02, ofyc2+0.035-0.01, text);
   txt->Delete();
   */
   //ell[0]->Draw();
}



