#include "TFile.h"
#include "TGraphErrors.h"
#include "TCanvas.h"
#include "TH2.h"
#include "TLine.h"
#include <vector>
#include "TStyle.h"
#include "TText.h"
#include "TGaxis.h"
#include "TROOT.h"
#include <iostream>

using namespace std;

//double xshift=-122213;
double xshift=0;

void drawSetLimits(double ymin, double ymax)
{
  vector<int> runs;

  int firstset=55;

  runs.push_back(122213); // 55
  runs.push_back(122223);
  runs.push_back(122466); // 56
  runs.push_back(122596);
  runs.push_back(122597); // 57
  runs.push_back(122732);
  runs.push_back(122735); // 58
  runs.push_back(122850);
  runs.push_back(122851); // 59
  runs.push_back(123037);
  runs.push_back(123039); // 60
  runs.push_back(123224);
  runs.push_back(123227); // 61
  runs.push_back(123363);
  runs.push_back(123367); // 62
  runs.push_back(123487);
  runs.push_back(123489); // 63
  runs.push_back(123564);

  char set[80];

  for ( size_t i = 0; i < runs.size(); ++i )
    {
      TLine* l = new TLine(runs[i]+xshift,ymin,runs[i]+xshift,ymax);
      l->SetLineStyle(2);
      l->SetLineColor(4);
      l->Draw();
      sprintf(set,"S%d",firstset+i/2);
      if ( i % 2 == 0 )
	{
	  TText* t = new TText(runs[i]+xshift,ymax*0.8,set);
	  t->SetTextAngle(45);
	  t->SetTextSize(0.03);
	  t->Draw();
	}
    }
}

void drawRunLimits(double ymin, double ymax)
{
  vector<int> runs;

  // Run4 boudaries from T.Awes Run4 Summaries

  runs.push_back(106935); // Run4 Au+Au 200 GeV
  runs.push_back(122223);

  runs.push_back(122466); // Run4 Au+Au 62.4 GeV
  runs.push_back(123564); 

  // Run5 boundaries from goodruns_pg.php
  // connected to database daq on phnxdb1
  // select distinct run.runnumber,run.brtimestamp,run.runstate,run.brunixtime,run.erunixtime,run.updateunixtime,run.triggerconfig,run.triggerversion,run.eventsinrun,trigger.name,trigger.scalerberraw,trigger.scalerberlive,trigger.scalerberscaled,trigger.scalerupdateraw,trigger.scalerupdatelive,trigger.scalerupdatescaled from run, trigger where run.partitionname='Big' and run.triggerconfig like 'BigRun5' and run.runtype='PHYSICS' and run.loggingon!='NO' and run.eventsinrun>10000 and run.runnumber>149538 and run.runnumber<160488 and run.runnumber=trigger.runnumber and (trigger.name='ZDCNS' or trigger.name='BBCLL1' or trigger.name='BBCLL1(>0 tubes)')
  
  runs.push_back(149539); // Run5 Cu+Cu 200 GeV
  runs.push_back(160487);

  // connected to database daq on phnxdb1
  // select distinct run.runnumber,run.brtimestamp,run.runstate,run.brunixtime,run.erunixtime,run.updateunixtime,run.triggerconfig,run.triggerversion,run.eventsinrun,trigger.name,trigger.scalerberraw,trigger.scalerberlive,trigger.scalerberscaled,trigger.scalerupdateraw,trigger.scalerupdatelive,trigger.scalerupdatescaled from run, trigger where run.partitionname='Big' and run.triggerconfig like 'BigRun5' and run.runtype='PHYSICS' and run.loggingon!='NO' and run.eventsinrun>10000 and run.runnumber>161196 and run.runnumber<200000 and run.runnumber=trigger.runnumber and (trigger.name='ZDCNS' or trigger.name='BBCLL1' or trigger.name='BBCLL1(>0 tubes)')
  runs.push_back(161197); // Run5 Cu+Cu 62.4 GeV
  runs.push_back(163627);

  //  char set[80];

  for ( size_t i = 0; i < runs.size(); ++i )
    {
      TLine* l = new TLine(runs[i]+xshift,ymin,runs[i]+xshift,ymax);
      l->SetLineStyle(2);
      l->SetLineColor(4);
      l->Draw();
//       sprintf(set,"S%d",firstset+i/2);
//       if ( i % 2 == 0 )
// 	{
// 	  TText* t = new TText(runs[i]+xshift,ymax*0.8,set);
// 	  t->SetTextAngle(45);
// 	  t->SetTextSize(0.03);
// 	  t->Draw();
// 	}
    }
}


void
sectorOnlineToOffline(int sectorOnline, int& arm,
		      int& offline_sector)
{
  if ( sectorOnline < 0 || sectorOnline > 8 ) 
    {
      arm=-1;
      offline_sector=-1;
    }

  if ( sectorOnline < 4 ) 
    {
      arm = 0;
      offline_sector = sectorOnline;
    }
  else if ( sectorOnline < 6 )
    {
      arm = 1;
      offline_sector = sectorOnline-2;
    }
  else
    {
      arm = 1;
      offline_sector = sectorOnline-6;
    }
}

void shift(TGraphErrors* g, double xshift)
{
  for ( int i = 0; i < g->GetN(); ++i ) 
    {
      double x,y;
      g->GetPoint(i,x,y);
      g->SetPoint(i,x+xshift,y);
      g->SetPointError(i,0.5,0);
    }
}

void draw(const char* sector,
	  double xmin, double xmax, double ymin, double ymax,
	  TGraphErrors** g, int n)
{ 
  TH2* h = (TH2*)(gPad->FindObject(sector));
  if (!h)
    {
      cout << "Creating frame histo " << sector << endl;
      h = new TH2F(sector,sector,100,xmin+xshift,xmax+xshift,100,ymin,ymax);
      h->Draw();
      //      h->SetNdivisions(505);
      h->SetNdivisions(420);
    }

  int color[] = { 1,2,4,5,6,7 } ;
  int marker[] = { 20,21,22,23,24,25 } ;

  for (int i = 0; i < n; ++i )
    {
      if ( !g[i] ) continue;
      TGraphErrors* gc = (TGraphErrors*)g[i]->Clone();
      shift(gc,xshift);
      gc->SetLineStyle(i+1);
      gc->SetLineColor(color[i]);
      gc->SetMarkerColor(color[i]);
      gc->SetMarkerStyle(marker[i]);
      gc->SetMarkerSize(0.7);
      cout << "Drawing " << gc->GetName() << endl;
      gc->Draw("LP");
      
    }
}

void getGraphLimits(TGraphErrors& g, double& xmin, double& xmax)
{
  xmin = g.GetX()[0];
  xmax = g.GetX()[g.GetN()-1];
}

void draw_GainEvolution(const char* file="gainEvolution.runlist_63GeV_good_and_or_ppg.root", const char* type = "92446_abs", double ymin=0, double ymax=0, double xmin=0, double xmax=0)
// type can be rprod = relative to what is used in production
// or reor3 = relative to end of run3
// or abs = real value of gains
{
  TGaxis::SetMaxDigits(7);
  gROOT->SetStyle("Plain");

  gStyle->SetOptStat(0);

  TFile* f = new TFile(file);

  const int NG = 4;
  const int NS = 6;

  TGraphErrors* g[NG];

  const char* sectors[] = { "W0","W1","W2","W3","E2","E3" };
  const char* gpattern[] = 
    {
      "g%s_0_x_ZS_AVOFRATIO_%s",
      "g%s_5_x_ZS_AVOFRATIO_%s",
      "g%s_0_x_ZS_AVOFASYM_%s",
      "g%s_5_x_ZS_AVOFASYM_%s"
    };

  TCanvas* c = new TCanvas("c","c",800,600);
  c->Divide(2,4,0.001,0.001);
  c->Draw();

  //  int percent = 5;

  int ipad[] = { 7,5,3,1,4,2,8,6 };

  for ( int i = 0; i < NS; ++i ) 
    {
      for ( int ig = 0; ig < NG; ++ig ) 
	{
	  char gname[80];
	  sprintf(gname,gpattern[ig],sectors[i],type);
	  g[ig] = (TGraphErrors*)f->Get(gname);
	  if ( !g[ig] )
	    {
	      printf("Graph %s does not exist in file\n",gname);
	      continue;
	    }
	  
	  if ( xmin==0 && xmax==0 )
	    {
	      getGraphLimits(*(g[ig]),xmin,xmax);
	      printf("%f %f\n",xmin,xmax);
	    }
	}

      char cname[80];
      sprintf(cname,"%s_%s",sectors[i],type);
      int arm,sector;
      sectorOnlineToOffline(i,arm,sector);
      //      int width = 600;
      //      int height = 250;
      //      int topx = arm*width;
      //      int topy = sector*(height+30);
      //      c[i] = new TCanvas(cname,cname,topx,topy,width,height);
      TVirtualPad* pad = c->cd(ipad[i]);
      pad->SetGridy();
      pad->SetGridx();
      pad->SetTicky();
      pad->SetTickx();
      pad->SetRightMargin(0.01);
      pad->SetTopMargin(0.01);

      draw(sectors[i],xmin,xmax,ymin,ymax,g,NG);
      //      drawSetLimits(ymin,ymax);
      drawRunLimits(ymin,ymax);

      pad->Update();

//       sprintf(cname,"%s_%s.eps",sectors[i],type);      
//       pad->Print(cname);
    }
}
