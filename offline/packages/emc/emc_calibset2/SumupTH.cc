
#include "SumupTH.hh"
#include <stream.h>
#include <cmath>

//===========================================================================
//===========================================================================
int SumupTH1(TH1* hout, TH1* hin, Double_t shiftin, char *opt){
  char* opt_e;
  opt_e = strstr(opt,"E");
  char* opt_d;
  opt_d = strstr(opt,"D");

  int tnum_sumup = 0;
  TAxis* axout = hout->GetXaxis();
  TAxis* axin = hin->GetXaxis();
  Axis_t widthout = axout->GetBinWidth(0);
  Axis_t widthin = axin->GetBinWidth(0);
  int binnum = axout->GetNbins();
  if( opt_d ){
    cout<<" ------------------------ SumupTH1 -------------------------- "<<endl;
    cout<<" widthin,out = "<<widthin<<","<<widthout<<endl;
  }
  if( (widthout - widthin) * (double)binnum < widthout ){
    int bin_firstout = axout->GetFirst();
    int bin_lastout = axout->GetLast();
    int bin_firstin = axin->GetFirst();
    int bin_lastin = axin->GetLast();
    int bin;
    Stat_t cont,err;
    Stat_t overflow = 0;
    Stat_t overflow_err = 0;
    Stat_t underflow = 0;
    Stat_t underflow_err = 0;

    //------------------------------------ Calculation for proper shift.
    //Axis_t tmp_shiftin = 
    //  (axin->GetBinLowEdge(bin_firstin) + shiftin - axout->GetBinCenter(bin_firstout) )
    //  /widthout;
    //int bin_shiftin = (int) floor(tmp_shiftin) + 1;
    Axis_t tmp_shiftin = 
      (axin->GetBinCenter(bin_firstin) + shiftin - axout->GetBinLowEdge(bin_firstout) )
      /widthout;
    int bin_shiftin = (int) floor(tmp_shiftin);
    if( opt_d ){
      //cout<<" widthin = "<<widthin<<endl;
      cout<<" bin_firstin,lastin = "<<bin_firstin<<","<<bin_lastin<<endl;
      cout<<" shiftin = "<<shiftin<<endl;
      cout<<" axin->GetBinLowEdge(bin_firstin) = "<<axin->GetBinLowEdge(bin_firstin)<<"      ";
      cout<<" axout->GetBinCenter(bin_firstout) = "<<axout->GetBinCenter(bin_firstout)<<endl;
      cout<<" shiftin = "<<tmp_shiftin<<" --> "<<bin_shiftin<<endl;
    }
    //------------------------------------ Copy hin to hout
    overflow += hout->GetBinContent(bin_lastout+1);
    underflow += hout->GetBinContent(bin_firstout-1);
    overflow += hin->GetBinContent(bin_lastin+1);
    underflow += hin->GetBinContent(bin_firstin-1);
    bin = bin_lastin + 1;
    while( bin -- > bin_firstin ){
      if( bin+bin_shiftin > bin_lastout ){
	overflow += hin->GetBinContent(bin);
	if( opt_e ) overflow_err += hin->GetBinError(bin)*hin->GetBinError(bin);
      } else if( bin+bin_shiftin < bin_firstout ){
	underflow += hin->GetBinContent(bin);
	if( opt_e ) underflow_err += hin->GetBinError(bin)*hin->GetBinError(bin);
      } else {
	cont = hout->GetBinContent(bin+bin_shiftin) + hin->GetBinContent(bin);
	hout->SetBinContent(bin+bin_shiftin,cont);
	if( opt_e ){
	  err = sqrt( hout->GetBinError(bin+bin_shiftin)*hout->GetBinError(bin+bin_shiftin)
		      + hin->GetBinError(bin)*hin->GetBinError(bin) );
	  hout->SetBinError(bin+bin_shiftin,err);
	}
      }
      tnum_sumup++;
    }
    //------------------------------------- Entries,Overflow,Underflow.....
    hout->SetEntries( hin->GetEntries() + hout->GetEntries() );
    hout->SetBinContent(bin_firstout-1, underflow);
    hout->SetBinContent(bin_lastout+1, overflow);
    if( opt_e ) {
      underflow_err = sqrt(underflow_err);
      overflow_err = sqrt(overflow_err);
      hout->SetBinError(bin_firstout-1, underflow_err);
      hout->SetBinError(bin_lastout+1, overflow_err);
    }
  } else {
    if( opt_d ){
      cout<<" ERROR:: Can't Sumup .. "<<endl;
      cout<<"    (widthout == widthin) = "<<(bool)(widthout == widthin)<<endl;
      printf("widthin =   %.30f\n",widthin);
      printf("widthout = %.30f\n",widthout);
    }
  }
  if( opt_d )
    cout<<" ------------------------------------------------------------ "<<endl;
  
  return tnum_sumup;
};

//===========================================================================
//===========================================================================
int SumupTH2(TH2* hout, TH2* hin, Double_t shiftin, char *opt){
  char* opt_e;
  opt_e = strstr(opt,"E");
  char* opt_2d;
  opt_2d = strstr(opt,"2D");
  //
  if( opt_2d ) cout<<" ============================== SumupTH1 ============================= "<<endl;
  int binx,biny,bin;
  TAxis* xao = hout->GetXaxis();
  TAxis* yao = hout->GetYaxis();
  int yao_first = yao->GetFirst();
  int yao_last  = yao->GetLast();
  int xao_first = xao->GetFirst();
  int xao_last  = xao->GetLast();
  int bin_first,bin_last;
  float cont,err;
  int tnum_sumup = 0;
  //
  if( hout->GetNbinsY() == hin->GetNbinsY() ){
    for( biny = yao_first; biny <= yao_last ; biny++){
      TH1D* hout_px = hout->ProjectionX("hout_px",biny,biny);
      TH1D* hin_px = hin->ProjectionX("hin_px",biny,biny);
      if( opt_2d && biny > yao_last - 3 )
	tnum_sumup += SumupTH1(hout_px,hin_px,shiftin,opt);
      else
	tnum_sumup += SumupTH1(hout_px,hin_px,shiftin);
      bin_first = hout->GetBin(xao_first,biny); // Underflow
      //bin_last  = hout->GetBin(xao_last,biny);  // Overflow
      bin = bin_first - 1;
      for( binx = xao_first-1 ; binx <= xao_last+1 ; binx++ ){
	cont = hout_px->GetBinContent(binx);
	hout->SetBinContent(bin,cont);
	if( opt_e ){
	  err = hout_px->GetBinError(binx);
	  hout->SetBinError(bin,err);
	}
	bin++;
      }
      hout_px->Delete();
      hin_px->Delete();
    }

  }
  // else {
  // cout<<" Error:: SumupTH() :: can't sumup "<<endl;
  //}
  if( opt_2d ) cout<<" ============================== SumupTH1 ============================= "<<endl;

  return tnum_sumup;
};
//===========================================================================
//===========================================================================
int SumupTH2(TH2* hout, TH2* hin, TH1* hshiftin, char *opt = ""){
  char* opt_e;
  opt_e = strstr(opt,"E");
  char* opt_d;
  opt_d = strstr(opt,"D");
  int binx,biny,bin;
  TAxis* xao = hout->GetXaxis();
  TAxis* yao = hout->GetYaxis();
  int yao_first = yao->GetFirst();
  int yao_last  = yao->GetLast();
  int xao_first = xao->GetFirst();
  int xao_last  = xao->GetLast();
  int bin_first,bin_last;
  float cont,err;
  int tnum_sumup = 0;
  float shiftin;
  //
  if( hout->GetNbinsY() == hin->GetNbinsY() ){
    for( biny = yao_first; biny <= yao_last ; biny++){
      TH1D* hout_px = hout->ProjectionX("hout_px",biny,biny);
      TH1D* hin_px = hin->ProjectionX("hin_px",biny,biny);
      if( hshiftin == NULL )
	shiftin = 0;
      else
	shiftin = hshiftin->GetBinContent(biny);
      tnum_sumup += SumupTH1(hout_px,hin_px,shiftin,opt);
      bin_first = hout->GetBin(xao_first,biny); // Underflow
      //bin_last  = hout->GetBin(xao_last,biny);  // Overflow
      bin = bin_first - 1;
      for( binx = xao_first-1 ; binx <= xao_last+1 ; binx++ ){
	cont = hout_px->GetBinContent(binx);
	hout->SetBinContent(bin,cont);
	if( opt_e ){
	  err = hout_px->GetBinError(binx);
	  hout->SetBinError(bin,err);
	}
	bin++;
      }
      hout_px->Delete();
      hin_px->Delete();
    }

  }
  // else {
  // cout<<" Error:: SumupTH() :: can't sumup "<<endl;
  //}

  return tnum_sumup;
};
//===========================================================================
//===========================================================================
int SumupTH2(TH2* hout, TH2* hin, TGraph* gshiftin, char *opt = ""){
  char* opt_e;
  opt_e = strstr(opt,"E");
  char* opt_d;
  opt_d = strstr(opt,"D");
  int binx,biny,bin;
  TAxis* xao = hout->GetXaxis();
  TAxis* yao = hout->GetYaxis();
  int yao_first = yao->GetFirst();
  int yao_last  = yao->GetLast();
  int xao_first = xao->GetFirst();
  int xao_last  = xao->GetLast();
  int bin_first,bin_last;
  float cont,err;
  int tnum_sumup = 0;
  float shiftin;
  //
  if( hout->GetNbinsY() == hin->GetNbinsY() ){
    for( biny = yao_first; biny <= yao_last ; biny++){
      TH1D* hout_px = hout->ProjectionX("hout_px",biny,biny);
      TH1D* hin_px = hin->ProjectionX("hin_px",biny,biny);
      if( gshiftin == NULL )
	shiftin = 0;
      else{
	double* y = gshiftin->GetY();
	shiftin = y[biny-1];
	// This is [biny-1] not [biny]!!!!
	// Because TAxis is 1 start otherwise TGraph is 0 start.
      }
      tnum_sumup += SumupTH1(hout_px,hin_px,shiftin,opt);
      bin_first = hout->GetBin(xao_first,biny); // Underflow
      //bin_last  = hout->GetBin(xao_last,biny);  // Overflow
      bin = bin_first - 1;
      for( binx = xao_first-1 ; binx <= xao_last+1 ; binx++ ){
	cont = hout_px->GetBinContent(binx);
	hout->SetBinContent(bin,cont);
	if( opt_e ){
	  err = hout_px->GetBinError(binx);
	  hout->SetBinError(bin,err);
	}
	bin++;
      }
      hout_px->Delete();
      hin_px->Delete();
    }

  }
  // else {
  // cout<<" Error:: SumupTH() :: can't sumup "<<endl;
  //}

  return tnum_sumup;
};
//===========================================================================
int SumupY_TH3(TH3* hout, TH3* hin, TH1* hshiftin, char *opt = ""){
  char* opt_e;
  opt_e = strstr(opt,"E");
  char* opt_d;
  opt_d = strstr(opt,"D");
  int binx,biny,binz,bin;
  TAxis* xao = hout->GetXaxis();
  TAxis* yao = hout->GetYaxis();
  TAxis* zao = hout->GetZaxis();
  int xao_first = xao->GetFirst();
  int xao_last  = xao->GetLast();
  int yao_first = yao->GetFirst();
  int yao_last  = yao->GetLast();
  int zao_first = zao->GetFirst();
  int zao_last  = zao->GetLast();
  float cont,err;
  int tnum_sumup = 0;
  float shiftin;
  //
  if( hout->GetNbinsY() == hin->GetNbinsY() ){
    for( binz = zao_first; binz <= zao_last ; binz++){
      for( binx = xao_first; binx <= xao_last ; binx++){
	hout->GetZaxis()->SetRange(binz,binz);
	hout->GetXaxis()->SetRange(binx,binx);
	TH1* hout_p = hout->Project3D("y");
	hin->GetZaxis()->SetRange(binz,binz);
	hin->GetXaxis()->SetRange(binx,binx);
	TH1* hin_p = hin->Project3D("y");
	if( hshiftin == NULL )
	  shiftin = 0;
	else
	  shiftin = hshiftin->GetBinContent(binz);
	tnum_sumup += SumupTH1(hout_p,hin_p,shiftin,opt);
	for( biny = yao_first-1 ; biny <= yao_last+1 ; biny++ ){
	  cont = hout_p->GetBinContent(biny);
	  bin = hout->GetBin(binx,biny,binz);
	  hout->SetBinContent(bin,cont);
	  if( opt_e ){
	    err = hout_p->GetBinError(biny);
	    hout->SetBinError(bin,err);
	  }
	}
	hout_p->Delete();
	hin_p->Delete();
      }
    }
  }
  hout->GetZaxis()->SetRange(zao_first,zao_last);
  hout->GetXaxis()->SetRange(xao_first,xao_last);

  return tnum_sumup;
};
//===========================================================================
int SumupY_TH3(TH3* hout, TH3* hin, TGraph* gshiftin, char *opt = ""){
  char* opt_e;
  opt_e = strstr(opt,"E");
  char* opt_d;
  opt_d = strstr(opt,"D");
  int binx,biny,binz,bin;
  TAxis* xao = hout->GetXaxis();
  TAxis* yao = hout->GetYaxis();
  TAxis* zao = hout->GetZaxis();
  int xao_first = xao->GetFirst();
  int xao_last  = xao->GetLast();
  int yao_first = yao->GetFirst();
  int yao_last  = yao->GetLast();
  int zao_first = zao->GetFirst();
  int zao_last  = zao->GetLast();
  float cont,err;
  int tnum_sumup = 0;
  float shiftin;
  double* y = gshiftin->GetY();
  //
  if( hout->GetNbinsZ() == hin->GetNbinsZ() &&
      hout->GetNbinsX() == hin->GetNbinsX() ){ 
    for( binz = zao_first; binz <= zao_last ; binz++){
      for( binx = xao_first; binx <= xao_last ; binx++){
	hout->GetZaxis()->SetRange(binz,binz);
	hout->GetXaxis()->SetRange(binx,binx);
	TH1* hout_p = hout->Project3D("y");
	hin->GetZaxis()->SetRange(binz,binz);
	hin->GetXaxis()->SetRange(binx,binx);
	TH1* hin_p = hin->Project3D("y");
	if( gshiftin == NULL )
	  shiftin = 0;
	else
	  shiftin = y[binz];
	tnum_sumup += SumupTH1(hout_p,hin_p,shiftin,opt);
	for( biny = yao_first-1 ; biny <= yao_last+1 ; biny++ ){
	  cont = hout_p->GetBinContent(biny);
	  bin = hout->GetBin(binx,biny,binz);
	  hout->SetBinContent(bin,cont);
	  if( opt_e ){
	    err = hout_p->GetBinError(biny);
	    hout->SetBinError(bin,err);
	  }
	}
	hout_p->Delete();
	hin_p->Delete();
      }
    }
  }
  hout->GetZaxis()->SetRange(zao_first,zao_last);
  hout->GetXaxis()->SetRange(xao_first,xao_last);

  return tnum_sumup;
};
//===========================================================================
//===========================================================================
//===========================================================================
int SumupTH1(TH1* hout, TH1* hin, TF1* fx, char *opt = "",int nslice=10){
  //  char* opt_e;
  //  opt_e = strstr(opt,"e");
  char* opt_d;
  opt_d = strstr(opt,"d");
  int tnum_sumup = 0;
  int binx;
  float cont,err;
  int slx;
  float lowx;
  float upx;
  float xin;
  float xout;
  TAxis* ax = hin->GetXaxis();
  int ax_first = ax->GetFirst();
  int ax_last  = ax->GetLast();
  //
  for( binx = ax_first; binx <= ax_last; binx++){
    lowx = ax->GetBinLowEdge(binx);
    upx = ax->GetBinUpEdge(binx);
    cont = hin->GetBinContent(binx);
    if( cont != 0. ){
      cont = cont / nslice ;
      slx = nslice;
      while( slx-- ){
	xin = ( upx - lowx ) * (slx + 0.5)/nslice + lowx;
	xout = fx->Eval(xin);
	hout->Fill(xout,cont);
      }
    } // End of if(cont!=0.)
    //
    tnum_sumup++;
  }
  hout->SetEntries( hin->GetEntries() );

  return tnum_sumup;
};
//===========================================================================
int SumupTH2(TH2* hout, TH2* hin, TF1* fx, TF1* fy, char *opt = "",int nslice=10){
  //  char* opt_e;
  //  opt_e = strstr(opt,"e");
  char* opt_d;
  opt_d = strstr(opt,"d");
  int tnum_sumup = 0;
  int binx,biny;
  float cont,err;
  int slx,sly;
  float lowx,lowy;
  float upx,upy;
  float xin,yin;
  float xout,yout;
  TAxis* ax = hin->GetXaxis();
  TAxis* ay = hin->GetYaxis();
  int ax_first = ax->GetFirst();
  int ax_last  = ax->GetLast();
  int ay_first = ay->GetFirst();
  int ay_last  = ay->GetLast();
  //
  for( binx = ax_first; binx <= ax_last; binx++){
    for( biny = ay_first; biny <= ay_last; biny++){
      lowx = ax->GetBinLowEdge(binx);
      lowy = ay->GetBinLowEdge(biny);
      upx = ax->GetBinUpEdge(binx);
      upy = ay->GetBinUpEdge(biny);
      cont = hin->GetBinContent(binx,biny);
      if( cont != 0. ){
	cont = cont / nslice / nslice ;
	//if( opt_d && cont > 0 ) cout<<" bin("<<binx<<","<<biny<<") : cont = "<<cont<<endl;
	slx = nslice;
	while( slx-- ){
	  sly = nslice;
	  while( sly-- ){
	    xin = ( upx - lowx ) * (slx + 0.5)/nslice + lowx;
	    yin = ( upy - lowy ) * (sly + 0.5)/nslice + lowy;
	    xout = fx->Eval(xin,yin);
	    yout = fy->Eval(xin,yin);
	    //if( opt_d && cont > 0 ) cout<<" out("<<xout<<","<<yout<<") : cont = "<<cont<<endl;
	    hout->Fill(xout,yout,cont);
	  }
	}
      } // End of if(cont!=0.)
      //
      tnum_sumup++;
    }
  }
  hout->SetEntries( hin->GetEntries() );

  return tnum_sumup;
};
//===========================================================================
int SumupTH3(TH3* hout, TH3* hin, TF1* fx, TF1* fy, TF1* fz, char *opt = "",int nslice=4){
  //
  //  char* opt_e;
  //  opt_e = strstr(opt,"E");
  char* opt_d;
  opt_d = strstr(opt,"D");
  int tnum_sumup = 0;
  int binx,biny,binz;
  float cont,err;
  int slx,sly,slz;
  float lowx,lowy,lowz;
  float upx,upy,upz;
  float xin,yin,zin;
  float xout,yout,zout;
  TAxis* ax = hin->GetXaxis();
  TAxis* ay = hin->GetYaxis();
  TAxis* az = hin->GetZaxis();
  int ax_first = ax->GetFirst();
  int ax_last  = ax->GetLast();
  int ay_first = ay->GetFirst();
  int ay_last  = ay->GetLast();
  int az_first = az->GetFirst();
  int az_last  = az->GetLast();
  //
  for( binx = ax_first; binx <= ax_last; binx++){
    for( biny = ay_first; biny <= ay_last; biny++){
      for( binz = az_first; binz <= az_last; binz++){
	lowx = ax->GetBinLowEdge(binx);
	lowy = ay->GetBinLowEdge(biny);
	lowz = az->GetBinLowEdge(binz);
	upx = ax->GetBinUpEdge(binx);
	upy = ay->GetBinUpEdge(biny);
	upz = az->GetBinUpEdge(binz);
	cont = hin->GetBinContent(binx,biny,binz);
	if( cont != 0. ){
	  cont = cont / nslice / nslice / nslice;
	  //
	  slx = nslice;
	  while( slx-- ){
	    sly = nslice;
	    while( sly-- ){
	      slz = nslice;
	      while( slz-- ){
		xin = ( upx - lowx ) * (slx + 0.5)/nslice + lowx;
		yin = ( upy - lowy ) * (sly + 0.5)/nslice + lowy;
		zin = ( upz - lowz ) * (slz + 0.5)/nslice + lowz;
		xout = fx->Eval(xin,yin,zin);
		yout = fy->Eval(xin,yin,zin);
		zout = fz->Eval(xin,yin,zin);
		hout->Fill(xout,yout,zout,cont);
	      }
	    }
	  }
	} // End of if(cont!=0.)
	//
	tnum_sumup++;
      }
    }
  }
  hout->SetEntries( hin->GetEntries() );

  return tnum_sumup;
};

//===========================================================================
//===========================================================================
//===========================================================================
//===========================================================================
//===========================================================================
//===========================================================================
//===========================================================================
//===========================================================================
//===========================================================================
//===========================================================================
//===========================================================================
//===========================================================================
//===========================================================================
//===========================================================================
#ifdef SKIPSKIPSKIPSKIPPPPPPPPPP
//===========================================================================
//===========================================================================
int SumupTH1(TH1* hout, TH1* hin1, TH1* hin2, Double_t shift1,Double_t shift2,char *opt){
  char* opt_e;
  opt_e = strstr(opt,"E");
  char* opt_d;
  opt_d = strstr(opt,"D");

  int tnum_sumup = 0;
  TAxis* axout = hout->GetXaxis();
  TAxis* ax1 = hin1->GetXaxis();
  TAxis* ax2 = hin2->GetXaxis();
  Axis_t widthout = axout->GetBinWidth(0);
  Axis_t width1 = ax1->GetBinWidth(0);
  Axis_t width2 = ax2->GetBinWidth(0);
  int binnum = axout->GetNbins();
  if( opt_d ){
    cout<<" ------------------------ SumupTH1 -------------------------- "<<endl;
    cout<<" width1,width2 = "<<width1<<","<<width2<<endl;
    cout<<" widthout = "<<widthout<<endl;
  }
  if( (widthout - width1) * (double)binnum < widthout &&
      (widthout - width2) * (double)binnum < width2    ){
    int bin_firstout = axout->GetFirst();
    int bin_lastout = axout->GetLast();
    int bin_first1 = ax1->GetFirst();
    int bin_last1 = ax1->GetLast();
    int bin_first2 = ax2->GetFirst();
    int bin_last2 = ax2->GetLast();
    int bin;
    Stat_t cont,err;
    Stat_t overflow = 0;
    Stat_t overflow_err = 0;
    Stat_t underflow = 0;
    Stat_t underflow_err = 0;

    //------------------------------------ Calculation for proper shift.
    Axis_t tmp_shift1 = 
      (ax1->GetBinLowEdge(bin_first1) + shift1 - axout->GetBinCenter(bin_firstout) )
      /widthout;
    Axis_t tmp_shift2 = 
      ( ax2->GetBinCenter(bin_first2) + shift2 - axout->GetBinCenter(bin_firstout) )
      /widthout;
    int bin_shift1 = (int) floor(tmp_shift1) + 1;
    int bin_shift2 = (int) floor(tmp_shift2) + 1;
    if( opt_d ){
      cout<<" width1,width2 = "<<width1<<","<<width2<<endl;
      cout<<" bin_first1,last1 = "<<bin_first1<<","<<bin_last1<<endl;
      cout<<" bin_first2,last2 = "<<bin_first2<<","<<bin_last2<<endl;
      cout<<" shift1,shift2 = "<<shift1<<","<<shift2<<endl;
      cout<<" ax1->GetBinLowEdge(bin_first1) = "<<ax1->GetBinLowEdge(bin_first1)<<endl;
      cout<<" ax1->GetBinCenter(bin_first1) = "<<ax1->GetBinCenter(bin_first1)<<endl;
      cout<<" ax2->GetBinCenter(bin_first2) = "<<ax2->GetBinCenter(bin_first2)<<endl;
      cout<<" tmp_shift1 = "<<tmp_shift1<<endl;
      cout<<" tmp_shift2= "<<tmp_shift2<<endl;
      cout<<" bin_shift1 = "<<bin_shift1<<endl;
      cout<<" bin_shift2 = "<<bin_shift2<<endl;
    }
    //------------------------------------ Copy hin1 to hout
    overflow += hin1->GetBinContent(bin_last1+1);
    underflow += hin1->GetBinContent(bin_first1-1);
    bin = bin_last1;
    while( bin -- > bin_first1 ){
      if( bin+bin_shift1 > bin_lastout ){
	overflow += hin1->GetBinContent(bin);
	if( opt_e ) overflow_err = hin1->GetBinError(bin)*hin1->GetBinError(bin);
      } else if( bin+bin_shift1 < bin_firstout ){
	underflow += hin1->GetBinContent(bin);
	if( opt_e ) underflow_err = hin1->GetBinError(bin)*hin1->GetBinError(bin);
      } else {
	//cont = hout->GetBinContent(bin+bin_shift1) + hin1->GetBinContent(bin);
	cont = hin1->GetBinContent(bin);
	hout->SetBinContent(bin+bin_shift1,cont);
	if( opt_e ){
	  //err = sqrt( hout->GetBinError(bin+bin_shift1)*hout->GetBinError(bin+bin_shift1)
	  //      + hin1->GetBinError(bin)*hin1->GetBinError(bin) );
	  err = hin1->GetBinError(bin);
	  hout->SetBinError(bin+bin_shift1,err);
	}
      }
      tnum_sumup++;
    }
    //------------------------------------ Copy hin2 to hout
    overflow += hin2->GetBinContent(bin_last2+1);
    underflow += hin2->GetBinContent(bin_first2-1);
    bin = bin_last2;
    while( bin -- > bin_first2 ){
      if( bin+bin_shift2 > bin_lastout ){
	overflow += hin2->GetBinContent(bin);
	if( opt_e ) overflow_err = hin2->GetBinError(bin)*hin2->GetBinError(bin);
      } else if( bin+bin_shift2 < bin_firstout ){
	underflow += hin2->GetBinContent(bin);
	if( opt_e ) underflow_err = hin2->GetBinError(bin)*hin2->GetBinError(bin);
      } else {
	cont = hout->GetBinContent(bin+bin_shift2) + hin2->GetBinContent(bin);
	hout->SetBinContent(bin+bin_shift2,cont);
	if( opt_e ) {
	  err = sqrt( hout->GetBinError(bin+bin_shift2)*hout->GetBinError(bin+bin_shift2)
		      + hin2->GetBinError(bin)*hin2->GetBinError(bin) );
	  hout->SetBinError(bin+bin_shift2,err);
	}
      }
      tnum_sumup++;
    }
    //------------------------------------- Entries,Overflow,Underflow.....
    hout->SetEntries( hin1->GetEntries() + hin2->GetEntries() );
    hout->SetBinContent(bin_firstout-1, underflow);
    hout->SetBinContent(bin_lastout+1, overflow);
    if( opt_e ) {
      underflow_err += hin1->GetBinError(bin_first1-1)*hin1->GetBinError(bin_first1-1);
      underflow_err += hin2->GetBinError(bin_first2-1)*hin2->GetBinError(bin_first2-1);
      underflow_err = sqrt(underflow_err);
      overflow_err += hin1->GetBinError(bin_last1+1)*hin1->GetBinError(bin_last1+1);
      overflow_err += hin2->GetBinError(bin_last2+1)*hin2->GetBinError(bin_last2+1);
      overflow_err = sqrt(overflow_err);
      hout->SetBinError(bin_firstout-1, underflow_err);
      hout->SetBinError(bin_lastout+1, overflow_err);
    }
  } else {
    if( opt_d ){
      cout<<" ERROR:: Can't Sumup .. "<<endl;
      cout<<"    (widthout == width1) = "<<(bool)(widthout == width1)<<endl;
      cout<<"    (widthout == width2) = "<<(bool)(widthout == width2)<<endl;
      printf("width1 =   %.30f\n",width1);
      printf("width2 =   %.30f\n",width2);
      printf("widthout = %.30f\n",widthout);
    }
  }
  if( opt_d )
    cout<<" ------------------------------------------------------------ "<<endl;
  
  return tnum_sumup;
};
//===========================================================================
//===========================================================================
int SumupTH2(TH2* hout, TH2* hin1, TH2* hin2,Double_t shift1,Double_t shift2,char *opt){
  char* opt_e;
  opt_e = strstr(opt,"E");
  //
  int binx,biny,bin;
  TAxis* xao = hout->GetXaxis();
  TAxis* yao = hout->GetYaxis();
  int yao_first = yao->GetFirst();
  int yao_last  = yao->GetLast();
  int xao_first = xao->GetFirst();
  int xao_last  = xao->GetLast();
  int bin_first,bin_last;
  float cont,err;
  int tnum_sumup = 0;
  //
  if( hout->GetNbinsY() == hin1->GetNbinsY() &&
      hout->GetNbinsY() == hin2->GetNbinsY() ){
    for( biny = yao_first; biny <= yao_last ; biny++){
      TH1D* hout_px = hout->ProjectionX("hout_px",biny,biny);
      TH1D* hin1_px = hin1->ProjectionX("hin1_px",biny,biny);
      TH1D* hin2_px = hin2->ProjectionX("hin2_px",biny,biny);
      char* opt_2d;
      opt_2d = strstr(opt,"2D");
      if( opt_2d )
	tnum_sumup += SumupTH1(hout_px,hin1_px,hin2_px,shift1,shift2,opt);
      else
	tnum_sumup += SumupTH1(hout_px,hin1_px,hin2_px,shift1,shift2);
      bin_first = hout->GetBin(xao_first,biny); // Underflow
      //bin_last  = hout->GetBin(xao_last,biny);  // Overflow
      bin = bin_first - 1;
      for( binx = xao_first-1 ; binx <= xao_last+1 ; binx++ ){
	cont = hout_px->GetBinContent(binx);
	hout->SetBinContent(bin,cont);
	if( opt_e ){
	  err = hout_px->GetBinError(binx);
	  hout->SetBinError(bin,err);
	}
	bin++;
      }
      hout_px->Delete();
      hin1_px->Delete();
      hin2_px->Delete();
    }

  }
  // else {
  // cout<<" Error:: SumupTH() :: can't sumup "<<endl;
  //}

  return tnum_sumup;
};
//===========================================================================
//===========================================================================
int SumupTH2(TH2* hout, TH2* hin1, TH2* hin2,TH1* hshift1,TH1* hshift2,char *opt = ""){
  char* opt_e;
  opt_e = strstr(opt,"E");
  char* opt_d;
  opt_d = strstr(opt,"D");
  int binx,biny,bin;
  TAxis* xao = hout->GetXaxis();
  TAxis* yao = hout->GetYaxis();
  int yao_first = yao->GetFirst();
  int yao_last  = yao->GetLast();
  int xao_first = xao->GetFirst();
  int xao_last  = xao->GetLast();
  int bin_first,bin_last;
  float cont,err;
  int tnum_sumup = 0;
  float shift1,shift2;
  //
  if( hout->GetNbinsY() == hin1->GetNbinsY() &&
      hout->GetNbinsY() == hin2->GetNbinsY() ){
    for( biny = yao_first; biny <= yao_last ; biny++){
      TH1D* hout_px = hout->ProjectionX("hout_px",biny,biny);
      TH1D* hin1_px = hin1->ProjectionX("hin1_px",biny,biny);
      TH1D* hin2_px = hin2->ProjectionX("hin2_px",biny,biny);
      if( hshift1 == NULL )
	shift1 = 0;
      else
	shift1 = hshift1->GetBinContent(biny);
      if( hshift2 == NULL )
	shift2 = 0;
      else
	shift2 = hshift2->GetBinContent(biny);
      tnum_sumup += SumupTH1(hout_px,hin1_px,hin2_px,shift1,shift2,opt);
      bin_first = hout->GetBin(xao_first,biny); // Underflow
      //bin_last  = hout->GetBin(xao_last,biny);  // Overflow
      bin = bin_first - 1;
      for( binx = xao_first-1 ; binx <= xao_last+1 ; binx++ ){
	cont = hout_px->GetBinContent(binx);
	hout->SetBinContent(bin,cont);
	if( opt_e ){
	  err = hout_px->GetBinError(binx);
	  hout->SetBinError(bin,err);
	}
	bin++;
      }
      hout_px->Delete();
      hin1_px->Delete();
      hin2_px->Delete();
    }

  }
  // else {
  // cout<<" Error:: SumupTH() :: can't sumup "<<endl;
  //}

  return tnum_sumup;
};

#endif
//===========================================================================
//===========================================================================
