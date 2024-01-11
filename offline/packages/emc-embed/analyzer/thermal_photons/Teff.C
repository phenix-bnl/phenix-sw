void Teff(){

   //==========================Var Vol =============================
  //TFile *fileV = new TFile("sumcnstvol.root");
 // TFile *fileV = new TFile("sumvarvol.root");  //for tau=0.6 fm/c
  TFile *fileV = new TFile("sum_varvol_qgp_tau015_v00.root");
  TH2F *hTotV = (TH2F*)fileV->Get("h100");
  
  const Int_t Nranges = 10;

  Double_t slopeV[100][Nranges] ;
  Double_t slopeErrV[100][Nranges] ;
  
  TF1 * fExp[Nranges];
  for(Int_t i=0;i<Nranges;i++){
    char tit[10] ;
    sprintf(tit,"Ext%d",i) ;
    fExp[i] = new TF1(tit,"[0]*exp(-x/[1])",0.5*i,0.5*(i+1)) ;
    fExp[i]->SetParameters(10,0.2) ;
    fExp[i]->SetParNames("Const","T") ;
  }

  //Calculate effective temperature
  for(Int_t j=1;j<=100;j++){
    TH1D * fSlix =  hTotV->ProjectionX("slix",j,j) ;
    for(Int_t i=1;i<=fSlix->GetNbinsX();i++){
      fSlix->SetBinError(i,0.001*fSlix->GetBinContent(i)) ;
    }
    for(Int_t i=0;i<Nranges;i++){
      fSlix->Fit(fExp[i],"RIQ") ;
      slopeV[j-1][i]   = fExp[i]->GetParameter(1) ;
      slopeErrV[j-1][i]= fExp[i]->GetParError(1) ;
    }
  }
  
  cout << "Teff OK " << endl ;
  
  //Calculate Et   
  Double_t EtV[100] ;
  TH2F *hGamV = (TH2F*)fileV->Get("h1");
  TH2F *hPipV = (TH2F*)fileV->Get("h2");
  TH2F *hPimV = (TH2F*)fileV->Get("h3");
  TH2F *hKpV = (TH2F*)fileV->Get("h4");
  TH2F *hPV = (TH2F*)fileV->Get("h3");
  
  
  for(Int_t j=1;j<=100;j++){
    cout << "j" << j << endl; 
    EtV[j-1]=0. ;
    TH1D * fSlix =  hGamV->ProjectionX("slixG",j,j) ;
    Double_t dp = fSlix->GetXaxis()->GetBinWidth(1) ;
    for(Int_t i=1;i<=fSlix->GetNbinsX();i++){
      Double_t pt = (i-0.5)*dp ;
      EtV[j-1]+=TMath::TwoPi()*pt*pt*dp*fSlix->GetBinContent(i) ;
    }
    fSlix =  hPipV->ProjectionX("slixPip",j,j) ;
    for(Int_t i=1;i<=fSlix->GetNbinsX();i++){
      Double_t pt = (i-0.5)*dp ;
      EtV[j-1]+=TMath::TwoPi()*pt*TMath::Sqrt(dp*dp+0.135*0.135)*dp*fSlix->GetBinContent(i) ;
    }
    fSlix =  hPimV->ProjectionX("slixPim",j,j) ;
    for(Int_t i=1;i<=fSlix->GetNbinsX();i++){
      Double_t pt = (i-0.5)*dp ;
      EtV[j-1]+=TMath::TwoPi()*pt*TMath::Sqrt(dp*dp+0.135*0.135)*dp*fSlix->GetBinContent(i) ;
    }
    fSlix =  hKpV->ProjectionX("slixKp",j,j) ;
    for(Int_t i=1;i<=fSlix->GetNbinsX();i++){
      Double_t pt = (i-0.5)*dp ;
      EtV[j-1]+=2.*TMath::TwoPi()*pt*TMath::Sqrt(dp*dp+0.495*0.495)*dp*fSlix->GetBinContent(i) ;
    }
    fSlix =  hPV->ProjectionX("slixPr",j,j) ;
    for(Int_t i=1;i<=fSlix->GetNbinsX();i++){
      Double_t pt = (i-0.5)*dp ;
      EtV[j-1]+=2.*TMath::TwoPi()*pt*TMath::Sqrt(dp*dp+0.938*0.938)*dp*fSlix->GetBinContent(i) ;
    }
  }
  
  
  //Find Et bins with minimal Chi2
  Int_t chiMinPi[11] ;
  Int_t chiMinK[11] ;
  Int_t chiMinPr[11] ;
  for(Int_t cen=0; cen<11;cen++){
    TString key("h") ;key+=(901+cen) ;
    TH1F * tmp =  (TH1F*)fileV->Get(key) ;
    chiMinPi[cen]=tmp->GetMinimumBin() ;
    if(chiMinPi[cen]<0)chiMinPi[cen]=0;
    if(chiMinPi[cen]>99)chiMinPi[cen]=99;
    key="h"; key+=(1901+cen) ;
    tmp =  (TH1F*)fileV->Get(key) ;
    chiMinK[cen]=tmp->GetMinimumBin() ;
    if(chiMinK[cen]<0)chiMinK[cen]=0;
    if(chiMinK[cen]>99)chiMinK[cen]=99;
    key="h"; key+=(2901+cen) ;
    tmp =  (TH1F*)fileV->Get(key) ;
    chiMinPr[cen]=tmp->GetMinimumBin() ;
    if(chiMinPr[cen]<0)chiMinPr[cen]=0;
    if(chiMinPr[cen]>99)chiMinPr[cen]=99;
  }
  

  //Read another variables
  Double_t Ein[100] ;
  Double_t EinAv[100] ;
  Double_t Tin[100] ;
  Double_t TinAv[100] ;
  Double_t Sin[100] ;
  Double_t SinAv[100] ;
  Double_t TotVol[100] ;
  Double_t dNdy[100] ;
  Double_t Et[100] ;
  ifstream in("sum_varvol_qgp_tau015_v00.dat") ;
  ofstream out("sumvarvol_qgp_tau015_v00.dat") ;  //for tau = 0.1 fm/c
  Int_t i ;
  in >> i >> Ein[i-1] >>EinAv[i-1] >>Tin[i-1] >>TinAv[i-1] 
     >>Sin[i-1] >>SinAv[i-1] >>TotVol[i-1] >> dNdy[i-1] >> Et[i-1] ;
  while(i<100){
    in >> i >> Ein[i-1] >>EinAv[i-1] >>Tin[i-1] >>TinAv[i-1] 
       >>Sin[i-1] >>SinAv[i-1] >>TotVol[i-1] >> dNdy[i-1] >> Et[i-1] ;
    
  }
  in.close() ;
  
  for(Int_t i=0;i<100;i++){
    cout << "i " << i << endl ;
    out <<  i << " " <<  Ein[i] << " " << EinAv[i] << " " << Tin[i] << " " << TinAv[i] 
	<< " " << Sin[i] << " " << SinAv[i] << " " << TotVol[i] << " " <<  dNdy[i] << " " <<  Et[i] << " " ;

    for(Int_t ir=0; ir<Nranges; ir++)
      out<< slopeV[i][ir] << " " <<  slopeErrV[i][ir] << " " ;
    out << endl ;
  }
  for(Int_t cen=0;cen<11;cen++)
    out << cen << " " << chiMinPi[cen] << " " << chiMinK[cen] << " " << chiMinPr[cen] << endl ;
  out.close() ;
  
  
}
