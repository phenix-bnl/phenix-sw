#include "makeHBDCalib.h"
TTree *fOutTree;
TCanvas *Canvas;
TPad *toppad,*botpad;
TCanvas *CanvasModAve;
TPad *toppadModAve,*botpadModAve;

Float_t gNominalSlope=-0.05;//From average of module 1
Float_t gNominalPeak =10;//From average of module 1

const Int_t gNModules=2;//South or north
const Int_t gNSectors=6;
const Int_t gNCells=96;
const Int_t gNArms=2;

TH1F hCharge[gNSectors][gNModules][gNCells][gNArms];
Float_t fSlope[gNSectors][gNModules][gNCells][gNArms];
Float_t feSlope[gNSectors][gNModules][gNCells][gNArms];
Float_t fPeak[gNSectors][gNModules][gNCells][gNArms];
TH1F hChargeModAve[gNSectors][gNModules][gNArms];
Float_t fSlopeModAve[gNSectors][gNModules][gNArms];
Float_t feSlopeModAve[gNSectors][gNModules][gNArms];
Float_t fPeakModAve[gNSectors][gNModules][gNArms];

TH1F *hIntAboveThresh[gNSectors][gNModules][gNArms];
TH1F *hGain[gNSectors][gNModules][gNArms];
TH1F *hShift[gNSectors][gNModules][gNArms];
TH1F *hIntAboveThreshModAve[gNArms];
TH1F *hGainModAve[gNArms];

Bool_t kSave_to_file = !kFALSE;
Bool_t kDrawAveOnly=kTRUE;

void makeHBDCalib()
{
  setup();
   
  process(0);//Preliminary fit for slope
  process(1);//Final fit for slope
  process(2);//Final AFTER-Calibration histograms
  
  if(!kDrawAveOnly) {
    drawIntegralAboveThreshold(0,1,0);
    drawFinalGain(0,1,0);}

  if(kSave_to_file){ writeTree(); }
}

void writeTree()
{
  fOutTree = new TTree("EventInfo","Calibration Info for HBD");
  Int_t fRunInfo[1] = {Int_t(l_RunInfo_run->GetValue()+0.5)};
  fOutTree->Branch("Info",fRunInfo,"Run/I");
  const Int_t f_ModAve = gNSectors*gNModules*gNArms;
  Char_t fLeafList[256];
  sprintf(fLeafList,"InvSlope[%d]/F:eInvSlope[%d]/F:sector[%d]/I:module[%d]/I:arm[%d]/I",f_ModAve,f_ModAve,f_ModAve,f_ModAve,f_ModAve);
  
  fOutTree->Branch("ModAve",0,fLeafList);
  Float_t fModAveInvSlope[f_ModAve];
  Float_t fModAveeInvSlope[f_ModAve];
  Int_t fModAveSector[f_ModAve];
  Int_t fModAveModule[f_ModAve];
  Int_t fModAveArm[f_ModAve];
  Int_t iCount=0;
  for(Int_t iSector=0 ; iSector<gNSectors ; iSector++) {
    for(Int_t iModule=0 ; iModule<gNModules ; iModule++) {
      for(Int_t iArm=0 ; iArm<gNArms ; iArm++) {
	fModAveArm[iCount] = iArm;
	fModAveSector[iCount] = iSector;
	fModAveModule[iCount] = iModule;
	fModAveInvSlope[iCount] = hGainModAve[iArm]->GetBinContent(iSector*gNModules+iModule+1);
	fModAveeInvSlope[iCount] = hGainModAve[iArm]->GetBinError(iSector*gNModules+iModule+1);
	iCount++;}}}
  TBranch *b = fOutTree->GetBranch("ModAve");
  ((TLeaf *) b->GetListOfLeaves()->At(0))->SetAddress(fModAveInvSlope);
  ((TLeaf *) b->GetListOfLeaves()->At(1))->SetAddress(fModAveeInvSlope);
  ((TLeaf *) b->GetListOfLeaves()->At(2))->SetAddress(fModAveSector);
  ((TLeaf *) b->GetListOfLeaves()->At(3))->SetAddress(fModAveModule);
  ((TLeaf *) b->GetListOfLeaves()->At(4))->SetAddress(fModAveArm);
  
  const Int_t f_Cells = gNSectors*gNModules*gNArms*gNCells;
  sprintf(fLeafList,"InvSlope[%d]/F:eInvSlope[%d]/F:sector[%d]/I:module[%d]/I:arm[%d]/I:cell[%d]/I",f_Cells,f_Cells,f_Cells,f_Cells,f_Cells,f_Cells);
  fOutTree->Branch("Cells",0,fLeafList);
  Float_t fCellsInvSlope[f_Cells];  Float_t fCellseInvSlope[f_Cells];
  Int_t fCellsSector[f_Cells];  Int_t fCellsModule[f_Cells];
  Int_t fCellsArm[f_Cells];  Int_t fCellsCell[f_Cells];
  Int_t iCount=0;
  for(Int_t iSector=0 ; iSector<gNSectors ; iSector++) {
    for(Int_t iModule=0 ; iModule<gNModules ; iModule++) {
      for(Int_t iCell=0 ; iCell<gNCells ; iCell++) {
	for(Int_t iArm=0 ; iArm<gNArms ; iArm++) {
	  fCellsArm[iCount] = iArm;
	  fCellsSector[iCount] = iSector;
	  fCellsModule[iCount] = iModule;
	  fCellsCell[iCount] = iCell;
	  fCellsInvSlope[iCount] = hGain[iSector][iModule][iArm]->GetBinContent(iCell+1);
	  fCellseInvSlope[iCount] = hGain[iSector][iModule][iArm]->GetBinError(iCell+1);
	  iCount++;}}}}
  b = fOutTree->GetBranch("Cells");
  ((TLeaf *) b->GetListOfLeaves()->At(0))->SetAddress(fCellsInvSlope);
  ((TLeaf *) b->GetListOfLeaves()->At(1))->SetAddress(fCellseInvSlope);
  ((TLeaf *) b->GetListOfLeaves()->At(2))->SetAddress(fCellsSector);
  ((TLeaf *) b->GetListOfLeaves()->At(3))->SetAddress(fCellsModule);
  ((TLeaf *) b->GetListOfLeaves()->At(4))->SetAddress(fCellsArm);
  ((TLeaf *) b->GetListOfLeaves()->At(5))->SetAddress(fCellsCell);
  
  fOutTree->Fill();


  TFile f(TString("HBD_Output_gain_parameters_run_")+fRunInfo[0]+".root","RECREATE");
  f.cd();
  fOutTree->Write();
  f.Close();
}

void setup()
{
  gROOT->SetStyle("Plain");
  gStyle->SetLabelSize(0.08,"X"); gStyle->SetLabelSize(0.08,"Y");
  gStyle->SetTitleSize(0.08,"X"); gStyle->SetTitleSize(0.08,"Y");
  gStyle->SetNdivisions(505,"X"); gStyle->SetNdivisions(505,"Y");
  
  gStyle->SetPalette(1);
  
  if(kDrawAveOnly!=kTRUE) { defineCanvas();}
    
  CanvasModAve = new TCanvas("CanvasModAve","HBD Calibrator: Module Average",0,0,500,500);
  CanvasModAve->Draw();
  CanvasModAve->cd();
  gPad->SetLogy();
  gPad->SetLeftMargin(0.2);
  gPad->SetBottomMargin(0.2);
  
  tree = new TChain("EventInfo");
  tree->Add("run_238165_ntuples_nocorr_avggain_th8_all.root");
  
  bookHistos();
}

void defineCanvas()
{
  Canvas = new TCanvas("Canvas","HBD Calibrator",0,0,1200,1200);
  Canvas->Draw();
  toppad = new TPad("toppad","",0.0,0.2,1.0,1.0);
  toppad->Draw();
  toppad->Divide(6,4,0.0025,0.005);
  for(Int_t i=0 ; i<24 ; i++) {
    toppad->cd(i+1);
    gPad->SetLogy();
    gPad->SetLeftMargin(0.2);
    gPad->SetBottomMargin(0.2);}
  Canvas->cd();
  botpad = new TPad("botpad","",0.0,0.0,1.0,0.2);
  botpad->Draw();
  botpad->Divide(2,1,0.0025,0.005);
  for(Int_t i=0 ; i<2 ; i++) {
    botpad->cd(i+1);
    gPad->SetLeftMargin(0.2/2);
    gPad->SetBottomMargin(0.2);
    gPad->SetRightMargin(0.1/3.);}
  Canvas->Update();
}

void fillHistos(Bool_t kFinalFit=kFALSE)
{  
  for(Int_t iCell=0 ; iCell<l__HBDCells->GetValue() ; iCell++) {
    Int_t fThisSector = l_HBDCells_sector->GetValue(iCell);
    Int_t fThisModule = l_HBDCells_module->GetValue(iCell);
    Int_t fThisPadNum = l_HBDCells_padnum->GetValue(iCell);
    Int_t fThisArm    = l_HBDCells_arm->GetValue(iCell);
    
    fThisPadNum = fThisPadNum-fThisModule*gNCells;
    if(fThisSector>=gNSectors || fThisPadNum>=gNCells) { continue; }

    Float_t fCorrCharge = l_HBDCells_charge->GetValue(iCell);
    fCorrCharge *= fSlope[fThisSector][fThisModule][fThisPadNum][fThisArm];
    
    if(kFinalFit) {
      fCorrCharge  = l_HBDCells_charge->GetValue(iCell);
      fCorrCharge -= fPeak[fThisSector][fThisModule][fThisPadNum][fThisArm];
      fCorrCharge *= fSlope[fThisSector][fThisModule][fThisPadNum][fThisArm];
      fCorrCharge += gNominalPeak;}

    Float_t fCorrChargeModAve = l_HBDCells_charge->GetValue(iCell);
    fCorrChargeModAve *= fSlopeModAve[fThisSector][fThisModule][fThisArm];
    
    if(kFinalFit) {
      fCorrChargeModAve  = l_HBDCells_charge->GetValue(iCell);
      fCorrChargeModAve -= fPeakModAve[fThisSector][fThisModule][fThisArm];
      fCorrChargeModAve *= fSlopeModAve[fThisSector][fThisModule][fThisArm];
      fCorrChargeModAve += gNominalPeak;}
    
    hCharge[fThisSector][fThisModule][fThisPadNum][fThisArm].Fill(fCorrCharge);
    hChargeModAve[fThisSector][fThisModule][fThisArm].Fill(fCorrChargeModAve);}
}

void resetHistos()
{
  for(Int_t i=0 ; i<gNSectors ; i++) {
    for(Int_t j=0 ; j<gNModules ; j++) {
      for(Int_t l=0 ; l<gNArms ; l++) {
	for(Int_t k=0 ; k<gNCells ; k++) {
	  hCharge[i][j][k][l].Reset(); }
	hChargeModAve[i][j][l].Reset(); }}}
}

void fitHistos(Int_t fPassNumber=0)
{//sets the slopes, fits in getslope()
  TF1 *fThisFit = NULL;
  for(Int_t i=0 ; i<gNSectors ; i++) {
    for(Int_t j=0 ; j<gNModules ; j++) {
      for(Int_t l=0 ; l<gNArms ; l++) {      
	for(Int_t k=0 ; k<gNCells ; k++) {
	  fThisFit = fitSlope(fPassNumber,i,j,k,l);
	  if(!fThisFit) { continue; }
	  fSlope[i][j][k][l]  = fSlope[i][j][k][l]*fThisFit->GetParameter(1);
	  feSlope[i][j][k][l]  = fSlope[i][j][k][l]*fThisFit->GetParError(1);
	  fSlope[i][j][k][l] /= gNominalSlope; 
	  feSlope[i][j][k][l]/= gNominalSlope; 
	  delete fThisFit;
	  if(fPeak[i][j][k][l]<0) {
	    fPeak[i][j][k][l] =
	      hCharge[i][j][k][l].GetBinCenter(hCharge[i][j][k][l].GetMaximumBin());
	  }}
	fThisFit = fitSlope(fPassNumber,i,j,-1,l);
	if(!fThisFit) { continue; }
	fSlopeModAve[i][j][l] = fSlopeModAve[i][j][l]*fThisFit->GetParameter(1);
	feSlopeModAve[i][j][l] = fSlopeModAve[i][j][l]*fThisFit->GetParError(1);
	fSlopeModAve[i][j][l] /= gNominalSlope; 
	feSlopeModAve[i][j][l] /= gNominalSlope; 
	delete fThisFit;
	if(fPeakModAve[i][j][l]<0) {
	  fPeakModAve[i][j][l] =
	    hChargeModAve[i][j][l].GetBinCenter(hChargeModAve[i][j][l].GetMaximumBin());}
      }}}
}

void normaliseHistos()
{
  Int_t fLowBin = hCharge[0][0][0][0].FindBin(20);
  Int_t fHighBin = hCharge[0][0][0][0].FindBin(30);

  for(Int_t i=0 ; i<gNSectors ; i++) {
    for(Int_t j=0 ; j<gNModules ; j++) {
      for(Int_t l=0 ; l<gNArms ; l++) {
	for(Int_t k=0 ; k<gNCells ; k++) {
	  Float_t fPeakHeight = hCharge[i][j][k][l].Integral(fLowBin,fHighBin);
	  if(fPeakHeight>0) {
	    hCharge[i][j][k][l].Scale(1./fPeakHeight); }}
	
	Float_t fPeakHeight = hChargeModAve[i][j][l].Integral(fLowBin,fHighBin);
	if(fPeakHeight>0) {
	  hChargeModAve[i][j][l].Scale(1./fPeakHeight); }
      }}}
}

TF1 *fitSlope(Int_t fPassNumber=0, 
	      Int_t fSector=1,Int_t fModule=0, Int_t fCell=0, Int_t fArm=0)
{
  Double_t fLowFitPoint=15.;
  Double_t fHighFitPoint=60.;
  if(fPassNumber==0) {
    fLowFitPoint=15; fHighFitPoint=30;}
  
  TF1 *fit = new TF1("fit","[0]*exp([1]*x)",fLowFitPoint,fHighFitPoint);
  fit->SetParameters(100,-0.05);

  if(fCell>=0) { hCharge[fSector][fModule][fCell][fArm].Fit("fit","RQ"); }
  else{ 
    Int_t hist_entries = hChargeModAve[fSector][fModule][fArm].GetEntries();
    
    if(hist_entries==0) return 0;
    hChargeModAve[fSector][fModule][fArm].Fit("fit","RQ");
    if(fPassNumber==0) {//This is the trial fit, let's see the output
      cout << " Arm: " << fArm
	   << " Sector: " << fSector
	   << " Module: " << fModule 
	   << " Gain=" << fit->GetParameter(1)
	   << " (preliminary fit)." << endl;}
    
    gPad->Update(); gSystem->Sleep(0*1000);}  
  
  return fit;
}

void process(Int_t fPassNumber=0)
{
  cout << "PASS: " << fPassNumber+1 << "/3" << endl;
  if(!kDrawAveOnly) {
    if(fPassNumber==0) { Canvas->SetTitle("HBD Calibrator: Preliminary Fit"); }
    else if(fPassNumber==1) { Canvas->SetTitle("HBD Calibrator: Slope Fit"); }
    else if(fPassNumber==2) { Canvas->SetTitle("HBD Calibrator: Calibrated"); }}
  resetHistos();
  Int_t fEvents = tree->GetEntries()*0.001;
  Int_t previous_tree=-1,current_tree=-1;
  for(Int_t iEvent=0 ; iEvent<fEvents ; iEvent++) {
    if(iEvent%(fEvents/10)==0 && iEvent>0) {
      cout << 10.*(iEvent/(fEvents/10)) << "% complete" << endl; }
    previous_tree = tree->GetTreeNumber();
    tree->GetEntry(iEvent);
    current_tree = tree->GetTreeNumber();
    if(previous_tree!=current_tree || iEvent==0) { initialize(); }
    fillHistos((fPassNumber!=0));}
  if(fPassNumber<2) { fitHistos(fPassNumber); }
  else {
    fillIntAboveThresh(10,fEvents);
    fillGain();
    fillShift();
    normaliseHistos(); }

  if(kDrawAveOnly==kTRUE) {
    if(fPassNumber==2) { drawModAve();}
    return; }

  drawModule(0,1,0);
}

void drawModule(Int_t fArm=0, Int_t fSector=1, Int_t fModule=0)
{
  if(!Canvas) { defineCanvas(); }
  //draw one module
  for(Int_t i=0 ; i<24 ; i++) {
    toppad->cd(i+1);
    TH1F *hFrame = gPad->DrawFrame(0,1.e-4,200,0.999e0);
    hFrame->GetXaxis()->SetTitle(hCharge[fSector][fModule][0+i*4][fArm].GetXaxis()->GetTitle());
    if(hCharge[fSector][fModule][0+i*4][fArm].GetMaximum()>2) {
      hCharge[fSector][fModule][0+i*4][fArm].Draw("");}
    else {
      hCharge[fSector][fModule][0+i*4][fArm].Draw("same");}
    hCharge[fSector][fModule][1+i*4][fArm].Draw("same");
    hCharge[fSector][fModule][2+i*4][fArm].Draw("same");
    hCharge[fSector][fModule][3+i*4][fArm].Draw("same");
    drawLabels(fSector,fModule,0+i*4,fArm); }

  drawIntegralAboveThreshold();
  drawFinalGain();

  gPad->Update();
}

void drawLabels(Int_t fSector=1,Int_t fModule=0, Int_t fCell=0, Int_t fArm=0)
{
  //*******************
  // Label drawer for the big cell-by-cell display
  //*******************
  TString fTitle="Sector: ";
  fTitle = fTitle+fSector+" ; Module: "+fModule+" ; Arm "+fArm+" ; Cells ";
  TLatex *l = new TLatex(-40,1.06,fTitle);
  l->Draw();
  
  fTitle="";
  for(Int_t i=0 ; i<4 ; i++) {
    TLatex *l = new TLatex(180+i*15.,1.06,fTitle+(fCell+i));
    l->SetTextColor(i+1);
    l->Draw();}
}

void fillIntAboveThresh(Float_t fThreshold = 10, Float_t fEvents=1)
{
  //*******************
  // Calculates the integral above a theshold
  //*******************
  Int_t fThresholdBin = hCharge[0][0][0][0].FindBin(fThreshold);
  Int_t fMaxBin = hCharge[0][0][0][0].GetNbinsX();
  for(Int_t l=0 ; l<gNArms ; l++) {
    for(Int_t i=0 ; i<gNSectors ; i++) {
      for(Int_t j=0 ; j<gNModules ; j++) {
	for(Int_t k=0 ; k<gNCells ; k++) {
	  Float_t fIntegral = hCharge[i][j][k][l].Integral(fThresholdBin,fMaxBin);
	  
	  if(fEvents!=0) hIntAboveThresh[i][j][l]->SetBinContent(k+1,fIntegral/fEvents);}
  
	Float_t fIntegral = hChargeModAve[i][j][l].Integral(fThresholdBin,fMaxBin);
	if(fEvents!=0) hIntAboveThreshModAve[l]->SetBinContent(i*gNModules+j+1,fIntegral/fEvents);}}}
}

void fillGain()
{
  //*******************
  // This fills the final gain histograms, ready of display
  //*******************
  for(Int_t l=0 ; l<gNArms ; l++) {
    for(Int_t i=0 ; i<gNSectors ; i++) {
      for(Int_t j=0 ; j<gNModules ; j++) {
	for(Int_t k=0 ; k<gNCells ; k++) {
	  if(fSlope[i][j][k][l]!=1) {//Only fill if there is a measured slope
	    hGain[i][j][l]->SetBinContent(k+1,fSlope[i][j][k][l]*gNominalSlope);	    hGain[i][j][l]->SetBinError(k+1,feSlope[i][j][k][l]*gNominalSlope);}}
	if(fSlopeModAve[i][j][l]!=1) {//Only fill if there is a measured slope
	  hGainModAve[l]->SetBinContent(i*gNModules+j+1,fSlopeModAve[i][j][l]*gNominalSlope);
	  hGainModAve[l]->SetBinError(i*gNModules+j+1,feSlopeModAve[i][j][l]*gNominalSlope); }}}}
}

void fillShift()
{
  //*******************
  // This fills the peak position histograms
  //*******************
  for(Int_t i=0 ; i<gNSectors ; i++) {
    for(Int_t j=0 ; j<gNModules ; j++) {
      for(Int_t l=0 ; l<gNArms ; l++) {
	for(Int_t k=0 ; k<gNCells ; k++) {
	  hShift[i][j][l]->SetBinContent(k,fPeak[i][j][k][l]-gNominalPeak);}}}}
}

void drawIntegralAboveThreshold(Int_t fArm=0, Int_t fSector=1, Int_t fModule=0)
{
  //*******************
  // Draws the integral above a theshold
  // The number in (Integral>Number) is set via fillIntAboveThresh()
  //*******************
  botpad->cd(1);
  TH1F *hFrame = gPad->DrawFrame(0,0,96,0.60);
  hFrame->GetXaxis()->SetTitle("Pad Number");
  hFrame->GetYaxis()->SetTitle("(Integral>10)/Event");
  hFrame->GetYaxis()->SetTitleOffset(0.5);

  hIntAboveThresh[fSector][fModule][fArm]->Draw("same");
}

void drawFinalGain(Int_t fArm=0, Int_t fSector=1, Int_t fModule=0)
{
  //*******************
  // Draws the gain for each cell versus cell number
  // Cell numbers <=96 are in "module==0", >96 are in "module==1"
  //*******************
  botpad->cd(2);
  TH1F *hFrame = gPad->DrawFrame(0,0,96,5);
  hFrame->GetXaxis()->SetTitle("Pad Number");
  hFrame->GetYaxis()->SetTitle("Gain (Slope)");
  hFrame->GetYaxis()->SetTitleOffset(0.5);

  hGain[fSector][fModule][fArm]->Draw("same");
}

void bookHistos()
{
  //*******************
  // Create all the histograms
  //*******************
  TString fName = "hCharge";
  TString fNameInt = "hIntAboveThresh";
  TString fNameGain = "hGain";
  TString fNameShift = "hShift";
  for(Int_t l=0 ; l<gNArms ; l++) {
    for(Int_t i=0 ; i<gNSectors ; i++) {
      for(Int_t j=0 ; j<gNModules ; j++) {
	for(Int_t k=0 ; k<gNCells ; k++) {
	  hCharge[i][j][k][l] = new TH1F(fName+"_"+i+"_"+j+"_"+l+"_"+k,"",100,0,200);
	  hCharge[i][j][k][l].SetStats(0);
	  hCharge[i][j][k][l].Sumw2();
	  hCharge[i][j][k][l].SetLineColor(k+1-4*(k/4));
	  hCharge[i][j][k][l].SetMarkerColor(k+1-4*(k/4));
	  hCharge[i][j][k][l].GetXaxis()->SetTitle("Cell Charge");
	  
	  fSlope[i][j][k][l] = 1;
	  feSlope[i][j][k][l]= 0;
	  fPeak[i][j][k][l] = -1;}
	
	hIntAboveThresh[i][j][l] = new TH1F(fNameInt+"_"+i+"_"+j+"_"+l,"",gNCells,0,gNCells);
	hIntAboveThresh[i][j][l]->SetStats(0);
	hGain[i][j][l] = new TH1F(fNameGain+"_"+i+"_"+j+"_"+l,"",gNCells,0,gNCells);
	hGain[i][j][l]->SetStats(0);
	hShift[i][j][l] = new TH1F(fNameShift+"_"+i+"_"+j+"_"+l,"",gNCells,0,gNCells);
	hShift[i][j][l]->SetStats(0);
	
	//Sector-Averaged Histograms
	hChargeModAve[i][j][l] = new TH1F(fName+"ModAve_"+i+"_"+j+"_"+l,"",100,0,200);
	hChargeModAve[i][j][l].SetStats(0);
	hChargeModAve[i][j][l].Sumw2();
	hChargeModAve[i][j][l].SetLineColor(i+1-3*(i/3));
	hChargeModAve[i][j][l].SetMarkerColor(i+1-3*(i/3));
	hChargeModAve[i][j][l].GetXaxis()->SetTitle("Cell Charge");
	hChargeModAve[i][j][l].GetXaxis()->SetTitleSize(0.04);
	hChargeModAve[i][j][l].GetXaxis()->SetTitleOffset(1.6);
	hChargeModAve[i][j][l].GetXaxis()->SetLabelSize(0.04);
	hChargeModAve[i][j][l].GetYaxis()->SetTitle("Counts");
	hChargeModAve[i][j][l].GetYaxis()->SetTitleSize(0.04);
	hChargeModAve[i][j][l].GetYaxis()->SetTitleOffset(1.8);
	hChargeModAve[i][j][l].GetYaxis()->SetLabelSize(0.04);
	
	fSlopeModAve[i][j][l] = 1;
	feSlopeModAve[i][j][l]= 0;
	fPeakModAve[i][j][l] = -1;
      }}
    hIntAboveThreshModAve[l] = new TH1F(fNameInt+"ModAve_"+l,"",gNModules*gNSectors,0,gNModules*gNSectors);
    hIntAboveThreshModAve[l]->SetStats(0);
    hGainModAve[l] = new TH1F(fNameGain+"ModAve_"+l,"",gNModules*gNSectors,0,gNModules*gNSectors);
    hGainModAve[l]->SetStats(0);
  }
}

void drawModAve()
{
  //*******************
  // Draws the summary Canvas for the module averaged analysis
  //*******************
  if(CanvasModAve) { delete CanvasModAve; }
  CanvasModAve = new TCanvas("Canvas","HBD Calibrator: Module Average",0,0,1200,1200);
  CanvasModAve->Draw();
  toppadModAve = new TPad("toppadModAve","",0.0,0.5,1.0,1.0);
  toppadModAve->Draw();
  toppadModAve->Divide(2,1,0.0025,0.005);
 
  for(Int_t i=0 ; i<2 ; i++) {
    toppadModAve->cd(i+1);
    gPad->SetLogy();
    gPad->SetLeftMargin(0.2);
    gPad->SetBottomMargin(0.2);
  }
  
  CanvasModAve->cd();
  botpadModAve = new TPad("botpadModAve","",0.0,0.0,1.0,0.5);
  botpadModAve->Draw();
  botpadModAve->Divide(2,2,0.0025,0.005);
  for(Int_t i=0 ; i<4 ; i++) {
    botpadModAve->cd(i+1);
    gPad->SetLeftMargin(0.2);
    gPad->SetBottomMargin(0.2);
    gPad->SetRightMargin(0.1);}
  CanvasModAve->Update();
 
  TString arm_name[2]={"East Arm (0)","West Arm (1)"}; 
  for(Int_t iArm=0 ; iArm<2 ; iArm++) {
    toppadModAve->cd(iArm+1);
    gPad->SetTicks();
    TH1F *hFrame = gPad->DrawFrame(0,1.e-4,200,0.999e0);
    hFrame->GetXaxis()->SetTitle("Cell Charge");
    hFrame->GetXaxis()->SetTitleSize(0.04);
    hFrame->GetXaxis()->SetTitleOffset(1.6);
    hFrame->GetXaxis()->SetLabelSize(0.04);
    hFrame->GetYaxis()->SetLabelSize(0.04);
    TLatex*t= new TLatex(56,1.5,arm_name[iArm]);
    t->Draw("");
  
    for(Int_t i=0 ; i<gNSectors ; i++) {
      for(Int_t j=0 ; j<gNModules ; j++) {
	if(hChargeModAve[i][j][iArm].GetEntries()) {
	  hChargeModAve[i][j][iArm].Draw("same"); }}}}
  
  for(Int_t i=0 ; i<2 ; i++) {
    botpadModAve->cd(i+1);
    gPad->SetTicks();
    TH1F *hFrame = gPad->DrawFrame(0,-0.15,gNModules*gNSectors,0.0);
    hFrame->GetXaxis()->SetNdivisions(502);
    hFrame->GetXaxis()->SetTitle("module");
    hFrame->GetXaxis()->SetTitleOffset(1000);
    hFrame->GetXaxis()->SetLabelOffset(1000);
    hFrame->GetYaxis()->SetTitle("Gain (Inverse Slope)");
    hGainModAve[i]->Draw("same");
    
    TLatex *latex = new TLatex(-1.5,-0.165,"module:");
    latex->SetTextColor(4);latex->Draw();
    latex = new TLatex(-1.5,-0.175,"sector:");
    latex->SetTextColor(2);latex->Draw();
    for(Int_t iSector=0 ; iSector<gNSectors ; iSector++) {
      latex = new TLatex(iSector*2+0.5,-0.165,"0");
      latex->SetTextColor(4);latex->Draw();
      latex = new TLatex(iSector*2+1.5,-0.165,"1");
      latex->SetTextColor(4);latex->Draw();
      latex = new TLatex(iSector*2+0.85,-0.175,TString("")+iSector);
      latex->SetTextSize(0.1); latex->SetTextColor(2);
      latex->Draw();}
    
    botpadModAve->cd(i+3);
    gPad->SetTicks();
    hFrame = gPad->DrawFrame(0,0,gNModules*gNSectors,50.0);
    hFrame->GetXaxis()->SetNdivisions(502);
    hFrame->GetXaxis()->SetTitle("module");
    hFrame->GetXaxis()->SetTitleOffset(1000);
    hFrame->GetXaxis()->SetLabelOffset(1000);
    hFrame->GetYaxis()->SetTitle("(Integral>10)/Event");
    hIntAboveThreshModAve[i]->Draw("same");
    TLatex *latex = new TLatex(-1.5,-5.0,"module:");
    latex->SetTextColor(4);latex->Draw();
    latex = new TLatex(-1.5,-8.33,"sector:");
    latex->SetTextColor(2);latex->Draw();
    for(Int_t iSector=0 ; iSector<gNSectors ; iSector++) {
      latex = new TLatex(iSector*2+0.5,-5.0,"0");
      latex->SetTextColor(4);latex->Draw();
      latex = new TLatex(iSector*2+1.5,-5.0,"1");
      latex->SetTextColor(4);latex->Draw();
      latex = new TLatex(iSector*2+0.85,-8.33,TString("")+iSector);
      latex->SetTextSize(0.1); latex->SetTextColor(2);
      latex->Draw();}
  }
  
  
  
  for(Int_t l=0 ; l<gNArms ; l++) {
    for(Int_t i=0 ; i<gNSectors ; i++) {
      for(Int_t j=0 ; j<gNModules ; j++) {
	if(fSlopeModAve[i][j][l]!=1 && fPeakModAve[i][j][l]!=1) {
	  cout << " Arm: " << l
	       << " Sector: " << i
	       << " Module: " << j 
	       << " Gain=" << fSlopeModAve[i][j][l]*gNominalSlope 
	       << " Peak=" << fPeakModAve[i][j][l] << endl;}}}
  }
}
