#include "SaveCanvas.C"




void
ResidFromProfile_hvh10()
{
  // change: fit to all 4 points
  gStyle->SetOptFit(); // show fit results
  gStyle->SetOptStat(0); // do not show stats (meaningless anyway)

//  TFile *_file0 = TFile::Open("data/367607/367607.FileList.cp1.fvtx_eval.root");
//  TFile *_file0 = TFile::Open("/phenix/u/jinhuang/links/spin_data/fvtx_test_data/miliped_work/GOLDENEVENT_DIMUON_run12_online_muon-run_0000367000_0000368000.RunList.fvtx_eval.root");
//  TFile *_file0 =
//      TFile::Open(
//          "/phenix/u/jinhuang/links/spin_data/fvtx_test_data/miliped_work/GOLDENEVENT_DIMUON_run12_online_muon-run_0000367000_0000368000.RunList.Fun4Muons_RecoPRDF_Test.fvtx_eval.root");
    TFile *_file0 = TFile::Open("data/miliped_work/Iter2/GOLDENEVENT_DIMUON_run12_online_muon-run_0000367000_0000368000.RunList.Fun4Muons_RecoPRDF_Test.fvtx_eval.root");


  p1=new TProfile("p1","North LSQ all->0 Residual vs. Phi;Phi (degrees);Residual Sigma (cm)",48, -90, 270.,"s");
  p2=new TProfile("p2","North LSQ all->1 Residual vs. Phi;Phi (degrees);Residual Sigma (cm)",48, -90, 270.,"s");
  p3=new TProfile("p3","North LSQ all->2 Residual vs. Phi;Phi (degrees);Residual Sigma (cm)",48, -90, 270.,"s");
  p4=new TProfile("p4","North LSQ all->3 Residual vs. Phi;Phi (degrees);Residual Sigma (cm)",48, -90, 270.,"s");
  // TCut projcut="sector1==sector2&&sector2==sector4";
     TCut projcut="(r1meas+(r4meas-r1meas)/(z4reco-z1reco)*(z2reco-z1reco)-r2meas)>-0.00 && (r1meas+(r4meas-r1meas)/(z4reco-z1reco)*(z2reco-z1reco)-r2meas)<.01&&sector1==sector2&&sector2==sector4";


  // st1:
 trk_eval->Draw("r1meas - z1meas*((4*(z1meas*r1meas+z2meas*r2meas+z3meas*r3meas+z4meas*r4meas)-(z1meas+z2meas+z3meas+z4meas)*(r1meas+r2meas+r3meas+r4meas))/(4*(z1meas**2+z2meas**2+z3meas**2+z4meas**2)-(z1meas+z2meas+z3meas+z4meas)**2)) - ((r1meas+r2meas+r3meas+r4meas)*(z1meas**2+z2meas**2+z3meas**2+z4meas**2)-(z1meas+z2meas+z3meas+z4meas)*(z1meas*r1meas+z2meas*r2meas+z3meas*r3meas+z4meas*r4meas))/(4*(z1meas**2+z2meas**2+z3meas**2+z4meas**2)-(z1meas+z2meas+z3meas+z4meas)**2):((phi3meas*57.3+270)%360-90)>>p1","r3meas>0&&r4meas>0&&r2meas>0&&arm==1&&abs(z0reco)<10&&z3reco>0&&size3<3"&&projcut,"prof");

 // st2:
 trk_eval->Draw("r2meas - z2meas*((4*(z1meas*r1meas+z2meas*r2meas+z3meas*r3meas+z4meas*r4meas)-(z1meas+z2meas+z3meas+z4meas)*(r1meas+r2meas+r3meas+r4meas))/(4*(z1meas**2+z2meas**2+z3meas**2+z4meas**2)-(z1meas+z2meas+z3meas+z4meas)**2)) - ((r1meas+r2meas+r3meas+r4meas)*(z1meas**2+z2meas**2+z3meas**2+z4meas**2)-(z1meas+z2meas+z3meas+z4meas)*(z1meas*r1meas+z2meas*r2meas+z3meas*r3meas+z4meas*r4meas))/(4*(z1meas**2+z2meas**2+z3meas**2+z4meas**2)-(z1meas+z2meas+z3meas+z4meas)**2):((phi3meas*57.3+270)%360-90)>>p2","r3meas>0&&r4meas>0&&r2meas>0&&arm==1&&abs(z0reco)<10&&z3reco>0&&z3reco>-99&&size3<3"&&projcut,"prof");

 // st3:
 trk_eval->Draw("r3meas - z3meas*((4*(z1meas*r1meas+z2meas*r2meas+z3meas*r3meas+z4meas*r4meas)-(z1meas+z2meas+z3meas+z4meas)*(r1meas+r2meas+r3meas+r4meas))/(4*(z1meas**2+z2meas**2+z3meas**2+z4meas**2)-(z1meas+z2meas+z3meas+z4meas)**2)) - ((r1meas+r2meas+r3meas+r4meas)*(z1meas**2+z2meas**2+z3meas**2+z4meas**2)-(z1meas+z2meas+z3meas+z4meas)*(z1meas*r1meas+z2meas*r2meas+z3meas*r3meas+z4meas*r4meas))/(4*(z1meas**2+z2meas**2+z3meas**2+z4meas**2)-(z1meas+z2meas+z3meas+z4meas)**2):((phi3meas*57.3+270)%360-90)>>p3","r3meas>0&&r4meas>0&&r2meas>0&&arm==1&&abs(z0reco)<10&&z3reco>0&&z3reco>-99&&size3<3"&&projcut,"prof");

 // st4:
 trk_eval->Draw("r4meas - z4meas*((4*(z1meas*r1meas+z2meas*r2meas+z3meas*r3meas+z4meas*r4meas)-(z1meas+z2meas+z3meas+z4meas)*(r1meas+r2meas+r3meas+r4meas))/(4*(z1meas**2+z2meas**2+z3meas**2+z4meas**2)-(z1meas+z2meas+z3meas+z4meas)**2)) - ((r1meas+r2meas+r3meas+r4meas)*(z1meas**2+z2meas**2+z3meas**2+z4meas**2)-(z1meas+z2meas+z3meas+z4meas)*(z1meas*r1meas+z2meas*r2meas+z3meas*r3meas+z4meas*r4meas))/(4*(z1meas**2+z2meas**2+z3meas**2+z4meas**2)-(z1meas+z2meas+z3meas+z4meas)**2):((phi3meas*57.3+270)%360-90)>>p4","r3meas>0&&r4meas>0&&r2meas>0&&arm==1&&abs(z0reco)<10&&z3reco>0&&z3reco>-99&&size3<3"&&projcut,"prof");



  TCanvas *c1 = new TCanvas("c1","title",800,800);
  c1->Divide(2,2);

  line1 = new TLine(90,-0.03,90,0.03);
  line1->SetLineWidth(2);
  line1->SetLineColor(2);
  Float_t x; Float_t y; Float_t tmp;
  float bigerror; bigerror = 0.019;

  float fval;
  TString cval;
  ostringstream convert1_1, convert1_2;         ///make a stream variable to hold the converted number
  ostringstream convert2_1, convert2_2;
  ostringstream convert3_1, convert3_2;
  ostringstream convert4_1, convert4_2;

  //----------------- 1 ---------------------------------------------------------
  c1_1->cd();
  TF1 *myfit1_1=new TF1("myfit1_1","[0]+[1]*sin(1.57+x*3.14159/180)");
  myfit1_1->SetRange(-90,90);
  TF1 *myfit1_2=new TF1("myfit1_2","[0]+[1]*sin(1.57-x*3.14159/180)");
  myfit1_1->SetRange(-90,90);
  // myfit1_2->SetParameter(1,0.0.001);
  TH1D *proj = p1->ProjectionX();       // station 1:
  for (Int_t i=1; i<=48; i++) {
    tmp = p1_px->GetBinError(i);
    if (tmp<0.0002) {
      p1_px->SetBinError(i,0.02);
      }
  }
  p1_px->SetMinimum(-0.04);
  p1_px->SetMaximum(+0.04);
  p1_px->Fit("myfit1_1","","",-90,90);    // fit St 1 left
  p1_px->Fit("myfit1_2","","",90,270);    // fit St 1 right
  line1->Draw();
  myfit1_1->Draw("same");
  TText *t1 = new TText(-40,-0.03,"NE0"); t1->Draw();
  TText *t2 = new TText(180,-0.03,"NW0"); t2->Draw();

  for (Int_t i=1; i<=48; i++) {
    if (  p1_px->GetBinError(i) > bigerror) {
      x = p1_px->GetBinCenter(i);
      y = p1_px->GetBinContent(i);
      line3 = new TLine(x,y-0.02,x,y+0.02);
      line3->SetLineColor(0);
      line3->Draw();
      }
  }
  myfit1_1->Draw("same");
  myfit1_2->Draw("same");

  fval = 10000*myfit1_1->GetParameter(1);
  convert1_1 <<"shift: "<< fval<<" um";     //give your number to the converter variable
  cval = convert1_1.str();                  //now set your TString to the converter variable
  TText *t3 = new TText(-60,-0.035,cval);
  t3->SetTextSize(0.04); t3->Draw();


  fval = 10000*myfit1_2->GetParameter(1);
  convert1_2 <<"shift: "<< fval<<" um";
  cval = convert1_2.str();
  TText *t4 = new TText(120,-0.035,cval);
  t4->SetTextSize(0.04); t4->Draw();


  //----------------- 2 -------------------------------------------

 c1_2->cd();
  TF1 *myfit2_1=new TF1("myfit2_1","[0]+[1]*sin(1.57+x*3.14159/180)");
  myfit2_1->SetRange(-90,90);
  TF1 *myfit2_2=new TF1("myfit2_2","[0]+[1]*sin(1.57-x*3.14159/180)");
  myfit2_2->SetRange(90,270);
  TH1D *proj = p2->ProjectionX();      // station 2:
  for (Int_t i=1; i<=48; i++) {
    tmp = p2_px->GetBinError(i);
    if (tmp<0.0002) {
      p2_px->SetBinError(i,0.02);
      }
  }
  p2_px->SetMinimum(-0.04);
  p2_px->SetMaximum(+0.04);
  p2_px->Fit("myfit2_1","","",-90,90);    // fit St 2 left
  p2_px->Fit("myfit2_2","","",90,270);    // fit St 2 right
  line1->Draw();
  myfit2_1->Draw("same");
  TText *t1 = new TText(-40,-0.03,"NE1"); t1->Draw();
  TText *t2 = new TText(180,-0.03,"NW1"); t2->Draw();

  for (Int_t i=1; i<=48; i++) {
    if (  p2_px->GetBinError(i) > bigerror) {
      x = p2_px->GetBinCenter(i);
      y = p2_px->GetBinContent(i);
      line3 = new TLine(x,y-0.02,x,y+0.02);
      line3->SetLineColor(0);
      line3->Draw();
      }
  }
  myfit2_1->Draw("same");
  myfit2_2->Draw("same");

  fval = 10000*myfit2_1->GetParameter(1);
  convert2_1 <<"shift: "<< fval<<" um";     ////give your number to the converter variable
  cval = convert2_1.str();   ////now set your TString to the converter variable
  TText *t3 = new TText(-60,-0.035,cval);
  t3->SetTextSize(0.04); t3->Draw();


  fval = 10000*myfit2_2->GetParameter(1);
  convert2_2 <<"shift: "<< fval<<" um";     ////give your number to the converter variable
  cval = convert2_2.str();   ////now set your TString to the converter variable
  TText *t4 = new TText(120,-0.035,cval);
  t4->SetTextSize(0.04); t4->Draw();

  //------------- 3 ---------------------------------------------------

  c1_3->cd();
  TF1 *mrfit=new TF1("myfit3_1","[0]+[1]*sin(1.57+x*3.14159/180)");
  myfit3_1->SetRange(-90,90);
  TF1 *mrfit=new TF1("myfit3_2","[0]+[1]*sin(1.57-x*3.14159/180)");
  myfit3_2->SetRange(90,270);

  TH1D *proj = p3->ProjectionX();      // station 3:
  for (Int_t i=1; i<=48; i++) {
    tmp = p3_px->GetBinError(i);
    if (tmp<0.00055) {
      p3_px->SetBinError(i,0.02);
      }
  }
  p3_px->SetMinimum(-0.04);
  p3_px->SetMaximum(+0.04);
  p3_px->Fit("myfit3_1","","",-90,90);    // fit St 3 left
  p3_px->Fit("myfit3_2","","",90,270);    // fit St 3 right
  line1->Draw();
  myfit3_1->Draw("same");
  TText *t1 = new TText(-40,-0.03,"NE2"); t1->Draw();
  TText *t2 = new TText(180,-0.03,"NW2"); t2->Draw();

  for (Int_t i=1; i<=48; i++) {
    if (  p3_px->GetBinError(i) > bigerror) {
      x = p3_px->GetBinCenter(i);
      y = p3_px->GetBinContent(i);
      line3 = new TLine(x,y-0.02,x,y+0.02);
      line3->SetLineColor(0);
      line3->Draw();
      }
  }
  myfit3_1->Draw("same");
  myfit3_2->Draw("same");

  fval = 10000*myfit3_1->GetParameter(1);
  convert3_1 <<"shift: "<< fval<<" um";     ////give your number to the converter variable
  cval = convert3_1.str();   ////now set your TString to the converter variable
  TText *t3 = new TText(-60,-0.035,cval);
  t3->SetTextSize(0.04); t3->Draw();


  fval = 10000*myfit3_2->GetParameter(1);
  convert3_2 <<"shift: "<< fval<<" um";     ////give your number to the converter variable
  cval = convert3_2.str();   ////now set your TString to the converter variable
  TText *t4 = new TText(120,-0.035,cval);
  t4->SetTextSize(0.04); t4->Draw();

  //--------------------------------- 4 -------------------------------

  c1_4->cd();
  TF1 *mrfit=new TF1("myfit4_1","[0]+[1]*sin(1.57+x*3.14159/180)");
  myfit4_1->SetRange(-90,90);
  TF1 *mrfit=new TF1("myfit4_2","[0]+[1]*sin(1.57-x*3.14159/180)");
  myfit4_2->SetRange(90,270);
  TH1D *proj = p4->ProjectionX();      // st 4:
  for (Int_t i=1; i<=48; i++) {
    tmp = p4_px->GetBinError(i);
    if (tmp<0.00036) {
      p4_px->SetBinError(i,0.02);
      }
  }
  p4_px->SetMinimum(-0.04);
  p4_px->SetMaximum(+0.04);
  p4_px->Fit("myfit4_1","","",-90,90);   // fit St 4 left
  p4_px->Fit("myfit4_2","","",90,270);    // fit St 4 right
  line1->Draw();
  myfit4_1->Draw("same");
  TText *t1 = new TText(-40,-0.03,"NE3"); t1->Draw();
  TText *t2 = new TText(180,-0.03,"NW3"); t2->Draw();

  for (Int_t i=1; i<=48; i++) {
    if ( p4_px->GetBinError(i) > bigerror) {
      x = p4_px->GetBinCenter(i);
      y = p4_px->GetBinContent(i);
      line3 = new TLine(x,y-0.02,x,y+0.02);
      line3->SetLineColor(0);
      line3->Draw();
      }
  }
  myfit4_1->Draw("same");
  myfit4_2->Draw("same");


  fval = 10000*myfit4_1->GetParameter(1);
  convert4_1 <<"shift: "<< fval<<" um";     ////give your number to the converter variable
  cval = convert4_1.str();   ////now set your TString to the converter variable
  TText *t3 = new TText(-60,-0.035,cval);
  t3->SetTextSize(0.04); t3->Draw();


  fval = 10000*myfit4_2->GetParameter(1);
  convert4_2 <<"shift: "<< fval<<" um";     ////give your number to the converter variable
  cval = convert4_2.str();   ////now set your TString to the converter variable
  TText *t4 = new TText(120,-0.035,cval);
  t4->SetTextSize(0.04); t4->Draw();

  //---------------------------------------------------------------------------

  c1_1->cd();
  TText *t3 = new TText(-90,-0.05,"Resid...hvh10.C");
  t3->SetTextSize(0.04);
  t3->Draw();

  SaveCanvas(c1,TString(_file0->GetName()) + "_" + c1->GetTitle(),kFALSE);

}
