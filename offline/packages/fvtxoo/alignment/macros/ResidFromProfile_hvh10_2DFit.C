#include "SaveCanvas.C"
#include <iostream>
#include <TText.h>
using namespace std;

void
ResidFromProfile_hvh10_2DFit(void)
{
  // change: fit to all 4 points
  gStyle->SetOptFit(); // show fit results
  gStyle->SetOptStat(0); // do not show stats (meaningless anyway)

//  TFile *_file0 = TFile::Open("data/367607/367607.FileList.cp1.fvtx_eval.root");
//  TFile *_file0 = TFile::Open("/phenix/u/jinhuang/links/spin_data/fvtx_test_data/miliped_work/GOLDENEVENT_DIMUON_run12_online_muon-run_0000367000_0000368000.RunList.fvtx_eval.root");
//  TFile *_file0 =
//      TFile::Open(
//          "/phenix/u/jinhuang/links/spin_data/fvtx_test_data/miliped_work/Iter1/GOLDENEVENT_DIMUON_run12_online_muon-run_0000367000_0000368000.RunList.Fun4Muons_RecoPRDF_Test.fvtx_eval.root");
//  TFile *_file0 = TFile::Open("data/miliped_work/Iter2/GOLDENEVENT_DIMUON_run12_online_muon-run_0000367000_0000368000.RunList.Fun4Muons_RecoPRDF_Test.fvtx_eval.root");
//
//  TFile *_file0 = TFile::Open("data/miliped_work/100kGolden_ZeroShift/RunList.Fun4Muons_RecoPRDF_Test.fvtx_eval.root");
  TFile *_file0 = TFile::Open("data/miliped_work/100kGolden_200umShift/RunList.Fun4Muons_RecoPRDF_Test.fvtx_eval.root");

  TH2F * p1 = new TH2F("p1",
      "North LSQ all->0 Residual vs. Phi;Phi (degrees);Residual Sigma (cm)", 48,
      -90, 270, 1000, -.1, .1);
  TH2F * p2 = new TH2F("p2",
      "North LSQ all->1 Residual vs. Phi;Phi (degrees);Residual Sigma (cm)", 48,
      -90, 270, 1000, -.1, .1);
  TH2F * p3 = new TH2F("p3",
      "North LSQ all->2 Residual vs. Phi;Phi (degrees);Residual Sigma (cm)", 48,
      -90, 270, 1000, -.1, .1);
  TH2F * p4 = new TH2F("p4",
      "North LSQ all->3 Residual vs. Phi;Phi (degrees);Residual Sigma (cm)", 48,
      -90, 270, 1000, -.1, .1);

  // TCut projcut="sector1==sector2&&sector2==sector4";
  TCut projcut =
      "(r1meas+(r4meas-r1meas)/(z4reco-z1reco)*(z2reco-z1reco)-r2meas)>-0.00 && (r1meas+(r4meas-r1meas)/(z4reco-z1reco)*(z2reco-z1reco)-r2meas)<.01&&sector1==sector2&&sector2==sector4";

  cout << "// st1:" << endl;
  trk_eval->Draw(
      "r1meas - z1meas*((4*(z1meas*r1meas+z2meas*r2meas+z3meas*r3meas+z4meas*r4meas)-(z1meas+z2meas+z3meas+z4meas)*(r1meas+r2meas+r3meas+r4meas))/(4*(z1meas**2+z2meas**2+z3meas**2+z4meas**2)-(z1meas+z2meas+z3meas+z4meas)**2)) - ((r1meas+r2meas+r3meas+r4meas)*(z1meas**2+z2meas**2+z3meas**2+z4meas**2)-(z1meas+z2meas+z3meas+z4meas)*(z1meas*r1meas+z2meas*r2meas+z3meas*r3meas+z4meas*r4meas))/(4*(z1meas**2+z2meas**2+z3meas**2+z4meas**2)-(z1meas+z2meas+z3meas+z4meas)**2):((phi3meas*57.3+270)%360-90)>>p1",
      "r3meas>0&&r4meas>0&&r2meas>0&&arm==1&&abs(z0reco)<10&&z3reco>0&&size3<3"
          && projcut, "COLZ goff");

  cout << "// st2:" << endl;
  trk_eval->Draw(
      "r2meas - z2meas*((4*(z1meas*r1meas+z2meas*r2meas+z3meas*r3meas+z4meas*r4meas)-(z1meas+z2meas+z3meas+z4meas)*(r1meas+r2meas+r3meas+r4meas))/(4*(z1meas**2+z2meas**2+z3meas**2+z4meas**2)-(z1meas+z2meas+z3meas+z4meas)**2)) - ((r1meas+r2meas+r3meas+r4meas)*(z1meas**2+z2meas**2+z3meas**2+z4meas**2)-(z1meas+z2meas+z3meas+z4meas)*(z1meas*r1meas+z2meas*r2meas+z3meas*r3meas+z4meas*r4meas))/(4*(z1meas**2+z2meas**2+z3meas**2+z4meas**2)-(z1meas+z2meas+z3meas+z4meas)**2):((phi3meas*57.3+270)%360-90)>>p2",
      "r3meas>0&&r4meas>0&&r2meas>0&&arm==1&&abs(z0reco)<10&&z3reco>0&&z3reco>-99&&size3<3"
          && projcut, "COLZ goff");

  cout << "// st3:" << endl;
  trk_eval->Draw(
      "r3meas - z3meas*((4*(z1meas*r1meas+z2meas*r2meas+z3meas*r3meas+z4meas*r4meas)-(z1meas+z2meas+z3meas+z4meas)*(r1meas+r2meas+r3meas+r4meas))/(4*(z1meas**2+z2meas**2+z3meas**2+z4meas**2)-(z1meas+z2meas+z3meas+z4meas)**2)) - ((r1meas+r2meas+r3meas+r4meas)*(z1meas**2+z2meas**2+z3meas**2+z4meas**2)-(z1meas+z2meas+z3meas+z4meas)*(z1meas*r1meas+z2meas*r2meas+z3meas*r3meas+z4meas*r4meas))/(4*(z1meas**2+z2meas**2+z3meas**2+z4meas**2)-(z1meas+z2meas+z3meas+z4meas)**2):((phi3meas*57.3+270)%360-90)>>p3",
      "r3meas>0&&r4meas>0&&r2meas>0&&arm==1&&abs(z0reco)<10&&z3reco>0&&z3reco>-99&&size3<3"
          && projcut, "COLZ goff");

  cout << "// st4:" << endl;
  trk_eval->Draw(
      "r4meas - z4meas*((4*(z1meas*r1meas+z2meas*r2meas+z3meas*r3meas+z4meas*r4meas)-(z1meas+z2meas+z3meas+z4meas)*(r1meas+r2meas+r3meas+r4meas))/(4*(z1meas**2+z2meas**2+z3meas**2+z4meas**2)-(z1meas+z2meas+z3meas+z4meas)**2)) - ((r1meas+r2meas+r3meas+r4meas)*(z1meas**2+z2meas**2+z3meas**2+z4meas**2)-(z1meas+z2meas+z3meas+z4meas)*(z1meas*r1meas+z2meas*r2meas+z3meas*r3meas+z4meas*r4meas))/(4*(z1meas**2+z2meas**2+z3meas**2+z4meas**2)-(z1meas+z2meas+z3meas+z4meas)**2):((phi3meas*57.3+270)%360-90)>>p4",
      "r3meas>0&&r4meas>0&&r2meas>0&&arm==1&&abs(z0reco)<10&&z3reco>0&&z3reco>-99&&size3<3"
          && projcut, "COLZ goff");
//

  p1->GetYaxis()->SetRangeUser(-.04, .04);
  p2->GetYaxis()->SetRangeUser(-.04, .04);
  p3->GetYaxis()->SetRangeUser(-.04, .04);
  p4->GetYaxis()->SetRangeUser(-.04, .04);

  TCanvas *c1 = new TCanvas("c1", "ResidFromProfile_hvh10_2DFit", 800, 800);
  c1->Divide(2, 2);
  int idx = 1;

  TLine * line1 = new TLine(90, -0.03, 90, 0.03);
  line1->SetLineWidth(2);
  line1->SetLineColor(2);
  Float_t x;
  Float_t y;
  Float_t tmp;
  float bigerror;
  bigerror = 0.019;

  float fval;
  TString cval;
  ostringstream convert1_1, convert1_2; ///make a stream variable to hold the converted number
  ostringstream convert2_1, convert2_2;
  ostringstream convert3_1, convert3_2;
  ostringstream convert4_1, convert4_2;

  //----------------- ---------------------------------------------------------
  c1->cd(idx++);
  c1->Update();

  TF1 *myfit1_1 = new TF1("myfit1_1",
      "[0]+[1]*sin(1.57+x*3.14159/180)+[2]*cos(1.57+x*3.14159/180)", -90, 90);
  TF1 *myfit1_2 = new TF1("myfit1_2",
      "[0]+[1]*sin(1.57-x*3.14159/180)+[2]*cos(1.57+x*3.14159/180)", 90, 270);

  p1->Fit(myfit1_1, "MR"); // fit St 1 left
  p1->Fit(myfit1_2, "MR"); // fit St 1 right

  TText *t1 = new TText(-40, -0.03, "NE0");
  t1->Draw();
  TText *t2 = new TText(180, -0.03, "NW0");
  t2->Draw();

  p1->Draw("COLZ");
  line1->Draw();
  myfit1_1->Draw("same");
  myfit1_2->Draw("same");


  TLatex *t3 = new TLatex(
      -60,
      -0.035,
      Form("#Deltax = %.1f #pm %.1f, #Deltay = %.1f #pm %.1f"
          , 10000 * myfit1_1->GetParameter(1)
          , 10000 * myfit1_1->GetParError(1)
          , 10000 * myfit1_1->GetParameter(2)
          , 10000 * myfit1_1->GetParError(2)
          )
          );
  t3->SetTextSize(0.04);
  t3->Draw();

  TLatex *t3 = new TLatex(
      0,
      -0.038,
      Form("#Deltax = %.1f #pm %.1f, #Deltay = %.1f #pm %.1f"
          , 10000 * myfit1_2->GetParameter(1)
          , 10000 * myfit1_2->GetParError(1)
          , 10000 * myfit1_2->GetParameter(2)
          , 10000 * myfit1_2->GetParError(2)
          )
          );
  t3->SetTextSize(0.04);
  t3->Draw();


  //----------------- ---------------------------------------------------------
  c1->cd(idx++);
  c1->Update();

  TF1 *myfit1_1 = new TF1("myfit2_1",
      "[0]+[1]*sin(1.57+x*3.14159/180)+[2]*cos(1.57+x*3.14159/180)", -90, 90);
  TF1 *myfit1_2 = new TF1("myfit2_2",
      "[0]+[1]*sin(1.57-x*3.14159/180)+[2]*cos(1.57+x*3.14159/180)", 90, 270);

  p2->Fit(myfit1_1, "MR"); // fit St 1 left
  p2->Fit(myfit1_2, "MR"); // fit St 1 right

  TText *t1 = new TText(-40, -0.03, "NE0");
  t1->Draw();
  TText *t2 = new TText(180, -0.03, "NW0");
  t2->Draw();

  p2->Draw("COLZ");
  line1->Draw();
  myfit1_1->Draw("same");
  myfit1_2->Draw("same");

  TLatex *t3 = new TLatex(
      -60,
      -0.035,
      Form("#Deltax = %.1f #pm %.1f, #Deltay = %.1f #pm %.1f"
          , 10000 * myfit1_1->GetParameter(1)
          , 10000 * myfit1_1->GetParError(1)
          , 10000 * myfit1_1->GetParameter(2)
          , 10000 * myfit1_1->GetParError(2)
          )
          );
  t3->SetTextSize(0.04);
  t3->Draw();

  TLatex *t3 = new TLatex(
      0,
      -0.038,
      Form("#Deltax = %.1f #pm %.1f, #Deltay = %.1f #pm %.1f"
          , 10000 * myfit1_2->GetParameter(1)
          , 10000 * myfit1_2->GetParError(1)
          , 10000 * myfit1_2->GetParameter(2)
          , 10000 * myfit1_2->GetParError(2)
          )
          );
  t3->SetTextSize(0.04);
  t3->Draw();

  //----------------- ---------------------------------------------------------
  c1->cd(idx++);
  c1->Update();

  TF1 *myfit1_1 = new TF1("myfit3_1",
      "[0]+[1]*sin(1.57+x*3.14159/180)+[2]*cos(1.57+x*3.14159/180)", -90, 90);
  TF1 *myfit1_2 = new TF1("myfit3_2",
      "[0]+[1]*sin(1.57-x*3.14159/180)+[2]*cos(1.57+x*3.14159/180)", 90, 270);

  p3->Fit(myfit1_1, "MR"); // fit St 1 left
  p3->Fit(myfit1_2, "MR"); // fit St 1 right

  TText *t1 = new TText(-40, -0.03, "NE0");
  t1->Draw();
  TText *t2 = new TText(180, -0.03, "NW0");
  t2->Draw();

  p3->Draw("COLZ");
  line1->Draw();
  myfit1_1->Draw("same");
  myfit1_2->Draw("same");


  TLatex *t3 = new TLatex(
      -60,
      -0.035,
      Form("#Deltax = %.1f #pm %.1f, #Deltay = %.1f #pm %.1f"
          , 10000 * myfit1_1->GetParameter(1)
          , 10000 * myfit1_1->GetParError(1)
          , 10000 * myfit1_1->GetParameter(2)
          , 10000 * myfit1_1->GetParError(2)
          )
          );
  t3->SetTextSize(0.04);
  t3->Draw();

  TLatex *t3 = new TLatex(
      0,
      -0.038,
      Form("#Deltax = %.1f #pm %.1f, #Deltay = %.1f #pm %.1f"
          , 10000 * myfit1_2->GetParameter(1)
          , 10000 * myfit1_2->GetParError(1)
          , 10000 * myfit1_2->GetParameter(2)
          , 10000 * myfit1_2->GetParError(2)
          )
          );
  t3->SetTextSize(0.04);
  t3->Draw();


  //----------------- ---------------------------------------------------------
  c1->cd(idx++);
  c1->Update();

  TF1 *myfit1_1 = new TF1("myfit4_1",
      "[0]+[1]*sin(1.57+x*3.14159/180)+[2]*cos(1.57+x*3.14159/180)", -90, 90);
  TF1 *myfit1_2 = new TF1("myfit4_2",
      "[0]+[1]*sin(1.57-x*3.14159/180)+[2]*cos(1.57+x*3.14159/180)", 90, 270);

  p4->Fit(myfit1_1, "MR"); // fit St 1 left
  p4->Fit(myfit1_2, "MR"); // fit St 1 right

  TText *t1 = new TText(-40, -0.03, "NE0");
  t1->Draw();
  TText *t2 = new TText(180, -0.03, "NW0");
  t2->Draw();

  p4->Draw("COLZ");
  line1->Draw();
  myfit1_1->Draw("same");
  myfit1_2->Draw("same");


  TLatex *t3 = new TLatex(
      -60,
      -0.035,
      Form("#Deltax = %.1f #pm %.1f, #Deltay = %.1f #pm %.1f"
          , 10000 * myfit1_1->GetParameter(1)
          , 10000 * myfit1_1->GetParError(1)
          , 10000 * myfit1_1->GetParameter(2)
          , 10000 * myfit1_1->GetParError(2)
          )
          );
  t3->SetTextSize(0.04);
  t3->Draw();

  TLatex *t3 = new TLatex(
      0,
      -0.038,
      Form("#Deltax = %.1f #pm %.1f, #Deltay = %.1f #pm %.1f"
          , 10000 * myfit1_2->GetParameter(1)
          , 10000 * myfit1_2->GetParError(1)
          , 10000 * myfit1_2->GetParameter(2)
          , 10000 * myfit1_2->GetParError(2)
          )
          );
  t3->SetTextSize(0.04);
  t3->Draw();

  SaveCanvas(c1, TString(_file0->GetName()) + "_" + c1->GetTitle(), kFALSE);

}
