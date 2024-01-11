Int_t niter = 0;

TFile* ifile = 0;
TH1* h1_height = 0;
TH1* h1_mean = 0;
TH1* h1_sigma = 0;
TH1* h1_rchi2 = 0;
TH1* h1_bg = 0;
TCanvas* c_dists = 0;

string  PI0_HIST_ROOT_FILE_NAME;
string  PI0_HIST_TABLE_FILE_NAME_TOWER;

string  COEF_FILE_NAME_IN;
string  COEF_FILE_NAME_OUT;

string  COEF_FILE_NAME_SUPERMOD_IN;
string  COEF_FILE_NAME_SUPERMOD_OUT;

string  UNCALIB_LIST_FILE_NAME_IN;
string  UNCALIB_LIST_FILE_NAME_OUT;

void check_calib_result(Int_t n)
{
  cout<<"created"<<endl;
  niter = n;
  init();
  cout<<"intitialized"<<endl;
  check();
  cout<<"checked"<<endl;
}

void init()
{
   gSystem->Load("libEmcOffCal.so");
   
   PI0_HIST_ROOT_FILE_NAME        = gSystem->Getenv("PI0_HIST_ROOT_FILE_NAME");
   PI0_HIST_TABLE_FILE_NAME_TOWER = gSystem->Getenv("PI0_HIST_TABLE_FILE_NAME_TOWER");
   
   COEF_FILE_NAME_IN  = gSystem->Getenv("COEF_FILE_NAME_IN");
   COEF_FILE_NAME_OUT = gSystem->Getenv("COEF_FILE_NAME_OUT");
   COEF_FILE_NAME_SUPERMOD_IN  = gSystem->Getenv("COEF_FILE_NAME_SUPERMOD_IN");
   COEF_FILE_NAME_SUPERMOD_OUT = gSystem->Getenv("COEF_FILE_NAME_SUPERMOD_OUT");

   UNCALIB_LIST_FILE_NAME_IN  = gSystem->Getenv("UNCALIB_LIST_FILE_NAME_IN");
   UNCALIB_LIST_FILE_NAME_OUT = gSystem->Getenv("UNCALIB_LIST_FILE_NAME_OUT");

   gROOT->SetStyle("Plain");
//     gStyle->SetHistLineWidth(2);
   gStyle->SetLabelFont(22, "XYZ");
   gStyle->SetTitleFont(22, "");
   gStyle->SetLabelFont(22, "XYZ");
   gStyle->SetStatFont(22);
   gStyle->SetTextFont(22);
   gStyle->SetOptStat(1111);
   gStyle->SetOptFit();

   ifile = new TFile(PI0_HIST_ROOT_FILE_NAME.c_str());
   if(!ifile->IsOpen()) 
     {
       cout<<"TFile:"<<PI0_HIST_ROOT_FILE_NAME.c_str()<<" not found!"<<endl; 
       return;
     }
   gROOT->cd();
   h1_height = (TH1*)ifile->Get("h1_height_dist")->Clone("h1_height");
   h1_mean   = (TH1*)ifile->Get("h1_mean_dist")  ->Clone("h1_mean");
   h1_sigma  = (TH1*)ifile->Get("h1_sigma_dist") ->Clone("h1_sigma");
   h1_rchi2  = (TH1*)ifile->Get("h1_rchi2_dist") ->Clone("h1_rchi2");
   h1_bg     = (TH1*)ifile->Get("h1_bg_dist")    ->Clone("h1_bg");
   ifile->Close();

   gStyle->SetOptLogy(kTRUE);
   c_dists = new TCanvas("c_dists", "");
   c_dists->Divide(2, 3);
   c_dists->cd(1);  h1_height->Draw();
   c_dists->cd(2);  h1_mean  ->Draw();
   c_dists->cd(3);  h1_sigma ->Draw();
   c_dists->cd(4);  h1_rchi2 ->Draw();
   c_dists->cd(5);  h1_bg    ->Draw();
   gStyle->SetOptLogy(kFALSE);

   cout << "height: underflow " 
        << h1_height->GetBinContent(0) << ", overflow "
        << h1_height->GetBinContent(10000) << endl
        << "mean: underflow " 
        << h1_mean->GetBinContent(0) << ", overflow "
        << h1_mean->GetBinContent(10000) << endl
        << "sigma: underflow " 
        << h1_sigma->GetBinContent(0) << ", overflow "
        << h1_sigma->GetBinContent(10000) << endl
        << "rchi2: underflow " 
        << h1_rchi2->GetBinContent(0) << ", overflow "
        << h1_rchi2->GetBinContent(10000) << endl
        << "bg: underflow " 
        << h1_bg->GetBinContent(0) << ", overflow "
        << h1_bg->GetBinContent(10000) << endl;
}

void check(
   Double_t height_give_up = 10,
   Double_t height_low = 20,
   Double_t height_high = 1500, 
//    Double_t mean_low = 0.110, original .. gave a lot of errors CGal 120406
//    Double_t mean_high = 0.210, 
//    Double_t mean_low = 0.090,//old, too loose for Run 12 heavy ion//HG
//    Double_t mean_high = 0.210, 
   Double_t mean_low = 0.12,
   Double_t mean_high = 0.155, 
//    Double_t sigma_low = 0.001,//old, way too loose for Run 12 heavy ion.//HG
//    Double_t sigma_high = 0.1,
   Double_t sigma_low = 0.005,
   Double_t sigma_high = 0.03,
   Double_t rchi2_low = 0, 
   Double_t rchi2_high = 20
)
{
  if (c_dists) delete c_dists;
  cout<<"min:"<<height_low<<" "<<height_high<<endl;
  cout<<"min:"<<mean_low<<" "<<mean_high<<endl;
  TowerHistChecker* checker = new TowerHistChecker(niter);
  //   checker->SetFlagNoEyeCheck(true);
  checker->CheckCalibResult(
			    height_give_up, height_low, height_high, 
			    mean_low, mean_high, sigma_low, sigma_high, rchi2_low, rchi2_high,
			    PI0_HIST_ROOT_FILE_NAME.c_str(),
			    PI0_HIST_TABLE_FILE_NAME_TOWER.c_str(),
			    UNCALIB_LIST_FILE_NAME_IN.c_str(),
			    UNCALIB_LIST_FILE_NAME_OUT.c_str(),
			    COEF_FILE_NAME_IN.c_str(),
			    COEF_FILE_NAME_OUT.c_str(),
			    COEF_FILE_NAME_SUPERMOD_IN.c_str(),
			    COEF_FILE_NAME_SUPERMOD_OUT.c_str()
			    );
}
