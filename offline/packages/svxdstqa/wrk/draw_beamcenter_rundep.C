using namespace std;
#define MAXBUF 1024 

void SetStyle()
{
  gROOT->SetStyle("Plain");
  gStyle->SetPadGridX(1);
  gStyle->SetPadGridY(1);
  gStyle->SetPadTickX(1);
  gStyle->SetPadTickY(1);
  gStyle->SetLabelSize(0.03,"x");
  gStyle->SetLabelSize(0.03,"y");
}

void Makecanvas(const Char_t *canvasname)
{
  canvas = new TCanvas(canvasname,canvasname,0,0,1200,800);
  canvas->SetGrid();
}

void draw_beamcenter_rundep(const char *filename)
{
   SetStyle();
   
   ifstream fin;
   fin.open(filename);
   string str;

   if(!fin){
     cout << "can not open : " << filename << endl;
     return ;
   }
   Int_t n_ana=0;
   Float_t x_run[MAXBUF]={-1000};
   Float_t x_seg[MAXBUF]={-1000.};
   Float_t beamx_const[MAXBUF]={-1000.};
   Float_t beamy_const[MAXBUF]={-1000.};
   Float_t beamx_mean[MAXBUF]={-1000.};
   Float_t beamy_mean[MAXBUF]={-1000.};
   Float_t beamx_sigma[MAXBUF]={-1000.};
   Float_t beamy_sigma[MAXBUF]={-1000.};
   Float_t beamx_const_err[MAXBUF]={-1000.};
   Float_t beamy_const_err[MAXBUF]={-1000.};
   Float_t beamx_mean_err[MAXBUF]={-1000.};
   Float_t beamy_mean_err[MAXBUF]={-1000.};
   Float_t beamx_sigma_err[MAXBUF]={-1000.};
   Float_t beamy_sigma_err[MAXBUF]={-1000.};
   Float_t entry_x[MAXBUF];
   Float_t entry_x_err[MAXBUF];
   Float_t entry_y[MAXBUF];
   Float_t entry_y_err[MAXBUF];
   Float_t status[MAXBUF]= {0};
   Float_t x_run_err[MAXBUF] = {0};

  // while(getline(fin,str)){
   while(fin){
   
     fin >> x_run[n_ana]
         >> x_seg[n_ana] 
         >> beamx_const[n_ana]
         >> beamx_const_err[n_ana]
         >> beamx_mean[n_ana]
         >> beamx_mean_err[n_ana]
         >> beamx_sigma[n_ana]
         >> beamx_sigma_err[n_ana]
         >> beamy_const[n_ana]
         >> beamy_const_err[n_ana]
         >> beamy_mean[n_ana]
         >> beamy_mean_err[n_ana]
         >> beamy_sigma[n_ana]
         >> beamy_sigma_err[n_ana]
         >> entry_x[n_ana]
         >> entry_x_err[n_ana]
         >> entry_y[n_ana]
         >> entry_y_err[n_ana]
         >> status[n_ana];
 //      if (!fin.good()) break;

 /*
     sscanf(str.data(),"%f %f %f %f %f",
           &x_run[n_ana],
           &x_seg[n_ana],
           &beamx_const[n_ana],
           &beamx_const_err[n_ana],
           &beamx_mean[n_ana]);
      sscanf(str.data(),"%f %f %f %f %f",
          &beamx_mean_err[n_ana],
          &beamx_sigma[n_ana],
          &beamx_sigma_err[n_ana],
          &beamy_const[n_ana],
          &beamy_const_err[n_ana]);
     sscanf(str.data(),"%f %f %f %f %f", 
          &beamy_mean[n_ana],
          &beamy_mean_err[n_ana],
          &beamy_sigma[n_ana],
          &beamy_sigma_err[n_ana],
          &entry_x[n_ana]);
     sscanf(str.data(),"%f %f %f %d",
          &entry_x_err[n_ana],
          &entry_y[n_ana],
          &entry_y_err[n_ana],
          &status[n_ana]);
   */        
 //    cout << n_ana << " "  << x_run[n_ana]   << " "  <<  beamx_mean[n_ana]  << "  " << beamx_mean_err[n_ana] <<"  " <<status[n_ana] << endl;
     if(   
           (beamx_sigma[n_ana] == 0.05 )
         ||(beamx_sigma[n_ana] > 0.03 ) 
         ||(beamy_sigma[n_ana] == 0.05 )
         ||(beamy_sigma[n_ana] > 0.03 ) 
         ||(beamy_sigma[n_ana] <-0.0 ) 
         ||(fabs(beamx_sigma[n_ana]) <0.006 ) 
         ||(fabs(beamy_sigma[n_ana]) <0.006 ) 
         ||(fabs(beamx_const[n_ana]) <10 ) 
         ||(fabs(beamy_const[n_ana]) <10 )
         || status[n_ana] == 0 ){
       beamx_const[n_ana] = -1000;
       beamy_const[n_ana] = -1000;
       beamx_mean[n_ana] = -1000;
       beamy_mean[n_ana] = -1000;    
       beamx_sigma[n_ana] = -1000;       
       beamy_sigma[n_ana] = -1000;       
       cout << "bad data or bad fit " << x_run[n_ana] << endl;
     }
     n_ana++;
   }
   n_ana--;
   Makecanvas("beam center position x");
   TGraphErrors *gr = new TGraphErrors(n_ana, x_run, beamx_mean,x_run_err,beamx_mean_err);
   gr->SetMarkerStyle(24);
   gr->SetMarkerColor(kGreen);
   gr->SetMarkerSize(1.1);
   gr->SetTitle("mean of (primary_x - beam center in DB)");
   gr->GetXaxis()->SetTitle("run #");
   gr->GetYaxis()->SetTitle("position (cm)");
 //  gr->SetMinimum(-0.1);
 //  gr->SetMaximum(0.1);
   gr->Draw("AP");
   
   Makecanvas("beam center position y");
   TGraphErrors *gr = new TGraphErrors(n_ana, x_run, beamy_mean,x_run_err,beamy_mean_err);
   gr->SetMarkerStyle(24);
   gr->SetMarkerColor(kBlue);
   gr->SetMarkerSize(1.1);
   gr->SetTitle("mean of (primary_y - beam center in DB)");
   gr->GetXaxis()->SetTitle("run #");
   gr->GetYaxis()->SetTitle("position (cm)");
 //  gr->SetMinimum(-0.1);
 //  gr->SetMaximum(0.1);
   gr->Draw("AP");

   Makecanvas("beam width x");
   TGraphErrors *gr = new TGraphErrors(n_ana, x_run, beamx_sigma,x_run_err,beamx_sigma_err);
   gr->SetMarkerStyle(24);
   gr->SetMarkerColor(kGreen);
   gr->SetMarkerSize(1.1);
   gr->SetTitle("beam gaussian width x");
   gr->GetXaxis()->SetTitle("run #");
   gr->GetYaxis()->SetTitle("width x (cm)");
//   gr->SetMinimum(-0.1);
//   gr->SetMaximum(0.1);
   gr->Draw("AP");
  
   Makecanvas("beam width y");
   TGraphErrors *gr = new TGraphErrors(n_ana, x_run, beamy_sigma,x_run_err,beamy_sigma_err);
   gr->SetMarkerStyle(24);
   gr->SetMarkerColor(kBlue);
   gr->SetMarkerSize(1.1);
   gr->SetTitle("beam gaussian width y");
   gr->GetXaxis()->SetTitle("run #");
   gr->GetYaxis()->SetTitle("width y (cm)");
//   gr->SetMinimum(-0.1);
//   gr->SetMaximum(0.1);
   gr->Draw("AP");


}
