void getCorr(){
  
  const int N=32;
  const int NGood=29;
  
  float p0[N];
  float p1[N];
  float p2[N];
  float p3[N];

  float gain1[N];
  float gain3[N];


  FILE* fp;
  fp=fopen("parameters/gain.txt","r");
  TGraph* gr = new TGraph();
  int ipt=0;
  for(int i=0; i<N; i++){

    fscanf(fp,"%f %f %f %f", &p0[i], &p1[i], &p2[i], &p3[i]);
    if(i==20-8 || i==23-8 || i==39-8 ) // Dead channel
      continue;

    gr->SetPoint(ipt, p1[i], p3[i]);
    ipt++;
  }

  gr->SetMarkerStyle(7);
  gr->SetTitle("p1 & p3 correlation");
  gr->GetXaxis()->SetTitle("p1");
  gr->GetYaxis()->SetTitle("p3");
  gr->Draw("ap");
  gStyle->SetOptFit();
  TF1 fit("pol1","pol1");
  fit->SetLineColor(2);
  gr->Fit(&fit);
  
  for(int i=0; i<8; i++){
    int ref=3;
    gain1[i]=p1[i]/p1[ref];
    gain3[i]=p3[i]/p3[ref];
    cout<<i<<" "<<p1[i]<<" "<<gain1[i]<<endl;
  }
  for(int i=8; i<16; i++){
    int ref=11;
    gain1[i]=p1[i]/p1[ref];
    gain3[i]=p3[i]/p3[ref];
    cout<<i<<" "<<p1[i]<<" "<<gain1[i]<<endl;
  }
  for(int i=16; i<24; i++){
    int ref=19;
    gain1[i]=p1[i]/p1[ref];
    gain3[i]=p3[i]/p3[ref];
    cout<<i<<" "<<p1[i]<<" "<<gain1[i]<<endl;
  }
  for(int i=24; i<32; i++){
    int ref=27;
    gain1[i]=p1[i]/p1[ref];
    gain3[i]=p3[i]/p3[ref];
    cout<<i<<" "<<p1[i]<<" "<<gain1[i]<<endl;
  }


  FILE* fp1;
  fp1=fopen("parameters/gainp1.txt","w"); 
  FILE* fp3;
  fp3=fopen("parameters/gainp3.txt","w"); 
  
  for(int i=0; i<16; i++){
    fprintf(fp1,"%.6f\n",gain1[i]);
    fprintf(fp3,"%.6f\n",gain3[i]);
  }

  for(int i=16; i<32; i++){
    fprintf(fp1,"%.6f\n",gain1[i]);
    fprintf(fp3,"%.6f\n",gain3[i]);
  }

  fclose(fp1);
  fclose(fp3);
}
