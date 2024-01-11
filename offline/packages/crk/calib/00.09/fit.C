char *hlist[47] = {
  "sADC320","sADC336","sADC384","sADC400","sADC416",
  "sADC432","sADC448","sADC480","sADC496","sADC560",
  "sADC576","sADC592","sADC608","sADC624","sADC640",
  "sADC960","sADC976","sADC1024","sADC1072","sADC1088",
  "sADC1104","sADC1120","sADC1136","sADC1152","sADC1200",
  "sADC1232","sADC1248","sADC1264","sADC1265","sADC1584",
  "sADC1600","sADC1616","sADC1664","sADC1840","sADC1888",
  "sADC1904","sADC1936","sADC2128","sADC2208","sADC2224",
  "sADC2240","sADC2496","sADC2528","sADC2544","sADC2545",
  "sADC3520","sADC3824"
};

float threshold[47] = {
  55,50,50,35,50,
  45,50,65,50,50,
  50,50,50,80,50,
  58,50,40,35,40,
  50,50,50,50,40,
  40,40,65,35,50,
  59,45,45,40,50,
  85,40,45,55,60,
  50,40,45,70,40,
  50,50
};

fit(void){
  char ans;
  double param[9];
  ofstream ofile("fit_results.txt");
  for(int i=0;i<47;i++) {
    ge = new TF1("ge","gaus(0)+expo(3)",threshold[i],350);
    ge->SetParameters(100,80,30,4.2,-0.01);
    TH1F *h = gROOT->FindObject(hlist[i]);
    h->Fit("ge","R");
    h->Draw(); 
    c1->Update();
    ge->GetParameters(param);
    delete ge;
    ofile << hlist[i]+4 <<" "<<param[1]<<" "<<param[2]<<endl;
    cout << "Next (y)?";
    cin >> ans;
    if(ans != 'y') break;
  }
}
