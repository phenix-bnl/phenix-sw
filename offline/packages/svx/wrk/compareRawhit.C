class SvxRawhitListv2;
class SvxRawhitv2;
bool compareHitlist(SvxRawhitListv2 *rawhits_reco, SvxRawhitListv2 *rawhits_prdf);
bool compareHit(SvxRawhitv2 *hit_reco, SvxRawhitv2 *hit_prdf);

void compareRawhit() {
  gROOT->SetStyle("Plain");

  gSystem->Load("libfun4all");
  gSystem->Load("libfun4allfuncs");
  gSystem->Load("libphool");
  gSystem->Load("libsvx");

  TDirectory *gDir = gDirectory;
  
  
  char* inn_reco = "recoDST_sim.root";
  char* inn_prdf = "simDST_invert3.root";

  TTree *t_reco, *t_prdf;
  TFile* inf_reco=NULL, *inf_prdf=NULL;
  
  {
    inf_reco = TFile::Open(inn_reco);
    gDirectory = gDir;
    t_reco = (TTree*)inf_reco->Get("T");
    
    inf_prdf = TFile::Open(inn_prdf);
    gDirectory = gDir;
    t_prdf = (TTree*)inf_prdf->Get("T");
  }
  

  
  SvxRawhitListv2 *rawhits_reco=NULL, *rawhits_prdf=NULL;
  t_reco->SetBranchAddress("DST/SVX/SvxRawhitList", &rawhits_reco);
  t_prdf->SetBranchAddress("DST/SVX/SvxRawhitList", &rawhits_prdf);

  int nmax_reco = t_reco->GetEntries();
  int nmax_prdf = t_prdf->GetEntries();
  
  printf("Number of events is %d %d\n", nmax_reco, nmax_prdf);

  int nmax = nmax_reco;
  if(nmax_reco>nmax_prdf) { nmax = nmax_prdf; cout<<"PRDF has small Nevt"<<endl; }

  for (int i = 0; i < nmax; i++) {
    t_reco->GetEntry(i);
    t_prdf->GetEntry(i);
  
    //if (i == 0) {
    	cout<<"Event "<<i<<": rawhits(reco:prdf)  "<<rawhits_reco->get_nRawhits()<<" "<< rawhits_prdf->get_nRawhits()<<endl;
    //}

    if(compareHitlist(rawhits_reco, rawhits_prdf)){
      cout<< "Event "<<i<<" : OK"<<endl;
    } 
    else {
      cout<< "Event "<<i<<" : Fail -- Different"<<endl;
    }

  }

  if(inf_reco!=NULL) {inf_reco->Close(); delete inf_reco;}
  if(inf_prdf!=NULL) {inf_prdf->Close(); delete inf_prdf;}
  
}

bool compareHitlist(SvxRawhitListv2 *rawhits_reco, SvxRawhitListv2 *rawhits_prdf){
  int nhit_reco = rawhits_reco->get_nRawhits();
  int nhit_prdf = rawhits_prdf->get_nRawhits();

  bool ret = true;
  if(nhit_reco!=nhit_prdf){
    cout<<"   Nreco : Nprdf "<<nhit_reco<<" "<<nhit_prdf<<"  -- FAIL"<<endl;
    ret = false;
  }

  int *arySame = new int[nhit_prdf];
  for(int i=0; i<nhit_prdf; i++){
    arySame[i]=0;
  }
    
  int nFail=0;
  for(int i=0; i<nhit_reco; i++) {
    SvxRawhitv2 *hit_reco = rawhits_reco->get_Rawhit(i);

    bool bSame=false;
    for(int j=0; j<nhit_prdf;j++) {
      SvxRawhitv2 *hit_prdf = rawhits_prdf->get_Rawhit(j);

      if(compareHit(hit_reco, hit_prdf)){
        //cout<<"HitReco: "<<i<<", HitPrdf: "<<j<<" : Same"<<endl;
        bSame=true;
        arySame[j]++;
        break;
      }
    }
    if(!bSame) {
      cout<<"Fail : ly, ld, sen, sec, rd, ch "<<i<<" "<<hit_reco->get_layer()<<" ";
      cout<<hit_reco->get_ladder()<<" "<<hit_reco->get_sensor()<<" ";
      cout<<hit_reco->get_sensorSection()<<" "<<hit_reco->get_sensorReadout()<<" ";
      cout<<hit_reco->get_channel()<<endl;
      nFail++;
    }
  }

  for(int i=0; i<nhit_prdf; i++){
    if(arySame[i]!=1){
      SvxRawhitv2 *hit_prdf = rawhits_prdf->get_Rawhit(i);

      cout<<"Fail : ly, ld, sen, sec, rd, ch "<<i<<" "<<hit_prdf->get_layer()<<" ";
      cout<<hit_prdf->get_ladder()<<" "<<hit_prdf->get_sensor()<<" ";
      cout<<hit_prdf->get_sensorSection()<<" "<<hit_prdf->get_sensorReadout()<<" ";
      cout<<hit_prdf->get_channel()<<" "<<hit_prdf->get_adc()<<endl;
      if(arySame[i]==0){ cout<<"No same hit ch: "<<i<<endl;}
      else if(arySame[i]>1){ cout<<"More than 1hit  ch: "<<i<<endl;}
      nFail++;
    }
  }

  delete [] arySame;

  return (nFail==0)&&ret;
}

int compareInt(int a, int b, char *label=""){
  //if(a==b) {cout<<"    same : "     <<label<<" "<<a<<" "<<b<<endl; return true; }
  if(a==b) {return true; }
  //else     {cout<<"    different : "<<label<<" "<<a<<" "<<b<<endl;  }
  return false;
}

bool compareHit(SvxRawhitv2 *hit_reco, SvxRawhitv2 *hit_prdf){
//  cout<<"   compareHit"<<endl;
  int nFail=0;
  if(!compareInt(hit_reco->get_layer(),  hit_prdf->get_layer(),  "layer") ) { nFail++;}
  if(!compareInt(hit_reco->get_ladder(), hit_prdf->get_ladder(), "ladder") ){ nFail++;}
  if(!compareInt(hit_reco->get_sensor(), hit_prdf->get_sensor(), "sensor") ){ nFail++;}
  if(!compareInt(hit_reco->get_sensorSection(), hit_prdf->get_sensorSection(), "section") ){ nFail++;}
  if(!compareInt(hit_reco->get_sensorReadout(), hit_prdf->get_sensorReadout(), "readout") ){ nFail++;}
  if(!compareInt(hit_reco->get_channel(), hit_prdf->get_channel(), "channel") ){ nFail++;}

/*
  if(nFail==0){
    cout<<"aa"<<endl;
    cout<<"layer   "<<hit_reco->get_layer()<<" "<< hit_prdf->get_layer()<<endl;
    cout<<"ladder  "<<hit_reco->get_ladder()<<" "<<hit_prdf->get_ladder()<<endl;
    cout<<"sensor  "<<hit_reco->get_sensor()<<" "<<hit_prdf->get_sensor()<<endl;
    cout<<"section "<<hit_reco->get_sensorSection()<<" "<<hit_prdf->get_sensorSection()<<endl;
    cout<<"readout "<<hit_reco->get_sensorReadout()<<" "<<hit_prdf->get_sensorReadout()<<endl;
    cout<<"channel "<<hit_reco->get_channel()<<" "<<hit_prdf->get_channel()<<endl;
  }
*/

  return (nFail==0);
}
