//Example of macro to add histogram files

//  example of macro to add two histogram files containing the same histograms
//  (in a directory structure) or ntuples/trees.
//  Histograms are added in memory as well as profile histograms.
//  ntuples and trees are merged.
//  The resulting histograms are saved into a new file.
//  original implementation : Rene Brun
//  extensions by Dirk Geppert to support files with sub-directories

#include "hadd.hh"
#include <stream.h>

//______________________________________________________________________
//
//
//
//______________________________________________________________________
int AddRecursive(TDirectory* root0,TDirectory *root1,TDirectory* outdir) {
  TFile   *fnew;
  TList   *flist;
  TFile   *afile, *file1;
  
  TH1     *h1, *h0;
  TTree   *t1, *t0;
  TObject *obj;
  TKey    *key1,*key0;
  static TDirectory *dact;
  int num_add = 0;


  TDirectory *dirsav;

  //We create an iterator to loop on all objects(keys) of first file
  TIter nextkey(root0->GetListOfKeys());
  while (key0 = (TKey*)(nextkey.Next())) {
    root0->cd();
    obj = key0->ReadObj();
     // ======================================= case of a TTree or TNtuple
    if (obj->IsA()->InheritsFrom("TTree")) {
      t0 = (TTree*)obj;
      // this part still to be implemented
    } else
      // ====================================== case of TH1 or TProfile
      if(obj->IsA()->InheritsFrom("TH1")) {
      h0 = (TH1*)obj;
      h1 = (TH1*) root1->Get(h0->GetName());
      if (h1) { // here we should check that we can add
	h0->Add(h1);
      }
    } else
      // ====================================== case of TDirectory
      if(obj->IsA()->InheritsFrom("TDirectory")) {
      // recursion
      root1->cd();
      dact=root1->mkdir(obj->GetName(),obj->GetTitle());
      dact->cd();
      TObject *objsave = obj;
      TKey    *keysave = key0;
      AddRecursive(dact,(TDirectory*)obj,dact);
      obj = objsave;
      key0 = keysave;
    } else {
      // ====================================== case of another object
      printf("anotherobjname=%s, title=%sn",obj->GetName(),obj->GetTitle());
      cout<<endl;
    }
    // ======================================== write root object, modified or not into fnew
    if (obj) {
      num_add++;
      outdir->cd();
      TObject* tmpobj = outdir->Get(key0->GetName());
      if( tmpobj ) tmpobj->Delete();
      TKey* tmpkey = outdir->GetKey(key0->GetName());
      if( tmpkey ) tmpkey->Delete();
      obj->Write(key0->GetName());
      delete obj;
      obj=NULL;
      if( num_add % 10 == 0 )cout<<"."<<flush;
    }
  }
  root1->cd();
  return num_add;
}

//______________________________________________________________________
//
//
//
//______________________________________________________________________
