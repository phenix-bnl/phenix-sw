/*
 * VERY heavily hacked from the ROOT tutorial file work.C
 *
 * Parameters x and y indicate number of histograms across and down the page.
 * Parameter page is max. number of pages to generate.
 * For QA histograms, I get decent results with Hist2PS(2,3,20).  This prints
 * all of the histograms (because they take less than 20 pages).
 * 
 * Be sure to change the declarations of f1 and ps to have correct filenames.
 *
 * Ed Jackson <ejackson@iastate.edu>
 * July 2001
 */
void
Hist2PS (int x, int y, int page)
{
  int page_cnt, hist_per_page, i;
  TFile *f1 = new TFile ("qaout.root");   //ROOT file containing histograms
  TCanvas *c1 = new TCanvas ("c1", "hist2EPS", 640, 828); // 8.5 x 11 aspect
  TPostScript *ps = new TPostScript ("histos.ps", 111);	// portrait
  
  c1->Divide (x, y);
  hist_per_page = x * y;

  TIter next (f1->GetListOfKeys ());	//make an iterator on list of keys
  TKey *key;
  
  while (page_cnt < page)
  {
    ps->NewPage ();
    i = 1;
    while (hist_per_page >= i)
    {
      c1->cd (i);
  
      key = (TKey *) next ();	//get next key on the file
      if (!key)
	break;			//if no more keys, key=0
  
      TObject *obj = key->ReadObj ();	//read object associated to key
  
      if (obj->InheritsFrom ("TH1"))
      {				//interested by histograms only
	obj->Draw ();		//draw histogram with default option
	i++;
      }
      
    }

    for (Int_t j = i; j <= hist_per_page; j++)	// Clear unused pads on last page
    {
      c1->cd (j);
      gPad->Clear ();
    }

    c1->Update ();

    if (!key) 	//Bail out of loop if no more keys.
      break;

    page_cnt++;
  }
  
  ps->Close ();
}
