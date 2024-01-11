//
// compare the ped values from two different sets of MpcCal.*ped files
//
void compareped(const char *ped1_postfix = "0001", const char *ped2_postfix = "0002")
{
  //vector<string> fname;
  //fname.push_back( orig_led_fname );
  //fname.push_back( new_led_fname );

  ifstream pedfile[2][5];	// 5 diff ped files, 2 versions
  const char *filetype[5] = { "lopostped", "lopreped", "hipostped", "hipreped", "overflow" };

  int temp_ifee576ch;
  int temp_amu;
  float temp_mean;
  float temp_meanerr;
  int temp_status;

  float preped[2][5][576][64] = {{{{-1}}}};
  float prepedrms[2][5][576][64] = {{{{-1}}}};

  TString name;
  for (int iversion=0; iversion<2; iversion++)
    {
      for (int ifiletype=0; ifiletype<5; ifiletype++)
        {
          name = "MpcCal."; name += filetype[ifiletype]; name += ".";
          if ( iversion==0 ) name += ped1_postfix;
          else if ( iversion==1 ) name += ped2_postfix;
          pedfile[iversion][ifiletype].open( name.Data() );
          cout << "Opening " << name << endl;
          while ( pedfile[iversion][ifiletype] >> temp_ifee576ch >> temp_amu
                    >> temp_mean >> temp_meanerr >> temp_status )
            {
              preped[iversion][ifiletype][temp_ifee576ch][temp_amu] = temp_mean;
              prepedrms[iversion][ifiletype][temp_ifee576ch][temp_amu] = temp_meanerr;
            }
          pedfile[iversion][ifiletype].close();
        }
    }

  TH1 *hratio[2][5];	// 2 arms, 5 types
  for (int iarm=0; iarm<2; iarm++)
    {
      for (int itype=0; itype<5; itype++)
        {
          name = "h"; name += filetype[itype]; name += iarm;
          hratio[iarm][itype] = new TH1D(name,name,1000,0.,10.);
        }
    }

  int arm = 0;
  for (int ich=0; ich<576; ich++)
    {
      for (int iamu=0; iamu<64; iamu++)
        {
          if ( ich<288 ) arm = 0;
          else           arm = 1; 

          for (int itype=0; itype<5; itype++)
            {
              if ( preped[0][itype][ich][iamu] == 0 ) continue;
              double ratio = preped[1][itype][ich][iamu]/preped[0][itype][ich][iamu];
              hratio[arm][itype]->Fill( ratio );
            }
        }
    }

  // Plot new over old
  TCanvas *ac = new TCanvas("ac","ac",1000,425);
  ac->Divide(5,2);
  for (int iarm=0; iarm<2; iarm++)
    {
      for (int itype=0; itype<5; itype++)
        {
          ac->cd( iarm*5 + itype + 1);
          hratio[iarm][itype]->Draw();
          //gPad->SetLogy(1);
        }
    }

}

