#include<strstream.h>

TGraphErrors* mean_graphs[2][3][16];
TGraph* rms_graphs[2][3][16];
TGraph* sigma_graphs[2][3][16];
TCanvas* canvas1=0;
TCanvas* canvas2=0;

residual()
{
  TFile *file = new TFile("/phenix/data25/kelly/run3/run63517_stub_ntuple.root");

  const size_t N_ARM = 2;
  const size_t N_STA = 3;
  const size_t N_HOCT = 16;
  const size_t MAX_CATH = 6;

  // create graphs for sigmas of residual distributions
  // 6(4) points/graph -- 16 graphs/panel -- 6 panels 
  // (station 0-2, arms 0-1)
  //
  for(int i = 0;i < N_ARM; ++i) {
    for(int j = 0; j < N_STA; ++j) {
      for(int k = 0; k < N_HOCT; ++k) {
	mean_graphs[i][j][k] = new TGraphErrors(get_n_cath(j));
	mean_graphs[i][j][k]->SetName(mean_gr_name(i,j,k));
	mean_graphs[i][j][k]->Draw("AP");
	std::cout << "Creating " << mean_gr_name(i,j,k) << std::endl;
//  	rms_graphs[i][j][k] = new TGraph(get_n_cath(j));
//  	sigma_graphs[i][j][k] = new TGraph(get_n_cath(j));
      }
    }
  }

  // create w-residual histograms 
  //
  TH1F* temp = new TH1F("temp","temp",100,-0.5,0.5);
  TH1F* w_res[N_ARM][N_STA][N_HOCT][MAX_CATH];
  for(int i = 0;i < N_ARM; ++i) {
    for(int j = 0; j < N_STA; ++j) {
      for(int k = 0; k < N_HOCT; ++k) {
	for(int l = 0; l < get_n_cath(j); ++l) {
	  
	  // Define a cut for the location
	  // 
	  char* cut = select(i,j,k,l);
	  //Fill the histogram
	  //
	  nt1->Draw("w_trk-w_meas>>temp",cut);
	  
	  // Copy temp and rename
	  //
	  w_res[i][j][k][l] = static_cast<TH1F*>(temp->Clone());	  
	  char* name = h_name(i,j,k,l);
  	  w_res[i][j][k][l]->SetName(name);
	  std::cout << "Filling " << name << std::endl;
	}
      }
    }
  }

  // mine the created histograms for summary statistics
  //
  for(int i = 0;i < N_ARM; ++i) {
    for(int j = 0; j < N_STA; ++j) {
      for(int k = 0; k < N_HOCT; ++k) {
	for(int l = 0; l < get_n_cath(j); ++l) {
	  mean_graphs[i][j][k]->SetPoint(l, l+0.5,w_res[i][j][k][l]->GetMean());
	  mean_graphs[i][j][k]->SetPointError(l, 0, w_res[i][j][k][l]->GetRMS());
	  //	  rms_graphs[i][j][k]->SetPoint(l, l+0.5, w_res[i][j][k][l]->GetRMS());
	}
      }
    }
  }
}


// returns a cut string that selects the desired cathode plane
//
char*
select(size_t arm, size_t sta, size_t hoct, size_t cath)
{

  size_t octant = std::floor(hoct/2.0);
  size_t half_octant = hoct%2;
  size_t gap = std::float(cath/2.0);
  size_t cathode = cath%2;

  static char line[80];

  sprintf(line,"arm==%i&&sta==%i&&oct==%i&&hoct==%i&&gap==%i&&cath==%i",
	  arm,sta,octant,half_octant,gap,cathode);

  return line;
}

// returns the histogram name corresponding to given location
//
char* 
h_name(size_t arm, size_t sta, size_t hoct, size_t cath)
{
  size_t octant = std::floor(hoct/2.0);
  size_t half_octant =  hoct%2;
  size_t gap = std::float(cath/2.0);
  size_t cathode = cath%2;
  static char line[80];
  sprintf(line,"h_%i_%i_%i_%i_%i_%i", arm, sta, octant, half_octant, gap ,cathode);
  return line;
}

// returns the graph name corresponding to given location
//
char* 
mean_gr_name(size_t arm, size_t sta, size_t hoct)
{
  size_t octant = std::floor(hoct/2.0);
  size_t half_octant =  hoct%2;
  static char line[80];
  sprintf(line,"mean_%i_%i_%i_%i", arm, sta, octant, half_octant);
  return line;
}

size_t
get_n_cath(size_t sta)
{
  return (sta < 2) ? 6 : 4;
}

void
draw_station(size_t arm, size_t sta)
{
  if( arm<0 || arm>1) { usage(); return; }
  if( sta<0 || sta>2) { usage(); return; }
  
  // Style
  //
  gStyle->SetOptStat(0);

  char title[80];
  char* south = "South";
  char* north = "North";
  char* arm_string = (arm==0) ? south : north;

  if(canvas1) delete canvas1;
  if(canvas2) delete canvas2;

  sprintf(title,"MUTR %s ARM: STATION %i Octant 0-3", arm_string,sta);
  canvas1 = new TCanvas("canvas1",title,1000,900);
  canvas1->Draw();
  canvas1->Divide(2,4);
  
  TH2F* temp1 = new TH2F("temp","temp",2,0,6,2,-0.5,0.5);
  for(size_t hoct=0;hoct<8;++hoct){
    canvas1->cd(hoct+1);  
    int octant = std::floor((hoct+0.5)/2.0);
    int half_octant = hoct%2;
    sprintf(title,"Octant %i, Half Octant, %i", octant, half_octant);
    temp1->DrawClone();
    mean_graphs[arm][sta][hoct]->SetTitle(title);
    mean_graphs[arm][sta][hoct]->Draw("PSame");
  }
  
  sprintf(title,"MUTR %s ARM: STATION %i Octant 4-7", arm_string, sta);
  canvas2 = new TCanvas("canvas2",title,1000,900);
  canvas2->Draw();
  canvas2->Divide(2,4);
  
  TH2F* temp2 = new TH2F("temp","temp",2,0,6,2,-0.5,0.5);
  for(size_t hoct=8;hoct<16;++hoct){
    canvas2->cd(hoct+1-8);  
    int octant = std::floor((hoct+0.5)/2.0);
    int half_octant = hoct%2;
    sprintf(title,"Octant %i, Half Octant, %i", octant, half_octant);
    temp2->DrawClone();
    mean_graphs[arm][sta][hoct]->SetTitle(title);
    mean_graphs[arm][sta][hoct]->Draw("PSame");
  }
  
}


void usage()
{
  std::cout << "draw_station(arm,station)" << std::endl;
  std::cout << "arm - [0,1]" << std::endl;
  std::cout << "station - [0,2]" << std::endl;
}





