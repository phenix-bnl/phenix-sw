//***********************************************************************************//
//
// Hadron Blind Detector Event Display
// 2004.10.24 - 2004.12.12
// ODA, Susumu : oda@cns.s.u-tokyo.ac.jp
//
//***********************************************************************************//
//
// o You need three files;
//    1. ./event_display_susumu.C (this file)
//    2. ./simDST.root (one DST file)
//    3. ./simNanoDST.root (one nDST file)
//
// o You can see event display as follows;
//    % root
//    root [] .L event_display_susumu.C
//    root [] event_display_susumu()
//    root [] event_display_susumu(1, 1, 0, 0)
//    root [] event_display_susumu(1, 0, 1, 1, 0.)
//
// o You can choose display option;
//     event_display_susumu(Short_t ith_event=0, Short_t pad_display=0, Short_t nano_display = 1, Short_t ghit_display = 1, 
//                              Float_t threshold_pt = 0.2, Short_t vertex_display = 1, Bool_t rejection_display = 1, 
//                              Bool_t category_display = 1, Float_t threshold_ptot_category = 0.015, Bool_t pt_or_ptot = 0, Short_t efct_display = 0){
//    1st (Short_t) number is event number (default is 0)
//    2nd (Short_t) number is option for displaying pads not included in clusters
//        0 : doesn't display pads not included in clusters (default)
//        1 : display pads not included in clusters
//        2 : display raw pad data only
//    3rd (Short_t) number is option for displaying nanoDST (PHCentralTrack) tracks
//        0 : display no track
//        1 : display only electron (and positron) tracks (default)
//        2 : display all tracks
//    4th (Short_t) number is option for displaying HBD Geant hit tracks
//        0 : display no track
//        1 : display only electron (and positron) tracks (default)
//        2 : display electron and Cherenkov tracks
//        3 : display electron and other charged particles
//        4 : display all (electron, Cherenkov and other charged paricles) tracks
//    5th (Float_t) number is transverse momentum threshold for displaying HBD Geant hit tracks
//        default is 0.2 [GeV/c]
//    6th (Short_t) number is option for vertex cut
//        0 : display all particles
//        1 : display particles come from vertex as large circles and particles come from other places as small circles
//        2 : display only particles come from vertex
//        3 : display only particles come from other places
//    7th (Bool_t) number is option for rejection step for PHCentralTrack
//        0 : doesn't display rejection step number of each electron
//        1 : display rejection step number of each electron (default)
//    8th (Bool_t) number is option for cluster category
//        0 : doesn't distinguish clusters
//        1 : Clusters created by electrons come from vertex are blue
//            Clusters created by electrons come from other places are red
//            Other clusters are green
//    9th (Float_t) number is Momentum threshold for cluster category
//        default is 0.015 [GeV/c]. This number must be less than 5th number.
//    10th (Bool_t) number is option of P_T cut or P cut for Geant hit
//        0 : Transeverse momentum cut
//        1 : Momentum cut
//    11th (Short_t) display option of effective area of GEM
//        0 : doesn't display (default)
//        1 : impose effective area
//        2 : display effective area information only
//***********************************************************************************//

const Short_t N_arm = 2;  // number of arms 
const Short_t N_row = 9*4; // number of rows
const Short_t N_col = 19; // number of columns
const Short_t N_sect = 4; // Number of sectors per arm
const Short_t NY1 = N_row/N_sect; // Number of rows per sector

const Float_t panel_h = 26.02; // Panel Hight [cm]

const Short_t color_step = 21; // number of color steps
const Short_t color_max = 100; // maximum number in color palette for color change
const Short_t color_min = color_max - color_step + 1; // minimum number in color palette for color change

const Double_t size_hexagon = 0.012*(19./(Double_t)N_col);  // hexagon size
const Double_t dx0 = 1.; // number for hexagon
const Double_t dx1 = 0.5; // number for hexagon
const Double_t dy0 = 0.; // number for hexagon
const Double_t dy1 = TMath::Sqrt(0.75); // number for hexagon

// Offset in canvas for display
const Double_t ofx = 0.04;  // offset for pad display
const Double_t ofy = 0.92; // offset for pad display
const Double_t disx = 0.37; //distance between two panels
const Double_t ofxc = 0.81;  // offset for color display
const Double_t ofyca = 0.051-0.001*(Double_t)color_step; // offset for color display
const Double_t ofyc0 = 0.6; // offset for color display
const Double_t ofyc1 = 0.94; // offset for color display
const Double_t ofyc2 = ofyc0-2.2*(ofyc1-ofyc0)/(Double_t)(color_step-1)-0.05; // offset for color display
const Double_t ofyc5 = ofyc2 - 0.03; // offset for Geant Hit Legend
const Double_t ofyc6 = ofyc2 *0.35; // offset for Geant Hit Legend
const Double_t ofyc3 = ofyc6 - 0.03; // offset for PHCentralTrack Legend
const Double_t ofyc4 = 0.02; // offset for PHCentralTrack Legend
const Double_t ofyc7 =  0.36*ofyc4+0.64*ofyc3; // offset for PHCentralTrack Legend
const Double_t ofyc7a = 0.58*ofyc4+0.42*ofyc3; // offset for PHCentralTrack Legend
const Double_t ofyc8  = 0.80*ofyc4+0.20*ofyc3; // offset for PHCentralTrack Legend
const Double_t ofyc9 =    0.85*ofyc5+0.15*ofyc6; // offset for Geant Hit Legend, Electron
const Double_t ofyc9a =   0.79*ofyc5+0.21*ofyc6; // offset for Geant Hit Legend, Electron from vtx
const Double_t ofyc9b =   0.73*ofyc5+0.27*ofyc6; // offset for Geant Hit Legend, Electron not from vtx
const Double_t ofyc10 =   0.64*ofyc5+0.36*ofyc6; // offset for Geant Hit Legend, Positron
const Double_t ofyc10a =  0.58*ofyc5+0.42*ofyc6; // offset for Geant Hit Legend, Positron from vtx
const Double_t ofyc10b =  0.52*ofyc5+0.48*ofyc6; // offset for Geant Hit Legend, Positron not from vtx
const Double_t ofyc11 =   0.43*ofyc5+0.57*ofyc6; // offset for Geant Hit Legend, Cherenkov Light
const Double_t ofyc12 =   0.34*ofyc5+0.66*ofyc6; // offset for Geant Hit Legend, Charged particle
const Double_t ofyc12a =  0.28*ofyc5+0.72*ofyc6; // offset for Geant Hit Legend, Charged particle from vtx
const Double_t ofyc12a1 = 0.22*ofyc5+0.78*ofyc6; // offset for Geant Hit Legend, Charged particle not from vtx
const Double_t ofyc12b =  0.14*ofyc5+0.86*ofyc6; // offset for Geant Hit Legend, Pt threshold
const Double_t ofyc12c =  0.06*ofyc5+0.94*ofyc6; // offset for Geant Hit Legend, Rvtx, Zvtx

const Double_t bar_width = size_hexagon*6.; // offset for color bars
const Double_t bar_offset = 0.008; // offset for color bars
const Double_t bar_height = (ofyc1-ofyc0)/(Double_t)(color_step-1); // offset for color bars

const Float_t thresh_charge = 1.6; // hit threshold of charge [fC]
const Float_t limit_charge = 64.; // maximum limit of charge [fC]
const Float_t thresh_time = 135.; // threshold of time [ns]

const Short_t max_num_cluster = 200; // maximum number of clusters 
const Short_t max_num_ghit = 2500; // maximum number of ghits 
const Short_t max_num_nano = 1000; // maximum number of nanoDST tracks 

const Double_t cog_size = 0.5; // size of cog circle
const Short_t cog_width = 1; // linewidth of cog circle

const Short_t delete_line_width = 1; // linewidth of delete line

const Short_t shared_bold = 1;
// LineWidth and LineColor option for boundary of clusters and shared pads
// 0 : Magenta line
// 1 : Magenta bold line
// 2 : Black line

// Attribute of Geant Hit in Display
//// Electron
const Float_t circle_size_ghit_e = 1.1*size_hexagon*cog_size;
const Short_t line_width_ghit_e = 2;
const Short_t line_color_ghit_e = 2;
const Short_t fill_color_ghit_e = 5;
//// Positron
const Short_t line_color_ghit_p = fill_color_ghit_e;
const Short_t fill_color_ghit_p = line_color_ghit_e;
//// Cherenkov Light
const Float_t circle_size_ghit_c = 0.5*size_hexagon*cog_size;
const Short_t line_width_ghit_c = 1;
const Short_t line_color_ghit_c = 1;
const Short_t fill_color_ghit_c = 6;
//// Other Charged Praticles
const Float_t circle_size_ghit_q = 0.5*size_hexagon*cog_size;
const Short_t line_width_ghit_q = 1;
const Short_t line_color_ghit_q = 1;
const Short_t fill_color_ghit_q = 4;
// Attribute of PHCentralTrack in Display
//// Electron
const Float_t circle_size_nano_e = 1.1*size_hexagon*cog_size;
const Short_t line_width_nano_e = 2;
const Short_t line_color_nano_e = 4;
const Short_t fill_color_nano_e = 3;
//// Positron
const Short_t line_color_nano_p = fill_color_nano_e;
const Short_t fill_color_nano_p = line_color_nano_e;
//// Other Charged Praticles
const Float_t circle_size_nano_q = 0.5*size_hexagon*cog_size;
const Short_t line_width_nano_q = 1;
const Short_t line_color_nano_q = 1;
const Short_t fill_color_nano_q = 5;

// Cut Parameter for track selection
const Short_t good_quality_1 = 63;
const Short_t good_quality_2 = 31;

// Parameters for vertex cut
const Float_t ZvertexMax = 3.; // [cm]
const Float_t RvertexMax = 3.; // [cm]

// Cut Parameter for electron identification of Cetral Arm
const Short_t n0Min = 3; 
const Float_t eopMin = 0.7; 
const Float_t eopMax = 1.3;
const Float_t chi2Max = 10.0;
const Float_t dispMax = 5.0;

// Cut Parameters for electron identification of HBD
const Float_t p0_dz = 1.011;
const Float_t p1_dz = 0.08437;
const Float_t p2_dz = 0.3436; // [cm]
const Float_t p0_dphi = 1.178;
const Float_t p1_dphi = 0.0005200;
const Float_t p2_dphi = 0.005195; // [rad]
const Float_t double_amplitude = 96.; // [fC]
const Float_t closestMin = 57.*0.200; // [cm]

// Graphic Classes for display
TPolyLine *hexagon[N_arm][N_row][N_col]; // hexagonal pad
TPolyLine *line[3][N_arm][N_row+1][N_col+1]; // lines for pad and cluster
TEllipse *cog_circle[max_num_cluster]; // circles for center of gravity

TPolyLine *bar_color[color_step]; // bars for color display
TPolyLine *tick_color[color_step]; // ticks for color display

TEllipse *ell[8]; // circle for color and etc. display
TLine *del[9]; // line for suppressed particle of ghit and nanoDST
TPaveText *pt[7]; // TPaveText for information and color, etc. display

Short_t row_col_cluster_table[4][N_arm*N_row*N_col]; // Table of relationship between row,col and cluster id
Short_t n_cluster_part[3][max_num_cluster]; // Number of particles in cluster
// 0 : e+ or e- from vertex
// 1 : e+ or e- from other places
// 2 : other charged particles
Short_t n_category[4]; // Number of clusters of each category
// 0 : Electron clusters from vertex
// 1 : Electron clusters from other places
// 2 : Other charged particle clusters
// 3 : Clusters don't have any particle

Bool_t g_vertex[max_num_ghit]; // flag for ghit comes from vertex or not
TEllipse *elg[max_num_ghit]; // circles for ghit
TEllipse *eln[max_num_nano]; // circles for nanoDST

Short_t line_property[3][N_arm][N_row+1][N_col+1][4]; // flag of lines of hexagon
Float_t cog[max_num_cluster][3]; // array for center of gravity
Float_t hexa_charge[N_arm][N_row+1][N_col+1]; // array for combining shared pads' charge

Short_t first = 1; // flag for first time execution

// TFile*, TTree*, TCanvas* 
TFile *f;
TTree *t;
TCanvas *c;
TFile *f_nano;
TTree *t_nano;

// number for PHCentralTrack and Geant Hit [cm] to determine whether a track is in HBD or not
//// number for matching 
const Float_t r_match_hbd = 58.;
const Float_t dr_match_hbd = 5.;
const Float_t dl_match_project_nano = 0.1;
const Float_t dl_match_project_ghit = 1.0;

//// HBD dimension and its offset in Ghit and PHCentralTrack
const Float_t scd[N_sect] = {40.500, 66.000, 91.500, 117.000};
Float_t scx[2][N_arm][N_sect][2];
Float_t scy[2][N_arm][N_sect][2];
Float_t scg[N_sect] = { 0.000,  0.000,  0.000,   0.000};
Float_t scn[N_sect] = {-0.018, -0.007, -0.839,  -2.010};
// Float_t scgx = -45.330+0.450; // 2004.11.22
// Float_t scgy = -37.705-0.100; // 2004.11.22
Float_t scgx = -45.330+0.000; // 2004.11.22 : according to TTransform, TPanel in HbdSlowSimModule.C
Float_t scgy = -37.705+0.000; // 2004.11.22
Float_t scnx = -45.330+0.023;
Float_t scny = -37.705+0.020;

const Float_t w_fr =   0.60;
const Float_t z_eff = 23.80;
const Float_t y_eff = 24.82;
const Float_t a_hex = 26.02/NY1/TMath::Sqrt(3.);
TPolyLine *S_eff[N_arm*N_sect*2]; // hexagonal pad

void init_S_eff(){
  for(Short_t i=0; i<N_arm*N_sect*2; i++){
    Double_t z0 = (Double_t)((N_col-1)/2)*1.5*a_hex + (2.*w_fr+z_eff)*(Double_t)(i%2) -(w_fr+z_eff);
    Double_t y0 = -((Double_t)((i/2)%N_sect)*panel_h + w_fr) + a_hex * TMath::Sqrt(0.75);
    Double_t tmpz[5] = {z0, z0, z0+z_eff, z0+z_eff, z0};
    Double_t tmpy[5] = {y0, y0-y_eff, y0-y_eff, y0, y0};
    for(Short_t j=0; j<5; j++){
      tmpz[j] *= size_hexagon/a_hex;
      tmpz[j] += ofx + disx*(Double_t)(i/(N_sect*2));
      tmpy[j] *= size_hexagon/a_hex;
      tmpy[j] += ofy;
    }
    S_eff[i] = new TPolyLine(5, tmpz, tmpy);
    S_eff[i]->SetFillStyle(3001);
    S_eff[i]->SetFillColor(6);
    S_eff[i]->SetLineWidth(2);
    S_eff[i]->SetLineColor(6);
  }
}

void set_S_eff(Short_t j){
  Short_t row = j/N_col;
  Short_t col = j%N_col;
  Float_t charge;
  
  if(col%2==0){
    if(row%NY1==0 || row%NY1==NY1-1) charge = 3.;
    else charge = -0.1;
  }
  else{
    if(col==(N_col-1)/2){
      if(row%NY1==NY1-1){
	if(row%N_row==N_row-1) charge = 5.;
	else charge = 4.;
      }
      else charge = 1.;
    }
    else{
      if(row%NY1==NY1-1){
	if(row%N_row==N_row-1) charge = 0.;
	else charge = 2.;
      }
      else charge = -0.1;
    }
  }
  charge = charge*12.5 + 2.;
  set_pad_charge(row, col, charge);
}

void draw_S_eff(){
  for(Short_t i=0; i<N_arm*N_sect*2; i++) S_eff[i]->Draw();
}

// This function prepares a Canvas. : 16
void init_canvas(){
  gStyle->SetCanvasColor(0);
  gStyle->SetCanvasBorderMode(0);
  gStyle->SetTextFont(40);
  
  c = new TCanvas("c", "Hadron Blind Detector Event Display", 0, 0, 800, 830);
  c->SetFillColor(0);
  //  c->SetFillStyle(3001);
  c->Modified();
}

// This function clear a Canvas. : 17
void clear_canvas(){
  c->Clear();
}

// This function changes default color to rainbow color in palette for event display.
void change_color(){
  for(Short_t i=color_min; i<=color_max; i++){
    Float_t x = (Float_t)(color_max-i)/(Float_t)(color_max-color_min)*4.7;
    Float_t r, g, b;
    Float_t pow = 1.5;

    if(x>=0. && x<1.)      { r = 1.;                         g = 1.-TMath::Power(1.-x, pow); b = 0.; }
    else if(x>=1. && x<2.) { r = 1.-TMath::Power(x-1., pow); g = 1.;                         b = 0.; }
    else if(x>=2. && x<3.) { r = 0.;                         g = 1.;                         b = 1.-TMath::Power(3.-x, pow); }
    else if(x>=3. && x<4.) { r = 0.;                         g = 1.-TMath::Power(x-3., pow); b = 1.; }
    else if(x>=4. && x<=5.){ r = 1.-TMath::Power(5.-x, pow); g = 0.;                         b = 1.; }
    cl = (TColor*) (gROOT->GetListOfColors()->At(i));
    cl->SetRGB(r, g, b);
    if(i==color_min) cl->SetRGB(0.9, 0.9, 0.9); // non hit pad set to be gray 
  }
}

// This function prepares color display
void init_color_charge(){
  Double_t cy;

  cy = ofyc0 - ofyca;
  
  Double_t tmpx[5];
  Double_t tmpy[5];
  Double_t tickx[2];
  Double_t ticky[2];

  tmpx[0] = tmpx[3] =tmpx[4] = ofxc + bar_offset;
  tmpx[1] = tmpx[2] = tickx[0] = ofxc + bar_width + bar_offset;
  tickx[1] = tickx[0]+bar_width*0.2;

  tmpy[0] = tmpy[1] =tmpy[4] = cy;
  tmpy[2] = tmpy[3] = cy + bar_height*(Double_t)color_step;

  tick_color[0] = new TPolyLine(5, tmpx, tmpy);
  tick_color[0]->SetLineColor(1);

  for(Int_t i=0; i<color_step; i++){
    tmpy[0] = tmpy[1] =tmpy[4] = ticky[0] = ticky[1] = cy;
    tmpy[2] = tmpy[3] = cy + bar_height;

    bar_color[i] = new TPolyLine(5, tmpx, tmpy, "F");
    bar_color[i]->SetFillStyle(1001); 
    bar_color[i]->SetFillColor(i+color_min); 

    if(i!=0){
      tick_color[i] = new TPolyLine(2, tickx, ticky);
      tick_color[i]->SetLineColor(1);
    }
    cy += bar_height;
  }
  
  ell[0] = new TEllipse(ofxc, ofyc2+0.035, size_hexagon*cog_size);
  ell[0]->SetLineWidth(cog_width);
  ell[0]->SetLineColor(1);
  ell[0]->SetFillColor(10);
  ell[0]->SetFillStyle(1001);
}

// This function shows event infomation; i-th event and number of clusters.
void init_info(){
  Char_t txt[50];
  pt[2] = new TPaveText(ofx-0.02, 0.02, ofx+1.45*disx, 0.15);
  pt[2]->SetFillColor(0);
  pt[2]->SetTextFont(40);

  pt[0] = new TPaveText(ofx, 0.7*ofy+0.3, ofx+0.4*disx, 0.2*ofy+0.8);
  pt[0]->AddText("East");
  pt[0]->SetFillColor(0);
  pt[0]->SetTextFont(40);

  pt[1] = new TPaveText(ofx+disx, 0.7*ofy+0.3, ofx+1.4*disx, 0.2*ofy+0.8);
  pt[1]->AddText("West");
  pt[1]->SetFillColor(0);
  pt[1]->SetTextFont(40);
 
  pt[3] = new TPaveText(ofxc-0.03, ofyc2, ofxc*0.05+0.94, ofyc1+0.04); 
  pt[3]->SetFillColor(0);
}

// This function initialize flags.
void init_line_property(){
  for(Short_t i_part=0; i_part<3; i_part++){
    for(Short_t i_arm=0; i_arm<N_arm; i_arm++){
      for(Short_t i_row=0; i_row<N_row+1; i_row++){
	for(Short_t i_col=0; i_col<N_col+1; i_col++){
	  line_property[i_part][i_arm][i_row][i_col][0] = 0; // number of clusters in one side
	  line_property[i_part][i_arm][i_row][i_col][1] = 0; // number of clusters in another side
	  line_property[i_part][i_arm][i_row][i_col][2] = -1; // cluster id
	  line_property[i_part][i_arm][i_row][i_col][3] = 1; // number of clusters 
	}
      }
    }
  }
}

// This function prepares lines of hexagonal pads.
void init_line(){
  for(Short_t i_part=0; i_part<3; i_part++){
    for(Short_t i_arm=0; i_arm<N_arm; i_arm++){
      for(Short_t i_row=0; i_row<N_row+1; i_row++){
	for(Short_t i_col=0; i_col<N_col+1; i_col++){
	  Double_t x[2], y[2];
	  x[0] = x[1] = (Double_t)(i_col/2)*(dx0+dx1)*2.;
	  y[0] = y[1] = -(Double_t)i_row*dy1*2.;
	  if(i_part==0){
	    if(i_col%2==0){ x[0] -= dx0; x[1] -= dx1;    y[0] += dy0; y[1] += dy1; }
	    else{           x[0] += dx1; x[1] += dx0;    y[0] -= dy1; y[1] += dy0; }
	  }
	  else if(i_part==1){
	    if(i_col%2==0){ x[0] -= dx1; x[1] += dx1;    y[0] += dy1; y[1] += dy1; }
	    else{           x[0] += dx0; x[1] += 2.*dx0; y[0] += dy0; y[1] += dy0; }
	  }
	  else{
	    if(i_col%2==0){ x[0] -= dx0; x[1] -= dx1;    y[0] += dy0; y[1] -= dy1; }
	    else{           x[0] += dx1; x[1] += dx0;    y[0] += dy1; y[1] += dy0; }
	  }
	  for(Short_t s=0; s<2; s++){
	    x[s] *= size_hexagon;
	    x[s] += ofx + disx*(Float_t)i_arm;
	    y[s] *= size_hexagon;
	    y[s] += ofy;
	  }
	  line[i_part][i_arm][i_row][i_col] = new TPolyLine(2, x, y);
	  line[i_part][i_arm][i_row][i_col]->SetLineColor(1);
	}
      }
    }
  }
}

// This function initialize hexagon charge information.
void init_pad_charge(){
  for(Short_t i_arm=0; i_arm<N_arm; i_arm++){
    for(Short_t i_row=0; i_row<N_row; i_row++){
      for(Short_t i_col=0; i_col<N_col; i_col++){
	hexa_charge[i_arm][i_row][i_col] = 0.;
      }
    }
  }
}

// This function initializes hexagonal pads.
void init_pad_hexa(){
  const Double_t x[7] = {dx1, dx0,  dx1, -dx1, -dx0, -dx1, dx1};
  const Double_t y[7] = {dy1, dy0, -dy1, -dy1,  dy0,  dy1, dy1};

  Double_t cx[N_col];
  Double_t cy[N_col];

  cx[0] = 0.;
  cy[0] = 0.;
  for(Short_t j_col=1; j_col<N_col; j_col++){
    cx[j_col] = cx[j_col-1] + (dx0+dx1);
    cy[j_col] = - (dy1 + cy[j_col-1]);
  }
  for(Short_t i_arm=0; i_arm<N_arm; i_arm++){
    for(Short_t i_row=0; i_row<N_row; i_row++){
      for(Short_t i_col=0; i_col<N_col; i_col++){
	Double_t tmpx[7];
	Double_t tmpy[7];
	for(Short_t k=0; k<7; k++){
	  tmpx[k] = size_hexagon*(x[k]+cx[i_col])+ofx+(Float_t)i_arm*disx;
	  tmpy[k] = size_hexagon*(y[k]+cy[i_col]-(Float_t)i_row*2.*dy1)+ofy;
	}
	hexagon[i_arm][i_row][i_col] = new TPolyLine(7, tmpx, tmpy, "F");
	hexagon[i_arm][i_row][i_col]->SetFillStyle(1001);
      }
    }
  }
}

// This function initialize cog array
void init_cluster_cog_array(){
  for(Short_t i_clust=0; i_clust<max_num_cluster; i_clust++){
    for(Short_t i_data=0; i_data<3; i_data++){
      cog[i_clust][i_data] = 0.;
    }
  }
}

// This function initialize cog circle
void init_cluster_cog_circle(){
  for(Short_t i=0; i<max_num_cluster; i++){
    cog_circle[i] = new TEllipse(0., 0., size_hexagon*cog_size);
    cog_circle[i]->SetLineWidth(cog_width);
    cog_circle[i]->SetLineColor(1);
    cog_circle[i]->SetFillColor(10);
    cog_circle[i]->SetFillStyle(1001);
  }
}

// This function initializes legend of PHCentralTrack and rejection step legend
void init_nano_info(){
  pt[4] = new TPaveText(ofxc-0.03, ofyc4, ofxc*0.05+0.94, ofyc3); 
  pt[4]->SetFillColor(0);
  pt[4]->SetTextFont(40);

  ell[1] = new TEllipse(ofxc-0.01, ofyc7-0.003, circle_size_nano_e);
  ell[1]->SetLineWidth(line_width_nano_e);
  ell[1]->SetLineColor(line_color_nano_e);
  ell[1]->SetFillColor(fill_color_nano_e);
  ell[1]->SetFillStyle(1001);

  ell[6] = new TEllipse(ofxc-0.01, ofyc7a-0.003, circle_size_nano_e);
  ell[6]->SetLineWidth(line_width_nano_e);
  ell[6]->SetLineColor(line_color_nano_p);
  ell[6]->SetFillColor(fill_color_nano_p);
  ell[6]->SetFillStyle(1001);

  ell[2] = new TEllipse(ofxc-0.01, ofyc8-0.003, circle_size_nano_q);
  ell[2]->SetLineWidth(line_width_nano_q);
  ell[2]->SetLineColor(line_color_nano_q);
  ell[2]->SetFillColor(fill_color_nano_q);
  ell[2]->SetFillStyle(1001);

  del[0] = new TLine();
  del[0]->SetLineColor(1);
  del[0]->SetLineWidth(delete_line_width);

  pt[6] = new TPaveText(ofx+1.45*disx+0.02, 0.02, ofxc-0.05, 0.15); 
  pt[6]->SetFillColor(0);
  pt[6]->SetTextFont(40);
}

// This function initializes number of particles in clusters
void init_cluster_n_part(){
  for(Short_t i=0; i<max_num_cluster; i++){
    for(Short_t j=0; j<3; j++){
      n_cluster_part[j][i] = 0;
    }
  }
  n_category[0] = n_category[1] = n_category[2] = n_category[3] = 0;
}

// This function initializes row_col_cluster_table
void init_row_col_cluster_table(){
  for(Short_t i_pad=0; i_pad<N_arm*N_row*N_col; i_pad++){
    row_col_cluster_table[3][i_pad] = 0;
  }
}

// This function initializes legend of GeantHit
void init_ghit_info(){
  pt[5] = new TPaveText(ofxc-0.03, ofyc6, ofxc*0.05+0.94, ofyc5); 
  pt[5]->SetFillColor(0);
  pt[5]->SetTextFont(40);

  ell[3] = new TEllipse(ofxc-0.01, ofyc9-0.003, circle_size_ghit_e);
  ell[3]->SetLineWidth(line_width_ghit_e);
  ell[3]->SetLineColor(line_color_ghit_e);
  ell[3]->SetFillColor(fill_color_ghit_e);
  ell[3]->SetFillStyle(1001);

  ell[7] = new TEllipse(ofxc-0.01, ofyc10-0.003, circle_size_ghit_e);
  ell[7]->SetLineWidth(line_width_ghit_e);
  ell[7]->SetLineColor(line_color_ghit_p);
  ell[7]->SetFillColor(fill_color_ghit_p);
  ell[7]->SetFillStyle(1001);

  ell[4] = new TEllipse(ofxc-0.01, ofyc11-0.003, circle_size_ghit_c);
  ell[4]->SetLineWidth(line_width_ghit_c);
  ell[4]->SetLineColor(line_color_ghit_c);
  ell[4]->SetFillColor(fill_color_ghit_c);
  ell[4]->SetFillStyle(1001);

  ell[5] = new TEllipse(ofxc-0.01, ofyc12-0.003, circle_size_ghit_q);
  ell[5]->SetLineColor(line_width_ghit_q);
  ell[5]->SetLineColor(line_color_ghit_q);
  ell[5]->SetFillColor(fill_color_ghit_q);
  ell[5]->SetFillStyle(1001);

  for(Short_t i=1; i<9; i++){
    del[i] = new TLine();
    del[i]->SetLineColor(1);
    del[i]->SetLineWidth(delete_line_width);
  }
}

// This function prepare HBD geometry for Geanthit to determine whether track belong HBD or not. : 30
void init_ghit_sector(){
  for(Short_t i_sect=0; i_sect<N_sect; i_sect++){
    scg[i_sect] += scd[i_sect];
    scg[i_sect] *= TMath::Pi()/180.;
  }
  for(Short_t i_sect=N_sect-1; i_sect>=0; i_sect--){
    if(i_sect==N_sect-1){ scx[0][0][N_sect-1][1] = scgx; scy[0][0][N_sect-1][1] = scgy; }
    else{           scx[0][0][i_sect][1] = scx[0][0][i_sect+1][0]; scy[0][0][i_sect][1] = scy[0][0][i_sect+1][0]; }
    scx[0][0][i_sect][0] =  scx[0][0][i_sect][1] + panel_h*cos(scg[i_sect]); scy[0][0][i_sect][0] =  scy[0][0][i_sect][1] + panel_h*sin(scg[i_sect]);
    scx[0][1][i_sect][0] = -scx[0][0][i_sect][0];                       scx[0][1][i_sect][1] = -scx[0][0][i_sect][1];
    scy[0][1][i_sect][0] =  scy[0][0][i_sect][0];                       scy[0][1][i_sect][1] =  scy[0][0][i_sect][1];
  }
}

// This function prepare HBD geometry for nanoDST(PHCentralTrack) to determine whether track belong HBD or not. : 31
void init_nano_sector(){
  for(Short_t i=0; i<N_sect; i++){
    scn[i] += scd[i];
    scn[i] *= TMath::Pi()/180.;
  }
  for(Short_t i=N_sect-1; i>=0; i--){
    if(i==N_sect-1){ scx[1][0][N_sect-1][1] = scnx; scy[1][0][N_sect-1][1] = scny; }
    else{ scx[1][0][i][1] = scx[1][0][i+1][0]; scy[1][0][i][1] = scy[1][0][i+1][0]; }
    scx[1][0][i][0] =  scx[1][0][i][1] + panel_h*cos(scn[i]); scy[1][0][i][0] =  scy[1][0][i][1] + panel_h*sin(scn[i]);
    scx[1][1][i][0] = -scx[1][0][i][0];                       scx[1][1][i][1] = -scx[1][0][i][1];
    scy[1][1][i][0] =  scy[1][0][i][0];                       scy[1][1][i][1] =  scy[1][0][i][1];
  }
}

// This function initializes ghit circles : 32
void init_ghit_circle(){
  for(Short_t i=0; i<max_num_ghit; i++){
    elg[i] = new TEllipse(0., 0., 0.);
    g_vertex[i] = 0;
  }
}

// This function initializes nanoDST circles : 33
void init_nano_circle(){
  for(Short_t i=0; i<max_num_nano; i++){
    eln[i] = new TEllipse(0., 0., 0.);
  }
}

// This function sets charge(= pad color) for each pad. : 18
void set_pad_charge(Short_t row, Short_t col, Float_t charge){
  Short_t arm = row/N_row;
  row %= N_row;
  charge += get_cluster_pad_charge(arm, row, col);
  set_cluster_pad_charge(arm, row, col, charge);
  Short_t color = (Short_t) ((charge-thresh_charge)/(limit_charge-thresh_charge) * (Float_t)(color_step-2)+1.);
  if(color>color_step-1) color = color_step-1;
  hexagon[arm][row][col]->SetFillColor(color_min+color);
}

// This function sets clusters cog. : 19
void set_cluster_cog(Short_t row, Short_t col, Float_t charge, Short_t cluster){
  if(cluster<max_num_cluster){
    Short_t arm = row/N_row;
    row %= N_row;
    Double_t x = (Double_t)col*1.5*size_hexagon + ofx + disx*(Double_t)arm;
    Double_t y = -((Double_t)(col%2)+(Double_t)row*2.)*size_hexagon*sqrt(0.75) + ofy;

    x *= (Double_t)charge;
    y *= (Double_t)charge;
    cog[cluster][0] += x; 
    cog[cluster][1] += y; 
    cog[cluster][2] += (Double_t)charge; 
  }
}

// This function is sub function of set_cluster_pad : 20
void set_cluster_pad_sub(Short_t jk, Short_t jj, Short_t il, Short_t jm, Short_t jn, Short_t ib){
  if     (line_property[jk][il][jm][jn][2]==-1){ line_property[jk][il][jm][jn][jj]+=line_property[jk][il][jm][jn][3]; line_property[jk][il][jm][jn][2] = ib; }
  else if(line_property[jk][il][jm][jn][2]==ib){ line_property[jk][il][jm][jn][jj]+=line_property[jk][il][jm][jn][3];                     
}
  else                                         { line_property[jk][il][jm][jn][3]*=2; line_property[jk][il][jm][jn][jj]+=line_property[jk][il][jm][jn][3]; line_property[jk][il][jm][jn][2] = ib; }
}

// This function sets clusters (row, column, id). : 21
void set_cluster_pad(Short_t row, Short_t col, Short_t cluster){
  Short_t arm = row/N_row;
  row %= N_row;

  set_cluster_pad_sub(0, 0, arm, row,         col,   cluster);
  set_cluster_pad_sub(0, 1, arm, row+col%2,   col+1, cluster);
  set_cluster_pad_sub(1, 0, arm, row+col%2,   col,   cluster);
  set_cluster_pad_sub(1, 1, arm, row-col%2+1, col,   cluster);
  set_cluster_pad_sub(2, 0, arm, row+col%2,   col,   cluster);
  set_cluster_pad_sub(2, 1, arm, row,         col+1, cluster);
}

// This function sets pad charge for combining shared pads' charge : 22
void set_cluster_pad_charge(Short_t arm, Short_t row, Short_t col, Float_t charge){
  hexa_charge[arm][row][col] += charge;
}

// This function gets pad charge for combining shared pads' charge : 23
Float_t get_cluster_pad_charge(Short_t arm, Short_t row, Short_t col){
  return hexa_charge[arm][row][col];
}

// This function encloses each cluster with bold line. : 24
void set_cluster(Short_t pad_display, Bool_t category_display){
  for(Short_t k=0; k<3; k++){
    for(Short_t i_arm=0; i_arm<N_arm; i_arm++){
      for(Short_t i_row=0; i_row<N_row+1; i_row++){
	for(Short_t i_col=0; i_col<N_col+1; i_col++){
	  TPolyLine *tmp_Line = line[k][i_arm][i_row][i_col];
	  tmp_Line->SetLineWidth(1);
	  tmp_Line->SetLineColor(1);
	  if(pad_display<2){
	    if(line_property[k][i_arm][i_row][i_col][0]*line_property[k][i_arm][i_row][i_col][1]==0){
	      if(line_property[k][i_arm][i_row][i_col][0]+line_property[k][i_arm][i_row][i_col][1]==0); // case0
	      else{
		tmp_Line->SetLineWidth(3);
		if(line_property[k][i_arm][i_row][i_col][0]+line_property[k][i_arm][i_row][i_col][1]==1){ // case1
		  if(category_display){
		    Short_t category = get_cluster_category(line_property[k][i_arm][i_row][i_col][2]);
		    if(category==0) tmp_Line->SetLineColor(4);
		    else if(category==1) tmp_Line->SetLineColor(2);
		    else if(category==2) tmp_Line->SetLineColor(3);
		    else tmp_Line->SetLineColor(5);
		  }
		}
		else{ // case2
		  if(shared_bold<=1) tmp_Line->SetLineColor(6);
		}
	      }
	    }
	    else{
	      if(line_property[k][i_arm][i_row][i_col][0]*line_property[k][i_arm][i_row][i_col][1]==1); // case3
	      else if(line_property[k][i_arm][i_row][i_col][0]*line_property[k][i_arm][i_row][i_col][1]==2){ // case4
		tmp_Line->SetLineWidth(3);
		if(shared_bold<=1) tmp_Line->SetLineColor(6);
	      }
	      else{ // case5
		if(shared_bold<=1){
		  tmp_Line->SetLineColor(6);
		  if(shared_bold==1) tmp_Line->SetLineWidth(3);
		}
	      }
	    }
	  }
	  line_property[k][i_arm][i_row][i_col][0] = 1;
	}
      }
    }
  }
}

// This function displays legend of PHCentralTrack and rejection step
void draw_nano_info(Short_t nano_display, Short_t nele, Short_t npos, Short_t ncharged, Bool_t rejection_display){
  if(nano_display>0){
    Char_t text[100];
    pt[4]->Draw();
    TText *txt = new TText();
    txt->SetTextSize(0.025);
    sprintf(text, "PHCentralTrack");
    txt->DrawText(ofxc-0.02, 0.5*ofyc7+0.5*ofyc3-0.005, text);

    ell[1]->Draw();
    txt->SetTextSize(0.02);
    sprintf(text, "Electron : %1d", nele);
    txt->DrawText(ofxc+0.015, ofyc7-0.01, text);

    ell[6]->Draw();
    txt->SetTextSize(0.02);
    sprintf(text, "Positron : %1d", npos);
    txt->DrawText(ofxc+0.015, ofyc7a-0.01, text);

    ell[2]->Draw();
    txt->SetTextSize(0.017);
    sprintf(text, "Charged Particle : %1d", ncharged);
    txt->DrawText(ofxc+0.01, ofyc8-0.01, text);
    if(!(nano_display>1)) del[0]->DrawLine((ofxc-0.03)+0.005, ofyc8-0.005, (ofxc*0.05+0.94)-0.005, ofyc8-0.005);

    if(rejection_display){
      pt[6]->Draw();

      txt->SetTextSize(0.018);
      txt->SetTextColor(1);
      sprintf(text, "Rejection step");
      txt->DrawText(ofx+1.45*disx+0.028, 0.13, text);

      txt->SetTextSize(0.018);
      txt->SetTextColor(1);
      sprintf(text, "using HBD");
      txt->DrawText(ofx+1.45*disx+0.085, 0.111, text);

      for(Short_t i=1; i<=4; i++){
	txt->SetTextSize(0.02);
	txt->SetTextColor(5);
	sprintf(text, "%1d", i);
	txt->DrawText(ofx+1.45*disx+0.025, 0.108-(Float_t)i*0.02, text);

	txt->SetTextSize(0.016);
	txt->SetTextColor(1);
	if(i==1) sprintf(text, ": Central Arm eID");
	else if(i==2) sprintf(text, ": 3 sigma matching");
	else if(i==3) sprintf(text, ": Pile-up rejection");
	else sprintf(text, ": Close hit cut");
	txt->DrawText(ofx+1.45*disx+0.04, 0.108-(Float_t)i*0.02+0.001, text);
      }
    }
    txt->Delete();
  }
}

// This function displays legend of GeantHit
void draw_ghit_info(Short_t ghit_display, Float_t threshold_pt, Short_t nele, Short_t npos, Short_t ncrk, Short_t ncharged, Short_t vertex_display, Short_t nele_vtx, Short_t npos_vtx, Short_t ncharged_vtx, Bool_t pt_or_ptot){
  if(ghit_display>0){
    pt[5]->Draw();
    Char_t text[30];

    TText *txt = new TText();
    txt->SetTextSize(0.025);
    sprintf(text, "Generated Track");
    txt->DrawText(ofxc-0.02, ofyc9*0.7+ofyc5*0.3+0.003, text);

    ell[3]->Draw();
    txt->SetTextSize(0.02);
    sprintf(text, "Electron :");
    txt->DrawText(ofxc+0.015, ofyc9-0.01, text);

    txt->SetTextSize(0.017);
    sprintf(text, "%1d (from vertex)", nele_vtx);
    txt->DrawText(ofxc+0.005, ofyc9a-0.01, text);

    sprintf(text, "%1d (from other places)", nele-nele_vtx);
    txt->DrawText(ofxc+0.005, ofyc9b-0.01, text);

    if(vertex_display==2) del[4]->DrawLine((ofxc-0.03)+0.005, ofyc9b-0.005, (ofxc*0.05+0.94)-0.005, ofyc9b-0.005);
    else if(vertex_display==3) del[3]->DrawLine((ofxc-0.03)+0.005, ofyc9a-0.005, (ofxc*0.05+0.94)-0.005, ofyc9a-0.005);

    ell[7]->Draw();
    txt->SetTextSize(0.02);
    sprintf(text, "Positron :");
    txt->DrawText(ofxc+0.015, ofyc10-0.01, text);

    txt->SetTextSize(0.017);
    sprintf(text, "%1d (from vertex)", npos_vtx);
    txt->DrawText(ofxc+0.005, ofyc10a-0.01, text);

    sprintf(text, "%1d (from other places)", npos-npos_vtx);
    txt->DrawText(ofxc+0.005, ofyc10b-0.01, text);

    if(vertex_display==2) del[6]->DrawLine((ofxc-0.03)+0.005, ofyc10b-0.005, (ofxc*0.05+0.94)-0.005, ofyc10b-0.005);
    else if(vertex_display==3) del[5]->DrawLine((ofxc-0.03)+0.005, ofyc10a-0.005, (ofxc*0.05+0.94)-0.005, ofyc10a-0.005);

    if(!pt_or_ptot) sprintf(text, "Pt threshold %5.3f GeV/c", threshold_pt);
    else sprintf(text, "P threshold %5.3f GeV/c", threshold_pt);
    txt->DrawText(ofxc-0.015, ofyc12b-0.01, text);

    txt->SetTextSize(0.016);
    sprintf(text, "|Zvtx|< %3.1fcm, Rvtx< %3.1fcm", ZvertexMax, RvertexMax);
    txt->DrawText(ofxc-0.02, ofyc12c-0.01, text);

    ell[4]->Draw();
    sprintf(text, "Cherenkov Light : %1d", ncrk);
    txt->DrawText(ofxc+0.005, ofyc11-0.01, text);
    if(!(ghit_display==2||ghit_display>=4)) del[1]->DrawLine((ofxc-0.03)+0.005, ofyc11-0.005, (ofxc*0.05+0.94)-0.005, ofyc11-0.005);
      
    ell[5]->Draw();
    txt->SetTextSize(0.017);
    sprintf(text, "Charged Particle :");
    txt->DrawText(ofxc+0.005, ofyc12-0.01, text);

    txt->SetTextSize(0.016);
    sprintf(text, "%1d (from vertex)", ncharged_vtx);
    txt->DrawText(ofxc+0.005, ofyc12a-0.01, text);

    sprintf(text, "%1d (from other places)", ncharged-ncharged_vtx);
    txt->DrawText(ofxc+0.005, ofyc12a1-0.01, text);

    txt->Delete();

    if(!(ghit_display>=3)) del[2]->DrawLine((ofxc-0.03)+0.005, ofyc12-0.005, (ofxc*0.05+0.94)-0.005, ofyc12-0.005);
    if(vertex_display==2 || !(ghit_display>=3)) del[8]->DrawLine((ofxc-0.03)+0.005, ofyc12a1-0.005, (ofxc*0.05+0.94)-0.005, ofyc12a1-0.005);
    if(vertex_display==3 || !(ghit_display>=3)) del[7]->DrawLine((ofxc-0.03)+0.005, ofyc12a-0.005, (ofxc*0.05+0.94)-0.005, ofyc12a-0.005);
  }
}

// This function draws lines of pads. : 25
void draw_line(){
  for(Short_t i_arm=0; i_arm<N_arm; i_arm++){
    for(Short_t i_row=0; i_row<N_row+1; i_row++){
      for(Short_t i_col=0; i_col<N_col+1; i_col++){
	if(line_property[0][i_arm][i_row][i_col][0]==1 && (i_row!=N_row || (i_col%2==0 && i_col!=0))) line[0][i_arm][i_row][i_col]->Draw();
	if(line_property[1][i_arm][i_row][i_col][0]==1 && (i_col<N_col)) line[1][i_arm][i_row][i_col]->Draw();
	if(line_property[2][i_arm][i_row][i_col][0]==1 && (i_row!=N_row || (i_col%2==1 && i_col!=N_col))) line[2][i_arm][i_row][i_col]->Draw();
      }
    }
  }
}

// This function draw hexagonal pads. : 26
void draw_pad_hexa(){
  for(Short_t i_arm=0; i_arm<N_arm; i_arm++){
    for(Short_t i_row=0; i_row<N_row; i_row++){
      for(Short_t i_col=0; i_col<N_col; i_col++){
	hexagon[i_arm][i_row][i_col]->Draw();
      }
    }
  }
}

// This function draws clusters cog. : 27
void draw_cluster_cog(Short_t n_clusters){
  if(n_clusters>=max_num_cluster){
    cout << "Maximum number of clusters is " << max_num_cluster << "." << endl;
    cout << "Current number of clusters is " << n_clusters   << "." << endl;
    n_clusters = max_num_cluster;
  }
  for(Short_t i_cluster=0; i_cluster<n_clusters; i_cluster++){
    cog[i_cluster][0] /= cog[i_cluster][2];
    cog[i_cluster][1] /= cog[i_cluster][2];

    cog_circle[i_cluster]->SetX1(cog[i_cluster][0]);
    cog_circle[i_cluster]->SetY1(cog[i_cluster][1]);
    cog_circle[i_cluster]->Draw();
  }
}

// This function shows event infomation; i-th event and number of clusters. : 28
void draw_info(Short_t iev, Short_t n_clusters, Bool_t category_display, Float_t threshold_ptot){
  pt[0]->Draw();
  pt[1]->Draw();
  pt[2]->Draw();
  Char_t txt[100];

  sprintf(txt, "Hadron Blind Detector Event Display");
  TText *text = new TText();
  text->SetTextSize(0.03);
  text->SetTextColor(1);
  text->DrawText(ofx+0.034, 0.115, txt);

  text->SetTextSize(0.0225);
  text->SetTextColor(1);
  sprintf(txt, "Event : %4d", iev);
  text->DrawText(ofx+0.005, 0.075, txt);

  text->SetTextColor(1);
  sprintf(txt, "Number of Clusters : %2d", n_clusters);
  text->DrawText(ofx+0.005, 0.035, txt);

  if(category_display){
    text->SetTextSize(0.016);
    text->SetTextColor(4);
    sprintf(txt, "Electron clusters from vertex : %2d", n_category[0]);
    text->DrawText(ofx+disx*0.7, 0.077, txt);

    text->SetTextColor(2);
    sprintf(txt, "Electron clusters from other places : %2d", n_category[1]);
    text->DrawText(ofx+disx*0.7, 0.060, txt);

    text->SetTextColor(3);
    sprintf(txt, "Other charged particle clusters : %2d", n_category[2]);
    text->DrawText(ofx+disx*0.7, 0.043, txt);

    text->SetTextColor(5);
    sprintf(txt, "Other clusters : %2d", n_category[3]);
    text->DrawText(ofx+disx*0.7, 0.026, txt);

    text->SetTextColor(1);
    sprintf(txt, "Momentum threshold : %5.3f GeV/c", threshold_ptot);
    text->DrawText(ofx+disx*0.7, 0.094, txt);
  }
  text->Delete();
}

// This function shows the relation between color and charge. : 29
void draw_color_charge(){
  pt[3]->Draw();

  TText *txt = new TText();
  Char_t text[30];

  Double_t cx;
  Double_t cy;

  cx = ofxc + bar_offset + bar_width;
  cy = ofyc0 - ofyca;

  for(Int_t i=0; i<color_step; i++){
    bar_color[i]->Draw();
    if(i!=0){
      txt->SetTextSize(0.025*TMath::Sqrt(16./(Float_t)(color_step+5)));
      sprintf(text, " %4.1f", (limit_charge-thresh_charge)*(Float_t)(i-1)/(Float_t)(color_step-2)+thresh_charge);		 
      txt->DrawText(cx+bar_width*0.2+0.005, cy-0.008, text);
      tick_color[i]->Draw();
    }
    cy += (ofyc1-ofyc0)/(Double_t)(color_step-1);
  }
  tick_color[0]->Draw();

  cx = ofxc;
  txt->SetTextSize(0.025);
  sprintf(text, "Charge [fC]");
  txt->DrawText(cx+0.01, ofyc1-ofyca+(ofyc1-ofyc0)/(Double_t)(color_step-1)+0.01, text);

  txt->SetTextSize(0.02);
  sprintf(text, "Center of Gravity");
  txt->DrawText(cx+0.02, ofyc2+0.035-0.01, text);
  txt->Delete();

  ell[0]->Draw();
}

// This function sets ghit circles : 34
void set_ghit_circle(Short_t i, Float_t x, Float_t y, Float_t r, Short_t LineWidth, Short_t LineColor, Short_t FillColor, Bool_t comes_from_vertex){
  if(i<max_num_ghit){
    elg[i]->SetX1((Double_t)x);
    elg[i]->SetY1((Double_t)y);
    elg[i]->SetR1((Double_t)r);
    elg[i]->SetR2((Double_t)r);
    elg[i]->SetLineWidth(LineWidth);
    elg[i]->SetLineColor(LineColor);
    elg[i]->SetFillColor(FillColor);
    g_vertex[i] = comes_from_vertex;
  }
}

// This function sets nanoDST circles : 35
void set_nano_circle(Short_t i_nano, Float_t x, Float_t y, Float_t r, Short_t LineWidth, Short_t LineColor, Short_t FillColor){
  if(i_nano<max_num_nano){
    eln[i_nano]->SetX1((Double_t)x);
    eln[i_nano]->SetY1((Double_t)y);
    eln[i_nano]->SetR1((Double_t)r);
    eln[i_nano]->SetR2((Double_t)r);
    eln[i_nano]->SetLineWidth(LineWidth);
    eln[i_nano]->SetLineColor(LineColor);
    eln[i_nano]->SetFillColor(FillColor);
  }
}

// This function resisters row-column and cluster relation ships : 36
void set_row_col_cluster_table(Short_t row, Short_t col, Short_t cluster){
  Short_t row_col = row*N_col + col;
  row_col_cluster_table[row_col_cluster_table[3][row_col]][row_col] = cluster;
  row_col_cluster_table[3][row_col]++;
} 

// This function adds particle including clusters : 37
void set_cluster_n_part(Short_t category, Short_t row_col){
  for(Short_t i=0; i<row_col_cluster_table[3][row_col]; i++){
    n_cluster_part[category][row_col_cluster_table[i][row_col]]++;
  }
}

// This function gets cluster category information : 38
Short_t get_cluster_category(Short_t cluster){
  Short_t category;
  if(n_cluster_part[0][cluster]>=1) category = 0;
  else if(n_cluster_part[1][cluster]>=1) category = 1;
  else if(n_cluster_part[2][cluster]>=1) category = 2;
  else category = 3;

  return category;
}

// This function counts number of clusters of each category : 39
void count_cluster_category(Short_t n_clusters){
  for(Short_t i_cluster=0; i_cluster<n_clusters; i_cluster++){
    Short_t category = get_cluster_category(i_cluster);
    n_category[category]++;
  }
}

// This function draws ghit circles. : 40
void draw_ghit_circle(Short_t n_ghits, Short_t ghit_display, Short_t vertex_display){
  if(n_ghits>max_num_ghit){
    cout << "Maximum Number of Particles of Geant Hit is " << max_num_ghit << "." << endl;
    cout << "Current Number of Particles of Geant Hit is " << n_ghits      << "." << endl;
    n_ghits = max_num_ghit;
  }
  if(vertex_display==1){
    for(Short_t i=0; i<n_ghits; i++){
      Float_t Magnify = 1.3;
      if(g_vertex[i]==1){
	elg[i]->SetR1(elg[i]->GetR1()*Magnify);
	elg[i]->SetR2(elg[i]->GetR2()*Magnify);
      }
      else{
	elg[i]->SetR1(elg[i]->GetR1()/Magnify);
	elg[i]->SetR2(elg[i]->GetR2()/Magnify);
      }
    }
  }
  if(ghit_display>=2){
    for(Short_t i=0; i<n_ghits; i++){
      Short_t line_color = elg[i]->GetLineColor();
      Short_t fill_color = elg[i]->GetFillColor();
      if((line_color==line_color_ghit_c && fill_color==fill_color_ghit_c)|| (line_color==line_color_ghit_q && fill_color==fill_color_ghit_q)) elg[i]->Draw();
    }
  }
  for(Short_t i=0; i<n_ghits; i++){
    Short_t line_color = elg[i]->GetLineColor();
    Short_t fill_color = elg[i]->GetFillColor();
    if((line_color==line_color_ghit_e && fill_color==fill_color_ghit_e) || (line_color==line_color_ghit_p && fill_color==fill_color_ghit_p)) elg[i]->Draw();
  }
}

// This function draws nanoDST(PHCentralTrack) circles. : 41
void draw_nano_circle(Short_t n, Short_t nano_display){
  if(n>max_num_nano){
    cout << "Maximum Number of Particles of Nano DST is " << max_num_nano << "." << endl;
    cout << "Current Number of Particles of Nano DST is " << n            << "." << endl;
    n = max_num_nano;
  }
  if(nano_display>=2){
    for(Short_t i=0; i<n; i++){
      Short_t line_color = eln[i]->GetLineColor();
      Short_t fill_color = eln[i]->GetFillColor();
      if(line_color==line_color_nano_q && fill_color==fill_color_nano_q) eln[i]->Draw();
    }
  }
  for(Short_t i=0; i<n; i++){
    Short_t line_color = eln[i]->GetLineColor();
    Short_t fill_color = eln[i]->GetFillColor();
    if((line_color==line_color_nano_e && fill_color==fill_color_nano_e) || (line_color==line_color_nano_p && fill_color_nano_p)) eln[i]->Draw();
  }
}

// This function gets arm id (0:east, 1:west) : 42
Short_t get_arm(Float_t phbdx){
  Short_t arm = 0;
  if(phbdx>0.) arm = 1;
  return arm;
}

// This function gets sector id : 43
Short_t get_sect(Float_t phbdy, Short_t arm, Short_t mode){
  Short_t sect = -1;
  for(Short_t i=0; i<N_sect; i++){
    if(phbdy <= scy[mode][arm][i][0] && phbdy >= scy[mode][arm][i][1]){
      sect = i; 
      break; 
    } 
  }
  return sect;
}

// This function gets sector id (Geant Hit) : 44
Short_t get_ghit_sect(Float_t phbdy, Short_t arm){
  return get_sect(phbdy, arm, 0);
}

// This function gets sector id (nanoDST) : 45
Short_t get_nano_sect(Float_t phbdy, Short_t arm){
  return get_sect(phbdy, arm, 1);
}

// This function projects a track onto HBD plane because there are some offset among Geanthit, PHCentralTrack and TTranform : 46
Float_t project_x(Float_t phbdy, Short_t arm, Short_t sect, Short_t mode){
  return (phbdy-scy[mode][arm][sect][0])/(scy[mode][arm][sect][1]-scy[mode][arm][sect][0])*(scx[mode][arm][sect][1]-scx[mode][arm][sect][0])+scx[mode][arm][sect][0];
}

// This function projects a track onto HBD plane (Ghit) : 47
Float_t project_ghit_x(Float_t phbdy, Short_t arm, Short_t sect){
  return project_x(phbdy, arm, sect, 0);
}

// This function projects a track onto HBD plane (Nano) : 48
Float_t project_nano_x(Float_t phbdy, Short_t arm, Short_t sect){
  return project_x(phbdy, arm, sect, 1);
}

// This function translates global y-coordinate to coordinate of canvas : 49
Float_t trans_glob2disp_y(Float_t phbdy, Short_t arm, Short_t sect, Short_t mode){
  Float_t dispy = (phbdy-scy[mode][arm][sect][1])/(scy[mode][arm][sect][0]-scy[mode][arm][sect][1]);
  dispy = (dispy + 0.5/(Float_t)NY1-1.)*TMath::Sqrt(3.)*(Float_t)NY1;
  dispy = size_hexagon*(dispy-(Float_t)(sect*NY1)*TMath::Sqrt(3.)) + ofy;
  return dispy;
}

// This function translates global y-coordinate to coordinate of canvas (Ghit) : 50
Float_t trans_ghit_glob2disp_y(Float_t phbdy, Short_t arm, Short_t sect){
  return trans_glob2disp_y(phbdy, arm, sect, 0);
}

// This function translates global y-coordinate to coordinate of canvas (nDST) : 51
Float_t trans_nano_glob2disp_y(Float_t phbdy, Short_t arm, Short_t sect){
  return trans_glob2disp_y(phbdy, arm, sect, 1);
}

// This function translates global z-coordinate to coordinate of canvas : 52
Float_t trans_glob2disp_z(Float_t phbdz, Short_t arm, Short_t sect){
  Float_t dispz = phbdz/panel_h;
  dispz = (dispz - TMath::Sqrt(0.75)*(Float_t)(1-N_col)/(Float_t)(2*NY1))*TMath::Sqrt(3.)*(Float_t)NY1;
  dispz = size_hexagon*dispz + ofx + disx*(Float_t)arm;
  return dispz;
}

// This function determines whether a track belong HBD or not. : 53
Bool_t match_hbd(Float_t x, Float_t y){
  return fabs(TMath::Sqrt(x*x + y*y)-r_match_hbd) < dr_match_hbd;
}

// : 54
Short_t get_row_col(Float_t locy, Float_t locz, Short_t arm){
  Short_t row_col;

  locz = (locz - ofx - disx*(Float_t)arm) / size_hexagon;
  locy = (locy - ofy) / size_hexagon;

  if(locz>=-1. && locy<=1.){
    Float_t z[2] = {locz/3., locz/3.-0.5};
    Float_t y[2] = {locy/TMath::Sqrt(3.), locy/TMath::Sqrt(3.)+0.5};
    
    Short_t Z[2];
    Short_t Y[2];
    Float_t Rsq[2];

    for(Short_t i=0; i<2; i++){
      Z[i] = (Short_t)( z[i] + 0.5);
      Y[i] = (Short_t)(-y[i] + 0.5);
      Rsq[i] = 9.*(z[i]-(Float_t)Z[i])*(z[i]-(Float_t)Z[i]) + 3.*(-y[i]-(Float_t)Y[i])*(-y[i]-(Float_t)Y[i]);
    }
    if(Rsq[0]<=1. || Rsq[1]<=1.){
      if(Rsq[0]<Rsq[1]) row_col = 2*Z[0] + Y[0]*N_col;
      else row_col = 2*Z[1]+1 + Y[1]*N_col;
      if(arm==1) row_col += N_row*N_col;
    }
    else row_col = -1;
  }
  else row_col = -1;

  return row_col;
}

// 2004.11.25 : 1
Bool_t id_electron_or_positron_ghit(Short_t ghit_id){
  return ghit_id==2||ghit_id==3;
}

// 2004.11.25 : 2
Bool_t id_other_charged_particle_ghit(Short_t ghit_id){
  return ghit_id==5||ghit_id==6||ghit_id==8||ghit_id==9||ghit_id==11||ghit_id==12||ghit_id==14||ghit_id==15||ghit_id==45||ghit_id==46||ghit_id==47;
}

// 2004.11.25 : 3
Bool_t id_charged_particle_ghit(Short_t ghit_id){
  return id_electron_or_positron_ghit(ghit_id) || id_other_charged_particle_ghit(ghit_id);
}

// 2004.11.25 : 4
Bool_t id_cherenkov_photon_ghit(Short_t ghit_id){
  return ghit_id==50;
}

// 2004.11.25 : 5
Short_t get_flag_ghit(Short_t ghit_id, Short_t ghit_display){
  Short_t flag_ghit = 0;
  // Electron and positoron
  if((ghit_display>=1 && id_electron_or_positron_ghit(ghit_id))) flag_ghit = 1;
  // Cherenkov light
  else if((ghit_display==2||ghit_display>=4) && id_cherenkov_photon_ghit(ghit_id)) flag_ghit = 2;
  // Other charged particles : mu+, mu-, pi+, pi-, K+, K-, p, pbar, d, t, alpha //
  else if(ghit_display>=3 && id_other_charged_particle_ghit(ghit_id)) flag_ghit = 3;
  return flag_ghit;
}

// 2004.11.25 : 6
Bool_t check_comes_from_vertex(Float_t ghit_Zvertex, Float_t ghit_Rvertex){
  return ghit_Zvertex>-ZvertexMax && ghit_Zvertex<ZvertexMax && ghit_Rvertex>-0.0001 && ghit_Rvertex<RvertexMax;
}

// 2004.11.25 : 7
Bool_t match_ghit_project_x(Float_t ghit_phbdx, Float_t ghit_phbdy, Short_t arm, Short_t sect){
  return fabs(ghit_phbdx-project_ghit_x(ghit_phbdy, arm, sect)) < dl_match_project_ghit;
}

// 2004.11.25 : 8
Bool_t match_nano_project_x(Float_t phbdx, Float_t phbdy, Short_t arm, Short_t sect){
  return fabs(phbdx-project_nano_x(phbdy, arm, sect)) < dl_match_project_nano;
}

// 2004.11.25 : 9
void draw_rejection_step(Short_t rejection_step, Float_t locz, Float_t locy){
  TText *trs = new TText();
  trs->SetTextSize(0.05);
  trs->SetTextFont(62);
  trs->SetTextColor(5);
  Char_t text[2];
  sprintf(text, "%1d", rejection_step);
  trs->DrawText(locz, locy, text);
  trs->Delete();
}

// 2004.11.25 : 10
void init_at_once(){
  change_color();
  init_color_charge();

  init_canvas();
  init_info();
  init_pad_hexa();
  init_line();
  init_cluster_cog_circle();
  
  init_ghit_sector();
  init_ghit_circle();
  init_ghit_info();
  
  init_nano_sector();
  init_nano_circle();
  init_nano_info();

  init_S_eff();
}

// 2004.11.25 : 11
void init_always(Bool_t category_display){
  init_line_property();
  init_pad_charge();
  init_cluster_cog_array();
  
  clear_canvas();
  
  if(category_display){
    init_cluster_n_part();
    init_row_col_cluster_table();
  }
}

// 2004.11.25 : 12
void draw_hbd(Short_t ith_event, Short_t n_clusters, Short_t pad_display, Bool_t category_display, Float_t threshold_ptot_category){
  // Draw pad, cluster and center of gravity
  draw_pad_hexa();
  draw_line();
  if(pad_display<2) draw_cluster_cog(n_clusters);
  
  // Draw event information and color table
  draw_info(ith_event, n_clusters, category_display, threshold_ptot_category);
  draw_color_charge();
}

// 2004.11.26 : 13
Bool_t check_quality(Short_t quality){
  return quality==good_quality_1 || quality==good_quality_2;
}

// This function is a main function of this event display. : 55
void event_display_susumu(Short_t ith_event=0, Short_t pad_display=0, Short_t nano_display = 1, Short_t ghit_display = 1, Float_t threshold_pt = 0.2, Short_t vertex_display = 1, Bool_t rejection_display = 1, Bool_t category_display = 1, Float_t threshold_ptot_category = 0.015, Bool_t pt_or_ptot = 0, Short_t efct_display = 0){
  // At the first time execution, load libralies, open files, gets trees, change color table, initialize some variables and classes, prepare a canvas 
  // I. init_at_once();
  if(first==1){
    first = 0;
    gSystem->Load("libfun4all.so");
    gSystem->Load("libsimreco.so");
    f = new TFile("simDST.root");
    t = (TTree*) f->Get("T");
    f_nano = new TFile("simNanoDST.root");
    t_nano = (TTree*) f_nano->Get("T");

    init_at_once();
  }

  if(threshold_ptot_category>threshold_pt) threshold_ptot_category = threshold_pt;
  if(efct_display==2){
    pad_display = 2;
    nano_display = ghit_display = vertex_display = rejection_display = category_display = 0;
  }

  // At beginning of every execution, intialize some variables and clear canvas
  init_always(category_display);

  // prepare HBD Classes and set addresses 
  HbdBlobList *hbl = new HbdBlobListv1();
  HbdBlob *hb = new HbdBlobv1();    
  HbdCellList *hcl = new HbdCellListv1();
  HbdCell *hc = new HbdCellv1();
  HbdGhitList *hgl = new HbdGhitListv1();
  HbdGhit *hg = new HbdGhitv1();

  t->SetBranchAddress("DST/HbdBlobList", &hbl);    
  t->SetBranchAddress("DST/HbdCellList", &hcl); // for pad display //
  t->SetBranchAddress("DST/HbdGhitList", &hgl); // for ghit displat //

  // get ith_event-th event data
  t->GetEntry(ith_event); 

  // set charge value of each pad
  for(Short_t j=0; j<N_arm*N_row*N_col; j++){
    if(efct_display==2) set_S_eff(j);
    else if(pad_display){
      hc = hcl->get_cell(j);
      Short_t row = hc->get_row();
      Short_t col = hc->get_col();
      Float_t charge = hc->get_charge();
      Float_t time = hc->get_time();
      if(time > thresh_time) charge = 0.;
      set_pad_charge(row, col, charge);
    }
    else set_pad_charge(j/N_col, j%N_col, 0.);
  }

  // set boundaries of clusters
  Short_t n_clusters = hbl->get_nBlobs();
  for(Short_t j=0; j<n_clusters; j++){
    hb = hbl->get_blob(j);
    for(Short_t k=0; k<hb->get_size(); k++){
      Short_t row = ((HbdBlobv1*)hb)->get_cell_row(k);
      Short_t col = ((HbdBlobv1*)hb)->get_cell_col(k);
      Float_t charge = ((HbdBlobv1*)hb)->get_cell_charge(k);
      if(!pad_display) set_pad_charge(row, col, charge);
      set_cluster_pad(row, col, j);
      set_cluster_cog(row, col, charge, j);
      if(category_display) set_row_col_cluster_table(row, col, j);
    }
  }

  // Superimpose Generated Tracks Using Ghit
  if(ghit_display>0 || category_display){
    Short_t nGhits = hgl->get_nGhits();
    Short_t nGhits_hbd = 0;

    Short_t nele = 0;
    Short_t npos = 0;
    Short_t ncrk = 0;
    Short_t ncharged = 0;
    Short_t nele_vtx = 0;
    Short_t npos_vtx = 0;
    Short_t ncharged_vtx = 0;

    for(Short_t j=0; j<nGhits; j++){
      hg = hgl->get_ghit(j);
      Short_t ghit_id = hg->get_idPart();
      Short_t flag_ghit = get_flag_ghit(ghit_id, ghit_display);

      if(flag_ghit>0 || category_display){
	Float_t ghit_px = hg->get_pxyz(0);
	Float_t ghit_py = hg->get_pxyz(1);
	Float_t ghit_pz = hg->get_pxyz(2);
	Float_t ghit_pt = TMath::Sqrt(ghit_px*ghit_px + ghit_py*ghit_py);
	Float_t ghit_ptot = TMath::Sqrt(ghit_px*ghit_px + ghit_py*ghit_py + ghit_pz*ghit_pz);
	if(pt_or_ptot) ghit_pt = ghit_ptot;

	Float_t ghit_phbdx = hg->get_xyzin(0);
	Float_t ghit_phbdy = hg->get_xyzin(1);
	Float_t ghit_phbdz = hg->get_xyzin(2);
	
	Float_t ghit_Zvertex = hg->get_Zvertex();
	Float_t ghit_Rvertex = hg->get_Rvertex();
	Bool_t comes_from_vertex = check_comes_from_vertex(ghit_Zvertex, ghit_Rvertex);

	if(match_hbd(ghit_phbdx, ghit_phbdy)){
	  Short_t arm = get_arm(ghit_phbdx);
	  Short_t sect = get_ghit_sect(ghit_phbdy, arm);
	  if(sect>=0){
	    if(match_ghit_project_x(ghit_phbdx, ghit_phbdy, arm, sect)){
	      Float_t locy = trans_ghit_glob2disp_y(ghit_phbdy, arm, sect);
	      Float_t locz = trans_glob2disp_z(ghit_phbdz, arm, sect);

	      if(category_display && id_charged_particle_ghit(ghit_id) && ghit_ptot>threshold_ptot_category){
		Short_t part_row_col = get_row_col(locy, locz, arm);
		if(part_row_col>=0){
		  Short_t category;
		  if(id_electron_or_positron_ghit(ghit_id)){
		    if(comes_from_vertex) category = 0;
		    else category = 1;
		  }
		  else category = 2;
		  set_cluster_n_part(category, part_row_col);
		}
	      }
	      
	      if(flag_ghit>0 && (ghit_pt>threshold_pt || flag_ghit==2)){
		Bool_t display_this_particle = 0;
		if(vertex_display==0 || vertex_display==1 || (vertex_display==2 && comes_from_vertex==1) || (vertex_display==3 && comes_from_vertex==0)) display_this_particle = 1;
		if(flag_ghit==1){
		  if(ghit_id==2){
		    if(display_this_particle) set_ghit_circle(nGhits_hbd, locz, locy, circle_size_ghit_e, line_width_ghit_e, line_color_ghit_p, fill_color_ghit_p, comes_from_vertex);
		    npos++;
		    if(comes_from_vertex) npos_vtx++;
		  }
		  else{
		    if(display_this_particle) set_ghit_circle(nGhits_hbd, locz, locy, circle_size_ghit_e, line_width_ghit_e, line_color_ghit_e, fill_color_ghit_e, comes_from_vertex);
		    nele++;
		    if(comes_from_vertex) nele_vtx++;
		  }
		}
		else if(flag_ghit==2){
		  if(display_this_particle) set_ghit_circle(nGhits_hbd, locz, locy, circle_size_ghit_c, line_width_ghit_c, line_color_ghit_c, fill_color_ghit_c, comes_from_vertex);
		  ncrk++;
		}
		else if(flag_ghit==3){
		  if(display_this_particle) set_ghit_circle(nGhits_hbd, locz, locy, circle_size_ghit_q, line_width_ghit_q, line_color_ghit_q, fill_color_ghit_q, comes_from_vertex);
		  ncharged++;
		  if(comes_from_vertex) ncharged_vtx++;
		}
		if(display_this_particle) nGhits_hbd++;
	      }
	    }
	  }
	}
      }
    }
  }

  if(category_display) count_cluster_category(n_clusters);

  set_cluster(pad_display, category_display);

  draw_hbd(ith_event, n_clusters, pad_display, category_display, threshold_ptot_category);

  if(ghit_display>0){
    draw_ghit_circle(nGhits_hbd, ghit_display, vertex_display);
    draw_ghit_info(ghit_display, threshold_pt, nele, npos, ncrk, ncharged, vertex_display, nele_vtx, npos_vtx, ncharged_vtx, pt_or_ptot);
  }

  // Delete cluster, pad and Ghit classes
  hbl->Delete();
  hb->Delete();
  hcl->Delete();
  hc->Delete();
  hgl->Delete();
  hg->Delete();  

  // Superimpose Electron (and Positron) Tracks Using Nano DST
  if(nano_display>0){
    PHCentralTrackv11 *d_cnt = new PHCentralTrackv11();
    t_nano->SetBranchAddress("DST/PHCentralTrack", &d_cnt);
    t_nano->GetEvent(ith_event);

    Short_t ntrk = d_cnt->get_npart();
    Short_t nele = 0;
    Short_t npos = 0;
    Short_t ncharged = 0;
    Short_t ntot = 0;

    for(Short_t itrk=0; itrk<ntrk; itrk++){
      Short_t n0 = d_cnt->get_n0(itrk);
      Short_t flag_electron = 0;
      Short_t rejection_step = 0;

      // Electron and positron identification 
      if(n0>=n0Min){
	Float_t npe0 = d_cnt->get_npe0(itrk);
	Float_t chi2 = d_cnt->get_chi2(itrk);
	Float_t disp = d_cnt->get_disp(itrk);
	if(chi2/npe0<=chi2Max && disp<=dispMax){
	  Float_t mom = d_cnt->get_mom(itrk);
	  Float_t ecore = d_cnt->get_ecore(itrk);
	  if(ecore/mom>=eopMin && ecore/mom<=eopMax){
	    flag_electron = (Short_t)d_cnt->get_charge(itrk);
	    rejection_step++;
	    Float_t hbddz = d_cnt->get_hbddz(itrk);
	    Float_t hbddphi = d_cnt->get_hbddphi(itrk);
	    Float_t hbddzMax = p0_dz * TMath::Sqrt(p1_dz*p1_dz/mom/mom + p2_dz*p2_dz);
	    Float_t hbddphiMax = p0_dphi * TMath::Sqrt(p1_dphi*p1_dphi/mom/mom + p2_dphi*p2_dphi);
	    if(hbddz>-3.*hbddzMax && hbddz<3.*hbddzMax && hbddphi>-3.*hbddphiMax && hbddphi<3.*hbddphiMax){ // 2nd HBD rejection step
	      rejection_step++;
	      Float_t hbdcharge = d_cnt->get_hbdcharge(itrk);
	      if(hbdcharge<double_amplitude){ // 3rd HBD rejection step
		rejection_step++;
		Float_t hbdclosest = d_cnt->get_hbdclosest(itrk);
		if(hbdclosest>closestMin){ // 4th HBD rejection step
		  //		  cout << hbddz << " " << hbddphi << " " << hbdcharge << " " << hbdclosest << endl;
		  rejection_step++;
		}
	      }
	    }
	  }
	}
      }
      
      if(((flag_electron==1 || flag_electron==-1)&&nano_display==1)||(nano_display==2)){
	Short_t quality = d_cnt->get_quality(itrk);
	Float_t phbdx = d_cnt->get_phbdx(itrk);
	Float_t phbdy = d_cnt->get_phbdy(itrk);
	Float_t phbdz = d_cnt->get_phbdz(itrk);
	
	if(check_quality(quality) && match_hbd(phbdx, phbdy)){
	  Short_t arm = get_arm(phbdx);
	  Short_t sect = get_nano_sect(phbdy, arm);
	  if(sect>=0){
	    if(match_nano_project_x(phbdx, phbdy, arm, sect)){
	      Float_t locy = trans_nano_glob2disp_y(phbdy, arm, sect);
	      Float_t locz = trans_glob2disp_z(phbdz, arm, sect);

	      if(flag_electron==-1){
		set_nano_circle(ntot, locz, locy, circle_size_nano_e, line_width_nano_e, line_color_nano_e, fill_color_nano_e);
		nele++;
	      }
	      else if(flag_electron==1){
		set_nano_circle(ntot, locz, locy, circle_size_nano_e, line_width_nano_e, line_color_nano_p, fill_color_nano_p);
		npos++;
	      }
	      else{
		set_nano_circle(ntot, locz, locy, circle_size_nano_q, line_width_nano_q, line_color_nano_q, fill_color_nano_q);
		ncharged++;
	      }
	      if(rejection_display && (flag_electron==-1 || flag_electron==1)){
		draw_rejection_step(rejection_step, locz, locy);
	      }
	      ntot++;
	    }
	  }
	}
      }
    }
    d_cnt->Delete();
  
    draw_nano_circle(ntot, nano_display);
    draw_nano_info(nano_display, nele, npos, ncharged, rejection_display);
  }
  if(efct_display>=1) draw_S_eff();
}

// End of File //
