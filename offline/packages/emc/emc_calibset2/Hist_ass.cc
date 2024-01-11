#include "Hist_ass.hh"

ClassImp(Hist_ass)
//=====================================================================
Hist_ass::Hist_ass(){
};
//=====================================================================
Hist_ass::Hist_ass(char* pname,char* ptitle,char* opt) : Hist(pname,ptitle){
  bool status;
  if( strstr(opt,"hide") ){
    status = TH1::AddDirectoryStatus();
    TH1::AddDirectory(false);
  }
  //=============================================================================
  //=============================================================================
  isect = 8;
  while( isect-- ){
    if( IsValidSector(isect) ){
      //
      // --------- Association
      //
      sprintf(hname,"%s_h_pdy_sect%d",pname,isect);
      sprintf(htitle,"%s:Y position difference in Sect%d",ptitle,isect);
      Register( h_pdy_sect[isect] = new TH1F(hname,htitle,200,-50,50) );
      //
      sprintf(hname,"%s_h_pdz_sect%d",pname,isect);
      sprintf(htitle,"%s:Z position difference in Sect%d",ptitle,isect);
      Register( h_pdz_sect[isect] = new TH1F(hname,htitle,200,-50,50) );
      //
      sprintf(hname,"%s_h2_pdzy_sect%d",pname,isect);
      sprintf(htitle,"%s:ZY position difference in Sect%d",ptitle,isect);
      Register( h2_pdzy_sect[isect] = new TH2F(hname,htitle,50,-50,50,50,-50,50) );
      //
      ipid = 8;
      while( ipid-- ){
	sprintf(hname,"%s_h2_pdzmom_sectpid%d_%d",pname,isect,ipid);
	sprintf(htitle,"%s:Z position difference vs mom in Sect%d Pid%d",ptitle,isect,ipid);
	Register( h2_pdzmom_sectpid[isect][ipid] = new TH2F(hname,htitle,30,0,3,200,-50,50) );
	sprintf(hname,"%s_h2_pdymom_sectpid%d_%d",pname,isect,ipid);
	sprintf(htitle,"%s:Y position difference vs mom in Sect%d Pid%d",ptitle,isect,ipid);
	Register( h2_pdymom_sectpid[isect][ipid] = new TH2F(hname,htitle,30,0,3,200,-50,50) );
	//
	sprintf(hname,"%s_h2_pdzmom_ang_sectpid%d_%d",pname,isect,ipid);
	sprintf(htitle,"%s:Z position difference vs sin(angle) in Sect%d Pid%d",ptitle,isect,ipid);
	Register( h2_pdzmom_ang_sectpid[isect][ipid] = new TH2F(hname,htitle,100,-0.5,0.5,200,-50,50) );
	sprintf(hname,"%s_h2_pdymom_ang_sectpid%d_%d",pname,isect,ipid);
	sprintf(htitle,"%s:Y position difference vs sin(angle) in Sect%d Pid%d",ptitle,isect,ipid);
	Register( h2_pdymom_ang_sectpid[isect][ipid] = new TH2F(hname,htitle,30,-0.5,0.5,200,-50,50) );
	//
	sprintf(hname,"%s_h2_pdzmom_perp_sectpid%d_%d",pname,isect,ipid);
	sprintf(htitle,"%s:Z position difference perp vs mom in Sect%d Pid%d",ptitle,isect,ipid);
	Register( h2_pdzmom_perp_sectpid[isect][ipid] = new TH2F(hname,htitle,30,0,3,200,-50,50) );
	sprintf(hname,"%s_h2_pdymom_perp_sectpid%d_%d",pname,isect,ipid);
	sprintf(htitle,"%s:Y position difference perp vs mom in Sect%d Pid%d",ptitle,isect,ipid);
	Register( h2_pdymom_perp_sectpid[isect][ipid] = new TH2F(hname,htitle,30,0,3,200,-50,50) );
      }
      //
      sprintf(hname,"%s_h2_posproj_sect%d",pname,isect);
      sprintf(htitle,"%s:XY emc local position in Sect%d",ptitle,isect);
      if( isect >= 6 )
	Register( h2_posproj_sect[isect] = new TH2F(hname,htitle,208,-4,100,112,-4,52) );
      else
	Register( h2_posproj_sect[isect] = new TH2F(hname,htitle,160,-4,76,88,-4,40) );
      //
      // --------- Particle ID
      sprintf(hname,"%s_h2_mmom_sect%d",pname,isect);
      sprintf(htitle,"%s:Mom vs Mass in Sect%d",ptitle,isect);
      Register( h2_mmom_sect[isect] = new TH2F(hname,htitle,400,-2,2,200,0,2) );
      sprintf(hname,"%s_h2_m2mom_sect%d",pname,isect);
      sprintf(htitle,"%s:Mom vs Mass^{2} in Sect%d",ptitle,isect);
      Register( h2_m2mom_sect[isect] = new TH2F(hname,htitle,400,-2,2,300,-1,2) );
      //
    }
  }
  //=============================================================================
  //=============================================================================
  if( strstr(opt,"hide") ){
    TH1::AddDirectory(status);
  }
};
//=====================================================================
bool Hist_ass::Fill(Global& glb,Track& trk,Clust& clt,Pid& emcpid,float weight=1.){
  Hist::Fill(glb,trk,clt,emcpid);
  if( trk.quality > 20 && trk.ptot < 10 ){
    // --------------------------- Hit position ------------------------
    //float emcproj_pos[3];
    //emcgeometry->GlobalToLocal(trk.proj[0],trk.proj[1],trk.proj[2],trk.sector,
    //emcproj_pos[0],emcproj_pos[1],emcproj_pos[2]);
    //emcproj_pos[0] /= twr_size[isect][0];
    //emcproj_pos[1] /= twr_size[isect][1];
    //cout<<"  emcproj_pos = "<<emcproj_pos[0]<<","<<emcproj_pos[1]<<","<<emcproj_pos[2]<<endl;
    
    // --------- Association
    if( clt.e > 0.25 && trk.ptot > 0.3 ){
      h_pdy_sect[isect]->Fill(clt.pos[1] - trk.proj[1]);
      h_pdz_sect[isect]->Fill(clt.pos[2] - trk.proj[2]);
      h2_pdzy_sect[isect]->Fill(clt.pos[2] - trk.proj[2],
				clt.pos[1] - trk.proj[1]);
    }
    if( ipid > -1 ){
      h2_pdymom_sectpid[isect][ipid]->Fill(trk.ptot,clt.pos[1] - trk.proj[1]);
      h2_pdzmom_sectpid[isect][ipid]->Fill(trk.ptot,clt.pos[2] - trk.proj[2]);
      if( fabs(trk.dir[1]) < 0.1 ) // sin 6deg = 0.1
	h2_pdymom_perp_sectpid[isect][ipid]->Fill(trk.ptot,clt.pos[1] - trk.proj[1]);
      if( fabs(trk.dir[2]) < 0.1 )
	h2_pdzmom_perp_sectpid[isect][ipid]->Fill(trk.ptot,clt.pos[2] - trk.proj[2]);
      h2_pdymom_ang_sectpid[isect][ipid]->Fill(trk.dir[1],clt.pos[1] - trk.proj[1]);
      h2_pdzmom_ang_sectpid[isect][ipid]->Fill(trk.dir[2],clt.pos[2] - trk.proj[2]);
    }
    if( fabs(clt.pos[1]-trk.proj[1])<20 &&
	fabs(clt.pos[2]-trk.proj[2])<20 ){
      // --------- Particle ID
      h2_mmom_sect[isect]->Fill(emcpid.signedmom,emcpid.mass);
      h2_m2mom_sect[isect]->Fill(emcpid.signedmom,emcpid.mass2);
    }
  }
  return true;
};
//=====================================================================
//=====================================================================
void Hist_ass::Draw(TCanvas* c1,TPostScript* ps){
  int canvas_num =0 ;
  //=================================================================== Position Difference
  cout<<" Plotting position difference  "<<endl;
  canvas_num = 0;
  isect = 8;
  c1->Divide(3,3);
  while( isect-- ){
    if( IsValidSector(isect) ){
      c1->cd( ++canvas_num );
      h2_pdzy_sect[isect]->Draw("surf");
      c1->cd( ++canvas_num );
      h_pdz_sect[isect]->Draw();
      c1->cd( ++canvas_num );
      h_pdy_sect[isect]->Draw();
    }
  }
  c1->cd();
  c1->Update();
  getchar();
  ps->NewPage();

  //=================================================================== Particle ID
  cout<<" Particle ID "<<endl;
  canvas_num = 0;
  isect = 8;
  c1->Clear();
  c1->Divide(2,3);
  while( isect-- ){
    if( IsValidSector(isect) ){
      c1->cd( ++canvas_num );
      h2_m2mom_sect[isect]->SetMinimum(0);
      h2_m2mom_sect[isect]->Draw("colz");
      c1->cd( ++canvas_num );
      h2_mmom_sect[isect]->SetMinimum(0);
      h2_mmom_sect[isect]->Draw("colz");
    }
  }
  c1->cd();
  c1->Update();
  ps->NewPage();
  getchar();
  cout<<" Closing trk_assplot.ps .. "<<endl;
  ps->Close();
}
//=====================================================================
//=====================================================================
//=====================================================================
//=====================================================================

