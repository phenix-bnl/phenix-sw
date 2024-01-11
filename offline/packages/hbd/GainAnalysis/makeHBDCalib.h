TChain *tree;

TBranch *b_RunInfo;
TLeaf *l_RunInfo_run,*l_RunInfo_seg,*l_RunInfo_event,*l_RunInfo_zvtx,
      *l_RunInfo_bbcS,*l_RunInfo_bbcN,*l_RunInfo_nDCtracks;

TBranch *b__HBDCells;
TLeaf *l__HBDCells;

TBranch *b_HBDCells;
TLeaf *l_HBDCells_charge,*l_HBDCells_sector,*l_HBDCells_arm,*l_HBDCells_module,*l_HBDCells_padnum,*l_HBDCells_row,*l_HBDCells_col,*l_HBDCells_trackflag,*l_HBDCells_locy,*l_HBDCells_locz,*l_HBDCells_globx,*l_HBDCells_globy,*l_HBDCells_globz,*l_HBDCells_globphi;

TBranch *b__TrackProj;
TLeaf *l__TrackProj;

TBranch *b_TrackProj;
TLeaf *l_TrackProj_x,*l_TrackProj_y,*l_TrackProj_z,*l_TrackProj_phi;


void initialize()
{
  b_RunInfo = tree->GetBranch("RunInfo");
  if(b_RunInfo) {
    l_RunInfo_run       = (TLeaf *) b_RunInfo->GetLeaf("run");
    l_RunInfo_seg       = (TLeaf *) b_RunInfo->GetLeaf("seg");
    l_RunInfo_event     = (TLeaf *) b_RunInfo->GetLeaf("event");
    l_RunInfo_zvtx      = (TLeaf *) b_RunInfo->GetLeaf("zvtx");
    l_RunInfo_bbcS      = (TLeaf *) b_RunInfo->GetLeaf("bbcS");
    l_RunInfo_bbcN      = (TLeaf *) b_RunInfo->GetLeaf("bbcN");
    l_RunInfo_nDCtracks = (TLeaf *) b_RunInfo->GetLeaf("nDCtracks");}

  b__HBDCells = tree->GetBranch("_HBDCells");
  if(b__HBDCells) {
    l__HBDCells = (TLeaf *) b__HBDCells->GetLeaf("_HBDCells");}
  
  b_HBDCells = tree->GetBranch("HBDCells");
  if(b_HBDCells) {
    l_HBDCells_charge  = (TLeaf *) b_HBDCells->GetLeaf("charge");
    l_HBDCells_sector  = (TLeaf *) b_HBDCells->GetLeaf("sector");
    l_HBDCells_arm     = (TLeaf *) b_HBDCells->GetLeaf("arm");
    l_HBDCells_module  = (TLeaf *) b_HBDCells->GetLeaf("module");
    l_HBDCells_padnum  = (TLeaf *) b_HBDCells->GetLeaf("padnum");
    l_HBDCells_row     = (TLeaf *) b_HBDCells->GetLeaf("row");
    l_HBDCells_col     = (TLeaf *) b_HBDCells->GetLeaf("col");
    l_HBDCells_trackflag = (TLeaf *) b_HBDCells->GetLeaf("trackflag");
    l_HBDCells_locy    = (TLeaf *) b_HBDCells->GetLeaf("locy");
    l_HBDCells_locz    = (TLeaf *) b_HBDCells->GetLeaf("locz");
    l_HBDCells_globx   = (TLeaf *) b_HBDCells->GetLeaf("globx");
    l_HBDCells_globy   = (TLeaf *) b_HBDCells->GetLeaf("globy");
    l_HBDCells_globz   = (TLeaf *) b_HBDCells->GetLeaf("globz");
    l_HBDCells_globphi = (TLeaf *) b_HBDCells->GetLeaf("globphi");}
  
  b__TrackProj = tree->GetBranch("_TrackProj");
  if(b__TrackProj) {
    l__TrackProj = (TLeaf *) b__TrackProj->GetLeaf("_TrackProj");}
  
  b_TrackProj = tree->GetBranch("TrackProj");
  if(b_TrackProj) {
    l_TrackProj_x   = (TLeaf *) b_TrackProj->GetLeaf("x");
    l_TrackProj_y   = (TLeaf *) b_TrackProj->GetLeaf("y");
    l_TrackProj_z   = (TLeaf *) b_TrackProj->GetLeaf("z");
    l_TrackProj_phi = (TLeaf *) b_TrackProj->GetLeaf("phi");}
}
