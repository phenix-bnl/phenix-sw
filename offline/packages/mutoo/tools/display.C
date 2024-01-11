{
   bar = new TControlBar("vertical", "Mutoo_Display");
   bar->AddButton("next event","drun(1)");
   bar->AddButton("dump_stub","dump_stub()");
   bar->AddButton("dump_trk","dump_trk()");
   bar->AddButton("draw_octant(0)","draw_octant(0)");
   bar->AddButton("draw_octant(1)","draw_octant(1)");
   bar->AddButton("draw_plane(0,0)","draw_plane(0,0)");
   bar->AddButton("draw_plane(0,1)","draw_plane(0,1)");
   bar->AddButton("draw_plane(0,2)","draw_plane(0,2)");
   bar->AddButton("draw_plane(0,3)","draw_plane(0,3)");
   bar->AddButton("draw_plane(0,4)","draw_plane(0,4)");
   bar->AddButton("draw_plane(0,5)","draw_plane(0,5)");
   bar->AddButton("draw_plane(0,6)","draw_plane(0,6)");
   bar->AddButton("draw_plane(0,7)","draw_plane(0,7)");
   bar->AddButton("draw_plane(1,0)","draw_plane(1,0)");
   bar->AddButton("draw_plane(1,1)","draw_plane(1,1)");
   bar->AddButton("draw_plane(1,2)","draw_plane(1,2)");
   bar->AddButton("draw_plane(1,3)","draw_plane(1,3)");
   bar->AddButton("draw_plane(1,4)","draw_plane(1,4)");
   bar->AddButton("draw_plane(1,5)","draw_plane(1,5)");
   bar->AddButton("draw_plane(1,6)","draw_plane(1,6)");
   bar->AddButton("draw_plane(1,7)","draw_plane(1,7)");
   bar->Show();
   gROOT->SaveContext();
}

