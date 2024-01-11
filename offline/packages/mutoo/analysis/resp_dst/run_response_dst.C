void run_response_dst(int nevnt = 1)
{
  //  const char* datafile = "/phenix/data07/zhangc/sandbox/response_r/mutoo_simdst_newpisa_p0_nms_3500.root";
  const char* datafile = "mutoo_slowsim_dst.root";
  //  const char* datafile = "/phenix/data07/zhangc/sandbox/response_r/mutoo_simdst_ms_p0_jpsi_mumu.root";
  dfileopen(datafile);
  dstout_fopen("test.root");
  drun(nevnt);
  //  end_all();
}

