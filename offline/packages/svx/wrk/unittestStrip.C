void unittestStrip(){
  gSystem->Load("libsvx.so");

  float m_stripixel_sAQ                = 0.109; // based on beam test
  float m_stripixel_sNOISE             = 0; //10.2; // based on measurement (2008 June)
  float m_stripixel_adcthre_zs         = 1; //31; // 3 sigma of sNOISE
  float m_stripixel_adcthre_rawhit     = 1; //31; // 3 sigma of sNOISE
  float m_stripixel_adcthre_rawhit_sum = 1; //50; // a speculated value


  SvxStrip11v2 *strip = new SvxStrip11v2(0, 2, 0, 0);
  strip->set_sAQ               (m_stripixel_sAQ);
  strip->set_sNOISE            (m_stripixel_sNOISE);
  strip->set_adcthre_zs        (m_stripixel_adcthre_zs);
  strip->set_adcthre_rawhit    (m_stripixel_adcthre_rawhit);
  strip->set_adcthre_rawhit_sum(m_stripixel_adcthre_rawhit_sum);

  strip->test_CollectNeighborRawhits();
  strip->test_makeGrouphits();

}
