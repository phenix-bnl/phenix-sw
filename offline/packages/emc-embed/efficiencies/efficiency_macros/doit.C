{

emcEfficiency eff; 

eff.open("merged_0_500_runs_run2.root");

eff.setVerbose(0);

eff.run();

eff.close();

eff.efficiency_all_cuts();

}
