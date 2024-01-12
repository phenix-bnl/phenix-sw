#! /usr/bin/perl

open(IN1, $ARGV[0]);
open(OUT1, ">$ARGV[1]");

$line_counter = 0;
@quantity =(" ");

print(OUT1 "Run\t");
print(OUT1 "Mean_of_hits_per_event\t");
print(OUT1 "Sigma_of_hits_per_event\t");
print(OUT1 "Mean_of_number_of_rings_at_Sector_0\t");
print(OUT1 "Mean_of_number_of_rings_at_Sector_1\t");
print(OUT1 "Mean_of_number_of_rings_at_Sector_2\t");
print(OUT1 "Mean_of_number_of_rings_at_Sector_3\t");
print(OUT1 "Mean_of_Z_position_of_ring_at_Sector_0\t");
print(OUT1 "Mean_of_Z_position_of_ring_at_Sector_1\t");
print(OUT1 "Mean_of_Z_position_of_ring_at_Sector_2\t");
print(OUT1 "Mean_of_Z_position_of_ring_at_Sector_3\t");
print(OUT1 "Sigma_of_Z_position_of_ring_at_Sector_0\t");
print(OUT1 "Sigma_of_Z_position_of_ring_at_Sector_1\t");
print(OUT1 "Sigma_of_Z_position_of_ring_at_Sector_2\t");
print(OUT1 "Sigma_of_Z_position_of_ring_at_Sector_3\t");
print(OUT1 "Mean_of_Phi_position_of_ring_at_Sector_0\t");
print(OUT1 "Mean_of_Phi_position_of_ring_at_Sector_1\t");
print(OUT1 "Mean_of_Phi_position_of_ring_at_Sector_2\t");
print(OUT1 "Mean_of_Phi_position_of_ring_at_Sector_3\t");
print(OUT1 "Sigma_of_Phi_position_of_ring_at_Sector_0\t");
print(OUT1 "Sigma_of_Phi_position_of_ring_at_Sector_1\t");
print(OUT1 "Sigma_of_Phi_position_of_ring_at_Sector_2\t");
print(OUT1 "Sigma_of_Phi_position_of_ring_at_Sector_3\t");
print(OUT1 "Mean_of_NPE_of_ring_at_Sector_0\t");
print(OUT1 "Mean_of_NPE_of_ring_at_Sector_1\t");
print(OUT1 "Mean_of_NPE_of_ring_at_Sector_2\t");
print(OUT1 "Mean_of_NPE_of_ring_at_Sector_3\t");
print(OUT1 "Sigma_of_NPE_of_ring_at_Sector_0\t");
print(OUT1 "Sigma_of_NPE_of_ring_at_Sector_1\t");
print(OUT1 "Sigma_of_NPE_of_ring_at_Sector_2\t");
print(OUT1 "Sigma_of_NPE_of_ring_at_Sector_3\t");
print(OUT1 "Mean_of_Hits_in_a_PMT\t");
print(OUT1 "Sigma_of_Hits_in_a_PMT\t");
print(OUT1 "Number_of_Error_pmt_section_in_nHits\t");
print(OUT1 "Mean_of_Hits_in_a_PMT_in_limited\t");
print(OUT1 "Sigma_of_Hits_in_a_PMT_in_limited\t");
print(OUT1 "Number_of_Error_pmt_section_in_nHits_limited\t");
print(OUT1 "Mean_of_t0_mean_in_a_PMT\t");
print(OUT1 "Sigma_of_t0_mean_in_a_PMT\t");
print(OUT1 "Number_of_Error_pmt_section_in_t0_mean\t");
print(OUT1 "Mean_of_t0_sigma_in_a_PMT\t");
print(OUT1 "Sigma_of_t0_sigma_in_a_PMT\t");
print(OUT1 "Number_of_Error_pmt_section_in_t0_sigma\t");
print(OUT1 "Single-pe_peak_for_part_0\t");
print(OUT1 "Single-pe_peak_for_part_1\t");
print(OUT1 "Single-pe_peak_for_part_2\t");
print(OUT1 "Single-pe_peak_for_part_3\t");
print(OUT1 "Single-pe_peak_for_part_4\t");
print(OUT1 "Single-pe_peak_for_part_5\t");
print(OUT1 "Single-pe_peak_for_part_6\t");
print(OUT1 "Single-pe_peak_for_part_7\t");
print(OUT1 "Single-pe_peak_for_part_8\t");
print(OUT1 "Single-pe_peak_for_part_9\t");
print(OUT1 "Single-pe_peak_for_part_10\t");
print(OUT1 "Single-pe_peak_for_part_11\t");
print(OUT1 "Single-pe_peak_for_part_12\t");
print(OUT1 "Single-pe_peak_for_part_13\t");
print(OUT1 "Single-pe_peak_for_part_14\t");
print(OUT1 "Single-pe_peak_for_part_15\t");
print(OUT1 "Single-pe_peak_for_part_16\t");
print(OUT1 "Single-pe_peak_for_part_17\t");
print(OUT1 "Single-pe_peak_for_part_18\t");
print(OUT1 "Single-pe_peak_for_part_19\t");
print(OUT1 "Single-pe_peak_for_part_20\t");
print(OUT1 "Single-pe_peak_for_part_21\t");
print(OUT1 "Single-pe_peak_for_part_22\t");
print(OUT1 "Single-pe_peak_for_part_23\t");
print(OUT1 "Single-pe_peak_for_part_24\t");
print(OUT1 "Single-pe_peak_for_part_25\t");
print(OUT1 "Single-pe_peak_for_part_26\t");
print(OUT1 "Single-pe_peak_for_part_27\t");
print(OUT1 "Single-pe_peak_for_part_28\t");
print(OUT1 "Single-pe_peak_for_part_29\t");
print(OUT1 "Single-pe_peak_for_part_30\t");
print(OUT1 "Single-pe_peak_for_part_31\t");
print(OUT1 "Number_of_Error_pmt_in_charge_gain\t");
print(OUT1 "Statistics\t");
print(OUT1 "More Times Run\t");
print(OUT1 "CRK_Total_Status\t");
print(OUT1 "CRK_GAIN_ERRORS\t");
print(OUT1 "CRK_HIT_ERRORS\t");
print(OUT1 "CRK_Status\n");

while(<IN1>){
  $inputfile = $_;
  open(IN2, $inputfile);

  while(<IN2>){
    chop();
    @lines =split();
#    print @lines;
    if($lines[1] =~ CRK && $lines[2] =~QA && $lines[3] =~Summary){
#       print @lines;
       $line_counter=1;
       next;
    }
    $quantity[$line_counter-1]=$lines[1];

    if($line_counter>0){
      $line_counter++;
    }
  }
  close(IN2);

  $line_counter=0;

  for($i=1; $i<80; $i++){
    printf(OUT1 "%s\t",$quantity[$i]);
  }
  printf(OUT1 "%d\t",($quantity[79]>>12)&0x03f);
  printf(OUT1 "%d\t",($quantity[79]>>4)&0x0ff);
  printf(OUT1 "%d\t",($quantity[79])&0x0f);
  printf(OUT1 "\n");
}

close(OUT1);
close(IN1);

