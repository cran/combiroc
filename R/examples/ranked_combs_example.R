demo_data # combiroc built-in demo data (proteomics data from Zingaretti et al. 2012 - PMC3518104)

combs <- combi(data= demo_data, signalthr=450, combithr=1)  # compute combinations

combs_SE_SP <- se_sp(data=demo_data, combinations_table=combs) # compute SE and SP
                                                               # of each combination



# To rank combinations by Youden index and filter-out the ones with SE < min_SE and SP < min_SP

rc <- ranked_combs(data= demo_data, combo_table= combs_SE_SP,
                       case_class='A', min_SE=40, min_SP=80)
rc$table # to visualize the selected gold combinations through a data.frame
rc$bubble_chart # to visualize the selected gold combinations through a data.frame
