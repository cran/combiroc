demo_data # combiroc built-in demo data (proteomics data from Zingaretti et al. 2012 - PMC3518104)

combs <- combi(data= demo_data, signalthr=450, combithr=1)  # compute combinations


# To compute sensitivity and specificity of each combination

combs_SE_SP <- se_sp(data=demo_data, combinations_table=combs)
