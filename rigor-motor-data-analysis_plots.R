# # # # # # # # # # # # # # #
# RIGOR MOTOR DATA ANALYSIS #
# PLOTS                     #
# # # # # # # # # # # # # # #

#### To-do ####

# Automatically save output plots as PNG files
# Plot many sample MSD curves together, along with an average; likewise for density curves; likewise for x positions

#### Notes ####

# modified_try1_3_5-1.txt, modified_try1_3_6-2.txt, modified_try1_3_6-3.txt, and modified_try1_3_10-4
#  have non-zero start times - is there a reason?
# Not all paths are (on average) vertical:
#  most seem to be <20 degrees from vertical;
#  path 12 is L-shaped;
#  path 22 is ~30 degrees from vertical;
#  path 23 is nearly horizontal.
# y's are approximately Gaussian except for paths 3, 8, 12, 13, 19, 23 (maybe 9, 21, and 24)

# Linear model works well for finding the microtubule for all paths except 12 and 23.
# However, as noted in the relevant section, this solution is "good"/workable but not the *best*.
# Try to find a method independent of the orientation of the data (or, equivalently, the microtubule).

# Paths have length(t) == 3000 except for...
# 7: 2371 (1-2371),  12: 2174 (827-3000),  14: 746 (1-746),  15: 1853 (1148-3000),
# 16: 522 (1993-2514),  22: 2263 (1-2263),  24: 1311 (1690-3000)
