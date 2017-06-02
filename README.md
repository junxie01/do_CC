# do_CC
# do NCF calculation:
#              executable file:"do_2COM_CC_2017_02_15" in directory "cc-love/cc/src", where other versions can be found (not necessary to pay attention to).
#              source file    :do_2COM_CC_2017_02_15.f90 in directory "cc-love/cc/src", comple with make. See the makefile for subrountines been called.
#                              This code calculate NN, NE, EN, EE component NCFs, you can either rotate them to tt,tr,rt,rr or not with a single option.
#              dow to run     :A parameter file is needed, with the form:
#                              "station.list1
#                               station.list2
#                               year_begin day_begin year_end day_end
#                               length_of_sacfile_with_seconds multiplication_with_percent component length_of_outputfile_in_dots do_rotation_or_not(1/0)
#                               SAC_file_directory
#                               Output_file_directory"
#                              See the example: for_cc
#              attention      :this code calculate the NCF between stations in list1 with those in list2, just depend on the loop method, you can modify the code as you wish.
