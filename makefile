# call : make "name of program"
FC=gfortran
#FC=ifort
#FFLAG1= -mcmodel=medium -ffixed-line-length-none
#libsac=/home/junxie/opt/sac/lib/sacio.a 
#saclib=/home/junxie/opt/sac/lib/libsac.a 
#sources=getzh.f realft.f getalpha.f gaussfilter.f envelope.f four1.f 
#sources=cc.f clogc.f do_cc.f wrsac.f readsac.f
#objects=$(sources:.f=.o)
objects=do_ncc.o clogc.o do_3COM_CC.o zfour.o hilbert.o pcc.o do_pcc.o analytic.o sacio.o
objects1=do_ncc.o clogc.o do_CC_short.o zfour.o hilbert.o pcc.o do_pcc.o analytic.o sacio.o
objects2=do_ncc.o clogc.o do_3COM_CC_new.o zfour.o hilbert.o pcc.o do_pcc.o analytic.o sacio.o
objects3=do_ncc.o clogc.o do_3COM_CC_2016_11_04.o zfour.o hilbert.o pcc.o do_pcc.o analytic.o sacio.o
objects4=do_ncc_2017_01_06.o clogc.o do_3COM_CC_2017_01_06.o zfour.o hilbert.o pcc.o do_pcc.o analytic.o sacio.o
objects5=do_ncc.o clogc.o do_CC_short_2017_02_07.o rot9com.o zfour.o hilbert.o pcc.o do_pcc.o analytic.o sacio.o
objects6=do_ncc_2017_01_06.o clogc.o do_3COM_CC_2017_02_07.o rot9com.o zfour.o hilbert.o pcc.o do_pcc.o analytic.o sacio.o
objects7=do_ncc.o clogc.o do_3COM_CC_2017_02_16.o rot9com.o zfour.o hilbert.o pcc.o do_pcc.o analytic.o sacio.o
objects8=do_ncc_2017_01_06.o clogc.o do_3COM_CC_2017_02_16_2.o rot9com.o zfour.o hilbert.o pcc.o do_pcc.o analytic.o sacio.o
objects9=do_ncc.o clogc.o do_3COM_CC_2017_02_16_1.o rot9com.o zfour.o hilbert.o pcc.o do_pcc.o analytic.o sacio.o
objects10=do_ncc.o clogc.o do_3COM_CC_2017_02_16_3.o rot9com.o zfour.o hilbert.o pcc.o do_pcc.o analytic.o sacio.o
objects11=do_ncc_2017_01_06.o clogc.o do_3COM_CC_1list_2017_03_08.o rot9com.o zfour.o hilbert.o pcc.o do_pcc.o analytic.o sacio.o
objects12=do_ncc_2017_01_06.o clogc.o do_3COM_CC_2list_2017_03_08.o rot9com.o zfour.o hilbert.o pcc.o do_pcc.o analytic.o sacio.o
executable=do_3COM_CC
all:sacio.mod $(executable) do_CC_short do_3COM_CC_new do_3COM_CC_2016_11_04 do_3COM_CC_2017_01_06\
	do_CC_short_2017_02_07 do_3COM_CC_2017_02_07 do_3COM_CC_2017_02_16 do_3COM_CC_2017_02_16_2\
	do_3COM_CC_2017_02_16_1 do_3COM_CC_2017_02_16_3 do_3COM_CC_1list_2017_03_08\
	do_3COM_CC_2list_2017_03_08
.f.o:
	$(FC) $(FFLAG1) $< -c
%.o:%.f90
	$(FC) $(FFLAG1) $< -c
sacio.mod:sacio.f90
	$(FC) $< -c
$(executable):$(objects)
	$(FC) $(FFLAG1) $(objects) $(libsac) $(saclib) -o $@
do_CC_short:$(objects1)
	$(FC) $(FFLAG1) $(objects1) $(libsac) $(saclib) -o $@
do_3COM_CC_new:$(objects2)
	$(FC) $(FFLAG1) $(objects2) $(libsac) $(saclib) -o $@
do_3COM_CC_2016_11_04:$(objects3)
	$(FC) $(FFLAG1) $(objects3) $(libsac) $(saclib) -o $@
do_3COM_CC_2017_01_06:$(objects4)
	$(FC) $(FFLAG1) $^ $(libsac) $(saclib) -o $@
do_CC_short_2017_02_07:$(objects5)
	$(FC) $(FFLAG1) $^ $(libsac) $(saclib) -o $@
do_3COM_CC_2017_02_07:$(objects6)
	$(FC) $(FFLAG1) $^ $(libsac) $(saclib) -o $@
do_3COM_CC_2017_02_16:$(objects7)
	$(FC) $(FFLAG1) $^ $(libsac) $(saclib) -o $@
do_3COM_CC_2017_02_16_2:$(objects8)
	$(FC) $(FFLAG1) $^ $(libsac) $(saclib) -o $@
do_3COM_CC_2017_02_16_1:$(objects9)
	$(FC) $(FFLAG1) $^ $(libsac) $(saclib) -o $@
do_3COM_CC_2017_02_16_3:$(objects10)
	$(FC) $(FFLAG1) $^ $(libsac) $(saclib) -o $@
do_3COM_CC_1list_2017_03_08:$(objects11)
	$(FC) $(FFLAG1) $^ $(libsac) $(saclib) -o $@
do_3COM_CC_2list_2017_03_08:$(objects11)
	$(FC) $(FFLAG1) $^ $(libsac) $(saclib) -o $@
install:
	cp $(executable) do_3COM_CC_2017_02_16 do_3COM_CC_2017_02_16_2 do_3COM_CC_2017_02_16_1  do_3COM_CC_2017_02_07 do_CC_short ../../bin
	cp do_CC_short_2017_02_07 do_3COM_CC_new do_3COM_CC_2017_02_16_3 do_3COM_CC_2016_11_04 do_3COM_CC_2017_01_06 ../../bin/
	cp do_3COM_CC_2list_2017_03_08           ../../bin/
	cp do_3COM_CC_1list_2017_03_08           ../../bin/
uninstall:
	-rm ~/bin/$(excecutable)
clean:
	-rm *.o *.mod
