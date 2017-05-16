program do_cc
use sacio
implicit none
type (sac_head) :: sachead1,sachead2
integer,parameter:: nptsmax=10000000,nsmax=1000000
integer:: niargc,nsmpl,nerr,shift,npts
character(180):: sacfile1,sacfile2,output_filename
character(10):: kstnm1,kstnm2,kstnm,args
real:: sig1(nptsmax),sig2(nptsmax),signcc(nptsmax)
if(iargc().ne.4)stop 'usage: docc sacfile1 sacfile2 shift [output file name]'
call getarg(1,sacfile1)
call getarg(2,sacfile2)
call getarg(3,args)
read(args,'(i20)')shift
call getarg(4,output_filename)
call read_sachead(sacfile1,sachead1,nerr)
call read_sac(sacfile1,sig1,sachead1,nerr)
if(nerr.eq.-1)stop 'Error reading in sacfile: '
call read_sachead(sacfile2,sachead2,nerr)
call read_sac(sacfile2,sig2,sachead2,nerr)
if(nerr.eq.-1)stop 'Error reading in sacfile: '
call do_ncc(sachead1%npts,sig1,sig2,signcc,shift,sachead1%delta)
nsmpl=2*shift+1
call write_ncf_sac(output_filename,signcc,sachead1,sachead2,nsmpl,1,nerr)
end program
