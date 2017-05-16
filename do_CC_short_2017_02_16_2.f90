program do_3COM_CC
! do 3 component NCF calculation
use sacio
implicit none
type(sac_head):: sachead1,sachead2
integer nsmpl,nerr,imark
integer nzhour,nzmin,nzsec
integer year_b,year_e,day_b,day_e
integer dhour,nst,i,nh,j,nnh,nlen,cn
integer jday,npts,npt2,icc(10000),itemp
integer ibeg1,ibeg2,iend1,iend2,ibeg,iend
integer,parameter:: nmax=4000000,nstmax=200000
integer dsec,multpt,doncc,dopcc,dorot
integer iy,id,ih,ist,nsamp,is,it,dotl,dopw,dotf,iseg
integer begday,endday,ic1,ic2,numcc(3,3),nseg,dseg,nptseg
real t1,t2,wu,dt
real hilb(nmax),theta
real pi,twpi,cyc,f1,f2
real stla,stlo,evla,evlo
real outncctl(nmax,3,3),outpcctl(nmax,3,3)
real outnccpw(nmax,3,3),outncctf(nmax,3,3)
real outpccpw(nmax,3,3),outpcctf(nmax,3,3)
real sig1(nmax,3),sig2(nmax,3),signcc(nmax,3,3),sigpcc(nmax,3,3)
real signccrot(nmax,3,3),sigpccrot(nmax,3,3)
complex pw_ncc_sig(nmax,3,3)
complex pw_pcc_sig(nmax,3,3)
character (180)targetd
character (4)append
character (180)command
character (2)note(3,3)
character (3)com(3),comm
character (20)year_day,nd
character (80)name1z,name1n,name1e
character (80)sac1(3),sac2(3),sacfile
character (2)net1(nstmax),net2(nstmax)
character (12)sta1(nstmax),sta2(nstmax)
character (80)name1,name2,dir_day,dir
character (80)name2z,name2n,name2e,name
character (100)output_ncc(3,3),output_pcc(3,3),output
character (100)output_tl_pcc,output_pw_pcc
character (80)dirinn,dirout,input,list,output_tmp
logical ext
wu=2.0
if (iargc().ne.1)then
   write(*,*)'Usage: do_3COM_CC param.dat '
   write(*,*)'param.dat is like:'
   write(*,*)'tobecomputed.list'
   write(*,*)'year_b day_b year_e day_e'
   write(*,*)'cn,com,append,nlen'
   write(*,*)'dsec,multpt'
   write(*,*)'doncc,dopcc,dorot'
   write(*,*)'/inputdir/'
   write(*,*)'/outputdir/'
   stop
endif
call getarg(1,input)
open(10,file=input)
read(10,*)list
read(10,*)year_b,day_b,year_e,day_e
read(10,*)cn,comm,append,npt2
read(10,*)dsec,multpt
read(10,*)doncc,dopcc,dorot
read(10,*)dirinn
read(10,*)dirout
close(10)
! read the parameters done
note(1,1)="zz"
note(1,2)="zr"
note(1,3)="zt"
note(2,1)="rz"
note(2,2)="rr"
note(2,3)="rt"
note(3,1)="tz"
note(3,2)="tr"
note(3,3)="tt"
if(doncc.eq.0.and.dopcc.eq.0)stop 'Hey, dude, what do you want to do?'
if(cn.eq.3)then
   com(1)=trim(comm)//'Z'
   com(2)=trim(comm)//'N'
   com(3)=trim(comm)//'E'
else
   com(1)=comm
endif
!write(*,'("Overlapping ",i2.2,"%")')multpt
open(11,file=list)   ! read in station pairs
do i=1,nstmax
   read(11,*,err=13,end=13) net1(i),sta1(i),net2(i),sta2(i)
enddo
13 close(11)
nst=i-1
if(multpt.ge.100)stop "Hi the overlapping percentage is too big!"
write(*,'("There are ",i0," station pairs")')nst
write(*,'("Do CC from ",i0,"/",i0," to ",i0,"/",i0)')year_b,day_b,year_e,day_e
write(*,'("Number of component is ",1i3)')cn
write(*,'("Component is ",1a)')comm
write(*,'("Append=",1a)')append
!if(iden.eq.1)write(*,*)'Do PWS'
!if(iden.ne.1)write(*,*)'Do linear stack'
write(name,'(i0,"_",i3.3,"_",i0,"_",i3.3)')year_b,day_b,year_e,day_e
nsmpl=2*npt2+1
call cpu_time(t1)
dseg=int((1-real(multpt)/100.0)*dsec) ! the left points without overlapping
nseg=int((86400-dsec)/dseg)+1 
do is=1,nst   ! loop over station pair
   write(targetd,'(1a,"/",1a,"_",1a,"_",1a,"_",1a)')trim(dirout),trim(net1(is)),trim(sta1(is)),trim(net2(is)),trim(sta2(is))    
   numcc=0              ! num of cc
   do iy=year_b,year_e  ! loop over year
      jday=365
      if(mod(iy,4).eq.0.and.mod(iy,100).ne.0.or.mod(iy,400).eq.0)jday=366
      endday=day_e
      if(iy.ne.year_e)endday=jday
      begday=day_b
      if(iy.ne.year_b)begday=1
      do id=begday,endday ! loop over day
         write(command,'("mkdir -p ",1a)')trim(targetd)
         call system(command)   ! mkdir for outdir
         do iseg=1,nseg         ! loop over each segment
            nzhour=(iseg-1)*dseg/3600
            nzmin=mod((iseg-1)*dseg,3600)/60
            nzsec=mod(mod((iseg-1)*dseg,3600),60)
            do ic1=1,cn                     ! check whether all three components exist
               write(sacfile,'(1a,"/",i4.4,"_",i3.3,"/",i4.4,"_",i3.3,"_",i2.2,"_",i2.2,"_",i2.2,"_",1a,"_",1a,"_",1a,".SAC_",1a)')&
               trim(dirinn),iy,id,iy,id,nzhour,nzmin,nzsec,trim(net1(is)),trim(sta1(is)),trim(com(ic1)),trim(append)
               if(append.eq."null")&
               write(sacfile,'(1a,"/",i4.4,"_",i3.3,"/",i4.4,"_",i3.3,"_",i2.2,"_",i2.2,"_",i2.2,"_",1a,"_",1a,"_",1a,".SAC")')&
               trim(dirinn),iy,id,iy,id,nzhour,nzmin,nzsec,trim(net1(is)),trim(sta1(is)),trim(com(ic1))
               sac1(ic1)=sacfile
               inquire(file=sac1(ic1),exist=ext)
               it=0
               if(.not.ext)exit
               it=ic1
               call read_sachead(sac1(ic1),sachead1,nerr)
               call read_sac(sac1(ic1),sig1(:,ic1),sachead1,nerr)
               if(nerr.eq.-1)exit
            enddo              ! check station one done!
 !100        continue
            if(it.ne.cn)cycle ! if all three components of station 1 exist
            do ic2=1,cn        ! check whether all three components of station two exist
               write(sacfile,'(1a,"/",i4.4,"_",i3.3,"/",i4.4,"_",i3.3,"_",i2.2,"_",i2.2,"_",i2.2,"_",1a,"_",1a,"_",1a,".SAC_",1a)')&
               trim(dirinn),iy,id,iy,id,nzhour,nzmin,nzsec,trim(net2(is)),trim(sta2(is)),trim(com(ic2)),trim(append)
               if(append.eq."null")&
               write(sacfile,'(1a,"/",i4.4,"_",i3.3,"/",i4.4,"_",i3.3,"_",i2.2,"_",i2.2,"_",i2.2,"_",1a,"_",1a,"_",1a,".SAC")')&
               trim(dirinn),iy,id,iy,id,nzhour,nzmin,nzsec,trim(net2(is)),trim(sta2(is)),trim(com(ic2))
               sac2(ic2)=sacfile
               inquire(file=sac2(ic2),exist=ext)
               it=0
               if(.not.ext)exit
               it=ic2
               call read_sachead(sac2(ic2),sachead2,nerr)
               call read_sac(sac2(ic2),sig2(:,ic2),sachead2,nerr)
               if(nerr.eq.-1)exit
            enddo                ! check and read station two done
            if(it.ne.cn)cycle     ! if all three components of station 2 exist
            if(sachead2%delta.ne.sachead1%delta)exit
            dt=sachead1%delta
            nsamp=sachead1%npts
            do ic1=1,cn                     ! loop over com1
               do ic2=1,cn                  ! loop over com2
                  write(*,'("doncc: ",1a,1x,1a)')trim(sac1(ic1)),trim(sac2(ic2))
                  numcc(ic1,ic2)=numcc(ic1,ic2)+1 ! number of segements
                  write(output_ncc(ic1,ic2),'(1a,"/ncc_",i4.4,"_",i3.3,"_",i2.2,"_",i2.2,"_",i2.2,"_",1a,"_",1a,"_",1a,"_",1a,"_",&
                  1a,"_",1a,".SAC")')trim(targetd),iy,id,nzhour,nzmin,nzsec,trim(net1(is)),trim(sta1(is)),trim(net2(is)),&
                  trim(sta2(is)),trim(com(ic1)),trim(com(ic2))
                  write(output_pcc(ic1,ic2),'(1a,"/pcc_",i4.4,"_",i3.3,"_",i2.2,"_",i2.2,"_",i2.2,"_",1a,"_",1a,"_",1a,"_",1a,"_",&
                  1a,"_",1a,".SAC")')trim(targetd),iy,id,nzhour,nzmin,nzsec,trim(net1(is)),trim(sta1(is)),trim(net2(is)),&
                  trim(sta2(is)), trim(com(ic1)),trim(com(ic2))
                  if(dorot.eq.1)then
                     write(output_ncc(ic1,ic2),'(1a,"/ncc_",i4.4,"_",i3.3,"_",i2.2,"_",i2.2,"_",i2.2,&
                     "_",1a,"_",1a,"_",1a,"_",1a,".",1a)')trim(targetd),iy,id,nzhour,nzmin,nzsec,&
                     trim(net1(is)),trim(sta1(is)),trim(net2(is)),trim(sta2(is)),note(ic1,ic2)
                     write(output_pcc(ic1,ic2),'(1a,"/pcc_",i4.4,"_",i3.3,"_",i2.2,"_",i2.2,"_",i2.2,&
                     "_",1a,"_",1a,"_",1a,"_",1a,".",1a)')trim(targetd),iy,id,nzhour,nzmin,nzsec,&
                     trim(net1(is)),trim(sta1(is)),trim(net2(is)),trim(sta2(is)),note(ic1,ic2)
                  endif
                  if(doncc.eq.1)then      ! do normal cc
                     call do_ncc(nsamp,sig1(1:nsamp,ic1),sig2(1:nsamp,ic2),signcc(:,ic1,ic2),npt2,dt)
                     outncctl(1:nsamp,ic1,ic2)=outncctl(1:nsamp,ic1,ic2)+signcc(1:nsamp,ic1,ic2)
                  endif
                  if(dopcc.eq.1)then      ! do pcc
                     call do_pcc(nsamp,sig1(1:nsamp,ic1),sig2(1:nsamp,ic2),sigpcc(:,ic1,ic2),npt2,dt,0)
                     outpcctl(1:nsamp,ic1,ic2)=outpcctl(1:nsamp,ic1,ic2)+sigpcc(1:nsamp,ic1,ic2)
                  endif
                  !if(dopw.eq.1)then    ! do pws stack
                  !   if(doncc.eq.1)then
                  !      call hilbert(signcc(:,ic1,ic2),nsmpl,dt,hilb)           ! do hilbert transform
                  !      do i=1,nsmpl
                  !         theta=atan2(hilb(i),signcc(i,ic1,ic2))
                  !         pw_ncc_sig(i,ic1,ic2)=pw_ncc_sig(i,ic1,ic2)+cmplx(cos(theta),sin(theta))
                  !      enddo
                  !   endif
                  !   if(dopcc.eq.1)then
                  !      call hilbert(sigpcc(:,ic1,ic2),nsmpl,dt,hilb)           ! do hilbert transform
                  !      do i=1,nsmpl
                  !         theta=atan2(hilb(i),sigpcc(i))
                  !         pw_pcc_sig(i,ic1,ic2)=pw_pcc_sig(i,ic1,ic2)+cmplx(cos(theta),sin(theta))
                  !      enddo
                  !   endif
                  !else                   ! wirte out short ncf
                  !   if(doncc.eq.1)then  ! do normal cc
                  !      call write_ncf_sac(output_ncc,signcc,sachead1,sachead2,nsmpl,1,nerr)
                  !   endif
                  !   if(dopcc.eq.1)then ! do pcc
                  !      write(*,*)trim(output_pcc)
                  !      call write_ncf_sac(output_pcc,sigpcc,sachead1,sachead2,nsmpl,1,nerr)
                  !   endif
                  !endif
               enddo    ! end loop over com2      
            enddo       ! end loop over com1
            if(dorot.eq.1)then
               if(doncc.eq.1)call rot9com(sachead1,sachead2,nsmpl,signcc,signccrot)
               if(dopcc.eq.1)call rot9com(sachead1,sachead2,nsmpl,sigpcc,sigpccrot)
               do ic1=1,cn
                  do ic2=1,cn
                     if(doncc.eq.1)call write_ncf_sac(output_ncc(ic1,ic2),signccrot(:,ic1,ic2),sachead1,sachead2,nsmpl,1,nerr)
                     if(dopcc.eq.1)call write_ncf_sac(output_pcc(ic1,ic2),sigpccrot(:,ic1,ic2),sachead1,sachead2,nsmpl,1,nerr)
                  enddo ! end loop over com2
               enddo    ! end loop over com1
            else
               do ic1=1,cn
                  do ic2=1,cn
                     if(doncc.eq.1)call write_ncf_sac(output_ncc(ic1,ic2),signcc(:,ic1,ic2),sachead1,sachead2,nsmpl,1,nerr)
                     if(dopcc.eq.1)call write_ncf_sac(output_pcc(ic1,ic2),sigpcc(:,ic1,ic2),sachead1,sachead2,nsmpl,1,nerr)
                  enddo ! end loop over com2
               enddo    ! end loop over com1
            endif
         enddo          ! end loop hour
      enddo             ! end loop day
   enddo                ! end loop year
!   do ic1=1,cn
!      do ic2=1,cn
!         if(numcc(ic1,ic2).gt.0)then
!            if(dotl.eq.1)then
!               if(doncc.eq.1)then
!                  write(output,'(1a,"/",1a,"_",1a,"/tl_ncc_COR_",1a,"_",1a,"_",1a,"_",1a,"_",1a,".SAC")')trim(dirout),&
!                  trim(sta1(is)),trim(sta2(is)),trim(name),trim(sta1(is)),trim(sta2(is)),com(ic1),com(ic2)
!                  !call wrsac(output,kst,beg,dt,outcctl(:,ic1,ic2),nsmpl,evla,evlo,stla,stlo,numcc(ic1,ic2))
!                  call write_ncf_sac(output,outncctl(:,ic1,ic2),sachead1,sachead2,nsmpl,numcc(ic1,ic2),nerr)
!               endif
!               if(dopcc.eq.1)then
!                  write(output,'(1a,"/",1a,"_",1a,"/tl_pcc_COR_",1a,"_",1a,"_",1a,"_",1a,"_",1a,".SAC")')trim(dirout),&
!                  trim(sta1(is)),trim(sta2(is)),trim(name),trim(sta1(is)),trim(sta2(is)),com(ic1),com(ic2)
!                  !call wrsac(output,kst,beg,dt,outpcctl(:,ic1,ic2),nsmpl,evla,evlo,stla,stlo,numcc(ic1,ic2))
!                  call write_ncf_sac(output,outpcctl(:,ic1,ic2),sachead1,sachead2,nsmpl,numcc(ic1,ic2),nerr)
!               endif
!            endif
!            if(dopw.eq.1)then
!               if(doncc.eq.1)then
!                  do i=1,nsmpl 
!                  outnccpw(i,ic1,ic2)=cabs(pw_ncc_sig(i,ic1,ic2))**wu/real(numcc(ic1,ic2))*outncctl(i,ic1,ic2)
!                  enddo
!                  write(output,'(1a,"/",1a,"_",1a,"/pw_ncc_COR_",1a,"_",1a,"_",1a,"_",1a,"_",1a,".SAC")')trim(dirout),&
!                  trim(sta1(is)),trim(sta2(is)),trim(name),trim(sta1(is)),trim(sta2(is)),com(ic1),com(ic2)
!                  !call wrsac(output,kst,beg,dt,outccpws(:,ic1,ic2),nsmpl,evla,evlo,stla,stlo,numcc(ic1,ic2))
!                  call write_ncf_sac(output,outnccpw(:,ic1,ic2),sachead1,sachead2,nsmpl,numcc(ic1,ic2),nerr)
!               endif
!               if(dopcc.eq.1)then
!                  do i=1,nsmpl 
!                  outpccpw(i,ic1,ic2)=cabs(pw_pcc_sig(i,ic1,ic2))**wu/real(numcc(ic1,ic2))*outpcctl(i,ic1,ic2)
!                  enddo
!                  write(output,'(1a,"/",1a,"_",1a,"/pw_pcc_COR_",1a,"_",1a,"_",1a,"_",1a,"_",1a,".SAC")')trim(dirout),&
!                  trim(sta1(is)),trim(sta2(is)),trim(name),trim(sta1(is)),trim(sta2(is)),com(ic1),com(ic2)
!                  !call wrsac(output,kst,beg,dt,outpccpws(:,ic1,ic2),nsmpl,evla,evlo,stla,stlo,numcc(ic1,ic2))
!                  call write_ncf_sac(output,outpccpw(:,ic1,ic2),sachead1,sachead2,nsmpl,numcc(ic1,ic2),nerr)
!               endif
!            endif
!         endif ! if cross correlation been calculated
!      enddo    ! loop over component 1
!   enddo       ! loop over component 2
enddo          ! end loop station pair
call cpu_time(t2)
write(*,'("Segment length:",i5.5," s Overlaping:",i2.2,"%, Time costs: ",f17.3,"s")')dsec,multpt,t2-t1
end program
