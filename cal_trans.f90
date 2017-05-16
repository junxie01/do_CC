subroutine rot9com(sachead1,sachead2,trans,azim)
use sacio
implicit none
type(sac_head) :: sachead1,sachead2
real,dimension(4,4) :: trans
real,dimension(2,2) :: azim
real :: az,baz,sbaz,saz,caz,cbaz
real :: a,b,P,lp
real,parameter :: rad=atan2(1.0,1.0)*4/180
integer :: npts
!az=sachead%az*rad
!baz=sachead%baz*rad
!npts=sachead%npts
a=(90-sachead2%stla)*rad
b=(90-sachead1%stla)*rad
P=abs(sachead1%stlo-sachead2%stlo)*rad
lp=acos(cos(b)*cos(a)+sin(a)*sin(b)*cos(P))
saz =a*sin(P)/lp
sbaz=b*sin(P)/lp
caz =(cos(a)-cos(b)*cos(lp))/(sin(b)*sin(lp))
cbaz=(cos(b)-cos(a)*cos(lp))/(sin(a)*sin(lp))
!saz=sin(az)
!caz=cos(az)
!sbaz=sin(baz)
!cbaz=cos(baz)
azim(1,1)=saz
azim(1,2)=caz
azim(2,1)=sbaz
azim(2,2)=cbaz

trans(1,1)= saz*sbaz
trans(1,2)= saz*cbaz
trans(1,3)=-caz*sbaz
trans(1,4)=-caz*cbaz

trans(2,1)=-saz*cbaz
trans(2,2)= saz*sbaz
trans(2,3)= caz*cbaz
trans(2,4)=-caz*sbaz

trans(3,1)= caz*sbaz
trans(3,2)= caz*cbaz
trans(3,3)= saz*sbaz
trans(3,4)= saz*cbaz

trans(4,1)=-caz*cbaz
trans(4,2)= caz*sbaz
trans(4,3)=-saz*cbaz
trans(4,4)= saz*sbaz
!RZ
sigout(1:npts,2,1)=-sig(1:npts,2,1)* caz - sig(1:npts,3,1)* saz
!ZT
sigout(1:npts,1,3)=-sig(1:npts,1,2)*sbaz - sig(1:npts,1,3)*cbaz
!TZ
sigout(1:npts,3,1)=-sig(1:npts,2,1)* saz + sig(1:npts,3,1)* caz
sigout(1:npts,1,1)= sig(1:npts,1,1)
return
end subroutine
