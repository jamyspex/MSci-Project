subroutine pipeline_1_press(dxs,dys,dzs,rhs)
      real(4) :: cn1
      real(4) :: cn2l
      real(4) :: cn2s
      real(4) :: cn3l
      real(4) :: cn3s
      real(4) :: cn4l
      real(4) :: cn4s
      real(4) :: dz1
      real(4) :: dz2
      real(4), dimension(0:300), intent(In) :: dxs
      real(4), dimension(0:300), intent(In) :: dys
      real(4), dimension(-1:82), intent(In) :: dzs
      integer :: i
      integer :: j
      integer :: k
      real(4), dimension(0:302,0:302,0:81) :: p0
      real(4), dimension(0:302,0:302,0:81) :: p1
      real(4), dimension(0:301,0:301,0:81), intent(Out) :: rhs
      real(4) :: sor
      real(4) :: reltmp
      integer :: synthIdx0
      integer :: synthIdx1
      integer :: synthIdx2
! Original Subroutine Name: press {
! OpenCLMap ( ["dzs","p0"],[],["(l,1,50,1)","(nrd,0,1,1)","(k,1,80,1)","(j,1,300,1)","(i,1,300,1)","(i,1,300,1)","(j,1,300,1)","(i,1,300,1)","(i,1,300,1)","(k,0,81,1)","(j,0,301,1)","(synthIdx2,0,302,1)","(synthIdx2,0,302,1)","(j,0,301,1)","(synthIdx2,0,302,1)","(synthIdx2,0,302,1)","(k,0,81,1)","(synthIdx1,0,302,1)","(i,0,301,1)","(i,0,301,1)","(synthIdx1,0,302,1)","(i,0,301,1)","(i,0,301,1)","(synthIdx0,0,81,1)","(j,0,301,1)","(i,0,301,1)","(i,0,301,1)","(j,0,301,1)","(i,0,301,1)","(i,0,301,1)"],[]) {
    sor = 0.0
    do nrd = 0, 1, 1
! OpenCLMap ( ["dzs","nrd","p0"],["p0"],["(k,1,80,1)","(j,1,300,1)","(i,1,300,1)","(i,1,300,1)"],["nrd"]) {
! OpenCLMap ( ["dzs","nrd","p0"],["p0"],["(j,1,300,1)","(i,1,300,1)"],["nrd"]) {
! OpenCLMap ( ["dzs","nrd","p0"],["p0"],["(i,1,300,1)"],["nrd"]) {
        dz1 = dzs(k-1)
        dz2 = dzs(k)
        cn4s = 2./(dz1*(dz1+dz2))
        cn4l = 2./(dz2*(dz1+dz2))
        cn3s = 2./(dys(j-1)*(dys(j-1)+dys(j)))
        cn3l = 2./(dys(j)*(dys(j-1)+dys(j)))
        cn2s = 2./(dxs(i-1)*(dxs(i-1)+dxs(i)))
        cn2l = 2./(dxs(i)*(dxs(i-1)+dxs(i)))
        cn1 = 1./(2./(dxs(i-1)*dxs(i))+2./(dys(j-1)*dys(j))+2./(dz1*dz2))
        if (nrd==0) then
            reltmp = 1.0*(cn1*(cn2l*p0(i+1,j,k)+cn2s*p0(i-1,j,k)+cn3l*p0(i,j+1,k)+cn3s*p0(i,j-1,k)+cn4l*p0(i,j,k+1)+cn4s*p0(i,j,k-1)-rhs(i,j,k))-p0(i,j,k))
            p1(i,j,k) = p0(i,j,k)+reltmp
        else
            reltmp = 1.0*(cn1*(cn2l*p1(i+1,j,k)+cn2s*p1(i-1,j,k)+cn3l*p1(i,j+1,k)+cn3s*p1(i,j-1,k)+cn4l*p1(i,j,k+1)+cn4s*p1(i,j,k-1)-rhs(i,j,k))-p1(i,j,k))
            p0(i,j,k) = p1(i,j,k)+reltmp
        end if
!}
!}
!}
! OpenCLMap ( [],["p0"],["(k,0,81,1)","(j,0,301,1)","(synthIdx2,0,302,1)","(synthIdx2,0,302,1)"],["nrd"]) {
! OpenCLMap ( [],["p0"],["(j,0,301,1)","(synthIdx2,0,302,1)"],["nrd"]) {
! OpenCLMap ( [],["p0"],["(synthIdx2,0,302,1)"],["nrd"]) {
        if (synthIdx2==0) then
            p0(synthIdx2,j,k) = p0(synthIdx2+1,j,k)
        end if
        if (synthIdx2==301) then
            p0(synthIdx2,j,k) = p0(synthIdx2-1,j,k)
        end if
!}
!}
!}
! OpenCLMap ( [],["p0"],["(k,0,81,1)","(synthIdx1,0,302,1)","(i,0,301,1)","(i,0,301,1)"],["nrd"]) {
! OpenCLMap ( [],["p0"],["(synthIdx1,0,302,1)","(i,0,301,1)"],["nrd"]) {
! OpenCLMap ( ["synthIdx1"],["p0"],["(i,0,301,1)"],["nrd"]) {
        if (synthIdx1==0) then
            p0(i,synthIdx1,k) = p0(i,synthIdx1+300,k)
        end if
        if (synthIdx1==301) then
            p0(i,synthIdx1,k) = p0(i,synthIdx1-300,k)
        end if
!}
!}
!}
    end do
! OpenCLMap ( [],[],["(synthIdx0,0,81,1)","(j,0,301,1)","(i,0,301,1)","(i,0,301,1)"],[]) {
! OpenCLMap ( ["synthIdx0"],[],["(j,0,301,1)","(i,0,301,1)"],[]) {
! OpenCLMap ( ["synthIdx0"],[],["(i,0,301,1)"],[]) {
    if (synthIdx0==0) then
        p0(i,j,synthIdx0) = p0(i,j,synthIdx0+1)
    end if
    if (synthIdx0==81) then
        p0(i,j,synthIdx0) = p0(i,j,synthIdx0-1)
    end if
!}
!}
!}
!}
!}
end subroutine pipeline_1_press
