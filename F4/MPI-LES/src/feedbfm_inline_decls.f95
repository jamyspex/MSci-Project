module module_feedbfm
 contains
subroutine feedbfm(km,jm,im,amask1,bmask1,cmask1,dmask1,zbm,z2,dzn)
      implicit none
    real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1) , intent(Out) :: amask1
    real(kind=4), dimension(-1:ip+1,0:jp+1,0:kp+1) , intent(Out) :: bmask1
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(Out) :: cmask1
    real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1) , intent(Out) :: dmask1
    real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
    integer, intent(In) :: im
    integer, intent(In) :: jm
    integer, intent(In) :: km
    real(kind=4), dimension(0:kp+2) , intent(In) :: z2
    real(kind=4), dimension(-1:ipmax+1,-1:jpmax+1) , intent(InOut) :: zbm
    integer :: i, j, k
    real(kind=4), dimension(-1:3001,-1:751) :: dsm,dem
    do k = 1,km
        do j = 1,jm
            do i = 1,im
                amask1(i,j,k) = 1.
                bmask1(i,j,k) = 0.
                cmask1(i,j,k) = 0.
                dmask1(i,j,k) = 0.
            end do
        end do
    end do
        print*, 'zbm sum - file getting read'
      open(70,file='./GIS/DEM_LES_GIS.txt',form='formatted',status='unknown')
      do j=1,750
        do i=1,3000
          read(70,*) dem(i,j)
        end do
      end do
      close(70)
      open(71,file='./GIS/DSM_LES_GIS.txt',form='formatted',status='unknown')
      do j=1,750
        do i=1,3000
          read(71,*) dsm(i,j)
        end do
      end do
      close(71)
      do j=1,250
        do i=1,2750
          zbm(i+125,j+25)=dsm(i+250,j+138)-dem(i+250,j+138)
        end do
      end do
    do j = 1,jm
        do i = 1,im
            do k = 1,km
                if(zbm(i,j) > z2(k)+0.5*dzn(k)) then
                    amask1(i,j,k) = 0.0
                end if
            end do
        end do
    end do
    if (isMaster()) then
      do k=1,km
        write(*,*) 'z2=',z2(k)
      end do
    end if
    do k = 1,km
        do j = 1,jm
            do i = 1,im
                if(amask1(i,j,k) == 0.0) then
                    bmask1(i,j,k) = 1.0
                    cmask1(i,j,k) = 1.0
                    dmask1(i,j,k) = 1.0
                end if
            end do
        end do
    end do
end subroutine feedbfm
end module module_feedbfm
