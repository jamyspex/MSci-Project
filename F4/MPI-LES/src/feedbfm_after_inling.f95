module module_feedbfm
 contains
subroutine feedbfm(amask1,bmask1,cmask1,dmask1,zbm,z2,dzn)
      implicit none
integer, parameter :: kp=80
    integer, parameter :: ip = 300
    integer, parameter :: jp = 300
    integer, parameter :: ipmax = ip
    integer, parameter :: jpmax = jp
    character(300) :: datafile = '../GIS/Kyoto_1km2_4m_with_buffer.txt'
    real, parameter :: dxgrid = 4.
    real, parameter :: dygrid = 4.
    real, parameter :: cs0 = 0.14
    integer, parameter :: i_anime=1
    integer, parameter :: avetime=2
    integer, parameter :: km_sl=80
    integer, parameter :: i_aveflow=0
    integer, parameter :: i_ifdata_out=0
    real, parameter :: dt_orig = 0.05 
    real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1) , intent(Out) :: amask1
    real(kind=4), dimension(-1:ip+1,0:jp+1,0:kp+1) , intent(Out) :: bmask1
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(Out) :: cmask1
    real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1) , intent(Out) :: dmask1
    real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
    real(kind=4), dimension(0:kp+2) , intent(In) :: z2
    real(kind=4), dimension(-1:ipmax+1,-1:jpmax+1) , intent(InOut) :: zbm
    integer :: i, j, k
    real(kind=4), dimension(-1:ipmax+1,-1:jpmax+1) :: dsm,dem
    do k = 1,kp
        do j = 1,jp
            do i = 1,ip
                amask1(i,j,k) = 1.
                bmask1(i,j,k) = 0.
                cmask1(i,j,k) = 0.
                dmask1(i,j,k) = 0.
            end do
        end do
    end do
    zbm=0.
        open(70,file=datafile,form='formatted',status='unknown')
           do j=1,jpmax
              do i=1,ipmax
                 read(70,*) zbm(i,j)
              end do
           end do
        close(70)
      do j = 1,jp
          do i = 1,ip
              do k = 1,kp
                  if(zbm(i,j) > z2(k)+0.5*dzn(k)) then
                      amask1(i,j,k) = 0.0
                  end if
              end do
          end do
      end do
    do k = 1,kp
        do j = 1,jp
            do i = 1,ip
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
