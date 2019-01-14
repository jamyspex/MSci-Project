module module_boundsm
 contains
subroutine boundsm(km,jm,sm,im)
      implicit none
    integer, intent(In) :: im
    integer, intent(In) :: jm
    integer, intent(In) :: km
    real(kind=4), dimension(-1:ip+1,-1:jp+1,0:kp+1) , intent(InOut) :: sm
    integer :: i, j, k
        do k = 0,km+1
            do j = -1,jm+1
                    sm( 0,j,k) = sm(1 ,j,k) 
                    sm(im+1,j,k) = sm(im,j,k)
            end do
        end do
        do k = 0,km+1
            do i = 0,im+1
                    sm(i,jm+1,k) = sm(i,jm ,k)
                    sm(i,0,k) = sm(i,1 ,k) 
            end do
        end do
    do j = -1,jm+1
        do i = 0,im+1
            sm(i,j, 0) = -sm(i,j, 1)
            sm(i,j,km+1) = sm(i,j,km)
        end do
    end do
end subroutine boundsm
end module module_boundsm
