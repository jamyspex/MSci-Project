module module_boundp
implicit none
 contains
subroutine boundp2(jm,im,p,km)
    use common_sn 
    integer, intent(In) :: im
    integer, intent(In) :: jm
    integer, intent(In) :: km
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+1) , intent(InOut) :: p
    integer :: i, j
    do j = 0,jm+1
        do i = 0,im+1
            p(i,j, 0) = p(i,j,1)
            p(i,j,km+1) = p(i,j,km)
        end do
    end do
end subroutine boundp2
subroutine boundp1(km,jm,p,im)
    use common_sn 
    integer, intent(In) :: im
    integer, intent(In) :: jm
    integer, intent(In) :: km
    real(kind=4), dimension(0:ip+2,0:jp+2,0:kp+1) , intent(InOut) :: p
    integer :: i, j, k
        do k = 0,km+1
            do j = 0,jm+1
                    p( 0,j,k) = p(1 ,j,k)
                    p(im+1,j,k) = p(im,j,k)
            end do
        end do
    do k = 0,km+1
        do i = 0,im+1
            p(i, 0,k) = p(i,jm,k) 
            p(i,jm+1,k) = p(i, 1,k) 
        end do
    end do
end subroutine boundp1
end module module_boundp
