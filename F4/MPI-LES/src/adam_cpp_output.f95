module module_adam
 contains
subroutine adam(n,nmax,data21,fold,im,jm,km,gold,hold,fghold,f,g,h)
    use common_sn ! create_new_include_statements() line 102
    character(len=70), intent(In) :: data21
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: f
    real(kind=4), dimension(ip,jp,kp) , intent(In) :: fghold
    real(kind=4), dimension(ip,jp,kp) , intent(InOut) :: fold
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: g
    real(kind=4), dimension(ip,jp,kp) , intent(InOut) :: gold
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: h
    real(kind=4), dimension(ip,jp,kp) , intent(InOut) :: hold
    integer, intent(In) :: im
    integer, intent(In) :: jm
    integer, intent(In) :: km
    integer, intent(In) :: n
    integer, intent(In) :: nmax
    do k = 1,km
        do j = 1,jm
            do i = 1,im
                fd = f(i,j,k)
                gd = g(i,j,k)
                hd = h(i,j,k)
                f(i,j,k) = 1.5*f(i,j,k)-0.5*fold(i,j,k)
                g(i,j,k) = 1.5*g(i,j,k)-0.5*gold(i,j,k)
                h(i,j,k) = 1.5*h(i,j,k)-0.5*hold(i,j,k)
                fold(i,j,k) = fd
                gold(i,j,k) = gd
                hold(i,j,k) = hd
            end do
        end do
    end do
end subroutine adam
end module module_adam
