module module_adam
#ifdef WV_NEW
    implicit none
#endif
contains

subroutine adam(n,nmax,data21,fold,gold,hold,&
!fghold,&
f,g,h)
#ifdef WV_NEW
    use params_common_sn

#else
    use common_sn ! create_new_include_statements() line 102
#endif
    character(len=70), intent(In) :: data21
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: f
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: g
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: h
!    real(kind=4), dimension(ip,jp,kp) , intent(In) :: fghold
    real(kind=4), dimension(ip,jp,kp) , intent(InOut) :: fold
    real(kind=4), dimension(ip,jp,kp) , intent(InOut) :: gold
    real(kind=4), dimension(ip,jp,kp) , intent(InOut) :: hold
    integer, intent(In) :: n
    integer, intent(In) :: nmax
#ifdef WV_NEW
    integer :: i,j,k
    real(kind=4) :: fd,gd,hd
#endif
#if !defined(NO_IO) && !defined(MPI)
    if (mod(n,1000) == 0.or.n == nmax) then
        open(unit=21,file=data21,form='unformatted',status='unknown')
        write(21) (((fold(i,j,k),i=1,ip),j=1,jp),k=1,kp)
        write(21) (((gold(i,j,k),i=1,ip),j=1,jp),k=1,kp)
        write(21) (((hold(i,j,k),i=1,ip),j=1,jp),k=1,kp)
!        write(21) (((fghold(i,j,k),i=1,ip),j=1,jp),k=1,kp)
        close(unit=21)
    end if

    do k = 1,kp
        do j = 1,jp
            do i = 1,ip
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
#else
    do k = 1,kp
        do j = 1,jp
            do i = 1,ip
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
#endif
#ifdef WV_DEBUG
    print *, 'F95 FGHSUM after adam:',sum(f)+sum(g)+sum(h)
    print *, 'F95 FSUM after adam:',sum(f)
    print *, 'F95 GSUM after adam:',sum(g)
    print *, 'F95 HSUM after adam:',sum(h)
#endif
end subroutine adam

end module module_adam
