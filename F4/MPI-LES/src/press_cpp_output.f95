module module_press
    use module_bondFG ! add_module_decls() line 156
    use module_boundp ! add_module_decls() line 156
    implicit none
 contains
subroutine press(u,v,w,p,rhs,f,g,h,dx1,dy1,dzn,dxs,dys,dzs,dt,n,nmax &
)
    use params_common_sn
    implicit none
    real(kind=4), dimension(0:ip) , intent(In) :: dxs
    real(kind=4), dimension(0:jp) , intent(In) :: dys
    real(kind=4), dimension(-1:kp+2) , intent(In) :: dzs
    real(kind=4) :: cn1,cn2l,cn2s,cn3l,cn3s,cn4l,cn4s,dz1,dz2
    real(kind=4), intent(In) :: dt
    real(kind=4), dimension(-1:ip+1) , intent(In) :: dx1
    real(kind=4), dimension(0:jp+1) , intent(In) :: dy1
    real(kind=4), dimension(-1:kp+2) , intent(In) :: dzn
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: f
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: g
    real(kind=4), dimension(0:ip,0:jp,0:kp) , intent(InOut) :: h
    integer, intent(In) :: n
    integer, intent(In) :: nmax
    real(kind=4), dimension(0:1,0:ip+2,0:jp+2,0:kp+1) :: p
    real(kind=4), dimension(0:ip+1,0:jp+1,0:kp+1) , intent(Out) :: rhs
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: u
    real(kind=4), dimension(0:ip+1,-1:jp+1,0:kp+1) , intent(In) :: v
    real(kind=4), dimension(0:ip+1,-1:jp+1,-1:kp+1) , intent(In) :: w
    integer :: nn
    integer :: i,j,k,l,nrd
    real(kind=4) :: rhsav, pav, area, pco, sor, reltmp
    !
    ! --only used mac method
    real, parameter :: pjuge = 0.0001
    integer, parameter :: nmaxp = 50 ! WV was 50
    real, parameter :: omega = 1.
! --inflow condition
      do k=1,kp
      do j=1,jp
        f( 0,j,k)=f(1 ,j,k)
   end do
      end do
! --sideflow condition
      do k=1,kp
      do i=1,ip
        g(i, 0,k)=g(i,jp ,k)
   end do
      end do
! --ground and top condition
      do j=1,jp
      do i=1,ip
        h(i,j, 0)=0.0
        h(i,j,kp)=0.0
   end do
      end do
    do k = 1,kp
        do j = 1,jp
            do i = 1,ip
                rhs(i,j,k) = (-u(i-1,j,k)+u(i,j,k))/dx1(i) +(-v(i,j-1,k)+ &
                             v(i,j,k))/dy1(j) +(-w(i,j,k-1)+w(i,j,k))/dzn(k)
! --stretch
                rhs(i,j,k) = (f(i,j,k)-f(i-1,j,k))/dx1(i) +(g(i,j,k)- &
                              g(i,j-1,k))/dy1(j) +(h(i,j,k)-h(i,j,k-1))/dzn(k) &
                              +rhs(i,j,k)/dt
            end do
        end do
    end do
!
! rhs=0
    rhsav = 0.0
    area = 0.0
    do k = 1,kp
        do j = 1,jp
            do i = 1,ip
                rhsav = rhsav+dx1(i)*dy1(j)*dzn(k)*rhs(i,j,k)
                area = area +dx1(i)*dy1(j)*dzn(k)
            end do
        end do
    end do
    rhsav = rhsav/area
    do k = 1,kp
        do j = 1,jp
            do i = 1,ip
                rhs(i,j,k) = rhs(i,j,k)-rhsav
            end do
        end do
    end do
! --SOR
    do l = 1,nmaxp
        sor = 0.0
        do nrd = 0,1
            do k = 1,kp
                do j = 1,jp
                    do i=1,ip
                dz1 = dzs(k-1)
                dz2 = dzs(k)
                cn4s = 2./(dz1*(dz1+dz2))
                cn4l = 2./(dz2*(dz1+dz2))
                    cn3s = 2./(dys(j-1)*(dys(j-1)+dys(j)))
                    cn3l = 2./(dys(j)*(dys(j-1)+dys(j)))
                        cn2s = 2./(dxs(i-1)*(dxs(i-1)+dxs(i)))
                        cn2l = 2./(dxs(i)*(dxs(i-1)+dxs(i)))
                        cn1 = 1./ (2./(dxs(i-1)*dxs(i)) + 2./(dys(j-1)*dys(j)) + 2./(dz1*dz2))
! cn1 = 1./cn1
                      if (nrd==0) then
                        ! buffer 0
                        reltmp = omega*(cn1 *(cn2l*p(0,i+1,j,k) + &
                                 cn2s*p(0,i-1,j,k) +cn3l*p(0,i,j+1,k) + &
                                 cn3s*p(0,i,j-1,k) +cn4l*p(0,i,j,k+1) + &
                                 cn4s*p(0,i,j,k-1) -rhs(i,j,k))-p(0,i,j,k))
                        p(1,i,j,k) = p(0,i,j,k) +reltmp
                      else
                          ! buffer 1
                        reltmp = omega*(cn1 *(cn2l*p(1,i+1,j,k) + &
                                 cn2s*p(1,i-1,j,k) +cn3l*p(1,i,j+1,k) + &
                                 cn3s*p(1,i,j-1,k) +cn4l*p(1,i,j,k+1) + &
                                 cn4s*p(1,i,j,k-1) -rhs(i,j,k))-p(1,i,j,k))
                        p(0,i,j,k) = p(1,i,j,k) +reltmp
                      end if
                    end do
                end do
            end do
      ! --computational boundary(neumann condition)
          do k=0,kp+1
          do j=0,jp+1
            p(0, 0,j,k) = p(0,1 ,j,k)
            p(0,ip+1,j,k) = p(0,ip,j,k)
          end do
          end do
          do k=0,kp+1
          do i=0,ip+1
            p(0,i, 0,k) = p(0,i,jp,k)
            p(0,i,jp+1,k) = p(0,i, 1,k)
          end do
          end do
        end do ! nrd
! --computational boundary(neumann condition)
      do j=0,jp+1
      do i=0,ip+1
        p(0,i,j, 0) = p(0,i,j,1)
        p(0,i,j,kp+1) = p(0,i,j,kp)
      end do
      end do
    end do ! l
!print *,rank,'SOR iterations:',l
    pav = 0.0
    pco = 0.0
    do k = 1,kp
        do j = 1,jp
            do i = 1,ip
                pav = pav+p(0,i,j,k)*dx1(i)*dy1(j)*dzn(k)
                pco = pco+dx1(i)*dy1(j)*dzn(k)
            end do
        end do
    end do
    pav = pav/pco
    do k = 1,kp
        do j = 1,jp
            do i = 1,ip
                p(0,i,j,k) = p(0,i,j,k)-pav
            end do
        end do
    end do
      ! --computational boundary(neumann condition)
          do k=0,kp+1
          do j=0,jp+1
            p(0, 0,j,k) = p(0,1 ,j,k)
            p(0,ip+1,j,k) = p(0,ip,j,k)
          end do
          end do
          do k=0,kp+1
          do i=0,ip+1
            p(0,i, 0,k) = p(0,i,jp,k)
            p(0,i,jp+1,k) = p(0,i, 1,k)
          end do
          end do
! --computational boundary(neumann condition)
      do j=0,jp+1
      do i=0,ip+1
        p(0,i,j, 0) = p(0,i,j,1)
        p(0,i,j,kp+1) = p(0,i,j,kp)
      end do
      end do
end subroutine press
end module module_press
