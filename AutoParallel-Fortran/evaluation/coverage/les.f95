module module_les

      use module_boundsm
contains

      subroutine les(km,delx1,dzn,dx1,dy1,jm,im,diu1,diu2,diu3,diu4,diu5,diu6,diu7,diu8,diu9,sm,f,g,h)
      use params_common_sn!, only : ip, jp, kp !, only : ip, jp, kp
      integer, intent(In) :: km
      real, dimension(1:kp), intent(InOut) :: delx1
      real, dimension(-1:kp+2), intent(In) :: dzn
      real, dimension(-1:ip+1), intent(In) :: dx1
      real, dimension(0:jp+1), intent(In) :: dy1
      integer, intent(In) :: jm
      integer, intent(In) :: im
      real, dimension(-1:ip+2,0:jp+2,0:kp+2), intent(In) :: diu1
      real, dimension(0:ip+2,0:jp+2,0:kp+2), intent(In) :: diu2
      real, dimension(0:ip+2,0:jp+2,0:kp+2), intent(In) :: diu3
      real, dimension(0:ip+2,0:jp+2,0:kp+2), intent(In) :: diu4
      real, dimension(-1:ip+2,0:jp+2,0:kp+2), intent(In) :: diu5
      real, dimension(0:ip+2,0:jp+2,0:kp+2), intent(In) :: diu6
      real, dimension(0:ip+2,0:jp+2,0:kp+2), intent(In) :: diu7
      real, dimension(0:ip+2,0:jp+2,0:kp+2), intent(In) :: diu8
      real, dimension(0:ip+2,0:jp+2,0:kp+2), intent(In) :: diu9
      real, dimension(-1:ip+1,-1:jp+1,0:kp+1), intent(InOut) :: sm
      real, dimension(0:ip,0:jp,0:kp), intent(InOut) :: f
      real, dimension(0:ip,0:jp,0:kp), intent(InOut) :: g
      real, dimension(0:ip,0:jp,0:kp), intent(InOut) :: h
      real :: cs0
      integer :: k
      integer :: j
      integer :: i
      real :: dudxx1
      real :: dudyx1
      real :: dudzx1
      real :: dvdxx1
      real :: dvdyx1
      real :: dvdzx1
      real :: dwdxx1
      real :: dwdyx1
      real :: dwdzx1
      real :: csx1
      real :: evsx2
      real :: evsx1
      real :: evsy2
      real :: evsy1
      real :: evsz2
      real :: evsz1
      real :: visux2
      real :: visux1
      real :: visuy2
      real :: visuy1
      real :: visuz2
      real :: visuz1
      real :: vfu
      real :: visvx2
      real :: visvx1
      real :: visvy2
      real :: visvy1
      real :: visvz2
      real :: visvz1
      real :: vfv
      real :: viswx2
      real :: viswx1
      real :: viswy2
      real :: viswy1
      real :: viswz2
      real :: viswz1
      real :: vfw
! 
! 
      cs0=.1 
! --length scale
        do k=1,km
!  WV: was          delx1(k)=(dx1(i)*dy1(j)*dzn(k))**(1./3.)
          delx1(k)=(dx1(0)*dy1(0)*dzn(k))**(1./3.)
        end do 
! ----
      do k=1,km
      do j=1,jm
      do i=1,im
! --calculation of sgs eddy viscosity coeficient
      dudxx1= diu1(i,j,k)
      dudyx1=(diu2(i-1,j,k)+diu2(i-1,j+1,k)       +diu2(i  ,j,k)+diu2(i  ,j+1,k) )      *.25
      dudzx1=(diu3(i-1,j,k)+diu3(i-1,j,k+1)       +diu3(i  ,j,k)+diu3(i  ,j,k+1) )      *.25
      dvdxx1=(diu4(i  ,j,k)+diu4(i  ,j-1,k)       +diu4(i+1,j,k)+diu4(i+1,j-1,k) )      *.25
      dvdyx1= diu5(i,j,k)
      dvdzx1=(diu6(i,j-1,k)+diu6(i,j-1,k+1)       +diu6(i,j  ,k)+diu6(i,j  ,k+1) )       *.25
      dwdxx1=(diu7(i  ,j,k)+diu7(i  ,j,k-1)       +diu7(i+1,j,k)+diu7(i+1,j,k-1) )      *.25
      dwdyx1=(diu8(i,j  ,k)+diu8(i,j  ,k-1)       +diu8(i,j+1,k)+diu8(i,j+1,k-1) )      *.25
      dwdzx1= diu9(i,j,k)
! 
      csx1=cs0
! --abl or channel
      sm(i,j,k)=( csx1*delx1(k) )**2*sqrt( 2.*( dudxx1**2+dvdyx1**2+dwdzx1**2 )           +( dudyx1+dvdxx1 )**2+( dwdyx1+dvdzx1 )**2+( dudzx1+dwdxx1 )**2                 )
   end do
      end do
      end do
! 
      call boundsm(km,jm,sm,im)

! --calculation of viscosity terms in momentum eq.(x-comp.)
      do k=1,km
      do j=1,jm
      do i=1,im
! --eddyviscosity on face
      evsx2=sm(i+1,j,k)
      evsx1=sm(i,j,k)
      evsy2=(dy1(j+1)*((dx1(i+1)*sm(i,j,k)+dx1(i)*sm(i+1,j,k))     /(dx1(i)+dx1(i+1)))      +dy1(j)*((dx1(i+1)*sm(i,j+1,k)+dx1(i)*sm(i+1,j+1,k))     /(dx1(i)+dx1(i+1))))     /(dy1(j)+dy1(j+1))     
      evsy1=(dy1(j+1)*((dx1(i+1)*sm(i,j-1,k)+dx1(i)*sm(i+1,j-1,k))     /(dx1(i)+dx1(i+1)))      +dy1(j)*((dx1(i+1)*sm(i,j,k)+dx1(i)*sm(i+1,j,k))     /(dx1(i)+dx1(i+1))))     /(dy1(j)+dy1(j+1))
      evsz2=(dzn(k+1)*((dx1(i+1)*sm(i,j,k)+dx1(i)*sm(i+1,j,k))     /(dx1(i)+dx1(i+1)))      +dzn(k)*((dx1(i+1)*sm(i,j,k+1)+dx1(i)*sm(i+1,j,k+1))     /(dx1(i)+dx1(i+1))))     /(dzn(k)+dzn(k+1))
      evsz1=(dzn(k)*((dx1(i+1)*sm(i,j,k-1)+dx1(i)*sm(i+1,j,k-1))     /(dx1(i)+dx1(i+1)))      +dzn(k-1)*((dx1(i+1)*sm(i,j,k)+dx1(i)*sm(i+1,j,k))     /(dx1(i)+dx1(i+1))))     /(dzn(k-1)+dzn(k))
! 
      visux2=(evsx2)*2.*diu1(i+1,j  ,k  )
      visux1=(evsx1)*2.*diu1(i  ,j,  k  )
      visuy2=(evsy2)* ( diu2(i  ,j+1,k  )+diu4(i+1,j  ,k  ) )
      visuy1=(evsy1)* ( diu2(i  ,j  ,k  )+diu4(i+1,j-1,k  ) )
      visuz2=(evsz2)* ( diu3(i  ,j  ,k+1)+diu7(i+1,j  ,k  ) )
      visuz1=(evsz1)* ( diu3(i  ,j  ,k  )+diu7(i+1,j  ,k-1) )
! 
      vfu= (visux2-visux1)/dx1(i)    +(visuy2-visuy1)/dy1(j)    +(visuz2-visuz1)/dzn(k) 
! 
      f(i,j,k)=(f(i,j,k)+vfu) 
   end do
      end do
      end do
! --calculation of viscosity terms in momentum eq.(y-comp.)
      do k=1,km
      do j=1,jm
      do i=1,im
! --eddyviscosity on face
      evsy2=sm(i,j+1,k)
      evsy1=sm(i,j,k)
      evsx2=(dy1(j+1)*((dx1(i+1)*sm(i,j,k)+dx1(i)*sm(i+1,j,k))     /(dx1(i)+dx1(i+1)))      +dy1(j)*((dx1(i+1)*sm(i,j+1,k)+dx1(i)*sm(i+1,j+1,k))     /(dx1(i)+dx1(i+1))))     /(dy1(j)+dy1(j+1))
      evsx1=(dy1(j+1)*((dx1(i)*sm(i-1,j,k)+dx1(i-1)*sm(i,j,k))     /(dx1(i-1)+dx1(i)))      +dy1(j)*((dx1(i)*sm(i-1,j+1,k)+dx1(i-1)*sm(i,j+1,k))     /(dx1(i-1)+dx1(i))))     /(dy1(j)+dy1(j+1))
      evsz2=(dzn(k+1)*((dx1(i+1)*sm(i,j,k)+dx1(i)*sm(i+1,j,k))     /(dx1(i)+dx1(i+1)))      +dzn(k)*((dx1(i+1)*sm(i,j,k+1)+dx1(i)*sm(i+1,j,k+1))     /(dx1(i)+dx1(i+1))))     /(dzn(k)+dzn(k+1))
      evsz1=(dzn(k)*((dx1(i+1)*sm(i,j,k-1)+dx1(i)*sm(i+1,j,k-1))     /(dx1(i)+dx1(i+1)))      +dzn(k-1)*((dx1(i+1)*sm(i,j,k)+dx1(i)*sm(i+1,j,k))     /(dx1(i)+dx1(i+1))))     /(dzn(k-1)+dzn(k))
! 
      visvx2=(evsx2)* ( diu2(i  ,j+1,k  )+diu4(i+1,j  ,k  ) )
      visvx1=(evsx1)* ( diu2(i-1,j+1,k  )+diu4(i  ,j  ,k  ) )
      visvy2=(evsy2)*2.*diu5(i  ,j+1,k  )
      visvy1=(evsy1)*2.*diu5(i  ,j  ,k  )
      visvz2=(evsz2)* ( diu6(i  ,j  ,k+1)+diu8(i  ,j+1,k  ) )
      visvz1=(evsz1)* ( diu6(i  ,j  ,k  )+diu8(i  ,j+1,k-1) )
! 
      vfv=(visvx2-visvx1)/dx1(i)   +(visvy2-visvy1)/dy1(j)   +(visvz2-visvz1)/dzn(k) 
! 
      g(i,j,k)=(g(i,j,k)+vfv) 
   end do
      end do
      end do
! --calculation of viscosity terms in momentum eq.(z-comp.)
      do k=1,km
      do j=1,jm
      do i=1,im
! --eddyviscosity on face
      evsz2=sm(i,j,k+1)
      evsz1=sm(i,j,k)
      evsx2=(dzn(k+1)*((dx1(i+1)*sm(i,j,k)+dx1(i)*sm(i+1,j,k))     /(dx1(i)+dx1(i+1)))      +dzn(k)*((dx1(i+1)*sm(i,j,k+1)+dx1(i)*sm(i+1,j,k+1))     /(dx1(i)+dx1(i+1))))     /(dzn(k)+dzn(k+1))
      evsx1=(dzn(k+1)*((dx1(i)*sm(i-1,j,k)+dx1(i-1)*sm(i,j,k))     /(dx1(i-1)+dx1(i)))      +dzn(k)*((dx1(i)*sm(i-1,j,k+1)+dx1(i-1)*sm(i,j,k+1))     /(dx1(i-1)+dx1(i))))     /(dzn(k)+dzn(k+1))   
      evsy2=(dzn(k+1)*((dy1(j+1)*sm(i,j,k)+dy1(j)*sm(i,j+1,k))     /(dy1(j)+dy1(j+1)))      +dzn(k)*((dy1(j+1)*sm(i,j,k+1)+dy1(j)*sm(i,j+1,k+1))     /(dy1(j)+dy1(j+1))))     /(dzn(k)+dzn(k+1))
      evsy1=(dzn(k+1)*((dy1(j)*sm(i,j-1,k)+dy1(j-1)*sm(i,j,k))     /(dy1(j-1)+dy1(j)))      +dzn(k)*((dy1(j)*sm(i,j-1,k+1)+dy1(j-1)*sm(i,j,k+1))     /(dy1(j-1)+dy1(j))))     /(dzn(k)+dzn(k+1))
! 
      viswx2=(evsx2)* ( diu3(i  ,j  ,k+1)+diu7(i+1,j  ,k  ) )
      viswx1=(evsx1)* ( diu3(i-1,j  ,k+1)+diu7(i  ,j  ,k  ) )
      viswy2=(evsy2)* ( diu6(i  ,j  ,k+1)+diu8(i  ,j+1,k  ) )
      viswy1=(evsy1)* ( diu6(i  ,j-1,k+1)+diu8(i  ,j  ,k  ) )
      viswz2=(evsz2)*2.*diu9(i  ,j  ,k+1)
      viswz1=(evsz1)*2.*diu9(i  ,j  ,k  )
! 
      vfw=(viswx2-viswx1)/dx1(i)   +(viswy2-viswy1)/dy1(j)   +(viswz2-viswz1)/dzn(k) 
! 
      h(i,j,k)=(h(i,j,k)+vfw) 
   end do
      end do
      end do
! 
      return
      end subroutine les

end module module_les

